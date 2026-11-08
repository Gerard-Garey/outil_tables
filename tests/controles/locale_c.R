# -----------------------------------------------------------------------------
#  tests/controles/locale_c.R : tablexp installé et exécuté sous la locale C
#  (issue #66 ; ADR 0007, point 11 et annotation du 19/10/2026 ; M54, M59 (c))
#
#  Les batteries installent toujours tablexp en UTF-8 (installer_tablexp(),
#  tests/harnais.R) : sans ce contrôle, une régression qui n'apparaît que sous
#  la locale C (paquet installé en C, session en C) passerait la CI. Défauts
#  de cette nature déjà trouvés par lancement manuel : ecrire_fiche() refusée,
#  échecs unitaires, unite_exposition (revue finale de B4).
#
#  Déroulement :
#    1. R CMD INSTALL de l'arbre de travail dans deux bibliothèques
#       temporaires : l'une sous LC_ALL=C (LANG et LC_CTYPE=C aussi), l'autre
#       sous la première locale UTF-8 disponible (C.UTF-8, C.utf8,
#       en_US.UTF-8 : sondée comme le harnais) ;
#    2. dans un sous-processus par bibliothèque, sous la locale de son
#       installation, qui vérifie d'abord que la session est bien en C (resp.
#       en UTF-8) : pour R1 à R13 (plans_reference() de
#       tests/reference/outils_reference.R, sur l'entrée de chaque plan :
#       jeu_reference(), jeu_reference_defauts() pour R9, ou
#       agregats_reference() pour R13, #169), construire_table(), puis
#       ecrire_fiche(x$fiche) et ecrire_plan(x$fiche$plan), relus par
#       lire_fiche() et lire_plan() : aller-retour identical(), et réécriture
#       de l'objet relu identique octet à octet au texte écrit ; rapport figé
#       ecrire_rapport(x) (#87, C1) ; pour R1, rejouer_fiche() sur la fiche
#       relue : chaînes non ASCII du bilan marquées UTF-8 (#86) ; messages
#       de refus du rejeu et de objet_modifie (et citation d'une chaîne
#       latin-1) marqués UTF-8 et sur une ligne (#94) ; puis un refus par
#       module citant une valeur non ASCII native, latin-1 et UTF-8, et
#       trois refus de lire_texte() et deux type_incompatible sur un rôle
#       date (libellé « entières » de TYPES_ADMIS) : marqués UTF-8, sans
#       octet « <xx> » (#127) ;
#    3. les textes (fiche, plan et rapport figé de R1 à R13, bilan du rejeu et
#       messages de refus de R1, messages de refus par module) écrits sous C
#       et sous UTF-8 sont comparés
#       octet à octet. Aucune
#       différence n'est admise : à entrées, rôles, plan et graine identiques
#       sur la même machine, la locale ne change aucun octet de la fiche, du
#       plan ni du rapport, empreinte du
#       code (MD5 du relevé des MD5 de R/*.R, fait à l'installation :
#       indépendant de la locale) et empreinte du résultat (« même machine »)
#       comprises.
#
#  Si une garde de chargement hors UTF-8 (#25) refuse un jour le chargement
#  sous C, ce contrôle portera sur le refus nommé plutôt que sur l'écriture.
#
#  Usage : Rscript tests/controles/locale_c.R [racine du dépôt]
#  Code de sortie 1 au moindre écart (message ECHEC sur la sortie standard,
#  ligne ::error sur la CI). Sourcé, le fichier ne fait que définir les
#  fonctions.
# -----------------------------------------------------------------------------

LOCALES_UTF8 <- c("C.UTF-8", "C.utf8", "en_US.UTF-8")  # comme tests/harnais.R
LOCALE_INVENTEE <- "xx_XX.UTF-8"  # comme tests/harnais.R (critère de #109)
IDENTIFIANTS <- paste0("R", 1:13)  # R13 : #169, recette agrégats

# Orchestration commune (installer, executer, avec_environnement, duree,
# ecart_octets : tests/controles/orchestration_locale.R, #117), chargée dans
# l'environnement courant. Dossiers cherchés : celui du script lancé (Rscript,
# --file=), son sous-dossier controles/ (contrôle sourcé depuis
# tests/test_unitaires.R), puis tests/controles/ sous le répertoire courant ;
# n'est retenu que celui qui contient aussi ce contrôle : on charge son voisin,
# jamais un homonyme placé à côté du script lanceur. Lue en UTF-8 quelle que
# soit la locale.
for (e_ in parse(encoding = "UTF-8", keep.source = FALSE, file = local({
  controle_ <- "locale_c.R"
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1L]
  d <- c(if (!is.na(f)) c(dirname(f), file.path(dirname(f), "controles")), file.path("tests", "controles"))
  d <- d[file.exists(file.path(d, controle_))]
  if (!length(d) || !file.exists(file.path(d[1L], "orchestration_locale.R")))
    stop("orchestration_locale.R introuvable à côté de ", controle_)
  file.path(d[1L], "orchestration_locale.R")
}))) eval(e_, environment())
rm(e_)

# Octets tels quels (marque d'encodage retirée) pour cat() : sous la locale C,
# cat() écrit « <U+00E9> » pour une chaîne marquée UTF-8.
octets_bruts <- function(x) { Encoding(x) <- "unknown"; x }

# Première locale UTF-8 que Sys.setlocale accepte, "" si aucune. Copie à
# l'identique de locale_utf8_disponible() de tests/harnais.R (critère de #109 :
# un nom inventé que l'UCRT accepte est refusé ; voir le commentaire de cette
# fonction).
locale_utf8_disponible <- function(candidats = LOCALES_UTF8) {
  ancienne <- vapply(c("LC_CTYPE", "LC_MONETARY"), Sys.getlocale, "")
  on.exit(for (n in names(ancienne)) Sys.setlocale(n, ancienne[[n]]))
  devise <- function(l) if (nzchar(suppressWarnings(Sys.setlocale("LC_MONETARY", l))))
    Sys.localeconv()[["int_curr_symbol"]]
  inventee <- devise(LOCALE_INVENTEE)
  for (l in candidats) {
    if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) next
    if (!is.null(inventee) && identical(devise(l), inventee)) next
    return(l)
  }
  ""
}

environnement_locale <- function(locale) c(LC_ALL = locale, LANG = locale, LC_CTYPE = locale)

# --- sous-processus : R1 à R13, fiche et plan --------------------------------

travailleur <- function(args) {
  lib <- args[1L]; attendu <- args[2L]; sortie <- args[3L]; racine <- args[4L]
  library(tablexp, lib.loc = lib)
  for (e in parse(file.path(racine, "tests", "reference", "outils_reference.R"),
                  encoding = "UTF-8", keep.source = FALSE)) eval(e, globalenv())
  utf8 <- isTRUE(l10n_info()[["UTF-8"]])
  cat(sprintf("  session : LC_CTYPE = %s ; UTF-8 : %s\n", Sys.getlocale("LC_CTYPE"), utf8))
  if (!identical(utf8, identical(attendu, "utf8"))) {
    cat(sprintf("ECHEC : session %s, attendue %s : le contrôle ne porterait pas sur la locale visée\n",
                if (utf8) "UTF-8" else "hors UTF-8", if (identical(attendu, "utf8")) "UTF-8" else "hors UTF-8"))
    quit(save = "no", status = 1L)
  }
  defauts <- character(0)
  # Une entrée par plan (`entree`, ADR 0009, point 4.1 amendé), chaque entrée
  # distincte tirée une seule fois.
  entrees <- entrees_des_plans(plans_reference())
  # IDENTIFIANTS (comparaison octet à octet du processus principal) couvre
  # tous les plans de plans_reference() : un plan ajouté sans mise à jour de
  # IDENTIFIANTS serait construit ici et jamais comparé.
  ids_plans <- vapply(plans_reference(), function(p) p$identifiant, "")
  if (!identical(unname(ids_plans), IDENTIFIANTS))
    defauts <- c(defauts, sprintf("plans_reference() (%s) different de IDENTIFIANTS (%s)",
                                  paste(ids_plans, collapse = ", "), paste(IDENTIFIANTS, collapse = ", ")))
  for (p in plans_reference()) {
    id <- p$identifiant
    jeu <- entrees[[p$entree]]
    d <- function(m) defauts <<- c(defauts, sprintf("%s : %s", id, octets_bruts(m)))
    r <- tryCatch({
      x <- construire_table(jeu$donnees, jeu$roles, p$plan, tables_reference = tables_du_plan(p))
      f <- x$fiche
      tf <- ecrire_fiche(f); tp <- ecrire_plan(f$plan)
      if (!is.raw(tf) || !is.raw(tp)) stop("texte non écrit en octets (raw)")
      writeBin(tf, file.path(sortie, paste0(id, ".fiche.txt")))
      writeBin(tp, file.path(sortie, paste0(id, ".plan.txt")))
      # Rapport figé (#87, C1) : octets comparés ensuite entre les deux installations.
      tr <- ecrire_rapport(x)
      if (!is.raw(tr)) stop("rapport figé non écrit en octets (raw)")
      writeBin(tr, file.path(sortie, paste0(id, ".rapport.txt")))
      if (!identical(lire_fiche(tf), f)) d("fiche : lire_fiche(ecrire_fiche(f)) n'est pas identical() à f")
      if (!identical(lire_plan(tp), f$plan)) d("plan : lire_plan(ecrire_plan(p)) n'est pas identical() au plan")
      if (!identical(ecrire_fiche(lire_fiche(tf)), tf)) d("fiche : réécriture de la fiche relue différente du texte écrit")
      if (!identical(ecrire_plan(lire_plan(tp)), tp)) d("plan : réécriture du plan relu différente du texte écrit")
      # Bilan du rejeu outillé de R1 (#86, T10) : chaînes non ASCII marquées
      # UTF-8 ; octets des cinq colonnes, une cellule par ligne, comparés
      # ensuite entre les deux installations.
      if (identical(id, "R1")) {
        v <- unlist(rejouer_fiche(jeu$donnees, lire_fiche(tf))$bilan, use.names = FALSE)
        if (any(grepl("[^\001-\177]", v, useBytes = TRUE) & Encoding(v) != "UTF-8"))
          d("bilan du rejeu : chaîne non ASCII non marquée UTF-8")
        writeBin(unlist(lapply(v, function(s) c(charToRaw(s), as.raw(10L)))), file.path(sortie, "R1.bilan.txt"))
        # Messages de refus du rejeu et de objet_modifie (#94, critère 3) :
        # marqués UTF-8 sous la locale C, sur une ligne ; citation latin-1
        # (« âge ») convertie et marquée UTF-8 ; octets comparés ensuite entre
        # les deux installations.
        refus_msg <- function(expr) tryCatch({ force(expr); NA_character_ },
                                             refus_tablexp = function(r) conditionMessage(r))
        u <- function(...) intToUtf8(c(...))
        fr <- lire_fiche(tf)
        f1 <- fr; f1$empreintes$algorithme <- u(0x6d, 0x64, 0x0b, 0xe9)
        f2 <- fr; f2$version_fiche <- 3L; f2$versions$tablexp <- u(0x61, 0x2028, 0x62)
        y1 <- x; y1$fiche$empreintes$resultat <- u(0xe9)
        y2 <- x; attr(y2$etapes, "truc") <- 1
        l1 <- rawToChar(as.raw(c(0xe2, 0x67, 0x65))); Encoding(l1) <- "latin1"
        m <- c(refus_msg(rejouer_fiche(jeu$donnees, f1)), refus_msg(rejouer_fiche(jeu$donnees, f2)),
               refus_msg(ecrire_rapport(y1)), refus_msg(ecrire_rapport(y2)), tablexp:::citer(l1))
        if (anyNA(m)) d("refus attendu non levé (rejeu ou objet_modifie)")
        else {
          if (any(Encoding(m) != "UTF-8")) d("message de refus ou citation non marqué UTF-8")
          if (any(grepl("[\n\v\f]", m, useBytes = TRUE))) d("message de refus sur plusieurs lignes")
          if (!identical(charToRaw(m[5L]), as.raw(c(0xc3, 0xa2, 0x67, 0x65)))) d("citation latin-1 non convertie en UTF-8")
          writeBin(unlist(lapply(m, function(s) c(charToRaw(s), as.raw(10L)))), file.path(sortie, "R1.refus.txt"))
        }
      }
      NULL
    }, error = function(e) e)
    if (inherits(r, "error")) d(sprintf("erreur [%s] : %s", paste(class(r)[1L], collapse = "/"), conditionMessage(r)))
    cat(sprintf("  %s : %s\n", id, if (length(defauts) && any(startsWith(defauts, paste0(id, " :")))) "ECHEC" else "ok"))
  }
  # Messages de refus par module (#127, critère 2 ; ADR 0007, annotation du
  # 05/11/2026, (c) et (f)) : un refus par module qui cite une valeur reçue
  # non ASCII (« âge ») de codage natif non marqué, latin-1 et UTF-8, le refus
  # portant le module et le motif attendus de l'entrée (neuf modules, dont
  # validation : #127, B13 ; positionnement : #167 ; homogeneite : #168, dont
  # aucun refus ne cite de valeur reçue : la valeur non ASCII y est passée en
  # tranches_age, refusée par sa forme, et le texte « âge » contrôlé est celui
  # du gabarit, marqué UTF-8 par formuler()) ; puis
  # les trois refus de lire_texte() de tests/unitaires/test_serialisation.R
  # (première ligne inconnue, type inconnu, valeur illisible ; entrées en
  # octets bruts). Chaque message : marqué UTF-8, sur une ligne, sans octet
  # écrit « <xx> » (gabarit échappé par sprintf() sous C, régression de #94),
  # la citation gardée en octets UTF-8 (c3 a2 67 65, ou c3 a9). Octets
  # comparés ensuite entre les deux installations.
  d <- function(m) defauts <<- c(defauts, sprintf("refus par module : %s", octets_bruts(m)))
  r <- tryCatch({
    refus_msg <- function(expr) tryCatch({ force(expr); NA_character_ },
                                         refus_tablexp = function(r) conditionMessage(r))
    brut_age <- as.raw(c(0xc3, 0xa2, 0x67, 0x65))
    natif <- rawToChar(brut_age)
    latin1 <- rawToChar(as.raw(c(0xe2, 0x67, 0x65))); Encoding(latin1) <- "latin1"
    utf8_ <- rawToChar(brut_age); Encoding(utf8_) <- "UTF-8"
    valeurs <- list(natif = natif, latin1 = latin1, utf8 = utf8_)
    modules <- list(
      donnees = function(v) tablexp:::controler_roles(data.frame(x = 1), c(sexe = v), character(0)),
      exposition = function(v) tablexp:::controler_parametres_exposition(v, FALSE),
      brut = function(v) tablexp:::controler_parametres_brut(v, 0.95, FALSE),
      simulation = function(v) tablexp:::controler_defauts(
        list(graine = 1L, declarations = list(list(type = v, cible = "deces", taux = 0.1))), 2L),
      rapport = function(v) tablexp:::ecrire_valeurs(structure(1, class = v), "texte", "r"),
      construire_table = function(v) valider_plan(stats::setNames(list(1), v)),
      positionnement = function(v) tablexp:::controler_entrees_validation(
        list(annee = 1), v, tablexp:::CONVENTIONS_VALIDATION[1L], character(0), c(0, 1), "positionnement"),
      validation = function(v) tablexp:::controler_entrees_validation(
        list(annee = 1), v, tablexp:::CONVENTIONS_VALIDATION[1L], character(0), c(0, 1), "validation"),
      homogeneite = function(v) tablexp:::controler_parametres_homogeneite(c(0, 1), v, 1, "sexe"))
    # Motif attendu de chaque refus : sans lui, un refus voisin du même module
    # (autre garde, même citation) passerait.
    motifs_attendus <- c(donnees = "colonne_absente", exposition = "convention_invalide", brut = "estimateur_inconnu",
                         simulation = "defauts_invalides", rapport = "type_non_canonique",
                         construire_table = "champ_inconnu", positionnement = "table_annee_non_admise",
                         validation = "table_annee_non_admise", homogeneite = "tranches_age_invalides")
    msgs <- character(0); noms <- character(0)
    for (mo in names(modules)) for (en in names(valeurs)) {
      cond <- tryCatch({ force(modules[[mo]](valeurs[[en]])); NULL }, refus_tablexp = function(r) r)
      nom <- sprintf("%s (%s)", mo, en)
      if (is.null(cond)) { d(sprintf("%s : refus attendu non levé", nom)); next }
      m <- conditionMessage(cond)
      if (!identical(cond$module, mo)) d(sprintf("%s : refus du module « %s » au lieu de « %s »", nom, cond$module, mo))
      if (!identical(cond$motif, motifs_attendus[[mo]]))
        d(sprintf("%s : refus de motif « %s » au lieu de « %s »", nom, cond$motif, motifs_attendus[[mo]]))
      if (!grepl(rawToChar(brut_age), octets_bruts(m), fixed = TRUE, useBytes = TRUE))
        d(sprintf("%s : citation absente ou altérée : %s", nom, m))
      msgs <- c(msgs, m); noms <- c(noms, nom)
    }
    # type_incompatible sur un rôle date (M1 de la revue finale de #127) : le
    # libellé TYPES_ADMIS["date"] (« valeurs entières finies »), constante du
    # moteur non ASCII, est inséré par %s ; sans texte_moteur(), sprintf()
    # l'écrit « enti<c3><a8>res » sous C. Colonne de type character, puis de
    # classe (chaîne d'encodage bytes, Date) ; octets « è » (c3 a8) du
    # libellé exigés dans chaque message.
    classe_bytes <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9))); Encoding(classe_bytes) <- "bytes"
    col_date <- structure(18262, class = c(classe_bytes, "Date"))
    dates <- list("type_incompatible, rôle date, colonne character" = data.frame(d = "2020-01-01"),
                  "type_incompatible, rôle date, classe bytes" = structure(list(d = col_date), class = "data.frame",
                                                                          row.names = 1L))
    for (nom in names(dates)) {
      m <- refus_msg(tablexp:::controler_roles(dates[[nom]], c(date_entree = "d"), character(0)))
      if (is.na(m)) { d(sprintf("%s : refus attendu non levé", nom)); next }
      if (!grepl(rawToChar(as.raw(c(0x65, 0x6e, 0x74, 0x69, 0xc3, 0xa8, 0x72, 0x65, 0x73))), octets_bruts(m),
                 fixed = TRUE, useBytes = TRUE))
        d(sprintf("%s : libellé « entières » absent ou altéré : %s", nom, m))
      msgs <- c(msgs, m); noms <- c(noms, nom)
    }
    u8 <- function(...) { x <- rawToChar(as.raw(c(...))); Encoding(x) <- "UTF-8"; x }
    e_acute <- c(0xc3, 0xa9)
    tete_ <- charToRaw("tablexp plan 1\n@noms\tcharacter\t1\t\"a\"\n")
    lectures <- list(
      "première ligne inconnue" = c(charToRaw("tablexp plan 1"), as.raw(e_acute), as.raw(c(0x0b, 0x0a)), tete_[-(1:15)]),
      "type inconnu" = c(tete_, charToRaw("a\ttyp"), as.raw(e_acute), charToRaw("\t1\t1\n")),
      "valeur illisible" = c(tete_, charToRaw("a\tinteger\t1\t1"), as.raw(e_acute), charToRaw("\n")))
    for (nom in names(lectures)) {
      m <- refus_msg(lire_plan(lectures[[nom]]))
      if (is.na(m)) { d(sprintf("lire_plan, %s : refus attendu non levé", nom)); next }
      if (!grepl(u8(e_acute), m, fixed = TRUE)) d(sprintf("lire_plan, %s : citation absente ou altérée : %s", nom, m))
      msgs <- c(msgs, m); noms <- c(noms, paste("lire_plan,", nom))
    }
    for (i in seq_along(msgs)) {
      if (!identical(Encoding(msgs[i]), "UTF-8")) d(sprintf("%s : message non marqué UTF-8", noms[i]))
      if (grepl("<[0-9a-f]{2}>", msgs[i], useBytes = TRUE)) d(sprintf("%s : octets écrits <xx> : %s", noms[i], msgs[i]))
      if (grepl("[\n\v\f]", msgs[i], useBytes = TRUE)) d(sprintf("%s : message sur plusieurs lignes", noms[i]))
    }
    writeBin(unlist(lapply(msgs, function(s) c(charToRaw(s), as.raw(10L)))), file.path(sortie, "modules.refus.txt"))
    cat(sprintf("  refus par module (#127) : %d messages, %s\n", length(msgs),
                if (any(startsWith(defauts, "refus par module"))) "ECHEC" else "ok"))
    NULL
  }, error = function(e) e)
  if (inherits(r, "error")) d(sprintf("erreur [%s] : %s", class(r)[1L], conditionMessage(r)))
  if (length(defauts)) { cat(paste0("  DEFAUT ", defauts, "\n"), sep = ""); quit(save = "no", status = 1L) }
  invisible(TRUE)
}

# --- orchestration (installer(), executer() : orchestration_locale.R) ----------

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]))
  if (length(args) && identical(args[1L], "--travailleur")) return(travailleur(args[-1L]))
  racine <- normalizePath(if (length(args)) args[1L] else dirname(dirname(dirname(script))))
  t_debut <- Sys.time()
  defauts <- character(0)
  echec <- function() {
    cat(paste0("  ECHEC : ", defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Locale C::", paste(gsub("\n", " ", defauts), collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("tablexp sous la locale C (installation et exécution) :", racine, "\n")
  utf8 <- locale_utf8_disponible()
  if (!nzchar(utf8)) {
    defauts <- sprintf("aucune locale UTF-8 disponible parmi %s : comparaison impossible", paste(LOCALES_UTF8, collapse = ", "))
    echec()
  }
  tmp <- tempfile("locale_c_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  cas <- list(list(nom = "C", locale = "C", attendu = "c"),
              list(nom = utf8, locale = utf8, attendu = "utf8"))
  for (k in seq_along(cas)) {
    cas[[k]]$lib <- file.path(tmp, paste0("lib", k)); cas[[k]]$sortie <- file.path(tmp, paste0("sortie", k))
    dir.create(cas[[k]]$lib); dir.create(cas[[k]]$sortie)
  }
  for (c_ in cas) {
    i <- installer(racine, c_$lib, environnement_locale(c_$locale))
    cat(sprintf("  R CMD INSTALL sous LC_ALL=%s : %s (%s)\n", c_$locale, if (i$ok) "ok" else "ECHEC", i$duree))
    if (!i$ok) { cat(i$sortie, sep = "\n"); defauts <- c(defauts, sprintf("installation sous LC_ALL=%s en échec", c_$locale)) }
  }
  if (length(defauts)) echec()
  for (c_ in cas) {
    cat(sprintf("  %s, fiche, plan et rapport figé, sous LC_ALL=%s :\n", paste(IDENTIFIANTS, collapse = ", "), c_$locale))
    e <- executer(script, racine, c_$lib, environnement_locale(c_$locale), c_$attendu, c_$sortie)
    cat(paste0("  ", e$sortie), sep = "\n")
    cat(sprintf("  durée : %s\n", e$duree))
    if (!e$ok) defauts <- c(defauts, sprintf("sous-processus sous LC_ALL=%s en échec (voir ci-dessus)", c_$locale))
  }
  if (length(defauts)) echec()
  # Comparaison octet à octet : textes écrits sous C contre sous UTF-8.
  n_comparees <- 0L
  for (id in IDENTIFIANTS) for (nature in c("fiche", "plan", "rapport")) {
    nom <- sprintf("%s.%s.txt", id, nature)
    f <- file.path(vapply(cas, `[[`, "", "sortie"), nom)
    if (!all(file.exists(f))) { defauts <- c(defauts, sprintf("%s : texte absent (%s)", nom, paste(basename(dirname(f))[!file.exists(f)], collapse = ", "))); next }
    a <- readBin(f[1L], "raw", file.size(f[1L])); b <- readBin(f[2L], "raw", file.size(f[2L]))
    n_comparees <- n_comparees + 1L
    if (!length(a) || !identical(a, b))
      defauts <- c(defauts, sprintf("%s : texte écrit sous C différent de celui écrit sous %s (%s)", nom, utf8, ecart_octets(a, b)))
  }
  # Bilan du rejeu de R1 (#86, T10) : octets des cinq colonnes.
  f <- file.path(vapply(cas, `[[`, "", "sortie"), "R1.bilan.txt")
  if (!all(file.exists(f))) defauts <- c(defauts, "R1.bilan.txt : bilan du rejeu absent") else {
    a <- readBin(f[1L], "raw", file.size(f[1L])); b <- readBin(f[2L], "raw", file.size(f[2L]))
    n_comparees <- n_comparees + 1L
    if (!length(a) || !identical(a, b))
      defauts <- c(defauts, sprintf("R1.bilan.txt : bilan du rejeu sous C différent de celui sous %s (%s)", utf8, ecart_octets(a, b)))
  }
  # Messages de refus du rejeu et de objet_modifie, citation latin-1 (#94).
  f <- file.path(vapply(cas, `[[`, "", "sortie"), "R1.refus.txt")
  if (!all(file.exists(f))) defauts <- c(defauts, "R1.refus.txt : messages de refus absents") else {
    a <- readBin(f[1L], "raw", file.size(f[1L])); b <- readBin(f[2L], "raw", file.size(f[2L]))
    n_comparees <- n_comparees + 1L
    if (!length(a) || !identical(a, b))
      defauts <- c(defauts, sprintf("R1.refus.txt : messages de refus sous C différents de ceux sous %s (%s)", utf8, ecart_octets(a, b)))
  }
  # Messages de refus par module et de lire_texte() (#127, critère 2).
  f <- file.path(vapply(cas, `[[`, "", "sortie"), "modules.refus.txt")
  if (!all(file.exists(f))) defauts <- c(defauts, "modules.refus.txt : messages de refus par module absents") else {
    a <- readBin(f[1L], "raw", file.size(f[1L])); b <- readBin(f[2L], "raw", file.size(f[2L]))
    n_comparees <- n_comparees + 1L
    if (!length(a) || !identical(a, b))
      defauts <- c(defauts, sprintf("modules.refus.txt : messages de refus par module sous C différents de ceux sous %s (%s)",
                                    utf8, ecart_octets(a, b)))
  }
  if (n_comparees != 3L * length(IDENTIFIANTS) + 3L && !length(defauts)) defauts <- "nombre de textes comparés inattendu"
  if (length(defauts)) echec()
  cat(sprintf("  OK : %d textes (fiche, plan et rapport figé de %s, bilan du rejeu et messages de refus de R1, messages de refus par module) identiques octet à octet sous C et sous %s ; aller-retour identical() ; durée totale %s.\n",
              n_comparees, paste(IDENTIFIANTS, collapse = ", "), utf8, duree(t_debut)))
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
