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
#       en UTF-8) : jeu_reference() puis, pour R1 à R6 (plans_reference() de
#       tests/reference/outils_reference.R), construire_table(), puis
#       ecrire_fiche(x$fiche) et ecrire_plan(x$fiche$plan), relus par
#       lire_fiche() et lire_plan() : aller-retour identical(), et réécriture
#       de l'objet relu identique octet à octet au texte écrit ; pour R1,
#       rejouer_fiche() sur la fiche relue : chaînes non ASCII du bilan
#       marquées UTF-8 (#86) ;
#    3. les textes (fiche et plan de R1 à R6, bilan du rejeu de R1) écrits
#       sous C et sous UTF-8 sont comparés octet à octet. Aucune différence n'est admise : à
#       entrées, rôles, plan et graine identiques sur la même machine, la
#       locale ne change aucun octet de la fiche ni du plan, empreinte du
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
IDENTIFIANTS <- paste0("R", 1:6)

# Octets tels quels (marque d'encodage retirée) pour cat() : sous la locale C,
# cat() écrit « <U+00E9> » pour une chaîne marquée UTF-8.
octets_bruts <- function(x) { Encoding(x) <- "unknown"; x }

# Première locale UTF-8 que Sys.setlocale accepte, "" si aucune.
locale_utf8_disponible <- function() {
  ancienne <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ancienne))
  for (l in LOCALES_UTF8) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) return(l)
  ""
}

# Variables « NOM=valeur » posées dans ce processus le temps de l'appel (le
# sous-processus en hérite) ; restaurées ensuite.
avec_environnement <- function(env, expr) {
  noms <- names(env)
  anciens <- Sys.getenv(noms, unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(env))
  on.exit(for (n in noms) if (is.na(anciens[[n]])) Sys.unsetenv(n) else
    do.call(Sys.setenv, stats::setNames(list(anciens[[n]]), n)))
  force(expr)
}

environnement_locale <- function(locale) c(LC_ALL = locale, LANG = locale, LC_CTYPE = locale)

# --- sous-processus : R1 à R6, fiche et plan --------------------------------

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
  jeu <- jeu_reference()
  for (p in plans_reference()) {
    id <- p$identifiant
    d <- function(m) defauts <<- c(defauts, sprintf("%s : %s", id, octets_bruts(m)))
    r <- tryCatch({
      x <- construire_table(jeu$donnees, jeu$roles, p$plan)
      f <- x$fiche
      tf <- ecrire_fiche(f); tp <- ecrire_plan(f$plan)
      if (!is.raw(tf) || !is.raw(tp)) stop("texte non écrit en octets (raw)")
      writeBin(tf, file.path(sortie, paste0(id, ".fiche.txt")))
      writeBin(tp, file.path(sortie, paste0(id, ".plan.txt")))
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
      }
      NULL
    }, error = function(e) e)
    if (inherits(r, "error")) d(sprintf("erreur [%s] : %s", paste(class(r)[1L], collapse = "/"), conditionMessage(r)))
    cat(sprintf("  %s : %s\n", id, if (length(defauts) && any(startsWith(defauts, paste0(id, " :")))) "ECHEC" else "ok"))
  }
  if (length(defauts)) { cat(paste0("  DEFAUT ", defauts, "\n"), sep = ""); quit(save = "no", status = 1L) }
  invisible(TRUE)
}

# --- orchestration ------------------------------------------------------------

duree <- function(depuis) sprintf("%.1f s", as.numeric(difftime(Sys.time(), depuis, units = "secs")))

installer <- function(racine, lib, locale) {
  t0 <- Sys.time()
  r <- avec_environnement(environnement_locale(locale),
    suppressWarnings(system2(file.path(R.home("bin"), "R"),
                             c("CMD", "INSTALL", "--no-docs", "-l", shQuote(lib), shQuote(racine)),
                             stdout = TRUE, stderr = TRUE)))
  statut <- attr(r, "status")
  if (!is.null(statut) && statut != 0L)
    return(list(ok = FALSE, sortie = r, duree = duree(t0)))
  list(ok = TRUE, sortie = r, duree = duree(t0))
}

executer <- function(script, racine, lib, locale, attendu, sortie) {
  t0 <- Sys.time()
  r <- avec_environnement(environnement_locale(locale),
    suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                             c(shQuote(script), "--travailleur", shQuote(lib), attendu, shQuote(sortie), shQuote(racine)),
                             stdout = TRUE, stderr = TRUE)))
  statut <- attr(r, "status")
  list(ok = is.null(statut) || statut == 0L, sortie = r, duree = duree(t0))
}

# Premier octet différent, avec le numéro de ligne (LF) : message d'écart.
ecart_octets <- function(a, b) {
  n <- min(length(a), length(b))
  k <- which(a[seq_len(n)] != b[seq_len(n)])[1L]
  if (is.na(k)) k <- n + 1L
  ligne <- 1L + sum(a[seq_len(k - 1L)] == as.raw(10L))
  sprintf("longueurs %d et %d octets ; premier octet différent : %d (ligne %d)", length(a), length(b), k, ligne)
}

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
    i <- installer(racine, c_$lib, c_$locale)
    cat(sprintf("  R CMD INSTALL sous LC_ALL=%s : %s (%s)\n", c_$locale, if (i$ok) "ok" else "ECHEC", i$duree))
    if (!i$ok) { cat(i$sortie, sep = "\n"); defauts <- c(defauts, sprintf("installation sous LC_ALL=%s en échec", c_$locale)) }
  }
  if (length(defauts)) echec()
  for (c_ in cas) {
    cat(sprintf("  R1 à R6, fiche et plan, sous LC_ALL=%s :\n", c_$locale))
    e <- executer(script, racine, c_$lib, c_$locale, c_$attendu, c_$sortie)
    cat(paste0("  ", e$sortie), sep = "\n")
    cat(sprintf("  durée : %s\n", e$duree))
    if (!e$ok) defauts <- c(defauts, sprintf("sous-processus sous LC_ALL=%s en échec (voir ci-dessus)", c_$locale))
  }
  if (length(defauts)) echec()
  # Comparaison octet à octet : textes écrits sous C contre sous UTF-8.
  n_comparees <- 0L
  for (id in IDENTIFIANTS) for (nature in c("fiche", "plan")) {
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
  if (n_comparees != 2L * length(IDENTIFIANTS) + 1L && !length(defauts)) defauts <- "nombre de textes comparés inattendu"
  if (length(defauts)) echec()
  cat(sprintf("  OK : %d textes (fiche et plan de %s, bilan du rejeu de R1) identiques octet à octet sous C et sous %s ; aller-retour identical() ; durée totale %s.\n",
              n_comparees, paste(range(IDENTIFIANTS), collapse = " à "), utf8, duree(t_debut)))
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
