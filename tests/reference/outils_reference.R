# -----------------------------------------------------------------------------
#  tests/reference/outils_reference.R : outillage des références de
#  non-régression (issue #17 ; ADR 0009, corps et annotations du 17/10/2026)
#
#  Chargé par tests/test_reproductibilite.R, produire_references.R et
#  comparer_references.R par charger_utf8() (parse(encoding = "UTF-8") : les
#  littéraux non ASCII de l'outillage sont marqués UTF-8 quelle que soit la
#  locale, mesuré en C et C.UTF-8) ; R de base, sans évaluation de code à la
#  lecture (ni source(), ni eval(parse()), ni dget() sur un fichier de
#  référence).
#
#  Chaînes (ADR 0007, point 9.3) : toute chaîne écrite passe par
#  utf8_ou_refus(), qui applique la règle de tablexp:::octets_chaines() (une
#  chaîne au codage « bytes », non UTF-8 valide, ou non ASCII non marquée
#  dans une session ni UTF-8 ni latin-1 est refusée, jamais convertie).
#
#  - plans_reference(), jeu_reference() : plans R1 à R6 et entrée de
#    référence (points 4.1 et 4.3 ; R6 : première annotation, (b)) ;
#  - aplatir_construction(x) : objet `construction` -> grandeurs (chemin,
#    type, valeurs), moins `execution`, `fiche$versions`, `fiche$plateforme`
#    (point 1.1) ; tables de lignes de RESUMES_PAR_EMPREINTE résumées par
#    @lignes et @empreinte (point 1.2) ; lignes @ de structure (point 2.3) ;
#    refus d'écriture (condition de classe `refus_reference`, chemin fautif)
#    sur toute classe ou tout attribut hors format ;
#  - ecrire_reference(grandeurs, fichier, entete) : TSV UTF-8, LF, octet par
#    octet, valeurs par tablexp:::ecrire_valeurs() en forme « texte »
#    (doubles en %.17g ; ADR 0007, point 9.3 ; ADR 0011, point 7) ;
#  - lire_reference(fichier) : lecture stricte (tout double hors de la forme
#    de `%.17g` est refusé ; hors Windows, tout texte différent de
#    sprintf("%.17g") de la valeur lue l'est aussi : double_lu()) ; aller-retour
#    exact (point 2.4) ;
#  - TOLERANCES, tolerance_de(chemin) : classes de tolérance, clé qualifiée
#    `<table>/<colonne>` prioritaire, recherche par le suffixe le plus long
#    (seconde annotation, (a)) ;
#  - comparer_references(reference, obtenu, tolerances) : empreinte des
#    données en premier (point 3.2), structure exacte (point 3.1), valeurs
#    élément par élément (point 3.3 ; seconde annotation, (c)) ; un double
#    sans classe est un échec (point 3.4) ;
#  - imprimer_comparaison(r) : impression du point 3.5 ;
#  - references_orphelines(dossier) : fichiers sans plan (point 3.7) ;
#  - regle_mode(), controle_bascule(), sortie_interdite() : règles des modes
#    du workflow (point 5.2) et garde du dossier de sortie (point 9.4,
#    portable : forme_chemin()), appliquées par produire_references.R ;
#  - identifiant_plateforme(racine) : bloc de tests/controles/plateforme.R
#    (ADR 0008, point 5), pour l'en-tête (point 2.2).
#
#  Décision d'implémentation (signalée à actuary) : un vecteur atomique nommé
#  (fiche/roles, fiche/generateur) porte, comme une liste, sa ligne
#  `chemin/@noms` ; sans elle, un changement de noms passerait inaperçu.
# -----------------------------------------------------------------------------

# Chargement d'un fichier de l'outillage, littéraux marqués UTF-8 (même
# fonction, recopiée, dans les scripts qui chargent celui-ci).
charger_utf8 <- function(fichier, envir) {
  for (e in parse(fichier, encoding = "UTF-8", keep.source = FALSE)) eval(e, envir)
  invisible(envir)
}

FORMAT_REFERENCE <- "tablexp reference 1"
CHEMIN_EMPREINTE_DONNEES <- "fiche/empreintes/donnees"
CHEMIN_REPRESENTATION <- "fiche/empreintes/representation"
CHEMIN_RESUMES <- "etapes/donnees/resultat"
RESUMES_PAR_EMPREINTE <- c("lignes_conservees", "numeros_conserves", "lignes_exclues",
                           "observations", "observation_des_lignes")
TYPES_REFERENCE <- c("double", "integer", "logical", "character", "Date", "factor", "NULL")

# Classes de tolérance (ADR 0009, point 3.4 et seconde annotation, (b)) ;
# 0 : classe exacte (égalité élément par élément par identical()). Seules
# les grandeurs de type double consultent cette table ; les autres types et
# les lignes @ sont exacts par leur type. Toute modification est annotée
# dans l'ADR 0009, datée, avec l'avis d'actuary (point 9.3).
CLASSES_TOLERANCE <- c(exacte = 0, "1e-14" = 1e-14, "1e-12" = 1e-12)
# Aucune clé pour une grandeur de type integer (quatrième annotation, (b)) :
# une clé la classerait aussi si elle redevenait double, et masquerait ce
# retour à doubles_sans_tolerance().
TOLERANCES <- c(
  # exacte : doubles à valeur entière ou déclarée (point 3.4)
  exposition_jours = 0, niveau = 0, delai_declaration = 0,
  tolerance_effectif = 0,
  # 1e-14 : arithmétique IEEE de base (point 3.4)
  exposition_centrale = 1e-14, complement_initial_cellule = 1e-14,
  complement_initial_fenetre = 1e-14, exposition_initiale = 1e-14, taux = 1e-14,
  # 1e-12 : fonctions spéciales et libm (point 3.4)
  borne_inf_exacte = 1e-12, borne_sup_exacte = 1e-12,
  borne_inf_asymptotique = 1e-12, borne_sup_asymptotique = 1e-12,
  probabilite = 1e-12, probabilite_borne_inf_exacte = 1e-12,
  probabilite_borne_sup_exacte = 1e-12, probabilite_borne_inf_asymptotique = 1e-12,
  probabilite_borne_sup_asymptotique = 1e-12,
  # Kaplan-Meier, clés qualifiées (seconde annotation, (b))
  "kaplan_meier/taux" = 1e-12, "kaplan_meier/variance" = 1e-12,
  "kaplan_meier_sauts/survie_cellule" = 1e-12,
  "kaplan_meier_sauts/age_exact" = 1e-14,
  "kaplan_meier_sauts/variance_log_survie_cellule" = 1e-14)

nom_classe <- function(tol) names(CLASSES_TOLERANCE)[match(tol, CLASSES_TOLERANCE)]

# --- entrée et plans de référence (ADR 0009, points 4.1 et 4.3) --------------

PERIODE_REFERENCE <- list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31"))
GRAINE_REFERENCE <- 20261016L
ROLES_REFERENCE <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe",
                     date_naissance = "date_naissance", date_entree = "date_entree",
                     date_sortie = "date_sortie", cause_sortie = "cause_sortie")

jeu_reference <- function() {
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
              rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
             origine = as.Date("2005-01-01"))
  list(donnees = tablexp::simuler_portefeuille(loi, pf, PERIODE_REFERENCE, GRAINE_REFERENCE),
       roles = ROLES_REFERENCE)
}

plan_reference <- function(transition = c("vivant", "deces"), segmentation = "sexe", delai = 0,
                           convention = "plus_fine", par_annee = FALSE,
                           estimateurs = c("poisson", "binomial"), niveau = 0.95) {
  list(transition = list(depart = transition[1L], arrivee = transition[2L]),
       periode = PERIODE_REFERENCE, delai_declaration = delai, segmentation = segmentation,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = convention, par_annee = par_annee),
                     brut = list(estimateurs = estimateurs, niveau = niveau)))
}

# Un identifiant n'est jamais réemployé (point 4.3).
plans_reference <- function() {
  p <- function(id, description, plan) list(identifiant = id, description = description, plan = plan)
  list(
    R1 = p("R1", "décès, référence principale", plan_reference()),
    R2 = p("R2", "sans segmentation, délai 90, niveau 0,99",
           plan_reference(segmentation = character(0), delai = 90, niveau = 0.99)),
    R3 = p("R3", "convention forcée (age_atteint_annee)", plan_reference(convention = "age_atteint_annee")),
    R4 = p("R4", "par année (surface), Poisson seul", plan_reference(par_annee = TRUE, estimateurs = "poisson")),
    R5 = p("R5", "en cours -> racheté, unité contrat", plan_reference(transition = c("en_cours", "rachat"))),
    R6 = p("R6", "Kaplan-Meier (R1 avec kaplan_meier)",
           plan_reference(estimateurs = c("poisson", "binomial", "kaplan_meier"))))
}

# --- aplatissement (points 1 et 2.3) ------------------------------------------

refus_reference <- function(chemin, message) {
  stop(structure(class = c("refus_reference", "error", "condition"),
                 list(message = sprintf("référence, refus d'écriture : %s : %s",
                                        if (nzchar(chemin)) chemin else "(objet)", message),
                      chemin = chemin, call = NULL)))
}

# Règle d'octets_chaines() (R/rapport.R, ADR 0007, point 9.3), reprise par
# ::: : un refus_tablexp devient un refus d'écriture au chemin fautif.
utf8_ou_refus <- function(x, chemin) {
  x <- as.character(x)
  tryCatch(tablexp:::octets_chaines(x, chemin),
           refus_tablexp = function(e) refus_reference(chemin, conditionMessage(e)))
  enc2utf8(x)
}

joindre <- function(chemin, nom) if (nzchar(chemin)) paste0(chemin, "/", nom) else nom

# Noms admis dans un chemin : non vides (sinon [i]), sans `/`, tabulation,
# fin de ligne, ni `@` ou `#` initial, ni forme `[i]` ; uniques.
segments <- function(x, chemin) {
  n <- names(x)
  if (is.null(n)) n <- rep("", length(x))
  if (anyNA(n)) refus_reference(chemin, "nom manquant (NA)")
  vides <- !nzchar(n)
  bad <- !vides & (grepl("[/\t\n\r]", n, useBytes = TRUE) | grepl("^[@#]", n) | grepl("^\\[[0-9]+\\]$", n))
  if (any(bad)) refus_reference(chemin, sprintf("nom inadmissible dans un chemin : %s", n[bad][1L]))
  s <- ifelse(vides, sprintf("[%d]", seq_along(n)), n)
  if (anyDuplicated(s)) refus_reference(chemin, sprintf("nom en double : %s", s[duplicated(s)][1L]))
  s
}

attributs_hors <- function(x, admis) setdiff(names(attributes(x)), admis)

# Vecteur atomique comparé : rend list(type, valeurs) normalisée, ou refuse.
atome <- function(x, chemin) {
  if (is.factor(x)) {
    a <- attributs_hors(x, c("levels", "class", "names"))
    if (length(a) || !identical(class(x), "factor"))
      refus_reference(chemin, sprintf("facteur de classe %s ou d'attribut %s", paste(class(x), collapse = "/"),
                                      paste(a, collapse = ", ")))
    return(list(type = "factor", valeurs = utf8_ou_refus(unname(x), chemin)))
  }
  if (is.object(x)) {
    if (!identical(class(x), "Date"))
      refus_reference(chemin, sprintf("classe %s hors du format (version 1)", paste(class(x), collapse = "/")))
    a <- attributs_hors(x, c("class", "names"))
    if (length(a)) refus_reference(chemin, sprintf("attribut inattendu : %s", paste(a, collapse = ", ")))
    j <- unclass(unname(x))
    if (!is.double(j) && !is.integer(j)) refus_reference(chemin, "date de type non numérique")
    j <- as.double(j)
    if (any(!is.na(j) & (!is.finite(j) | j != floor(j))))
      refus_reference(chemin, "date non entière ou infinie (non représentable en %Y-%m-%d)")
    return(list(type = "Date", valeurs = structure(j, class = "Date")))
  }
  a <- attributs_hors(x, "names")
  if (length(a)) refus_reference(chemin, sprintf("attribut inattendu : %s", paste(a, collapse = ", ")))
  type <- switch(typeof(x), double = "double", integer = "integer", logical = "logical",
                 character = "character", NA_character_)
  if (is.na(type)) refus_reference(chemin, sprintf("type %s hors du format", typeof(x)))
  v <- unname(x)
  if (type == "character") v <- utf8_ou_refus(v, chemin)
  list(type = type, valeurs = v)
}

# Représentation canonique d'une table de lignes résumée (point 1.2) :
# data.frame : par colonne « #<nom><TAB><type> » puis ses valeurs ;
# vecteur : « #<nom><TAB><type> » puis ses valeurs ; liste : par élément
# « @<nom> » puis sa représentation. Attributs hors names, class et
# row.names ignorés (l'entrée est identifiée par fiche/empreintes/donnees).
octets_resume <- function(x, nom, chemin) {
  colonne <- function(v, n) {
    cc <- tryCatch(tablexp:::ecrire_valeurs(v, "texte", n), error = function(e)
      refus_reference(joindre(chemin, n), conditionMessage(e)))
    c(charToRaw(utf8_ou_refus(sprintf("#%s\t%s\n", n, cc$type), joindre(chemin, n))), cc$octets)
  }
  if (is.data.frame(x))
    return(unlist(lapply(names(x), function(n) colonne(x[[n]], n)), use.names = FALSE))
  if (is.list(x)) {
    s <- segments(x, chemin)
    return(unlist(lapply(seq_along(x), function(i)
      c(charToRaw(utf8_ou_refus(sprintf("@%s\n", s[i]), joindre(chemin, s[i]))), octets_resume(x[[i]], s[i], joindre(chemin, s[i])))),
      use.names = FALSE))
  }
  colonne(x, nom)
}

empreinte_resume <- function(x, nom, chemin) {
  f <- tempfile("reference_resume_")
  on.exit(unlink(f))
  writeBin(c(raw(0), octets_resume(x, nom, chemin)), f)
  unname(tools::md5sum(f))
}

# Rend list(types = <chr nommé par chemin>, valeurs = <liste nommée par
# chemin>), dans l'ordre de parcours en profondeur.
aplatir_construction <- function(x) {
  if (!is.list(x) || !identical(class(x), "construction"))
    refus_reference("", sprintf("objet de classe %s au lieu de construction", paste(class(x), collapse = "/")))
  x$execution <- NULL
  if (is.list(x$fiche)) { x$fiche$versions <- NULL; x$fiche$plateforme <- NULL }
  acc <- new.env(parent = emptyenv())
  acc$types <- character(0); acc$valeurs <- list(); acc$n <- 0L
  ajouter <- function(chemin, type, valeurs) {
    acc$n <- acc$n + 1L
    acc$types[acc$n] <- type; names(acc$types)[acc$n] <- chemin
    acc$valeurs[acc$n] <- list(valeurs); names(acc$valeurs)[acc$n] <- chemin
  }
  noms <- function(chemin, x) ajouter(joindre(chemin, "@noms"), "character", segments_ecrits(x, chemin))
  segments_ecrits <- function(x, chemin) { segments(x, chemin); n <- names(x); if (is.null(n)) character(0) else utf8_ou_refus(n, chemin) }
  parcourir <- function(v, chemin, nom) {
    if (identical(dirname_chemin(chemin), CHEMIN_RESUMES) && nom %in% RESUMES_PAR_EMPREINTE) {
      ajouter(joindre(chemin, "@lignes"), "integer", as.integer(NROW(v)))
      ajouter(joindre(chemin, "@empreinte"), "character", empreinte_resume(v, nom, chemin))
      return(invisible())
    }
    if (is.null(v)) return(ajouter(chemin, "NULL", NULL))
    if (is.data.frame(v)) {
      a <- attributs_hors(v, c("names", "class", "row.names"))
      if (!identical(class(v), "data.frame") || length(a))
        refus_reference(chemin, sprintf("data.frame de classe %s ou d'attribut %s",
                                        paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
      if (.row_names_info(v, type = 1L) > 0L) refus_reference(chemin, "noms de lignes non automatiques")
      s <- segments(v, chemin)
      ajouter(joindre(chemin, "@colonnes"), "character", utf8_ou_refus(names(v), chemin))
      ajouter(joindre(chemin, "@lignes"), "integer", nrow(v))
      for (i in seq_along(v)) {
        if (is.list(v[[i]])) refus_reference(joindre(chemin, s[i]), "colonne de type liste")
        parcourir(v[[i]], joindre(chemin, s[i]), s[i])
      }
      return(invisible())
    }
    if (is.list(v)) {
      admis <- if (nzchar(chemin)) "names" else c("names", "class")
      a <- attributs_hors(v, admis)
      if (length(a) || (nzchar(chemin) && is.object(v)))
        refus_reference(chemin, sprintf("liste de classe %s ou d'attribut %s",
                                        paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
      s <- segments(v, chemin)
      noms(chemin, v)
      for (i in seq_along(v)) parcourir(v[[i]], joindre(chemin, s[i]), s[i])
      return(invisible())
    }
    if (!is.atomic(v)) refus_reference(chemin, sprintf("type %s hors du format", typeof(v)))
    r <- atome(v, chemin)
    if (!is.null(names(v))) noms(chemin, v)
    ajouter(chemin, r$type, r$valeurs)
  }
  ajouter("@classe", "character", "construction")
  parcourir(unclass(x), "", "")
  list(types = acc$types, valeurs = acc$valeurs)
}

dirname_chemin <- function(chemin) sub("/[^/]*$", "", chemin)

# --- écriture et lecture (points 2.1 à 2.4) -----------------------------------

octets_valeurs <- function(type, v, chemin) {
  if (type == "NULL" || !length(v)) return(raw(0))
  w <- if (type == "factor") factor(v, levels = unique(v)) else v
  tryCatch(tablexp:::ecrire_valeurs(w, "texte", chemin)$octets, error = function(e)
    refus_reference(chemin, conditionMessage(e)))
}

nettoyer_entete <- function(x) {
  y <- gsub("[\001-\037\177]", " ", utf8_ou_refus(x, "(en-tête)"), useBytes = TRUE)
  Encoding(y) <- "UTF-8"
  y
}

# entete : vecteur de chaînes nommé (clé -> valeur), écrit après la ligne de
# format, dans l'ordre ; une clé peut se répéter.
ecrire_reference <- function(grandeurs, fichier, entete = character(0)) {
  lf <- as.raw(10L); tab <- as.raw(9L)
  e <- c(paste0("# ", FORMAT_REFERENCE),
         if (length(entete)) sprintf("# %s : %s", nettoyer_entete(names(entete)), nettoyer_entete(entete)))
  morceaux <- vector("list", length(grandeurs$types) + 1L)
  morceaux[[1L]] <- unlist(lapply(utf8_ou_refus(e, "(en-tête)"), function(l) c(charToRaw(l), lf)), use.names = FALSE)
  chemins <- names(grandeurs$types)
  for (i in seq_along(chemins)) {
    type <- grandeurs$types[[i]]; v <- grandeurs$valeurs[[i]]
    if (!type %in% TYPES_REFERENCE) refus_reference(chemins[i], sprintf("type %s hors du format", type))
    o <- octets_valeurs(type, v, chemins[i])
    tete <- charToRaw(utf8_ou_refus(sprintf("%s\t%s\t%d", chemins[i], type, length(v)), chemins[i]))
    morceaux[[i + 1L]] <- if (length(o)) { o[o == lf] <- tab; o[length(o)] <- lf; c(tete, tab, o) } else c(tete, lf)
  }
  writeBin(unlist(morceaux, use.names = FALSE), fichier)
  invisible(fichier)
}

erreur_lecture <- function(fichier, message)
  stop(sprintf("référence illisible (%s) : %s", basename(fichier), message), call. = FALSE)

# Désechappement octet par octet (useBytes partout ; jamais substr(), qui
# compte en caractères sur une chaîne marquée UTF-8) ; le résultat est
# marqué UTF-8 et vérifié valide.
desechapper <- function(s, fichier, chemin) {
  na <- s == "NA"
  ok <- na | grepl('^".*"$', s, useBytes = TRUE)
  if (!all(ok)) erreur_lecture(fichier, sprintf("%s : chaîne sans guillemets", chemin))
  y <- sub('^"(.*)"$', "\\1", s[!na], useBytes = TRUE)
  m <- gregexpr("\\\\.|\"", y, useBytes = TRUE)
  table_esc <- c("\\\\" = "\\", "\\n" = "\n", "\\r" = "\r", "\\t" = "\t", "\\\"" = "\"")
  regmatches(y, m) <- lapply(regmatches(y, m), function(e) {
    r <- table_esc[e]
    if (anyNA(r)) erreur_lecture(fichier, sprintf("%s : échappement ou guillemet inattendu", chemin))
    unname(r)
  })
  Encoding(y) <- "UTF-8"
  if (!all(validUTF8(y))) erreur_lecture(fichier, sprintf("%s : chaîne qui n'est pas de l'UTF-8 valide", chemin))
  x <- rep(NA_character_, length(s))
  x[!na] <- y
  x
}

TEXTES_SPECIAUX_DOUBLE <- c("NA", "NaN", "Inf", "-Inf")

# Forme des textes de double que produit `%.17g` (conversion g de la norme C,
# précision 17, sans indicateur « # ») : NA, NaN, Inf, -Inf ; sinon au plus 17
# chiffres significatifs, zéros terminaux de la partie fractionnaire
# supprimés, notation fixe si l'exposant décimal X vérifie -4 <= X < 17 et
# notation à exposant (signé, deux chiffres au moins) sinon ; ni espace, ni
# « + » ou zéro de tête, ni hexadécimal. Cette forme est la même sur toutes
# les plateformes ; seuls les chiffres peuvent différer (voir double_lu()).
forme_double_17g <- function(s) {
  fixe <- grepl("^-?(0|[1-9][0-9]{0,16}(\\.[0-9]*[1-9])?|0\\.0{0,3}[1-9]([0-9]*[1-9])?)$", s)
  expo <- grepl("^-?[1-9](\\.[0-9]{0,15}[1-9])?e[-+]([0-9]{2}|[1-9][0-9]{2})$", s)
  x <- suppressWarnings(as.integer(sub("^.*e", "", s)))
  chiffres <- nchar(sub("^0+", "", gsub("[^0-9]", "", sub("e.*$", "", s))))
  s %in% TEXTES_SPECIAUX_DOUBLE | ((fixe | (expo & ((x <= -5L | x >= 17L) %in% TRUE))) & chiffres <= 17L)
}

# Lecture d'un double (ADR 0009, point 2.4 : mêmes valeurs au bit près). Un
# texte de 17 chiffres significatifs au plus désigne un seul double, mais
# as.numeric() (R_strtod) ne lit pas toujours ce double : certains décimaux
# acceptés par la forme de `%.17g` sont lus en un double voisin (ex.
# 0.001673229520730213 : 9ac77e84086a5b3f au lieu de 9bc77e84086a5b3f ;
# mesuré). Deux branches, selon la plateforme (`windows`) :
#  - hors Windows : preuve d'aller-retour. Le texte doit être égal à
#    sprintf("%.17g") de la valeur lue ; la bibliothèque C (glibc) arrondit
#    correctement (mesuré sous R 4.3.3 : 0 écart à l'arrondi correct de Python
#    sur 551 008 doubles tirés, et 0 texte relu en un autre double), donc un
#    texte ainsi reproduit est le seul texte de la valeur lue et as.numeric()
#    la lit sans erreur ; un texte lu de travers n'est pas reproduit : il est
#    refusé ;
#  - sous Windows (R 4.3.3 ucrt) : forme de `%.17g` seulement
#    (forme_double_17g()). Ce n'est pas une preuve d'aller-retour mais un
#    constat de mesure : as.numeric() relit au bit près les textes
#    correctement arrondis (0 erreur sur 300 000 tirés et sur les 42 952
#    textes des fichiers de la CI). La forme n'est pas comparée à sprintf :
#    sous Windows, sprintf n'arrondit pas toujours correctement le 17e chiffre
#    (36 694 écarts à l'arrondi correct de Python sur 551 008 doubles tirés,
#    dont 13 aux exposants décimaux de -5 à 5) et son texte ne se relit pas
#    toujours en le même double (5 sur 551 008, exposants au-delà de 266). Un
#    décimal hors de ces textes (ex. 0.001673229520730213) est accepté par la
#    forme et peut être lu de travers : les références sont produites par la
#    CI (Linux), jamais éditées à la main.
# Un dépassement (Inf) ou un sous-dépassement (0) du texte est refusé dans la
# branche Windows ; la branche stricte le refuse par la comparaison. `windows`
# et `formater` (sprintf("%.17g") d'une bibliothèque C simulée) ne servent
# qu'à tester chaque branche sur toute plateforme.
double_lu <- function(s, fichier, chemin, windows = .Platform$OS.type == "windows",
                      formater = function(v) sprintf("%.17g", v)) {
  v <- suppressWarnings(as.numeric(s))
  if (windows) {
    if (!all(forme_double_17g(s)))
      erreur_lecture(fichier, sprintf("%s : valeur hors de la forme de %%.17g (lecture sous Windows : forme seule, voir double_lu())", chemin))
    if (any(!s %in% TEXTES_SPECIAUX_DOUBLE & (!is.finite(v) | (v == 0 & !s %in% c("0", "-0")))))
      erreur_lecture(fichier, sprintf("%s : valeur hors de l'intervalle des doubles", chemin))
  } else if (!identical(formater(v), s)) {
    erreur_lecture(fichier, sprintf("%s : valeur non canonique pour le type double (différente de sprintf(\"%%.17g\") de la valeur lue)", chemin))
  }
  v
}

# Écriture canonique d'un vecteur de Date (ADR 0011, point 4 ; #19) pour le
# contrôle de lecture et les messages : %04d-%02d-%02d sur as.POSIXlt(), jetons
# NA et NaN. Ne passe pas par tablexp:::ecrire_valeurs() : la lecture et la
# comparaison s'exécutent sans installation de tablexp (comparer_references.R) ;
# c'est aussi une seconde écriture, indépendante de celle du moteur.
texte_dates <- function(v) {
  lt <- as.POSIXlt(structure(as.double(unclass(v)), class = "Date"))
  t <- sprintf("%04d-%02d-%02d", lt$year + 1900L, lt$mon + 1L, lt$mday)
  t[is.na(v)] <- "NA"; t[is.nan(unclass(v))] <- "NaN"
  t
}

valeurs_lues <- function(type, s, fichier, chemin) {
  canonique <- function(v, texte) {
    if (!identical(texte, s)) erreur_lecture(fichier, sprintf("%s : valeur non canonique pour le type %s", chemin, type))
    v
  }
  switch(type,
    double = double_lu(s, fichier, chemin),
    integer = { v <- suppressWarnings(as.integer(s)); canonique(v, ifelse(is.na(v), "NA", sprintf("%d", v))) },
    logical = { v <- c("TRUE" = TRUE, "FALSE" = FALSE, "NA" = NA)[s]
                if (anyNA(names(v))) erreur_lecture(fichier, sprintf("%s : logique invalide", chemin)); unname(v) },
    # Date : jetons NA et NaN lus tels quels (NaN en Date stockée NaN) ; forme
    # canonique (année sur quatre chiffres, ADR 0011, point 4 ; #19) contrôlée
    # par la réécriture de la valeur lue, texte_dates().
    Date = { u <- as.double(unclass(as.Date(ifelse(s %in% c("NA", "NaN"), NA_character_, s), format = "%Y-%m-%d")))
             u[s == "NaN"] <- NaN
             v <- structure(u, class = "Date")
             canonique(v, texte_dates(v)) },
    character = , factor = desechapper(s, fichier, chemin),
    erreur_lecture(fichier, sprintf("%s : type %s inconnu", chemin, type)))
}

lire_reference <- function(fichier) {
  o <- readBin(fichier, "raw", file.info(fichier)$size)
  if (!length(o)) erreur_lecture(fichier, "fichier vide")
  if (any(o == as.raw(13L))) erreur_lecture(fichier, "retour chariot (CR) présent")
  if (o[length(o)] != as.raw(10L)) erreur_lecture(fichier, "dernière ligne sans LF")
  texte <- rawToChar(o)
  Encoding(texte) <- "UTF-8"
  if (!validUTF8(texte)) erreur_lecture(fichier, "UTF-8 invalide")
  l <- strsplit(texte, "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  Encoding(l) <- "UTF-8"
  if (!identical(l[1L], paste0("# ", FORMAT_REFERENCE)))
    erreur_lecture(fichier, sprintf("première ligne « %s » au lieu de « # %s »", l[1L], FORMAT_REFERENCE))
  n_entete <- which(!startsWith(l, "#"))[1L] - 1L
  if (is.na(n_entete)) n_entete <- length(l)
  e <- sub("^# ", "", l[seq.int(2L, length.out = n_entete - 1L)])
  entete <- stats::setNames(sub("^.*? : ", "", e, perl = TRUE), sub(" : .*$", "", e, perl = TRUE))
  corps <- l[seq.int(n_entete + 1L, length.out = length(l) - n_entete)]
  champs <- lapply(strsplit(corps, "\t", fixed = TRUE, useBytes = TRUE), function(f) { Encoding(f) <- "UTF-8"; f })
  chemins <- vapply(champs, `[`, "", 1L)
  if (anyDuplicated(chemins)) erreur_lecture(fichier, sprintf("chemin en double : %s", chemins[duplicated(chemins)][1L]))
  types <- stats::setNames(vapply(champs, function(f) if (length(f) >= 2L) f[2L] else NA_character_, ""), chemins)
  valeurs <- stats::setNames(vector("list", length(champs)), chemins)
  for (i in seq_along(champs)) {
    f <- champs[[i]]
    if (length(f) < 3L || !types[[i]] %in% TYPES_REFERENCE)
      erreur_lecture(fichier, sprintf("ligne %d : chemin, type et longueur attendus", n_entete + i))
    n <- suppressWarnings(as.integer(f[3L]))
    if (is.na(n) || !identical(sprintf("%d", n), f[3L]) || length(f) - 3L != n)
      erreur_lecture(fichier, sprintf("%s : longueur %s pour %d valeur(s)", f[1L], f[3L], length(f) - 3L))
    if (types[[i]] == "NULL") {
      if (n != 0L) erreur_lecture(fichier, sprintf("%s : NULL de longueur %d", f[1L], n))
      next
    }
    valeurs[i] <- list(if (n) valeurs_lues(types[[i]], f[-(1:3)], fichier, f[1L]) else
      switch(types[[i]], double = numeric(0), integer = integer(0), logical = logical(0),
             Date = structure(numeric(0), class = "Date"), character(0)))
  }
  list(types = types, valeurs = valeurs, entete = entete)
}

# --- tolérances et comparaison (point 3) --------------------------------------

# Clés de `tolerances` qui classent `chemin`, de la plus longue à la plus
# courte (suffixes du chemin, segment par segment).
cles_de <- function(chemin, tolerances = TOLERANCES) {
  s <- strsplit(chemin, "/", fixed = TRUE)[[1L]]
  suffixes <- vapply(seq_along(s), function(k) paste(s[k:length(s)], collapse = "/"), "")
  suffixes[suffixes %in% names(tolerances)]
}
tolerance_de <- function(chemin, tolerances = TOLERANCES) {
  k <- cles_de(chemin, tolerances)
  if (!length(k)) return(list(cle = NA_character_, tol = NA_real_, autres = character(0)))
  list(cle = k[1L], tol = unname(tolerances[[k[1L]]]), autres = k[-1L])
}

texte_valeur <- function(v, type) {
  if (type == "double") return(sprintf("%.17g", v))
  if (type == "Date") return(texte_dates(v))
  if (type %in% c("character", "factor")) return(ifelse(is.na(v), "NA", sprintf("\"%s\"", encodeString(v))))
  ifelse(is.na(v), "NA", as.character(v))
}

# Paires élément par élément (seconde annotation, (c)) : identiques (au sens
# d'identical(), NA et NaN distingués), sinon jugées par la règle relative.
identiques_elements <- function(a, b) {
  if (is.double(a)) return((!is.na(a) & !is.na(b) & a == b) | (is.na(a) & is.na(b) & is.nan(a) == is.nan(b)))
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

comparer_references <- function(reference, obtenu, tolerances = TOLERANCES) {
  ch_r <- names(reference$types); ch_o <- names(obtenu$types)
  structure_ <- character(0)
  # Entrée, en premier (point 3.2).
  e_r <- if (CHEMIN_EMPREINTE_DONNEES %in% ch_r) reference$valeurs[[CHEMIN_EMPREINTE_DONNEES]] else NA_character_
  e_o <- if (CHEMIN_EMPREINTE_DONNEES %in% ch_o) obtenu$valeurs[[CHEMIN_EMPREINTE_DONNEES]] else NA_character_
  # Représentation canonique des deux côtés (NA si absente) : troisième cause
  # d'une entrée différente (annotation du 19/10/2026 (troisième), (f)).
  rep_de <- function(g) if (CHEMIN_REPRESENTATION %in% names(g$types) && length(g$valeurs[[CHEMIN_REPRESENTATION]]) == 1L)
    as.character(g$valeurs[[CHEMIN_REPRESENTATION]]) else NA_character_
  entree <- list(reference = e_r, obtenu = e_o, differente = !identical(e_r, e_o),
                 representation_reference = rep_de(reference), representation_obtenue = rep_de(obtenu))
  # Exigée des deux côtés (point 3.2) : son absence n'est jamais une égalité.
  for (cote in c("référence", "obtenu"))
    if (anyNA(if (cote == "obtenu") e_o else e_r))
      structure_ <- c(structure_, sprintf("%s : empreinte des données absente (%s)", CHEMIN_EMPREINTE_DONNEES, cote))
  for (c_ in setdiff(ch_r, ch_o)) structure_ <- c(structure_, sprintf("%s : manquant", c_))
  for (c_ in setdiff(ch_o, ch_r)) structure_ <- c(structure_, sprintf("%s : en trop", c_))
  communs <- ch_r[ch_r %in% ch_o]
  n <- length(communs)
  g <- data.frame(chemin = communs, type = character(n), longueur = integer(n), cle = NA_character_,
                  tolerance = NA_real_, n_differents = integer(n), ecart_relatif_max = NA_real_,
                  position = NA_integer_, avant = NA_character_, apres = NA_character_,
                  verdict = NA_character_, stringsAsFactors = FALSE)
  garde <- rep(TRUE, n)
  cm <- list(chemin = character(0), cle = character(0), classe = character(0), autres = character(0))
  for (k in seq_len(n)) {
    c_ <- communs[k]
    t_r <- reference$types[[c_]]; t_o <- obtenu$types[[c_]]
    a <- reference$valeurs[[c_]]; b <- obtenu$valeurs[[c_]]
    if (!identical(t_r, t_o)) {
      structure_ <- c(structure_, sprintf("%s : type %s au lieu de %s", c_, t_o, t_r)); garde[k] <- FALSE; next
    }
    if (length(a) != length(b)) {
      structure_ <- c(structure_, sprintf("%s : longueur %d au lieu de %d", c_, length(b), length(a)))
      garde[k] <- FALSE; next
    }
    tol <- if (t_r == "double") tolerance_de(c_, tolerances) else list(cle = NA_character_, tol = 0, autres = character(0))
    if (length(tol$autres)) {
      cm$chemin <- c(cm$chemin, c_); cm$cle <- c(cm$cle, tol$cle)
      cm$classe <- c(cm$classe, nom_classe(tol$tol)); cm$autres <- c(cm$autres, paste(tol$autres, collapse = ", "))
    }
    va <- if (t_r == "Date") unclass(a) else a; vb <- if (t_r == "Date") unclass(b) else b
    id <- if (t_r == "NULL") logical(0) else identiques_elements(va, vb)
    if (all(id)) {
      g$type[k] <- t_r; g$longueur[k] <- length(a); g$cle[k] <- tol$cle; g$tolerance[k] <- tol$tol
      g$ecart_relatif_max[k] <- if (t_r == "double") 0 else NA_real_
      g$verdict[k] <- if (t_r == "double" && is.na(tol$tol)) "sans tolérance déclarée" else "identique"
      next
    }
    non_fini <- if (t_r == "double") !is.finite(va) | !is.finite(vb) else is.na(va) | is.na(vb)
    pos_nf <- which(!id & non_fini)
    if (length(pos_nf)) {
      i <- pos_nf[1L]
      structure_ <- c(structure_, sprintf("%s : position des valeurs non finies ou manquantes différente (%d élément(s) ; élément %d : %s au lieu de %s)",
                                          c_, length(pos_nf), i, texte_valeur(b[i], t_r), texte_valeur(a[i], t_r)))
    }
    diff <- which(!id & !non_fini)
    rel <- if (t_r == "double" && length(diff))
      abs(va[diff] - vb[diff]) / pmax(abs(va[diff]), abs(vb[diff])) else numeric(0)
    meme_signe <- if (length(diff) && t_r == "double") sign(va[diff]) == sign(vb[diff]) & va[diff] != 0 & vb[diff] != 0 else logical(0)
    sans_tol <- t_r == "double" && is.na(tol$tol)
    hors <- if (t_r != "double" || sans_tol) rep(TRUE, length(diff)) else !(meme_signe & rel <= tol$tol)
    j <- if (length(rel)) diff[which.max(rel)] else if (length(diff)) diff[1L]
         else if (length(pos_nf)) pos_nf[1L] else NA_integer_
    g$type[k] <- t_r; g$longueur[k] <- length(a); g$cle[k] <- tol$cle; g$tolerance[k] <- tol$tol
    g$n_differents[k] <- length(diff) + length(pos_nf)
    g$ecart_relatif_max[k] <- if (length(rel)) max(rel) else if (t_r == "double") 0 else NA_real_
    g$position[k] <- j
    if (!is.na(j)) { g$avant[k] <- texte_valeur(a[j], t_r); g$apres[k] <- texte_valeur(b[j], t_r) }
    g$verdict[k] <- if (sans_tol) "sans tolérance déclarée"
                    else if (length(pos_nf) || any(hors)) "écart"
                    else if (length(diff)) "sous tolérance" else "identique"
  }
  g <- g[garde, , drop = FALSE]
  rownames(g) <- NULL
  cm <- as.data.frame(cm, stringsAsFactors = FALSE)
  sans <- g$chemin[g$verdict == "sans tolérance déclarée"]
  ok <- !entree$differente && !anyNA(c(e_r, e_o)) && !length(structure_) && all(g$verdict %in% c("identique", "sous tolérance"))
  list(ok = ok, entree = entree, structure = structure_, grandeurs = g, sans_tolerance = sans,
       cles_multiples = cm)
}

# Doubles de `grandeurs` qu'aucune clé ne classe (règle de complétude,
# point 3.4) ; appliquée aussi à la production.
doubles_sans_tolerance <- function(grandeurs, tolerances = TOLERANCES) {
  d <- names(grandeurs$types)[grandeurs$types == "double"]
  d[vapply(d, function(c_) is.na(tolerance_de(c_, tolerances)$tol), NA)]
}

# Impression du point 3.5 ; rend les lignes (character).
imprimer_comparaison <- function(r, titre = "") {
  g <- r$grandeurs
  out <- if (nzchar(titre)) titre else character(0)
  out <- c(out, if (r$entree$differente)
    c(sprintf("  ENTRÉE DIFFÉRENTE : empreinte des données %s (référence) contre %s (obtenu)",
              r$entree$reference, r$entree$obtenu),
      "  entrée différente : écarts non interprétables comme un tableau avant / après ;",
      "  causes possibles : le simulateur a changé pour cette graine (ADR 0009, point 4.2), ou la plateforme a produit un tirage différent (ADR 0008, hypothèse sur la libm)",
      if (!is.na(r$entree$representation_reference) && !is.na(r$entree$representation_obtenue) &&
          !identical(r$entree$representation_reference, r$entree$representation_obtenue))
        sprintf("  ou la représentation canonique a changé (%s : « %s » (référence) contre « %s » (obtenu)) ; une entrée inchangée se prouve par la garde 3.2 de tests/test_reproductibilite.R",
                CHEMIN_REPRESENTATION, r$entree$representation_reference, r$entree$representation_obtenue))
    else sprintf("  Entrée : empreinte des données identique (%s)", r$entree$reference))
  out <- c(out, sprintf("  Bilan : %d grandeur(s) comparée(s) ; %d identique(s) au bit près ; %d à écart non nul sous tolérance ; %d en écart ; %d sans tolérance déclarée ; %d écart(s) structurel(s)",
                        nrow(g), sum(g$verdict == "identique"), sum(g$verdict == "sous tolérance"),
                        sum(g$verdict == "écart"), sum(g$verdict == "sans tolérance déclarée"), length(r$structure)))
  d <- g[g$type == "double" & !is.na(g$tolerance), , drop = FALSE]
  for (k in names(CLASSES_TOLERANCE)) {
    x <- d$ecart_relatif_max[d$tolerance == CLASSES_TOLERANCE[[k]]]
    out <- c(out, sprintf("  Écart relatif maximal, classe %-6s : %.3g (%d grandeur(s) double)", k,
                          if (length(x)) max(x) else 0, length(x)))
  }
  if (length(r$structure)) out <- c(out, paste0("  STRUCTURE : ", r$structure))
  for (c_ in r$sans_tolerance) out <- c(out, sprintf("  grandeur sans tolérance déclarée : %s", c_))
  if (nrow(r$cles_multiples))
    out <- c(out, sprintf("  clé retenue : %s -> %s (classe %s ; aussi : %s)", r$cles_multiples$chemin,
                          r$cles_multiples$cle, r$cles_multiples$classe, r$cles_multiples$autres))
  e <- g[g$n_differents > 0L, , drop = FALSE]
  if (nrow(e))
    out <- c(out, "  Grandeurs à écart non nul (chemin | longueur | éléments différents | écart relatif max | position | avant | après | tolérance | verdict) :",
             sprintf("    %s | %d | %d | %s | %d | %s | %s | %s | %s", e$chemin, e$longueur, e$n_differents,
                     ifelse(is.na(e$ecart_relatif_max), "-", sprintf("%.3g", e$ecart_relatif_max)), e$position,
                     e$avant, e$apres, ifelse(is.na(e$tolerance), "aucune", nom_classe(e$tolerance)), e$verdict))
  c(out, sprintf("  Verdict : %s", if (r$ok) "OK" else "ECHEC"))
}

# Fichiers <id>.tsv de `dossier` sans plan de référence (point 3.7).
references_orphelines <- function(dossier, plans = plans_reference()) {
  f <- list.files(dossier, pattern = "\\.tsv$")
  f[!sub("\\.tsv$", "", f) %in% names(plans)]
}

# Identifiant de plateforme (ADR 0008, point 5) : lignes contrôlées par
# tests/controles/plateforme.R, et lignes consignées sans contrôle.
identifiant_plateforme <- function(racine) {
  env <- new.env()
  charger_utf8(file.path(racine, "tests", "controles", "plateforme.R"), env)
  r <- env$relever_plateforme()
  list(controle = trimws(env$lignes_identifiant(r)), consigne = trimws(env$lignes_consignees()),
       defauts = env$defauts_plateforme(r))
}

# --- règles des modes du workflow (point 5.2) ----------------------------------

# Rend NA si le plan est admis dans ce mode, sinon la raison du refus.
# vise : plan dans --plans ; present : fichier commité ; reussie :
# comparaison au fichier commité réussie (FALSE s'il est absent).
regle_mode <- function(mode, vise, present, reussie) {
  if (mode == "releve") return(NA_character_)
  if (mode == "bascule") return(if (present) NA_character_ else "fichier commité absent")
  if (vise && mode == "creation")
    return(if (!present) NA_character_ else "un fichier commité existe déjà (une création ne modifie pas une référence)")
  if (vise && mode == "regeneration")
    return(if (present) NA_character_ else "fichier commité absent (c'est une création)")
  if (!present) return("fichier commité absent")
  if (!reussie) return("comparaison en écart")
  NA_character_
}

# Bascule : les en-têtes commités portent un même identifiant de plateforme,
# différent de celui de l'exécution. Rend NA si c'est le cas, sinon la raison.
controle_bascule <- function(plateformes_commises, courante) {
  u <- unique(plateformes_commises)
  if (length(u) != 1L || !length(u[[1L]]))
    return("identifiants de plateforme des en-têtes commités absents ou différents entre eux")
  if (identical(u[[1L]], courante))
    return("l'identifiant de plateforme de l'exécution est celui des en-têtes commités (ce n'est pas une bascule)")
  NA_character_
}

# Forme comparable d'un chemin, par le texte seul (aucun accès au système de
# fichiers) : absolu (relatif à `courant`), séparateur « / », sans « . », « .. »
# ni « / » final (`reduire = FALSE` conserve les « .. » : le texte est alors
# résolu par le système avant toute réduction, voir sortie_interdite()).
# `windows` : « \ » est un séparateur, un chemin absolu est `X:/...`,
# `//serveur/partage/...` (UNC, préfixes `\\?\` et `\\?\UNC\` compris) ou
# `/...` (lecteur du répertoire courant), `X:rel` est relatif au répertoire
# courant du lecteur X (ramené à `courant` si c'est le lecteur courant), et la
# casse est ignorée (lettre de lecteur comprise).
forme_chemin <- function(p, courant, windows = .Platform$OS.type == "windows", reduire = TRUE) {
  prefixe <- ""
  if (windows) {
    p <- gsub("\\", "/", p, fixed = TRUE); courant <- gsub("\\", "/", courant, fixed = TRUE)
    p <- sub("^//[?.]/", "", sub("^//[?.]/UNC/", "//", p))
    lecteur <- function(x) if (grepl("^[A-Za-z]:", x)) toupper(substr(x, 1L, 2L)) else ""
    if (grepl("^[A-Za-z]:([^/]|$)", p)) {
      base <- if (identical(lecteur(p), lecteur(courant))) courant else substr(p, 1L, 2L)
      p <- paste0(base, "/", substring(p, 3L))
    } else if (grepl("^/([^/]|$)", p)) {
      p <- paste0(lecteur(courant), p)
    } else if (!grepl("^(//|[A-Za-z]:/)", p)) {
      p <- paste0(courant, "/", p)
    }
    if (startsWith(p, "//")) { prefixe <- "/"; p <- substring(p, 2L) }
    else if (grepl("^[A-Za-z]:", p)) { prefixe <- substr(p, 1L, 2L); p <- substring(p, 3L) }
    p <- tolower(p); prefixe <- tolower(prefixe)
  } else if (!startsWith(p, "/")) p <- paste0(courant, "/", p)
  pile <- character(0)
  for (seg in strsplit(p, "/", fixed = TRUE)[[1L]]) {
    if (!nzchar(seg) || identical(seg, ".")) next
    if (reduire && identical(seg, "..")) pile <- pile[-length(pile)] else pile <- c(pile, seg)
  }
  paste0(prefixe, "/", paste(pile, collapse = "/"))
}

# Le dossier de sortie est-il tests/reference/ ou l'un de ses sous-dossiers ?
# Refusé si l'une des deux formes l'indique : la forme du texte (portable :
# séparateurs, lecteur, casse, UNC) et celle que résout le système de fichiers
# pour la partie existante (liens, noms courts), seulement si `windows` est
# la plateforme de l'hôte et `resoudre` vrai. `courant`, `windows` et
# `resoudre = FALSE` ne servent qu'à tester, par le texte seul et sur
# n'importe quelle plateforme, les formes de chemin d'une autre.
sortie_interdite <- function(sortie, commis, courant = getwd(), windows = .Platform$OS.type == "windows",
                             resoudre = TRUE) {
  dedans <- function(s, c_) identical(s, c_) || startsWith(s, paste0(c_, "/"))
  texte <- function(p) forme_chemin(p, courant, windows)
  # Ancêtre existant du texte BRUT (« .. » non réduits : sous POSIX, `<lien>/..`
  # est le parent de la cible du lien, pas celui du lien), résolu par le système ;
  # le reste du chemin lui est rattaché puis réduit.
  resolu <- function(p) {
    t <- forme_chemin(p, courant, windows, reduire = FALSE); reste <- character(0)
    while (!file.exists(t) && !identical(dirname(t), t)) { reste <- c(basename(t), reste); t <- dirname(t) }
    forme_chemin(file.path(normalizePath(t, winslash = "/", mustWork = FALSE), paste(reste, collapse = "/")), courant, windows)
  }
  dedans(texte(sortie), texte(commis)) ||
    (resoudre && identical(windows, .Platform$OS.type == "windows") && dedans(resolu(sortie), resolu(commis)))
}

# En-tête d'une référence (point 2.2), construit ici pour que ses clés non
# ASCII soient des littéraux marqués UTF-8.
entete_reference <- function(p, plateforme, x, ci, mode, date_utc) {
  c(identifiant = p$identifiant, description = p$description,
    stats::setNames(plateforme$controle, rep("plateforme", length(plateforme$controle))),
    stats::setNames(plateforme$consigne, rep("plateforme consignée", length(plateforme$consigne))),
    stats::setNames(unlist(x$fiche$versions), paste0("fiche$versions/", names(x$fiche$versions))),
    stats::setNames(unlist(x$fiche$plateforme), paste0("fiche$plateforme/", names(x$fiche$plateforme))),
    commit = ci$commit, execution = ci$execution, adresse = ci$adresse, mode = mode,
    date = date_utc,
    # Clé en chaîne, jamais en étiquette de c() : une étiquette est un symbole,
    # traduit en natif par parse() (« donn<U+00E9>es » en locale C, mesuré).
    stats::setNames(x$fiche$empreintes$donnees, "empreinte des données"))
}

# Lignes de texte en octets UTF-8, une par ligne, LF final (ecarts.txt).
octets_texte <- function(lignes) unlist(lapply(lignes, function(l) c(charToRaw(l), as.raw(10L))), use.names = FALSE)
