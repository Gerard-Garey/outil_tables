# -----------------------------------------------------------------------------
#  tests/controles/absence_java.R : absence de Java dans les dépendances
#  récursives des packages du projet (ADR 0002, A1, M40 ; ADR 0008, point 7 ;
#  issue #3)
#
#  Ensembles contrôlés :
#    - application et contre-vérification : packages listés dans
#      tests/controles/packages_hors_moteur.csv (colonnes ensemble, package,
#      statut, source ; un seul fichier, lu par ce script) ;
#    - moteur : Depends, Imports et LinkingTo de DESCRIPTION hors R (vides
#      tant qu'aucun ADR de méthode n'admet de package ; la liste blanche de
#      DESCRIPTION, hors réseau, reste le contrôle de tablexp) ;
#    - témoin : packages ajoutés par --ajouter (cas témoin : ELT, qui dépend
#      de Java par xlsx).
#
#  Règles (code de sortie 1 si l'une est violée) :
#    - aucun package de PACKAGES_JAVA dans un ensemble ni dans la fermeture
#      récursive de ses dépendances (Depends, Imports, LinkingTo, par
#      tools::package_dependencies(recursive = TRUE)) ; la chaîne qui y mène
#      est affichée ;
#    - chaque package listé, et chaque dépendance récursive hors priorité
#      base de R 4.3.3, figure dans les métadonnées : un package introuvable
#      n'a pas de dépendances connues, il ne passe pas en silence ;
#    - le fichier de liste est bien formé (en-tête, ensembles et statuts
#      connus, noms de packages valides, sans doublon, ensembles non vides).
#
#  Métadonnées : instantané daté du CRAN chez Posit Package Manager
#  (DEPOT_INSTANTANE), lu par available.packages() sans rien installer. La
#  date ne change que par un commit `repo:` (ADR 0008, point 7) ; elle est
#  choisie un jour ouvré. Seul le filtre « duplicates » est appliqué. Le
#  filtre par version de R écarterait des métadonnées les packages qui
#  exigent un R plus récent que celui qui exécute le contrôle, alors que la
#  contre-vérification peut tourner sur un autre poste muni d'un R récent
#  (ADR 0002, A11). Mesuré sous R 4.3.3 sur le CRAN du 17/10/2026 : mgcv,
#  MortalityLaws, lifecontingencies, Matrix et MASS écartés. Ils seraient
#  alors signalés absents des métadonnées, donc en faux échec, non en
#  succès silencieux. Le filtre par système d'exploitation est écarté aussi,
#  le poste étant sous Windows et la CI sous Linux. Les dépendances sont
#  celles de la version la plus récente de chaque package dans l'instantané.
#
#  Limite : la détection porte sur les seuls noms de PACKAGES_JAVA. Le champ
#  SystemRequirements, qui pourrait déclarer Java, n'est pas renseigné dans
#  le fichier PACKAGES du CRAN (mesuré le 17/10/2026 : NA pour tous les
#  packages, rJava compris). Un package qui exigerait Java sans dépendre de
#  l'un de ces noms échapperait au contrôle. Étendre la détection revient au
#  mainteneur.
#
#  Usage (depuis n'importe où) :
#      Rscript tests/controles/absence_java.R [--depot <url>] [--liste <csv>]
#              [--description <DESCRIPTION>] [--ajouter <p1,p2,...>]
#  --depot remplace l'instantané (miroir, ou dépôt local file:// des tests).
#  Codes de sortie : 0 conforme ; 1 règle violée ; 2 métadonnées illisibles
#  (panne réseau ou adresse fausse : aucun verdict, ADR 0008, point 7).
#  Sourcé (tests), le fichier ne fait que définir les fonctions.
# -----------------------------------------------------------------------------

INSTANTANE <- "2026-10-15"  # jeudi, jour ouvré
DEPOT_INSTANTANE <- sprintf("https://packagemanager.posit.co/cran/%s", INSTANTANE)

PACKAGES_JAVA <- c("rJava", "xlsx", "xlsxjars")
CHAMPS_DEPENDANCES <- c("Depends", "Imports", "LinkingTo")
FILTRES <- "duplicates"

COLONNES_LISTE <- c("ensemble", "package", "statut", "source")
ENSEMBLES_LISTE <- c("application", "contre_verification")
STATUTS_LISTE <- c("obligatoire", "facultatif")
MOTIF_PACKAGE <- "^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$"

# PACKAGES_BASE_R433 et lire_dependances(), tenus par la liste blanche de
# DESCRIPTION (même dossier).
charger_liste_blanche <- function(dossier) {
  env <- new.env()
  sys.source(file.path(dossier, "liste_blanche_description.R"), envir = env)
  env
}

# Rend list(liste = data.frame, defauts = character).
lire_liste <- function(chemin) {
  if (!file.exists(chemin)) return(list(liste = NULL, defauts = sprintf("liste absente : %s", chemin)))
  l <- tryCatch(utils::read.csv(chemin, colClasses = "character", strip.white = TRUE,
                                na.strings = character(0), encoding = "UTF-8"),
                error = function(e) e)
  if (inherits(l, "error"))
    return(list(liste = NULL, defauts = paste("liste illisible :", conditionMessage(l))))
  if (!identical(names(l), COLONNES_LISTE))
    return(list(liste = NULL, defauts = sprintf("liste : colonnes « %s » au lieu de « %s »",
                                                 paste(names(l), collapse = ","),
                                                 paste(COLONNES_LISTE, collapse = ","))))
  d <- character(0)
  for (i in which(!(l$ensemble %in% ENSEMBLES_LISTE)))
    d <- c(d, sprintf("liste, ligne %d : ensemble « %s » inconnu", i + 1L, l$ensemble[i]))
  for (i in which(!(l$statut %in% STATUTS_LISTE)))
    d <- c(d, sprintf("liste, ligne %d : statut « %s » inconnu", i + 1L, l$statut[i]))
  for (i in which(!grepl(MOTIF_PACKAGE, l$package)))
    d <- c(d, sprintf("liste, ligne %d : nom de package « %s » invalide", i + 1L, l$package[i]))
  for (i in which(duplicated(l[c("ensemble", "package")])))
    d <- c(d, sprintf("liste, ligne %d : %s en double dans l'ensemble %s", i + 1L, l$package[i], l$ensemble[i]))
  for (e in setdiff(ENSEMBLES_LISTE, l$ensemble))
    d <- c(d, sprintf("liste : ensemble %s vide", e))
  list(liste = l, defauts = d)
}

# Packages déclarés par DESCRIPTION dans CHAMPS_DEPENDANCES, hors R.
packages_moteur <- function(chemin, lb) {
  if (!file.exists(chemin)) return(structure(character(0), defaut = sprintf("fichier absent : %s", chemin)))
  dcf <- tryCatch(read.dcf(chemin), error = function(e) e)
  if (inherits(dcf, "error"))
    return(structure(character(0), defaut = paste("DESCRIPTION illisible :", conditionMessage(dcf))))
  p <- character(0); defauts <- character(0)
  for (n in intersect(CHAMPS_DEPENDANCES, colnames(dcf))) {
    d <- lb$lire_dependances(dcf[1L, n])
    defauts <- c(defauts, sprintf("DESCRIPTION, %s : entrée illisible « %s »", n, d$brut[is.na(d$package)]))
    p <- c(p, d$package[!is.na(d$package)])
  }
  structure(unique(p[p != "R"]), defaut = if (length(defauts)) defauts)
}

# Métadonnées du dépôt : list(db = matrice d'available.packages, ou NULL si
# illisibles ; motif = erreurs et avertissements de la lecture).
lire_metadonnees <- function(depot) {
  motif <- character(0)
  db <- tryCatch(withCallingHandlers(
    utils::available.packages(repos = depot, type = "source", filters = FILTRES),
    warning = function(w) { motif <<- c(motif, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) { motif <<- c(motif, conditionMessage(e)); NULL })
  if (!is.null(db) && !nrow(db)) db <- NULL
  if (is.null(db)) motif <- c(motif, "aucun package lu")
  list(db = db, motif = paste(motif, collapse = " ; "))
}

# Chaîne la plus courte de `depart` à `cible` dans le graphe des dépendances
# directes `directes` (liste nommée), par parcours en largeur.
chaine <- function(depart, cible, directes) {
  precedent <- character(0)
  file_attente <- depart
  vus <- depart
  while (length(file_attente)) {
    n <- file_attente[1L]; file_attente <- file_attente[-1L]
    if (identical(n, cible)) break
    for (s in setdiff(directes[[n]], vus)) {
      precedent[s] <- n
      vus <- c(vus, s)
      file_attente <- c(file_attente, s)
    }
  }
  if (!(cible %in% vus)) return(NA_character_)
  ch <- cible
  while (!identical(ch[1L], depart)) ch <- c(precedent[[ch[1L]]], ch)
  paste(ch, collapse = " -> ")
}

# `ensembles` : liste nommée de vecteurs de packages ; `db` : métadonnées ;
# `base` : packages de priorité base (sans dépendances hors base). Rend les
# défauts, avec en attribut le relevé (ensemble, package, version,
# dependances).
defauts_java <- function(ensembles, db, base) {
  d <- character(0)
  releve <- data.frame(ensemble = character(0), package = character(0), version = character(0),
                       dependances = integer(0), stringsAsFactors = FALSE)
  connus <- rownames(db)
  for (e in names(ensembles)) for (p in ensembles[[e]]) {
    if (p %in% PACKAGES_JAVA) {
      d <- c(d, sprintf("%s : %s est un package Java", e, p))
      next
    }
    if (p %in% base) {
      releve[nrow(releve) + 1L, ] <- list(e, p, "(base)", 0L)
      next
    }
    if (!(p %in% connus)) {
      d <- c(d, sprintf("%s : %s absent des métadonnées (dépendances inconnues)", e, p))
      next
    }
    ferm <- tools::package_dependencies(p, db = db, which = CHAMPS_DEPENDANCES, recursive = TRUE)[[1L]]
    releve[nrow(releve) + 1L, ] <- list(e, p, db[p, "Version"], length(ferm))
    for (q in setdiff(ferm, c(connus, base)))
      d <- c(d, sprintf("%s : %s, dépendance %s absente des métadonnées (dépendances inconnues)", e, p, q))
    java <- intersect(PACKAGES_JAVA, ferm)
    if (length(java)) {
      noeuds <- intersect(c(p, ferm), connus)
      directes <- tools::package_dependencies(noeuds, db = db, which = CHAMPS_DEPENDANCES, recursive = FALSE)
      for (j in java)
        d <- c(d, sprintf("%s : %s dépend de Java (%s)", e, p, chaine(p, j, directes)))
    }
  }
  attr(d, "releve") <- releve
  d
}

valeur_option <- function(args, option) {
  i <- match(option, args)
  if (is.na(i)) return(NULL)
  if (i == length(args) || startsWith(args[i + 1L], "--")) return(NA_character_)
  args[i + 1L]
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  dossier <- dirname(normalizePath(script))
  racine <- dirname(dirname(dossier))
  erreur <- function(defauts, statut = 1L) {
    cat(paste0("  ECHEC : ", defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Absence de Java::", paste(defauts, collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = statut)
  }
  options <- c("--depot", "--liste", "--description", "--ajouter")
  valeurs <- lapply(stats::setNames(options, options), valeur_option, args = args)
  inconnues <- setdiff(grep("^--", args, value = TRUE), options)
  manquantes <- names(Filter(function(v) !is.null(v) && is.na(v), valeurs))
  if (length(inconnues) || length(manquantes))
    erreur(c(sprintf("option inconnue : %s", inconnues), sprintf("option sans valeur : %s", manquantes)))
  depot <- if (is.null(valeurs[["--depot"]])) DEPOT_INSTANTANE else valeurs[["--depot"]]
  liste <- if (is.null(valeurs[["--liste"]])) file.path(dossier, "packages_hors_moteur.csv") else valeurs[["--liste"]]
  description <- if (is.null(valeurs[["--description"]])) file.path(racine, "DESCRIPTION") else valeurs[["--description"]]
  ajoutes <- if (is.null(valeurs[["--ajouter"]])) character(0) else
    trimws(strsplit(valeurs[["--ajouter"]], ",", fixed = TRUE)[[1L]])

  cat("Absence de Java : dépôt", depot, "\n")
  cat("  liste :", liste, "\n")
  lb <- charger_liste_blanche(dossier)
  l <- lire_liste(liste)
  defauts <- l$defauts
  moteur <- packages_moteur(description, lb)
  defauts <- c(defauts, attr(moteur, "defaut"))
  if (length(defauts)) erreur(defauts)
  ensembles <- split(l$liste$package, factor(l$liste$ensemble, levels = ENSEMBLES_LISTE))
  ensembles <- c(list(moteur = as.character(moteur)), ensembles)
  if (length(ajoutes)) ensembles$temoin <- ajoutes

  m <- lire_metadonnees(depot)
  if (is.null(m$db)) erreur(sprintf("métadonnées illisibles (%s) : %s", depot, m$motif), statut = 2L)
  db <- m$db
  cat(sprintf("  métadonnées : %d packages\n", nrow(db)))
  defauts <- defauts_java(ensembles, db, lb$PACKAGES_BASE_R433)
  r <- attr(defauts, "releve")
  if (!length(ensembles$moteur)) cat("  moteur : aucune dépendance déclarée hors R\n")
  if (nrow(r))
    cat(sprintf("  %-20s %-18s %-12s %4d dépendance(s) récursive(s)\n",
                r$ensemble, r$package, r$version, r$dependances), sep = "")
  if (length(defauts)) erreur(defauts)
  cat(sprintf("  OK : aucun de %s dans les dépendances récursives (%s).\n",
              paste(PACKAGES_JAVA, collapse = ", "), paste(CHAMPS_DEPENDANCES, collapse = ", ")))
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
