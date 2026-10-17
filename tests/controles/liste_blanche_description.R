# -----------------------------------------------------------------------------
#  tests/controles/liste_blanche_description.R : liste blanche des
#  dépendances déclarées dans DESCRIPTION (ADR 0002, point 3, A8, M44 ;
#  ADR 0007, point 1.1 ; issue #8)
#
#  Règles :
#    - Depends : exactement une entrée `R (>= 4.3.3)` (M44) ; toute autre
#      entrée est un package de priorité base de R 4.3.3 ou un package admis ;
#    - Imports : packages de priorité base de R 4.3.3, ou admis ;
#    - LinkingTo, Suggests, Enhances : packages admis seulement (ADR 0007,
#      point 1.1 : ni Suggests ni LinkingTo sans ADR de méthode ; A8 : le
#      contrôle couvre LinkingTo).
#  Un package est « admis » s'il figure dans ADMIS avec le champ et l'ADR de
#  méthode qui l'introduit (ADR 0002, point 5 et A8 : l'ADR qui introduit
#  Rcpp l'inscrit ici). ADMIS est vide.
#
#  La liste des packages de priorité base est écrite en dur (R 4.3.3), pour
#  que le verdict ne dépende pas de la machine ; le script signale, sans
#  échouer, un écart avec installed.packages(priority = "base").
#
#  Usage (depuis n'importe où) :
#      Rscript tests/controles/liste_blanche_description.R [DESCRIPTION]
#  Sans argument : DESCRIPTION à la racine du dépôt. Code de sortie 1 si une
#  règle est violée. Sourcé (tests), le fichier ne fait que définir les
#  fonctions.
# -----------------------------------------------------------------------------

PACKAGES_BASE_R433 <- c("base", "compiler", "datasets", "graphics", "grDevices",
                        "grid", "methods", "parallel", "splines", "stats",
                        "stats4", "tcltk", "tools", "utils")

DEPENDS_R_ATTENDU <- c(operateur = ">=", version = "4.3.3")

# Packages admis hors priorité base : package, champ DESCRIPTION, ADR.
ADMIS <- data.frame(package = character(0), champ = character(0),
                    adr = character(0), stringsAsFactors = FALSE)

CHAMPS_CONTROLES <- c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")

# "a (>= 1.0), b" -> data.frame(package, operateur, version) ; NA si sans
# contrainte ; une entrée illisible donne package NA.
lire_dependances <- function(texte) {
  vide <- data.frame(package = character(0), operateur = character(0),
                     version = character(0), brut = character(0),
                     stringsAsFactors = FALSE)
  if (is.null(texte) || is.na(texte) || !nzchar(trimws(texte))) return(vide)
  entrees <- trimws(strsplit(gsub("[[:space:]]+", " ", texte), ",", fixed = TRUE)[[1L]])
  entrees <- entrees[nzchar(entrees)]
  motif <- "^([A-Za-z][A-Za-z0-9.]*)[[:space:]]*(\\([[:space:]]*(>=|<=|==|>|<)[[:space:]]*([^)[:space:]]+)[[:space:]]*\\))?$"
  ok <- grepl(motif, entrees)
  data.frame(
    package = ifelse(ok, sub(motif, "\\1", entrees), NA_character_),
    operateur = ifelse(ok & grepl("\\(", entrees), sub(motif, "\\3", entrees), NA_character_),
    version = ifelse(ok & grepl("\\(", entrees), sub(motif, "\\4", entrees), NA_character_),
    brut = entrees, stringsAsFactors = FALSE)
}

# Rend les défauts (vecteur vide si conforme) du fichier DESCRIPTION `chemin`.
defauts_description <- function(chemin) {
  if (!file.exists(chemin)) return(sprintf("fichier absent : %s", chemin))
  dcf <- tryCatch(read.dcf(chemin), error = function(e) e)
  if (inherits(dcf, "error")) return(paste("DESCRIPTION illisible :", conditionMessage(dcf)))
  champ <- function(n) if (n %in% colnames(dcf)) dcf[1L, n] else NA_character_
  defauts <- character(0)
  for (n in CHAMPS_CONTROLES) {
    d <- lire_dependances(champ(n))
    for (i in seq_len(nrow(d))) {
      p <- d$package[i]
      if (is.na(p)) {
        defauts <- c(defauts, sprintf("%s : entrée illisible « %s »", n, d$brut[i]))
        next
      }
      admis <- any(ADMIS$package == p & ADMIS$champ == n)
      if (n == "Depends" && p == "R") next
      if (p == "R") {
        defauts <- c(defauts, sprintf("%s : R ne se déclare que dans Depends", n))
      } else if (n %in% c("Depends", "Imports")) {
        if (!(p %in% PACKAGES_BASE_R433) && !admis)
          defauts <- c(defauts, sprintf(
            "%s : %s n'est pas de priorité base et n'est admis par aucun ADR (ADR 0002, point 3)", n, p))
      } else if (!admis) {
        defauts <- c(defauts, sprintf(
          "%s : %s n'est admis par aucun ADR de méthode (ADR 0002, A8 ; ADR 0007, point 1.1)", n, p))
      }
    }
  }
  dep_r <- lire_dependances(champ("Depends"))
  dep_r <- dep_r[!is.na(dep_r$package) & dep_r$package == "R", , drop = FALSE]
  attendu <- sprintf("R (%s %s)", DEPENDS_R_ATTENDU[["operateur"]], DEPENDS_R_ATTENDU[["version"]])
  if (nrow(dep_r) != 1L) {
    defauts <- c(defauts, sprintf("Depends : %d entrée(s) R au lieu d'une seule « %s » (M44)",
                                  nrow(dep_r), attendu))
  } else if (!identical(dep_r$operateur, DEPENDS_R_ATTENDU[["operateur"]]) ||
             !identical(dep_r$version, DEPENDS_R_ATTENDU[["version"]])) {
    defauts <- c(defauts, sprintf("Depends : « %s » au lieu de « %s » (M44)", dep_r$brut, attendu))
  }
  defauts
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  chemin <- if (length(args)) args[1L] else
    file.path(dirname(dirname(dirname(normalizePath(script)))), "DESCRIPTION")
  cat("Liste blanche de DESCRIPTION :", chemin, "\n")
  installes <- rownames(utils::installed.packages(lib.loc = .Library, priority = "base"))
  if (!setequal(installes, PACKAGES_BASE_R433))
    cat("Note : priorité base installée différente de la liste R 4.3.3 écrite en dur :",
        paste(sort(union(setdiff(installes, PACKAGES_BASE_R433),
                         setdiff(PACKAGES_BASE_R433, installes))), collapse = ", "), "\n")
  defauts <- defauts_description(chemin)
  if (length(defauts)) {
    cat(paste0("  ECHEC : ", defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Liste blanche de DESCRIPTION::", paste(defauts, collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("  OK : Depends, Imports, LinkingTo, Suggests et Enhances conformes.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
