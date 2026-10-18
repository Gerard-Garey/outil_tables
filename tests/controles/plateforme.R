# -----------------------------------------------------------------------------
#  tests/controles/plateforme.R : contrôle et relevé de la plateforme de
#  calcul (ADR 0008, points 3 à 5 ; issue #8)
#
#  Deux modes :
#    --exiger  mode strict (CI, workflow de régénération) : échoue (code 1,
#              ligne ::error) si une condition ci-dessous n'est pas remplie ;
#    (défaut)  mode relevé (poste, session) : affiche les mêmes grandeurs et
#              les écarts, sans échouer.
#
#  Conditions (mode strict) :
#    - R.version.string vaut exactement R_ATTENDU ;
#    - extSoftVersion()["BLAS"], sessionInfo()$BLAS, sessionInfo()$LAPACK et
#      La_library() sont non vides, ne désignent aucune BLAS optimisée
#      (MOTIF_OPTIMISE) et désignent les chemins de référence (MOTIF_BLAS,
#      MOTIF_LAPACK) ;
#    - La_version() vaut LA_VERSION_ATTENDUE ;
#    - OPENBLAS_NUM_THREADS et OMP_NUM_THREADS valent 1 (point 4) ;
#    - TABLEXP_IMAGE (posée par le job avec la même chaîne que
#      container.image) est épinglée par empreinte : MOTIF_IMAGE (point 1).
#
#  Valeurs attendues : HYPOTHÈSE de départ de l'ADR 0008 (point 3), non
#  vérifiée : rocker/r-ver:4.3.3 sur Ubuntu 22.04, R compilé avec
#  --with-blas --with-lapack, BLAS et LAPACK Netlib du système, LAPACK 3.10.0.
#  La première exécution de la CI tranche ; un écart est un point de décision
#  (ADR 0008, point 3), jamais un ajustement silencieux.
#
#  Identifiant de plateforme (point 5) écrit dans le journal et, s'il existe,
#  dans $GITHUB_STEP_SUMMARY ; consignés sans faire échouer : processeur et
#  drapeaux (fma, avx2, avx512f), ImageOS et ImageVersion de l'hôte,
#  LC_COLLATE, fuseau horaire, présence d'un exécutable java, GITHUB_SHA.
#
#  Usage : Rscript tests/controles/plateforme.R [--exiger]
#  Sourcé (tests), le fichier ne fait que définir les fonctions.
# -----------------------------------------------------------------------------

R_ATTENDU <- "R version 4.3.3 (2024-02-29)"
LA_VERSION_ATTENDUE <- "3.10.0"
MOTIF_BLAS <- "^/usr/lib/x86_64-linux-gnu/blas/libblas\\.so\\.3"
MOTIF_LAPACK <- "^/usr/lib/x86_64-linux-gnu/lapack/liblapack\\.so\\.3"
MOTIF_OPTIMISE <- "openblas|mkl|atlas|blis|flexiblas"
MOTIF_IMAGE <- "^rocker/r-ver:4\\.3\\.3@sha256:[0-9a-f]{64}$"
VARIABLES_FILS <- c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS")

# Relevé des grandeurs contrôlées, sous forme de liste nommée.
relever_plateforme <- function() {
  si <- utils::sessionInfo()
  vide_si_null <- function(x) if (is.null(x) || !length(x) || is.na(x[1L])) "" else as.character(x[1L])
  list(
    r = R.version.string,
    bibliotheques = c(
      "extSoftVersion()[\"BLAS\"]" = vide_si_null(unname(extSoftVersion()["BLAS"])),
      "sessionInfo()$BLAS" = vide_si_null(si$BLAS),
      "sessionInfo()$LAPACK" = vide_si_null(si$LAPACK),
      "La_library()" = vide_si_null(La_library())
    ),
    la_version = La_version(),
    fils = Sys.getenv(VARIABLES_FILS, unset = "(non définie)", names = TRUE),
    image = Sys.getenv("TABLEXP_IMAGE", unset = "(non définie)")
  )
}

# Rend les écarts (vecteur vide si conforme) du relevé r.
defauts_plateforme <- function(r) {
  defauts <- character(0)
  if (!identical(r$r, R_ATTENDU))
    defauts <- c(defauts, sprintf("R : %s au lieu de %s", r$r, R_ATTENDU))
  motifs <- c(MOTIF_BLAS, MOTIF_BLAS, MOTIF_LAPACK, MOTIF_LAPACK)
  for (i in seq_along(r$bibliotheques)) {
    v <- r$bibliotheques[[i]]; n <- names(r$bibliotheques)[i]
    if (!nzchar(v))
      defauts <- c(defauts, sprintf("%s : chemin vide", n))
    else if (grepl(MOTIF_OPTIMISE, v, ignore.case = TRUE))
      defauts <- c(defauts, sprintf("%s : BLAS ou LAPACK optimisée (%s)", n, v))
    else if (!grepl(motifs[i], v))
      defauts <- c(defauts, sprintf("%s : bibliothèque inattendue (%s)", n, v))
  }
  if (!identical(r$la_version, LA_VERSION_ATTENDUE))
    defauts <- c(defauts, sprintf("La_version() : %s au lieu de %s", r$la_version, LA_VERSION_ATTENDUE))
  for (n in VARIABLES_FILS)
    if (!identical(unname(r$fils[n]), "1"))
      defauts <- c(defauts, sprintf("%s : %s au lieu de 1", n, unname(r$fils[n])))
  if (!grepl(MOTIF_IMAGE, r$image))
    defauts <- c(defauts, sprintf("TABLEXP_IMAGE : %s n'est pas rocker/r-ver:4.3.3 épinglée par empreinte (@sha256:...)",
                                  r$image))
  defauts
}

# Grandeurs consignées sans faire échouer (ADR 0008, point 5).
consignes_plateforme <- function() {
  cpu <- if (file.exists("/proc/cpuinfo")) readLines("/proc/cpuinfo", warn = FALSE) else character(0)
  modele <- sub("^[^:]*:[[:space:]]*", "", grep("^model name", cpu, value = TRUE)[1L])
  drapeaux <- strsplit(sub("^[^:]*:[[:space:]]*", "", grep("^flags", cpu, value = TRUE)[1L]), " ")[[1L]]
  os <- if (file.exists("/etc/os-release")) {
    l <- grep("^PRETTY_NAME=", readLines("/etc/os-release", warn = FALSE), value = TRUE)
    if (length(l)) gsub("\"", "", sub("^PRETTY_NAME=", "", l[1L])) else ""
  } else paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
  java <- Sys.which("java")
  # Noms en chaînes, non en étiquettes de c() : une étiquette non ASCII est un
  # symbole, traduit en natif à l'analyse (« Syst<U+00E8>me » en locale C
  # sous parse(encoding = "UTF-8"), mesuré).
  stats::setNames(
    c(os,
      if (length(cpu)) modele else "(non relevé : /proc/cpuinfo absent)",
      if (length(cpu))
        paste(c("fma", "avx2", "avx512f"), ifelse(c("fma", "avx2", "avx512f") %in% drapeaux, "oui", "non"),
              sep = " : ", collapse = " ; ") else "(non relevé)",
      paste(Sys.getenv("ImageOS", "(non définie)"), Sys.getenv("ImageVersion", "(non définie)"), sep = " / "),
      Sys.getlocale("LC_COLLATE"),
      as.character(Sys.timezone()),
      if (nzchar(java)) unname(java) else "absent",
      Sys.getenv("GITHUB_SHA", "(non défini)")),
    c("Système", "Processeur", "Drapeaux fma / avx2 / avx512f", "ImageOS / ImageVersion (hôte)",
      "LC_COLLATE", "Fuseau horaire", "java", "GITHUB_SHA"))
}

# Lignes du bloc d'identifiant (grandeurs contrôlées du relevé r) et des
# grandeurs consignées ; reprises dans l'en-tête des références de
# non-régression (ADR 0009, point 2.2 ; tests/reference/outils_reference.R).
lignes_identifiant <- function(r) {
  c(sprintf("  Image                     : %s", r$image),
    sprintf("  R                         : %s", r$r),
    sprintf("  %-26s: %s", names(r$bibliotheques), r$bibliotheques),
    sprintf("  La_version()              : %s", r$la_version),
    sprintf("  %-26s: %s", names(r$fils), r$fils))
}
lignes_consignees <- function() {
  sprintf("  %-30s: %s", names(consignes_plateforme()), consignes_plateforme())
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  exiger <- "--exiger" %in% args
  r <- relever_plateforme()
  lignes <- c(
    "Identifiant de plateforme (ADR 0008, point 5)",
    lignes_identifiant(r),
    "Consigné sans contrôle",
    lignes_consignees()
  )
  cat(lignes, sep = "\n")
  cat("\nsessionInfo()\n")
  print(utils::sessionInfo())
  resume <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(resume))
    cat(c("### Identifiant de plateforme", "", "```", lignes, "", utils::capture.output(print(utils::sessionInfo())),
          "```", ""), file = resume, sep = "\n", append = TRUE)
  defauts <- defauts_plateforme(r)
  if (length(defauts)) {
    cat(sprintf("\nPlateforme NON CONFORME à la plateforme de référence (%s) :\n",
                if (exiger) "mode strict" else "mode relevé, sans échec"))
    cat(paste0("  - ", defauts, "\n"), sep = "")
    if (exiger) {
      cat("::error title=Plateforme non conforme::", paste(defauts, collapse = " ; "), "\n", sep = "")
      quit(save = "no", status = 1L)
    }
  } else {
    cat("\nPlateforme conforme à la plateforme de référence.\n")
  }
  invisible(defauts)
}

if (sys.nframe() == 0L) principal()
