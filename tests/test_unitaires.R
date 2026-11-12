# -----------------------------------------------------------------------------
#  tests/test_unitaires.R : batterie des tests unitaires (ADR 0007, point 11)
#
#  Installe l'arbre de travail dans une bibliothèque temporaire, puis exécute
#  chaque tests/unitaires/test_*.R dans son propre environnement. Les
#  fonctions non exportées s'appellent par tablexp::: (seul endroit admis,
#  ADR 0007, point 11.3).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/test_unitaires.R [--dossier <dossier de test_*.R>]
#  Détail de toutes les assertions :
#      Rscript -e "options(harnais.verbeux = TRUE); source('tests/test_unitaires.R')"
#  Code de sortie 1 si au moins un échec ou un succès inattendu.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(normalizePath(f[1L])), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
installer_tablexp(RACINE)

# --dossier <d> : autre dossier de fichiers test_*.R (témoins des tests du
# lanceur lui-même, tests/unitaires/test_harnais.R).
ARGS <- commandArgs(trailingOnly = TRUE)
DOSSIER <- if ("--dossier" %in% ARGS) ARGS[match("--dossier", ARGS) + 1L] else
  file.path(RACINE, "tests", "unitaires")
if (is.na(DOSSIER) || !dir.exists(DOSSIER)) {
  verifier("dossier des tests unitaires", sprintf("dossier introuvable : %s", DOSSIER))
  bilan()
}

# Boucle dans local() : f, env, avant, r, e et t0 ne restent pas dans
# globalenv(), parent des environnements de test, où un test qui lirait l'une
# d'elles comme variable libre la trouverait silencieusement (mesuré, #17).
# Les fonctions de lecture (analyser_fichier() et ses auxiliaires) sont dans le
# même local() pour la même raison.
local({
  # Lecture d'un fichier de test en UTF-8 quelle que soit la locale native (#213,
  # suite de #206) : analyser_fichier() et ses auxiliaires sont dans
  # tests/controles/lecture_utf8.R (#228), chargé ici, dans ce local().
  # Les noms non ASCII des fichiers de test (étiquettes de cas, champs d'un objet
  # éprouvé) restent altérés hors UTF-8 : un cas écrit avec un tel nom ne porte
  # alors pas sur le caractère non ASCII (limite de parse(), non levée ici).
  sys.source(file.path(RACINE, "tests", "controles", "lecture_utf8.R"), envir = environment())

  t0 <- Sys.time()
  for (f in sort(list.files(DOSSIER,
                            pattern = "^test_.*\\.R$", full.names = TRUE))) {
    fichier_courant(basename(f))
    avant <- nrow(.harnais$lignes)
    env <- new.env(parent = globalenv())
    # quit() et q() neutralisés dans le fichier de test (base::quit() est
    # rattrapé par la garde de sortie du harnais) ; bilan() aussi : un bilan
    # intermédiaire marquerait la batterie comme close avant la fin.
    env$quit <- env$q <- function(...) stop("quit() interdit dans un fichier de test")
    env$bilan <- function(...) stop("bilan() interdit dans un fichier de test")
    # Lecture en UTF-8 en toute locale (analyser_fichier(), #213) : littéraux
    # non ASCII des tests marqués UTF-8, comme ceux du moteur installé sous une
    # locale UTF-8 (M54) ; source(encoding = "UTF-8") n'exécute rien en locale
    # C (mesuré). Un fichier qui n'est pas de l'UTF-8 valide est refusé.
    r <- tryCatch({ for (e in analyser_fichier(f, getOption("keep.source"))) eval(e, env); NULL }, error = function(e) e)
    if (inherits(r, "error"))
      verifier("chargement complet du fichier", paste("erreur :", conditionMessage(r)))
    else if (nrow(.harnais$lignes) == avant)
      verifier("au moins une assertion", "fichier de test sans aucune assertion")
  }
  cat(sprintf("\nDurée : %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
})
bilan()
