# -----------------------------------------------------------------------------
#  tests/unitaires/test_controles.R : scripts de tests/controles/ et de la
#  concordance, sur le dépôt et sur des témoins (issue #8, critères
#  d'acceptation). Chaque script est exécuté comme en CI, dans un
#  sous-processus Rscript, et jugé sur son code de sortie.
#  Nature des attendus : valeurs exactes (codes de sortie, écarts relevés).
# -----------------------------------------------------------------------------

CONTROLES <- file.path(RACINE, "tests", "controles")
controle <- function(script, ...) executer_rscript(file.path(CONTROLES, script), c(...))
echoue <- function(r, motif = NULL) {
  if (r$statut == 0L) return("code de sortie 0")
  if (!is.null(motif) && !any(grepl(motif, r$sortie, fixed = TRUE)))
    return(paste("écart attendu non signalé :", motif, "| sortie :", paste(r$sortie, collapse = " | ")))
  TRUE
}
passe <- function(r) if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | ")

# --- liste blanche de DESCRIPTION (ADR 0002, point 3, A8, M44) ---------------

verifier("liste blanche : passe sur DESCRIPTION du dépôt",
         passe(controle("liste_blanche_description.R")))
description_temoin <- function(remplacements) {
  l <- readLines(file.path(RACINE, "DESCRIPTION"))
  l <- l[!grepl("^Depends:", l)]
  f <- tempfile("DESCRIPTION_")
  writeLines(c(l, remplacements), f)
  f
}
for (cas in list(
  list(lignes = c("Depends: R (>= 4.3.3)", "Imports: survival"), motif = "Imports : survival"),
  list(lignes = c("Depends: R (>= 4.3.3)", "LinkingTo: Rcpp"), motif = "LinkingTo : Rcpp"),
  list(lignes = "Depends: R (>= 4.3.1)", motif = "R (>= 4.3.1)"),
  list(lignes = c("Depends: R (>= 4.3.3)", "Suggests: openxlsx"), motif = "Suggests : openxlsx"),
  list(lignes = c("Depends: R (>= 4.3.3), Matrix"), motif = "Depends : Matrix"),
  list(lignes = "Imports: stats", motif = "0 entrée(s) R")
)) {
  f <- description_temoin(cas$lignes)
  verifier(sprintf("liste blanche : échoue sur le témoin « %s »", paste(cas$lignes, collapse = " ; ")),
           echoue(controle("liste_blanche_description.R", shQuote(f)), cas$motif))
  unlink(f)
}
f <- description_temoin(c("Depends: R (>= 4.3.3)", "Imports: stats, tools (>= 4.3.0)"))
verifier("liste blanche : passe avec des Imports de priorité base",
         passe(controle("liste_blanche_description.R", shQuote(f))))
unlink(f)

# --- ordre des modules (ADR 0007, points 2.1 à 2.3) --------------------------

verifier("ordre des modules : passe sur R/ du dépôt", passe(controle("graphe_modules.R")))
modules_temoins <- function(fichiers) {
  d <- tempfile("R_temoin_")
  dir.create(d)
  for (n in names(fichiers)) writeLines(fichiers[[n]], file.path(d, paste0(n, ".R")))
  d
}
for (cas in list(
  list(nom = "exposition appelle brut",
       fichiers = list(brut = "estimer_brut <- function(x) x",
                       exposition = "calculer_exposition <- function(x) estimer_brut(x)"),
       motif = "appel interdit exposition -> brut"),
  list(nom = "donnees appelle simulation",
       fichiers = list(simulation = "simuler_portefeuille <- function(n) n",
                       donnees = "lire_donnees <- function(d) simuler_portefeuille(d)"),
       motif = "appel interdit donnees -> simulation"),
  list(nom = "lissage appelle positionnement",
       fichiers = list(positionnement = "positionner <- function(x) x",
                       lissage = "lisser <- function(x) positionner(x)"),
       motif = "appel interdit lissage -> positionnement"),
  list(nom = "construire_table appelle simulation",
       fichiers = list(simulation = "simuler_portefeuille <- function(n) n",
                       construire_table = "construire_table <- function(d) simuler_portefeuille(d)"),
       motif = "appel interdit construire_table -> simulation"),
  list(nom = "do.call par chaîne",
       fichiers = list(outils = c("f <- function(x) do.call(\"g\", list(x))", "g <- function(x) x")),
       motif = "do.call(what = \"g\", ...)"),
  list(nom = "eval(parse())",
       fichiers = list(outils = "f <- function() eval(parse(text = \"1\"))"),
       motif = "eval(parse(...))"),
  list(nom = "::: dans R/",
       fichiers = list(outils = "f <- function() tablexp:::g()"),
       motif = "tablexp:::g"),
  list(nom = "get()",
       fichiers = list(outils = "f <- function() get(\"g\")"),
       motif = "get()"),
  list(nom = "base::get() qualifié",
       fichiers = list(outils = "f <- function() base::get(\"g\")"), motif = "get()"),
  list(nom = "utils::getFromNamespace() qualifié",
       fichiers = list(outils = "f <- function() utils::getFromNamespace(\"g\", \"tablexp\")"),
       motif = "getFromNamespace()"),
  list(nom = "base::do.call() par chaîne",
       fichiers = list(outils = "f <- function() base::do.call(\"g\", list())"),
       motif = "do.call(what = \"g\", ...)"),
  list(nom = "base::eval(parse())",
       fichiers = list(outils = "f <- function() base::eval(parse(text = \"1\"))"),
       motif = "eval(parse(...))"),
  list(nom = "do.call(args = , what = chaîne)",
       fichiers = list(outils = "f <- function() do.call(args = list(1), what = \"g\")"),
       motif = "do.call(what = \"g\", ...)"),
  list(nom = "do.call(paste0(...))",
       fichiers = list(outils = "f <- function() do.call(paste0(\"g\"), list())"),
       motif = "do.call(what = paste0(\"g\"), ...)"),
  list(nom = "eval(call())",
       fichiers = list(outils = "f <- function() eval(call(\"g\"))"), motif = "eval(call(...))"),
  list(nom = "eval(as.call())",
       fichiers = list(outils = "f <- function() eval(as.call(list(as.name(\"g\"))))"),
       motif = "eval(as.call(...))"),
  list(nom = "fichier hors des modules",
       fichiers = list(affichage = "f <- function() 1"),
       motif = "affichage.R : fichier hors des modules"),
  list(nom = "symbole défini dans deux modules",
       fichiers = list(outils = "f <- function() 1", donnees = "f <- function() 2"),
       motif = "symbole f défini plusieurs fois")
)) {
  d <- modules_temoins(cas$fichiers)
  verifier(sprintf("ordre des modules : échoue sur le témoin « %s »", cas$nom),
           echoue(controle("graphe_modules.R", shQuote(d)), cas$motif))
  unlink(d, recursive = TRUE)
}
d <- modules_temoins(list(
  outils = "f <- function(x) x",
  donnees = "lire <- function(x) f(x)",
  simulation = "simuler <- function(x) lire(f(x))",
  positionnement = "positionner <- function(x) lisser(x)",
  lissage = c("lisser <- function(x) do.call(f, list(x))",
              "lisser2 <- function(x) do.call(function(y) y, list(x))",
              "lisser3 <- function(e) eval(e)")))
verifier("ordre des modules : passe sur un graphe conforme (simulation -> donnees, positionnement -> lissage, do.call(f), do.call(function), eval(e))",
         passe(controle("graphe_modules.R", shQuote(d))))
unlink(d, recursive = TRUE)

# --- identité des batteries et de l'image (CLAUDE.md, BATTERIES, ci.yml) -----

verifier("identité des batteries : passe sur le dépôt", passe(controle("identite_batteries.R")))
copie_racine <- function() {
  d <- tempfile("racine_temoin_")
  dir.create(file.path(d, ".claude", "workflows"), recursive = TRUE)
  dir.create(file.path(d, ".github", "workflows"), recursive = TRUE)
  file.copy(file.path(RACINE, "CLAUDE.md"), d)
  file.copy(file.path(RACINE, ".claude", "workflows", "circuit-technique.js"),
            file.path(d, ".claude", "workflows"))
  file.copy(file.path(RACINE, ".github", "workflows", "ci.yml"), file.path(d, ".github", "workflows"))
  d
}
modifier <- function(chemin, motif, remplacement) {
  l <- readLines(chemin)
  n <- sum(grepl(motif, l, fixed = TRUE))
  writeLines(sub(motif, remplacement, l, fixed = TRUE), chemin)
  n
}
for (cas in list(
  list(nom = "batterie retirée de BATTERIES", fichier = c(".claude", "workflows", "circuit-technique.js"),
       motif = "  'Rscript tests/test_reproductibilite.R',", remplacement = "",
       ecart = "batteries différentes"),
  list(nom = "batterie modifiée dans CLAUDE.md", fichier = "CLAUDE.md",
       motif = "Rscript tests/concordance_doc_moteur.R --strict",
       remplacement = "Rscript tests/concordance_doc_moteur.R", ecart = "batteries différentes"),
  list(nom = "étape de batterie modifiée dans ci.yml", fichier = c(".github", "workflows", "ci.yml"),
       motif = "run: Rscript tests/test_unitaires.R", remplacement = "run: Rscript tests/test_unitaires.R --x",
       ecart = "batteries différentes"),
  list(nom = "marque « À ADAPTER » dans CLAUDE.md", fichier = "CLAUDE.md",
       motif = NA, remplacement = NA, ecart = "marque"),
  list(nom = "continue-on-error sur une étape de batterie", fichier = c(".github", "workflows", "ci.yml"),
       motif = "        run: Rscript tests/test_unitaires.R",
       remplacement = "        continue-on-error: true\n        run: Rscript tests/test_unitaires.R",
       ecart = "continue-on-error interdit"),
  list(nom = "job de tests sans container:", fichier = c(".github", "workflows", "ci.yml"),
       motif = "    container:", remplacement = "    conteneur_retire:",
       ecart = "sans `container:`"),
  list(nom = "job de tests renommé (batteries hors du job tests:)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "  tests:", remplacement = "  autres:",
       ecart = "job `tests:` absent"),
  list(nom = "TABLEXP_IMAGE différente de container.image", fichier = c(".github", "workflows", "ci.yml"),
       motif = "TABLEXP_IMAGE: rocker/r-ver:4.3.3", remplacement = "TABLEXP_IMAGE: rocker/r-ver:4.3.2",
       ecart = "chaînes d'image différentes")
)) {
  d <- copie_racine()
  chemin <- do.call(file.path, as.list(c(d, cas$fichier)))
  n <- if (cas$ecart == "marque") {
    # Octets UTF-8 écrits tels quels (useBytes), quelle que soit la locale.
    con <- file(chemin, "a"); writeLines("**\u00c0 ADAPTER** : marque témoin", con, useBytes = TRUE)
    close(con); 1L
  } else modifier(chemin, cas$motif, cas$remplacement)
  verifier(sprintf("identité des batteries : témoin « %s » préparé (%d remplacement)", cas$nom, n), n == 1L)
  verifier(sprintf("identité des batteries : échoue sur le témoin « %s »", cas$nom),
           echoue(controle("identite_batteries.R", shQuote(d)), cas$ecart))
  unlink(d, recursive = TRUE)
}

# --- plateforme (ADR 0008, points 3 à 5) --------------------------------------

env_plateforme <- new.env()
sys.source(file.path(CONTROLES, "plateforme.R"), envir = env_plateforme)
conforme <- list(
  r = "R version 4.3.3 (2024-02-29)",
  bibliotheques = c("extSoftVersion()[\"BLAS\"]" = "/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0",
                    "sessionInfo()$BLAS" = "/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0",
                    "sessionInfo()$LAPACK" = "/usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0",
                    "La_library()" = "/usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0"),
  la_version = "3.10.0",
  fils = c(OPENBLAS_NUM_THREADS = "1", OMP_NUM_THREADS = "1"),
  image = paste0("rocker/r-ver:4.3.3@sha256:", strrep("0123456789abcdef", 4L)))
verifier("plateforme : relevé simulé conforme -> aucun écart",
         identical(env_plateforme$defauts_plateforme(conforme), character(0)))
perturber <- function(champ, valeur, nom = NULL) {
  x <- conforme
  if (is.null(nom)) x[[champ]] <- valeur else x[[champ]][[nom]] <- valeur
  x
}
for (cas in list(
  list(nom = "R 4.3.1", x = perturber("r", "R version 4.3.1 (2023-06-16)"), motif = "R :"),
  list(nom = "OpenBLAS", x = perturber("bibliotheques", "/usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3",
                                       "extSoftVersion()[\"BLAS\"]"), motif = "optimisée"),
  list(nom = "BLAS interne de R", x = perturber("bibliotheques", "/usr/local/lib/R/lib/libRblas.so",
                                                "sessionInfo()$BLAS"), motif = "inattendue"),
  list(nom = "LAPACK vide", x = perturber("bibliotheques", "", "La_library()"), motif = "chemin vide"),
  list(nom = "LAPACK 3.12.0", x = perturber("la_version", "3.12.0"), motif = "La_version()"),
  list(nom = "OPENBLAS_NUM_THREADS = 2", x = perturber("fils", "2", "OPENBLAS_NUM_THREADS"),
       motif = "OPENBLAS_NUM_THREADS"),
  list(nom = "OMP_NUM_THREADS non définie", x = perturber("fils", "(non définie)", "OMP_NUM_THREADS"),
       motif = "OMP_NUM_THREADS"),
  list(nom = "image sans empreinte", x = perturber("image", "rocker/r-ver:4.3.3"), motif = "TABLEXP_IMAGE")
)) {
  d <- env_plateforme$defauts_plateforme(cas$x)
  verifier(sprintf("plateforme : écart relevé sur le témoin « %s »", cas$nom),
           if (length(d) == 1L && grepl(cas$motif, d, fixed = TRUE)) TRUE
           else paste("écarts :", paste(d, collapse = " ; ")))
}
# GITHUB_STEP_SUMMARY vidée : le résumé du job CI ne reçoit que le relevé de
# l'étape « Plateforme (mode strict) », pas ceux des témoins.
r <- executer_rscript(file.path(CONTROLES, "plateforme.R"), env = "GITHUB_STEP_SUMMARY=")
verifier("plateforme : mode relevé -> code 0 et sessionInfo() au journal",
         r$statut == 0L && any(r$sortie == "sessionInfo()") &&
           any(grepl(R.version.string, r$sortie, fixed = TRUE)))
r <- executer_rscript(file.path(CONTROLES, "plateforme.R"), "--exiger",
                      env = c("OPENBLAS_NUM_THREADS=2", "GITHUB_STEP_SUMMARY="))
verifier("plateforme : mode strict hors plateforme de référence -> code non nul et ::error",
         r$statut != 0L && any(startsWith(r$sortie, "::error")))

# --- concordance documentation <-> moteur (E3.3) -----------------------------

CONCORDANCE <- file.path(RACINE, "tests", "concordance_doc_moteur.R")
concordance <- function(...) executer_rscript(CONCORDANCE, c(...))
latex_temoin <- function(lignes) {
  d <- tempfile("latex_temoin_"); dir.create(d)
  writeLines(lignes, file.path(d, "temoin.tex"))
  d
}
attente_temoin <- function(lignes) {
  f <- tempfile("attente_", fileext = ".csv")
  writeLines(c("nom,issue,motif", lignes), f)
  f
}
vide <- latex_temoin("% aucun \\code{construire\\_table()} cité : ligne commentée")
cite <- latex_temoin("Point d'entrée : \\code{construire\\_table()}, \\code{tools::md5sum()}, \\code{set.seed()}.")
inconnu <- latex_temoin("\\code{construire\\_table()} et \\code{fonction\\_inconnue()}.")
inconnu_args <- latex_temoin("\\code{construire\\_table(donnees, roles, plan)} et \\code{autre\\_inconnue(x, y = 2)}.")
cite_args <- latex_temoin("\\code{construire\\_table(donnees, roles, plan)}.")
attente_ok <- attente_temoin("construire_table,#11,en attente")
attente_vide <- attente_temoin(character(0))
attente_perimee <- attente_temoin(c("construire_table,#11,en attente", "nom_disparu,#9,retiré"))
attente_sans_issue <- attente_temoin("construire_table,11,en attente")

verifier("concordance : passe sur le dépôt (--strict)", passe(concordance("--strict")))
verifier("concordance : export non cité, en attente -> code 0",
         passe(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_ok))))
verifier("concordance : export non cité, hors attente -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_vide)),
                "absent de la liste d'attente"))
verifier("concordance : export cité et encore en attente -> succès inattendu, échec",
         echoue(concordance("--strict", "--latex", shQuote(cite), "--attente", shQuote(attente_ok)),
                "succès inattendu"))
verifier("concordance : export cité, attente vide, fonctions de base citées -> code 0",
         passe(concordance("--strict", "--latex", shQuote(cite), "--attente", shQuote(attente_vide))))
verifier("concordance : fonction citée introuvable -> échec",
         echoue(concordance("--strict", "--latex", shQuote(inconnu), "--attente", shQuote(attente_vide)),
                "fonction_inconnue() existe"))
verifier("concordance : ligne d'attente qui n'est pas un export -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_perimee)),
                "nom_disparu"))
verifier("concordance : ligne d'attente sans renvoi « #N » -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_sans_issue))))
verifier("concordance : fonction citée avec arguments introuvable -> échec",
         echoue(concordance("--strict", "--latex", shQuote(inconnu_args), "--attente", shQuote(attente_vide)),
                "autre_inconnue() existe"))
verifier("concordance : export cité avec arguments -> lu comme cité (succès inattendu en attente)",
         echoue(concordance("--strict", "--latex", shQuote(cite_args), "--attente", shQuote(attente_ok)),
                "succès inattendu"))
verifier("concordance : --latex sans valeur -> échec",
         echoue(concordance("--strict", "--latex")))
verifier("concordance : --latex vers un dossier inexistant -> échec",
         echoue(concordance("--strict", "--latex", shQuote(tempfile("absent_"))), "dossier introuvable"))
verifier("concordance : sans --strict, écart -> code 0 (mode rapport)",
         passe(concordance("--latex", shQuote(inconnu), "--attente", shQuote(attente_vide))))
unlink(c(vide, cite, inconnu, inconnu_args, cite_args), recursive = TRUE)
unlink(c(attente_ok, attente_vide, attente_perimee, attente_sans_issue))
