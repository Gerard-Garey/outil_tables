# -----------------------------------------------------------------------------
#  tests/unitaires/test_harnais.R : le harnais lui-même (ADR 0007, point
#  11.2 ; issue #8, critère « harnais testé »)
#
#  Chaque témoin est un script exécuté dans un sous-processus Rscript qui
#  source tests/harnais.R : on lit son code de sortie et sa ligne de bilan.
#  Nature des attendus : valeurs exactes (codes de sortie, décomptes).
# -----------------------------------------------------------------------------

HARNAIS <- file.path(RACINE, "tests", "harnais.R")

# Exécute un témoin dont `corps` est le code R après le chargement du harnais.
temoin <- function(corps) {
  f <- tempfile("temoin_harnais_", fileext = ".R")
  on.exit(unlink(f))
  writeLines(c(sprintf("source(%s)", deparse(HARNAIS)), corps), f)
  executer_rscript(f)
}
ligne_bilan <- function(r) grep("^Bilan : ", r$sortie, value = TRUE)

r <- temoin(c('verifier("vrai", TRUE)', "bilan()"))
verifier("témoin : assertion vraie -> code 0", r$statut == 0L)
verifier("témoin : la ligne de bilan cite R.version.string et les décomptes",
         # Octets comparés : sortie du sous-processus non marquée, littéral
         # marqué UTF-8 (chargement des tests par parse(encoding = "UTF-8")).
         identical(lapply(ligne_bilan(r), charToRaw), list(charToRaw(sprintf(
           "Bilan : %s ; 2 tests ; 0 échec(s) ; 0 échec(s) attendu(s) ; 0 succès inattendu(s)",
           R.version.string)))))

r <- temoin(c('verifier("faux", FALSE)', "bilan()"))
verifier("témoin : assertion fausse -> code non nul", r$statut != 0L)
verifier("témoin : assertion fausse -> 1 échec au bilan",
         grepl("; 1 échec(s) ;", ligne_bilan(r), fixed = TRUE, useBytes = TRUE))

r <- temoin(c('verifier("erreur", stop("boum"))', "bilan()"))
verifier("témoin : erreur dans une assertion -> code non nul", r$statut != 0L)

r <- temoin(c('verifier("chaîne", "écart décrit")', "bilan()"))
verifier("témoin : assertion qui rend une chaîne -> échec", r$statut != 0L)

r <- temoin(c('echec_attendu("défaut connu", "#8", FALSE)', "bilan()"))
verifier("témoin : échec attendu qui échoue -> code 0, compté attendu",
         r$statut == 0L && grepl("; 1 échec(s) attendu(s) ;", ligne_bilan(r), fixed = TRUE, useBytes = TRUE))

r <- temoin(c('echec_attendu("défaut corrigé", "#8", TRUE)', "bilan()"))
verifier("témoin : échec attendu qui réussit (succès inattendu) -> code non nul",
         r$statut != 0L && grepl("; 1 succès inattendu(s)", ligne_bilan(r), fixed = TRUE, useBytes = TRUE))

r <- temoin(c('echec_attendu("sans issue", "issue 8", FALSE)', "bilan()"))
verifier("témoin : échec attendu sans renvoi « #N » -> code non nul", r$statut != 0L)

r <- temoin(c('verifier("faux", FALSE)', "bilan(code_sortie = FALSE)"))
verifier("témoin : bilan(code_sortie = FALSE) -> code 0 malgré l'échec (mode rapport)",
         r$statut == 0L && any(r$sortie == "ECHEC"))

# Espace de noms hors priorité base chargé en fin de batterie : un package
# témoin minimal, installé dans une bibliothèque temporaire (aucun package
# du CRAN n'est requis).
lib_temoin <- tempfile("lib_temoin_")
src_temoin <- file.path(tempfile("src_temoin_"), "temoinhorsbase")
dir.create(lib_temoin); dir.create(file.path(src_temoin, "R"), recursive = TRUE)
writeLines(c("Package: temoinhorsbase", "Version: 0.0.1", "Title: Temoin",
             "Description: Paquet temoin des tests du harnais.",
             "Authors@R: person('T', 'T', role = c('aut', 'cre'), email = 't@example.org')",
             "License: GPL-2"), file.path(src_temoin, "DESCRIPTION"))
writeLines("export(f)", file.path(src_temoin, "NAMESPACE"))
writeLines("f <- function() 1", file.path(src_temoin, "R", "f.R"))
inst <- suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                 c("CMD", "INSTALL", "--no-docs", "-l", shQuote(lib_temoin),
                                   shQuote(src_temoin)), stdout = TRUE, stderr = TRUE))
verifier("package témoin hors base installé",
         if (is.null(attr(inst, "status"))) TRUE else paste(inst, collapse = " | "))
r <- temoin(c(sprintf('loadNamespace("temoinhorsbase", lib.loc = %s)', deparse(lib_temoin)),
              'verifier("vrai", TRUE)', "bilan()"))
verifier("témoin : espace de noms hors base chargé en fin de batterie -> code non nul",
         r$statut != 0L && any(grepl("temoinhorsbase", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(c(lib_temoin, dirname(src_temoin)), recursive = TRUE)

verifier("espaces_hors_base() : base et tablexp exclus, le reste relevé",
         identical(espaces_hors_base(c("base", "stats", "tablexp", "survival", "Matrix")),
                   c("Matrix", "survival")))

# Outils du harnais.
verifier("proche() : égalité dans la tolérance", isTRUE(proche(1 + 1e-13, 1, rel = 1e-12, abs = 0)))
verifier("proche() : écart hors tolérance décrit",
         is.character(proche(1.1, 1, rel = 1e-12, abs = 0)))
verifier("proche() : tolérances obligatoires",
         inherits(tryCatch(proche(1, 1), error = function(e) e), "error"))
verifier("sans_execution() retire le seul champ execution",
         identical(sans_execution(list(a = 1, execution = Sys.time(), b = 2)), list(a = 1, b = 2)))

# --- garde de sortie et lanceur (audit de #8, m1 et m2) ----------------------

r <- temoin(c('verifier("vrai", TRUE)', "quit(save = 'no', status = 0L)"))
verifier("témoin : quit(status = 0) avant bilan() -> code non nul", r$statut != 0L)
r <- temoin('verifier("vrai", TRUE)')
verifier("témoin : fin de script sans bilan() -> code non nul", r$statut != 0L)

LANCEUR <- file.path(RACINE, "tests", "test_unitaires.R")
dossier_temoin <- function(fichiers) {
  d <- tempfile("unitaires_temoin_"); dir.create(d)
  for (n in names(fichiers)) writeLines(fichiers[[n]], file.path(d, n))
  d
}
for (cas in list(
  list(nom = "quit() dans un fichier de test", f = c('verifier("vrai", TRUE)', "quit(status = 0)"),
       motif = "quit() interdit"),
  list(nom = "base::quit() dans un fichier de test", f = c('verifier("vrai", TRUE)', "base::quit(status = 0)"),
       motif = "avant la ligne de bilan"),
  list(nom = "fichier de test sans assertion", f = "x <- 1", motif = "sans aucune assertion")
)) {
  d <- dossier_temoin(list(test_temoin.R = cas$f))
  r <- executer_rscript(LANCEUR, c("--dossier", shQuote(d)))
  verifier(sprintf("lanceur : %s -> code non nul", cas$nom),
           if (r$statut == 0L) "code de sortie 0"
           else if (!any(grepl(cas$motif, r$sortie, fixed = TRUE, useBytes = TRUE)))
             paste("motif non signalé :", paste(r$sortie, collapse = " | ")) else TRUE)
  unlink(d, recursive = TRUE)
}
d <- dossier_temoin(list(test_temoin.R = 'verifier("vrai", TRUE)'))
r <- executer_rscript(LANCEUR, c("--dossier", shQuote(d)))
verifier("lanceur : fichier témoin conforme -> code 0",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
unlink(d, recursive = TRUE)
# Le lanceur ne laisse aucune de ses variables dans globalenv(), parent des
# environnements de test : un test qui lirait `e`, `f`, `env`, `r`, `avant` ou
# `t0` comme variable libre lirait celle du lanceur (ou d'un fichier précédent).
# Le second fichier les cherche (le premier laisse `env` et `r` en cas de fuite).
d <- dossier_temoin(list(
  test_a.R = 'verifier("premier fichier", TRUE)',
  test_b.R = c('presentes <- intersect(c("e", "f", "env", "r", "avant", "t0"), ls(globalenv(), all.names = TRUE))',
               'verifier("globalenv sans variable du lanceur", if (length(presentes)) paste("présentes :", paste(presentes, collapse = ", ")) else TRUE)')))
r <- executer_rscript(LANCEUR, c("--dossier", shQuote(d)))
verifier("lanceur : aucune variable du lanceur dans globalenv() pour les fichiers de test",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
unlink(d, recursive = TRUE)

# --- natures d'un échec attendu (audit de #8, m3) ----------------------------

refus_temoin <- 'structure(class = c("refus_tablexp", "error", "condition"), list(message = "refus", call = NULL, motif = "role_absent"))'
for (cas in list(
  list(nom = "erreur ordinaire", expr = 'stop("panne")', opt = "", code0 = FALSE),
  list(nom = "erreur ordinaire, erreur_quelconque = TRUE", expr = 'stop("panne")',
       opt = ", erreur_quelconque = TRUE", code0 = TRUE, nature = "1 par erreur quelconque"),
  list(nom = "refus sans motif attendu", expr = sprintf("stop(%s)", refus_temoin), opt = "",
       code0 = TRUE, nature = "1 par refus"),
  list(nom = "refus du motif attendu", expr = sprintf("stop(%s)", refus_temoin),
       opt = ', motif = "role_absent"', code0 = TRUE, nature = "1 par refus"),
  list(nom = "refus d'un autre motif", expr = sprintf("stop(%s)", refus_temoin),
       opt = ', motif = "autre_motif"', code0 = FALSE),
  list(nom = "valeur FALSE", expr = "FALSE", opt = "", code0 = TRUE, nature = "1 par valeur")
)) {
  r <- temoin(c(sprintf('echec_attendu("défaut", "#8", %s%s)', cas$expr, cas$opt), "bilan()"))
  verifier(sprintf("échec attendu, %s -> code %s", cas$nom, if (cas$code0) "0" else "non nul"),
           if ((r$statut == 0L) != cas$code0) paste("code", r$statut, ":", paste(r$sortie, collapse = " | "))
           else if (!is.null(cas$nature) && !any(grepl(cas$nature, r$sortie, fixed = TRUE, useBytes = TRUE)))
             "nature non décomptée au bilan" else TRUE)
}

# --- réutilisation d'une bibliothèque (audit de #8, M1) ----------------------

# Copie de l'arbre source (DESCRIPTION, NAMESPACE, R/) ; le témoin appelle
# installer_tablexp() sur la copie avec la bibliothèque de la batterie,
# transmise par executer_rscript() (TABLEXP_BIBLIOTHEQUE).
copie_source <- function(modifier = NULL) {
  d <- tempfile("source_temoin_"); dir.create(file.path(d, "R"), recursive = TRUE)
  file.copy(file.path(RACINE, c("DESCRIPTION", "NAMESPACE", "LICENSE")), d)
  file.copy(list.files(file.path(RACINE, "R"), full.names = TRUE), file.path(d, "R"))
  if (!is.null(modifier)) modifier(d)
  d
}
installe <- function(d) temoin(c(sprintf("installer_tablexp(%s)", deparse(d)), 'verifier("vrai", TRUE)', "bilan()"))

d <- copie_source()
r <- installe(d)
verifier("bibliothèque fournie, arbre identique -> réutilisée, code 0",
         r$statut == 0L && any(grepl("empreinte de l'arbre identique", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(d, recursive = TRUE)
d <- copie_source(function(d) cat("f_ajoutee <- function() 1\n", file = file.path(d, "R", "outils.R"), append = TRUE))
r <- installe(d)
verifier("bibliothèque fournie, arbre modifié -> réinstallée, code 0",
         r$statut == 0L && any(grepl("réinstallation", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(d, recursive = TRUE)
d <- copie_source(function(d) cat("f_cassee <- function( {\n", file = file.path(d, "R", "outils.R"), append = TRUE))
r <- installe(d)
verifier("bibliothèque fournie, arbre cassé -> réinstallation en échec, code non nul",
         r$statut != 0L && any(grepl("réinstallation", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(d, recursive = TRUE)

# --- proche() et noms d'assertion (audit de #8, m6) --------------------------

verifier("proche() : Inf égal à Inf", isTRUE(proche(c(1, Inf, -Inf), c(1, Inf, -Inf), rel = 0, abs = 0)))
verifier("proche() : Inf différent de -Inf", is.character(proche(Inf, -Inf, rel = 1, abs = 1)))
verifier("proche() : Inf différent d'un fini", is.character(proche(Inf, 1, rel = 1, abs = 1)))
verifier("proche() : NaN distingué de NA", is.character(proche(NaN, NA_real_, rel = 0, abs = 0)))
verifier("proche() : NaN égal à NaN, NA égal à NA",
         isTRUE(proche(c(NaN, NA), c(NaN, NA), rel = 0, abs = 0)))
r <- temoin(c('verifier(c("a", "b"), TRUE)', "bilan()"))
verifier("témoin : nom d'assertion de longueur 2 -> code non nul", r$statut != 0L)

# --- constats résiduels d'audit de #8 (R1, R4, R5, R6, valeurs d'echec_attendu)

# R1 : bilan() neutralisé dans un fichier de test.
d <- dossier_temoin(list(test_temoin.R = c('verifier("vrai", TRUE)', "bilan()")))
r <- executer_rscript(LANCEUR, c("--dossier", shQuote(d)))
verifier("lanceur : bilan() dans un fichier de test -> code non nul, motif signalé",
         if (r$statut == 0L) "code de sortie 0"
         else if (!any(grepl("bilan() interdit", r$sortie, fixed = TRUE, useBytes = TRUE)))
           paste("motif non signalé :", paste(r$sortie, collapse = " | ")) else TRUE)
unlink(d, recursive = TRUE)

# R4 : proche() refuse les comparaisons sans objet ou mal déclarées.
verifier("proche() : chaîne contre nombre -> écart décrit (pas d'égalité par as.numeric)",
         is.character(proche("1", 1, rel = 0, abs = 0)))
verifier("proche() : NULL contre NULL -> écart décrit", is.character(proche(NULL, NULL, rel = 0, abs = 0)))
verifier("proche() : longueur 0 des deux côtés -> écart décrit",
         is.character(proche(numeric(0), numeric(0), rel = 0, abs = 0)))
verifier("proche() : logique admis (TRUE contre 1)", isTRUE(proche(TRUE, 1, rel = 0, abs = 0)))
for (tol in list(list(rel = NA_real_, abs = 0), list(rel = 0, abs = -1), list(rel = c(0, 1), abs = 0),
                 list(rel = "0", abs = 0)))
  verifier(sprintf("proche() : tolérances rel = %s, abs = %s -> erreur", deparse(tol$rel), deparse(tol$abs)),
           inherits(tryCatch(proche(1, 1, rel = tol$rel, abs = tol$abs), error = function(e) e), "error"))

# Valeurs d'echec_attendu() : ni FALSE ni chaîne -> ECHEC, comme verifier().
for (v in c("NA", "1", "list(FALSE)", "character(0)")) {
  r <- temoin(c(sprintf('echec_attendu("défaut", "#8", %s)', v), "bilan()"))
  verifier(sprintf("échec attendu qui rend %s -> code non nul (ECHEC)", v),
           r$statut != 0L && grepl("; 1 échec(s) ;", ligne_bilan(r), fixed = TRUE, useBytes = TRUE))
}

# R5 : empreinte de l'arbre installable (inst/, data/, src/, sous-dossiers).
d <- copie_source()
e0 <- empreinte_source(d)
dir.create(file.path(d, "inst", "extdata"), recursive = TRUE)
writeLines("a", file.path(d, "inst", "extdata", "table.csv"))
e1 <- empreinte_source(d)
writeLines("b", file.path(d, "inst", "extdata", "table.csv"))
e2 <- empreinte_source(d)
dir.create(file.path(d, "data")); writeLines("x", file.path(d, "data", "jeu.R"))
dir.create(file.path(d, "src")); writeLines("/* */", file.path(d, "src", "f.c"))
e3 <- empreinte_source(d)
verifier("empreinte_source() : change quand inst/ apparaît, quand un fichier d'inst/ change, quand data/ et src/ apparaissent",
         length(unique(c(e0, e1, e2, e3))) == 4L)
unlink(file.path(d, c("data", "src")), recursive = TRUE)
r <- installe(d)
verifier("bibliothèque fournie, fichier d'inst/ ajouté -> réinstallée, code 0",
         r$statut == 0L && any(grepl("réinstallation", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(d, recursive = TRUE)

# R6 : sorties délibérées sans le message de la garde .Last.
GARDE <- "sortie du processus avant la ligne de bilan"
d <- copie_source(function(d) cat("f_cassee <- function( {\n", file = file.path(d, "R", "outils.R"), append = TRUE))
r <- installe(d)
verifier("installation en échec : code non nul, sans le message de la garde de sortie",
         r$statut != 0L && any(grepl("ECHEC : installation", r$sortie, fixed = TRUE, useBytes = TRUE)) &&
           !any(grepl(GARDE, r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(d, recursive = TRUE)
r <- executer_rscript(file.path(RACINE, "tests", "concordance_doc_moteur.R"), c("--strict", "--latex"))
verifier("concordance, option sans valeur : code non nul, sans le message de la garde de sortie",
         r$statut != 0L && any(grepl("option --latex sans valeur", r$sortie, fixed = TRUE, useBytes = TRUE)) &&
           !any(grepl(GARDE, r$sortie, fixed = TRUE, useBytes = TRUE)))
r <- temoin(c('verifier("faux", FALSE)', "bilan()"))
verifier("bilan() en échec : code non nul, sans le message de la garde de sortie",
         r$statut != 0L && !any(grepl(GARDE, r$sortie, fixed = TRUE, useBytes = TRUE)))
