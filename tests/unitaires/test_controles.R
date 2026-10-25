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
  if (!is.null(motif) && !any(grepl(motif, r$sortie, fixed = TRUE, useBytes = TRUE)))
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

# --- absence de Java (ADR 0002, A1 ; ADR 0008, point 7 ; issue #3) ----------
# Hors réseau : dépôts locaux file:// dont PACKAGES est écrit ici ; la liste
# du dépôt est lue telle quelle, sauf témoin de liste. Le contrôle sur
# l'instantané daté est le job CI « Absence de Java ».

depot_temoin <- function(entrees) {
  d <- tempfile("depot_temoin_")
  dir.create(file.path(d, "src", "contrib"), recursive = TRUE)
  lignes <- unlist(lapply(names(entrees), function(p)
    c(sprintf("Package: %s", p), "Version: 1.0", entrees[[p]], "")))
  writeLines(lignes, file.path(d, "src", "contrib", "PACKAGES"))
  d
}
# URL de fichier absolue : « file:// » + chemin commençant par « / »
# (POSIX : file:///tmp/…) ; sous Windows, le chemin « C:/… » prend un « / »
# de tête (file:///C:/…, forme portable), faute de quoi « C: » serait lu
# comme un hôte (issue #33).
url_depot <- function(d) {
  chemin <- normalizePath(d, winslash = "/", mustWork = FALSE)
  paste0("file://", if (startsWith(chemin, "/")) "" else "/", chemin)
}
liste_temoin <- function(lignes) {
  f <- tempfile("liste_", fileext = ".csv")
  writeLines(c("ensemble,package,statut,source", lignes), f)
  f
}
# Dépôt où figurent tous les packages de la liste du dépôt, sans dépendance,
# plus ELT -> xlsx -> xlsxjars -> rJava (dépendances d'ELT 1.7 et de xlsx
# relevées sur le CRAN le 17/10/2026, réduites à la chaîne Java) et deux
# packages propres à des témoins.
noms_liste <- utils::read.csv(file.path(CONTROLES, "packages_hors_moteur.csv"),
                              colClasses = "character")$package
entrees <- stats::setNames(rep(list(character(0)), length(noms_liste)), noms_liste)
entrees <- c(entrees, list(
  ELT = "Depends: R (>= 2.10.0), locfit, lattice, xlsx",
  lattice = "Imports: grid, grDevices, graphics, stats, utils",
  xlsx = "Imports: rJava, xlsxjars, grDevices, utils",
  xlsxjars = "Depends: rJava",
  rJava = "Depends: R (>= 3.6.0), methods",
  appui = "Imports: intermediaire",
  intermediaire = "LinkingTo: xlsxjars",
  orphelin = "Imports: introuvable, stats"))
depot <- depot_temoin(entrees)
aj <- function(...) controle("absence_java.R", "--depot", shQuote(url_depot(depot)), ...)

verifier("absence de Java : URL du dépôt local file:///… (trois barres, pas quatre), lisible par available.packages",
         grepl("^file:///[^/]", url_depot(depot)) &&
           nrow(utils::available.packages(repos = url_depot(depot), type = "source")) > 0L)
verifier("absence de Java : passe sur la liste du dépôt (dépôt local)", passe(aj()))
verifier("absence de Java : échoue sur le témoin ELT, chaîne affichée",
         echoue(aj("--ajouter", "ELT"), "ELT dépend de Java (ELT -> xlsx -> rJava)"))
r <- aj("--ajouter", "ELT")
verifier("absence de Java : témoin ELT -> code de sortie 1 (verdict, pas panne)", r$statut == 1L)
verifier("absence de Java : échoue sur un package Java listé directement",
         echoue(aj("--ajouter", "rJava"), "rJava est un package Java"))
verifier("absence de Java : échoue par LinkingTo, au second niveau",
         echoue(aj("--ajouter", "appui"), "appui -> intermediaire -> xlsxjars -> rJava"))
verifier("absence de Java : échoue sur un package absent des métadonnées",
         echoue(aj("--ajouter", "inexistant"), "inexistant absent des métadonnées"))
verifier("absence de Java : échoue sur une dépendance absente des métadonnées",
         echoue(aj("--ajouter", "orphelin"), "dépendance introuvable absente"))
verifier("absence de Java : package de priorité base ajouté -> passe",
         passe(aj("--ajouter", "stats")))
f <- tempfile("DESCRIPTION_")
writeLines(c("Package: x", "Depends: R (>= 4.3.3)", "LinkingTo: xlsxjars"), f)
verifier("absence de Java : échoue sur une dépendance Java du moteur (DESCRIPTION)",
         echoue(aj("--description", shQuote(f)), "moteur : xlsxjars est un package Java"))
writeLines(c("Package: x", "Depends: R (>= 4.3.3)", "Imports: xlsx [bad]"), f)
verifier("absence de Java : échoue sur une entrée illisible de DESCRIPTION",
         echoue(aj("--description", shQuote(f)), "Imports : entrée illisible « xlsx [bad] »"))
writeLines(c("Package: x", "ligne sans deux-points"), f)
r <- aj("--description", shQuote(f))
verifier("absence de Java : DESCRIPTION mal formée -> défaut explicite, code 1",
         r$statut == 1L && any(grepl("ECHEC : DESCRIPTION illisible", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(f)
for (cas in list(
  list(nom = "ensemble inconnu", lignes = c("application,shiny,obligatoire,x", "contre_verification,WH,obligatoire,x",
                                           "moteur,stats,obligatoire,x"), motif = "ensemble « moteur » inconnu"),
  list(nom = "statut inconnu", lignes = c("application,shiny,requis,x", "contre_verification,WH,obligatoire,x"),
       motif = "statut « requis » inconnu"),
  list(nom = "doublon", lignes = c("application,shiny,obligatoire,x", "application,shiny,facultatif,y",
                                   "contre_verification,WH,obligatoire,x"), motif = "shiny en double"),
  list(nom = "ensemble vide", lignes = "application,shiny,obligatoire,x", motif = "ensemble contre_verification vide"),
  list(nom = "nom invalide", lignes = c("application,shiny pro,obligatoire,x", "contre_verification,WH,obligatoire,x"),
       motif = "« shiny pro » invalide")
)) {
  f <- liste_temoin(cas$lignes)
  verifier(sprintf("absence de Java : échoue sur la liste témoin « %s »", cas$nom),
           echoue(aj("--liste", shQuote(f)), cas$motif))
  unlink(f)
}
f <- tempfile("liste_", fileext = ".csv")
writeLines("ensemble,package", f)
verifier("absence de Java : échoue sur une liste aux colonnes inattendues",
         echoue(aj("--liste", shQuote(f)), "colonnes"))
unlink(f)
verifier("absence de Java : échoue sur une option inconnue", echoue(aj("--inconnue"), "option inconnue"))
verifier("absence de Java : échoue sur --ajouter sans valeur", echoue(aj("--ajouter"), "option sans valeur"))
r <- controle("absence_java.R", "--depot", shQuote(url_depot(tempfile("depot_absent_"))))
verifier("absence de Java : métadonnées illisibles -> code de sortie 2, sans verdict",
         r$statut == 2L && any(grepl("métadonnées illisibles", r$sortie, fixed = TRUE, useBytes = TRUE)))
unlink(depot, recursive = TRUE)

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
  list(nom = "get passée comme objet (R3)",
       fichiers = list(outils = "f <- function(x) lapply(x, get)"), motif = "get()"),
  list(nom = "base::get passée comme objet (R3)",
       fichiers = list(outils = "f <- function(x) vapply(x, base::get, numeric(1))"), motif = "get()"),
  list(nom = "match.fun passée comme objet (R3)",
       fichiers = list(outils = "f <- function(x) Map(match.fun, x)"), motif = "match.fun()"),
  list(nom = "asNamespace() (R3)",
       fichiers = list(outils = "f <- function() asNamespace(\"tablexp\")$g"), motif = "asNamespace()"),
  list(nom = "getNamespace() (R3)",
       fichiers = list(outils = "f <- function() getNamespace(\"tablexp\")"), motif = "getNamespace()"),
  list(nom = "methods::getFunction() (R3)",
       fichiers = list(outils = "f <- function() methods::getFunction(\"g\")"), motif = "getFunction()"),
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

# --- tablexp sous la locale C (issue #66) -------------------------------------
# Le contrôle complet (deux installations, R1 à R6 sous C et sous UTF-8) dure
# une trentaine de secondes : il est une étape de la CI, non rejouée ici. Seul
# le chemin d'échec bon marché l'est : une racine sans package sort en code 1
# et nomme l'installation en échec.

racine_sans_package <- tempfile("racine_sans_package_")
dir.create(racine_sans_package)
verifier("locale C : racine sans package -> échec, installation sous LC_ALL=C nommée",
         echoue(controle("locale_c.R", shQuote(racine_sans_package)), "installation sous LC_ALL=C en échec"))
unlink(racine_sans_package, recursive = TRUE)

# locale_c.R sourcé ne fait que définir (sa dernière ligne ne lance principal()
# que hors de toute fonction, sys.nframe() == 0L). LOCALES_UTF8 y est dupliquée
# du harnais : l'égalité est contrôlée ici. ecart_octets() (message d'écart de
# la comparaison octet à octet) n'est pas atteinte par le chemin d'échec bon
# marché ci-dessus : attendus écrits à la main.
# Chargement par parse(encoding = "UTF-8"), comme le harnais lit les tests
# (tests/test_unitaires.R) : sys.source() sans encodage laisse les littéraux
# non ASCII de locale_c.R « unknown » alors que les attendus ci-dessous sont
# marqués UTF-8, et identical() rend alors FALSE sous la locale C (issue #81).
# La garde sys.nframe() == 0L reste sans effet : eval() ouvre un cadre.
env_locale_c <- new.env()
for (expr_ in parse(file.path(CONTROLES, "locale_c.R"), encoding = "UTF-8")) eval(expr_, env_locale_c)
verifier("locale C : sourcé, locale_c.R ne définit que des fonctions et constantes ; LOCALES_UTF8 identique à celle du harnais",
         identical(env_locale_c$LOCALES_UTF8, LOCALES_UTF8) && is.function(env_locale_c$ecart_octets) &&
           is.function(env_locale_c$locale_utf8_disponible) &&
           identical(env_locale_c$locale_utf8_disponible(), locale_utf8_disponible()))
octets_ <- function(t) charToRaw(t)
verifier("locale C : ecart_octets() — textes identiques (le premier octet « différent » est celui qui suit la fin)",
         identical(env_locale_c$ecart_octets(octets_("ab\ncd\n"), octets_("ab\ncd\n")),
                   "longueurs 6 et 6 octets ; premier octet différent : 7 (ligne 3)"))
verifier("locale C : ecart_octets() — premier octet différent à la ligne 2",
         identical(env_locale_c$ecart_octets(octets_("ab\ncd\n"), octets_("ab\nXd\n")),
                   "longueurs 6 et 6 octets ; premier octet différent : 4 (ligne 2)"))
verifier("locale C : ecart_octets() — longueurs inégales à préfixe commun (premier octet différent = premier octet manquant)",
         identical(env_locale_c$ecart_octets(octets_("ab\ncd"), octets_("ab\ncd\nef")),
                   "longueurs 5 et 8 octets ; premier octet différent : 6 (ligne 2)") &&
           identical(env_locale_c$ecart_octets(octets_("ab\ncd\nef"), octets_("ab\ncd")),
                     "longueurs 8 et 5 octets ; premier octet différent : 6 (ligne 2)"))

# --- identité des batteries et de l'image (CLAUDE.md, BATTERIES, ci.yml) -----

verifier("identité des batteries : passe sur le dépôt", passe(controle("identite_batteries.R")))
copie_racine <- function() {
  d <- tempfile("racine_temoin_")
  dir.create(file.path(d, ".claude", "workflows"), recursive = TRUE)
  dir.create(file.path(d, ".github", "workflows"), recursive = TRUE)
  file.copy(file.path(RACINE, "CLAUDE.md"), d)
  file.copy(file.path(RACINE, ".claude", "workflows", "circuit-technique.js"),
            file.path(d, ".claude", "workflows"))
  file.copy(file.path(RACINE, ".github", "workflows", c("ci.yml", "references.yml")),
            file.path(d, ".github", "workflows"))
  d
}
# Remplace la première occurrence de `motif` (éventuellement sur plusieurs
# lignes, séparées par \n) dans le texte entier ; rend le nombre
# d'occurrences.
modifier <- function(chemin, motif, remplacement) {
  texte <- paste(readLines(chemin), collapse = "\n")
  n <- sum(gregexpr(motif, texte, fixed = TRUE, useBytes = TRUE)[[1L]] > 0L)
  writeLines(sub(motif, remplacement, texte, fixed = TRUE, useBytes = TRUE), chemin)
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
  list(nom = "continue-on-error sur une étape hors batterie (R2)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "        run: Rscript tests/controles/plateforme.R --exiger",
       remplacement = "        continue-on-error: true\n        run: Rscript tests/controles/plateforme.R --exiger",
       ecart = "continue-on-error interdit"),
  list(nom = "continue-on-error au niveau du job (R2)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "    timeout-minutes: 30", remplacement = "    timeout-minutes: 30\n    continue-on-error: true",
       ecart = "continue-on-error interdit"),
  list(nom = "if: au niveau du job de tests (m4)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "    timeout-minutes: 30", remplacement = "    timeout-minutes: 30\n    if: false",
       ecart = "condition interdite au niveau du job"),
  list(nom = "if: false sur une étape de batterie (R2)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "      - name: \"Batterie : concordance documentation et moteur\"\n        if: ${{ !cancelled() }}",
       remplacement = "      - name: \"Batterie : concordance documentation et moteur\"\n        if: false",
       ecart = "condition `if: false` interdite"),
  list(nom = "if: always() sur une étape de batterie (R2)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "      - name: \"Batterie : tests unitaires\"\n        if: ${{ !cancelled() }}",
       remplacement = "      - name: \"Batterie : tests unitaires\"\n        if: ${{ always() && false }}",
       ecart = "interdite sur l'étape"),
  list(nom = "job de tests sans container:", fichier = c(".github", "workflows", "ci.yml"),
       motif = "    timeout-minutes: 30\n    container:", remplacement = "    timeout-minutes: 30\n    conteneur_retire:",
       ecart = "sans `container:`"),
  list(nom = "job de tests renommé (batteries hors du job tests:)", fichier = c(".github", "workflows", "ci.yml"),
       motif = "  tests:", remplacement = "  autres:",
       ecart = "job `tests:` absent"),
  list(nom = "TABLEXP_IMAGE différente de container.image", fichier = c(".github", "workflows", "ci.yml"),
       motif = "TABLEXP_IMAGE: rocker/r-ver:4.3.3", remplacement = "TABLEXP_IMAGE: rocker/r-ver:4.3.2",
       # Une TABLEXP_IMAGE par job conteneurisé (tests, absence_java) : en
       # modifier une seule (la première) suffit à l'écart.
       plusieurs = TRUE,
       ecart = "chaînes d'image différentes"),
  # Contrôle 4 : workflow de régénération (ADR 0009, point 6.1). Motifs
  # d'écart en ASCII : la sortie du sous-processus est lue sans conversion
  # (locale C en CI comme sur le poste).
  list(nom = "bascule BLAS et LAPACK modifiée dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "          apt-get update -q\n", remplacement = "          apt-get update -qq\n",
       ecart = "de celui de ci.yml (ligne 4 du bloc"),
  list(nom = "bascule BLAS et LAPACK modifiée dans ci.yml seul", fichier = c(".github", "workflows", "ci.yml"),
       motif = "          dpkg-query -W -f='${Package} ${Version}\\n' libblas3",
       remplacement = "          dpkg-query -l\n          dpkg-query -W -f='${Package} ${Version}\\n' libblas3",
       ecart = "de celui de ci.yml (ligne 10 du bloc"),
  list(nom = "on: push ajouté à references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "on:\n  workflow_dispatch:", remplacement = "on:\n  push:\n  workflow_dispatch:",
       ecart = "(s) push, workflow_dispatch au lieu de workflow_dispatch seul"),
  list(nom = "étape de plateforme retirée de references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "          Rscript tests/controles/plateforme.R --exiger | tee", remplacement = "          echo plateforme | tee",
       ecart = "plateforme (`Rscript tests/controles/plateforme.R --exiger`) absente"),
  list(nom = "commande de production à la place de la plateforme dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "          Rscript tests/controles/plateforme.R --exiger | tee \"$SORTIE/plateforme.txt\"\n",
       remplacement = "          Rscript tests/reference/produire_references.R --mode x\n",
       ecart = "absente ou en double"),
  list(nom = "if: always() sur la production de references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "      - name: Production et comparaison des références\n",
       remplacement = "      - name: Production et comparaison des références\n        if: always()\n",
       ecart = "condition `if:` interdite"),
  list(nom = "permissions: contents: write dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "  contents: read", remplacement = "  contents: write", ecart = "`contents: read` attendu"),
  list(nom = "permissions au niveau du job de references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "    timeout-minutes: 30\n", remplacement = "    timeout-minutes: 30\n    permissions: write-all\n",
       ecart = "`contents: read` attendu"),
  list(nom = "secret employé dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "      SORTIE: sortie_references", remplacement = "      SORTIE: sortie_references\n      JETON: ${{ secrets.JETON }}",
       ecart = "references.yml : secret employ"),
  list(nom = "continue-on-error dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "    timeout-minutes: 30\n", remplacement = "    timeout-minutes: 30\n    continue-on-error: true\n",
       ecart = "references.yml : continue-on-error interdit"),
  list(nom = "OMP_NUM_THREADS à 2 dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "      OMP_NUM_THREADS: \"1\"", remplacement = "      OMP_NUM_THREADS: \"2\"", ecart = "OMP_NUM_THREADS : 2"),
  list(nom = "SHA d'actions/checkout différent dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "actions/checkout@11d5960a326750d5838078e36cf38b85af677262",
       remplacement = "actions/checkout@0000000000000000000000000000000000000000", ecart = "SHA de actions/checkout"),
  list(nom = "action non épinglée par SHA dans references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "actions/upload-artifact@ea165f8d65b6e75b540449e92b4886f43607fa02", remplacement = "actions/upload-artifact@v4",
       ecart = "par SHA : actions/upload-artifact@v4"),
  list(nom = "references.yml sans container:", fichier = c(".github", "workflows", "references.yml"),
       motif = "    container:\n", remplacement = "    conteneur_retire:\n", ecart = "sans `container:`"),
  list(nom = "inputs interpolé dans un run: de references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "--motif \"$MOTIF\"", remplacement = "--motif \"${{ inputs.motif }}\"",
       ecart = "interpolation d'entr"),
  list(nom = "github.event interpolé dans un run: de references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "            echo \"mode : $MODE\"", remplacement = "            echo \"mode : ${{ github.event.inputs.mode }}\"",
       ecart = "interpolation d'entr"),
  list(nom = "persist-credentials retiré de references.yml", fichier = c(".github", "workflows", "references.yml"),
       motif = "          persist-credentials: false", remplacement = "          persist-credentials: true",
       ecart = "persist-credentials: false absent")
)) {
  d <- copie_racine()
  chemin <- do.call(file.path, as.list(c(d, cas$fichier)))
  n <- if (cas$ecart == "marque") {
    # Octets UTF-8 écrits tels quels (useBytes), quelle que soit la locale.
    con <- file(chemin, "a"); writeLines("**\u00c0 ADAPTER** : marque témoin", con, useBytes = TRUE)
    close(con); 1L
  } else modifier(chemin, cas$motif, cas$remplacement)
  verifier(sprintf("identité des batteries : témoin « %s » préparé (%d occurrence(s), 1 remplacée)", cas$nom, n),
           if (isTRUE(cas$plusieurs)) n >= 1L else n == 1L)
  verifier(sprintf("identité des batteries : échoue sur le témoin « %s »", cas$nom),
           echoue(controle("identite_batteries.R", shQuote(d)), cas$ecart))
  unlink(d, recursive = TRUE)
}

d <- copie_racine()
unlink(file.path(d, ".github", "workflows", "references.yml"))
verifier("identité des batteries : échoue sur le témoin « references.yml absent »",
         echoue(controle("identite_batteries.R", shQuote(d)), "references.yml absent"))
unlink(d, recursive = TRUE)
d <- copie_racine()
verifier("identité des batteries : passe sur la copie non altérée (references.yml compris)",
         passe(controle("identite_batteries.R", shQuote(d))))
unlink(d, recursive = TRUE)

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
           if (length(d) == 1L && grepl(cas$motif, d, fixed = TRUE, useBytes = TRUE)) TRUE
           else paste("écarts :", paste(d, collapse = " ; ")))
}
# Version de la glibc (issue #72) : consignée, hors identifiant contrôlé.
verifier("plateforme : analyser_glibc lit getconf, puis ldd, sinon « inconnue »",
         identical(env_plateforme$analyser_glibc("glibc 2.35"), "glibc 2.35") &&
           identical(env_plateforme$analyser_glibc("getconf: Unrecognized variable", "ldd (Ubuntu GLIBC 2.35-0ubuntu3.8) 2.35"),
                     "ldd (Ubuntu GLIBC 2.35-0ubuntu3.8) 2.35") &&
           identical(env_plateforme$analyser_glibc(character(0), "musl libc (x86_64)"), "inconnue") &&
           identical(env_plateforme$analyser_glibc(), "inconnue"))
verifier("plateforme : version_glibc() rend une chaîne unique, sans erreur ni avertissement",
         {
           avert <- character(0)
           v <- withCallingHandlers(env_plateforme$version_glibc(),
                                    warning = function(w) { avert <<- c(avert, conditionMessage(w)); invokeRestart("muffleWarning") })
           is.character(v) && length(v) == 1L && nzchar(v) && !length(avert)
         })
verifier("plateforme : la glibc figure dans consignes_plateforme(), pas dans lignes_identifiant()",
         "glibc (libm)" %in% names(env_plateforme$consignes_plateforme()) &&
           !any(grepl("glibc", env_plateforme$lignes_identifiant(conforme), ignore.case = TRUE)) &&
           any(grepl("glibc (libm)", env_plateforme$lignes_consignees(), fixed = TRUE)))
# GITHUB_STEP_SUMMARY vidée : le résumé du job CI ne reçoit que le relevé de
# l'étape « Plateforme (mode strict) », pas ceux des témoins.
r <- executer_rscript(file.path(CONTROLES, "plateforme.R"), env = "GITHUB_STEP_SUMMARY=")
verifier("plateforme : mode relevé -> code 0 et sessionInfo() au journal",
         r$statut == 0L && any(r$sortie == "sessionInfo()") &&
           any(grepl(R.version.string, r$sortie, fixed = TRUE, useBytes = TRUE)))
r <- executer_rscript(file.path(CONTROLES, "plateforme.R"), "--exiger",
                      env = c("OPENBLAS_NUM_THREADS=2", "GITHUB_STEP_SUMMARY="))
verifier("plateforme : mode strict hors plateforme de référence -> code non nul et ::error",
         r$statut != 0L && any(startsWith(r$sortie, "::error")))

# --- concordance documentation <-> moteur (E3.3) -----------------------------

CONCORDANCE <- file.path(RACINE, "tests", "concordance_doc_moteur.R")
concordance <- function(...) executer_rscript(CONCORDANCE, c(...))
# Écriture en octets (useBytes = TRUE) : sous la locale C, writeLines() sans
# useBytes convertit les chaînes marquées UTF-8 vers le codage natif et écrit
# « <U+00E9> » à la place de « é » (#70). Pas d'enc2utf8() : sous la locale C, il
# change en « <c3><a9> » une chaîne non marquée qui porte des octets UTF-8.
latex_temoin <- function(lignes) {
  d <- tempfile("latex_temoin_"); dir.create(d)
  writeLines(lignes, file.path(d, "temoin.tex"), useBytes = TRUE)
  d
}
attente_temoin <- function(lignes) {
  f <- tempfile("attente_", fileext = ".csv")
  writeLines(c("type,nom,issue,motif", lignes), f, useBytes = TRUE)
  f
}
# Témoins construits sur les sources des catalogues requis (exports,
# fonctions-<fichier>, roles, recettes ; docs/latex/CONVENTIONS.md, section
# 3.2), relues ici indépendamment du script, pour rester valables quand un
# export, une définition ou un rôle s'ajoute.
EXPORTS <- sort(getNamespaceExports("tablexp"))
SOURCES <- local({
  g <- new.env()
  sys.source(file.path(CONTROLES, "graphe_modules.R"), envir = g)
  defs <- g$analyser_modules(file.path(RACINE, "R"))$symboles
  fichiers <- sort(sub("\\.[Rr]$", "", list.files(file.path(RACINE, "R"), pattern = "\\.[Rr]$")))
  f <- lapply(fichiers, function(m) defs$symbole[defs$module == m])
  names(f) <- paste0("fonctions-", gsub("_", "-", fichiers, fixed = TRUE, useBytes = TRUE))
  c(list(exports = EXPORTS), f,
    list(roles = catalogue_roles()$role, recettes = tablexp:::RECETTE_BRANCHE))
})
echapper <- function(n) gsub("_", "\\_", n, fixed = TRUE, useBytes = TRUE)
code_latex <- function(n) sprintf("\\code{%s()}", echapper(n))
# Entrée \catalogue{k}{nom} ; `()` pour une fonction de tablexp dans exports
# et fonctions-<fichier>.
entree <- function(k, n) {
  fonction <- (k == "exports" || startsWith(k, "fonctions-")) &&
    is.function(get(n, envir = asNamespace("tablexp")))
  sprintf("\\catalogue{%s}{%s%s}", k, echapper(n), if (fonction) "()" else "")
}
CATALOGUES_COMPLETS <- unlist(lapply(names(SOURCES), function(k)
  vapply(SOURCES[[k]], function(n) entree(k, n), character(1L))), use.names = FALSE)
retirer <- function(lignes, x) lignes[lignes != x]
vide <- latex_temoin("% aucun \\code{construire\\_table()} cité : ligne commentée")
cite <- latex_temoin(paste0("Exports : ", paste(code_latex(EXPORTS), collapse = ", "),
                            ", \\code{tools::md5sum()}, \\code{set.seed()}."))
inconnu <- latex_temoin("\\code{construire\\_table()} et \\code{fonction\\_inconnue()}.")
inconnu_args <- latex_temoin("\\code{construire\\_table(donnees, roles, plan)} et \\code{autre\\_inconnue(x, y = 2)}.")
cite_args <- latex_temoin("\\code{construire\\_table(donnees, roles, plan)}.")
# Catalogues complets, exports cités par le seul catalogue `exports` ; une
# entrée commentée n'est pas lue ; la ligne de définition de la macro n'est
# pas une entrée.
complet <- latex_temoin(c("\\newcommand{\\catalogue}[2]{\\code{#2}}", CATALOGUES_COMPLETS,
                          "% \\catalogue{bidule}{commentee}"))
role_retire <- SOURCES$roles[1L]
incomplet <- latex_temoin(retirer(CATALOGUES_COMPLETS, entree("roles", role_retire)))
en_trop <- latex_temoin(c(CATALOGUES_COMPLETS, "\\catalogue{recettes}{recette\\_inventee}"))
doublon <- latex_temoin(c(CATALOGUES_COMPLETS, entree("fonctions-outils", "sous_graine")))
cat_inconnu <- latex_temoin(c(CATALOGUES_COMPLETS, "\\catalogue{bidule}{x}"))
sans_roles <- latex_temoin(CATALOGUES_COMPLETS[!startsWith(CATALOGUES_COMPLETS, "\\catalogue{roles}")])
forme <- latex_temoin(c(retirer(CATALOGUES_COMPLETS, entree("fonctions-outils", "sous_graine")),
                        "\\catalogue{fonctions-outils}{sous\\_graine}"))
lignes_catalogues <- sprintf("catalogue,%s,#11,en attente", names(SOURCES))
attente_ok <- attente_temoin(c(sprintf("export,%s,#11,en attente", EXPORTS), lignes_catalogues))
attente_cat <- attente_temoin(lignes_catalogues)
attente_vide <- attente_temoin(character(0))
attente_perimee <- attente_temoin(c(sprintf("export,%s,#11,en attente", EXPORTS), lignes_catalogues,
                                    "export,nom_disparu,#9,retiré"))
attente_sans_issue <- attente_temoin(c("export,construire_table,11,en attente", lignes_catalogues))
attente_cat_inconnu <- attente_temoin(c(lignes_catalogues, "catalogue,bidule,#11,en attente"))
attente_type <- attente_temoin(c(lignes_catalogues, "fonction,construire_table,#11,en attente"))
attente_doublon <- attente_temoin(c(lignes_catalogues, lignes_catalogues[1L]))
attente_ancienne <- tempfile("attente_", fileext = ".csv")
writeLines(c("nom,issue,motif", "construire_table,#11,en attente"), attente_ancienne)

verifier("concordance : passe sur le dépôt (--strict)", passe(concordance("--strict")))
verifier("concordance : export non cité, en attente -> code 0",
         passe(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_ok))))
verifier("concordance : export non cité, hors attente -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_cat)),
                "absent de la liste d'attente"))
verifier("concordance : export cité et encore en attente -> succès inattendu, échec",
         echoue(concordance("--strict", "--latex", shQuote(cite), "--attente", shQuote(attente_ok)),
                "succès inattendu"))
verifier("concordance : export cité, catalogues en attente, fonctions de base citées -> code 0",
         passe(concordance("--strict", "--latex", shQuote(cite), "--attente", shQuote(attente_cat))))
verifier("concordance : fonction citée introuvable -> échec",
         echoue(concordance("--strict", "--latex", shQuote(inconnu), "--attente", shQuote(attente_cat)),
                "fonction_inconnue() existe"))
verifier("concordance : ligne d'attente qui n'est pas un export -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_perimee)),
                "nom_disparu"))
verifier("concordance : ligne d'attente sans renvoi « #N » -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_sans_issue))))
verifier("concordance : fonction citée avec arguments introuvable -> échec",
         echoue(concordance("--strict", "--latex", shQuote(inconnu_args), "--attente", shQuote(attente_cat)),
                "autre_inconnue() existe"))
verifier("concordance : export cité avec arguments -> lu comme cité (succès inattendu en attente)",
         echoue(concordance("--strict", "--latex", shQuote(cite_args), "--attente", shQuote(attente_ok)),
                "succès inattendu"))
# Citation \code{nom(...)} répartie sur deux lignes ou plus (#90) : lue comme
# une citation sur une ligne ; un témoin de chaque issue du contrôle 1.
inconnu_2l <- latex_temoin(c("\\code{construire\\_table(donnees, roles, plan)} et",
                             "\\code{fantome\\_deux\\_lignes(x,", "  y = 2)}."))
inconnu_3l <- latex_temoin(c("\\code{fantome\\_trois\\_lignes(x,", "  y = 2,", "  z = 3)}."))
cite_2l <- latex_temoin(c("\\code{construire\\_table(donnees,", "  roles, plan)}."))
base_2l <- latex_temoin(c("\\code{tools::md5sum(files,", "  x)} et \\code{set.seed(", "1)}."))
verifier("concordance : fonction citée introuvable, \\code{nom(...)} sur deux lignes -> échec (#90)",
         echoue(concordance("--strict", "--latex", shQuote(inconnu_2l), "--attente", shQuote(attente_cat)),
                "fantome_deux_lignes() existe"))
verifier("concordance : fonction citée introuvable, \\code{nom(...)} sur trois lignes -> échec (#90)",
         echoue(concordance("--strict", "--latex", shQuote(inconnu_3l), "--attente", shQuote(attente_cat)),
                "fantome_trois_lignes() existe"))
verifier("concordance : export cité sur deux lignes -> lu comme cité (succès inattendu en attente) (#90)",
         echoue(concordance("--strict", "--latex", shQuote(cite_2l), "--attente", shQuote(attente_ok)),
                "succès inattendu"))
verifier("concordance : fonctions de base citées sur deux lignes (pkg::nom et nom) -> code 0 (#90)",
         passe(concordance("--strict", "--latex", shQuote(base_2l), "--attente", shQuote(attente_ok))))
verifier("concordance : --latex sans valeur -> échec",
         echoue(concordance("--strict", "--latex")))
verifier("concordance : --latex vers un dossier inexistant -> échec",
         echoue(concordance("--strict", "--latex", shQuote(tempfile("absent_"))), "dossier introuvable"))
verifier("concordance : sans --strict, écart -> code 0 (mode rapport)",
         passe(concordance("--latex", shQuote(inconnu), "--attente", shQuote(attente_vide))))
# Catalogues (CONVENTIONS.md, section 3.2).
verifier(sprintf("concordance : témoin des catalogues complet (%d entrées, %d catalogues)",
                 length(CATALOGUES_COMPLETS), length(SOURCES)),
         length(CATALOGUES_COMPLETS) > length(SOURCES) && all(lengths(SOURCES) > 0L))
verifier("concordance : catalogues complets, attente vide -> code 0 (exports cités par le catalogue)",
         passe(concordance("--strict", "--latex", shQuote(complet), "--attente", shQuote(attente_vide))))
verifier("concordance : catalogue incomplet -> échec",
         echoue(concordance("--strict", "--latex", shQuote(incomplet), "--attente", shQuote(attente_vide)),
                paste("absents du document :", role_retire)))
verifier("concordance : nom de catalogue en trop -> échec",
         echoue(concordance("--strict", "--latex", shQuote(en_trop), "--attente", shQuote(attente_vide)),
                "absents de la source : recette_inventee"))
verifier("concordance : nom de catalogue en double -> échec",
         echoue(concordance("--strict", "--latex", shQuote(doublon), "--attente", shQuote(attente_vide)),
                "en double : sous_graine"))
verifier("concordance : catalogue inconnu -> échec",
         echoue(concordance("--strict", "--latex", shQuote(cat_inconnu), "--attente", shQuote(attente_vide)),
                "catalogue inconnu"))
verifier("concordance : fonction de catalogue sans () -> échec",
         echoue(concordance("--strict", "--latex", shQuote(forme), "--attente", shQuote(attente_vide)),
                "forme fausse : sous_graine (fonction sans ())"))
# Entrées de catalogue en attente (type `entree`, #15).
entree_attendue <- SOURCES[["fonctions-exposition"]][1L]
sans_entree <- latex_temoin(retirer(CATALOGUES_COMPLETS, entree("fonctions-exposition", entree_attendue)))
attente_entree <- attente_temoin(sprintf("entree,fonctions-exposition:%s,#15,en attente", entree_attendue))
verifier("concordance : entrée de catalogue absente, en attente (type entree) -> code 0",
         passe(concordance("--strict", "--latex", shQuote(sans_entree), "--attente", shQuote(attente_entree))))
verifier("concordance : entrée de catalogue absente, hors attente -> échec",
         echoue(concordance("--strict", "--latex", shQuote(sans_entree), "--attente", shQuote(attente_vide)),
                paste("absents du document :", entree_attendue)))
verifier("concordance : entrée de catalogue présente et encore en attente -> succès inattendu, échec",
         echoue(concordance("--strict", "--latex", shQuote(complet), "--attente", shQuote(attente_entree)),
                "succès inattendu"))
verifier("concordance : entrée en attente absente de la source -> échec",
         echoue(concordance("--strict", "--latex", shQuote(complet), "--attente",
                            shQuote(attente_temoin("entree,fonctions-exposition:nom_disparu,#15,en attente"))),
                "absent de la source de fonctions-exposition"))
verifier("concordance : entrée en attente hors de la forme <catalogue>:<nom> -> échec",
         echoue(concordance("--strict", "--latex", shQuote(complet), "--attente",
                            shQuote(attente_temoin(sprintf("entree,%s,#15,en attente", entree_attendue)))),
                "hors de la forme"))
verifier("concordance : entrée en attente d'un catalogue lui-même en attente -> échec (ligne redondante)",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente",
                            shQuote(attente_temoin(c(sprintf("export,%s,#11,en attente", EXPORTS), lignes_catalogues,
                                                     sprintf("entree,fonctions-exposition:%s,#15,en attente",
                                                             entree_attendue))))),
                "ligne redondante"))
# Entrées que le motif ne lit pas : signalées, jamais ignorées (constat M1).
for (cas in list(
  list(nom = "catalogue en majuscules", lignes = "\\catalogue{Roles}{x}"),
  list(nom = "espace avant l'accolade", lignes = "\\catalogue {roles}{x}"),
  list(nom = "accolade dans le nom", lignes = "\\catalogue{roles}{\\textit{x}}"),
  list(nom = "entrée coupée sur deux lignes", lignes = c("\\catalogue{roles}", "{x}")),
  list(nom = "deuxième entrée illisible sur une ligne lue",
       lignes = "\\catalogue{roles}{x} & \\catalogue{Roles}{y}")
)) {
  d <- latex_temoin(c(CATALOGUES_COMPLETS, cas$lignes))
  verifier(sprintf("concordance : entrée de catalogue illisible (%s) -> échec", cas$nom),
           echoue(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide)),
                  "entrée de catalogue illisible"))
  unlink(d, recursive = TRUE)
}
# Retrait des commentaires LaTeX : `%` précédé d'un nombre pair de barres
# obliques inverses, hors \verb et premier argument de \url et \href. Une
# citation placée après un `%` qui n'ouvre pas de commentaire est lue (et
# échoue ici, la fonction n'existant pas) ; après un vrai commentaire, elle
# ne l'est pas.
for (cas in list(
  list(nom = "après \\url{...%...}", ligne = "\\url{https://exemple.org/a%20b} \\code{inconnue\\_url()}",
       motif = "inconnue_url() existe"),
  list(nom = "après \\href{...%...}{...}", ligne = "\\href{https://exemple.org/%41}{lien} \\code{inconnue\\_href()}",
       motif = "inconnue_href() existe"),
  list(nom = "après \\verb|...%...|", ligne = "\\verb|50%| \\code{inconnue\\_verb()}",
       motif = "inconnue_verb() existe"),
  list(nom = "après \\%", ligne = "50\\% \\code{inconnue\\_pct()}", motif = "inconnue_pct() existe"),
  list(nom = "après \\\\\\% (trois barres)", ligne = "a\\\\\\% \\code{inconnue\\_trois()}",
       motif = "inconnue_trois() existe")
)) {
  d <- latex_temoin(c(CATALOGUES_COMPLETS, cas$ligne))
  verifier(sprintf("concordance : %% qui n'ouvre pas de commentaire (%s) -> citation lue, échec", cas$nom),
           echoue(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide)), cas$motif))
  unlink(d, recursive = TRUE)
}
d <- latex_temoin(c(CATALOGUES_COMPLETS, "fin de ligne\\\\% \\code{inconnue\\_commentee()}",
                    "\\url{https://exemple.org/a%20b} texte % \\catalogue{bidule}{commentee}",
                    "\\verb|%| texte % \\code{inconnue\\_apres\\_verb()}"))
verifier("concordance : citations après un vrai commentaire (\\\\%, % après \\url et \\verb) non lues -> code 0",
         passe(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide))))
unlink(d, recursive = TRUE)
verifier("concordance : catalogue requis absent, hors attente -> échec",
         echoue(concordance("--strict", "--latex", shQuote(sans_roles), "--attente", shQuote(attente_vide)),
                "catalogue requis sans entrée"))
verifier("concordance : catalogues présents et encore en attente -> succès inattendu, échec",
         echoue(concordance("--strict", "--latex", shQuote(complet), "--attente", shQuote(attente_cat)),
                "succès inattendu"))
verifier("concordance : ligne d'attente d'un catalogue inconnu -> échec",
         echoue(concordance("--strict", "--latex", shQuote(vide), "--attente", shQuote(attente_cat_inconnu)),
                "pas un catalogue requis"))
verifier("concordance : ligne d'attente de type inconnu -> échec",
         echoue(concordance("--strict", "--latex", shQuote(cite), "--attente", shQuote(attente_type)),
                "type « fonction »"))
verifier("concordance : ligne d'attente en double -> échec",
         echoue(concordance("--strict", "--latex", shQuote(cite), "--attente", shQuote(attente_doublon)),
                "ligne en double"))
verifier("concordance : liste d'attente à l'ancien format (nom, issue, motif) -> échec",
         echoue(concordance("--strict", "--latex", shQuote(complet), "--attente", shQuote(attente_ancienne)),
                "colonnes lues : nom, issue, motif"))
unlink(c(vide, cite, inconnu, inconnu_args, cite_args, inconnu_2l, inconnu_3l, cite_2l, base_2l, complet, incomplet, en_trop, doublon,
         cat_inconnu, sans_roles, forme), recursive = TRUE)
unlink(c(attente_ok, attente_cat, attente_vide, attente_perimee, attente_sans_issue,
         attente_cat_inconnu, attente_type, attente_doublon, attente_ancienne))

# --- concordance : constantes de version et fiche de construction (#70) ------
#
# Témoins construits sur les valeurs du package (constantes, ORDRE_CHAMPS_FICHE
# et BLOCS_LECTURE_FICHE ; leur égalité à names(fiche) est contrôlée par
# test_serialisation.R), pour rester valables quand la fiche évolue : la
# concordance est lancée sur un document minimal qui reprend les formes lues
# (liste d'items à partir de \textbf{identification}, paragraphe
# \emph{<bloc>}~:, citations des constantes, ligne de catalogue), dans l'état
# juste, puis avec des écarts ; les écarts de constantes, de noms fantômes et
# de blocs, qui se signalent par des messages distincts, sont groupés par
# témoin (un lancement du script coûte quelques secondes).
attente_vide <- attente_temoin(character(0))  # la précédente est supprimée plus haut
NS_TABLEXP <- asNamespace("tablexp")
V_FICHE <- NS_TABLEXP[["VERSION_FICHE"]]
CHAMPS_FICHE <- NS_TABLEXP[["ORDRE_CHAMPS_FICHE"]]
BLOCS_FICHE <- NS_TABLEXP[["BLOCS_LECTURE_FICHE"]]
numero <- function(nom) as.integer(sub("^.*[^0-9]", "", NS_TABLEXP[[nom]]))
code_champs <- function(x, sep) paste(sprintf("\\code{%s}", echapper(x)), collapse = sep)
# Lignes du document minimal ; chaque argument remplace la valeur juste.
# `ajout` : texte ajouté à la fin de la liste des champs ; `phrase` : ligne
# ajoutée après la liste.
fiche_temoin <- function(version = V_FICHE, version_catalogue = V_FICHE, champs = CHAMPS_FICHE,
                         blocs = BLOCS_FICHE, entete = numero("ENTETE_CANONIQUE"),
                         resultat = numero("REPRESENTATION_RESULTAT"),
                         plan = numero("PREMIERE_LIGNE_PLAN"), fiche = numero("PREMIERE_LIGNE_FICHE"),
                         ajout = "", phrase = character(0)) {
  c(sub("\\catalogue{fonctions-rapport}{VERSION\\_FICHE}",
        sprintf("\\catalogue{fonctions-rapport}{VERSION\\_FICHE} & constante & --- & version de la fiche (%s) \\\\", version_catalogue),
        CATALOGUES_COMPLETS, fixed = TRUE),
    "\\begin{itemize}",
    sprintf("  \\item \\textbf{identification}~: %s (\\code{VERSION\\_FICHE}, %s)~; %s%s", code_champs(champs[1L], ""),
            version, code_champs(champs[-1L], "~; "), ajout),
    "\\end{itemize}",
    phrase,
    sprintf("(\\code{ENTETE\\_CANONIQUE}, \\code{\"tablexp canonique %s\"})", entete),
    sprintf("\\code{tablexp resultat %s} (\\code{REPRESENTATION\\_RESULTAT})", resultat),
    sprintf("\\code{tablexp plan %s} (\\code{PREMIERE\\_LIGNE\\_PLAN}) et \\code{tablexp fiche %s}", plan, fiche),
    paste0(paste(sprintf("\\emph{%s}~: %s", names(blocs), vapply(blocs, code_champs, "", sep = ", ")), collapse = "~; "), "."))
}
# Vrai si le code de sortie est non nul et que la sortie contient chaque motif.
echoue_tous <- function(r, motifs) {
  v <- vapply(motifs, function(m) isTRUE(echoue(r, m)), logical(1L))
  if (all(v)) TRUE else paste("motifs non signalés :", paste(motifs[!v], collapse = " | "), "| sortie :",
                              paste(r$sortie, collapse = " | "))
}
# Témoins à écarts, chacun avec les motifs que la sortie doit tous contenir.
ecarts_fiche <- list(
  list("six écarts de constantes (VERSION_FICHE dans la liste et dans le catalogue, quatre en-têtes)",
       fiche_temoin(version = V_FICHE - 1L, version_catalogue = V_FICHE + 1L, entete = 9L, resultat = 9L, plan = 9L, fiche = 9L),
       c(sprintf("VERSION_FICHE citée avec sa valeur (%d)", V_FICHE),
         sprintf("le document cite %d, %d, le package %d", V_FICHE - 1L, V_FICHE + 1L, V_FICHE),
         "ENTETE_CANONIQUE citée avec sa valeur", "REPRESENTATION_RESULTAT citée avec sa valeur",
         "PREMIERE_LIGNE_PLAN citée avec sa valeur", "PREMIERE_LIGNE_FICHE citée avec sa valeur")),
  list("VERSION_FICHE citée en prose (« La constante \\code{VERSION\\_FICHE} vaut N. »)",
       fiche_temoin(phrase = sprintf("La constante \\code{VERSION\\_FICHE} vaut %d.", V_FICHE - 1L)),
       sprintf("le document cite %d, le package %d", V_FICHE - 1L, V_FICHE)),
  list("champ de la fiche absent du document (nombre_lignes_exclues)",
       fiche_temoin(champs = setdiff(CHAMPS_FICHE, "nombre_lignes_exclues")),
       "champs de la fiche absents du document : nombre_lignes_exclues"),
  list("ordre des champs", fiche_temoin(champs = CHAMPS_FICHE[c(2L, 1L, seq_along(CHAMPS_FICHE)[-(1:2)])]),
       "ordre du document"),
  list("trois noms fantômes (en tête de clause, après « et », après une virgule)",
       fiche_temoin(ajout = " \\code{versions} et \\code{fantome\\_et}, \\code{fantome\\_virgule}~; \\code{fantome\\_tete}"),
       "absents de la fiche, du package et de la liste blanche : fantome_et, fantome_virgule, fantome_tete"),
  list("fonction citée avec () inconnue dans la liste des champs",
       fiche_temoin(ajout = " \\code{fonction\\_fantome()}"),
       "absents de la fiche, du package et de la liste blanche : fonction_fantome"),
  list("trois écarts de blocs de lecture (champ absent, ordre d'un bloc, bloc en plus)",
       fiche_temoin(blocs = c(Map(function(champs, b) if (b == "hypothèses") setdiff(champs, "nombre_lignes_exclues")
                                                      else if (b == "calculé") rev(champs) else champs,
                                  BLOCS_FICHE, names(BLOCS_FICHE)), list(en_plus = "plan"))),
       c("bloc hypothèses : document", "bloc calculé : document", "bloc(s) dans le document")),
  list("ordre des blocs de lecture (hypothèses avant calculé)", fiche_temoin(blocs = BLOCS_FICHE[c(2L, 1L, 3L)]),
       "ordre des blocs du document : hypothèses, calculé, rejeu ; ordre du package : calculé, hypothèses, rejeu")
)
verifier("concordance fiche : témoin exact -> code 0 (constantes, champs, blocs)",
         { d <- latex_temoin(fiche_temoin()); on.exit(unlink(d, recursive = TRUE))
           passe(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide))) })
for (cas in ecarts_fiche) {
  d <- latex_temoin(cas[[2L]])
  verifier(sprintf("concordance fiche : %s -> échec", cas[[1L]]),
           echoue_tous(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide)), cas[[3L]]))
  unlink(d, recursive = TRUE)
}
# Liste d'attente (type fiche) : écart déclaré -> échec attendu ; document juste
# et ligne encore là -> succès inattendu ; nom hors de la liste -> échec.
d_ecart <- latex_temoin(fiche_temoin(version = V_FICHE - 1L)); d_juste <- latex_temoin(fiche_temoin())
attente_fiche_v <- attente_temoin("fiche,VERSION_FICHE,#70,document en retard")
verifier("concordance fiche : écart déclaré en attente (type fiche) -> code 0",
         passe(concordance("--strict", "--latex", shQuote(d_ecart), "--attente", shQuote(attente_fiche_v))))
verifier("concordance fiche : document juste et ligne encore en attente -> succès inattendu, échec",
         echoue(concordance("--strict", "--latex", shQuote(d_juste), "--attente", shQuote(attente_fiche_v)),
                "succès inattendu"))
verifier("concordance fiche : ligne d'attente de type fiche à nom inconnu -> échec",
         echoue(concordance("--strict", "--latex", shQuote(d_juste), "--attente",
                            shQuote(attente_temoin("fiche,VERSION_INCONNUE,#70,x"))), "hors de : VERSION_FICHE"))
unlink(c(d_ecart, d_juste, attente_fiche_v, attente_vide), recursive = TRUE)

# --- concordance : aucun nom coupé par \allowbreak entre deux \code{} (#84) --
#
# Contrôle 6 : deux motifs interdits, \code{}\allowbreak\code{...( et
# \code{...(...}\allowbreak (parenthèse ouverte non fermée). Témoins sur le
# document minimal du bloc #70 (état juste) auquel on ajoute des lignes : les
# formes interdites sont groupées (même ligne, coupure de ligne après
# \allowbreak, espace après \allowbreak, \allowbreak{}, nom coupé dans ses
# arguments, `%` de fin de ligne), de même que les emplois légitimes de
# \allowbreak, qui doivent passer (dont un \allowbreak séparateur entre deux
# citations).
attente_vide <- attente_temoin(character(0))
coupes_interdites <- c(
  "Nom coupé~: \\code{kaplan\\_meier\\_}\\allowbreak\\code{ensemble()}.",
  "Coupure de ligne~: \\code{tests/}\\allowbreak",
  "\\code{fichier(x, y)}.",
  "Espace~: \\code{tablexp::}\\allowbreak \\code{construire\\_table()}.",
  "Accolades~: \\code{lire\\_}\\allowbreak{}\\code{plan()}.",
  "Arguments~: \\code{construire\\_table(}\\allowbreak\\code{donnees)}.",
  "Pour-cent~: \\code{lire\\_}%",
  "\\allowbreak\\code{plan()}.")
allowbreak_licites <- c(
  # chemins et noms sans parenthèse (usage du document), parenthèse avant ou
  # après le \code{}, \allowbreak séparateur (virgule ou texte avant, {} suivi
  # d'un blanc)
  "Chemin~: \\code{etapes/}\\allowbreak\\code{donnees/}\\allowbreak\\code{exclusions} (\\code{c(MOTIFS\\_LIGNES,} \\code{MOTIFS\\_TETE)}).",
  "Voir (\\code{docs/}\\allowbreak\\code{feuille-de-route.md}, §~2) et \\code{construire\\_table()}.",
  "Nom~: \\code{kaplan\\_meier\\_}\\allowbreak\\code{ensemble\\_a\\_risque\\_vide}~; \\code{set.seed()}.",
  "Séparateurs~: \\code{construire\\_table()}, \\allowbreak \\code{set.seed()}~; \\code{stats::sd()}~: \\allowbreak \\code{tools::md5sum()}",
  "et \\code{construire\\_table()}\\allowbreak{} \\code{set.seed()}.",
  "% \\code{commentaire\\_}\\allowbreak\\code{retire()} : ligne commentée")
verifier("concordance \\allowbreak : témoin exact (usages licites de \\allowbreak) -> code 0",
         { d <- latex_temoin(fiche_temoin(phrase = allowbreak_licites)); on.exit(unlink(d, recursive = TRUE))
           passe(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide))) })
verifier("concordance \\allowbreak : six noms coupés (même ligne, fin de ligne, espace, {}, arguments, après `%`) -> échec",
         { d <- latex_temoin(fiche_temoin(phrase = coupes_interdites)); on.exit(unlink(d, recursive = TRUE))
           echoue_tous(concordance("--strict", "--latex", shQuote(d), "--attente", shQuote(attente_vide)),
                       c("aucun nom de fonction coupé par \\allowbreak entre deux \\code{}",
                         "6 occurrence(s)", "« \\code{kaplan\\_meier\\_}\\allowbreak\\code{ensemble( »",
                         "« \\code{tests/}\\allowbreak \\code{fichier( »",
                         "« \\code{tablexp::}\\allowbreak \\code{construire\\_table( »",
                         "« \\code{lire\\_}\\allowbreak{}\\code{plan( »",
                         "« \\code{construire\\_table(}\\allowbreak »",
                         "« \\code{lire\\_} \\allowbreak\\code{plan( »")) })
unlink(attente_vide)
