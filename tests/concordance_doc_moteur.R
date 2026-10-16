# -----------------------------------------------------------------------------
#  tests/concordance_doc_moteur.R : concordance documentation <-> moteur
#  (E3.3 ; ADR 0007, points 3.4 et 11 ; issue #8)
#
#  Lit les fichiers .tex de docs/latex/ (récursivement) et confronte aux
#  exports et au code de tablexp, installé depuis l'arbre de travail :
#    1. toute fonction citée existe : forme \code{nom()} ou
#       \code{nom(arguments)} (souligné échappé \_ admis) ; `nom` est une fonction de l'espace de noms de tablexp
#       (exportée ou interne), une fonction d'un package de priorité base
#       (base, stats, utils, tools...), ou, pour la forme \code{pkg::nom()},
#       une fonction exportée de `pkg` ; un outil des batteries (fonction de
#       premier niveau de tests/harnais.R) est admis ;
#    2. toute fonction exportée est citée ; un export pas encore documenté
#       figure dans la liste d'attente tests/concordance_attente.csv (nom,
#       issue, motif), où il est compté en échec attendu avec renvoi à
#       l'issue ; s'il est cité, c'est un succès inattendu : la ligne est à
#       retirer de la liste ;
#    3. toute ligne de la liste d'attente désigne un export existant et
#       porte un renvoi d'issue « #N ».
#  Les noms de rôles et de recettes s'ajouteront à ces contrôles avec #9 et
#  #11 (catalogue des rôles, recettes d'exposition).
#
#  docs/latex/ absent ou sans .tex : aucune citation lue (le squelette LaTeX
#  vient avec docwriter, #8) ; les contrôles 2 et 3 s'appliquent. Constante
#  LATEX_EXIGE à passer à TRUE quand le squelette existe. Un dossier donné
#  par --latex doit exister ; une option sans valeur est un échec.
#
#  Convention de citation lue : \code{nom()} (charte d'outil_USP), à tenir
#  identique à docs/latex/CONVENTIONS.md quand il existera.
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/concordance_doc_moteur.R --strict   # code 1 si écart
#      Rscript tests/concordance_doc_moteur.R            # rapport, code 0
#  Options pour les tests du script : --latex <dossier>, --attente <fichier>.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(normalizePath(f[1L])), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

ARGS <- commandArgs(trailingOnly = TRUE)
# Valeur de l'option `nom`, `defaut` si l'option est absente ; une option
# présente sans valeur est une erreur d'appel (code 1).
option <- function(nom, defaut) {
  i <- match(nom, ARGS)
  if (is.na(i)) return(defaut)
  if (i == length(ARGS) || startsWith(ARGS[i + 1L], "--")) {
    cat(sprintf("ECHEC : option %s sans valeur\n", nom))
    quit(save = "no", status = 1L, runLast = FALSE)  # sortie délibérée : sans la garde .Last
  }
  ARGS[i + 1L]
}
STRICT <- "--strict" %in% ARGS
RACINE <- racine_depot()
DOSSIER_LATEX <- option("--latex", file.path(RACINE, "docs", "latex"))
FICHIER_ATTENTE <- option("--attente", file.path(RACINE, "tests", "concordance_attente.csv"))
# docs/latex/ absent : admis tant que docwriter n'a pas créé le squelette
# LaTeX (#8, point 7). Passer à TRUE dans le commit qui le crée : l'absence
# du dossier fera alors échouer la batterie.
LATEX_EXIGE <- FALSE

cat(R.version.string, "\n")
installer_tablexp(RACINE)

# Citations \code{nom()} ou \code{pkg::nom()} des .tex du dossier.
citations_latex <- function(dossier) {
  tex <- if (dir.exists(dossier))
    list.files(dossier, pattern = "\\.tex$", recursive = TRUE, full.names = TRUE) else character(0)
  if (!length(tex)) return(character(0))
  texte <- unlist(lapply(tex, readLines, warn = FALSE, encoding = "UTF-8"))
  texte <- sub("(^|[^\\\\])%.*$", "\\1", texte)  # commentaires LaTeX
  # \code{nom()} ou \code{nom(arguments)}, souligné échappé \_ admis ; les
  # arguments ne contiennent pas d'accolade.
  m <- unlist(regmatches(texte, gregexpr("\\\\code\\{[A-Za-z0-9._\\\\:]+\\([^{}]*\\)\\}", texte)))
  m <- gsub("\\\\_", "_", sub("\\(.*$", "", sub("^\\\\code\\{", "", m)))
  sort(unique(m))
}

# Packages de priorité base où chercher une fonction citée ; tcltk exclu :
# charger son espace de noms demande Tcl/Tk, absent d'une image sans
# affichage, et le moteur n'a rien à en citer.
PACKAGES_BASE <- setdiff(rownames(utils::installed.packages(lib.loc = .Library, priority = "base")),
                         "tcltk")
OUTILS_BATTERIES <- local({
  e <- parse(file.path(RACINE, "tests", "harnais.R"), keep.source = FALSE, encoding = "UTF-8")
  noms <- vapply(e, function(x) if (is.call(x) && as.character(x[[1L]]) %in% c("<-", "=") &&
                                    is.symbol(x[[2L]]) && is.call(x[[3L]]) &&
                                    identical(x[[3L]][[1L]], as.name("function")))
                   as.character(x[[2L]]) else NA_character_, character(1L))
  noms[!is.na(noms)]
})

# TRUE si `nom` (éventuellement pkg::nom) désigne une fonction connue.
fonction_existe <- function(nom) {
  if (grepl("::", nom, fixed = TRUE)) {
    p <- sub("::.*$", "", nom); f <- sub("^.*::", "", nom)
    if (identical(p, "tablexp")) return(f %in% getNamespaceExports("tablexp"))
    if (!(p %in% PACKAGES_BASE)) return(sprintf("package %s hors des packages de priorité base cherchés", p))
    return(f %in% getNamespaceExports(p) && is.function(getExportedValue(p, f)))
  }
  ns <- asNamespace("tablexp")
  if (exists(nom, envir = ns, inherits = FALSE) && is.function(ns[[nom]])) return(TRUE)
  if (nom %in% OUTILS_BATTERIES) return(TRUE)
  for (p in PACKAGES_BASE) {
    if (nom %in% getNamespaceExports(p) && is.function(getExportedValue(p, nom))) return(TRUE)
  }
  FALSE
}

cites <- citations_latex(DOSSIER_LATEX)
exports <- sort(getNamespaceExports("tablexp"))
attente <- if (file.exists(FICHIER_ATTENTE))
  utils::read.csv(FICHIER_ATTENTE, colClasses = "character",
                  strip.white = TRUE) else
  data.frame(nom = character(0), issue = character(0), motif = character(0))

cat(sprintf("Documentation : %s (%d fonction(s) citée(s))\n", DOSSIER_LATEX, length(cites)))
cat(sprintf("Exports de tablexp : %s\n", paste(exports, collapse = ", ")))
cat(sprintf("Liste d'attente : %s (%d ligne(s))\n", FICHIER_ATTENTE, nrow(attente)))

fichier_courant("0. documentation lue")
if ("--latex" %in% ARGS || LATEX_EXIGE)
  verifier(sprintf("dossier de documentation existant : %s", DOSSIER_LATEX),
           if (dir.exists(DOSSIER_LATEX)) TRUE else "dossier introuvable")
if (!file.exists(FICHIER_ATTENTE) && "--attente" %in% ARGS)
  verifier(sprintf("liste d'attente existante : %s", FICHIER_ATTENTE), "fichier introuvable")

fichier_courant("1. fonctions citées dans docs/latex/")
if (!length(cites)) cat("  (aucune citation lue : docs/latex/ absent ou sans \\code{nom()})\n")
for (n in cites)
  verifier(sprintf("%s() existe", n), {
    r <- fonction_existe(n)
    if (isTRUE(r)) TRUE else if (is.character(r)) r else "fonction introuvable"
  })

fichier_courant("2. fonctions exportées citées dans docs/latex/")
verifier("liste d'attente : colonnes nom, issue, motif",
         if (identical(names(attente), c("nom", "issue", "motif"))) TRUE
         else paste("colonnes lues :", paste(names(attente), collapse = ", ")))
for (n in exports) {
  ligne <- attente[attente$nom == n, , drop = FALSE]
  if (nrow(ligne) == 1L && grepl(MOTIF_ISSUE, ligne$issue)) {
    echec_attendu(sprintf("%s() citée dans docs/latex/", n), ligne$issue,
                  if (n %in% cites) TRUE else sprintf("non citée (%s)", ligne$motif))
  } else {
    verifier(sprintf("%s() citée dans docs/latex/", n),
             if (n %in% cites) TRUE
             else "export non cité et absent de la liste d'attente (tests/concordance_attente.csv)")
  }
}

fichier_courant("3. liste d'attente")
for (i in seq_len(nrow(attente))) {
  verifier(sprintf("attente %s : export existant", attente$nom[i]),
           if (attente$nom[i] %in% exports) TRUE else "pas un export de tablexp : ligne à retirer")
  verifier(sprintf("attente %s : renvoi d'issue \"#N\"", attente$nom[i]),
           if (grepl(MOTIF_ISSUE, attente$issue[i])) TRUE else sprintf("issue « %s »", attente$issue[i]))
}
verifier("attente : aucun nom en double",
         if (anyDuplicated(attente$nom)) "nom en double" else TRUE)

if (!STRICT) cat("\nMode rapport : code de sortie 0 même en cas d'écart (--strict pour échouer).\n")
bilan(code_sortie = STRICT)
