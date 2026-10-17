# -----------------------------------------------------------------------------
#  tests/concordance_doc_moteur.R : concordance documentation <-> moteur
#  (E3.3, E4.9 ; ADR 0007, points 3.4 et 11 ; issue #8 ;
#  docs/latex/CONVENTIONS.md, section 3)
#
#  Lit les fichiers .tex de docs/latex/ (récursivement), commentaires LaTeX
#  retirés, et confronte aux exports et au code de tablexp, installé depuis
#  l'arbre de travail :
#    1. toute fonction citée existe : forme \code{nom()} ou
#       \code{nom(arguments)} (souligné échappé \_ admis), ou entrée de
#       catalogue dont le nom porte `()` ; `nom` est une fonction de l'espace
#       de noms de tablexp (exportée ou interne), une fonction d'un package
#       de priorité base (base, stats, utils, tools...), ou, pour la forme
#       \code{pkg::nom()}, une fonction exportée de `pkg` ; un outil des
#       batteries (fonction de premier niveau de tests/harnais.R) est admis ;
#    2. toute fonction exportée est citée (même formes) ; un export pas encore
#       documenté figure dans la liste d'attente tests/concordance_attente.csv
#       (type `export`), où il est compté en échec attendu avec renvoi à
#       l'issue ; s'il est cité, c'est un succès inattendu : la ligne est à
#       retirer de la liste ;
#    3. toute ligne de la liste d'attente désigne un export existant (type
#       `export`) ou un catalogue requis (type `catalogue`) et porte un
#       renvoi d'issue « #N » ; aucune ligne en double ;
#    4. entrées \catalogue{<catalogue>}{<nom>} (CONVENTIONS.md, section 3.2 ;
#       `\_` lu `_`, un `()` final retiré) :
#       - une occurrence de \catalogue que le motif ne lit pas (ligne où
#         les deux décomptes diffèrent) est un écart : entrée illisible ;
#       - un catalogue lu hors des catalogues requis est un écart ;
#       - pour chaque catalogue présent, l'ensemble des noms lus est égal à
#         l'ensemble de sa source, sans doublon ; dans `exports` et
#         `fonctions-<fichier>`, un nom de la source porte `()` si et
#         seulement s'il désigne une fonction ; dans `roles` et `recettes`,
#         aucun nom ne porte `()` ;
#       - un catalogue requis sans aucune entrée est un échec attendu s'il
#         figure dans la liste d'attente (type `catalogue`), un échec sinon ;
#         présent et encore en attente, c'est un succès inattendu.
#       Catalogues requis et sources :
#         exports              getNamespaceExports("tablexp") ;
#         fonctions-<fichier>  définitions de premier niveau de R/<fichier>.R
#                              (souligné du nom de fichier lu comme un tiret :
#                              fonctions-construire-table), lues par parse()
#                              par analyser_modules() de
#                              tests/controles/graphe_modules.R ;
#         roles                catalogue_roles()$role ;
#         recettes             RECETTE_BRANCHE (R/donnees.R), source unique ;
#                              les clés de CONVENTION_NATIVE (R/exposition.R)
#                              lui sont égales (contrôle 4).
#
#  docs/latex/ doit exister (LATEX_EXIGE) ; un dossier donné par --latex doit
#  exister ; une option sans valeur est un échec.
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
# Le squelette LaTeX existe (#8, point 7) : l'absence du dossier fait échouer
# la batterie.
LATEX_EXIGE <- TRUE
COLONNES_ATTENTE <- c("type", "nom", "issue", "motif")
TYPES_ATTENTE <- c("export", "catalogue")

cat(R.version.string, "\n")
installer_tablexp(RACINE)

# Lignes des .tex du dossier, commentaires LaTeX retirés.
texte_latex <- function(dossier) {
  tex <- if (dir.exists(dossier))
    list.files(dossier, pattern = "\\.tex$", recursive = TRUE, full.names = TRUE) else character(0)
  if (!length(tex)) return(character(0))
  texte <- unlist(lapply(tex, readLines, warn = FALSE, encoding = "UTF-8"))
  sub("(^|[^\\\\])%.*$", "\\1", texte)  # commentaires LaTeX
}

# Citations \code{nom()} ou \code{pkg::nom()}.
citations_latex <- function(texte) {
  # \code{nom()} ou \code{nom(arguments)}, souligné échappé \_ admis ; les
  # arguments ne contiennent pas d'accolade.
  m <- unlist(regmatches(texte, gregexpr("\\\\code\\{[A-Za-z0-9._\\\\:]+\\([^{}]*\\)\\}", texte)))
  m <- gsub("\\\\_", "_", sub("\\(.*$", "", sub("^\\\\code\\{", "", m)))
  sort(unique(m))
}

# Entrées \catalogue{<catalogue>}{<nom>}, dans l'ordre du texte :
# data.frame(catalogue, nom, fonction), `fonction` vaut TRUE si le nom
# portait un `()` final (retiré de `nom`). Attribut `illisibles` : lignes où
# le nombre d'occurrences de la macro (\catalogue non suivi d'une lettre)
# diffère du nombre d'entrées lues par le motif (catalogue en majuscules,
# espace avant l'accolade, accolade dans le nom, entrée coupée sur deux
# lignes...) ; la ligne de définition \newcommand{\catalogue} est exclue.
MOTIF_CATALOGUE <- "\\\\catalogue\\{([a-z0-9-]+)\\}\\{([^{}]+)\\}"
entrees_catalogue <- function(texte) {
  motif <- MOTIF_CATALOGUE
  m <- unlist(regmatches(texte, gregexpr(motif, texte)))
  catalogue <- sub(motif, "\\1", m)
  nom <- gsub("\\\\_", "_", sub(motif, "\\2", m))
  fonction <- grepl("\\(\\)$", nom)
  d <- data.frame(catalogue = catalogue, nom = sub("\\(\\)$", "", nom), fonction = fonction,
                  stringsAsFactors = FALSE)
  compte <- function(re, perl = FALSE)
    vapply(gregexpr(re, texte, perl = perl), function(g) sum(g > 0L), integer(1L))
  definition <- grepl("\\\\(re|provide)?newcommand\\*?\\{?\\\\catalogue[}\\[]", texte)
  ecart <- !definition & compte("\\\\catalogue(?![A-Za-z])", perl = TRUE) != compte(motif)
  attr(d, "illisibles") <- trimws(texte[ecart])
  d
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

# Sources des catalogues requis (CONVENTIONS.md, section 3.2) : liste nommée
# catalogue -> noms. Les définitions de premier niveau sont lues par le même
# code que le contrôle de l'ordre des modules.
NS <- asNamespace("tablexp")
SOURCES_CATALOGUES <- local({
  g <- new.env()
  sys.source(file.path(RACINE, "tests", "controles", "graphe_modules.R"), envir = g)
  dossier_r <- file.path(RACINE, "R")
  defs <- g$analyser_modules(dossier_r)$symboles
  fichiers <- sort(sub("\\.[Rr]$", "", list.files(dossier_r, pattern = "\\.[Rr]$")))
  fonctions <- lapply(fichiers, function(m) if (is.null(defs)) character(0)
                                            else defs$symbole[defs$module == m])
  names(fonctions) <- paste0("fonctions-", gsub("_", "-", fichiers, fixed = TRUE))
  c(list(exports = getNamespaceExports("tablexp")),
    fonctions,
    list(roles = tablexp::catalogue_roles()$role,
         recettes = NS[["RECETTE_BRANCHE"]]))
})
# Catalogues où `()` marque une fonction ; ailleurs, aucun nom n'en porte.
avec_fonctions <- function(catalogue) catalogue == "exports" || startsWith(catalogue, "fonctions-")

texte <- texte_latex(DOSSIER_LATEX)
entrees <- entrees_catalogue(texte)
cites <- sort(unique(c(citations_latex(texte), entrees$nom[entrees$fonction])))
exports <- sort(getNamespaceExports("tablexp"))
attente <- if (file.exists(FICHIER_ATTENTE))
  utils::read.csv(FICHIER_ATTENTE, colClasses = "character",
                  strip.white = TRUE) else
  as.data.frame(stats::setNames(rep(list(character(0)), length(COLONNES_ATTENTE)), COLONNES_ATTENTE))
colonnes_lues <- names(attente)
colonnes_ok <- identical(colonnes_lues, COLONNES_ATTENTE)
if (!colonnes_ok)  # contrôle 2 en échec ; colonnes complétées pour la suite
  for (k in setdiff(COLONNES_ATTENTE, names(attente))) attente[[k]] <- rep(NA_character_, nrow(attente))
attente_exports <- attente[attente$type %in% "export", , drop = FALSE]
attente_catalogues <- attente[attente$type %in% "catalogue", , drop = FALSE]

cat(sprintf("Documentation : %s (%d fonction(s) citée(s), %d entrée(s) de catalogue)\n",
            DOSSIER_LATEX, length(cites), nrow(entrees)))
cat(sprintf("Exports de tablexp : %s\n", paste(exports, collapse = ", ")))
cat(sprintf("Catalogues requis : %s\n", paste(names(SOURCES_CATALOGUES), collapse = ", ")))
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
verifier("liste d'attente : colonnes type, nom, issue, motif",
         if (colonnes_ok) TRUE
         else paste("colonnes lues :", paste(colonnes_lues, collapse = ", ")))
for (n in exports) {
  ligne <- attente_exports[attente_exports$nom == n, , drop = FALSE]
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
  etiquette <- sprintf("attente %s %s", attente$type[i], attente$nom[i])
  verifier(sprintf("%s : type export ou catalogue", etiquette),
           if (attente$type[i] %in% TYPES_ATTENTE) TRUE else sprintf("type « %s »", attente$type[i]))
  if (identical(attente$type[i], "export"))
    verifier(sprintf("%s : export existant", etiquette),
             if (attente$nom[i] %in% exports) TRUE else "pas un export de tablexp : ligne à retirer")
  if (identical(attente$type[i], "catalogue"))
    verifier(sprintf("%s : catalogue requis", etiquette),
             if (attente$nom[i] %in% names(SOURCES_CATALOGUES)) TRUE
             else "pas un catalogue requis : ligne à retirer")
  verifier(sprintf("%s : renvoi d'issue \"#N\"", etiquette),
           if (grepl(MOTIF_ISSUE, attente$issue[i])) TRUE else sprintf("issue « %s »", attente$issue[i]))
}
verifier("attente : aucune ligne en double (type, nom)",
         if (anyDuplicated(attente[, c("type", "nom")])) "ligne en double" else TRUE)

fichier_courant("4. catalogues de docs/latex/")
verifier("entrées de catalogue toutes lisibles (forme \\catalogue{<catalogue>}{<nom>} sur une ligne)", {
  il <- attr(entrees, "illisibles")
  if (length(il)) paste("entrée de catalogue illisible :", paste(sprintf("« %s »", il), collapse = " ; "))
  else TRUE
})
inconnus <- setdiff(unique(entrees$catalogue), names(SOURCES_CATALOGUES))
for (k in inconnus)
  verifier(sprintf("catalogue %s : catalogue requis", k),
           sprintf("catalogue inconnu (%d entrée(s)) ; requis : %s", sum(entrees$catalogue == k),
                   paste(names(SOURCES_CATALOGUES), collapse = ", ")))
verifier("recettes : clés de CONVENTION_NATIVE égales à RECETTE_BRANCHE (source unique)", {
  cles <- names(NS[["CONVENTION_NATIVE"]])
  if (setequal(cles, SOURCES_CATALOGUES$recettes)) TRUE
  else sprintf("CONVENTION_NATIVE : %s ; RECETTE_BRANCHE : %s", paste(cles, collapse = ", "),
               paste(SOURCES_CATALOGUES$recettes, collapse = ", "))
})
for (k in names(SOURCES_CATALOGUES)) {
  lus <- entrees[entrees$catalogue == k, , drop = FALSE]
  source_k <- SOURCES_CATALOGUES[[k]]
  ligne <- attente_catalogues[attente_catalogues$nom == k, , drop = FALSE]
  if (nrow(ligne) == 1L && grepl(MOTIF_ISSUE, ligne$issue)) {
    echec_attendu(sprintf("catalogue %s présent dans docs/latex/", k), ligne$issue,
                  if (nrow(lus)) TRUE else sprintf("aucune entrée (%s)", ligne$motif))
  } else {
    verifier(sprintf("catalogue %s présent dans docs/latex/", k),
             if (nrow(lus)) TRUE
             else "catalogue requis sans entrée et absent de la liste d'attente (tests/concordance_attente.csv)")
  }
  if (!nrow(lus)) next
  verifier(sprintf("catalogue %s : noms égaux à la source (%d)", k, length(unique(source_k))), {
    manquants <- setdiff(source_k, lus$nom); en_trop <- setdiff(lus$nom, source_k)
    if (!length(manquants) && !length(en_trop)) TRUE
    else paste(c(if (length(manquants)) paste("absents du document :", paste(sort(manquants), collapse = ", ")),
                 if (length(en_trop)) paste("absents de la source :", paste(sort(en_trop), collapse = ", "))),
               collapse = " ; ")
  })
  verifier(sprintf("catalogue %s : aucun nom en double", k), {
    doubles <- unique(lus$nom[duplicated(lus$nom)])
    if (length(doubles)) paste("en double :", paste(doubles, collapse = ", ")) else TRUE
  })
  verifier(sprintf("catalogue %s : `()` si et seulement si fonction", k), {
    dans_source <- lus[lus$nom %in% source_k, , drop = FALSE]
    attendu <- if (avec_fonctions(k))
      vapply(dans_source$nom, function(n) exists(n, envir = NS, inherits = FALSE) && is.function(NS[[n]]),
             logical(1L))
      else rep(FALSE, nrow(dans_source))
    ko <- dans_source$fonction != attendu
    if (any(ko))
      paste("forme fausse :", paste(ifelse(dans_source$fonction[ko],
                                           sprintf("%s() (pas une fonction)", dans_source$nom[ko]),
                                           sprintf("%s (fonction sans ())", dans_source$nom[ko])),
                                    collapse = ", "))
    else TRUE
  })
}

if (!STRICT) cat("\nMode rapport : code de sortie 0 même en cas d'écart (--strict pour échouer).\n")
bilan(code_sortie = STRICT)
