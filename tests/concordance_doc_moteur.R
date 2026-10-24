# -----------------------------------------------------------------------------
#  tests/concordance_doc_moteur.R : concordance documentation <-> moteur
#  (E3.3, E4.9 ; ADR 0007, points 3.4 et 11 ; issue #8 ;
#  docs/latex/CONVENTIONS.md, section 3)
#
#  Lit les fichiers .tex de docs/latex/ (récursivement), commentaires LaTeX
#  retirés (retirer_commentaires()), et confronte aux exports et au code de tablexp, installé depuis
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
#       `export`), un catalogue requis (type `catalogue`) ou une entrée de
#       catalogue (type `entree`, nom de la forme `<catalogue>:<nom>`, nom
#       de la source d'un catalogue requis qui n'est pas lui-même en
#       attente) et porte un renvoi d'issue « #N » ; aucune ligne en double ;
#    4. entrées \catalogue{<catalogue>}{<nom>} (CONVENTIONS.md, section 3.2 ;
#       `\_` lu `_`, un `()` final retiré) :
#       - une occurrence de \catalogue que le motif ne lit pas (ligne où
#         les deux décomptes diffèrent) est un écart : entrée illisible ;
#       - un catalogue lu hors des catalogues requis est un écart ;
#       - pour chaque catalogue présent, l'ensemble des noms lus est égal à
#         l'ensemble de sa source, sans doublon, aux entrées en attente près
#         (type `entree`, #15 : définition ajoutée à un module dont le
#         catalogue est déjà écrit ; règle 9, docwriter en fin de branche) :
#         une entrée en attente absente du document est un échec attendu,
#         présente c'est un succès inattendu ; dans `exports` et
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
#    5. constantes de version et fiche de construction (#70) ; seule la
#       valeur est comparée, la rédaction du document reste libre :
#       - toute valeur citée des constantes de version est égale à sa valeur
#         dans le package, et chacune est citée au moins une fois :
#         VERSION_FICHE (tout entier qui suit \code{VERSION\_FICHE} à au plus
#         15 caractères non numériques près, ou entier « (N) » de la 4e colonne
#         d'une ligne de catalogue), ENTETE_CANONIQUE, REPRESENTATION_RESULTAT,
#         PREMIERE_LIGNE_PLAN et PREMIERE_LIGNE_FICHE (en-têtes « tablexp
#         <nature> N » cités dans le texte). Limite : toute mention d'une
#         autre valeur (« passe de 2 à 3 » à moins de 15 caractères de
#         \code{VERSION\_FICHE}, « tablexp canonique 1 » historique) est lue
#         comme une citation et échoue ; un entier plus loin ou sans
#         \code{VERSION\_FICHE} échappe ;
#       - liste des champs de la fiche du document (le bloc d'items qui
#         commence à \textbf{identification}) : ordre des champs de premier
#         niveau, lus par leur première occurrence \code{nom}, égal à celui de
#         names(fiche) rendue par construire_table() sur un plan avec étape
#         brut, la fiche sans étape brut en étant un sous-ensemble ; en outre,
#         chaque \code{} du bloc est classé dans l'une de ces sources, sinon
#         c'est un écart : (1) noms de la fiche à tous les niveaux (names()
#         récursif) de fiches témoins avec une provenance fournie et
#         l'estimateur kaplan_meier retenu ; (2) objets de l'espace de noms de
#         tablexp (constantes citées : VERSION_FICHE, CATALOGUE_ROLES,
#         GENERATEUR) ; (3) valeurs de ESTIMATEURS_BRUT (kaplan_meier) ;
#         (4) liste blanche minimale WHITELIST_FICHE (aucun catalogue source
#         pour unite_contrat, code d'avertissement littéral de R/donnees.R) ;
#         un nom suivi de () est une fonction, contrôlée comme au contrôle 1.
#         Limite : un nom de premier niveau retiré du package qui survit
#         comme sous-nom passerait (mesuré par audit : convention, par_annee,
#         exclusions sont aussi des sous-noms des fiches témoins) ; leur
#         absence du document reste, elle, contrôlée ;
#       - blocs de lecture du document (\emph{<bloc>}~: suivi des champs,
#         jusqu'au bloc suivant ou au point) : mêmes blocs, dans le même
#         ordre, avec les mêmes champs dans le même ordre que
#         BLOCS_LECTURE_FICHE (R/rapport.R).
#       Un écart transitoire se déclare dans la liste d'attente, type `fiche`,
#       nom d'une des constantes ci-dessus, `champs_fiche` ou `blocs_lecture` :
#       échec attendu, succès inattendu dès que le document concorde. Avec
#       --latex (témoins des tests du script), un contrôle dont l'ancre
#       (citation, liste, blocs) est absente du document est sauté ; sur
#       docs/latex/, une ancre absente est un écart.
#
#    6. aucun nom coupé par \allowbreak entre deux \code{} (#84, partie
#       mécanique ; la règle de rédaction relève de CONVENTIONS.md). Deux
#       motifs interdits, cherchés sur le texte joint (commentaires retirés) :
#       - \code{...}\allowbreak\code{...(} : un \allowbreak qui suit la fin d'un
#         \code{} (blancs admis entre les deux : un `%` de fin de ligne, retiré
#         avec le commentaire, les colle) et précède un \code{} contenant une
#         parenthèse ouvrante, directement, après des espaces ou fins de ligne
#         (ignorés par LaTeX après une macro) ou après {} (sans blanc après).
#         Le fragment \code{b()} de \code{a\_}\allowbreak\code{b()} serait lu
#         par le contrôle 1 comme la fonction b, et le nom entier échapperait
#         au contrôle. Un \allowbreak séparateur (\code{f()},
#         \allowbreak \code{g()}) n'est pas touché : une virgule ou du texte le
#         sépare du \code{} précédent. Limite : le même \allowbreak après de
#         simples blancs (\code{f()} \allowbreak \code{g()}) est lu comme une
#         coupure, alors qu'il est redondant avec le blanc ;
#       - \code{f(}\allowbreak\code{x)} : un \code{} à parenthèse ouverte non
#         fermée, suivi de \allowbreak (nom coupé dans ses arguments, que le
#         contrôle 1 ne lit pas non plus).
#       Aucune ancre n'est exigée (une interdiction est vérifiée à vide).
#       Hors du motif : \hspace{0pt}, \- et \linebreak[0], qui coupent aussi
#       sans que ce contrôle les voie.
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
TYPES_ATTENTE <- c("export", "catalogue", "entree", "fiche")
# Contrôles 5 : constantes de version (nom de la constante) et deux listes.
CONSTANTES_VERSION <- c("VERSION_FICHE", "ENTETE_CANONIQUE", "REPRESENTATION_RESULTAT",
                        "PREMIERE_LIGNE_PLAN", "PREMIERE_LIGNE_FICHE")
NOMS_ATTENTE_FICHE <- c(CONSTANTES_VERSION, "champs_fiche", "blocs_lecture")
# Noms cités dans la liste des champs de la fiche qui ne sont ni un nom de la
# fiche (à tous les niveaux), ni un objet de tablexp, ni une valeur de
# ESTIMATEURS_BRUT : les valeurs NA et NULL, le type integer, le code
# d'avertissement unite_contrat (littéral de R/donnees.R, aucun catalogue
# des codes), R.version.string (objet de base, versions$R) et le littéral 0L.
WHITELIST_FICHE <- c("NA", "NULL", "integer", "unite_contrat", "R.version.string", "0L")

cat(R.version.string, "\n")
installer_tablexp(RACINE)

# Chaînes en octets UTF-8 non marqués, convertis à la source (document lu, noms
# et valeurs tirés du package), une seule fois. Sous la locale C, une chaîne
# marquée UTF-8 se comporte autrement qu'un littéral de ce script (non marqué) :
# cat() l'écrit « <U+00E9> », sprintf() la mêle à un littéral accentué en
# « <c2><ab> », et %in%/match() ne l'égalent pas à la même chaîne non marquée.
# Tout le texte comparé ou affiché ici est donc non marqué : un motif accentué
# (« bloc hypothèses : document ») se retrouve dans la sortie du script lancé
# par les tests, et le verdict est celui de la locale UTF-8 (#70). Positions de
# regexpr()/substring()/nchar() : en octets sous C, en caractères sous UTF-8,
# mais toujours sur des chaînes de même régime, donc cohérentes entre elles.
# Listes et noms traités ; sans effet en locale UTF-8.
octets_utf8 <- function(x) {
  if (is.list(x)) x[] <- lapply(x, octets_utf8)
  else if (is.character(x)) {
    u <- !is.na(x) & Encoding(x) == "UTF-8"
    if (any(u)) {
      y <- x[u]
      Encoding(y) <- "unknown"
      x[u] <- y
    }
  }
  if (!is.null(names(x))) names(x) <- octets_utf8(names(x))
  x
}

# Retire les commentaires LaTeX : sur chaque ligne, tout ce qui suit le
# premier `%` précédé d'un nombre pair (zéro compris) de barres obliques
# inverses (`\%` est un pour-cent, `\\%` une fin de ligne suivie d'un
# commentaire), hors du contenu de \verb<d>...<d> (et \verb*) et du premier
# argument de \url{...} et \href{...}{...}, où `%` est un caractère. Les
# positions sont cherchées sur une copie où ces contenus sont remplacés par
# des espaces de même longueur ; le texte gardé est celui de la ligne.
# Limites : un \verb ou un \url coupé sur deux lignes, ou un \url dont
# l'argument contient une accolade, n'est pas reconnu.
retirer_commentaires <- function(lignes) {
  masque <- lignes
  for (re in c("\\\\verb\\*?([^A-Za-z*[:space:]]).*?\\1", "\\\\(url|href)\\{[^{}]*\\}")) {
    m <- gregexpr(re, masque, perl = TRUE)
    regmatches(masque, m) <- lapply(regmatches(masque, m), function(x) strrep(" ", nchar(x)))
  }
  p <- regexpr("(?<!\\\\)(?:\\\\\\\\)*%", masque, perl = TRUE)
  fin <- p + attr(p, "match.length") - 1L  # position du `%`
  ifelse(p > 0L, substr(lignes, 1L, fin - 1L), lignes)
}

# Lignes des .tex du dossier, commentaires LaTeX retirés.
texte_latex <- function(dossier) {
  tex <- if (dir.exists(dossier))
    list.files(dossier, pattern = "\\.tex$", recursive = TRUE, full.names = TRUE) else character(0)
  if (!length(tex)) return(character(0))
  # Sans encoding = : chaînes non marquées (voir octets_utf8()).
  texte <- unlist(lapply(tex, readLines, warn = FALSE))
  retirer_commentaires(texte)
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
SOURCES_CATALOGUES <- octets_utf8(local({
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
}))
# Catalogues où `()` marque une fonction ; ailleurs, aucun nom n'en porte.
avec_fonctions <- function(catalogue) catalogue == "exports" || startsWith(catalogue, "fonctions-")

texte <- texte_latex(DOSSIER_LATEX)
entrees <- entrees_catalogue(texte)
cites <- sort(unique(c(citations_latex(texte), entrees$nom[entrees$fonction])))
exports <- octets_utf8(sort(getNamespaceExports("tablexp")))
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
attente_entrees <- attente[attente$type %in% "entree", , drop = FALSE]
attente_entrees$catalogue <- sub(":.*$", "", attente_entrees$nom)
attente_entrees$entree <- sub("^[^:]*:", "", attente_entrees$nom)
attente_fiche <- attente[attente$type %in% "fiche", , drop = FALSE]

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
  verifier(sprintf("%s : type admis (export, catalogue, entree, fiche)", etiquette),
           if (attente$type[i] %in% TYPES_ATTENTE) TRUE else sprintf("type « %s »", attente$type[i]))
  if (identical(attente$type[i], "export"))
    verifier(sprintf("%s : export existant", etiquette),
             if (attente$nom[i] %in% exports) TRUE else "pas un export de tablexp : ligne à retirer")
  if (identical(attente$type[i], "catalogue"))
    verifier(sprintf("%s : catalogue requis", etiquette),
             if (attente$nom[i] %in% names(SOURCES_CATALOGUES)) TRUE
             else "pas un catalogue requis : ligne à retirer")
  if (identical(attente$type[i], "entree")) {
    k <- sub(":.*$", "", attente$nom[i]); n <- sub("^[^:]*:", "", attente$nom[i])
    verifier(sprintf("%s : entrée d'un catalogue requis, nom de sa source", etiquette),
             if (!grepl("^[a-z0-9-]+:[^:]+$", attente$nom[i])) "nom hors de la forme <catalogue>:<nom>"
             else if (!(k %in% names(SOURCES_CATALOGUES))) sprintf("catalogue « %s » non requis", k)
             else if (k %in% attente_catalogues$nom) sprintf("catalogue « %s » déjà en attente : ligne redondante", k)
             else if (!(n %in% SOURCES_CATALOGUES[[k]])) sprintf("« %s » absent de la source de %s : ligne à retirer", n, k)
             else TRUE)
  }
  if (identical(attente$type[i], "fiche"))
    verifier(sprintf("%s : constante de version, champs_fiche ou blocs_lecture", etiquette),
             if (attente$nom[i] %in% NOMS_ATTENTE_FICHE) TRUE
             else sprintf("« %s » hors de : %s", attente$nom[i], paste(NOMS_ATTENTE_FICHE, collapse = ", ")))
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
  en_attente <- attente_entrees[attente_entrees$catalogue == k & grepl(MOTIF_ISSUE, attente_entrees$issue), ,
                                drop = FALSE]
  for (j in seq_len(nrow(en_attente)))
    echec_attendu(sprintf("catalogue %s : entrée %s présente dans docs/latex/", k, en_attente$entree[j]),
                  en_attente$issue[j],
                  if (en_attente$entree[j] %in% lus$nom) TRUE else sprintf("absente (%s)", en_attente$motif[j]))
  verifier(sprintf("catalogue %s : noms égaux à la source (%d), entrées en attente exceptées (%d)", k,
                   length(unique(source_k)), nrow(en_attente)), {
    manquants <- setdiff(setdiff(source_k, en_attente$entree), lus$nom); en_trop <- setdiff(lus$nom, source_k)
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

fichier_courant("5. constantes de version et fiche de construction (#70)")
# Avec --latex (témoins des tests du script), un contrôle sans ancre dans le
# document est sauté ; sur docs/latex/, l'ancre est exigée.
ANCRE_EXIGEE <- !("--latex" %in% ARGS)
plein <- paste(texte, collapse = "\n")
sans_echappement <- function(x) gsub("\\\\_", "_", x)
# Noms \code{identifiant} (sans parenthèse), dans l'ordre du texte.
noms_code <- function(x)
  sans_echappement(unlist(regmatches(x, gregexpr("(?<=\\\\code\\{)[A-Za-z][A-Za-z0-9_\\\\]*(?=\\})", x, perl = TRUE))))
# Contrôle `nom` de la liste d'attente (type fiche) : échec attendu s'il y est
# déclaré. `ancre` FALSE : sauté avec --latex, écart sinon. `valeur` : TRUE ou
# la description de l'écart.
controle_fiche <- function(nom, etiquette, ancre, valeur) {
  if (!ancre && !ANCRE_EXIGEE) {
    cat(sprintf("  (sauté, ancre absente du document donné par --latex : %s)\n", etiquette))
    return(invisible(NULL))
  }
  r <- if (ancre) valeur else sprintf("ancre absente du document (%s)", etiquette)
  ligne <- attente_fiche[attente_fiche$nom == nom, , drop = FALSE]
  if (nrow(ligne) == 1L && grepl(MOTIF_ISSUE, ligne$issue))
    echec_attendu(etiquette, ligne$issue, if (isTRUE(r)) TRUE else sprintf("%s (%s)", r, ligne$motif))
  else verifier(etiquette, r)
}

# 5a. Valeurs citées des constantes de version : VERSION_FICHE (entier, forme
# \code{VERSION\_FICHE}, N ou ligne de catalogue « (N) ») ; en-têtes « tablexp
# <nature> N » (ENTETE_CANONIQUE...), cités dans le texte avec leur numéro.
valeurs_citees <- function(nom) {
  if (identical(nom, "VERSION_FICHE")) {
    echappe <- gsub("_", "\\\\\\\\_", nom)
    a <- regmatches(plein, gregexpr(sprintf("\\\\code\\{%s\\}[^0-9.]{0,15}[0-9]+", echappe), plein))[[1L]]
    lignes <- texte[grepl(sprintf("\\\\catalogue\\{[a-z0-9-]+\\}\\{%s\\}", echappe), texte)]
    cat_ <- unlist(lapply(strsplit(lignes, "&", fixed = TRUE), function(c)
      if (length(c) >= 4L) regmatches(c[4L], gregexpr("\\(([0-9]+)\\)", c[4L]))[[1L]]))
    c(sub("^.*[^0-9]", "", a), gsub("[()]", "", cat_))
  } else {
    valeur <- octets_utf8(NS[[nom]])
    prefixe <- sub("[0-9]+$", "", valeur)
    m <- regmatches(plein, gregexpr(paste0(prefixe, "[0-9]+"), plein, fixed = FALSE))[[1L]]
    substring(m, nchar(prefixe) + 1L)
  }
}
for (nom in CONSTANTES_VERSION) {
  attendu <- as.character(if (identical(nom, "VERSION_FICHE")) NS[[nom]] else sub("^.*[^0-9]", "", NS[[nom]]))
  cites_v <- valeurs_citees(nom)
  controle_fiche(nom, sprintf("%s citée avec sa valeur (%s)", nom, attendu), length(cites_v) > 0L,
                 if (all(cites_v == attendu)) TRUE
                 else sprintf("le document cite %s, le package %s", paste(unique(cites_v[cites_v != attendu]), collapse = ", "),
                              attendu))
}

# 5b. Fiche : champs de names(fiche) (plan avec et sans étape brut) et liste
# des champs du document.
fiches_temoins <- local({
  d <- as.Date
  base <- data.frame(
    contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
    naissance = d(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
    entree = d(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
    sortie = d(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
    cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
  roles <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
             date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
  plan <- list(transition = list(depart = "vivant", arrivee = "deces"),
               periode = list(debut = d("2015-01-01"), fin = d("2024-12-31")),
               delai_declaration = 0, segmentation = "sexe",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  plan_brut <- plan
  plan_brut$etapes$brut <- list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95)
  provenance <- list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
  list(sans_brut = tablexp::construire_table(base, roles, plan)$fiche,
       avec_brut = tablexp::construire_table(base, roles, plan_brut, provenance)$fiche)
})
champs_avec <- octets_utf8(names(fiches_temoins$avec_brut))
champs_sans <- octets_utf8(names(fiches_temoins$sans_brut))
# Noms à tous les niveaux (listes et data.frames).
noms_recursifs <- function(x) if (is.list(x)) c(names(x), unlist(lapply(x, noms_recursifs), use.names = FALSE)) else names(x)
noms_fiche <- octets_utf8(unique(unlist(lapply(fiches_temoins, noms_recursifs), use.names = FALSE)))
noms_tablexp <- octets_utf8(ls(NS, all.names = TRUE))
cat(sprintf("Champs de la fiche : %d avec étape brut, %d sans (conditionnels : %s)\n",
            length(champs_avec), length(champs_sans), paste(setdiff(champs_avec, champs_sans), collapse = ", ")))

i_liste <- grep("\\\\textbf\\{identification\\}", texte)
bloc_liste <- if (length(i_liste)) {
  fin <- grep("\\\\end\\{itemize\\}", texte)
  fin <- fin[fin > i_liste[1L]]
  if (length(fin)) paste(texte[i_liste[1L]:fin[1L]], collapse = "\n") else ""
} else ""
controle_fiche("champs_fiche", "liste des champs de la fiche du document égale à names(fiche), dans l'ordre",
               nzchar(bloc_liste), {
  jetons <- noms_code(bloc_liste)
  doc_champs <- unique(jetons[jetons %in% champs_avec])
  manquants <- setdiff(champs_avec, doc_champs)
  # Chaque \code{} du bloc : fonction citée avec (), ou nom classé.
  tous <- sans_echappement(unlist(regmatches(bloc_liste, gregexpr("(?<=\\\\code\\{)[^{}]+(?=\\})", bloc_liste, perl = TRUE))))
  fonctions <- unique(sub("\\(.*\\)$", "", tous[grepl("\\(.*\\)$", tous)]))
  nommes <- unique(tous[!grepl("\\(.*\\)$", tous)])
  inconnus <- c(nommes[!(nommes %in% c(noms_fiche, noms_tablexp, octets_utf8(NS[["ESTIMATEURS_BRUT"]]), WHITELIST_FICHE))],
                fonctions[!vapply(fonctions, function(f) isTRUE(fonction_existe(f)), logical(1L))])
  ecarts <- c(if (!all(champs_sans %in% champs_avec)) "fiche sans étape brut : champs absents de la fiche avec étape brut",
              if (length(manquants)) paste("champs de la fiche absents du document :", paste(manquants, collapse = ", ")),
              if (length(inconnus)) paste("noms du document absents de la fiche, du package et de la liste blanche :",
                                          paste(inconnus, collapse = ", ")),
              if (!length(manquants) && !identical(doc_champs, champs_avec))
                paste("ordre du document :", paste(doc_champs, collapse = ", "), "; ordre de la fiche :",
                      paste(champs_avec, collapse = ", ")))
  if (length(ecarts)) paste(ecarts, collapse = " ; ") else TRUE
})

# 5c. Blocs de lecture : \emph{<bloc>}~: champs..., jusqu'au bloc suivant ou
# au point qui clôt l'énumération.
blocs_code <- octets_utf8(tablexp:::BLOCS_LECTURE_FICHE)
ancres_blocs <- vapply(names(blocs_code), function(b)
  as.integer(regexpr(sprintf("\\emph{%s}~:", b), plein, fixed = TRUE)), integer(1L))
controle_fiche("blocs_lecture", "blocs de lecture du document égaux à BLOCS_LECTURE_FICHE (blocs, champs, ordre)",
               any(ancres_blocs > 0L), {
  if (any(ancres_blocs < 0L))
    sprintf("blocs absents du document : %s", paste(names(blocs_code)[ancres_blocs < 0L], collapse = ", "))
  else {
    debut <- min(ancres_blocs)
    reste <- substring(plein, debut)
    point <- regexpr("\\.(\\s|$)", reste)
    paragraphe <- substr(reste, 1L, if (point > 0L) point else nchar(reste))
    bornes <- c(sort(ancres_blocs) - debut + 1L, nchar(paragraphe) + 1L)
    ecarts <- character(0)
    if (!identical(names(sort(ancres_blocs)), names(blocs_code)))
      ecarts <- sprintf("ordre des blocs du document : %s ; ordre du package : %s",
                        paste(names(sort(ancres_blocs)), collapse = ", "), paste(names(blocs_code), collapse = ", "))
    for (b in names(blocs_code)) {
      k <- match(ancres_blocs[[b]], sort(ancres_blocs))
      lus <- noms_code(substr(paragraphe, bornes[k], bornes[k + 1L] - 1L))
      if (!identical(lus, unname(blocs_code[[b]])))
        ecarts <- c(ecarts, sprintf("bloc %s : document « %s », package « %s »", b, paste(lus, collapse = ", "),
                                    paste(blocs_code[[b]], collapse = ", ")))
    }
    n_blocs_doc <- lengths(regmatches(paragraphe, gregexpr("\\\\emph\\{[^{}]+\\}~:", paragraphe)))
    if (n_blocs_doc != length(blocs_code))
      ecarts <- c(ecarts, sprintf("%d bloc(s) dans le document, %d dans le package", n_blocs_doc, length(blocs_code)))
    if (length(ecarts)) paste(ecarts, collapse = " ; ") else TRUE
  }
})

fichier_courant("6. aucun nom coupé par \\allowbreak entre deux \\code{} (#84)")
# Motifs interdits, cherchés sur `plein` (fichiers joints par des fins de
# ligne). (1) fin d'un \code{}, éventuellement des blancs (un `%` de fin de
# ligne, retiré avec le commentaire, colle pourtant les deux), \allowbreak,
# puis des blancs (LaTeX les ignore après une macro) ou {} sans blanc, un
# \code{} qui contient une parenthèse ouvrante
# (fragment final d'un nom de fonction coupé) ; (2) \code{} à parenthèse
# ouverte non fermée, puis \allowbreak (nom coupé dans ses arguments).
MOTIFS_ALLOWBREAK_CODE <- c(
  "\\\\code\\{[^{}]*\\}\\s*\\\\allowbreak(\\s*|\\{\\})\\\\code\\{[^{}]*\\(",
  "\\\\code\\{[^{}]*\\([^{})]*\\}\\s*\\\\allowbreak")
coupes <- unlist(lapply(MOTIFS_ALLOWBREAK_CODE, function(re) regmatches(plein, gregexpr(re, plein))[[1L]]))
verifier("aucun nom de fonction coupé par \\allowbreak entre deux \\code{} (\\code{}\\allowbreak\\code{...( ; \\code{f(}\\allowbreak)",
         if (!length(coupes)) TRUE
         else sprintf("%d occurrence(s) : %s", length(coupes),
                      paste(sprintf("« %s »", gsub("\n", " ", coupes)), collapse = ", ")))

if (!STRICT) cat("\nMode rapport : code de sortie 0 même en cas d'écart (--strict pour échouer).\n")
bilan(code_sortie = STRICT)
