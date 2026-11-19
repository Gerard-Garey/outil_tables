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
#       attente), un écart de citation (type `citation`, nom `marqueurs`, contrôle
#       7), un écart de renvoi de ligne (type `renvoi`, nom `lignes`, contrôle 8)
#       et porte un renvoi d'issue « #N » ; aucune ligne en double ;
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
#         recettes             CATALOGUE_RECETTES$recette (R/donnees.R), source
#                              unique (ADR 0016, point 1.5).
#    5. constantes de version et fiche de construction (#70) ; seule la
#       valeur est comparée, la rédaction du document reste libre :
#       - toute valeur citée des constantes de version est égale à sa valeur
#         dans le package, et chacune est citée au moins une fois :
#         VERSION_FICHE (tout entier qui suit \code{VERSION\_FICHE} à au plus
#         15 caractères non numériques près, ou entier « (N) » de la 4e colonne
#         d'une ligne de catalogue), ENTETE_CANONIQUE, REPRESENTATION_RESULTAT,
#         REPRESENTATION_TABLE (#73), REPRESENTATION_DECALAGES (#273),
#         PREMIERE_LIGNE_PLAN et PREMIERE_LIGNE_FICHE (en-têtes « tablexp
#         <nature> N » cités dans le texte). Limite : toute mention d'une
#         autre valeur (« passe de 2 à 3 » à moins de 15 caractères de
#         \code{VERSION\_FICHE}, « tablexp canonique 1 » historique) est lue
#         comme une citation et échoue ; un entier plus loin ou sans
#         \code{VERSION\_FICHE} échappe ;
#       - liste des champs de la fiche du document (le bloc d'items qui
#         commence à \textbf{identification}) : ordre des champs de premier
#         niveau, lus par leur première occurrence \code{nom}, égal à celui de
#         names(fiche) rendue par construire_table() sur des agrégats avec
#         étape brut (fiche qui porte tous les champs, dont le champ
#         conditionnel hypotheses_recette, #169), les fiches en lignes avec et
#         sans étape brut en étant des sous-ensembles ; en outre,
#         chaque \code{} du bloc est classé dans l'une de ces sources, sinon
#         c'est un écart : (1) noms de la fiche à tous les niveaux (names()
#         récursif) de fiches témoins avec une provenance fournie,
#         l'estimateur kaplan_meier retenu, et sous la recette agrégats ; (2) objets de l'espace de noms de
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
#    7. citations « mot pour mot » (#130 ; CONVENTIONS.md, section 3.5) : une
#       citation d'un libellé de CONVENTIONS_TEXTE ou de MENTIONS_RAPPORT
#       (R/rapport.R) est désignée par un marqueur, ligne de commentaire
#       LaTeX `% citation : CONVENTIONS_TEXTE["doubles"]` (lue sur le texte
#       brut : les marqueurs sont des commentaires), suivie de la citation entre
#       « » ; l'exemption, pour une phrase « mot pour mot » qui ne cite aucune
#       constante, est `% citation : aucune (motif)` :
#       - un marqueur illisible est un écart : toute ligne de commentaire de la
#         forme `^\s*%+\s*citations?\s*:` (casse indifférente) est lue comme
#         marqueur, valide seulement sous l'une des deux formes ci-dessus,
#         exactes (`% Citation :`, `%% citation :`, `% citations :` sont
#         illisibles) ; une constante autre que CONVENTIONS_TEXTE et
#         MENTIONS_RAPPORT, un motif d'exemption vide en sont aussi ;
#       - le libellé existe dans la constante, la citation « » existe entre le
#         marqueur et la fin de son paragraphe (ligne blanche ou marqueur
#         suivant) ; sinon écart ;
#       - la citation, normalisée par la règle fermée (\code{x} lu x, ~ lu une
#         espace, \% lu %, \_ lu _, blancs consécutifs réduits à une espace,
#         blancs de bord retirés), est égale à la valeur de la constante dans
#         l'espace de noms de tablexp ; sinon le message donne le libellé, les
#         lignes et la première position d'écart (points de code du texte
#         normalisé) ;
#       - toute expression « mot pour mot » du texte (commentaires retirés,
#         casse et passage à la ligne indifférents ; mots bornés, « mot pour
#         motiver » n'en est pas une ; blancs, `~` et `\ ` admis entre les
#         mots) est suivie, à la ligne qui suit celle où elle se termine, d'un
#         marqueur ; sinon écart, attendu s'il est déclaré dans la liste
#         d'attente (type `citation`, nom `marqueurs`), succès inattendu dès que
#         tout est marqué.
#       Limites : un libellé dont la valeur porte une barre oblique inverse,
#       une accolade, un `~`, un `#`, un `$`, un `&`, un `^`, un « ou un », des
#       blancs de bord ou deux blancs consécutifs ne se cite pas sous cette
#       règle fermée (le texte LaTeX correspondant n'est pas l'inverse de la
#       normalisation) ; une phrase qui cite sans écrire « mot pour mot » n'est
#       pas vue.
#
#    8. aucun numéro de ligne vers un fichier du dépôt (#187 ; décision D4 (b) du
#       19/11/2026, feuille de route § 1.32 ; CONVENTIONS.md : règle à écrire) : le
#       renvoi à un emplacement du code se fait par le symbole, jamais par un
#       numéro de ligne, qui dérive à chaque commit du fichier visé. Lu sur le
#       texte sans commentaires LaTeX des .tex, fichier par fichier (CONVENTIONS.md
#       n'est pas lu). Trois formes de renvoi, fermées :
#       - `l.` ou `ll.` suivi d'un numéro (« l.~516 », « l. 98 », « l.~350 à~352 »,
#         « l.~3592--3610 » : le numéro de tête suffit), `l` précédé ni d'une
#         lettre, ni d'un chiffre, ni d'un souligné ; blancs, `~`, `\ ` et `\,`
#         admis entre le point et le numéro ;
#       - un chemin suivi de `:` et d'un numéro, blancs ou `~` admis de part et
#         d'autre du `:` (« R/x.R:118 », « \code{R/x.R}:~118 »,
#         « \code{R/x.R}~:~118 »), ou d'une virgule et de « ligne(s) N »
#         (« \code{R/x.R}, ligne~118 ») ;
#       - « ligne(s) N (à M) de|du|d' [fichier] <chemin> », initiale majuscule
#         admise (« Ligne~118 de \code{R/x.R} ») ;
#       - une URL du dépôt (ci-dessous) qui porte l'ancre `#L<n>` ou
#         `#L<n>-L<m>` (`\#` admis), quelle que soit la ref (branche ou SHA).
#       Un chemin est un jeton à extension fermée (R, Rd, Rmd, md, tex, csv, tsv,
#       yml, yaml, sh, js, json, txt, c, h, cpp, py, html, pdf, cff), souligné
#       échappé admis, jamais précédé d'un `/`. Critère « fichier du dépôt » : le
#       premier segment du chemin existe à la racine du dépôt, ou est `app`
#       (dossier de CLAUDE.md, « Architecture », pas encore au dépôt), ou le
#       chemin est un nom seul (sans `/`) qui est celui d'un fichier de l'arbre
#       (hors .git). Ce dernier cas dépend des fichiers présents dans l'arbre de
#       travail, suivis ou non (git n'est pas lu : il est absent du job de la CI) :
#       un fichier non suivi de même nom qu'un chemin cité rend ce chemin « du
#       dépôt ». Une URL (argument de \url ou du premier argument de \href) est du
#       dépôt si son hôte est github.com, www.github.com ou
#       raw.githubusercontent.com et son chemin commence par
#       Gerard-Garey/outil_tables/ (casse indifférente ; décision d'`actuary`,
#       précision d'application de D4 (b)). Tout autre chemin, et toute autre URL,
#       est une source externe. Une forme `l.` est rattachée au chemin ou à l'URL
#       le plus proche qui la précède dans la même phrase (la phrase commence
#       après un point suivi de blancs et d'une majuscule, d'une lettre accentuée
#       ou d'une commande LaTeX `\nom`, une ligne blanche, un \item ou un `\\`
#       double) : refusée si cette source est du dépôt, admise si elle est
#       externe (étiquette fixe d'une source externe, « l.~3592--3610 » du
#       fichier C de R), et refusée si aucune source n'est nommée avant le
#       renvoi dans la phrase (une source nommée après ne compte pas). Aucune
#       ancre n'est exigée. Écart transitoire
#       déclaré dans la liste d'attente, type `renvoi`, nom `lignes` : échec
#       attendu, succès inattendu dès qu'aucun renvoi ne reste. Limites : seules
#       les formes ci-dessus sont lues ; « ligne 118 » sans chemin juste avant
#       (virgule) ni « de <chemin> » juste après n'est pas vu (les « ligne~9 » du
#       bilan du rejeu ne sont pas des renvois), ni « ligne 118 » suivi d'autre
#       chose que « de|du|d' [fichier] » avant le chemin (« ligne~118 dans
#       \code{R/x.R} »), ni « l. 118 » écrit « l 118 » ou « l.118 » d'un \code{} ;
#       inversement « \code{R/x.R}~: 3 fonctions » serait lu comme un renvoi ; un
#       chemin hors de la liste d'extensions n'est pas reconnu ; une URL écrite
#       hors de \url et \href (texte nu, \code{}) n'est pas reconnue ; une
#       frontière de phrase n'est pas vue après une abréviation suivie d'une
#       minuscule, ni vue à tort après « cf. \ref{} » (sans effet : une source
#       qui précède est alors simplement ignorée) ; un renvoi à une source
#       externe sans chemin ni URL avant lui dans la phrase est refusé.
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
TYPES_ATTENTE <- c("export", "catalogue", "entree", "fiche", "citation", "renvoi")
# Contrôles 5 : constantes de version (nom de la constante) et deux listes.
CONSTANTES_VERSION <- c("VERSION_FICHE", "ENTETE_CANONIQUE", "REPRESENTATION_RESULTAT", "REPRESENTATION_TABLE",
                        "REPRESENTATION_DECALAGES", "PREMIERE_LIGNE_PLAN", "PREMIERE_LIGNE_FICHE")
NOMS_ATTENTE_FICHE <- c(CONSTANTES_VERSION, "champs_fiche", "blocs_lecture")
# Contrôle 7 : constantes de R/rapport.R dont une citation est contrôlée, et
# nom de l'écart transitoire admis dans la liste d'attente (type citation).
CONSTANTES_CITABLES <- c("CONVENTIONS_TEXTE", "MENTIONS_RAPPORT")
NOMS_ATTENTE_CITATION <- "marqueurs"
# Contrôle 8 : nom de l'écart transitoire admis dans la liste d'attente (type renvoi).
NOMS_ATTENTE_RENVOI <- "lignes"
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

# Lignes des .tex du dossier : data.frame(fichier, ligne, brut, texte), `fichier`
# relatif au dossier, `ligne` numérotée dans le fichier, `brut` la ligne lue,
# `texte` la ligne sans commentaire LaTeX. Les contrôles qui lisent le texte
# prennent `texte` ; ceux qui lisent les commentaires (marqueurs, contrôle 7) ou
# qui rapportent un emplacement fichier:ligne prennent `brut`, `fichier`, `ligne`.
lignes_latex <- function(dossier) {
  tex <- if (dir.exists(dossier))
    list.files(dossier, pattern = "\\.tex$", recursive = TRUE) else character(0)
  # Sans encoding = : chaînes non marquées (voir octets_utf8()).
  brut <- lapply(tex, function(f) readLines(file.path(dossier, f), warn = FALSE))
  d <- data.frame(fichier = rep(tex, lengths(brut)), ligne = as.integer(unlist(lapply(lengths(brut), seq_len))),
                  brut = as.character(unlist(brut)), stringsAsFactors = FALSE)
  d$texte <- if (nrow(d)) retirer_commentaires(d$brut) else character(0)
  d
}

# Citations \code{nom()} ou \code{pkg::nom()}.
citations_latex <- function(texte) {
  # \code{nom()} ou \code{nom(arguments)}, souligné échappé \_ admis ; les
  # arguments ne contiennent pas d'accolade. Cherchées sur le texte joint par
  # des fins de ligne (comme `plein`, contrôles 5 et 6) : la liste d'arguments
  # peut courir sur plusieurs lignes (#90). `[^{}]*` ne franchit pas d'accolade :
  # \code{f(}\allowbreak\code{x)} n'est toujours pas une citation.
  plein <- paste(texte, collapse = "\n")
  m <- unlist(regmatches(plein, gregexpr("\\\\code\\{[A-Za-z0-9._\\\\:]+\\([^{}]*\\)\\}", plein)))
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
         recettes = NS[["CATALOGUE_RECETTES"]]$recette))
}))
# Catalogues où `()` marque une fonction ; ailleurs, aucun nom n'en porte.
avec_fonctions <- function(catalogue) catalogue == "exports" || startsWith(catalogue, "fonctions-")

lignes <- lignes_latex(DOSSIER_LATEX)
texte <- lignes$texte
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
attente_citation <- attente[attente$type %in% "citation", , drop = FALSE]
attente_renvoi <- attente[attente$type %in% "renvoi", , drop = FALSE]

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
  verifier(sprintf("%s : type admis (export, catalogue, entree, fiche, citation, renvoi)", etiquette),
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
  if (identical(attente$type[i], "citation"))
    verifier(sprintf("%s : écart de citation admis", etiquette),
             if (attente$nom[i] %in% NOMS_ATTENTE_CITATION) TRUE
             else sprintf("« %s » hors de : %s", attente$nom[i], paste(NOMS_ATTENTE_CITATION, collapse = ", ")))
  if (identical(attente$type[i], "renvoi"))
    verifier(sprintf("%s : écart de renvoi de ligne admis", etiquette),
             if (attente$nom[i] %in% NOMS_ATTENTE_RENVOI) TRUE
             else sprintf("« %s » hors de : %s", attente$nom[i], paste(NOMS_ATTENTE_RENVOI, collapse = ", ")))
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
  # Agrégats (#169) : champ conditionnel hypotheses_recette, absent sous les
  # lignes.
  agregats <- data.frame(sexe = c("H", "F"), age = c(60, 61), ec = c(10, 12), ei = c(10.5, 12), d = c(1, 0),
                         stringsAsFactors = FALSE)
  plan_agregats <- plan
  plan_agregats$etapes$exposition$convention <- "dernier_anniversaire"
  plan_agregats$etapes$brut <- list(estimateurs = c("poisson", "binomial"), niveau = 0.95)
  list(sans_brut = tablexp::construire_table(base, roles, plan)$fiche,
       avec_brut = tablexp::construire_table(base, roles, plan_brut, provenance)$fiche,
       agregats = tablexp::construire_table(agregats, c(sexe = "sexe", age = "age", exposition_centrale = "ec",
                                                        exposition_initiale = "ei", evenements = "d"),
                                            plan_agregats, provenance)$fiche)
})
champs_avec <- octets_utf8(names(fiches_temoins$agregats))
champs_sans <- octets_utf8(names(fiches_temoins$sans_brut))
# Noms à tous les niveaux (listes et data.frames).
noms_recursifs <- function(x) if (is.list(x)) c(names(x), unlist(lapply(x, noms_recursifs), use.names = FALSE)) else names(x)
noms_fiche <- octets_utf8(unique(unlist(lapply(fiches_temoins, noms_recursifs), use.names = FALSE)))
noms_tablexp <- octets_utf8(ls(NS, all.names = TRUE))
cat(sprintf("Champs de la fiche : %d (agrégats, étape brut), %d en lignes sans étape brut (conditionnels : %s)\n",
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
  ecarts <- c(if (!all(octets_utf8(names(fiches_temoins$avec_brut)) %in% champs_avec) ||
                  !all(octets_utf8(names(fiches_temoins$sans_brut)) %in% champs_avec))
                "fiche en lignes (avec ou sans étape brut) : champs absents de la fiche des agrégats avec étape brut",
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
    reste <- substring(plein, debut, nchar(plein))
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

fichier_courant("7. citations « mot pour mot » des constantes de R/rapport.R (#130)")
# Règle de normalisation, fermée (CONVENTIONS.md, section 3.5) : \code{x} lu x,
# `~` lu une espace, `\%` lu `%`, `\_` lu `_`, blancs consécutifs réduits à une
# espace, blancs de bord retirés (dans « «~texte~» », le `~` de bord est une espace).
normaliser_citation <- function(x) {
  x <- gsub("\\\\code\\{([^{}]*)\\}", "\\1", x)
  x <- gsub("~", " ", x, fixed = TRUE)
  x <- gsub("\\%", "%", x, fixed = TRUE)
  x <- gsub("\\_", "_", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}
# Première position (point de code, de 1 à n + 1) où deux textes diffèrent, NA
# s'ils sont égaux ; utf8ToInt() lit les octets comme de l'UTF-8 quelle que soit
# la locale.
premiere_difference <- function(a, b) {
  ca <- utf8ToInt(a); cb <- utf8ToInt(b)
  n <- min(length(ca), length(cb))
  i <- which(ca[seq_len(n)] != cb[seq_len(n)])
  if (length(i)) i[1L] else if (length(ca) == length(cb)) NA_integer_ else n + 1L
}
# Les `n` premiers points de code de chaque élément de `x` (pas d'octets : sous
# la locale C, substr() couperait un caractère en deux).
tronquer <- function(x, n)
  vapply(x, function(y) {
    cp <- utf8ToInt(y)
    octets_utf8(intToUtf8(cp[seq_len(min(n, length(cp)))]))
  }, "", USE.NAMES = FALSE)
# Voisinage de la position `pos` (points de code) de `x`, pour un message.
voisinage <- function(x, pos, marge = 15L) {
  cp <- utf8ToInt(x)
  if (pos > length(cp)) return("(fin du texte)")
  sprintf("« %s »", octets_utf8(intToUtf8(cp[max(1L, pos - marge):min(length(cp), pos + marge)])))
}
# Toute ligne de commentaire qui ressemble à un marqueur (`%` en nombre
# quelconque, « Citation » ou « citations », casse indifférente) est lue comme
# tel ; elle est valide seulement sous la forme exacte (un `%`, « citation »
# en minuscules au singulier), sinon c'est un marqueur illisible.
MOTIF_MARQUEUR_APPROCHE <- "^[[:space:]]*%+[[:space:]]*citations?[[:space:]]*:"
MOTIF_MARQUEUR <- "^[[:space:]]*%[[:space:]]*citation[[:space:]]*:[[:space:]]*"
MOTIF_MARQUEUR_CONSTANTE <- "^([A-Z_]+)\\[\"([^\"]+)\"\\][[:space:]]*$"
MOTIF_MARQUEUR_AUCUNE <- "^aucune[[:space:]]*\\([^()[:space:]][^()]*\\)[[:space:]]*$"
# Expression bornée (ni lettre ASCII, ni chiffre, ni souligné de part et d'autre :
# « mot pour motiver » n'en est pas une), séparée par des blancs, `~` ou `\ `.
MOTIF_MOT_POUR_MOT <- "(?<![A-Za-z0-9_])mot(?:[[:space:]~]|\\\\ )+pour(?:[[:space:]~]|\\\\ )+mot(?![A-Za-z0-9_])"
# Marqueurs : data.frame(fichier, ligne, forme, constante, libelle), `forme`
# valant constante, aucune ou illisible, lus sur le texte brut.
lire_marqueurs <- function(lignes) {
  est <- grepl(MOTIF_MARQUEUR_APPROCHE, lignes$brut, ignore.case = TRUE)
  exact <- grepl(MOTIF_MARQUEUR, lignes$brut[est])
  corps <- sub(MOTIF_MARQUEUR, "", lignes$brut[est])
  forme <- ifelse(exact & grepl(MOTIF_MARQUEUR_CONSTANTE, corps), "constante",
                  ifelse(exact & grepl(MOTIF_MARQUEUR_AUCUNE, corps), "aucune", "illisible"))
  data.frame(fichier = lignes$fichier[est], ligne = lignes$ligne[est], forme = forme,
             constante = ifelse(forme == "constante", sub(MOTIF_MARQUEUR_CONSTANTE, "\\1", corps), NA_character_),
             libelle = ifelse(forme == "constante", sub(MOTIF_MARQUEUR_CONSTANTE, "\\2", corps), NA_character_),
             stringsAsFactors = FALSE)
}
# Citation « » qui suit la ligne `j` d'un fichier (`f` : toutes les lignes du
# fichier) : data.frame d'une ligne (texte, ligne) ou chaîne qui décrit
# l'écart. Zone lue : de la ligne suivante à la dernière ligne avant la
# première ligne blanche ou le premier marqueur, texte sans commentaire ; deux
# lignes se joignent par une espace, sauf si la première portait un commentaire
# (un `%` de fin de ligne colle les lignes, LaTeX).
citation_apres <- function(f, j) {
  suite <- seq_len(nrow(f))[seq_len(nrow(f)) > j]
  arret <- suite[grepl("^[[:space:]]*$", f$brut[suite]) | grepl(MOTIF_MARQUEUR_APPROCHE, f$brut[suite], ignore.case = TRUE)]
  zone <- if (length(arret)) suite[suite < arret[1L]] else suite
  if (!length(zone)) return("aucune ligne entre le marqueur et la fin de son paragraphe")
  t <- f$texte[zone]
  collee <- t != f$brut[zone]
  zone_texte <- paste0(t, ifelse(collee, "", " "), collapse = "")
  ouvrante <- regexpr("«", zone_texte, fixed = TRUE)
  if (ouvrante < 0L) return("aucune citation « » entre le marqueur et la fin de son paragraphe")
  reste <- substring(zone_texte, ouvrante + attr(ouvrante, "match.length"))
  fermante <- regexpr("»", reste, fixed = TRUE)
  if (fermante < 0L) return("citation « non fermée par » avant la fin du paragraphe")
  data.frame(texte = substr(reste, 1L, fermante - 1L),
             ligne = f$ligne[zone][which(grepl("«", t, fixed = TRUE))[1L]], stringsAsFactors = FALSE)
}
# Écart du marqueur `m` (une ligne de lire_marqueurs()) : TRUE ou la description.
ecart_citation <- function(m, f) {
  if (!(m$constante %in% CONSTANTES_CITABLES))
    return(sprintf("constante %s non citable (admises : %s)", m$constante, paste(CONSTANTES_CITABLES, collapse = ", ")))
  valeurs <- octets_utf8(NS[[m$constante]])
  if (!(m$libelle %in% names(valeurs)))
    return(sprintf("libellé « %s » inexistant dans %s", m$libelle, m$constante))
  c_ <- citation_apres(f, m$ligne)
  if (is.character(c_)) return(c_)
  doc <- normaliser_citation(c_$texte)
  attendu <- valeurs[[m$libelle]]
  pos <- premiere_difference(doc, attendu)
  if (is.na(pos)) return(TRUE)
  sprintf("première différence à la position %d du texte normalisé : document %s, constante %s (citation ligne %d)",
          pos, voisinage(doc, pos), voisinage(attendu, pos), c_$ligne)
}
marqueurs <- lire_marqueurs(lignes)
expressions <- local({
  r <- list()
  for (fi in unique(lignes$fichier)) {
    f <- lignes[lignes$fichier == fi, , drop = FALSE]
    plein_f <- paste0(f$texte, "\n", collapse = "")
    debuts <- cumsum(c(1L, nchar(f$texte) + 1L))
    p <- gregexpr(MOTIF_MOT_POUR_MOT, plein_f, ignore.case = TRUE, perl = TRUE)[[1L]]
    if (p[1L] > 0L) {
      fin <- p + attr(p, "match.length") - 1L
      l <- f$ligne[findInterval(fin, debuts)]
      r[[fi]] <- data.frame(fichier = fi, ligne = l, texte = trimws(f$texte[match(l, f$ligne)]), stringsAsFactors = FALSE)
    }
  }
  if (length(r)) do.call(rbind, r) else data.frame(fichier = character(0), ligne = integer(0), texte = character(0))
})
cat(sprintf("Marqueurs de citation : %d ; expressions « mot pour mot » : %d\n", nrow(marqueurs), nrow(expressions)))

verifier("marqueurs `% citation :` tous lisibles (CONSTANTE[\"libellé\"] ou aucune (motif))", {
  il <- marqueurs[marqueurs$forme == "illisible", , drop = FALSE]
  if (nrow(il)) paste("marqueur illisible :", paste(sprintf("%s ligne %d", il$fichier, il$ligne), collapse = " ; "))
  else TRUE
})
for (i in which(marqueurs$forme == "constante")) {
  m <- marqueurs[i, , drop = FALSE]
  r <- ecart_citation(m, lignes[lignes$fichier == m$fichier, , drop = FALSE])
  verifier(sprintf("citation %s[\"%s\"] (%s ligne %d) égale à la valeur de la constante", m$constante, m$libelle,
                   m$fichier, m$ligne), r)
}
non_marquees <- expressions[!(paste(expressions$fichier, expressions$ligne + 1L) %in%
                                paste(marqueurs$fichier, marqueurs$ligne)), , drop = FALSE]
ligne_citation <- attente_citation[attente_citation$nom == "marqueurs", , drop = FALSE]
r_marquees <- if (!nrow(non_marquees)) TRUE else
  sprintf("%d expression(s) sans marqueur sur la ligne suivante : %s", nrow(non_marquees),
          paste(sprintf("%s ligne %d « %s »", non_marquees$fichier, non_marquees$ligne,
                        tronquer(non_marquees$texte, 60L)), collapse = " ; "))
if (nrow(ligne_citation) == 1L && grepl(MOTIF_ISSUE, ligne_citation$issue)) {
  echec_attendu("toute expression « mot pour mot » est suivie d'un marqueur de citation", ligne_citation$issue,
                if (isTRUE(r_marquees)) TRUE else sprintf("%s (%s)", r_marquees, ligne_citation$motif))
} else {
  verifier("toute expression « mot pour mot » est suivie d'un marqueur de citation", r_marquees)
}

fichier_courant("8. aucun numéro de ligne vers un fichier du dépôt (#187)")
# Formes lues et critère « fichier du dépôt » : voir l'en-tête, contrôle 8. Les
# motifs sont bornés (extensions fermées, lookbehind) ; ceux qui portent un
# caractère non ASCII s'en passent (une classe d'octets ou de points de code
# \x80-\xff couvre « à » en UTF-8 comme en C), pour un verdict identique sous les
# deux locales.
EXTENSIONS_CHEMIN <- c("R", "Rd", "Rmd", "md", "tex", "csv", "tsv", "yml", "yaml", "sh", "js", "json", "txt",
                       "c", "h", "cpp", "py", "html", "pdf", "cff")
RACINES_PREVUES <- "app"
BLANC_RENVOI <- "(?:[[:space:]~]|\\\\ |\\\\,)"
SEGMENT_CHEMIN <- "(?:[A-Za-z0-9.-]|\\\\_|_)+"
MOTIF_CHEMIN <- sprintf("(?<![A-Za-z0-9_./:\\\\-])%s(?:/%s)*\\.(?:%s)(?![A-Za-z0-9_/-])", SEGMENT_CHEMIN, SEGMENT_CHEMIN,
                        paste(EXTENSIONS_CHEMIN, collapse = "|"))
MOTIF_SOURCE_URL <- "\\\\(?:url|href)\\{[^{}]*\\}"
MOTIF_RENVOI_L <- sprintf("(?<![A-Za-z0-9_])ll?\\.%s*[0-9]+", BLANC_RENVOI)
MOTIF_RENVOI_SUFFIXE <- sprintf("^\\}?%1$s*(?::|,%1$s*[Ll]ignes?%1$s)%1$s*[0-9]", BLANC_RENVOI)
MOTIF_RENVOI_LIGNE_DE <- sprintf(paste0("(?<![A-Za-z0-9_])[Ll]ignes?%1$s+[0-9]+(?:%1$s*(?:[A-Za-z\\x80-\\xff]{1,2}|--|,)%1$s*[0-9]+)*",
                                        "%1$s+(?:de|du|d')%1$s*(?:fichier%1$s+)?(?:\\\\code\\{)?$"), BLANC_RENVOI)
MOTIF_FRONTIERE_PHRASE <- "\\.[[:space:]~]+(?=[A-Z]|[^\\x01-\\x7f]|\\\\[A-Za-z])|\\n[ \\t]*\\n|\\\\item(?![A-Za-z])|\\\\\\\\"
# URL du dépôt (casse indifférente) et ancre de ligne d'une telle URL.
MOTIF_URL_DEPOT <- "^(?:https?://)?(?:(?:www\\.)?github\\.com|raw\\.githubusercontent\\.com)/gerard-garey/outil_tables/"
MOTIF_ANCRE_LIGNE <- "#L[0-9]+(?:-L[0-9]+)?$"
SANS_SOURCE <- "aucune source nommée avant le renvoi dans la phrase"
# Noms des fichiers de l'arbre (hors .git, suivis ou non), calculés à la première demande.
noms_fichiers_depot <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      haut <- setdiff(list.files(RACINE, all.files = TRUE, no.. = TRUE), ".git")
      dossiers <- haut[dir.exists(file.path(RACINE, haut))]
      tous <- unlist(lapply(dossiers, function(d) list.files(file.path(RACINE, d), recursive = TRUE, all.files = TRUE)))
      cache <<- unique(c(basename(tous), haut[!(haut %in% dossiers)]))
    }
    cache
  }
})
# TRUE si le chemin `x` (souligné échappé déjà lu) désigne un fichier du dépôt.
chemin_du_depot <- function(x) {
  premier <- sub("/.*$", "", x)
  file.exists(file.path(RACINE, premier)) || premier %in% RACINES_PREVUES ||
    (!grepl("/", x, fixed = TRUE) && x %in% noms_fichiers_depot())
}
# Débuts et fins (positions) des occurrences de `re` dans `x`.
occurrences <- function(re, x) {
  g <- gregexpr(re, x, perl = TRUE)[[1L]]
  if (g[1L] > 0L) data.frame(debut = as.integer(g), fin = as.integer(g) + attr(g, "match.length") - 1L)
  else data.frame(debut = integer(0), fin = integer(0))
}
# substring() qui accepte des vecteurs vides (elle les refuse).
sous_texte <- function(x, debut, fin = rep(.Machine$integer.max, length(debut)))
  if (length(debut)) substring(x, debut, fin) else character(0)
# Renvois d'un fichier (`f` : ses lignes, comme lignes_latex()) :
# data.frame(fichier, ligne, texte, chemin), `chemin` valant le chemin ou l'URL
# du dépôt visé, ou SANS_SOURCE.
renvois_de_ligne <- function(f) {
  plein_f <- paste0(f$texte, "\n", collapse = "")
  debuts <- cumsum(c(1L, nchar(f$texte) + 1L))
  ch <- occurrences(MOTIF_CHEMIN, plein_f)
  ch$chemin <- gsub("\\_", "_", sous_texte(plein_f, ch$debut, ch$fin), fixed = TRUE)
  ch$depot <- vapply(ch$chemin, chemin_du_depot, logical(1L))
  ur <- occurrences(MOTIF_SOURCE_URL, plein_f)
  ur$url <- gsub("\\\\([#_%&])", "\\1", sub("^\\\\(?:url|href)\\{(.*)\\}$", "\\1", sous_texte(plein_f, ur$debut, ur$fin), perl = TRUE))
  ur$depot <- grepl(MOTIF_URL_DEPOT, ur$url, ignore.case = TRUE, perl = TRUE)
  ancre <- ur$depot & grepl(MOTIF_ANCRE_LIGNE, ur$url, perl = TRUE)
  sources <- rbind(data.frame(debut = ch$debut, fin = ch$fin, depot = ch$depot, chemin = ch$chemin, stringsAsFactors = FALSE),
                   data.frame(debut = ur$debut, fin = ur$fin, depot = ur$depot, chemin = ur$url, stringsAsFactors = FALSE))
  frontieres <- occurrences(MOTIF_FRONTIERE_PHRASE, plein_f)$fin
  # Renvois « l. N » : source nommée la plus proche dans la même phrase.
  rl <- occurrences(MOTIF_RENVOI_L, plein_f)
  vers <- vapply(rl$debut, function(p) {
    d0 <- max(c(0L, frontieres[frontieres < p]))
    s <- sources[sources$debut > d0 & sources$fin < p, , drop = FALSE]
    if (!nrow(s)) return(SANS_SOURCE)
    s <- s[which.max(s$debut), ]
    if (s$depot) s$chemin else NA_character_
  }, "")
  # Renvois attachés au chemin : « chemin:N », « chemin, ligne N » et « ligne(s) N de chemin ».
  suffixe <- vapply(seq_len(nrow(ch)), function(i)
    ch$depot[i] && grepl(MOTIF_RENVOI_SUFFIXE, substring(plein_f, ch$fin[i] + 1L, ch$fin[i] + 40L), perl = TRUE), NA)
  ligne_de <- vapply(seq_len(nrow(ch)), function(i)
    ch$depot[i] && grepl(MOTIF_RENVOI_LIGNE_DE, substring(plein_f, max(1L, ch$debut[i] - 80L), ch$debut[i] - 1L), perl = TRUE), NA)
  debut <- c(rl$debut[!is.na(vers)], ch$debut[suffixe | ligne_de], ur$debut[ancre])
  chemin <- c(vers[!is.na(vers)], ch$chemin[suffixe | ligne_de], ur$url[ancre])
  i <- findInterval(debut, debuts)
  data.frame(fichier = rep(f$fichier[1L], length(debut)), ligne = f$ligne[i],
             texte = tronquer(sous_texte(f$texte[i], debut - debuts[i] + 1L), 60L),
             chemin = chemin, stringsAsFactors = FALSE)[order(debut), , drop = FALSE]
}
renvois <- local({
  r <- lapply(unique(lignes$fichier), function(fi) renvois_de_ligne(lignes[lignes$fichier == fi, , drop = FALSE]))
  if (length(r)) do.call(rbind, r) else data.frame(fichier = character(0), ligne = integer(0), texte = character(0),
                                                    chemin = character(0))
})
cat(sprintf("Renvois par numéro de ligne : %d\n", nrow(renvois)))
r_renvois <- if (!nrow(renvois)) TRUE else
  sprintf("%d renvoi(s) par numéro de ligne vers un fichier du dépôt ou sans source nommée : %s", nrow(renvois),
          paste(sprintf("%s ligne %d « %s » %s", renvois$fichier, renvois$ligne, renvois$texte,
                        ifelse(renvois$chemin == SANS_SOURCE, paste(":", SANS_SOURCE), paste("vers", renvois$chemin))),
                collapse = " ; "))
ligne_renvoi <- attente_renvoi[attente_renvoi$nom == "lignes", , drop = FALSE]
if (nrow(ligne_renvoi) == 1L && grepl(MOTIF_ISSUE, ligne_renvoi$issue)) {
  echec_attendu("aucun renvoi par numéro de ligne vers un fichier du dépôt", ligne_renvoi$issue,
                if (isTRUE(r_renvois)) TRUE else sprintf("%s (%s)", r_renvois, ligne_renvoi$motif))
} else {
  verifier("aucun renvoi par numéro de ligne vers un fichier du dépôt", r_renvois)
}

if (!STRICT) cat("\nMode rapport : code de sortie 0 même en cas d'écart (--strict pour échouer).\n")
bilan(code_sortie = STRICT)
