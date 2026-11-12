# -----------------------------------------------------------------------------
#  tests/controles/lecture_utf8.R : lecture d'un fichier R en UTF-8 quelle que
#  soit la locale native (#206, #213 ; fichier commun des lectures, #228)
#
#  Définit analyser_fichier(fichier, keep.source) et ses auxiliaires
#  (session_utf8(), remplacer_non_ascii(), controler_non_ascii(),
#  source_ascii(), MARQUEUR_NON_ASCII). Chargé par sys.source() dans
#  l'environnement de l'appelant par les contrôles de tests/controles/ qui lisent
#  des fichiers R (graphe_modules.R, recettes_invariant.R, messages_refus.R,
#  graines.R), par tests/test_unitaires.R et par
#  tests/unitaires/test_controles_portables.R. Une seule définition : ces six
#  lecteurs étaient auparavant des copies, que rien ne contrôlait.
#  Ne sont recopiés que les dix lignes du chargeur local({...}) de chacun des quatre
#  contrôles (recherche du chemin de ce fichier) : leur identité est contrôlée par
#  tests/unitaires/test_controles_portables.R.
#
#  Ce fichier est lui-même lu en locale native, comme les contrôles : son code
#  n'a de caractère non ASCII que dans des chaînes (messages) et des
#  commentaires, qui ne dépendent pas de la locale. Il n'appelle aucune
#  fonction de tablexp ni des autres scripts. Les remarques propres à chaque
#  appelant (noms non ASCII, limites de lecture) sont dans son en-tête.
# -----------------------------------------------------------------------------

# Analyse syntaxique d'un fichier R (UTF-8), indépendante de la locale
# native (#213, suite de #206). En locale UTF-8 : parse(fichier, encoding =
# "UTF-8"). Sinon (mesuré sous LC_ALL=C, R 4.3.3), ce même appel accepte sans
# rien dire un fichier dont les octets ne sont pas de l'UTF-8 valide (en UTF-8,
# refusé dans un littéral : « EOF whilst reading MBCS char ») et avertit
# « unable to translate » pour chaque nom non ASCII (chaîne en position de nom
# d'argument : "délais" = ...), qu'il altère en « <U+00E9> » ; les littéraux non
# ASCII restent intacts. Le fichier est donc lu en octets UTF-8 (validUTF8() :
# sinon erreur) ; chaque caractère non ASCII est réécrit en échappement \U{...} (le
# parseur l'interprète sans traduction : chaîne ordinaire lue comme en UTF-8 ;
# sans effet dans un commentaire), puis le texte ASCII est analysé ; les
# numéros de ligne sont conservés. La réécriture lit autrement qu'en UTF-8 deux
# constructions, refusées (controler_non_ascii()) : un caractère non ASCII dans
# une chaîne brute (r"(...)"), et un caractère non ASCII précédé d'une barre
# oblique inverse dans une chaîne (échappement invalide en UTF-8). Limites : un
# nom (symbole, ou chaîne en position de nom d'argument) ne peut pas être natif
# hors UTF-8 ; parse() le traduit en « <U+00E9> » et avertit, avertissement
# écarté ici (seul son marqueur « <U+ » est écarté, tout autre avertissement
# passe). Un symbole non ASCII hors chaîne (nom de définition, d'appel) est
# refusé par le parseur. Divergence résiduelle : un symbole non ASCII entre
# accents graves est accepté en UTF-8 et refusé hors UTF-8 (parse() :
# « \Uxxxxxxxx sequences not supported inside backticks »), jamais accepté à
# tort hors UTF-8.
# Autre divergence (#213, constat d'audit) : dans une même chaîne,
# un échappement \x.. ou octal suivi d'un caractère non ASCII est refusé
# hors UTF-8 (« mixing Unicode and octal/hex escapes in a string is not
# allowed ») et accepté en UTF-8 : refus, jamais acceptation à tort.
# Un appel d'opérateur non ASCII (a %é% b) est lu hors UTF-8
# sous le nom littéral « %\U{e9}% » ; aucun tel opérateur ne peut être défini
# hors UTF-8 et le dépôt n'en contient aucun.
session_utf8 <- function() isTRUE(l10n_info()[["UTF-8"]])
# Remplace chaque caractère non ASCII d'un vecteur de lignes UTF-8 par
# `par(points de code)`.
remplacer_non_ascii <- function(l, par) {
  na <- grepl("[^\\001-\\177]", l, perl = TRUE, useBytes = TRUE)
  l[na] <- vapply(l[na], function(s) {
    cp <- utf8ToInt(s)
    paste(ifelse(cp > 127L, par(cp), intToUtf8(cp, multiple = TRUE)), collapse = "")
  }, "", USE.NAMES = FALSE)
  l
}
# Refus des deux constructions que la réécriture en \U{...} lirait autrement
# qu'en UTF-8 (#206) : un caractère non ASCII dans une chaîne brute
# (r"(...)", R'[...]', avec ou sans tirets), où \U{e9} resterait le texte
# « \U{e9} » ; un caractère non ASCII précédé d'une barre oblique inverse dans
# une chaîne (échappement invalide en UTF-8, valide une fois réécrit). Les
# lignes sont analysées une fois avec un marqueur ASCII à la place de chaque
# caractère non ASCII : une barre oblique inverse devant le marqueur est un
# échappement invalide (erreur du parseur) ; un jeton chaîne commençant par r
# ou R qui contient le marqueur est une chaîne brute non ASCII.
# Marqueur en deux morceaux : le texte de ce bloc ne le contient pas, il reste
# lisible par lui-même (graines.R lit les fichiers de tests/, #213).
MARQUEUR_NON_ASCII <- paste0("QNONASCII", "Q")
controler_non_ascii <- function(l) {
  if (any(grepl(MARQUEUR_NON_ASCII, l, fixed = TRUE)))
    stop("le texte contient le marqueur interne ", MARQUEUR_NON_ASCII, " : lecture impossible", call. = FALSE)
  e <- tryCatch(parse(text = remplacer_non_ascii(l, function(cp) MARQUEUR_NON_ASCII), keep.source = TRUE),
                error = function(e)
                  stop("échappement invalide devant un caractère non ASCII, ou syntaxe (non ASCII remplacé par ",
                       MARQUEUR_NON_ASCII, " pour l'analyse) : ", conditionMessage(e), call. = FALSE))
  pd <- utils::getParseData(e, includeText = NA)
  if (is.null(pd)) return(invisible(NULL))
  s <- pd[pd$token == "STR_CONST", , drop = FALSE]
  if (!nrow(s)) return(invisible(NULL))
  texte <- utils::getParseText(pd, s$id)
  brute <- grepl("^[rR]", texte) & grepl(MARQUEUR_NON_ASCII, texte, fixed = TRUE)
  if (any(brute))
    stop(sprintf("caractère non ASCII dans une chaîne brute (ligne %s) : lue hors UTF-8 comme du texte échappé, refusée",
                 paste(s$line1[brute], collapse = ", ")), call. = FALSE)
  invisible(NULL)
}
source_ascii <- function(fichier) {
  l <- readLines(fichier, encoding = "UTF-8", warn = FALSE)
  if (!all(validUTF8(l))) stop("octets non UTF-8 dans ", basename(fichier), call. = FALSE)
  if (any(grepl("[^\\001-\\177]", l, perl = TRUE, useBytes = TRUE))) controler_non_ascii(l)
  remplacer_non_ascii(l, function(cp) sprintf("\\U{%x}", cp))
}
analyser_fichier <- function(fichier, keep.source) {
  if (session_utf8()) return(parse(fichier, keep.source = keep.source, encoding = "UTF-8"))
  withCallingHandlers(parse(text = source_ascii(fichier), keep.source = keep.source),
                      warning = function(w)
                        if (grepl("<U+", conditionMessage(w), fixed = TRUE)) invokeRestart("muffleWarning"))
}
