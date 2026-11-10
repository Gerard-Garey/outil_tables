# -----------------------------------------------------------------------------
#  tests/controles/graphe_modules.R : graphe d'appels des modules de R/
#  (ADR 0007, points 2.1 à 2.3 ; issue #8), en R de base, sans codetools
#  (priorité recommended, exclu par l'ADR 0002)
#
#  Méthode (ADR 0007, point 2.2) :
#    1. chaque fichier R/<module>.R est lu par parse(keep.source = FALSE), en
#       UTF-8 quelle que soit la locale (analyser_fichier(), #206) ;
#       un fichier dont le nom n'est pas un module connu (MODULES) est un
#       écart ;
#    2. les affectations de premier niveau (`nom <- ...`, `nom = ...`,
#       fonctions comme constantes) donnent la table « symbole -> module » ;
#       un symbole défini dans deux modules est un écart ;
#    3. pour chaque définition, all.names() de sa valeur, intersecté avec la
#       table, donne les arêtes ; toute arête hors de APPELS_AUTORISES est un
#       écart, imprimé avec module appelant, module appelé, symbole appelant
#       et symbole appelé ;
#    4. appels dynamiques interdits (point 2.3), cherchés dans tout l'arbre
#       syntaxique, aussi sous forme qualifiée (base::, utils::, methods::) :
#       do.call() dont l'argument `what` (apparié par match.call(), position
#       ou nom) n'est ni un symbole ni une fonction anonyme (chaîne,
#       paste0(...), etc.) ; toute occurrence (appel, ou fonction passée
#       comme objet : lapply(x, get)) de match.fun, get, get0, mget,
#       getFromNamespace, getExportedValue, getFunction, asNamespace,
#       getNamespace (all.names()) ; eval()/evalq() d'un
#       parse()/str2lang()/str2expression()/call()/as.call(), `:::`
#       (interdit dans R/, point 11.3) et `tablexp::`. Par rapport à l'ADR
#       0007 (point 2.3), la liste est élargie (audit de #8) : formes
#       qualifiées, `what` non symbolique, call() et as.call() sous eval(),
#       toute occurrence des symboles d'appel dynamique et trois symboles
#       de plus (getFunction, asNamespace, getNamespace). Un nom local
#       homonyme (variable `get`, x$get) est aussi relevé : faux positif
#       possible, assumé.
#
#  Limites : (i) un nom local (variable, argument) homonyme d'un symbole
#  d'un autre module compte comme une arête (prudent : faux positif possible,
#  jamais d'arête manquée par homonymie) ; (ii) le code hors affectation de
#  premier niveau (appel nu au chargement) n'est pas attribué : il est un
#  écart ; (iii) les arêtes vers les packages de base ne sont pas
#  contrôlées ici (liste blanche de DESCRIPTION).
#
#  Usage : Rscript tests/controles/graphe_modules.R [dossier R/]
#  Sans argument : R/ à la racine du dépôt. Code de sortie 1 en cas d'écart.
#  Sourcé (tests), le fichier ne fait que définir les fonctions.
# -----------------------------------------------------------------------------

ORDRE_MODULES <- c("outils", "donnees", "exposition", "brut", "lissage",
                   "positionnement", "prospective", "fermeture", "validation",
                   "rapport")
MODULES <- c(ORDRE_MODULES, "simulation", "construire_table")

# Tableau 2.1 de l'ADR 0007 : module appelant -> modules qu'il peut appeler.
APPELS_AUTORISES <- c(
  stats::setNames(lapply(seq_along(ORDRE_MODULES),
                         function(i) ORDRE_MODULES[seq_len(i - 1L)]),
                  ORDRE_MODULES),
  list(simulation = c("donnees", "outils"),
       construire_table = ORDRE_MODULES)
)

# Toute occurrence de ces symboles est interdite, appel ou fonction passée
# comme objet (lapply(x, get)) : élargissement issu de l'audit de #8 (R3),
# avec getFunction, asNamespace et getNamespace.
APPELS_DYNAMIQUES <- c("match.fun", "get", "get0", "mget", "getFromNamespace",
                       "getExportedValue", "getFunction", "asNamespace", "getNamespace")
# Constructeurs d'appel interdits sous eval()/evalq() (élargissement de
# l'ADR 0007, point 2.3, issu de l'audit de #8 : call() et as.call()).
CONSTRUCTEURS_EVAL <- c("parse", "str2lang", "str2expression", "call", "as.call")
# Packages dont la forme qualifiée pkg::f / pkg:::f est lue comme f.
PACKAGES_QUALIFIES <- c("base", "utils", "methods")

# Élément i d'un appel ou d'une liste d'arguments, ou NULL s'il est vide
# (argument manquant d'une liste formelle).
.element <- function(x, i) {
  v <- x[[i]]
  if (missing(v)) NULL else v
}

# Nom de la fonction appelée par la tête `tete` d'un appel : le symbole, ou,
# pour base::f, utils::f, methods::f (et :::), le nom f ; "" sinon.
nom_tete <- function(tete) {
  if (is.symbol(tete)) return(as.character(tete))
  if (is.call(tete) && is.symbol(tete[[1L]]) && as.character(tete[[1L]]) %in% c("::", ":::") &&
      length(tete) == 3L && as.character(tete[[2L]]) %in% PACKAGES_QUALIFIES)
    return(as.character(tete[[3L]]))
  ""
}

# Argument `arg` de l'appel x, apparié comme le ferait R pour la fonction
# `f` (positionnel ou nommé) ; NULL s'il est absent ou si l'appariement
# échoue.
argument_apparie <- function(x, f, arg) {
  x[[1L]] <- as.name("f_appariee")
  mc <- tryCatch(match.call(f, x), error = function(e) NULL)
  if (is.null(mc)) return(NULL)
  .element(as.list(mc), arg)
}

# Parcourt récursivement l'expression e et rend les constructions
# dynamiques interdites (chaînes descriptives).
appels_interdits <- function(e) {
  trouves <- character(0)
  visiter <- function(x) {
    if (is.call(x)) {
      tete <- x[[1L]]
      nom <- nom_tete(tete)
      n <- length(x)
      premier <- if (n >= 2L) .element(x, 2L) else NULL
      if (nom == "do.call") {
        quoi <- argument_apparie(x, base::do.call, "what")
        # Admis : un symbole (objet fonction nommé) ou une fonction anonyme.
        admis <- is.symbol(quoi) ||
          (is.call(quoi) && is.symbol(quoi[[1L]]) && as.character(quoi[[1L]]) == "function")
        if (!admis)
          trouves <<- c(trouves, sprintf("do.call(what = %s, ...)",
                                         paste(deparse(quoi), collapse = " ")))
      }
      if (is.symbol(tete) && nom == ":::")
        trouves <<- c(trouves, paste(deparse(x), collapse = " "))
      if (is.symbol(tete) && nom == "::" && identical(as.character(premier), "tablexp"))
        trouves <<- c(trouves, paste(deparse(x), collapse = " "))
      if (nom %in% c("eval", "evalq")) {
        quoi <- argument_apparie(x, if (nom == "eval") base::eval else base::evalq, "expr")
        if (is.call(quoi) && nom_tete(quoi[[1L]]) %in% CONSTRUCTEURS_EVAL)
          trouves <<- c(trouves, sprintf("%s(%s(...))", nom, nom_tete(quoi[[1L]])))
      }
      for (i in seq_len(n)) { v <- .element(x, i); if (!is.null(v)) visiter(v) }
    } else if (is.pairlist(x) || is.expression(x) || is.list(x)) {
      for (i in seq_along(x)) { v <- .element(x, i); if (!is.null(v)) visiter(v) }
    }
  }
  visiter(e)
  # Symboles d'appel dynamique, en tête d'appel ou comme objet (all.names()
  # voit aussi la forme qualifiée base::get).
  dyn <- intersect(all.names(e), APPELS_DYNAMIQUES)
  unique(c(trouves, sprintf("%s() (appel, ou fonction passée comme objet)", dyn)))
}

# Analyse syntaxique d'un fichier de R/ (UTF-8), indépendante de la locale
# native (#206). En locale UTF-8 : parse(fichier, encoding = "UTF-8"). Sinon
# (mesuré sous LC_ALL=C), parse() ne peut pas traduire vers l'encodage natif
# les littéraux non ASCII : il avertit et les altère (« <U+00E9> »), sans
# refus. Le fichier est alors lu en octets UTF-8 (validUTF8() : sinon erreur),
# chaque caractère non ASCII est réécrit en échappement \U{...} (le parseur
# l'interprète sans traduction : chaîne ordinaire lue comme en UTF-8 ; sans
# effet dans un commentaire), puis analysé en texte ASCII ; les numéros de
# ligne sont conservés. La réécriture lit autrement qu'en UTF-8 deux
# constructions, refusées (écart « illisible », controler_non_ascii()) : un
# caractère non ASCII dans une chaîne brute (r"(...)"), et un caractère non
# ASCII précédé d'une barre oblique inverse dans une chaîne (échappement
# invalide en UTF-8). Limites : un nom (symbole, ou chaîne en position de nom
# d'argument : "délais" = ...) ne peut pas être natif hors UTF-8 ; parse() le
# traduit en « <U+00E9> » et avertit, avertissement écarté ici (seul son
# marqueur « <U+ » est écarté, tout autre avertissement passe). Un symbole
# non ASCII hors chaîne (nom de définition, d'appel) est refusé par le
# parseur : écart « illisible ». Divergence résiduelle : un symbole non ASCII
# entre accents graves est accepté en UTF-8 et refusé hors UTF-8 (parse() :
# « \Uxxxxxxxx sequences not supported inside backticks »), jamais accepté à
# tort hors UTF-8.
# Un appel d'opérateur non ASCII (a %é% b) est lu hors UTF-8 sous le nom
# littéral « %\U{e9}% » ; aucun tel opérateur ne peut être défini hors UTF-8
# (forme entre accents graves refusée par parse()) et R/ n'en contient aucun.
# Aucune lecture de ce script ne dépend d'un nom non ASCII en position de nom
# d'argument.
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
MARQUEUR_NON_ASCII <- "QNONASCIIQ"
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

# Analyse le dossier R/ ; rend list(defauts, aretes, symboles).
analyser_modules <- function(dossier) {
  defauts <- character(0)
  fichiers <- sort(list.files(dossier, pattern = "\\.[Rr]$", full.names = TRUE))
  if (!length(fichiers)) return(list(defauts = sprintf("aucun fichier R dans %s", dossier),
                                     aretes = NULL, symboles = NULL))
  defs <- data.frame(module = character(0), symbole = character(0),
                     stringsAsFactors = FALSE)
  valeurs <- list()
  for (f in fichiers) {
    module <- sub("\\.[Rr]$", "", basename(f))
    if (!(module %in% MODULES)) {
      defauts <- c(defauts, sprintf("%s : fichier hors des modules connus (ADR 0007, point 1.2)",
                                    basename(f)))
      next
    }
    exprs <- tryCatch(analyser_fichier(f, keep.source = FALSE),
                      error = function(e) e)
    if (inherits(exprs, "error")) {
      defauts <- c(defauts, sprintf("%s : illisible (%s)", basename(f), conditionMessage(exprs)))
      next
    }
    for (e in exprs) {
      interdits <- appels_interdits(e)
      if (length(interdits))
        defauts <- c(defauts, sprintf("%s : appel dynamique interdit (ADR 0007, point 2.3) : %s",
                                      module, interdits))
      if (is.call(e) && is.symbol(e[[1L]]) && as.character(e[[1L]]) %in% c("<-", "=") &&
          is.symbol(e[[2L]])) {
        defs[nrow(defs) + 1L, ] <- list(module, as.character(e[[2L]]))
        valeurs[[length(valeurs) + 1L]] <- e[[3L]]
      } else {
        defauts <- c(defauts, sprintf("%s : expression de premier niveau qui n'est pas une affectation : %s",
                                      module, substr(paste(deparse(e), collapse = " "), 1L, 80L)))
      }
    }
  }
  doubles <- unique(defs$symbole[duplicated(defs$symbole)])
  for (s in doubles)
    defauts <- c(defauts, sprintf("symbole %s défini plusieurs fois (%s)", s,
                                  paste(defs$module[defs$symbole == s], collapse = ", ")))
  aretes <- data.frame(de = character(0), vers = character(0), appelant = character(0),
                       appele = character(0), stringsAsFactors = FALSE)
  for (i in seq_len(nrow(defs))) {
    noms <- intersect(all.names(valeurs[[i]]), defs$symbole)
    for (s in noms) {
      for (m in unique(defs$module[defs$symbole == s])) {
        if (m == defs$module[i]) next
        aretes[nrow(aretes) + 1L, ] <- list(defs$module[i], m, defs$symbole[i], s)
      }
    }
  }
  for (k in seq_len(nrow(aretes))) {
    if (!(aretes$vers[k] %in% APPELS_AUTORISES[[aretes$de[k]]]))
      defauts <- c(defauts, sprintf("appel interdit %s -> %s : %s() appelle %s (ADR 0007, point 2.1)",
                                    aretes$de[k], aretes$vers[k], aretes$appelant[k], aretes$appele[k]))
  }
  list(defauts = defauts, aretes = aretes, symboles = defs)
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  dossier <- if (length(args)) args[1L] else
    file.path(dirname(dirname(dirname(normalizePath(script)))), "R")
  cat("Graphe d'appels des modules :", dossier, "\n")
  r <- analyser_modules(dossier)
  if (!is.null(r$symboles))
    cat(sprintf("  %d symbole(s) de premier niveau dans %d module(s) ; %d arête(s) entre modules\n",
                nrow(r$symboles), length(unique(r$symboles$module)), nrow(r$aretes)))
  for (k in seq_len(NROW(r$aretes)))
    cat(sprintf("    %s -> %s (%s -> %s)\n", r$aretes$de[k], r$aretes$vers[k],
                r$aretes$appelant[k], r$aretes$appele[k]))
  if (length(r$defauts)) {
    cat(paste0("  ECHEC : ", r$defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Ordre des modules::", paste(r$defauts, collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("  OK : graphe d'appels conforme à l'ADR 0007, aucun appel dynamique interdit.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
