# -----------------------------------------------------------------------------
#  tests/controles/graphe_modules.R : graphe d'appels des modules de R/
#  (ADR 0007, points 2.1 à 2.3 ; issue #8), en R de base, sans codetools
#  (priorité recommended, exclu par l'ADR 0002)
#
#  Méthode (ADR 0007, point 2.2) :
#    1. chaque fichier R/<module>.R est lu par parse(keep.source = FALSE) ;
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
#       paste0(...), etc.), match.fun(), get(), get0(), mget(),
#       getFromNamespace(), getExportedValue(), eval()/evalq() d'un
#       parse()/str2lang()/str2expression()/call()/as.call(), `:::`
#       (interdit dans R/, point 11.3) et `tablexp::`. Par rapport à l'ADR
#       0007 (point 2.3), la liste est élargie (audit de #8) : formes
#       qualifiées, `what` non symbolique, call() et as.call() sous eval().
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

APPELS_DYNAMIQUES <- c("match.fun", "get", "get0", "mget", "getFromNamespace",
                       "getExportedValue")
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
      if (nom %in% APPELS_DYNAMIQUES)
        trouves <<- c(trouves, sprintf("%s()", nom))
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
  unique(trouves)
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
    exprs <- tryCatch(parse(f, keep.source = FALSE, encoding = "UTF-8"),
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
