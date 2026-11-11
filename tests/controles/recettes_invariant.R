# -----------------------------------------------------------------------------
#  tests/controles/recettes_invariant.R : « aucun module ne teste un nom de
#  recette » (ADR 0016, point 4.4 ; annotation du 10/11/2026, décision M380 ;
#  issue #194), en R de base, sans codetools (priorité recommended, exclu par
#  l'ADR 0002)
#
#  Méthode (annotation du 10/11/2026, (b) à (d)) :
#    1. chaque fichier de R/ est lu par parse(keep.source = TRUE) (en UTF-8,
#       quelle que soit la locale : analyser_fichier(), #206) et
#       utils::getParseData() ; les noms des recettes sont lus dans l'arbre de
#       R/donnees.R, à l'argument `recette = c(...)` de l'affectation de
#       premier niveau `CATALOGUE_RECETTES <- data.frame(...)`, jamais
#       recopiés ; leur absence, ou une valeur qui n'est pas un vecteur
#       littéral de chaînes, est un écart ;
#    2. trois règles, relevées dans tout l'arbre :
#         R1 (sujet)      : switch(E, ...) dont le sujet E se termine par
#                           $recette ou [["recette"]] ;
#         R2 (cas)        : dans un switch, étiquette de cas (symbole ou
#                           chaîne) égale à un nom de recette ;
#         R3 (comparaison): opérande de ==, != ou %in%, ou argument de
#                           identical(), match() ou is.element(), qui est un
#                           littéral égal à un nom de recette (seul ou dans
#                           c(...)) ou une expression terminée par $recette ou
#                           [["recette"]] ;
#       aucune règle ne vise un littéral isolé (clés, familles de rôles) ;
#       lire recette$recette pour l'inscrire (paramètres, fiche, messages)
#       n'est relevé par aucune règle ; des parenthèses autour d'un sujet ou
#       d'un opérande sont traversées ;
#    3. chaque occurrence est attribuée à sa fonction englobante : la
#       définition de premier niveau (`nom <- ...`, `nom = ...`) qui la
#       contient, déterminée par l'arbre (une fonction locale appartient à sa
#       définition de premier niveau ; `<premier niveau>` hors affectation) ;
#    4. une occurrence est admise si LIEUX_ADMIS (liste fermée, par fichier et
#       fonction, jamais par numéro de ligne) l'admet pour sa règle. Un écart
#       est : une occurrence hors lieu admis, une règle non admise dans un
#       lieu admis, ou une admission devenue inutile (lieu où plus aucune
#       occurrence de ses règles n'est relevée). Ajouter, retirer ou étendre
#       un lieu admis demande une annotation de l'ADR 0016.
#
#  Limite (annotation, (d)) : le contrôle est syntaxique ; une indirection par
#  variable (nom de recette porté par une variable intermédiaire, puis
#  comparé ou passé à un switch sous un autre nom) lui échappe. Il en va de
#  même des formes suivantes, non traitées : le pipe natif (r$recette |>
#  switch(...), "agregats" |> identical(x)), un opérateur écrit en appel
#  (`==`(r$recette, "agregats")), un argument nommé dans [[ (r[["recette",
#  exact = TRUE]]) et les fonctions de chaîne ou de comparaison hors de la
#  liste fermée de R3 (grepl, startsWith, all.equal, etc.). L'audit reste en
#  appui sur ces points.
#
#  Usage : Rscript tests/controles/recettes_invariant.R [dossier R/]
#  Sans argument : R/ à la racine du dépôt. Un écart par ligne (fichier:ligne,
#  fonction, règle, jeton) ; code de sortie 1 en cas d'écart, 0 sinon.
#  Sourcé (tests), le fichier ne fait que définir ses fonctions.
# -----------------------------------------------------------------------------

# Lieux admis (annotation du 10/11/2026, (c)) : un lieu = un fichier et une
# fonction englobante de premier niveau ("*" : tout le fichier) ; `regles` :
# règles admises, séparées par des virgules.
LIEUX_ADMIS <- data.frame(
  fichier = c("donnees.R", "exposition.R", "construire_table.R", "simulation.R", "simulation.R"),
  fonction = c("*", "exposition_fenetres", "controler_parametres_recette", "controler_forme",
               "simuler_portefeuille"),
  regles = c("R1,R2,R3", "R1,R2", "R3", "R3", "R3"),
  justification = c(
    "point 4.4 : catalogue, matrice, preparer_donnees()",
    "point 4.1 : second switch de l'aiguillage ; annotation du 09/11/2026, (c)",
    "recherche par clé dans PARAMETRES_ETAPES$recette (point 7.2) ; annotation du 09/11/2026, (d)",
    "forme du simulateur (ADR 0003, point 9), non l'objet recette ; annotation du 10/11/2026, (a)",
    "forme du simulateur (ADR 0003, point 9), non l'objet recette ; annotation du 10/11/2026, (a)"),
  stringsAsFactors = FALSE)

FICHIER_CATALOGUE <- "donnees.R"
NOM_CATALOGUE <- "CATALOGUE_RECETTES"
FONCTIONS_COMPARAISON <- c("identical", "match", "is.element")
PREMIER_NIVEAU <- "<premier niveau>"

# Valeur d'un littéral de chaîne (texte du jeton, guillemets compris), NA si
# ce n'en est pas un.
valeur_chaine <- function(texte) {
  v <- tryCatch(eval(parse(text = texte, keep.source = FALSE)[[1L]]), error = function(e) NA_character_)
  if (is.character(v) && length(v) == 1L) v else NA_character_
}

# Nom d'un symbole sans ses accents graves (`nom` -> nom).
sans_accents_graves <- function(texte) sub("^`(.*)`$", "\\1", texte)

# Arbre d'un fichier : rend des fermetures sur le tableau de getParseData().
# Les identifiants sont ceux de getParseData() ; les enfants sont rendus dans
# l'ordre du texte, commentaires exclus.
creer_arbre <- function(pd) {
  pd <- pd[pd$token != "COMMENT", , drop = FALSE]
  fils <- split(seq_len(nrow(pd)), pd$parent)
  ligne_de <- function(i) pd$line1[match(i, pd$id)]
  enfants <- function(id) {
    k <- fils[[as.character(id)]]
    if (is.null(k)) return(integer(0))
    k <- k[order(pd$line1[k], pd$col1[k])]
    pd$id[k]
  }
  jeton <- function(id) pd$token[match(id, pd$id)]
  texte_terminal <- function(id) pd$text[match(id, pd$id)]
  texte <- function(id) {
    if (pd$terminal[match(id, pd$id)]) return(texte_terminal(id))
    paste(vapply(enfants(id), texte, ""), collapse = "")
  }
  # Traverse les parenthèses : ( expr ) -> expr.
  deballer <- function(id) {
    repeat {
      k <- enfants(id)
      if (length(k) == 3L && jeton(k[1L]) == "'('" && jeton(k[3L]) == "')'") id <- k[2L] else return(id)
    }
  }
  # Valeur du littéral de chaîne que porte le nœud (expr d'un seul STR_CONST), sinon NA.
  litteral <- function(id) {
    id <- deballer(id)
    k <- enfants(id)
    if (jeton(id) == "expr" && length(k) == 1L && jeton(k) == "STR_CONST")
      valeur_chaine(texte_terminal(k)) else NA_character_
  }
  # Le nœud se termine-t-il par $recette ou [["recette"]] ?
  accesseur_recette <- function(id) {
    id <- deballer(id)
    k <- enfants(id)
    if (jeton(id) != "expr") return(FALSE)
    if (length(k) == 3L && jeton(k[2L]) == "'$'" && jeton(k[3L]) %in% c("SYMBOL", "STR_CONST")) {
      nom <- texte_terminal(k[3L])
      nom <- if (jeton(k[3L]) == "STR_CONST") valeur_chaine(nom) else sans_accents_graves(nom)
      return(identical(nom, "recette"))
    }
    length(k) == 5L && jeton(k[2L]) == "LBB" && identical(litteral(k[3L]), "recette") &&
      jeton(k[4L]) == "']'" && jeton(k[5L]) == "']'"
  }
  # Arguments de l'appel dont le jeton SYMBOL_FUNCTION_CALL est `id` (appel
  # nu ou qualifié pkg::f) : liste de list(nom, id_nom, expr), `expr` valant
  # NULL pour un argument vide ; `appel` est le nœud de l'appel.
  arguments <- function(id) {
    appel <- pd$parent[match(pd$parent[match(id, pd$id)], pd$id)]
    k <- enfants(appel)
    debut <- which(vapply(k, jeton, "") == "'('")[1L]
    args <- list()
    nouveau <- function() list(nom = NA_character_, id_nom = NA_integer_, expr = NULL)
    cour <- nouveau()
    vide <- TRUE
    clore <- function() { args[[length(args) + 1L]] <<- cour; cour <<- nouveau() }
    for (j in k[-seq_len(debut)]) {
      t <- jeton(j)
      if (t == "','") { clore(); vide <- FALSE }
      else if (t == "')'") { if (!is.null(cour$expr) || !is.na(cour$nom) || !vide) clore() }
      else if (t == "EQ_SUB") next
      else if (t %in% c("SYMBOL_SUB", "STR_CONST", "NULL_CONST")) {
        nom <- texte_terminal(j)
        cour$nom <- if (t == "STR_CONST") valeur_chaine(nom) else sans_accents_graves(nom)
        cour$id_nom <- j
      } else if (t == "expr") cour$expr <- j
    }
    list(appel = appel, args = args)
  }
  # Définition de premier niveau qui contient le nœud : son nom.
  fonction_englobante <- function(id) {
    while (TRUE) {
      p <- pd$parent[match(id, pd$id)]
      if (is.na(p) || p <= 0L) break
      id <- p
    }
    k <- enfants(id)
    if (length(k) >= 3L && jeton(k[2L]) %in% c("LEFT_ASSIGN", "EQ_ASSIGN")) {
      g <- enfants(k[1L])
      if (length(g) == 1L && jeton(g) == "SYMBOL") return(sans_accents_graves(texte_terminal(g)))
      if (length(g) == 1L && jeton(g) == "STR_CONST") return(valeur_chaine(texte_terminal(g)))
    }
    PREMIER_NIVEAU
  }
  list(pd = pd, enfants = enfants, jeton = jeton, texte = texte, texte_terminal = texte_terminal,
       ligne = ligne_de, deballer = deballer, litteral = litteral, accesseur_recette = accesseur_recette,
       arguments = arguments, fonction_englobante = fonction_englobante)
}

# Analyse syntaxique d'un fichier de R/ (UTF-8), indépendante de la locale
# native (#206). En locale UTF-8 : parse(fichier, encoding = "UTF-8"). Sinon
# (mesuré sous LC_ALL=C, R 4.3.3), parse() accepte sans rien dire un fichier
# dont les octets ne sont pas de l'UTF-8 valide, et altère en « <U+00E9> »
# les noms non ASCII (chaîne en position de nom d'argument), avec un
# avertissement ; les littéraux non ASCII restent intacts (#213). Le fichier est alors lu
# en octets UTF-8 (validUTF8() : sinon erreur),
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
# Autre divergence (#213, constat d'audit) : dans une même chaîne,
# un échappement \x.. ou octal suivi d'un caractère non ASCII est refusé
# hors UTF-8 (« mixing Unicode and octal/hex escapes in a string is not
# allowed ») et accepté en UTF-8 : refus, jamais acceptation à tort.
# Un appel d'opérateur non ASCII (a %é% b) est lu hors UTF-8 sous le nom
# littéral « %\U{e9}% » ; aucun tel opérateur ne peut être défini hors UTF-8
# (forme entre accents graves refusée par parse()) et R/ n'en contient aucun.
# Une étiquette de cas écrite en chaîne non ASCII (règle R2) est lue
# correctement (getParseData() + valeur_chaine() : mesuré, mêmes occurrences
# sous LC_ALL=C et sous UTF-8) ; seuls les names() de l'objet langage que
# produit parse() sont altérés, et ce script ne s'en sert pas. Par prudence,
# analyser_recettes() refuse néanmoins, hors locale UTF-8, un nom de recette
# non ASCII.
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

# Lit et analyse un fichier : rend l'arbre, ou une erreur.
lire_arbre <- function(fichier) {
  exprs <- tryCatch(analyser_fichier(fichier, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) return(exprs)
  pd <- utils::getParseData(exprs, includeText = NA)
  if (is.null(pd)) return(simpleError("aucun arbre d'analyse (fichier vide ?)"))
  creer_arbre(pd)
}

# Noms des recettes, lus à l'argument `recette = c(...)` de
# `CATALOGUE_RECETTES <- data.frame(...)` (premier niveau de donnees.R).
# Rend list(noms, defaut) : `defaut` est un message en cas d'absence ou de
# valeur non littérale.
noms_recettes <- function(dossier) {
  f <- file.path(dossier, FICHIER_CATALOGUE)
  absent <- function(raison) list(noms = character(0),
                                  defaut = sprintf("%s : %s (ADR 0016, point 4.4)", NOM_CATALOGUE, raison))
  if (!file.exists(f)) return(absent(sprintf("%s introuvable dans %s", FICHIER_CATALOGUE, dossier)))
  a <- lire_arbre(f)
  if (inherits(a, "error")) return(absent(sprintf("%s illisible (%s)", FICHIER_CATALOGUE, conditionMessage(a))))
  racines <- a$pd$id[a$pd$parent == 0L]
  for (r in racines) {
    k <- a$enfants(r)
    if (length(k) < 3L || !(a$jeton(k[2L]) %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))) next
    g <- a$enfants(k[1L])
    if (!(length(g) == 1L && a$jeton(g) == "SYMBOL" && a$texte_terminal(g) == NOM_CATALOGUE)) next
    # Valeur : un appel ; l'argument nommé `recette` doit être c(...) de chaînes littérales.
    h <- a$enfants(a$enfants(k[3L])[1L])
    sfc <- h[vapply(h, a$jeton, "") == "SYMBOL_FUNCTION_CALL"]
    if (!length(sfc)) return(absent("la valeur n'est pas un appel de fonction (data.frame(recette = c(...)) attendu)"))
    av <- a$arguments(sfc[1L])
    for (x in av$args) {
      if (!identical(x$nom, "recette") || is.null(x$expr)) next
      e <- a$deballer(x$expr)
      ke <- a$enfants(e)
      unique_lit <- a$litteral(e)
      if (!is.na(unique_lit)) return(list(noms = unique_lit, defaut = NULL))
      if (length(ke) >= 3L && a$jeton(ke[1L]) == "expr") {
        tete <- a$enfants(ke[1L])
        if (length(tete) == 1L && a$jeton(tete) == "SYMBOL_FUNCTION_CALL" && a$texte_terminal(tete) == "c") {
          ac <- a$arguments(tete)$args
          vals <- vapply(ac, function(y) if (is.null(y$expr)) NA_character_ else a$litteral(y$expr), "")
          if (length(vals) && !anyNA(vals) && !anyDuplicated(vals))
            return(list(noms = vals, defaut = NULL))
        }
      }
      return(absent("l'argument `recette` n'est pas un vecteur littéral de chaînes distinctes"))
    }
    return(absent("l'appel n'a pas d'argument nommé `recette`"))
  }
  absent("affectation de premier niveau absente")
}

# Occurrences des règles R1 à R3 dans un fichier : data.frame(fichier, ligne,
# fonction, regle, jeton), ou une erreur de lecture.
occurrences_fichier <- function(fichier, noms) {
  a <- lire_arbre(fichier)
  if (inherits(a, "error")) return(a)
  pd <- a$pd
  occ <- list()
  noter <- function(id, regle, jeton, ligne = a$ligne(id))
    occ[[length(occ) + 1L]] <<- data.frame(fichier = basename(fichier), ligne = ligne,
                                           fonction = a$fonction_englobante(id), regle = regle,
                                           jeton = substr(jeton, 1L, 60L), stringsAsFactors = FALSE)
  # R1, R2 : appels de switch (nus ou qualifiés).
  for (s in pd$id[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "switch"]) {
    av <- a$arguments(s)
    args <- av$args
    if (!length(args)) next
    noms_args <- vapply(args, function(x) x$nom, "")
    isujet <- if (any(noms_args == "EXPR", na.rm = TRUE)) which(noms_args == "EXPR")[1L] else 1L
    sujet <- args[[isujet]]$expr
    if (!is.null(sujet) && a$accesseur_recette(sujet)) noter(sujet, "R1", a$texte(a$deballer(sujet)))
    for (j in seq_along(args)[-isujet]) {
      if (!is.na(args[[j]]$nom) && args[[j]]$nom %in% noms)
        noter(args[[j]]$id_nom, "R2", args[[j]]$nom)
    }
  }
  # R3 : opérande d'un opérateur de comparaison ou argument de identical(), match(), is.element().
  operande <- function(id) {
    id <- a$deballer(id)
    if (a$accesseur_recette(id)) return(noter(id, "R3", a$texte(id)))
    v <- a$litteral(id)
    if (!is.na(v)) { if (v %in% noms) noter(id, "R3", a$texte(id)); return(invisible()) }
    k <- a$enfants(id)
    if (a$jeton(id) == "expr" && length(k) >= 3L && a$jeton(k[1L]) == "expr") {
      tete <- a$enfants(k[1L])
      if (length(tete) == 1L && a$jeton(tete) == "SYMBOL_FUNCTION_CALL" && a$texte_terminal(tete) == "c")
        for (y in a$arguments(tete)$args)
          if (!is.null(y$expr)) {
            w <- a$litteral(y$expr)
            if (!is.na(w) && w %in% noms) noter(y$expr, "R3", a$texte(a$deballer(y$expr)))
          }
    }
  }
  ops <- pd$id[pd$token %in% c("EQ", "NE") | (pd$token == "SPECIAL" & pd$text == "%in%")]
  for (o in ops) {
    n <- pd$parent[match(o, pd$id)]
    k <- a$enfants(n)
    if (length(k) != 3L) next
    for (x in k[c(1L, 3L)]) operande(x)
  }
  for (s in pd$id[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% FONCTIONS_COMPARAISON])
    for (x in a$arguments(s)$args) if (!is.null(x$expr)) operande(x$expr)
  if (!length(occ)) return(data.frame(fichier = character(0), ligne = integer(0), fonction = character(0),
                                      regle = character(0), jeton = character(0), stringsAsFactors = FALSE))
  occ <- do.call(rbind, occ)
  occ[!duplicated(occ), , drop = FALSE]
}

# Analyse le dossier R/ ; rend list(defauts, occurrences, noms).
analyser_recettes <- function(dossier, lieux = LIEUX_ADMIS) {
  defauts <- character(0)
  nr <- noms_recettes(dossier)
  if (!is.null(nr$defaut)) return(list(defauts = nr$defaut, occurrences = NULL, noms = nr$noms))
  # Hors locale UTF-8, un nom de recette non ASCII est refusé par prudence :
  # la lecture des étiquettes de cas est mesurée correcte (#206), mais les
  # names() de l'objet langage, eux, sont altérés par parse().
  if (!session_utf8() && any(grepl("[^ -~]", nr$noms, useBytes = TRUE)))
    return(list(defauts = sprintf("nom de recette non ASCII (%s) : refus de prudence hors locale UTF-8 (lecture mesurée correcte, #206)",
                                  paste(nr$noms[grepl("[^ -~]", nr$noms, useBytes = TRUE)], collapse = ", ")),
                occurrences = NULL, noms = nr$noms))
  fichiers <- sort(list.files(dossier, pattern = "\\.[Rr]$", full.names = TRUE))
  if (!length(fichiers)) return(list(defauts = sprintf("aucun fichier R dans %s", dossier),
                                     occurrences = NULL, noms = nr$noms))
  occ <- NULL
  for (f in fichiers) {
    o <- occurrences_fichier(f, nr$noms)
    if (inherits(o, "error")) {
      defauts <- c(defauts, sprintf("%s : illisible (%s)", basename(f), conditionMessage(o)))
      next
    }
    occ <- rbind(occ, o)
  }
  if (is.null(occ)) occ <- data.frame(fichier = character(0), ligne = integer(0), fonction = character(0),
                                      regle = character(0), jeton = character(0), stringsAsFactors = FALSE)
  occ <- occ[order(occ$fichier, occ$ligne, occ$regle), , drop = FALSE]
  # Admission : fichier, fonction ("*" : tout le fichier) et règle.
  regles_lieu <- strsplit(lieux$regles, ",", fixed = TRUE)
  lieu_de <- function(fichier, fonction, regle) {
    which(lieux$fichier == fichier & (lieux$fonction == "*" | lieux$fonction == fonction) &
            vapply(regles_lieu, function(r) regle %in% r, NA))
  }
  utilise <- rep(FALSE, nrow(lieux))
  for (i in seq_len(nrow(occ))) {
    l <- lieu_de(occ$fichier[i], occ$fonction[i], occ$regle[i])
    if (length(l)) { utilise[l] <- TRUE; next }
    defauts <- c(defauts, sprintf("%s:%d : fonction %s : règle %s : jeton %s (hors des lieux admis, ADR 0016, point 4.4)",
                                  occ$fichier[i], occ$ligne[i], occ$fonction[i], occ$regle[i], occ$jeton[i]))
  }
  for (j in which(!utilise))
    defauts <- c(defauts, sprintf("admission inutile : %s, fonction %s (règles %s) : aucune occurrence ; à retirer de LIEUX_ADMIS (ADR 0016, point 4.4)",
                                  lieux$fichier[j], lieux$fonction[j], lieux$regles[j]))
  list(defauts = defauts, occurrences = occ, noms = nr$noms, lieux_utilises = utilise)
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  dossier <- if (length(args)) args[1L] else
    file.path(dirname(dirname(dirname(normalizePath(script)))), "R")
  cat("Invariant des recettes :", dossier, "\n")
  r <- analyser_recettes(dossier)
  if (length(r$noms)) cat(sprintf("  %d nom(s) de recette lu(s) dans %s : %s\n", length(r$noms),
                                  NOM_CATALOGUE, paste(r$noms, collapse = ", ")))
  if (!is.null(r$occurrences))
    cat(sprintf("  %d occurrence(s) des règles R1 à R3 relevée(s)\n", nrow(r$occurrences)))
  if (length(r$defauts)) {
    cat(paste0("  ECHEC : ", r$defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Invariant des recettes::", paste(r$defauts, collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("  OK : toutes les occurrences sont en lieu admis, aucune admission inutile (ADR 0016, point 4.4).\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
