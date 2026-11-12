# -----------------------------------------------------------------------------
#  tests/test_unitaires.R : batterie des tests unitaires (ADR 0007, point 11)
#
#  Installe l'arbre de travail dans une bibliothèque temporaire, puis exécute
#  chaque tests/unitaires/test_*.R dans son propre environnement. Les
#  fonctions non exportées s'appellent par tablexp::: (seul endroit admis,
#  ADR 0007, point 11.3).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/test_unitaires.R [--dossier <dossier de test_*.R>]
#  Détail de toutes les assertions :
#      Rscript -e "options(harnais.verbeux = TRUE); source('tests/test_unitaires.R')"
#  Code de sortie 1 si au moins un échec ou un succès inattendu.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(normalizePath(f[1L])), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
installer_tablexp(RACINE)

# --dossier <d> : autre dossier de fichiers test_*.R (témoins des tests du
# lanceur lui-même, tests/unitaires/test_harnais.R).
ARGS <- commandArgs(trailingOnly = TRUE)
DOSSIER <- if ("--dossier" %in% ARGS) ARGS[match("--dossier", ARGS) + 1L] else
  file.path(RACINE, "tests", "unitaires")
if (is.na(DOSSIER) || !dir.exists(DOSSIER)) {
  verifier("dossier des tests unitaires", sprintf("dossier introuvable : %s", DOSSIER))
  bilan()
}

# Boucle dans local() : f, env, avant, r, e et t0 ne restent pas dans
# globalenv(), parent des environnements de test, où un test qui lirait l'une
# d'elles comme variable libre la trouverait silencieusement (mesuré, #17).
# Les fonctions de lecture (analyser_fichier() et ses auxiliaires) sont dans le
# même local() pour la même raison.
local({
  # Analyse syntaxique d'un fichier de test (UTF-8), indépendante de la locale
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
  # Règle de construction des noms de test (#229). Hors UTF-8, une chaîne non
  # ASCII en position de nom (nom d'argument : list("é" = 1), c("é" = 1) ; après
  # $ ; membre gauche : "é" <- 1) est altérée en « <U+00E9> » ; un symbole non
  # ASCII, nu ou entre accents graves, est refusé (voir plus haut). Quand le nom
  # est l'objet éprouvé (champ, nom d'élément que le code compare ou cite), le
  # fichier de test ne l'écrit donc jamais ainsi : il le pose par setNames() ou
  # `names<-`, ou par x[["é"]] <- v, à partir d'une chaîne en position de
  # valeur, intacte en toute locale (setNames(list(1), "é")). Une étiquette de
  # cas altérée ne touche que le libellé et reste admise. Une chaîne d'encodage
  # « bytes » se construit par Encoding(x) <- "bytes" sur une chaîne en position
  # de valeur ("\xe9", ou "é" pour les octets UTF-8).
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

  t0 <- Sys.time()
  for (f in sort(list.files(DOSSIER,
                            pattern = "^test_.*\\.R$", full.names = TRUE))) {
    fichier_courant(basename(f))
    avant <- nrow(.harnais$lignes)
    env <- new.env(parent = globalenv())
    # quit() et q() neutralisés dans le fichier de test (base::quit() est
    # rattrapé par la garde de sortie du harnais) ; bilan() aussi : un bilan
    # intermédiaire marquerait la batterie comme close avant la fin.
    env$quit <- env$q <- function(...) stop("quit() interdit dans un fichier de test")
    env$bilan <- function(...) stop("bilan() interdit dans un fichier de test")
    # Lecture en UTF-8 en toute locale (analyser_fichier(), #213) : littéraux
    # non ASCII des tests marqués UTF-8, comme ceux du moteur installé sous une
    # locale UTF-8 (M54) ; source(encoding = "UTF-8") n'exécute rien en locale
    # C (mesuré). Un fichier qui n'est pas de l'UTF-8 valide est refusé.
    r <- tryCatch({ for (e in analyser_fichier(f, getOption("keep.source"))) eval(e, env); NULL }, error = function(e) e)
    if (inherits(r, "error"))
      verifier("chargement complet du fichier", paste("erreur :", conditionMessage(r)))
    else if (nrow(.harnais$lignes) == avant)
      verifier("au moins une assertion", "fichier de test sans aucune assertion")
  }
  cat(sprintf("\nDurée : %.1f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
})
bilan()
