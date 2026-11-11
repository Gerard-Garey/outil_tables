# -----------------------------------------------------------------------------
#  tests/unitaires/test_controles_portables.R : utilitaire commun des scripts
#  de tests/controles/ (tests/controles/orchestration_locale.R, issue #117).
#  Interpréteur Python du juge indépendant (issue #123) : recherche testée avec
#  de faux interpréteurs (scripts sh), donc hors Windows.
#  Cas bon marché (moins de deux secondes) : le contrôle complet de chaque
#  locale est une étape de la CI. Nature des attendus : valeurs exactes.
# -----------------------------------------------------------------------------

CONTROLES_ <- file.path(RACINE, "tests", "controles")
# Lecture d'un script en UTF-8 quelle que soit la locale (#213) : les fonctions
# restent dans ce local(), hors de l'environnement du fichier de test.
lecture_ <- local({
  # Analyse syntaxique d'un script de tests/controles/ (UTF-8), indépendante de
  # la locale native (#213, suite de #206). En locale UTF-8 : parse(fichier,
  # encoding = "UTF-8"). Sinon (mesuré sous LC_ALL=C, R 4.3.3), ce même appel accepte sans
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
  # Les noms non ASCII de ces scripts (étiquettes de cas, dans le corps d'une
  # fonction de locale_c.R) ne sont exécutés par aucun cas de ce fichier.
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
  list(analyser_fichier = analyser_fichier, source_ascii = source_ascii)
})
charger_ <- function(script, env, dossier = CONTROLES_) {
  for (e in lecture_$analyser_fichier(file.path(dossier, script), keep.source = FALSE)) eval(e, env)
  env
}
orch_ <- charger_("orchestration_locale.R", new.env())

verifier("orchestration : l'utilitaire ne définit que les huit fonctions communes",
         identical(sort(ls(orch_)), c("appeler_python", "avec_environnement", "duree", "ecart_octets", "executer",
                                     "installer", "message_python_absent", "trouver_python")))

# --- lecture des scripts en UTF-8, quelle que soit la locale (#213) ---------------
# Avant #213 (mesuré sous LC_ALL=C) : charger_() acceptait sans rien dire un script
# dont les octets ne sont pas de l'UTF-8 valide (refusé en UTF-8) et écartait tous
# les avertissements du parseur. Le sous-processus sous LC_ALL=C évalue les trois
# premières définitions de ce fichier (CONTROLES_, lecture_, charger_), pour que le
# cas porte sur la locale C quelle que soit la session de la batterie.

dossier_octets_ <- tempfile("controle_octets_"); dir.create(dossier_octets_)
writeBin(c(charToRaw("x <- \""), as.raw(0xff), charToRaw("\"\n")), file.path(dossier_octets_, "invalide.R"))
writeBin(c(charToRaw("f <- function() c(\"d"), as.raw(c(0xc3, 0xa9)), charToRaw("lais\" = \""), as.raw(c(0xc3, 0xa9)),
           charToRaw("t"), as.raw(c(0xc3, 0xa9)), charToRaw("\")  # "), as.raw(c(0xc3, 0xa9)), charToRaw("\n")),
         file.path(dossier_octets_, "accentue.R"))
verifier("charger_ : un script qui n'est pas de l'UTF-8 valide est refusé (session de la batterie)",
         inherits(tryCatch(charger_("invalide.R", new.env(), dossier_octets_), error = function(e) e), "error"))
verifier("lecture_ : source_ascii() refuse un fichier qui n'est pas de l'UTF-8 valide, quelle que soit la locale",
         inherits(tryCatch(lecture_$source_ascii(file.path(dossier_octets_, "invalide.R")), error = function(e) e), "error"))
avertissements_ <- character(0)
e_ <- withCallingHandlers(charger_("accentue.R", new.env(), dossier_octets_),
                          warning = function(w) { avertissements_ <<- c(avertissements_, conditionMessage(w)); invokeRestart("muffleWarning") })
verifier("charger_ : un script à nom, littéral et commentaire non ASCII est chargé sans avertissement, littéral intact",
         length(avertissements_) == 0L &&
           identical(as.character(charToRaw(e_$f()[[1L]])), c("c3", "a9", "74", "c3", "a9")))
lanceur_ <- tempfile(fileext = ".R")
writeLines(c(sprintf("RACINE <- %s", deparse(RACINE)),
             sprintf("for (e in parse(%s, encoding = 'UTF-8', keep.source = FALSE))",
                     deparse(file.path(RACINE, "tests", "unitaires", "test_controles_portables.R"))),
             "  if (is.call(e) && is.symbol(e[[2L]]) && as.character(e[[2L]]) %in% c('CONTROLES_', 'lecture_', 'charger_')) eval(e)",
             sprintf("r <- tryCatch(charger_('invalide.R', new.env(), %s), error = function(e) conditionMessage(e))",
                     deparse(dossier_octets_)),
             "cat('UTF-8 :', l10n_info()[['UTF-8']], '\\n')",
             "cat('RESULTAT :', if (is.environment(r)) 'charge' else r, '\\n')"), lanceur_)
r_ <- executer_rscript(lanceur_, env = "LC_ALL=C")
verifier("charger_ : sous LC_ALL=C, un script qui n'est pas de l'UTF-8 valide est refusé (octets non UTF-8)",
         if (any(r_$sortie == "UTF-8 : FALSE ") && any(grepl("^RESULTAT : octets non UTF-8", r_$sortie, useBytes = TRUE))) TRUE
         else paste(r_$sortie, collapse = " | "))
unlink(c(dossier_octets_, lanceur_), recursive = TRUE)

# --- avec_environnement ---------------------------------------------------------

noms_ <- c("TABLEXP_ORCH_ABSENTE", "TABLEXP_ORCH_PRESENTE")
Sys.unsetenv(noms_[1L])
Sys.setenv(TABLEXP_ORCH_PRESENTE = "avant")
vu_ <- orch_$avec_environnement(c(TABLEXP_ORCH_ABSENTE = "x", TABLEXP_ORCH_PRESENTE = "y"),
                               c(Sys.getenv(noms_[1L]), Sys.getenv(noms_[2L])))
verifier("avec_environnement : variables posées pendant l'appel, valeur de l'expression rendue",
         identical(vu_, c("x", "y")))
verifier("avec_environnement : variable absente supprimée, variable présente restaurée",
         identical(Sys.getenv(noms_[1L], unset = NA), NA_character_) &&
           identical(Sys.getenv(noms_[2L], unset = NA), "avant"))
r_ <- tryCatch(orch_$avec_environnement(c(TABLEXP_ORCH_ABSENTE = "x", TABLEXP_ORCH_PRESENTE = "y"), stop("boum")),
               error = function(e) conditionMessage(e))
verifier("avec_environnement : restauration aussi après une erreur de l'expression",
         identical(r_, "boum") && identical(Sys.getenv(noms_[1L], unset = NA), NA_character_) &&
           identical(Sys.getenv(noms_[2L], unset = NA), "avant"))
Sys.unsetenv(noms_)

# --- ecart_octets, duree ----------------------------------------------------------

verifier("ecart_octets : premier octet différent et ligne ; préfixe commun",
         identical(orch_$ecart_octets(charToRaw("ab\ncd\n"), charToRaw("ab\nXd\n")),
                   "longueurs 6 et 6 octets ; premier octet différent : 4 (ligne 2)") &&
           identical(orch_$ecart_octets(charToRaw("ab\ncd"), charToRaw("ab\ncd\nef")),
                     "longueurs 5 et 8 octets ; premier octet différent : 6 (ligne 2)"))
t0_ <- Sys.time() - 2.5
d_txt_ <- orch_$duree(t0_)
verifier("duree : texte « n.n s » (2,5 s écoulées au moins)",
         grepl("^[0-9]+\\.[0-9] s$", d_txt_) && as.numeric(sub(" s$", "", d_txt_)) >= 2.5)

# --- executer, installer : statut de sortie et environnement transmis -------------

script_ <- tempfile("orch_", fileext = ".R")
writeLines(c("a <- commandArgs(trailingOnly = TRUE)",
             "cat('TABLEXP_ORCH=', Sys.getenv('TABLEXP_ORCH'), ' args=', length(a), '\\n', sep = '')",
             "if (identical(a[3L], 'echec')) quit(save = 'no', status = 3L)"), script_)
ex_ok_ <- orch_$executer(script_, "racine", "lib", c(TABLEXP_ORCH = "oui"), "attendu", "sortie")
ex_ko_ <- orch_$executer(script_, "racine", "lib", c(TABLEXP_ORCH = "oui"), "echec", "sortie")
verifier("executer : sous-processus lancé sous l'environnement donné (--travailleur et 5 arguments), ok = TRUE",
         isTRUE(ex_ok_$ok) && identical(as.vector(ex_ok_$sortie), "TABLEXP_ORCH=oui args=5") &&
           grepl("^[0-9]+\\.[0-9] s$", ex_ok_$duree))
verifier("executer : statut de sortie non nul -> ok = FALSE",
         identical(ex_ko_$ok, FALSE) && identical(attr(ex_ko_$sortie, "status"), 3L))
unlink(script_)

sans_package_ <- tempfile("orch_racine_"); dir.create(sans_package_)
lib_ <- tempfile("orch_lib_"); dir.create(lib_)
ins_ <- orch_$installer(sans_package_, lib_, c(LC_ALL = "C"))
verifier("installer : racine sans package -> ok = FALSE, sortie et durée rendues",
         identical(ins_$ok, FALSE) && length(ins_$sortie) > 0L && grepl("^[0-9]+\\.[0-9] s$", ins_$duree))
unlink(c(sans_package_, lib_), recursive = TRUE)

# --- sourçage par les deux contrôles de locale -----------------------------------
# Les contrôles, lus comme le fait tests/unitaires/test_controles.R (parse en
# UTF-8, eval() dans un environnement), retrouvent l'utilitaire (mêmes
# fonctions, signatures à env = vecteur d'environnement complet). La
# comparaison de corps ne voit pas une recopie mot pour mot : le texte des
# contrôles est donc lu aussi, qui ne doit définir aucune des cinq fonctions.

for (script_ in c("locale_c.R", "locale_latin1.R")) {
  e_ <- charger_(script_, new.env())
  verifier(sprintf("%s : orchestration chargée depuis l'utilitaire (mêmes fonctions, signatures à env)", script_),
           all(vapply(names(orch_), function(n) identical(body(e_[[n]]), body(orch_[[n]])) &&
                        identical(formals(e_[[n]]), formals(orch_[[n]])), TRUE)) &&
             identical(names(formals(e_$installer)), c("racine", "lib", "env")) &&
             identical(names(formals(e_$executer)), c("script", "racine", "lib", "env", "attendu", "sortie")))
  l_ <- readLines(file.path(CONTROLES_, script_), encoding = "UTF-8")
  redefinies_ <- grep("^(avec_environnement|duree|installer|executer|ecart_octets) *<- *function", l_, value = TRUE)
  verifier(sprintf("%s : aucune des cinq fonctions communes n'est redéfinie dans le contrôle", script_),
           if (length(redefinies_)) paste("redéfinies :", paste(redefinies_, collapse = " | ")) else TRUE)
}

# Un orchestration_locale.R homonyme à côté du script lanceur (ici D/, où
# tests/test_unitaires.R sourcerait le contrôle) n'est pas chargé : seul le
# dossier qui contient aussi le contrôle est retenu (D/controles/) ; le lanceur
# s'exécute hors du dépôt (le repli sur tests/controles du répertoire courant
# ne joue pas). Sans
# utilitaire à côté du contrôle, le chargement refuse (aucun repli sur
# l'homonyme).
for (script_ in c("locale_c.R", "locale_latin1.R")) {
  d_ <- tempfile("homonyme_"); dir.create(file.path(d_, "controles"), recursive = TRUE)
  file.copy(file.path(CONTROLES_, c(script_, "orchestration_locale.R")), file.path(d_, "controles"))
  writeLines('installer <- function(...) "FAUX"', file.path(d_, "orchestration_locale.R"))
  lanceur_ <- file.path(d_, "lanceur.R")
  writeLines(c("setwd(tempdir())  # hors du dépôt : pas de repli sur tests/controles du répertoire courant",
               sprintf('e <- new.env(); for (x in parse(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), "controles", "%s"), encoding = "UTF-8")) eval(x, e)', script_),
               'cat("installer:", names(formals(e$installer)), "\n")'), lanceur_)
  r_ <- executer_rscript(lanceur_)
  verifier(sprintf("%s : homonyme à côté du script lanceur non chargé (installer à env, pas « FAUX »)", script_),
           identical(r_$statut, 0L) && any(r_$sortie == "installer: racine lib env "))
  file.remove(file.path(d_, "controles", "orchestration_locale.R"))
  r_ <- executer_rscript(lanceur_)
  verifier(sprintf("%s : sans utilitaire à côté du contrôle, refus nommé (pas de repli sur l'homonyme)", script_),
           r_$statut != 0L && any(grepl("orchestration_locale.R introuvable", r_$sortie, fixed = TRUE)) &&
             any(grepl(script_, r_$sortie, fixed = TRUE)))
  unlink(d_, recursive = TRUE)
}

# --- trouver_python, appeler_python (issue #123) ------------------------------------
# Faux interpréteurs : scripts sh qui consignent leur nom et leurs arguments dans
# un journal, impriment éventuellement un texte, puis sortent avec le code
# demandé ; la recherche est injectée (`chercher`), de sorte qu'aucun vrai
# Python ni aucune variable d'environnement n'entre en jeu. Hors Windows (sh).

if (.Platform$OS.type != "windows") {
  d_py_ <- tempfile("faux_python_")
  dir.create(file.path(d_py_, "bin"), recursive = TRUE); dir.create(file.path(d_py_, "WindowsApps"))
  journal_ <- file.path(d_py_, "journal.txt")
  # `nargs` : le journal porte aussi le nombre d'arguments reçus ($#).
  faux_ <- function(chemin, nom, code, sortie = "", nargs = FALSE) {
    writeLines(c("#!/bin/sh", sprintf("echo '%s' %s\"$@\" >> '%s'", nom, if (nargs) "\"$#\" " else "", journal_),
                 sortie, sprintf("exit %d", code)), chemin)
    Sys.chmod(chemin, "755")
    chemin
  }
  vue_ <- function(...) { unlink(journal_); orch_$trouver_python(...) }
  chemins_ <- c(python3 = file.path(d_py_, "bin", "python3"), python = file.path(d_py_, "bin", "python"),
                py = file.path(d_py_, "bin", "py"))
  cherche_ <- function(m) function(nom) if (nom %in% names(m)) m[[nom]] else ""
  appels_ <- function() if (file.exists(journal_)) sub(" .*", "", readLines(journal_)) else character(0)

  # Ordre de priorité : python3 d'abord, une fois exécuté (-I -c, sys.version_info >= (3, 8)).
  for (n_ in names(chemins_)) faux_(chemins_[[n_]], n_, 0L)
  r_ <- vue_(chercher = cherche_(chemins_))
  verifier("trouver_python : python3 retenu en premier, seul exécuté, sans préfixe",
           identical(r_, list(commande = chemins_[["python3"]], prefixe = character(0))) &&
             identical(appels_(), "python3"))
  verifier("trouver_python : le candidat est vérifié par -I -c sys.version_info >= (3, 8)",
           grepl("^python3 -I -c import sys; sys.exit\\(0 if sys.version_info >= \\(3, 8\\) else 1\\)$", readLines(journal_)))
  vue_(chercher = cherche_(chemins_), version_min = c(3L, 11L))
  verifier("trouver_python : version minimale paramétrable (3, 11)", grepl("\\(3, 11\\)", readLines(journal_)))

  # Un candidat dont l'exécution échoue (code non nul, ou fichier non exécutable) est écarté.
  faux_(chemins_[["python3"]], "python3", 1L)
  r_ <- vue_(chercher = cherche_(chemins_))
  verifier("trouver_python : python3 en échec écarté, python retenu (python3 puis python exécutés)",
           identical(r_, list(commande = chemins_[["python"]], prefixe = character(0))) &&
             identical(appels_(), c("python3", "python")))
  writeLines("pas un programme", chemins_[["python3"]]); Sys.chmod(chemins_[["python3"]], "644")
  r_ <- vue_(chercher = cherche_(chemins_))
  verifier("trouver_python : candidat non exécutable écarté sans erreur",
           identical(r_$commande, chemins_[["python"]]) && identical(appels_(), "python"))
  faux_(chemins_[["python3"]], "python3", 0L)

  # Alias du Store : chemin contenant WindowsApps, écarté sans être exécuté.
  alias_ <- faux_(file.path(d_py_, "WindowsApps", "python3"), "alias", 0L)
  r_ <- vue_(chercher = cherche_(c(python3 = alias_, python = chemins_[["python"]], py = chemins_[["py"]])))
  verifier("trouver_python : chemin WindowsApps écarté sans être exécuté, python retenu",
           identical(r_$commande, chemins_[["python"]]) && identical(appels_(), "python"))
  r_ <- vue_(chercher = cherche_(c(python3 = sub("WindowsApps", "windowsapps", alias_), python = chemins_[["python"]])))
  verifier("trouver_python : WindowsApps reconnu sans égard à la casse",
           identical(r_$commande, chemins_[["python"]]) && identical(appels_(), "python"))
  # Dossier du chemin résolu en WindowsApps : le fichier est un lien vers un vrai
  # interpréteur (sa forme longue, comme celle d'un alias dont la résolution
  # aboutit ailleurs, ne contient pas WindowsApps) et le chemin donné n'a pas le
  # mot, car il passe par un lien symbolique « lien » vers WindowsApps. Seule la
  # forme longue du dossier le révèle. Témoin : même montage vers un dossier
  # ordinaire, exécuté.
  file.symlink(chemins_[["python3"]], file.path(d_py_, "WindowsApps", "relais"))
  file.symlink(file.path(d_py_, "WindowsApps"), file.path(d_py_, "lien"))
  dir.create(file.path(d_py_, "ordinaire"))
  file.symlink(chemins_[["python3"]], file.path(d_py_, "ordinaire", "relais"))
  file.symlink(file.path(d_py_, "ordinaire"), file.path(d_py_, "lien_ordinaire"))
  via_ <- file.path(d_py_, "lien", "relais"); via_ok_ <- file.path(d_py_, "lien_ordinaire", "relais")
  verifier("trouver_python : montage du dossier WindowsApps (ni le chemin donné, ni la forme longue du fichier ne portent le mot)",
           !grepl("WindowsApps", via_, ignore.case = TRUE) &&
             !grepl("WindowsApps", normalizePath(via_, winslash = "/"), ignore.case = TRUE) &&
             grepl("WindowsApps", normalizePath(dirname(via_), winslash = "/"), ignore.case = TRUE) &&
             file.exists(via_))
  cand_py3_ <- list(list(nom = "python3", prefixe = character(0)))
  r_ <- vue_(candidats = cand_py3_, chercher = function(nom) via_)
  verifier("trouver_python : dossier résolu en WindowsApps (lien symbolique), fichier résolu ailleurs : écarté sans être exécuté",
           is.null(r_) && identical(appels_(), character(0)))
  r_ <- vue_(candidats = cand_py3_, chercher = function(nom) via_ok_)
  verifier("trouver_python : même montage vers un dossier ordinaire, exécuté et retenu : le témoin passe bien par le lien",
           identical(r_$commande, via_ok_) && identical(appels_(), "python3"))
  file.remove(file.path(d_py_, c("lien", "lien_ordinaire")))

  # Lanceur py : -3 placé avant les arguments de la vérification.
  faux_(chemins_[["python3"]], "python3", 1L); faux_(chemins_[["python"]], "python", 3L)
  r_ <- vue_(chercher = cherche_(chemins_))
  verifier("trouver_python : py retenu avec le préfixe -3, appelé « py -3 -I -c … »",
           identical(r_, list(commande = chemins_[["py"]], prefixe = "-3")) &&
             identical(appels_(), c("python3", "python", "py")) &&
             grepl("^py -3 -I -c ", tail(readLines(journal_), 1L)))

  # Un candidat qui ne rend pas la main dans le délai est écarté (délai court injecté).
  writeLines(c("#!/bin/sh", sprintf("echo 'python3' >> '%s'", journal_), "exec sleep 5"), chemins_[["python3"]])
  faux_(chemins_[["python"]], "python", 0L)
  t0_ <- Sys.time()
  r_ <- vue_(chercher = cherche_(chemins_), delai = 1)
  verifier("trouver_python : candidat qui dépasse le délai écarté (python3 dort 5 s, délai 1 s), python retenu",
           identical(r_$commande, chemins_[["python"]]) && identical(appels_(), c("python3", "python")) &&
             as.numeric(difftime(Sys.time(), t0_, units = "secs")) < 3)
  faux_(chemins_[["python3"]], "python3", 1L); faux_(chemins_[["python"]], "python", 3L)

  # Aucun candidat : introuvables, ou tous en échec, ou tous écartés -> NULL.
  verifier("trouver_python : aucun candidat trouvé par la recherche -> NULL",
           is.null(orch_$trouver_python(chercher = function(nom) "")))
  faux_(chemins_[["py"]], "py", 1L)
  r_ <- vue_(chercher = cherche_(chemins_))
  verifier("trouver_python : tous les candidats en échec -> NULL, les trois ont été exécutés",
           is.null(r_) && identical(appels_(), c("python3", "python", "py")))
  r_ <- vue_(chercher = cherche_(c(python3 = alias_, python = alias_)))
  verifier("trouver_python : tous les chemins WindowsApps -> NULL, aucun exécuté",
           is.null(r_) && identical(appels_(), character(0)))
  r_ <- vue_(candidats = list(list(nom = "x", prefixe = character(0))), chercher = cherche_(c(x = file.path(d_py_, "absent"))))
  verifier("trouver_python : chemin inexistant -> NULL, sans erreur ni avertissement", is.null(r_))
  verifier("trouver_python : liste de candidats vide -> NULL", is.null(orch_$trouver_python(candidats = list())))

  # appeler_python : statut, sortie capturée (standard et erreur), UTF-8, CRLF ramenées à LF.
  faux_(chemins_[["python"]], "python", 2L,
        c("printf 'ligne \\303\\251\\r\\n'", "printf 'erreur\\n' >&2"), nargs = TRUE)
  p_ <- list(commande = chemins_[["python"]], prefixe = "-3")
  c_ <- orch_$appeler_python(p_, c("-I", shQuote("x y")), capturer = TRUE)
  verifier("appeler_python : statut rendu, sortie standard et d'erreur capturées, CRLF ramenées à LF",
           identical(c_$statut, 2L) && length(c_$sortie) == 2L &&
             identical(charToRaw(c_$sortie[1L]), as.raw(c(0x6c, 0x69, 0x67, 0x6e, 0x65, 0x20, 0xc3, 0xa9))) &&
             identical(c_$sortie[2L], "erreur"))
  verifier("appeler_python : préfixe puis arguments transmis, « x y » protégé par shQuote en un seul argument ($# = 3)",
           identical(tail(readLines(journal_), 1L), "python 3 -3 -I x y"))
  verifier("appeler_python : accents lus en UTF-8 (marque UTF-8, sauf locale C hors Windows où les octets sont gardés)",
           identical(Encoding(c_$sortie[1L]), if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) "unknown" else "UTF-8") &&
             (!identical(Encoding(c_$sortie[1L]), "UTF-8") || identical(enc2utf8(c_$sortie[1L]), "ligne \u00e9")))
  faux_(chemins_[["python"]], "python", 0L)
  verifier("appeler_python : sans capture, statut 0 rendu et sortie vide",
           identical(orch_$appeler_python(p_, "--version"), list(statut = 0L, sortie = character(0))))
  verifier("message_python_absent : ligne ECHEC nommant les trois candidats, terminée par un saut de ligne",
           grepl("^ECHEC : .*python3.*python.*py -3.*\n$", orch_$message_python_absent()))
  unlink(d_py_, recursive = TRUE)
} else {
  cat("  [sauté] trouver_python (hors alias du Store), appeler_python, message_python_absent : faux interpréteurs en scripts sh, plateforme Windows : non exercés\n")

  # Alias du Store sous Windows : Sys.which() rend des chemins courts 8.3
  # (…\MICROS~1\WINDOW~1\python3.exe) où « WindowsApps » n'apparaît pas ; le
  # filtre porte donc sur la forme longue. Témoin : python3.cmd, dans un dossier
  # nommé WindowsApps, qui écrit un fichier trace s'il est lancé ; `chercher`
  # rend sa forme courte (shortPathName). Un témoin identique hors de ce dossier
  # prouve qu'il laisse bien sa trace quand il est exécuté.
  d_wa_ <- tempfile("faux_store_"); dir.create(file.path(d_wa_, "WindowsApps"), recursive = TRUE)
  dir.create(file.path(d_wa_, "bin"))
  trace_ <- file.path(d_wa_, "trace.txt")
  temoin_ <- function(chemin) {
    writeLines(c("@echo off", sprintf("echo lance> \"%s\"", gsub("/", "\\", trace_, fixed = TRUE)), "exit /b 0"), chemin)
    chemin
  }
  cmd_wa_ <- temoin_(file.path(d_wa_, "WindowsApps", "python3.cmd"))
  cmd_ok_ <- temoin_(file.path(d_wa_, "bin", "python3.cmd"))
  court_ <- utils::shortPathName(cmd_wa_)
  cand_ <- list(list(nom = "python3", prefixe = character(0)))
  # Sans forme courte qui cache le nom du dossier, le test ne prouverait rien.
  if (!grepl("WindowsApps", court_, ignore.case = TRUE)) {
    r_ <- orch_$trouver_python(cand_, function(nom) court_)
    verifier("trouver_python : alias WindowsApps rendu en chemin court 8.3 (sans « WindowsApps »), écarté sans être exécuté (aucune trace)",
             is.null(r_) && !file.exists(trace_))
    r_ <- orch_$trouver_python(cand_, function(nom) cmd_ok_)
    verifier("trouver_python : même témoin hors de WindowsApps, exécuté (trace écrite) et retenu : le témoin laisse bien sa trace",
             identical(r_$commande, cmd_ok_) && file.exists(trace_))
  } else cat("  [sauté] trouver_python : alias WindowsApps en chemin court 8.3 : ce volume ne fournit pas de forme courte distincte (noms 8.3 inactifs) : filtre sur forme longue non exercé\n")
  unlink(d_wa_, recursive = TRUE)
}

# Vrai interpréteur, quand il y en a un. La CI actuelle n'en a pas au moment de
# la batterie unitaire (python3 n'y est installé que pour les étapes des
# contrôles, après elle) : le saut est alors annoncé, jamais silencieux.
p_reel_ <- orch_$trouver_python()
if (!is.null(p_reel_)) {  # sauté sans interpréteur utilisable (Windows avec le seul alias du Store)
  verifier("trouver_python : vrai interpréteur >= 3.8 trouvé, version trop haute (99.0) refusée",
           is.null(orch_$trouver_python(version_min = c(99L, 0L))))
} else cat("  [sauté] trouver_python : vrai interpréteur >= 3.8 : aucun interpréteur Python >= 3.8 trouvé, version trop haute (99.0) non exercée\n")

# --- sourçage par les deux contrôles à juge Python -------------------------------
# Les contrôles ne se chargent pas comme ceux de locale (ils s'exécutent en
# entier) : on vérifie leur texte (aucune redéfinition de l'utilitaire, plus de
# Sys.which("python3")) et on exécute leur bloc de chargement, extrait tel quel,
# comme plus haut (homonyme à côté du lanceur non chargé ; refus nommé).

for (script_ in c("ecriture_doubles.R", "ecriture_doubles_texte.R")) {
  l_ <- readLines(file.path(CONTROLES_, script_), encoding = "UTF-8")
  redefinies_ <- grep("^(trouver_python|appeler_python|message_python_absent|avec_environnement|installer|executer) *<- *function", l_, value = TRUE)
  verifier(sprintf("%s : aucune fonction de l'utilitaire redéfinie ; python trouvé par trouver_python(), plus de Sys.which(\"python3\")", script_),
           if (length(redefinies_)) paste("redéfinies :", paste(redefinies_, collapse = " | ")) else
             any(grepl("^python <- trouver_python\\(\\)", l_)) && !any(grepl("Sys.which(\"python3\")", l_, fixed = TRUE)))
  debut_ <- grep("^for \\(e_ in parse\\(", l_); fin_ <- grep("^rm\\(e_\\)", l_)
  d_ <- tempfile("homonyme_py_"); dir.create(file.path(d_, "controles"), recursive = TRUE)
  file.copy(file.path(CONTROLES_, "orchestration_locale.R"), file.path(d_, "controles"))
  writeLines("# contrôle factice", file.path(d_, "controles", script_))
  writeLines('trouver_python <- function(...) "FAUX"', file.path(d_, "orchestration_locale.R"))
  lanceur_ <- file.path(d_, "lanceur.R")
  writeLines(c("setwd(tempdir())", l_[debut_:fin_], 'cat("trouver_python:", names(formals(trouver_python)), "\n")'), lanceur_)
  verifier(sprintf("%s : bloc de chargement unique (une seule fois dans le fichier)", script_),
           length(debut_) == 1L && length(fin_) == 1L && fin_ > debut_)
  r_ <- executer_rscript(lanceur_)
  verifier(sprintf("%s : homonyme à côté du script lanceur non chargé (trouver_python à candidats, pas « FAUX »)", script_),
           identical(r_$statut, 0L) && any(r_$sortie == "trouver_python: candidats chercher version_min delai "))
  file.remove(file.path(d_, "controles", "orchestration_locale.R"))
  r_ <- executer_rscript(lanceur_)
  verifier(sprintf("%s : sans utilitaire à côté du contrôle, refus nommé (pas de repli sur l'homonyme)", script_),
           r_$statut != 0L && any(grepl("orchestration_locale.R introuvable", r_$sortie, fixed = TRUE)) &&
             any(grepl(script_, r_$sortie, fixed = TRUE)))
  unlink(d_, recursive = TRUE)
}

# --- témoins d'échec des contrôles à juge Python (issue #119) -----------------------
# La règle de tests/unitaires/test_controles.R (un cas fautif par script) vaut
# aussi pour ecriture_doubles_texte.R et ecriture_doubles.R, dont l'exécution
# complète (10^6 doubles et plus, 15 à 25 s) reste hors batterie. Les témoins
# bon marché ci-dessous exécutent le script du dépôt (ou une copie mutée, dans
# une arborescence temporaire R/, tests/controles/, tests/unitaires/ : le
# contrôle retrouve sa racine d'après son propre emplacement) :
#  - juge absent (hors Windows) : PATH réduit à quelques outils système, sans
#    Python ; le script sort en 1 et nomme l'absence, avant toute construction ;
#  - vrai défaut (copie mutée, Python requis) : MD5 de T2 de la batterie
#    altérée, juge Python faussé (.16g au lieu de .17g), N_EGALITES réduit et
#    n = 1000 pour la durée ; le contrôle doit relever les deux écarts, et eux
#    seuls (glibc et MD5 de T3 inchangés) ;
#  - ecriture_doubles.R sous un dossier temporaire dont le nom contient une
#    espace (R refuse une telle valeur de TMPDIR hors Windows : c'est la copie
#    qui redéfinit tempfile()) ; la même copie sans shQuote() échoue.
# Nature des attendus : valeurs exactes (codes de sortie, lignes relevées).

lignes_ <- function(f) readLines(f, encoding = "UTF-8", warn = FALSE)
# Remplace `ancien` par `nouveau` (texte littéral, octets) dans l'unique ligne de
# `l` qui le contient ; rend NULL si zéro ou plusieurs lignes le contiennent.
muter_ <- function(l, ancien, nouveau) {
  k <- grep(ancien, l, fixed = TRUE, useBytes = TRUE)
  if (length(k) != 1L) return(NULL)
  l[k] <- sub(ancien, nouveau, l[k], fixed = TRUE, useBytes = TRUE)
  l
}
# Arborescence temporaire portant le contrôle `script` (lignes `l`, précédées de
# `avant`) et ce qu'il lit à côté ; rend le chemin du contrôle copié.
copie_controle_ <- function(script, l, avant = character(0)) {
  racine_ <- tempfile("controle_copie_")
  for (d in c("R", file.path("tests", "controles"), file.path("tests", "unitaires")))
    dir.create(file.path(racine_, d), recursive = TRUE)
  file.copy(file.path(RACINE, "R", c("outils.R", "rapport.R")), file.path(racine_, "R"))
  file.copy(file.path(CONTROLES_, "orchestration_locale.R"), file.path(racine_, "tests", "controles"))
  file.copy(file.path(RACINE, "tests", "unitaires", "test_construire_table.R"), file.path(racine_, "tests", "unitaires"))
  f <- file.path(racine_, "tests", "controles", script)
  writeLines(c(avant, l), f, useBytes = TRUE)
  f
}
# Motifs sur octets (la sortie suit la locale : é en UTF-8, en latin-1 ou brut).
a_ <- function(r, motif) any(grepl(motif, r$sortie, useBytes = TRUE))

# Juge absent : outils minimaux de l'enveloppe de R dans un dossier sans Python.
if (.Platform$OS.type != "windows") {
  d_path_ <- tempfile("sans_python_"); dir.create(d_path_)
  for (o_ in c("uname", "sed", "rm", "dirname", "basename", "cat", "mkdir")) {
    w_ <- unname(Sys.which(o_))
    if (nzchar(w_)) file.symlink(w_, file.path(d_path_, o_))
  }
  r_ <- executer_rscript(file.path(CONTROLES_, "ecriture_doubles_texte.R"), env = paste0("PATH=", d_path_))
  verifier("ecriture_doubles_texte.R : sans Python dans le PATH, code 1 et absence nommée, avant toute construction",
           identical(r_$statut, 1L) && a_(r_, "^ECHEC : aucun interpr[^ ]* Python >= 3\\.8") &&
             !a_(r_, "doubles :") && !any(r_$sortie == "OK"))
  r_ <- executer_rscript(file.path(CONTROLES_, "ecriture_doubles.R"), env = paste0("PATH=", d_path_))
  verifier("ecriture_doubles.R : sans Python dans le PATH, code 1 et absence nommée",
           identical(r_$statut, 1L) && a_(r_, "^ECHEC : aucun interpr[^ ]* Python >= 3\\.8") && !any(r_$sortie == "OK"))
  unlink(d_path_, recursive = TRUE)
} else cat("  [sauté] ecriture_doubles_texte.R, ecriture_doubles.R : Python absent du PATH : plateforme Windows (effet d'un PATH vide non vérifié)\n")

# Vrai défaut : le contrôle sait voir un écart (copie mutée, Python requis).
if (!is.null(p_reel_)) {
  l_ <- lignes_(file.path(CONTROLES_, "ecriture_doubles_texte.R"))
  md5_t2_ <- sub("^MD5_T2 <- \"([0-9a-f]{32})\".*$", "\\1", grep("^MD5_T2 <- ", l_, value = TRUE))
  md5_faux_ <- paste0(substr(md5_t2_, 1L, 31L), if (substring(md5_t2_, 32L) == "0") "1" else "0")
  m_ <- muter_(l_, "N_EGALITES <- 2e5L", "N_EGALITES <- 1000L")
  if (!is.null(m_)) m_ <- muter_(m_, paste0("MD5_T2 <- \"", md5_t2_, "\""), paste0("MD5_T2 <- \"", md5_faux_, "\""))
  if (!is.null(m_)) m_ <- muter_(m_, "return format(x + 0.0, '.17g')", "return format(x + 0.0, '.16g')")
  verifier("ecriture_doubles_texte.R : trois mutations de la copie chacune appliquée à une seule ligne",
           if (is.null(m_)) "une mutation ne touche pas exactement une ligne" else
             identical(sum(m_ != l_), 3L) && nchar(md5_t2_) == 32L)
  if (!is.null(m_)) {
    f_ <- copie_controle_("ecriture_doubles_texte.R", m_)
    r_ <- executer_rscript(f_, "1000")
    verifier("ecriture_doubles_texte.R : MD5 de T2 altéré relevé (MD5 recalculé = celui de la batterie, constante fausse), code 1",
             identical(r_$statut, 1L) &&
               a_(r_, sprintf("^MD5 du texte de T2 : %s ; constante de la batterie %s ; DIFF", md5_t2_, md5_faux_)))
    verifier("ecriture_doubles_texte.R : MD5 de T3, tranches d'égalités exactes et table des puissances inchangés",
             a_(r_, "^MD5 du texte de T3 : ([0-9a-f]{32}) ; constante de la batterie \\1 ; [^;]*gal ;") &&
               a_(r_, "^  T1 \\(lignes [0-9]+ [^ ]+ [0-9]+\\) : 3 [^ ]*galit[^ ]*, attendu 3$") &&
               !a_(r_, "CART$") && a_(r_, "^Table des puissances : 2098 ligne\\(s\\), 0 "))
    verifier("ecriture_doubles_texte.R : juge Python faussé relevé (écarts non nuls, code du juge 1), juge glibc sans écart",
             a_(r_, "^Juge Python [0-9.]+ : [0-9]+ ligne\\(s\\), [1-9][0-9]* ") && a_(r_, ", code 1$") &&
               (.Platform$OS.type == "windows" || a_(r_, "^Juge glibc .* : [0-9]+ ligne\\(s\\), 0 ")))
    verifier("ecriture_doubles_texte.R : deux contrôles en écart nommés, pas de « OK »",
             a_(r_, "^ECHEC : 2 contr") && !any(r_$sortie == "OK"))
    unlink(dirname(dirname(dirname(f_))), recursive = TRUE)
    # Preuve par mutation du motif « CART$ » : un décompte d'égalités de T1 faussé
    # (attendu 4 au lieu de 3) est relevé par le juge sur la ligne de la tranche,
    # qui finit par « ÉCART » ; avec lui, les trois assertions sur T3 et T1 qui
    # précèdent échoueraient.
    m4_ <- muter_(m_, "fin = fin[[\"T1\"]], attendu = 3L)", "fin = fin[[\"T1\"]], attendu = 4L)")
    verifier("ecriture_doubles_texte.R : quatrième défaut (décompte d'égalités de T1 attendu 4 au lieu de 3) relevé sur la ligne de tranche, qui finit par ÉCART",
             if (is.null(m4_)) "la mutation ne touche pas exactement une ligne" else {
               f4_ <- copie_controle_("ecriture_doubles_texte.R", m4_)
               r4_ <- executer_rscript(f4_, "1000")
               unlink(dirname(dirname(dirname(f4_))), recursive = TRUE)
               identical(r4_$statut, 1L) && a_(r4_, "^  T1 \\(lignes [0-9]+ [^ ]+ [0-9]+\\) : 3 [^ ]*galit[^ ]*, attendu 4 : [^ ]*CART$") &&
                 a_(r4_, "CART$")
             })
  }
} else cat("  [sauté] ecriture_doubles_texte.R : vrai défaut (copie mutée : MD5 de T2, juge Python faussé) : aucun interpréteur Python >= 3.8\n")

# ecriture_doubles.R : dossier temporaire dont le nom contient une espace. Hors
# Windows (le script y refuse de s'exécuter) et avec Python.
if (.Platform$OS.type != "windows" && !is.null(p_reel_)) {
  l_ <- lignes_(file.path(CONTROLES_, "ecriture_doubles.R"))
  espace_ <- tempfile("dossier_espace_"); dir.create(file.path(espace_, "a b"), recursive = TRUE)
  # tempfile() du contrôle redéfini en tête de copie : fichiers dans « a b ».
  avant_ <- sprintf("tempfile <- function(pattern = \"file\", tmpdir = %s, fileext = \"\") base::tempfile(pattern, tmpdir, fileext)",
                    deparse(file.path(espace_, "a b")))
  f_ <- copie_controle_("ecriture_doubles.R", l_, avant_)
  r_ <- executer_rscript(f_, "1000")
  verifier("ecriture_doubles.R : chemins du juge protégés par shQuote(), contrôle passé avec un dossier temporaire à espace",
           identical(r_$statut, 0L) && any(r_$sortie == "OK") && a_(r_, "^Lignes : [0-9]+ \\(R\\) contre [0-9]+ \\(Python\\) ; [^ ]*cart[^ ]* : 0$"))
  m_ <- muter_(l_, "shQuote(c(script, entree, sortie))", "script, entree, sortie")
  verifier("ecriture_doubles.R : sans shQuote(), la copie échoue sous ce même dossier (le témoin voit le défaut)",
           if (is.null(m_)) "la mutation ne touche pas exactement une ligne" else {
             f_m_ <- copie_controle_("ecriture_doubles.R", m_, avant_)
             r_m_ <- executer_rscript(f_m_, "1000")
             unlink(dirname(dirname(dirname(f_m_))), recursive = TRUE)
             identical(r_m_$statut, 1L) && a_(r_m_, "^ECHEC : le juge Python a [^ ]*chou") && !any(r_m_$sortie == "OK")
           })
  unlink(c(espace_, dirname(dirname(dirname(f_)))), recursive = TRUE)
} else cat(sprintf("  [sauté] ecriture_doubles.R : dossier temporaire à espace, avec et sans shQuote() : %s\n",
                   if (.Platform$OS.type == "windows") "plateforme Windows (le contrôle ne s'y exécute pas)" else
                     "aucun interpréteur Python >= 3.8"))

# --- locale_utf8_disponible() : refus d'un nom inventé (issue #109) -------------------
# Sous l'UCRT de Windows, Sys.setlocale accepte tout nom inventé (« xx_XX.UTF-8 »)
# et lui donne la culture invariante, que seul Sys.localeconv() sous LC_MONETARY
# distingue d'une locale réelle (devise XDR contre USD pour en_US.UTF-8). Sous
# glibc le nom inventé est refusé : le résultat est le même ("") sans le critère.
# Limite résiduelle, non testée : « xx_US.UTF-8 » (langue inventée, pays réel,
# devise USD) échappe au critère. Nature des attendus : valeurs exactes.

monnaie_avant_ <- Sys.getlocale("LC_MONETARY"); ctype_avant_ <- Sys.getlocale("LC_CTYPE")
verifier("locale_utf8_disponible() : un nom inventé seul n'est pas disponible (UCRT : refusé par le critère ; glibc : refusé par setlocale)",
         identical(locale_utf8_disponible(c("xx_XX.UTF-8")), ""))
reference_ <- locale_utf8_disponible()
verifier("locale_utf8_disponible() : un nom inventé placé avant les candidates ne change pas le résultat",
         identical(locale_utf8_disponible(c("xx_XX.UTF-8", LOCALES_UTF8)), reference_))
verifier("locale_utf8_disponible() : LC_CTYPE et LC_MONETARY restaurées (appels refusés et acceptés)",
         identical(Sys.getlocale("LC_CTYPE"), ctype_avant_) && identical(Sys.getlocale("LC_MONETARY"), monnaie_avant_))
if (.Platform$OS.type == "windows") {
  verifier("locale_utf8_disponible() : en_US.UTF-8 derrière un nom inventé -> en_US.UTF-8 (locale réelle de l'UCRT)",
           identical(locale_utf8_disponible(c("xx_XX.UTF-8", "en_US.UTF-8")), "en_US.UTF-8"))
  verifier("locale_utf8_disponible() : yy_YY.utf8 et en_XX.UTF-8 (devise XDR) refusés, en_US.UTF-8 retenu",
           identical(locale_utf8_disponible(c("yy_YY.utf8", "en_XX.UTF-8", "en_US.UTF-8")), "en_US.UTF-8"))
  # Session en page de code 1252 (cas de locale_installation()) : currency_symbol
  # y est rendu en 1252 (a4), sous UTF-8 en c2 a4 ; seul int_curr_symbol (ASCII)
  # permet de comparer la session et un candidat.
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "English_United States.1252")))) {
    en_1252_ <- tryCatch(suppressWarnings(locale_utf8_disponible(c("yy_YY.utf8", "en_XX.UTF-8", "en_US.UTF-8"))),
                         finally = Sys.setlocale("LC_CTYPE", ctype_avant_))
    verifier("locale_utf8_disponible() : session English_United States.1252, noms inventés refusés, en_US.UTF-8 retenu",
             identical(en_1252_, "en_US.UTF-8") && identical(Sys.getlocale("LC_CTYPE"), ctype_avant_))
  } else verifier("locale_utf8_disponible() : English_United States.1252 non posable sous cette installation de Windows", FALSE)
  # Critère inactif quand LOCALE_INVENTEE est refusée (cas de glibc) : le nom
  # inventé redevient accepté, comme avant #109.
  inventee_ <- LOCALE_INVENTEE
  LOCALE_INVENTEE <<- "xx_XX.UTF-8@tablexp"
  inactif_ <- tryCatch(locale_utf8_disponible(c("xx_XX.UTF-8")), finally = LOCALE_INVENTEE <<- inventee_)
  verifier("locale_utf8_disponible() : LOCALE_INVENTEE refusée -> critère inactif, le nom inventé est accepté (glibc : rien ne change)",
           identical(inactif_, "xx_XX.UTF-8") && identical(LOCALE_INVENTEE, "xx_XX.UTF-8"))
  verifier("locale_utf8_disponible() : LC_CTYPE et LC_MONETARY restaurées après les cas Windows",
           identical(Sys.getlocale("LC_CTYPE"), ctype_avant_) && identical(Sys.getlocale("LC_MONETARY"), monnaie_avant_))
} else cat("  [sauté] locale_utf8_disponible() : en_US.UTF-8 retenue derrière un nom inventé, critère inactif : l'acceptation d'un nom inventé est propre à l'UCRT, plateforme hors Windows\n")
