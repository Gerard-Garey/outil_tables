# -----------------------------------------------------------------------------
#  tests/unitaires/test_empiler_stocks.R : adapter empiler_stocks() et
#  provenance de k fichiers (issue #181 ; ADR 0014, point 11 et question 5,
#  annotation du 08/11/2026, (d) ; ADR 0016, point 8.5 ; ADR 0007, annotation
#  du 08/11/2026, (a) ; avis de forme (0'') de B15, G9, G10, G12 ; M332)
#
#  Commit (5) : empiler_stocks(inventaires, evenements, colonne_type),
#  exportée, structure seule (ordre des lignes et des colonnes, complétion de
#  structure par NA de la classe de la colonne présente, aucune conversion),
#  refus nommés chacun levé seul ; forme rendue consommée telle quelle par la
#  recette `stocks_successifs` ; provenance de k >= 1 fichiers en vecteurs
#  parallèles : contrôle, fiche, écriture et relecture de la fiche, rejeu,
#  rapport figé.
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  valeurs écrites à la main (tables construites à la main, refus, motifs,
#  détails, champs de fiche) ; identité exacte (empilement de la forme
#  « stocks successifs » du simulateur scindée dans le test, construction sur
#  la table empilée contre la construction sur la forme du simulateur,
#  aller-retour de la fiche, rejeu). Graine du jeu simulé : 186L (< 1000,
#  exemptée du contrôle de #176, règle A5).
# -----------------------------------------------------------------------------

D <- as.Date
rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
rn <- function(x) { rownames(x) <- NULL; x }

# --- signature et export (G10) ---------------------------------------------------

verifier("empiler_stocks : exportée, signature (inventaires, evenements, colonne_type) sans défaut (valeur écrite à la main)",
         "empiler_stocks" %in% getNamespaceExports("tablexp") &&
           identical(names(formals(empiler_stocks)), c("inventaires", "evenements", "colonne_type")) &&
           all(vapply(formals(empiler_stocks), function(a) identical(a, quote(expr = )), logical(1L))))

# --- structure (valeurs écrites à la main) ---------------------------------------

INV <- data.frame(id = c(1L, 2L, 1L), di = D(c("2014-12-31", "2014-12-31", "2015-12-31")), sx = c("H", "F", "H"),
                  n = D(c("1960-01-01", "1970-05-05", "1960-01-01")), stringsAsFactors = FALSE)
EVT <- data.frame(id = c(2L, 3L), sx = c("F", "H"), n = D(c("1970-05-05", "1955-02-02")),
                  s = D(c("2015-06-01", "2015-09-30")), c = c("deces", "rachat"), stringsAsFactors = FALSE)
verifier("structure (valeurs écrites à la main) : inventaires puis événements dans l'ordre reçu ; colonnes des inventaires, puis des seuls événements, puis colonne_type ; NA de la classe de la colonne présente ; data.frame nu, row.names automatiques", {
  attendu <- data.frame(id = c(1L, 2L, 1L, 2L, 3L),
                        di = D(c("2014-12-31", "2014-12-31", "2015-12-31", NA, NA)),
                        sx = c("H", "F", "H", "F", "H"),
                        n = D(c("1960-01-01", "1970-05-05", "1960-01-01", "1970-05-05", "1955-02-02")),
                        s = D(c(NA, NA, NA, "2015-06-01", "2015-09-30")),
                        c = c(NA, NA, NA, "deces", "rachat"),
                        t = c("inventaire", "inventaire", "inventaire", "evenement", "evenement"),
                        stringsAsFactors = FALSE)
  identical(empiler_stocks(INV, EVT, "t"), attendu)
})
verifier("structure (valeurs écrites à la main) : table d'inventaires ou d'événements sans ligne admise ; aucune ligne créée", {
  a <- empiler_stocks(INV[0L, ], EVT, "t")
  b <- empiler_stocks(INV, EVT[0L, ], "t")
  identical(nrow(a), 2L) && identical(a$t, rep("evenement", 2L)) && identical(a$di, D(c(NA_character_, NA_character_))) &&
    identical(names(a), c("id", "di", "sx", "n", "s", "c", "t")) &&
    identical(nrow(b), 3L) && identical(b$t, rep("inventaire", 3L)) && identical(b$c, rep(NA_character_, 3L)) &&
    identical(nrow(empiler_stocks(INV[0L, ], EVT[0L, ], "t")), 0L)
})
verifier("structure (valeurs écrites à la main) : aucune conversion ; facteur commun aux mêmes niveaux, Date à stockage integer, colonne propre à une table de classe facteur : classe, niveaux et stockage gardés", {
  inv <- transform(INV, sx = factor(sx, levels = c("F", "H")))
  inv$di <- structure(as.integer(unclass(inv$di)), class = "Date")
  inv$n <- structure(as.integer(unclass(inv$n)), class = "Date")
  evt <- transform(EVT, sx = factor(sx, levels = c("F", "H")), c = factor(c, levels = c("deces", "rachat", "autre")))
  evt$n <- structure(as.integer(unclass(evt$n)), class = "Date")
  x <- empiler_stocks(inv, evt, "t")
  identical(x$sx, factor(c("H", "F", "H", "F", "H"), levels = c("F", "H"))) &&
    identical(typeof(x$di), "integer") && identical(class(x$di), "Date") && identical(x$di[4:5], x$di[c(NA_integer_, NA_integer_)]) &&
    identical(x$n, structure(c(as.integer(unclass(INV$n)), as.integer(unclass(EVT$n))), class = "Date")) &&
    identical(x$c, factor(c(NA, NA, NA, "deces", "rachat"), levels = c("deces", "rachat", "autre"))) &&
    identical(x$t, c(rep("inventaire", 3L), rep("evenement", 2L)))
})
verifier("structure (valeurs écrites à la main) : colonnes hors rôles recopiées comme les autres ; tables d'entrée inchangées", {
  inv <- INV; inv$extra <- c(10, 20, 30)
  inv0 <- inv; evt0 <- EVT
  x <- empiler_stocks(inv, EVT, "t")
  identical(x$extra, c(10, 20, 30, NA, NA)) && identical(inv, inv0) && identical(EVT, evt0) &&
    identical(names(x), c("id", "di", "sx", "n", "extra", "s", "c", "t"))
})

# --- refus (G10 ; chacun levé seul, valeurs écrites à la main) ---------------------

CAS_REFUS <- list(
  list("inventaires hors data.frame", list(as.list(INV), EVT, "t"), "empilement_table_invalide",
       function(r) identical(r$details$table, "inventaires") && identical(r$details$classe, list(inventaires = "list"))),
  list("evenements hors data.frame (matrice)", list(INV, as.matrix(EVT), "t"), "empilement_table_invalide",
       function(r) identical(r$details$table, "evenements")),
  list("les deux tables hors data.frame", list(NULL, 1, "t"), "empilement_table_invalide",
       function(r) identical(r$details$table, c("inventaires", "evenements"))),
  list("colonne_type de longueur 2", list(INV, EVT, c("a", "b")), "empilement_colonne_type_invalide",
       function(r) identical(r$details$colonne_type, c("a", "b"))),
  list("colonne_type NA", list(INV, EVT, NA_character_), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type vide", list(INV, EVT, ""), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type nommée", list(INV, EVT, c(a = "t")), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type numérique", list(INV, EVT, 1), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type d'encodage bytes", list(INV, EVT, `Encoding<-`("t\xe9", "bytes")),
       "empilement_colonne_type_invalide", function(r) TRUE),
  list("nom de colonne en double dans les inventaires", list(setNames(INV, c("id", "di", "id", "n")), EVT, "t"),
       "empilement_noms_invalides", function(r) identical(r$details, list(table = "inventaires", colonne = "id"))),
  list("nom de colonne vide dans les événements", list(INV, setNames(EVT, c("id", "", "n", "s", "c")), "t"),
       "empilement_noms_invalides", function(r) identical(r$details, list(table = "evenements", colonne = ""))),
  list("nom de colonne NA dans les événements", list(INV, setNames(EVT, c("id", NA, "n", "s", "c")), "t"),
       "empilement_noms_invalides", function(r) identical(r$details, list(table = "evenements", colonne = NA_character_))),
  list("colonne_type déjà présente dans les événements", list(INV, EVT, "c"), "empilement_colonne_type_presente",
       function(r) identical(r$details, list(colonne = "c", table = "evenements"))),
  list("colonne_type déjà présente dans les deux tables", list(INV, EVT, "sx"), "empilement_colonne_type_presente",
       function(r) identical(r$details, list(colonne = "sx", table = c("inventaires", "evenements")))),
  list("colonne commune integer contre double", list(transform(INV, id = as.numeric(id)), EVT, "t"),
       "empilement_colonnes_discordantes", function(r) identical(r$details, list(colonne = "id"))),
  list("colonne commune Date contre character, et facteur contre character", list(transform(INV, sx = factor(sx)),
       transform(EVT, n = as.character(n)), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = c("sx", "n")))),
  list("colonne commune Date à stockage double contre integer",
       list(INV, within(EVT, n <- structure(as.integer(unclass(n)), class = "Date")), "t"),
       "empilement_colonnes_discordantes", function(r) identical(r$details, list(colonne = "n")) &&
         grepl("stockage double", conditionMessage(r), fixed = TRUE)),
  list("facteur commun aux niveaux différents", list(transform(INV, sx = factor(sx, levels = c("F", "H"))),
       transform(EVT, sx = factor(sx, levels = c("H", "F"))), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "sx")) && grepl("attribut(s) diff", conditionMessage(r), fixed = TRUE) &&
         grepl("levels", conditionMessage(r), fixed = TRUE)),
  list("POSIXct commun de fuseaux différents", list(within(INV, h <- as.POSIXct("2015-01-01 12:00", tz = "UTC") + 0:2),
       within(EVT, h <- as.POSIXct("2015-01-01 12:00", tz = "Europe/Paris") + 0:1), "t"),
       "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "h")) && grepl("tzone", conditionMessage(r), fixed = TRUE)),
  list("difftime commun d'unités différentes", list(within(INV, u <- as.difftime(1:3, units = "days")),
       within(EVT, u <- as.difftime(1:2, units = "hours")), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "u")) && grepl("units", conditionMessage(r), fixed = TRUE)),
  list("facteur ordonné contre facteur simple, mêmes niveaux", list(transform(INV, sx = factor(sx, levels = c("F", "H"),
       ordered = TRUE)), transform(EVT, sx = factor(sx, levels = c("F", "H"))), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "sx")) && grepl("class", conditionMessage(r), fixed = TRUE)),
  list("colonne matrice dans les inventaires", list(within(INV, m <- matrix(1:6, 3L)), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "m"))),
  list("colonne liste dans les inventaires", list(within(INV, l <- I(list(1, 2, 3))), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "l"))),
  list("colonne data.frame dans les inventaires", list(local({ x <- INV; x$d <- data.frame(a = 1:3); x }), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "d"))),
  list("colonne raw dans les inventaires", list(within(INV, r <- as.raw(1:3)), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "r"))),
  list("colonne matrice dans les événements", list(INV, within(EVT, m <- matrix(1:4, 2L)), "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "evenements", colonne = "m"))),
  list("colonne liste dans les événements", list(INV, within(EVT, l <- I(list(1, 2))), "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "evenements", colonne = "l"))),
  list("colonne data.frame dans les événements", list(INV, local({ x <- EVT; x$d <- data.frame(a = 1:2); x }), "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "evenements", colonne = "d"))),
  list("colonnes raw et matrice dans les événements", list(INV, within(EVT, { r <- as.raw(1:2); m <- matrix(1:4, 2L) }), "t"),
       "empilement_colonne_non_atomique",
       function(r) identical(r$details$table, "evenements") && setequal(r$details$colonne, c("r", "m"))))
for (cas in CAS_REFUS)
  verifier(sprintf("refus (valeur écrite à la main) : %s -> donnees/%s", cas[[1L]], cas[[3L]]), {
    r <- rf(do.call(empiler_stocks, cas[[2L]]))
    k <- controle(r, "donnees", cas[[3L]])
    if (!isTRUE(k)) k else if (!isTRUE(cas[[4L]](r))) "détails ou message inattendus" else TRUE
  })
verifier("structure (valeurs écrites à la main) : mêmes attributs conservés admis (POSIXct de même fuseau, difftime de même unité, facteur ordonné aux mêmes niveaux) ; classe, fuseau, unité et niveaux gardés", {
  h <- as.POSIXct("2015-01-01 12:00", tz = "Europe/Paris") + 0:4
  inv <- within(INV, { h <- h[1:3]; u <- as.difftime(1:3, units = "hours"); o <- factor(sx, levels = c("F", "H"), ordered = TRUE) })
  evt <- within(EVT, { h <- h[4:5]; u <- as.difftime(4:5, units = "hours"); o <- factor(sx, levels = c("F", "H"), ordered = TRUE) })
  x <- empiler_stocks(inv, evt, "t")
  identical(x$h, h) && identical(x$u, as.difftime(1:5, units = "hours")) &&
    identical(x$o, factor(c("H", "F", "H", "F", "H"), levels = c("F", "H"), ordered = TRUE))
})
verifier("refus (valeur écrite à la main) : ordre table, colonne_type, noms, colonne non atomique, colonne_type présente, discordance (premier défaut seul)", {
  identical(rf(empiler_stocks(1, EVT, 1))$motif, "empilement_table_invalide") &&
    identical(rf(empiler_stocks(setNames(INV, c("a", "a", "b", "c")), EVT, 1))$motif, "empilement_colonne_type_invalide") &&
    identical(rf(empiler_stocks(setNames(INV, c("a", "a", "sx", "c")), EVT, "sx"))$motif, "empilement_noms_invalides") &&
    identical(rf(empiler_stocks(within(INV, m <- matrix(1:6, 3L)), setNames(EVT, c("a", "a", "b", "c", "d")), "sx"))$motif,
              "empilement_noms_invalides") &&
    identical(rf(empiler_stocks(within(INV, m <- matrix(1:6, 3L)), EVT, "sx"))$motif, "empilement_colonne_non_atomique") &&
    identical(rf(empiler_stocks(transform(INV, id = as.numeric(id)), EVT, "sx"))$motif, "empilement_colonne_type_presente")
})

# --- liste des colonnes et noms de longueurs différentes (#355, F355) -------------
#
# Spécification d'actuary (commentaire 7124469548 de #355) : sous une classe
# dont length() rend 1, `[.data.frame` rend une liste d'un élément munie de
# tous les noms, puis reclassée « data.frame ». Avant #355 : erreur R
# « subscript out of bounds » (.subset2(tab, j)) ; un environnement classé
# « data.frame » : erreur R « a character vector argument expected »
# (Encoding(n)) (mesuré à d3b11ac). Après : refus empilement_table_invalide,
# première table en défaut (inventaires, puis evenements), avant le contrôle
# de colonne_type ; l'environnement reçoit le texte existant (garde
# is.list()). Un sous-processus Rscript par cas (consigne de #355 : la même
# entrée arrête R dans table_reference()) ; résultat rendu par saveRDS().
# Valeurs écrites à la main (texte de la spécification).
local({
  # Avertissements R comptés au second appel (comme au test de #344) : sous
  # LC_ALL=C, le premier usage des chaînes UTF-8 du package avertit « strings
  # not representable in native encoding will be translated to UTF-8 », appel
  # nominal compris (mesuré en sous-processus).
  script <- tempfile("e355_", fileext = ".R")
  on.exit(unlink(script))
  writeLines(c(
    "a <- commandArgs(TRUE)",
    sprintf("suppressPackageStartupMessages(library(tablexp, lib.loc = %s))", deparse(.harnais$bibliotheque)),
    "length.k355 <- function(x) 1L",
    "incoherente <- function(d) { x <- structure(d, class = c('k355', 'data.frame'))[-2L, , drop = FALSE]; class(x) <- 'data.frame'; x }",
    "env_df <- function() { e <- new.env(); class(e) <- 'data.frame'; e }",
    "I2 <- data.frame(id = 1:3, sx = c('H', 'F', 'H'), stringsAsFactors = FALSE)",
    "E3 <- data.frame(id = 2:4, sx = c('F', 'H', 'H'), c = c('deces', 'rachat', 'deces'), stringsAsFactors = FALSE)",
    "args <- switch(a[2L],",
    "  inventaires = list(incoherente(I2), E3, 't'), evenements = list(I2, incoherente(E3), 't'),",
    "  deux = list(incoherente(I2), incoherente(E3), 't'), colonne_type = list(incoherente(I2), E3, 1),",
    "  env_inventaires = list(env_df(), E3, 't'), env_evenements = list(I2, env_df(), 't'),",
    "  env_et_incoherente = list(env_df(), incoherente(E3), 't'))",
    "appel <- function() tryCatch({ do.call(empiler_stocks, args); NULL }, refus_tablexp = function(e) e, error = function(e) e)",
    "r <- suppressWarnings(appel())",
    "n_avert <- 0L",
    "withCallingHandlers(appel(), warning = function(w) { n_avert <<- n_avert + 1L; invokeRestart('muffleWarning') })",
    "saveRDS(list(classe = class(r), module = r$module, motif = r$motif,",
    "             message = if (!is.null(r)) conditionMessage(r), details = r$details, avertissements = n_avert), a[1L])"),
    script)
  executer <- function(k) {
    sortie <- tempfile("e355_", fileext = ".rds")
    on.exit(unlink(sortie))
    p <- executer_rscript(script, c(shQuote(sortie), k))
    if (!file.exists(sortie)) return(sprintf("aucun résultat : code de sortie %d (%s)", p$statut,
                                             paste(utils::tail(p$sortie, 3L), collapse = " | ")))
    readRDS(sortie)
  }
  incoherente <- function(t, noms) list(
    paste0("Empilement des stocks successifs : la table « ", t, " » est un data.frame mal formé, dont la ",
           "liste des colonnes et les noms de colonnes n'ont pas la même longueur (liste de 1 colonne(s) pour ", noms,
           " nom(s) de colonne ; classe data.frame). Le moteur ne choisit pas entre ces décomptes, et ne les corrige ",
           "pas : passer un data.frame dont la liste des colonnes et les noms de colonnes ont la même longueur."),
    list(table = t, classe = setNames(list("data.frame"), t), nombre_colonnes = 1L, nombre_noms = noms))
  environnement <- function(t) list(
    paste0("Empilement des stocks successifs : un data.frame est attendu pour « ", t,
           " » (classe(s) reçue(s) : data.frame) ; le moteur ne lit pas de fichier."),
    list(table = t, classe = setNames(list("data.frame"), t)))
  cas <- list(
    list("inventaires de 1 colonne pour 2 noms", "inventaires", incoherente("inventaires", 2L)),
    list("evenements de 1 colonne pour 3 noms", "evenements", incoherente("evenements", 3L)),
    list("les deux tables incohérentes (première table en défaut)", "deux", incoherente("inventaires", 2L)),
    list("inventaires incohérente et colonne_type invalide (cohérence jugée avant colonne_type)", "colonne_type",
         incoherente("inventaires", 2L)),
    list("inventaires environnement classé data.frame (avant : erreur R)", "env_inventaires", environnement("inventaires")),
    list("evenements environnement classé data.frame (avant : erreur R)", "env_evenements", environnement("evenements")),
    list("inventaires environnement, evenements incohérente (table hors data.frame jugée d'abord)", "env_et_incoherente",
         environnement("inventaires")))
  for (c1 in cas)
    verifier(sprintf("#355 refus (valeur écrite à la main) : %s -> donnees/empilement_table_invalide, message et details exacts, une ligne marquée UTF-8, 0 avertissement R, sous-processus", c1[[1L]]), {
      x <- executer(c1[[2L]])
      if (is.character(x)) x
      else if (!"refus_tablexp" %in% x$classe) sprintf("condition %s : %s", paste(x$classe, collapse = "/"),
                                                       paste(x$message, collapse = ""))
      else if (!identical(x$module, "donnees") || !identical(x$motif, "empilement_table_invalide"))
        sprintf("%s / %s", x$module, x$motif)
      else if (!identical(x$message, c1[[3L]][[1L]])) paste("message :", x$message)
      else if (grepl("[\n\v\f]", x$message) || !validUTF8(x$message) || !identical(Encoding(x$message), "UTF-8"))
        "message sur plusieurs lignes ou non marqué UTF-8"
      else if (!identical(x$details, c1[[3L]][[2L]])) paste("details :", paste(deparse(x$details), collapse = " "))
      else if (x$avertissements != 0L) sprintf("%d avertissement(s) R", x$avertissements)
      else TRUE
    })
})

# --- nombre de lignes et longueur de chaque colonne (#368) ----------------------------
#
# Spécification d'actuary (avis de l'étape (0') de B41) : le nombre de lignes lu
# sans dispatch (.row_names_info(x, 2L)) doit être celui de chaque colonne ; sinon
# refus empilement_table_invalide, première table en défaut (inventaires, puis
# evenements), dans la boucle de #355, avant le contrôle de colonne_type. Avant
# #368 (mesuré à 992749c) : une colonne plus courte était complétée par des NA
# (six lignes dont sx = H, NA, NA), une plus longue tronquée, une table sans
# attribut row.names perdait ses lignes, des noms de lignes illisibles
# (row.names = c(NA_integer_, NA_integer_)) arrêtaient R (« argument must be
# coercible to non-negative integer »). Une table vraiment vide (colonnes de
# longueur 0) reste admise. Messages et details écrits à la main (texte de
# l'avis) ; lignes_colonne est un double (décompte lu par decomptes_colonnes()).
local({
  I3 <- data.frame(id = 1:3, sx = c("H", "F", "H"), stringsAsFactors = FALSE)
  E3 <- data.frame(id = 2:4, sx = c("F", "H", "H"), c = c("deces", "rachat", "deces"), stringsAsFactors = FALSE)
  structure_df <- function(l, row.names) {
    x <- structure(l, class = "data.frame")
    if (!missing(row.names)) attr(x, "row.names") <- row.names
    x
  }
  sans_noms <- structure_df(as.list(I3))
  sans_noms_e <- structure_df(as.list(E3))
  # Avertissements R comptés au second appel (comme au test de #355).
  appel <- function(i, e, type = "t") {
    f <- function() tryCatch(empiler_stocks(i, e, type), refus_tablexp = function(c) c, error = function(c) c)
    r <- suppressWarnings(f())
    n <- 0L
    withCallingHandlers(f(), warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") })
    list(r = r, n = n)
  }
  debut <- function(t) paste0("Empilement des stocks successifs : la table « ", t, " » est un data.frame mal formé, dont ",
                              "les noms de lignes et les colonnes ne donnent pas un même nombre de lignes (")
  fin <- paste0(" ; classe data.frame). Le moteur ne choisit pas entre ces décomptes, et ne les corrige pas (une colonne ",
                "plus courte serait complétée par des valeurs manquantes, une plus longue tronquée) : passer un data.frame ",
                "dont les noms de lignes et chaque colonne comptent le même nombre de lignes.")
  cas <- list(
    list("colonne sx de 1 valeur pour 3 lignes (inventaires)",
         structure_df(list(id = 1:3, sx = "H"), c(NA_integer_, -3L)), E3, "t",
         paste0(debut("inventaires"), "noms de lignes : 3 ; colonne « sx » : 1", fin),
         list(table = "inventaires", classe = list(inventaires = "data.frame"), lignes = 3L, colonne = "sx",
              position = 2L, lignes_colonne = 1)),
    list("colonne sx de 4 valeurs pour 3 lignes (inventaires)",
         structure_df(list(id = 1:3, sx = c("H", "F", "H", "F")), c(NA_integer_, -3L)), E3, "t",
         paste0(debut("inventaires"), "noms de lignes : 3 ; colonne « sx » : 4", fin),
         list(table = "inventaires", classe = list(inventaires = "data.frame"), lignes = 3L, colonne = "sx",
              position = 2L, lignes_colonne = 4)),
    list("colonne c de 2 valeurs pour 3 lignes (evenements)", I3,
         structure_df(list(id = 2:4, sx = c("F", "H", "H"), c = c("deces", "rachat")), c(NA_integer_, -3L)), "t",
         paste0(debut("evenements"), "noms de lignes : 3 ; colonne « c » : 2", fin),
         list(table = "evenements", classe = list(evenements = "data.frame"), lignes = 3L, colonne = "c",
              position = 3L, lignes_colonne = 2)),
    list("inventaires sans attribut row.names, colonnes de 3 valeurs (avant : trois lignes perdues)", sans_noms, E3, "t",
         paste0(debut("inventaires"), "noms de lignes : 0 ; colonne « id » : 3 ; colonne « sx » : 3", fin),
         list(table = "inventaires", classe = list(inventaires = "data.frame"), lignes = 0L, colonne = c("id", "sx"),
              position = 1:2, lignes_colonne = c(3, 3))),
    list("evenements avec row.names = integer(0), colonnes de 3 valeurs", I3,
         structure_df(as.list(E3), integer(0)), "t",
         paste0(debut("evenements"), "noms de lignes : 0 ; colonne « id » : 3 ; colonne « sx » : 3 ; colonne « c » : 3", fin),
         list(table = "evenements", classe = list(evenements = "data.frame"), lignes = 0L, colonne = c("id", "sx", "c"),
              position = 1:3, lignes_colonne = c(3, 3, 3))),
    list("inventaires aux noms de lignes illisibles (avant : erreur R)",
         structure_df(list(id = 1:2, sx = c("H", "F")), c(NA_integer_, NA_integer_)), E3, "t",
         paste0(debut("inventaires"), "noms de lignes : illisibles", fin),
         list(table = "inventaires", classe = list(inventaires = "data.frame"), lignes = NA_integer_,
              colonne = character(0), position = integer(0), lignes_colonne = numeric(0))),
    list("les deux tables en défaut (première table en défaut : inventaires)", sans_noms, sans_noms_e, "t",
         paste0(debut("inventaires"), "noms de lignes : 0 ; colonne « id » : 3 ; colonne « sx » : 3", fin),
         list(table = "inventaires", classe = list(inventaires = "data.frame"), lignes = 0L, colonne = c("id", "sx"),
              position = 1:2, lignes_colonne = c(3, 3))),
    list("inventaires en défaut et colonne_type invalide (nombre de lignes jugé avant colonne_type)", sans_noms, E3, 1,
         paste0(debut("inventaires"), "noms de lignes : 0 ; colonne « id » : 3 ; colonne « sx » : 3", fin),
         list(table = "inventaires", classe = list(inventaires = "data.frame"), lignes = 0L, colonne = c("id", "sx"),
              position = 1:2, lignes_colonne = c(3, 3))))
  for (c1 in cas)
    verifier(sprintf("#368 refus (valeur écrite à la main) : %s -> donnees/empilement_table_invalide, message et details exacts, une ligne marquée UTF-8, aucune erreur R, 0 avertissement R", c1[[1L]]), {
      a <- appel(c1[[2L]], c1[[3L]], c1[[4L]])
      x <- a$r
      if (!inherits(x, "refus_tablexp")) {
        if (inherits(x, "error")) paste("erreur R :", conditionMessage(x)) else "aucun refus levé"
      }
      else if (!identical(x$module, "donnees") || !identical(x$motif, "empilement_table_invalide"))
        sprintf("%s / %s", x$module, x$motif)
      else if (!identical(conditionMessage(x), c1[[5L]])) paste("message :", conditionMessage(x))
      else if (grepl("[\n\v\f]", conditionMessage(x)) || !validUTF8(conditionMessage(x)) ||
               !identical(Encoding(conditionMessage(x)), "UTF-8")) "message sur plusieurs lignes ou non marqué UTF-8"
      else if (!identical(x$details, c1[[6L]])) paste("details :", paste(deparse(x$details), collapse = " "))
      else if (a$n != 0L) sprintf("%d avertissement(s) R", a$n)
      else TRUE
    })
  # Contre-épreuves : tables bien formées, dont la table vraiment vide (colonnes de
  # longueur 0 : aucune ligne d'inventaire, trois lignes d'événement) ; valeurs
  # écrites à la main.
  verifier("#368 table vraiment vide (inventaires de colonnes de longueur 0) : admise, trois lignes d'événement, aucune ligne perdue ni ajoutée (valeur écrite à la main)", {
    a <- appel(I3[0L, ], E3)
    identical(a$r, data.frame(id = 2:4, sx = c("F", "H", "H"), c = c("deces", "rachat", "deces"),
                              t = rep("evenement", 3L), stringsAsFactors = FALSE)) && a$n == 0L
  })
  verifier("#368 témoin : deux data.frame bien formés rendent la table de six lignes écrite à la main, colonne c complétée par NA aux trois lignes d'inventaire (complétion de structure, aucune ligne créée)", {
    a <- appel(I3, E3)
    identical(a$r, data.frame(id = c(1:3, 2:4), sx = c("H", "F", "H", "F", "H", "H"),
                              c = c(NA, NA, NA, "deces", "rachat", "deces"),
                              t = rep(c("inventaire", "evenement"), each = 3L), stringsAsFactors = FALSE)) && a$n == 0L
  })
  # Colonne liste classée dont la méthode length() ou dim() lève une erreur R
  # (revue finale de B41 ; #368 comptait les lignes d'une telle colonne par NROW(),
  # avec dispatch : erreur R nue, là où avant #368 (mesuré à 3ed3973) le refus
  # empilement_colonne_non_atomique était levé). Une telle colonne n'est pas
  # comptée : son refus est celui d'avant, une colonne atomique discordante de la
  # même table reste refusée par #368. Méthodes S3 de session dans globalenv(),
  # retirées ensuite ; les data.frame sont formés avant leur pose (leur
  # construction appelle length()). Valeurs écrites à la main.
  k <- structure(list(1, 2, 3), class = "k368")
  k2 <- structure(list(1, 2), class = "k368")
  I_k <- structure_df(list(id = 1:3, l = k), c(NA_integer_, -3L))
  E_k <- structure_df(list(id = 2:4, l = k), c(NA_integer_, -3L))
  I_k_court <- structure_df(list(id = 1:3, l = k2), c(NA_integer_, -3L))
  I_k_et_court <- structure_df(list(id = 1:3, l = k, sx = "H"), c(NA_integer_, -3L))
  methodes <- list(length = list(length.k368 = function(x) stop("length.k368 : erreur de session")),
                   dim = list(dim.k368 = function(x) stop("dim.k368 : erreur de session")),
                   `length et dim` = list(length.k368 = function(x) stop("length.k368 : erreur de session"),
                                          dim.k368 = function(x) stop("dim.k368 : erreur de session")))
  motif_non_atomique <- function(a, table) {
    x <- a$r
    if (!inherits(x, "refus_tablexp")) {
      if (inherits(x, "error")) paste("erreur R :", conditionMessage(x)) else "aucun refus levé"
    }
    else if (!identical(x$module, "donnees") || !identical(x$motif, "empilement_colonne_non_atomique"))
      sprintf("%s / %s", x$module, x$motif)
    else if (!identical(x$details, list(table = table, colonne = "l"))) paste("details :", paste(deparse(x$details), collapse = " "))
    else if (a$n != 0L) sprintf("%d avertissement(s) R", a$n)
    else TRUE
  }
  for (m in names(methodes)) {
    for (n in names(methodes[[m]])) assign(n, methodes[[m]][[n]], envir = globalenv())
    ap <- list(inv = appel(I_k, E3), eve = appel(I3, E_k), court = appel(I_k_court, E3), mixte = appel(I_k_et_court, E3))
    for (n in names(methodes[[m]])) rm(list = n, envir = globalenv())
    verifier(sprintf("#368 colonne liste classée l, méthode %s en erreur, dans inventaires : refus empilement_colonne_non_atomique (comme avant #368), aucune erreur R", m),
             motif_non_atomique(ap$inv, "inventaires"))
    verifier(sprintf("#368 colonne liste classée l, méthode %s en erreur, dans evenements : refus empilement_colonne_non_atomique, aucune erreur R", m),
             motif_non_atomique(ap$eve, "evenements"))
    verifier(sprintf("#368 colonne liste classée l de 2 valeurs pour 3 lignes, méthode %s en erreur : refus empilement_colonne_non_atomique (non comptée), aucune erreur R", m),
             motif_non_atomique(ap$court, "inventaires"))
    verifier(sprintf("#368 colonne liste classée l et colonne atomique sx de 1 valeur pour 3 lignes, méthode %s en erreur : refus #368, qui cite sx seule (acquis conservé), aucune erreur R", m), {
      x <- ap$mixte$r
      if (!inherits(x, "refus_tablexp")) {
        if (inherits(x, "error")) paste("erreur R :", conditionMessage(x)) else "aucun refus levé"
      }
      else if (!identical(x$motif, "empilement_table_invalide")) x$motif
      else if (!identical(x$details$colonne, "sx") || !identical(x$details$position, 3L) ||
               !identical(x$details$lignes_colonne, 1)) paste("details :", paste(deparse(x$details), collapse = " "))
      else if (ap$mixte$n != 0L) sprintf("%d avertissement(s) R", ap$mixte$n)
      else TRUE
    })
  }
  verifier("#368 les méthodes length.k368 et dim.k368 de session sont retirées de globalenv()",
           !any(c("length.k368", "dim.k368") %in% ls(globalenv(), all.names = TRUE)))
})

# --- classe d'encodage bytes (#366) ---------------------------------------------------
#
# Spécification d'actuary (avis de l'étape (0') de B41) : une table dont la classe
# porte une chaîne d'encodage « bytes » est refusée en tête de fonction, avant
# is.data.frame() (motif empilement_table_invalide, toutes les tables en défaut
# citées). Avant #366 (mesuré à 41093de) : erreur R « translating strings with
# "bytes" encoding is not allowed », appel inherits(x, "data.frame") dans
# is.data.frame(), que la table soit passée en inventaires ou en evenements. Une
# classe ASCII n'est pas marquée « bytes » (Encoding() rend "unknown") : le cas
# emploie une classe non ASCII. Messages et details écrits à la main (texte de
# l'avis) ; la classe est citée sous le repère de neutraliser_citation().
local({
  I3 <- data.frame(id = 1:3, sx = c("H", "F", "H"), stringsAsFactors = FALSE)
  E3 <- data.frame(id = 2:4, sx = c("F", "H", "H"), c = c("deces", "rachat", "deces"), stringsAsFactors = FALSE)
  classe_b <- c("k\xe9", "data.frame")
  Encoding(classe_b) <- c("bytes", "unknown")
  avec_classe <- function(x) { class(x) <- classe_b; x }
  Ib <- avec_classe(I3)
  Eb <- avec_classe(E3)
  appel <- function(i, e, type = "t") {
    f <- function() tryCatch(empiler_stocks(i, e, type), refus_tablexp = function(c) c, error = function(c) c)
    r <- suppressWarnings(f())
    n <- 0L
    withCallingHandlers(f(), warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") })
    list(r = r, n = n)
  }
  msg <- function(t) paste0("Empilement des stocks successifs : la classe de ", t, " porte une chaîne d'encodage « bytes » ",
                            "(classe(s) reçue(s) : <chaîne d'encodage bytes>/data.frame), non convertible en UTF-8 sans ",
                            "deviner : la table n'est pas lue. Passer, pour chaque table, un data.frame dont la classe ne ",
                            "porte aucune chaîne d'encodage bytes.")
  msg2 <- paste0("Empilement des stocks successifs : la classe de « inventaires », « evenements » porte une chaîne ",
                 "d'encodage « bytes » (classe(s) reçue(s) : <chaîne d'encodage bytes>/data.frame ; ",
                 "<chaîne d'encodage bytes>/data.frame), non convertible en UTF-8 sans deviner : la table n'est pas lue. ",
                 "Passer, pour chaque table, un data.frame dont la classe ne porte aucune chaîne d'encodage bytes.")
  cas <- list(
    list("inventaires de classe bytes", Ib, E3, msg("« inventaires »"),
         list(table = "inventaires", classe = list(inventaires = classe_b))),
    list("evenements de classe bytes", I3, Eb, msg("« evenements »"),
         list(table = "evenements", classe = list(evenements = classe_b))),
    list("les deux tables de classe bytes (toutes citées)", Ib, Eb, msg2,
         list(table = c("inventaires", "evenements"), classe = list(inventaires = classe_b, evenements = classe_b))))
  verifier("#366 une classe non ASCII est marquée bytes, une classe ASCII ne l'est pas (hypothèse du test, valeur écrite à la main)",
           identical(Encoding(class(Ib)), c("bytes", "unknown")) && !tablexp:::classe_bytes(I3) && tablexp:::classe_bytes(Ib) &&
             !tablexp:::classe_bytes(structure(I3, class = c("k", "data.frame"))))
  for (c1 in cas)
    verifier(sprintf("#366 refus (valeur écrite à la main) : %s -> donnees/empilement_table_invalide, message et details exacts, une ligne marquée UTF-8, aucune erreur R, 0 avertissement R", c1[[1L]]), {
      a <- appel(c1[[2L]], c1[[3L]])
      x <- a$r
      if (!inherits(x, "refus_tablexp")) {
        if (inherits(x, "error")) paste("erreur R :", conditionMessage(x)) else "aucun refus levé"
      }
      else if (!identical(x$module, "donnees") || !identical(x$motif, "empilement_table_invalide"))
        sprintf("%s / %s", x$module, x$motif)
      else if (!identical(conditionMessage(x), c1[[4L]])) paste("message :", conditionMessage(x))
      else if (grepl("[\n\v\f]", conditionMessage(x)) || !validUTF8(conditionMessage(x)) ||
               !identical(Encoding(conditionMessage(x)), "UTF-8")) "message sur plusieurs lignes ou non marqué UTF-8"
      else if (!identical(x$details, c1[[5L]])) paste("details :", paste(deparse(x$details), collapse = " "))
      else if (a$n != 0L) sprintf("%d avertissement(s) R", a$n)
      else TRUE
    })
  verifier("#366 témoin : deux data.frame de classe sans chaîne bytes (dont une classe ASCII supplémentaire) rendent la table de six lignes écrite à la main, aucun refus ni avertissement R", {
    a <- appel(I3, E3)
    ascii <- I3
    class(ascii) <- c("k", "data.frame")
    b <- appel(ascii, E3)
    attendu <- data.frame(id = c(1:3, 2:4), sx = c("H", "F", "H", "F", "H", "H"),
                          c = c(NA, NA, NA, "deces", "rachat", "deces"),
                          t = rep(c("inventaire", "evenement"), each = 3L), stringsAsFactors = FALSE)
    identical(a$r, attendu) && a$n == 0L && identical(b$r, attendu) && b$n == 0L
  })
  verifier("#366 class() et citer() ne lèvent pas d'erreur sur une classe bytes (précédent #158) : repère rendu, valeur écrite à la main", {
    r <- tryCatch(tablexp:::citer(class(Ib)), error = function(e) e)
    identical(r, c("<chaîne d'encodage bytes>", "data.frame"))
  })
})

# --- forme consommée par la recette `stocks_successifs` (identité exacte) ------------

PER_E <- list(debut = D("2015-01-01"), fin = D("2017-12-31"))
G_E <- D(c("2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31"))
LOI_E <- local({
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
})
PF_E <- list(mode = "uniforme", n = 2000, proportion_hommes = 0.5, age_min = 30, age_max = 80, origine = D("2010-01-01"))
SIM_E <- simuler_portefeuille(LOI_E, PF_E, PER_E, 186L, forme = list(nom = "stocks_successifs", inventaires = G_E))
attr(SIM_E, "simulation") <- NULL
EST_INV <- SIM_E$type_ligne == "inventaire"
INV_E <- rn(SIM_E[EST_INV, c("id_contrat", "id_tete", "date_inventaire", "sexe", "date_naissance")])
EVT_E <- rn(SIM_E[!EST_INV, c("id_contrat", "id_tete", "sexe", "date_naissance", "date_sortie", "cause_sortie")])
EMP_E <- empiler_stocks(INV_E, EVT_E, "type_ligne")
RS_E <- c(id_tete = "id_tete", type_ligne = "type_ligne", date_inventaire = "date_inventaire", sexe = "sexe",
          date_naissance = "date_naissance", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
PLAN_E <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER_E, delai_declaration = 0,
               inventaires = G_E, segmentation = "sexe",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE,
                                               entree_entre_inventaires = "milieu"),
                             brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95)))
verifier("forme (identité exacte) : la forme stocks du simulateur, scindée par type_ligne dans le test puis empilée, est identical() à ses lignes rangées inventaires puis événements, colonnes dans l'ordre de l'empilement", {
  o <- c(which(EST_INV), which(!EST_INV))
  attendu <- rn(SIM_E[o, c("id_contrat", "id_tete", "date_inventaire", "sexe", "date_naissance", "date_sortie",
                           "cause_sortie", "type_ligne")])
  sum(EST_INV) > 0L && sum(!EST_INV) > 0L && identical(EMP_E, attendu)
})
X_SIM <- construire_table(SIM_E, RS_E, PLAN_E)
X_EMP <- construire_table(EMP_E, RS_E, PLAN_E)
verifier("forme (identité exacte) : construire_table() sur la table empilée, recette stocks_successifs, tables d'exposition et de brut et table finale identical() à celles de la forme du simulateur ; seules les empreintes des données et du résultat diffèrent dans la fiche", {
  fs <- X_SIM$fiche; fe <- X_EMP$fiche
  ch <- setdiff(names(fs), "empreintes")
  identical(fe$recette, "stocks_successifs") &&
    identical(X_EMP$etapes$exposition$resultat, X_SIM$etapes$exposition$resultat) &&
    identical(X_EMP$etapes$brut$resultat, X_SIM$etapes$brut$resultat) && identical(X_EMP$table, X_SIM$table) &&
    identical(fe[ch], fs[ch]) &&
    identical(fe$empreintes[setdiff(names(fe$empreintes), c("donnees", "resultat"))],
              fs$empreintes[setdiff(names(fs$empreintes), c("donnees", "resultat"))]) &&
    !identical(fe$empreintes$donnees, fs$empreintes$donnees)
})

# --- provenance de k fichiers (G9 ; M332) ------------------------------------------

cp <- tablexp:::controler_provenance
PROV1 <- list(fichier = "stocks.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
PROV2 <- list(fichier = c("inventaires.csv", "evenements.csv"),
              empreinte = c("00112233445566778899aabbccddeeff", "ffeeddccbbaa99887766554433221100"),
              algorithme = c("md5", "md5"))
verifier("provenance (valeur écrite à la main) : k = 1, k = 2 et k = 3 admises, noms dans un ordre quelconque ; NULL admis",
         isTRUE(cp(PROV1)) && isTRUE(cp(PROV2)) && isTRUE(cp(PROV2[c("algorithme", "fichier", "empreinte")])) &&
           isTRUE(cp(lapply(PROV2, function(v) c(v, v[1L])))) && isTRUE(cp(NULL)))
# Clés en ASCII (une clé accentuée émet un avertissement sous LC_ALL=C, comme
# dans tests/unitaires/test_construire_table.R).
PROV_K_REFUSEES <- list(
  "longueurs inegales (2, 2, 1)" = modifyList(PROV2, list(algorithme = "md5")),
  "longueurs inegales (1, 2, 2)" = modifyList(PROV2, list(fichier = "a.csv")),
  "vecteurs de longueur 0" = lapply(PROV2, function(v) character(0)),
  "NA dans un vecteur" = modifyList(PROV2, list(empreinte = c("0123", NA))),
  "chaine vide dans un vecteur" = modifyList(PROV2, list(fichier = c("a.csv", ""))),
  "vecteur nomme" = modifyList(PROV2, list(fichier = c(inventaires = "a.csv", evenements = "b.csv"))),
  "vecteur a attribut dim" = modifyList(PROV2, list(algorithme = matrix(c("md5", "md5")))),
  "liste a la place d'un vecteur" = modifyList(PROV2, list(fichier = list("a.csv", "b.csv"))),
  "facteur" = modifyList(PROV2, list(algorithme = factor(c("md5", "md5")))))
for (k in names(PROV_K_REFUSEES))
  verifier(sprintf("provenance (valeur écrite à la main) : %s -> rapport/provenance_invalide", k),
           controle(rf(cp(PROV_K_REFUSEES[[k]])), "rapport", "provenance_invalide"))

X_P1 <- construire_table(EMP_E, RS_E, PLAN_E, PROV1)
X_P2 <- construire_table(EMP_E, RS_E, PLAN_E, PROV2)
verifier("provenance k = 2 (valeur écrite à la main) : fiche$provenance garde ses trois noms et ses vecteurs dans l'ordre reçu ; empreintes$fichier et algorithme_fichier de longueur 2 ; mention du nombre de fichiers", {
  f <- X_P2$fiche
  identical(f$provenance, PROV2) && identical(f$empreintes$fichier, PROV2$empreinte) &&
    identical(f$empreintes$algorithme_fichier, PROV2$algorithme) &&
    identical(f$provenance_mention, "2 fichiers lus par la couche de lecture, un élément par fichier dans fiche/provenance, dans l'ordre de la lecture") &&
    identical(X_P1$fiche$provenance_mention, "fichier lu par la couche de lecture") &&
    identical(X_P1$fiche$empreintes$fichier, PROV1$empreinte)
})
verifier("provenance k = 2 (identité exacte) : objet identical() à celui de k = 1 hors execution, fiche/provenance, fiche/provenance_mention, empreintes de fichier et du résultat", {
  a <- sans_execution(X_P1); b <- sans_execution(X_P2)
  for (o in c("a", "b")) {
    v <- get(o)
    v$fiche[c("provenance", "provenance_mention")] <- NULL
    v$fiche$empreintes[c("fichier", "algorithme_fichier", "resultat")] <- NULL
    assign(o, v)
  }
  identical(a, b) && !identical(X_P1$fiche$empreintes$resultat, X_P2$fiche$empreintes$resultat)
})
verifier("provenance k = 2 (identité exacte) : fiche écrite puis relue identical() à la fiche ; construire_table() sur ses rôles, plan et provenance identical() hors execution", {
  lue <- lire_fiche(ecrire_fiche(X_P2$fiche))
  identical(lue, X_P2$fiche) &&
    identical(sans_execution(construire_table(EMP_E, lue$roles, lue$plan, lue$provenance)), sans_execution(X_P2))
})
verifier("provenance k = 2 (identité exacte) : rejouer_fiche() sur la fiche relue rend la construction identical() hors execution, bilan de 15 lignes sans écart ; ligne 6 et témoin 14 citent chacun des deux fichiers entre guillemets, dans l'ordre", {
  r <- rejouer_fiche(EMP_E, lire_fiche(ecrire_fiche(X_P2$fiche)))
  identical(sans_execution(r$construction), sans_execution(X_P2)) && identical(nrow(r$bilan), 15L) &&
    identical(r$bilan$verdict, c(rep("conforme", 8L), rep("identique", 5L), "non vérifiée", "identique")) &&
    endsWith(r$bilan$lu[6L], "; provenance : « inventaires.csv », « evenements.csv »") &&
    identical(r$bilan$lu[14L], paste0("« ", PROV2$empreinte, " »", collapse = ", "))
})
verifier("provenance k = 1 (valeur écrite à la main) : rejeu, ligne 6 et témoin 14 au texte d'un seul fichier", {
  r <- rejouer_fiche(EMP_E, lire_fiche(ecrire_fiche(X_P1$fiche)))
  endsWith(r$bilan$lu[6L], "; provenance : stocks.csv") && identical(r$bilan$lu[14L], PROV1$empreinte)
})
verifier("provenance k = 2 (valeur écrite à la main) : rapport figé rendu ; les deux fichiers, les deux empreintes et la mention y figurent, chemins de longueur 2", {
  h <- rawToChar(ecrire_rapport(X_P2)); Encoding(h) <- "UTF-8"
  all(vapply(c(PROV2$fichier, PROV2$empreinte, "2 fichiers lus par la couche de lecture"),
             function(s) grepl(s, h, fixed = TRUE), logical(1L))) &&
    grepl("<tr><td>fiche/provenance/fichier</td><td>character</td><td>2</td><td>&quot;inventaires.csv&quot;<br>&quot;evenements.csv&quot;</td></tr>",
          h, fixed = TRUE) &&
    grepl("<tr><td>fiche/empreintes/fichier</td><td>character</td><td>2</td>", h, fixed = TRUE)
})

# --- identité de la fiche à provenance d'un fichier (m4 de l'audit de #181) ---------

# Octets de la fiche écrite d'une construction à provenance k = 1 (étapes
# donnees et exposition seules : aucun calcul dépendant de la bibliothèque
# mathématique), lignes versions/, plateforme/, empreintes/code,
# empreintes/resultat et empreintes/statut_resultat_code retirées (propres au
# poste et au code installé ; l'empreinte du résultat n'est qu'un témoin
# d'identité sur la même machine, E3.6, E4.6 : audit de la reprise de #181,
# n1). MD5 écrit à la main, relevé sur f00d701 (avant #181) et sur l'arbre de
# #181, sous C et C.UTF-8 : égaux (015a20128fdfbd3aea2169926a73518b). Règle
# de mise à jour : au commit qui change la grammaire ou les champs de la
# fiche, avec tableau avant / après.
verifier("provenance k = 1 (valeur écrite à la main) : MD5 de la fiche écrite hors versions, plateforme, empreintes/code, resultat et statut_resultat_code inchangé depuis f00d701", {
  base <- data.frame(contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
                     naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
                     entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
                     sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
                     cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
  roles <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
             date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
  plan <- list(transition = list(depart = "vivant", arrivee = "deces"),
               periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0, segmentation = "sexe",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  prov <- list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
  l <- strsplit(rawToChar(ecrire_fiche(construire_table(base, roles, plan, prov)$fiche)), "\n", fixed = TRUE)[[1L]]
  l <- l[!grepl("^(versions|plateforme|empreintes/(code|resultat|statut_resultat_code))(/|\t)", l)]
  f <- tempfile(); on.exit(unlink(f))
  writeBin(charToRaw(paste0(paste(l, collapse = "\n"), "\n")), f)
  identical(unname(tools::md5sum(f)), "015a20128fdfbd3aea2169926a73518b")
})
