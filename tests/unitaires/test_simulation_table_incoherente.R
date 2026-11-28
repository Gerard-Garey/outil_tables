# -----------------------------------------------------------------------------
#  tests/unitaires/test_simulation_table_incoherente.R : table de loi classée
#  table_reference dont la liste des colonnes est plus courte que ses noms
#  (#365, F344 ; ADR 0015, point 1.3)
#
#  Cas de l'issue #365 : sous une classe dont length() rend 1,
#  `[.data.frame` rend une liste d'un élément munie des trois noms (age,
#  sexe, qx), puis reclassée c("table_reference", "data.frame").
#  loi_effective() lisait tab$sexe sans contrôler cette cohérence :
#  simuler_portefeuille() arrêtait R (segmentation fault, code de sortie 139,
#  mesuré à f2d532c) ; elle refuse désormais, avant toute lecture de colonne
#  (etat_table_reference(), module donnees, appelé par loi_effective() ;
#  #253). D'où un sous-processus Rscript (bibliothèque des batteries,
#  locale de la batterie), dont le résultat revient par saveRDS() : sur l'état
#  antérieur, le processus de test lui-même s'arrêterait. Contre-épreuve : le
#  sous-processus n'écrit aucun résultat (code de sortie 139) sur f2d532c.
#  Message attendu écrit à la main, en caractères UTF-8 bruts dans le fichier.
# -----------------------------------------------------------------------------

local({
  script <- tempfile("t365_", fileext = ".R")
  on.exit(unlink(script))
  writeLines(c(
    "a <- commandArgs(TRUE)",
    sprintf("suppressPackageStartupMessages(library(tablexp, lib.loc = %s))", deparse(.harnais$bibliotheque)),
    "length.k355 <- function(x) 1L",
    "TRI <- data.frame(age = 0:120, sexe = 'ensemble', qx = 0.01)",
    "x <- structure(TRI, class = c('k355', 'data.frame'))[-2L, , drop = FALSE]; class(x) <- c('table_reference', 'data.frame')",
    "F1 <- c(H = 1, F = 1)",
    "RACHAT <- tablexp:::table_constante(0.05, 0:120, 'rachat constant 5 %')",
    "PF <- list(mode = 'uniforme', n = 100, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = as.Date('2005-01-01'))",
    "PER <- list(debut = as.Date('2015-01-01'), fin = as.Date('2024-12-31'))",
    "loi <- switch(a[2L], deces = list(deces = list(table = x, facteurs = F1), rachat = list(table = RACHAT, facteurs = F1)),",
    "              rachat = list(deces = list(table = RACHAT, facteurs = F1), rachat = list(table = x, facteurs = F1)))",
    "appel <- function() tryCatch({ simuler_portefeuille(loi, PF, PER, 1L); NULL }, refus_tablexp = function(e) e, error = function(e) e)",
    "r <- suppressWarnings(appel())",
    "n_avert <- 0L",
    "withCallingHandlers(appel(), warning = function(w) { n_avert <<- n_avert + 1L; invokeRestart('muffleWarning') })",
    "saveRDS(list(classe = class(r), module = r$module, motif = r$motif, message = if (!is.null(r)) conditionMessage(r),",
    "             details = r$details, avertissements = n_avert,",
    "             lues = list(longueur = length(unclass(x)), noms = names(x))), a[1L])"),
    script)
  executer <- function(k) {
    sortie <- tempfile("t365_", fileext = ".rds")
    on.exit(unlink(sortie))
    p <- executer_rscript(script, c(shQuote(sortie), k))
    if (!file.exists(sortie)) return(sprintf("aucun résultat : code de sortie %d (%s)", p$statut,
                                             paste(utils::tail(p$sortie, 3L), collapse = " | ")))
    c(readRDS(sortie), list(statut = p$statut))
  }
  MSG <- function(cause) paste0(
    "Loi « ", cause, " » : la table n'est pas l'objet rendu par table_reference() (objet modifié après table_reference()). ",
    "Passer l'objet rendu par table_reference(), sans modification (ADR 0015, point 1.3).")
  for (cause in c("deces", "rachat"))
    verifier(sprintf("#365 simuler_portefeuille() : table de la transition %s de 1 colonne pour 3 noms -> refus loi_invalide du module simulation, message et details exacts, une ligne UTF-8, 0 avertissement R, code de sortie 0, sous-processus", cause), {
      x <- executer(cause)
      if (is.character(x)) x
      else if (x$statut != 0L) sprintf("code de sortie %d", x$statut)
      else if (!identical(x$lues, list(longueur = 1L, noms = c("age", "sexe", "qx")))) "objet de test : liste de 1 colonne pour 3 noms attendue"
      else if (!"refus_tablexp" %in% x$classe) sprintf("condition %s : %s", paste(x$classe, collapse = "/"), paste(x$message, collapse = ""))
      else if (!identical(x$module, "simulation") || !identical(x$motif, "loi_invalide")) sprintf("%s / %s", x$module, x$motif)
      else if (!identical(x$message, MSG(cause))) paste("message :", x$message)
      else if (grepl("[\n\v\f]", x$message) || !validUTF8(x$message) || !identical(Encoding(x$message), "UTF-8"))
        "message sur plusieurs lignes ou non marqué UTF-8"
      else if (!identical(x$details, list(cause = cause, constat = "objet modifié après table_reference()",
                                          lu = "data.frame nu non reconstructible", attendu = "colonnes de table_reference()",
                                          motif_constructeur = NA_character_)))
        paste("details :", paste(deparse(x$details), collapse = " "))
      else if (x$avertissements != 0L) sprintf("%d avertissement(s) R", x$avertissements)
      else TRUE
    })
})
