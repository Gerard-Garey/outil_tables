# -----------------------------------------------------------------------------
#  tests/unitaires/test_attendus_hors_plage.R : refus attendus_hors_plage
#  (issue #360 ; spécification d'actuary-approfondi, commentaire 7137107852,
#  décision du mainteneur P1 (a) du 24/11/2026) et gardes d'erreur de
#  programmation G1 (smr_comparateur_1()) et G2 (retrait_contrefactuel())
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur écrite à la main : constante PLAGE_ATTENDUS, module, motif,
#    message et details des refus ; essais T1, T2, T3, T5, T10, T11 et T8 de
#    la spécification (recette agregats, 4 cellules, d = (3, 5, 4, 6), sauf
#    T8) ; chacun aboutissait (sorties infinies, NaN ou table positionnée
#    nulle) ou levait une erreur de R avant #360 (mesuré sur 9d5d22a) ;
#  - constat d'exécution : bornes 0,5e100 et 2e-100 (sorties finies, ou NA
#    à état nommé, aucun avertissement de R), juste hors plage refusé ;
#    gardes G1 et G2 par appel direct.
#  Constantes non ASCII des messages attendus écrites en \uXXXX. Graine du
#  plan 360 (inférieure à 1000, exemptée, tests/controles/graines.R).
# -----------------------------------------------------------------------------

D <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
erreur_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e)
# Refus attendu : module, motif ; TRUE ou le texte de l'écart.
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!inherits(r, "refus_tablexp")) return(paste("erreur de R :", conditionMessage(r)))
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}

# --- jeu agrégats écrit à la main et plans -----------------------------------------

plan_hp <- function(etapes) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"),
            periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0,
            segmentation = character(0),
            etapes = c(list(donnees = list(exclusions = character(0)),
                            exposition = list(convention = "dernier_anniversaire", par_annee = FALSE)), etapes))
  if (!is.null(etapes$validation)) p$graine <- 360L
  p
}
table_q <- function(q) table_reference(data.frame(age = 0:120, sexe = "ensemble", qx = q), "q", "importee", "test")
POS <- list(positionnement = list(methode = "smr", table_reference = "t", plage_ages = c(50, 53), niveau = 0.95))
VAL <- function(relation) list(validation = list(table_reference = "t", relation_table = relation, plage_ages = c(50, 53),
                                                 tranches_age = c(50, 52), niveau = 0.95, replications = 99))
R_AG <- c(age = "age", exposition_centrale = "ec", evenements = "d")
D4 <- c(3L, 5L, 4L, 6L)
# Construction sur 4 cellules (âges 50 à 53) ; rend le résultat, le refus ou
# l'erreur, et les avertissements de R relevés. L'avertissement de
# traduction « strings not representable in native encoding », levé hors
# UTF-8 par la lecture de textes du moteur (sous la locale C, dès le témoin
# T0, avant comme après #360 ; mesuré), n'est pas un avertissement de calcul :
# il est écarté du relevé.
construire_hp <- function(ec, q, etapes, d = D4) {
  ag <- data.frame(age = 50:(49 + length(d)), ec = ec, d = d)
  w <- character(0)
  r <- withCallingHandlers(
    erreur_de(construire_table(ag, R_AG, plan_hp(etapes), tables_reference = list(t = table_q(q)))),
    warning = function(x) {
      if (!grepl("not representable in native encoding", conditionMessage(x), fixed = TRUE))
        w <<- c(w, conditionMessage(x))
      invokeRestart("muffleWarning")
    })
  list(r = r, avertissements = w)
}
MU01 <- -log1p(-0.01)
Q01 <- rep(0.01, 121L); Q99 <- rep(0.99, 121L); Q10 <- rep(1e-10, 121L); Q320 <- rep(1e-320, 121L)
EC10 <- c(1e-300, 1e150, 1e-300, 1e150)
message_attendu <- function(etape, n, cle)
  paste0(etape, " : ", n, " cellule(s) expos\u00e9e(s) dont l'attendu A = E^c (-ln(1 - q_x)) (table de cl\u00e9 \u00ab ",
         cle, " \u00bb) sort de la plage de calcul du moteur [1e-100, 1e+100] ; v\u00e9rifier la colonne de ",
         "l'exposition centrale et la table ; la construction est refus\u00e9e.")
DETAILS_4 <- list(cle = "t", cellules = 4L, limite_inferieure = 1e-100, limite_superieure = 1e100)

# --- A. Constante (valeur écrite à la main) ------------------------------------------

verifier("A PLAGE_ATTENDUS = c(1e-100, 1e100) (spécification de #360, P1 (a))",
         identical(tablexp:::PLAGE_ATTENDUS, c(1e-100, 1e100)))

# --- B. Essais de la spécification (valeur écrite à la main) ---------------------------

verifier("T0 (témoin) : E^c = 100, q~ = 0,01, validation independante : aboutit, aucun avertissement", {
  x <- construire_hp(100, Q01, VAL("independante"))
  if (inherits(x$r, "error") || length(x$avertissements)) "refus, erreur ou avertissement" else TRUE
})
verifier("T1 : E^c = 1e160, q~ = 0,01, validation independante -> refus validation / attendus_hors_plage, message et details exacts (avant #360 : X2 = Inf produite)", {
  r <- construire_hp(1e160, Q01, VAL("independante"))$r
  tous(refus = controle(r, "validation", "attendus_hors_plage"),
       message = if (!is.null(r) && identical(conditionMessage(r), message_attendu("Validation", 4L, "t"))) TRUE
                 else if (is.null(r)) "aucun refus" else conditionMessage(r),
       details = if (identical(r$details, DETAILS_4)) TRUE else "details")
})
verifier("T2 : E^c = 2e307, q~ = 0,99, validation independante -> refus validation / attendus_hors_plage, aucun avertissement (avant #360 : attendus Inf, NaN, « NaNs produced »)", {
  x <- construire_hp(2e307, Q99, VAL("independante"))
  tous(refus = controle(x$r, "validation", "attendus_hors_plage"),
       avertissements = if (length(x$avertissements)) paste(x$avertissements, collapse = " ; ") else TRUE)
})
verifier("T3 : E^c = 2e307, q~ = 0,99, positionnement seul -> refus positionnement / attendus_hors_plage, message et details exacts (avant #360 : table positionnée qx = 0 produite)", {
  r <- construire_hp(2e307, Q99, POS)$r
  tous(refus = controle(r, "positionnement", "attendus_hors_plage"),
       message = if (!is.null(r) && identical(conditionMessage(r), message_attendu("Positionnement", 4L, "t"))) TRUE
                 else if (is.null(r)) "aucun refus" else conditionMessage(r),
       details = if (identical(r$details, DETAILS_4)) TRUE else "details")
})
verifier("T5 : E^c = 2e307, q~ = 0,99, positionnement et validation positionnee -> refus positionnement / attendus_hors_plage (avant #360 : erreur de R dans replications_multinomiales())",
         controle(construire_hp(2e307, Q99, c(POS, VAL("positionnee")))$r, "positionnement", "attendus_hors_plage"))
verifier("T10 : E^c = (1e-300, 1e150, 1e-300, 1e150), q~ = 1e-10, positionnee -> refus positionnement / attendus_hors_plage (avant #360 : erreur de R, sous-dépassement de Ar)",
         controle(construire_hp(EC10, Q10, c(POS, VAL("positionnee")))$r, "positionnement", "attendus_hors_plage"))
verifier("T11 : même jeu que T10, validation independante -> refus validation / attendus_hors_plage (avant #360 : X2 = Inf)",
         controle(construire_hp(EC10, Q10, VAL("independante"))$r, "validation", "attendus_hors_plage"))
verifier("T8 : E^c = 1, q~ = 1e-320, d = 1, validation independante -> refus validation / attendus_hors_plage, 1 cellule (avant #360 : X2 = Inf)", {
  r <- construire_hp(1, Q320, VAL("independante"), d = 1L)$r
  tous(refus = controle(r, "validation", "attendus_hors_plage"),
       cellules = if (identical(r$details$cellules, 1L)) TRUE else "cellules")
})

# --- C. Bornes (constat d'exécution) ---------------------------------------------------

# Sorties finies, ou NA à état nommé (différent de produite), aucun
# avertissement de R ; table positionnée finie dans [0, 1].
sorties_saines <- function(x) {
  if (inherits(x$r, "error")) return(paste("refus ou erreur :", conditionMessage(x$r)))
  if (length(x$avertissements)) return(paste("avertissement :", paste(x$avertissements, collapse = " ; ")))
  v <- x$r$etapes$validation
  if (!is.null(v)) {
    st <- v$resultat$statistiques
    if (any(!is.na(st$valeur) & !is.finite(st$valeur))) return("statistique non finie")
    if (any(is.na(st$valeur) & st$etat == "produite")) return("statistique NA à l'état produite")
    sg <- v$resultat$sous_groupes
    n <- vapply(sg, is.double, NA)
    if (any(vapply(sg[n], function(c) any(is.infinite(c) | is.nan(c)), NA))) return("sous-groupe non fini")
  }
  p <- x$r$etapes$positionnement
  if (!is.null(p)) {
    q <- p$resultat$table_positionnee$qx
    if (!all(is.finite(q) & q >= 0 & q <= 1)) return("table positionnée non finie")
    s <- unlist(p$resultat$smr[c("smr", "attendus", "borne_inf_exacte", "borne_sup_exacte")])
    if (!all(is.finite(s))) return("SMR non fini")
  }
  TRUE
}
verifier("bornes : A = 0,5e100 et A = 2e-100 par cellule (q~ = 0,01), validation independante, positionnement et validation positionnee : sorties finies ou NA à état nommé, aucun avertissement de R", {
  ec_haut <- 0.5e100 / MU01; ec_bas <- 2e-100 / MU01
  tous(pre = if (all(ec_haut * MU01 <= 1e100, ec_bas * MU01 >= 1e-100)) TRUE else "attendus hors plage",
       haut_ind = sorties_saines(construire_hp(ec_haut, Q01, VAL("independante"))),
       haut_pos = sorties_saines(construire_hp(ec_haut, Q01, c(POS, VAL("positionnee")))),
       bas_ind = sorties_saines(construire_hp(ec_bas, Q01, VAL("independante"))),
       bas_pos = sorties_saines(construire_hp(ec_bas, Q01, c(POS, VAL("positionnee")))))
})
verifier("bornes : juste hors de la plage (A = 1,000001e100 et 0,999999e-100 par cellule) -> refus attendus_hors_plage, sous validation et sous positionnement", {
  ec_haut <- 1.000001e100 / MU01; ec_bas <- 0.999999e-100 / MU01
  tous(pre = if (all(ec_haut * MU01 > 1e100, ec_bas * MU01 < 1e-100)) TRUE else "attendus dans la plage",
       haut_val = controle(construire_hp(ec_haut, Q01, VAL("independante"))$r, "validation", "attendus_hors_plage"),
       haut_pos = controle(construire_hp(ec_haut, Q01, POS)$r, "positionnement", "attendus_hors_plage"),
       bas_val = controle(construire_hp(ec_bas, Q01, VAL("independante"))$r, "validation", "attendus_hors_plage"),
       bas_pos = controle(construire_hp(ec_bas, Q01, POS)$r, "positionnement", "attendus_hors_plage"))
})

# --- D. attendus_cellules() en appel direct (valeur écrite à la main) -------------------

verifier("attendus_cellules() : q~ = 0 avec E^c = 1e300 -> taux_table_nul et evenement_sous_taux_nul, sans refus ; q~ > 0 à produit nul -> refus (taux_table_nul restreint à q~ = 0)", {
  ac <- tablexp:::attendus_cellules
  a <- ac(c(1e300, 1e300), c(0L, 2L), c(0, 0), "t", "validation")
  tous(nul = if (identical(a$etat, c("taux_table_nul", "evenement_sous_taux_nul")) && identical(a$attendus, c(0, 0)))
               TRUE else paste(a$etat, collapse = ", "),
       produit_nul = controle(refus_de(ac(1e-300, 0L, 1e-300, "t", "positionnement")), "positionnement",
                              "attendus_hors_plage"))
})
verifier("attendus_cellules() : clé NULL (relation lissee, table construite) -> message qui nomme la table construite, details de clé NULL", {
  r <- refus_de(tablexp:::attendus_cellules(1e160, 1L, 0.01, NULL, "validation"))
  m <- paste0("Validation : 1 cellule(s) expos\u00e9e(s) dont l'attendu A = E^c (-ln(1 - q_x)) (table construite) sort ",
              "de la plage de calcul du moteur [1e-100, 1e+100] ; v\u00e9rifier la colonne de l'exposition centrale et ",
              "la table ; la construction est refus\u00e9e.")
  tous(refus = controle(r, "validation", "attendus_hors_plage"),
       message = if (!is.null(r) && identical(conditionMessage(r), m)) TRUE
                 else if (is.null(r)) "aucun refus" else conditionMessage(r),
       details = if (identical(r$details, list(cle = NULL, cellules = 1L, limite_inferieure = 1e-100,
                                               limite_superieure = 1e100))) TRUE else "details")
})

# --- E. Comparateur 2 (constat d'exécution) ----------------------------------------------
# Jeu T1 de test_importance_exclusions.R plus Z (sexe manquant, exclue, née
# le 1949-01-01, décès le 2015-02-01) : seule cellule exposée à 66 ans, des
# lignes exclues seulement ; table à q~_66 = 1e-320 (0,01 ailleurs).
# L'étape validation aboutit (E^c = 0 à 66 ans parmi les conservées) ;
# avant #360, le comparateur 2 sommait un attendu de 8,5e-322 (mesuré).
T1Z <- data.frame(
  id_contrat = paste0("C", 1:9), id_tete = c("C", "D", "E", "G", "A", "B", "C", "H", "Z"),
  sexe = c("H", "F", "H", "F", NA, NA, NA, "H", NA), date_naissance = D(c(rep("1950-01-01", 8L), "1949-01-01")),
  date_entree = D(c("2015-01-01", "2015-01-01", "2015-01-01", "2014-01-01", "2015-01-01", "2015-01-01",
                    "2015-05-06", NA, "2015-01-01")),
  date_sortie = D(c("2015-10-01", NA, "2015-03-30", "2014-12-31", "2015-04-01", "2015-04-03", "2015-10-01",
                    "2015-06-01", "2015-02-01")),
  cause_sortie = c("deces", "en_cours", "deces", "rachat", "deces", "deces", "deces", "deces", "deces"),
  stringsAsFactors = FALSE)
ROLES_Z <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
             date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
plan_z <- function(importance) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"),
            periode = list(debut = D("2015-01-01"), fin = D("2015-12-31")), delai_declaration = 0,
            segmentation = character(0), graine = 360L,
            etapes = list(donnees = list(exclusions = c("entree_manquante", "sexe_manquant")),
                          exposition = list(convention = "plus_fine", par_annee = FALSE),
                          validation = list(table_reference = "tq", relation_table = "independante",
                                            plage_ages = c(60, 70), tranches_age = 60, niveau = 0.95,
                                            replications = 9)))
  if (importance) p$etapes$importance_exclusions <- list(niveau = 0.95)
  p
}
verifier("comparateur 2 : cellule à q~ = 1e-320 exposée des seules lignes exclues -> refus validation / attendus_hors_plage depuis importance_exclusions ; sans importance_exclusions, la construction aboutit", {
  t66 <- table_reference(data.frame(age = 0:120, sexe = "ensemble", qx = ifelse(0:120 == 66, 1e-320, 0.01)), "q66",
                         "importee", "test")
  sans <- erreur_de(construire_table(T1Z, ROLES_Z, plan_z(FALSE), tables_reference = list(tq = t66)))
  r <- refus_de(construire_table(T1Z, ROLES_Z, plan_z(TRUE), tables_reference = list(tq = t66)))
  tous(validation_seule = if (is.null(sans) || !inherits(sans, "error")) TRUE else conditionMessage(sans),
       refus = controle(r, "validation", "attendus_hors_plage"),
       details = if (identical(r$details$cle, "tq") && identical(r$details$cellules, 1L)) TRUE else "details")
})

# --- F. Gardes G1 et G2 (constat d'exécution, appel direct) -------------------------------

verifier("G1 smr_comparateur_1() : attendus infinis, ou rapport de variance infini à attendus finis -> erreur de programmation (pas un refus) ; cas fini inchangé", {
  c1 <- function(ec_ret, ec_cons) tablexp:::smr_comparateur_1(
    data.frame(age = 60L, evenements_retires = 1L, exposition_jours_retiree = 365, exposition_centrale_retiree = ec_ret),
    data.frame(age = 60L, evenements = 1L, exposition_jours = 365, exposition_centrale = ec_cons),
    character(0), FALSE, 0.95)
  g1 <- function(e) if (!is.null(e) && !inherits(e, "refus_tablexp") &&
                        identical(conditionMessage(e), paste("smr_comparateur_1() : attendus ou rapport de variance",
                                                             "non finis (erreur de programmation).")))
                      TRUE else if (is.null(e)) "aucune erreur" else conditionMessage(e)
  ok <- c1(2, 1)$ligne
  tous(A_infini = g1(erreur_de(c1(1e300, 1e-300))),
       rv_infini = g1(erreur_de(c1(1e300, 1))),
       fini = if (identical(ok$attendus, 2) && identical(ok$rapport_variance, 2)) TRUE else "cas fini")
})
verifier("G2 retrait_contrefactuel() : exposition centrale retirée de somme au-delà de LIMITE_SOMMES_DOUBLE ou NaN -> erreur de programmation (pas un refus) ; cas fini inchangé", {
  # Appel direct du corps de la fonction, exposition_lignes() remplacée par
  # une table d'exposition écrite à la main (unité contrat, ligne 1 seule,
  # mesurable).
  avec_expo <- function(ec) {
    f <- tablexp:::retrait_contrefactuel
    env <- new.env(parent = environment(f))
    env$exposition_lignes <- function(lignes, cles, ctx)
      data.frame(age = 60:61, evenements = c(1L, 0L), exposition_jours = c(365, 365), exposition_centrale = ec)
    environment(f) <- env
    f(1L, character(0), list(lignes_exclues = list(sexe_manquant = 1L), unite = "contrat", par_annee = FALSE))
  }
  # Message comparé sur ses parties ASCII : hors UTF-8, stop() rend le
  # caractère accentué du moteur en <U+00E9> (mesuré sous la locale C).
  g2 <- function(e) if (!is.null(e) && !inherits(e, "refus_tablexp") &&
                        startsWith(conditionMessage(e), "retrait_contrefactuel() : exposition centrale retir") &&
                        endsWith(conditionMessage(e), "hors de la plage de calcul (erreur de programmation)."))
                      TRUE else if (is.null(e)) "aucune erreur" else conditionMessage(e)
  ok <- avec_expo(c(1, 1))
  tous(somme_infinie = g2(erreur_de(avec_expo(c(.Machine$double.xmax, .Machine$double.xmax)))),
       nan = g2(erreur_de(avec_expo(c(NaN, 1)))),
       fini = if (identical(ok$cellules$exposition_centrale_retiree, c(1, 1))) TRUE else "cas fini")
})
