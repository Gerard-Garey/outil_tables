# -----------------------------------------------------------------------------
#  tests/unitaires/test_positionnement.R : étape `positionnement`, méthode 1
#  (SMR) (issue #167 ; spécification d'actuary-approfondi décidée, M288,
#  M289, M293, commentaire 6705863583 ; avis de forme (0''), F2, F4, F7, F9,
#  M292, commentaire 6706003047)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : petit jeu de cellules (theta = 16/13),
#    constantes, catalogue, motifs et ordre des refus, états ;
#  - recoupement indépendant par `stats` (priorité base, en CI ; part SMR de
#    #5) : glm() de Poisson à offset (theta par segment, bornes
#    asymptotiques par confint.default()), poisson.test() (bornes exactes) ;
#  - identité exacte : refus de `validation` inchangés (F2), résultats de
#    `validation` inchangés par l'ajout de l'étape, rejeu identical() hors
#    execution, table positionnée égale sous par_annee.
# -----------------------------------------------------------------------------

D <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
# Refus attendu : module, motif ; TRUE ou le texte de l'écart.
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
# Plusieurs comparaisons : TRUE ou la première en écart.
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}

# --- A. Constantes et catalogue (valeur calculée à la main) -----------------------

verifier("A ETAPES : positionnement après brut, avant homogeneite (#168) et validation, module positionnement, calculer_positionnement, préalables donnees et exposition, non aléatoire", {
  e <- tablexp:::ETAPES
  k <- match("positionnement", e$etape)
  identical(k, 4L) && identical(e$etape[k + 1:2], c("homogeneite", "validation")) &&
    identical(e$module[k], "positionnement") &&
    identical(e$fonction[k], "calculer_positionnement") && identical(e$prealable[[k]], c("donnees", "exposition")) &&
    identical(e$aleatoire[k], FALSE)
})
verifier("A PARAMETRES_ETAPES$positionnement : methode, table_reference, plage_ages, niveau (tous requis, sans défaut, M293)", {
  p <- tablexp:::PARAMETRES_ETAPES$positionnement
  identical(p$parametre, c("methode", "table_reference", "plage_ages", "niveau")) &&
    identical(p$type, c("character", "character", "double", "double"))
})
verifier("A constantes : METHODES_POSITIONNEMENT « smr », PARAMETRES_ESTIMES_POSITIONNEMENT 1L, états, PARAMETRES_CLE_TABLE cite la clé du positionnement", {
  identical(tablexp:::METHODES_POSITIONNEMENT, "smr") && identical(tablexp:::PARAMETRES_ESTIMES_POSITIONNEMENT, 1L) &&
    identical(tablexp:::ETATS_TABLE_POSITIONNEE, c("produite", "taux_table_nul", "taux_table_egal_un", "qx_arrondi_un")) &&
    identical(tablexp:::ETATS_CELLULE_POSITIONNEMENT, tablexp:::ETATS_CELLULE_VALIDATION) &&
    identical(tablexp:::ETAPES_TABLE, c("positionnement", "validation")) &&
    "etapes/positionnement/table_reference" %in% tablexp:::PARAMETRES_CLE_TABLE
})
verifier("A REFERENCE_METHODE_POSITIONNEMENT : note II1291-12 v1.7, SHA-256 de la spécification, référence de place", {
  r <- tablexp:::REFERENCE_METHODE_POSITIONNEMENT
  grepl("II1291-12 v1.7", r, fixed = TRUE) &&
    grepl("93537ddb13bf3c989f186b3d7676976e8759d5b6ca106910c8ab66cd0e4e6dce", r, fixed = TRUE) &&
    grepl("référence de place", r, fixed = TRUE) && identical(Encoding(r), "UTF-8")
})
verifier("A STATUTS_POSITIONNEMENT : (table, grandeur) uniques ; mention d'indépendance sur chaque borne (M278) ; aucune attribution d'auteur (M200)", {
  s <- tablexp:::STATUTS_POSITIONNEMENT
  b <- grepl("^borne_", s$grandeur)
  !anyDuplicated(paste(s$table, s$grandeur)) && all(nzchar(s$statut)) && sum(b) == 8L &&
    all(grepl("unités d'observation indépendantes ; sous l'unité contrat, les décès sont sur-dispersés", s$statut[b],
              fixed = TRUE)) &&
    !any(grepl("Liddell|Garwood|Byar|Wilson|Hilferty", s$statut))
})

# --- B. Petit jeu de cellules (valeur calculée à la main) --------------------------
# Une seule séquence sans segmentation, âges 50 à 52 : E^c = (128, 256, 512),
# mu~ = (1/256, 1/128, 1/128), q~ = 1 - exp(-mu~) ; A = (0,5 ; 2 ; 4),
# A_s = 6,5 ; D = (0, 3, 5), O = 8 ; theta = 16/13 ; table positionnée
# q = 1 - exp(-theta mu~) ; bornes exactes G_8^-1(0,025) / 6,5 et
# G_9^-1(0,975) / 6,5 ; asymptotiques theta exp(-+ 1,959963984540054 / sqrt(8)).
MU <- c(1 / 256, 1 / 128, 1 / 128)
EXPO_B <- data.frame(age = 50:52, exposition_jours = 0, exposition_centrale = c(128, 256, 512), evenements = c(0L, 3L, 5L),
                     sorties_concurrentes = 0L)
T_B <- table_reference(data.frame(age = 40:70, sexe = "ensemble", qx = replace(rep(0.01, 31L), 11:13, -expm1(-MU))),
                       "B", "importee", "test")
positionnement_direct <- function(expo = EXPO_B, table = T_B, plage = c(50, 52), segmentation = character(0), ...) {
  a <- list(exposition = expo, segmentation = segmentation, par_annee = FALSE, convention = "dernier_anniversaire",
            table = table, table_reference = "t", methode = "smr", plage_ages = plage, niveau = 0.95)
  m <- list(...)
  a[names(m)] <- m
  do.call(tablexp:::calculer_positionnement, a)
}
RB <- positionnement_direct()
verifier("B : O = 8, A = 6,5, theta = 16/13, 3 cellules retenues, état produite", {
  s <- RB$resultat$smr
  tous(o = proche(s$observes, 8, 0, 0), a = proche(s$attendus, 6.5, 1e-14, 0), t = proche(s$smr, 16 / 13, 1e-14, 0),
       n = if (identical(s$cellules_retenues, 3L) && identical(s$etat, "produite")) TRUE else "cellules ou état")
})
verifier("B : bornes exactes qgamma(0,025 ; 8) / 6,5 et qgamma(0,975 ; 9) / 6,5 ; asymptotiques theta exp(-+ z / sqrt(8))", {
  s <- RB$resultat$smr
  z <- 1.959963984540054
  tous(ie = proche(s$borne_inf_exacte, stats::qgamma(0.025, 8) / 6.5, 1e-14, 0),
       se = proche(s$borne_sup_exacte, stats::qgamma(0.975, 9) / 6.5, 1e-14, 0),
       ia = proche(s$borne_inf_asymptotique, 16 / 13 * exp(-z / sqrt(8)), 1e-14, 0),
       sa = proche(s$borne_sup_asymptotique, 16 / 13 * exp(z / sqrt(8)), 1e-14, 0))
})
verifier("B : table positionnée q = 1 - exp(-theta mu~) aux âges 50 à 52, bornes images des bornes de theta, q~ recopié", {
  tp <- RB$resultat$table_positionnee
  s <- RB$resultat$smr
  identical(names(tp), c("age", "qx_reference", "qx", "borne_inf_exacte", "borne_sup_exacte", "borne_inf_asymptotique",
                         "borne_sup_asymptotique", "etat")) &&
    isTRUE(tous(age = if (identical(tp$age, 50:52)) TRUE else "âges",
                ref = proche(tp$qx_reference, -expm1(-MU), 0, 0),
                q = proche(tp$qx, -expm1(-16 / 13 * MU), 1e-14, 0),
                ie = proche(tp$borne_inf_exacte, -expm1(-s$borne_inf_exacte * MU), 1e-14, 0),
                sa = proche(tp$borne_sup_asymptotique, -expm1(-s$borne_sup_asymptotique * MU), 1e-14, 0),
                ordre = if (all(tp$borne_inf_exacte < tp$qx & tp$qx < tp$borne_sup_exacte)) TRUE else "ordre",
                etat = if (all(tp$etat == "produite")) TRUE else "état"))
})
verifier("B : sur les cellules retenues, somme de E^c (theta mu~) = O (le facteur reproduit les décès observés)",
         proche(sum(EXPO_B$exposition_centrale * 16 / 13 * MU), RB$resultat$smr$observes, 1e-14, 0))
verifier("B : paramètres de l'étape (méthode, clé, plage, niveau, convention, appariement, p = 1, statuts, référence)", {
  p <- RB$parametres
  identical(names(p), c("methode", "table_reference", "table_nom", "plage_ages", "niveau", "convention",
                        "appariement_sexe", "parametres_estimes", "statuts", "reference_methode")) &&
    identical(p$methode, "smr") && identical(p$table_nom, "B") && identical(p$parametres_estimes, 1L) &&
    identical(p$appariement_sexe, "sans_sexe") && identical(p$statuts, tablexp:::STATUTS_POSITIONNEMENT) &&
    identical(names(RB$resultat), c("cellules", "smr", "table_positionnee"))
})
verifier("B : statuts : une ligne par colonne double de resultat$smr et de resultat$table_positionnee, dans l'ordre", {
  s <- tablexp:::STATUTS_POSITIONNEMENT
  doubles <- function(t) names(t)[vapply(t, is.double, NA)]
  identical(s$grandeur[s$table == "smr"], doubles(RB$resultat$smr)) &&
    identical(s$grandeur[s$table == "table_positionnee"], doubles(RB$resultat$table_positionnee))
})

# États et cas aux limites (spécification, § 4 ; calculer_positionnement() en
# appel direct) : âge 50 sans exposition (q~ = 1) ; 51 décès sans exposition ;
# 52 q~ = 0 sans décès ; 53 q~ = 0 avec 2 décès ; 54 à 56 retenues ; 57 absent
# de l'exposition ; 60 hors plage.
EXPO_E <- data.frame(age = c(50L, 51L, 52L, 53L, 54L, 55L, 56L, 60L), exposition_jours = 0,
                     exposition_centrale = c(0, 0, 10, 10, 100, 120, 80, 50), evenements = c(0L, 1L, 0L, 2L, 1L, 3L, 0L, 4L),
                     sorties_concurrentes = 0L)
T_E <- table_reference(data.frame(age = 40:70, sexe = "ensemble",
                                  qx = ifelse(40:70 == 50, 1, ifelse(40:70 %in% 52:53, 0, 0.01))), "etats", "importee", "test")
RE <- positionnement_direct(expo = EXPO_E, table = T_E, plage = c(50, 57))
verifier("états : q~ = 1 sans exposition -> q = 1, état taux_table_egal_un ; q~ = 0 -> q = 0, état taux_table_nul ; table sur toute la plage (8 âges)", {
  tp <- RE$resultat$table_positionnee
  identical(tp$age, 50:57) &&
    identical(tp$etat, c("taux_table_egal_un", "produite", "taux_table_nul", "taux_table_nul", rep("produite", 4L))) &&
    identical(tp$qx[1L], 1) && identical(tp$borne_inf_exacte[1L], 1) && identical(tp$qx[3:4], c(0, 0)) &&
    all(tp$qx[-c(1, 3, 4)] > 0 & tp$qx[-c(1, 3, 4)] < 1)
})
verifier("états : cellules (sans_exposition, evenement_sans_exposition, taux_table_nul, evenement_sous_taux_nul, retenue) ; O = 4 sur les retenues ; avertissement evenements_sans_attendu ; hors plage", {
  c_ <- RE$resultat$cellules
  d <- RE$diagnostics
  identical(c_$etat, c("sans_exposition", "evenement_sans_exposition", "taux_table_nul", "evenement_sous_taux_nul",
                       "retenue", "retenue", "retenue", "sans_exposition")) &&
    identical(RE$resultat$smr$observes, 4) && identical(d$evenements_sans_attendu, 3) &&
    identical(d$evenements_hors_plage, 4) && identical(d$cellules_hors_plage, 1L) &&
    identical(d$etats$cellules, c(3L, 2L, 1L, 1L, 1L)) &&
    identical(RE$avertissements$etape, "positionnement") && identical(RE$avertissements$code, "evenements_sans_attendu")
})

# Arrondi à 1 en double précision (avis d'actuary du 07/11/2026) :
# -expm1(theta log1p(-q~)) vaut 1 dès que theta mu~ >= 54 ln 2 (environ
# 37,43) ; état qx_arrondi_un, avertissement, ni refus ni plafonnement.
# T1 : q~ = (0,001 ; 0,001 ; 0,99), E^c = (1 ; 1 ; 0,001), D = (1 ; 0 ; 0) :
# theta^ = 1 / A voisin de 151, theta^ mu~(52) voisin de 696.
EXPO_T1 <- data.frame(age = 50:52, exposition_jours = 0, exposition_centrale = c(1, 1, 0.001), evenements = c(1L, 0L, 0L),
                      sorties_concurrentes = 0L)
T_T1 <- table_reference(data.frame(age = 40:70, sexe = "ensemble", qx = replace(rep(0.01, 31L), 11:13, c(0.001, 0.001, 0.99))),
                        "T1", "importee", "test")
RT1 <- positionnement_direct(expo = EXPO_T1, table = T_T1)
COLONNES_Q <- c("qx", "borne_inf_exacte", "borne_sup_exacte", "borne_inf_asymptotique", "borne_sup_asymptotique")
verifier("T1 : âge 52 -> état qx_arrondi_un, qx identical() à 1 ; âges 50 et 51 -> produite, qx < 1, bornes strictement ordonnées", {
  tp <- RT1$resultat$table_positionnee
  p <- tp[1:2, ]
  identical(tp$etat, c("produite", "produite", "qx_arrondi_un")) && identical(tp$qx[3L], 1) &&
    all(p$qx < 1 & p$borne_inf_exacte < p$qx & p$qx < p$borne_sup_exacte &
          p$borne_inf_asymptotique < p$qx & p$qx < p$borne_sup_asymptotique)
})
verifier("T1 : avertissement qx_arrondi_un (module positionnement, 1 ligne) qui cite l'âge 52 et 54 ln 2", {
  a <- RT1$avertissements
  k <- a$code == "qx_arrondi_un"
  sum(k) == 1L && identical(a$etape[k], "positionnement") && identical(a$nombre[k], 1L) &&
    grepl("âge 52, theta^ = ", a$message[k], fixed = TRUE) && grepl("54 ln 2", a$message[k], fixed = TRUE) &&
    !grepl("âge 50", a$message[k], fixed = TRUE)
})
# T2 : invariant des états sur une table positionnée.
invariant_etats <- function(tp) {
  p <- tp[tp$etat == "produite", ]
  a <- tp[tp$etat == "qx_arrondi_un", ]
  ok_p <- all(0 < p$borne_inf_exacte & p$borne_inf_exacte < p$qx & p$qx < p$borne_sup_exacte & p$borne_sup_exacte < 1 &
                0 < p$borne_inf_asymptotique & p$borne_inf_asymptotique < p$qx & p$qx < p$borne_sup_asymptotique &
                p$borne_sup_asymptotique < 1)
  ok_a <- all(a$qx_reference < 1) && all(rowSums(as.matrix(a[COLONNES_Q]) == 1) > 0)
  if (ok_p && ok_a) TRUE else sprintf("produite : %s ; qx_arrondi_un : %s", ok_p, ok_a)
}
verifier("T2 (sur T1) : toute ligne produite a 0 < borne_inf < qx < borne_sup < 1 ; toute ligne qx_arrondi_un a q~ < 1 et une valeur égale à 1",
         invariant_etats(RT1$resultat$table_positionnee))
# T3 : frontière. Âge 50 : E^c = 5e7, D = 1e8, q~ = 1 - e^-1 (theta^ voisin de
# 2, bornes à 2e-4 près en relatif) ; âge 51 sans exposition, mu~ = y / 2.
frontiere <- function(y) {
  ex <- data.frame(age = 50:51, exposition_jours = 0, exposition_centrale = c(5e7, 0), evenements = c(100000000L, 0L),
                   sorties_concurrentes = 0L)
  t <- table_reference(data.frame(age = 40:70, sexe = "ensemble", qx = replace(rep(0.01, 31L), 11:12, -expm1(-c(1, y / 2)))),
                       "T3", "importee", "test")
  r <- positionnement_direct(expo = ex, table = t, plage = c(50, 51))
  list(y = r$resultat$smr$smr * r$resultat$cellules$taux_table[2L], etat = r$resultat$table_positionnee$etat[2L])
}
verifier("T3 : frontière theta mu~ = 54 ln 2 : y = 37,42 -> produite ; y = 37,44 -> qx_arrondi_un", {
  a <- frontiere(37.42); b <- frontiere(37.44)
  # y obtenu à 1e-6 près en relatif : 1 - q~ (environ 7,5e-9) est arrondi en
  # double, d'où une erreur relative voisine de 1,5e-8 sur mu~ (mesurée 1,17e-8).
  tous(ya = proche(a$y, 37.42, 1e-6, 0), yb = proche(b$y, 37.44, 1e-6, 0),
       etats = if (identical(c(a$etat, b$etat), c("produite", "qx_arrondi_un"))) TRUE
               else paste(a$etat, b$etat))
})

# --- C. Refus en cours d'étape (valeur calculée à la main : module, motif) --------

verifier("refus en cours d'étape : q~ = 1 avec E^c > 0 -> taux_table_egal_un (module positionnement)",
         controle(refus_de(positionnement_direct(table = table_reference(
           data.frame(age = 40:70, sexe = "ensemble", qx = ifelse(40:70 == 51, 1, 0.01)), "u", "importee", "test"))),
           "positionnement", "taux_table_egal_un"))
verifier("refus en cours d'étape : segment sans décès (O = 0) -> positionnement_sans_deces, qui cite le segment, A et renvoie à la validation", {
  e2 <- rbind(transform(EXPO_B, sexe = "H"), transform(EXPO_B, sexe = "F", evenements = 0L))
  r <- refus_de(positionnement_direct(expo = e2, segmentation = "sexe"))
  ok <- controle(r, "positionnement", "positionnement_sans_deces")
  if (!isTRUE(ok)) ok
  else if (!grepl(sprintf("sexe = « F » (A = %.17g)", r$details$attendus), conditionMessage(r), fixed = TRUE) ||
           grepl("« H »", conditionMessage(r), fixed = TRUE)) conditionMessage(r)
  else if (!grepl("relation independante", conditionMessage(r), fixed = TRUE)) "renvoi à la validation absent"
  else if (!identical(r$details$segments$sexe, "F")) "details"
  else proche(r$details$attendus, 6.5, 1e-14, 0)
})
verifier("refus en cours d'étape : segment sans cellule retenue (A = 0) -> positionnement_sans_attendu, avant positionnement_sans_deces", {
  e2 <- rbind(transform(EXPO_B, sexe = "H"), transform(EXPO_B, sexe = "F", exposition_centrale = 0, evenements = 0L))
  r <- refus_de(positionnement_direct(expo = e2, segmentation = "sexe"))
  ok <- controle(r, "positionnement", "positionnement_sans_attendu")
  if (isTRUE(ok) && !grepl("sexe = « F » (A = 0)", conditionMessage(r), fixed = TRUE)) conditionMessage(r) else ok
})
verifier("refus en cours d'étape : table sans valeur pour un sexe de la construction -> cellule_sans_valeur_table (module positionnement)", {
  e2 <- rbind(transform(EXPO_B, sexe = "H"), transform(EXPO_B, sexe = "F"))
  th <- table_reference(data.frame(age = 40:70, sexe = "H", qx = 0.01), "H seul", "importee", "test")
  controle(refus_de(positionnement_direct(expo = e2, segmentation = "sexe", table = th)), "positionnement",
           "cellule_sans_valeur_table")
})
verifier("refus (appel direct, défense) : table mêlant ensemble et H -> table_sexes_ambigus (module positionnement, préfixe Positionnement)", {
  t <- data.frame(age = c(20:90, 20:90), sexe = rep(c("H", "ensemble"), each = 71), qx = 0.01, stringsAsFactors = FALSE)
  r <- refus_de(tablexp:::controler_entrees_validation(t, "t", "dernier_anniversaire", "sexe", c(20, 90), "positionnement"))
  ok <- controle(r, "positionnement", "table_sexes_ambigus")
  if (isTRUE(ok) && !startsWith(conditionMessage(r), "Positionnement : ")) conditionMessage(r) else ok
})

# --- D. Recoupements indépendants par `stats` (part SMR de #5) ---------------------

PERIODE <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
JEU <- local({
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 20000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
  simuler_portefeuille(loi, pf, PERIODE, 20261106L)
})
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
# Table de référence importée, distincte de la loi vraie (H 1,2 q_GM, F 0,6 q_GM) :
# theta^ attendu voisin de 1/1,2 pour H (q~ plafonné à 1 au-delà de 116 ans,
# hors plage).
T_HF <- table_reference(rbind(data.frame(age = GM$age, sexe = "H", qx = pmin(1, 1.2 * GM$qx)),
                              data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx)),
                        "GM par sexe (H majorée)", "importee", "test")
POS <- list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95)
VAL <- list(table_reference = "tv", relation_table = "independante", plage_ages = c(20, 90),
            tranches_age = c(20, 50, 70), niveau = 0.95, replications = 99)
plan_p <- function(segmentation = "sexe", convention = "plus_fine", par_annee = FALSE, pos = POS, val = NULL) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PERIODE, delai_declaration = 0,
            segmentation = segmentation,
            etapes = list(donnees = list(exclusions = character(0)),
                          exposition = list(convention = convention, par_annee = par_annee)))
  if (!is.null(pos)) p$etapes$positionnement <- pos
  if (!is.null(val)) { p$etapes$validation <- val; p$graine <- 20261106L }
  p
}
X <- construire_table(JEU, ROLES, plan_p(), tables_reference = list(tv = T_HF))
P <- X$etapes$positionnement
local({
  c_ <- P$resultat$cellules
  r <- c_$etat == "retenue"
  s <- P$resultat$smr
  d <- data.frame(D = c_$evenements[r], A = c_$attendus[r], seg = factor(c_$sexe[r], levels = s$sexe))
  # Second ajustement parti des coefficients du premier : la matrice de
  # covariance de glm() est celle des poids de la dernière itération ; partie
  # d'un point convergé, elle est celle de theta^ (se = 1 / sqrt(O)).
  fit0 <- stats::glm(D ~ 0 + seg, family = stats::poisson(), data = d, offset = log(d$A),
                     control = stats::glm.control(epsilon = 1e-16, maxit = 100))
  fit <- stats::glm(D ~ 0 + seg, family = stats::poisson(), data = d, offset = log(d$A), start = stats::coef(fit0),
                    control = stats::glm.control(epsilon = 1e-16, maxit = 100))
  ci <- exp(stats::confint.default(fit, level = 0.95))
  verifier("recoupement : glm de Poisson à offset (D ~ 0 + segment) : exp(coef) = theta par segment ; exp(confint.default) = bornes asymptotiques (convergé)", tous(
    conv = if (isTRUE(fit0$converged) && isTRUE(fit$converged)) TRUE else "glm non convergé",
    theta = proche(s$smr, exp(unname(stats::coef(fit))), 1e-12, 0),
    inf = proche(s$borne_inf_asymptotique, unname(ci[, 1L]), 1e-12, 0),
    sup = proche(s$borne_sup_asymptotique, unname(ci[, 2L]), 1e-12, 0)))
  verifier("recoupement : poisson.test(O, A)$conf.int = bornes exactes, par segment", {
    pt <- lapply(seq_len(nrow(s)), function(i) as.numeric(stats::poisson.test(s$observes[i], s$attendus[i],
                                                                               conf.level = 0.95)$conf.int))
    tous(inf = proche(s$borne_inf_exacte, vapply(pt, `[`, 0, 1L), 1e-14, 0),
         sup = proche(s$borne_sup_exacte, vapply(pt, `[`, 0, 2L), 1e-14, 0))
  })
  tp <- P$resultat$table_positionnee
  th <- s$smr[match(tp$sexe, s$sexe)]
  verifier("recoupement : q = 1 - (1 - q~)^theta recalculé (puissance), écart relatif <= 1e-12 ; q < 1 ; bornes encadrantes", tous(
    q = proche(tp$qx, 1 - (1 - tp$qx_reference)^th, 1e-12, 0),
    sup1 = if (all(tp$borne_sup_exacte < 1 & tp$borne_sup_asymptotique < 1)) TRUE else "q >= 1",
    ordre = if (all(tp$borne_inf_exacte < tp$qx & tp$qx < tp$borne_sup_exacte &
                    tp$borne_inf_asymptotique < tp$qx & tp$qx < tp$borne_sup_asymptotique)) TRUE else "ordre"))
  verifier("recoupement : par segment, somme sur C+ de E^c theta mu~ = O (échelle de la force, M288)", {
    k <- match(c_$sexe[r], s$sexe)
    proche(as.numeric(tapply(c_$exposition_centrale[r] * s$smr[k] * c_$taux_table[r], k, sum)), s$observes, 1e-12, 0)
  })
})

# --- E. Étape dans construire_table() (identité exacte ; valeur calculée à la main) --

verifier("étape : x$table NULL ; resultat (cellules, smr, table_positionnee) ; grille sexe x âges 20 à 90 ; table positionnée segment x âge, colonnes de F7", {
  tp <- P$resultat$table_positionnee
  is.null(X$table) && identical(names(P$resultat), c("cellules", "smr", "table_positionnee")) &&
    identical(nrow(P$resultat$cellules), 142L) && identical(P$resultat$smr$sexe, unique(P$resultat$cellules$sexe)) &&
    identical(names(tp), c("sexe", "age", "qx_reference", "qx", "borne_inf_exacte", "borne_sup_exacte",
                           "borne_inf_asymptotique", "borne_sup_asymptotique", "etat")) &&
    identical(tp$age, rep(20:90, 2L)) && is.data.frame(tp) && is.null(attr(tp, "nom", exact = TRUE))
})
verifier("étape : identité O + sans attendu + hors plage = événements de l'exposition ; paramètres (clé, nom de table, p = 1)", {
  d <- P$diagnostics
  sum(P$resultat$smr$observes) + d$evenements_sans_attendu + d$evenements_hors_plage ==
    sum(X$etapes$exposition$resultat$exposition$evenements) &&
    identical(P$parametres$table_reference, "tv") && identical(P$parametres$table_nom, "GM par sexe (H majorée)") &&
    identical(P$parametres$appariement_sexe, "par_sexe") && identical(P$parametres$convention, "dernier_anniversaire")
})
verifier("étape : par_annee = TRUE -> cellules par année, table positionnée sans année (une ligne par segment x âge), theta égal au cas sans année à 1e-12 près", {
  x <- construire_table(JEU, ROLES, plan_p(par_annee = TRUE), tables_reference = list(tv = T_HF))
  q <- x$etapes$positionnement$resultat
  tous(annee = if ("annee" %in% names(q$cellules) && !("annee" %in% names(q$table_positionnee)) &&
                   identical(q$table_positionnee$age, rep(20:90, 2L))) TRUE else "colonnes ou lignes",
       o = proche(q$smr$observes, P$resultat$smr$observes, 0, 0),
       t = proche(q$smr$smr, P$resultat$smr$smr, 1e-12, 0),
       qx = proche(q$table_positionnee$qx, P$resultat$table_positionnee$qx, 1e-12, 0))
})
verifier("étape : sans segmentation, table « ensemble » -> une ligne par âge, sans colonne de segment", {
  te <- table_reference(data.frame(age = GM$age, sexe = "ensemble", qx = 0.8 * GM$qx), "ens", "importee", "test")
  x <- construire_table(JEU, ROLES, plan_p(segmentation = character(0)), tables_reference = list(tv = te))
  q <- x$etapes$positionnement$resultat
  identical(nrow(q$smr), 1L) && identical(names(q$table_positionnee)[1:2], c("age", "qx_reference")) &&
    identical(q$table_positionnee$age, 20:90)
})
verifier("étape : positionnement et validation sur la même clé -> une seule table dans la fiche ; validation identical() à celle d'un plan sans positionnement (aucun résultat existant ne change)", {
  x1 <- construire_table(JEU, ROLES, plan_p(val = VAL), tables_reference = list(tv = T_HF))
  x0 <- construire_table(JEU, ROLES, plan_p(pos = NULL, val = VAL), tables_reference = list(tv = T_HF))
  identical(names(x1$fiche$tables_reference), "tv") && identical(x1$etapes$validation, x0$etapes$validation) &&
    identical(names(x1$etapes), c("donnees", "exposition", "positionnement", "validation")) &&
    identical(x1$fiche$version_fiche, 6L)
})
verifier("étape : clés distinctes pour positionnement et validation -> deux tables dans la fiche", {
  x <- construire_table(JEU, ROLES, plan_p(val = modifyList(VAL, list(table_reference = "autre"))),
                        tables_reference = list(tv = T_HF, autre = T_HF))
  identical(names(x$fiche$tables_reference), c("autre", "tv"))
})
verifier("T2 (sur le jeu simulé) : invariant des états de la table positionnée", invariant_etats(P$resultat$table_positionnee))
verifier("étape : période sans observation -> positionnement_sans_attendu « aucune cellule », details à zéro segment", {
  p <- plan_p(); p$periode <- list(debut = D("1990-01-01"), fin = D("1994-12-31"))
  r <- refus_de(construire_table(JEU, ROLES, p, tables_reference = list(tv = T_HF)))
  ok <- controle(r, "positionnement", "positionnement_sans_attendu")
  if (!isTRUE(ok)) ok
  else if (!grepl("aucune cellule", conditionMessage(r), fixed = TRUE)) conditionMessage(r)
  else if (!identical(nrow(r$details$segments), 0L) || !identical(r$details$attendus, numeric(0)) ||
           !identical(r$details$cle, "tv")) "details"
  else TRUE
})
verifier("rejeu : rejouer_fiche() sur la fiche écrite puis relue, identical() hors execution ; sans la table -> table_reference_absente", {
  f <- lire_fiche(ecrire_fiche(X$fiche))
  r <- rejouer_fiche(JEU, f, list(tv = T_HF))
  tous(rejeu = if (identical(sans_execution(r$construction), sans_execution(X)) &&
                   all(r$bilan$verdict[1:8] == "conforme")) TRUE else "rejeu",
       absente = controle(refus_de(rejouer_fiche(JEU, f)), "construire_table", "table_reference_absente"))
})

# --- F. Refus avant exécution (valeur calculée à la main : module, motif) ----------

for (cas in list(list("methode", "SMR", "methode_invalide"), list("methode", c("smr", "smr"), "methode_invalide"),
                 list("methode", NA_character_, "methode_invalide"), list("methode", c(a = "smr"), "methode_invalide"),
                 list("table_reference", 1, "cle_table_invalide"), list("table_reference", "", "cle_table_invalide"),
                 list("plage_ages", c(20L, 90L), "plage_ages_invalide"), list("plage_ages", c(90, 20), "plage_ages_invalide"),
                 list("plage_ages", 20, "plage_ages_invalide"), list("plage_ages", c(20.5, 90), "plage_ages_invalide"),
                 list("niveau", 1, "niveau_invalide"), list("niveau", c(a = 0.95), "niveau_invalide")))
  verifier(sprintf("refus valider_plan : %s = %s -> %s (module positionnement)", cas[[1L]],
                   paste(deparse(cas[[2L]]), collapse = ""), cas[[3L]]),
           controle(refus_de(valider_plan({ p <- plan_p(); p$etapes$positionnement[cas[[1L]]] <- list(cas[[2L]]); p })),
                    "positionnement", cas[[3L]]))
verifier("refus valider_plan : paramètre methode absent -> parametre_manquant (aucun défaut, M293)",
         controle(refus_de(valider_plan({ p <- plan_p(); p$etapes$positionnement$methode <- NULL; p })),
                  "construire_table", "parametre_manquant"))
verifier("refus valider_plan : positionnement sans exposition -> etape_prealable_manquante",
         controle(refus_de(valider_plan({ p <- plan_p(); p$etapes$exposition <- NULL; p })),
                  "construire_table", "etape_prealable_manquante"))
verifier("refus valider_plan : positionnement après validation -> etapes_hors_ordre", {
  p <- plan_p(val = VAL)
  p$etapes <- p$etapes[c("donnees", "exposition", "validation", "positionnement")]
  controle(refus_de(valider_plan(p)), "construire_table", "etapes_hors_ordre")
})
verifier("refus valider_plan : plan à positionnement seul avec une graine -> graine_inattendue (étape non aléatoire)",
         controle(refus_de(valider_plan({ p <- plan_p(); p$graine <- 1L; p })), "construire_table", "graine_inattendue"))
avant_execution <- function(plan, tables, motif, module = "positionnement") {
  r <- refus_de(construire_table(JEU, ROLES, plan, tables_reference = tables))
  controle(r, module, motif)
}
verifier("refus avant exécution : convention age_atteint_annee -> convention_positionnement_non_admise",
         avant_execution(plan_p(convention = "age_atteint_annee"), list(tv = T_HF), "convention_positionnement_non_admise"))
verifier("refus avant exécution : table avec annee -> table_annee_non_admise (module positionnement)", {
  ta <- table_reference(data.frame(age = rep(0:120, 2), sexe = "ensemble", qx = 0.01, annee = rep(2015:2016, each = 121)),
                        "prospective", "importee", "test")
  avant_execution(plan_p(), list(tv = ta), "table_annee_non_admise")
})
verifier("refus avant exécution : table par sexe sans sexe dans la segmentation -> table_sexe_incompatible (module positionnement)",
         avant_execution(plan_p(segmentation = character(0)), list(tv = T_HF), "table_sexe_incompatible"))
verifier("refus avant exécution : plage non couverte par la table -> plage_ages_hors_table (module positionnement)",
         avant_execution(plan_p(pos = modifyList(POS, list(plage_ages = c(20, 130)))), list(tv = T_HF),
                         "plage_ages_hors_table"))
verifier("refus avant exécution : clé citée absente des tables reçues -> table_reference_absente (contrôle 1.3)",
         avant_execution(plan_p(), NULL, "table_reference_absente", "construire_table"))
verifier("refus avant exécution : positionnement et validation au plan, convention non admise -> le positionnement refuse en premier (ordre des étapes)",
         avant_execution(plan_p(convention = "age_atteint_annee", val = VAL), list(tv = T_HF),
                         "convention_positionnement_non_admise"))

# --- G. Refus de `validation` inchangés (F2 ; identité exacte avec les textes
# d'avant #167, relevés à 179ba54) ----------------------------------------------

verifier("F2 : refus de validation inchangés en module, motif et message (convention, table par sexe, plage hors table)", {
  tv <- list(tv = T_HF)
  a <- refus_de(construire_table(JEU, ROLES, plan_p(convention = "age_atteint_annee", pos = NULL, val = VAL), tables_reference = tv))
  b <- refus_de(construire_table(JEU, ROLES, plan_p(segmentation = character(0), pos = NULL, val = VAL), tables_reference = tv))
  c_ <- refus_de(construire_table(JEU, ROLES, plan_p(pos = NULL, val = modifyList(VAL, list(plage_ages = c(20, 130)))),
                                  tables_reference = tv))
  tous(
    a = if (isTRUE(controle(a, "validation", "convention_validation_non_admise")) && identical(conditionMessage(a), paste(
      "Validation : convention d'âge retenue « age_atteint_annee » non admise (admise : « dernier_anniversaire »).",
      "Sous age_atteint_annee, l'appariement d'une cellule d'année civile à la table n'est pas spécifié : la",
      "validation est refusée (décision Q7 (a) de #4)."))) TRUE else conditionMessage(a),
    b = if (isTRUE(controle(b, "validation", "table_sexe_incompatible")) && identical(conditionMessage(b), paste(
      "Validation : la table de clé « tv » est par sexe (« H », « F ») et la construction n'est pas segmentée par",
      "sexe : aucune valeur de la table ne s'applique sans déduction ; segmenter par sexe, ou fournir une table",
      "« ensemble »."))) TRUE else conditionMessage(b),
    c = if (isTRUE(controle(c_, "validation", "plage_ages_hors_table")) && identical(conditionMessage(c_), paste(
      "Validation : la table de clé « tv » (sexe H) couvre les âges 0 à 120, qui ne contiennent pas la plage",
      "d'âges 20 à 130."))) TRUE else conditionMessage(c_))
})

# --- H. Rapport figé (valeur calculée à la main) --------------------------------------

verifier("rapport : section etape_positionnement, avant etape_homogeneite (#168) et etape_validation, avec ses mentions fixes et ses tables", {
  h <- rawToChar(ecrire_rapport(X)); Encoding(h) <- "UTF-8"
  s <- tablexp:::SECTIONS_RAPPORT$section
  identical(match("etape_positionnement", s) + 1:2, match(c("etape_homogeneite", "etape_validation"), s)) &&
    all(vapply(c("<section id=\"etape_positionnement\">", "Positionnement par SMR (etapes/positionnement/parametres/methode) : pour chaque segment, theta = O / A",
                 "ne couvrent que l'incertitude sur ce facteur", "Écart à la note de méthode",
                 "<caption><code>etapes/positionnement/resultat/smr</code>",
                 "<caption><code>etapes/positionnement/resultat/table_positionnee</code>",
                 "<caption><code>etapes/positionnement/parametres/statuts</code>",
                 "<p>Étape « validation » : non demandée par le plan"),
               function(m) grepl(m, h, fixed = TRUE), NA)) &&
    !grepl("Unité d'observation contrat, sans identifiant de tête", h, fixed = TRUE)
})
verifier("rapport : plan sans positionnement -> « Étape « positionnement » : non demandée par le plan »", {
  x <- construire_table(JEU, ROLES, plan_p(pos = NULL))
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  grepl("<p>Étape « positionnement » : non demandée par le plan (chemin etapes/positionnement absent de l'objet).</p>",
        h, fixed = TRUE)
})
verifier("rapport : unité contrat (sans id_tete) -> mention d'anti-conservatisme dans la section du positionnement", {
  x <- construire_table(JEU, ROLES[names(ROLES) != "id_tete"], plan_p(), tables_reference = list(tv = T_HF))
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  sec <- regmatches(h, regexpr("<section id=\"etape_positionnement\">.*?</section>", h))
  length(sec) == 1L && grepl("Unité d'observation contrat, sans identifiant de tête", sec, fixed = TRUE)
})
