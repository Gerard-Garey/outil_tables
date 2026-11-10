# -----------------------------------------------------------------------------
#  tests/unitaires/test_validation.R : étape `validation`, statistiques de
#  validation de la table de référence prise telle quelle (issue #4 ;
#  spécification décidée M217, commentaire 6664408617 de #4 ; avis de forme
#  (0''), M242 à M248 ; six points, M253 à M258, commentaire 6682191067)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : jeux J1 à J4 de la spécification (§ 4 ;
#    valeurs recalculées par la session principale sous R 4.3.3), formule de
#    l'erreur de Monte-Carlo (M253), constantes, motifs et ordre des refus,
#    états ;
#  - recoupement indépendant par `stats` : glm() de Poisson (déviance, résidus
#    de Pearson, SMR), poisson.test(), binom.test(), wilcox.test() ; loi des
#    suites par énumération (combn()) ;
#  - identité exacte : reproductibilité au bit à graine égale, découpage des
#    réplications par blocs, rejeu identical() hors execution, égalité de
#    mu^ avec l'étape `brut` ;
#  - constat de simulation : taille des p-valeurs de Monte-Carlo sur une
#    petite base (J5), région d'acceptation binomiale déclarée.
#  Bout en bout de #73 (M247) : construction puis rejeu avec une table de
#  référence importée.
# -----------------------------------------------------------------------------

D <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
sv <- tablexp:::statistiques_validation
valeur <- function(s, cle) s$valeur[match(cle, paste(s$statistique, s$grandeur, sep = "/"))]
etat_de <- function(s, cle) s$etat[match(cle, paste(s$statistique, s$grandeur, sep = "/"))]
# Refus attendu : module, motif ; TRUE ou le texte de l'écart.
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
# Plusieurs comparaisons proche() : TRUE ou la première en écart.
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}

# --- A. Constantes et catalogue (valeur calculée à la main) -----------------------

verifier("A constantes : RELATIONS_TABLE (positionnee = 1L, #167, F5), CONVENTIONS_VALIDATION, N_MAX_WILCOXON_EXACT = 1000, N_MAX_RUNS_EXACT = 10000, TOLERANCE_RESIDU_NUL = 1e-10, PARAMETRES_CLE_TABLE (positionnement puis validation, #167)",
         identical(tablexp:::RELATIONS_TABLE, c(independante = 0L, ajustee_hors_outil = NA_integer_, positionnee = 1L)) &&
           identical(tablexp:::CONVENTIONS_VALIDATION, "dernier_anniversaire") &&
           identical(tablexp:::N_MAX_WILCOXON_EXACT, 1000) && identical(tablexp:::N_MAX_RUNS_EXACT, 10000) &&
           identical(tablexp:::TOLERANCE_RESIDU_NUL, 1e-10) &&
           identical(tablexp:::PARAMETRES_CLE_TABLE,
                     c("etapes/positionnement/table_reference", "etapes/validation/table_reference")))
verifier("A ETAPES : validation après brut, positionnement (#167) et homogeneite (#168), module validation, préalables donnees et exposition, aléatoire comme homogeneite (importance_exclusions après elle, #69)", {
  e <- tablexp:::ETAPES
  k <- match("validation", e$etape)
  identical(e$etape, c("donnees", "exposition", "brut", "positionnement", "homogeneite", "validation",
                       "importance_exclusions")) &&
    identical(e$module[k], "validation") &&
    identical(e$fonction[k], "calculer_validation") && identical(e$prealable[[k]], c("donnees", "exposition")) &&
    identical(e$aleatoire, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))
})
verifier("A PARAMETRES_ETAPES$validation : six paramètres dans l'ordre, types character et double", {
  p <- tablexp:::PARAMETRES_ETAPES$validation
  identical(p$parametre, c("table_reference", "relation_table", "plage_ages", "tranches_age", "niveau", "replications")) &&
    identical(p$type, c("character", "character", "double", "double", "double", "double"))
})
verifier("A correspondance MODALITES_SEXE -> SEXES_TABLE : identité (M253)",
         identical(tablexp:::SEXES_TABLE[match(tablexp:::MODALITES_SEXE, tablexp:::SEXES_TABLE)], tablexp:::MODALITES_SEXE))
verifier("A STATUTS_VALIDATION : (statistique, grandeur) uniques ; sous_h0 logique ; statuts non vides", {
  s <- tablexp:::STATUTS_VALIDATION
  !anyDuplicated(paste(s$statistique, s$grandeur)) && is.logical(s$sous_h0) && !anyNA(s$sous_h0) && all(nzchar(s$statut))
})
verifier("A erreur de Monte-Carlo (M253) : B = 999, K = 49 -> p = 0,05, e = 0,00688857750192302 ; sens", {
  pv <- tablexp:::pvaleur_monte_carlo
  t_rep <- c(rep(2, 49), rep(0, 950))
  a <- pv(1, t_rep, "superieure")
  b <- pv(1, t_rep, "inferieure")
  c_ <- pv(1, t_rep, "centrale")
  tous(sup = proche(a, c(0.05, 0.00688857750192302), 1e-15, 0), inf = proche(b[["p"]], 951 / 1000, 1e-15, 0),
       cen = proche(c_[["p"]], 0.1, 1e-15, 0))
})

# --- B. J1, J2, J3 (d) : statistiques sur des cellules (valeur calculée à la main) --

J1 <- list(E = c(128, 256, 512, 64), mu = c(1 / 256, 1 / 128, 1 / 128, 7 / 128), D = c(0L, 3L, 5L, 4L))
stat_j <- function(E, mu, D, pe = "independante", B = 99, graine = 1L)
  sv(D, E, -expm1(-mu), mu, E * mu, rep("retenue", length(D)), rep(1L, length(D)), seq_along(D), 0.95, pe, B, graine)
S1 <- stat_j(J1$E, J1$mu, J1$D)
s1 <- S1$statistiques
verifier("J1 : A = (0,5 ; 2 ; 4 ; 3,5) ; SMR 1,2 ; O - A = 2 ; IC 95 % exact ; queues de Poisson et centrale", tous(
  A = proche(S1$cellules$attendus, c(0.5, 2, 4, 3.5), 1e-15, 0),
  smr = proche(valeur(s1, "smr/smr"), 1.2, 1e-15, 0), ecart = proche(valeur(s1, "smr/ecart"), 2, 1e-15, 0),
  ic = proche(valeur(s1, c("smr/borne_inf_exacte", "smr/borne_sup_exacte")), c(0.620057510872222, 2.0961585048177), 1e-13, 0),
  sup = proche(valeur(s1, "smr/p_superieure"), 0.303223853696893, 1e-13, 0),
  inf = proche(valeur(s1, "smr/p_inferieure"), 0.791556476394874, 1e-13, 0),
  cen = proche(valeur(s1, "smr/p_centrale"), 0.606447707393787, 1e-13, 0)))
verifier("J1 : racine cubique corrigée z = 0,516546517899557 (p = 0,30274), forme imprimée -0,74739 non obtenue", tous(
  z = proche(valeur(s1, "smr/z_racine_cubique"), 0.516546517899557, 1e-13, 0),
  p = proche(valeur(s1, "smr/p_racine_cubique"), 0.30274, 0, 5e-6),
  imprimee = if (abs(valeur(s1, "smr/z_racine_cubique") + 0.74739) > 0.1) TRUE else "forme imprimée obtenue"))
verifier("J1 : X2 = 37/28, G2 = 1,73247730278726, ddl 4", tous(
  x2 = proche(valeur(s1, "khi2_pearson/statistique"), 37 / 28, 1e-14, 0),
  g2 = proche(valeur(s1, "deviance/statistique"), 1.73247730278726, 1e-13, 0),
  ddl = proche(valeur(s1, c("khi2_pearson/ddl", "deviance/ddl")), c(4, 4), 0, 0)))
verifier("J1 : signes n+ = 3, n- = 1, statistique 0,5, p = 0,617075077451974, exacte 0,625", tous(
  n = proche(valeur(s1, c("signes/positifs", "signes/negatifs", "signes/nuls", "signes/effectif")), c(3, 1, 0, 4), 0, 0),
  xi = proche(valeur(s1, "signes/statistique"), 0.5, 1e-15, 0),
  p = proche(valeur(s1, "signes/p_asymptotique"), 0.617075077451974, 1e-13, 0),
  ex = proche(valeur(s1, "signes/p_exacte"), 0.625, 1e-15, 0)))
verifier("J1 : runs (-, +, +, +) R = 2, espérance 2,5, variance 0,25, statistique -1, exacte P(R <= 2) = 0,5", tous(
  r = proche(valeur(s1, c("runs/sequences", "runs/runs", "runs/esperance", "runs/variance", "runs/statistique")),
             c(1, 2, 2.5, 0.25, -1), 1e-15, 0),
  ex = proche(valeur(s1, "runs/p_exacte"), 0.5, 1e-13, 0)))
verifier("J1 : Wilcoxon w+ = 7,5, variance 7,375, z = 0,736459694318659, p = 0,461450987833361 ; exacte : état ex_aequo", tous(
  w = proche(valeur(s1, c("wilcoxon/w_plus", "wilcoxon/variance", "wilcoxon/groupes_ex_aequo")), c(7.5, 7.375, 1), 1e-15, 0),
  z = proche(valeur(s1, "wilcoxon/statistique"), 0.736459694318659, 1e-13, 0),
  p = proche(valeur(s1, "wilcoxon/p_asymptotique"), 0.461450987833361, 1e-13, 0),
  etat = if (identical(etat_de(s1, "wilcoxon/p_exacte"), "ex_aequo") && is.na(valeur(s1, "wilcoxon/p_exacte"))) TRUE else "état"))
verifier("J1 : MAPE 21,9444444444444 (5,48611111111111 non obtenu) ; R2 = 2391/2491 ; III.2.a 0,0170708640315692 (9/512 non obtenu)", tous(
  mape = proche(valeur(s1, "mape/valeur"), 21.9444444444444, 1e-13, 0),
  r2 = proche(valeur(s1, "r2/valeur"), 2391 / 2491, 1e-13, 0),
  d = proche(valeur(s1, "distance_probabilites/valeur"), 0.0170708640315692, 1e-12, 0),
  non = if (abs(valeur(s1, "mape/valeur") - 5.48611111111111) > 1 &&
            abs(valeur(s1, "distance_probabilites/valeur") - 9 / 512) > 1e-4) TRUE else "forme écartée obtenue"))
verifier("J1 : fluctuation 95 % L = (0, 0, 1, 0), U = (2, 5, 8, 8) ; espérance hors intervalle 0,0805040178804083 ; E[#|r^P| > 2] = 0,190959384354065", tous(
  L = proche(S1$cellules$fluctuation_inf_exacte, c(0, 0, 1, 0), 0, 0),
  U = proche(S1$cellules$fluctuation_sup_exacte, c(2, 5, 8, 8), 0, 0),
  e = proche(valeur(s1, "fluctuation/esperance_hors_intervalle_exact"), 0.0805040178804083, 1e-12, 0),
  hors = proche(valeur(s1, "fluctuation/hors_intervalle_exact"), 0, 0, 0),
  r2 = proche(valeur(s1, "residus/esperance_au_dela_2"), 0.190959384354065, 1e-12, 0)))
verifier("J1 : toutes les grandeurs de STATUTS_VALIDATION (hors sous_groupes) présentes, dans l'ordre ; états dans ETATS_GRANDEUR_VALIDATION", {
  sv_ <- tablexp:::STATUTS_VALIDATION
  sv_ <- sv_[sv_$statistique != "sous_groupes", ]
  identical(s1$statistique, sv_$statistique) && identical(s1$grandeur, sv_$grandeur) &&
    all(s1$etat %in% tablexp:::ETATS_GRANDEUR_VALIDATION) && identical(names(s1), c("statistique", "grandeur", "valeur", "etat", "statut"))
})
verifier("J1, sous-groupe {1, 2} sous p = 0 : Poisson(2,5), P(X >= 3) = 0,45618688411667", {
  s <- tablexp:::smr_poisson(3, 2.5, 0.95)
  proche(s$p_superieure, 0.45618688411667, 1e-12, 0)
})
verifier("J2, résidu nul (cellule E = 8, mu~ = 1/4, D = 2 insérée) : n+ = 3, n- = 1, nuls 1, R = 2 ; X2, G2 inchangés ; n+ = 4 non obtenu", {
  E <- c(128, 8, 256, 512, 64); mu <- c(1 / 256, 1 / 4, 1 / 128, 1 / 128, 7 / 128); Dd <- c(0L, 2L, 3L, 5L, 4L)
  s2 <- stat_j(E, mu, Dd)$statistiques
  tous(signes = proche(valeur(s2, c("signes/positifs", "signes/negatifs", "signes/nuls")), c(3, 1, 1), 0, 0),
       runs = proche(valeur(s2, "runs/runs"), 2, 0, 0),
       x2 = proche(valeur(s2, c("khi2_pearson/statistique", "deviance/statistique")),
                   valeur(s1, c("khi2_pearson/statistique", "deviance/statistique")), 1e-14, 0),
       wilc = proche(valeur(s2, "wilcoxon/nuls"), 1, 0, 0))
})
verifier("J3 (d), D = 0 partout, somme A = 2 : SMR 0, IC [0 ; 1,84443972705697], P(X <= 0) = 0,135335283236613, z = 1,11309648301795, G2 = 4, X2 = 2, MAPE (aucun_deces) et R2 (variance_observee_nulle) NA", {
  s <- stat_j(c(1, 1), c(1, 1), c(0L, 0L))$statistiques
  tous(smr = proche(valeur(s, c("smr/smr", "smr/borne_inf_exacte", "smr/borne_sup_exacte")), c(0, 0, 1.84443972705697), 1e-13, 0),
       p = proche(valeur(s, "smr/p_inferieure"), 0.135335283236613, 1e-13, 0),
       z = proche(valeur(s, "smr/z_racine_cubique"), 1.11309648301795, 1e-13, 0),
       g = proche(valeur(s, c("deviance/statistique", "khi2_pearson/statistique")), c(4, 2), 1e-15, 0),
       na = if (identical(etat_de(s, c("mape/valeur", "r2/valeur")), c("aucun_deces", "variance_observee_nulle")) &&
                all(is.na(valeur(s, c("mape/valeur", "r2/valeur"))))) TRUE else "états MAPE / R2")
})
verifier("J1 sous ajustee_hors_outil : grandeurs sous H0 NA (ddl_non_etabli), descriptives produites, aucun tirage", {
  S <- stat_j(J1$E, J1$mu, J1$D, pe = "ajustee_hors_outil")
  s <- S$statistiques
  h0 <- tablexp:::STATUTS_VALIDATION$sous_h0[tablexp:::STATUTS_VALIDATION$statistique != "sous_groupes"]
  if (!all(s$etat[h0] == "ddl_non_etabli") || !all(is.na(s$valeur[h0]))) "grandeurs sous H0"
  else if (!all(s$etat[!h0] == "produite") || !identical(s$valeur[!h0], s1$valeur[!h0])) "grandeurs descriptives"
  else if (!identical(S$tirages_effectues, 0L) || !all(is.na(S$cellules$fluctuation_inf_exacte))) "tirages ou intervalles"
  else TRUE
})
verifier("aucune cellule retenue : toutes les grandeurs NA, état aucune_cellule_retenue, aucun tirage", {
  S <- sv(0L, 0, 0.1, -log1p(-0.1), 0, "sans_exposition", 1L, 1L, 0.95, "independante", 9, 1L)
  all(S$statistiques$etat == "aucune_cellule_retenue") && all(is.na(S$statistiques$valeur)) && S$tirages_effectues == 0
})

# Résidus nuls (M1 de l'audit de #4) : un seul ensemble de zéros, |D - A| <=
# TOLERANCE_RESIDU_NUL x A, pour les signes, les suites et Wilcoxon, observé et
# répliques (valeur calculée à la main ; identité exacte).
verifier("résidu nul à la tolérance : E = 49, mu~ = 1/49, D = 1 (A - 1 = -1,1e-16 en double) -> nul pour S6, S7, S8", {
  E <- c(J1$E, 49); mu <- c(J1$mu, 1 / 49); Dd <- c(J1$D, 1L)
  s <- stat_j(E, mu, Dd)$statistiques
  tous(ecart = if ((E * mu)[5L] != 1) TRUE else "A = 1 exactement : cas non construit",
       signes = proche(valeur(s, c("signes/positifs", "signes/negatifs", "signes/nuls")), c(3, 1, 1), 0, 0),
       runs = proche(valeur(s, "runs/runs"), 2, 0, 0),
       wilc = proche(valeur(s, c("wilcoxon/effectif", "wilcoxon/nuls")), c(4, 1), 0, 0))
})
verifier("résidu |D - A| = 1e-6 non nul (E = 2, mu~ = (1 - 1e-6) / 2, D = 1) : positif pour S6, S7, S8", {
  E <- c(J1$E, 2); mu <- c(J1$mu, (1 - 1e-6) / 2); Dd <- c(J1$D, 1L)
  s <- stat_j(E, mu, Dd)$statistiques
  tous(signes = proche(valeur(s, c("signes/positifs", "signes/negatifs", "signes/nuls")), c(4, 1, 0), 0, 0),
       runs = proche(valeur(s, "runs/runs"), 2, 0, 0),
       wilc = proche(valeur(s, c("wilcoxon/effectif", "wilcoxon/nuls")), c(5, 0), 0, 0))
})
verifier("statistiques_test() et le chemin observé : mêmes signes, suites et z de Wilcoxon (identical), zéros à la tolérance, deux séquences", {
  E <- c(128, 49, 256, 2, 512, 64, 49, 8); mu <- c(1 / 256, 1 / 49, 1 / 128, (1 - 1e-6) / 2, 1 / 128, 7 / 128, 1 / 49, 1 / 4)
  Dd <- c(0L, 1L, 3L, 1L, 5L, 4L, 1L, 2L); sq <- c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L); A <- E * mu
  s <- sv(Dd, E, -expm1(-mu), mu, A, rep("retenue", 8L), sq, seq_along(Dd), 0.95, "independante", 19, 1L)$statistiques
  t <- tablexp:::statistiques_test(matrix(as.numeric(Dd), ncol = 1L), A, E, mu, match(sq, sq))
  np <- valeur(s, "signes/positifs"); nm <- valeur(s, "signes/negatifs")
  identical(t$signes, (np - nm) / sqrt(np + nm)) && identical(as.numeric(t$runs), valeur(s, "runs/runs")) &&
    identical(t$wilcoxon, valeur(s, "wilcoxon/statistique")) && identical(valeur(s, "signes/nuls"), 3)
})
verifier("Wilcoxon sur d = D / E^c - mu~ : deux cellules D = 0 de même mu~ = 0,1, E = 3 et 7, en un seul groupe d'ex aequo ; T* = T (identical)", {
  E <- c(3, 7, 10, 20); mu <- rep(0.1, 4L); Dd <- c(0L, 0L, 3L, 1L); A <- E * mu
  s <- sv(Dd, E, -expm1(-mu), mu, A, rep("retenue", 4L), rep(1L, 4L), seq_along(Dd), 0.95, "independante", 19, 1L)$statistiques
  t <- tablexp:::statistiques_test(matrix(as.numeric(Dd), ncol = 1L), A, E, mu, rep(1L, 4L))
  tous(construit = if ((0 - A[1L]) / E[1L] != (0 - A[2L]) / E[2L]) TRUE else "(D - A) / E^c égaux : cas non construit",
       groupes = proche(valeur(s, c("wilcoxon/effectif", "wilcoxon/groupes_ex_aequo")), c(4, 1), 0, 0),
       etat = if (identical(etat_de(s, "wilcoxon/p_exacte"), "ex_aequo")) TRUE else "état de p_exacte",
       t = if (identical(t$wilcoxon, valeur(s, "wilcoxon/statistique"))) TRUE else "T* différent de T")
})
verifier("aucun résidu non nul (E = 2, mu~ = 0,5, D = A = 1 partout) : p de Monte-Carlo et erreur NA (aucun_residu_non_nul) pour signes, suites, Wilcoxon ; X2 et G2 produites", {
  s <- stat_j(rep(2, 5), rep(0.5, 5), rep(1L, 5), B = 19)$statistiques
  k <- paste0(rep(c("signes", "runs", "wilcoxon"), each = 2L), "/", c("p_monte_carlo", "erreur_monte_carlo"))
  k2 <- c("khi2_pearson/p_monte_carlo", "deviance/p_monte_carlo")
  all(is.na(valeur(s, k))) && all(etat_de(s, k) == "aucun_residu_non_nul") &&
    all(etat_de(s, k2) == "produite") && identical(valeur(s, k2), c(1, 1))
})
# Plafond de la loi exacte des suites (M2 de l'audit de #4 ; valeur calculée à
# la main) : somme des n = 10 000 produite, 10 001 -> NA, effectif_au_dela_n_max,
# asymptotique et Monte-Carlo produites.
for (nr in c(10000, 10001))
  verifier(sprintf("suites : %d résidus non nuls -> p exacte %s ; asymptotique et Monte-Carlo produites", nr,
                   if (nr > 10000) "NA (effectif_au_dela_n_max)" else "produite"), {
    s <- stat_j(rep(1, nr), rep(0.5, nr), rep(c(0L, 1L), length.out = nr), B = 1)$statistiques
    e <- etat_de(s, c("runs/p_exacte", "runs/p_asymptotique", "runs/p_monte_carlo"))
    if (nr > 10000) identical(e, c("effectif_au_dela_n_max", "produite", "produite")) && is.na(valeur(s, "runs/p_exacte"))
    else all(e == "produite") && !is.na(valeur(s, "runs/p_exacte"))
  })

# --- C. J4 : loi exacte des suites (énumération par combn(), indépendante) --------

runs_enum <- function(n1, n2) {
  n <- n1 + n2
  r <- apply(utils::combn(n, n1), 2L, function(pos) { s <- rep(-1, n); s[pos] <- 1; 1 + sum(diff(s) != 0) })
  tabulate(r, nbins = n) / length(r)
}
verifier("J4 : (3, 3) -> P(R = 2..6) = (0,1 ; 0,2 ; 0,4 ; 0,2 ; 0,1), égale à l'énumération", {
  l <- tablexp:::loi_runs(3, 3)
  tous(spec = proche(l[3:7], c(0.1, 0.2, 0.4, 0.2, 0.1), 1e-13, 0), enum = proche(l[-1L], runs_enum(3, 3), 1e-13, 0))
})
verifier("J4 : séquences (1, 1) et (3, 1) -> P(R = 4) = P(R = 5) = 0,5", {
  l <- tablexp:::loi_runs(c(1, 3), c(1, 1))
  proche(l, c(0, 0, 0, 0, 0.5, 0.5, 0), 1e-13, 0)
})
for (nn in list(c(5, 4), c(7, 2), c(1, 6), c(6, 6)))
  verifier(sprintf("J4 : loi des suites (%d, %d) égale à l'énumération (combn)", nn[1L], nn[2L]),
           proche(tablexp:::loi_runs(nn[1L], nn[2L])[-1L], runs_enum(nn[1L], nn[2L]), 1e-12, 0))

# --- D. J5 : Monte-Carlo (identité exacte ; constat de simulation) -----------------

verifier("J5 : reproductible au bit près à graine égale ; autre graine, autres p-valeurs de Monte-Carlo", {
  a <- stat_j(J1$E, J1$mu, J1$D, B = 199, graine = 7L)
  b <- stat_j(J1$E, J1$mu, J1$D, B = 199, graine = 7L)
  E <- c(rep(64, 30)); mu <- seq(0.01, 0.1, length.out = 30); set.seed(3); Dd <- rpois(30, E * mu)
  c1 <- stat_j(E, mu, Dd, B = 199, graine = 7L); c2 <- stat_j(E, mu, Dd, B = 199, graine = 8L)
  identical(a, b) && !identical(valeur(c1$statistiques, "khi2_pearson/p_monte_carlo"),
                                valeur(c2$statistiques, "khi2_pearson/p_monte_carlo"))
})
verifier("J5 : réplications par blocs de colonnes identiques au tirage d'un seul bloc (découpage par réplications)", {
  A <- c(0.5, 2, 4, 3.5, 10)
  f <- function(Dm) list(s = colSums(Dm), m = Dm[1L, ])
  r1 <- tablexp:::replications_poisson(A, 997, 11L, f)
  r2 <- tablexp:::replications_poisson(A, 997, 11L, f, elements_bloc = 7 * length(A))
  r3 <- tablexp:::sous_graine(11L, stats::rpois(length(A) * 997, A))
  identical(r1, r2) && identical(r1$m, r3[seq(1, length(r3), by = length(A))]) && length(r1$s) == 997L
})
verifier("J5 : état de l'aléa de l'appelant restauré après l'étape (sous_graine)", {
  set.seed(42); avant <- .Random.seed
  stat_j(J1$E, J1$mu, J1$D, B = 19, graine = 3L)
  identical(.Random.seed, avant)
})
# Constat de simulation (plan de la spécification § 5 complété : profil
# d'exposition non consigné, E^c constante par âge retenue ici ; âges 20 à
# 100, mu = exp(-10,4 + 0,1 x), somme A = 497) : 400 jeux sous H0, B = 199.
# Région d'acceptation : taille de chaque p-valeur de Monte-Carlo au plus
# 0,05 + 3 sqrt(0,05 x 0,95 / 400) = 0,0827 (niveau exact P_H0(p_MC <= alpha)
# <= alpha) ; signes asymptotique : taille exacte sous H0 (i) par convolution
# de la loi de Poisson-binomiale de n+ (0,9187), à 3 écarts-types ; la taille
# des signes asymptotique n'est pas comparable aux 0,5220 de la spécification.
# Mesure hors batterie, 2 000 jeux : compte rendu du commit (3).
local({
  ages <- 20:100; mu <- exp(-10.4 + 0.1 * ages)
  E <- rep(497 / sum(mu), length(ages)); A <- E * mu
  set.seed(20261106L, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  Dall <- matrix(rpois(length(A) * 400L, A), nrow = length(A))
  cles <- c("khi2_pearson/p_monte_carlo", "deviance/p_monte_carlo", "signes/p_monte_carlo", "runs/p_monte_carlo",
            "wilcoxon/p_monte_carlo", "signes/p_asymptotique")
  P <- t(vapply(seq_len(400L), function(i)
    valeur(sv(Dall[, i], E, -expm1(-mu), mu, A, rep("retenue", length(A)), rep(1L, length(A)), ages, 0.95, "independante", 199,
              1000L + i)$statistiques, cles), numeric(length(cles))))
  taille <- colMeans(P <= 0.05)
  names(taille) <- cles
  # Taille exacte du test des signes asymptotique : n+ ~ Poisson-binomiale de
  # paramètres P(D_c - A_c > 1e-10 A_c) (aucun A_c entier : n = 81).
  pp <- stats::ppois(floor(A * (1 + 1e-10)), A, lower.tail = FALSE)
  f <- 1
  for (p in pp) f <- c(f * (1 - p), 0) + c(0, f * p)
  k <- 0:length(A)
  t_ex <- sum(f[pmin(1, 2 * stats::pnorm((abs(2 * k - length(A)) - 1) / sqrt(length(A)), lower.tail = FALSE)) <= 0.05])
  verifier(sprintf("J5 constat de simulation, petite base (400 jeux, B = 199) : tailles de Monte-Carlo <= 0,0827 (%s) ; signes asymptotique %.4f, taille exacte %.4f",
                   paste(sprintf("%.4f", taille[1:5]), collapse = ", "), taille[6L], t_ex),
           all(taille[1:5] <= 0.05 + 3 * sqrt(0.05 * 0.95 / 400)) && abs(t_ex - 0.9187) < 5e-5 &&
             abs(taille[6L] - t_ex) <= 3 * sqrt(t_ex * (1 - t_ex) / 400))
})

# --- E. Recoupements indépendants par `stats` --------------------------------------

recouper <- function(D, A, E, mu, s, etiquette) {
  m0 <- stats::glm(D ~ offset(log(A)) - 1, family = stats::poisson())
  m1 <- stats::glm(D ~ 1 + offset(log(A)), family = stats::poisson(), control = stats::glm.control(epsilon = 1e-14))
  O <- sum(D); At <- sum(A)
  pt <- stats::poisson.test(O, At)
  r <- D - A
  np <- sum(r > 0); nm <- sum(r < 0)
  w <- stats::wilcox.test(D / E - mu, exact = FALSE, correct = TRUE)
  verifier(sprintf("recoupement %s : glm Poisson (déviance = G2, somme des résidus de Pearson^2 = X2, exp(coef) = SMR), poisson.test (IC, queues), binom.test (signes), wilcox.test (asymptotique)", etiquette), tous(
    g2 = proche(valeur(s, "deviance/statistique"), stats::deviance(m0), 1e-10, 0),
    x2 = proche(valeur(s, "khi2_pearson/statistique"), sum(stats::residuals(m0, type = "pearson")^2), 1e-10, 0),
    smr = proche(valeur(s, "smr/smr"), exp(unname(stats::coef(m1))), 1e-10, 0),
    ic = proche(valeur(s, c("smr/borne_inf_exacte", "smr/borne_sup_exacte")), as.numeric(pt$conf.int), 1e-12, 0),
    sup = proche(valeur(s, "smr/p_superieure"), stats::poisson.test(O, At, alternative = "greater")$p.value, 1e-12, 0),
    inf = proche(valeur(s, "smr/p_inferieure"), stats::poisson.test(O, At, alternative = "less")$p.value, 1e-12, 0),
    signes = proche(valeur(s, "signes/p_exacte"), stats::binom.test(np, np + nm)$p.value, 1e-12, 0),
    wilc = proche(valeur(s, "wilcoxon/p_asymptotique"), w$p.value, 1e-12, 0)))
}
recouper(as.numeric(J1$D), J1$E * J1$mu, J1$E, J1$mu, s1, "J1")
verifier("recoupement J1 : la p-valeur bilatérale de poisson.test() n'est pas la centrale (0,5234 contre 0,6064)", tous(
  pt = proche(stats::poisson.test(12, 10)$p.value, 0.5234, 0, 1e-4),
  cen = proche(valeur(s1, "smr/p_centrale"), 0.6064, 0, 1e-4)))

# Jeu simulé (20 000 têtes, loi vraie de jeu_reference()) et table vraie.
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
T_HF <- table_reference(rbind(data.frame(age = GM$age, sexe = "H", qx = GM$qx),
                              data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx)),
                        "GM vraie par sexe", "importee", "loi vraie du jeu simulé (test)")
T_ENS <- table_reference(data.frame(age = GM$age, sexe = "ensemble", qx = 0.8 * GM$qx), "GM ensemble", "importee",
                         "loi moyenne (test)")
VAL <- list(table_reference = "tv", relation_table = "independante", plage_ages = c(20, 90),
            tranches_age = c(20, 50, 70), niveau = 0.95, replications = 199)
plan_v <- function(segmentation = "sexe", convention = "plus_fine", par_annee = FALSE, val = VAL, brut = TRUE,
                   graine = 20261106L) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PERIODE, delai_declaration = 0,
            segmentation = segmentation,
            etapes = list(donnees = list(exclusions = character(0)),
                          exposition = list(convention = convention, par_annee = par_annee)))
  if (brut) p$etapes$brut <- list(estimateurs = "poisson", niveau = 0.95)
  if (!is.null(val)) p$etapes$validation <- val
  if (!is.null(graine)) p$graine <- graine
  p
}
X <- construire_table(JEU, ROLES, plan_v(), tables_reference = list(tv = T_HF))
V <- X$etapes$validation
local({
  c_ <- V$resultat$cellules
  r <- c_$etat == "retenue"
  o <- order(c_$sexe[r], c_$age[r])
  recouper(as.numeric(c_$evenements[r][o]), c_$attendus[r][o], c_$exposition_centrale[r][o], c_$taux_table[r][o],
           V$resultat$statistiques, "jeu simulé (20 000 têtes)")
  d <- (c_$evenements / c_$exposition_centrale - c_$taux_table)[r]
  verifier("recoupement jeu simulé : sans ex aequo, n <= 1000, p exacte de Wilcoxon = wilcox.test(exact = TRUE)", tous(
    groupes = proche(valeur(V$resultat$statistiques, "wilcoxon/groupes_ex_aequo"), 0, 0, 0),
    p = proche(valeur(V$resultat$statistiques, "wilcoxon/p_exacte"), stats::wilcox.test(d, exact = TRUE)$p.value, 1e-12, 0)))
})

# --- F. Étape dans construire_table() (identité exacte ; valeur calculée à la main) --

verifier("étape : mu^ (taux_brut) identique au taux de Poisson de l'étape brut, cellule par cellule", {
  b <- X$etapes$brut$resultat$poisson
  c_ <- V$resultat$cellules
  k <- match(paste(c_$sexe, c_$age), paste(b$sexe, b$age))
  identical(c_$taux_brut[!is.na(k)], b$taux[k[!is.na(k)]]) && all(is.na(c_$taux_brut[is.na(k)]))
})
verifier("étape : grille sexe x âges 20 à 90 ; identité O + sans attendu + hors plage = événements de l'exposition", {
  c_ <- V$resultat$cellules; d <- V$diagnostics
  nrow(c_) == 2L * 71L && identical(c_$age, rep(20:90, 2L)) &&
    sum(c_$evenements[c_$etat == "retenue"]) + d$evenements_sans_attendu + d$evenements_hors_plage ==
      sum(X$etapes$exposition$resultat$exposition$evenements) &&
    valeur(V$resultat$statistiques, "smr/observes") == sum(c_$evenements[c_$etat == "retenue"])
})
verifier("étape : paramètres (référence de méthode M244, p = 0, appariement par sexe), monte_carlo (B tirages, graine du plan)", {
  p <- V$parametres
  identical(p$reference_methode, tablexp:::REFERENCE_METHODE_VALIDATION) && identical(p$parametres_estimes, 0L) &&
    identical(p$appariement_sexe, "par_sexe") && identical(p$table_reference, "tv") &&
    identical(p$table_nom, "GM vraie par sexe") && identical(V$resultat$monte_carlo$tirages_effectues, 199L) &&
    identical(V$resultat$monte_carlo$graine, 20261106L) && identical(X$fiche$graine_mention, "graine du plan")
})
verifier("étape : sous-groupes segment x tranche, tranche, segment ; somme des O par regroupement = O global", {
  g <- V$resultat$sous_groupes
  O <- valeur(V$resultat$statistiques, "smr/observes")
  identical(unique(g$regroupement), c("segment_tranche", "tranche", "segment")) &&
    all(tapply(g$observes, g$regroupement, sum) == O) &&
    identical(g$tranche_fin[g$regroupement == "tranche"], c(49L, 69L, 90L))
})
verifier("étape : table « ensemble » face à une segmentation par sexe -> appariement ensemble_par_sexe, mention au rapport", {
  x <- construire_table(JEU, ROLES, plan_v(brut = FALSE), tables_reference = list(tv = T_ENS))
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  identical(x$etapes$validation$parametres$appariement_sexe, "ensemble_par_sexe") &&
    grepl("appliquée à chaque sexe de la segmentation", h, fixed = TRUE)
})
verifier("étape : par_annee = TRUE admis, séquences sexe x année, sous-groupe annee ; SMR égal au cas sans année (O, A additifs)", {
  x <- construire_table(JEU, ROLES, plan_v(par_annee = TRUE, brut = FALSE), tables_reference = list(tv = T_HF))
  v <- x$etapes$validation
  s <- v$resultat$statistiques
  tous(seq = proche(valeur(s, "runs/sequences"), 20, 0, 0),
       smr = proche(valeur(s, "smr/smr"), valeur(V$resultat$statistiques, "smr/smr"), 1e-12, 0),
       an = if ("annee" %in% v$resultat$sous_groupes$regroupement && identical(nrow(v$resultat$sequences_runs), 20L)) TRUE else "annee")
})
verifier("étape : relation ajustee_hors_outil -> aucun tirage, sous-groupes sans bornes (ddl_non_etabli), mention au rapport", {
  val <- VAL; val$relation_table <- "ajustee_hors_outil"
  x <- construire_table(JEU, ROLES, plan_v(val = val, brut = FALSE), tables_reference = list(tv = T_HF))
  v <- x$etapes$validation
  g <- v$resultat$sous_groupes
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  identical(v$resultat$monte_carlo$tirages_effectues, 0L) && identical(v$parametres$parametres_estimes, NA_integer_) &&
    all(g$etat == "ddl_non_etabli") && all(is.na(g$p_centrale)) && !anyNA(g$smr) &&
    grepl("Relation « ajustee_hors_outil »", h, fixed = TRUE) &&
    !grepl("seules les p-valeurs de Monte-Carlo ont un niveau garanti", h, fixed = TRUE) &&
    !grepl("Aucune correction de multiplicité", h, fixed = TRUE) &&
    !grepl("p-valeurs de Monte-Carlo : p_MC = (1 + K) / (B + 1)", h, fixed = TRUE)
})

# États de cellule et décomptes (calculer_validation() en appel direct ; valeur
# calculée à la main) : âge 50 sans exposition ni décès ; 51 décès sans
# exposition ; 52 q~ = 0 sans décès ; 53 q~ = 0 avec 2 décès ; 54 à 56
# retenues ; 57 absent de l'exposition (grille) ; 60 hors plage.
EXPO <- data.frame(age = c(50L, 51L, 52L, 53L, 54L, 55L, 56L, 60L), exposition_jours = 0,
                   exposition_centrale = c(0, 0, 10, 10, 100, 120, 80, 50), evenements = c(0L, 1L, 0L, 2L, 1L, 3L, 0L, 4L),
                   sorties_concurrentes = 0L)
T_ET <- table_reference(data.frame(age = 40:70, sexe = "ensemble", qx = ifelse(40:70 %in% 52:53, 0, 0.01)),
                        "etats", "importee", "test")
validation_directe <- function(expo = EXPO, table = T_ET, plage = c(50, 57), segmentation = character(0), ...) {
  a <- list(exposition = expo, segmentation = segmentation, par_annee = FALSE, convention = "dernier_anniversaire",
            table = table, table_reference = "t", relation_table = "independante", plage_ages = plage,
            tranches_age = plage[1L], niveau = 0.95, replications = 19, graine = 5L, positionnement = NULL)
  m <- list(...)
  a[names(m)] <- m
  do.call(tablexp:::calculer_validation, a)
}
verifier("états : retenue, sans_exposition (âges 50 et 57), taux_table_nul, evenement_sans_exposition, evenement_sous_taux_nul ; avertissement evenements_sans_attendu ; hors plage", {
  r <- validation_directe()
  c_ <- r$resultat$cellules
  d <- r$diagnostics
  identical(c_$age, 50:57) &&
    identical(c_$etat, c("sans_exposition", "evenement_sans_exposition", "taux_table_nul", "evenement_sous_taux_nul",
                         "retenue", "retenue", "retenue", "sans_exposition")) &&
    identical(d$etats$cellules, c(3L, 2L, 1L, 1L, 1L)) && identical(d$evenements_sans_attendu, 3L) &&
    identical(d$evenements_hors_plage, 4L) && identical(d$exposition_hors_plage, 50) && identical(d$cellules_hors_plage, 1L) &&
    identical(r$avertissements$code, "evenements_sans_attendu") && identical(r$avertissements$nombre, 2L) &&
    identical(valeur(r$resultat$statistiques, "smr/observes"), 4) && all(is.na(c_$residu_pearson[c_$etat != "retenue"]))
})
# Type des dénombrements (#174) : cellules_retenues des sous-groupes en integer,
# comme celui du diagnostic, quel que soit le nombre de segments ou de cellules.
verifier("dénombrements (#174) : sous_groupes$cellules_retenues et diagnostics$cellules_retenues en integer, un seul segment (segmentation vide), table vide, par_annee sans cellule, relation ajustee_hors_outil, segmentation par sexe", {
  expo_a <- EXPO; expo_a$annee <- 2020L
  cas <- list(un_segment = validation_directe(), vide = validation_directe(expo = EXPO[0L, ]),
              ajustee = validation_directe(relation_table = "ajustee_hors_outil"),
              annee_vide = validation_directe(expo = expo_a[0L, ], par_annee = TRUE),
              par_sexe = X$etapes$validation)
  types <- vapply(cas, function(r) paste(typeof(r$resultat$sous_groupes$cellules_retenues),
                                         typeof(r$diagnostics$cellules_retenues)), "")
  g <- cas$un_segment$resultat$sous_groupes
  tous(types = if (all(types == "integer integer")) TRUE else paste(names(types), types, collapse = "; "),
       valeurs = if (identical(g$cellules_retenues, 3L) && identical(cas$un_segment$diagnostics$cellules_retenues, 3L) &&
                     identical(cas$vide$resultat$sous_groupes$cellules_retenues, 0L)) TRUE else "décomptes",
       sexe = {
         sg <- cas$par_sexe$resultat$sous_groupes
         som <- vapply(c("segment", "tranche"), function(r) sum(sg$cellules_retenues[sg$regroupement == r]), 0)
         if (identical(as.integer(som), rep(cas$par_sexe$diagnostics$cellules_retenues, 2L))) TRUE else "somme par regroupement"
       })
})
# Type des dénombrements (#199 ; ADR 0009, point 9.6) : les neuf chemins de
# l'étape validation en integer dans toutes les branches (cellules retenues,
# aucune cellule retenue avec sequences_runs vide, ajustee_hors_outil sans
# tirage, par_annee sans cellule, construction par sexe) ; valeurs écrites à
# la main sur un_segment.
types_validation_199 <- function(r) {
  t <- c(sous_groupes_observes = typeof(r$resultat$sous_groupes$observes),
         positifs = typeof(r$resultat$sequences_runs$positifs), negatifs = typeof(r$resultat$sequences_runs$negatifs),
         runs = typeof(r$resultat$sequences_runs$runs),
         tirages_effectues = typeof(r$resultat$monte_carlo$tirages_effectues),
         etats_evenements = typeof(r$diagnostics$etats$evenements),
         evenements_sans_attendu = typeof(r$diagnostics$evenements_sans_attendu),
         evenements_hors_plage = typeof(r$diagnostics$evenements_hors_plage),
         evenements_total = typeof(r$diagnostics$evenements_total))
  if (all(t == "integer")) TRUE else paste(names(t)[t != "integer"], t[t != "integer"], collapse = ", ")
}
verifier("dénombrements (#199) : sous_groupes$observes, sequences_runs (positifs, negatifs, runs), tirages_effectues, diagnostics (etats$evenements, evenements_sans_attendu, evenements_hors_plage, evenements_total) en integer, cinq branches ; valeurs inchangées", {
  expo_a <- EXPO; expo_a$annee <- 2020L
  un <- validation_directe()
  vide <- validation_directe(expo = EXPO[0L, ])
  cas <- list(un_segment = un, vide = vide, ajustee = validation_directe(relation_table = "ajustee_hors_outil"),
              annee_vide = validation_directe(expo = expo_a[0L, ], par_annee = TRUE), par_sexe = X$etapes$validation)
  d <- un$diagnostics
  do.call(tous, c(lapply(cas, types_validation_199), list(
    vide_seq = if (nrow(vide$resultat$sequences_runs) == 0L && identical(vide$resultat$monte_carlo$tirages_effectues, 0L))
                 TRUE else "aucune cellule retenue : sequences_runs vide et aucun tirage attendus",
    valeurs = if (identical(d$etats$evenements, c(4L, 0L, 0L, 1L, 2L)) && identical(d$evenements_total, 11L) &&
                  identical(un$resultat$sous_groupes$observes, 4L) &&
                  identical(un$resultat$monte_carlo$tirages_effectues, 19L)) TRUE else "valeurs")))
})
verifier("états : E^c = 0 et q~ = 1 -> A = 0, état sans_exposition, pas de refus ni de NaN", {
  t1 <- table_reference(data.frame(age = 40:70, sexe = "ensemble", qx = ifelse(40:70 == 50, 1, 0.01)), "u", "importee", "test")
  r <- validation_directe(table = t1)
  identical(r$resultat$cellules$attendus[1L], 0) && identical(r$resultat$cellules$etat[1L], "sans_exposition")
})

# --- G. Refus (valeur calculée à la main : module, motif, ordre) --------------------

P0 <- plan_v()
avec_val <- function(nom, v) { p <- P0; p$etapes$validation[nom] <- list(v); p }
for (cas in list(list("table_reference", 1, "cle_table_invalide"), list("table_reference", "", "cle_table_invalide"),
                 list("relation_table", "independant", "relation_table_invalide"),
                 list("relation_table", NA_character_, "relation_table_invalide"),
                 list("plage_ages", c(20L, 90L), "plage_ages_invalide"), list("plage_ages", c(90, 20), "plage_ages_invalide"),
                 list("plage_ages", c(-1, 20), "plage_ages_invalide"), list("plage_ages", 20, "plage_ages_invalide"),
                 list("plage_ages", c(20.5, 90), "plage_ages_invalide"),
                 list("tranches_age", c(20L, 50L), "tranches_age_invalides"),
                 list("tranches_age", c(30, 50), "tranches_age_invalides"),
                 list("tranches_age", c(20, 50, 50), "tranches_age_invalides"),
                 list("tranches_age", c(20, 91), "tranches_age_invalides"),
                 list("niveau", 1, "niveau_invalide"), list("niveau", c(a = 0.95), "niveau_invalide"),
                 list("replications", 999L, "replications_invalides"), list("replications", 0, "replications_invalides"),
                 list("replications", 9.5, "replications_invalides"), list("replications", c(9, 9), "replications_invalides")))
  verifier(sprintf("refus valider_plan : %s = %s -> %s (module validation)", cas[[1L]], paste(deparse(cas[[2L]]), collapse = ""),
                   cas[[3L]]),
           controle(refus_de(valider_plan(avec_val(cas[[1L]], cas[[2L]]))), "validation", cas[[3L]]))
verifier("refus : validation au plan sans graine -> graine_manquante (étape aléatoire, #61)",
         controle(refus_de(valider_plan(plan_v(graine = NULL))), "construire_table", "graine_manquante"))
verifier("refus : validation sans exposition -> etape_prealable_manquante",
         controle(refus_de(valider_plan({ p <- P0; p$etapes$exposition <- NULL; p$etapes$brut <- NULL; p })),
                  "construire_table", "etape_prealable_manquante"))
# Appels de construire_table() et preparer_donnees() pendant expr (trace(),
# même méthode que tests/unitaires/test_tables_reference.R).
ns <- asNamespace("tablexp")
compter_appels <- function(expr) {
  n <- new.env()
  n$construire_table <- 0L; n$preparer_donnees <- 0L
  for (f in c("construire_table", "preparer_donnees"))
    suppressMessages(trace(f, where = ns, print = FALSE,
                           tracer = bquote(assign(.(f), get(.(f), envir = .(n)) + 1L, envir = .(n)))))
  on.exit(for (f in c("construire_table", "preparer_donnees")) suppressMessages(untrace(f, where = ns)))
  r <- tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
  list(refus = r, n = c(construire_table = n$construire_table, preparer_donnees = n$preparer_donnees))
}
# Refus avant exécution : preparer_donnees() jamais appelée.
avant_execution <- function(expr_plan, tables, motif) {
  a <- compter_appels(construire_table(JEU, ROLES, expr_plan, tables_reference = tables))
  r <- controle(a$refus, "validation", motif)
  if (isTRUE(r) && a$n[["preparer_donnees"]]) "preparer_donnees() appelée" else r
}
verifier("refus avant exécution : convention age_atteint_annee -> convention_validation_non_admise (Q7 (a))",
         avant_execution(plan_v(convention = "age_atteint_annee"), list(tv = T_HF), "convention_validation_non_admise"))
verifier("refus avant exécution : table avec annee -> table_annee_non_admise", {
  ta <- table_reference(data.frame(age = rep(0:120, 2), sexe = "ensemble", qx = 0.01, annee = rep(2015:2016, each = 121)),
                        "prospective", "importee", "test")
  avant_execution(P0, list(tv = ta), "table_annee_non_admise")
})
verifier("refus avant exécution : table par sexe sans sexe dans la segmentation -> table_sexe_incompatible",
         avant_execution(plan_v(segmentation = character(0)), list(tv = T_HF), "table_sexe_incompatible"))
verifier("refus avant exécution : plage d'âges non couverte -> plage_ages_hors_table",
         avant_execution(P0, list(tv = table_reference(data.frame(age = 30:120, sexe = "ensemble", qx = 0.01), "court",
                                                       "importee", "test")), "plage_ages_hors_table"))
verifier("refus (appel direct, défense) : table mêlant ensemble et H -> table_sexes_ambigus", {
  t <- data.frame(age = c(20:90, 20:90), sexe = rep(c("H", "ensemble"), each = 71), qx = 0.01, stringsAsFactors = FALSE)
  controle(refus_de(tablexp:::controler_entrees_validation(t, "t", "dernier_anniversaire", "sexe", c(20, 90), "validation")),
           "validation", "table_sexes_ambigus")
})
verifier("refus en cours d'étape : table H seule face à H et F -> cellule_sans_valeur_table", {
  th <- table_reference(data.frame(age = 0:120, sexe = "H", qx = GM$qx), "H seul", "importee", "test")
  controle(refus_de(construire_table(JEU, ROLES, P0, tables_reference = list(tv = th))), "validation", "cellule_sans_valeur_table")
})
verifier("refus en cours d'étape : q~ = 1 avec E^c > 0 -> taux_table_egal_un",
         controle(refus_de(validation_directe(table = table_reference(data.frame(age = 40:70, sexe = "ensemble",
                                                                                 qx = ifelse(40:70 == 55, 1, 0.01)),
                                                                      "u", "importee", "test"))),
                  "validation", "taux_table_egal_un"))
verifier("refus : rôle de pondération (montant) -> role_non_pris_en_charge (table pondérée refusée, E2.8)", {
  j <- JEU; j$m <- 1
  controle(refus_de(construire_table(j, c(ROLES, montant = "m"), P0, tables_reference = list(tv = T_HF))),
           "donnees", "role_non_pris_en_charge")
})

# --- H. Bout en bout de #73 (M247) : construction, fiche, rejeu, rapport ------------

verifier("bout en bout : fiche/tables_reference porte la clé tv (importee, empreinte de table) ; rejeu identical() hors execution", {
  f <- lire_fiche(ecrire_fiche(X$fiche))
  r <- rejouer_fiche(JEU, f, list(tv = T_HF))
  identical(names(X$fiche$tables_reference), "tv") && identical(X$fiche$tables_reference$tv$statut, "importee") &&
    identical(X$fiche$tables_reference$tv$empreinte, tablexp:::empreinte_table(T_HF)) &&
    identical(sans_execution(r$construction), sans_execution(X)) && all(r$bilan$verdict[1:8] == "conforme")
})
verifier("bout en bout : rejeu sans la table -> table_reference_absente ; avec une table altérée d'un ulp -> empreinte_table_discordante", {
  f <- X$fiche
  t2 <- T_HF; t2$qx[5L] <- t2$qx[5L] * (1 + .Machine$double.eps)
  t2 <- table_reference(data.frame(age = t2$age, sexe = t2$sexe, qx = t2$qx), "GM vraie par sexe", "importee",
                        "loi vraie du jeu simulé (test)")
  tous(a = controle(refus_de(rejouer_fiche(JEU, f)), "construire_table", "table_reference_absente"),
       b = controle(refus_de(rejouer_fiche(JEU, f, list(tv = t2))), "construire_table", "empreinte_table_discordante"))
})
verifier("bout en bout : rejeu d'une fiche dont la plage dépasse la table -> plage_ages_hors_table, levé par l'unique appel de construire_table() avant toute étape (#82 : contrôle d'entrée fait une fois)", {
  f <- X$fiche
  f$plan$etapes$validation$plage_ages <- c(20, 130)
  a <- compter_appels(rejouer_fiche(JEU, f, list(tv = T_HF)))
  r <- controle(a$refus, "validation", "plage_ages_hors_table")
  if (isTRUE(r) && !identical(a$n, c(construire_table = 1L, preparer_donnees = 0L))) "appels" else r
})
# Refus du contrôle 1.3 et des contrôles d'entrée de la validation au rejeu
# (#82 ; avis (P) rafraîchi, critère 4) : même module, motif, message et
# details que construire_table() sur le plan, la provenance et les tables de
# la fiche (identité exacte).
for (cas in list(
  list("clé citée par le plan absente de fiche/tables_reference (1.3)", "construire_table", "table_reference_absente",
       function(f) { f$plan$etapes$validation$table_reference <- "autre"; f }),
  list("plage d'âges hors de la table (contrôle d'entrée de la validation)", "validation", "plage_ages_hors_table",
       function(f) { f$plan$etapes$validation$plage_ages <- c(20, 130); f }),
  list("segmentation sans sexe, table par sexe (contrôle d'entrée de la validation)", "validation",
       "table_sexe_incompatible", function(f) { f$plan$segmentation <- character(0); f }))) {
  f <- cas[[4L]](X$fiche)
  r <- refus_de(rejouer_fiche(JEU, f, list(tv = T_HF)))
  r_ct <- refus_de(construire_table(JEU, f$roles, f$plan, f$provenance, list(tv = T_HF)))
  verifier(sprintf("rejeu (#82), %s : %s/%s, refus identical() à celui de construire_table()", cas[[1L]], cas[[2L]],
                   cas[[3L]]),
           if (is.null(r) || is.null(r_ct)) "aucun refus" else
             if (!identical(unclass(r), unclass(r_ct))) "refus différent de celui de construire_table()" else
               controle(r, cas[[2L]], cas[[3L]]))
}
# Comptes d'exécution (#82 ; #164, point 2, M281 ; valeur calculée à la main,
# mesurée par trace() sur l'espace de noms installé) : construire_table()
# exécute chaque contrôle une fois ; rejouer_fiche() exécute la couche
# plan-rôles deux fois (contrôle 6 du rejeu, puis construire_table()), le
# contrôle 1.3 et les contrôles d'entrée de la validation une fois.
CONTROLES_82 <- c("valider_plan", "controler_roles", "deduire_recette", "resoudre_convention", "controler_provenance",
                  "controler_plan_roles", "controler_entrees", "controler_tables_reference",
                  "controler_entrees_validation", "construire_table")
compter_82 <- function(expr) {
  n <- new.env()
  for (f in CONTROLES_82) assign(f, 0L, envir = n)
  for (f in CONTROLES_82)
    suppressMessages(trace(f, where = ns, print = FALSE,
                           tracer = bquote(assign(.(f), get(.(f), envir = .(n)) + 1L, envir = .(n)))))
  on.exit(for (f in CONTROLES_82) suppressMessages(untrace(f, where = ns)))
  force(expr)
  vapply(CONTROLES_82, function(f) get(f, envir = n), 0L)
}
verifier("comptes (#82 ; #164, point 2) : construire_table() sur un plan à validation, chaque contrôle une fois", {
  # Par l'espace de noms : la copie de construire_table() de l'environnement
  # du package attaché n'est pas tracée par trace(where = ns).
  a <- compter_82(tablexp:::construire_table(JEU, ROLES, P0, tables_reference = list(tv = T_HF)))
  if (all(a == 1L)) TRUE else paste(names(a), a, sep = "=", collapse = " ")
})
verifier("comptes (#82 ; #164, point 2) : rejouer_fiche() sur un plan à validation : couche plan-rôles 2, controler_tables_reference() 1, controler_entrees_validation() 1, construire_table() 1", {
  f <- lire_fiche(ecrire_fiche(X$fiche))
  a <- compter_82(rejouer_fiche(JEU, f, list(tv = T_HF)))
  attendu <- c(valider_plan = 2L, controler_roles = 2L, deduire_recette = 2L, resoudre_convention = 2L,
               controler_provenance = 2L, controler_plan_roles = 2L, controler_entrees = 1L,
               controler_tables_reference = 1L, controler_entrees_validation = 1L, construire_table = 1L)
  if (identical(a, attendu)) TRUE else paste(names(a), a, sep = "=", collapse = " ")
})
verifier("rapport : section etape_validation avec ses mentions fixes (sans verdict, independante, H0, multiplicité, Monte-Carlo, référence) et ses tables", {
  h <- rawToChar(ecrire_rapport(X)); Encoding(h) <- "UTF-8"
  all(vapply(c("<section id=\"etape_validation\">", "Aucune table positionnée n'est produite",
               "Relation « independante »", "Hypothèse nulle : D_c ~ Poisson(A_c)", "Aucune correction de multiplicité",
               "p_MC = (1 + K) / (B + 1)", "Référence de méthode",
               "seules les p-valeurs de Monte-Carlo ont un niveau garanti",
               "<caption><code>etapes/validation/resultat/statistiques</code>",
               "<caption><code>etapes/validation/resultat/cellules</code>"),
             function(m) grepl(m, h, fixed = TRUE), NA)) &&
    !grepl("Unité d'observation contrat, sans identifiant de tête", h, fixed = TRUE)
})
verifier("rapport : plan sans validation -> « Étape « validation » : non demandée par le plan »", {
  x <- construire_table(JEU, ROLES, plan_v(val = NULL, graine = NULL))
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  grepl("<p>Étape « validation » : non demandée par le plan (chemin etapes/validation absent de l'objet).</p>", h, fixed = TRUE)
})
verifier("rapport : unité contrat (sans id_tete) -> mention d'anti-conservatisme", {
  x <- construire_table(JEU, ROLES[names(ROLES) != "id_tete"], plan_v(brut = FALSE), tables_reference = list(tv = T_HF))
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  grepl("Unité d'observation contrat, sans identifiant de tête", h, fixed = TRUE)
})
