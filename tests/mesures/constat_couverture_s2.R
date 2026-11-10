# -----------------------------------------------------------------------------
#  tests/mesures/constat_couverture_s2.R : constat de simulation hors CI de la
#  couverture des intervalles du taux brut sous deux inventaires (scénario S2
#  du constat des stocks successifs), âges 50 à 70 (issue #197 ;
#  spécification (0')(c) d'actuary-approfondi sur #197, acceptée par le
#  mainteneur le 10/11/2026, M382 : réemploi de la plage de graines 20301200,
#  R = 2 000 par défaut ; étape (4) de B17, PR #204).
#
#  Question (#197) : au passage consigné de #188 (arbre fbf47af), à 60 ans,
#  les intervalles asymptotiques couvrent le q_x vrai au-delà de 0,95 sous T
#  et M (et sous L, à un moindre degré). Hypothèses (spécification) : H0
#  fluctuation de Monte-Carlo ; H1 discrétisation aux petits effectifs
#  (couverture égale, pour chaque lecture, à la couverture exacte pi sous D ~
#  Poisson aux expositions réalisées) ; H2 biais de cellule de M (E_M > E_L,
#  D_M = D_L) combiné au manque asymétrique de l'intervalle asymptotique ;
#  H3 troncature sous T (effectifs moindres) ; H4 dispersion modifiée par
#  l'imputation (phi) ; H5 artefact propre à 60 ans ; H6 multiplicité.
#
#  S2 recopié à l'identique de tests/mesures/constat_stocks_successifs.R :
#  loi vraie (décès Gompertz-Makeham A = 2e-4, B = 2,5e-5, c = 1,1, facteurs
#  H 1, F 0,6 sur q_x ; rachat constant 5 %), portefeuille `uniforme` (âge à
#  l'entrée 20 à 75, origine 2005-01-01, n = 2e4 têtes, moitié d'hommes),
#  période 2015-2024, grille {2014-12-31, 2024-12-31}, plan (délai 0,
#  segmentation par sexe, convention `dernier_anniversaire`, par_annee FALSE,
#  estimateurs poisson, binomial, kaplan_meier, niveau 0,95). Lectures : L
#  (forme en lignes, exposition exacte), T (stocks, `premier_inventaire`), M
#  (stocks, `milieu`) ; la forme stocks est celle du simulateur, même graine
#  que L. Intervalles : Poisson exact, Poisson asymptotique (normal sur
#  log mu), Kaplan-Meier asymptotique, sur la probabilité (couverture du q_x
#  vrai, comme constat_stocks_successifs.R).
#
#  Grandeurs, par jeu, lecture, sexe et âge x (50 à 70) : D, E (exposition
#  centrale de la lecture), indicatrices de couverture des trois intervalles
#  (NA si l'intervalle n'est pas produit), pi_r des deux intervalles de
#  Poisson : probabilité exacte de couverture sous D ~ Poisson(lambda),
#  lambda = mu_x E_L (L et M ; M : D_M = D_L, intervalle calculé sur E_M) ou
#  mu_x E_T (T), aux expositions réalisées du jeu, l'intervalle étant calculé
#  par les formules du moteur (R/brut.R, taux_poisson() ; somme tronquée où
#  P(D > d_max) < 1e-15) ; pour l'intervalle asymptotique, non produit à
#  D = 0, pi_r est conditionnel à D >= 1. b_x = E_M / E_L. Kaplan-Meier :
#  pas de pi_r (McNemar et comparaison à 0,95 seulement).
#
#  Tests (spécification, (1) à (7)) :
#   (1) taux observé contre 0,95 : binomial exact (binom.test), jeux où
#       l'intervalle est produit ;
#   (2) taux observé contre pi : loi exacte du nombre de couvertures,
#       Poisson-binomiale de paramètres pi_r (jeux où l'intervalle est
#       produit), p bilatérale par la méthode de binom.test (somme des
#       probabilités <= celle de l'observé, tolérance relative 1e-7) ;
#   (3) McNemar exact L/M et L/T : binomial sur les paires discordantes
#       (jeux où les deux intervalles sont produits) ; différence des taux
#       face à pi(M) - pi(L) (resp. T), z apparié sur les contributions
#       centrées c_r = 1{produit} (I - pi), d'espérance nulle sous
#       l'approximation (calcul sur tous les jeux, sans conditionnement
#       croisé) ;
#   (4) Holm (p.adjust) par famille sexe x lecture x intervalle de 21 âges,
#       pour (1) et (2) ;
#   (5) statistique groupée S_r = somme sur x de c_r(x), par famille :
#       moyenne, e.t. sd(S_r) / sqrt(R), z ;
#   (6) phi_x = Var(D - mu E) / E[mu E] par lecture (E de la lecture),
#       e.t. par la méthode delta (moments empiriques) ;
#   (7) biais de cellule somme D / somme mu E - 1, e.t. de Monte-Carlo
#       sd(D - (1 + b) mu E) / (sqrt(R) moyenne(mu E)).
#  Verdict par une règle fixée à l'avance (spécification, critère 2), sur
#  les 12 familles de Poisson (2 sexes x 3 lectures x 2 intervalles) :
#   « discrétisation (H1 + H2, H3) établie » si (a) aucune ligne rejetée
#     après Holm contre pi, (b) aucune statistique groupée significative à
#     5 % (|z| > 1,96), (c) écart M - L à 60 ans concordant avec
#     pi(M) - pi(L) (|z| <= 2, deux sexes, deux intervalles) ;
#   « fluctuation établie » sinon, si aucune ligne n'est rejetée après Holm
#     contre 0,95 et que (a) ou (b) échoue ;
#   « non expliqué » sinon, avec phi_x, b_x et résidus imprimés (ne fait pas
#     échouer le script).
#  Cette règle est le verdict de référence, imprimée en premier avec son
#  défaut de calibrage (aucun risque global contrôlé), telle quelle
#  (décision du mainteneur, M383, point (i)) : le critère (c) porte sur les
#  quatre z. Le nombre de paires discordantes n_x + n_l de chaque z est
#  imprimé, avec un avertissement quand R < 1000 ou n_x + n_l < 10 (z peu
#  informatif ; constat mineur C1 de l'audit), sans changer le verdict de
#  référence ; seule la lecture calibrée écarte ces z. Kaplan-Meier, sans
#  pi, n'entre pas dans le verdict.
#  Lecture calibrée (ajoutée le 10/11/2026 après un pilote à R = 1000 sur les
#  jeux 1 à 1000 ; décision du mainteneur, M383) : risque global de 5 %
#  partagé en trois parts de 5 %/3 (Bonferroni entre les parts) : (a) Holm
#  sur les 252 lignes de Poisson contre pi ; (b) Holm sur les 12
#  statistiques groupées (p normale bilatérale) ; (c) Holm sur les z appariés
#  M - L à 60 ans applicables (p normale bilatérale). Libellé « compatible
#  avec la discrétisation (aucun écart détecté, risque global de 5 %) »,
#  jamais « établie », sinon « écart détecté » avec les lignes en cause ;
#  taille d'effet S/21 par famille (intervalle ± 1,96 e.t. MC) et écart
#  minimal détectable par âge (approché : normal, puissance 80 %, seuil de
#  la première étape de Holm de (a)). Calculée sur les jeux 1 à R et sur les
#  jeux 1001 à R seuls (partie confirmatoire, non vus au choix de la règle ;
#  « non applicable » si R <= 1000).
#  Descriptif groupé (M383, hors verdict, déclaré avant le passage consigné) :
#  phi groupé par sexe x lecture = somme_x Var(U_x) / somme_x moyenne(A_x),
#  e.t. par la méthode delta sur Q_r = somme_x (U_x,r - Ubar_x)^2 et
#  B_r = somme_x A_x,r ; biais groupé pondéré de L et son e.t. de
#  Monte-Carlo. phi_L = 1 est exact sous le simulateur (force constante par
#  âge entier, R/simulation.R:59) : un écart significatif appellerait une
#  issue.
#  Statuts : couvertures, McNemar, phi, biais de cellule : constats de
#  simulation (erreur de Monte-Carlo, fonction de R) ; pi : calcul exact sous
#  approximation (D ~ Poisson conditionnellement aux expositions réalisées),
#  validée par la lecture L ; erreur d'estimation de chaque jeu (1 / sqrt(D)
#  par cellule) résumée à part.
#
#  Identités exactes (verifier(), un échec fait échouer le script) :
#   - D_M = D_L dans chaque cellule de la table, à chaque jeu ;
#   - indicatrices des intervalles de Poisson recalculées par les formules
#     du moteur au D observé égales à celles du moteur (même motif de NA) :
#     elles fondent le calcul de pi ;
#   - rattachement à #197 (si R >= 1000) : sur les jeux r <= 1000, les neuf
#     couvertures à 60 ans de l'issue (H Poisson asymptotique, F Poisson
#     asymptotique, F Kaplan-Meier ; L, T, M) reproduites à l'arrondi
#     imprimé dans l'issue (trois décimales).
#
#  Graines : jeu r sous GRAINE_JEU + r = 20301200 + r, r < 10 000 ; jeux 1 à
#  1000 = ceux du passage consigné de #188 (S1 et S2) ; réemploi A1 de la
#  plage 20301200 déclarée dans tests/controles/graines_admises.csv. Aucune
#  autre source d'aléa.
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/constat_couverture_s2.R [jeux=2000]
#  Clé : jeux (R, défaut 2000 ; e.t. de Monte-Carlo 0,0049 à 0,95, 0,0038 à
#  0,97). Durée : extrapolée après le premier jeu, durée réelle imprimée ;
#  mesurée au pilote (un cœur, Xeon 2,1 GHz) : 1,02 s par jeu à R = 1000
#  (17,0 min, analyse comprise), soit environ 35 min aux défauts.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
# Locale requise : les libellés du script et du package sont en UTF-8.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  verifier("locale UTF-8 requise (l10n_info()[[\"UTF-8\"]])", FALSE)
  bilan()
}
installer_tablexp(RACINE)

# --- paramètres ----------------------------------------------------------------
ARGS <- commandArgs(trailingOnly = TRUE)
argument <- function(cle, defaut) {
  a <- grep(paste0("^", cle, "="), ARGS, value = TRUE)
  if (length(a)) sub("^[^=]*=", "", a[length(a)]) else defaut
}
R_JEUX <- as.integer(argument("jeux", "2000"))
stopifnot(R_JEUX >= 2L, R_JEUX < 10000L)  # au-delà, la plage rejoindrait la suivante
GRAINE_JEU <- 20301200L                   # jeu r sous GRAINE_JEU + r (S1 et S2 de #188)
N_TETES <- 2e4
NIVEAU <- 0.95
ALPHA_TEST <- 0.05
AGES <- 50:70
SEXES <- c("H", "F")
LECTURES <- c("L", "T", "M")
INTERVALLES <- c("poisson_exact", "poisson_asymptotique", "kaplan_meier")
POISSON <- INTERVALLES[1:2]
R_RATTACHEMENT <- 1000L
# Couvertures à 60 ans de #197 (passage consigné de #188, arbre fbf47af),
# à trois décimales.
RATTACHEMENT <- data.frame(
  sexe = c(rep("H", 3L), rep("F", 6L)),
  intervalle = c(rep("poisson_asymptotique", 6L), rep("kaplan_meier", 3L)),
  lecture = rep(LECTURES, 3L),
  valeur = c(0.957, 0.974, 0.970, 0.965, 0.971, 0.977, 0.965, 0.973, 0.978),
  stringsAsFactors = FALSE)

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
G_DEUX <- D(c("2014-12-31", "2024-12-31"))
gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
FACTEURS <- c(H = 1, F = 0.6)
LOI <- list(deces = list(table = gm, facteurs = FACTEURS), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
# Loi vraie par sexe, indexée par âge + 1 : q_s = min(f_s q, 1), mu = -ln(1 - q).
Q_VRAI <- lapply(FACTEURS, function(f) pmin(f * gm$qx[order(gm$age)], 1))
MU_VRAI <- lapply(Q_VRAI, function(q) -log1p(-q))
PF <- list(mode = "uniforme", n = N_TETES, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
ROLES_L <- c(id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance", date_entree = "date_entree",
             date_sortie = "date_sortie", cause_sortie = "cause_sortie")
ROLES_S <- c(id_tete = "id_tete", type_ligne = "type_ligne", date_inventaire = "date_inventaire", sexe = "sexe",
             date_naissance = "date_naissance", date_sortie = "date_sortie", cause_sortie = "cause_sortie")

# Plan (celui de constat_stocks_successifs.R) : `mode` NULL pour L, sinon le
# mode d'entrée des stocks et la grille.
plan_lecture <- function(mode = NULL, grille = NULL) {
  ex <- list(convention = "dernier_anniversaire", par_annee = FALSE)
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER, delai_declaration = 0)
  if (!is.null(mode)) { p$inventaires <- grille; ex$entree_entre_inventaires <- mode }
  c(p, list(segmentation = "sexe",
            etapes = list(donnees = list(exclusions = character(0)), exposition = ex,
                          brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = NIVEAU))))
}

# --- outils ------------------------------------------------------------------------
AVERTIS <- new.env(); AVERTIS$n <- list(); AVERTIS$codes <- list()
noter <- function(w) {
  m <- substr(conditionMessage(w), 1L, 90L)
  AVERTIS$n[[m]] <- c(AVERTIS$n[[m]], 0L)[1L] + 1L
  invokeRestart("muffleWarning")
}
construire <- function(donnees, roles, plan, lecture) {
  x <- withCallingHandlers(construire_table(donnees, roles, plan), warning = noter)
  for (cd in unique(x$avertissements$code)) {
    nm <- paste(lecture, cd, sep = " : ")
    AVERTIS$codes[[nm]] <- c(AVERTIS$codes[[nm]], 0L)[1L] + 1L
  }
  x
}
simuler <- function(r, grille = NULL)
  withCallingHandlers(simuler_portefeuille(LOI, PF, PER, GRAINE_JEU + r,
                                           forme = if (!is.null(grille)) list(nom = "stocks_successifs", inventaires = grille)),
                      warning = noter)

# Indicatrices de couverture du q vrai par les intervalles de Poisson exact et
# asymptotique, pour des décès d (vecteur) sur l'exposition Ei, par les
# formules de taux_poisson() (R/brut.R) et la conversion -expm1(-mu) du
# moteur ; NA où l'intervalle n'est pas produit (Ei <= 0 ; d = 0 pour
# l'asymptotique).
Z_INT <- stats::qnorm((1 - NIVEAU) / 2, lower.tail = FALSE)
couvre_d <- function(d, Ei, q) {
  n <- length(d); alpha <- 1 - NIVEAU
  pe <- pa <- rep(NA, n)
  if (!(Ei > 0)) return(list(pe = pe, pa = pa))
  k <- d > 0
  inf_e <- rep(0, n); inf_e[k] <- stats::qgamma(alpha / 2, d[k]) / Ei
  sup_e <- stats::qgamma(alpha / 2, d + 1, lower.tail = FALSE) / Ei
  pe <- -expm1(-inf_e) <= q & q <= -expm1(-sup_e)
  taux <- d[k] / Ei
  pa[k] <- -expm1(-(taux * exp(-Z_INT / sqrt(d[k])))) <= q & q <= -expm1(-(taux * exp(Z_INT / sqrt(d[k]))))
  list(pe = pe, pa = pa)
}
# pi exact (D ~ Poisson(lambda), intervalle sur Ei) ; asymptotique
# conditionnel à D >= 1. NA si lambda = 0 ou Ei <= 0.
pi_cellule <- function(lambda, Ei, q) {
  if (!(lambda > 0) || !(Ei > 0)) return(c(pe = NA_real_, pa = NA_real_))
  d <- 0:(stats::qpois(1e-15, lambda, lower.tail = FALSE) + 5)
  p <- stats::dpois(d, lambda); cv <- couvre_d(d, Ei, q)
  c(pe = sum(p[cv$pe]), pa = sum(p[-1L][cv$pa[-1L]]) / -expm1(-lambda))
}

# Cellules (sexe, âge) d'une lecture, ordre SEXES x AGES : D, E, indicatrices.
couvre <- function(inf, sup, q) ifelse(is.na(inf) | is.na(sup), NA, inf <= q & q <= sup)
lire <- function(x) {
  ex <- x$etapes$exposition$resultat$exposition
  po <- x$etapes$brut$resultat$poisson; km <- x$etapes$brut$resultat$kaplan_meier
  cle <- paste(rep(SEXES, each = length(AGES)), AGES)
  q <- unlist(lapply(SEXES, function(sx) Q_VRAI[[sx]][AGES + 1L]))
  ie <- match(cle, paste(ex$sexe, ex$age)); ip <- match(cle, paste(po$sexe, po$age)); ik <- match(cle, paste(km$sexe, km$age))
  Dc <- ex$evenements[ie]; Dc[is.na(Dc)] <- 0L
  Ec <- ex$exposition_centrale[ie]; Ec[is.na(Ec)] <- 0
  list(D = as.numeric(Dc), E = Ec, q = q,
       poisson_exact = couvre(po$probabilite_borne_inf_exacte[ip], po$probabilite_borne_sup_exacte[ip], q),
       poisson_asymptotique = couvre(po$probabilite_borne_inf_asymptotique[ip], po$probabilite_borne_sup_asymptotique[ip], q),
       kaplan_meier = couvre(km$borne_inf_asymptotique[ik], km$borne_sup_asymptotique[ik], q))
}

# --- un jeu ------------------------------------------------------------------------
# Rend un vecteur nommé : "lecture|sexe|âge|grandeur" (D, E, I_<intervalle>,
# pi_<intervalle de Poisson>), plus les identités id|DM et id|indicatrices.
un_jeu <- function(r) {
  j <- simuler(r)
  xl <- construire(j, ROLES_L, plan_lecture(), "L")
  st <- simuler(r, G_DEUX)
  xt <- construire(st, ROLES_S, plan_lecture("premier_inventaire", G_DEUX), "T")
  xm <- construire(st, ROLES_S, plan_lecture("milieu", G_DEUX), "M")
  # D_M = D_L dans chaque cellule de la table (cellules à décès appariées).
  el <- xl$etapes$exposition$resultat$exposition; em <- xm$etapes$exposition$resultat$exposition
  dl <- el[el$evenements > 0L, ]; dm <- em[em$evenements > 0L, ]
  cl <- paste(dl$sexe, dl$age); cm <- paste(dm$sexe, dm$age)
  dm_ok <- identical(sort(cl), sort(cm)) && identical(dm$evenements[match(cl, cm)], dl$evenements)
  ce <- list(L = lire(xl), T = lire(xt), M = lire(xm))
  mu <- unlist(lapply(SEXES, function(sx) MU_VRAI[[sx]][AGES + 1L]))
  noms <- paste(rep(SEXES, each = length(AGES)), AGES, sep = "|")
  o <- numeric(0); ind_ok <- TRUE
  for (lc in LECTURES) {
    c0 <- ce[[lc]]
    lam <- mu * (if (lc == "T") ce$T$E else ce$L$E)
    pis <- vapply(seq_along(lam), function(i) pi_cellule(lam[i], c0$E[i], c0$q[i]), c(pe = 0, pa = 0))
    for (i in seq_along(lam)) {
      cv <- couvre_d(c0$D[i], c0$E[i], c0$q[i])
      ind_ok <- ind_ok && identical(cv$pe, c0$poisson_exact[i]) && identical(cv$pa, c0$poisson_asymptotique[i])
    }
    v <- c(D = c0$D, E = c0$E, I_pe = c0$poisson_exact, I_pa = c0$poisson_asymptotique, I_km = c0$kaplan_meier,
           pi_pe = pis["pe", ], pi_pa = pis["pa", ])
    names(v) <- paste(lc, noms, rep(c("D", "E", "I_poisson_exact", "I_poisson_asymptotique", "I_kaplan_meier",
                                      "pi_poisson_exact", "pi_poisson_asymptotique"), each = length(noms)), sep = "|")
    o <- c(o, v)
  }
  c(o, `id|DM` = dm_ok, `id|indicatrices` = ind_ok)
}

# --- tests -------------------------------------------------------------------------
# Loi exacte du nombre de succès de Bernoulli indépendantes de paramètres p.
poisson_binomiale <- function(p) {
  f <- 1
  for (pr in p) f <- c(f * (1 - pr), 0) + c(0, f * pr)
  f
}
# p bilatérale de k sous la loi discrète f (indices 0..), méthode de
# binom.test : somme des probabilités <= f(k) (1 + 1e-7).
p_bilaterale <- function(f, k) min(1, sum(f[f <= f[k + 1L] * (1 + 1e-7)]))
# phi = Var(U) / moyenne(A), U = D - A ; e.t. par la méthode delta.
phi_et <- function(U, A) {
  R <- length(U); V <- stats::var(U); m <- mean(A)
  u <- U - mean(U); a <- A - m
  vV <- (mean(u^4) - V^2) / R; vm <- stats::var(A) / R; cvm <- mean(u^2 * a) / R
  c(phi = V / m, et = sqrt(max(0, vV / m^2 + V^2 * vm / m^4 - 2 * V * cvm / m^3)))
}
imprimer <- function(df) { o <- options(width = 250); on.exit(options(o)); print(df, row.names = FALSE, right = FALSE) }

# --- exécution -----------------------------------------------------------------------
proc <- if (file.exists("/proc/cpuinfo")) sub("^.*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1L]) else NA
nproc <- suppressWarnings(tryCatch(system2("nproc", stdout = TRUE), error = function(e) NA_character_))
arbre <- suppressWarnings(tryCatch(system2("git", c("-C", RACINE, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
                                   error = function(e) NA_character_))
modifs <- suppressWarnings(tryCatch(system2("git", c("-C", RACINE, "status", "--porcelain"), stdout = TRUE, stderr = FALSE),
                                    error = function(e) NA_character_))
cat(sprintf("Plateforme : %s ; %s ; processeur %s ; cœurs disponibles (nproc) %s, détectés (detectCores) %d ; BLAS %s ; LAPACK %s ; calcul séquentiel\n",
            R.version.string, R.version$platform, proc, nproc[1L], parallel::detectCores(), extSoftVersion()[["BLAS"]],
            La_library()))
cat(sprintf("Arbre : %s ; fichiers modifiés ou non suivis (git status --porcelain) : %d\n",
            if (length(arbre)) arbre[1L] else NA, length(modifs)))
cat(sprintf("Paramètres : R = %d jeux ; n = %.0f têtes ; grille %s ; niveau %.2f ; âges %d à %d ; tests à %.2f\n",
            R_JEUX, N_TETES, paste(G_DEUX, collapse = ", "), NIVEAU, min(AGES), max(AGES), ALPHA_TEST))
cat(sprintf("Graines : jeu r sous %d + r, de %d à %d (jeux 1 à 1000 : ceux du passage consigné de #188)\n",
            GRAINE_JEU, GRAINE_JEU + 1L, GRAINE_JEU + R_JEUX))
cat(sprintf("E.t. de Monte-Carlo nominale d'une couverture : %.4f à 0,95, %.4f à 0,97\n",
            sqrt(0.95 * 0.05 / R_JEUX), sqrt(0.97 * 0.03 / R_JEUX)))
t0 <- proc.time()[["elapsed"]]
jeux <- vector("list", R_JEUX); erreurs <- character(0); refus <- character(0)
for (r in seq_len(R_JEUX)) {
  jeux[[r]] <- tryCatch(un_jeu(r),
                        refus_tablexp = function(e) {
                          refus <<- c(refus, sprintf("jeu %d : %s (%s)", r, conditionMessage(e), e$motif)); NULL },
                        error = function(e) { erreurs <<- c(erreurs, sprintf("jeu %d : %s", r, conditionMessage(e))); NULL })
  if (r == 1L) cat(sprintf("Premier jeu : %.2f s ; durée extrapolée pour %d jeux : %.1f min\n",
                           proc.time()[["elapsed"]] - t0, R_JEUX, (proc.time()[["elapsed"]] - t0) * R_JEUX / 60))
}
duree <- proc.time()[["elapsed"]] - t0
ok <- !vapply(jeux, is.null, NA)
cat(sprintf("Durée des jeux : %.1f min (%.2f s par jeu) ; jeux refusés par le moteur : %d ; jeux en erreur : %d\n",
            duree / 60, duree / R_JEUX, length(refus), length(erreurs)))
if (length(refus)) cat(paste("  refus :", utils::head(refus, 5L)), sep = "\n")
if (length(erreurs)) cat(paste("  erreur :", utils::head(erreurs, 5L)), sep = "\n")
verifier("aucun jeu en erreur", length(erreurs) == 0L)
verifier("aucun jeu refusé (rattachement et appariement des jeux)", length(refus) == 0L)
if (sum(ok) < 2L) bilan()
noms <- names(jeux[[which(ok)[1L]]])
verifier("mêmes grandeurs (noms, ordre) dans chaque jeu", all(vapply(jeux[ok], function(x) identical(names(x), noms), NA)))
m <- do.call(rbind, jeux[ok])
R <- nrow(m)
col <- function(lc, sx, a, g) m[, paste(lc, sx, a, g, sep = "|")]

cat("\n=== Identités exactes (jeux où elle tient / jeux) ===\n")
for (nm in c("id|DM", "id|indicatrices")) {
  v <- as.logical(m[, nm])
  cat(sprintf("  %-16s %d / %d\n", sub("^id\\|", "", nm), sum(v), R))
}
verifier("D_M = D_L dans chaque cellule, à chaque jeu", all(as.logical(m[, "id|DM"])))
verifier("indicatrices de Poisson recalculées (formules du moteur) égales à celles du moteur, à chaque jeu",
         all(as.logical(m[, "id|indicatrices"])))

taux_prod <- function(I) { p <- !is.na(I); c(n = sum(p), k = sum(I[p])) }
cat(sprintf("\n=== Rattachement à #197 (couvertures à 60 ans, jeux r <= %d, trois décimales) ===\n", R_RATTACHEMENT))
r_ratt <- which(ok)[which(ok) <= R_RATTACHEMENT]
ratt <- RATTACHEMENT
ratt$mesure <- vapply(seq_len(nrow(ratt)), function(i) {
  I <- m[seq_along(r_ratt), paste(ratt$lecture[i], ratt$sexe[i], 60, paste0("I_", ratt$intervalle[i]), sep = "|")]
  t <- taux_prod(I); t[["k"]] / t[["n"]] }, 0)
ratt$arrondi <- round(ratt$mesure, 3)
# Contrôle fait seulement si les jeux 1 à 1000 sont tous disponibles ;
# sinon la colonne egal est NA (constat mineur C2 de l'audit).
ratt_fait <- length(r_ratt) == R_RATTACHEMENT && all(ok[seq_len(R_RATTACHEMENT)])
ratt$egal <- if (ratt_fait) ratt$arrondi == ratt$valeur else NA
imprimer(transform(ratt, mesure = round(mesure, 5)))
if (ratt_fait) {
  for (i in seq_len(nrow(ratt)))
    verifier(sprintf("rattachement #197 : %s %s %s à 60 ans = %.3f", ratt$lecture[i], ratt$sexe[i], ratt$intervalle[i],
                     ratt$valeur[i]), ratt$egal[i])
} else cat(sprintf("  non vérifié : %d jeux disponibles parmi r <= %d (R = %d)\n", length(r_ratt), R_RATTACHEMENT, R_JEUX))

# --- lignes : (1), (2), (4) ------------------------------------------------------------
lignes <- list(); S_fam <- list(); pi_manquant <- character(0)
for (lc in LECTURES) for (iv in INTERVALLES) for (sx in SEXES) {
  fam <- paste(sx, lc, iv)
  Sr <- numeric(R)
  for (a in AGES) {
    I <- col(lc, sx, a, paste0("I_", iv)); p <- !is.na(I); n <- sum(p); k <- sum(I[p])
    p1 <- if (n) stats::binom.test(k, n, NIVEAU)$p.value else NA_real_
    pibar <- p2 <- zres <- NA_real_
    if (iv %in% POISSON) {
      pis <- col(lc, sx, a, paste0("pi_", iv))
      # pi défini partout où l'intervalle est produit (l'asymptotique l'est
      # aussi aux jeux où D = 0 : conditionnel à D >= 1).
      if (anyNA(pis[p])) pi_manquant <- c(pi_manquant, sprintf("%s âge %d", fam, a))
      pp <- pis[p]
      if (n) {
        pibar <- mean(pp); p2 <- p_bilaterale(poisson_binomiale(pp), k)
        zres <- (k - sum(pp)) / sqrt(sum(pp * (1 - pp)))
        Sr[p] <- Sr[p] + (I[p] - pp)
      }
    }
    lignes[[length(lignes) + 1L]] <- data.frame(famille = fam, lecture = lc, intervalle = iv, sexe = sx, age = a, R = n,
                                                non_produits = R - n, observe = k / n, pi_bar = pibar,
                                                p_095 = p1, p_pi = p2, z_residu = zres, stringsAsFactors = FALSE)
  }
  if (iv %in% POISSON) S_fam[[fam]] <- Sr
}
tab <- do.call(rbind, lignes)
verifier("pi défini dans chaque jeu où l'intervalle de Poisson est produit",
         if (length(pi_manquant)) paste("pi manquant :", paste(utils::head(pi_manquant, 5L), collapse = " ; ")) else TRUE)
tab$p_095_holm <- ave(tab$p_095, tab$famille, FUN = function(p) stats::p.adjust(p, "holm"))
tab$p_pi_holm <- ave(tab$p_pi, tab$famille, FUN = function(p) if (all(is.na(p))) p else stats::p.adjust(p, "holm"))
cat(sprintf(paste("\n=== Couverture du q_x vrai, âges %d à %d (constats de simulation ; R = jeux où l'intervalle est produit ;",
                  "pi_bar : moyenne des pi_r, calcul exact sous approximation Poisson ; p_095 : binomial exact contre 0,95 ;",
                  "p_pi : Poisson-binomiale exacte contre les pi_r ; Holm par famille sexe x lecture x intervalle de %d âges ;",
                  "z_residu = (k - somme pi) / sqrt(somme pi (1 - pi))) ===\n"), min(AGES), max(AGES), length(AGES)))
fmt <- tab
for (nm in c("observe", "pi_bar")) fmt[[nm]] <- round(fmt[[nm]], 4)
for (nm in c("p_095", "p_pi", "p_095_holm", "p_pi_holm")) fmt[[nm]] <- signif(fmt[[nm]], 3)
fmt$z_residu <- round(fmt$z_residu, 2)
imprimer(fmt[, c("lecture", "intervalle", "sexe", "age", "R", "non_produits", "observe", "pi_bar", "z_residu",
                 "p_pi", "p_pi_holm", "p_095", "p_095_holm")])

# --- (5) statistique groupée ------------------------------------------------------------
cat("\n=== Statistique groupée S_r = somme sur les âges de 1{produit} (I - pi), par famille (constat de simulation) ===\n")
grp <- do.call(rbind, lapply(names(S_fam), function(f) {
  s <- S_fam[[f]]; z <- mean(s) / (stats::sd(s) / sqrt(R))
  data.frame(famille = f, moyenne = signif(mean(s), 4), et_MC = signif(stats::sd(s) / sqrt(R), 3), z = round(z, 2),
             significative_5pc = abs(z) > stats::qnorm(1 - ALPHA_TEST / 2), stringsAsFactors = FALSE)
}))
imprimer(grp)

# --- (3) McNemar et écarts appariés -------------------------------------------------------
cat("\n=== McNemar exact (paires discordantes, jeux où les deux intervalles sont produits) ; écart des taux face à pi(X) - pi(L), z apparié sur les contributions centrées (tous les jeux) ===\n")
mcn <- list()
for (x in c("M", "T")) for (iv in INTERVALLES) for (sx in SEXES) for (a in AGES) {
  IL <- col("L", sx, a, paste0("I_", iv)); IX <- col(x, sx, a, paste0("I_", iv))
  p <- !is.na(IL) & !is.na(IX)
  n_x <- sum(IX[p] & !IL[p]); n_l <- sum(IL[p] & !IX[p])
  pm <- if (n_x + n_l) stats::binom.test(n_x, n_x + n_l, 0.5)$p.value else 1
  ecart <- (n_x - n_l) / sum(p)
  dpi <- z <- NA_real_
  if (iv %in% POISSON) {
    pL <- col("L", sx, a, paste0("pi_", iv)); pX <- col(x, sx, a, paste0("pi_", iv))
    dpi <- mean(pX[p] - pL[p])
    cL <- ifelse(is.na(IL), 0, IL - pL); cX <- ifelse(is.na(IX), 0, IX - pX)
    dd <- cX - cL
    z <- if (stats::sd(dd) > 0) mean(dd) / (stats::sd(dd) / sqrt(R)) else NA_real_
  }
  mcn[[length(mcn) + 1L]] <- data.frame(paire = paste0("L/", x), intervalle = iv, sexe = sx, age = a, paires = sum(p),
                                        X_seul_couvre = n_x, L_seul_couvre = n_l, p_mcnemar = pm,
                                        ecart_X_moins_L = ecart, pi_X_moins_pi_L = dpi, z_apparie = z,
                                        stringsAsFactors = FALSE)
}
mcn <- do.call(rbind, mcn)
mcn$p_mcnemar_holm <- ave(mcn$p_mcnemar, paste(mcn$paire, mcn$intervalle, mcn$sexe), FUN = function(p) stats::p.adjust(p, "holm"))
fm <- mcn
fm$p_mcnemar <- signif(fm$p_mcnemar, 3); fm$p_mcnemar_holm <- signif(fm$p_mcnemar_holm, 3)
fm$ecart_X_moins_L <- round(fm$ecart_X_moins_L, 4); fm$pi_X_moins_pi_L <- round(fm$pi_X_moins_pi_L, 4)
fm$z_apparie <- round(fm$z_apparie, 2)
imprimer(fm)

# --- (6), (7), b_x et erreur d'estimation --------------------------------------------------
cat("\n=== Par cellule (moyennes sur les jeux) : b_x = E_M / E_L, E_T / E_L ; phi_x = Var(D - mu E) / E[mu E] (E de la lecture), e.t. delta ; biais de cellule somme D / somme mu E - 1, e.t. MC (constats de simulation) ===\n")
cel <- list()
for (sx in SEXES) for (a in AGES) {
  mu <- MU_VRAI[[sx]][a + 1L]
  EL <- col("L", sx, a, "E"); ET <- col("T", sx, a, "E"); EM <- col("M", sx, a, "E")
  ligne <- data.frame(sexe = sx, age = a, D_L = round(mean(col("L", sx, a, "D")), 2),
                      lambda_L = round(mean(mu * EL), 2), lambda_T = round(mean(mu * ET), 2),
                      b_x = round(mean(EM / EL), 4), b_x_et = signif(stats::sd(EM / EL), 3), ET_sur_EL = round(mean(ET / EL), 4),
                      stringsAsFactors = FALSE)
  for (lc in LECTURES) {
    Dc <- col(lc, sx, a, "D"); A <- mu * col(lc, sx, a, "E")
    ph <- phi_et(Dc - A, A)
    b <- sum(Dc) / sum(A) - 1
    ligne[[paste0("phi_", lc)]] <- round(ph[["phi"]], 3); ligne[[paste0("phi_", lc, "_et")]] <- round(ph[["et"]], 3)
    ligne[[paste0("biais_", lc)]] <- round(b, 4)
    ligne[[paste0("biais_", lc, "_et")]] <- round(stats::sd(Dc - (1 + b) * A) / (sqrt(R) * mean(A)), 4)
  }
  cel[[length(cel) + 1L]] <- ligne
}
cel <- do.call(rbind, cel)
imprimer(cel)
cat("\nErreur d'estimation de chaque jeu (lecture L, distincte de l'erreur de Monte-Carlo) : 1 / sqrt(D) moyen sur les jeux où D >= 1\n")
for (sx in SEXES) {
  v <- vapply(AGES, function(a) { d <- col("L", sx, a, "D"); mean(1 / sqrt(d[d >= 1])) }, 0)
  cat(sprintf("  %s : âge 50 %.3f ; âge 60 %.3f ; âge 70 %.3f (min %.3f, max %.3f sur %d-%d)\n", sx, v[AGES == 50], v[AGES == 60],
              v[AGES == 70], min(v), max(v), min(AGES), max(AGES)))
}

# --- descriptif groupé (M383, déclaré avant le passage consigné ; hors verdict) -----------------
cat("\n=== Descriptif groupé (hors verdict ; décision du mainteneur du 10/11/2026, M383, déclaré avant le passage consigné ; constats de simulation) ===\n")
cat(paste("  phi groupé = somme sur x de Var(U_x) / somme sur x de moyenne(A_x), U = D - mu E, A = mu E (E de la lecture) ;",
          "e.t. par la méthode delta sur Q_r = somme sur x de (U_x,r - Ubar_x)^2 et B_r = somme sur x de A_x,r ;",
          "biais groupé pondéré (L) = somme D / somme mu E_L - 1 sur les âges 50 à 70, e.t. de Monte-Carlo.\n"))
for (sx in SEXES) for (lc in LECTURES) {
  mu <- MU_VRAI[[sx]][AGES + 1L]
  U <- vapply(seq_along(AGES), function(i) col(lc, sx, AGES[i], "D") - mu[i] * col(lc, sx, AGES[i], "E"), numeric(R))
  A <- vapply(seq_along(AGES), function(i) mu[i] * col(lc, sx, AGES[i], "E"), numeric(R))
  Q <- rowSums(sweep(U, 2L, colMeans(U))^2); B <- rowSums(A)
  rho <- mean(Q) / mean(B)
  phi <- rho * R / (R - 1)                     # somme des Var(U_x) (dénominateur R - 1) / somme des moy(A_x)
  et <- R / (R - 1) * stats::sd(Q - rho * B) / (sqrt(R) * mean(B))
  ligne <- sprintf("  %s, %s : phi groupé %.4f (e.t. %.4f ; z contre 1 : %.2f)", sx, lc, phi, et, (phi - 1) / et)
  if (lc == "L") {
    Ds <- vapply(AGES, function(a) col("L", sx, a, "D"), numeric(R)); Dt <- rowSums(Ds)
    b <- sum(Dt) / sum(B) - 1
    ligne <- paste0(ligne, sprintf(" ; biais groupé pondéré %.5f (e.t. MC %.5f)", b, stats::sd(Dt - (1 + b) * B) / (sqrt(R) * mean(B))))
  }
  cat(ligne, "\n", sep = "")
}
cat(paste("  Rappel : phi_L = 1 est exact sous le simulateur (force constante par âge entier, R/simulation.R:59,",
          "HYPOTHESE_REPARTITION ; identité du compensateur) ; un écart significatif de phi_L à 1 serait un constat appelant une issue.\n"))

# --- verdict de référence (M382) -----------------------------------------------------------------
cat("\n=== Verdict de référence (règle fixée à l'avance, spécification (0')(c), critère 2, M382 ; 12 familles de Poisson : 2 sexes x 3 lectures x 2 intervalles) ===\n")
cat(paste("  Défaut de calibrage (M383) : la règle ne contrôle aucun risque global (Holm par famille au risque de 5 %,",
          "12 statistiques groupées à 5 % sans ajustement) ; des fausses alarmes sont attendues même sous H1.\n"))
tp <- tab[tab$intervalle %in% POISSON, ]
rej_pi <- tp[!is.na(tp$p_pi_holm) & tp$p_pi_holm < ALPHA_TEST, ]
rej_095 <- tp[!is.na(tp$p_095_holm) & tp$p_095_holm < ALPHA_TEST, ]
grp_sig <- grp[grp$significative_5pc, ]
m60 <- mcn[mcn$paire == "L/M" & mcn$age == 60L & mcn$intervalle %in% POISSON, ]
# Constat mineur C1 de l'audit : z apparié peu informatif si n_x + n_l < 10
# ou R < 1000 ; signalé, sans changer le verdict de référence (M383, (i)).
N_DISC_MIN <- 10L; R_MIN_Z <- 1000L
disc60 <- m60$X_seul_couvre + m60$L_seul_couvre
c_app <- R >= R_MIN_Z && all(disc60 >= N_DISC_MIN)
c_ok <- all(!is.na(m60$z_apparie) & abs(m60$z_apparie) <= 2)
a_ok <- !nrow(rej_pi); b_ok <- !nrow(grp_sig); f_ok <- !nrow(rej_095)
cat(sprintf("  (a) lignes rejetées après Holm contre pi : %d / %d\n", nrow(rej_pi), nrow(tp)))
cat(sprintf("  (b) statistiques groupées significatives à 5 %% : %d / %d (%.2f fausse(s) alarme(s) attendue(s) sous H1 si indépendantes)\n",
            nrow(grp_sig), nrow(grp), ALPHA_TEST * nrow(grp)))
cat(sprintf("  (c) écart M - L à 60 ans, |z apparié| <= 2 : %s (z, paires discordantes n_x + n_l : %s)\n",
            paste0(if (c_ok) "oui" else "non",
                   if (!c_app) sprintf(" (avertissement : R < %d ou n_x + n_l < %d pour au moins un z, z peu informatif)", R_MIN_Z, N_DISC_MIN) else ""),
            paste(sprintf("%s %s %.2f, %d", m60$sexe, m60$intervalle, m60$z_apparie, disc60), collapse = " ; ")))
cat(sprintf("  compatibilité avec 0,95 : lignes rejetées après Holm contre 0,95 : %d / %d\n", nrow(rej_095), nrow(tp)))
verdict <- if (a_ok && b_ok && c_ok) "discrétisation (H1 + H2, H3) établie" else
  if (f_ok && !(a_ok && b_ok)) "fluctuation établie" else "non expliqué"
cat(sprintf("  VERDICT DE RÉFÉRENCE : %s%s\n", verdict, if (!c_app) " (avertissement sur (c) ci-dessus)" else ""))
if (verdict == "non expliqué") {
  cat("  Éléments (phi_x, b_x, biais de cellule : tableau par cellule ci-dessus ; résidus : z_residu) :\n")
  if (nrow(rej_pi)) { cat("  lignes rejetées contre pi :\n"); imprimer(fmt[rownames(rej_pi), c("lecture", "intervalle", "sexe", "age", "observe", "pi_bar", "z_residu", "p_pi_holm")]) }
  if (nrow(grp_sig)) { cat("  familles à statistique groupée significative :\n"); imprimer(grp_sig) }
  if (!c_ok) { cat("  écart M - L à 60 ans non concordant :\n"); imprimer(fm[fm$paire == "L/M" & fm$age == 60L & fm$intervalle %in% POISSON, ]) }
}
cat("  L'excès de couverture de Kaplan-Meier n'est pas couvert par le verdict (pas de pi pour Kaplan-Meier).\n")

# --- lecture calibrée (M383) -----------------------------------------------------------------------
# Sur les lignes `ix` de m : (a) Holm sur les 252 lignes de Poisson contre pi,
# (b) Holm sur les 12 statistiques groupées (p normale bilatérale), (c) Holm
# sur les z appariés M - L à 60 ans applicables (p normale bilatérale), chacun
# au risque ALPHA_TEST / 3 : risque global ALPHA_TEST (Bonferroni entre les
# trois parts).
calc_pi_lignes <- function(ix) {
  o <- list()
  for (lc in LECTURES) for (iv in POISSON) for (sx in SEXES) for (a in AGES) {
    I <- col(lc, sx, a, paste0("I_", iv))[ix]; pis <- col(lc, sx, a, paste0("pi_", iv))[ix]
    p <- !is.na(I); n <- sum(p); k <- sum(I[p]); pp <- pis[p]
    o[[length(o) + 1L]] <- data.frame(famille = paste(sx, lc, iv), lecture = lc, intervalle = iv, sexe = sx, age = a, R = n,
                                      observe = if (n) k / n else NA_real_, pi_bar = if (n) mean(pp) else NA_real_,
                                      p_pi = if (n) p_bilaterale(poisson_binomiale(pp), k) else NA_real_,
                                      stringsAsFactors = FALSE)
  }
  do.call(rbind, o)
}
calc_S <- function(ix) {
  o <- list()
  for (lc in LECTURES) for (iv in POISSON) for (sx in SEXES) {
    s <- numeric(length(ix))
    for (a in AGES) {
      I <- col(lc, sx, a, paste0("I_", iv))[ix]; pis <- col(lc, sx, a, paste0("pi_", iv))[ix]
      p <- !is.na(I); s[p] <- s[p] + (I[p] - pis[p])
    }
    o[[paste(sx, lc, iv)]] <- s
  }
  o
}
calc_z60 <- function(ix) {
  o <- list()
  for (iv in POISSON) for (sx in SEXES) {
    IL <- col("L", sx, 60L, paste0("I_", iv))[ix]; IM <- col("M", sx, 60L, paste0("I_", iv))[ix]
    pL <- col("L", sx, 60L, paste0("pi_", iv))[ix]; pM <- col("M", sx, 60L, paste0("pi_", iv))[ix]
    p <- !is.na(IL) & !is.na(IM)
    n_x <- sum(IM[p] & !IL[p]); n_l <- sum(IL[p] & !IM[p])
    dd <- ifelse(is.na(IM), 0, IM - pM) - ifelse(is.na(IL), 0, IL - pL)
    z <- if (stats::sd(dd) > 0) mean(dd) / (stats::sd(dd) / sqrt(length(ix))) else NA_real_
    o[[length(o) + 1L]] <- data.frame(sexe = sx, intervalle = iv, n_x_plus_n_l = n_x + n_l, z = z,
                                      applicable = length(ix) >= R_MIN_Z && n_x + n_l >= N_DISC_MIN && !is.na(z),
                                      stringsAsFactors = FALSE)
  }
  do.call(rbind, o)
}
lecture_calibree <- function(ix, titre) {
  Rx <- length(ix); a3 <- ALPHA_TEST / 3
  cat(sprintf("\n--- Lecture calibrée, %s (%d jeux) ---\n", titre, Rx))
  if (Rx < 2L) { cat("  non applicable (moins de deux jeux)\n"); return(invisible(NULL)) }
  pl <- calc_pi_lignes(ix)
  if (Rx == R) verifier("lecture calibrée sur tous les jeux : p contre pi identiques à celles du tableau des lignes",
                        identical(pl$p_pi, tp$p_pi))
  pl$p_holm <- stats::p.adjust(pl$p_pi, "holm")
  rejA <- pl[!is.na(pl$p_holm) & pl$p_holm < a3, ]
  S <- calc_S(ix)
  if (Rx == R) verifier("lecture calibrée sur tous les jeux : S_r identiques à ceux de la statistique groupée",
                        identical(S, S_fam[names(S)]))
  gs <- do.call(rbind, lapply(names(S), function(f) {
    s <- S[[f]]; e <- stats::sd(s) / sqrt(Rx); z <- mean(s) / e
    data.frame(famille = f, effet_S_sur_21 = mean(s) / length(AGES),
               ic95_inf = (mean(s) - 1.96 * e) / length(AGES), ic95_sup = (mean(s) + 1.96 * e) / length(AGES),
               z = z, p = 2 * stats::pnorm(-abs(z)), stringsAsFactors = FALSE)
  }))
  gs$p_holm <- stats::p.adjust(gs$p, "holm")
  rejB <- gs[gs$p_holm < a3, ]
  zc <- calc_z60(ix)
  zc$p <- ifelse(zc$applicable, 2 * stats::pnorm(-abs(zc$z)), NA_real_)
  zc$p_holm <- if (any(zc$applicable)) stats::p.adjust(zc$p, "holm") else NA_real_
  rejC <- zc[!is.na(zc$p_holm) & zc$p_holm < a3, ]
  cat(sprintf("  (a) Holm sur les %d lignes de Poisson contre pi, risque %.4f : %d ligne(s) en écart\n", sum(!is.na(pl$p_pi)), a3, nrow(rejA)))
  cat(sprintf("  (b) Holm sur les %d statistiques groupées (p normale bilatérale), risque %.4f : %d famille(s) en écart ; taille d'effet S/21 par famille, intervalle ± 1,96 e.t. MC (descriptif) :\n",
              nrow(gs), a3, nrow(rejB)))
  g2 <- gs; for (nm in c("effet_S_sur_21", "ic95_inf", "ic95_sup")) g2[[nm]] <- signif(g2[[nm]], 3)
  g2$z <- round(g2$z, 2); g2$p <- signif(g2$p, 3); g2$p_holm <- signif(g2$p_holm, 3)
  imprimer(g2)
  cat(sprintf("  (c) Holm sur les z appariés M - L à 60 ans applicables (R >= %d et n_x + n_l >= %d), risque %.4f : %d en écart%s\n",
              R_MIN_Z, N_DISC_MIN, a3, nrow(rejC), if (!any(zc$applicable)) " ; (c) non applicable" else ""))
  z2 <- zc; z2$z <- round(z2$z, 2); z2$p <- signif(z2$p, 3); z2$p_holm <- signif(z2$p_holm, 3)
  imprimer(z2)
  # Écart minimal détectable (approché) : normal, puissance 80 %, seuil
  # bilatéral de la première étape de Holm de (a), (alpha/3) / nombre de lignes.
  nl <- sum(!is.na(pl$p_pi))
  zq <- stats::qnorm(1 - a3 / nl / 2) + stats::qnorm(0.8)
  pl$mde <- zq * sqrt(pl$pi_bar * (1 - pl$pi_bar) / pl$R)
  md <- do.call(rbind, lapply(AGES, function(a) {
    v <- pl$mde[pl$age == a]
    data.frame(age = a, mde_min = round(min(v, na.rm = TRUE), 4), mde_max = round(max(v, na.rm = TRUE), 4))
  }))
  cat(sprintf("  Écart minimal détectable par âge sur la couverture (approché : normal, puissance 80 %%, seuil bilatéral %.2g de la première étape de Holm de (a) ; min et max sur les 12 familles) :\n",
              a3 / nl))
  imprimer(md)
  if (!nrow(rejA) && !nrow(rejB) && !nrow(rejC)) {
    cat(sprintf("  LECTURE CALIBRÉE : compatible avec la discrétisation (aucun écart détecté, risque global de 5 %%)%s\n",
                if (!any(zc$applicable)) " ; (c) non applicable" else ""))
  } else {
    cat("  LECTURE CALIBRÉE : écart détecté ; lignes en cause :\n")
    if (nrow(rejA)) { r2 <- rejA; r2$p_holm <- signif(r2$p_holm, 3); r2$observe <- round(r2$observe, 4); r2$pi_bar <- round(r2$pi_bar, 4); imprimer(r2[, c("lecture", "intervalle", "sexe", "age", "R", "observe", "pi_bar", "p_holm")]) }
    if (nrow(rejB)) imprimer(g2[g2$famille %in% rejB$famille, ])
    if (nrow(rejC)) imprimer(z2[paste(z2$sexe, z2$intervalle) %in% paste(rejC$sexe, rejC$intervalle), ])
  }
  invisible(NULL)
}
cat("\n=== Lecture calibrée (ajoutée le 10/11/2026 après un pilote à R = 1 000 sur les jeux 1 à 1 000 ; décision du mainteneur, M383) : risque global de 5 % partagé en trois parts de 5 %/3 ===\n")
# Titres sur les jeux effectivement lus (rangs, nombre), non sur les jeux demandés
# (R_JEUX) : si un jeu est refusé ou en erreur, la lecture compte moins de jeux
# que la demande, et le titre le dit (#207). `lus` : rangs des jeux réussis ;
# `demandes` : rangs demandés pour cette lecture.
titre_jeux <- function(lus, demandes) {
  manq <- length(demandes) - length(lus)
  s <- sprintf("jeux %d à %d", min(lus), max(lus))
  if (manq > 0L) s <- sprintf("%s ; %d jeu(x) manquant(s) sur %d demandés", s, manq, length(demandes))
  s
}
rangs <- which(ok)  # rang du jeu de la ligne i de m : rangs[i]
lecture_calibree(seq_len(R), titre_jeux(rangs, seq_len(R_JEUX)))
# Partie confirmatoire : jeux réussis de rang > R_RATTACHEMENT (lignes de m).
conf <- which(rangs > R_RATTACHEMENT)
if (!length(conf)) {
  cat(sprintf("\n--- Lecture calibrée, partie confirmatoire (jeux de rang > %d, non vus au choix de la règle) : non applicable (%s) ---\n",
              R_RATTACHEMENT,
              if (R_JEUX <= R_RATTACHEMENT) sprintf("aucun jeu demandé à ces rangs : %d jeux demandés <= %d", R_JEUX, R_RATTACHEMENT)
              else sprintf("%d jeux demandés à ces rangs, aucun réussi ; %d jeux lus sur %d demandés",
                           R_JEUX - R_RATTACHEMENT, R, R_JEUX)))
} else lecture_calibree(conf, sprintf("partie confirmatoire, %s (non vus au choix de la règle)",
                                      titre_jeux(rangs[conf], seq_len(max(R_JEUX - R_RATTACHEMENT, 0L)) + R_RATTACHEMENT)))
cat(paste("\nStatuts : couvertures, McNemar, phi, biais de cellule = constats de simulation (erreur de Monte-Carlo, fonction de R) ;",
          "pi = calcul exact sous approximation (D ~ Poisson conditionnellement aux expositions réalisées, validée par L) ;",
          "erreur d'estimation (1 / sqrt(D) par cellule) résumée à part.\n"))

cat("\nAvertissements du résultat (x$avertissements : lecture, code ; nombre de constructions qui l'émettent) :\n")
if (length(AVERTIS$codes)) for (nm in names(AVERTIS$codes)) cat(sprintf("  %6d  %s\n", AVERTIS$codes[[nm]], nm)) else cat("  aucun\n")
cat("\nAvertissements R reçus (warning(), texte tronqué) :\n")
if (length(AVERTIS$n)) for (nm in names(AVERTIS$n)) cat(sprintf("  %6d  %s\n", AVERTIS$n[[nm]], nm)) else cat("  aucun\n")
cat(sprintf("\nDurée totale : %.1f min\n", (proc.time()[["elapsed"]] - t0) / 60))
bilan()
