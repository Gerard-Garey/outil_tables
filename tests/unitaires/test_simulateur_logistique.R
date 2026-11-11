# -----------------------------------------------------------------------------
#  tests/unitaires/test_simulateur_logistique.R : loi vraie logistique,
#  table_logistique() du module `simulation` (issue #211 ; E2.19, E2.20 ;
#  ADR 0007, points 7 et 11.4 ; spécification d'actuary-approfondi de #211 et
#  son amendement, T1 à T9)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : q_x à 12 décimales de la spécification (T2,
#    voie stats::integrate), asymptote 1 - exp(-(a + 1)), équivalents aux
#    extrêmes, refus ;
#  - recoupement : stats::integrate de mu sur [x, x+1) (T3), dont le moteur ne
#    se sert pas ; comparaison à table_gompertz_makeham() (T5) ;
#  - identité exacte : reproductibilité et consignation (T8) ;
#  - constat de simulation (E2.20), section finale (T9) : n, alpha, régions
#    et probabilité de fausse alarme écrits.
#  Trois erreurs à ne pas confondre dans le constat : (i) le **constat de
#  simulation** est un test d'hypothèse dont la fausse alarme est la
#  probabilité, sur les graines, qu'une implémentation juste échoue ; (ii)
#  l'**erreur Monte-Carlo** est la dispersion binomiale du décompte autour de
#  n q_x (n = 10^5 têtes par cohorte) ; c'est elle que mesure la largeur de la
#  région d'acceptation ; (iii) l'**erreur d'estimation** (exposition)
#  n'entre pas ici : le décompte est comparé à la probabilité exacte du
#  modèle simulé, aucune quantité n'est estimée.
# -----------------------------------------------------------------------------

simuler <- tablexp::simuler_portefeuille
table_logistique <- tablexp:::table_logistique
table_gm <- tablexp:::table_gompertz_makeham
table_constante <- tablexp:::table_constante
loi_effective <- tablexp:::loi_effective
D <- as.Date

# Graines fixées avant exécution (spécification, § 4 ; sous-plage 22 001 200
# - 22 001 229) ; changer de graine après un échec est interdit.
GRAINES_JEUX <- c(G = 22001200L, L = 22001201L)
GRAINES_CONSTAT <- c(L100 = 22001202L, G100 = 22001203L, L110 = 22001204L, G110 = 22001205L,
                     L120 = 22001206L, G120 = 22001207L)

# Lois de la spécification : LT logistique, GT Gompertz-Makeham appariée
# (mêmes a et b, c_GM = e^c).
LT_A <- 2e-4; LT_B <- 2.5e-5; LT_C <- log(1.1)
LT <- table_logistique(LT_A, LT_B, LT_C, 0:130)
L0 <- table_logistique(0, LT_B, LT_C, 0:130)
GT <- table_gm(LT_A, LT_B, 1.1, 0:130)
G0 <- table_gm(0, LT_B, 1.1, 0:130)
mu_logistique <- function(a, b, c) function(t) a + b * exp(c * t) / (1 + b * exp(c * t))
GRILLE <- expand.grid(a = c(0, 2e-4, 5e-3), b = c(1e-6, 2.5e-5, 1e-2), c = c(0.05, log(1.1), 0.2))
TABLES_GRILLE <- lapply(seq_len(nrow(GRILLE)), function(i)
  table_logistique(GRILLE$a[i], GRILLE$b[i], GRILLE$c[i], 0:130))

# --- T1 : forme ------------------------------------------------------------------------

verifier("T1 forme (valeur calculée à la main) : quatre paramètres a, b, c, ages, sans valeur par défaut",
         identical(names(formals(table_logistique)), c("a", "b", "c", "ages")) &&
           all(vapply(formals(table_logistique), function(f) identical(f, quote(expr = )), logical(1L))))
verifier("T1 forme (valeur calculée à la main) : classe, statut, sexe, âges, paramètres, nom",
         inherits(LT, "table_reference") && identical(attr(LT, "statut"), "synthetique") &&
           identical(unique(LT$sexe), "ensemble") && identical(LT$age, 0:130) &&
           identical(attr(LT, "parametres"),
                     list(loi = "logistique", a = LT_A, b = LT_B, c = LT_C, ages = c(0L, 130L))) &&
           identical(attr(LT, "nom"), "Logistique synthétique (a = 2e-04, b = 2.5e-05, c = 0.09531018)"))

# --- T2 : valeurs à 12 décimales -----------------------------------------------------------

AGES_T2 <- c(0, 40, 60, 80, 100, 110, 120, 130)
verifier("T2 (valeur calculée à la main, voie stats::integrate de la spécification) : LT, q_x à 12 décimales",
         proche(LT$qx[match(AGES_T2, LT$age)],
                c(0.000226203870, 0.001384788475, 0.008090365980, 0.049900229572,
                  0.233299664772, 0.383682964335, 0.507713611711, 0.578221538326),
                rel = 0, abs = 6e-13))
verifier("T2 (valeur calculée à la main, voie stats::integrate de la spécification) : a = 0 (Kannisto), q_x à 12 décimales",
         proche(L0$qx[match(AGES_T2, L0$age)],
                c(0.000026229114, 0.001185045459, 0.007891964213, 0.049710190615,
                  0.233146309370, 0.383559688601, 0.507615144587, 0.578137174197),
                rel = 0, abs = 6e-13))

# --- T3 : critère 1e-12 contre stats::integrate ----------------------------------------------

verifier("T3 (recoupement : stats::integrate de mu sur [x, x+1), le moteur emploie la forme close) : 27 lois x 0:130, 1e-12 relatif",
         all(vapply(seq_len(nrow(GRILLE)), function(i) {
           mu <- mu_logistique(GRILLE$a[i], GRILLE$b[i], GRILLE$c[i])
           ref <- vapply(0:130, function(x)
             -expm1(-stats::integrate(mu, x, x + 1, rel.tol = 1e-13)$value), numeric(1L))
           isTRUE(proche(TABLES_GRILLE[[i]]$qx, ref, rel = 1e-12, abs = 0))
         }, logical(1L))))
verifier("T3 (identité exacte) : loi_effective() reprend les q_x de la table à l'identique (facteurs 1)",
         {
           e <- loi_effective(list(table = LT, facteurs = c(H = 1, F = 1)), "deces", c("H", "F"))
           identical(e$qx[e$sexe == "H"], LT$qx) && identical(e$qx[e$sexe == "F"], LT$qx)
         })

# --- T4 : monotonie et plateau -----------------------------------------------------------------

verifier("T4 (valeur calculée à la main) : q_x strictement croissant sur 0:130, 27 lois",
         all(vapply(TABLES_GRILLE, function(t) all(diff(t$qx) > 0), logical(1L))))
verifier("T4 (valeur calculée à la main) : q_x <= 1 - exp(-(a + 1)) sur 0:130, 27 lois",
         all(vapply(seq_len(nrow(GRILLE)), function(i)
           all(TABLES_GRILLE[[i]]$qx <= -expm1(-(GRILLE$a[i] + 1))), logical(1L))))
LT_LONG <- table_logistique(LT_A, LT_B, LT_C, 0:1000)
PLATEAU_LT <- -expm1(-(LT_A + 1))
verifier("T4 (valeur calculée à la main : asymptote 1 - exp(-(a + 1))) : LT sur 0:1000, croissant au sens large, majoré par le plateau, q_1000 = plateau à 1e-14 relatif",
         !anyNA(LT_LONG$qx) && all(diff(LT_LONG$qx) >= 0) && all(LT_LONG$qx <= PLATEAU_LT) &&
           isTRUE(proche(LT_LONG$qx[LT_LONG$age == 1000L], PLATEAU_LT, rel = 1e-14, abs = 0)))
verifier("T4 (valeur calculée à la main) : plateau de a = 0 = 0,632120558828558",
         proche(-expm1(-1), 0.632120558828558, rel = 0, abs = 1e-15))

# --- T5 : comparaison à Gompertz-Makeham ---------------------------------------------------------

borne_t5 <- function(tl, tg, b, c) {
  u <- b * exp(c * tl$age)
  all(tl$qx < tg$qx) && all(1 - tl$qx / tg$qx <= u * (exp(c) + 1) / 2 + 1e-12)
}
verifier("T5 (recoupement : table_gompertz_makeham(), c_GM = e^c) : LT, q_L < q_GM et 1 - q_L/q_GM <= u_x (e^c + 1)/2 sur 0:130",
         borne_t5(LT, GT, LT_B, LT_C))
verifier("T5 (recoupement : table_gompertz_makeham(), c_GM = e^c) : a = 0, q_L < q_GM et 1 - q_L/q_GM <= u_x (e^c + 1)/2 sur 0:130",
         borne_t5(L0, G0, LT_B, LT_C))

# --- T6 : extrêmes -----------------------------------------------------------------------------

verifier("T6 (valeur calculée à la main : équivalent b expm1(c) / c) : b = 1e-300, c = 0,1, x = 0, 1e-12 relatif",
         proche(table_logistique(0, 1e-300, 0.1, 0)$qx, 1e-300 * expm1(0.1) / 0.1, rel = 1e-12, abs = 0))
B_GRAND <- table_logistique(0, 1e10, 0.1, 0:1000)
verifier("T6 (valeur calculée à la main : plateau 1 - exp(-1)) : b = 1e10, c = 0,1, sans NaN, majoré par le plateau, plateau à 1e-14 relatif dès 100 ans",
         !anyNA(B_GRAND$qx) && all(diff(B_GRAND$qx) >= 0) && all(B_GRAND$qx <= -expm1(-1)) &&
           isTRUE(proche(B_GRAND$qx[B_GRAND$age >= 100L], rep(-expm1(-1), 901L), rel = 1e-14, abs = 0)))
C_GRAND <- table_logistique(0, LT_B, 709, 0:130)
verifier("T6 (valeur calculée à la main) : c = 709 accepté, q_x finis dans (0, 1]",
         all(is.finite(C_GRAND$qx)) && all(C_GRAND$qx > 0) && all(C_GRAND$qx <= 1))
verifier("T6 (valeur calculée à la main : limite c -> 0, mu = a + b/(1 + b)) : c = 1e-8, b = 1e-2, x = 0, 1e-7 relatif",
         proche(table_logistique(0, 1e-2, 1e-8, 0)$qx, -expm1(-1e-2 / (1 + 1e-2)), rel = 1e-7, abs = 0))

# --- T7 : refus ----------------------------------------------------------------------------------

PARAMETRES_REFUSES <- list(
  "a = -1e-10" = list(a = -1e-10), "b = 0" = list(b = 0), "b = -1" = list(b = -1),
  "c = 0" = list(c = 0), "c = -0,1" = list(c = -0.1), "c = 710" = list(c = 710),
  "a = NA" = list(a = NA), "b = Inf" = list(b = Inf), "c = NaN" = list(c = NaN),
  "a = c(0, 0)" = list(a = c(0, 0)), "b = \"1e-5\"" = list(b = "1e-5"),
  "a = numeric(0)" = list(a = numeric(0)),
  "ages = c(0, 1.5)" = list(ages = c(0, 1.5)), "ages = -1" = list(ages = -1),
  "ages = integer(0)" = list(ages = integer(0)), "ages = c(0, NA)" = list(ages = c(0, NA)),
  "ages = \"0\"" = list(ages = "0"))
for (cas in names(PARAMETRES_REFUSES)) {
  args <- modifyList(list(a = LT_A, b = LT_B, c = LT_C, ages = 0:130), PARAMETRES_REFUSES[[cas]])
  verifier(sprintf("T7 refus (valeur calculée à la main) : %s refusé (loi_invalide)", cas),
           leve_refus(do.call(table_logistique, args), "loi_invalide"))
}

# --- T8 : un jeu par forme de queue ------------------------------------------------------------------

RACHAT_NUL <- table_constante(0, 0:130, "transition absente (q = 0)")
loi_t8 <- function(deces) list(deces = list(table = deces, facteurs = c(H = 1, F = 1)),
                               rachat = list(table = RACHAT_NUL, facteurs = c(H = 1, F = 1)))
PF_T8 <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 60, age_max = 100,
              origine = D("2005-01-01"))
PER_T8 <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
for (forme in c("G", "L")) {
  tab <- if (forme == "G") GT else LT
  g <- GRAINES_JEUX[[forme]]
  s1 <- simuler(loi_t8(tab), PF_T8, PER_T8, g)
  s2 <- simuler(loi_t8(tab), PF_T8, PER_T8, g)
  info <- attr(s1, "simulation")
  verifier(sprintf("T8 (identité exacte) : jeu %s, même graine, jeux identiques ; table des décès et graine consignées", forme),
           identical(s1, s2) && identical(info$loi_vraie$tables$deces, tab) &&
             identical(info$graine, g))
}

# --- T9 : constat de simulation (E2.19, E2.20) --------------------------------------------------------
#
# Six cohortes (L et G à 100, 110 et 120 ans ; graines GRAINES_CONSTAT) de
# n = 10^5 têtes entrées à l'âge exact x le 2016-03-15 (anniversaire),
# période du 2016-03-15 au 2017-12-31, sans rachat : l'année x entière est
# observée. Décompte D des décès datés avant anniv(x+1) = 2017-03-15 ; sous
# le modèle simulé, D ~ Binomiale(n, q_x) exactement. Région bilatérale
# exacte [qbinom(a_k/2), qbinom(1 - a_k/2)], alpha global 10^-3, K = 6,
# a_k = alpha / K ; régions écrites en dur (spécification), recalculées et
# comparées. Fausse alarme familiale bornée par l'inégalité de l'union
# sum(FA_k) <= alpha (9,836e-4), sans hypothèse d'indépendance. Pouvoir
# discriminant : probabilité exacte, sous l'une des deux lois d'un même âge,
# que D tombe dans la région de l'autre, <= 1e-12. Erreur Monte-Carlo
# binomiale (n = 10^5 par cohorte), pas d'erreur d'estimation.

N_CONSTAT <- 1e5; ALPHA <- 1e-3; K <- 6L; ALPHA_K <- ALPHA / K
E9 <- D("2016-03-15"); PER_T9 <- list(debut = E9, fin = D("2017-12-31")); FIN_ANNEE <- D("2017-03-15")
REGIONS_SPEC <- list(L100 = c(22828, 23835), G100 = c(29802, 30897), L110 = c(37790, 38948),
                     G110 = c(60268, 61430), L120 = c(50176, 51367), G120 = c(90875, 91549))
p_constat <- function(nom) {
  tab <- if (substr(nom, 1L, 1L) == "L") LT else GT
  tab$qx[tab$age == as.integer(substring(nom, 2L))]
}
regions <- lapply(names(REGIONS_SPEC), function(nom) {
  p <- p_constat(nom)
  c(stats::qbinom(ALPHA_K / 2, N_CONSTAT, p), stats::qbinom(1 - ALPHA_K / 2, N_CONSTAT, p))
})
names(regions) <- names(REGIONS_SPEC)
fa <- vapply(names(REGIONS_SPEC), function(nom) {
  p <- p_constat(nom); r <- regions[[nom]]
  stats::pbinom(r[1L] - 1, N_CONSTAT, p) + stats::pbinom(r[2L], N_CONSTAT, p, lower.tail = FALSE)
}, numeric(1L))
verifier("T9 constat (valeur calculée à la main) : régions exactes recalculées = régions de la spécification",
         identical(regions, REGIONS_SPEC))
verifier("T9 constat (valeur calculée à la main) : sum(FA_k) = 9,836e-4 <= alpha",
         isTRUE(proche(sum(fa), 9.836e-4, rel = 0, abs = 5e-8)) && sum(fa) <= ALPHA)
verifier("T9 constat (valeur calculée à la main) : pouvoir discriminant, P(D dans la région de l'autre loi) <= 1e-12",
         all(vapply(c(100L, 110L, 120L), function(x) {
           l <- sprintf("L%d", x); g <- sprintf("G%d", x)
           dans <- function(p, r) stats::pbinom(r[2L], N_CONSTAT, p) - stats::pbinom(r[1L] - 1, N_CONSTAT, p)
           dans(p_constat(l), REGIONS_SPEC[[g]]) <= 1e-12 && dans(p_constat(g), REGIONS_SPEC[[l]]) <= 1e-12
         }, logical(1L))))
for (nom in names(REGIONS_SPEC)) {
  tab <- if (substr(nom, 1L, 1L) == "L") LT else GT
  x <- as.integer(substring(nom, 2L)); r <- REGIONS_SPEC[[nom]]; g <- GRAINES_CONSTAT[[nom]]
  sc <- simuler(loi_t8(tab), list(mode = "age_exact", n = N_CONSTAT, proportion_hommes = 0.5,
                                  age = x, date_entree = E9), PER_T9, g)
  d <- sum(sc$cause_sortie == "deces" & sc$date_sortie < FIN_ANNEE, na.rm = TRUE)
  ligne <- sprintf(paste("T9 constat de simulation %s : D = %d dans [%d, %d] (n = %g, q_x = %.10g,",
                         "alpha_k = %.4g, fausse alarme exacte FA_k = %.4g ; graine %d)"),
                   nom, d, r[1L], r[2L], N_CONSTAT, p_constat(nom), ALPHA_K, fa[[nom]], g)
  cat("  ", ligne, "\n", sep = "")
  verifier(ligne, d >= r[1L] && d <= r[2L])
}
