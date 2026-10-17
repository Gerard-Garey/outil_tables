# -----------------------------------------------------------------------------
#  tests/unitaires/test_constats_brut.R : constats de simulation des taux
#  bruts, retrouver la loi vraie par l'étape `brut` (issue #16 ; E2.20 ;
#  ADR 0007, point 11.4 ; spécification d'actuary du 17/10/2026, A.1 à A.8)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée : cibles exactes du moteur sous le modèle simulé (A.1),
#    régions de décision, seuils de la borne exponentielle, couvertures
#    exactes de C3a, puissances exactes ; écrites en dur d'après la
#    spécification (A.6) puis recalculées ici, sur le modèle de
#    `region_spec` de test_simulation.R (#10) ;
#  - identité exacte : E^i = n et Kaplan-Meier = binomial sur des cellules
#    entières (A.10 de #15), définitions « cellule » et « fenêtre » de
#    l'exposition initiale sous censure par la période (A.2 de #14) ;
#  - constat de simulation (E2.20) : sous une graine fixée, la sortie du
#    moteur tombe dans une région dont la fausse alarme est écrite. Ce
#    n'est jamais un résultat exact sur le moteur.
#
#  Trois erreurs à ne pas confondre (reprise de #10) : (i) le **constat de
#  simulation** est un test d'hypothèse dont la fausse alarme est la
#  probabilité, sur les graines, qu'une implémentation juste échoue ; la
#  graine est fixe, le test est déterministe ; (ii) l'**erreur
#  Monte-Carlo** est due au nombre fini de réplications : en C3, la
#  dispersion binomiale du nombre d'intervalles couvrants, sqrt(C(1 - C)/R),
#  fonction de R seul ; (iii) l'**erreur d'estimation** est celle d'un taux
#  brut rapporté à une exposition : en C1 et C2 (une réplication, R = 1),
#  c'est la largeur de la région ; en C3, la demi-largeur moyenne des
#  intervalles, fonction de l'exposition de chaque bloc. Chaque ligne de C3
#  écrit les deux.
#
#  Cibles (A.1, preuve écrite par actuary) : le simulateur tire des
#  instants continus sous force constante par âge entier, le moteur voit
#  des jours (convention T1 : exposition sur [a, s), événement du jour s à
#  l'instant s). Dans une cellule de L jours, forces mu_d et mu_r, m =
#  mu_d + mu_r, une tête à risque au début d'un jour subit la transition
#  avec h_d = (mu_d/m)(1 - e^(-m/L)), sort par la cause concurrente avec
#  h_r = (mu_r/m)(1 - e^(-m/L)), survit avec s = e^(-m/L). D'où :
#  - binomial, cellule entière : D ~ Bin(n, pi) exactement, pi = q_x sans
#    cause concurrente ;
#  - Kaplan-Meier, grille unique : limite q* = 1 - (1 - h_d)^L ;
#  - Poisson : E[D - mu* E^c] = 0 exactement, mu* = (mu_d/m) L (e^(m/L) - 1),
#    soit mu*/mu_d - 1 ~ m/(2L) : biais exact et négligeable de
#    l'estimateur de Poisson dû à T1 (un demi-jour d'exposition manquant en
#    moyenne par sortie) ;
#  - binomial, censure à la fraction c de la cellule : D ~ Bin(n, pi_c),
#    pi_c = 1 - e^(-mu c) ; limites pi_c/(c(1 - pi_c) + pi_c) (« cellule »)
#    et pi_c/c (« fenêtre ») ; aucune ne vaut q.
#  Chaque test compare l'estimateur à sa cible exacte sous le modèle
#  simulé ; l'écart de la cible à la loi vraie est écrit à côté.
#
#  Fausse alarme familiale (A.3) : borne de l'union pondérée sur 190 tests,
#  sum(alpha_k) = 9,8.10^-4 <= alpha = 10^-3, sans hypothèse
#  d'indépendance ; parties exactes ou bornées exactement, parties
#  nominales asymptotiques (C1.5 Kaplan-Meier, C2) écrites au bilan.
#  Graines déclarées (A.5) : 20261611 à 20261616, 20261621, 20261631,
#  20261641 ; tout aléa passe par simuler_portefeuille() et sa graine
#  locale (ADR 0007, point 8). Chaque ligne de constat est écrite (cat)
#  hors du mode verbeux.
# -----------------------------------------------------------------------------

simuler <- tablexp::simuler_portefeuille
table_gm <- tablexp:::table_gompertz_makeham
table_constante <- tablexp:::table_constante
D <- as.Date

# Loi vraie de #10 (A.2).
GM_A <- 2e-4; GM_B <- 2.5e-5; GM_C <- 1.1
GM <- table_gm(GM_A, GM_B, GM_C, 0:120)
RACHAT <- table_constante(0.05, 0:120, "rachat constant 5 %")
NUL <- table_constante(0, 0:120, "transition absente (q = 0)")
loi <- function(deces = GM, rachat = RACHAT, f_deces = c(H = 1, F = 0.6), f_rachat = c(H = 1, F = 1))
  list(deces = list(table = deces, facteurs = f_deces), rachat = list(table = rachat, facteurs = f_rachat))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
ROLES <- c(id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance", date_entree = "date_entree",
           date_sortie = "date_sortie", cause_sortie = "cause_sortie")
ROLES_CONTRAT <- c(id_contrat = "id_contrat", ROLES)
plan_constat <- function(periode, estimateurs = c("poisson", "binomial", "kaplan_meier"),
                         segmentation = character(0), transition = VD, niveau = 0.95)
  list(transition = transition, periode = periode, delai_declaration = 0, segmentation = segmentation,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "plus_fine", par_annee = FALSE),
                     brut = list(estimateurs = estimateurs, niveau = niveau)))
duree_depuis <- function(t0) proc.time()[["elapsed"]] - t0

ALPHA <- 1e-3
A_C1 <- 2e-5; A_C2_CELLULES <- 1.5e-4; A_C2_GLOBAL <- 1e-4; A_C3A <- 3e-5; A_C3B <- 2e-5
L <- 365
NIVEAU <- 0.95
# Registre des tests statistiques du constat : nom, accepté, alpha_k,
# fausse alarme exacte ou bornée (NA si nominale), nature.
CONSTATS <- data.frame(test = character(0), accepte = logical(0), alpha_k = numeric(0),
                       fa = numeric(0), nature = character(0), stringsAsFactors = FALSE)
noter <- function(test, accepte, alpha_k, fa, nature)
  CONSTATS[nrow(CONSTATS) + 1L, ] <<- list(test, isTRUE(accepte), alpha_k, fa, nature)
ecrire <- function(ligne) cat("  ", ligne, "\n", sep = "")

q_gm <- function(x) GM$qx[GM$age == x]
mu_de_q <- function(q) -log1p(-q)
# q de Gompertz-Makeham à l'âge x + s (décalage d'âge des témoins C2) ;
# même formule que la table, écrite -expm1(...) ici et 1 - exp(...) dans le
# moteur (R/simulation.R:66) : l'écart à s = 0, au plus 2,1e-13 relatif
# (âge 9, mesuré), est un arrondi flottant (approximation numérique), pas
# une différence de formule. Les cibles sans décalage se lisent sur la
# table elle-même.
q_gm_decale <- function(x, s) -expm1(-(GM_A + GM_B * GM_C^(x + s) * (GM_C - 1) / log(GM_C)))
verifier("q de Gompertz-Makeham décalé (valeur calculée) : à s = 0, égal à la table sur 0:120, 1e-12 relatif",
         proche(q_gm_decale(0:120, 0), GM$qx, rel = 1e-12, abs = 0))

# Modèle discrétisé au jour (A.1) : cellule de L jours, forces mu_d, mu_r.
modele_jour <- function(mu_d, mu_r, L) {
  m <- mu_d + mu_r
  s <- exp(-m / L)
  h_d <- if (m > 0) mu_d / m * (1 - s) else 0
  list(m = m, s = s, L = L, h_d = h_d,
       h_r = if (m > 0) mu_r / m * (1 - s) else 0,
       mu_etoile = if (m > 0) mu_d / m * L * expm1(m / L) else 0,   # E[D - mu* E^c] = 0
       q_km = -expm1(L * log1p(-h_d)),                              # limite de Kaplan-Meier
       q_net = -expm1(-mu_d))
}

# --- borne exponentielle de la région de Poisson (A.2, C1) ---------------------------------------
#
# Une tête observée depuis le début de la cellule, fenêtre de J jours :
# X = D_i - mu* E_i prend les valeurs 1 - mu* k/L (transition le jour k,
# probabilité s^k h_d), -mu* k/L (sortie concurrente le jour k, s^k h_r),
# -mu* J/L (survie à la fin de la fenêtre, s^J). U = somme de n copies
# indépendantes. Pour tout theta > 0, P(U >= t) <= exp(-theta t + n
# Lambda(theta)), Lambda = log E[e^(theta X)] (inégalité de Markov sur
# e^(theta U)) : borne exacte. Le calcul de t (optimize, uniroot) est une
# approximation numérique sans effet sur la validité : t est arrondi vers
# l'extérieur à 1e-6, puis la borne est recalculée en t au theta retenu et
# vérifiée <= alpha_k/2.
atomes_poisson <- function(md, J) {
  k <- 0:(J - 1)
  list(p = c(md$s^k * md$h_d, md$s^k * md$h_r, md$s^J),
       x = c(1 - md$mu_etoile * k / md$L, -md$mu_etoile * k / md$L, -md$mu_etoile * J / md$L))
}
log_borne <- function(at, n, t, theta) {
  y <- theta * at$x; mx <- max(y)
  -theta * t + n * (mx + log(sum(at$p * exp(y - mx))))
}
theta_optimal <- function(at, n, t)
  stats::optimize(function(th) log_borne(at, n, t, th), c(0, 50))$minimum
seuil_exponentiel <- function(at, n, a) {
  sd <- sqrt(n * sum(at$p * at$x^2))
  brut <- stats::uniroot(function(t) log_borne(at, n, t, theta_optimal(at, n, t)) - log(a),
                         c(sd, 20 * sd), tol = 1e-10)$root
  t <- ceiling(brut * 1e6) / 1e6                       # arrondi vers l'extérieur à 1e-6
  list(brut = brut, t = t, sd = sd, log_borne = log_borne(at, n, t, theta_optimal(at, n, t)))
}

# --- C1 : cohortes d'âge exact, n = 10^5 têtes (A.2) ----------------------------------------------
#
# Entrée à l'âge exact x le 2016-03-15, période du 2016-03-15 au
# 2017-03-14 : cellule x entière, L = 365 (sauf C1.6, période jusqu'au
# 2016-09-13, c = 183/365). Douze tests, alpha_k = 2.10^-5 chacun.
# Binomial : D dans [qbinom(a/2, n, pi), qbinom(1 - a/2, n, pi)], fausse
# alarme exacte. Poisson : U = D - mu* E^c dans [-t_lo, t_hi], fausse
# alarme <= alpha_k, borne exacte. Kaplan-Meier (C1.5) : z = (logit q -
# logit q*) q(1 - q)/sqrt(variance), |z| <= qnorm(1 - a/2), asymptotique.
# Le binomial de C1.5 n'est pas testé : sa cible sous le simulateur n'est
# pas q (A.2).

E1 <- D("2016-03-15")
PER_ENTIERE <- list(debut = E1, fin = D("2017-03-14"))
PER_CENSUREE <- list(debut = E1, fin = D("2016-09-13"))
N1 <- 1e5
C1 <- list(
  list(id = 1L, nom = "décès, x = 60, H, sans rachat", x = 60, pH = 1, loi = loi(rachat = NUL),
       q_d = q_gm(60), q_r = 0, transition = VD, periode = PER_ENTIERE,
       region_spec = c(697, 939), seuils_spec = c(133.1693034, 140.7553861)),
  list(id = 2L, nom = "décès, x = 80, F", x = 80, pH = 0, loi = loi(rachat = NUL),
       q_d = 0.6 * q_gm(80), q_r = 0, transition = VD, periode = PER_ENTIERE,
       region_spec = c(2917, 3388), seuils_spec = c(265.6469133, 272.9572338)),
  list(id = 3L, nom = "décès, x = 100, F", x = 100, pH = 0, loi = loi(rachat = NUL),
       q_d = 0.6 * q_gm(100), q_r = 0, transition = VD, periode = PER_ENTIERE,
       region_spec = c(17691, 18731), seuils_spec = c(644.959662, 650.3999641)),
  list(id = 4L, nom = "rachat, x = 40, q^r = 0,05, sans décès, unité contrat", x = 40, pH = 0.5,
       loi = loi(deces = NUL), q_d = 0.05, q_r = 0, transition = RA, periode = PER_ENTIERE,
       region_spec = c(4709, 5296), seuils_spec = c(335.7603012, 342.8513775)),
  list(id = 5L, nom = "décès, x = 60, H, rachat concurrent q^r = 0,30", x = 60, pH = 1,
       loi = loi(rachat = table_constante(0.30, 0:120, "rachat constant 30 %")),
       q_d = q_gm(60), q_r = 0.30, transition = VD, periode = PER_ENTIERE,
       region_spec = NULL, seuils_spec = c(121.8203129, 129.4128069)),
  list(id = 6L, nom = "décès, x = 100, F, période censurée au 2016-09-13 (c = 183/365)", x = 100, pH = 0,
       loi = loi(rachat = NUL), q_d = 0.6 * q_gm(100), q_r = 0, transition = VD, periode = PER_CENSUREE,
       region_spec = c(9192, 9986), seuils_spec = c(466.6703469, 473.2070948)))

T_C1 <- proc.time()[["elapsed"]]
for (co in C1) {
  graine <- 20261610L + co$id
  md <- modele_jour(mu_de_q(co$q_d), mu_de_q(co$q_r), L)
  J <- as.numeric(co$periode$fin + 1 - E1)              # jours observés de la cellule (365 ou 183)
  cc <- J / L
  at <- atomes_poisson(md, J)
  verifier(sprintf("C1.%d : atomes de X = D - mu* E (valeur calculée), somme des p = 1 et E[X] = 0 à 1e-13", co$id),
           abs(sum(at$p) - 1) < 1e-13 && abs(sum(at$p * at$x)) < 1e-13)
  s_hi <- seuil_exponentiel(at, N1, A_C1 / 2)
  s_lo <- seuil_exponentiel(list(p = at$p, x = -at$x), N1, A_C1 / 2)
  verifier(sprintf("C1.%d Poisson : seuils (t_lo ; t_hi) = (%.7f ; %.7f) de la spécification à 1e-6 (valeur calculée)",
                   co$id, co$seuils_spec[1], co$seuils_spec[2]),
           proche(c(s_lo$brut, s_hi$brut), co$seuils_spec, rel = 0, abs = 1e-6))
  verifier(sprintf("C1.%d Poisson : seuils arrondis vers l'extérieur à 1e-6 (%.6f ; %.6f), borne exponentielle recalculée en t <= alpha_k/2 dans chaque queue",
                   co$id, s_lo$t, s_hi$t),
           s_lo$t >= s_lo$brut && s_hi$t >= s_hi$brut &&
             s_lo$log_borne <= log(A_C1 / 2) && s_hi$log_borne <= log(A_C1 / 2))

  t0 <- proc.time()[["elapsed"]]
  sim <- simuler(co$loi, list(mode = "age_exact", n = N1, proportion_hommes = co$pH, age = co$x,
                              date_entree = E1), co$periode, graine)
  x <- construire_table(sim, if (identical(co$transition, RA)) ROLES_CONTRAT else ROLES,
                        plan_constat(co$periode, transition = co$transition))
  duree <- duree_depuis(t0)
  r <- x$etapes$brut$resultat
  p <- r$poisson[r$poisson$age == co$x, ]
  b <- r$binomial[r$binomial$age == co$x, ]
  km <- r$kaplan_meier[r$kaplan_meier$age == co$x, ]

  verifier(sprintf("C1.%d Poisson : identité (exacte) taux = evenements / exposition_centrale à 1e-13", co$id),
           proche(p$taux, p$evenements / p$exposition_centrale, rel = 1e-13, abs = 0))
  U <- p$evenements - md$mu_etoile * p$exposition_centrale
  ok <- isTRUE(U >= -s_lo$t && U <= s_hi$t)
  noter(sprintf("C1.%d Poisson", co$id), ok, A_C1, A_C1, "borne exacte")
  ligne <- sprintf(paste("constat C1.%d Poisson, %s : U = D - mu* E^c = %.4f dans [-%.6f, %.6f]",
                         "(%.3f à %.3f écarts-types ; n = %g, mu* = %.12g, mu*/mu - 1 = %.3e, biais de T1 ;",
                         "alpha_k = %g, fausse alarme <= alpha_k, borne exacte ; graine %d ; %.1f s)"),
                   co$id, co$nom, U, s_lo$t, s_hi$t, -s_lo$t / s_hi$sd, s_hi$t / s_hi$sd, N1, md$mu_etoile,
                   md$mu_etoile / mu_de_q(co$q_d) - 1, A_C1, graine, duree)
  ecrire(ligne); verifier(ligne, ok)

  if (co$q_r == 0) {
    pic <- -expm1(-mu_de_q(co$q_d) * cc)                # P(transition dans la fenêtre), exact
    cible <- pic / (cc * (1 - pic) + pic)               # limite « cellule » ; = q si c = 1
    lo <- stats::qbinom(A_C1 / 2, N1, pic); hi <- stats::qbinom(1 - A_C1 / 2, N1, pic)
    fa <- stats::pbinom(lo - 1, N1, pic) + stats::pbinom(hi, N1, pic, lower.tail = FALSE)
    verifier(sprintf("C1.%d binomial : région [%d, %d] = région de la spécification (valeur calculée)", co$id, lo, hi),
             identical(c(lo, hi), co$region_spec))
    q_cel <- function(d) d / ((N1 - d) * cc + d)
    # Région sur D (exacte), lue sur le taux : q dans [q_cel(lo), q_cel(hi)]
    # (q_cel(d) = d/n si c = 1), q_cel croissante.
    verifier(sprintf("C1.%d binomial : D = %d dans [%d, %d]", co$id, b$evenements, lo, hi),
             b$evenements >= lo && b$evenements <= hi)
    # Tolérance 1e-13 vers l'extérieur : écart flottant E^i/n - 1 <= 3,85e-14, bornes voisines distantes >= 5,3e-5 relatif (audit).
    ok <- isTRUE(b$taux >= q_cel(lo) * (1 - 1e-13) && b$taux <= q_cel(hi) * (1 + 1e-13))
    noter(sprintf("C1.%d binomial", co$id), ok, A_C1, fa, "exacte")
    ligne <- sprintf(paste("constat C1.%d binomial, %s : D = %d dans [%d, %d] (D ~ Bin(%g, %.10g)) ;",
                           "q = %.10g dans [%.10g, %.10g], cible %.10g (loi vraie %.10g, écart %+.2f %%) ;",
                           "alpha_k = %g, fausse alarme exacte %.4g ; graine %d ; %.1f s)"),
                     co$id, co$nom, b$evenements, lo, hi, N1, pic, b$taux, q_cel(lo), q_cel(hi), cible, co$q_d,
                     100 * (cible / co$q_d - 1), A_C1, fa, graine, duree)
    ecrire(ligne); verifier(ligne, ok)
    if (J == L) {
      verifier(sprintf("C1.%d : identités (exactes) E^i = n à 1e-13 relatif et Kaplan-Meier = binomial à 1e-13 (A.10 de #15)", co$id),
               isTRUE(proche(b$exposition_initiale, N1, rel = 1e-13, abs = 0)) &&
                 isTRUE(proche(km$taux, b$taux, rel = 1e-13, abs = 0)))
    } else {
      ex <- x$etapes$exposition$resultat$exposition
      ex <- ex[ex$age == co$x, ]
      verifier("C1.6 : identité (exacte) taux binomial = D/((n - D) c + D), définition « cellule », à 1e-13",
               proche(b$taux, q_cel(b$evenements), rel = 1e-13, abs = 0))
      verifier("C1.6 : identité (exacte) exposition_centrale + complement_initial_fenetre = n c, définition « fenêtre », à 1e-13",
               proche(ex$exposition_centrale + ex$complement_initial_fenetre, N1 * cc, rel = 1e-13, abs = 0))
      verifier("C1.6 : Kaplan-Meier à l'état ensemble_a_risque_vide (refus d'extrapoler au-delà de la censure)",
               identical(km$etat, "ensemble_a_risque_vide"))
      # Témoin de puissance : la définition « fenêtre » q = D/(n c), de
      # limite pi_c/c, sort de la région ; puissance exacte par sommation
      # sur Bin(n, pi_c).
      d <- 0:N1; q_fen <- d / (N1 * cc)
      puissance <- 1 - sum(stats::dbinom(d, N1, pic)[q_fen >= q_cel(lo) & q_fen <= q_cel(hi)])
      q_fen_obs <- b$evenements / (N1 * cc)
      ligne <- sprintf(paste("témoin de puissance C1.6 : définition « fenêtre » q = D/(n c) = %.10g hors de",
                             "[%.10g, %.10g] (limite %.5f contre cible %.5f) ; puissance exacte %.6f"),
                       q_fen_obs, q_cel(lo), q_cel(hi), pic / cc, cible, puissance)
      ecrire(ligne)
      verifier(ligne, (q_fen_obs < q_cel(lo) || q_fen_obs > q_cel(hi)) && puissance >= 0.999999)
    }
  } else {
    zc <- stats::qnorm(A_C1 / 2, lower.tail = FALSE)
    z <- (stats::qlogis(km$taux) - stats::qlogis(md$q_km)) * km$taux * (1 - km$taux) / sqrt(km$variance)
    ok <- isTRUE(abs(z) <= zc)
    noter("C1.5 Kaplan-Meier", ok, A_C1, NA_real_, "asymptotique")
    ligne <- sprintf(paste("constat C1.5 Kaplan-Meier, %s : q = %.10g, cible q* = %.12g (q*/q_net - 1 = %.3e) ;",
                           "z logit = %.4f dans [-%.4f, %.4f] (n = %g ; alpha_k = %g, fausse alarme nominale,",
                           "asymptotique ; graine %d ; %.1f s)"),
                     co$nom, km$taux, md$q_km, md$q_km / md$q_net - 1, z, zc, zc, N1, A_C1, graine, duree)
    ecrire(ligne); verifier(ligne, ok)
  }
}
ecrire(sprintf("C1 : durée %.1f s", duree_depuis(T_C1)))

# --- C2 : portefeuille avec troncature, censure et rachat concurrent (A.2) -----------------------
#
# Mode uniforme, n = 3.10^5, p_H = 0,5, âges 20 à 75, origine 2005-01-01,
# période 2015-2024, segmentation sexe. Sélection a priori, sur
# l'exposition seule : état defini pour Poisson et Kaplan-Meier, lambda =
# mu* E^c >= 30 (mu* à L = 365 ; écart à L = 366 <= 9,6.10^-7 relatif).
# K = 83 cellules sous la graine déclarée (écrit en dur, vérifié). Une
# cellule sélectionnée où q vaut 0 ou 1 compte comme un rejet.
# Par cellule (deux familles, a = 1,5.10^-4/K) : Poisson D dans
# [qpois(a/2, lambda), qpois(1 - a/2, lambda)], fausse alarme exacte à E^c
# fixe, asymptotique sous le modèle de survie ; Kaplan-Meier |z| <=
# qnorm(1 - a/2), cible q_net, asymptotique. Globaux (alpha_k = 10^-4) :
# Poisson signé sum(D - lambda)/sqrt(sum((1 + mu*/365) lambda)), variance
# exacte (accroissements de martingale) ; Kaplan-Meier sum(z)/sqrt(K) ;
# asymptotiques. Pas de constat binomial sur ce jeu (A.2).

K_SPEC <- 83L
G_C2 <- 20261621L
PER_C2 <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
t0 <- proc.time()[["elapsed"]]
sim <- simuler(loi(), list(mode = "uniforme", n = 3e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
                           origine = D("2005-01-01")), PER_C2, G_C2)
x <- construire_table(sim, ROLES, plan_constat(PER_C2, estimateurs = c("poisson", "kaplan_meier"),
                                               segmentation = "sexe"))
duree <- duree_depuis(t0)
p <- x$etapes$brut$resultat$poisson; k <- x$etapes$brut$resultat$kaplan_meier
verifier("C2 : tables de Poisson et de Kaplan-Meier sur les mêmes cellules, dans le même ordre (sexe, âge)",
         identical(p$sexe, k$sexe) && identical(p$age, k$age))
f <- ifelse(p$sexe == "H", 1, 0.6)
mu_r <- mu_de_q(0.05)
cible_c2 <- function(s) {
  q_x <- if (s == 0) GM$qx[match(p$age, GM$age)] else q_gm_decale(p$age, s)
  mu_d <- mu_de_q(pmin(f * q_x, 1)); m <- mu_d + mu_r
  list(mu_etoile = mu_d / m * L * expm1(m / L), q = -expm1(-mu_d))
}
c0 <- cible_c2(0)
lam <- c0$mu_etoile * p$exposition_centrale
sel <- p$etat == "defini" & k$etat == "defini" & lam >= 30
K <- sum(sel)
verifier(sprintf("C2 : K = %d cellules sélectionnées sur l'exposition = %d de la spécification", K, K_SPEC),
         identical(K, K_SPEC))
a_c <- A_C2_CELLULES / K_SPEC
lo <- stats::qpois(a_c / 2, lam); hi <- stats::qpois(a_c / 2, lam, lower.tail = FALSE)
fa_fixe <- stats::ppois(lo - 1, lam) + stats::ppois(hi, lam, lower.tail = FALSE)
defp <- p$etat == "defini"
verifier(sprintf("C2 Poisson : identité (exacte) taux = evenements / exposition_centrale à 1e-13 sur les %d cellules définies", sum(defp)),
         proche(p$taux[defp], p$evenements[defp] / p$exposition_centrale[defp], rel = 1e-13, abs = 0))
ok_p <- p$evenements >= lo & p$evenements <= hi
z_cell <- function(q) (stats::qlogis(k$taux) - stats::qlogis(q)) * k$taux * (1 - k$taux) / sqrt(k$variance)
z_k <- z_cell(c0$q)
zc <- stats::qnorm(a_c / 2, lower.tail = FALSE)
ok_k <- !is.na(z_k) & k$taux > 0 & k$taux < 1 & abs(z_k) <= zc   # q dans {0, 1} : rejet
for (i in which(sel)) {
  noter(sprintf("C2 Poisson %s %d", p$sexe[i], p$age[i]), ok_p[i], a_c, NA_real_, "asymptotique")
  noter(sprintf("C2 Kaplan-Meier %s %d", p$sexe[i], p$age[i]), ok_k[i], a_c, NA_real_, "asymptotique")
}
ages_sel <- paste(vapply(c("H", "F"), function(g) sprintf("%s de %d à %d", g, min(p$age[sel & p$sexe == g]),
                                                         max(p$age[sel & p$sexe == g])), character(1L)), collapse = ", ")
ligne <- sprintf(paste("constat C2 Poisson par cellule (n = 3e5 ; %d cellules, %s ; %d décès",
                       "sélectionnés) : %d rejet(s) ; alpha_k = %.4g par cellule, %g pour la famille ; fausse alarme",
                       "nominale, asymptotique ; exacte à E^c fixe : somme %.4g ; graine %d ; %.1f s)"),
                 K, ages_sel,
                 sum(p$evenements[sel]), sum(!ok_p[sel]), a_c, A_C2_CELLULES, sum(fa_fixe[sel]), G_C2, duree)
ecrire(ligne); verifier(ligne, all(ok_p[sel]))
ligne <- sprintf(paste("constat C2 Kaplan-Meier par cellule : %d rejet(s), |z| max %.4f, seuil %.4f",
                       "(cible q_net ; alpha_k = %.4g par cellule, %g pour la famille ; fausse alarme nominale,",
                       "asymptotique ; graine %d)"),
                 sum(!ok_k[sel]), max(abs(z_k[sel])), zc, a_c, A_C2_CELLULES, G_C2)
ecrire(ligne); verifier(ligne, all(ok_k[sel]))
zg <- stats::qnorm(A_C2_GLOBAL / 2, lower.tail = FALSE)
z_global_p <- function(c, lam) sum((p$evenements - lam)[sel]) / sqrt(sum(((1 + c$mu_etoile / L) * lam)[sel]))
z_global_k <- function(c) sum(z_cell(c$q)[sel]) / sqrt(K)
zP <- z_global_p(c0, lam); zK <- z_global_k(c0)
ok <- isTRUE(abs(zP) <= zg); noter("C2 Poisson global", ok, A_C2_GLOBAL, NA_real_, "asymptotique")
ligne <- sprintf(paste("constat C2 Poisson global, signé : z = %.4f dans [-%.4f, %.4f] (%d cellules ; alpha_k = %g,",
                       "fausse alarme nominale, asymptotique ; graine %d)"), zP, zg, zg, K, A_C2_GLOBAL, G_C2)
ecrire(ligne); verifier(ligne, ok)
ok <- isTRUE(abs(zK) <= zg); noter("C2 Kaplan-Meier global", ok, A_C2_GLOBAL, NA_real_, "asymptotique")
ligne <- sprintf(paste("constat C2 Kaplan-Meier global : z = %.4f dans [-%.4f, %.4f] (%d cellules ; alpha_k = %g,",
                       "fausse alarme nominale, asymptotique ; graine %d)"), zK, zg, zg, K, A_C2_GLOBAL, G_C2)
ecrire(ligne); verifier(ligne, ok)
# Témoin : constat sous la graine 20261621, statistiques globales
# recalculées sur les mêmes données avec une cible décalée d'un an d'âge
# (loi de la cellule x + 1) ; les deux globaux doivent rejeter. Ce n'est
# pas une puissance : la puissance asymptotique relève de A.4 et n'est pas
# mesurée ici.
c1 <- cible_c2(1)
zP1 <- z_global_p(c1, c1$mu_etoile * p$exposition_centrale); zK1 <- z_global_k(c1)
ligne <- sprintf(paste("témoin C2, cible décalée de s = 1 an (constat sous la graine %d, mêmes données, statistiques",
                       "recalculées ; pas une puissance) : z Poisson global %.3f, z Kaplan-Meier global %.3f,",
                       "hors de [-%.4f, %.4f]"), G_C2, zP1, zK1, zg, zg)
ecrire(ligne); verifier(ligne, abs(zP1) > zg && abs(zK1) > zg)

# --- C3 : couverture des intervalles, réplications par blocs (A.2) -------------------------------
#
# Les têtes sont indépendantes et de même loi : une simulation de R m
# têtes découpée en R blocs de m têtes (colonne segment) donne R
# réplications indépendantes, sous l'hypothèse usuelle sur le générateur.
# Un intervalle NA compte comme non couvrant, dans la mesure comme dans la
# référence.

dans <- function(lo, hi, v) !is.na(lo) & !is.na(hi) & lo <= v & v <= hi
z95 <- stats::qnorm((1 - NIVEAU) / 2, lower.tail = FALSE)
al <- 1 - NIVEAU

# C3a : blocs de cohortes (décès 100 F, m = 50, R = 2000), référence exacte.
# Couvertures exactes : binomial par sommation sur D ~ Bin(m, q) ; Poisson
# par sommation sur D et convolution de la loi de S = somme des jours de
# décès K_j (K | décès : géométrique tronquée sur 0 ... L-1), E^c = m - D +
# S/L ; convolution par fft complétée de zéros jusqu'à nextn() (sans ce
# complément, une longueur à grands facteurs premiers a dépassé 600 s,
# mesuré par actuary). N_couverts ~ Bin(R, C) exactement ; région
# bilatérale exacte à alpha_k = 3.10^-5, fausse alarme exacte.
M_A <- 50L; R_A <- 2000L; G_C3A <- 20261631L; X_A <- 100L
q_a <- 0.6 * q_gm(X_A); mu_a <- mu_de_q(q_a); md_a <- modele_jour(mu_a, 0, L)
COUV_SPEC <- c(0.974129060872, 0.958230588612, 0.968699258108, 0.955857903542)
REGIONS_C3A_SPEC <- list(c(1916, 1975), c(1877, 1951), c(1902, 1967), c(1871, 1947))
t0 <- proc.time()[["elapsed"]]
dd <- 0:M_A; pd <- stats::dbinom(dd, M_A, q_a)
borne_exacte_b <- function(d) c(if (d == 0) 0 else stats::qbeta(al / 2, d, M_A - d + 1),
                                if (d == M_A) 1 else stats::qbeta(al / 2, d + 1, M_A - d, lower.tail = FALSE))
borne_logit_b <- function(d) if (d == 0 || d == M_A) c(NA, NA) else
  stats::plogis(stats::qlogis(d / M_A) + c(-1, 1) * z95 * sqrt(1 / d + 1 / (M_A - d)))
couverture_b <- function(borne) sum(vapply(dd, function(j) { b <- borne(j); isTRUE(b[1] <= q_a && q_a <= b[2]) },
                                           logical(1L)) * pd)
pk <- md_a$s^(0:(L - 1)) * md_a$h_d; pk <- pk / sum(pk)
convoler <- function(a, b) {
  n <- length(a) + length(b) - 1L; N <- nextn(n)
  v <- Re(fft(fft(c(a, numeric(N - length(a)))) * fft(c(b, numeric(N - length(b)))), inverse = TRUE)) / N
  pmax(v[seq_len(n)], 0)
}
couverture_p <- function(cible) {
  exacte <- log_ <- 0; pS <- 1
  for (j in dd) {
    if (j > 0) pS <- convoler(pS, pk)
    Ec <- M_A - j + (seq_along(pS) - 1) / L
    lo <- if (j == 0) 0 else stats::qgamma(al / 2, j); hi <- stats::qgamma(al / 2, j + 1, lower.tail = FALSE)
    exacte <- exacte + pd[j + 1] * sum(pS[cible * Ec >= lo & cible * Ec <= hi])
    if (j > 0) log_ <- log_ + pd[j + 1] * sum(pS[cible * Ec >= j * exp(-z95 / sqrt(j)) & cible * Ec <= j * exp(z95 / sqrt(j))])
  }
  c(exacte, log_)
}
couv <- c(couverture_b(borne_exacte_b), couverture_b(borne_logit_b), couverture_p(mu_a))
duree_ref <- duree_depuis(t0)
verifier(sprintf("C3a : couvertures exactes (valeur calculée, %.2f s) = spécification à 5e-10 : %s", duree_ref,
                 paste(sprintf("%.12f", couv), collapse = " ; ")),
         proche(couv, COUV_SPEC, rel = 0, abs = 5e-10))
t0 <- proc.time()[["elapsed"]]
sim <- simuler(loi(rachat = NUL), list(mode = "age_exact", n = M_A * R_A, proportion_hommes = 0, age = X_A,
                                       date_entree = E1), PER_ENTIERE, G_C3A)
sim$segment <- (sim$id_tete - 1L) %/% M_A + 1L
x <- construire_table(sim, c(ROLES, segment = "segment"), plan_constat(PER_ENTIERE, segmentation = "segment"))
duree <- duree_depuis(t0)
r <- x$etapes$brut$resultat
b <- r$binomial[r$binomial$age == X_A, ]; p <- r$poisson[r$poisson$age == X_A, ]
km <- r$kaplan_meier[r$kaplan_meier$age == X_A, ]
verifier(sprintf("C3a : %d blocs dans chaque table (binomial, Poisson, Kaplan-Meier)", R_A),
         nrow(b) == R_A && nrow(p) == R_A && nrow(km) == R_A)
verifier("C3a : identité (exacte) bornes de Kaplan-Meier = bornes logit binomiales sur les 2000 blocs, à 1e-13 (A.10 de #15)",
         isTRUE(proche(km$borne_inf_asymptotique, b$borne_inf_asymptotique, rel = 1e-13, abs = 0)) &&
           isTRUE(proche(km$borne_sup_asymptotique, b$borne_sup_asymptotique, rel = 1e-13, abs = 0)))
n_couv <- c(sum(dans(b$borne_inf_exacte, b$borne_sup_exacte, q_a)),
            sum(dans(b$borne_inf_asymptotique, b$borne_sup_asymptotique, q_a)),
            sum(dans(p$borne_inf_exacte, p$borne_sup_exacte, mu_a)),
            sum(dans(p$borne_inf_asymptotique, p$borne_sup_asymptotique, mu_a)))
demi <- c(mean(b$borne_sup_exacte - b$borne_inf_exacte, na.rm = TRUE),
          mean(b$borne_sup_asymptotique - b$borne_inf_asymptotique, na.rm = TRUE),
          mean(p$borne_sup_exacte - p$borne_inf_exacte, na.rm = TRUE),
          mean(p$borne_sup_asymptotique - p$borne_inf_asymptotique, na.rm = TRUE)) / 2
noms_a <- c("binomial exact", "binomial logit", "Poisson exact", "Poisson log")
for (i in 1:4) {
  lo <- stats::qbinom(A_C3A / 2, R_A, couv[i]); hi <- stats::qbinom(1 - A_C3A / 2, R_A, couv[i])
  fa <- stats::pbinom(lo - 1, R_A, couv[i]) + stats::pbinom(hi, R_A, couv[i], lower.tail = FALSE)
  verifier(sprintf("C3a %s : région [%d, %d] = région de la spécification (valeur calculée)", noms_a[i], lo, hi),
           identical(c(lo, hi), REGIONS_C3A_SPEC[[i]]))
  ok <- n_couv[i] >= lo && n_couv[i] <= hi
  noter(paste("C3a", noms_a[i]), ok, A_C3A, fa, "exacte")
  ligne <- sprintf(paste("constat C3a %s (%d blocs de %d têtes, E[D] = %.3f) : couverts %d / %d = %.4f dans [%d, %d] ;",
                         "couverture exacte %.6f ; erreur Monte-Carlo %.4f ; erreur d'estimation (demi-largeur",
                         "moyenne) %.5f sur %s = %.5f ; alpha_k = %g, fausse alarme exacte %.4g ; graine %d ; %.1f s)"),
                   noms_a[i], R_A, M_A, M_A * q_a, n_couv[i], R_A, n_couv[i] / R_A, lo, hi, couv[i],
                   sqrt(couv[i] * (1 - couv[i]) / R_A), demi[i], if (i <= 2) "q" else "mu",
                   if (i <= 2) q_a else mu_a, A_C3A, fa, G_C3A, duree)
  ecrire(ligne); verifier(ligne, ok)
}

# C3b : blocs de portefeuille tronqué (H, âges 79 à 81, origine = début =
# 2015-01-01, période 2015-2016, rachat 0,05 ; m = 600, R = 2000), cellules
# 80 et 81, intervalles Poisson exact, Poisson log, Kaplan-Meier logit.
# Hypothèse nulle composite : couverture >= 0,95 - eta, eta = 0,02 déclaré
# par actuary (jugement, sans source). Région N >= qbinom(alpha_k, R,
# 0,93) = 1811, alpha_k = 2.10^-5 ; fausse alarme <= pbinom(1810, R, 0,93),
# borne exacte pour toute couverture vraie >= 0,93 (monotonie).
M_B <- 600L; R_B <- 2000L; G_C3B <- 20261641L; ETA <- 0.02; C0 <- NIVEAU - ETA
SEUIL_C3B_SPEC <- 1811
PER_C3B <- list(debut = D("2015-01-01"), fin = D("2016-12-31"))
seuil <- stats::qbinom(A_C3B, R_B, C0); fa_b <- stats::pbinom(seuil - 1, R_B, C0)
verifier(sprintf("C3b : seuil qbinom(%g, %d, %.2f) = %d de la spécification ; fausse alarme %.4g <= alpha_k (valeur calculée)",
                 A_C3B, R_B, C0, seuil, fa_b),
         identical(seuil, SEUIL_C3B_SPEC) && fa_b <= A_C3B)
t0 <- proc.time()[["elapsed"]]
sim <- simuler(loi(), list(mode = "uniforme", n = M_B * R_B, proportion_hommes = 1, age_min = 79, age_max = 81,
                           origine = D("2015-01-01")), PER_C3B, G_C3B)
sim$segment <- (sim$id_tete - 1L) %/% M_B + 1L
x <- construire_table(sim, c(ROLES, segment = "segment"),
                      plan_constat(PER_C3B, estimateurs = c("poisson", "kaplan_meier"), segmentation = "segment"))
duree <- duree_depuis(t0)
r <- x$etapes$brut$resultat
noms_b <- c("Poisson exact", "Poisson log", "Kaplan-Meier logit")
for (age in 80:81) {
  p <- r$poisson[r$poisson$age == age, ]; km <- r$kaplan_meier[r$kaplan_meier$age == age, ]
  qx <- q_gm(age); mu <- mu_de_q(qx)
  verifier(sprintf("C3b âge %d : %d blocs dans chaque table (Poisson, Kaplan-Meier)", age, R_B),
           nrow(p) == R_B && nrow(km) == R_B)
  n_couv <- c(sum(dans(p$borne_inf_exacte, p$borne_sup_exacte, mu)),
              sum(dans(p$borne_inf_asymptotique, p$borne_sup_asymptotique, mu)),
              sum(dans(km$borne_inf_asymptotique, km$borne_sup_asymptotique, qx)))
  demi <- c(mean(p$borne_sup_exacte - p$borne_inf_exacte, na.rm = TRUE),
            mean(p$borne_sup_asymptotique - p$borne_inf_asymptotique, na.rm = TRUE),
            mean(km$borne_sup_asymptotique - km$borne_inf_asymptotique, na.rm = TRUE)) / 2
  for (i in 1:3) {
    ok <- n_couv[i] >= seuil
    noter(sprintf("C3b %d %s", age, noms_b[i]), ok, A_C3B, fa_b, "borne exacte")
    cv <- n_couv[i] / R_B
    ligne <- sprintf(paste("constat C3b âge %d %s (%d blocs de %d têtes, D moyen %.2f) : couverts %d / %d = %.4f,",
                           "région N >= %d (H0 couverture >= %.2f) ; erreur Monte-Carlo estimée (couverture observée) %.4f ;",
                           "erreur d'estimation",
                           "(demi-largeur moyenne) %.5f sur %s = %.5f ; alpha_k = %g, fausse alarme <= %.4g,",
                           "borne exacte ; graine %d ; %.1f s)"),
                     age, noms_b[i], R_B, M_B, mean(p$evenements), n_couv[i], R_B, cv, seuil, C0,
                     sqrt(cv * (1 - cv) / R_B), demi[i], if (i <= 2) "mu" else "q", if (i <= 2) mu else qx,
                     A_C3B, fa_b, G_C3B, duree)
    ecrire(ligne); verifier(ligne, ok)
  }
}

# --- bilan : fausse alarme familiale (A.3) --------------------------------------------------------
#
# Borne de l'union pondérée : fausse alarme familiale <= sum(alpha_k), sans
# hypothèse d'indépendance (les tests d'une même cohorte sont fortement
# dépendants : aucune valeur « sous indépendance » n'est donnée).
exact <- !is.na(CONSTATS$fa)
ligne <- sprintf(paste("bilan des constats de #16 : %d tests, %d rejet(s) ; fausse alarme familiale <= sum(alpha_k) =",
                       "%.4g <= alpha = %g (borne de l'union pondérée, sans hypothèse d'indépendance) ; dont exacte",
                       "ou bornée exactement %.4g (%d tests : C1 binomial, C1 Poisson, C3a, C3b) et nominale",
                       "asymptotique %.4g (%d tests : C1.5 Kaplan-Meier, C2)"),
                 nrow(CONSTATS), sum(!CONSTATS$accepte), sum(CONSTATS$alpha_k), ALPHA, sum(CONSTATS$fa[exact]),
                 sum(exact), sum(CONSTATS$alpha_k[!exact]), sum(!exact))
ecrire(ligne)
verifier(ligne, nrow(CONSTATS) == 190L && sum(CONSTATS$alpha_k) <= ALPHA &&
           isTRUE(proche(sum(CONSTATS$alpha_k), 9.8e-4, rel = 1e-12, abs = 0)) &&
           sum(exact) == 21L && sum(!exact) == 169L &&
           isTRUE(proche(sum(CONSTATS$alpha_k[!exact]), 5.2e-4, rel = 1e-12, abs = 0)))
