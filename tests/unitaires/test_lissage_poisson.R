# -----------------------------------------------------------------------------
#  tests/unitaires/test_lissage_poisson.R : étape `lissage`, méthode
#  wh_poisson (#225, commit (3) de B19 ; corps de #225, décisions D1 à D13 du
#  mainteneur du 11/11/2026, M413 ; spécification de forme (0')(b)
#  d'actuary-approfondi, commentaire 6799616343, M416 ; ADR 0017, points 2,
#  3, 5.5, 8 et 9)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité mathématique : critères A1 à A5 du corps de #225 sur vecteurs
#    fixés (sans aléa), tolérances de la spécification ;
#  - recoupement : B1 (stats::glm, lambda = 1e12) ; C1 et C2 (WH 2.0.0, hors
#    CI, valeurs écrites en dur) ;
#  - valeur calculée à la main : catalogue, motifs, modules, messages et
#    details des refus, structure de etapes$lissage ;
#  - identité exacte : table lissée recalculée par lisser_poisson() sur les
#    entrées de l'étape, somme des leviers = ddl effectifs, aller-retour du
#    plan, rejeu de la fiche.
#  A2 porte sur lambda <= 1e6 : le résidu formé d - m^ - lambda K'K theta^
#  porte l'arrondi de theta^ multiplié par lambda K'K, borné par
#  eps lambda ||K'K||_inf ||theta^||_inf (eps = .Machine$double.eps ;
#  ||K'K||_inf = 2^(2z), 256 pour z = 4) ; rapportée à max(d) sur le jeu
#  plein, z = 4, cette borne vaut 1,4e-8 à lambda = 1e6, sous la tolérance
#  1e-6, et 1,4e-5 à lambda = 1e9 (résidu mesuré 2,3e-6), au-dessus : la
#  limite lambda <= 1e6 tient à l'arithmétique, non à l'algorithme. A3 :
#  z = 1 à 4, lambda = 1 et 1e3, contre une référence QR indépendante
#  (qr(LAPACK = TRUE), pivotage de colonnes de LAPACK) ; la référence par
#  solve(), gardée pour z = 1 et 2, perd sa précision quand le
#  conditionnement croît (mesuré à lambda = 1e3 : 5,0e-12 pour z = 3,
#  1,5e-11 pour z = 4). C1 : WH
#  appelé avec tol_dev = 1e-12 (tests/contre_verification/wh_lissage_poisson.R).
#  Jeux sans aléa : vecteurs et agrégats écrits à la main (aucun tirage) ;
#  graine du plan des cas de validation 22011206 (étape aléatoire, aucun
#  tirage sous lissee ; plage de la porteuse B19, k = 0, M416).
# -----------------------------------------------------------------------------

lp <- function(...) tablexp:::lisser_poisson(...)
ldp <- function(...) tablexp:::log_det_penalite(...)
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
# Refus attendu : module, motif, message (NULL : non comparé), details
# (NULL : non comparés) ; TRUE ou le texte de l'écart.
controle <- function(r, module, motif, message = NULL, details = NULL) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s »", r$module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!is.null(message) && !identical(conditionMessage(r), message)) return(sprintf("message « %s »", conditionMessage(r)))
  if (!is.null(details) && !identical(r$details, details)) return("details différents")
  TRUE
}
tous <- function(l) { k <- vapply(l, isTRUE, NA); if (all(k)) TRUE else paste(unlist(l[!k]), collapse = " ; ") }

# --- vecteurs fixés (sans aléa) -------------------------------------------------

X <- 30:70
N <- length(X)
E <- round(1500 * exp(-0.015 * (X - 30)), 2)
D <- round(E * 0.001 * exp(0.085 * (X - 30)) * (1 + 0.15 * sin(X)))
TROUS <- X %in% c(45, 52)               # deux cellules intérieures sans exposition
E0 <- ifelse(TROUS, 0, E); D0 <- ifelse(TROUS, 0, D)
DS <- round(E * 0.0002 * exp(0.085 * (X - 30)))   # 8 cellules exposées sans décès
L_GRILLE <- c(1e-6, 1, 1e3, 1e6, 1e9, 1e12)
JEUX <- list(plein = list(E = E, D = D), trous = list(E = E0, D = D0))
K_ <- function(z) diff(diag(N), differences = z)

# --- A. Identités sur vecteurs fixés (corps de #225, critères A1 à A6) -----------

verifier("A1 conservation : |somme m^ - somme d| <= 1e-8 somme d (z = 1 à 4) et |somme x (m^ - d)| <= 1e-8 somme x d (z = 2 à 4), lambda = 1e-6, 1, 1e3, 1e6, 1e9, 1e12, avec et sans cellules sans exposition (identité mathématique)", {
  e <- unlist(lapply(JEUX, function(j) unlist(lapply(1:4, function(z) vapply(L_GRILLE, function(l) {
    r <- lp(j$D, j$E, l, z, X, ldp(N, z))
    if (!identical(r$statut, "produite")) return(Inf)
    m <- exp(r$theta) * j$E
    max(abs(sum(m) - sum(j$D)) / sum(j$D), if (z >= 2) abs(sum(X * (m - j$D))) / sum(X * j$D) else 0)
  }, 0)))))
  if (all(e <= 1e-8)) TRUE else sprintf("écart relatif max %.3g", max(e))
})
verifier("A1 résidus rendus : residu_conservation_0 = somme (m^ - d), residu_conservation_1 = somme x (m^ - d) (identité exacte)", {
  r <- lp(D0, E0, 1e3, 3, X, ldp(N, 3))
  m <- numeric(N); m[E0 > 0] <- exp(r$theta[E0 > 0]) * E0[E0 > 0]
  identical(r$residu_conservation_0, sum(m - D0)) && identical(r$residu_conservation_1, sum(X * (m - D0)))
})
verifier("A2 score : ||d - m^ - lambda K'K theta^||_inf <= 1e-6 max(d), lambda = 1e-6, 1, 1e3, 1e6, z = 1 à 4, avec et sans cellules sans exposition (identité mathématique)", {
  e <- unlist(lapply(JEUX, function(j) unlist(lapply(1:4, function(z) vapply(L_GRILLE[1:4], function(l) {
    r <- lp(j$D, j$E, l, z, X, ldp(N, z))
    m <- exp(r$theta) * j$E
    max(abs(j$D - m - l * crossprod(K_(z), K_(z) %*% r$theta))) / max(j$D)
  }, 0)))))
  if (all(e <= 1e-6)) TRUE else sprintf("écart rapporté à max(d) %.3g", max(e))
})
verifier("A3 première itération de Newton depuis theta_0 = ln(d / e_c) (d > 0 partout) = WH original de Biessy (y = ln(d / e_c), w = d), référence qr(rbind(diag(sqrt(d)), sqrt(lambda) K), LAPACK = TRUE) pour z = 1 à 4 et solve() pour z = 1 et 2 : écart <= 1e-12, lambda = 1 et 1e3 (identité mathématique)", {
  e <- unlist(lapply(1:4, function(z) vapply(c(1, 1e3), function(l) {
    t0 <- log(D / E)
    it <- tablexp:::iteration_pirls(t0, D, E, rep(TRUE, N), sqrt(l) * K_(z))
    ref_qr <- qr.coef(qr(rbind(diag(sqrt(D)), sqrt(l) * K_(z)), LAPACK = TRUE), c(sqrt(D) * t0, numeric(N - z)))
    max(abs(it$theta - ref_qr),
        if (z <= 2L) abs(it$theta - solve(diag(D) + l * crossprod(K_(z)), D * t0)) else 0)
  }, 0)))
  if (all(e <= 1e-12)) TRUE else sprintf("écart max %.3g", max(e))
})
verifier("A4 theta^ identique à 1e-8 près en relatif pour epsilon = 1e-8 et 1e-4 (8 cellules exposées sans décès), z = 1 à 4, lambda = 1, 1e3, 1e6 (identité mathématique)", {
  e <- unlist(lapply(1:4, function(z) vapply(c(1, 1e3, 1e6), function(l) {
    a <- lp(DS, E, l, z, X, ldp(N, z), 1e-8); b <- lp(DS, E, l, z, X, ldp(N, z), 1e-4)
    max(abs(a$theta - b$theta) / abs(a$theta))
  }, 0)))
  if (sum(DS == 0) == 8L && all(e <= 1e-8)) TRUE else sprintf("écart relatif max %.3g", max(e))
})
verifier("A5 LAML de lisser_poisson() = formule de Biessy, p. 19, recalculée en clair (determinant(), eigen(), pénalité somme (Delta^z theta)^2) : écart relatif <= 1e-9, z = 1 à 4, lambda = 1, 1e3, 1e6, 1e9, cellules sans exposition comprises (identité mathématique)", {
  e <- unlist(lapply(1:4, function(z) vapply(c(1, 1e3, 1e6, 1e9), function(l) {
    r <- lp(D0, E0, l, z, X, ldp(N, z))
    m <- exp(r$theta) * E0
    ld <- as.numeric(determinant(diag(m) + l * crossprod(K_(z)), logarithm = TRUE)$modulus)
    ev <- eigen(l * crossprod(K_(z)), symmetric = TRUE, only.values = TRUE)$values[seq_len(N - z)]
    f <- sum(D0 * r$theta - m) - (l * sum(diff(r$theta, differences = z)^2) + ld - sum(log(ev)) - z * log(2 * pi)) / 2
    abs(r$laml - f) / abs(f)
  }, 0)))
  if (all(e <= 1e-9)) TRUE else sprintf("écart relatif max %.3g", max(e))
})
verifier("A leviers m^_i V_ii : 0 exactement aux cellules sans exposition, > 0 ailleurs ; ddl_effectifs identical() à leur somme ; ddl dans ]z, n+] et décroissants en lambda (identité mathématique)", {
  r <- lp(D0, E0, 1e3, 2, X, ldp(N, 2))
  nu <- vapply(c(1, 1e2, 1e4, 1e6), function(l) lp(D0, E0, l, 2, X, ldp(N, 2))$ddl_effectifs, 0)
  all(r$levier[TROUS] == 0) && all(r$levier[!TROUS] > 0) && identical(r$ddl_effectifs, sum(r$levier)) &&
    all(diff(nu) < 0) && all(nu > 2 & nu <= sum(!TROUS))
})
verifier("A régularité, déviance pénalisée : regularite = somme_differences_carrees(theta^, z) ; deviance_penalisee = deviance_penalisee(theta^) (identité exacte)", {
  r <- lp(D0, E0, 1e3, 3, X, ldp(N, 3))
  identical(r$regularite, tablexp:::somme_differences_carrees(r$theta, 3)) &&
    identical(r$deviance_penalisee, tablexp:::deviance_penalisee(r$theta, D0, E0, E0 > 0, 1e3, 3))
})
verifier("A6 non convergence : décès à un seul âge extrême (70), z = 2 : lisser_poisson() rend statut non_convergent (aucun maximum fini)", {
  d <- numeric(N); d[N] <- 10
  identical(lp(d, E, 1e3, 2, X, ldp(N, 2))$statut, "non_convergent")
})
verifier("A décès à un seul âge intérieur (49), z = 2 : maximum fini, statut produite (constat de la forme de la non-existence)", {
  d <- numeric(N); d[20] <- 10
  identical(lp(d, E, 1e3, 2, X, ldp(N, 2))$statut, "produite")
})
verifier("A réponse de travail theta - 1 aux cellules exposées sans décès (réserve R-a d'actuary-approfondi) : jeu DS (8 cellules sans décès), z = 4, lambda = 1e-6 : statut produite (non_convergent avant la règle : m arrondi à 0, D / m = 0 / 0) ; LAML sur [1e-6, 1e8] : lambda^ intérieur (borne NA), 371 297,92 à 1e-6 près en relatif (valeur d'actuary-approfondi) ; un seul âge intérieur avec décès, z = 3, reste non_convergent (valeur calculée à la main)", {
  r <- lp(DS, E, 1e-6, 4, X, ldp(N, 4))
  ch <- tablexp:::choisir_lambda_poisson(DS, E, 4, X, c(1e-6, 1e8), ldp(N, 4))
  d <- numeric(N); d[20] <- 10
  tous(list(identical(r$statut, "produite") || sprintf("statut %s", r$statut),
            (is.null(ch$echec) && is.na(ch$borne)) || "lambda^ non intérieur ou échec",
            isTRUE(proche(ch$ajustement$lambda, 371297.92, rel = 1e-6, abs = 0)) ||
              sprintf("lambda^ = %.17g", ch$ajustement$lambda),
            identical(lp(d, E, 1e3, 3, X, ldp(N, 3))$statut, "non_convergent") || "z = 3 : statut produit"))
})
verifier("A lambda_a_la_borne (règle Q1 d'actuary) : lambda retenu = la borne déclarée elle-même, identical(), sans recherche de Brent ; borne inférieure : z = 1, [1e3, 1e12] ; borne supérieure : z = 2, [1e-6, 1e12] (point de Brent 969 106 641 493,46 retenu avant la règle) (identité exacte sur la règle)", {
  bas <- tablexp:::choisir_lambda_poisson(D, E, 1, X, c(1e3, 1e12), ldp(N, 1))
  haut <- tablexp:::choisir_lambda_poisson(D, E, 2, X, c(1e-6, 1e12), ldp(N, 2))
  tous(list(identical(bas$borne, "inferieure") || sprintf("borne %s", bas$borne),
            identical(bas$ajustement$lambda, 1e3) || sprintf("lambda = %.17g", bas$ajustement$lambda),
            identical(haut$borne, "superieure") || sprintf("borne %s", haut$borne),
            identical(haut$ajustement$lambda, 1e12) || sprintf("lambda = %.17g", haut$ajustement$lambda)))
})
verifier("A exactement z cellules exposées, toutes à décès > 0 : statut produite, interpolation (leviers des cellules exposées à 1 à 1e-10 près), z = 1 à 4", {
  ok <- vapply(1:4, function(z) {
    idx <- round(seq(1, N, length.out = max(z, 2L)))[seq_len(z)]
    if (z == 1L) idx <- 1L
    e <- numeric(N); d <- numeric(N); e[idx] <- E[idx]; d[idx] <- pmax(D[idx], 1)
    r <- lp(d, e, 1e3, z, X, ldp(N, z))
    identical(r$statut, "produite") && isTRUE(proche(r$levier[idx], rep(1, z), rel = 0, abs = 1e-10))
  }, NA)
  all(ok)
})

# --- B. Recoupement (stats) -------------------------------------------------------

verifier("B1 lambda = 1e12 : m^ contre stats::glm(d ~ poly(x, z - 1, raw = TRUE), offset = log(e_c), family = poisson) (epsilon 1e-14), écart relatif <= 1e-6 sur les valeurs ajustées, z = 1 à 4 (limite polynomiale, Biessy, p. 14 ; recoupement)", {
  ctl <- stats::glm.control(epsilon = 1e-14, maxit = 100)
  e <- vapply(1:4, function(z) {
    f <- if (z == 1) stats::fitted(stats::glm(D ~ 1, offset = log(E), family = stats::poisson, control = ctl))
         else stats::fitted(stats::glm(D ~ poly(X, z - 1, raw = TRUE), offset = log(E), family = stats::poisson,
                                       control = ctl))
    max(abs(exp(lp(D, E, 1e12, z, X, ldp(N, z))$theta) * E - unname(f)) / unname(f))
  }, 0)
  if (all(e <= 1e-6)) TRUE else sprintf("écart relatif max %.3g", max(e))
})

# --- C. Catalogues (ADR 0017, point 3.4 ; spécification de forme, § 1) ------------

verifier("C METHODES_LISSAGE, critères admis, estimateur et échelle : wh_poisson lit poisson, pénalité sur ln_mu, declare et laml admis", {
  identical(tablexp:::METHODES_LISSAGE, c("wh_quadratique", "wh_poisson")) &&
    identical(tablexp:::CRITERES_ADMIS_LISSAGE, list(wh_quadratique = "declare", wh_poisson = c("declare", "laml"))) &&
    identical(tablexp:::ESTIMATEUR_LISSAGE[["wh_poisson"]], "poisson") &&
    identical(tablexp:::ECHELLE_PENALITE[["wh_poisson"]], "ln_mu")
})
verifier("C PARAMETRES_ETAPES$lissage : niveau (selon methode = wh_poisson) après ponderation, intervalle_lissage (selon critere_lissage = laml) après parametre_lissage", {
  t <- tablexp:::PARAMETRES_ETAPES$lissage
  identical(t$parametre, c("methode", "plage_ages", "ordre_difference", "critere_lissage", "ponderation", "niveau",
                           "parametre_lissage", "intervalle_lissage")) &&
    identical(t$type, c("character", "double", "double", "character", "character", "double", "double", "double")) &&
    identical(t$selon, c(NA, NA, NA, NA, "methode", "methode", "critere_lissage", "critere_lissage")) &&
    identical(unclass(t$valeurs), list(character(0), character(0), character(0), character(0), "wh_quadratique",
                                       "wh_poisson", "declare", "laml"))
})
verifier("C constantes de wh_poisson (spécification de forme, § 2 ; D12) : valeurs, origines tirées / non tirées de la source", {
  k <- tablexp:::CONSTANTES_LISSAGE_POISSON
  identical(k$constante, c("EPSILON_DEPART_LISSAGE", "EPSILON_DEVIANCE_LISSAGE", "ITERATIONS_MAX_LISSAGE",
                           "DEMI_PAS_MAX_LISSAGE", "POINTS_GRILLE_LISSAGE", "TOLERANCE_BRENT_LISSAGE")) &&
    identical(k$valeur, c(1e-8, 1e-8, 100, 30, 31, 1e-6)) &&
    identical(startsWith(k$origine, "tirée de la source"), c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
})

# --- jeu agrégats écrit à la main et plan ----------------------------------------

AGES <- 40:60
EH <- round(1000 * exp(-0.01 * (AGES - 40)), 3)
DH <- round(EH * 0.002 * exp(0.08 * (AGES - 40)))
jeu <- function(ages = AGES, eh = EH, dh = DH, ef = eh * 1.1, df = round(dh * 0.6))
  data.frame(sexe = rep(c("H", "F"), c(length(ages), length(ages))), age = as.double(c(ages, ages)),
             evenements = as.double(c(dh, df)), exposition_centrale = c(eh, ef), stringsAsFactors = FALSE)
ROLES <- c(sexe = "sexe", age = "age", evenements = "evenements", exposition_centrale = "exposition_centrale")
LISSAGE <- list(methode = "wh_poisson", plage_ages = c(40, 60), ordre_difference = 2, critere_lissage = "declare",
                niveau = 0.95, parametre_lissage = 100)
LAML <- list(methode = "wh_poisson", plage_ages = c(40, 60), ordre_difference = 2, critere_lissage = "laml",
             niveau = 0.95, intervalle_lissage = c(1e-2, 1e4))
VAL <- list(relation_table = "lissee", plage_ages = c(40, 60), tranches_age = c(40, 50), niveau = 0.95,
            replications = 99)
plan_l <- function(lissage = LISSAGE, par_annee = FALSE, estimateurs = "poisson", segmentation = "sexe",
                   validation = NULL) {
  etapes <- list(donnees = list(exclusions = character(0)),
                 exposition = list(convention = "dernier_anniversaire", par_annee = par_annee),
                 brut = list(estimateurs = estimateurs, niveau = 0.95), lissage = lissage)
  p <- list(transition = list(depart = "vivant", arrivee = "deces"),
            periode = list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31")), delai_declaration = 0,
            segmentation = segmentation, etapes = etapes)
  if (!is.null(validation)) {
    p$etapes$validation <- validation
    p$graine <- 22011206L
  }
  p
}
avec <- function(base = LISSAGE, ...) { l <- base; v <- list(...); for (n in names(v)) l[n] <- list(v[[n]]); l }
sans <- function(nom, base = LISSAGE) base[setdiff(names(base), nom)]
rv <- function(...) refus_de(valider_plan(plan_l(...)))
rc <- function(d = jeu(), ...) refus_de(construire_table(d, ROLES, plan_l(...)))

# --- D. Refus de valider_plan(), chacun levé seul (spécification de forme, § 1) ----

verifier("D wh_poisson + declare et wh_poisson + laml admis par valider_plan()", is.null(rv()) && is.null(rv(lissage = LAML)))
verifier("D niveau absent sous wh_poisson : parametre_manquant (gouvernant methode)", controle(
  rv(lissage = sans("niveau")), "construire_table", "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « niveau » sous methode = « wh_poisson » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "niveau", gouvernant = "methode", valeur = "wh_poisson")))
verifier("D ponderation présente sous wh_poisson : parametre_inattendu (gouvernant methode)", controle(
  rv(lissage = avec(ponderation = "exposition")), "construire_table", "parametre_inattendu", NULL,
  list(etape = "lissage", parametre = "ponderation", gouvernant = "methode", valeur = "wh_poisson")))
verifier("D intervalle_lissage absent sous laml : parametre_manquant (gouvernant critere_lissage)", controle(
  rv(lissage = sans("intervalle_lissage", LAML)), "construire_table", "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « intervalle_lissage » sous critere_lissage = « laml » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "intervalle_lissage", gouvernant = "critere_lissage", valeur = "laml")))
verifier("D parametre_lissage sous laml, intervalle_lissage sous declare : parametre_inattendu", tous(list(
  controle(rv(lissage = avec(LAML, parametre_lissage = 100)), "construire_table", "parametre_inattendu", NULL,
           list(etape = "lissage", parametre = "parametre_lissage", gouvernant = "critere_lissage", valeur = "laml")),
  controle(rv(lissage = avec(intervalle_lissage = c(1, 10))), "construire_table", "parametre_inattendu", NULL,
           list(etape = "lissage", parametre = "intervalle_lissage", gouvernant = "critere_lissage",
                valeur = "declare")))))
verifier("D niveau_invalide (module lissage) : 0, 1, NA_real_, 1L, c(0.9, 0.95), nommé, message des intervalles de crédibilité", {
  m <- paste("Le niveau des intervalles de crédibilité (niveau) doit être un nombre (double) unique, sans attribut, fini,",
             "strictement compris entre 0 et 1.")
  tous(lapply(list(0, 1, NA_real_, 1L, c(0.9, 0.95), c(n = 0.95)), function(v)
    controle(rv(lissage = avec(niveau = v)), "lissage", "niveau_invalide", m, list(niveau = v))))
})
verifier("D intervalle_lissage_invalide : c(0, 1), c(10, 1), c(1, 1), c(1, Inf), 1, c(1L, 10L), nommé ; message exact, details", {
  m <- paste("L'intervalle du paramètre de lissage (intervalle_lissage) doit être un vecteur double sans attribut de",
             "deux valeurs finies 0 < a < b (bornes de lambda ; recherche de ln lambda dans [ln a, ln b]), sans défaut.")
  tous(lapply(list(c(0, 1), c(10, 1), c(1, 1), c(1, Inf), 1, c(1L, 10L), c(a = 1, b = 10), c(-1, 1)), function(v)
    controle(rv(lissage = avec(LAML, intervalle_lissage = v)), "lissage", "intervalle_lissage_invalide", m,
             list(intervalle_lissage = v))))
})
verifier("D parametre_lissage_invalide sous wh_poisson (lambda = 0)", controle(
  rv(lissage = avec(parametre_lissage = 0)), "lissage", "parametre_lissage_invalide"))
verifier("D estimateur_lissage_absent : wh_poisson sans poisson dans brut ; message exact, details", controle(
  rv(estimateurs = "binomial"), "lissage", "estimateur_lissage_absent",
  "Lissage : la méthode « wh_poisson » lit l'estimateur brut « poisson », absent de l'étape brut (estimateurs : « binomial ») ; l'ajouter à ses estimateurs.",
  list(methode = "wh_poisson", estimateur = "poisson", estimateurs = "binomial")))
verifier("D lissage_par_annee sous wh_poisson (brut poisson admis sous par_annee) : levé par valider_plan(), message exact", controle(
  rv(par_annee = TRUE), "lissage", "lissage_par_annee",
  "Lissage : découpage par année (par_annee = TRUE) non admis : le lissage porte sur l'âge seul.",
  list(par_annee = TRUE)))
verifier("D methode_lissage_invalide : message liste les deux méthodes", controle(
  rv(lissage = avec(methode = "wh_autre")), "lissage", "methode_lissage_invalide",
  "La méthode de lissage (methode) doit être une chaîne unique, sans attribut, prise dans « wh_quadratique », « wh_poisson », sans défaut."))
verifier("D ordre des refus de valeurs : niveau avant parametre_lissage, intervalle avant estimateur", tous(list(
  controle(rv(lissage = avec(niveau = 2, parametre_lissage = 0)), "lissage", "niveau_invalide"),
  controle(rv(lissage = avec(LAML, intervalle_lissage = c(2, 1)), estimateurs = "binomial"), "lissage",
           "intervalle_lissage_invalide"))))
verifier("D controler_parametres_lissage() : arguments formels hors contexte = paramètres du catalogue, dans l'ordre (ADR 0017, point 3.5)",
         identical(names(formals(tablexp:::controler_parametres_lissage)),
                   c(tablexp:::PARAMETRES_ETAPES$lissage$parametre, "brut", "par_annee")))

# --- F. Refus en cours d'étape (spécification de forme, § 1 ; Q4 de #239) ----------

verifier("F lissage_evenement_sans_exposition (appel direct : la recette agrégats refuse la ligne, ligne_invalide de donnees) : H, âge 50, E^c = 0 et D = 4 ; message exact", {
  ex <- construire_table(jeu(), ROLES, plan_l())$etapes$exposition$resultat$exposition
  i <- which(ex$sexe == "H" & ex$age == 50L)
  ex$exposition_centrale[i] <- 0; ex$evenements[i] <- 4L
  controle(refus_de(tablexp:::calculer_lissage(ex, NULL, "sexe", "dernier_anniversaire", "wh_poisson", c(40, 60), 2,
                                               "declare", NULL, 0.95, 100, NULL)),
           "lissage", "lissage_evenement_sans_exposition",
           paste("Lissage : 1 cellule(s) de la plage d'âges 40 à 60 sans exposition centrale (E^c = 0) qui comptent des",
                 "décès : sexe = « H » (âge 50 : D = 4). La vraisemblance de Poisson y est nulle quel que soit mu",
                 "(attendu E^c mu nul, décès observés) : le lissage est refusé."))
})
verifier("F pas de refus D > E^i sous wh_poisson : H, âge 50, D = 30 > E^c = 20 admis (ADR 0017, point 15)", {
  eh <- EH; eh[11L] <- 20; dh <- DH; dh[11L] <- 30
  x <- construire_table(jeu(eh = eh, dh = dh, ef = EH * 1.1), ROLES, plan_l())
  t <- x$etapes$lissage$resultat$table_lissee
  is.null(refus_de(x)) && all(t$qx > 0 & t$qx < 1)
})
verifier("F lissage_plage_bornee_sans_exposition (D11) sous wh_poisson : plage 39 à 60", controle(
  rc(lissage = avec(plage_ages = c(39, 60))), "lissage", "lissage_plage_bornee_sans_exposition"))
verifier("F lissage_cellules_insuffisantes sous wh_poisson : âges 40 et 60 seuls observés, z = 3 ; message de la vraisemblance de Poisson", {
  d <- jeu(ages = c(40, 60), eh = EH[c(1L, 21L)], dh = DH[c(1L, 21L)])
  controle(rc(d, lissage = avec(ordre_difference = 3)), "lissage", "lissage_cellules_insuffisantes",
           paste("Lissage : 2 segment(s) comptent moins de z = 3 cellule(s) de poids non nul sur la plage d'âges 40 à 60 :",
                 "sexe = « F » (2 cellule(s) de poids non nul) ; sexe = « H » (2 cellule(s) de poids non nul). La",
                 "vraisemblance de Poisson pénalisée n'a pas de maximum unique (au moins z cellules exposées requises :",
                 "la pénalité ne contraint pas les polynômes de degré z - 1)."))
})
verifier("F lissage_sans_deces sous wh_poisson : segment F sans décès", controle(
  rc(jeu(df = rep(0, length(AGES)))), "lissage", "lissage_sans_deces"))
verifier("F lissage_interpolation_deces_nul (Q4 de #239) : âges 40 et 60 seuls observés, z = 2, H sans décès à 40 ; message exact, details", {
  d <- jeu(ages = c(40, 60), eh = EH[c(1L, 21L)], dh = c(0, DH[21L]), df = c(2, 3))
  controle(rc(d), "lissage", "lissage_interpolation_deces_nul",
           paste("Lissage : 1 segment(s) comptent exactement z = 2 cellule(s) exposée(s) (E^c > 0), dont au moins une sans",
                 "décès, sur la plage d'âges 40 à 60 : sexe = « H » (âge(s) sans décès : 40). La vraisemblance de",
                 "Poisson pénalisée y interpole les taux bruts et ln mu tend vers -infini aux cellules sans décès :",
                 "aucun maximum fini, aucune table lissée n'est produite (élargir la plage d'âges ou réduire l'ordre",
                 "des différences)."))
})
verifier("F témoin : sans la règle, exactement z cellules exposées dont une sans décès ne convergent pas (lisser_poisson() : non_convergent ou rang_deficient)", {
  e <- numeric(21); d <- numeric(21); e[c(1, 21)] <- EH[c(1, 21)]; d[21] <- DH[21]
  lp(d, e, 100, 2, AGES, ldp(21, 2))$statut %in% c("non_convergent", "rang_deficient")
})
verifier("F lissage_non_convergent (A6) : décès d'un segment au seul âge 60, z = 2 ; message exact (lambda en %.17g), details", {
  dh <- numeric(length(AGES)); dh[21L] <- 10
  controle(rc(jeu(dh = dh, df = round(DH * 0.6))), "lissage", "lissage_non_convergent",
           paste("Lissage : l'algorithme de la vraisemblance de Poisson pénalisée ne converge pas pour sexe = « H »,",
                 "lambda = 100 (plafond de 100 itérations atteint, déviance pénalisée non décroissante ou non finie après",
                 "30 divisions du pas sans convergence numérique, ou rang numérique du système perdu en cours",
                 "d'itération) ; l'existence du maximum n'est pas établie pour ces données ; aucune table lissée n'est",
                 "produite."))
})
verifier("F details de lissage_interpolation_deces_nul et de lissage_non_convergent : plage, ordre, segments cités (clés), lambda", {
  d <- jeu(ages = c(40, 60), eh = EH[c(1L, 21L)], dh = c(0, DH[21L]), df = c(2, 3))
  r1 <- rc(d)
  dh <- numeric(length(AGES)); dh[21L] <- 10
  r2 <- rc(jeu(dh = dh, df = round(DH * 0.6)))
  identical(names(r1$details), c("plage_ages", "ordre_difference", "segments")) &&
    identical(r1$details$segments$sexe, "H") && identical(r1$details$ordre_difference, 2) &&
    identical(names(r2$details), c("plage_ages", "ordre_difference", "segments", "parametre_lissage")) &&
    identical(r2$details$segments$sexe, "H") && identical(r2$details$parametre_lissage, 100)
})
verifier("F lissage_non_convergent sous laml : le segment est cité au lambda de la grille où l'algorithme échoue (borne inférieure déclarée, évaluée exactement)", {
  dh <- numeric(length(AGES)); dh[21L] <- 10
  r <- rc(jeu(dh = dh, df = round(DH * 0.6)), lissage = LAML)
  ok <- controle(r, "lissage", "lissage_non_convergent")
  if (!isTRUE(ok)) return(ok)
  identical(r$details$parametre_lissage, 1e-2)
})
verifier("F lissage_systeme_rang_deficient sous wh_poisson : lambda = 1e20, z = 3 ; message de la variante (b), details", {
  r <- rc(lissage = avec(parametre_lissage = 1e20, ordre_difference = 3))
  ok <- controle(r, "lissage", "lissage_systeme_rang_deficient")
  if (!isTRUE(ok)) return(ok)
  tous(list(startsWith(conditionMessage(r), paste("Lissage : système des moindres carrés augmentés de l'algorithme de la",
                                                  "vraisemblance de Poisson pénalisée numériquement de rang déficient")),
            identical(r$details$parametre_lissage, rep(1e20, nrow(r$details$segments)))))
})

# --- G. Résultat : structure, contrat, recalcul ------------------------------------

X1 <- construire_table(jeu(), ROLES, plan_l())
L1 <- X1$etapes$lissage
verifier("G etapes$lissage$parametres sous declare : paramètres du plan dans l'ordre du catalogue, puis convention, estimateur_brut (poisson), echelle_penalite (ln_mu), regle_force, hypothese, reference_methode, constantes (4 lignes), statuts (sans segments/parametre_lissage)", {
  p <- L1$parametres
  identical(names(p), c("methode", "plage_ages", "ordre_difference", "critere_lissage", "niveau", "parametre_lissage",
                        "convention", "estimateur_brut", "echelle_penalite", "regle_force", "hypothese",
                        "reference_methode", "constantes", "statuts")) &&
    identical(p[1:6], LISSAGE) && identical(p$estimateur_brut, "poisson") && identical(p$echelle_penalite, "ln_mu") &&
    identical(p$constantes$constante, tablexp:::CONSTANTES_LISSAGE_POISSON$constante[1:4]) &&
    identical(rownames(p$constantes), as.character(1:4)) &&
    !("segments/parametre_lissage" %in% p$statuts$grandeur) &&
    identical(p$reference_methode, tablexp:::texte_moteur(tablexp:::REFERENCE_METHODE_LISSAGE_POISSON)) &&
    grepl("theta_0 = 0 aux cellules sans exposition", p$hypothese, fixed = TRUE)
})
XL <- construire_table(jeu(), ROLES, plan_l(lissage = LAML))
verifier("G sous laml : intervalle_lissage dans les paramètres, constantes (6 lignes), statut de segments/parametre_lissage", {
  p <- XL$etapes$lissage$parametres
  identical(names(p)[1:6], names(LAML)) && identical(p$intervalle_lissage, c(1e-2, 1e4)) &&
    identical(nrow(p$constantes), 6L) && "segments/parametre_lissage" %in% p$statuts$grandeur
})
verifier("G table_lissee : data.frame nu, colonnes sexe, age, qx, etat, levier, borne_inf_credibilite, borne_sup_credibilite ; bornes encadrant qx", {
  t <- L1$resultat$table_lissee
  identical(class(t), "data.frame") &&
    identical(names(t), c("sexe", "age", "qx", "etat", "levier", "borne_inf_credibilite", "borne_sup_credibilite")) &&
    is.integer(t$age) && all(t$qx > 0 & t$qx < 1) && all(t$etat == "observee") &&
    all(t$borne_inf_credibilite < t$qx & t$qx < t$borne_sup_credibilite)
})
verifier("G segments : colonnes et types (iterations integer) ; etat produite sous declare", {
  s <- L1$resultat$segments
  identical(names(s), c("sexe", "cellules", "cellules_ponderees", "cellules_interpolees", "evenements",
                        "parametre_lissage", "ddl_effectifs", "regularite", "laml", "deviance_penalisee", "iterations",
                        "residu_conservation_0", "residu_conservation_1", "etat")) &&
    all(vapply(s[c("cellules", "cellules_ponderees", "cellules_interpolees", "evenements", "iterations")], is.integer, NA)) &&
    all(vapply(s[c("parametre_lissage", "ddl_effectifs", "regularite", "laml", "deviance_penalisee",
                   "residu_conservation_0", "residu_conservation_1")], is.double, NA)) &&
    identical(s$parametre_lissage, c(100, 100)) && identical(s$etat, c("produite", "produite"))
})
verifier("G diagnostics : exposition_centrale_hors_plage (au lieu de exposition_initiale_hors_plage), identité plage + hors plage = total", {
  d <- L1$diagnostics
  identical(names(d), c("cellules", "cellules_interpolees", "evenements_plage", "cellules_hors_plage",
                        "evenements_hors_plage", "exposition_centrale_hors_plage", "evenements_total")) &&
    identical(d$evenements_plage + d$evenements_hors_plage, d$evenements_total)
})
verifier("G table_lissee recalculée par lisser_poisson() sur D et E^c de l'étape : qx = -expm1(-exp(theta^)), levier, bornes, ddl, LAML, itérations : identité exacte, segment H", {
  ex <- X1$etapes$exposition$resultat$exposition
  k <- ex$sexe == "H"
  r <- lp(ex$evenements[k], ex$exposition_centrale[k], 100, 2, ex$age[k], ldp(sum(k), 2))
  t <- L1$resultat$table_lissee; s <- L1$resultat$segments
  za <- stats::qnorm((1 - 0.95) / 2, lower.tail = FALSE)
  identical(t$qx[t$sexe == "H"], -expm1(-exp(r$theta))) && identical(t$levier[t$sexe == "H"], r$levier) &&
    identical(t$borne_inf_credibilite[t$sexe == "H"], -expm1(-exp(r$theta - za * sqrt(r$variance)))) &&
    identical(s$ddl_effectifs[s$sexe == "H"], r$ddl_effectifs) && identical(s$laml[s$sexe == "H"], r$laml) &&
    identical(s$iterations[s$sexe == "H"], r$iterations)
})
verifier("G contrat de la table construite (ADR 0017, points 1.5 et 5.5) : table_construite() accepte table_lissee de wh_poisson, somme des leviers identical() à ddl_effectifs", {
  r <- tablexp:::table_construite(X1$etapes, "sexe", "lissage")
  identical(r$table, L1$resultat$table_lissee) && identical(r$ddl_effectifs, L1$resultat$segments[c("sexe", "ddl_effectifs")])
})
verifier("G cellule intérieure sans ligne (H, âge 50) : état interpolee, levier 0, bornes produites ; cellules_interpolees", {
  d <- jeu(); d <- d[!(d$sexe == "H" & d$age == 50), ]
  x <- construire_table(d, ROLES, plan_l())
  t <- x$etapes$lissage$resultat$table_lissee; s <- x$etapes$lissage$resultat$segments
  i <- which(t$sexe == "H" & t$age == 50L)
  identical(t$etat[i], "interpolee") && identical(t$levier[i], 0) && t$qx[i] > 0 && t$qx[i] < 1 &&
    t$borne_inf_credibilite[i] < t$qx[i] && identical(s$cellules_interpolees, c(0L, 1L)) &&
    identical(x$etapes$lissage$diagnostics$cellules_interpolees, 1L)
})
verifier("G conservation sur l'objet : somme E^c (-ln(1 - qx)) = somme D par segment à 1e-10 près en relatif", {
  ex <- X1$etapes$exposition$resultat$exposition; t <- L1$resultat$table_lissee
  e <- vapply(c("H", "F"), function(s) abs(sum(ex$exposition_centrale[ex$sexe == s] * -log1p(-t$qx[t$sexe == s])) -
                                           sum(ex$evenements[ex$sexe == s])) / sum(ex$evenements[ex$sexe == s]), 0)
  all(e <= 1e-10)
})
verifier("G lambda_a_la_borne : jeu quasi gompertzien, z = 2, intervalle [1e-2, 1e4] : état des deux segments, un avertissement lambda_a_la_borne (module lissage) par segment, message exact (segment H)", {
  s <- XL$etapes$lissage$resultat$segments
  a <- XL$avertissements[XL$avertissements$code == "lambda_a_la_borne", , drop = FALSE]
  lh <- s$parametre_lissage[s$sexe == "H"]
  identical(s$etat, c("lambda_a_la_borne", "lambda_a_la_borne")) && identical(a$etape, c("lissage", "lissage")) &&
    identical(a$nombre, c(1L, 1L)) &&
    identical(a$message[2L], sprintf(paste("Lissage, sexe = « H » : le meilleur point de la grille de la LAML est la",
                                           "borne supérieure de l'intervalle déclaré (lambda = %.17g) ; lambda retenu :",
                                           "%.17g. Un maximum intérieur peut ne pas exister (la LAML peut croître",
                                           "jusqu'à la limite polynomiale, polynôme de degré z - 1 en ln mu, quand",
                                           "lambda tend vers l'infini) ; élargir l'intervalle ou déclarer lambda."),
                                     1e4, lh))
})
verifier("G laml : lambda^ = point de grille G[k*] ou point de Brent, celui de plus grande LAML ; LAML au lambda^ >= LAML en chaque point de la grille, extrémités aux bornes déclarées ; sur [1e-2, 1e12], meilleur point à la borne supérieure : lambda^ = 1e12 exactement pour F et H (règle Q1 d'actuary) (identité exacte sur la règle)", {
  ex <- X1$etapes$exposition$resultat$exposition; k <- ex$sexe == "F"
  x <- construire_table(jeu(), ROLES, plan_l(lissage = avec(LAML, intervalle_lissage = c(1e-2, 1e12))))
  s <- x$etapes$lissage$resultat$segments
  G <- seq(log(1e-2), log(1e12), length.out = 31L)
  lambdas <- exp(G); lambdas[c(1L, 31L)] <- c(1e-2, 1e12)
  lg <- vapply(lambdas, function(l) lp(ex$evenements[k], ex$exposition_centrale[k], l, 2, ex$age[k], ldp(sum(k), 2))$laml, 0)
  s$laml[s$sexe == "F"] >= max(lg) && identical(s$parametre_lissage, c(1e12, 1e12))
})
verifier("G avertissements : aucun sous declare sur ce jeu (aucune ligne d'étape lissage)", !any(X1$avertissements$etape == "lissage"))
verifier("G qx_arrondi_un : H, âge 60, E^c = 0,2 et D = 10 (mu^ voisin de 50 >= 54 ln 2), lambda = 1e-6 : qx = 1 par arrondi, état qx_arrondi_un, avertissement qx_arrondi_un (module lissage) qui cite la ligne ; table construite admise (qx dans [0, 1])", {
  eh <- EH; eh[21L] <- 0.2; dh <- DH; dh[21L] <- 10
  x <- construire_table(jeu(eh = eh, dh = dh, ef = EH * 1.1), ROLES, plan_l(lissage = avec(parametre_lissage = 1e-6)))
  t <- x$etapes$lissage$resultat$table_lissee
  a <- x$avertissements[x$avertissements$code == "qx_arrondi_un", , drop = FALSE]
  i <- which(t$sexe == "H" & t$age == 60L)
  identical(t$qx[i], 1) && identical(t$etat[i], "qx_arrondi_un") && identical(sum(t$etat == "qx_arrondi_un"), 1L) &&
    identical(a$etape, "lissage") && identical(a$nombre, 1L) &&
    startsWith(a$message, paste("Lissage : 1 ligne(s) de la table lissée où q ou une borne de crédibilité vaut 1 par",
                                "arrondi : sexe = « H », âge 60, valeur(s) égale(s) à 1 : « qx »")) &&
    is.list(tablexp:::table_construite(x$etapes, "sexe", "lissage"))
})

# --- V. Validation lissee sous wh_poisson (#224 C1 ; Q4 de #239) ------------------

verifier("V relation lissee sous wh_poisson : SMR = O / A = 1 à 1e-10 près par segment (conservation, #224, critère C1 sous (b))", {
  x <- construire_table(jeu(), ROLES, plan_l(validation = VAL))
  ce <- x$etapes$validation$resultat$cellules
  e <- vapply(c("H", "F"), function(s) {
    k <- ce$sexe == s & ce$etat == "retenue"
    abs(sum(ce$evenements[k]) / sum(ce$attendus[k]) - 1)
  }, 0)
  all(e <= 1e-10)
})
verifier("V interpolation exacte sous wh_poisson (Q4 de #239) : z = 2 cellules exposées par segment, toutes à D > 0 : leviers > 0, interpolation_exacte vrai, ddl du khi-deux 0 exactement", {
  d <- jeu(ages = c(40, 60), eh = EH[c(1L, 21L)], dh = c(2, DH[21L]), df = c(1, 3))
  x <- construire_table(d, ROLES, plan_l(validation = VAL))
  t <- x$etapes$lissage$resultat$table_lissee
  st <- x$etapes$validation$resultat$statistiques
  ddl <- st$valeur[st$statistique == "khi2_pearson" & st$grandeur == "ddl"]
  pd <- x$etapes$validation$parametres$ddl_effectifs
  sum(t$levier > 0) == 4L && all(pd$interpolation_exacte) && identical(pd$ddl_effectifs_retenus, c(2, 2)) &&
    identical(ddl, 0)
})

# --- H. Plan : aller-retour et canonisation ; rejeu ; rapport figé ----------------

verifier("H canoniser_plan() idempotente sur un plan wh_poisson + laml ; aller-retour ecrire_plan() / lire_plan() identique (identité exacte)", {
  cn <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                             tablexp:::ordres_canoniques())
  p <- plan_l(lissage = LAML[c(6L, 5L, 4L, 3L, 2L, 1L)])
  c1 <- cn(p)
  identical(names(c1$etapes$lissage), names(LAML)) && identical(cn(c1), c1) &&
    identical(lire_plan(ecrire_plan(c1)), c1) && identical(XL$fiche$plan, c1)
})
verifier("H rejeu de la fiche d'une construction wh_poisson + laml : objet identique hors execution (déterministe, sans graine)", {
  r <- rejouer_fiche(jeu(), XL$fiche)
  identical(sans_execution(r$construction), sans_execution(XL))
})
verifier("H rapport figé : section etape_lissage sous wh_poisson (mentions lissage_poisson, lissage_conservation_poisson, lissage_echelle, table lissée)", {
  h <- rawToChar(ecrire_rapport(XL)); Encoding(h) <- "UTF-8"
  m <- tablexp:::MENTIONS_RAPPORT
  echapper <- tablexp:::echapper_html
  tous(list(all(vapply(c("lissage_poisson", "lissage_conservation_poisson", "lissage_echelle"), function(k)
              grepl(echapper(m[[k]]), h, fixed = TRUE), NA)),
            !grepl(echapper(m[["lissage_quadratique"]]), h, fixed = TRUE),
            grepl("etapes/lissage/resultat/table_lissee", h, fixed = TRUE)))
})

# --- I. Contre-vérification C1 et C2 (WH::WH() de WH 2.0.0, hors CI) ----------------
#
# Valeurs écrites par tests/contre_verification/wh_lissage_poisson.R (hors
# CI, ADR 0002, troisième annotation du 16/10/2026) dans
# tests/contre_verification/valeurs_wh_lissage_poisson.csv, sur le jeu fixé
# jeu_wh_lissage_poisson.csv (trois jeux : sans segmentation ; par sexe ; par
# sexe avec une cellule intérieure sans exposition, absente des données du
# moteur). C1 (lambda déclaré, WH à tol_dev = 1e-12) : écart relatif <= 1e-8
# sur theta^ contre y_hat, <= 1e-6 sur ddl_effectifs contre la somme de edf,
# <= 1e-8 sur LAML - l_sat contre -REML. C2 (lambda choisi, intervalle
# [1e-6, 1e12]) : |ln lambda^ - ln lambda_WH| <= 1e-3, LAML(lambda^) >=
# LAML(lambda_WH) - 1e-6 (corps de #225, C1 et C2 ; recoupement).

DOSSIER_CV <- file.path(RACINE, "tests", "contre_verification")
FICHIER_WH <- file.path(DOSSIER_CV, "valeurs_wh_lissage_poisson.csv")
verifier("I C1 et C2 fichier de valeurs : présent, en-tête « # WH : 2.0.0 » et version de R consignées", {
  if (!file.exists(FICHIER_WH)) return("fichier de valeurs absent (à produire par wh_lissage_poisson.R)")
  e <- readLines(FICHIER_WH, warn = FALSE, encoding = "UTF-8")
  e <- e[startsWith(e, "#")]
  "# WH : 2.0.0" %in% e && any(startsWith(e, "# R : R version "))
})
CV <- local({
  if (!file.exists(FICHIER_WH)) return(NULL)
  jeu_cv <- utils::read.csv(file.path(DOSSIER_CV, "jeu_wh_lissage_poisson.csv"), comment.char = "#",
                            stringsAsFactors = FALSE)
  l <- readLines(FICHIER_WH, warn = FALSE, encoding = "UTF-8")
  val <- utils::read.csv(text = l[!startsWith(l, "#")], stringsAsFactors = FALSE, colClasses = "character")
  val$age <- as.integer(val$age)
  for (k in c("y_hat", "ddl", "reml", "lambda_laml", "reml_laml")) val[[k]] <- as.numeric(val[[k]])
  lignes <- list()
  for (j in unique(jeu_cv$jeu)) {
    t <- jeu_cv[jeu_cv$jeu == j, , drop = FALSE]
    seg <- if (all(t$segment == "ensemble")) character(0) else "sexe"
    plage <- as.double(range(t$age)); z <- as.double(unique(t$ordre_difference))
    t2 <- t[t$exposition_centrale > 0, , drop = FALSE]
    d <- data.frame(age = as.double(t2$age), evenements = as.double(t2$evenements),
                    exposition_centrale = t2$exposition_centrale, stringsAsFactors = FALSE)
    roles <- c(age = "age", evenements = "evenements", exposition_centrale = "exposition_centrale")
    if (length(seg)) { d$sexe <- t2$segment; roles <- c(roles, sexe = "sexe") }
    x1 <- construire_table(d, roles, plan_l(segmentation = seg, lissage = list(
      methode = "wh_poisson", plage_ages = plage, ordre_difference = z, critere_lissage = "declare", niveau = 0.95,
      parametre_lissage = as.double(unique(t$parametre_lissage)))))
    x2 <- construire_table(d, roles, plan_l(segmentation = seg, lissage = list(
      methode = "wh_poisson", plage_ages = plage, ordre_difference = z, critere_lissage = "laml", niveau = 0.95,
      intervalle_lissage = c(1e-6, 1e12))))
    tl <- x1$etapes$lissage$resultat$table_lissee
    s1 <- x1$etapes$lissage$resultat$segments; s2 <- x2$etapes$lissage$resultat$segments
    for (s in unique(t$segment)) {
      k <- if (length(seg)) tl$sexe == s else rep(TRUE, nrow(tl))
      ks <- if (length(seg)) which(s1$sexe == s) else 1L
      v <- val[val$jeu == j & val$segment == s, , drop = FALSE]
      ts <- t[t$segment == s, , drop = FALSE]
      if (nrow(v) != sum(k) || !identical(v$age, tl$age[k])) return(sprintf("%s %s : cellules différentes", j, s))
      a <- ts$exposition_centrale > 0 & ts$evenements > 0
      l_sat <- sum(ts$evenements[a] * log(ts$evenements[a] / ts$exposition_centrale[a]) - ts$evenements[a])
      r_wh <- lp(ts$evenements, ts$exposition_centrale, v$lambda_laml[1L], z, ts$age, ldp(nrow(ts), z))
      lignes[[length(lignes) + 1L]] <- c(
        theta = max(abs(log(-log1p(-tl$qx[k])) - v$y_hat) / abs(v$y_hat)),
        ddl = abs(s1$ddl_effectifs[ks] - v$ddl[1L]) / v$ddl[1L],
        laml = abs((s1$laml[ks] - l_sat) + v$reml[1L]) / abs(v$reml[1L]),
        ln_lambda = abs(log(s2$parametre_lissage[ks]) - log(v$lambda_laml[1L])),
        laml_c2 = r_wh$laml - s2$laml[ks])
    }
  }
  do.call(rbind, lignes)
})
verifier("I C1 moteur contre WH::WH(d, ec, lambda, q = z, reg = FALSE) : theta^ contre y_hat <= 1e-8, ddl_effectifs contre somme de edf <= 1e-6, LAML - l_sat contre -REML <= 1e-8 (écarts relatifs), trois jeux", {
  if (is.null(CV)) return("fichier de valeurs absent")
  if (is.character(CV)) return(CV)
  e <- c(max(CV[, "theta"]) / 1e-8, max(CV[, "ddl"]) / 1e-6, max(CV[, "laml"]) / 1e-8)
  if (all(e <= 1)) TRUE else sprintf("écarts rapportés aux tolérances : %s", paste(signif(e, 3), collapse = ", "))
})
verifier("I C2 lambda choisi par la LAML contre WH::WH(d, ec, q = z, reg = FALSE) : |ln lambda^ - ln lambda_WH| <= 1e-3 et LAML(lambda^) >= LAML(lambda_WH) - 1e-6, trois jeux", {
  if (is.null(CV)) return("fichier de valeurs absent")
  if (is.character(CV)) return(CV)
  if (all(CV[, "ln_lambda"] <= 1e-3) && all(CV[, "laml_c2"] <= 1e-6)) TRUE
  else sprintf("|écart ln lambda| max %.3g ; LAML(lambda_WH) - LAML(lambda^) max %.3g", max(CV[, "ln_lambda"]),
               max(CV[, "laml_c2"]))
})
