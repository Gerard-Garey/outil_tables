# -----------------------------------------------------------------------------
#  tests/unitaires/test_lissage_quadratique.R : étape `lissage`, méthode
#  wh_quadratique (#223, commit (1b) de B19 ; ADR 0017, points 1 à 4, 9, 10,
#  11 et 15 ; spécification de forme (0')(b) d'actuary-approfondi, M415)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité mathématique : critères A1 à A5 du corps de #223 sur vecteurs
#    fixés (sans aléa), tolérances de la spécification ;
#  - recoupement : B1 (stats::lm, h = 1e10) et B2 (h = 1e-10) ;
#  - valeur calculée à la main : catalogues, motifs, modules, messages et
#    details des refus, structure de etapes$lissage ;
#  - identité exacte : aller-retour du plan, canoniser_plan() idempotente,
#    table_lissee recalculée par lisser_quadratique() sur les entrées de
#    l'étape.
#  Critères amendés par actuary le 11/11/2026 (#223, commentaire
#  6802281536) : A1 est l'erreur inverse normée (<= 1e-14 ; le résidu
#  relatif des équations normales croît avec h quel que soit le solveur),
#  A6 porte sur A1 et A2 ; A3 et A4 : 1e-12 pour h <= 1e4, 1e-9 à h = 1e8 ;
#  B1 sur n = 12.
#  Jeux sans aléa : agrégats écrits à la main (aucune graine).
# -----------------------------------------------------------------------------

lq <- function(...) tablexp:::lisser_quadratique(...)
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
erreur_programmation <- function(expr) {
  e <- tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e)
  !is.null(e) && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
}
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
Q <- D / E
WE <- E / mean(E)                       # pondération exposition
WU <- rep(1, N)                         # pondération uniforme
TROUS <- X %in% c(45, 52)               # deux cellules intérieures sans exposition
E0 <- ifelse(TROUS, 0, E); D0 <- ifelse(TROUS, 0, D)
Q0 <- ifelse(TROUS, 0, D0 / ifelse(TROUS, 1, E0))
WE0 <- ifelse(TROUS, 0, E0 / mean(E0[!TROUS]))
H_GRILLE <- c(1e-4, 1, 1e4, 1e8)
JEUX <- list(uniforme = list(q = Q, w = WU), exposition = list(q = Q, w = WE),
             exposition_trous = list(q = Q0, w = WE0))

# --- A. Identités sur vecteurs fixés (corps de #223, critères A1 à A6, amendés
# par actuary le 11/11/2026) -------------------------------------------------------

verifier("A1 et A6 erreur inverse normée ||(W + hK'K)g - Wq^||_2 / (||W + hK'K||_F ||g||_2 + ||Wq^||_2) <= 1e-14, grille h = 1e-4, 1, 1e4, 1e8, z = 1 à 4, trois jeux (uniforme, exposition, exposition avec deux cellules interpolées) (identité mathématique)", {
  e <- unlist(lapply(JEUX, function(j) vapply(1:4, function(z) max(vapply(H_GRILLE, function(h) {
    g <- lq(j$q, j$w, h, z, X)$g
    A <- diag(j$w) + h * crossprod(diff(diag(N), differences = z))
    sqrt(sum((A %*% g - j$w * j$q)^2)) / (norm(A, "F") * sqrt(sum(g^2)) + sqrt(sum((j$w * j$q)^2)))
  }, 0)), 0)))
  if (all(e <= 1e-14)) TRUE else sprintf("erreur inverse max %.3g", max(e))
})
verifier("A2 et A6 pondération exposition : |somme E^i g - somme D| <= 1e-8 somme D (z = 1 à 4) et |somme x E^i (g - q^)| <= 1e-8 somme x D (z = 2 à 4), grille h = 1e-4, 1, 1e4, 1e8, avec et sans cellules interpolées (identité mathématique)", {
  e <- unlist(lapply(list(list(q = Q, w = WE, E = E, D = D), list(q = Q0, w = WE0, E = E0, D = D0)), function(j)
    unlist(lapply(1:4, function(z) vapply(H_GRILLE, function(h) {
      g <- lq(j$q, j$w, h, z, X)$g
      c0 <- abs(sum(j$E * g) - sum(j$D)) / sum(j$D)
      c1 <- if (z >= 2) abs(sum(X * j$E * (g - j$q))) / sum(X * j$D) else 0
      max(c0, c1)
    }, 0)))))
  if (all(e <= 1e-8)) TRUE else sprintf("écart relatif max %.3g", max(e))
})
verifier("A2 z = 1 : la seconde identité (somme x E^i g = somme x D) ne tient pas (écart relatif > 1e-3 à h = 1, 1e4, 1e8 ; écart aux Lignes directrices, p. 32-33 ; valeur mesurée)", {
  e <- vapply(c(1, 1e4, 1e8), function(h) abs(sum(X * E * (lq(Q, WE, h, 1, X)$g - Q))) / sum(X * D), 0)
  if (all(e > 1e-3)) TRUE else sprintf("écarts %s", paste(signif(e, 3), collapse = ", "))
})
verifier("A2 résidus de conservation rendus : residu_conservation_0 = somme w (g - q^), residu_conservation_1 = somme x w (g - q^) (identité exacte)", {
  r <- lq(Q0, WE0, 1e4, 3, X)
  identical(r$residu_conservation_0, sum(WE0 * (r$g - Q0))) && identical(r$residu_conservation_1, sum(X * WE0 * (r$g - Q0)))
})
# Tolérance de A3 et A4 par h (amendement d'actuary, point 2).
TOL_A34 <- c(1e-12, 1e-12, 1e-12, 1e-9)
verifier("A3 q^ polynôme de degré z - 1 : g = q^ en relatif à 1e-12 près pour h = 1e-4, 1, 1e4 et à 1e-9 près pour h = 1e8, z = 1 à 4, poids avec cellules nulles (identité mathématique)", {
  e <- vapply(1:4, function(z) {
    p <- 0.002 + (if (z > 1) 2e-4 * (X - 30) else 0 * X) + (if (z > 2) 3e-6 * (X - 30)^2 else 0) +
      (if (z > 3) 1e-7 * (X - 30)^3 else 0)
    max(vapply(seq_along(H_GRILLE), function(k) max(abs(lq(p, WE0, H_GRILLE[k], z, X)$g - p) / p) / TOL_A34[k], 0))
  }, 0)
  if (all(e <= 1)) TRUE else sprintf("écart relatif rapporté à la tolérance max %.3g", max(e))
})
verifier("A4 invariance (w, h) -> (c w, c h), c = 7,3 : g inchangé en relatif à 1e-12 près pour h = 1e-4, 1, 1e4 et à 1e-9 près pour h = 1e8, z = 1 à 4 (identité mathématique)", {
  e <- vapply(1:4, function(z) max(vapply(seq_along(H_GRILLE), function(k) {
    h <- H_GRILLE[k]
    g1 <- lq(Q, WE, h, z, X)$g; g2 <- lq(Q, 7.3 * WE, 7.3 * h, z, X)$g
    max(abs(g2 - g1) / abs(g1)) / TOL_A34[k]
  }, 0)), 0)
  if (all(e <= 1)) TRUE else sprintf("écart relatif rapporté à la tolérance max %.3g", max(e))
})
verifier("A5 ddl effectifs strictement décroissants en h (1e-4 à 1e8) et dans ]z, n+], z = 1 à 4, poids avec cellules nulles (n+ = 39 ; identité mathématique)", {
  ok <- vapply(1:4, function(z) {
    nu <- vapply(c(1e-4, 1e-2, 1, 1e2, 1e4, 1e6, 1e8), function(h) lq(Q0, WE0, h, z, X)$ddl_effectifs, 0)
    all(diff(nu) < 0) && all(nu > z & nu <= sum(WE0 > 0))
  }, NA)
  all(ok)
})
verifier("A F, S, M : fidelite = somme w (g - q^)^2, regularite = somme_differences_carrees(g, z) = somme (diff(g, z))^2, critere = F + h S (identité exacte)", {
  r <- lq(Q, WE, 1e2, 2, X)
  identical(r$fidelite, sum(WE * (r$g - Q)^2)) && identical(r$regularite, sum(diff(r$g, differences = 2)^2)) &&
    identical(r$regularite, tablexp:::somme_differences_carrees(r$g, 2)) &&
    identical(r$critere, r$fidelite + 1e2 * r$regularite)
})

# --- B. Recoupements (stats) ----------------------------------------------------

verifier("B1 h = 1e10 : g contre stats::lm(q^ ~ poly(x, z - 1, raw = TRUE), weights = w), écart relatif <= 1e-6, z = 1 à 4, n = 12 (Lignes directrices, p. 31 ; recoupement)", {
  i <- 1:12; x <- X[i]; q <- Q[i]; w <- WE[i]
  e <- vapply(1:4, function(z) {
    f <- if (z == 1) unname(stats::fitted(stats::lm(q ~ 1, weights = w)))
         else unname(stats::fitted(stats::lm(q ~ poly(x, z - 1, raw = TRUE), weights = w)))
    max(abs(lq(q, w, 1e10, z, x)$g - f) / abs(f))
  }, 0)
  if (all(e <= 1e-6)) TRUE else sprintf("écart relatif max %.3g", max(e))
})
verifier("B2 h = 1e-10 : g = q^ à 1e-8 près sur les cellules de poids non nul, z = 1 à 4 (recoupement)", {
  e <- vapply(1:4, function(z) max(abs(lq(Q0, WE0, 1e-10, z, X)$g - Q0)[WE0 > 0]), 0)
  if (all(e <= 1e-8)) TRUE else sprintf("écart max %.3g", max(e))
})

# --- C. Catalogues (ADR 0017, points 2.1, 3.4 et 4.1 ; valeur calculée à la main)

verifier("C ETAPES : ligne lissage entre brut et positionnement ; module lissage, calculer_lissage, préalables donnees, exposition, brut, non aléatoire, table_construite table_lissee", {
  et <- tablexp:::ETAPES
  k <- match("lissage", et$etape)
  identical(et$etape[k - 1L], "brut") && identical(et$etape[k + 1L], "positionnement") &&
    identical(et$module[k], "lissage") && identical(et$fonction[k], "calculer_lissage") &&
    identical(et$prealable[[k]], c("donnees", "exposition", "brut")) && identical(et$aleatoire[k], FALSE) &&
    identical(et$table_construite[k], "table_lissee")
})
verifier("C PARAMETRES_ETAPES$lissage : methode, plage_ages, ordre_difference, critere_lissage, ponderation, niveau, parametre_lissage, intervalle_lissage ; types ; conditions (ponderation selon methode = wh_quadratique, niveau selon methode = wh_poisson, parametre_lissage selon critere_lissage = declare, intervalle_lissage selon critere_lissage = laml ; #225)", {
  t <- tablexp:::PARAMETRES_ETAPES$lissage
  identical(t$parametre, c("methode", "plage_ages", "ordre_difference", "critere_lissage", "ponderation", "niveau",
                           "parametre_lissage", "intervalle_lissage")) &&
    identical(t$type, c("character", "double", "double", "character", "character", "double", "double", "double")) &&
    all(is.na(t$recette)) &&
    identical(t$selon, c(NA, NA, NA, NA, "methode", "methode", "critere_lissage", "critere_lissage")) &&
    identical(unclass(t$valeurs), list(character(0), character(0), character(0), character(0), "wh_quadratique",
                                       "wh_poisson", "declare", "laml"))
})
verifier("C controler_parametres_lissage() : arguments formels hors contexte (brut, par_annee) = paramètres du catalogue, dans l'ordre (ADR 0017, point 3.5)", {
  f <- names(formals(tablexp:::controler_parametres_lissage))
  identical(f, c(tablexp:::PARAMETRES_ETAPES$lissage$parametre, "brut", "par_annee"))
})
verifier("C MATRICE_RECETTES : une ligne (recette, lissage, NA, oui) par recette, sans rôle ni motif", {
  m <- tablexp:::MATRICE_RECETTES
  l <- m[m$etape == "lissage", ]
  identical(l$recette, c("lignes_dates_completes", "agregats", "stocks_successifs")) && all(is.na(l$estimateur)) &&
    all(l$admis == "oui") && all(is.na(l$role_requis)) && all(is.na(l$motif))
})

# --- jeu agrégats écrit à la main et plan ----------------------------------------

AGES <- 40:60
EH <- round(1000 * exp(-0.01 * (AGES - 40)), 3)
DH <- round(EH * 0.002 * exp(0.08 * (AGES - 40)))
jeu <- function(ages = AGES, eh = EH, dh = DH, ef = eh * 1.1, df = round(dh * 0.6))
  data.frame(sexe = rep(c("H", "F"), c(length(ages), length(ages))), age = as.double(c(ages, ages)),
             evenements = as.double(c(dh, df)), exposition_initiale = c(eh, ef), stringsAsFactors = FALSE)
ROLES <- c(sexe = "sexe", age = "age", evenements = "evenements", exposition_initiale = "exposition_initiale")
LISSAGE <- list(methode = "wh_quadratique", plage_ages = c(40, 60), ordre_difference = 2, critere_lissage = "declare",
                ponderation = "exposition", parametre_lissage = 100)
plan_l <- function(lissage = LISSAGE, convention = "dernier_anniversaire", par_annee = FALSE,
                   estimateurs = "binomial", positionnement = NULL, segmentation = "sexe") {
  etapes <- list(donnees = list(exclusions = character(0)),
                 exposition = list(convention = convention, par_annee = par_annee),
                 brut = list(estimateurs = estimateurs, niveau = 0.95))
  if (!is.null(lissage)) etapes$lissage <- lissage
  if (!is.null(positionnement)) etapes$positionnement <- positionnement
  list(transition = list(depart = "vivant", arrivee = "deces"),
       periode = list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31")), delai_declaration = 0,
       segmentation = segmentation, etapes = etapes)
}
avec <- function(...) { l <- LISSAGE; v <- list(...); for (n in names(v)) l[n] <- list(v[[n]]); l }
sans <- function(nom) LISSAGE[setdiff(names(LISSAGE), nom)]
rv <- function(...) refus_de(valider_plan(plan_l(...)))
rc <- function(d = jeu(), ...) refus_de(construire_table(d, ROLES, plan_l(...)))

# --- D. Refus de valider_plan(), chacun levé seul (spécification, § 2) ------------

verifier("D methode_lissage_invalide (module lissage) : message exact, details", controle(
  rv(lissage = avec(methode = "wh_autre")), "lissage", "methode_lissage_invalide",
  "La méthode de lissage (methode) doit être une chaîne unique, sans attribut, prise dans « wh_quadratique », « wh_poisson », sans défaut.",
  list(methode = "wh_autre")))
verifier("D critere_lissage_invalide : message exact, details", controle(
  rv(lissage = avec(critere_lissage = "auto")), "lissage", "critere_lissage_invalide",
  paste("Le critère de choix du paramètre de lissage (critere_lissage) doit être une chaîne unique, sans attribut,",
        "prise dans « declare », « laml », sans défaut."),
  list(critere_lissage = "auto")))
L_LAML <- sans("parametre_lissage"); L_LAML$critere_lissage <- "laml"
verifier("D critere_lissage_non_admis : wh_quadratique + laml sans parametre_lissage -> ce motif, jamais parametre_manquant (ADR 0017, points 3.2 et 3.4)", controle(
  rv(lissage = L_LAML),
  "lissage", "critere_lissage_non_admis",
  paste("Lissage : le critère « laml » n'est pas admis sous la méthode « wh_quadratique » : le paramètre h du critère",
        "quadratique (Lignes directrices, chap. 3, IV.2) est déclaré (critere_lissage = « declare »)."),
  list(methode = "wh_quadratique", critere_lissage = "laml")))
verifier("D critere_lissage_non_admis : avec parametre_lissage présent, même motif (gouvernants avant la présence)", controle(
  rv(lissage = avec(critere_lissage = "laml")), "lissage", "critere_lissage_non_admis"))
verifier("D ponderation absente sous wh_quadratique : parametre_manquant (module construire_table, gouvernant methode)", controle(
  rv(lissage = sans("ponderation")), "construire_table", "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « ponderation » sous methode = « wh_quadratique » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "ponderation", gouvernant = "methode", valeur = "wh_quadratique")))
verifier("D parametre_lissage absent sous declare : parametre_manquant (gouvernant critere_lissage)", controle(
  rv(lissage = sans("parametre_lissage")), "construire_table", "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « parametre_lissage » sous critere_lissage = « declare » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "parametre_lissage", gouvernant = "critere_lissage", valeur = "declare")))
verifier("D paramètre sans condition absent (ordre_difference) : parametre_manquant de la boucle des noms", controle(
  rv(lissage = sans("ordre_difference")), "construire_table", "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « ordre_difference » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "ordre_difference")))
verifier("D plage_ages_invalide sous le module lissage (integer refusé ; min > max) : message de la brique d'exposition", {
  m <- paste("La plage d'âges (plage_ages) doit être un vecteur double sans attribut de deux valeurs",
             "entières finies, 0 <= min <= max (un integer est refusé).")
  tous(list(controle(rv(lissage = avec(plage_ages = c(40L, 60L))), "lissage", "plage_ages_invalide", m,
                     list(plage_ages = c(40L, 60L))),
            controle(rv(lissage = avec(plage_ages = c(60, 40))), "lissage", "plage_ages_invalide", m)))
})
verifier("D ordre_difference_invalide : 5, 0, 2.5, 2L, c(2, 3), et z > max - min (plage 40 à 42, z = 3) ; message exact, details", {
  m <- paste("L'ordre des différences (ordre_difference) doit être un nombre double unique, sans attribut, à valeur",
             "entière dans {1, 2, 3, 4}, au plus égal à max(plage_ages) - min(plage_ages) (un integer est refusé).")
  tous(c(lapply(list(5, 0, 2.5, 2L, c(2, 3)), function(z)
           controle(rv(lissage = avec(ordre_difference = z)), "lissage", "ordre_difference_invalide", m,
                    list(ordre_difference = z, plage_ages = c(40, 60)))),
         list(controle(rv(lissage = avec(plage_ages = c(40, 42), ordre_difference = 3)), "lissage",
                       "ordre_difference_invalide", m, list(ordre_difference = 3, plage_ages = c(40, 42))))))
})
verifier("D ordre_difference = max - min admis (plage 40 à 42, z = 2)", is.null(rv(lissage = avec(plage_ages = c(40, 42)))))
verifier("D ponderation_invalide : message exact, details", controle(
  rv(lissage = avec(ponderation = "poids")), "lissage", "ponderation_invalide",
  paste("La pondération (ponderation) doit être une chaîne unique, sans attribut, prise dans « uniforme », « exposition »",
        "(uniforme : w = 1 ; exposition : w = E^i rapportée à sa moyenne sur les cellules définies du segment), sans",
        "défaut."),
  list(ponderation = "poids")))
verifier("D parametre_lissage_invalide : 0, -1, Inf, NA_real_, c(1, 2), 1L, nommé ; message exact", {
  m <- paste("Le paramètre de lissage (parametre_lissage : h sous wh_quadratique, lambda sous wh_poisson) doit être un",
             "nombre double unique, sans attribut, fini, strictement positif, sans défaut.")
  tous(lapply(list(0, -1, Inf, NA_real_, c(1, 2), 1L, c(h = 1)), function(h)
    controle(rv(lissage = avec(parametre_lissage = h)), "lissage", "parametre_lissage_invalide", m,
             list(parametre_lissage = h))))
})
verifier("D estimateur_lissage_absent (D1) : brut sans binomial ; message exact, details", controle(
  rv(estimateurs = "poisson"), "lissage", "estimateur_lissage_absent",
  "Lissage : la méthode « wh_quadratique » lit l'estimateur brut « binomial », absent de l'étape brut (estimateurs : « poisson ») ; l'ajouter à ses estimateurs.",
  list(methode = "wh_quadratique", estimateur = "binomial", estimateurs = "poisson")))
verifier("D par_annee = TRUE : valider_plan() lève binomial_par_annee (bloc brut avant le bloc lissage) ; appel direct : lissage_par_annee, message exact", {
  r1 <- rv(par_annee = TRUE, estimateurs = c("poisson", "binomial"))
  r2 <- refus_de(do.call(tablexp:::controler_parametres_lissage,
                         c(LISSAGE, list(brut = list(estimateurs = "binomial", niveau = 0.95), par_annee = TRUE))))
  tous(list(controle(r1, "brut", "binomial_par_annee"),
            controle(r2, "lissage", "lissage_par_annee",
                     "Lissage : découpage par année (par_annee = TRUE) non admis : le lissage porte sur l'âge seul.",
                     list(par_annee = TRUE))))
})
verifier("D appel direct de controler_parametres_lissage() : gouvernants rappelés (methode invalide refusée) ; paramètres conditionnels absents admis", {
  ctx <- list(brut = list(estimateurs = "binomial", niveau = 0.95), par_annee = FALSE)
  tous(list(controle(refus_de(do.call(tablexp:::controler_parametres_lissage, c(avec(methode = "x"), ctx))), "lissage",
                     "methode_lissage_invalide"),
            isTRUE(do.call(tablexp:::controler_parametres_lissage, c(sans(c("ponderation", "parametre_lissage")), ctx)))))
})
verifier("D ordre des refus de valeurs : plage avant ordre, ordre avant pondération, pondération avant h, h avant estimateur", {
  tous(list(controle(rv(lissage = avec(plage_ages = c(40L, 60L), ordre_difference = 9)), "lissage", "plage_ages_invalide"),
            controle(rv(lissage = avec(ordre_difference = 9, ponderation = "x")), "lissage", "ordre_difference_invalide"),
            controle(rv(lissage = avec(ponderation = "x", parametre_lissage = 0)), "lissage", "ponderation_invalide"),
            controle(rv(lissage = avec(parametre_lissage = 0), estimateurs = "poisson"), "lissage",
                     "parametre_lissage_invalide")))
})

# --- E. D9 : une seule étape productrice (ADR 0017, point 2.2) -------------------

POSITIONNEMENT <- list(methode = "smr", table_reference = "tv", plage_ages = c(40, 60), niveau = 0.95)
verifier("E tables_construites_multiples (module construire_table) : lissage + positionnement ; message exact, details", controle(
  rv(positionnement = POSITIONNEMENT), "construire_table", "tables_construites_multiples",
  paste("Le plan déclare plusieurs étapes qui produisent une table construite (« lissage », « positionnement ») : une",
        "seule est admise par construction (décision D9 du 11/11/2026)."),
  list(etape = c("lissage", "positionnement"))))
verifier("E lissage seul, positionnement seul : admis par valider_plan()",
         is.null(rv()) && is.null(rv(lissage = NULL, positionnement = POSITIONNEMENT)))
verifier("E tables_construites_multiples levé avant le contrôle des valeurs (lissage invalide, positionnement présent)", controle(
  rv(lissage = avec(methode = "x"), positionnement = POSITIONNEMENT), "construire_table", "tables_construites_multiples"))

# --- F. Avant exécution et en cours d'étape ---------------------------------------

verifier("F convention_lissage_non_admise (controler_entrees()) : convention retenue age_atteint_annee ; message exact, details", controle(
  rc(convention = "age_atteint_annee"), "lissage", "convention_lissage_non_admise",
  paste("Lissage : convention d'âge retenue « age_atteint_annee » non admise (admise : « dernier_anniversaire ») : la",
        "table lissée est une table par âge entier [x, x+1), lue sous cette convention par la validation et la",
        "fermeture."),
  list(convention = "age_atteint_annee", attendu = "dernier_anniversaire")))
verifier("F lissage_sans_segment (appel direct, table d'exposition vide) : message exact", {
  ex <- data.frame(sexe = character(0), age = integer(0), evenements = integer(0), exposition_initiale = numeric(0),
                   stringsAsFactors = FALSE)
  b <- list(binomial = data.frame(sexe = character(0), age = integer(0), evenements = integer(0),
                                  exposition_initiale = numeric(0), taux = numeric(0), etat = character(0),
                                  stringsAsFactors = FALSE))
  controle(refus_de(tablexp:::calculer_lissage(ex, b, "sexe", "dernier_anniversaire", "wh_quadratique", c(40, 60), 2,
                                               "declare", "exposition", NULL, 100, NULL)),
           "lissage", "lissage_sans_segment",
           paste("Lissage : la table d'exposition ne compte aucune cellule (aucune exposition sur la période",
                 "d'observation) : aucun segment à lisser sur la plage d'âges 40 à 60."),
           list(plage_ages = c(40, 60)))
})
verifier("F lissage_effectif_inferieur_aux_evenements : H, âge 50, D = 30 > E^i = 20 ; message exact", {
  eh <- EH; eh[11L] <- 20; dh <- DH; dh[11L] <- 30
  controle(rc(jeu(eh = eh, dh = dh, ef = EH * 1.1)), "lissage", "lissage_effectif_inferieur_aux_evenements",
           paste("Lissage : 1 cellule(s) de la plage d'âges 40 à 60 dont l'exposition initiale est inférieure au nombre",
                 "de décès (état effectif_inferieur_aux_evenements de l'estimateur binomial : q^ = D / E^i n'est pas",
                 "défini) : sexe = « H » (âge 50 : D = 30, E^i = 20). Le lissage quadratique est refusé."))
})
verifier("F lissage_plage_bornee_sans_exposition (D11) : plage 39 à 60, âge 39 sans ligne dans les deux segments (cités dans l'ordre de la table d'exposition) ; message exact", controle(
  rc(lissage = avec(plage_ages = c(39, 60))), "lissage", "lissage_plage_bornee_sans_exposition",
  paste("Lissage : 2 segment(s) dont la première ou la dernière cellule de la plage d'âges 39 à 60 n'a aucune exposition",
        "(poids nul) : sexe = « F » (âge(s) 39) ; sexe = « H » (âge(s) 39). Le lissage n'extrapole pas (décision D11) :",
        "réduire la plage d'âges aux âges observés ; l'extrapolation relève de la fermeture.")))
verifier("F lissage_plage_bornee_sans_exposition, borne supérieure : plage 40 à 61, âge 61 sans ligne ; message exact", controle(
  rc(lissage = avec(plage_ages = c(40, 61))), "lissage", "lissage_plage_bornee_sans_exposition",
  paste("Lissage : 2 segment(s) dont la première ou la dernière cellule de la plage d'âges 40 à 61 n'a aucune exposition",
        "(poids nul) : sexe = « F » (âge(s) 61) ; sexe = « H » (âge(s) 61). Le lissage n'extrapole pas (décision D11) :",
        "réduire la plage d'âges aux âges observés ; l'extrapolation relève de la fermeture.")))
verifier("F cas limite admis : exactement z = 3 cellules pondérées (âges 40, 50, 60 observés), première et dernière cellules exposées ; ddl effectifs = z", {
  d <- jeu(ages = c(40, 60, 50), eh = EH[c(1L, 21L, 11L)], dh = DH[c(1L, 21L, 11L)])
  x <- construire_table(d, ROLES, plan_l(lissage = avec(ordre_difference = 3)))
  s <- x$etapes$lissage$resultat$segments
  identical(s$cellules_ponderees, c(3L, 3L)) && identical(s$cellules_interpolees, c(18L, 18L)) &&
    isTRUE(proche(s$ddl_effectifs, c(3, 3), rel = 1e-12, abs = 0))
})
verifier("F lissage_cellules_insuffisantes : âges 40 et 60 seuls observés, z = 3 ; message exact", {
  d <- jeu(ages = c(40, 60), eh = EH[c(1L, 21L)], dh = DH[c(1L, 21L)])
  controle(rc(d, lissage = avec(ordre_difference = 3)), "lissage", "lissage_cellules_insuffisantes",
           paste("Lissage : 2 segment(s) comptent moins de z = 3 cellule(s) de poids non nul sur la plage d'âges 40 à 60 :",
                 "sexe = « F » (2 cellule(s) de poids non nul) ; sexe = « H » (2 cellule(s) de poids non nul). Le minimum",
                 "de M = F + h S n'est pas unique (Lignes directrices, chap. 3, IV.2.d, p. 33 : au moins z points",
                 "pivots, condition donnée comme suffisante, ici aussi nécessaire)."))
})
verifier("F lissage_sans_deces : segment F sans décès sur la plage ; message exact", controle(
  rc(jeu(df = rep(0, length(AGES)))), "lissage", "lissage_sans_deces",
  paste("Lissage : 1 segment(s) sans décès observé sur la plage d'âges 40 à 60 : sexe = « F ». La table lissée serait",
        "à mortalité nulle : le lissage est refusé.")))
verifier("F lissage_taux_hors_domaine (D4) : décès concentrés aux derniers âges, pondération uniforme, z = 2, h = 1e8 (droite des moindres carrés négative aux premiers âges) ; message cite segments, âges et valeurs en %.17g ; aucune table", {
  r <- rc(jeu(dh = c(rep(0, 15), 5, 20, 50, 100, 200, 400), df = c(rep(0, 15), 3, 12, 30, 60, 120, 240)),
          lissage = avec(ponderation = "uniforme", parametre_lissage = 1e8))
  ok <- controle(r, "lissage", "lissage_taux_hors_domaine")
  if (!isTRUE(ok)) return(ok)
  a <- r$details$age; g <- r$details$qx
  tous(list(all(g <= 0 | g >= 1), length(a) > 0L,
            grepl(sprintf("âge %d : g = %.17g", a[1L], g[1L]), conditionMessage(r), fixed = TRUE),
            startsWith(conditionMessage(r), sprintf("Lissage : %d taux lissé(s) hors de ]0, 1[ sur la plage d'âges 40 à 60 : ",
                                                    length(a)))))
})

verifier("F lissage_systeme_rang_deficient : h = 1e16, z = 2 (rang numérique perdu, tolérance de qr()) ; refus après évaluation de tous les segments, avant le contrôle de domaine ; message exact, details", controle(
  rc(lissage = avec(parametre_lissage = 1e16)), "lissage", "lissage_systeme_rang_deficient",
  paste("Lissage : système des moindres carrés augmentés numériquement de rang déficient pour 2 segment(s) sous h =",
        "10000000000000000, z = 2 (tolérance de rang par défaut de qr(), 1e-7) : sexe = « F » ; sexe = « H ». À cette",
        "valeur, la table lissée se confond numériquement avec le polynôme de degré z - 1 ajusté par moindres carrés",
        "pondérés (Lignes directrices, chap. 3, IV.2.a, p. 31) ; réduire parametre_lissage."),
  list(plage_ages = c(40, 60), parametre_lissage = 1e16, ordre_difference = 2,
       segments = data.frame(sexe = c("F", "H"), stringsAsFactors = FALSE))))
verifier("F lisser_quadratique() à rang perdu (h = 1e16) : rend le rang seul, sans erreur (l'appelant refuse)", {
  r <- lq(Q, WE, 1e16, 2, X)
  identical(names(r), "rang") && r$rang < N
})

# --- G. Résultat : structure, contrat, recalcul ------------------------------------

X1 <- construire_table(jeu(), ROLES, plan_l())
L1 <- X1$etapes$lissage
verifier("G etapes$lissage : parametres (paramètres du plan dans l'ordre du catalogue, puis convention, estimateur_brut, echelle_penalite, regle_force, hypothese, reference_methode, constantes, statuts) ; valeur calculée à la main", {
  p <- L1$parametres
  identical(names(p), c("methode", "plage_ages", "ordre_difference", "critere_lissage", "ponderation", "parametre_lissage",
                        "convention", "estimateur_brut", "echelle_penalite", "regle_force", "hypothese",
                        "reference_methode", "constantes", "statuts")) &&
    identical(p[1:6], LISSAGE) && identical(p$convention, "dernier_anniversaire") &&
    identical(p$estimateur_brut, "binomial") && identical(p$echelle_penalite, "q") &&
    identical(p$regle_force, "mu = -ln(1 - q) : force constante par âge entier, règle unique du moteur") &&
    grepl("chap. 3, IV.2.a (p. 31-32) et IV.2.d (p. 33), SHA-256", p$reference_methode, fixed = TRUE) &&
    identical(dim(p$constantes), c(0L, 3L)) && identical(names(p$constantes), c("constante", "valeur", "origine")) &&
    is.double(p$constantes$valeur) &&
    identical(p$statuts$grandeur, c("table_lissee/qx", "table_lissee/levier", "regle_force", "segments/ddl_effectifs", "segments/fidelite",
                                    "segments/regularite", "segments/critere", "segments/residu_conservation_0",
                                    "segments/residu_conservation_1"))
})
verifier("G table_lissee : data.frame nu, colonnes sexe, age (integer), qx, etat, levier (double), poids ; segments dans l'ordre de la table d'exposition, âges croissants", {
  t <- L1$resultat$table_lissee
  identical(class(t), "data.frame") && identical(names(t), c("sexe", "age", "qx", "etat", "levier", "poids")) &&
    is.integer(t$age) && identical(t$age, rep(40:60, 2L)) && is.double(t$levier) &&
    identical(unique(t$sexe), unique(X1$etapes$exposition$resultat$exposition$sexe)) &&
    all(t$qx > 0 & t$qx < 1) && all(t$etat == "observee") && all(t$poids > 0)
})
verifier("G segments : colonnes et types (dénombrements integer, ADR 0009, point 9.6) ; etat produite", {
  s <- L1$resultat$segments
  identical(names(s), c("sexe", "cellules", "cellules_ponderees", "cellules_interpolees", "evenements",
                        "parametre_lissage", "ddl_effectifs", "fidelite", "regularite", "critere",
                        "residu_conservation_0", "residu_conservation_1", "etat")) &&
    all(vapply(s[c("cellules", "cellules_ponderees", "cellules_interpolees", "evenements")], is.integer, NA)) &&
    all(vapply(s[c("parametre_lissage", "ddl_effectifs", "fidelite", "regularite", "critere", "residu_conservation_0",
                   "residu_conservation_1")], is.double, NA)) &&
    identical(s$cellules, c(21L, 21L)) && identical(s$etat, c("produite", "produite"))
})
verifier("G diagnostics : noms, dénombrements integer, exposition_initiale_hors_plage double, identité plage + hors plage = total", {
  d <- L1$diagnostics
  identical(names(d), c("cellules", "cellules_interpolees", "evenements_plage", "cellules_hors_plage",
                        "evenements_hors_plage", "exposition_initiale_hors_plage", "evenements_total")) &&
    all(vapply(d[c("cellules", "cellules_interpolees", "evenements_plage", "cellules_hors_plage",
                   "evenements_hors_plage", "evenements_total")], is.integer, NA)) &&
    is.double(d$exposition_initiale_hors_plage) && identical(d$evenements_plage + d$evenements_hors_plage, d$evenements_total)
})
verifier("G table_lissee recalculée par lisser_quadratique() sur les entrées de l'étape (q^ binomial, w = E^i / moyenne) : identité exacte, segment H", {
  b <- X1$etapes$brut$resultat$binomial
  k <- b$sexe == "H"
  w <- b$exposition_initiale[k] / mean(b$exposition_initiale[k])
  r <- lq(b$taux[k], w, 100, 2, b$age[k])
  t <- L1$resultat$table_lissee
  s <- L1$resultat$segments
  identical(t$qx[t$sexe == "H"], r$g) && identical(t$poids[t$sexe == "H"], w) &&
    identical(t$levier[t$sexe == "H"], r$levier) &&
    identical(s$ddl_effectifs[s$sexe == "H"], r$ddl_effectifs) &&
    identical(s$residu_conservation_1[s$sexe == "H"], r$residu_conservation_1)
})
verifier("G contrat de la table construite : table_construite() rend table_lissee et ddl_effectifs (clés, ddl) (ADR 0017, point 1.5)", {
  r <- tablexp:::table_construite(X1$etapes, "sexe", "lissage")
  identical(r$table, L1$resultat$table_lissee) && identical(r$plage_ages, c(40, 60)) &&
    identical(r$ddl_effectifs, L1$resultat$segments[c("sexe", "ddl_effectifs")])
})
verifier("G cellule intérieure sans ligne (H, âge 50) : état interpolee, poids 0, qx dans ]0, 1[ ; Ē^i sur les cellules définies ; décomptes", {
  d <- jeu(); d <- d[!(d$sexe == "H" & d$age == 50), ]
  x <- construire_table(d, ROLES, plan_l(lissage = avec(ordre_difference = 3, parametre_lissage = 1e3)))
  t <- x$etapes$lissage$resultat$table_lissee
  s <- x$etapes$lissage$resultat$segments
  i <- which(t$sexe == "H" & t$age == 50L)
  eh <- EH[-11L]
  identical(t$etat[i], "interpolee") && identical(t$poids[i], 0) && t$qx[i] > 0 && t$qx[i] < 1 &&
    identical(t$poids[t$sexe == "H" & t$age != 50L], eh / mean(eh)) &&
    identical(s$cellules_interpolees[s$sexe == "H"], 1L) && identical(s$cellules_interpolees[s$sexe == "F"], 0L) &&
    identical(s$cellules_ponderees[s$sexe == "H"], 20L) &&
    identical(x$etapes$lissage$diagnostics$cellules_interpolees, 1L)
})
verifier("G levier (ADR 0017, point 5.5) : 0 exactement sur la cellule interpolee (H, 50), > 0 ailleurs ; somme de levier par segment, dans l'ordre de la table, identical() à segments$ddl_effectifs", {
  d <- jeu(); d <- d[!(d$sexe == "H" & d$age == 50), ]
  x <- construire_table(d, ROLES, plan_l(lissage = avec(ordre_difference = 3, parametre_lissage = 1e3)))
  t <- x$etapes$lissage$resultat$table_lissee
  s <- x$etapes$lissage$resultat$segments
  int <- t$etat == "interpolee"
  sommes <- vapply(s$sexe, function(v) sum(t$levier[t$sexe == v]), 0, USE.NAMES = FALSE)
  identical(sum(int), 1L) && identical(t$levier[int], 0) && all(t$levier[!int] > 0) &&
    identical(sommes, s$ddl_effectifs) &&
    identical(vapply(L1$resultat$segments$sexe, function(v) sum(L1$resultat$table_lissee$levier[
      L1$resultat$table_lissee$sexe == v]), 0, USE.NAMES = FALSE), L1$resultat$segments$ddl_effectifs)
})
verifier("G pondération exposition : somme E^i g = somme D par segment à 1e-12 près en relatif (identité de conservation, sur l'objet)", {
  b <- X1$etapes$brut$resultat$binomial
  t <- L1$resultat$table_lissee
  e <- vapply(c("H", "F"), function(s) abs(sum(b$exposition_initiale[b$sexe == s] * t$qx[t$sexe == s]) -
                                           sum(b$evenements[b$sexe == s])) / sum(b$evenements[b$sexe == s]), 0)
  all(e <= 1e-12)
})
verifier("G hors plage : cellules, événements et E^i hors de la plage 45 à 55 ; identité des décomptes", {
  x <- construire_table(jeu(), ROLES, plan_l(lissage = avec(plage_ages = c(45, 55))))
  d <- x$etapes$lissage$diagnostics
  hors <- !(AGES %in% 45:55)
  identical(d$cellules, 22L) && identical(d$cellules_hors_plage, 20L) &&
    identical(d$evenements_hors_plage, as.integer(sum(DH[hors]) + sum(round(DH * 0.6)[hors]))) &&
    identical(d$exposition_initiale_hors_plage, sum(x$etapes$brut$resultat$binomial$exposition_initiale[c(hors, hors)])) &&
    identical(d$evenements_plage + d$evenements_hors_plage, d$evenements_total)
})
verifier("G exposition_initiale_hors_plage somme E^i et non E^c (agrégats déclarant les deux expositions, E^c = 0,9 E^i ; plage 45 à 55)", {
  d <- jeu(); d$exposition_centrale <- 0.9 * d$exposition_initiale
  x <- construire_table(d, c(ROLES, exposition_centrale = "exposition_centrale"),
                        plan_l(estimateurs = c("poisson", "binomial"), lissage = avec(plage_ages = c(45, 55))))
  hors <- !(d$age %in% 45:55)
  v <- x$etapes$lissage$diagnostics$exposition_initiale_hors_plage
  ex <- x$etapes$exposition$resultat$exposition
  identical(v, sum(x$etapes$brut$resultat$binomial$exposition_initiale[!(ex$age %in% 45:55)])) &&
    isTRUE(proche(v, sum(d$exposition_initiale[hors]), rel = 1e-12, abs = 0)) &&
    !isTRUE(proche(v, sum(d$exposition_centrale[hors]), rel = 1e-3, abs = 0))
})
verifier("G sans segmentation : un segment, table sans clé", {
  d <- jeu()[seq_along(AGES), ]; d$sexe <- NULL
  x <- construire_table(d, ROLES[-1L], plan_l(segmentation = character(0)))
  t <- x$etapes$lissage$resultat$table_lissee
  identical(names(t), c("age", "qx", "etat", "levier", "poids")) && identical(nrow(x$etapes$lissage$resultat$segments), 1L)
})
verifier("G avertissements : aucun sous wh_quadratique (aucune ligne d'étape lissage)",
         !any(X1$avertissements$etape == "lissage"))

# --- H. Plan : aller-retour et canonisation ; rapport figé --------------------------

verifier("H canoniser_plan() idempotente sur un plan de lissage ; aller-retour ecrire_plan() / lire_plan() identique (identité exacte)", {
  cn <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                             tablexp:::ordres_canoniques())
  p <- plan_l(lissage = LISSAGE[c(6L, 5L, 4L, 3L, 2L, 1L)])
  c1 <- cn(p)
  identical(names(c1$etapes$lissage), names(LISSAGE)) && identical(cn(c1), c1) &&
    identical(lire_plan(ecrire_plan(c1)), c1) && identical(X1$fiche$plan, c1)
})
verifier("H rapport figé : section etape_lissage (titre, trois mentions, racine etapes/lissage, aucune mention d'unité) avant etape_positionnement ; absente du plan : « non demandée par le plan »", {
  h <- rawToChar(ecrire_rapport(X1)); Encoding(h) <- "UTF-8"
  x0 <- construire_table(jeu(), ROLES, plan_l(lissage = NULL))
  h0 <- rawToChar(ecrire_rapport(x0)); Encoding(h0) <- "UTF-8"
  m <- tablexp:::MENTIONS_RAPPORT
  echapper <- tablexp:::echapper_html
  tous(list(grepl("<section id=\"etape_lissage\">", h, fixed = TRUE),
            regexpr("id=\"etape_lissage\"", h, fixed = TRUE) < regexpr("id=\"etape_positionnement\"", h, fixed = TRUE),
            grepl("Étape lissage : lissage de Whittaker-Henderson et table lissée", h, fixed = TRUE),
            all(vapply(c("lissage_quadratique", "lissage_conservation", "lissage_echelle"), function(k)
              grepl(echapper(m[[k]]), h, fixed = TRUE), NA)),
            grepl("etapes/lissage/resultat/table_lissee", h, fixed = TRUE),
            # Aucune mention d'unité sous wh_quadratique (amendement d'actuary, point 6).
            { sec <- sub("</section>.*$", "", sub("^.*<section id=\"etape_lissage\">", "", h))
              !grepl(echapper(m[["validation_unite_contrat"]]), sec, fixed = TRUE) &&
                !grepl(echapper(m[["validation_unite_non_determinee"]]), sec, fixed = TRUE) },
            grepl(echapper(paste0(m[["etape_absente_1"]], "lissage", m[["etape_absente_2"]], "lissage",
                                  m[["etape_absente_3"]])), h0, fixed = TRUE)))
})

# --- I. Contre-vérification C1 (WH::WH() de WH 2.0.0, hors CI) ---------------------
#
# Valeurs écrites par tests/contre_verification/wh_lissage_quadratique.R
# (hors CI, ADR 0002, troisième annotation du 16/10/2026) dans
# tests/contre_verification/valeurs_wh_lissage_quadratique.csv, sur le jeu
# fixé jeu_wh_lissage_quadratique.csv (trois jeux : sans segmentation ; par
# sexe ; par sexe avec une cellule intérieure sans exposition, absente des
# données du moteur). Écart relatif <= 1e-10 sur qx contre y_hat et sur
# ddl_effectifs contre la somme de edf (corps de #223, C1 ; recoupement).

DOSSIER_CV <- file.path(RACINE, "tests", "contre_verification")
FICHIER_WH <- file.path(DOSSIER_CV, "valeurs_wh_lissage_quadratique.csv")
verifier("I C1 fichier de valeurs : présent, en-tête « # WH : 2.0.0 » et version de R consignées", {
  if (!file.exists(FICHIER_WH)) return("fichier de valeurs absent (à produire par wh_lissage_quadratique.R)")
  e <- readLines(FICHIER_WH, warn = FALSE, encoding = "UTF-8")
  e <- e[startsWith(e, "#")]
  "# WH : 2.0.0" %in% e && any(startsWith(e, "# R : R version "))
})
verifier("I C1 moteur contre WH::WH(y, wt, lambda = h, q = z, reg = TRUE) : qx contre y_hat et ddl_effectifs contre somme de edf, écart relatif <= 1e-10, trois jeux", {
  if (!file.exists(FICHIER_WH)) return("fichier de valeurs absent")
  jeu_cv <- utils::read.csv(file.path(DOSSIER_CV, "jeu_wh_lissage_quadratique.csv"), comment.char = "#",
                            stringsAsFactors = FALSE)
  l <- readLines(FICHIER_WH, warn = FALSE, encoding = "UTF-8")
  val <- utils::read.csv(text = l[!startsWith(l, "#")], stringsAsFactors = FALSE, colClasses = "character")
  val$age <- as.integer(val$age); val$y_hat <- as.numeric(val$y_hat); val$ddl <- as.numeric(val$ddl)
  ecarts <- lapply(unique(jeu_cv$jeu), function(j) {
    t <- jeu_cv[jeu_cv$jeu == j, , drop = FALSE]
    seg <- if (all(t$segment == "ensemble")) character(0) else "sexe"
    plage <- as.double(range(t$age))
    t <- t[t$exposition_initiale > 0, , drop = FALSE]
    d <- data.frame(age = as.double(t$age), evenements = as.double(t$evenements),
                    exposition_initiale = t$exposition_initiale, stringsAsFactors = FALSE)
    roles <- c(age = "age", evenements = "evenements", exposition_initiale = "exposition_initiale")
    if (length(seg)) { d$sexe <- t$segment; roles <- c(roles, sexe = "sexe") }
    x <- construire_table(d, roles, plan_l(segmentation = seg, lissage = list(
      methode = "wh_quadratique", plage_ages = plage, ordre_difference = as.double(unique(t$ordre_difference)),
      critere_lissage = "declare", ponderation = unique(t$ponderation),
      parametre_lissage = as.double(unique(t$parametre_lissage)))))
    tl <- x$etapes$lissage$resultat$table_lissee
    sg <- x$etapes$lissage$resultat$segments
    v <- val[val$jeu == j, , drop = FALSE]
    cle_t <- paste(if (length(seg)) tl$sexe else "ensemble", tl$age)
    k <- match(cle_t, paste(v$segment, v$age))
    if (anyNA(k) || nrow(v) != nrow(tl)) return(sprintf("%s : cellules différentes", j))
    ddl_v <- v$ddl[match(if (length(seg)) sg$sexe else "ensemble", v$segment)]
    c(max(abs(tl$qx - v$y_hat[k]) / abs(v$y_hat[k])), max(abs(sg$ddl_effectifs - ddl_v) / ddl_v))
  })
  if (any(vapply(ecarts, is.character, NA))) return(unlist(ecarts[vapply(ecarts, is.character, NA)]))
  e <- unlist(ecarts)
  if (all(e <= 1e-10)) TRUE else sprintf("écart relatif max %.3g", max(e))
})
