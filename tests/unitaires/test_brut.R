# -----------------------------------------------------------------------------
#  tests/unitaires/test_brut.R : module `brut` (issue #14 ; E2.12, E2.14,
#  E2.20 ; ADR 0007, points 5 et 6 ; spécification d'actuary du 17/10/2026,
#  A.1 à A.7, cas 1 à 9 de A.6 ; décisions du mainteneur sur A.8)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - forme close : bornes à D = 0, D = 1 et D = E^i écrites en dur d'après la
#    spécification (A.6, point 1), indépendantes de qgamma et de qbeta ;
#    résidus des équations qui définissent les bornes (série de Poisson,
#    somme binomiale, équations de D = 1), calculés sans pgamma ni pbeta ;
#  - valeur calculée à la main : taux et bornes des cas H, F et du cas
#    composite C, écrits en dur d'après la spécification (A.6, points 2 à 7),
#    états, décomptes et codes ;
#  - recoupement : stats::poisson.test et stats::binom.test (n entier
#    seulement) ; le moteur repose sur la même fonction (qgamma, qbeta), ce
#    que le libellé dit : le recoupement vérifie la formule, les
#    branchements et alpha / 2, pas la fonction spéciale ;
#  - identité exacte : colonnes reprises de la table d'exposition, mu E^c = D.
#  Tolérances déclarées dans chaque test : 1e-14 (relative) pour les
#  quotients D / E, 1e-13 (relative) pour les bornes ; égalité exacte pour
#  les entiers, les états et les codes. z = 1,959963984540054 écrit en dur.
# -----------------------------------------------------------------------------

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
TQ <- 1e-14   # quotients D / E
TB <- 1e-13   # bornes
Z <- 1.959963984540054
A2 <- 0.025   # alpha / 2 au niveau 0,95
taux_poisson <- tablexp:::taux_poisson
taux_binomial <- tablexp:::taux_binomial
controler_parametres_brut <- tablexp:::controler_parametres_brut
calculer_taux_bruts <- tablexp:::calculer_taux_bruts

plan_brut <- function(estimateurs = c("poisson", "binomial"), niveau = 0.95, convention = "plus_fine",
                      par_annee = FALSE, delai = 0, transition = VD, segmentation = character(0))
  list(transition = transition, periode = PER, delai_declaration = delai, segmentation = segmentation,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = convention, par_annee = par_annee),
                     brut = list(estimateurs = estimateurs, niveau = niveau)))

# Construction d'un cas d'une tête (lignes : naissance ; entrée -> sortie,
# cause), identifiant de tête facultatif ; rend les tables Poisson et
# binomiale, la table d'exposition et l'objet.
construire_cas <- function(naissance, entree, sortie, cause, tete = NULL, ...) {
  d <- data.frame(n = D(naissance), e = D(entree), s = D(sortie), c = cause, stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
  if (!is.null(tete)) { d$t <- tete; roles <- c(roles, id_tete = "t") }
  x <- construire_table(d, roles, plan_brut(...))
  list(p = x$etapes$brut$resultat$poisson, b = x$etapes$brut$resultat$binomial,
       e = x$etapes$exposition$resultat$exposition, objet = x)
}
BORNES <- c("borne_inf_exacte", "borne_sup_exacte", "borne_inf_asymptotique", "borne_sup_asymptotique")
# Compare une ligne (taux, quatre bornes) aux attendus : taux à TQ, bornes à
# TB, en relatif ; NA attendus à leur place.
controler_ligne <- function(ligne, taux, bornes) {
  r1 <- proche(ligne$taux, taux, rel = TQ, abs = 0)
  r2 <- proche(unlist(ligne[BORNES]), bornes, rel = TB, abs = 0)
  if (!isTRUE(r1)) paste("taux :", r1) else if (!isTRUE(r2)) paste("bornes :", r2) else TRUE
}

# --- 1. formes closes (indépendantes de qgamma et de qbeta) -------------------

E <- c(1, 1.7, 1 / 3, 118 / 365)
p0 <- taux_poisson(rep(0L, 4L), E, 0.95)
verifier("Poisson D = 0 (forme close) : [0 ; -ln(0,025) / E = 3,688879454113936 / E], asymptotiques NA, tolérance 1e-13",
         isTRUE(proche(p0$borne_sup_exacte, 3.688879454113936 / E, rel = TB, abs = 0)) &&
           identical(p0$borne_inf_exacte, rep(0, 4L)) && identical(p0$taux, rep(0, 4L)) &&
           all(is.na(p0$borne_inf_asymptotique)) && all(is.na(p0$borne_sup_asymptotique)) &&
           identical(p0$etat, rep("defini", 4L)))
p1 <- taux_poisson(rep(1L, 4L), E, 0.95)
verifier("Poisson D = 1 (forme close) : [-ln(0,975) / E = 0,02531780798428990 / E ; 5,571643390938899 / E], tolérance 1e-13",
         proche(c(p1$borne_inf_exacte, p1$borne_sup_exacte),
                c(0.02531780798428990 / E, 5.571643390938899 / E), rel = TB, abs = 0))
verifier("Poisson D = 1 (forme close, résidu) : u = borne sup x E résout (1 + u) e^-u = 0,025 ; u par Newton sans qgamma, tolérance 1e-13", {
  u <- 5
  for (k in 1:60) u <- u - ((1 + u) * exp(-u) - A2) / (-u * exp(-u))
  r <- proche(p1$borne_sup_exacte * E, rep(u, 4L), rel = TB, abs = 0)
  if (!isTRUE(r)) r else proche((1 + p1$borne_sup_exacte * E) * exp(-p1$borne_sup_exacte * E), rep(A2, 4L),
                                rel = 0, abs = TB)
})
verifier("Poisson D = 1 (forme close) : asymptotique mu exp(-+ z), z = 1,959963984540054 écrit en dur, tolérance 1e-13",
         proche(c(p1$borne_inf_asymptotique, p1$borne_sup_asymptotique),
                c(exp(-Z) / E, exp(Z) / E), rel = TB, abs = 0))
verifier("Poisson D = 0 à 5, E = 1,7 (forme close : série explicite, sans pgamma) : P(X >= D | m = inf E) = 0,025 et P(X <= D | m = sup E) = 0,025, tolérance 1e-13 absolue", {
  serie <- function(m, K) { s <- 0; t <- exp(-m); for (k in 0:K) { if (k > 0) t <- t * m / k; s <- s + t }; s }
  p <- taux_poisson(0:5, rep(1.7, 6L), 0.95)
  res <- c(vapply(1:5, function(d) 1 - serie(p$borne_inf_exacte[d + 1L] * 1.7, d - 1L) - A2, numeric(1L)),
           vapply(0:5, function(d) serie(p$borne_sup_exacte[d + 1L] * 1.7, d) - A2, numeric(1L)))
  if (p$borne_inf_exacte[1L] != 0) "borne inférieure non nulle à D = 0"
  else proche(res, rep(0, 11L), rel = 0, abs = TB)
})

NR <- c(1.480515008608429, 3.5, 10)
b0 <- taux_binomial(rep(0L, 3L), NR, 0.95, 1e-9)
verifier("binomial D = 0, E^i réel (forme close) : [0 ; 1 - 0,025^(1/n)], asymptotiques NA, tolérance 1e-13",
         isTRUE(proche(b0$borne_sup_exacte, 1 - A2^(1 / NR), rel = TB, abs = 0)) &&
           identical(b0$borne_inf_exacte, rep(0, 3L)) && all(is.na(b0$borne_inf_asymptotique)))
b1 <- taux_binomial(rep(1L, 3L), NR, 0.95, 1e-9)
verifier("binomial D = 1, E^i réel (forme close) : borne inférieure 1 - 0,975^(1/n), tolérance 1e-13",
         proche(b1$borne_inf_exacte, 1 - 0.975^(1 / NR), rel = TB, abs = 0))
verifier("binomial D = 1, E^i réel (forme close, résidu) : u = borne sup résout (1 - u)^(n-1) (1 + (n-1) u) = 0,025, tolérance 1e-13 absolue",
         proche((1 - b1$borne_sup_exacte)^(NR - 1) * (1 + (NR - 1) * b1$borne_sup_exacte), rep(A2, 3L),
                rel = 0, abs = TB))
verifier("binomial D = E^i = 3 (forme close) : q = 1, [(0,025)^(1/3) = 0,2924017738212867 ; 1], asymptotiques NA",
         {
           b <- taux_binomial(3L, 3, 0.95, 1e-9)
           r <- proche(b$borne_inf_exacte, 0.2924017738212867, rel = TB, abs = 0)
           if (!isTRUE(r)) r else identical(c(b$taux, b$borne_sup_exacte), c(1, 1)) &&
             is.na(b$borne_inf_asymptotique) && is.na(b$borne_sup_asymptotique)
         })
verifier("binomial n = 2, 5, 12, D = 0 à n (forme close : somme binomiale explicite, sans pbeta) : P(X >= D | inf) = 0,025, P(X <= D | sup) = 0,025, tolérance 1e-12 absolue", {
  res <- numeric(0)
  for (n in c(2, 5, 12)) {
    b <- taux_binomial(0:n, rep(n, n + 1), 0.95, 1e-9)
    pk <- function(q, ks) sum(choose(n, ks) * q^ks * (1 - q)^(n - ks))
    for (d in 0:n) {
      if (d > 0) res <- c(res, pk(b$borne_inf_exacte[d + 1L], d:n) - A2)
      if (d < n) res <- c(res, pk(b$borne_sup_exacte[d + 1L], 0:d) - A2)
    }
  }
  proche(res, rep(0, length(res)), rel = 0, abs = 1e-12)
})
verifier("binomial n = 2, 5, 12, D = 0 à n (recoupement : stats::binom.test ; le moteur repose sur la même fonction, qbeta), tolérance 1e-13", {
  ecarts <- character(0)
  for (n in c(2, 5, 12)) {
    b <- taux_binomial(0:n, rep(n, n + 1), 0.95, 1e-9)
    bt <- t(vapply(0:n, function(d) stats::binom.test(d, n)$conf.int[1:2], numeric(2L)))
    r <- proche(c(b$borne_inf_exacte, b$borne_sup_exacte), c(bt[, 1L], bt[, 2L]), rel = TB, abs = 1e-300)
    if (!isTRUE(r)) ecarts <- c(ecarts, sprintf("n = %d : %s", n, r))
  }
  if (length(ecarts)) ecarts else TRUE
})
verifier("Poisson D = 0 à 5, E = 1,7 (recoupement : stats::poisson.test ; le moteur repose sur la même fonction, qgamma), tolérance 1e-13", {
  p <- taux_poisson(0:5, rep(1.7, 6L), 0.95)
  pt <- t(vapply(0:5, function(d) stats::poisson.test(d, 1.7)$conf.int[1:2], numeric(2L)))
  proche(c(p$borne_inf_exacte, p$borne_sup_exacte), c(pt[, 1L], pt[, 2L]), rel = TB, abs = 0)
})
verifier("binomial, |E^i - D| <= 1e-9 : le calcul prend E^i := D (q = 1, état defini), au-delà effectif inférieur", {
  b <- taux_binomial(c(1L, 1L, 1L), c(1 - 2.2e-16, 1 + 2.2e-16, 1 - 1e-6), 0.95, 1e-9)
  identical(b$etat, c("defini", "defini", "effectif_inferieur_aux_evenements")) &&
    identical(b$taux[1:2], c(1, 1)) && is.na(b$taux[3L]) &&
    identical(b$effectif_egal, c(TRUE, TRUE, FALSE)) && all(is.na(b$borne_inf_asymptotique))
})

# --- 2. cas à un seul événement (valeurs calculées à la main, A.6, point 2) ---

UN <- list(
  H2 = list(construire_cas("1945-10-15", "2016-05-20", "2019-02-10", "deces"), 73L,
            c(3.09322033898305, 0.078313558595473, 17.2343206584127, 0.435721824949359, 21.9589919935969)),
  H4a = list(construire_cas("1940-12-05", "2022-01-01", "2024-11-15", "deces"), 83L,
             c(1.05780346820809, 0.0267812650932084, 5.89370370255386, 0.149005892595716, 7.50942233129673)),
  H7 = list(construire_cas("1948-08-20", c("2016-01-01", "2018-07-01"), c("2019-01-01", "2021-05-10"),
                           c("terme", "deces"), tete = c("T", "T")), 72L,
            c(1.38783269961977, 0.0351368818032921, 7.73250888856539, 0.195494963285264, 9.85232340397124)),
  H9 = list(construire_cas("1970-03-03", "2017-05-05", "2021-08-08", "rachat", transition = RA), 51L,
            c(2.31012658227848, 0.0584873412295305, 12.8712015043842, 0.32541250217737, 16.3997535142053)),
  H12 = list(construire_cas("1945-10-15", "2016-05-20", "2019-02-10", "deces", convention = "age_atteint_annee"),
             74L, c(9.125, 0.231024997856645, 50.8412459423175, 1.28537938360061, 64.7790263811109)),
  F2 = list(construire_cas("1952-09-30", c("2016-01-01", "2018-01-01"), c("2018-01-01", "2021-05-10"),
                           c("terme", "deces"), tete = c("T", "T")), 68L,
            c(1.64414414414414, 0.0416261257399361, 9.16058485447161, 0.231599888937047, 11.6718966452452)),
  F4 = list(construire_cas("1945-06-06", c("2016-01-01", "2019-01-01"), c("2022-03-01", "2022-03-01"),
                           c("deces", "deces"), tete = c("T", "T")), 76L,
            c(1.36194029850746, 0.0344813429636784, 7.58824566303246, 0.191847669194121, 9.66851140016581)))
# Les attendus de la spécification sont écrits à 15 chiffres significatifs :
# tolérance relative 1e-13 sur le taux comme sur les bornes.
for (nom in names(UN)) {
  cas <- UN[[nom]][[1L]]; age <- UN[[nom]][[2L]]; att <- UN[[nom]][[3L]]
  verifier(sprintf("%s, âge %d, D = 1 (valeur calculée à la main, A.6) : Poisson mu = %.15g, exact [%.15g ; %.15g], asymptotique [%.15g ; %.15g], tolérance 1e-13",
                   nom, age, att[1], att[2], att[3], att[4], att[5]), {
    l <- cas$p[cas$p$age == age, , drop = FALSE]
    if (nrow(l) != 1L || l$evenements != 1L || l$etat != "defini") "ligne de l'événement absente ou non définie"
    else {
      r1 <- proche(l$taux, att[1], rel = TB, abs = 0)
      r2 <- proche(unlist(l[BORNES]), att[2:5], rel = TB, abs = 0)
      if (!isTRUE(r1)) r1 else r2
    }
  })
  verifier(sprintf("%s, âge %d (valeur calculée à la main) : binomial E^i = 1, q = 1, exact [0,025 ; 1], asymptotique NA", nom, age), {
    l <- cas$b[cas$b$age == age, , drop = FALSE]
    r <- proche(c(l$exposition_initiale, l$taux, l$borne_inf_exacte, l$borne_sup_exacte), c(1, 1, A2, 1),
                rel = TB, abs = 0)
    if (!isTRUE(r)) r else identical(l$etat, "defini") && is.na(l$borne_inf_asymptotique) &&
      is.na(l$borne_sup_asymptotique)
  })
}
verifier("H2, âge 73 (valeur calculée à la main) : probabilite = 1 - exp(-mu) et bornes transformées par 1 - exp(-.), tolérance 1e-13", {
  l <- UN$H2[[1L]]$p[UN$H2[[1L]]$p$age == 73L, ]
  att <- c(3.09322033898305, 0.078313558595473, 17.2343206584127, 0.435721824949359, 21.9589919935969)
  proche(unlist(l[c("probabilite", "probabilite_borne_inf_exacte", "probabilite_borne_sup_exacte",
                    "probabilite_borne_inf_asymptotique", "probabilite_borne_sup_asymptotique")]),
         1 - exp(-att), rel = TB, abs = 0)
})

verifier("H4c, délai 30 j, âge 83 (cas complémentaire de test_exposition.R, calculé à la main par coder) : exposition initiale « cellule » E^i = 346/366 + 20/366 = 1, et non la fenêtre 346/366 + 17/366 ; q = 1, [0,025 ; 1]", {
  cas <- construire_cas("1940-12-05", "2022-01-01", "2024-11-15", "deces", delai = 30)
  l <- cas$b[cas$b$age == 83L, ]
  proche(c(l$exposition_initiale, l$taux, l$borne_inf_exacte, l$borne_sup_exacte), c(1, 1, A2, 1),
         rel = TB, abs = 0)
})

# --- 3. exposition nulle (valeurs calculées à la main, A.6, point 3) ----------

H8 <- construire_cas("1955-04-12", "2019-09-01", "2020-04-12", "deces")
verifier("H8, âge 65 (valeur calculée à la main) : Poisson evenement_sans_exposition, taux et bornes NA ; binomial defini, E^i = 1, [0,025 ; 1]", {
  lp <- H8$p[H8$p$age == 65L, ]; lb <- H8$b[H8$b$age == 65L, ]
  if (!identical(lp$etat, "evenement_sans_exposition") || !all(is.na(unlist(lp[c("taux", BORNES, "probabilite")]))))
    "Poisson"
  else if (!identical(lb$etat, "defini")) "état binomial"
  else proche(c(lb$exposition_initiale, lb$taux, lb$borne_inf_exacte, lb$borne_sup_exacte), c(1, 1, A2, 1),
              rel = TB, abs = 0)
})
verifier("H8 : avertissement poisson_evenement_sans_exposition (1 cellule, 1 événement), aucun avertissement binomial", {
  a <- H8$objet$avertissements
  k <- a$etape == "brut"
  identical(a$code[k], "poisson_evenement_sans_exposition") && identical(a$nombre[k], 1) &&
    grepl("1 événement", a$message[k], fixed = TRUE)
})
for (k in list(list("H10b", "1950-05-05", "2005-01-01", "2015-01-01", 64L, 124 / 365),
               list("H11", "1980-01-01", "2018-06-15", "2018-06-15", 38L, 200 / 365))) {
  cas <- construire_cas(k[[2]], k[[3]], k[[4]], "deces")
  verifier(sprintf("%s, âge %d (valeur calculée à la main) : Poisson evenement_sans_exposition ; binomial effectif_inferieur_aux_evenements, E^i = %s, taux et bornes NA ; deux avertissements",
                   k[[1]], k[[5]], if (k[[1]] == "H10b") "124/365" else "200/365"), {
    lp <- cas$p[cas$p$age == k[[5]], ]; lb <- cas$b[cas$b$age == k[[5]], ]
    codes <- cas$objet$avertissements$code[cas$objet$avertissements$etape == "brut"]
    if (!identical(lp$etat, "evenement_sans_exposition") || !is.na(lp$taux)) "Poisson"
    else if (!identical(lb$etat, "effectif_inferieur_aux_evenements") || !all(is.na(unlist(lb[c("taux", BORNES)]))))
      "binomial"
    else if (!identical(lb$evenements, 1L)) "D absent de la ligne"
    else if (!identical(codes, c("poisson_evenement_sans_exposition", "binomial_effectif_inferieur_aux_evenements")))
      sprintf("avertissements %s", paste(codes, collapse = ", "))
    else proche(lb$exposition_initiale, k[[6]], rel = TQ, abs = 0)
  })
}

# --- 4. D = 0 partout (formes closes, A.6, point 4) ----------------------------

ZERO <- list(
  H1 = construire_cas("1960-07-01", "2020-03-01", NA, "en_cours"),
  H3 = construire_cas("1950-01-10", "2010-06-01", NA, "en_cours"),
  H4b = construire_cas("1940-12-05", "2022-01-01", "2024-11-15", "deces", delai = 90),
  H5 = construire_cas("1955-06-30", "2023-01-01", "2025-02-01", "deces"),
  H6 = construire_cas("1964-02-29", "2021-01-01", NA, "en_cours"),
  F1 = construire_cas("1950-03-15", c("2016-01-01", "2018-07-01"), c("2019-12-31", NA), c("rachat", "en_cours"),
                      tete = c("T", "T")),
  F3 = construire_cas("1949-11-11", c("2016-01-01", "2017-01-02"), c("2017-01-01", NA), c("terme", "en_cours"),
                      tete = c("T", "T")))
for (nom in names(ZERO)) {
  cas <- ZERO[[nom]]
  verifier(sprintf("%s, D = 0 partout (forme close) : Poisson [0 ; 3,688879454113936 / E^c], binomial [0 ; 1 - 0,025^(1/E^i)], E^i = E^c, asymptotiques NA, tolérance 1e-13", nom), {
    p <- cas$p; b <- cas$b
    if (any(p$evenements != 0L) || any(p$etat != "defini") || any(b$etat != "defini")) "états ou événements"
    else if (!identical(b$exposition_initiale, p$exposition_centrale)) "E^i différente de E^c"
    else if (any(p$taux != 0) || any(p$borne_inf_exacte != 0) || any(b$borne_inf_exacte != 0)) "bornes inférieures"
    else if (!all(is.na(c(unlist(p[BORNES[3:4]]), unlist(b[BORNES[3:4]]))))) "asymptotiques non NA"
    else {
      r <- proche(p$borne_sup_exacte, 3.688879454113936 / p$exposition_centrale, rel = TB, abs = 0)
      if (!isTRUE(r)) paste("Poisson :", r)
      else proche(b$borne_sup_exacte, 1 - A2^(1 / b$exposition_initiale), rel = TB, abs = 0)
    }
  })
}
verifier("H1, âge 59, E = 122/366 = 1/3 (valeur calculée à la main) : Poisson [0 ; 11,06663836234181] ; binomial [0 ; 1 - 0,025^3 = 0,999984375], tolérance 1e-13", {
  p <- ZERO$H1$p[ZERO$H1$p$age == 59L, ]; b <- ZERO$H1$b[ZERO$H1$b$age == 59L, ]
  proche(c(p$borne_sup_exacte, b$borne_sup_exacte), c(11.06663836234181, 0.999984375), rel = TB, abs = 0)
})

# --- 5. H10a : tables à zéro ligne (valeur calculée à la main) -----------------

H10a <- construire_cas("1950-05-05", "2005-01-01", "2014-12-31", "deces")
COL_P <- c("age", "evenements", "exposition_centrale", "taux", BORNES, "probabilite",
           "probabilite_borne_inf_exacte", "probabilite_borne_sup_exacte",
           "probabilite_borne_inf_asymptotique", "probabilite_borne_sup_asymptotique", "etat")
COL_B <- c("age", "evenements", "exposition_initiale", "taux", BORNES, "etat")
verifier("H10a (valeur calculée à la main) : tables Poisson et binomiale à zéro ligne, colonnes de la spécification (A.5), types conservés",
         nrow(H10a$p) == 0L && nrow(H10a$b) == 0L && identical(names(H10a$p), COL_P) &&
           identical(names(H10a$b), COL_B) && is.integer(H10a$p$evenements) && is.character(H10a$p$etat) &&
           is.double(H10a$b$taux) && identical(H10a$objet$etapes$brut$diagnostics$cellules, 0L))

# --- 6. H13 : par_annee (valeur calculée à la main) ----------------------------

H13 <- construire_cas("1960-07-01", "2020-03-01", NA, "en_cours", estimateurs = "poisson", par_annee = TRUE)
verifier("H13 par_annee, Poisson seul (valeur calculée à la main) : 60 lignes, 10 defini, 50 exposition_nulle ; clés (age, annee) ; aucune table binomiale",
         nrow(H13$p) == 60L && sum(H13$p$etat == "defini") == 10L && sum(H13$p$etat == "exposition_nulle") == 50L &&
           identical(names(H13$p)[1:2], c("age", "annee")) &&
           identical(names(H13$objet$etapes$brut$resultat), "poisson"))
verifier("H13 : binomial sous par_annee -> refus binomial_par_annee depuis valider_plan(), avant toute étape",
         leve_refus(valider_plan(plan_brut(par_annee = TRUE)), "binomial_par_annee"))
verifier("H13 : binomial sous par_annee -> refus binomial_par_annee depuis l'appel direct de la fonction d'étape",
         leve_refus(calculer_taux_bruts(H13$e, character(0), TRUE, "dernier_anniversaire", "u",
                                        c("poisson", "binomial"), 0.95), "binomial_par_annee"))
verifier("H13 : le refus nomme par_annee et q_x", {
  r <- tryCatch(valider_plan(plan_brut(par_annee = TRUE)), refus_tablexp = function(r) r)
  identical(r$module, "brut") && grepl("par_annee", conditionMessage(r), fixed = TRUE) &&
    grepl("q_x", conditionMessage(r), fixed = TRUE)
})

# --- 7. cas composite C (valeurs calculées à la main, A.6, point 7) -----------

DC <- data.frame(
  n = D(c("1960-07-01", "1945-10-15", "1950-01-10", "1940-12-05", "1955-06-30", "1964-02-29", "1955-04-12",
          "1970-03-03", "1950-05-05", "1950-05-05", "1980-01-01")),
  e = D(c("2020-03-01", "2016-05-20", "2010-06-01", "2022-01-01", "2023-01-01", "2021-01-01", "2019-09-01",
          "2017-05-05", "2005-01-01", "2005-01-01", "2018-06-15")),
  s = D(c(NA, "2019-02-10", NA, "2024-11-15", "2025-02-01", NA, "2020-04-12", "2021-08-08", "2014-12-31",
          "2015-01-01", "2018-06-15")),
  c = c("en_cours", "deces", "en_cours", "deces", "deces", "en_cours", "deces", "rachat", "deces", "deces",
        "deces"),
  t = 1:11, stringsAsFactors = FALSE)
ROLES_C <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", id_tete = "t")
XC <- construire_table(DC, ROLES_C, plan_brut())
PC <- XC$etapes$brut$resultat$poisson
BC <- XC$etapes$brut$resultat$binomial
EC <- XC$etapes$exposition$resultat$exposition
verifier("C (valeur calculée à la main) : 46 cellules (38 à 83), 11 437 jours, E^c = 31,308847967662249 (1e-14), D = 5",
         nrow(PC) == 46L && identical(PC$age, 38:83) && sum(EC$exposition_jours) == 11437 &&
           sum(PC$evenements) == 5L &&
           isTRUE(proche(sum(PC$exposition_centrale), 31.308847967662249, rel = TQ, abs = 0)))
verifier("C (valeur calculée à la main) : états Poisson 27 defini, 18 exposition_nulle, 1 evenement_sans_exposition ; binomial 27 defini, 18 exposition_nulle, 1 effectif_inferieur_aux_evenements",
         identical(XC$etapes$brut$diagnostics$etats,
                   data.frame(estimateur = rep(c("poisson", "binomial"), each = 3L),
                              etat = c("defini", "exposition_nulle", "evenement_sans_exposition",
                                       "defini", "exposition_nulle", "effectif_inferieur_aux_evenements"),
                              cellules = c(27L, 18L, 1L, 27L, 18L, 1L), stringsAsFactors = FALSE)))
verifier("C (valeur calculée à la main, additivité) : E^c et E^i à 64, 65 et 73 : 193/365 + 224/366 et 317/365 + 224/366 ; 1 et 2 ; 483/365 et 2, tolérance 1e-14", {
  i <- match(c(64L, 65L, 73L), PC$age)
  proche(c(PC$exposition_centrale[i], BC$exposition_initiale[i]),
         c(193 / 365 + 224 / 366, 1, 483 / 365, 317 / 365 + 224 / 366, 2, 2), rel = TQ, abs = 0)
})
ATT_C <- list(
  `64` = list(p = c(0.876586306906915, 0.0221932437999271, 4.88402630346545, 0.123479010065178, 6.22294876717190),
              b = c(0.675440636660566, 0.0169552896943785, 0.999795182938104, 0.0625357371835182, 0.984831267393196)),
  `65` = list(p = c(1, 0.0253178079842899, 5.57164339093890, 0.140863494093218, 7.09907138423133),
              b = c(0.5, 0.0125791170934251, 0.987420882906575, 0.0588667871415690, 0.941133212858431)),
  `73` = list(p = c(0.755693581780538, 0.0191325049984799, 4.21045515050248, 0.106449638393425, 5.36472268166550),
              b = c(0.5, 0.0125791170934251, 0.987420882906575, 0.0588667871415690, 0.941133212858431)),
  `83` = list(p = c(1.057803468208092, 0.0267812650932084, 5.89370370255386, 0.149005892595716, 7.50942233129673),
              b = c(1, A2, 1, NA, NA)))
for (a in names(ATT_C)) {
  att <- ATT_C[[a]]
  verifier(sprintf("C, âge %s (valeur calculée à la main, A.6) : Poisson et binomial, taux et quatre bornes, tolérance 1e-13 (attendus à 15 chiffres)", a), {
    lp <- PC[PC$age == as.integer(a), ]; lb <- BC[BC$age == as.integer(a), ]
    r1 <- proche(unlist(lp[c("taux", BORNES)]), att$p, rel = TB, abs = 0)
    r2 <- proche(unlist(lb[c("taux", BORNES)]), att$b, rel = TB, abs = 0)
    if (!isTRUE(r1)) paste("Poisson :", r1) else if (!isTRUE(r2)) paste("binomial :", r2)
    else identical(c(lp$etat, lb$etat), c("defini", "defini"))
  })
}
verifier("C : diagnostics (valeur calculée à la main) : 46 cellules, 5 événements, 41 cellules sans événement, 1 cellule où D = E^i > 0 (âge 83)",
         identical(XC$etapes$brut$diagnostics[c("cellules", "evenements", "cellules_sans_evenement",
                                                "cellules_effectif_egal_aux_evenements")],
                   list(cellules = 46L, evenements = 5L, cellules_sans_evenement = 41L,
                        cellules_effectif_egal_aux_evenements = 1L)))
verifier("C : cellules_effectif_egal_aux_evenements = cellules définies à D > 0 dont les bornes asymptotiques binomiales sont NA (même branche)",
         identical(XC$etapes$brut$diagnostics$cellules_effectif_egal_aux_evenements,
                   sum(BC$etat == "defini" & BC$evenements > 0L & is.na(BC$borne_inf_asymptotique))))
verifier("C, âges 64, 65, 73, 83 (recoupement : stats::poisson.test(D, E^c) ; le moteur repose sur la même fonction, qgamma), tolérance 1e-13", {
  i <- match(c(64L, 65L, 73L, 83L), PC$age)
  pt <- vapply(i, function(j) stats::poisson.test(PC$evenements[j], PC$exposition_centrale[j])$conf.int[1:2],
               numeric(2L))
  proche(c(PC$borne_inf_exacte[i], PC$borne_sup_exacte[i]), c(pt[1L, ], pt[2L, ]), rel = TB, abs = 0)
})
verifier("C, âges 65 et 73, E^i = 2 entier (recoupement : stats::binom.test(1, 2) ; le moteur repose sur la même fonction, qbeta), tolérance 1e-13", {
  i <- match(c(65L, 73L), BC$age)
  bt <- stats::binom.test(1, 2)$conf.int[1:2]
  proche(c(BC$borne_inf_exacte[i], BC$borne_sup_exacte[i]), rep(bt, each = 2L), rel = TB, abs = 0)
})

# --- 8. refus (valeur calculée à la main : codes) ------------------------------

cas_refus <- list(
  list("estimateurs absent", "parametre_manquant",
       { p <- plan_brut(); p$etapes$brut$estimateurs <- NULL; p }),
  list("niveau absent", "parametre_manquant", { p <- plan_brut(); p$etapes$brut$niveau <- NULL; p }),
  list("paramètre inconnu (exposition_initiale, fixée sans paramètre)", "parametre_inconnu",
       { p <- plan_brut(); p$etapes$brut$exposition_initiale <- "cellule"; p }),
  list("estimateurs character(0)", "estimateurs_invalides", plan_brut(estimateurs = character(0))),
  list("estimateurs en double", "estimateurs_invalides", plan_brut(estimateurs = c("poisson", "poisson"))),
  list("estimateurs NA", "estimateurs_invalides", plan_brut(estimateurs = NA)),
  list("estimateurs NA_character_", "estimateurs_invalides", plan_brut(estimateurs = c("poisson", NA))),
  list("estimateur inconnu", "estimateur_inconnu", plan_brut(estimateurs = "estimateur_fictif")),
  list("brut sans exposition", "etape_prealable_manquante",
       { p <- plan_brut(); p$etapes$exposition <- NULL; p }),
  list("brut avant exposition", "etapes_hors_ordre",
       { p <- plan_brut(); p$etapes <- p$etapes[c("donnees", "brut", "exposition")]; p }))
for (niv in list(0, 1, -0.1, 1.5, NA, "0.95", c(0.9, 0.95), Inf, NA_real_, NaN, TRUE,
                 matrix(0.95), c(a = 0.95), structure(0.95, foo = "x")))
  cas_refus[[length(cas_refus) + 1L]] <-
    list(sprintf("niveau %s", paste(deparse(niv), collapse = "")), "niveau_invalide", plan_brut(niveau = niv))
for (k in cas_refus)
  verifier(sprintf("valider_plan : %s -> refus %s", k[[1]], k[[2]]), leve_refus(valider_plan(k[[3]]), k[[2]]))
verifier("refus de valeur portés par le module brut (codes identiques depuis valider_plan() et la fonction d'étape)", {
  r1 <- tryCatch(valider_plan(plan_brut(niveau = 2)), refus_tablexp = function(r) r)
  r2 <- tryCatch(calculer_taux_bruts(EC, character(0), FALSE, "dernier_anniversaire", "u", "poisson", 2),
                 refus_tablexp = function(r) r)
  r3 <- tryCatch(controler_parametres_brut("estimateur_fictif", 0.95, FALSE), refus_tablexp = function(r) r)
  identical(c(r1$module, r2$module, r3$module), rep("brut", 3L)) &&
    identical(c(r1$motif, r2$motif, r3$motif), c("niveau_invalide", "niveau_invalide", "estimateur_inconnu")) &&
    identical(r3$details$estimateur, "estimateur_fictif")
})
verifier("niveau 1 - 2^-53 admis : bornes exactes et asymptotiques finies dans les cellules définies (cas C)", {
  x <- construire_table(DC, ROLES_C, plan_brut(niveau = 1 - 2^-53))
  p <- x$etapes$brut$resultat$poisson; b <- x$etapes$brut$resultat$binomial
  vp <- unlist(p[p$etat == "defini", BORNES[1:2]]); vb <- unlist(b[b$etat == "defini", BORNES[1:2]])
  ap <- unlist(p[p$etat == "defini" & p$evenements > 0L, BORNES[3:4]])
  all(is.finite(c(vp, vb, ap)))
})

# --- 9. identités et forme -----------------------------------------------------

verifier("C (identité exacte) : clés, evenements et exposition_centrale identical() aux colonnes de la table d'exposition, ordre compris",
         identical(PC[c("age", "evenements", "exposition_centrale")], EC[c("age", "evenements", "exposition_centrale")]) &&
           identical(BC[c("age", "evenements")], EC[c("age", "evenements")]))
verifier("C (identité) : mu E^c = D dans les cellules définies, tolérance 1e-14 relative", {
  k <- PC$etat == "defini" & PC$evenements > 0L
  proche(PC$taux[k] * PC$exposition_centrale[k], PC$evenements[k], rel = TQ, abs = 0)
})
verifier("C (identité) : exposition_initiale = E^c + complement_initial_cellule, exactement", {
  identical(BC$exposition_initiale, EC$exposition_centrale + EC$complement_initial_cellule)
})
verifier("segmentation sexe, décès d'un seul sexe : D = 0 et taux nul dans tout le segment F, clés identical() à l'exposition", {
  d <- data.frame(n = D(c("1960-07-01", "1945-10-15")), e = D(c("2020-03-01", "2016-05-20")),
                  s = D(c(NA, "2019-02-10")), c = c("en_cours", "deces"), sx = c("F", "H"), stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", sexe = "sx")
  x <- construire_table(d, roles, plan_brut(segmentation = "sexe"))
  p <- x$etapes$brut$resultat$poisson; e <- x$etapes$exposition$resultat$exposition
  f <- p$sexe == "F"
  any(f) && all(p$evenements[f] == 0L) && all(p$taux[f] == 0) && sum(p$evenements[!f]) == 1L &&
    identical(p[c("sexe", "age")], e[c("sexe", "age")]) && identical(names(p)[1:2], c("sexe", "age"))
})
verifier("estimateurs déclarés c(\"binomial\", \"poisson\") : résultats dans l'ordre canonique (poisson, binomial), etapes$brut identical() à celle du plan déclaré c(\"poisson\", \"binomial\") ; paramètres effectifs (A.5)", {
  xr <- construire_table(DC, ROLES_C, plan_brut(estimateurs = c("binomial", "poisson")))
  pa <- xr$etapes$brut$parametres
  identical(xr$etapes$brut, XC$etapes$brut) &&
    identical(names(xr$etapes$brut$resultat), c("poisson", "binomial")) &&
    identical(pa$estimateurs, c("poisson", "binomial")) && identical(pa$niveau, 0.95) &&
    identical(pa$exposition_initiale, "cellule") && identical(pa$tolerance_effectif, 1e-9) &&
    identical(pa$convention, "dernier_anniversaire") && startsWith(pa$unite_exposition, "annee_d_age") &&
    identical(names(pa$hypotheses), c("poisson", "binomial", "probabilite")) &&
    identical(names(pa$statuts), c("estimateur", "grandeur", "intervalle", "statut", "loi_supposee", "calcul"))
})
verifier("statuts : binomial exact « couverture non établie » si E^i n'est pas entier ; Poisson exact sous E^c fixe ; asymptotiques non définis à D = 0", {
  s <- XC$etapes$brut$parametres$statuts
  be <- s$statut[s$estimateur == "binomial" & s$intervalle == "exact"]
  pa <- s$statut[s$estimateur == "poisson" & s$grandeur == "taux" & s$intervalle == "asymptotique"]
  length(be) == 1L && grepl("couverture non établie", be, fixed = TRUE) &&
    any(grepl("E^c fixe", s$loi_supposee, fixed = TRUE)) && grepl("D = 0", pa, fixed = TRUE)
})
verifier("statuts (valeur calculée à la main) : 9 lignes, une par (estimateur, grandeur, intervalle) ; « fixe » dans la loi supposée des seuls intervalles exacts ; statuts commençant par un terme canonique", {
  s <- XC$etapes$brut$parametres$statuts
  cle <- paste(s$estimateur, s$grandeur, s$intervalle)
  ponct <- "sans objet (estimation ponctuelle)"
  att <- c(paste("poisson taux", c(ponct, "exact", "asymptotique")),
           paste("poisson probabilite", c(ponct, "exact", "asymptotique")),
           paste("binomial taux", c(ponct, "exact", "asymptotique")))
  if (!identical(cle, att)) sprintf("lignes %s", paste(cle, collapse = " | "))
  else if (!identical(grepl("fixe", s$loi_supposee, fixed = TRUE), s$intervalle == "exact")) "« fixe » hors des intervalles exacts"
  else if (!all(startsWith(s$statut, ifelse(s$intervalle == "asymptotique", "asymptotique",
                                            ifelse(s$intervalle == "exact", "exact sous la loi supposée",
                                                   ifelse(s$estimateur == "binomial", "estimateur des moments",
                                                          "résultat exact"))))))
    "terme canonique en tête du statut"
  else grepl("non sans biais à effectif fini", s$statut[7L], fixed = TRUE) &&
    grepl("E^i q(1-q) -> infini", s$statut[9L], fixed = TRUE) && identical(s$calcul[4L], "-expm1(-taux)")
})
verifier("Poisson seul : une seule table, aucun statut ni hypothèse binomiale ; diagnostic D = E^i NA", {
  x <- construire_table(DC, ROLES_C, plan_brut(estimateurs = "poisson"))
  b <- x$etapes$brut
  identical(names(b$resultat), "poisson") && identical(names(b$parametres$hypotheses), c("poisson", "probabilite")) &&
    all(b$parametres$statuts$estimateur == "poisson") &&
    identical(b$diagnostics$cellules_effectif_egal_aux_evenements, NA_integer_) &&
    identical(b$resultat$poisson, PC)
})
verifier("binomial seul : aucune hypothèse de probabilité, table identical() à celle du plan à deux estimateurs", {
  x <- construire_table(DC, ROLES_C, plan_brut(estimateurs = "binomial"))
  identical(names(x$etapes$brut$parametres$hypotheses), "binomial") &&
    identical(x$etapes$brut$resultat$binomial, BC)
})
verifier("hypothèse de probabilité selon la cellule : année d'âge (dernier_anniversaire), année civile (age_atteint_annee), âge x année (par_annee)",
         grepl("année d'âge", XC$etapes$brut$parametres$hypotheses$probabilite, fixed = TRUE) &&
           grepl("année civile", UN$H12[[1L]]$objet$etapes$brut$parametres$hypotheses$probabilite, fixed = TRUE) &&
           grepl("mu_(x,t)", H13$objet$etapes$brut$parametres$hypotheses$probabilite, fixed = TRUE))
verifier("objet : etapes$brut (parametres, resultat, diagnostics), table NULL", {
  identical(names(XC$etapes), c("donnees", "exposition", "brut")) &&
    identical(names(XC$etapes$brut), c("parametres", "resultat", "diagnostics")) && is.null(XC$table)
})
CHAMPS_FICHE_SANS_BRUT <- c("version_fiche", "versions", "plateforme", "roles", "plan", "transition", "transition_code",
                     "periode", "delai_declaration", "fin_effective", "segmentation", "unite_observation",
                     "avertissement_unite", "recette", "convention", "unite_exposition", "par_annee", "exclusions",
                     "hypothese_censure", "graine", "graine_mention", "generateur", "provenance",
                     "provenance_mention", "empreintes", "avertissements")
verifier("fiche : hypotheses_brut en fin de liste si l'étape brut est exécutée, copie des paramètres de l'étape", {
  f <- XC$fiche
  identical(names(f), c(CHAMPS_FICHE_SANS_BRUT, "hypotheses_brut")) &&
    identical(f$hypotheses_brut, XC$etapes$brut$parametres[c("estimateurs", "niveau", "exposition_initiale",
                                                             "tolerance_effectif", "hypotheses", "statuts")])
})
# Invariance du plan sans `brut` : l'objet est celui du plan avec `brut`
# privé des seuls ajouts de l'étape. La comparaison à l'état de f9ab269
# relève des références de non-régression (#17).
invariance_sans_brut <- function(x, donnees, roles) {
  p <- x$fiche$plan; p$etapes$brut <- NULL
  y <- sans_execution(construire_table(donnees, roles, p))
  z <- sans_execution(x)
  z$etapes$brut <- NULL; z$fiche$hypotheses_brut <- NULL; z$fiche$plan <- p
  garde <- z$avertissements$etape != "brut"
  z$avertissements <- z$avertissements[garde, , drop = FALSE]; rownames(z$avertissements) <- NULL
  z$fiche$avertissements <- z$avertissements
  if (!identical(names(y$fiche), CHAMPS_FICHE_SANS_BRUT)) "champs de la fiche du plan sans brut"
  else identical(y, z)
}
verifier("invariance du plan sans brut (cas C, poisson et binomial) : fiche sans hypotheses_brut ; objet identical() à celui du plan avec brut hors etapes$brut, fiche$plan, fiche$hypotheses_brut et avertissements de l'étape brut",
         invariance_sans_brut(XC, DC, ROLES_C))
verifier("invariance du plan sans brut (cas C, par_annee = TRUE, poisson seul) : même contrôle",
         invariance_sans_brut(construire_table(DC, ROLES_C, plan_brut(estimateurs = "poisson", par_annee = TRUE)),
                              DC, ROLES_C))
