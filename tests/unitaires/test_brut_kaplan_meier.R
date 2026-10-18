# -----------------------------------------------------------------------------
#  tests/unitaires/test_brut_kaplan_meier.R : estimateur de Kaplan-Meier de
#  l'étape `brut` (issue #15 ; E2.12, E2.20 ; ADR 0007, points 5, 6 et 11.4 ;
#  spécification d'actuary du 17/10/2026, A.1 à A.12 ; décisions du
#  mainteneur du 17/10/2026 sur ses questions A.14)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : états, taux, variances et bornes des cas H,
#    F, K et du cas composite C, écrits en dur d'après la spécification
#    (A.11, points 1 à 3 et 7), en formes closes (1/6, 29/45, 3/10...) ;
#  - identité exacte : Kaplan-Meier = binomial sur des cohortes d'âge exact
#    (A.10), somme de d par cellule = événements de l'exposition, clés et
#    ordre de la table d'exposition, 1 <= d <= r, survie au dernier saut =
#    1 - taux ; correspondance dates <-> âges rationnels du jeu fixé (A.12) ;
#  - recoupement : bornes logit binomiales recalculées par stats::qlogis et
#    stats::plogis (le moteur repose sur les mêmes fonctions : le test
#    vérifie la formule Sigma / q^2, pas la fonction) ;
#  - contre-vérification : valeurs de survival::survfit() produites hors CI
#    par tests/contre_verification/survfit_kaplan_meier.R (#7), versionnées
#    dans tests/contre_verification/valeurs_survfit_kaplan_meier.csv.
#  Tolérances déclarées dans chaque test (A.11) : 1e-14 relative sur q et la
#  variance, 1e-13 relative sur les bornes, 1e-13 relative pour l'identité
#  avec le binomial ; égalité exacte pour r, d, numerateur, les états et les
#  codes. z = 1,959963984540054 écrit en dur.
# -----------------------------------------------------------------------------

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
TQ <- 1e-14   # q et variance
TB <- 1e-13   # bornes
Z <- 1.959963984540054
M <- 133590L
calculer_taux_bruts <- tablexp:::calculer_taux_bruts
controler_parametres_brut <- tablexp:::controler_parametres_brut

plan_km <- function(estimateurs = "kaplan_meier", niveau = 0.95, convention = "plus_fine", par_annee = FALSE,
                    delai = 0, transition = VD, segmentation = character(0))
  list(transition = transition, periode = PER, delai_declaration = delai, segmentation = segmentation,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = convention, par_annee = par_annee),
                     brut = list(estimateurs = estimateurs, niveau = niveau)))

# Cas d'une tête (lignes : naissance ; entrée -> sortie, cause), identifiant
# de tête facultatif ; rend les tables de Kaplan-Meier, la table
# d'exposition et l'objet.
construire_cas <- function(naissance, entree, sortie, cause, tete = NULL, ...) {
  d <- data.frame(n = D(naissance), e = D(entree), s = D(sortie), c = cause, stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
  if (!is.null(tete)) { d$t <- tete; roles <- c(roles, id_tete = "t") }
  x <- construire_table(d, roles, plan_km(...))
  list(km = x$etapes$brut$resultat$kaplan_meier, sauts = x$etapes$brut$resultat$kaplan_meier_sauts,
       e = x$etapes$exposition$resultat$exposition, objet = x)
}

# États attendus d'une table : liste de (âges, code) ; codes « E »
# ensemble_a_risque_vide (taux NA), « N » exposition_nulle (taux NA), « 0 »
# défini à q = 0, « 1 » défini à q = 1 ; variance et bornes NA dans tous ces
# cas (q dans {0, 1} ou non défini). Rend TRUE ou l'écart.
controler_etats <- function(km, attendu, toutes = TRUE) {
  ages <- unlist(lapply(attendu, `[[`, 1L))
  codes <- unlist(lapply(attendu, function(a) rep(a[[2L]], length(a[[1L]]))))
  if (toutes && !identical(km$age, as.integer(ages)))
    return(sprintf("âges %s au lieu de %s", paste(km$age, collapse = " "), paste(ages, collapse = " ")))
  i <- match(ages, km$age)
  if (anyNA(i)) return("âge attendu absent de la table")
  etat <- c(E = "ensemble_a_risque_vide", N = "exposition_nulle", `0` = "defini", `1` = "defini")[codes]
  taux <- c(E = NA, N = NA, `0` = 0, `1` = 1)[codes]
  if (!identical(km$etat[i], unname(etat)))
    return(sprintf("états %s", paste(km$etat[i], collapse = " ")))
  if (!identical(km$taux[i], unname(as.numeric(taux))))
    return(sprintf("taux %s", paste(km$taux[i], collapse = " ")))
  if (!all(is.na(c(km$variance[i], km$borne_inf_asymptotique[i], km$borne_sup_asymptotique[i]))))
    return("variance ou bornes non NA")
  TRUE
}

# --- 1. cas H et F (valeurs calculées à la main, A.11, point 1) --------------

E <- "E"; N0 <- "0"; N1 <- "1"
CAS_HF <- list(
  list("H1", list("1960-07-01", "2020-03-01", NA, "en_cours"), list(list(59, E), list(60:63, N0), list(64, E))),
  list("H2", list("1945-10-15", "2016-05-20", "2019-02-10", "deces"), list(list(70, E), list(71:72, N0), list(73, N1))),
  list("H3", list("1950-01-10", "2010-06-01", NA, "en_cours"), list(list(64, E), list(65:73, N0), list(74, E))),
  list("H4a", list("1940-12-05", "2022-01-01", "2024-11-15", "deces"), list(list(81, E), list(82, N0), list(83, N1))),
  list("H4b (délai 90)", list("1940-12-05", "2022-01-01", "2024-11-15", "deces", delai = 90),
       list(list(81, E), list(82, N0), list(83, E))),
  list("H5", list("1955-06-30", "2023-01-01", "2025-02-01", "deces"), list(list(67, E), list(68, N0), list(69, E))),
  list("H6", list("1964-02-29", "2021-01-01", NA, "en_cours"), list(list(56, E), list(57:59, N0), list(60, E))),
  list("H7 (fusion)", list("1948-08-20", c("2016-01-01", "2018-07-01"), c("2019-01-01", "2021-05-10"),
                           c("terme", "deces"), tete = c("T", "T")),
       list(list(67, E), list(68:71, N0), list(72, N1))),
  list("H8", list("1955-04-12", "2019-09-01", "2020-04-12", "deces"), list(list(64, E), list(65, N1))),
  list("H9 (décès)", list("1970-03-03", "2017-05-05", "2021-08-08", "rachat"),
       list(list(47, E), list(48:50, N0), list(51, E))),
  list("H12 (age_atteint_annee)", list("1945-10-15", "2016-05-20", "2019-02-10", "deces",
                                        convention = "age_atteint_annee"),
       list(list(71, E), list(72:73, N0), list(74, N1))),
  list("F1", list("1950-03-15", c("2016-01-01", "2018-07-01"), c("2019-12-31", NA), c("rachat", "en_cours"),
                  tete = c("T", "T")), list(list(65, E), list(66:73, N0), list(74, E))),
  list("F2", list("1952-09-30", c("2016-01-01", "2018-01-01"), c("2018-01-01", "2021-05-10"), c("terme", "deces"),
                  tete = c("T", "T")), list(list(63, E), list(64:67, N0), list(68, N1))),
  list("F3 (jour d'écart)", list("1949-11-11", c("2016-01-01", "2017-01-02"), c("2017-01-01", NA),
                                 c("terme", "en_cours"), tete = c("T", "T")),
       list(list(66, E), list(67, E), list(68:74, N0), list(75, E))),
  list("F4", list("1945-06-06", c("2016-01-01", "2019-01-01"), c("2022-03-01", "2022-03-01"), c("deces", "deces"),
                  tete = c("T", "T")), list(list(70, E), list(71:75, N0), list(76, N1))))
RES_HF <- list()
for (k in CAS_HF) {
  cas <- do.call(construire_cas, k[[2]])
  RES_HF[[k[[1]]]] <- cas
  verifier(sprintf("%s (valeur calculée à la main, A.11) : états et taux par cellule, variance et bornes NA (q dans {0, 1}) ; evenements identical() à l'exposition",
                   k[[1]]), {
    r <- controler_etats(cas$km, k[[3]])
    if (!isTRUE(r)) r else identical(cas$km$evenements, cas$e$evenements) &&
      identical(cas$km$age, cas$e$age)
  })
}
verifier("H8 (valeur calculée à la main) : un saut à tau = 65 (numerateur 0), r = d = 1 ; q = 1 comme le binomial (Poisson non défini, test_brut.R) ; cellules_survie_nulle 1 (cellule définie à q = 1)", {
  s <- RES_HF$H8$sauts
  if (!identical(RES_HF$H8$objet$etapes$brut$diagnostics$kaplan_meier$cellules_survie_nulle, 1L)) return("diagnostic")
  nrow(s) == 1L && identical(s$age, 65L) && identical(s$numerateur, 0L) && identical(s$age_exact, 65) &&
    identical(s$ensemble_a_risque, 1L) && identical(s$evenements, 1L) && identical(s$survie_cellule, 0) &&
    identical(s$variance_log_survie_cellule, Inf)
})
verifier("F4 (valeur calculée à la main) : cellule 76, r = d = 1 (unité tête)", {
  s <- RES_HF$F4$sauts
  identical(s$age, 76L) && identical(s$ensemble_a_risque, 1L) && identical(s$evenements, 1L)
})
verifier("H9 rachat (unité contrat, valeur calculée à la main, A.11 point 7) : cellule 51 définie, q = 1", {
  cas <- construire_cas("1970-03-03", "2017-05-05", "2021-08-08", "rachat", transition = RA)
  controler_etats(cas$km, list(list(51, N1)), toutes = FALSE)
})
verifier("F4 sans id_tete (unité contrat, valeur calculée à la main, A.11 point 7) : cellule 76, r = d = 2, evenements = 2", {
  cas <- construire_cas("1945-06-06", c("2016-01-01", "2019-01-01"), c("2022-03-01", "2022-03-01"), c("deces", "deces"))
  s <- cas$sauts; l <- cas$km[cas$km$age == 76L, ]
  identical(s$age, 76L) && identical(s$ensemble_a_risque, 2L) && identical(s$evenements, 2L) &&
    identical(l$evenements, 2L) && identical(l$instants_evenement, 1L)
})
for (k in list(list("H10b", "1950-05-05", "2005-01-01", "2015-01-01", 64L, 241L, 365L),
               list("H11", "1980-01-01", "2018-06-15", "2018-06-15", 38L, 165L, 365L))) {
  cas <- construire_cas(k[[2]], k[[3]], k[[4]], "deces")
  verifier(sprintf("%s (valeur calculée à la main) : cellule %d ensemble_a_risque_vide, saut r = d = 1 à %d + %d/%d (numerateur %d), compté dans l'ensemble à risque fermé ; survie et Sigma du saut NA (cellule non définie) ; cellules_survie_nulle 0 ; avertissement (1 cellule, 1 événement)",
                   k[[1]], k[[5]], k[[5]], k[[6]], k[[7]], k[[6]] * (M %/% k[[7]])), {
    s <- cas$sauts
    r <- controler_etats(cas$km, list(list(k[[5]], E)))
    a <- cas$objet$avertissements
    if (!isTRUE(r)) r
    else if (!identical(a$code[a$etape == "brut"], "kaplan_meier_ensemble_a_risque_vide") ||
             !identical(a$nombre[a$etape == "brut"], 1L)) "avertissement"
    else identical(s$age, k[[5]]) && identical(s$numerateur, k[[6]] * (M %/% k[[7]])) &&
      identical(s$survie_cellule, NA_real_) && identical(s$variance_log_survie_cellule, NA_real_) &&
      identical(cas$objet$etapes$brut$diagnostics$kaplan_meier$cellules_survie_nulle, 0L) &&
      identical(s$ensemble_a_risque, 1L) && identical(s$evenements, 1L) && identical(cas$km$evenements, 1L) &&
      isTRUE(proche(s$age_exact, k[[5]] + k[[6]] / k[[7]], rel = 1e-15, abs = 0)) &&
      identical(cas$objet$etapes$brut$diagnostics$kaplan_meier$evenements_a_l_instant_d_entree, 1L)
  })
}
verifier("H10a (valeur calculée à la main) : tables kaplan_meier et kaplan_meier_sauts à zéro ligne, colonnes de la spécification (A.9), types conservés ; diagnostics nuls", {
  cas <- construire_cas("1950-05-05", "2005-01-01", "2014-12-31", "deces")
  dk <- cas$objet$etapes$brut$diagnostics$kaplan_meier
  nrow(cas$km) == 0L && nrow(cas$sauts) == 0L &&
    identical(names(cas$km), c("age", "evenements", "instants_evenement", "taux", "variance",
                               "borne_inf_asymptotique", "borne_sup_asymptotique", "etat")) &&
    identical(names(cas$sauts), c("age", "numerateur", "age_exact", "ensemble_a_risque", "evenements",
                                  "survie_cellule", "variance_log_survie_cellule")) &&
    identical(vapply(cas$km, typeof, ""), c(age = "integer", evenements = "integer", instants_evenement = "integer",
                                             taux = "double", variance = "double", borne_inf_asymptotique = "double",
                                             borne_sup_asymptotique = "double", etat = "character")) &&
    identical(vapply(cas$sauts, typeof, ""), c(age = "integer", numerateur = "integer", age_exact = "double",
                                                ensemble_a_risque = "integer", evenements = "integer",
                                                survie_cellule = "double", variance_log_survie_cellule = "double")) &&
    identical(dk, list(instants = 0L, evenements = 0L, evenements_a_l_instant_d_entree = 0L,
                       cellules_survie_nulle = 0L, ensemble_a_risque_maximal = 0L))
})

# --- 2. cas K : ex aequo (valeurs calculées à la main, A.11, point 2) ---------

DK <- data.frame(
  t = paste0("K", 1:9),
  n = D(c("1950-01-01", "1950-01-01", "1950-01-01", "1950-01-01", "1950-07-01", "1950-07-01", "1950-07-01",
          "1950-01-01", "1950-01-01")),
  e = D(c("2019-06-01", "2020-01-01", "2019-01-01", "2018-01-01", "2020-03-01", "2010-01-01", "2016-01-01",
          "2020-06-01", "2020-03-01")),
  s = D(c(NA, "2020-03-01", "2020-01-01", "2020-03-01", "2020-03-01", "2020-08-15", "2021-01-01", "2020-09-01", NA)),
  c = c("en_cours", "deces", "deces", "rachat", "deces", "deces", "terme", "deces", "en_cours"),
  stringsAsFactors = FALSE)
ROLES <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", id_tete = "t")
XK <- construire_table(DK, ROLES, plan_km(estimateurs = c("poisson", "binomial", "kaplan_meier")))
KK <- XK$etapes$brut$resultat$kaplan_meier
SK <- XK$etapes$brut$resultat$kaplan_meier_sauts
verifier("K (valeur calculée à la main) : cinq sauts ; age_exact 69 + 244/366, 70, 70 + 45/365, 70 + 60/366, 70 + 244/366 (1e-15 relative) ; numerateur, r et d exacts", {
  r <- proche(SK$age_exact, c(69 + 244 / 366, 70, 70 + 45 / 365, 70 + 60 / 366, 70 + 244 / 366), rel = 1e-15, abs = 0)
  if (!isTRUE(r)) r
  else identical(SK$age, c(69L, 70L, 70L, 70L, 70L)) &&
    identical(SK$numerateur, c(244L * 365L, 0L, 45L * 366L, 60L * 365L, 244L * 365L)) &&
    identical(SK$ensemble_a_risque, c(6L, 6L, 5L, 5L, 3L)) && identical(SK$evenements, rep(1L, 5L))
})
verifier("K (valeur calculée à la main) : états 64 ensemble_a_risque_vide, 65 à 68 et 71 à 74 définis à q = 0",
         controler_etats(KK[KK$age != 69L & KK$age != 70L, ], list(list(64, E), list(65:68, N0), list(71:74, N0))))
verifier("K, âge 69 (valeur calculée à la main) : q = 1/6, variance (5/6)^2 / 30 = 0,02314814814814815 (1e-14) ; bornes [0,02283254988925365 ; 0,6312527902719756] (1e-13)", {
  l <- KK[KK$age == 69L, ]
  r1 <- proche(c(l$taux, l$variance), c(1 / 6, 0.02314814814814815), rel = TQ, abs = 0)
  r2 <- proche(c(l$borne_inf_asymptotique, l$borne_sup_asymptotique), c(0.02283254988925365, 0.6312527902719756),
               rel = TB, abs = 0)
  if (!isTRUE(r1)) r1 else if (!isTRUE(r2)) r2 else identical(l$etat, "defini") && identical(l$instants_evenement, 1L)
})
verifier("K, âge 69 (recoupement : bornes logit binomiales n = 6, D = 1, plogis(qlogis(1/6) -+ z sqrt(1 + 1/5)) ; le moteur repose sur plogis et qlogis), 1e-13", {
  l <- KK[KK$age == 69L, ]
  proche(c(l$borne_inf_asymptotique, l$borne_sup_asymptotique),
         stats::plogis(stats::qlogis(1 / 6) + c(-1, 1) * Z * sqrt(1 + 1 / 5)), rel = TB, abs = 0)
})
verifier("K, âge 70 (valeur calculée à la main) : q = 29/45, Sigma = 3/10, variance (16/45)^2 x 0,3 = 0,03792592592592593 (1e-14) ; bornes [0,2551950273598813 ; 0,9055531347639162] (1e-13)", {
  l <- KK[KK$age == 70L, ]
  s <- SK[SK$age == 70L, ]
  r1 <- proche(c(l$taux, l$variance, s$variance_log_survie_cellule[4L], s$survie_cellule[4L]),
               c(29 / 45, 0.03792592592592593, 3 / 10, 16 / 45), rel = TQ, abs = 0)
  r2 <- proche(c(l$borne_inf_asymptotique, l$borne_sup_asymptotique), c(0.2551950273598813, 0.9055531347639162),
               rel = TB, abs = 0)
  if (!isTRUE(r1)) r1 else if (!isTRUE(r2)) r2
  else identical(l$etat, "defini") && identical(l$evenements, 4L) && identical(l$instants_evenement, 4L)
})
verifier("K, âge 70 (valeur calculée à la main) : survie dans la cellule 5/6, 2/3, 8/15, 16/45 ; Sigma cumulée 1/30, 1/12, 2/15, 3/10 (1e-14)",
         proche(c(SK$survie_cellule[2:5], SK$variance_log_survie_cellule[2:5]),
                c(5 / 6, 2 / 3, 8 / 15, 16 / 45, 1 / 30, 1 / 12, 2 / 15, 3 / 10), rel = TQ, abs = 0))
verifier("K (valeur calculée à la main) : le cas départage les conventions d'ex aequo : q_70 = 29/45 sous l'ensemble fermé, différent de 0,68 (variante prévisible) et du binomial 4 / E^i",
         abs(KK$taux[KK$age == 70L] - 0.68) > 0.03 &&
           abs(KK$taux[KK$age == 70L] - XK$etapes$brut$resultat$binomial$taux[KK$age == 70L]) > 0.01)
verifier("K : diagnostics (valeur calculée à la main) : 5 instants, 5 événements, 1 à l'instant d'entrée (K5), 0 cellule de survie nulle, ensemble à risque maximal 6 ; tous integer",
         identical(XK$etapes$brut$diagnostics$kaplan_meier,
                   list(instants = 5L, evenements = 5L, evenements_a_l_instant_d_entree = 1L,
                        cellules_survie_nulle = 0L, ensemble_a_risque_maximal = 6L)))

# --- 2 bis. segment Z : censure à l'instant x de la cellule (constat C1) ----

DZ <- data.frame(t = paste0("Z", 1:3), n = D(rep("1950-01-01", 3L)), e = D(rep("2016-01-01", 3L)),
                 s = D(c("2020-01-01", "2020-01-01", NA)), c = c("deces", "rachat", "en_cours"),
                 stringsAsFactors = FALSE)
XZ <- construire_table(DZ, c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c",
                             id_tete = "t"), plan_km())
KZ <- XZ$etapes$brut$resultat$kaplan_meier
SZ <- XZ$etapes$brut$resultat$kaplan_meier_sauts
verifier("Z (valeur calculée à la main) : décès, rachat et unité en cours à l'instant 70 : un saut à 70 + 0/366, r = 3 (la censure de Z2 suit l'événement), d = 1 ; q_70 = 1/3 (1e-14) ; 66 à 69 et 71 à 74 définis à q = 0", {
  l <- KZ[KZ$age == 70L, ]
  r <- proche(l$taux, 1 / 3, rel = TQ, abs = 0)
  if (!isTRUE(r)) r
  else identical(SZ$age, 70L) && identical(SZ$numerateur, 0L) && identical(SZ$age_exact, 70) &&
    identical(SZ$ensemble_a_risque, 3L) && identical(SZ$evenements, 1L) && identical(l$etat, "defini") &&
    isTRUE(controler_etats(KZ[KZ$age != 70L & KZ$age != 75L, ], list(list(66:69, N0), list(71:74, N0))))
})

# --- 3. cas composite C de #14 (valeurs calculées à la main, A.11, point 3) --

DC <- data.frame(   # repris de tests/unitaires/test_brut.R (cas composite C)
  n = D(c("1960-07-01", "1945-10-15", "1950-01-10", "1940-12-05", "1955-06-30", "1964-02-29", "1955-04-12",
          "1970-03-03", "1950-05-05", "1950-05-05", "1980-01-01")),
  e = D(c("2020-03-01", "2016-05-20", "2010-06-01", "2022-01-01", "2023-01-01", "2021-01-01", "2019-09-01",
          "2017-05-05", "2005-01-01", "2005-01-01", "2018-06-15")),
  s = D(c(NA, "2019-02-10", NA, "2024-11-15", "2025-02-01", NA, "2020-04-12", "2021-08-08", "2014-12-31",
          "2015-01-01", "2018-06-15")),
  c = c("en_cours", "deces", "en_cours", "deces", "deces", "en_cours", "deces", "rachat", "deces", "deces",
        "deces"),
  t = 1:11, stringsAsFactors = FALSE)
XC <- construire_table(DC, ROLES, plan_km(estimateurs = c("poisson", "binomial", "kaplan_meier")))
KC <- XC$etapes$brut$resultat$kaplan_meier
SC <- XC$etapes$brut$resultat$kaplan_meier_sauts
EC <- XC$etapes$exposition$resultat$exposition
BC <- XC$etapes$brut$resultat$binomial
verifier("C (valeur calculée à la main) : 22 cellules définies, 6 ensemble_a_risque_vide (38, 47, 51, 56, 74, 81), 18 exposition_nulle ; lignes kaplan_meier de diagnostics$etats", {
  et <- XC$etapes$brut$diagnostics$etats
  identical(KC$age[KC$etat == "ensemble_a_risque_vide"], c(38L, 47L, 51L, 56L, 74L, 81L)) &&
    identical(et[et$estimateur == "kaplan_meier", ],
              data.frame(estimateur = "kaplan_meier", etat = c("defini", "exposition_nulle", "ensemble_a_risque_vide"),
                         cellules = c(22L, 18L, 6L), row.names = 7:9, stringsAsFactors = FALSE))
})
verifier("C, âges 64, 65 et 73 (valeur calculée à la main) : r = 2, d = 1, q = 0,5, variance 0,125 (1e-14), bornes [0,0588667871415690 ; 0,941133212858431] (1e-13)", {
  l <- KC[match(c(64L, 65L, 73L), KC$age), ]
  s <- SC[SC$age %in% c(64L, 65L, 73L), ]
  r1 <- proche(c(l$taux, l$variance), c(rep(0.5, 3L), rep(0.125, 3L)), rel = TQ, abs = 0)
  r2 <- proche(c(l$borne_inf_asymptotique, l$borne_sup_asymptotique),
               rep(c(0.0588667871415690, 0.941133212858431), each = 3L), rel = TB, abs = 0)
  if (!isTRUE(r1)) r1 else if (!isTRUE(r2)) r2
  else identical(s$ensemble_a_risque, rep(2L, 3L)) && identical(s$evenements, rep(1L, 3L)) &&
    all(l$etat == "defini")
})
verifier("C, âge 65 (identité) : bornes de Kaplan-Meier égales aux bornes logit binomiales de la même cellule (E^i = 2, D = 1), 1e-13", {
  i <- match(65L, KC$age)
  proche(c(KC$borne_inf_asymptotique[i], KC$borne_sup_asymptotique[i]),
         c(BC$borne_inf_asymptotique[i], BC$borne_sup_asymptotique[i]), rel = TB, abs = 0)
})
verifier("C, âge 83 (valeur calculée à la main) : défini, q = 1, variance et bornes NA ; âge 64 : 0,5 pour Kaplan-Meier contre 0,675440636660566 pour le binomial", {
  r <- controler_etats(KC, list(list(83, N1)), toutes = FALSE)
  if (!isTRUE(r)) r
  else isTRUE(proche(BC$taux[BC$age == 64L], 0.675440636660566, rel = TB, abs = 0)) &&
    identical(KC$taux[KC$age == 64L], 0.5)
})
verifier("C (identité exacte) : clés et ordre de la table d'exposition, evenements identical() ; somme de d par cellule = evenements ; instants_evenement = nombre de sauts par cellule", {
  sd <- vapply(KC$age, function(a) sum(SC$evenements[SC$age == a]), integer(1L))
  ni <- vapply(KC$age, function(a) sum(SC$age == a), integer(1L))
  identical(KC[c("age", "evenements")], EC[c("age", "evenements")]) && identical(sd, KC$evenements) &&
    identical(ni, KC$instants_evenement)
})
verifier("C, K (identité exacte) : 1 <= d <= r à chaque saut ; sauts triés par instant", {
  s <- rbind(SC, SK)
  all(s$evenements >= 1L & s$evenements <= s$ensemble_a_risque) &&
    !is.unsorted(SC$age_exact, strictly = TRUE) && !is.unsorted(SK$age_exact, strictly = TRUE)
})
verifier("C, K (identité) : survie_cellule au dernier saut de chaque cellule définie = 1 - taux, 1e-15 absolue", {
  ecarts <- c()
  for (p in list(list(KC, SC), list(KK, SK))) {
    d <- p[[1]][p[[1]]$etat == "defini" & p[[1]]$evenements > 0L, ]
    dern <- vapply(d$age, function(a) { s <- p[[2]]$survie_cellule[p[[2]]$age == a]; s[length(s)] }, numeric(1L))
    ecarts <- c(ecarts, dern - (1 - d$taux))
  }
  length(ecarts) > 0L && isTRUE(proche(ecarts, rep(0, length(ecarts)), rel = 0, abs = 1e-15))
})
verifier("C, K (identité) : exposition_nulle exactement dans les cellules sans exposition ni événement de la table d'exposition", {
  ek <- XK$etapes$exposition$resultat$exposition
  identical(KC$etat == "exposition_nulle", EC$exposition_jours == 0 & EC$evenements == 0L) &&
    identical(KK$etat == "exposition_nulle", ek$exposition_jours == 0 & ek$evenements == 0L)
})
verifier("C (valeur calculée à la main) : avertissement kaplan_meier_ensemble_a_risque_vide réservé aux cellules ensemble_a_risque_vide qui portent un événement : nombre 1 (âge 38, C11), 1 événement ; renvoi à diagnostics$etats (6 cellules de cet état)", {
  a <- XC$avertissements[XC$avertissements$code == "kaplan_meier_ensemble_a_risque_vide", ]
  nrow(a) == 1L && identical(a$etape, "brut") && identical(a$nombre, 1L) &&
    grepl("1 cellule(s) à couverture incomplète de l'ensemble à risque qui comptent 1 événement(s)",
          a$message, fixed = TRUE) && grepl("diagnostics$etats", a$message, fixed = TRUE)
})
verifier("K (valeur calculée à la main) : aucun avertissement kaplan_meier_ensemble_a_risque_vide (seule cellule de cet état, 64, sans événement)",
         !any(XK$avertissements$code == "kaplan_meier_ensemble_a_risque_vide") &&
           identical(KK$etat[KK$age == 64L], "ensemble_a_risque_vide"))
verifier("C (avis d'actuary) : survie_cellule et variance_log_survie_cellule NA aux sauts des cellules non définies (38), renseignées ailleurs ; numerateur, age_exact, r et d toujours renseignés", {
  nd <- SC$age %in% KC$age[KC$etat != "defini"]
  any(nd) && all(is.na(SC$survie_cellule[nd])) && all(is.na(SC$variance_log_survie_cellule[nd])) &&
    !anyNA(SC$survie_cellule[!nd]) && !anyNA(SC[c("numerateur", "age_exact", "ensemble_a_risque", "evenements")])
})
verifier("types (consigne du mainteneur, avis de tolérance d'actuary) : numerateur, ensemble_a_risque, evenements, instants_evenement integer ; diagnostics de kaplan_meier integer ; denominateur_instants = 133590L", {
  dk <- XC$etapes$brut$diagnostics$kaplan_meier
  identical(vapply(SC[c("numerateur", "ensemble_a_risque", "evenements")], typeof, ""),
            c(numerateur = "integer", ensemble_a_risque = "integer", evenements = "integer")) &&
    identical(vapply(KC[c("evenements", "instants_evenement")], typeof, ""),
              c(evenements = "integer", instants_evenement = "integer")) &&
    identical(names(dk), c("instants", "evenements", "evenements_a_l_instant_d_entree", "cellules_survie_nulle",
                           "ensemble_a_risque_maximal")) &&
    all(vapply(dk, typeof, "") == "integer") &&
    identical(XC$etapes$brut$parametres$denominateur_instants, 133590L)
})

# --- 4. cohortes d'âge exact : identité avec le binomial (A.10) ---------------

gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
r0 <- tablexp:::table_constante(0, 0:120, "sans rachat")
LOI0 <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
             rachat = list(table = r0, facteurs = c(H = 1, F = 1)))
RO_SIM <- c(id_tete = "id_tete", date_naissance = "date_naissance", date_entree = "date_entree",
            date_sortie = "date_sortie", cause_sortie = "cause_sortie")
for (cfg in list(list(n = 1e5, age = 60L, graine = 20261016L, D = 878L, k = 2L),
                 list(n = 2e4, age = 100L, graine = 20261018L, D = 6060L, k = 11L))) {
  pf <- list(mode = "age_exact", n = cfg$n, proportion_hommes = 1, age = cfg$age, date_entree = D("2016-03-15"))
  sim <- simuler_portefeuille(LOI0, pf, PER, cfg$graine)
  x <- construire_table(sim, RO_SIM, plan_km(estimateurs = c("binomial", "kaplan_meier")))
  km <- x$etapes$brut$resultat$kaplan_meier; bi <- x$etapes$brut$resultat$binomial
  i <- match(cfg$age, km$age)
  verifier(sprintf("cohorte d'âge exact %d ans, n = %.0f, graine %d (identité exacte A.10, sans entrée ni censure dans la cellule) : E^i = n (somme de fractions, 1e-13), q KM = q binomial = D / n et bornes logit égales, 1e-13 relative ; D = %d dont %d le jour d'entrée (comptés : ensemble fermé)",
                   cfg$age, cfg$n, cfg$graine, cfg$D, cfg$k), {
    r <- proche(c(km$taux[i], km$borne_inf_asymptotique[i], km$borne_sup_asymptotique[i]),
                c(bi$taux[i], bi$borne_inf_asymptotique[i], bi$borne_sup_asymptotique[i]), rel = 1e-13, abs = 0)
    k0 <- sum(sim$cause_sortie == "deces" & sim$date_sortie == sim$date_entree, na.rm = TRUE)
    if (!isTRUE(r)) r
    else if (!identical(km$evenements[i], cfg$D) || k0 != cfg$k) sprintf("D = %d, k = %d", km$evenements[i], k0)
    else isTRUE(proche(bi$exposition_initiale[i], cfg$n, rel = 1e-13, abs = 0)) && identical(km$etat[i], "defini") &&
      isTRUE(proche(km$taux[i], cfg$D / cfg$n, rel = 1e-13, abs = 0))
  })
  if (cfg$age == 60L)
    verifier("cohorte 60 ans (débordement, A.7) : r jusqu'à 10^5, aucune variance NA dans les cellules définies à 0 < q < 1 ; ensemble à risque maximal 100000L",
             identical(x$etapes$brut$diagnostics$kaplan_meier$ensemble_a_risque_maximal, 100000L) &&
               !anyNA(km$variance[km$etat == "defini" & km$taux > 0 & km$taux < 1]))
}

# --- 5. refus (valeur calculée à la main : codes et messages) ----------------

verifier("par_annee : kaplan_meier -> refus kaplan_meier_par_annee depuis valider_plan(), avant toute étape",
         leve_refus(valider_plan(plan_km(par_annee = TRUE)), "kaplan_meier_par_annee"))
verifier("par_annee : kaplan_meier -> refus kaplan_meier_par_annee depuis l'appel direct de la fonction d'étape",
         leve_refus(calculer_taux_bruts(EC, character(0), TRUE, "dernier_anniversaire", "u", "kaplan_meier", 0.95),
                    "kaplan_meier_par_annee"))
verifier("par_annee : le refus nomme par_annee et q_x, module brut", {
  r <- tryCatch(valider_plan(plan_km(par_annee = TRUE)), refus_tablexp = function(r) r)
  identical(r$module, "brut") && grepl("par_annee", conditionMessage(r), fixed = TRUE) &&
    grepl("q_x n'y est pas défini", conditionMessage(r), fixed = TRUE) &&
    identical(r$details, list(estimateur = "kaplan_meier", par_annee = TRUE))
})
verifier("par_annee : c(\"binomial\", \"kaplan_meier\") -> binomial_par_annee, levé en premier",
         leve_refus(valider_plan(plan_km(estimateurs = c("binomial", "kaplan_meier"), par_annee = TRUE)),
                    "binomial_par_annee"))
verifier("kaplan_meier n'est plus un estimateur inconnu : plan validé ; estimateurs disponibles dans l'ordre canonique",
         identical(valider_plan(plan_km()), plan_km()) &&
           identical(tablexp:::ESTIMATEURS_BRUT, c("poisson", "binomial", "kaplan_meier")))
verifier("etape_prealable_manquante : brut sans exposition, message et détails inchangés", {
  p <- plan_km(); p$etapes$exposition <- NULL
  r <- tryCatch(valider_plan(p), refus_tablexp = function(r) r)
  identical(r$motif, "etape_prealable_manquante") &&
    identical(conditionMessage(r), "Étape(s) « brut » sans son étape préalable « exposition ».") &&
    identical(r$details, list(etape = "brut", prealable = "exposition"))
})
verifier("etape_prealable_manquante : brut seul, préalables donnees et exposition cités, au pluriel (chaque élément contrôlé)", {
  p <- plan_km(); p$etapes <- p$etapes["brut"]
  r <- tryCatch(valider_plan(p), refus_tablexp = function(r) r)
  identical(r$motif, "etape_prealable_manquante") &&
    identical(conditionMessage(r), "Étape(s) « brut » sans ses étapes préalables « donnees », « exposition ».") &&
    identical(r$details, list(etape = "brut", prealable = c("donnees", "exposition")))
})
verifier("ETAPES : colonne prealable en liste (donnees : aucun ; exposition : donnees ; brut : donnees, exposition)",
         identical(unclass(tablexp:::ETAPES$prealable), list(character(0), "donnees", c("donnees", "exposition"))))

# --- 6. forme, statuts, fiche, invariance ------------------------------------

verifier("objet : tables kaplan_meier et kaplan_meier_sauts après poisson et binomial (ordre canonique) ; paramètres ensemble_a_risque = \"ferme\", denominateur_instants ; hypothèse kaplan_meier", {
  pa <- XC$etapes$brut$parametres
  identical(names(XC$etapes$brut$resultat), c("poisson", "binomial", "kaplan_meier", "kaplan_meier_sauts")) &&
    identical(pa$estimateurs, c("poisson", "binomial", "kaplan_meier")) &&
    identical(pa$ensemble_a_risque, "ferme") &&
    identical(names(pa$hypotheses), c("poisson", "binomial", "kaplan_meier", "probabilite")) &&
    grepl("troncature à gauche non informative", pa$hypotheses$kaplan_meier, fixed = TRUE) &&
    grepl("entrées, puis événements, puis censures", pa$hypotheses$kaplan_meier, fixed = TRUE)
})
verifier("statuts kaplan_meier (valeur calculée à la main) : 4 lignes (taux ponctuel, variance, taux asymptotique, taux exact « aucun : non restitué »)", {
  s <- XC$etapes$brut$parametres$statuts
  k <- s[s$estimateur == "kaplan_meier", ]
  identical(paste(k$grandeur, k$intervalle),
            c("taux sans objet (estimation ponctuelle)", "variance sans objet (estimation ponctuelle)",
              "taux asymptotique", "taux exact")) &&
    startsWith(k$statut[1L], "résultat exact : maximum de vraisemblance") &&
    all(startsWith(k$statut[2:3], "asymptotique")) && startsWith(k$statut[4L], "aucun : non restitué") &&
    grepl("non défini à q = 0 et q = 1", k$statut[3L], fixed = TRUE) && nrow(s) == 13L
})
verifier("fiche : hypotheses_brut porte statuts et hypothèses de Kaplan-Meier, ensemble_a_risque et denominateur_instants", {
  h <- XC$fiche$hypotheses_brut
  identical(names(h), c("estimateurs", "niveau", "exposition_initiale", "tolerance_effectif", "hypotheses",
                        "statuts", "ensemble_a_risque", "denominateur_instants")) &&
    identical(h, XC$etapes$brut$parametres[names(h)])
})
verifier("diagnostics : champ kaplan_meier en fin de liste si l'estimateur est retenu, absent sinon", {
  x <- construire_table(DC, ROLES, plan_km(estimateurs = c("poisson", "binomial")))
  identical(tail(names(XC$etapes$brut$diagnostics), 1L), "kaplan_meier") &&
    is.null(x$etapes$brut$diagnostics$kaplan_meier) && is.null(x$etapes$brut$parametres$ensemble_a_risque) &&
    is.null(x$fiche$hypotheses_brut$denominateur_instants) && is.null(x$etapes$brut$resultat$kaplan_meier_sauts)
})
verifier("age_atteint_annee admis (décision du mainteneur, A.14 question 3) : H12, cellule 74 définie, q = 1 ; convention inscrite",
         identical(RES_HF[["H12 (age_atteint_annee)"]]$objet$etapes$brut$parametres$convention, "age_atteint_annee"))
verifier("Kaplan-Meier seul : tables identical() à celles du plan à trois estimateurs", {
  x <- construire_table(DC, ROLES, plan_km())
  identical(x$etapes$brut$resultat, XC$etapes$brut$resultat[c("kaplan_meier", "kaplan_meier_sauts")]) &&
    identical(names(x$etapes$brut$parametres$hypotheses), "kaplan_meier")
})
# Invariance (A.11, point 6) : un plan sans kaplan_meier donne l'objet du plan
# avec kaplan_meier privé de ses seuls ajouts. L'identité à l'état 103d396 est
# mesurée hors batterie (commit) et relève des références (#17).
invariance_sans_km <- function(x, donnees, roles) {
  p <- x$fiche$plan
  p$etapes$brut$estimateurs <- setdiff(p$etapes$brut$estimateurs, "kaplan_meier")
  y <- sans_execution(construire_table(donnees, roles, p))
  z <- sans_execution(x)
  b <- z$etapes$brut
  b$resultat$kaplan_meier <- NULL; b$resultat$kaplan_meier_sauts <- NULL
  b$parametres$estimateurs <- setdiff(b$parametres$estimateurs, "kaplan_meier")
  b$parametres$hypotheses$kaplan_meier <- NULL
  b$parametres$ensemble_a_risque <- NULL; b$parametres$denominateur_instants <- NULL
  b$parametres$statuts <- b$parametres$statuts[b$parametres$statuts$estimateur != "kaplan_meier", , drop = FALSE]
  b$diagnostics$kaplan_meier <- NULL
  b$diagnostics$etats <- b$diagnostics$etats[b$diagnostics$etats$estimateur != "kaplan_meier", , drop = FALSE]
  z$etapes$brut <- b
  garde <- z$avertissements$code != "kaplan_meier_ensemble_a_risque_vide"
  z$avertissements <- z$avertissements[garde, , drop = FALSE]; rownames(z$avertissements) <- NULL
  z$fiche$avertissements <- z$avertissements
  z$fiche$plan <- p
  z$fiche$hypotheses_brut <- b$parametres[c("estimateurs", "niveau", "exposition_initiale", "tolerance_effectif",
                                            "hypotheses", "statuts")]
  identical(y, z)
}
verifier("invariance du plan sans kaplan_meier (cas C, trois estimateurs) : objet identical() à celui du plan avec kaplan_meier privé de ses seuls ajouts",
         invariance_sans_km(XC, DC, ROLES))
verifier("invariance du plan sans kaplan_meier (cas K) : même contrôle", invariance_sans_km(XK, DK, ROLES))
verifier("deux constructions identical() hors execution ; rejeu de la fiche identical() (cas C, trois estimateurs)",
         identical(sans_execution(XC), sans_execution(construire_table(DC, ROLES, XC$fiche$plan))) &&
           identical(sans_execution(XC), sans_execution(construire_table(DC, XC$fiche$roles, XC$fiche$plan,
                                                                         XC$fiche$provenance))))

# --- 7. jeu fixé de la contre-vérification par survfit (A.12, #7) ------------

DOSSIER_CV <- file.path(RACINE, "tests", "contre_verification")
JEU <- utils::read.csv(file.path(DOSSIER_CV, "jeu_kaplan_meier.csv"), comment.char = "#", stringsAsFactors = FALSE)
verifier("jeu fixé (identité exacte) : dates des segments C, K et Z identical() aux cas C, K et Z de ce fichier", {
  jc <- JEU[JEU$segment == "C", ]; jk <- JEU[JEU$segment == "K", ]; jz <- JEU[JEU$segment == "Z", ]
  if (!identical(unique(JEU$segment), c("C", "K", "Z")) || !identical(D(jz$naissance), DZ$n) ||
      !identical(D(jz$entree), DZ$e) || !identical(D(jz$sortie), DZ$s) || !identical(jz$cause, DZ$c) ||
      !identical(jz$tete, DZ$t)) return("segment Z")
  identical(D(jc$naissance), DC$n) && identical(D(jc$entree), DC$e) && identical(D(jc$sortie), DC$s) &&
    identical(jc$cause, DC$c) && identical(D(jk$naissance), DK$n) && identical(D(jk$entree), DK$e) &&
    identical(D(jk$sortie), DK$s) && identical(jk$cause, DK$c) && identical(jk$tete, DK$t)
})
verifier("jeu fixé (identité exacte) : âges rationnels x + j / L de la fenêtre et indicateurs retenue / événement égaux à ceux du moteur (fenetres_observations(), position_dans_cellule()) ; C9 seule non retenue", {
  obs <- data.frame(date_naissance = D(JEU$naissance), debut = D(JEU$entree), fin = D(JEU$sortie), cause = JEU$cause)
  f <- tablexp:::fenetres_observations(obs, as.numeric(PER$debut), as.numeric(PER$fin) + 1, "deces")
  pa <- tablexp:::position_dans_cellule(f$calendrier, f$composantes, f$a, "dernier_anniversaire")
  pb <- tablexp:::position_dans_cellule(f$calendrier, f$composantes, f$b, "dernier_anniversaire")
  ret <- f$a < f$b | f$evenement
  na_si <- function(v) as.integer(ifelse(ret, v, NA))
  identical(JEU$retenue, ret) && identical(JEU$evenement, f$evenement) && identical(JEU$tete[!ret], "C9") &&
    identical(JEU[c("debut_x", "debut_j", "debut_L", "fin_x", "fin_j", "fin_L")],
              data.frame(debut_x = na_si(pa$x), debut_j = na_si(pa$j), debut_L = na_si(pa$longueur),
                         fin_x = na_si(pb$x), fin_j = na_si(pb$j), fin_L = na_si(pb$longueur)))
})
verifier("jeu fixé, exemples de la spécification (valeur calculée à la main, A.12) : C7 64 + 142/366 -> 65 + 0/365 ; C10 et C11 de durée nulle ; K1 69 + 151/365 -> 75 + 0/365", {
  ligne <- function(t) unlist(JEU[JEU$tete == t, c("debut_x", "debut_j", "debut_L", "fin_x", "fin_j", "fin_L")])
  identical(unname(ligne("C7")), c(64L, 142L, 366L, 65L, 0L, 365L)) &&
    identical(unname(ligne("C10")), c(64L, 241L, 365L, 64L, 241L, 365L)) &&
    identical(unname(ligne("C11")), c(38L, 165L, 365L, 38L, 165L, 365L)) &&
    identical(unname(ligne("K1")), c(69L, 151L, 365L, 75L, 0L, 365L))
})
XJ <- construire_table(data.frame(n = D(JEU$naissance), e = D(JEU$entree), s = D(JEU$sortie), c = JEU$cause,
                                  t = JEU$tete, g = JEU$segment, stringsAsFactors = FALSE),
                       c(ROLES, segment = "g"), plan_km(segmentation = "segment"))
KJ <- XJ$etapes$brut$resultat$kaplan_meier
SJ <- XJ$etapes$brut$resultat$kaplan_meier_sauts
verifier("jeu fixé (identité exacte) : segments C, K et Z de la base commune identical() aux constructions séparées (tables kaplan_meier et sauts, hors colonne segment)", {
  sans <- function(t, g) { u <- t[t$segment == g, -1L, drop = FALSE]; rownames(u) <- NULL; u }
  identical(sans(KJ, "C"), KC) && identical(sans(KJ, "K"), KK) && identical(sans(SJ, "C"), SC) &&
    identical(sans(SJ, "K"), SK) && identical(sans(KJ, "Z"), KZ) && identical(sans(SJ, "Z"), SZ)
})
source(file.path(DOSSIER_CV, "comparer_survfit_kaplan_meier.R"), local = TRUE)
FICHIER_SURVFIT <- file.path(DOSSIER_CV, "valeurs_survfit_kaplan_meier.csv")
# Fichier versionné depuis #7 (valeurs produites hors CI sur le poste du
# mainteneur) : son absence, un en-tête incomplet ou un écart est un ECHEC.
verifier("contre-vérification par survival::survfit() (valeurs hors CI, poste du mainteneur) : cellules définies des segments C, K et Z, age_exact 1e-13 absolue, r et d exacts, survie, q et std.err 1e-13 relative, bornes 1e-12 relative", {
  if (!file.exists(FICHIER_SURVFIT)) "fichier de valeurs absent (tests/contre_verification/valeurs_survfit_kaplan_meier.csv, à produire par survfit_kaplan_meier.R)"
  else {
    v <- lire_valeurs_survfit(FICHIER_SURVFIT)
    if (!any(grepl("^# survival : ", v$entete)) || !any(grepl("^# R : ", v$entete)) ||
        !any(grepl("^# date : ", v$entete)) || !any(grepl("^# sessionInfo", v$entete)))
      stop("en-tête du fichier de valeurs incomplet (R, survival, date, sessionInfo)")
    r <- comparer_survfit(v$valeurs, KJ, SJ)
    if (!isTRUE(r)) stop(paste(r, collapse = " ; "))
    TRUE
  }
})
