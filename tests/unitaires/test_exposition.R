# -----------------------------------------------------------------------------
#  tests/unitaires/test_exposition.R : module `exposition` (issue #11 ; E2.3,
#  E2.5, E2.6 ; ADR 0007, points 5.2, 5.3 et 10 ; spécification d'actuary,
#  A.0, A.3 et cas H1 à H13, F1 à F4 de A.4)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : cas H1 à H13 et F1 à F4 de la
#    spécification (jours, fractions j / L, totaux, événements et
#    compléments), tolérance 1e-12 sur les années ; les longueurs L_x des
#    années d'âge entières (« 1 » dans la spécification) sont écrites en dur
#    et contrôlées par le total en jours de la spécification ; un cas
#    complémentaire H4c (complément c2 différent de c1) est calculé à la main
#    par coder ;
#  - recoupement : arithmétique de dates du module contre as.Date() ;
#  - âge au dernier anniversaire comparé à age_revolu() du module donnees,
#    dont l'exposition ne se sert pas.
#  Les cas passent par construire_table() (étapes donnees puis exposition).
# -----------------------------------------------------------------------------

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))  # B = 2025-01-01
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
TOL <- 1e-12

# Construction d'un cas : lignes (naissance ; entrée -> sortie, cause),
# identifiant de tête facultatif ; rend la table d'exposition et l'objet.
construire_cas <- function(naissance, entree, sortie, cause, tete = NULL, delai = 0,
                           convention = "plus_fine", par_annee = FALSE, transition = VD) {
  d <- data.frame(n = D(naissance), e = D(entree), s = D(sortie), c = cause, stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
  if (!is.null(tete)) { d$t <- tete; roles <- c(roles, id_tete = "t") }
  plan <- list(transition = transition, periode = PER, delai_declaration = delai,
               segmentation = character(0),
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = convention, par_annee = par_annee)))
  x <- construire_table(d, roles, plan)
  list(table = x$etapes$exposition$resultat$exposition, objet = x)
}

# Compare la table aux attendus : ages, jours (numérateurs), L (dénominateurs),
# total en jours et en années (spécification), événements (age, c1, c2 en
# jours, dénominateur L de la cellule de l'événement).
controler_cas <- function(t, ages, jours, longueurs, total_jours, total_annees = NA,
                          ev_age = integer(0), c1 = numeric(0), c2 = c1, ev_L = numeric(0)) {
  ecarts <- character(0)
  exp_ages <- t$age[t$exposition_jours > 0]
  if (!identical(exp_ages, as.integer(ages)))
    ecarts <- c(ecarts, sprintf("âges exposés %s au lieu de %s", paste(exp_ages, collapse = " "),
                                paste(ages, collapse = " ")))
  else {
    i <- match(ages, t$age)
    if (!identical(t$exposition_jours[i], as.numeric(jours)))
      ecarts <- c(ecarts, sprintf("jours %s au lieu de %s", paste(t$exposition_jours[i], collapse = " "),
                                  paste(jours, collapse = " ")))
    if (length(ages)) {
      r <- proche(t$exposition_centrale[i], jours / longueurs, rel = 0, abs = TOL)
      if (!isTRUE(r)) ecarts <- c(ecarts, paste("années :", r))
    }
  }
  if (!identical(sum(t$exposition_jours), as.numeric(total_jours)))
    ecarts <- c(ecarts, sprintf("total %s j au lieu de %s", sum(t$exposition_jours), total_jours))
  if (!is.na(total_annees)) {
    r <- proche(sum(t$exposition_centrale), total_annees, rel = 0, abs = TOL)
    if (!isTRUE(r)) ecarts <- c(ecarts, paste("total en années :", r))
  }
  ev <- t[t$evenements > 0, , drop = FALSE]
  if (!identical(ev$age, as.integer(ev_age)) || !identical(ev$evenements, rep(1L, length(ev_age))))
    ecarts <- c(ecarts, sprintf("événements aux âges %s au lieu de %s", paste(ev$age, collapse = " "),
                                paste(ev_age, collapse = " ")))
  else if (length(ev_age) && "complement_initial_cellule" %in% names(t)) {
    r1 <- proche(ev$complement_initial_cellule, c1 / ev_L, rel = 0, abs = TOL)
    r2 <- proche(ev$complement_initial_fenetre, c2 / ev_L, rel = 0, abs = TOL)
    if (!isTRUE(r1)) ecarts <- c(ecarts, paste("c1 :", r1))
    if (!isTRUE(r2)) ecarts <- c(ecarts, paste("c2 :", r2))
  }
  if (length(ev_age) == 0L && "complement_initial_cellule" %in% names(t) &&
      (any(t$complement_initial_cellule != 0) || any(t$complement_initial_fenetre != 0)))
    ecarts <- c(ecarts, "compléments non nuls sans événement")
  if (length(ecarts)) paste(ecarts, collapse = " ; ") else TRUE
}

# --- cas H1 à H13 (valeurs calculées à la main, spécification A.4) -------------

H1 <- construire_cas("1960-07-01", "2020-03-01", NA, "en_cours")
verifier("H1 entrée en cours d'âge, fin de période (valeur calculée à la main) : 59 : 122/366 ; 60-63 : 1 ; 64 : 184/365 ; 1767 j ; 4,837442922374430",
         controler_cas(H1$table, 59:64, c(122, 365, 365, 365, 366, 184), c(366, 365, 365, 365, 366, 365),
                       1767, 4.837442922374430))
H2 <- construire_cas("1945-10-15", "2016-05-20", "2019-02-10", "deces")
verifier("H2 décès (valeur calculée à la main) : 70 : 148/366 ; 71, 72 : 1 ; 73 : 118/365 ; 996 j ; 2,727659255932330 ; événement 73, c1 = c2 = 247/365",
         controler_cas(H2$table, 70:73, c(148, 365, 365, 118), c(366, 365, 365, 365), 996, 2.727659255932330,
                       73L, 247, 247, 365))
verifier("H2 : exposition initiale E^i_73 = E^c_73 + c1 = 1 (valeur calculée à la main)",
         proche(sum(H2$table[H2$table$age == 73L, c("exposition_centrale", "complement_initial_cellule")]),
                1, rel = 0, abs = TOL))
H3 <- construire_cas("1950-01-10", "2010-06-01", NA, "en_cours")
verifier("H3 entrée avant la période (valeur calculée à la main) : 64 : 9/365 ; 65-73 : 1 (L de la spécification) ; 74 : 357/366 ; 3653 j ; 10,000067370312149",
         controler_cas(H3$table, 64:74,
                       c(9, 365, 366, 365, 365, 365, 366, 365, 365, 365, 357),
                       c(365, 365, 366, 365, 365, 365, 366, 365, 365, 365, 366), 3653, 10.000067370312149))
H4a <- construire_cas("1940-12-05", "2022-01-01", "2024-11-15", "deces")
verifier("H4a délai 0 (valeur calculée à la main) : 81 : 338/365 ; 82 : 1 ; 83 : 346/366 ; 1049 j ; 2,871382588517104 ; événement 83, 20/366",
         controler_cas(H4a$table, 81:83, c(338, 365, 346), c(365, 365, 366), 1049, 2.871382588517104,
                       83L, 20, 20, 366))
H4b <- construire_cas("1940-12-05", "2022-01-01", "2024-11-15", "deces", delai = 90)
verifier("H4b délai 90 j, B = 2024-10-03 (valeur calculée à la main) : 81 : 338/365 ; 82 : 1 ; 83 : 303/366 ; 1006 j ; 2,753896249719290 ; aucun événement",
         controler_cas(H4b$table, 81:83, c(338, 365, 303), c(365, 365, 366), 1006, 2.753896249719290))
verifier("H4b : fin effective 2024-10-02 inscrite dans la fiche et les paramètres de l'étape",
         identical(H4b$objet$fiche$fin_effective, D("2024-10-02")) &&
           identical(H4b$objet$etapes$exposition$parametres$fin_effective, D("2024-10-02")))
H4c <- construire_cas("1940-12-05", "2022-01-01", "2024-11-15", "deces", delai = 30)
verifier("H4c (cas complémentaire, calculé à la main par coder) délai 30 j, B = 2024-12-02 avant anniv(84) = 2024-12-05 : événement 83, c1 = 20/366, c2 = 17/366",
         controler_cas(H4c$table, 81:83, c(338, 365, 346), c(365, 365, 366), 1049, NA,
                       83L, 20, 17, 366))
H5 <- construire_cas("1955-06-30", "2023-01-01", "2025-02-01", "deces")
verifier("H5 sortie après la fin (valeur calculée à la main) : 67 : 180/365 ; 68 : 366/366 ; 69 : 185/365 ; 731 j ; 2 ; aucun événement",
         controler_cas(H5$table, 67:69, c(180, 366, 185), c(365, 366, 365), 731, 2))
H6 <- construire_cas("1964-02-29", "2021-01-01", NA, "en_cours")
verifier("H6 29 février (valeur calculée à la main) : 56 : 59/366 ; 57-59 : 1 ; 60 : 307/366 ; 1461 j ; 4",
         controler_cas(H6$table, 56:60, c(59, 365, 365, 365, 307), c(366, 365, 365, 365, 366), 1461, 4))
H7 <- construire_cas("1948-08-20", c("2016-01-01", "2018-07-01"), c("2019-01-01", "2021-05-10"),
                     c("terme", "deces"), tete = c("T", "T"))
verifier("H7 tête fusionnée (valeur calculée à la main) : 67 : 232/366 ; 68-70 : 365/365 ; 71 : 366/366 ; 72 : 263/365 ; 1956 j ; 5,354427726626245 ; événement 72, 102/365",
         controler_cas(H7$table, 67:72, c(232, 365, 365, 365, 366, 263), c(366, 365, 365, 365, 366, 365),
                       1956, 5.354427726626245, 72L, 102, 102, 365))
H8 <- construire_cas("1955-04-12", "2019-09-01", "2020-04-12", "deces")
verifier("H8 décès le jour de l'anniversaire (valeur calculée à la main) : 64 : 224/366 ; 224 j ; 0,612021857923497 ; événement 65 à exposition nulle, 365/365",
         controler_cas(H8$table, 64L, 224, 366, 224, 0.612021857923497, 65L, 365, 365, 365))
verifier("H8 : cellule 65 présente, exposition nulle, relevé evenement_sans_exposition = 1",
         identical(H8$table$age, 64:65) && H8$table$exposition_jours[2L] == 0 &&
           identical(H8$objet$etapes$exposition$diagnostics$cellules_evenement_sans_exposition, 1L) &&
           "evenement_sans_exposition" %in% H8$objet$avertissements$code)
H9d <- construire_cas("1970-03-03", "2017-05-05", "2021-08-08", "rachat")
verifier("H9 sortie concurrente, transition décès (valeur calculée à la main) : 47 : 302/365 ; 48-50 : 1 ; 51 : 158/365 ; 1556 j ; 4,260273972602739 ; aucun décès, une sortie concurrente à 51",
         isTRUE(controler_cas(H9d$table, 47:51, c(302, 365, 366, 365, 158), c(365, 365, 366, 365, 365),
                              1556, 4.260273972602739)) &&
           identical(H9d$table$sorties_concurrentes, c(0L, 0L, 0L, 0L, 1L)))
H9r <- construire_cas("1970-03-03", "2017-05-05", "2021-08-08", "rachat", transition = RA)
verifier("H9 transition en cours -> racheté (valeur calculée à la main) : même exposition ; événement 51, 207/365",
         controler_cas(H9r$table, 47:51, c(302, 365, 366, 365, 158), c(365, 365, 366, 365, 365),
                       1556, 4.260273972602739, 51L, 207, 207, 365))
H10a <- construire_cas("1950-05-05", "2005-01-01", "2014-12-31", "deces")
verifier("H10a sortie avant le début (valeur calculée à la main) : aucune cellule, 0 j, aucun événement ; avertissement aucune_exposition",
         nrow(H10a$table) == 0L && "aucune_exposition" %in% H10a$objet$avertissements$code)
H10b <- construire_cas("1950-05-05", "2005-01-01", "2015-01-01", "deces")
verifier("H10b décès le premier jour de la période (valeur calculée à la main) : aucune exposition ; événement 64, exposition nulle, 124/365",
         identical(H10b$table$age, 64L) && controler_cas(H10b$table, integer(0), numeric(0), numeric(0), 0, NA, 64L, 124, 124, 365))
H11 <- construire_cas("1980-01-01", "2018-06-15", "2018-06-15", "deces")
verifier("H11 décès le jour de l'entrée (valeur calculée à la main) : aucune exposition ; événement 38, 200/365",
         identical(H11$table$age, 38L) && controler_cas(H11$table, integer(0), numeric(0), numeric(0), 0, NA, 38L, 200, 200, 365))
H12 <- construire_cas("1945-10-15", "2016-05-20", "2019-02-10", "deces", convention = "age_atteint_annee")
verifier("H12 forçage age_atteint_annee (valeur calculée à la main) : 71 : 226/366 ; 72, 73 : 1 ; 74 : 40/365 ; 996 j ; événement 74, 325/365",
         controler_cas(H12$table, 71:74, c(226, 365, 365, 40), c(366, 365, 365, 365), 996, NA,
                       74L, 325, 325, 365))
verifier("H12 : convention retenue age_atteint_annee, forcée, inscrite dans la fiche ; unité année civile ; avertissement convention_forcee",
         identical(H12$objet$fiche$convention,
                   list(demandee = "age_atteint_annee", native = "dernier_anniversaire",
                        retenue = "age_atteint_annee", forcee = TRUE)) &&
           startsWith(H12$objet$fiche$unite_exposition, "annee_civile") &&
           "convention_forcee" %in% H12$objet$avertissements$code)
H13 <- construire_cas("1960-07-01", "2020-03-01", NA, "en_cours", par_annee = TRUE)
attendu_h13 <- data.frame(
  age = c(59L, 60L, 60L, 61L, 61L, 62L, 62L, 63L, 63L, 64L),
  annee = c(2020L, 2020L, 2021L, 2021L, 2022L, 2022L, 2023L, 2023L, 2024L, 2024L),
  jours = c(122, 184, 181, 184, 181, 184, 181, 184, 182, 184),
  L = c(366, 365, 365, 365, 365, 365, 365, 366, 366, 365))
verifier("H13 par_annee (valeur calculée à la main) : (59, 2020) : 122/366 ; (60, 2020) : 184/365 ; ... ; (64, 2024) : 184/365 ; 1767 j ; autres cellules nulles",
         {
           t <- H13$table
           nz <- t[t$exposition_jours > 0, , drop = FALSE]
           if (!identical(nz$age, attendu_h13$age) || !identical(nz$annee, attendu_h13$annee))
             "cellules non nulles différentes"
           else if (!identical(nz$exposition_jours, attendu_h13$jours)) "jours différents"
           else if (sum(t$exposition_jours) != 1767) "total différent de 1767 j"
           else proche(nz$exposition_centrale, attendu_h13$jours / attendu_h13$L, rel = 0, abs = TOL)
         })
verifier("H13 : somme sur les années = exposition par âge seul (H1), exactement en jours",
         identical(as.numeric(tapply(H13$table$exposition_jours, H13$table$age, sum)),
                   H1$table$exposition_jours))
verifier("H13 : par_annee sans compléments de l'exposition initiale (A.3)",
         !any(c("complement_initial_cellule", "complement_initial_fenetre") %in% names(H13$table)))

# --- fusion par tête F1 à F4 (valeurs calculées à la main, A.4) --------------

F1 <- list("1950-03-15", c("2016-01-01", "2018-07-01"), c("2019-12-31", NA), c("rachat", "en_cours"))
F1t <- construire_cas(F1[[1]], F1[[2]], F1[[3]], F1[[4]], tete = c("T", "T"))
verifier("F1 chevauchants, unité tête (valeur calculée à la main) : 65 : 74/366 ; 66-73 : 1 ; 74 : 292/365 ; 3288 j ; aucun événement",
         controler_cas(F1t$table, 65:74, c(74, 365, 365, 365, 366, 365, 365, 365, 366, 292),
                       c(366, 365, 365, 365, 366, 365, 365, 365, 366, 365), 3288))
F1c <- construire_cas(F1[[1]], F1[[2]], F1[[3]], F1[[4]])
verifier("F1, unité contrat (sans id_tete ; valeur calculée à la main) : 3836 j",
         sum(F1c$table$exposition_jours) == 3836)
F2 <- construire_cas("1952-09-30", c("2016-01-01", "2018-01-01"), c("2018-01-01", "2021-05-10"),
                     c("terme", "deces"), tete = c("T", "T"))
verifier("F2 contigus (valeur calculée à la main) : 63 : 273/366 ; 64-67 : 1 ; 68 : 222/365 ; 1956 j ; événement 68, 143/365",
         controler_cas(F2$table, 63:68, c(273, 365, 365, 365, 366, 222), c(366, 365, 365, 365, 366, 365),
                       1956, NA, 68L, 143, 143, 365))
F3 <- construire_cas("1949-11-11", c("2016-01-01", "2017-01-02"), c("2017-01-01", NA),
                     c("terme", "en_cours"), tete = c("T", "T"))
verifier("F3 disjoints d'un jour (valeur calculée à la main) : 66 : 315/366 ; 67 : 364/365 ; 68-74 : 1 ; 75 : 51/365 ; 3287 j",
         controler_cas(F3$table, 66:75, c(315, 364, 365, 365, 366, 365, 365, 365, 366, 51),
                       c(366, 365, 365, 365, 366, 365, 365, 365, 366, 365), 3287))
F4 <- list("1945-06-06", c("2016-01-01", "2019-01-01"), c("2022-03-01", "2022-03-01"), c("deces", "deces"))
F4t <- construire_cas(F4[[1]], F4[[2]], F4[[3]], F4[[4]], tete = c("T", "T"))
verifier("F4 décès sur les deux contrats, unité tête (valeur calculée à la main) : 70 : 157/366 ; 71-75 : 1 ; 76 : 268/365 ; 2251 j ; un décès à 76, 97/365",
         controler_cas(F4t$table, 70:76, c(157, 365, 365, 365, 366, 365, 268), c(366, 365, 365, 365, 366, 365, 365),
                       2251, NA, 76L, 97, 97, 365))
F4c <- construire_cas(F4[[1]], F4[[2]], F4[[3]], F4[[4]])
verifier("F4, unité contrat (valeur calculée à la main) : 3406 j, 2 décès ; avertissement unite_contrat",
         sum(F4c$table$exposition_jours) == 3406 && sum(F4c$table$evenements) == 2L &&
           "unite_contrat" %in% F4c$objet$avertissements$code)

# --- paramètres, recette et convention -----------------------------------------

controler_parametres_exposition <- tablexp:::controler_parametres_exposition
resoudre_convention <- tablexp:::resoudre_convention
deduire_recette <- tablexp:::deduire_recette
verifier("deduire_recette : rôles requis présents -> lignes_dates_completes, convention native dernier_anniversaire",
         identical(deduire_recette(c(date_naissance = "a", date_entree = "b", date_sortie = "c",
                                     cause_sortie = "d"))[c("recette", "convention_native")],
                   list(recette = "lignes_dates_completes", convention_native = "dernier_anniversaire")))
verifier("deduire_recette : rôle requis absent -> refus aucune_recette, message qui nomme le rôle et la recette", {
  r <- tryCatch(deduire_recette(c(date_naissance = "a", date_entree = "b", cause_sortie = "d")),
                refus_tablexp = function(r) r)
  inherits(r, "refus_tablexp") && identical(r$motif, "aucune_recette") &&
    identical(r$details$role, "date_sortie") &&
    grepl("date_sortie", conditionMessage(r), fixed = TRUE) &&
    grepl("lignes_dates_completes", conditionMessage(r), fixed = TRUE)
})
verifier("resoudre_convention : plus_fine -> convention native, non forcée",
         identical(resoudre_convention("plus_fine", "dernier_anniversaire"),
                   list(demandee = "plus_fine", native = "dernier_anniversaire",
                        retenue = "dernier_anniversaire", forcee = FALSE)))
verifier("resoudre_convention : dernier_anniversaire demandé explicitement -> non forcée",
         isFALSE(resoudre_convention("dernier_anniversaire", "dernier_anniversaire")$forcee))
verifier("resoudre_convention : forçage vers une convention plus fine que la native -> refus convention_plus_fine",
         leve_refus(resoudre_convention("dernier_anniversaire", "age_atteint_annee"), "convention_plus_fine"))
verifier("controler_parametres_exposition : age_fourni -> refus convention_non_mise_en_oeuvre",
         leve_refus(controler_parametres_exposition("age_fourni", FALSE), "convention_non_mise_en_oeuvre"))
verifier("controler_parametres_exposition : convention inconnue (deduite, ancienne valeur du point 5.2) -> convention_invalide",
         leve_refus(controler_parametres_exposition("deduite", FALSE), "convention_invalide"))
verifier("controler_parametres_exposition : convention non chaîne -> convention_invalide",
         leve_refus(controler_parametres_exposition(NA_character_, FALSE), "convention_invalide"))
verifier("controler_parametres_exposition : par_annee NA ou non logique -> par_annee_invalide",
         isTRUE(leve_refus(controler_parametres_exposition("plus_fine", NA), "par_annee_invalide")) &&
           isTRUE(leve_refus(controler_parametres_exposition("plus_fine", "oui"), "par_annee_invalide")))

# --- arithmétique de dates (recoupement : as.Date ; comparaison à age_revolu()) --

annee_du_jour <- tablexp:::annee_du_jour
composantes_jour <- tablexp:::composantes_jour
anniversaire_age <- tablexp:::anniversaire_age
age_dernier_anniversaire <- tablexp:::age_dernier_anniversaire
CAL <- tablexp:::calendrier(1690, 2210)
jours <- seq(as.numeric(D("1696-01-01")), as.numeric(D("2204-12-31")), by = 1)
verifier("composantes_jour (recoupement : format(as.Date)) : an, mois, jour exacts du 1696-01-01 au 2204-12-31 (1700, 1800, 1900, 2100 non bissextiles ; 2000 bissextile)", {
  cj <- composantes_jour(jours)
  dd <- structure(jours, class = "Date")
  identical(cj$an, as.numeric(format(dd, "%Y"))) && identical(cj$mois, as.numeric(format(dd, "%m"))) &&
    identical(cj$jour, as.numeric(format(dd, "%d")))
})
verifier("annee_calendrier (recoupement : format(as.Date)) : année civile exacte du 1696-01-01 au 2204-12-31",
         identical(tablexp:::annee_calendrier(CAL, jours),
                   as.numeric(format(structure(jours, class = "Date"), "%Y"))))
verifier("anniversaire_age (recoupement : as.Date) : anniversaires 0 à 120 de naissances du 1890-01-01 au 2010-12-31 tous les 7 jours, 29/02 -> 01/03 des années non bissextiles", {
  n <- seq(as.numeric(D("1890-01-01")), as.numeric(D("2010-12-31")), by = 7)
  n <- c(n, as.numeric(D(c("1896-02-29", "1904-02-29", "1964-02-29", "2000-02-29"))))
  cn <- composantes_jour(n)
  nd <- structure(n, class = "Date")
  ok <- TRUE
  for (x in 0:120) {
    att <- as.Date(sprintf("%04d-%s", as.integer(format(nd, "%Y")) + x, format(nd, "%m-%d")), optional = TRUE)
    m <- is.na(att); att[m] <- as.Date(sprintf("%04d-03-01", as.integer(format(nd[m], "%Y")) + x))
    if (!identical(anniversaire_age(CAL, cn, x), as.numeric(att))) { ok <- sprintf("écart à x = %d", x); break }
  }
  ok
})
verifier("anniversaire_age (valeur calculée à la main, T2) : 1964-02-29 -> 2020-02-29, 2021-03-01, 2022-03-01, 2023-03-01, 2024-02-29",
         identical(anniversaire_age(CAL, composantes_jour(rep(as.numeric(D("1964-02-29")), 5)), 56:60),
                   as.numeric(D(c("2020-02-29", "2021-03-01", "2022-03-01", "2023-03-01", "2024-02-29")))))
verifier("age_dernier_anniversaire (comparée à age_revolu() du module donnees) : 2·10^4 couples (naissance, jour) tirés, plus trois naissances au 29/02 sur tous les jours de 1900 à 2030", {
  tir <- tablexp:::sous_graine(11L, list(
    n = sample(as.numeric(D("1890-01-01")):as.numeric(D("2010-12-31")), 2e4, replace = TRUE),
    d = sample(0:(110 * 366), 2e4, replace = TRUE)))
  t29 <- as.numeric(D("1900-01-01")):as.numeric(D("2030-12-31"))
  n <- c(tir$n, rep(as.numeric(D(c("1896-02-29", "1964-02-29", "2000-02-29"))), each = length(t29)))
  t <- c(tir$n + tir$d, rep(t29, 3L))
  garde <- t >= n
  identical(as.numeric(age_dernier_anniversaire(CAL, composantes_jour(n[garde]), t[garde])),
            as.numeric(tablexp:::age_revolu(n[garde], t[garde])))
})
verifier("age_dernier_anniversaire (valeur calculée à la main, T2) : 1964-02-29 a 56 ans au 2021-02-28, 57 au 2021-03-01, 60 au 2024-02-29",
         identical(age_dernier_anniversaire(CAL, composantes_jour(rep(as.numeric(D("1964-02-29")), 3)),
                                            as.numeric(D(c("2021-02-28", "2021-03-01", "2024-02-29")))),
                   c(56, 57, 60)))

# --- segmentation -----------------------------------------------------------------

verifier("segmentation sexe x segment : une plage d'âges par segment, segments ordonnés (radix), types conservés", {
  d <- data.frame(n = D(c("1960-07-01", "1945-10-15", "1960-07-01")),
                  e = D(c("2020-03-01", "2016-05-20", "2020-03-01")),
                  s = D(c(NA, "2019-02-10", NA)), c = c("en_cours", "deces", "en_cours"),
                  sx = c("H", "F", "H"), sg = c(2L, 1L, 1L), stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c",
             sexe = "sx", segment = "sg")
  plan <- list(transition = VD, periode = PER, delai_declaration = 0, segmentation = c("sexe", "segment"),
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  t <- construire_table(d, roles, plan)$etapes$exposition$resultat$exposition
  cles <- unique(t[c("sexe", "segment")])
  if (!identical(cles$sexe, c("F", "H", "H")) || !identical(cles$segment, c(1L, 1L, 2L))) "ordre des segments"
  else if (!identical(t$age[t$sexe == "F"], 70:73)) "plage F"
  else if (!identical(t$exposition_jours[t$sexe == "H" & t$segment == 1L], H1$table$exposition_jours)) "segment H-1"
  else if (sum(t$evenements) != 1L) "événements"
  else TRUE
})

verifier("segmentation : clés indépendantes des niveaux d'un facteur (facteurs aux niveaux permutés et chaîne : tables identical(), clés en character)", {
  d <- data.frame(n = D(c("1960-07-01", "1945-10-15", "1960-07-01", "1950-01-10")),
                  e = D(c("2020-03-01", "2016-05-20", "2020-03-01", "2010-06-01")),
                  s = D(c(NA, "2019-02-10", NA, NA)), c = c("en_cours", "deces", "en_cours", "en_cours"),
                  sx = c("H", "F", "H", "F"), sg = c("nord", "sud", "sud", "nord"), stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c",
             sexe = "sx", segment = "sg")
  plan <- list(transition = VD, periode = PER, delai_declaration = 0, segmentation = c("sexe", "segment"),
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  tab <- function(d) construire_table(d, roles, plan)$etapes$exposition$resultat$exposition
  t_chaine <- tab(d)
  d1 <- d; d1$sx <- factor(d$sx, levels = c("H", "F")); d1$sg <- factor(d$sg, levels = c("sud", "nord"))
  d2 <- d; d2$sx <- factor(d$sx, levels = c("F", "H")); d2$sg <- factor(d$sg, levels = c("nord", "sud"))
  t1 <- tab(d1); t2 <- tab(d2)
  if (!identical(t1, t2)) "facteurs aux niveaux permutés : tables différentes"
  else if (!identical(t1, t_chaine)) "facteur et chaîne : tables différentes"
  else if (!is.character(t1$sexe) || !is.character(t1$segment)) "clés non character"
  else if (!identical(unique(paste(t1$sexe, t1$segment)), c("F nord", "F sud", "H nord", "H sud")))
    "ordre des segments (radix sur les chaînes)"
  else TRUE
})
verifier("segmentation : segment entier rendu en integer et trié sur sa valeur (2 avant 10)", {
  d <- data.frame(n = D(c("1960-07-01", "1945-10-15")), e = D(c("2020-03-01", "2016-05-20")),
                  s = D(c(NA, "2019-02-10")), c = c("en_cours", "deces"), sg = c(10L, 2L),
                  stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", segment = "sg")
  plan <- list(transition = VD, periode = PER, delai_declaration = 0, segmentation = "segment",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  t <- construire_table(d, roles, plan)$etapes$exposition$resultat$exposition
  is.integer(t$segment) && identical(unique(t$segment), c(2L, 10L))
})

# Nombre de cellules en integer (#37) : diagnostics$cellules de l'étape est un
# integer égal au nombre de lignes de la table (sans et avec par_annee, table
# vide, plusieurs segments) ; nombre_cellules() refuse au-delà de
# .Machine$integer.max sans avertissement de débordement entier.
verifier("diagnostics$cellules de l'étape exposition : integer égal à nrow(table) (H1 sans par_annee, H13 par_annee, H10a table vide 0L)", {
  v <- function(x) x$objet$etapes$exposition$diagnostics$cellules
  is.integer(v(H1)) && identical(v(H1), nrow(H1$table)) &&
    is.integer(v(H13)) && identical(v(H13), nrow(H13$table)) && v(H13) > v(H1) &&
    identical(v(H10a), 0L)
})
verifier("diagnostics$cellules de l'étape exposition : integer avec deux segments (segment 2 et 10)", {
  d <- data.frame(n = D(c("1960-07-01", "1945-10-15")), e = D(c("2020-03-01", "2016-05-20")),
                  s = D(c(NA, "2019-02-10")), c = c("en_cours", "deces"), sg = c(10L, 2L),
                  stringsAsFactors = FALSE)
  roles <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", segment = "sg")
  plan <- list(transition = VD, periode = PER, delai_declaration = 0, segmentation = "segment",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  x <- construire_table(d, roles, plan)$etapes$exposition
  is.integer(x$diagnostics$cellules) && identical(x$diagnostics$cellules, nrow(x$resultat$exposition)) &&
    x$diagnostics$cellules > 1L
})
nombre_cellules <- tablexp:::nombre_cellules
verifier("nombre_cellules : somme des plages d'âges par le nombre d'années, en integer ; 0L pour aucune plage", {
  identical(nombre_cellules(c(5, 0, 3), 2L), 16L) && identical(nombre_cellules(c(5L, 0L, 3L), 1L), 8L) &&
    identical(nombre_cellules(numeric(0), 1L), 0L)
})
verifier("nombre_cellules : .Machine$integer.max admis ; un de plus -> refus trop_de_cellules, sans avertissement de débordement entier", {
  lim <- .Machine$integer.max
  avert <- character(0)
  r <- withCallingHandlers(
    list(nombre_cellules(lim, 1L), leve_refus(nombre_cellules(c(lim, 1), 1L), "trop_de_cellules"),
         leve_refus(nombre_cellules(c(lim, lim), 3L), "trop_de_cellules")),
    warning = function(w) { avert <<- c(avert, conditionMessage(w)); invokeRestart("muffleWarning") })
  identical(r[[1L]], lim) && isTRUE(r[[2L]]) && isTRUE(r[[3L]]) && !length(avert)
})
