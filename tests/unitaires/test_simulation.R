# -----------------------------------------------------------------------------
#  tests/unitaires/test_simulation.R : module `simulation`, simulateur minimal
#  (issue #10 ; E2.19, E2.20 ; ADR 0007, points 3.2, 7, 8 et 11.4 ;
#  spécification d'actuary, A.0 et A.2)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : q_x de la spécification (A.2), dates
#    d'anniversaire (T2), identités exactes de cohérence des lignes ;
#  - recoupement : appel direct d'une fonction de `stats` ou de `base`
#    (integrate, as.Date), dont le moteur ne se sert pas pour la même
#    quantité ;
#  - constat de simulation (E2.20), section finale : n, alpha et
#    probabilité de fausse alarme écrits.
#  Trois erreurs à ne pas confondre dans le constat : (i) le **constat de
#  simulation** est un test d'hypothèse dont la fausse alarme est la
#  probabilité, sur les graines, qu'une implémentation juste échoue ; (ii)
#  l'**erreur Monte-Carlo** est la dispersion binomiale du décompte autour de
#  n p, due au nombre fini de têtes simulées (ici n = 10^6 réplications de
#  l'épreuve de Bernoulli « événement dans l'année x ») ; c'est elle que
#  mesure la largeur de la région d'acceptation ; (iii) l'**erreur
#  d'estimation** (variance d'un taux brut rapporté à une exposition) n'entre
#  pas ici : le décompte est comparé à la probabilité exacte du modèle
#  simulé, aucune quantité n'est estimée sur une exposition (elle viendra
#  avec `exposition` et `brut`).
# -----------------------------------------------------------------------------

simuler <- tablexp::simuler_portefeuille
table_gm <- tablexp:::table_gompertz_makeham
table_constante <- tablexp:::table_constante
loi_effective <- tablexp:::loi_effective
jours_civils <- tablexp:::jours_civils
jour_anniversaire <- tablexp:::jour_anniversaire
age_anniversaire <- tablexp:::age_anniversaire
composantes_date <- tablexp:::composantes_date
age_revolu <- tablexp:::age_revolu
preparer_donnees <- tablexp:::preparer_donnees
GENERATEUR <- tablexp:::GENERATEUR
D <- as.Date

# Loi vraie de la spécification (A.2).
GM_A <- 2e-4; GM_B <- 2.5e-5; GM_C <- 1.1
GM <- table_gm(GM_A, GM_B, GM_C, 0:120)
RACHAT <- table_constante(0.05, 0:120, "rachat constant 5 %")
NUL <- table_constante(0, 0:120, "transition absente (q = 0)")
loi <- function(deces = GM, rachat = RACHAT, f_deces = c(H = 1, F = 0.6), f_rachat = c(H = 1, F = 1))
  list(deces = list(table = deces, facteurs = f_deces), rachat = list(table = rachat, facteurs = f_rachat))
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
PF <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
           origine = D("2005-01-01"))
GRAINE <- 20261016L

# --- tables synthétiques ---------------------------------------------------------

verifier("Gompertz-Makeham (valeur calculée à la main, spécification A.2) : q40, q60, q80, q100, q120 à 12 décimales",
         proche(GM$qx[match(c(40, 60, 80, 100, 120), GM$age)],
                c(0.001386195256, 0.008153179142, 0.052501417592, 0.303486068312, 0.912136646914),
                rel = 0, abs = 5e-13))
verifier("Gompertz-Makeham (recoupement : stats::integrate de mu sur [x, x+1), le moteur emploie la forme close) : 1e-12 relatif",
         proche(GM$qx, vapply(GM$age, function(x)
           1 - exp(-stats::integrate(function(a) GM_A + GM_B * GM_C^a, x, x + 1, rel.tol = 1e-13)$value),
           numeric(1L)), rel = 1e-12, abs = 0))
verifier("Gompertz-Makeham : classe table_reference, statut synthetique, sexe ensemble, attribut parametres (loi, A, B, c, âges)",
         inherits(GM, "table_reference") && identical(attr(GM, "statut"), "synthetique") &&
           all(GM$sexe == "ensemble") && identical(GM$age, 0:120) &&
           identical(attr(GM, "parametres"),
                     list(loi = "gompertz_makeham", A = GM_A, B = GM_B, c = GM_C, ages = c(0L, 120L))))
verifier("table constante : q constant, statut synthetique, parametres (loi, q, âges)",
         all(RACHAT$qx == 0.05) && identical(attr(RACHAT, "parametres"),
                                              list(loi = "constante", q = 0.05, ages = c(0L, 120L))))
verifier("Gompertz-Makeham : c <= 1 refusé (loi_invalide)", leve_refus(table_gm(GM_A, GM_B, 1, 0:120), "loi_invalide"))
verifier("table constante : q > 1 refusé (loi_invalide)", leve_refus(table_constante(1.5, 0:10, "x"), "loi_invalide"))

# --- loi vraie effective -----------------------------------------------------------

le <- loi_effective(list(table = GM, facteurs = c(H = 1, F = 0.6)), "deces", c("H", "F"))
verifier("loi effective : q_x^F = 0,6 q_x, q_x^H = q_x, mu = -ln(1 - q) (valeur calculée à la main)",
         identical(le$qx[le$sexe == "H"], GM$qx) &&
           identical(le$qx[le$sexe == "F"], 0.6 * GM$qx) &&
           isTRUE(proche(le$mu, -log(1 - le$qx), rel = 1e-12, abs = 0)))
le2 <- loi_effective(list(table = table_constante(0.6, 0:10, "x"), facteurs = c(H = 2, F = 1)), "rachat", c("H", "F"))
verifier("loi effective : plafond 1 (2 x 0,6 -> 1, force infinie)",
         all(le2$qx[le2$sexe == "H"] == 1) && all(is.infinite(le2$mu[le2$sexe == "H"])) &&
           all(le2$qx[le2$sexe == "F"] == 0.6))
HF <- table_reference(data.frame(age = rep(0:120, 2), sexe = rep(c("H", "F"), each = 121),
                                 qx = c(GM$qx, GM$qx / 2)),
                      nom = "HF", statut = "synthetique", source = "essai", parametres = list(loi = "essai"))
le3 <- loi_effective(list(table = HF, facteurs = c(H = 1, F = 1)), "deces", c("H", "F"))
verifier("loi effective : table par sexe (H et F) lue sexe par sexe",
         identical(le3$qx[le3$sexe == "F"], GM$qx / 2) && identical(le3$qx[le3$sexe == "H"], GM$qx))

# --- dates (T2) ----------------------------------------------------------------------

jj <- seq(D("1850-01-01"), D("2150-12-31"), by = "day")
lt <- as.POSIXlt(jj)
verifier("jours_civils (recoupement : as.Date, chaque jour de 1850 à 2150)",
         identical(jours_civils(lt$year + 1900L, lt$mon + 1L, lt$mday), as.numeric(jj)))
cn29 <- composantes_date(as.numeric(D("1964-02-29")))
verifier("anniversaires de 1964-02-29 (valeur calculée à la main, T2) : 2020-02-29, 2021-03-01, 2022-03-01, 2023-03-01, 2024-02-29",
         identical(jour_anniversaire(cn29, 56:60),
                   as.numeric(D(c("2020-02-29", "2021-03-01", "2022-03-01", "2023-03-01", "2024-02-29")))))
verifier("age_anniversaire (comparée à age_revolu() du module donnees) : 10^5 couples dont 1 000 naissances au 29/02", {
  set.seed(1L)
  n <- c(sample(as.numeric(D("1900-01-01")):as.numeric(D("2000-12-31")), 99000, replace = TRUE),
         rep(as.numeric(D(c("1964-02-29", "1996-02-29"))), 500))
  t <- n + sample.int(40000, length(n), replace = TRUE) - 1
  identical(as.integer(age_anniversaire(composantes_date(n), composantes_date(t)$an, t)),
            as.integer(age_revolu(n, t)))
})
verifier("âge au dernier anniversaire d'une naissance au 29/02 : 56 au 2021-02-28, 57 au 2021-03-01, 60 au 2024-02-29",
         identical(as.integer(age_anniversaire(cn29, c(2021L, 2021L, 2024L),
                                               as.numeric(D(c("2021-02-28", "2021-03-01", "2024-02-29"))))),
                   c(56L, 57L, 60L)))

# --- portefeuille de la spécification (n = 10^5) ----------------------------------------

s <- simuler(loi(), PF, PER, GRAINE)
info <- attr(s, "simulation")
verifier("sortie : colonnes id_contrat, id_tete, sexe, date_naissance, date_entree, date_sortie, cause_sortie ; 10^5 lignes",
         identical(names(s), c("id_contrat", "id_tete", "sexe", "date_naissance", "date_entree",
                               "date_sortie", "cause_sortie")) && nrow(s) == 1e5)
verifier("sortie : types (entiers, chaînes, Date) ; id_tete = id_contrat, uniques",
         is.integer(s$id_contrat) && identical(s$id_tete, s$id_contrat) && !anyDuplicated(s$id_contrat) &&
           is.character(s$sexe) && is.character(s$cause_sortie) &&
           all(vapply(s[c("date_naissance", "date_entree", "date_sortie")], function(x)
             identical(class(x), "Date"), logical(1L))))
verifier("cohérence exacte : naissance < entrée <= sortie <= fin (lignes sorties)", {
  so <- !is.na(s$date_sortie)
  all(s$date_naissance < s$date_entree) && all(s$date_entree[so] <= s$date_sortie[so]) &&
    all(s$date_sortie[so] <= PER$fin)
})
verifier("cohérence exacte : entrée dans [origine, fin] ; sortie NA si et seulement si en_cours",
         all(s$date_entree >= PF$origine & s$date_entree <= PER$fin) &&
           identical(is.na(s$date_sortie), s$cause_sortie == "en_cours"))
verifier("cohérence exacte : cause dans {deces, rachat, en_cours}, sexe dans {H, F}",
         all(s$cause_sortie %in% c("deces", "rachat", "en_cours")) && all(s$sexe %in% c("H", "F")))
verifier("cohérence exacte : dates entières finies (classe Date, aucun jour fractionnaire)",
         all(vapply(s[c("date_naissance", "date_entree", "date_sortie")], function(x) {
           v <- unclass(x); v <- v[!is.na(v)]; all(is.finite(v) & v == trunc(v))
         }, logical(1L))))
verifier("cohérence exacte : âge au dernier anniversaire à l'entrée dans [20, 75] (âge tiré dans [20, 75), naissance arrondie au jour ; y_min(20) = 20)", {
  a <- age_revolu(s$date_naissance, s$date_entree)
  min(a) >= 20L && max(a) <= 75L
})
verifier("troncature à gauche exercée : entrées avant le début, sorties avant le début conservées et décomptées", {
  av <- !is.na(s$date_sortie) & s$date_sortie < PER$debut
  info$decomptes$sorties_avant_debut == sum(av) && sum(av) > 0L &&
    info$decomptes$entrees_avant_debut == sum(s$date_entree < PER$debut)
})
verifier("décomptes consignés = décomptes recomptés (décès, rachats, en cours, lignes)",
         info$decomptes$deces == sum(s$cause_sortie == "deces") &&
           info$decomptes$rachats == sum(s$cause_sortie == "rachat") &&
           info$decomptes$en_cours == sum(s$cause_sortie == "en_cours") &&
           info$decomptes$lignes == 1e5)
verifier("attribut simulation : graine, générateur, hypothèse de répartition, version de R, paramètres reçus",
         identical(info$graine, GRAINE) && identical(info$generateur, GENERATEUR) &&
           identical(info$loi_vraie$hypothese_repartition,
                     "force constante par âge entier ; risques concurrents indépendants") &&
           identical(info$version_r, R.version.string) && identical(info$portefeuille, PF) &&
           identical(info$periode, PER) && length(info$ordre_tirages) == 5L)
verifier("attribut simulation : loi vraie effective par transition et par sexe (q^F décès = 0,6 q), tables et facteurs",
         identical(sort(unique(paste(info$loi_vraie$effective$cause, info$loi_vraie$effective$sexe))),
                   c("deces F", "deces H", "rachat F", "rachat H")) &&
           identical(info$loi_vraie$effective$qx[info$loi_vraie$effective$cause == "deces" &
                                                   info$loi_vraie$effective$sexe == "F"], 0.6 * GM$qx) &&
           identical(info$loi_vraie$tables$deces, GM) &&
           identical(info$loi_vraie$facteurs$deces, c(H = 1, F = 0.6)) &&
           identical(info$loi_vraie$sexes_controles, c("H", "F")))

# --- compatibilité avec le module donnees (#9) -------------------------------------------

ROLES_SIM <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe",
               date_naissance = "date_naissance", date_entree = "date_entree",
               date_sortie = "date_sortie", cause_sortie = "cause_sortie")
for (tr in list(list(depart = "vivant", arrivee = "deces"), list(depart = "en_cours", arrivee = "rachat"))) {
  r <- preparer_donnees(s, ROLES_SIM, tr, PER, 0, "sexe", character(0))
  verifier(sprintf("module donnees (%s -> %s) : aucun motif, aucune exclusion, lignes conservées identical()",
                   tr$depart, tr$arrivee),
           all(r$diagnostics$motifs$nombre == 0L) && r$resultat$nombre_lignes_exclues == 0L &&
             identical(r$resultat$lignes_conservees, s))
}

# --- reproductibilité et état de l'aléa (ADR 0007, point 8) ---------------------------------

verifier("même graine : résultats identical() (données et attribut)",
         identical(s, simuler(loi(), PF, PER, GRAINE)))
verifier("graine différente : résultats différents",
         !identical(s[1:7], simuler(loi(), PF, PER, GRAINE + 1L)[1:7]))
set.seed(7L, kind = "L'Ecuyer-CMRG"); invisible(runif(3))
avant <- globalenv()$.Random.seed; reglage <- RNGkind()
s_bis <- simuler(loi(), PF, PER, GRAINE)
verifier("appelant sous L'Ecuyer-CMRG : .Random.seed global et RNGkind() inchangés",
         identical(globalenv()$.Random.seed, avant) && identical(RNGkind(), reglage))
verifier("appelant sous L'Ecuyer-CMRG : résultat identical() (l'état de l'appelant n'influence pas)",
         identical(s, s_bis))
RNGkind("default", "default", "default")
if (exists(".Random.seed", globalenv())) rm(".Random.seed", envir = globalenv())
invisible(simuler(loi(), PF, PER, GRAINE))
verifier("appelant sans .Random.seed : absence restaurée",
         !exists(".Random.seed", globalenv(), inherits = FALSE))
set.seed(3L); avant <- globalenv()$.Random.seed
r <- tryCatch(simuler(loi(deces = table_constante(0.01, 0:90, "courte")), PF, PER, GRAINE),
              refus_tablexp = function(e) e)
verifier("refus (table trop courte) : .Random.seed global inchangé",
         inherits(r, "refus_tablexp") && identical(globalenv()$.Random.seed, avant))

# --- mode âge exact ------------------------------------------------------------------------

E <- D("2016-03-15")
PF_X <- list(mode = "age_exact", n = 1000, proportion_hommes = 0.5, age = 60, date_entree = E)
sx <- simuler(loi(), PF_X, list(debut = E, fin = D("2017-12-31")), GRAINE)
verifier("mode âge exact : entrée = E, naissance = E - 60 ans (1956-03-15), âge au dernier anniversaire 60 à l'entrée",
         all(sx$date_entree == E) && all(sx$date_naissance == D("1956-03-15")) &&
           all(age_revolu(sx$date_naissance, sx$date_entree) == 60L))
verifier("mode âge exact : entrée le 29/02 refusée (entree_29_fevrier)",
         leve_refus(simuler(loi(), modifyList(PF_X, list(date_entree = D("2016-02-29"))),
                            list(debut = D("2016-01-01"), fin = D("2017-12-31")), GRAINE),
                    "entree_29_fevrier"))

# --- refus ----------------------------------------------------------------------------------

verifier("graine absente : refus graine_invalide (aucune graine par défaut)",
         leve_refus(simuler(loi(), PF, PER), "graine_invalide"))
verifier("graine non entière : refus graine_invalide", leve_refus(simuler(loi(), PF, PER, 1.5), "graine_invalide"))
verifier("loi sans rachat : refus loi_invalide", leve_refus(simuler(loi()["deces"], PF, PER, GRAINE), "loi_invalide"))
verifier("facteur négatif : refus loi_invalide",
         leve_refus(simuler(loi(f_deces = c(H = 1, F = -1)), PF, PER, GRAINE), "loi_invalide"))
verifier("table qui n'est pas une table_reference : refus loi_invalide",
         leve_refus(simuler(loi(deces = as.data.frame(GM)), PF, PER, GRAINE), "loi_invalide"))
verifier("portefeuille : champ manquant (origine) refusé, sans défaut implicite",
         leve_refus(simuler(loi(), PF[setdiff(names(PF), "origine")], PER, GRAINE), "portefeuille_invalide"))
verifier("portefeuille : champ inconnu refusé",
         leve_refus(simuler(loi(), c(PF, list(age_moyen = 40)), PER, GRAINE), "portefeuille_invalide"))
verifier("portefeuille : origine postérieure au début refusée",
         leve_refus(simuler(loi(), modifyList(PF, list(origine = D("2015-01-02"))), PER, GRAINE),
                    "portefeuille_invalide"))
verifier("portefeuille : age_min >= age_max refusé",
         leve_refus(simuler(loi(), modifyList(PF, list(age_min = 75)), PER, GRAINE), "portefeuille_invalide"))
verifier("portefeuille : mode inconnu refusé",
         leve_refus(simuler(loi(), modifyList(PF, list(mode = "agregats")), PER, GRAINE), "portefeuille_invalide"))
# Forme exacte du mode (#60) : chaîne unique sans nom ni attribut ; champ lu
# par [[ ]], jamais par $ (appariement partiel). Le message distingue le refus
# du mode du refus des champs (un mode lu par $ passerait au contrôle suivant).
message_refus <- function(expr)
  tryCatch({ force(expr); NA_character_ }, refus_tablexp = function(c) conditionMessage(c))
MODES_MAL_FORMES <- list(
  list("mode nommé", c(m = "uniforme")),
  list("mode à attribut", structure("uniforme", foo = "x")),
  list("mode facteur", factor("uniforme")),
  list("mode NA", NA_character_),
  list("mode vide", ""),
  list("mode de longueur 2", c("uniforme", "uniforme")))
for (k in MODES_MAL_FORMES)
  verifier(sprintf("portefeuille : %s refusé (portefeuille_invalide), message du mode", k[[1]]),
           leve_refus(simuler(loi(), modifyList(PF, list(mode = k[[2]])), PER, GRAINE), "portefeuille_invalide") &&
             grepl("avec un champ mode", message_refus(simuler(loi(), modifyList(PF, list(mode = k[[2]])), PER, GRAINE)),
                   fixed = TRUE))
PF_PARTIEL <- PF; names(PF_PARTIEL)[names(PF_PARTIEL) == "mode"] <- "mode_x"
verifier("portefeuille : champ « mode_x » non apparié à « mode » (lecture par [[ ]]), message du mode",
         leve_refus(simuler(loi(), PF_PARTIEL, PER, GRAINE), "portefeuille_invalide") &&
           grepl("avec un champ mode", message_refus(simuler(loi(), PF_PARTIEL, PER, GRAINE)), fixed = TRUE))
verifier("période : début postérieur à la fin refusé (periode_invalide)",
         leve_refus(simuler(loi(), PF, list(debut = PER$fin, fin = PER$debut), GRAINE), "periode_invalide"))
verifier("période : dates non Date refusées (periode_invalide)",
         leve_refus(simuler(loi(), PF, list(debut = "2015-01-01", fin = "2024-12-31"), GRAINE), "periode_invalide"))
verifier("table trop courte (0 à 90 ; âge atteignable 75 + 20 + 1 = 96) : refus table_trop_courte, sans extrapolation",
         leve_refus(simuler(loi(deces = table_constante(0.01, 0:90, "courte")), PF, PER, GRAINE),
                    "table_trop_courte"))
verifier("q_x = 1 à un âge atteignable : refus qx_egal_a_un (force non définie)",
         leve_refus(simuler(loi(rachat = table_constante(0.5, 0:120, "x"), f_rachat = c(H = 2, F = 1)),
                            PF, PER, GRAINE), "qx_egal_a_un"))
verifier("q_x = 1 pour un sexe de proportion nulle : admis (sexe non tiré)",
         is.data.frame(simuler(loi(rachat = table_constante(0.5, 0:120, "x"), f_rachat = c(H = 2, F = 1)),
                               modifyList(PF, list(n = 100, proportion_hommes = 0)), PER, GRAINE)))
T_H <- table_reference(data.frame(age = 0:120, sexe = "H", qx = GM$qx), nom = "H seul",
                       statut = "synthetique", source = "essai", parametres = list(loi = "essai"))
verifier("table du seul sexe H avec proportion_hommes = 1 : admise, loi construite et contrôlée pour H seul (sexes_controles)", {
  r <- simuler(loi(deces = T_H, rachat = T_H), modifyList(PF, list(n = 100, proportion_hommes = 1)), PER, GRAINE)
  a <- attr(r, "simulation")$loi_vraie
  all(r$sexe == "H") && identical(a$sexes_controles, "H") && all(a$effective$sexe == "H")
})
verifier("table du seul sexe H avec proportion_hommes = 0,5 : refus loi_invalide (sexe F tiré, sans table)",
         leve_refus(simuler(loi(deces = T_H), modifyList(PF, list(n = 100)), PER, GRAINE), "loi_invalide"))

# Borne basse exacte de l'âge à l'entrée (reprise C1 de #10, décision
# d'actuary) : y_min = max{y : 365 y + ceiling(y/4) <= round(age_min * 365,25)}.
age_revolu_minimal <- tablexp:::age_revolu_minimal
verifier("y_min (valeur calculée à la main) : y_min(20) = 20 (7 305 jours), y_min(21) = 20 (7 670 < 7 671 jours), y_min(1) = 0 (365 < 366)",
         identical(c(age_revolu_minimal(round(20 * 365.25)), age_revolu_minimal(round(21 * 365.25)),
                     age_revolu_minimal(round(1 * 365.25))), c(20, 20, 0)))
for (cas in list(list(age_min = 21, age_max = 75, ages = 21:120),
                 list(age_min = 1, age_max = 10, ages = 1:120))) {
  verifier(sprintf(paste("table %d:%d, age_min = %g : refus table_trop_courte pour chacune des graines",
                         "1 à 20 (n = 1), indépendant de la graine"), min(cas$ages), max(cas$ages), cas$age_min),
           all(vapply(1:20, function(g) isTRUE(leve_refus(
             simuler(loi(deces = table_constante(0.01, cas$ages, "courte")),
                     modifyList(PF, list(n = 1, age_min = cas$age_min, age_max = cas$age_max)), PER, g),
             "table_trop_courte")), logical(1L))))
}

# --- constat de simulation (E2.20 ; spécification A.2) -------------------------------------------
#
# Cinq cohortes (graines distinctes GRAINE + 0 à 4) de n = 10^6 têtes
# entrées à l'âge exact x le 2016-03-15 (anniversaire), période du
# 2016-03-15 au 2017-12-31 : l'année x entière est observée, sans censure
# par la période. Décompte D des événements datés avant anniv(x+1) =
# 2017-03-15. Sous le modèle simulé, D ~ Binomiale(n, p), p exact.
# Région d'acceptation bilatérale exacte [qbinom(a_k/2), qbinom(1 - a_k/2)] ;
# Bonferroni, alpha global 10^-3, K = 5, a_k = 2.10^-4. Fausse alarme exacte
# de chaque test FA_k = P(D < lo) + P(D > hi). Fausse alarme familiale :
# borne de Bonferroni sum(FA_k) <= alpha, vérifiée, qui ne suppose aucune
# indépendance ; la valeur 1 - prod(1 - FA_k) est donnée pour information,
# valable sous indépendance des cohortes (hypothèse usuelle sur le
# générateur, graines distinctes), non démontrée. La graine est fixe : le
# test est déterministe, sa fausse alarme est la probabilité, sur les
# graines, qu'une implémentation juste échoue. Chaque ligne du constat est
# écrite (cat) hors du mode verbeux.

N_CONSTAT <- 1e6; ALPHA <- 1e-3; K <- 5L; ALPHA_K <- ALPHA / K
PER_X <- list(debut = E, fin = D("2017-12-31"))
FIN_ANNEE <- D("2017-03-15")
q <- function(x) GM$qx[GM$age == x]
mu_d <- -log(1 - q(60)); mu_r <- -log(1 - 0.30)
CONSTAT <- list(
  list(nom = "1. décès, x = 60, H, sans rachat", x = 60, pH = 1, loi = loi(rachat = NUL),
       cause = "deces", p = q(60), region_spec = c(7821, 8490)),
  list(nom = "2. décès, x = 80, F (facteur 0,6), sans rachat", x = 80, pH = 0, loi = loi(rachat = NUL),
       cause = "deces", p = 0.6 * q(80), region_spec = c(30853, 32152)),
  list(nom = "3. décès, x = 100, F, sans rachat", x = 100, pH = 0, loi = loi(rachat = NUL),
       cause = "deces", p = 0.6 * q(100), region_spec = c(180658, 183528)),
  list(nom = "4. rachat, x = 40, q^r = 0,05, sans décès", x = 40, pH = 0.5, loi = loi(deces = NUL),
       cause = "rachat", p = 0.05, region_spec = c(49191, 50812)),
  list(nom = "5. décès, x = 60, H, rachat concurrent q^r = 0,30", x = 60, pH = 1,
       loi = loi(rachat = table_constante(0.30, 0:120, "rachat constant 30 %")),
       cause = "deces", p = mu_d / (mu_d + mu_r) * (1 - exp(-(mu_d + mu_r))), region_spec = c(6554, 7168)))

verifier("constat : p des tests 2, 3 et 5 conformes à la spécification (0,03150085 ; 0,18209164 ; 0,0068593203)",
         proche(vapply(CONSTAT[c(2, 3, 5)], `[[`, numeric(1L), "p"),
                c(0.03150085, 0.18209164, 0.0068593203), rel = 0, abs = 6e-9))
fa <- numeric(K)
for (k in seq_len(K)) {
  ct <- CONSTAT[[k]]
  lo <- stats::qbinom(ALPHA_K / 2, N_CONSTAT, ct$p)
  hi <- stats::qbinom(1 - ALPHA_K / 2, N_CONSTAT, ct$p)
  fa[k] <- stats::pbinom(lo - 1, N_CONSTAT, ct$p) +
    stats::pbinom(hi, N_CONSTAT, ct$p, lower.tail = FALSE)
  verifier(sprintf("constat %d : région [%d, %d] = région de la spécification", k, lo, hi),
           identical(c(lo, hi), ct$region_spec))
  t0 <- proc.time()[["elapsed"]]
  sc <- simuler(ct$loi, list(mode = "age_exact", n = N_CONSTAT, proportion_hommes = ct$pH,
                             age = ct$x, date_entree = E), PER_X, GRAINE + k - 1L)
  duree <- proc.time()[["elapsed"]] - t0
  d <- sum(sc$cause_sortie == ct$cause & sc$date_sortie < FIN_ANNEE, na.rm = TRUE)
  ligne <- sprintf(paste("constat de simulation %s : D = %d dans [%d, %d] (n = %g, p = %.10g,",
                         "alpha_k = %g, fausse alarme exacte FA_k = %.4g ; graine %d ; %.1f s)"),
                   ct$nom, d, lo, hi, N_CONSTAT, ct$p, ALPHA_K, fa[k], GRAINE + k - 1L, duree)
  cat("  ", ligne, "\n", sep = "")
  verifier(ligne, d >= lo && d <= hi)
}
ligne <- sprintf(paste("constat : fausse alarme familiale <= sum(FA_k) = %.4g <= alpha global %g",
                       "(borne de Bonferroni, K = %d, sans hypothèse d'indépendance) ;",
                       "1 - prod(1 - FA_k) = %.4g sous indépendance des cohortes (hypothèse usuelle",
                       "sur le générateur)"), sum(fa), ALPHA, K, 1 - prod(1 - fa))
cat("  ", ligne, "\n", sep = "")
verifier(ligne, sum(fa) <= ALPHA)

# --- forme exacte des listes contrôlées (#44) -------------------------------------
# sort() retire un nom NA : un élément de plus nommé NA passait les contrôles de
# loi, loi_t et periode, qui ne vérifiaient pas la longueur ; liste_aux_noms()
# la contrôle, et refuse aussi une liste à classe ou à attribut. Avec "x", le
# refus nommé remplace l'erreur R brute « subscript out of bounds » (loi).
PF_FORME <- modifyList(PF, list(n = 1000))
plus_nom_na <- function(l, v) { l <- c(l, list(v)); names(l)[length(l)] <- NA; l }
LISTES_FORME <- list(
  list("loi : élément de plus nommé NA, loi valide", "loi_invalide",
       function() simuler(plus_nom_na(loi(), loi()$deces), PF_FORME, PER, GRAINE)),
  list("loi : élément de plus nommé NA valant \"x\" (plus d'erreur R brute)", "loi_invalide",
       function() simuler(plus_nom_na(loi(), "x"), PF_FORME, PER, GRAINE)),
  list("loi : liste à classe", "loi_invalide",
       function() { l <- loi(); class(l) <- "x"; simuler(l, PF_FORME, PER, GRAINE) }),
  list("loi_t : élément de plus nommé NA, valide", "loi_invalide",
       function() simuler(list(deces = plus_nom_na(loi()$deces, 1), rachat = loi()$rachat), PF_FORME, PER, GRAINE)),
  list("loi_t : élément de plus nommé NA valant \"x\"", "loi_invalide",
       function() simuler(list(deces = plus_nom_na(loi()$deces, "x"), rachat = loi()$rachat), PF_FORME, PER, GRAINE)),
  list("loi_t : liste à attribut quelconque", "loi_invalide",
       function() { l <- loi(); attr(l$deces, "foo") <- "x"; simuler(l, PF_FORME, PER, GRAINE) }),
  list("facteurs : élément de plus nommé NA (déjà refusé par la garde de longueur)", "loi_invalide",
       function() { f <- c(H = 1, F = 0.6, 1); names(f)[3L] <- NA; simuler(loi(f_deces = f), PF_FORME, PER, GRAINE) }),
  list("periode : élément de plus nommé NA, date valide", "periode_invalide",
       function() simuler(loi(), PF_FORME, plus_nom_na(PER, D("2020-01-01")), GRAINE)),
  list("periode : élément de plus nommé NA valant \"x\"", "periode_invalide",
       function() simuler(loi(), PF_FORME, plus_nom_na(PER, "x"), GRAINE)),
  list("periode : data.frame (debut, fin)", "periode_invalide",
       function() simuler(loi(), PF_FORME, data.frame(debut = PER$debut, fin = PER$fin), GRAINE)),
  list("periode : liste à classe", "periode_invalide",
       function() { p <- PER; class(p) <- "x"; simuler(loi(), PF_FORME, p, GRAINE) }))
for (k in LISTES_FORME)
  verifier(sprintf("forme exacte : %s -> refus %s", k[[1]], k[[2]]), leve_refus(k[[3]](), k[[2]]))

# --- forme exacte des bornes de la période et des facteurs (#58) -------------------
# Bornes : la forme de date1() de controler_transition_periode() (donnees) :
# double ou integer, seul attribut la classe "Date" exacte, sous periode_invalide.
# Facteurs : forme_exacte(f, c("double", "integer"), "names"), noms H et F
# comparés après un tri radix, sous loi_invalide. Les formes admises restent
# admises ; le jeu tiré ne dépend ni du stockage des bornes ni de celui des
# facteurs (les tirages sont identiques, seul l'attribut `simulation` consigne
# la forme reçue).
nommee <- function(d) structure(c(a = unclass(d)), class = "Date")
attribuee <- function(d) { attr(d, "foo") <- "x"; d }
classe_plus <- function(d) structure(unclass(d), class = c("Date", "x"))
vers_integer <- function(d) structure(as.integer(unclass(d)), class = "Date")
avec_attr <- function(f, nom, v) { attr(f, nom) <- v; f }
MATRICE_1x2 <- function(f) structure(unclass(f), dim = c(1L, 2L))
PERIODE_FORME <- list(
  list("debut nommée (Date nommée)", function() list(debut = nommee(PER$debut), fin = PER$fin)),
  list("fin nommée", function() list(debut = PER$debut, fin = nommee(PER$fin))),
  list("debut à attribut quelconque", function() list(debut = attribuee(PER$debut), fin = PER$fin)),
  list("fin à attribut quelconque", function() list(debut = PER$debut, fin = attribuee(PER$fin))),
  list("debut de classe c(\"Date\", \"x\")", function() list(debut = classe_plus(PER$debut), fin = PER$fin)),
  list("debut en caractère de classe Date", function()
    list(debut = structure("2015-01-01", class = "Date"), fin = PER$fin)),
  list("fin non entière (jour fractionnaire)", function()
    list(debut = PER$debut, fin = structure(unclass(PER$fin) + 0.5, class = "Date"))))
for (k in PERIODE_FORME) {
  verifier(sprintf("forme exacte : période, %s -> refus periode_invalide", k[[1]]),
           leve_refus(simuler(loi(), PF_FORME, k[[2]](), GRAINE), "periode_invalide"))
  verifier(sprintf("forme exacte : période, %s -> message de forme", k[[1]]),
           grepl("sans autre attribut", message_refus(simuler(loi(), PF_FORME, k[[2]](), GRAINE)), fixed = TRUE))
}

FACTEURS_FORME <- list(
  list("attribut quelconque", avec_attr(c(H = 1, F = 0.6), "foo", "x")),
  list("classe", structure(c(H = 1, F = 0.6), class = "x")),
  list("matrice 1 x 2 sans noms de dimension", MATRICE_1x2(c(H = 1, F = 0.6))),
  list("sans noms", c(1, 0.6)),
  list("noms en minuscules", c(h = 1, f = 0.6)),
  list("noms H et H", c(H = 1, H = 0.6)),
  list("nom NA", structure(c(1, 0.6), names = c("H", NA))),
  list("caractère", c(H = "1", F = "0.6")),
  list("logique", c(H = TRUE, F = FALSE)),
  list("valeur manquante", c(H = 1, F = NA_real_)),
  list("valeur infinie", c(H = 1, F = Inf)),
  list("trois éléments", c(H = 1, F = 0.6, X = 1)),
  # Octets construits par rawToChar() (chaînes natives non marquées, quelle que
  # soit la locale du test) : é en UTF-8 (non ASCII natif en locale C) et un
  # octet invalide en UTF-8 ; le refus nommé remplace l'erreur de sort(method =
  # "radix") (« Character encoding must be UTF-8, Latin-1 or bytes »).
  list("nom non ASCII natif", structure(c(1, 0.6), names = c(rawToChar(as.raw(c(0xc3, 0xa9))), "H"))),
  list("nom en octets invalides", structure(c(1, 0.6), names = c(rawToChar(as.raw(0xc9)), "H"))))
for (k in FACTEURS_FORME) {
  verifier(sprintf("forme exacte : facteurs, %s -> refus loi_invalide", k[[1]]),
           leve_refus(simuler(loi(f_deces = k[[2]]), PF_FORME, PER, GRAINE), "loi_invalide"))
  verifier(sprintf("forme exacte : facteurs de rachat, %s -> refus loi_invalide", k[[1]]),
           leve_refus(simuler(loi(f_rachat = k[[2]]), PF_FORME, PER, GRAINE), "loi_invalide"))
}
verifier("forme exacte : facteurs à attribut -> message de forme (sans autre attribut que les noms)",
         grepl("sans autre attribut que les noms",
               message_refus(simuler(loi(f_deces = avec_attr(c(H = 1, F = 0.6), "foo", "x")), PF_FORME, PER, GRAINE)),
               fixed = TRUE))

# Formes admises : mêmes tirages, seul l'attribut `simulation` diffère.
sans_attribut_simulation <- function(x) { attr(x, "simulation") <- NULL; x }
REF_FORME <- simuler(loi(), PF_FORME, PER, GRAINE)
verifier("forme exacte : bornes en stockage integer admises, tirages identiques",
         identical(sans_attribut_simulation(
           simuler(loi(), PF_FORME, list(debut = vers_integer(PER$debut), fin = vers_integer(PER$fin)), GRAINE)),
           sans_attribut_simulation(REF_FORME)))
verifier("forme exacte : facteurs integer admis, tirages identiques à ceux des facteurs double",
         identical(sans_attribut_simulation(simuler(loi(f_rachat = c(H = 1L, F = 1L)), PF_FORME, PER, GRAINE)),
                   sans_attribut_simulation(REF_FORME)))
verifier("forme exacte : noms des facteurs permutés c(F, H) admis, tirages identiques",
         identical(sans_attribut_simulation(simuler(loi(f_deces = c(F = 0.6, H = 1)), PF_FORME, PER, GRAINE)),
                   sans_attribut_simulation(REF_FORME)))
verifier("forme exacte : entrée valide, attribut simulation$periode et $loi_vraie$facteurs de forme canonique",
         identical(attr(REF_FORME, "simulation")$periode, PER) &&
           identical(attr(REF_FORME, "simulation")$loi_vraie$facteurs,
                     list(deces = c(H = 1, F = 0.6), rachat = c(H = 1, F = 1))))

# --- forme exacte du portefeuille et de ses champs numériques (#78) ----------------------
# Portefeuille : liste nue (forme_exacte(x, "list", "names")) ; champs
# numériques : double ou integer, longueur 1, aucun attribut ; dates : Date
# seule, comme les bornes de la période (#58) ; noms comparés sans tri
# (setdiff, anyDuplicated). Refus sous le motif existant portefeuille_invalide,
# testé par motif et par message ; les formes admises le restent, avec les
# mêmes tirages. Réserve 2 de l'audit de #58 : origine et date_entree, dont le
# message dit désormais « sans autre attribut ».
PF_AGE <- PF_X
PORTEFEUILLE_FORME <- list(
  list("data.frame (uniforme)", PF_FORME, function(p) as.data.frame(p)),
  list("data.frame (âge exact)", PF_AGE, function(p) as.data.frame(p)),
  list("liste classée (uniforme)", PF_FORME, function(p) structure(p, class = "maclasse")),
  list("liste classée (âge exact)", PF_AGE, function(p) structure(p, class = "maclasse")),
  list("liste de classe c(\"list\", \"x\")", PF_FORME, function(p) structure(p, class = c("list", "x"))),
  list("liste portant un attribut (uniforme)", PF_FORME, function(p) avec_attr(p, "foo", "x")),
  list("liste portant un attribut (âge exact)", PF_AGE, function(p) avec_attr(p, "foo", "x")),
  list("vecteur atomique nommé", PF_FORME, function(p) c(mode = "uniforme", n = "1000")),
  list("NULL", PF_FORME, function(p) NULL))
for (k in PORTEFEUILLE_FORME) {
  pf <- k[[3]](k[[2]]); per <- PER
  verifier(sprintf("forme exacte : portefeuille, %s -> refus portefeuille_invalide", k[[1]]),
           leve_refus(simuler(loi(), pf, per, GRAINE), "portefeuille_invalide"))
  verifier(sprintf("forme exacte : portefeuille, %s -> message de forme", k[[1]]),
           grepl("liste nue attendue", message_refus(simuler(loi(), pf, per, GRAINE)), fixed = TRUE))
}

# Champs numériques : (cas, portefeuille de base, champ, valeur reçue, début du message).
CHAMPS_FORME <- list(
  list("n nommé", PF_FORME, "n", c(a = 1000), "Portefeuille : n entier"),
  list("n nommé en integer", PF_FORME, "n", c(a = 1000L), "Portefeuille : n entier"),
  list("n en matrice 1 x 1", PF_FORME, "n", matrix(1000), "Portefeuille : n entier"),
  list("n à attribut", PF_FORME, "n", structure(1000, foo = "x"), "Portefeuille : n entier"),
  list("n de classe", PF_FORME, "n", structure(1000, class = "x"), "Portefeuille : n entier"),
  list("proportion_hommes à attribut", PF_FORME, "proportion_hommes", structure(0.5, foo = "x"), "Portefeuille : proportion_hommes réel"),
  list("proportion_hommes nommée", PF_FORME, "proportion_hommes", c(a = 0.5), "Portefeuille : proportion_hommes réel"),
  list("age_min nommé", PF_FORME, "age_min", c(a = 20), "Portefeuille : âges réels"),
  list("age_max à attribut", PF_FORME, "age_max", structure(75, foo = "x"), "Portefeuille : âges réels"),
  list("age_max en matrice 1 x 1", PF_FORME, "age_max", matrix(75), "Portefeuille : âges réels"),
  list("age nommé", PF_AGE, "age", c(a = 60), "Portefeuille : âge exact entier"),
  list("age à attribut", PF_AGE, "age", structure(60, foo = "x"), "Portefeuille : âge exact entier"),
  list("age de classe", PF_AGE, "age", structure(60, class = "x"), "Portefeuille : âge exact entier"))
for (k in CHAMPS_FORME) {
  pf <- modifyList(k[[2]], setNames(list(k[[4]]), k[[3]]))
  verifier(sprintf("forme exacte : portefeuille, %s -> refus portefeuille_invalide", k[[1]]),
           leve_refus(simuler(loi(), pf, PER, GRAINE), "portefeuille_invalide"))
  verifier(sprintf("forme exacte : portefeuille, %s -> message du champ, sans attribut", k[[1]]),
           grepl(k[[5]], message_refus(simuler(loi(), pf, PER, GRAINE)), fixed = TRUE) &&
             grepl("sans attribut", message_refus(simuler(loi(), pf, PER, GRAINE)), fixed = TRUE))
}

# Dates du portefeuille (réserve 2 de l'audit de #58) : nommée, à attribut,
# classe ajoutée, caractère de classe Date.
DATES_PF_FORME <- list(
  list("origine nommée", PF_FORME, "origine", nommee(PF_FORME$origine), "origine : date"),
  list("origine à attribut", PF_FORME, "origine", attribuee(PF_FORME$origine), "origine : date"),
  list("origine de classe c(\"Date\", \"x\")", PF_FORME, "origine", classe_plus(PF_FORME$origine), "origine : date"),
  list("origine en caractère de classe Date", PF_FORME, "origine",
       structure("2005-01-01", class = "Date"), "origine : date"),
  list("date_entree nommée", PF_AGE, "date_entree", nommee(PF_AGE$date_entree), "date_entree : date"),
  list("date_entree à attribut", PF_AGE, "date_entree", attribuee(PF_AGE$date_entree), "date_entree : date"),
  list("date_entree de classe c(\"Date\", \"x\")", PF_AGE, "date_entree",
       classe_plus(PF_AGE$date_entree), "date_entree : date"))
for (k in DATES_PF_FORME) {
  pf <- modifyList(k[[2]], setNames(list(k[[4]]), k[[3]]))
  verifier(sprintf("forme exacte : portefeuille, %s -> refus portefeuille_invalide", k[[1]]),
           leve_refus(simuler(loi(), pf, PER, GRAINE), "portefeuille_invalide"))
  verifier(sprintf("forme exacte : portefeuille, %s -> message de forme (sans autre attribut)", k[[1]]),
           grepl(k[[5]], message_refus(simuler(loi(), pf, PER, GRAINE)), fixed = TRUE) &&
             grepl("sans autre attribut", message_refus(simuler(loi(), pf, PER, GRAINE)), fixed = TRUE))
}

# Noms des champs comparés sans tri : un nom de plus non ASCII natif, en octets
# invalides, NA ou doublé est refusé sous portefeuille_invalide (aucune erreur R).
plus_champ <- function(p, nom) { p <- c(p, list(1)); names(p)[length(p)] <- nom; p }
NOMS_PF_FORME <- list(
  list("champ de plus au nom non ASCII natif", plus_champ(PF_FORME, rawToChar(as.raw(c(0xc3, 0xa9))))),
  list("champ de plus au nom en octets invalides", plus_champ(PF_FORME, rawToChar(as.raw(0xc9)))),
  list("champ de plus au nom NA", plus_champ(PF_FORME, NA_character_)),
  list("champ n doublé", plus_champ(PF_FORME, "n")),
  list("champ mode doublé", c(PF_FORME, list(mode = "uniforme"))),
  list("liste sans noms", unname(PF_FORME)))
for (k in NOMS_PF_FORME)
  verifier(sprintf("forme exacte : portefeuille, %s -> refus portefeuille_invalide", k[[1]]),
           leve_refus(simuler(loi(), k[[2]], PER, GRAINE), "portefeuille_invalide"))

# Nom de champ marqué « bytes » (#111) : sa citation dans sprintf() lèverait une erreur R
# (« translating strings with "bytes" encoding is not allowed ») ; en première position
# ou en l'absence de mode, la lecture [["mode"]] lèverait aussi. Le refus
# portefeuille_invalide cite le nom sous un repère. Mode marqué « bytes » : refusé aussi.
NOM_BYTES <- "\xe9t\xe9"; Encoding(NOM_BYTES) <- "bytes"
nom_en <- function(p, i) { names(p)[i] <- NOM_BYTES; p }
PF_BYTES <- list(
  list("champ de plus en dernière position", plus_champ(PF_FORME, NOM_BYTES), TRUE),
  list("champ de plus en première position", nom_en(c(list(1), PF_FORME), 1L), TRUE),
  list("champ de plus en seconde position", nom_en(c(PF_FORME[1], list(1), PF_FORME[-1]), 2L), TRUE),
  list("champ mode renommé (première position, mode absent)", nom_en(PF_FORME, 1L), FALSE),
  list("champ n renommé (seconde position)", nom_en(PF_FORME, 2L), TRUE),
  list("champ en première position, mode âge exact", nom_en(c(list(1), PF_AGE), 1L), TRUE),
  list("mode d'encodage bytes", modifyList(PF_FORME, list(mode = NOM_BYTES)), FALSE))
for (k in PF_BYTES) {
  verifier(sprintf("forme exacte : portefeuille, %s, nom d'encodage bytes -> refus portefeuille_invalide", k[[1]]),
           leve_refus(simuler(loi(), k[[2]], PER, GRAINE), "portefeuille_invalide"))
  if (k[[3]])
    verifier(sprintf("forme exacte : portefeuille, %s, nom d'encodage bytes -> nom remplacé par un repère", k[[1]]),
             grepl("inconnus : « <chaîne d'encodage bytes> »", message_refus(simuler(loi(), k[[2]], PER, GRAINE)), fixed = TRUE))
}

# Formes admises : stockage integer des champs numériques et des dates, ordre
# des champs permuté ; mêmes tirages, seul l'attribut `simulation` diffère.
verifier("forme exacte : champs numériques en integer admis, tirages identiques",
         identical(sans_attribut_simulation(
           simuler(loi(), modifyList(PF_FORME, list(n = 1000L, age_min = 20L, age_max = 75L)), PER, GRAINE)),
           sans_attribut_simulation(REF_FORME)))
verifier("forme exacte : origine en stockage integer admise, tirages identiques",
         identical(sans_attribut_simulation(
           simuler(loi(), modifyList(PF_FORME, list(origine = vers_integer(PF_FORME$origine))), PER, GRAINE)),
           sans_attribut_simulation(REF_FORME)))
verifier("forme exacte : champs du portefeuille permutés admis, tirages identiques",
         identical(sans_attribut_simulation(simuler(loi(), rev(PF_FORME), PER, GRAINE)),
                   sans_attribut_simulation(REF_FORME)))
verifier("forme exacte : portefeuille du mode âge exact, age et date_entree en integer admis, tirages identiques",
         identical(sans_attribut_simulation(
           simuler(loi(), modifyList(PF_AGE, list(age = 60L, date_entree = vers_integer(PF_AGE$date_entree))), PER, GRAINE)),
           sans_attribut_simulation(simuler(loi(), PF_AGE, PER, GRAINE))))
verifier("forme exacte : attribut simulation$portefeuille consigne le portefeuille reçu, liste nue",
         identical(attr(REF_FORME, "simulation")$portefeuille, PF_FORME) &&
           identical(names(attributes(attr(REF_FORME, "simulation")$portefeuille)), "names"))

# --- têtes multi-contrats (#92) -------------------------------------------------------------
# Spécification d'actuary-approfondi du 25/10/2026 (#92, § 1 et § 4) ; ADR
# 0007, annotation du 25/10/2026, (a) à (d). Natures des attendus : littéraux
# des constantes (contrainte (a)) ; identités exactes (décomptes refaits sur
# les lignes, comparés à l'attribut et au moteur) ; constats de simulation,
# famille A (fausse alarme exacte) et famille B (asymptotique). Non-circularité
# (annotation, (d)) : aucune attente n'est établie par une fonction du module
# `donnees` ; ses sorties (motifs, relevés, exposition, décès, par
# construire_table()) sont ce qui est vérifié.

ORDRE_TIRAGES_LU <- tablexp:::ORDRE_TIRAGES
ORDRE_TIRAGES_MULTI <- tablexp:::ORDRE_TIRAGES_MULTI
CHAMPS_PORTEFEUILLE <- tablexp:::CHAMPS_PORTEFEUILLE
verifier("ORDRE_TIRAGES_MULTI égale à son littéral (spécification, § 1)",
         identical(ORDRE_TIRAGES_MULTI,
                   list(uniforme_multi = c(
                          "1. sexe : runif(n) < proportion_hommes",
                          paste("2. nombre K de contrats par tête : 1 + findInterval(runif(n), cumsum(loi_contrats)[-m]) ;",
                                "aucun tirage si loi_contrats n'a qu'une probabilité non nulle"),
                          paste("3. jour d'entrée, par contrat tiré (têtes puis contrats) : sample.int(nombre de jours",
                                "de [origine, fin], N, replace = TRUE)"),
                          "4. âge à la première entrée : age_min + (age_max - age_min) * runif(n)",
                          "5. décès, par tête : rexp(n)",
                          "6. rachat, par contrat tiré (souscrit ou non) : rexp(N)"),
                        age_exact_multi = c(
                          "1. sexe : runif(n) < proportion_hommes",
                          paste("2. nombre K de contrats par tête : 1 + findInterval(runif(n), cumsum(loi_contrats)[-m]) ;",
                                "aucun tirage si loi_contrats n'a qu'une probabilité non nulle"),
                          "3. décès, par tête : rexp(n)",
                          "4. rachat, par contrat tiré (souscrit ou non) : rexp(N)"))))
verifier("contrainte (a) : ORDRE_TIRAGES des modes existants égale à son littéral (37ad4d3)",
         identical(ORDRE_TIRAGES_LU,
                   c("1. sexe : runif(n) < proportion_hommes",
                     "2. jour d'entrée : sample.int(nombre de jours de [origine, fin], n, replace = TRUE) (mode uniforme)",
                     "3. âge à l'entrée : age_min + (age_max - age_min) * runif(n) (mode uniforme)",
                     "4. décès : rexp(n)",
                     "5. rachat : rexp(n)")))
verifier("contrainte (a) : CHAMPS_PORTEFEUILLE des modes existants égaux à leurs littéraux (37ad4d3)",
         identical(CHAMPS_PORTEFEUILLE[c("uniforme", "age_exact")],
                   list(uniforme = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine"),
                        age_exact = c("mode", "n", "proportion_hommes", "age", "date_entree"))))
verifier("modes multi-contrats : champs de la spécification (§ 1.1), tous requis",
         identical(names(CHAMPS_PORTEFEUILLE), c("uniforme", "age_exact", "uniforme_multi", "age_exact_multi")) &&
           identical(CHAMPS_PORTEFEUILLE[c("uniforme_multi", "age_exact_multi")],
                     list(uniforme_multi = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine",
                                             "loi_contrats"),
                          age_exact_multi = c("mode", "n", "proportion_hommes", "age", "date_entree", "loi_contrats"))))
CHAMPS_ATTRIBUT <- c("loi_vraie", "portefeuille", "periode", "graine", "generateur", "ordre_tirages",
                     "version_r", "decomptes")
DECOMPTES_EXISTANTS <- c("lignes", "deces", "rachats", "en_cours", "sorties_avant_debut", "entrees_avant_debut")
DECOMPTES_MULTI <- c("tetes", "contrats_tires", "contrats_non_souscrits", "tetes_decedees_observees",
                     "tetes_decedees_sans_contrat")
verifier("contrainte (a) : attribut simulation des modes uniforme et age_exact sans champ ajouté, ordre_tirages = ORDRE_TIRAGES",
         all(vapply(list(info, attr(sx, "simulation")), function(a)
           identical(names(a), CHAMPS_ATTRIBUT) && identical(names(a$decomptes), DECOMPTES_EXISTANTS) &&
             identical(a$ordre_tirages, ORDRE_TIRAGES_LU), logical(1L))))

# Critère 2 : loi_contrats = 1 (loi dégénérée, aucun tirage de K) : mêmes
# tirages que le mode existant, jeu identical() hors attribut (test exact).
PF_1E4 <- modifyList(PF, list(n = 1e4))
PF_X_1E4 <- modifyList(PF_X, list(n = 1e4))
PER_X2 <- list(debut = E, fin = D("2017-12-31"))
for (k in list(list("uniforme", PF_1E4, PER), list("age_exact", PF_X_1E4, PER_X2))) {
  pm <- c(modifyList(k[[2]], list(mode = paste0(k[[1]], "_multi"))), list(loi_contrats = 1))
  verifier(sprintf("critère 2 : %s_multi avec loi_contrats = 1 identical() au mode %s hors attribut (10^4 têtes)",
                   k[[1]], k[[1]]),
           identical(sans_attribut_simulation(simuler(loi(), pm, k[[3]], GRAINE)),
                     sans_attribut_simulation(simuler(loi(), k[[2]], k[[3]], GRAINE))))
}

# --- jeu de la famille B (spécification, § 1.6) : uniforme_multi, 10^5 têtes ---------------
PF_B <- c(modifyList(PF, list(mode = "uniforme_multi")), list(loi_contrats = c(0.6, 0.3, 0.1)))
t0 <- proc.time()[["elapsed"]]
sb <- simuler(loi(), PF_B, PER, GRAINE)
duree_b <- proc.time()[["elapsed"]] - t0
ib <- attr(sb, "simulation")
nb <- nrow(sb); NT_B <- 1e5
lignes_par_tete <- tabulate(sb$id_tete, nbins = NT_B)

verifier("multi : colonnes et types des modes existants (entiers, chaînes, Date), noms de lignes automatiques",
         identical(names(sb), names(s)) && identical(vapply(sb, typeof, ""), vapply(s, typeof, "")) &&
           identical(lapply(sb, class), lapply(s, class)) && .row_names_info(sb) == -nb)
verifier("multi : id_tete de 1 à n, chaque tête écrite (premier contrat toujours souscrit) ; id_contrat = 1..lignes",
         identical(range(sb$id_tete), c(1L, as.integer(NT_B))) && all(lignes_par_tete >= 1L) &&
           identical(sb$id_contrat, seq_len(nb)))
verifier("multi : lignes ordonnées par (id_tete, date_entree)",
         !is.unsorted(sb$id_tete) &&
           !any(diff(sb$id_tete) == 0L & diff(as.numeric(sb$date_entree)) < 0))
verifier("multi : cohérence exacte, naissance < entrée <= sortie <= fin ; entrée dans [origine, fin] ; sortie NA si et seulement si en_cours", {
  so <- !is.na(sb$date_sortie)
  all(sb$date_naissance < sb$date_entree) && all(sb$date_entree[so] <= sb$date_sortie[so]) &&
    all(sb$date_sortie[so] <= PER$fin) && all(sb$date_entree >= PF$origine & sb$date_entree <= PER$fin) &&
    identical(is.na(sb$date_sortie), sb$cause_sortie == "en_cours")
})
prem_b <- !duplicated(sb$id_tete)
# Âge révolu calculé localement (as.POSIXlt, convention T2 : anniversaire
# d'une naissance au 29/02 le 01/03 des années non bissextiles), sans
# fonction du module donnees.
age_revolu_local <- function(n, t) {
  a <- as.POSIXlt(n); b <- as.POSIXlt(t)
  an_t <- b$year + 1900L
  bis <- (an_t %% 4L == 0L & an_t %% 100L != 0L) | an_t %% 400L == 0L
  cle_n <- 100L * (a$mon + 1L) + a$mday
  cle_n[cle_n == 229L & !bis] <- 301L
  (b$year - a$year) - (cle_n > 100L * (b$mon + 1L) + b$mday)
}
verifier("multi : naissance et sexe constants par tête ; âge révolu à la première entrée dans [20, 75]", {
  i <- match(sb$id_tete, sb$id_tete[prem_b])
  identical(sb$date_naissance, sb$date_naissance[prem_b][i]) && identical(sb$sexe, sb$sexe[prem_b][i]) &&
    all(age_revolu_local(sb$date_naissance[prem_b], sb$date_entree[prem_b]) %in% 20:75)
})
dec_b <- sb$cause_sortie == "deces"
jour_deces_b <- rep(NA_real_, NT_B)
jour_deces_b[sb$id_tete[dec_b]] <- as.numeric(sb$date_sortie[dec_b])
verifier("multi : décès commun (une seule date de décès par tête), aucune ligne en cours dans une tête décédée, rachats et entrées <= date de décès", {
  td <- jour_deces_b[sb$id_tete]; avec <- !is.na(td)
  dates_par_tete <- tapply(as.numeric(sb$date_sortie[dec_b]), sb$id_tete[dec_b], function(v) length(unique(v)))
  all(dates_par_tete == 1L) && !any(sb$cause_sortie[avec] == "en_cours") &&
    all(as.numeric(sb$date_entree[avec]) <= td[avec]) &&
    all(as.numeric(sb$date_sortie[avec & sb$cause_sortie == "rachat"]) <= td[avec & sb$cause_sortie == "rachat"])
})
verifier("multi (critère 3) : attribut, champs des modes existants puis contrats en fin ; decomptes, six champs puis cinq",
         identical(names(ib), c(CHAMPS_ATTRIBUT, "contrats")) &&
           identical(names(ib$decomptes), c(DECOMPTES_EXISTANTS, DECOMPTES_MULTI)) &&
           identical(ib$ordre_tirages, ORDRE_TIRAGES_MULTI$uniforme_multi) && length(ib$ordre_tirages) == 6L &&
           length(ORDRE_TIRAGES_MULTI$age_exact_multi) == 4L &&
           identical(ORDRE_TIRAGES_MULTI$age_exact_multi[1:2], ORDRE_TIRAGES_MULTI$uniforme_multi[1:2]) &&
           identical(ib$contrats$loi, c(0.6, 0.3, 0.1)) && identical(ib$portefeuille, PF_B))
verifier("multi (critère 3) : décomptes de l'attribut = décomptes refaits sur les lignes (identités exactes)", {
  dc <- ib$decomptes; tp <- ib$contrats$tires_par_tete
  so <- as.numeric(sb$date_sortie)
  dc$lignes == nb && dc$deces == sum(dec_b) && dc$rachats == sum(sb$cause_sortie == "rachat") &&
    dc$en_cours == sum(sb$cause_sortie == "en_cours") &&
    dc$sorties_avant_debut == sum(!is.na(so) & so < as.numeric(PER$debut)) &&
    dc$entrees_avant_debut == sum(sb$date_entree < PER$debut) &&
    dc$tetes == NT_B && sum(tp) == NT_B && dc$contrats_tires == sum(seq_along(tp) * tp) &&
    dc$contrats_non_souscrits == dc$contrats_tires - nb && dc$contrats_non_souscrits > 0L &&
    dc$tetes_decedees_observees == sum(!is.na(jour_deces_b)) && dc$tetes_decedees_sans_contrat > 0L
})
verifier("multi : ordre des tirages 1 et 2 (sexe puis K par inversion) recalculé sous la graine (valeur calculée)", {
  u <- tablexp:::sous_graine(GRAINE, list(sexe = stats::runif(NT_B), k = stats::runif(NT_B)))
  K <- 1L + findInterval(u$k, c(0.6, 0.9))
  identical(tabulate(K, 3L), ib$contrats$tires_par_tete) &&
    identical(sb$sexe[prem_b], ifelse(u$sexe < 0.5, "H", "F"))
})
cat(sprintf(paste("  jeu B (uniforme_multi, 10^5 têtes, loi_contrats = (0,6 ; 0,3 ; 0,1), graine %d) : %d contrats",
                  "tirés, %d écrits, %d non souscrits, %d têtes multi-contrats, %d têtes décédées dans le jeu,",
                  "%d décédées sans contrat en cours ; %.1f s\n"),
            GRAINE, ib$decomptes$contrats_tires, nb, ib$decomptes$contrats_non_souscrits, sum(lignes_par_tete > 1L),
            ib$decomptes$tetes_decedees_observees, ib$decomptes$tetes_decedees_sans_contrat, duree_b))

# Moteur sur le jeu B : décès en unité tête, rachat en unité contrat. Plan
# sans délai de déclaration, segmenté par sexe, Poisson seul.
plan_multi <- function(transition, periode, segmentation)
  list(transition = transition, periode = periode, delai_declaration = 0, segmentation = segmentation,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "plus_fine", par_annee = FALSE),
                     brut = list(estimateurs = "poisson", niveau = 0.95)))
VD <- list(depart = "vivant", arrivee = "deces"); RA <- list(depart = "en_cours", arrivee = "rachat")
ROLES_SANS_TETE <- ROLES_SIM[names(ROLES_SIM) != "id_tete"]
t0 <- proc.time()[["elapsed"]]
xb_d <- construire_table(sb, ROLES_SIM, plan_multi(VD, PER, "sexe"))
xb_r <- construire_table(sb, ROLES_SIM, plan_multi(RA, PER, "sexe"))
duree_mb <- proc.time()[["elapsed"]] - t0

# Fenêtres d'exposition des lignes, en jours (T1 : [entrée, sortie), censure
# à B = fin + 1, troncature au début), calculées ici sur les lignes, sans le
# module `donnees`.
fenetres_jours <- function(d, periode) {
  borne <- as.numeric(periode$fin) + 1
  f <- as.numeric(d$date_sortie); f[is.na(f)] <- borne
  list(s = pmax(as.numeric(d$date_entree), as.numeric(periode$debut)), f = pmin(f, borne))
}
# Réunion par tête des fenêtres [s, f) : lignes triées par (tête, s) ;
# maximum courant des fins des lignes précédentes de la tête (rang par rang).
union_jours <- function(tete, s, f) {
  n <- length(tete); prem <- c(TRUE, tete[-1L] != tete[-n])
  rang <- seq_len(n) - cummax(ifelse(prem, seq_len(n), 0L)) + 1L
  M <- rep(-Inf, n)
  for (r in seq_len(max(rang))[-1L]) { i <- which(rang == r); M[i] <- pmax(M[i - 1L], f[i - 1L]) }
  sum(pmax(0, f - pmax(s, M)))
}
fb <- fenetres_jours(sb, PER)
dans_periode <- function(d) !is.na(d$date_sortie) & d$date_sortie >= PER$debut & d$date_sortie <= PER$fin
verifier("multi, moteur, décès en unité tête : unité tete, les 20 motifs à 0, aucune exclusion",
         identical(xb_d$etapes$donnees$parametres$unite_observation, "tete") &&
           nrow(xb_d$etapes$donnees$diagnostics$motifs) == 20L &&
           all(xb_d$etapes$donnees$diagnostics$motifs$nombre == 0L) &&
           xb_d$etapes$donnees$resultat$nombre_lignes_exclues == 0L &&
           all(xb_r$etapes$donnees$diagnostics$motifs$nombre == 0L))
verifier("multi, moteur (identité exacte) : décès en unité tête = têtes distinctes à décès daté dans la période",
         sum(xb_d$etapes$brut$resultat$poisson$evenements) ==
           length(unique(sb$id_tete[dec_b & dans_periode(sb)])))
verifier("multi, moteur (identité exacte) : rachats en unité contrat = lignes à rachat datées dans la période",
         sum(xb_r$etapes$brut$resultat$poisson$evenements) == sum(sb$cause_sortie == "rachat" & dans_periode(sb)))
expo_union_b <- union_jours(sb$id_tete, fb$s, fb$f)
expo_somme_b <- sum(pmax(0, fb$f - fb$s))
verifier("multi, moteur (identité exacte) : exposition en jours, unité tête = réunion des fenêtres par tête",
         sum(xb_d$etapes$exposition$resultat$exposition$exposition_jours) == expo_union_b)
verifier("multi, moteur (identité exacte) : exposition en jours, unité contrat = somme des fenêtres des lignes",
         sum(xb_r$etapes$exposition$resultat$exposition$exposition_jours) == expo_somme_b)

# Famille B (spécification, § 1.6) : Z = (sum D - sum mu_x E_x) / sqrt(sum
# mu_x E_x), sommes sur les cellules (sexe, âge) du moteur, mu_x force vraie
# (loi vraie, -ln(1 - f_s q_x)), E_x exposition centrale en années d'âge.
# Statut ASYMPTOTIQUE : Z est asymptotiquement N(0, 1) (processus de
# comptage, Andersen, Borgan, Gill et Keiding, 1993, page non vérifiée) ;
# fausse alarme nominale 2 x (1 - Phi(3,4808)) = 5.10^-4 par test, 10^-3 pour
# les deux, non exacte. L'arrondi des sorties au jour (T1) biaise E[Z]
# d'environ 0,02 écart-type selon la spécification (hypothèse non mesurée).
# Robustesse (calcul exact sur la loi normale, seuil 3,4808) : un décalage
# delta de la moyenne de Z porte la fausse alarme de chaque test,
# pnorm(-3,4808 - delta) + pnorm(-3,4808 + delta), de 4,999.10^-4
# (delta = 0) à 5,012.10^-4 (delta = 0,02) et 5,326.10^-4 (delta = 0,1).
# Valeurs informatives, sans verdict : X^2 / ddl par cellule ; phi^ = sum C^2
# / sum C (C : lignes à décès d'une tête, dans la période), rapports des
# décès et des expositions en unité contrat et en unité tête.
SEUIL_Z <- 3.4808
verifier("famille B : seuil |Z| <= 3,4808 = quantile de N(0, 1) d'ordre 1 - 10^-3 / 4 à 1e-4",
         abs(stats::qnorm(1 - 1e-3 / 4) - SEUIL_Z) < 1e-4)
mu_vraie <- function(cause, sexe, age) {
  f <- if (cause == "deces") c(H = 1, F = 0.6) else c(H = 1, F = 1)
  q <- if (cause == "deces") GM$qx[match(age, GM$age)] else rep(0.05, length(age))
  -log1p(-pmin(f[sexe] * q, 1))
}
for (b in list(list(id = "B1", nom = "décès, unité tête", x = xb_d, cause = "deces"),
               list(id = "B2", nom = "rachat, unité contrat", x = xb_r, cause = "rachat"))) {
  pt <- b$x$etapes$brut$resultat$poisson
  att <- mu_vraie(b$cause, pt$sexe, pt$age) * pt$exposition_centrale
  z <- (sum(pt$evenements) - sum(att)) / sqrt(sum(att))
  x2 <- sum(((pt$evenements - att)^2 / att)[att > 0]) / sum(att > 0)
  ligne <- sprintf(paste("constat de simulation (asymptotique) %s, %s : Z = %.4f, |Z| <= %.4f (D = %d, attendu",
                         "sum mu E = %.2f ; 10^5 têtes, graine %d ; fausse alarme nominale 5e-4, non exacte) ;",
                         "informatif : X^2/ddl = %.4f sur %d cellules"),
                   b$id, b$nom, z, SEUIL_Z, sum(pt$evenements), sum(att), GRAINE, x2, sum(att > 0))
  cat("  ", ligne, "\n", sep = "")
  verifier(ligne, abs(z) <= SEUIL_Z)
}
C_b <- tabulate(sb$id_tete[dec_b & dans_periode(sb)], nbins = NT_B); C_b <- C_b[C_b > 0L]
cat(sprintf(paste("  famille B, informatif (sans verdict) : décès unité contrat / unité tête = %d / %d = %.4f ;",
                  "exposition unité contrat / unité tête = %.4f ; phi^ = sum C^2 / sum C = %.4f ; moteur %.1f s\n"),
            sum(C_b), length(C_b), sum(C_b) / length(C_b), expo_somme_b / expo_union_b,
            sum(C_b^2) / sum(C_b), duree_mb))

# --- famille A (spécification, § 1.6) : age_exact_multi, fausse alarme exacte ---------------
# n = 3.10^5 têtes, hommes, x = 60, entrée 2016-03-15 (anniversaire), période
# [2016-03-15, 2017-03-14] : l'année d'âge 60 entière (L = 365 jours), sans
# censure avant B = anniv(61). Décès Gompertz-Makeham (q_60), rachat q^r =
# 0,30, loi_contrats = (0,5 ; 0,3 ; 0,2), graine 20261025. mu = -ln(1 - q_60),
# rho = -ln(0,7). N_c = nombre de têtes ayant c contrats en cours à leur
# décès dans l'année : N_c ~ Bin(n, pi_c) exactement, pi_c = sum_K p_K
# pi_c(K), pi_c(K) = C(K, c) sum_{j=0}^{K-c} C(K-c, j) (-1)^j mu / (mu + (c
# + j) rho) (1 - e^{-(mu + (c + j) rho)}) (forme close ; recoupement par
# stats::integrate). Têtes à k lignes ~ Bin(n, p_k) (aucun contrat non
# souscrit : toutes les entrées valent e_(1)). Sept tests binomiaux,
# Bonferroni, alpha = 10^-3, a_k = alpha / 7 ; régions bilatérales exactes
# [qbinom(a_k/2), qbinom(1 - a_k/2)] et fausses alarmes exactes FA_k,
# recalculées ici et comparées à celles de la spécification.
N_A <- 3e5; ALPHA_A <- 1e-3; K_A <- 7L; A_K <- ALPHA_A / K_A; GRAINE_A <- 20261025L
P_K <- c(0.5, 0.3, 0.2)
PER_A <- list(debut = E, fin = D("2017-03-14")); B_A <- as.numeric(PER_A$fin) + 1
R30 <- table_constante(0.30, 0:120, "rachat constant 30 %")
mu_a <- -log(1 - q(60)); rho_a <- -log(0.7)
pi_cK <- function(c, K) choose(K, c) * sum(vapply(0:(K - c), function(j)
  choose(K - c, j) * (-1)^j * mu_a / (mu_a + (c + j) * rho_a) * (1 - exp(-(mu_a + (c + j) * rho_a))), numeric(1L)))
pi_cK_integrale <- function(c, K) stats::integrate(function(t)
  mu_a * exp(-mu_a * t) * choose(K, c) * exp(-c * rho_a * t) * (1 - exp(-rho_a * t))^(K - c),
  0, 1, rel.tol = 1e-13)$value
pi_c <- vapply(0:3, function(c) sum(vapply(max(c, 1L):3, function(K) P_K[K] * pi_cK(c, K), numeric(1L))), numeric(1L))
pi_c_int <- vapply(0:3, function(c) sum(vapply(max(c, 1L):3, function(K) P_K[K] * pi_cK_integrale(c, K), numeric(1L))),
                   numeric(1L))
verifier("famille A : pi_0 à pi_3, forme close = stats::integrate (recoupement) à 1e-12 relatif",
         proche(pi_c, pi_c_int, rel = 1e-12, abs = 0))
verifier("famille A : mu = 0,00818659807815 et rho = 0,356674943939 de la spécification, à l'arrondi de leur dernière décimale (5e-15 et 5e-13)",
         isTRUE(proche(mu_a, 0.00818659807815, rel = 0, abs = 5e-15)) && isTRUE(proche(rho_a, 0.356674943939, rel = 0, abs = 5e-13)))
TESTS_A <- list(
  list(id = "A1", nom = "têtes à 1 ligne", p = P_K[1], region_spec = c(148958, 151042), fa_spec = 1.408e-4),
  list(id = "A2", nom = "têtes à 2 lignes", p = P_K[2], region_spec = c(89046, 90955), fa_spec = 1.419e-4),
  list(id = "A3", nom = "têtes à 3 lignes", p = P_K[3], region_spec = c(59168, 60835), fa_spec = 1.408e-4),
  list(id = "A4", nom = "N_1", p = pi_c[2], p_spec = 0.00416937354957, region_spec = c(1119, 1387), fa_spec = 1.380e-4),
  list(id = "A5", nom = "N_2", p = pi_c[3], p_spec = 0.00224283291693, region_spec = c(577, 774), fa_spec = 1.329e-4),
  list(id = "A6", nom = "N_3", p = pi_c[4], p_spec = 0.00100193504543, region_spec = c(237, 369), fa_spec = 1.231e-4),
  list(id = "A7", nom = "D à 60 ans, Poisson, unité tête (moteur)", p = sum(pi_c[2:4]), p_spec = 0.00741414151193,
       region_spec = c(2048, 2405), fa_spec = 1.390e-4))
verifier("famille A : p de A4 à A7 (forme close) = valeurs de la spécification à 1e-14",
         proche(vapply(TESTS_A[4:7], `[[`, numeric(1L), "p"), vapply(TESTS_A[4:7], `[[`, numeric(1L), "p_spec"),
                rel = 0, abs = 1e-14))

t0 <- proc.time()[["elapsed"]]
sa <- simuler(loi(rachat = R30),
              list(mode = "age_exact_multi", n = N_A, proportion_hommes = 1, age = 60, date_entree = E,
                   loi_contrats = P_K), PER_A, GRAINE_A)
xa_t <- construire_table(sa, ROLES_SIM, plan_multi(VD, PER_A, character(0)))
xa_c <- construire_table(sa, ROLES_SANS_TETE, plan_multi(VD, PER_A, character(0)))
duree_a <- proc.time()[["elapsed"]] - t0
ia <- attr(sa, "simulation")
dec_a <- sa$cause_sortie == "deces"
C_a <- tabulate(sa$id_tete[dec_a], nbins = N_A)
N_c <- tabulate(C_a[C_a > 0L], nbins = 3L)
L_k <- tabulate(tabulate(sa$id_tete, nbins = N_A), nbins = 3L)
pt_a <- xa_t$etapes$brut$resultat$poisson; pc_a <- xa_c$etapes$brut$resultat$poisson
verifier("famille A : toutes les entrées le 2016-03-15, aucun contrat non souscrit, têtes à k lignes = tires_par_tete (identité exacte)",
         all(sa$date_entree == E) && ia$decomptes$contrats_non_souscrits == 0L &&
           identical(L_k, ia$contrats$tires_par_tete) &&
           identical(ia$ordre_tirages, ORDRE_TIRAGES_MULTI$age_exact_multi))
verifier("famille A, moteur : une seule cellule (âge 60), dans chaque unité ; unités tete et contrat",
         identical(pt_a$age, 60L) && identical(pc_a$age, 60L) &&
           identical(xa_t$etapes$donnees$parametres$unite_observation, "tete") &&
           identical(xa_c$etapes$donnees$parametres$unite_observation, "contrat"))
OBSERVE_A <- c(L_k, N_c, pt_a$evenements)
fa_a <- numeric(K_A)
for (k in seq_len(K_A)) {
  tk <- TESTS_A[[k]]
  lo <- stats::qbinom(A_K / 2, N_A, tk$p); hi <- stats::qbinom(1 - A_K / 2, N_A, tk$p)
  fa_a[k] <- stats::pbinom(lo - 1, N_A, tk$p) + stats::pbinom(hi, N_A, tk$p, lower.tail = FALSE)
  verifier(sprintf("famille A, %s : région [%d, %d] = région de la spécification ; FA_k = %.4g = %.4g de la spécification (4 chiffres)",
                   tk$id, lo, hi, fa_a[k], tk$fa_spec),
           identical(c(lo, hi), tk$region_spec) && isTRUE(proche(signif(fa_a[k], 4), tk$fa_spec, rel = 1e-12, abs = 0)))
  ligne <- sprintf(paste("constat de simulation (exact) %s, %s : %d dans [%d, %d] (n = %g têtes, p = %.12g,",
                         "a_k = %.4g, fausse alarme exacte FA_k = %.4g ; graine %d)"),
                   tk$id, tk$nom, OBSERVE_A[k], lo, hi, N_A, tk$p, A_K, fa_a[k], GRAINE_A)
  cat("  ", ligne, "\n", sep = "")
  verifier(ligne, OBSERVE_A[k] >= lo && OBSERVE_A[k] <= hi)
}
ligne <- sprintf(paste("famille A : fausse alarme familiale <= sum(FA_k) = %.4g <= alpha = %g (Bonferroni, K = %d,",
                       "sans hypothèse d'indépendance ; spécification : 9,566e-4)"), sum(fa_a), ALPHA_A, K_A)
cat("  ", ligne, "\n", sep = "")
verifier(ligne, sum(fa_a) <= ALPHA_A && isTRUE(proche(signif(sum(fa_a), 4), 9.566e-4, rel = 1e-12, abs = 0)))

# Identités exactes de la famille A (spécification, § 1.6).
fa_j <- fenetres_jours(sa, PER_A)
o_a <- order(sa$id_tete, fa_j$f, method = "radix")
fin_tete_a <- fa_j$f[o_a][!duplicated(sa$id_tete[o_a], fromLast = TRUE)]
verifier("famille A (identité exacte) : D unité tête (moteur) = sum N_c ; D unité contrat (moteur) = sum c N_c",
         pt_a$evenements == sum(N_c) && pc_a$evenements == sum(seq_len(3L) * N_c))
verifier("famille A (identité exacte) : exposition en jours, unité tête = sum_têtes (max_k f_k - E) ; unité contrat = sum (f_k - E)",
         xa_t$etapes$exposition$resultat$exposition$exposition_jours == sum(fin_tete_a - as.numeric(E)) &&
           xa_c$etapes$exposition$resultat$exposition$exposition_jours == sum(fa_j$f - as.numeric(E)))
verifier("famille A (identité exacte) : chevauchements + contiguïtés = lignes - têtes ; aucun intervalle disjoint ; les 20 motifs à 0", {
  rl <- xa_t$etapes$donnees$diagnostics$releves
  mo <- xa_t$etapes$donnees$diagnostics$motifs
  rl$chevauchements + rl$contiguites == nrow(sa) - N_A && rl$disjoints == 0L &&
    nrow(mo) == 20L && all(mo$nombre == 0L)
})

# Écart entre unité tête et unité contrat (spécification, § 1.6) : valeurs
# exactes sous le modèle simulé. Exposition en jours (T1) : un jour d de
# l'année est exposé si l'instant de sortie est >= d / L, d'où E[E] = (n/L)
# sum_{d=1}^{L} S(d/L), S_K(t) = e^{-mu t} (1 - (1 - e^{-rho t})^K) pour la
# tête, K e^{-(mu + rho) t} pour ses contrats.
L_A <- 365; dd <- seq_len(L_A) / L_A
EE_t <- N_A / L_A * sum(vapply(1:3, function(K) P_K[K] * sum(exp(-mu_a * dd) * (1 - (1 - exp(-rho_a * dd))^K)),
                               numeric(1L)))
EE_c <- N_A / L_A * sum(vapply(1:3, function(K) P_K[K] * K * sum(exp(-(mu_a + rho_a) * dd)), numeric(1L)))
ED_t <- N_A * sum(pi_c[2:4]); ED_c <- N_A * sum(1:3 * pi_c[2:4])
phi_a <- sum((1:3)^2 * pi_c[2:4]) / sum(1:3 * pi_c[2:4])
ecart_ut <- c(ED_c / ED_t, EE_c / EE_t, (ED_c / EE_c) / (ED_t / EE_t), phi_a)
verifier(sprintf(paste("famille A, unité tête / unité contrat (valeurs exactes) : E[D_c]/E[D_t] = %.6f, E[E_c]/E[E_t] = %.6f,",
                       "rapport des taux = %.6f, phi = %.6f, égaux à la spécification à 5e-7"),
                 ecart_ut[1], ecart_ut[2], ecart_ut[3], ecart_ut[4]),
         proche(ecart_ut, c(1.572784, 1.572448, 1.000214, 1.900216), rel = 0, abs = 5e-7))
cat(sprintf(paste("  famille A, informatif (sans verdict) : observé D_c/D_t = %.4f (exact %.6f), E_c/E_t = %.4f",
                  "(exact %.6f), rapport des taux %.6f (exact %.6f), phi^ = %.4f (exact %.6f) ; l'intervalle",
                  "nominal en unité contrat est trop étroit d'un facteur sqrt(phi) = %.4f ; têtes décédées sans",
                  "contrat en cours %d (attendu n pi_0 = %.1f) ; %.1f s\n"),
            pc_a$evenements / pt_a$evenements, ecart_ut[1],
            pc_a$exposition_centrale / pt_a$exposition_centrale, ecart_ut[2],
            pc_a$taux / pt_a$taux, ecart_ut[3], sum((1:3)^2 * N_c) / sum(1:3 * N_c), phi_a, sqrt(phi_a),
            ia$decomptes$tetes_decedees_sans_contrat, N_A * pi_c[1], duree_a))

# --- loi_contrats : refus (critère 7) et formes admises ---------------------------------------
PF_M <- c(modifyList(PF_FORME, list(mode = "uniforme_multi")), list(loi_contrats = c(0.6, 0.3, 0.1)))
LOI_CONTRATS_FORME <- list(
  list("nommée", c(a = 0.6, b = 0.3, c = 0.1)),
  list("à attribut", structure(c(0.6, 0.3, 0.1), foo = "x")),
  list("matrice 1 x 3", matrix(c(0.6, 0.3, 0.1), 1L)),
  list("de classe", structure(c(0.6, 0.3, 0.1), class = "x")),
  list("caractère", c("0.6", "0.3", "0.1")),
  list("logique", TRUE),
  list("liste", list(0.6, 0.3, 0.1)),
  list("vide", numeric(0)),
  list("NULL", NULL),
  list("valeur manquante", c(0.6, NA, 0.4)),
  list("valeur infinie", c(Inf, 0.3)),
  list("valeur négative", c(1.2, -0.2)),
  list("dernière probabilité nulle (p_m = 0)", c(0.7, 0.3, 0)),
  list("somme 0,9", c(0.6, 0.3)),
  list("somme 1 + 1e-11", c(0.6, 0.4 + 1e-11)))
for (k in LOI_CONTRATS_FORME) {
  pf <- PF_M; pf["loi_contrats"] <- list(k[[2]])
  verifier(sprintf("loi_contrats %s -> refus portefeuille_invalide, message du champ", k[[1]]),
           leve_refus(simuler(loi(), pf, PER, GRAINE), "portefeuille_invalide") &&
             grepl("loi_contrats : probabilités", message_refus(simuler(loi(), pf, PER, GRAINE)), fixed = TRUE))
}
verifier("loi_contrats absente d'un mode multi -> refus portefeuille_invalide (champ manquant, aucun défaut implicite)",
         leve_refus(simuler(loi(), PF_M[names(PF_M) != "loi_contrats"], PER, GRAINE), "portefeuille_invalide"))
verifier("loi_contrats dans un mode existant -> refus portefeuille_invalide (champ inconnu)",
         leve_refus(simuler(loi(), c(PF_FORME, list(loi_contrats = 1)), PER, GRAINE), "portefeuille_invalide"))
verifier("garde de débordement : n x length(loi_contrats) > .Machine$integer.max -> refus portefeuille_invalide, sans tirage", {
  set.seed(5L); avant <- globalenv()$.Random.seed
  leve_refus(simuler(loi(), modifyList(PF_M, list(n = 1e9)), PER, GRAINE), "portefeuille_invalide") &&
    grepl("integer.max", message_refus(simuler(loi(), modifyList(PF_M, list(n = 1e9)), PER, GRAINE)), fixed = TRUE) &&
    identical(globalenv()$.Random.seed, avant)
})
verifier("garde de débordement : n = 1000000000L (integer) -> refus portefeuille_invalide, sans débordement integer, sans tirage", {
  set.seed(6L); avant <- globalenv()$.Random.seed
  pf <- modifyList(PF_M, list(n = 1000000000L))
  leve_refus(simuler(loi(), pf, PER, GRAINE), "portefeuille_invalide") &&
    grepl("(3000000000)", message_refus(simuler(loi(), pf, PER, GRAINE)), fixed = TRUE) &&
    identical(globalenv()$.Random.seed, avant)
})
verifier("loi_contrats admises : integer, somme à 1e-13 près ; loi dégénérée c(0, 1) : K = 2 pour toute tête, sans tirage de K", {
  r1 <- simuler(loi(), modifyList(PF_M, list(loi_contrats = c(0L, 1L))), PER, GRAINE)
  r2 <- simuler(loi(), modifyList(PF_M, list(loi_contrats = c(0.6, 0.3, 0.1 + 1e-13))), PER, GRAINE)
  x2 <- simuler(loi(), modifyList(PF_X, list(mode = "age_exact_multi", n = 1000, loi_contrats = c(0, 1))), PER_X2, GRAINE)
  # Sans tirage de K, les tirages du mode age_exact_multi sont sexe, décès (n),
  # rachat (2 n) : le sexe et l'instant de décès de chaque tête sont ceux du
  # mode age_exact ; une tête dont le décès figure dans les deux jeux a donc la
  # même date de décès dans les deux.
  x1 <- simuler(loi(), PF_X, PER_X2, GRAINE)
  d1 <- x1$cause_sortie == "deces"; d2 <- x2$cause_sortie == "deces"
  commun <- intersect(x1$id_tete[d1], x2$id_tete[d2])
  identical(attr(r1, "simulation")$contrats$tires_par_tete, c(0L, 1000L)) &&
    identical(attr(r1, "simulation")$contrats$loi, c(0, 1)) && is.data.frame(r2) &&
    identical(attr(x2, "simulation")$contrats$tires_par_tete, c(0L, 1000L)) && nrow(x2) == 2000L &&
    identical(x2$sexe[!duplicated(x2$id_tete)], x1$sexe) && length(commun) > 0L &&
    identical(x1$date_sortie[d1][match(commun, x1$id_tete[d1])], x2$date_sortie[d2][match(commun, x2$id_tete[d2])])
})

# --- défauts paramétrés (#93) -------------------------------------------------------------
# Spécification d'actuary-approfondi du 25/10/2026 (#92, § 2 et § 4, #93) ;
# réponse d'actuary à la question (f) (#93, 25/10/2026) ; ADR 0007,
# annotation du 25/10/2026, (c), points 6 à 9, et (d). Natures des attendus :
# littéraux de la spécification (catalogue, § 2.2, recopié ici, non lu dans
# le package) ; identités exactes (décomptes et sélections refaits sur les
# lignes, ordre des tirages rejoué sous la graine des défauts) ; constat de
# simulation du § 2.6 (fausse alarme exacte pour les décès exclus, borne
# exacte de Hoeffding pour l'exposition exclue). Non-circularité : les
# sorties du module `donnees` (preparer_donnees(), construire_table()) sont
# confrontées à l'attribut `defauts`, jamais employées pour l'établir.

CATALOGUE_DEFAUTS <- tablexp:::CATALOGUE_DEFAUTS
TOUS_MOTIFS <- c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE)
GRAINE_DEF <- 20261025L
TOUTES <- c("deces", "rachat", "en_cours"); DR <- c("deces", "rachat")
# Catalogue de la spécification, § 2.2 : type, unité, causes éligibles
# (types de ligne), condition, motif attendu (NA : refus date_hors_domaine).
SPEC_DEFAUTS <- list(
  list("contrat_duplique", "ligne", TOUTES, "", "contrat_duplique"),
  list("id_contrat_manquant", "ligne", TOUTES, "", "id_contrat_manquant"),
  list("id_tete_manquant", "ligne", TOUTES, "", "id_tete_manquant"),
  list("naissance_manquante", "ligne", TOUTES, "", "naissance_manquante"),
  list("entree_manquante", "ligne", TOUTES, "", "entree_manquante"),
  list("sortie_manquante", "ligne", DR, "", "sortie_manquante"),
  list("cause_manquante", "ligne", TOUTES, "", "cause_manquante"),
  list("cause_hors_modalites", "ligne", DR, "", "cause_hors_modalites"),
  list("sexe_manquant", "ligne", TOUTES, "", "sexe_manquant"),
  list("sexe_hors_modalites", "ligne", TOUTES, "", "sexe_hors_modalites"),
  list("naissance_apres_entree", "ligne", TOUTES, "", "naissance_apres_entree"),
  list("sortie_avant_entree", "ligne", DR, "", "sortie_avant_entree"),
  list("en_cours_avant_fin", "ligne", "en_cours", "entree_avant_fin", "en_cours_avant_fin"),
  list("age_aberrant", "ligne", TOUTES, "", "age_aberrant"),
  list("date_hors_domaine", "ligne", TOUTES, "", NA_character_),
  list("tete_naissance_incoherente", "tete", TOUTES, "tete_multi", "tete_naissance_incoherente"),
  list("tete_sexe_incoherent", "tete", TOUTES, "tete_multi", "tete_sexe_incoherent"),
  list("tete_observee_apres_deces", "tete", TOUTES, "tete_multi_deces", "tete_observee_apres_deces"))
SPEC_TYPE <- vapply(SPEC_DEFAUTS, `[[`, "", 1L)
verifier("défauts, catalogue (§ 2.2) : types, unités, causes éligibles, conditions, motifs et refus attendus = spécification", {
  cat_ok <- identical(CATALOGUE_DEFAUTS$defaut, SPEC_TYPE) &&
    identical(CATALOGUE_DEFAUTS$unite, vapply(SPEC_DEFAUTS, `[[`, "", 2L)) &&
    identical(CATALOGUE_DEFAUTS$condition, vapply(SPEC_DEFAUTS, `[[`, "", 4L)) &&
    identical(CATALOGUE_DEFAUTS$motif_attendu, vapply(SPEC_DEFAUTS, `[[`, "", 5L)) &&
    identical(CATALOGUE_DEFAUTS$refus_attendu, ifelse(is.na(CATALOGUE_DEFAUTS$motif_attendu), "date_hors_domaine",
                                                      "ligne_invalide"))
  lignes_ok <- all(vapply(seq_along(SPEC_DEFAUTS), function(i) SPEC_DEFAUTS[[i]][[2L]] == "tete" ||
    identical(TOUTES[unlist(CATALOGUE_DEFAUTS[i, TOUTES])], SPEC_DEFAUTS[[i]][[3L]]), logical(1L)))
  cat_ok && lignes_ok
})
verifier("défauts, test de clôture : motifs attendus du catalogue + 3 motifs hors périmètre = c(MOTIFS_LIGNES, MOTIFS_TETE)", {
  m <- c(CATALOGUE_DEFAUTS$motif_attendu[!is.na(CATALOGUE_DEFAUTS$motif_attendu)],
         "annee_naissance_incoherente", "segment_manquant", "tete_segment_incoherent")
  !anyDuplicated(m) && length(m) == length(TOUS_MOTIFS) && setequal(m, TOUS_MOTIFS)
})
verifier("défauts : COMBINAISONS_DEFAUTS et ORDRE_TIRAGES_DEFAUTS égales à leurs littéraux (spécification, § 2.3 et § 2.4)",
         identical(tablexp:::COMBINAISONS_DEFAUTS, list(c("cause_manquante", "sexe_manquant"))) &&
           identical(tablexp:::ORDRE_TIRAGES_DEFAUTS, c(
             paste("0. après le tirage complet du jeu propre, un seul sous_graine(defauts$graine), graine différente",
                   "de celle du jeu ; déclarations traitées dans l'ordre reçu"),
             paste("1. éligibles : unités non encore altérées, en ordre croissant (lignes du jeu propre ; têtes dont",
                   "aucune ligne n'est altérée) ; une tête retenue rend toutes ses lignes inéligibles ensuite"),
             "2. taux : unités retenues = éligibles[runif(nombre d'éligibles) < taux]",
             "3. nombre : unités retenues = sort(éligibles[sample.int(nombre d'éligibles, nombre)])")))

# Critère 1 : defauts = NULL, jeu identical() à l'appel sans l'argument, sans attribut defauts.
PF_M_1E3 <- modifyList(PF_M, list(n = 1000))
verifier("défauts (critère 1) : defauts = NULL -> jeu identical() à l'appel sans l'argument, aucun attribut defauts (uniforme, uniforme_multi, age_exact_multi)", {
  px <- modifyList(PF_X, list(mode = "age_exact_multi", loi_contrats = c(0.5, 0.5)))
  all(vapply(list(list(PF_FORME, PER), list(PF_M_1E3, PER), list(px, PER_X2)), function(a) {
    r <- simuler(loi(), a[[1]], a[[2]], GRAINE, defauts = NULL)
    identical(r, simuler(loi(), a[[1]], a[[2]], GRAINE)) && is.null(attr(r, "defauts"))
  }, logical(1L)))
})

# Jeu catalogue (critère 3) : uniforme_multi, 5 000 têtes, nombre = 5 par
# type (hors date_hors_domaine, jeu à part) et pour la combinaison admise.
PF_CAT <- modifyList(PF_M, list(n = 5000))
DECL_CAT <- c(lapply(setdiff(SPEC_TYPE, "date_hors_domaine"), function(t)
  if (CATALOGUE_DEFAUTS$unite[CATALOGUE_DEFAUTS$defaut == t] == "ligne")
    list(type = t, cible = TOUTES, nombre = 5) else list(type = t, nombre = 5)),
  list(list(type = c("cause_manquante", "sexe_manquant"), cible = TOUTES, nombre = 5)))
DEF_CAT <- list(graine = GRAINE_DEF, declarations = DECL_CAT)
t0 <- proc.time()[["elapsed"]]
sc0 <- simuler(loi(), PF_CAT, PER, GRAINE)
set.seed(11L); avant <- globalenv()$.Random.seed
sc <- simuler(loi(), PF_CAT, PER, GRAINE, defauts = DEF_CAT)
duree_cat <- proc.time()[["elapsed"]] - t0
verifier("défauts : appelant sous set.seed(11) : .Random.seed global inchangé après une injection",
         identical(globalenv()$.Random.seed, avant))
idf <- attr(sc, "defauts"); lg <- idf$lignes; N0 <- nrow(sc0)
verifier("défauts (critère 2) : attribut simulation identical() à celui du jeu propre ; mêmes colonnes et types",
         identical(attr(sc, "simulation"), attr(sc0, "simulation")) && identical(names(sc), names(sc0)) &&
           identical(lapply(sc, class), lapply(sc0, class)) && identical(vapply(sc, typeof, ""), vapply(sc0, typeof, "")) &&
           .row_names_info(sc) == -nrow(sc))
verifier("défauts (critère 2) : lignes non altérées identiques au jeu propre, colonne par colonne", {
  ln <- setdiff(seq_len(N0), lg$ligne)
  length(ln) > 0L && all(vapply(names(sc0), function(v) identical(sc[[v]][ln], sc0[[v]][ln]), logical(1L)))
})
verifier("défauts : attribut defauts, champs dans l'ordre (parametre, graine, generateur, ordre_tirages, validite, lignes, decomptes)",
         identical(names(idf), c("parametre", "graine", "generateur", "ordre_tirages", "validite", "lignes", "decomptes")) &&
           identical(idf$parametre, DEF_CAT) && identical(idf$graine, GRAINE_DEF) && identical(idf$generateur, GENERATEUR) &&
           identical(idf$ordre_tirages, tablexp:::ORDRE_TIRAGES_DEFAUTS) &&
           identical(idf$validite$periode, PER) && identical(idf$validite$delai_declaration, 0) &&
           identical(unname(idf$validite$roles), names(ROLES_SIM)) && identical(names(idf$validite$roles), names(ROLES_SIM)))
verifier("défauts : lignes de l'attribut, colonnes (ligne, declaration, defaut, unite, role, motif_attendu, refus_attendu, premier_deces) et types",
         identical(names(lg), c("ligne", "declaration", "defaut", "unite", "role", "motif_attendu", "refus_attendu",
                                "premier_deces")) &&
           is.integer(lg$ligne) && is.integer(lg$declaration) && identical(class(lg$premier_deces), "Date") &&
           all(lg$role %in% c("alteree", "copie", "original", "tete")) &&
           identical(lg$motif_attendu, CATALOGUE_DEFAUTS$motif_attendu[match(lg$defaut, CATALOGUE_DEFAUTS$defaut)]))

# Ordre des tirages rejoué (identité exacte) : éligibles recalculés d'après
# le catalogue de la spécification (SPEC_DEFAUTS), disjonction comprise,
# sous sous_graine(GRAINE_DEF), déclarations dans l'ordre reçu.
rejouer_defauts <- function(jeu, decl, graine, fin) {
  n <- nrow(jeu); nt <- max(jeu$id_tete); alt <- logical(n); out <- vector("list", length(decl))
  tablexp:::sous_graine(graine, for (k in seq_along(decl)) {
    d <- decl[[k]]; sp <- SPEC_DEFAUTS[match(d$type, SPEC_TYPE)]
    if (sp[[1]][[2]] == "ligne") {
      ok <- !alt & jeu$cause_sortie %in% d$cible
      for (x in sp) {
        ok <- ok & jeu$cause_sortie %in% x[[3]]
        if (x[[4]] == "entree_avant_fin") ok <- ok & as.numeric(jeu$date_entree) < fin
      }
    } else {
      ok <- tabulate(jeu$id_tete, nt) >= 2L & tabulate(jeu$id_tete[alt], nt) == 0L
      if (sp[[1]][[4]] == "tete_multi_deces") ok <- ok & tabulate(jeu$id_tete[jeu$cause_sortie == "deces"], nt) > 0L
    }
    el <- which(ok)
    r <- if (!is.null(d$taux)) el[stats::runif(length(el)) < d$taux] else sort(el[sample.int(length(el), d$nombre)])
    if (sp[[1]][[2]] == "ligne") alt[r] <- TRUE else alt[jeu$id_tete %in% r] <- TRUE
    out[[k]] <- list(retenues = r, eligibles = length(el))
  })
  out
}
rj <- rejouer_defauts(sc0, DECL_CAT, GRAINE_DEF, as.numeric(PER$fin))
verifier("défauts : ordre des tirages rejoué sous la graine des défauts = unités retenues et éligibles consignés (identité exacte)", {
  dd <- idf$decomptes$declarations
  identical(dd$eligibles, vapply(rj, `[[`, 0L, "eligibles")) && all(dd$retenues == 5L) &&
    all(vapply(seq_along(DECL_CAT), function(k) {
      l <- lg[lg$declaration == k, , drop = FALSE]
      u <- if (DECL_CAT[[k]]$type[1L] %in% SPEC_TYPE[16:18]) sort(unique(sc0$id_tete[l$ligne]))
           else sort(unique(l$ligne[l$role != "copie"]))
      identical(u, rj[[k]]$retenues)
    }, logical(1L)))
})
verifier("défauts : disjonction, une ligne n'appartient qu'à une déclaration ; une tête altérée n'a aucune ligne dans une autre déclaration", {
  decl_par_ligne <- tapply(lg$declaration, lg$ligne, function(v) length(unique(v)))
  tetes_t <- unique(sc0$id_tete[lg$ligne[lg$unite == "tete"]])
  all(decl_par_ligne == 1L) &&
    !any(sc0$id_tete[lg$ligne[lg$unite == "ligne" & lg$ligne <= N0]] %in% tetes_t)
})
verifier("défauts : copies (contrat_duplique) en fin de jeu, lignes N + 1 à N + 5, valeurs de l'original ; décomptes", {
  o <- lg$ligne[lg$role == "original"]; cp <- lg$ligne[lg$role == "copie"]
  identical(cp, N0 + 1:5) && nrow(sc) == N0 + 5L &&
    all(vapply(names(sc0), function(v) identical(sc[[v]][cp], sc0[[v]][o]) && identical(sc[[v]][o], sc0[[v]][o]),
               logical(1L))) &&
    idf$decomptes$lignes_jeu_propre == N0 && idf$decomptes$copies == 5L && idf$decomptes$lignes == N0 + 5L &&
    idf$decomptes$lignes_en_defaut == length(unique(lg$ligne))
})
verifier("défauts : altérations déterministes (spécification, § 2.2), valeur par valeur", {
  a <- function(t) lg$ligne[lg$defaut == t & lg$role == "alteree" & lg$declaration != length(DECL_CAT)]
  j0 <- function(v, t) as.numeric(sc0[[v]][a(t)]); j1 <- function(v, t) as.numeric(sc[[v]][a(t)])
  an_e <- as.POSIXlt(sc0$date_entree[a("age_aberrant")])$year + 1900L
  comb <- lg$ligne[lg$declaration == length(DECL_CAT)]
  t_obs <- lg[lg$defaut == "tete_observee_apres_deces", , drop = FALSE]
  all(is.na(sc$id_contrat[a("id_contrat_manquant")])) && all(is.na(sc$id_tete[a("id_tete_manquant")])) &&
    all(is.na(sc$date_naissance[a("naissance_manquante")])) && all(is.na(sc$date_entree[a("entree_manquante")])) &&
    all(is.na(sc$date_sortie[a("sortie_manquante")])) && all(is.na(sc$cause_sortie[a("cause_manquante")])) &&
    identical(sc$cause_sortie[a("cause_hors_modalites")], toupper(sc0$cause_sortie[a("cause_hors_modalites")])) &&
    all(is.na(sc$sexe[a("sexe_manquant")])) &&
    identical(sc$sexe[a("sexe_hors_modalites")], tolower(sc0$sexe[a("sexe_hors_modalites")])) &&
    identical(j1("date_naissance", "naissance_apres_entree"), j0("date_entree", "naissance_apres_entree") + 1) &&
    identical(j1("date_sortie", "sortie_avant_entree"), j0("date_entree", "sortie_avant_entree") - 1) &&
    identical(j1("date_sortie", "en_cours_avant_fin"), j0("date_entree", "en_cours_avant_fin")) &&
    identical(sc$date_naissance[a("age_aberrant")], as.Date(sprintf("%04d-01-01", an_e - 150L))) &&
    identical(j1("date_naissance", "tete_naissance_incoherente"), j0("date_naissance", "tete_naissance_incoherente") - 1) &&
    identical(sc$sexe[a("tete_sexe_incoherent")], ifelse(sc0$sexe[a("tete_sexe_incoherent")] == "H", "F", "H")) &&
    all(sc$cause_sortie[a("tete_observee_apres_deces")] == "en_cours") &&
    all(is.na(sc$date_sortie[a("tete_observee_apres_deces")])) &&
    all(is.na(sc$cause_sortie[comb]) & is.na(sc$sexe[comb])) && length(comb) == 10L &&
    identical(as.vector(table(lg$defaut[lg$declaration == length(DECL_CAT)])), c(5L, 5L)) &&
    length(t_obs$ligne) > 0L
})
# Lignes effectivement modifiées, établies par comparaison au jeu propre
# (valeur par valeur, NA compris), et non lues dans l'attribut.
modifiees <- function(jeu, jeu0, lignes) {
  egal <- Reduce(`&`, lapply(names(jeu0), function(v) {
    a <- jeu[[v]][lignes]; b <- jeu0[[v]][lignes]
    e <- (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
    e & !is.na(e)
  }))
  lignes[!egal]
}
verifier("défauts : lignes du jeu propre effectivement modifiées = lignes de rôle alteree de l'attribut (identité exacte) ; decomptes$lignes_alterees = modifiées + copies, < lignes_en_defaut",
         identical(modifiees(sc, sc0, seq_len(N0)), sort(unique(lg$ligne[lg$role == "alteree"]))) &&
           idf$decomptes$lignes_alterees == length(modifiees(sc, sc0, seq_len(N0))) + 5L &&
           idf$decomptes$lignes_alterees < idf$decomptes$lignes_en_defaut)
# Défauts de tête (spécification, § 2.2) : toutes les lignes de la tête
# déclarées ; seule modifiée (comparaison au jeu propre) la dernière ligne, ou
# pour tete_observee_apres_deces la dernière autre que la première à décès,
# dont la date est premier_deces.
tetes_conformes <- function(jeu, jeu0, lt) {
  all(vapply(split(lt, lt$declaration * 1e6 + jeu0$id_tete[lt$ligne]), function(x) {
    li <- which(jeu0$id_tete == jeu0$id_tete[x$ligne[1L]])
    p <- li[jeu0$cause_sortie[li] == "deces"][1L]
    obs <- x$defaut[1L] == "tete_observee_apres_deces"
    l_att <- if (obs) max(li[li != p]) else max(li)
    length(li) >= 2L && identical(x$ligne, li) && identical(modifiees(jeu, jeu0, li), l_att) &&
      identical(x$ligne[x$role == "alteree"], l_att) &&
      (if (obs) all(x$premier_deces == jeu0$date_sortie[p]) else all(is.na(x$premier_deces)))
  }, logical(1L)))
}
verifier("défauts de tête, jeu catalogue : lignes déclarées, ligne modifiée et premier_deces conformes à la spécification (§ 2.2)",
         tetes_conformes(sc, sc0, lg[lg$unite == "tete", , drop = FALSE]) &&
           length(unique(lg$declaration[lg$unite == "tete"])) == 3L)
verifier("défauts de tête, têtes à 3 lignes (age_exact_multi, loi_contrats = (0, 0, 1), 80 ans) : dernière ligne autre que la première à décès, dernière ligne", {
  px3 <- modifyList(PF_X, list(mode = "age_exact_multi", n = 2000, age = 80, loi_contrats = c(0, 0, 1)))
  d3 <- list(graine = GRAINE_DEF, declarations = list(list(type = "tete_observee_apres_deces", taux = 1),
                                                      list(type = "tete_naissance_incoherente", taux = 0.1)))
  j0 <- simuler(loi(), px3, PER_X2, GRAINE); j1 <- simuler(loi(), px3, PER_X2, GRAINE, defauts = d3)
  l <- attr(j1, "defauts")$lignes
  all(tabulate(j0$id_tete) == 3L) && all(table(l$declaration) > 0L) && length(unique(l$declaration)) == 2L &&
    tetes_conformes(j1, j0, l)
})
cat(sprintf("  jeu catalogue (uniforme_multi, 5 000 têtes, graine %d, graine des défauts %d) : %d lignes propres, %d copies, %d lignes en défaut ; %.1f s\n",
            GRAINE, GRAINE_DEF, N0, idf$decomptes$copies, idf$decomptes$lignes_en_defaut, duree_cat))

# Critère 3 : confrontation au module donnees (vérification, non source de l'attente).
VD <- list(depart = "vivant", arrivee = "deces")
attendus_par_motif <- lapply(stats::setNames(TOUS_MOTIFS, TOUS_MOTIFS), function(m)
  sort(unique(lg$ligne[lg$motif_attendu %in% m])))
rp <- preparer_donnees(sc, ROLES_SIM, VD, PER, 0, "sexe", TOUS_MOTIFS)
verifier("défauts (critère 3) : toutes exclusions déclarées -> lignes_exclues de preparer_donnees() = lignes de l'attribut, motif par motif (20 motifs)",
         identical(names(rp$resultat$lignes_exclues), TOUS_MOTIFS) &&
           all(vapply(TOUS_MOTIFS, function(m) identical(rp$resultat$lignes_exclues[[m]], attendus_par_motif[[m]]),
                      logical(1L))) &&
           rp$resultat$nombre_lignes_exclues == idf$decomptes$lignes_en_defaut)
verifier("défauts (critère 3) : chaque motif du catalogue exercé (17 motifs non vides, 3 hors périmètre vides)",
         identical(names(attendus_par_motif)[lengths(attendus_par_motif) > 0L],
                   TOUS_MOTIFS[TOUS_MOTIFS %in% CATALOGUE_DEFAUTS$motif_attendu]) &&
           sum(lengths(attendus_par_motif) > 0L) == 17L)
verifier("défauts (critère 3) : aucune exclusion -> refus ligne_invalide, details$lignes = lignes de l'attribut, motif par motif", {
  e <- tryCatch(preparer_donnees(sc, ROLES_SIM, VD, PER, 0, "sexe", character(0)), refus_tablexp = function(e) e)
  inherits(e, "refus_tablexp") && identical(e$motif, "ligne_invalide") &&
    identical(e$details$lignes, attendus_par_motif[lengths(attendus_par_motif) > 0L])
})
verifier("défauts : construire_table() accepte le jeu altéré (attribut defauts), exclusions des motifs attendus déclarées", {
  x <- construire_table(sc, ROLES_SIM, list(transition = VD, periode = PER, delai_declaration = 0, segmentation = "sexe",
    etapes = list(donnees = list(exclusions = TOUS_MOTIFS), exposition = list(convention = "plus_fine", par_annee = FALSE),
                  brut = list(estimateurs = "poisson", niveau = 0.95))))
  inherits(x, "construction") && x$etapes$donnees$resultat$nombre_lignes_exclues == idf$decomptes$lignes_en_defaut
})
verifier("défauts (critère 3) : jeu à part, date_hors_domaine -> naissance 0000-12-31, refus date_hors_domaine sur les lignes de l'attribut", {
  sh <- simuler(loi(), PF_CAT, PER, GRAINE,
                defauts = list(graine = GRAINE_DEF, declarations = list(list(type = "date_hors_domaine", cible = TOUTES,
                                                                              nombre = 5))))
  l <- attr(sh, "defauts")$lignes
  e <- tryCatch(preparer_donnees(sh, ROLES_SIM, VD, PER, 0, "sexe", TOUS_MOTIFS), refus_tablexp = function(e) e)
  nrow(l) == 5L && all(is.na(l$motif_attendu)) && all(l$refus_attendu == "date_hors_domaine") &&
    all(unclass(sh$date_naissance[l$ligne]) == -719163) &&
    with(as.POSIXlt(as.Date(-719163)), year + 1900L == 0L && mon == 11L && mday == 31L) &&
    inherits(e, "refus_tablexp") && identical(e$motif, "date_hors_domaine") &&
    identical(e$details$lignes, list(date_naissance = l$ligne))
})
verifier("défauts : reproductibilité, même appel identical()",
         identical(sc, simuler(loi(), PF_CAT, PER, GRAINE, defauts = DEF_CAT)))
verifier("défauts : combinaison saisie dans l'autre ordre -> même jeu, même attribut hors parametre (types rangés dans l'ordre du catalogue)", {
  inv <- DEF_CAT; k <- length(inv$declarations)
  inv$declarations[[k]]$type <- rev(inv$declarations[[k]]$type)
  si <- simuler(loi(), PF_CAT, PER, GRAINE, defauts = inv)
  a <- attr(si, "defauts")
  identical(a$parametre, inv) && identical(a[names(a) != "parametre"], idf[names(idf) != "parametre"]) &&
    identical(unclass(si)[seq_along(si)], unclass(sc)[seq_along(sc)]) &&
    identical(a$decomptes$declarations$defaut[k], "cause_manquante + sexe_manquant")
})
verifier("défauts : ecrire_rapport() rend le rapport figé d'une construction issue du jeu à défauts (exclusions déclarées)", {
  x <- construire_table(sc, ROLES_SIM, list(transition = VD, periode = PER, delai_declaration = 0, segmentation = "sexe",
    etapes = list(donnees = list(exclusions = TOUS_MOTIFS), exposition = list(convention = "plus_fine", par_annee = FALSE),
                  brut = list(estimateurs = "poisson", niveau = 0.95))))
  o <- ecrire_rapport(x)
  is.raw(o) && length(o) > 0L
})
verifier("défauts : declarations vide admise, jeu identical() au jeu propre hors attribut defauts", {
  r <- simuler(loi(), PF_M_1E3, PER, GRAINE, defauts = list(graine = GRAINE_DEF, declarations = list()))
  a <- attr(r, "defauts"); attr(r, "defauts") <- NULL
  identical(r, simuler(loi(), PF_M_1E3, PER, GRAINE)) && nrow(a$lignes) == 0L
})

# Critère 5 : refus defauts_invalides (avant tout tirage) et defauts_ineligibles.
D1 <- list(type = "sexe_manquant", cible = "deces", taux = 0.5)
OCTETS <- "d\u00e9c"; Encoding(OCTETS) <- "bytes"
DEFAUTS_INVALIDES <- list(
  list("defauts non liste", 1),
  list("defauts data.frame", data.frame(graine = 1L)),
  list("defauts de classe", structure(list(graine = 1L, declarations = list()), class = "x")),
  list("graine manquante", list(declarations = list(D1))),
  list("champ inconnu", list(graine = 1L, declarations = list(D1), seed = 2L)),
  list("noms en double", list(graine = 1L, graine = 2L)),
  list("champ au nom non ASCII", list(graine = 1L, declarations = list(D1), "défauts" = 1)),
  list("graine non entière", list(graine = 1.5, declarations = list(D1))),
  list("graine NA", list(graine = NA_integer_, declarations = list(D1))),
  list("graine caractère", list(graine = "1", declarations = list(D1))),
  list("graine nommée", list(graine = c(g = 1L), declarations = list(D1))),
  list("graine hors entier R", list(graine = 2^31, declarations = list(D1))),
  list("declarations nommées", list(graine = 1L, declarations = list(a = D1))),
  list("declarations non liste", list(graine = 1L, declarations = "sexe_manquant")),
  list("déclaration non liste", list(graine = 1L, declarations = list("sexe_manquant"))),
  list("type inconnu", list(graine = 1L, declarations = list(list(type = "doublon", cible = "deces", taux = 0.1)))),
  list("type manquant", list(graine = 1L, declarations = list(list(cible = "deces", taux = 0.1)))),
  list("type NA", list(graine = 1L, declarations = list(list(type = NA_character_, cible = "deces", taux = 0.1)))),
  list("combinaison non admise", list(graine = 1L, declarations = list(
    list(type = c("sexe_manquant", "entree_manquante"), cible = "deces", taux = 0.1)))),
  list("combinaison en double", list(graine = 1L, declarations = list(
    list(type = c("sexe_manquant", "sexe_manquant"), cible = "deces", taux = 0.1)))),
  list("cible manquante (type de ligne)", list(graine = 1L, declarations = list(list(type = "sexe_manquant", taux = 0.1)))),
  list("cible interdite (type de tête)", list(graine = 1L, declarations = list(
    list(type = "tete_sexe_incoherent", cible = "deces", nombre = 1)))),
  list("cible inconnue", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = "terme", taux = 0.1)))),
  list("cible vide", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = character(0), taux = 0.1)))),
  list("cible en double", list(graine = 1L, declarations = list(
    list(type = "sexe_manquant", cible = c("deces", "deces"), taux = 0.1)))),
  list("taux et nombre", list(graine = 1L, declarations = list(
    list(type = "sexe_manquant", cible = "deces", taux = 0.1, nombre = 1)))),
  list("ni taux ni nombre", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = "deces")))),
  list("taux > 1", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = "deces", taux = 1.5)))),
  list("taux NA", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = "deces", taux = NA_real_)))),
  list("nombre négatif", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = "deces", nombre = -1)))),
  list("nombre non entier", list(graine = 1L, declarations = list(list(type = "sexe_manquant", cible = "deces", nombre = 1.5)))),
  list("champ inconnu d'une déclaration", list(graine = 1L, declarations = list(
    list(type = "sexe_manquant", cible = "deces", taux = 0.1, poids = 1)))),
  list("type en octets invalides", list(graine = 1L, declarations = list(
    list(type = "\xff\xfe", cible = "deces", taux = 0.1)))),
  list("type d'encodage bytes (non cité dans le message)", list(graine = 1L, declarations = list(
    list(type = OCTETS, cible = "deces", taux = 0.1)))),
  list("combinaison avec un type d'encodage bytes", list(graine = 1L, declarations = list(
    list(type = c("sexe_manquant", OCTETS), cible = "deces", taux = 0.1)))),
  list("champ de defauts au nom d'encodage bytes", stats::setNames(list(1L, list(D1)), c("graine", OCTETS))),
  list("champ d'une déclaration au nom d'encodage bytes", list(graine = 1L, declarations = list(
    stats::setNames(list("sexe_manquant", "deces", 0.1), c("type", "cible", OCTETS))))),
  # Cible disjointe par construction des causes admises par le catalogue
  # (complément de spécification d'actuary, 25/10/2026), taux ou nombre.
  list("sortie_manquante, cible en_cours (vide par construction), taux", list(graine = 1L, declarations = list(
    list(type = "sortie_manquante", cible = "en_cours", taux = 0.5)))),
  list("sortie_manquante, cible en_cours (vide par construction), nombre = 1", list(graine = 1L, declarations = list(
    list(type = "sortie_manquante", cible = "en_cours", nombre = 1)))),
  list("sortie_manquante, cible en_cours (vide par construction), nombre = 0", list(graine = 1L, declarations = list(
    list(type = "sortie_manquante", cible = "en_cours", nombre = 0)))),
  list("en_cours_avant_fin, cible deces (vide par construction), taux", list(graine = 1L, declarations = list(
    list(type = "en_cours_avant_fin", cible = "deces", taux = 0.5)))),
  list("en_cours_avant_fin, cible deces (vide par construction), nombre = 1", list(graine = 1L, declarations = list(
    list(type = "en_cours_avant_fin", cible = "deces", nombre = 1)))),
  list("en_cours_avant_fin, cible deces (vide par construction), nombre = 0", list(graine = 1L, declarations = list(
    list(type = "en_cours_avant_fin", cible = "deces", nombre = 0)))))
for (k in DEFAUTS_INVALIDES) {
  set.seed(21L); avant <- globalenv()$.Random.seed
  verifier(sprintf("défauts : %s -> refus defauts_invalides, avant tout tirage (graine du jeu invalide non atteinte), aléa de l'appelant inchangé",
                   k[[1]]),
           leve_refus(simuler(loi(), PF_M_1E3, PER, 1.5, defauts = k[[2]]), "defauts_invalides") &&
             leve_refus(simuler(loi(), PF_M_1E3, PER, GRAINE, defauts = k[[2]]), "defauts_invalides") &&
             identical(globalenv()$.Random.seed, avant))
}
verifier("défauts : ordre des refus figé, graine du jeu invalide et défauts invalides -> defauts_invalides ; graine du jeu invalide et défauts valides -> graine_invalide", {
  d_inv <- list(graine = 1L, declarations = list(list(type = "sortie_manquante", cible = "en_cours", nombre = 0)))
  isTRUE(leve_refus(simuler(loi(), PF_M_1E3, PER, 1.5, defauts = d_inv), "defauts_invalides")) &&
    isTRUE(leve_refus(simuler(loi(), PF_M_1E3, PER, 1.5, defauts = list(graine = 1L, declarations = list(D1))),
                      "graine_invalide")) &&
    grepl("cible « en_cours » disjointe des causes admises pour sortie_manquante (« deces », « rachat »)",
          message_refus(simuler(loi(), PF_M_1E3, PER, GRAINE, defauts = d_inv)), fixed = TRUE)
})
verifier("défauts : graine des défauts égale à celle du jeu (double puis integer) -> refus defauts_invalides, texte et détails d'actuary", {
  e1 <- tryCatch(simuler(loi(), PF_M_1E3, PER, GRAINE, defauts = list(graine = as.double(GRAINE), declarations = list(D1))),
                 refus_tablexp = function(e) e)
  e2 <- tryCatch(simuler(loi(), PF_M_1E3, PER, as.double(GRAINE), defauts = list(graine = GRAINE, declarations = list(D1))),
                 refus_tablexp = function(e) e)
  all(vapply(list(e1, e2), function(e) inherits(e, "refus_tablexp") && identical(e$motif, "defauts_invalides") &&
    grepl("La graine des défauts doit différer de la graine du jeu : avec la même graine", conditionMessage(e), fixed = TRUE) &&
    identical(names(e$details), c("parametre", "graine", "graine_defauts")) &&
    identical(e$details$parametre, "defauts$graine"), logical(1L))) &&
    identical(e1$details$graine_defauts, as.double(GRAINE)) && identical(e2$details$graine, as.double(GRAINE))
})
verifier("défauts : graine des défauts = graine du jeu + 1 admise",
         is.data.frame(simuler(loi(), PF_M_1E3, PER, GRAINE, defauts = list(graine = GRAINE + 1L, declarations = list(D1)))))
verifier("défauts : nombre > éligibles -> refus defauts_ineligibles (après le tirage du jeu propre), aléa de l'appelant inchangé", {
  set.seed(22L); avant <- globalenv()$.Random.seed
  d_tete <- list(graine = GRAINE_DEF, declarations = list(list(type = "tete_sexe_incoherent", nombre = 1)))
  # Vide qui dépend du jeu : aucun rachat tiré (loi de rachat nulle).
  d_ligne <- list(graine = GRAINE_DEF, declarations = list(D1, list(type = "sortie_manquante", cible = "rachat", nombre = 1)))
  isTRUE(leve_refus(simuler(loi(), PF_FORME, PER, GRAINE, defauts = d_tete), "defauts_ineligibles")) &&
    isTRUE(leve_refus(simuler(loi(rachat = NUL), PF_M_1E3, PER, GRAINE, defauts = d_ligne), "defauts_ineligibles")) &&
    grepl("déclaration 2 (sortie_manquante) : nombre = 1 dépasse le nombre d'unités éligibles (0)",
          message_refus(simuler(loi(rachat = NUL), PF_M_1E3, PER, GRAINE, defauts = d_ligne)), fixed = TRUE) &&
    identical(globalenv()$.Random.seed, avant)
})
verifier("défauts : vide qui dépend du jeu, en taux -> admis, 0 unité retenue tracée dans decomptes", {
  r <- simuler(loi(rachat = NUL), PF_M_1E3, PER, GRAINE, defauts = list(graine = GRAINE_DEF, declarations = list(
    list(type = "sortie_manquante", cible = "rachat", taux = 0.5))))
  dd <- attr(r, "defauts")$decomptes$declarations
  identical(dd$eligibles, 0L) && identical(dd$retenues, 0L) && nrow(attr(r, "defauts")$lignes) == 0L
})

# Critère 4 : jeu à exclusion informative pour #69 (spécification, § 2.6).
# Mode uniforme, 10^5 contrats, loi et portefeuille de jeu_reference(),
# période [2015-01-01, 2019-12-31] ; sexe_manquant sur les lignes à décès
# (taux 0,5) et sur les lignes à rachat ou en cours (taux 0,02). Niveau
# alpha = 10^-3, K = 2 tests, a = alpha / 2 chacun.
# - Décès exclus : conditionnellement au jeu propre, K_D (décès datés dans la
#   période, sexe retiré) ~ Bin(D_w, 0,5) EXACTEMENT ; région bilatérale
#   [qbinom(a/2), qbinom(1 - a/2)], fausse alarme exacte.
# - Exposition exclue : GARDE-FOU à fausse alarme garantie (borne exacte de
#   Hoeffding), de faible puissance (+/- 0,008 sur un taux de 0,02 à environ
#   50 % de puissance, avis d'actuary du 25/10/2026) ; la vérification forte
#   est portée par les identités exactes et le rejeu des tirages.
#   E_e (jours dans la période) = sum_l B_l e_l, B_l
#   Bernoulli indépendants de probabilité p_l ; inégalité de Hoeffding (1963)
#   pour des termes dans [0, e_l] : P(|E_e - sum p_l e_l| >= s) <= a avec
#   s = sqrt(sum e_l^2 ln(2/a) / 2) : BORNE EXACTE de la fausse alarme.
# - Identités exactes entre jeu propre et jeu altéré (moteur, exclusion
#   sexe_manquant déclarée) : écart des décès = K_D ; écart des jours = E_e.
PER_26 <- list(debut = D("2015-01-01"), fin = D("2019-12-31"))
DEF_26 <- list(graine = GRAINE_DEF, declarations = list(
  list(type = "sexe_manquant", cible = "deces", taux = 0.5),
  list(type = "sexe_manquant", cible = c("rachat", "en_cours"), taux = 0.02)))
t0 <- proc.time()[["elapsed"]]
s26_0 <- simuler(loi(), PF, PER_26, GRAINE)
s26 <- simuler(loi(), PF, PER_26, GRAINE, defauts = DEF_26)
plan_26 <- function(ex) list(transition = VD, periode = PER_26, delai_declaration = 0, segmentation = character(0),
                             etapes = list(donnees = list(exclusions = ex),
                                           exposition = list(convention = "plus_fine", par_annee = FALSE),
                                           brut = list(estimateurs = "poisson", niveau = 0.95)))
x26_0 <- construire_table(s26_0, ROLES_SIM, plan_26(character(0)))
x26 <- construire_table(s26, ROLES_SIM, plan_26("sexe_manquant"))
duree_26 <- proc.time()[["elapsed"]] - t0
sel26 <- attr(s26, "defauts")$lignes$ligne
f26 <- fenetres_jours(s26_0, PER_26); e26 <- pmax(0, f26$f - f26$s)
dw26 <- s26_0$cause_sortie == "deces" & !is.na(s26_0$date_sortie) & s26_0$date_sortie >= PER_26$debut &
  s26_0$date_sortie <= PER_26$fin
D_w <- sum(dw26); K_D <- sum(dw26[sel26]); E_e <- sum(e26[sel26])
p26 <- ifelse(s26_0$cause_sortie == "deces", 0.5, 0.02)
A_26 <- 1e-3 / 2
lo26 <- stats::qbinom(A_26 / 2, D_w, 0.5); hi26 <- stats::qbinom(1 - A_26 / 2, D_w, 0.5)
fa26 <- stats::pbinom(lo26 - 1, D_w, 0.5) + stats::pbinom(hi26, D_w, 0.5, lower.tail = FALSE)
EE_e <- sum(p26 * e26); s_h <- sqrt(sum(e26^2) * log(2 / A_26) / 2)
verifier("constat § 2.6 : rejeu des tirages (identité exacte), lignes à décès runif < 0,5 puis lignes à rachat ou en cours runif < 0,02", {
  i_d <- which(s26_0$cause_sortie == "deces"); i_a <- which(s26_0$cause_sortie != "deces")
  u <- tablexp:::sous_graine(GRAINE_DEF, list(stats::runif(length(i_d)), stats::runif(length(i_a))))
  identical(sel26, c(i_d[u[[1]] < 0.5], i_a[u[[2]] < 0.02])) && all(is.na(s26$sexe[sel26])) &&
    all(attr(s26, "defauts")$lignes$motif_attendu == "sexe_manquant")
})
ligne <- sprintf(paste("constat de simulation (exact) § 2.6, décès exclus : K_D = %d dans [%d, %d] (D_w = %d décès datés",
                       "dans la période, p = 0,5, a = %g, fausse alarme exacte %.4g ; graines %d et %d)"),
                 K_D, lo26, hi26, D_w, A_26, fa26, GRAINE, GRAINE_DEF)
cat("  ", ligne, "\n", sep = "")
verifier(ligne, K_D >= lo26 && K_D <= hi26 && fa26 <= A_26)
ligne <- sprintf(paste("constat de simulation § 2.6, exposition exclue, garde-fou à fausse alarme garantie (borne exacte de",
                       "Hoeffding, faible puissance ; vérification forte : identités et rejeu) : E_e = %.0f j, |E_e - %.1f|",
                       "= %.1f <= s = %.1f j (fausse alarme <= a = %g)"),
                 E_e, EE_e, abs(E_e - EE_e), s_h, A_26)
cat("  ", ligne, "\n", sep = "")
verifier(ligne, abs(E_e - EE_e) <= s_h)
g26 <- function(x) c(D = sum(x$etapes$brut$resultat$poisson$evenements),
                     E = sum(x$etapes$brut$resultat$poisson$exposition_centrale),
                     J = sum(x$etapes$exposition$resultat$exposition$exposition_jours))
a0 <- g26(x26_0); a1 <- g26(x26)
verifier("constat § 2.6 (identités exactes) : décès propre - altéré = K_D ; jours propre - altéré = E_e ; lignes exclues = lignes altérées",
         a0[["D"]] - a1[["D"]] == K_D && a0[["J"]] - a1[["J"]] == E_e &&
           x26$etapes$donnees$resultat$nombre_lignes_exclues == length(sel26) &&
           identical(x26$etapes$donnees$resultat$lignes_exclues$sexe_manquant, sort(sel26)))
cat(sprintf(paste("  constat § 2.6, informatif : %d lignes altérées sur %d (%.2f %%) ; décès exclus %.1f %% (région [%.1f %%, %.1f %%]) ;",
                  "exposition exclue %.2f %% (attendue %.2f %% +/- %.2f pt) ; taux brut global %.5f -> %.5f ; %.1f s\n"),
            length(sel26), nrow(s26_0), 100 * length(sel26) / nrow(s26_0), 100 * K_D / D_w, 100 * lo26 / D_w,
            100 * hi26 / D_w, 100 * E_e / sum(e26), 100 * EE_e / sum(e26), 100 * s_h / sum(e26),
            a0[["D"]] / a0[["E"]], a1[["D"]] / a1[["E"]], duree_26))
