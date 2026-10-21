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
