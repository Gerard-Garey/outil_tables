# -----------------------------------------------------------------------------
#  R/simulation.R : module `simulation`, simulateur minimal (issue #10 ;
#  E2.19, E2.20 ; ADR 0003, point 9 ; ADR 0007, points 3.2, 7 et 8 ;
#  spécification d'actuary, A.0 et A.2)
#
#  - table_gompertz_makeham(), table_constante() : tables synthétiques à
#    paramètres déclarés (classe `table_reference`, statut `synthetique`,
#    attribut `parametres`) ;
#  - loi_effective() : loi vraie effective par transition et par sexe,
#    q_x^s = min(f_s q_x, 1), force mu_x = -ln(1 - q_x) ;
#  - jours_civils(), jour_anniversaire(), age_anniversaire() : dates
#    d'anniversaire et âge au dernier anniversaire (T2) en arithmétique
#    entière, sans conversion as.POSIXlt par tête ;
#  - temps_latent() : inversion du risque cumulé d'une transition ;
#  - simuler_portefeuille() : point d'entrée (exporté) ; lignes individuelles
#    sous la loi vraie, deux transitions concurrentes (vivant -> décédé, en
#    cours -> racheté), troncature à gauche (entrée après la naissance,
#    éventuellement avant le début de la période), censure par la fin de la
#    période ; loi vraie, paramètres, graine et générateur consignés dans
#    l'attribut `simulation`.
#
#  Hypothèse de répartition dans l'année (A.2) : force constante par âge
#  entier, dans l'échelle des années d'âge (A.0, T3 : une année d'âge de
#  L_x = anniv(x+1) - anniv(x) jours vaut 1) ; risques concurrents
#  indépendants. Modes `uniforme` et `age_exact` : une tête = un contrat.
#  Modes `uniforme_multi` et `age_exact_multi` (#92 ; ADR 0007, annotation
#  du 25/10/2026, (c), points 1 à 5) : K contrats tirés par tête, K de loi
#  déclarée, indépendant de la mortalité et du rachat ; sexe, naissance et
#  décès par tête, décès tiré à partir de la première entrée ; entrée et
#  rachat par contrat ; un contrat dont l'entrée suit l'instant de décès de
#  la tête n'est pas écrit (contrat non souscrit).
#  Défauts paramétrés (#93 ; ADR 0007, annotation du 25/10/2026, (c), points
#  6 à 9) : argument `defauts` ; altérations déterministes de lignes ou de
#  têtes du jeu propre, tirées après lui sous leur propre graine, consignées
#  dans l'attribut `defauts` avec le motif ou le refus attendu, lus dans le
#  catalogue CATALOGUE_DEFAUTS (jamais calculés par un contrôle de `donnees` :
#  non-circularité, annotation, (d)).
#  Aléa : tous les tirages sous sous_graine() (outils), dans l'ordre fixé par
#  la spécification, qui fait partie du contrat de reproductibilité.
#  Ce module n'appelle que `donnees` et `outils` ; aucun module du moteur ne
#  l'appelle (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# Transitions simulées : cause de sortie produite -> transition (A.2).
CAUSES_SIMULEES <- c(deces = "vivant -> deces", rachat = "en_cours -> rachat")

# Hypothèse de répartition consignée dans le résultat (A.2).
HYPOTHESE_REPARTITION <- "force constante par âge entier ; risques concurrents indépendants"

# Ordre des tirages (A.2), consigné : il fait partie du contrat.
ORDRE_TIRAGES <- c("1. sexe : runif(n) < proportion_hommes",
                   "2. jour d'entrée : sample.int(nombre de jours de [origine, fin], n, replace = TRUE) (mode uniforme)",
                   "3. âge à l'entrée : age_min + (age_max - age_min) * runif(n) (mode uniforme)",
                   "4. décès : rexp(n)",
                   "5. rachat : rexp(n)")

# Ordre des tirages des modes multi-contrats (#92 ; ADR 0007, annotation du
# 25/10/2026, (c), point 3), consigné de même ; ORDRE_TIRAGES, celui des
# modes existants, est inchangé. N = nombre de contrats tirés = sum(K).
ORDRE_TIRAGES_MULTI <- list(
  uniforme_multi = c(
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
    "4. rachat, par contrat tiré (souscrit ou non) : rexp(N)"))

# Champs du portefeuille, par mode : tous requis, aucun défaut implicite.
# Dans tous les modes, n est le nombre de têtes.
CHAMPS_PORTEFEUILLE <- list(
  uniforme = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine"),
  age_exact = c("mode", "n", "proportion_hommes", "age", "date_entree"),
  uniforme_multi = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine", "loi_contrats"),
  age_exact_multi = c("mode", "n", "proportion_hommes", "age", "date_entree", "loi_contrats"))

# Mode de base d'un mode multi-contrats : mêmes champs, mêmes contrôles, mêmes
# bornes d'âge.
MODE_BASE <- c(uniforme = "uniforme", age_exact = "age_exact",
               uniforme_multi = "uniforme", age_exact_multi = "age_exact")

# Catalogue des défauts paramétrés (#93 ; spécification d'actuary du
# 25/10/2026, § 2.2 ; ADR 0007, annotation du 25/10/2026, (c), point 6) : un
# type par ligne. unite : ligne ou tête ; deces, rachat, en_cours : causes de
# sortie (du jeu propre) des lignes éligibles, à intersecter avec la cible de
# la déclaration (types de ligne) ; condition : éligibilité supplémentaire
# (entree_avant_fin : entrée < fin de la période ; tete_multi : tête d'au
# moins deux lignes ; tete_multi_deces : idem, dont au moins une à décès ;
# une tête n'est éligible que si aucune de ses lignes n'est déjà altérée) ;
# alteration : altération déterministe ; motif_attendu : motif que le module
# `donnees` doit lever (NA : aucun, refus à la place) ; refus_attendu : refus
# attendu sans règle d'exclusion déclarée pour ce motif. Les attendus sont
# écrits ici, en dur, d'après la spécification : jamais calculés par un
# contrôle de `donnees` (non-circularité, annotation, (d)). Hors périmètre
# (M136) : annee_naissance_incoherente, segment_manquant,
# tete_segment_incoherent (aucune colonne annee_naissance ni segment).
CATALOGUE_DEFAUTS <- data.frame(
  defaut = c("contrat_duplique", "id_contrat_manquant", "id_tete_manquant", "naissance_manquante",
             "entree_manquante", "sortie_manquante", "cause_manquante", "cause_hors_modalites",
             "sexe_manquant", "sexe_hors_modalites", "naissance_apres_entree", "sortie_avant_entree",
             "en_cours_avant_fin", "age_aberrant", "date_hors_domaine",
             "tete_naissance_incoherente", "tete_sexe_incoherent", "tete_observee_apres_deces"),
  unite = c(rep("ligne", 15L), rep("tete", 3L)),
  deces    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE,  FALSE, TRUE, TRUE,
               TRUE, TRUE, TRUE),
  rachat   = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE,  FALSE, TRUE, TRUE,
               TRUE, TRUE, TRUE),
  en_cours = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,  TRUE, TRUE,
               TRUE, TRUE, TRUE),
  condition = c(rep("", 12L), "entree_avant_fin", "", "", "tete_multi", "tete_multi", "tete_multi_deces"),
  alteration = c("copie de la ligne ajoutée en fin de jeu (lignes N + 1, ...)",
                 "id_contrat <- NA", "id_tete <- NA", "date_naissance <- NA", "date_entree <- NA",
                 "date_sortie <- NA", "cause_sortie <- NA", "cause_sortie <- toupper(cause_sortie)",
                 "sexe <- NA", "sexe <- tolower(sexe)", "date_naissance <- date_entree + 1",
                 "date_sortie <- date_entree - 1", "date_sortie <- date_entree",
                 "date_naissance <- 1er janvier de (année d'entrée - 150)", "date_naissance <- 0000-12-31",
                 "date_naissance de la dernière ligne de la tête <- date_naissance - 1",
                 "sexe de la dernière ligne de la tête inversé (H <-> F)",
                 paste("dernière ligne de la tête autre que sa première ligne à décès : cause_sortie <- en_cours,",
                       "date_sortie <- NA")),
  motif_attendu = c("contrat_duplique", "id_contrat_manquant", "id_tete_manquant", "naissance_manquante",
                    "entree_manquante", "sortie_manquante", "cause_manquante", "cause_hors_modalites",
                    "sexe_manquant", "sexe_hors_modalites", "naissance_apres_entree", "sortie_avant_entree",
                    "en_cours_avant_fin", "age_aberrant", NA,
                    "tete_naissance_incoherente", "tete_sexe_incoherent", "tete_observee_apres_deces"),
  refus_attendu = c(rep("ligne_invalide", 14L), "date_hors_domaine", rep("ligne_invalide", 3L)),
  stringsAsFactors = FALSE)

# Combinaisons de types admises sur une même ligne (spécification, § 2.4 ;
# #57) : liste fermée ; une combinaison nouvelle demande l'avis d'actuary.
# Une déclaration de combinaison porte le vecteur des types (dans un ordre
# quelconque) ; chaque ligne retenue reçoit toutes ses altérations.
COMBINAISONS_DEFAUTS <- list(c("cause_manquante", "sexe_manquant"))

# Ordre des tirages des défauts (spécification, § 2.3 ; ADR 0007, annotation
# du 25/10/2026, (c), point 7, et réponse d'actuary à la question (f)),
# consigné dans l'attribut `defauts`.
ORDRE_TIRAGES_DEFAUTS <- c(
  paste("0. après le tirage complet du jeu propre, un seul sous_graine(defauts$graine), graine différente",
        "de celle du jeu ; déclarations traitées dans l'ordre reçu"),
  paste("1. éligibles : unités non encore altérées, en ordre croissant (lignes du jeu propre ; têtes dont",
        "aucune ligne n'est altérée) ; une tête retenue rend toutes ses lignes inéligibles ensuite"),
  "2. taux : unités retenues = éligibles[runif(nombre d'éligibles) < taux]",
  "3. nombre : unités retenues = sort(éligibles[sample.int(nombre d'éligibles, nombre)])")

# --- tables synthétiques ------------------------------------------------------

# Table de Gompertz-Makeham, mu(x) = A + B c^x, sans distinction de sexe :
# q_x = 1 - exp(-A - B c^x (c - 1) / ln c), intégrale exacte de mu sur
# [x, x+1) (A.2). Paramètres déclarés, sans valeur par défaut.
table_gompertz_makeham <- function(A, B, c, ages) {
  reel <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v)
  if (!reel(A) || !reel(B) || !reel(c) || A < 0 || B < 0 || c <= 1)
    refuser("simulation", "loi_invalide",
            "Gompertz-Makeham : A >= 0, B >= 0 et c > 1, réels finis, à déclarer.",
            list(A = A, B = B, c = c))
  if (!is.numeric(ages) || !length(ages) || anyNA(ages) || any(ages != trunc(ages)) ||
      any(ages < 0))
    refuser("simulation", "loi_invalide",
            "Gompertz-Makeham : les âges sont des entiers >= 0.", list(ages = ages))
  ages <- as.integer(ages)
  qx <- 1 - exp(-A - B * c^ages * (c - 1) / log(c))
  table_reference(data.frame(age = ages, sexe = "ensemble", qx = qx, stringsAsFactors = FALSE),
                  nom = sprintf("Gompertz-Makeham synthétique (A = %s, B = %s, c = %s)",
                                format(A), format(B), format(c)),
                  statut = "synthetique",
                  source = paste("Table synthétique : mu(x) = A + B c^x ;",
                                 "q_x = 1 - exp(-A - B c^x (c - 1) / ln c) (spécification d'actuary, A.2, #10)"),
                  parametres = list(loi = "gompertz_makeham", A = A, B = B, c = c,
                                    ages = range(ages)))
}

# Table à q_x constant sur les âges `ages`, sans distinction de sexe (loi de
# rachat de la branche, A.2 ; q = 0 pour une transition absente).
table_constante <- function(q, ages, nom) {
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q < 0 || q > 1)
    refuser("simulation", "loi_invalide", "Table constante : q réel fini dans [0, 1].", list(q = q))
  if (!is.numeric(ages) || !length(ages) || anyNA(ages) || any(ages != trunc(ages)) ||
      any(ages < 0))
    refuser("simulation", "loi_invalide", "Table constante : les âges sont des entiers >= 0.",
            list(ages = ages))
  ages <- as.integer(ages)
  table_reference(data.frame(age = ages, sexe = "ensemble", qx = rep(as.numeric(q), length(ages)),
                             stringsAsFactors = FALSE),
                  nom = nom, statut = "synthetique",
                  source = "Table synthétique à q_x constant (spécification d'actuary, A.2, #10)",
                  parametres = list(loi = "constante", q = q, ages = range(ages)))
}

# --- loi vraie effective --------------------------------------------------------

# Loi d'une transition : list(table = <table_reference>, facteurs = c(H =,
# F =)). Pour chaque sexe de `sexes` (les sexes tirés, de proportion non
# nulle), la table du sexe s'il y figure, sinon la table `ensemble` ;
# q_x^s = min(f_s q_x, 1). Un sexe non tiré n'est ni construit ni contrôlé.
# Rend un data.frame (cause, sexe, age, qx, mu) trié par sexe puis âge ;
# mu = -ln(1 - q_x), Inf si q_x = 1.
loi_effective <- function(loi_t, cause, sexes) {
  if (!liste_aux_noms(loi_t, c("facteurs", "table")))
    refuser("simulation", "loi_invalide",
            sprintf("Loi « %s » : liste (table, facteurs) attendue.", cause), list(cause = cause))
  tab <- loi_t$table; f <- loi_t$facteurs
  if (!inherits(tab, "table_reference"))
    refuser("simulation", "loi_invalide",
            sprintf("Loi « %s » : la table doit être de classe table_reference (ADR 0007, point 7).", cause),
            list(cause = cause))
  if ("annee" %in% names(tab))
    refuser("simulation", "loi_invalide",
            sprintf("Loi « %s » : table prospective (colonne annee) non prise en charge par le simulateur minimal.", cause),
            list(cause = cause))
  # Facteurs : forme exacte (#58, forme_exacte()) : double ou integer, seul
  # attribut names (ni classe, ni dim), de noms exactement H et F, sans tri :
  # setequal() ne dépend pas de la locale et ne lève pas pour un nom non ASCII
  # natif (locale C) ou en octets invalides (sort(method = "radix") lève alors,
  # mesuré) ; des facteurs sans noms, un nom NA ou doublé sont refusés.
  if (!forme_exacte(f, c("double", "integer"), "names") || length(f) != 2L ||
      anyNA(names(f)) || anyDuplicated(names(f)) || !setequal(names(f), c("H", "F")) ||
      any(!is.finite(f)) || any(f < 0))
    refuser("simulation", "loi_invalide",
            sprintf(paste("Loi « %s » : facteurs = c(H = , F = ), réels finis >= 0, de type double ou integer,",
                          "sans autre attribut que les noms."), cause),
            list(cause = cause, facteurs = f))
  morceaux <- lapply(sexes, function(s) {
    t <- if (s %in% tab$sexe) tab[tab$sexe == s, , drop = FALSE] else tab[tab$sexe == "ensemble", , drop = FALSE]
    if (!nrow(t))
      refuser("simulation", "loi_invalide",
              sprintf("Loi « %s » : aucune ligne pour le sexe %s ni pour « ensemble ».", cause, s),
              list(cause = cause, sexe = s))
    t <- t[order(t$age), , drop = FALSE]
    q <- pmin(f[[s]] * t$qx, 1)
    data.frame(cause = cause, sexe = s, age = t$age, qx = q, mu = -log1p(-q),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, morceaux)
}

# --- dates ---------------------------------------------------------------------

# Jours depuis 1970-01-01 de la date civile (an, mois 1-12, jour), calendrier
# grégorien proleptique (algorithme « days from civil », arithmétique
# entière ; vectorisé ; vérifié contre as.Date dans les tests). Implémentation
# volontairement indépendante de celle du module `exposition` (M89, #22
# point 3) : le sens inverse, la décomposition d'un jour en composantes, passe
# par composantes_date() de `donnees`, simple enveloppe de as.POSIXlt ; l'accord
# des trois arithmétiques est vérifié sur [0001-01-01, 9999-12-31]
# (tests/unitaires/test_exposition.R) : exhaustif pour la décomposition et les
# jours civils, partiel (six âges, naissances voisines du 29/02, tirage et
# balayage) pour les anniversaires et les âges.
jours_civils <- function(an, mois, jour) {
  a <- an - (mois <= 2L)
  ere <- a %/% 400L
  aoe <- a - ere * 400L
  jda <- (153L * ((mois + 9L) %% 12L) + 2L) %/% 5L + jour - 1L
  joe <- aoe * 365L + aoe %/% 4L - aoe %/% 100L + jda
  ere * 146097 + joe - 719468
}

# Date (jours) de l'anniversaire d'âge x d'une tête de composantes de
# naissance cn (composantes_date(), mois 0-11) : convention T2, une
# naissance au 29/02 a son anniversaire le 01/03 des années non bissextiles.
jour_anniversaire <- function(cn, x) {
  an <- cn$an + x
  mois <- rep_len(cn$mois + 1L, length(an)); jour <- rep_len(cn$jour, length(an))
  bissextile <- (an %% 4L == 0L & an %% 100L != 0L) | an %% 400L == 0L
  fev29 <- mois == 2L & jour == 29L & !bissextile
  mois[fev29] <- 3L; jour[fev29] <- 1L
  jours_civils(an, mois, jour)
}

# composantes_date() calculée sur les valeurs distinctes seulement (la
# conversion par as.POSIXlt domine le temps de calcul à 10^6 têtes, mesuré).
composantes_distinctes <- function(j) {
  u <- unique(j)
  cu <- composantes_date(u)
  i <- match(j, u)
  list(an = cu$an[i], mois = cu$mois[i], jour = cu$jour[i])
}

# Âge au dernier anniversaire au jour t (vecteur ou scalaire), d'année
# civile an_t : nombre d'anniversaires jour_anniversaire() atteints à t.
# Même convention que age_revolu() (T2), sans nouvelle conversion de dates ;
# l'égalité des deux est vérifiée dans les tests, 29/02 compris.
age_anniversaire <- function(cn, an_t, t) {
  x <- an_t - cn$an
  x - (jour_anniversaire(cn, x) > t)
}

# --- contrôle des entrées ------------------------------------------------------

# Date unique : la forme de date1() de controler_transition_periode() (donnees ;
# #58) : stockage double ou integer, seul attribut la classe "Date" exacte (ni
# noms, ni autre attribut), longueur 1, finie, en jours entiers.
date_unique <- function(x) forme_exacte(x, c("double", "integer"), "class") &&
  identical(class(x), "Date") && length(x) == 1L && !is.na(x) &&
  is.finite(unclass(x)) && unclass(x) == trunc(unclass(x))

controler_portefeuille <- function(portefeuille, periode) {
  if (!liste_aux_noms(periode, c("debut", "fin")) ||
      !date_unique(periode[["debut"]]) || !date_unique(periode[["fin"]]))
    refuser("simulation", "periode_invalide",
            paste("La période d'observation doit être une liste (debut, fin) de deux dates (classe Date,",
                  "sans autre attribut)."),
            list(periode = periode))
  if (periode[["debut"]] > periode[["fin"]])
    refuser("simulation", "periode_invalide",
            sprintf("Période vide : début %s postérieur à la fin %s.", format(periode[["debut"]]), format(periode[["fin"]])),
            list(periode = periode))
  # Forme exacte du portefeuille (#78, forme_exacte()) : liste nue, seul
  # attribut names (ni data.frame, ni classe, ni attribut quelconque). Noms
  # comparés plus bas sans tri (setdiff, anyDuplicated), indépendamment de la
  # locale.
  if (!forme_exacte(portefeuille, "list", "names"))
    refuser("simulation", "portefeuille_invalide",
            paste("Portefeuille : liste nue attendue (ni data.frame, ni classe, aucun attribut",
                  "autre que les noms)."),
            list(classe = class(portefeuille)))
  # Champs lus par [[ ]] (jamais $, qui apparie les noms partiels) ; ici, le
  # mode : chaîne unique de forme exacte (chaine_unique(), #60).
  mode <- portefeuille[["mode"]]
  if (!chaine_unique(mode) || !(mode %in% names(CHAMPS_PORTEFEUILLE)))
    refuser("simulation", "portefeuille_invalide",
            sprintf("Portefeuille : liste avec un champ mode pris dans %s.",
                    liste_guillemets(names(CHAMPS_PORTEFEUILLE))),
            list(mode = mode))
  attendus <- CHAMPS_PORTEFEUILLE[[mode]]
  manquants <- setdiff(attendus, names(portefeuille)); inconnus <- setdiff(names(portefeuille), attendus)
  if (length(manquants) || length(inconnus) || anyDuplicated(names(portefeuille)))
    refuser("simulation", "portefeuille_invalide",
            sprintf("Portefeuille (mode %s) : champs attendus %s ; manquants : %s ; inconnus : %s.", mode,
                    liste_guillemets(attendus),
                    if (length(manquants)) liste_guillemets(manquants) else "aucun",
                    if (length(inconnus)) liste_guillemets(inconnus) else "aucun"),
            list(manquants = manquants, inconnus = inconnus))
  p <- portefeuille
  # Champs numériques : forme exacte (#78), double ou integer sans aucun
  # attribut (ni names, ni dim, ni classe), longueur 1, fini.
  reel <- function(v) forme_exacte(v, c("double", "integer")) && length(v) == 1L && is.finite(v)
  entier <- function(v) reel(v) && v == trunc(v)
  invalide <- function(msg) refuser("simulation", "portefeuille_invalide", paste("Portefeuille :", msg))
  if (!entier(p[["n"]]) || p[["n"]] < 1 || p[["n"]] > .Machine$integer.max) invalide("n entier >= 1, de type double ou integer, sans attribut.")
  if (!reel(p[["proportion_hommes"]]) || p[["proportion_hommes"]] < 0 || p[["proportion_hommes"]] > 1)
    invalide("proportion_hommes réel dans [0, 1], de type double ou integer, sans attribut.")
  fin <- as.numeric(periode[["fin"]])
  if (MODE_BASE[[mode]] == "uniforme") {
    if (!reel(p[["age_min"]]) || !reel(p[["age_max"]]) || p[["age_min"]] * 365.25 < 1 || p[["age_min"]] >= p[["age_max"]])
      invalide(paste("âges réels finis, de type double ou integer, sans attribut ;",
                     "age_min d'au moins un jour (1/365,25) et age_min < age_max."))
    if (!date_unique(p[["origine"]]) || p[["origine"]] > periode[["debut"]])
      invalide(paste("origine : date (classe Date, sans autre attribut) antérieure ou égale",
                     "au début de la période."))
    age_bas <- age_revolu_minimal(round(p[["age_min"]] * 365.25))
    # Borne haute (erratum d'actuary du 17/10/2026, point 4). Soit span(y)
    # le nombre de jours de la naissance à l'anniversaire y (T2) : pour
    # y <= 130 (âges des tables des tests), span(y) >= 365,25 y - 2,75
    # (minimum exact -2,75 mesuré sur les naissances de 1700 à 2150, atteint
    # pour y = 107, naissance au 1796-03-01). Une tête d'âge révolu y à B
    # vérifie span(y) <= B - n, et
    # B - n <= (B - origine) + round(age_max * 365,25)
    # <= (B - origine) + age_max * 365,25 + 0,5 ; donc
    # y <= age_max + (B - origine) / 365,25 + 3,25 / 365,25 < age_haut.
    # Domaine : aucune règle du code ne limite les âges de la table à 130.
    # Au-delà, le minimum mesuré sur les mêmes naissances jusqu'à y = 260 est
    # -3,75 (atteint pour y = 207, naissance au 2096-03-01 ; premier
    # y < -2,75 à y = 204) : la même preuve donne une marge de 4,25 / 365,25,
    # encore inférieure au « + 1 » d'age_haut.
    age_haut <- p[["age_max"]] + (fin + 1 - as.numeric(p[["origine"]])) / 365.25 + 1
  } else {
    if (!entier(p[["age"]]) || p[["age"]] < 1) invalide("âge exact entier >= 1, de type double ou integer, sans attribut.")
    if (!date_unique(p[["date_entree"]]) || p[["date_entree"]] > periode[["fin"]])
      invalide(paste("date_entree : date (classe Date, sans autre attribut) antérieure ou égale",
                     "à la fin de la période."))
    ce <- composantes_date(as.numeric(p[["date_entree"]]))
    if (ce$mois == 1L && ce$jour == 29L)
      refuser("simulation", "entree_29_fevrier",
              "Mode âge exact : une entrée le 29/02 est refusée (la naissance « x années avant » serait ambiguë).",
              list(date_entree = p[["date_entree"]]))
    age_bas <- p[["age"]]
    # Borne haute : même preuve qu'en mode uniforme, avec B - n =
    # (B - date_entree) + span(age) et, pour age <= 130, span(age) <=
    # 365,25 age + 0,75 (maximum exact +0,75 mesuré sur les mêmes
    # naissances) : pour y <= 130, y <= age + (B - date_entree) / 365,25
    # + 3,5 / 365,25 < age_haut. Domaine : au-delà de 130, le maximum mesuré
    # jusqu'à 260 reste +0,75 et le minimum devient -3,75 (mode uniforme) :
    # marge de 4,5 / 365,25, encore inférieure au « + 1 ».
    age_haut <- p[["age"]] + (fin + 1 - as.numeric(p[["date_entree"]])) / 365.25 + 1
  }
  # Modes multi-contrats (#92) : les bornes d'âge restent valables, la
  # naissance étant fixée à la première entrée et les entrées suivantes
  # restant dans [origine, fin] (uniforme_multi) ou égales à date_entree.
  # loi_contrats : probabilités p_1, ..., p_m de K sur {1, ..., m} ; forme
  # exacte double ou integer sans aucun attribut ; p_m > 0 (m est le plus
  # grand nombre tirable) ; somme 1 à 1e-12 près. Loi effectivement tirée :
  # (p_1, ..., p_{m-1}, 1 - F_{m-1}), à moins de 1e-12 de la loi déclarée
  # (approximation numérique).
  if (mode != MODE_BASE[[mode]]) {
    lc <- p[["loi_contrats"]]
    if (!forme_exacte(lc, c("double", "integer")) || !length(lc) || any(!is.finite(lc)) || any(lc < 0) ||
        lc[length(lc)] <= 0 || abs(sum(lc) - 1) > 1e-12)
      invalide(paste("loi_contrats : probabilités p_1, ..., p_m (m >= 1) du nombre de contrats par tête,",
                     "réels finis >= 0, de type double ou integer, sans attribut, p_m > 0, de somme 1",
                     "à 1e-12 près."))
    # Garde de débordement : le nombre de contrats tirés, au plus n x m,
    # est un entier R.
    if (as.numeric(p[["n"]]) * length(lc) > .Machine$integer.max)
      invalide(sprintf("n x length(loi_contrats) (%.0f) dépasse .Machine$integer.max (%d).",
                       as.numeric(p[["n"]]) * length(lc), .Machine$integer.max))
  }
  list(mode = mode, age_bas = age_bas, age_haut = age_haut)
}

# Borne basse exacte de l'âge révolu d'une tête née d_min jours avant son
# entrée (d_min = round(age_min * 365,25), le plus petit écart tirable) :
# y_min = max{y : 365 y + ceiling(y / 4) <= d_min}, y années comptant au plus
# ceiling(y / 4) jours bissextiles. Indépendante de la graine (décision
# d'actuary, reprise C1 de #10) ; y_min(20) = 20, y_min(21) = 20.
age_revolu_minimal <- function(d_min) {
  y <- floor(d_min / 365)
  while (y > 0 && 365 * y + ceiling(y / 4) > d_min) y <- y - 1
  y
}

# Refus si la loi ne couvre pas les âges atteignables [age_bas, age_haut]
# (A.2 : aucune extrapolation) ou y atteint q_x = 1 (force non définie),
# pour les sexes de proportion non nulle.
controler_couverture <- function(loi, age_bas, age_haut, sexes) {
  for (cause in names(CAUSES_SIMULEES)) for (s in sexes) {
    l <- loi[loi$cause == cause & loi$sexe == s, , drop = FALSE]
    if (floor(age_bas) < min(l$age) || floor(age_haut) > max(l$age))
      refuser("simulation", "table_trop_courte",
              sprintf(paste("Loi « %s », sexe %s : la table couvre les âges %d à %d ; âges atteignables",
                            "%d à %d (A.2 : aucune extrapolation)."),
                      cause, s, min(l$age), max(l$age), as.integer(floor(age_bas)),
                      as.integer(floor(age_haut))),
              list(cause = cause, sexe = s, ages_table = range(l$age),
                   ages_atteignables = c(age_bas, age_haut)))
    un <- l$age[l$age >= floor(age_bas) & l$age <= floor(age_haut) & l$qx >= 1]
    if (length(un))
      refuser("simulation", "qx_egal_a_un",
              sprintf("Loi « %s », sexe %s : q_x = 1 à l'âge atteignable %d (force constante non définie).",
                      cause, s, un[1L]),
              list(cause = cause, sexe = s, ages = un))
  }
  invisible(TRUE)
}

# --- temps latents --------------------------------------------------------------

# Instant (jours, continu) et jour de l'événement latent d'une transition,
# pour les têtes `idx` d'un même sexe, par inversion du risque cumulé
# linéaire par morceaux sur l'âge : cible H(a0) + E, E ~ Exp(1), recherche
# par findInterval(). Rend list(instant, jour) ; Inf / NA au-delà de la
# table (n'arrive que hors de la fenêtre observée, controler_couverture()).
# Le jour est anniv(k) + floor(fraction * L_k), borné à [entrée,
# anniv(k+1) - 1] : la borne ne corrige que l'arrondi de la virgule
# flottante, P(événement dans l'année k) est conservée exactement.
temps_latent <- function(l, cn, x0, frac0, entree, e) {
  a_bas <- l$age[1L]
  cumul <- c(0, cumsum(l$mu))                  # risque cumulé aux âges entiers a_bas, a_bas + 1, ...
  j0 <- x0 - a_bas + 1L
  cible <- cumul[j0] + l$mu[j0] * frac0 + e
  i <- findInterval(cible, cumul)
  n <- length(e)
  instant <- rep(Inf, n); jour <- rep(NA_real_, n)
  ok <- which(i < length(cumul))
  if (length(ok)) {
    io <- i[ok]
    fraction <- (cible[ok] - cumul[io]) / l$mu[io]
    k <- a_bas + io - 1L
    cnk <- lapply(cn, `[`, ok)
    ak <- jour_anniversaire(cnk, k)
    lk <- jour_anniversaire(cnk, k + 1L) - ak
    instant[ok] <- ak + fraction * lk
    jour[ok] <- pmax(entree[ok], ak + pmin(floor(fraction * lk), lk - 1))
  }
  list(instant = instant, jour = jour)
}

# --- défauts paramétrés (#93) ---------------------------------------------------

# Contrôle de l'argument `defauts` (non NULL), avant tout tirage ; refus
# defauts_invalides. Forme : liste nue (seul attribut les noms) de champs
# exactement graine et declarations ; graine de la forme exigée par
# sous_graine() (#43), différente de la graine du jeu (réponse d'actuary à la
# question (f), 25/10/2026) ; declarations : liste sans aucun attribut (vide
# admise) de déclarations, chacune liste nue de champs type, cible (types de
# ligne seulement, requise ; interdite pour un type de tête), et exactement
# un de taux (réel de [0, 1]) ou nombre (entier >= 0). type : un type du
# catalogue, ou une combinaison admise (COMBINAISONS_DEFAUTS). Noms comparés
# sans tri (setdiff, anyDuplicated, anyNA ; #100). `graine` : la graine du
# jeu, telle que reçue ; NULL si l'argument manque (le refus graine_invalide
# vient alors de sous_graine()). Rend list(graine, declarations) : graine
# entière ; pour chaque déclaration, list(type, unite, cible, taux, nombre),
# cible = causes du catalogue si absente, taux ou nombre NULL.
controler_defauts <- function(defauts, graine) {
  invalide <- function(msg, details = list())
    refuser("simulation", "defauts_invalides", paste("Défauts :", msg), details)
  # Chaîne de l'utilisateur citée dans un message : une chaîne marquée
  # « bytes » ferait lever sprintf() (« translating strings with "bytes"
  # encoding is not allowed », mesuré sous C.UTF-8) ; elle n'est pas citée.
  montrer <- function(v) {
    v <- as.character(v)
    v[!is.na(v) & Encoding(v) == "bytes"] <- "<chaîne d'encodage bytes>"
    v
  }
  # Noms reçus comparés aux noms admis, sans tri ; rend la chaîne d'écart, ou "".
  ecart_noms <- function(x, requis, admis) {
    n <- names(x)
    if (is.null(n)) n <- rep("", length(x))
    n <- as.character(n)
    if (anyNA(n) || anyDuplicated(n)) return("noms manquants ou en double")
    manquants <- setdiff(requis, n); inconnus <- setdiff(n, admis)
    if (!length(manquants) && !length(inconnus)) return("")
    sprintf("champs attendus %s ; manquants : %s ; inconnus : %s", liste_guillemets(admis),
            if (length(manquants)) liste_guillemets(manquants) else "aucun",
            if (length(inconnus)) liste_guillemets(montrer(inconnus)) else "aucun")
  }
  forme_graine <- function(g) forme_exacte(g, c("double", "integer")) && length(g) == 1L && !is.na(g) &&
    is.finite(g) && g == trunc(g) && abs(g) <= .Machine$integer.max
  if (!forme_exacte(defauts, "list", "names"))
    invalide("liste nue attendue, list(graine, declarations) (ni data.frame, ni classe, aucun attribut autre que les noms).",
             list(parametre = "defauts"))
  e <- ecart_noms(defauts, c("graine", "declarations"), c("graine", "declarations"))
  if (nzchar(e)) invalide(paste0(e, "."), list(parametre = "defauts"))
  dg <- defauts[["graine"]]
  if (!forme_graine(dg))
    invalide("la graine des défauts doit être un nombre entier unique, explicite et fini, sans attribut.",
             list(parametre = "defauts$graine", graine_defauts = dg))
  if (!is.null(graine) && forme_graine(graine) && as.integer(dg) == as.integer(graine))
    refuser("simulation", "defauts_invalides",
            paste("La graine des défauts doit différer de la graine du jeu : avec la même graine, les défauts",
                  "seraient tirés sur le même flux pseudo-aléatoire que le jeu et corrélés à ses colonnes",
                  "(par exemple le sexe)."),
            list(parametre = "defauts$graine", graine = graine, graine_defauts = dg))
  decl <- defauts[["declarations"]]
  if (!forme_exacte(decl, "list"))
    invalide("declarations : liste sans attribut (ni noms, ni classe) de déclarations.",
             list(parametre = "defauts$declarations"))
  causes <- c("deces", "rachat", "en_cours")
  declarations <- lapply(seq_along(decl), function(k) {
    d <- decl[[k]]
    inv <- function(msg) invalide(sprintf("déclaration %d : %s", k, msg), list(parametre = "defauts$declarations",
                                                                              declaration = k))
    if (!forme_exacte(d, "list", "names"))
      inv("liste nue attendue, list(type, cible, taux) ou list(type, cible, nombre).")
    e <- ecart_noms(d, "type", c("type", "cible", "taux", "nombre"))
    if (nzchar(e)) inv(paste0(e, "."))
    type <- d[["type"]]
    if (!forme_exacte(type, "character") || !length(type) || anyNA(type) || anyDuplicated(type))
      inv("type : chaîne (ou vecteur de chaînes d'une combinaison admise) sans attribut, sans NA ni doublon.")
    if (length(type) == 1L) {
      if (!(type %in% CATALOGUE_DEFAUTS$defaut))
        inv(sprintf("type « %s » inconnu ; types admis : %s.", montrer(type),
                    liste_guillemets(CATALOGUE_DEFAUTS$defaut)))
    } else if (!any(vapply(COMBINAISONS_DEFAUTS, function(cmb)
      length(cmb) == length(type) && all(type %in% cmb), logical(1L)))) {
      inv(sprintf("combinaison %s non admise ; combinaisons admises : %s.", liste_guillemets(montrer(type)),
                  paste(vapply(COMBINAISONS_DEFAUTS, liste_guillemets, character(1L)), collapse = " ; ")))
    }
    unite <- CATALOGUE_DEFAUTS$unite[match(type[1L], CATALOGUE_DEFAUTS$defaut)]
    a_cible <- "cible" %in% names(d)
    if (unite == "tete" && a_cible)
      inv(sprintf("type de tête « %s » : champ cible interdit.", type))
    if (unite == "ligne" && !a_cible)
      inv("type de ligne : champ cible requis (sous-ensemble de « deces », « rachat », « en_cours »).")
    cible <- if (a_cible) d[["cible"]] else causes
    if (!forme_exacte(cible, "character") || !length(cible) || anyNA(cible) || anyDuplicated(cible) ||
        !all(cible %in% causes))
      inv(sprintf("cible : vecteur de chaînes sans attribut, non vide, sans NA ni doublon, pris dans %s.",
                  liste_guillemets(causes)))
    # Types d'une combinaison rangés dans l'ordre du catalogue : l'attribut
    # ne dépend pas de l'ordre de saisie (parametre garde la saisie brute).
    type <- CATALOGUE_DEFAUTS$defaut[CATALOGUE_DEFAUTS$defaut %in% type]
    # Cible disjointe, par construction, des causes que le catalogue admet
    # pour le type (pour une combinaison, pour tous ses types) : refus
    # (complément de spécification d'actuary, 25/10/2026). Un vide qui
    # dépend du jeu (condition entree < fin, conditions de tête, cause
    # absente du jeu propre) n'est pas refusé ici.
    if (unite == "ligne") {
      i_cat <- match(type, CATALOGUE_DEFAUTS$defaut)
      admises <- causes[vapply(causes, function(cs) all(CATALOGUE_DEFAUTS[[cs]][i_cat]), logical(1L))]
      if (!any(cible %in% admises))
        inv(sprintf("cible %s disjointe des causes admises pour %s (%s) : aucune ligne éligible par construction.",
                    liste_guillemets(cible), paste(type, collapse = " + "),
                    if (length(admises)) liste_guillemets(admises) else "aucune"))
    }
    a_taux <- "taux" %in% names(d); a_nombre <- "nombre" %in% names(d)
    if (a_taux == a_nombre) inv("exactement un des champs taux et nombre est requis.")
    v <- if (a_taux) d[["taux"]] else d[["nombre"]]
    reel <- forme_exacte(v, c("double", "integer")) && length(v) == 1L && is.finite(v)
    if (a_taux && !(reel && v >= 0 && v <= 1))
      inv("taux : réel de [0, 1], de type double ou integer, sans attribut.")
    if (a_nombre && !(reel && v == trunc(v) && v >= 0 && v <= .Machine$integer.max))
      inv("nombre : entier >= 0, de type double ou integer, sans attribut.")
    list(type = type, unite = unite, cible = cible,
         taux = if (a_taux) as.numeric(v), nombre = if (a_nombre) as.integer(v))
  })
  list(graine = as.integer(dg), declarations = declarations)
}

# Injecte les défauts dans le jeu propre `jeu` (data.frame de
# simuler_portefeuille(), attribut `simulation`) : sélection sous un seul
# sous_graine(ctl$graine) (ORDRE_TIRAGES_DEFAUTS), refus defauts_ineligibles
# si un `nombre` dépasse le nombre d'éligibles ; puis altérations
# déterministes (CATALOGUE_DEFAUTS). Les lignes non altérées et l'attribut
# `simulation` sont inchangés ; les copies (contrat_duplique) sont ajoutées
# en fin de jeu, lignes N + 1, ..., dans l'ordre des déclarations puis des
# lignes. Rend le jeu altéré, d'attribut `defauts` (parametre, graine,
# generateur, ordre_tirages, validite, lignes, decomptes). Dans `lignes`
# (une ligne par couple (ligne, motif)), `unite` décrit le défaut (ligne ou
# tête) et `role` l'altération de la ligne : alteree (ligne modifiée), copie
# (doublon ajouté), original (ligne dupliquée, non modifiée), tete (autre
# ligne d'une tête en défaut, non modifiée). decomptes : lignes_en_defaut
# compte les lignes qui portent un motif ou un refus attendu (originaux et
# lignes `tete` compris) ; lignes_alterees, les lignes modifiées ou ajoutées
# (rôles alteree et copie).
injecter_defauts <- function(jeu, ctl, defauts, periode) {
  N <- nrow(jeu)
  fin <- as.numeric(periode[["fin"]])
  tete <- jeu$id_tete; cause <- jeu$cause_sortie
  entree <- as.numeric(jeu$date_entree)
  nt <- max(tete)
  # Lignes de chaque tête : contiguës (jeu trié par tête ; toute tête a au
  # moins une ligne), de prem_ligne[h] à prem_ligne[h] + nb_lignes[h] - 1.
  if (is.unsorted(tete))
    stop("injecter_defauts() : jeu propre non trié par tête (erreur de programmation).", call. = FALSE)
  nb_lignes <- tabulate(tete, nbins = nt)
  prem_ligne <- cumsum(nb_lignes) - nb_lignes + 1L
  lignes_de <- function(h) sequence(nb_lignes[h], from = prem_ligne[h])
  a_deces <- tabulate(tete[cause == "deces"], nbins = nt) > 0L
  cat_ <- CATALOGUE_DEFAUTS
  decl <- ctl$declarations

  # Sélection, dans l'ordre reçu (disjonction : une ligne altérée au plus une
  # fois ; une tête retenue rend toutes ses lignes inéligibles).
  alteree <- logical(N)
  eligibles_n <- integer(length(decl))
  retenues <- vector("list", length(decl))
  sous_graine(ctl$graine, for (k in seq_along(decl)) {
    d <- decl[[k]]
    i_cat <- match(d$type, cat_$defaut)
    if (d$unite == "ligne") {
      ok <- !alteree & cause %in% d$cible
      for (i in i_cat) {
        admis <- c(deces = cat_$deces[i], rachat = cat_$rachat[i], en_cours = cat_$en_cours[i])
        ok <- ok & unname(admis[cause])
        if (cat_$condition[i] == "entree_avant_fin") ok <- ok & entree < fin
      }
    } else {
      ok <- nb_lignes >= 2L & tabulate(tete[alteree], nbins = nt) == 0L
      if (cat_$condition[i_cat] == "tete_multi_deces") ok <- ok & a_deces
    }
    el <- which(ok)
    eligibles_n[k] <- length(el)
    if (!is.null(d$taux)) {
      r <- el[stats::runif(length(el)) < d$taux]
    } else {
      if (d$nombre > length(el))
        refuser("simulation", "defauts_ineligibles",
                sprintf(paste("Défauts : déclaration %d (%s) : nombre = %d dépasse le nombre d'unités éligibles",
                              "(%d) du jeu propre."),
                        k, paste(d$type, collapse = " + "), d$nombre, length(el)),
                list(declaration = k, type = d$type, nombre = d$nombre, eligibles = length(el)))
      r <- sort(el[sample.int(length(el), d$nombre)])
    }
    if (d$unite == "ligne") alteree[r] <- TRUE else alteree[lignes_de(r)] <- TRUE
    retenues[[k]] <- r
  })

  # Altérations déterministes, sur les valeurs du jeu propre.
  id_c <- jeu$id_contrat; id_t <- jeu$id_tete; sexe <- jeu$sexe
  nai <- as.numeric(jeu$date_naissance); ent <- entree; sor <- as.numeric(jeu$date_sortie); cs <- cause
  copies <- integer(0)
  morceaux <- vector("list", length(decl))
  ligne_df <- function(ligne, k, defaut, unite, role, premier = rep(NA_real_, length(ligne))) {
    i <- match(defaut, cat_$defaut)
    data.frame(ligne = as.integer(ligne), declaration = rep(as.integer(k), length(ligne)),
               defaut = rep(defaut, length(ligne)), unite = rep(unite, length(ligne)), role = role,
               motif_attendu = rep(cat_$motif_attendu[i], length(ligne)),
               refus_attendu = rep(cat_$refus_attendu[i], length(ligne)),
               premier_deces = structure(as.numeric(premier), class = "Date"), stringsAsFactors = FALSE)
  }
  for (k in seq_along(decl)) {
    d <- decl[[k]]; r <- retenues[[k]]
    if (d$unite == "ligne") {
      parts <- list()
      for (t in d$type) {
        switch(t,
               contrat_duplique = {
                 nouv <- N + length(copies) + seq_along(r)
                 copies <- c(copies, r)
               },
               id_contrat_manquant = id_c[r] <- NA_integer_,
               id_tete_manquant = id_t[r] <- NA_integer_,
               naissance_manquante = nai[r] <- NA_real_,
               entree_manquante = ent[r] <- NA_real_,
               sortie_manquante = sor[r] <- NA_real_,
               cause_manquante = cs[r] <- NA_character_,
               cause_hors_modalites = cs[r] <- toupper(cause[r]),
               sexe_manquant = sexe[r] <- NA_character_,
               sexe_hors_modalites = sexe[r] <- tolower(jeu$sexe[r]),
               naissance_apres_entree = nai[r] <- entree[r] + 1,
               sortie_avant_entree = sor[r] <- entree[r] - 1,
               en_cours_avant_fin = sor[r] <- entree[r],
               age_aberrant = nai[r] <- jours_civils(composantes_distinctes(entree[r])$an - 150L, 1L, 1L),
               date_hors_domaine = nai[r] <- rep(jours_civils(0L, 12L, 31L), length(r)),
               stop("injecter_defauts() : type de ligne sans altération (erreur de programmation).",
                    call. = FALSE))
        parts[[length(parts) + 1L]] <- if (t == "contrat_duplique")
          ligne_df(c(r, nouv), k, t, "ligne", rep(c("original", "copie"), each = length(r)))
        else ligne_df(r, k, t, "ligne", rep("alteree", length(r)))
      }
      m <- do.call(rbind, parts)
    } else {
      # Lignes des têtes retenues : contiguës (jeu trié par tête), de la
      # première prem_l à la dernière dern_l ; ligne altérée l : la dernière,
      # ou (tete_observee_apres_deces) la dernière autre que la première à
      # décès p, de date de décès s_d = premier_deces.
      t <- d$type
      li <- lignes_de(r)
      h <- rep.int(seq_along(r), nb_lignes[r])
      dern_l <- prem_ligne[r] + nb_lignes[r] - 1L
      prem_dc <- rep(NA_real_, length(r))
      if (t == "tete_observee_apres_deces") {
        dc <- cause[li] == "deces"
        p <- li[dc][match(seq_along(r), h[dc])]
        l <- ifelse(p == dern_l, dern_l - 1L, dern_l)
        prem_dc <- sor[p]
      } else {
        l <- dern_l
      }
      switch(t,
             tete_naissance_incoherente = nai[l] <- nai[l] - 1,
             tete_sexe_incoherent = sexe[l] <- ifelse(sexe[l] == "H", "F", "H"),
             tete_observee_apres_deces = { cs[l] <- "en_cours"; sor[l] <- NA_real_ },
             stop("injecter_defauts() : type de tête sans altération (erreur de programmation).", call. = FALSE))
      m <- ligne_df(li, k, t, "tete", ifelse(li == l[h], "alteree", "tete"), prem_dc[h])
    }
    morceaux[[k]] <- m[order(m$ligne, method = "radix"), , drop = FALSE]
  }
  lignes <- do.call(rbind, c(list(ligne_df(integer(0), 0L, character(0), character(0), character(0))), morceaux))
  row.names(lignes) <- NULL

  en_date <- function(j) structure(as.numeric(j), class = "Date")
  # Copies : valeurs du jeu propre (une ligne dupliquée n'est pas altérée autrement).
  resultat <- data.frame(id_contrat = c(id_c, jeu$id_contrat[copies]), id_tete = c(id_t, jeu$id_tete[copies]),
                         sexe = c(sexe, jeu$sexe[copies]),
                         date_naissance = en_date(c(nai, as.numeric(jeu$date_naissance)[copies])),
                         date_entree = en_date(c(ent, entree[copies])),
                         date_sortie = en_date(c(sor, as.numeric(jeu$date_sortie)[copies])),
                         cause_sortie = c(cs, cause[copies]), stringsAsFactors = FALSE)
  attr(resultat, "simulation") <- attr(jeu, "simulation")
  roles <- names(jeu)
  attr(resultat, "defauts") <- list(
    parametre = defauts, graine = ctl$graine, generateur = GENERATEUR,
    ordre_tirages = ORDRE_TIRAGES_DEFAUTS,
    validite = list(
      roles = stats::setNames(roles, roles), periode = periode, delai_declaration = 0,
      texte = paste("Motifs et refus attendus sous trois conditions : les sept rôles déclarés (colonne du même nom),",
                    "un plan de période égale à celle de la simulation, un délai de déclaration nul.",
                    "tete_observee_apres_deces : motif attendu si le premier décès de la tête (colonne",
                    "premier_deces de lignes) précède fin - délai + 1 ; toujours le cas sous un délai nul.",
                    "date_hors_domaine : refus du même nom, quelles que soient les exclusions déclarées.",
                    "Si aucune déclaration date_hors_domaine n'a retenu de ligne : toutes les exclusions des",
                    "motifs attendus déclarées, lignes exclues = lignes de l'attribut, motif par motif ; aucune",
                    "exclusion, refus ligne_invalide.")),
    lignes = lignes,
    decomptes = list(
      lignes_jeu_propre = N, copies = length(copies), lignes = N + length(copies),
      lignes_en_defaut = length(unique(lignes$ligne)),
      lignes_alterees = length(unique(lignes$ligne[lignes$role %in% c("alteree", "copie")])),
      declarations = data.frame(declaration = seq_along(decl),
                                defaut = vapply(decl, function(d) paste(d$type, collapse = " + "), character(1L)),
                                unite = vapply(decl, `[[`, character(1L), "unite"),
                                eligibles = eligibles_n, retenues = lengths(retenues),
                                stringsAsFactors = FALSE)))
  resultat
}

# --- point d'entrée ---------------------------------------------------------------

# Simule un portefeuille de n têtes : une tête par contrat (modes `uniforme`
# et `age_exact`), ou K contrats tirés par tête (modes `uniforme_multi` et
# `age_exact_multi`, #92).
#   loi          : list(deces = list(table, facteurs), rachat = list(table,
#                  facteurs)) ; table : `table_reference` (H et F, ou
#                  `ensemble`) ; facteurs : c(H = , F = ), double ou integer, seul
#                  attribut les noms (#58), appliqués à q_x, plafond 1 ;
#   portefeuille : liste nue (seul attribut les noms, #78), champs numériques
#                  double ou integer sans attribut, dates de classe Date seule ;
#                  list(mode = "uniforme", n, proportion_hommes, age_min,
#                  age_max, origine) — entrée uniforme discrète sur les jours
#                  de [origine, fin], âge à l'entrée uniforme continu sur
#                  [age_min, age_max), naissance = entrée - round(âge *
#                  365,25) jours ; ou list(mode = "age_exact", n,
#                  proportion_hommes, age, date_entree) — entrée à l'âge
#                  exact `age` à la date `date_entree` (constat de
#                  simulation) ; ou les modes `uniforme_multi` et
#                  `age_exact_multi`, aux mêmes champs plus `loi_contrats`
#                  (probabilités p_1, ..., p_m du nombre K de contrats par
#                  tête) : en `uniforme_multi`, entrées iid par contrat sur
#                  les jours de [origine, fin] et âge à la première entrée
#                  uniforme sur [age_min, age_max) ; en `age_exact_multi`,
#                  tous les contrats entrent à `date_entree` ;
#   periode      : list(debut, fin), dates (classe Date seule, #58) ;
#                  censure à B = fin + 1 ;
#   graine       : entier explicite (sous_graine, générateur GENERATEUR) ;
#   defauts      : NULL (défaut de l'argument) : aucun défaut injecté, aucun
#                  attribut `defauts` (ce n'est pas une graine par défaut ;
#                  ADR 0007, annotation du 25/10/2026, (b)) ; ou list(graine,
#                  declarations) (controler_defauts()) : défauts paramétrés
#                  injectés après le tirage complet du jeu propre
#                  (injecter_defauts()).
# Rend un data.frame (id_contrat, id_tete, sexe, date_naissance, date_entree,
# date_sortie, cause_sortie) avec l'attribut `simulation` (et `defauts` si des
# défauts sont déclarés). Toutes les lignes sont gardées, y compris celles
# sorties avant le début de la période ; seuls manquent les contrats non
# souscrits (modes multi-contrats), décomptés dans l'attribut.
simuler_portefeuille <- function(loi, portefeuille, periode, graine, defauts = NULL) {
  if (!liste_aux_noms(loi, names(CAUSES_SIMULEES)))
    refuser("simulation", "loi_invalide",
            sprintf("La loi vraie doit être une liste nommée %s.", liste_guillemets(names(CAUSES_SIMULEES))),
            list(noms = names(loi)))
  ctl <- controler_portefeuille(portefeuille, periode)
  p <- portefeuille
  n <- as.integer(p$n)
  # Sexes tirés (proportion non nulle) : seuls construits et contrôlés.
  sexes <- MODALITES_SEXE[c(p$proportion_hommes > 0, p$proportion_hommes < 1)]
  effective <- do.call(rbind, lapply(names(CAUSES_SIMULEES), function(cause)
    loi_effective(loi[[cause]], cause, sexes)))
  controler_couverture(effective, ctl$age_bas, ctl$age_haut, sexes)
  debut <- as.numeric(periode$debut); fin <- as.numeric(periode$fin); borne <- fin + 1
  # Défauts : contrôlés avant tout tirage (refus defauts_invalides).
  if (!is.null(defauts)) {
    graine_jeu <- if (missing(graine)) NULL else graine
    ctl_defauts <- controler_defauts(defauts, graine_jeu)
  }

  # Tirages, dans l'ordre fixé (ORDRE_TIRAGES ; modes multi-contrats :
  # ORDRE_TIRAGES_MULTI). Modes existants : K = 1 par tête sans tirage (loi
  # dégénérée), N = n : tirages et ordre inchangés (ADR 0007, annotation du
  # 25/10/2026, (a)).
  multi <- ctl$mode != MODE_BASE[[ctl$mode]]
  uniforme <- MODE_BASE[[ctl$mode]] == "uniforme"
  lc <- if (multi) p[["loi_contrats"]] else 1
  m <- length(lc)
  tirages <- sous_graine(graine, {
    u_sexe <- stats::runif(n)
    # K par inversion explicite ; aucun tirage si la loi n'a qu'une
    # probabilité non nulle (p_m > 0 : K = m).
    K <- if (sum(lc > 0) == 1L) rep.int(m, n) else 1L + findInterval(stats::runif(n), cumsum(lc)[-m])
    nk <- sum(K)
    if (uniforme) {
      j_entree <- sample.int(as.integer(fin - as.numeric(p$origine) + 1), nk, replace = TRUE)
      u_age <- stats::runif(n)
    } else {
      j_entree <- u_age <- NULL
    }
    list(u_sexe = u_sexe, K = K, j_entree = j_entree, u_age = u_age,
         e_deces = stats::rexp(n), e_rachat = stats::rexp(nk))
  })

  # Contrats tirés, dans l'ordre des tirages (têtes, puis contrats) : tête de
  # chaque contrat, entrée ; première entrée e_(1) de chaque tête (toute tête
  # a au moins un contrat) ; lignes ordonnées par (tête, entrée, rang de
  # tirage), tri radix stable.
  K <- tirages$K; nk <- length(tirages$e_rachat)
  tete <- rep.int(seq_len(n), K)
  sexe <- ifelse(tirages$u_sexe < p$proportion_hommes, "H", "F")
  entree <- if (uniforme) as.numeric(p$origine) + tirages$j_entree - 1 else rep(as.numeric(p$date_entree), nk)
  o <- order(tete, entree, method = "radix")
  e1 <- entree[o[!duplicated(tete[o])]]
  if (uniforme) {
    age_entree <- p$age_min + (p$age_max - p$age_min) * tirages$u_age
    naissance <- e1 - round(age_entree * 365.25)
  } else {
    ce <- composantes_date(as.numeric(p$date_entree))
    naissance <- rep(jours_civils(ce$an - as.integer(p$age), ce$mois + 1L, ce$jour), n)
  }

  # Âge exact à la première entrée, par tête, recalculé depuis les dates (T2, T3).
  cn <- composantes_distinctes(naissance)
  x0 <- age_anniversaire(cn, composantes_distinctes(e1)$an, e1)
  a0 <- jour_anniversaire(cn, x0)
  frac0 <- (e1 - a0) / (jour_anniversaire(cn, x0 + 1L) - a0)
  # Assertion : les âges effectivement parcourus (de l'entrée à l'instant B)
  # restent dans les bornes a priori [age_bas, age_haut] de
  # controler_portefeuille(), donc dans la table ; un écart serait une
  # erreur de programmation, jamais un refus dépendant de la graine.
  x_borne <- age_anniversaire(cn, composantes_date(borne)$an, borne)
  if (min(x0) < floor(ctl$age_bas) || max(x_borne) > floor(ctl$age_haut))
    stop(sprintf(paste("simuler_portefeuille() : âges parcourus %d à %d hors des bornes a priori",
                       "%d à %d (erreur de programmation)."),
                 min(x0), max(x_borne), as.integer(floor(ctl$age_bas)), as.integer(floor(ctl$age_haut))),
         call. = FALSE)

  # Temps latent d'une transition pour des unités (têtes ou contrats) de
  # composantes de naissance cnu, d'âge x0u et de fraction frac0u au jour de
  # départ du, de sexe su, de tirages e.
  latent <- function(cause, cnu, x0u, frac0u, du, su, e) {
    instant <- rep(Inf, length(e)); jour <- rep(NA_real_, length(e))
    for (s in sexes) {
      idx <- which(su == s)
      if (!length(idx)) next
      t <- temps_latent(effective[effective$cause == cause & effective$sexe == s, , drop = FALSE],
                        lapply(cnu, `[`, idx), x0u[idx], frac0u[idx], du[idx], e[idx])
      instant[idx] <- t$instant; jour[idx] <- t$jour
    }
    list(instant = instant, jour = jour)
  }
  # Décès par tête, à partir de la première entrée ; rachat par contrat, à
  # partir de son entrée, à l'âge de la tête (modes existants : contrat = tête).
  dc <- latent("deces", cn, x0, frac0, e1, sexe, tirages$e_deces)
  if (multi) {
    cnk <- lapply(cn, `[`, tete)
    xk <- age_anniversaire(cnk, composantes_distinctes(entree)$an, entree)
    ak <- jour_anniversaire(cnk, xk)
    fk <- (entree - ak) / (jour_anniversaire(cnk, xk + 1L) - ak)
    rc <- latent("rachat", cnk, xk, fk, entree, sexe[tete], tirages$e_rachat)
  } else {
    rc <- latent("rachat", cn, x0, frac0, entree, sexe, tirages$e_rachat)
  }

  # Sortie : minimum des deux temps latents et de la censure à B, en temps
  # continu ; égalité (probabilité nulle) attribuée au décès. Décès commun
  # aux contrats de la tête en cours à son instant.
  td <- dc$instant[tete]; tr <- rc$instant
  est_deces <- td <= tr & td < borne
  est_rachat <- tr < td & tr < borne
  cause_sortie <- rep("en_cours", nk)
  cause_sortie[est_deces] <- "deces"; cause_sortie[est_rachat] <- "rachat"
  sortie <- rep(NA_real_, nk)
  sortie[est_deces] <- dc$jour[tete][est_deces]
  sortie[est_rachat] <- rc$jour[est_rachat]
  if (anyNA(sortie[cause_sortie != "en_cours"]))
    stop("simuler_portefeuille() : événement sans date (erreur de programmation).", call. = FALSE)
  # Contrat non souscrit : un contrat n'est écrit que si son entrée précède
  # l'instant de décès de la tête (double de temps_latent()). Un contrat entré
  # le jour de la première entrée l'est toujours : l'instant de décès suit
  # e_(1) (tirage exponentiel > 0) ; la règle ne dépend donc d'aucun arrondi
  # qui rendrait cet instant égal à e_(1). Modes existants : toutes les lignes.
  ecrit <- entree <= e1[tete] | entree < td
  sel <- o[ecrit[o]]
  if (any(sortie[sel][est_deces[sel]] < entree[sel][est_deces[sel]]))
    stop("simuler_portefeuille() : décès antérieur à l'entrée d'un contrat écrit (erreur de programmation).",
         call. = FALSE)

  en_date <- function(j) structure(as.numeric(j), class = "Date")
  # id_tete : seq_len(n) dans les modes existants (même objet qu'avant #92).
  resultat <- data.frame(id_contrat = seq_len(length(sel)), id_tete = if (multi) tete[sel] else seq_len(n),
                         sexe = sexe[tete][sel],
                         date_naissance = en_date(naissance[tete][sel]), date_entree = en_date(entree[sel]),
                         date_sortie = en_date(sortie[sel]), cause_sortie = cause_sortie[sel],
                         stringsAsFactors = FALSE)
  so <- sortie[sel]
  decomptes <- list(lignes = length(sel), deces = sum(est_deces[sel]), rachats = sum(est_rachat[sel]),
                    en_cours = sum(cause_sortie[sel] == "en_cours"),
                    sorties_avant_debut = sum(!is.na(so) & so < debut),
                    entrees_avant_debut = sum(entree[sel] < debut))
  if (multi) {
    # Têtes dont le décès figure dans le jeu (au moins une ligne à décès) ;
    # têtes décédées avant B sans contrat en cours à leur décès (aucune ligne
    # à décès : décès absent du jeu).
    observee <- tabulate(tete[sel][est_deces[sel]], nbins = n) > 0L
    decomptes <- c(decomptes, list(tetes = n, contrats_tires = nk, contrats_non_souscrits = nk - length(sel),
                                   tetes_decedees_observees = sum(observee),
                                   tetes_decedees_sans_contrat = sum(dc$instant < borne & !observee)))
  }
  attr(resultat, "simulation") <- list(
    loi_vraie = list(tables = lapply(loi, `[[`, "table"), facteurs = lapply(loi, `[[`, "facteurs"),
                     effective = effective, transitions = CAUSES_SIMULEES,
                     hypothese_repartition = HYPOTHESE_REPARTITION, sexes_controles = sexes),
    portefeuille = portefeuille, periode = periode,
    graine = as.integer(graine), generateur = GENERATEUR,
    ordre_tirages = if (multi) ORDRE_TIRAGES_MULTI[[ctl$mode]] else ORDRE_TIRAGES,
    version_r = R.version.string,
    decomptes = decomptes)
  # Modes multi-contrats : champ ajouté en fin seulement (annotation du
  # 25/10/2026, (c), point 5).
  if (multi)
    attr(resultat, "simulation")$contrats <- list(loi = as.double(lc), tires_par_tete = tabulate(K, nbins = m))
  # Défauts paramétrés (#93) : après le tirage complet du jeu propre, sous
  # leur propre graine ; jeu propre inchangé si defauts = NULL.
  if (!is.null(defauts)) resultat <- injecter_defauts(resultat, ctl_defauts, defauts, periode)
  resultat
}
