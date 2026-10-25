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
#  indépendants. Une tête = un contrat dans cette branche.
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

# Champs du portefeuille, par mode : tous requis, aucun défaut implicite.
CHAMPS_PORTEFEUILLE <- list(
  uniforme = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine"),
  age_exact = c("mode", "n", "proportion_hommes", "age", "date_entree"))

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
  # Champs lus par [[ ]] (jamais $, qui apparie les noms partiels) ; ici, le
  # mode : chaîne unique de forme exacte (chaine_unique(), #60).
  mode <- if (is.list(portefeuille)) portefeuille[["mode"]] else NULL
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
  reel <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v)
  entier <- function(v) reel(v) && v == trunc(v)
  invalide <- function(msg) refuser("simulation", "portefeuille_invalide", paste("Portefeuille :", msg))
  if (!entier(p[["n"]]) || p[["n"]] < 1 || p[["n"]] > .Machine$integer.max) invalide("n entier >= 1.")
  if (!reel(p[["proportion_hommes"]]) || p[["proportion_hommes"]] < 0 || p[["proportion_hommes"]] > 1)
    invalide("proportion_hommes réel dans [0, 1].")
  fin <- as.numeric(periode[["fin"]])
  if (mode == "uniforme") {
    if (!reel(p[["age_min"]]) || !reel(p[["age_max"]]) || p[["age_min"]] * 365.25 < 1 || p[["age_min"]] >= p[["age_max"]])
      invalide("âges réels finis, age_min d'au moins un jour (1/365,25) et age_min < age_max.")
    if (!date_unique(p[["origine"]]) || p[["origine"]] > periode[["debut"]])
      invalide("origine : date (classe Date) antérieure ou égale au début de la période.")
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
    if (!entier(p[["age"]]) || p[["age"]] < 1) invalide("âge exact entier >= 1.")
    if (!date_unique(p[["date_entree"]]) || p[["date_entree"]] > periode[["fin"]])
      invalide("date_entree : date (classe Date) antérieure ou égale à la fin de la période.")
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

# --- point d'entrée ---------------------------------------------------------------

# Simule un portefeuille de n contrats (une tête par contrat).
#   loi          : list(deces = list(table, facteurs), rachat = list(table,
#                  facteurs)) ; table : `table_reference` (H et F, ou
#                  `ensemble`) ; facteurs : c(H = , F = ), double ou integer, seul
#                  attribut les noms (#58), appliqués à q_x, plafond 1 ;
#   portefeuille : list(mode = "uniforme", n, proportion_hommes, age_min,
#                  age_max, origine) — entrée uniforme discrète sur les jours
#                  de [origine, fin], âge à l'entrée uniforme continu sur
#                  [age_min, age_max), naissance = entrée - round(âge *
#                  365,25) jours ; ou list(mode = "age_exact", n,
#                  proportion_hommes, age, date_entree) — entrée à l'âge
#                  exact `age` à la date `date_entree` (constat de
#                  simulation) ;
#   periode      : list(debut, fin), dates (classe Date seule, #58) ;
#                  censure à B = fin + 1 ;
#   graine       : entier explicite (sous_graine, générateur GENERATEUR).
# Rend un data.frame (id_contrat, id_tete, sexe, date_naissance, date_entree,
# date_sortie, cause_sortie) avec l'attribut `simulation`. Toutes les lignes
# sont gardées, y compris celles sorties avant le début de la période.
simuler_portefeuille <- function(loi, portefeuille, periode, graine) {
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

  # Tirages, dans l'ordre fixé (ORDRE_TIRAGES).
  tirages <- sous_graine(graine, {
    u_sexe <- stats::runif(n)
    if (ctl$mode == "uniforme") {
      j_entree <- sample.int(as.integer(fin - as.numeric(p$origine) + 1), n, replace = TRUE)
      u_age <- stats::runif(n)
    } else {
      j_entree <- u_age <- NULL
    }
    list(u_sexe = u_sexe, j_entree = j_entree, u_age = u_age,
         e_deces = stats::rexp(n), e_rachat = stats::rexp(n))
  })

  sexe <- ifelse(tirages$u_sexe < p$proportion_hommes, "H", "F")
  if (ctl$mode == "uniforme") {
    entree <- as.numeric(p$origine) + tirages$j_entree - 1
    age_entree <- p$age_min + (p$age_max - p$age_min) * tirages$u_age
    naissance <- entree - round(age_entree * 365.25)
  } else {
    entree <- rep(as.numeric(p$date_entree), n)
    ce <- composantes_date(as.numeric(p$date_entree))
    naissance <- rep(jours_civils(ce$an - as.integer(p$age), ce$mois + 1L, ce$jour), n)
  }

  # Âge exact à l'entrée, recalculé depuis les dates (T2, T3).
  cn <- composantes_distinctes(naissance)
  x0 <- age_anniversaire(cn, composantes_distinctes(entree)$an, entree)
  a0 <- jour_anniversaire(cn, x0)
  frac0 <- (entree - a0) / (jour_anniversaire(cn, x0 + 1L) - a0)
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

  lat <- list()
  for (cause in names(CAUSES_SIMULEES)) {
    instant <- rep(Inf, n); jour <- rep(NA_real_, n)
    e <- tirages[[paste0("e_", cause)]]
    for (s in sexes) {
      idx <- which(sexe == s)
      if (!length(idx)) next
      t <- temps_latent(effective[effective$cause == cause & effective$sexe == s, , drop = FALSE],
                        lapply(cn, `[`, idx), x0[idx], frac0[idx], entree[idx], e[idx])
      instant[idx] <- t$instant; jour[idx] <- t$jour
    }
    lat[[cause]] <- list(instant = instant, jour = jour)
  }

  # Sortie : minimum des deux temps latents et de la censure à B, en temps
  # continu ; égalité (probabilité nulle) attribuée au décès.
  td <- lat$deces$instant; tr <- lat$rachat$instant
  est_deces <- td <= tr & td < borne
  est_rachat <- tr < td & tr < borne
  cause_sortie <- rep("en_cours", n)
  cause_sortie[est_deces] <- "deces"; cause_sortie[est_rachat] <- "rachat"
  sortie <- rep(NA_real_, n)
  sortie[est_deces] <- lat$deces$jour[est_deces]
  sortie[est_rachat] <- lat$rachat$jour[est_rachat]
  if (anyNA(sortie[cause_sortie != "en_cours"]))
    stop("simuler_portefeuille() : événement sans date (erreur de programmation).", call. = FALSE)

  en_date <- function(j) structure(as.numeric(j), class = "Date")
  resultat <- data.frame(id_contrat = seq_len(n), id_tete = seq_len(n), sexe = sexe,
                         date_naissance = en_date(naissance), date_entree = en_date(entree),
                         date_sortie = en_date(sortie), cause_sortie = cause_sortie,
                         stringsAsFactors = FALSE)
  attr(resultat, "simulation") <- list(
    loi_vraie = list(tables = lapply(loi, `[[`, "table"), facteurs = lapply(loi, `[[`, "facteurs"),
                     effective = effective, transitions = CAUSES_SIMULEES,
                     hypothese_repartition = HYPOTHESE_REPARTITION, sexes_controles = sexes),
    portefeuille = portefeuille, periode = periode,
    graine = as.integer(graine), generateur = GENERATEUR, ordre_tirages = ORDRE_TIRAGES,
    version_r = R.version.string,
    decomptes = list(lignes = n, deces = sum(est_deces), rachats = sum(est_rachat),
                     en_cours = sum(cause_sortie == "en_cours"),
                     sorties_avant_debut = sum(!is.na(sortie) & sortie < debut),
                     entrees_avant_debut = sum(entree < debut)))
  resultat
}
