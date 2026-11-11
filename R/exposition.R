# -----------------------------------------------------------------------------
#  R/exposition.R : module `exposition` (issue #11 ; E2.3, E2.5, E2.6 ;
#  ADR 0003, points 1, 3, 6 et 8 ; ADR 0007, points 5.2, 5.3 et 10 ;
#  spécification d'actuary, A.0 et A.3)
#
#  - controler_parametres_exposition(), resoudre_convention() : paramètres
#    de l'étape (convention d'âge, découpage par année) ; forçage vers une
#    convention plus grossière admis et inscrit, vers une convention plus
#    fine ou non mise en oeuvre refusé ; sans convention native, convention
#    déclarée, `plus_fine` refusé (ADR 0016, point 5.5). La recette est
#    déduite des rôles dans le module `donnees` (deduire_recette(), ADR 0016,
#    point 2.1) ;
#  - fenetres_observations(), segments_observations(),
#    position_dans_cellule() : fenêtres et événements (T5), segments des
#    observations, position d'un instant dans sa cellule ; employées par
#    calculer_exposition() et par l'estimateur de Kaplan-Meier du module
#    `brut` (#15, spécification d'actuary, A.9 : factorisation à résultats
#    identiques) ;
#  - calculer_exposition() : fonction d'étape `exposition`, aiguillage sur le
#    seam de la recette (ADR 0016, point 4.1) ;
#  - exposition_fenetres() : seam « fenêtres » — fenêtre de chaque
#    observation (troncature au début de la période, censure à
#    B = fin - délai + 1), exposition centrale par cellule (segment, âge
#    [, année civile]) en jours et en unité d'exposition, événements de la
#    transition étudiée, compléments de l'exposition initiale, sorties
#    concurrentes (censures à droite à leur instant) ;
#  - exposition_cellules(), hypotheses_agregats() : seam « cellules »
#    (agrégats, #169) — somme des cellules reçues par clé, sans en créer,
#    relevés, hypothèses de la recette (champ hypotheses_recette) ;
#  - fenetres_stocks_successifs(), hypotheses_stocks() : fenêtres des unités
#    de la recette `stocks_successifs` (#180, § 5) au seam « fenêtres » —
#    début selon l'hypothèse d'entrée entre inventaires déclarée
#    (entree_entre_inventaires), fin à la sortie ou censure à B —, relevés,
#    hypothèses de la recette ;
#  - CONVENTIONS_VALIDATION, ETAPES_PLAGE_AGES, controler_plage_ages() :
#    conventions d'âge admises par les étapes qui lisent une plage d'âges,
#    forme de la plage d'âges sous le module de l'étape appelante (déplacées
#    depuis le module positionnement, ADR 0017, point 10.1 ; motifs, modules,
#    details et messages des refus inchangés) ;
#  - grille_plage() : grille des cellules segments x âges de la plage
#    [x années] (E^c, D, cellules sans ligne à zéro, hors plage), extraite
#    d'apparier_table() (module positionnement ; ADR 0017, point 10.1).
#
#  Conventions de temps (A.0) : une date est un jour ; une observation est
#  l'intervalle [debut, fin) (fin = +Inf pour une observation en cours) ; un
#  événement du jour s survient à l'instant s. Fenêtre : a = max(debut, D),
#  b = min(fin, B) ; exposition [a, b), vide si a >= b ; événement compté si
#  la cause est la cible et D <= s < B (T5).
#  Cellules : convention `dernier_anniversaire` : [anniv(x), anniv(x+1)),
#  unité l'année d'âge (jours / L_x, L_x = anniv(x+1) - anniv(x), 365 ou
#  366 : T3) ; naissance au 29/02 : anniversaire au 01/03 des années non
#  bissextiles (T2). Convention `age_atteint_annee` : âge an(t) - an(n),
#  cellule [01/01, 01/01 suivant), unité la fraction d'année civile
#  (jours / 365 ou 366).
#  Arithmétique de dates entière et vectorisée (calendrier grégorien
#  proleptique), écrite indépendamment de celle du simulateur, qu'aucun
#  module du moteur n'appelle (ADR 0007, point 2.1) ; vérifiée contre
#  as.Date() et age_revolu() dans les tests. Elle double volontairement
#  composantes_date() de `donnees` (enveloppe de as.POSIXlt, dont dépend
#  age_revolu()) et les jours civils du simulateur (M89, #22 point 3) : trois
#  implémentations indépendantes de la même convention du 29/02 (T2), dont
#  l'accord est vérifié sur [0001-01-01, 9999-12-31] (tests/unitaires/
#  test_exposition.R) : exhaustif pour la décomposition et les jours civils ;
#  pour les anniversaires, exhaustif sur le sous-domaine énuméré (six âges ;
#  28/02, 29/02 et 01/03 des années bissextiles), partiel sur le domaine ; pour
#  les âges, par tirage et balayage ; ne pas les unifier sans reprendre ce test.
#  Ce module n'appelle que `donnees` et `outils` (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# --- recettes et conventions (A.3) --------------------------------------------

# Conventions d'âge, de la plus fine à la plus grossière (A.3).
ORDRE_CONVENTIONS <- c("dernier_anniversaire", "age_atteint_annee", "age_fourni")
# Conventions mises en oeuvre dans la branche.
CONVENTIONS_MISES_EN_OEUVRE <- c("dernier_anniversaire", "age_atteint_annee")
# Valeurs admises du paramètre `convention` du plan (ADR 0007, annotation du
# 17/10/2026, M49) : `plus_fine` retient la convention native de la recette.
VALEURS_CONVENTION <- c("plus_fine", "dernier_anniversaire", "age_atteint_annee")
# Unité de l'exposition centrale par convention (T3), inscrite dans la fiche.
UNITES_EXPOSITION <- c(
  dernier_anniversaire = "annee_d_age : jours / L_x, L_x = anniv(x+1) - anniv(x) (365 ou 366 jours)",
  age_atteint_annee = "annee_civile : jours / nombre de jours de l'année civile (365 ou 366)")
# Valeurs admises du paramètre conditionnel `entree_entre_inventaires` de
# l'étape (recette `stocks_successifs` ; spécification de #180, § 5, QS1,
# M328 ; ADR 0016, point 7.2) : entrée retardée au premier inventaire où
# l'unité est présente, ou entrée au milieu de l'intervalle des instants
# d'entrée compatibles avec les données. Sans défaut.
VALEURS_ENTREE_ENTRE_INVENTAIRES <- c("premier_inventaire", "milieu")
# Hypothèse sur les sorties concurrentes, inscrite dans la fiche (A.3).
HYPOTHESE_CENSURE <- paste("censure non informative : les sorties concurrentes sont des censures",
                           "à droite à leur instant (risques indépendants)")

# Contrôle les paramètres propres à l'étape `exposition` (valeurs ; les noms
# sont contrôlés par valider_plan()). Forme exacte (#23, forme_exacte()) :
# type de base exact, aucun attribut (ni names, ni dim, ni classe), sinon
# refus sous le motif du paramètre. `entree_entre_inventaires`, paramètre
# conditionnel à la recette (ADR 0016, point 7.2), n'est contrôlé que s'il
# est passé (paramètre présent dans le plan ; sa présence selon la recette
# est contrôlée après la sélection de la recette, controler_plan_roles()) :
# chaîne unique prise dans VALEURS_ENTREE_ENTRE_INVENTAIRES, sinon refus
# entree_entre_inventaires_invalide. Rend TRUE, invisible.
controler_parametres_exposition <- function(convention, par_annee, entree_entre_inventaires) {
  if (!forme_exacte(convention, "character") || length(convention) != 1L || is.na(convention))
    refuser("exposition", "convention_invalide",
            formuler("La convention d'âge doit être une chaîne unique, sans attribut, prise dans %s.",
                     liste_guillemets(VALEURS_CONVENTION)),
            list(convention = convention))
  if (convention %in% setdiff(ORDRE_CONVENTIONS, CONVENTIONS_MISES_EN_OEUVRE))
    refuser("exposition", "convention_non_mise_en_oeuvre",
            formuler(paste("Convention d'âge « %s » non mise en oeuvre (rôles d'âge fourni non pris",
                           "en charge) : la construction est refusée (valeurs admises : %s)."),
                     citer(convention), liste_guillemets(VALEURS_CONVENTION)),
            list(convention = convention))
  if (!(convention %in% VALEURS_CONVENTION))
    refuser("exposition", "convention_invalide",
            formuler("Convention d'âge inconnue %s (valeurs admises : %s).", liste_guillemets(convention),
                     liste_guillemets(VALEURS_CONVENTION)),
            list(convention = convention))
  if (!forme_exacte(par_annee, "logical") || length(par_annee) != 1L || is.na(par_annee))
    refuser("exposition", "par_annee_invalide",
            formuler("Le découpage par année (par_annee) doit valoir TRUE ou FALSE, sans attribut."),
            list(par_annee = par_annee))
  if (!missing(entree_entre_inventaires) &&
      (!forme_exacte(entree_entre_inventaires, "character") || length(entree_entre_inventaires) != 1L ||
       is.na(entree_entre_inventaires) || !(entree_entre_inventaires %in% VALEURS_ENTREE_ENTRE_INVENTAIRES)))
    refuser("exposition", "entree_entre_inventaires_invalide",
            formuler(paste("L'hypothèse d'entrée entre inventaires (entree_entre_inventaires) doit être une chaîne",
                           "unique, sans attribut, prise dans %s (aucun défaut)."),
                     liste_guillemets(VALEURS_ENTREE_ENTRE_INVENTAIRES)),
            list(entree_entre_inventaires = entree_entre_inventaires))
  invisible(TRUE)
}

# Convention retenue à partir de la convention demandée et de la convention
# native de la recette (CATALOGUE_RECETTES$convention_native, module
# donnees) : `plus_fine` retient la native ; toute autre valeur est retenue
# telle quelle. Une convention plus grossière que la native est un forçage,
# admis ; une convention plus fine est refusée (les rôles ne la permettent
# pas). Sans convention native (native = NA : la convention ne se déduit pas
# des rôles ; ADR 0016, point 5.5) : `plus_fine` est refusé
# (convention_non_declaree), toute autre valeur est retenue telle quelle,
# non forcée. Rend list(demandee, native, retenue, forcee), valeur validée
# que construire_table() transmet aux étapes (#82).
resoudre_convention <- function(convention, native) {
  if (is.na(native)) {
    if (identical(convention, "plus_fine"))
      refuser("exposition", "convention_non_declaree",
              formuler(paste("Convention d'âge « plus_fine » sans objet : la recette retenue n'a pas de convention native",
                             "(la convention d'âge ne se déduit pas des rôles) ; déclarer la convention des données,",
                             "prise dans %s."),
                       liste_guillemets(CONVENTIONS_MISES_EN_OEUVRE)),
              list(convention = convention, convention_native = native))
    return(list(demandee = convention, native = native, retenue = convention, forcee = FALSE))
  }
  retenue <- if (identical(convention, "plus_fine")) native else convention
  if (match(retenue, ORDRE_CONVENTIONS) < match(native, ORDRE_CONVENTIONS))
    refuser("exposition", "convention_plus_fine",
            formuler(paste("Convention d'âge « %s » plus fine que celle que la recette permet",
                           "(« %s ») : le forçage vers une convention plus fine est refusé."),
                     citer(retenue), native),
            list(convention = retenue, convention_native = native))
  list(demandee = convention, native = native, retenue = retenue, forcee = !identical(retenue, native))
}

# --- arithmétique de dates (jours depuis 1970-01-01) -------------------------

# Bissextile (grégorien) ; nombre d'années bissextiles de 1 à an - 1
# (fonction de comptage, différences exactes pour tout entier).
an_bissextile <- function(an) (an %% 4 == 0 & an %% 100 != 0) | an %% 400 == 0
bissextiles_anterieures <- function(an) {
  k <- an - 1
  k %/% 4 - k %/% 100 + k %/% 400
}
# Jour (depuis 1970-01-01) du 1er janvier de l'année `an`.
premier_janvier <- function(an) 365 * (an - 1970) + bissextiles_anterieures(an) -
  bissextiles_anterieures(1970)
# Jours écoulés avant le 1er de chaque mois, année commune.
JOURS_AVANT_MOIS <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
# Année civile du jour t : estimation puis correction exacte (l'estimation
# est à une année près).
annee_du_jour <- function(t) {
  an <- 1970 + floor(t / 365.2425)
  an <- an - (premier_janvier(an) > t)
  an + (premier_janvier(an + 1) <= t)
}
# Composantes (an, mois 1-12, jour) du jour t, et quantième de (mois, jour)
# dans une année commune (0 au 1er janvier ; 59 pour le 29/02 comme pour le
# 01/03).
composantes_jour <- function(t) {
  an <- annee_du_jour(t)
  quantieme <- t - premier_janvier(an)                     # 0 au 1er janvier
  avant <- function(m) JOURS_AVANT_MOIS[m] + (m > 2 & an_bissextile(an))
  mois <- rep(1, length(t))
  for (m in 2:12) mois <- mois + (quantieme >= avant(m))
  jour <- quantieme - avant(mois) + 1
  list(an = an, mois = mois, jour = jour, quantieme_commun = JOURS_AVANT_MOIS[mois] + jour - 1)
}

# Table calendaire des années an_min à an_max : 1er janvier (jours) et
# bissextilité, lues par indice (calcul vectorisé sur les morceaux).
calendrier <- function(an_min, an_max) {
  an <- an_min:an_max
  list(an_min = an_min, an_max = an_max, premier_janvier = premier_janvier(an),
       bissextile = an_bissextile(an))
}
indice_annee <- function(cal, an) {
  i <- an - cal$an_min + 1
  if (length(i) && (min(i) < 1 || max(i) > length(cal$premier_janvier)))
    stop("indice_annee() : année hors de la table calendaire (erreur de programmation).", call. = FALSE)
  i
}
# Année civile du jour t (an_min <= année <= an_max).
annee_calendrier <- function(cal, t) cal$an_min - 1 + findInterval(t, cal$premier_janvier)
# Anniversaire d'âge x d'une naissance de composantes cn (T2) : 1er janvier
# de l'année an(n) + x, plus le quantième commun, plus un jour après février
# d'une année bissextile ; une naissance au 29/02 (quantième commun 59) a
# donc son anniversaire le 01/03 des années non bissextiles, le 29/02 des
# années bissextiles.
anniversaire_age <- function(cal, cn, x) {
  i <- indice_annee(cal, cn$an + x)
  cal$premier_janvier[i] + cn$quantieme_commun + (cn$mois > 2 & cal$bissextile[i])
}
# Âge au dernier anniversaire au jour t (T2).
age_dernier_anniversaire <- function(cal, cn, t) {
  x <- annee_calendrier(cal, t) - cn$an
  x - (anniversaire_age(cal, cn, x) > t)
}

# Début de la cellule d'âge x et cellule du jour t, pour les naissances de
# composantes cn, selon la convention retenue.
debut_cellule <- function(cal, cn, x, convention)
  if (convention == "dernier_anniversaire") anniversaire_age(cal, cn, x) else
    cal$premier_janvier[indice_annee(cal, cn$an + x)]
cellule_du_jour <- function(cal, cn, t, convention)
  if (convention == "dernier_anniversaire") age_dernier_anniversaire(cal, cn, t) else
    annee_calendrier(cal, t) - cn$an

# Sous-ensemble i des composantes cn.
extraire_composantes <- function(cn, i) lapply(cn, `[`, i)

# Découpe les intervalles [a, b) (non vides) des observations `obs` en
# morceaux contenus chacun dans une cellule [debut_cellule(x),
# debut_cellule(x+1)). Rend list(obs, x, debut, fin, longueur) : longueur =
# longueur de la cellule (jours).
decouper_cellules <- function(cal, cn, obs, a, b, convention) {
  cn_o <- extraire_composantes(cn, obs)
  x_a <- cellule_du_jour(cal, cn_o, a, convention)
  k <- cellule_du_jour(cal, cn_o, b - 1, convention) - x_a + 1
  rang <- rep.int(seq_along(obs), k)
  x <- x_a[rang] + (seq_along(rang) - rep.int(cumsum(k) - k, k)) - 1
  cn_m <- extraire_composantes(cn_o, rang)
  bas <- debut_cellule(cal, cn_m, x, convention)
  haut <- debut_cellule(cal, cn_m, x + 1, convention)
  list(obs = obs[rang], x = x, debut = pmax(a[rang], bas), fin = pmin(b[rang], haut),
       longueur = haut - bas)
}

# Découpe des morceaux [debut, fin) (non vides) au 1er janvier : rend les
# indices des morceaux répétés, l'année civile et les bornes des sous-morceaux.
decouper_annees <- function(cal, debut, fin) {
  y0 <- annee_calendrier(cal, debut)
  k <- annee_calendrier(cal, fin - 1) - y0 + 1
  rang <- rep.int(seq_along(debut), k)
  y <- y0[rang] + (seq_along(rang) - rep.int(cumsum(k) - k, k)) - 1
  i <- indice_annee(cal, y)
  list(rang = rang, annee = y, debut = pmax(debut[rang], cal$premier_janvier[i]),
       fin = pmin(fin[rang], cal$premier_janvier[i + 1]))
}

# Somme de v par cellule (indices entiers 1..n_cellules, sans NA), dans
# l'ordre des valeurs (rowsum) ; 0 pour une cellule sans valeur. Avec
# reorder = TRUE, les lignes de rowsum() suivent sort(unique(cellule)) :
# l'affectation se fait par ces indices, sans relire les noms de ligne
# (écriture des nombres dépendante de l'option scipen).
somme_par_cellule <- function(cellule, v, n_cellules) {
  r <- numeric(n_cellules)
  if (length(cellule)) {
    s <- rowsum(as.numeric(v), cellule, reorder = TRUE)
    r[sort(unique(cellule))] <- s[, 1L]
  }
  r
}

# Nombre de cellules de la table, en integer (#37) : somme, sur les segments,
# des âges de la plage par le nombre d'années. Le produit est formé en double,
# où il ne déborde pas ; au-delà de .Machine$integer.max la recette refuse
# (motif trop_de_cellules) avant les allocations par cellule : tabulate()
# refuse un nbins d'au moins 2^31 (« attempt to make a table with >= 2^31
# elements », mesuré). Le libellé (deux conversions : le nombre, la limite) et le
# nom du diagnostic se précisent pour la garde de la matrice de présence (#45),
# qui compte des couples (segment, âge) et non des cellules.
nombre_cellules <- function(n_age, n_an,
                            libelle = "La table compterait %s cellules, au-delà de %d : segmentation trop fine ou plage d'âges et d'années trop étendue.",
                            diagnostic = "cellules") {
  n <- sum(as.double(n_age) * n_an)
  if (n > .Machine$integer.max)
    refuser("exposition", "trop_de_cellules",
            formuler(libelle, format(n, scientific = FALSE, trim = TRUE), .Machine$integer.max),
            stats::setNames(list(n), diagnostic))
  as.integer(n)
}

# Fenêtres et événements des observations (T5), sur la période [debut_p, B)
# (jours ; borne = B = fin - délai + 1) et la cause cible. Rend
# list(composantes, calendrier, sortie, a, b, evenement, sortie_concurrente) :
# composantes de la naissance de chaque observation (calculées sur les
# naissances distinctes) ; table calendaire de l'année de naissance la plus
# ancienne à l'année qui suit la cellule de B (âges atteints au plus à B,
# plus une cellule) ; sortie (Inf pour une observation en cours) ; fenêtre
# [a, b), a = max(entrée, D), b = min(sortie, B) ; evenement : cause cible et
# D <= s < B ; sortie_concurrente : autre cause, observation close dans la
# fenêtre. L'observation en cours est exclue par is.finite(sortie) seul :
# preparer_donnees() rend NA la fin de toute observation de cause `en_cours`
# (sortie finie <=> cause différente de `en_cours`, témoin dans
# tests/unitaires/test_exposition.R).
fenetres_observations <- function(observations, debut_p, borne, cible) {
  nai <- as.numeric(observations$date_naissance)
  entree <- as.numeric(observations$debut)
  sortie <- as.numeric(observations$fin); sortie[is.na(sortie)] <- Inf
  cause <- as.character(observations$cause)
  nai_u <- unique(nai)
  cn <- extraire_composantes(composantes_jour(nai_u), match(nai, nai_u))
  cal <- calendrier(min(cn$an, annee_du_jour(debut_p)), annee_du_jour(borne) + 2)
  dans_fenetre <- is.finite(sortie) & sortie >= debut_p & sortie < borne
  list(composantes = cn, calendrier = cal, sortie = sortie,
       a = pmax(entree, debut_p), b = pmin(sortie, borne),
       evenement = dans_fenetre & cause == cible,
       sortie_concurrente = dans_fenetre & cause != cible)
}

# Segments : combinaisons des clés de segmentation. Les clés ne dépendent
# pas des niveaux d'un facteur : `sexe` et un `segment` reçu en facteur ou
# en chaîne sont rendus en chaînes (character) et triés sur ces chaînes en
# ordre d'octets (radix) ; un `segment` entier est rendu en integer et trié
# sur sa valeur. Un segment unique sans segmentation. Rend list(g, segments) :
# g, indice du segment de chaque observation ; segments, data.frame des clés,
# une ligne par segment, dans l'ordre des indices.
segments_observations <- function(observations, segmentation) {
  n_obs <- nrow(observations)
  # Valeurs distinctes (sans NA) en ordre d'octets de la représentation que
  # hache l'empreinte des données (octets_chaines() : enc2utf8, erratum A.3,
  # point 4), donc indépendant du marquage. Le tri radix lève « Character
  # encoding must be UTF-8, Latin-1 or bytes » sur une chaîne non ASCII
  # d'encodage `unknown`, forme que donne read.csv() d'un CSV UTF-8 (#106) :
  # le tri porte sur une copie marquée `bytes`, comparée octet à octet sans
  # traduction, sous C comme sous une locale UTF-8. Avant le tri, la conversion
  # de la règle unique qu'appelle aussi octets_chaines(), vers_utf8_locale()
  # (outils ; #113) : dans une session UTF-8 ou latin-1,
  # toutes les chaînes passent par enc2utf8() ; sinon (locale C) seules les
  # chaînes marquées latin-1 sont converties, les `unknown` gardent leurs
  # octets (enc2utf8 les échapperait en <c3><a9>). Ce n'est pas l'ordre de
  # l'ancien tri radix : mesuré sur c(ÿ latin-1, € UTF-8, "a"), il donnait
  # a, €, ÿ (octet natif ff après e2 82 ac), l'ordre actuel est a, ÿ, € (c3 bf
  # avant e2 82 ac). La branche de locale latin-1 est exercée sur la CI par
  # tests/controles/locale_latin1.R (#115). Les valeurs rendues gardent leur
  # encodage d'origine.
  ordre_octets <- function(v) {
    u <- unique(v)
    u <- u[!is.na(u)]
    if (!is.character(u)) return(u[order(u, method = "radix")])
    w <- vers_utf8_locale(u)
    Encoding(w) <- "bytes"
    u[order(w, method = "radix")]
  }
  if (length(segmentation)) {
    cles <- lapply(observations[segmentation], function(v) if (is.integer(v)) v else as.character(v))
    codes <- lapply(cles, function(v) match(v, ordre_octets(v)))
    o <- do.call(order, c(unname(codes), list(method = "radix")))
    cle_triee <- do.call(paste, c(unname(codes), list(sep = "\r")))[o]
    nouvelle <- c(TRUE, cle_triee[-1L] != cle_triee[-length(cle_triee)])
    g <- integer(n_obs); g[o] <- cumsum(nouvelle)
    segments <- as.data.frame(lapply(cles, function(v) v[o[nouvelle]]), stringsAsFactors = FALSE)
    names(segments) <- segmentation
  } else {
    g <- rep(1L, n_obs)
    segments <- data.frame(row.names = 1L)
  }
  list(g = g, segments = segments)
}

# Position de l'instant t (jour) dans sa cellule, pour les naissances de
# composantes cn, selon la convention : list(x, j, longueur, haut) ; x,
# cellule du jour (T2) ; haut, début de la cellule x + 1 ; longueur =
# haut - début de la cellule x (365 ou 366) ; j = t - début de la cellule x
# (0 <= j < longueur). Âge en années d'âge (T3) : x + j / longueur.
position_dans_cellule <- function(cal, cn, t, convention) {
  x <- cellule_du_jour(cal, cn, t, convention)
  bas <- debut_cellule(cal, cn, x, convention)
  haut <- debut_cellule(cal, cn, x + 1, convention)
  list(x = x, j = t - bas, longueur = haut - bas, haut = haut)
}

# --- fonction d'étape `exposition` ---------------------------------------------

# Étape `exposition` du plan.
#   observations      : data.frame des observations de l'étape `donnees`
#                       (resultat$observations : date_naissance, debut, fin
#                       [NA = en cours], cause, et les rôles de segmentation) ;
#   p                 : valeurs validées de la transition, de la période et du
#                       délai (valeurs_parametres_donnees()) : p$periode,
#                       list(debut, fin), dates, fin incluse, et
#                       p$delai_declaration, entier de jours >= 0, sont
#                       inscrits tels quels dans parametres (#182) ;
#   segmentation      : sous-ensemble de c("sexe", "segment"), vide admis ;
#   recette           : recette validée (deduire_recette(), module donnees) ;
#   conv              : convention validée (resoudre_convention()) ;
#   par_annee         : TRUE pour découper aussi par année civile ;
#   entree_entre_inventaires : hypothèse d'entrée entre inventaires validée,
#                       paramètre conditionnel de la recette
#                       `stocks_successifs` (lu sous cette recette seulement ;
#                       NULL, paramètre absent du plan, sous les autres) ;
#   refuser_vide      : TRUE (défaut) pour refuser la période sans exposition
#                       (motif periode_sans_exposition, ci-dessous) ; FALSE
#                       pour un sous-ensemble de lignes (exposition_lignes()).
# Aucun contrôle (#82 ; ADR 0007, annotation du 17/10/2026, (c) et (d)) : les
# valeurs sont celles que construire_table() a validées (controler_entrees())
# et inscrites dans la fiche ; une seule source pour la recette et la
# convention.
# Rend list(parametres, resultat, diagnostics, avertissements, fenetres) ;
# fenetres (#38) : objet de classe `fenetres_exposition`, fenêtres des
# observations (composantes, calendrier, a, b, evenement :
# fenetres_observations()) et segments (g, segments :
# segments_observations()), avec les paramètres sous lesquels ils sont
# calculés (début D et borne B de la période, code de la transition dont
# l'état d'arrivée fixe l'événement, segmentation) et la convention retenue
# (que fenetres_observations() ne prend pas : elle sert à `brut` de contrôle
# de cohérence avec cette étape), que l'étape `brut` contrôle à l'égalité
# exacte (les observations elles-mêmes
# ne sont pas contrôlées : construire_table() passe celles de l'étape) ; construire_table() le passe à `brut`
# (argument observations) et ne l'inscrit ni dans l'objet résultat ni dans
# la fiche. Aucune copie : les vecteurs sont ceux de f et sg. Les positions
# dans la cellule n'y figurent pas : seul Kaplan-Meier les emploie, et il
# les calcule une fois.
# resultat$exposition : une ligne par cellule (clés de segmentation, age
# [, annee]) sur la plage contiguë des âges, par segment, des cellules qui
# ont une exposition > 0, un événement ou une sortie concurrente ; zéros
# explicites. Colonnes : exposition_jours, exposition_centrale (unité
# parametres$unite_exposition), complement_initial_cellule et
# complement_initial_fenetre (sans par_annee seulement), evenements,
# sorties_concurrentes.
# Refus (#210, M399) : aucune cellule d'exposition strictement positive et
# aucun événement de la transition -> refus_tablexp, module `exposition`,
# motif `periode_sans_exposition` (message : période et fin effective ;
# détails : debut, fin_effective, observations, cellules) ; un événement
# sans exposition ou un vide partiel est produit.
# Aiguillage à deux points (ADR 0016, point 4.1), seuls branchements sur la
# recette hors du module donnees : sur la propriété `seam` de la recette
# (exposition_fenetres() ou exposition_cellules()), puis, dans le seam
# « fenêtres », sur le nom de la recette pour former les fenêtres.
calculer_exposition <- function(observations, p, segmentation, recette, conv, par_annee, entree_entre_inventaires,
                                refuser_vide = TRUE) {
  res <- switch(recette$seam,
    fenetres = exposition_fenetres(observations, p, segmentation, recette, conv, par_annee, entree_entre_inventaires),
    cellules = exposition_cellules(observations, p, segmentation, recette, conv, par_annee),
    stop(sprintf("calculer_exposition() : seam « %s » inconnu (erreur de programmation).", recette$seam),
         call. = FALSE))
  # Période sans exposition (#210, M399) : aucune cellule d'exposition
  # strictement positive (exposition_jours, exposition_centrale ou
  # exposition_initiale, selon les colonnes de la recette) et aucun événement
  # de la transition. Un événement sans exposition (H10b, H11 : état
  # evenement_sans_exposition) n'est pas un vide total ; un vide partiel (une
  # année, un segment, une plage d'âges) est produit. Le plus souvent une
  # erreur de période : la période est déclarée, jamais déduite (E2.3, E2.5).
  # Le refus porte sur la table de la construction : exposition_lignes()
  # (validation) passe refuser_vide = FALSE pour les sous-ensembles de lignes
  # exclues, dont l'exposition peut être légitimement vide (mesuré, #210).
  t <- res$resultat$exposition
  colonnes <- intersect(c("exposition_jours", "exposition_centrale", "exposition_initiale"), names(t))
  if (refuser_vide && !any(vapply(t[colonnes], function(v) any(v > 0, na.rm = TRUE), NA)) && !any(t$evenements > 0L, na.rm = TRUE))
    refuser("exposition", "periode_sans_exposition",
            formuler(paste("Période d'observation sans exposition : aucune exposition ni aucun événement de la",
                           "transition entre %s et la fin effective %s (fin %s, délai %s jour(s)) ; %d observation(s),",
                           "%d cellule(s). Vérifier la période déclarée (une erreur de période est la cause la plus",
                           "fréquente) ; la construction est refusée."),
                     citer(format(p$periode[["debut"]])), citer(format(res$parametres$fin_effective)),
                     citer(format(p$periode[["fin"]])), citer(format(p$delai_declaration)),
                     res$diagnostics$observations, res$diagnostics$cellules),
            list(debut = p$periode[["debut"]], fin_effective = res$parametres$fin_effective,
                 observations = res$diagnostics$observations, cellules = res$diagnostics$cellules))
  res
}

# Seam « fenêtres » (ADR 0016, point 4.2 ; ADR 0014, point 6) : corps de
# calculer_exposition() extrait à l'identique (#169, commit (3b)) ; mêmes
# arguments, même valeur rendue (ci-dessus). Les observations à fenêtres
# (date_naissance, debut, fin, cause et clés de segmentation) sont formées
# par la recette — celles de l'étape donnees telles quelles pour
# `lignes_dates_completes` ; une par unité exposable, de début et de fin
# fixés par fenetres_stocks_successifs() pour `stocks_successifs` (qui rend
# aussi ses paramètres retenus, diagnostics et avertissements propres,
# ajoutés en fin des listes communes) — ; fenetres_observations(), le
# découpage en cellules, les segments, les sommes et l'objet
# `fenetres_exposition` sont communs.
exposition_fenetres <- function(observations, p, segmentation, recette, conv, par_annee, entree_entre_inventaires) {
  cv <- conv$retenue
  cible <- p$transition$arrivee
  debut_p <- p$debut; borne <- p$borne
  fs <- switch(recette$recette,
    lignes_dates_completes = list(observations = observations, avertissements = avertissements_vides()),
    stocks_successifs = fenetres_stocks_successifs(observations, p, segmentation, entree_entre_inventaires),
    stop(sprintf("exposition_fenetres() : recette « %s » sans fenêtres (erreur de programmation).", recette$recette),
         call. = FALSE))
  observations <- fs$observations
  manquantes <- setdiff(c("date_naissance", "debut", "fin", "cause", segmentation), names(observations))
  if (length(manquantes))
    stop(sprintf("exposition_fenetres() : colonne(s) %s absente(s) des observations (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  n_obs <- nrow(observations)

  # Fenêtre (T5) : troncature au début de la période, censure à B ;
  # composantes des naissances et table calendaire.
  f <- fenetres_observations(observations, debut_p, borne, cible)
  cn <- f$composantes; cal <- f$calendrier; sortie <- f$sortie
  a <- f$a; b <- f$b
  exposees <- which(a < b)
  m <- decouper_cellules(cal, cn, exposees, a[exposees], b[exposees], cv)
  if (par_annee) {
    if (cv == "dernier_anniversaire") {
      ya <- decouper_annees(cal, m$debut, m$fin)
      m <- list(obs = m$obs[ya$rang], x = m$x[ya$rang], debut = ya$debut, fin = ya$fin,
                longueur = m$longueur[ya$rang], annee = ya$annee)
    } else {
      m$annee <- cn$an[m$obs] + m$x
    }
  }
  m$jours <- m$fin - m$debut

  # Événements de la transition (D <= s < B) et sorties concurrentes (autre
  # cause, observation close dans la fenêtre), attribués à la cellule du jour s.
  ev <- which(f$evenement)
  sc <- which(f$sortie_concurrente)
  attribuer <- function(i) {
    pos <- position_dans_cellule(cal, extraire_composantes(cn, i), sortie[i], cv)
    list(obs = i, x = pos$x, annee = annee_calendrier(cal, sortie[i]), haut = pos$haut,
         longueur = pos$longueur)
  }
  e <- attribuer(ev); s <- attribuer(sc)

  # Segments (segments_observations()).
  sg <- segments_observations(observations, segmentation)
  g <- sg$g; segments <- sg$segments
  n_seg <- nrow(segments)

  # Plage contiguë des âges par segment, et des années si par_annee.
  gm <- g[m$obs]; ge <- g[e$obs]; gs <- g[s$obs]
  xs <- c(m$x, e$x, s$x); gx <- c(gm, ge, gs)
  age_min <- age_max <- rep(NA_real_, n_seg)
  if (length(xs)) {
    # Présence par (segment, âge) sur la plage globale des âges, puis
    # premier et dernier âge présents de chaque segment.
    x0 <- min(xs); etendue <- max(xs) - x0 + 1
    # Garde de la matrice de présence (#45) : n_seg · etendue est la taille de
    # cette matrice (couples (segment, âge) de la plage globale). Sans
    # par_annee, c'est un majorant du nombre de cellules, qui peut être
    # lâche ; avec par_annee, ce n'en est pas nécessairement un (la table
    # compte aussi les années). Même motif que plus bas, avant le tabulate()
    # qui refuserait un nbins d'au moins 2^31.
    nombre_cellules(etendue, n_seg,
                    "Le décompte de présence des âges compterait %s couples (segment, âge), au-delà de %d : segmentation trop fine ou plage d'âges trop étendue.",
                    "couples_presence")
    presence <- matrix(tabulate((gx - 1) * etendue + (xs - x0) + 1, nbins = n_seg * etendue) > 0L,
                       nrow = etendue)
    occupe <- colSums(presence) > 0
    age_min[occupe] <- x0 - 1 + apply(presence[, occupe, drop = FALSE], 2L, which.max)
    age_max[occupe] <- x0 + etendue - apply(presence[etendue:1, occupe, drop = FALSE], 2L, which.max)
  }
  n_age <- ifelse(is.na(age_min), 0, age_max - age_min + 1)
  annees <- if (par_annee) annee_du_jour(debut_p):annee_du_jour(borne - 1) else 0
  n_an <- length(annees)
  decalage <- c(0, cumsum(n_age * n_an))[seq_len(n_seg)]
  n_cel <- nombre_cellules(n_age, n_an)
  indice <- function(gg, x, y) {
    i <- decalage[gg] + (x - age_min[gg]) * n_an + 1
    if (par_annee) i + (y - annees[1L]) else i
  }
  im <- indice(gm, m$x, m$annee); ie <- indice(ge, e$x, e$annee); is_ <- indice(gs, s$x, s$annee)

  rang_seg <- rep.int(seq_len(n_seg), n_age * n_an)
  table_exp <- segments[rang_seg, , drop = FALSE]
  rownames(table_exp) <- NULL
  age <- unlist(lapply(seq_len(n_seg), function(k)
    if (n_age[k]) rep(seq(age_min[k], age_max[k]), each = n_an) else numeric(0)))
  table_exp$age <- as.integer(age)
  if (par_annee) table_exp$annee <- as.integer(rep(annees, length.out = n_cel))
  table_exp$exposition_jours <- somme_par_cellule(im, m$jours, n_cel)
  table_exp$exposition_centrale <- somme_par_cellule(im, m$jours / m$longueur, n_cel)
  if (!par_annee) {
    # Compléments de l'exposition initiale (A.3), en unité d'exposition :
    # c1 jusqu'à la fin de la cellule, c2 jusqu'à min(fin de la cellule, B).
    table_exp$complement_initial_cellule <-
      somme_par_cellule(ie, (e$haut - sortie[ev]) / e$longueur, n_cel)
    table_exp$complement_initial_fenetre <-
      somme_par_cellule(ie, (pmin(e$haut, borne) - sortie[ev]) / e$longueur, n_cel)
  }
  table_exp$evenements <- tabulate(ie, nbins = n_cel)
  # Sorties concurrentes : toutes les sorties dans la fenêtre dont la cause
  # n'est pas la cible (hors observation en cours), censures programmées
  # comprises (terme, fin de garantie ; composante de tête close sans décès).
  table_exp$sorties_concurrentes <- tabulate(is_, nbins = n_cel)

  sans_exposition <- sum(table_exp$evenements > 0L & table_exp$exposition_jours == 0)
  avert <- avertissements_vides()
  if (conv$forcee)
    avert <- avertir(avert, "exposition", "convention_forcee",
                     sprintf("Convention d'âge forcée : « %s » au lieu de « %s » (convention native de la recette « %s »).",
                             cv, conv$native, recette$recette), 1)
  if (sans_exposition > 0L)
    avert <- avertir(avert, "exposition", "evenement_sans_exposition",
                     sprintf("Relevé : %d cellule(s) à exposition nulle qui comptent au moins un événement.",
                             sans_exposition), sans_exposition)
  # Avertissements propres à la recette (aucun sous `lignes_dates_completes`).
  for (k in seq_len(nrow(fs$avertissements)))
    avert <- avertir(avert, "exposition", fs$avertissements$code[k], fs$avertissements$message[k],
                     fs$avertissements$nombre[k])

  list(
    parametres = c(list(recette = recette$recette, convention = conv,
                        # Texte du moteur marqué UTF-8 en toute locale (#50).
                        unite_exposition = texte_moteur(UNITES_EXPOSITION[[cv]]), par_annee = par_annee,
                        transition = p$transition$code, segmentation = segmentation,
                        periode = p$periode, delai_declaration = p$delai_declaration,
                        fin_effective = structure(borne - 1, class = "Date"),
                        hypothese_censure = texte_moteur(HYPOTHESE_CENSURE)),
                   fs$parametres),
    resultat = list(exposition = table_exp),
    diagnostics = c(list(observations = n_obs, observations_exposees = length(exposees),
                         exposition_jours = sum(m$jours), evenements = length(ev),
                         sorties_concurrentes = length(sc), cellules = n_cel,
                         cellules_evenement_sans_exposition = sans_exposition),
                    fs$diagnostics),
    avertissements = avert,
    fenetres = structure(c(f[c("composantes", "calendrier", "a", "b", "evenement")], sg,
                           list(debut = debut_p, borne = borne, transition = p$transition$code, convention = cv,
                                segmentation = segmentation)),
                         class = "fenetres_exposition")
  )
}

# --- seam « cellules » (ADR 0016, point 4.3 ; #169, § 5) ------------------------

# Unité de l'exposition reçue et hypothèse sur les sorties concurrentes sous
# le seam « cellules » (#169, § 10), inscrites dans les paramètres de l'étape
# et dans la fiche à la place de UNITES_EXPOSITION et de HYPOTHESE_CENSURE.
UNITE_EXPOSITION_DECLAREE <- "déclarée : années de la convention déclarée, définition des jours non vérifiable"
HYPOTHESE_CENSURE_DECLAREE <- "traitement des sorties concurrentes fait par le producteur, non vérifiable"
# Statut commun des hypothèses de la recette `agregats` (#169, § 10).
STATUT_HYPOTHESE_DECLAREE <- "hypothèse déclarée, non vérifiable par le moteur"

# Hypothèses de la recette `agregats` (#169, § 10 ; champ conditionnel
# hypotheses_recette, ADR 0016, point 8.1) : data.frame (code, enonce, nature,
# statut), une ligne par hypothèse, celles des expositions pour les seules
# expositions déclarées. Textes du moteur marqués UTF-8 (#50).
#   declarees : rôles d'exposition déclarés ; annee : rôle annee déclaré ;
#   convention : convention retenue (déclarée).
hypotheses_agregats <- function(declarees, annee, convention) {
  h <- data.frame(
    code = c("exposition_centrale_declaree", "exposition_initiale_declaree", "unite_exposition",
             "convention_declaree", "une_annee_d_age", "periode_delai_producteur", "evenements_transition",
             "unite_observation"),
    enonce = c(
      paste("exposition centrale E^c reçue : somme des durées de présence dans la cellule, en années de la",
            "convention déclarée ; ni recalculée ni vérifiée par le moteur"),
      paste("exposition initiale E^i reçue, de définition « cellule » : E^c plus, pour chaque événement de la",
            "transition, le complément jusqu'à la fin de la cellule, aucun complément pour une sortie",
            "concurrente ; aucune exposition n'est déduite de l'autre"),
      paste("exposition reçue en années de la convention déclarée (jours / L, L nombre de jours de l'année d'âge",
            "sous dernier_anniversaire, de l'année civile sous age_atteint_annee : 365 ou 366), sans conversion ;",
            "un décompte en jours / 365 ou jours / 365,25 l'écarte d'au plus 1/365 en relatif (borne exacte)"),
      sprintf(paste("âge des cellules (et année, si le rôle annee est déclaré) formés par le producteur sous la",
                    "convention déclarée « %s », retenue telle quelle ; aucune convention n'est déduite des",
                    "données ni forcée"), convention),
      paste("une valeur de age désigne une seule année d'âge (ni classe ouverte, ni classe quinquennale) ;",
            "une convention hors catalogue n'est pas déclarée sous l'une des deux conventions"),
      paste("période d'observation et délai de déclaration appliqués par le producteur de l'agrégat",
            "(le moteur ne tronque pas une cellule agrégée)"),
      paste("événements reçus : ceux de la transition du plan, en nombres d'unités non pondérés (E2.8)"),
      paste("unité d'observation non déterminée : rien n'indique si les effectifs comptent des têtes ou des",
            "contrats")),
    nature = c("hypothèse de données", "hypothèse de données", "hypothèse de données", "convention",
               "hypothèse de données", "hypothèse de données", "hypothèse de données", "hypothèse de données"),
    statut = c(STATUT_HYPOTHESE_DECLAREE, STATUT_HYPOTHESE_DECLAREE, STATUT_HYPOTHESE_DECLAREE,
               STATUT_HYPOTHESE_DECLAREE, paste0(STATUT_HYPOTHESE_DECLAREE, " ; relevé ages_manquants"),
               if (annee) paste("hypothèse déclarée, vérifiable en partie par le moteur : motif annee_hors_periode",
                                "(années civiles hors de la période ; troncature et délai à l'intérieur d'une année",
                                "non vérifiables)")
               else STATUT_HYPOTHESE_DECLAREE,
               STATUT_HYPOTHESE_DECLAREE, paste0(STATUT_HYPOTHESE_DECLAREE, " ; avertissement unite_agregat")),
    stringsAsFactors = FALSE)
  garder <- c("exposition_centrale" %in% declarees, "exposition_initiale" %in% declarees, rep(TRUE, 6L))
  h <- h[garder, , drop = FALSE]
  rownames(h) <- NULL
  texte_moteur(h)
}

# Seam « cellules » (ADR 0016, point 4.3) : somme des cellules reçues par clé
# (segmentation, age, annee si par_annee), sans en créer aucune (#169, § 5).
#   observations : cellules conservées par l'étape donnees
#                  (observations_agregats() : clés de segmentation, age,
#                  [annee], expositions déclarées, evenements) ;
#   p, segmentation, recette, conv, par_annee : comme calculer_exposition().
# Sommes : D décompte exact (somme de doubles entiers, rendue en integer) ;
# E en arithmétique flottante, somme dans l'ordre des lignes (rowsum(), comme
# somme_par_cellule()) : une permutation des lignes reçues peut changer les
# derniers bits d'une somme (même règle que #39) ; une cellule sans somme
# garde sa valeur exacte. Ordre des segments : segments_observations() ;
# dans un segment, âge puis année croissants (ordre de la recette des lignes).
# resultat$exposition : clés, age, [annee], expositions déclarées
# (exposition_centrale puis exposition_initiale, double), evenements
# (integer) ; aucune colonne propre aux lignes (exposition_jours,
# sorties_concurrentes, complement_initial_*), ni en 0 ni en NA. Aucun objet
# `fenetres_exposition` (Kaplan-Meier est refusé par la matrice). Relevés :
# annees_sommees (annee déclarée, par_annee = FALSE), ages_manquants (trous
# de la plage d'âges de chaque segment, de chaque (segment, année) sous
# par_annee), evenement_sans_exposition
# (événements d'une cellule d'exposition centrale nulle, ou d'exposition
# initiale nulle sans exposition centrale déclarée) ; convention_declaree
# (convention sans native : déclarée, non vérifiable).
exposition_cellules <- function(observations, p, segmentation, recette, conv, par_annee) {
  cv <- conv$retenue
  declarees <- intersect(c("exposition_centrale", "exposition_initiale"), names(observations))
  manquantes <- setdiff(c("age", "evenements", segmentation, if (par_annee) "annee"), names(observations))
  if (length(manquantes) || !length(declarees))
    stop(sprintf("exposition_cellules() : colonne(s) %s absente(s) des observations (erreur de programmation).",
                 paste(c(manquantes, if (!length(declarees)) "exposition"), collapse = ", ")), call. = FALSE)
  n_obs <- nrow(observations)
  sg <- segments_observations(observations, segmentation)
  g <- sg$g; segments <- sg$segments
  age <- observations$age
  annee <- if (par_annee) observations$annee else rep(0, n_obs)
  o <- order(g, age, annee, method = "radix")
  # Nouvelle cellule à chaque changement de clé dans l'ordre trié (égalité
  # exacte, sans écriture décimale).
  gs <- g[o]; as_ <- age[o]; ys <- annee[o]
  nouvelle <- if (n_obs) c(TRUE, gs[-1L] != gs[-n_obs] | as_[-1L] != as_[-n_obs] | ys[-1L] != ys[-n_obs])
              else logical(0)
  cellule <- integer(n_obs); cellule[o] <- cumsum(nouvelle)
  n_cel <- sum(nouvelle)
  premiere <- o[nouvelle]
  table_exp <- segments[g[premiere], , drop = FALSE]
  rownames(table_exp) <- NULL
  table_exp$age <- as.integer(age[premiere])
  if (par_annee) table_exp$annee <- as.integer(annee[premiere])
  for (r in declarees) table_exp[[r]] <- somme_par_cellule(cellule, observations[[r]], n_cel)
  d <- somme_par_cellule(cellule, observations$evenements, n_cel)
  if (any(d > .Machine$integer.max))
    refuser("exposition", "evenements_hors_plage",
            formuler(paste("Somme des événements hors de la plage des entiers du moteur (au plus %d) dans %d",
                           "cellule(s) ; vérifier la colonne du rôle « evenements » ; la construction est refusée."),
                     .Machine$integer.max, sum(d > .Machine$integer.max)),
            list(cellules = sum(d > .Machine$integer.max)))
  table_exp$evenements <- as.integer(d)
  # Total des événements (#199, M397 ; ADR 0009, point 9.6, (d) 3) : formé en
  # double, rangé en integer par denombrement_entier(), qui refuse au-delà de
  # .Machine$integer.max (sum() d'un integer rendrait sinon un double, sans
  # avertissement). Après la garde par cellule ; les sommes des étapes en aval
  # portent sur des sous-ensembles de ces cellules.
  evenements_total <- denombrement_entier(sum(d), "exposition", "evenements")

  # Relevés.
  sommees <- !par_annee && "annee" %in% names(observations)
  sans_exposition <- sum(table_exp$evenements > 0L & table_exp[[declarees[1L]]] == 0)
  # Trous dans la plage d'âges de chaque (segment, année) sous par_annee, de
  # chaque segment sinon (spécification de #169, § 6.3, révisée par actuary) :
  # cellules (âge entier) absentes entre l'âge minimal et l'âge maximal reçus.
  trous <- 0
  if (n_cel) {
    groupe <- if (par_annee) paste(g[premiere], annee[premiere], sep = "\r") else g[premiere]
    plage <- vapply(split(table_exp$age, groupe), function(a) max(a) - min(a) + 1, numeric(1L))
    trous <- sum(plage) - n_cel
  }
  avert <- avertissements_vides()
  if (is.na(conv$native))
    avert <- avertir(avert, "exposition", "convention_declaree",
                     sprintf("Convention d'âge déclarée « %s », non vérifiable sur des agrégats.", cv), 1)
  # Sous dernier_anniversaire avec exposition_initiale déclarée, le texte
  # précise que la somme des E^i des carrés de Lexis n'est pas l'E^i de
  # l'année d'âge (texte visé par actuary ; binomial refusé par
  # regles_conditionnelles_recette()).
  if (sommees)
    avert <- avertir(avert, "exposition", "annees_sommees",
                     if ("exposition_initiale" %in% declarees && identical(cv, "dernier_anniversaire"))
                       paste("Rôle « annee » déclaré sous par_annee = FALSE : exposition et événements sommés sur",
                             "les années de chaque cellule (somme exacte pour l'exposition centrale et les",
                             "événements ; sous « dernier_anniversaire », la somme des expositions initiales des",
                             "carrés de Lexis n'est pas l'exposition initiale de l'année d'âge : la colonne",
                             "exposition_initiale est la somme des valeurs reçues, l'estimateur « binomial » est",
                             "refusé).")
                     else
                       paste("Rôle « annee » déclaré sous par_annee = FALSE : exposition et événements sommés sur",
                             "les années de chaque cellule (somme exacte pour l'exposition centrale et les",
                             "événements)."), 1)
  if (trous > 0)
    avert <- avertir(avert, "exposition", "ages_manquants",
                     sprintf(paste("Relevé : %.0f cellule(s) absente(s) de la plage d'âges de leur segment%s",
                                   "(classes d'âge groupées possibles ; aucune cellule n'est créée)."),
                             trous, if (par_annee) " et de leur année civile" else ""), trous)
  if (sans_exposition > 0L)
    avert <- avertir(avert, "exposition", "evenement_sans_exposition",
                     sprintf("Relevé : %d cellule(s) à exposition nulle qui comptent au moins un événement.",
                             sans_exposition), sans_exposition)

  list(
    parametres = list(recette = recette$recette, convention = conv,
                      unite_exposition = texte_moteur(UNITE_EXPOSITION_DECLAREE), par_annee = par_annee,
                      transition = p$transition$code, segmentation = segmentation,
                      periode = p$periode, delai_declaration = p$delai_declaration,
                      fin_effective = structure(p$borne - 1, class = "Date"),
                      hypothese_censure = texte_moteur(HYPOTHESE_CENSURE_DECLAREE),
                      hypotheses_recette = hypotheses_agregats(declarees, "annee" %in% names(observations), cv)),
    resultat = list(exposition = table_exp),
    diagnostics = list(observations = n_obs, evenements = evenements_total, cellules = n_cel,
                       cellules_evenement_sans_exposition = sans_exposition,
                       cellules_absentes_de_la_plage = as.integer(trous)),
    avertissements = avert
  )
}

# --- recette `stocks_successifs` au seam « fenêtres » (#180, § 5 et § 6) --------

# Hypothèses nommées de la recette `stocks_successifs` (spécification
# d'actuary-approfondi de #180, § 6, M328, et amendement A1, § A1.6, M339 ;
# champ conditionnel hypotheses_recette, ADR 0016, point 8.1) : data.frame
# (code, enonce, nature, statut), neuf lignes dans l'ordre de la
# spécification, `exclusion_par_unite` en dernier ; énoncé et statut des
# lignes entree_entre_inventaires et entres_sortis selon l'hypothèse
# d'entrée déclarée. Le statut porte le contrôle de la spécification. Textes
# du moteur marqués UTF-8 (#50).
#   entree : hypothèse d'entrée entre inventaires validée
#            (VALEURS_ENTREE_ENTRE_INVENTAIRES).
hypotheses_stocks <- function(entree) {
  milieu <- identical(entree, "milieu")
  h <- data.frame(
    code = c("instant_inventaire", "grille_periode", "inventaires_exhaustifs", "evenements_exhaustifs",
             "presence_continue", "entree_entre_inventaires", "entres_sortis", "fin_au_dernier_inventaire",
             "exclusion_par_unite"),
    enonce = c(
      paste("une ligne d'inventaire datée d atteste la présence de l'unité à l'instant d au sens d'A.0",
            "(entrée e <= d < sortie s : état arrêté en fin de journée)"),
      paste("premier inventaire t_1 = veille du début de la période, dernier inventaire t_M = fin effective",
            "(fin moins le délai de déclaration)"),
      "chaque inventaire recense toutes les unités en vigueur à sa date",
      "toute sortie survenue jusqu'à la fin effective figure au fichier des événements",
      "aucune sortie suivie d'une remise en vigueur entre deux présences d'une même unité",
      if (milieu)
        paste("entree_entre_inventaires = « milieu » : une unité vue pour la première fois à l'inventaire t_f",
              "(f >= 2) entre au jour t_{f-1} + plafond((t_f - t_{f-1}) / 2) ; une unité entrée et sortie entre",
              "t_i et t_{i+1} (sortie s) entre au jour t_i + plafond((s - t_i) / 2) ; une unité présente à t_1",
              "entre à t_1")
      else
        paste("entree_entre_inventaires = « premier_inventaire » : une unité vue pour la première fois à",
              "l'inventaire t_f (f >= 2) entre à t_f (troncature à gauche) ; une unité présente à t_1 entre à t_1"),
      if (milieu)
        paste("unités entrées et sorties entre deux inventaires (sans ligne d'inventaire) : fenêtre imputée",
              "sous l'hypothèse « milieu », sorties prises en compte (événement de la transition ou sortie",
              "concurrente)")
      else
        paste("unités entrées et sorties entre deux inventaires (sans ligne d'inventaire) : sans fenêtre sous",
              "l'hypothèse « premier_inventaire », sorties écartées et décomptées par cause"),
      paste("unité présente au dernier inventaire t_M : censurée à B = fin effective + 1 jour, sans événement",
            "compté après la fin effective"),
      paste("l'exclusion déclarée d'un motif de ligne écarte toutes les lignes de l'unité (même identifiant) ;",
            "une ligne sans identifiant valide n'écarte qu'elle-même ; aucune histoire d'unité identifiée n'est",
            "tronquée par une exclusion")),
    nature = c("convention", "convention", "hypothèse de données", "hypothèse de données", "hypothèse de données",
               "hypothèse de modèle", "conséquence du mode", "exact sous hypothèses", "convention"),
    statut = c(
      "convention (A.0), sans contrôle propre",
      "convention ; contrôle : refus inventaires_periode",
      paste("hypothèse de données, vérifiée en partie : motifs sortie_non_renseignee, presence_interrompue ;",
            "refus inventaire_sans_ligne"),
      "hypothèse de données, vérifiée en partie : motif sortie_non_renseignee",
      paste("hypothèse de données, non vérifiable : une remise en vigueur visible est relevée par les motifs",
            "presence_apres_evenement et presence_interrompue (refus par défaut) ; une remise en vigueur entre",
            "deux inventaires ne laisse aucune trace"),
      if (milieu)
        paste("approximation de modélisation : écart à l'instant d'entrée inconnu au plus égal à la partie",
              "entière de L / 2 jours par unité (L : longueur de l'intervalle des instants compatibles ;",
              "majoration certaine, atteinte pour une unité vue pour la première fois à un inventaire) ;",
              "contrôle : constat de simulation (effet moyen sur les taux)")
      else
        paste("hypothèse de modèle : troncature à gauche non informative à t_f, la même que la troncature au",
              "début de la période sous la recette des lignes ; exposition exacte, en jours entiers, pour ce",
              "schéma d'observation ; contrôle : constat de simulation"),
      "conséquence du mode ; relevé entres_sortis_entre_inventaires (décompte par cause)",
      paste("exact sous les hypothèses inventaires_exhaustifs, evenements_exhaustifs et presence_continue"),
      "convention ; relevé unites_exclues_par_propagation"),
    stringsAsFactors = FALSE)
  texte_moteur(h)
}

# Observations à fenêtres de la recette `stocks_successifs` (spécification de
# #180, § 5 ; ADR 0016, point 4.2) : une par unité exposable, dans l'ordre des
# unités de l'étape donnees (observations_stocks()), colonnes date_naissance,
# clés de segmentation, debut (â), fin (ŝ ; NA pour une unité censurée) et
# cause (« en_cours » pour une unité censurée), que exposition_fenetres()
# passe à fenetres_observations() (a = max(â, D), b = min(ŝ, B)) :
#   - début â : unité présente à t_1 (P0), â = t_1 ; unité vue pour la
#     première fois à t_f, f >= 2 (E), â = t_f sous « premier_inventaire »,
#     â = t_{f-1} + plafond((t_f - t_{f-1}) / 2) sous « milieu » ; unité
#     entrée et sortie entre t_i et t_{i+1} (ES, sortie s), sans fenêtre sous
#     « premier_inventaire » (unité écartée, sorties non comptées),
#     â = t_i + plafond((s - t_i) / 2) sous « milieu » ; unité à événement
#     hors de ]t_1, t_M] sans inventaire (HF) : sans fenêtre ;
#   - fin ŝ : sortie s (fin S), +Inf (fin C : présente à t_M, censurée à B).
# Jours entiers ; t_i < â <= t_{i+1} (ou s) : le début imputé reste dans la
# période. Un instant d'événement d'une unité exposable est sa date de
# sortie exacte (aucune hypothèse sur les sorties ni sur les âges).
#   observations : observations de l'étape donnees (observations_stocks()) ;
#   p            : valeurs validées, dont p$inventaires (grille canonique) ;
#   segmentation : clés de segmentation du plan ;
#   entree       : hypothèse d'entrée validée.
# Rend list(observations, parametres (entree_entre_inventaires,
# hypotheses_recette), diagnostics (unites_sans_fenetre : ES écartées et HF ;
# fenetres_vides : unités gardées de fenêtre vide, max(â, D) >= min(ŝ, B),
# sans avertissement (amendement mineur du § 7.5 par actuary) ;
# exposition_imputee_jours : jours de [â, t_f) des unités E et de [â, s)
# des unités ES sous « milieu », 0 sous « premier_inventaire » : part de
# l'exposition qui repose sur l'hypothèse), avertissements (relevés
# entres_sortis_entre_inventaires, par cause, et, sous « milieu »,
# intervalle_inventaires_long : intervalle de plus de 366 jours)).
fenetres_stocks_successifs <- function(observations, p, segmentation, entree) {
  if (is.null(p$inventaires) || !chaine_unique(entree) || !(entree %in% VALEURS_ENTREE_ENTRE_INVENTAIRES))
    stop(paste("fenetres_stocks_successifs() : grille des inventaires absente de p ou hypothèse d'entrée hors des",
               "valeurs admises (erreur de programmation)."), call. = FALSE)
  grille <- as.numeric(unclass(p$inventaires))
  milieu <- identical(entree, "milieu")
  classe <- observations$classe
  f <- observations$premier_inventaire; i <- observations$intervalle
  s <- as.numeric(unclass(observations$date_sortie))
  cause_u <- observations$cause
  censure <- !is.na(observations$fin) & observations$fin == "C"
  p0 <- classe == "P0"; e <- classe == "E"; es <- classe == "ES"
  garder <- p0 | e | (milieu & es)
  debut <- rep(NA_real_, length(classe))
  debut[p0] <- grille[1L]
  if (milieu) {
    debut[e] <- grille[f[e] - 1L] + ceiling((grille[f[e]] - grille[f[e] - 1L]) / 2)
    debut[es] <- grille[i[es]] + ceiling((s[es] - grille[i[es]]) / 2)
  } else {
    debut[e] <- grille[f[e]]
  }
  fin <- s; fin[censure] <- NA_real_
  fin_inf <- fin; fin_inf[is.na(fin_inf)] <- Inf
  cause <- cause_u; cause[censure] <- "en_cours"
  imputee <- if (milieu) sum(grille[f[e]] - debut[e]) + sum(s[es] - debut[es]) else 0
  o <- observations[garder, c("date_naissance", segmentation), drop = FALSE]
  rownames(o) <- NULL
  o$debut <- structure(debut[garder], class = "Date")
  o$fin <- structure(fin[garder], class = "Date")
  o$cause <- cause[garder]

  avert <- avertissements_vides()
  causes <- setdiff(MODALITES_CAUSE, "en_cours")
  n_es <- sum(es)
  if (n_es > 0L) {
    detail <- paste(sprintf("%s : %d", causes,
                            vapply(causes, function(x) sum(es & !is.na(cause_u) & cause_u == x), integer(1L))),
                    collapse = ", ")
    avert <- avertir(avert, "exposition", "entres_sortis_entre_inventaires",
                     if (milieu)
                       sprintf(paste("Relevé : %d unité(s) entrée(s) et sortie(s) entre deux inventaires, sans ligne",
                                     "d'inventaire : fenêtre imputée sous l'hypothèse d'entrée « milieu » (entrée au",
                                     "milieu de l'intervalle des instants compatibles), sorties prises en compte,",
                                     "événement de la transition ou sortie concurrente (%s)."),
                               n_es, detail)
                     else
                       sprintf(paste("Relevé : %d unité(s) entrée(s) et sortie(s) entre deux inventaires, sans ligne",
                                     "d'inventaire : sans fenêtre sous l'hypothèse d'entrée « premier_inventaire »,",
                                     "sorties écartées, non comptées (%s)."), n_es, detail),
                     n_es)
  }
  longs <- if (milieu) sum(diff(grille) > 366) else 0L
  if (longs > 0L)
    avert <- avertir(avert, "exposition", "intervalle_inventaires_long",
                     sprintf(paste("Relevé : %d intervalle(s) entre inventaires de plus de 366 jours sous l'hypothèse",
                                   "d'entrée « milieu » (l'écart à l'instant d'entrée inconnu, borné par la moitié de",
                                   "l'intervalle, croît avec sa longueur)."), longs),
                     longs)
  list(observations = o,
       parametres = list(entree_entre_inventaires = entree, hypotheses_recette = hypotheses_stocks(entree)),
       diagnostics = list(unites_sans_fenetre = sum(!garder), exposition_imputee_jours = imputee,
                          fenetres_vides = sum(pmax(debut[garder], p$debut) >= pmin(fin_inf[garder], p$borne))),
       avertissements = avert)
}

# --- plage d'âges et grille des cellules (ADR 0017, point 10.1) ---------------

# Conventions d'âge admises (M255, point 3) : cellule [x, x+1).
CONVENTIONS_VALIDATION <- "dernier_anniversaire"

# Étapes admises par controler_plage_ages() : chacune donne au refus son
# module (F2 de #167). Liste blanche propre à ce module, qui ne lit pas
# ETAPES_TABLE (module positionnement, de rang supérieur) ; elle ne nomme que
# des étapes qui existent.
ETAPES_PLAGE_AGES <- c("positionnement", "validation", "homogeneite")

# Forme de la plage d'âges (plage_ages_invalide), sous le module de l'étape
# `etape` (F2 ; extraite de controler_parametres_validation(), messages de
# `validation` inchangés). Rend TRUE, invisible.
controler_plage_ages <- function(plage_ages, etape) {
  # Admise aussi pour l'étape `homogeneite` (#168, module validation), qui ne
  # lit aucune table : même forme de plage_ages que `validation`.
  if (!(etape %in% ETAPES_PLAGE_AGES))
    stop("controler_plage_ages() : étape inconnue (erreur de programmation).", call. = FALSE)
  if (!valeurs_entieres_double(plage_ages) || length(plage_ages) != 2L || plage_ages[1L] < 0 ||
      plage_ages[1L] > plage_ages[2L] || plage_ages[2L] > .Machine$integer.max)
    refuser(etape, "plage_ages_invalide",
            formuler(paste("La plage d'âges (plage_ages) doit être un vecteur double sans attribut de deux valeurs",
                           "entières finies, 0 <= min <= max (un integer est refusé).")),
            list(plage_ages = plage_ages))
  invisible(TRUE)
}

# Grille des cellules de la plage (M253, points 2 et 5 ; extraite
# d'apparier_table(), ADR 0017, point 10.1, arithmétique inchangée).
#   exposition : table de l'étape `exposition` ; segmentation, par_annee ;
#   plage_ages : plage contrôlée (controler_plage_ages()).
# Grille : segments présents dans la table d'exposition (dans son ordre) x
# âges de la plage (x années de la table d'exposition si par_annee), E^c = 0
# et D = 0 pour une cellule sans ligne d'exposition ; aucun segment absent
# n'est créé. Aucun refus. Rend
# list(cellules (clés, age [, annee], exposition_centrale, evenements),
# segment (indice du segment de chaque cellule), ligne (integer : ligne de la
# table d'exposition de chaque cellule, NA sans ligne ; les lignes de la
# sortie de `brut` sont alignées sur celles de la table d'exposition),
# hors_plage (cellules, evenements, exposition)).
grille_plage <- function(exposition, segmentation, par_annee, plage_ages) {
  cles_seg <- if (length(segmentation))
    do.call(paste, c(unname(as.list(exposition[segmentation])), list(sep = "\r"))) else rep("", nrow(exposition))
  premiers <- !duplicated(cles_seg)
  segments <- exposition[premiers, segmentation, drop = FALSE]
  rownames(segments) <- NULL
  g_expo <- match(cles_seg, cles_seg[premiers])
  n_seg <- sum(premiers)
  ages <- seq(as.integer(plage_ages[1L]), as.integer(plage_ages[2L]))
  annees <- if (isTRUE(par_annee)) sort(unique(exposition$annee)) else NA_integer_
  n_age <- length(ages); n_an <- length(annees)
  n <- n_seg * n_age * n_an
  # Ordre de la grille : segment, âge, année (ordre de la table d'exposition).
  g <- rep(seq_len(n_seg), each = n_age * n_an)
  cellules <- segments[g, , drop = FALSE]
  rownames(cellules) <- NULL
  cellules$age <- rep(rep(ages, each = n_an), times = n_seg)
  if (isTRUE(par_annee)) cellules$annee <- as.integer(rep(annees, times = n_seg * n_age))
  dans <- exposition$age >= plage_ages[1L] & exposition$age <= plage_ages[2L]
  i <- ((g_expo[dans] - 1) * n_age + (exposition$age[dans] - ages[1L])) * n_an +
    (if (isTRUE(par_annee)) match(exposition$annee[dans], annees) else 1)
  Ec <- numeric(n); D <- integer(n)
  Ec[i] <- exposition$exposition_centrale[dans]
  D[i] <- exposition$evenements[dans]
  ligne <- rep(NA_integer_, n)
  ligne[i] <- which(dans)
  cellules$exposition_centrale <- Ec
  cellules$evenements <- D
  list(cellules = cellules, segment = g, ligne = ligne,
       hors_plage = list(cellules = sum(!dans), evenements = sum(as.numeric(exposition$evenements[!dans])),
                         exposition = sum(exposition$exposition_centrale[!dans])))
}
