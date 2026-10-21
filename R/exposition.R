# -----------------------------------------------------------------------------
#  R/exposition.R : module `exposition` (issue #11 ; E2.3, E2.5, E2.6 ;
#  ADR 0003, points 1, 3, 6 et 8 ; ADR 0007, points 5.2, 5.3 et 10 ;
#  spécification d'actuary, A.0 et A.3)
#
#  - deduire_recette() : recette d'exposition déduite des rôles (ADR 0007,
#    point 5.3) ; aucune recette applicable : refus, jamais de devinette ;
#  - controler_parametres_exposition(), resoudre_convention() : paramètres
#    de l'étape (convention d'âge, découpage par année) ; forçage vers une
#    convention plus grossière admis et inscrit, vers une convention plus
#    fine ou non mise en oeuvre refusé ;
#  - fenetres_observations(), segments_observations(),
#    position_dans_cellule() : fenêtres et événements (T5), segments des
#    observations, position d'un instant dans sa cellule ; employées par
#    calculer_exposition() et par l'estimateur de Kaplan-Meier du module
#    `brut` (#15, spécification d'actuary, A.9 : factorisation à résultats
#    identiques) ;
#  - calculer_exposition() : fonction d'étape `exposition` — fenêtre de
#    chaque observation (troncature au début de la période, censure à
#    B = fin - délai + 1), exposition centrale par cellule (segment, âge
#    [, année civile]) en jours et en unité d'exposition, événements de la
#    transition étudiée, compléments de l'exposition initiale, sorties
#    concurrentes (censures à droite à leur instant).
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
#  as.Date() et age_revolu() dans les tests.
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
# Convention native (la plus fine que les rôles permettent) de chaque recette.
CONVENTION_NATIVE <- c(lignes_dates_completes = "dernier_anniversaire")
# Unité de l'exposition centrale par convention (T3), inscrite dans la fiche.
UNITES_EXPOSITION <- c(
  dernier_anniversaire = "annee_d_age : jours / L_x, L_x = anniv(x+1) - anniv(x) (365 ou 366 jours)",
  age_atteint_annee = "annee_civile : jours / nombre de jours de l'année civile (365 ou 366)")
# Hypothèse sur les sorties concurrentes, inscrite dans la fiche (A.3).
HYPOTHESE_CENSURE <- paste("censure non informative : les sorties concurrentes sont des censures",
                           "à droite à leur instant (risques indépendants)")

# Recette déduite des rôles (ADR 0007, point 5.3) : la recette dont tous les
# rôles requis sont déclarés. Seule recette de la branche :
# `lignes_dates_completes`, rôles requis du catalogue (CATALOGUE_ROLES,
# usage `requis`). Rend list(recette, convention_native, roles_requis).
deduire_recette <- function(roles) {
  requis <- CATALOGUE_ROLES$role[CATALOGUE_ROLES$usage == "requis"]
  manquants <- setdiff(requis, names(roles))
  if (length(manquants))
    refuser("exposition", "aucune_recette", message_aucune_recette(requis, manquants),
            list(recette = RECETTE_BRANCHE, role = manquants))
  list(recette = RECETTE_BRANCHE, convention_native = CONVENTION_NATIVE[[RECETTE_BRANCHE]],
       roles_requis = requis)
}

# Contrôle les paramètres propres à l'étape `exposition` (valeurs ; les noms
# sont contrôlés par valider_plan()). Forme exacte (#23, forme_exacte()) :
# type de base exact, aucun attribut (ni names, ni dim, ni classe), sinon
# refus sous le motif du paramètre. Rend TRUE, invisible.
controler_parametres_exposition <- function(convention, par_annee) {
  if (!forme_exacte(convention, "character") || length(convention) != 1L || is.na(convention))
    refuser("exposition", "convention_invalide",
            sprintf("La convention d'âge doit être une chaîne unique, sans attribut, prise dans %s.",
                    liste_guillemets(VALEURS_CONVENTION)),
            list(convention = convention))
  if (convention %in% setdiff(ORDRE_CONVENTIONS, CONVENTIONS_MISES_EN_OEUVRE))
    refuser("exposition", "convention_non_mise_en_oeuvre",
            sprintf(paste("Convention d'âge « %s » non mise en oeuvre (rôles d'âge fourni non pris",
                          "en charge) : la construction est refusée (valeurs admises : %s)."),
                    convention, liste_guillemets(VALEURS_CONVENTION)),
            list(convention = convention))
  if (!(convention %in% VALEURS_CONVENTION))
    refuser("exposition", "convention_invalide",
            sprintf("Convention d'âge inconnue « %s » (valeurs admises : %s).", convention,
                    liste_guillemets(VALEURS_CONVENTION)),
            list(convention = convention))
  if (!forme_exacte(par_annee, "logical") || length(par_annee) != 1L || is.na(par_annee))
    refuser("exposition", "par_annee_invalide",
            "Le découpage par année (par_annee) doit valoir TRUE ou FALSE, sans attribut.",
            list(par_annee = par_annee))
  invisible(TRUE)
}

# Convention retenue à partir de la convention demandée et de la convention
# native de la recette : `plus_fine` retient la native ; une convention plus
# grossière est un forçage, admis ; une convention plus fine que la native
# est refusée (les rôles ne la permettent pas). Rend list(demandee, native,
# retenue, forcee).
resoudre_convention <- function(convention, native) {
  retenue <- if (identical(convention, "plus_fine")) native else convention
  if (match(retenue, ORDRE_CONVENTIONS) < match(native, ORDRE_CONVENTIONS))
    refuser("exposition", "convention_plus_fine",
            sprintf(paste("Convention d'âge « %s » plus fine que celle que la recette permet",
                          "(« %s ») : le forçage vers une convention plus fine est refusé."),
                    retenue, native),
            list(convention = retenue, convention_native = native))
  list(demandee = convention, native = native, retenue = retenue,
       forcee = !identical(retenue, native))
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
            sprintf(libelle, format(n, scientific = FALSE, trim = TRUE), .Machine$integer.max),
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
  if (length(segmentation)) {
    cles <- lapply(observations[segmentation], function(v) if (is.integer(v)) v else as.character(v))
    codes <- lapply(cles, function(v) match(v, sort(unique(v), method = "radix")))
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
#   roles             : vecteur nommé rôle -> colonne (déduction de la recette) ;
#   transition        : list(depart, arrivee) ;
#   periode           : list(debut, fin), dates ; fin incluse ;
#   delai_declaration : entier de jours >= 0 ;
#   segmentation      : sous-ensemble de c("sexe", "segment"), vide admis ;
#   convention        : plus_fine, dernier_anniversaire ou age_atteint_annee ;
#   par_annee         : TRUE pour découper aussi par année civile.
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
calculer_exposition <- function(observations, roles, transition, periode, delai_declaration,
                                segmentation, convention, par_annee) {
  p <- controler_parametres_donnees(transition, periode, delai_declaration, segmentation,
                                    character(0))
  controler_parametres_exposition(convention, par_annee)
  rec <- deduire_recette(roles)
  conv <- resoudre_convention(convention, rec$convention_native)
  cv <- conv$retenue
  manquantes <- setdiff(c("date_naissance", "debut", "fin", "cause", segmentation), names(observations))
  if (length(manquantes))
    stop(sprintf("calculer_exposition() : colonne(s) %s absente(s) des observations (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  cible <- p$transition$arrivee
  debut_p <- p$debut; borne <- p$borne
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
                             cv, conv$native, rec$recette), 1)
  if (sans_exposition > 0L)
    avert <- avertir(avert, "exposition", "evenement_sans_exposition",
                     sprintf("Relevé : %d cellule(s) à exposition nulle qui comptent au moins un événement.",
                             sans_exposition), sans_exposition)
  if (!n_cel)
    avert <- avertir(avert, "exposition", "aucune_exposition",
                     "Aucune exposition ni aucun événement dans la période d'observation.", 0)

  list(
    parametres = list(recette = rec$recette, convention = conv,
                      # Texte du moteur marqué UTF-8 en toute locale (#50).
                      unite_exposition = texte_moteur(UNITES_EXPOSITION[[cv]]), par_annee = par_annee,
                      transition = p$transition$code, segmentation = segmentation,
                      periode = periode, delai_declaration = delai_declaration,
                      fin_effective = structure(borne - 1, class = "Date"),
                      hypothese_censure = texte_moteur(HYPOTHESE_CENSURE)),
    resultat = list(exposition = table_exp),
    diagnostics = list(observations = n_obs, observations_exposees = length(exposees),
                       exposition_jours = sum(m$jours), evenements = length(ev),
                       sorties_concurrentes = length(sc), cellules = n_cel,
                       cellules_evenement_sans_exposition = sans_exposition),
    avertissements = avert,
    fenetres = structure(c(f[c("composantes", "calendrier", "a", "b", "evenement")], sg,
                           list(debut = debut_p, borne = borne, transition = p$transition$code, convention = cv,
                                segmentation = segmentation)),
                         class = "fenetres_exposition")
  )
}
