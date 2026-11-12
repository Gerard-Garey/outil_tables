# -----------------------------------------------------------------------------
#  R/lissage.R : module `lissage`, étape `lissage` (issue #223 ; E2.12,
#  variante (a) : lissage de Whittaker-Henderson, critère quadratique ;
#  ADR 0017, points 1 à 4, 9, 10, 11 et 15 ; corps de #223, décisions D1 à
#  D13 du mainteneur du 11/11/2026, M413 ; spécification de forme (0')(b)
#  d'actuary-approfondi, M415)
#
#  - controler_gouvernants_lissage() : forme et domaine des deux paramètres
#    gouvernants (methode, critere_lissage), contrôle croisé compris ;
#    appelée par valider_plan() avant le contrôle de présence des paramètres
#    conditionnels (ADR 0017, point 3.2) ;
#  - controler_parametres_lissage() : valeurs des paramètres de l'étape,
#    estimateur brut exigé par la méthode et découpage par année ; appelée
#    par valider_plan() seule (#82) ;
#  - controler_convention_lissage() : convention d'âge retenue, contrôlée
#    avant exécution par controler_entrees() (construire_table()) ;
#  - somme_differences_carrees() : S = somme des (Delta^z v)^2, seule
#    fonction de S (ADR 0017, point 10.3 ; lue aussi, plus tard, par l'étape
#    coherence_regularite) ;
#  - lisser_quadratique() : g, degrés de liberté effectifs, F, S, M et
#    résidus de conservation d'un segment, par QR des moindres carrés
#    augmentés (ADR 0017, point 9.1) ;
#  - calculer_lissage() : fonction d'étape `lissage`.
#
#  Notations (Lignes directrices mortalité, chap. 3, IV.2.a, p. 31 à 33) :
#  q^ = D / E^i, estimateur binomial de l'étape brut (D1) ; w poids ; h
#  paramètre de lissage (parametre_lissage, déclaré, D3) ; z ordre des
#  différences (ordre_difference) ; K matrice des différences avant d'ordre
#  z, (n - z) x n ; g minimise M = F + h S, F = somme des w (g - q^)^2,
#  S = somme des (Delta^z g)^2. Aucune extrapolation hors de la plage (D11) ;
#  aucun écrêtage (D4).
#  Ce module n'appelle que `outils` (refuser(), formuler(), forme_exacte(),
#  chaine_unique(), valeurs_entieres_double(), denombrement_entier(),
#  texte_moteur(), avertissements_vides()), `donnees` (citer(),
#  liste_guillemets()) et `exposition` (controler_plage_ages(),
#  CONVENTIONS_VALIDATION, grille_plage()) (ADR 0007, point 2.1 ; ADR 0017,
#  point 10.4).
# -----------------------------------------------------------------------------

# Méthodes de lissage mises en oeuvre (E2.12 ; (a) en #223, wh_poisson ajoutée
# par #225), critères de choix du paramètre de lissage (D3, D5) et critères
# admis par méthode (contrôle croisé des gouvernants, ADR 0017, point 3.4 :
# « laml » refusé sous wh_quadratique).
METHODES_LISSAGE <- "wh_quadratique"
CRITERES_LISSAGE <- c("declare", "laml")
CRITERES_ADMIS_LISSAGE <- list(wh_quadratique = "declare")
# Pondérations du critère quadratique (D2 ; Lignes directrices, p. 31 et 32 :
# type A, w = 1 ; type B, w = E^i / moyenne de E^i).
PONDERATIONS_LISSAGE <- c("uniforme", "exposition")
# Ordres des différences admis (Lignes directrices, p. 31 : « en pratique,
# z = 2, 3 ou 4 » ; z = 1 admis par la spécification de #223).
ORDRES_DIFFERENCE <- c(1, 2, 3, 4)
# Estimateur brut lu par chaque méthode (D1) et échelle de la pénalité S.
ESTIMATEUR_LISSAGE <- c(wh_quadratique = "binomial")
ECHELLE_PENALITE <- c(wh_quadratique = "q")
# États d'une ligne de la table lissée : observee (poids non nul) ;
# interpolee (poids nul : cellule sans exposition, valeur interpolée par le
# lissage ; Lignes directrices, p. 33).
ETATS_TABLE_LISSEE <- c("observee", "interpolee")
# États binomiaux d'une cellule de la plage (ETATS_BRUT$binomial, module
# brut) : `defini` reçoit un poids, `exposition_nulle` (et une cellule sans
# ligne) un poids nul ; `effectif_inferieur_aux_evenements` est refusé.
ETAT_BRUT_DEFINI <- "defini"
ETAT_BRUT_INFERIEUR <- "effectif_inferieur_aux_evenements"

# Textes recopiés dans etapes/lissage/parametres (spécification de forme,
# § 4 ; marqués UTF-8 par texte_moteur() à l'exécution).
REGLE_FORCE_LISSAGE <- "mu = -ln(1 - q) : force constante par âge entier, règle unique du moteur"
HYPOTHESE_LISSAGE <- paste(
  "q^ : estimateur binomial de l'étape brut, q^ = D / E^i, sous l'hypothèse de répartition dans la cellule",
  "_(1-t)q_(x+t) = (1 - t) q_x pour 0 <= t <= 1 (etapes/brut/parametres/hypotheses/binomial) ; une cellule où",
  "D > E^i (q^ > 1) n'est pas définie par l'étape brut et le lissage est refusé ; g : solution du problème",
  "quadratique min F + h S sur la plage d'âges, sans modèle probabiliste (ni vraisemblance, ni intervalle) ;",
  "cellules de poids nul interpolées, aucune extrapolation hors de la plage ; la force se lit sur g par regle_force",
  "(force constante par âge entier), qui diffère de l'hypothèse de répartition du binomial d'entrée : écart entre",
  "les taux centraux de l'ordre de q^3/12.")
REFERENCE_METHODE_LISSAGE <- paste(
  "Lignes directrices mortalité de la Commission d'agrément de l'Institut des actuaires, recommandation du 20 juin",
  "2006, chap. 3, IV.2.a (p. 31 à 33), SHA-256 70ba7463b840bb4fd378dd05c7dad66213239d0bb033ea126b7b0b4669d26835 ;",
  "écarts documentés")
# Statuts (E2.20) des grandeurs de l'étape (spécification de forme, § 6).
STATUTS_LISSAGE <- local({
  descriptif <- "descriptif, arithmétique flottante"
  residu <- paste("nuls en arithmétique exacte quelle que soit la pondération (somme des w g = somme des w q^ ;",
                  "somme des x w g = somme des x w q^ pour z >= 2) ; interprétés comme conservation des décès et de",
                  "l'âge moyen au décès sous la pondération exposition seulement ; valeur calculée : approximation",
                  "numérique")
  l <- list(
    c("table_lissee/qx",
      paste("résultat exact du problème quadratique min F + h S (minimum unique dès qu'au moins z cellules ont un",
            "poids non nul) ; valeur calculée par QR des moindres carrés augmentés, approximation numérique ;",
            "estimateur lissé, sans propriété de maximum de vraisemblance ni d'absence de biais ; aucun intervalle",
            "restitué")),
    c("table_lissee/levier",
      paste("approximation numérique : élément diagonal de la matrice de lissage de la cellule, w_i V_ii",
            "(résolution QR, R base) ; dans [0, 1] en arithmétique exacte, égal à 1 seulement dans un segment",
            "qui compte exactement z cellules de poids non nul ; valeur calculée >= 0, qui peut dépasser 1 de",
            "l'ordre de l'erreur d'arrondi ; 0 exactement sur une cellule de poids nul ; somme sur le segment =",
            "ddl_effectifs")),
    c("regle_force",
      paste("transformation exacte sous la force constante par âge entier ; diffère de l'hypothèse de répartition",
            "_(1-t)q_(x+t) = (1 - t) q_x du binomial d'entrée (écart entre les taux centraux de l'ordre de q^3/12)")),
    c("segments/ddl_effectifs",
      paste("trace de (W + h K'K)^-1 W, définition des degrés de liberté effectifs du cas normal ; approximation",
            "numérique (QR)")),
    c("segments/fidelite", descriptif),
    c("segments/regularite", descriptif),
    c("segments/critere", descriptif),
    c("segments/residu_conservation_0", residu),
    c("segments/residu_conservation_1", residu))
  data.frame(grandeur = vapply(l, `[`, "", 1L), statut = vapply(l, `[`, "", 2L), stringsAsFactors = FALSE)
})
# Constantes numériques de l'étape (ADR 0017, point 9.3 ; D12) : aucune sous
# wh_quadratique (data.frame à zéro ligne, colonnes fixées).
CONSTANTES_LISSAGE <- data.frame(constante = character(0), valeur = numeric(0), origine = character(0),
                                 stringsAsFactors = FALSE)

# Gouvernants de l'étape (ADR 0017, points 3.2 et 3.4), dans cet ordre :
# methode (methode_lissage_invalide), critere_lissage
# (critere_lissage_invalide), puis contrôle croisé (critere_lissage_non_admis,
# D3). Appelée par valider_plan() avant controler_parametres_condition(), et
# par controler_parametres_lissage() (idempotente). Rend TRUE, invisible.
controler_gouvernants_lissage <- function(methode, critere_lissage) {
  if (!chaine_unique(methode) || !(methode %in% METHODES_LISSAGE))
    refuser("lissage", "methode_lissage_invalide",
            formuler(paste("La méthode de lissage (methode) doit être une chaîne unique, sans attribut, prise dans %s,",
                           "sans défaut."),
                     liste_guillemets(METHODES_LISSAGE)),
            list(methode = methode))
  if (!chaine_unique(critere_lissage) || !(critere_lissage %in% CRITERES_LISSAGE))
    refuser("lissage", "critere_lissage_invalide",
            formuler(paste("Le critère de choix du paramètre de lissage (critere_lissage) doit être une chaîne unique,",
                           "sans attribut, prise dans %s, sans défaut."),
                     liste_guillemets(CRITERES_LISSAGE)),
            list(critere_lissage = critere_lissage))
  if (!(critere_lissage %in% CRITERES_ADMIS_LISSAGE[[methode]]))
    refuser("lissage", "critere_lissage_non_admis",
            formuler(paste("Lissage : le critère « laml » n'est pas admis sous la méthode « %s » : le paramètre h du",
                           "critère quadratique (Lignes directrices, chap. 3, IV.2) est déclaré (critere_lissage =",
                           "« declare »)."),
                     citer(methode)),
            list(methode = methode, critere_lissage = critere_lissage))
  invisible(TRUE)
}

# Valeurs des paramètres de l'étape `lissage` (spécification de forme, § 2.3),
# un argument formel par paramètre de PARAMETRES_ETAPES$lissage, dans l'ordre
# du catalogue (absent = missing() : paramètre conditionnel non requis), plus
# deux arguments de contexte : `brut`, paramètres de l'étape brut du plan
# (préalable, contrôlée avant) ; `par_annee`, paramètre de l'étape
# exposition. Appelée par valider_plan() par do.call() (paramètres présents
# passés par leur nom). Gouvernants d'abord (controler_gouvernants_lissage(),
# idempotente : un appel direct est complet), puis, dans l'ordre du
# catalogue : plage_ages (plage_ages_invalide, module lissage),
# ordre_difference (ordre_difference_invalide), ponderation
# (ponderation_invalide), parametre_lissage (parametre_lissage_invalide) ;
# puis inter-étapes : estimateur_lissage_absent (D1), lissage_par_annee.
# Forme exacte (#23) ; un integer est refusé (M257). Rend TRUE, invisible.
controler_parametres_lissage <- function(methode, plage_ages, ordre_difference, critere_lissage, ponderation,
                                         parametre_lissage, brut, par_annee) {
  controler_gouvernants_lissage(methode, critere_lissage)
  controler_plage_ages(plage_ages, "lissage")
  z <- ordre_difference
  if (!valeurs_entieres_double(z) || length(z) != 1L || !(z %in% ORDRES_DIFFERENCE) ||
      z > plage_ages[2L] - plage_ages[1L])
    refuser("lissage", "ordre_difference_invalide",
            formuler(paste("L'ordre des différences (ordre_difference) doit être un nombre double unique, sans",
                           "attribut, à valeur entière dans {1, 2, 3, 4}, au plus égal à max(plage_ages) -",
                           "min(plage_ages) (un integer est refusé).")),
            list(ordre_difference = ordre_difference, plage_ages = plage_ages))
  if (!missing(ponderation) && (!chaine_unique(ponderation) || !(ponderation %in% PONDERATIONS_LISSAGE)))
    refuser("lissage", "ponderation_invalide",
            formuler(paste("La pondération (ponderation) doit être une chaîne unique, sans attribut, prise dans %s",
                           "(uniforme : w = 1 ; exposition : w = E^i rapportée à sa moyenne sur les cellules définies",
                           "du segment), sans défaut."),
                     liste_guillemets(PONDERATIONS_LISSAGE)),
            list(ponderation = ponderation))
  if (!missing(parametre_lissage) &&
      (!forme_exacte(parametre_lissage, "double") || length(parametre_lissage) != 1L ||
       !is.finite(parametre_lissage) || parametre_lissage <= 0))
    refuser("lissage", "parametre_lissage_invalide",
            formuler(paste("Le paramètre de lissage (parametre_lissage : h sous wh_quadratique, lambda sous",
                           "wh_poisson) doit être un nombre double unique, sans attribut, fini, strictement positif,",
                           "sans défaut.")),
            list(parametre_lissage = parametre_lissage))
  estimateur <- ESTIMATEUR_LISSAGE[[methode]]
  estimateurs <- brut[["estimateurs"]]
  if (!(estimateur %in% estimateurs))
    refuser("lissage", "estimateur_lissage_absent",
            formuler(paste("Lissage : la méthode « %s » lit l'estimateur brut « %s », absent de l'étape brut",
                           "(estimateurs : %s) ; l'ajouter à ses estimateurs."),
                     citer(methode), citer(estimateur), liste_guillemets(estimateurs)),
            list(methode = methode, estimateur = estimateur, estimateurs = estimateurs))
  if (isTRUE(par_annee))
    refuser("lissage", "lissage_par_annee",
            formuler("Lissage : découpage par année (par_annee = TRUE) non admis : le lissage porte sur l'âge seul."),
            list(par_annee = par_annee))
  invisible(TRUE)
}

# Convention d'âge retenue (spécification de forme, § 2.5), contrôlée avant
# exécution par controler_entrees() (construire_table()), après le contrôle
# 1.3 des tables, dans l'ordre des étapes : la table lissée est une table par
# âge entier [x, x+1) (CONVENTIONS_VALIDATION, module exposition). Refus
# convention_lissage_non_admise. Rend TRUE, invisible.
controler_convention_lissage <- function(convention) {
  if (!(convention %in% CONVENTIONS_VALIDATION))
    refuser("lissage", "convention_lissage_non_admise",
            formuler(paste("Lissage : convention d'âge retenue « %s » non admise (admise : %s) : la table lissée est",
                           "une table par âge entier [x, x+1), lue sous cette convention par la validation et la",
                           "fermeture."),
                     citer(convention), liste_guillemets(CONVENTIONS_VALIDATION)),
            list(convention = convention, attendu = CONVENTIONS_VALIDATION))
  invisible(TRUE)
}

# S = somme des (Delta^z v)^2, différences avant d'ordre z (base::diff ;
# Lignes directrices, chap. 3, IV.2.a, p. 31, et chap. 4, III.1), seule
# fonction de S (ADR 0017, point 10.3) : l'appelant choisit l'échelle de v
# (q sous wh_quadratique). v : double ; z : ordre, entier positif. Rend un
# double.
somme_differences_carrees <- function(v, z) sum(diff(v, differences = z)^2)

# Critère quadratique d'un segment (ADR 0017, point 9.1 ; spécification de
# forme, § 3) : q (q^, 0 aux cellules de poids nul), w (poids >= 0), h > 0,
# z ordre, x âges, vecteurs de même longueur n > z, au moins z poids non nuls
# (contrôlé par l'appelant). g = (W + h K'K)^-1 W q^, par QR des moindres
# carrés augmentés [W^(1/2) ; sqrt(h) K] (qr(LAPACK = FALSE), routine dqrdc2
# de R ; tolérance de rang par défaut de qr(), 1e-7), puis qr.coef() ;
# aucune inversion explicite, aucune équation normale. Rang numérique < n
# (h très grand : amendement d'actuary du 11/11/2026 sur #223, point 4) :
# rend list(rang) seul, sans calcul, l'appelant refuse
# (lissage_systeme_rang_deficient). Sinon V = (W + h K'K)^-1 = chol2inv(R),
# remise dans l'ordre de la permutation des colonnes ; leviers w_i V_ii
# (0 exactement sur un poids nul), ddl effectifs = leur somme, calculée sur
# ce vecteur (identité au bit près avec la somme de la colonne levier ; ADR
# 0017, point 5.5 (ii)). Rend list(rang (n), g, levier, ddl_effectifs,
# fidelite (F), regularite (S), critere (M = F + h S), residu_conservation_0 (somme des
# w (g - q^)), residu_conservation_1 (somme des x w (g - q^))).
lisser_quadratique <- function(q, w, h, z, x) {
  n <- length(q)
  racine_w <- sqrt(w)
  M <- rbind(diag(racine_w, n), sqrt(h) * diff(diag(n), differences = z))
  Q <- qr(M, LAPACK = FALSE)
  if (Q$rank < n) return(list(rang = Q$rank))
  g <- qr.coef(Q, c(racine_w * q, numeric(n - z)))
  V <- matrix(0, n, n)
  V[Q$pivot, Q$pivot] <- chol2inv(qr.R(Q))
  r <- g - q
  fidelite <- sum(w * r^2)
  regularite <- somme_differences_carrees(g, z)
  levier <- w * diag(V)
  list(rang = Q$rank, g = g, levier = levier, ddl_effectifs = sum(levier), fidelite = fidelite,
       regularite = regularite,
       critere = fidelite + h * regularite, residu_conservation_0 = sum(w * r),
       residu_conservation_1 = sum(x * w * r))
}

# Fonction d'étape `lissage` (#223), méthode wh_quadratique.
#   exposition   : table de l'étape `exposition` (cellules) ;
#   brut         : resultat de l'étape `brut` (table binomial, aux lignes et
#                  dans l'ordre de la table d'exposition) ;
#   segmentation : champ du plan ; convention : convention retenue ;
#   methode, plage_ages, ordre_difference, critere_lissage, ponderation,
#   parametre_lissage : paramètres de l'étape (NULL pour un paramètre
#                  conditionnel absent).
# Aucun contrôle de paramètre (#82) : valider_plan() et controler_entrees()
# les ont faits. Grille : grille_plage() (module exposition), sans année.
# Poids : cellule binomiale `defini` : w = 1 (uniforme) ou E^i / moyenne de
# E^i sur les cellules `defini` de la plage du segment (exposition) ; cellule
# `exposition_nulle` ou sans ligne : w = 0, q^ remplacé par 0 dans le
# système, état interpolee. Refus en cours d'étape, chacun évalué sur tous
# les segments, qui sont tous cités, dans l'ordre : lissage_sans_segment,
# lissage_effectif_inferieur_aux_evenements, lissage_plage_bornee_sans_exposition
# (D11), lissage_cellules_insuffisantes, lissage_sans_deces,
# lissage_systeme_rang_deficient, lissage_taux_hors_domaine (D4, sans
# écrêtage). Rend list(parametres,
# resultat (table_lissee, segments), diagnostics, avertissements (vide)).
calculer_lissage <- function(exposition, brut, segmentation, convention, methode, plage_ages, ordre_difference,
                             critere_lissage, ponderation, parametre_lissage) {
  if (!identical(methode, "wh_quadratique"))
    stop("calculer_lissage() : méthode non mise en oeuvre (erreur de programmation).", call. = FALSE)
  tb <- brut[["binomial"]]
  if (!is.data.frame(tb) || nrow(tb) != nrow(exposition) ||
      !all(c("evenements", "exposition_initiale", "taux", "etat") %in% names(tb)))
    stop("calculer_lissage() : table binomial de l'étape brut absente ou non alignée (erreur de programmation).",
         call. = FALSE)
  z <- ordre_difference
  h <- parametre_lissage
  gr <- grille_plage(exposition, segmentation, FALSE, plage_ages)
  cel <- gr$cellules
  g_seg <- gr$segment
  n_seg <- if (length(g_seg)) max(g_seg) else 0L
  segs <- cel[!duplicated(g_seg), segmentation, drop = FALSE]
  rownames(segs) <- NULL
  p1 <- as.integer(plage_ages[1L]); p2 <- as.integer(plage_ages[2L])
  # Clés des segments `idx` cités dans un refus, une chaîne par segment ;
  # chaque refus y accole son complément et joint par « ; » (texte formé :
  # aide de message, règles 2 et 4 de tests/controles/messages_refus.R).
  segments_cites <- function(idx)
    vapply(idx, function(i)
      if (length(segmentation)) paste(vapply(segmentation, function(v)
        formuler("%s = « %s »", citer(v), citer(as.character(segs[[v]][i]))), ""), collapse = ", ")
      else formuler("sans segmentation"), "")
  if (!n_seg)
    refuser("lissage", "lissage_sans_segment",
            formuler(paste("Lissage : la table d'exposition ne compte aucune cellule (aucune exposition sur la période",
                           "d'observation) : aucun segment à lisser sur la plage d'âges %d à %d."), p1, p2),
            list(plage_ages = plage_ages))
  ligne <- gr$ligne
  avec_ligne <- !is.na(ligne)
  etat_b <- rep(NA_character_, nrow(cel))
  etat_b[avec_ligne] <- tb$etat[ligne[avec_ligne]]
  Ei <- numeric(nrow(cel))
  Ei[avec_ligne] <- tb$exposition_initiale[ligne[avec_ligne]]
  D <- cel$evenements
  defini <- avec_ligne & etat_b == ETAT_BRUT_DEFINI
  defini[is.na(defini)] <- FALSE

  # Cellules de la plage hors du modèle binomial (D > E^i, D > 0 avec E^i
  # nulle comprise) : refus, sous (a) seulement.
  inferieur <- which(avec_ligne & etat_b %in% ETAT_BRUT_INFERIEUR)
  if (length(inferieur)) {
    seg_inf <- unique(g_seg[inferieur])
    refuser("lissage", "lissage_effectif_inferieur_aux_evenements",
            formuler(paste("Lissage : %d cellule(s) de la plage d'âges %d à %d dont l'exposition initiale est inférieure",
                           "au nombre de décès (état effectif_inferieur_aux_evenements de l'estimateur binomial : q^ =",
                           "D / E^i n'est pas défini) : %s. Le lissage quadratique est refusé."),
                     length(inferieur), p1, p2,
                     paste(paste0(segments_cites(seg_inf), vapply(seg_inf, function(s) {
                       i <- inferieur[g_seg[inferieur] == s]
                       formuler(" (%s)", paste(vapply(i, function(j)
                         formuler("âge %d : D = %d, E^i = %.17g", cel$age[j], D[j], Ei[j]), ""), collapse = ", "))
                     }, "")), collapse = " ; ")),
            list(plage_ages = plage_ages, segments = segs[seg_inf, , drop = FALSE], age = cel$age[inferieur],
                 evenements = D[inferieur], exposition_initiale = Ei[inferieur]))
  }

  # Poids et q^ par cellule.
  w <- numeric(nrow(cel))
  q <- numeric(nrow(cel))
  q[defini] <- tb$taux[ligne[defini]]
  par_segment <- split(seq_len(nrow(cel)), factor(g_seg, levels = seq_len(n_seg)))
  for (s in seq_len(n_seg)) {
    i <- par_segment[[s]]
    d <- i[defini[i]]
    w[d] <- switch(ponderation,
      uniforme = 1,
      exposition = Ei[d] / mean(Ei[d]),
      stop("calculer_lissage() : pondération inconnue (erreur de programmation).", call. = FALSE))
  }

  # Plage bornée par une cellule sans exposition (D11).
  bornee <- which(vapply(par_segment, function(i) !(w[i[1L]] > 0) || !(w[i[length(i)]] > 0), NA, USE.NAMES = FALSE))
  if (length(bornee))
    refuser("lissage", "lissage_plage_bornee_sans_exposition",
            formuler(paste("Lissage : %d segment(s) dont la première ou la dernière cellule de la plage d'âges %d à %d",
                           "n'a aucune exposition (poids nul) : %s. Le lissage n'extrapole pas (décision D11) :",
                           "réduire la plage d'âges aux âges observés ; l'extrapolation relève de la fermeture."),
                     length(bornee), p1, p2,
                     paste(paste0(segments_cites(bornee), vapply(bornee, function(s) {
                       i <- par_segment[[s]]
                       a <- unique(c(i[1L], i[length(i)]))
                       formuler(" (âge(s) %s)", paste(vapply(a[!(w[a] > 0)], function(j) formuler("%d", cel$age[j]), ""),
                                                      collapse = ", "))
                     }, "")), collapse = " ; ")),
            list(plage_ages = plage_ages, segments = segs[bornee, , drop = FALSE]))
  # Moins de z cellules de poids non nul (Lignes directrices, p. 33).
  ponderees <- vapply(par_segment, function(i) sum(w[i] > 0), 0L, USE.NAMES = FALSE)
  insuffisants <- which(ponderees < z)
  if (length(insuffisants))
    refuser("lissage", "lissage_cellules_insuffisantes",
            formuler(paste("Lissage : %d segment(s) comptent moins de z = %d cellule(s) de poids non nul sur la plage",
                           "d'âges %d à %d : %s. Le minimum de M = F + h S n'est pas unique (Lignes directrices,",
                           "chap. 3, IV.2.a, p. 33 : au moins z points pivots)."),
                     length(insuffisants), as.integer(z), p1, p2,
                     paste(paste0(segments_cites(insuffisants), vapply(insuffisants, function(s)
                       formuler(" (%d cellule(s) de poids non nul)", ponderees[s]), "")), collapse = " ; ")),
            list(plage_ages = plage_ages, ordre_difference = z, segments = segs[insuffisants, , drop = FALSE],
                 cellules_ponderees = ponderees[insuffisants]))
  # Aucun décès sur la plage (précédent positionnement_sans_deces).
  deces <- vapply(par_segment, function(i) sum(as.numeric(D[i])), 0, USE.NAMES = FALSE)
  sans_deces <- which(deces == 0)
  if (length(sans_deces))
    refuser("lissage", "lissage_sans_deces",
            formuler(paste("Lissage : %d segment(s) sans décès observé sur la plage d'âges %d à %d : %s. La table",
                           "lissée serait à mortalité nulle : le lissage est refusé."),
                     length(sans_deces), p1, p2, paste(segments_cites(sans_deces), collapse = " ; ")),
            list(plage_ages = plage_ages, segments = segs[sans_deces, , drop = FALSE]))

  # Lissage, segment par segment.
  x <- cel$age
  g <- numeric(nrow(cel))
  res <- vector("list", n_seg)
  for (s in seq_len(n_seg)) {
    i <- par_segment[[s]]
    res[[s]] <- lisser_quadratique(q[i], w[i], h, z, x[i])
  }
  # Rang numérique perdu (h très grand) : tous les segments évalués, puis
  # refus, avant le contrôle de domaine (amendement d'actuary, point 4).
  deficients <- which(vapply(seq_len(n_seg), function(s) res[[s]]$rang < length(par_segment[[s]]), NA))
  if (length(deficients))
    refuser("lissage", "lissage_systeme_rang_deficient",
            formuler(paste("Lissage : système des moindres carrés augmentés numériquement de rang déficient pour %d",
                           "segment(s) sous h = %.17g, z = %d (tolérance de rang par défaut de qr(), 1e-7) : %s. À",
                           "cette valeur, la table lissée se confond numériquement avec le polynôme de degré z - 1",
                           "ajusté par moindres carrés pondérés (Lignes directrices, chap. 3, IV.2.a, p. 31) ; réduire",
                           "parametre_lissage."),
                     length(deficients), h, as.integer(z), paste(segments_cites(deficients), collapse = " ; ")),
            list(plage_ages = plage_ages, parametre_lissage = h, ordre_difference = z,
                 segments = segs[deficients, , drop = FALSE]))
  for (s in seq_len(n_seg)) g[par_segment[[s]]] <- res[[s]]$g
  # Domaine (D4) : g dans ]0, 1[ sur la plage, cellules interpolées
  # comprises ; aucun écrêtage.
  hors <- which(!(g > 0 & g < 1))
  if (length(hors)) {
    seg_hors <- unique(g_seg[hors])
    refuser("lissage", "lissage_taux_hors_domaine",
            formuler(paste("Lissage : %d taux lissé(s) hors de ]0, 1[ sur la plage d'âges %d à %d : %s. Aucun",
                           "écrêtage (décision D4) : modifier le paramètre de lissage, l'ordre des différences, la",
                           "pondération ou la plage d'âges."),
                     length(hors), p1, p2,
                     paste(paste0(segments_cites(seg_hors), vapply(seg_hors, function(s) {
                       i <- hors[g_seg[hors] == s]
                       formuler(" (%s)", paste(vapply(i, function(j) formuler("âge %d : g = %.17g", x[j], g[j]), ""),
                                               collapse = ", "))
                     }, "")), collapse = " ; ")),
            list(plage_ages = plage_ages, segments = segs[seg_hors, , drop = FALSE], age = x[hors], qx = g[hors]))
  }

  etat <- ETATS_TABLE_LISSEE[ifelse(w > 0, 1L, 2L)]
  table_lissee <- cel[segmentation]
  rownames(table_lissee) <- NULL
  table_lissee$age <- as.integer(x)
  table_lissee$qx <- g
  table_lissee$etat <- etat
  table_lissee$levier <- numeric(nrow(cel))
  for (s in seq_len(n_seg)) table_lissee$levier[par_segment[[s]]] <- res[[s]]$levier
  table_lissee$poids <- w

  lire <- function(nom) vapply(res, function(r) r[[nom]], 0)
  segments <- segs
  segments$cellules <- lengths(par_segment, use.names = FALSE)
  segments$cellules_ponderees <- ponderees
  segments$cellules_interpolees <- segments$cellules - ponderees
  segments$evenements <- denombrement_entier(deces, "lissage", "segments/evenements")
  segments$parametre_lissage <- rep(h, n_seg)
  segments$ddl_effectifs <- lire("ddl_effectifs")
  segments$fidelite <- lire("fidelite")
  segments$regularite <- lire("regularite")
  segments$critere <- lire("critere")
  segments$residu_conservation_0 <- lire("residu_conservation_0")
  segments$residu_conservation_1 <- lire("residu_conservation_1")
  segments$etat <- rep("produite", n_seg)

  # Décomptes : identité plage + hors plage = total vérifiée en double avant
  # le rangement en integer (#199 ; ADR 0009, point 9.6, (iii)).
  ev_plage <- sum(deces)
  total <- sum(as.numeric(exposition$evenements))
  if (!identical(ev_plage + gr$hors_plage$evenements, total))
    stop("calculer_lissage() : décomptes d'événements incohérents (erreur de programmation).", call. = FALSE)
  dans <- exposition$age >= plage_ages[1L] & exposition$age <= plage_ages[2L]
  parametres <- Filter(Negate(is.null),
                       list(methode = methode, plage_ages = plage_ages, ordre_difference = ordre_difference,
                            critere_lissage = critere_lissage, ponderation = ponderation,
                            parametre_lissage = parametre_lissage))
  parametres <- c(parametres,
                  list(convention = convention, estimateur_brut = ESTIMATEUR_LISSAGE[[methode]],
                       echelle_penalite = ECHELLE_PENALITE[[methode]],
                       regle_force = texte_moteur(REGLE_FORCE_LISSAGE), hypothese = texte_moteur(HYPOTHESE_LISSAGE),
                       reference_methode = texte_moteur(REFERENCE_METHODE_LISSAGE),
                       constantes = CONSTANTES_LISSAGE, statuts = texte_moteur(STATUTS_LISSAGE)))
  list(parametres = parametres,
       resultat = list(table_lissee = table_lissee, segments = segments),
       diagnostics = list(cellules = nrow(table_lissee), cellules_interpolees = sum(!(w > 0)),
                          evenements_plage = denombrement_entier(ev_plage, "lissage", "evenements_plage"),
                          cellules_hors_plage = gr$hors_plage$cellules,
                          evenements_hors_plage = denombrement_entier(gr$hors_plage$evenements, "lissage",
                                                                      "evenements_hors_plage"),
                          exposition_initiale_hors_plage = sum(tb$exposition_initiale[!dans]),
                          evenements_total = denombrement_entier(total, "lissage", "evenements_total")),
       avertissements = avertissements_vides())
}
