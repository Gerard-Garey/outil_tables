# -----------------------------------------------------------------------------
#  R/brut.R : module `brut`, taux bruts par cellule (issues #14 et #15 ;
#  E2.12, E2.14, E2.20 ; ADR 0007, points 2.1, 5.2, 6.1 et 6.5 ;
#  spécifications d'actuary du 17/10/2026, A.1 à A.5 de #14 et A.1 à A.12
#  de #15 ; décisions du mainteneur du 17/10/2026 sur les questions A.8 de
#  #14 et A.14 de #15)
#
#  - controler_parametres_brut() : paramètres de l'étape (estimateurs,
#    niveau) et refus du binomial et de Kaplan-Meier sous découpage par
#    année ; appelée par valider_plan() et par la fonction d'étape ;
#  - taux_poisson(), taux_binomial() : estimateur, intervalles exacts et
#    asymptotiques et état de chaque cellule ;
#  - taux_kaplan_meier() : estimateur de Kaplan-Meier sur l'âge (#15),
#    tables par cellule et par instant d'événement ;
#  - calculer_taux_bruts() : fonction d'étape `brut`.
#
#  Notations (A.1) : D = evenements ; E^c = exposition_centrale ; E^i = E^c +
#  complement_initial_cellule (exposition initiale « cellule », sans
#  paramètre : décision du mainteneur, A.8 question 1 ; la colonne
#  complement_initial_fenetre reste un relevé de l'étape `exposition`) ;
#  alpha = 1 - niveau ; intervalles à queues égales alpha / 2. Les quantiles
#  supérieurs sont calculés par lower.tail = FALSE (A.3 : à niveau =
#  1 - 2^-53, 1 - alpha / 2 vaut 1 en double et qgamma(1 - alpha / 2, .)
#  vaut Inf).
#  Une cellule où l'estimateur n'est pas défini reçoit NA, un état et, si
#  elle compte un événement, un avertissement ; jamais de refus (A.8,
#  question 3).
#  Kaplan-Meier (#15) : âge en années d'âge de la convention retenue (A.1),
#  tau = x + j / L, comparé exactement par la clé entière K = x 133 590 +
#  j (133 590 / L) ; ensemble à risque fermé r(t) = #{i : tau(a_i) <= t <=
#  tau(b_i)} : à un même instant, entrées, puis événements, puis censures
#  (A.2, décision du mainteneur, A.14 question 1) ; q_x = 1 - prod (1 -
#  d / r) sur les instants de [x, x+1) (A.3) ; cellule définie si [x, t0)
#  est couvert, t0 premier instant où d = r (A.6) ; aucun intervalle exact,
#  variance et bornes NA à q = 0 et q = 1 (A.7) ; pas de survie cumulée par
#  segment (A.14 question 2).
#  Ce module n'appelle que `donnees` (liste_guillemets(),
#  controler_parametres_donnees()), `exposition` (fenetres_observations(),
#  segments_observations(), extraire_composantes(), position_dans_cellule(),
#  somme_par_cellule()) et `outils` (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# Estimateurs disponibles, dans l'ordre canonique des résultats (A.4).
ESTIMATEURS_BRUT <- c("poisson", "binomial", "kaplan_meier")
# Définition de l'exposition initiale du binomial, fixée sans paramètre (A.2).
EXPOSITION_INITIALE_BRUT <- "cellule"
# Tolérance absolue sur |E^i - D| (A.3), approximation numérique : en deçà,
# le calcul prend E^i := D (la colonne exposition_initiale garde la somme
# calculée). Plus petit écart rationnel non nul entre E^i et un entier, avec
# L dans {365, 366} : 1/133 590, environ 7,5e-6.
TOLERANCE_EFFECTIF <- 1e-9
# États d'une cellule par estimateur (A.5) ; taux et bornes NA hors `defini`.
ETATS_BRUT <- list(
  poisson = c("defini", "exposition_nulle", "evenement_sans_exposition"),
  binomial = c("defini", "exposition_nulle", "effectif_inferieur_aux_evenements"),
  kaplan_meier = c("defini", "exposition_nulle", "ensemble_a_risque_vide"))
# Kaplan-Meier (#15, A.1 et A.9) : dénominateur commun des instants, 365 x
# 366 (deux instants distincts diffèrent d'au moins 1 / 133 590 an ; clés
# entières exactes en double) ; convention d'ensemble à risque.
DENOMINATEUR_INSTANTS <- 133590L
ENSEMBLE_A_RISQUE_KM <- "ferme"

# Hypothèses inscrites dans les paramètres de l'étape et dans la fiche (A.1),
# nommées par leur formule (aucune appellation d'auteur avant #6).
HYPOTHESES_BRUT <- list(
  poisson = paste("force de la transition constante dans la cellule ; sorties concurrentes non",
                  "informatives (hypothese_censure) ; troncature à gauche traitée par la fenêtre",
                  "d'observation. Log-vraisemblance D log(mu) - mu E^c, maximum mu = D / E^c :",
                  "estimateur du maximum de vraisemblance, résultat exact (valeur en arithmétique",
                  "flottante)."),
  binomial = paste("exposition initiale « cellule » : E^i = E^c + somme, sur les événements, du",
                   "complément jusqu'à la fin de la cellule ; hypothèse de répartition dans la",
                   "cellule _(1-t)q_(x+t) = (1 - t) q_x pour 0 <= t <= 1 (1/S linéaire dans la",
                   "cellule). Estimateur des moments q = D / E^i : E[D] = q E[E^i] exactement sous",
                   "cette hypothèse, pour toute troncature à gauche et toute censure indépendante",
                   "dans la cellule ; convergent ; non sans biais à effectif fini."),
  kaplan_meier = paste("ensemble à risque fermé {tau(a) <= t <= tau(b)} : à un même instant, entrées,",
                       "puis événements, puis censures. Risques discrets h_j aux instants d'événement ;",
                       "vraisemblance prod_j h_j^d_j (1 - h_j)^(r_j - d_j), maximum h_j = d_j / r_j :",
                       "q_x = 1 - prod_j (1 - d_j / r_j) sur les instants de [x, x+1), estimateur du",
                       "maximum de vraisemblance, résultat exact (valeur en arithmétique flottante) ;",
                       "aucune hypothèse de répartition dans la cellule ; non sans biais à effectif fini.",
                       "Hypothèses : troncature à gauche non informative (aucun effet de sélection à l'entrée) ;",
                       "censure non informative (hypothese_censure : sorties concurrentes, délai de",
                       "déclaration et fin de période censurés à leur instant) ; unités d'observation",
                       "indépendantes. Âge en années d'âge de la convention retenue, x + j / L. q_x n'est",
                       "pas identifié là où personne n'est à risque : cellule à couverture incomplète",
                       "non définie (NA, état ensemble_a_risque_vide)."),
  probabilite = paste("probabilite = 1 - exp(-taux), bornes transformées par la même fonction",
                      "croissante (couverture conservée exactement) : force constante sur la",
                      "cellule entière, égale au taux de la cellule."))
# Portée de la probabilité selon le découpage (A.1), ajoutée à son hypothèse.
PORTEE_PROBABILITE <- c(
  dernier_anniversaire = "Unité l'année d'âge : q_x exact sous force constante par âge entier.",
  age_atteint_annee = "Unité l'année civile : probabilité sur l'année civile de la cellule.",
  par_annee = "Cellule âge x année : q_x de l'année t à force mu_(x,t).")

# Statut de chaque grandeur restituée (A.1, A.3 ; E2.14, E2.20), une ligne par
# (estimateur, grandeur, intervalle) ; « E^c fixe » et « E^i fixe » réservés
# aux intervalles exacts (avis d'actuary sur la reprise de #14).
STATUTS_BRUT <- data.frame(
  estimateur = c(rep("poisson", 6L), rep("binomial", 3L), rep("kaplan_meier", 4L)),
  grandeur = c(rep("taux", 3L), rep("probabilite", 3L), rep("taux", 3L), "taux", "variance", "taux", "taux"),
  intervalle = c(rep(c("sans objet (estimation ponctuelle)", "exact", "asymptotique"), 3L),
                 "sans objet (estimation ponctuelle)", "sans objet (estimation ponctuelle)", "asymptotique",
                 "exact"),
  statut = c(
    "résultat exact : maximum de vraisemblance du modèle à force constante dans la cellule",
    paste("exact sous la loi supposée : couverture >= niveau pour tout mu, E^c tenue pour fixe ;",
          "sous le modèle de survie (E^c aléatoire), couverture seulement asymptotique"),
    "asymptotique (mu E^c -> infini) ; non défini à D = 0",
    paste("résultat exact : image du maximum de vraisemblance par q = 1 - exp(-mu) (invariance du",
          "maximum de vraisemblance), force constante sur la cellule entière"),
    paste("exact sous la loi supposée (transformation croissante des bornes exactes du taux :",
          "couverture identique)"),
    paste("asymptotique (transformation croissante des bornes asymptotiques du taux : même",
          "régime) ; non défini à D = 0"),
    paste("estimateur des moments : E[D] = q E[E^i] exactement sous l'hypothèse de répartition ;",
          "convergent ; non sans biais à effectif fini"),
    paste("exact sous la loi supposée si E^i est entier ; si E^i n'est pas entier, prolongement",
          "continu de la formule exacte : couverture non établie"),
    "asymptotique (E^i q(1-q) -> infini) ; non défini à D = 0 et à D = E^i",
    paste("résultat exact : maximum de vraisemblance non paramétrique (risques discrets aux instants",
          "d'événement) ; non sans biais à effectif fini"),
    paste("asymptotique (méthode delta sur log P, ensemble à risque -> infini sur toute la cellule) ;",
          "non définie à q = 0 et q = 1"),
    paste("asymptotique (ensemble à risque -> infini sur toute la cellule), sous indépendance des",
          "unités d'observation ; non défini à q = 0 et q = 1 ; sous l'unité contrat, les contrats",
          "d'une même tête sont supposés indépendants (une dépendance positive rend l'intervalle trop",
          "étroit ; relevé tetes_multi_contrats de l'étape donnees)"),
    paste("aucun : non restitué (sous troncature et censure, D ne suit aucune loi binomiale)")),
  loi_supposee = c(
    rep(c("force constante dans la cellule (vraisemblance de survie, proportionnelle à celle de Poisson)",
          "D ~ Poisson(mu E^c), E^c fixe", "D ~ Poisson(mu E^c)"), 2L),
    "répartition _(1-t)q_(x+t) = (1 - t) q_x dans la cellule, censure indépendante",
    "D ~ Bin(E^i, q), E^i fixe", "D ~ Bin(E^i, q)",
    rep(paste("troncature à gauche et censure non informatives, unités indépendantes ; aucune",
              "hypothèse de répartition dans la cellule"), 3L),
    "sans objet"),
  calcul = c(
    "D / E^c",
    paste("[G_D^-1(alpha/2), G_(D+1)^-1(1 - alpha/2)] / E^c, G_k loi gamma de forme k et",
          "d'échelle 1 ; borne inférieure 0 si D = 0 ; approximation numérique (qgamma)"),
    "taux exp(-+ z / sqrt(D)) : Wald sur log mu ; z = qnorm(alpha/2, lower.tail = FALSE)",
    "-expm1(-taux)",
    "1 - exp(-borne), par -expm1(-borne)",
    "1 - exp(-borne), par -expm1(-borne)",
    "D / E^i",
    paste("[B^-1_(D, E^i - D + 1)(alpha/2), B^-1_(D+1, E^i - D)(1 - alpha/2)], B loi bêta ;",
          "0 si D = 0, 1 si D = E^i ; approximation numérique (qbeta)"),
    paste("plogis(qlogis(q) -+ z sqrt(1/D + 1/(E^i - D))) : Wald sur logit q ;",
          "z = qnorm(alpha/2, lower.tail = FALSE)"),
    "-expm1(sum(log1p(-d/r))) sur les instants d'événement de la cellule",
    "P^2 Sigma, P = exp(sum(log1p(-d/r))) = 1 - q, Sigma = sum d / (r (r - d)) en double",
    paste("plogis(qlogis(q) -+ z sqrt(Sigma) / q) : Wald sur logit q, Var(logit q) ~ Sigma / q^2 ;",
          "z = qnorm(alpha/2, lower.tail = FALSE)"),
    "sans objet"),
  stringsAsFactors = FALSE)

# Contrôle les paramètres propres à l'étape `brut` (valeurs ; les noms sont
# contrôlés par valider_plan()). par_annee : paramètre de l'étape
# `exposition`, déjà contrôlé par controler_parametres_exposition(). Forme
# exacte (#23, forme_exacte()) : type de base exact, aucun attribut (ni
# names, ni dim, ni classe), sinon refus sous le motif du paramètre. Rend
# TRUE, invisible.
controler_parametres_brut <- function(estimateurs, niveau, par_annee) {
  if (!forme_exacte(estimateurs, "character") || !length(estimateurs) || anyNA(estimateurs) ||
      anyDuplicated(estimateurs))
    refuser("brut", "estimateurs_invalides",
            sprintf(paste("Les estimateurs doivent être un vecteur de chaînes non vide, sans attribut,",
                          "sans doublon ni NA, pris dans %s."),
                    liste_guillemets(ESTIMATEURS_BRUT)),
            list(estimateurs = estimateurs))
  inconnus <- setdiff(estimateurs, ESTIMATEURS_BRUT)
  if (length(inconnus))
    refuser("brut", "estimateur_inconnu",
            sprintf("Estimateur(s) inconnu(s) %s (estimateurs disponibles : %s).",
                    liste_guillemets(inconnus), liste_guillemets(ESTIMATEURS_BRUT)),
            list(estimateur = inconnus))
  # Double nu, sans attribut (ni dim, ni names, ni classe).
  if (!forme_exacte(niveau, "double") || length(niveau) != 1L || !is.finite(niveau) ||
      niveau <= 0 || niveau >= 1)
    refuser("brut", "niveau_invalide",
            paste("Le niveau de confiance doit être un nombre (double) unique, sans attribut, fini,",
                  "strictement compris entre 0 et 1."),
            list(niveau = niveau))
  if ("binomial" %in% estimateurs && isTRUE(par_annee))
    refuser("brut", "binomial_par_annee",
            paste("Estimateur binomial refusé sous découpage par année (par_annee = TRUE) : l'exposition",
                  "initiale n'est pas produite sous par_annee, et une cellule âge x année ne couvre pas",
                  "une année d'âge entière, si bien que q_x n'y est pas défini."),
            list(estimateur = "binomial", par_annee = par_annee))
  if ("kaplan_meier" %in% estimateurs && isTRUE(par_annee))
    refuser("brut", "kaplan_meier_par_annee",
            paste("Estimateur de Kaplan-Meier refusé sous découpage par année (par_annee = TRUE) : une",
                  "cellule âge x année ne couvre pas une année d'âge entière ; q_x n'y est pas défini."),
            list(estimateur = "kaplan_meier", par_annee = par_annee))
  invisible(TRUE)
}

# Estimateur de Poisson (A.1, A.3) : D entiers, E expositions centrales,
# niveau dans ]0, 1[. Rend une liste de vecteurs de même longueur : taux,
# quatre bornes, etat. Exposition nulle : NA, état exposition_nulle (D = 0)
# ou evenement_sans_exposition (D > 0, maximum de vraisemblance infini).
taux_poisson <- function(D, E, niveau) {
  n <- length(D)
  alpha <- 1 - niveau
  z <- stats::qnorm(alpha / 2, lower.tail = FALSE)
  etat <- rep("defini", n)
  etat[!(E > 0) & D == 0L] <- "exposition_nulle"
  etat[!(E > 0) & D > 0L] <- "evenement_sans_exposition"
  d <- etat == "defini"
  k <- d & D > 0L
  taux <- inf_e <- sup_e <- inf_a <- sup_a <- rep(NA_real_, n)
  taux[d] <- D[d] / E[d]
  inf_e[d] <- 0
  inf_e[k] <- stats::qgamma(alpha / 2, D[k]) / E[k]
  sup_e[d] <- stats::qgamma(alpha / 2, D[d] + 1, lower.tail = FALSE) / E[d]
  # Wald sur log mu, Var(log mu) ~ 1/D : non défini à D = 0.
  inf_a[k] <- taux[k] * exp(-z / sqrt(D[k]))
  sup_a[k] <- taux[k] * exp(z / sqrt(D[k]))
  list(taux = taux, borne_inf_exacte = inf_e, borne_sup_exacte = sup_e,
       borne_inf_asymptotique = inf_a, borne_sup_asymptotique = sup_a, etat = etat)
}

# Estimateur binomial (A.1, A.3) : D entiers, Ei expositions initiales,
# niveau dans ]0, 1[, tolerance (|Ei - D| <= tolerance : le calcul prend
# Ei := D). Rend la même liste que taux_poisson(), plus effectif_egal :
# cellules définies où D = E^i > 0 (q = 1), celles-là mêmes où les bornes
# asymptotiques sont NA faute de D < E^i. Ei = 0 (D = 0) : état
# exposition_nulle ; Ei < D : état effectif_inferieur_aux_evenements (le
# modèle binomial ne s'applique pas). Ei non entier : prolongement continu de
# la formule exacte (statut « couverture non établie », STATUTS_BRUT).
taux_binomial <- function(D, Ei, niveau, tolerance) {
  n <- length(D)
  alpha <- 1 - niveau
  z <- stats::qnorm(alpha / 2, lower.tail = FALSE)
  N <- Ei
  egal <- abs(Ei - D) <= tolerance
  N[egal] <- D[egal]
  etat <- rep("defini", n)
  etat[N < D] <- "effectif_inferieur_aux_evenements"
  etat[!(N > 0) & D == 0L] <- "exposition_nulle"
  d <- etat == "defini"
  k <- d & D > 0L
  m <- d & D < N
  a <- k & m
  taux <- inf_e <- sup_e <- inf_a <- sup_a <- rep(NA_real_, n)
  taux[d] <- D[d] / N[d]
  inf_e[d] <- 0
  inf_e[k] <- stats::qbeta(alpha / 2, D[k], N[k] - D[k] + 1)
  sup_e[d] <- 1
  sup_e[m] <- stats::qbeta(alpha / 2, D[m] + 1, N[m] - D[m], lower.tail = FALSE)
  # Wald sur logit q, Var(logit q) ~ 1/D + 1/(E^i - D) : non défini à D = 0
  # et à D = E^i.
  l <- stats::qlogis(taux[a])
  s <- z * sqrt(1 / D[a] + 1 / (N[a] - D[a]))
  inf_a[a] <- stats::plogis(l - s)
  sup_a[a] <- stats::plogis(l + s)
  list(taux = taux, borne_inf_exacte = inf_e, borne_sup_exacte = sup_e,
       borne_inf_asymptotique = inf_a, borne_sup_asymptotique = sup_a, etat = etat,
       effectif_egal = k & !m)
}

# Estimateur de Kaplan-Meier sur l'âge (#15, A.1 à A.7), par cellule de la
# table d'exposition.
#   observations : fenêtres transmises par l'étape `exposition` (objet
#                  fenetres_exposition, construire_table(), #38) ou, en
#                  appel direct, observations de l'étape `donnees`
#                  (data.frame) ;
#   exposition   : table de l'étape `exposition`, appariée par clé (segment,
#                  âge), quel que soit l'ordre de ses lignes ;
#   segmentation, transition, periode, delai_declaration : champs du plan ;
#   convention   : convention d'âge retenue par l'étape `exposition` ;
#   niveau       : niveau de confiance de l'intervalle asymptotique ;
#   parametres_exposition : paramètres de l'étape `exposition`
#                  (fin_effective, transition), contrôlés à l'égalité exacte
#                  contre la période et la transition recalculées ici.
# Rend list(table, sauts, diagnostics) : table aux lignes et dans l'ordre de
# la table d'exposition (clés, age, evenements, instants_evenement, taux,
# variance, borne_inf_asymptotique, borne_sup_asymptotique, etat) ; sauts,
# une ligne par (segment, instant d'événement), triée par segment puis par
# instant.
# Fenêtres et événements : ceux de calculer_exposition() lorsqu'ils sont
# transmis ; recalculés par fenetres_observations() (T5) et
# segments_observations(), à l'identique de calculer_exposition(),
# seulement en appel direct sur un data.frame ;
# observation retenue : a < b, ou un
# événement (a <= s = b). Calcul en O(n log n) : r(t) = #{K_a <= t} -
# #{K_b < t} par findInterval() ; couverture, union des [K_a, K_b) non
# vides, par cummax().
taux_kaplan_meier <- function(observations, exposition, segmentation, transition, periode,
                              delai_declaration, convention, niveau, parametres_exposition) {
  p <- controler_parametres_donnees(transition, periode, delai_declaration, segmentation, character(0))
  if (!identical(p$borne - 1, as.numeric(parametres_exposition$fin_effective)) ||
      !identical(p$transition$code, parametres_exposition$transition))
    stop(paste("taux_kaplan_meier() : période ou transition différente de celle de l'étape exposition",
               "(erreur de programmation)."), call. = FALSE)
  # observations (#38) : soit les fenêtres transmises par l'étape
  # `exposition` (objet fenetres_exposition de calculer_exposition(), passé
  # par construire_table()), reprises telles quelles après contrôle à
  # l'égalité exacte de leurs paramètres : début et borne de la période,
  # transition, segmentation (dont dépendent fenetres_observations() et
  # segments_observations()), et convention (qu'elles ne prennent pas : ce
  # contrôle vérifie la cohérence avec la convention de l'étape `exposition`
  # qui a produit la table) ; soit, en appel direct, les observations de
  # l'étape `donnees`, dont les fenêtres et segments sont recalculés à
  # l'identique.
  # La table d'exposition est appariée par clé (segment, âge), dans tout
  # ordre de ses lignes.
  if (inherits(observations, "fenetres_exposition")) {
    fe <- observations
    if (!identical(fe$debut, p$debut) || !identical(fe$borne, p$borne) ||
        !identical(fe$transition, p$transition$code) || !identical(fe$convention, convention) ||
        !identical(fe$segmentation, segmentation))
      stop(paste("taux_kaplan_meier() : fenêtres transmises calculées sous une autre période, transition,",
                 "convention ou segmentation (erreur de programmation)."), call. = FALSE)
  } else {
    manquantes <- setdiff(c("date_naissance", "debut", "fin", "cause", segmentation), names(observations))
    if (length(manquantes))
      stop(sprintf("taux_kaplan_meier() : colonne(s) %s absente(s) des observations (erreur de programmation).",
                   paste(manquantes, collapse = ", ")), call. = FALSE)
    fe <- c(fenetres_observations(observations, p$debut, p$borne, p$transition$arrivee),
            segments_observations(observations, segmentation))
  }
  M <- as.numeric(DENOMINATEUR_INSTANTS)
  z <- stats::qnorm((1 - niveau) / 2, lower.tail = FALSE)
  retenue <- which(fe$a < fe$b | fe$evenement)
  cn <- extraire_composantes(fe$composantes, retenue)
  pa <- position_dans_cellule(fe$calendrier, cn, fe$a[retenue], convention)
  pb <- position_dans_cellule(fe$calendrier, cn, fe$b[retenue], convention)
  # Clé entière de l'instant (A.1) : K = x M + j (M / L), M / L = 366 ou
  # 365 ; segments séparés par le décalage g * etendue, etendue > toute clé
  # (valeurs entières exactes en double).
  n <- nrow(exposition)
  etendue <- (max(c(0, pb$x, exposition$age)) + 2) * M
  g <- fe$g[retenue]
  ka <- g * etendue + pa$x * M + pa$j * (M / pa$longueur)
  kb <- g * etendue + pb$x * M + pb$j * (M / pb$longueur)
  if (length(kb) && max(kb) >= 2^53)
    stop("taux_kaplan_meier() : clé d'instant hors des entiers exacts (erreur de programmation).", call. = FALSE)
  e <- fe$evenement[retenue]

  # Instants d'événement, d et r (ensemble à risque fermé, A.2).
  te <- sort(kb[e])
  tj <- unique(te)
  d <- tabulate(match(te, tj), nbins = length(tj))
  r <- findInterval(tj, sort(ka)) - findInterval(tj, sort(kb), left.open = TRUE)
  gj <- tj %/% etendue
  kj <- tj - gj * etendue
  xj <- kj %/% M
  numerateur <- kj - xj * M

  # Lignes de la table : segment de chaque ligne (clés), début de cellule
  # lo et fin hi sur l'échelle des clés.
  gt <- if (length(segmentation))
    match(do.call(paste, c(unname(as.list(exposition[segmentation])), list(sep = "\r"))),
          do.call(paste, c(unname(as.list(fe$segments)), list(sep = "\r"))))
  else rep(1, n)
  lo <- gt * etendue + exposition$age * M
  hi <- lo + M
  # Appariement par clé (#38) : ligne de la table d'exposition de chaque
  # instant, par le début de sa cellule ; aucune hypothèse sur l'ordre des
  # lignes. Une clé (segment, âge) par ligne (par_annee est refusé).
  kr <- match(gj * etendue + xj * M, lo)
  if (anyDuplicated(lo))
    stop("taux_kaplan_meier() : clé (segment, âge) en double dans la table d'exposition (erreur de programmation).",
         call. = FALSE)
  if (anyNA(kr) || anyNA(gt))
    stop("taux_kaplan_meier() : instant d'événement hors des cellules de la table d'exposition (erreur de programmation).",
         call. = FALSE)

  # Sommes cumulées dans la cellule. Les instants tj sont triés et les
  # cellules sont des intervalles disjoints [lo, hi) de l'échelle des clés :
  # les instants d'une cellule forment une plage contiguë de tj, numérotée
  # par plage, quel que soit l'ordre des lignes (kr n'est pas supposé
  # croissant). Sigma en double (A.7 : r (r - d) déborde en entiers).
  rd <- as.numeric(r); dd <- as.numeric(d)
  plage <- cumsum(c(TRUE, kr[-1L] != kr[-length(kr)]))
  if (anyDuplicated(kr[!duplicated(plage)]))
    stop("taux_kaplan_meier() : instants d'une cellule non contigus (erreur de programmation).", call. = FALSE)
  cumul <- function(v) if (!length(v)) numeric(0) else unlist(lapply(split(v, plage), cumsum), use.names = FALSE)
  log_survie <- cumul(log1p(-dd / rd))
  sigma <- cumul(dd / (rd * (rd - dd)))
  dernier <- !duplicated(kr, fromLast = TRUE)
  L <- S2 <- numeric(n)
  L[kr[dernier]] <- log_survie[dernier]
  S2[kr[dernier]] <- sigma[dernier]
  evenements <- as.integer(somme_par_cellule(kr, d, n))
  instants <- tabulate(kr, nbins = n)

  # Couverture (A.6) : blocs disjoints de l'union des [K_a, K_b) non vides ;
  # t0 premier instant de la cellule où d = r, hi sinon ; cellule définie
  # si t0 = lo ou si un bloc couvre [lo, t0).
  nv <- ka < kb
  o <- order(ka[nv])
  A <- ka[nv][o]; B <- cummax(kb[nv][o])
  nouveau <- c(TRUE, A[-1L] > B[-length(B)])
  debuts <- A[nouveau]
  fins <- B[!duplicated(cumsum(nouveau), fromLast = TRUE)]
  nul <- which(d == r)
  t0 <- hi
  premier_nul <- nul[!duplicated(kr[nul])]
  t0[kr[premier_nul]] <- tj[premier_nul]
  k1 <- findInterval(lo, debuts)
  couvert <- t0 == lo | (k1 > 0L & fins[pmax(k1, 1L)] >= t0)
  k2 <- findInterval(hi, debuts, left.open = TRUE)
  touche <- k2 > 0L & fins[pmax(k2, 1L)] > lo
  etat <- ifelse(couvert, "defini", ifelse(!touche & evenements == 0L, "exposition_nulle",
                                           "ensemble_a_risque_vide"))
  if (!n) etat <- character(0)

  # Taux, variance (méthode delta) et Wald sur logit q (A.3, A.7) ; 0 - x
  # rend +0 pour une cellule sans événement.
  def <- etat == "defini"
  taux <- variance <- inf_a <- sup_a <- rep(NA_real_, n)
  taux[def] <- 0 - expm1(L[def])
  k <- def & taux > 0 & taux < 1
  variance[k] <- exp(L[k])^2 * S2[k]
  l <- stats::qlogis(taux[k])
  w <- z * sqrt(S2[k]) / taux[k]
  inf_a[k] <- stats::plogis(l - w)
  sup_a[k] <- stats::plogis(l + w)

  table <- exposition[c(segmentation, "age")]
  table$evenements <- evenements
  table$instants_evenement <- instants
  table$taux <- taux
  table$variance <- variance
  table$borne_inf_asymptotique <- inf_a
  table$borne_sup_asymptotique <- sup_a
  table$etat <- etat

  sauts <- fe$segments[gj, , drop = FALSE]
  rownames(sauts) <- NULL
  sauts$age <- as.integer(xj)
  sauts$numerateur <- as.integer(numerateur)
  # Quotient correctement arrondi du rationnel K / M = x + j / L.
  sauts$age_exact <- kj / M
  sauts$ensemble_a_risque <- r
  sauts$evenements <- d
  # Survie et Sigma cumulées restituées dans les cellules définies seulement
  # (avis d'actuary sur la reprise de #15, point 2) ; NA ailleurs.
  saut_defini <- def[kr]
  survie <- variance_log <- rep(NA_real_, length(kr))
  survie[saut_defini] <- exp(log_survie[saut_defini])
  variance_log[saut_defini] <- sigma[saut_defini]
  sauts$survie_cellule <- survie
  sauts$variance_log_survie_cellule <- variance_log

  list(table = table, sauts = sauts,
       diagnostics = list(instants = length(tj), evenements = as.integer(sum(d)),
                          # Événements dont l'instant coïncide avec le début
                          # d'observation retenu de l'unité, max(entrée,
                          # début de la période d'observation) ; ceux
                          # qu'exclurait l'ensemble à risque prévisible
                          # {a < t <= b}.
                          evenements_a_l_instant_d_entree = sum(e & ka == kb),
                          # Cellules définies où q = 1 est publié (au moins
                          # un saut d = r).
                          cellules_survie_nulle = length(unique(kr[nul][def[kr[nul]]])),
                          # Maximum de r aux instants d'événement (0 sans
                          # événement).
                          ensemble_a_risque_maximal = if (length(r)) max(r) else 0L))
}

# Étape `brut` du plan.
#   exposition       : table de l'étape `exposition`
#                      (etapes$exposition$resultat$exposition) ;
#   segmentation     : clés de segmentation (champ du plan) ;
#   par_annee        : découpage par année de l'étape `exposition` ;
#   convention       : convention d'âge retenue par l'étape `exposition` ;
#   unite_exposition : unité de l'exposition centrale (étape `exposition`) ;
#   estimateurs      : sous-ensemble de ESTIMATEURS_BRUT ;
#   niveau           : niveau de confiance des intervalles ;
#   observations     : fenêtres transmises par l'étape `exposition`
#                      (construire_table(), #38) ou observations de l'étape
#                      `donnees` (appel direct), et transition,
#                      periode, delai_declaration : champs du plan, et
#                      parametres_exposition : paramètres de l'étape
#                      `exposition` ; requis par Kaplan-Meier seulement
#                      (NULL sinon admis).
# Rend list(parametres, resultat, diagnostics, avertissements) ;
# resultat : une table par estimateur retenu, dans l'ordre canonique, aux
# mêmes lignes et dans le même ordre que la table d'exposition (zéro ligne
# si elle n'en a aucune) ; Kaplan-Meier ajoute la table kaplan_meier_sauts
# (une ligne par instant d'événement).
calculer_taux_bruts <- function(exposition, segmentation, par_annee, convention, unite_exposition,
                                estimateurs, niveau, observations = NULL, transition = NULL,
                                periode = NULL, delai_declaration = NULL, parametres_exposition = NULL) {
  # Forme seule de la segmentation (#43, forme_exacte()) : type, attributs, NA,
  # doublons ; appel direct de la fonction d'étape. Le catalogue
  # ROLES_SEGMENTATION est contrôlé en amont par valider_plan() et, sur le
  # chemin Kaplan-Meier, par controler_parametres_donnees().
  if (!forme_exacte(segmentation, "character") || anyNA(segmentation) || anyDuplicated(segmentation))
    refuser("brut", "segmentation_invalide",
            "La segmentation doit être un vecteur de chaînes sans attribut, sans doublon ni NA (vide explicite admis).",
            list(segmentation = segmentation))
  controler_parametres_brut(estimateurs, niveau, par_annee)
  retenus <- ESTIMATEURS_BRUT[ESTIMATEURS_BRUT %in% estimateurs]
  cles <- c(segmentation, "age", if (isTRUE(par_annee)) "annee")
  requises <- c(cles, "evenements", "exposition_centrale",
                if ("binomial" %in% retenus) "complement_initial_cellule")
  manquantes <- setdiff(requises, names(exposition))
  if (length(manquantes))
    stop(sprintf("calculer_taux_bruts() : colonne(s) %s absente(s) de la table d'exposition (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  D <- exposition$evenements
  Ec <- exposition$exposition_centrale
  ajouter <- function(t, colonnes) {
    for (nom in names(colonnes)) t[[nom]] <- colonnes[[nom]]
    t
  }

  resultat <- list()
  effectif_egal <- NA_integer_
  avert <- avertissements_vides()
  hypotheses <- list()
  if ("poisson" %in% retenus) {
    p <- taux_poisson(D, Ec, niveau)
    tp <- ajouter(exposition[cles], list(evenements = D, exposition_centrale = Ec))
    tp <- ajouter(tp, p[c("taux", "borne_inf_exacte", "borne_sup_exacte",
                          "borne_inf_asymptotique", "borne_sup_asymptotique")])
    tp <- ajouter(tp, list(probabilite = -expm1(-p$taux),
                           probabilite_borne_inf_exacte = -expm1(-p$borne_inf_exacte),
                           probabilite_borne_sup_exacte = -expm1(-p$borne_sup_exacte),
                           probabilite_borne_inf_asymptotique = -expm1(-p$borne_inf_asymptotique),
                           probabilite_borne_sup_asymptotique = -expm1(-p$borne_sup_asymptotique),
                           etat = p$etat))
    resultat$poisson <- tp
    hypotheses$poisson <- HYPOTHESES_BRUT$poisson
    sans_expo <- p$etat == "evenement_sans_exposition"
    if (any(sans_expo))
      avert <- avertir(avert, "brut", "poisson_evenement_sans_exposition",
                       sprintf(paste("Poisson : %d cellule(s) à exposition centrale nulle qui comptent %d",
                                     "événement(s) : taux et bornes non définis (NA, état",
                                     "evenement_sans_exposition)."),
                               sum(sans_expo), sum(D[sans_expo])),
                       sum(sans_expo))
  }
  if ("binomial" %in% retenus) {
    Ei <- Ec + exposition$complement_initial_cellule
    b <- taux_binomial(D, Ei, niveau, TOLERANCE_EFFECTIF)
    tb <- ajouter(exposition[cles], list(evenements = D, exposition_initiale = Ei))
    tb <- ajouter(tb, b[c("taux", "borne_inf_exacte", "borne_sup_exacte",
                          "borne_inf_asymptotique", "borne_sup_asymptotique", "etat")])
    resultat$binomial <- tb
    effectif_egal <- sum(b$effectif_egal)
    hypotheses$binomial <- HYPOTHESES_BRUT$binomial
    inferieur <- b$etat == "effectif_inferieur_aux_evenements"
    if (any(inferieur))
      avert <- avertir(avert, "brut", "binomial_effectif_inferieur_aux_evenements",
                       sprintf(paste("Binomial : %d cellule(s) dont l'exposition initiale est inférieure au",
                                     "nombre d'événements : modèle binomial inapplicable, taux et bornes",
                                     "non définis (NA, état effectif_inferieur_aux_evenements)."),
                               sum(inferieur)),
                       sum(inferieur))
  }
  diag_km <- NULL
  if ("kaplan_meier" %in% retenus) {
    if (is.null(observations))
      stop("calculer_taux_bruts() : observations requises par Kaplan-Meier (erreur de programmation).",
           call. = FALSE)
    km <- taux_kaplan_meier(observations, exposition, segmentation, transition, periode,
                            delai_declaration, convention, niveau, parametres_exposition)
    if (!identical(km$table$evenements, exposition$evenements))
      stop("calculer_taux_bruts() : somme de d par cellule différente des événements de l'exposition (erreur de programmation).",
           call. = FALSE)
    resultat$kaplan_meier <- km$table
    resultat$kaplan_meier_sauts <- km$sauts
    diag_km <- km$diagnostics
    hypotheses$kaplan_meier <- HYPOTHESES_BRUT$kaplan_meier
    # Avertissement réservé aux cellules à couverture incomplète qui portent
    # au moins un événement (décision du mainteneur du 17/10/2026) ; toutes
    # les cellules de cet état sont relevées par diagnostics$etats.
    perdus <- km$table$etat == "ensemble_a_risque_vide" & km$table$evenements > 0L
    if (any(perdus))
      avert <- avertir(avert, "brut", "kaplan_meier_ensemble_a_risque_vide",
                       sprintf(paste("Kaplan-Meier : %d cellule(s) à couverture incomplète de l'ensemble à",
                                     "risque qui comptent %d événement(s) : taux, variance et bornes non",
                                     "définis (NA, état ensemble_a_risque_vide) ; relevé de toutes les",
                                     "cellules de cet état : diagnostics$etats."),
                               sum(perdus), sum(km$table$evenements[perdus])),
                       sum(perdus))
  }
  if ("poisson" %in% retenus)
    hypotheses$probabilite <- paste(HYPOTHESES_BRUT$probabilite,
                                    PORTEE_PROBABILITE[[if (isTRUE(par_annee)) "par_annee" else convention]])
  statuts <- STATUTS_BRUT[STATUTS_BRUT$estimateur %in% retenus, , drop = FALSE]
  rownames(statuts) <- NULL

  etats <- do.call(rbind, lapply(retenus, function(e)
    data.frame(estimateur = e, etat = ETATS_BRUT[[e]],
               cellules = vapply(ETATS_BRUT[[e]], function(s) sum(resultat[[e]]$etat == s), integer(1L),
                                 USE.NAMES = FALSE),
               stringsAsFactors = FALSE)))
  rownames(etats) <- NULL

  parametres <- list(estimateurs = retenus, niveau = niveau,
                     exposition_initiale = EXPOSITION_INITIALE_BRUT,
                     tolerance_effectif = TOLERANCE_EFFECTIF, convention = convention,
                     unite_exposition = unite_exposition,
                     # Texte du moteur marqué UTF-8 en toute locale (#50).
                     hypotheses = texte_moteur(hypotheses), statuts = texte_moteur(statuts))
  diagnostics <- list(cellules = nrow(exposition), evenements = sum(D),
                      cellules_sans_evenement = sum(D == 0L),
                      # D = E^i > 0 à la tolérance près (q = 1) : masque de
                      # taux_binomial() ; NA si le binomial n'est pas retenu.
                      cellules_effectif_egal_aux_evenements = effectif_egal,
                      etats = etats)
  # Champs de Kaplan-Meier ajoutés seulement si l'estimateur est retenu : un
  # plan sans kaplan_meier garde l'objet antérieur à #15.
  if (!is.null(diag_km)) {
    parametres$ensemble_a_risque <- ENSEMBLE_A_RISQUE_KM
    parametres$denominateur_instants <- DENOMINATEUR_INSTANTS
    diagnostics$kaplan_meier <- diag_km
  }
  list(parametres = parametres, resultat = resultat, diagnostics = diagnostics, avertissements = avert)
}
