# -----------------------------------------------------------------------------
#  R/brut.R : module `brut`, taux bruts par cellule (issue #14 ; E2.12,
#  E2.14, E2.20 ; ADR 0007, points 2.1, 5.2, 6.1 et 6.5 ; spécification
#  d'actuary du 17/10/2026, A.1 à A.5 ; décisions du mainteneur du
#  17/10/2026 sur ses questions A.8)
#
#  - controler_parametres_brut() : paramètres de l'étape (estimateurs,
#    niveau) et refus du binomial sous découpage par année ; appelée par
#    valider_plan() et par la fonction d'étape ;
#  - taux_poisson(), taux_binomial() : estimateur, intervalles exacts et
#    asymptotiques et état de chaque cellule ;
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
#  Ce module n'appelle que `donnees` (liste_guillemets()) et `outils`
#  (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# Estimateurs disponibles, dans l'ordre canonique des résultats (A.4).
ESTIMATEURS_BRUT <- c("poisson", "binomial")
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
  binomial = c("defini", "exposition_nulle", "effectif_inferieur_aux_evenements"))

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
  estimateur = c(rep("poisson", 6L), rep("binomial", 3L)),
  grandeur = c(rep("taux", 3L), rep("probabilite", 3L), rep("taux", 3L)),
  intervalle = rep(c("sans objet (estimation ponctuelle)", "exact", "asymptotique"), 3L),
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
    "asymptotique (E^i q(1-q) -> infini) ; non défini à D = 0 et à D = E^i"),
  loi_supposee = c(
    rep(c("force constante dans la cellule (vraisemblance de survie, proportionnelle à celle de Poisson)",
          "D ~ Poisson(mu E^c), E^c fixe", "D ~ Poisson(mu E^c)"), 2L),
    "répartition _(1-t)q_(x+t) = (1 - t) q_x dans la cellule, censure indépendante",
    "D ~ Bin(E^i, q), E^i fixe", "D ~ Bin(E^i, q)"),
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
          "z = qnorm(alpha/2, lower.tail = FALSE)")),
  stringsAsFactors = FALSE)

# Contrôle les paramètres propres à l'étape `brut` (valeurs ; les noms sont
# contrôlés par valider_plan()). par_annee : paramètre de l'étape
# `exposition`, déjà contrôlé par controler_parametres_exposition(). Rend
# TRUE, invisible.
controler_parametres_brut <- function(estimateurs, niveau, par_annee) {
  if (!is.character(estimateurs) || !length(estimateurs) || anyNA(estimateurs) ||
      anyDuplicated(estimateurs))
    refuser("brut", "estimateurs_invalides",
            sprintf("Les estimateurs doivent être un vecteur de chaînes non vide, sans doublon ni NA, pris dans %s.",
                    liste_guillemets(ESTIMATEURS_BRUT)),
            list(estimateurs = estimateurs))
  inconnus <- setdiff(estimateurs, ESTIMATEURS_BRUT)
  if (length(inconnus))
    refuser("brut", "estimateur_inconnu",
            sprintf("Estimateur(s) inconnu(s) %s (estimateurs disponibles : %s).",
                    liste_guillemets(inconnus), liste_guillemets(ESTIMATEURS_BRUT)),
            list(estimateur = inconnus))
  # Double nu, sans attribut (ni dim, ni names, ni classe).
  if (!is.double(niveau) || !is.null(attributes(niveau)) || length(niveau) != 1L || !is.finite(niveau) ||
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

# Étape `brut` du plan.
#   exposition       : table de l'étape `exposition`
#                      (etapes$exposition$resultat$exposition) ;
#   segmentation     : clés de segmentation (champ du plan) ;
#   par_annee        : découpage par année de l'étape `exposition` ;
#   convention       : convention d'âge retenue par l'étape `exposition` ;
#   unite_exposition : unité de l'exposition centrale (étape `exposition`) ;
#   estimateurs      : sous-ensemble de ESTIMATEURS_BRUT ;
#   niveau           : niveau de confiance des intervalles.
# Rend list(parametres, resultat, diagnostics, avertissements) ;
# resultat : une table par estimateur retenu, dans l'ordre canonique, aux
# mêmes lignes et dans le même ordre que la table d'exposition (zéro ligne
# si elle n'en a aucune).
calculer_taux_bruts <- function(exposition, segmentation, par_annee, convention, unite_exposition,
                                estimateurs, niveau) {
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

  list(
    parametres = list(estimateurs = retenus, niveau = niveau,
                      exposition_initiale = EXPOSITION_INITIALE_BRUT,
                      tolerance_effectif = TOLERANCE_EFFECTIF, convention = convention,
                      unite_exposition = unite_exposition, hypotheses = hypotheses, statuts = statuts),
    resultat = resultat,
    diagnostics = list(cellules = nrow(exposition), evenements = sum(D),
                       cellules_sans_evenement = sum(D == 0L),
                       # D = E^i > 0 à la tolérance près (q = 1) : masque de
                       # taux_binomial() ; NA si le binomial n'est pas retenu.
                       cellules_effectif_egal_aux_evenements = effectif_egal,
                       etats = etats),
    avertissements = avert
  )
}
