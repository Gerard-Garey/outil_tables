# -----------------------------------------------------------------------------
#  R/lissage.R : module `lissage`, étape `lissage` (issues #223 et #225 ;
#  E2.12, variante (a) : lissage de Whittaker-Henderson, critère quadratique ;
#  variante (b) : vraisemblance de Poisson pénalisée sur ln mu, paramètre de
#  lissage déclaré ou choisi par la LAML ; ADR 0017, points 1 à 4, 9, 10, 11
#  et 15 ; corps de #223 et de #225, décisions D1 à D13 du mainteneur du
#  11/11/2026, M413 ; spécifications de forme (0')(b) d'actuary-approfondi,
#  M415 et M416)
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
#  - deviance_penalisee(), iteration_pirls(), log_det_penalite(),
#    lisser_poisson(), choisir_lambda_poisson() : variante (b) (#225) :
#    déviance pénalisée, itération de Newton (PIRLS) par QR des moindres
#    carrés augmentés, ln |K'K|+, maximum de la vraisemblance pénalisée d'un
#    segment à lambda fixé (leviers, ddl effectifs, LAML, variances a
#    posteriori), choix de lambda par la LAML (grille, puis Brent) ;
#  - calculer_lissage() : fonction d'étape `lissage`.
#
#  Notations (a) (Lignes directrices mortalité, chap. 3, IV.2.a, p. 31-32,
#  et IV.2.d, p. 33) : q^ = D / E^i, estimateur binomial de l'étape brut
#  (D1) ; w poids ; h paramètre de lissage (parametre_lissage, déclaré, D3) ;
#  z ordre des différences (ordre_difference) ; K matrice des différences
#  avant d'ordre z, (n - z) x n ; g minimise M = F + h S, F = somme des w (g - q^)^2,
#  S = somme des (Delta^z g)^2. Aucune extrapolation hors de la plage (D11) ;
#  aucun écrêtage (D4).
#  Notations (b) (G. Biessy, « Whittaker-Henderson Smoothing Revisited »,
#  arXiv:2306.06932 v5, § 4.1, § 5.4 et annexe D) : D décès et E^c exposition
#  centrale de chaque cellule (statistiques de l'estimateur poisson) ;
#  theta = ln mu ; l_P(theta) = somme des (D theta - E^c e^theta) - lambda
#  S(theta) / 2 ; m = E^c e^theta ; W = Diag(m) ; P = lambda K'K ; déviance
#  pénalisée dev_P = 2 somme des [D ln(D / m) - (D - m)] (0 ln 0 = 0, cellules
#  E^c > 0) + lambda S(theta) ; LAML = somme des (D theta^ - m^) - [lambda
#  S(theta^) + ln |W + P| - ln |P|+ - z ln(2 pi)] / 2 ; q = 1 - exp(-mu^).
#  Ce module n'appelle que `outils` (refuser(), formuler(), forme_exacte(),
#  chaine_unique(), valeurs_entieres_double(), denombrement_entier(),
#  texte_moteur(), avertissements_vides(), avertir()), `donnees` (citer(),
#  liste_guillemets()) et `exposition` (controler_plage_ages(),
#  CONVENTIONS_VALIDATION, grille_plage()) (ADR 0007, point 2.1 ; ADR 0017,
#  point 10.4).
# -----------------------------------------------------------------------------

# Méthodes de lissage mises en oeuvre (E2.12 ; (a) en #223, wh_poisson ajoutée
# par #225), critères de choix du paramètre de lissage (D3, D5) et critères
# admis par méthode (contrôle croisé des gouvernants, ADR 0017, point 3.4 :
# « laml » refusé sous wh_quadratique, admis sous wh_poisson).
METHODES_LISSAGE <- c("wh_quadratique", "wh_poisson")
CRITERES_LISSAGE <- c("declare", "laml")
CRITERES_ADMIS_LISSAGE <- list(wh_quadratique = "declare", wh_poisson = c("declare", "laml"))
# Pondérations du critère quadratique (D2 ; Lignes directrices, p. 31 et 32 :
# type A, w = 1 ; type B, w = E^i / moyenne de E^i).
PONDERATIONS_LISSAGE <- c("uniforme", "exposition")
# Ordres des différences admis (Lignes directrices, p. 31 : « en pratique,
# z = 2, 3 ou 4 » ; z = 1 admis par la spécification de #223).
ORDRES_DIFFERENCE <- c(1, 2, 3, 4)
# Estimateur brut lu par chaque méthode (D1 ; poisson sous (b), #225) et
# échelle de la pénalité S.
ESTIMATEUR_LISSAGE <- c(wh_quadratique = "binomial", wh_poisson = "poisson")
ECHELLE_PENALITE <- c(wh_quadratique = "q", wh_poisson = "ln_mu")
# États d'une ligne de la table lissée : observee (poids non nul) ;
# interpolee (poids nul : cellule sans exposition, valeur interpolée par le
# lissage ; Lignes directrices, p. 33), prioritaire ; qx_arrondi_un (sous
# wh_poisson : qx ou une borne de crédibilité vaut 1 par arrondi, mu >= 54
# ln 2 ; précédent du positionnement).
ETATS_TABLE_LISSEE <- c("observee", "interpolee", "qx_arrondi_un")
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
  "2006, chap. 3, IV.2.a (p. 31-32) et IV.2.d (p. 33), SHA-256",
  "70ba7463b840bb4fd378dd05c7dad66213239d0bb033ea126b7b0b4669d26835 ; écarts documentés")
# Variante (b) (#225 ; spécification de forme, §§ 2 et 3) : hypothèse, règles
# non tirées de la source comprises (D12), et référence.
HYPOTHESE_LISSAGE_POISSON <- paste(
  "D et E^c : décès et exposition centrale de chaque cellule (segment x âge) de la plage, statistiques de",
  "l'estimateur poisson de l'étape brut (force constante par âge entier) ; vraisemblance de Poisson exacte",
  "sur theta = ln mu, sans surdispersion (sigma^2 = 1), pénalisée par lambda S(theta) / 2, S = somme des",
  "(Delta^z theta)^2 ; theta^ : maximum de la vraisemblance pénalisée (mode a posteriori) ; cellules sans",
  "exposition (E^c = 0, D = 0) de poids nul, valeur interpolée, aucune extrapolation hors de la plage ;",
  "aucun refus D > E^i. Algorithme de la source (annexe D, algorithme 1) : itérations de Newton poursuivies",
  "tant que la déviance pénalisée décroît d'au moins EPSILON_DEVIANCE_LISSAGE en relatif (« < » au lieu du « <= »",
  "de la source, pour arrêter à l'égalité). Règles non tirées de la source, reprises du code de WH 2.0.0 :",
  "départ theta_0 = ln(max(D, EPSILON_DEPART_LISSAGE) / E^c) où E^c > 0 et theta_0 = 0 aux cellules sans",
  "exposition ; réponse de travail z = theta aux cellules sans exposition ; divisions du pas par deux tant que la",
  "déviance pénalisée croît ou n'est pas finie. Règles de l'outil, non tirées de la source : réponse de",
  "travail theta - 1 aux cellules exposées sans décès (D / m pris nul, valeur exacte pour m > 0, conservée",
  "quand m est arrondi à 0) ; à l'épuisement des divisions du pas, arrêt au dernier itéré si la diminution",
  "de la déviance pénalisée prédite par le pas de Newton (décrément d'(W + P)d) est au plus",
  "EPSILON_DEVIANCE_LISSAGE |dev_P| + epsilon machine x somme des D, refus lissage_non_convergent sinon ; rang",
  "numérique du système perdu : refus lissage_systeme_rang_deficient à la première itération ou au maximum,",
  "lissage_non_convergent à une itération suivante ; choix de lambda par la LAML : grille de ln lambda,",
  "extrémités évaluées aux bornes déclarées, puis, si le meilleur point de la grille est intérieur, méthode",
  "de Brent dans son encadrement ; à l'état lambda_a_la_borne, lambda retenu = la borne déclarée, sans",
  "recherche de Brent ; tout échec de l'algorithme en un point évalué refuse le segment. q = 1 - exp(-mu^), règle",
  "regle_force.")
REFERENCE_METHODE_LISSAGE_POISSON <- paste(
  "G. Biessy, « Whittaker-Henderson Smoothing Revisited », arXiv:2306.06932 v5, SHA-256",
  "1af94c962b3f6b9cc1bc571ccec0885630454cb52f14b6d43753b8b0ed0b1cdd : § 4.1, § 5.4 (p. 19), annexe D,",
  "algorithme 1 (p. 54) ; critère LAML (« REML » réservé au cas normal, D5) ; points non tirés de la source",
  "documentés")
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
# Statuts sous wh_poisson (#225 ; spécification de forme, § 3) ; la ligne
# segments/parametre_lissage n'est rendue que sous critere_lissage = laml
# (lambda déclaré sinon).
STATUTS_LISSAGE_POISSON <- local({
  descriptif <- "descriptif, arithmétique flottante"
  residu <- paste("nuls à la convergence près : somme de (m^ - D) par l'équation du score (P 1 = 0) ; somme de",
                  "x (m^ - D) pour z >= 2")
  bornes <- paste("intervalles bayésiens asymptotiques (approximation de Laplace), conditionnels au paramètre de",
                  "lissage retenu : theta^ ± z_alpha sqrt(diag((W + P)^-1)), z_alpha quantile d'ordre (1 + niveau) /",
                  "2 de la loi normale, image par q = 1 - exp(-exp(theta)) ; ce ne sont pas des intervalles de",
                  "confiance ; couverture fréquentiste non établie (aucun constat de simulation versé)")
  l <- list(
    c("table_lissee/qx",
      paste("-expm1(-exp(theta^)), theta^ maximum de la vraisemblance de Poisson pénalisée (mode a posteriori),",
            "caractérisation exacte ; valeur calculée par l'algorithme itératif arrêté sur la déviance pénalisée,",
            "approximation numérique ; aucune propriété fréquentiste revendiquée")),
    c("table_lissee/levier",
      paste("approximation numérique : élément diagonal de la matrice de lissage à la convergence, m^_i V_ii,",
            "V = (W + lambda K'K)^-1, W = Diag(m^) au maximum (résolution QR, R base) ; dans [0, 1] en",
            "arithmétique exacte, égal à 1 seulement dans un segment qui compte exactement z cellules exposées ;",
            "0 exactement sur une cellule sans exposition ; somme sur le segment = ddl_effectifs")),
    c("table_lissee/borne_inf_credibilite", bornes),
    c("table_lissee/borne_sup_credibilite", bornes),
    c("regle_force",
      paste("transformation exacte sous la force constante par âge entier, qui est l'hypothèse de l'estimateur",
            "poisson d'entrée ; q = -expm1(-mu^), mu^ relu par -log1p(-q) à quelques unités de dernier rang près",
            "(sauf q arrondi à 1, état qx_arrondi_un)")),
    c("segments/parametre_lissage",
      paste("maximiseur local, sur l'intervalle déclaré (meilleur point d'une grille de ln lambda, puis, si le",
            "meilleur point de la grille est intérieur, méthode de Brent dans son encadrement ; à l'état",
            "lambda_a_la_borne, lambda retenu = la borne déclarée, sans recherche de Brent), d'un critère approché",
            "(approximation de Laplace, exacte dans le seul cas",
            "normal) ; unimodalité non établie ; approximation numérique, de",
            "précision fixée par mesure (classe optimisation) ; état lambda_a_la_borne si le meilleur point de la",
            "grille est une borne")),
    c("segments/ddl_effectifs",
      paste("trace de (W + P)^-1 W, W à la convergence ; définition du cas normal transposée ; approximation",
            "numérique")),
    c("segments/regularite", paste("descriptif, arithmétique flottante : somme des (Delta^z theta^)^2, échelle",
                                   "ln mu")),
    c("segments/laml",
      paste("approximation de Laplace de la vraisemblance marginale (Biessy, § 5.4, p. 19), exacte dans le seul cas",
            "normal ; valeur calculée au maximum approché, approximation numérique ; log-vraisemblance de la source",
            "(§ 4.1), sans le terme - somme des ln D! (constant en lambda)")),
    c("segments/deviance_penalisee",
      "valeur de la déviance pénalisée en theta^ calculé (minimum approché), approximation numérique"),
    c("segments/residu_conservation_0", residu),
    c("segments/residu_conservation_1", residu))
  data.frame(grandeur = vapply(l, `[`, "", 1L), statut = vapply(l, `[`, "", 2L), stringsAsFactors = FALSE)
})
# Constantes numériques de l'étape (ADR 0017, point 9.3 ; D12) : aucune sous
# wh_quadratique (data.frame à zéro ligne, colonnes fixées).
CONSTANTES_LISSAGE <- data.frame(constante = character(0), valeur = numeric(0), origine = character(0),
                                 stringsAsFactors = FALSE)
# Constantes de wh_poisson (#225 ; spécification de forme, § 2) : epsilon de
# départ (d* = max(D, epsilon)), seuil relatif d'arrêt sur la déviance
# pénalisée, plafond d'itérations, plafond de divisions du pas, points de la
# grille de ln lambda et tolérance de Brent (en ln lambda ; ces deux-ci sous
# critere_lissage = laml seulement).
EPSILON_DEPART_LISSAGE <- 1e-8
EPSILON_DEVIANCE_LISSAGE <- 1e-8
ITERATIONS_MAX_LISSAGE <- 100L
DEMI_PAS_MAX_LISSAGE <- 30L
POINTS_GRILLE_LISSAGE <- 31L
TOLERANCE_BRENT_LISSAGE <- 1e-6
CONSTANTES_LISSAGE_POISSON <- data.frame(
  constante = c("EPSILON_DEPART_LISSAGE", "EPSILON_DEVIANCE_LISSAGE", "ITERATIONS_MAX_LISSAGE",
                "DEMI_PAS_MAX_LISSAGE", "POINTS_GRILLE_LISSAGE", "TOLERANCE_BRENT_LISSAGE"),
  valeur = c(EPSILON_DEPART_LISSAGE, EPSILON_DEVIANCE_LISSAGE, ITERATIONS_MAX_LISSAGE, DEMI_PAS_MAX_LISSAGE,
             POINTS_GRILLE_LISSAGE, TOLERANCE_BRENT_LISSAGE),
  origine = c("non tirée de la source (code de WH 2.0.0, init_y_hat)",
              "tirée de la source (algorithme 1, annexe D, p. 54)",
              "non tirée de la source",
              "non tirée de la source (recherche linéaire du code de WH 2.0.0)",
              "non tirée de la source ; critere_lissage = laml seulement",
              "non tirée de la source (en ln lambda) ; critere_lissage = laml seulement"),
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
# (ponderation_invalide), niveau (niveau_invalide, message propre aux
# intervalles de crédibilité ; #225), parametre_lissage (parametre_lissage_invalide),
# intervalle_lissage (intervalle_lissage_invalide ; #225) ; puis
# inter-étapes : estimateur_lissage_absent (D1 ; poisson sous wh_poisson),
# lissage_par_annee. Forme exacte (#23) ; un integer est refusé (M257). Rend
# TRUE, invisible.
controler_parametres_lissage <- function(methode, plage_ages, ordre_difference, critere_lissage, ponderation,
                                         niveau, parametre_lissage, intervalle_lissage, brut, par_annee) {
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
  if (!missing(niveau) &&
      (!forme_exacte(niveau, "double") || length(niveau) != 1L || !is.finite(niveau) || niveau <= 0 || niveau >= 1))
    refuser("lissage", "niveau_invalide",
            formuler(paste("Le niveau des intervalles de crédibilité (niveau) doit être un nombre (double) unique,",
                           "sans attribut, fini, strictement compris entre 0 et 1.")),
            list(niveau = niveau))
  if (!missing(parametre_lissage) &&
      (!forme_exacte(parametre_lissage, "double") || length(parametre_lissage) != 1L ||
       !is.finite(parametre_lissage) || parametre_lissage <= 0))
    refuser("lissage", "parametre_lissage_invalide",
            formuler(paste("Le paramètre de lissage (parametre_lissage : h sous wh_quadratique, lambda sous",
                           "wh_poisson) doit être un nombre double unique, sans attribut, fini, strictement positif,",
                           "sans défaut.")),
            list(parametre_lissage = parametre_lissage))
  if (!missing(intervalle_lissage) &&
      (!forme_exacte(intervalle_lissage, "double") || length(intervalle_lissage) != 2L ||
       !all(is.finite(intervalle_lissage)) || !(intervalle_lissage[1L] > 0) ||
       !(intervalle_lissage[1L] < intervalle_lissage[2L])))
    refuser("lissage", "intervalle_lissage_invalide",
            formuler(paste("L'intervalle du paramètre de lissage (intervalle_lissage) doit être un vecteur double sans",
                           "attribut de deux valeurs finies 0 < a < b (bornes de lambda ; recherche de ln lambda dans",
                           "[ln a, ln b]), sans défaut.")),
            list(intervalle_lissage = intervalle_lissage))
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

# Déviance pénalisée de wh_poisson (#225 ; spécification de forme, § 2 ;
# non écrite par la source, D12) : 2 somme des [D ln(D / m) - (D - m)] sur
# les cellules exposées (pond, E^c > 0 ; 0 ln 0 = 0), m = E^c e^theta, plus
# lambda S(theta), S par somme_differences_carrees() (jamais theta' P theta :
# LAML bruitée et NaN à lambda >= 1e15 sous cette forme, mesure
# d'actuary). Rend un double, éventuellement non fini (m nul ou infini).
deviance_penalisee <- function(theta, D, Ec, pond, lambda, z) {
  m <- exp(theta[pond]) * Ec[pond]
  d <- D[pond]
  2 * sum(ifelse(d > 0, d * log(d / m), 0) - (d - m)) + lambda * somme_differences_carrees(theta, z)
}

# Une itération de Newton de wh_poisson (PIRLS ; Biessy, p. 13, et annexe
# D, algorithme 1, p. 54) : m = E^c e^theta (0 aux cellules sans
# exposition), réponse de travail z_k = theta + D / m - 1 où E^c > 0, theta
# sinon (code de WH 2.0.0, compute_z ; D12) ; aux cellules exposées sans
# décès, D / m pris nul (z_k = theta - 1 : valeur exacte pour m > 0,
# conservée quand m est arrondi à 0 ; règle de l'outil, réserve R-a
# d'actuary-approfondi), puis theta_new par QR des
# moindres carrés augmentés [W^(1/2) ; sqrt(lambda) K] (A = sqrt(lambda) K,
# qr(LAPACK = FALSE), qr.coef() ; ADR 0017, point 9.1). Rend list(fini,
# rang, theta, racine_m) ; fini = FALSE (m ou z_k non fini : l'algorithme
# diverge) sans autre élément ; rang < n : theta absent.
iteration_pirls <- function(theta, D, Ec, pond, A) {
  n <- length(theta)
  m <- numeric(n)
  m[pond] <- exp(theta[pond]) * Ec[pond]
  zk <- theta
  zk[pond] <- theta[pond] + ifelse(D[pond] > 0, D[pond] / m[pond], 0) - 1
  if (!all(is.finite(m)) || !all(is.finite(zk))) return(list(fini = FALSE))
  r <- sqrt(m)
  Q <- qr(rbind(diag(r, n), A), LAPACK = FALSE)
  if (Q$rank < n) return(list(fini = TRUE, rang = Q$rank))
  list(fini = TRUE, rang = Q$rank, theta = qr.coef(Q, c(r * zk, numeric(nrow(A)))), racine_m = r)
}

# ln |K'K|+ (spécification de forme, § 2) : somme des logarithmes des n - z
# plus grandes valeurs propres de K'K, K = diff(diag(n), differences = z)
# (eigen(symmetric = TRUE, only.values = TRUE), valeurs décroissantes) ; ne
# dépend que de n et z. ln |P|+ = (n - z) ln lambda + ce terme. Rend un
# double.
log_det_penalite <- function(n, z) {
  s <- eigen(crossprod(diff(diag(n), differences = z)), symmetric = TRUE, only.values = TRUE)$values
  sum(log(s[seq_len(n - z)]))
}

# Maximum de la vraisemblance de Poisson pénalisée d'un segment à lambda
# fixé (#225 ; spécification de forme, § 2) : D (décès), Ec (exposition
# centrale), x (âges), vecteurs de même longueur n > z, cellules sans
# exposition de D nul, au moins z cellules exposées (contrôlés par
# l'appelant) ; lambda > 0 ; z ordre ; log_det_p = log_det_penalite(n, z) ;
# epsilon : epsilon de départ (EPSILON_DEPART_LISSAGE ; argument pour le
# critère A4 de #225). Départ theta_0 = ln(max(D, epsilon) / E^c) où
# E^c > 0, 0 sinon ; itérations de Newton (iteration_pirls()) ; recherche
# linéaire : tant que dev_P du candidat est non finie ou supérieure à celle
# de l'itéré, au plus DEMI_PAS_MAX_LISSAGE divisions du pas par deux ; à leur
# épuisement, arrêt au dernier itéré si le décrément de Newton d'(W + P)d,
# d pas complet, est fini et au plus EPSILON_DEVIANCE_LISSAGE |dev_P| +
# .Machine$double.eps x somme des D (diminution non résoluble : convergence
# numérique, règle de l'outil), sinon non convergence ; poursuite si et
# seulement si dev_new < (1 - EPSILON_DEVIANCE_LISSAGE) dev_old ; plafond
# ITERATIONS_MAX_LISSAGE itérations. Rang numérique perdu : à la première
# itération (depuis theta_0) ou au maximum, rang_deficient (lambda trop grand
# pour la tolérance de rang de qr()) ; à une itération suivante, non
# convergence (l'itéré dégénère : m tend vers 0 en certaines cellules). Après
# convergence, QR à theta^ :
# V = (W + P)^-1 = chol2inv(R) remise dans l'ordre du pivot, leviers
# m^_i V_ii (0 exactement aux cellules sans exposition), ddl effectifs =
# leur somme (identité au bit près avec la colonne levier ; ADR 0017,
# point 5.5 (ii)), LAML avec ln |W + P| = 2 somme des ln |R_ii|. Rend
# list(statut = "non_convergent" | "rang_deficient", iterations) en cas
# d'échec, sinon list(statut = "produite", iterations (integer), theta,
# variance (diag(V)), levier, ddl_effectifs, regularite (S(theta^)), laml,
# deviance_penalisee, residu_conservation_0 (somme des (m^ - D)),
# residu_conservation_1 (somme des x (m^ - D))).
lisser_poisson <- function(D, Ec, lambda, z, x, log_det_p, epsilon = EPSILON_DEPART_LISSAGE) {
  n <- length(D)
  pond <- Ec > 0
  A <- sqrt(lambda) * diff(diag(n), differences = z)
  theta <- numeric(n)
  theta[pond] <- log(pmax(D[pond], epsilon) / Ec[pond])
  dev <- deviance_penalisee(theta, D, Ec, pond, lambda, z)
  plancher <- .Machine$double.eps * sum(D)
  echec <- function(statut, k) list(statut = statut, iterations = k)
  k <- 0L
  repeat {
    if (k >= ITERATIONS_MAX_LISSAGE) return(echec("non_convergent", k))
    it <- iteration_pirls(theta, D, Ec, pond, A)
    if (!it$fini) return(echec("non_convergent", k))
    if (it$rang < n) return(echec(if (k == 0L) "rang_deficient" else "non_convergent", k))
    nouveau <- it$theta
    dev_n <- deviance_penalisee(nouveau, D, Ec, pond, lambda, z)
    j <- 0L
    numerique <- FALSE
    while (!is.finite(dev_n) || dev_n > dev) {
      if (j >= DEMI_PAS_MAX_LISSAGE) {
        pas <- it$theta - theta
        decrement <- sum((it$racine_m * pas)^2) + lambda * somme_differences_carrees(pas, z)
        if (is.finite(decrement) && decrement <= EPSILON_DEVIANCE_LISSAGE * abs(dev) + plancher) {
          numerique <- TRUE
          break
        }
        return(echec("non_convergent", k))
      }
      nouveau <- (theta + nouveau) / 2
      dev_n <- deviance_penalisee(nouveau, D, Ec, pond, lambda, z)
      j <- j + 1L
    }
    if (numerique) break
    k <- k + 1L
    continuer <- dev_n < (1 - EPSILON_DEVIANCE_LISSAGE) * dev
    theta <- nouveau
    dev <- dev_n
    if (!continuer) break
  }
  m <- numeric(n)
  m[pond] <- exp(theta[pond]) * Ec[pond]
  Q <- qr(rbind(diag(sqrt(m), n), A), LAPACK = FALSE)
  if (Q$rank < n) return(echec("rang_deficient", k))
  R <- qr.R(Q)
  V <- matrix(0, n, n)
  V[Q$pivot, Q$pivot] <- chol2inv(R)
  v <- diag(V)
  levier <- m * v
  regularite <- somme_differences_carrees(theta, z)
  laml <- sum(D[pond] * theta[pond] - m[pond]) -
    (lambda * regularite + 2 * sum(log(abs(diag(R)))) - ((n - z) * log(lambda) + log_det_p) - z * log(2 * pi)) / 2
  list(statut = "produite", iterations = k, theta = theta, variance = v, levier = levier,
       ddl_effectifs = sum(levier), regularite = regularite, laml = laml, deviance_penalisee = dev,
       residu_conservation_0 = sum(m - D), residu_conservation_1 = sum(x * (m - D)))
}

# Choix de lambda par la LAML d'un segment (#225 ; spécification de forme,
# § 2) : intervalle = c(a, b), 0 < a < b ; grille G = seq(ln a, ln b,
# length.out = POINTS_GRILLE_LISSAGE), lisser_poisson() à froid (depuis
# theta_0) en chaque point, les extrémités à lambda = a et b exactement (et
# non exp(ln a), exp(ln b) ; réserve R-c d'actuary-approfondi), les points
# intérieurs à exp(G[k]) ; k* premier which.max de la LAML ; si k* vaut 1
# ou N (état lambda_a_la_borne), retenu : la borne déclarée elle-même
# (grille[[k*]]), sans recherche de Brent (règle Q1 d'actuary) ; sinon
# crochet [G[k* - 1], G[k* + 1]] ; stats::optimize() de -LAML(exp(l)) dans le
# crochet, tol = TOLERANCE_BRENT_LISSAGE ; retenu : celui de G[k*] et du
# point de Brent dont la LAML est la plus grande, G[k*] en cas d'égalité.
# Un échec de lisser_poisson() (grille ou Brent) arrête la recherche du
# segment. Rend list(ajustement (valeur de lisser_poisson(), avec l'élément
# lambda), borne (NA, "inferieure" ou "superieure" : k* vaut 1 ou N)), ou
# list(echec = statut, lambda, iterations).
choisir_lambda_poisson <- function(D, Ec, z, x, intervalle, log_det_p) {
  G <- seq(log(intervalle[1L]), log(intervalle[2L]), length.out = POINTS_GRILLE_LISSAGE)
  ajuster_lambda <- function(lambda) c(lisser_poisson(D, Ec, lambda, z, x, log_det_p), list(lambda = lambda))
  ajuster <- function(l) ajuster_lambda(exp(l))
  lambda_grille <- exp(G)
  lambda_grille[c(1L, length(G))] <- intervalle
  grille <- vector("list", length(G))
  for (k in seq_along(G)) {
    grille[[k]] <- ajuster_lambda(lambda_grille[k])
    if (!identical(grille[[k]]$statut, "produite"))
      return(list(echec = grille[[k]]$statut, lambda = grille[[k]]$lambda, iterations = grille[[k]]$iterations))
  }
  k_etoile <- which.max(vapply(grille, function(r) r$laml, 0))
  # Meilleur point de la grille à une borne : lambda retenu = la borne
  # déclarée, exactement (grille[[k*]]), sans recherche de Brent (règle Q1
  # d'actuary).
  if (k_etoile == 1L || k_etoile == length(G))
    return(list(ajustement = grille[[k_etoile]], borne = if (k_etoile == 1L) "inferieure" else "superieure"))
  echec_brent <- NULL
  f <- function(l) {
    if (!is.null(echec_brent)) return(.Machine$double.xmax)
    r <- ajuster(l)
    if (!identical(r$statut, "produite")) {
      echec_brent <<- r
      return(.Machine$double.xmax)
    }
    -r$laml
  }
  crochet <- G[c(k_etoile - 1L, k_etoile + 1L)]
  l_brent <- stats::optimize(f, crochet, tol = TOLERANCE_BRENT_LISSAGE)$minimum
  brent <- if (is.null(echec_brent)) ajuster(l_brent)
  if (is.null(echec_brent) && !identical(brent$statut, "produite")) echec_brent <- brent
  if (!is.null(echec_brent))
    return(list(echec = echec_brent$statut, lambda = echec_brent$lambda, iterations = echec_brent$iterations))
  retenu <- if (brent$laml > grille[[k_etoile]]$laml) brent else grille[[k_etoile]]
  list(ajustement = retenu, borne = NA_character_)
}

# Suite de calculer_lissage() sous wh_poisson (#225), après les refus de
# cellules communs aux deux méthodes : cel, g_seg, segs, par_segment,
# segments_cites (aide de message de calculer_lissage()), D, Ec, pondere
# (E^c > 0), ponderees, deces (par segment), ev_plage, total, gr
# (grille_plage()) ; convention et paramètres de l'étape. Refus, chacun
# évalué sur tous les segments, dans l'ordre : lissage_interpolation_deces_nul
# (exactement z cellules pondérées, dont au moins une sans décès : la
# vraisemblance pénalisée interpole, ln mu tend vers -infini à la cellule
# sans décès ; avis d'actuary, Q4 de #239) ; lissage_systeme_rang_deficient
# (rang numérique perdu à la première itération, depuis theta_0, ou au
# maximum) ; lissage_non_convergent (plafond d'itérations, recherche linéaire
# épuisée sans convergence numérique, ou rang numérique perdu à une itération
# suivante). Bornes de crédibilité theta^ ± z_alpha
# sqrt(V_ii), z_alpha = qnorm((1 - niveau) / 2, lower.tail = FALSE), image
# par q = -expm1(-exp(.)). État de ligne : interpolee si E^c = 0
# (prioritaire), sinon qx_arrondi_un si qx ou une borne vaut 1, sinon
# observee. Avertissements : lambda_a_la_borne (un par segment dont le
# meilleur point de la grille est une borne de l'intervalle déclaré) ;
# qx_arrondi_un (toutes les lignes concernées, interpolées comprises). Rend
# la valeur de calculer_lissage().
lisser_segments_poisson <- function(cel, g_seg, segs, par_segment, segmentation, segments_cites, D, Ec, pondere,
                                    ponderees, deces, ev_plage, total, gr, convention, methode, plage_ages,
                                    ordre_difference, critere_lissage, niveau, parametre_lissage,
                                    intervalle_lissage) {
  z <- ordre_difference
  n_seg <- length(par_segment)
  p1 <- as.integer(plage_ages[1L]); p2 <- as.integer(plage_ages[2L])
  # Exactement z cellules pondérées, dont une sans décès : aucun maximum fini.
  interpolation_nulle <- which(ponderees == z &
                                 vapply(par_segment, function(i) any(pondere[i] & D[i] == 0), NA, USE.NAMES = FALSE))
  if (length(interpolation_nulle))
    refuser("lissage", "lissage_interpolation_deces_nul",
            formuler(paste("Lissage : %d segment(s) comptent exactement z = %d cellule(s) exposée(s) (E^c > 0), dont au",
                           "moins une sans décès, sur la plage d'âges %d à %d : %s. La vraisemblance de Poisson",
                           "pénalisée y interpole les taux bruts et ln mu tend vers -infini aux cellules sans décès :",
                           "aucun maximum fini, aucune table lissée n'est produite (élargir la plage d'âges ou réduire",
                           "l'ordre des différences)."),
                     length(interpolation_nulle), as.integer(z), p1, p2,
                     paste(paste0(segments_cites(interpolation_nulle), vapply(interpolation_nulle, function(s) {
                       i <- par_segment[[s]]
                       formuler(" (âge(s) sans décès : %s)",
                                paste(vapply(i[pondere[i] & D[i] == 0], function(j) formuler("%d", cel$age[j]), ""),
                                      collapse = ", "))
                     }, "")), collapse = " ; ")),
            list(plage_ages = plage_ages, ordre_difference = z, segments = segs[interpolation_nulle, , drop = FALSE]))

  x <- cel$age
  laml <- identical(critere_lissage, "laml")
  log_det_p <- log_det_penalite(length(par_segment[[1L]]), z)
  res <- vector("list", n_seg)
  borne <- rep(NA_character_, n_seg)
  statut_echec <- rep("", n_seg)
  lambda_echec <- rep(NA_real_, n_seg)
  for (s in seq_len(n_seg)) {
    i <- par_segment[[s]]
    if (laml) {
      r <- choisir_lambda_poisson(D[i], Ec[i], z, x[i], intervalle_lissage, log_det_p)
      if (!is.null(r$echec)) {
        statut_echec[s] <- r$echec
        lambda_echec[s] <- r$lambda
      } else {
        res[[s]] <- r$ajustement
        borne[s] <- r$borne
      }
    } else {
      r <- lisser_poisson(D[i], Ec[i], parametre_lissage, z, x[i], log_det_p)
      if (!identical(r$statut, "produite")) {
        statut_echec[s] <- r$statut
        lambda_echec[s] <- parametre_lissage
      } else {
        res[[s]] <- c(r, list(lambda = parametre_lissage))
      }
    }
  }
  deficients <- which(statut_echec == "rang_deficient")
  if (length(deficients))
    refuser("lissage", "lissage_systeme_rang_deficient",
            formuler(paste("Lissage : système des moindres carrés augmentés de l'algorithme de la vraisemblance de",
                           "Poisson pénalisée numériquement de rang déficient (tolérance de rang par défaut de qr(),",
                           "1e-7 ; z = %d) pour %d segment(s) : %s. À ces valeurs de lambda, la table lissée se",
                           "confond numériquement avec sa limite polynomiale (polynôme de degré z - 1 en ln mu, ajusté",
                           "par maximum de vraisemblance de Poisson) ; réduire parametre_lissage ou la borne",
                           "supérieure d'intervalle_lissage."),
                     as.integer(z), length(deficients), paste(vapply(deficients, function(s)
                       formuler("%s, lambda = %.17g", segments_cites(s), lambda_echec[s]), ""), collapse = " ; ")),
            list(plage_ages = plage_ages, ordre_difference = z, segments = segs[deficients, , drop = FALSE],
                 parametre_lissage = lambda_echec[deficients]))
  non_convergents <- which(statut_echec == "non_convergent")
  if (length(non_convergents))
    refuser("lissage", "lissage_non_convergent",
            formuler(paste("Lissage : l'algorithme de la vraisemblance de Poisson pénalisée ne converge pas pour %s",
                           "(plafond de %d itérations atteint, déviance pénalisée non décroissante ou non finie après",
                           "%d divisions du pas sans convergence numérique, ou rang numérique du système perdu en",
                           "cours d'itération) ; l'existence du maximum n'est pas établie pour ces données ; aucune",
                           "table lissée n'est produite."),
                     paste(vapply(non_convergents, function(s)
                       formuler("%s, lambda = %.17g", segments_cites(s), lambda_echec[s]), ""), collapse = " ; "),
                     ITERATIONS_MAX_LISSAGE, DEMI_PAS_MAX_LISSAGE),
            list(plage_ages = plage_ages, ordre_difference = z, segments = segs[non_convergents, , drop = FALSE],
                 parametre_lissage = lambda_echec[non_convergents]))

  n <- nrow(cel)
  theta <- numeric(n)
  variance <- numeric(n)
  levier <- numeric(n)
  for (s in seq_len(n_seg)) {
    i <- par_segment[[s]]
    theta[i] <- res[[s]]$theta
    variance[i] <- res[[s]]$variance
    levier[i] <- res[[s]]$levier
  }
  z_alpha <- stats::qnorm((1 - niveau) / 2, lower.tail = FALSE)
  qx <- -expm1(-exp(theta))
  borne_inf <- -expm1(-exp(theta - z_alpha * sqrt(variance)))
  borne_sup <- -expm1(-exp(theta + z_alpha * sqrt(variance)))
  un <- cbind(qx == 1, borne_inf == 1, borne_sup == 1)
  arrondi <- rowSums(un) > 0
  table_lissee <- cel[segmentation]
  rownames(table_lissee) <- NULL
  table_lissee$age <- as.integer(x)
  table_lissee$qx <- qx
  table_lissee$etat <- ETATS_TABLE_LISSEE[ifelse(!pondere, 2L, ifelse(arrondi, 3L, 1L))]
  table_lissee$levier <- levier
  table_lissee$borne_inf_credibilite <- borne_inf
  table_lissee$borne_sup_credibilite <- borne_sup

  lire <- function(nom) vapply(res, function(r) r[[nom]], 0)
  segments <- segs
  segments$cellules <- lengths(par_segment, use.names = FALSE)
  segments$cellules_ponderees <- ponderees
  segments$cellules_interpolees <- segments$cellules - ponderees
  segments$evenements <- denombrement_entier(deces, "lissage", "segments/evenements")
  segments$parametre_lissage <- lire("lambda")
  segments$ddl_effectifs <- lire("ddl_effectifs")
  segments$regularite <- lire("regularite")
  segments$laml <- lire("laml")
  segments$deviance_penalisee <- lire("deviance_penalisee")
  segments$iterations <- vapply(res, function(r) r$iterations, 0L)
  segments$residu_conservation_0 <- lire("residu_conservation_0")
  segments$residu_conservation_1 <- lire("residu_conservation_1")
  segments$etat <- ifelse(is.na(borne), "produite", "lambda_a_la_borne")

  avert <- avertissements_vides()
  inferieure <- borne %in% "inferieure"
  for (s in which(!is.na(borne)))
    avert <- avertir(avert, "lissage", "lambda_a_la_borne",
                     formuler(paste("Lissage, %s : le meilleur point de la grille de la LAML est la borne %s de",
                                    "l'intervalle déclaré (lambda = %.17g) ; lambda retenu : %.17g. Un maximum",
                                    "intérieur peut ne pas exister (%s) ; élargir l'intervalle ou déclarer lambda."),
                              segments_cites(s),
                              if (inferieure[s]) formuler("inférieure") else formuler("supérieure"),
                              intervalle_lissage[if (inferieure[s]) 1L else 2L], segments$parametre_lissage[s],
                              if (inferieure[s])
                                formuler("la LAML peut croître quand lambda tend vers 0, limite des taux bruts")
                              else
                                formuler(paste("la LAML peut croître jusqu'à la limite polynomiale, polynôme de degré",
                                               "z - 1 en ln mu, quand lambda tend vers l'infini"))),
                     1L)
  if (any(arrondi))
    avert <- avertir(avert, "lissage", "qx_arrondi_un",
                     formuler(paste("Lissage : %d ligne(s) de la table lissée où q ou une borne de crédibilité vaut 1",
                                    "par arrondi : %s. mu >= 54 ln 2 (environ 37,43) : exp(-mu), inférieur à 2^-54,",
                                    "laisse 1 - exp(-mu) arrondi à 1 en double précision (état qx_arrondi_un, sauf",
                                    "cellule interpolée, état prioritaire ; ni refus, ni plafonnement)."),
                              sum(arrondi),
                              paste(vapply(which(arrondi), function(j)
                                formuler("%sâge %d, valeur(s) égale(s) à 1 : %s",
                                         paste(vapply(segmentation, function(v)
                                           formuler("%s = « %s », ", citer(v), citer(as.character(cel[[v]][j]))), ""),
                                           collapse = ""),
                                         cel$age[j],
                                         liste_guillemets(c("qx", "borne_inf_credibilite",
                                                            "borne_sup_credibilite")[un[j, ]])), ""),
                                    collapse = " ; ")),
                     sum(arrondi))

  statuts <- STATUTS_LISSAGE_POISSON
  if (!laml) statuts <- statuts[statuts$grandeur != "segments/parametre_lissage", , drop = FALSE]
  rownames(statuts) <- NULL
  constantes <- CONSTANTES_LISSAGE_POISSON[seq_len(if (laml) 6L else 4L), , drop = FALSE]
  rownames(constantes) <- NULL
  parametres <- Filter(Negate(is.null),
                       list(methode = methode, plage_ages = plage_ages, ordre_difference = ordre_difference,
                            critere_lissage = critere_lissage, niveau = niveau, parametre_lissage = parametre_lissage,
                            intervalle_lissage = intervalle_lissage))
  parametres <- c(parametres,
                  list(convention = convention, estimateur_brut = ESTIMATEUR_LISSAGE[[methode]],
                       echelle_penalite = ECHELLE_PENALITE[[methode]],
                       regle_force = texte_moteur(REGLE_FORCE_LISSAGE),
                       hypothese = texte_moteur(HYPOTHESE_LISSAGE_POISSON),
                       reference_methode = texte_moteur(REFERENCE_METHODE_LISSAGE_POISSON),
                       constantes = texte_moteur(constantes), statuts = texte_moteur(statuts)))
  list(parametres = parametres,
       resultat = list(table_lissee = table_lissee, segments = segments),
       diagnostics = list(cellules = nrow(table_lissee), cellules_interpolees = sum(!pondere),
                          evenements_plage = denombrement_entier(ev_plage, "lissage", "evenements_plage"),
                          cellules_hors_plage = gr$hors_plage$cellules,
                          evenements_hors_plage = denombrement_entier(gr$hors_plage$evenements, "lissage",
                                                                      "evenements_hors_plage"),
                          exposition_centrale_hors_plage = gr$hors_plage$exposition,
                          evenements_total = denombrement_entier(total, "lissage", "evenements_total")),
       avertissements = avert)
}

# Fonction d'étape `lissage` (#223, méthode wh_quadratique ; #225, méthode
# wh_poisson).
#   exposition   : table de l'étape `exposition` (cellules) ;
#   brut         : resultat de l'étape `brut` (table binomial, aux lignes et
#                  dans l'ordre de la table d'exposition, lue sous
#                  wh_quadratique) ;
#   segmentation : champ du plan ; convention : convention retenue ;
#   methode, plage_ages, ordre_difference, critere_lissage, ponderation,
#   niveau, parametre_lissage, intervalle_lissage : paramètres de l'étape
#                  (NULL pour un paramètre conditionnel absent).
# Aucun contrôle de paramètre (#82) : valider_plan() et controler_entrees()
# les ont faits. Grille : grille_plage() (module exposition), sans année.
# wh_quadratique : poids : cellule binomiale `defini` : w = 1 (uniforme) ou
# E^i / moyenne de E^i sur les cellules `defini` de la plage du segment
# (exposition) ; cellule `exposition_nulle` ou sans ligne : w = 0, q^
# remplacé par 0 dans le système, état interpolee. wh_poisson : D et E^c de
# la grille ; cellule pondérée si et seulement si E^c > 0 ; lambda déclaré
# (lisser_poisson()) ou choisi par la LAML (choisir_lambda_poisson()). Refus
# en cours d'étape, chacun évalué sur tous les segments, qui sont tous
# cités, dans l'ordre : lissage_sans_segment ;
# lissage_effectif_inferieur_aux_evenements (wh_quadratique) ou
# lissage_evenement_sans_exposition (wh_poisson : E^c = 0 et D > 0) ;
# lissage_plage_bornee_sans_exposition (D11), lissage_cellules_insuffisantes,
# lissage_sans_deces ; sous wh_poisson, lissage_interpolation_deces_nul
# (exactement z cellules pondérées dont une sans décès : aucun maximum fini,
# avis d'actuary, Q4 de #239), puis lissage_systeme_rang_deficient et
# lissage_non_convergent ; sous wh_quadratique, lissage_systeme_rang_deficient
# et lissage_taux_hors_domaine (D4, sans écrêtage). Rend list(parametres,
# resultat (table_lissee, segments), diagnostics, avertissements (vide sous
# wh_quadratique ; lambda_a_la_borne et qx_arrondi_un sous wh_poisson)).
calculer_lissage <- function(exposition, brut, segmentation, convention, methode, plage_ages, ordre_difference,
                             critere_lissage, ponderation, niveau, parametre_lissage, intervalle_lissage) {
  if (!(methode %in% METHODES_LISSAGE))
    stop("calculer_lissage() : méthode non mise en oeuvre (erreur de programmation).", call. = FALSE)
  poisson <- identical(methode, "wh_poisson")
  if (!poisson) {
    tb <- brut[["binomial"]]
    if (!is.data.frame(tb) || nrow(tb) != nrow(exposition) ||
        !all(c("evenements", "exposition_initiale", "taux", "etat") %in% names(tb)))
      stop("calculer_lissage() : table binomial de l'étape brut absente ou non alignée (erreur de programmation).",
           call. = FALSE)
  }
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
  D <- cel$evenements
  par_segment <- split(seq_len(nrow(cel)), factor(g_seg, levels = seq_len(n_seg)))
  if (poisson) {
    if (!("exposition_centrale" %in% names(cel)))
      stop("calculer_lissage() : exposition centrale absente de la grille sous wh_poisson (erreur de programmation).",
           call. = FALSE)
    Ec <- cel$exposition_centrale
    # Cellules de la plage sans exposition centrale qui comptent des décès :
    # refus, sous (b) seulement (spécification de forme, § 1).
    sans_expo <- which(!(Ec > 0) & D > 0)
    if (length(sans_expo)) {
      seg_se <- unique(g_seg[sans_expo])
      refuser("lissage", "lissage_evenement_sans_exposition",
              formuler(paste("Lissage : %d cellule(s) de la plage d'âges %d à %d sans exposition centrale (E^c = 0) qui",
                             "comptent des décès : %s. La vraisemblance de Poisson y est nulle quel que soit mu (attendu",
                             "E^c mu nul, décès observés) : le lissage est refusé."),
                       length(sans_expo), p1, p2,
                       paste(paste0(segments_cites(seg_se), vapply(seg_se, function(s) {
                         i <- sans_expo[g_seg[sans_expo] == s]
                         formuler(" (%s)", paste(vapply(i, function(j) formuler("âge %d : D = %d", cel$age[j], D[j]), ""),
                                                 collapse = ", "))
                       }, "")), collapse = " ; ")),
              list(plage_ages = plage_ages, segments = segs[seg_se, , drop = FALSE], age = cel$age[sans_expo],
                   evenements = D[sans_expo]))
    }
    pondere <- Ec > 0
  } else {
    ligne <- gr$ligne
    avec_ligne <- !is.na(ligne)
    etat_b <- rep(NA_character_, nrow(cel))
    etat_b[avec_ligne] <- tb$etat[ligne[avec_ligne]]
    Ei <- numeric(nrow(cel))
    Ei[avec_ligne] <- tb$exposition_initiale[ligne[avec_ligne]]
    defini <- avec_ligne & etat_b == ETAT_BRUT_DEFINI
    defini[is.na(defini)] <- FALSE

    # Cellules de la plage hors du modèle binomial (D > E^i, D > 0 avec E^i
    # nulle comprise) : refus, sous (a) seulement.
    inferieur <- which(avec_ligne & etat_b %in% ETAT_BRUT_INFERIEUR)
    if (length(inferieur)) {
      seg_inf <- unique(g_seg[inferieur])
      refuser("lissage", "lissage_effectif_inferieur_aux_evenements",
              formuler(paste("Lissage : %d cellule(s) de la plage d'âges %d à %d dont l'exposition initiale est",
                             "inférieure au nombre de décès (état effectif_inferieur_aux_evenements de l'estimateur",
                             "binomial : q^ = D / E^i n'est pas défini) : %s. Le lissage quadratique est refusé."),
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
    for (s in seq_len(n_seg)) {
      i <- par_segment[[s]]
      d <- i[defini[i]]
      w[d] <- switch(ponderation,
        uniforme = 1,
        exposition = Ei[d] / mean(Ei[d]),
        stop("calculer_lissage() : pondération inconnue (erreur de programmation).", call. = FALSE))
    }
    pondere <- w > 0
  }

  # Plage bornée par une cellule sans exposition (D11).
  bornee <- which(vapply(par_segment, function(i) !pondere[i[1L]] || !pondere[i[length(i)]], NA, USE.NAMES = FALSE))
  if (length(bornee))
    refuser("lissage", "lissage_plage_bornee_sans_exposition",
            formuler(paste("Lissage : %d segment(s) dont la première ou la dernière cellule de la plage d'âges %d à %d",
                           "n'a aucune exposition (poids nul) : %s. Le lissage n'extrapole pas (décision D11) :",
                           "réduire la plage d'âges aux âges observés ; l'extrapolation relève de la fermeture."),
                     length(bornee), p1, p2,
                     paste(paste0(segments_cites(bornee), vapply(bornee, function(s) {
                       i <- par_segment[[s]]
                       a <- unique(c(i[1L], i[length(i)]))
                       formuler(" (âge(s) %s)", paste(vapply(a[!pondere[a]], function(j) formuler("%d", cel$age[j]), ""),
                                                      collapse = ", "))
                     }, "")), collapse = " ; ")),
            list(plage_ages = plage_ages, segments = segs[bornee, , drop = FALSE]))
  # Moins de z cellules de poids non nul (Lignes directrices, chap. 3, IV.2.d,
  # p. 33 ; sous wh_poisson, W + P singulière : aucun maximum unique).
  ponderees <- vapply(par_segment, function(i) sum(pondere[i]), 0L, USE.NAMES = FALSE)
  insuffisants <- which(ponderees < z)
  if (length(insuffisants))
    refuser("lissage", "lissage_cellules_insuffisantes",
            formuler(paste("Lissage : %d segment(s) comptent moins de z = %d cellule(s) de poids non nul sur la plage",
                           "d'âges %d à %d : %s. %s"),
                     length(insuffisants), as.integer(z), p1, p2,
                     paste(paste0(segments_cites(insuffisants), vapply(insuffisants, function(s)
                       formuler(" (%d cellule(s) de poids non nul)", ponderees[s]), "")), collapse = " ; "),
                     if (poisson)
                       formuler(paste("La vraisemblance de Poisson pénalisée n'a pas de maximum unique (au moins z",
                                      "cellules exposées requises : la pénalité ne contraint pas les polynômes de degré",
                                      "z - 1)."))
                     else formuler(paste("Le minimum de M = F + h S n'est pas unique (Lignes directrices, chap. 3,",
                                         "IV.2.d, p. 33 : au moins z points pivots, condition donnée comme",
                                         "suffisante, ici aussi nécessaire)."))),
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

  x <- cel$age
  ev_plage <- sum(deces)
  total <- sum(as.numeric(exposition$evenements))
  if (!identical(ev_plage + gr$hors_plage$evenements, total))
    stop("calculer_lissage() : décomptes d'événements incohérents (erreur de programmation).", call. = FALSE)
  dans <- exposition$age >= plage_ages[1L] & exposition$age <= plage_ages[2L]
  if (poisson)
    return(lisser_segments_poisson(cel, g_seg, segs, par_segment, segmentation, segments_cites, D, Ec, pondere,
                                   ponderees, deces, ev_plage, total, gr, convention, methode, plage_ages,
                                   ordre_difference, critere_lissage, niveau, parametre_lissage, intervalle_lissage))

  # Lissage, segment par segment.
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
  # le rangement en integer (#199 ; ADR 0009, point 9.6, (iii)), ci-dessus.
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
