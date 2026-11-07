# -----------------------------------------------------------------------------
#  R/positionnement.R : module `positionnement`, briques d'appariement à une
#  table de référence et de SMR partagées par les étapes `positionnement` et
#  `validation` (issue #167 ; avis de forme (0'') d'architect-approfondi, F1 :
#  déplacement depuis R/validation.R ; ADR 0007, point 2.1 : `positionnement`
#  précède `validation`), et étape `positionnement`, méthode 1 (SMR ; #167,
#  spécification d'actuary-approfondi décidée, M288, M289, M293 ; avis F2,
#  F4, F7)
#
#  - CONVENTIONS_VALIDATION : conventions d'âge admises ;
#  - valeurs_entieres_double() : forme des paramètres à valeur entière ;
#  - controler_cle_table(), controler_plage_ages() : forme de la clé de table
#    et de la plage d'âges, sous le module de l'étape appelante (F2) ;
#  - controler_entrees_validation() : convention d'âge retenue, table sans
#    année, appariement par sexe, couverture de la plage ; appelée avant
#    exécution par controler_entrees() (construire_table(), #82), une fois,
#    rejeu compris, pour chaque étape qui lit une table ;
#  - apparier_table(), attendus_cellules() : grille des cellules de la plage,
#    q~ apparié par clé exacte (sexe, âge), A = E^c mu~, mu~ = -log1p(-q~),
#    état de chaque cellule ;
#  - smr_poisson() : SMR et inférence de Poisson exacte et asymptotique ;
#  - controler_parametres_positionnement() : paramètres de l'étape ; appelée
#    par valider_plan() seule (#82) ;
#  - calculer_positionnement() : fonction d'étape `positionnement`.
#
#  Notations : celles de R/validation.R (M217) ; theta = O / A par segment.
#  Les refus des briques partagées prennent le module de l'étape appelante
#  (`etape`, positionnement ou validation) et le préfixe de son message ; les
#  refus de `validation` sont inchangés en motif, module, details et message
#  (F2). Aucun verdict, aucun seuil ; formules sans attribution d'auteur
#  (M200).
#  Ce module n'appelle que `outils` (refuser(), formuler(), forme_exacte(),
#  chaine_unique(), avertir(), texte_moteur()) et `donnees` (citer(),
#  liste_guillemets(), SEXES_TABLE) (ADR 0007, point 2.1). Les arêtes vers
#  donnees::table_reference éventuellement relevées par graphe_modules.R sont
#  des homonymies de l'argument `table_reference` (clé de la table).
# -----------------------------------------------------------------------------

# Conventions d'âge admises (M255, point 3) : cellule [x, x+1).
CONVENTIONS_VALIDATION <- "dernier_anniversaire"

# Entier unique de type double, fini, à valeur entière (paramètres à valeur
# entière, M246, M257 : un integer est refusé sous le motif du paramètre).
valeurs_entieres_double <- function(v) forme_exacte(v, "double") && !anyNA(v) && all(is.finite(v)) && all(v == trunc(v))

# Étapes qui lisent une table de référence par les briques de ce module
# (F2) : chacune donne aux refus son module et le préfixe de leur message.
ETAPES_TABLE <- c("positionnement", "validation")

# Forme de la clé de table (cle_table_invalide) et de la plage d'âges
# (plage_ages_invalide), sous le module de l'étape `etape` (F2 ; extraites
# de controler_parametres_validation(), messages de `validation` inchangés).
# Rendent TRUE, invisible.
controler_cle_table <- function(table_reference, etape) {
  if (!(etape %in% ETAPES_TABLE)) stop("controler_cle_table() : étape inconnue (erreur de programmation).", call. = FALSE)
  if (!chaine_unique(table_reference))
    refuser(etape, "cle_table_invalide",
            if (identical(etape, "validation"))
              formuler(paste("La clé de la table de référence (table_reference) doit être une chaîne unique, non vide,",
                             "sans attribut : le nom, dans l'argument « tables_reference », de la table à valider."))
            else
              formuler(paste("La clé de la table de référence (table_reference) doit être une chaîne unique, non vide,",
                             "sans attribut : le nom, dans l'argument « tables_reference », de la table sur laquelle",
                             "positionner.")),
            list(table_reference = table_reference))
  invisible(TRUE)
}
controler_plage_ages <- function(plage_ages, etape) {
  # Admise aussi pour l'étape `homogeneite` (#168, module validation), qui ne
  # lit aucune table : même forme de plage_ages que `validation`.
  if (!(etape %in% c(ETAPES_TABLE, "homogeneite")))
    stop("controler_plage_ages() : étape inconnue (erreur de programmation).", call. = FALSE)
  if (!valeurs_entieres_double(plage_ages) || length(plage_ages) != 2L || plage_ages[1L] < 0 ||
      plage_ages[1L] > plage_ages[2L] || plage_ages[2L] > .Machine$integer.max)
    refuser(etape, "plage_ages_invalide",
            formuler(paste("La plage d'âges (plage_ages) doit être un vecteur double sans attribut de deux valeurs",
                           "entières finies, 0 <= min <= max (un integer est refusé).")),
            list(plage_ages = plage_ages))
  invisible(TRUE)
}

# Contrôles d'entrée avant exécution (avis (0''), M253 ; ADR 0015, point 7.3),
# dans l'ordre : convention d'âge retenue (convention_validation_non_admise,
# ou convention_positionnement_non_admise) ; table sans annee
# (table_annee_non_admise) ; modalités de sexe de la table M(T) mêlant
# ensemble et H ou F (table_sexes_ambigus) ; table par sexe sans sexe dans la
# segmentation (table_sexe_incompatible) ; chaque sexe de la table couvre
# plage_ages (plage_ages_hors_table ; les âges d'un sexe sont contigus,
# contrôlé par table_reference()). `table` : objet table_reference contrôlé
# (controler_tables_reference()) ; `convention` : convention retenue ;
# `cle` : clé citée par le plan ; `etape` : étape qui lit la table (module
# et préfixe des refus, F2). Rend TRUE, invisible.
controler_entrees_validation <- function(table, cle, convention, segmentation, plage_ages, etape) {
  if (!(etape %in% ETAPES_TABLE))
    stop("controler_entrees_validation() : étape inconnue (erreur de programmation).", call. = FALSE)
  prefixe <- if (identical(etape, "validation")) "Validation" else "Positionnement"
  if (!(convention %in% CONVENTIONS_VALIDATION)) {
    if (identical(etape, "validation"))
      refuser("validation", "convention_validation_non_admise",
              formuler(paste("Validation : convention d'âge retenue « %s » non admise (admise : %s). Sous",
                             "age_atteint_annee, l'appariement d'une cellule d'année civile à la table n'est pas",
                             "spécifié : la validation est refusée (décision Q7 (a) de #4)."),
                       citer(convention), liste_guillemets(CONVENTIONS_VALIDATION)),
              list(convention = convention, attendu = CONVENTIONS_VALIDATION))
    refuser("positionnement", "convention_positionnement_non_admise",
            formuler(paste("Positionnement : convention d'âge retenue « %s » non admise (admise : %s). Sous",
                           "age_atteint_annee, l'appariement d'une cellule d'année civile à la table n'est pas",
                           "spécifié : le positionnement est refusé (même motif que la décision Q7 (a) de #4)."),
                     citer(convention), liste_guillemets(CONVENTIONS_VALIDATION)),
            list(convention = convention, attendu = CONVENTIONS_VALIDATION))
  }
  if ("annee" %in% names(table))
    refuser(etape, "table_annee_non_admise",
            if (identical(etape, "validation"))
              formuler(paste("Validation : la table de clé « %s » porte une colonne annee ; cette version ne valide que",
                             "des tables sans année (ADR 0015, point 7.4)."), citer(cle))
            else
              formuler(paste("Positionnement : la table de clé « %s » porte une colonne annee ; cette version ne",
                             "positionne que sur des tables sans année (ADR 0015, point 7.4)."), citer(cle)),
            list(cle = cle))
  sexes <- unique(table$sexe)
  if ("ensemble" %in% sexes && length(sexes) > 1L)
    refuser(etape, "table_sexes_ambigus",
            formuler(paste("%s : la table de clé « %s » mêle la modalité « ensemble » et %s : l'appariement",
                           "par sexe serait ambigu, il est refusé."),
                     prefixe, citer(cle), liste_guillemets(setdiff(sexes, "ensemble"))),
            list(cle = cle, sexes = sexes))
  if (!("sexe" %in% segmentation) && !("ensemble" %in% sexes))
    refuser(etape, "table_sexe_incompatible",
            formuler(paste("%s : la table de clé « %s » est par sexe (%s) et la construction n'est pas",
                           "segmentée par sexe : aucune valeur de la table ne s'applique sans déduction ; segmenter",
                           "par sexe, ou fournir une table « ensemble »."),
                     prefixe, citer(cle), liste_guillemets(sexes)),
            list(cle = cle, sexes = sexes))
  for (s in SEXES_TABLE[SEXES_TABLE %in% sexes]) {
    a <- table$age[table$sexe == s]
    if (min(a) > plage_ages[1L] || max(a) < plage_ages[2L])
      refuser(etape, "plage_ages_hors_table",
              formuler(paste("%s : la table de clé « %s » (sexe %s) couvre les âges %d à %d, qui ne",
                             "contiennent pas la plage d'âges %d à %d."),
                       prefixe, citer(cle), citer(s), min(a), max(a), as.integer(plage_ages[1L]),
                       as.integer(plage_ages[2L])),
              list(cle = cle, sexe = s, ages_table = range(a), plage_ages = plage_ages))
  }
  invisible(TRUE)
}

# Grille des cellules de la plage et q~ apparié (M253, points 2 et 5).
#   exposition : table de l'étape `exposition` ; segmentation, par_annee ;
#   table : table_reference contrôlée par controler_entrees_validation() ;
#   plage_ages ; cle : clé citée (messages) ; etape : étape appelante (module
#   et préfixe du refus, F2).
# Grille : segments présents dans la table d'exposition (dans son ordre) x
# âges de la plage (x années de la table d'exposition si par_annee), E^c = 0
# et D = 0 pour une cellule sans ligne d'exposition ; aucun segment absent
# n'est créé. Appariement sur la clé exacte (sexe, âge), sans déduction : la
# modalité ensemble s'applique à chaque cellule ; sinon le sexe de la cellule.
# Refus en cours d'étape : cellule_sans_valeur_table (sexe absent de la
# table) ; avec refuser_sans_valeur = FALSE (comparateur 2 de l'étape
# importance_exclusions, #69, sans refus, appelée sous l'étape validation),
# q~ NA pour ces cellules. Rend
# list(cellules (clés, age [, annee], exposition_centrale, evenements,
# qx_table), segment (indice), hors_plage (cellules, evenements,
# exposition), appariement_sexe).
apparier_table <- function(exposition, segmentation, par_annee, table, plage_ages, cle, etape,
                           refuser_sans_valeur = TRUE) {
  if (!(etape %in% ETAPES_TABLE)) stop("apparier_table() : étape inconnue (erreur de programmation).", call. = FALSE)
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
  cellules$exposition_centrale <- Ec
  cellules$evenements <- D
  sexes <- unique(table$sexe)
  ensemble <- identical(sexes, "ensemble")
  sexe_cle <- if (ensemble || !("sexe" %in% segmentation)) rep("ensemble", n) else as.character(cellules$sexe)
  k <- match(paste(sexe_cle, cellules$age, sep = "\r"), paste(table$sexe, table$age, sep = "\r"))
  if (anyNA(k) && refuser_sans_valeur) {
    manquants <- unique(sexe_cle[is.na(k)])
    refuser(etape, "cellule_sans_valeur_table",
            formuler(paste("%s : %d cellule(s) de la plage sans valeur dans la table de clé « %s » (sexe(s) %s",
                           "de la construction absent(s) de la table) ; l'appariement se fait sur la clé exacte, sans",
                           "déduction."),
                     if (identical(etape, "validation")) "Validation" else "Positionnement", sum(is.na(k)), citer(cle),
                     liste_guillemets(manquants)),
            list(cle = cle, sexes = manquants, cellules = sum(is.na(k))))
  }
  cellules$qx_table <- table$qx[k]
  list(cellules = cellules, segment = g,
       hors_plage = list(cellules = sum(!dans), evenements = sum(as.numeric(exposition$evenements[!dans])),
                         exposition = sum(exposition$exposition_centrale[!dans])),
       appariement_sexe = if (!("sexe" %in% segmentation)) "sans_sexe"
                          else if (ensemble) "ensemble_par_sexe" else "par_sexe")
}

# Attendus et états (M253, point 1) : mu~ = -log1p(-q~) ; A = E^c mu~, A = 0
# si E^c = 0 (pas de 0 x Inf) ; refus taux_table_egal_un si q~ = 1 et E^c > 0,
# sous le module de l'étape appelante `etape` (F2).
# Rend list(taux_table, attendus, etat).
attendus_cellules <- function(Ec, D, qx, cle, etape) {
  if (!(etape %in% ETAPES_TABLE)) stop("attendus_cellules() : étape inconnue (erreur de programmation).", call. = FALSE)
  un <- qx == 1 & Ec > 0
  if (any(un))
    refuser(etape, "taux_table_egal_un",
            formuler(paste("%s : %d cellule(s) exposée(s) où la table de clé « %s » donne q_x = 1 : l'attendu",
                           "A = E^c (-ln(1 - q_x)) est infini, le modèle de Poisson ne s'applique pas."),
                     if (identical(etape, "validation")) "Validation" else "Positionnement", sum(un), citer(cle)),
            list(cle = cle, cellules = sum(un)))
  mu <- -log1p(-qx)
  A <- numeric(length(Ec))
  e <- Ec > 0
  A[e] <- Ec[e] * mu[e]
  etat <- rep("retenue", length(Ec))
  etat[!e & D == 0L] <- "sans_exposition"
  etat[!e & D > 0L] <- "evenement_sans_exposition"
  etat[e & !(A > 0) & D == 0L] <- "taux_table_nul"
  etat[e & !(A > 0) & D > 0L] <- "evenement_sous_taux_nul"
  list(taux_table = mu, attendus = A, etat = etat)
}

# SMR et inférence de Poisson exacte (S4, S5), vectorisé sur (O, A), A > 0 :
# smr, bornes exactes, p inférieure P(X <= O), supérieure P(X >= O),
# centrale, z et p de la racine cubique corrigée (O >= A : queue supérieure ;
# O < A : queue inférieure, O* = O + 1).
smr_poisson <- function(O, A, niveau) {
  alpha <- 1 - niveau
  inf <- rep(0, length(O))
  k <- O > 0
  inf[k] <- stats::qgamma(alpha / 2, O[k]) / A[k]
  sup <- stats::qgamma(alpha / 2, O + 1, lower.tail = FALSE) / A
  p_inf <- stats::ppois(O, A)
  p_sup <- stats::ppois(O - 1, A, lower.tail = FALSE)
  haut <- O >= A
  z <- numeric(length(O))
  z[haut] <- 3 * sqrt(O[haut]) * (1 - 1 / (9 * O[haut]) - (A[haut] / O[haut])^(1 / 3))
  Os <- O[!haut] + 1
  z[!haut] <- 3 * sqrt(Os) * ((A[!haut] / Os)^(1 / 3) + 1 / (9 * Os) - 1)
  list(smr = O / A, borne_inf = inf, borne_sup = sup, p_inferieure = p_inf, p_superieure = p_sup,
       p_centrale = pmin(1, 2 * pmin(p_inf, p_sup)), z = z, p_z = stats::pnorm(z, lower.tail = FALSE))
}

# --- étape `positionnement` (#167) ----------------------------------------------
#
# Spécification d'actuary-approfondi décidée (#167, commentaire 6705863583 ;
# M288 : échelle de la force ; M289 : Q2 à Q7 ; M293 : paramètre methode) ;
# avis de forme (0'') (commentaire 6706003047 ; M292 : F2, F4, F7, F8, F9).
# Modèle, par segment s : D_c ~ Poisson(theta_s A_c) indépendants sur les
# cellules retenues C+_s de la plage, A_c = E^c_c mu~_c (attendus_cellules(),
# comme `validation`, M217). theta_s = O_s / A_s ; table positionnée
# q_x = 1 - (1 - q~_x)^theta_s (force de la référence multipliée par theta_s,
# M288), aux seuls âges de la plage (M289, Q3) ; q < 1 pour tout q~ < 1 et
# theta fini en arithmétique exacte ; en double précision, arrondi à 1 si
# theta mu~ >= 54 ln 2, état qx_arrondi_un. Aucun verdict, aucun seuil, aucune p-valeur (le test theta = 1
# est rendu par l'étape validation).

# Méthodes de positionnement admises (E2.12 ; M293 : valeur unique en v1,
# paramètre requis sans défaut).
METHODES_POSITIONNEMENT <- "smr"
# Nombre de paramètres estimés par segment positionné (F5, F7).
PARAMETRES_ESTIMES_POSITIONNEMENT <- 1L
# États d'une ligne de la table positionnée (§ 4 de la spécification) :
# produite ; taux_table_nul (q~ = 0, image exacte q = 0) ; taux_table_egal_un
# (q~ = 1 sur une cellule sans exposition, image exacte q = 1 ; avec
# exposition, refus taux_table_egal_un) ; qx_arrondi_un (q~ < 1 et au moins
# une des cinq valeurs, qx ou une borne, vaut 1 par arrondi en double
# précision : theta mu~ >= 54 ln 2, environ 37,43 ; ni refus ni plafonnement,
# avis d'actuary du 07/11/2026). taux_table_nul et taux_table_egal_un
# l'emportent. « produite » est aussi la seule valeur de smr$etat sous M289
# (O = 0 et A = 0 refusés).
ETATS_TABLE_POSITIONNEE <- c("produite", "taux_table_nul", "taux_table_egal_un", "qx_arrondi_un")
# États d'une cellule, rendus par attendus_cellules() (M253, point 1 ; mêmes
# valeurs, dans le même ordre, que ETATS_CELLULE_VALIDATION, module
# validation, que ce module ne lit pas : graphe d'appels).
ETATS_CELLULE_POSITIONNEMENT <- c("retenue", "sans_exposition", "taux_table_nul", "evenement_sans_exposition",
                                  "evenement_sous_taux_nul")
# Identité de la note de méthode (M244 ; spécification, § 7), dans
# etapes/positionnement/parametres/reference_methode.
REFERENCE_METHODE_POSITIONNEMENT <- texte_moteur(paste(
  "note II1291-12 v1.7, « Méthodes de positionnement : aspects méthodologiques », fichier II1291-12_v1.7.pdf",
  "(jeu ISFA), SHA-256 93537ddb13bf3c989f186b3d7676976e8759d5b6ca106910c8ab66cd0e4e6dce, § 3.1, p. 3/13 ;",
  "référence de place, écarts documentés"))

# Statuts (E2.20) des grandeurs de resultat$smr et resultat$table_positionnee
# (spécification, § 5), une ligne par (table, grandeur), dans l'ordre des
# colonnes ; chaque statut qui porte sur une loi se termine par la mention
# d'indépendance des unités (M278, #159). Formules sans attribution (M200).
STATUTS_POSITIONNEMENT <- local({
  indep <- paste("unités d'observation indépendantes ; sous l'unité contrat, les décès sont sur-dispersés et",
                 "l'intervalle est trop étroit")
  exact <- paste("exact sous la loi supposée (O ~ Poisson(theta A), A fixe), couverture >= niveau (intervalle exact",
                 "conservatif) ; asymptotique sous le modèle de survie ; qgamma, approximation numérique ;")
  asym <- paste("asymptotique (O -> infini) : ln theta^ de loi normale, de variance 1/O (inverse de l'information de",
                "Fisher de ln theta, évaluée en theta^), z = Phi^-1(1 - alpha/2) ; exp, approximation numérique ;")
  image <- paste("par q = 1 - (1 - q~)^theta, fonction croissante de theta (transformation exacte) ; en double",
                 "précision, l'image vaut 1 dès que theta mu~ >= 54 ln 2 (environ 37,43) : état qx_arrondi_un, ordre des",
                 "bornes alors large ; même statut que l'intervalle de theta dont elle est l'image ; couverture simultanée de la table positionnée vraie (force theta mu~) à tous les âges",
                 "de la plage, sous l'hypothèse de forme ; ne couvre pas l'écart de forme ; log1p, expm1, approximation",
                 "numérique ;")
  l <- list(
    c("smr", "observes", "descriptif : O = somme de D sur les cellules retenues du segment (C+_s)"),
    c("smr", "attendus",
      "descriptif : A = somme sur C+_s de E^c mu~, mu~ = -ln(1 - q~) (arithmétique flottante ; log1p)"),
    c("smr", "smr",
      paste("résultat exact : theta^ = O / A, maximum de vraisemblance de theta dans le modèle D_c ~ Poisson(theta",
            "A_c) indépendants ; vraisemblance de même forme sous le modèle de survie à force constante par",
            "cellule")),
    c("smr", "borne_inf_exacte", paste(exact, "G_O^-1(alpha/2) / A, G_k loi gamma de forme k ;", indep)),
    c("smr", "borne_sup_exacte", paste(exact, "G_(O+1)^-1(1 - alpha/2) / A ;", indep)),
    c("smr", "borne_inf_asymptotique", paste(asym, "theta^ exp(-z / sqrt(O)) ;", indep)),
    c("smr", "borne_sup_asymptotique", paste(asym, "theta^ exp(z / sqrt(O)) ;", indep)),
    c("table_positionnee", "qx_reference", "recopie de q~, valeur de la table de référence à l'âge x"),
    c("table_positionnee", "qx",
      paste("estimation : 1 - (1 - q~)^theta^, force de la référence multipliée par theta^, constante sur",
            "[x, x+1) ; -expm1(theta^ log1p(-q~)), approximation numérique ; vaut 1 par arrondi si theta^ mu~ >=",
            "54 ln 2 (état qx_arrondi_un)")),
    c("table_positionnee", "borne_inf_exacte", paste("image de la borne inférieure exacte de theta", image, indep)),
    c("table_positionnee", "borne_sup_exacte", paste("image de la borne supérieure exacte de theta", image, indep)),
    c("table_positionnee", "borne_inf_asymptotique",
      paste("image de la borne inférieure asymptotique de theta", image, indep)),
    c("table_positionnee", "borne_sup_asymptotique",
      paste("image de la borne supérieure asymptotique de theta", image, indep)))
  texte_moteur(data.frame(table = vapply(l, `[`, "", 1L), grandeur = vapply(l, `[`, "", 2L),
                          statut = vapply(l, `[`, "", 3L), stringsAsFactors = FALSE))
})

# Contrôle les paramètres propres à l'étape `positionnement` (valeurs ; les
# noms sont contrôlés par valider_plan()), dans l'ordre du catalogue : methode
# (methode_invalide), table_reference (cle_table_invalide), plage_ages
# (plage_ages_invalide), niveau (niveau_invalide, même code que `brut`).
# Forme exacte (#23) ; un integer reçu pour plage_ages est refusé (M257).
# Appelée par valider_plan() seule (#82). Rend TRUE, invisible.
controler_parametres_positionnement <- function(methode, table_reference, plage_ages, niveau) {
  if (!chaine_unique(methode) || !(methode %in% METHODES_POSITIONNEMENT))
    refuser("positionnement", "methode_invalide",
            formuler(paste("La méthode de positionnement (methode) doit être une chaîne unique, sans attribut, prise",
                           "dans %s, sans défaut (E2.12 : méthode 1, SMR, seule mise en oeuvre)."),
                     liste_guillemets(METHODES_POSITIONNEMENT)),
            list(methode = methode))
  controler_cle_table(table_reference, "positionnement")
  controler_plage_ages(plage_ages, "positionnement")
  if (!forme_exacte(niveau, "double") || length(niveau) != 1L || !is.finite(niveau) || niveau <= 0 || niveau >= 1)
    refuser("positionnement", "niveau_invalide",
            formuler(paste("Le niveau de confiance doit être un nombre (double) unique, sans attribut, fini,",
                           "strictement compris entre 0 et 1.")),
            list(niveau = niveau))
  invisible(TRUE)
}

# Fonction d'étape `positionnement` (#167), méthode `smr`.
#   exposition       : table de l'étape `exposition` (cellules) ;
#   segmentation, par_annee, convention : segmentation du plan, par_annee et
#                      convention retenue de l'étape `exposition` ;
#   table            : objet table_reference de la clé citée (argument
#                      tables_reference de construire_table()) ;
#   methode, table_reference, plage_ages, niveau : paramètres de l'étape.
# Aucun contrôle de paramètre (#82) : construire_table() a validé le plan
# (valider_plan()), la table (contrôle 1.3) et les entrées
# (controler_entrees_validation(), étape positionnement) une fois, dans
# controler_entrees(). Refus en cours d'étape, dans l'ordre :
# cellule_sans_valeur_table, taux_table_egal_un (apparier_table(),
# attendus_cellules()) ; positionnement_sans_attendu (A_s = 0 : aucune
# cellule retenue), puis positionnement_sans_deces (O_s = 0 : theta = 0
# donnerait une table à mortalité nulle) (M289, Q2), chacun citant tous les
# segments en défaut. Rend list(parametres, resultat (cellules, smr,
# table_positionnee), diagnostics, avertissements). Aucune copie de la table
# de référence (ADR 0015, point 5) : table_positionnee est un data.frame nu
# (ADR 0015, point 5.2 ; F7), une ligne par segment x âge de la plage, même
# sous par_annee (theta mu~ ne dépend pas de l'année).
calculer_positionnement <- function(exposition, segmentation, par_annee, convention, table, table_reference, methode,
                                    plage_ages, niveau) {
  manquantes <- setdiff(c(segmentation, "age", if (isTRUE(par_annee)) "annee", "evenements", "exposition_centrale"),
                        names(exposition))
  if (length(manquantes))
    stop(sprintf("calculer_positionnement() : colonne(s) %s absente(s) de la table d'exposition (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  if (!identical(methode, "smr"))
    stop("calculer_positionnement() : méthode non mise en oeuvre (erreur de programmation).", call. = FALSE)
  ap <- apparier_table(exposition, segmentation, par_annee, table, plage_ages, table_reference, "positionnement")
  cel <- ap$cellules
  at <- attendus_cellules(cel$exposition_centrale, cel$evenements, cel$qx_table, table_reference, "positionnement")
  cel$taux_table <- at$taux_table
  cel$attendus <- at$attendus
  cel$etat <- at$etat
  ret <- at$etat == "retenue"
  g <- ap$segment
  n_seg <- if (length(g)) max(g) else 0L
  # O_s et A_s : sommes sur C+_s, dans l'ordre de la grille (âge, année).
  par_segment <- split(which(ret), factor(g[ret], levels = seq_len(n_seg)))
  O <- vapply(par_segment, function(i) sum(as.numeric(cel$evenements[i])), 0, USE.NAMES = FALSE)
  A <- vapply(par_segment, function(i) sum(at$attendus[i]), 0, USE.NAMES = FALSE)
  segs <- cel[!duplicated(g), segmentation, drop = FALSE]
  rownames(segs) <- NULL
  sans_a <- which(!(A > 0))
  sans_o <- which(O == 0)
  # Segments cités dans un refus : clés et attendus (texte formé, une
  # expression par refus : règle 2 de tests/controles/messages_refus.R).
  # Aucune cellule dans la table d'exposition (période sans observation) :
  # aucun segment (avis d'actuary du 07/11/2026).
  if (!n_seg)
    refuser("positionnement", "positionnement_sans_attendu",
            formuler(paste("Positionnement : la table d'exposition ne compte aucune cellule (aucune exposition sur la",
                           "période d'observation) : aucun segment à positionner sur la plage d'âges %d à %d. Le",
                           "facteur theta = O / A n'est pas défini : aucune table positionnée n'est produite."),
                     as.integer(plage_ages[1L]), as.integer(plage_ages[2L])),
            list(cle = table_reference, plage_ages = plage_ages, segments = segs, attendus = numeric(0)))
  if (length(sans_a))
    refuser("positionnement", "positionnement_sans_attendu",
            formuler(paste("Positionnement : %d segment(s) sans cellule retenue sur la plage d'âges %d à %d (aucune",
                           "cellule exposée où la table de clé « %s » donne q_x > 0 : attendus A = 0) : %s. Le facteur",
                           "theta = O / A n'est pas défini : aucune table positionnée n'est produite."),
                     length(sans_a), as.integer(plage_ages[1L]), as.integer(plage_ages[2L]), citer(table_reference),
                     paste(vapply(sans_a, function(i)
                       formuler("%s (A = %.17g)",
                                if (length(segmentation)) paste(vapply(segmentation, function(v)
                                  formuler("%s = « %s »", citer(v), citer(as.character(segs[[v]][i]))), ""),
                                  collapse = ", ")
                                else formuler("sans segmentation"), A[i]), ""), collapse = " ; ")),
            list(cle = table_reference, plage_ages = plage_ages, segments = segs[sans_a, , drop = FALSE],
                 attendus = A[sans_a]))
  if (length(sans_o))
    refuser("positionnement", "positionnement_sans_deces",
            formuler(paste("Positionnement : %d segment(s) sans décès observé sur les cellules retenues de la plage",
                           "d'âges %d à %d : %s. theta = O / A = 0 donnerait une table à mortalité nulle : le",
                           "positionnement est refusé. L'étape validation (relation independante) donne, sur ces",
                           "données, le SMR et sa borne supérieure exacte -ln(alpha/2) / A."),
                     length(sans_o), as.integer(plage_ages[1L]), as.integer(plage_ages[2L]),
                     paste(vapply(sans_o, function(i)
                       formuler("%s (A = %.17g)",
                                if (length(segmentation)) paste(vapply(segmentation, function(v)
                                  formuler("%s = « %s »", citer(v), citer(as.character(segs[[v]][i]))), ""),
                                  collapse = ", ")
                                else formuler("sans segmentation"), A[i]), ""), collapse = " ; ")),
            list(cle = table_reference, plage_ages = plage_ages, segments = segs[sans_o, , drop = FALSE],
                 attendus = A[sans_o]))

  # theta^, bornes exactes (smr_poisson()) et asymptotiques (log theta^
  # normal de variance 1/O ; O > 0 ici, même forme que taux_poisson()).
  sm <- smr_poisson(O, A, niveau)
  z <- stats::qnorm((1 - niveau) / 2, lower.tail = FALSE)
  inf_a <- sm$smr * exp(-z / sqrt(O))
  sup_a <- sm$smr * exp(z / sqrt(O))
  smr <- segs
  smr$cellules_retenues <- lengths(par_segment, use.names = FALSE)
  smr$observes <- O
  smr$attendus <- A
  smr$smr <- sm$smr
  smr$borne_inf_exacte <- sm$borne_inf
  smr$borne_sup_exacte <- sm$borne_sup
  smr$borne_inf_asymptotique <- inf_a
  smr$borne_sup_asymptotique <- sup_a
  smr$etat <- rep("produite", n_seg)

  # Table positionnée : une ligne par segment x âge (première année de la
  # grille sous par_annee : q~ ne dépend pas de l'année).
  k <- !duplicated(cbind(g, cel$age))
  q <- cel$qx_table[k]
  gk <- g[k]
  tp <- cel[k, segmentation, drop = FALSE]
  rownames(tp) <- NULL
  tp$age <- cel$age[k]
  tp$qx_reference <- q
  lq <- log1p(-q)
  tp$qx <- -expm1(sm$smr[gk] * lq)
  tp$borne_inf_exacte <- -expm1(sm$borne_inf[gk] * lq)
  tp$borne_sup_exacte <- -expm1(sm$borne_sup[gk] * lq)
  tp$borne_inf_asymptotique <- -expm1(inf_a[gk] * lq)
  tp$borne_sup_asymptotique <- -expm1(sup_a[gk] * lq)
  # Valeurs arrondies à 1 (theta mu~ >= 54 ln 2) : état qx_arrondi_un, si
  # q~ < 1 (q~ = 0 donne 0 ; q~ = 1, état taux_table_egal_un).
  colonnes_q <- c("qx", "borne_inf_exacte", "borne_sup_exacte", "borne_inf_asymptotique", "borne_sup_asymptotique")
  un <- as.matrix(tp[colonnes_q]) == 1
  arrondi <- q > 0 & q < 1 & rowSums(un) > 0
  tp$etat <- ETATS_TABLE_POSITIONNEE[ifelse(q == 0, 2L, ifelse(q == 1, 3L, ifelse(arrondi, 4L, 1L)))]

  # Décomptes et identité des événements (comme `validation`, M253, point 5).
  sans_attendu <- at$etat %in% c("evenement_sans_exposition", "evenement_sous_taux_nul")
  ev_sans <- sum(as.numeric(cel$evenements[sans_attendu]))
  total <- sum(as.numeric(exposition$evenements))
  if (!identical(sum(O) + ev_sans + ap$hors_plage$evenements, total))
    stop("calculer_positionnement() : décomptes d'événements incohérents (erreur de programmation).", call. = FALSE)
  avert <- avertissements_vides()
  if (any(sans_attendu))
    avert <- avertir(avert, "positionnement", "evenements_sans_attendu",
                     sprintf(paste("Positionnement : %d cellule(s) qui comptent %d événement(s) sans attendu",
                                   "(exposition nulle, ou q_x de la table nul) : hors de O et de A (états",
                                   "evenement_sans_exposition, evenement_sous_taux_nul) ; événements restitués à part."),
                             sum(sans_attendu), as.integer(ev_sans)),
                     sum(sans_attendu))
  if (any(arrondi))
    avert <- avertir(avert, "positionnement", "qx_arrondi_un",
                     formuler(paste("Positionnement : %d ligne(s) de la table positionnée où q~ < 1 et une valeur vaut",
                                    "1 par arrondi : %s. theta (-ln(1 - q~)) >= 54 ln 2 (environ 37,43) : la survie",
                                    "(1 - q~)^theta, inférieure à 2^-54, est arrondie à 1 en double précision (état",
                                    "qx_arrondi_un ; ni refus, ni plafonnement)."),
                              sum(arrondi),
                              paste(vapply(which(arrondi), function(i)
                                formuler("%sâge %d, theta^ = %.17g, valeur(s) égale(s) à 1 : %s",
                                         paste(vapply(segmentation, function(v)
                                           formuler("%s = « %s », ", citer(v), citer(as.character(tp[[v]][i]))), ""),
                                           collapse = ""),
                                         tp$age[i], sm$smr[gk[i]], liste_guillemets(colonnes_q[un[i, ]])), ""),
                                    collapse = " ; ")),
                     sum(arrondi))
  etats <- ETATS_CELLULE_POSITIONNEMENT
  etats <- data.frame(etat = etats,
                      cellules = vapply(etats, function(e) sum(at$etat == e), 0L, USE.NAMES = FALSE),
                      evenements = vapply(etats, function(e) sum(as.numeric(cel$evenements[at$etat == e])), 0,
                                          USE.NAMES = FALSE),
                      stringsAsFactors = FALSE)
  parametres <- list(methode = methode, table_reference = table_reference,
                     table_nom = attr(table, "nom", exact = TRUE), plage_ages = plage_ages, niveau = niveau,
                     convention = convention, appariement_sexe = ap$appariement_sexe,
                     parametres_estimes = PARAMETRES_ESTIMES_POSITIONNEMENT, statuts = STATUTS_POSITIONNEMENT,
                     reference_methode = REFERENCE_METHODE_POSITIONNEMENT)
  list(parametres = parametres,
       resultat = list(cellules = cel, smr = smr, table_positionnee = tp),
       diagnostics = list(cellules = nrow(cel), cellules_retenues = sum(ret), etats = etats,
                          evenements_sans_attendu = ev_sans, cellules_hors_plage = ap$hors_plage$cellules,
                          evenements_hors_plage = ap$hors_plage$evenements,
                          exposition_hors_plage = ap$hors_plage$exposition, evenements_total = total),
       avertissements = avert)
}
