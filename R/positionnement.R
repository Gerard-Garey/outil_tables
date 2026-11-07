# -----------------------------------------------------------------------------
#  R/positionnement.R : module `positionnement`, briques d'appariement à une
#  table de référence et de SMR partagées par les étapes `positionnement` et
#  `validation` (issue #167 ; avis de forme (0'') d'architect-approfondi, F1 :
#  déplacement pur depuis R/validation.R, sans changement de corps ni de
#  signature ; ADR 0007, point 2.1 : `positionnement` précède `validation`)
#
#  - CONVENTIONS_VALIDATION : conventions d'âge admises ;
#  - valeurs_entieres_double() : forme des paramètres à valeur entière ;
#  - controler_entrees_validation() : convention d'âge retenue, table sans
#    année, appariement par sexe, couverture de la plage ; appelée avant
#    exécution par controler_entrees() (construire_table(), #82), une fois,
#    rejeu compris ;
#  - apparier_table(), attendus_cellules() : grille des cellules de la plage,
#    q~ apparié par clé exacte (sexe, âge), A = E^c mu~, mu~ = -log1p(-q~),
#    état de chaque cellule ;
#  - smr_poisson() : SMR et inférence de Poisson exacte et asymptotique.
#
#  Notations : celles de R/validation.R (M217). Les refus gardent le module
#  `validation` et leurs motifs (déplacement pur ; leur généralisation à
#  l'étape appelante est l'objet d'un commit ultérieur, F2).
#  Ce module n'appelle que `outils` (refuser(), formuler(), forme_exacte())
#  et `donnees` (citer(), liste_guillemets(), SEXES_TABLE) (ADR 0007,
#  point 2.1). Les arêtes vers donnees::table_reference éventuellement
#  relevées par graphe_modules.R sont des homonymies de l'argument
#  `table_reference` (clé de la table).
# -----------------------------------------------------------------------------

# Conventions d'âge admises (M255, point 3) : cellule [x, x+1).
CONVENTIONS_VALIDATION <- "dernier_anniversaire"

# Entier unique de type double, fini, à valeur entière (paramètres à valeur
# entière, M246, M257 : un integer est refusé sous le motif du paramètre).
valeurs_entieres_double <- function(v) forme_exacte(v, "double") && !anyNA(v) && all(is.finite(v)) && all(v == trunc(v))

# Contrôles d'entrée avant exécution (avis (0''), M253 ; ADR 0015, point 7.3),
# dans l'ordre : convention d'âge retenue (convention_validation_non_admise) ;
# table sans annee (table_annee_non_admise) ; modalités de sexe de la table
# M(T) mêlant ensemble et H ou F (table_sexes_ambigus) ; table par sexe sans
# sexe dans la segmentation (table_sexe_incompatible) ; chaque sexe de la
# table couvre plage_ages (plage_ages_hors_table ; les âges d'un sexe sont
# contigus, contrôlé par table_reference()). `table` : objet table_reference
# contrôlé (controler_tables_reference()) ; `convention` : convention
# retenue ; `cle` : clé citée par le plan. Rend TRUE, invisible.
controler_entrees_validation <- function(table, cle, convention, segmentation, plage_ages) {
  if (!(convention %in% CONVENTIONS_VALIDATION))
    refuser("validation", "convention_validation_non_admise",
            formuler(paste("Validation : convention d'âge retenue « %s » non admise (admise : %s). Sous",
                           "age_atteint_annee, l'appariement d'une cellule d'année civile à la table n'est pas",
                           "spécifié : la validation est refusée (décision Q7 (a) de #4)."),
                     citer(convention), liste_guillemets(CONVENTIONS_VALIDATION)),
            list(convention = convention, attendu = CONVENTIONS_VALIDATION))
  if ("annee" %in% names(table))
    refuser("validation", "table_annee_non_admise",
            formuler(paste("Validation : la table de clé « %s » porte une colonne annee ; cette version ne valide que",
                           "des tables sans année (ADR 0015, point 7.4)."), citer(cle)),
            list(cle = cle))
  sexes <- unique(table$sexe)
  if ("ensemble" %in% sexes && length(sexes) > 1L)
    refuser("validation", "table_sexes_ambigus",
            formuler(paste("Validation : la table de clé « %s » mêle la modalité « ensemble » et %s : l'appariement",
                           "par sexe serait ambigu, il est refusé."),
                     citer(cle), liste_guillemets(setdiff(sexes, "ensemble"))),
            list(cle = cle, sexes = sexes))
  if (!("sexe" %in% segmentation) && !("ensemble" %in% sexes))
    refuser("validation", "table_sexe_incompatible",
            formuler(paste("Validation : la table de clé « %s » est par sexe (%s) et la construction n'est pas",
                           "segmentée par sexe : aucune valeur de la table ne s'applique sans déduction ; segmenter",
                           "par sexe, ou fournir une table « ensemble »."),
                     citer(cle), liste_guillemets(sexes)),
            list(cle = cle, sexes = sexes))
  for (s in SEXES_TABLE[SEXES_TABLE %in% sexes]) {
    a <- table$age[table$sexe == s]
    if (min(a) > plage_ages[1L] || max(a) < plage_ages[2L])
      refuser("validation", "plage_ages_hors_table",
              formuler(paste("Validation : la table de clé « %s » (sexe %s) couvre les âges %d à %d, qui ne",
                             "contiennent pas la plage d'âges %d à %d."),
                       citer(cle), citer(s), min(a), max(a), as.integer(plage_ages[1L]), as.integer(plage_ages[2L])),
              list(cle = cle, sexe = s, ages_table = range(a), plage_ages = plage_ages))
  }
  invisible(TRUE)
}

# Grille des cellules de la plage et q~ apparié (M253, points 2 et 5).
#   exposition : table de l'étape `exposition` ; segmentation, par_annee ;
#   table : table_reference contrôlée par controler_entrees_validation() ;
#   plage_ages ; cle : clé citée (messages).
# Grille : segments présents dans la table d'exposition (dans son ordre) x
# âges de la plage (x années de la table d'exposition si par_annee), E^c = 0
# et D = 0 pour une cellule sans ligne d'exposition ; aucun segment absent
# n'est créé. Appariement sur la clé exacte (sexe, âge), sans déduction : la
# modalité ensemble s'applique à chaque cellule ; sinon le sexe de la cellule.
# Refus en cours d'étape : cellule_sans_valeur_table (sexe absent de la
# table) ; avec refuser_sans_valeur = FALSE (comparateur 2 de l'étape
# importance_exclusions, #69, sans refus), q~ NA pour ces cellules. Rend
# list(cellules (clés, age [, annee], exposition_centrale, evenements,
# qx_table), segment (indice), hors_plage (cellules, evenements,
# exposition), appariement_sexe).
apparier_table <- function(exposition, segmentation, par_annee, table, plage_ages, cle, refuser_sans_valeur = TRUE) {
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
    refuser("validation", "cellule_sans_valeur_table",
            formuler(paste("Validation : %d cellule(s) de la plage sans valeur dans la table de clé « %s » (sexe(s) %s",
                           "de la construction absent(s) de la table) ; l'appariement se fait sur la clé exacte, sans",
                           "déduction."),
                     sum(is.na(k)), citer(cle), liste_guillemets(manquants)),
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
# si E^c = 0 (pas de 0 x Inf) ; refus taux_table_egal_un si q~ = 1 et E^c > 0.
# Rend list(taux_table, attendus, etat).
attendus_cellules <- function(Ec, D, qx, cle) {
  un <- qx == 1 & Ec > 0
  if (any(un))
    refuser("validation", "taux_table_egal_un",
            formuler(paste("Validation : %d cellule(s) exposée(s) où la table de clé « %s » donne q_x = 1 : l'attendu",
                           "A = E^c (-ln(1 - q_x)) est infini, le modèle de Poisson ne s'applique pas."),
                     sum(un), citer(cle)),
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
