# -----------------------------------------------------------------------------
#  R/validation.R : module `validation`, statistiques de validation d'une table
#  de référence prise telle quelle (p = 0) (issue #4 ; spécification
#  d'actuary-approfondi décidée, M217 ; avis de forme (0''), M242 à M248 ;
#  six points d'actuary-approfondi, M253 à M258 ; ADR 0015, point 7 et
#  seconde annotation du 06/11/2026 ; ADR 0007 et ADR 0012, secondes
#  annotations du 06/11/2026)
#
#  - controler_parametres_validation() : paramètres de l'étape (clé de table,
#    relation table / observations, plage d'âges, tranches, niveau,
#    réplications) ; appelée par valider_plan() et, en appel direct, par la
#    fonction d'étape ;
#  - controler_entrees_validation() : convention d'âge retenue, table sans
#    année, appariement par sexe, couverture de la plage ; appelée avant
#    exécution par construire_table() et par rejouer_fiche() ;
#  - apparier_table(), attendus_cellules() : grille des cellules de la plage,
#    q~ apparié par clé exacte (sexe, âge), A = E^c mu~, mu~ = -log1p(-q~),
#    état de chaque cellule ;
#  - statistiques_test() : statistiques des tests de Monte-Carlo (X2, G2,
#    signes, runs, Wilcoxon) sur une matrice de décès (une colonne par jeu) :
#    observé et répliques passent par la même fonction ;
#  - smr_poisson(), loi_runs(), wilcoxon_rangs(), probabilite_hors() : briques
#    exactes et asymptotiques ;
#  - replications_poisson() (sous sous_graine()) et pvaleur_monte_carlo() ;
#  - statistiques_validation() : S1 à S12 sur les cellules (cœur partagé avec
#    #69 et le futur suivre_experience(), ADR 0015, point 7.5) ;
#  - calculer_validation() : fonction d'étape `validation`.
#
#  Notations (M217) : D = evenements ; E^c = exposition_centrale ; q~ = q_x
#  de la table ; mu~ = -log1p(-q~) ; A = E^c mu~ (A = 0 si E^c = 0, même pour
#  q~ = 1) ; C+ = {A > 0} ; r = D - A ; mu^ = D / E^c (taux_poisson(), module
#  brut) ; alpha = 1 - niveau. H0 (i) : D_c ~ Poisson(A_c) indépendants ;
#  exacte avec E^c fixe, asymptotique sous le modèle de survie. p = 0
#  (`independante`) ; `ajustee_hors_outil` : p non établi, aucune grandeur
#  sous H0 produite, aucun tirage (M254). Aucun verdict, aucun seuil (M217,
#  Q4). Formules sans attribution d'auteur (M200).
#  Ce module n'appelle que `outils`, `donnees` (citer(), liste_guillemets(),
#  controler_segmentation(), SEXES_TABLE) et `brut` (taux_poisson()) ; le
#  niveau est contrôlé ici sous le code niveau_invalide de `brut` (M253)
#  (ADR 0007, point 2.1 et seconde annotation du 06/11/2026, (d)). Les
#  arêtes vers donnees::table_reference relevées par graphe_modules.R sont
#  des homonymies de l'argument `table_reference` (clé de la table).
# -----------------------------------------------------------------------------

# Relation entre la table et les observations (ADR 0015, point 7.1 ; M237,
# M253) -> nombre de paramètres estimés sur ces données : 0 (table prise
# telle quelle) ou NA (non établi, ddl_non_etabli). 1L est réservé à A2.
RELATIONS_TABLE <- c(independante = 0L, ajustee_hors_outil = NA_integer_)
# Conventions d'âge admises (M255, point 3) : cellule [x, x+1).
CONVENTIONS_VALIDATION <- "dernier_anniversaire"
# Effectif maximal de la loi exacte de Wilcoxon par psignrank() (M256) :
# finie jusqu'à n = 1038 sur l'hôte de mesure, hypothèse non mesurée ailleurs.
N_MAX_WILCOXON_EXACT <- 1000
# Nombre maximal de résidus non nuls (somme sur les séquences) pour la loi
# exacte des suites par loi_runs() (M2 de l'audit de #4) : au-delà, p exacte
# NA (effectif_au_dela_n_max), asymptotique et Monte-Carlo produites ; choix
# technique validé par actuary.
N_MAX_RUNS_EXACT <- 10000
# Tolérance relative d'un résidu nul (M1 de l'audit de #4) : r = D - A est nul
# si |r| <= TOLERANCE_RESIDU_NUL x A (signe_residu()) ; un seul ensemble de
# zéros pour les signes, les suites et Wilcoxon, observé et répliques.
TOLERANCE_RESIDU_NUL <- 1e-10
# Nombre maximal d'éléments d'un bloc de réplications (cellules x
# réplications) : les réplications sont tirées par blocs de colonnes, à la
# suite, sous une seule graine (jamais par cellules) ; le découpage ne change
# aucun tirage (mesuré par le test J5).
ELEMENTS_BLOC_REPLICATIONS <- 2^22
# Identité de la note de méthode (M244 ; spécification, § 1), dans
# etapes$validation$parametres$reference_methode.
REFERENCE_METHODE_VALIDATION <- texte_moteur(paste(
  "note II1291-14 v1.4, « Critères de validation : aspects méthodologiques », fichier II1291-14_v1.4.pdf",
  "(jeu ISFA), SHA-256 1c653faf3f13e0d626cb7e7929eb46974cf773ccf65a12c7630b6bf51baba301 ;",
  "référence de place, statistiques sous leur forme usuelle, écarts",
  "documentés"))
# Hypothèse nulle (M217, § 1), dans les paramètres de l'étape.
HYPOTHESE_VALIDATION <- texte_moteur(paste(
  "H0 (i) : D_c ~ Poisson(A_c) indépendants, A_c = E^c_c mu~_c, mu~_c = -ln(1 - q~_c) ; exacte avec E^c tenue",
  "pour fixe, asymptotique sous le modèle de survie (E^c aléatoire) ; unités d'observation indépendantes",
  "(l'unité contrat rend les tests anti-conservateurs) ; cellule [x, x+1) de la convention",
  "dernier_anniversaire."))

# États d'une cellule (M253, point 1) : retenue (A > 0, dans C+) ;
# sans_exposition (E^c = 0, D = 0) ; taux_table_nul (A = 0 avec E^c > 0, D =
# 0) ; evenement_sans_exposition (E^c = 0, D > 0) ; evenement_sous_taux_nul
# (A = 0 avec E^c > 0, D > 0). A = 0 avec E^c > 0 : q~ = 0, ou produit
# E^c mu~ nul en double.
ETATS_CELLULE_VALIDATION <- c("retenue", "sans_exposition", "taux_table_nul", "evenement_sans_exposition",
                              "evenement_sous_taux_nul")
# États d'une grandeur (valeur NA hors `produite`).
ETATS_GRANDEUR_VALIDATION <- c("produite", "ddl_non_etabli", "aucune_cellule_retenue", "aucun_residu_non_nul",
                               "variance_nulle", "ex_aequo", "effectif_au_dela_n_max", "aucun_deces",
                               "variance_observee_nulle")

# Statuts (E2.20) des grandeurs de resultat$statistiques, une ligne par
# (statistique, grandeur), dans l'ordre du tableau ; sous_h0 : grandeur
# définie par H0 (p-valeur, intervalle, espérance ou variance sous H0,
# statistique standardisée, ddl), non produite sous `ajustee_hors_outil`
# (M254) ; les lignes `sous_groupes` décrivent resultat$sous_groupes.
STATUTS_VALIDATION <- local({
  desc <- "descriptif : calcul direct sur les cellules retenues (C+), valeur en arithmétique flottante"
  exact_h0 <- paste("exact sous H0 (i), E^c tenue pour fixe ; asymptotique sous le modèle de survie ;",
                    "fonctions de répartition de Poisson (ppois), approximation numérique")
  chi2 <- "asymptotique : loi du khi-deux à n - p degrés de liberté (A_c -> infini), p = 0"
  mc <- paste("niveau exact sous H0 (i), E^c tenue pour fixe : P_H0(p_MC <= alpha) <= alpha ; e est l'erreur due",
              "au nombre fini de réplications, distincte de l'erreur d'estimation")
  e_mc <- paste("erreur de Monte-Carlo e = sqrt(B p_MC (1 - p_MC)) / (B + 1), due au nombre fini de",
                "réplications, distincte de l'erreur d'estimation")
  signes_h <- "P(r_c > 0) = 1/2 par cellule, ce qui n'est pas H0 (i)"
  runs_h <- paste("conditionnellement à (n+, n-) par séquence, sous l'hypothèse que tous les ordres des signes",
                  "sont équiprobables, ce qui n'est pas H0 (i)")
  zero <- "; zéro : |D - A| <= 1e-10 A"
  wilc_h <- "différences indépendantes et symétriques autour de 0, ce qui n'est pas H0 (i)"
  un_signe <- paste("état variance_nulle (NA) quand la variance conditionnelle de R est nulle : R est alors certain",
                    "(chaque séquence n'a qu'un signe, ou un résidu de chaque signe), p_exacte vaut 1")
  l <- list(
    c("residus", "au_dela_2", "FALSE", "descriptif : nombre de cellules retenues où |D - A| / sqrt(A) > 2"),
    c("residus", "esperance_au_dela_2", "TRUE", paste(exact_h0, "; somme sur C+ de P(|D_c - A_c| > 2 sqrt(A_c))")),
    c("residus", "au_dela_3", "FALSE", "descriptif : nombre de cellules retenues où |D - A| / sqrt(A) > 3"),
    c("residus", "esperance_au_dela_3", "TRUE", paste(exact_h0, "; somme sur C+ de P(|D_c - A_c| > 3 sqrt(A_c))")),
    c("khi2_pearson", "statistique", "FALSE", "descriptif : X2 = somme sur C+ de (D - A)^2 / A"),
    c("khi2_pearson", "ddl", "TRUE", "n - p : n cellules retenues, p paramètres estimés sur ces données (0)"),
    c("khi2_pearson", "p_asymptotique", "TRUE", paste(chi2, "; queue supérieure")),
    c("khi2_pearson", "p_monte_carlo", "TRUE", paste(mc, "; queue supérieure")),
    c("khi2_pearson", "erreur_monte_carlo", "TRUE", e_mc),
    c("deviance", "statistique", "FALSE",
      "descriptif : G2 = 2 somme sur C+ de [D ln(D / A) - (D - A)] (0 ln 0 = 0), rapport de vraisemblance"),
    c("deviance", "ddl", "TRUE", "n - p : n cellules retenues, p paramètres estimés sur ces données (0)"),
    c("deviance", "p_asymptotique", "TRUE", paste(chi2, "; queue supérieure")),
    c("deviance", "p_monte_carlo", "TRUE", paste(mc, "; queue supérieure")),
    c("deviance", "erreur_monte_carlo", "TRUE", e_mc),
    c("smr", "observes", "FALSE", "descriptif : O = somme de D sur C+"),
    c("smr", "attendus", "FALSE", "descriptif : A = somme de A_c sur C+"),
    c("smr", "smr", "FALSE",
      "résultat exact : O / A, maximum de vraisemblance de theta dans le modèle D_c ~ Poisson(theta A_c)"),
    c("smr", "ecart", "FALSE", "descriptif : O - A"),
    c("smr", "borne_inf_exacte", "TRUE",
      paste("exact sous la loi supposée (O ~ Poisson, A fixe) : G_O^-1(alpha/2) / A, G_k loi gamma de forme k",
            "(0 si O = 0) ; couverture >= niveau (intervalle exact conservatif) ; qgamma, approximation numérique")),
    c("smr", "borne_sup_exacte", "TRUE",
      paste("exact sous la loi supposée (O ~ Poisson, A fixe) : G_(O+1)^-1(1 - alpha/2) / A ; couverture >= niveau",
            "(intervalle exact conservatif) ; qgamma, approximation numérique")),
    c("smr", "p_inferieure", "TRUE", paste(exact_h0, ": P(X <= O), X ~ Poisson(A)")),
    c("smr", "p_superieure", "TRUE", paste(exact_h0, ": P(X >= O), X ~ Poisson(A)")),
    c("smr", "p_centrale", "TRUE", paste(exact_h0, ": min(1, 2 min(P(X <= O), P(X >= O)))")),
    c("smr", "z_racine_cubique", "TRUE",
      paste("asymptotique : racine cubique corrigée, O >= A : z = 3 sqrt(O) (1 - 1/(9 O) - (A/O)^(1/3)) ;",
            "O < A : O* = O + 1, z = 3 sqrt(O*) ((A/O*)^(1/3) + 1/(9 O*) - 1)")),
    c("smr", "p_racine_cubique", "TRUE",
      paste("asymptotique : 1 - Phi(z), approximation de la p-valeur unilatérale exacte dans le sens de l'écart",
            "observé (P(X >= O) si O >= A, P(X <= O) sinon)")),
    c("signes", "positifs", "FALSE", paste("descriptif : n+ = nombre de cellules retenues où D - A > 0", zero)),
    c("signes", "negatifs", "FALSE", paste("descriptif : n- = nombre de cellules retenues où D - A < 0", zero)),
    c("signes", "nuls", "FALSE",
      paste("descriptif : nombre de cellules retenues de résidu nul (retirées du test)", zero)),
    c("signes", "effectif", "FALSE", "descriptif : n = n+ + n-"),
    c("signes", "statistique", "TRUE",
      paste("asymptotique sous", signes_h, ": (|n+ - n-| - 1) / sqrt(n), loi normale centrée réduite")),
    c("signes", "p_asymptotique", "TRUE", paste("asymptotique sous", signes_h, ": min(1, 2 (1 - Phi(statistique)))")),
    c("signes", "p_exacte", "TRUE", paste("exacte sous", signes_h, ": loi binomiale (n, 1/2), p centrale ; pbinom")),
    c("signes", "esperance_positifs_h0", "TRUE",
      paste(exact_h0, ": somme sur C+ de P(D_c - A_c > 1e-10 A_c), diagnostic de l'écart entre H0 (i) et", signes_h)),
    c("signes", "p_monte_carlo", "TRUE",
      paste(mc, "; p centrale sur (n+ - n-) / sqrt(n), valeur 0 si n = 0 pour une réplique ; NA si n = 0 pour l'observé")),
    c("signes", "erreur_monte_carlo", "TRUE", e_mc),
    c("runs", "sequences", "FALSE",
      paste("descriptif : nombre de séquences (segment x année), âges croissants, résidus nuls retirés", zero)),
    c("runs", "runs", "FALSE", "descriptif : R = somme sur les séquences du nombre de suites de signes égaux"),
    c("runs", "esperance", "TRUE", paste("somme des moments par séquence", runs_h, ": 1 + 2 n+ n- / n")),
    c("runs", "variance", "TRUE",
      paste("somme des moments par séquence", runs_h, ": 2 n+ n- (2 n+ n- - n) / (n^2 (n - 1))")),
    c("runs", "statistique", "TRUE",
      paste("asymptotique", runs_h, ": (R - esperance) / sqrt(variance) ;", un_signe)),
    c("runs", "p_asymptotique", "TRUE",
      paste("asymptotique", runs_h, ": Phi(statistique), queue inférieure ;", un_signe)),
    c("runs", "p_exacte", "TRUE",
      paste("exacte", runs_h, ": P(R <= r), convolution des lois par séquence (lchoose, approximation numérique) ;",
            "produite si la somme des n est au plus N_MAX_RUNS_EXACT")),
    c("runs", "p_monte_carlo", "TRUE",
      paste(mc, "; queue inférieure sur R ; statistique R non conditionnelle à (n+, n-) : sensible au déséquilibre",
            "des signes comme au regroupement")),
    c("runs", "erreur_monte_carlo", "TRUE", e_mc),
    c("wilcoxon", "effectif", "FALSE",
      paste("descriptif : n = nombre de cellules retenues où la différence d = D / E^c - mu~ (= mu^ - mu~) est",
            "non nulle", zero)),
    c("wilcoxon", "nuls", "FALSE",
      paste("descriptif : nombre de cellules retenues de différence nulle (retirées du test)", zero)),
    c("wilcoxon", "groupes_ex_aequo", "FALSE", "descriptif : nombre de groupes de |mu^ - mu~| égaux (taille >= 2)"),
    c("wilcoxon", "w_plus", "FALSE", "descriptif : somme des rangs moyens de |mu^ - mu~| des différences positives"),
    c("wilcoxon", "variance", "TRUE",
      paste("sous l'hypothèse de", wilc_h, ": n (n + 1) (2 n + 1) / 24 - somme (t^3 - t) / 48 (ex aequo)")),
    c("wilcoxon", "statistique", "TRUE",
      paste("asymptotique sous l'hypothèse de", wilc_h,
            ": z = (w+ - n (n + 1) / 4 - signe x 1/2) / sqrt(variance), correction de continuité")),
    c("wilcoxon", "p_asymptotique", "TRUE",
      paste("asymptotique sous l'hypothèse de", wilc_h, ": p centrale min(1, 2 min(Phi(z), 1 - Phi(z)))")),
    c("wilcoxon", "p_exacte", "TRUE",
      paste("loi exacte sous l'hypothèse de", wilc_h, "; psignrank, approximation numérique (écart relatif mesuré",
            "<= 1e-13) ; sans ex aequo et n <= N_MAX_WILCOXON_EXACT ; p centrale")),
    c("wilcoxon", "p_monte_carlo", "TRUE",
      paste(mc, "; p centrale sur z, z = 0 si n = 0 pour une réplique ; NA si n = 0 pour l'observé")),
    c("wilcoxon", "erreur_monte_carlo", "TRUE", e_mc),
    c("mape", "valeur", "FALSE", "descriptif, fragile : 100 / m+ somme sur C+, D > 0, de |D - A| / D (m+ cellules)"),
    c("r2", "valeur", "FALSE",
      paste("descriptif, fragile : 1 - somme (mu^ - mu~)^2 / somme (mu^ - moyenne de mu^)^2 sur C+ ; peut être",
            "négatif")),
    c("distance_probabilites", "valeur", "FALSE",
      "descriptif : somme sur C+ de |q^ - q~|, q^ = 1 - exp(-mu^) (échelle des probabilités)"),
    c("fluctuation", "hors_intervalle_exact", "TRUE",
      "nombre de cellules retenues où D est hors de [qpois(alpha/2, A), qpois(1 - alpha/2, A)]"),
    c("fluctuation", "esperance_hors_intervalle_exact", "TRUE", paste(exact_h0, ": somme sur C+ de P(D_c hors intervalle)")),
    c("fluctuation", "hors_intervalle_asymptotique", "TRUE",
      "nombre de cellules retenues où D est hors de A +- z sqrt(A), z = qnorm(alpha/2, lower.tail = FALSE)"),
    c("fluctuation", "esperance_hors_intervalle_asymptotique", "TRUE",
      paste(exact_h0, ": somme sur C+ de P(D_c hors de A +- z sqrt(A))")),
    c("sous_groupes", "smr", "FALSE", "résultat exact : O / A du sous-groupe (cellules retenues)"),
    c("sous_groupes", "bornes_exactes", "TRUE",
      "exact sous la loi supposée (O ~ Poisson, A fixe) ; qgamma, approximation numérique"),
    c("sous_groupes", "p_valeurs", "TRUE",
      paste(exact_h0, "; p = 0 : X ~ Poisson(A) ; aucune correction de multiplicité")))
  data.frame(statistique = vapply(l, `[`, "", 1L), grandeur = vapply(l, `[`, "", 2L),
             sous_h0 = vapply(l, `[`, "", 3L) == "TRUE", statut = vapply(l, `[`, "", 4L),
             stringsAsFactors = FALSE)
})

# Entier unique de type double, fini, à valeur entière (paramètres à valeur
# entière, M246, M257 : un integer est refusé sous le motif du paramètre).
valeurs_entieres_double <- function(v) forme_exacte(v, "double") && !anyNA(v) && all(is.finite(v)) && all(v == trunc(v))

# Contrôle les paramètres propres à l'étape `validation` (valeurs ; les noms
# sont contrôlés par valider_plan()), dans l'ordre du catalogue. Forme exacte
# (#23) : type de base exact, aucun attribut, sinon refus sous le motif du
# paramètre ; un integer reçu pour plage_ages, tranches_age ou replications
# est refusé (M257). Rend TRUE, invisible.
controler_parametres_validation <- function(table_reference, relation_table, plage_ages, tranches_age, niveau,
                                            replications) {
  if (!chaine_unique(table_reference))
    refuser("validation", "cle_table_invalide",
            formuler(paste("La clé de la table de référence (table_reference) doit être une chaîne unique, non vide,",
                           "sans attribut : le nom, dans l'argument « tables_reference », de la table à valider.")),
            list(table_reference = table_reference))
  if (!chaine_unique(relation_table) || !(relation_table %in% names(RELATIONS_TABLE)))
    refuser("validation", "relation_table_invalide",
            formuler(paste("La relation entre la table et les observations (relation_table) doit être une chaîne",
                           "unique, sans attribut, prise dans %s, sans défaut (ADR 0015, point 7.1)."),
                     liste_guillemets(names(RELATIONS_TABLE))),
            list(relation_table = relation_table))
  if (!valeurs_entieres_double(plage_ages) || length(plage_ages) != 2L || plage_ages[1L] < 0 ||
      plage_ages[1L] > plage_ages[2L] || plage_ages[2L] > .Machine$integer.max)
    refuser("validation", "plage_ages_invalide",
            formuler(paste("La plage d'âges (plage_ages) doit être un vecteur double sans attribut de deux valeurs",
                           "entières finies, 0 <= min <= max (un integer est refusé).")),
            list(plage_ages = plage_ages))
  if (!valeurs_entieres_double(tranches_age) || !length(tranches_age) || is.unsorted(tranches_age, strictly = TRUE) ||
      tranches_age[1L] != plage_ages[1L] || tranches_age[length(tranches_age)] > plage_ages[2L])
    refuser("validation", "tranches_age_invalides",
            formuler(paste("Les tranches d'âge (tranches_age) doivent être les bornes inférieures, vecteur double sans",
                           "attribut de valeurs entières strictement croissantes, la première égale au minimum de",
                           "plage_ages, la dernière au plus égale à son maximum (un integer est refusé).")),
            list(tranches_age = tranches_age))
  if (!forme_exacte(niveau, "double") || length(niveau) != 1L || !is.finite(niveau) || niveau <= 0 || niveau >= 1)
    refuser("validation", "niveau_invalide",
            formuler(paste("Le niveau de confiance doit être un nombre (double) unique, sans attribut, fini,",
                           "strictement compris entre 0 et 1.")),
            list(niveau = niveau))
  if (!valeurs_entieres_double(replications) || length(replications) != 1L || replications < 1 ||
      replications > .Machine$integer.max)
    refuser("validation", "replications_invalides",
            formuler(paste("Le nombre de réplications de Monte-Carlo (replications) doit être un nombre double unique,",
                           "sans attribut, entier, au moins 1 et au plus 2147483647, sans défaut (un integer est",
                           "refusé).")),
            list(replications = replications))
  invisible(TRUE)
}

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
# table). Rend list(cellules (clés, age [, annee], exposition_centrale,
# evenements, qx_table), segment (indice), hors_plage (cellules, evenements,
# exposition), appariement_sexe).
apparier_table <- function(exposition, segmentation, par_annee, table, plage_ages, cle) {
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
  if (anyNA(k)) {
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

# P(X < l) + P(X > u), X ~ Poisson(A), par cellule (bornes réelles ou
# entières ; ppois d'un argument négatif vaut 0).
probabilite_hors <- function(A, l, u)
  stats::ppois(ceiling(l) - 1, A) + stats::ppois(floor(u), A, lower.tail = FALSE)

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

# Loi exacte du nombre total de suites R sur des séquences indépendantes
# (S7), conditionnellement à (n1, n2) par séquence, tous les ordres
# équiprobables : P(R = 2k) = 2 C(n1-1, k-1) C(n2-1, k-1) / C(n, n1) ;
# P(R = 2k+1) = [C(n1-1, k-1) C(n2-1, k) + C(n1-1, k) C(n2-1, k-1)] / C(n, n1) ;
# R = 1 si une seule modalité, 0 si la séquence est vide ; convolution
# directe. Rend le vecteur P(R = 0), P(R = 1)...
loi_runs <- function(n1, n2) {
  loi <- 1
  for (s in seq_along(n1)) {
    a <- n1[s]; b <- n2[s]; n <- a + b
    p <- numeric(n + 1L)
    if (!n) p[1L] <- 1
    else if (!a || !b) p[2L] <- 1
    else {
      r <- 2:n
      k <- r %/% 2
      lc <- lchoose(n, a)
      pair <- log(2) + lchoose(a - 1, k - 1) + lchoose(b - 1, k - 1) - lc
      impair <- exp(lchoose(a - 1, k - 1) + lchoose(b - 1, k) - lc) + exp(lchoose(a - 1, k) + lchoose(b - 1, k - 1) - lc)
      p[r + 1L] <- ifelse(r %% 2 == 0, exp(pair), impair)
    }
    v <- numeric(length(loi) + length(p) - 1L)
    for (j in seq_along(p)) {
      ix <- j - 1L + seq_along(loi)
      v[ix] <- v[ix] + loi * p[j]
    }
    loi <- v
  }
  loi
}

# Rangs signés (S8) sur les différences d : zéros retirés et décomptés ;
# rangs moyens des |d| (ex aequo par égalité exacte) ; w+ ; variance corrigée
# des ex aequo ; z avec correction de continuité (forme de wilcox.test,
# correct = TRUE) ; z = 0 si aucune différence non nulle.
wilcoxon_rangs <- function(d) {
  nz <- d[d != 0]
  n <- length(nz)
  if (!n) return(list(effectif = 0, nuls = length(d), groupes = 0, w = 0, variance = 0, z = 0, ex_aequo = FALSE))
  a <- abs(nz)
  w <- sum(rank(a)[nz > 0])
  t <- tabulate(match(a, unique(a)))
  v <- n * (n + 1) * (2 * n + 1) / 24 - sum(t^3 - t) / 48
  c0 <- w - n * (n + 1) / 4
  list(effectif = n, nuls = length(d) - n, groupes = sum(t > 1L), w = w, variance = v,
       z = (c0 - sign(c0) * 0.5) / sqrt(v), ex_aequo = any(t > 1L))
}

# Signe d'un résidu r = D - A (vecteur ou matrice, A recyclé par colonne) :
# 0 si |r| <= TOLERANCE_RESIDU_NUL x A, sinon sign(r) ; seul ensemble de zéros
# des signes, des suites et de Wilcoxon, pour l'observé comme pour les
# répliques (M1 de l'audit de #4).
signe_residu <- function(r, A) {
  s <- sign(r)
  s[abs(r) <= TOLERANCE_RESIDU_NUL * A] <- 0
  s
}

# Statistiques des tests de Monte-Carlo (M253, point 1 : « observé et
# répliques passent par la même fonction »), pour chaque colonne de la
# matrice Dm (cellules de C+ x jeux), cellules rangées par séquence puis âge ;
# debut : indice de la première ligne de la séquence de chaque ligne. Rend
# list(khi2, deviance, signes = (n+ - n-) / sqrt(n) (0 si n = 0), runs = R,
# wilcoxon = z (0 si n = 0)), un élément par colonne.
statistiques_test <- function(Dm, A, Ec, mu, debut) {
  n <- nrow(Dm)
  r <- Dm - A
  l <- Dm * log(Dm / A)
  l[Dm == 0] <- 0
  sg <- signe_residu(r, A)
  pos <- colSums(sg > 0); neg <- colSums(sg < 0); nn <- pos + neg
  signes <- numeric(ncol(Dm))
  signes[nn > 0] <- (pos[nn > 0] - neg[nn > 0]) / sqrt(nn[nn > 0])
  # Suites : une ligne non nulle ouvre une suite si aucune ligne non nulle ne
  # la précède dans sa séquence, ou si la précédente non nulle est de signe
  # différent (zéros retirés, séquence recousue).
  dernier <- matrix(apply((sg != 0) * seq_len(n), 2L, cummax), nrow = n)
  prec <- rbind(0, dernier[-n, , drop = FALSE])
  valide <- prec >= debut & prec > 0
  sg_prec <- numeric(length(sg))
  sg_prec[valide] <- sg[(col(sg)[valide] - 1) * n + prec[valide]]
  nouveau <- sg != 0 & (!valide | sg_prec != sg)
  d <- Dm / Ec - mu
  d[sg == 0] <- 0
  list(khi2 = colSums(r^2 / A), deviance = 2 * colSums(l - r), signes = signes, runs = colSums(nouveau),
       wilcoxon = vapply(seq_len(ncol(d)), function(j) wilcoxon_rangs(d[, j])$z, 0))
}

# Réplications de Monte-Carlo (M253, point 1 ; avis (0'')) : B jeux D* ~
# Poisson(A) (cellules de C+), tirés sous sous_graine(graine) par blocs de
# colonnes à la suite (ELEMENTS_BLOC_REPLICATIONS), chaque bloc réduit par
# `statistiques` (fonction d'une matrice, rendant une liste de vecteurs, un
# élément par colonne) ; un seul jeu de B réplications sert à toutes les
# statistiques. Rend la liste des vecteurs concaténés (longueur B).
replications_poisson <- function(A, B, graine, statistiques, elements_bloc = ELEMENTS_BLOC_REPLICATIONS) {
  n <- length(A)
  taille <- max(1, floor(elements_bloc / max(1, n)))
  sous_graine(graine, {
    morceaux <- list()
    fait <- 0
    while (fait < B) {
      m <- min(taille, B - fait)
      Dm <- matrix(stats::rpois(n * m, A), nrow = n)
      morceaux[[length(morceaux) + 1L]] <- statistiques(Dm)
      fait <- fait + m
    }
    stats::setNames(lapply(names(morceaux[[1L]]), function(k) unlist(lapply(morceaux, `[[`, k), use.names = FALSE)),
                    names(morceaux[[1L]]))
  })
}

# p-valeur de Monte-Carlo (M253, point 1) : p = (1 + K) / (B + 1), K nombre
# de répliques au moins aussi extrêmes (sens superieure : T* >= T ;
# inferieure : T* <= T ; centrale : min(1, 2 min(p_inf, p_sup))) ; erreur e =
# sqrt(B p (1 - p)) / (B + 1). Rend c(p, e).
pvaleur_monte_carlo <- function(t, t_rep, sens) {
  B <- length(t_rep)
  p_sup <- (1 + sum(t_rep >= t)) / (B + 1)
  p_inf <- (1 + sum(t_rep <= t)) / (B + 1)
  p <- switch(sens, superieure = p_sup, inferieure = p_inf, centrale = min(1, 2 * min(p_inf, p_sup)),
              stop("pvaleur_monte_carlo() : sens inconnu (erreur de programmation).", call. = FALSE))
  c(p = p, e = sqrt(B * p * (1 - p)) / (B + 1))
}

# Statistiques S1 à S12 sur des cellules (cœur partagé, ADR 0015, point 7.5) :
# reçoit des cellules (décès, exposition centrale, q~, état, séquence, âge),
# ne connaît pas l'origine des observations.
#   D, Ec, qx, mu, A, etat : par cellule (attendus_cellules()) ;
#   sequence : identifiant de séquence (segment x année) de chaque cellule ;
#   age : âge de chaque cellule ; niveau ;
#   parametres_estimes : 0L ou NA (RELATIONS_TABLE) ;
#   replications, graine : Monte-Carlo (aucun tirage si parametres_estimes est
#   NA ou si aucune cellule n'est retenue).
# Rend list(statistiques (format long, une ligne par ligne de
# STATUTS_VALIDATION hors sous_groupes : statistique, grandeur, valeur, etat,
# statut), cellules (colonnes calculées par cellule), sequences (n+, n-, R par
# séquence, dans l'ordre des séquences), tirages_effectues).
statistiques_validation <- function(D, Ec, qx, mu, A, etat, sequence, age, niveau, parametres_estimes,
                                    replications, graine) {
  alpha <- 1 - niveau
  zq <- stats::qnorm(alpha / 2, lower.tail = FALSE)
  nc <- length(D)
  ret <- etat == "retenue"
  ajustee <- is.na(parametres_estimes)
  # Cellules retenues rangées par séquence puis âge (ordre des suites).
  o <- which(ret)[order(sequence[ret], age[ret], method = "radix")]
  n <- length(o)
  Dr <- as.numeric(D[o]); Ar <- A[o]; Er <- Ec[o]; mur <- mu[o]; sr <- sequence[o]
  debut <- match(sr, sr)
  mu_hat <- taux_poisson(D, Ec, niveau)$taux

  val <- list(); et <- list()
  pose <- function(s, g, v, e = "produite") {
    k <- paste(s, g, sep = "/")
    val[[k]] <<- v; et[[k]] <<- e
  }
  # Colonnes par cellule (NA hors C+).
  rep_na <- function() rep(NA_real_, nc)
  cl <- list(attendus = A, taux_brut = mu_hat, probabilite_brute = -expm1(-mu_hat), residu_reponse = rep_na(),
             residu_pearson = rep_na(), residu_deviance = rep_na(), fluctuation_inf_exacte = rep_na(),
             fluctuation_sup_exacte = rep_na(), fluctuation_inf_asymptotique = rep_na(),
             fluctuation_sup_asymptotique = rep_na())
  seqs <- data.frame(sequence = integer(0), positifs = numeric(0), negatifs = numeric(0), runs = numeric(0))
  tirages <- 0
  if (n) {
    rr <- Dr - Ar
    dev <- Dr * log(Dr / Ar); dev[Dr == 0] <- 0
    dev <- dev - rr
    cl$residu_reponse[o] <- rr
    cl$residu_pearson[o] <- rr / sqrt(Ar)
    cl$residu_deviance[o] <- sign(rr) * sqrt(pmax(0, 2 * dev))
    # S1 : décomptes au-delà de 2 et 3 (bornes A -+ k sqrt(A)) et espérances.
    for (k in 2:3) {
      lo <- Ar - k * sqrt(Ar); hi <- Ar + k * sqrt(Ar)
      pose("residus", paste0("au_dela_", k), sum(Dr < lo | Dr > hi))
      pose("residus", paste0("esperance_au_dela_", k), sum(probabilite_hors(Ar, lo, hi)))
    }
    # Statistiques de test observées (même fonction que les répliques).
    t_obs <- statistiques_test(matrix(Dr, ncol = 1L), Ar, Er, mur, debut)
    ddl <- n - parametres_estimes
    pose("khi2_pearson", "statistique", t_obs$khi2)
    pose("khi2_pearson", "ddl", ddl)
    pose("khi2_pearson", "p_asymptotique", stats::pchisq(t_obs$khi2, ddl, lower.tail = FALSE))
    pose("deviance", "statistique", t_obs$deviance)
    pose("deviance", "ddl", ddl)
    pose("deviance", "p_asymptotique", stats::pchisq(t_obs$deviance, ddl, lower.tail = FALSE))
    # S4 : SMR global.
    O <- sum(Dr); At <- sum(Ar)
    sm <- smr_poisson(O, At, niveau)
    pose("smr", "observes", O); pose("smr", "attendus", At); pose("smr", "smr", sm$smr); pose("smr", "ecart", O - At)
    pose("smr", "borne_inf_exacte", sm$borne_inf); pose("smr", "borne_sup_exacte", sm$borne_sup)
    pose("smr", "p_inferieure", sm$p_inferieure); pose("smr", "p_superieure", sm$p_superieure)
    pose("smr", "p_centrale", sm$p_centrale)
    pose("smr", "z_racine_cubique", sm$z); pose("smr", "p_racine_cubique", sm$p_z)
    # S6 : signes.
    sgn <- signe_residu(rr, Ar)
    np <- sum(sgn > 0); nm <- sum(sgn < 0); ns <- np + nm
    pose("signes", "positifs", np); pose("signes", "negatifs", nm); pose("signes", "nuls", n - ns)
    pose("signes", "effectif", ns)
    pose("signes", "esperance_positifs_h0",
         sum(stats::ppois(floor(Ar * (1 + TOLERANCE_RESIDU_NUL)), Ar, lower.tail = FALSE)))
    if (ns) {
      xi <- (abs(np - nm) - 1) / sqrt(ns)
      pose("signes", "statistique", xi)
      pose("signes", "p_asymptotique", min(1, 2 * stats::pnorm(xi, lower.tail = FALSE)))
      pose("signes", "p_exacte", min(1, 2 * min(stats::pbinom(np, ns, 0.5), stats::pbinom(np - 1, ns, 0.5, lower.tail = FALSE))))
    } else for (g in c("statistique", "p_asymptotique", "p_exacte")) pose("signes", g, NA_real_, "aucun_residu_non_nul")
    # S7 : suites par séquence (zéros retirés).
    us <- unique(sr)
    n1 <- as.numeric(tabulate(match(sr[sgn > 0], us), nbins = length(us)))
    n2 <- as.numeric(tabulate(match(sr[sgn < 0], us), nbins = length(us)))
    # Une suite s'ouvre à la première cellule non nulle d'une séquence, ou
    # quand le signe change entre deux cellules non nulles consécutives.
    s_nz <- sr[sgn != 0]; g_nz <- sgn[sgn != 0]; k_nz <- length(s_nz)
    ouvre <- if (k_nz) c(TRUE, s_nz[-1L] != s_nz[-k_nz] | g_nz[-1L] != g_nz[-k_nz]) else logical(0)
    seqs <- data.frame(sequence = us, positifs = n1, negatifs = n2,
                       runs = as.numeric(tabulate(match(s_nz[ouvre], us), nbins = length(us))))
    if (!identical(sum(seqs$runs), as.numeric(t_obs$runs)))
      stop("statistiques_validation() : décompte des suites par séquence incohérent (erreur de programmation).",
           call. = FALSE)
    m <- n1 + n2
    mu_r <- ifelse(m > 0, 1 + ifelse(n1 * n2 > 0, 2 * n1 * n2 / m, 0), 0)
    v_r <- ifelse(n1 * n2 > 0, 2 * n1 * n2 * (2 * n1 * n2 - m) / (m^2 * (m - 1)), 0)
    R <- t_obs$runs
    pose("runs", "sequences", length(us)); pose("runs", "runs", R)
    pose("runs", "esperance", sum(mu_r)); pose("runs", "variance", sum(v_r))
    if (!sum(m)) {
      for (g in c("statistique", "p_asymptotique", "p_exacte")) pose("runs", g, NA_real_, "aucun_residu_non_nul")
    } else {
      if (sum(v_r) > 0) {
        xr <- (R - sum(mu_r)) / sqrt(sum(v_r))
        pose("runs", "statistique", xr); pose("runs", "p_asymptotique", stats::pnorm(xr))
      } else for (g in c("statistique", "p_asymptotique")) pose("runs", g, NA_real_, "variance_nulle")
      if (sum(m) > N_MAX_RUNS_EXACT) pose("runs", "p_exacte", NA_real_, "effectif_au_dela_n_max")
      else pose("runs", "p_exacte", min(1, sum(loi_runs(n1, n2)[seq_len(R + 1)])))
    }
    # S8 : Wilcoxon sur d = D / E^c - mu~ (= (D - A) / E^c, forme qui garde les
    # ex aequo exacts des cellules D = 0), nul hors des résidus non nuls (même
    # différence que statistiques_test()).
    dw <- Dr / Er - mur
    dw[sgn == 0] <- 0
    wr <- wilcoxon_rangs(dw)
    pose("wilcoxon", "effectif", wr$effectif); pose("wilcoxon", "nuls", wr$nuls)
    pose("wilcoxon", "groupes_ex_aequo", wr$groupes); pose("wilcoxon", "w_plus", wr$w)
    if (wr$effectif) {
      pose("wilcoxon", "variance", wr$variance); pose("wilcoxon", "statistique", wr$z)
      pose("wilcoxon", "p_asymptotique", min(1, 2 * min(stats::pnorm(wr$z), stats::pnorm(wr$z, lower.tail = FALSE))))
      if (wr$ex_aequo) pose("wilcoxon", "p_exacte", NA_real_, "ex_aequo")
      else if (wr$effectif > N_MAX_WILCOXON_EXACT) pose("wilcoxon", "p_exacte", NA_real_, "effectif_au_dela_n_max")
      else pose("wilcoxon", "p_exacte",
                min(1, 2 * min(stats::psignrank(wr$w, wr$effectif),
                               stats::psignrank(wr$w - 1, wr$effectif, lower.tail = FALSE))))
    } else for (g in c("variance", "statistique", "p_asymptotique", "p_exacte"))
      pose("wilcoxon", g, NA_real_, "aucun_residu_non_nul")
    # S9 : MAPE sur les cellules retenues où D > 0.
    if (any(Dr > 0)) pose("mape", "valeur", 100 * mean(abs(rr[Dr > 0]) / Dr[Dr > 0]))
    else pose("mape", "valeur", NA_real_, "aucun_deces")
    # S10 : R2 sur les taux.
    mh <- Dr / Er
    sc <- sum((mh - mean(mh))^2)
    if (sc > 0) pose("r2", "valeur", 1 - sum((mh - mur)^2) / sc)
    else pose("r2", "valeur", NA_real_, "variance_observee_nulle")
    # S11 : distance sur l'échelle des probabilités.
    pose("distance_probabilites", "valeur", sum(abs(-expm1(-mh) - qx[o])))
    # S12 : intervalles de fluctuation, exact et asymptotique.
    L <- stats::qpois(alpha / 2, Ar); U <- stats::qpois(alpha / 2, Ar, lower.tail = FALSE)
    la <- Ar - zq * sqrt(Ar); ua <- Ar + zq * sqrt(Ar)
    cl$fluctuation_inf_exacte[o] <- L; cl$fluctuation_sup_exacte[o] <- U
    cl$fluctuation_inf_asymptotique[o] <- la; cl$fluctuation_sup_asymptotique[o] <- ua
    pose("fluctuation", "hors_intervalle_exact", sum(Dr < L | Dr > U))
    pose("fluctuation", "esperance_hors_intervalle_exact", sum(probabilite_hors(Ar, L, U)))
    pose("fluctuation", "hors_intervalle_asymptotique", sum(Dr < la | Dr > ua))
    pose("fluctuation", "esperance_hors_intervalle_asymptotique", sum(probabilite_hors(Ar, la, ua)))
    # Monte-Carlo : un seul jeu de B réplications (p = 0), sauf relation
    # ajustee_hors_outil (aucun tirage, M254).
    if (!ajustee) {
      t_rep <- replications_poisson(Ar, replications, graine,
                                    function(Dm) statistiques_test(Dm, Ar, Er, mur, debut))
      tirages <- replications
      # Signes, suites, Wilcoxon : aucun résidu non nul observé (ns = 0) ->
      # p de Monte-Carlo et erreur NA, état aucun_residu_non_nul.
      mc <- function(s, t, tr, sens) {
        if (s %in% c("signes", "runs", "wilcoxon") && !ns) {
          pose(s, "p_monte_carlo", NA_real_, "aucun_residu_non_nul")
          pose(s, "erreur_monte_carlo", NA_real_, "aucun_residu_non_nul")
        } else {
          pe <- pvaleur_monte_carlo(t, tr, sens)
          pose(s, "p_monte_carlo", pe[["p"]]); pose(s, "erreur_monte_carlo", pe[["e"]])
        }
      }
      mc("khi2_pearson", t_obs$khi2, t_rep$khi2, "superieure")
      mc("deviance", t_obs$deviance, t_rep$deviance, "superieure")
      mc("signes", t_obs$signes, t_rep$signes, "centrale")
      mc("runs", t_obs$runs, t_rep$runs, "inferieure")
      mc("wilcoxon", t_obs$wilcoxon, t_rep$wilcoxon, "centrale")
    }
    if (ajustee) {
      for (k in which(STATUTS_VALIDATION$sous_h0 & STATUTS_VALIDATION$statistique != "sous_groupes")) {
        s <- STATUTS_VALIDATION$statistique[k]; g <- STATUTS_VALIDATION$grandeur[k]
        pose(s, g, NA_real_, "ddl_non_etabli")
      }
      cl[c("fluctuation_inf_exacte", "fluctuation_sup_exacte", "fluctuation_inf_asymptotique",
           "fluctuation_sup_asymptotique")] <- list(rep_na())
    }
  }
  sv <- STATUTS_VALIDATION[STATUTS_VALIDATION$statistique != "sous_groupes", , drop = FALSE]
  cles <- paste(sv$statistique, sv$grandeur, sep = "/")
  stat <- data.frame(statistique = sv$statistique, grandeur = sv$grandeur,
                     valeur = vapply(cles, function(k) if (is.null(val[[k]])) NA_real_ else as.numeric(val[[k]]), 0,
                                     USE.NAMES = FALSE),
                     etat = vapply(cles, function(k) if (is.null(et[[k]])) "aucune_cellule_retenue" else et[[k]], "",
                                   USE.NAMES = FALSE),
                     statut = sv$statut, stringsAsFactors = FALSE)
  if (!all(names(val) %in% cles))
    stop("statistiques_validation() : grandeur hors de STATUTS_VALIDATION (erreur de programmation).", call. = FALSE)
  list(statistiques = stat, cellules = cl, sequences = seqs, tirages_effectues = tirages)
}

# Sous-groupes (S5 ; M253, point 5) : regroupements segment x tranche,
# tranche, segment (si la segmentation n'est pas vide), année (si
# par_annee), sur les cellules retenues ; p = 0 : Poisson exact
# (smr_poisson()) ; ajustee_hors_outil : O, A et SMR seuls, bornes et
# p-valeurs NA, état ddl_non_etabli ; groupe sans cellule retenue : état
# aucune_cellule_retenue.
sous_groupes_validation <- function(cellules, segment, segments_cles, tranche, tranches, plage_ages, D, A, ret,
                                    par_annee, niveau, ajustee) {
  fin_tranche <- c(tranches[-1L] - 1, plage_ages[2L])
  n_seg <- nrow(segments_cles)
  Dr <- ifelse(ret, as.numeric(D), 0); Ar <- ifelse(ret, A, 0); nr <- as.numeric(ret)
  groupe <- function(nom, cle, n_g, seg_g, tr_g, an_g) {
    O <- vapply(seq_len(n_g), function(k) sum(Dr[cle == k]), 0)
    At <- vapply(seq_len(n_g), function(k) sum(Ar[cle == k]), 0)
    nb <- vapply(seq_len(n_g), function(k) sum(nr[cle == k]), 0)
    d <- data.frame(regroupement = rep(nom, n_g), stringsAsFactors = FALSE)
    for (k in names(segments_cles)) d[[k]] <- segments_cles[[k]][seg_g]
    d$tranche_debut <- as.integer(tranches[tr_g])
    d$tranche_fin <- as.integer(fin_tranche[tr_g])
    if (par_annee) d$annee <- as.integer(an_g)
    d$cellules_retenues <- nb
    d$observes <- O
    d$attendus <- At
    s <- smr_poisson(O, At, niveau)
    ok <- At > 0
    for (c_ in c("smr", "borne_inf", "borne_sup", "p_inferieure", "p_superieure", "p_centrale")) {
      v <- s[[c_]]; v[!ok] <- NA_real_
      if (ajustee && c_ != "smr") v[] <- NA_real_
      d[[switch(c_, borne_inf = "borne_inf_exacte", borne_sup = "borne_sup_exacte", c_)]] <- v
    }
    d$etat <- ifelse(!ok, "aucune_cellule_retenue", if (ajustee) "ddl_non_etabli" else "produite")
    d
  }
  n_tr <- length(tranches)
  na <- NA_integer_
  morceaux <- list()
  if (length(names(segments_cles))) {
    morceaux$segment_tranche <- groupe("segment_tranche", (segment - 1L) * n_tr + tranche, n_seg * n_tr,
                                       rep(seq_len(n_seg), each = n_tr), rep(seq_len(n_tr), times = n_seg), na)
  }
  morceaux$tranche <- groupe("tranche", tranche, n_tr, rep(na, n_tr), seq_len(n_tr), na)
  if (length(names(segments_cles)))
    morceaux$segment <- groupe("segment", segment, n_seg, seq_len(n_seg), rep(na, n_seg), na)
  if (par_annee) {
    an <- sort(unique(cellules$annee))
    morceaux$annee <- groupe("annee", match(cellules$annee, an), length(an), rep(na, length(an)),
                             rep(na, length(an)), an)
  }
  r <- do.call(rbind, unname(morceaux))
  rownames(r) <- NULL
  r
}

# Étape `validation` du plan (#4).
#   exposition       : table de l'étape `exposition`
#                      (etapes$exposition$resultat$exposition) ;
#   segmentation     : clés de segmentation (champ du plan) ;
#   par_annee        : découpage par année de l'étape `exposition` ;
#   convention       : convention d'âge retenue par l'étape `exposition` ;
#   table            : objet table_reference de la clé citée (argument
#                      tables_reference de construire_table()) ;
#   table_reference, relation_table, plage_ages, tranches_age, niveau,
#   replications     : paramètres de l'étape ;
#   graine           : graine du plan (étape aléatoire, ADR 0015, point 7.2) ;
#   controler        : TRUE (appel direct) contrôle la segmentation, les
#                      paramètres, la classe de la table et les entrées ;
#                      FALSE, réservé à construire_table() qui les a
#                      contrôlés, n'en refait aucun (#22).
# Rend list(parametres, resultat (cellules, statistiques, sous_groupes,
# sequences_runs, monte_carlo), diagnostics, avertissements). Aucune copie de
# la table (ADR 0015, point 5).
calculer_validation <- function(exposition, segmentation, par_annee, convention, table, table_reference,
                                relation_table, plage_ages, tranches_age, niveau, replications, graine,
                                controler = TRUE) {
  if (!isFALSE(controler)) {
    controler_segmentation(segmentation, "validation")
    controler_parametres_validation(table_reference, relation_table, plage_ages, tranches_age, niveau, replications)
    if (!identical(attr(table, "class", exact = TRUE), c("table_reference", "data.frame")))
      stop("calculer_validation() : `table` doit être un objet table_reference (erreur de programmation).", call. = FALSE)
    controler_entrees_validation(table, table_reference, convention, segmentation, plage_ages)
  }
  manquantes <- setdiff(c(segmentation, "age", if (isTRUE(par_annee)) "annee", "evenements", "exposition_centrale"),
                        names(exposition))
  if (length(manquantes))
    stop(sprintf("calculer_validation() : colonne(s) %s absente(s) de la table d'exposition (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  pe <- RELATIONS_TABLE[[relation_table]]
  ap <- apparier_table(exposition, segmentation, par_annee, table, plage_ages, table_reference)
  cel <- ap$cellules
  at <- attendus_cellules(cel$exposition_centrale, cel$evenements, cel$qx_table, table_reference)
  ret <- at$etat == "retenue"
  # Séquence des suites : segment x année.
  sequence <- if (isTRUE(par_annee)) (ap$segment - 1L) * length(unique(cel$annee)) + match(cel$annee, sort(unique(cel$annee)))
              else ap$segment
  s <- statistiques_validation(cel$evenements, cel$exposition_centrale, cel$qx_table, at$taux_table, at$attendus,
                               at$etat, sequence, cel$age, niveau, pe, replications, graine)
  tranche <- findInterval(cel$age, tranches_age)
  cel$taux_table <- at$taux_table
  for (k in names(s$cellules)) cel[[k]] <- s$cellules[[k]]
  cel$tranche_age <- as.integer(tranches_age[tranche])
  cel$etat <- at$etat
  segments_cles <- cel[!duplicated(ap$segment), segmentation, drop = FALSE]
  rownames(segments_cles) <- NULL
  sg <- sous_groupes_validation(cel, ap$segment, segments_cles, tranche, tranches_age, plage_ages, cel$evenements,
                                at$attendus, ret, isTRUE(par_annee), niveau, is.na(pe))
  # Séquences : clés du segment (et année).
  seq_cles <- cel[match(s$sequences$sequence, sequence), c(segmentation, if (isTRUE(par_annee)) "annee"), drop = FALSE]
  rownames(seq_cles) <- NULL
  sequences_runs <- cbind(seq_cles, s$sequences[c("positifs", "negatifs", "runs")])

  # Décomptes et identité des événements (M253, point 5).
  O <- sum(as.numeric(cel$evenements[ret]))
  sans_attendu <- at$etat %in% c("evenement_sans_exposition", "evenement_sous_taux_nul")
  ev_sans <- sum(as.numeric(cel$evenements[sans_attendu]))
  total <- sum(as.numeric(exposition$evenements))
  if (!identical(O + ev_sans + ap$hors_plage$evenements, total))
    stop("calculer_validation() : décomptes d'événements incohérents (erreur de programmation).", call. = FALSE)
  avert <- avertissements_vides()
  if (any(sans_attendu))
    avert <- avertir(avert, "validation", "evenements_sans_attendu",
                     sprintf(paste("Validation : %d cellule(s) qui comptent %d événement(s) sans attendu (exposition",
                                   "nulle, ou q_x de la table nul) : statistiques non définies dans ces cellules (états",
                                   "evenement_sans_exposition, evenement_sous_taux_nul) ; événements restitués à part,",
                                   "non comptés dans O."),
                             sum(sans_attendu), as.integer(ev_sans)),
                     sum(sans_attendu))
  etats <- data.frame(etat = ETATS_CELLULE_VALIDATION,
                      cellules = vapply(ETATS_CELLULE_VALIDATION, function(e) sum(at$etat == e), 0L, USE.NAMES = FALSE),
                      evenements = vapply(ETATS_CELLULE_VALIDATION, function(e)
                        sum(as.numeric(cel$evenements[at$etat == e])), 0, USE.NAMES = FALSE),
                      stringsAsFactors = FALSE)
  parametres <- list(table_reference = table_reference, table_nom = attr(table, "nom", exact = TRUE),
                     relation_table = relation_table, parametres_estimes = pe, plage_ages = plage_ages,
                     tranches_age = tranches_age, niveau = niveau, replications = replications,
                     convention = convention, appariement_sexe = ap$appariement_sexe,
                     n_max_wilcoxon_exact = N_MAX_WILCOXON_EXACT, n_max_runs_exact = N_MAX_RUNS_EXACT,
                     hypothese_nulle = HYPOTHESE_VALIDATION, reference_methode = REFERENCE_METHODE_VALIDATION,
                     statuts_sous_groupes = texte_moteur(STATUTS_VALIDATION[STATUTS_VALIDATION$statistique == "sous_groupes",
                                                                            c("grandeur", "statut")]))
  rownames(parametres$statuts_sous_groupes) <- NULL
  list(parametres = parametres,
       resultat = list(cellules = cel, statistiques = texte_moteur(s$statistiques), sous_groupes = sg,
                       sequences_runs = sequences_runs,
                       monte_carlo = list(replications = replications, tirages_effectues = s$tirages_effectues,
                                          graine = graine)),
       diagnostics = list(cellules = nrow(cel), cellules_retenues = sum(ret), etats = etats,
                          evenements_sans_attendu = ev_sans, cellules_hors_plage = ap$hors_plage$cellules,
                          evenements_hors_plage = ap$hors_plage$evenements,
                          exposition_hors_plage = ap$hors_plage$exposition, evenements_total = total),
       avertissements = avert)
}
