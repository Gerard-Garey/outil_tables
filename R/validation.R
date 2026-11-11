# -----------------------------------------------------------------------------
#  R/validation.R : module `validation`, statistiques de validation d'une table
#  de référence prise telle quelle (p = 0) (issue #4 ; spécification
#  d'actuary-approfondi décidée, M217 ; avis de forme (0''), M242 à M248 ;
#  six points d'actuary-approfondi, M253 à M258 ; ADR 0015, point 7 et
#  seconde annotation du 06/11/2026 ; ADR 0007 et ADR 0012, secondes
#  annotations du 06/11/2026) ou de la table positionnée de la construction
#  (p = 1 par segment, relation `positionnee` ; #167, spécification
#  d'actuary-approfondi, § 8, M289 ; avis de forme (0''), F3, F5, F9, M292) ;
#  étape `homogeneite`, test d'homogénéité entre segments (#168 ; avis de
#  forme (0''), F10, F11)
#
#  - controler_tranches_age(), controler_replications() : forme des tranches
#    d'âge et du nombre de réplications, sous le module de l'étape appelante
#    (`validation` ou `homogeneite`) ;
#  - controler_parametres_validation() : paramètres de l'étape (clé de table,
#    relation table / observations, plage d'âges, tranches, niveau,
#    réplications) ; appelée par valider_plan() seule (#82) ;
#  - statistiques_test() : statistiques des tests de Monte-Carlo (X2, G2,
#    signes, runs, Wilcoxon) sur une matrice de décès (une colonne par jeu) :
#    observé et répliques passent par la même fonction ;
#  - loi_runs(), wilcoxon_rangs(), probabilite_hors(),
#    probabilite_hors_binomiale() : briques exactes et asymptotiques ;
#  - statuts_validation() : statuts des grandeurs selon la relation
#    (STATUTS_VALIDATION, STATUTS_VALIDATION_POSITIONNEE) ;
#  - replications_poisson() et replications_multinomiales() (sous
#    sous_graine()), pvaleur_monte_carlo() ;
#  - loi_somme_binomiales() : loi d'une somme de binomiales indépendantes
#    (convolution, brique partagée avec #168, F3) ;
#  - p_somme_binomiales_normale() : p-valeurs asymptotiques de cette somme
#    (approximation normale, correction de continuité ; #178) ;
#  - statistiques_validation() : S1 à S12 sur les cellules (cœur partagé avec
#    #69 et le futur suivre_experience(), ADR 0015, point 7.5) ;
#  - sous_groupes_validation(), sous_groupes_positionnee() : S5 par
#    regroupement ;
#  - calculer_validation() : fonction d'étape `validation` ;
#  - controler_parametres_homogeneite() : paramètres de l'étape
#    `homogeneite` ; appelée par valider_plan() seule ;
#  - somme_par(), score_homogeneite(), p_exacte_homogeneite() : tests du
#    score conditionnels aux décès de chaque strate (U, V, ddl, statistique
#    des répliques), p-valeur exacte à deux groupes ;
#  - calculer_homogeneite() : fonction d'étape `homogeneite` (#168) ;
#  - classe_motifs(), classer_lignes_exclues() : classes des motifs et des
#    lignes exclues (mesurable, non calculable faute de clé, non mesurable),
#    seule source des ensembles S (#69) ;
#  - retrait_contrefactuel() : événements et exposition retirés par
#    différence contrefactuelle (construire_observations(),
#    evaluer_motifs(lignes_tete), calculer_exposition()) ;
#  - smr_comparateur_1(), smr_comparateur_2() : SMR des lignes exclues ;
#  - calculer_importance_exclusions() : fonction d'étape
#    `importance_exclusions` (#69).
#
#  Notations (M217) : D = evenements ; E^c = exposition_centrale ; q~ = q_x
#  de la table ; mu~ = -log1p(-q~) ; A = E^c mu~ (A = 0 si E^c = 0, même pour
#  q~ = 1) ; C+ = {A > 0} ; r = D - A ; mu^ = D / E^c (taux_poisson(), module
#  brut) ; alpha = 1 - niveau. H0 (i) : D_c ~ Poisson(A_c) indépendants ;
#  exacte avec E^c fixe, asymptotique sous le modèle de survie. p = 0
#  (`independante`) ; `ajustee_hors_outil` : p non établi, aucune grandeur
#  sous H0 produite, aucun tirage (M254) ; `positionnee` : table positionnée
#  de l'étape `positionnement` de la même construction, A_c = theta^_s E^c
#  mu~_c (force de la référence multipliée par theta^_s, jamais relue dans
#  table_positionnee$qx), p = 1 par segment positionné, ddl = n - S, lois
#  sous H0 prises conditionnellement aux décès O_s de chaque segment
#  (D_c | O_s ~ Bin(O_s, pi_c), pi_c = A_c / A_s ; multinomiale (O_s ; pi_c)
#  pour les réplications), libres de theta. Aucun verdict, aucun seuil (M217,
#  Q4). Formules sans attribution d'auteur (M200).
#  controler_entrees_validation(), apparier_table(), attendus_cellules() et
#  smr_poisson() sont dans R/positionnement.R (#167, F1) ;
#  CONVENTIONS_VALIDATION et controler_plage_ages() dans R/exposition.R,
#  valeurs_entieres_double() dans R/outils.R (ADR 0017, point 10.1).
#  Ce module n'appelle que `outils` (dont sous_graine(), par
#  replications_poisson() et replications_multinomiales() ;
#  valeurs_entieres_double()), `donnees` (liste_guillemets() ; pour #69 :
#  construire_observations(), evaluer_motifs(), MOTIFS_TETE, MODALITES_CAUSE),
#  `exposition` (calculer_exposition(), #69 ; controler_plage_ages()), `brut`
#  (taux_poisson()) et `positionnement` (controler_cle_table(),
#  apparier_table(), attendus_cellules(), smr_poisson()) ; le
#  niveau est contrôlé ici sous le code niveau_invalide de `brut` (M253)
#  (ADR 0007, point 2.1 et seconde annotation du 06/11/2026, (d)). Les
#  arêtes vers donnees::table_reference relevées par graphe_modules.R sont
#  des homonymies de l'argument `table_reference` (clé de la table).
# -----------------------------------------------------------------------------

# Relation entre la table et les observations (ADR 0015, point 7.1 ; M237,
# M253 ; #167, F5, M289) -> nombre de paramètres estimés sur ces données : 0
# (table prise telle quelle), NA (non établi, ddl_non_etabli) ou 1 par
# segment positionné (table positionnée de la construction : ddl = n - S).
RELATIONS_TABLE <- c(independante = 0L, ajustee_hors_outil = NA_integer_, positionnee = 1L)
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
# Nombre maximal de produits de la convolution directe de
# loi_somme_binomiales() (relation positionnee, sous-groupes de plusieurs
# segments ; F3, M185 ; audit A2) : coût calculé avant la convolution par
# cout_convolution_binomiales() ; au-delà, p-valeurs exactes NA, p-valeurs
# asymptotiques par p_somme_binomiales_normale() (convolution_au_dela_n_max ;
# #178). Mesures (hôte cloud Linux, R 4.3.3,
# 07/11/2026 ; débit d'environ 1,1 à 1,4e8 produits/s) :
#   segments x n_s, pi                        produits        durée
#   2 x 50 000, 0,5                           73 599 241      0,61 s
#   3 x (20 000, 30 000, 10 000), (0,2 ; 0,7 ; 0,4)
#                                             64 905 001      0,52 s
#   2 x 100 000, 0,5                          147 452 449     1,28 s
#   8 x 12 500, 0,5                           507 804 829     3,79 s
N_MAX_PRODUITS_CONVOLUTION <- 1e8
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
# Hypothèse nulle sous la relation positionnee (spécification de #167, § 8).
HYPOTHESE_VALIDATION_POSITIONNEE <- texte_moteur(paste(
  "H0 (i) : D_c ~ Poisson(theta_s A~_c) indépendants, A~_c = E^c_c mu~_c, mu~_c = -ln(1 - q~_c), theta_s inconnu,",
  "un par segment, estimé par theta^_s = O_s / A~_s (étape positionnement) ; attendus de la table positionnée",
  "A_c = theta^_s A~_c ; conditionnellement à O_s, statistique exhaustive de theta_s, (D_c) suit la loi",
  "multinomiale (O_s ; pi_c), pi_c = A~_c / A~_s (= A_c / A_s), libre de theta_s ; exacte avec E^c tenue pour fixe,",
  "asymptotique sous le modèle de survie (E^c aléatoire) ; unités d'observation indépendantes (l'unité contrat",
  "rend les tests anti-conservateurs) ; cellule [x, x+1) de la convention dernier_anniversaire."))

# États d'une cellule (M253, point 1) : retenue (A > 0, dans C+) ;
# sans_exposition (E^c = 0, D = 0) ; taux_table_nul (A = 0 avec E^c > 0, D =
# 0) ; evenement_sans_exposition (E^c = 0, D > 0) ; evenement_sous_taux_nul
# (A = 0 avec E^c > 0, D > 0). A = 0 avec E^c > 0 : q~ = 0, ou produit
# E^c mu~ nul en double.
ETATS_CELLULE_VALIDATION <- c("retenue", "sans_exposition", "taux_table_nul", "evenement_sans_exposition",
                              "evenement_sous_taux_nul")
# États d'une grandeur (valeur NA hors `produite`).
# contraint_par_positionnement : grandeur sous H0 du SMR global sous la
# relation positionnee (SMR = 1 par construction, non produite), et toute
# grandeur de test sous positionnee quand ddl = n - S = 0 (une cellule
# retenue par segment : D_c = O_s, loi conditionnelle dégénérée ; audit A1).
ETATS_GRANDEUR_VALIDATION <- c("produite", "ddl_non_etabli", "aucune_cellule_retenue", "aucun_residu_non_nul",
                               "variance_nulle", "ex_aequo", "effectif_au_dela_n_max", "aucun_deces",
                               "variance_observee_nulle", "contraint_par_positionnement")
# États d'une ligne de resultat$sous_groupes : produite ; ddl_non_etabli
# (ajustee_hors_outil) ; aucune_cellule_retenue ; sous la relation
# positionnee : contraint_par_positionnement (regroupement segment, et tout
# sous-groupe qui contient, pour chaque segment qu'il touche, toutes les
# cellules retenues de ce segment : O_b déterministe sachant les O_s ; SMR
# descriptif, bornes et p-valeurs NA ; C1), bornes_non_definies (sous-groupe
# de plusieurs segments : p-valeurs produites, bornes NA),
# convolution_au_dela_n_max (sous-groupe de plusieurs segments, coût de la
# convolution au-delà de N_MAX_PRODUITS_CONVOLUTION : p-valeurs exactes et
# bornes NA, p-valeurs asymptotiques produites à la place des exactes ;
# #178). Les colonnes p_*_asymptotique sont NA dans tout autre état.
ETATS_SOUS_GROUPES_VALIDATION <- c("produite", "ddl_non_etabli", "aucune_cellule_retenue",
                                   "contraint_par_positionnement", "bornes_non_definies", "convolution_au_dela_n_max")

# Phrases des statuts des p asymptotiques sur attendus faibles (#179, #200),
# posées une fois pour STATUTS_VALIDATION et STATUTS_VALIDATION_POSITIONNEE :
# petits (réserve sur les attendus A_c petits), fin_x2 (niveau réel de la p
# de X2), g2 (espérance de la contribution d'une cellule à G2). Chaînes non
# marquées, comme leurs copies d'origine : le texte_moteur() de chaque table
# les marque après le paste (même parcours d'octets sous la locale C, #50).
PHRASES_ATTENDUS_FAIBLES <- list(
  petits = "approximation non garantie quand des attendus A_c sont petits",
  fin_x2 = paste("le niveau réel du test fondé sur cette p n'est pas établi en général et dépasse le niveau nominal",
                 "dans les constats de simulation du projet ; seule p_monte_carlo a un niveau garanti sous H0 (i)"),
  g2 = paste("l'espérance de la contribution d'une cellule est inférieure à 1 pour A_c inférieur à 0,49 environ et",
             "supérieure à 1 au-delà (maximum voisin de 1,16), par sommation des probabilités (approximation",
             "numérique) : la p asymptotique est trop grande ou trop petite selon la répartition des A_c ; seule",
             "p_monte_carlo a un niveau garanti sous H0 (i)"))

# Statuts (E2.20) des grandeurs de resultat$statistiques, une ligne par
# (statistique, grandeur), dans l'ordre du tableau ; sous_h0 : grandeur
# définie par H0 (p-valeur, intervalle, espérance ou variance sous H0,
# statistique standardisée, ddl), non produite sous `ajustee_hors_outil`
# (M254) ; les lignes `sous_groupes` décrivent resultat$sous_groupes.
# Textes marqués UTF-8 (texte_moteur(), comme STATUTS_POSITIONNEMENT).
STATUTS_VALIDATION <- local({
  desc <- "descriptif : calcul direct sur les cellules retenues (C+), valeur en arithmétique flottante"
  exact_h0 <- paste("exact sous H0 (i), E^c tenue pour fixe ; asymptotique sous le modèle de survie ;",
                    "fonctions de répartition de Poisson (ppois), approximation numérique")
  chi2 <- "asymptotique : loi du khi-deux à n - p degrés de liberté (A_c -> infini dans chaque cellule retenue), p = 0"
  # Attendus faibles (#179 ; spécification (0'), M359, M360) : avertissement
  # sans seuil, chiffré par khi2_pearson/variance_h0 ; phrases posées une
  # fois (PHRASES_ATTENDUS_FAIBLES), communes aux deux tables de statuts.
  petits <- PHRASES_ATTENDUS_FAIBLES$petits
  fin_x2 <- PHRASES_ATTENDUS_FAIBLES$fin_x2
  g2 <- PHRASES_ATTENDUS_FAIBLES$g2
  mc <- paste("niveau exact sous H0 (i), E^c tenue pour fixe : P_H0(p_MC <= alpha) <= alpha ; e est l'erreur due",
              "au nombre fini de réplications, distincte de l'erreur d'estimation")
  e_mc <- paste("erreur de Monte-Carlo e = sqrt(B p_MC (1 - p_MC)) / (B + 1), due au nombre fini de",
                "réplications, distincte de l'erreur d'estimation")
  # Relation ajustee_hors_outil des p-valeurs de sous-groupe (#178, #200) :
  # fragment commun à sous_groupes/p_valeurs et p_valeurs_asymptotiques.
  ajustee_p <- paste("relation ajustee_hors_outil : non produites, p non établi, aucune p-valeur du sous-groupe",
                     "(ddl_non_etabli) ; colonnes NA")
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
    c("khi2_pearson", "variance_h0", "TRUE",
      paste("exact sous H0 (i), E^c tenue pour fixe ; asymptotique sous le modèle de survie : Var(X2) = somme sur C+",
            "de (2 + 1/A_c) (moments de Poisson d'ordre 4), à comparer à 2 (n - p), variance de la loi de référence",
            "de khi2_pearson/p_asymptotique (les espérances sont égales) ; arithmétique flottante")),
    c("khi2_pearson", "p_asymptotique", "TRUE",
      paste(chi2, "; queue supérieure ;", petits, ": sous H0 (i), E^c tenue pour fixe, E[X2] = n - p exactement mais",
            "Var(X2) = somme sur C+ de (2 + 1/A_c) > 2 (n - p) (moments de Poisson, résultat exact ;",
            "khi2_pearson/variance_h0) ;", fin_x2)),
    c("khi2_pearson", "p_monte_carlo", "TRUE", paste(mc, "; queue supérieure")),
    c("khi2_pearson", "erreur_monte_carlo", "TRUE", e_mc),
    c("deviance", "statistique", "FALSE",
      "descriptif : G2 = 2 somme sur C+ de [D ln(D / A) - (D - A)] (0 ln 0 = 0), rapport de vraisemblance"),
    c("deviance", "ddl", "TRUE", "n - p : n cellules retenues, p paramètres estimés sur ces données (0)"),
    c("deviance", "p_asymptotique", "TRUE",
      paste(chi2, "; queue supérieure ;", petits, ": sous H0 (i), E^c tenue pour fixe,", g2)),
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
      paste("relation independante : exactes sous la loi supposée (O ~ Poisson, A fixe), p = 0 :",
            "[G_O^-1(alpha/2), G_(O+1)^-1(1 - alpha/2)] / A, G_k loi gamma de forme k (0 si O = 0) ; couverture >=",
            "niveau (intervalle exact conservatif) ; qgamma, approximation numérique ; relation ajustee_hors_outil :",
            "non produites, p non établi, aucun intervalle du sous-groupe (ddl_non_etabli) ; colonnes NA")),
    c("sous_groupes", "p_valeurs", "TRUE",
      paste("relation independante :", exact_h0, "; p = 0 : X ~ Poisson(A) ; aucune correction de multiplicité ;",
            ajustee_p)),
    c("sous_groupes", "p_valeurs_asymptotiques", "TRUE",
      paste("relation independante : non produites, p = 0, la loi de Poisson exacte du sous-groupe est toujours",
            "calculée ;", ajustee_p)))
  texte_moteur(data.frame(statistique = vapply(l, `[`, "", 1L), grandeur = vapply(l, `[`, "", 2L),
                          sous_h0 = vapply(l, `[`, "", 3L) == "TRUE", statut = vapply(l, `[`, "", 4L),
                          stringsAsFactors = FALSE))
})

# Statuts sous la relation positionnee (#167, spécification, § 8 ; F5 : les
# statuts « (0) » dépendent de la relation) : mêmes lignes (statistique,
# grandeur, sous_h0) que STATUTS_VALIDATION, statut remplacé là où la loi,
# les degrés de liberté ou la production changent ; A_c désigne les attendus
# de la table positionnée, A_c = theta^_s E^c mu~_c. Textes marqués UTF-8
# (texte_moteur()).
STATUTS_VALIDATION_POSITIONNEE <- local({
  s <- STATUTS_VALIDATION
  indep <- paste("; unités d'observation indépendantes ; sous l'unité contrat, les décès sont sur-dispersés et les",
                 "tests sont anti-conservateurs")
  cond <- paste("exact sous H0 (i) conditionnellement aux décès observés O_s de chaque segment, E^c tenue pour fixe,",
                "quel que soit theta : D_c | O_s ~ Bin(O_s, pi_c), pi_c = A_c / A_s ; asymptotique sous le modèle de",
                "survie ; fonctions de répartition binomiales (pbinom), approximation numérique", indep)
  # Corps de la loi de référence (#179, spécification (0') et visa
  # d'actuary) : X2 et G2 ont le même corps, « (A_c -> infini dans chaque
  # cellule retenue) » ; seul G2 précise que ses seuils sont calculés sous
  # Poisson, sans conditionner par les O_s ; les deux finissent par `indep`.
  loi_s <- "asymptotique : loi du khi-deux à n - S degrés de liberté"
  theta_s <- "theta estimé par maximum de vraisemblance sur ces cellules, un par segment positionné (S segments)"
  petits <- PHRASES_ATTENDUS_FAIBLES$petits
  fin_x2 <- PHRASES_ATTENDUS_FAIBLES$fin_x2
  g2 <- PHRASES_ATTENDUS_FAIBLES$g2
  var_s <- paste("Var(X2) = somme sur les segments de [2 (n_s - 1) + somme sur C+_s de 1/A_c - (n_s^2 + 2 n_s - 2) /",
                 "O_s]")
  mc <- paste("niveau exact sous H0 (i) conditionnellement aux O_s, E^c tenue pour fixe, quel que soit theta :",
              "P_H0(p_MC <= alpha | O_s) <= alpha ; réplications multinomiales (O_s ; pi_c) par segment ; e est",
              "l'erreur due au nombre fini de réplications, distincte de l'erreur d'estimation", indep)
  ddl <- paste("n - S : n cellules retenues, p = 1 paramètre (theta) estimé sur ces données par segment positionné,",
               "S segments ; si n - S = 0 (une cellule retenue par segment) : loi conditionnelle dégénérée,",
               "D_c = O_s ; aucun test défini, aucun tirage (statistiques standardisées, moments des suites et",
               "variance de Wilcoxon, p-valeurs asymptotiques, exactes et de Monte-Carlo, erreurs de Monte-Carlo :",
               "état contraint_par_positionnement)")
  non <- paste("non produit : le SMR de la table positionnée vaut 1 par construction (theta estimé sur ces",
               "cellules) ; état contraint_par_positionnement")
  # nc est collé à un statut de STATUTS_VALIDATION, marqué UTF-8 : marqué
  # aussi, sinon paste0() le traduirait depuis le codage natif (installation
  # sous la locale C).
  nc <- texte_moteur(" ; table positionnée sur ces données : loi non corrigée de l'estimation de theta")
  remplacer <- function(stat, gr, texte) {
    k <- which(s$statistique == stat & s$grandeur == gr)
    if (length(k) != 1L) stop("STATUTS_VALIDATION_POSITIONNEE : ligne inconnue (erreur de programmation).", call. = FALSE)
    s$statut[k] <<- texte
  }
  completer <- function(stat, gr) remplacer(stat, gr, paste0(s$statut[s$statistique == stat & s$grandeur == gr], nc))
  for (k in 2:3)
    remplacer("residus", paste0("esperance_au_dela_", k),
              paste0(cond, " ; somme sur C+ de P(|D_c - A_c| > ", k, " sqrt(A_c))"))
  for (st in c("khi2_pearson", "deviance")) {
    remplacer(st, "ddl", ddl)
    remplacer(st, "p_monte_carlo", paste(mc, "; queue supérieure"))
  }
  remplacer("khi2_pearson", "variance_h0",
            paste("exact sous H0 (i) conditionnellement aux O_s, E^c tenue pour fixe, quel que soit theta ;",
                  "asymptotique sous le modèle de survie : Var(X2 | O_s) = somme sur les segments de [2 (n_s - 1) +",
                  "somme sur C+_s de 1/A_c - (n_s^2 + 2 n_s - 2) / O_s], A_c = O_s pi_c (moments multinomiaux",
                  "d'ordre 4), à comparer à 2 (n - S) ; arithmétique flottante", indep))
  remplacer("khi2_pearson", "p_asymptotique",
            paste0(loi_s, " (A_c -> infini dans chaque cellule retenue), ", theta_s, " ; queue supérieure ; ", petits,
                   " : sous H0 (i) conditionnellement aux O_s, E^c tenue pour fixe, E[X2] = n - S exactement et ",
                   var_s, " (moments multinomiaux, résultat exact ; khi2_pearson/variance_h0), supérieure à 2 (n - S)",
                   " quand des A_c sont petits devant ceux de leur segment, inférieure quand les A_c d'un segment",
                   " sont égaux ; ", fin_x2, " ", indep))
  remplacer("deviance", "p_asymptotique",
            paste0(loi_s, " (A_c -> infini dans chaque cellule retenue), ", theta_s, " ; queue supérieure ; ", petits,
                   " : sous H0 (i), E^c tenue pour fixe, pour des cellules de loi de Poisson d'espérance A_c, sans",
                   " conditionner par les O_s (conditionnellement aux O_s, D_c suit une loi binomiale et les valeurs",
                   " ci-après ne valent qu'approximativement, quand pi_c est petit), ", g2, " ", indep))
  remplacer("smr", "attendus",
            "descriptif : A = somme de A_c sur C+, A_c = theta^_s E^c mu~_c ; égal à O par construction, à l'arrondi près")
  remplacer("smr", "smr", "descriptif : O / A, égal à 1 par construction, à l'arrondi près")
  remplacer("smr", "ecart", "descriptif : O - A, nul par construction, à l'arrondi près")
  for (g in c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure", "p_centrale", "z_racine_cubique",
              "p_racine_cubique"))
    remplacer("smr", g, non)
  for (g in c("statistique", "p_asymptotique", "p_exacte")) completer("signes", g)
  remplacer("signes", "esperance_positifs_h0",
            paste(cond, ": somme sur C+ de P(D_c - A_c > 1e-10 A_c), diagnostic de l'écart entre H0 (i) et",
                  "P(r_c > 0) = 1/2 par cellule, ce qui n'est pas H0 (i)"))
  remplacer("signes", "p_monte_carlo",
            paste(mc, "; p centrale sur (n+ - n-) / sqrt(n), valeur 0 si n = 0 pour une réplique ; NA si n = 0 pour",
                  "l'observé"))
  for (g in c("esperance", "variance", "statistique", "p_asymptotique", "p_exacte")) completer("runs", g)
  remplacer("runs", "p_monte_carlo",
            paste(mc, "; queue inférieure sur R ; statistique R non conditionnelle à (n+, n-) : sensible au",
                  "déséquilibre des signes comme au regroupement"))
  for (g in c("variance", "statistique", "p_asymptotique", "p_exacte")) completer("wilcoxon", g)
  remplacer("wilcoxon", "p_monte_carlo",
            paste(mc, "; p centrale sur z, z = 0 si n = 0 pour une réplique ; NA si n = 0 pour l'observé"))
  # Différences, R2 et distance sur la table positionnée (mu_pos = theta^_s
  # mu~, q_pos = 1 - exp(-mu_pos) ; M309), non sur la référence.
  zero <- "; zéro : |D - A| <= 1e-10 A"
  remplacer("wilcoxon", "effectif",
            paste("descriptif : n = nombre de cellules retenues où la différence d = D / E^c - mu_pos (= mu^ - mu_pos),",
                  "mu_pos = theta^_s mu~ (force de la table positionnée), est non nulle", zero))
  remplacer("wilcoxon", "groupes_ex_aequo", "descriptif : nombre de groupes de |mu^ - mu_pos| égaux (taille >= 2)")
  remplacer("wilcoxon", "w_plus", "descriptif : somme des rangs moyens de |mu^ - mu_pos| des différences positives")
  remplacer("r2", "valeur",
            paste("descriptif, fragile : 1 - somme (mu^ - mu_pos)^2 / somme (mu^ - moyenne de mu^)^2 sur C+,",
                  "mu_pos = theta^_s mu~ (force de la table positionnée) ; peut être négatif"))
  remplacer("distance_probabilites", "valeur",
            paste("descriptif : somme sur C+ de |q^ - q_pos|, q^ = 1 - exp(-mu^), q_pos = 1 - exp(-theta^_s mu~)",
                  "(table positionnée ; échelle des probabilités)"))
  remplacer("fluctuation", "hors_intervalle_exact",
            "nombre de cellules retenues où D est hors de [qbinom(alpha/2, O_s, pi_c), qbinom(1 - alpha/2, O_s, pi_c)]")
  remplacer("fluctuation", "esperance_hors_intervalle_exact", paste(cond, ": somme sur C+ de P(D_c hors intervalle)"))
  remplacer("fluctuation", "hors_intervalle_asymptotique",
            paste("nombre de cellules retenues où D est hors de A +- z sqrt(A (1 - pi_c)), z = qnorm(alpha/2,",
                  "lower.tail = FALSE)"))
  remplacer("fluctuation", "esperance_hors_intervalle_asymptotique",
            paste(cond, ": somme sur C+ de P(D_c hors de A +- z sqrt(A (1 - pi_c)))"))
  deg <- paste("non produites pour le regroupement segment ni pour un sous-groupe qui contient, pour chaque segment",
               "qu'il touche, toutes les cellules retenues de ce segment (O_b déterministe sachant les O_s : état",
               "contraint_par_positionnement)")
  remplacer("sous_groupes", "smr",
            paste("descriptif : O / A du sous-groupe (cellules retenues), A attendus de la table positionnée ;",
                  "regroupement segment : 1 par construction (état contraint_par_positionnement)"))
  remplacer("sous_groupes", "bornes_exactes",
            paste("sous-groupe d'un seul segment : exactes conditionnellement à O_s, couverture >= niveau (intervalle",
                  "exact conservatif) de la probabilité conditionnelle qu'un décès du segment tombe dans le",
                  "sous-groupe, divisée par pi_b = A_b / A_s : intervalle exact binomial (qbeta) divisé par pi_b ;",
                  "qbeta, approximation numérique ; non produites pour un sous-groupe de plusieurs segments (état",
                  "bornes_non_definies) ;", deg, indep))
  remplacer("sous_groupes", "p_valeurs",
            paste("exactes conditionnellement aux O_s : O_b | O_s ~ Bin(O_s, pi_b) pour un sous-groupe d'un seul",
                  "segment (pbinom) ; somme de binomiales indépendantes Bin(O_s, pi_b,s) pour un sous-groupe de",
                  "plusieurs segments (convolution des dbinom, approximation numérique ; produites si le nombre de",
                  "produits de la convolution est au plus N_MAX_PRODUITS_CONVOLUTION, état",
                  "convolution_au_dela_n_max sinon, p-valeurs asymptotiques produites à leur place) ;", deg,
                  "; aucune correction de multiplicité", indep))
  # p asymptotiques au-delà du plafond (#178 ; spécification (0'), M361).
  remplacer("sous_groupes", "p_valeurs_asymptotiques",
            paste("asymptotiques (sigma^2 -> infini) conditionnellement aux O_s : approximation normale de la somme",
                  "de binomiales indépendantes Bin(O_s, pi_b,s) d'un sous-groupe de plusieurs segments, moyenne somme",
                  "de O_s pi_b,s, variance sigma^2 = somme de O_s pi_b,s (1 - pi_b,s), correction de continuité",
                  "(P(O_b <= o) par Phi((o + 1/2 - moyenne) / sigma), P(O_b >= o) par 1 - Phi((o - 1/2 - moyenne) /",
                  "sigma)), p centrale min(1, 2 min) ; produites seulement si le nombre de produits de la convolution",
                  "dépasse N_MAX_PRODUITS_CONVOLUTION (état convolution_au_dela_n_max), les p-valeurs exactes étant",
                  "alors NA ; jamais produites à côté d'une p-valeur exacte ; erreur relative croissante dans les",
                  "queues (asymétrie de la somme, d'ordre 1/sigma) ; aucune correction de multiplicité", indep))
  texte_moteur(s)
})

# Statuts des grandeurs selon la relation (switch explicite, F5).
statuts_validation <- function(relation_table)
  switch(relation_table, independante = , ajustee_hors_outil = STATUTS_VALIDATION,
         positionnee = STATUTS_VALIDATION_POSITIONNEE,
         stop("statuts_validation() : relation inconnue (erreur de programmation).", call. = FALSE))

# Forme des tranches d'âge (tranches_age_invalides) et du nombre de
# réplications (replications_invalides), sous le module de l'étape `etape`
# (`validation` ou `homogeneite`, #168 ; extraites de
# controler_parametres_validation(), messages inchangés). plage_ages est déjà
# contrôlée (controler_plage_ages()). Rendent TRUE, invisible.
ETAPES_TRANCHES_REPLICATIONS <- c("validation", "homogeneite")
controler_tranches_age <- function(tranches_age, plage_ages, etape) {
  if (!(etape %in% ETAPES_TRANCHES_REPLICATIONS))
    stop("controler_tranches_age() : étape inconnue (erreur de programmation).", call. = FALSE)
  if (!valeurs_entieres_double(tranches_age) || !length(tranches_age) || is.unsorted(tranches_age, strictly = TRUE) ||
      tranches_age[1L] != plage_ages[1L] || tranches_age[length(tranches_age)] > plage_ages[2L])
    refuser(etape, "tranches_age_invalides",
            formuler(paste("Les tranches d'âge (tranches_age) doivent être les bornes inférieures, vecteur double sans",
                           "attribut de valeurs entières strictement croissantes, la première égale au minimum de",
                           "plage_ages, la dernière au plus égale à son maximum (un integer est refusé).")),
            list(tranches_age = tranches_age))
  invisible(TRUE)
}
controler_replications <- function(replications, etape) {
  if (!(etape %in% ETAPES_TRANCHES_REPLICATIONS))
    stop("controler_replications() : étape inconnue (erreur de programmation).", call. = FALSE)
  if (!valeurs_entieres_double(replications) || length(replications) != 1L || replications < 1 ||
      replications > .Machine$integer.max)
    refuser(etape, "replications_invalides",
            formuler(paste("Le nombre de réplications de Monte-Carlo (replications) doit être un nombre double unique,",
                           "sans attribut, entier, au moins 1 et au plus 2147483647, sans défaut (un integer est",
                           "refusé).")),
            list(replications = replications))
  invisible(TRUE)
}

# Contrôle les paramètres propres à l'étape `validation` (valeurs ; les noms
# sont contrôlés par valider_plan()), dans l'ordre du catalogue. Forme exacte
# (#23) : type de base exact, aucun attribut, sinon refus sous le motif du
# paramètre ; un integer reçu pour plage_ages, tranches_age ou replications
# est refusé (M257). Clé et plage : controler_cle_table() et
# controler_plage_ages() (modules positionnement et exposition, sous le
# module validation ; #167, F2 ; ADR 0017, point 10.1). Puis, sous la relation positionnee (#167, F5), refus
# inter-étapes, dans l'ordre : positionnement_absent (étape positionnement
# absente du plan) ; plage_ages_differente_positionnement ;
# table_reference_differente_positionnement (la table validée est la table
# positionnée sur cette référence, sur cette plage : la contrainte
# somme A = O et les lois conditionnelles n'en tiennent qu'à ce prix).
# `positionnement` : paramètres de l'étape positionnement du plan (déjà
# contrôlés par valider_plan()), ou NULL si elle est absente. Rend TRUE,
# invisible.
controler_parametres_validation <- function(table_reference, relation_table, plage_ages, tranches_age, niveau,
                                            replications, positionnement) {
  controler_cle_table(table_reference, "validation")
  if (!chaine_unique(relation_table) || !(relation_table %in% names(RELATIONS_TABLE)))
    refuser("validation", "relation_table_invalide",
            formuler(paste("La relation entre la table et les observations (relation_table) doit être une chaîne",
                           "unique, sans attribut, prise dans %s, sans défaut (ADR 0015, point 7.1)."),
                     liste_guillemets(names(RELATIONS_TABLE))),
            list(relation_table = relation_table))
  controler_plage_ages(plage_ages, "validation")
  controler_tranches_age(tranches_age, plage_ages, "validation")
  if (!forme_exacte(niveau, "double") || length(niveau) != 1L || !is.finite(niveau) || niveau <= 0 || niveau >= 1)
    refuser("validation", "niveau_invalide",
            formuler(paste("Le niveau de confiance doit être un nombre (double) unique, sans attribut, fini,",
                           "strictement compris entre 0 et 1.")),
            list(niveau = niveau))
  controler_replications(replications, "validation")
  if (identical(relation_table, "positionnee")) {
    if (is.null(positionnement))
      refuser("validation", "positionnement_absent",
              formuler(paste("La relation « positionnee » valide la table positionnée de cette construction : l'étape",
                             "positionnement doit figurer au plan (avant l'étape validation).")),
              list(relation_table = relation_table))
    if (!identical(plage_ages, positionnement[["plage_ages"]]))
      refuser("validation", "plage_ages_differente_positionnement",
              formuler(paste("Relation « positionnee » : la plage d'âges de la validation (%d à %d) doit être celle du",
                             "positionnement (%d à %d) : theta est estimé sur les cellules de cette plage, et les lois",
                             "conditionnelles des tests n'en tiennent qu'à ce prix (les tranches d'âge restent libres)."),
                       as.integer(plage_ages[1L]), as.integer(plage_ages[2L]),
                       as.integer(positionnement[["plage_ages"]][1L]), as.integer(positionnement[["plage_ages"]][2L])),
              list(plage_ages = plage_ages, plage_ages_positionnement = positionnement[["plage_ages"]]))
    if (!identical(table_reference, positionnement[["table_reference"]]))
      refuser("validation", "table_reference_differente_positionnement",
              formuler(paste("Relation « positionnee » : la clé de table de la validation (« %s ») doit être celle du",
                             "positionnement (« %s ») : elle désigne la référence de la table positionnée validée."),
                       citer(table_reference), citer(positionnement[["table_reference"]])),
              list(table_reference = table_reference,
                   table_reference_positionnement = positionnement[["table_reference"]]))
  }
  invisible(TRUE)
}

# P(X < l) + P(X > u), X ~ Poisson(A), par cellule (bornes réelles ou
# entières ; ppois d'un argument négatif vaut 0).
probabilite_hors <- function(A, l, u)
  stats::ppois(ceiling(l) - 1, A) + stats::ppois(floor(u), A, lower.tail = FALSE)
# Même probabilité sous X ~ Bin(n, p) (relation positionnee : loi
# conditionnelle D_c | O_s ; pbinom d'un argument négatif vaut 0).
probabilite_hors_binomiale <- function(n, p, l, u)
  stats::pbinom(ceiling(l) - 1, n, p) + stats::pbinom(floor(u), n, p, lower.tail = FALSE)

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

# Réplications multinomiales conditionnelles par groupe (F3 ; #167,
# spécification, § 8 : le groupe est le segment pour p = 1 ; la cellule ou
# la strate pour #168) : B jeux D*, chaque groupe g tiré selon la loi
# multinomiale (effectifs[g] ; poids du groupe / leur somme), de sorte que
# la somme de D* sur le groupe vaut exactement effectifs[g] dans chaque
# réplique. `groupe` : indice de groupe de chaque cellule (1..G, G =
# length(effectifs)) ; `poids` : poids positifs des cellules (attendus).
# Ordre de tirage fixé : réplique par réplique, puis groupe par groupe dans
# l'ordre des indices, un appel rmultinom(1, effectifs[g], pi_g) chacun, sous
# sous_graine(graine) ; les répliques sont réduites par blocs de colonnes
# (ELEMENTS_BLOC_REPLICATIONS) par `statistiques`, comme
# replications_poisson() ; l'ordre réplique par réplique rend les tirages
# indépendants du découpage en blocs. Rend la liste des vecteurs concaténés
# (longueur B). Un groupe sans cellule n'est admis que d'effectif nul (il
# est sauté, sans tirage) : d'effectif positif, ses décès n'auraient aucune
# cellule où se répartir (erreur de programmation, contrôlée avant tout
# tirage).
replications_multinomiales <- function(poids, groupe, effectifs, B, graine, statistiques,
                                       elements_bloc = ELEMENTS_BLOC_REPLICATIONS) {
  n <- length(poids)
  G <- length(effectifs)
  if (length(groupe) != n || anyNA(groupe) || any(groupe < 1L | groupe > G) || any(!(poids > 0)) ||
      anyNA(effectifs) || any(effectifs < 0))
    stop("replications_multinomiales() : groupes, poids ou effectifs invalides (erreur de programmation).",
         call. = FALSE)
  idx <- lapply(seq_len(G), function(g) which(groupe == g))
  if (any(!lengths(idx) & effectifs > 0))
    stop("replications_multinomiales() : groupe d'effectif positif sans cellule (erreur de programmation).",
         call. = FALSE)
  pi_g <- lapply(idx, function(i) poids[i] / sum(poids[i]))
  taille <- max(1, floor(elements_bloc / max(1, n)))
  sous_graine(graine, {
    morceaux <- list()
    fait <- 0
    while (fait < B) {
      m <- min(taille, B - fait)
      Dm <- matrix(0, nrow = n, ncol = m)
      for (j in seq_len(m)) for (g in seq_len(G))
        if (length(idx[[g]])) Dm[idx[[g]], j] <- stats::rmultinom(1L, effectifs[g], pi_g[[g]])
      morceaux[[length(morceaux) + 1L]] <- statistiques(Dm)
      fait <- fait + m
    }
    stats::setNames(lapply(names(morceaux[[1L]]), function(k) unlist(lapply(morceaux, `[[`, k), use.names = FALSE)),
                    names(morceaux[[1L]]))
  })
}

# dbinom(0:n, n, p) réduite à l'intervalle de ses valeurs non nulles en
# double (les valeurs retirées sont des zéros exacts de dbinom) : list(d,
# debut = première valeur de X gardée).
binomiale_elaguee <- function(n, p) {
  d <- stats::dbinom(0:n, n, p)
  nz <- which(d > 0)
  list(d = d[nz[1L]:nz[length(nz)]], debut = nz[1L] - 1)
}

# Coût de loi_somme_binomiales(n, p), calculé avant la convolution (audit
# A2) : somme sur k >= 2 de len(loi_(k-1)) x len(d_k), len(d_k) longueur
# élaguée de dbinom, len(loi_k) = somme des len(d_j), j <= k, moins (k - 1) ;
# en O(somme des n_s).
cout_convolution_binomiales <- function(n, p) {
  l <- vapply(seq_along(n), function(s) length(binomiale_elaguee(n[s], p[s])$d), 0)
  if (length(l) < 2L) return(0)
  cumul <- cumsum(l) - (seq_along(l) - 1)
  sum(cumul[-length(l)] * l[-1L])
}

# Loi d'une somme de binomiales indépendantes Bin(n_s, p_s) (F3 ; brique
# partagée avec #168) : convolution directe des dbinom élaguées
# (binomiale_elaguee()) ; approximation numérique (dbinom, sommes de
# produits). Rend le vecteur P(X = 0), ..., P(X = somme des n_s), avec
# l'attribut `produits` : nombre de produits effectués (égal à
# cout_convolution_binomiales()).
loi_somme_binomiales <- function(n, p) {
  loi <- 1
  debut <- 0
  produits <- 0
  for (s in seq_along(n)) {
    e <- binomiale_elaguee(n[s], p[s])
    a <- e$d
    debut <- debut + e$debut
    if (s > 1L) produits <- produits + length(a) * length(loi)
    if (length(a) < length(loi)) {
      b <- a; a <- loi
    } else b <- loi
    v <- numeric(length(a) + length(b) - 1L)
    for (j in seq_along(b)) {
      ix <- j - 1L + seq_along(a)
      v[ix] <- v[ix] + a * b[j]
    }
    loi <- v
  }
  r <- numeric(sum(n) + 1)
  r[debut + seq_along(loi)] <- loi
  attr(r, "produits") <- produits
  r
}

# p-valeurs asymptotiques d'une somme X de binomiales indépendantes Bin(n_s,
# p_s) en o (#178 ; spécification (0'), M361) : approximation normale de
# moyenne mu = somme de n_s p_s et de variance sigma^2 = somme de n_s p_s (1 -
# p_s), avec correction de continuité : P(X <= o) par Phi((o + 1/2 - mu) /
# sigma), P(X >= o) par 1 - Phi((o - 1/2 - mu) / sigma) (pnorm(lower.tail =
# FALSE)), p centrale min(1, 2 min). sigma = 0 (toutes les p_s dans {0, 1})
# est inatteignable au-delà de N_MAX_PRODUITS_CONVOLUTION : erreur de
# programmation. Rend c(inferieure, superieure, centrale).
p_somme_binomiales_normale <- function(n, p, o) {
  mu <- sum(n * p)
  sigma <- sqrt(sum(n * p * (1 - p)))
  if (!isTRUE(sigma > 0))
    stop("p_somme_binomiales_normale() : variance nulle (erreur de programmation).", call. = FALSE)
  inf <- stats::pnorm((o + 0.5 - mu) / sigma)
  sup <- stats::pnorm((o - 0.5 - mu) / sigma, lower.tail = FALSE)
  c(inferieure = inf, superieure = sup, centrale = min(1, 2 * min(inf, sup)))
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
#   relation_table : relation (RELATIONS_TABLE), lue par des switch
#   explicites à trois valeurs (F5) ;
#   replications, graine : Monte-Carlo (aucun tirage sous ajustee_hors_outil
#   ou si aucune cellule n'est retenue) ;
#   segment : indice de segment de chaque cellule (relation positionnee
#   seule : groupes des lois conditionnelles et des réplications
#   multinomiales ; NULL sinon). Sous positionnee, qx, mu et A sont ceux de
#   la table positionnée (theta^_s q~ sur l'échelle de la force).
# Rend list(statistiques (format long, une ligne par ligne des statuts de la
# relation hors sous_groupes : statistique, grandeur, valeur, etat, statut),
# cellules (colonnes calculées par cellule), sequences (n+, n-, R par
# séquence, dans l'ordre des séquences), tirages_effectues).
statistiques_validation <- function(D, Ec, qx, mu, A, etat, sequence, age, niveau, relation_table,
                                    replications, graine, segment = NULL) {
  alpha <- 1 - niveau
  zq <- stats::qnorm(alpha / 2, lower.tail = FALSE)
  nc <- length(D)
  ret <- etat == "retenue"
  statuts <- statuts_validation(relation_table)
  ajustee <- switch(relation_table, independante = FALSE, ajustee_hors_outil = TRUE, positionnee = FALSE,
                    stop("statistiques_validation() : relation inconnue (erreur de programmation).", call. = FALSE))
  positionnee <- identical(relation_table, "positionnee")
  if (positionnee != !is.null(segment))
    stop("statistiques_validation() : segment requis sous la seule relation positionnee (erreur de programmation).",
         call. = FALSE)
  # Cellules retenues rangées par séquence puis âge (ordre des suites).
  o <- which(ret)[order(sequence[ret], age[ret], method = "radix")]
  n <- length(o)
  Dr <- as.numeric(D[o]); Ar <- A[o]; Er <- Ec[o]; mur <- mu[o]; sr <- sequence[o]
  debut <- match(sr, sr)
  # Relation positionnee : segments des cellules retenues renumérotés
  # 1..S dans l'ordre de leur indice, O_s, pi_c = A_c / A_s (lois
  # conditionnelles, libres de theta).
  if (positionnee && n) {
    useg <- sort(unique(segment[o]))
    gr <- match(segment[o], useg)
    Os <- as.numeric(rowsum(Dr, gr, reorder = TRUE))
    pic <- Ar / as.numeric(rowsum(Ar, gr, reorder = TRUE))[gr]
    nr <- Os[gr]
  }
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
  # Décomptes rangés en integer (#199 ; ADR 0009, point 9.6, (ii)) : n+, n- et
  # R d'une séquence sont bornés par le nombre de cellules retenues, les
  # tirages par replications (au plus .Machine$integer.max, contrôlé) ;
  # calculs en double.
  seqs <- data.frame(sequence = integer(0), positifs = integer(0), negatifs = integer(0), runs = integer(0))
  tirages <- 0L
  if (n) {
    rr <- Dr - Ar
    dev <- Dr * log(Dr / Ar); dev[Dr == 0] <- 0
    dev <- dev - rr
    cl$residu_reponse[o] <- rr
    cl$residu_pearson[o] <- rr / sqrt(Ar)
    cl$residu_deviance[o] <- sign(rr) * sqrt(pmax(0, 2 * dev))
    # Probabilité hors de [l, u] sous la loi de la relation : Poisson(A_c)
    # (p = 0), binomiale conditionnelle Bin(O_s, pi_c) (positionnee).
    hors <- function(l, u) if (positionnee) probabilite_hors_binomiale(nr, pic, l, u) else probabilite_hors(Ar, l, u)
    # S1 : décomptes au-delà de 2 et 3 (bornes A -+ k sqrt(A)) et espérances.
    for (k in 2:3) {
      lo <- Ar - k * sqrt(Ar); hi <- Ar + k * sqrt(Ar)
      pose("residus", paste0("au_dela_", k), sum(Dr < lo | Dr > hi))
      pose("residus", paste0("esperance_au_dela_", k), sum(hors(lo, hi)))
    }
    # Statistiques de test observées (même fonction que les répliques).
    t_obs <- statistiques_test(matrix(Dr, ncol = 1L), Ar, Er, mur, debut)
    # ddl = n - p (p = 0), n - S (un paramètre par segment positionné) ; NA
    # sous ajustee_hors_outil (remplacé plus bas par ddl_non_etabli).
    ddl <- switch(relation_table, independante = n - RELATIONS_TABLE[["independante"]],
                  ajustee_hors_outil = n - RELATIONS_TABLE[["ajustee_hors_outil"]],
                  positionnee = n - length(Os) * RELATIONS_TABLE[["positionnee"]],
                  stop("statistiques_validation() : relation inconnue (erreur de programmation).", call. = FALSE))
    # Relation positionnee, ddl = n - S = 0 (une cellule retenue par segment,
    # D_c = O_s : loi conditionnelle dégénérée ; audit A1) : branchement
    # explicite sur le décompte, aucun test défini, aucun tirage.
    degenere <- positionnee && ddl == 0
    pose("khi2_pearson", "statistique", t_obs$khi2)
    pose("khi2_pearson", "ddl", ddl)
    # Variance exacte de X2 sous H0 (i), E^c tenue pour fixe (#179 ;
    # spécification (0'), M359) : moments de Poisson, somme sur C+ de
    # (2 + 1/A_c) ; moments multinomiaux conditionnels aux O_s, par segment
    # 2 (n_s - 1) + somme de 1/A_c - (n_s^2 + 2 n_s - 2) / O_s avec A_c =
    # O_s pi_c (loi des réplications ; O_s >= 1 par le refus
    # positionnement_sans_deces) ; NA sous ajustee_hors_outil (remplacé plus
    # bas par ddl_non_etabli).
    pose("khi2_pearson", "variance_h0", switch(relation_table,
      independante = sum(2 + 1 / Ar),
      ajustee_hors_outil = NA_real_,
      positionnee = {
        n_s <- tabulate(gr, nbins = length(Os))
        # nr * pic = O_s pi_c, l'espérance conditionnelle A_c = O_s pi_c que
        # nomment le statut et la documentation ; non remplacé par Ar =
        # theta^_s E^c mu~_c, qui ne lui est égal qu'à l'arrondi près (#200).
        inv_s <- as.numeric(rowsum(1 / (nr * pic), gr, reorder = TRUE))
        sum(2 * (n_s - 1) + inv_s - (n_s^2 + 2 * n_s - 2) / Os)
      },
      stop("statistiques_validation() : relation inconnue (erreur de programmation).", call. = FALSE)))
    pose("khi2_pearson", "p_asymptotique", stats::pchisq(t_obs$khi2, ddl, lower.tail = FALSE))
    pose("deviance", "statistique", t_obs$deviance)
    pose("deviance", "ddl", ddl)
    pose("deviance", "p_asymptotique", stats::pchisq(t_obs$deviance, ddl, lower.tail = FALSE))
    # S4 : SMR global.
    O <- sum(Dr); At <- sum(Ar)
    sm <- smr_poisson(O, At, niveau)
    pose("smr", "observes", O); pose("smr", "attendus", At); pose("smr", "smr", sm$smr); pose("smr", "ecart", O - At)
    if (positionnee) {
      # SMR de la table positionnée : 1 par construction (theta estimé sur ces
      # cellules) ; aucune grandeur sous H0.
      for (g in c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure", "p_centrale",
                  "z_racine_cubique", "p_racine_cubique"))
        pose("smr", g, NA_real_, "contraint_par_positionnement")
    } else {
      pose("smr", "borne_inf_exacte", sm$borne_inf); pose("smr", "borne_sup_exacte", sm$borne_sup)
      pose("smr", "p_inferieure", sm$p_inferieure); pose("smr", "p_superieure", sm$p_superieure)
      pose("smr", "p_centrale", sm$p_centrale)
      pose("smr", "z_racine_cubique", sm$z); pose("smr", "p_racine_cubique", sm$p_z)
    }
    # S6 : signes.
    sgn <- signe_residu(rr, Ar)
    np <- sum(sgn > 0); nm <- sum(sgn < 0); ns <- np + nm
    pose("signes", "positifs", np); pose("signes", "negatifs", nm); pose("signes", "nuls", n - ns)
    pose("signes", "effectif", ns)
    pose("signes", "esperance_positifs_h0",
         sum(if (positionnee) stats::pbinom(floor(Ar * (1 + TOLERANCE_RESIDU_NUL)), nr, pic, lower.tail = FALSE)
             else stats::ppois(floor(Ar * (1 + TOLERANCE_RESIDU_NUL)), Ar, lower.tail = FALSE)))
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
    runs_s <- as.numeric(tabulate(match(s_nz[ouvre], us), nbins = length(us)))
    if (!identical(sum(runs_s), as.numeric(t_obs$runs)))
      stop("statistiques_validation() : décompte des suites par séquence incohérent (erreur de programmation).",
           call. = FALSE)
    seqs <- data.frame(sequence = us, positifs = as.integer(n1), negatifs = as.integer(n2), runs = as.integer(runs_s))
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
    # ex aequo exacts des cellules D = 0 ; mu~ est mu_pos = theta^_s mu~ sous
    # positionnee), nul hors des résidus non nuls (même différence que
    # statistiques_test()).
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
    # S12 : intervalles de fluctuation, exact et asymptotique : Poisson(A_c)
    # (p = 0) ; binomiale Bin(O_s, pi_c), variance A_c (1 - pi_c)
    # (positionnee).
    if (positionnee) {
      L <- stats::qbinom(alpha / 2, nr, pic); U <- stats::qbinom(alpha / 2, nr, pic, lower.tail = FALSE)
      la <- Ar - zq * sqrt(Ar * (1 - pic)); ua <- Ar + zq * sqrt(Ar * (1 - pic))
    } else {
      L <- stats::qpois(alpha / 2, Ar); U <- stats::qpois(alpha / 2, Ar, lower.tail = FALSE)
      la <- Ar - zq * sqrt(Ar); ua <- Ar + zq * sqrt(Ar)
    }
    cl$fluctuation_inf_exacte[o] <- L; cl$fluctuation_sup_exacte[o] <- U
    cl$fluctuation_inf_asymptotique[o] <- la; cl$fluctuation_sup_asymptotique[o] <- ua
    pose("fluctuation", "hors_intervalle_exact", sum(Dr < L | Dr > U))
    pose("fluctuation", "esperance_hors_intervalle_exact", sum(hors(L, U)))
    pose("fluctuation", "hors_intervalle_asymptotique", sum(Dr < la | Dr > ua))
    pose("fluctuation", "esperance_hors_intervalle_asymptotique", sum(hors(la, ua)))
    # Monte-Carlo : un seul jeu de B réplications, Poisson(A_c) (p = 0) ou
    # multinomiales (O_s ; pi_c) par segment (positionnee), sauf relation
    # ajustee_hors_outil (aucun tirage, M254).
    if (!ajustee && !degenere) {
      f_stat <- function(Dm) statistiques_test(Dm, Ar, Er, mur, debut)
      t_rep <- switch(relation_table,
                      independante = replications_poisson(Ar, replications, graine, f_stat),
                      positionnee = replications_multinomiales(Ar, gr, Os, replications, graine, f_stat),
                      stop("statistiques_validation() : relation sans réplications (erreur de programmation).",
                           call. = FALSE))
      tirages <- as.integer(replications)
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
    if (degenere) {
      mc_ <- c("p_monte_carlo", "erreur_monte_carlo")
      tests <- list(khi2_pearson = c("variance_h0", "p_asymptotique", mc_), deviance = c("p_asymptotique", mc_),
                    signes = c("statistique", "p_asymptotique", "p_exacte", mc_),
                    runs = c("esperance", "variance", "statistique", "p_asymptotique", "p_exacte", mc_),
                    wilcoxon = c("variance", "statistique", "p_asymptotique", "p_exacte", mc_))
      for (st in names(tests)) for (g in tests[[st]]) pose(st, g, NA_real_, "contraint_par_positionnement")
    }
    if (ajustee) {
      for (k in which(statuts$sous_h0 & statuts$statistique != "sous_groupes")) {
        s <- statuts$statistique[k]; g <- statuts$grandeur[k]
        pose(s, g, NA_real_, "ddl_non_etabli")
      }
      cl[c("fluctuation_inf_exacte", "fluctuation_sup_exacte", "fluctuation_inf_asymptotique",
           "fluctuation_sup_asymptotique")] <- list(rep_na())
    }
  }
  sv <- statuts[statuts$statistique != "sous_groupes", , drop = FALSE]
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

# Colonne annee d'un regroupement de sous_groupes_validation() : integer de
# longueur n_g. n_g = 0 : integer(0) (#210) ; longueur des années égale à n_g :
# as.integer() ; toute autre longueur est une erreur de programmation, jamais
# recyclée en silence (#217 ; rep_len() la masquerait).
annees_groupe <- function(an_g, n_g) {
  if (n_g == 0L) return(integer(0))
  if (length(an_g) != n_g)
    stop("annees_groupe() : longueur des années différente du nombre de groupes (erreur de programmation).", call. = FALSE)
  as.integer(an_g)
}

# Sous-groupes (S5 ; M253, point 5) : regroupements segment x tranche,
# tranche, segment (si la segmentation n'est pas vide), année (si
# par_annee), sur les cellules retenues ; selon la relation (switch
# explicite, F5) : p = 0 (independante) : Poisson exact (smr_poisson()) ;
# ajustee_hors_outil : O, A et SMR seuls, bornes et p-valeurs NA, état
# ddl_non_etabli ; positionnee (A attendus de la table positionnée) :
# sous_groupes_positionnee() ; groupe sans cellule retenue : état
# aucune_cellule_retenue.
sous_groupes_validation <- function(cellules, segment, segments_cles, tranche, tranches, plage_ages, D, A, ret,
                                    par_annee, niveau, relation_table) {
  fin_tranche <- c(tranches[-1L] - 1, plage_ages[2L])
  n_seg <- nrow(segments_cles)
  Dr <- ifelse(ret, as.numeric(D), 0); Ar <- ifelse(ret, A, 0); nr <- as.integer(ret)
  groupe <- function(nom, cle, n_g, seg_g, tr_g, an_g) {
    O <- vapply(seq_len(n_g), function(k) sum(Dr[cle == k]), 0)
    At <- vapply(seq_len(n_g), function(k) sum(Ar[cle == k]), 0)
    nb <- vapply(seq_len(n_g), function(k) sum(nr[cle == k]), 0L)  # dénombrement : integer (#174)
    d <- data.frame(regroupement = rep(nom, n_g), stringsAsFactors = FALSE)
    for (k in names(segments_cles)) d[[k]] <- segments_cles[[k]][seg_g]
    d$tranche_debut <- as.integer(tranches[tr_g])
    d$tranche_fin <- as.integer(fin_tranche[tr_g])
    if (par_annee) d$annee <- annees_groupe(an_g, n_g)  # integer de longueur n_g, y compris pour n_g = 0 (#210, #217)
    d$cellules_retenues <- nb
    d$observes <- denombrement_entier(O, "validation", "sous_groupes/observes")  # integer (#199) ; O reste double
    d$attendus <- At
    ok <- At > 0
    if (identical(relation_table, "positionnee")) {
      v <- sous_groupes_positionnee(nom, cle, n_g, segment, ret, Dr, Ar, O, At, niveau)
      for (c_ in names(v)) d[[c_]] <- v[[c_]]
      return(d)
    }
    ajustee <- switch(relation_table, independante = FALSE, ajustee_hors_outil = TRUE,
                      stop("sous_groupes_validation() : relation inconnue (erreur de programmation).", call. = FALSE))
    s <- smr_poisson(O, At, niveau)
    for (c_ in c("smr", "borne_inf", "borne_sup", "p_inferieure", "p_superieure", "p_centrale")) {
      v <- s[[c_]]; v[!ok] <- NA_real_
      if (ajustee && c_ != "smr") v[] <- NA_real_
      d[[switch(c_, borne_inf = "borne_inf_exacte", borne_sup = "borne_sup_exacte", c_)]] <- v
    }
    # p asymptotiques (#178 ; M361) : colonnes présentes sous les trois
    # relations, produites sous positionnee seule (NA ici).
    for (c_ in c("p_inferieure_asymptotique", "p_superieure_asymptotique", "p_centrale_asymptotique"))
      d[[c_]] <- rep(NA_real_, n_g)
    # character même sans groupe (#201) : ifelse() rend logical(0) sur ok vide.
    etat <- rep(if (ajustee) "ddl_non_etabli" else "produite", n_g); etat[!ok] <- "aucune_cellule_retenue"
    d$etat <- etat
    d
  }
  n_tr <- length(tranches)
  na <- NA_integer_
  morceaux <- list()
  if (length(names(segments_cles))) {
    morceaux$segment_tranche <- groupe("segment_tranche", (segment - 1L) * n_tr + tranche, n_seg * n_tr,
                                       rep(seq_len(n_seg), each = n_tr), rep(seq_len(n_tr), times = n_seg), rep(na, n_seg * n_tr))
  }
  morceaux$tranche <- groupe("tranche", tranche, n_tr, rep(na, n_tr), seq_len(n_tr), rep(na, n_tr))
  if (length(names(segments_cles)))
    morceaux$segment <- groupe("segment", segment, n_seg, seq_len(n_seg), rep(na, n_seg), rep(na, n_seg))
  if (par_annee) {
    an <- sort(unique(cellules$annee))
    morceaux$annee <- groupe("annee", match(cellules$annee, an), length(an), rep(na, length(an)),
                             rep(na, length(an)), an)
  }
  r <- do.call(rbind, unname(morceaux))
  rownames(r) <- NULL
  r
}

# Sous-groupes sous la relation positionnee (#167, spécification, § 8) :
# pour chaque groupe k du regroupement `nom` (indices `cle` des cellules,
# n_g groupes ; O et At : décès et attendus de la table positionnée du
# groupe), colonnes smr, borne_inf_exacte, borne_sup_exacte, p_inferieure,
# p_superieure, p_centrale, p_inferieure_asymptotique,
# p_superieure_asymptotique, p_centrale_asymptotique, etat. Lois
# conditionnelles aux décès O_s de chaque segment s : O_b,s | O_s ~ Bin(O_s,
# pi_b,s), pi_b,s = A_b,s / A_s.
#   - regroupement segment, et tout groupe qui contient, pour chaque segment
#     qu'il touche, toutes les cellules retenues de ce segment (C1 : O_b
#     déterministe sachant les O_s ; détecté par les décomptes de cellules
#     retenues, jamais par un test flottant pi = 1) : SMR descriptif, bornes
#     et p-valeurs NA, état contraint_par_positionnement ;
#   - groupe d'un seul segment (cellules retenues d'un seul segment) :
#     p-valeurs binomiales exactes (pbinom), bornes de O_b / A_b par
#     l'intervalle exact binomial de pi_b (qbeta), divisé par pi_b ;
#   - groupe de plusieurs segments : p-valeurs exactes par la loi de la somme
#     des binomiales (loi_somme_binomiales() ; un segment complet y entre
#     avec pi = 1 exactement, loi dégénérée en O_s), bornes NA, état
#     bornes_non_definies ; coût (cout_convolution_binomiales()) au-delà
#     de n_max (N_MAX_PRODUITS_CONVOLUTION ; autre valeur pour les seuls
#     tests) : p-valeurs exactes et bornes NA, p-valeurs asymptotiques
#     (p_somme_binomiales_normale() ; #178), état convolution_au_dela_n_max ;
#     une ligne ne porte jamais à la fois une p exacte et une p asymptotique ;
#   - groupe sans cellule retenue : NA, état aucune_cellule_retenue.
# Colonnes p_*_asymptotique NA hors de l'état convolution_au_dela_n_max.
sous_groupes_positionnee <- function(nom, cle, n_g, segment, ret, Dr, Ar, O, At, niveau,
                                     n_max = N_MAX_PRODUITS_CONVOLUTION) {
  alpha <- 1 - niveau
  na <- rep(NA_real_, n_g)
  # smr (double) et etat (character) même sans groupe (#201) : ifelse() rend
  # logical(0) sur At vide.
  smr <- na; smr[At > 0] <- (O / At)[At > 0]
  etat <- rep("aucune_cellule_retenue", n_g); etat[At > 0] <- "produite"
  r <- list(smr = smr, borne_inf_exacte = na, borne_sup_exacte = na, p_inferieure = na,
            p_superieure = na, p_centrale = na, p_inferieure_asymptotique = na, p_superieure_asymptotique = na,
            p_centrale_asymptotique = na, etat = etat)
  if (identical(nom, "segment")) {
    r$etat[At > 0] <- "contraint_par_positionnement"
    return(r)
  }
  segs <- sort(unique(segment[ret]))
  Os <- vapply(segs, function(s_) sum(Dr[ret & segment == s_]), 0)
  As <- vapply(segs, function(s_) sum(Ar[ret & segment == s_]), 0)
  Ns <- vapply(segs, function(s_) sum(ret & segment == s_), 0L)
  for (k in which(At > 0)) {
    dans <- ret & cle == k
    sk <- sort(unique(segment[dans]))
    j <- match(sk, segs)
    complet <- vapply(sk, function(s_) sum(dans & segment == s_), 0L) == Ns[j]
    if (all(complet)) {
      r$etat[k] <- "contraint_par_positionnement"
      next
    }
    pib <- vapply(sk, function(s_) sum(Ar[dans & segment == s_]), 0) / As[j]
    pib[complet] <- 1
    ob <- O[k]
    if (length(sk) == 1L) {
      n <- Os[j]
      pinf <- stats::pbinom(ob, n, pib)
      psup <- stats::pbinom(ob - 1, n, pib, lower.tail = FALSE)
      r$borne_inf_exacte[k] <- if (ob > 0) stats::qbeta(alpha / 2, ob, n - ob + 1) / pib else 0
      r$borne_sup_exacte[k] <- if (ob < n) stats::qbeta(1 - alpha / 2, ob + 1, n - ob) / pib else 1 / pib
    } else if (cout_convolution_binomiales(Os[j], pib) > n_max) {
      pa <- p_somme_binomiales_normale(Os[j], pib, ob)
      r$p_inferieure_asymptotique[k] <- pa[["inferieure"]]
      r$p_superieure_asymptotique[k] <- pa[["superieure"]]
      r$p_centrale_asymptotique[k] <- pa[["centrale"]]
      r$etat[k] <- "convolution_au_dela_n_max"
      next
    } else {
      loi <- loi_somme_binomiales(Os[j], pib)
      pinf <- min(1, sum(loi[seq_len(ob + 1)]))
      psup <- min(1, sum(loi[(ob + 1):length(loi)]))
      r$etat[k] <- "bornes_non_definies"
    }
    r$p_inferieure[k] <- pinf
    r$p_superieure[k] <- psup
    r$p_centrale[k] <- min(1, 2 * min(pinf, psup))
  }
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
#   positionnement   : etapes$positionnement de la construction (parametres,
#                      resultat), ou NULL si l'étape n'est pas au plan ; lu
#                      sous la seule relation positionnee (F5) : theta^ par
#                      segment (resultat$smr), jamais table_positionnee$qx ;
#                      ignoré sous les autres relations.
# Relation positionnee : la grille (clés, âges, E^c, D, q~) est celle du
# positionnement (même exposition, même table, même plage : contrôlé,
# erreur de programmation sinon) ; attendus A_c = theta^_s A~_c, force
# theta^_s mu~_c, q_c = 1 - exp(-theta^_s mu~_c) (critère d'actuary du
# 07/11/2026 : calculés sur la force, finis même si la table positionnée
# vaut 1 par arrondi, état qx_arrondi_un).
# Aucun contrôle (#82 ; ADR 0007, annotation du 17/10/2026, (c) et (d)) :
# construire_table() a validé la segmentation et les paramètres
# (valider_plan()), la table (contrôle 1.3) et les entrées
# (controler_entrees_validation()) une fois, dans controler_entrees().
# Rend list(parametres, resultat (cellules, statistiques, sous_groupes,
# sequences_runs, monte_carlo), diagnostics, avertissements). Aucune copie de
# la table (ADR 0015, point 5).
calculer_validation <- function(exposition, segmentation, par_annee, convention, table, table_reference,
                                relation_table, plage_ages, tranches_age, niveau, replications, graine,
                                positionnement) {
  manquantes <- setdiff(c(segmentation, "age", if (isTRUE(par_annee)) "annee", "evenements", "exposition_centrale"),
                        names(exposition))
  if (length(manquantes))
    stop(sprintf("calculer_validation() : colonne(s) %s absente(s) de la table d'exposition (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  positionnee <- switch(relation_table, independante = , ajustee_hors_outil = FALSE, positionnee = TRUE,
                        stop("calculer_validation() : relation inconnue (erreur de programmation).", call. = FALSE))
  pe <- RELATIONS_TABLE[[relation_table]]
  if (positionnee && is.null(positionnement))
    stop("calculer_validation() : étape positionnement requise sous la relation positionnee (erreur de programmation).",
         call. = FALSE)
  ap <- apparier_table(exposition, segmentation, par_annee, table, plage_ages, table_reference, "validation")
  cel <- ap$cellules
  at <- attendus_cellules(cel$exposition_centrale, cel$evenements, cel$qx_table, table_reference, "validation")
  ret <- at$etat == "retenue"
  # Table validée : la référence (p = 0, ajustee_hors_outil) ou la table
  # positionnée, sur l'échelle de la force (positionnee).
  qx_v <- cel$qx_table; mu_v <- at$taux_table; A_v <- at$attendus
  if (positionnee) {
    pc <- positionnement$resultat$cellules
    colonnes <- c(segmentation, "age", if (isTRUE(par_annee)) "annee", "exposition_centrale", "evenements", "qx_table")
    sm <- positionnement$resultat$smr
    seg_cles <- cel[!duplicated(ap$segment), segmentation, drop = FALSE]
    rownames(seg_cles) <- NULL
    if (!identical(cel[colonnes], pc[colonnes]) || !identical(at$etat, pc$etat) ||
        !identical(seg_cles, sm[segmentation]) || !identical(positionnement$parametres$plage_ages, plage_ages))
      stop("calculer_validation() : grille différente de celle du positionnement (erreur de programmation).",
           call. = FALSE)
    theta <- sm$smr[ap$segment]
    mu_v <- theta * at$taux_table
    A_v <- theta * at$attendus
    qx_v <- -expm1(-mu_v)
  }
  # Séquence des suites : segment x année.
  sequence <- if (isTRUE(par_annee)) (ap$segment - 1L) * length(unique(cel$annee)) + match(cel$annee, sort(unique(cel$annee)))
              else ap$segment
  s <- statistiques_validation(cel$evenements, cel$exposition_centrale, qx_v, mu_v, A_v,
                               at$etat, sequence, cel$age, niveau, relation_table, replications, graine,
                               if (positionnee) ap$segment)
  tranche <- findInterval(cel$age, tranches_age)
  cel$taux_table <- at$taux_table
  if (positionnee) {
    cel$smr_positionnement <- theta
    cel$taux_positionne <- mu_v
  }
  for (k in names(s$cellules)) cel[[k]] <- s$cellules[[k]]
  cel$tranche_age <- as.integer(tranches_age[tranche])
  cel$etat <- at$etat
  segments_cles <- cel[!duplicated(ap$segment), segmentation, drop = FALSE]
  rownames(segments_cles) <- NULL
  sg <- sous_groupes_validation(cel, ap$segment, segments_cles, tranche, tranches_age, plage_ages, cel$evenements,
                                A_v, ret, isTRUE(par_annee), niveau, relation_table)
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
  # Rangement en integer après l'identité, vérifiée en double (#199 ; ADR 0009,
  # point 9.6, (iii)) : garde défensive, le total de l'étape exposition étant
  # déjà gardé (M397) ; l'avertissement qui suit cite la valeur rangée.
  ev_sans <- denombrement_entier(ev_sans, "validation", "evenements_sans_attendu")
  ev_hors <- denombrement_entier(ap$hors_plage$evenements, "validation", "evenements_hors_plage")
  total <- denombrement_entier(total, "validation", "evenements_total")
  avert <- avertissements_vides()
  if (any(sans_attendu))
    avert <- avertir(avert, "validation", "evenements_sans_attendu",
                     sprintf(paste("Validation : %d cellule(s) qui comptent %d événement(s) sans attendu (exposition",
                                   "nulle, ou q_x de la table nul) : statistiques non définies dans ces cellules (états",
                                   "evenement_sans_exposition, evenement_sous_taux_nul) ; événements restitués à part,",
                                   "non comptés dans O."),
                             sum(sans_attendu), ev_sans),
                     sum(sans_attendu))
  etats <- data.frame(etat = ETATS_CELLULE_VALIDATION,
                      cellules = vapply(ETATS_CELLULE_VALIDATION, function(e) sum(at$etat == e), 0L, USE.NAMES = FALSE),
                      evenements = denombrement_entier(vapply(ETATS_CELLULE_VALIDATION, function(e)
                        sum(as.numeric(cel$evenements[at$etat == e])), 0, USE.NAMES = FALSE),
                        "validation", "etats/evenements"),
                      stringsAsFactors = FALSE)
  parametres <- list(table_reference = table_reference, table_nom = attr(table, "nom", exact = TRUE),
                     relation_table = relation_table, parametres_estimes = pe, plage_ages = plage_ages,
                     tranches_age = tranches_age, niveau = niveau, replications = replications,
                     convention = convention, appariement_sexe = ap$appariement_sexe,
                     n_max_wilcoxon_exact = N_MAX_WILCOXON_EXACT, n_max_runs_exact = N_MAX_RUNS_EXACT,
                     hypothese_nulle = if (positionnee) HYPOTHESE_VALIDATION_POSITIONNEE else HYPOTHESE_VALIDATION,
                     reference_methode = REFERENCE_METHODE_VALIDATION,
                     statuts_sous_groupes = texte_moteur(statuts_validation(relation_table)[
                       statuts_validation(relation_table)$statistique == "sous_groupes", c("grandeur", "statut")]))
  rownames(parametres$statuts_sous_groupes) <- NULL
  list(parametres = parametres,
       resultat = list(cellules = cel, statistiques = texte_moteur(s$statistiques), sous_groupes = sg,
                       sequences_runs = sequences_runs,
                       monte_carlo = list(replications = replications, tirages_effectues = s$tirages_effectues,
                                          graine = graine)),
       diagnostics = list(cellules = nrow(cel), cellules_retenues = sum(ret), etats = etats,
                          evenements_sans_attendu = ev_sans, cellules_hors_plage = ap$hors_plage$cellules,
                          evenements_hors_plage = ev_hors,
                          exposition_hors_plage = ap$hors_plage$exposition, evenements_total = total),
       avertissements = avert)
}

# --- étape `homogeneite` (#168) ------------------------------------------------
#
# Test d'homogénéité entre segments (E2.9, E2.12 ; forme décidée par le
# mainteneur, M209 ; périmètre complet, M290 ; spécification
# d'actuary-approfondi, #168, commentaire 6705873492 ; avis de forme (0''),
# F10, F11, M292 ; F11 confirmé, M294). Notations : cellule c (âge, et année
# civile si par_annee) de plage_ages ; segment s ; D_sc décès, E_sc exposition
# centrale (étape exposition) ; D_.c et E_.c sommes sur les segments ;
# pi_sc = E_sc / E_.c. Strate : la cellule (test global, tranches), ou
# cellule x modalité de l'autre variable (test par variable) ; groupe : le
# segment, ou la modalité de la variable testée. H0 : taux communs aux
# groupes de chaque strate ; conditionnellement à D_.c, loi multinomiale
# (D_.c ; pi_c), libre du taux commun. T = U' V^- U, U = O - A, A = somme de
# D_. pi, V = somme de D_. (diag pi - pi pi'), sur les strates informatives
# (D_. > 0, au moins deux groupes exposés) ; ddl = rang de V. Aucun verdict,
# aucun seuil ; formules sans attribution (M200). Étape aléatoire : chaque
# jeu de réplications tire sous sous_graine(graine du plan), flux propre à
# l'étape (F11, M294).

# États d'un test (resultat$tests$etat) : produite ;
# evenement_sans_exposition (une cellule du test porte un événement sans
# exposition, E_sc = 0 et D_sc > 0 : test non produit, sans refus, décision
# 7 de #124, granularité Q4 de M290) ; moins_de_deux_segments_informatifs
# (ddl = 0 : tests non produits, sans refus).
ETATS_TESTS_HOMOGENEITE <- c("produite", "evenement_sans_exposition", "moins_de_deux_segments_informatifs")
# États de la p-valeur exacte (resultat$tests$etat_p_exacte) : ceux du test
# quand il n'est pas produit ; sinon produite (deux groupes comparés),
# plus_de_deux_groupes, convolution_au_dela_n_max (coût de la convolution
# au-delà de N_MAX_PRODUITS_CONVOLUTION, brique de #167, F3), ou
# non_produite_par_cette_version (somme des tranches : la loi exacte d'une
# somme de statistiques de tranche n'est pas mise en oeuvre).
ETATS_P_EXACTE_HOMOGENEITE <- c("produite", "plus_de_deux_groupes", "convolution_au_dela_n_max",
                                "non_produite_par_cette_version", "evenement_sans_exposition",
                                "moins_de_deux_segments_informatifs")
# États d'une ligne de resultat$segments, par priorité :
# evenement_sans_exposition pour un segment d'exposition nulle sur la portée
# qui y porte des décès (reprise de l'audit, m1) ; segment_sans_exposition
# (E_s = 0 sur la portée, sans décès : hors du test, hors de ddl, O = A = 0) ;
# evenement_sans_exposition (portée dont une cellule porte un événement sans
# exposition : z non produit) ; variance_nulle (V_ss = 0, segment absent des
# strates informatives : z non défini) ; produite.
ETATS_SEGMENTS_HOMOGENEITE <- c("produite", "segment_sans_exposition", "evenement_sans_exposition", "variance_nulle")
# Mention d'indépendance des unités (M278, #159), en fin de chaque statut
# d'une loi.
INDEPENDANCE_HOMOGENEITE <- paste("unités d'observation indépendantes ; sous l'unité contrat, les décès sont",
                                  "sur-dispersés et le test est anti-conservateur (p-valeurs trop petites)")
# Hypothèse nulle (spécification de #168, § 1), dans les paramètres de l'étape.
HYPOTHESE_HOMOGENEITE <- texte_moteur(paste(
  "H0 : D_sc ~ Poisson(E_sc mu_c) indépendants, mu_c commun à tous les segments s dans chaque cellule c de la",
  "plage (âge, et année civile si par_annee), force constante sur la cellule, exposition centrale E tenue pour",
  "fixe ; conditionnellement à D_.c, (D_1c, ..., D_Sc) suit la loi multinomiale (D_.c ; pi_c), pi_sc =",
  "E_sc / E_.c, libre de mu_c ; test par variable : mêmes lois, strates cellule x modalité de l'autre variable,",
  "groupes modalités de la variable testée ;", INDEPENDANCE_HOMOGENEITE))
# Référence de méthode (forme M209, E2.12) : source relue sur l'image le
# 07/11/2026 (M290, M305) ; URL, SHA-256 des fichiers lus et pages
# consignés, écarts de l'outil à la source nommés.
REFERENCE_METHODE_HOMOGENEITE <- texte_moteur(paste(
  "Breslow N. E., Day N. E. (1987), Statistical Methods in Cancer Research, vol. II, IARC Scientific Publications",
  "n° 82 ; chap. 3, https://publications.iarc.who.int/download/SP82_vol2-3.pdf, SHA-256",
  "8c20ae8827229febf2f21aca19abf695ad436542ab5875ed2568d7232e6d0dba : § 3.5, éq. (3.13), p. 104 ; § 3.6, p. 107-108 ;",
  "§ 3.6 (f), éq. (3.23)-(3.24), p. 113-114 ; § 3.6 (g), p. 114-115 (exemple) ; chap. 4,",
  "https://publications.iarc.who.int/download/SP82_vol2-4.pdf,",
  "SHA-256 a270d917a68b30985b27c37f3d1cf7b1dbd92aed6bee1c7318c23f8863e0cac9 : § 4.2, p. 131-135 ; consultés le",
  "07/11/2026, lus sur l'image ; source de méthode, qui ne prescrit rien pour l'actuariat ; écarts documentés :",
  "degrés de liberté pris au rang de V (K - 1 dans la source), tests par tranche d'âge et leur somme, p-valeur",
  "exacte bilatérale à deux groupes et p-valeur de Monte-Carlo conditionnelle, absents de la source"))

# Statuts (E2.20) des grandeurs de l'étape, par table du résultat (tests,
# segments) ; spécification de #168, § 4.
STATUTS_HOMOGENEITE <- local({
  ind <- INDEPENDANCE_HOMOGENEITE
  l <- list(
    c("tests", "strates_informatives",
      paste("décompte exact : strates (cellules ; cellule x modalité de l'autre variable pour un test par",
            "variable) où D_. > 0 et où au moins deux groupes sont exposés ; les autres strates apportent 0 à U",
            "et V (exact) ; somme_tranches : somme sur les tranches à ddl > 0")),
    c("tests", "groupes",
      paste("décompte exact : groupes (segments ; modalités de la variable testée) exposés dans une strate",
            "informative ; somme_tranches : NA (un décompte par tranche)")),
    c("tests", "statistique",
      paste("T = U' V^- U, U = O - A, V = somme sur les strates informatives de D_. (diag pi - pi pi') ; somme des T",
            "des tranches (ddl > 0) pour somme_tranches ; arithmétique flottante, système linéaire résolu sur V",
            "réduite (un groupe de référence par composante connexe du graphe groupes-strates) : approximation",
            "numérique")),
    c("tests", "ddl",
      paste("exact (combinatoire) : rang de V, nombre de groupes exposés dans les strates informatives moins nombre",
            "de composantes connexes du graphe groupes-strates ; somme des ddl des tranches pour somme_tranches")),
    c("tests", "p_asymptotique",
      paste("asymptotique : loi du khi-deux à ddl degrés de liberté quand l'information du test croît (plus",
            "petite valeur propre de V réduite -> infini), à nombre de groupes fixé, le nombre de strates étant",
            "fixe ou croissant (D_. pouvant rester borné dans chaque strate quand le nombre de strates croît) ;",
            "sous H0 avec E tenue pour fixe ; asymptotique aussi sous le modèle de survie ; queue supérieure ;",
            ind)),
    c("tests", "p_exacte",
      paste("exacte conditionnellement aux D_. de chaque strate sous H0, E tenue pour fixe, quand deux groupes",
            "seulement sont comparés ; asymptotique sous le modèle de survie ; O_1 somme de binomiales indépendantes",
            "Bin(D_., pi_1), p = P(|O_1 - A_1| >= |o_1 - A_1|), égalité à 1e-10 A_1 près ; convolution des dbinom,",
            "approximation numérique ; produite si le nombre de produits de la convolution est au plus",
            "N_MAX_PRODUITS_CONVOLUTION (état convolution_au_dela_n_max sinon) ; plus de deux groupes : état",
            "plus_de_deux_groupes ; somme_tranches : état non_produite_par_cette_version ;", ind)),
    c("tests", "p_monte_carlo",
      paste("niveau exact conditionnellement aux D_. de chaque strate sous H0, E tenue pour fixe : P_H0(p_MC <=",
            "alpha) <= alpha, p_MC = (1 + K) / (B + 1), queue supérieure ; asymptotique sous le modèle de survie ;",
            "réplications multinomiales (D_. ; pi) par strate sous la graine du plan, un seul jeu pour les tests",
            "global, tranche et somme_tranches, un jeu par test par variable ; e est l'erreur due au nombre fini de",
            "réplications, distincte de l'erreur d'estimation ;", ind)),
    c("tests", "erreur_monte_carlo",
      paste("erreur de Monte-Carlo e = sqrt(B p_MC (1 - p_MC)) / (B + 1), due au nombre fini de réplications,",
            "distincte de l'erreur d'estimation")),
    c("segments", "exposition_centrale",
      paste("descriptif : somme de E^c du segment sur les cellules de la portée (plage, ou tranche), hors cellules",
            "qui portent un événement sans exposition (restituées aux diagnostics)")),
    c("segments", "observes", "descriptif : O_s, somme des décès du segment sur les mêmes cellules"),
    c("segments", "attendus",
      paste("descriptif : A_s = somme sur les mêmes cellules de D_.c pi_sc, décès attendus si les taux étaient",
            "communs aux segments de chaque cellule (standardisation interne)")),
    c("segments", "rapport_observes_attendus",
      paste("descriptif : O_s / A_s, rapport interne au regroupé (standardisation interne) ; ce n'est pas un SMR",
            "rapporté à une table")),
    c("segments", "z",
      paste("z_s = (O_s - A_s) / sqrt(V_ss), écart réduit du segment au regroupé, de loi asymptotique N(0, 1) sous",
            "H0 (V_ss -> infini, même régime que p_asymptotique), E tenue pour fixe ; aucune p-valeur produite ;",
            "calcul en arithmétique flottante, approximation numérique ; aucune correction de multiplicité ;", ind)))
  texte_moteur(data.frame(table = vapply(l, `[`, "", 1L), grandeur = vapply(l, `[`, "", 2L),
                          statut = vapply(l, `[`, "", 3L), stringsAsFactors = FALSE))
})

# Contrôle les paramètres de l'étape `homogeneite` (valeurs ; les noms sont
# contrôlés par valider_plan()), dans l'ordre du catalogue, sous le module
# homogeneite : plage_ages (controler_plage_ages(), module exposition),
# tranches_age et replications (formes de `validation`), puis le refus
# inter-champs homogeneite_sans_segmentation (avis (0''), F10) :
# `segmentation` est le champ du plan, déjà contrôlé par
# controler_parametres_donnees(). Appelée par valider_plan() seule. Rend
# TRUE, invisible.
controler_parametres_homogeneite <- function(plage_ages, tranches_age, replications, segmentation) {
  controler_plage_ages(plage_ages, "homogeneite")
  controler_tranches_age(tranches_age, plage_ages, "homogeneite")
  controler_replications(replications, "homogeneite")
  if (!length(segmentation))
    refuser("homogeneite", "homogeneite_sans_segmentation",
            formuler(paste("Le test d'homogénéité compare les segments de la construction : le plan doit déclarer une",
                           "segmentation (champ segmentation non vide ; E2.9).")),
            list(segmentation = segmentation))
  invisible(TRUE)
}

# Somme de x par indice i (1..n), dans l'ordre des lignes ; 0 pour un
# indice sans ligne.
somme_par <- function(x, i, n) {
  s <- numeric(n)
  if (length(x)) {
    r <- rowsum(as.numeric(x), i, reorder = TRUE)
    s[as.integer(rownames(r))] <- r[, 1L]
  }
  s
}

# Cœur des tests du score conditionnels (spécification de #168, § 1), sur
# des lignes (groupe, strate) d'une portée : D décès, E exposition centrale,
# strate (1..K) et groupe (1..G) entiers ; aucune ligne d'une cellule qui
# porte un événement sans exposition (écartées par l'appelant). Strate
# informative : D_. > 0 et au moins deux groupes à E > 0 ; ligne utilisée :
# strate informative et E > 0. Composantes connexes du graphe groupes-strates
# par union au plus petit indice ; système réduit : un groupe de référence
# (le plus petit indice) retiré par composante (V réduite définie positive,
# U de somme nulle par composante : forme quadratique indépendante du choix).
# Rend list(O, A (par groupe, toutes les lignes), U, v (diagonale de V),
# utilisee (logique par ligne), presents, garde, ddl, strates_informatives,
# statistique (fonction d'une matrice de décès des lignes utilisées, une
# colonne par jeu ; observé et répliques passent par elle), t (observée),
# poids, strate_tirage, effectifs (pour les réplications), exacte (list(n,
# p, o, a) quand deux groupes seulement sont comparés, NULL sinon)).
score_homogeneite <- function(D, E, strate, groupe, K, G) {
  D <- as.numeric(D)
  Ds <- somme_par(D, strate, K)
  Es <- somme_par(E, strate, K)
  ne <- somme_par(E > 0, strate, K)
  inf <- Ds > 0 & ne >= 2
  pi <- numeric(length(D))
  pos <- Es[strate] > 0
  pi[pos] <- E[pos] / Es[strate][pos]
  Ar <- Ds[strate] * pi
  u <- inf[strate] & E > 0
  gu <- as.integer(groupe[u]); Du <- D[u]; Au <- Ar[u]
  si <- which(inf)
  ru <- match(strate[u], si)
  presents <- sort(unique(gu))
  comp <- seq_len(G)
  racine <- function(g) { while (comp[g] != g) g <- comp[g]; g }
  for (gs in split(gu, ru)) {
    r <- unique(vapply(gs, racine, 0L))
    comp[r] <- min(r)
  }
  garde <- presents[vapply(presents, racine, 0L) != presents]
  P <- matrix(0, length(si), G)
  P[cbind(ru, gu)] <- pi[u]
  V <- diag(somme_par(Au, gu, G), G) - crossprod(P, P * Ds[si])
  Vg <- V[garde, garde, drop = FALSE]
  # U = O - A par groupe : O somme d'entiers (exacte en double), A calculée
  # une fois ; deux jeux de mêmes O par groupe ont ainsi le même T au bit
  # près (ex aequo du Monte-Carlo préservés ; reprise de l'audit, C1).
  Ag <- rowsum(Au, gu, reorder = TRUE)[, 1L]
  statistique <- function(Dm) {
    Um <- rowsum(Dm, gu, reorder = TRUE) - Ag
    Uk <- Um[match(garde, as.integer(rownames(Um))), , drop = FALSE]
    colSums(Uk * solve(Vg, Uk))
  }
  ddl <- length(garde)
  exacte <- if (length(presents) == 2L && ddl == 1L)
    list(n = Ds[si], p = P[, garde], o = sum(Du[gu == garde]), a = sum(Au[gu == garde]))
  list(O = somme_par(D, groupe, G), A = somme_par(Ar, groupe, G), U = somme_par(Du - Au, gu, G), v = diag(V),
       utilisee = u, presents = presents, garde = garde, ddl = ddl, strates_informatives = length(si),
       statistique = statistique, t = if (ddl) statistique(matrix(Du, ncol = 1L)) else NA_real_,
       poids = E[u], strate_tirage = ru, effectifs = Ds[si], exacte = exacte)
}

# p-valeur exacte d'un test produit (score_homogeneite()) : deux groupes
# comparés, O_1 somme de binomiales indépendantes Bin(D_., pi_1) sur les
# strates informatives (loi_somme_binomiales(), brique de #167, F3), p =
# P(|O_1 - A_1| >= |o_1 - A_1|), égalité à TOLERANCE_RESIDU_NUL x A_1 près ;
# coût au-delà de N_MAX_PRODUITS_CONVOLUTION : état
# convolution_au_dela_n_max. Rend list(p, etat).
p_exacte_homogeneite <- function(sc) {
  e <- sc$exacte
  if (is.null(e)) return(list(p = NA_real_, etat = "plus_de_deux_groupes"))
  if (cout_convolution_binomiales(e$n, e$p) > N_MAX_PRODUITS_CONVOLUTION)
    return(list(p = NA_real_, etat = "convolution_au_dela_n_max"))
  loi <- loi_somme_binomiales(e$n, e$p)
  k <- seq_along(loi) - 1
  ext <- abs(k - e$a) >= abs(e$o - e$a) - TOLERANCE_RESIDU_NUL * e$a
  list(p = min(1, sum(loi[ext])), etat = "produite")
}

# Étape `homogeneite` du plan (#168).
#   exposition    : table de l'étape `exposition`
#                   (etapes$exposition$resultat$exposition) ;
#   segmentation  : clés de segmentation (champ du plan, non vide : contrôlé
#                   par valider_plan()) ;
#   par_annee, convention : de l'étape `exposition` (toutes conventions
#                   admises : comparaison interne, aucune table appariée) ;
#   plage_ages, tranches_age, replications : paramètres de l'étape ;
#   graine        : graine du plan (étape aléatoire ; F11 : chaque jeu de
#                   réplications sous sous_graine(graine), flux propre).
# Segments : combinaisons des clés présentes dans la table d'exposition, dans
# son ordre ; cellules : (âge[, année]) de la plage présentes dans la table,
# par âge puis année. Tests (une ligne chacun, resultat$tests) : global ;
# chaque tranche d'âge ; somme des tranches (T = somme des T de tranche, ddl
# = somme des ddl) ; si la segmentation a deux variables, chaque variable
# conditionnellement à l'autre. Un jeu de réplications pour global, tranches
# et somme ; un par test par variable ; aucun tirage sans test produit.
# Aucun contrôle (#82) : valider_plan() a contrôlé les paramètres. Rend
# list(parametres, resultat (tests, segments, monte_carlo), diagnostics,
# avertissements).
calculer_homogeneite <- function(exposition, segmentation, par_annee, convention, plage_ages, tranches_age,
                                 replications, graine) {
  manquantes <- setdiff(c(segmentation, "age", if (isTRUE(par_annee)) "annee", "evenements", "exposition_centrale"),
                        names(exposition))
  if (length(manquantes))
    stop(sprintf("calculer_homogeneite() : colonne(s) %s absente(s) de la table d'exposition (erreur de programmation).",
                 paste(manquantes, collapse = ", ")), call. = FALSE)
  if (!length(segmentation))
    stop("calculer_homogeneite() : segmentation requise (erreur de programmation).", call. = FALSE)
  # Segments, dans l'ordre de la table d'exposition.
  cles_seg <- do.call(paste, c(unname(as.list(exposition[segmentation])), list(sep = "\r")))
  premiers <- !duplicated(cles_seg)
  segments <- exposition[premiers, segmentation, drop = FALSE]
  rownames(segments) <- NULL
  S <- nrow(segments)
  dans <- exposition$age >= plage_ages[1L] & exposition$age <= plage_ages[2L]
  ex <- exposition[dans, , drop = FALSE]
  sg <- match(cles_seg[dans], cles_seg[premiers])
  D <- as.numeric(ex$evenements)
  E <- ex$exposition_centrale
  # Cellules de la plage, par âge puis année.
  cle_cel <- if (isTRUE(par_annee)) paste(ex$age, ex$annee, sep = "\r") else as.character(ex$age)
  o <- if (isTRUE(par_annee)) order(ex$age, ex$annee, method = "radix") else order(ex$age, method = "radix")
  ucel <- unique(cle_cel[o])
  cel <- match(cle_cel, ucel)
  K <- length(ucel)
  nb <- length(tranches_age)
  fin_tranche <- as.integer(c(tranches_age[-1L] - 1, plage_ages[2L]))
  tranche <- findInterval(ex$age, tranches_age)
  tranche_cel <- tranche[match(seq_len(K), cel)]
  # Cellules qui portent un événement sans exposition (E_sc = 0, D_sc > 0) :
  # écartées des sommes, tests qui les contiennent non produits (Q4).
  fautive <- tabulate(cel[E == 0 & D > 0], K) > 0
  saine <- !fautive[cel]
  tranche_fautive <- tabulate(tranche_cel[fautive], nb) > 0
  toute_faute <- any(fautive)

  sc_g <- score_homogeneite(D[saine], E[saine], cel[saine], sg[saine], K, S)
  lignes_g <- which(saine)[sc_g$utilisee]
  sc_b <- lapply(seq_len(nb), function(b) {
    l <- saine & tranche == b
    s <- score_homogeneite(D[l], E[l], cel[l], sg[l], K, S)
    s$position <- match(which(l)[s$utilisee], lignes_g)
    s
  })
  prod_g <- !toute_faute && sc_g$ddl > 0
  prod_b <- vapply(seq_len(nb), function(b) !tranche_fautive[b] && sc_b[[b]]$ddl > 0, NA)
  ddl_b <- vapply(sc_b, function(s) s$ddl, 0L)
  prod_somme <- !toute_faute && sum(ddl_b) > 0

  # Réplications du jeu « cellules » : global, tranches, somme.
  jeux <- list()
  t_rep <- NULL
  t_obs <- NULL
  if (prod_g || any(prod_b)) {
    f_stat <- function(Dm) {
      r <- list(global = if (prod_g) sc_g$statistique(Dm) else rep(NA_real_, ncol(Dm)))
      somme <- numeric(ncol(Dm))
      for (b in which(prod_b)) {
        r[[paste0("tranche_", b)]] <- sc_b[[b]]$statistique(Dm[sc_b[[b]]$position, , drop = FALSE])
        somme <- somme + r[[paste0("tranche_", b)]]
      }
      r$somme_tranches <- somme
      r
    }
    t_rep <- replications_multinomiales(sc_g$poids, sc_g$strate_tirage, sc_g$effectifs, replications, graine, f_stat)
    # Valeurs observées du jeu par la même fonction que les répliques (global,
    # tranches, somme accumulée de la même façon ; reprise de l'audit, C2).
    t_obs <- f_stat(matrix(D[lignes_g], ncol = 1L))
    jeux$cellules <- replications
  } else jeux$cellules <- 0

  na_i <- NA_integer_
  ligne <- function(test, variable, conditionnellement_a, debut, fin, sc, t, ddl, etat, pe, mc) {
    data.frame(test = test, variable = variable, conditionnellement_a = conditionnellement_a,
               tranche_debut = as.integer(debut), tranche_fin = as.integer(fin),
               strates_informatives = if (is.null(sc)) na_i else as.integer(sc$strates_informatives),
               groupes = if (is.null(sc$presents)) na_i else length(sc$presents), statistique = t,
               ddl = as.integer(ddl),
               p_asymptotique = if (identical(etat, "produite")) stats::pchisq(t, ddl, lower.tail = FALSE)
                                else NA_real_,
               p_exacte = pe$p, etat_p_exacte = pe$etat, p_monte_carlo = mc[["p"]], erreur_monte_carlo = mc[["e"]],
               etat = etat, stringsAsFactors = FALSE)
  }
  non_produit <- function(etat) list(p = NA_real_, etat = etat)
  mc_na <- c(p = NA_real_, e = NA_real_)
  etat_de <- function(faute, ddl) if (faute) "evenement_sans_exposition"
                                   else if (ddl == 0L) "moins_de_deux_segments_informatifs" else "produite"
  tests <- list()
  e_g <- etat_de(toute_faute, sc_g$ddl)
  tests[[1L]] <- ligne("global", NA_character_, NA_character_, na_i, na_i, sc_g,
                       if (prod_g) t_obs$global else NA_real_, if (toute_faute) na_i else sc_g$ddl, e_g,
                       if (prod_g) p_exacte_homogeneite(sc_g) else non_produit(e_g),
                       if (prod_g) pvaleur_monte_carlo(t_obs$global, t_rep$global, "superieure") else mc_na)
  for (b in seq_len(nb)) {
    e_b <- etat_de(tranche_fautive[b], ddl_b[b])
    tests[[length(tests) + 1L]] <-
      ligne("tranche", NA_character_, NA_character_, tranches_age[b], fin_tranche[b], sc_b[[b]],
            if (prod_b[b]) t_obs[[paste0("tranche_", b)]] else NA_real_, if (tranche_fautive[b]) na_i else ddl_b[b],
            e_b, if (prod_b[b]) p_exacte_homogeneite(sc_b[[b]]) else non_produit(e_b),
            if (prod_b[b]) pvaleur_monte_carlo(t_obs[[paste0("tranche_", b)]], t_rep[[paste0("tranche_", b)]],
                                               "superieure") else mc_na)
  }
  e_s <- etat_de(toute_faute, sum(ddl_b))
  t_somme <- if (prod_somme) t_obs$somme_tranches else NA_real_
  # Somme des tranches : strates informatives des tranches à ddl > 0 ; groupes
  # NA (un décompte par tranche, non sommable ; reprise de l'audit, m2).
  sc_somme <- list(strates_informatives = sum(vapply(sc_b[ddl_b > 0], function(s) s$strates_informatives, 0L)))
  tests[[length(tests) + 1L]] <-
    ligne("somme_tranches", NA_character_, NA_character_, na_i, na_i, sc_somme, t_somme,
          if (toute_faute) na_i else sum(ddl_b), e_s,
          if (prod_somme) non_produit("non_produite_par_cette_version") else non_produit(e_s),
          if (prod_somme) pvaleur_monte_carlo(t_somme, t_rep$somme_tranches, "superieure") else mc_na)
  # Tests par variable, conditionnellement à l'autre (segmentation à deux
  # variables ; décision 6 de #124) : strates cellule x modalité de l'autre
  # variable, groupes modalités de la variable testée ; un jeu de
  # réplications par test.
  if (length(segmentation) == 2L) for (k in 1:2) {
    v <- segmentation[k]; w <- segmentation[3L - k]
    mv <- unique(segments[[v]]); mw <- unique(segments[[w]])
    gv <- match(ex[[v]], mv); gw <- match(ex[[w]], mw)
    st <- (cel - 1L) * length(mw) + gw
    sc_v <- score_homogeneite(D[saine], E[saine], st[saine], gv[saine], K * length(mw), length(mv))
    prod_v <- !toute_faute && sc_v$ddl > 0
    nom_jeu <- paste0("variable_", v)
    mc <- mc_na
    if (prod_v) {
      r <- replications_multinomiales(sc_v$poids, sc_v$strate_tirage, sc_v$effectifs, replications, graine,
                                      function(Dm) list(t = sc_v$statistique(Dm)))
      mc <- pvaleur_monte_carlo(sc_v$t, r$t, "superieure")
      jeux[[nom_jeu]] <- replications
    } else jeux[[nom_jeu]] <- 0
    e_v <- etat_de(toute_faute, sc_v$ddl)
    tests[[length(tests) + 1L]] <-
      ligne("variable", v, w, na_i, na_i, sc_v, if (prod_v) sc_v$t else NA_real_,
            if (toute_faute) na_i else sc_v$ddl, e_v,
            if (prod_v) p_exacte_homogeneite(sc_v) else non_produit(e_v), mc)
  }
  tests <- do.call(rbind, tests)
  rownames(tests) <- NULL

  # Observés et attendus par segment (portée globale, puis par tranche).
  portee <- function(nom, debut, fin, sc, l_toutes, faute) {
    l_saines <- l_toutes & saine
    e_tout <- somme_par(E[l_toutes], sg[l_toutes], S)
    d_tout <- somme_par(D[l_toutes], sg[l_toutes], S)
    d <- data.frame(portee = rep(nom, S), stringsAsFactors = FALSE)
    for (k in segmentation) d[[k]] <- segments[[k]]
    d$tranche_debut <- rep(as.integer(debut), S)
    d$tranche_fin <- rep(as.integer(fin), S)
    d$exposition_centrale <- somme_par(E[l_saines], sg[l_saines], S)
    d$observes <- denombrement_entier(sc$O, "homogeneite", "segments/observes")  # integer (#199)
    d$attendus <- sc$A
    # double et character même sans segment (#201) : ifelse() rend logical(0) sur S = 0.
    rap <- rep(NA_real_, S); pos <- which(sc$A > 0)
    rap[pos] <- sc$O[pos] / sc$A[pos]
    d$rapport_observes_attendus <- rap
    etat <- rep(if (faute) "evenement_sans_exposition" else "produite", S)
    if (!faute) etat[!(sc$v > 0)] <- "variance_nulle"
    etat[e_tout == 0] <- "segment_sans_exposition"
    etat[e_tout == 0 & d_tout > 0] <- "evenement_sans_exposition"
    z <- rep(NA_real_, S)
    ok <- etat == "produite"
    z[ok] <- sc$U[ok] / sqrt(sc$v[ok])
    d$z <- z
    d$etat <- etat
    d
  }
  seg <- c(list(portee("global", na_i, na_i, sc_g, rep(TRUE, nrow(ex)), toute_faute)),
           lapply(seq_len(nb), function(b) portee("tranche", tranches_age[b], fin_tranche[b], sc_b[[b]], tranche == b,
                                                  tranche_fautive[b])))
  seg <- do.call(rbind, seg)
  rownames(seg) <- NULL

  # Décomptes et identité des événements.
  Dc <- somme_par(D, cel, K)
  nexp <- somme_par(E > 0, cel, K)
  O <- sum(sc_g$O)
  ev_fautives <- sum(D[!saine])
  total <- sum(as.numeric(exposition$evenements))
  ev_hors <- sum(as.numeric(exposition$evenements[!dans]))
  if (!identical(O + ev_fautives + ev_hors, total))
    stop("calculer_homogeneite() : décomptes d'événements incohérents (erreur de programmation).", call. = FALSE)
  # Rangement en integer après l'identité, vérifiée en double (#199 ; ADR 0009,
  # point 9.6, (iii)) : garde défensive (total de l'étape exposition gardé,
  # M397) ; l'avertissement qui suit cite la valeur rangée.
  ev_fautives <- denombrement_entier(ev_fautives, "homogeneite", "evenements_cellules_evenement_sans_exposition")
  ev_hors <- denombrement_entier(ev_hors, "homogeneite", "evenements_hors_plage")
  total <- denombrement_entier(total, "homogeneite", "evenements_total")
  avert <- avertissements_vides()
  if (toute_faute)
    avert <- avertir(avert, "homogeneite", "evenements_sans_exposition",
                     sprintf(paste("Homogénéité : %d cellule(s) de la plage où un segment compte des événements sans",
                                   "exposition (%d événement(s) dans ces cellules) : cellules écartées des sommes, tests",
                                   "qui les contiennent non produits (état evenement_sans_exposition)."),
                             sum(fautive), ev_fautives),
                     sum(fautive))
  list(parametres = list(plage_ages = plage_ages, tranches_age = tranches_age, replications = replications,
                         convention = convention, hypothese_nulle = HYPOTHESE_HOMOGENEITE,
                         statuts = STATUTS_HOMOGENEITE, reference_methode = REFERENCE_METHODE_HOMOGENEITE),
       # Tirages effectués par jeu en integer (#199), bornés par replications
       # (au plus .Machine$integer.max, contrôlé par controler_replications()).
       resultat = list(tests = tests, segments = seg,
                       monte_carlo = list(replications = replications, graine = graine,
                                          jeux = data.frame(jeu = names(jeux),
                                                            tirages_effectues = as.integer(unlist(jeux,
                                                                                                  use.names = FALSE)),
                                                            stringsAsFactors = FALSE))),
       diagnostics = list(cellules = K, cellules_informatives = sc_g$strates_informatives,
                          cellules_sans_deces = sum(!fautive & Dc == 0),
                          cellules_non_informatives = sum(!fautive & Dc > 0 & nexp < 2),
                          cellules_evenement_sans_exposition = sum(fautive),
                          evenements_cellules_evenement_sans_exposition = ev_fautives,
                          lignes_exposition_hors_plage = sum(!dans), evenements_hors_plage = ev_hors,
                          exposition_hors_plage = sum(exposition$exposition_centrale[!dans]),
                          evenements_total = total),
       avertissements = avert)
}

# --- étape `importance_exclusions` (#69) ----------------------------------------
#
# Spécification d'actuary-approfondi décidée (#69, commentaire 6663938080 ;
# M215 : Q1 (b), Q2 à Q5 (a)) ; avis de forme (0'') (M242) ; décisions du
# 06/11/2026 (#69, commentaire 6686457325 ; M255, M259). Notations : R lignes
# conservées, S ensemble de lignes exclues mesurables ; D_e,c = D_c(R u S) -
# D_c(R), E_e,c = E_c(R u S) - E_c(R) dans l'unité d'observation de la
# construction (unité contrat : E(S) par additivité ; unité tête : sur les
# seules têtes touchées) ; niveau A : cellules âge (x année si par_annee),
# population regroupée ; niveau B : cellules de la construction. Aucun
# verdict, aucun seuil ; des agrégats seulement, aucun vecteur par ligne.

# Classes des motifs (§ 2) : toujours mesurables ; clé de cellule rendue
# manquante (mesurable si la clé n'est pas une clé des cellules, non
# calculable `cle_segmentation_manquante` sinon) ; id_tete_manquant mesurable
# en unité contrat seulement ; contrat_duplique mesurable en unité tête
# seulement (décision du mainteneur du 06/11/2026, option (b) : non mesurable
# en unité contrat, raison contrat_duplique_unite_contrat) ; tout autre motif
# (motifs de tête compris) non mesurable (Q5 (a) pour age_aberrant et
# annee_naissance_incoherente).
MOTIFS_MESURABLES_IMPORTANCE <- c("id_contrat_manquant", "contrat_duplique")
CLES_MOTIFS_IMPORTANCE <- c(sexe_manquant = "sexe", sexe_hors_modalites = "sexe", segment_manquant = "segment")
# États des événements déclarés d'une ligne exclue (§ 1.2), unité la ligne.
ETATS_EVENEMENTS_EXCLUS <- c("evenement_dans_fenetre", "sans_evenement_dans_fenetre", "evenement_date_inconnue",
                             "cause_inconnue_sortie_dans_fenetre", "cause_et_sortie_inconnues")
# États des grandeurs de l'étape et leur libellé (trois familles : non
# demandée par le plan, non produite par cette version, non calculable).
ETATS_IMPORTANCE <- texte_moteur(data.frame(
  etat = c("produite", "non_demandee_par_le_plan", "non_produite_par_cette_version", "aucune_ligne_mesurable",
           "cle_segmentation_manquante", "tete_indeterminee_apres_reintegration", "aucun_attendu",
           "observe_sans_attendu", "sans_exposition_retenue", "cellule_sans_valeur_table", "taux_table_egal_un",
           "contrat_duplique_unite_contrat", "ddl_non_etabli"),
  libelle = c("produite", "non demandée par le plan", "non produite par cette version",
              "non calculable : aucune_ligne_mesurable", "non calculable : cle_segmentation_manquante",
              "non calculable : tete_indeterminee_apres_reintegration", "non calculable : aucun_attendu",
              "non calculable : observe_sans_attendu", "non calculable : sans_exposition_retenue",
              "non calculable : cellule_sans_valeur_table", "non calculable : taux_table_egal_un",
              "non calculable : contrat_duplique_unite_contrat", "non calculable : ddl_non_etabli"),
  stringsAsFactors = FALSE))
# Statuts (E2.20) des grandeurs de l'étape, par table du résultat.
STATUTS_IMPORTANCE <- local({
  l <- list(
    c("volumes", "lignes", paste("décompte exact ; identité lignes_recues = lignes_conservees + lignes_exclues",
                                 "vérifiée")),
    c("motifs", "lignes", paste("décompte exact ; une ligne compte sous chacun de ses motifs, seule la ligne total",
                                "est distincte ; mesurable : tous les motifs de la ligne sont mesurables au niveau",
                                "(parametres/classes_motifs) ; non calculable au niveau B : une clé de segmentation",
                                "employée manque ; non mesurable : événements déclarés comptés, exposition et SMR non",
                                "calculés")),
    c("evenements_declares", "lignes",
      paste("décompte exact des lignes exclues par état de leurs déclarations (unité : la ligne), événement dans la",
            "fenêtre si cause = état d'arrivée et début <= sortie < borne ; cause_inconnue_sortie_dans_fenetre est",
            "une borne, pas un nombre d'événements")),
    c("evenements_declares", "tetes",
      paste("décompte exact des têtes distinctes (id_tete non manquant) des lignes evenement_dans_fenetre, dont les",
            "têtes déjà porteuses d'un événement compté par la construction ; NA si id_tete n'est pas déclaré")),
    c("retraits", "evenements_exposition",
      paste("différence contrefactuelle D_c(R u S) - D_c(R), E_c(R u S) - E_c(R), même recette, convention et",
            "fenêtre que la construction, dans son unité d'observation (unité contrat : E(S) par additivité ;",
            "unité tête : têtes touchées seules) ; décomptes et jours exacts, E^c en arithmétique flottante (nulle",
            "si aucun jour retiré) ; par motif, les différences ne s'additionnent pas")),
    c("retraits", "tetes_indeterminees",
      paste("unité tête, garde de réintégration : têtes où un motif de tête apparaît sur R_t u S (segmentation de",
            "la construction), retirées entièrement des retraits ; une valeur manquante (naissance, sexe, sexe",
            "hors modalités, segment) ne rend pas une tête indéterminée : il faut deux valeurs valides distinctes ;",
            "lignes_indeterminees : lignes de S de ces têtes ; NA en unité contrat")),
    c("retraits", "niveaux",
      paste("niveau A : population regroupée, cellules âge (x année si par_annee) ; niveau B : cellules de la",
            "construction (segment, âge [, année])")),
    c("smr_comparateur_1", "smr",
      paste("D_e / A, A = somme sur les cellules à exposition retenue positive de E^c_e mu^_r, mu^_r = D_r / E^c_r",
            "(taux de Poisson des lignes conservées, comparateur estimé) ; niveau A : standardisé sur l'âge (et",
            "l'année si par_annee), pas sur le segment")),
    c("smr_comparateur_1", "bornes_exactes",
      paste("exact sous D_e ~ Poisson(theta A), A tenu pour fixe ; le comparateur est estimé : la couverture n'est",
            "pas garantie ; qgamma, approximation numérique")),
    c("smr_comparateur_1", "bornes_asymptotiques",
      "asymptotique : normal sur l'échelle de log(SMR), variance 1 / D_e ; non défini à D_e = 0"),
    c("smr_comparateur_1", "rapport_variance",
      paste("somme E^c_e^2 mu^_r / E^c_r / A : estimation par substitution de Var(A^) / E(D_e) ; Var(A^) exacte sous",
            "Poisson à mu connu, E^c fixe")),
    c("smr_comparateur_1", "sans_exposition_retenue",
      "cellules retirées sans exposition retenue (E^c_r = 0) : D_e et E^c_e restitués à part, hors du SMR"),
    c("smr_comparateur_2", "smr",
      paste("D_e / A, A = somme E^c_e mu~, mu~ = -ln(1 - q~) de la table de l'étape validation, sur sa plage_ages,",
            "appariement de l'étape validation, cellules à A > 0 ; O et A excluent les cellules hors plage",
            "(événements et E^c comptés à part) ; non calculable dès qu'une cellule retirée est sans valeur de table",
            "(cellule_sans_valeur_table, prioritaire) ou à q~ = 1 avec E^c > 0 (taux_table_egal_un) : aucun agrégat",
            "partiel, ces cellules et leurs événements restitués à part ; table par sexe au niveau A : contrefactuel",
            "propre sans les lignes sexe_manquant et sexe_hors_modalites (non calculables)")),
    c("smr_comparateur_2", "bornes",
      paste("relation independante : bornes exactes, exact sous D_e ~ Poisson(theta A), A connu (table, E^c",
            "tenue pour fixe), qgamma, approximation numérique ; bornes asymptotiques, asymptotique (normal sur",
            "l'échelle de log(SMR), variance 1 / D_e ; non défini à D_e = 0) ; relation ajustee_hors_outil :",
            "valeurs ponctuelles seules, bornes non produites (ddl_non_etabli)")),
    c("smr_comparateur_2", "conserves",
      paste("SMR des lignes conservées de l'étape validation (etapes/validation/resultat/statistiques, smr),",
            "côte à côte, sans rapport entre eux")))
  texte_moteur(data.frame(table = vapply(l, `[`, "", 1L), grandeur = vapply(l, `[`, "", 2L),
                          statut = vapply(l, `[`, "", 3L), stringsAsFactors = FALSE))
})

# Contrôle le paramètre de l'étape (valeur ; le nom est contrôlé par
# valider_plan()) : niveau, double nu, 0 < niveau < 1, même code que `brut`.
controler_parametres_importance_exclusions <- function(niveau) {
  if (!forme_exacte(niveau, "double") || length(niveau) != 1L || !is.finite(niveau) || niveau <= 0 || niveau >= 1)
    refuser("importance_exclusions", "niveau_invalide",
            formuler(paste("Le niveau de confiance doit être un nombre (double) unique, sans attribut, fini,",
                           "strictement compris entre 0 et 1.")),
            list(niveau = niveau))
  invisible(TRUE)
}

# Classe de chaque motif (§ 2) dans la configuration : unité d'observation et
# clés des cellules du niveau (`cles` : character(0) au niveau A, segmentation
# de la construction au niveau B, "sexe" pour le comparateur 2 au niveau A
# avec une table par sexe). Rend "M" (mesurable), "cle" (non calculable,
# clé manquante) ou "N" (non mesurable), un par motif.
classe_motifs <- function(motifs, unite, cles) {
  cl <- rep("N", length(motifs))
  cl[motifs %in% MOTIFS_MESURABLES_IMPORTANCE] <- "M"
  if (identical(unite, "contrat")) cl[motifs == "id_tete_manquant"] <- "M"
  if (identical(unite, "contrat")) cl[motifs == "contrat_duplique"] <- "N"
  k <- motifs %in% names(CLES_MOTIFS_IMPORTANCE)
  cl[k] <- ifelse(CLES_MOTIFS_IMPORTANCE[motifs[k]] %in% cles, "cle", "M")
  cl
}

# Seule fonction de classement des lignes exclues (condition 1 du visa
# d'actuary) : une ligne est M si tous ses motifs sont M, sinon N si l'un
# d'eux est N, sinon cle. `lignes_exclues` : liste nommée motif -> numéros de
# ligne (resultat$lignes_exclues de `donnees`). Rend list(lignes (croissantes,
# distinctes), classe).
classer_lignes_exclues <- function(lignes_exclues, unite, cles) {
  rang <- match(classe_motifs(names(lignes_exclues), unite, cles), c("M", "cle", "N"))
  l <- unlist(lignes_exclues, use.names = FALSE)
  if (is.null(l)) l <- integer(0)
  r <- rep.int(rang, lengths(lignes_exclues))
  o <- order(l, -r, method = "radix")
  prem <- !duplicated(l[o])
  list(lignes = l[o][prem], classe = c("M", "cle", "N")[r[o][prem]])
}

# Clé de cellule (clés de segmentation, âge [, année]) de chaque ligne d'une
# table de cellules.
cle_cellules <- function(t, cles, par_annee)
  do.call(paste, c(unname(as.list(t[c(cles, "age", if (isTRUE(par_annee)) "annee")])), list(sep = "\r")))

# Table d'exposition des lignes `lignes` de l'entrée (numéros croissants),
# segmentée par `cles` : construire_observations() puis calculer_exposition(),
# une seule implémentation de la fusion et de l'exposition (M242), sous la
# recette et la convention validées de la construction (ctx$recette,
# ctx$conv : une seule source, #82). NULL sans ligne. Sans refus de la période
# sans exposition (refuser_vide = FALSE, #210) : un sous-ensemble de lignes
# exclues peut n'avoir aucune exposition dans la période.
exposition_lignes <- function(lignes, cles, ctx) {
  if (!length(lignes)) return(NULL)
  ob <- construire_observations(ctx$donnees[lignes, , drop = FALSE], lignes, ctx$roles, cles, ctx$p)
  if (!identical(ob$unite, ctx$unite))
    stop("exposition_lignes() : unité d'observation différente de la construction (erreur de programmation).",
         call. = FALSE)
  calculer_exposition(ob$observations, ctx$p, cles, ctx$recette, ctx$conv,
                      ctx$par_annee, refuser_vide = FALSE)$resultat$exposition
}

# Événements et exposition retirés par l'ensemble S (numéros de lignes
# exclues mesurables), cellules segmentées par `cles` (niveau). Unité
# contrat : exposition de S seule (additivité), sans garde. Unité tête :
# têtes touchées T (id_tete des lignes de S), lignes conservées R_t de ces
# têtes ; garde de réintégration : evaluer_motifs() sur le sous-tableau R_t
# u S, lignes_tete = toutes ses lignes (indices relatifs), segmentation de
# la construction, seuls ses motifs de tête lus (ses motifs de ligne, dont
# contrat_duplique, dépendent du jeu entier) ; une tête qui porte un motif de
# tête est indéterminée et sort entièrement ; puis deux passages, R_t u S et
# R_t, sur les têtes restantes. Invariants (erreur de programmation) : S n'a
# que des lignes M (classer_lignes_exclues()) ; D_e,c >= 0, jours >= 0 ;
# E^c_e,c = 0 si aucun jour retiré, > 0 sinon. Rend list(cellules (clés,
# age [, annee], evenements_retires, exposition_jours_retiree,
# exposition_centrale_retiree ; cellules à jours ou événements retirés
# non nuls), tetes_touchees, tetes_indeterminees, lignes_indeterminees).
retrait_contrefactuel <- function(S, cles, ctx) {
  cl <- classer_lignes_exclues(ctx$lignes_exclues, ctx$unite, cles)
  mesurables <- cl$lignes[cl$classe == "M"]
  if (!length(S) || !all(S %in% mesurables) || is.unsorted(S, strictly = TRUE))
    stop("retrait_contrefactuel() : S vide, non trié ou hors des lignes mesurables (erreur de programmation).",
         call. = FALSE)
  pa <- ctx$par_annee
  colonnes <- c(cles, "age", if (isTRUE(pa)) "annee")
  if (identical(ctx$unite, "contrat")) {
    t_rs <- exposition_lignes(S, cles, ctx); t_r <- NULL
    touchees <- indeterminees <- lignes_ind <- NA_integer_
  } else {
    idt <- ctx$id_tete
    tt <- unique(idt[S])
    r_t <- ctx$R[idt[ctx$R] %in% tt]
    L <- sort(c(r_t, S))
    # Aucune ligne N (ni cle) n'entre dans lignes_tete (condition 1 du visa).
    if (any(L %in% cl$lignes[cl$classe != "M"]))
      stop("retrait_contrefactuel() : ligne non mesurable dans lignes_tete (erreur de programmation).", call. = FALSE)
    ev <- evaluer_motifs(ctx$donnees[L, , drop = FALSE], ctx$roles, ctx$p$debut, ctx$p$fin, ctx$p$borne,
                         ctx$segmentation, lignes_tete = seq_along(L))
    ti <- unique(idt[L[unique(unlist(ev$motifs[MOTIFS_TETE], use.names = FALSE))]])
    garde <- !(idt[L] %in% ti)
    t_rs <- exposition_lignes(L[garde], cles, ctx)
    t_r <- exposition_lignes(L[garde & L %in% ctx$R], cles, ctx)
    touchees <- length(tt); indeterminees <- length(ti); lignes_ind <- sum(idt[S] %in% ti)
  }
  if (is.null(t_rs))
    return(list(cellules = retrait_vide(ctx, cles), tetes_touchees = touchees, tetes_indeterminees = indeterminees,
                lignes_indeterminees = lignes_ind))
  De <- t_rs$evenements; Je <- t_rs$exposition_jours; Ee <- t_rs$exposition_centrale
  if (!is.null(t_r)) {
    k_rs <- cle_cellules(t_rs, cles, pa); k_r <- cle_cellules(t_r, cles, pa)
    k <- match(k_rs, k_r)
    absentes <- !(k_r %in% k_rs) & (t_r$evenements > 0L | t_r$exposition_jours > 0)
    if (any(absentes))
      stop("retrait_contrefactuel() : cellule de R absente de R u S (erreur de programmation).", call. = FALSE)
    a <- !is.na(k)
    De[a] <- De[a] - t_r$evenements[k[a]]
    Je[a] <- Je[a] - t_r$exposition_jours[k[a]]
    Ee[a] <- Ee[a] - t_r$exposition_centrale[k[a]]
  }
  if (any(De < 0L) || any(Je < 0))
    stop("retrait_contrefactuel() : événements ou jours retirés négatifs (erreur de programmation).", call. = FALSE)
  Ee[Je == 0] <- 0
  if (any(Je > 0 & !(Ee > 0)))
    stop("retrait_contrefactuel() : exposition centrale retirée non positive avec des jours retirés (erreur de programmation).",
         call. = FALSE)
  garder <- De > 0L | Je > 0
  cel <- t_rs[garder, colonnes, drop = FALSE]
  rownames(cel) <- NULL
  cel$evenements_retires <- De[garder]
  cel$exposition_jours_retiree <- Je[garder]
  cel$exposition_centrale_retiree <- Ee[garder]
  list(cellules = cel, tetes_touchees = touchees, tetes_indeterminees = indeterminees,
       lignes_indeterminees = lignes_ind)
}

# Cellules des lignes conservées au niveau A : table d'exposition de la
# construction sommée sur les segments, par âge (et année), dans l'ordre
# (âge [, année]).
cellules_regroupees <- function(exposition, par_annee) {
  k <- cle_cellules(exposition, character(0), par_annee)
  u <- unique(k)
  g <- match(k, u)
  prem <- exposition[!duplicated(g), c("age", if (isTRUE(par_annee)) "annee"), drop = FALSE]
  s <- rowsum(cbind(as.numeric(exposition$evenements), exposition$exposition_jours, exposition$exposition_centrale),
              g, reorder = TRUE)
  t <- prem
  t$evenements <- as.integer(s[, 1L]); t$exposition_jours <- s[, 2L]; t$exposition_centrale <- s[, 3L]
  o <- if (isTRUE(par_annee)) order(t$age, t$annee, method = "radix") else order(t$age, method = "radix")
  t <- t[o, , drop = FALSE]
  rownames(t) <- NULL
  t
}

# Comparateur 1 (§ 1.4) : taux bruts de Poisson des lignes conservées,
# mu^_r,c = D_r,c / E^c_r,c (taux_poisson(), module brut), cellule par
# cellule. `ret` : cellules retirées ; `conservees` : cellules des lignes
# conservées au même niveau. Rend list(cellules (ret complétée :
# evenements_conserves, exposition_centrale_conservee, taux_conserves,
# attendus_comparateur_1, etat_cellule), ligne (valeurs du SMR ; état
# sans_exposition_retenue si aucune cellule retirée n'a d'exposition
# retenue)).
smr_comparateur_1 <- function(ret, conservees, cles, par_annee, niveau) {
  k <- match(cle_cellules(ret, cles, par_annee), cle_cellules(conservees, cles, par_annee))
  mu_r <- taux_poisson(conservees$evenements, conservees$exposition_centrale, niveau)$taux
  Er <- numeric(nrow(ret)); Dr <- integer(nrow(ret)); mu <- rep(NA_real_, nrow(ret))
  a <- !is.na(k)
  Er[a] <- conservees$exposition_centrale[k[a]]; Dr[a] <- conservees$evenements[k[a]]
  ok <- Er > 0
  mu[ok] <- mu_r[k[ok]]
  att <- rep(NA_real_, nrow(ret)); att[ok] <- ret$exposition_centrale_retiree[ok] * mu[ok]
  ret$evenements_conserves <- Dr
  ret$exposition_centrale_conservee <- Er
  ret$taux_conserves <- mu
  ret$attendus_comparateur_1 <- att
  # character même sans cellule (#177) : ifelse() rend logical(0) sur ok vide.
  etat_cel <- rep("sans_exposition_retenue", nrow(ret)); etat_cel[ok] <- "produite"
  ret$etat_cellule <- etat_cel
  O <- sum(as.numeric(ret$evenements_retires[ok])); A <- sum(att[ok])
  tp <- taux_poisson(O, A, niveau)
  # Sommes d'événements rangées en integer (#199) : gardes défensives ; sous la
  # forme fenêtres, seule admise ici, le total est majoré par le nombre de lignes
  # (voir total_evenements_retires()).
  list(cellules = ret,
       ligne = list(observes = denombrement_entier(O, "importance_exclusions", "smr_comparateur_1/observes"),
                    attendus = A, smr = tp$taux, borne_inf_exacte = tp$borne_inf_exacte,
                    borne_sup_exacte = tp$borne_sup_exacte, borne_inf_asymptotique = tp$borne_inf_asymptotique,
                    borne_sup_asymptotique = tp$borne_sup_asymptotique,
                    rapport_variance = if (A > 0) sum(ret$exposition_centrale_retiree[ok]^2 * mu[ok] / Er[ok]) / A
                                       else NA_real_,
                    cellules_sans_exposition_retenue = sum(!ok),
                    evenements_sans_exposition_retenue = denombrement_entier(
                      sum(as.numeric(ret$evenements_retires[!ok])), "importance_exclusions",
                      "smr_comparateur_1/evenements_sans_exposition_retenue"),
                    exposition_centrale_sans_exposition_retenue = sum(ret$exposition_centrale_retiree[!ok]),
                    etat = if (nrow(ret) && !any(ok)) "sans_exposition_retenue"
                           else switch(tp$etat, defini = "produite", exposition_nulle = "aucun_attendu",
                                       evenement_sans_exposition = "observe_sans_attendu")))
}

# Comparateur 2 (§ 1.4 ; M242, M255) : table de l'étape validation sur sa
# plage_ages, même appariement (apparier_table(), sans refus des cellules
# sans valeur) et mêmes attendus (attendus_cellules()) ; O et A sur les
# cellules à A > 0 ; cellules hors plage exclues de O et A, restituées à
# part ; relation ajustee_hors_outil : bornes non produites. Tout ou rien
# (avis d'actuary, C2) : une cellule retirée sans valeur de table (sexe
# absent) ou à q~ = 1 avec E^c > 0 rend l'étape non calculable (O, A, SMR
# et bornes NA, aucun agrégat partiel) ; les cellules à q~ = 1 sont écartées
# avant attendus_cellules() (aucun refus taux_table_egal_un ici). Ordre des
# états : cellule_sans_valeur_table l'emporte sur taux_table_egal_un, qui
# l'emporte sur les états de taux_poisson(). Les décomptes des deux cas sont
# toujours restitués.
smr_comparateur_2 <- function(ret, cles, par_annee, table, plage_ages, cle_table, relation_table, niveau) {
  sans_valeur <- un <- 0L; ev_sans_valeur <- ev_un <- ev_sans_attendu <- ev_hors <- e_hors <- 0
  O <- 0; A <- 0
  if (nrow(ret)) {
    ex <- ret[c(cles, "age", if (isTRUE(par_annee)) "annee")]
    ex$exposition_centrale <- ret$exposition_centrale_retiree
    ex$evenements <- ret$evenements_retires
    ap <- apparier_table(ex, cles, par_annee, table, plage_ages, cle_table, "validation", refuser_sans_valeur = FALSE)
    c_ <- ap$cellules
    sv <- is.na(c_$qx_table)
    rempli <- c_$exposition_centrale > 0 | c_$evenements > 0L
    sans_valeur <- sum(sv & rempli); ev_sans_valeur <- sum(as.numeric(c_$evenements[sv]))
    q1 <- !sv & c_$qx_table == 1 & c_$exposition_centrale > 0
    un <- sum(q1); ev_un <- sum(as.numeric(c_$evenements[q1]))
    g <- !sv & !q1
    at <- attendus_cellules(c_$exposition_centrale[g], c_$evenements[g], c_$qx_table[g], cle_table, "validation")
    r <- at$etat == "retenue"
    O <- sum(as.numeric(c_$evenements[g][r])); A <- sum(at$attendus[r])
    ev_sans_attendu <- sum(as.numeric(c_$evenements[g][at$etat %in% c("evenement_sans_exposition",
                                                                    "evenement_sous_taux_nul")]))
    ev_hors <- ap$hors_plage$evenements; e_hors <- ap$hors_plage$exposition
  }
  tp <- taux_poisson(O, A, niveau)
  # Bornes produites sous independante seule ; jamais appelée sous
  # positionnee (comparateur 2 non produit, Q6 ; F5).
  ind <- switch(relation_table, independante = TRUE, ajustee_hors_outil = FALSE,
                stop("smr_comparateur_2() : relation sans comparateur 2 (erreur de programmation).", call. = FALSE))
  etat <- if (sans_valeur > 0L) "cellule_sans_valeur_table"
          else if (un > 0L) "taux_table_egal_un"
          else switch(tp$etat, defini = "produite", exposition_nulle = "aucun_attendu",
                      evenement_sans_exposition = "observe_sans_attendu")
  nc <- etat %in% c("cellule_sans_valeur_table", "taux_table_egal_un")
  b <- function(v) if (ind && !nc) v else NA_real_
  # Sommes d'événements rangées en integer (#199) : gardes défensives ; sous la
  # forme fenêtres, seule admise ici, le total est majoré par le nombre de lignes
  # (voir total_evenements_retires()) ; calculs et états en double ci-dessus.
  ent <- function(v, g) denombrement_entier(v, "importance_exclusions", paste0("smr_comparateur_2/", g))
  list(observes = ent(if (nc) NA_real_ else O, "observes"), attendus = if (nc) NA_real_ else A,
       smr = if (nc) NA_real_ else tp$taux,
       borne_inf_exacte = b(tp$borne_inf_exacte), borne_sup_exacte = b(tp$borne_sup_exacte),
       borne_inf_asymptotique = b(tp$borne_inf_asymptotique), borne_sup_asymptotique = b(tp$borne_sup_asymptotique),
       cellules_sans_valeur_table = sans_valeur,
       evenements_sans_valeur_table = ent(ev_sans_valeur, "evenements_sans_valeur_table"),
       cellules_taux_table_egal_un = un, evenements_taux_table_egal_un = ent(ev_un, "evenements_taux_table_egal_un"),
       evenements_sans_attendu = ent(ev_sans_attendu, "evenements_sans_attendu"),
       evenements_hors_plage = ent(ev_hors, "evenements_hors_plage"),
       exposition_centrale_hors_plage = e_hors, etat = etat,
       etat_intervalle = if (!identical(etat, "produite")) etat else if (ind) "produite" else "ddl_non_etabli")
}

# État des déclarations de chaque ligne `lignes` (§ 1.2) : cause connue (dans
# MODALITES_CAUSE) ou non, sortie datée ou non, dans la fenêtre [début,
# borne) ou non ; `cible` : état d'arrivée de la transition.
etats_evenements_exclus <- function(donnees, roles, lignes, debut, borne, cible) {
  cause <- as.character(donnees[[roles[["cause_sortie"]]]][lignes])
  sor <- as.numeric(unclass(donnees[[roles[["date_sortie"]]]][lignes]))
  connue <- !is.na(cause) & cause %in% MODALITES_CAUSE
  datee <- !is.na(sor)
  dans <- datee & sor >= debut & sor < borne
  etat <- rep("sans_evenement_dans_fenetre", length(lignes))
  etat[connue & cause == cible & dans] <- "evenement_dans_fenetre"
  etat[connue & cause == cible & !datee] <- "evenement_date_inconnue"
  etat[!connue & dans] <- "cause_inconnue_sortie_dans_fenetre"
  etat[!connue & !datee] <- "cause_et_sortie_inconnues"
  etat
}

# Ligne d'une table de résultat : data.frame d'une ligne (ensemble, niveau,
# valeurs de la liste v).
ligne_resultat <- function(ensemble, niveau, v)
  data.frame(c(list(ensemble = ensemble, niveau = niveau), v), stringsAsFactors = FALSE)

# Étape `importance_exclusions` du plan (#69).
#   donnees, roles     : entrée de construire_table() (data.frame reçu) ;
#   etape_donnees      : etapes$donnees (parametres, resultat) ;
#   etape_exposition   : etapes$exposition (parametres, resultat) ;
#   etape_validation   : etapes$validation, ou NULL si `validation` n'est pas
#                        au plan (comparateur 2 non demandé) ; sous la
#                        relation positionnee, comparateur 2 non produit ;
#   table              : objet table_reference de l'étape validation, ou NULL ;
#   p                  : valeurs validées de la transition, de la période et
#                        du délai (valeurs_parametres_donnees()), dont
#                        p$periode et p$delai_declaration (#182) ;
#   segmentation       : champ du plan ;
#   recette, conv      : recette et convention validées (deduire_recette(),
#                        resoudre_convention()), celles de l'étape exposition
#                        et de la fiche : les expositions contrefactuelles
#                        (exposition_lignes()) les emploient telles quelles ;
#   niveau             : paramètre de l'étape.
# Aucun contrôle (#82) : construire_table() a validé la segmentation et le
# niveau (valider_plan()) avant toute étape.
# Rend list(parametres, resultat (volumes, motifs, evenements_declares,
# retraits, cellules_niveau_a, cellules_niveau_b, smr_comparateur_1,
# smr_comparateur_2), diagnostics, avertissements). Aucun vecteur par ligne.
calculer_importance_exclusions <- function(donnees, roles, etape_donnees, etape_exposition, etape_validation, table,
                                           p, recette, conv, segmentation, niveau) {
  if (is.null(etape_validation) != is.null(table))
    stop("calculer_importance_exclusions() : étape validation et table incohérentes (erreur de programmation).",
         call. = FALSE)
  # Contrefactuel par lignes et fusion par tête : étape admise sans condition
  # par la ligne de matrice de la recette, et seam « fenêtres » ; le refus
  # utilisateur est levé en amont par controler_matrice_recette()
  # (importance_exclusions_agregats). Propriétés de la recette, jamais son
  # nom (ADR 0016, points 4.4 et 4.6).
  m <- recette$matrice
  ligne <- m[m$etape == "importance_exclusions" & is.na(m$estimateur), , drop = FALSE]
  if (nrow(ligne) != 1L || !identical(ligne$admis, "oui"))
    stop("calculer_importance_exclusions() : étape non admise par la matrice de la recette (erreur de programmation).",
         call. = FALSE)
  if (!identical(recette$seam, "fenetres"))
    stop("calculer_importance_exclusions() : recette sans fenêtres (erreur de programmation).", call. = FALSE)
  rd <- etape_donnees$resultat
  pe <- etape_exposition$parametres
  unite <- rd$unite_observation
  le <- rd$lignes_exclues
  n <- nrow(donnees)
  if (!identical(as.numeric(n), as.numeric(length(rd$numeros_conserves) + rd$nombre_lignes_exclues)))
    stop("calculer_importance_exclusions() : lignes reçues différentes de conservées + exclues (erreur de programmation).",
         call. = FALSE)
  par_annee <- pe$par_annee
  expo <- etape_exposition$resultat$exposition
  ctx <- list(donnees = donnees, roles = roles, p = p,
              recette = recette, conv = conv, par_annee = par_annee,
              unite = unite, segmentation = segmentation, lignes_exclues = le, R = rd$numeros_conserves,
              id_tete = if ("id_tete" %in% names(roles)) donnees[[roles[["id_tete"]]]],
              prototype = expo[0L, c(segmentation, "age", if (isTRUE(par_annee)) "annee"), drop = FALSE])
  # Ensembles : chaque motif déclaré (S_k), puis le total (lignes distinctes).
  ens <- c(names(le), "total")
  lignes_ens <- c(le, list(total = sort(unique(c(integer(0), unlist(le, use.names = FALSE))))))
  niveaux <- list(A = character(0), B = segmentation)
  classes <- lapply(niveaux, function(cl) classer_lignes_exclues(le, unite, cl))
  de_classe <- function(lig, cl, k) lig[lig %in% cl$lignes[cl$classe == k]]
  cache <- list()
  # Motif déclaré sans ligne (ou aucune exclusion : total vide ; avis
  # d'actuary, C1) : retrait nul, sans calcul contrefactuel (têtes 0 en unité
  # tête, NA en unité contrat).
  zero <- if (identical(unite, "tete")) 0L else NA_integer_
  retrait <- function(S, cles) {
    if (!length(S))
      return(list(cellules = retrait_vide(ctx, cles), tetes_touchees = zero, tetes_indeterminees = zero,
                  lignes_indeterminees = zero))
    for (e in cache) if (identical(e$cles, cles) && identical(e$S, S)) return(e$r)
    r <- retrait_contrefactuel(S, cles, ctx)
    cache[[length(cache) + 1L]] <<- list(cles = cles, S = S, r = r)
    r
  }

  # État initial d'un ensemble de lignes `lig` (S : ses lignes M) : produite
  # si S est non vide ou si l'ensemble n'a aucune ligne (retrait nul) ;
  # sinon cle_segmentation_manquante si une ligne est cle,
  # contrat_duplique_unite_contrat pour ce motif en unité contrat, sinon
  # aucune_ligne_mesurable (lignes toutes non mesurables).
  etat_ensemble <- function(ensemble, lig, S, n_cle)
    if (length(S) || !length(lig)) "produite" else if (n_cle) "cle_segmentation_manquante"
    else if (identical(unite, "contrat") && identical(ensemble, "contrat_duplique")) "contrat_duplique_unite_contrat"
    else "aucune_ligne_mesurable"
  indeterminee <- function(r) identical(unite, "tete") && r$tetes_touchees > 0L &&
    r$tetes_indeterminees == r$tetes_touchees

  # 1.1 Volumes et lignes par classe.
  volumes <- data.frame(lignes_recues = n, lignes_conservees = length(rd$numeros_conserves),
                        lignes_exclues = rd$nombre_lignes_exclues)
  motifs <- do.call(rbind, lapply(seq_along(ens), function(i) {
    lig <- lignes_ens[[i]]
    data.frame(ensemble = ens[i], lignes = length(lig),
               lignes_mesurables_niveau_a = length(de_classe(lig, classes$A, "M")),
               lignes_mesurables_niveau_b = length(de_classe(lig, classes$B, "M")),
               lignes_non_calculables_niveau_b = length(de_classe(lig, classes$B, "cle")),
               lignes_non_mesurables = length(de_classe(lig, classes$A, "N")), stringsAsFactors = FALSE)
  }))

  # 1.2 Événements déclarés sur les lignes exclues.
  tout <- lignes_ens$total
  etat_l <- etats_evenements_exclus(donnees, roles, tout, p$debut, p$borne, p$transition$arrivee)
  obs <- rd$observations
  ofin <- as.numeric(obs$fin)
  compte <- obs$cause == p$transition$arrivee & !is.na(ofin) & ofin >= p$debut & ofin < p$borne
  id_obs <- if (identical(unite, "tete")) obs$unite else obs$id_tete
  tetes_comptees <- unique(id_obs[compte])
  evenements <- do.call(rbind, lapply(seq_along(ens), function(i) {
    m <- match(lignes_ens[[i]], tout)
    e <- etat_l[m]
    d <- data.frame(ensemble = ens[i], lignes = length(m), stringsAsFactors = FALSE)
    for (s in ETATS_EVENEMENTS_EXCLUS) d[[s]] <- sum(e == s)
    ids <- if (is.null(ctx$id_tete)) NULL else ctx$id_tete[lignes_ens[[i]][e == "evenement_dans_fenetre"]]
    u <- unique(ids[!is.na(ids)])
    d$tetes_evenement_dans_fenetre <- if (is.null(ids)) NA_integer_ else length(u)
    d$tetes_deja_porteuses <- if (is.null(ids)) NA_integer_ else sum(u %in% tetes_comptees)
    d$lignes_evenement_sans_identifiant_tete <- if (is.null(ids)) NA_integer_ else sum(is.na(ids))
    d
  }))

  # 1.3 et 1.4 Retraits (niveaux A et B) et comparateur 1.
  conservees <- list(A = cellules_regroupees(expo, par_annee), B = expo)
  retraits <- list(); cellules <- list(A = list(), B = list()); smr1 <- list()
  for (nv in names(niveaux)) for (i in seq_along(ens)) {
    lig <- lignes_ens[[i]]
    S <- de_classe(lig, classes[[nv]], "M")
    n_cle <- length(de_classe(lig, classes[[nv]], "cle"))
    v <- list(lignes_mesurables = length(S), lignes_non_calculables = n_cle, tetes_touchees = NA_integer_,
              tetes_indeterminees = NA_integer_, lignes_indeterminees = NA_integer_, evenements_retires = NA_integer_,
              exposition_jours_retiree = NA_real_, exposition_centrale_retiree = NA_real_)
    etat <- etat_ensemble(ens[i], lig, S, n_cle)
    if (identical(etat, "produite")) {
      r <- retrait(S, niveaux[[nv]])
      v$tetes_touchees <- r$tetes_touchees; v$tetes_indeterminees <- r$tetes_indeterminees
      v$lignes_indeterminees <- r$lignes_indeterminees
      if (indeterminee(r)) {
        etat <- "tete_indeterminee_apres_reintegration"
      } else {
        v$evenements_retires <- total_evenements_retires(r$cellules$evenements_retires, ens[i], nv)  # integer (#174)
        v$exposition_jours_retiree <- sum(r$cellules$exposition_jours_retiree)
        v$exposition_centrale_retiree <- sum(r$cellules$exposition_centrale_retiree)
        s1 <- smr_comparateur_1(r$cellules, conservees[[nv]], niveaux[[nv]], par_annee, niveau)
        cel <- s1$cellules
        if (nrow(cel)) cellules[[nv]][[length(cellules[[nv]]) + 1L]] <- cbind(ensemble = rep(ens[i], nrow(cel)), cel,
                                                                               stringsAsFactors = FALSE)
        smr1[[length(smr1) + 1L]] <- ligne_resultat(ens[i], nv, s1$ligne)
      }
    }
    v$etat <- etat
    retraits[[length(retraits) + 1L]] <- ligne_resultat(ens[i], nv, v)
    if (!identical(etat, "produite"))
      smr1[[length(smr1) + 1L]] <- ligne_resultat(ens[i], nv, list(
        observes = NA_integer_, attendus = NA_real_, smr = NA_real_, borne_inf_exacte = NA_real_,
        borne_sup_exacte = NA_real_, borne_inf_asymptotique = NA_real_, borne_sup_asymptotique = NA_real_,
        rapport_variance = NA_real_, cellules_sans_exposition_retenue = NA_integer_,
        evenements_sans_exposition_retenue = NA_integer_, exposition_centrale_sans_exposition_retenue = NA_real_,
        etat = etat))
  }
  retraits <- do.call(rbind, retraits)
  smr1 <- do.call(rbind, smr1)
  # Ordre des lignes de smr_comparateur_1 : celui de retraits.
  smr1 <- smr1[match(paste(retraits$niveau, retraits$ensemble), paste(smr1$niveau, smr1$ensemble)), , drop = FALSE]
  rownames(smr1) <- NULL
  modele <- function(nv) {
    cel <- smr_comparateur_1(retrait_vide(ctx, niveaux[[nv]]), conservees[[nv]], niveaux[[nv]], par_annee,
                             niveau)$cellules
    cbind(ensemble = character(0), cel, stringsAsFactors = FALSE)
  }
  cellules <- lapply(stats::setNames(names(niveaux), names(niveaux)), function(nv) {
    t <- if (length(cellules[[nv]])) do.call(rbind, cellules[[nv]]) else modele(nv)
    rownames(t) <- NULL
    t
  })

  # Comparateur 2 : si et seulement si `validation` est au plan (M242), hors
  # relation positionnee (#167, Q6 ; F5) : la table de l'étape validation y
  # est estimée sur les lignes conservées, le comparateur 2 n'est pas produit
  # (état non_produite_par_cette_version, en attendant le comparateur 3).
  etat_c2 <- if (is.null(etape_validation)) "non_demandee_par_le_plan"
             else switch(etape_validation$parametres$relation_table, independante = , ajustee_hors_outil = "produite",
                         positionnee = "non_produite_par_cette_version",
                         stop("calculer_importance_exclusions() : relation inconnue (erreur de programmation).",
                              call. = FALSE))
  smr2 <- NULL
  if (identical(etat_c2, "produite")) {
    pv <- etape_validation$parametres
    st <- etape_validation$resultat$statistiques
    val <- function(g) st$valeur[match(paste("smr", g, sep = "/"), paste(st$statistique, st$grandeur, sep = "/"))]
    # Clés des cellules du comparateur 2 : niveau A sans clé (table ensemble)
    # ou par sexe (table par sexe : contrefactuel propre S(2), M255) ;
    # niveau B, segmentation de la construction.
    cles2 <- list(A = if (identical(unique(table$sexe), "ensemble")) character(0) else "sexe", B = segmentation)
    smr2 <- do.call(rbind, lapply(names(cles2), function(nv) do.call(rbind, lapply(seq_along(ens), function(i) {
      cl2 <- classer_lignes_exclues(le, unite, cles2[[nv]])
      S <- de_classe(lignes_ens[[i]], cl2, "M")
      n_cle <- length(de_classe(lignes_ens[[i]], cl2, "cle"))
      etat <- etat_ensemble(ens[i], lignes_ens[[i]], S, n_cle)
      v <- NULL
      if (identical(etat, "produite")) {
        r <- retrait(S, cles2[[nv]])
        if (indeterminee(r))
          etat <- "tete_indeterminee_apres_reintegration"
        else v <- smr_comparateur_2(r$cellules, cles2[[nv]], par_annee, table, pv$plage_ages, pv$table_reference,
                                    pv$relation_table, niveau)
      }
      if (is.null(v))
        v <- list(observes = NA_integer_, attendus = NA_real_, smr = NA_real_, borne_inf_exacte = NA_real_,
                  borne_sup_exacte = NA_real_, borne_inf_asymptotique = NA_real_, borne_sup_asymptotique = NA_real_,
                  cellules_sans_valeur_table = NA_integer_, evenements_sans_valeur_table = NA_integer_,
                  cellules_taux_table_egal_un = NA_integer_, evenements_taux_table_egal_un = NA_integer_,
                  evenements_sans_attendu = NA_integer_, evenements_hors_plage = NA_integer_,
                  exposition_centrale_hors_plage = NA_real_, etat = etat, etat_intervalle = etat)
      ligne_resultat(ens[i], nv, c(list(lignes_mesurables = length(S), lignes_non_calculables = n_cle), v,
                                   list(observes_conserves = denombrement_entier(
                                          val("observes"), "importance_exclusions",
                                          "smr_comparateur_2/observes_conserves"),
                                        attendus_conserves = val("attendus"),
                                        smr_conserves = val("smr"))))
    }))))
    rownames(smr2) <- NULL
  }

  comparateurs <- texte_moteur(data.frame(
    comparateur = 1:3,
    definition = c("taux bruts de Poisson des lignes conservées, mu^_r = D_r / E^c_r, cellule par cellule",
                   "table de référence de l'étape validation (etapes/validation/parametres : table_reference, plage_ages, relation_table)",
                   "table construite"),
    etat = c("produite", etat_c2, "non_produite_par_cette_version"),
    stringsAsFactors = FALSE))
  comparateurs$libelle <- ETATS_IMPORTANCE$libelle[match(comparateurs$etat, ETATS_IMPORTANCE$etat)]
  classes_motifs <- data.frame(motif = names(le), classe_niveau_a = classe_motifs(names(le), unite, character(0)),
                               classe_niveau_b = classe_motifs(names(le), unite, segmentation),
                               stringsAsFactors = FALSE)
  list(parametres = list(niveau = niveau, unite_observation = unite, segmentation = segmentation,
                         par_annee = par_annee, classes_motifs = classes_motifs, comparateurs = comparateurs,
                         etats = ETATS_IMPORTANCE, statuts = STATUTS_IMPORTANCE),
       resultat = list(volumes = volumes, motifs = motifs, evenements_declares = evenements, retraits = retraits,
                       cellules_niveau_a = cellules$A, cellules_niveau_b = cellules$B, smr_comparateur_1 = smr1,
                       smr_comparateur_2 = smr2),
       diagnostics = list(calculs_contrefactuels = length(cache)),
       avertissements = avertissements_vides())
}

# Total des événements retirés d'un ensemble, en integer (#174). La somme est
# formée en double, où elle ne déborde pas ; au-delà de .Machine$integer.max
# l'étape refuse, plutôt que de laisser as.integer() rendre NA avec un simple
# avertissement. Garde défensive : sous la forme fenêtres, seule admise ici, le
# décompte par cellule vient de tabulate() (exposition_fenetres()) et le total
# est majoré par le nombre de lignes des observations ; le cas est donc
# inatteignable par construire_table(), et testé en appel direct. Le refus
# evenements_hors_plage de l'étape exposition ne vaut que pour la forme cellules
# (exposition_cellules()), que cette étape n'admet pas.
total_evenements_retires <- function(evenements, ensemble, niveau) {
  n <- sum(as.numeric(evenements))
  if (n > .Machine$integer.max)
    refuser("importance_exclusions", "evenements_retires_hors_plage",
            formuler(paste("Le total des événements retirés (%.0f) pour l'ensemble « %s », niveau %s, est hors de la",
                           "plage des entiers du moteur (au plus %d) : la construction est refusée."),
                     n, citer(ensemble), citer(niveau),
                     .Machine$integer.max),
            list(ensemble = ensemble, niveau = niveau, evenements_retires = n))
  as.integer(n)
}

# Cellules retirées vides (gabarit des colonnes d'une table de cellules).
retrait_vide <- function(ctx, cles) {
  cel <- ctx$prototype[0L, c(cles, "age", if (isTRUE(ctx$par_annee)) "annee"), drop = FALSE]
  rownames(cel) <- NULL
  cel$evenements_retires <- integer(0); cel$exposition_jours_retiree <- numeric(0)
  cel$exposition_centrale_retiree <- numeric(0)
  cel
}
