# -----------------------------------------------------------------------------
#  R/simulation.R : module `simulation`, simulateur minimal (issue #10 ;
#  E2.19, E2.20 ; ADR 0003, point 9 ; ADR 0007, points 3.2, 7 et 8 ;
#  spécification d'actuary, A.0 et A.2)
#
#  - table_gompertz_makeham(), table_logistique() (#211), table_constante() :
#    tables synthétiques à paramètres déclarés (classe `table_reference`,
#    statut `synthetique`, attribut `parametres`) ;
#  - loi_effective() : loi vraie effective par transition et par sexe,
#    q_x^s = min(f_s q_x, 1), force mu_x = -ln(1 - q_x) ;
#  - jours_civils(), jour_anniversaire(), age_anniversaire() : dates
#    d'anniversaire et âge au dernier anniversaire (T2) en arithmétique
#    entière, sans conversion as.POSIXlt par tête ;
#  - temps_latent() : inversion du risque cumulé d'une transition ;
#  - simuler_portefeuille() : point d'entrée (exporté) ; lignes individuelles
#    sous la loi vraie, deux transitions concurrentes (vivant -> décédé, en
#    cours -> racheté), troncature à gauche (entrée après la naissance,
#    éventuellement avant le début de la période), censure par la fin de la
#    période ; loi vraie, paramètres, graine et générateur consignés dans
#    l'attribut `simulation` ;
#  - controler_forme(), projeter_agregats() : forme « agrégats » (#169 ; ADR
#    0003, point 9, et ADR 0007, point 3.2, annotations du 08/11/2026 ;
#    spécification (0') de #169, § 11) : argument `forme`, contrôlé avant tout
#    tirage, puis projection du jeu propre en cellules (sexe, âge au dernier
#    anniversaire [, année]), sans tirage supplémentaire ni appel à
#    `exposition` ; convention `age_atteint_annee`, unité tête ou contrat
#    sous les modes multi-contrats (fusionner_tetes()) et défauts propres aux
#    agrégats (DEFAUTS_FORMES, injecter_defauts_agregats(), tirer_defauts() ;
#    #189) ;
#  - projeter_stocks() : forme « stocks successifs » (#180 ; spécification
#    d'actuary-approfondi de #180, § 11, M328) : projection du jeu propre en
#    lignes d'inventaire et d'événement sur une grille alignée sur la période,
#    décomptes de la projection dans l'attribut `simulation`, sans tirage
#    supplémentaire ni appel à `exposition` ; défauts propres aux stocks
#    (DEFAUTS_FORMES, injecter_defauts_stocks() ; #191).
#
#  Hypothèse de répartition dans l'année (A.2) : force constante par âge
#  entier, dans l'échelle des années d'âge (A.0, T3 : une année d'âge de
#  L_x = anniv(x+1) - anniv(x) jours vaut 1) ; risques concurrents
#  indépendants. Modes `uniforme` et `age_exact` : une tête = un contrat.
#  Modes `uniforme_multi` et `age_exact_multi` (#92 ; ADR 0007, annotation
#  du 25/10/2026, (c), points 1 à 5) : K contrats tirés par tête, K de loi
#  déclarée, indépendant de la mortalité et du rachat ; sexe, naissance et
#  décès par tête, décès tiré à partir de la première entrée ; entrée et
#  rachat par contrat ; un contrat dont l'entrée suit l'instant de décès de
#  la tête n'est pas écrit (contrat non souscrit).
#  Défauts paramétrés (#93 ; ADR 0007, annotation du 25/10/2026, (c), points
#  6 à 9) : argument `defauts` ; altérations déterministes de lignes ou de
#  têtes du jeu propre, tirées après lui sous leur propre graine, consignées
#  dans l'attribut `defauts` avec le motif ou le refus attendu, lus dans le
#  catalogue CATALOGUE_DEFAUTS (jamais calculés par un contrôle de `donnees` :
#  non-circularité, annotation, (d)).
#  Aléa : tous les tirages sous sous_graine() (outils), dans l'ordre fixé par
#  la spécification, qui fait partie du contrat de reproductibilité.
#  Ce module n'appelle que `donnees` et `outils` ; aucun module du moteur ne
#  l'appelle (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# Transitions simulées : cause de sortie produite -> transition (A.2).
CAUSES_SIMULEES <- c(deces = "vivant -> deces", rachat = "en_cours -> rachat")

# Hypothèse de répartition consignée dans le résultat (A.2).
HYPOTHESE_REPARTITION <- "force constante par âge entier ; risques concurrents indépendants"

# Ordre des tirages (A.2), consigné : il fait partie du contrat.
ORDRE_TIRAGES <- c("1. sexe : runif(n) < proportion_hommes",
                   "2. jour d'entrée : sample.int(nombre de jours de [origine, fin], n, replace = TRUE) (mode uniforme)",
                   "3. âge à l'entrée : age_min + (age_max - age_min) * runif(n) (mode uniforme)",
                   "4. décès : rexp(n)",
                   "5. rachat : rexp(n)")

# Ordre des tirages des modes multi-contrats (#92 ; ADR 0007, annotation du
# 25/10/2026, (c), point 3), consigné de même ; ORDRE_TIRAGES, celui des
# modes existants, est inchangé. N = nombre de contrats tirés = sum(K).
ORDRE_TIRAGES_MULTI <- list(
  uniforme_multi = c(
    "1. sexe : runif(n) < proportion_hommes",
    paste("2. nombre K de contrats par tête : 1 + findInterval(runif(n), cumsum(loi_contrats)[-m]) ;",
          "aucun tirage si loi_contrats n'a qu'une probabilité non nulle"),
    paste("3. jour d'entrée, par contrat tiré (têtes puis contrats) : sample.int(nombre de jours",
          "de [origine, fin], N, replace = TRUE)"),
    "4. âge à la première entrée : age_min + (age_max - age_min) * runif(n)",
    "5. décès, par tête : rexp(n)",
    "6. rachat, par contrat tiré (souscrit ou non) : rexp(N)"),
  age_exact_multi = c(
    "1. sexe : runif(n) < proportion_hommes",
    paste("2. nombre K de contrats par tête : 1 + findInterval(runif(n), cumsum(loi_contrats)[-m]) ;",
          "aucun tirage si loi_contrats n'a qu'une probabilité non nulle"),
    "3. décès, par tête : rexp(n)",
    "4. rachat, par contrat tiré (souscrit ou non) : rexp(N)"))

# Champs du portefeuille, par mode : tous requis, aucun défaut implicite.
# Dans tous les modes, n est le nombre de têtes.
CHAMPS_PORTEFEUILLE <- list(
  uniforme = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine"),
  age_exact = c("mode", "n", "proportion_hommes", "age", "date_entree"),
  uniforme_multi = c("mode", "n", "proportion_hommes", "age_min", "age_max", "origine", "loi_contrats"),
  age_exact_multi = c("mode", "n", "proportion_hommes", "age", "date_entree", "loi_contrats"))

# Mode de base d'un mode multi-contrats : mêmes champs, mêmes contrôles, mêmes
# bornes d'âge.
MODE_BASE <- c(uniforme = "uniforme", age_exact = "age_exact",
               uniforme_multi = "uniforme", age_exact_multi = "age_exact")

# Catalogue des défauts paramétrés (#93 ; spécification d'actuary du
# 25/10/2026, § 2.2 ; ADR 0007, annotation du 25/10/2026, (c), point 6) : un
# type par ligne. unite : ligne ou tête ; deces, rachat, en_cours : causes de
# sortie (du jeu propre) des lignes éligibles, à intersecter avec la cible de
# la déclaration (types de ligne) ; condition : éligibilité supplémentaire
# (entree_avant_fin : entrée < fin de la période ; tete_multi : tête d'au
# moins deux lignes ; tete_multi_deces : idem, dont au moins une à décès ;
# une tête n'est éligible que si aucune de ses lignes n'est déjà altérée) ;
# alteration : altération déterministe ; motif_attendu : motif que le module
# `donnees` doit lever (NA : aucun, refus à la place) ; refus_attendu : refus
# attendu sans règle d'exclusion déclarée pour ce motif. Les attendus sont
# écrits ici, en dur, d'après la spécification : jamais calculés par un
# contrôle de `donnees` (non-circularité, annotation, (d)). Hors périmètre
# (M136) : annee_naissance_incoherente, segment_manquant,
# tete_segment_incoherent (aucune colonne annee_naissance ni segment).
CATALOGUE_DEFAUTS <- data.frame(
  defaut = c("contrat_duplique", "id_contrat_manquant", "id_tete_manquant", "naissance_manquante",
             "entree_manquante", "sortie_manquante", "cause_manquante", "cause_hors_modalites",
             "sexe_manquant", "sexe_hors_modalites", "naissance_apres_entree", "sortie_avant_entree",
             "en_cours_avant_fin", "age_aberrant", "date_hors_domaine",
             "tete_naissance_incoherente", "tete_sexe_incoherent", "tete_observee_apres_deces"),
  unite = c(rep("ligne", 15L), rep("tete", 3L)),
  deces    = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE,  FALSE, TRUE, TRUE,
               TRUE, TRUE, TRUE),
  rachat   = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,  TRUE, TRUE,  TRUE, TRUE, TRUE, TRUE,  FALSE, TRUE, TRUE,
               TRUE, TRUE, TRUE),
  en_cours = c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE,  TRUE, TRUE,
               TRUE, TRUE, TRUE),
  condition = c(rep("", 12L), "entree_avant_fin", "", "", "tete_multi", "tete_multi", "tete_multi_deces"),
  alteration = c("copie de la ligne ajoutée en fin de jeu (lignes N + 1, ...)",
                 "id_contrat <- NA", "id_tete <- NA", "date_naissance <- NA", "date_entree <- NA",
                 "date_sortie <- NA", "cause_sortie <- NA", "cause_sortie <- toupper(cause_sortie)",
                 "sexe <- NA", "sexe <- tolower(sexe)", "date_naissance <- date_entree + 1",
                 "date_sortie <- date_entree - 1", "date_sortie <- date_entree",
                 "date_naissance <- 1er janvier de (année d'entrée - 150)", "date_naissance <- 0000-12-31",
                 "date_naissance de la dernière ligne de la tête <- date_naissance - 1",
                 "sexe de la dernière ligne de la tête inversé (H <-> F)",
                 paste("dernière ligne de la tête autre que sa première ligne à décès : cause_sortie <- en_cours,",
                       "date_sortie <- NA")),
  motif_attendu = c("contrat_duplique", "id_contrat_manquant", "id_tete_manquant", "naissance_manquante",
                    "entree_manquante", "sortie_manquante", "cause_manquante", "cause_hors_modalites",
                    "sexe_manquant", "sexe_hors_modalites", "naissance_apres_entree", "sortie_avant_entree",
                    "en_cours_avant_fin", "age_aberrant", NA,
                    "tete_naissance_incoherente", "tete_sexe_incoherent", "tete_observee_apres_deces"),
  refus_attendu = c(rep("ligne_invalide", 14L), "date_hors_domaine", rep("ligne_invalide", 3L)),
  stringsAsFactors = FALSE)

# Combinaisons de types admises sur une même ligne (spécification, § 2.4 ;
# #57) : liste fermée ; une combinaison nouvelle demande l'avis d'actuary.
# Une déclaration de combinaison porte le vecteur des types (dans un ordre
# quelconque) ; chaque ligne retenue reçoit toutes ses altérations.
COMBINAISONS_DEFAUTS <- list(c("cause_manquante", "sexe_manquant"))

# Ordre des tirages des défauts (spécification, § 2.3 ; ADR 0007, annotation
# du 25/10/2026, (c), point 7, et réponse d'actuary à la question (f)),
# consigné dans l'attribut `defauts`.
ORDRE_TIRAGES_DEFAUTS <- c(
  paste("0. après le tirage complet du jeu propre, un seul sous_graine(defauts$graine), graine différente",
        "de celle du jeu ; déclarations traitées dans l'ordre reçu"),
  paste("1. éligibles : unités non encore altérées, en ordre croissant (lignes du jeu propre ; têtes dont",
        "aucune ligne n'est altérée) ; une tête retenue rend toutes ses lignes inéligibles ensuite"),
  "2. taux : unités retenues = éligibles[runif(nombre d'éligibles) < taux]",
  "3. nombre : unités retenues = sort(éligibles[sample.int(nombre d'éligibles, nombre)])")

# Règles des défauts par forme du jeu simulé (#189 ; avis de forme
# d'architect, Q2, et spécification d'actuary-approfondi de #189, § 1 et § 2) :
# une entrée par forme, indexée par le nom de forme (`lignes` : forme = NULL,
# lignes individuelles), lue par clé, jamais par comparaison du nom. Champs :
# catalogue (data.frame, un type par ligne : defaut, unite, condition,
# alteration, motif_attendu, refus_attendu, plus les colonnes d'éligibilité
# propres à la forme) ; combinaisons (liste fermée des combinaisons admises) ;
# cible (domaine du champ cible : causes de sortie du jeu propre ; vide si
# la forme n'en a pas) ; unites_cible (unités des types dont la cible est
# requise ; interdite pour les autres) ; ordre_tirages et validite (textes
# consignés dans l'attribut `defauts`) ; exclusifs (facultatif : types qui
# doivent être l'unique déclaration ; absent, aucun). Toute forme de
# CHAMPS_FORMES y a son entrée. L'entrée `lignes` reprend CATALOGUE_DEFAUTS,
# COMBINAISONS_DEFAUTS et ORDRE_TIRAGES_DEFAUTS tels quels.
# Agrégats (spécification de #189, § 1) : unité cellule (ligne de la table
# agrégée propre), cible interdite, une cellule altérée au plus une fois,
# copie jamais éligible ; colonnes d'éligibilité exposition_positive
# (exposition_centrale > 0), evenements_nuls (evenements == 0),
# evenements_positifs (evenements >= 1) ; condition « annee » : colonne annee
# présente (par_annee = TRUE), vide structurel sinon (conditions_vides()).
# Attendus écrits en dur d'après la spécification, motifs de
# MOTIFS_LIGNES_AGREGATS (R/donnees.R), jamais calculés par `donnees`.
DEFAUTS_FORMES <- list(
  lignes = list(
    catalogue = CATALOGUE_DEFAUTS, combinaisons = COMBINAISONS_DEFAUTS,
    cible = c("deces", "rachat", "en_cours"), unites_cible = "ligne",
    ordre_tirages = ORDRE_TIRAGES_DEFAUTS,
    validite = paste("Motifs et refus attendus sous trois conditions : les sept rôles déclarés (colonne du même nom),",
                     "un plan de période égale à celle de la simulation, un délai de déclaration nul.",
                     "tete_observee_apres_deces : motif attendu si le premier décès de la tête (colonne",
                     "premier_deces de lignes) précède fin - délai + 1 ; toujours le cas sous un délai nul.",
                     "date_hors_domaine : refus du même nom, quelles que soient les exclusions déclarées.",
                     "Si aucune déclaration date_hors_domaine n'a retenu de ligne : toutes les exclusions des",
                     "motifs attendus déclarées, lignes exclues = lignes de l'attribut, motif par motif ; aucune",
                     "exclusion, refus ligne_invalide.")),
  agregats = list(
    catalogue = data.frame(
      defaut = c("cellule_dupliquee", "exposition_negative", "evenements_non_entiers", "evenements_sans_exposition",
                 "annee_hors_periode"),
      unite = rep("cellule", 5L),
      exposition_positive = c(FALSE, TRUE, TRUE, TRUE, FALSE),
      evenements_nuls = c(FALSE, TRUE, FALSE, FALSE, FALSE),
      evenements_positifs = c(FALSE, FALSE, FALSE, TRUE, FALSE),
      condition = c("", "", "", "", "annee"),
      alteration = c("copie de la cellule ajoutée en fin de table (lignes N + 1, ...)",
                     "chaque colonne d'exposition présente : x <- -x",
                     "evenements <- evenements + 0.5 (colonne evenements passée en double si une cellule au moins est retenue)",
                     "chaque colonne d'exposition présente <- 0",
                     "annee <- annee - n_annees, n_annees = an(fin) - an(début) + 1"),
      motif_attendu = c("cellule_dupliquee", "exposition_negative", "evenements_non_entiers",
                        "evenements_sans_exposition", "annee_hors_periode"),
      refus_attendu = rep("ligne_invalide", 5L),
      stringsAsFactors = FALSE),
    combinaisons = list(), cible = character(0), unites_cible = character(0),
    ordre_tirages = c(
      paste("0. après le tirage complet du jeu propre et sa projection en agrégats (aucun tirage), un seul",
            "sous_graine(defauts$graine), graine différente de celle du jeu ; déclarations traitées dans l'ordre reçu"),
      paste("1. éligibles : cellules de la table agrégée propre non encore altérées, en ordre croissant ; une",
            "cellule est altérée au plus une fois ; une copie (cellule_dupliquee) n'est jamais éligible"),
      ORDRE_TIRAGES_DEFAUTS[3:4]),
    validite = paste("Motifs attendus sous ces conditions : recette agregats, rôles déclarés aux noms des colonnes",
                     "(roles), transition de la forme, plan de période égale à celle de la simulation, délai de",
                     "déclaration nul, convention et par_annee de la forme (convention, par_annee).",
                     "Aucune exclusion déclarée : refus ligne_invalide nommant les motifs attendus ; toutes les",
                     "exclusions des motifs attendus déclarées : lignes exclues = lignes de l'attribut, motif par",
                     "motif. Toute cellule exclue manque à la table (cellule_dupliquee : l'original et la copie) :",
                     "relevé ages_manquants possible.")),
  # Stocks successifs (#191 ; spécification d'actuary-approfondi de #191,
  # § 1 à § 3 ; avis de forme d'architect, partie #191 ; décision du
  # mainteneur du 11/11/2026 : inventaire perdu, toutes les dates de la grille
  # éligibles). Unité de sélection : l'unité inventoriée (id_contrat), altérée
  # au plus une fois tous types confondus ; unite décrit le défaut : ligne
  # (ligne d'inventaire altérée ou copiée), unite (motif d'unité), inventaire
  # (date de la grille). Colonnes d'éligibilité : deces, rachat, en_cours
  # (cause de l'unité dans le jeu propre, à intersecter avec la cible ; NA :
  # sans objet, cible interdite) ; inventaires_min (nombre minimal de lignes
  # d'inventaire de l'unité) ; evenement (ligne d'événement requise) ;
  # condition « grille_trois_dates » : grille d'au moins trois dates, vide
  # structurel sinon (conditions_vides()). inventaire_perdu : type exclusif,
  # refus inventaire_sans_ligne (précédent date_hors_domaine). Attendus écrits
  # en dur d'après la spécification, motifs de MOTIFS_LIGNES_STOCKS et
  # MOTIFS_UNITE_STOCKS (R/donnees.R), jamais calculés par `donnees`.
  stocks_successifs = list(
    catalogue = data.frame(
      defaut = c("sortie_non_renseignee", "presence_interrompue", "inventaire_hors_grille", "inventaire_duplique",
                 "inventaire_perdu"),
      unite = c("unite", "unite", "ligne", "ligne", "inventaire"),
      deces = c(TRUE, TRUE, TRUE, TRUE, NA),
      rachat = c(TRUE, TRUE, TRUE, TRUE, NA),
      en_cours = c(FALSE, TRUE, TRUE, TRUE, NA),
      inventaires_min = c(1L, 3L, 1L, 1L, NA),
      evenement = c(TRUE, FALSE, FALSE, FALSE, NA),
      condition = c("", "grille_trois_dates", "", "", ""),
      alteration = c("ligne d'événement de l'unité retirée",
                     "ligne d'inventaire t_(f+1) de l'unité retirée (f : premier inventaire de l'unité)",
                     "date_inventaire du premier inventaire t_f <- min{d > t_f : d hors de la grille}",
                     "copie du premier inventaire de l'unité ajoutée en fin de jeu (lignes N' + 1, ...)",
                     "toutes les lignes d'inventaire de la date retirées"),
      motif_attendu = c("sortie_non_renseignee", "presence_interrompue", "inventaire_hors_grille",
                        "inventaire_duplique", NA),
      refus_attendu = c(rep("ligne_invalide", 4L), "inventaire_sans_ligne"),
      stringsAsFactors = FALSE),
    combinaisons = list(), cible = c("deces", "rachat", "en_cours"), unites_cible = c("ligne", "unite"),
    exclusifs = "inventaire_perdu",
    ordre_tirages = c(
      paste("0. après le tirage complet du jeu propre et sa projection en stocks successifs (aucun tirage), un",
            "seul sous_graine(defauts$graine), graine différente de celle du jeu ; déclarations traitées dans",
            "l'ordre reçu"),
      paste("1. éligibles : unités inventoriées (id_contrat) non encore altérées, en ordre croissant",
            "d'identifiant (une unité est altérée au plus une fois, tous types confondus) ; inventaire_perdu :",
            "dates de la grille, dans l'ordre de la grille"),
      ORDRE_TIRAGES_DEFAUTS[3:4],
      paste("4. numérotation : lignes de la projection dans leur ordre, moins les lignes retirées, puis copies",
            "(inventaire_duplique) en fin, dans l'ordre des déclarations, puis des unités")),
    validite = paste("Motifs et refus attendus sous ces conditions : recette stocks_successifs, rôles déclarés aux",
                     "noms des colonnes (roles ; identifiant id_contrat seul), plan de période égale à celle de la",
                     "simulation, délai de déclaration nul, grille du plan égale à celle de la forme (inventaires).",
                     "Motifs (sans inventaire_perdu) : aucune exclusion déclarée, refus ligne_invalide nommant les",
                     "motifs attendus ; toutes les exclusions des motifs attendus déclarées, lignes exclues = lignes",
                     "de l'attribut qui portent le motif, motif par motif ; nombre de lignes exclues = ces lignes",
                     "plus celles du rôle propagation (amendement A1 : autres lignes d'une unité porteuse d'un motif",
                     "de ligne, sans motif ; decomptes$lignes_propagation, decomptes$unites_propagation).",
                     "inventaire_perdu (une ou plusieurs dates perdues) : refus inventaire_sans_ligne ; cascade",
                     "attendue, selon les unités présentes. Une date perdue : première date, refus",
                     "inventaire_sans_ligne direct, quelles que soient les exclusions ; date intermédiaire, motifs",
                     "parmi presence_interrompue et evenement_apres_absence (refus ligne_invalide), puis, ces motifs",
                     "exclus, refus inventaire_sans_ligne ; dernière date, motif sortie_non_renseignee, puis, exclu,",
                     "refus inventaire_sans_ligne. Plusieurs dates perdues : aucun motif si elles forment un segment",
                     "initial t_1, ..., t_k de la grille (grille entière comprise), refus inventaire_sans_ligne direct ;",
                     "sinon, motifs cumulés parmi presence_interrompue (date perdue comprise entre deux dates",
                     "conservées), evenement_apres_absence (date perdue autre que la dernière, postérieure à une date",
                     "conservée) et sortie_non_renseignee (dernière date perdue, postérieure à une date conservée),",
                     "refus ligne_invalide, puis, ces motifs exclus, refus inventaire_sans_ligne. Sous exclusion de",
                     "tous les motifs, refus inventaire_sans_ligne aux dates sans présent sain",
                     "(decomptes$presents_sains), dates perdues comprises.")))

# --- tables synthétiques ------------------------------------------------------

# Table de Gompertz-Makeham, mu(x) = A + B c^x, sans distinction de sexe :
# q_x = 1 - exp(-A - B c^x (c - 1) / ln c), intégrale exacte de mu sur
# [x, x+1) (A.2). Paramètres déclarés, sans valeur par défaut.
table_gompertz_makeham <- function(A, B, c, ages) {
  reel <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v)
  if (!reel(A) || !reel(B) || !reel(c) || A < 0 || B < 0 || c <= 1)
    refuser("simulation", "loi_invalide",
            formuler("Gompertz-Makeham : A >= 0, B >= 0 et c > 1, réels finis, à déclarer."),
            list(A = A, B = B, c = c))
  if (!is.numeric(ages) || !length(ages) || anyNA(ages) || any(ages != trunc(ages)) ||
      any(ages < 0))
    refuser("simulation", "loi_invalide",
            formuler("Gompertz-Makeham : les âges sont des entiers >= 0."), list(ages = ages))
  ages <- as.integer(ages)
  qx <- 1 - exp(-A - B * c^ages * (c - 1) / log(c))
  table_reference(data.frame(age = ages, sexe = "ensemble", qx = qx, stringsAsFactors = FALSE),
                  nom = sprintf("Gompertz-Makeham synthétique (A = %s, B = %s, c = %s)",
                                format(A), format(B), format(c)),
                  statut = "synthetique",
                  source = paste("Table synthétique : mu(x) = A + B c^x ;",
                                 "q_x = 1 - exp(-A - B c^x (c - 1) / ln c) (spécification d'actuary, A.2, #10)"),
                  parametres = list(loi = "gompertz_makeham", A = A, B = B, c = c,
                                    ages = range(ages)))
}

# Table logistique, mu(x) = a + b e^(cx) / (1 + b e^(cx)), sans distinction de
# sexe (Lignes directrices mortalité, Institut des actuaires, 2006, p. 25/40 ;
# a = 0 : modèle de Kannisto ; spécification d'actuary, #211). Risque cumulé
# H(x) = a x + ln(1 + b e^(cx)) / c ; avec v = plogis(ln b + c x) (identité
# exacte), Delta H_x = a + log1p(expm1(c) v) / c et q_x = -expm1(-Delta H_x) :
# intégrale exacte de mu sur [x, x+1) (A.2), sans débordement de b e^(cx)
# (plateau q -> 1 - exp(-(a + 1))). exp(c) fini exigé : au-delà, q vaudrait 1
# à tort. Paramètres déclarés, sans valeur par défaut.
table_logistique <- function(a, b, c, ages) {
  reel <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v)
  if (!reel(a) || !reel(b) || !reel(c) || a < 0 || b <= 0 || c <= 0 || !is.finite(expm1(c)))
    refuser("simulation", "loi_invalide",
            formuler("Logistique : a >= 0, b > 0 et c > 0, réels finis, exp(c) fini, à déclarer."),
            list(a = a, b = b, c = c))
  if (!is.numeric(ages) || !length(ages) || anyNA(ages) || any(ages != trunc(ages)) ||
      any(ages < 0))
    refuser("simulation", "loi_invalide",
            formuler("Logistique : les âges sont des entiers >= 0."), list(ages = ages))
  ages <- as.integer(ages)
  v <- stats::plogis(log(b) + c * ages)
  qx <- -expm1(-(a + log1p(expm1(c) * v) / c))
  table_reference(data.frame(age = ages, sexe = "ensemble", qx = qx, stringsAsFactors = FALSE),
                  nom = sprintf("Logistique synthétique (a = %s, b = %s, c = %s)",
                                format(a), format(b), format(c)),
                  statut = "synthetique",
                  source = paste("Table synthétique : mu(x) = a + b e^(cx) / (1 + b e^(cx)) (Lignes directrices",
                                 "mortalité, Institut des actuaires, 2006, p. 25/40 ; a = 0 : modèle de Kannisto) ;",
                                 "q_x = 1 - exp(-a - log1p(expm1(c) plogis(ln b + c x)) / c) (spécification",
                                 "d'actuary, #211)"),
                  parametres = list(loi = "logistique", a = a, b = b, c = c, ages = range(ages)))
}

# Table à q_x constant sur les âges `ages`, sans distinction de sexe (loi de
# rachat de la branche, A.2 ; q = 0 pour une transition absente).
table_constante <- function(q, ages, nom) {
  if (!is.numeric(q) || length(q) != 1L || !is.finite(q) || q < 0 || q > 1)
    refuser("simulation", "loi_invalide", formuler("Table constante : q réel fini dans [0, 1]."), list(q = q))
  if (!is.numeric(ages) || !length(ages) || anyNA(ages) || any(ages != trunc(ages)) ||
      any(ages < 0))
    refuser("simulation", "loi_invalide", formuler("Table constante : les âges sont des entiers >= 0."),
            list(ages = ages))
  ages <- as.integer(ages)
  table_reference(data.frame(age = ages, sexe = "ensemble", qx = rep(as.numeric(q), length(ages)),
                             stringsAsFactors = FALSE),
                  nom = nom, statut = "synthetique",
                  source = "Table synthétique à q_x constant (spécification d'actuary, A.2, #10)",
                  parametres = list(loi = "constante", q = q, ages = range(ages)))
}

# --- loi vraie effective --------------------------------------------------------

# Loi d'une transition : list(table = <table_reference>, facteurs = c(H =,
# F =)). Pour chaque sexe de `sexes` (les sexes tirés, de proportion non
# nulle), la table du sexe s'il y figure, sinon la table `ensemble` ;
# q_x^s = min(f_s q_x, 1). Un sexe non tiré n'est ni construit ni contrôlé.
# Rend un data.frame (cause, sexe, age, qx, mu) trié par sexe puis âge ;
# mu = -ln(1 - q_x), Inf si q_x = 1.
loi_effective <- function(loi_t, cause, sexes) {
  if (!liste_aux_noms(loi_t, c("facteurs", "table")))
    refuser("simulation", "loi_invalide",
            formuler("Loi « %s » : liste (table, facteurs) attendue.", cause), list(cause = cause))
  tab <- loi_t$table; f <- loi_t$facteurs
  if (!inherits(tab, "table_reference"))
    refuser("simulation", "loi_invalide",
            formuler("Loi « %s » : la table doit être de classe table_reference (ADR 0007, point 7).", cause),
            list(cause = cause))
  if ("annee" %in% names(tab))
    refuser("simulation", "loi_invalide",
            formuler("Loi « %s » : table prospective (colonne annee) non prise en charge par le simulateur minimal.", cause),
            list(cause = cause))
  # Facteurs : forme exacte (#58, forme_exacte()) : double ou integer, seul
  # attribut names (ni classe, ni dim), de noms exactement H et F, sans tri :
  # setequal() ne dépend pas de la locale et ne lève pas pour un nom non ASCII
  # natif (locale C) ou en octets invalides (sort(method = "radix") lève alors,
  # mesuré) ; des facteurs sans noms, un nom NA ou doublé sont refusés.
  if (!forme_exacte(f, c("double", "integer"), "names") || length(f) != 2L ||
      anyNA(names(f)) || anyDuplicated(names(f)) || !setequal(names(f), c("H", "F")) ||
      any(!is.finite(f)) || any(f < 0))
    refuser("simulation", "loi_invalide",
            formuler(paste("Loi « %s » : facteurs = c(H = , F = ), réels finis >= 0, de type double ou integer,",
                           "sans autre attribut que les noms."), cause),
            list(cause = cause, facteurs = f))
  morceaux <- lapply(sexes, function(s) {
    t <- if (s %in% tab$sexe) tab[tab$sexe == s, , drop = FALSE] else tab[tab$sexe == "ensemble", , drop = FALSE]
    if (!nrow(t))
      refuser("simulation", "loi_invalide",
              formuler("Loi « %s » : aucune ligne pour le sexe %s ni pour « ensemble ».", cause, s),
              list(cause = cause, sexe = s))
    t <- t[order(t$age), , drop = FALSE]
    q <- pmin(f[[s]] * t$qx, 1)
    data.frame(cause = cause, sexe = s, age = t$age, qx = q, mu = -log1p(-q),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, morceaux)
}

# --- dates ---------------------------------------------------------------------

# Jours depuis 1970-01-01 de la date civile (an, mois 1-12, jour), calendrier
# grégorien proleptique (algorithme « days from civil », arithmétique
# entière ; vectorisé ; vérifié contre as.Date dans les tests). Implémentation
# volontairement indépendante de celle du module `exposition` (M89, #22
# point 3) : le sens inverse, la décomposition d'un jour en composantes, passe
# par composantes_date() de `donnees`, simple enveloppe de as.POSIXlt ; l'accord
# des trois arithmétiques est vérifié sur [0001-01-01, 9999-12-31]
# (tests/unitaires/test_exposition.R) : exhaustif pour la décomposition et les
# jours civils, partiel (six âges, naissances voisines du 29/02, tirage et
# balayage) pour les anniversaires et les âges.
jours_civils <- function(an, mois, jour) {
  a <- an - (mois <= 2L)
  ere <- a %/% 400L
  aoe <- a - ere * 400L
  jda <- (153L * ((mois + 9L) %% 12L) + 2L) %/% 5L + jour - 1L
  joe <- aoe * 365L + aoe %/% 4L - aoe %/% 100L + jda
  ere * 146097 + joe - 719468
}

# Date (jours) de l'anniversaire d'âge x d'une tête de composantes de
# naissance cn (composantes_date(), mois 0-11) : convention T2, une
# naissance au 29/02 a son anniversaire le 01/03 des années non bissextiles.
jour_anniversaire <- function(cn, x) {
  an <- cn$an + x
  mois <- rep_len(cn$mois + 1L, length(an)); jour <- rep_len(cn$jour, length(an))
  bissextile <- (an %% 4L == 0L & an %% 100L != 0L) | an %% 400L == 0L
  fev29 <- mois == 2L & jour == 29L & !bissextile
  mois[fev29] <- 3L; jour[fev29] <- 1L
  jours_civils(an, mois, jour)
}

# composantes_date() calculée sur les valeurs distinctes seulement (la
# conversion par as.POSIXlt domine le temps de calcul à 10^6 têtes, mesuré).
composantes_distinctes <- function(j) {
  u <- unique(j)
  cu <- composantes_date(u)
  i <- match(j, u)
  list(an = cu$an[i], mois = cu$mois[i], jour = cu$jour[i])
}

# Âge au dernier anniversaire au jour t (vecteur ou scalaire), d'année
# civile an_t : nombre d'anniversaires jour_anniversaire() atteints à t.
# Même convention que age_revolu() (T2), sans nouvelle conversion de dates ;
# l'égalité des deux est vérifiée dans les tests, 29/02 compris.
age_anniversaire <- function(cn, an_t, t) {
  x <- an_t - cn$an
  x - (jour_anniversaire(cn, x) > t)
}

# Jours (numériques) rendus en Date, stockage double : colonnes de dates du
# jeu simulé et du jeu à défauts (#113 : une seule définition).
en_date <- function(j) structure(as.numeric(j), class = "Date")

# --- contrôle des entrées ------------------------------------------------------

# Dates uniques (bornes de la période, origine, date_entree) : date_unique()
# (outils ; #58, #113), même règle que les bornes de la période des données.

# Noms reçus d'une liste comparés aux noms admis, sans tri (setdiff,
# anyDuplicated, anyNA ; #100) : règle unique des noms des arguments du
# simulateur (portefeuille, defauts et chaque déclaration ; #113). Rend ""
# si les noms sont complets et admis ; sinon la chaîne d'écart, noms
# requis absents et noms non admis cités par liste_guillemets() (règle
# neutraliser_citation(), R/outils.R ; #111, #94 ; un nom marqué « bytes »
# est cité sous un repère). Une liste sans noms a des noms vides.
ecart_noms <- function(x, requis, admis) {
  n <- names(x)
  if (is.null(n)) n <- rep("", length(x))
  n <- as.character(n)
  if (anyNA(n) || anyDuplicated(n)) return("noms manquants ou en double")
  manquants <- setdiff(requis, n); inconnus <- setdiff(n, admis)
  if (!length(manquants) && !length(inconnus)) return("")
  formuler("champs attendus %s ; manquants : %s ; inconnus : %s", liste_guillemets(admis),
           if (length(manquants)) liste_guillemets(manquants) else "aucun",
           if (length(inconnus)) liste_guillemets(inconnus) else "aucun")
}

controler_portefeuille <- function(portefeuille, periode) {
  if (!liste_aux_noms(periode, c("debut", "fin")) ||
      !date_unique(periode[["debut"]]) || !date_unique(periode[["fin"]]))
    refuser("simulation", "periode_invalide",
            formuler(paste("La période d'observation doit être une liste (debut, fin) de deux dates (classe Date,",
                           "sans autre attribut).")),
            list(periode = periode))
  if (periode[["debut"]] > periode[["fin"]])
    refuser("simulation", "periode_invalide",
            formuler("Période vide : début %s postérieur à la fin %s.", citer(format(periode[["debut"]])),
                     citer(format(periode[["fin"]]))),
            list(periode = periode))
  # Forme exacte du portefeuille (#78, forme_exacte()) : liste nue, seul
  # attribut names (ni data.frame, ni classe, ni attribut quelconque). Noms
  # comparés plus bas sans tri (setdiff, anyDuplicated), indépendamment de la
  # locale ; la comparaison ne lève pas sur un nom marqué « bytes » (#111), et sa
  # citation passe par liste_guillemets(), règle neutraliser_citation()
  # (R/outils.R ; #111, #94). Noms attendus et manquants : CHAMPS_PORTEFEUILLE
  # (ASCII).
  if (!forme_exacte(portefeuille, "list", "names"))
    refuser("simulation", "portefeuille_invalide",
            formuler(paste("Portefeuille : liste nue attendue (ni data.frame, ni classe, aucun attribut",
                           "autre que les noms).")),
            list(classe = class(portefeuille)))
  # Champs lus par [[ ]] (jamais $, qui apparie les noms partiels) ; ici, le
  # mode : chaîne unique de forme exacte (chaine_unique(), #60).
  # Position trouvée par match(), qui ne lève pas sur un nom « bytes » ; [["mode"]]
  # lèverait ici (« translating strings with "bytes" encoding is not allowed »,
  # mesuré sous C.UTF-8 et sous C) si un nom « bytes » précédait ou remplaçait mode.
  i_mode <- match("mode", names(portefeuille))
  mode <- if (is.na(i_mode)) NULL else portefeuille[[i_mode]]
  if (!chaine_unique(mode) || !(mode %in% names(CHAMPS_PORTEFEUILLE)))
    refuser("simulation", "portefeuille_invalide",
            formuler("Portefeuille : liste avec un champ mode pris dans %s.",
                     liste_guillemets(names(CHAMPS_PORTEFEUILLE))),
            list(mode = mode))
  # Noms comparés par ecart_noms() (#113), règle partagée avec defauts.
  attendus <- CHAMPS_PORTEFEUILLE[[mode]]
  e <- ecart_noms(portefeuille, attendus, attendus)
  if (nzchar(e))
    refuser("simulation", "portefeuille_invalide",
            formuler("Portefeuille (mode %s) : %s.", citer(mode), e),
            list(mode = mode, attendus = attendus, noms = names(portefeuille)))
  p <- portefeuille
  # Champs numériques : forme exacte (#78), double ou integer sans aucun
  # attribut (ni names, ni dim, ni classe), longueur 1, fini.
  reel <- function(v) forme_exacte(v, c("double", "integer")) && length(v) == 1L && is.finite(v)
  entier <- function(v) reel(v) && v == trunc(v)
  invalide <- function(msg) refuser("simulation", "portefeuille_invalide", formuler("Portefeuille : %s", msg))
  if (!entier(p[["n"]]) || p[["n"]] < 1 || p[["n"]] > .Machine$integer.max) invalide(formuler("n entier >= 1, de type double ou integer, sans attribut."))
  if (!reel(p[["proportion_hommes"]]) || p[["proportion_hommes"]] < 0 || p[["proportion_hommes"]] > 1)
    invalide(formuler("proportion_hommes réel dans [0, 1], de type double ou integer, sans attribut."))
  fin <- as.numeric(periode[["fin"]])
  if (MODE_BASE[[mode]] == "uniforme") {
    if (!reel(p[["age_min"]]) || !reel(p[["age_max"]]) || p[["age_min"]] * 365.25 < 1 || p[["age_min"]] >= p[["age_max"]])
      invalide(formuler(paste("âges réels finis, de type double ou integer, sans attribut ;",
                              "age_min d'au moins un jour (1/365,25) et age_min < age_max.")))
    if (!date_unique(p[["origine"]]) || p[["origine"]] > periode[["debut"]])
      invalide(formuler(paste("origine : date (classe Date, sans autre attribut) antérieure ou égale",
                              "au début de la période.")))
    age_bas <- age_revolu_minimal(round(p[["age_min"]] * 365.25))
    # Borne haute (erratum d'actuary du 17/10/2026, point 4). Soit span(y)
    # le nombre de jours de la naissance à l'anniversaire y (T2) : pour
    # y <= 130 (âges des tables des tests), span(y) >= 365,25 y - 2,75
    # (minimum exact -2,75 mesuré sur les naissances de 1700 à 2150, atteint
    # pour y = 107, naissance au 1796-03-01). Une tête d'âge révolu y à B
    # vérifie span(y) <= B - n, et
    # B - n <= (B - origine) + round(age_max * 365,25)
    # <= (B - origine) + age_max * 365,25 + 0,5 ; donc
    # y <= age_max + (B - origine) / 365,25 + 3,25 / 365,25 < age_haut.
    # Domaine : aucune règle du code ne limite les âges de la table à 130.
    # Au-delà, le minimum mesuré sur les mêmes naissances jusqu'à y = 260 est
    # -3,75 (atteint pour y = 207, naissance au 2096-03-01 ; premier
    # y < -2,75 à y = 204) : la même preuve donne une marge de 4,25 / 365,25,
    # encore inférieure au « + 1 » d'age_haut.
    age_haut <- p[["age_max"]] + (fin + 1 - as.numeric(p[["origine"]])) / 365.25 + 1
  } else {
    if (!entier(p[["age"]]) || p[["age"]] < 1) invalide(formuler("âge exact entier >= 1, de type double ou integer, sans attribut."))
    if (!date_unique(p[["date_entree"]]) || p[["date_entree"]] > periode[["fin"]])
      invalide(formuler(paste("date_entree : date (classe Date, sans autre attribut) antérieure ou égale",
                              "à la fin de la période.")))
    ce <- composantes_date(as.numeric(p[["date_entree"]]))
    if (ce$mois == 1L && ce$jour == 29L)
      refuser("simulation", "entree_29_fevrier",
              formuler("Mode âge exact : une entrée le 29/02 est refusée (la naissance « x années avant » serait ambiguë)."),
              list(date_entree = p[["date_entree"]]))
    age_bas <- p[["age"]]
    # Borne haute : même preuve qu'en mode uniforme, avec B - n =
    # (B - date_entree) + span(age) et, pour age <= 130, span(age) <=
    # 365,25 age + 0,75 (maximum exact +0,75 mesuré sur les mêmes
    # naissances) : pour y <= 130, y <= age + (B - date_entree) / 365,25
    # + 3,5 / 365,25 < age_haut. Domaine : au-delà de 130, le maximum mesuré
    # jusqu'à 260 reste +0,75 et le minimum devient -3,75 (mode uniforme) :
    # marge de 4,5 / 365,25, encore inférieure au « + 1 ».
    age_haut <- p[["age"]] + (fin + 1 - as.numeric(p[["date_entree"]])) / 365.25 + 1
  }
  # Modes multi-contrats (#92) : les bornes d'âge restent valables, la
  # naissance étant fixée à la première entrée et les entrées suivantes
  # restant dans [origine, fin] (uniforme_multi) ou égales à date_entree.
  # loi_contrats : probabilités p_1, ..., p_m de K sur {1, ..., m} ; forme
  # exacte double ou integer sans aucun attribut ; p_m > 0 (m est le plus
  # grand nombre tirable) ; somme 1 à 1e-12 près. Loi effectivement tirée :
  # (p_1, ..., p_{m-1}, 1 - F_{m-1}), à moins de 1e-12 de la loi déclarée
  # (approximation numérique).
  if (mode != MODE_BASE[[mode]]) {
    lc <- p[["loi_contrats"]]
    if (!forme_exacte(lc, c("double", "integer")) || !length(lc) || any(!is.finite(lc)) || any(lc < 0) ||
        lc[length(lc)] <= 0 || abs(sum(lc) - 1) > 1e-12)
      invalide(formuler(paste("loi_contrats : probabilités p_1, ..., p_m (m >= 1) du nombre de contrats par tête,",
                              "réels finis >= 0, de type double ou integer, sans attribut, p_m > 0, de somme 1",
                              "à 1e-12 près.")))
    # Garde de débordement : le nombre de contrats tirés, au plus n x m,
    # est un entier R.
    if (as.numeric(p[["n"]]) * length(lc) > .Machine$integer.max)
      invalide(formuler("n x length(loi_contrats) (%.0f) dépasse .Machine$integer.max (%d).",
                        as.numeric(p[["n"]]) * length(lc), .Machine$integer.max))
  }
  list(mode = mode, age_bas = age_bas, age_haut = age_haut)
}

# Borne basse exacte de l'âge révolu d'une tête née d_min jours avant son
# entrée (d_min = round(age_min * 365,25), le plus petit écart tirable) :
# y_min = max{y : 365 y + ceiling(y / 4) <= d_min}, y années comptant au plus
# ceiling(y / 4) jours bissextiles. Indépendante de la graine (décision
# d'actuary, reprise C1 de #10) ; y_min(20) = 20, y_min(21) = 20.
age_revolu_minimal <- function(d_min) {
  y <- floor(d_min / 365)
  while (y > 0 && 365 * y + ceiling(y / 4) > d_min) y <- y - 1
  y
}

# Refus si la loi ne couvre pas les âges atteignables [age_bas, age_haut]
# (A.2 : aucune extrapolation) ou y atteint q_x = 1 (force non définie),
# pour les sexes de proportion non nulle.
controler_couverture <- function(loi, age_bas, age_haut, sexes) {
  for (cause in names(CAUSES_SIMULEES)) for (s in sexes) {
    l <- loi[loi$cause == cause & loi$sexe == s, , drop = FALSE]
    if (floor(age_bas) < min(l$age) || floor(age_haut) > max(l$age))
      refuser("simulation", "table_trop_courte",
              formuler(paste("Loi « %s », sexe %s : la table couvre les âges %d à %d ; âges atteignables",
                             "%d à %d (A.2 : aucune extrapolation)."),
                       cause, s, min(l$age), max(l$age), as.integer(floor(age_bas)),
                       as.integer(floor(age_haut))),
              list(cause = cause, sexe = s, ages_table = range(l$age),
                   ages_atteignables = c(age_bas, age_haut)))
    un <- l$age[l$age >= floor(age_bas) & l$age <= floor(age_haut) & l$qx >= 1]
    if (length(un))
      refuser("simulation", "qx_egal_a_un",
              formuler("Loi « %s », sexe %s : q_x = 1 à l'âge atteignable %d (force constante non définie).",
                       cause, s, un[1L]),
              list(cause = cause, sexe = s, ages = un))
  }
  invisible(TRUE)
}

# --- temps latents --------------------------------------------------------------

# Instant (jours, continu) et jour de l'événement latent d'une transition,
# pour les têtes `idx` d'un même sexe, par inversion du risque cumulé
# linéaire par morceaux sur l'âge : cible H(a0) + E, E ~ Exp(1), recherche
# par findInterval(). Rend list(instant, jour) ; Inf / NA au-delà de la
# table (n'arrive que hors de la fenêtre observée, controler_couverture()).
# Le jour est anniv(k) + floor(fraction * L_k), borné à [entrée,
# anniv(k+1) - 1] : la borne ne corrige que l'arrondi de la virgule
# flottante, P(événement dans l'année k) est conservée exactement.
temps_latent <- function(l, cn, x0, frac0, entree, e) {
  a_bas <- l$age[1L]
  cumul <- c(0, cumsum(l$mu))                  # risque cumulé aux âges entiers a_bas, a_bas + 1, ...
  j0 <- x0 - a_bas + 1L
  cible <- cumul[j0] + l$mu[j0] * frac0 + e
  i <- findInterval(cible, cumul)
  n <- length(e)
  instant <- rep(Inf, n); jour <- rep(NA_real_, n)
  ok <- which(i < length(cumul))
  if (length(ok)) {
    io <- i[ok]
    fraction <- (cible[ok] - cumul[io]) / l$mu[io]
    k <- a_bas + io - 1L
    cnk <- lapply(cn, `[`, ok)
    ak <- jour_anniversaire(cnk, k)
    lk <- jour_anniversaire(cnk, k + 1L) - ak
    instant[ok] <- ak + fraction * lk
    jour[ok] <- pmax(entree[ok], ak + pmin(floor(fraction * lk), lk - 1))
  }
  list(instant = instant, jour = jour)
}

# --- défauts paramétrés (#93) ---------------------------------------------------

# Contrôle de l'argument `defauts` (non NULL), avant tout tirage ; refus
# defauts_invalides. Forme : liste nue (seul attribut les noms) de champs
# exactement graine et declarations ; graine de la forme exigée par
# sous_graine() (#43), différente de la graine du jeu (réponse d'actuary à la
# question (f), 25/10/2026) ; declarations : liste sans aucun attribut (vide
# admise) de déclarations, chacune liste nue de champs type, cible (types de
# ligne seulement, requise ; interdite pour un type de tête), et exactement
# un de taux (réel de [0, 1]) ou nombre (entier >= 0). type : un type du
# catalogue, ou une combinaison admise (COMBINAISONS_DEFAUTS). Noms comparés
# sans tri (setdiff, anyDuplicated, anyNA ; #100). `graine` : la graine du
# jeu, telle que reçue ; NULL si l'argument manque (le refus graine_invalide
# vient alors de sous_graine()). `vides` : vecteur nommé de
# conditions_vides() (noms : conditions du catalogue structurellement vides
# pour le portefeuille ; valeurs : raison citée au refus) ; par défaut,
# aucune condition vide. Une déclaration dont un type porte l'une d'elles est
# refusée (ADR 0007, annotation du 05/11/2026, seconde, (c) et (d) ; #112,
# #135). `regles` : règles des défauts de la forme (entrée de
# DEFAUTS_FORMES ; par défaut, lignes individuelles) : catalogue,
# combinaisons, domaine de la cible et unités qui la requièrent, types
# exclusifs (seule déclaration admise ; #191) ; un type hors du catalogue de
# la forme est un type inconnu (#189).
# Rend list(graine,
# declarations) : graine entière ; pour chaque déclaration, list(type,
# unite, cible, taux, nombre), cible = causes du catalogue si absente (vide
# si la forme n'a pas de cible), taux ou nombre NULL.
controler_defauts <- function(defauts, graine, vides = stats::setNames(character(0), character(0)),
                              regles = DEFAUTS_FORMES$lignes) {
  invalide <- function(msg, details = list())
    refuser("simulation", "defauts_invalides", formuler("Défauts : %s", msg), details)
  # Chaînes de l'utilisateur citées par liste_guillemets() : règle
  # neutraliser_citation() (R/outils.R ; #111, #94). Noms : ecart_noms() ;
  # graine : forme_graine() (outils), règle de sous_graine() (#113).
  if (!forme_exacte(defauts, "list", "names"))
    invalide(formuler("liste nue attendue, list(graine, declarations) (ni data.frame, ni classe, aucun attribut autre que les noms)."),
             list(parametre = "defauts"))
  e <- ecart_noms(defauts, c("graine", "declarations"), c("graine", "declarations"))
  if (nzchar(e)) invalide(formuler("%s.", e), list(parametre = "defauts"))
  dg <- defauts[["graine"]]
  if (!forme_graine(dg))
    invalide(formuler("la graine des défauts doit être un nombre entier unique, explicite et fini, sans attribut."),
             list(parametre = "defauts$graine", graine_defauts = dg))
  if (!is.null(graine) && forme_graine(graine) && as.integer(dg) == as.integer(graine))
    refuser("simulation", "defauts_invalides",
            formuler(paste("La graine des défauts doit différer de la graine du jeu : avec la même graine, les défauts",
                           "seraient tirés sur le même flux pseudo-aléatoire que le jeu et corrélés à ses colonnes",
                           "(par exemple le sexe).")),
            list(parametre = "defauts$graine", graine = graine, graine_defauts = dg))
  decl <- defauts[["declarations"]]
  if (!forme_exacte(decl, "list"))
    invalide(formuler("declarations : liste sans attribut (ni noms, ni classe) de déclarations."),
             list(parametre = "defauts$declarations"))
  causes <- regles$cible
  cat_ <- regles$catalogue
  combinaisons <- regles$combinaisons
  declarations <- lapply(seq_along(decl), function(k) {
    d <- decl[[k]]
    inv <- function(msg) invalide(formuler("déclaration %d : %s", k, msg), list(parametre = "defauts$declarations",
                                                                              declaration = k))
    if (!forme_exacte(d, "list", "names"))
      inv(formuler("liste nue attendue, list(type, cible, taux) ou list(type, cible, nombre)."))
    e <- ecart_noms(d, "type", c("type", "cible", "taux", "nombre"))
    if (nzchar(e)) inv(formuler("%s.", e))
    type <- d[["type"]]
    if (!forme_exacte(type, "character") || !length(type) || anyNA(type) || anyDuplicated(type))
      inv(formuler("type : chaîne (ou vecteur de chaînes d'une combinaison admise) sans attribut, sans NA ni doublon."))
    if (length(type) == 1L) {
      if (!(type %in% cat_$defaut))
        inv(formuler("type %s inconnu ; types admis : %s.", liste_guillemets(type),
                     liste_guillemets(cat_$defaut)))
    } else if (!any(vapply(combinaisons, function(cmb)
      length(cmb) == length(type) && all(type %in% cmb), logical(1L)))) {
      inv(formuler("combinaison %s non admise ; combinaisons admises : %s.", liste_guillemets(type),
                   if (length(combinaisons)) paste(vapply(combinaisons, liste_guillemets, character(1L)), collapse = " ; ")
                   else "aucune"))
    }
    unite <- cat_$unite[match(type[1L], cat_$defaut)]
    a_cible <- "cible" %in% names(d)
    # Cible requise pour les unités de regles$unites_cible (lignes : type de
    # ligne), interdite pour les autres (type de tête ; cellule d'agrégats).
    cible_requise <- unite %in% regles$unites_cible
    if (!cible_requise && a_cible)
      inv(if (unite == "tete") formuler("type de tête « %s » : champ cible interdit.", citer(type))
          else formuler("type « %s » (unité %s) : champ cible interdit.", citer(type), citer(unite)))
    if (cible_requise && !a_cible)
      inv(if (unite == "ligne")
            formuler("type de ligne : champ cible requis (sous-ensemble de « deces », « rachat », « en_cours »).")
          else formuler("type d'unité « %s » : champ cible requis (sous-ensemble de %s).", citer(type),
                        liste_guillemets(causes)))
    cible <- if (a_cible) d[["cible"]] else causes
    if (a_cible && (!forme_exacte(cible, "character") || !length(cible) || anyNA(cible) || anyDuplicated(cible) ||
        !all(cible %in% causes)))
      inv(formuler("cible : vecteur de chaînes sans attribut, non vide, sans NA ni doublon, pris dans %s.",
                   liste_guillemets(causes)))
    # Types d'une combinaison rangés dans l'ordre du catalogue : l'attribut
    # ne dépend pas de l'ordre de saisie (parametre garde la saisie brute).
    type <- cat_$defaut[cat_$defaut %in% type]
    # Cible disjointe, par construction, des causes que le catalogue admet
    # pour le type (pour une combinaison, pour tous ses types) : refus
    # (complément de spécification d'actuary, 25/10/2026). Puis condition du
    # catalogue structurellement vide (`vides`) : refus. Un vide qui dépend
    # du tirage (cause absente du jeu propre, aucune tête à deux lignes en
    # mode multi, aucune entrée tirée avant la fin quand origine < fin) n'est
    # pas refusé ici (defauts_ineligibles ou eligibles = 0,
    # injecter_defauts()).
    i_cat <- match(type, cat_$defaut)
    if (cible_requise) {
      admises <- causes[vapply(causes, function(cs) all(cat_[[cs]][i_cat]), logical(1L))]
      if (!any(cible %in% admises))
        inv(formuler("cible %s disjointe des causes admises pour %s (%s) : aucune %s éligible par construction.",
                     liste_guillemets(cible), paste(citer(type), collapse = " + "),
                     if (length(admises)) liste_guillemets(admises) else "aucune",
                     if (unite == "ligne") "ligne" else formuler("unité")))
    }
    # Noms vides ("") jamais retenus : une chaîne vide ne fait pas refuser un
    # type sans condition.
    vide <- cat_$condition[i_cat]
    vide <- vide[nzchar(vide) & vide %in% names(vides)[nzchar(names(vides))]]
    if (length(vide))
      inv(formuler("type %s : aucune unité éligible par construction (%s), quel que soit le tirage.",
                   paste(citer(type), collapse = " + "), paste(unique(vides[vide]), collapse = " ; ")))
    a_taux <- "taux" %in% names(d); a_nombre <- "nombre" %in% names(d)
    if (a_taux == a_nombre) inv(formuler("exactement un des champs taux et nombre est requis."))
    v <- if (a_taux) d[["taux"]] else d[["nombre"]]
    reel <- forme_exacte(v, c("double", "integer")) && length(v) == 1L && is.finite(v)
    if (a_taux && !(reel && v >= 0 && v <= 1))
      inv(formuler("taux : réel de [0, 1], de type double ou integer, sans attribut."))
    if (a_nombre && !(reel && v == trunc(v) && v >= 0 && v <= .Machine$integer.max))
      inv(formuler("nombre : entier >= 0, de type double ou integer, sans attribut."))
    list(type = type, unite = unite, cible = cible,
         taux = if (a_taux) as.numeric(v), nombre = if (a_nombre) as.integer(v))
  })
  # Type exclusif de la forme (regles$exclusifs ; stocks successifs :
  # inventaire_perdu, #191) : seule déclaration admise.
  excl <- which(vapply(declarations, function(d) any(d$type %in% regles$exclusifs), logical(1L)))
  if (length(excl) && length(declarations) > 1L)
    invalide(formuler("déclaration %d : type « %s » exclusif : il doit être la seule déclaration (%d déclarations reçues).",
                      excl[1L], citer(declarations[[excl[1L]]]$type), length(declarations)),
             list(parametre = "defauts$declarations", declaration = excl[1L]))
  list(graine = as.integer(dg), declarations = declarations)
}

# Conditions du catalogue (colonne condition de CATALOGUE_DEFAUTS)
# structurellement vides pour un portefeuille déjà validé
# (controler_portefeuille(), mode `mode`) et la période : vide connu avant
# tout tirage, quelle que soit la graine (ADR 0007, annotation du
# 05/11/2026, seconde, (c) et (d)). Un seul contrat par tête (modes
# uniforme et age_exact ; modes multi dont loi_contrats n'a qu'une
# probabilité, K = 1, p_m > 0) : les conditions des types de tête, qui
# exigent une tête d'au moins deux lignes (injecter_defauts()), sont vides
# (#112). Aucune entrée avant la fin de la période (#135) : la condition
# entree_avant_fin (entrée < fin) est vide quand date_entree == fin (modes
# age_exact et age_exact_multi : toute entrée vaut date_entree) ou origine
# == fin (modes uniforme et uniforme_multi : entrées tirées dans [origine,
# fin]) ; controler_portefeuille() impose date_entree <= fin et origine <=
# debut <= fin, ce sont donc exactement les cas vides. Rend un vecteur
# nommé : noms, conditions vides (jamais "") ; valeurs, raison formée à
# partir des valeurs reçues (mode, longueur de loi_contrats ; date_entree ou
# origine, et fin), citée par controler_defauts() ; vecteur vide si aucune.
# `forme` : forme validée (controler_forme()), NULL par défaut (lignes
# individuelles) ; sinon, seules les conditions du catalogue de la forme
# (DEFAUTS_FORMES) sont examinées, d'après la forme seule (#189) : agrégats
# sans colonne annee (par_annee = FALSE), condition `annee` vide ; stocks
# successifs sur une grille de moins de trois dates, condition
# `grille_trois_dates` vide (#191).
conditions_vides <- function(mode, portefeuille, periode, forme = NULL) {
  vides <- stats::setNames(character(0), character(0))
  if (!is.null(forme)) {
    if (identical(forme$par_annee, FALSE))
      vides <- c(vides, annee = formuler("aucune colonne annee dans les agrégats : par_annee = FALSE"))
    if (!is.null(forme$inventaires) && length(forme$inventaires) < 3L)
      vides <- c(vides, grille_trois_dates = formuler("grille de %d dates d'inventaire, moins de trois",
                                                      length(forme$inventaires)))
    return(vides)
  }
  raison <- if (mode == MODE_BASE[[mode]])
    formuler("un seul contrat par tête : mode « %s »", citer(mode))
  else if (length(portefeuille[["loi_contrats"]]) == 1L)
    formuler("un seul contrat par tête : mode « %s », loi_contrats de longueur 1", citer(mode))
  if (!is.null(raison)) {
    cond <- unique(CATALOGUE_DEFAUTS$condition[CATALOGUE_DEFAUTS$unite == "tete"])
    cond <- cond[nzchar(cond)]
    vides <- c(vides, stats::setNames(rep(raison, length(cond)), cond))
  }
  fin <- periode[["fin"]]
  raison <- if (MODE_BASE[[mode]] == "uniforme") {
    if (as.numeric(portefeuille[["origine"]]) == as.numeric(fin))
      formuler("aucune entrée avant la fin de la période : origine « %s » égale à la fin « %s »",
               citer(format(portefeuille[["origine"]])), citer(format(fin)))
  } else if (as.numeric(portefeuille[["date_entree"]]) == as.numeric(fin)) {
    formuler("aucune entrée avant la fin de la période : date_entree « %s » égale à la fin « %s »",
             citer(format(portefeuille[["date_entree"]])), citer(format(fin)))
  }
  if (!is.null(raison)) vides <- c(vides, entree_avant_fin = raison)
  vides
}

# Sélection des unités en défaut, règle unique de toutes les formes (#93 ;
# #189, avis de forme d'architect, Q4 ; ORDRE_TIRAGES_DEFAUTS, points 0 à
# 3) : un seul sous_graine(graine) ; déclarations `decl` (controler_defauts())
# dans l'ordre reçu ; pour chacune, éligibles = eligibles(d, etat) (indices
# des unités éligibles, croissants, non encore altérées), puis retenues :
# taux, éligibles[runif(nombre d'éligibles) < taux] ; nombre,
# sort(éligibles[sample.int(nombre d'éligibles, nombre)]), refus
# defauts_ineligibles si nombre dépasse les éligibles ; puis etat <-
# marquer(d, retenues, etat) (disjonction). `etat` : état initial (unités déjà
# altérées), propre à la forme. Rend list(eligibles, retenues) : nombre
# d'éligibles et unités retenues, par déclaration.
tirer_defauts <- function(graine, decl, etat, eligibles, marquer) {
  eligibles_n <- integer(length(decl))
  retenues <- vector("list", length(decl))
  sous_graine(graine, for (k in seq_along(decl)) {
    d <- decl[[k]]
    el <- eligibles(d, etat)
    eligibles_n[k] <- length(el)
    if (!is.null(d$taux)) {
      r <- el[stats::runif(length(el)) < d$taux]
    } else {
      if (d$nombre > length(el))
        refuser("simulation", "defauts_ineligibles",
                formuler(paste("Défauts : déclaration %d (%s) : nombre = %d dépasse le nombre d'unités éligibles",
                               "(%d) non encore altérées (unité %s)."),
                         k, paste(citer(d$type), collapse = " + "), d$nombre, length(el), citer(d$unite)),
                list(declaration = k, type = d$type, nombre = d$nombre, eligibles = length(el)))
      r <- sort(el[sample.int(length(el), d$nombre)])
    }
    etat <- marquer(d, r, etat)
    retenues[[k]] <- r
  })
  list(eligibles = eligibles_n, retenues = retenues)
}

# Injecte les défauts dans le jeu propre `jeu` (data.frame de
# simuler_portefeuille(), attribut `simulation`) : sélection sous un seul
# sous_graine(ctl$graine) (ORDRE_TIRAGES_DEFAUTS), refus defauts_ineligibles
# si un `nombre` dépasse le nombre d'éligibles ; puis altérations
# déterministes (CATALOGUE_DEFAUTS). Les lignes non altérées et l'attribut
# `simulation` sont inchangés ; les copies (contrat_duplique) sont ajoutées
# en fin de jeu, lignes N + 1, ..., dans l'ordre des déclarations puis des
# lignes. Rend le jeu altéré, d'attribut `defauts` (parametre, graine,
# generateur, ordre_tirages, validite, lignes, decomptes). Dans `lignes`
# (une ligne par couple (ligne, motif)), `unite` décrit le défaut (ligne ou
# tête) et `role` l'altération de la ligne : alteree (ligne modifiée), copie
# (doublon ajouté), original (ligne dupliquée, non modifiée), tete (autre
# ligne d'une tête en défaut, non modifiée). decomptes : lignes_en_defaut
# compte les lignes qui portent un motif ou un refus attendu (originaux et
# lignes `tete` compris) ; lignes_alterees, les lignes modifiées ou ajoutées
# (rôles alteree et copie).
injecter_defauts <- function(jeu, ctl, defauts, periode) {
  N <- nrow(jeu)
  fin <- as.numeric(periode[["fin"]])
  tete <- jeu$id_tete; cause <- jeu$cause_sortie
  entree <- as.numeric(jeu$date_entree)
  nt <- max(tete)
  # Lignes de chaque tête : contiguës (jeu trié par tête ; toute tête a au
  # moins une ligne), de prem_ligne[h] à prem_ligne[h] + nb_lignes[h] - 1.
  if (is.unsorted(tete))
    stop("injecter_defauts() : jeu propre non trié par tête (erreur de programmation).", call. = FALSE)
  nb_lignes <- tabulate(tete, nbins = nt)
  prem_ligne <- cumsum(nb_lignes) - nb_lignes + 1L
  lignes_de <- function(h) sequence(nb_lignes[h], from = prem_ligne[h])
  a_deces <- tabulate(tete[cause == "deces"], nbins = nt) > 0L
  regles <- DEFAUTS_FORMES$lignes
  cat_ <- regles$catalogue
  decl <- ctl$declarations

  # Sélection, dans l'ordre reçu (tirer_defauts() ; disjonction : une ligne
  # altérée au plus une fois ; une tête retenue rend toutes ses lignes
  # inéligibles). État : lignes déjà altérées.
  selection <- tirer_defauts(ctl$graine, decl, logical(N),
    eligibles = function(d, alteree) {
      i_cat <- match(d$type, cat_$defaut)
      if (d$unite == "ligne") {
        ok <- !alteree & cause %in% d$cible
        for (i in i_cat) {
          admis <- c(deces = cat_$deces[i], rachat = cat_$rachat[i], en_cours = cat_$en_cours[i])
          ok <- ok & unname(admis[cause])
          if (cat_$condition[i] == "entree_avant_fin") ok <- ok & entree < fin
        }
      } else {
        ok <- nb_lignes >= 2L & tabulate(tete[alteree], nbins = nt) == 0L
        if (cat_$condition[i_cat] == "tete_multi_deces") ok <- ok & a_deces
      }
      which(ok)
    },
    marquer = function(d, r, alteree) {
      if (d$unite == "ligne") alteree[r] <- TRUE else alteree[lignes_de(r)] <- TRUE
      alteree
    })
  eligibles_n <- selection$eligibles
  retenues <- selection$retenues

  # Altérations déterministes, sur les valeurs du jeu propre.
  id_c <- jeu$id_contrat; id_t <- jeu$id_tete; sexe <- jeu$sexe
  nai <- as.numeric(jeu$date_naissance); ent <- entree; sor <- as.numeric(jeu$date_sortie); cs <- cause
  copies <- integer(0)
  morceaux <- vector("list", length(decl))
  ligne_df <- function(ligne, k, defaut, unite, role, premier = rep(NA_real_, length(ligne))) {
    i <- match(defaut, cat_$defaut)
    data.frame(ligne = as.integer(ligne), declaration = rep(as.integer(k), length(ligne)),
               defaut = rep(defaut, length(ligne)), unite = rep(unite, length(ligne)), role = role,
               motif_attendu = rep(cat_$motif_attendu[i], length(ligne)),
               refus_attendu = rep(cat_$refus_attendu[i], length(ligne)),
               premier_deces = structure(as.numeric(premier), class = "Date"), stringsAsFactors = FALSE)
  }
  for (k in seq_along(decl)) {
    d <- decl[[k]]; r <- retenues[[k]]
    if (d$unite == "ligne") {
      parts <- list()
      for (t in d$type) {
        switch(t,
               contrat_duplique = {
                 nouv <- N + length(copies) + seq_along(r)
                 copies <- c(copies, r)
               },
               id_contrat_manquant = id_c[r] <- NA_integer_,
               id_tete_manquant = id_t[r] <- NA_integer_,
               naissance_manquante = nai[r] <- NA_real_,
               entree_manquante = ent[r] <- NA_real_,
               sortie_manquante = sor[r] <- NA_real_,
               cause_manquante = cs[r] <- NA_character_,
               cause_hors_modalites = cs[r] <- toupper(cause[r]),
               sexe_manquant = sexe[r] <- NA_character_,
               sexe_hors_modalites = sexe[r] <- tolower(jeu$sexe[r]),
               naissance_apres_entree = nai[r] <- entree[r] + 1,
               sortie_avant_entree = sor[r] <- entree[r] - 1,
               en_cours_avant_fin = sor[r] <- entree[r],
               age_aberrant = nai[r] <- jours_civils(composantes_distinctes(entree[r])$an - 150L, 1L, 1L),
               date_hors_domaine = nai[r] <- rep(jours_civils(0L, 12L, 31L), length(r)),
               stop("injecter_defauts() : type de ligne sans altération (erreur de programmation).",
                    call. = FALSE))
        parts[[length(parts) + 1L]] <- if (t == "contrat_duplique")
          ligne_df(c(r, nouv), k, t, "ligne", rep(c("original", "copie"), each = length(r)))
        else ligne_df(r, k, t, "ligne", rep("alteree", length(r)))
      }
      m <- do.call(rbind, parts)
    } else {
      # Lignes des têtes retenues : contiguës (jeu trié par tête), de la
      # première prem_l à la dernière dern_l ; ligne altérée l : la dernière,
      # ou (tete_observee_apres_deces) la dernière autre que la première à
      # décès p, de date de décès s_d = premier_deces.
      t <- d$type
      li <- lignes_de(r)
      h <- rep.int(seq_along(r), nb_lignes[r])
      dern_l <- prem_ligne[r] + nb_lignes[r] - 1L
      prem_dc <- rep(NA_real_, length(r))
      if (t == "tete_observee_apres_deces") {
        dc <- cause[li] == "deces"
        p <- li[dc][match(seq_along(r), h[dc])]
        l <- ifelse(p == dern_l, dern_l - 1L, dern_l)
        prem_dc <- sor[p]
      } else {
        l <- dern_l
      }
      switch(t,
             tete_naissance_incoherente = nai[l] <- nai[l] - 1,
             tete_sexe_incoherent = sexe[l] <- ifelse(sexe[l] == "H", "F", "H"),
             tete_observee_apres_deces = { cs[l] <- "en_cours"; sor[l] <- NA_real_ },
             stop("injecter_defauts() : type de tête sans altération (erreur de programmation).", call. = FALSE))
      m <- ligne_df(li, k, t, "tete", ifelse(li == l[h], "alteree", "tete"), prem_dc[h])
    }
    morceaux[[k]] <- m[order(m$ligne, method = "radix"), , drop = FALSE]
  }
  lignes <- do.call(rbind, c(list(ligne_df(integer(0), 0L, character(0), character(0), character(0))), morceaux))
  row.names(lignes) <- NULL

  # Copies : valeurs du jeu propre (une ligne dupliquée n'est pas altérée autrement).
  resultat <- data.frame(id_contrat = c(id_c, jeu$id_contrat[copies]), id_tete = c(id_t, jeu$id_tete[copies]),
                         sexe = c(sexe, jeu$sexe[copies]),
                         date_naissance = en_date(c(nai, as.numeric(jeu$date_naissance)[copies])),
                         date_entree = en_date(c(ent, entree[copies])),
                         date_sortie = en_date(c(sor, as.numeric(jeu$date_sortie)[copies])),
                         cause_sortie = c(cs, cause[copies]), stringsAsFactors = FALSE)
  attr(resultat, "simulation") <- attr(jeu, "simulation")
  roles <- names(jeu)
  attr(resultat, "defauts") <- list(
    parametre = defauts, graine = ctl$graine, generateur = GENERATEUR,
    ordre_tirages = regles$ordre_tirages,
    validite = list(
      roles = stats::setNames(roles, roles), periode = periode, delai_declaration = 0,
      texte = regles$validite),
    lignes = lignes,
    decomptes = list(
      lignes_jeu_propre = N, copies = length(copies), lignes = N + length(copies),
      lignes_en_defaut = length(unique(lignes$ligne)),
      lignes_alterees = length(unique(lignes$ligne[lignes$role %in% c("alteree", "copie")])),
      declarations = data.frame(declaration = seq_along(decl),
                                defaut = vapply(decl, function(d) paste(d$type, collapse = " + "), character(1L)),
                                unite = vapply(decl, `[[`, character(1L), "unite"),
                                eligibles = eligibles_n, retenues = lengths(retenues),
                                stringsAsFactors = FALSE)))
  resultat
}

# Injecte les défauts d'une forme « agrégats » (#189 ; spécification
# d'actuary-approfondi de #189, § 1, § 2 et § 5) dans la table agrégée propre
# `tab` (projeter_agregats(), attribut `simulation`), après la projection :
# sélection des cellules par tirer_defauts() sous ctl$graine (une cellule
# altérée au plus une fois ; éligibilité lue dans le catalogue de la forme,
# DEFAUTS_FORMES), puis altérations déterministes sur les valeurs de la table
# propre. Les cellules non altérées et l'attribut `simulation` sont
# inchangés ; les copies (cellule_dupliquee) sont ajoutées en fin de table,
# lignes N + 1, ..., dans l'ordre des déclarations puis des cellules, aux
# valeurs de la table propre ; la colonne evenements passe en double si une
# cellule au moins est retenue par evenements_non_entiers. `forme` : forme
# validée (controler_forme()). Rend la table altérée, d'attribut `defauts` aux
# sept champs de injecter_defauts() (validite : rôles aux noms des colonnes,
# période, délai nul, texte, puis convention et par_annee de la forme ;
# lignes : unité cellule, rôles alteree, copie, original).
injecter_defauts_agregats <- function(tab, ctl, defauts, periode, forme) {
  N <- nrow(tab)
  regles <- DEFAUTS_FORMES[[forme$nom]]
  cat_ <- regles$catalogue
  decl <- ctl$declarations
  ec <- tab$exposition_centrale; ev <- tab$evenements

  # Sélection, dans l'ordre reçu ; état : cellules déjà altérées.
  selection <- tirer_defauts(ctl$graine, decl, logical(N),
    eligibles = function(d, alteree) {
      ok <- !alteree
      for (i in match(d$type, cat_$defaut)) {
        if (cat_$exposition_positive[i]) ok <- ok & ec > 0
        if (cat_$evenements_nuls[i]) ok <- ok & ev == 0
        if (cat_$evenements_positifs[i]) ok <- ok & ev >= 1
      }
      which(ok)
    },
    marquer = function(d, r, alteree) {
      alteree[r] <- TRUE
      alteree
    })
  retenues <- selection$retenues

  # Altérations déterministes : table propre puis copies (valeurs propres),
  # altérations portées sur les lignes 1 à N seulement.
  copies <- unlist(lapply(seq_along(decl), function(k)
    if (identical(decl[[k]]$type, "cellule_dupliquee")) retenues[[k]]), use.names = FALSE)
  copies <- as.integer(copies)
  res <- tab[c(seq_len(N), copies), , drop = FALSE]
  row.names(res) <- NULL
  expositions <- intersect(c("exposition_centrale", "exposition_initiale"), names(tab))
  n_annees <- composantes_date(periode$fin)$an - composantes_date(periode$debut)$an + 1L
  ligne_df <- function(ligne, k, defaut, role) {
    i <- match(defaut, cat_$defaut)
    data.frame(ligne = as.integer(ligne), declaration = rep(as.integer(k), length(ligne)),
               defaut = rep(defaut, length(ligne)), unite = rep("cellule", length(ligne)), role = role,
               motif_attendu = rep(cat_$motif_attendu[i], length(ligne)),
               refus_attendu = rep(cat_$refus_attendu[i], length(ligne)),
               premier_deces = structure(rep(NA_real_, length(ligne)), class = "Date"), stringsAsFactors = FALSE)
  }
  n_copies <- 0L
  morceaux <- vector("list", length(decl))
  for (k in seq_along(decl)) {
    t <- decl[[k]]$type; r <- retenues[[k]]
    switch(t,
           cellule_dupliquee = {
             nouv <- N + n_copies + seq_along(r)
             n_copies <- n_copies + length(r)
           },
           exposition_negative = for (e in expositions) res[[e]][r] <- -tab[[e]][r],
           evenements_non_entiers = if (length(r)) {
             res$evenements <- as.double(res$evenements)
             res$evenements[r] <- tab$evenements[r] + 0.5
           },
           evenements_sans_exposition = for (e in expositions) res[[e]][r] <- 0,
           annee_hors_periode = res$annee[r] <- tab$annee[r] - n_annees,
           stop("injecter_defauts_agregats() : type sans altération (erreur de programmation).", call. = FALSE))
    m <- if (t == "cellule_dupliquee") ligne_df(c(r, nouv), k, t, rep(c("original", "copie"), each = length(r)))
         else ligne_df(r, k, t, rep("alteree", length(r)))
    morceaux[[k]] <- m[order(m$ligne, method = "radix"), , drop = FALSE]
  }
  lignes <- do.call(rbind, c(list(ligne_df(integer(0), 0L, character(0), character(0))), morceaux))
  row.names(lignes) <- NULL

  attr(res, "simulation") <- attr(tab, "simulation")
  roles <- names(tab)
  attr(res, "defauts") <- list(
    parametre = defauts, graine = ctl$graine, generateur = GENERATEUR,
    ordre_tirages = regles$ordre_tirages,
    validite = list(roles = stats::setNames(roles, roles), periode = periode, delai_declaration = 0,
                    texte = regles$validite, convention = forme$convention, par_annee = forme$par_annee),
    lignes = lignes,
    decomptes = list(
      lignes_jeu_propre = N, copies = length(copies), lignes = N + length(copies),
      lignes_en_defaut = length(unique(lignes$ligne)),
      lignes_alterees = length(unique(lignes$ligne[lignes$role %in% c("alteree", "copie")])),
      declarations = data.frame(declaration = seq_along(decl),
                                defaut = vapply(decl, function(d) paste(d$type, collapse = " + "), character(1L)),
                                unite = vapply(decl, `[[`, character(1L), "unite"),
                                eligibles = selection$eligibles, retenues = lengths(retenues),
                                stringsAsFactors = FALSE)))
  res
}

# Injecte les défauts d'une forme « stocks successifs » (#191 ; spécification
# d'actuary-approfondi de #191, § 1 à § 3 ; avis de forme d'architect, partie
# #191) dans le jeu propre de la forme `tab` (projeter_stocks(), attribut
# `simulation`), après la projection. Unités : identifiants id_contrat
# distincts, en ordre croissant (tab trié par id_contrat) ; cause de l'unité
# dans le jeu propre : celle de sa ligne d'événement, « en_cours » sans ligne
# d'événement (une sortie finie postérieure à t_1 a toujours sa ligne
# d'événement, t_M étant la fin de la période). Sélection par tirer_defauts()
# sous ctl$graine : unités non encore altérées (une unité altérée au plus une
# fois, tous types confondus), dates de la grille pour inventaire_perdu
# (type exclusif, controler_defauts()) ; éligibilité lue dans le catalogue de
# la forme (DEFAUTS_FORMES). Altérations déterministes (colonne alteration du
# catalogue) ; f désigne le premier inventaire de l'unité. Table rendue :
# lignes de la projection dans leur ordre, moins les lignes retirées, puis
# copies (inventaire_duplique) en fin, dans l'ordre des déclarations puis des
# unités ; lignes non altérées et attribut `simulation` inchangés. `forme` :
# forme validée (controler_forme()). Attribut `defauts` : les sept champs de
# injecter_defauts(), puis `retirees` en fin ; validite : rôles aux noms des
# colonnes (identifiant id_contrat seul), période, délai nul, texte, puis la
# grille (inventaires) ; lignes (numéros de la table rendue, lignes présentes
# seulement) : rôles alteree (inventaire_hors_grille), original et copie
# (inventaire_duplique), unite (toute ligne restante d'une unité à motif
# d'unité, qui porte ce motif), propagation (autre ligne d'une unité porteuse
# d'un motif de ligne, amendement A1 : motif_attendu et refus_attendu NA) ;
# retirees (une ligne par ligne retirée : rang dans le jeu propre de la
# forme, declaration, defaut, unite, id_contrat, type_ligne, date_inventaire
# — la date perdue pour inventaire_perdu —, motif_attendu, refus_attendu).
# decomptes : ceux de injecter_defauts() (lignes = lignes de la table
# rendue ; lignes_en_defaut : lignes de `lignes` qui portent un motif ou un
# refus attendu, rôle propagation exclu), puis retirees (nombre de lignes
# retirées), lignes_propagation, unites_propagation et presents_sains
# (data.frame date_inventaire, presents_sains : unités présentes à chaque
# date de la grille, hors unités altérées et hors unités qu'une ligne retirée
# rend incohérentes : présence interrompue, dernier inventaire l < M sans
# événement, ou événement postérieur à t_(l+1) ; règle écrite ici d'après la
# spécification, jamais calculée par `donnees`).
injecter_defauts_stocks <- function(tab, ctl, defauts, periode, forme) {
  N <- nrow(tab)
  regles <- DEFAUTS_FORMES[[forme$nom]]
  cat_ <- regles$catalogue
  decl <- ctl$declarations
  g <- as.numeric(unclass(forme$inventaires)); M <- length(g)
  id <- tab$id_contrat
  if (is.unsorted(id))
    stop("injecter_defauts_stocks() : jeu propre de la forme non trié par id_contrat (erreur de programmation).",
         call. = FALSE)
  inv <- tab$type_ligne == "inventaire"
  di <- as.numeric(unclass(tab$date_inventaire))
  uid <- unique(id); U <- length(uid)
  u <- match(id, uid)
  n_inv <- tabulate(u[inv], nbins = U)
  # Ligne d'événement de chaque unité (au plus une), premier inventaire
  # (lignes d'une unité contiguës, inventaires dans l'ordre de la grille, puis
  # l'événement ; projeter_stocks()).
  evt_l <- rep(NA_integer_, U); evt_l[u[!inv]] <- which(!inv)
  cause_u <- rep("en_cours", U); cause_u[u[!inv]] <- tab$cause_sortie[!inv]
  prem <- which(inv)[match(seq_len(U), u[inv])]

  # Sélection, dans l'ordre reçu ; état : unités et dates déjà altérées.
  selection <- tirer_defauts(ctl$graine, decl, list(unites = logical(U), dates = logical(M)),
    eligibles = function(d, etat) {
      if (d$unite == "inventaire") return(which(!etat$dates))
      i <- match(d$type, cat_$defaut)
      admis <- c(deces = cat_$deces[i], rachat = cat_$rachat[i], en_cours = cat_$en_cours[i])
      ok <- !etat$unites & cause_u %in% d$cible & unname(admis[cause_u]) & n_inv >= cat_$inventaires_min[i]
      if (cat_$evenement[i]) ok <- ok & !is.na(evt_l)
      which(ok)
    },
    marquer = function(d, r, etat) {
      if (d$unite == "inventaire") etat$dates[r] <- TRUE else etat$unites[r] <- TRUE
      etat
    })
  retenues <- selection$retenues

  # Altérations déterministes, numérotation du jeu propre de la forme (copies :
  # N + 1, ..., renumérotées ensuite).
  ligne_df <- function(ligne, k, defaut, role) {
    i <- match(defaut, cat_$defaut)
    role <- rep_len(role, length(ligne))
    motif <- rep(cat_$motif_attendu[i], length(ligne)); refus <- rep(cat_$refus_attendu[i], length(ligne))
    motif[role == "propagation"] <- NA_character_; refus[role == "propagation"] <- NA_character_
    data.frame(ligne = as.integer(ligne), declaration = rep(as.integer(k), length(ligne)),
               defaut = rep(defaut, length(ligne)), unite = rep(cat_$unite[i], length(ligne)), role = role,
               motif_attendu = motif, refus_attendu = refus,
               premier_deces = structure(rep(NA_real_, length(ligne)), class = "Date"), stringsAsFactors = FALSE)
  }
  retire_df <- function(ligne, k, defaut) {
    i <- match(defaut, cat_$defaut)
    data.frame(ligne = as.integer(ligne), declaration = rep(as.integer(k), length(ligne)),
               defaut = rep(defaut, length(ligne)), unite = rep(cat_$unite[i], length(ligne)),
               id_contrat = id[ligne], type_ligne = tab$type_ligne[ligne], date_inventaire = en_date(di[ligne]),
               motif_attendu = rep(cat_$motif_attendu[i], length(ligne)),
               refus_attendu = rep(cat_$refus_attendu[i], length(ligne)), stringsAsFactors = FALSE)
  }
  dinv <- di
  copies <- integer(0)
  morceaux <- vector("list", length(decl)); retraits <- vector("list", length(decl))
  for (k in seq_along(decl)) {
    t <- decl[[k]]$type; r <- retenues[[k]]
    lu <- which(u %in% r)
    rm <- integer(0); m <- ligne_df(integer(0), k, t, character(0))
    switch(t,
           sortie_non_renseignee = {
             rm <- evt_l[r]
             m <- ligne_df(setdiff(lu, rm), k, t, "unite")
           },
           presence_interrompue = {
             rm <- prem[r] + 1L
             m <- ligne_df(setdiff(lu, rm), k, t, "unite")
           },
           inventaire_hors_grille = {
             l <- prem[r]
             d0 <- di[l] + 1
             while (any(sur <- d0 %in% g)) d0[sur] <- d0[sur] + 1
             dinv[l] <- d0
             m <- rbind(ligne_df(l, k, t, rep("alteree", length(l))),
                        ligne_df(setdiff(lu, l), k, t, "propagation"))
           },
           inventaire_duplique = {
             l <- prem[r]
             nouv <- N + length(copies) + seq_along(r)
             copies <- c(copies, l)
             m <- rbind(ligne_df(c(l, nouv), k, t, rep(c("original", "copie"), each = length(l))),
                        ligne_df(setdiff(lu, l), k, t, "propagation"))
           },
           inventaire_perdu = rm <- which(inv & di %in% g[r]),
           stop("injecter_defauts_stocks() : type sans altération (erreur de programmation).", call. = FALSE))
    morceaux[[k]] <- m[order(m$ligne, method = "radix"), , drop = FALSE]
    retraits[[k]] <- retire_df(sort(rm), k, t)
  }
  retirees <- do.call(rbind, c(list(retire_df(integer(0), 0L, character(0))), retraits))
  row.names(retirees) <- NULL
  gardees <- setdiff(seq_len(N), retirees$ligne)
  lignes <- do.call(rbind, c(list(ligne_df(integer(0), 0L, character(0), character(0))), morceaux))
  row.names(lignes) <- NULL
  lignes$ligne <- ifelse(lignes$ligne > N, length(gardees) + lignes$ligne - N, match(lignes$ligne, gardees))
  lignes$ligne <- as.integer(lignes$ligne)
  if (anyNA(lignes$ligne))
    stop("injecter_defauts_stocks() : ligne citée retirée (erreur de programmation).", call. = FALSE)

  # Table rendue : lignes gardées puis copies (valeurs du jeu propre).
  res <- tab
  res$date_inventaire <- en_date(dinv)
  res <- res[c(gardees, copies), , drop = FALSE]
  row.names(res) <- NULL

  # Présents sains par date (voir l'en-tête) : unités non altérées, sur les
  # lignes gardées, sans incohérence née d'un retrait.
  alterees <- logical(U)
  for (k in seq_along(decl)) if (decl[[k]]$unite != "inventaire") alterees[retenues[[k]]] <- TRUE
  gi <- seq_len(N) %in% gardees
  ki <- inv & gi
  kk <- match(di[ki], g)
  f <- rep(NA_integer_, U); l <- rep(NA_integer_, U)
  n_g <- tabulate(u[ki], nbins = U)
  ord <- order(u[ki], kk, method = "radix")
  uu <- u[ki][ord]; ko <- kk[ord]
  f[uu[!duplicated(uu)]] <- ko[!duplicated(uu)]
  l[uu[!duplicated(uu, fromLast = TRUE)]] <- ko[!duplicated(uu, fromLast = TRUE)]
  s_u <- rep(NA_real_, U)
  ev_g <- !inv & gi
  s_u[u[ev_g]] <- as.numeric(unclass(tab$date_sortie))[ev_g]
  a_inv <- n_g > 0L
  avant_fin <- a_inv & l < M
  suivant <- rep(NA_real_, U); suivant[which(avant_fin)] <- g[l[which(avant_fin)] + 1L]
  incoherente <- a_inv & ((l - f + 1L) != n_g | (avant_fin & (is.na(s_u) | s_u > suivant)))
  incoherente[is.na(incoherente)] <- FALSE
  sain <- !alterees & !incoherente
  presents <- tabulate(kk[sain[u[ki]]], nbins = M)

  attr(res, "simulation") <- attr(tab, "simulation")
  roles <- setdiff(names(tab), "id_tete")
  prop <- lignes$role == "propagation"
  attr(res, "defauts") <- list(
    parametre = defauts, graine = ctl$graine, generateur = GENERATEUR,
    ordre_tirages = regles$ordre_tirages,
    validite = list(roles = stats::setNames(roles, roles), periode = periode, delai_declaration = 0,
                    texte = regles$validite, inventaires = forme$inventaires),
    lignes = lignes,
    decomptes = list(
      lignes_jeu_propre = N, copies = length(copies), lignes = length(gardees) + length(copies),
      lignes_en_defaut = length(unique(lignes$ligne[!prop])),
      lignes_alterees = length(unique(lignes$ligne[lignes$role %in% c("alteree", "copie")])),
      declarations = data.frame(declaration = seq_along(decl),
                                defaut = vapply(decl, function(d) paste(d$type, collapse = " + "), character(1L)),
                                unite = vapply(decl, `[[`, character(1L), "unite"),
                                eligibles = selection$eligibles, retenues = lengths(retenues),
                                stringsAsFactors = FALSE),
      retirees = nrow(retirees),
      lignes_propagation = sum(prop),
      unites_propagation = length(unique(res$id_contrat[lignes$ligne[prop]])),
      presents_sains = data.frame(date_inventaire = forme$inventaires, presents_sains = presents)),
    retirees = retirees)
  res
}

# --- formes du simulateur (#169, commit (3c) ; #180, commit (4c)) ------------------

# Formes admises de l'argument `forme` (ADR 0007, annotation du 08/11/2026,
# (b) ; avis (0''), G11) : nom -> champs, tous requis, aucun défaut implicite.
# Conventions d'âge des agrégats mises en œuvre (spécification (0') de #169,
# § 11 ; spécification de #189, § 4) : `dernier_anniversaire` et
# `age_atteint_annee` ; toute autre valeur est une forme invalide. Forme
# « stocks successifs » (spécification de #180, § 11) : la grille des
# inventaires seule (le simulateur n'a pas de délai de déclaration).
CHAMPS_FORMES <- list(agregats = c("nom", "transition", "convention", "par_annee"),
                      stocks_successifs = c("nom", "inventaires"))
CONVENTIONS_FORMES <- list(agregats = c("dernier_anniversaire", "age_atteint_annee"))
# Champs ajoutés sous les modes multi-contrats (#189 ; avis de forme
# d'architect, partie #189) : requis sous uniforme_multi et age_exact_multi,
# refusés sous uniforme et age_exact (précédent loi_contrats). Une forme
# absente de la liste est refusée sous les modes multi-contrats (stocks
# successifs : fusion par tête non prise en charge, #190).
CHAMPS_FORMES_MULTI <- list(agregats = "unite_observation")

# Contrôle de l'argument `forme` (non NULL), avant tout tirage ; refus
# forme_invalide, module simulation. `mode` : mode validé du portefeuille ;
# `debut`, `fin` : bornes de la période validée (jours). Toute forme a son
# catalogue de défauts (DEFAUTS_FORMES ; stocks successifs, #191) : les
# défauts ne sont plus refusés ici (paramètre avec_defauts retiré, avis de
# forme d'architect, Q4). Refus nommés (spécification (0') de #169, § 11 ;
# spécification de #180, § 11 ; spécification de #189, § 3) : mode
# multi-contrats pour une forme absente de CHAMPS_FORMES_MULTI (fusion par
# tête des stocks non prise en charge, #190) ; agrégats sous mode multi-contrats : unite_observation
# requise, « tete » ou « contrat », « tete » refusée sous la transition de
# rachat (transition du contrat, M46) ; grille mal formée ou non alignée
# (stocks : t_1 = début - 1, t_M = fin, délai nul).
# Rend, pour les agrégats, list(nom, transition = list(depart, arrivee),
# cause, convention, par_annee [, unite_observation]), cause : cause de
# sortie du jeu propre comptée comme événement (nom de CAUSES_SIMULEES), les
# autres sorties étant des censures ; unite_observation sous les modes
# multi-contrats seulement ; pour les stocks, list(nom, inventaires), grille
# en Date à stockage double.
controler_forme <- function(forme, mode, debut, fin) {
  invalide <- function(msg, details = list())
    refuser("simulation", "forme_invalide", formuler("Forme du jeu simulé : %s", msg), details)
  if (!forme_exacte(forme, "list", "names"))
    invalide(formuler(paste("liste nue attendue, list(nom = \"agregats\", transition, convention, par_annee)",
                            "ou list(nom = \"stocks_successifs\", inventaires) (ni data.frame, ni classe, aucun",
                            "attribut autre que les noms).")),
             list(parametre = "forme"))
  nom <- forme[["nom"]]
  if (!chaine_unique(nom) || !(nom %in% names(CHAMPS_FORMES)))
    invalide(formuler("champ nom : une chaîne prise dans %s.", liste_guillemets(names(CHAMPS_FORMES))),
             list(parametre = "forme$nom"))
  multi <- mode != MODE_BASE[[mode]]
  champs <- c(CHAMPS_FORMES[[nom]], if (multi) CHAMPS_FORMES_MULTI[[nom]])
  e <- ecart_noms(forme, champs, champs)
  if (nzchar(e)) invalide(formuler("forme « %s » : %s.", citer(nom), e), list(parametre = "forme"))
  if (multi && is.null(CHAMPS_FORMES_MULTI[[nom]]))
    invalide(formuler(paste("forme « %s » sous le mode multi-contrats « %s » : l'unité inventoriée (tête ou",
                            "contrat) serait ambiguë ; la fusion par tête des contrats inventoriés n'est pas prise",
                            "en charge ; modes admis : %s."),
                      citer(nom), citer(mode), liste_guillemets(unique(MODE_BASE))),
             list(parametre = "forme", forme = nom, mode = mode))
  if (nom == "stocks_successifs") {
    g <- forme[["inventaires"]]
    j <- if (forme_exacte(g, c("double", "integer"), "class") && identical(class(g), "Date")) as.numeric(unclass(g))
         else NA_real_
    if (length(j) < 2L || anyNA(j) || !all(is.finite(j) & j == trunc(j)) || any(diff(j) <= 0))
      invalide(formuler(paste("inventaires : vecteur de dates (classe Date seule, stockage double ou integer, sans",
                              "autre attribut) d'au moins deux dates en jours entiers, sans valeur manquante,",
                              "strictement croissantes.")),
               list(parametre = "forme$inventaires"))
    M <- length(j)
    if (j[1L] != debut - 1 || j[M] != fin)
      invalide(formuler(paste("inventaires : grille non alignée sur la période (premier inventaire %s, dernier %s) ;",
                              "la première date doit être la veille du début (%s) et la dernière la fin de la période",
                              "(%s), le simulateur n'ayant pas de délai de déclaration."),
                        citer(format(en_date(j[1L]))), citer(format(en_date(j[M]))), citer(format(en_date(debut - 1))),
                        citer(format(en_date(fin)))),
               list(parametre = "forme$inventaires", attendu = en_date(c(debut - 1, fin))))
    return(list(nom = nom, inventaires = en_date(j)))
  }
  tr <- forme[["transition"]]
  code <- if (liste_aux_noms(tr, c("depart", "arrivee")) && chaine_unique(tr[["depart"]]) &&
              chaine_unique(tr[["arrivee"]])) paste(tr[["depart"]], "->", tr[["arrivee"]]) else NA_character_
  if (is.na(code) || !(code %in% CAUSES_SIMULEES))
    invalide(formuler("transition : liste nue (depart, arrivee) de deux chaînes, transition prise dans %s.",
                      liste_guillemets(unname(CAUSES_SIMULEES))),
             list(parametre = "forme$transition"))
  cv <- forme[["convention"]]
  if (!chaine_unique(cv) || !(cv %in% CONVENTIONS_FORMES[[nom]]))
    invalide(formuler("convention : une chaîne prise dans %s.", liste_guillemets(CONVENTIONS_FORMES[[nom]])),
             list(parametre = "forme$convention"))
  pa <- forme[["par_annee"]]
  if (!forme_exacte(pa, "logical") || length(pa) != 1L || is.na(pa))
    invalide(formuler("par_annee : TRUE ou FALSE (logique unique, sans attribut)."),
             list(parametre = "forme$par_annee"))
  ctl <- list(nom = nom, transition = list(depart = tr[["depart"]], arrivee = tr[["arrivee"]]),
              cause = names(CAUSES_SIMULEES)[match(code, CAUSES_SIMULEES)], convention = cv, par_annee = pa)
  # Unité de l'agrégat sous les modes multi-contrats (spécification de #189,
  # § 3) : contrat (lignes du jeu propre) ou tête (fenêtres réunies par tête,
  # décès seulement : le rachat est une transition du contrat, M46).
  if (multi) {
    uo <- forme[["unite_observation"]]
    if (!chaine_unique(uo) || !(uo %in% c("tete", "contrat")))
      invalide(formuler("unite_observation : une chaîne prise dans « tete », « contrat » (modes multi-contrats)."),
               list(parametre = "forme$unite_observation"))
    if (uo == "tete" && ctl$cause != "deces")
      invalide(formuler(paste("unite_observation « tete » sous la transition « %s » : le rachat est une transition",
                              "du contrat ; unité admise : « contrat »."), citer(code)),
               list(parametre = "forme$unite_observation", transition = code))
    ctl$unite_observation <- uo
  }
  ctl
}

# Projection du jeu propre `jeu` (lignes individuelles d'un mode à une tête
# par contrat) en agrégats par cellule (sexe, âge au dernier anniversaire
# [, année civile]) ; spécification (0') de #169, § 11. Par l'arithmétique de
# dates du simulateur (jours_civils(), jour_anniversaire(),
# age_anniversaire()), sans appel au module `exposition` (ADR 0003, point 9 ;
# ADR 0007, options écartées ; graphe_modules.R) ; aucun tirage. Fenêtre de
# chaque ligne [a, b), a = max(entrée, D), b = min(sortie, B), B = fin + 1
# (sortie = Inf en cours) ; une année d'âge [anniv(x), anniv(x + 1)) de L_x
# jours vaut 1 (A.0, T3). Événement : sortie de cause `cause` avec D <= sortie
# < B, compté dans la cellule du jour de sortie ; les autres sorties sont des
# censures. exposition_centrale : somme, sur les morceaux de fenêtre de la
# cellule, de (fin - début) / L_x ; exposition_initiale (sans par_annee) :
# exposition_centrale plus, pour chaque événement, (anniv(x + 1) - sortie) /
# L_x, complément jusqu'à la fin de la cellule, aucun pour une censure. Sous
# par_annee, les morceaux sont coupés au 1er janvier, l'événement va à l'année
# civile de sa sortie. Lignes : par sexe (ordre des octets), plage contiguë
# des âges des cellules qui portent un morceau de fenêtre, un événement ou une
# autre sortie dans [D, B) ; sous par_annee, toutes les années civiles de
# [D, B - 1] pour chaque âge ; zéros explicites. Rend le data.frame (sexe,
# age, [annee], exposition_centrale, [exposition_initiale], evenements),
# colonnes aux noms des rôles, age, annee et evenements en integer.
# `convention` (#189 ; spécification de #189, § 4) : `dernier_anniversaire`
# (défaut, ci-dessus) ou `age_atteint_annee` : cellule (année civile t, âge
# x = t - an(naissance)) sur [01/01/t, 01/01/(t + 1)), morceaux coupés au
# 1er janvier, un jour valant 1 / longueur de l'année civile (365 ou 366) ;
# événement et censure de date s à l'âge an(s) - an(naissance) ;
# exposition_initiale produite avec et sans par_annee : exposition_centrale
# plus, pour chaque événement, (01/01/(an(s) + 1) - s) / longueur de l'année
# an(s) ; sans par_annee, cellules (sexe, x), sommes sur t. `unite` (#189,
# § 3) : NULL ou « contrat » (lignes du jeu propre), ou « tete » (fenêtres
# réunies par tête, fusionner_tetes(), avant la projection).
projeter_agregats <- function(jeu, cause, par_annee, debut, borne, convention = "dernier_anniversaire",
                              unite = NULL) {
  if (identical(unite, "tete")) jeu <- fusionner_tetes(jeu)
  atteint <- convention == "age_atteint_annee"
  an_de <- function(j) composantes_distinctes(j)$an
  nai <- as.numeric(jeu$date_naissance)
  entree <- as.numeric(jeu$date_entree)
  sortie <- as.numeric(jeu$date_sortie); sortie[is.na(sortie)] <- Inf
  cn <- composantes_distinctes(nai)
  sous <- function(i) lapply(cn, `[`, i)
  a <- pmax(entree, debut); b <- pmin(sortie, borne)
  ex <- which(a < b)
  if (atteint) {
    # Morceaux de fenêtre, un par année civile parcourue ; âge atteint dans
    # l'année.
    y0 <- an_de(a[ex]); k <- an_de(b[ex] - 1) - y0 + 1L
    rang <- rep.int(seq_along(ex), k)
    obs <- ex[rang]
    y <- y0[rang] + (seq_along(rang) - rep.int(cumsum(k) - k, k)) - 1L
    x <- y - cn$an[obs]
    bas <- jours_civils(y, 1L, 1L); haut <- jours_civils(y + 1L, 1L, 1L)
    m_deb <- pmax(a[obs], bas); m_fin <- pmin(b[obs], haut); longueur <- haut - bas
  } else {
    # Morceaux de fenêtre, un par âge parcouru.
    x_a <- age_anniversaire(sous(ex), an_de(a[ex]), a[ex])
    x_b <- age_anniversaire(sous(ex), an_de(b[ex] - 1), b[ex] - 1)
    k <- x_b - x_a + 1L
    rang <- rep.int(seq_along(ex), k)
    obs <- ex[rang]
    x <- x_a[rang] + (seq_along(rang) - rep.int(cumsum(k) - k, k)) - 1L
    cm <- sous(obs)
    bas <- jour_anniversaire(cm, x); haut <- jour_anniversaire(cm, x + 1L)
    m_deb <- pmax(a[obs], bas); m_fin <- pmin(b[obs], haut); longueur <- haut - bas
  }
  if (par_annee && !atteint) {
    y0 <- an_de(m_deb); k2 <- an_de(m_fin - 1) - y0 + 1L
    r2 <- rep.int(seq_along(m_deb), k2)
    y <- y0[r2] + (seq_along(r2) - rep.int(cumsum(k2) - k2, k2)) - 1L
    obs <- obs[r2]; x <- x[r2]; longueur <- longueur[r2]
    m_deb <- pmax(m_deb[r2], jours_civils(y, 1L, 1L)); m_fin <- pmin(m_fin[r2], jours_civils(y + 1L, 1L, 1L))
  }
  # Sorties dans [D, B) : événements (cause comptée) et censures.
  dans <- is.finite(sortie) & sortie >= debut & sortie < borne
  ev <- which(dans & jeu$cause_sortie == cause)
  au <- which(dans & jeu$cause_sortie != cause)
  if (atteint) {
    an_e <- an_de(sortie[ev])
    x_e <- an_e - cn$an[ev]
    bas_e <- jours_civils(an_e, 1L, 1L); haut_e <- jours_civils(an_e + 1L, 1L, 1L)
    x_c <- an_de(sortie[au]) - cn$an[au]
  } else {
    ce <- sous(ev)
    x_e <- age_anniversaire(ce, an_de(sortie[ev]), sortie[ev])
    bas_e <- jour_anniversaire(ce, x_e); haut_e <- jour_anniversaire(ce, x_e + 1L)
    x_c <- age_anniversaire(sous(au), an_de(sortie[au]), sortie[au])
  }

  # Cellules : par sexe, plage contiguë des âges présents ; années de [D, B - 1].
  sexes <- sort(unique(jeu$sexe), method = "radix")
  s_m <- jeu$sexe[obs]; s_e <- jeu$sexe[ev]; s_c <- jeu$sexe[au]
  annees <- if (par_annee) seq.int(an_de(debut), an_de(borne - 1)) else NA_integer_
  plages <- lapply(sexes, function(s) {
    xs <- c(x[s_m == s], x_e[s_e == s], x_c[s_c == s])
    if (length(xs)) seq.int(min(xs), max(xs)) else integer(0)
  })
  n_age <- lengths(plages); n_an <- length(annees)
  cellules <- data.frame(sexe = rep(sexes, n_age * n_an),
                         age = as.integer(rep(unlist(plages), each = n_an)), stringsAsFactors = FALSE)
  if (par_annee) cellules$annee <- as.integer(rep(annees, length.out = nrow(cellules)))
  cle <- function(s, xx, yy) if (par_annee) paste(s, xx, yy) else paste(s, xx)
  cles <- if (par_annee) cle(cellules$sexe, cellules$age, cellules$annee) else cle(cellules$sexe, cellules$age)
  somme <- function(i, v) {
    r <- numeric(nrow(cellules))
    if (length(i)) { s <- rowsum(as.numeric(v), i, reorder = TRUE); r[sort(unique(i))] <- s[, 1L] }
    r
  }
  i_m <- match(if (par_annee) cle(s_m, x, y) else cle(s_m, x), cles)
  i_e <- match(if (par_annee) cle(s_e, x_e, an_de(sortie[ev])) else cle(s_e, x_e), cles)
  if (anyNA(i_m) || anyNA(i_e))
    stop("projeter_agregats() : morceau ou événement hors des cellules (erreur de programmation).", call. = FALSE)
  cellules$exposition_centrale <- somme(i_m, (m_fin - m_deb) / longueur)
  if (!par_annee || atteint)
    cellules$exposition_initiale <- cellules$exposition_centrale + somme(i_e, (haut_e - sortie[ev]) / (haut_e - bas_e))
  cellules$evenements <- tabulate(i_e, nbins = nrow(cellules))
  cellules
}

# Réunion par tête des fenêtres d'un jeu propre multi-contrats `jeu` (trié
# par tête, puis entrée ; simuler_portefeuille()) pour l'agrégat en têtes
# (#189 ; spécification de #189, § 3 ; règle d'E2.4, écrite ici sans appel au
# module `donnees`, non-circularité) : fenêtres [entrée, sortie) d'une même
# tête (sortie = +Inf en cours) réunies si entrée suivante <= fin courante
# (maximum des sorties précédentes de la tête) ; une composante va de sa
# première entrée à sa fin courante ; cause « deces » si l'une de ses lignes
# sort par décès (les contrats en cours au décès de la tête sortent tous ce
# jour-là, aucun contrat n'est écrit après), « en_cours » si sa fin est
# infinie, « rachat » sinon (censure sous la transition de décès). Rend un
# data.frame (id_tete, sexe, date_naissance, date_entree, date_sortie,
# cause_sortie), une ligne par composante, dans l'ordre des têtes puis des
# composantes.
fusionner_tetes <- function(jeu) {
  tete <- jeu$id_tete
  if (is.unsorted(tete))
    stop("fusionner_tetes() : jeu propre non trié par tête (erreur de programmation).", call. = FALSE)
  e <- as.numeric(jeu$date_entree)
  s <- as.numeric(jeu$date_sortie); s[is.na(s)] <- Inf
  n <- length(tete)
  prem <- !duplicated(tete)
  if (n && any(diff(e)[!prem[-1L]] < 0))
    stop("fusionner_tetes() : entrées non triées dans une tête (erreur de programmation).", call. = FALSE)
  # Maximum courant des sorties par tête : boucle sur les rangs dans la tête
  # (au plus max(K) passages), vectorisée sur les têtes.
  rang <- sequence(tabulate(match(tete, unique(tete))))
  fc <- s
  for (j in seq_len(if (n) max(rang) else 0L)[-1L]) {
    i <- which(rang == j)
    fc[i] <- pmax(s[i], fc[i - 1L])
  }
  prec <- c(-Inf, fc[-n])
  nouvelle <- prem | e > prec
  comp <- cumsum(nouvelle)
  dern <- !duplicated(comp, fromLast = TRUE)
  fin <- fc[dern]
  deces <- tabulate(comp[jeu$cause_sortie == "deces"], nbins = sum(nouvelle)) > 0L
  cause <- ifelse(deces, "deces", ifelse(is.finite(fin), "rachat", "en_cours"))
  i <- which(nouvelle)
  data.frame(id_tete = tete[i], sexe = jeu$sexe[i], date_naissance = jeu$date_naissance[i],
             date_entree = en_date(e[i]), date_sortie = en_date(ifelse(is.finite(fin), fin, NA_real_)),
             cause_sortie = cause, stringsAsFactors = FALSE)
}

# Projection du jeu propre `jeu` (lignes individuelles d'un mode à une tête
# par contrat) en stocks successifs sur la grille `grille` (Date, validée par
# controler_forme()) ; spécification de #180, § 11. Comparaisons de dates du
# simulateur seules, sans appel au module `exposition` (ADR 0003, point 9 ;
# graphe_modules.R) ; aucun tirage. Pour chaque ligne d'entrée e et de
# sortie s (s = +Inf en cours) :
#   - une ligne d'inventaire par date t_i de la grille avec e <= t_i < s (une
#     date d'inventaire désigne l'état en fin de journée : unité entrée le
#     jour t_i présente, unité sortie le jour t_i absente ; § 2.1) ;
#   - une ligne d'événement si la sortie est finie et t_1 < s <= t_M ;
#   - aucune ligne sinon (sortie au plus tard le jour t_1).
# Colonnes aux noms des rôles : id_contrat, id_tete, type_ligne
# (« inventaire » ou « evenement »), date_inventaire, sexe, date_naissance,
# date_sortie, cause_sortie ; dates manquantes en NA de classe Date, causes
# manquantes en NA de type caractère. Ordre : par id_contrat, puis par date
# de la ligne (inventaires avant l'événement, toujours postérieur). Rend
# list(lignes, decomptes) ; decomptes, calculés sur le jeu propre : lignes
# (inventaire, evenement), p0 (présentes à t_1), entrees (vues pour la
# première fois à t_f, f >= 2), entres_sortis (sans ligne d'inventaire,
# sortie dans ]t_1, t_M], par cause de CAUSES_SIMULEES), presents_dernier
# (présentes à t_M) ; entiers.
projeter_stocks <- function(jeu, grille) {
  g <- as.numeric(unclass(grille)); M <- length(g)
  e <- as.numeric(jeu$date_entree)
  s <- as.numeric(jeu$date_sortie); s[is.na(s)] <- Inf
  # Inventaires de présence : de kp (premier t_i >= e) à kd (dernier t_i < s).
  kp <- findInterval(e, g, left.open = TRUE) + 1L
  kd <- findInterval(s, g, left.open = TRUE)
  nb <- pmax(kd - kp + 1L, 0L)
  rang <- rep.int(seq_along(nb), nb)
  k <- kp[rang] + sequence(nb) - 1L
  ev <- which(is.finite(s) & s > g[1L] & s <= g[M])
  u <- c(rang, ev)
  o <- order(jeu$id_contrat[u], c(g[k], s[ev]), method = "radix")
  u <- u[o]
  ni <- length(rang); ne <- length(ev)
  lignes <- data.frame(id_contrat = jeu$id_contrat[u], id_tete = jeu$id_tete[u],
                       type_ligne = rep(c("inventaire", "evenement"), c(ni, ne))[o],
                       date_inventaire = en_date(c(g[k], rep(NA_real_, ne))[o]),
                       sexe = jeu$sexe[u], date_naissance = jeu$date_naissance[u],
                       date_sortie = en_date(c(rep(NA_real_, ni), s[ev])[o]),
                       cause_sortie = c(rep(NA_character_, ni), jeu$cause_sortie[ev])[o],
                       stringsAsFactors = FALSE)
  p0 <- e <= g[1L] & g[1L] < s
  vu <- nb > 0L
  es <- !vu & is.finite(s) & s > g[1L] & s <= g[M]
  causes <- names(CAUSES_SIMULEES)
  decomptes <- list(lignes = c(inventaire = ni, evenement = ne),
                    p0 = sum(p0), entrees = sum(vu & !p0),
                    entres_sortis = vapply(causes, function(x) sum(es & jeu$cause_sortie == x), integer(1L)),
                    presents_dernier = sum(e <= g[M] & g[M] < s))
  list(lignes = lignes, decomptes = decomptes)
}

# --- point d'entrée ---------------------------------------------------------------

# Simule un portefeuille de n têtes : une tête par contrat (modes `uniforme`
# et `age_exact`), ou K contrats tirés par tête (modes `uniforme_multi` et
# `age_exact_multi`, #92).
#   loi          : list(deces = list(table, facteurs), rachat = list(table,
#                  facteurs)) ; table : `table_reference` (H et F, ou
#                  `ensemble`) ; facteurs : c(H = , F = ), double ou integer, seul
#                  attribut les noms (#58), appliqués à q_x, plafond 1 ;
#   portefeuille : liste nue (seul attribut les noms, #78), champs numériques
#                  double ou integer sans attribut, dates de classe Date seule ;
#                  list(mode = "uniforme", n, proportion_hommes, age_min,
#                  age_max, origine) — entrée uniforme discrète sur les jours
#                  de [origine, fin], âge à l'entrée uniforme continu sur
#                  [age_min, age_max), naissance = entrée - round(âge *
#                  365,25) jours ; ou list(mode = "age_exact", n,
#                  proportion_hommes, age, date_entree) — entrée à l'âge
#                  exact `age` à la date `date_entree` (constat de
#                  simulation) ; ou les modes `uniforme_multi` et
#                  `age_exact_multi`, aux mêmes champs plus `loi_contrats`
#                  (probabilités p_1, ..., p_m du nombre K de contrats par
#                  tête) : en `uniforme_multi`, entrées iid par contrat sur
#                  les jours de [origine, fin] et âge à la première entrée
#                  uniforme sur [age_min, age_max) ; en `age_exact_multi`,
#                  tous les contrats entrent à `date_entree` ;
#   periode      : list(debut, fin), dates (classe Date seule, #58) ;
#                  censure à B = fin + 1 ;
#   graine       : entier explicite (sous_graine, générateur GENERATEUR) ;
#   defauts      : NULL (défaut de l'argument) : aucun défaut injecté, aucun
#                  attribut `defauts` (ce n'est pas une graine par défaut ;
#                  ADR 0007, annotation du 25/10/2026, (b)) ; ou list(graine,
#                  declarations) (controler_defauts()) : défauts paramétrés
#                  injectés après le tirage complet du jeu propre
#                  (injecter_defauts()) ;
#   forme        : NULL (défaut de l'argument) : lignes individuelles (ce
#                  n'est pas une valeur choisie à la place de l'appelant ;
#                  ADR 0007, annotation du 08/11/2026, (b)) ; ou list(nom =
#                  "agregats", transition = list(depart, arrivee), convention
#                  = "dernier_anniversaire", par_annee) (controler_forme(),
#                  avant tout tirage) : projection du jeu propre en agrégats
#                  (projeter_agregats()), même loi, même portefeuille, même
#                  graine, aucun tirage supplémentaire (#169) ; convention
#                  "age_atteint_annee" admise ; sous les modes
#                  multi-contrats, champ unite_observation ("tete" ou
#                  "contrat") en plus (#189) ; défauts admis, propres aux
#                  agrégats (DEFAUTS_FORMES), injectés après la projection
#                  (injecter_defauts_agregats(), #189) ; ou list(nom =
#                  "stocks_successifs", inventaires) (grille alignée : t_1 =
#                  début - 1, t_M = fin) : projection du jeu propre en lignes
#                  d'inventaire et d'événement (projeter_stocks()), de même
#                  (#180) ; défauts admis, propres aux stocks
#                  (DEFAUTS_FORMES), injectés après la projection
#                  (injecter_defauts_stocks(), #191).
# Rend un data.frame (id_contrat, id_tete, sexe, date_naissance, date_entree,
# date_sortie, cause_sortie) avec l'attribut `simulation` (et `defauts` si des
# défauts sont déclarés). Toutes les lignes sont gardées, y compris celles
# sorties avant le début de la période ; seuls manquent les contrats non
# souscrits (modes multi-contrats), décomptés dans l'attribut. Avec `forme`,
# le data.frame de la forme (projeter_agregats(), projeter_stocks()),
# d'attribut `simulation` celui du jeu propre plus un champ `forme` ajouté en
# fin : forme validée (agrégats ; unite_observation sous les modes
# multi-contrats seulement) ; nom, grille (Date à stockage double) et
# décomptes de la projection (stocks) ; attribut `defauts` de la forme si
# des défauts sont déclarés (agrégats, #189 ; stocks, #191).
simuler_portefeuille <- function(loi, portefeuille, periode, graine, defauts = NULL, forme = NULL) {
  if (!liste_aux_noms(loi, names(CAUSES_SIMULEES)))
    refuser("simulation", "loi_invalide",
            formuler("La loi vraie doit être une liste nommée %s.", liste_guillemets(names(CAUSES_SIMULEES))),
            list(noms = names(loi)))
  ctl <- controler_portefeuille(portefeuille, periode)
  p <- portefeuille
  n <- as.integer(p$n)
  # Sexes tirés (proportion non nulle) : seuls construits et contrôlés.
  sexes <- MODALITES_SEXE[c(p$proportion_hommes > 0, p$proportion_hommes < 1)]
  effective <- do.call(rbind, lapply(names(CAUSES_SIMULEES), function(cause)
    loi_effective(loi[[cause]], cause, sexes)))
  controler_couverture(effective, ctl$age_bas, ctl$age_haut, sexes)
  debut <- as.numeric(periode$debut); fin <- as.numeric(periode$fin); borne <- fin + 1
  # Forme : contrôlée avant tout tirage et avant les défauts (refus
  # forme_invalide).
  if (!is.null(forme)) ctl_forme <- controler_forme(forme, ctl$mode, debut, fin)
  # Défauts : contrôlés avant tout tirage (refus defauts_invalides).
  # Avec une forme (agrégats, #189 ; stocks successifs, #191) : catalogue des
  # défauts de la forme (DEFAUTS_FORMES, lu par clé) et conditions vides de la
  # forme validée.
  if (!is.null(defauts)) {
    graine_jeu <- if (missing(graine)) NULL else graine
    ctl_defauts <- if (is.null(forme))
      controler_defauts(defauts, graine_jeu, conditions_vides(ctl$mode, p, periode))
    else
      controler_defauts(defauts, graine_jeu, conditions_vides(ctl$mode, p, periode, ctl_forme),
                        DEFAUTS_FORMES[[ctl_forme$nom]])
  }

  # Tirages, dans l'ordre fixé (ORDRE_TIRAGES ; modes multi-contrats :
  # ORDRE_TIRAGES_MULTI). Modes existants : K = 1 par tête sans tirage (loi
  # dégénérée), N = n : tirages et ordre inchangés (ADR 0007, annotation du
  # 25/10/2026, (a)).
  multi <- ctl$mode != MODE_BASE[[ctl$mode]]
  uniforme <- MODE_BASE[[ctl$mode]] == "uniforme"
  lc <- if (multi) p[["loi_contrats"]] else 1
  m <- length(lc)
  tirages <- sous_graine(graine, {
    u_sexe <- stats::runif(n)
    # K par inversion explicite ; aucun tirage si la loi n'a qu'une
    # probabilité non nulle (p_m > 0 : K = m).
    K <- if (sum(lc > 0) == 1L) rep.int(m, n) else 1L + findInterval(stats::runif(n), cumsum(lc)[-m])
    nk <- sum(K)
    if (uniforme) {
      j_entree <- sample.int(as.integer(fin - as.numeric(p$origine) + 1), nk, replace = TRUE)
      u_age <- stats::runif(n)
    } else {
      j_entree <- u_age <- NULL
    }
    list(u_sexe = u_sexe, K = K, j_entree = j_entree, u_age = u_age,
         e_deces = stats::rexp(n), e_rachat = stats::rexp(nk))
  })

  # Contrats tirés, dans l'ordre des tirages (têtes, puis contrats) : tête de
  # chaque contrat, entrée ; première entrée e_(1) de chaque tête (toute tête
  # a au moins un contrat) ; lignes ordonnées par (tête, entrée, rang de
  # tirage), tri radix stable.
  K <- tirages$K; nk <- length(tirages$e_rachat)
  tete <- rep.int(seq_len(n), K)
  sexe <- ifelse(tirages$u_sexe < p$proportion_hommes, "H", "F")
  entree <- if (uniforme) as.numeric(p$origine) + tirages$j_entree - 1 else rep(as.numeric(p$date_entree), nk)
  o <- order(tete, entree, method = "radix")
  e1 <- entree[o[!duplicated(tete[o])]]
  if (uniforme) {
    age_entree <- p$age_min + (p$age_max - p$age_min) * tirages$u_age
    naissance <- e1 - round(age_entree * 365.25)
  } else {
    ce <- composantes_date(as.numeric(p$date_entree))
    naissance <- rep(jours_civils(ce$an - as.integer(p$age), ce$mois + 1L, ce$jour), n)
  }

  # Âge exact à la première entrée, par tête, recalculé depuis les dates (T2, T3).
  cn <- composantes_distinctes(naissance)
  x0 <- age_anniversaire(cn, composantes_distinctes(e1)$an, e1)
  a0 <- jour_anniversaire(cn, x0)
  frac0 <- (e1 - a0) / (jour_anniversaire(cn, x0 + 1L) - a0)
  # Assertion : les âges effectivement parcourus (de l'entrée à l'instant B)
  # restent dans les bornes a priori [age_bas, age_haut] de
  # controler_portefeuille(), donc dans la table ; un écart serait une
  # erreur de programmation, jamais un refus dépendant de la graine.
  x_borne <- age_anniversaire(cn, composantes_date(borne)$an, borne)
  if (min(x0) < floor(ctl$age_bas) || max(x_borne) > floor(ctl$age_haut))
    stop(sprintf(paste("simuler_portefeuille() : âges parcourus %d à %d hors des bornes a priori",
                       "%d à %d (erreur de programmation)."),
                 min(x0), max(x_borne), as.integer(floor(ctl$age_bas)), as.integer(floor(ctl$age_haut))),
         call. = FALSE)

  # Temps latent d'une transition pour des unités (têtes ou contrats) de
  # composantes de naissance cnu, d'âge x0u et de fraction frac0u au jour de
  # départ du, de sexe su, de tirages e.
  latent <- function(cause, cnu, x0u, frac0u, du, su, e) {
    instant <- rep(Inf, length(e)); jour <- rep(NA_real_, length(e))
    for (s in sexes) {
      idx <- which(su == s)
      if (!length(idx)) next
      t <- temps_latent(effective[effective$cause == cause & effective$sexe == s, , drop = FALSE],
                        lapply(cnu, `[`, idx), x0u[idx], frac0u[idx], du[idx], e[idx])
      instant[idx] <- t$instant; jour[idx] <- t$jour
    }
    list(instant = instant, jour = jour)
  }
  # Décès par tête, à partir de la première entrée ; rachat par contrat, à
  # partir de son entrée, à l'âge de la tête (modes existants : contrat = tête).
  dc <- latent("deces", cn, x0, frac0, e1, sexe, tirages$e_deces)
  if (multi) {
    cnk <- lapply(cn, `[`, tete)
    xk <- age_anniversaire(cnk, composantes_distinctes(entree)$an, entree)
    ak <- jour_anniversaire(cnk, xk)
    fk <- (entree - ak) / (jour_anniversaire(cnk, xk + 1L) - ak)
    rc <- latent("rachat", cnk, xk, fk, entree, sexe[tete], tirages$e_rachat)
  } else {
    rc <- latent("rachat", cn, x0, frac0, entree, sexe, tirages$e_rachat)
  }

  # Sortie : minimum des deux temps latents et de la censure à B, en temps
  # continu ; égalité (probabilité nulle) attribuée au décès. Décès commun
  # aux contrats de la tête en cours à son instant.
  td <- dc$instant[tete]; tr <- rc$instant
  est_deces <- td <= tr & td < borne
  est_rachat <- tr < td & tr < borne
  cause_sortie <- rep("en_cours", nk)
  cause_sortie[est_deces] <- "deces"; cause_sortie[est_rachat] <- "rachat"
  sortie <- rep(NA_real_, nk)
  sortie[est_deces] <- dc$jour[tete][est_deces]
  sortie[est_rachat] <- rc$jour[est_rachat]
  if (anyNA(sortie[cause_sortie != "en_cours"]))
    stop("simuler_portefeuille() : événement sans date (erreur de programmation).", call. = FALSE)
  # Contrat non souscrit : un contrat n'est écrit que si son entrée précède
  # l'instant de décès de la tête (double de temps_latent()). Un contrat entré
  # le jour de la première entrée l'est toujours : l'instant de décès suit
  # e_(1) (tirage exponentiel > 0) ; la règle ne dépend donc d'aucun arrondi
  # qui rendrait cet instant égal à e_(1). Modes existants : toutes les lignes.
  ecrit <- entree <= e1[tete] | entree < td
  sel <- o[ecrit[o]]
  if (any(sortie[sel][est_deces[sel]] < entree[sel][est_deces[sel]]))
    stop("simuler_portefeuille() : décès antérieur à l'entrée d'un contrat écrit (erreur de programmation).",
         call. = FALSE)

  # id_tete : seq_len(n) dans les modes existants (même objet qu'avant #92).
  resultat <- data.frame(id_contrat = seq_len(length(sel)), id_tete = if (multi) tete[sel] else seq_len(n),
                         sexe = sexe[tete][sel],
                         date_naissance = en_date(naissance[tete][sel]), date_entree = en_date(entree[sel]),
                         date_sortie = en_date(sortie[sel]), cause_sortie = cause_sortie[sel],
                         stringsAsFactors = FALSE)
  so <- sortie[sel]
  decomptes <- list(lignes = length(sel), deces = sum(est_deces[sel]), rachats = sum(est_rachat[sel]),
                    en_cours = sum(cause_sortie[sel] == "en_cours"),
                    sorties_avant_debut = sum(!is.na(so) & so < debut),
                    entrees_avant_debut = sum(entree[sel] < debut))
  if (multi) {
    # Têtes dont le décès figure dans le jeu (au moins une ligne à décès) ;
    # têtes décédées avant B sans contrat en cours à leur décès (aucune ligne
    # à décès : décès absent du jeu).
    observee <- tabulate(tete[sel][est_deces[sel]], nbins = n) > 0L
    decomptes <- c(decomptes, list(tetes = n, contrats_tires = nk, contrats_non_souscrits = nk - length(sel),
                                   tetes_decedees_observees = sum(observee),
                                   tetes_decedees_sans_contrat = sum(dc$instant < borne & !observee)))
  }
  attr(resultat, "simulation") <- list(
    loi_vraie = list(tables = lapply(loi, `[[`, "table"), facteurs = lapply(loi, `[[`, "facteurs"),
                     effective = effective, transitions = CAUSES_SIMULEES,
                     hypothese_repartition = HYPOTHESE_REPARTITION, sexes_controles = sexes),
    portefeuille = portefeuille, periode = periode,
    graine = as.integer(graine), generateur = GENERATEUR,
    ordre_tirages = if (multi) ORDRE_TIRAGES_MULTI[[ctl$mode]] else ORDRE_TIRAGES,
    version_r = R.version.string,
    decomptes = decomptes)
  # Modes multi-contrats : champ ajouté en fin seulement (annotation du
  # 25/10/2026, (c), point 5).
  if (multi)
    attr(resultat, "simulation")$contrats <- list(loi = as.double(lc), tires_par_tete = tabulate(K, nbins = m))
  # Défauts paramétrés (#93) : après le tirage complet du jeu propre, sous
  # leur propre graine ; jeu propre inchangé si defauts = NULL. Avec une
  # forme, après la projection (ci-dessous).
  if (!is.null(defauts) && is.null(forme)) resultat <- injecter_defauts(resultat, ctl_defauts, defauts, periode)
  # Forme (#169, #180) : projection du jeu propre, attribut `simulation` du
  # jeu propre plus le champ `forme` en fin (ajout compatible).
  if (!is.null(forme)) {
    sim <- attr(resultat, "simulation")
    if (ctl_forme$nom == "agregats") {
      resultat <- projeter_agregats(resultat, ctl_forme$cause, ctl_forme$par_annee, debut, borne,
                                    ctl_forme$convention, ctl_forme$unite_observation)
      # unite_observation en fin, sous les modes multi-contrats seulement (#189).
      sim$forme <- ctl_forme[c("nom", "transition", "convention", "par_annee",
                               if (!is.null(ctl_forme$unite_observation)) "unite_observation")]
      injecter <- injecter_defauts_agregats
    } else {
      pr <- projeter_stocks(resultat, ctl_forme$inventaires)
      resultat <- pr$lignes
      sim$forme <- list(nom = ctl_forme$nom, inventaires = ctl_forme$inventaires, decomptes = pr$decomptes)
      injecter <- injecter_defauts_stocks
    }
    attr(resultat, "simulation") <- sim
    # Défauts de la forme (#189, #191), après la projection, sous leur propre
    # graine.
    if (!is.null(defauts)) resultat <- injecter(resultat, ctl_defauts, defauts, periode, ctl_forme)
  }
  resultat
}
