# -----------------------------------------------------------------------------
#  tests/reference/outils_reference.R : outillage des références de
#  non-régression (issue #17 ; ADR 0009, corps et annotations du 17/10/2026)
#
#  Chargé par tests/test_reproductibilite.R, produire_references.R et
#  comparer_references.R par charger_utf8() (parse(encoding = "UTF-8") : les
#  littéraux non ASCII de l'outillage sont marqués UTF-8 quelle que soit la
#  locale, mesuré en C et C.UTF-8) ; R de base, sans évaluation de code à la
#  lecture (ni source(), ni eval(parse()), ni dget() sur un fichier de
#  référence).
#
#  Chaînes (ADR 0007, point 9.3) : toute chaîne écrite passe par
#  utf8_ou_refus(), qui applique la règle de tablexp:::octets_chaines() (une
#  chaîne au codage « bytes », non UTF-8 valide, ou non ASCII non marquée
#  dans une session ni UTF-8 ni latin-1 est refusée, jamais convertie).
#
#  - plans_reference(), jeu_reference(), jeu_reference_defauts() : plans R1 à
#    R12 et entrées de référence (points 4.1 et 4.3 ; R6 : première annotation,
#    (b) ; R7 : #32 ; R8 : #166, avec `tables_reference`, table importée en
#    valeurs littérales ; R9 : #166, avec `entree`, une entrée par plan,
#    annotation du 07/11/2026, (d) ; R10 : #167, positionnement par SMR et
#    validation de la table positionnée ; R11 : #168, test d'homogénéité entre
#    segments ; R12 : #168, le même test sous H0 vraie ; plans de méthode,
#    point 5.2) ;
#  - tirer_entree(nom), entrees_des_plans(plans) : tirage de l'entrée d'un
#    plan, chaque entrée distincte une seule fois ;
#  - aplatir_construction(x) : objet `construction` -> grandeurs (chemin,
#    type, valeurs), moins `execution`, `fiche$versions`, `fiche$plateforme`
#    (point 1.1) ; valeurs de fiche/empreintes/resultat et /code remplacées
#    par le jeton "<md5>" si et seulement si elles ont la forme ^[0-9a-f]{32}$
#    (CHEMINS_JETON_MD5 ; annotation du 20/10/2026, seconde, #67) ; tables de lignes de RESUMES_PAR_EMPREINTE résumées par
#    @lignes et @empreinte (point 1.2) ; lignes @ de structure (point 2.3) ;
#    refus d'écriture (condition de classe `refus_reference`, chemin fautif)
#    sur toute classe ou tout attribut hors format ;
#  - ecrire_reference(grandeurs, fichier, entete) : TSV UTF-8, LF, octet par
#    octet, valeurs par tablexp:::ecrire_valeurs() en forme « texte »
#    (doubles en %.17g ; ADR 0007, point 9.3 ; ADR 0011, point 7) ;
#  - lire_reference(fichier) : lecture stricte (tout double hors de la forme
#    de `%.17g` est refusé ; hors Windows, tout texte différent de
#    sprintf("%.17g") de la valeur lue l'est aussi : double_lu()) ; aller-retour
#    exact (point 2.4) ;
#  - TOLERANCES, tolerance_de(chemin) : classes de tolérance, clé qualifiée
#    `<table>/<colonne>` prioritaire, recherche par le suffixe le plus long
#    (seconde annotation, (a)) ;
#  - comparer_references(reference, obtenu, tolerances) : empreinte des
#    données en premier (point 3.2), structure exacte (point 3.1), valeurs
#    élément par élément (point 3.3 ; seconde annotation, (c)) ; un double
#    sans classe est un échec (point 3.4) ;
#  - imprimer_comparaison(r) : impression du point 3.5 ;
#  - references_orphelines(dossier) : fichiers sans plan (point 3.7) ;
#  - regle_mode(), controle_bascule(), sortie_interdite() : règles des modes
#    du workflow (point 5.2) et garde du dossier de sortie (point 9.4,
#    portable : forme_chemin()), appliquées par produire_references.R ;
#  - identifiant_plateforme(racine) : bloc de tests/controles/plateforme.R
#    (ADR 0008, point 5), pour l'en-tête (point 2.2).
#
#  Décision d'implémentation (signalée à actuary) : un vecteur atomique nommé
#  (fiche/roles, fiche/generateur) porte, comme une liste, sa ligne
#  `chemin/@noms` ; sans elle, un changement de noms passerait inaperçu.
# -----------------------------------------------------------------------------

# Chargement d'un fichier de l'outillage, littéraux marqués UTF-8 (même
# fonction, recopiée, dans les scripts qui chargent celui-ci).
charger_utf8 <- function(fichier, envir) {
  for (e in parse(fichier, encoding = "UTF-8", keep.source = FALSE)) eval(e, envir)
  invisible(envir)
}

FORMAT_REFERENCE <- "tablexp reference 1"
CHEMIN_EMPREINTE_DONNEES <- "fiche/empreintes/donnees"
# Comparaison de forme (ADR 0009, annotation du 20/10/2026, seconde ; #67) :
# les deux seuls chemins dont la valeur est remplacée par JETON_MD5, liste
# fermée, jamais une règle générique sur la forme hexadécimale (condition 1 :
# fiche/empreintes/donnees et les lignes @empreinte restent comparées en
# valeur). Valeurs réelles dans l'en-tête, hors comparaison (condition 2).
CHEMINS_JETON_MD5 <- c("fiche/empreintes/resultat", "fiche/empreintes/code")
JETON_MD5 <- "<md5>"
CHEMIN_REPRESENTATION <- "fiche/empreintes/representation"
CHEMIN_RESUMES <- "etapes/donnees/resultat"
RESUMES_PAR_EMPREINTE <- c("lignes_conservees", "numeros_conserves", "lignes_exclues",
                           "observations", "observation_des_lignes")
TYPES_REFERENCE <- c("double", "integer", "logical", "character", "Date", "factor", "NULL")

# Classes de tolérance (ADR 0009, point 3.4 et seconde annotation, (b)) ;
# 0 : classe exacte (égalité élément par élément par identical()). Seules
# les grandeurs de type double consultent cette table ; les autres types et
# les lignes @ sont exacts par leur type. Toute modification est annotée
# dans l'ADR 0009, datée, avec l'avis d'actuary (point 9.3).
CLASSES_TOLERANCE <- c(exacte = 0, "1e-14" = 1e-14, "1e-12" = 1e-12, "1e-10" = 1e-10)
# Aucune clé pour une grandeur de type integer (quatrième annotation, (b)) :
# une clé la classerait aussi si elle redevenait double, et masquerait ce
# retour à doubles_sans_tolerance().
TOLERANCES <- c(
  # Aucune clé nue (#175) : toute clé commence par le nom de l'étape
  # (`exposition`, `brut`, `positionnement`, `homogeneite`, `validation`,
  # `importance_exclusions`) ou par
  # `fiche/` (écho du plan et des hypothèses : fiche/plan/…,
  # fiche/hypotheses_<étape>/…). Une colonne de même nom d'une étape ultérieure
  # (#167, #168) ne reçoit ainsi aucune classe sans contrôle de complétude
  # (point 3.4) ; les invariants de test_reproductibilite.R le contrôlent.
  # Étapes exposition et brut (R1 à R9). Classes par nature de calcul.
  # exacte : doubles à valeur entière ou déclarée (point 3.4)
  "exposition/resultat/exposition/exposition_jours" = 0,
  "exposition/diagnostics/exposition_jours" = 0,
  "exposition/parametres/delai_declaration" = 0, "fiche/plan/delai_declaration" = 0,
  "brut/parametres/niveau" = 0, "brut/niveau" = 0, "fiche/hypotheses_brut/niveau" = 0,
  "brut/parametres/tolerance_effectif" = 0, "fiche/hypotheses_brut/tolerance_effectif" = 0,
  # 1e-14 : arithmétique IEEE de base (point 3.4)
  "exposition/resultat/exposition/exposition_centrale" = 1e-14,
  "exposition/resultat/exposition/complement_initial_cellule" = 1e-14,
  "exposition/resultat/exposition/complement_initial_fenetre" = 1e-14,
  "brut/resultat/poisson/exposition_centrale" = 1e-14, "brut/resultat/poisson/taux" = 1e-14,
  "brut/resultat/binomial/exposition_initiale" = 1e-14, "brut/resultat/binomial/taux" = 1e-14,
  "brut/resultat/kaplan_meier_sauts/age_exact" = 1e-14,
  "brut/resultat/kaplan_meier_sauts/variance_log_survie_cellule" = 1e-14,
  # 1e-12 : fonctions spéciales et libm (point 3.4)
  "brut/resultat/poisson/borne_inf_exacte" = 1e-12, "brut/resultat/poisson/borne_sup_exacte" = 1e-12,
  "brut/resultat/poisson/borne_inf_asymptotique" = 1e-12,
  "brut/resultat/poisson/borne_sup_asymptotique" = 1e-12,
  "brut/resultat/poisson/probabilite" = 1e-12,
  "brut/resultat/poisson/probabilite_borne_inf_exacte" = 1e-12,
  "brut/resultat/poisson/probabilite_borne_sup_exacte" = 1e-12,
  "brut/resultat/poisson/probabilite_borne_inf_asymptotique" = 1e-12,
  "brut/resultat/poisson/probabilite_borne_sup_asymptotique" = 1e-12,
  "brut/resultat/binomial/borne_inf_exacte" = 1e-12, "brut/resultat/binomial/borne_sup_exacte" = 1e-12,
  "brut/resultat/binomial/borne_inf_asymptotique" = 1e-12,
  "brut/resultat/binomial/borne_sup_asymptotique" = 1e-12,
  "brut/resultat/kaplan_meier/borne_inf_asymptotique" = 1e-12,
  "brut/resultat/kaplan_meier/borne_sup_asymptotique" = 1e-12,
  "brut/resultat/kaplan_meier/taux" = 1e-12, "brut/resultat/kaplan_meier/variance" = 1e-12,
  "brut/resultat/kaplan_meier_sauts/survie_cellule" = 1e-12,
  # Validation et importance des exclusions (R8, #166). Toute clé de ces deux
  # étapes commence par le nom de l'étape : `<étape>/resultat/<table>/<colonne>`
  # pour les tables du résultat, `<étape>/parametres/<paramètre>` et
  # `<étape>/diagnostics/…`, et `<étape>/<paramètre>` pour l'écho du plan dans
  # la fiche (fiche/plan/etapes/<étape>/…). Les tables génériques (`cellules`,
  # `statistiques`…) d'une étape ultérieure (#167, #168) ne reçoivent ainsi
  # aucune classe sans contrôle de complétude (point 3.4).
  # exacte : paramètres déclarés, dénombrements (doubles à valeur entière),
  # recopie de la table reçue (qx_table, aucune opération)
  "validation/parametres/plage_ages" = 0, "validation/parametres/tranches_age" = 0,
  "validation/parametres/niveau" = 0, "validation/parametres/replications" = 0,
  "validation/parametres/n_max_wilcoxon_exact" = 0, "validation/parametres/n_max_runs_exact" = 0,
  "validation/resultat/cellules/qx_table" = 0,
  "validation/resultat/sous_groupes/cellules_retenues" = 0,
  "validation/resultat/sous_groupes/observes" = 0,
  "validation/resultat/sequences_runs/positifs" = 0,
  "validation/resultat/sequences_runs/negatifs" = 0, "validation/resultat/sequences_runs/runs" = 0,
  "validation/resultat/monte_carlo/replications" = 0,
  "validation/resultat/monte_carlo/tirages_effectues" = 0,
  "validation/diagnostics/etats/evenements" = 0,
  "validation/diagnostics/evenements_sans_attendu" = 0,
  "validation/diagnostics/evenements_hors_plage" = 0,
  "validation/diagnostics/evenements_total" = 0, "importance_exclusions/parametres/niveau" = 0,
  "importance_exclusions/resultat/retraits/evenements_retires" = 0,
  "importance_exclusions/resultat/retraits/exposition_jours_retiree" = 0,
  "importance_exclusions/resultat/cellules_niveau_a/exposition_jours_retiree" = 0,
  "importance_exclusions/resultat/cellules_niveau_b/exposition_jours_retiree" = 0,
  "importance_exclusions/resultat/smr_comparateur_1/observes" = 0,
  "importance_exclusions/resultat/smr_comparateur_1/evenements_sans_exposition_retenue" = 0,
  "importance_exclusions/resultat/smr_comparateur_2/observes" = 0,
  "importance_exclusions/resultat/smr_comparateur_2/evenements_sans_valeur_table" = 0,
  "importance_exclusions/resultat/smr_comparateur_2/evenements_taux_table_egal_un" = 0,
  "importance_exclusions/resultat/smr_comparateur_2/evenements_sans_attendu" = 0,
  "importance_exclusions/resultat/smr_comparateur_2/evenements_hors_plage" = 0,
  "importance_exclusions/resultat/smr_comparateur_2/observes_conserves" = 0,
  "validation/plage_ages" = 0, "validation/tranches_age" = 0, "validation/niveau" = 0,
  "validation/replications" = 0, "importance_exclusions/niveau" = 0,
  # 1e-14 : sommes, produits et quotients IEEE sans libm
  "validation/resultat/cellules/exposition_centrale" = 1e-14,
  "validation/resultat/cellules/taux_brut" = 1e-14,
  "validation/diagnostics/exposition_hors_plage" = 1e-14,
  "importance_exclusions/resultat/retraits/exposition_centrale_retiree" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_a/exposition_centrale_retiree" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_a/exposition_centrale_conservee" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_a/taux_conserves" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_a/attendus_comparateur_1" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_b/exposition_centrale_retiree" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_b/exposition_centrale_conservee" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_b/taux_conserves" = 1e-14,
  "importance_exclusions/resultat/cellules_niveau_b/attendus_comparateur_1" = 1e-14,
  "importance_exclusions/resultat/smr_comparateur_1/rapport_variance" = 1e-14,
  "importance_exclusions/resultat/smr_comparateur_1/exposition_centrale_sans_exposition_retenue" = 1e-14,
  "importance_exclusions/resultat/smr_comparateur_2/exposition_centrale_hors_plage" = 1e-14,
  # 1e-12 : log1p, exp, racines, qpois, qgamma, ppois, pnorm, pchisq, Monte-Carlo
  "validation/resultat/cellules/taux_table" = 1e-12,
  "validation/resultat/cellules/smr_positionnement" = 1e-12,
  "validation/resultat/cellules/taux_positionne" = 1e-12,
  "validation/resultat/cellules/attendus" = 1e-12,
  "validation/resultat/cellules/probabilite_brute" = 1e-12,
  "validation/resultat/cellules/residu_reponse" = 1e-12,
  "validation/resultat/cellules/residu_pearson" = 1e-12,
  "validation/resultat/cellules/residu_deviance" = 1e-12,
  "validation/resultat/cellules/fluctuation_inf_exacte" = 1e-12,
  "validation/resultat/cellules/fluctuation_sup_exacte" = 1e-12,
  "validation/resultat/cellules/fluctuation_inf_asymptotique" = 1e-12,
  "validation/resultat/cellules/fluctuation_sup_asymptotique" = 1e-12,
  "validation/resultat/statistiques/valeur" = 1e-12,
  "validation/resultat/sous_groupes/attendus" = 1e-12,
  "validation/resultat/sous_groupes/smr" = 1e-12,
  "validation/resultat/sous_groupes/borne_inf_exacte" = 1e-12,
  "validation/resultat/sous_groupes/borne_sup_exacte" = 1e-12,
  "validation/resultat/sous_groupes/p_inferieure" = 1e-12,
  "validation/resultat/sous_groupes/p_superieure" = 1e-12,
  "validation/resultat/sous_groupes/p_centrale" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_1/attendus" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_1/smr" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_1/borne_inf_exacte" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_1/borne_sup_exacte" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_1/borne_inf_asymptotique" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_1/borne_sup_asymptotique" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/attendus" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/smr" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/borne_inf_exacte" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/borne_sup_exacte" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/borne_inf_asymptotique" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/borne_sup_asymptotique" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/attendus_conserves" = 1e-12,
  "importance_exclusions/resultat/smr_comparateur_2/smr_conserves" = 1e-12,
  # Positionnement (#167), préparées pour le plan de méthode de (R) ; classes
  # de la spécification d'actuary-approfondi (§ 5 : observes exacte ;
  # bornes, qx et ses bornes 1e-12), complétées sur
  # le modèle de `validation` pour les cellules, les paramètres et les
  # diagnostics. Toute clé commence par `positionnement/` (#175).
  # exacte : paramètres déclarés, dénombrements, recopie de la table reçue
  "positionnement/parametres/plage_ages" = 0, "positionnement/parametres/niveau" = 0,
  "positionnement/resultat/cellules/qx_table" = 0,
  "positionnement/resultat/smr/observes" = 0,
  "positionnement/resultat/table_positionnee/qx_reference" = 0,
  "positionnement/diagnostics/etats/evenements" = 0,
  "positionnement/diagnostics/evenements_sans_attendu" = 0,
  "positionnement/diagnostics/evenements_hors_plage" = 0,
  "positionnement/diagnostics/evenements_total" = 0,
  "positionnement/plage_ages" = 0, "positionnement/niveau" = 0,
  # 1e-14 : sommes et quotients IEEE
  "positionnement/resultat/cellules/exposition_centrale" = 1e-14,
  "positionnement/diagnostics/exposition_hors_plage" = 1e-14,
  # 1e-12 : log1p, expm1, exp, qnorm, qgamma ; attendus et smr (somme de
  # E^c mu~, mu~ par log1p) : spécification corrigée par l'avis d'actuary du
  # 07/11/2026
  "positionnement/resultat/smr/attendus" = 1e-12,
  "positionnement/resultat/smr/smr" = 1e-12,
  "positionnement/resultat/cellules/taux_table" = 1e-12,
  "positionnement/resultat/cellules/attendus" = 1e-12,
  "positionnement/resultat/smr/borne_inf_exacte" = 1e-12,
  "positionnement/resultat/smr/borne_sup_exacte" = 1e-12,
  "positionnement/resultat/smr/borne_inf_asymptotique" = 1e-12,
  "positionnement/resultat/smr/borne_sup_asymptotique" = 1e-12,
  "positionnement/resultat/table_positionnee/qx" = 1e-12,
  "positionnement/resultat/table_positionnee/borne_inf_exacte" = 1e-12,
  "positionnement/resultat/table_positionnee/borne_sup_exacte" = 1e-12,
  "positionnement/resultat/table_positionnee/borne_inf_asymptotique" = 1e-12,
  "positionnement/resultat/table_positionnee/borne_sup_asymptotique" = 1e-12,
  # Homogénéité entre segments (#168), préparées pour le plan de méthode de
  # (R), sur le modèle de `validation` : paramètres déclarés, dénombrements
  # et sommes d'entiers exacts ; sommes, produits et quotients IEEE à 1e-14 ;
  # dbinom (p exacte) et Monte-Carlo à 1e-12 ; grandeurs dérivées de
  # U = O - A (statistique du score, z, p asymptotique) à 1e-10 (décision du
  # mainteneur du 08/11/2026, sur avis d'actuary). Toute clé commence par
  # `homogeneite/` (#175).
  # exacte
  "homogeneite/parametres/plage_ages" = 0, "homogeneite/parametres/tranches_age" = 0,
  "homogeneite/parametres/replications" = 0,
  "homogeneite/resultat/segments/observes" = 0,
  "homogeneite/resultat/monte_carlo/replications" = 0,
  "homogeneite/resultat/monte_carlo/jeux/tirages_effectues" = 0,
  "homogeneite/diagnostics/evenements_cellules_evenement_sans_exposition" = 0,
  "homogeneite/diagnostics/evenements_hors_plage" = 0, "homogeneite/diagnostics/evenements_total" = 0,
  "homogeneite/plage_ages" = 0, "homogeneite/tranches_age" = 0, "homogeneite/replications" = 0,
  # 1e-14
  "homogeneite/resultat/segments/exposition_centrale" = 1e-14,
  "homogeneite/resultat/segments/attendus" = 1e-14,
  "homogeneite/resultat/segments/rapport_observes_attendus" = 1e-14,
  "homogeneite/diagnostics/exposition_hors_plage" = 1e-14,
  # 1e-12 : p exacte (avis d'actuary du 08/11/2026) : elle dépend des
  # attendus par pi = E_g / E_strate (score_homogeneite()) et par la
  # frontière |k - A_1| >= |o_1 - A_1|, tenue à TOLERANCE_RESIDU_NUL x A_1
  # près ; elle ne contient pas la différence O - A, donc pas d'annulation.
  # L'erreur relative de pi entre dans dbinom avec un facteur borné, d'ordre
  # z sqrt(Var(O_1)) / (1 - pi) sur la queue (mesuré par actuary : <= 9e-15
  # pour +-4 ulp sur pi, jusqu'à 16 000 décès, z = 4) ; marge qui décroît
  # comme z sqrt(n) : passer à 1e-10 si un futur jeu de référence atteint
  # ~1e6 décès par strate en queue lointaine (motif : conditionnement en
  # pi). p de Monte-Carlo (1 + K) / (B + 1) et son erreur : comptage de
  # répliques.
  "homogeneite/resultat/tests/p_exacte" = 1e-12, "homogeneite/resultat/tests/p_monte_carlo" = 1e-12,
  "homogeneite/resultat/tests/erreur_monte_carlo" = 1e-12,
  # 1e-10 : U = O - A, différence de deux grandeurs proches (annulation :
  # écart relatif de U de l'ordre de eps A / |U|), puis T = U' V^- U,
  # z = U / sqrt(v) et pchisq(T, ddl)
  "homogeneite/resultat/tests/statistique" = 1e-10, "homogeneite/resultat/tests/p_asymptotique" = 1e-10,
  "homogeneite/resultat/segments/z" = 1e-10)

nom_classe <- function(tol) names(CLASSES_TOLERANCE)[match(tol, CLASSES_TOLERANCE)]

# --- entrée et plans de référence (ADR 0009, points 4.1 et 4.3) --------------

PERIODE_REFERENCE <- list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31"))
GRAINE_REFERENCE <- 20261016L
GRAINE_MONTE_CARLO_R8 <- 20261107L  # Monte-Carlo de la validation de R8 et de R9 (distincte de celle du jeu)
# Monte-Carlo de la validation de R10 (réplications multinomiales conditionnelles
# aux décès de chaque segment, relation positionnee) et du test d'homogénéité de
# R11 (réplications conditionnelles aux décès de chaque cellule) : une graine
# propre à chaque plan, distinctes entre elles, de celle du jeu
# (GRAINE_REFERENCE), de GRAINE_MONTE_CARLO_R8, de celle des défauts de R9
# (GRAINE_DEFAUTS_R9) et de toute graine d'un test ou d'une mesure du dépôt
# (recherche de 2026xxxx dans le dépôt entier au 07/11/2026 : 20261113 et
# 20261114 n'y figurent pas).
# R12 (#168, M307) : test d'homogénéité sous H0 exactement vraie (transition
# en_cours -> rachat, facteurs de rachat H = F = 1 dans simuler_reference()),
# où les p de Monte-Carlo ne sont pas au plancher et épinglent le flux des
# réplications ; graine propre, absente du dépôt au 07/11/2026 (20261115).
GRAINE_MONTE_CARLO_R10 <- 20261113L
GRAINE_MONTE_CARLO_R11 <- 20261114L
GRAINE_MONTE_CARLO_R12 <- 20261115L
ROLES_REFERENCE <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe",
                     date_naissance = "date_naissance", date_entree = "date_entree",
                     date_sortie = "date_sortie", cause_sortie = "cause_sortie")

# Entrée de R9 (#166 ; annotation du 07/11/2026, (d)) : le jeu de
# jeu_reference() (même loi, même portefeuille, même graine du jeu), altéré
# après son tirage complet par simuler_portefeuille(defauts =), sous une graine
# propre aux défauts, distincte de celle du jeu (GRAINE_REFERENCE) et de celle
# du Monte-Carlo (GRAINE_MONTE_CARLO_R8). Un motif par classe de mesurabilité
# de l'étape importance_exclusions : id_contrat_manquant (mesurable aux niveaux
# A et B), sexe_manquant (mesurable au niveau A, non calculable au niveau B
# sous la segmentation `sexe`), entree_manquante (non mesurable). Nombre fixe
# de lignes par motif, toutes causes de sortie admises. La graine des défauts
# diffère de 20261025, déjà employée par les tests unitaires
# (test_importance_exclusions.R, test_simulation.R) et tests/mesures/ (décision
# du mainteneur du 07/11/2026).
GRAINE_DEFAUTS_R9 <- 20261108L
MOTIFS_DEFAUTS_R9 <- c("id_contrat_manquant", "sexe_manquant", "entree_manquante")
NOMBRE_PAR_MOTIF_R9 <- 500L

# Jeu de l'entrée de référence : `defauts` NULL pour jeu_reference() (aucun
# défaut), sinon la liste (graine, declarations) de simuler_portefeuille().
simuler_reference <- function(defauts = NULL) {
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
              rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
             origine = as.Date("2005-01-01"))
  tablexp::simuler_portefeuille(loi, pf, PERIODE_REFERENCE, GRAINE_REFERENCE, defauts = defauts)
}

jeu_reference <- function() list(donnees = simuler_reference(), roles = ROLES_REFERENCE)

jeu_reference_defauts <- function() {
  declarations <- lapply(MOTIFS_DEFAUTS_R9, function(m)
    list(type = m, cible = c("deces", "rachat", "en_cours"), nombre = NOMBRE_PAR_MOTIF_R9))
  list(donnees = simuler_reference(list(graine = GRAINE_DEFAUTS_R9, declarations = declarations)),
       roles = ROLES_REFERENCE)
}

# Entrée d'un plan (champ `entree` de plans_reference() : le nom de l'entrée) :
# `reference` (jeu_reference(), R1 à R8, R10 à R12) ou `reference_defauts`
# (jeu_reference_defauts(), R9). Un nom plutôt qu'une fonction, pour que
# l'outillage tire chaque entrée distincte une seule fois (entrees_des_plans()).
tirer_entree <- function(nom)
  switch(nom, reference = jeu_reference(), reference_defauts = jeu_reference_defauts(),
         stop("entrée de référence inconnue : ", nom, call. = FALSE))

# Entrées des plans `plans` : liste nommée (nom de l'entrée -> list(donnees,
# roles)), chaque entrée tirée une fois ; l'entrée d'un plan p est
# `entrees[[p$entree]]`.
entrees_des_plans <- function(plans) {
  noms <- unique(vapply(plans, function(p) {
    if (!is.character(p$entree) || length(p$entree) != 1L || is.na(p$entree))
      stop("plan ", if (is.null(p$identifiant)) "(sans identifiant)" else p$identifiant, " : champ entree absent ou non chaîne", call. = FALSE)
    p$entree
  }, ""))
  stats::setNames(lapply(noms, tirer_entree), noms)
}

# Table de référence de R8 (#166) : statut `importee`, en valeurs littérales
# (aucun calcul de la libm à l'exécution : la référence ne dépend ni d'une
# table synthétique, ADR 0015, point 2.3, ni de #163). Âges 20 à 90, sexes H
# et F. Les valeurs ont été produites une seule fois, hors de l'outillage, par
#   H : q_x = 1 - exp(-A - B c^x (c - 1) / ln c), A = 2e-4, B = 2,5e-5, c = 1,1
#       (loi de Gompertz-Makeham de jeu_reference()) ;
#   F : 0,6 fois q_H non arrondi ;
# chaque valeur est ensuite arrondie à 5 chiffres significatifs, puis recopiée
# ici : elles ne se recalculent pas. La table est indépendante des observations
# (aucun ajustement sur le jeu tiré).
AGES_TABLE_R8 <- 20:90
QX_TABLE_R8 <- list(
  H = c(
    0.00037639, 0.00039403, 0.00041344, 0.00043478, 0.00045825, 0.00048408,
    0.00051248, 0.00054373, 0.0005781, 0.0006159, 0.00065748, 0.00070322,
    0.00075353, 0.00080887, 0.00086974, 0.00093669, 0.0010103, 0.0010913,
    0.0011804, 0.0012784, 0.0013862, 0.0015047, 0.0016351, 0.0017785,
    0.0019362, 0.0021097, 0.0023005, 0.0025103, 0.002741, 0.0029948,
    0.0032738, 0.0035807, 0.0039181, 0.0042892, 0.0046972, 0.0051458,
    0.005639, 0.0061813, 0.0067774, 0.0074328, 0.0081532, 0.008945,
    0.0098153, 0.010772, 0.011823, 0.012978, 0.014246, 0.01564,
    0.017171, 0.018852, 0.020698, 0.022725, 0.024949, 0.02739,
    0.030068, 0.033005, 0.036226, 0.039756, 0.043625, 0.047862,
    0.052501, 0.057579, 0.063132, 0.069203, 0.075836, 0.083078,
    0.090978, 0.099589, 0.10897, 0.11917, 0.13026),
  F = c(
    0.00022584, 0.00023642, 0.00024806, 0.00026087, 0.00027495, 0.00029045,
    0.00030749, 0.00032624, 0.00034686, 0.00036954, 0.00039449, 0.00042193,
    0.00045212, 0.00048532, 0.00052184, 0.00056202, 0.0006062, 0.0006548,
    0.00070825, 0.00076705, 0.00083172, 0.00090284, 0.00098107, 0.0010671,
    0.0011617, 0.0012658, 0.0013803, 0.0015062, 0.0016446, 0.0017969,
    0.0019643, 0.0021484, 0.0023509, 0.0025735, 0.0028183, 0.0030875,
    0.0033834, 0.0037088, 0.0040665, 0.0044597, 0.0048919, 0.005367,
    0.0058892, 0.006463, 0.0070936, 0.0077865, 0.0085478, 0.009384,
    0.010303, 0.011311, 0.012419, 0.013635, 0.014969, 0.016434,
    0.018041, 0.019803, 0.021735, 0.023854, 0.026175, 0.028717,
    0.031501, 0.034547, 0.037879, 0.041522, 0.045502, 0.049847,
    0.054587, 0.059754, 0.065381, 0.071503, 0.078156))

tables_reference_r8 <- function() {
  list(tv = tablexp::table_reference(
    data.frame(age = rep(AGES_TABLE_R8, 2L), sexe = rep(c("H", "F"), each = length(AGES_TABLE_R8)),
               qx = c(QX_TABLE_R8$H, QX_TABLE_R8$F), stringsAsFactors = FALSE),
    nom = "table de référence de R8 (loi vraie arrondie, valeurs littérales)", statut = "importee",
    source = "valeurs littérales de tests/reference/outils_reference.R (QX_TABLE_R8), arrondies à 5 chiffres significatifs"))
}

# `exclusions`, `positionnement`, `homogeneite`, `validation`, `importance` et
# `graine` : étapes et graine facultatives (R8 ; R10 à R12 : #167, #168) ;
# absents, le plan est celui de R1 à R7 (champs et ordre inchangés). Les
# étapes sont écrites dans l'ordre d'ETAPES (R/construire_table.R).
plan_reference <- function(transition = c("vivant", "deces"), segmentation = "sexe", delai = 0,
                           convention = "plus_fine", par_annee = FALSE,
                           estimateurs = c("poisson", "binomial"), niveau = 0.95,
                           exclusions = character(0), positionnement = NULL, homogeneite = NULL,
                           validation = NULL, importance = NULL, graine = NULL) {
  etapes <- list(donnees = list(exclusions = exclusions),
                 exposition = list(convention = convention, par_annee = par_annee),
                 brut = list(estimateurs = estimateurs, niveau = niveau))
  if (!is.null(positionnement)) etapes$positionnement <- positionnement
  if (!is.null(homogeneite)) etapes$homogeneite <- homogeneite
  if (!is.null(validation)) etapes$validation <- validation
  if (!is.null(importance)) etapes$importance_exclusions <- importance
  plan <- list(transition = list(depart = transition[1L], arrivee = transition[2L]),
               periode = PERIODE_REFERENCE, delai_declaration = delai, segmentation = segmentation)
  if (!is.null(graine)) plan$graine <- graine
  plan$etapes <- etapes
  plan
}

# Tables lues par un plan : NULL, ou une fonction sans argument qui rend la
# liste nommée des objets `table_reference` (argument `tables_reference` de
# construire_table() et de rejouer_fiche(), ADR 0015). Fonction et non liste :
# plans_reference() est appelée avant le chargement de tablexp (par exemple par
# produire_references.R, qui lit les plans avant d'installer le package), et
# tablexp::table_reference() n'est alors pas disponible.
tables_du_plan <- function(p) if (is.null(p$tables_reference)) NULL else p$tables_reference()

# Un identifiant n'est jamais réemployé (point 4.3). `entree` : nom de
# l'entrée du plan (tirer_entree()), `reference` pour R1 à R8, R10 à R12.
plans_reference <- function() {
  p <- function(id, description, plan, tables_reference = NULL, entree = "reference")
    list(identifiant = id, description = description, plan = plan, tables_reference = tables_reference,
         entree = entree)
  # Étapes validation et importance_exclusions de R8 et de R9 : un seul texte.
  validation <- list(table_reference = "tv", relation_table = "independante",
                     plage_ages = c(20, 90), tranches_age = c(20, 50, 70),
                     niveau = 0.95, replications = 999)
  importance <- list(niveau = 0.95)
  # R10 (#167) : positionnement par SMR sur la table de R8, puis validation de
  # la table positionnée (relation positionnee : même clé de table et même
  # plage d'âges que le positionnement, tranches libres).
  positionnement <- list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95)
  validation_positionnee <- validation
  validation_positionnee$relation_table <- "positionnee"
  # R11 (#168) et R12 (#168, M307) : test d'homogénéité entre segments (sexe), sous H0 fausse (R1) et vraie (R5).
  homogeneite <- list(plage_ages = c(20, 90), tranches_age = c(20, 50, 70), replications = 999)
  list(
    R1 = p("R1", "décès, référence principale", plan_reference()),
    R2 = p("R2", "sans segmentation, délai 90, niveau 0,99",
           plan_reference(segmentation = character(0), delai = 90, niveau = 0.99)),
    R3 = p("R3", "convention forcée (age_atteint_annee)", plan_reference(convention = "age_atteint_annee")),
    R4 = p("R4", "par année (surface), Poisson seul", plan_reference(par_annee = TRUE, estimateurs = "poisson")),
    R5 = p("R5", "en cours -> racheté, unité contrat", plan_reference(transition = c("en_cours", "rachat"))),
    R6 = p("R6", "Kaplan-Meier (R1 avec kaplan_meier)",
           plan_reference(estimateurs = c("poisson", "binomial", "kaplan_meier"))),
    R7 = p("R7", "Kaplan-Meier sous age_atteint_annee (R3 avec kaplan_meier)",
           plan_reference(convention = "age_atteint_annee",
                          estimateurs = c("poisson", "binomial", "kaplan_meier"))),
    R8 = p("R8", "validation et importance des exclusions (R1 avec table importée ; exclusions déclarées, sans ligne touchée par l'entrée de référence)",
           plan_reference(exclusions = c("entree_manquante", "sexe_manquant"),
                          validation = validation, importance = importance, graine = GRAINE_MONTE_CARLO_R8),
           tables_reference = tables_reference_r8),
    R9 = p("R9", sprintf("validation et importance des exclusions sur une entrée à défauts injectés (plan de R8, exclusions id_contrat_manquant en plus ; %s, %d lignes chacun, graine des défauts %d)",
                         paste(MOTIFS_DEFAUTS_R9, collapse = ", "), NOMBRE_PAR_MOTIF_R9, GRAINE_DEFAUTS_R9),
           plan_reference(exclusions = c("id_contrat_manquant", "entree_manquante", "sexe_manquant"),
                          validation = validation, importance = importance, graine = GRAINE_MONTE_CARLO_R8),
           tables_reference = tables_reference_r8, entree = "reference_defauts"),
    R10 = p("R10", "positionnement par SMR sur la table de R8 et validation de la table positionnée (plan de R8 avec les étapes positionnement et validation en relation positionnee ; exclusions déclarées, sans ligne touchée par l'entrée de référence)",
            plan_reference(exclusions = c("entree_manquante", "sexe_manquant"), positionnement = positionnement,
                           validation = validation_positionnee, importance = importance,
                           graine = GRAINE_MONTE_CARLO_R10),
            tables_reference = tables_reference_r8),
    R11 = p("R11", "test d'homogénéité entre segments (R1 avec l'étape homogeneite)",
            plan_reference(homogeneite = homogeneite, graine = GRAINE_MONTE_CARLO_R11)),
    R12 = p("R12", "test d'homogénéité entre segments sous H0 (R5 avec l'étape homogeneite ; le sexe est sans effet sur la loi vraie du rachat) : épingle le flux des réplications",
            plan_reference(transition = c("en_cours", "rachat"), homogeneite = homogeneite,
                           graine = GRAINE_MONTE_CARLO_R12)))
}

# --- aplatissement (points 1 et 2.3) ------------------------------------------

refus_reference <- function(chemin, message) {
  stop(structure(class = c("refus_reference", "error", "condition"),
                 list(message = sprintf("référence, refus d'écriture : %s : %s",
                                        if (nzchar(chemin)) chemin else "(objet)", message),
                      chemin = chemin, call = NULL)))
}

# Règle d'octets_chaines() (R/rapport.R, ADR 0007, point 9.3), reprise par
# ::: : un refus_tablexp devient un refus d'écriture au chemin fautif.
utf8_ou_refus <- function(x, chemin) {
  x <- as.character(x)
  tryCatch(tablexp:::octets_chaines(x, chemin),
           refus_tablexp = function(e) refus_reference(chemin, conditionMessage(e)))
  enc2utf8(x)
}

joindre <- function(chemin, nom) if (nzchar(chemin)) paste0(chemin, "/", nom) else nom

# Noms admis dans un chemin : non vides (sinon [i]), sans `/`, tabulation,
# fin de ligne, ni `@` ou `#` initial, ni forme `[i]` ; uniques.
segments <- function(x, chemin) {
  n <- names(x)
  if (is.null(n)) n <- rep("", length(x))
  if (anyNA(n)) refus_reference(chemin, "nom manquant (NA)")
  vides <- !nzchar(n)
  bad <- !vides & (grepl("[/\t\n\r]", n, useBytes = TRUE) | grepl("^[@#]", n) | grepl("^\\[[0-9]+\\]$", n))
  if (any(bad)) refus_reference(chemin, sprintf("nom inadmissible dans un chemin : %s", n[bad][1L]))
  s <- ifelse(vides, sprintf("[%d]", seq_along(n)), n)
  if (anyDuplicated(s)) refus_reference(chemin, sprintf("nom en double : %s", s[duplicated(s)][1L]))
  s
}

attributs_hors <- function(x, admis) setdiff(names(attributes(x)), admis)

# Vecteur atomique comparé : rend list(type, valeurs) normalisée, ou refuse.
atome <- function(x, chemin) {
  if (is.factor(x)) {
    a <- attributs_hors(x, c("levels", "class", "names"))
    if (length(a) || !identical(class(x), "factor"))
      refus_reference(chemin, sprintf("facteur de classe %s ou d'attribut %s", paste(class(x), collapse = "/"),
                                      paste(a, collapse = ", ")))
    return(list(type = "factor", valeurs = utf8_ou_refus(unname(x), chemin)))
  }
  if (is.object(x)) {
    if (!identical(class(x), "Date"))
      refus_reference(chemin, sprintf("classe %s hors du format (version 1)", paste(class(x), collapse = "/")))
    a <- attributs_hors(x, c("class", "names"))
    if (length(a)) refus_reference(chemin, sprintf("attribut inattendu : %s", paste(a, collapse = ", ")))
    j <- unclass(unname(x))
    if (!is.double(j) && !is.integer(j)) refus_reference(chemin, "date de type non numérique")
    j <- as.double(j)
    if (any(!is.na(j) & (!is.finite(j) | j != floor(j))))
      refus_reference(chemin, "date non entière ou infinie (non représentable en %Y-%m-%d)")
    return(list(type = "Date", valeurs = structure(j, class = "Date")))
  }
  a <- attributs_hors(x, "names")
  if (length(a)) refus_reference(chemin, sprintf("attribut inattendu : %s", paste(a, collapse = ", ")))
  type <- switch(typeof(x), double = "double", integer = "integer", logical = "logical",
                 character = "character", NA_character_)
  if (is.na(type)) refus_reference(chemin, sprintf("type %s hors du format", typeof(x)))
  v <- unname(x)
  if (type == "character") v <- utf8_ou_refus(v, chemin)
  list(type = type, valeurs = v)
}

# Représentation canonique d'une table de lignes résumée (point 1.2) :
# data.frame : par colonne « #<nom><TAB><type> » puis ses valeurs ;
# vecteur : « #<nom><TAB><type> » puis ses valeurs ; liste : par élément
# « @<nom> » puis sa représentation. Attributs hors names, class et
# row.names ignorés (l'entrée est identifiée par fiche/empreintes/donnees).
octets_resume <- function(x, nom, chemin) {
  colonne <- function(v, n) {
    cc <- tryCatch(tablexp:::ecrire_valeurs(v, "texte", n), error = function(e)
      refus_reference(joindre(chemin, n), conditionMessage(e)))
    c(charToRaw(utf8_ou_refus(sprintf("#%s\t%s\n", n, cc$type), joindre(chemin, n))), cc$octets)
  }
  if (is.data.frame(x))
    return(unlist(lapply(names(x), function(n) colonne(x[[n]], n)), use.names = FALSE))
  if (is.list(x)) {
    s <- segments(x, chemin)
    return(unlist(lapply(seq_along(x), function(i)
      c(charToRaw(utf8_ou_refus(sprintf("@%s\n", s[i]), joindre(chemin, s[i]))), octets_resume(x[[i]], s[i], joindre(chemin, s[i])))),
      use.names = FALSE))
  }
  colonne(x, nom)
}

empreinte_resume <- function(x, nom, chemin) {
  f <- tempfile("reference_resume_")
  on.exit(unlink(f))
  writeBin(c(raw(0), octets_resume(x, nom, chemin)), f)
  unname(tools::md5sum(f))
}

# Rend list(types = <chr nommé par chemin>, valeurs = <liste nommée par
# chemin>), dans l'ordre de parcours en profondeur.
aplatir_construction <- function(x) {
  if (!is.list(x) || !identical(class(x), "construction"))
    refus_reference("", sprintf("objet de classe %s au lieu de construction", paste(class(x), collapse = "/")))
  x$execution <- NULL
  if (is.list(x$fiche)) { x$fiche$versions <- NULL; x$fiche$plateforme <- NULL }
  acc <- new.env(parent = emptyenv())
  acc$types <- character(0); acc$valeurs <- list(); acc$n <- 0L
  ajouter <- function(chemin, type, valeurs) {
    acc$n <- acc$n + 1L
    acc$types[acc$n] <- type; names(acc$types)[acc$n] <- chemin
    acc$valeurs[acc$n] <- list(valeurs); names(acc$valeurs)[acc$n] <- chemin
  }
  noms <- function(chemin, x) ajouter(joindre(chemin, "@noms"), "character", segments_ecrits(x, chemin))
  segments_ecrits <- function(x, chemin) { segments(x, chemin); n <- names(x); if (is.null(n)) character(0) else utf8_ou_refus(n, chemin) }
  parcourir <- function(v, chemin, nom) {
    if (identical(dirname_chemin(chemin), CHEMIN_RESUMES) && nom %in% RESUMES_PAR_EMPREINTE) {
      ajouter(joindre(chemin, "@lignes"), "integer", as.integer(NROW(v)))
      ajouter(joindre(chemin, "@empreinte"), "character", empreinte_resume(v, nom, chemin))
      return(invisible())
    }
    if (is.null(v)) return(ajouter(chemin, "NULL", NULL))
    if (is.data.frame(v)) {
      a <- attributs_hors(v, c("names", "class", "row.names"))
      if (!identical(class(v), "data.frame") || length(a))
        refus_reference(chemin, sprintf("data.frame de classe %s ou d'attribut %s",
                                        paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
      if (.row_names_info(v, type = 1L) > 0L) refus_reference(chemin, "noms de lignes non automatiques")
      s <- segments(v, chemin)
      ajouter(joindre(chemin, "@colonnes"), "character", utf8_ou_refus(names(v), chemin))
      ajouter(joindre(chemin, "@lignes"), "integer", nrow(v))
      for (i in seq_along(v)) {
        if (is.list(v[[i]])) refus_reference(joindre(chemin, s[i]), "colonne de type liste")
        parcourir(v[[i]], joindre(chemin, s[i]), s[i])
      }
      return(invisible())
    }
    if (is.list(v)) {
      admis <- if (nzchar(chemin)) "names" else c("names", "class")
      a <- attributs_hors(v, admis)
      if (length(a) || (nzchar(chemin) && is.object(v)))
        refus_reference(chemin, sprintf("liste de classe %s ou d'attribut %s",
                                        paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
      s <- segments(v, chemin)
      noms(chemin, v)
      for (i in seq_along(v)) parcourir(v[[i]], joindre(chemin, s[i]), s[i])
      return(invisible())
    }
    if (!is.atomic(v)) refus_reference(chemin, sprintf("type %s hors du format", typeof(v)))
    r <- atome(v, chemin)
    if (!is.null(names(v))) noms(chemin, v)
    ajouter(chemin, r$type, r$valeurs)
  }
  ajouter("@classe", "character", "construction")
  parcourir(unclass(x), "", "")
  # Jeton (annotation du 20/10/2026, seconde, (b)) : seulement un character de
  # longueur 1 de la forme ^[0-9a-f]{32}$ ; toute autre valeur (NA, majuscules,
  # autre longueur ou type) est gardée telle quelle, donc comparée au jeton de
  # la référence et relevée en écart. Chemin, type et longueur inchangés.
  for (chemin in intersect(CHEMINS_JETON_MD5, names(acc$valeurs))) {
    v <- acc$valeurs[[chemin]]
    if (identical(acc$types[[chemin]], "character") && length(v) == 1L && !is.na(v) &&
        grepl("^[0-9a-f]{32}$", v)) acc$valeurs[[chemin]] <- JETON_MD5
  }
  list(types = acc$types, valeurs = acc$valeurs)
}

dirname_chemin <- function(chemin) sub("/[^/]*$", "", chemin)

# --- écriture et lecture (points 2.1 à 2.4) -----------------------------------

octets_valeurs <- function(type, v, chemin) {
  if (type == "NULL" || !length(v)) return(raw(0))
  w <- if (type == "factor") factor(v, levels = unique(v)) else v
  tryCatch(tablexp:::ecrire_valeurs(w, "texte", chemin)$octets, error = function(e)
    refus_reference(chemin, conditionMessage(e)))
}

nettoyer_entete <- function(x) {
  y <- gsub("[\001-\037\177]", " ", utf8_ou_refus(x, "(en-tête)"), useBytes = TRUE)
  Encoding(y) <- "UTF-8"
  y
}

# entete : vecteur de chaînes nommé (clé -> valeur), écrit après la ligne de
# format, dans l'ordre ; une clé peut se répéter.
ecrire_reference <- function(grandeurs, fichier, entete = character(0)) {
  lf <- as.raw(10L); tab <- as.raw(9L)
  e <- c(paste0("# ", FORMAT_REFERENCE),
         if (length(entete)) sprintf("# %s : %s", nettoyer_entete(names(entete)), nettoyer_entete(entete)))
  morceaux <- vector("list", length(grandeurs$types) + 1L)
  morceaux[[1L]] <- unlist(lapply(utf8_ou_refus(e, "(en-tête)"), function(l) c(charToRaw(l), lf)), use.names = FALSE)
  chemins <- names(grandeurs$types)
  for (i in seq_along(chemins)) {
    type <- grandeurs$types[[i]]; v <- grandeurs$valeurs[[i]]
    if (!type %in% TYPES_REFERENCE) refus_reference(chemins[i], sprintf("type %s hors du format", type))
    o <- octets_valeurs(type, v, chemins[i])
    tete <- charToRaw(utf8_ou_refus(sprintf("%s\t%s\t%d", chemins[i], type, length(v)), chemins[i]))
    morceaux[[i + 1L]] <- if (length(o)) { o[o == lf] <- tab; o[length(o)] <- lf; c(tete, tab, o) } else c(tete, lf)
  }
  writeBin(unlist(morceaux, use.names = FALSE), fichier)
  invisible(fichier)
}

erreur_lecture <- function(fichier, message)
  stop(sprintf("référence illisible (%s) : %s", basename(fichier), message), call. = FALSE)

# Désechappement octet par octet (useBytes partout ; jamais substr(), qui
# compte en caractères sur une chaîne marquée UTF-8) ; le résultat est
# marqué UTF-8 et vérifié valide.
desechapper <- function(s, fichier, chemin) {
  na <- s == "NA"
  ok <- na | grepl('^".*"$', s, useBytes = TRUE)
  if (!all(ok)) erreur_lecture(fichier, sprintf("%s : chaîne sans guillemets", chemin))
  y <- sub('^"(.*)"$', "\\1", s[!na], useBytes = TRUE)
  m <- gregexpr("\\\\.|\"", y, useBytes = TRUE)
  table_esc <- c("\\\\" = "\\", "\\n" = "\n", "\\r" = "\r", "\\t" = "\t", "\\\"" = "\"")
  regmatches(y, m) <- lapply(regmatches(y, m), function(e) {
    r <- table_esc[e]
    if (anyNA(r)) erreur_lecture(fichier, sprintf("%s : échappement ou guillemet inattendu", chemin))
    unname(r)
  })
  Encoding(y) <- "UTF-8"
  if (!all(validUTF8(y))) erreur_lecture(fichier, sprintf("%s : chaîne qui n'est pas de l'UTF-8 valide", chemin))
  x <- rep(NA_character_, length(s))
  x[!na] <- y
  x
}

TEXTES_SPECIAUX_DOUBLE <- c("NA", "NaN", "Inf", "-Inf")

# Forme des textes de double que produit `%.17g` (conversion g de la norme C,
# précision 17, sans indicateur « # ») : NA, NaN, Inf, -Inf ; sinon au plus 17
# chiffres significatifs, zéros terminaux de la partie fractionnaire
# supprimés, notation fixe si l'exposant décimal X vérifie -4 <= X < 17 et
# notation à exposant (signé, deux chiffres au moins) sinon ; ni espace, ni
# « + » ou zéro de tête, ni hexadécimal. Cette forme est la même sur toutes
# les plateformes ; seuls les chiffres peuvent différer (voir double_lu()).
forme_double_17g <- function(s) {
  fixe <- grepl("^-?(0|[1-9][0-9]{0,16}(\\.[0-9]*[1-9])?|0\\.0{0,3}[1-9]([0-9]*[1-9])?)$", s)
  expo <- grepl("^-?[1-9](\\.[0-9]{0,15}[1-9])?e[-+]([0-9]{2}|[1-9][0-9]{2})$", s)
  x <- suppressWarnings(as.integer(sub("^.*e", "", s)))
  chiffres <- nchar(sub("^0+", "", gsub("[^0-9]", "", sub("e.*$", "", s))))
  s %in% TEXTES_SPECIAUX_DOUBLE | ((fixe | (expo & ((x <= -5L | x >= 17L) %in% TRUE))) & chiffres <= 17L)
}

# Lecture d'un double (ADR 0009, point 2.4 : mêmes valeurs au bit près). Un
# texte de 17 chiffres significatifs au plus désigne un seul double, mais
# as.numeric() (R_strtod) ne lit pas toujours ce double : certains décimaux
# acceptés par la forme de `%.17g` sont lus en un double voisin (ex.
# 0.001673229520730213 : 9ac77e84086a5b3f au lieu de 9bc77e84086a5b3f ;
# mesuré). Deux branches, selon la plateforme (`windows`) :
#  - hors Windows : preuve d'aller-retour. Le texte doit être égal à
#    sprintf("%.17g") de la valeur lue ; la bibliothèque C (glibc) arrondit
#    correctement (mesuré sous R 4.3.3 : 0 écart à l'arrondi correct de Python
#    sur 551 008 doubles tirés, et 0 texte relu en un autre double), donc un
#    texte ainsi reproduit est le seul texte de la valeur lue et as.numeric()
#    la lit sans erreur ; un texte lu de travers n'est pas reproduit : il est
#    refusé ;
#  - sous Windows (R 4.3.3 ucrt) : forme de `%.17g` seulement
#    (forme_double_17g()). Ce n'est pas une preuve d'aller-retour mais un
#    constat de mesure : as.numeric() relit au bit près les textes
#    correctement arrondis (0 erreur sur 300 000 tirés et sur les 42 952
#    textes des fichiers de la CI). La forme n'est pas comparée à sprintf :
#    sous Windows, sprintf n'arrondit pas toujours correctement le 17e chiffre
#    (36 694 écarts à l'arrondi correct de Python sur 551 008 doubles tirés,
#    dont 13 aux exposants décimaux de -5 à 5) et son texte ne se relit pas
#    toujours en le même double (5 sur 551 008, exposants au-delà de 266). Un
#    décimal hors de ces textes (ex. 0.001673229520730213) est accepté par la
#    forme et peut être lu de travers : les références sont produites par la
#    CI (Linux), jamais éditées à la main.
# Un dépassement (Inf) ou un sous-dépassement (0) du texte est refusé dans la
# branche Windows ; la branche stricte le refuse par la comparaison. `windows`
# et `formater` (sprintf("%.17g") d'une bibliothèque C simulée) ne servent
# qu'à tester chaque branche sur toute plateforme.
double_lu <- function(s, fichier, chemin, windows = .Platform$OS.type == "windows",
                      formater = function(v) sprintf("%.17g", v)) {
  v <- suppressWarnings(as.numeric(s))
  if (windows) {
    if (!all(forme_double_17g(s)))
      erreur_lecture(fichier, sprintf("%s : valeur hors de la forme de %%.17g (lecture sous Windows : forme seule, voir double_lu())", chemin))
    if (any(!s %in% TEXTES_SPECIAUX_DOUBLE & (!is.finite(v) | (v == 0 & !s %in% c("0", "-0")))))
      erreur_lecture(fichier, sprintf("%s : valeur hors de l'intervalle des doubles", chemin))
  } else if (!identical(formater(v), s)) {
    erreur_lecture(fichier, sprintf("%s : valeur non canonique pour le type double (différente de sprintf(\"%%.17g\") de la valeur lue)", chemin))
  }
  v
}

# Écriture canonique d'un vecteur de Date (ADR 0011, point 4 ; #19) pour le
# contrôle de lecture et les messages : %04d-%02d-%02d sur as.POSIXlt(), jetons
# NA et NaN. Ne passe pas par tablexp:::ecrire_valeurs() : la lecture et la
# comparaison s'exécutent sans installation de tablexp (comparer_references.R) ;
# c'est aussi une seconde écriture, indépendante de celle du moteur.
texte_dates <- function(v) {
  lt <- as.POSIXlt(structure(as.double(unclass(v)), class = "Date"))
  t <- sprintf("%04d-%02d-%02d", lt$year + 1900L, lt$mon + 1L, lt$mday)
  t[is.na(v)] <- "NA"; t[is.nan(unclass(v))] <- "NaN"
  t
}

# Domaine des dates [0001-01-01, 9999-12-31] (ADR 0011, point 4 ; #65), en
# jours depuis 1970-01-01 : mêmes bornes que dates_hors_domaine() du moteur
# (non appelée ici, comme texte_dates()). Hors domaine : refus à la lecture.
JOURS_DATES_REFERENCE <- c(-719162, 2932896)

valeurs_lues <- function(type, s, fichier, chemin) {
  canonique <- function(v, texte) {
    if (!identical(texte, s)) erreur_lecture(fichier, sprintf("%s : valeur non canonique pour le type %s", chemin, type))
    v
  }
  switch(type,
    double = double_lu(s, fichier, chemin),
    integer = { v <- suppressWarnings(as.integer(s)); canonique(v, ifelse(is.na(v), "NA", sprintf("%d", v))) },
    logical = { v <- c("TRUE" = TRUE, "FALSE" = FALSE, "NA" = NA)[s]
                if (anyNA(names(v))) erreur_lecture(fichier, sprintf("%s : logique invalide", chemin)); unname(v) },
    # Date : jetons NA et NaN lus tels quels (NaN en Date stockée NaN) ; forme
    # canonique (année sur quatre chiffres, ADR 0011, point 4 ; #19) contrôlée
    # par la réécriture de la valeur lue, texte_dates().
    Date = { u <- as.double(unclass(as.Date(ifelse(s %in% c("NA", "NaN"), NA_character_, s), format = "%Y-%m-%d")))
             u[s == "NaN"] <- NaN
             if (any(!is.na(u) & (u < JOURS_DATES_REFERENCE[1L] | u > JOURS_DATES_REFERENCE[2L])))
               erreur_lecture(fichier, sprintf("%s : date_hors_domaine : date hors du domaine [0001-01-01, 9999-12-31], sans écriture canonique", chemin))
             v <- structure(u, class = "Date")
             canonique(v, texte_dates(v)) },
    character = , factor = desechapper(s, fichier, chemin),
    erreur_lecture(fichier, sprintf("%s : type %s inconnu", chemin, type)))
}

lire_reference <- function(fichier) {
  o <- readBin(fichier, "raw", file.info(fichier)$size)
  if (!length(o)) erreur_lecture(fichier, "fichier vide")
  if (any(o == as.raw(13L))) erreur_lecture(fichier, "retour chariot (CR) présent")
  if (o[length(o)] != as.raw(10L)) erreur_lecture(fichier, "dernière ligne sans LF")
  texte <- rawToChar(o)
  Encoding(texte) <- "UTF-8"
  if (!validUTF8(texte)) erreur_lecture(fichier, "UTF-8 invalide")
  l <- strsplit(texte, "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  Encoding(l) <- "UTF-8"
  if (!identical(l[1L], paste0("# ", FORMAT_REFERENCE)))
    erreur_lecture(fichier, sprintf("première ligne « %s » au lieu de « # %s »", l[1L], FORMAT_REFERENCE))
  n_entete <- which(!startsWith(l, "#"))[1L] - 1L
  if (is.na(n_entete)) n_entete <- length(l)
  e <- sub("^# ", "", l[seq.int(2L, length.out = n_entete - 1L)])
  entete <- stats::setNames(sub("^.*? : ", "", e, perl = TRUE), sub(" : .*$", "", e, perl = TRUE))
  corps <- l[seq.int(n_entete + 1L, length.out = length(l) - n_entete)]
  champs <- lapply(strsplit(corps, "\t", fixed = TRUE, useBytes = TRUE), function(f) { Encoding(f) <- "UTF-8"; f })
  chemins <- vapply(champs, `[`, "", 1L)
  if (anyDuplicated(chemins)) erreur_lecture(fichier, sprintf("chemin en double : %s", chemins[duplicated(chemins)][1L]))
  types <- stats::setNames(vapply(champs, function(f) if (length(f) >= 2L) f[2L] else NA_character_, ""), chemins)
  valeurs <- stats::setNames(vector("list", length(champs)), chemins)
  for (i in seq_along(champs)) {
    f <- champs[[i]]
    if (length(f) < 3L || !types[[i]] %in% TYPES_REFERENCE)
      erreur_lecture(fichier, sprintf("ligne %d : chemin, type et longueur attendus", n_entete + i))
    n <- suppressWarnings(as.integer(f[3L]))
    if (is.na(n) || !identical(sprintf("%d", n), f[3L]) || length(f) - 3L != n)
      erreur_lecture(fichier, sprintf("%s : longueur %s pour %d valeur(s)", f[1L], f[3L], length(f) - 3L))
    if (types[[i]] == "NULL") {
      if (n != 0L) erreur_lecture(fichier, sprintf("%s : NULL de longueur %d", f[1L], n))
      next
    }
    valeurs[i] <- list(if (n) valeurs_lues(types[[i]], f[-(1:3)], fichier, f[1L]) else
      switch(types[[i]], double = numeric(0), integer = integer(0), logical = logical(0),
             Date = structure(numeric(0), class = "Date"), character(0)))
  }
  list(types = types, valeurs = valeurs, entete = entete)
}

# --- tolérances et comparaison (point 3) --------------------------------------

# Clés de `tolerances` qui classent `chemin`, de la plus longue à la plus
# courte (suffixes du chemin, segment par segment).
cles_de <- function(chemin, tolerances = TOLERANCES) {
  s <- strsplit(chemin, "/", fixed = TRUE)[[1L]]
  suffixes <- vapply(seq_along(s), function(k) paste(s[k:length(s)], collapse = "/"), "")
  suffixes[suffixes %in% names(tolerances)]
}
tolerance_de <- function(chemin, tolerances = TOLERANCES) {
  k <- cles_de(chemin, tolerances)
  if (!length(k)) return(list(cle = NA_character_, tol = NA_real_, autres = character(0)))
  list(cle = k[1L], tol = unname(tolerances[[k[1L]]]), autres = k[-1L])
}

texte_valeur <- function(v, type) {
  if (type == "double") return(sprintf("%.17g", v))
  if (type == "Date") return(texte_dates(v))
  if (type %in% c("character", "factor")) return(ifelse(is.na(v), "NA", sprintf("\"%s\"", encodeString(v))))
  ifelse(is.na(v), "NA", as.character(v))
}

# Paires élément par élément (seconde annotation, (c)) : identiques (au sens
# d'identical(), NA et NaN distingués), sinon jugées par la règle relative.
identiques_elements <- function(a, b) {
  if (is.double(a)) return((!is.na(a) & !is.na(b) & a == b) | (is.na(a) & is.na(b) & is.nan(a) == is.nan(b)))
  (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
}

comparer_references <- function(reference, obtenu, tolerances = TOLERANCES) {
  ch_r <- names(reference$types); ch_o <- names(obtenu$types)
  structure_ <- character(0)
  # Entrée, en premier (point 3.2).
  e_r <- if (CHEMIN_EMPREINTE_DONNEES %in% ch_r) reference$valeurs[[CHEMIN_EMPREINTE_DONNEES]] else NA_character_
  e_o <- if (CHEMIN_EMPREINTE_DONNEES %in% ch_o) obtenu$valeurs[[CHEMIN_EMPREINTE_DONNEES]] else NA_character_
  # Représentation canonique des deux côtés (NA si absente) : troisième cause
  # d'une entrée différente (annotation du 19/10/2026 (troisième), (f)).
  rep_de <- function(g) if (CHEMIN_REPRESENTATION %in% names(g$types) && length(g$valeurs[[CHEMIN_REPRESENTATION]]) == 1L)
    as.character(g$valeurs[[CHEMIN_REPRESENTATION]]) else NA_character_
  entree <- list(reference = e_r, obtenu = e_o, differente = !identical(e_r, e_o),
                 representation_reference = rep_de(reference), representation_obtenue = rep_de(obtenu))
  # Exigée des deux côtés (point 3.2) : son absence n'est jamais une égalité.
  for (cote in c("référence", "obtenu"))
    if (anyNA(if (cote == "obtenu") e_o else e_r))
      structure_ <- c(structure_, sprintf("%s : empreinte des données absente (%s)", CHEMIN_EMPREINTE_DONNEES, cote))
  for (c_ in setdiff(ch_r, ch_o)) structure_ <- c(structure_, sprintf("%s : manquant", c_))
  for (c_ in setdiff(ch_o, ch_r)) structure_ <- c(structure_, sprintf("%s : en trop", c_))
  communs <- ch_r[ch_r %in% ch_o]
  n <- length(communs)
  g <- data.frame(chemin = communs, type = character(n), longueur = integer(n), cle = NA_character_,
                  tolerance = NA_real_, n_differents = integer(n), ecart_relatif_max = NA_real_,
                  position = NA_integer_, avant = NA_character_, apres = NA_character_,
                  verdict = NA_character_, stringsAsFactors = FALSE)
  garde <- rep(TRUE, n)
  cm <- list(chemin = character(0), cle = character(0), classe = character(0), autres = character(0))
  for (k in seq_len(n)) {
    c_ <- communs[k]
    t_r <- reference$types[[c_]]; t_o <- obtenu$types[[c_]]
    a <- reference$valeurs[[c_]]; b <- obtenu$valeurs[[c_]]
    if (!identical(t_r, t_o)) {
      structure_ <- c(structure_, sprintf("%s : type %s au lieu de %s", c_, t_o, t_r)); garde[k] <- FALSE; next
    }
    if (length(a) != length(b)) {
      structure_ <- c(structure_, sprintf("%s : longueur %d au lieu de %d", c_, length(b), length(a)))
      garde[k] <- FALSE; next
    }
    tol <- if (t_r == "double") tolerance_de(c_, tolerances) else list(cle = NA_character_, tol = 0, autres = character(0))
    if (length(tol$autres)) {
      cm$chemin <- c(cm$chemin, c_); cm$cle <- c(cm$cle, tol$cle)
      cm$classe <- c(cm$classe, nom_classe(tol$tol)); cm$autres <- c(cm$autres, paste(tol$autres, collapse = ", "))
    }
    va <- if (t_r == "Date") unclass(a) else a; vb <- if (t_r == "Date") unclass(b) else b
    id <- if (t_r == "NULL") logical(0) else identiques_elements(va, vb)
    if (all(id)) {
      g$type[k] <- t_r; g$longueur[k] <- length(a); g$cle[k] <- tol$cle; g$tolerance[k] <- tol$tol
      g$ecart_relatif_max[k] <- if (t_r == "double") 0 else NA_real_
      g$verdict[k] <- if (t_r == "double" && is.na(tol$tol)) "sans tolérance déclarée" else "identique"
      next
    }
    non_fini <- if (t_r == "double") !is.finite(va) | !is.finite(vb) else is.na(va) | is.na(vb)
    pos_nf <- which(!id & non_fini)
    if (length(pos_nf)) {
      i <- pos_nf[1L]
      structure_ <- c(structure_, sprintf("%s : position des valeurs non finies ou manquantes différente (%d élément(s) ; élément %d : %s au lieu de %s)",
                                          c_, length(pos_nf), i, texte_valeur(b[i], t_r), texte_valeur(a[i], t_r)))
    }
    diff <- which(!id & !non_fini)
    rel <- if (t_r == "double" && length(diff))
      abs(va[diff] - vb[diff]) / pmax(abs(va[diff]), abs(vb[diff])) else numeric(0)
    meme_signe <- if (length(diff) && t_r == "double") sign(va[diff]) == sign(vb[diff]) & va[diff] != 0 & vb[diff] != 0 else logical(0)
    sans_tol <- t_r == "double" && is.na(tol$tol)
    hors <- if (t_r != "double" || sans_tol) rep(TRUE, length(diff)) else !(meme_signe & rel <= tol$tol)
    j <- if (length(rel)) diff[which.max(rel)] else if (length(diff)) diff[1L]
         else if (length(pos_nf)) pos_nf[1L] else NA_integer_
    g$type[k] <- t_r; g$longueur[k] <- length(a); g$cle[k] <- tol$cle; g$tolerance[k] <- tol$tol
    g$n_differents[k] <- length(diff) + length(pos_nf)
    g$ecart_relatif_max[k] <- if (length(rel)) max(rel) else if (t_r == "double") 0 else NA_real_
    g$position[k] <- j
    if (!is.na(j)) { g$avant[k] <- texte_valeur(a[j], t_r); g$apres[k] <- texte_valeur(b[j], t_r) }
    g$verdict[k] <- if (sans_tol) "sans tolérance déclarée"
                    else if (length(pos_nf) || any(hors)) "écart"
                    else if (length(diff)) "sous tolérance" else "identique"
  }
  g <- g[garde, , drop = FALSE]
  rownames(g) <- NULL
  cm <- as.data.frame(cm, stringsAsFactors = FALSE)
  sans <- g$chemin[g$verdict == "sans tolérance déclarée"]
  ok <- !entree$differente && !anyNA(c(e_r, e_o)) && !length(structure_) && all(g$verdict %in% c("identique", "sous tolérance"))
  list(ok = ok, entree = entree, structure = structure_, grandeurs = g, sans_tolerance = sans,
       cles_multiples = cm)
}

# Doubles de `grandeurs` qu'aucune clé ne classe (règle de complétude,
# point 3.4) ; appliquée aussi à la production.
doubles_sans_tolerance <- function(grandeurs, tolerances = TOLERANCES) {
  d <- names(grandeurs$types)[grandeurs$types == "double"]
  d[vapply(d, function(c_) is.na(tolerance_de(c_, tolerances)$tol), NA)]
}

# Impression du point 3.5 ; rend les lignes (character).
imprimer_comparaison <- function(r, titre = "") {
  g <- r$grandeurs
  out <- if (nzchar(titre)) titre else character(0)
  out <- c(out, if (r$entree$differente)
    c(sprintf("  ENTRÉE DIFFÉRENTE : empreinte des données %s (référence) contre %s (obtenu)",
              r$entree$reference, r$entree$obtenu),
      "  entrée différente : écarts non interprétables comme un tableau avant / après ;",
      "  causes possibles : le simulateur a changé pour cette graine (ADR 0009, point 4.2), ou la plateforme a produit un tirage différent (ADR 0008, hypothèse sur la libm)",
      if (!is.na(r$entree$representation_reference) && !is.na(r$entree$representation_obtenue) &&
          !identical(r$entree$representation_reference, r$entree$representation_obtenue))
        sprintf("  ou la représentation canonique a changé (%s : « %s » (référence) contre « %s » (obtenu)) ; une entrée inchangée se prouve par la garde 3.2 de tests/test_reproductibilite.R",
                CHEMIN_REPRESENTATION, r$entree$representation_reference, r$entree$representation_obtenue))
    else sprintf("  Entrée : empreinte des données identique (%s)", r$entree$reference))
  out <- c(out, sprintf("  Bilan : %d grandeur(s) comparée(s) ; %d identique(s) au bit près ; %d à écart non nul sous tolérance ; %d en écart ; %d sans tolérance déclarée ; %d écart(s) structurel(s)",
                        nrow(g), sum(g$verdict == "identique"), sum(g$verdict == "sous tolérance"),
                        sum(g$verdict == "écart"), sum(g$verdict == "sans tolérance déclarée"), length(r$structure)))
  d <- g[g$type == "double" & !is.na(g$tolerance), , drop = FALSE]
  for (k in names(CLASSES_TOLERANCE)) {
    x <- d$ecart_relatif_max[d$tolerance == CLASSES_TOLERANCE[[k]]]
    out <- c(out, sprintf("  Écart relatif maximal, classe %-6s : %.3g (%d grandeur(s) double)", k,
                          if (length(x)) max(x) else 0, length(x)))
  }
  if (length(r$structure)) out <- c(out, paste0("  STRUCTURE : ", r$structure))
  for (c_ in r$sans_tolerance) out <- c(out, sprintf("  grandeur sans tolérance déclarée : %s", c_))
  if (nrow(r$cles_multiples))
    out <- c(out, sprintf("  clé retenue : %s -> %s (classe %s ; aussi : %s)", r$cles_multiples$chemin,
                          r$cles_multiples$cle, r$cles_multiples$classe, r$cles_multiples$autres))
  e <- g[g$n_differents > 0L, , drop = FALSE]
  if (nrow(e))
    out <- c(out, "  Grandeurs à écart non nul (chemin | longueur | éléments différents | écart relatif max | position | avant | après | tolérance | verdict) :",
             sprintf("    %s | %d | %d | %s | %d | %s | %s | %s | %s", e$chemin, e$longueur, e$n_differents,
                     ifelse(is.na(e$ecart_relatif_max), "-", sprintf("%.3g", e$ecart_relatif_max)), e$position,
                     e$avant, e$apres, ifelse(is.na(e$tolerance), "aucune", nom_classe(e$tolerance)), e$verdict))
  c(out, sprintf("  Verdict : %s", if (r$ok) "OK" else "ECHEC"))
}

# Fichiers <id>.tsv de `dossier` sans plan de référence (point 3.7).
references_orphelines <- function(dossier, plans = plans_reference()) {
  f <- list.files(dossier, pattern = "\\.tsv$")
  f[!sub("\\.tsv$", "", f) %in% names(plans)]
}

# Identifiant de plateforme (ADR 0008, point 5) : lignes contrôlées par
# tests/controles/plateforme.R, et lignes consignées sans contrôle.
identifiant_plateforme <- function(racine) {
  env <- new.env()
  charger_utf8(file.path(racine, "tests", "controles", "plateforme.R"), env)
  r <- env$relever_plateforme()
  list(controle = trimws(env$lignes_identifiant(r)), consigne = trimws(env$lignes_consignees()),
       defauts = env$defauts_plateforme(r))
}

# --- règles des modes du workflow (point 5.2) ----------------------------------

# Rend NA si le plan est admis dans ce mode, sinon la raison du refus.
# vise : plan dans --plans ; present : fichier commité ; reussie :
# comparaison au fichier commité réussie (FALSE s'il est absent).
regle_mode <- function(mode, vise, present, reussie) {
  if (mode == "releve") return(NA_character_)
  if (mode == "bascule") return(if (present) NA_character_ else "fichier commité absent")
  if (vise && mode == "creation")
    return(if (!present) NA_character_ else "un fichier commité existe déjà (une création ne modifie pas une référence)")
  if (vise && mode == "regeneration")
    return(if (present) NA_character_ else "fichier commité absent (c'est une création)")
  if (!present) return("fichier commité absent")
  if (!reussie) return("comparaison en écart")
  NA_character_
}

# Bascule : les en-têtes commités portent un même identifiant de plateforme,
# différent de celui de l'exécution. Rend NA si c'est le cas, sinon la raison.
controle_bascule <- function(plateformes_commises, courante) {
  u <- unique(plateformes_commises)
  if (length(u) != 1L || !length(u[[1L]]))
    return("identifiants de plateforme des en-têtes commités absents ou différents entre eux")
  if (identical(u[[1L]], courante))
    return("l'identifiant de plateforme de l'exécution est celui des en-têtes commités (ce n'est pas une bascule)")
  NA_character_
}

# Forme comparable d'un chemin, par le texte seul (aucun accès au système de
# fichiers) : absolu (relatif à `courant`), séparateur « / », sans « . », « .. »
# ni « / » final (`reduire = FALSE` conserve les « .. » : le texte est alors
# résolu par le système avant toute réduction, voir sortie_interdite()).
# `windows` : « \ » est un séparateur, un chemin absolu est `X:/...`,
# `//serveur/partage/...` (UNC, préfixes `\\?\` et `\\?\UNC\` compris) ou
# `/...` (lecteur du répertoire courant), `X:rel` est relatif au répertoire
# courant du lecteur X (ramené à `courant` si c'est le lecteur courant), et la
# casse est ignorée (lettre de lecteur comprise).
forme_chemin <- function(p, courant, windows = .Platform$OS.type == "windows", reduire = TRUE) {
  prefixe <- ""
  if (windows) {
    p <- gsub("\\", "/", p, fixed = TRUE); courant <- gsub("\\", "/", courant, fixed = TRUE)
    p <- sub("^//[?.]/", "", sub("^//[?.]/UNC/", "//", p))
    lecteur <- function(x) if (grepl("^[A-Za-z]:", x)) toupper(substr(x, 1L, 2L)) else ""
    if (grepl("^[A-Za-z]:([^/]|$)", p)) {
      base <- if (identical(lecteur(p), lecteur(courant))) courant else substr(p, 1L, 2L)
      p <- paste0(base, "/", substring(p, 3L))
    } else if (grepl("^/([^/]|$)", p)) {
      p <- paste0(lecteur(courant), p)
    } else if (!grepl("^(//|[A-Za-z]:/)", p)) {
      p <- paste0(courant, "/", p)
    }
    if (startsWith(p, "//")) { prefixe <- "/"; p <- substring(p, 2L) }
    else if (grepl("^[A-Za-z]:", p)) { prefixe <- substr(p, 1L, 2L); p <- substring(p, 3L) }
    p <- tolower(p); prefixe <- tolower(prefixe)
  } else if (!startsWith(p, "/")) p <- paste0(courant, "/", p)
  pile <- character(0)
  for (seg in strsplit(p, "/", fixed = TRUE)[[1L]]) {
    if (!nzchar(seg) || identical(seg, ".")) next
    if (reduire && identical(seg, "..")) pile <- pile[-length(pile)] else pile <- c(pile, seg)
  }
  paste0(prefixe, "/", paste(pile, collapse = "/"))
}

# Le dossier de sortie est-il tests/reference/ ou l'un de ses sous-dossiers ?
# Refusé si l'une des deux formes l'indique : la forme du texte (portable :
# séparateurs, lecteur, casse, UNC) et celle que résout le système de fichiers
# pour la partie existante (liens, noms courts), seulement si `windows` est
# la plateforme de l'hôte et `resoudre` vrai. `courant`, `windows` et
# `resoudre = FALSE` ne servent qu'à tester, par le texte seul et sur
# n'importe quelle plateforme, les formes de chemin d'une autre.
sortie_interdite <- function(sortie, commis, courant = getwd(), windows = .Platform$OS.type == "windows",
                             resoudre = TRUE) {
  dedans <- function(s, c_) identical(s, c_) || startsWith(s, paste0(c_, "/"))
  texte <- function(p) forme_chemin(p, courant, windows)
  # Ancêtre existant du texte BRUT (« .. » non réduits : sous POSIX, `<lien>/..`
  # est le parent de la cible du lien, pas celui du lien), résolu par le système ;
  # le reste du chemin lui est rattaché puis réduit.
  resolu <- function(p) {
    t <- forme_chemin(p, courant, windows, reduire = FALSE); reste <- character(0)
    while (!file.exists(t) && !identical(dirname(t), t)) { reste <- c(basename(t), reste); t <- dirname(t) }
    forme_chemin(file.path(normalizePath(t, winslash = "/", mustWork = FALSE), paste(reste, collapse = "/")), courant, windows)
  }
  dedans(texte(sortie), texte(commis)) ||
    (resoudre && identical(windows, .Platform$OS.type == "windows") && dedans(resolu(sortie), resolu(commis)))
}

# En-tête d'une référence (point 2.2), construit ici pour que ses clés non
# ASCII soient des littéraux marqués UTF-8.
entete_reference <- function(p, plateforme, x, ci, mode, date_utc) {
  c(identifiant = p$identifiant, description = p$description,
    stats::setNames(plateforme$controle, rep("plateforme", length(plateforme$controle))),
    stats::setNames(plateforme$consigne, rep("plateforme consignée", length(plateforme$consigne))),
    stats::setNames(unlist(x$fiche$versions), paste0("fiche$versions/", names(x$fiche$versions))),
    stats::setNames(unlist(x$fiche$plateforme), paste0("fiche$plateforme/", names(x$fiche$plateforme))),
    # Valeurs réelles des deux empreintes comparées par jeton, hors
    # comparaison (ADR 0009, annotation du 20/10/2026, seconde, (d) ; #67).
    stats::setNames(vapply(c("resultat", "code"), function(n) {
      v <- x$fiche$empreintes[[n]]
      if (is.character(v) && length(v) == 1L && !is.na(v)) v else "NA"
    }, ""), paste0("fiche$empreintes/", c("resultat", "code"))),
    commit = ci$commit, execution = ci$execution, adresse = ci$adresse, mode = mode,
    date = date_utc,
    # Clé en chaîne, jamais en étiquette de c() : une étiquette est un symbole,
    # traduit en natif par parse() (« donn<U+00E9>es » en locale C, mesuré).
    stats::setNames(x$fiche$empreintes$donnees, "empreinte des données"))
}

# Lignes de texte en octets UTF-8, une par ligne, LF final (ecarts.txt).
octets_texte <- function(lignes) unlist(lapply(lignes, function(l) c(charToRaw(l), as.raw(10L))), use.names = FALSE)
