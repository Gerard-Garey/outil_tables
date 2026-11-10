# -----------------------------------------------------------------------------
#  R/donnees.R : module `donnees` (issue #9 ; E2.2, E2.4, E2.7 ; ADR 0003,
#  points 2, 5 et 7 ; ADR 0007, points 4, 6.4, 6.5, 7 et 10 ; spécification
#  d'actuary, A.0 et A.1)
#
#  - CATALOGUE_ROLES et catalogue_roles() : catalogue fermé des rôles de la v1
#    (exporté : l'application le lit pour la déclaration des rôles) ;
#  - CATALOGUE_RECETTES, MATRICE_RECETTES : catalogue des recettes
#    d'exposition et matrice recette x étape x estimateur (ADR 0016, points 1
#    et 5), seul endroit où s'écrit la connaissance « recette » ;
#  - deduire_recette() : recette déduite des rôles (ADR 0007, point 5.3 ;
#    ADR 0016, point 2) ; controler_matrice_recette() : étapes et estimateurs
#    du plan admis sous la recette retenue (ADR 0016, point 5) ;
#  - controler_roles() : refus au niveau des rôles (liste fermée) ;
#  - regles_conditionnelles_recette() : règles de la matrice qui ne tiennent
#    pas dans une ligne (agrégats : par_annee_sans_annee,
#    binomial_annee_dernier_anniversaire ; ADR 0016, point 5.2) ;
#  - evaluer_motifs() : motifs de ligne et motifs de tête (liste fermée
#    MOTIFS_LIGNES, MOTIFS_TETE), évaluation exhaustive, sans court-circuit ;
#    lignes d'évaluation des motifs de tête désignables (#69) ;
#  - construire_observations() : unité d'observation, observations et
#    fusion par tête sur des lignes retenues (#69) ;
#  - evaluer_motifs_agregats(), observations_agregats() : motifs de ligne
#    (MOTIFS_LIGNES_AGREGATS) et observations de la recette `agregats`, une
#    observation par cellule reçue, unité non déterminée (#169) ; libellé de
#    chaque code de motif : LIBELLES_MOTIFS ;
#  - empiler_stocks() : table des inventaires et table des événements
#    empilées en une table de la recette `stocks_successifs`, structure seule
#    (exportée : couche de lecture et application ; ADR 0014, point 11 ;
#    #181) ;
#  - preparer_donnees() : fonction d'étape `donnees` — refus par défaut,
#    exclusions déclarées (comptées, listées, lignes conservées inchangées),
#    unité d'observation et fusion par tête, relevés non bloquants,
#    avertissements inscrits dans le résultat ;
#  - table_reference() : représentation commune d'une table de référence et
#    son contrôle, lignes dans l'ordre canonique (exportée ; ADR 0015,
#    point 2.1) ;
#  - TABLES_EMBARQUEES, table_embarquee() : catalogue des tables embarquées
#    (vide) et son accesseur (exporté ; ADR 0015, point 6).
#
#  Conventions de temps (A.0) : une date est un jour ; un intervalle
#  d'observation est [entrée, sortie) ; une ligne `en_cours` est ouverte à
#  droite (+Inf). Âge au dernier anniversaire : age_revolu().
#  Aucune donnée n'est modifiée : les lignes conservées sont celles de
#  l'entrée ; les quantités dérivées (observations fusionnées) sont à part.
#  Ce module n'appelle que `outils` (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# --- catalogue fermé des rôles (A.1) ------------------------------------------

# type : code contrôlé par type_conforme(). L'emploi d'un rôle par chaque
# recette (requis, un parmi, facultatif) est porté par CATALOGUE_RECETTES ;
# un rôle qu'aucune recette n'emploie est refusé (role_non_pris_en_charge :
# son omission changerait le sens du résultat ; ADR 0016, point 3).
CATALOGUE_ROLES <- data.frame(
  role = c("id_contrat", "id_tete", "sexe", "date_naissance", "annee_naissance",
           "age_fourni", "date_age_fourni", "date_entree", "date_sortie",
           "cause_sortie", "date_evenement", "type_ligne", "date_inventaire", "age", "annee",
           "exposition_centrale", "exposition_initiale", "evenements", "montant",
           "segment", "etat_depart", "etat_arrivee", "date_changement"),
  famille = c("identification", "identification", "etat_civil", "etat_civil",
              "etat_civil", "etat_civil", "etat_civil", "observation",
              "observation", "observation", "observation", "observation", "observation",
              "agregats", "agregats", "agregats", "agregats", "agregats",
              "ponderation", "segmentation", "multi_etats", "multi_etats",
              "multi_etats"),
  type = c("identifiant", "identifiant", "categorie", "date", "entier",
           "entier_positif", "date", "date", "date", "categorie", "date", "categorie", "date",
           "numerique", "numerique", "numerique", "numerique", "numerique",
           "reel_strictement_positif", "categorie_ou_entier", "categorie",
           "categorie", "date"),
  stringsAsFactors = FALSE
)

# Libellé des types R admis, par code de type (restitué par catalogue_roles()).
# `numerique` : classe seule (ADR 0016, point 6.5 ; #169, Q5) ; les défauts de
# domaine (manquant, non entier, négatif, non fini) sont des motifs de ligne.
TYPES_ADMIS <- c(
  identifiant = "character, integer, double entier fini",
  categorie = "character, factor",
  categorie_ou_entier = "character, factor, integer",
  date = "Date (valeurs entières finies)",
  entier = "integer, double entier fini",
  entier_positif = "integer, double entier fini >= 0",
  reel_strictement_positif = "double fini > 0",
  numerique = "integer, double"
)

# Modalités des rôles catégoriels (A.1), comparées après as.character,
# sensibles à la casse.
MODALITES_SEXE <- c("H", "F")
MODALITES_CAUSE <- c("deces", "rachat", "terme", "autre", "en_cours")
# Modalités du rôle type_ligne de la recette `stocks_successifs` (spécification
# d'actuary-approfondi de #180, § 3, M328) : ligne d'inventaire (présence de
# l'unité à la date d'inventaire) ou ligne d'événement (sortie de l'unité).
MODALITES_TYPE_LIGNE <- c("inventaire", "evenement")
ROLES_SEGMENTATION <- c("sexe", "segment")

# Âge (au dernier anniversaire) à partir duquel une ligne est aberrante
# (motif age_aberrant). Constante de catalogue, choix de conception (A.1) :
# la borne laisse passer toute durée de vie humaine connue ; ce n'est pas un
# paramètre du plan.
AGE_ABERRANT <- 130L

# Dates hors du domaine [0001-01-01, 9999-12-31] (ADR 0011, point 4) : TRUE pour
# chaque jour (depuis 1970-01-01) hors de [-719162, 2932896], infinis compris ;
# un manquant (NA, NaN) n'est pas hors domaine. Les deux bornes sont les jours
# où composantes_date() rend l'année 1 puis 9999 (0 et 10000 aux jours voisins :
# test_donnees.R). Hors de ce domaine, aucune écriture ISO 8601 à quatre chiffres
# n'existe : refus date_hors_domaine, avant toute empreinte (données et bornes
# du plan) ; l'écrivain du module `rapport` s'en sert aussi, en garde-fou.
dates_hors_domaine <- function(j) {
  j <- as.numeric(j)
  !is.na(j) & (j < -719162 | j > 2932896)
}

# Transitions prises en charge (ADR 0007, point 5.1 : deux noms d'états) et
# porteur de l'état de départ : la tête (vivant -> décédé) ou le contrat (en
# cours -> racheté, unité contrat, M46).
TRANSITIONS <- data.frame(
  depart = c("vivant", "en_cours"),
  arrivee = c("deces", "rachat"),
  code = c("vivant_decede", "en_cours_rachete"),
  etat_de = c("tete", "contrat"),
  stringsAsFactors = FALSE
)

# Motifs de ligne et motifs de tête : liste fermée (A.1), dans l'ordre de
# restitution. Les codes servent aussi de règles d'exclusion du plan.
MOTIFS_LIGNES <- c("id_contrat_manquant", "id_tete_manquant", "naissance_manquante",
                   "entree_manquante", "sortie_manquante", "cause_manquante",
                   "cause_hors_modalites", "sexe_manquant", "sexe_hors_modalites",
                   "segment_manquant", "naissance_apres_entree",
                   "sortie_avant_entree", "en_cours_avant_fin",
                   "annee_naissance_incoherente", "age_aberrant", "contrat_duplique")
MOTIFS_TETE <- c("tete_naissance_incoherente", "tete_sexe_incoherent",
                 "tete_segment_incoherent", "tete_observee_apres_deces")
# Motifs de ligne de la recette `agregats` (une ligne est une cellule ;
# spécification d'actuary de #169, § 6.2, M327, Q5), dans l'ordre de
# restitution : défauts de valeur (manquant, non entier ou non fini, négatif)
# des rôles d'âge, d'année, d'exposition (chaque rôle d'exposition déclaré)
# et d'événements, année hors de la période, segmentation, événements sans
# exposition, cellule dupliquée. Aucun motif d'unité. Les codes communs avec
# MOTIFS_LIGNES (sexe_manquant, sexe_hors_modalites, segment_manquant,
# age_aberrant) ont le même sens (LIBELLES_MOTIFS).
MOTIFS_LIGNES_AGREGATS <- c("age_manquant", "age_non_entier", "age_negatif", "age_aberrant",
                            "annee_manquante", "annee_non_entiere", "annee_hors_periode",
                            "sexe_manquant", "sexe_hors_modalites", "segment_manquant",
                            "exposition_manquante", "exposition_non_finie", "exposition_negative",
                            "evenements_manquants", "evenements_non_entiers", "evenements_negatifs",
                            "evenements_sans_exposition", "cellule_dupliquee")
# Motifs de ligne et motifs d'unité de la recette `stocks_successifs`
# (spécification d'actuary-approfondi de #180, § 7.2 et § 7.3, M328), dans
# l'ordre de restitution : motifs de toute ligne, puis de la ligne
# d'inventaire, puis de la ligne d'événement ; motifs d'unité évalués sur les
# unités exemptes de motif de ligne (evaluer_motifs_stocks() ; amendement A1
# de la spécification, M339). Les codes communs avec MOTIFS_LIGNES ont le
# même sens (LIBELLES_MOTIFS).
MOTIFS_LIGNES_STOCKS <- c("type_ligne_manquant", "type_ligne_hors_modalites", "id_contrat_manquant",
                          "id_tete_manquant", "naissance_manquante", "sexe_manquant", "sexe_hors_modalites",
                          "segment_manquant", "annee_naissance_incoherente", "age_aberrant",
                          "inventaire_sans_date", "inventaire_hors_grille", "inventaire_avec_sortie",
                          "naissance_apres_inventaire", "inventaire_duplique",
                          "sortie_manquante", "cause_manquante", "cause_hors_modalites", "cause_en_cours",
                          "evenement_avec_date_inventaire", "naissance_apres_sortie")
MOTIFS_UNITE_STOCKS <- c("unite_naissance_incoherente", "unite_sexe_incoherent", "unite_segment_incoherent",
                         "evenements_multiples", "presence_apres_evenement", "presence_interrompue",
                         "sortie_non_renseignee", "evenement_apres_absence")
# Libellé de chaque code de motif de l'union des recettes (motifs_recettes()) :
# un code a un seul sens, quelle que soit la recette qui le lève (invariant
# « un libellé unique par code », avis d'actuary du 08/11/2026 sur #169,
# point 2 ; contrôlé par tests/unitaires/test_catalogue_recettes.R).
LIBELLES_MOTIFS <- c(
  id_contrat_manquant = "identifiant de contrat manquant",
  id_tete_manquant = "identifiant de tête manquant",
  naissance_manquante = "date de naissance manquante",
  entree_manquante = "date d'entrée manquante",
  sortie_manquante = "date de sortie manquante, cause de sortie renseignée autre que « en_cours »",
  cause_manquante = "cause de sortie manquante",
  cause_hors_modalites = "cause de sortie hors des modalités",
  sexe_manquant = "sexe manquant (rôle sexe déclaré)",
  sexe_hors_modalites = "sexe hors des modalités H, F (rôle sexe déclaré)",
  segment_manquant = "segment manquant (rôle segment déclaré)",
  naissance_apres_entree = "date de naissance postérieure à la date d'entrée",
  sortie_avant_entree = "date de sortie antérieure à la date d'entrée",
  en_cours_avant_fin = "ligne « en_cours » dont la date de sortie précède la fin de la période",
  annee_naissance_incoherente = "année de naissance différente de l'année de la date de naissance",
  age_aberrant = sprintf("âge d'au moins %d ans", AGE_ABERRANT),
  contrat_duplique = "identifiant de contrat présent sur plusieurs lignes",
  tete_naissance_incoherente = "dates de naissance différentes au sein d'une même tête",
  tete_sexe_incoherent = "sexes différents au sein d'une même tête",
  tete_segment_incoherent = "segments différents au sein d'une même tête (segment employé par la segmentation)",
  tete_observee_apres_deces = "tête observée après son premier décès, survenu avant la fin effective de la période",
  age_manquant = "âge manquant",
  age_non_entier = "âge non entier ou non fini",
  age_negatif = "âge négatif",
  annee_manquante = "année manquante",
  annee_non_entiere = "année non entière ou non finie",
  annee_hors_periode = "année hors des années civiles de la période, du début à la fin effective",
  exposition_manquante = "exposition déclarée manquante",
  exposition_non_finie = "exposition déclarée infinie",
  exposition_negative = "exposition déclarée négative",
  evenements_manquants = "nombre d'événements manquant",
  evenements_non_entiers = "nombre d'événements non entier ou non fini",
  evenements_negatifs = "nombre d'événements négatif",
  evenements_sans_exposition = "événements sans aucune exposition déclarée strictement positive",
  cellule_dupliquee = "clé de cellule (sexe, segment, âge, année déclarés) présente sur plusieurs lignes",
  type_ligne_manquant = "type de ligne manquant",
  type_ligne_hors_modalites = "type de ligne hors des modalités inventaire, evenement",
  inventaire_sans_date = "ligne d'inventaire sans date d'inventaire",
  inventaire_hors_grille = "date d'inventaire hors de la grille des inventaires du plan",
  inventaire_avec_sortie = "ligne d'inventaire qui porte une date ou une cause de sortie",
  naissance_apres_inventaire = "date de naissance postérieure à la date d'inventaire",
  inventaire_duplique = "même unité inventoriée sur plusieurs lignes à la même date",
  cause_en_cours = "ligne d'événement de cause « en_cours »",
  evenement_avec_date_inventaire = "ligne d'événement qui porte une date d'inventaire",
  naissance_apres_sortie = "date de naissance postérieure à la date de sortie",
  unite_naissance_incoherente = "dates de naissance différentes au sein d'une même unité",
  unite_sexe_incoherent = "sexes différents au sein d'une même unité",
  unite_segment_incoherent = "segments différents au sein d'une même unité (segment employé par la segmentation)",
  evenements_multiples = "unité qui porte plusieurs lignes d'événement",
  presence_apres_evenement = "unité inventoriée à une date égale ou postérieure à une de ses dates de sortie",
  presence_interrompue = "unité absente d'un inventaire situé entre deux inventaires où elle est présente",
  sortie_non_renseignee = paste("unité absente de l'inventaire qui suit sa dernière présence, sans ligne",
                                "d'événement"),
  evenement_apres_absence = paste("date de sortie postérieure à l'inventaire qui suit la dernière présence de",
                                  "l'unité"))

# Nombre de numéros de ligne cités dans un message de refus (A.1) ; l'objet
# de refus porte la liste complète.
NB_LIGNES_CITEES <- 10L

# --- catalogue des recettes (ADR 0016, point 1 ; ADR 0014, point 5) -----------

# Une ligne par recette d'exposition, colonnes-listes (précédent :
# ETAPES$prealable) :
#   recette           : nom de la recette ;
#   requis            : rôles requis ;
#   un_parmi          : groupes de rôles dont au moins un est requis (liste de
#                       vecteurs ; vide si aucun) ;
#   facultatifs       : rôles employés sans être requis (dont ceux de la
#                       segmentation, requis par la règle commune
#                       ROLES_SEGMENTATION si le plan les emploie) ;
#   discriminants     : rôles propres à la recette ; ceux de deux recettes
#                       déclarés ensemble sont un refus (recettes_concurrentes),
#                       jamais un arbitrage ;
#   convention_native : convention d'âge la plus fine que les rôles
#                       permettent (ORDRE_CONVENTIONS, module exposition), ou
#                       NA si la convention ne se déduit pas des rôles ;
#   seam              : forme rendue au noyau, "fenetres" ou "cellules"
#                       (ADR 0016, point 4) ;
#   motifs_ligne, motifs_unite : motifs de ligne et d'unité de la recette
#                       (constantes MOTIFS_*), codes des règles d'exclusion.
# Lignes `lignes_dates_completes` (A.3), `agregats` (#169, § 1 : âge,
# événements et au moins une des deux expositions requis ; année, sexe et
# segment facultatifs ; convention déclarée, sans native, Q2) et
# `stocks_successifs` (#180, § 3 : type de ligne, date d'inventaire,
# naissance, date et cause de sortie requises, un identifiant parmi id_tete
# et id_contrat — exactement un : identifiants_concurrents,
# regles_conditionnelles_recette() — ; année de naissance, sexe et segment
# facultatifs ; convention native dernier_anniversaire, § 9 ; seam
# « fenêtres », ADR 0016, point 4.2) ; ajouter une recette ajoute une ligne
# ici et ses lignes dans MATRICE_RECETTES.
CATALOGUE_RECETTES <- data.frame(
  recette = c("lignes_dates_completes", "agregats", "stocks_successifs"),
  requis = I(list(c("date_naissance", "date_entree", "date_sortie", "cause_sortie"), c("age", "evenements"),
                  c("type_ligne", "date_inventaire", "date_naissance", "date_sortie", "cause_sortie"))),
  un_parmi = I(list(list(), list(c("exposition_centrale", "exposition_initiale")), list(c("id_tete", "id_contrat")))),
  facultatifs = I(list(c("id_contrat", "id_tete", "sexe", "annee_naissance", "segment"),
                       c("annee", "sexe", "segment"), c("annee_naissance", "sexe", "segment"))),
  discriminants = I(list("date_entree",
                         c("age", "annee", "exposition_centrale", "exposition_initiale", "evenements"),
                         c("type_ligne", "date_inventaire"))),
  convention_native = c("dernier_anniversaire", NA_character_, "dernier_anniversaire"),
  seam = c("fenetres", "cellules", "fenetres"),
  motifs_ligne = I(list(MOTIFS_LIGNES, MOTIFS_LIGNES_AGREGATS, MOTIFS_LIGNES_STOCKS)),
  motifs_unite = I(list(MOTIFS_TETE, character(0), MOTIFS_UNITE_STOCKS)),
  stringsAsFactors = FALSE
)

# Matrice recette x étape x estimateur (ADR 0016, point 5), format long et
# complet : une ligne par couple (recette, étape de ETAPES, estimateur NA) et
# par triplet (recette, "brut", estimateur de ESTIMATEURS_BRUT) ; complétude
# contrôlée par tests/unitaires/test_catalogue_recettes.R (ETAPES et
# ESTIMATEURS_BRUT ne sont pas lisibles depuis ce module, ADR 0007, point 2.2).
#   admis       : "oui", "non" ou "si_role" (admis si `role_requis` est
#                 déclaré) ;
#   role_requis : rôle qui conditionne "si_role" (NA sinon) ;
#   motif       : motif du refus quand la ligne n'admet pas (NA sinon).
# Sous `lignes_dates_completes`, tout est admis : les refus de Kaplan-Meier et
# du binomial sous par_annee ne dépendent pas de la recette et restent dans
# controler_parametres_brut() (ADR 0016, point 5.7). Sous `agregats` (#169,
# § 4 et § 7) : poisson avec exposition_centrale, binomial avec
# exposition_initiale, Kaplan-Meier refusé (non identifiable sur des
# agrégats, § 4.3) ; positionnement, homogeneite et validation avec
# exposition_centrale (elles lisent E^c) ; importance_exclusions refusée (Q6).
# Sous `stocks_successifs` (#180, § 8) : tout est admis (les trois
# estimateurs reçoivent les mêmes fenêtres), sauf importance_exclusions,
# dont le contrefactuel fusionne des lignes par tête et n'existe pas pour les
# stocks (QS4, #192).
MATRICE_RECETTES <- data.frame(
  recette = rep(c("lignes_dates_completes", "agregats", "stocks_successifs"), each = 10L),
  etape = c("donnees", "exposition", "brut", "brut", "brut", "brut", "positionnement", "homogeneite",
            "validation", "importance_exclusions"),
  estimateur = c(NA, NA, NA, "poisson", "binomial", "kaplan_meier", NA, NA, NA, NA),
  admis = c(rep("oui", 10L),
            "oui", "oui", "oui", "si_role", "si_role", "non", "si_role", "si_role", "si_role", "non",
            rep("oui", 9L), "non"),
  role_requis = c(rep(NA_character_, 10L),
                  NA, NA, NA, "exposition_centrale", "exposition_initiale", NA, "exposition_centrale",
                  "exposition_centrale", "exposition_centrale", NA,
                  rep(NA_character_, 10L)),
  motif = c(rep(NA_character_, 10L),
            NA, NA, NA, "poisson_sans_exposition_centrale", "binomial_sans_exposition_initiale",
            "kaplan_meier_agregats", "etape_exige_exposition_centrale", "etape_exige_exposition_centrale",
            "etape_exige_exposition_centrale", "importance_exclusions_agregats",
            rep(NA_character_, 9L), "importance_exclusions_stocks"),
  stringsAsFactors = FALSE
)

# Rôles employés par la recette de rang k du catalogue `catalogue` : requis,
# un parmi, facultatifs, dans cet ordre, sans doublon.
roles_recette <- function(catalogue, k)
  unique(c(catalogue$requis[[k]], unlist(catalogue$un_parmi[[k]], use.names = FALSE),
           catalogue$facultatifs[[k]]))

# Recettes du catalogue `catalogue` qui emploient chaque rôle de `r` : liste
# parallèle à `r` de noms de recettes, dans l'ordre du catalogue.
recettes_du_role <- function(r, catalogue) {
  employes <- lapply(seq_len(nrow(catalogue)), function(k) roles_recette(catalogue, k))
  lapply(r, function(x) catalogue$recette[vapply(employes, function(e) x %in% e, logical(1L))])
}

# Codes de motif de toutes les recettes (union), motifs de chaque recette dans
# l'ordre du catalogue, ligne puis unité : référence de exclusion_inconnue
# (valider_plan(), qui ne connaît pas les rôles) et ordre canonique des
# exclusions (ordres_canoniques(), module construire_table ; ADR 0016,
# point 6.4). Sous la seule recette des lignes : c(MOTIFS_LIGNES, MOTIFS_TETE).
motifs_recettes <- function()
  unique(unlist(lapply(seq_len(nrow(CATALOGUE_RECETTES)), function(k)
    c(CATALOGUE_RECETTES$motifs_ligne[[k]], CATALOGUE_RECETTES$motifs_unite[[k]])), use.names = FALSE))

# Catalogue des rôles restitué (exporté) : role, famille, type_admis et
# recettes (recettes qui emploient le rôle, requis, un parmi ou facultatif,
# dans l'ordre de CATALOGUE_RECETTES, jointes par « , » ; "" si aucune ;
# ADR 0016, point 3.1, M330).
catalogue_roles <- function() {
  data.frame(role = CATALOGUE_ROLES$role, famille = CATALOGUE_ROLES$famille,
             type_admis = unname(TYPES_ADMIS[CATALOGUE_ROLES$type]),
             recettes = vapply(recettes_du_role(CATALOGUE_ROLES$role, CATALOGUE_RECETTES), paste,
                               character(1L), collapse = ", "),
             stringsAsFactors = FALSE)
}

# --- outils de dates ----------------------------------------------------------

# Composantes (année, mois 0-11, jour) de jours depuis 1970-01-01.
composantes_date <- function(j) {
  lt <- as.POSIXlt(structure(as.numeric(j), class = "Date"))
  list(an = lt$year + 1900L, mois = lt$mon, jour = lt$mday)
}

# Âge au dernier anniversaire (A.0, T2) :
# an(t) - an(n) - 1{(mois(t), jour(t)) < (mois(n), jour(n))}, sans cas
# particulier : une naissance au 29/02 a son anniversaire le 01/03 des années
# non bissextiles. n, t : jours depuis 1970-01-01 (ou Date) ; NA si l'un
# des deux l'est.
age_revolu <- function(n, t) {
  cn <- composantes_date(n); ct <- composantes_date(t)
  (ct$an - cn$an) - ((ct$mois * 100L + ct$jour) < (cn$mois * 100L + cn$jour))
}

# --- contrôle des types --------------------------------------------------------

entiers_finis <- function(v) {
  v <- v[!is.na(v)]
  all(is.finite(v) & v == trunc(v))
}

# TRUE si la colonne x est du type `type` du catalogue. Une colonne-liste,
# une matrice ou une classe autre que celles admises (POSIXct, difftime...)
# est refusée. Dimension lue par attr() et facteur par a_classe() (R/outils.R,
# #128) : dim() et is.factor() dispatchent sur la classe et lèvent une erreur R
# sur une classe d'encodage « bytes » (mesuré) ; la colonne est alors refusée
# comme toute classe non admise.
type_conforme <- function(x, type) {
  if (!is.atomic(x) || !is.null(attr(x, "dim", exact = TRUE))) return(FALSE)
  nu <- !is.object(x)
  chaine <- nu && is.character(x)
  entier <- nu && is.integer(x)
  double_entier <- nu && is.double(x) && entiers_finis(x)
  switch(type,
    identifiant = chaine || entier || double_entier,
    categorie = chaine || a_classe(x, "factor"),
    categorie_ou_entier = chaine || a_classe(x, "factor") || entier,
    date = identical(class(x), "Date") && is.numeric(unclass(x)) &&
      entiers_finis(unclass(x)),
    entier = entier || double_entier,
    entier_positif = (entier || double_entier) && all(x[!is.na(x)] >= 0),
    reel_strictement_positif = nu && is.double(x) && all(is.finite(x[!is.na(x)])) &&
      all(x[!is.na(x)] > 0),
    numerique = nu && (is.integer(x) || is.double(x)),
    stop(sprintf("type_conforme() : type de catalogue inconnu « %s »", type), call. = FALSE))
}

# --- refus au niveau des rôles (A.1) ------------------------------------------

# Chaînes de l'utilisateur citées dans un message (#111, #94) : règle unique
# neutraliser_citation() (R/outils.R) : chaîne « bytes » citée sous un
# repère (sprintf() lèverait « translating strings with "bytes" encoding is
# not allowed » au lieu du refus), caractères de contrôle et séparateurs de
# ligne écrits <U+XXXX>, octets qui ne sont pas de l'UTF-8 valide écrits
# <xx>, marque UTF-8. Rend le vecteur neutralisé, sans paste ; NA reste NA.
citer <- function(x) neutraliser_citation(x)

# Valeurs citées entre guillemets, jointes par « , » : texte formé, argument
# admis de formuler() sans nouvelle citation (ADR 0007, annotation du
# 05/11/2026, (c) 3 ; #127). Les guillemets sont marqués par texte_moteur()
# avant le collage : sous la locale C, paste0() traduirait des guillemets non
# marqués (paquet installé sous C) mêlés à une citation marquée UTF-8 et en
# écrirait les octets « <c2><ab> » (mesuré, même annotation, (b)).
liste_guillemets <- function(x)
  paste0(texte_moteur("« "), citer(x), texte_moteur(" »"), collapse = ", ")

# Message du refus `aucune_recette` (texte visé par actuary, #169, avis du
# 08/11/2026) : pour chaque recette listée, ses rôles requis absents puis,
# pour chaque groupe `un_parmi` non couvert, « un rôle parmi ... ».
#   recettes : noms des recettes listées ;
#   requis   : liste parallèle des rôles requis absents ;
#   groupes  : liste parallèle des groupes un_parmi non couverts.
message_aucune_recette <- function(recettes, requis, groupes) {
  manquants <- vapply(seq_along(recettes), function(k)
    paste(c(if (length(requis[[k]])) liste_guillemets(requis[[k]]) else character(0),
            vapply(groupes[[k]], function(g) formuler("un rôle parmi %s", liste_guillemets(g)), character(1L))),
          collapse = ", "), character(1L))
  formuler("Aucune recette d'exposition ne s'applique ; rôles requis absents, recette par recette : %s.",
           paste(formuler("recette « %s » : %s", citer(recettes), manquants), collapse = " ; "))
}

# Refus `recettes_concurrentes` (ADR 0016, point 2.2, refus 5) : des rôles
# discriminants de plusieurs recettes du catalogue `catalogue` sont déclarés
# ensemble ; la recette n'est pas choisie à la place de l'utilisateur. Ne lit
# que les noms des rôles. Rend NULL, invisible.
controler_recettes_concurrentes <- function(roles, catalogue) {
  r <- names(roles)
  propres <- lapply(catalogue$discriminants, function(d) r[r %in% d])
  k <- which(lengths(propres) > 0L)
  if (length(k) > 1L)
    refuser("donnees", "recettes_concurrentes",
            formuler(paste("Les rôles déclarés relèvent de plusieurs recettes d'exposition (rôles propres à chacune : %s) ;",
                           "la recette n'est pas choisie à la place de l'utilisateur : retirer les rôles de la recette qui ne",
                           "s'applique pas, la construction est refusée."),
                     paste(formuler("recette « %s » : %s (colonnes %s)", citer(catalogue$recette[k]),
                                    vapply(propres[k], liste_guillemets, character(1L)),
                                    vapply(propres[k], function(x) liste_guillemets(roles[x]), character(1L))),
                           collapse = " ; ")),
            list(recette = catalogue$recette[k], role = unlist(propres[k], use.names = FALSE),
                 colonne = unname(roles[unlist(propres[k], use.names = FALSE)])))
  invisible(NULL)
}

# Recette déduite des rôles (ADR 0007, point 5.3 ; ADR 0016, point 2.1),
# seul endroit où s'écrit la sélection : la recette du catalogue `catalogue`
# dont tous les rôles requis sont déclarés et dont chaque groupe `un_parmi` a
# au moins un rôle déclaré ; des discriminants de deux recettes déclarés
# ensemble sont un refus (recettes_concurrentes) ; aucune recette complète :
# refus aucune_recette, qui liste la recette dont des rôles propres sont
# déclarés, sinon toutes, dans l'ordre du catalogue. Arguments `catalogue` et
# `matrice` par défaut les constantes du package : injection en test (M247 ;
# #196) ; la matrice injectée doit porter les lignes de la recette d'un
# catalogue injecté, sans quoi la matrice de la recette rendue est vide. Rend
# la valeur validée que construire_table() transmet aux étapes (#82) :
# list(recette, convention_native, roles_requis, roles_employes, seam, motifs
# (ligne puis unité), matrice (lignes de `matrice` de la recette, sans la
# colonne recette)) ; les étapes lisent ses propriétés, non son nom (ADR 0016,
# point 4.4).
deduire_recette <- function(roles, catalogue = CATALOGUE_RECETTES, matrice = MATRICE_RECETTES) {
  controler_recettes_concurrentes(roles, catalogue)
  r <- names(roles)
  requis <- lapply(catalogue$requis, function(q) setdiff(q, r))
  groupes <- lapply(catalogue$un_parmi, function(u) Filter(function(g) !any(g %in% r), u))
  complete <- which(lengths(requis) == 0L & lengths(groupes) == 0L)
  if (!length(complete)) {
    propres <- which(vapply(catalogue$discriminants, function(d) any(d %in% r), logical(1L)))
    k <- if (length(propres) == 1L) propres else seq_len(nrow(catalogue))
    refuser("donnees", "aucune_recette", message_aucune_recette(catalogue$recette[k], requis[k], groupes[k]),
            list(recette = catalogue$recette[k], role = stats::setNames(requis[k], catalogue$recette[k]),
                 un_parmi = stats::setNames(groupes[k], catalogue$recette[k])))
  }
  if (length(complete) > 1L)
    stop(sprintf("deduire_recette() : recettes %s complètes sans discriminant concurrent (catalogue mal formé).",
                 paste(catalogue$recette[complete], collapse = ", ")), call. = FALSE)
  k <- complete
  m <- matrice[matrice$recette == catalogue$recette[k],
               c("etape", "estimateur", "admis", "role_requis", "motif"), drop = FALSE]
  rownames(m) <- NULL
  list(recette = catalogue$recette[k], convention_native = catalogue$convention_native[k],
       roles_requis = catalogue$requis[[k]], roles_employes = roles_recette(catalogue, k),
       seam = catalogue$seam[k], motifs = c(catalogue$motifs_ligne[[k]], catalogue$motifs_unite[[k]]),
       matrice = m)
}

# Contrôle `roles` (vecteur de chaînes nommé rôle -> colonne, ADR 0007,
# point 4.1) contre le catalogue et `donnees`. Forme exacte (#62,
# forme_exacte()) : type character, seul attribut names (ni classe, ni dim,
# ni attribut quelconque), sans nom ni valeur manquants. Ordre des contrôles
# (ADR 0016, point 2.2) : (1) forme ; (2) rôle inconnu ; (3) rôle déclaré
# deux fois ; (4) rôle qu'aucune recette du catalogue n'emploie
# (role_non_pris_en_charge) ; (5) discriminants de plusieurs recettes
# (recettes_concurrentes) ; (6) colonne absente, colonne ambiguë (nom présent
# plusieurs fois dans `donnees`), colonne déclarée deux fois, type
# incompatible, date hors du domaine (date_hors_domaine) ; (7) recette
# impossible (aucune_recette, deduire_recette()) ; (8) rôle que la recette
# retenue n'emploie pas (role_non_pris_en_charge) ; (9) rôle requis par la
# segmentation manquant. Les refus 4 et 5 ne lisent que les noms de rôles ;
# le refus 8 suppose la sélection. Chaque refus nomme tous les rôles en
# cause. Rend la recette retenue (deduire_recette()).
controler_roles <- function(donnees, roles, segmentation) {
  if (!is.data.frame(donnees))
    refuser("donnees", "donnees_mal_formees",
            formuler("Les données doivent être un data.frame (le moteur ne lit pas de fichier)."),
            list(classe = class(donnees)))
  if (!forme_exacte(roles, "character", "names") || !noms_complets(names(roles)) || anyNA(roles))
    refuser("donnees", "roles_mal_formes",
            formuler(paste("Les rôles doivent être un vecteur de chaînes nommé (nom du rôle -> nom",
                           "de colonne), sans autre attribut que ses noms, sans valeur",
                           "ni nom manquant (ADR 0007, point 4.1).")),
            list(classe = class(roles), attributs = names(attributes(roles))))
  r <- names(roles)
  inconnus <- setdiff(r, CATALOGUE_ROLES$role)
  if (length(inconnus))
    refuser("donnees", "role_inconnu",
            formuler("Rôle inconnu du catalogue fermé : %s (colonne %s).",
                     liste_guillemets(inconnus), liste_guillemets(roles[r %in% inconnus])),
            list(role = inconnus, colonne = unname(roles[r %in% inconnus])))
  doubles <- unique(r[duplicated(r)])
  if (length(doubles))
    refuser("donnees", "role_declare_deux_fois",
            formuler("Rôle déclaré plusieurs fois : %s (colonnes %s).",
                     liste_guillemets(doubles), liste_guillemets(roles[r %in% doubles])),
            list(role = doubles, colonne = unname(roles[r %in% doubles])))
  employeurs <- recettes_du_role(r, CATALOGUE_RECETTES)
  non_pris <- r[lengths(employeurs) == 0L]
  if (length(non_pris))
    refuser("donnees", "role_non_pris_en_charge",
            formuler(paste("Rôle déclaré qu'aucune recette d'exposition du catalogue n'emploie : %s (colonne %s) ;",
                           "l'ignorer changerait le sens du résultat, la construction est refusée."),
                     liste_guillemets(non_pris), liste_guillemets(roles[non_pris])),
            list(role = non_pris, colonne = unname(roles[non_pris])))
  controler_recettes_concurrentes(roles, CATALOGUE_RECETTES)
  absentes <- r[!(roles %in% names(donnees))]
  if (length(absentes))
    refuser("donnees", "colonne_absente",
            formuler("Colonne absente des données : %s (rôle %s).",
                     liste_guillemets(roles[absentes]), liste_guillemets(absentes)),
            list(role = absentes, colonne = unname(roles[absentes])))
  ambigues <- r[vapply(roles, function(cl) sum(names(donnees) == cl) > 1L, logical(1L))]
  if (length(ambigues))
    refuser("donnees", "colonne_ambigue",
            formuler("Colonne présente plusieurs fois sous le même nom dans les données : %s (rôle %s).",
                     liste_guillemets(roles[ambigues]), liste_guillemets(ambigues)),
            list(role = ambigues, colonne = unname(roles[ambigues])))
  col_doubles <- unique(roles[duplicated(roles)])
  if (length(col_doubles))
    refuser("donnees", "colonne_declaree_deux_fois",
            formuler("Colonne déclarée pour plusieurs rôles : %s (rôles %s).",
                     liste_guillemets(col_doubles), liste_guillemets(r[roles %in% col_doubles])),
            list(role = r[roles %in% col_doubles], colonne = col_doubles))
  types <- CATALOGUE_ROLES$type[match(r, CATALOGUE_ROLES$role)]
  # Colonnes lues par lire_champ() (R/outils.R, #128) : donnees[[nom]] lève une
  # erreur R sur un nom de colonne d'encodage « bytes » (mesuré) ; la colonne,
  # présente (colonne_absente a refusé sinon), est jugée sur son type.
  colonne <- function(i) lire_champ(donnees, roles[[i]])
  ko <- !vapply(seq_along(r), function(i) type_conforme(colonne(i), types[i]),
                logical(1L))
  # TYPES_ADMIS (« entières », non ASCII) est une constante du moteur non
  # marquée : texte_moteur() la marque UTF-8 avant l'insertion par %s (sous
  # la locale C, sprintf() en écrirait les octets « <c3><a8> » ; #127).
  if (any(ko))
    refuser("donnees", "type_incompatible",
            formuler("Type incompatible : %s.",
                     paste(formuler("rôle « %s », colonne « %s » : %s attendu, %s reçu",
                                    citer(r[ko]), citer(roles[ko]),
                                    texte_moteur(TYPES_ADMIS[types[ko]]),
                                    vapply(which(ko), function(i)
                                      paste(citer(class(colonne(i))), collapse = "/"),
                                      character(1L))),
                           collapse = " ; ")),
            list(role = r[ko], colonne = unname(roles[ko]), type_attendu = unname(TYPES_ADMIS[types[ko]])))
  # Dates hors du domaine [0001-01-01, 9999-12-31] : type conforme, mais aucune
  # écriture canonique ; refus nommé, tous les rôles en cause et leurs lignes
  # sont cités (ADR 0011, point 4) ; details$lignes : liste nommée rôle ->
  # lignes, comme ligne_invalide.
  hors <- lapply(stats::setNames(which(types == "date"), r[types == "date"]),
                 function(i) which(dates_hors_domaine(unclass(colonne(i)))))
  hors <- hors[lengths(hors) > 0L]
  if (length(hors))
    refuser("donnees", "date_hors_domaine",
            formuler(paste("Dates hors du domaine [0001-01-01, 9999-12-31] : %s ; aucune écriture canonique à",
                           "quatre chiffres n'existe."),
                     paste(formuler("rôle « %s », colonne « %s » : %d date(s) (premières lignes %s)",
                                    citer(names(hors)), citer(roles[names(hors)]), lengths(hors),
                                    vapply(hors, function(h) paste(utils::head(h, NB_LIGNES_CITEES), collapse = ", "),
                                           character(1L))),
                           collapse = " ; ")),
            list(role = names(hors), colonne = unname(roles[names(hors)]), lignes = hors))
  recette <- deduire_recette(roles)
  hors <- r[!(r %in% recette$roles_employes)]
  if (length(hors)) {
    autres <- unique(unlist(recettes_du_role(hors, CATALOGUE_RECETTES), use.names = FALSE))
    refuser("donnees", "role_non_pris_en_charge",
            formuler(paste("Rôle déclaré que la recette « %s » n'emploie pas : %s (colonne %s, employé par %s %s) ;",
                           "l'ignorer changerait le sens du résultat, la construction est refusée."),
                     citer(recette$recette), liste_guillemets(hors), liste_guillemets(roles[hors]),
                     if (length(autres) > 1L) "les recettes" else "la recette", liste_guillemets(autres)),
            list(role = hors, colonne = unname(roles[hors]), recette = recette$recette))
  }
  seg_manquants <- setdiff(segmentation, r)
  if (length(seg_manquants))
    refuser("donnees", "role_requis_manquant",
            formuler(paste("Rôle requis manquant : %s, employé par la segmentation du plan",
                           "(recette « %s »)."),
                     liste_guillemets(seg_manquants), citer(recette$recette)),
            list(role = seg_manquants, recette = recette$recette))
  recette
}

# Étapes et estimateurs du plan admis sous la recette retenue (ADR 0016,
# point 5.2), seul endroit où s'applique la matrice : pour chaque étape du
# plan, dans l'ordre du plan, la ligne (étape, estimateur NA), puis, pour
# l'étape `brut`, la ligne de chaque estimateur, dans l'ordre du plan ; une
# ligne `non`, ou `si_role` sans son rôle déclaré, est un refus sous le motif
# de la ligne (module donnees). Puis les règles qui ne tiennent pas dans une
# ligne (regles_conditionnelles_recette()), puis une exclusion du plan hors
# des motifs de la recette retenue (exclusion_hors_recette ; valider_plan() a
# contrôlé l'union, point 6.4).
#   recette : valeur validée (deduire_recette()), dont matrice et motifs ;
#   roles   : rôles contrôlés ; plan : plan canonique ;
#   conv    : convention validée (resoudre_convention()), NULL sans étape
#             exposition ;
#   p       : valeurs validées de la transition, de la période et du délai
#             (valeurs_parametres_donnees()) ; la règle identifiant_inadapte
#             des stocks lit le porteur de la transition (p$transition).
# Une étape ou un estimateur sans exactement une ligne est une erreur de
# programmation (matrice incomplète), jamais une admission. Appelée par
# controler_plan_roles() après resoudre_convention() (couche partagée par
# construire_table() et rejouer_fiche(), point 5.3). Rend NULL, invisible.
controler_matrice_recette <- function(recette, roles, plan, conv, p) {
  m <- recette$matrice
  etapes <- plan[["etapes"]]
  cles <- do.call(rbind, lapply(names(etapes), function(e)
    data.frame(etape = e, estimateur = c(NA_character_, if (identical(e, "brut")) etapes[[e]][["estimateurs"]]),
               stringsAsFactors = FALSE)))
  for (i in seq_len(nrow(cles))) {
    e <- cles$etape[i]; est <- cles$estimateur[i]
    k <- which(m$etape == e & (if (is.na(est)) is.na(m$estimateur) else (!is.na(m$estimateur) & m$estimateur == est)))
    if (length(k) != 1L)
      stop(sprintf("controler_matrice_recette() : %d ligne(s) de matrice pour (%s, %s, %s) (erreur de programmation).",
                   length(k), recette$recette, e, est), call. = FALSE)
    admis <- m$admis[k]
    if (identical(admis, "oui") || (identical(admis, "si_role") && m$role_requis[k] %in% names(roles))) next
    quoi <- if (is.na(est)) formuler("l'étape « %s » n'est pas admise", citer(e))
            else formuler("l'estimateur « %s » de l'étape « brut » n'est pas admis", citer(est))
    if (identical(admis, "si_role"))
      refuser("donnees", m$motif[k],
              formuler("Sous la recette « %s », %s sans le rôle %s ; la construction est refusée.",
                       citer(recette$recette), quoi, liste_guillemets(m$role_requis[k])),
              list(recette = recette$recette, etape = e, estimateur = est, role = m$role_requis[k]))
    refuser("donnees", m$motif[k],
            formuler("Sous la recette « %s », %s ; la construction est refusée.",
                     citer(recette$recette), quoi),
            list(recette = recette$recette, etape = e, estimateur = est))
  }
  regles_conditionnelles_recette(recette, roles, etapes, conv, p)
  hors <- setdiff(etapes[["donnees"]][["exclusions"]], recette$motifs)
  if (length(hors))
    refuser("donnees", "exclusion_hors_recette",
            formuler("Code d'exclusion sans objet pour la recette « %s » : %s (motifs de cette recette : %s).",
                     citer(recette$recette), liste_guillemets(hors), liste_guillemets(recette$motifs)),
            list(recette = recette$recette, exclusions = hors))
  invisible(NULL)
}

# Règles de la matrice qui ne tiennent pas dans une ligne (recette, étape,
# estimateur) (ADR 0016, point 5.2), par recette (aiguillage sur le nom,
# admis dans ce module, point 4.4). Sous `agregats` (#169, § 3 et § 4), dans
# cet ordre :
#   - par_annee_sans_annee : étape exposition avec par_annee = TRUE sans le
#     rôle annee (une cellule agrégée ne se redécoupe pas par année) ;
#   - binomial_annee_dernier_anniversaire : binomial avec le rôle annee sous
#     la convention retenue dernier_anniversaire (E^i n'est pas additive sur
#     la partition de Lexis d'une année d'âge ; binomial sous par_annee =
#     TRUE déjà refusé par controler_parametres_brut()).
# Sous `stocks_successifs` (#180, § 3, QS3), dans cet ordre :
#   - identifiants_concurrents : id_tete et id_contrat déclarés ensemble
#     (l'unité des stocks est un seul identifiant ; la fusion par tête de
#     contrats inventoriés n'est pas prise en charge, #190) ;
#   - identifiant_inadapte : transition portée par le contrat
#     (p$transition$etat_de) avec id_tete seul.
# Aucune règle sous `lignes_dates_completes`. Rend NULL, invisible.
regles_conditionnelles_recette <- function(recette, roles, etapes, conv, p) {
  switch(recette$recette,
    agregats = {
      if ("exposition" %in% names(etapes) && isTRUE(etapes[["exposition"]][["par_annee"]]) &&
          !("annee" %in% names(roles)))
        refuser("donnees", "par_annee_sans_annee",
                formuler(paste("Sous la recette « %s », le découpage par année (par_annee = TRUE) exige le rôle",
                               "« annee » : une cellule agrégée ne se redécoupe pas par année civile ; la",
                               "construction est refusée."),
                         citer(recette$recette)),
                list(recette = recette$recette, etape = "exposition", role = "annee"))
      if ("binomial" %in% etapes[["brut"]][["estimateurs"]] && "annee" %in% names(roles) &&
          identical(conv$retenue, "dernier_anniversaire"))
        refuser("donnees", "binomial_annee_dernier_anniversaire",
                formuler(paste("Sous la recette « %s », l'estimateur « binomial » n'est pas admis avec le rôle",
                               "« annee » sous la convention « dernier_anniversaire » : l'exposition initiale",
                               "d'une année d'âge n'est pas la somme de celles des carrés de Lexis (le complément",
                               "d'un événement court jusqu'à l'anniversaire suivant, à travers la fin de l'année",
                               "civile) ; retenir l'estimateur « poisson », ou fournir des cellules par âge seul",
                               "dont l'exposition initiale a été formée par le producteur sur l'année d'âge ; la",
                               "construction est refusée."),
                         citer(recette$recette)),
                list(recette = recette$recette, etape = "brut", estimateur = "binomial", role = "annee",
                     convention = conv$retenue))
    },
    stocks_successifs = {
      ids <- intersect(c("id_tete", "id_contrat"), names(roles))
      if (length(ids) > 1L)
        refuser("donnees", "identifiants_concurrents",
                formuler(paste("Sous la recette « %s », les rôles %s sont déclarés ensemble : l'unité d'observation",
                               "des stocks successifs est un seul identifiant (la fusion par tête de contrats",
                               "inventoriés n'est pas prise en charge) ; déclarer un seul des deux (« id_tete » pour",
                               "une transition sur la tête, « id_contrat » pour une transition sur le contrat), la",
                               "construction est refusée."),
                         citer(recette$recette), liste_guillemets(ids)),
                list(recette = recette$recette, role = ids, colonne = unname(roles[ids])))
      if (identical(p$transition$etat_de, "contrat") && identical(ids, "id_tete"))
        refuser("donnees", "identifiant_inadapte",
                formuler(paste("Sous la recette « %s », la transition « %s » porte sur le contrat : le rôle « id_tete »",
                               "seul ne désigne pas l'unité de la transition (les contrats d'une même tête ne se",
                               "distinguent pas) ; déclarer le rôle « id_contrat », la construction est refusée."),
                         citer(recette$recette), citer(p$transition$code)),
                list(recette = recette$recette, transition = p$transition$code, role = "id_tete"))
    },
    NULL)
  invisible(NULL)
}

# --- paramètres de l'étape ----------------------------------------------------

# Contrôle de la segmentation du plan (#59), seul endroit où s'écrivent sa
# forme et son catalogue : vecteur character sans attribut (ni names, ni dim,
# ni classe), sans NA ni doublon, dont chaque clé est prise dans
# ROLES_SEGMENTATION (vide explicite admis ; la chaîne vide n'en fait pas
# partie). Sinon refus sous le motif segmentation_invalide, module donnees
# (seul appelant depuis #82 : controler_parametres_donnees(), appelée par
# valider_plan()). Rend NULL sans rien modifier.
controler_segmentation <- function(segmentation) {
  if (!forme_exacte(segmentation, "character") || anyNA(segmentation) || anyDuplicated(segmentation) ||
      !all(segmentation %in% ROLES_SEGMENTATION))
    refuser("donnees", "segmentation_invalide",
            formuler(paste("La segmentation doit être un vecteur de chaînes sans attribut, sans doublon,",
                           "pris dans %s (vide explicite admis)."),
                     liste_guillemets(ROLES_SEGMENTATION)),
            list(segmentation = segmentation))
  invisible(NULL)
}

# Ligne de TRANSITIONS de la transition `transition` (list(depart, arrivee) de
# forme déjà contrôlée) ; zéro ligne si elle n'est pas prise en charge. Seul
# endroit où s'écrit la recherche.
ligne_transition <- function(transition)
  TRANSITIONS[TRANSITIONS$depart == transition[["depart"]] & TRANSITIONS$arrivee == transition[["arrivee"]], ,
              drop = FALSE]

# Valeurs que les paramètres de transition, de période et de délai déjà
# contrôlés donnent (#22) : list(transition, debut, fin, borne, periode,
# delai_declaration[, inventaires]), la ligne de TRANSITIONS, les bornes de la
# période et la borne B = fin - délai + 1 (jours depuis 1970-01-01), puis la
# période et le délai tels que le plan les porte (#182 : seule source des
# champs validés ; periode et delai_declaration sont inscrits tels quels dans
# les paramètres de l'étape exposition), puis la grille des inventaires telle
# que le plan la porte (champ conditionnel `inventaires`, ADR 0016, point
# 7.1 ; #180) : l'élément n'existe que si `inventaires` n'est pas NULL (champ
# absent du plan). Ne contrôle rien : rendue par controler_transition_periode()
# (sans grille) et par controler_plan_roles() (orchestration, grille du plan
# canonique), qui la transmet aux fonctions d'étape après valider_plan() (#82).
valeurs_parametres_donnees <- function(transition, periode, delai_declaration, inventaires = NULL) {
  fin <- as.numeric(periode[["fin"]])
  v <- list(transition = ligne_transition(transition), debut = as.numeric(periode[["debut"]]), fin = fin,
            borne = (fin - delai_declaration) + 1, periode = periode, delai_declaration = delai_declaration)
  if (!is.null(inventaires)) v$inventaires <- inventaires
  v
}

# Contrôle la transition, la période d'observation et le délai de déclaration
# du plan (controler_parametres_donnees()) ; rend valeurs_parametres_donnees().
# Forme exacte (#23, forme_exacte()) : chaque valeur a son type de base exact
# et ne porte aucun attribut (ni names, ni dim, ni classe), hormis les noms
# des listes `transition` et `periode` et la classe Date des bornes de la
# période ; sinon refus sous le motif du paramètre. Ordre : transition,
# période, délai, période vide.
controler_transition_periode <- function(transition, periode, delai_declaration) {
  if (!liste_aux_noms(transition, c("depart", "arrivee")) || !chaine_unique(transition[["depart"]]) ||
      !chaine_unique(transition[["arrivee"]]))
    refuser("donnees", "transition_inconnue",
            formuler(paste("La transition doit être une liste de deux noms d'états (chaînes non vides, sans attribut),",
                           "de champs exactement depart et arrivee, sans autre attribut (ADR 0007, point 5.1).")),
            list(transition = transition))
  depart <- transition[["depart"]]; arrivee <- transition[["arrivee"]]
  if (!nrow(ligne_transition(transition))) {
    # États cités par citer() : règle neutraliser_citation() (R/outils.R ; #111, #94).
    refuser("donnees", "transition_inconnue",
            formuler("Transition non prise en charge : %s -> %s (prises en charge : %s).",
                     citer(depart), citer(arrivee),
                     paste(TRANSITIONS$depart, "->", TRANSITIONS$arrivee, collapse = ", ")),
            list(transition = transition))
  }
  # Stockage double ou integer admis (inchangé), seul attribut : la classe
  # (date_unique(), outils ; #113).
  if (!liste_aux_noms(periode, c("debut", "fin")) || !date_unique(periode[["debut"]]) ||
      !date_unique(periode[["fin"]]))
    refuser("donnees", "periode_invalide",
            formuler(paste("La période d'observation doit être une liste de champs exactement debut et fin,",
                           "sans autre attribut, deux dates (classe Date, sans autre attribut) non manquantes.")),
            list(periode = periode))
  # Bornes hors du domaine des dates (ADR 0011, point 4), même domaine que les données.
  hors_p <- c(debut = dates_hors_domaine(periode[["debut"]]), fin = dates_hors_domaine(periode[["fin"]]))
  if (any(hors_p))
    refuser("donnees", "date_hors_domaine",
            formuler("Période d'observation : borne(s) %s hors du domaine des dates [0001-01-01, 9999-12-31].",
                     paste(names(hors_p)[hors_p], collapse = " et ")),
            list(bornes = names(hors_p)[hors_p]))
  if (!forme_exacte(delai_declaration, c("integer", "double")) || length(delai_declaration) != 1L ||
      is.na(delai_declaration) || !is.finite(delai_declaration) ||
      delai_declaration != trunc(delai_declaration) || delai_declaration < 0)
    refuser("donnees", "delai_invalide",
            formuler("Le délai de déclaration doit être un nombre entier unique de jours >= 0, sans attribut (A.0, T4)."),
            list(delai_declaration = delai_declaration))
  debut <- as.numeric(periode[["debut"]]); fin <- as.numeric(periode[["fin"]])
  fin_effective <- fin - delai_declaration
  if (debut > fin_effective)
    refuser("donnees", "periode_vide",
            formuler("Période d'observation vide : début %s postérieur à la fin effective %s (fin %s, délai %s jour(s)).",
                     citer(format(periode[["debut"]])), citer(format(structure(fin_effective, class = "Date"))),
                     citer(format(periode[["fin"]])), citer(format(delai_declaration))),
            list(debut = periode[["debut"]], fin_effective = structure(fin_effective, class = "Date")))
  valeurs_parametres_donnees(transition, periode, delai_declaration)
}

# Contrôle les paramètres que l'étape `donnees` reçoit ; rend la ligne de
# TRANSITIONS et les bornes de la période (controler_transition_periode()).
# Ordre : transition, période, délai, période vide, segmentation (motif
# segmentation_invalide), exclusions.
controler_parametres_donnees <- function(transition, periode, delai_declaration,
                                         segmentation, exclusions) {
  p <- controler_transition_periode(transition, periode, delai_declaration)
  controler_segmentation(segmentation)
  if (!forme_exacte(exclusions, "character") || anyNA(exclusions) || anyDuplicated(exclusions))
    refuser("donnees", "exclusions_invalides",
            formuler(paste("Les règles d'exclusion doivent être un vecteur de codes de motif sans attribut, sans doublon",
                           "(vide explicite admis).")),
            list(exclusions = exclusions))
  inconnus <- setdiff(exclusions, motifs_recettes())
  if (length(inconnus))
    refuser("donnees", "exclusion_inconnue",
            formuler("Code d'exclusion inconnu : %s (liste fermée des motifs).", liste_guillemets(inconnus)),
            list(exclusions = inconnus))
  p
}

# --- motifs de ligne et de tête (A.1) -----------------------------------------

# Groupes (entiers) qui portent au moins deux valeurs valides distinctes de
# v : une valeur manquante est ignorée ((H, F, NA) hétérogène, (H, NA) non ;
# #69, décision d'actuary).
groupes_heterogenes <- function(g, v) {
  ok <- !is.na(v)
  g <- g[ok]; v <- v[ok]
  o <- order(g, v, method = "radix")
  gs <- g[o]; vs <- v[o]
  prem <- !duplicated(gs); dern <- !duplicated(gs, fromLast = TRUE)
  gs[prem][vs[prem] != vs[dern]]
}

# Minimum et maximum de x par groupe : list(groupe, min, max).
extremes_par_groupe <- function(g, x) {
  o <- order(g, x, method = "radix")
  gs <- g[o]; xs <- x[o]
  prem <- !duplicated(gs); dern <- !duplicated(gs, fromLast = TRUE)
  list(groupe = gs[prem], min = xs[prem], max = xs[dern])
}

# Premier décès s_d de chaque tête (g : entier de tête) et observation après
# ce décès : une fin d'observation (+Inf pour une ligne en cours, quelle que
# soit sa date de sortie, comme dans la fusion ; NA compté +Inf) dépasse s_d.
# Rend list(groupe, premier_deces, apres) ; premier_deces = +Inf sans décès.
deces_par_tete <- function(g, sor, cause) {
  fin_obs <- sor; fin_obs[is.na(fin_obs) | cause == "en_cours"] <- Inf
  dec <- rep(Inf, length(g))
  est_dec <- cause == "deces"
  dec[est_dec] <- sor[est_dec]
  e_dec <- extremes_par_groupe(g, dec)
  e_fin <- extremes_par_groupe(g, fin_obs)
  list(groupe = e_dec$groupe, premier_deces = e_dec$min, apres = e_fin$max > e_dec$min)
}

# Évalue tous les motifs sur toutes les lignes (aucun court-circuit ; un
# motif de comparaison n'est pas levé si l'une de ses valeurs est NA). Les
# motifs de tête sont évalués sur les lignes exemptes de motif de ligne et
# s'appliquent aux lignes propres de la tête ; une valeur manquante (sexe
# hors modalités compris) n'y rend pas une tête incohérente (#69). Rend
# list(motifs) : liste nommée code -> numéros de ligne (1-based, croissants),
# dans l'ordre MOTIFS_LIGNES puis MOTIFS_TETE (integer(0) pour un motif sans
# objet).
#   lignes_tete : facultatif (#69) ; NULL (défaut) : motifs de tête évalués
#                 sur les lignes exemptes de motif de ligne ; sinon, numéros
#                 (integer sans attribut, sans NA ni doublon, dans
#                 1..nrow(donnees)) des lignes sur lesquelles ils sont
#                 évalués et auxquelles ils s'appliquent, quels que soient
#                 leurs motifs de ligne (contrefactuel sans exclusions).
#                 Les motifs de ligne n'en dépendent pas. Une ligne désignée
#                 sans identifiant de tête, sans cause de sortie ou sans
#                 date de sortie hors « en_cours » (motifs non mesurables,
#                 #69, § 2) est une erreur de programmation.
evaluer_motifs <- function(donnees, roles, debut, fin, borne, segmentation, lignes_tete = NULL) {
  n <- nrow(donnees)
  if (!is.null(lignes_tete) && (!is.integer(lignes_tete) || !is.null(attributes(lignes_tete)) ||
                                anyNA(lignes_tete) || anyDuplicated(lignes_tete) ||
                                any(lignes_tete < 1L | lignes_tete > n)))
    stop(paste("evaluer_motifs() : lignes_tete hors de la forme admise (integer sans attribut, sans NA ni",
               "doublon, dans 1..nrow(donnees)) (erreur de programmation)."), call. = FALSE)
  col <- function(role) if (role %in% names(roles)) donnees[[roles[[role]]]] else NULL
  jours <- function(role) as.numeric(unclass(col(role)))
  nai <- jours("date_naissance"); ent <- jours("date_entree"); sor <- jours("date_sortie")
  cause <- as.character(col("cause_sortie"))
  m <- stats::setNames(rep(list(logical(n)), length(MOTIFS_LIGNES)), MOTIFS_LIGNES)
  vrai <- function(x) !is.na(x) & x

  id_c <- col("id_contrat")
  if (!is.null(id_c)) {
    m$id_contrat_manquant <- is.na(id_c)
    m$contrat_duplique <- !is.na(id_c) &
      (duplicated(id_c, incomparables = NA) | duplicated(id_c, fromLast = TRUE, incomparables = NA))
  }
  id_t <- col("id_tete")
  if (!is.null(id_t)) m$id_tete_manquant <- is.na(id_t)
  m$naissance_manquante <- is.na(nai)
  m$entree_manquante <- is.na(ent)
  m$cause_manquante <- is.na(cause)
  m$cause_hors_modalites <- !is.na(cause) & !(cause %in% MODALITES_CAUSE)
  m$sortie_manquante <- is.na(sor) & !is.na(cause) & cause != "en_cours"
  sexe <- col("sexe")
  if (!is.null(sexe)) {
    sexe <- as.character(sexe)
    m$sexe_manquant <- is.na(sexe)
    m$sexe_hors_modalites <- !is.na(sexe) & !(sexe %in% MODALITES_SEXE)
  }
  segment <- col("segment")
  if (!is.null(segment)) m$segment_manquant <- is.na(segment)
  m$naissance_apres_entree <- vrai(nai > ent)
  m$sortie_avant_entree <- vrai(sor < ent)
  m$en_cours_avant_fin <- vrai(cause == "en_cours" & sor < fin)
  an_nai <- col("annee_naissance")
  if (!is.null(an_nai))
    m$annee_naissance_incoherente <- vrai(an_nai != composantes_date(nai)$an)
  t_age <- ifelse(is.na(sor), fin, pmin(sor, fin))
  m$age_aberrant <- vrai(age_revolu(nai, t_age) >= AGE_ABERRANT)

  t <- stats::setNames(rep(list(logical(n)), length(MOTIFS_TETE)), MOTIFS_TETE)
  if (!is.null(id_t)) {
    if (!is.null(lignes_tete) &&
        any(m$id_tete_manquant[lignes_tete] | m$cause_manquante[lignes_tete] | m$sortie_manquante[lignes_tete]))
      stop(paste("evaluer_motifs() : lignes_tete désigne une ligne sans identifiant de tête, sans cause de sortie",
                 "ou sans date de sortie hors « en_cours » (erreur de programmation)."), call. = FALSE)
    propres <- if (is.null(lignes_tete)) which(!Reduce(`|`, m)) else lignes_tete
    g <- match(id_t[propres], unique(id_t[propres]))
    marquer <- function(groupes) {
      x <- logical(n)
      x[propres[g %in% groupes]] <- TRUE
      x
    }
    t$tete_naissance_incoherente <- marquer(groupes_heterogenes(g, nai[propres]))
    if (!is.null(sexe)) {
      sexe_t <- sexe[propres]; sexe_t[!(sexe_t %in% MODALITES_SEXE)] <- NA
      t$tete_sexe_incoherent <- marquer(groupes_heterogenes(g, sexe_t))
    }
    if (!is.null(segment) && "segment" %in% segmentation)
      t$tete_segment_incoherent <- marquer(groupes_heterogenes(g, as.character(segment[propres])))
    # Lecture B d'E2.4 (décision du mainteneur du 16/10/2026) : motif si le
    # premier décès s_d de la tête précède `borne` (= fin - délai + 1) et
    # qu'une fin d'observation le dépasse ; si s_d >= borne, le décès est
    # hors de la période effective : aucune ligne refusée, relevé non
    # bloquant compté par preparer_donnees() sur les lignes conservées.
    dt <- deces_par_tete(g, sor[propres], cause[propres])
    t$tete_observee_apres_deces <- marquer(dt$groupe[dt$apres & dt$premier_deces < borne])
  }
  list(motifs = lapply(c(m, t), which))
}

# --- motifs et observations de la recette `agregats` (#169, § 5 à § 7) -------

# Lignes d'une même clé exacte, toutes les lignes en cause (règle commune de
# cellule_dupliquee et d'inventaire_duplique) : rang de chaque valeur parmi les
# valeurs distinctes de sa colonne (match(), égalité exacte, sans écriture
# décimale), rangs collés par colonne de clé, puis doublons sur les rangs dans
# les deux sens ; une ligne `complete` est une ligne retenue pour la
# comparaison (clé renseignée, et, pour les inventaires, ligne d'inventaire) :
# le masquage des rangs des autres lignes (NA) les écarte de l'appariement avec
# une ligne complète de même clé apparente, et `complete &` les ramène à FALSE.
#   cles     : liste de vecteurs de clé, de même longueur ;
#   complete : logique, une valeur par ligne.
# Rend un logique, une valeur par ligne.
lignes_cle_dupliquee <- function(cles, complete) {
  rangs <- do.call(paste, c(lapply(cles, function(v) match(v, unique(v))), list(sep = "\r")))
  rangs[!complete] <- NA
  complete & (duplicated(rangs, incomparables = NA) | duplicated(rangs, fromLast = TRUE, incomparables = NA))
}

# Sous-indiçage [i] d'une colonne de catégorie (sexe, segment) des
# observations, type d'origine conservé. Un facteur est recopié sous forme
# normalisée (#67, M72) : classe ramenée à factor, ou ordered s'il est
# ordonné ; niveaux tels quels (ordre et niveaux inutilisés compris) ; tout
# autre attribut (contrasts, sous-classe…) retiré, que l'empreinte du résultat
# refuserait. Chaîne ou entier : sous-indiçage [i], qui retire les attributs
# de colonne (M71, Q4).
sous_indicer_categorie <- function(x, i) {
  if (is.factor(x)) structure(unclass(x)[i], levels = levels(x),
                              class = if (is.ordered(x)) c("ordered", "factor") else "factor")
  else x[i]
}

# Évalue tous les motifs de ligne de la recette `agregats` (MOTIFS_LIGNES_AGREGATS)
# sur toutes les lignes (une ligne est une cellule), sans court-circuit ; un
# motif de comparaison n'est pas levé sur une valeur manquante, comme dans
# evaluer_motifs(). Défauts de valeur (Q5) : manquant (NA ou NaN), non entier
# (une valeur infinie est non entière : evenements_non_entiers, « non finis
# compris », § 6.2 ; de même pour l'âge et l'année), négatif ; une exposition
# est manquante (NA ou NaN), infinie (exposition_non_finie) ou négative, pour
# chaque rôle d'exposition déclaré. annee_hors_periode : année antérieure à
# l'année du début ou postérieure à celle de la fin effective (borne - 1 ; le
# délai est reçu comme appliqué par le producteur, Q3, et ne sert qu'à ce
# contrôle). evenements_sans_exposition : D > 0 alors qu'aucune exposition
# déclarée n'est > 0, toutes renseignées. cellule_dupliquee : toutes les
# lignes d'une même clé (rôles de clé déclarés : sexe, segment, age, annee,
# employés ou non par la segmentation, § 5), clés non manquantes, valeurs
# comparées exactement. Rend list(motifs) : liste nommée code -> numéros de
# ligne (1-based, croissants), dans l'ordre MOTIFS_LIGNES_AGREGATS.
evaluer_motifs_agregats <- function(donnees, roles, p) {
  n <- nrow(donnees)
  col <- function(role) if (role %in% names(roles)) donnees[[roles[[role]]]] else NULL
  vrai <- function(x) !is.na(x) & x
  non_entier <- function(x) !is.na(x) & (!is.finite(x) | x != trunc(x))
  m <- stats::setNames(rep(list(logical(n)), length(MOTIFS_LIGNES_AGREGATS)), MOTIFS_LIGNES_AGREGATS)
  age <- as.numeric(col("age"))
  m$age_manquant <- is.na(age)
  m$age_non_entier <- non_entier(age)
  m$age_negatif <- vrai(age < 0)
  m$age_aberrant <- vrai(age >= AGE_ABERRANT)
  annee <- col("annee")
  if (!is.null(annee)) {
    annee <- as.numeric(annee)
    m$annee_manquante <- is.na(annee)
    m$annee_non_entiere <- non_entier(annee)
    m$annee_hors_periode <- vrai(annee < composantes_date(p$debut)$an | annee > composantes_date(p$borne - 1)$an)
  }
  sexe <- col("sexe")
  if (!is.null(sexe)) {
    sexe <- as.character(sexe)
    m$sexe_manquant <- is.na(sexe)
    m$sexe_hors_modalites <- !is.na(sexe) & !(sexe %in% MODALITES_SEXE)
  }
  segment <- col("segment")
  if (!is.null(segment)) m$segment_manquant <- is.na(segment)
  expositions <- lapply(intersect(c("exposition_centrale", "exposition_initiale"), names(roles)),
                        function(r) as.numeric(col(r)))
  m$exposition_manquante <- Reduce(`|`, lapply(expositions, is.na))
  m$exposition_non_finie <- Reduce(`|`, lapply(expositions, function(e) !is.na(e) & !is.finite(e)))
  m$exposition_negative <- Reduce(`|`, lapply(expositions, function(e) vrai(e < 0)))
  ev <- as.numeric(col("evenements"))
  m$evenements_manquants <- is.na(ev)
  m$evenements_non_entiers <- non_entier(ev)
  m$evenements_negatifs <- vrai(ev < 0)
  m$evenements_sans_exposition <- vrai(ev > 0) & !m$exposition_manquante &
    !Reduce(`|`, lapply(expositions, function(e) vrai(e > 0)))
  # Clé exacte (lignes_cle_dupliquee()).
  cles <- Filter(Negate(is.null), lapply(c("sexe", "segment", "age", "annee"), function(r) {
    v <- col(r)
    if (is.null(v)) NULL else if (is.factor(v)) as.character(v) else v
  }))
  complete <- !Reduce(`|`, lapply(cles, is.na))
  m$cellule_dupliquee <- lignes_cle_dupliquee(cles, complete)
  list(motifs = lapply(m, which))
}

# Observations de la recette `agregats` sur les lignes retenues : une
# observation par cellule reçue et conservée, sans aucune complétion (§ 5).
# Unité d'observation `non_determinee` (Q4). Colonnes : sexe s'il est
# déclaré, segment s'il est employé par la segmentation (même règle et même
# recopie que construire_observations()), age et annee (s'il est déclaré) en
# double, les expositions déclarées (exposition_centrale, exposition_initiale)
# en double, evenements en double ; les valeurs, contrôlées par les motifs,
# ne sont pas modifiées (la somme et la conversion en entier des événements
# relèvent de l'étape exposition).
#   lignes, numeros, roles, segmentation : comme construire_observations() ;
#   motifs     : motifs évalués sur toutes les lignes (evaluer_motifs_agregats()) ;
#   exclusions : codes exclus du plan ;
#   n          : nombre de lignes reçues.
# Rend list(unite, observations, observation_des_lignes, releves) : releves
# porte le nombre de cellules conservées, les relevés de cohérence des deux
# expositions quand elles sont déclarées ensemble (§ 2 ; NA sinon) et le
# poids des cellules exclues, par motif déclaré (§ 7, Q6) : cellules exclues
# sous le motif, puis, sur celles dont D et les expositions déclarées ne
# portent aucun motif de valeur, leur nombre, Σ D et Σ E par exposition
# déclarée (NA pour une exposition non déclarée).
observations_agregats <- function(lignes, numeros, roles, segmentation, motifs, exclusions, n, donnees) {
  col <- function(role, d = lignes) if (role %in% names(roles)) d[[roles[[role]]]] else NULL
  i <- seq_along(numeros)
  observations <- data.frame(row.names = seq_along(i))
  if ("sexe" %in% names(roles)) observations$sexe <- sous_indicer_categorie(col("sexe"), i)
  if ("segment" %in% segmentation) observations$segment <- sous_indicer_categorie(col("segment"), i)
  observations$age <- as.numeric(col("age"))
  if ("annee" %in% names(roles)) observations$annee <- as.numeric(col("annee"))
  declarees <- intersect(c("exposition_centrale", "exposition_initiale"), names(roles))
  for (r in declarees) observations[[r]] <- as.numeric(col(r))
  observations$evenements <- as.numeric(col("evenements"))
  rownames(observations) <- NULL
  deux <- length(declarees) == 2L
  ec <- observations$exposition_centrale; ei <- observations$exposition_initiale
  valeur <- c("exposition_manquante", "exposition_non_finie", "exposition_negative", "evenements_manquants",
              "evenements_non_entiers", "evenements_negatifs")
  sans_defaut <- !(seq_len(n) %in% unlist(motifs[valeur], use.names = FALSE))
  somme <- function(r, k) if (r %in% declarees) sum(as.numeric(col(r, donnees))[k]) else NA_real_
  poids <- do.call(rbind, lapply(exclusions, function(x) {
    k <- motifs[[x]][sans_defaut[motifs[[x]]]]
    data.frame(motif = x, cellules = length(motifs[[x]]), cellules_sommees = length(k),
               evenements = sum(as.numeric(col("evenements", donnees))[k]),
               exposition_centrale = somme("exposition_centrale", k),
               exposition_initiale = somme("exposition_initiale", k), stringsAsFactors = FALSE)
  }))
  if (is.null(poids))
    poids <- data.frame(motif = character(0), cellules = integer(0), cellules_sommees = integer(0),
                        evenements = numeric(0), exposition_centrale = numeric(0),
                        exposition_initiale = numeric(0), stringsAsFactors = FALSE)
  releves <- list(cellules = length(i),
                  exposition_initiale_inferieure_centrale = if (deux) sum(ei < ec) else NA_integer_,
                  complement_superieur_aux_evenements = if (deux) sum(ei - ec > observations$evenements)
                                                        else NA_integer_,
                  poids_exclusions = poids)
  list(unite = "non_determinee", observations = observations, observation_des_lignes = i, releves = releves)
}

# --- motifs et observations de la recette `stocks_successifs` (#180, § 4 et § 7) --

# Fonctions de la recette `stocks_successifs` (ligne de CATALOGUE_RECETTES),
# appelées par les aiguillages de preparer_donnees() ; avertissements des
# relevés : avertir_releves_stocks().
# Conventions (spécification d'actuary-approfondi de #180, § 2.1 et § 4,
# M328) : une ligne d'inventaire datée d atteste la présence de l'unité à
# l'instant d au sens d'A.0 (e <= d < s : état en fin de journée) ; la grille
# t_1 < ... < t_M (p$inventaires, jours depuis 1970-01-01) est celle du plan
# canonique, alignée par valider_plan() (t_1 = début - 1, t_M = fin effective) ;
# l'unité est l'identifiant déclaré, id_tete ou id_contrat (exactement un :
# le refus identifiants_concurrents relève de la matrice, ADR 0016, point 5.2).

# Identifiant d'unité déclaré des stocks : "id_tete" ou "id_contrat". Deux
# identifiants, ou aucun, sont une erreur de programmation (refus en amont :
# identifiants_concurrents, aucune_recette).
identifiant_stocks <- function(roles) {
  id <- intersect(c("id_tete", "id_contrat"), names(roles))
  if (length(id) != 1L)
    stop(sprintf("identifiant_stocks() : %d identifiant(s) déclaré(s), un seul attendu (erreur de programmation).",
                 length(id)), call. = FALSE)
  id
}

# Faits par unité des stocks, sur des lignes de type valide : g, entier
# d'unité de chaque ligne (rang de première apparition) ; inv, evt : lignes
# d'inventaire et d'événement (logical) ; k : indice dans la grille de la date
# d'inventaire de chaque ligne (NA hors grille et pour un événement) ; s,
# cause : date de sortie (jours) et cause de chaque ligne ; G : nombre
# d'unités. Rend list(f, l, n_inventaires, n_evenements, s_min, s_max,
# cause) par unité : indices du premier et du dernier inventaire où l'unité
# est présente (NA sans inventaire), nombres de lignes d'inventaire et
# d'événement, plus petite et plus grande date de sortie de ses événements
# (NA sans événement), cause de l'événement de plus petite date (le premier
# dans l'ordre des lignes à date égale ; NA sans événement).
faits_unites_stocks <- function(g, inv, evt, k, s, cause, G) {
  f <- l <- rep(NA_integer_, G)
  ki <- k[inv]
  if (any(inv)) {
    e <- extremes_par_groupe(g[inv][!is.na(ki)], ki[!is.na(ki)])
    f[e$groupe] <- e$min; l[e$groupe] <- e$max
  }
  s_min <- s_max <- rep(NA_real_, G)
  cause_u <- rep(NA_character_, G)
  if (any(evt)) {
    ge <- g[evt]; se <- s[evt]; ce <- cause[evt]
    ok <- !is.na(se)
    e <- extremes_par_groupe(ge[ok], se[ok])
    s_min[e$groupe] <- e$min; s_max[e$groupe] <- e$max
    o <- order(ge[ok], se[ok], method = "radix")
    prem <- o[!duplicated(ge[ok][o])]
    cause_u[ge[ok][prem]] <- ce[ok][prem]
  }
  list(f = f, l = l, n_inventaires = tabulate(g[inv], nbins = G), n_evenements = tabulate(g[evt], nbins = G),
       s_min = s_min, s_max = s_max, cause = cause_u)
}

# Évalue tous les motifs de la recette `stocks_successifs` (spécification de
# #180, § 7.2 et § 7.3) sur toutes les lignes, sans court-circuit ; un motif
# de comparaison n'est pas levé sur une valeur manquante. Motifs de ligne
# (MOTIFS_LIGNES_STOCKS) :
#   - toute ligne : type_ligne manquant ou hors de MODALITES_TYPE_LIGNE ;
#     identifiant déclaré manquant ; naissance manquante ; sexe et segment
#     (rôles déclarés) ; annee_naissance_incoherente (rôle déclaré) ;
#     age_aberrant, âge au dernier anniversaire >= AGE_ABERRANT à la date de
#     la ligne (date d'inventaire d'une ligne d'inventaire, date de sortie
#     d'une ligne d'événement ; non levé sans type valide ou sans date) ;
#   - ligne d'inventaire : sans date ; date hors de la grille ; date ou cause
#     de sortie renseignée (« en_cours » compris) ; naissance postérieure à la
#     date d'inventaire ; inventaire_duplique (même identifiant, même date,
#     valeurs non manquantes : toutes les lignes en cause) ;
#   - ligne d'événement : sortie_manquante (date de sortie manquante, cause
#     renseignée autre que « en_cours », même sens que sous les lignes) ;
#     cause manquante, hors de MODALITES_CAUSE, ou « en_cours » ; date
#     d'inventaire renseignée ; naissance postérieure à la sortie.
# Motifs d'unité (MOTIFS_UNITE_STOCKS), évalués sur les unités exemptes de
# motif de ligne — identifiants valides dont aucune ligne reçue ne porte de
# motif de ligne — et marqués sur toutes les lignes de l'unité (amendement A1
# de la spécification, § 7.3, M339) : une unité porteuse d'un motif de ligne
# n'est soumise à aucun motif d'unité, son sort est fixé par ses motifs de
# ligne ; avec f, l les indices du premier et du dernier inventaire où
# l'unité est présente, M le nombre d'inventaires :
#   - dates de naissance, sexes (modalités H, F) ou segments (segment employé
#     par la segmentation) différents au sein de l'unité ;
#   - evenements_multiples : au moins deux lignes d'événement ;
#   - presence_apres_evenement : une date d'inventaire de l'unité >= une de
#     ses dates de sortie (inventaire après le décès, remise en vigueur) ;
#   - presence_interrompue : l'unité manque à un inventaire entre f et l ;
#   - sortie_non_renseignee : l < M sans ligne d'événement ;
#   - evenement_apres_absence : l < M et une date de sortie > t_{l+1}.
#   donnees, roles : data.frame et vecteur nommé rôle -> colonne ;
#   p              : valeurs validées (valeurs_parametres_donnees()), dont
#                    p$inventaires (grille du plan canonique, requise) ;
#   segmentation   : sous-ensemble de c("sexe", "segment"), vide admis.
# Rend list(motifs, lignes_propagation, unites_propagation) : motifs, liste
# nommée code -> numéros de ligne (1-based, croissants), dans l'ordre
# MOTIFS_LIGNES_STOCKS puis MOTIFS_UNITE_STOCKS (integer(0) pour un motif
# sans objet) ; lignes_propagation, numéros (1-based, croissants) des lignes
# sans motif de ligne d'une unité porteuse (amendement A1, § 7.6, étape 4 :
# calculées sans le plan ; preparer_donnees() les exclut avec les lignes des
# motifs exclus, tout motif de ligne présent étant alors déclaré) ;
# unites_propagation, nombre d'identifiants distincts parmi ces lignes.
evaluer_motifs_stocks <- function(donnees, roles, p, segmentation) {
  if (is.null(p$inventaires))
    stop("evaluer_motifs_stocks() : grille des inventaires absente de p (erreur de programmation).", call. = FALSE)
  grille <- as.numeric(unclass(p$inventaires)); M <- length(grille)
  n <- nrow(donnees)
  col <- function(role) if (role %in% names(roles)) donnees[[roles[[role]]]] else NULL
  jours <- function(role) as.numeric(unclass(col(role)))
  vrai <- function(x) !is.na(x) & x
  role_id <- identifiant_stocks(roles)
  id <- col(role_id)
  tl <- as.character(col("type_ligne"))
  inv <- vrai(tl == "inventaire"); evt <- vrai(tl == "evenement")
  nai <- jours("date_naissance"); dinv <- jours("date_inventaire"); sor <- jours("date_sortie")
  cause <- as.character(col("cause_sortie"))
  m <- stats::setNames(rep(list(logical(n)), length(MOTIFS_LIGNES_STOCKS)), MOTIFS_LIGNES_STOCKS)
  m$type_ligne_manquant <- is.na(tl)
  m$type_ligne_hors_modalites <- !is.na(tl) & !(tl %in% MODALITES_TYPE_LIGNE)
  m[[paste0(role_id, "_manquant")]] <- is.na(id)
  m$naissance_manquante <- is.na(nai)
  sexe <- col("sexe")
  if (!is.null(sexe)) {
    sexe <- as.character(sexe)
    m$sexe_manquant <- is.na(sexe)
    m$sexe_hors_modalites <- !is.na(sexe) & !(sexe %in% MODALITES_SEXE)
  }
  segment <- col("segment")
  if (!is.null(segment)) m$segment_manquant <- is.na(segment)
  an_nai <- col("annee_naissance")
  if (!is.null(an_nai))
    m$annee_naissance_incoherente <- vrai(an_nai != composantes_date(nai)$an)
  d_ligne <- rep(NA_real_, n); d_ligne[inv] <- dinv[inv]; d_ligne[evt] <- sor[evt]
  m$age_aberrant <- vrai(age_revolu(nai, d_ligne) >= AGE_ABERRANT)
  m$inventaire_sans_date <- inv & is.na(dinv)
  m$inventaire_hors_grille <- inv & !is.na(dinv) & !(dinv %in% grille)
  m$inventaire_avec_sortie <- inv & (!is.na(sor) | !is.na(cause))
  m$naissance_apres_inventaire <- inv & vrai(nai > dinv)
  # Clé exacte (identifiant, date), comme cellule_dupliquee (lignes_cle_dupliquee()).
  m$inventaire_duplique <- lignes_cle_dupliquee(list(id, dinv), inv & !is.na(id) & !is.na(dinv))
  m$sortie_manquante <- evt & is.na(sor) & !is.na(cause) & cause != "en_cours"
  m$cause_manquante <- evt & is.na(cause)
  m$cause_hors_modalites <- evt & !is.na(cause) & !(cause %in% MODALITES_CAUSE)
  m$cause_en_cours <- evt & vrai(cause == "en_cours")
  m$evenement_avec_date_inventaire <- evt & !is.na(dinv)
  m$naissance_apres_sortie <- evt & vrai(nai > sor)

  u <- stats::setNames(rep(list(logical(n)), length(MOTIFS_UNITE_STOCKS)), MOTIFS_UNITE_STOCKS)
  # Amendement A1 (§ 7.6, étapes 2 à 4) : unités porteuses (identifiants
  # valides d'au moins une ligne à motif de ligne ; même identifiant :
  # égalité exacte, par match()) ; lignes propres, celles des unités non
  # porteuses ; lignes de propagation, lignes sans motif de ligne d'une unité
  # porteuse. Une ligne sans identifiant valide porte <id>_manquant : elle
  # n'est dans aucun des deux ensembles et n'écarte qu'elle-même.
  a_motif <- Reduce(`|`, m)
  porteurs <- unique(id[a_motif & !is.na(id)])
  de_porteuse <- !a_motif & id %in% porteurs
  propres <- which(!a_motif & !de_porteuse)
  propagation <- which(de_porteuse)
  g <- match(id[propres], unique(id[propres]))
  G <- max(0L, g)
  marquer <- function(groupes) {
    x <- logical(n)
    x[propres[g %in% groupes]] <- TRUE
    x
  }
  u$unite_naissance_incoherente <- marquer(groupes_heterogenes(g, nai[propres]))
  if (!is.null(sexe)) {
    # Lignes propres : sexe dans MODALITES_SEXE (sexe_manquant et
    # sexe_hors_modalites sont des motifs de ligne).
    u$unite_sexe_incoherent <- marquer(groupes_heterogenes(g, sexe[propres]))
  }
  if (!is.null(segment) && "segment" %in% segmentation)
    u$unite_segment_incoherent <- marquer(groupes_heterogenes(g, as.character(segment[propres])))
  fu <- faits_unites_stocks(g, inv[propres], evt[propres], match(dinv[propres], grille), sor[propres],
                            cause[propres], G)
  a_inv <- fu$n_inventaires > 0L
  a_evt <- fu$n_evenements > 0L
  avant_fin <- a_inv & fu$l < M
  suivant <- rep(NA_real_, G); suivant[avant_fin] <- grille[fu$l[avant_fin] + 1L]
  u$evenements_multiples <- marquer(which(fu$n_evenements >= 2L))
  u$presence_apres_evenement <- marquer(which(a_inv & a_evt & grille[fu$l] >= fu$s_min))
  u$presence_interrompue <- marquer(which(a_inv & (fu$l - fu$f + 1L) != fu$n_inventaires))
  u$sortie_non_renseignee <- marquer(which(avant_fin & !a_evt))
  u$evenement_apres_absence <- marquer(which(avant_fin & a_evt & fu$s_max > suivant))
  list(motifs = lapply(c(m, u), which), lignes_propagation = propagation,
       unites_propagation = length(unique(id[propagation])))
}

# Observations de la recette `stocks_successifs` sur les lignes retenues
# (spécification de #180, § 4) : une observation par unité, faits établis
# sans aucune hypothèse (l'instant d'entrée d'une unité absente de
# l'inventaire précédent reste inconnu ; l'hypothèse qui le fixe relève de
# l'étape exposition, ADR 0016, point 4.2). Unité d'observation `tete` si
# l'identifiant déclaré est id_tete, `contrat` s'il est id_contrat. Unités
# rangées par identifiant (order(method = "radix") : ordre des octets pour
# une chaîne, ordre numérique pour un nombre), indépendamment de l'ordre des
# lignes reçues. Colonnes : unite (identifiant), ligne (premier numéro de
# ligne de l'unité), nb_lignes, date_naissance, sexe (déclaré), segment
# (employé par la segmentation), premier_inventaire et dernier_inventaire
# (indices f <= l dans la grille, NA sans inventaire), date_sortie et cause
# (événement, NA sans événement), intervalle (i tel que t_i < s <= t_{i+1},
# NA sans événement ou hors de ]t_1, t_M]), classe et fin :
#   - classe P0 : présente à t_1 (f = 1) ; E : vue pour la première fois à
#     t_f, f >= 2 ; ES : événement sans inventaire, t_1 < s <= t_M ; HF :
#     événement sans inventaire, s <= t_1 ou s > t_M ;
#   - fin S : événement avec t_l < s <= t_{l+1} ; C : présente à t_M, sans
#     événement ou avec un événement postérieur à t_M ; NA pour ES et HF.
# Les lignes reçues ici ne portent aucun motif (refus par défaut ou
# exclusion) : une unité incohérente (plusieurs événements, présence
# interrompue, fin introuvable) est une erreur de programmation.
# Refus inventaire_sans_ligne (§ 7.4 ; module donnees, non excluable) : une
# date de la grille sans aucune unité présente sur les lignes retenues ; le
# message distingue la date sans aucune ligne reçue (lignes dont
# date_inventaire vaut cette date, quel que soit leur type_ligne) de la date
# dont toutes les lignes reçues sont exclues.
#   lignes, numeros, roles, segmentation : comme construire_observations() ;
#   p       : valeurs validées, dont p$inventaires ;
#   donnees : lignes reçues (avant exclusions), pour le message du refus.
# Rend list(unite, observations, observation_des_lignes, releves) : releves
# porte le nombre d'unités, le décompte de chaque classe (p0, entrees,
# entres_sortis, hors_fenetre, fin_sortie, fin_censure), les unités ES par
# cause (entres_sortis_par_cause, causes de MODALITES_CAUSE hors « en_cours »),
# les unités présentes par inventaire (inventaires : date_inventaire,
# presents) et, par intervalle ]t_i, t_{i+1}] (intervalles : debut, fin,
# entrants, entres_sortis, sorties_<cause> des unités de fin S).
observations_stocks <- function(lignes, numeros, roles, segmentation, p, donnees) {
  if (is.null(p$inventaires))
    stop("observations_stocks() : grille des inventaires absente de p (erreur de programmation).", call. = FALSE)
  grille <- as.numeric(unclass(p$inventaires)); M <- length(grille)
  role_id <- identifiant_stocks(roles)
  col <- function(role, d = lignes) if (role %in% names(roles)) d[[roles[[role]]]] else NULL
  id <- col(role_id)
  tl <- as.character(col("type_ligne"))
  inv <- !is.na(tl) & tl == "inventaire"; evt <- !is.na(tl) & tl == "evenement"
  s <- as.numeric(unclass(col("date_sortie"))); cause <- as.character(col("cause_sortie"))
  k <- match(as.numeric(unclass(col("date_inventaire"))), grille)
  uid <- unique(id)
  g <- match(id, uid)
  G <- length(uid)
  fu <- faits_unites_stocks(g, inv, evt, k, s, cause, G)
  a_inv <- fu$n_inventaires > 0L
  a_evt <- fu$n_evenements > 0L
  # Unité incohérente : erreur de programmation (motifs d'unité non appliqués).
  suivant <- rep(NA_real_, G); avant_fin <- a_inv & fu$l < M
  suivant[avant_fin] <- grille[fu$l[avant_fin] + 1L]
  incoherente <- fu$n_evenements > 1L | !(a_inv | a_evt) | (!a_inv & is.na(fu$s_min)) |
    (a_inv & (fu$l - fu$f + 1L) != fu$n_inventaires) |
    (avant_fin & !(a_evt & !is.na(fu$s_min) & fu$s_min > grille[pmax(fu$l, 1L)] & fu$s_min <= suivant)) |
    (a_inv & !avant_fin & a_evt & !(fu$s_min > grille[M]))
  if (any(incoherente, na.rm = TRUE) || anyNA(incoherente))
    stop(sprintf("observations_stocks() : %d unité(s) incohérente(s) (erreur de programmation : motifs non appliqués).",
                 sum(incoherente | is.na(incoherente))), call. = FALSE)

  # Présentes par inventaire : +1 à f, -1 à l + 1.
  presents <- cumsum(tabulate(fu$f[a_inv], nbins = M + 1L) - tabulate(fu$l[a_inv] + 1L, nbins = M + 1L))[seq_len(M)]
  vides <- which(presents == 0L)
  if (length(vides)) {
    d_r <- as.numeric(unclass(col("date_inventaire", donnees)))
    recues <- vapply(grille[vides], function(t) sum(!is.na(d_r) & d_r == t),
                     integer(1L))
    dates <- citer(format(structure(grille[vides], class = "Date")))
    refuser("donnees", "inventaire_sans_ligne",
            formuler(paste("Inventaire(s) sans aucune unité présente sur les lignes retenues : %s ; la grille du plan",
                           "suppose un inventaire exhaustif à chacune de ses dates (un portefeuille vide à une date",
                           "d'inventaire n'est pas pris en charge), la construction est refusée."),
                     paste(ifelse(recues == 0L,
                                  formuler("%s (aucune ligne reçue à cette date d'inventaire)", dates),
                                  formuler("%s (%d ligne(s) reçue(s) à cette date d'inventaire, toutes exclues)", dates, recues)),
                           collapse = " ; ")),
            list(dates = structure(grille[vides], class = "Date"), lignes_recues = recues))
  }

  t1 <- grille[1L]; tM <- grille[M]
  classe <- ifelse(a_inv, ifelse(fu$f == 1L, "P0", "E"), ifelse(fu$s_min > t1 & fu$s_min <= tM, "ES", "HF"))
  fin <- rep(NA_character_, G); fin[a_inv] <- ifelse(fu$l[a_inv] == M, "C", "S")
  intervalle <- findInterval(fu$s_min, grille, left.open = TRUE)
  intervalle[is.na(fu$s_min) | intervalle == 0L | intervalle == M] <- NA_integer_

  o <- order(uid, method = "radix")
  rang <- integer(G); rang[o] <- seq_len(G)
  premiere <- match(seq_len(G), g)
  i <- premiere[o]
  as_date <- function(j) structure(as.numeric(j), class = "Date")
  observations <- data.frame(unite = id[i], ligne = numeros[i], nb_lignes = tabulate(g, nbins = G)[o],
                             stringsAsFactors = FALSE, row.names = NULL)
  observations$date_naissance <- col("date_naissance")[i]
  if ("sexe" %in% names(roles)) observations$sexe <- sous_indicer_categorie(col("sexe"), i)
  if ("segment" %in% segmentation) observations$segment <- sous_indicer_categorie(col("segment"), i)
  observations$premier_inventaire <- fu$f[o]
  observations$dernier_inventaire <- fu$l[o]
  observations$date_sortie <- as_date(fu$s_min[o])
  observations$cause <- fu$cause[o]
  observations$intervalle <- intervalle[o]
  observations$classe <- classe[o]
  observations$fin <- fin[o]

  causes <- setdiff(MODALITES_CAUSE, "en_cours")
  es <- classe == "ES"; sortie <- !is.na(fin) & fin == "S"
  intervalles <- data.frame(debut = as_date(grille[-M]), fin = as_date(grille[-1L]),
                            entrants = tabulate(fu$f[classe == "E"] - 1L, nbins = M - 1L),
                            entres_sortis = tabulate(intervalle[es], nbins = M - 1L),
                            stringsAsFactors = FALSE)
  for (x in causes)
    intervalles[[paste0("sorties_", x)]] <- tabulate(intervalle[sortie & fu$cause == x], nbins = M - 1L)
  releves <- list(unites = G, p0 = sum(classe == "P0"), entrees = sum(classe == "E"), entres_sortis = sum(es),
                  hors_fenetre = sum(classe == "HF"), fin_sortie = sum(sortie), fin_censure = sum(fin %in% "C"),
                  entres_sortis_par_cause = vapply(causes, function(x) sum(es & fu$cause == x), integer(1L)),
                  inventaires = data.frame(date_inventaire = as_date(grille), presents = presents),
                  intervalles = intervalles)
  list(unite = if (role_id == "id_tete") "tete" else "contrat", observations = observations,
       observation_des_lignes = rang[g], releves = releves)
}

# --- empilement des tables des stocks successifs (#181 ; ADR 0014, point 11) ---

# Empile la table des inventaires et celle des événements en une seule table,
# la forme que consomme la recette `stocks_successifs` (adapter de structure,
# sans calcul ; avis de forme (0'') de B15, G10 ; ADR 0014, point 11, et son
# annotation du 08/11/2026, (d)). Exportée : appelée par la couche de lecture
# et par l'application avant construire_table(), pour que la règle
# d'empilement ne soit pas réécrite par chaque appelant.
#   inventaires  : data.frame des lignes d'inventaire ;
#   evenements   : data.frame des lignes d'événement ;
#   colonne_type : nom de la colonne écrite (chaîne unique, non vide, sans
#                  attribut, d'encodage autre que « bytes »), à déclarer au
#                  rôle type_ligne : « inventaire » pour une ligne de
#                  `inventaires`, « evenement » pour une ligne de `evenements`
#                  (MODALITES_TYPE_LIGNE ; déclaration par la table d'origine,
#                  jamais déduction), en character.
# Rend un data.frame nu (classe data.frame seule, row.names automatiques) :
# lignes de `inventaires` puis de `evenements`, chacune dans l'ordre reçu ;
# colonnes de `inventaires` dans leur ordre, puis celles des seuls
# `evenements` dans le leur, puis `colonne_type`. Une colonne commune (même
# nom) garde ses valeurs ; une colonne absente d'une table y est complétée par
# des NA de la classe de la colonne présente (sous-indiçage [NA] de cette
# colonne : complétion de structure, aucune ligne créée). Aucune conversion :
# une colonne commune doit avoir même type de stockage (typeof) et mêmes
# attributs conservés par le sous-indiçage (attributes(x[0L]), names
# retiré : classe, niveaux d'un facteur, fuseau d'un POSIXct, unité d'un
# difftime) ; ses valeurs sont posées par [<- sur la colonne des inventaires
# complétée. Les attributs de colonne que le sous-indiçage ne conserve pas
# (dont « label ») et la classe de table (tibble…) ne sont pas recopiés.
# Seules les colonnes atomiques sans dimension, hors raw, sont admises (une
# colonne raw n'a pas de NA : la complétion écrirait 00). Refus (module
# donnees), dans l'ordre :
#   empilement_table_invalide        : `inventaires` ou `evenements` n'est pas
#                                      un data.frame (le moteur ne lit pas de
#                                      fichier) ;
#   empilement_colonne_type_invalide : `colonne_type` hors de la forme
#                                      ci-dessus ;
#   empilement_noms_invalides        : nom de colonne NA, vide, en double ou
#                                      d'encodage « bytes » dans une table
#                                      (les colonnes communes s'apparient par
#                                      leur nom) ; première table en défaut ;
#   empilement_colonne_non_atomique  : colonne non atomique, munie d'une
#                                      dimension ou de type raw (liste,
#                                      matrice, tableau, data.frame, raw) ;
#                                      première table en défaut ;
#   empilement_colonne_type_presente : `colonne_type` est déjà le nom d'une
#                                      colonne (elle serait écrasée) ;
#   empilement_colonnes_discordantes : colonne(s) commune(s) de type de
#                                      stockage ou d'attributs conservés
#                                      différents, toutes citées.
empiler_stocks <- function(inventaires, evenements, colonne_type) {
  tables <- list(inventaires = inventaires, evenements = evenements)
  non_df <- names(tables)[!vapply(tables, is.data.frame, logical(1L))]
  if (length(non_df))
    refuser("donnees", "empilement_table_invalide",
            formuler(paste("Empilement des stocks successifs : un data.frame est attendu pour %s (classe(s) reçue(s) : %s) ;",
                           "le moteur ne lit pas de fichier."),
                     liste_guillemets(non_df),
                     paste(vapply(tables[non_df], function(x) paste(citer(class(x)), collapse = "/"), character(1L)),
                           collapse = " ; ")),
            list(table = non_df, classe = lapply(tables[non_df], class)))
  if (!chaine_unique(colonne_type) || Encoding(colonne_type) == "bytes")
    refuser("donnees", "empilement_colonne_type_invalide",
            formuler(paste("Empilement des stocks successifs : colonne_type, nom de la colonne du type de ligne",
                           "(rôle type_ligne), doit être une chaîne unique, non vide, sans attribut, d'encodage",
                           "autre que « bytes ».")),
            list(colonne_type = colonne_type))
  for (nom_table in names(tables)) {
    n <- attr(tables[[nom_table]], "names", exact = TRUE)
    mal <- is.na(n) | !nzchar(n) | Encoding(n) == "bytes"
    mal <- mal | n %in% n[!mal][duplicated(n[!mal])]
    if (any(mal))
      refuser("donnees", "empilement_noms_invalides",
              formuler(paste("Empilement des stocks successifs : table « %s », nom(s) de colonne NA, vide(s), en double",
                             "ou d'encodage « bytes » : %s ; les colonnes communes s'apparient par leur nom, sans",
                             "deviner."),
                       citer(nom_table), liste_guillemets(unique(n[mal]))),
              list(table = nom_table, colonne = unique(n[mal])))
  }
  for (nom_table in names(tables)) {
    tab <- tables[[nom_table]]
    n <- attr(tab, "names", exact = TRUE)
    mal <- n[!vapply(seq_along(n), function(j) {
      x <- .subset2(tab, j)
      is.atomic(x) && is.null(attr(x, "dim", exact = TRUE)) && !identical(typeof(x), "raw")
    }, logical(1L))]
    if (length(mal))
      refuser("donnees", "empilement_colonne_non_atomique",
              formuler(paste("Empilement des stocks successifs : table « %s », colonne(s) non atomique(s), munie(s)",
                             "d'une dimension ou de type raw (liste, matrice, tableau, data.frame, raw) : %s ;",
                             "l'empilement ne complète que des colonnes atomiques sans dimension, hors raw."),
                       citer(nom_table), liste_guillemets(mal)),
              list(table = nom_table, colonne = mal))
  }
  ni <- attr(inventaires, "names", exact = TRUE)
  ne <- attr(evenements, "names", exact = TRUE)
  avec_type <- names(tables)[c(colonne_type %in% ni, colonne_type %in% ne)]
  if (length(avec_type))
    refuser("donnees", "empilement_colonne_type_presente",
            formuler(paste("Empilement des stocks successifs : la colonne « %s » (colonne_type) existe déjà dans %s ;",
                           "elle serait écrasée : choisir un autre nom ou retirer cette colonne."),
                     citer(colonne_type), liste_guillemets(avec_type)),
            list(colonne = colonne_type, table = avec_type))
  communes <- ni[ni %in% ne]
  # Noms des attributs conservés par le sous-indiçage (attributes(x[0L]),
  # names retiré) qui diffèrent entre les deux colonnes de nom k.
  attributs_differents <- function(k) {
    aa <- attributes(.subset2(inventaires, match(k, ni))[0L]); aa$names <- NULL
    ab <- attributes(.subset2(evenements, match(k, ne))[0L]); ab$names <- NULL
    u <- union(names(aa), names(ab))
    u[!vapply(u, function(m) identical(aa[[m]], ab[[m]]), logical(1L))]
  }
  discordantes <- communes[vapply(communes, function(k)
    !identical(typeof(.subset2(inventaires, match(k, ni))), typeof(.subset2(evenements, match(k, ne)))) ||
      length(attributs_differents(k)) > 0L, logical(1L))]
  if (length(discordantes)) {
    desc <- vapply(discordantes, function(k) {
      a <- .subset2(inventaires, match(k, ni)); b <- .subset2(evenements, match(k, ne))
      d <- attributs_differents(k)
      formuler("« %s » (inventaires : classe %s, stockage %s ; événements : classe %s, stockage %s%s)", citer(k),
               paste(citer(class(a)), collapse = "/"), citer(typeof(a)), paste(citer(class(b)), collapse = "/"),
               citer(typeof(b)),
               if (length(d)) formuler(" ; attribut(s) différent(s) : %s", liste_guillemets(d)) else "")
    }, character(1L), USE.NAMES = FALSE)
    refuser("donnees", "empilement_colonnes_discordantes",
            formuler(paste("Empilement des stocks successifs : colonne(s) commune(s) discordante(s) : %s ; une colonne",
                           "commune doit avoir même classe, même type de stockage et mêmes attributs conservés par le",
                           "sous-indiçage (niveaux d'un facteur, fuseau d'un POSIXct, unité d'un difftime) ; aucune",
                           "conversion n'est faite."),
                     paste(desc, collapse = " ; ")),
            list(colonne = discordantes))
  }
  n_i <- nrow(inventaires); n_e <- nrow(evenements)
  noms <- c(ni, ne[!(ne %in% ni)])
  colonnes <- lapply(noms, function(k) {
    ja <- match(k, ni); jb <- match(k, ne)
    if (is.na(jb)) return(.subset2(inventaires, ja)[c(seq_len(n_i), rep(NA_integer_, n_e))])
    b <- .subset2(evenements, jb)
    if (is.na(ja)) return(b[c(rep(NA_integer_, n_i), seq_len(n_e))])
    v <- .subset2(inventaires, ja)[c(seq_len(n_i), rep(NA_integer_, n_e))]
    v[n_i + seq_len(n_e)] <- b
    v
  })
  structure(c(colonnes, list(rep(MODALITES_TYPE_LIGNE, c(n_i, n_e)))), names = c(noms, colonne_type),
            row.names = .set_row_names(n_i + n_e), class = "data.frame")
}

# --- fusion par tête (A.1, « Unité d'observation et fusion par tête ») --------

# Fusionne les intervalles [entree, fin) des lignes d'une même tête (g :
# entier de tête ; fin = +Inf pour une ligne en cours) : deux intervalles
# sont fusionnés si e_suivant <= fin_courante (chevauchement ou contiguïté
# exacte) ; aucune tolérance d'un jour. Rend list(ordre, composante, debut,
# fin, premiere, releves) : `ordre` trie les lignes par (tête, entrée) ;
# `composante[i]` est la composante de la i-ème ligne triée.
fusionner_intervalles <- function(g, entree, fin) {
  n <- length(g)
  o <- order(g, entree, method = "radix")
  gs <- g[o]; es <- entree[o]; fs <- fin[o]
  prem <- !duplicated(gs)
  if (!n) return(list(ordre = o, composante = integer(0), debut = numeric(0), fin = numeric(0),
                      premiere = integer(0),
                      releves = list(chevauchements = 0L, contiguites = 0L, disjoints = 0L,
                                     ecarts_un_jour = 0L)))
  # Maximum cumulé des fins par tête, en un seul cummax : +Inf est remplacé
  # par `haut` (> toute date), et chaque tête est décalée d'une largeur qui
  # excède l'étendue des valeurs (têtes triées croissantes).
  finies <- c(es, fs[is.finite(fs)])
  bas <- min(finies); haut <- max(finies) + 1
  largeur <- haut - bas + 1
  # Garde : le produit (numéro de tête maximal) x (étendue des dates) doit rester
  # sous 2^52 pour que le décalage par tête soit exact. Depuis #19, les dates
  # sont dans [0001-01-01, 9999-12-31] (étendue au plus 3 652 060 jours) : la
  # garde n'est atteignable par construire_table() qu'au-delà d'environ
  # 1,23e9 têtes (2^52 / 3 652 060), sous .Machine$integer.max ; elle reste
  # atteignable par une étendue de dates plus grande si cette fonction est appelée
  # directement.
  if (max(gs) * largeur >= 2^52)
    refuser("donnees", "dates_hors_plage",
            formuler(paste("Étendue des dates d'entrée et de sortie de %.0f jours pour %d têtes : leur produit",
                           "dépasse 2^52, hors de la plage que la fusion par tête calcule exactement ;",
                           "vérifier les dates et le nombre de têtes."),
                     largeur, max(gs)),
            list(etendue_jours = largeur, tetes = max(gs)))
  fc <- fs; fc[!is.finite(fc)] <- haut
  decalage <- (gs - 1) * largeur
  cm <- cummax(decalage + (fc - bas)) - decalage + bas
  prec <- c(NA, cm[-n]); prec[prem] <- NA
  nouvelle <- prem | es > prec
  composante <- cumsum(nouvelle)
  dern <- !duplicated(composante, fromLast = TRUE)
  fin_comp <- cm[dern]; fin_comp[fin_comp == haut] <- Inf
  suite <- !prem
  list(ordre = o, composante = composante, debut = es[nouvelle], fin = fin_comp,
       premiere = o[nouvelle],
       releves = list(chevauchements = sum(suite & es < prec),
                      contiguites = sum(suite & es == prec),
                      disjoints = sum(suite & es > prec),
                      ecarts_un_jour = sum(suite & es - prec == 1)))
}

# --- fonction d'étape `donnees` -----------------------------------------------

# Étape `donnees` du plan : contrôle des rôles et des lignes, refus par
# défaut (motif `ligne_invalide`), exclusions déclarées, unité
# d'observation, fusion par tête, relevés et avertissements.
#   donnees, roles     : data.frame et vecteur nommé rôle -> colonne ;
#   p                  : valeurs validées de la transition, de la période
#                        (fin incluse, A.0, T4) et du délai
#                        (valeurs_parametres_donnees()) ;
#   segmentation       : sous-ensemble de c("sexe", "segment"), vide admis ;
#   exclusions         : codes de motif exclus, vide admis ;
#   recette            : recette validée (deduire_recette()) ; deux
#                        aiguillages sur son nom (ADR 0016, point 6.2) :
#                        évaluation des motifs (evaluer_motifs(),
#                        evaluer_motifs_agregats(), evaluer_motifs_stocks())
#                        et construction des observations
#                        (construire_observations(), observations_agregats(),
#                        observations_stocks()), plus les avertissements
#                        propres à la recette ; squelette commun (refus par
#                        défaut, exclusions déclarées, aucune_ligne_conservee).
# Lignes de propagation (stocks successifs, amendement A1 de la
# spécification de #180, M339) : l'évaluation des motifs peut rendre
# lignes_propagation (NULL sous les autres recettes, qui restent identiques
# au bit près) ; ces lignes, sans motif, sont exclues avec les lignes des
# motifs exclus (une exclusion de ligne ne tronque jamais l'histoire d'une
# unité identifiée), n'entrent pas dans le refus par défaut et ne sont pas
# listées par motif (resultat$exclusions et resultat$lignes_exclues comptent
# les lignes qui portent le motif ; nombre_lignes_exclues les compte) ;
# relevés unites_exclues_par_propagation (avertissement si > 0) et
# lignes_exclues_par_propagation, en fin de diagnostics$releves.
# Aucun contrôle des paramètres ni des rôles (#82 ; ADR 0007, annotation du
# 17/10/2026, (c) et (d)) : construire_table() les a validés
# (controler_entrees() : valider_plan(), controler_roles()) avant toute étape.
# Rend list(parametres, resultat, diagnostics, avertissements), noms communs
# aux recettes, contenu propre à chacune ; les lignes
# conservées (resultat$lignes_conservees) sont celles de l'entrée, inchangées
# (`donnees` lui-même s'il n'y a aucune exclusion) ;
# resultat$observation_des_lignes donne, pour chaque ligne conservée, le
# numéro de son observation (composante fusionnée ou contrat ; la cellule
# elle-même sous `agregats`).
preparer_donnees <- function(donnees, roles, p, segmentation, exclusions, recette) {
  n <- nrow(donnees)
  if (!n) refuser("donnees", "base_vide", formuler("La base d'observation ne contient aucune ligne."))
  ev <- switch(recette$recette,
    lignes_dates_completes = evaluer_motifs(donnees, roles, p$debut, p$fin, p$borne, segmentation),
    agregats = evaluer_motifs_agregats(donnees, roles, p),
    stocks_successifs = evaluer_motifs_stocks(donnees, roles, p, segmentation),
    stop(sprintf("preparer_donnees() : recette « %s » sans évaluation des motifs (erreur de programmation).",
                 recette$recette), call. = FALSE))
  motifs <- ev$motifs
  nombre <- lengths(motifs)

  # Refus par défaut : toute ligne qui porte un motif non déclaré (même si
  # elle en porte aussi un déclaré).
  non_declares <- names(motifs)[nombre > 0L & !(names(motifs) %in% exclusions)]
  if (length(non_declares)) {
    lignes <- motifs[non_declares]
    premieres <- lapply(lignes, utils::head, NB_LIGNES_CITEES)
    total <- length(unique(unlist(lignes, use.names = FALSE)))
    refuser("donnees", "ligne_invalide",
            formuler(paste("Lignes invalides, sans règle d'exclusion déclarée pour leur motif",
                           "(E2.7) : %s. Total : %d ligne(s) distincte(s)."),
                     paste(formuler("« %s » : %d ligne(s), premières lignes %s%s", non_declares,
                                    lengths(lignes),
                                    vapply(premieres, paste, character(1L), collapse = ", "),
                                    ifelse(lengths(lignes) > NB_LIGNES_CITEES, " et suivantes", "")),
                           collapse = " ; "),
                     total),
            list(decompte = lengths(lignes), premieres_lignes = premieres, lignes = lignes,
                 total_lignes = total))
  }

  # Lignes exclues : lignes des motifs exclus et lignes de propagation (tout
  # motif de ligne présent est ici déclaré : le refus par défaut a refusé
  # sinon).
  exclues <- sort(unique(c(unlist(motifs[exclusions], use.names = FALSE), ev$lignes_propagation)))
  if (is.null(exclues)) exclues <- integer(0)
  conservees <- if (length(exclues)) donnees[-exclues, , drop = FALSE] else donnees
  numeros <- setdiff(seq_len(n), exclues)
  if (!length(numeros))
    refuser("donnees", "aucune_ligne_conservee",
            formuler("Toutes les lignes (%d) sont écartées par les règles d'exclusion déclarées (%s).",
                     n, liste_guillemets(exclusions[lengths(motifs[exclusions]) > 0L])),
            list(lignes_exclues = length(exclues)))

  # Unité d'observation (E2.4, M46) et observations.
  ob <- switch(recette$recette,
    lignes_dates_completes = construire_observations(conservees, numeros, roles, segmentation, p),
    agregats = observations_agregats(conservees, numeros, roles, segmentation, motifs, exclusions, n, donnees),
    stocks_successifs = observations_stocks(conservees, numeros, roles, segmentation, p, donnees),
    stop(sprintf("preparer_donnees() : recette « %s » sans observations (erreur de programmation).",
                 recette$recette), call. = FALSE))
  unite <- ob$unite; observations <- ob$observations; releves <- ob$releves
  if (!is.null(ev$lignes_propagation))
    releves <- c(releves, list(unites_exclues_par_propagation = as.integer(ev$unites_propagation),
                               lignes_exclues_par_propagation = length(ev$lignes_propagation)))

  # Avertissements (ADR 0007, point 6.5).
  avert <- avertissements_vides()
  # Unité contrat sous une transition qui porte sur la tête : aucun id_tete
  # déclaré (construire_observations()).
  if (p$transition$etat_de == "tete" && unite == "contrat")
    avert <- avertir(avert, "donnees", "unite_contrat",
                     paste("Unité d'observation : contrat (aucun identifiant de tête déclaré) ;",
                           "une tête titulaire de plusieurs contrats est comptée plusieurs fois,",
                           "ce qui surestime l'exposition et peut compter plusieurs fois un même décès."),
                     nrow(observations))
  # Unité non déterminée (agrégats, Q4) : texte selon le porteur de la
  # transition (avis d'actuary du 08/11/2026 sur #169, point 3).
  if (unite == "non_determinee")
    avert <- avertir(avert, "donnees", "unite_agregat",
                     UNITE_AGREGAT[[p$transition$etat_de]], nrow(observations))
  # Texte selon la recette (aiguillage admis dans ce module, ADR 0016,
  # point 4.4) : sous `agregats`, les cellules de même clé sont sommées sur
  # les modalités du rôle (texte visé par actuary).
  for (s in setdiff(intersect(ROLES_SEGMENTATION, names(roles)), segmentation))
    avert <- avertir(avert, "donnees", "role_non_employe",
                     switch(recette$recette,
                       agregats = sprintf(paste("Rôle « %s » déclaré, non employé par la segmentation du plan :",
                                                "contrôlé (motifs, clé des cellules), puis cellules de même clé",
                                                "sommées sur ses modalités à l'étape exposition."), s),
                       sprintf("Rôle « %s » déclaré, non employé par la segmentation du plan (contrôlé seulement).", s)),
                     1)
  for (x in exclusions[lengths(motifs[exclusions]) > 0L])
    avert <- avertir(avert, "donnees", "exclusion",
                     sprintf("Exclusion déclarée « %s » : %d ligne(s) écartée(s).", x, length(motifs[[x]])),
                     length(motifs[[x]]))
  # Propagation d'une exclusion de ligne (amendement A1, § 7.5), juste après
  # les avertissements `exclusion` (texte du mainteneur et d'actuary).
  if (!is.null(ev$lignes_propagation) && releves$unites_exclues_par_propagation > 0L)
    avert <- avertir(avert, "donnees", "unites_exclues_par_propagation",
                     sprintf(paste("Relevé : %d unité(s) écartée(s) par propagation d'une exclusion de ligne (%d ligne(s) sans motif,",
                                   "écartée(s) avec la ligne exclue de leur unité, pour ne pas tronquer son histoire)."),
                             releves$unites_exclues_par_propagation, releves$lignes_exclues_par_propagation),
                     releves$unites_exclues_par_propagation)
  avert <- switch(recette$recette,
    lignes_dates_completes = avertir_releves_lignes(avert, releves, unite),
    agregats = avertir_releves_agregats(avert, releves),
    stocks_successifs = avertir_releves_stocks(avert, releves))

  nature <- switch(recette$recette,
    lignes_dates_completes = rep(c("ligne", "tete"), c(length(MOTIFS_LIGNES), length(MOTIFS_TETE))),
    agregats = rep("ligne", length(motifs)),
    stocks_successifs = rep(c("ligne", "unite"), c(length(MOTIFS_LIGNES_STOCKS), length(MOTIFS_UNITE_STOCKS))))
  list(
    parametres = list(transition = p$transition$code, unite_observation = unite,
                      segmentation = segmentation, exclusions = exclusions,
                      fin_effective = structure(p$borne - 1, class = "Date")),
    resultat = list(lignes_conservees = conservees, numeros_conserves = numeros,
                    exclusions = data.frame(motif = exclusions,
                                            nombre = unname(lengths(motifs[exclusions])),
                                            stringsAsFactors = FALSE),
                    lignes_exclues = motifs[exclusions],
                    nombre_lignes_exclues = length(exclues),
                    unite_observation = unite, observations = observations,
                    observation_des_lignes = ob$observation_des_lignes),
    diagnostics = list(motifs = data.frame(motif = names(motifs),
                                           nature = nature,
                                           nombre = unname(nombre),
                                           exclu = names(motifs) %in% exclusions,
                                           stringsAsFactors = FALSE),
                       lignes_porteuses = length(unique(unlist(motifs, use.names = FALSE))),
                       releves = releves),
    avertissements = avert
  )
}

# Textes de l'avertissement unite_agregat (agrégats, Q4), par porteur de la
# transition (TRANSITIONS$etat_de) ; textes visés par actuary (avis du
# 08/11/2026 sur #169, point 3).
UNITE_AGREGAT <- c(
  tete = paste("Unité d'observation : non déterminée (agrégats : rien n'indique si les effectifs comptent des",
               "têtes ou des contrats) ; si l'agrégat compte des contrats, une tête titulaire de plusieurs",
               "contrats est comptée plusieurs fois, ce qui surestime l'exposition et peut compter plusieurs fois",
               "un même décès ; les intervalles et les tests supposent des unités indépendantes et sont trop",
               "étroits en cas de dépendance positive."),
  contrat = paste("Unité d'observation : non déterminée (agrégats : rien n'indique si les effectifs comptent des",
                  "contrats ou des têtes) ; la transition porte sur le contrat, des effectifs comptés en têtes ne",
                  "lui conviennent pas ; les intervalles et les tests supposent des unités indépendantes et sont",
                  "trop étroits en cas de dépendance positive entre contrats d'une même tête."))

# Avertissements des relevés de la recette `agregats` (§ 2 et § 6.3), sur les
# cellules conservées, quand les deux expositions sont déclarées : E^i < E^c,
# E^i - E^c > D. Relevés non bloquants, jamais un refus. Rend `avert` augmenté.
avertir_releves_agregats <- function(avert, releves) {
  k <- releves$exposition_initiale_inferieure_centrale
  if (!is.na(k) && k > 0L)
    avert <- avertir(avert, "donnees", "exposition_initiale_inferieure_centrale",
                     sprintf(paste("Relevé : %d cellule(s) dont l'exposition initiale déclarée est inférieure à",
                                   "l'exposition centrale déclarée (définition « cellule » : 0 <= E^i - E^c <= D)."), k),
                     k)
  k <- releves$complement_superieur_aux_evenements
  if (!is.na(k) && k > 0L)
    avert <- avertir(avert, "donnees", "complement_superieur_aux_evenements",
                     sprintf(paste("Relevé : %d cellule(s) dont l'écart entre exposition initiale et exposition",
                                   "centrale déclarées dépasse le nombre d'événements (définition « cellule » :",
                                   "0 <= E^i - E^c <= D)."), k),
                     k)
  avert
}

# Avertissements des relevés de la recette `stocks_successifs` (#180, § 7.5),
# après celui de la propagation : unités vues pour la première fois à un
# inventaire postérieur au premier (classe E) ; unités dont la seule ligne
# est un événement hors de ]t_1, t_M] (classe HF). Le relevé des unités
# entrées et sorties entre deux inventaires (classe ES), dont le traitement
# dépend de l'hypothèse d'entrée, est averti par l'étape exposition
# (fenetres_stocks_successifs()). Relevés non bloquants. Rend `avert` augmenté.
avertir_releves_stocks <- function(avert, releves) {
  if (releves$entrees > 0L)
    avert <- avertir(avert, "donnees", "entrees_entre_inventaires",
                     sprintf(paste("Relevé : %d unité(s) vue(s) pour la première fois à un inventaire postérieur au",
                                   "premier (entrée entre deux inventaires, instant d'entrée inconnu : fixé par",
                                   "l'hypothèse d'entrée de l'étape exposition)."), releves$entrees),
                     releves$entrees)
  if (releves$hors_fenetre > 0L)
    avert <- avertir(avert, "donnees", "evenements_hors_fenetre",
                     sprintf(paste("Relevé : %d unité(s) sans ligne d'inventaire dont l'événement est daté au plus",
                                   "tard du premier inventaire ou après le dernier (hors de la période",
                                   "d'observation, sans exposition ni événement compté)."), releves$hors_fenetre),
                     releves$hors_fenetre)
  avert
}

# Avertissements des relevés de la recette `lignes_dates_completes` (fusion
# par tête, fenêtres vides, observations après décès hors période),
# extraits de preparer_donnees() à résultats identiques. Rend `avert` augmenté.
avertir_releves_lignes <- function(avert, releves, unite) {
  libelles <- c(chevauchements = "contrat(s) chevauchant le précédent d'une même tête",
                disjoints = "intervalle(s) disjoint(s) du précédent d'une même tête (trou non exposé)",
                ecarts_un_jour = paste("écart(s) d'un jour entre contrats d'une même tête (date de fin",
                                       "inclusive probable ; non comblé)"),
                fenetres_vides = "observation(s) sans intersection avec la période d'observation",
                observations_apres_deces_hors_periode = paste(
                  "tête(s) dont un contrat est observé après le premier décès de la tête, survenu",
                  "à partir de la fin effective de la période (contrôle par identifiant de tête ;",
                  "non refusé, E2.4, lecture B)"))
  # Relevés de fusion : avertis quand l'unité est la tête (ils décrivent la
  # fusion) ; pour l'unité contrat, restitués dans les diagnostics seulement.
  # Fenêtres vides et observations après décès hors période : avertis dans
  # toutes les unités.
  for (r in names(libelles))
    if (!is.na(releves[[r]]) && releves[[r]] > 0L &&
        (unite == "tete" || r %in% c("fenetres_vides", "observations_apres_deces_hors_periode")))
      avert <- avertir(avert, "donnees", r, sprintf("Relevé : %d %s.", releves[[r]], libelles[[r]]),
                       releves[[r]])
  avert
}

# --- observations de l'unité d'observation (E2.4, M46) ------------------------

# Observations de l'unité d'observation sur des lignes déjà retenues : unité
# (tête si la transition porte sur la tête et que id_tete est déclaré, sinon
# contrat), fusion par tête, relevés. Extraite de preparer_donnees() à
# résultats identiques (#69) ; l'étape importance_exclusions la rappelle sur
# le contrefactuel sans exclusions (une seule implémentation de la fusion).
#   lignes        : data.frame des lignes retenues (colonnes de l'entrée) ;
#   numeros       : numéros de ces lignes dans l'entrée (1-based, croissants) ;
#   roles         : vecteur nommé rôle -> colonne ;
#   segmentation  : sous-ensemble de c("sexe", "segment"), vide admis ;
#   p             : valeurs_parametres_donnees() (transition, debut, borne).
# Rend list(unite, observations, observation_des_lignes, releves) : champs
# unite_observation, observations et observation_des_lignes du résultat de
# preparer_donnees() ; releves, ses diagnostics$releves.
construire_observations <- function(lignes, numeros, roles, segmentation, p) {
  col <- function(role) if (role %in% names(roles)) lignes[[roles[[role]]]] else NULL
  ent <- as.numeric(unclass(col("date_entree"))); sor <- as.numeric(unclass(col("date_sortie")))
  cause <- as.character(col("cause_sortie"))
  fin <- sor; fin[cause == "en_cours"] <- Inf
  id_t <- col("id_tete")
  unite <- if (p$transition$etat_de == "tete" && !is.null(id_t)) "tete" else "contrat"
  releves <- list(tetes = NA_integer_, tetes_multi_contrats = NA_integer_,
                  chevauchements = NA_integer_, contiguites = NA_integer_,
                  disjoints = NA_integer_, ecarts_un_jour = NA_integer_)
  fusion <- NULL
  hors_periode <- NA_integer_
  if (!is.null(id_t)) {
    g <- match(id_t, unique(id_t))
    fusion <- fusionner_intervalles(g, ent, fin)
    tab <- tabulate(g, nbins = max(0L, g))  # nbins explicite : 0 tête si 0 ligne
    releves <- c(list(tetes = length(tab), tetes_multi_contrats = sum(tab > 1L)),
                 lapply(fusion$releves, as.integer))
    # Têtes observées après un premier décès s_d >= borne (lecture B d'E2.4),
    # quelle que soit l'unité (M48), sur les seules lignes reçues (dans
    # preparer_donnees(), les lignes conservées) : une tête écartée par une
    # exclusion déclarée n'est pas comptée ; une tête dont une ligne
    # seulement est exclue l'est sur ses lignes conservées.
    dt <- deces_par_tete(g, sor, cause)
    hors_periode <- sum(dt$apres & dt$premier_deces >= p$borne)
  }
  as_date <- function(j) { j[!is.finite(j)] <- NA; structure(j, class = "Date") }
  if (unite == "tete") {
    k <- fusion$composante
    dern <- !duplicated(k, fromLast = TRUE)
    o <- fusion$ordre
    fin_k <- fusion$fin[k]
    deces <- tabulate(k[cause[o] == "deces" & fin[o] == fin_k], nbins = length(fusion$debut)) > 0L
    cause_k <- rep("censure", length(deces))
    cause_k[is.infinite(fusion$fin)] <- "en_cours"
    cause_k[deces] <- "deces"
    composante_ligne <- integer(length(o)); composante_ligne[o] <- k
    i <- fusion$premiere
    # row.names = NULL (ici et dans l'unité contrat) : data.frame() prendrait
    # les noms de lignes dans l'attribut names d'une colonne d'identifiant
    # (list2DF), que l'empreinte du résultat refuserait (#67).
    observations <- data.frame(unite = id_t[i], ligne = numeros[i],
                               nb_lignes = tabulate(k, nbins = length(fusion$debut)),
                               stringsAsFactors = FALSE, row.names = NULL)
    observations$date_naissance <- col("date_naissance")[i]
  } else {
    i <- seq_along(ent)
    id_c <- col("id_contrat")
    # Sous-indiçage [i], comme dans l'unité tête : les colonnes recopiées
    # perdent les attributs de colonne de l'entrée (label…), que l'empreinte du
    # résultat refuserait ; valeurs, type et classe inchangés (#67, M71).
    observations <- data.frame(unite = if (is.null(id_c)) numeros else id_c[i],
                               ligne = numeros, nb_lignes = rep(1L, length(i)),
                               stringsAsFactors = FALSE, row.names = NULL)
    if (!is.null(id_t)) observations$id_tete <- id_t[i]  # rééchantillonnage par têtes (E2.14)
    observations$date_naissance <- col("date_naissance")[i]
    composante_ligne <- seq_along(i)
  }
  # Sexe s'il est déclaré ; segment s'il est employé par la segmentation ;
  # type d'origine conservé (sous_indicer_categorie()).
  if ("sexe" %in% names(roles)) observations$sexe <- sous_indicer_categorie(col("sexe"), i)
  if ("segment" %in% segmentation) observations$segment <- sous_indicer_categorie(col("segment"), i)
  if (unite == "tete") {
    observations$debut <- as_date(fusion$debut); observations$fin <- as_date(fusion$fin)
    observations$cause <- cause_k
  } else {
    observations$debut <- as_date(ent); observations$fin <- as_date(fin)
    observations$cause <- cause
  }
  ofin <- as.numeric(observations$fin); ofin[is.na(ofin)] <- Inf
  releves$fenetres_vides <- sum(pmax(as.numeric(observations$debut), p$debut) >= pmin(ofin, p$borne))
  releves$observations <- nrow(observations)
  releves$observations_apres_deces_hors_periode <- hors_periode

  list(unite = unite, observations = observations, observation_des_lignes = composante_ligne,
       releves = releves)
}

# --- table de référence (A.1 ; ADR 0007, point 7) ------------------------------

STATUTS_TABLE <- c("synthetique", "embarquee", "importee")
SEXES_TABLE <- c("H", "F", "ensemble")
FORMES_ORIGINE <- c("qx", "lx")

# Construit une table de référence (classe S3 `table_reference` sur un
# data.frame long : age entier, sexe, qx, et annee entier si fournie) et la
# contrôle ; refuse (module donnees) au lieu de corriger. Lignes rendues dans
# l'ordre canonique (sexe, annee, age), row.names automatiques.
#   x            : data.frame (age, sexe, qx [, annee]) ;
#   nom, source  : chaînes non vides (source : arrêté, URL, fichier de
#                  provenance) ;
#   statut       : synthetique, embarquee ou importee ;
#   parametres   : liste non vide des paramètres pour une table synthétique,
#                  NULL sinon ;
#   forme_origine: qx ou lx (forme de la transcription).
table_reference <- function(x, nom, statut, source, parametres = NULL, forme_origine = "qx") {
  if (!chaine_unique(nom))
    refuser("donnees", "table_nom_manquant", formuler("La table de référence doit avoir un nom (chaîne non vide, sans attribut)."))
  # Nom cité dans les messages : règle neutraliser_citation() (R/outils.R ;
  # #111, #94) ; `nom` lui-même reste celui de l'objet.
  nom_cite <- citer(nom)
  if (!chaine_unique(statut) || !(statut %in% STATUTS_TABLE))
    refuser("donnees", "table_statut_inconnu",
            formuler("Table « %s » : statut (chaîne sans attribut) à prendre dans %s.", nom_cite, liste_guillemets(STATUTS_TABLE)),
            list(statut = statut))
  if (!chaine_unique(source))
    refuser("donnees", "table_source_manquante",
            formuler("Table « %s » : la source (arrêté, URL, fichier de provenance ; chaîne non vide, sans attribut) est requise.", nom_cite))
  if (statut == "synthetique" && (!is.list(parametres) || !length(parametres)))
    refuser("donnees", "table_parametres_manquants",
            formuler("Table synthétique « %s » : ses paramètres (liste non vide) sont requis.", nom_cite))
  if (statut != "synthetique" && !is.null(parametres))
    refuser("donnees", "table_parametres_inattendus",
            formuler("Table « %s » de statut « %s » : aucun paramètre de loi attendu.", nom_cite, citer(statut)))
  if (!chaine_unique(forme_origine) || !(forme_origine %in% FORMES_ORIGINE))
    refuser("donnees", "table_forme_inconnue",
            formuler("Table « %s » : forme d'origine (chaîne sans attribut) à prendre dans %s.", nom_cite, liste_guillemets(FORMES_ORIGINE)))
  if (!is.data.frame(x))
    refuser("donnees", "table_mal_formee", formuler("Table « %s » : un data.frame est attendu.", nom_cite))
  colonnes <- c("age", "sexe", "qx", if ("annee" %in% names(x)) "annee")
  manquantes <- setdiff(c("age", "sexe", "qx"), names(x))
  if (length(manquantes))
    refuser("donnees", "table_colonne_manquante",
            formuler("Table « %s » : colonne(s) manquante(s) %s.", nom_cite, liste_guillemets(manquantes)),
            list(colonne = manquantes))
  en_trop <- setdiff(names(x), colonnes)
  if (length(en_trop))
    refuser("donnees", "table_colonne_inconnue",
            formuler("Table « %s » : colonne(s) inconnue(s) %s (admises : age, sexe, qx, annee).",
                     nom_cite, liste_guillemets(en_trop)),
            list(colonne = en_trop))
  if (!nrow(x)) refuser("donnees", "table_vide", formuler("Table « %s » : aucune ligne.", nom_cite))
  mal_typees <- c(
    age = !type_conforme(x$age, "entier_positif") || anyNA(x$age),
    sexe = !type_conforme(x$sexe, "categorie") || anyNA(x$sexe),
    qx = !(is.double(x$qx) && !is.object(x$qx)),
    annee = "annee" %in% names(x) && (!type_conforme(x$annee, "entier") || anyNA(x$annee)))
  if (any(mal_typees))
    refuser("donnees", "table_colonne_mal_typee",
            formuler("Table « %s » : colonne(s) mal typée(s) ou avec valeur manquante %s (age : entier >= 0 ; annee : entier ; sexe : chaîne ; qx : double).",
                     nom_cite, liste_guillemets(names(mal_typees)[mal_typees])),
            list(colonne = names(mal_typees)[mal_typees]))
  sexe <- as.character(x$sexe)
  hors <- which(!(sexe %in% SEXES_TABLE))
  if (length(hors))
    refuser("donnees", "table_sexe_hors_modalites",
            formuler("Table « %s » : sexe hors de %s aux lignes %s.", nom_cite, liste_guillemets(SEXES_TABLE),
                     paste(utils::head(hors, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = hors))
  # « ensemble » (table sans distinction de sexe) ne coexiste pas avec H ou
  # F, dans toute la table ou, si elle a des années, au sein d'une année.
  annee0 <- if ("annee" %in% names(x)) as.integer(x$annee) else rep(0L, length(sexe))
  melange <- unique(annee0[annee0 %in% annee0[sexe == "ensemble"] & annee0 %in% annee0[sexe != "ensemble"]])
  if (length(melange))
    refuser("donnees", "table_sexes_melanges",
            formuler("Table « %s » : la modalité « ensemble » coexiste avec H ou F%s.", nom_cite,
                     if ("annee" %in% names(x)) formuler(" (années %s)", paste(utils::head(melange, NB_LIGNES_CITEES), collapse = ", ")) else ""),
            list(annees = if ("annee" %in% names(x)) melange else NULL))
  non_fini <- which(!is.finite(x$qx))
  if (length(non_fini))
    refuser("donnees", "table_qx_non_fini",
            formuler("Table « %s » : q_x manquant ou non fini aux lignes %s.", nom_cite,
                     paste(utils::head(non_fini, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = non_fini))
  hors_bornes <- which(x$qx < 0 | x$qx > 1)
  if (length(hors_bornes))
    refuser("donnees", "table_qx_hors_bornes",
            formuler("Table « %s » : q_x hors de [0, 1] aux lignes %s.", nom_cite,
                     paste(utils::head(hors_bornes, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = hors_bornes))
  age <- as.integer(x$age)
  annee <- if ("annee" %in% names(x)) as.integer(x$annee) else NULL
  # Ordre canonique (ADR 0015, point 2.1 ; M235) : rang du sexe dans
  # SEXES_TABLE (par match(), jamais par un tri de chaînes), puis annee si
  # présente, puis age ; tri radix stable, indépendant de la locale. L'objet
  # rendu et son empreinte ne dépendent plus de l'ordre reçu (aucun refus
  # d'âges non croissants). Doublons et trous s'évaluent sur l'ordre trié ;
  # les lignes sont citées dans la numérotation reçue (o[k]).
  rang <- match(sexe, SEXES_TABLE)
  o <- if (is.null(annee)) order(rang, age, method = "radix") else order(rang, annee, age, method = "radix")
  n <- length(o)
  # Même groupe (sexe, annee) que la ligne triée précédente ; comparaison et
  # non diff() : une différence d'années extrêmes déborderait les integer.
  meme <- c(FALSE, rang[o][-1L] == rang[o][-n])
  if (!is.null(annee)) meme <- meme & c(FALSE, annee[o][-1L] == annee[o][-n])
  pas <- c(NA_integer_, diff(age[o]))  # âges >= 0 : aucun débordement
  # Ligne reçue la plus tardive de chaque doublon (tri stable), en
  # numérotation reçue croissante.
  doublons <- sort(o[which(meme & pas == 0L)])
  if (length(doublons))
    refuser("donnees", "table_age_double",
            formuler("Table « %s » : âge en double au sein d'un même sexe%s, lignes %s.", nom_cite,
                     if (is.null(annee)) "" else formuler(" et d'une même année"),
                     paste(utils::head(doublons, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = doublons))
  # Âges contigus par (sexe, annee) : un pas supérieur à 1 est un trou, cité
  # par la paire des lignes qui l'encadrent dans l'ordre canonique ;
  # details$lignes : ligne qui suit le trou.
  trous <- which(meme & pas > 1L)
  if (length(trous))
    refuser("donnees", "table_ages_non_contigus",
            formuler("Table « %s » : âges non contigus (âge manquant) au sein d'un même sexe, entre les lignes %s.",
                     nom_cite, paste(sprintf("%d et %d", o[trous - 1L], o[trous])[seq_len(min(length(trous), NB_LIGNES_CITEES))],
                                collapse = ", ")),
            list(lignes = o[trous]))
  # Colonnes construites par sous-indiçage [o], jamais x[o, ] : row.names
  # automatiques, aucun attribut de colonne reçu.
  t <- data.frame(age = age[o], sexe = sexe[o], qx = as.numeric(x$qx)[o], stringsAsFactors = FALSE)
  if (!is.null(annee)) t$annee <- annee[o]
  structure(t, class = c("table_reference", "data.frame"), nom = nom, statut = statut,
            source = source, forme_origine = forme_origine, parametres = parametres)
}

# Catalogue des tables embarquées (ADR 0015, point 6 ; décision Q2 du
# 06/11/2026) : liste nommée par `nom` d'objets table_reference de statut
# embarquee, lue à l'installation. Vide tant que la branche de transcription
# n'embarque aucune table : toute table de statut embarquee est refusée
# (table_embarquee_absente).
TABLES_EMBARQUEES <- structure(list(), names = character(0))

# Table embarquée de nom `nom` (ADR 0015, point 6.2), objet table_reference
# rendu tel qu'il est au catalogue ; refuse (module donnees) un nom absent du
# catalogue, ou qui n'est pas une chaîne unique. Exportée (appelants :
# application, tiers qui rejoue).
table_embarquee <- function(nom) {
  noms <- attr(TABLES_EMBARQUEES, "names", exact = TRUE)
  if (!chaine_unique(nom) || !(nom %in% noms))
    refuser("donnees", "table_embarquee_absente",
            formuler("Table embarquée « %s » : ce package n'embarque aucune table de ce nom (tables embarquées : %s).",
                     if (chaine_unique(nom)) citer(nom) else formuler("nom illisible"),
                     if (length(noms)) liste_guillemets(noms) else formuler("aucune")),
            list(lu = nom, attendu = noms))
  TABLES_EMBARQUEES[[nom]]
}
