# -----------------------------------------------------------------------------
#  R/donnees.R : module `donnees` (issue #9 ; E2.2, E2.4, E2.7 ; ADR 0003,
#  points 2, 5 et 7 ; ADR 0007, points 4, 6.4, 6.5, 7 et 10 ; spécification
#  d'actuary, A.0 et A.1)
#
#  - CATALOGUE_ROLES et catalogue_roles() : catalogue fermé des rôles de la v1
#    (exporté : l'application le lit pour la déclaration des rôles) ;
#  - controler_roles() : refus au niveau des rôles (liste fermée) ;
#  - evaluer_motifs() : motifs de ligne et motifs de tête (liste fermée
#    MOTIFS_LIGNES, MOTIFS_TETE), évaluation exhaustive, sans court-circuit ;
#  - preparer_donnees() : fonction d'étape `donnees` — refus par défaut,
#    exclusions déclarées (comptées, listées, lignes conservées inchangées),
#    unité d'observation et fusion par tête, relevés non bloquants,
#    avertissements inscrits dans le résultat ;
#  - table_reference() : représentation commune d'une table de référence et
#    son contrôle (exportée).
#
#  Conventions de temps (A.0) : une date est un jour ; un intervalle
#  d'observation est [entrée, sortie) ; une ligne `en_cours` est ouverte à
#  droite (+Inf). Âge au dernier anniversaire : age_revolu().
#  Aucune donnée n'est modifiée : les lignes conservées sont celles de
#  l'entrée ; les quantités dérivées (observations fusionnées) sont à part.
#  Ce module n'appelle que `outils` (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# --- catalogue fermé des rôles (A.1) ------------------------------------------

# type : code contrôlé par type_conforme() ; usage, pour la seule recette de
# la branche (`lignes_dates_completes`) : requis, facultatif (contrôle),
# segmentation (requis si la segmentation l'emploie), non_pris_en_charge
# (refus nommé : son omission changerait le sens du résultat).
CATALOGUE_ROLES <- data.frame(
  role = c("id_contrat", "id_tete", "sexe", "date_naissance", "annee_naissance",
           "age_fourni", "date_age_fourni", "date_entree", "date_sortie",
           "cause_sortie", "date_evenement", "date_inventaire", "age", "annee",
           "exposition", "evenements", "montant", "segment", "etat_depart",
           "etat_arrivee", "date_changement"),
  famille = c("identification", "identification", "etat_civil", "etat_civil",
              "etat_civil", "etat_civil", "etat_civil", "observation",
              "observation", "observation", "observation", "observation",
              "agregats", "agregats", "agregats", "agregats", "ponderation",
              "segmentation", "multi_etats", "multi_etats", "multi_etats"),
  type = c("identifiant", "identifiant", "categorie", "date", "entier",
           "entier_positif", "date", "date", "date", "categorie", "date", "date",
           "entier", "entier", "reel_positif", "reel_positif",
           "reel_strictement_positif", "categorie_ou_entier", "categorie",
           "categorie", "date"),
  usage = c("facultatif", "facultatif", "segmentation", "requis", "facultatif",
            "non_pris_en_charge", "non_pris_en_charge", "requis", "requis",
            "requis", "non_pris_en_charge", "non_pris_en_charge",
            "non_pris_en_charge", "non_pris_en_charge", "non_pris_en_charge",
            "non_pris_en_charge", "non_pris_en_charge", "segmentation",
            "non_pris_en_charge", "non_pris_en_charge", "non_pris_en_charge"),
  stringsAsFactors = FALSE
)

# Libellé des types R admis, par code de type (restitué par catalogue_roles()).
TYPES_ADMIS <- c(
  identifiant = "character, integer, double entier fini",
  categorie = "character, factor",
  categorie_ou_entier = "character, factor, integer",
  date = "Date (valeurs entières finies)",
  entier = "integer, double entier fini",
  entier_positif = "integer, double entier fini >= 0",
  reel_positif = "double fini >= 0",
  reel_strictement_positif = "double fini > 0"
)

# Seule recette d'exposition de la branche (A.3) et ses rôles requis.
RECETTE_BRANCHE <- "lignes_dates_completes"

# Modalités des rôles catégoriels (A.1), comparées après as.character,
# sensibles à la casse.
MODALITES_SEXE <- c("H", "F")
MODALITES_CAUSE <- c("deces", "rachat", "terme", "autre", "en_cours")
ROLES_SEGMENTATION <- c("sexe", "segment")

# Âge (au dernier anniversaire) à partir duquel une ligne est aberrante
# (motif age_aberrant). Constante de catalogue, choix de conception (A.1) :
# la borne laisse passer toute durée de vie humaine connue ; ce n'est pas un
# paramètre du plan.
AGE_ABERRANT <- 130L

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

# Nombre de numéros de ligne cités dans un message de refus (A.1) ; l'objet
# de refus porte la liste complète.
NB_LIGNES_CITEES <- 10L

catalogue_roles <- function() {
  data.frame(role = CATALOGUE_ROLES$role, famille = CATALOGUE_ROLES$famille,
             type_admis = unname(TYPES_ADMIS[CATALOGUE_ROLES$type]),
             usage = CATALOGUE_ROLES$usage,
             recettes = ifelse(CATALOGUE_ROLES$usage == "non_pris_en_charge", "",
                               RECETTE_BRANCHE),
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
# est refusée.
type_conforme <- function(x, type) {
  if (!is.atomic(x) || !is.null(dim(x))) return(FALSE)
  nu <- !is.object(x)
  chaine <- nu && is.character(x)
  entier <- nu && is.integer(x)
  double_entier <- nu && is.double(x) && entiers_finis(x)
  switch(type,
    identifiant = chaine || entier || double_entier,
    categorie = chaine || is.factor(x),
    categorie_ou_entier = chaine || is.factor(x) || entier,
    date = identical(class(x), "Date") && is.numeric(unclass(x)) &&
      entiers_finis(unclass(x)),
    entier = entier || double_entier,
    entier_positif = (entier || double_entier) && all(x[!is.na(x)] >= 0),
    reel_positif = nu && is.double(x) && all(is.finite(x[!is.na(x)])) &&
      all(x[!is.na(x)] >= 0),
    reel_strictement_positif = nu && is.double(x) && all(is.finite(x[!is.na(x)])) &&
      all(x[!is.na(x)] > 0),
    stop(sprintf("type_conforme() : type de catalogue inconnu « %s »", type), call. = FALSE))
}

# --- refus au niveau des rôles (A.1) ------------------------------------------

liste_guillemets <- function(x) paste0("« ", x, " »", collapse = ", ")

# Message du refus `aucune_recette` (texte unique, repris par le module
# `exposition`, deduire_recette()).
message_aucune_recette <- function(requis, manquants)
  sprintf(paste("Aucune recette d'exposition ne s'applique : la recette « %s »",
                "(seule disponible) exige les rôles %s ; absents : %s."),
          RECETTE_BRANCHE, liste_guillemets(requis), liste_guillemets(manquants))

# Contrôle `roles` (vecteur de chaînes nommé rôle -> colonne, ADR 0007,
# point 4.1) contre le catalogue et `donnees`. Ordre des contrôles :
# forme, rôle inconnu, rôle déclaré deux fois, rôle non pris en charge,
# colonne absente, colonne ambiguë (nom présent plusieurs fois dans
# `donnees`), colonne déclarée deux fois, type incompatible, recette
# impossible (aucune_recette), rôle requis par la segmentation manquant.
# Chaque refus nomme tous les rôles en cause. Rend `roles`, inchangé.
controler_roles <- function(donnees, roles, segmentation) {
  if (!is.data.frame(donnees))
    refuser("donnees", "donnees_mal_formees",
            "Les données doivent être un data.frame (le moteur ne lit pas de fichier).",
            list(classe = class(donnees)))
  if (!is.character(roles) || is.null(names(roles)) || anyNA(roles) ||
      anyNA(names(roles)) || any(!nzchar(names(roles))) || !is.null(dim(roles)))
    refuser("donnees", "roles_mal_formes",
            paste("Les rôles doivent être un vecteur de chaînes nommé (nom du rôle -> nom",
                  "de colonne), sans valeur ni nom manquant (ADR 0007, point 4.1)."),
            list(classe = class(roles)))
  r <- names(roles)
  inconnus <- setdiff(r, CATALOGUE_ROLES$role)
  if (length(inconnus))
    refuser("donnees", "role_inconnu",
            sprintf("Rôle inconnu du catalogue fermé : %s (colonne %s).",
                    liste_guillemets(inconnus), liste_guillemets(roles[r %in% inconnus])),
            list(role = inconnus, colonne = unname(roles[r %in% inconnus])))
  doubles <- unique(r[duplicated(r)])
  if (length(doubles))
    refuser("donnees", "role_declare_deux_fois",
            sprintf("Rôle déclaré plusieurs fois : %s (colonnes %s).",
                    liste_guillemets(doubles), liste_guillemets(roles[r %in% doubles])),
            list(role = doubles, colonne = unname(roles[r %in% doubles])))
  non_pris <- r[CATALOGUE_ROLES$usage[match(r, CATALOGUE_ROLES$role)] == "non_pris_en_charge"]
  if (length(non_pris))
    refuser("donnees", "role_non_pris_en_charge",
            sprintf(paste("Rôle déclaré qu'aucune recette disponible n'emploie : %s (colonne %s) ;",
                          "l'ignorer changerait le sens du résultat, la construction est refusée."),
                    liste_guillemets(non_pris), liste_guillemets(roles[non_pris])),
            list(role = non_pris, colonne = unname(roles[non_pris])))
  absentes <- r[!(roles %in% names(donnees))]
  if (length(absentes))
    refuser("donnees", "colonne_absente",
            sprintf("Colonne absente des données : %s (rôle %s).",
                    liste_guillemets(roles[absentes]), liste_guillemets(absentes)),
            list(role = absentes, colonne = unname(roles[absentes])))
  ambigues <- r[vapply(roles, function(cl) sum(names(donnees) == cl) > 1L, logical(1L))]
  if (length(ambigues))
    refuser("donnees", "colonne_ambigue",
            sprintf("Colonne présente plusieurs fois sous le même nom dans les données : %s (rôle %s).",
                    liste_guillemets(roles[ambigues]), liste_guillemets(ambigues)),
            list(role = ambigues, colonne = unname(roles[ambigues])))
  col_doubles <- unique(roles[duplicated(roles)])
  if (length(col_doubles))
    refuser("donnees", "colonne_declaree_deux_fois",
            sprintf("Colonne déclarée pour plusieurs rôles : %s (rôles %s).",
                    liste_guillemets(col_doubles), liste_guillemets(r[roles %in% col_doubles])),
            list(role = r[roles %in% col_doubles], colonne = col_doubles))
  types <- CATALOGUE_ROLES$type[match(r, CATALOGUE_ROLES$role)]
  ko <- !vapply(seq_along(r), function(i) type_conforme(donnees[[roles[[i]]]], types[i]),
                logical(1L))
  if (any(ko))
    refuser("donnees", "type_incompatible",
            sprintf("Type incompatible : %s.",
                    paste(sprintf("rôle « %s », colonne « %s » : %s attendu, %s reçu",
                                  r[ko], roles[ko], TYPES_ADMIS[types[ko]],
                                  vapply(r[ko], function(x)
                                    paste(class(donnees[[roles[[x]]]]), collapse = "/"),
                                    character(1L))),
                          collapse = " ; ")),
            list(role = r[ko], colonne = unname(roles[ko]), type_attendu = unname(TYPES_ADMIS[types[ko]])))
  requis <- CATALOGUE_ROLES$role[CATALOGUE_ROLES$usage == "requis"]
  manquants <- setdiff(requis, r)
  if (length(manquants))
    refuser("donnees", "aucune_recette", message_aucune_recette(requis, manquants),
            list(recette = RECETTE_BRANCHE, role = manquants))
  seg_manquants <- setdiff(segmentation, r)
  if (length(seg_manquants))
    refuser("donnees", "role_requis_manquant",
            sprintf(paste("Rôle requis manquant : %s, employé par la segmentation du plan",
                          "(recette « %s »)."),
                    liste_guillemets(seg_manquants), RECETTE_BRANCHE),
            list(role = seg_manquants, recette = RECETTE_BRANCHE))
  roles
}

# --- paramètres de l'étape ----------------------------------------------------

# Contrôle les paramètres que l'étape `donnees` reçoit ; rend la ligne de
# TRANSITIONS et les bornes de la période (jours depuis 1970-01-01).
# Forme exacte (#23, forme_exacte()) : chaque valeur a son type de base exact
# et ne porte aucun attribut (ni names, ni dim, ni classe), hormis les noms
# des listes `transition` et `periode` et la classe Date des bornes de la
# période ; sinon refus sous le motif du paramètre.
controler_parametres_donnees <- function(transition, periode, delai_declaration,
                                         segmentation, exclusions) {
  chaine <- function(x) forme_exacte(x, "character") && length(x) == 1L && !is.na(x)
  # Liste nue aux noms exactement `noms` (ni champ en trop, ni nom partiel :
  # lecture par [[ ]], jamais par $, qui apparie les noms partiels ; ni
  # data.frame ni autre attribut que names ; la longueur est contrôlée à part :
  # sort() retire un nom NA, qui passerait sinon avec un élément de plus).
  noms_exacts <- function(x, noms) forme_exacte(x, "list", "names") && length(x) == length(noms) &&
    !is.null(names(x)) &&
    identical(sort(names(x), method = "radix"), sort(noms, method = "radix"))
  if (!noms_exacts(transition, c("depart", "arrivee")) || !chaine(transition[["depart"]]) ||
      !chaine(transition[["arrivee"]]))
    refuser("donnees", "transition_inconnue",
            paste("La transition doit être une liste de deux noms d'états (chaînes sans attribut),",
                  "de champs exactement depart et arrivee, sans autre attribut (ADR 0007, point 5.1)."),
            list(transition = transition))
  depart <- transition[["depart"]]; arrivee <- transition[["arrivee"]]
  k <- which(TRANSITIONS$depart == depart & TRANSITIONS$arrivee == arrivee)
  if (!length(k))
    refuser("donnees", "transition_inconnue",
            sprintf("Transition non prise en charge : %s -> %s (prises en charge : %s).",
                    depart, arrivee,
                    paste(TRANSITIONS$depart, "->", TRANSITIONS$arrivee, collapse = ", ")),
            list(transition = transition))
  # Stockage double ou integer admis (inchangé), seul attribut : la classe.
  date1 <- function(x) forme_exacte(x, c("double", "integer"), "class") &&
    identical(class(x), "Date") && length(x) == 1L && !is.na(x) &&
    is.finite(unclass(x)) && unclass(x) == trunc(unclass(x))
  if (!noms_exacts(periode, c("debut", "fin")) || !date1(periode[["debut"]]) || !date1(periode[["fin"]]))
    refuser("donnees", "periode_invalide",
            paste("La période d'observation doit être une liste de champs exactement debut et fin,",
                  "sans autre attribut, deux dates (classe Date, sans autre attribut) non manquantes."),
            list(periode = periode))
  if (!forme_exacte(delai_declaration, c("integer", "double")) || length(delai_declaration) != 1L ||
      is.na(delai_declaration) || !is.finite(delai_declaration) ||
      delai_declaration != trunc(delai_declaration) || delai_declaration < 0)
    refuser("donnees", "delai_invalide",
            "Le délai de déclaration doit être un nombre entier unique de jours >= 0, sans attribut (A.0, T4).",
            list(delai_declaration = delai_declaration))
  debut <- as.numeric(periode[["debut"]]); fin <- as.numeric(periode[["fin"]])
  fin_effective <- fin - delai_declaration
  if (debut > fin_effective)
    refuser("donnees", "periode_vide",
            sprintf("Période d'observation vide : début %s postérieur à la fin effective %s (fin %s, délai %s jour(s)).",
                    format(periode[["debut"]]), format(structure(fin_effective, class = "Date")),
                    format(periode[["fin"]]), format(delai_declaration)),
            list(debut = periode[["debut"]], fin_effective = structure(fin_effective, class = "Date")))
  if (!forme_exacte(segmentation, "character") || anyNA(segmentation) || anyDuplicated(segmentation) ||
      !all(segmentation %in% ROLES_SEGMENTATION))
    refuser("donnees", "segmentation_invalide",
            sprintf(paste("La segmentation doit être un vecteur de chaînes sans attribut, sans doublon,",
                          "pris dans %s (vide explicite admis)."),
                    liste_guillemets(ROLES_SEGMENTATION)),
            list(segmentation = segmentation))
  if (!forme_exacte(exclusions, "character") || anyNA(exclusions) || anyDuplicated(exclusions))
    refuser("donnees", "exclusions_invalides",
            paste("Les règles d'exclusion doivent être un vecteur de codes de motif sans attribut, sans doublon",
                  "(vide explicite admis)."),
            list(exclusions = exclusions))
  inconnus <- setdiff(exclusions, c(MOTIFS_LIGNES, MOTIFS_TETE))
  if (length(inconnus))
    refuser("donnees", "exclusion_inconnue",
            sprintf("Code d'exclusion inconnu : %s (liste fermée des motifs).", liste_guillemets(inconnus)),
            list(exclusions = inconnus))
  list(transition = TRANSITIONS[k, , drop = FALSE], debut = debut, fin = fin,
       borne = fin_effective + 1)
}

# --- motifs de ligne et de tête (A.1) -----------------------------------------

# Groupes (entiers) dont la valeur v n'est pas constante.
groupes_heterogenes <- function(g, v) {
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
# s'appliquent aux lignes propres de la tête. Rend list(motifs) : liste
# nommée code -> numéros de ligne (1-based, croissants), dans l'ordre
# MOTIFS_LIGNES puis MOTIFS_TETE (integer(0) pour un motif sans objet).
evaluer_motifs <- function(donnees, roles, debut, fin, borne, segmentation) {
  n <- nrow(donnees)
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
    propres <- which(!Reduce(`|`, m))
    g <- match(id_t[propres], unique(id_t[propres]))
    marquer <- function(groupes) {
      x <- logical(n)
      x[propres[g %in% groupes]] <- TRUE
      x
    }
    t$tete_naissance_incoherente <- marquer(groupes_heterogenes(g, nai[propres]))
    if (!is.null(sexe))
      t$tete_sexe_incoherent <- marquer(groupes_heterogenes(g, sexe[propres]))
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
  if (max(gs) * largeur >= 2^52)
    refuser("donnees", "dates_hors_plage",
            sprintf(paste("Dates d'entrée et de sortie étendues sur %.0f jours : hors de la plage que la",
                          "fusion par tête calcule exactement (%d têtes) ; vérifier les dates."),
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
#   transition         : list(depart, arrivee) (ADR 0007, point 5.1) ;
#   periode            : list(debut, fin), dates ; fin incluse (A.0, T4) ;
#   delai_declaration  : entier de jours >= 0 ;
#   segmentation       : sous-ensemble de c("sexe", "segment"), vide admis ;
#   exclusions         : codes de motif exclus, vide admis.
# Rend list(parametres, resultat, diagnostics, avertissements) ; les lignes
# conservées (resultat$lignes_conservees) sont celles de l'entrée, inchangées
# (`donnees` lui-même s'il n'y a aucune exclusion) ;
# resultat$observation_des_lignes donne, pour chaque ligne conservée, le
# numéro de son observation (composante fusionnée ou contrat).
preparer_donnees <- function(donnees, roles, transition, periode, delai_declaration,
                             segmentation, exclusions) {
  p <- controler_parametres_donnees(transition, periode, delai_declaration,
                                    segmentation, exclusions)
  controler_roles(donnees, roles, segmentation)
  n <- nrow(donnees)
  if (!n) refuser("donnees", "base_vide", "La base d'observation ne contient aucune ligne.")
  ev <- evaluer_motifs(donnees, roles, p$debut, p$fin, p$borne, segmentation)
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
            sprintf(paste("Lignes invalides, sans règle d'exclusion déclarée pour leur motif",
                          "(E2.7) : %s. Total : %d ligne(s) distincte(s)."),
                    paste(sprintf("« %s » : %d ligne(s), premières lignes %s%s", non_declares,
                                  lengths(lignes),
                                  vapply(premieres, paste, character(1L), collapse = ", "),
                                  ifelse(lengths(lignes) > NB_LIGNES_CITEES, " et suivantes", "")),
                          collapse = " ; "),
                    total),
            list(decompte = lengths(lignes), premieres_lignes = premieres, lignes = lignes,
                 total_lignes = total))
  }

  exclues <- sort(unique(unlist(motifs[exclusions], use.names = FALSE)))
  if (is.null(exclues)) exclues <- integer(0)
  conservees <- if (length(exclues)) donnees[-exclues, , drop = FALSE] else donnees
  numeros <- setdiff(seq_len(n), exclues)
  if (!length(numeros))
    refuser("donnees", "aucune_ligne_conservee",
            sprintf("Toutes les lignes (%d) sont écartées par les règles d'exclusion déclarées (%s).",
                    n, liste_guillemets(exclusions[lengths(motifs[exclusions]) > 0L])),
            list(lignes_exclues = length(exclues)))

  # Unité d'observation (E2.4, M46) et observations.
  col <- function(role) if (role %in% names(roles)) conservees[[roles[[role]]]] else NULL
  ent <- as.numeric(unclass(col("date_entree"))); sor <- as.numeric(unclass(col("date_sortie")))
  cause <- as.character(col("cause_sortie"))
  fin <- sor; fin[cause == "en_cours"] <- Inf
  id_t <- col("id_tete")
  unite <- if (p$transition$etat_de == "tete" && !is.null(id_t)) "tete" else "contrat"
  avert <- avertissements_vides()
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
    # quelle que soit l'unité (M48), sur les seules lignes conservées : une
    # tête écartée par une exclusion déclarée n'est pas comptée ; une tête
    # dont une ligne seulement est exclue l'est sur ses lignes conservées.
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
    observations <- data.frame(unite = id_t[i], ligne = numeros[i],
                               nb_lignes = tabulate(k, nbins = length(fusion$debut)),
                               stringsAsFactors = FALSE)
    observations$date_naissance <- col("date_naissance")[i]
  } else {
    i <- seq_along(ent)
    id_c <- col("id_contrat")
    observations <- data.frame(unite = if (is.null(id_c)) numeros else id_c,
                               ligne = numeros, nb_lignes = rep(1L, length(i)),
                               stringsAsFactors = FALSE)
    if (!is.null(id_t)) observations$id_tete <- id_t  # rééchantillonnage par têtes (E2.14)
    observations$date_naissance <- col("date_naissance")
    composante_ligne <- seq_along(i)
  }
  # Sexe s'il est déclaré ; segment s'il est employé par la segmentation ;
  # type d'origine conservé.
  if ("sexe" %in% names(roles)) observations$sexe <- col("sexe")[i]
  if ("segment" %in% segmentation) observations$segment <- col("segment")[i]
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

  # Avertissements (ADR 0007, point 6.5).
  if (p$transition$etat_de == "tete" && is.null(id_t))
    avert <- avertir(avert, "donnees", "unite_contrat",
                     paste("Unité d'observation : contrat (aucun identifiant de tête déclaré) ;",
                           "une tête titulaire de plusieurs contrats est comptée plusieurs fois,",
                           "ce qui surestime l'exposition et peut compter plusieurs fois un même décès."),
                     nrow(observations))
  for (s in setdiff(intersect(ROLES_SEGMENTATION, names(roles)), segmentation))
    avert <- avertir(avert, "donnees", "role_non_employe",
                     sprintf("Rôle « %s » déclaré, non employé par la segmentation du plan (contrôlé seulement).", s),
                     1)
  for (x in exclusions[lengths(motifs[exclusions]) > 0L])
    avert <- avertir(avert, "donnees", "exclusion",
                     sprintf("Exclusion déclarée « %s » : %d ligne(s) écartée(s).", x, length(motifs[[x]])),
                     length(motifs[[x]]))
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
                    observation_des_lignes = composante_ligne),
    diagnostics = list(motifs = data.frame(motif = names(motifs),
                                           nature = rep(c("ligne", "tete"),
                                                        c(length(MOTIFS_LIGNES), length(MOTIFS_TETE))),
                                           nombre = unname(nombre),
                                           exclu = names(motifs) %in% exclusions,
                                           stringsAsFactors = FALSE),
                       lignes_porteuses = length(unique(unlist(motifs, use.names = FALSE))),
                       releves = releves),
    avertissements = avert
  )
}

# --- table de référence (A.1 ; ADR 0007, point 7) ------------------------------

STATUTS_TABLE <- c("synthetique", "embarquee", "importee")
SEXES_TABLE <- c("H", "F", "ensemble")
FORMES_ORIGINE <- c("qx", "lx")

# Construit une table de référence (classe S3 `table_reference` sur un
# data.frame long : age entier, sexe, qx, et annee entier si fournie) et la
# contrôle ; refuse (module donnees) au lieu de corriger.
#   x            : data.frame (age, sexe, qx [, annee]) ;
#   nom, source  : chaînes non vides (source : arrêté, URL, fichier de
#                  provenance) ;
#   statut       : synthetique, embarquee ou importee ;
#   parametres   : liste non vide des paramètres pour une table synthétique,
#                  NULL sinon ;
#   forme_origine: qx ou lx (forme de la transcription).
table_reference <- function(x, nom, statut, source, parametres = NULL, forme_origine = "qx") {
  chaine <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  if (!chaine(nom))
    refuser("donnees", "table_nom_manquant", "La table de référence doit avoir un nom (chaîne non vide).")
  if (!chaine(statut) || !(statut %in% STATUTS_TABLE))
    refuser("donnees", "table_statut_inconnu",
            sprintf("Table « %s » : statut à prendre dans %s.", nom, liste_guillemets(STATUTS_TABLE)),
            list(statut = statut))
  if (!chaine(source))
    refuser("donnees", "table_source_manquante",
            sprintf("Table « %s » : la source (arrêté, URL, fichier de provenance) est requise.", nom))
  if (statut == "synthetique" && (!is.list(parametres) || !length(parametres)))
    refuser("donnees", "table_parametres_manquants",
            sprintf("Table synthétique « %s » : ses paramètres (liste non vide) sont requis.", nom))
  if (statut != "synthetique" && !is.null(parametres))
    refuser("donnees", "table_parametres_inattendus",
            sprintf("Table « %s » de statut « %s » : aucun paramètre de loi attendu.", nom, statut))
  if (!chaine(forme_origine) || !(forme_origine %in% FORMES_ORIGINE))
    refuser("donnees", "table_forme_inconnue",
            sprintf("Table « %s » : forme d'origine à prendre dans %s.", nom, liste_guillemets(FORMES_ORIGINE)))
  if (!is.data.frame(x))
    refuser("donnees", "table_mal_formee", sprintf("Table « %s » : un data.frame est attendu.", nom))
  colonnes <- c("age", "sexe", "qx", if ("annee" %in% names(x)) "annee")
  manquantes <- setdiff(c("age", "sexe", "qx"), names(x))
  if (length(manquantes))
    refuser("donnees", "table_colonne_manquante",
            sprintf("Table « %s » : colonne(s) manquante(s) %s.", nom, liste_guillemets(manquantes)),
            list(colonne = manquantes))
  en_trop <- setdiff(names(x), colonnes)
  if (length(en_trop))
    refuser("donnees", "table_colonne_inconnue",
            sprintf("Table « %s » : colonne(s) inconnue(s) %s (admises : age, sexe, qx, annee).",
                    nom, liste_guillemets(en_trop)),
            list(colonne = en_trop))
  if (!nrow(x)) refuser("donnees", "table_vide", sprintf("Table « %s » : aucune ligne.", nom))
  mal_typees <- c(
    age = !type_conforme(x$age, "entier_positif") || anyNA(x$age),
    sexe = !type_conforme(x$sexe, "categorie") || anyNA(x$sexe),
    qx = !(is.double(x$qx) && !is.object(x$qx)),
    annee = "annee" %in% names(x) && (!type_conforme(x$annee, "entier") || anyNA(x$annee)))
  if (any(mal_typees))
    refuser("donnees", "table_colonne_mal_typee",
            sprintf("Table « %s » : colonne(s) mal typée(s) ou avec valeur manquante %s (age : entier >= 0 ; annee : entier ; sexe : chaîne ; qx : double).",
                    nom, liste_guillemets(names(mal_typees)[mal_typees])),
            list(colonne = names(mal_typees)[mal_typees]))
  sexe <- as.character(x$sexe)
  hors <- which(!(sexe %in% SEXES_TABLE))
  if (length(hors))
    refuser("donnees", "table_sexe_hors_modalites",
            sprintf("Table « %s » : sexe hors de %s aux lignes %s.", nom, liste_guillemets(SEXES_TABLE),
                    paste(utils::head(hors, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = hors))
  # « ensemble » (table sans distinction de sexe) ne coexiste pas avec H ou
  # F, dans toute la table ou, si elle a des années, au sein d'une année.
  annee0 <- if ("annee" %in% names(x)) as.integer(x$annee) else rep(0L, length(sexe))
  melange <- unique(annee0[annee0 %in% annee0[sexe == "ensemble"] & annee0 %in% annee0[sexe != "ensemble"]])
  if (length(melange))
    refuser("donnees", "table_sexes_melanges",
            sprintf("Table « %s » : la modalité « ensemble » coexiste avec H ou F%s.", nom,
                    if ("annee" %in% names(x)) sprintf(" (années %s)", paste(utils::head(melange, NB_LIGNES_CITEES), collapse = ", ")) else ""),
            list(annees = if ("annee" %in% names(x)) melange else NULL))
  non_fini <- which(!is.finite(x$qx))
  if (length(non_fini))
    refuser("donnees", "table_qx_non_fini",
            sprintf("Table « %s » : q_x manquant ou non fini aux lignes %s.", nom,
                    paste(utils::head(non_fini, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = non_fini))
  hors_bornes <- which(x$qx < 0 | x$qx > 1)
  if (length(hors_bornes))
    refuser("donnees", "table_qx_hors_bornes",
            sprintf("Table « %s » : q_x hors de [0, 1] aux lignes %s.", nom,
                    paste(utils::head(hors_bornes, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = hors_bornes))
  age <- as.integer(x$age)
  annee <- if ("annee" %in% names(x)) as.integer(x$annee) else NULL
  cle <- if (is.null(annee)) sexe else paste(sexe, annee, sep = "\r")
  doublons <- which(duplicated(paste(cle, age, sep = "\r")))
  if (length(doublons))
    refuser("donnees", "table_age_double",
            sprintf("Table « %s » : âge en double au sein d'un même sexe%s, lignes %s.", nom,
                    if (is.null(annee)) "" else " et d'une même année",
                    paste(utils::head(doublons, NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = doublons))
  # Âges croissants et contigus par (sexe, annee), dans l'ordre reçu.
  g <- match(cle, unique(cle))
  o <- order(g, method = "radix")  # tri stable : ordre reçu au sein du groupe
  pas <- diff(age[o]); meme <- diff(g[o]) == 0L
  decroit <- which(meme & pas < 0L)
  if (length(decroit))
    refuser("donnees", "table_ages_non_croissants",
            sprintf("Table « %s » : âges non croissants au sein d'un même sexe, lignes %s.", nom,
                    paste(utils::head(o[decroit + 1L], NB_LIGNES_CITEES), collapse = ", ")),
            list(lignes = o[decroit + 1L]))
  trous <- which(meme & pas > 1L)
  if (length(trous))
    refuser("donnees", "table_ages_non_contigus",
            sprintf("Table « %s » : âges non contigus (âge manquant) au sein d'un même sexe, entre les lignes %s.",
                    nom, paste(sprintf("%d et %d", o[trous], o[trous + 1L])[seq_len(min(length(trous), NB_LIGNES_CITEES))],
                               collapse = ", ")),
            list(lignes = o[trous + 1L]))
  t <- data.frame(age = age, sexe = sexe, qx = as.numeric(x$qx), stringsAsFactors = FALSE)
  if (!is.null(annee)) t$annee <- annee
  structure(t, class = c("table_reference", "data.frame"), nom = nom, statut = statut,
            source = source, forme_origine = forme_origine, parametres = parametres)
}
