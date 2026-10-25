# -----------------------------------------------------------------------------
#  tests/unitaires/test_donnees.R : module `donnees` (issue #9 ; E2.2, E2.4,
#  E2.7 ; ADR 0007, points 4, 6.4, 6.5 et 7 ; spécification d'actuary, A.0,
#  A.1 et cas F1 à F7 de A.4)
#
#  Nature des attendus : valeurs calculées à la main (numéros de ligne,
#  décomptes, intervalles fusionnés en dates, nombres de jours), écrites en
#  dur dans le test ; aucun attendu n'est produit par le module testé.
#  Les motifs de refus sont lus par leur code (champ `motif` du refus, et
#  `details` pour les motifs de ligne) ; les messages sont contrôlés sur
#  leurs fragments ASCII (noms de rôle, de colonne, numéros de ligne).
# -----------------------------------------------------------------------------

preparer_donnees <- tablexp:::preparer_donnees
age_revolu <- tablexp:::age_revolu
D <- as.Date

PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))  # B = 2025-01-01
B <- D("2025-01-01")
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")

# Base valide de quatre contrats, trois têtes (T3 : deux contrats).
BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"),
  tete = c("T1", "T2", "T3", "T3"),
  sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"),
  autre_colonne = c(1.5, 2.5, 3.5, 4.5),  # non déclarée : ignorée
  stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")

prep <- function(d = BASE, roles = ROLES, exclusions = character(0), transition = VD,
                 segmentation = "sexe", periode = PER, delai = 0)
  preparer_donnees(d, roles, transition, periode, delai, segmentation, exclusions)
# Condition de refus levée par expr (NULL si aucune).
refus <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
contient <- function(r, ...) {
  if (is.null(r)) return("aucun refus levé")
  m <- conditionMessage(r)
  manque <- Filter(function(f) !grepl(f, m, fixed = TRUE), c(...))
  if (length(manque)) sprintf("fragment(s) absent(s) du message : %s | message : %s",
                              paste(manque, collapse = " ; "), m) else TRUE
}

# --- catalogue fermé des rôles ------------------------------------------------

cat_r <- catalogue_roles()
verifier("catalogue_roles() : 21 rôles, colonnes role, famille, type_admis, usage, recettes",
         nrow(cat_r) == 21L && identical(names(cat_r), c("role", "famille", "type_admis", "usage", "recettes")))
verifier("catalogue_roles() : rôles requis de la recette = date_naissance, date_entree, date_sortie, cause_sortie",
         identical(cat_r$role[cat_r$usage == "requis"],
                   c("date_naissance", "date_entree", "date_sortie", "cause_sortie")))
NON_PRIS <- c("age_fourni", "date_age_fourni", "date_evenement", "date_inventaire", "age", "annee",
              "exposition", "evenements", "montant", "etat_depart", "etat_arrivee", "date_changement")
verifier("catalogue_roles() : rôles non employés par la recette de la branche signalés (usage non_pris_en_charge, recettes vide)",
         identical(cat_r$role[cat_r$usage == "non_pris_en_charge"], NON_PRIS) &&
           all(cat_r$recettes[cat_r$usage == "non_pris_en_charge"] == "") &&
           all(cat_r$recettes[cat_r$usage != "non_pris_en_charge"] == "lignes_dates_completes"))
verifier("catalogue_roles() : noms sans accents, uniques",
         !anyDuplicated(cat_r$role) && all(grepl("^[a-z_]+$", cat_r$role)))
verifier("catalogue_roles() : type admis de date_naissance = Date",
         identical(cat_r$type_admis[cat_r$role == "date_naissance"], "Date (valeurs entières finies)"))

# --- base valide --------------------------------------------------------------

r <- prep()
verifier("base valide : aucune exclusion, lignes conservées identical() à l'entrée",
         identical(r$resultat$lignes_conservees, BASE) &&
           identical(r$resultat$numeros_conserves, 1:4))
verifier("base valide : unité tête (vivant -> décédé, id_tete déclaré)",
         identical(r$resultat$unite_observation, "tete") &&
           identical(r$parametres$unite_observation, "tete") &&
           identical(r$parametres$transition, "vivant_decede"))
verifier("base valide : aucun motif porté, diagnostics complets (20 motifs)",
         nrow(r$diagnostics$motifs) == 20L && all(r$diagnostics$motifs$nombre == 0L) &&
           r$diagnostics$lignes_porteuses == 0L)
verifier("base valide : seul avertissement, le chevauchement des deux contrats de T3 (relevé de fusion)",
         identical(r$avertissements$code, "chevauchements") && r$avertissements$nombre == 1 &&
           r$diagnostics$releves$chevauchements == 1L)

# --- refus au niveau des rôles ------------------------------------------------

verifier("donnees non data.frame : refus donnees_mal_formees",
         leve_refus(prep(d = as.list(BASE)), "donnees_mal_formees"))
verifier("rôles non nommés : refus roles_mal_formes",
         leve_refus(prep(roles = unname(ROLES)), "roles_mal_formes"))
verifier("rôles en liste : refus roles_mal_formes",
         leve_refus(prep(roles = as.list(ROLES)), "roles_mal_formes"))
# Forme exacte des rôles (#62, forme_exacte()) : vecteur character dont le
# seul attribut est names. Chaque témoin porte un attribut de plus, ou un dim,
# sur des rôles valides par ailleurs : refus roles_mal_formes, par
# controler_roles() appelée directement et par preparer_donnees() ; ROLES nu
# est admis.
cr <- tablexp:::controler_roles
ROLES_REFUSES <- list(
  "attribut quelconque (foo)" = structure(ROLES, foo = 1),
  "classe noquote" = noquote(ROLES),
  "classe AsIs (I)" = I(ROLES),
  "classe quelconque (bar)" = structure(ROLES, class = "bar"),
  "attribut comment" = structure(ROLES, comment = "x"),
  "attribut foo et classe bar" = structure(ROLES, foo = 1, class = "bar"),
  "tableau a une dimension (dim, dimnames)" =
    array(unname(ROLES), dim = length(ROLES), dimnames = list(names(ROLES))),
  "matrice a une colonne (dim, dimnames)" =
    matrix(unname(ROLES), ncol = 1L, dimnames = list(names(ROLES), NULL)))
for (k in names(ROLES_REFUSES))
  verifier(sprintf("rôles, forme exacte : %s -> refus roles_mal_formes (controler_roles et preparer_donnees)", k),
           leve_refus(cr(BASE, ROLES_REFUSES[[k]], "sexe"), "roles_mal_formes") &&
             leve_refus(prep(roles = ROLES_REFUSES[[k]]), "roles_mal_formes"))
x <- refus(cr(BASE, structure(ROLES, foo = 1), "sexe"))
verifier("rôles, forme exacte : message et détails du refus nomment l'attribut en cause",
         identical(x$motif, "roles_mal_formes") && isTRUE(contient(x, "sans autre attribut que ses noms")) &&
           identical(x$details, list(classe = "character", attributs = c("names", "foo"))))
verifier("rôles, forme exacte : vecteur nommé sans autre attribut admis (controler_roles rend les rôles inchangés)",
         identical(cr(BASE, ROLES, "sexe"), ROLES) && identical(attributes(ROLES), list(names = names(ROLES))))
x <- refus(prep(roles = c(ROLES, age_exact = "autre_colonne")))
verifier("rôle inconnu : refus role_inconnu, message nomme rôle et colonne",
         identical(x$motif, "role_inconnu") && isTRUE(contient(x, "age_exact", "autre_colonne")) &&
           identical(x$details$role, "age_exact"))
x <- refus(prep(roles = c(ROLES, sexe = "autre_colonne")))
verifier("rôle déclaré deux fois : refus role_declare_deux_fois, message nomme rôle et colonnes",
         identical(x$motif, "role_declare_deux_fois") && isTRUE(contient(x, "sexe", "autre_colonne")))
for (role in NON_PRIS) {
  x <- refus(prep(roles = c(ROLES, stats::setNames("autre_colonne", role))))
  verifier(sprintf("rôle %s : refus role_non_pris_en_charge, message nomme rôle et colonne", role),
           if (is.null(x)) "aucun refus" else if (!identical(x$motif, "role_non_pris_en_charge"))
             x$motif else contient(x, role, "autre_colonne"))
}
x <- refus(prep(roles = replace(ROLES, "sexe", "genre")))
verifier("colonne absente : refus colonne_absente, message nomme rôle et colonne",
         identical(x$motif, "colonne_absente") && isTRUE(contient(x, "genre", "sexe")))
# Rôle ou colonne au codage bytes (#111) : refus nommé, message lisible, sans erreur R.
OCTETS <- "\xe9t\xe9"; Encoding(OCTETS) <- "bytes"
x <- refus(prep(roles = replace(ROLES, "sexe", OCTETS)))
verifier("colonne absente au codage bytes : refus colonne_absente, chaîne remplacée par un repère",
         identical(x$motif, "colonne_absente") && isTRUE(contient(x, "<chaîne d'encodage bytes>", "sexe")))
x <- refus(prep(roles = c(ROLES, stats::setNames("autre_colonne", OCTETS))))
verifier("rôle inconnu au codage bytes : refus role_inconnu, chaîne remplacée par un repère",
         identical(x$motif, "role_inconnu") && isTRUE(contient(x, "<chaîne d'encodage bytes>", "autre_colonne")))
x <- refus(prep(transition = list(depart = OCTETS, arrivee = "deces")))
verifier("transition au codage bytes : refus transition_inconnue, chaîne remplacée par un repère",
         identical(x$motif, "transition_inconnue") && isTRUE(contient(x, "<chaîne d'encodage bytes> -> deces")))
x <- refus(prep(roles = replace(ROLES, "id_contrat", "tete")))
verifier("colonne déclarée deux fois : refus colonne_declaree_deux_fois, message nomme colonne et rôles",
         identical(x$motif, "colonne_declaree_deux_fois") &&
           isTRUE(contient(x, "tete", "id_contrat", "id_tete")))
TYPES_KO <- list(
  "date en chaîne" = list(col = "naissance", val = as.character(BASE$naissance), role = "date_naissance"),
  "date POSIXct" = list(col = "entree", val = as.POSIXct(BASE$entree), role = "date_entree"),
  "date fractionnaire" = list(col = "sortie", val = BASE$sortie + 0.5, role = "date_sortie"),
  "date infinie" = list(col = "sortie", val = structure(c(1, 2, Inf, 3), class = "Date"), role = "date_sortie"),
  "date en nombre" = list(col = "naissance", val = as.numeric(BASE$naissance), role = "date_naissance"),
  "identifiant double non entier" = list(col = "contrat", val = c(1, 2.5, 3, 4), role = "id_contrat"),
  "identifiant facteur" = list(col = "tete", val = factor(BASE$tete), role = "id_tete"),
  "sexe entier" = list(col = "sexe", val = c(1L, 2L, 1L, 1L), role = "sexe"),
  "cause logique" = list(col = "cause", val = c(TRUE, FALSE, TRUE, TRUE), role = "cause_sortie"))
for (nom in names(TYPES_KO)) {
  cas <- TYPES_KO[[nom]]
  d <- BASE; d[[cas$col]] <- cas$val
  x <- refus(prep(d = d))
  verifier(sprintf("type incompatible (%s) : refus type_incompatible, message nomme rôle et colonne", nom),
           if (is.null(x)) "aucun refus" else if (!identical(x$motif, "type_incompatible")) x$motif
           else contient(x, cas$role, cas$col))
}
d <- BASE; d$an <- c(1950.5, 1960, 1945, 1945)
verifier("type incompatible : annee_naissance non entière",
         leve_refus(prep(d = d, roles = c(ROLES, annee_naissance = "an")), "type_incompatible"))
d <- BASE; d$contrat <- c(1, 2, 3, 4)
verifier("identifiant double entier fini : admis", is.list(prep(d = d)))
d <- BASE; d$sexe <- factor(d$sexe); d$cause <- factor(d$cause)
verifier("sexe et cause en facteur : admis", is.list(prep(d = d)))
x <- refus(prep(roles = ROLES[names(ROLES) != "date_naissance"]))
verifier("rôle requis de la recette absent : refus aucune_recette, message nomme la recette et le rôle absent",
         identical(x$motif, "aucune_recette") &&
           isTRUE(contient(x, "lignes_dates_completes", "date_naissance")) &&
           identical(x$details$role, "date_naissance"))
x <- refus(prep(segmentation = c("sexe", "segment")))
verifier("rôle employé par la segmentation absent : refus role_requis_manquant, message nomme rôle et recette",
         identical(x$motif, "role_requis_manquant") && isTRUE(contient(x, "segment", "lignes_dates_completes")))
verifier("sexe dans la segmentation sans rôle sexe : refus role_requis_manquant",
         leve_refus(prep(roles = ROLES[names(ROLES) != "sexe"]), "role_requis_manquant"))

# --- paramètres de l'étape ----------------------------------------------------

verifier("transition inconnue : refus transition_inconnue",
         leve_refus(prep(transition = list(depart = "vivant", arrivee = "rachat")), "transition_inconnue"))
verifier("transition en chaîne : refus transition_inconnue",
         leve_refus(prep(transition = "vivant_decede"), "transition_inconnue"))
verifier("période en chaînes : refus periode_invalide",
         leve_refus(prep(periode = list(debut = "2015-01-01", fin = "2024-12-31")), "periode_invalide"))
for (dl in list(-1, 1.5, NA_real_, "0", c(0, 1)))
  verifier(sprintf("délai %s : refus delai_invalide", deparse(dl)),
           leve_refus(prep(delai = dl), "delai_invalide"))
verifier("délai 3653 j (fin effective 2014-12-31 < début) : refus periode_vide",
         leve_refus(prep(delai = 3653), "periode_vide"))
verifier("délai 3652 j (fin effective = début) : admis", is.list(prep(delai = 3652)))
verifier("segmentation hors {sexe, segment} : refus segmentation_invalide",
         leve_refus(prep(segmentation = "age"), "segmentation_invalide"))
verifier("exclusions non chaînes : refus exclusions_invalides",
         leve_refus(prep(exclusions = NULL), "exclusions_invalides"))
x <- refus(prep(exclusions = c("naissance_manquante", "motif_invente")))
verifier("code d'exclusion inconnu : refus exclusion_inconnue, message nomme le code",
         identical(x$motif, "exclusion_inconnue") && isTRUE(contient(x, "motif_invente")))

# --- motifs de ligne : refus par défaut, puis exclusion déclarée -------------

# Pour chaque cas : base modifiée, rôles, motif attendu, numéros de ligne
# attendus (calculés à la main).
avec <- function(...) { d <- BASE; m <- list(...); for (n in names(m)) d[[n]] <- m[[n]]; d }
CAS_LIGNES <- list(
  list(motif = "id_contrat_manquant", d = avec(contrat = c("C1", NA, "C3", "C4")), lignes = 2L),
  list(motif = "id_tete_manquant", d = avec(tete = c("T1", "T2", NA, "T3")), lignes = 3L),
  list(motif = "naissance_manquante", d = avec(naissance = D(c("1950-03-15", NA, "1945-06-06", "1945-06-06"))),
       lignes = 2L),
  list(motif = "entree_manquante", d = avec(entree = D(c(NA, "2020-03-01", "2016-01-01", "2019-01-01"))),
       lignes = 1L),
  list(motif = "sortie_manquante", d = avec(sortie = D(c(NA, NA, "2022-03-01", "2022-03-01"))), lignes = 1L),
  list(motif = "cause_manquante", d = avec(cause = c("rachat", NA, "deces", "deces")), lignes = 2L),
  list(motif = "cause_hors_modalites", d = avec(cause = c("Rachat", "en_cours", "deces", "deces")),
       lignes = 1L),
  list(motif = "sexe_manquant", d = avec(sexe = c(NA, "F", "H", "H")), lignes = 1L),
  list(motif = "sexe_hors_modalites", d = avec(sexe = c("H", "M", "H", "H")), lignes = 2L),
  list(motif = "naissance_apres_entree",
       d = avec(naissance = D(c("2016-01-02", "1960-07-01", "1945-06-06", "1945-06-06"))), lignes = 1L),
  list(motif = "sortie_avant_entree", d = avec(sortie = D(c("2015-12-31", NA, "2022-03-01", "2022-03-01"))),
       lignes = 1L),
  list(motif = "en_cours_avant_fin", d = avec(sortie = D(c("2019-12-31", "2024-12-30", "2022-03-01", "2022-03-01"))),
       lignes = 2L),
  list(motif = "age_aberrant", d = avec(naissance = D(c("1950-03-15", "1894-12-31", "1945-06-06", "1945-06-06"))),
       lignes = 2L),
  list(motif = "contrat_duplique", d = avec(contrat = c("C1", "C3", "C3", "C4")), lignes = 2:3),
  # Motifs qui demandent un rôle facultatif ou une segmentation (constat m3).
  list(motif = "segment_manquant", d = avec(segment = c("a", NA, "b", "b")),
       roles = c(ROLES, segment = "segment"), seg = c("sexe", "segment"), lignes = 2L),
  list(motif = "annee_naissance_incoherente", d = avec(an = c(1950L, 1961L, 1945L, 1945L)),
       roles = c(ROLES, annee_naissance = "an"), lignes = 2L),
  # Motifs de tête (T3 = lignes 3 et 4).
  list(motif = "tete_sexe_incoherent", d = avec(sexe = c("H", "F", "H", "F")), lignes = 3:4),
  list(motif = "tete_segment_incoherent", d = avec(segment = c("a", "a", "b", "c")),
       roles = c(ROLES, segment = "segment"), seg = c("sexe", "segment"), lignes = 3:4),
  list(motif = "tete_observee_apres_deces",
       d = avec(sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-15")),
                cause = c("rachat", "en_cours", "deces", "autre")), lignes = 3:4)
)
for (cas in CAS_LIGNES) {
  cas_roles <- if (is.null(cas$roles)) ROLES else cas$roles
  cas_seg <- if (is.null(cas$seg)) "sexe" else cas$seg
  x <- refus(prep(d = cas$d, roles = cas_roles, segmentation = cas_seg))
  verifier(sprintf("%s : refus ligne_invalide, décompte %d, lignes %s, message", cas$motif,
                   length(cas$lignes), paste(cas$lignes, collapse = ",")),
           if (is.null(x)) "aucun refus" else if (!identical(x$motif, "ligne_invalide")) x$motif
           else if (!identical(names(x$details$decompte), cas$motif)) paste("motifs :", paste(names(x$details$decompte), collapse = ","))
           else if (!identical(unname(x$details$decompte), length(cas$lignes))) "décompte"
           else if (!identical(x$details$lignes[[cas$motif]], cas$lignes)) "numéros de ligne"
           else contient(x, cas$motif, sprintf("%d ligne(s)", length(cas$lignes)),
                         paste(cas$lignes, collapse = ", ")))
  r <- tryCatch(prep(d = cas$d, roles = cas_roles, segmentation = cas_seg, exclusions = cas$motif),
                refus_tablexp = function(e) e)
  verifier(sprintf("%s déclaré en exclusion : ligne(s) %s exclue(s), comptée(s), lignes conservées identical()",
                   cas$motif, paste(cas$lignes, collapse = ",")),
           if (inherits(r, "refus_tablexp")) paste("refus", r$motif, conditionMessage(r))
           else if (!identical(r$resultat$lignes_conservees, cas$d[-cas$lignes, , drop = FALSE])) "lignes conservées"
           else if (!identical(r$resultat$exclusions,
                               data.frame(motif = cas$motif, nombre = length(cas$lignes), stringsAsFactors = FALSE)))
             "décompte des exclusions"
           else if (!identical(r$resultat$lignes_exclues, stats::setNames(list(cas$lignes), cas$motif))) "liste des exclusions"
           else if (!identical(r$resultat$numeros_conserves, setdiff(seq_len(nrow(cas$d)), cas$lignes))) "numéros conservés"
           else if (!identical(r$resultat$nombre_lignes_exclues, length(cas$lignes))) "nombre de lignes exclues"
           else if (!any(r$avertissements$code == "exclusion")) "avertissement d'exclusion absent"
           else TRUE)
}
d <- avec(segment = c("a", NA, "b", "b"))
x <- refus(prep(d = d, roles = c(ROLES, segment = "segment"), segmentation = c("sexe", "segment")))
verifier("segment_manquant : refus ligne_invalide, ligne 2",
         identical(x$details$lignes$segment_manquant, 2L))
d <- avec(an = c(1950L, 1961L, 1945L, 1945L))
x <- refus(prep(d = d, roles = c(ROLES, annee_naissance = "an")))
verifier("annee_naissance_incoherente : refus, ligne 2 (1961 au lieu de 1960)",
         identical(x$details$lignes$annee_naissance_incoherente, 2L))
verifier("annee_naissance cohérente : admis",
         is.list(prep(d = avec(an = c(1950, 1960, 1945, 1945)), roles = c(ROLES, annee_naissance = "an"))))

# Égalités admises et motifs non levés sur NA.
verifier("naissance = entrée, sortie = entrée : admises",
         is.list(prep(d = avec(naissance = D(c("2016-01-01", "1960-07-01", "1945-06-06", "1945-06-06")),
                               sortie = D(c("2016-01-01", NA, "2022-03-01", "2022-03-01"))))))
verifier("en_cours avec sortie = fin ou postérieure : admis",
         is.list(prep(d = avec(sortie = D(c("2019-12-31", "2024-12-31", "2022-03-01", "2022-03-01"))))) &&
           is.list(prep(d = avec(sortie = D(c("2019-12-31", "2026-01-01", "2022-03-01", "2022-03-01"))))))
x <- refus(prep(d = avec(cause = c("rachat", NA, "deces", "deces"))))
verifier("cause NA et sortie NA : cause_manquante seule (sortie_manquante non levée)",
         identical(names(x$details$decompte), "cause_manquante"))
x <- refus(prep(d = avec(entree = D(c(NA, "2020-03-01", "2016-01-01", "2019-01-01")),
                         naissance = D(c("2030-01-01", "1960-07-01", "1945-06-06", "1945-06-06")))))
verifier("entrée NA : naissance_apres_entree non levée (comparaison à une date NA)",
         identical(names(x$details$decompte), "entree_manquante"))
# Âge aberrant : borne 130 (âge au dernier anniversaire à min(sortie, fin)).
verifier("age_aberrant : 129 ans à la fin (naissance 1895-01-01) admis",
         is.list(prep(d = avec(naissance = D(c("1950-03-15", "1895-01-01", "1945-06-06", "1945-06-06"))))))
x <- refus(prep(d = avec(naissance = D(c("1885-01-01", "1960-07-01", "1945-06-06", "1945-06-06")),
                         entree = D(c("2014-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
                         sortie = D(c("2014-12-31", NA, "2022-03-01", "2022-03-01")))))
verifier("age_aberrant : âge à la sortie (129 ans au 2014-12-31), non à la fin : admis",
         is.null(x))

# Exhaustivité : une ligne porte plusieurs motifs ; tous évalués.
d <- avec(sortie = D(c("2015-12-31", NA, "2022-03-01", "2022-03-01")),
          cause = c("Rachat", "en_cours", "deces", "deces"),
          sexe = c("H", "X", "H", "H"))
x <- refus(prep(d = d))
verifier("exhaustivité : ligne 1 porte cause_hors_modalites et sortie_avant_entree ; ligne 2 sexe_hors_modalites",
         identical(as.list(x$details$decompte),
                   list(cause_hors_modalites = 1L, sexe_hors_modalites = 1L, sortie_avant_entree = 1L)) &&
           identical(x$details$total_lignes, 2L))
r <- tryCatch(prep(d = d, exclusions = c("cause_hors_modalites", "sexe_hors_modalites")),
              refus_tablexp = function(e) e)
verifier("ligne portant un motif déclaré et un motif non déclaré : refus (sortie_avant_entree, ligne 1)",
         inherits(r, "refus_tablexp") && identical(names(r$details$decompte), "sortie_avant_entree") &&
           identical(r$details$lignes$sortie_avant_entree, 1L))

# Message limité aux 10 premiers numéros ; liste complète dans l'objet.
d <- BASE[rep(1L, 12L), ]; d$contrat <- sprintf("C%02d", 1:12); d$tete <- sprintf("T%02d", 1:12)
d$cause <- "Deces"; rownames(d) <- NULL
x <- refus(prep(d = d))
verifier("12 lignes invalides : message cite les 10 premières, objet les 12",
         identical(x$details$lignes$cause_hors_modalites, 1:12) &&
           identical(x$details$premieres_lignes$cause_hors_modalites, 1:10) &&
           isTRUE(contient(x, "12 ligne(s)", "1, 2, 3, 4, 5, 6, 7, 8, 9, 10 et suivantes")) &&
           !grepl("10, 11", conditionMessage(x), fixed = TRUE))

# --- motifs de tête -----------------------------------------------------------

# F7 : naissances discordantes au sein de T3 (lignes 3 et 4).
d <- avec(naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-07")))
x <- refus(prep(d = d))
verifier("F7 naissances discordantes : refus tete_naissance_incoherente, lignes 3 et 4",
         identical(names(x$details$decompte), "tete_naissance_incoherente") &&
           identical(x$details$lignes$tete_naissance_incoherente, 3:4))
r <- prep(d = d, exclusions = "tete_naissance_incoherente")
verifier("F7 exclu : toutes les lignes de la tête écartées, lignes 1 et 2 conservées identical()",
         identical(r$resultat$lignes_conservees, d[1:2, ]) && identical(r$resultat$exclusions$nombre, 2L))
x <- refus(prep(d = avec(sexe = c("H", "F", "H", "F"))))
verifier("sexes différents dans une tête : refus tete_sexe_incoherent, lignes 3 et 4",
         identical(x$details$lignes$tete_sexe_incoherent, 3:4))
x <- refus(prep(d = avec(sexe = c("H", "F", "H", "F")), segmentation = character(0)))
verifier("sexe déclaré hors segmentation : tete_sexe_incoherent levé quand même (contrôlé s'il est déclaré)",
         identical(x$details$lignes$tete_sexe_incoherent, 3:4))
d <- avec(segment = c("a", "a", "b", "c"))
x <- refus(prep(d = d, roles = c(ROLES, segment = "segment"), segmentation = c("sexe", "segment")))
verifier("segments différents dans une tête, segmentation par segment : refus tete_segment_incoherent, lignes 3 et 4",
         identical(x$details$lignes$tete_segment_incoherent, 3:4))
r <- prep(d = d, roles = c(ROLES, segment = "segment"), segmentation = "sexe")
verifier("segments différents, segment hors segmentation : admis, avertissement role_non_employe",
         "role_non_employe" %in% r$avertissements$code)
# F5 : observation après le décès ; F6 : deux décès à dates différentes.
x <- refus(prep(d = avec(sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-15")),
                         cause = c("rachat", "en_cours", "deces", "autre"))))
verifier("F5 observation après décès : refus tete_observee_apres_deces, les deux lignes listées",
         identical(names(x$details$decompte), "tete_observee_apres_deces") &&
           identical(x$details$lignes$tete_observee_apres_deces, 3:4))
x <- refus(prep(d = avec(sortie = D(c("2019-12-31", NA, "2022-03-01", "2023-01-10")))))
verifier("F6 deux décès à dates différentes : refus tete_observee_apres_deces, lignes 3 et 4",
         identical(x$details$lignes$tete_observee_apres_deces, 3:4))
x <- refus(prep(d = avec(sortie = D(c("2019-12-31", NA, "2022-03-01", NA)),
                         cause = c("rachat", "en_cours", "deces", "en_cours"))))
verifier("contrat en cours (sortie NA = +Inf) d'une tête décédée : refus tete_observee_apres_deces",
         identical(x$details$lignes$tete_observee_apres_deces, 3:4))
verifier("deux lignes closes par le décès le même jour (F4) : cohérentes", is.list(prep()))
# Motifs de tête évalués sur les lignes exemptes de motif de ligne : la ligne
# 4 porte cause_hors_modalites ; T3 n'a plus qu'une ligne propre.
d <- avec(naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-07")),
          cause = c("rachat", "en_cours", "deces", "Deces"))
x <- refus(prep(d = d))
verifier("motif de tête évalué sur les lignes propres seulement : seul cause_hors_modalites (ligne 4)",
         identical(names(x$details$decompte), "cause_hors_modalites") &&
           identical(x$details$lignes$cause_hors_modalites, 4L))
verifier("sans id_tete : aucun motif de tête évalué",
         is.list(prep(d = avec(naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-07"))),
                      roles = ROLES[names(ROLES) != "id_tete"])))

# --- fusion par tête : cas F1 à F4 et H7 (vivant -> décédé) -------------------

tete <- function(naissance, entree, sortie, cause, id = "T")
  data.frame(tete = id, naissance = D(naissance), entree = D(entree), sortie = D(sortie),
             cause = cause, stringsAsFactors = FALSE)
R_T <- c(id_tete = "tete", date_naissance = "naissance", date_entree = "entree",
         date_sortie = "sortie", cause_sortie = "cause")
R_C <- R_T[-1L]
pt <- function(d, roles = R_T, transition = VD, delai = 0)
  prep(d = d, roles = roles, transition = transition, segmentation = character(0), delai = delai)
# Jours observés dans la période [2015-01-01, B) (fin NA = +Inf).
jours <- function(o) {
  f <- o$fin; f[is.na(f)] <- B
  sum(pmax(0, as.numeric(pmin(f, B)) - as.numeric(pmax(o$debut, PER$debut))))
}
obs_attendue <- function(debut, fin, cause) list(debut = D(debut), fin = D(fin), cause = cause)
lire <- function(o) list(debut = o$debut, fin = o$fin, cause = o$cause)

F1 <- tete("1950-03-15", c("2016-01-01", "2018-07-01"), c("2019-12-31", NA), c("rachat", "en_cours"))
r <- pt(F1)
verifier("F1 chevauchants : [2016-01-01, +Inf), en cours, 3288 j dans la période",
         identical(lire(r$resultat$observations), obs_attendue("2016-01-01", NA, "en_cours")) &&
           jours(r$resultat$observations) == 3288)
verifier("F1 relevés : 1 tête multi-contrats, 1 chevauchement, 0 contiguïté, 0 disjoint",
         identical(r$diagnostics$releves[c("tetes", "tetes_multi_contrats", "chevauchements", "contiguites",
                                           "disjoints", "ecarts_un_jour", "fenetres_vides", "observations")],
                   list(tetes = 1L, tetes_multi_contrats = 1L, chevauchements = 1L, contiguites = 0L,
                        disjoints = 0L, ecarts_un_jour = 0L, fenetres_vides = 0L, observations = 1L)))
r <- pt(F1, roles = R_C)
verifier("F1 unité contrat : deux observations, 3836 j",
         nrow(r$resultat$observations) == 2L && jours(r$resultat$observations) == 3836)

F2 <- tete("1952-09-30", c("2016-01-01", "2018-01-01"), c("2018-01-01", "2021-05-10"), c("terme", "deces"))
r <- pt(F2)
verifier("F2 contigus : [2016-01-01, 2021-05-10), décès compté une fois, 1956 j",
         identical(lire(r$resultat$observations), obs_attendue("2016-01-01", "2021-05-10", "deces")) &&
           jours(r$resultat$observations) == 1956)
verifier("F2 relevés : 1 contiguïté, 0 chevauchement",
         r$diagnostics$releves$contiguites == 1L && r$diagnostics$releves$chevauchements == 0L)
r <- pt(F2, roles = R_C)
verifier("F2 unité contrat : 1956 j, 1 décès",
         jours(r$resultat$observations) == 1956 && sum(r$resultat$observations$cause == "deces") == 1L)

F3 <- tete("1949-11-11", c("2016-01-01", "2017-01-02"), c("2017-01-01", NA), c("terme", "en_cours"))
r <- pt(F3)
verifier("F3 disjoints d'un jour : deux composantes [2016-01-01, 2017-01-01) censure et [2017-01-02, +Inf), 3287 j",
         identical(lire(r$resultat$observations),
                   obs_attendue(c("2016-01-01", "2017-01-02"), c("2017-01-01", NA), c("censure", "en_cours"))) &&
           jours(r$resultat$observations) == 3287)
verifier("F3 relevés : 1 disjoint dont 1 écart d'un jour ; avertissement ecarts_un_jour",
         r$diagnostics$releves$disjoints == 1L && r$diagnostics$releves$ecarts_un_jour == 1L &&
           all(c("disjoints", "ecarts_un_jour") %in% r$avertissements$code))
verifier("F3 unité contrat : 3287 j", jours(pt(F3, roles = R_C)$resultat$observations) == 3287)

F4 <- tete("1945-06-06", c("2016-01-01", "2019-01-01"), c("2022-03-01", "2022-03-01"), c("deces", "deces"))
r <- pt(F4)
verifier("F4 décès sur les deux contrats : [2016-01-01, 2022-03-01), un seul décès, 2251 j",
         identical(lire(r$resultat$observations), obs_attendue("2016-01-01", "2022-03-01", "deces")) &&
           jours(r$resultat$observations) == 2251)
r <- pt(F4, roles = R_C)
verifier("F4 unité contrat : 3406 j, deux décès",
         jours(r$resultat$observations) == 3406 && sum(r$resultat$observations$cause == "deces") == 2L)

H7 <- tete("1948-08-20", c("2016-01-01", "2018-07-01"), c("2019-01-01", "2021-05-10"), c("terme", "deces"))
verifier("H7 tête fusionnée : [2016-01-01, 2021-05-10), décès, 1956 j",
         identical(lire(pt(H7)$resultat$observations), obs_attendue("2016-01-01", "2021-05-10", "deces")) &&
           jours(pt(H7)$resultat$observations) == 1956)

# Trois contrats : le troisième chevauche le premier, pas le deuxième
# (maximum cumulé des fins) ; têtes entrelacées dans l'ordre reçu.
d <- rbind(tete("1950-01-01", "2016-01-01", "2020-01-01", "rachat", "A"),
           tete("1960-01-01", "2017-05-05", NA, "en_cours", "Z"),
           tete("1950-01-01", "2017-01-01", "2018-01-01", "terme", "A"),
           tete("1950-01-01", "2019-01-01", "2021-01-01", "autre", "A"))
r <- pt(d)
o <- r$resultat$observations
verifier("maximum cumulé : tête A en une composante [2016-01-01, 2021-01-01) censure ; tête Z [2017-05-05, +Inf)",
         identical(lire(o), obs_attendue(c("2016-01-01", "2017-05-05"), c("2021-01-01", NA), c("censure", "en_cours"))) &&
           identical(o$unite, c("A", "Z")) && identical(o$ligne, c(1L, 2L)) && identical(o$nb_lignes, c(3L, 1L)) &&
           identical(o$date_naissance, D(c("1950-01-01", "1960-01-01"))))
verifier("maximum cumulé : 2 chevauchements, 2 têtes dont 1 multi-contrats",
         r$diagnostics$releves$chevauchements == 2L && r$diagnostics$releves$tetes == 2L &&
           r$diagnostics$releves$tetes_multi_contrats == 1L)
verifier("fusion : lignes conservées identical() à l'entrée (aucune donnée modifiée)",
         identical(r$resultat$lignes_conservees, d))

# Fenêtres vides : sortie avant le début (H10a), décès le jour d'entrée (H11).
d <- rbind(tete("1950-05-05", "2005-01-01", "2014-12-31", "deces", "H10a"),
           tete("1980-01-01", "2018-06-15", "2018-06-15", "deces", "H11"),
           tete("1960-07-01", "2020-03-01", NA, "en_cours", "H1"))
r <- pt(d)
verifier("fenêtres vides : H10a et H11 relevées (2), avertissement fenetres_vides",
         r$diagnostics$releves$fenetres_vides == 2L && "fenetres_vides" %in% r$avertissements$code)
d <- tete("1960-07-01", "2024-11-01", NA, "en_cours")
verifier("délai de 90 j (B = 2024-10-03) : entrée au 2024-11-01, fenêtre vide ; délai 0 : fenêtre non vide",
         pt(d, delai = 90)$diagnostics$releves$fenetres_vides == 1L &&
           pt(d)$diagnostics$releves$fenetres_vides == 0L)

# --- unité contrat et avertissement (E2.4, M46) -------------------------------

MSG_UNITE <- "contrat (aucun identifiant de t"
r <- pt(F4, roles = R_C)
verifier("sans id_tete (vivant -> décédé) : avertissement unite_contrat inscrit dans l'objet, nombre = 2",
         identical(r$avertissements$code, "unite_contrat") && identical(r$avertissements$etape, "donnees") &&
           r$avertissements$nombre == 2 && grepl(MSG_UNITE, r$avertissements$message, fixed = TRUE) &&
           identical(r$resultat$unite_observation, "contrat"))
verifier("avec id_tete : pas d'avertissement unite_contrat", !("unite_contrat" %in% pt(F4)$avertissements$code))
r <- pt(F1, transition = RA)
verifier("en cours -> racheté avec id_tete : unité contrat (M46), relevé de chevauchement sans avertissement, causes d'origine",
         identical(r$resultat$unite_observation, "contrat") && nrow(r$avertissements) == 0L &&
           r$diagnostics$releves$chevauchements == 1L &&
           identical(lire(r$resultat$observations),
                     obs_attendue(c("2016-01-01", "2018-07-01"), c("2019-12-31", NA), c("rachat", "en_cours"))) &&
           identical(r$parametres$transition, "en_cours_rachete"))
verifier("en cours -> racheté : contrôles de tête appliqués (F5 refusé)",
         leve_refus(pt(tete("1945-06-06", c("2016-01-01", "2019-01-01"), c("2022-03-01", "2022-03-15"),
                            c("deces", "autre")), transition = RA), "ligne_invalide"))
r <- pt(F1, roles = R_C, transition = RA)
verifier("en cours -> racheté sans id_tete : pas d'avertissement unite_contrat",
         nrow(r$avertissements) == 0L)
r <- prep(roles = ROLES[names(ROLES) != "id_tete"])
verifier("unité contrat avec id_contrat : unite = id_contrat, ligne = numéro",
         identical(r$resultat$observations$unite, BASE$contrat) && identical(r$resultat$observations$ligne, 1:4) &&
           identical(r$resultat$observations$sexe, BASE$sexe))

# --- âge au dernier anniversaire (A.0, T2 : 29 février) -----------------------

verifier("age_revolu : né le 1964-02-29 -> 56 au 2021-02-28, 57 au 2021-03-01, 59 au 2024-02-28, 60 au 2024-02-29",
         identical(age_revolu(D(rep("1964-02-29", 4)), D(c("2021-02-28", "2021-03-01", "2024-02-28", "2024-02-29"))),
                   c(56L, 57L, 59L, 60L)))
verifier("age_revolu : veille et jour d'anniversaire, NA propagé",
         identical(age_revolu(D(c("1955-04-12", "1955-04-12", NA)), D(c("2020-04-11", "2020-04-12", "2020-01-01"))),
                   c(64L, 65L, NA)))

# --- table de référence (A.1 ; ADR 0007, point 7) ------------------------------

TR <- data.frame(age = c(60L, 61L, 62L, 60L, 61L, 62L), sexe = c("H", "H", "H", "F", "F", "F"),
                 qx = c(0.01, 0.011, 0.012, 0.006, 0.0066, 0.0072), stringsAsFactors = FALSE)
tr <- function(x = TR, ...) {
  a <- list(x = x, nom = "essai", statut = "synthetique", source = "test unitaire",
            parametres = list(A = 2e-4))
  m <- list(...); a[names(m)] <- m
  do.call(table_reference, a)
}
t <- tr()
verifier("table_reference : classe, colonnes age (integer), sexe, qx, attributs",
         identical(class(t), c("table_reference", "data.frame")) &&
           identical(names(t), c("age", "sexe", "qx")) && is.integer(t$age) &&
           identical(attr(t, "nom"), "essai") && identical(attr(t, "statut"), "synthetique") &&
           identical(attr(t, "source"), "test unitaire") && identical(attr(t, "forme_origine"), "qx") &&
           identical(attr(t, "parametres"), list(A = 2e-4)) && identical(t$qx, TR$qx))
verifier("table_reference : sexe « ensemble », âges en double entier, bornes 0 et 1 admis",
         inherits(tr(data.frame(age = c(0, 1, 2), sexe = "ensemble", qx = c(0, 0.5, 1))), "table_reference"))
verifier("table_reference : embarquée sans paramètres admise",
         inherits(tr(statut = "embarquee", parametres = NULL), "table_reference"))
verifier("table_reference : annee (v2), contiguïté par (sexe, annee)",
         inherits(tr(data.frame(age = c(60L, 61L, 60L, 61L), sexe = "H", qx = 0.01, annee = c(2020L, 2020L, 2021L, 2021L))),
                  "table_reference"))
CAS_TABLE <- list(
  list(nom = "q_x > 1", x = transform(TR, qx = c(0.01, 1.2, 0.012, 0.006, 0.0066, 0.0072)),
       motif = "table_qx_hors_bornes", lignes = 2L),
  list(nom = "q_x < 0", x = transform(TR, qx = c(0.01, 0.011, 0.012, -0.006, 0.0066, 0.0072)),
       motif = "table_qx_hors_bornes", lignes = 4L),
  list(nom = "q_x NA", x = transform(TR, qx = c(0.01, NA, 0.012, 0.006, 0.0066, 0.0072)),
       motif = "table_qx_non_fini", lignes = 2L),
  list(nom = "âge en double", x = transform(TR, age = c(60L, 61L, 61L, 60L, 61L, 62L)),
       motif = "table_age_double", lignes = 3L),
  list(nom = "âges non contigus", x = transform(TR, age = c(60L, 61L, 63L, 60L, 61L, 62L)),
       motif = "table_ages_non_contigus", lignes = 3L),
  list(nom = "âges non croissants", x = transform(TR, age = c(60L, 62L, 61L, 60L, 61L, 62L)),
       motif = "table_ages_non_croissants", lignes = 3L),
  list(nom = "sexe hors modalités", x = transform(TR, sexe = c("H", "H", "H", "M", "M", "M")),
       motif = "table_sexe_hors_modalites", lignes = 4:6),
  list(nom = "âge non entier", x = transform(TR, age = c(60, 61.5, 62, 60, 61, 62)),
       motif = "table_colonne_mal_typee", lignes = NULL),
  list(nom = "colonne qx absente", x = TR[c("age", "sexe")], motif = "table_colonne_manquante", lignes = NULL),
  list(nom = "colonne q_x (orthographe de la spécification)", x = setNames(TR, c("age", "sexe", "q_x")),
       motif = "table_colonne_manquante", lignes = NULL),
  list(nom = "colonne inconnue", x = transform(TR, lx = 1), motif = "table_colonne_inconnue", lignes = NULL),
  list(nom = "table vide", x = TR[0, ], motif = "table_vide", lignes = NULL),
  list(nom = "âge négatif (Q10)", x = transform(TR, age = c(-1L, 0L, 1L, 60L, 61L, 62L)),
       motif = "table_colonne_mal_typee", lignes = NULL),
  list(nom = "ensemble avec H (Q10)", x = transform(TR, sexe = c("H", "H", "H", "ensemble", "ensemble", "ensemble")),
       motif = "table_sexes_melanges", lignes = NULL))
for (cas in CAS_TABLE) {
  x <- refus(tr(cas$x))
  verifier(sprintf("table_reference, %s : refus %s%s", cas$nom, cas$motif,
                   if (is.null(cas$lignes)) "" else paste0(", lignes ", paste(cas$lignes, collapse = ","))),
           if (is.null(x)) "aucun refus" else if (!identical(x$motif, cas$motif)) x$motif
           else if (!is.null(cas$lignes) && !identical(x$details$lignes, cas$lignes)) "numéros de ligne"
           else TRUE)
}
verifier("table_reference : âges non contigus au sein d'une même année",
         leve_refus(tr(data.frame(age = c(60L, 61L, 60L, 62L), sexe = "H", qx = 0.01, annee = c(2020L, 2020L, 2021L, 2021L))),
                    "table_ages_non_contigus"))
x <- refus(tr(transform(TR, age = c(-1L, 0L, 1L, 60L, 61L, 62L))))
verifier("table_reference : âge négatif, message explicite (entier >= 0)", contient(x, "age", ">= 0"))
verifier("table_reference : ensemble et H dans la même année refusés, dans deux années différentes admis",
         leve_refus(tr(data.frame(age = c(60L, 61L, 60L, 61L), sexe = c("H", "H", "ensemble", "ensemble"), qx = 0.01,
                                  annee = 2020L)), "table_sexes_melanges") &&
           inherits(tr(data.frame(age = c(60L, 61L, 60L, 61L), sexe = c("H", "H", "ensemble", "ensemble"), qx = 0.01,
                                  annee = c(2020L, 2020L, 2021L, 2021L))), "table_reference"))
verifier("table_reference : statut inconnu", leve_refus(tr(statut = "officielle"), "table_statut_inconnu"))
verifier("table_reference : synthétique sans paramètres",
         leve_refus(tr(parametres = NULL), "table_parametres_manquants"))
verifier("table_reference : importée avec paramètres",
         leve_refus(tr(statut = "importee"), "table_parametres_inattendus"))
verifier("table_reference : source manquante", leve_refus(tr(source = ""), "table_source_manquante"))
verifier("table_reference : nom manquant", leve_refus(tr(nom = NA_character_), "table_nom_manquant"))
verifier("table_reference : forme d'origine inconnue", leve_refus(tr(forme_origine = "mu"), "table_forme_inconnue"))
verifier("table_reference : data.frame exigé", leve_refus(tr(as.list(TR)), "table_mal_formee"))

# --- cas limites ----------------------------------------------------------------

verifier("aucune ligne : refus base_vide (Q10)", leve_refus(pt(F1[0, ]), "base_vide"))
d <- tete("1950-01-01", "2016-01-01", NA, "deces", "A")
x <- refus(prep(d = d, roles = R_T, segmentation = character(0), exclusions = "sortie_manquante"))
verifier("toutes les lignes exclues : refus aucune_ligne_conservee, message nomme le motif (Q10)",
         identical(x$motif, "aucune_ligne_conservee") && isTRUE(contient(x, "sortie_manquante")) &&
           identical(x$details$lignes_exclues, 1L))

# Constat M1 : colonne déclarée présente deux fois sous le même nom.
x <- refus(pt(cbind(F1, sortie = D(c("2030-01-01", "2030-01-01")))))
verifier("colonne présente deux fois dans les données : refus colonne_ambigue, message nomme colonne et rôle",
         identical(x$motif, "colonne_ambigue") && isTRUE(contient(x, "sortie", "date_sortie")))

# Constat m2 : étendue des dates hors de la précision de la fusion.
d <- tete("1950-01-01", c("2016-01-01", "2017-01-01"), c("2018-01-01", "2018-01-01"), c("rachat", "terme"))
d$sortie[1] <- structure(5e15, class = "Date")
# Depuis #19, le domaine des dates [0001-01-01, 9999-12-31] est contrôlé en amont
# (refus date_hors_domaine) : 5e15 jours n'atteint plus la fusion par tête, dont
# le refus dates_hors_plage reste atteint par appel direct (étendue de 5e15 jours).
verifier("date de sortie 5e15 jours : refus date_hors_domaine (#19), avant la fusion par tête",
         leve_refus(pt(d), "date_hors_domaine"))
verifier("fusion par tête, étendue de 5e15 jours (appel direct) : refus dates_hors_plage (et non erreur ordinaire)",
         leve_refus(tablexp:::fusionner_intervalles(c(1L, 2L), c(0, 0), c(5e15, 1)), "dates_hors_plage"))

# Constat m1 : cause des observations toujours en chaîne.
verifier("cause des observations : character (unité tête et unité contrat)",
         is.character(pt(F4)$resultat$observations$cause) && is.character(pt(F4, roles = R_C)$resultat$observations$cause))

# --- Q7 : lecture B d'E2.4 (décision du mainteneur du 16/10/2026) ------------

# Deux contrats d'une tête : A (entrée 2016-01-01, décès à sA), B en cours
# (entrée 2018-01-01, date de sortie sB). B = 2025-01-01 (délai 0).
q7 <- function(sA, sB, delai = 0, cA = "deces")
  pt(tete("1950-01-01", c("2016-01-01", "2018-01-01"), c(sA, sB), c(cA, "en_cours")), delai = delai)
for (cas in list(
  list(nom = "1 : décès 2025-02-01 (>= B), B en cours sortie NA", sA = "2025-02-01", sB = NA, delai = 0),
  list(nom = "2 : décès 2025-02-01, B en cours sortie 2025-01-15", sA = "2025-02-01", sB = "2025-01-15", delai = 0),
  list(nom = "3 : décès 2025-02-01, B en cours sortie 2025-03-01", sA = "2025-02-01", sB = "2025-03-01", delai = 0),
  list(nom = "4 : décès 2024-12-01, délai 90 (B = 2024-10-03), B en cours sortie NA", sA = "2024-12-01", sB = NA,
       delai = 90))) {
  r <- tryCatch(q7(cas$sA, cas$sB, cas$delai), refus_tablexp = function(e) e)
  verifier(sprintf("Q7 cas %s : accepté, relevé = 1, composante en cours [2016-01-01, +Inf), 0 décès, avertissement", cas$nom),
           if (inherits(r, "refus_tablexp")) paste("refus", r$motif)
           else if (!identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L)) "relevé"
           else if (!identical(lire(r$resultat$observations), obs_attendue("2016-01-01", NA, "en_cours"))) "composante"
           else if (!("observations_apres_deces_hors_periode" %in% r$avertissements$code)) "avertissement"
           else TRUE)
}
x <- refus(q7("2022-03-01", NA))
verifier("Q7 cas 5 : décès 2022-03-01 (< B), B en cours : refus tete_observee_apres_deces, lignes 1 et 2",
         identical(x$details$lignes$tete_observee_apres_deces, 1:2))
x <- refus(pt(tete("1950-01-01", c("2010-01-01", "2012-01-01"), c("2014-06-01", NA), c("deces", "en_cours"))))
verifier("Q7 : décès avant le début de période, B en cours : refus tete_observee_apres_deces",
         identical(x$details$lignes$tete_observee_apres_deces, 1:2))
r <- pt(tete("1950-01-01", c("2016-01-01", "2018-01-01"), c("2025-02-01", "2025-03-01"), c("deces", "deces")))
verifier("Q7 : deux décès à dates différentes, tous deux >= B : accepté, relevé = 1",
         identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L))
verifier("Q7 : sans observation après décès, relevé = 0 ; sans id_tete, relevé NA",
         identical(pt(F4)$diagnostics$releves$observations_apres_deces_hors_periode, 0L) &&
           identical(pt(F4, roles = R_C)$diagnostics$releves$observations_apres_deces_hors_periode, NA_integer_))

# Relevé en unité contrat (E2.4, M46, M48 : contrôle par identifiant de tête
# quelle que soit l'unité). Période 2016-01-01 -> 2023-12-31, délai 30 :
# fin effective 2023-12-01, B = 2023-12-02. Tête T1 : contrat décédé à sA,
# second contrat en cours.
PER_RA <- list(debut = D("2016-01-01"), fin = D("2023-12-31"))
t1 <- function(sA, id = "T1", naissance = c("1950-01-01", "1950-01-01"))
  tete(naissance, c("2016-01-01", "2018-01-01"), c(sA, NA), c("deces", "en_cours"), id)
pra <- function(d, transition = RA, exclusions = character(0))
  prep(d = d, roles = R_T, transition = transition, segmentation = character(0), periode = PER_RA,
       delai = 30, exclusions = exclusions)
r <- pra(t1("2023-12-20"))
verifier("relevé, unité contrat (en_cours -> rachat), décès 2023-12-20 >= B = 2023-12-02 : relevé = 1, averti",
         identical(r$resultat$unite_observation, "contrat") &&
           identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L) &&
           "observations_apres_deces_hors_periode" %in% r$avertissements$code)
x <- refus(pra(t1("2023-11-20")))
verifier("unité contrat, décès 2023-11-20 < B : refus tete_observee_apres_deces, lignes 1 et 2",
         identical(x$motif, "ligne_invalide") &&
           identical(x$details$lignes$tete_observee_apres_deces, 1:2))

# Exclusion de tête déclarée : T3 (naissances discordantes, même profil de
# décès après B) est écartée et n'entre pas dans le relevé (règle d'actuary :
# relevé calculé sur les seules lignes conservées).
d <- rbind(t1("2023-12-20"), t1("2023-12-20", "T3", c("1950-01-01", "1951-01-01")))
for (tr_ in list(RA, VD)) {
  r <- pra(d, transition = tr_, exclusions = "tete_naissance_incoherente")
  verifier(sprintf("relevé, %s -> %s, T3 exclue (tete_naissance_incoherente) : relevé = 1 (et non 2), lignes 1 et 2 conservées",
                   tr_$depart, tr_$arrivee),
           identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L) &&
             identical(r$resultat$numeros_conserves, 1:2) &&
             any(grepl("Relevé : 1 ", r$avertissements$message[
               r$avertissements$code == "observations_apres_deces_hors_periode"], fixed = TRUE)))
}
# Deux exclusions de tête déclarées : T1 (décès >= B, relevé), T2 (décès
# 2023-11-20 < B, écartée par tete_observee_apres_deces), T3 (écartée par
# tete_naissance_incoherente) : seules les lignes de T1 sont conservées.
d <- rbind(t1("2023-12-20"), t1("2023-11-20", "T2"), t1("2023-12-20", "T3", c("1950-01-01", "1951-01-01")))
for (tr_ in list(RA, VD)) {
  r <- pra(d, transition = tr_, exclusions = c("tete_naissance_incoherente", "tete_observee_apres_deces"))
  verifier(sprintf("relevé, %s -> %s, T2 et T3 exclues (deux motifs de tête) : relevé = 1, lignes 1 et 2 conservées",
                   tr_$depart, tr_$arrivee),
           identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L) &&
             identical(r$resultat$numeros_conserves, 1:2) &&
             identical(r$resultat$exclusions$nombre, c(2L, 2L)))
}
# Non-régression (non une preuve de la règle) : résultat identique à
# l'ancien calcul, les lignes à motif de ligne étant déjà hors de `propres`.
d <- rbind(t1("2023-12-20"), tete("1950-01-01", "2017-01-01", "2016-06-01", "rachat", "T1"))
r <- pra(d, exclusions = "sortie_avant_entree")
verifier("relevé : tête dont une seule ligne est exclue (motif de ligne) comptée sur ses lignes conservées (relevé = 1)",
         identical(r$resultat$numeros_conserves, 1:2) &&
           identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L))

# Borne B au jour près (B = fin - délai + 1) : période PER (fin 2024-12-31).
for (cas in list(list(delai = 0, sA = "2024-12-31", refuse = TRUE),
                 list(delai = 0, sA = "2025-01-01", refuse = FALSE),
                 list(delai = 30, sA = "2024-12-01", refuse = TRUE),
                 list(delai = 30, sA = "2024-12-02", refuse = FALSE))) {
  r <- tryCatch(pt(t1(cas$sA), delai = cas$delai), refus_tablexp = function(e) e)
  verifier(sprintf("borne B, délai %d, décès %s : %s", cas$delai, cas$sA,
                   if (cas$refuse) "refus tete_observee_apres_deces" else "accepté, relevé = 1"),
           if (cas$refuse) inherits(r, "refus_tablexp") &&
             identical(r$details$lignes$tete_observee_apres_deces, 1:2)
           else !inherits(r, "refus_tablexp") &&
             identical(r$diagnostics$releves$observations_apres_deces_hors_periode, 1L))
}

# --- Q9 : colonnes des observations, lignes exclues, correspondance -----------

d <- data.frame(id = c("A", "B"), t = c(1, 1), n = D(c("1950-01-01", "1950-01-01")),
                e = D(c("2016-01-01", "2017-01-01")), s = D(c("2018-01-01", NA)), c = c("terme", "en_cours"),
                g = factor(c("x", "y")), stringsAsFactors = FALSE)
R9 <- c(id_contrat = "id", id_tete = "t", date_naissance = "n", date_entree = "e", date_sortie = "s",
        cause_sortie = "c", segment = "g")
r <- prep(d = d, roles = R9, segmentation = character(0))
verifier("Q9 (cas q9.R) : segment déclaré hors segmentation -> pas de colonne segment dans les observations",
         !("segment" %in% names(r$resultat$observations)) && "role_non_employe" %in% r$avertissements$code)
verifier("Q9 : segment employé et incohérent dans la tête, tout exclu -> refus aucune_ligne_conservee",
         leve_refus(prep(d = d, roles = R9, segmentation = "segment", exclusions = "tete_segment_incoherent"),
                    "aucune_ligne_conservee"))
d$g <- factor(c("x", "x"))
r <- prep(d = d, roles = R9, segmentation = "segment")
verifier("Q9 : segment employé -> colonne segment, type d'origine (factor) conservé",
         is.factor(r$resultat$observations$segment) && identical(as.character(r$resultat$observations$segment), "x"))
r <- pt(F1, transition = RA)
verifier("rachat (unité contrat) avec id_tete : colonne id_tete dans les observations",
         identical(r$resultat$observations$id_tete, c("T", "T")))
d <- rbind(tete("1950-01-01", "2016-01-01", "2020-01-01", "rachat", "A"),
           tete("1960-01-01", "2017-05-05", NA, "en_cours", "Z"),
           tete("1950-01-01", "2017-01-01", "2018-01-01", "terme", "A"),
           tete("1950-01-01", "2019-01-01", "2021-01-01", "autre", "A"))
verifier("correspondance ligne conservée -> observation : A (lignes 1, 3, 4) -> 1, Z (ligne 2) -> 2",
         identical(pt(d)$resultat$observation_des_lignes, c(1L, 2L, 1L, 1L)) &&
           identical(pt(d, roles = R_C)$resultat$observation_des_lignes, 1:4))
d <- avec(sortie = D(c("2015-12-31", NA, "2022-03-01", "2022-03-01")),
          cause = c("Rachat", "en_cours", "deces", "deces"))
r <- prep(d = d, exclusions = c("cause_hors_modalites", "sortie_avant_entree"))
verifier("ligne 1 exclue pour deux motifs : décomptes par motif 1 et 1, lignes exclues distinctes 1",
         identical(r$resultat$exclusions$nombre, c(1L, 1L)) && identical(r$resultat$nombre_lignes_exclues, 1L))

# --- fusion : vérifiée contre un second algorithme (boucle naïve écrite dans
# le test), tête par tête -------------------------------------------------------

# Nature : valeur attendue calculée par un second algorithme (boucle), sur un
# jeu déterministe sans tirage : 600 têtes de 2 ou 3 lignes (300 de chaque,
# mesuré), dans un ordre mélangé, entrées et durées par arithmétique
# modulaire, 1 ligne sur 7 en cours (+Inf) ; 1123 composantes, 376
# chevauchements, 1 contiguïté, 523 disjonctions (mesuré).
fusionner_intervalles <- tablexp:::fusionner_intervalles
i <- seq_len(1500L)
g <- (i * 7919L) %% 600L + 1L
e <- as.numeric((i * 104729L) %% 3001L)
f <- e + (i * 1299709L) %% 801L
f[i %% 7L == 0L] <- Inf
r <- fusionner_intervalles(g, e, f)
naif <- do.call(rbind, lapply(split(i, g), function(k) {
  k <- k[order(e[k])]; out <- NULL; deb <- e[k[1L]]; fc <- f[k[1L]]
  for (j in k[-1L]) {
    if (e[j] <= fc) fc <- max(fc, f[j]) else { out <- rbind(out, c(deb, fc)); deb <- e[j]; fc <- f[j] }
  }
  rbind(out, c(deb, fc))
}))
verifier("fusion vectorisée = fusion naïve (600 têtes, 1500 lignes, ordre mélangé, 1123 composantes)",
         identical(r$debut, unname(naif[, 1L])) && identical(r$fin, unname(naif[, 2L])) &&
           length(unique(g)) == 600L && nrow(naif) == 1123L && any(is.infinite(r$fin)))

# --- chaîne unique et liste aux noms exacts (#48) -------------------------------

# table_reference() testait is.character() (attributs admis) : la forme exacte
# refuse désormais une chaîne nommée ou à attribut, sous le motif existant.
CHAINES_ATTR <- list("nommée" = c(n = "qx"), "à attribut foo" = structure("qx", foo = "x"))
for (k in names(CHAINES_ATTR)) {
  v <- CHAINES_ATTR[[k]]
  verifier(sprintf("table_reference : nom %s -> refus table_nom_manquant", k),
           leve_refus(tr(nom = v), "table_nom_manquant"))
  verifier(sprintf("table_reference : statut %s -> refus table_statut_inconnu", k),
           leve_refus(tr(statut = v), "table_statut_inconnu"))
  verifier(sprintf("table_reference : source %s -> refus table_source_manquante", k),
           leve_refus(tr(source = v), "table_source_manquante"))
  verifier(sprintf("table_reference : forme_origine %s -> refus table_forme_inconnue", k),
           leve_refus(tr(forme_origine = v), "table_forme_inconnue"))
}
verifier("table_reference : nom, statut, source et forme_origine nus admis (inchangé)",
         inherits(tr(forme_origine = "qx"), "table_reference"))
# Nom au codage bytes (#111) : cité sous un repère dans le message, sans erreur R ;
# le nom de l'objet reste celui reçu. Pour un nom ordinaire, le texte est inchangé.
TR_SEXE <- TR; TR_SEXE$sexe[1L] <- "Z"
TR_EN_PLUS <- TR; TR_EN_PLUS$zz <- 1
NOM_BYTES_CAS <- list(
  list("statut inconnu", list(statut = "zzz"), "table_statut_inconnu"),
  list("source vide", list(source = ""), "table_source_manquante"),
  list("paramètres manquants", list(parametres = NULL), "table_parametres_manquants"),
  list("paramètres inattendus", list(statut = "importee", parametres = list(A = 1)), "table_parametres_inattendus"),
  list("forme_origine inconnue", list(forme_origine = "zz"), "table_forme_inconnue"),
  list("x non data.frame", list(x = 1), "table_mal_formee"),
  list("colonne inconnue", list(x = TR_EN_PLUS), "table_colonne_inconnue"),
  list("sexe hors modalités", list(x = TR_SEXE), "table_sexe_hors_modalites"))
for (k in NOM_BYTES_CAS) {
  x <- refus(do.call(tr, c(list(nom = OCTETS), k[[2]])))
  verifier(sprintf("table_reference : nom au codage bytes, %s -> refus %s, nom remplacé par un repère", k[[1]], k[[3]]),
           identical(x$motif, k[[3]]) && isTRUE(contient(x, "Table « <chaîne d'encodage bytes> »")) ||
             identical(x$motif, k[[3]]) && isTRUE(contient(x, "Table synthétique « <chaîne d'encodage bytes> »")))
  y <- refus(do.call(tr, c(list(nom = "essai"), k[[2]])))
  verifier(sprintf("table_reference : nom ordinaire, %s -> même refus, nom cité tel quel", k[[1]]),
           identical(y$motif, k[[3]]) &&
             identical(conditionMessage(y), sub("<chaîne d'encodage bytes>", "essai", conditionMessage(x), fixed = TRUE)))
}
verifier("table_reference : nom au codage bytes admis hors refus, nom de l'objet inchangé",
         identical(attr(tr(nom = OCTETS), "nom"), OCTETS))
# Sous LC_ALL=C (sous-processus, bibliothèque des batteries) : un nom qui n'est pas bytes
# est cité exactement comme le ferait sprintf() sur le nom reçu (oracle : la formule
# d'avant #111, octet pour octet, sans traduction ajoutée par le masquage (un nom latin1
# reste « déc » en UTF-8 valide, jamais « d<e9>c ») ; le nom bytes est remplacé par le repère.
script_nom <- tempfile(fileext = ".R")
writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(.harnais$bibliotheque)),
  "tr <- function(nom) tryCatch(table_reference(data.frame(age = 0:1, sexe = 'ensemble', qx = c(.1, .2)), nom, 'zzz', 's'),",
  "                             refus_tablexp = function(c) conditionMessage(c))",
  "L1 <- 'd\\xe9c'; Encoding(L1) <- 'latin1'; B <- 'd\\xe9c'; Encoding(B) <- 'bytes'",
  "gabarit <- paste0('Table \\u00ab %s \\u00bb : statut (cha\\u00eene sans attribut) \\u00e0 prendre dans ',",
  "  '\\u00ab synthetique \\u00bb, \\u00ab embarquee \\u00bb, \\u00ab importee \\u00bb.')",
  "repere <- '<cha\\u00eene d\\'encodage bytes>'",
  "cas <- list(ascii = list('essai', 'essai'), latin1 = list(L1, L1), utf8 = list('d\\u00e9c\\u00e8s', 'd\\u00e9c\\u00e8s'),",
  "            bytes = list(B, repere))",
  "cat('LOCALE', Sys.getlocale('LC_CTYPE'), '\\n')",
  "for (k in names(cas)) {",
  "  m <- tr(cas[[k]][[1]]); e <- sprintf(gabarit, cas[[k]][[2]])",
  "  cat('CAS', k, identical(charToRaw(m), charToRaw(e)), if (!identical(charToRaw(m), charToRaw(e))) paste(charToRaw(m), collapse = ''), '\\n')",
  "}"), script_nom)
r_nom <- executer_rscript(script_nom, env = "LC_ALL=C")
unlink(script_nom)
for (k in c("ascii", "latin1", "utf8", "bytes"))
  verifier(sprintf("table_reference : nom %s sous LC_ALL=C, message égal octet pour octet à sprintf() sur le nom (repère pour bytes)", k), {
    loc <- trimws(sub("^LOCALE ", "", grep("^LOCALE ", r_nom$sortie, value = TRUE)))
    l <- grep(paste0("^CAS ", k, " "), r_nom$sortie, value = TRUE)
    if (r_nom$statut == 0L && identical(loc, "C") && length(l) == 1L && grepl(" TRUE ", paste0(l, " "), fixed = TRUE)) TRUE
    else paste("statut", r_nom$statut, ":", paste(r_nom$sortie, collapse = " | "))
  })
# Un nom d'état vide était refusé au second contrôle de la transition (non prise
# en charge) ; il l'est au premier (forme), sous le même motif.
verifier("transition : nom d'état vide (depart, puis arrivee) -> refus transition_inconnue",
         isTRUE(leve_refus(prep(transition = list(depart = "", arrivee = "deces")), "transition_inconnue")) &&
           isTRUE(leve_refus(prep(transition = list(depart = "vivant", arrivee = "")), "transition_inconnue")))

# --- ligne en_cours dont la date de sortie précède l'entrée : refus voulu (#20) --

# Avis d'actuary du 19/10/2026 (issue #20) : le refus est voulu. La date de
# sortie d'une ligne `en_cours` atteste que le contrat est en cours jusqu'à
# cette date ; antérieure à l'entrée, la ligne se contredit (cause fausse, ou
# dates fausses). Le moteur ne choisit pas entre la cause et les dates (E2.7 :
# aucune correction automatique) ; le recours est une exclusion déclarée des
# motifs. Cas (i) à (v) de l'avis ; attendus calculés à la main (ligne 2 de
# BASE modifiée, période 2015-01-01 à 2024-12-31).
EC_AVANT <- avec(entree = D(c("2016-01-01", "2005-01-01", "2016-01-01", "2019-01-01")),
                 sortie = D(c("2019-12-31", "2004-01-01", "2022-03-01", "2022-03-01")))
x <- refus(prep(d = EC_AVANT))
verifier("(i) en_cours, entrée 2005-01-01, sortie 2004-01-01 : refus ligne_invalide, motifs sortie_avant_entree et en_cours_avant_fin, ligne 2",
         if (is.null(x)) "aucun refus" else if (!identical(x$motif, "ligne_invalide")) x$motif
         else if (!identical(names(x$details$decompte), c("sortie_avant_entree", "en_cours_avant_fin")) ||
                    !identical(unname(x$details$decompte), c(1L, 1L))) "décompte"
         else if (!identical(x$details$lignes, list(sortie_avant_entree = 2L, en_cours_avant_fin = 2L))) "numéros de ligne"
         else if (!identical(x$details$total_lignes, 1L)) "total de lignes"
         else contient(x, "sortie_avant_entree", "en_cours_avant_fin"))
# Entrée après la fin de la période (2026-01-01 > 2024-12-31) : la sortie
# antérieure à l'entrée reste postérieure à la fin, en_cours_avant_fin n'est pas levé.
x <- refus(prep(d = avec(entree = D(c("2016-01-01", "2026-01-01", "2016-01-01", "2019-01-01")),
                         sortie = D(c("2019-12-31", "2025-06-01", "2022-03-01", "2022-03-01")))))
verifier("(ii) en_cours, entrée après la fin de la période, sortie antérieure à l'entrée : refus par sortie_avant_entree seul, ligne 2",
         if (is.null(x)) "aucun refus" else if (!identical(x$motif, "ligne_invalide")) x$motif
         else if (!identical(names(x$details$decompte), "sortie_avant_entree") ||
                    !identical(unname(x$details$decompte), 1L)) "décompte"
         else if (!identical(x$details$lignes, list(sortie_avant_entree = 2L))) "numéros de ligne"
         else contient(x, "sortie_avant_entree"))
r <- tryCatch(prep(d = avec(entree = D(c("2016-01-01", "2005-01-01", "2016-01-01", "2019-01-01")),
                            sortie = D(c("2019-12-31", "2024-12-31", "2022-03-01", "2022-03-01")))),
              refus_tablexp = function(e) e)
verifier("(iii) en_cours, entrée 2005-01-01, sortie = fin de la période : ligne acceptée, aucune exclusion",
         if (inherits(r, "refus_tablexp")) paste("refus", r$motif, conditionMessage(r))
         else if (!identical(r$resultat$numeros_conserves, 1:4)) "numéros conservés"
         else if (!identical(r$resultat$nombre_lignes_exclues, 0L)) "nombre de lignes exclues"
         else TRUE)
r <- tryCatch(prep(d = EC_AVANT, exclusions = c("sortie_avant_entree", "en_cours_avant_fin")),
              refus_tablexp = function(e) e)
verifier("(iv) exclusion déclarée des deux motifs : ligne 2 acceptée à l'exclusion, une ligne exclue (comptée sous chaque motif)",
         if (inherits(r, "refus_tablexp")) paste("refus", r$motif, conditionMessage(r))
         else if (!identical(r$resultat$nombre_lignes_exclues, 1L)) "nombre de lignes exclues"
         else if (!identical(r$resultat$numeros_conserves, c(1L, 3L, 4L))) "numéros conservés"
         else if (!identical(r$resultat$lignes_conservees, EC_AVANT[-2L, , drop = FALSE])) "lignes conservées"
         else if (!identical(r$resultat$exclusions,
                             data.frame(motif = c("sortie_avant_entree", "en_cours_avant_fin"), nombre = c(1L, 1L),
                                        stringsAsFactors = FALSE))) "décompte des exclusions"
         else if (!identical(r$resultat$lignes_exclues,
                             list(sortie_avant_entree = 2L, en_cours_avant_fin = 2L))) "liste des exclusions"
         else if (!identical(sum(r$avertissements$code == "exclusion"), 2L)) "avertissements d'exclusion (un par motif)"
         else TRUE)
x <- refus(prep(d = EC_AVANT, exclusions = "sortie_avant_entree"))
verifier("(v) exclusion de sortie_avant_entree seule : refus par en_cours_avant_fin (lever un seul motif ne change rien au cas), ligne 2",
         if (is.null(x)) "aucun refus" else if (!identical(x$motif, "ligne_invalide")) x$motif
         else if (!identical(names(x$details$decompte), "en_cours_avant_fin") ||
                    !identical(unname(x$details$decompte), 1L)) "décompte"
         else if (!identical(x$details$lignes, list(en_cours_avant_fin = 2L))) "numéros de ligne"
         else contient(x, "en_cours_avant_fin"))

# --- dates hors du domaine [0001-01-01, 9999-12-31] (#19 ; ADR 0011, point 4) ---

# Nature : bornes en jours depuis 1970-01-01 calculées hors R (Python 3.13 :
# date(1,1,1).toordinal() - date(1970,1,1).toordinal() = 1 - 719163 = -719162 ;
# date(9999,12,31).toordinal() - 719163 = 3652059 - 719163 = 2932896) ; les
# jours voisins (-719163, 2932897) portent les années 0 et 10000.
J <- function(j) structure(j, class = "Date")
verifier("domaine des dates : composantes_date() rend 0001-01-01 en -719162, l'année 0 la veille, 9999-12-31 en 2932896, l'année 10000 le lendemain", {
  cd <- tablexp:::composantes_date(c(-719163, -719162, 2932896, 2932897))
  identical(cd$an, c(0L, 1L, 9999L, 10000L)) && identical(cd$mois, c(11L, 0L, 11L, 0L)) &&
    identical(cd$jour, c(31L, 1L, 31L, 1L))
})
verifier("dates_hors_domaine : bornes incluses, manquants (NA, NaN) jamais hors domaine, infinis hors domaine",
         identical(tablexp:::dates_hors_domaine(c(-719163, -719162, 0, 2932896, 2932897, NA, NaN, Inf, -Inf)),
                   c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)))
for (cas in list(list("naissance", "date_naissance", 2L), list("entree", "date_entree", 3L),
                 list("sortie", "date_sortie", 1L)))
  for (v in c(-719163, 2932897)) {
    col <- BASE[[cas[[1L]]]]; col[cas[[3L]]] <- J(v)
    dd <- BASE; dd[[cas[[1L]]]] <- col
    x <- refus(prep(d = dd))
    verifier(sprintf("date hors domaine (rôle %s, jour %d) : refus date_hors_domaine, ligne %d citée", cas[[2L]], v, cas[[3L]]),
             if (is.null(x)) "aucun refus" else if (!identical(x$motif, "date_hors_domaine")) x$motif
             else if (!identical(x$details$lignes, stats::setNames(list(cas[[3L]]), cas[[2L]]))) "numéros de ligne"
             else contient(x, cas[[2L]], cas[[1L]], sprintf("lignes %d", cas[[3L]])))
  }
verifier("date hors domaine dans deux rôles (naissance ligne 2, sortie lignes 1 et 3) : un seul refus qui nomme les deux rôles, colonnes et lignes", {
  dd <- BASE
  dd$naissance[2L] <- J(-719163); dd$sortie[c(1L, 3L)] <- J(2932897)
  x <- refus(prep(d = dd))
  if (is.null(x)) "aucun refus" else if (!identical(x$motif, "date_hors_domaine")) x$motif
  else if (!identical(x$details$role, c("date_naissance", "date_sortie")) ||
           !identical(x$details$colonne, c("naissance", "sortie")) ||
           !identical(x$details$lignes, list(date_naissance = 2L, date_sortie = c(1L, 3L)))) "détails du refus"
  else contient(x, "« date_naissance », colonne « naissance » : 1 date(s) (premières lignes 2)",
                "« date_sortie », colonne « sortie » : 2 date(s) (premières lignes 1, 3)")
})
verifier("date hors domaine dans une colonne non déclarée : sans effet",
         is.list(prep(d = avec(autre_date = J(c(-719163, 2932897, NA, NaN))))))
verifier("dates aux bornes du domaine (0001-01-01, 9999-12-31) : acceptées par le contrôle des rôles",
         identical(tablexp:::controler_roles(
           avec(naissance = J(c(-719162, 2932896, -719162, 2932896)), entree = J(rep(-719162, 4L))),
           ROLES, "sexe"), ROLES))
verifier("période : borne début à 0000-12-31 (-719163) ou borne fin à 10000-01-01 (2932897) : refus date_hors_domaine",
         leve_refus(prep(periode = list(debut = J(-719163), fin = PER$fin)), "date_hors_domaine") &&
           leve_refus(prep(periode = list(debut = PER$debut, fin = J(2932897))), "date_hors_domaine") &&
           leve_refus(prep(periode = list(debut = J(-719163), fin = J(2932897))), "date_hors_domaine"))
verifier("période : bornes 0001-01-01 et 9999-12-31 acceptées par le contrôle des paramètres",
         is.list(tablexp:::controler_parametres_donnees(VD, list(debut = J(-719162), fin = J(2932896)), 0, "sexe",
                                                          character(0))))
verifier("période : borne non finie ou manquante reste periode_invalide (non date_hors_domaine)",
         leve_refus(prep(periode = list(debut = J(NA_real_), fin = PER$fin)), "periode_invalide") &&
           leve_refus(prep(periode = list(debut = PER$debut, fin = J(Inf))), "periode_invalide"))
# Années 1 à 999 : admises, relevées par age_aberrant, exclues par une règle déclarée.
d <- avec(naissance = J(c(-719162, as.numeric(D("1960-07-01")), as.numeric(D("1945-06-06")),
                          as.numeric(D("1945-06-06")))))  # ligne 1 : 0001-01-01
x <- refus(prep(d = d))
verifier("naissance en l'an 1 (0001-01-01) : non refusée comme date, relevée par age_aberrant (ligne 1)",
         identical(x$motif, "ligne_invalide") && identical(names(x$details$decompte), "age_aberrant") &&
           identical(x$details$lignes$age_aberrant, 1L))
r <- prep(d = d, exclusions = "age_aberrant")
verifier("naissance en l'an 1 avec la règle age_aberrant déclarée : ligne 1 exclue, comptée",
         identical(r$resultat$lignes_exclues, list(age_aberrant = 1L)) && r$resultat$nombre_lignes_exclues == 1L)
# 0019-05-12 = jour -712457 (Python : date(19,5,12).toordinal() - date(1970,1,1).toordinal())
d <- avec(naissance = J(c(-712457, as.numeric(D("1960-07-01")), as.numeric(D("1945-06-06")),
                          as.numeric(D("1945-06-06")))))
verifier("naissance en l'an 19 (0019-05-12) : relevée par age_aberrant (ligne 1), non refusée comme date",
         identical(refus(prep(d = d))$details$lignes$age_aberrant, 1L))
