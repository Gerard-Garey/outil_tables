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

# Contrôles du module donnees dans l'ordre de construire_table() (paramètres
# par controler_parametres_donnees(), que valider_plan() appelle, puis rôles
# par controler_roles()), puis l'étape sur les valeurs validées : la fonction
# d'étape ne contrôle plus rien (#82).
prep <- function(d = BASE, roles = ROLES, exclusions = character(0), transition = VD,
                 segmentation = "sexe", periode = PER, delai = 0) {
  p <- tablexp:::controler_parametres_donnees(transition, periode, delai, segmentation, exclusions)
  recette <- tablexp:::controler_roles(d, roles, segmentation)
  preparer_donnees(d, roles, p, segmentation, exclusions, recette)
}
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
verifier("catalogue_roles() : 23 rôles (type_ligne ajouté, #180, commit (4a)), colonnes role, famille, type_admis, recettes (sans usage ; ADR 0016, point 3)",
         nrow(cat_r) == 23L && identical(names(cat_r), c("role", "famille", "type_admis", "recettes")))
verifier("catalogue_roles() : rôles requis de la recette = date_naissance, date_entree, date_sortie, cause_sortie (CATALOGUE_RECETTES)",
         identical(tablexp:::CATALOGUE_RECETTES$requis[[1L]],
                   c("date_naissance", "date_entree", "date_sortie", "cause_sortie")))
NON_PRIS <- c("age_fourni", "date_age_fourni", "date_evenement", "montant", "etat_depart", "etat_arrivee",
              "date_changement")
AGREGATS <- c("age", "annee", "exposition_centrale", "exposition_initiale", "evenements")
STOCKS <- c("type_ligne", "date_inventaire")
verifier("catalogue_roles() : rôles qu'aucune recette n'emploie signalés (recettes vide), agregats et stocks_successifs pour leurs rôles propres, les trois recettes pour sexe et segment, lignes_dates_completes pour date_entree, « lignes_dates_completes, stocks_successifs » pour les autres (#169, #180 commit (4b))", {
  v <- stats::setNames(cat_r$recettes, cat_r$role)
  identical(cat_r$role[cat_r$recettes == ""], NON_PRIS) && all(v[AGREGATS] == "agregats") &&
    all(v[STOCKS] == "stocks_successifs") &&
    all(v[c("sexe", "segment")] == "lignes_dates_completes, agregats, stocks_successifs") &&
    identical(unname(v["date_entree"]), "lignes_dates_completes") &&
    all(v[setdiff(cat_r$role, c(NON_PRIS, AGREGATS, STOCKS, "sexe", "segment", "date_entree"))] ==
          "lignes_dates_completes, stocks_successifs")
})
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
# controler_roles() appelée directement et par prep() (controler_roles()
# avant preparer_donnees(), comme construire_table()) ; ROLES nu est admis.
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
  verifier(sprintf("rôles, forme exacte : %s -> refus roles_mal_formes (controler_roles et prep)", k),
           leve_refus(cr(BASE, ROLES_REFUSES[[k]], "sexe"), "roles_mal_formes") &&
             leve_refus(prep(roles = ROLES_REFUSES[[k]]), "roles_mal_formes"))
x <- refus(cr(BASE, structure(ROLES, foo = 1), "sexe"))
verifier("rôles, forme exacte : message et détails du refus nomment l'attribut en cause",
         identical(x$motif, "roles_mal_formes") && isTRUE(contient(x, "sans autre attribut que ses noms")) &&
           identical(x$details, list(classe = "character", attributs = c("names", "foo"))))
verifier("rôles, forme exacte : vecteur nommé sans autre attribut admis (controler_roles rend la recette retenue, rôles inchangés)",
         identical(cr(BASE, ROLES, "sexe"), tablexp:::deduire_recette(ROLES)) &&
           identical(attributes(ROLES), list(names = names(ROLES))))
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
# Rôles discriminants de la recette stocks_successifs (#180, commit (4b)) sur
# une base en lignes : recettes_concurrentes (auparavant role_non_pris_en_charge).
for (role in STOCKS) {
  x <- refus(prep(roles = c(ROLES, stats::setNames("autre_colonne", role))))
  verifier(sprintf("rôle %s (discriminant des stocks successifs) : refus recettes_concurrentes, message nomme rôle et colonne", role),
           if (is.null(x)) "aucun refus" else if (!identical(x$motif, "recettes_concurrentes"))
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
# Caractères de contrôle et séparateurs de ligne (#94) : citer() et
# liste_guillemets() appliquent neutraliser_citation() (R/outils.R) ; message
# sur une ligne, valeur exacte dans les details.
for (k in list(list("VT", 0x0B, "<U+000B>"), list("FF", 0x0C, "<U+000C>"), list("NEL", 0x85, "<U+0085>"),
               list("U+2028", 0x2028, "<U+2028>"))) {
  s <- intToUtf8(c(0x73, 0x65, k[[2L]], 0x78, 0x65))
  verifier(sprintf("citer() et liste_guillemets() : %s cité %s", k[[1L]], k[[3L]]),
           identical(tablexp:::citer(c(s, NA)), c(paste0("se", k[[3L]], "xe"), NA)) &&
             identical(tablexp:::liste_guillemets(s), paste0("« se", k[[3L]], "xe »")))
  x <- refus(prep(roles = replace(ROLES, "sexe", s)))
  verifier(sprintf("colonne absente « se<%s>xe » : refus colonne_absente, colonne citée %s, message sur une ligne", k[[1L]], k[[3L]]),
           identical(x$motif, "colonne_absente") && isTRUE(contient(x, paste0("« se", k[[3L]], "xe »"))) &&
             !grepl(intToUtf8(k[[2L]]), conditionMessage(x), fixed = TRUE))
}
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
# Colonne et classe citées par citer() (#127, constat 2 et critère 3) : VT et
# U+2028 écrits <U+XXXX>, message sur une ligne, marqué UTF-8 ; colonne brute
# dans les details.
for (k in list(list("VT", 0x0B, "<U+000B>"), list("U+2028", 0x2028, "<U+2028>"))) {
  s_ <- intToUtf8(c(0x73, 0x65, k[[2L]], 0x78, 0x65))
  d <- BASE; names(d)[names(d) == "sexe"] <- s_; d[[s_]] <- structure(1:4, class = s_)
  x <- refus(prep(d = d, roles = replace(ROLES, "sexe", s_)))
  verifier(sprintf("type incompatible, colonne et classe « se<%s>xe » : type_incompatible, cités %s, une ligne, UTF-8", k[[1L]], k[[3L]]),
           if (is.null(x)) "aucun refus" else if (!identical(x$motif, "type_incompatible")) x$motif
           else if (!isTRUE(contient(x, paste0("colonne « se", k[[3L]], "xe »"), paste0("attendu, se", k[[3L]], "xe reçu"))))
             contient(x, paste0("colonne « se", k[[3L]], "xe »"), paste0("attendu, se", k[[3L]], "xe reçu"))
           else if (grepl(intToUtf8(k[[2L]]), conditionMessage(x), fixed = TRUE)) "caractère écrit en clair"
           else if (!identical(Encoding(conditionMessage(x)), "UTF-8")) "message non marqué UTF-8"
           else identical(x$details$colonne, s_))
}
# Chaîne d'encodage bytes en nom ou en classe de colonne (#128, extension de
# périmètre acceptée par le mainteneur le 05/11/2026, reliquat du critère 3 de
# #127) : refus type_incompatible, colonne et classe citées sous le repère,
# message sur une ligne, marqué UTF-8, colonne brute dans les details. Avant
# #128, `[[.data.frame`, dim() et is.factor() levaient l'erreur R « translating
# strings with "bytes" encoding is not allowed » (contre-épreuve : ce test
# échoue sur le code antérieur). Le data.frame est assemblé sans `[[<-` ni
# `$<-`, qui lèveraient la même erreur.
local({
  avec_sexe <- function(nom, valeur) {
    l <- unclass(BASE); l$sexe <- valeur; names(l)[names(l) == "sexe"] <- nom
    structure(l, class = "data.frame", row.names = seq_len(nrow(BASE)))
  }
  # Refus ou erreur R (une erreur R est rendue, non propagée : le test échoue
  # proprement sur le code antérieur).
  refus_ou_erreur <- function(expr) tryCatch({ force(expr); NULL }, error = function(e) e)
  une_ligne_utf8 <- function(x, fragments) {
    r <- contient(x, fragments)
    if (!isTRUE(r)) r
    else if (!identical(x$motif, "type_incompatible")) x$motif
    else if (grepl("[\n\v\f]", conditionMessage(x))) "message sur plusieurs lignes"
    else if (!identical(Encoding(conditionMessage(x)), "UTF-8")) "message non marqué UTF-8" else TRUE
  }
  x <- refus_ou_erreur(prep(d = avec_sexe("sexe", structure(1:4, class = OCTETS))))
  verifier("type incompatible, classe de colonne d'encodage bytes : type_incompatible, classe citée sous le repère, une ligne, UTF-8", {
    ok <- if (is.null(x)) "aucun refus" else if (!inherits(x, "refus_tablexp")) conditionMessage(x)
          else une_ligne_utf8(x, c("rôle « sexe », colonne « sexe »", "<chaîne d'encodage bytes> reçu"))
    if (isTRUE(ok)) identical(x$details$colonne, "sexe") else ok
  })
  x <- refus_ou_erreur(prep(d = avec_sexe(OCTETS, structure(1:4, class = "k")), roles = replace(ROLES, "sexe", OCTETS)))
  verifier("type incompatible, nom de colonne d'encodage bytes : type_incompatible, colonne citée sous le repère, une ligne, UTF-8", {
    ok <- if (is.null(x)) "aucun refus" else if (!inherits(x, "refus_tablexp")) conditionMessage(x)
          else une_ligne_utf8(x, c("rôle « sexe », colonne « <chaîne d'encodage bytes> »", "k reçu"))
    if (isTRUE(ok)) identical(x$details$colonne, OCTETS) else ok
  })
  # Colonne conforme sous un nom bytes : le type est jugé, non le nom (le refus
  # chaine_codage_bytes de la fiche vient plus tard, module rapport).
  verifier("nom de colonne d'encodage bytes, type conforme : controler_roles() ne lève rien et rend la recette",
           identical(tryCatch(tablexp:::controler_roles(avec_sexe(OCTETS, BASE$sexe), replace(ROLES, "sexe", OCTETS), "sexe"),
                              error = function(e) conditionMessage(e)),
                     tablexp:::deduire_recette(ROLES)))
})
# Classe d'encodage bytes en position quelconque de la classe d'une colonne de
# rôle ou des données (#150) : a_classe(x, "factor") est vrai pour
# c(<bytes>, "factor") et la suite lèverait l'erreur R « translating strings
# with "bytes" encoding is not allowed » ; is.data.frame() la lève pour des
# données de classe c(<bytes>, "data.frame"). Colonne : refus type_incompatible
# du module donnees (avant #150 : controler_roles() ne refusait rien, et
# construire_table() refusait type_non_canonique au module rapport). Données :
# refus donnees_mal_formees (avant #150 : erreur R). Témoins non bytes
# (c("autre", "factor"), c("autre", "data.frame")) acceptés. Contre-épreuve : ces
# tests échouent sur le code antérieur. Une erreur R est rendue, non propagée ;
# les data.frame sont assemblés sans `$<-`, qui lèverait la même erreur.
local({
  b <- "\xe9"; Encoding(b) <- "bytes"
  PLAN <- list(
    transition = VD, periode = PER, delai_declaration = 0, segmentation = "sexe",
    etapes = list(donnees = list(exclusions = character(0)),
                  exposition = list(convention = "plus_fine", par_annee = FALSE),
                  brut = list(estimateurs = "poisson", niveau = 0.95)))
  attrape <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e)
  avec_sexe <- function(classe) {
    l <- unclass(BASE)
    l$sexe <- structure(as.integer(factor(l$sexe)), levels = c("F", "H"), class = classe)
    structure(l, class = "data.frame", row.names = seq_len(nrow(BASE)))
  }
  avec_classe <- function(classe) structure(unclass(BASE), class = classe, row.names = seq_len(nrow(BASE)))
  chemins <- list(controler_roles = function(d) tablexp:::controler_roles(d, ROLES, "sexe"),
                  construire_table = function(d) construire_table(d, ROLES, PLAN))
  verdict <- function(x, motif, fragments) {
    if (is.null(x)) return("aucun refus levé")
    if (!inherits(x, "refus_tablexp")) return(sprintf("condition %s : %s", class(x)[1L], conditionMessage(x)))
    r <- contient(x, fragments)
    if (!isTRUE(r)) r
    else if (!identical(x$module, "donnees")) sprintf("module « %s »", x$module)
    else if (!identical(x$motif, motif)) sprintf("motif « %s »", x$motif)
    else if (grepl("[\n\v\f]", conditionMessage(x))) "message sur plusieurs lignes"
    else if (!identical(Encoding(conditionMessage(x)), "UTF-8")) "message non marqué UTF-8" else TRUE
  }
  for (cl in list(list(c(b, "factor"), "<chaîne d'encodage bytes>/factor"),
                  list(c("factor", b), "factor/<chaîne d'encodage bytes>")))
    for (k in names(chemins))
      verifier(sprintf("colonne sexe de classe %s : %s -> refus type_incompatible du module donnees, classe citée sous le repère, une ligne, UTF-8",
                       cl[[2L]], k),
               verdict(attrape(chemins[[k]](avec_sexe(cl[[1L]]))), "type_incompatible",
                       c("rôle « sexe », colonne « sexe »", paste0("attendu, ", cl[[2L]], " reçu"))))
  for (cl in list(list(c(b, "data.frame"), "<chaîne d'encodage bytes>/data.frame"),
                  list(c("data.frame", b), "data.frame/<chaîne d'encodage bytes>")))
    for (k in names(chemins)) {
      x <- attrape(chemins[[k]](avec_classe(cl[[1L]])))
      verifier(sprintf("données de classe %s : %s -> refus donnees_mal_formees du module donnees, classe citée sous le repère, une ligne, UTF-8",
                       cl[[2L]], k),
               verdict(x, "donnees_mal_formees",
                       paste0("Les données portent une classe d'encodage « bytes » (classe ", cl[[2L]], ")")))
      verifier(sprintf("données de classe %s : %s -> details$classe = la classe reçue", cl[[2L]], k),
               inherits(x, "refus_tablexp") && identical(x$details, list(classe = cl[[1L]])))
    }
  for (k in names(chemins)) {
    verifier(sprintf("colonne sexe de classe c(\"autre\", \"factor\") (sans encodage bytes) : %s ne refuse rien", k),
             is.null(attrape(chemins[[k]](avec_sexe(c("autre", "factor"))))))
    verifier(sprintf("données de classe c(\"autre\", \"data.frame\") (sans encodage bytes) : %s ne refuse rien", k),
             is.null(attrape(chemins[[k]](avec_classe(c("autre", "data.frame"))))))
  }
  # Même colonne dans table_reference() (type_conforme() y juge age, sexe, annee) :
  # refus table_colonne_mal_typee ; avant #150, anyNA() levait l'erreur R.
  for (cl in list(c(b, "factor"), c("factor", b))) {
    t <- structure(list(age = 0:1, sexe = structure(c(1L, 1L), levels = "H", class = cl), qx = c(0.1, 0.2)),
                   class = "data.frame", row.names = 1:2)
    verifier(sprintf("table_reference() : colonne sexe de classe %s -> refus table_colonne_mal_typee (pas une erreur R)",
                     paste(tablexp:::neutraliser_citation(cl), collapse = "/")),
             identical(verdict(attrape(table_reference(t, "t", "importee", "source de test")),
                               "table_colonne_mal_typee", "colonne(s) mal typée(s)"), TRUE))
  }
})
# Colonne de rôle de type conforme sous un nom d'encodage bytes, à
# preparer_donnees() (#140, suite de #128) : lue par lire_champ(), sans erreur
# R ; observations et lignes conservées égales à celles du même jeu sous le
# nom d'origine. Avant #140, `[[.data.frame` levait « translating strings with
# "bytes" encoding is not allowed » dans les six lecteurs de colonne locaux
# (contre-épreuve : ce test échoue sur le code antérieur). Une erreur R est
# rendue, non propagée. Les trois recettes ; le data.frame est assemblé sans
# `[[<-` ni `$<-`, qui lèveraient la même erreur.
local({
  renomme <- function(d, ancien, nom) {
    l <- unclass(d); names(l)[names(l) == ancien] <- nom
    structure(l, class = "data.frame", row.names = seq_len(nrow(d)))
  }
  prepare <- function(d, roles, p, segmentation, exclusions = character(0))
    preparer_donnees(d, roles, p, segmentation, exclusions, tablexp:::controler_roles(d, roles, segmentation))
  G3 <- D(c("2014-12-31", "2015-12-31", "2016-12-31"))
  p_ldc <- tablexp:::controler_parametres_donnees(VD, PER, 0, "sexe", character(0))
  AG <- data.frame(sexe = c("H", "F", "H", "F"), age = c(60L, 60L, 61L, 61L), ec = c(100, 90, 95, 85),
                   d = c(1, 0, 2, 1), stringsAsFactors = FALSE)
  ST <- data.frame(id = c("A", "A", "A", "B", "B", "B"),
                   t = c("inventaire", "inventaire", "inventaire", "inventaire", "inventaire", "evenement"),
                   di = c(G3, G3[1:2], D(NA)), sx = c("H", "H", "H", "F", "F", "F"),
                   seg = c("a", "a", "a", "b", "b", "b"),
                   n = D(c(rep("1950-03-01", 3), rep("1960-07-15", 3))), an = c(rep(1950L, 3), rep(1960L, 3)),
                   s = D(c(NA, NA, NA, NA, NA, "2016-05-01")), c = c(NA, NA, NA, NA, NA, "deces"),
                   stringsAsFactors = FALSE)
  CAS <- list(
    lignes_dates_completes = list(BASE, ROLES, "sexe", p_ldc),
    agregats = list(AG, c(sexe = "sexe", age = "age", exposition_centrale = "ec", evenements = "d"), "sexe", p_ldc),
    stocks_successifs = list(ST, c(id_tete = "id", sexe = "sx", date_naissance = "n", annee_naissance = "an",
                                   type_ligne = "t", date_inventaire = "di", date_sortie = "s", cause_sortie = "c",
                                   segment = "seg"),
                             "segment", tablexp:::valeurs_parametres_donnees(VD, PER, 0, G3)))
  for (recette in names(CAS)) {
    k <- CAS[[recette]]
    ref <- prepare(k[[1L]], k[[2L]], k[[4L]], k[[3L]])
    for (role in names(k[[2L]])) {
      x <- tryCatch(prepare(renomme(k[[1L]], k[[2L]][[role]], OCTETS), replace(k[[2L]], role, OCTETS),
                            k[[4L]], k[[3L]]), error = function(e) e)
      verifier(sprintf("preparer_donnees, recette %s, colonne du rôle %s sous un nom d'encodage bytes : sans erreur R, observations et lignes conservées égales", recette, role),
               if (inherits(x, "error")) conditionMessage(x)
               else identical(x$resultat$observations, ref$resultat$observations) &&
                 identical(x$resultat$numeros_conserves, ref$resultat$numeros_conserves) &&
                 identical(x$resultat$observation_des_lignes, ref$resultat$observation_des_lignes))
    }
  }
  # Chemin des exclusions (retrait de lignes d'un data.frame aux noms bytes).
  d <- BASE; d$sexe[2L] <- NA
  x <- tryCatch(prepare(renomme(d, "sexe", OCTETS), replace(ROLES, "sexe", OCTETS), p_ldc, "sexe", "sexe_manquant"),
                error = function(e) e)
  verifier("preparer_donnees, ligne exclue (sexe_manquant), colonne sexe sous un nom d'encodage bytes : sans erreur R, une ligne exclue, lignes conservées 1, 3, 4",
           if (inherits(x, "error")) conditionMessage(x)
           else identical(x$resultat$nombre_lignes_exclues, 1L) && identical(x$resultat$numeros_conserves, c(1L, 3L, 4L)))
})
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
           identical(x$details$role, list(lignes_dates_completes = "date_naissance")))
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
  list(nom = "sexe hors modalités", x = transform(TR, sexe = c("H", "H", "H", "M", "M", "M")),
       motif = "table_sexe_hors_modalites", lignes = 4:6),
  list(nom = "âge non entier", x = transform(TR, age = c(60, 61.5, 62, 60, 61, 62)),
       motif = "table_colonne_mal_typee", lignes = NULL),
  list(nom = "colonne qx absente", x = TR[c("age", "sexe")], motif = "table_colonne_manquante", lignes = NULL),
  list(nom = "colonne q_x (orthographe de la spécification)", x = setNames(TR, c("age", "sexe", "q_x")),
       motif = "table_colonne_manquante", lignes = NULL),
  list(nom = "colonne inconnue", x = transform(TR, l_x = 1), motif = "table_colonne_inconnue", lignes = NULL),
  list(nom = "colonnes qx et lx ensemble (#146)", x = transform(TR, lx = 1), motif = "table_forme_incoherente",
       lignes = NULL),
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
# Âges non croissants (refus table_ages_non_croissants retiré, M235 ; ADR 0015,
# point 2.1 ; #73) : l'objet est rendu dans l'ordre canonique, chaque q_x
# suivant son âge (valeur calculée à la main).
verifier("table_reference : âges H reçus 60, 62, 61, rendus 60, 61, 62 avec leurs q_x ; F inchangé (ordre canonique, #73)", {
  t <- tr(transform(TR, age = c(60L, 62L, 61L, 60L, 61L, 62L)))
  identical(t$sexe, c("H", "H", "H", "F", "F", "F")) && identical(t$age, c(60L, 61L, 62L, 60L, 61L, 62L)) &&
    identical(t$qx, c(0.01, 0.012, 0.011, 0.006, 0.0066, 0.0072))
})
verifier("table_reference : âges non contigus au sein d'une même année",
         leve_refus(tr(data.frame(age = c(60L, 61L, 60L, 62L), sexe = "H", qx = 0.01, annee = c(2020L, 2020L, 2021L, 2021L))),
                    "table_ages_non_contigus"))
# Trou cité avec son groupe et ses deux âges, pour une table à annee comme pour
# les deux autres formes (ADR 0019, point 1.5 ; valeurs calculées à la main).
x <- refus(tr(data.frame(age = c(60L, 61L, 60L, 62L), sexe = "H", qx = 0.01, annee = c(2020L, 2020L, 2021L, 2021L))))
verifier("table_reference : trou d'une table à annee, message citant sexe, année et âges, details (ADR 0019, point 1.5)",
         identical(x$details, list(lignes = 4L, sexes = "H", annees = 2021L, ages = 60L, ages_suivants = 62L,
                                   lignes_precedentes = 3L)) &&
           isTRUE(contient(x, "entre les lignes 3 et 4 (H, année 2021, âges 60 et 62).")))
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

# --- table de référence transcrite en l_x (#146) --------------------------------
# Spécification d'actuary-approfondi (commentaire 6848954642 de #146), cas T1 à
# T11. Valeurs attendues des q_x : littéraux hexadécimaux de la spécification,
# confirmés par un oracle à fractions exactes (témoin calculé hors du moteur) ;
# numéros de ligne, âges, motifs et extraits de message : valeurs calculées à
# la main.
tl <- function(x, forme_origine = "lx") tr(x, statut = "importee", parametres = NULL, forme_origine = forme_origine)
lx_df <- function(age, lx, sexe = "ensemble", ...) data.frame(age = age, sexe = sexe, lx = lx, ..., stringsAsFactors = FALSE)
empreinte_table <- tablexp:::empreinte_table
controler_forme_tables <- tablexp:::controler_forme_tables
# T1 : conversion (l_x - l_{x+1}) / l_x, dernier âge sans ligne, forme d'origine ;
# la forme interdite 1 - l_{x+1} / l_x donne d'autres bits (non-vacuité).
t <- tl(lx_df(0:2, c(100000, 99616, 99583), sexe = "F"))
verifier("table_reference, l_x (T1) : âges 0 et 1, q_x exacts au bit près, forme d'origine lx, colonnes age, sexe, qx",
         identical(t$age, 0:1) && identical(t$sexe, c("F", "F")) &&
           identical(t$qx, c(0x1.f75104d551d69p-9, 0x1.5b5d2c5338fd5p-12)) &&
           identical(attr(t, "forme_origine"), "lx") && identical(names(t), c("age", "sexe", "qx")) &&
           identical(class(t), c("table_reference", "data.frame")))
verifier("table_reference, l_x (T1) : la forme interdite 1 - l_{x+1} / l_x donne d'autres bits (non-vacuité)",
         identical(1 - c(99616, 99583) / c(100000, 99616), c(0x1.f75104d551dp-9, 0x1.5b5d2c5339p-12)) &&
           identical(1 - 99000 / 100000, 0x1.47ae147ae148p-7) &&
           identical(tl(lx_df(0:1, c(100000, 99000)))$qx, 0x1.47ae147ae147bp-7))
# T2 : égalité l_{x+1} = l_x admise, q_x = +0.
t <- tl(lx_df(20:24, c(1000, 997, 994, 994, 990)))
verifier("table_reference, l_x (T2) : égalité l_{x+1} = l_x admise, q_x = +0, q_x exacts",
         identical(t$age, 20:23) &&
           identical(t$qx, c(0x1.89374bc6a7efap-9, 0x1.8a6631d03d9ffp-9, 0, 0x1.07b9f29b8eae2p-8)) &&
           identical(1 / t$qx[3L], Inf))
# T3 : l_x non décroissants, paires d'âges consécutifs citées.
LX3 <- c(1000, 990, 995, 980, 900, 960, 950, 955, 940)
x <- refus(tl(lx_df(0:8, LX3)))
verifier("table_reference, l_x (T3) : refus table_lx_non_decroissants, paires (1, 2), (4, 5), (6, 7), details",
         identical(x$module, "donnees") && identical(x$motif, "table_lx_non_decroissants") &&
           identical(x$details$ages, c(1L, 4L, 6L)) && identical(x$details$lx, c(990, 900, 950)) &&
           identical(x$details$lx_suivants, c(995, 960, 955)) && identical(x$details$sexes, rep("ensemble", 3L)) &&
           is.null(x$details$annees) && "annees" %in% names(x$details) &&
           identical(x$details$lignes, c(2L, 5L, 7L)) && identical(x$details$lignes_suivantes, c(3L, 6L, 8L)) &&
           isTRUE(contient(x, "3 paire(s)", "ensemble, âges 1 et 2 (l_x = 990 puis 995)", "âges 4 et 5", "âges 6 et 7",
                           "refusée sans correction", "statut « importee »", "ne se déclare pas homologuée")) &&
           !grepl("paires en tout", conditionMessage(x), fixed = TRUE))
perm <- c(9L, 3L, 1L, 7L, 5L, 2L, 8L, 4L, 6L)
x <- refus(tl(lx_df((0:8)[perm], LX3[perm])))
verifier("table_reference, l_x (T3) : lignes permutées, mêmes paires d'âges, lignes dans la numérotation reçue",
         identical(x$motif, "table_lx_non_decroissants") && identical(x$details$ages, c(1L, 4L, 6L)) &&
           identical(x$details$lignes, match(c(2L, 5L, 7L), perm)) &&
           identical(x$details$lignes_suivantes, match(c(3L, 6L, 8L), perm)))
x <- refus(tl(lx_df(rep(0:3, 2), c(100, 90, 80, 70, 100, 101, 90, 95), sexe = rep(c("F", "H"), each = 4))))
verifier("table_reference, l_x (T3) : deux sexes, paires de H seules citées, H avant F dans l'ordre canonique",
         identical(x$motif, "table_lx_non_decroissants") && identical(x$details$sexes, c("H", "H")) &&
           identical(x$details$ages, c(0L, 2L)) && identical(x$details$lignes, c(5L, 7L)) &&
           isTRUE(contient(x, "2 paire(s)", "H, âges 0 et 1 (l_x = 100 puis 101) ; H, âges 2 et 3 (l_x = 90 puis 95)")))
x <- refus(tl(lx_df(0:22, rep(c(10, 20), length.out = 23))))
verifier("table_reference, l_x (T3) : onze paires, dix citées puis le total, liste complète dans details",
         identical(x$motif, "table_lx_non_decroissants") && identical(x$details$ages, seq(0L, 20L, by = 2L)) &&
           isTRUE(contient(x, "11 paire(s)", "âges 18 et 19", "… (11 paires en tout ; liste complète dans les détails du refus)")) &&
           !grepl("âges 20 et 21", conditionMessage(x), fixed = TRUE))
x <- refus(tl(lx_df(c(0:2, 0:2), c(10, 11, 5, 10, 9, 8), sexe = "H", annee = rep(c(1950L, 1951L), each = 3))))
verifier("table_reference, l_x (T3) : avec annee, paire citée « H, année 1950, âges 0 et 1 », details$annees",
         identical(x$motif, "table_lx_non_decroissants") && identical(x$details$annees, 1950L) &&
           isTRUE(contient(x, "H, année 1950, âges 0 et 1 (l_x = 10 puis 11)")))
# T4 : un seul âge par groupe.
x <- refus(tl(data.frame(age = 60L, sexe = "H", lx = 1000)))
verifier("table_reference, l_x (T4) : un seul âge, refus table_lx_age_unique",
         identical(x$motif, "table_lx_age_unique") && identical(x$details$ages, 60L) &&
           isTRUE(contient(x, "un seul âge pour H (âge 60)", "au moins deux âges consécutifs")))
x <- refus(tl(lx_df(c(60:62, 70L), c(100, 90, 80, 50), sexe = c("H", "H", "H", "F"))))
verifier("table_reference, l_x (T4) : H sur trois âges, F sur un seul, refus table_lx_age_unique sur F",
         identical(x$motif, "table_lx_age_unique") && identical(x$details$sexes, "F") &&
           identical(x$details$ages, 70L) && identical(x$details$lignes, 4L))
# T5 : l_x nul au dernier âge admis (q_x = 1), nul suivi d'un âge refusé.
t <- tl(lx_df(108:110, c(10, 4, 0)))
verifier("table_reference, l_x (T5) : l_x nul au dernier âge, q_x = 1 exactement",
         identical(t$age, 108:109) && identical(t$qx, c(0x1.3333333333333p-1, 1)))
x <- refus(tl(lx_df(108:111, c(10, 4, 0, 0))))
verifier("table_reference, l_x (T5) : l_x nul suivi d'un autre âge, refus table_lx_nul à l'âge 110",
         identical(x$motif, "table_lx_nul") && identical(x$details$ages, 110L) && identical(x$details$lignes, 3L) &&
           isTRUE(contient(x, "ensemble, âge 110", "q_x indéfini")))
verifier("table_reference, l_x (T5) : l_x = 10, 0, 4 : refus table_lx_non_decroissants (avant table_lx_nul)",
         leve_refus(tl(lx_df(108:110, c(10, 0, 4))), "table_lx_non_decroissants"))
verifier("table_reference, l_x : -0 se comporte comme 0 (dernier âge admis, q_x = 1 ; suivi d'un âge, refus table_lx_nul)",
         identical(tl(lx_df(0:1, c(10, -0)))$qx, 1) && leve_refus(tl(lx_df(0:2, c(10, -0, 0))), "table_lx_nul"))
# T6 : dernier âge l_omega > 0, q_omega non posé.
verifier("table_reference, l_x (T6) : l_x = 10, 5, 2 aux âges 104 à 106 : deux lignes, q_106 absent",
         identical(tl(lx_df(104:106, c(10, 5, 2)))$age, 104:105))
# T7 : échelle de l_x sans effet, premier âge quelconque.
LX7 <- c(98000, 97000, 95500, 93000)
t1 <- tl(lx_df(40:43, LX7)); t2 <- tl(lx_df(40:43, 2 * LX7))
verifier("table_reference, l_x (T7) : l et 2 l donnent les mêmes q_x et la même empreinte de table ; premier âge 40 admis",
         identical(t1$qx, t2$qx) && identical(empreinte_table(t1), empreinte_table(t2)) && identical(t1$age, 40:42))
# T8 : l_x non entiers admis.
verifier("table_reference, l_x (T8) : l_x non entiers, q_x exacts au bit près",
         identical(tl(lx_df(0:3, c(1000.5, 999.25, 400.125, 7.75)))$qx,
                   c(0x1.4784286d8363fp-10, 0x1.32fb73dce6d44p-1, 0x1.f61545fa66d91p-1)))
# T9 : refus partagés et propres aux l_x.
verifier("table_reference, l_x (T9) : âges 0, 1, 3 : refus table_ages_non_contigus",
         leve_refus(tl(lx_df(c(0L, 1L, 3L), c(10, 9, 8))), "table_ages_non_contigus"))
for (v in list(c("10", "9"), factor(c("10", "9")))) {
  x <- refus(tl(lx_df(0:1, v)))
  verifier(sprintf("table_reference, l_x (T9) : lx de classe %s, refus table_colonne_mal_typee", class(v)),
           identical(x$motif, "table_colonne_mal_typee") && identical(x$details$colonne, "lx") &&
             isTRUE(contient(x, "lx : nombre")))
}
for (v in list(NA_real_, NaN, Inf, -Inf, NA_integer_)) {
  x <- refus(tl(lx_df(0:2, c(10, v, 5))))
  verifier(sprintf("table_reference, l_x (T9) : l_x %s, refus table_lx_non_fini, ligne 2 citée avec son âge", format(v)),
           identical(x$motif, "table_lx_non_fini") && identical(x$details$lignes, 2L) && identical(x$details$ages, 1L) &&
             isTRUE(contient(x, "aux lignes 2 (ensemble, âge 1)")))
}
x <- refus(tl(lx_df(0:2, c(10, 5, -1), sexe = "F", annee = 2000L)))
verifier("table_reference, l_x (T9) : l_x = -1, refus table_lx_negatif, ligne citée avec son groupe et son âge",
         identical(x$motif, "table_lx_negatif") && identical(x$details$lignes, 3L) && identical(x$details$annees, 2000L) &&
           isTRUE(contient(x, "aux lignes 3 (F, année 2000, âge 2)")))
verifier("table_reference, l_x (T9) : lx entier, mêmes q_x qu'en double",
         identical(tl(lx_df(0:2, c(100000L, 99616L, 99583L)))$qx, tl(lx_df(0:2, c(100000, 99616, 99583)))$qx))
# T10 : cohérence des colonnes et de la forme d'origine.
x <- refus(tl(lx_df(0:2, c(10, 9, 8)), forme_origine = "qx"))
verifier("table_reference, l_x (T10) : lx sous la forme d'origine par défaut, refus table_forme_incoherente",
         identical(x$motif, "table_forme_incoherente") && identical(x$details$colonnes, "lx") &&
           identical(x$details$forme_origine, "qx") && isTRUE(contient(x, "déclarer forme_origine = « lx »")))
verifier("table_reference, l_x (T10) : forme d'origine par défaut de table_reference(), lx refusée",
         leve_refus(table_reference(lx_df(0:2, c(10, 9, 8)), "essai", "importee", "test unitaire"),
                    "table_forme_incoherente"))
x <- refus(tl(transform(lx_df(0:2, c(10, 9, 8)), qx = 0.1)))
verifier("table_reference, l_x (T10) : qx et lx ensemble, refus table_forme_incoherente",
         identical(x$motif, "table_forme_incoherente") && identical(x$details$colonnes, c("qx", "lx")) &&
           isTRUE(contient(x, "reçues ensemble")))
t <- tl(data.frame(age = 0:1, sexe = "H", qx = c(0.1, 0.2)))
verifier("table_reference, l_x (T10) : qx déclarée forme d'origine lx admise, q_x inchangés",
         identical(attr(t, "forme_origine"), "lx") && identical(t$qx, c(0.1, 0.2)))
verifier("table_reference, l_x (T10) : un objet issu de l_x passe controler_forme_tables()",
         is.null(refus(controler_forme_tables(list(a = tl(lx_df(0:2, c(100000, 99616, 99583))))))))
x <- refus(tl(data.frame(age = 0:2, sexe = "H")))
verifier("table_reference : ni qx ni lx, refus table_colonne_manquante, details$colonne « qx », message des deux formes",
         identical(x$motif, "table_colonne_manquante") && identical(x$details$colonne, "qx") &&
           isTRUE(contient(x, "« qx » (une table de référence entre par « qx » ou par « lx »)")))
x <- refus(tr(transform(TR, l_x = 1)))
verifier("table_reference : colonne inconnue, message des colonnes admises (qx ou lx ; annee ou generation, ADR 0019)",
         isTRUE(contient(x, "(admises : age, sexe, qx ou lx, annee ou generation)")))
# T11 : q_x hors bornes, lignes citées avec leur groupe et leur âge.
x <- refus(tr(data.frame(age = 0:2, sexe = "ensemble", qx = c(0.01, -0.001, 0.02))))
verifier("table_reference (T11) : q_x hors de [0, 1], ligne citée avec son groupe et son âge, details$ages",
         identical(x$motif, "table_qx_hors_bornes") && identical(x$details$lignes, 2L) &&
           identical(x$details$ages, 1L) && identical(x$details$sexes, "ensemble") && is.null(x$details$annees) &&
           isTRUE(contient(x, "lignes 2 (ensemble, âge 1)")))

# --- table de génération (ADR 0019, points 1 et 2 ; #272) ----------------------
# Spécification d'actuary-approfondi (commentaire 6860921818 de #272, § 5),
# cas T1 à T7. Cas construits à la main, aucun tirage ; q_x attendus :
# rapports de petits entiers, calculés à la main ; numéros de ligne, âges,
# générations, motifs, details et extraits de message : valeurs calculées à
# la main. details de T7 : valeurs à bb2c318, calculées à la main et
# confirmées identical() à la bibliothèque installée depuis ce commit.
G1 <- data.frame(age = c(60L, 61L, 60L, 61L, 60L, 61L), sexe = c("F", "F", "F", "F", "H", "H"),
                 generation = c(1950L, 1950L, 1951L, 1951L, 1950L, 1950L), qx = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06),
                 stringsAsFactors = FALSE)
tg <- function(x) tr(x, statut = "importee", parametres = NULL)
t <- tg(G1)
verifier("table_reference, génération (T1) : table en q_x admise, colonnes age, sexe, qx, generation, types, ordre canonique (sexe, génération, âge)",
         identical(names(t), c("age", "sexe", "qx", "generation")) && is.integer(t$age) && is.character(t$sexe) &&
           is.double(t$qx) && is.integer(t$generation) && identical(t$sexe, c("H", "H", "F", "F", "F", "F")) &&
           identical(t$generation, c(1950L, 1950L, 1950L, 1950L, 1951L, 1951L)) &&
           identical(t$age, c(60L, 61L, 60L, 61L, 60L, 61L)) && identical(t$qx, c(0.05, 0.06, 0.01, 0.02, 0.03, 0.04)) &&
           identical(class(t), c("table_reference", "data.frame")) && identical(attr(t, "row.names"), 1:6))
verifier("table_reference, génération (T1) : objet identical() pour l'ordre reçu inverse et pour un ordre entrelacé",
         identical(tg(G1[6:1, ]), t) && identical(tg(G1[c(2L, 5L, 1L, 6L, 3L, 4L), ]), t))
# T2 : deux générations, premiers âges 2 et 0, derniers âges 4 et 1 ; reçues
# 1951 d'abord.
t <- tl(lx_df(c(0L, 1L, 2L, 3L, 4L), c(4, 3, 10, 8, 5), sexe = "F", generation = c(1951L, 1951L, 1950L, 1950L, 1950L)))
verifier("table_reference, génération (T2) : l_x par génération, q_2 = 2/10, q_3 = 3/8 (1950), q_0 = 1/4 (1951), aucun q au dernier âge, rien avant le premier",
         identical(t$age, c(2L, 3L, 0L)) && identical(t$generation, c(1950L, 1950L, 1951L)) &&
           identical(t$qx, c(0.2, 0.375, 0.25)) && identical(t$sexe, rep("F", 3L)) &&
           identical(names(t), c("age", "sexe", "qx", "generation")) && identical(attr(t, "forme_origine"), "lx"))
# T3 : contre-exemple d'enchaînement (1950 finit à 5 ans, 1951 commence à 6) ;
# enchaînées, les deux lignes donneraient l_6 = 9 > l_5 = 6 (hausse).
X3 <- lx_df(c(4L, 5L, 6L, 7L), c(10, 6, 9, 3), sexe = "H", generation = c(1950L, 1950L, 1951L, 1951L))
t3 <- tl(X3)
X3a <- X3; names(X3a)[names(X3a) == "generation"] <- "annee"
t3a <- tl(X3a)
verifier("table_reference, génération (T3) : aucun q_x entre deux générations (q_4 = 4/10, q_6 = 6/9) ; sous annee, mêmes q_x, autre empreinte",
         identical(t3$age, c(4L, 6L)) && identical(t3$qx, c(0.4, 2 / 3)) && identical(t3$generation, c(1950L, 1951L)) &&
           identical(t3a$qx, t3$qx) && identical(t3a$age, t3$age) && identical(t3a$annee, t3$generation) &&
           !identical(empreinte_table(t3a), empreinte_table(t3)))
# T4 : refus de forme.
x <- refus(tg(transform(G1, annee = 2000L)))
verifier("table_reference, génération (T4) : annee et generation reçues ensemble, refus table_annee_et_generation, details$colonnes",
         identical(x$module, "donnees") && identical(x$motif, "table_annee_et_generation") &&
           identical(x$details, list(colonnes = c("annee", "generation"))) &&
           isTRUE(contient(x, "colonnes « annee » et « generation » reçues ensemble", "au plus un axe temporel",
                           "« annee » (année calendaire, table prospective)",
                           "« generation » (année de naissance, table de génération).")))
verifier("table_reference, génération (T4) : annee et generation avec q_x manquant, table_annee_et_generation avant tout contrôle de valeurs ; colonne inconnue d'abord",
         leve_refus(tg(transform(G1, annee = 2000L, qx = NA_real_)), "table_annee_et_generation") &&
           leve_refus(tg(transform(G1, annee = 2000L, z = 1)), "table_colonne_inconnue"))
x <- refus(tg(setNames(G1, c("age", "sexe", "génération", "qx"))))
verifier("table_reference, génération (T4) : colonne « génération » accentuée, refus table_colonne_inconnue",
         identical(x$motif, "table_colonne_inconnue") && identical(x$details$colonne, "génération") &&
           isTRUE(contient(x, "(admises : age, sexe, qx ou lx, annee ou generation)")))
for (v in list(1950.5, NA_integer_, factor(1950L), "1950")) {
  x <- refus(tg(transform(G1, generation = v)))
  verifier(sprintf("table_reference, génération (T4) : generation %s (%s), refus table_colonne_mal_typee",
                   format(v), class(v)),
           identical(x$motif, "table_colonne_mal_typee") && identical(x$details$colonne, "generation") &&
             isTRUE(contient(x, "generation : entier")))
}
# T5 : refus de contenu nommant la génération.
x <- refus(tg(data.frame(age = c(60L, 62L, 60L, 61L), sexe = "F", generation = c(1951L, 1950L, 1950L, 1951L), qx = 0.01)))
verifier("table_reference, génération (T5) : trou, table_ages_non_contigus citant sexe, génération et âges ; details",
         identical(x$motif, "table_ages_non_contigus") &&
           identical(x$details, list(lignes = 2L, sexes = "F", generations = 1950L, ages = 60L, ages_suivants = 62L,
                                     lignes_precedentes = 3L)) &&
           isTRUE(contient(x, "entre les lignes 3 et 2 (F, génération 1950, âges 60 et 62).")))
# Onze trous reçus en ordre inverse (âges 22, 20, ..., 0) : dix paires citées
# puis le total ; details complets, lignes_precedentes (ligne qui précède le
# trou, âge ages) et lignes (ligne qui le suit, âge ages_suivants) en
# numérotation reçue (visa d'actuary, C1 et C2 ; valeurs calculées à la main).
x <- refus(tg(data.frame(age = seq(22L, 0L, by = -2L), sexe = "F", generation = 1950L, qx = 0.01)))
verifier("table_reference, génération (T5) : onze trous, dix paires citées puis le total, details lignes_precedentes et lignes",
         identical(x$motif, "table_ages_non_contigus") &&
           identical(x$details, list(lignes = 11:1, sexes = rep("F", 11L), generations = rep(1950L, 11L),
                                     ages = seq(0L, 20L, by = 2L), ages_suivants = seq(2L, 22L, by = 2L),
                                     lignes_precedentes = 12:2)) &&
           isTRUE(contient(x, "entre les lignes 12 et 11 (F, génération 1950, âges 0 et 2), 11 et 10",
                           "3 et 2 (F, génération 1950, âges 18 et 20) … (11 paires en tout ; liste complète dans les détails du refus).")) &&
           !grepl("âges 20 et 22", conditionMessage(x), fixed = TRUE))
x <- refus(tg(data.frame(age = c(60L, 61L, 60L), sexe = "F", generation = 1950L, qx = 0.01)))
verifier("table_reference, génération (T5) : doublon, table_age_double « au sein d'un même sexe et d'une même génération »",
         identical(x$motif, "table_age_double") && identical(x$details, list(lignes = 3L)) &&
           isTRUE(contient(x, "au sein d'un même sexe et d'une même génération, lignes 3.")))
x <- refus(tl(lx_df(rep(0:2, 2), c(10, 11, 5, 10, 9, 12), sexe = "H", generation = rep(c(1950L, 1951L), each = 3L))))
verifier("table_reference, génération (T5) : hausses dans deux générations, paires citées avec leur génération, aucune paire entre générations",
         identical(x$motif, "table_lx_non_decroissants") &&
           identical(x$details, list(sexes = c("H", "H"), generations = c(1950L, 1951L), ages = c(0L, 1L),
                                     lx = c(10, 9), lx_suivants = c(11, 12), lignes = c(1L, 5L), lignes_suivantes = c(2L, 6L))) &&
           isTRUE(contient(x, "2 paire(s)", "H, génération 1950, âges 0 et 1 (l_x = 10 puis 11) ; H, génération 1951, âges 1 et 2 (l_x = 9 puis 12)")))
x <- refus(tl(lx_df(0:22, rep(c(10, 20), length.out = 23), generation = 1960L)))
verifier("table_reference, génération (T5) : onze hausses, dix citées puis le total, liste complète dans details",
         identical(x$motif, "table_lx_non_decroissants") && identical(x$details$ages, seq(0L, 20L, by = 2L)) &&
           identical(x$details$generations, rep(1960L, 11L)) && !("annees" %in% names(x$details)) &&
           isTRUE(contient(x, "11 paire(s)", "ensemble, génération 1960, âges 18 et 19",
                           "… (11 paires en tout ; liste complète dans les détails du refus)")) &&
           !grepl("âges 20 et 21", conditionMessage(x), fixed = TRUE))
x <- refus(tl(lx_df(c(0L, 1L, 5L), c(10, 9, 8), sexe = "H", generation = c(1950L, 1950L, 1951L))))
verifier("table_reference, génération (T5) : une génération à un seul âge, table_lx_age_unique",
         identical(x$motif, "table_lx_age_unique") &&
           identical(x$details, list(sexes = "H", generations = 1951L, ages = 5L, lignes = 3L)) &&
           isTRUE(contient(x, "un seul âge pour H, génération 1951 (âge 5)", "par sexe (et par année ou par génération).")))
x <- refus(tl(lx_df(0:2, c(10, 0, 0), sexe = "H", generation = 1950L)))
verifier("table_reference, génération (T5) : l_x nul suivi d'un autre âge, table_lx_nul",
         identical(x$motif, "table_lx_nul") &&
           identical(x$details, list(sexes = "H", generations = 1950L, ages = 1L, lignes = 2L)) &&
           isTRUE(contient(x, "H, génération 1950, âge 1", "(et d'une année ou d'une génération)")))
x <- refus(tl(lx_df(0:2, c(10, NA, 5), sexe = "F", generation = 1950L)))
verifier("table_reference, génération (T5) : l_x manquant, table_lx_non_fini",
         identical(x$motif, "table_lx_non_fini") &&
           identical(x$details, list(lignes = 2L, ages = 1L, sexes = "F", generations = 1950L)) &&
           isTRUE(contient(x, "aux lignes 2 (F, génération 1950, âge 1)")))
x <- refus(tl(lx_df(0:2, c(10, 5, -1), sexe = "F", generation = 1950L)))
verifier("table_reference, génération (T5) : l_x négatif, table_lx_negatif",
         identical(x$motif, "table_lx_negatif") &&
           identical(x$details, list(lignes = 3L, ages = 2L, sexes = "F", generations = 1950L)) &&
           isTRUE(contient(x, "aux lignes 3 (F, génération 1950, âge 2)")))
x <- refus(tg(data.frame(age = 0:2, sexe = "ensemble", generation = 2000L, qx = c(0.01, 1.5, 0.02))))
verifier("table_reference, génération (T5) : q_x hors de [0, 1], table_qx_hors_bornes",
         identical(x$motif, "table_qx_hors_bornes") &&
           identical(x$details, list(lignes = 2L, ages = 1L, sexes = "ensemble", generations = 2000L)) &&
           isTRUE(contient(x, "aux lignes 2 (ensemble, génération 2000, âge 1)")))
x <- refus(tg(data.frame(age = 0L, sexe = c("F", "ensemble", "H", "H"), generation = c(1950L, 1950L, 1951L, 1952L), qx = 0.01)))
verifier("table_reference, génération (T5) : ensemble et F dans la même génération, table_sexes_melanges « (générations 1950) »",
         identical(x$motif, "table_sexes_melanges") && identical(x$details, list(generations = 1950L)) &&
           isTRUE(contient(x, "coexiste avec H ou F (générations 1950).")))
# T6 : générations extrêmes adjacentes (aucune différence d'entiers calculée).
M <- .Machine$integer.max
t <- tg(data.frame(age = c(0L, 1L, 0L, 1L), sexe = "H", generation = c(M, M, -M, -M), qx = c(0.1, 0.2, 0.3, 0.4)))
t6 <- tl(lx_df(c(0L, 1L, 0L, 1L), c(8, 6, 4, 1), sexe = "H", generation = c(M, M, -M, -M)))
verifier("table_reference, génération (T6) : générations ±.Machine$integer.max, aucun débordement, deux groupes distincts",
         identical(t$generation, c(-M, -M, M, M)) && identical(t$qx, c(0.3, 0.4, 0.1, 0.2)) &&
           identical(t6$generation, c(-M, M)) && identical(t6$qx, c(0.75, 0.25)))
# generation hors de la plage des integer de R (double entier) : refusée
# comme mal typée, jamais convertie en NA (ADR 0019, point 1.1 ; constat
# d'audit) ; la borne elle-même, en double, est admise.
for (v in list(3e9, -3e9, -2147483648)) {
  x <- refus(tg(data.frame(age = 0:1, sexe = "H", qx = 0.1, generation = v)))
  verifier(sprintf("table_reference, génération : generation = %.0f (double hors des integer), refus table_colonne_mal_typee", v),
           identical(x$motif, "table_colonne_mal_typee") && identical(x$details, list(colonne = "generation")))
}
verifier("table_reference, génération : generation = 2147483647 (double, borne des integer) admise, rendue integer",
         identical(tg(data.frame(age = 0:1, sexe = "H", qx = 0.1, generation = 2147483647))$generation, rep(M, 2L)))
# T7 : details des refus touchés, tables sans axe et à annee : inchangés.
T7 <- list(
  qx_hors_0 = list(refus(tr(data.frame(age = 0:2, sexe = "ensemble", qx = c(0.01, -0.001, 0.02)))),
                   list(lignes = 2L, ages = 1L, sexes = "ensemble", annees = NULL)),
  lx_non_fini_0 = list(refus(tl(lx_df(0:2, c(10, NA, 5)))), list(lignes = 2L, ages = 1L, sexes = "ensemble", annees = NULL)),
  lx_negatif_an = list(refus(tl(lx_df(0:2, c(10, 5, -1), sexe = "F", annee = 2000L))),
                       list(lignes = 3L, ages = 2L, sexes = "F", annees = 2000L)),
  melange_0 = list(refus(tr(data.frame(age = 0L, sexe = c("H", "ensemble"), qx = 0.01))), list(annees = NULL)),
  melange_an = list(refus(tr(data.frame(age = 0L, sexe = c("H", "ensemble"), qx = 0.01, annee = 2020L))), list(annees = 2020L)),
  double_an = list(refus(tr(data.frame(age = c(0L, 0L), sexe = "H", qx = 0.01, annee = 2020L))), list(lignes = 2L)),
  unique_0 = list(refus(tl(lx_df(c(60:62, 70L), c(100, 90, 80, 50), sexe = c("H", "H", "H", "F")))),
                  list(sexes = "F", annees = NULL, ages = 70L, lignes = 4L)),
  non_decr_0 = list(refus(tl(lx_df(0:2, c(10, 11, 5)))),
                    list(sexes = "ensemble", annees = NULL, ages = 0L, lx = 10, lx_suivants = 11, lignes = 1L,
                         lignes_suivantes = 2L)),
  non_decr_an = list(refus(tl(lx_df(c(0:2, 0:2), c(10, 11, 5, 10, 9, 8), sexe = "H", annee = rep(c(1950L, 1951L), each = 3)))),
                     list(sexes = "H", annees = 1950L, ages = 0L, lx = 10, lx_suivants = 11, lignes = 1L,
                          lignes_suivantes = 2L)),
  nul_an = list(refus(tl(lx_df(0:2, c(10, 0, 0), sexe = "F", annee = 1990L))),
                list(sexes = "F", annees = 1990L, ages = 1L, lignes = 2L)),
  inconnue = list(refus(tr(transform(TR, foo = 1))), list(colonne = "foo")),
  mal_typee = list(refus(tr(transform(TR, annee = 2020.5))), list(colonne = "annee")))
for (n in names(T7))
  verifier(sprintf("table_reference, génération (T7) : details du refus %s (table sans axe ou à annee) inchangés", n),
           !is.null(T7[[n]][[1L]]) && identical(T7[[n]][[1L]]$details, T7[[n]][[2L]]))

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
# Défauts de la table, jugés après le nom : un nom ordinaire laisse venir le refus
# propre à chaque défaut (contre-épreuve du refus du nom bytes, #158).
TR_SEXE <- TR; TR_SEXE$sexe[1L] <- "Z"
TR_EN_PLUS <- TR; TR_EN_PLUS$zz <- 1
DEFAUTS_TABLE <- list(
  list("statut inconnu", list(statut = "zzz"), "table_statut_inconnu"),
  list("source vide", list(source = ""), "table_source_manquante"),
  list("paramètres manquants", list(parametres = NULL), "table_parametres_manquants"),
  list("paramètres inattendus", list(statut = "importee", parametres = list(A = 1)), "table_parametres_inattendus"),
  list("forme_origine inconnue", list(forme_origine = "zz"), "table_forme_inconnue"),
  list("x non data.frame", list(x = 1), "table_mal_formee"),
  list("colonne inconnue", list(x = TR_EN_PLUS), "table_colonne_inconnue"),
  list("sexe hors modalités", list(x = TR_SEXE), "table_sexe_hors_modalites"))
# Nom au codage bytes (#158, règle de #128 ; avant, #111 le citait sous un repère
# et l'admettait) : refus table_nom_manquant dès l'entrée, avant tout autre
# contrôle de la table (donc quel que soit le défaut), message d'une ligne marqué
# UTF-8, sans erreur R, aucune valeur citée ; refus plutôt que l'écriture de la
# fiche, où octets_chaines() le refuserait (chaine_codage_bytes) après les calculs.
ligne_utf8 <- function(r) !grepl("[\n\v\f]", conditionMessage(r)) && identical(Encoding(conditionMessage(r)), "UTF-8")
for (k in c(list(list("aucun défaut", list(), NA_character_)), DEFAUTS_TABLE)) {
  x <- refus(do.call(tr, c(list(nom = OCTETS), k[[2]])))
  verifier(sprintf("table_reference : nom au codage bytes, %s -> refus table_nom_manquant, message propre, sans valeur citée", k[[1]]),
           identical(x$motif, "table_nom_manquant") && identical(x$module, "donnees") && ligne_utf8(x) &&
             isTRUE(contient(x, "Le nom de la table de référence est une chaîne d'encodage « bytes »", "sans deviner",
                             "sans encodage bytes")) &&
             !grepl("<chaîne d'encodage bytes>", conditionMessage(x), fixed = TRUE))
  if (!is.na(k[[3]])) {
    y <- refus(do.call(tr, c(list(nom = "essai"), k[[2]])))
    verifier(sprintf("table_reference : nom ordinaire, %s -> refus %s (contre-épreuve)", k[[1]], k[[3]]),
             identical(y$motif, k[[3]]) && !grepl("encodage « bytes »", conditionMessage(y), fixed = TRUE))
  }
}
x <- refus(tr(nom = structure("essai", class = OCTETS)))
verifier("table_reference : nom de classe bytes (attribut) -> refus table_nom_manquant existant, sans erreur R",
         identical(x$motif, "table_nom_manquant") && isTRUE(contient(x, "sans attribut")))
verifier("table_reference : nom ordinaire non ASCII (UTF-8) admis, nom de l'objet inchangé (contre-épreuve)",
         identical(attr(tr(nom = "d\u00e9c\u00e8s"), "nom"), "d\u00e9c\u00e8s"))
# Source au codage bytes : refus table_source_manquante, nom de la table cité ;
# le nom est jugé d'abord ; un défaut antérieur (statut) l'emporte.
x <- refus(tr(source = OCTETS))
verifier("table_reference : source au codage bytes -> refus table_source_manquante, nom cité, message propre, sans valeur citée",
         identical(x$motif, "table_source_manquante") && identical(x$module, "donnees") && ligne_utf8(x) &&
           isTRUE(contient(x, "Table « essai » : la source est une chaîne d'encodage « bytes »", "sans deviner",
                           "sans encodage bytes")) &&
           !grepl("<chaîne d'encodage bytes>", conditionMessage(x), fixed = TRUE))
verifier("table_reference : source ordinaire non ASCII (UTF-8) admise, attribut conservé (contre-épreuve)",
         identical(attr(tr(source = "arr\u00eat\u00e9"), "source"), "arr\u00eat\u00e9"))
verifier("table_reference : source bytes et nom bytes -> le nom est refusé d'abord",
         identical(refus(tr(nom = OCTETS, source = OCTETS))$motif, "table_nom_manquant"))
verifier("table_reference : source bytes et statut inconnu -> statut refusé d'abord (ordre nom, statut, source)",
         identical(refus(tr(source = OCTETS, statut = "zzz"))$motif, "table_statut_inconnu"))
x <- refus(tr(source = structure("s", class = OCTETS)))
verifier("table_reference : source de classe bytes (attribut) -> refus table_source_manquante existant, sans erreur R",
         identical(x$motif, "table_source_manquante") && isTRUE(contient(x, "sans attribut")))
# statut et forme_origine bytes : valeurs d'un ensemble fermé, refusées par %in%
# sous leur motif existant, sans erreur R et sans citer la valeur (mesuré).
x <- refus(tr(statut = OCTETS))
verifier("table_reference : statut au codage bytes -> refus table_statut_inconnu, sans erreur R, valeur non citée",
         identical(x$motif, "table_statut_inconnu") && ligne_utf8(x) &&
           !grepl("<chaîne d'encodage bytes>", conditionMessage(x), fixed = TRUE))
x <- refus(tr(forme_origine = OCTETS))
verifier("table_reference : forme_origine au codage bytes -> refus table_forme_inconnue, sans erreur R, valeur non citée",
         identical(x$motif, "table_forme_inconnue") && ligne_utf8(x) &&
           !grepl("<chaîne d'encodage bytes>", conditionMessage(x), fixed = TRUE))
# Table entière de classe d'encodage bytes (#158, réserve de l'audit de #150) :
# classe_bytes() avant is.data.frame(), dont inherits() levait l'erreur R
# « translating strings with "bytes" encoding is not allowed » (contre-épreuve :
# ces tests échouent sur le code antérieur) ; la classe est citée sous le repère.
refus_ou_erreur <- function(expr) tryCatch({ force(expr); NULL }, error = function(e) e)
for (k in list(list("c(<bytes>, data.frame)", c(OCTETS, "data.frame")),
               list("c(data.frame, <bytes>)", c("data.frame", OCTETS)),
               list("c(<bytes>, table_reference, data.frame)", c(OCTETS, "table_reference", "data.frame")))) {
  x <- refus_ou_erreur(tr(x = structure(TR, class = k[[2]])))
  verifier(sprintf("table_reference : x de classe %s -> refus table_mal_formee, classe citée sous le repère, sans erreur R", k[[1]]),
           if (is.null(x)) "aucun refus" else if (!inherits(x, "refus_tablexp")) conditionMessage(x)
           else identical(x$motif, "table_mal_formee") && identical(x$module, "donnees") && ligne_utf8(x) &&
             identical(x$details$classe, k[[2]]) &&
             isTRUE(contient(x, "Table « essai » : la table porte une classe d'encodage « bytes » (classe ",
                             "<chaîne d'encodage bytes>", "sans deviner", "ne porte aucune chaîne d'encodage bytes")))
}
verifier("table_reference : x de classe c(\"autre\", \"data.frame\") (sans encodage bytes) : accepté (contre-épreuve)",
         inherits(tr(x = structure(TR, class = c("autre", "data.frame"))), "table_reference"))
verifier("table_reference : x non data.frame sans classe bytes -> message « un data.frame est attendu » inchangé",
         isTRUE(contient(refus(tr(x = 1)), "un data.frame est attendu")))
# Sous LC_ALL=C (sous-processus, bibliothèque des batteries) : un nom qui n'est pas bytes
# est cité exactement comme le ferait sprintf() sur le nom reçu (oracle : la formule
# d'avant #111, octet pour octet, sans traduction ajoutée par le masquage (un nom latin1
# reste « déc » en UTF-8 valide, jamais « d<e9>c ») ; le nom bytes est remplacé par le repère.
script_nom <- tempfile(fileext = ".R")
writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(.harnais$bibliotheque)),
  "T0 <- data.frame(age = 0:1, sexe = 'ensemble', qx = c(.1, .2))",
  "tr <- function(nom, source = 's', x = T0, statut = 'zzz') tryCatch(table_reference(x, nom, statut, source),",
  "                             refus_tablexp = function(c) conditionMessage(c))",
  "L1 <- 'd\\xe9c'; Encoding(L1) <- 'latin1'; B <- 'd\\xe9c'; Encoding(B) <- 'bytes'",
  "gabarit <- paste0('Table \\u00ab %s \\u00bb : statut (cha\\u00eene sans attribut) \\u00e0 prendre dans ',",
  "  '\\u00ab synthetique \\u00bb, \\u00ab embarquee \\u00bb, \\u00ab importee \\u00bb.')",
  "cas <- list(ascii = list('essai', 'essai'), latin1 = list(L1, L1), utf8 = list('d\\u00e9c\\u00e8s', 'd\\u00e9c\\u00e8s'))",
  "cat('LOCALE', Sys.getlocale('LC_CTYPE'), '\\n')",
  "for (k in names(cas)) {",
  "  m <- tr(cas[[k]][[1]]); e <- sprintf(gabarit, cas[[k]][[2]])",
  "  cat('CAS', k, identical(charToRaw(m), charToRaw(e)), if (!identical(charToRaw(m), charToRaw(e))) paste(charToRaw(m), collapse = ''), '\\n')",
  "}",
  "# Chaines bytes (#158) : nom, source, classe de x ; messages attendus octet pour octet.",
  "gab_bytes <- list(",
  "  nom = paste0('Le nom de la table de r\\u00e9f\\u00e9rence est une cha\\u00eene d\\'encodage \\u00ab bytes \\u00bb, non convertible en UTF-8 ',",
  "               'sans deviner : il n\\'est pas lu. Passer un nom (cha\\u00eene non vide, sans attribut) sans encodage bytes.'),",
  "  source = paste0('Table \\u00ab essai \\u00bb : la source est une cha\\u00eene d\\'encodage \\u00ab bytes \\u00bb, non convertible en UTF-8 sans deviner : ',",
  "                  'elle n\\'est pas lue. Passer une source (cha\\u00eene non vide, sans attribut) sans encodage bytes.'),",
  "  classe = paste0('Table \\u00ab essai \\u00bb : la table porte une classe d\\'encodage \\u00ab bytes \\u00bb (classe <cha\\u00eene d\\'encodage bytes>/data.frame), ',",
  "                  'non convertible en UTF-8 sans deviner : elle n\\'est pas lue. Passer un data.frame dont la classe ne porte aucune ',",
  "                  'cha\\u00eene d\\'encodage bytes.'))",
  "obt <- list(nom = tr(B), source = tr('essai', source = B, statut = 'importee'),",
  "            classe = tr('essai', x = structure(T0, class = c(B, 'data.frame')), statut = 'importee'))",
  "for (k in names(gab_bytes)) {",
  "  m <- obt[[k]]; e <- gab_bytes[[k]]",
  "  cat('CAS bytes_', k, ' ', identical(charToRaw(m), charToRaw(e)), if (!identical(charToRaw(m), charToRaw(e))) paste(charToRaw(m), collapse = ''), '\\n', sep = '')",
  "}"), script_nom)
r_nom <- executer_rscript(script_nom, env = "LC_ALL=C")
unlink(script_nom)
for (k in c("ascii", "latin1", "utf8"))
  verifier(sprintf("table_reference : nom %s sous LC_ALL=C, message égal octet pour octet à sprintf() sur le nom", k), {
    loc <- trimws(sub("^LOCALE ", "", grep("^LOCALE ", r_nom$sortie, value = TRUE)))
    l <- grep(paste0("^CAS ", k, " "), r_nom$sortie, value = TRUE)
    if (r_nom$statut == 0L && identical(loc, "C") && length(l) == 1L && grepl(" TRUE ", paste0(l, " "), fixed = TRUE)) TRUE
    else paste("statut", r_nom$statut, ":", paste(r_nom$sortie, collapse = " | "))
  })
for (k in c("nom", "source", "classe"))
  verifier(sprintf("table_reference : %s au codage bytes sous LC_ALL=C, refus de message égal octet pour octet au texte attendu, sans erreur R", k), {
    loc <- trimws(sub("^LOCALE ", "", grep("^LOCALE ", r_nom$sortie, value = TRUE)))
    l <- grep(paste0("^CAS bytes_", k, " "), r_nom$sortie, value = TRUE)
    if (r_nom$statut == 0L && identical(loc, "C") && length(l) == 1L && grepl(" TRUE", l, fixed = TRUE)) TRUE
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
           ROLES, "sexe"), tablexp:::deduire_recette(ROLES)))
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

# --- assistants du module (#196, points 4 et 5) --------------------------------

verifier("lignes_cle_dupliquee() : toutes les lignes d'une même clé exacte (deux colonnes, valeurs distinctes mais proches non confondues), lignes incomplètes jamais en cause, aucune ligne -> logique(0) (valeur écrite à la main)", {
  f <- tablexp:::lignes_cle_dupliquee
  id <- c("a", "a", "b", "a", "b", NA, NA)
  v <- c(1, 1, 1, 1 + 1e-12, 1, 1, 1)
  identical(f(list(id, v), !is.na(id)), c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE)) &&
    identical(f(list(id, v), !is.na(id) & seq_along(id) != 2L), c(FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE)) &&
    identical(f(list(factor(c("x", "y", "x")), c(1, 2, 1)), rep(TRUE, 3L)), c(TRUE, FALSE, TRUE)) &&
    identical(f(list(character(0), numeric(0)), logical(0)), logical(0))
})
verifier("sous_indicer_categorie() : chaîne et entier sous-indicés sans attribut ; facteur recopié sous forme normalisée (classe factor ou ordered, niveaux et ordre inchangés, niveau inutilisé conservé, autre attribut retiré) (valeur écrite à la main)", {
  f <- tablexp:::sous_indicer_categorie
  fa <- factor(c("b", "a", "b"), levels = c("a", "b", "c")); attr(fa, "contrasts") <- "x"
  fo <- factor(c("b", "a", "b"), levels = c("b", "a", "c"), ordered = TRUE)
  ca <- structure(c("H", "F", "H"), label = "sexe")
  identical(f(ca, c(3L, 1L)), c("H", "H")) && identical(f(c(2L, 1L, 3L), 2:3), c(1L, 3L)) &&
    identical(f(fa, c(2L, 1L)), factor(c("a", "b"), levels = c("a", "b", "c"))) &&
    identical(f(fo, 1:2), factor(c("b", "a"), levels = c("b", "a", "c"), ordered = TRUE)) &&
    identical(class(f(fo, 1:2)), c("ordered", "factor"))
})
