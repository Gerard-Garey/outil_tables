# -----------------------------------------------------------------------------
#  tests/unitaires/test_catalogue_recettes.R : catalogue des recettes, matrice
#  recette x étape x estimateur, sélection de la recette et ordre des refus au
#  niveau des rôles (issue #169, commits (3a) et (3b) ; ADR 0016, points 1, 2,
#  3, 5, 5.5, 6.4 et 6.5 ; messages visés par actuary, avis du 08/11/2026 sur
#  #169). Refus propres aux agrégats : test_agregats.R.
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - invariant : propriété du catalogue ou de la matrice (ADR 0016, points 1.4
#    et 5.4), contrôlée sur les constantes du package installé ;
#  - valeur écrite à la main : ligne de la recette des lignes, colonnes de
#    catalogue_roles(), textes exacts des messages (gabarits visés par
#    actuary, valeurs insérées écrites à la main) ;
#  - injection : un catalogue ou une matrice de test remplace la constante
#    dans l'espace de noms le temps d'un appel (précédent : la recette fictive
#    de test_construire_table.R), puis est restauré ; depuis (3b), les refus 5
#    et 8 de l'ordre de l'ADR 0016, point 2.2, sont aussi atteints sur le
#    catalogue du package (rôles des lignes et des agrégats).
# -----------------------------------------------------------------------------

NS <- asNamespace("tablexp")
D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
CAT <- tablexp:::CATALOGUE_RECETTES
MAT <- tablexp:::MATRICE_RECETTES
R0 <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
BASE <- data.frame(n = D(c("1950-03-01", "1960-07-15")), e = D(c("2016-01-01", "2015-06-01")),
                   s = D(c("2020-01-01", "2024-12-31")), c = c("deces", "en_cours"), sx = c("H", "F"),
                   a = c(60L, 61L), ev = c(0L, 1L), m = c(1, 2), di = D(c("2016-01-01", "2017-01-01")),
                   stringsAsFactors = FALSE)
plan_type <- function(segmentation = character(0), etapes = NULL)
  list(transition = VD, periode = PER, delai_declaration = 0, segmentation = segmentation,
       etapes = if (is.null(etapes)) list(donnees = list(exclusions = character(0)),
                                          exposition = list(convention = "plus_fine", par_annee = FALSE))
                else etapes)
rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
avec_constante <- function(nom, valeur, expr) {
  ancienne <- get(nom, envir = NS)
  unlockBinding(nom, NS)
  assign(nom, valeur, envir = NS)
  on.exit({ assign(nom, ancienne, envir = NS); lockBinding(nom, NS) })
  force(expr)
}

# Catalogue de test à deux recettes (injection) : la recette des lignes et une
# recette fictive de forme « agrégats » (#169, § 1), qui emploie en plus le
# rôle `montant` en facultatif (non discriminant) pour atteindre le refus 8
# sur un rôle qu'aucune recette du package n'emploie.
CAT2 <- rbind(CAT[1L, ], data.frame(
  recette = "agregats_test", requis = I(list(c("age", "evenements"))),
  un_parmi = I(list(list(c("exposition_centrale", "exposition_initiale")))),
  facultatifs = I(list(c("annee", "sexe", "segment", "montant"))),
  discriminants = I(list(c("age", "annee", "exposition_centrale", "exposition_initiale", "evenements"))),
  convention_native = NA_character_, seam = "cellules", motifs_ligne = I(list(character(0))),
  motifs_unite = I(list(character(0))), stringsAsFactors = FALSE))

# --- catalogue des recettes (ADR 0016, points 1.2 et 1.4) ---------------------

verifier("catalogue des recettes : colonnes recette, requis, un_parmi, facultatifs, discriminants, convention_native, seam, motifs_ligne, motifs_unite (valeur écrite à la main)",
         identical(names(CAT), c("recette", "requis", "un_parmi", "facultatifs", "discriminants", "convention_native",
                                 "seam", "motifs_ligne", "motifs_unite")))
verifier("catalogue des recettes, (3b) : deux lignes, lignes_dates_completes puis agregats ; ligne des lignes : rôles, convention native, seam et motifs (valeur écrite à la main)",
         identical(CAT$recette, c("lignes_dates_completes", "agregats")) &&
           identical(CAT$requis[[1L]], c("date_naissance", "date_entree", "date_sortie", "cause_sortie")) &&
           identical(CAT$un_parmi[[1L]], list()) &&
           identical(CAT$facultatifs[[1L]], c("id_contrat", "id_tete", "sexe", "annee_naissance", "segment")) &&
           identical(CAT$discriminants[[1L]], "date_entree") &&
           identical(CAT$convention_native[1L], "dernier_anniversaire") && identical(CAT$seam[1L], "fenetres") &&
           identical(CAT$motifs_ligne[[1L]], tablexp:::MOTIFS_LIGNES) &&
           identical(CAT$motifs_unite[[1L]], tablexp:::MOTIFS_TETE))
verifier("catalogue des recettes, (3b) : ligne agregats (#169, § 1 : age et evenements requis, une exposition parmi deux, annee, sexe et segment facultatifs, cinq discriminants, sans convention native, seam cellules, motifs de ligne des agrégats, aucun motif d'unité ; valeur écrite à la main)",
         identical(CAT$requis[[2L]], c("age", "evenements")) &&
           identical(CAT$un_parmi[[2L]], list(c("exposition_centrale", "exposition_initiale"))) &&
           identical(CAT$facultatifs[[2L]], c("annee", "sexe", "segment")) &&
           identical(CAT$discriminants[[2L]], c("age", "annee", "exposition_centrale", "exposition_initiale", "evenements")) &&
           identical(CAT$convention_native[2L], NA_character_) && identical(CAT$seam[2L], "cellules") &&
           identical(CAT$motifs_ligne[[2L]], tablexp:::MOTIFS_LIGNES_AGREGATS) &&
           identical(CAT$motifs_unite[[2L]], character(0)))
verifier("MOTIFS_LIGNES_AGREGATS : liste fermée de la spécification de #169, § 6.2, dans l'ordre (valeur écrite à la main)",
         identical(tablexp:::MOTIFS_LIGNES_AGREGATS,
                   c("age_manquant", "age_non_entier", "age_negatif", "age_aberrant", "annee_manquante",
                     "annee_non_entiere", "annee_hors_periode", "sexe_manquant", "sexe_hors_modalites",
                     "segment_manquant", "exposition_manquante", "exposition_non_finie", "exposition_negative",
                     "evenements_manquants", "evenements_non_entiers", "evenements_negatifs",
                     "evenements_sans_exposition", "cellule_dupliquee")))
invariants_catalogue <- function(cat) {
  e <- character(0)
  if (anyDuplicated(cat$recette)) e <- c(e, "noms de recettes en double")
  d <- cat$discriminants
  if (any(lengths(d) == 0L)) e <- c(e, "recette sans discriminant")
  if (anyDuplicated(unlist(d))) e <- c(e, "discriminants non disjoints entre recettes")
  noms <- unique(c(unlist(cat$requis), unlist(cat$un_parmi), unlist(cat$facultatifs), unlist(d)))
  if (!all(noms %in% tablexp:::CATALOGUE_ROLES$role))
    e <- c(e, paste("rôle hors de CATALOGUE_ROLES :", paste(setdiff(noms, tablexp:::CATALOGUE_ROLES$role), collapse = ", ")))
  if (!all(is.na(cat$convention_native) | cat$convention_native %in% tablexp:::ORDRE_CONVENTIONS))
    e <- c(e, "convention native hors de ORDRE_CONVENTIONS et non NA")
  if (!all(cat$seam %in% c("fenetres", "cellules"))) e <- c(e, "seam hors de fenetres, cellules")
  motifs <- unlist(mget(grep("^MOTIFS_", ls(NS, all.names = TRUE), value = TRUE), envir = NS), use.names = FALSE)
  if (!all(unlist(c(cat$motifs_ligne, cat$motifs_unite)) %in% motifs)) e <- c(e, "motif renvoyé absent des constantes MOTIFS_*")
  if (!all(vapply(cat$un_parmi, function(u) all(vapply(u, function(g) is.character(g) && length(g) >= 2L, NA)), NA)))
    e <- c(e, "groupe un_parmi qui n'est pas un vecteur d'au moins deux rôles")
  # Invariant facultatif de l'audit de (3a) : une recette retenue porte au
  # moins un de ses discriminants (requis, ou membre d'un groupe un_parmi).
  porte <- vapply(seq_len(nrow(cat)), function(k)
    any(cat$discriminants[[k]] %in% c(cat$requis[[k]], unlist(cat$un_parmi[[k]]))), NA)
  if (!all(porte)) e <- c(e, paste("aucun discriminant requis ni dans un_parmi :", paste(cat$recette[!porte], collapse = ", ")))
  if (length(e)) e else TRUE
}
verifier("catalogue des recettes : invariants (recettes uniques, discriminants disjoints et au moins un par recette, rôles du catalogue des rôles, convention native dans ORDRE_CONVENTIONS ou NA, seam fenetres ou cellules, motifs renvoyés existants)",
         invariants_catalogue(CAT))
verifier("catalogue des recettes : les invariants rejettent un catalogue de test aux discriminants non disjoints et à un rôle inconnu (témoin du contrôle)", {
  mauvais <- CAT2
  mauvais$discriminants[[2L]] <- c("date_entree", "age")
  mauvais$facultatifs[[2L]] <- c(mauvais$facultatifs[[2L]], "role_absent")
  e <- invariants_catalogue(mauvais)
  is.character(e) && any(grepl("non disjoints", e)) && any(grepl("role_absent", e))
})
verifier("catalogue des recettes : les invariants rejettent une recette dont aucun discriminant n'est requis ni dans un_parmi (témoin du contrôle)", {
  mauvais <- CAT2
  mauvais$discriminants[[2L]] <- "annee"
  e <- invariants_catalogue(mauvais)
  is.character(e) && any(grepl("aucun discriminant requis ni dans un_parmi : agregats_test", e, fixed = TRUE))
})
# Invariant « un libellé unique par code de motif » (avis d'actuary du
# 08/11/2026 sur #169, point 2 ; critère de (3b)) : un code présent dans
# plusieurs constantes MOTIFS_* (sexe_manquant, sexe_hors_modalites,
# segment_manquant, age_aberrant) a un seul sens.
invariant_libelles <- function(libelles, motifs) {
  e <- character(0)
  n <- names(libelles)
  if (is.null(n) || anyNA(n) || anyDuplicated(n)) e <- c(e, "codes de libellé absents ou en double")
  if (!setequal(n, motifs)) e <- c(e, paste("codes sans libellé ou libellés sans code :",
                                             paste(c(setdiff(motifs, n), setdiff(n, motifs)), collapse = ", ")))
  if (anyNA(libelles) || !all(nzchar(libelles))) e <- c(e, "libellé vide ou manquant")
  if (anyDuplicated(unname(libelles))) e <- c(e, "deux codes au même libellé")
  if (length(e)) e else TRUE
}
verifier("libellés des motifs : un libellé et un seul par code de l'union des motifs des recettes, libellés distincts (invariant)",
         invariant_libelles(tablexp:::LIBELLES_MOTIFS, tablexp:::motifs_recettes()))
verifier("libellés des motifs : codes communs aux listes de motifs de plusieurs recettes recensés (sexe_manquant, sexe_hors_modalites, segment_manquant, age_aberrant), chacun avec un seul libellé (valeur écrite à la main)", {
  tous <- unlist(lapply(seq_len(nrow(CAT)), function(k) unique(c(CAT$motifs_ligne[[k]], CAT$motifs_unite[[k]]))),
                 use.names = FALSE)
  communs <- sort(unique(tous[duplicated(tous)]))
  identical(communs, sort(c("sexe_manquant", "sexe_hors_modalites", "segment_manquant", "age_aberrant"))) &&
    all(lengths(lapply(communs, function(x) which(names(tablexp:::LIBELLES_MOTIFS) == x))) == 1L)
})
verifier("libellés des motifs : l'invariant rejette un code en double, un code sans libellé et deux codes au même libellé (témoin du contrôle)", {
  l <- tablexp:::LIBELLES_MOTIFS
  doublon <- c(l, age_aberrant = "autre sens")
  sans <- l[names(l) != "cellule_dupliquee"]
  meme <- l; meme[["age_manquant"]] <- meme[["annee_manquante"]]
  m <- tablexp:::motifs_recettes()
  is.character(invariant_libelles(doublon, m)) && is.character(invariant_libelles(sans, m)) &&
    any(grepl("cellule_dupliquee", invariant_libelles(sans, m), fixed = TRUE)) &&
    identical(invariant_libelles(meme, m), "deux codes au même libellé")
})
verifier("catalogue des recettes : le catalogue de test CAT2 passe les invariants (témoin des injections)",
         invariants_catalogue(CAT2))
verifier("RECETTE_BRANCHE et CONVENTION_NATIVE n'existent plus ; deduire_recette() est définie dans le module donnees (ADR 0016, point 1.5 et 2.1)", {
  src <- function(f) readLines(file.path(racine_depot(), "R", f), encoding = "UTF-8")
  !exists("RECETTE_BRANCHE", envir = NS, inherits = FALSE) && !exists("CONVENTION_NATIVE", envir = NS, inherits = FALSE) &&
    any(grepl("^deduire_recette <- function", src("donnees.R"))) &&
    !any(grepl("^deduire_recette <- function", src("exposition.R")))
})

# --- matrice recette x étape x estimateur (ADR 0016, points 5.1 et 5.4) ---------

verifier("matrice : colonnes recette, etape, estimateur, admis, role_requis, motif (valeur écrite à la main)",
         identical(names(MAT), c("recette", "etape", "estimateur", "admis", "role_requis", "motif")))
verifier("matrice : complétude, pour chaque recette du catalogue, exactement une ligne par étape de ETAPES (estimateur NA) et une par estimateur de ESTIMATEURS_BRUT sous brut, et aucune autre (invariant)", {
  e <- character(0)
  for (rec in CAT$recette) {
    m <- MAT[MAT$recette == rec, , drop = FALSE]
    for (et in tablexp:::ETAPES$etape)
      if (sum(m$etape == et & is.na(m$estimateur)) != 1L) e <- c(e, sprintf("%s / %s", rec, et))
    for (es in tablexp:::ESTIMATEURS_BRUT)
      if (sum(m$etape == "brut" & !is.na(m$estimateur) & m$estimateur == es) != 1L)
        e <- c(e, sprintf("%s / brut / %s", rec, es))
    if (nrow(m) != length(tablexp:::ETAPES$etape) + length(tablexp:::ESTIMATEURS_BRUT))
      e <- c(e, sprintf("%s : %d lignes", rec, nrow(m)))
  }
  if (!all(MAT$recette %in% CAT$recette)) e <- c(e, "recette de la matrice absente du catalogue")
  if (length(e)) paste("lignes manquantes ou en trop :", paste(e, collapse = " ; ")) else TRUE
})
verifier("matrice : admis dans oui, non, si_role ; role_requis (rôle du catalogue) seulement sous si_role ; motif présent si et seulement si la ligne n'est pas oui (invariant)",
         all(MAT$admis %in% c("oui", "non", "si_role")) &&
           identical(!is.na(MAT$role_requis), MAT$admis == "si_role") &&
           all(MAT$role_requis[!is.na(MAT$role_requis)] %in% tablexp:::CATALOGUE_ROLES$role) &&
           identical(!is.na(MAT$motif), MAT$admis != "oui"))
verifier("matrice, (3a) : sous lignes_dates_completes, toute étape et tout estimateur admis (ADR 0016, point 5.7 ; valeur écrite à la main)",
         all(MAT$admis[MAT$recette == "lignes_dates_completes"] == "oui"))
verifier("matrice, (3b) : lignes agregats (#169, § 4 et § 7 ; ADR 0016, point 5.1 ; valeur écrite à la main)", {
  m <- MAT[MAT$recette == "agregats", -1L]; rownames(m) <- NULL
  identical(m, data.frame(
    etape = c("donnees", "exposition", "brut", "brut", "brut", "brut", "positionnement", "homogeneite", "validation",
              "importance_exclusions"),
    estimateur = c(NA, NA, NA, "poisson", "binomial", "kaplan_meier", NA, NA, NA, NA),
    admis = c("oui", "oui", "oui", "si_role", "si_role", "non", "si_role", "si_role", "si_role", "non"),
    role_requis = c(NA, NA, NA, "exposition_centrale", "exposition_initiale", NA, "exposition_centrale",
                    "exposition_centrale", "exposition_centrale", NA),
    motif = c(NA, NA, NA, "poisson_sans_exposition_centrale", "binomial_sans_exposition_initiale", "kaplan_meier_agregats",
              "etape_exige_exposition_centrale", "etape_exige_exposition_centrale", "etape_exige_exposition_centrale",
              "importance_exclusions_agregats"),
    stringsAsFactors = FALSE))
})

# --- catalogue_roles() (ADR 0016, point 3 ; M330) ------------------------------

CR <- catalogue_roles()
verifier("catalogue_roles() : colonnes role, famille, type_admis, recettes, sans usage (valeur écrite à la main)",
         identical(names(CR), c("role", "famille", "type_admis", "recettes")))
verifier("catalogue_roles() : 22 rôles ; exposition remplacé par exposition_centrale et exposition_initiale (famille agregats) ; type numerique pour age, annee, exposition_centrale, exposition_initiale, evenements (valeur écrite à la main)",
         nrow(CR) == 22L && !("exposition" %in% CR$role) &&
           identical(CR$famille[match(c("exposition_centrale", "exposition_initiale"), CR$role)], c("agregats", "agregats")) &&
           all(CR$type_admis[match(c("age", "annee", "exposition_centrale", "exposition_initiale", "evenements"), CR$role)] ==
                 "integer, double"))
verifier("catalogue_roles() : recettes dérivée du catalogue des recettes : lignes_dates_completes pour ses rôles propres, agregats pour les siens, les deux pour sexe et segment, vide sinon (valeur écrite à la main)", {
  v <- stats::setNames(CR$recettes, CR$role)
  emp <- c("id_contrat", "id_tete", "date_naissance", "annee_naissance", "date_entree", "date_sortie", "cause_sortie")
  agr <- c("age", "annee", "exposition_centrale", "exposition_initiale", "evenements")
  all(v[emp] == "lignes_dates_completes") && all(v[agr] == "agregats") &&
    all(v[c("sexe", "segment")] == "lignes_dates_completes, agregats") &&
    identical(CR$role[CR$recettes == ""], c("age_fourni", "date_age_fourni", "date_evenement", "date_inventaire", "montant",
                                            "etat_depart", "etat_arrivee", "date_changement"))
})
verifier("catalogue_roles() : sous un catalogue de deux recettes (injection), recettes jointes par « , » dans l'ordre du catalogue (sexe, segment), recette seule (montant, age) ou vide (date_inventaire)", {
  cr <- avec_constante("CATALOGUE_RECETTES", CAT2, catalogue_roles())
  v <- stats::setNames(cr$recettes, cr$role)
  identical(unname(v[c("sexe", "segment", "montant", "age", "date_naissance", "date_inventaire")]),
            c("lignes_dates_completes, agregats_test", "lignes_dates_completes, agregats_test", "agregats_test",
              "agregats_test", "lignes_dates_completes", "")) &&
    identical(catalogue_roles(), CR)
})

# --- type numerique (ADR 0016, point 6.5 ; #169, Q5) ----------------------------

verifier("type numerique : integer et double admis, valeurs manquantes, non finies, négatives et non entières comprises ; caractère, facteur, Date, logique, matrice refusés (classe seule)", {
  tc <- tablexp:::type_conforme
  all(vapply(list(1:3, c(1.5, NA, -2), c(Inf, NaN), NA_integer_, numeric(0)), tc, NA, type = "numerique")) &&
    !any(vapply(list("1", factor("a"), D("2020-01-01"), TRUE, matrix(1:4, 2L)), tc, NA, type = "numerique"))
})

# --- deduire_recette() (ADR 0016, point 2.1) ----------------------------------

verifier("deduire_recette() : rôles des lignes -> recette, convention native, rôles requis et employés, seam, motifs, matrice de la recette (valeur écrite à la main)", {
  r <- tablexp:::deduire_recette(R0)
  m <- MAT[MAT$recette == "lignes_dates_completes", -1L]; rownames(m) <- NULL
  identical(names(r), c("recette", "convention_native", "roles_requis", "roles_employes", "seam", "motifs", "matrice")) &&
    identical(r$recette, "lignes_dates_completes") && identical(r$convention_native, "dernier_anniversaire") &&
    identical(r$roles_requis, c("date_naissance", "date_entree", "date_sortie", "cause_sortie")) &&
    identical(r$roles_employes, c("date_naissance", "date_entree", "date_sortie", "cause_sortie", "id_contrat", "id_tete",
                                  "sexe", "annee_naissance", "segment")) &&
    identical(r$seam, "fenetres") && identical(r$motifs, c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE)) &&
    identical(r$matrice, m)
})
verifier("deduire_recette() : rôle requis absent -> refus aucune_recette, module donnees, message visé par actuary, details (recette, role et un_parmi nommés par recette)", {
  r <- rf(tablexp:::deduire_recette(R0[-3L]))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "aucune_recette") &&
    identical(conditionMessage(r), paste("Aucune recette d'exposition ne s'applique ; rôles requis absents, recette par recette :",
                                         "recette « lignes_dates_completes » : « date_sortie ».")) &&
    identical(r$details, list(recette = "lignes_dates_completes", role = list(lignes_dates_completes = "date_sortie"),
                              un_parmi = list(lignes_dates_completes = list())))
})
verifier("deduire_recette(), catalogue injecté : un rôle propre de la recette agrégats déclaré -> seule cette recette listée, rôles requis puis « un rôle parmi » (message écrit à la main)", {
  r <- rf(tablexp:::deduire_recette(c(age = "a"), CAT2))
  !is.null(r) && identical(r$motif, "aucune_recette") &&
    identical(conditionMessage(r), paste("Aucune recette d'exposition ne s'applique ; rôles requis absents, recette par recette :",
                                         "recette « agregats_test » : « evenements », un rôle parmi « exposition_centrale »,",
                                         "« exposition_initiale ».")) &&
    identical(r$details$recette, "agregats_test") &&
    identical(r$details$un_parmi, list(agregats_test = list(c("exposition_centrale", "exposition_initiale"))))
})
verifier("deduire_recette(), catalogue injecté : aucun rôle propre déclaré -> toutes les recettes listées, dans l'ordre du catalogue (message écrit à la main)", {
  r <- rf(tablexp:::deduire_recette(c(sexe = "sx"), CAT2))
  !is.null(r) && identical(r$motif, "aucune_recette") &&
    identical(conditionMessage(r), paste("Aucune recette d'exposition ne s'applique ; rôles requis absents, recette par recette :",
                                         "recette « lignes_dates_completes » : « date_naissance », « date_entree », « date_sortie »,",
                                         "« cause_sortie » ; recette « agregats_test » : « age », « evenements », un rôle parmi",
                                         "« exposition_centrale », « exposition_initiale ».")) &&
    identical(r$details$recette, c("lignes_dates_completes", "agregats_test"))
})
verifier("deduire_recette(), catalogue injecté : rôles complets de la recette agrégats -> agregats_test, convention native NA, seam cellules", {
  r <- tablexp:::deduire_recette(c(age = "a", evenements = "ev", exposition_initiale = "x"), CAT2)
  identical(r$recette, "agregats_test") && identical(r$convention_native, NA_character_) && identical(r$seam, "cellules")
})
MSG_CONC <- paste("Les rôles déclarés relèvent de plusieurs recettes d'exposition (rôles propres à chacune :",
                  "recette « lignes_dates_completes » : « date_entree » (colonnes « e ») ; recette « agregats_test » :",
                  "« age », « evenements » (colonnes « a », « ev »)) ; la recette n'est pas choisie à la place de",
                  "l'utilisateur : retirer les rôles de la recette qui ne s'applique pas, la construction est refusée.")
verifier("deduire_recette(), catalogue injecté : discriminants de deux recettes -> refus recettes_concurrentes, module donnees, message visé par actuary, details (recettes, rôles, colonnes)", {
  r <- rf(tablexp:::deduire_recette(c(R0, age = "a", evenements = "ev"), CAT2))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "recettes_concurrentes") &&
    identical(conditionMessage(r), MSG_CONC) &&
    identical(r$details, list(recette = c("lignes_dates_completes", "agregats_test"),
                              role = c("date_entree", "age", "evenements"), colonne = c("e", "a", "ev")))
})

# --- ordre des refus de controler_roles() (ADR 0016, point 2.2) --------------

cr_motif <- function(d, roles, segmentation = character(0)) {
  r <- rf(tablexp:::controler_roles(d, roles, segmentation))
  if (is.null(r)) "aucun refus" else r$motif
}
BASE_TXT <- BASE; BASE_TXT$n <- as.character(BASE_TXT$n)
BASE_HORS <- BASE; BASE_HORS$e[1L] <- structure(-800000, class = "Date")
verifier("ordre des refus, catalogue du package : (2) rôle inconnu avant (4) rôle non pris en charge ; (4) avant (5) recettes_concurrentes ; (4) et (5) avant (6) colonne absente ; (6) type incompatible et date hors domaine avant (7) aucune_recette ; (7) avant (9) segmentation ; (8) avant (9)", {
  obtenu <- c(cr_motif(BASE, c(R0, foo = "x", montant = "m")), cr_motif(BASE, c(R0, montant = "absente", age = "a")),
              cr_motif(BASE, c(R0, age = "absente")),
              cr_motif(BASE_TXT, R0[-3L]), cr_motif(BASE_HORS, R0[-3L]), cr_motif(BASE, R0[-3L], "sexe"),
              cr_motif(BASE, c(age = "a", evenements = "ev", exposition_centrale = "m", id_contrat = "sx"), "sexe"))
  attendu <- c("role_inconnu", "role_non_pris_en_charge", "recettes_concurrentes", "type_incompatible",
               "date_hors_domaine", "aucune_recette", "role_non_pris_en_charge")
  if (identical(obtenu, attendu)) TRUE else paste("obtenu", paste(obtenu, collapse = ", "))
})
verifier("ordre des refus, catalogue injecté : (4) avant (5) ; (5) avant (6) colonne absente ; (7) avant (8) ; (8) avant (9) segmentation", {
  obtenu <- avec_constante("CATALOGUE_RECETTES", CAT2, c(
    cr_motif(BASE, c(R0, age = "a", date_inventaire = "di")), cr_motif(BASE, c(R0, age = "absente")),
    cr_motif(BASE, c(R0[-3L], montant = "m")), cr_motif(BASE, c(R0, montant = "m"), "sexe")))
  attendu <- c("role_non_pris_en_charge", "recettes_concurrentes", "aucune_recette", "role_non_pris_en_charge")
  if (identical(obtenu, attendu)) TRUE else paste("obtenu", paste(obtenu, collapse = ", "))
})
verifier("controler_roles() rend la recette retenue (deduire_recette())", {
  identical(tablexp:::controler_roles(BASE, R0, character(0)), tablexp:::deduire_recette(R0))
})

# --- messages des refus au niveau des rôles (visés par actuary) --------------

verifier("role_non_pris_en_charge, refus 4 : message visé par actuary (rôle qu'aucune recette du catalogue n'emploie), details rôle et colonne", {
  r <- rf(construire_table(BASE, c(R0, montant = "m", date_evenement = "di"), plan_type()))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "role_non_pris_en_charge") &&
    identical(conditionMessage(r), paste("Rôle déclaré qu'aucune recette d'exposition du catalogue n'emploie :",
                                         "« montant », « date_evenement » (colonne « m », « di ») ; l'ignorer changerait",
                                         "le sens du résultat, la construction est refusée.")) &&
    identical(r$details, list(role = c("montant", "date_evenement"), colonne = c("m", "di")))
})
MSG_CONC_PKG <- paste("Les rôles déclarés relèvent de plusieurs recettes d'exposition (rôles propres à chacune :",
                      "recette « lignes_dates_completes » : « date_entree » (colonnes « e ») ; recette « agregats » :",
                      "« age », « evenements » (colonnes « a », « ev »)) ; la recette n'est pas choisie à la place de",
                      "l'utilisateur : retirer les rôles de la recette qui ne s'applique pas, la construction est refusée.")
verifier("recettes_concurrentes, catalogue du package (#169, ADR 0016, point 2.3 ; passage visé par actuary) : base en lignes qui déclare age et evenements -> refus 5, module donnees, message visé, details (recettes, rôles, colonnes)", {
  r <- rf(construire_table(BASE, c(R0, age = "a", evenements = "ev"), plan_type()))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "recettes_concurrentes") &&
    identical(conditionMessage(r), MSG_CONC_PKG) &&
    identical(r$details, list(recette = c("lignes_dates_completes", "agregats"), role = c("date_entree", "age", "evenements"),
                              colonne = c("e", "a", "ev")))
})
verifier("recettes_concurrentes, catalogue du package : base en lignes qui déclare exposition_centrale, exposition_initiale ou annee -> refus 5 (auparavant role_non_pris_en_charge ; point 6 du brief de (3b))", {
  all(vapply(list(c(R0, exposition_centrale = "m"), c(R0, exposition_initiale = "m"), c(R0, annee = "a")),
             function(ro) identical(rf(construire_table(BASE, ro, plan_type()))$motif, "recettes_concurrentes"), NA))
})
verifier("role_non_pris_en_charge, refus 8, catalogue du package : rôle des lignes déclaré avec les rôles des agrégats -> message relatif à la recette agregats (visé par actuary)", {
  ag <- data.frame(a = 60, ev = 1, x = 2, ct = "C1", stringsAsFactors = FALSE)
  r <- rf(tablexp:::controler_roles(ag, c(age = "a", evenements = "ev", exposition_centrale = "x", id_contrat = "ct"),
                                    character(0)))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "role_non_pris_en_charge") &&
    identical(conditionMessage(r), paste("Rôle déclaré que la recette « agregats » n'emploie pas :",
                                         "« id_contrat » (colonne « ct », employé par la recette « lignes_dates_completes ») ;",
                                         "l'ignorer changerait le sens du résultat, la construction est refusée.")) &&
    identical(r$details, list(role = "id_contrat", colonne = "ct", recette = "agregats"))
})
verifier("deduire_recette(), catalogue du package : age seul -> aucune_recette, seule la recette agregats listée (rôles propres déclarés), « un rôle parmi » (message écrit à la main)", {
  r <- rf(tablexp:::deduire_recette(c(age = "a")))
  !is.null(r) && identical(r$motif, "aucune_recette") &&
    identical(conditionMessage(r), paste("Aucune recette d'exposition ne s'applique ; rôles requis absents, recette par recette :",
                                         "recette « agregats » : « evenements », un rôle parmi « exposition_centrale »,",
                                         "« exposition_initiale ».")) &&
    identical(r$details, list(recette = "agregats", role = list(agregats = "evenements"),
                              un_parmi = list(agregats = list(c("exposition_centrale", "exposition_initiale")))))
})
verifier("deduire_recette(), catalogue du package : rôles des agrégats -> recette agregats, convention native NA, seam cellules, motifs des agrégats, matrice de la recette (valeur écrite à la main)", {
  r <- tablexp:::deduire_recette(c(age = "a", evenements = "ev", exposition_initiale = "x", sexe = "s"))
  m <- MAT[MAT$recette == "agregats", -1L]; rownames(m) <- NULL
  identical(r$recette, "agregats") && identical(r$convention_native, NA_character_) && identical(r$seam, "cellules") &&
    identical(r$roles_requis, c("age", "evenements")) &&
    identical(r$roles_employes, c("age", "evenements", "exposition_centrale", "exposition_initiale", "annee", "sexe", "segment")) &&
    identical(r$motifs, tablexp:::MOTIFS_LIGNES_AGREGATS) && identical(r$matrice, m)
})
verifier("role_non_pris_en_charge, refus 8 (catalogue injecté) : message visé par actuary (rôle que la recette retenue n'emploie pas, recette qui l'emploie), details rôle, colonne, recette", {
  r <- avec_constante("CATALOGUE_RECETTES", CAT2, rf(construire_table(BASE, c(R0, montant = "m"), plan_type())))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "role_non_pris_en_charge") &&
    identical(conditionMessage(r), paste("Rôle déclaré que la recette « lignes_dates_completes » n'emploie pas :",
                                         "« montant » (colonne « m », employé par la recette « agregats_test ») ;",
                                         "l'ignorer changerait le sens du résultat, la construction est refusée.")) &&
    identical(r$details, list(role = "montant", colonne = "m", recette = "lignes_dates_completes"))
})
verifier("recettes_concurrentes par construire_table() (catalogue injecté) : même refus et même message que deduire_recette(), avant le contrôle des colonnes", {
  r <- avec_constante("CATALOGUE_RECETTES", CAT2, rf(construire_table(BASE, c(R0, age = "a", evenements = "ev"), plan_type())))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "recettes_concurrentes") &&
    identical(conditionMessage(r), MSG_CONC)
})
verifier("rôle exposition retiré du catalogue (ADR 0016, point 2.3) : refus role_inconnu", {
  r <- rf(construire_table(BASE, c(R0, exposition = "m"), plan_type()))
  !is.null(r) && identical(r$motif, "role_inconnu")
})

# --- controler_matrice_recette() (ADR 0016, points 5.2 et 5.3) ------------------

REC <- tablexp:::deduire_recette(R0)
avec_ligne <- function(etape, estimateur, admis, role = NA_character_, motif = NA_character_) {
  rec <- REC
  k <- which(rec$matrice$etape == etape & (if (is.na(estimateur)) is.na(rec$matrice$estimateur)
                                           else (!is.na(rec$matrice$estimateur) & rec$matrice$estimateur == estimateur)))
  rec$matrice$admis[k] <- admis; rec$matrice$role_requis[k] <- role; rec$matrice$motif[k] <- motif
  rec
}
PLAN_BRUT <- plan_type(etapes = list(donnees = list(exclusions = character(0)),
                                     exposition = list(convention = "plus_fine", par_annee = FALSE),
                                     brut = list(estimateurs = c("poisson", "kaplan_meier"), niveau = 0.95)))
verifier("controler_matrice_recette() : matrice des lignes, plan avec brut -> aucun refus", {
  is.null(rf(tablexp:::controler_matrice_recette(REC, R0, PLAN_BRUT)))
})
verifier("controler_matrice_recette() : ligne d'étape non -> refus sous le motif de la ligne, module donnees, message qui nomme l'étape et la recette (message écrit à la main)", {
  r <- rf(tablexp:::controler_matrice_recette(avec_ligne("exposition", NA, "non", motif = "motif_test"), R0, PLAN_BRUT))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "motif_test") &&
    identical(conditionMessage(r), paste("Sous la recette « lignes_dates_completes », l'étape « exposition » n'est pas",
                                         "admise ; la construction est refusée.")) &&
    identical(r$details, list(recette = "lignes_dates_completes", etape = "exposition", estimateur = NA_character_))
})
verifier("controler_matrice_recette() : estimateur si_role sans son rôle -> refus qui nomme l'estimateur et le rôle ; avec le rôle déclaré -> admis ; estimateur absent du plan -> non lu", {
  rec <- avec_ligne("brut", "kaplan_meier", "si_role", role = "id_tete", motif = "km_test")
  r <- rf(tablexp:::controler_matrice_recette(rec, R0, PLAN_BRUT))
  plan_p <- PLAN_BRUT; plan_p$etapes$brut$estimateurs <- "poisson"
  !is.null(r) && identical(r$motif, "km_test") &&
    identical(conditionMessage(r), paste("Sous la recette « lignes_dates_completes », l'estimateur « kaplan_meier » de",
                                         "l'étape « brut » n'est pas admis sans le rôle « id_tete » ; la construction est refusée.")) &&
    identical(r$details, list(recette = "lignes_dates_completes", etape = "brut", estimateur = "kaplan_meier", role = "id_tete")) &&
    is.null(rf(tablexp:::controler_matrice_recette(rec, c(R0, id_tete = "t"), PLAN_BRUT))) &&
    is.null(rf(tablexp:::controler_matrice_recette(rec, R0, plan_p)))
})
verifier("controler_matrice_recette() : ligne absente de la matrice -> erreur de programmation, jamais une admission ni un refus", {
  rec <- REC; rec$matrice <- rec$matrice[rec$matrice$etape != "exposition", ]
  e <- tryCatch({ tablexp:::controler_matrice_recette(rec, R0, PLAN_BRUT); NULL }, error = function(e) e)
  inherits(e, "error") && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
})
verifier("controler_matrice_recette() : exclusion hors des motifs de la recette retenue -> refus exclusion_hors_recette, module donnees, message visé par actuary", {
  rec <- REC; rec$motifs <- c("contrat_duplique", "age_aberrant")
  pl <- PLAN_BRUT; pl$etapes$donnees$exclusions <- c("age_aberrant", "sexe_manquant")
  r <- rf(tablexp:::controler_matrice_recette(rec, R0, pl))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "exclusion_hors_recette") &&
    identical(conditionMessage(r), paste("Code d'exclusion sans objet pour la recette « lignes_dates_completes » :",
                                         "« sexe_manquant » (motifs de cette recette : « contrat_duplique », « age_aberrant »).")) &&
    identical(r$details, list(recette = "lignes_dates_completes", exclusions = "sexe_manquant"))
})
verifier("controler_matrice_recette() appelée par construire_table() avant toute étape (matrice injectée : Kaplan-Meier non admis sous les lignes)", {
  m <- MAT; m$admis[m$estimateur %in% "kaplan_meier"] <- "non"; m$motif[m$estimateur %in% "kaplan_meier"] <- "km_test"
  r <- avec_constante("MATRICE_RECETTES", m, rf(construire_table(BASE, R0, PLAN_BRUT)))
  !is.null(r) && identical(r$module, "donnees") && identical(r$motif, "km_test") &&
    !inherits(rf(construire_table(BASE, R0, PLAN_BRUT)), "refus_tablexp")
})

# --- resoudre_convention() sans convention native (ADR 0016, point 5.5) ------

verifier("resoudre_convention(), native NA : plus_fine -> refus convention_non_declaree, module exposition (message écrit à la main)", {
  r <- rf(tablexp:::resoudre_convention("plus_fine", NA_character_))
  !is.null(r) && identical(r$module, "exposition") && identical(r$motif, "convention_non_declaree") &&
    identical(conditionMessage(r), paste("Convention d'âge « plus_fine » sans objet : la recette retenue n'a pas de convention",
                                         "native (la convention d'âge ne se déduit pas des rôles) ; déclarer la convention des",
                                         "données, prise dans « dernier_anniversaire », « age_atteint_annee »."))
})
verifier("resoudre_convention(), native NA : convention déclarée retenue telle quelle, native NA, non forcée", {
  rc <- tablexp:::resoudre_convention
  identical(rc("dernier_anniversaire", NA_character_),
            list(demandee = "dernier_anniversaire", native = NA_character_, retenue = "dernier_anniversaire", forcee = FALSE)) &&
    identical(rc("age_atteint_annee", NA_character_),
              list(demandee = "age_atteint_annee", native = NA_character_, retenue = "age_atteint_annee", forcee = FALSE))
})

# --- union des exclusions (ADR 0016, point 6.4) ------------------------------

verifier("union des motifs des recettes : motifs des lignes (ligne puis tête), puis motifs des agrégats absents des précédents, sous le catalogue du package ; ordre canonique des exclusions des lignes inchangé (#169)", {
  u <- tablexp:::motifs_recettes()
  identical(u, unique(c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE, tablexp:::MOTIFS_LIGNES_AGREGATS))) &&
    identical(u[seq_len(20L)], c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE)) && length(u) == 34L &&
    identical(tablexp:::ordres_canoniques()[["etapes/donnees/exclusions"]], u)
})
verifier("exclusion propre aux agrégats dans un plan des lignes : admise par valider_plan() (union), refusée exclusion_hors_recette avant toute étape (auparavant exclusion_inconnue ; #169)", {
  pl <- plan_type(); pl$etapes$donnees$exclusions <- "cellule_dupliquee"
  r <- rf(construire_table(BASE, R0, pl))
  is.null(rf(valider_plan(pl))) && !is.null(r) && identical(r$motif, "exclusion_hors_recette") &&
    identical(r$details, list(recette = "lignes_dates_completes", exclusions = "cellule_dupliquee"))
})
verifier("union des motifs, catalogue injecté : motifs d'une autre recette admis par valider_plan() (exclusion_inconnue seulement hors de l'union)", {
  cat3 <- CAT2; cat3$motifs_ligne[[2L]] <- "motif_test_agregats"
  pl <- plan_type(); pl$etapes$donnees$exclusions <- "motif_test_agregats"
  pl2 <- pl; pl2$etapes$donnees$exclusions <- "motif_inconnu"
  avec_constante("CATALOGUE_RECETTES", cat3, {
    is.null(rf(valider_plan(pl))) && identical(rf(valider_plan(pl2))$motif, "exclusion_inconnue") &&
      identical(tablexp:::motifs_recettes(), c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE, "motif_test_agregats"))
  })
})
