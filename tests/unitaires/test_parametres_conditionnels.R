# -----------------------------------------------------------------------------
#  tests/unitaires/test_parametres_conditionnels.R : commit préparatoire (1a)
#  de B19 (#223 ; ADR 0017, points 1.2, 1.5, 3.2, 3.3 et 10.1) — colonnes
#  `selon` et `valeurs` de PARAMETRES_ETAPES, controler_parametres_condition(),
#  canoniser_plan() « sans condition », colonne `table_construite` d'ETAPES et
#  contrat de la table construite (table_construite()), liste blanche de
#  controler_plage_ages() dans le module exposition
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : catalogues fabriqués injectés (précédent
#    M247), motifs, modules, messages et details des refus, tables
#    construites fabriquées ;
#  - identité exacte : canoniser_plan() idempotente.
#  Les contrôles sur les plans de référence (apparier_table() identical() à
#  l'ancien corps sur R1 et R4, table_construite() sur R10 et R13) sont dans
#  tests/test_reproductibilite.R, qui construit ces plans.
# -----------------------------------------------------------------------------

refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
erreur_programmation <- function(expr) {
  e <- tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e)
  !is.null(e) && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
}
# Refus attendu : module, motif, message, details ; TRUE ou le texte de l'écart.
controle <- function(r, motif, message, details) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, "construire_table")) return(sprintf("module « %s »", r$module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!identical(conditionMessage(r), message)) return(sprintf("message « %s »", conditionMessage(r)))
  if (!identical(r$details, details)) return("details différents")
  TRUE
}

# Invariants du catalogue (ADR 0017, point 3.2) ; TRUE ou le texte de l'écart.
#  (i) jamais `recette` et `selon` non NA ensemble ;
#  (ii) `selon` non NA nomme un paramètre de la même étape, de type character,
#       dont `recette` et `selon` sont NA (profondeur 1) ;
#  (iii) is.na(selon) si et seulement si length(valeurs) == 0 ; sinon
#       `valeurs` est character, sans NA ni doublon.
invariants_catalogue <- function(cat) {
  for (e in names(cat)) {
    t <- cat[[e]]
    if (!identical(names(t), c("parametre", "type", "domaine", "recette", "selon", "valeurs")))
      return(sprintf("%s : colonnes %s", e, paste(names(t), collapse = ", ")))
    if (!is.character(t$selon) || !is.list(t$valeurs)) return(sprintf("%s : types de selon ou valeurs", e))
    if (any(!is.na(t$recette) & !is.na(t$selon))) return(sprintf("%s : (i) recette et selon ensemble", e))
    for (k in which(!is.na(t$selon))) {
      g <- match(t$selon[k], t$parametre)
      if (is.na(g) || !identical(t$type[g], "character") || !is.na(t$recette[g]) || !is.na(t$selon[g]))
        return(sprintf("%s : (ii) gouvernant de %s", e, t$parametre[k]))
    }
    vide <- lengths(t$valeurs) == 0L
    if (!identical(is.na(t$selon), vide)) return(sprintf("%s : (iii) selon NA <=> valeurs vides", e))
    for (k in which(!vide)) {
      v <- t$valeurs[[k]]
      if (!is.character(v) || anyNA(v) || anyDuplicated(v)) return(sprintf("%s : (iii) valeurs de %s", e, t$parametre[k]))
    }
  }
  TRUE
}

# Catalogue fabriqué d'une étape (noms indicatifs de l'ADR 0017, point 3.4) :
# deux gouvernants (methode, critere) ; les paramètres dépendants de critere
# sont écrits avant ceux de methode, pour vérifier que les gouvernants sont
# parcourus dans l'ordre du catalogue.
CAT_F <- data.frame(
  parametre = c("methode", "critere", "parametre_lissage", "intervalle_lissage", "ponderation", "niveau"),
  type = c("character", "character", "double", "double", "character", "double"),
  domaine = "fabriqué",
  recette = NA_character_,
  selon = c(NA, NA, "critere", "critere", "methode", "methode"),
  valeurs = I(list(character(0), character(0), "declare", "laml", c("a", "c"), "b")),
  stringsAsFactors = FALSE)
cpc <- function(pe, cat = CAT_F) tablexp:::controler_parametres_condition("lissage", pe, cat)

# --- A. Catalogue réel ----------------------------------------------------------

verifier("A PARAMETRES_ETAPES : colonnes parametre, type, domaine, recette, selon, valeurs ; invariants (i) à (iii) (ADR 0017, point 3.2 ; valeur calculée à la main)",
         invariants_catalogue(tablexp:::PARAMETRES_ETAPES))
verifier("A PARAMETRES_ETAPES : aucun paramètre conditionnel à la valeur d'un paramètre en (1a) (selon NA, valeurs vides partout)",
         all(vapply(tablexp:::PARAMETRES_ETAPES, function(t) all(is.na(t$selon)) && all(lengths(t$valeurs) == 0L), NA)))
verifier("A invariants : le catalogue fabriqué les tient ; chaque violation fabriquée est détectée (témoins)", {
  c1 <- CAT_F; c1$recette[3L] <- "agregats"                       # (i)
  c2 <- CAT_F; c2$selon[3L] <- "inconnu"                           # (ii) gouvernant absent
  c3 <- CAT_F; c3$selon[5L] <- "niveau"                            # (ii) gouvernant double
  c4 <- CAT_F; c4$selon[6L] <- "ponderation"                       # (ii) profondeur 2
  c5 <- CAT_F; c5$valeurs[[1L]] <- "x"                             # (iii) valeurs sans selon
  c6 <- CAT_F; c6$valeurs[[3L]] <- character(0)                    # (iii) selon sans valeurs
  c7 <- CAT_F; c7$valeurs[[5L]] <- c("a", "a")                     # (iii) doublon
  c8 <- CAT_F; c8$valeurs[[5L]] <- c("a", NA)                      # (iii) NA
  isTRUE(invariants_catalogue(list(lissage = CAT_F))) &&
    all(vapply(list(c1, c2, c3, c4, c5, c6, c7, c8), function(c_) !isTRUE(invariants_catalogue(list(lissage = c_))), NA))
})
verifier("A controler_parametres_condition() : sans effet sur chaque étape du catalogue réel (aucun gouvernant), paramètres quelconques",
         all(vapply(names(tablexp:::PARAMETRES_ETAPES), function(e)
           is.null(tablexp:::controler_parametres_condition(e, list(x = 1), tablexp:::PARAMETRES_ETAPES[[e]])), NA)))

# --- B. controler_parametres_condition() par injection (M247) -------------------

verifier("B plan complet sous methode = a, critere = declare : aucun refus", is.null(refus_de(
  cpc(list(methode = "a", critere = "declare", parametre_lissage = 1, ponderation = "exposition")))))
verifier("B plan complet sous methode = b, critere = laml : aucun refus", is.null(refus_de(
  cpc(list(methode = "b", critere = "laml", intervalle_lissage = c(1, 2), niveau = 0.95)))))
verifier("B valeur prise dans une liste de deux valeurs (methode = c, ponderation requise) : aucun refus", is.null(refus_de(
  cpc(list(methode = "c", critere = "declare", parametre_lissage = 1, ponderation = "uniforme")))))
verifier("B parametre_manquant seul (ponderation sous methode = a) : module, motif, message exact, details", controle(
  refus_de(cpc(list(methode = "a", critere = "declare", parametre_lissage = 1))), "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « ponderation » sous methode = « a » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "ponderation", gouvernant = "methode", valeur = "a")))
verifier("B parametre_inattendu seul (niveau sous methode = a) : module, motif, message exact, details", controle(
  refus_de(cpc(list(methode = "a", critere = "declare", parametre_lissage = 1, ponderation = "uniforme", niveau = 0.9))),
  "parametre_inattendu",
  paste("Étape « lissage » : paramètre(s) « niveau » sans objet sous methode = « a » (employé(s) sous methode = « b ») ;",
        "un paramètre sans effet n'est pas admis dans le plan."),
  list(etape = "lissage", parametre = "niveau", gouvernant = "methode", valeur = "a")))
verifier("B parametre_inattendu, valeurs d'emploi multiples (ponderation sous methode = b : employé sous a, c)", controle(
  refus_de(cpc(list(methode = "b", critere = "laml", intervalle_lissage = c(1, 2), niveau = 0.9, ponderation = "uniforme"))),
  "parametre_inattendu",
  paste("Étape « lissage » : paramètre(s) « ponderation » sans objet sous methode = « b » (employé(s) sous methode =",
        "« a », « c ») ; un paramètre sans effet n'est pas admis dans le plan."),
  list(etape = "lissage", parametre = "ponderation", gouvernant = "methode", valeur = "b")))
verifier("B parametre_manquant sous le second gouvernant (parametre_lissage sous critere = declare)", controle(
  refus_de(cpc(list(methode = "a", critere = "declare", ponderation = "uniforme"))), "parametre_manquant",
  "Étape « lissage » : paramètre(s) requis absent(s) « parametre_lissage » sous critere = « declare » (aucun défaut implicite).",
  list(etape = "lissage", parametre = "parametre_lissage", gouvernant = "critere", valeur = "declare")))
verifier("B ordre : gouvernants dans l'ordre du catalogue (methode avant critere, quoique ses dépendants soient écrits après)", {
  r <- refus_de(cpc(list(methode = "a", critere = "declare")))
  !is.null(r) && identical(r$details$gouvernant, "methode") && identical(r$details$parametre, "ponderation")
})
verifier("B ordre : sous un gouvernant, parametre_manquant avant parametre_inattendu", {
  r <- refus_de(cpc(list(methode = "a", critere = "declare", parametre_lissage = 1, niveau = 0.9)))
  !is.null(r) && identical(r$motif, "parametre_manquant") && identical(r$details$parametre, "ponderation")
})
verifier("B gouvernant absent ou non chaîne unique : erreur de programmation (gouvernant contrôlé avant)",
         erreur_programmation(cpc(list(critere = "declare", parametre_lissage = 1))) &&
           erreur_programmation(cpc(list(methode = c("a", "b"), critere = "declare"))))

# --- C. canoniser_plan() : « sans condition » (ADR 0017, point 3.2) -------------

local({
  pc <- tablexp:::PARAMETRES_ETAPES
  # Catalogue injecté : niveau de brut conditionnel à la valeur d'un
  # gouvernant fabriqué (seul le marquage `selon` importe à canoniser_plan()).
  pc$brut$selon[2L] <- "estimateurs"
  pc$brut$valeurs[[2L]] <- "binomial"
  cn <- function(p, cat = pc) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, cat, tablexp:::ordres_canoniques())
  erreur <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
  plan <- function(brut) list(
    transition = list(depart = "vivant", arrivee = "deces"),
    periode = list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31")), delai_declaration = 0,
    segmentation = "sexe",
    etapes = list(donnees = list(exclusions = character(0)), exposition = list(convention = "plus_fine", par_annee = FALSE),
                  brut = brut))
  absent <- plan(list(estimateurs = "poisson"))
  present <- plan(list(niveau = 0.95, estimateurs = "poisson"))
  verifier("C canoniser_plan(), catalogue injecté : paramètre conditionnel absent admis, plan idempotent",
           !erreur(cn(absent)) && identical(cn(cn(absent)), cn(absent)) && identical(names(cn(absent)$etapes$brut), "estimateurs"))
  verifier("C canoniser_plan(), catalogue injecté : paramètre conditionnel présent rangé dans l'ordre du catalogue, plan idempotent",
           identical(names(cn(present)$etapes$brut), c("estimateurs", "niveau")) && identical(cn(cn(present)), cn(present)))
  verifier("C canoniser_plan() : catalogue réel, paramètre sans condition absent -> erreur de programmation (témoin)",
           erreur(cn(absent, tablexp:::PARAMETRES_ETAPES)))
})

# --- D. ETAPES$table_construite et table_construite() (ADR 0017, points 1.2 et 1.5)

verifier("D ETAPES$table_construite : character, table_positionnee pour positionnement, NA ailleurs (valeur calculée à la main)",
         identical(tablexp:::ETAPES$table_construite, c(NA, NA, NA, "table_positionnee", NA, NA, NA)) &&
           identical(names(tablexp:::ETAPES), c("etape", "module", "fonction", "prealable", "aleatoire", "table_construite")))

local({
  tp <- data.frame(sexe = rep(c("H", "F"), each = 3L), age = rep(50:52, 2L), qx_reference = 0.01,
                   qx = c(0.01, 0.02, 0.03, 0.005, 0.01, 0.015), etat = "produite", stringsAsFactors = FALSE)
  etapes_de <- function(t, segments = NULL, plage = c(50, 52)) {
    r <- list(table_positionnee = t)
    if (!is.null(segments)) r$segments <- segments
    list(positionnement = list(parametres = list(plage_ages = plage), resultat = r))
  }
  tc <- function(t, segmentation = "sexe", ...) tablexp:::table_construite(etapes_de(t, ...), segmentation, "positionnement")
  verifier("D table_construite() : table fabriquée conforme rendue telle quelle, plage_ages de l'étape, ddl_effectifs NULL", {
    r <- tc(tp)
    identical(r, list(table = tp, plage_ages = c(50, 52), ddl_effectifs = NULL))
  })
  verifier("D table_construite() : ddl_effectifs lu par propriété (table segments portant la colonne) : clés et ddl_effectifs ; sans la colonne, NULL", {
    sg <- data.frame(sexe = c("H", "F"), autre = 1:2, ddl_effectifs = c(3.5, 4), stringsAsFactors = FALSE)
    identical(tc(tp, segments = sg)$ddl_effectifs, sg[c("sexe", "ddl_effectifs")]) &&
      is.null(tc(tp, segments = sg[c("sexe", "autre")])$ddl_effectifs)
  })
  verifier("D table_construite() : position des colonnes non contrôlée ; sans segmentation, un seul segment", {
    t1 <- tp[c("qx", "etat", "age", "sexe", "qx_reference")]
    t0 <- tp[1:3, c("age", "qx", "etat")]
    identical(tc(t1)$table, t1) && identical(tc(t0, character(0))$table, t0)
  })
  verifier("D table_construite() : clés hors de l'ordre du champ segmentation, segments non contigus ou en double, âges décroissants, data.frame de classe dérivée -> erreur de programmation", {
    t2 <- tp; t2$region <- "N"
    nc <- tp[c(1:3, 4:6, 1:3), ]; rownames(nc) <- NULL
    dec <- tp; dec$age <- rep(52:50, 2L)
    classe <- tp; class(classe) <- c("tbl", "data.frame")
    erreur_programmation(tc(t2, c("region", "sexe"))) &&
      erreur_programmation(tc(nc)) && erreur_programmation(tc(dec)) && erreur_programmation(tc(classe)) &&
      erreur_programmation(tc(tp, plage = c(50, 53)))
  })
  verifier("D table_construite() : étape sans table construite -> erreur de programmation",
           erreur_programmation(tablexp:::table_construite(list(brut = list(resultat = list())), "sexe", "brut")))
})

# --- E. Liste blanche de controler_plage_ages() (module exposition) ----------------

verifier("E ETAPES_PLAGE_AGES : positionnement, validation, homogeneite (constante du module exposition ; ADR 0017, point 10.1)",
         identical(tablexp:::ETAPES_PLAGE_AGES, c("positionnement", "validation", "homogeneite")))
verifier("E controler_plage_ages() : refus plage_ages_invalide sous le module de chaque étape admise, message inchangé", {
  m <- paste("La plage d'âges (plage_ages) doit être un vecteur double sans attribut de deux valeurs",
             "entières finies, 0 <= min <= max (un integer est refusé).")
  all(vapply(tablexp:::ETAPES_PLAGE_AGES, function(e) {
    r <- refus_de(tablexp:::controler_plage_ages(c(20L, 90L), e))
    !is.null(r) && identical(r$module, e) && identical(r$motif, "plage_ages_invalide") &&
      identical(conditionMessage(r), m) && identical(r$details, list(plage_ages = c(20L, 90L)))
  }, NA)) && erreur_programmation(tablexp:::controler_plage_ages(c(20, 90), "lissage"))
})
