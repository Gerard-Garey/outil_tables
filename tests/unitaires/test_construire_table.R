# -----------------------------------------------------------------------------
#  tests/unitaires/test_construire_table.R : point d'entrée, plan, objet
#  résultat, fiche en mémoire et empreinte (issue #11 ; ADR 0005, points 3
#  et 6 ; ADR 0007, points 3.1, 3.4, 5, 6 et 9, annotation du 17/10/2026 ;
#  spécification d'actuary, A.3)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : codes de refus, structure de l'objet et de
#    la fiche, empreinte MD5 d'un petit data.frame dont le texte canonique a
#    été écrit par un autre outil (printf et md5sum de GNU coreutils, hors R) ;
#  - identité exacte : sur un portefeuille simulé (#10), exposition totale =
#    somme des durées dans la période, décès et rachats comptés = événements
#    simulés dans la période réduite du délai ;
#  - recoupement : somme des années d'âge recalculée ligne par ligne, les
#    anniversaires par as.Date() et les âges extrêmes de chaque ligne par
#    age_revolu() du module donnees (comparaison, pas recoupement), dont
#    l'exposition ne se sert pas.
# -----------------------------------------------------------------------------

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")

BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
plan_type <- function() list(
  transition = VD, periode = PER, delai_declaration = 0, segmentation = "sexe",
  etapes = list(donnees = list(exclusions = character(0)),
                exposition = list(convention = "plus_fine", par_annee = FALSE)))
modifier <- function(plan, ...) { m <- list(...); for (n in names(m)) plan[n] <- list(m[[n]]); plan }
refus <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)

# --- exports et signature -------------------------------------------------------

verifier(paste("NAMESPACE : exports = catalogue_roles, construire_table, ecrire_fiche, ecrire_plan, ecrire_rapport,",
               "lire_fiche, lire_plan, rejouer_fiche, simuler_portefeuille, table_reference, valider_plan (ADR 0007,",
               "point 3.4 ; #9, #10, #11, #50, #86, #87)"),
         identical(sort(getNamespaceExports("tablexp")),
                   c("catalogue_roles", "construire_table", "ecrire_fiche", "ecrire_plan", "ecrire_rapport", "lire_fiche",
                     "lire_plan", "rejouer_fiche", "simuler_portefeuille", "table_reference", "valider_plan")))
verifier("construire_table : signature (donnees, roles, plan, provenance = NULL)",
         identical(names(formals(construire_table)), c("donnees", "roles", "plan", "provenance")) &&
           is.null(formals(construire_table)$provenance))
verifier("valider_plan : signature (plan) ; plan valide rendu inchangé",
         identical(names(formals(valider_plan)), "plan") && identical(valider_plan(plan_type()), plan_type()))

# --- refus de valider_plan (valeur calculée à la main : codes) ----------------

# Noms incomplets (#60) : liste_noms_complets() (noms_complets() de outils.R) est
# le prédicat commun de liste_nommee() (plan, paramètres : doublons refusés) et
# du contrôle de `etapes` (doublons admis à ce stade, signalés ensuite par
# etape_en_double).
avec_noms <- function(x, noms) { names(x) <- noms; x }
etapes_deux <- function(noms)
  avec_noms(list(list(exclusions = character(0)), list(convention = "plus_fine", par_annee = FALSE)), noms)

cas_refus <- list(
  list("plan non liste", "plan_mal_forme", "a"),
  list("plan sans noms", "plan_mal_forme", list(1, 2)),
  list("plan vide", "plan_mal_forme", list()),
  list("champ inconnu (unite_observation, M48)", "champ_inconnu",
       modifier(plan_type(), unite_observation = "tete")),
  list("période absente", "champ_manquant", plan_type()[setdiff(names(plan_type()), "periode")]),
  list("segmentation absente (requise, M49)", "champ_manquant",
       plan_type()[setdiff(names(plan_type()), "segmentation")]),
  list("délai absent (zéro explicite requis)", "champ_manquant",
       plan_type()[setdiff(names(plan_type()), "delai_declaration")]),
  list("graine déclarée sans étape aléatoire", "graine_inattendue", modifier(plan_type(), graine = 1L)),
  list("etapes non nommée", "etapes_mal_formees", modifier(plan_type(), etapes = list(list()))),
  list("aucune étape", "aucune_etape", modifier(plan_type(), etapes = list())),
  list("étape inconnue", "etape_inconnue",
       modifier(plan_type(), etapes = c(plan_type()$etapes, list(lissage = list())))),
  list("étape en double", "etape_en_double",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           donnees = list(exclusions = character(0))))),
  list("etapes : un nom vide", "etapes_mal_formees",
       modifier(plan_type(), etapes = etapes_deux(c("donnees", "")))),
  list("etapes : un nom NA", "etapes_mal_formees",
       modifier(plan_type(), etapes = etapes_deux(c("donnees", NA)))),
  list("etapes : deux noms vides (incomplets avant doublons)", "etapes_mal_formees",
       modifier(plan_type(), etapes = etapes_deux(c("", "")))),
  list("etapes : deux noms identiques (doublons admis par le contrôle de forme)", "etape_en_double",
       modifier(plan_type(), etapes = etapes_deux(c("donnees", "donnees")))),
  list("plan : un nom vide", "plan_mal_forme", avec_noms(plan_type(), c("", names(plan_type())[-1L]))),
  list("plan : un nom NA", "plan_mal_forme", avec_noms(plan_type(), c(NA, names(plan_type())[-1L]))),
  list("plan : deux noms identiques", "plan_mal_forme",
       avec_noms(plan_type(), c(names(plan_type())[1L], names(plan_type())[1L], names(plan_type())[-(1:2)]))),
  list("paramètres d'une étape : deux noms identiques", "parametres_mal_formes",
       modifier(plan_type(), etapes = list(donnees = avec_noms(list(character(0), character(0)),
                                                               c("exclusions", "exclusions"))))),
  list("étapes hors ordre", "etapes_hors_ordre",
       modifier(plan_type(), etapes = rev(plan_type()$etapes))),
  list("exposition sans donnees", "etape_prealable_manquante",
       modifier(plan_type(), etapes = plan_type()$etapes["exposition"])),
  list("paramètres non liste", "parametres_mal_formes",
       modifier(plan_type(), etapes = list(donnees = "aucune"))),
  list("paramètre inconnu (segmentation dans exposition, M49)", "parametre_inconnu",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "plus_fine", par_annee = FALSE,
                                                             segmentation = "sexe")))),
  list("paramètre manquant (par_annee)", "parametre_manquant",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "plus_fine")))),
  list("paramètre manquant (exclusions)", "parametre_manquant",
       modifier(plan_type(), etapes = list(donnees = list()))),
  list("période vide (début > fin - délai)", "periode_vide", modifier(plan_type(), delai_declaration = 4000)),
  list("délai négatif", "delai_invalide", modifier(plan_type(), delai_declaration = -1)),
  list("délai non entier", "delai_invalide", modifier(plan_type(), delai_declaration = 1.5)),
  list("période mal formée", "periode_invalide", modifier(plan_type(), periode = list(debut = "2015-01-01", fin = PER$fin))),
  list("période : nom partiel debut_obs (aucun appariement partiel)", "periode_invalide",
       modifier(plan_type(), periode = list(debut_obs = PER$debut, fin = PER$fin))),
  list("période : champ en trop", "periode_invalide",
       modifier(plan_type(), periode = list(debut = PER$debut, fin = PER$fin, origine = PER$debut))),
  list("transition : nom partiel departement (aucun appariement partiel)", "transition_inconnue",
       modifier(plan_type(), transition = list(departement = "vivant", arrivee = "deces"))),
  list("transition : champ en trop", "transition_inconnue",
       modifier(plan_type(), transition = list(depart = "vivant", arrivee = "deces", etat = "x"))),
  list("transition inconnue", "transition_inconnue",
       modifier(plan_type(), transition = list(depart = "vivant", arrivee = "rachat"))),
  list("segmentation hors catalogue", "segmentation_invalide", modifier(plan_type(), segmentation = "region")),
  list("code d'exclusion inconnu", "exclusion_inconnue",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = "motif_imaginaire"),
                                           exposition = list(convention = "plus_fine", par_annee = FALSE)))),
  list("forçage vers age_fourni", "convention_non_mise_en_oeuvre",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "age_fourni", par_annee = FALSE)))),
  list("convention inconnue", "convention_invalide",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "deduite", par_annee = FALSE)))),
  list("par_annee non logique", "par_annee_invalide",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "plus_fine", par_annee = 1))))
)
for (k in cas_refus)
  verifier(sprintf("valider_plan : %s -> refus %s", k[[1]], k[[2]]), leve_refus(valider_plan(k[[3]]), k[[2]]))

verifier("valider_plan : refus structurels portés par le module construire_table, valeurs par le module qui les contrôle (donnees, exposition)", {
  r1 <- refus(valider_plan(modifier(plan_type(), etapes = list(donnees = list()))))
  r2 <- refus(valider_plan(modifier(plan_type(), delai_declaration = -1)))
  r3 <- refus(valider_plan(modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                                            exposition = list(convention = "age_fourni", par_annee = FALSE)))))
  identical(c(r1$module, r2$module, r3$module), c("construire_table", "donnees", "exposition"))
})
verifier("valider_plan : le refus de paramètre manquant nomme l'étape et le paramètre", {
  r <- refus(valider_plan(modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                                           exposition = list(convention = "plus_fine")))))
  identical(r$details, list(etape = "exposition", parametre = "par_annee")) &&
    grepl("par_annee", conditionMessage(r), fixed = TRUE) && grepl("exposition", conditionMessage(r), fixed = TRUE)
})
verifier("valider_plan : plan réduit à l'étape donnees admis",
         identical(valider_plan(modifier(plan_type(), etapes = plan_type()$etapes["donnees"])),
                   modifier(plan_type(), etapes = plan_type()$etapes["donnees"])))

# --- forme exacte des valeurs du plan (#23 ; valeur calculée à la main : codes)
#
# Chaque paramètre d'étape (donnees, exposition, brut) et chaque champ de
# premier niveau, valeur valide munie de noms, d'une dimension ou d'un
# attribut quelconque, est refusé par valider_plan() sous le motif du
# paramètre, porté par le module qui le contrôle ; les fonctions de contrôle
# appelées par les fonctions d'étape refusent de même.

plan_complet <- function() modifier(plan_type(), etapes = c(plan_type()$etapes,
  list(brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95))))
temoins_forme <- function(v) list(
  "nommé" = { x <- v; names(x) <- rep("a", length(x)); x },
  "matrix" = matrix(v),
  "structure(foo)" = structure(v, foo = "x"))
# chemin dans le plan, valeur valide, motif, module.
VALEURS_PLAN <- list(
  list(c("delai_declaration"), 0, "delai_invalide", "donnees"),
  list(c("delai_declaration"), 90L, "delai_invalide", "donnees"),
  list(c("segmentation"), "sexe", "segmentation_invalide", "donnees"),
  list(c("segmentation"), character(0), "segmentation_invalide", "donnees"),
  list(c("transition", "depart"), "vivant", "transition_inconnue", "donnees"),
  list(c("transition", "arrivee"), "deces", "transition_inconnue", "donnees"),
  list(c("periode", "debut"), PER$debut, "periode_invalide", "donnees"),
  list(c("periode", "fin"), PER$fin, "periode_invalide", "donnees"),
  list(c("etapes", "donnees", "exclusions"), character(0), "exclusions_invalides", "donnees"),
  list(c("etapes", "donnees", "exclusions"), "id_tete_manquant", "exclusions_invalides", "donnees"),
  list(c("etapes", "exposition", "convention"), "plus_fine", "convention_invalide", "exposition"),
  list(c("etapes", "exposition", "par_annee"), FALSE, "par_annee_invalide", "exposition"),
  list(c("etapes", "brut", "estimateurs"), c("poisson", "binomial"), "estimateurs_invalides", "brut"),
  list(c("etapes", "brut", "estimateurs"), "poisson", "estimateurs_invalides", "brut"),
  list(c("etapes", "brut", "niveau"), 0.95, "niveau_invalide", "brut"))
poser <- function(plan, chemin, v) { plan[[chemin]] <- v; plan }
verifier("forme exacte : plan complet de référence admis, et chaque valeur valide nue admise", {
  ok <- vapply(VALEURS_PLAN, function(k) isTRUE(tryCatch({ valider_plan(poser(plan_complet(), k[[1]], k[[2]])); TRUE },
                                                         refus_tablexp = function(r) FALSE)), logical(1L))
  if (all(ok)) TRUE else paste("refusé :", vapply(VALEURS_PLAN[!ok], function(k) paste(k[[1]], collapse = "$"), ""))
})
for (k in VALEURS_PLAN) for (t in names(temoins_forme(k[[2]]))) {
  verifier(sprintf("forme exacte : %s = %s %s -> refus %s (module %s)", paste(k[[1]], collapse = "$"),
                   paste(deparse(k[[2]]), collapse = ""), t, k[[3]], k[[4]]), {
    r <- tryCatch({ valider_plan(poser(plan_complet(), k[[1]], temoins_forme(k[[2]])[[t]])); NULL },
                  refus_tablexp = function(r) r, error = function(e) e)
    if (is.null(r)) "aucun refus levé"
    else if (!inherits(r, "refus_tablexp")) paste("erreur qui n'est pas un refus_tablexp :", conditionMessage(r))
    else if (!identical(r$motif, k[[3]]) || !identical(r$module, k[[4]]))
      sprintf("refus %s du module %s", r$motif, r$module)
    else TRUE
  })
}
# Listes du plan : seul l'attribut names est admis.
LISTES_PLAN <- list(
  list("transition à attribut quelconque", function(p) { attr(p$transition, "foo") <- "x"; p }, "transition_inconnue"),
  list("transition en data.frame", function(p) { p$transition <- data.frame(depart = "vivant", arrivee = "deces"); p },
       "transition_inconnue"),
  list("transition à classe", function(p) { class(p$transition) <- "x"; p }, "transition_inconnue"),
  list("periode à attribut quelconque", function(p) { attr(p$periode, "foo") <- "x"; p }, "periode_invalide"),
  list("periode en data.frame", function(p) { p$periode <- data.frame(debut = PER$debut, fin = PER$fin); p },
       "periode_invalide"),
  list("etapes à attribut quelconque", function(p) { attr(p$etapes, "foo") <- "x"; p }, "etapes_mal_formees"),
  list("etapes à classe", function(p) { class(p$etapes) <- "x"; p }, "etapes_mal_formees"),
  list("paramètres de brut à attribut quelconque", function(p) { attr(p$etapes$brut, "foo") <- "x"; p },
       "parametres_mal_formes"),
  list("paramètres de donnees à classe", function(p) { class(p$etapes$donnees) <- "x"; p }, "parametres_mal_formes"),
  list("plan à attribut quelconque", function(p) { attr(p, "foo") <- "x"; p }, "plan_mal_forme"),
  list("plan à classe", function(p) { class(p) <- "x"; p }, "plan_mal_forme"))
for (k in LISTES_PLAN)
  verifier(sprintf("forme exacte : %s -> refus %s", k[[1]], k[[3]]), leve_refus(valider_plan(k[[2]](plan_complet())), k[[3]]))
# Élément en plus nommé NA (#23) : sort() retire un nom NA, la longueur de la
# liste est donc contrôlée à part (liste_aux_noms()).
LISTES_PLAN_NOM_NA <- list(
  list("transition avec un élément de plus nommé NA", function(p) { p$transition <- c(p$transition, list("x"))
                                                                    names(p$transition)[3L] <- NA; p },
       "transition_inconnue"),
  list("periode avec un élément de plus nommé NA", function(p) { p$periode <- c(p$periode, list("x"))
                                                                 names(p$periode)[3L] <- NA; p },
       "periode_invalide"))
for (k in LISTES_PLAN_NOM_NA)
  verifier(sprintf("forme exacte : %s -> refus %s", k[[1]], k[[3]]), leve_refus(valider_plan(k[[2]](plan_complet())), k[[3]]))
verifier("forme exacte : date de stockage integer admise (inchangé ; classe Date seule)",
         isTRUE(tryCatch({ valider_plan(poser(plan_complet(), c("periode", "debut"), structure(16436L, class = "Date")))
                           TRUE }, refus_tablexp = function(r) FALSE)))
verifier("forme exacte : transition$depart en matrix -> refus, et non plus erreur R « dims do not match »",
         leve_refus(valider_plan(poser(plan_complet(), c("transition", "depart"), matrix("vivant"))),
                    "transition_inconnue"))
# Les contrôles appelés par les fonctions d'étape (preparer_donnees,
# calculer_exposition, calculer_taux_bruts) refusent sous les mêmes motifs.
verifier("forme exacte : contrôles des fonctions d'étape, mêmes motifs (nommé)", {
  cpd <- tablexp:::controler_parametres_donnees
  cpe <- tablexp:::controler_parametres_exposition
  cpb <- tablexp:::controler_parametres_brut
  n <- function(v) { names(v) <- rep("a", length(v)); v }
  r <- list(leve_refus(cpd(VD, PER, n(0), "sexe", character(0)), "delai_invalide"),
            leve_refus(cpd(VD, PER, 0, n("sexe"), character(0)), "segmentation_invalide"),
            leve_refus(cpd(VD, PER, 0, "sexe", n("id_tete_manquant")), "exclusions_invalides"),
            leve_refus(cpd(list(depart = n("vivant"), arrivee = "deces"), PER, 0, "sexe", character(0)),
                       "transition_inconnue"),
            leve_refus(cpd(VD, list(debut = n(PER$debut), fin = PER$fin), 0, "sexe", character(0)),
                       "periode_invalide"),
            leve_refus(cpe(n("plus_fine"), FALSE), "convention_invalide"),
            leve_refus(cpe("plus_fine", n(FALSE)), "par_annee_invalide"),
            leve_refus(cpb(n("poisson"), 0.95, FALSE), "estimateurs_invalides"),
            leve_refus(cpb("poisson", n(0.95), FALSE), "niveau_invalide"))
  ok <- vapply(r, isTRUE, logical(1L))
  if (all(ok)) TRUE else paste(unlist(r[!ok]), collapse = " | ")
})
# Priorité de la forme sur la valeur (paramètres à liste fermée) : une valeur
# inconnue et nommée est refusée sous le motif de forme, avant le contrôle de
# la liste fermée.
PRIORITE_FORME <- list(
  list(c("etapes", "donnees", "exclusions"), c(a = "zzz"), "exclusions_invalides", "exclusion_inconnue"),
  list(c("etapes", "brut", "estimateurs"), c(a = "zzz"), "estimateurs_invalides", "estimateur_inconnu"),
  list(c("etapes", "exposition", "convention"), c(a = "age_fourni"), "convention_invalide",
       "convention_non_mise_en_oeuvre"))
for (k in PRIORITE_FORME)
  verifier(sprintf("forme exacte avant la valeur : %s = %s -> refus %s, et non %s", paste(k[[1]], collapse = "$"),
                   paste(deparse(k[[2]]), collapse = ""), k[[3]], k[[4]]),
           leve_refus(valider_plan(poser(plan_complet(), k[[1]], k[[2]])), k[[3]]))
# Colonne `type` de PARAMETRES_ETAPES confrontée aux contrôles : pour chaque
# paramètre, une valeur valide du type déclaré, sans attribut, est admise ;
# une valeur de tout autre type de base est refusée sous le motif du
# paramètre.
VALEUR_TYPE <- list(donnees.exclusions = list(character(0), "exclusions_invalides"),
                    exposition.convention = list("plus_fine", "convention_invalide"),
                    exposition.par_annee = list(FALSE, "par_annee_invalide"),
                    brut.estimateurs = list("poisson", "estimateurs_invalides"),
                    brut.niveau = list(0.95, "niveau_invalide"))
AUTRES_TYPES <- list(logical = TRUE, integer = 1L, double = 0.5, complex = 0.5 + 0i, character = "poisson",
                     list = list("poisson"), raw = as.raw(1L))
pe_cat <- tablexp:::PARAMETRES_ETAPES
cles_cat <- unlist(lapply(names(pe_cat), function(e) paste(e, pe_cat[[e]]$parametre, sep = ".")))
types_cat <- unlist(lapply(pe_cat, function(t) t$type), use.names = FALSE)
verifier("PARAMETRES_ETAPES : chaque paramètre a un témoin de type (liste VALEUR_TYPE à compléter sinon)",
         if (setequal(cles_cat, names(VALEUR_TYPE))) TRUE
         else sprintf("catalogue %s ; témoins %s", paste(cles_cat, collapse = ", "),
                      paste(names(VALEUR_TYPE), collapse = ", ")))
for (i in seq_along(cles_cat)) {
  cle <- cles_cat[i]; ty <- types_cat[i]; k <- VALEUR_TYPE[[cle]]
  if (is.null(k)) next
  chemin <- c("etapes", strsplit(cle, ".", fixed = TRUE)[[1L]])
  verifier(sprintf("PARAMETRES_ETAPES : %s de type %s, valeur nue du type déclaré admise", cle, ty),
           identical(typeof(k[[1]]), ty) &&
             isTRUE(tryCatch({ valider_plan(poser(plan_complet(), chemin, k[[1]])); TRUE },
                             refus_tablexp = function(r) FALSE)))
  for (t in setdiff(names(AUTRES_TYPES), ty))
    verifier(sprintf("PARAMETRES_ETAPES : %s de type %s, valeur de type %s -> refus %s", cle, ty, t, k[[2]]),
             leve_refus(valider_plan(poser(plan_complet(), chemin, AUTRES_TYPES[[t]])), k[[2]]))
}

# --- construire_table : refus et validation avant exécution ------------------

verifier("construire_table : plan validé avant toute exécution (données invalides et plan invalide : refus du plan)", {
  d <- BASE; d$naissance[1] <- NA
  leve_refus(construire_table(d, ROLES, modifier(plan_type(), delai_declaration = -1)), "delai_invalide")
})
verifier("construire_table : rôle requis absent (date_sortie) -> refus aucune_recette, message qui nomme le rôle et la recette", {
  r <- refus(construire_table(BASE, ROLES[names(ROLES) != "date_sortie"], plan_type()))
  !is.null(r) && identical(r$motif, "aucune_recette") && identical(r$details$role, "date_sortie") &&
    grepl("date_sortie", conditionMessage(r), fixed = TRUE) &&
    grepl("lignes_dates_completes", conditionMessage(r), fixed = TRUE)
})
verifier("construire_table : rôle employé par la segmentation absent -> refus role_requis_manquant",
         leve_refus(construire_table(BASE, ROLES[names(ROLES) != "sexe"], plan_type()), "role_requis_manquant"))
# Forme exacte des rôles (#62) par le point d'entrée : un attribut, une
# classe ou un dim est refusé sous roles_mal_formes avant toute exécution
# (construire_table() appelle controler_roles() avant preparer_donnees()).
ROLES_FORME <- list(
  attribut = structure(ROLES, foo = 1), classe = noquote(ROLES),
  dim = array(unname(ROLES), dim = length(ROLES), dimnames = list(names(ROLES))))
for (k in names(ROLES_FORME))
  verifier(sprintf("construire_table : rôles avec %s -> refus roles_mal_formes", k),
           leve_refus(construire_table(BASE, ROLES_FORME[[k]], plan_type()),
                      "roles_mal_formes"))
verifier("construire_table : provenance mal formée -> refus provenance_invalide",
         leve_refus(construire_table(BASE, ROLES, plan_type(), provenance = list(fichier = "a.csv")),
                    "provenance_invalide"))
# Forme exacte de la provenance (#21, forme_exacte()) : liste nue à trois chaînes
# nommées de longueur 1, sans attribut (seul names sur la liste). Chaque cas de
# refus a son témoin, sous le motif existant provenance_invalide (clés de la liste
# en ASCII : une clé accentuée émet un avertissement de parse() sous LC_ALL=C).
PROV <- list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
cp <- tablexp:::controler_provenance
verifier("provenance : liste nue valide admise, quel que soit l'ordre des noms, et NULL admis",
         isTRUE(cp(PROV)) && isTRUE(cp(PROV[c("algorithme", "fichier", "empreinte")])) && isTRUE(cp(NULL)))
PROV_REFUSEES <- list(
  "data.frame (trois colonnes, une ligne)" = as.data.frame(PROV, stringsAsFactors = FALSE),
  "objet de classe (liste de classe prov)" = structure(PROV, class = "prov"),
  "liste a attribut autre que names" = structure(PROV, source = "a"),
  "nom en trop (quatre elements)" = c(PROV, extra = "x"),
  "nom manquant (deux elements)" = PROV[c("fichier", "empreinte")],
  "nom manquant remplace par un doublon (trois elements)" = list(fichier = "a", fichier = "b", empreinte = "c"),
  "nom inconnu a la place d'un nom attendu" = setNames(PROV, c("fichier", "empreinte", "hash")),
  "nom NA en plus des trois noms" = setNames(c(PROV, "x"), c(names(PROV), NA)),
  "liste sans noms" = unname(PROV),
  "chaine de longueur 2" = modifyList(PROV, list(fichier = c("a.csv", "b.csv"))),
  "chaine de longueur 0" = modifyList(PROV, list(fichier = character(0))),
  "chaine a attribut names" = modifyList(PROV, list(fichier = c(a = "base.csv"))),
  "chaine a attribut class" = modifyList(PROV, list(algorithme = structure("md5", class = "algo"))),
  "chaine a attribut dim" = modifyList(PROV, list(empreinte = matrix("0123"))),
  "chaine NA" = modifyList(PROV, list(fichier = NA_character_)),
  "chaine vide" = modifyList(PROV, list(fichier = "")))
for (k in names(PROV_REFUSEES))
  verifier(sprintf("provenance : %s -> refus provenance_invalide", k),
           leve_refus(cp(PROV_REFUSEES[[k]]), "provenance_invalide"))
verifier("provenance : data.frame refusé aussi par construire_table, avant tout calcul",
         leve_refus(construire_table(BASE, ROLES, plan_type(), provenance = PROV_REFUSEES[[1L]]),
                    "provenance_invalide"))
verifier("construire_table : ligne invalide sans exclusion déclarée -> refus ligne_invalide (étape donnees)", {
  d <- BASE; d$naissance[2] <- NA
  leve_refus(construire_table(d, ROLES, plan_type()), "ligne_invalide")
})

verifier("construire_table : base sans ligne -> refus base_vide (empreinte calculée sans erreur avant l'étape donnees)",
         leve_refus(construire_table(BASE[0, ], ROLES, plan_type()), "base_vide"))
verifier("ETAPES et construire_table() concordent : une branche du switch par étape, appelant la fonction d'étape déclarée (valeur calculée à la main)", {
  corps <- body(construire_table)
  appels <- list()
  chercher <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1L]], as.name("switch"))) appels[[length(appels) + 1L]] <<- e
      for (k in as.list(e)[-1L]) if (!missing(k)) chercher(k)
    }
  }
  chercher(corps)
  if (length(appels) != 1L) return("un seul switch attendu dans construire_table()")
  br <- as.list(appels[[1L]])[-(1:2)]
  nommees <- br[nzchar(names(br))]
  fonctions <- vapply(nommees, function(b) as.character(b[[1L]]), character(1L))
  et <- tablexp:::ETAPES
  if (!identical(names(nommees), et$etape)) sprintf("branches %s", paste(names(nommees), collapse = ", "))
  else if (!identical(unname(fonctions), et$fonction)) sprintf("fonctions %s", paste(fonctions, collapse = ", "))
  else if (!identical(names(tablexp:::PARAMETRES_ETAPES), et$etape)) "PARAMETRES_ETAPES"
  else TRUE
})

# --- objet résultat et fiche --------------------------------------------------

x <- construire_table(BASE, ROLES, plan_type())
verifier("objet : classe construction, champs table, etapes, avertissements, fiche, execution (ADR 0007, point 6.1)",
         inherits(x, "construction") &&
           identical(names(x), c("table", "etapes", "avertissements", "fiche", "execution")) &&
           is.null(x$table) && identical(names(x$etapes), c("donnees", "exposition")) &&
           all(vapply(x$etapes, function(e) identical(names(e), c("parametres", "resultat", "diagnostics")),
                      logical(1L))))
verifier("objet : avertissements data.frame (etape, code, message, nombre)",
         is.data.frame(x$avertissements) &&
           identical(names(x$avertissements), c("etape", "code", "message", "nombre")))
verifier("objet : exposition par (sexe, âge), colonnes de la spécification (A.3)",
         identical(names(x$etapes$exposition$resultat$exposition),
                   c("sexe", "age", "exposition_jours", "exposition_centrale", "complement_initial_cellule",
                     "complement_initial_fenetre", "evenements", "sorties_concurrentes")))
CHAMPS_FICHE <- c("version_fiche", "versions", "plateforme", "roles", "provenance", "provenance_mention",
                  "empreintes", "plan", "transition_code", "fin_effective", "unite_observation", "recette",
                  "convention", "unite_exposition", "par_annee", "exclusions", "nombre_lignes_exclues",
                  "avertissement_unite", "hypothese_censure", "graine", "graine_mention", "generateur",
                  "avertissements")  # ordre de lecture (#64)
f <- x$fiche
verifier("fiche : champs de l'ADR 0007 (point 6.2) et de la spécification (A.3)", identical(names(f), CHAMPS_FICHE))
verifier("fiche : plan canonique (plan reçu, déjà canonique, inchangé), rôles, recette, convention retenue non forcée, unité tête",
         identical(f$plan, plan_type()) && identical(f$roles, ROLES) &&
           identical(f$recette, "lignes_dates_completes") &&
           identical(f$convention, list(demandee = "plus_fine", native = "dernier_anniversaire",
                                        retenue = "dernier_anniversaire", forcee = FALSE)) &&
           identical(f$unite_observation, "tete") && is.na(f$avertissement_unite) &&
           identical(f$transition_code, "vivant_decede") && identical(f$fin_effective, D("2024-12-31")))
verifier("fiche : graine absente (NA, « aucune étape aléatoire »), générateur consigné, version 4 (#57, #77)",
         identical(f$graine, NA_integer_) && identical(f$graine_mention, "aucune étape aléatoire") &&
           identical(f$generateur, tablexp:::GENERATEUR) && identical(f$version_fiche, 4L))
verifier("fiche : provenance NULL, mention « aucun fichier », empreinte du fichier NA",
         "provenance" %in% names(f) && is.null(f$provenance) && startsWith(f$provenance_mention, "aucun fichier") &&
           is.na(f$empreintes$fichier))
verifier(paste("fiche : empreintes (algorithme md5 nommé ; données, résultat et code en 32 chiffres hexadécimaux ;",
               "representation_resultat avant resultat ; statut des deux empreintes, code calculé) (#67)"),
         identical(names(f$empreintes), c("algorithme", "representation", "donnees", "fichier", "algorithme_fichier",
                                          "representation_resultat", "resultat", "code", "statut_resultat_code")) &&
           identical(f$empreintes$algorithme, "md5") && grepl("^[0-9a-f]{32}$", f$empreintes$donnees) &&
           identical(f$empreintes$representation_resultat, "tablexp resultat 1") &&
           grepl("^[0-9a-f]{32}$", f$empreintes$resultat) && grepl("^[0-9a-f]{32}$", f$empreintes$code) &&
           identical(f$empreintes$statut_resultat_code, paste(
             "résultat : témoin d'identité sur la même machine seulement (E3.6, E4.6), jamais critère de rejeu chez un",
             "tiers ; code : MD5 du relevé des MD5 des fichiers R/*.R, fait à l'installation (recalcul depuis la racine",
             "des sources : LC_ALL=C md5sum R/*.R | md5sum) ; identifie le texte source, non le comportement numérique,",
             "qui dépend aussi de R, du BLAS et de la libm")) &&
           identical(Encoding(f$empreintes$statut_resultat_code), "UTF-8"))
verifier("fiche : versions (tablexp, R) et plateforme (sysname, release, machine, blas, lapack, bibliotheque_c) (#77)",
         identical(names(f$versions), c("tablexp", "R")) && identical(f$versions$R, R.version.string) &&
           identical(names(f$plateforme), c("sysname", "release", "machine", "blas", "lapack", "bibliotheque_c")))
# Bibliothèque C (#77) : source lue dans src/main/platform.c de R 4.3.3
# (do_eSoftVersion(), l. 3592-3607, quatre formes dans cet ordre : « GNU
# libiconv X.Y » ou « Apple or GNU libiconv X.Y » (macOS) si R est lié à GNU
# libiconv, « win_iconv » sous Windows, « glibc X.Y » sous glibc sans
# libiconv, « unknown » sinon).
verifier("fiche : plateforme$bibliotheque_c, un texte unique non vide, égal à extSoftVersion()[[\"iconv\"]] quand elle est renseignée",
         is.character(f$plateforme$bibliotheque_c) && length(f$plateforme$bibliotheque_c) == 1L &&
           !is.na(f$plateforme$bibliotheque_c) && nzchar(f$plateforme$bibliotheque_c) &&
           is.null(attributes(f$plateforme$bibliotheque_c)) &&
           identical(f$plateforme$bibliotheque_c,
                     if (nzchar(extSoftVersion()[["iconv"]])) extSoftVersion()[["iconv"]]
                     else tablexp:::MENTION_BIBLIOTHEQUE_C_NON_RELEVEE))
verifier("bibliotheque_c() : valeur d'iconv rendue telle quelle (glibc, win_iconv, libiconv, unknown) ; vide, blanche, NA, absente ou non texte : « non relevée », UTF-8",
         identical(tablexp:::bibliotheque_c(c(zlib = "1.3", iconv = "glibc 2.35")), "glibc 2.35") &&
           identical(tablexp:::bibliotheque_c(c(iconv = "win_iconv")), "win_iconv") &&
           identical(tablexp:::bibliotheque_c(c(iconv = "GNU libiconv 1.16")), "GNU libiconv 1.16") &&
           identical(tablexp:::bibliotheque_c(c(iconv = "unknown")), "unknown") &&
           all(vapply(list(c(iconv = ""), c(iconv = "  "), c(iconv = NA_character_), c(zlib = "1.3"), character(0), NULL,
                           c(iconv = 1L), c("glibc 2.35")),
                      function(v) identical(tablexp:::bibliotheque_c(v), "non relevée") &&
                        identical(Encoding(tablexp:::bibliotheque_c(v)), "UTF-8"), logical(1L))) &&
           identical(tablexp:::MENTION_BIBLIOTHEQUE_C_NON_RELEVEE, "non relevée"))
verifier("fiche : aucun horodatage (aucun POSIXct ni POSIXlt dans la fiche)", {
  classes <- unlist(rapply(f, function(v) class(v)[1L], how = "unlist"))
  !any(classes %in% c("POSIXct", "POSIXlt"))
})
verifier("execution : horodatage, durée, session abrégée",
         inherits(x$execution$debut, "POSIXct") && is.numeric(x$execution$duree_secondes) &&
           identical(x$execution$session$R, R.version.string))
xp <- construire_table(BASE, ROLES, plan_type(),
                       provenance = list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef",
                                         algorithme = "md5"))
verifier("fiche : provenance reçue consignée (fichier, empreinte, algorithme)",
         identical(xp$fiche$provenance$fichier, "base.csv") &&
           identical(xp$fiche$empreintes$fichier, "0123456789abcdef0123456789abcdef") &&
           identical(xp$fiche$empreintes$algorithme_fichier, "md5"))
# nombre_lignes_exclues (#57) : lignes distinctes exclues, à côté du décompte
# par motif. Valeurs calculées à la main : C5 porte deux motifs déclarés
# (cause « Rachat » hors modalités, sortie avant l'entrée), C6 un seul (sortie
# avant l'entrée) ; décompte par motif 1 et 2 (somme 3), lignes distinctes 2.
local({
  BASE_X <- rbind(BASE, data.frame(
    contrat = c("C5", "C6"), tete = c("T5", "T6"), sexe = c("H", "F"),
    naissance = D(c("1950-01-01", "1950-01-01")), entree = D(c("2018-01-01", "2018-01-01")),
    sortie = D(c("2017-01-01", "2017-06-01")), cause = c("Rachat", "deces"), stringsAsFactors = FALSE))
  motifs <- c("cause_hors_modalites", "sortie_avant_entree")
  plan_x <- modifier(plan_type(), etapes = list(donnees = list(exclusions = motifs),
                                                 exposition = list(convention = "plus_fine", par_annee = FALSE)))
  y <- construire_table(BASE_X, ROLES, plan_x)
  fx <- y$fiche
  verifier("#57 : deux motifs sur une ligne, un sur l'autre -> exclusions$nombre c(1, 2) (somme 3), nombre_lignes_exclues 2L",
           identical(fx$exclusions$motif, motifs) && identical(fx$exclusions$nombre, c(1L, 2L)) &&
             identical(fx$nombre_lignes_exclues, 2L) && sum(fx$exclusions$nombre) != fx$nombre_lignes_exclues)
  verifier("#57 : identité exacte lignes reçues = lignes conservées + nombre_lignes_exclues (6 = 4 + 2)",
           nrow(BASE_X) == 6L &&
             nrow(y$etapes$donnees$resultat$lignes_conservees) + fx$nombre_lignes_exclues == nrow(BASE_X) &&
             identical(fx$nombre_lignes_exclues, y$etapes$donnees$resultat$nombre_lignes_exclues))
  verifier("#57 : nombre_lignes_exclues = lignes distinctes de lignes_exclues (union des numéros), integer de longueur 1",
           identical(fx$nombre_lignes_exclues,
                     length(unique(unlist(y$etapes$donnees$resultat$lignes_exclues, use.names = FALSE)))) &&
             is.integer(fx$nombre_lignes_exclues) && length(fx$nombre_lignes_exclues) == 1L)
  verifier("#57 : aller-retour lire_fiche(ecrire_fiche(fiche)) identical(), champ écrit en integer de longueur 1",
           identical(lire_fiche(ecrire_fiche(fx)), fx) &&
             any(grepl("^nombre_lignes_exclues\tinteger\t1\t2$",
                       strsplit(rawToChar(ecrire_fiche(fx)), "\n", fixed = TRUE)[[1L]])))
  z <- construire_table(BASE, ROLES, plan_x)$fiche
  verifier("#57 : exclusions déclarées sans ligne porteuse -> 0L et décomptes nuls c(0, 0) ; plan sans exclusion -> 0L, aucun décompte",
           identical(z$nombre_lignes_exclues, 0L) && identical(z$exclusions$nombre, c(0L, 0L)) &&
             identical(f$nombre_lignes_exclues, 0L) && identical(f$exclusions$nombre, integer(0)))
})
verifier("unité contrat sans id_tete : avertissement inscrit dans l'objet et dans la fiche", {
  y <- construire_table(BASE, ROLES[names(ROLES) != "id_tete"], plan_type())
  identical(y$fiche$unite_observation, "contrat") && !is.na(y$fiche$avertissement_unite) &&
    "unite_contrat" %in% y$fiche$avertissements$code
})
verifier("avertissements$nombre en integer dans l'objet et dans la fiche, avec avertissements (unité contrat) et sans aucun (BASE[1:3, ]) (#24)", {
  y <- construire_table(BASE, ROLES[names(ROLES) != "id_tete"], plan_type())
  z <- construire_table(BASE[1:3, ], ROLES, plan_type())
  nrow(y$avertissements) > 0L && identical(typeof(y$avertissements$nombre), "integer") &&
    identical(typeof(y$fiche$avertissements$nombre), "integer") &&
    nrow(z$avertissements) == 0L && nrow(z$fiche$avertissements) == 0L &&
    identical(typeof(z$avertissements$nombre), "integer") &&
    identical(typeof(z$fiche$avertissements$nombre), "integer")
})
verifier("fiche : fin effective lue dans les paramètres effectifs de l'étape donnees",
         identical(x$etapes$donnees$parametres$fin_effective, D("2024-12-31")) &&
           identical(x$fiche$fin_effective, x$etapes$donnees$parametres$fin_effective))
verifier("plan réduit à l'étape donnees : une seule étape exécutée, fiche sans convention", {
  y <- construire_table(BASE, ROLES, modifier(plan_type(), etapes = plan_type()$etapes["donnees"]))
  identical(names(y$etapes), "donnees") && is.null(y$fiche$convention) && is.na(y$fiche$unite_exposition)
})
verifier("construire_table ne touche pas .Random.seed de l'appelant (absent : reste absent ; présent : inchangé)", {
  env <- globalenv()
  avait <- exists(".Random.seed", envir = env, inherits = FALSE)
  sauve <- if (avait) env[[".Random.seed"]] else NULL
  if (avait) rm(".Random.seed", envir = env)
  invisible(construire_table(BASE, ROLES, plan_type()))
  absent <- !exists(".Random.seed", envir = env, inherits = FALSE)
  set.seed(5L); avant <- env[[".Random.seed"]]
  invisible(construire_table(BASE, ROLES, plan_type()))
  inchange <- identical(env[[".Random.seed"]], avant)
  if (avait) assign(".Random.seed", sauve, envir = env) else rm(".Random.seed", envir = env)
  absent && inchange
})

# --- empreinte (valeur calculée à la main, autre outil) ----------------------

# Texte canonique attendu (ADR 0007, point 9.3 ; ADR 0011, point 2), écrit par
# printf et haché par md5sum (GNU coreutils), hors de R ; octets des doubles de
# struct.pack('<d', v).hex() (Python 3.13) :
#   tablexp canonique 2<TAB>3<TAB>8
#   #cause_sortie<TAB>character / "deces" / "en_cours" / NA
#   #date_entree<TAB>Date / 2020-03-01 / 2016-05-20 / 2024-02-29
#   #date_naissance<TAB>Date / 1960-07-01 / 1945-10-15 / 1964-02-29
#   #date_sortie<TAB>Date / NA / 2019-02-10 / 2024-12-31
#   #id_contrat<TAB>double / 000000000000f03f / 0000000000000040 / 000000b08ef01b42
#   #id_tete<TAB>integer / 1 / NA / 3
#   #segment<TAB>character / "a\"b" / "c\\d\te" / "été" (UTF-8)
#   #sexe<TAB>factor / "H" / "F" / "H"
# (381 octets, LF, md5 2264ac10aa5504caec70eb1f97b2fb76 ; avant #35, doubles
# en %.17g « 1 / 2 / 30000000000 » : 346 octets, md5
# 80e1ed7c22b2c1b6086260ce4490d64c).
EMP <- data.frame(c = c("deces", "en_cours", NA), e = D(c("2020-03-01", "2016-05-20", "2024-02-29")),
                  n = D(c("1960-07-01", "1945-10-15", "1964-02-29")), s = D(c(NA, "2019-02-10", "2024-12-31")),
                  ic = c(1, 2, 3e10), it = c(1L, NA, 3L), sg = c("a\"b", "c\\d\te", "été"),
                  sx = factor(c("H", "F", "H")), autre = 1:3, stringsAsFactors = FALSE)
Encoding(EMP$sg) <- "UTF-8"
ROLES_EMP <- c(sexe = "sx", segment = "sg", id_tete = "it", id_contrat = "ic", date_sortie = "s",
               date_naissance = "n", date_entree = "e", cause_sortie = "c")
empreinte_donnees <- tablexp:::empreinte_donnees
verifier("empreinte (valeur calculée à la main par printf et md5sum, hors R) : 2264ac10aa5504caec70eb1f97b2fb76",
         identical(empreinte_donnees(EMP, ROLES_EMP), "2264ac10aa5504caec70eb1f97b2fb76"))
verifier("empreinte : texte canonique de 381 octets, sans CR", {
  fch <- tempfile()
  tablexp:::texte_canonique(EMP, ROLES_EMP, fch)
  o <- readBin(fch, "raw", 1000L)
  unlink(fch)
  length(o) == 381L && !any(o == as.raw(0x0d))
})
verifier("empreinte : indépendante du nom des colonnes et des colonnes non déclarées, sensible à l'ordre des lignes et aux valeurs", {
  e2 <- EMP; names(e2)[names(e2) == "ic"] <- "contrat"; r2 <- ROLES_EMP; r2[["id_contrat"]] <- "contrat"
  e2$autre <- 7:9
  e3 <- EMP[3:1, ]; e4 <- EMP; e4$ic[1] <- 1 + 2^-40
  identical(empreinte_donnees(e2, r2), "2264ac10aa5504caec70eb1f97b2fb76") &&
    !identical(empreinte_donnees(e3, ROLES_EMP), "2264ac10aa5504caec70eb1f97b2fb76") &&
    !identical(empreinte_donnees(e4, ROLES_EMP), "2264ac10aa5504caec70eb1f97b2fb76")
})
verifier("empreinte : zéro négatif normalisé (-0 et 0 donnent la même empreinte)", {
  a <- data.frame(x = c(0, 1)); b <- data.frame(x = c(-0, 1))
  identical(empreinte_donnees(a, c(id_contrat = "x")), empreinte_donnees(b, c(id_contrat = "x")))
})
verifier("empreinte : chaîne au codage bytes -> refus chaine_codage_bytes", {
  z <- "é"; Encoding(z) <- "bytes"
  leve_refus(empreinte_donnees(data.frame(x = c("a", z), stringsAsFactors = FALSE), c(segment = "x")),
             "chaine_codage_bytes")
})
verifier("empreinte : chaîne marquée UTF-8 invalide -> refus chaine_non_utf8", {
  z <- rawToChar(as.raw(c(0x61, 0xff))); Encoding(z) <- "UTF-8"
  leve_refus(empreinte_donnees(data.frame(x = c("a", z), stringsAsFactors = FALSE), c(segment = "x")),
             "chaine_non_utf8")
})
verifier("empreinte de la fiche = empreinte_donnees des données reçues (avant exclusions)",
         identical(x$fiche$empreintes$donnees, empreinte_donnees(BASE, ROLES)))

# --- empreinte du résultat (ADR 0011, point 11 ; cinquième annotation du 20/10/2026 ; #67) ---
#
# Natures : identité exacte (deux constructions, rejeu) ; valeur calculée à la
# main par un autre outil (texte du petit objet XT écrit et haché en Python,
# hashlib.md5, struct.pack('<d', x).hex() pour les doubles, hors R : 1057
# octets, md5 62ba547dde2bde7096af769a0af2a372) ; témoins de sensibilité et
# d'insensibilité sur des copies.
empreinte_resultat <- tablexp:::empreinte_resultat
# Double suivant d'un ulp (mot de poids faible des octets IEEE 754 + 1 ; v
# fini, non nul, de mot faible inférieur à .Machine$integer.max).
ulp_suivant <- function(v) {
  i <- readBin(writeBin(v, raw(), size = 8L, endian = "little"), "integer", n = 2L, size = 4L, endian = "little")
  i[1L] <- i[1L] + 1L
  readBin(writeBin(i, raw(), size = 4L, endian = "little"), "double", size = 8L, endian = "little")
}
verifier("empreinte du résultat (identité exacte) : égale sur deux constructions et au rejeu de la fiche, recalculée égale sur l'objet",
         identical(construire_table(BASE, ROLES, plan_type())$fiche$empreintes$resultat, x$fiche$empreintes$resultat) &&
           identical(construire_table(BASE, f$roles, f$plan, f$provenance)$fiche$empreintes$resultat,
                     x$fiche$empreintes$resultat) &&
           identical(empreinte_resultat(x), x$fiche$empreintes$resultat))
verifier("empreinte du résultat : inchangée quand execution, fiche$versions, fiche$plateforme, resultat, code ou statut_resultat_code changent", {
  y <- x
  y$execution <- list(debut = Sys.time() + 3600, duree_secondes = 1e9)
  y$fiche$versions <- list(tablexp = "9.9.9", R = "R version 0")
  y$fiche$plateforme$machine <- "autre"; y$fiche$plateforme$blas <- "autre BLAS"
  y$fiche$plateforme$bibliotheque_c <- "autre bibliothèque C"
  y$fiche$empreintes$resultat <- "0"; y$fiche$empreintes$code <- NA_character_
  y$fiche$empreintes$statut_resultat_code <- "autre"
  identical(empreinte_resultat(y), x$fiche$empreintes$resultat)
})
verifier("empreinte du résultat : fiche$plateforme$bibliotheque_c ajouté, changé ou retiré ne la change pas ; version_fiche incrémentée la change (#77)", {
  p <- x$fiche$plateforme
  sans <- x; sans$fiche$plateforme$bibliotheque_c <- NULL
  autre <- x; autre$fiche$plateforme$bibliotheque_c <- tablexp:::MENTION_BIBLIOTHEQUE_C_NON_RELEVEE
  v3 <- x; v3$fiche$version_fiche <- 3L
  identical(names(p), c("sysname", "release", "machine", "blas", "lapack", "bibliotheque_c")) &&
    identical(empreinte_resultat(sans), x$fiche$empreintes$resultat) &&
    identical(empreinte_resultat(autre), x$fiche$empreintes$resultat) &&
    identical(x$fiche$version_fiche, tablexp:::VERSION_FICHE) && identical(tablexp:::VERSION_FICHE, 4L) &&
    !identical(empreinte_resultat(v3), x$fiche$empreintes$resultat)
})
verifier("empreinte du résultat (témoin) : changée par un ulp d'un double du périmètre (exposition_jours[1]), et par la représentation", {
  y <- x
  v <- y$etapes$exposition$resultat$exposition$exposition_jours[1L]
  w <- ulp_suivant(v)
  y$etapes$exposition$resultat$exposition$exposition_jours[1L] <- w
  z <- x; z$fiche$empreintes$representation_resultat <- "tablexp resultat 0"
  w != v && abs(w - v) <= abs(v) * 2^-52 && !identical(empreinte_resultat(y), x$fiche$empreintes$resultat) &&
    !identical(empreinte_resultat(z), x$fiche$empreintes$resultat)
})
verifier("empreinte du résultat (témoin) : changée par l'empreinte des données, le plan et les rôles (dans le périmètre)", {
  a <- x; a$fiche$empreintes$donnees <- "0"
  b <- x; b$fiche$plan$delai_declaration <- 1
  r <- x; names(r$fiche$roles)[1L] <- "autre"
  length(unique(c(x$fiche$empreintes$resultat, empreinte_resultat(a), empreinte_resultat(b), empreinte_resultat(r)))) == 4L
})
XT <- structure(list(
  table = NULL,
  etapes = list(donnees = list(resultat = list(
                  lignes_conservees = structure(data.frame(t = as.POSIXct(0, origin = "1970-01-01", tz = "UTC"),
                                                           f = factor("a")), simulation = list(graine = 1L)),
                  numeros_conserves = 1L)),
                e = list(v = c(0.95, -0, NA, NaN, Inf, -Inf), n = c(a = 1L, b = NA), d = D(c("2024-02-29", NA)),
                         f = factor(c("y", "x")))),
  avertissements = data.frame(code = tablexp:::texte_moteur(c("é", NA)), nombre = c(2L, 0L), stringsAsFactors = FALSE),
  fiche = list(versions = list(R = "x"), plateforme = list(s = "y"),
               empreintes = list(algorithme = "md5", representation_resultat = "tablexp resultat 1",
                                 resultat = "z", code = NA_character_, statut_resultat_code = "s"),
               ok = TRUE, l = list(1.5, "a\tb\"c")),
  execution = list(debut = 1)), class = "construction")
verifier(paste("empreinte du résultat (valeur calculée à la main, hors R, Python) : 62ba547dde2bde7096af769a0af2a372",
               "(doubles en octets, jetons, -0, Date, facteur, chaînes échappées, data.frame, liste sans noms ;",
               "lignes_conservees, versions, plateforme, trois champs d'empreinte et execution hors du texte)"),
         identical(empreinte_resultat(XT), "62ba547dde2bde7096af769a0af2a372"))
verifier("empreinte du résultat : lignes_conservees hors du périmètre (colonne, classe, attribut ou noms de lignes de l'entrée sans effet ; M71, Q1, #67)", {
  y <- x
  lc <- y$etapes$donnees$resultat$lignes_conservees
  lc$autre <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC"); attr(lc, "simulation") <- list(a = 1); attr(lc, "foo") <- 1
  y$etapes$donnees$resultat$lignes_conservees <- lc
  identical(empreinte_resultat(y), x$fiche$empreintes$resultat)
})
# retirer() de empreinte_resultat() ignore un chemin absent : un champ renommé
# rentrerait en silence dans le périmètre (son contenu, la version de R par
# exemple, ferait varier l'empreinte). Chaque chemin de CHEMINS_HORS_RESULTAT
# existe donc dans une construction sans brut et dans une avec brut (poisson,
# binomial ; kaplan_meier) ; execution, ajouté après le calcul de l'empreinte,
# est le seul absent de l'objet que reçoit empreinte_resultat().
verifier("empreinte du résultat : chaque chemin de CHEMINS_HORS_RESULTAT, sauf execution, existe dans une construction sans brut, avec brut, avec kaplan_meier", {
  existe <- function(v, segments) {
    for (sg in segments) { if (!is.list(v) || !sg %in% names(v)) return(FALSE); v <- v[[sg]] }
    TRUE
  }
  chemins <- setdiff(tablexp:::CHEMINS_HORS_RESULTAT, "execution")
  etapes_brut <- function(est) {
    e <- plan_type()$etapes; e$brut <- list(estimateurs = est, niveau = 0.95); modifier(plan_type(), etapes = e)
  }
  plans <- list(plan_type(), etapes_brut(c("poisson", "binomial")), etapes_brut("kaplan_meier"))
  length(chemins) == length(tablexp:::CHEMINS_HORS_RESULTAT) - 1L && "execution" %in% tablexp:::CHEMINS_HORS_RESULTAT &&
    all(vapply(plans, function(pl) {
      z <- construire_table(BASE, ROLES, pl)
      all(vapply(strsplit(chemins, "/", fixed = TRUE), function(sg) existe(z, sg), logical(1L))) && "execution" %in% names(z)
    }, logical(1L)))
})
verifier("empreinte du résultat : attribut inattendu (dont simulation hors de lignes_conservees), classe autre que construction ou valeur sans écriture -> erreur de programmation", {
  msg <- function(expr) tryCatch({ force(expr); "" }, refus_tablexp = function(e) "refus", error = function(e) conditionMessage(e))
  prog <- function(m) grepl("erreur de programmation", m, fixed = TRUE)
  a <- x; attr(a$etapes$exposition$resultat$exposition, "foo") <- 1
  b <- x; attr(b$etapes$donnees$resultat$observations, "simulation") <- list(a = 1)
  c_ <- x; class(c_) <- c("construction", "autre")
  d <- x; d$etapes$exposition$resultat$exposition$age <- as.POSIXct(0, origin = "1970-01-01", tz = "UTC")
  e <- x; attr(e$fiche$roles, "foo") <- "x"
  all(vapply(list(msg(empreinte_resultat(a)), msg(empreinte_resultat(b)), msg(empreinte_resultat(c_)),
                  msg(empreinte_resultat(unclass(x))), msg(empreinte_resultat(d)), msg(empreinte_resultat(e))), prog, NA))
})
verifier("empreinte du résultat : colonne de rôle catégorie en facteur admise (type factor, libellés), niveaux permutés sans effet", {
  b1 <- BASE; b1$sexe <- factor(b1$sexe, levels = c("H", "F"))
  b2 <- BASE; b2$sexe <- factor(b2$sexe, levels = c("F", "H"))
  e1 <- construire_table(b1, ROLES, plan_type())$fiche$empreintes$resultat
  grepl("^[0-9a-f]{32}$", e1) && identical(e1, construire_table(b2, ROLES, plan_type())$fiche$empreintes$resultat)
})
# Attributs de colonne de l'entrée (label) sur chaque colonne de rôle, dans
# les deux unités (#67, M71, Q4) : la construction est acceptée et son
# empreinte égale celle de la même entrée sans attribut (observations recopie
# les colonnes par sous-indiçage, qui retire ces attributs).
for (u in c("tete", "contrat")) {
  r_u <- if (u == "tete") ROLES else ROLES[names(ROLES) != "id_tete"]
  verifier(sprintf("empreinte du résultat, unité %s : attribut label sur chaque colonne de rôle -> construction acceptée, empreinte égale à celle sans attribut", u), {
    ref_u <- construire_table(BASE, r_u, plan_type())
    ok <- identical(ref_u$fiche$unite_observation, u)
    for (col in unname(r_u)) {
      b <- BASE; attr(b[[col]], "label") <- "Libellé"
      e <- tryCatch(construire_table(b, r_u, plan_type())$fiche$empreintes$resultat, error = function(err) conditionMessage(err))
      ok <- ok && identical(e, ref_u$fiche$empreintes$resultat)
    }
    ok
  })
}
# Unité contrat avec id_tete déclaré (transition en_cours -> rachat, rôles
# complets) : la copie id_t[i] d'observations est exercée (audit de #67).
PLAN_RACHAT <- modifier(plan_type(), transition = RA)
verifier("empreinte du résultat, unité contrat avec id_tete déclaré : attribut label sur la colonne de tête -> construction acceptée, empreinte égale à celle sans attribut", {
  x0 <- construire_table(BASE, ROLES, PLAN_RACHAT)
  b <- BASE; attr(b$tete, "label") <- "Libellé"
  e <- tryCatch(construire_table(b, ROLES, PLAN_RACHAT)$fiche$empreintes$resultat, error = function(err) conditionMessage(err))
  identical(x0$fiche$unite_observation, "contrat") && "id_tete" %in% names(x0$etapes$donnees$resultat$observations) &&
    identical(e, x0$fiche$empreintes$resultat)
})
# Colonne d'identifiant portant l'attribut names (data.frame construit par
# list2DF) : data.frame() en tirait les noms de lignes d'observations, que
# l'empreinte du résultat refuse (régression de #67, revue finale). La
# construction aboutit, observations porte des noms de lignes automatiques,
# l'entrée est intacte et l'empreinte égale celle de l'entrée sans noms.
# Les deux premiers cas échouaient avant la correction ; le troisième
# (id_tete recopiée par `$<-` en unité contrat) aboutissait déjà et protège
# ce chemin.
for (cas_ in list(list("tete", "tete", plan_type(), ROLES),
                  list("contrat", "contrat", PLAN_RACHAT, ROLES),
                  list("contrat", "tete", PLAN_RACHAT, ROLES))) {
  verifier(sprintf("empreinte du résultat, unité %s, colonne %s portant names (list2DF) -> construction acceptée, noms de lignes d'observations automatiques, entrée intacte, empreinte égale à celle sans names",
                   cas_[[1L]], cas_[[2L]]), {
    l <- as.list(BASE); l[[cas_[[2L]]]] <- setNames(l[[cas_[[2L]]]], paste0("n", seq_len(nrow(BASE))))
    b <- list2DF(l); b0 <- b
    x0 <- construire_table(BASE, cas_[[4L]], cas_[[3L]])
    x <- tryCatch(construire_table(b, cas_[[4L]], cas_[[3L]]), error = function(err) conditionMessage(err))
    !is.null(names(b[[cas_[[2L]]]])) && is.list(x) && identical(x$fiche$unite_observation, cas_[[1L]]) &&
      identical(attr(x$etapes$donnees$resultat$observations, "row.names"), seq_len(nrow(x$etapes$donnees$resultat$observations))) &&
      identical(b, b0) && identical(x$fiche$empreintes$resultat, x0$fiche$empreintes$resultat)
  })
}
# Facteurs de rôle catégorie (sexe ; segment segmenté) recopiés normalisés
# dans observations (#67, M72), dans les deux unités : contrasts, sous-classe
# ou sous-classe d'ordered -> construction acceptée, empreinte égale à celle du
# facteur simple (de l'ordered) aux mêmes libellés.
BASE_SEG <- BASE; BASE_SEG$seg <- c("A", "B", "A", "A")
ROLES_SEG <- c(ROLES, segment = "seg")
for (u_ in list(list("tête", plan_type()), list("contrat", PLAN_RACHAT))) {
  for (r_ in list(list("sexe", "sexe"), list("segment", "seg"))) {
    verifier(sprintf("empreinte du résultat, unité %s, %s en facteur avec contrasts, de sous-classe mafac/factor ou x/ordered/factor -> construction acceptée, empreinte égale à celle du facteur simple (de l'ordered)",
                     u_[[1L]], r_[[1L]]), {
      pl <- u_[[2L]]
      if (r_[[1L]] == "segment") pl$segmentation <- "segment"
      col <- r_[[2L]]
      emp <- function(v) {
        b <- BASE_SEG; b[[col]] <- v
        tryCatch(construire_table(b, ROLES_SEG, pl)$fiche$empreintes$resultat, error = function(err) conditionMessage(err))
      }
      fs <- factor(BASE_SEG[[col]]); fo <- factor(BASE_SEG[[col]], ordered = TRUE)
      ct <- fs; contrasts(ct) <- contr.sum(2)
      sc <- fs; class(sc) <- c("mafac", "factor")
      so <- fo; class(so) <- c("x", "ordered", "factor")
      ef <- emp(fs); eo <- emp(fo)
      grepl("^[0-9a-f]{32}$", ef) && grepl("^[0-9a-f]{32}$", eo) &&
        identical(emp(ct), ef) && identical(emp(sc), ef) && identical(emp(so), eo)
    })
  }
}
verifier("texte de la fiche : facteur toujours refusé en forme texte (grammaire 1), admis en forme empreinte", {
  o <- list(f = factor("a"))
  leve_refus(tablexp:::texte_objet(o, "t", character(0)), "serialisation_impossible") &&
    is.raw(tablexp:::texte_objet(o, "t", character(0), "empreinte"))
})
verifier("texte empreinté : facteur admis seulement de classe exactement factor ou c(ordered, factor) ; c(factor, x) et c(ordered, factor, x) refusés", {
  sous <- function(cl) { f <- factor(c("a", "b")); class(f) <- cl; list(f = f) }
  leve_refus(tablexp:::texte_objet(sous(c("factor", "x")), "t", character(0), "empreinte"), "serialisation_impossible") &&
    leve_refus(tablexp:::texte_objet(sous(c("ordered", "factor", "x")), "t", character(0), "empreinte"), "serialisation_impossible") &&
    is.raw(tablexp:::texte_objet(sous("factor"), "t", character(0), "empreinte")) &&
    is.raw(tablexp:::texte_objet(sous(c("ordered", "factor")), "t", character(0), "empreinte"))
})

# --- empreinte du code (option (iv), M70 ; cinquième annotation du 20/10/2026, (c) ; #67) ---
#
# Nature : valeur recalculée par un autre outil (md5sum de GNU coreutils, sur
# R/*.R de la racine installée par la batterie), ou à défaut recalculée en R
# sans empreinte_sources().
recalcul_code <- function(racine) {
  fichiers <- sort(list.files(file.path(racine, "R"), pattern = "[.]R$"), method = "radix")
  md5 <- unname(tools::md5sum(file.path(racine, "R", fichiers)))
  t <- tempfile(); on.exit(unlink(t))
  writeBin(charToRaw(paste0(md5, "  R/", fichiers, "\n", collapse = "")), t)
  unname(tools::md5sum(t))
}
CODE <- tablexp:::EMPREINTE_CODE
verifier("empreinte du code : non NA, égale au recalcul en R sur R/*.R de la racine installée, et à fiche$empreintes$code",
         is.character(CODE) && length(CODE) == 1L && !is.na(CODE) && identical(CODE, recalcul_code(RACINE)) &&
           identical(f$empreintes$code, CODE))
if (nzchar(Sys.which("md5sum")) && nzchar(Sys.which("sh"))) {
  verifier("empreinte du code (autre outil) : égale à `LC_ALL=C md5sum R/*.R | md5sum` à la racine installée", {
    s <- system2("sh", c("-c", shQuote(sprintf("cd %s && LC_ALL=C md5sum R/*.R | md5sum", shQuote(RACINE)))), stdout = TRUE)
    identical(sub(" .*$", "", s), CODE)
  })
} else cat("  (md5sum ou sh absent : comparaison à l'autre outil non faite ; recalcul en R seulement)\n")
verifier("empreinte du code : R/ ne contient que des fichiers *.R (R CMD INSTALL évalue aussi .r, .S, .s, .q)",
         all(grepl("[.]R$", list.files(file.path(RACINE, "R"), all.files = TRUE, no.. = TRUE))))
verifier("empreinte du code : sources introuvables -> NA (sans DESCRIPTION, autre paquet, sans R/rapport.R, fichier illisible), statut nommé ; racine conforme -> recalcul", {
  es <- tablexp:::empreinte_sources
  r <- tempfile("racine_"); dir.create(file.path(r, "R"), recursive = TRUE)
  sans_description <- es(r)
  writeLines(c("Package: autre", "Version: 1"), file.path(r, "DESCRIPTION"))
  autre <- es(r)
  writeLines(c("Package: tablexp", "Version: 1"), file.path(r, "DESCRIPTION"))
  sans_rapport <- es(r)
  writeLines("x <- 1", file.path(r, "R", "rapport.R")); writeLines("y <- 2", file.path(r, "R", "a.R"))
  writeLines("z <- 3", file.path(r, "R", "b.r"))
  st <- tablexp:::statut_empreintes(NA_character_)
  conforme <- es(r); attendu <- recalcul_code(r)
  # Fichier illisible (lien cassé) : md5sum rend NA -> NA (audit de #67).
  casse <- if (isTRUE(file.symlink(file.path(r, "absent.R"), file.path(r, "R", "z.R")))) es(r) else NA_character_
  unlink(r, recursive = TRUE)
  is.na(sans_description) && is.na(autre) && is.na(sans_rapport) && identical(conforme, attendu) && is.na(casse) &&
    grepl("code : non calculée, sources introuvables à l'installation", st, fixed = TRUE) &&
    startsWith(st, "résultat : témoin d'identité sur la même machine seulement (E3.6, E4.6)") &&
    identical(Encoding(st), "UTF-8") && !grepl("introuvables", tablexp:::statut_empreintes("0"), fixed = TRUE)
})

# --- portefeuille simulé (#10) : identités exactes et recoupement -----------

gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
LOI <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
            rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
PF <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
           origine = D("2005-01-01"))
SIM <- simuler_portefeuille(LOI, PF, PER, 20261016L)
ROLES_SIM <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
               date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
DELAI <- 90
plan_sim <- function(transition, par_annee = FALSE)
  list(transition = transition, periode = PER, delai_declaration = DELAI, segmentation = "sexe",
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "plus_fine", par_annee = par_annee)))
t0 <- Sys.time()
XD <- construire_table(SIM, ROLES_SIM, plan_sim(VD))
duree <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("  construire_table, %d têtes simulées, vivant -> décédé : %.1f s\n", nrow(SIM), duree))
XR <- construire_table(SIM, ROLES_SIM, plan_sim(RA))
XA <- construire_table(SIM, ROLES_SIM, plan_sim(VD, par_annee = TRUE))
TD <- XD$etapes$exposition$resultat$exposition
TR <- XR$etapes$exposition$resultat$exposition
TA <- XA$etapes$exposition$resultat$exposition

borne <- as.numeric(PER$fin) - DELAI + 1
deb <- as.numeric(PER$debut)
ent <- as.numeric(SIM$date_entree); sor <- as.numeric(SIM$date_sortie); sor[is.na(sor)] <- Inf
a <- pmax(ent, deb); b <- pmin(sor, borne)
verifier("portefeuille simulé (identité exacte) : exposition totale en jours = somme des max(0, b - a), égalité entière",
         identical(sum(TD$exposition_jours), sum(pmax(0, b - a))) &&
           identical(sum(TR$exposition_jours), sum(pmax(0, b - a))))
verifier("portefeuille simulé (identité exacte) : décès comptés = décès simulés avec debut <= s <= fin - délai",
         identical(sum(TD$evenements),
                   sum(SIM$cause_sortie == "deces" & !is.na(sor) & sor >= deb & sor < borne)))
verifier("portefeuille simulé (identité exacte) : rachats comptés = rachats simulés avec debut <= s <= fin - délai, décès censurés",
         identical(sum(TR$evenements),
                   sum(SIM$cause_sortie == "rachat" & !is.na(sor) & sor >= deb & sor < borne)) &&
           identical(sum(TR$sorties_concurrentes),
                     sum(SIM$cause_sortie == "deces" & !is.na(sor) & sor >= deb & sor < borne)))
verifier("portefeuille simulé (identité exacte) : décès par sexe et par âge = décès simulés par âge au dernier anniversaire (age_revolu)", {
  k <- SIM$cause_sortie == "deces" & !is.na(sor) & sor >= deb & sor < borne
  att <- table(SIM$sexe[k], tablexp:::age_revolu(as.numeric(SIM$date_naissance[k]), sor[k]))
  obs <- TD[TD$evenements > 0, ]
  identical(as.integer(att[cbind(obs$sexe, as.character(obs$age))]), obs$evenements) &&
    sum(att) == sum(obs$evenements)
})
verifier("portefeuille simulé (recoupement : anniversaires par as.Date, âges extrêmes comparés à age_revolu() du module donnees, ligne par ligne) : somme des années d'âge à 1e-12 relatif", {
  i <- which(a < b)
  nai <- as.numeric(SIM$date_naissance)[i]
  xa <- tablexp:::age_revolu(nai, a[i]); xb <- tablexp:::age_revolu(nai, b[i] - 1)
  k <- xb - xa + 1
  r <- rep(seq_along(i), k)
  x <- xa[r] + sequence(k) - 1
  nd <- structure(nai, class = "Date")
  an_n <- as.integer(format(nd, "%Y"))[r]; md <- format(nd, "%m-%d")[r]
  anniv <- function(y) {
    v <- as.Date(sprintf("%04d-%s", y, md), optional = TRUE)
    v[is.na(v)] <- as.Date(sprintf("%04d-03-01", y[is.na(v)]))
    as.numeric(v)
  }
  bas <- anniv(an_n + x); haut <- anniv(an_n + x + 1)
  j <- pmin(b[i][r], haut) - pmax(a[i][r], bas)
  if (!identical(sum(j), sum(TD$exposition_jours))) "jours ligne par ligne différents"
  else proche(sum(TD$exposition_centrale), sum(j / (haut - bas)), rel = 1e-12, abs = 0)
})
verifier("portefeuille simulé (identité exacte) : par_annee, somme sur les années = exposition par âge seul, en jours", {
  s <- aggregate(exposition_jours ~ sexe + age, data = TA, FUN = sum)
  m <- merge(s, TD[c("sexe", "age", "exposition_jours")], by = c("sexe", "age"), all = TRUE)
  !anyNA(m) && identical(m$exposition_jours.x, m$exposition_jours.y) &&
    identical(sum(TA$evenements), sum(TD$evenements))
})
verifier("portefeuille simulé : unité tête, aucune exclusion, aucune cellule d'âge hors de la plage contiguë",
         identical(XD$fiche$unite_observation, "tete") && XD$etapes$donnees$resultat$nombre_lignes_exclues == 0L &&
           all(tapply(TD$age, TD$sexe, function(v) identical(v, seq(min(v), max(v))))))

# --- refus etape_prealable_manquante : préalable absent par étape (#27) ---------
# Attendus écrits à la main (ETAPES$prealable : exposition <- donnees ;
# brut <- donnees, exposition), vecteurs aplatis `etape` et `prealable`
# inchangés.
plan_prealable <- function(...) {
  e <- list(donnees = list(exclusions = character(0)),
            exposition = list(convention = "plus_fine", par_annee = FALSE),
            brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95))
  modifier(plan_type(), etapes = e[c(...)])
}
verifier("etape_prealable_manquante : exposition et brut sans donnees, préalable absent de chaque étape fautive, message inchangé", {
  r <- refus(valider_plan(plan_prealable("exposition", "brut")))
  identical(r$motif, "etape_prealable_manquante") &&
    identical(conditionMessage(r), "Étape(s) « exposition », « brut » sans son étape préalable « donnees ».") &&
    identical(r$details, list(etape = c("exposition", "brut"), prealable = "donnees",
                              prealables_par_etape = list(exposition = "donnees", brut = "donnees")))
})
verifier("etape_prealable_manquante : brut sans exposition (donnees présente), puis brut seul : préalables absents selon le plan", {
  r <- refus(valider_plan(plan_prealable("donnees", "brut")))
  r2 <- refus(valider_plan(plan_prealable("brut")))
  identical(r$motif, "etape_prealable_manquante") &&
    identical(r$details, list(etape = "brut", prealable = "exposition",
                              prealables_par_etape = list(brut = "exposition"))) &&
    identical(conditionMessage(r2), "Étape(s) « brut » sans ses étapes préalables « donnees », « exposition ».") &&
    identical(r2$details, list(etape = "brut", prealable = c("donnees", "exposition"),
                               prealables_par_etape = list(brut = c("donnees", "exposition"))))
})
verifier("etape_prealable_manquante : seule l'étape fautive figure dans prealables_par_etape (exposition seule fautive, brut non)", {
  r <- refus(valider_plan(plan_prealable("exposition")))
  identical(r$details, list(etape = "exposition", prealable = "donnees",
                            prealables_par_etape = list(exposition = "donnees")))
})
verifier("etape_prealable_manquante : plans à préalables présents toujours admis", {
  p1 <- plan_prealable("donnees", "exposition"); p2 <- plan_prealable("donnees", "exposition", "brut")
  identical(valider_plan(p1), p1) && identical(valider_plan(p2), p2)
})

# --- écriture canonique des valeurs en deux formes (#35 ; ADR 0011, points 2 et 3) ---
#
# Natures des attendus : valeur calculée hors R (octets de struct.pack('<d')
# de Python 3.13 sur les vecteurs de la table de l'ADR 0011), identité exacte
# (aller-retour readBin des seize hexadécimaux), recoupement (MD5 du texte
# attendu d'un échantillon de 10^5 doubles, produit hors R par le juge Python
# de tests/controles/ecriture_doubles.R, qui recoupe 10^6 doubles ligne à
# ligne ; ce contrôle n'est pas une assertion de la batterie).
local({
  ev <- tablexp:::ecrire_valeurs
  texte <- function(x, forme = "empreinte") rawToChar(ev(x, forme, "x")$octets)
  lignes <- function(x, forme = "empreinte") { t <- texte(x, forme); if (nzchar(t)) strsplit(t, "\n", fixed = TRUE)[[1L]] else character(0) }
  octets8 <- function(...) readBin(as.raw(c(...)), "double", n = 1L, size = 8L, endian = "little")

  verifier("écriture des valeurs, forme empreinte : vecteurs de test de l'ADR 0011 (0.95, 1, 0, -0, 1e-9, 5e-324, Inf, -Inf), type double",
           identical(lignes(c(0.95, 1, 0, -0, 1e-9, 5e-324, Inf, -Inf)),
                     c("666666666666ee3f", "000000000000f03f", "0000000000000000", "0000000000000000",
                       "95d626e80b2e113e", "0100000000000000", "Inf", "-Inf")) &&
             identical(ev(1, "empreinte", "x")$type, "double"))
  verifier("écriture des valeurs, forme empreinte : un double s'écrit en seize hexadécimaux minuscules, petit-boutiste, LF final, sans CR",
           identical(texte(0.95), "666666666666ee3f\n") && !grepl("\r", texte(c(1, 2, 3))))
  verifier("écriture des valeurs, forme texte : %.17g, jetons NA, NaN, Inf, -Inf (ADR 0011, point 3.1)",
           identical(lignes(c(0.95, 1, 0, -0, 1e-9, NA, NaN, Inf, -Inf), "texte"),
                     c("0.94999999999999996", "1", "0", "0", "1.0000000000000001e-09", "NA", "NaN", "Inf", "-Inf")))
  verifier("écriture des valeurs, forme empreinte : -0 et 0 donnent les mêmes octets (x + 0)",
           identical(lignes(-0), "0000000000000000") && identical(texte(-0), texte(0)))
  verifier("écriture des valeurs, forme empreinte : signe négatif, sous-normaux, plus petit normal, maximum (octets construits, valeurs calculées hors R)",
           identical(lignes(c(-1, -5e-324, octets8(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x0f, 0x00), octets8(0, 0, 0, 0, 0, 0, 0x10, 0),
                              .Machine$double.xmax, -.Machine$double.xmax, .Machine$double.xmin)),
                     c("000000000000f0bf", "0100000000000080", "ffffffffffff0f00", "0000000000001000",
                       "ffffffffffffef7f", "ffffffffffffefff", "0000000000001000")))
  verifier("écriture des valeurs, forme empreinte : NA, NA + 1, NaN et 0/0 s'écrivent par jeton, quels que soient leurs octets",
           identical(lignes(c(NA_real_, NA_real_ + 1, NaN, 0 / 0)), c("NA", "NA", "NaN", "NaN")))
  verifier("écriture des valeurs : les octets de NA_real_ et de NA_real_ + 1 diffèrent sous x86-64 (constat de plateforme qui motive les jetons, ADR 0011, point 2.2) ; le jeton est le même",
           {
             o <- function(v) writeBin(v, raw(), size = 8L, endian = "little")
             (Sys.info()[["machine"]] != "x86_64" || (!identical(o(NA_real_), o(NA_real_ + 1)) && !identical(o(NaN), o(0 / 0)))) &&
               identical(texte(NA_real_), texte(NA_real_ + 1)) && identical(texte(NaN), texte(0 / 0))
           })
  verifier("écriture des valeurs : le classement des jetons se lit sur la valeur reçue, non sur x + 0 (décision d'actuary, #35) ; mêmes lignes pour x et x + 0, dans les deux formes", {
    x <- c(NA, NaN, NA + 1, 0 / 0, -0, 0, Inf, -Inf, 1.5)
    cl <- function(v) c(is.na(v) & !is.nan(v), is.nan(v), is.finite(v), v == Inf, v == -Inf)
    identical(cl(x), cl(x + 0)) &&
      identical(lignes(x), c("NA", "NaN", "NA", "NaN", "0000000000000000", "0000000000000000", "Inf", "-Inf", "000000000000f83f")) &&
      identical(lignes(x), lignes(x + 0)) &&
      identical(lignes(x, "texte"), c("NA", "NaN", "NA", "NaN", "0", "0", "Inf", "-Inf", "1.5")) &&
      identical(lignes(x, "texte"), lignes(x + 0, "texte"))
  })
  verifier("écriture des valeurs : un double portant un attribut dim ou names s'écrit comme le vecteur nu, dans les deux formes, avec ou sans non fini (writeBin refuse dim)", {
    v <- c(1, 2.5, 0.95); w <- c(1, NA, 0.95, Inf)
    mat <- function(u) matrix(u, ncol = 1L)
    identical(texte(mat(v)), texte(v)) && identical(texte(mat(w)), texte(w)) && identical(texte(c(a = 1, b = NA)), texte(c(1, NA))) &&
      identical(texte(mat(v), "texte"), texte(v, "texte")) && identical(texte(mat(w), "texte"), texte(w, "texte")) &&
      identical(ev(mat(v), "empreinte", "x")$type, "double")
  })
  verifier("empreinte des données : une colonne double portant un attribut dim a l'empreinte de la colonne nue (finie ou avec NA)", {
    emp <- function(v) { d <- data.frame(i = seq_along(v)); d$x <- v; tablexp:::empreinte_donnees(d, c(id_contrat = "x")) }
    identical(emp(matrix(c(1, 2), 2L)), emp(c(1, 2))) && identical(emp(matrix(c(1, NA), 2L)), emp(c(1, NA)))
  })
  verifier("écriture des valeurs, forme empreinte : mélange de finis et de jetons, positions et ordre conservés (début, milieu, fin, consécutifs)", {
    x <- c(NA, 1, NaN, Inf, 0.95, -Inf, NA, NA, 5e-324, NaN)
    attendu <- c("NA", "000000000000f03f", "NaN", "Inf", "666666666666ee3f", "-Inf", "NA", "NA", "0100000000000000", "NaN")
    identical(lignes(x), attendu) &&
      identical(lignes(c(1, NA)), c("000000000000f03f", "NA")) && identical(lignes(c(NA, 1)), c("NA", "000000000000f03f"))
  })
  verifier("écriture des valeurs, forme empreinte : longueurs limites (vecteur vide, une valeur, uniquement des jetons)",
           identical(ev(numeric(0), "empreinte", "x")$octets, raw(0)) && identical(lignes(NA_real_), "NA") &&
             identical(lignes(c(NA_real_, -Inf)), c("NA", "-Inf")) && identical(lignes(2^-1074), "0100000000000000"))
  verifier("écriture des valeurs : les types autres que double s'écrivent de même dans les deux formes (integer, logical, character, factor, Date)", {
    d <- as.Date(c("2020-03-01", NA))
    cas <- list(c(1L, NA), c(TRUE, NA, FALSE), c("a\"b", NA, "été"), factor(c("H", "F", NA)), d)
    types <- c("integer", "logical", "character", "factor", "Date")
    all(vapply(cas, function(v) identical(ev(v, "empreinte", "x"), ev(v, "texte", "x")), NA)) &&
      identical(vapply(cas, function(v) ev(v, "texte", "x")$type, ""), types)
  })
  verifier("écriture des valeurs : forme inconnue ou mal formée -> refus forme_ecriture_inconnue",
           leve_refus(ev(1, "hexa", "x"), "forme_ecriture_inconnue") && leve_refus(ev(1, NA_character_, "x"), "forme_ecriture_inconnue") &&
             leve_refus(ev(1, c("texte", "empreinte"), "x"), "forme_ecriture_inconnue"))
  verifier("écriture des valeurs : type sans représentation -> refus type_non_canonique dans les deux formes",
           leve_refus(ev(list(1), "empreinte", "x"), "type_non_canonique") && leve_refus(ev(Sys.time(), "texte", "x"), "type_non_canonique"))

  # Échantillon de 10^5 doubles sous graine locale (recette de
  # tests/controles/ecriture_doubles.R) : motifs de 64 bits tirés au hasard et
  # valeurs de runif() transformées par des opérations IEEE exactes.
  echantillon <- function(n, graine) tablexp:::sous_graine(graine, {
    m <- n %/% 2L
    octets <- as.raw(sample.int(256L, 8L * m, replace = TRUE) - 1L)
    brut <- readBin(octets, "double", n = m, size = 8L, endian = "little")
    brut[!is.finite(brut)] <- 0.5
    u <- runif(n - m)
    c(brut, u[seq_len((n - m) %/% 2L)] * 1e6 - 5e5, 1 / u[-seq_len((n - m) %/% 2L)])
  })
  ech <- echantillon(1e5L, 20261019L)
  o <- ev(ech, "empreinte", "x")$octets
  verifier("écriture des valeurs, forme empreinte : échantillon de 10^5 doubles (graine 20261019), MD5 du texte égal à celui du juge Python (struct.pack('<d'), hors R)", {
    f <- tempfile(); writeBin(o, f); m <- unname(tools::md5sum(f)); unlink(f)
    if (length(ech) == 1e5L && identical(m, "b0a409caef68f9219d6b70501c190a4e")) TRUE
    else sprintf("%d doubles, MD5 %s au lieu de b0a409caef68f9219d6b70501c190a4e", length(ech), m)
  })
  verifier("écriture des valeurs, forme empreinte : l'échantillon de 10^5 doubles avec des jetons insérés (début, milieu, fin) s'écrit comme l'échantillon seul, aux jetons près", {
    x <- c(NA, ech[1:50000], NaN, Inf, ech[50001:1e5], -Inf, NA_real_ + 1)
    l <- lignes(x); k <- c(1L, 50002L, 50003L, 100004L, 100005L)
    identical(l[k], c("NA", "NaN", "Inf", "-Inf", "NA")) && identical(l[-k], lignes(ech))
  })
  verifier("écriture des valeurs, forme empreinte : échantillon de 10^5 doubles, aller-retour readBin des hexadécimaux identical() (x + 0)", {
    d <- match(as.integer(o[o != as.raw(0x0a)]), c(48:57, 97:102)) - 1L
    b <- as.raw(d[c(TRUE, FALSE)] * 16L + d[c(FALSE, TRUE)])
    length(o) == 17L * length(ech) && identical(readBin(b, "double", n = length(ech), size = 8L, endian = "little"), ech + 0)
  })

  # Empreinte des données : NA et NaN se distinguent ; les variantes d'octets
  # de NA et de NaN ne changent pas l'empreinte.
  verifier("empreinte des données, colonne double : NA et NaN donnent des empreintes différentes ; NA, NA + 1 et NaN, 0/0 donnent la même", {
    emp <- function(v) tablexp:::empreinte_donnees(data.frame(x = v), c(id_contrat = "x"))
    !identical(emp(c(1, NA)), emp(c(1, NaN))) && identical(emp(c(1, NA)), emp(c(1, NA_real_ + 1))) &&
      identical(emp(c(1, NaN)), emp(c(1, 0 / 0))) && !identical(emp(c(1, 2)), emp(c(1, 2 + 2^-51)))
  })
})

# --- écriture des chaînes sans liste d'un vecteur raw par ligne (#22, point 1 ; #67) ---

# Oracle : assemblage d'octets_chaines() avant #22, point 1 (R/rapport.R à
# df74de8, une liste de 3n vecteurs raw puis unlist()), recopié ici et non dans
# le package ; partie de refus omise (inchangée), appliquée à des entrées
# admises. Les cas sont du code texte, évalué ici et dans un sous-processus
# LC_ALL=C : chaînes non ASCII construites depuis leurs octets.
local({
  oracle <- function(x) {
    y <- enc2utf8(x)
    if (!length(y)) return(raw(0))
    valeurs <- unique(y)
    na <- is.na(valeurs)
    esc <- valeurs[!na]
    for (r in list(c("\\", "\\\\"), c("\n", "\\n"), c("\r", "\\r"), c("\t", "\\t"), c("\"", "\\\"")))
      esc <- gsub(r[1L], r[2L], esc, fixed = TRUE, useBytes = TRUE)
    corps <- vector("list", length(valeurs))
    corps[!na] <- lapply(esc, charToRaw)
    corps[na] <- list(charToRaw("NA"))
    i <- match(y, valeurs)
    na_i <- na[i]
    morceaux <- vector("list", 3L * length(y))
    morceaux[seq.int(1L, by = 3L, length.out = length(y))] <-
      ifelse(na_i, list(raw(0)), list(as.raw(0x22)))
    morceaux[seq.int(2L, by = 3L, length.out = length(y))] <- corps[i]
    morceaux[seq.int(3L, by = 3L, length.out = length(y))] <-
      ifelse(na_i, list(as.raw(0x0a)), list(as.raw(c(0x22, 0x0a))))
    unlist(morceaux, use.names = FALSE)
  }
  code_cas <- c(
    "u8 <- function(o) { x <- rawToChar(as.raw(o)); Encoding(x) <- 'UTF-8'; x }",
    "l1 <- rawToChar(as.raw(c(0x63, 0xe9))); Encoding(l1) <- 'latin1'",
    "e <- u8(c(0xc3, 0xa9)); euro <- u8(c(0xe2, 0x82, 0xac))",
    "CAS <- list(character(0), 'a', NA_character_, '', c(NA_character_, NA), c('', '', NA, ''), 'NA',",
    "  c('NA', NA, '\"NA\"', '\"\"', '\"', '\"\"\"'), c('\\\\', '\\n', '\\r', '\\t', '\"', '\\\\\\\\n\"\\r\\t', 'a\"\"\"b', '\\\\\"'),",
    "  c(e, 'x', euro, NA, e, paste0(euro, '\\t', e, '\"')), l1, c(l1, 'a', e, NA), rep(c('ab', NA, 'ab', ''), 50),",
    "  sprintf('C%09d', c(1:500, 500:1)), c(NA, sprintf('T%05d', 1:300), NA), paste0(strrep('z', 5000), '\\t'),",
    "  c(rawToChar(as.raw(c(0x01, 0x7f))), 'fin'))",
    "LIMITES <- c(2^30, 1, 3, 4, 5, 7, 10, 64)",
    "ecarts <- function(nouvelle, oracle) {",
    "  r <- character(0)",
    "  for (k in seq_along(CAS)) for (lim in LIMITES)",
    "    if (!identical(nouvelle(CAS[[k]], 'r', limite = lim), oracle(CAS[[k]]))) r <- c(r, sprintf('cas %d, limite %g', k, lim))",
    "  r",
    "}")
  eval(parse(text = code_cas))
  oc <- tablexp:::octets_chaines
  verifier(sprintf("octets_chaines() : octets identiques à l'oracle avant #22 (liste de 3n raw), %d cas adverses (NA, chaîne vide, non ASCII UTF-8 et latin-1, cinq échappements, triples guillemets, répétitions, longueurs 0 et 1) x %d limites de bloc, session %s",
                   length(CAS), length(LIMITES), Sys.getlocale("LC_CTYPE")), {
    r <- ecarts(oc, oracle)
    if (!length(r)) TRUE else paste("écarts :", paste(r, collapse = " ; "))
  })
  verifier("octets_chaines() : sortie à la main (NA sans guillemets, \"NA\" entre guillemets, chaîne vide, échappements, non ASCII)",
           identical(oc(c(NA, "NA", "", "a\"\\\n", u8(c(0xc3, 0xa9))), "r"),
                     c(charToRaw("NA\n\"NA\"\n\"\"\n\"a\\\"\\\\\\n\"\n\""), as.raw(c(0xc3, 0xa9)), charToRaw("\"\n"))))
  verifier("octets_chaines() : découpage en blocs (limite de 64 octets) sur 2 000 lignes avec NA, octets identiques à l'unique bloc", {
    x <- sprintf("V%06d", 1:2000); x[c(1L, 17L, 1000L, 2000L)] <- NA
    identical(oc(x, "r", limite = 64), oc(x, "r")) && identical(oc(x, "r"), oracle(x))
  })
  # Bornes des blocs (bornes_blocs()), tailles mesurées ici, indépendamment de
  # la fonction : taille d'une ligne = octets de guillemet, corps, guillemet,
  # LF ; taille d'un bloc = nchar() du texte collé comme dans octets_chaines()
  # (avant le gsub() des NA, dont le corps guillemet compte 4 octets).
  verifier("bornes_blocs() : partition de 1..n dans l'ordre ; bloc de plusieurs lignes <= limite et maximal (la ligne suivante ne tiendrait pas) ; ligne plus longue que limite seule dans son bloc, et elle seule marquée ; 45 limites x 3 jeux de corps", {
    e <- u8(c(0xc3, 0xa9))
    jeux <- list(strrep("x", (seq_len(300L) * 7L) %% 13L),
                 c("\"", "", "ab", paste0(e, e), "\"", strrep("y", 9L), "\\\"", "\"", strrep(e, 4L), "z"),
                 "seule")
    jeux <- lapply(jeux, function(v) { Encoding(v) <- "bytes"; v })
    taille_bloc <- function(v) nchar(paste0("\"", paste(v, collapse = "\"\n\""), "\"\n"), type = "bytes")
    ecarts <- character(0)
    for (j in seq_along(jeux)) for (lim in c(1:40, 64, 100, 1000, 2^20, 2^30)) {
      corps <- jeux[[j]]
      n <- length(corps)
      ligne <- nchar(corps, type = "bytes") + 3
      b <- tablexp:::bornes_blocs(corps, lim)
      k <- length(b$debut)
      partition <- k >= 1L && b$debut[1L] == 1L && b$fin[k] == n && all(b$debut <= b$fin) &&
        all(b$debut[-1L] == b$fin[-k] + 1L)
      ok <- partition && length(b$fin) == k && length(b$seule) == k
      if (ok) for (q in seq_len(k)) {
        s <- b$debut[q]; f <- b$fin[q]
        t <- taille_bloc(corps[s:f])
        ok <- ok && identical(b$seule[q], s == f && ligne[s] > lim) &&
          (if (s < f) t <= lim else TRUE) && (if (s == f && ligne[s] > lim) TRUE else t <= lim) &&
          (if (q < k && !b$seule[q] && !b$seule[q + 1L]) t + ligne[f + 1L] > lim else TRUE)
      }
      if (!ok) ecarts <- c(ecarts, sprintf("jeu %d, limite %g", j, lim))
    }
    if (!length(ecarts)) TRUE else paste("écarts :", paste(ecarts, collapse = " ; "))
  })
  script <- tempfile(fileext = ".R")
  writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(.harnais$bibliotheque)),
               paste("oracle <-", paste(deparse(oracle), collapse = "\n")), code_cas,
               "r <- ecarts(tablexp:::octets_chaines, oracle)",
               "cat('LOCALE', Sys.getlocale('LC_CTYPE'), '\\n')",
               "cat('ECARTS', length(r), paste(r, collapse = ' ; '), '\\n')"), script)
  r <- executer_rscript(script, env = "LC_ALL=C")
  unlink(script)
  verifier("octets_chaines() : sous-processus LC_ALL=C, octets identiques à l'oracle sur les mêmes cas et limites", {
    loc <- trimws(sub("^LOCALE ", "", grep("^LOCALE ", r$sortie, value = TRUE)))
    ec <- trimws(sub("^ECARTS ", "", grep("^ECARTS ", r$sortie, value = TRUE)))
    if (r$statut == 0L && identical(loc, "C") && identical(ec, "0")) TRUE
    else paste("statut", r$statut, ":", paste(r$sortie, collapse = " | "))
  })
})

# --- écriture des entiers et des dates sans chaîne par ligne (#71) ---
#
# Oracle : écrivains d'avant #71 (R/rapport.R à 5a95f9c), recopiés ici :
# entier par octets_lignes(sprintf("%d", x)), date par
# octets_lignes(txt[match(j, u)]), soit octets_lignes(textes[i]) pour
# octets_lignes_indices(textes, i). Identité exacte des octets, dans les deux
# formes. Valeurs pseudo-aléatoires sans générateur : (k * 2654435761) mod
# (2^32 - 1) - (2^31 - 1), exact en double (produit < 2^53), dans
# [-(2^31 - 1), 2^31 - 1].
local({
  ol <- tablexp:::octets_lignes
  oe <- tablexp:::octets_entiers
  oli <- tablexp:::octets_lignes_indices
  ev <- tablexp:::ecrire_valeurs
  oracle <- function(x) ol(sprintf("%d", x))
  im <- .Machine$integer.max
  k <- seq_len(1e5)
  CAS <- list(integer(0), NA_integer_, 0L, 7L, -1L, c(NA_integer_, NA), c(a = 1L, b = -2L, c = NA), matrix(c(1:5, NA), 2L),
              c(0L, 1L, -1L, 9L, 10L, -10L, 99L, 100L, -100L, im, -im, 999999999L, 1000000000L, -1000000000L, NA),
              rep(7L, 50L), -(1:2000), 1:1e5, rep(c(NA, 3L), 1000L),
              as.integer((k * 2654435761) %% (2^32 - 1) - (2^31 - 1)))
  verifier(sprintf("octets_entiers() et ecrire_valeurs(integer), deux formes : octets identiques à octets_lignes(sprintf(\"%%d\", x)), %d cas (vide, NA, 0, signes, 1 à 10 chiffres, ±(2^31 - 1), noms, dim, constante, 10^5 valeurs)",
                   length(CAS)), {
    ko <- which(!vapply(CAS, function(x) identical(oe(x), oracle(x)) && identical(ev(x, "empreinte", "x")$octets, oracle(x)) &&
                                         identical(ev(x, "texte", "x")$octets, oracle(x)) &&
                                         identical(ev(x, "texte", "x")$type, "integer"), NA))
    if (!length(ko)) TRUE else paste("cas en écart :", paste(ko, collapse = ", "))
  })
  verifier("octets_entiers() : sortie à la main (NA, 0, -1, 2147483647, -2147483647, 10)",
           identical(rawToChar(oe(c(NA, 0L, -1L, im, -im, 10L))), "NA\n0\n-1\n2147483647\n-2147483647\n10\n"))
  TXT <- c("1970-01-01", "NA", "NaN", "", "0001-01-01", "x")
  INDICES <- list(integer(0), 1L, 4L, c(4L, 4L), 6:1, rep(2:3, 100L), c(1L, 1L, 5L, 4L, 1L), (k %% 6L) + 1L)
  verifier(sprintf("octets_lignes_indices() : octets identiques à octets_lignes(textes[i]), %d suites d'indices (vide, texte vide, répétitions, ordre inverse, 10^5 indices)",
                   length(INDICES)), {
    ko <- which(!vapply(INDICES, function(i) identical(oli(TXT, i), ol(TXT[i])), NA))
    if (!length(ko)) TRUE else paste("suites en écart :", paste(ko, collapse = ", "))
  })
  verifier("ecrire_valeurs(Date), deux formes : octets identiques à l'écriture d'avant #71 (texte des valeurs distinctes, une chaîne par ligne), NA, NaN, stockage integer, valeurs répétées, 10^5 jours du domaine", {
    J <- function(j) structure(j, class = "Date")
    avant <- function(x) {
      j <- as.vector(unclass(x)); u <- unique(j)
      txt <- ifelse(is.nan(u), "NaN", "NA"); a <- !is.na(u)
      if (any(a)) txt[a] <- tablexp:::texte_date(u[a])
      ol(txt[match(j, u)])
    }
    dates <- list(J(numeric(0)), J(NA), J(NaN), J(c(0, NA, NaN, 0, -719162, 2932896, NA)), J(c(3L, NA, 3L, 0L)),
                  J(-719162 + (k * 2654435761) %% 3652059))
    all(vapply(dates, function(x) identical(ev(x, "empreinte", "d")$octets, avant(x)) &&
                                    identical(ev(x, "texte", "d")$octets, avant(x)), NA))
  })
})

# --- dates : année sur quatre chiffres, refus hors du domaine (#19 ; ADR 0011, point 4) ---

# Nature des attendus : textes écrits à la main ; jours depuis 1970-01-01 des
# quatre dates calculés hors R (Python 3.13, date(a, m, j).toordinal() -
# date(1970,1,1).toordinal()) : 0001-01-01 -> -719162, 0019-05-12 -> -712457,
# 0999-01-01 -> -354650, 9999-12-31 -> 2932896 ; isoformat() de Python écrit
# ces quatre dates comme attendu ci-dessous. Témoin d'empreinte : texte écrit
# par printf et haché par md5sum (GNU coreutils 9.4), hors de R :
#   tablexp canonique 2<TAB>3<TAB>3
#   #date_entree<TAB>Date / 0999-01-01 / 1000-01-01 / 2020-03-01
#   #date_naissance<TAB>Date / 0019-05-12 / 0950-03-01 / 1960-07-01
#   #date_sortie<TAB>Date / NA / 9999-12-31 / 0001-01-01
# (172 octets, md5 a96cb8dddf746b5aef7d4014096e6fad) ; avec NaN à la place du
# premier NA de date_sortie : 173 octets, md5 9253374d78befa9073e8f1b2e0f69181.
# Avant #19, l'écriture par format.Date donnait 999-01-01, 19-05-12 et 950-03-01
# (années sans zéros), et NA et NaN s'écrivaient NA.
local({
  ev <- tablexp:::ecrire_valeurs
  J <- function(j) structure(j, class = "Date")
  lignes <- function(x, forme = "empreinte") strsplit(rawToChar(ev(x, forme, "d")$octets), "\n", fixed = TRUE)[[1L]]

  verifier("écriture des dates, deux formes : 0999-01-01, 0019-05-12, 0001-01-01, 9999-12-31 sur quatre chiffres, type Date",
           all(vapply(c("empreinte", "texte"), function(f)
             identical(lignes(J(c(-354650, -712457, -719162, 2932896)), f),
                       c("0999-01-01", "0019-05-12", "0001-01-01", "9999-12-31")), NA)) &&
             identical(ev(J(0), "texte", "d")$type, "Date"))
  verifier("écriture des dates : 1970-01-01, 1000-01-01 (-354285) et un 29 février bissextile (2024-02-29, 19782) inchangés",
           identical(lignes(J(c(0, -354285, 19782))), c("1970-01-01", "1000-01-01", "2024-02-29")))
  verifier("écriture des dates : tous les jours de 1930-01-01 à 2024-12-31 identiques à format.Date (R1 à R6 inchangés)", {
    j <- as.numeric(as.Date("1930-01-01")):as.numeric(as.Date("2024-12-31"))
    identical(lignes(J(j)), format(J(j), "%Y-%m-%d"))
  })
  verifier("écriture des dates : stockage integer, valeurs répétées, ordre conservé, attribut names sans effet",
           identical(lignes(J(c(1L, 0L, 1L))), c("1970-01-02", "1970-01-01", "1970-01-02")) &&
             identical(lignes(J(c(a = 0, b = 31))), c("1970-01-01", "1970-02-01")))
  verifier("écriture des dates : vecteur vide -> aucun octet",
           identical(ev(J(numeric(0)), "empreinte", "d")$octets, raw(0)))
  verifier("écriture des dates : NA s'écrit NA, NaN s'écrit NaN, quel que soit le calcul qui les a produits, dans les deux formes",
           all(vapply(c("empreinte", "texte"), function(f)
             identical(lignes(J(c(NA, NaN, 0, NA_real_ + 1, 0 / 0, NA_integer_)), f),
                       c("NA", "NaN", "1970-01-01", "NA", "NaN", "NA")), NA)))
  verifier("écriture des dates : hors de [0001-01-01, 9999-12-31] (-719163, 2932897, infinis) -> refus date_hors_domaine, dans les deux formes",
           all(vapply(c("empreinte", "texte"), function(f)
             leve_refus(ev(J(c(0, -719163)), f, "d"), "date_hors_domaine") &&
               leve_refus(ev(J(c(0, 2932897)), f, "d"), "date_hors_domaine") &&
               leve_refus(ev(J(Inf), f, "d"), "date_hors_domaine") &&
               leve_refus(ev(J(-Inf), f, "d"), "date_hors_domaine"), NA)))

  # Témoin d'empreinte calculée hors R (années < 1000).
  TEM <- data.frame(n = J(c(-712457, -372488, as.numeric(as.Date("1960-07-01")))),
                    e = J(c(-354650, -354285, as.numeric(as.Date("2020-03-01")))),
                    s = J(c(NA, 2932896, -719162)))
  ROLES_TEM <- c(date_naissance = "n", date_entree = "e", date_sortie = "s")
  emp <- tablexp:::empreinte_donnees
  verifier("empreinte (valeur calculée à la main par printf et md5sum, hors R), années 19, 950, 999, 1 : a96cb8dddf746b5aef7d4014096e6fad",
           identical(emp(TEM, ROLES_TEM), "a96cb8dddf746b5aef7d4014096e6fad"))
  verifier("empreinte : texte canonique des dates à année < 1000 de 172 octets, sans CR", {
    fch <- tempfile(); tablexp:::texte_canonique(TEM, ROLES_TEM, fch)
    o <- readBin(fch, "raw", 1000L); unlink(fch)
    length(o) == 172L && !any(o == as.raw(0x0d))
  })
  TEM_NAN <- TEM; TEM_NAN$s[1L] <- J(NaN)
  verifier("empreinte (valeur calculée à la main, hors R), même jeu avec une date NaN à la place du NA : 9253374d78befa9073e8f1b2e0f69181, distincte de celle du NA",
           identical(emp(TEM_NAN, ROLES_TEM), "9253374d78befa9073e8f1b2e0f69181") &&
             !identical(emp(TEM_NAN, ROLES_TEM), emp(TEM, ROLES_TEM)))

  # Refus avant l'empreinte : la colonne cause_sortie porte une chaîne au codage
  # bytes, que le calcul de l'empreinte refuserait (chaine_codage_bytes) ; le
  # refus de la date hors domaine est levé le premier.
  z <- "é"; Encoding(z) <- "bytes"
  octets_ko <- BASE; octets_ko$cause <- c(z, BASE$cause[-1L])
  date_ko <- BASE; date_ko$sortie[1L] <- J(2932897)
  verifier("construire_table : sans date hors domaine, la chaîne bytes est refusée par l'empreinte (chaine_codage_bytes)",
           leve_refus(construire_table(octets_ko, ROLES, plan_type()), "chaine_codage_bytes"))
  verifier("construire_table : date hors domaine dans les données -> refus date_hors_domaine, avant le calcul de l'empreinte",
           { d <- date_ko; d$cause <- octets_ko$cause
             leve_refus(construire_table(d, ROLES, plan_type()), "date_hors_domaine") })
  verifier("construire_table : date hors domaine (10000-01-01) seule -> refus date_hors_domaine",
           leve_refus(construire_table(date_ko, ROLES, plan_type()), "date_hors_domaine"))
  verifier("construire_table : naissance à 0000-12-31 -> refus date_hors_domaine",
           { d <- BASE; d$naissance[2L] <- J(-719163)
             leve_refus(construire_table(d, ROLES, plan_type()), "date_hors_domaine") })
  verifier("construire_table : borne du plan hors domaine (début 0000-12-31, fin 10000-01-01) -> refus date_hors_domaine, avant le calcul de l'empreinte",
           leve_refus(construire_table(octets_ko, ROLES, modifier(plan_type(), periode = list(debut = J(-719163), fin = PER$fin))),
                      "date_hors_domaine") &&
             leve_refus(construire_table(octets_ko, ROLES, modifier(plan_type(), periode = list(debut = PER$debut, fin = J(2932897)))),
                        "date_hors_domaine"))

  # Une date stockée NaN atteint l'écrivain par construire_table() (mesuré :
  # type_conforme() la laisse passer comme un manquant) ; elle est traitée comme
  # une date manquante par les étapes, et se distingue de NA dans l'empreinte.
  nan_ <- BASE; nan_$sortie[2L] <- J(NaN)
  na_ <- BASE; na_$sortie[2L] <- J(NA)
  x_nan <- construire_table(nan_, ROLES, plan_type()); x_na <- construire_table(na_, ROLES, plan_type())
  # Les lignes conservées gardent la valeur reçue (NaN, non corrigée en NA) :
  # c'est le seul élément des étapes qui diffère, par identical() (mesuré).
  verifier("construire_table : une date NaN est acceptée et traitée comme un manquant (exposition, observations et avertissements identiques à ceux d'un NA ; lignes conservées : la valeur reçue), mais l'empreinte des données la distingue",
           identical(x_nan$etapes$exposition, x_na$etapes$exposition) &&
             identical(x_nan$etapes$donnees$resultat$observations, x_na$etapes$donnees$resultat$observations) &&
             isTRUE(all.equal(x_nan$etapes, x_na$etapes)) &&
             is.nan(unclass(x_nan$etapes$donnees$resultat$lignes_conservees$sortie[2L])) &&
             identical(x_nan$avertissements, x_na$avertissements) &&
             !identical(x_nan$fiche$empreintes$donnees, x_na$fiche$empreintes$donnees) &&
             identical(x_na$fiche$empreintes$donnees, emp(na_, ROLES)) && identical(x_nan$fiche$empreintes$donnees, emp(nan_, ROLES)))
})

# Date non entière (#19, reprise) : 100 et 100,5 s'écriraient de même ; refus nommé.
local({
  ev <- tablexp:::ecrire_valeurs
  J <- function(j) structure(j, class = "Date")
  for (f in c("empreinte", "texte")) {
    verifier(sprintf("écriture des dates, forme %s : 100.5 et -0.5 refusés (date_non_entiere), 100 accepté", f),
             leve_refus(ev(J(100.5), f, "d"), "date_non_entiere") && leve_refus(ev(J(c(0, -0.5)), f, "d"), "date_non_entiere") &&
               leve_refus(ev(J(c(NA, 3, 1e-9)), f, "d"), "date_non_entiere") &&
               identical(rawToChar(ev(J(100), f, "d")$octets), "1970-04-11\n"))
  }
  verifier("écriture des dates : un NaN ou un NA n'est pas une date non entière ; un infini reste date_hors_domaine ; ligne citée",
           identical(rawToChar(ev(J(c(NaN, NA)), "texte", "d")$octets), "NaN\nNA\n") &&
             leve_refus(ev(J(Inf), "texte", "d"), "date_hors_domaine") &&
             identical(tryCatch(ev(J(c(1, 2.5, 3, 2.5)), "texte", "d"), refus_tablexp = function(r) r$details$lignes), c(2L, 4L)))
  # Garde dates_hors_plage de fusionner_intervalles : étendue maximale du domaine
  # (3 652 060 jours) ; seuil 2^52 / 3 652 060 = 1,2332e9 têtes (mesuré).
  fu <- tablexp:::fusionner_intervalles
  verifier("fusionner_intervalles : étendue maximale du domaine, numéro de tête 1 232 000 000 admis, 1 234 000 000 refusé (dates_hors_plage), message citant étendue et têtes",
           is.list(fu(c(1L, 1232000000L), c(-719162, 0), c(2932896, 1))) &&
             leve_refus(fu(c(1L, 1234000000L), c(-719162, 0), c(2932896, 1)), "dates_hors_plage") &&
             grepl("3652060 jours pour 1234000000 têtes",
                   conditionMessage(tryCatch(fu(c(1L, 1234000000L), c(-719162, 0), c(2932896, 1)), refus_tablexp = function(r) r)),
                   fixed = TRUE))
})

# Plan canonique et fiche canonique (#42 ; ADR 0011, points 5.1 à 5.5 ; #22,
# point 6). Valeur calculée à la main : formes et ordres attendus ; identité
# exacte : deux plans de même sens donnent des objets identical() hors
# execution (le stockage d'une Date est invisible aux références).
local({
  cn <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES, tablexp:::ordres_canoniques())
  erreur <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = function(e) e), "error")
  J <- function(j) structure(j, class = "Date")
  pb <- modifier(plan_type(), etapes = c(plan_type()$etapes,
                                         list(brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95))))
  # Liste fermée des stockages admis, un témoin chacun.
  verifier("canoniser_plan : borne Date à stockage double inchangée, à stockage integer convertie en Date double",
           identical(cn(pb)$periode, PER) && typeof(PER$debut) == "double" &&
             identical(cn(modifier(pb, periode = list(debut = J(16436L), fin = J(20088L))))$periode, PER))
  verifier("canoniser_plan : délai double inchangé, délai integer converti en double",
           identical(cn(pb)$delai_declaration, 0) && identical(cn(modifier(pb, delai_declaration = 90L))$delai_declaration, 90))
  verifier("canoniser_plan : graine integer inchangée, graine double entière convertie en integer",
           identical(cn(modifier(pb, graine = 7L))$graine, 7L) && identical(cn(modifier(pb, graine = 7))$graine, 7L) &&
             identical(names(cn(modifier(pb, graine = 7L))), tablexp:::CHAMPS_PLAN))
  verifier("canoniser_plan : stockage hors de la liste fermée -> erreur (délai character, borne non Date, graine 1,5 ou hors étendue des integer)",
           erreur(cn(modifier(pb, delai_declaration = "0"))) &&
             erreur(cn(modifier(pb, periode = list(debut = 16436, fin = PER$fin)))) &&
             erreur(cn(modifier(pb, graine = 1.5))) && erreur(cn(modifier(pb, graine = 3e9))))
  verifier("canoniser_plan : garde-fous de la liste fermée -> erreur (graine NA integer ou double, étape absente du catalogue, paramètre en trop ou manquant)",
           erreur(cn(modifier(pb, graine = NA_integer_))) && erreur(cn(modifier(pb, graine = NA_real_))) &&
             erreur(cn(modifier(pb, etapes = c(pb$etapes, list(lissage = list()))))) &&
             erreur(cn(modifier(pb, etapes = { et <- pb$etapes; et$brut <- list(estimateurs = "poisson", niveau = 0.95, x = 1); et }))) &&
             erreur(cn(modifier(pb, etapes = { et <- pb$etapes; et$brut <- list(niveau = 0.95); et }))))
  verifier("canoniser_plan : nom en double -> erreur, jamais réduit en silence à sa première occurrence (paramètre d'étape, étape)",
           erreur(cn(modifier(pb, etapes = { et <- pb$etapes; et$brut <- c(et$brut, list(niveau = 0.5)); et }))) &&
             erreur(cn(modifier(pb, etapes = c(pb$etapes, pb$etapes["brut"])))) &&
             erreur(cn(modifier(pb, etapes = c(pb$etapes["donnees"], pb$etapes)))))
  # Plans de même sens que pb.
  variantes <- list(
    "début de période en integer" = modifier(pb, periode = list(debut = J(16436L), fin = PER$fin)),
    "délai de déclaration 0L" = modifier(pb, delai_declaration = 0L),
    "noms du plan, de transition et de periode permutés" =
      list(etapes = pb$etapes, segmentation = "sexe", periode = list(fin = PER$fin, debut = PER$debut),
           delai_declaration = 0, transition = list(arrivee = "deces", depart = "vivant")),
    "paramètres de brut et d'exposition permutés" =
      modifier(pb, etapes = list(donnees = pb$etapes$donnees, exposition = list(par_annee = FALSE, convention = "plus_fine"),
                                 brut = list(niveau = 0.95, estimateurs = c("poisson", "binomial")))))
  verifier("canoniser_plan : plan déjà canonique rendu identical() ; champs dans l'ordre de CHAMPS_PLAN",
           identical(cn(pb), pb) && identical(cn(plan_type()), plan_type()))
  for (n in names(variantes)) {
    q <- variantes[[n]]
    verifier(sprintf("canoniser_plan (%s) : canon = plan canonique, idempotente, valider_plan(canon) le rend inchangé", n),
             !identical(q, pb) && identical(cn(q), pb) && identical(cn(cn(q)), cn(q)) && identical(valider_plan(cn(q)), cn(q)))
  }
  prov <- list(fichier = "base.csv", empreinte = "d41d8cd98f00b204e9800998ecf8427e", algorithme = "md5")
  x0 <- sans_execution(construire_table(BASE, ROLES, pb, prov))
  for (n in names(variantes))
    verifier(sprintf("construire_table (%s) : objet identical() hors execution à celui du plan canonique", n),
             identical(sans_execution(construire_table(BASE, ROLES, variantes[[n]], prov)), x0))
  verifier("construire_table (rôles dans l'ordre inverse) : objet identical() hors execution ; fiche$roles dans l'ordre du catalogue",
           identical(sans_execution(construire_table(BASE, rev(ROLES), pb, prov)), x0) &&
             identical(x0$fiche$roles, ROLES) &&
             !is.unsorted(match(names(x0$fiche$roles), catalogue_roles()$role)))
  verifier("construire_table (provenance permutée) : objet identical() hors execution ; fiche$provenance dans l'ordre (fichier, empreinte, algorithme)",
           identical(sans_execution(construire_table(BASE, ROLES, pb, prov[c("algorithme", "fichier", "empreinte")])), x0) &&
             identical(x0$fiche$provenance, prov))
  verifier("construire_table : etapes$exposition$parametres$periode dans l'ordre (debut, fin), plan aux noms permutés",
           identical(names(sans_execution(construire_table(BASE, ROLES, variantes[[3L]], prov))$etapes$exposition$parametres$periode),
                     c("debut", "fin")))
  y <- construire_table(BASE, rev(ROLES), variantes[[3L]], prov[3:1])
  verifier("construire_table : rejeu de la fiche (rôles, plan canonique, provenance) identical() hors execution",
           identical(sans_execution(construire_table(BASE, y$fiche$roles, y$fiche$plan, y$fiche$provenance)), x0) &&
             identical(y$fiche$plan, pb))
  verifier("fiche : version 4, aucune copie du plan au premier niveau (transition, periode, delai_declaration, segmentation)",
           identical(x0$fiche$version_fiche, 4L) &&
             !any(c("transition", "periode", "delai_declaration", "segmentation") %in% names(x0$fiche)))

  # Ordre canonique des valeurs de segmentation, exclusions et estimateurs (#63 ;
  # ADR 0011, quatrième annotation du 20/10/2026). Valeur calculée à la main :
  # ordres des catalogues ; identité exacte : objets identical() hors execution,
  # fiche comprise, sur un portefeuille simulé (n = 5 000, graine 7) auquel est
  # ajouté un rôle segment constant par tête (trois segments).
  ord <- tablexp:::ordres_canoniques()
  verifier("ordres_canoniques : segmentation = sexe, segment ; exclusions = motifs de ligne puis de tête ; estimateurs = poisson, binomial, kaplan_meier",
           identical(names(ord), c("segmentation", "etapes/donnees/exclusions", "etapes/brut/estimateurs")) &&
             identical(ord$segmentation, c("sexe", "segment")) &&
             identical(ord[["etapes/brut/estimateurs"]], c("poisson", "binomial", "kaplan_meier")) &&
             identical(ord[["etapes/donnees/exclusions"]], c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE)))
  SIM63 <- simuler_portefeuille(LOI, modifier(PF, n = 5000), PER, 7L)
  SIM63$segment <- paste0("s", SIM63$id_tete %% 3L)
  ROLES63 <- c(ROLES_SIM, segment = "segment")
  EXC <- c("sortie_avant_entree", "age_aberrant", "tete_sexe_incoherent")
  EST <- c("poisson", "binomial", "kaplan_meier")
  SEG <- c("sexe", "segment")
  plan63 <- function(seg, exc, est)
    list(transition = VD, periode = PER, delai_declaration = 0, segmentation = seg,
         etapes = list(donnees = list(exclusions = exc),
                       exposition = list(convention = "plus_fine", par_annee = FALSE),
                       brut = list(estimateurs = est, niveau = 0.95)))
  canon63 <- plan63(SEG, EXC, EST)
  verifier("canoniser_plan (ordre des valeurs) : plan déjà canonique rendu identical(), idempotente, valide ; exclusions vides et segmentation vide inchangées",
           identical(cn(canon63), canon63) && identical(valider_plan(canon63), canon63) &&
             identical(cn(plan63(character(0), character(0), "poisson")), plan63(character(0), character(0), "poisson")))
  permutations <- list(
    "segmentation seule" = plan63(rev(SEG), EXC, EST),
    "exclusions seules" = plan63(SEG, EXC[c(3, 1, 2)], EST),
    "estimateurs seuls" = plan63(SEG, EXC, EST[c(3, 1, 2)]),
    "les trois champs" = plan63(rev(SEG), rev(EXC), rev(EST)),
    "les trois champs et les permutations du point 5.1" = {
      q <- plan63(rev(SEG), EXC[c(2, 3, 1)], EST[c(2, 3, 1)])
      list(etapes = list(donnees = q$etapes$donnees,
                         exposition = list(par_annee = FALSE, convention = "plus_fine"),
                         brut = list(niveau = 0.95, estimateurs = q$etapes$brut$estimateurs)),
           segmentation = q$segmentation, periode = list(fin = PER$fin, debut = PER$debut),
           delai_declaration = 0L, transition = list(arrivee = "deces", depart = "vivant"))
    })
  x63 <- sans_execution(construire_table(SIM63, ROLES63, canon63))
  for (n in names(permutations)) {
    q <- permutations[[n]]
    verifier(sprintf("canoniser_plan (ordre des valeurs, %s) : canon = plan canonique, idempotente, valider_plan(canon) le rend inchangé", n),
             !identical(q, canon63) && identical(cn(q), canon63) && identical(cn(cn(q)), cn(q)) &&
               identical(valider_plan(cn(q)), cn(q)))
    verifier(sprintf("construire_table (ordre des valeurs, %s) : objet identical() hors execution à celui du plan canonique, fiche comprise", n),
             identical(sans_execution(construire_table(SIM63, ROLES63, q)), x63))
  }
  verifier("construire_table (ordre des valeurs) : fiche$plan et fiche$hypotheses_brut$estimateurs dans l'ordre des catalogues",
           identical(x63$fiche$plan$segmentation, SEG) && identical(x63$fiche$plan$etapes$donnees$exclusions, EXC) &&
             identical(x63$fiche$plan$etapes$brut$estimateurs, EST) &&
             identical(x63$fiche$hypotheses_brut$estimateurs, EST))
  verifier("construire_table (ordre des valeurs) : rejeu de la fiche d'un plan permuté identical() hors execution",
           identical(sans_execution(construire_table(SIM63, ROLES63, construire_table(SIM63, ROLES63, permutations[[4L]])$fiche$plan)), x63))
  # Erreur de programmation sur une valeur hors du vecteur d'ordre, un doublon ou
  # un attribut, passés directement à canoniser_plan() (jamais retirés en silence).
  verifier("canoniser_plan (ordre des valeurs) : valeur inconnue, doublon ou attribut -> erreur de programmation pour chacun des trois champs",
           erreur(cn(plan63(c("sexe", "inconnu"), EXC, EST))) && erreur(cn(plan63(c("sexe", "sexe"), EXC, EST))) &&
             erreur(cn(plan63(SEG, c(EXC, "inconnu"), EST))) && erreur(cn(plan63(SEG, c(EXC, EXC[1L]), EST))) &&
             erreur(cn(plan63(SEG, EXC, c("poisson", "inconnu")))) && erreur(cn(plan63(SEG, EXC, c("poisson", "poisson")))) &&
             erreur(cn(plan63(SEG, EXC, c(a = "poisson")))) && erreur(cn(plan63(SEG, EXC, NA_character_))) &&
             erreur(cn(plan63(1L, EXC, EST))) &&
             grepl("erreur de programmation", conditionMessage(tryCatch(cn(plan63(SEG, EXC, c("poisson", "inconnu"))),
                                                                       error = function(e) e)), fixed = TRUE))
  verifier("canoniser_plan (ordre des valeurs) : catalogues fabriqués ; étape absente du plan -> rien à canoniser ; liste ordres vide -> valeurs inchangées",
           identical(tablexp:::canoniser_plan(plan63(c("a", "b"), EXC, EST), tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                              list(segmentation = c("b", "a")))$segmentation, c("b", "a")) &&
             identical(cn(plan_type()), plan_type()) &&
             identical(tablexp:::canoniser_plan(plan63(rev(SEG), rev(EXC), rev(EST)), tablexp:::CHAMPS_PLAN,
                                                tablexp:::PARAMETRES_ETAPES, list())$etapes$brut$estimateurs, rev(EST)))
})

# --- contrôles non répétés (#22, point 2 et notes de B6) ----------------------
#
# Nature des attendus : valeurs calculées à la main (compte d'exécutions de
# chaque contrôle, mesuré par trace() sur l'espace de noms installé) et
# identité exacte (controler = FALSE rend les mêmes objets que controler =
# TRUE sur des entrées valides ; chaque valeur dérivée égale la valeur rendue
# par son contrôle).
# construire_table() contrôle chaque entrée une seule fois, avant toute
# exécution ; les fonctions d'étape, appelées directement, contrôlent les
# leurs (refus conservés), sans refaire un contrôle entre elles.
CONTROLES_22 <- c("controler_roles", "controler_parametres_donnees", "controler_transition_periode",
                  "controler_segmentation", "controler_parametres_exposition", "controler_parametres_brut",
                  "deduire_recette", "resoudre_convention")
COMPTEUR_22 <- new.env()
compter_controles <- function(expr) {
  ns <- asNamespace("tablexp")
  for (nom in CONTROLES_22) assign(nom, 0L, envir = COMPTEUR_22)
  for (nom in CONTROLES_22)
    suppressMessages(trace(nom, where = ns, print = FALSE,
                           tracer = bquote(assign(.(nom), get(.(nom), envir = .(COMPTEUR_22)) + 1L, envir = .(COMPTEUR_22)))))
  on.exit(for (nom in CONTROLES_22) suppressMessages(untrace(nom, where = ns)))
  force(expr)
  vapply(CONTROLES_22, function(nom) get(nom, envir = COMPTEUR_22), integer(1L))
}
attendu_22 <- function(...) {
  x <- stats::setNames(integer(length(CONTROLES_22)), CONTROLES_22)
  m <- c(...); x[names(m)] <- as.integer(m)
  x
}
ecart_22 <- function(obtenu, attendu) {
  if (identical(obtenu, attendu)) return(TRUE)
  sprintf("obtenu %s ; attendu %s", paste(names(obtenu), obtenu, sep = "=", collapse = " "),
          paste(names(attendu), attendu, sep = "=", collapse = " "))
}
plan_km_22 <- modifier(plan_type(), etapes = c(plan_type()$etapes,
  list(brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95))))
plan_pb_22 <- modifier(plan_type(), etapes = c(plan_type()$etapes,
  list(brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95))))
plan_donnees_22 <- modifier(plan_type(), etapes = plan_type()$etapes["donnees"])

verifier("contrôles : construire_table() avec donnees, exposition et brut (Kaplan-Meier compris), chaque contrôle exécuté une fois (valeur calculée à la main)",
         ecart_22(compter_controles(construire_table(BASE, ROLES, plan_km_22)),
                  attendu_22(controler_roles = 1, controler_parametres_donnees = 1, controler_transition_periode = 1,
                             controler_segmentation = 1, controler_parametres_exposition = 1,
                             controler_parametres_brut = 1, deduire_recette = 1, resoudre_convention = 1)))
verifier("contrôles : construire_table() sans Kaplan-Meier, mêmes comptes (aucun contrôle du chemin Kaplan-Meier)",
         ecart_22(compter_controles(construire_table(BASE, ROLES, plan_pb_22)),
                  attendu_22(controler_roles = 1, controler_parametres_donnees = 1, controler_transition_periode = 1,
                             controler_segmentation = 1, controler_parametres_exposition = 1,
                             controler_parametres_brut = 1, deduire_recette = 1, resoudre_convention = 1)))
verifier("contrôles : construire_table() avec l'étape donnees seule, chaque contrôle de ses entrées exécuté une fois, aucun contrôle d'exposition ni de brut",
         ecart_22(compter_controles(construire_table(BASE, ROLES, plan_donnees_22)),
                  attendu_22(controler_roles = 1, controler_parametres_donnees = 1, controler_transition_periode = 1,
                             controler_segmentation = 1, deduire_recette = 1)))
verifier("contrôles : construire_table() avec plan refusé par valider_plan(), aucun contrôle au-delà du refus",
         ecart_22(compter_controles(refus(construire_table(BASE, ROLES, modifier(plan_km_22, delai_declaration = -1)))),
                  attendu_22(controler_parametres_donnees = 1, controler_transition_periode = 1)))

# Appels directs des fonctions d'étape : refus conservés, un contrôle une fois.
OBS_22 <- tablexp:::preparer_donnees(BASE, ROLES, VD, PER, 0, "sexe", character(0))$resultat$observations
EXPO_22 <- tablexp:::calculer_exposition(OBS_22, ROLES, VD, PER, 0, "sexe", "plus_fine", FALSE)
verifier("contrôles : preparer_donnees() en appel direct, contrôle des paramètres puis des rôles, une fois chacun",
         ecart_22(compter_controles(tablexp:::preparer_donnees(BASE, ROLES, VD, PER, 0, "sexe", character(0))),
                  attendu_22(controler_roles = 1, controler_parametres_donnees = 1, controler_transition_periode = 1,
                             controler_segmentation = 1)))
verifier("contrôles : calculer_exposition() en appel direct, paramètres, recette et convention contrôlés une fois chacun, rôles non contrôlés",
         ecart_22(compter_controles(tablexp:::calculer_exposition(OBS_22, ROLES, VD, PER, 0, "sexe", "plus_fine", FALSE)),
                  attendu_22(controler_parametres_donnees = 1, controler_transition_periode = 1, controler_segmentation = 1,
                             controler_parametres_exposition = 1, deduire_recette = 1, resoudre_convention = 1)))
appel_brut_22 <- function(estimateurs, observations = EXPO_22$fenetres, ...)
  tablexp:::calculer_taux_bruts(EXPO_22$resultat$exposition, "sexe", FALSE, EXPO_22$parametres$convention$retenue,
                                EXPO_22$parametres$unite_exposition, estimateurs, 0.95, observations = observations,
                                transition = VD, periode = PER, delai_declaration = 0,
                                parametres_exposition = EXPO_22$parametres, ...)
verifier("contrôles : calculer_taux_bruts() en appel direct avec Kaplan-Meier, la segmentation contrôlée une fois (module brut) et la transition, la période et le délai une fois, sans second contrôle de segmentation ni d'exclusions",
         ecart_22(compter_controles(appel_brut_22(c("poisson", "kaplan_meier"))),
                  attendu_22(controler_segmentation = 1, controler_parametres_brut = 1, controler_transition_periode = 1)))
verifier("contrôles : calculer_taux_bruts() en appel direct sans Kaplan-Meier, ni transition ni période contrôlées",
         ecart_22(compter_controles(appel_brut_22(c("poisson", "binomial"))),
                  attendu_22(controler_segmentation = 1, controler_parametres_brut = 1)))
verifier("contrôles : taux_kaplan_meier() ne contrôle rien (appelé par calculer_taux_bruts() après ses contrôles)",
         ecart_22(compter_controles(tablexp:::taux_kaplan_meier(EXPO_22$fenetres, EXPO_22$resultat$exposition, "sexe", VD, PER, 0,
                                                                 EXPO_22$parametres$convention$retenue, 0.95, EXPO_22$parametres)),
                  attendu_22()))

verifier("contrôles : taux_kaplan_meier() en appel direct sur un data.frame avec une segmentation dupliquée -> erreur de programmation (garde), non un refus ni une table à colonne parasite",
         {
           e <- tryCatch(tablexp:::taux_kaplan_meier(OBS_22, EXPO_22$resultat$exposition, c("sexe", "sexe"), VD, PER, 0,
                                                     EXPO_22$parametres$convention$retenue, 0.95, EXPO_22$parametres),
                         error = function(e) e)
           inherits(e, "error") && !inherits(e, "refus_tablexp") &&
             grepl("segmentation avec doublon (erreur de programmation)", conditionMessage(e), fixed = TRUE)
         })

# Refus en appel direct conservés, mêmes modules et mêmes motifs qu'avant.
refus_22 <- function(expr) { r <- refus(expr); if (is.null(r)) NULL else c(r$module, r$motif) }
brut_direct_22 <- function(segmentation, transition)
  tablexp:::calculer_taux_bruts(EXPO_22$resultat$exposition, segmentation, FALSE, "dernier_anniversaire",
                                EXPO_22$parametres$unite_exposition, "kaplan_meier", 0.95,
                                observations = EXPO_22$fenetres, transition = transition, periode = PER,
                                delai_declaration = 0, parametres_exposition = EXPO_22$parametres)
verifier("contrôles : refus en appel direct conservés (valeur calculée à la main : module et motif)", {
  r <- list(
    preparer_donnees_roles = refus_22(tablexp:::preparer_donnees(BASE, structure(ROLES, foo = 1), VD, PER, 0, "sexe", character(0))),
    preparer_donnees_delai = refus_22(tablexp:::preparer_donnees(BASE, ROLES, VD, PER, -1, "sexe", character(0))),
    exposition_convention = refus_22(tablexp:::calculer_exposition(OBS_22, ROLES, VD, PER, 0, "sexe", "age_fourni", FALSE)),
    exposition_recette = refus_22(tablexp:::calculer_exposition(OBS_22, ROLES[names(ROLES) != "date_sortie"], VD, PER, 0, "sexe", "plus_fine", FALSE)),
    brut_segmentation_km = refus_22(brut_direct_22("pays", VD)),
    brut_transition_km = refus_22(brut_direct_22("sexe", list(depart = "x", arrivee = "y"))))
  attendu <- list(preparer_donnees_roles = c("donnees", "roles_mal_formes"),
                  preparer_donnees_delai = c("donnees", "delai_invalide"),
                  exposition_convention = c("exposition", "convention_non_mise_en_oeuvre"),
                  exposition_recette = c("exposition", "aucune_recette"),
                  brut_segmentation_km = c("brut", "segmentation_invalide"),
                  brut_transition_km = c("donnees", "transition_inconnue"))
  if (identical(r, attendu)) TRUE else paste(names(r)[!mapply(identical, r, attendu)], collapse = ", ")
})
verifier("contrôles : seule la valeur FALSE omet les contrôles (0, NA, une chaîne et 0L contrôlent comme TRUE)", {
  r <- lapply(list(0, NA, "non", 0L), function(v)
    refus_22(tablexp:::preparer_donnees(BASE, ROLES, VD, PER, -1, "sexe", character(0), controler = v)))
  all(vapply(r, identical, logical(1L), c("donnees", "delai_invalide")))
})

# controler = FALSE : mêmes objets que controler = TRUE sur des entrées valides.
verifier("contrôles : preparer_donnees(controler = FALSE) rend le même objet que controler = TRUE",
         identical(tablexp:::preparer_donnees(BASE, ROLES, VD, PER, 0, "sexe", character(0), controler = FALSE),
                   tablexp:::preparer_donnees(BASE, ROLES, VD, PER, 0, "sexe", character(0))))
verifier("contrôles : calculer_exposition(controler = FALSE) rend le même objet que controler = TRUE, convention native et forcée, sans et avec découpage par année", {
  ok <- vapply(list(c("plus_fine", FALSE), c("age_atteint_annee", FALSE), c("dernier_anniversaire", TRUE),
                    c("age_atteint_annee", TRUE)), function(a)
    identical(tablexp:::calculer_exposition(OBS_22, ROLES, VD, PER, 0, "sexe", a[1L], as.logical(a[2L]), controler = FALSE),
              tablexp:::calculer_exposition(OBS_22, ROLES, VD, PER, 0, "sexe", a[1L], as.logical(a[2L]))), logical(1L))
  if (all(ok)) TRUE else paste("cas", paste(which(!ok), collapse = ", "))
})
verifier("contrôles : calculer_taux_bruts(controler = FALSE) rend le même objet que controler = TRUE, Kaplan-Meier compris",
         identical(appel_brut_22(c("poisson", "binomial", "kaplan_meier"), controler = FALSE),
                   appel_brut_22(c("poisson", "binomial", "kaplan_meier"))))

# Les valeurs dérivées sans contrôle égalent celles que rendent les contrôles.
verifier("contrôles : valeurs_parametres_donnees() égale la valeur rendue par controler_parametres_donnees(), pour chaque transition prise en charge et plusieurs délais", {
  tr <- tablexp:::TRANSITIONS
  ok <- unlist(lapply(seq_len(nrow(tr)), function(i) lapply(c(0, 1, 90), function(d) {
    t <- list(depart = tr$depart[i], arrivee = tr$arrivee[i])
    identical(tablexp:::valeurs_parametres_donnees(t, PER, d),
              tablexp:::controler_parametres_donnees(t, PER, d, "sexe", character(0)))
  })))
  if (all(ok)) TRUE else "valeurs et contrôle diffèrent"
})
verifier("contrôles : recette_branche() égale deduire_recette() et convention_retenue() égale resoudre_convention() (plus_fine, native, forcée)", {
  r <- identical(tablexp:::recette_branche(), tablexp:::deduire_recette(ROLES)) &&
    all(vapply(c("plus_fine", "dernier_anniversaire", "age_atteint_annee"), function(cv)
      identical(tablexp:::convention_retenue(cv, "dernier_anniversaire"),
                tablexp:::resoudre_convention(cv, "dernier_anniversaire")), logical(1L)))
  r && isTRUE(tablexp:::convention_retenue("age_atteint_annee", "dernier_anniversaire")$forcee)
})

# Règle des noms complets (outils.R), écrite une seule fois.
verifier("noms_complets : présents, sans NA, non vides (valeur calculée à la main)",
         identical(vapply(list(c("a", "b"), "a", character(0), NULL, c("a", ""), "", c("a", NA), NA_character_),
                          tablexp:::noms_complets, logical(1L)),
                   c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)))
