# -----------------------------------------------------------------------------
#  tests/unitaires/test_coherence_regularite.R : étape `coherence_regularite`,
#  cohérence des taux (Lignes directrices mortalité, chap. 4, II.1 et II.3) et
#  régularité (chap. 4, III.1) de la table construite et des tables de
#  référence évaluées en comparaison (issue #144, commit (4) de B19 ;
#  spécification de fond, critères C1 à C5 et tests T1 à T8, commentaire
#  6795722466 ; décisions D1 à D8 du mainteneur du 11/11/2026 ; spécification
#  de forme, commentaire 6799631402 ; ADR 0017, points 10.3, 13, 14 et 15 ;
#  M415)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : constantes, catalogue, motifs, modules,
#    messages et ordre des refus ; valeurs dyadiques de T1 à T3 (exactes en
#    double, identical() sauf mention ; Lowrie à 1e-14 en relatif) ;
#  - identité exacte : S_z de la table lissée (a) égal au S de l'étape
#    lissage (T6, 1e-12 en relatif), mêmes âges de décroissance pour une
#    table positionnée et sa référence (T5), invariance à l'ordre des lignes
#    et reproductibilité (T7), canonisation et aller-retour du plan ;
#  - propriété exacte : S_z non croissante en h sous Whittaker-Henderson (a)
#    (T6) ; ordre des segments positionnés sur une même référence ensemble
#    selon l'ordre des theta (T4) ;
#  - constat sur une loi déterministe : Makeham de la spécification (T8).
#  Jeux : agrégats écrits à la main (aucune graine), ceux de
#  tests/unitaires/test_validation_lissee.R ; aucune étape aléatoire, sauf
#  le plan à étape validation du test C (graine 22011207, plage de la
#  porteuse B19, M416).
# -----------------------------------------------------------------------------

D_ <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
# Refus attendu : module, motif, message (NULL : non comparé), fragment du
# message (NULL : non cherché) ; TRUE ou le texte de l'écart.
controle <- function(r, motif, message = NULL, fragment = NULL, module = "coherence_regularite") {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s »", r$module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!is.null(message) && !identical(conditionMessage(r), message)) return(sprintf("message « %s »", conditionMessage(r)))
  if (!is.null(fragment) && !all(vapply(fragment, grepl, NA, conditionMessage(r), fixed = TRUE)))
    return(sprintf("message « %s »", conditionMessage(r)))
  TRUE
}
tous <- function(l) { k <- vapply(l, isTRUE, NA); if (all(k)) TRUE else paste(names(l)[!k], unlist(l[!k]), collapse = " ; ") }
# Appel direct de la fonction d'étape.
cr <- function(construite, ordres, segmentation = character(0), plage = range(construite$age), tables = list(),
               variable = character(0), superieur = character(0), inferieur = character(0))
  tablexp:::calculer_coherence_regularite(construite, tables, segmentation, as.double(plage), ordres, variable,
                                          superieur, inferieur, "dernier_anniversaire")
tc <- function(g, ages = 60:65) data.frame(age = ages, qx = g, etat = "produite", stringsAsFactors = FALSE)

# --- jeu agrégats écrit à la main et plans ----------------------------------------

AGES <- 40:60
EH <- round(1000 * exp(-0.01 * (AGES - 40)), 3)
DH <- round(EH * 0.002 * exp(0.08 * (AGES - 40)))
JEU <- local({
  dh <- c(DH, round(DH * 0.6)); ei <- c(EH, EH * 1.1)
  data.frame(sexe = rep(c("H", "F"), each = length(AGES)), age = as.double(c(AGES, AGES)), evenements = as.double(dh),
             exposition_initiale = ei, exposition_centrale = ei - dh / 2, stringsAsFactors = FALSE)
})
ROLES <- c(sexe = "sexe", age = "age", evenements = "evenements", exposition_initiale = "exposition_initiale",
           exposition_centrale = "exposition_centrale")
LISSAGE <- list(methode = "wh_quadratique", plage_ages = c(40, 60), ordre_difference = 2, critere_lissage = "declare",
                ponderation = "exposition", parametre_lissage = 100)
LISSAGE_B <- list(methode = "wh_poisson", plage_ages = c(40, 60), ordre_difference = 2, critere_lissage = "declare",
                  niveau = 0.95, parametre_lissage = 100)
COH <- list(tables_reference = character(0), plage_ages = c(40, 60), ordres = c(1, 2),
            ordre_variable = "sexe", ordre_superieur = "H", ordre_inferieur = "F")
coh <- function(...) { l <- COH; v <- list(...); for (n in names(v)) l[n] <- list(v[[n]]); l }
plan_c <- function(coherence = COH, lissage = LISSAGE, positionnement = NULL, validation = NULL,
                   segmentation = "sexe", estimateurs = c("poisson", "binomial")) {
  etapes <- list(donnees = list(exclusions = character(0)),
                 exposition = list(convention = "dernier_anniversaire", par_annee = FALSE),
                 brut = list(estimateurs = estimateurs, niveau = 0.95))
  if (!is.null(lissage)) etapes$lissage <- lissage
  if (!is.null(positionnement)) etapes$positionnement <- positionnement
  if (!is.null(coherence)) etapes$coherence_regularite <- coherence
  if (!is.null(validation)) etapes$validation <- validation
  p <- list(transition = list(depart = "vivant", arrivee = "deces"),
            periode = list(debut = D_("2015-01-01"), fin = D_("2024-12-31")), delai_declaration = 0,
            segmentation = segmentation)
  if (!is.null(validation)) p$graine <- 22011207L
  p$etapes <- etapes
  p
}
rv <- function(...) refus_de(valider_plan(plan_c(...)))
# Tables de référence : ensemble sur 40-60 (positionnement, T4, T5), avec
# une décroissance non infime entre 49 et 50 ; par sexe H et F sur 30-70.
Q_ENS <- local({ q <- 0.002 * exp(0.08 * (AGES - 40)); q[AGES == 50] <- 0.9 * q[AGES == 50]; q })
importee <- function(d, nom) tablexp::table_reference(d, nom = nom, statut = "importee",
                                                     source = "valeurs écrites dans tests/unitaires/test_coherence_regularite.R")
T_ENS <- importee(data.frame(age = AGES, sexe = "ensemble", qx = Q_ENS, stringsAsFactors = FALSE), "ensemble 40-60")
T_HF <- importee(data.frame(age = rep(30:70, 2L), sexe = rep(c("H", "F"), each = 41L),
                            qx = c(0.001 * 1.09^(0:40), 0.0006 * 1.09^(0:40)), stringsAsFactors = FALSE), "HF 30-70")
X <- construire_table(JEU, ROLES, plan_c())
C <- X$etapes$coherence_regularite

# --- A. Catalogue et constantes (valeur calculée à la main) -------------------------

verifier("A ETAPES : coherence_regularite au rang 7, après homogeneite et avant validation, module validation, calculer_coherence_regularite, préalables donnees et exposition, non aléatoire, table_construite NA (ADR 0017, point 13.1)", {
  e <- tablexp:::ETAPES
  k <- match("coherence_regularite", e$etape)
  identical(k, 7L) && identical(e$etape[k + c(-1L, 1L)], c("homogeneite", "validation")) &&
    identical(e$module[k], "validation") && identical(e$fonction[k], "calculer_coherence_regularite") &&
    identical(e$prealable[[k]], c("donnees", "exposition")) && identical(e$aleatoire[k], FALSE) &&
    is.na(e$table_construite[k])
})
verifier("A PARAMETRES_ETAPES$coherence_regularite : six paramètres dans l'ordre, types, tous sans condition (ADR 0017, point 13.2)", {
  p <- tablexp:::PARAMETRES_ETAPES$coherence_regularite
  identical(p$parametre, c("tables_reference", "plage_ages", "ordres", "ordre_variable", "ordre_superieur",
                           "ordre_inferieur")) &&
    identical(p$type, c("character", "double", "double", "character", "character", "character")) &&
    all(is.na(p$recette)) && all(is.na(p$selon)) && all(lengths(p$valeurs) == 0L)
})
verifier("A MATRICE_RECETTES : coherence_regularite admise (oui) sous les trois recettes, sans rôle ni motif", {
  m <- tablexp:::MATRICE_RECETTES[tablexp:::MATRICE_RECETTES$etape == "coherence_regularite", ]
  identical(m$recette, c("lignes_dates_completes", "agregats", "stocks_successifs")) &&
    identical(m$admis, rep("oui", 3L)) && all(is.na(m$role_requis)) && all(is.na(m$motif)) && all(is.na(m$estimateur))
})
verifier("A ETATS_REGULARITE, ETATS_ORDRE_SEGMENTS, TRI_NUMERIQUE, ETAPES_PLAGE_AGES",
         identical(tablexp:::ETATS_REGULARITE, c("produite", "differences_nulles", "r_hors_domaine")) &&
           identical(tablexp:::ETATS_ORDRE_SEGMENTS, c("produite", "sans_objet")) &&
           identical(tablexp:::TRI_NUMERIQUE, "tri_numerique") &&
           "coherence_regularite" %in% tablexp:::ETAPES_PLAGE_AGES)
verifier("A statuts : sept lignes (C1, C2, C3 sur la table construite et sur une table de référence, C4, C5 pour r et S^r) ; textes de la spécification (fragments) ; référence de méthode (SHA-256 complet)", {
  s <- tablexp:::STATUTS_COHERENCE_REGULARITE
  st <- function(t, g, n) s$statut[s$table == t & s$grandeur == g & s$nature == n]
  tous(list(
    lignes = identical(paste(s$table, s$grandeur, s$nature),
                       c("ages croissance toutes", "ages differences_croissantes toutes",
                         "ordre_segments inegalite_verifiee construite", "ordre_segments inegalite_verifiee reference",
                         "regularite regularite toutes", "regularite r_estime toutes", "regularite regularite_r toutes")),
    c1 = startsWith(st("ages", "croissance", "toutes"), "résultat exact sur les valeurs enregistrées de la table"),
    c2 = startsWith(st("ages", "differences_croissantes", "toutes"), "descriptif : delta2_x = (g_(x+2) - g_(x+1)) - (g_(x+1) - g_x)"),
    c3 = grepl("« si les groupes constitués par les hommes et les femmes ont des distributions comparables",
               st("ordre_segments", "inegalite_verifiee", "construite"), fixed = TRUE),
    c3_ref = identical(st("ordre_segments", "inegalite_verifiee", "reference"),
                       "choix de l'outil, à finalité comparative, non prescrit par II.3"),
    c4 = endsWith(st("regularite", "regularite", "toutes"), paste(
      "pour une table lissée par Whittaker-Henderson (a), au même z et sur la plage même du lissage, fonction non",
      "croissante du paramètre h en arithmétique exacte (résultat exact : pour h1 < h2, (h2 - h1)(S_z(g_h2) -",
      "S_z(g_h1)) <= 0, somme des deux inégalités d'optimalité) ; sur une plage strictement contenue dans celle du",
      "lissage, S_z n'est pas monotone en h en général")),
    c5 = identical(st("regularite", "r_estime", "toutes"), st("regularite", "regularite_r", "toutes")) &&
      grepl("S^r = somme (a_x - r b_x)^2, égale en arithmétique exacte à S_z - (somme a b)^2 / somme b^2 <= S_z",
            st("regularite", "r_estime", "toutes"), fixed = TRUE) &&
      startsWith(st("regularite", "r_estime", "toutes"), "résultat exact à g fixé (moindres carrés à un paramètre)"),
    reference = grepl("70ba7463b840bb4fd378dd05c7dad66213239d0bb033ea126b7b0b4669d26835",
                      tablexp:::REFERENCE_METHODE_COHERENCE_REGULARITE, fixed = TRUE),
    nom = identical(tablexp:::NOM_TABLE_CONSTRUITE, "table construite (table de l'étape productrice du plan)"),
    convention = identical(tablexp:::MENTION_CONVENTION_TABLE_REFERENCE, paste(
      "non portée par la table de référence : l'objet table_reference n'a pas de champ de convention d'âge ; voir sa",
      "source (colonne source)"))))
})

# --- B. Refus de valider_plan() (valeur calculée à la main) --------------------------

M_SANS <- paste("L'étape coherence_regularite évalue toujours la table construite de la construction : une",
                "étape qui la produit (lissage ou positionnement) doit figurer au plan, avant elle.")
verifier("B coherence_sans_table_construite : plan sans étape productrice, sans et avec tables_reference, message de la spécification (M415)",
         tous(list(sans = controle(rv(lissage = NULL), "coherence_sans_table_construite", M_SANS),
                   avec = controle(rv(lissage = NULL, coherence = coh(tables_reference = "t")),
                                   "coherence_sans_table_construite", M_SANS))))
verifier("B cles_tables_invalides : NA, chaîne vide, doublon, nommé, liste, logical", {
  l <- list(na = NA_character_, vide = "", doublon = c("a", "a"), nomme = c(a = "t"), liste = list("t"), logique = TRUE)
  tous(lapply(l, function(v) controle(rv(coherence = coh(tables_reference = v)), "cles_tables_invalides",
                                      fragment = "character(0) si aucune table de référence n'est évaluée")))
})
verifier("B plage_ages_invalide sous le module coherence_regularite (integer, min > max)",
         tous(list(entier = controle(rv(coherence = coh(plage_ages = c(40L, 60L))), "plage_ages_invalide"),
                   inverse = controle(rv(coherence = coh(plage_ages = c(60, 40))), "plage_ages_invalide"))))
verifier("B ordres_invalides : vide, 0, doublon, integer, non entier, z = max - min + 1 (T3) ; z = max - min admis", {
  l <- list(vide = numeric(0), zero = 0, doublon = c(2, 2), entier = 2L, fraction = 1.5, trop = 21)
  tous(c(lapply(l, function(v) controle(rv(coherence = coh(ordres = v)), "ordres_invalides",
                                        fragment = "max(plage_ages) - min(plage_ages) = 20")),
         list(limite = is.null(rv(coherence = coh(ordres = 20))))))
})
verifier("B ordre_segments_invalide : longueurs, variable hors segmentation, ordre sans segmentation, a = b, doublon, sexe hors H/F, forme ; tous les défauts cités", {
  o <- function(v, a, b, ...) rv(coherence = coh(ordre_variable = v, ordre_superieur = a, ordre_inferieur = b), ...)
  tous(list(
    longueurs = controle(o("sexe", c("H", "F"), "F"), "ordre_segments_invalide", fragment = "longueurs différentes (1, 2 et 1)"),
    hors = controle(o("segment", "a", "b"), "ordre_segments_invalide",
                    fragment = "variable(s) « segment » hors de la segmentation du plan (« sexe »)"),
    sans_segmentation = controle(o("sexe", "H", "F", segmentation = character(0)), "ordre_segments_invalide",
                                 fragment = "hors de la segmentation du plan (aucune variable de segmentation)"),
    egales = controle(o("sexe", "H", "H"), "ordre_segments_invalide",
                      fragment = "modalités supérieure et inférieure égales (ordre(s) n° 1)"),
    doublon = controle(o(c("sexe", "sexe"), c("H", "H"), c("F", "F")), "ordre_segments_invalide",
                       fragment = "ordre(s) déclaré(s) en double (ordre(s) n° 2)"),
    sexe = controle(o("sexe", "X", "F"), "ordre_segments_invalide", fragment = "modalité(s) de sexe « X » hors de « H », « F »"),
    forme = controle(o("sexe", NA_character_, "F"), "ordre_segments_invalide", fragment = "pour « ordre_superieur »"),
    tous_cites = controle(o(c("segment", "sexe"), c("a", "F"), c("b", "F")), "ordre_segments_invalide",
                          fragment = c("« segment » hors de la segmentation", "égales (ordre(s) n° 2)")),
    vides = is.null(o(character(0), character(0), character(0)))))
})
verifier("B plage_ages_hors_table_construite : plage hors de celle de l'étape productrice (lissage 40-60), message",
         tous(list(bas = controle(rv(coherence = coh(plage_ages = c(39, 60))), "plage_ages_hors_table_construite",
                                  paste("Cohérence et régularité : la plage d'âges 39 à 60 n'est pas contenue dans celle de",
                                        "la table construite (40 à 60, plage d'âges de l'étape qui la produit).")),
                   haut = controle(rv(coherence = coh(plage_ages = c(41, 61))), "plage_ages_hors_table_construite"),
                   incluse = is.null(rv(coherence = coh(plage_ages = c(45, 55)))))))
verifier("B ordre des refus : cles avant plage, plage avant ordres, ordres avant ordre entre segments, ordre entre segments avant coherence_sans_table_construite, celle-ci avant la plage hors table construite", {
  tous(list(
    a = controle(rv(coherence = coh(tables_reference = NA_character_, plage_ages = 1L)), "cles_tables_invalides"),
    b = controle(rv(coherence = coh(plage_ages = 1L, ordres = 0)), "plage_ages_invalide"),
    c = controle(rv(coherence = coh(ordres = 0, ordre_variable = "x")), "ordres_invalides"),
    d = controle(rv(lissage = NULL, coherence = coh(ordre_variable = "x")), "ordre_segments_invalide"),
    e = controle(rv(lissage = NULL, coherence = coh(plage_ages = c(0, 100))), "coherence_sans_table_construite")))
})

# --- C. Contrôles d'entrée avant exécution et clés multiples -------------------------

verifier("C table_reference_absente (contrôle 1.3) : clé citée par coherence_regularite non reçue", {
  r <- refus_de(construire_table(JEU, ROLES, plan_c(coherence = coh(tables_reference = "hf"))))
  controle(r, "table_reference_absente", module = "construire_table",
           fragment = "(paramètre etapes/coherence_regularite/tables_reference)")
})
verifier("C table_annee_non_admise puis plage_ages_hors_table, module coherence_regularite, chaque clé dans l'ordre reçu ; seconde clé en défaut citée", {
  t_annee <- importee(data.frame(age = rep(40:60, 2L), sexe = "H", qx = 0.01, annee = rep(2020:2021, each = 21L)),
                      "avec annee")
  t_courte <- importee(data.frame(age = 45:60, sexe = "ensemble", qx = 0.01), "courte")
  pl <- function(k) plan_c(coherence = coh(tables_reference = k))
  tous(list(
    annee = controle(refus_de(construire_table(JEU, ROLES, pl("a"), tables_reference = list(a = t_annee))),
                     "table_annee_non_admise", fragment = "Cohérence et régularité : la table de clé « a » porte une colonne annee"),
    plage = controle(refus_de(construire_table(JEU, ROLES, pl(c("hf", "c")), tables_reference = list(hf = T_HF, c = t_courte))),
                     "plage_ages_hors_table",
                     paste("Cohérence et régularité : la table de clé « c » (sexe ensemble) couvre les âges 45 à 60, qui",
                           "ne contiennent pas la plage d'âges 40 à 60."))))
})
verifier("C clés multiples : deux tables évaluées dans l'ordre reçu (hf puis ens) ; une clé citée à la fois par la validation et par coherence_regularite", {
  x2 <- construire_table(JEU, ROLES, plan_c(coherence = coh(tables_reference = c("hf", "ens"))),
                         tables_reference = list(ens = T_ENS, hf = T_HF))
  t2 <- x2$etapes$coherence_regularite$parametres$tables
  val <- list(table_reference = "hf", relation_table = "independante", plage_ages = c(40, 60), tranches_age = c(40, 50),
              niveau = 0.95, replications = 9)
  x3 <- construire_table(JEU, ROLES, plan_c(coherence = coh(tables_reference = "hf"), validation = val),
                         tables_reference = list(hf = T_HF))
  tous(list(ordre = identical(t2$cle, c(NA, "hf", "ens")) && identical(t2$nature, c("construite", "reference", "reference")),
            fiche = identical(names(x2$fiche$tables_reference), c("ens", "hf")),
            convention = identical(t2$convention_age, c("dernier_anniversaire", rep(tablexp:::MENTION_CONVENTION_TABLE_REFERENCE, 2L))),
            meta = identical(t2$nom, c(tablexp:::NOM_TABLE_CONSTRUITE, "HF 30-70", "ensemble 40-60")) &&
              identical(t2$statut, c(NA, "importee", "importee")) && identical(t2$sexes, c(NA, "H, F", "ensemble")),
            double_citation = identical(names(x3$fiche$tables_reference), "hf") &&
              identical(x3$etapes$coherence_regularite$parametres$tables$cle, c(NA, "hf"))))
})
verifier("C cles_citees() : vecteur de clés, une ligne par élément, dans l'ordre des chemins ; character(0) ne cite rien", {
  p <- plan_c(coherence = coh(tables_reference = c("b", "a")), positionnement = NULL)
  cc <- tablexp:::cles_citees(p, tablexp:::PARAMETRES_CLE_TABLE)
  p0 <- plan_c()
  identical(cc, data.frame(cle = c("b", "a"), parametre = rep("etapes/coherence_regularite/tables_reference", 2L),
                           stringsAsFactors = FALSE)) &&
    identical(nrow(tablexp:::cles_citees(p0, tablexp:::PARAMETRES_CLE_TABLE)), 0L)
})
verifier("C ordre_modalite_absente en cours d'étape : ordre sexe H > F sur une table construite sans segment F", {
  r <- refus_de(construire_table(JEU[JEU$sexe == "H", ], ROLES, plan_c()))
  controle(r, "ordre_modalite_absente",
           paste("Cohérence et régularité : modalité(s) déclarée(s) dans l'ordre entre segments absente(s) de",
                 "tous les segments de la table construite : sexe = « F »."))
})

# --- D. Calculs : T1 à T3, T8 (valeur calculée à la main ; constat) -----------------

R1 <- cr(tc(c(10, 12, 11, 15, 20, 30) / 1024), c(1, 2))
verifier("D T1 : delta1 1024 = (2, -1, 4, 5, 10, NA), delta2 1024 = (-3, 5, 1, 5, NA, NA) ; décroissance à 61, différences décroissantes à 60 ; S_1 = 146/1024^2, S_2 = 60/1024^2 ; termes 5 et 4", {
  a <- R1$resultat$ages; co <- R1$resultat$coherence; g <- R1$resultat$regularite
  tous(list(d1 = identical(a$delta1 * 1024, c(2, -1, 4, 5, 10, NA)), d2 = identical(a$delta2 * 1024, c(-3, 5, 1, 5, NA, NA)),
            c1 = identical(a$croissance, c(TRUE, FALSE, TRUE, TRUE, TRUE, NA)) &&
              identical(co$croissance_ages_en_defaut, "61") && identical(co$croissance_en_defaut, 1L),
            c2 = identical(a$differences_croissantes, c(FALSE, TRUE, TRUE, TRUE, NA, NA)) &&
              identical(co$differences_ages_en_defaut, "60") && identical(co$differences_examinees, 4L),
            s = identical(g$regularite, c(146, 60) / 1024^2), m = identical(g$termes, c(5L, 4L)),
            ages = identical(a$age, 60:65) && identical(a$segment, rep("sans segmentation", 6L))))
})
verifier("D T1 Lowrie (C5, 1e-14 en relatif) : z = 1, r = 109/330, S^r 1024^2 = 37,9909090909091 ; z = 2, r = 9/23, S^r 1024^2 = 1218/23 ; S^r <= S_z", {
  g <- R1$resultat$regularite
  rel <- function(a, b) abs(a - b) <= 1e-14 * abs(b)
  tous(list(r1 = rel(g$r_estime[1L], 109 / 330), r2 = rel(g$r_estime[2L], 9 / 23),
            s1 = rel(g$regularite_r[1L] * 1024^2, 146 - 327^2 / 990), s2 = rel(g$regularite_r[2L] * 1024^2, 1218 / 23),
            borne = all(g$regularite_r <= g$regularite), etat = identical(g$etat, c("produite", "produite"))))
})
verifier("D C5 sans soustraction (K1 du visa d'actuary) : g = 2e-5 1,1^x, exponentielle exacte, 20-100, z = 1 à 3 : S^r >= 0 et S^r <= 1e-12 S_z ; S^r = somme (a - r b)^2", {
  x <- 20:100; g <- 2e-5 * 1.1^x
  r <- cr(data.frame(age = x, qx = g, etat = "p"), c(1, 2, 3))$resultat$regularite
  a <- diff(g); b <- g[seq_along(a)]
  identical(r$etat, rep("produite", 3L)) && all(r$regularite_r >= 0) && all(r$regularite_r <= 1e-12 * r$regularite) &&
    identical(r$regularite_r[1L], sum((a - r$r_estime[1L] * b)^2))
})
verifier("D T2 : g = (1 + 2^k)/1024 : aucune inégalité en défaut ; S_2 = 85/1024^2 ; z = 2 : r = 1, S^r = 0 exactement", {
  r <- cr(tc((1 + 2^(0:5)) / 1024), 2)
  g <- r$resultat$regularite; co <- r$resultat$coherence
  identical(co$croissance_en_defaut, 0L) && identical(co$differences_en_defaut, 0L) &&
    identical(co$croissance_ages_en_defaut, "") && identical(g$regularite, 85 / 1024^2) &&
    identical(g$r_estime, 1) && identical(g$regularite_r, 0)
})
verifier("D T3 limites : (1, 0, 1, 0, 1, 0)/1024, z = 1 -> r = -1, r_hors_domaine, S^r NA ; constante, z = 2 -> S_2 = 0, differences_nulles, aucune décroissance ; droite croissante -> S_2 = 0 ; z = max - min : un terme", {
  a <- cr(tc(c(1, 0, 1, 0, 1, 0) / 1024), 1)$resultat$regularite
  k <- cr(tc(rep(5, 6) / 1024), c(2, 5))
  d <- cr(tc((1:6) / 1024), c(2, 5))$resultat$regularite
  tous(list(hors = identical(a$r_estime, -1) && identical(a$etat, "r_hors_domaine") && identical(a$regularite_r, NA_real_),
            constante = identical(k$resultat$regularite$regularite, c(0, 0)) &&
              identical(k$resultat$regularite$etat, rep("differences_nulles", 2L)) &&
              identical(k$resultat$regularite$r_estime, c(NA_real_, NA_real_)) &&
              identical(k$resultat$coherence$croissance_en_defaut, 0L),
            droite = identical(d$regularite[1L], 0) && identical(d$etat[1L], "produite") && identical(d$regularite_r[1L], 0),
            un_terme = identical(d$termes, c(4L, 1L))))
})
verifier("D T8 (constat, loi déterministe) : Makeham mu = 5e-4 + 2e-5 1,1^x, q = 1 - e^-mu, 20-120 : aucune décroissance ; delta2 < 0 exactement à 113-118 ; sur 20-100 : aucun âge", {
  x <- 20:120; q <- 1 - exp(-(5e-4 + 2e-5 * 1.1^x))
  r <- cr(data.frame(age = x, qx = q, etat = "p"), 2)
  r100 <- cr(data.frame(age = 20:100, qx = q[1:81], etat = "p"), 2)
  identical(r$resultat$coherence$croissance_en_defaut, 0L) &&
    identical(r$resultat$coherence$differences_ages_en_defaut, "113, 114, 115, 116, 117, 118") &&
    identical(r100$resultat$coherence$differences_en_defaut, 0L)
})

# --- E. Ordre entre segments (C3) : T4, T5, tables de référence ---------------------

verifier("E T4 : H et F sur 60-65, g_F > g_H à un seul âge (62) : seul âge en défaut, détail et synthèse ; autres = « sans autre variable »", {
  gH <- c(10, 12, 14, 16, 18, 20) / 1024; gF <- c(5, 6, 15, 8, 9, 10) / 1024
  t <- data.frame(sexe = rep(c("H", "F"), each = 6L), age = rep(60:65, 2L), qx = c(gH, gF), etat = "p",
                  stringsAsFactors = FALSE)
  r <- cr(t, 1, "sexe", variable = "sexe", superieur = "H", inferieur = "F")
  d <- r$resultat$ordre_segments; s <- r$resultat$ordre_segments_synthese
  identical(d$age[!d$inegalite_verifiee], 62L) && identical(d$ecart, gH - gF) &&
    identical(s$ages_en_defaut, 1L) && identical(s$liste_ages_en_defaut, "62") && identical(s$ages_examines, 6L) &&
    identical(s$autres, "sans autre variable") && identical(s$etat, "produite") && identical(r$diagnostics$ordre_segments_en_defaut, 1L) &&
    identical(unique(r$resultat$ages$segment), c("sexe = H", "sexe = F"))
})
POS <- list(methode = "smr", table_reference = "ens", plage_ages = c(40, 60), niveau = 0.95)
XP <- construire_table(JEU, ROLES, plan_c(lissage = NULL, positionnement = POS,
                                          coherence = coh(tables_reference = "ens", ordre_variable = c("sexe", "sexe"),
                                                          ordre_superieur = c("H", "F"), ordre_inferieur = c("F", "H"))),
                       tables_reference = list(ens = T_ENS))
verifier("E T4 (propriété exacte) : deux segments positionnés sur une même référence ensemble : aucune inversion pour H > F (theta_H >= theta_F), inversion à tous les âges pour F > H", {
  th <- XP$etapes$positionnement$resultat$smr
  s <- XP$etapes$coherence_regularite$resultat$ordre_segments_synthese
  sc <- s[s$nature == "construite", ]
  th$smr[th$sexe == "H"] >= th$smr[th$sexe == "F"] &&
    identical(sc$superieure, c("H", "F")) && identical(sc$ages_en_defaut, c(0L, 21L))
})
verifier("E T5 (identité exacte) : âges de décroissance de la table positionnée = ceux de la référence (49, décroissance non infime) ; C3 sur une table ensemble : sans_objet, sans refus", {
  co <- XP$etapes$coherence_regularite$resultat$coherence
  s <- XP$etapes$coherence_regularite$resultat$ordre_segments_synthese
  sr <- s[s$nature == "reference", ]
  identical(co$croissance_ages_en_defaut, rep("49", 3L)) && identical(co$nature, c("construite", "construite", "reference")) &&
    identical(sr$etat, rep("sans_objet", 2L)) && identical(sr$ages_examines, c(0L, 0L)) && all(is.na(sr$ages_en_defaut)) &&
    identical(XP$etapes$coherence_regularite$diagnostics$ordre_segments_sans_objet, 2L)
})
verifier("E C3 sur une table de référence H et F : ordre sexe évalué (H >= F à tous les âges) ; ordre sur une autre variable (segment) : sans_objet", {
  t <- data.frame(sexe = rep(c("H", "F"), each = 6L), segment = "s1", age = rep(40:45, 2L),
                  qx = c(6:11, 3:8) / 1024, etat = "p", stringsAsFactors = FALSE)
  t2 <- t; t2$segment <- "s2"; t2$qx <- t2$qx / 2
  r <- cr(rbind(t, t2), 1, c("sexe", "segment"), plage = c(40, 45), tables = list(hf = T_HF),
          variable = c("sexe", "segment"), superieur = c("H", "s1"), inferieur = c("F", "s2"))
  s <- r$resultat$ordre_segments_synthese
  tous(list(cles = identical(paste(s$nature, s$variable, s$autres),
                             c("construite sexe segment = s1", "construite sexe segment = s2",
                               "construite segment sexe = H", "construite segment sexe = F",
                               "reference sexe sans autre variable", "reference segment NA")),
            etats = identical(s$etat, c(rep("produite", 5L), "sans_objet")),
            reference = identical(s$ages_en_defaut[5L], 0L) && identical(s$ages_examines[5L], 6L),
            libelles = identical(unique(r$resultat$ages$segment[r$resultat$ages$nature == "construite"]),
                                 c("sexe = H ; segment = s1", "sexe = F ; segment = s1", "sexe = H ; segment = s2",
                                   "sexe = F ; segment = s2"))))
})
verifier("E C3 : combinaison où l'un des deux segments manque : aucune ligne", {
  t <- data.frame(sexe = c(rep("H", 3L), rep("F", 3L), rep("H", 3L)), segment = rep(c("s1", "s1", "s2"), each = 3L),
                  age = rep(40:42, 3L), qx = c(3:5, 1:3, 2:4) / 1024, etat = "p", stringsAsFactors = FALSE)
  r <- cr(t, 1, c("sexe", "segment"), variable = "sexe", superieur = "H", inferieur = "F")
  identical(r$resultat$ordre_segments_synthese$autres, "segment = s1") && identical(nrow(r$resultat$ordre_segments), 3L)
})

# --- F. Régularité sur la table lissée (T6) ------------------------------------------

verifier("F T6 (a) : S_2 de la table lissée sur la même plage = S de l'étape lissage (1e-12 en relatif), par segment ; S_z toujours sur qx", {
  s <- X$etapes$lissage$resultat$segments
  g <- C$resultat$regularite; g <- g[g$ordre == 2, ]
  identical(g$segment, paste("sexe =", s$sexe)) &&
    all(abs(g$regularite - s$regularite) <= 1e-12 * abs(s$regularite))
})
verifier("F T6 (propriété exacte) : S_2 non croissante sur h = 1, 100, 1e4 (Whittaker-Henderson (a), z = 2)", {
  S <- sapply(c(1, 100, 1e4), function(h) {
    l <- LISSAGE; l$parametre_lissage <- h
    g <- construire_table(JEU, ROLES, plan_c(lissage = l))$etapes$coherence_regularite$resultat$regularite
    g$regularite[g$ordre == 2]
  })
  all(S[, 1L] >= S[, 2L]) && all(S[, 2L] >= S[, 3L]) && all(S[, 1L] > S[, 3L])
})
verifier("F T6 (b) : sous wh_poisson, S_2 de coherence_regularite (sur qx) différent du S de l'étape lissage (sur ln mu, echelle_penalite)", {
  xb <- construire_table(JEU, ROLES, plan_c(lissage = LISSAGE_B))
  s <- xb$etapes$lissage$resultat$segments
  g <- xb$etapes$coherence_regularite$resultat$regularite; g <- g[g$ordre == 2, ]
  q <- xb$etapes$lissage$resultat$table_lissee
  identical(xb$etapes$lissage$parametres$echelle_penalite, "ln_mu") &&
    all(abs(g$regularite - s$regularite) > 1e-3 * abs(s$regularite)) &&
    identical(g$segment, paste("sexe =", s$sexe)) &&
    identical(g$regularite, vapply(s$sexe, function(k) sum(diff(q$qx[q$sexe == k], differences = 2)^2), 0,
                                   USE.NAMES = FALSE))
})

# --- G. Reproductibilité, plan, rapport (identité exacte) ---------------------------

verifier("G T7 : invariance à l'ordre des lignes des données (étape coherence_regularite identical()) ; reproductibilité identical() hors execution ; aucun tirage (aucune graine au plan)", {
  xp <- construire_table(JEU[rev(seq_len(nrow(JEU))), ], ROLES, plan_c())
  x2 <- construire_table(JEU, ROLES, plan_c())
  identical(xp$etapes$coherence_regularite, C) && identical(sans_execution(x2), sans_execution(X)) &&
    is.null(X$fiche$plan$graine)
})
verifier("G ordres canonisés par tri croissant : c(2, 1) -> c(1, 2), objet identical() hors execution, fiche comprise ; canoniser_plan() idempotente ; aller-retour ecrire_plan() / lire_plan() à vecteurs vides", {
  cn <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                             tablexp:::ordres_canoniques())
  xi <- construire_table(JEU, ROLES, plan_c(coherence = coh(ordres = c(2, 1))))
  pv <- plan_c(coherence = coh(ordre_variable = character(0), ordre_superieur = character(0),
                               ordre_inferieur = character(0), ordres = c(3, 1)))
  c1 <- cn(pv)
  tous(list(objet = identical(sans_execution(xi), sans_execution(X)),
            canon = identical(c1$etapes$coherence_regularite$ordres, c(1, 3)) && identical(cn(c1), c1),
            aller_retour = identical(lire_plan(ecrire_plan(c1)), c1),
            vides = identical(lire_plan(ecrire_plan(c1))$etapes$coherence_regularite$tables_reference, character(0))))
})
verifier("G canoniser_plan() : ordres non canonisable (integer, NA, doublon) -> erreur de programmation", {
  cn <- function(o) { p <- plan_c(); p$etapes$coherence_regularite$ordres <- o
    tryCatch({ tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                        tablexp:::ordres_canoniques()); FALSE },
             error = function(e) grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)) }
  cn(1:2) && cn(c(1, NA)) && cn(c(2, 2))
})
verifier("G structure du résultat : colonnes et types de ages, coherence, ordre_segments, ordre_segments_synthese, regularite ; diagnostics integer ; aucun avertissement", {
  r <- C$resultat
  ty <- function(d) vapply(d, typeof, "")
  tous(list(
    noms = identical(names(r), c("ages", "coherence", "ordre_segments", "ordre_segments_synthese", "regularite")),
    ages = identical(ty(r$ages), c(nature = "character", cle = "character", segment = "character", age = "integer",
                                   qx = "double", delta1 = "double", delta2 = "double", croissance = "logical",
                                   differences_croissantes = "logical")),
    regularite = identical(ty(r$regularite), c(nature = "character", cle = "character", segment = "character",
                                               ordre = "double", termes = "integer", regularite = "double",
                                               r_estime = "double", regularite_r = "double", etat = "character")),
    synthese = identical(names(r$ordre_segments_synthese),
                         c("nature", "cle", "variable", "superieure", "inferieure", "autres", "ages_examines",
                           "ages_en_defaut", "liste_ages_en_defaut", "etat")) &&
      identical(typeof(r$ordre_segments_synthese$ages_en_defaut), "integer"),
    detail = identical(names(r$ordre_segments),
                       c("nature", "cle", "variable", "superieure", "inferieure", "autres", "age", "g_superieure",
                         "g_inferieure", "ecart", "inegalite_verifiee")),
    diagnostics = all(vapply(C$diagnostics, is.integer, NA)),
    parametres = identical(names(C$parametres), c("tables_reference", "plage_ages", "ordres", "ordre_variable",
                                                  "ordre_superieur", "ordre_inferieur", "tables", "reference_methode",
                                                  "statuts")),
    avertissements = !any(X$avertissements$etape == "coherence_regularite")))
})
verifier("G rapport figé : section etape_coherence_regularite entre etape_homogeneite et etape_validation, titre, trois mentions, racine ; absente du plan : « non demandée par le plan »", {
  h <- rawToChar(ecrire_rapport(X)); Encoding(h) <- "UTF-8"
  x0 <- construire_table(JEU, ROLES, plan_c(coherence = NULL))
  h0 <- rawToChar(ecrire_rapport(x0)); Encoding(h0) <- "UTF-8"
  m <- tablexp:::MENTIONS_RAPPORT; e <- tablexp:::echapper_html
  ids <- tablexp:::SECTIONS_RAPPORT$section
  k <- match("etape_coherence_regularite", ids)
  tous(list(ordre = identical(ids[k + c(-1L, 1L)], c("etape_homogeneite", "etape_validation")),
            titre = grepl(e(paste("Étape coherence_regularite : cohérence des taux et régularité de la table construite",
                                  "(Lignes directrices, chap. 4, II et III.1)")), h, fixed = TRUE),
            mentions = all(vapply(c("coherence_portee", "coherence_comparaison", "coherence_convention"), function(n)
              grepl(paste0("<p>", e(m[[n]]), "</p>"), h, fixed = TRUE), NA)),
            table = grepl("<caption><code>etapes/coherence_regularite/resultat/regularite</code>", h, fixed = TRUE),
            absente = grepl("<p>Étape « coherence_regularite » : non demandée par le plan", h0, fixed = TRUE),
            p36 = !grepl("p. 36", m[["coherence_comparaison"]], fixed = TRUE)))
})
