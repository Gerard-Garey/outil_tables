# -----------------------------------------------------------------------------
#  tests/unitaires/test_homogeneite.R : étape `homogeneite`, test
#  d'homogénéité entre segments (issue #168 ; E2.9, E2.12, forme M209 ;
#  périmètre M290 ; spécification d'actuary-approfondi, commentaire
#  6705873492 ; avis de forme (0''), F10, F11, M292 ; F11 confirmé, M294)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : petit jeu de cellules (T = 529/53, tranches
#    121/21 et 9/2), deux composantes connexes, constantes, catalogue, motifs
#    et ordre des refus, états, loi exacte par énumération directe ;
#  - valeur imprimée dans la source : exemple de Breslow et Day (1987), vol.
#    II, § 3.6 (g), p. 114-115 (T, attendus, variance, Pearson interne), à la
#    précision imprimée ;
#  - recoupement indépendant par `stats` (priorité base, en CI ; M290) : test
#    du score (Rao) d'un GLM de Poisson à offset, anova(..., test = "Rao"),
#    sous les quatre conditions de la spécification, § 7 (ajustement nul à
#    glm.control(epsilon = 1e-14, maxit = 100), plan de l'alternative écrit
#    de plein rang, strates à D_. = 0 et lignes à E = 0 retirées, écart
#    relatif 1e-7) ;
#  - identité exacte : localité de la graine (F11, M294) dans les deux sens,
#    égalité à l'appel direct sous la graine du plan, rejeu identical() hors
#    execution.
# -----------------------------------------------------------------------------

D <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}
# Appel direct de la fonction d'étape (graine 1L par défaut du test).
homog <- function(expo, segmentation, plage, tranches, B = 99, graine = 1L, par_annee = FALSE)
  tablexp:::calculer_homogeneite(expo, segmentation, par_annee, "dernier_anniversaire", plage, tranches, B, graine)
ligne_test <- function(r, test, variable = NA_character_, debut = NA_integer_) {
  t <- r$resultat$tests
  k <- which(t$test == test & (is.na(variable) | t$variable %in% variable) & (is.na(debut) | t$tranche_debut %in% debut))
  t[k, , drop = FALSE]
}

# --- A. Constantes et catalogue (valeur calculée à la main) -----------------------

verifier("A ETAPES : homogeneite après positionnement, avant coherence_regularite (#144) et validation (F10), module validation, calculer_homogeneite, préalables donnees et exposition, aléatoire (rang 6 depuis l'étape lissage, #223)", {
  e <- tablexp:::ETAPES
  k <- match("homogeneite", e$etape)
  identical(k, 6L) && identical(e$etape[k + c(-1L, 1L, 2L)], c("positionnement", "coherence_regularite", "validation")) &&
    identical(e$module[k], "validation") && identical(e$fonction[k], "calculer_homogeneite") &&
    identical(e$prealable[[k]], c("donnees", "exposition")) && identical(e$aleatoire[k], TRUE)
})
verifier("A PARAMETRES_ETAPES$homogeneite : plage_ages, tranches_age, replications (double, requis, sans défaut ; pas de niveau)", {
  p <- tablexp:::PARAMETRES_ETAPES$homogeneite
  identical(p$parametre, c("plage_ages", "tranches_age", "replications")) && identical(p$type, rep("double", 3L))
})
verifier("A états : tests, p exacte, segments (precision_insuffisante des tests et de la p exacte : M570, P1 (a), #334)", {
  identical(tablexp:::ETATS_TESTS_HOMOGENEITE,
            c("produite", "evenement_sans_exposition", "moins_de_deux_segments_informatifs", "precision_insuffisante")) &&
    identical(tablexp:::ETATS_P_EXACTE_HOMOGENEITE,
              c("produite", "plus_de_deux_groupes", "convolution_au_dela_n_max", "non_produite_par_cette_version",
                "evenement_sans_exposition", "moins_de_deux_segments_informatifs", "precision_insuffisante")) &&
    identical(tablexp:::ETATS_SEGMENTS_HOMOGENEITE,
              c("produite", "segment_sans_exposition", "evenement_sans_exposition", "variance_nulle"))
})
verifier("A STATUTS_HOMOGENEITE : (table, grandeur) uniques ; mention d'indépendance sur chaque loi (M278) ; O/A non nommé SMR ; aucune attribution d'auteur (M200)", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  loi <- paste(s$table, s$grandeur) %in% c("tests p_asymptotique", "tests p_exacte", "tests p_monte_carlo", "segments z")
  !anyDuplicated(paste(s$table, s$grandeur)) && all(nzchar(s$statut)) && sum(loi) == 4L &&
    all(grepl("unités d'observation indépendantes ; sous l'unité contrat, les décès sont sur-dispersés et le test est anti-conservateur",
              s$statut[loi], fixed = TRUE)) &&
    grepl("ce n'est pas un SMR", s$statut[s$grandeur == "rapport_observes_attendus"], fixed = TRUE) &&
    !any(grepl("Breslow|Day|Rao|Mantel|Haenszel|Cochran|Pearson", s$statut)) &&
    identical(Encoding(s$statut[1L]), "UTF-8")
})
verifier("A STATUTS_HOMOGENEITE : p exacte et p de Monte-Carlo asymptotiques sous le modèle de survie (Breslow et Day, § 4.2), après « E tenue pour fixe » ; statut terminé par la mention d'indépendance", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  st <- s$statut[s$table == "tests" & s$grandeur %in% c("p_exacte", "p_monte_carlo")]
  pos <- function(motif, x) regexpr(motif, x, fixed = TRUE)
  f <- pos("E tenue pour fixe", st); a <- pos("asymptotique sous le modèle de survie", st)
  length(st) == 2L && all(f > 0L) && all(a > f) &&
    all(endsWith(st, paste(";", tablexp:::INDEPENDANCE_HOMOGENEITE)))
})
# T185 (spécification (0') de #185, M362, option 1a) : régime asymptotique de
# p_asymptotique et de z, information du test croissante (plus petite valeur
# propre de V réduite -> infini), et non plus D_. -> infini dans chaque strate.
verifier("T185.1 STATUTS_HOMOGENEITE : p_asymptotique sous le régime de la plus petite valeur propre de V (texte de la spécification) ; z au même régime ; plus aucun « (D_. -> infini) »", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  st <- function(t, g) s$statut[s$table == t & s$grandeur == g]
  # Texte de #202 (texte (B) de la relecture (0')(c) d'actuary, commentaire 6775943400)
  p <- paste("asymptotique : loi du khi-deux à ddl degrés de liberté quand l'information du test croît (plus",
             "petite valeur propre de V réduite -> infini), à nombre de groupes fixé, le nombre de strates étant",
             "fixe ou croissant (D_. pouvant rester borné dans chaque strate quand le nombre de strates croît) ;",
             "sous H0 avec E tenue pour fixe ; asymptotique aussi sous le modèle de survie ; queue supérieure ;",
             tablexp:::INDEPENDANCE_HOMOGENEITE)
  identical(st("tests", "p_asymptotique"), p) &&
    grepl("de loi asymptotique N(0, 1) sous H0 (V_ss -> infini, même régime que p_asymptotique), E tenue pour fixe ;",
          st("segments", "z"), fixed = TRUE) &&
    sum(grepl("plus petite valeur propre de V", s$statut, fixed = TRUE)) == 1L &&
    !any(grepl("(D_. -> infini)", s$statut, fixed = TRUE))
})
# T202 (#202 ; relecture (0')(c) d'actuary) : la condition « aucune strate ne
# portant à elle seule une part non négligeable de V » est retirée (superflue, et
# impossible à nombre de strates fixe : la plus grande part vaut au moins 1/K) ;
# « D_. peut rester petit dans chaque strate » (faux à K fixe) est remplacé ; le
# nombre de groupes est dit fixé ; aucun numéro de théorème ni source citée (M388).
# Texte de z complété par #334 (spécification d'actuary-approfondi, Q3, § 4) :
# V_ss par sommes hors diagonale, U du groupe de référence.
verifier("T202 STATUTS_HOMOGENEITE : p_asymptotique sans condition sur les parts des strates, à nombre de groupes fixé, D_. borné seulement quand le nombre de strates croît ; z au texte de #334 ; aucune source citée", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  pa <- s$statut[s$table == "tests" & s$grandeur == "p_asymptotique"]
  z <- s$statut[s$table == "segments" & s$grandeur == "z"]
  length(pa) == 1L && length(z) == 1L &&
    grepl("à nombre de groupes fixé", pa, fixed = TRUE) &&
    grepl("quand le nombre de strates croît", pa, fixed = TRUE) &&
    !grepl("peut rester petit", pa, fixed = TRUE) &&
    !any(grepl("aucune strate ne portant|part non négligeable", s$statut)) &&
    !any(grepl("théorème|Billingsley", s$statut, ignore.case = TRUE)) &&
    identical(z, paste("z_s = (O_s - A_s) / sqrt(V_ss), écart réduit du segment au regroupé, de loi asymptotique N(0, 1) sous",
                       "H0 (V_ss -> infini, même régime que p_asymptotique), E tenue pour fixe ; V_ss = somme des w_sk (k",
                       "différent de s), sans soustraction ; U du groupe de référence de sa composante pris égal à moins la",
                       "somme des U des autres groupes (identité exacte) ; aucune p-valeur produite ; calcul en",
                       "arithmétique flottante, approximation numérique ; aucune correction de multiplicité ;",
                       tablexp:::INDEPENDANCE_HOMOGENEITE))
})
# Texte de statistique (#334 ; spécification d'actuary-approfondi, Q3, § 4,
# complété de l'état precision_insuffisante du test, M570, P1 (a)) : littéral.
verifier("I #334 STATUTS_HOMOGENEITE : statut de statistique au texte de la spécification (référence d'attendu A le plus grand, élimination sans soustraction, T = +Inf, état precision_insuffisante) ; plus de « système linéaire » ; statut de p_exacte : groupe 1 = groupe retenu", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  st <- s$statut[s$table == "tests" & s$grandeur == "statistique"]
  att <- paste("T = U' V^- U, U = O - A, V = somme sur les strates informatives de D_. (diag pi - pi pi') ; somme des T",
               "des tranches (ddl > 0) pour somme_tranches ; arithmétique flottante : par composante connexe du graphe",
               "groupes-strates, le groupe d'attendu A le plus grand (le plus petit indice à égalité) est la référence",
               "retirée ; V réduite factorisée par élimination sans soustraction (w_jk = somme sur les strates",
               "informatives de D_. pi_j pi_k, pivots formés de sommes de termes positifs), T = somme des y_i^2 / d_i, y",
               "obtenu de U des groupes retenus : approximation numérique ; T = +Inf quand la statistique dépasse le",
               "plus grand double ; état precision_insuffisante (T et p-valeurs NA) quand un pivot de l'élimination",
               "n'est pas fini et strictement positif")
  pe <- s$statut[s$table == "tests" & s$grandeur == "p_exacte"]
  frag <- paste("Bin(D_., pi_1), groupe 1 : le groupe retenu, d'attendu A le plus petit, la référence (attendu le plus",
                "grand, le plus petit indice à égalité) étant retirée comme pour statistique ; p = P(")
  tous(texte = if (identical(st, att)) TRUE else "texte de statistique",
       p_exacte = if (lengths(regmatches(pe, gregexpr(frag, pe, fixed = TRUE))) == 1L) TRUE
                  else "groupe retenu absent du statut de p_exacte (réserve d'actuary, Q1)",
       ancien = if (!any(grepl("système linéaire", s$statut, fixed = TRUE))) TRUE else "« système linéaire » présent",
       utf8 = if (identical(Encoding(st), "UTF-8")) TRUE else "encodage")
})
verifier("A REFERENCE_METHODE_HOMOGENEITE : données consignées de la source (URL, SHA-256, pages ; E2.25), après relecture sur l'image (M290)", {
  r <- tablexp:::REFERENCE_METHODE_HOMOGENEITE
  grepl("[0-9a-f]{64}", r) && grepl("https://", r, fixed = TRUE) && !grepl("à compléter", r, fixed = TRUE) &&
    identical(Encoding(r), "UTF-8")
})

# --- B. Refus (valeur calculée à la main : module, motif, ordre) -------------------

PERIODE <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
HOM <- list(plage_ages = c(20, 90), tranches_age = c(20, 50, 70), replications = 99)
plan_h <- function(segmentation = "sexe", convention = "plus_fine", par_annee = FALSE, hom = HOM, val = NULL,
                   graine = 20261106L) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PERIODE, delai_declaration = 0,
            segmentation = segmentation, graine = graine,
            etapes = list(donnees = list(exclusions = character(0)),
                          exposition = list(convention = convention, par_annee = par_annee)))
  if (!is.null(hom)) p$etapes$homogeneite <- hom
  if (!is.null(val)) p$etapes$validation <- val
  if (is.null(hom) && is.null(val)) p$graine <- NULL
  p
}
verifier("refus valider_plan : segmentation vide -> homogeneite_sans_segmentation (module homogeneite), message sans valeur reçue", {
  r <- refus_de(valider_plan(plan_h(segmentation = character(0))))
  ok <- controle(r, "homogeneite", "homogeneite_sans_segmentation")
  if (isTRUE(ok) && !identical(r$details$segmentation, character(0))) "details" else ok
})
for (cas in list(list("plage_ages", c(20L, 90L), "plage_ages_invalide"), list("plage_ages", c(90, 20), "plage_ages_invalide"),
                 list("plage_ages", 20, "plage_ages_invalide"), list("tranches_age", c(20L, 50L), "tranches_age_invalides"),
                 list("tranches_age", c(30, 50), "tranches_age_invalides"),
                 list("tranches_age", c(20, 50, 50), "tranches_age_invalides"),
                 list("tranches_age", c(20, 95), "tranches_age_invalides"),
                 list("replications", 99L, "replications_invalides"), list("replications", 0, "replications_invalides"),
                 list("replications", 9.5, "replications_invalides"), list("replications", c(a = 99), "replications_invalides")))
  verifier(sprintf("refus valider_plan : %s = %s -> %s (module homogeneite)", cas[[1L]],
                   paste(deparse(cas[[2L]]), collapse = ""), cas[[3L]]),
           controle(refus_de(valider_plan({ p <- plan_h(); p$etapes$homogeneite[cas[[1L]]] <- list(cas[[2L]]); p })),
                    "homogeneite", cas[[3L]]))
verifier("refus valider_plan : ordre plage_ages, tranches_age, replications, puis homogeneite_sans_segmentation", {
  p <- plan_h(segmentation = character(0), hom = list(plage_ages = 20, tranches_age = 1L, replications = 0))
  r1 <- controle(refus_de(valider_plan(p)), "homogeneite", "plage_ages_invalide")
  p$etapes$homogeneite$plage_ages <- c(20, 90)
  r2 <- controle(refus_de(valider_plan(p)), "homogeneite", "tranches_age_invalides")
  p$etapes$homogeneite$tranches_age <- 20
  r3 <- controle(refus_de(valider_plan(p)), "homogeneite", "replications_invalides")
  p$etapes$homogeneite$replications <- 9
  r4 <- controle(refus_de(valider_plan(p)), "homogeneite", "homogeneite_sans_segmentation")
  tous(r1 = r1, r2 = r2, r3 = r3, r4 = r4)
})
verifier("refus valider_plan : paramètre replications absent -> parametre_manquant (aucun défaut)",
         controle(refus_de(valider_plan({ p <- plan_h(); p$etapes$homogeneite$replications <- NULL; p })),
                  "construire_table", "parametre_manquant"))
verifier("refus valider_plan : étape homogeneite sans graine -> graine_manquante (étape aléatoire)",
         controle(refus_de(valider_plan({ p <- plan_h(); p$graine <- NULL; p })), "construire_table", "graine_manquante"))
verifier("refus valider_plan : homogeneite après validation -> etapes_hors_ordre", {
  p <- plan_h(val = list(table_reference = "tv", relation_table = "independante", plage_ages = c(20, 90),
                         tranches_age = c(20, 50), niveau = 0.95, replications = 9))
  p$etapes <- p$etapes[c("donnees", "exposition", "validation", "homogeneite")]
  controle(refus_de(valider_plan(p)), "construire_table", "etapes_hors_ordre")
})
# Briques extraites de controler_parametres_validation() : messages de
# `validation` inchangés (texte d'avant #168, recopié ici).
verifier("refus de validation inchangés par l'extraction (tranches_age_invalides, replications_invalides : module, motif, message)", {
  rt <- refus_de(tablexp:::controler_parametres_validation("t", "independante", c(20, 90), c(30, 50), 0.95, 9, NULL))
  rr <- refus_de(tablexp:::controler_parametres_validation("t", "independante", c(20, 90), c(20, 50), 0.95, 9L, NULL))
  mt <- paste("Les tranches d'âge (tranches_age) doivent être les bornes inférieures, vecteur double sans",
              "attribut de valeurs entières strictement croissantes, la première égale au minimum de",
              "plage_ages, la dernière au plus égale à son maximum (un integer est refusé).")
  mr <- paste("Le nombre de réplications de Monte-Carlo (replications) doit être un nombre double unique,",
              "sans attribut, entier, au moins 1 et au plus 2147483647, sans défaut (un integer est",
              "refusé).")
  tous(t = controle(rt, "validation", "tranches_age_invalides"), r = controle(rr, "validation", "replications_invalides"),
       mt = if (identical(conditionMessage(rt), mt)) TRUE else conditionMessage(rt),
       mr = if (identical(conditionMessage(rr), mr)) TRUE else conditionMessage(rr),
       dt = if (identical(rt$details, list(tranches_age = c(30, 50)))) TRUE else "details tranches")
})
verifier("briques de contrôle : étape inconnue -> erreur de programmation (non un refus)", {
  e <- function(expr) { r <- tryCatch({ force(expr); NULL }, error = function(e) e)
    !is.null(r) && !inherits(r, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(r), fixed = TRUE) }
  e(tablexp:::controler_tranches_age(20, c(20, 90), "brut")) && e(tablexp:::controler_replications(9, "brut")) &&
    e(tablexp:::controler_plage_ages(c(20, 90), "brut"))
})

# --- C. Petit jeu de cellules (valeur calculée à la main) --------------------------
# Segments F et H, âges 50 à 54, plage 50 à 54, tranches 50-51 et 52-54.
#   âge 50 : E = (100, 100), D = (3, 0) ; âge 51 : E = (100, 300), D = (2, 1) ;
#   âge 52 : E = (200, 200), D = (7, 1) ; âge 53 : E = (100, 100), D = (0, 0)
#   (cellule sans décès) ; âge 54 : F seule, E = 50, D = 2 (non informative).
# pi_F = (1/2, 1/4, 1/2) sur les cellules informatives, D_. = (3, 3, 8) :
# A_F = 3/2 + 3/4 + 4 = 25/4, O_F = 12 ; U_F = 23/4 ; V_FF = 3/4 + 9/16 + 2 =
# 53/16 ; T = U^2 / V = 529/53. Tranche 50-51 : U = 11/4, V = 21/16, T = 121/21 ;
# tranche 52-54 : U = 3, V = 2, T = 9/2. Âge 54 : O = A = 2 (pi = 1).
EXPO_C <- data.frame(sexe = c("F", "F", "F", "F", "F", "H", "H", "H", "H"),
                     age = c(50:54, 50:53), exposition_centrale = c(100, 100, 200, 100, 50, 100, 300, 200, 100),
                     evenements = c(3L, 2L, 7L, 0L, 2L, 0L, 1L, 1L, 0L), stringsAsFactors = FALSE)
RC <- homog(EXPO_C, "sexe", c(50, 54), c(50, 52))
verifier("C : tests global, tranches, somme ; T = 529/53, 121/21, 9/2 et leur somme ; ddl 1, 1, 1, 2 ; p asymptotique pchisq", {
  t <- RC$resultat$tests
  tous(lignes = if (identical(t$test, c("global", "tranche", "tranche", "somme_tranches")) &&
                    identical(t$tranche_debut, c(NA, 50L, 52L, NA)) && identical(t$tranche_fin, c(NA, 51L, 54L, NA)))
                  TRUE else "lignes",
       t = proche(t$statistique, c(529 / 53, 121 / 21, 9 / 2, 121 / 21 + 9 / 2), 1e-14, 0),
       ddl = if (identical(t$ddl, c(1L, 1L, 1L, 2L))) TRUE else "ddl",
       inf = if (identical(t$strates_informatives, c(3L, 2L, 1L, 3L)) && identical(t$groupes, c(2L, 2L, 2L, NA)))
               TRUE else "strates ou groupes",
       pa = proche(t$p_asymptotique, stats::pchisq(t$statistique, t$ddl, lower.tail = FALSE), 1e-14, 0),
       etat = if (all(t$etat == "produite")) TRUE else "état")
})
# Loi exacte par énumération directe (indépendante de loi_somme_binomiales()).
loi_directe <- function(n, p) {
  loi <- 1
  for (s in seq_along(n)) {
    d <- stats::dbinom(0:n[s], n[s], p[s])
    m <- outer(loi, d)
    loi <- as.numeric(tapply(m, row(m) + col(m) - 2L, sum))
  }
  loi
}
verifier("C : p exacte = P(|O_F - A_F| >= |o_F - A_F|) par énumération directe ; somme_tranches : non_produite_par_cette_version", {
  t <- RC$resultat$tests
  pe <- function(n, p, o, a) { l <- loi_directe(n, p); k <- seq_along(l) - 1; sum(l[abs(k - a) >= abs(o - a)]) }
  tous(g = proche(t$p_exacte[1L], pe(c(3, 3, 8), c(0.5, 0.25, 0.5), 12, 25 / 4), 1e-13, 0),
       t1 = proche(t$p_exacte[2L], pe(c(3, 3), c(0.5, 0.25), 5, 9 / 4), 1e-13, 0),
       t2 = proche(t$p_exacte[3L], pe(8, 0.5, 7, 4), 1e-13, 0),
       s = if (is.na(t$p_exacte[4L]) && identical(t$etat_p_exacte, c(rep("produite", 3L), "non_produite_par_cette_version")))
             TRUE else "somme")
})
verifier("C : segments, portée globale : O = (14, 2), A = (33/4, 31/4) (âge 54 compris, O = A), z = -+ U / sqrt(V), O/A", {
  s <- RC$resultat$segments
  g <- s[s$portee == "global", ]
  tous(o = proche(g$observes, c(14, 2), 0, 0), a = proche(g$attendus, c(33 / 4, 31 / 4), 1e-14, 0),
       e = proche(g$exposition_centrale, c(550, 700), 1e-14, 0),
       oa = proche(g$rapport_observes_attendus, c(14 / (33 / 4), 2 / (31 / 4)), 1e-14, 0),
       z = proche(g$z, c(23 / 4, -23 / 4) / sqrt(53 / 16), 1e-14, 0),
       etat = if (all(g$etat == "produite") && identical(g$sexe, c("F", "H"))) TRUE else "état")
})
verifier("C : segments par tranche : z = U / sqrt(V) de chaque tranche ; colonnes", {
  s <- RC$resultat$segments
  identical(names(s), c("portee", "sexe", "tranche_debut", "tranche_fin", "exposition_centrale", "observes", "attendus",
                        "rapport_observes_attendus", "z", "etat")) &&
    isTRUE(proche(s$z[s$portee == "tranche"], c(11 / 4 / sqrt(21 / 16), -11 / 4 / sqrt(21 / 16), 3 / sqrt(2), -3 / sqrt(2)),
                  1e-14, 0))
})
verifier("C : diagnostics : 5 cellules, 3 informatives, 1 sans décès, 1 non informative, 0 événement sans exposition", {
  d <- RC$diagnostics
  identical(d$cellules, 5L) && identical(d$cellules_informatives, 3L) && identical(d$cellules_sans_deces, 1L) &&
    identical(d$cellules_non_informatives, 1L) && identical(d$cellules_evenement_sans_exposition, 0L) &&
    identical(d$evenements_total, 16L) && identical(d$evenements_hors_plage, 0L)
})
verifier("C : apport nul (exact) des cellules sans décès et non informatives : T identical() sans elles", {
  r <- homog(EXPO_C[EXPO_C$age <= 52, ], "sexe", c(50, 52), c(50, 52))
  identical(r$resultat$tests$statistique, RC$resultat$tests$statistique)
})
verifier("C : Monte-Carlo : p dans [1/(B+1), 1], e = sqrt(B p (1 - p)) / (B + 1), même graine -> identical(), un jeu tiré", {
  t <- RC$resultat$tests
  B <- 99
  r2 <- homog(EXPO_C, "sexe", c(50, 54), c(50, 52))
  all(t$p_monte_carlo >= 1 / (B + 1) & t$p_monte_carlo <= 1) &&
    isTRUE(proche(t$erreur_monte_carlo, sqrt(B * t$p_monte_carlo * (1 - t$p_monte_carlo)) / (B + 1), 1e-14, 0)) &&
    identical(r2, RC) &&
    identical(RC$resultat$monte_carlo$jeux, data.frame(jeu = "cellules", tirages_effectues = 99L, stringsAsFactors = FALSE))
})
verifier("C : statistique de Monte-Carlo : la statistique d'une réplique égale aux décès observés vaut T observé (même fonction)", {
  sc <- tablexp:::score_homogeneite(EXPO_C$evenements, EXPO_C$exposition_centrale, EXPO_C$age - 49L,
                                    match(EXPO_C$sexe, c("F", "H")), 5L, 2L)
  Du <- EXPO_C$evenements[sc$utilisee]
  identical(unname(sc$statistique(cbind(Du, Du))), rep(sc$t, 2L)) && isTRUE(proche(sc$t, 529 / 53, 1e-14, 0))
})

# Ancre sur la source (valeur imprimée) : Breslow et Day (1987), vol. II,
# chap. 3, § 3.6 (g), p. 114-115, lu sur l'image. Deux strates (cellules 1
# et 2), deux groupes (segments 1 non exposés, 2 exposés) : strate I, d =
# (25, 5), n = (10 000, 1 000) ; strate II, d = (5, 25), n = (4 000,
# 10 000). Imprimé : attendus 35.844 (non exposés) et 24.156 (exposés),
# Var = 8.602, chi2_1 = (30 - 24.156)^2 / 8.602 = 3.97 (p. 115), Pearson
# interne 2.37 ; comparés à la précision imprimée (2 décimales pour les
# statistiques, 3 pour A et V). Le Pearson interne, Somme (O - A)^2 / A,
# n'est pas produit par le moteur : il est calculé ici à partir de O et A.
# Étiquetage : les lignes « E2 » et « E1 » de la p. 114 portent, par le
# calcul, les attendus des non-exposés et des exposés, à l'inverse de la
# convention du § 3.6 (a), p. 108 (O_2 aux exposés), et de la formule
# (O_2 E_1) / (O_1 E_2) = 1.48 de la p. 115 : incohérence d'étiquetage
# interne à la source ; le test suit les colonnes, et à deux groupes la
# variance ne dépend pas de l'indice. La p. 115 imprime p = 0.13 et p =
# 0.047 ; la queue supérieure du khi-deux à 1 ddl des statistiques imprimées
# elles-mêmes (2.37 ; 3.97) vaut 0.1237 et 0.0463, et ni la correction de
# continuité ni la queue normale unilatérale ne redonnent les valeurs
# imprimées : écart interne à la source, p-valeurs imprimées non retenues
# comme ancre.
verifier("C ancre (Breslow et Day, § 3.6 (g)) : A = (35.844, 24.156), V = 8.602, T = 3.97, Pearson interne 2.37, ddl 1, à la précision imprimée", {
  sc <- tablexp:::score_homogeneite(c(25, 5, 5, 25), c(10000, 1000, 4000, 10000), c(1L, 1L, 2L, 2L), c(1L, 2L, 1L, 2L),
                                    2L, 2L)
  pearson <- sum((sc$O - sc$A)^2 / sc$A)
  tous(o = if (identical(sc$O, c(30, 30))) TRUE else "O",
       a = if (identical(round(sc$A, 3), c(35.844, 24.156))) TRUE else paste(format(sc$A, digits = 10), collapse = " "),
       v = if (identical(round(sc$v, 3), c(8.602, 8.602))) TRUE else paste(format(sc$v, digits = 10), collapse = " "),
       t = if (identical(round(sc$t, 2), 3.97)) TRUE else format(sc$t, digits = 10),
       pearson = if (identical(round(pearson, 2), 2.37)) TRUE else format(pearson, digits = 10),
       ddl = if (identical(sc$ddl, 1L) && identical(sc$strates_informatives, 2L)) TRUE else "ddl ou strates")
})

# Jeu de cellules sexe x segment (3 modalités), âges 40 à 69, tiré sous une
# graine locale (expositions non dyadiques) ; employé ici et en D.
EXPO_D <- tablexp:::sous_graine(20261109L, {
  g <- expand.grid(age = 40:69, segment = c("a", "b", "c"), sexe = c("F", "H"), stringsAsFactors = FALSE)
  g <- g[, c("sexe", "segment", "age")]
  g$exposition_centrale <- stats::runif(nrow(g), 300, 3000)
  f <- c(F = 0.7, H = 1)[g$sexe] * c(a = 1, b = 1.15, c = 0.9)[g$segment]
  g$evenements <- stats::rpois(nrow(g), g$exposition_centrale * 0.002 * exp(0.08 * (g$age - 40)) * f)
  g
})
EXPO_D_A <- EXPO_D[EXPO_D$segment == "a", c("sexe", "age", "exposition_centrale", "evenements")]

# Ex aequo du Monte-Carlo (reprise de l'audit, C1, C2, T1) : un jeu qui garde
# les totaux par strate et les O par groupe (décès déplacés entre cellules,
# dans une même tranche) a le même T au bit près, pour le global, la tranche
# et la somme des tranches. Expositions non dyadiques (jeu D ci-dessous,
# segment a, groupes F et H) : les arrondis de A ne s'annulent pas.
verifier("C ex aequo (T1) : jeu de mêmes totaux par strate et mêmes O par groupe -> T global, de tranche et somme_tranches identical() au T observé", {
  e <- EXPO_D_A
  tr <- findInterval(e$age, c(40, 50, 60))
  a1 <- which(e$sexe == "F" & e$evenements > 0 & tr == 1L)[1L]
  a2 <- which(e$sexe == "H" & e$evenements > 0 & tr == 1L & e$age != e$age[a1])[1L]
  b1 <- which(e$sexe == "H" & e$age == e$age[a1]); b2 <- which(e$sexe == "F" & e$age == e$age[a2])
  e2 <- e
  e2$evenements[c(a1, b1, a2, b2)] <- e$evenements[c(a1, b1, a2, b2)] + c(-1L, 1L, -1L, 1L)
  r1 <- homog(e, "sexe", c(40, 69), c(40, 50, 60)); r2 <- homog(e2, "sexe", c(40, 69), c(40, 50, 60))
  sc <- tablexp:::score_homogeneite(e$evenements, e$exposition_centrale, e$age - 39L, match(e$sexe, c("F", "H")), 30L, 2L)
  Dm <- cbind(e$evenements[sc$utilisee], e2$evenements[sc$utilisee])
  tous(deplace = if (!identical(e$evenements, e2$evenements)) TRUE else "aucun déplacement",
       direct = if (identical(unname(sc$statistique(Dm)), rep(sc$t, 2L))) TRUE else "statistique() des deux jeux",
       tests = if (identical(r1$resultat$tests$statistique, r2$resultat$tests$statistique)) TRUE
               else paste(format(r1$resultat$tests$statistique - r2$resultat$tests$statistique), collapse = " "))
})
# Même exigence sur des répliques tirées (C1) : les répliques de même O_F
# ont le même T au bit près, et celles de O_F = o_F le T observé. Mesuré sur
# l'ancienne forme rowsum(Dm - Au) : 28 valeurs de O_F sur 36 portaient
# plusieurs T (20 000 répliques, tranche 40-49) ; les déplacements isolés du
# test précédent n'y suffisaient pas à révéler l'écart.
verifier("C ex aequo (C1) : 2 000 répliques multinomiales, même O_F -> même T au bit près ; O_F = o_F -> T observé", {
  e <- EXPO_D_A[EXPO_D_A$age < 50, ]
  g <- match(e$sexe, c("F", "H"))
  sc <- tablexp:::score_homogeneite(e$evenements, e$exposition_centrale, e$age - 39L, g, 10L, 2L)
  f <- g[sc$utilisee] == 1L
  r <- tablexp:::replications_multinomiales(sc$poids, sc$strate_tirage, sc$effectifs, 2000, 20261112L,
         function(Dm) list(t = sc$statistique(Dm), o = colSums(Dm[f, , drop = FALSE])))
  n <- tapply(r$t, r$o, function(v) length(unique(v)))
  o_obs <- sum(e$evenements[sc$utilisee][f])
  tous(groupes = if (length(n) > 10L && all(n == 1L)) TRUE else sprintf("%d valeurs de O_F à plusieurs T", sum(n > 1L)),
       observe = if (any(r$o == o_obs) && all(r$t[r$o == o_obs] == sc$t)) TRUE else "T observé")
})
verifier("C somme_tranches (C2) : T observé = addition séquentielle en double, à partir de 0, des T de tranche (forme des répliques)", {
  t <- homog(EXPO_D_A, "sexe", c(40, 69), c(40, 50, 60))$resultat$tests
  s_ <- 0
  for (v in t$statistique[t$test == "tranche"]) s_ <- s_ + v
  identical(t$statistique[t$test == "somme_tranches"], s_)
})
# Concordance à deux groupes (audit) : avec B grand, la p de Monte-Carlo
# concorde avec la p exacte à quelques e_MC près (loi discrète : tranche
# 52-54, Bin(8 ; 1/2)). Constat de simulation sous une graine fixée,
# tolérance 4 e_MC + 1/(B + 1) (biais de (1 + K)/(B + 1)). Contrôle de
# cohérence seulement, pas une garde de C1 : les expositions d'EXPO_C sont
# dyadiques (pi = 1/2, 1/4), les ex aequo y sont exacts sous l'ancienne forme
# aussi (mesuré par l'audit) ; C1 est gardé par le test « C ex aequo (C1) ».
verifier("C concordance (B = 19 999) : |p_MC - p_exacte| <= 4 e_MC + 1/(B + 1), global et tranches", {
  B <- 19999
  t <- homog(EXPO_C, "sexe", c(50, 54), c(50, 52), B = B, graine = 20261111L)$resultat$tests[1:3, ]
  ec <- abs(t$p_monte_carlo - t$p_exacte)
  if (all(ec <= 4 * t$erreur_monte_carlo + 1 / (B + 1))) TRUE
  else paste("écarts", paste(format(ec), collapse = " "), "; e", paste(format(t$erreur_monte_carlo), collapse = " "))
})

# --- D. Recoupement par le test du score d'un GLM de Poisson à offset (M290) ------
# Jeu EXPO_D (section C) ; tranches 40, 50, 60. Conditions de la
# spécification, § 7.

RD <- homog(EXPO_D, c("sexe", "segment"), c(40, 69), c(40, 50, 60))
CTL <- stats::glm.control(epsilon = 1e-14, maxit = 100)
# Test de Rao entre l'ajustement nul (strates) et l'alternative (strates + X,
# X matrice de plein rang écrite explicitement) ; strates à D_. = 0 et lignes
# à E = 0 retirées. Rend c(Rao, Df).
rao <- function(d, strate, X) {
  Ds <- tapply(d$evenements, strate, sum)
  k <- d$exposition_centrale > 0 & strate %in% names(Ds)[Ds > 0]
  d <- d[k, ]; st <- factor(strate[k]); X <- X[k, , drop = FALSE]
  m0 <- stats::glm(d$evenements ~ st, family = stats::poisson(), offset = log(d$exposition_centrale), control = CTL)
  m1 <- stats::glm(d$evenements ~ st + X, family = stats::poisson(), offset = log(d$exposition_centrale), control = CTL)
  a <- stats::anova(m0, m1, test = "Rao")
  c(a$Rao[2L], a$Df[2L])
}
indicatrices <- function(f, garder = -1L) {
  lv <- sort(unique(f))
  sapply(lv[garder], function(l) as.numeric(f == l))
}
GRP_D <- paste(EXPO_D$sexe, EXPO_D$segment)
TR_D <- findInterval(EXPO_D$age, c(40, 50, 60))
verifier("D global (S = 6) : T et ddl = Rao et Df de glm (D ~ cellule + segment), écart relatif <= 1e-7", {
  r <- rao(EXPO_D, EXPO_D$age, indicatrices(GRP_D))
  t <- ligne_test(RD, "global")
  tous(t = proche(t$statistique, r[1L], 1e-7, 0), ddl = if (identical(t$ddl, as.integer(r[2L]))) TRUE else "ddl",
       p = proche(t$p_asymptotique, stats::pchisq(r[1L], r[2L], lower.tail = FALSE), 1e-6, 0),
       pe = if (is.na(t$p_exacte) && identical(t$etat_p_exacte, "plus_de_deux_groupes")) TRUE else "p exacte")
})
verifier("D somme_tranches : T = Rao de l'alternative segment x tranche (plan explicite de plein rang), Df = somme des ddl", {
  X <- do.call(cbind, lapply(1:3, function(b) indicatrices(GRP_D) * (TR_D == b)))
  r <- rao(EXPO_D, EXPO_D$age, X)
  t <- ligne_test(RD, "somme_tranches")
  tous(t = proche(t$statistique, r[1L], 1e-7, 0), ddl = if (identical(t$ddl, as.integer(r[2L])) && t$ddl == 15L) TRUE else "ddl",
       somme = proche(t$statistique, sum(ligne_test(RD, "tranche")$statistique), 1e-14, 0))
})
for (b in 1:3)
  verifier(sprintf("D tranche %d : T = Rao de glm restreint aux cellules de la tranche", b), {
    k <- TR_D == b
    r <- rao(EXPO_D[k, ], EXPO_D$age[k], indicatrices(GRP_D[k]))
    t <- ligne_test(RD, "tranche", debut = c(40L, 50L, 60L)[b])
    tous(t = proche(t$statistique, r[1L], 1e-7, 0), ddl = if (identical(t$ddl, as.integer(r[2L]))) TRUE else "ddl")
  })
verifier("D variable sexe | segment : T = Rao de glm (strates cellule x segment, alternative + sexe), deux groupes : p exacte produite", {
  r <- rao(EXPO_D, paste(EXPO_D$age, EXPO_D$segment), indicatrices(EXPO_D$sexe))
  t <- ligne_test(RD, "variable", "sexe")
  tous(t = proche(t$statistique, r[1L], 1e-7, 0), ddl = if (identical(t$ddl, 1L)) TRUE else "ddl",
       cond = if (identical(t$conditionnellement_a, "segment") && identical(t$etat_p_exacte, "produite") &&
                  t$p_exacte > 0 && t$p_exacte <= 1) TRUE else "p exacte")
})
verifier("D variable segment | sexe : T = Rao de glm (strates cellule x sexe, alternative + segment), ddl 2, plus_de_deux_groupes", {
  r <- rao(EXPO_D, paste(EXPO_D$age, EXPO_D$sexe), indicatrices(EXPO_D$segment))
  t <- ligne_test(RD, "variable", "segment")
  tous(t = proche(t$statistique, r[1L], 1e-7, 0), ddl = if (identical(t$ddl, 2L)) TRUE else "ddl",
       pe = if (identical(t$etat_p_exacte, "plus_de_deux_groupes")) TRUE else "p exacte")
})
verifier("D Monte-Carlo : trois jeux (cellules, variable_sexe, variable_segment), B tirages chacun ; p de Monte-Carlo de chaque test produit", {
  j <- RD$resultat$monte_carlo$jeux
  identical(j$jeu, c("cellules", "variable_sexe", "variable_segment")) && identical(j$tirages_effectues, rep(99L, 3L)) &&
    !anyNA(RD$resultat$tests$p_monte_carlo)
})
verifier("D sous H0 (facteurs égaux) : T global = Rao à 1e-7 (spécification : pire écart mesuré sous H0)", {
  e <- tablexp:::sous_graine(20261110L, { g <- EXPO_D
    g$evenements <- stats::rpois(nrow(g), g$exposition_centrale * 0.002 * exp(0.08 * (g$age - 40))); g })
  r0 <- homog(e, c("sexe", "segment"), c(40, 69), c(40, 50, 60))
  r <- rao(e, e$age, indicatrices(paste(e$sexe, e$segment)))
  proche(ligne_test(r0, "global")$statistique, r[1L], 1e-7, 0)
})

# --- E. Deux composantes connexes (valeur calculée à la main) ----------------------
# Segments a, b aux âges 50 et 51 ; c, d aux âges 52 et 53 ; aucune cellule
# commune : rang de V = 4 - 2 = 2 ; T = T_ab + T_cd (formes closes à deux
# groupes).
EXPO_E <- data.frame(segment = rep(c("a", "b", "c", "d"), each = 2L), age = c(50, 51, 50, 51, 52, 53, 52, 53),
                     exposition_centrale = c(100, 200, 300, 200, 100, 100, 100, 300),
                     evenements = c(4L, 1L, 2L, 3L, 5L, 2L, 1L, 6L), stringsAsFactors = FALSE)
EXPO_E$age <- as.integer(EXPO_E$age)
verifier("E deux composantes : ddl = 2, groupes = 4, T = somme des formes closes, p exacte plus_de_deux_groupes", {
  r <- homog(EXPO_E, "segment", c(50, 53), 50)
  t <- ligne_test(r, "global")
  ferme <- function(o, n, p) (sum(o) - sum(n * p))^2 / sum(n * p * (1 - p))
  tab <- ferme(c(4, 1), c(6, 4), c(1 / 4, 1 / 2)); tcd <- ferme(c(5, 2), c(6, 8), c(1 / 2, 1 / 4))
  tous(ddl = if (identical(t$ddl, 2L) && identical(t$groupes, 4L)) TRUE else "ddl ou groupes",
       t = proche(t$statistique, tab + tcd, 1e-14, 0),
       pe = if (identical(t$etat_p_exacte, "plus_de_deux_groupes")) TRUE else t$etat_p_exacte)
})
# Écart minimal détectable par composante (#218, correction du 19/11/2026 ;
# M1 de l'audit) : chaque segment est la référence de sa composante C(s),
# lambda_s ne porte que sur l'autre segment de sa composante, lambda_80 à
# 2 ddl (ddl du test global). Attendus : recalcul indépendant d'audit à deux
# segments par composante (a : 8.5616380313444296 ; le moteur rend
# 8.5616380313444367, écart 8,3e-16 ; b, c, d identiques au bit près). Un
# calcul sur tous les segments présents au lieu de C(s) échoue.
verifier("E deux composantes, écart minimal détectable (recalcul indépendant d'audit, deux segments par composante) : a theta+, b theta-, c theta+, d theta- produites à 1e-12 ; autres branches non_atteint", {
  em <- homog(EXPO_E, "segment", c(50, 53), 50)$resultat$ecart_minimal
  tous(segments = if (identical(em$segment, c("a", "b", "c", "d"))) TRUE else "segments",
       etats = if (identical(em$etat_superieur, c("produite", "non_atteint", "produite", "non_atteint")) &&
                   identical(em$etat_inferieur, c("non_atteint", "produite", "non_atteint", "produite"))) TRUE
               else paste(c(em$etat_superieur, em$etat_inferieur), collapse = "/"),
       valeurs = proche(c(em$theta_80_superieur[c(1L, 3L)], em$theta_80_inferieur[c(2L, 4L)]),
                        c(8.5616380313444296, 5.7680030155899793, 0.11680007918332541, 0.17337022836103966), 1e-12, 0),
       na = if (all(is.na(c(em$theta_80_superieur[c(2L, 4L)], em$theta_80_inferieur[c(1L, 3L)])))) TRUE else "NA")
})

# --- F. Cas aux limites (valeur calculée à la main : états, décomptes) -------------

verifier("F segment sans exposition sur la plage : hors du test, ligne O = A = 0, état segment_sans_exposition ; tests inchangés", {
  e <- rbind(EXPO_C, data.frame(sexe = "X", age = 40L, exposition_centrale = 500, evenements = 3L))
  r <- homog(e, "sexe", c(50, 54), c(50, 52))
  g <- r$resultat$segments[r$resultat$segments$portee == "global", ]
  tous(t = proche(r$resultat$tests$statistique, RC$resultat$tests$statistique, 1e-15, 0),
       seg = if (identical(g$sexe, c("F", "H", "X")) && identical(g$etat[3L], "segment_sans_exposition") &&
                 g$observes[3L] == 0 && g$attendus[3L] == 0 && is.na(g$z[3L]) && is.na(g$rapport_observes_attendus[3L]))
               TRUE else "ligne X",
       hors = if (identical(r$diagnostics$evenements_hors_plage, 3L) && identical(r$diagnostics$lignes_exposition_hors_plage, 1L))
                TRUE else "hors plage",
       groupes = if (identical(r$resultat$tests$groupes[1L], 2L)) TRUE else "groupes")
})
verifier("F moins de deux segments informatifs : tests NA, état moins_de_deux_segments_informatifs, ddl 0, aucun tirage ; z variance_nulle", {
  e <- EXPO_C[EXPO_C$sexe == "F", ]
  e <- rbind(e, data.frame(sexe = "H", age = 53L, exposition_centrale = 100, evenements = 0L))
  r <- homog(e, "sexe", c(50, 54), c(50, 52))
  t <- r$resultat$tests
  all(t$etat == "moins_de_deux_segments_informatifs") && all(is.na(t$statistique)) && all(is.na(t$p_monte_carlo)) &&
    identical(t$ddl, c(0L, 0L, 0L, 0L)) && identical(t$etat_p_exacte, t$etat) &&
    identical(r$resultat$monte_carlo$jeux$tirages_effectues, 0L) &&
    all(r$resultat$segments$etat[r$resultat$segments$sexe == "F"] == "variance_nulle")
})
verifier("F événement sans exposition (âge 52, tranche 52-54) : global, tranche 52-54 et somme non produits ; tranche 50-51 produite et identique ; avertissement ; identité des décomptes", {
  e <- rbind(EXPO_C, data.frame(sexe = "X", age = 52L, exposition_centrale = 0, evenements = 2L))
  r <- homog(e, "sexe", c(50, 54), c(50, 52))
  t <- r$resultat$tests
  s <- r$resultat$segments
  d <- r$diagnostics
  tous(etat = if (identical(t$etat, c("evenement_sans_exposition", "produite", "evenement_sans_exposition",
                                      "evenement_sans_exposition"))) TRUE else paste(t$etat, collapse = ", "),
       ddl = if (identical(t$ddl, c(NA, 1L, NA, NA))) TRUE else "ddl",
       t1 = proche(t$statistique[2L], 121 / 21, 1e-14, 0),
       na = if (all(is.na(t$statistique[-2L])) && all(is.na(t$p_monte_carlo[-2L]))) TRUE else "valeurs",
       # Segment X, d'exposition nulle avec des décès : evenement_sans_exposition
       # en priorité (m1) ; sans ligne dans la tranche 50-51 :
       # segment_sans_exposition.
       seg = if (identical(s$etat[s$portee == "global"], rep("evenement_sans_exposition", 3L)) &&
                 identical(s$etat[s$portee == "tranche" & s$tranche_debut == 52L], rep("evenement_sans_exposition", 3L)) &&
                 identical(s$etat[s$portee == "tranche" & s$tranche_debut == 50L],
                           c("produite", "produite", "segment_sans_exposition")))
               TRUE else paste(s$etat, collapse = ", "),
       diag = if (identical(d$cellules_evenement_sans_exposition, 1L) &&
                  identical(d$evenements_cellules_evenement_sans_exposition, 10L) &&
                  sum(s$observes[s$portee == "global"]) + 10 == d$evenements_total) TRUE else "diagnostics",
       avert = if (identical(r$avertissements$code, "evenements_sans_exposition") &&
                   identical(r$avertissements$etape, "homogeneite") && identical(r$avertissements$nombre, 1L))
                 TRUE else "avertissement")
})
verifier("F segmentation à une variable : aucun test par variable (le test global en tient lieu)",
         !("variable" %in% RC$resultat$tests$test))
verifier("F variable à une seule modalité présente : test par variable moins_de_deux_segments_informatifs, sans tirage", {
  e <- EXPO_C; e$segment <- "a"
  r <- homog(e, c("sexe", "segment"), c(50, 54), c(50, 52))
  v <- ligne_test(r, "variable", "segment")
  identical(v$etat, "moins_de_deux_segments_informatifs") &&
    identical(r$resultat$monte_carlo$jeux$tirages_effectues[r$resultat$monte_carlo$jeux$jeu == "variable_segment"], 0L) &&
    isTRUE(proche(ligne_test(r, "variable", "sexe")$statistique, 529 / 53, 1e-14, 0))
})

# Types des colonnes de segments (#201) : character (etat) et double
# (rapport_observes_attendus) même sans segment ; ifelse() sur un vecteur vide
# rendait logical(0).
verifier("F (#201) : exposition à 0 ligne -> segments à 0 ligne, colonnes etat (character) et rapport_observes_attendus, z (double)", {
  e0 <- data.frame(sexe = character(0), age = integer(0), exposition_centrale = numeric(0), evenements = integer(0))
  s <- homog(e0, "sexe", c(50, 54), c(50, 52))$resultat$segments
  if (nrow(s) == 0L && identical(s$etat, character(0)) && identical(s$rapport_observes_attendus, numeric(0)) &&
      identical(s$z, numeric(0))) TRUE else
    paste(nrow(s), typeof(s$etat), typeof(s$rapport_observes_attendus), typeof(s$z))
})

# Type des dénombrements (#199 ; ADR 0009, point 9.6) : segments$observes,
# jeux$tirages_effectues et les trois diagnostics d'événements en integer,
# avec tirages (C), avec événement sans exposition (F), sans aucun test
# produit (jeu à 0 tirage) et sur une exposition à 0 ligne.
verifier("F (#199) : segments$observes, monte_carlo$jeux$tirages_effectues, evenements_cellules_evenement_sans_exposition, evenements_hors_plage, evenements_total en integer (tirages, événement sans exposition, aucun test produit, 0 ligne)", {
  ty <- function(r) vapply(list(r$resultat$segments$observes, r$resultat$monte_carlo$jeux$tirages_effectues,
                                r$diagnostics$evenements_cellules_evenement_sans_exposition,
                                r$diagnostics$evenements_hors_plage, r$diagnostics$evenements_total), typeof, "")
  fe <- homog(rbind(EXPO_C, data.frame(sexe = "X", age = 52L, exposition_centrale = 0, evenements = 2L)), "sexe",
              c(50, 54), c(50, 52))
  e1 <- EXPO_C[EXPO_C$sexe == "F", ]
  sans_test <- homog(rbind(e1, data.frame(sexe = "H", age = 53L, exposition_centrale = 100, evenements = 0L)), "sexe",
                     c(50, 54), c(50, 52))
  e0 <- homog(data.frame(sexe = character(0), age = integer(0), exposition_centrale = numeric(0), evenements = integer(0)),
              "sexe", c(50, 54), c(50, 52))
  cas <- list(tirages = RC, sans_exposition = fe, sans_test = sans_test, vide = e0)
  r <- vapply(cas, function(x) all(ty(x) == "integer"), NA)
  tous(types = if (all(r)) TRUE else paste("non integer :", paste(names(r)[!r], collapse = ", ")),
       sans_test = if (identical(sans_test$resultat$monte_carlo$jeux$tirages_effectues, 0L)) TRUE else "tirages",
       vide = if (nrow(e0$resultat$segments) == 0L && identical(e0$diagnostics$evenements_total, 0L)) TRUE else "vide")
})

# --- G. Étape dans construire_table() (identité exacte ; recoupement) --------------

GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
JEU <- local({
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 20000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
  simuler_portefeuille(loi, pf, PERIODE, 20261106L)
})
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
T_HF <- table_reference(rbind(data.frame(age = GM$age, sexe = "H", qx = GM$qx), data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx)),
                        "GM par sexe", "importee", "test")
VAL <- list(table_reference = "tv", relation_table = "independante", plage_ages = c(20, 90),
            tranches_age = c(20, 50, 70), niveau = 0.95, replications = 99)
X <- construire_table(JEU, ROLES, plan_h())
H <- X$etapes$homogeneite
verifier("G objet : parametres (constantes de l'écart minimal détectable après convention, #218), resultat (tests, segments, ecart_minimal, monte_carlo), diagnostics ; graine du plan consignée", {
  identical(names(H), c("parametres", "resultat", "diagnostics")) &&
    identical(names(H$parametres), c("plage_ages", "tranches_age", "replications", "convention",
                                     "niveau_ecart_minimal", "puissance_ecart_minimal", "seuil_ecart_minimal",
                                     "hypothese_nulle", "statuts", "reference_methode")) &&
    identical(names(H$resultat), c("tests", "segments", "ecart_minimal", "monte_carlo")) &&
    identical(H$resultat$monte_carlo$graine, 20261106L) && identical(H$parametres$convention, "dernier_anniversaire") &&
    identical(names(H$resultat$tests), c("test", "variable", "conditionnellement_a", "tranche_debut", "tranche_fin",
                                         "strates_informatives", "groupes", "statistique", "ddl", "p_asymptotique",
                                         "p_exacte", "etat_p_exacte", "p_monte_carlo", "erreur_monte_carlo", "etat"))
})
verifier("G (#210, M399) : période sans exposition (jeu hors période) -> refus exposition / periode_sans_exposition avant l'étape homogeneite", {
  p0 <- plan_h(); p0$periode <- list(debut = D("1990-01-01"), fin = D("1990-12-31"))
  controle(refus_de(construire_table(JEU, ROLES, p0)), "exposition", "periode_sans_exposition")
})
verifier("G (#201) : exposition à 0 ligne (appel direct) -> étape produite, segments à 0 ligne, mêmes types de colonnes que sur une période exposée", {
  s0 <- homog(X$etapes$exposition$resultat$exposition[0L, ], "sexe", HOM$plage_ages, HOM$tranches_age)$resultat$segments
  s1 <- H$resultat$segments
  if (nrow(s0) == 0L && nrow(s1) > 0L && identical(vapply(s0, typeof, ""), vapply(s1, typeof, "")) &&
      identical(s0$etat, character(0)) && identical(s0$rapport_observes_attendus, numeric(0))) TRUE else
    paste(nrow(s0), paste(vapply(s0, typeof, ""), collapse = "/"))
})
verifier("G recoupement sur le jeu simulé : T global et somme_tranches = Rao de glm à 1e-7 ; forme close S = 2 recalculée à la main à 1e-12", {
  ex <- X$etapes$exposition$resultat$exposition
  ex <- ex[ex$age >= 20 & ex$age <= 90, ]
  tr <- findInterval(ex$age, c(20, 50, 70))
  r1 <- rao(ex, ex$age, indicatrices(ex$sexe))
  r2 <- rao(ex, ex$age, do.call(cbind, lapply(1:3, function(b) indicatrices(ex$sexe) * (tr == b))))
  Dc <- tapply(ex$evenements, ex$age, sum); Ec <- tapply(ex$exposition_centrale, ex$age, sum)
  h <- ex[ex$sexe == "H" & ex$exposition_centrale > 0, ]
  p <- h$exposition_centrale / Ec[as.character(h$age)]; n <- Dc[as.character(h$age)]
  ferme <- (sum(h$evenements) - sum(n * p))^2 / sum(n * p * (1 - p))
  t <- H$resultat$tests
  tous(g = proche(t$statistique[1L], r1[1L], 1e-7, 0), s = proche(t$statistique[t$test == "somme_tranches"], r2[1L], 1e-7, 0),
       f = proche(t$statistique[1L], ferme, 1e-12, 0))
})
verifier("G segments : somme des O globaux + hors plage = événements de l'exposition ; somme O = somme A par portée", {
  s <- H$resultat$segments
  e <- X$etapes$exposition$resultat$exposition
  sum(s$observes[s$portee == "global"]) + H$diagnostics$evenements_hors_plage == sum(e$evenements) &&
    isTRUE(proche(sum(s$attendus[s$portee == "global"]), sum(s$observes[s$portee == "global"]), 1e-13, 0))
})
verifier("G conventions : age_atteint_annee et par_annee admis (comparaison interne) ; par_annee : T global = Rao sur les cellules âge x année", {
  x1 <- construire_table(JEU, ROLES, plan_h(convention = "age_atteint_annee"))
  x2 <- construire_table(JEU, ROLES, plan_h(par_annee = TRUE))
  ex <- x2$etapes$exposition$resultat$exposition
  ex <- ex[ex$age >= 20 & ex$age <= 90, ]
  r <- rao(ex, paste(ex$age, ex$annee), indicatrices(ex$sexe))
  tous(c = if (identical(x1$etapes$homogeneite$parametres$convention, "age_atteint_annee")) TRUE else "convention",
       a = proche(x2$etapes$homogeneite$resultat$tests$statistique[1L], r[1L], 1e-7, 0))
})
# Localité de la graine (F11 ; M294, avis d'actuary, § 3) : étape entière
# identical() avec et sans l'autre étape aléatoire, dans les deux sens ;
# non-vacuité (tirages effectués, ddl > 0, une autre graine change des p de
# Monte-Carlo de chaque étape) ; égalité à l'appel direct sous plan$graine.
X_HV <- construire_table(JEU, ROLES, plan_h(val = VAL), tables_reference = list(tv = T_HF))
X_V <- construire_table(JEU, ROLES, plan_h(hom = NULL, val = VAL), tables_reference = list(tv = T_HF))
verifier("G localité (F11) : etapes$validation identical() avec et sans homogeneite au plan", {
  identical(X_HV$etapes$validation, X_V$etapes$validation)
})
verifier("G localité (F11) : avec importance_exclusions au plan, etapes$validation et etapes$importance_exclusions identical() avec et sans homogeneite", {
  imp <- function(p) { p$etapes$importance_exclusions <- list(niveau = 0.95); p }
  a <- construire_table(JEU, ROLES, imp(plan_h(val = VAL)), tables_reference = list(tv = T_HF))
  b <- construire_table(JEU, ROLES, imp(plan_h(hom = NULL, val = VAL)), tables_reference = list(tv = T_HF))
  identical(a$etapes$validation, b$etapes$validation) &&
    identical(a$etapes$importance_exclusions, b$etapes$importance_exclusions) &&
    identical(a$etapes$homogeneite, X$etapes$homogeneite)
})
verifier("G localité (F11) : etapes$homogeneite identical() avec et sans validation au plan", {
  identical(X_HV$etapes$homogeneite, X$etapes$homogeneite)
})
verifier("G localité (F11), non-vacuité : tirages effectués dans les deux étapes, ddl > 0, relation independante", {
  X_HV$etapes$validation$resultat$monte_carlo$tirages_effectues > 0 &&
    all(X_HV$etapes$homogeneite$resultat$monte_carlo$jeux$tirages_effectues > 0) &&
    all(X_HV$etapes$homogeneite$resultat$tests$ddl > 0) &&
    identical(X_HV$etapes$validation$parametres$relation_table, "independante")
})
verifier("G localité (F11), non-vacuité : une autre graine change des p de Monte-Carlo de chaque étape", {
  # Graine 12 : p de Monte-Carlo de la tranche 20-49 (B = 99) égale pour les
  # graines 7, 8, 9 et 11, différente pour 12 (mesuré) ; seule la non-vacuité
  # importe ici.
  y <- construire_table(JEU, ROLES, plan_h(val = VAL, graine = 12L), tables_reference = list(tv = T_HF))
  sv <- function(x) x$etapes$validation$resultat$statistiques
  pv <- function(x) sv(x)$valeur[sv(x)$grandeur == "p_monte_carlo"]
  !identical(pv(y), pv(X_HV)) &&
    !identical(y$etapes$homogeneite$resultat$tests$p_monte_carlo, X_HV$etapes$homogeneite$resultat$tests$p_monte_carlo)
})
verifier("G localité (F11) : étape homogeneite = appel direct de calculer_homogeneite() sous la graine du plan (règle exacte, aucune graine dérivée)", {
  pe <- X_HV$etapes$exposition$parametres
  r <- tablexp:::calculer_homogeneite(X_HV$etapes$exposition$resultat$exposition, "sexe", pe$par_annee,
                                      pe$convention$retenue, HOM$plage_ages, HOM$tranches_age, HOM$replications,
                                      20261106L)
  identical(r[c("parametres", "resultat", "diagnostics")], X_HV$etapes$homogeneite)
})
verifier("G étape aléatoire : état du générateur de l'appelant inchangé par construire_table()", {
  set.seed(42); avant <- .Random.seed
  construire_table(JEU, ROLES, plan_h())
  identical(.Random.seed, avant)
})

# --- H. Rapport figé et rejeu (identité exacte) ------------------------------------

lignes_rapport <- function(x) { l <- strsplit(rawToChar(ecrire_rapport(x)), "\n", fixed = TRUE)[[1L]]; Encoding(l) <- "UTF-8"; l }
section <- function(l, id) {
  d <- match(paste0("<section id=\"", id, "\">"), l)
  if (is.na(d)) character(0) else l[d:(d - 1L + match("</section>", l[d:length(l)]))]
}
verifier("H rapport : section etape_homogeneite entre etape_positionnement et etape_coherence_regularite (#144), avant etape_validation ; mention fixe ; tables tests et segments", {
  l <- lignes_rapport(X)
  s <- section(l, "etape_homogeneite")
  ids <- sub("^<section id=\"([a-z_]+)\">$", "\\1", grep("^<section id=", l, value = TRUE))
  k <- match("etape_homogeneite", ids)
  m <- tablexp:::MENTIONS_RAPPORT[["homogeneite_portee"]]
  length(s) > 0L &&
    identical(ids[k + c(-1L, 1L, 2L)], c("etape_positionnement", "etape_coherence_regularite", "etape_validation")) &&
    any(s == paste0("<p>", tablexp:::echapper_html(m), "</p>")) &&
    any(startsWith(s, "<caption><code>etapes/homogeneite/resultat/tests</code>")) &&
    any(startsWith(s, "<caption><code>etapes/homogeneite/resultat/segments</code>")) &&
    grepl("Diagnostic sans verdict ni seuil", m, fixed = TRUE)
})
verifier("H rapport : sans l'étape -> « Étape « homogeneite » : non demandée par le plan »", {
  x <- construire_table(JEU, ROLES, plan_h(hom = NULL))
  any(grepl("<p>Étape « homogeneite » : non demandée par le plan", section(lignes_rapport(x), "etape_homogeneite"),
            fixed = TRUE))
})
verifier("H rejeu : rejouer_fiche() sur la fiche écrite puis relue, identical() hors execution", {
  r <- rejouer_fiche(JEU, lire_fiche(ecrire_fiche(X$fiche)))
  identical(sans_execution(r$construction), sans_execution(X))
})

# --- I. Écart minimal détectable theta_80 par segment (#218) -----------------------
#
# Spécification d'actuary-approfondi, commentaire 7012205013 de #218 ; avis de
# forme F218 (M545, D4 (a)) ; critères 2, 3 et 5 de l'issue. Natures des
# attendus, nommées dans chaque test :
# - littéral de la spécification : constantes, états, textes de statut, du
#   signal et du rapport ;
# - valeur calculée à la main : forme close à parts constantes,
#   theta+ = 1 + r / (1 - pi r), theta- = 1 - r / (1 + pi r),
#   r = sqrt(lambda_80 / (D pi (1 - pi))), D décès des strates informatives ;
#   états des cas construits ;
# - recalcul indépendant : lambda_s(theta) = delta' V^+ delta, V complète,
#   pseudo-inverse de Moore-Penrose par svd(), reconstruit des agrégats sans
#   score_homogeneite() ;
# - calcul exact sans aléa (critère 3) : puissance exacte, loi binomiale, du
#   test asymptotique en theta_80(D).
# Recalcul indépendant sur R11 et R12 et valeurs de C4 : batterie de
# reproductibilité (objets de référence construits une fois).

L80_1 <- tablexp:::non_centralite_ecart_minimal(1)
# Exposition construite : E et D matrices S x K (segments x âges) ; une ligne
# par (segment, âge) ; segments s1, s2, ... ; âges 50, 51, ...
expo_em <- function(E, D) {
  S <- nrow(E); K <- ncol(E)
  data.frame(segment = rep(sprintf("s%d", seq_len(S)), K), age = rep(50L + seq_len(K) - 1L, each = S),
             exposition_centrale = as.vector(E), evenements = as.integer(as.vector(D)), stringsAsFactors = FALSE)
}
em_de <- function(E, D) homog(expo_em(E, D), "segment", c(50, 50 + ncol(E) - 1), 50, B = 9)
forme_close <- function(lambda, D, pi) {
  r <- sqrt(lambda / (D * pi * (1 - pi)))
  c(superieur = 1 + r / (1 - pi * r), inferieur = 1 - r / (1 + pi * r))
}
# lambda_s(theta) indépendant : strates informatives (D_.c > 0, au moins deux
# segments exposés), V complète, pseudo-inverse par svd().
lambda_independant <- function(E, D, s, theta) {
  Dc <- colSums(D); inf <- Dc > 0 & colSums(E > 0) >= 2
  S <- nrow(E); delta <- numeric(S); V <- matrix(0, S, S)
  for (k in which(inf)) {
    pi <- E[, k] / sum(E[, k])
    w <- rep(1, S); w[s] <- theta
    delta <- delta + Dc[k] * (pi * w / sum(pi * w) - pi)
    V <- V + Dc[k] * (diag(pi) - tcrossprod(pi))
  }
  sv <- svd(V); g <- sv$d > max(sv$d) * 1e-12
  Vp <- sv$v[, g, drop = FALSE] %*% (t(sv$u[, g, drop = FALSE]) / sv$d[g])
  list(lambda = drop(crossprod(delta, Vp %*% delta)), V = V, ddl = sum(g))
}
# lambda_s(theta) du moteur : système réduit de score_homogeneite() (même
# appel que l'étape, lignes (segment, âge)), appliqué à D_.c p_c(theta).
lambda_moteur <- function(E, D, s, theta) {
  S <- nrow(E); K <- ncol(E)
  sc <- tablexp:::score_homogeneite(as.vector(D), as.vector(E), rep(seq_len(K), each = S), rep(seq_len(S), K), K, S)
  ru <- sc$strate_tirage
  pi <- sc$poids / tablexp:::somme_par(sc$poids, ru, length(sc$effectifs))[ru]
  pw <- pi * ifelse(sc$groupe_utilise == s, theta, 1)
  p <- pw / tablexp:::somme_par(pw, ru, length(sc$effectifs))[ru]
  list(lambda = sc$statistique(matrix(sc$effectifs[ru] * p, ncol = 1L)), v = sc$v)
}

# Parts constantes (pi_s identique dans chaque cellule), S = 2 et S = 4 ;
# parts différentes entre cellules, S = 2 et S = 3.
E_PC2 <- outer(c(0.3, 0.7), c(1000, 2000, 500)); D_PC2 <- rbind(c(10, 30, 5), c(30, 45, 25))
E_PC4 <- outer(c(0.1, 0.2, 0.3, 0.4), c(800, 1500, 1200))
D_PC4 <- rbind(c(9, 25, 14), c(21, 38, 30), c(30, 60, 47), c(40, 77, 59))
E_PD2 <- rbind(c(100, 400, 900, 300), c(900, 600, 100, 300)); D_PD2 <- rbind(c(4, 12, 30, 9), c(30, 20, 5, 10))
E_PD3 <- rbind(c(100, 400, 900, 300), c(900, 600, 100, 300), c(200, 50, 700, 400))
D_PD3 <- rbind(c(4, 12, 30, 9), c(30, 20, 5, 10), c(6, 2, 25, 14))
R_PC2 <- em_de(E_PC2, D_PC2); R_PC4 <- em_de(E_PC4, D_PC4); R_PD2 <- em_de(E_PD2, D_PD2); R_PD3 <- em_de(E_PD3, D_PD3)

verifier("I constantes (littéral de la spécification, § 5 et 4) : niveau 0,05, puissance 0,80, seuil 1,25 ; états de ecart_minimal ; lambda_80 = 7,848860509 à 1 ddl (mesure d'actuary-approfondi)", {
  tous(constantes = if (identical(tablexp:::NIVEAU_ECART_MINIMAL, 0.05) && identical(tablexp:::PUISSANCE_ECART_MINIMAL, 0.80) &&
                        identical(tablexp:::SEUIL_ECART_MINIMAL, 1.25)) TRUE else "constantes",
       etats = if (identical(tablexp:::ETATS_ECART_MINIMAL_HOMOGENEITE,
                             c("produite", "non_atteint", "precision_insuffisante", "evenement_sans_exposition",
                               "moins_de_deux_segments_informatifs", "segment_sans_exposition", "variance_nulle"))) TRUE
              else "états",
       lambda = proche(L80_1, 7.848860509, 0, 5e-10),
       puissance = proche(stats::pchisq(stats::qchisq(0.95, 1), 1, ncp = L80_1, lower.tail = FALSE), 0.80, 0, 1e-12),
       ddl3 = proche(stats::pchisq(stats::qchisq(0.95, 3), 3, ncp = tablexp:::non_centralite_ecart_minimal(3),
                                   lower.tail = FALSE), 0.80, 0, 1e-12))
})
verifier("I paramètres de l'étape (F218) : niveau_ecart_minimal, puissance_ecart_minimal, seuil_ecart_minimal (double) après convention, avant hypothese_nulle, égaux aux constantes", {
  p <- H$parametres
  k <- match(c("convention", "niveau_ecart_minimal", "puissance_ecart_minimal", "seuil_ecart_minimal", "hypothese_nulle"),
             names(p))
  identical(k, 4:8) && identical(p$niveau_ecart_minimal, tablexp:::NIVEAU_ECART_MINIMAL) &&
    identical(p$puissance_ecart_minimal, tablexp:::PUISSANCE_ECART_MINIMAL) &&
    identical(p$seuil_ecart_minimal, tablexp:::SEUIL_ECART_MINIMAL)
})
# Texte de la spécification, § 7 (theta_80_superieur), modifié par la
# correction du 19/11/2026 (constat C1 de la revue finale de B30 ;
# spécification d'actuary-approfondi, § 2 : V « celle du test global »,
# monotonie exacte, uniroot en log theta, état precision_insuffisante), puis
# par le motif de precision_insuffisante (#340, spécification
# d'actuary-approfondi, Q4, amendée : racine non calculable par cette
# méthode, qu'elle soit ou non représentable), puis par le sens de l'erreur
# de la branche + (#331, spécification d'actuary-approfondi, Q5, corrigée à
# la reprise : prudente, optimiste ou de sens non établi ; branche -
# inchangée), et ses quatre substitutions pour theta_80_inferieur. Cause
# « part d'exposition arrondie à 0 » de precision_insuffisante nommée par #333
# (spécification d'actuary-approfondi, Q4, § 3 ; M570).
STATUT_SUP_218 <- paste(
  "approximation asymptotique : plus petit theta > 1 tel que lambda_s(theta) >= lambda_80, theta multipliant la force",
  "du seul segment s dans chaque cellule ; lambda_s(theta) = delta_s(theta)' V^- delta_s(theta), delta_s(theta) = somme",
  "sur les strates informatives du test global de D_.c (p_c(theta) - pi_c), p_jc(theta) = pi_jc w_j / somme sur k de",
  "pi_kc w_k, w_s = theta, w_j = 1 sinon : espérance exacte de U sachant les D_.c sous le modèle de Poisson à exposition",
  "tenue pour fixe ; V variance de U sous H0 (celle du test global) ; lambda_s(theta) strictement monotone sur chaque",
  "branche (exact), racine unique ; lambda_80 : non-centralité pour",
  "laquelle la loi du khi-deux non centrale à ddl degrés de liberté (ddl du test global) dépasse le quantile d'ordre",
  "1 - niveau_ecart_minimal de la loi centrale avec la probabilité puissance_ecart_minimal (conventions) ; la variance",
  "de U sous l'alternative diffère de V : à deux segments en strate unique, en approximation normale, la puissance",
  "réelle en theta_80 s'écarte de puissance_ecart_minimal de 0,03 au plus quand theta_80 est au plus",
  "seuil_ecart_minimal, davantage au-delà, et la valeur est prudente (theta_80 réel au plus aussi éloigné de 1) quand",
  "pi >= 1 / (1 + sqrt(theta_80)), pi part d'exposition du segment (environ 0,472 quand theta_80 vaut",
  "seuil_ecart_minimal), et, en deçà, où rho^2 = theta_80 / (1 + pi (theta_80 - 1))^2, rapport exact des variances de",
  "U sous l'alternative et sous H0, dépasse 1, optimiste (theta_80 réel plus éloigné de 1) si theta_80 <= 51,38 ou pi",
  ">= 0,0049, de sens non établi sinon (moins de 0,2 décès attendu dans le segment en theta_80) ;",
  "au-delà de deux segments ou à parts différentes entre strates, aucune borne établie ; lambda_80",
  "(pchisq non centrale) et theta_80 (uniroot en log theta, lambda_s calculée sans soustraction) par résolution",
  "numérique : approximation numérique ; état non_atteint",
  "(NA) quand lambda_s(theta) reste au plus lambda_80 (1 + 1e-8), tolérance relative de la résolution numérique,",
  "lorsque theta tend vers l'infini ; état precision_insuffisante (NA) quand la racine n'est pas calculable par cette",
  "méthode en double précision à cette tolérance, qu'elle soit ou non représentable (pivot de l'élimination non fini",
  "ou non positif, limite indéterminée (dont une part d'exposition arrondie à 0 dans une strate où le segment et un",
  "autre segment sont exposés), résolution non convergée, ou postcondition |lambda_s(theta_80) / lambda_80 - 1|",
  "<= 1e-8 non tenue) ; unités d'observation indépendantes ; sous l'unité contrat, les décès sont sur-dispersés et",
  "theta_80 est trop proche de 1 (écart minimal détectable sous-estimé)")
verifier("I STATUTS_HOMOGENEITE (littéral de la spécification, § 7) : deux lignes ecart_minimal en fin (theta_80_superieur, theta_80_inferieur), textes exacts, inférieur par les quatre substitutions ; 15 lignes, UTF-8", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  inf <- STATUT_SUP_218
  for (r in list(c("plus petit theta > 1", "plus grand theta < 1"),
                 c("quand theta_80 est au plus seuil_ecart_minimal", "quand theta_80 est au moins 1/seuil_ecart_minimal"),
                 c(paste("la valeur est prudente (theta_80 réel au plus aussi éloigné de 1) quand pi >= 1 / (1 +",
                         "sqrt(theta_80)), pi part d'exposition du segment (environ 0,472 quand theta_80 vaut",
                         "seuil_ecart_minimal), et, en deçà, où rho^2 = theta_80 / (1 + pi (theta_80 - 1))^2, rapport",
                         "exact des variances de U sous l'alternative et sous H0, dépasse 1, optimiste (theta_80 réel",
                         "plus éloigné de 1) si theta_80 <= 51,38 ou pi >= 0,0049, de sens non établi sinon (moins de",
                         "0,2 décès attendu dans le segment en theta_80)"),
                   "la valeur est prudente pour un segment minoritaire (theta_80 réel plus proche de 1)"),
                 c("lorsque theta tend vers l'infini", "lorsque theta tend vers 0"))) {
    if (lengths(regmatches(inf, gregexpr(r[1L], inf, fixed = TRUE))) != 1L) return(paste("substitution non unique :", r[1L]))
    inf <- sub(r[1L], r[2L], inf, fixed = TRUE)
  }
  # Statuts choisis par (table, grandeur), non par position (mineur de la
  # revue finale de B32, D4 (a), M569) ; la position en fin reste contrôlée
  # par « lignes ».
  k <- match(c("theta_80_superieur", "theta_80_inferieur"), s$grandeur[s$table == "ecart_minimal"])
  st <- s$statut[s$table == "ecart_minimal"][k]
  tous(lignes = if (nrow(s) == 15L && identical(s$table[14:15], c("ecart_minimal", "ecart_minimal")) &&
                    identical(s$grandeur[14:15], c("theta_80_superieur", "theta_80_inferieur"))) TRUE else "lignes",
       sup = if (identical(st[1L], STATUT_SUP_218)) TRUE else "texte de theta_80_superieur",
       inf = if (identical(st[2L], inf)) TRUE else "texte de theta_80_inferieur",
       utf8 = if (!anyNA(k) && all(Encoding(st) == "UTF-8")) TRUE else "encodage")
})
# Audit de #218, constat m3 : le texte des deux statuts ecart_minimal écrit la
# tolérance en dur, « (1 + 1e-8) » (statut_ecart_minimal()), alors que le calcul
# lit TOLERANCE_ECART_MINIMAL (R/validation.R). Le test précédent compare le
# texte à un littéral, pas à la constante : celui-ci relit la valeur citée par
# chaque statut et la compare à la constante. Depuis #331, la branche + cite
# aussi « (1 + sqrt(theta_80)) » et « (1 + pi (theta_80 - 1)) » : ces deux
# occurrences exactes (liste blanche) sont retirées avant la relecture ; toute
# autre occurrence de « (1 + » compte.
verifier("I statuts ecart_minimal (m3 de #218) : hors liste blanche exacte, chaque statut cite « (1 + x) » une seule fois, x relu du texte identique à TOLERANCE_ECART_MINIMAL ; littéral 1e-8 ; témoin : une autre valeur est détectée", {
  tol <- tablexp:::TOLERANCE_ECART_MINIMAL
  s <- tablexp:::STATUTS_HOMOGENEITE
  blanche <- c("(1 + sqrt(theta_80))", "(1 + pi (theta_80 - 1))")
  citee <- function(txt) {
    for (b in blanche) txt <- gsub(b, "", txt, fixed = TRUE)
    m <- regmatches(txt, gregexpr("\\(1 \\+ [^)]+\\)", txt))[[1L]]
    if (length(m) != 1L) return(NA_real_)
    suppressWarnings(as.numeric(sub("\\)$", "", sub("^\\(1 \\+ ", "", m))))
  }
  em <- s$statut[s$table == "ecart_minimal"]
  v <- vapply(em, citee, 0, USE.NAMES = FALSE)
  tous(lignes = if (length(em) == 2L) TRUE else "deux statuts ecart_minimal attendus",
       citee = if (!anyNA(v) && all(v == tol)) TRUE else paste("valeur citée :", format(v), "; constante :", format(tol)),
       litteral = if (identical(tol, 1e-8)) TRUE else paste("constante :", format(tol)),
       temoin = if (!is.na(citee("(1 + 1e-9)")) && citee("(1 + 1e-9)") != tol && is.na(citee("sans mention"))) TRUE else "témoin",
       blanche = if (identical(citee("(1 + 1e-8) (1 + sqrt(theta_80)) (1 + pi (theta_80 - 1))"), 1e-8) &&
                     is.na(citee("(1 + 1e-8) (1 + sqrt(theta))"))) TRUE else "liste blanche")
})
# #331 (spécification d'actuary-approfondi, Q5, corrigée à la reprise) :
# chiffres cités par la branche + du statut, recalculés à partir des
# constantes. rho* : seconde racine (au-delà du minimum, rho ~ 2,517) de
# Phi(a / rho) + Phi(-b / rho) = puissance, a, b = sqrt(lambda_80) -/+ z ;
# 0,472 = 1 / (1 + sqrt(seuil)) arrondi ; 51,38 = rho*^2 arrondi par défaut ;
# 0,0049 = pi_c, racine de 1 / (4 pi (1 - pi)) = rho*^2, arrondi par excès ;
# décès attendus dans le segment en theta_80, à D_. tenu pour fixe : D_. p,
# D_. = lambda_80 pi (1 - pi) / (p - pi)^2, p = pi theta / (1 + pi (theta -
# 1)), sous 0,2 au coin (51,38 ; 0,0049). Fragments ASCII (locale C).
verifier("I statut theta_80_superieur (#331) : chiffres cités (0,472 ; 51,38 ; 0,0049 ; 0,2 décès) recalculés à partir de NIVEAU, PUISSANCE et SEUIL_ECART_MINIMAL et de non_centralite_ecart_minimal(1) ; témoin : un chiffre altéré est détecté", {
  l80 <- tablexp:::non_centralite_ecart_minimal(1)
  z <- stats::qnorm(1 - tablexp:::NIVEAU_ECART_MINIMAL / 2)
  a <- sqrt(l80) - z; b <- sqrt(l80) + z
  rs2 <- stats::uniroot(function(r) stats::pnorm(a / r) + stats::pnorm(-b / r) - tablexp:::PUISSANCE_ECART_MINIMAL,
                        c(3, 100), tol = 1e-12)$root^2
  pc <- (1 - sqrt(1 - 1 / rs2)) / 2
  virgule <- function(x, n) sub(".", ",", sprintf("%.*f", n, x), fixed = TRUE)
  seuil_pi <- virgule(round(1 / (1 + sqrt(tablexp:::SEUIL_ECART_MINIMAL)), 3), 3)
  rho2 <- virgule(floor(100 * rs2) / 100, 2)
  pi_c <- virgule(ceiling(1e4 * pc) / 1e4, 4)
  th <- floor(100 * rs2) / 100; pp <- ceiling(1e4 * pc) / 1e4
  p <- pp * th / (1 + pp * (th - 1))
  deces <- l80 * pp * (1 - pp) * p / (p - pp)^2
  fragments <- c(paste0("environ ", seuil_pi, " quand theta_80 vaut seuil_ecart_minimal"),
                 paste0("si theta_80 <= ", rho2, " ou pi >= ", pi_c, ","), "(moins de 0,2 d")
  controle <- function(txt) {
    n <- vapply(fragments, function(f) lengths(regmatches(txt, gregexpr(f, txt, fixed = TRUE))), 0L)
    if (all(n == 1L)) TRUE else paste("fragment(s) absent(s) ou répété(s) :", paste(fragments[n != 1L], collapse = " | "))
  }
  # Statut choisi par (table, grandeur), non par position (D4 (a), M569).
  s <- tablexp:::STATUTS_HOMOGENEITE
  sup <- s$statut[s$table == "ecart_minimal" & s$grandeur == "theta_80_superieur"]
  tous(chiffres = if (identical(c(seuil_pi, rho2, pi_c), c("0,472", "51,38", "0,0049"))) TRUE
                  else paste("recalculés :", seuil_pi, rho2, pi_c),
       unique = if (length(sup) == 1L) TRUE else sprintf("%d statut(s) theta_80_superieur", length(sup)),
       statut = controle(sup),
       deces = if (deces < 0.2) TRUE else paste("décès attendus au coin :", format(deces, digits = 6)),
       # Témoin de chaque chiffre cité, « 0,2 » compris (mineur de la revue
       # finale de B32, D4 (a), M569).
       temoin = if (!isTRUE(controle(sub("51,38", "51,39", sup, fixed = TRUE))) &&
                    !isTRUE(controle(sub("0,0049", "0,0048", sup, fixed = TRUE))) &&
                    !isTRUE(controle(sub("0,472", "0,473", sup, fixed = TRUE))) &&
                    !isTRUE(controle(sub("(moins de 0,2 d", "(moins de 0,3 d", sup, fixed = TRUE)))) TRUE else "témoin")
})
verifier("I table ecart_minimal (F218) : une ligne par segment dans l'ordre des lignes globales de segments, clés de segmentation, theta_80_superieur, etat_superieur, theta_80_inferieur, etat_inferieur ; types (double, character) ; aucune ligne de tranche", {
  em <- H$resultat$ecart_minimal
  g <- H$resultat$segments[H$resultat$segments$portee == "global", ]
  identical(names(em), c("sexe", "theta_80_superieur", "etat_superieur", "theta_80_inferieur", "etat_inferieur")) &&
    identical(em$sexe, g$sexe) && identical(vapply(em, typeof, ""), c(sexe = "character", theta_80_superieur = "double",
                                                                       etat_superieur = "character", theta_80_inferieur = "double",
                                                                       etat_inferieur = "character")) &&
    identical(rownames(em), as.character(seq_len(nrow(em))))
})
verifier("I forme close à parts constantes (valeur calculée à la main), S = 2 : quatre valeurs à 1e-8", {
  em <- R_PC2$resultat$ecart_minimal; Dt <- sum(D_PC2)
  f1 <- forme_close(L80_1, Dt, 0.3); f2 <- forme_close(L80_1, Dt, 0.7)
  tous(etats = if (all(c(em$etat_superieur, em$etat_inferieur) == "produite")) TRUE else "états",
       s1 = proche(c(em$theta_80_superieur[1L], em$theta_80_inferieur[1L]), unname(f1), 0, 1e-8),
       s2 = proche(c(em$theta_80_superieur[2L], em$theta_80_inferieur[2L]), unname(f2), 0, 1e-8))
})
verifier("I forme close à parts constantes (valeur calculée à la main), S = 4 (ddl 3) : huit valeurs à 1e-8", {
  em <- R_PC4$resultat$ecart_minimal; Dt <- sum(D_PC4); l3 <- tablexp:::non_centralite_ecart_minimal(3)
  f <- vapply(c(0.1, 0.2, 0.3, 0.4), function(pi) forme_close(l3, Dt, pi), c(superieur = 0, inferieur = 0))
  tous(ddl = if (identical(R_PC4$resultat$tests$ddl[1L], 3L)) TRUE else "ddl",
       etats = if (all(c(em$etat_superieur, em$etat_inferieur) == "produite")) TRUE else "états",
       sup = proche(em$theta_80_superieur, f["superieur", ], 0, 1e-8),
       inf = proche(em$theta_80_inferieur, f["inferieur", ], 0, 1e-8))
})
verifier("I S = 2 (identité exacte, parts différentes et constantes) : theta_80+ du segment 1 x theta_80- du segment 2 = 1, et réciproquement, à 1e-8", {
  pr <- function(r) { em <- r$resultat$ecart_minimal
    c(em$theta_80_superieur[1L] * em$theta_80_inferieur[2L], em$theta_80_inferieur[1L] * em$theta_80_superieur[2L]) }
  tous(pd = proche(pr(R_PD2), c(1, 1), 0, 1e-8), pc = proche(pr(R_PC2), c(1, 1), 0, 1e-8))
})
verifier("I lambda_s(theta_80) = lambda_80 à 1e-8 en relatif sur chaque branche produite (recalcul indépendant, V complète et pseudo-inverse ; et système réduit du moteur), S = 2, 3, 4", {
  e <- character(0)
  for (cas in list(list(R_PC2, E_PC2, D_PC2), list(R_PC4, E_PC4, D_PC4), list(R_PD2, E_PD2, D_PD2),
                   list(R_PD3, E_PD3, D_PD3))) {
    em <- cas[[1L]]$resultat$ecart_minimal; E <- cas[[2L]]; Dm <- cas[[3L]]
    l80 <- tablexp:::non_centralite_ecart_minimal(lambda_independant(E, Dm, 1L, 1)$ddl)
    for (s in seq_len(nrow(E))) for (th in c(em$theta_80_superieur[s], em$theta_80_inferieur[s])) {
      if (is.na(th)) { e <- c(e, "branche non produite"); next }
      li <- lambda_independant(E, Dm, s, th)$lambda; lm <- lambda_moteur(E, Dm, s, th)$lambda
      if (abs(li / l80 - 1) > 1e-8 || abs(lm / l80 - 1) > 1e-8)
        e <- c(e, sprintf("S = %d, segment %d, theta %.10f : %.3e, %.3e", nrow(E), s, th, li / l80 - 1, lm / l80 - 1))
    }
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
verifier("I premier ordre (identité asymptotique) : lambda_s(1 + h) / (h^2 V_ss) à 1e-4 de 1 pour h = 1e-6 et h = -1e-6, chaque segment, S = 3 à parts différentes (système réduit du moteur)", {
  r <- unlist(lapply(1:3, function(s) vapply(c(1e-6, -1e-6), function(h) {
    l <- lambda_moteur(E_PD3, D_PD3, s, 1 + h); l$lambda / (h^2 * l$v[s]) }, 0)))
  proche(r, rep(1, 6L), 0, 1e-4)
})
# Monotonie stricte de lambda_s sur chaque branche : résultat exact
# (spécification de la correction du 19/11/2026, § 2, Q1 : L_s Z-matrice
# symétrique définie positive, d'inverse >= 0 ; |h_c| strictement croissant).
# Ce test n'en est qu'un contrôle sur un cas.
verifier("I monotonie et extrémalité (résultat exact ; contrôle sur un cas) : lambda_s(theta) < lambda_80 entre 1 et theta_80 sur chaque branche (grille de 50 points), S = 3 à parts différentes", {
  em <- R_PD3$resultat$ecart_minimal
  l80 <- tablexp:::non_centralite_ecart_minimal(2L)
  ok <- vapply(1:3, function(s) {
    g <- c(seq(1, em$theta_80_superieur[s], length.out = 52L)[2:51], seq(em$theta_80_inferieur[s], 1, length.out = 52L)[2:51])
    all(vapply(g, function(th) lambda_independant(E_PD3, D_PD3, s, th)$lambda, 0) < l80)
  }, NA)
  if (all(ok)) TRUE else paste("segments", paste(which(!ok), collapse = ", "))
})
verifier("I état non_atteint (valeur calculée à la main) : S = 2, une cellule, E = (10, 90), D = (2, 18) : branche - du segment 1 et branche + du segment 2 non_atteint (limite lambda = 20/9 < lambda_80), valeurs NA ; autres branches produites ; signal pour 2 segments", {
  r <- em_de(matrix(c(10, 90), 2L), matrix(c(2, 18), 2L)); em <- r$resultat$ecart_minimal
  a <- r$avertissements
  tous(etats = if (identical(em$etat_superieur, c("produite", "non_atteint")) &&
                   identical(em$etat_inferieur, c("non_atteint", "produite"))) TRUE
               else paste(c(em$etat_superieur, em$etat_inferieur), collapse = ", "),
       na = if (is.na(em$theta_80_inferieur[1L]) && is.na(em$theta_80_superieur[2L]) &&
                is.finite(em$theta_80_superieur[1L]) && is.finite(em$theta_80_inferieur[2L])) TRUE else "valeurs",
       limite = proche(lambda_independant(matrix(c(10, 90), 2L), matrix(c(2, 18), 2L), 1L, 1e-12)$lambda, 20 / 9, 1e-9, 0),
       signal = if (identical(a$code, "ecart_minimal_detectable") && identical(a$nombre, 2L)) TRUE else "signal")
})
# Seuil de non_atteint (reprise de l'audit, M1 ; règle d'actuary) : S = 2,
# une cellule, D = 1, E = (1, a) puis (a, 1), a = lambda_80 (1 + eps) ; la
# limite de lambda vaut a sur la branche + du segment de part 1 / (1 + a) et
# sur la branche - de l'autre (1 / a < lambda_80 sur les deux autres
# branches). Le segment 1 est le groupe de référence du système réduit (hors
# de garde), le segment 2 y est : chaque branche est éprouvée dans les deux
# rôles. eps <= 1e-9 : non_atteint (seuil inclusif lambda_80 (1 + 1e-8)) ;
# eps >= 2e-8 : produite, theta fini, postcondition recalculée.
seuil_218 <- function(eps) {
  a <- L80_1 * (1 + eps)
  list(A = em_de(matrix(c(1, a), 2L), matrix(c(1, 0), 2L)), B = em_de(matrix(c(a, 1), 2L), matrix(c(1, 0), 2L)),
       EA = matrix(c(1, a), 2L), EB = matrix(c(a, 1), 2L))
}
verifier("I seuil de non_atteint (valeur calculée à la main) : eps dans {0, 1e-14, 1e-13, 1e-9} -> non_atteint et NA sur chaque branche, segment de référence ou non, sans erreur ; signal ecart_minimal_detectable", {
  e <- character(0)
  for (eps in c(0, 1e-14, 1e-13, 1e-9)) {
    r <- tryCatch(seuil_218(eps), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, sprintf("eps %g : erreur %s", eps, conditionMessage(r))); next }
    a <- r$A$resultat$ecart_minimal; b <- r$B$resultat$ecart_minimal
    ok <- identical(c(a$etat_superieur, a$etat_inferieur, b$etat_superieur, b$etat_inferieur), rep("non_atteint", 8L)) &&
      all(is.na(c(a$theta_80_superieur, a$theta_80_inferieur, b$theta_80_superieur, b$theta_80_inferieur))) &&
      identical(r$A$avertissements$code, "ecart_minimal_detectable") && identical(r$A$avertissements$nombre, 2L) &&
      identical(r$B$avertissements$code, "ecart_minimal_detectable") && identical(r$B$avertissements$nombre, 2L)
    if (!ok) e <- c(e, sprintf("eps %g : %s", eps, paste(c(a$etat_superieur, a$etat_inferieur, b$etat_superieur,
                                                             b$etat_inferieur), collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
verifier("I seuil de non_atteint (valeur calculée à la main) : eps dans {2e-8, 1e-6} -> produite, theta fini (theta+ > 1, 0 < theta- < 1), postcondition recalculée (recalcul indépendant) ; même résultat que le segment soit ou non le groupe de référence", {
  e <- character(0)
  for (eps in c(2e-8, 1e-6)) {
    r <- seuil_218(eps)
    a <- r$A$resultat$ecart_minimal; b <- r$B$resultat$ecart_minimal
    # Branches au seuil : A, segment 1 (+) et segment 2 (-) ; B, segment 2 (+) et segment 1 (-).
    th <- c(a$theta_80_superieur[1L], b$theta_80_superieur[2L], a$theta_80_inferieur[2L], b$theta_80_inferieur[1L])
    et <- c(a$etat_superieur[1L], b$etat_superieur[2L], a$etat_inferieur[2L], b$etat_inferieur[1L])
    rel <- c(lambda_independant(r$EA, matrix(c(1, 0), 2L), 1L, th[1L])$lambda,
             lambda_independant(r$EB, matrix(c(1, 0), 2L), 2L, th[2L])$lambda,
             lambda_independant(r$EA, matrix(c(1, 0), 2L), 2L, th[3L])$lambda,
             lambda_independant(r$EB, matrix(c(1, 0), 2L), 1L, th[4L])$lambda) / L80_1 - 1
    if (!all(et == "produite") || !all(is.finite(th)) || !all(th[1:2] > 1) || !all(th[3:4] > 0 & th[3:4] < 1))
      e <- c(e, sprintf("eps %g : états ou valeurs (%s ; %s)", eps, paste(et, collapse = ", "), paste(format(th), collapse = ", ")))
    else if (any(abs(rel) > 1e-8)) e <- c(e, sprintf("eps %g : postcondition %s", eps, paste(format(rel), collapse = ", ")))
    else if (abs(th[1L] / th[2L] - 1) > 1e-6 || abs(th[3L] / th[4L] - 1) > 1e-6)
      e <- c(e, sprintf("eps %g : résultat selon le rôle du segment (%s)", eps, paste(format(th, digits = 15), collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Limite en forme close (reprise de l'audit, m2) : S = 2, une cellule, D = 8,
# parts pi et 1 - pi, (1 - pi) / pi = lambda_80 (1,002) / 8 : lambda(infini) =
# 1,002 lambda_80, lambda(1000) < lambda_80 ; theta+ du segment 1 > 1000,
# égal à la forme close. Une évaluation à theta fini (t = 0,999, theta = 1000)
# au lieu de la limite rendrait non_atteint.
verifier("I limite theta -> infini en forme close (valeur calculée à la main) : lambda(1000) < lambda_80 < lambda(infini) ; theta+ produit, > 1000, égal à la forme close à 1e-6", {
  pi <- 1 / (1 + L80_1 * 1.002 / 8)
  E <- matrix(1000 * c(pi, 1 - pi), 2L); Dm <- matrix(c(4, 4), 2L)
  r <- em_de(E, Dm); em <- r$resultat$ecart_minimal
  l1000 <- lambda_independant(E, Dm, 1L, 1000)$lambda
  tous(cas = if (l1000 < L80_1 && lambda_independant(E, Dm, 1L, 1e12)$lambda > L80_1) TRUE else sprintf("lambda(1000) = %.6f", l1000),
       etat = if (identical(em$etat_superieur[1L], "produite") && em$theta_80_superieur[1L] > 1000) TRUE
              else paste(em$etat_superieur[1L], em$theta_80_superieur[1L]),
       valeur = proche(em$theta_80_superieur[1L], forme_close(L80_1, 8, pi)[["superieur"]], 1e-6, 0))
})
verifier("I états hérités (valeur calculée à la main) : evenement_sans_exposition et moins_de_deux_segments_informatifs du test global ; segment_sans_exposition et variance_nulle de la ligne globale du segment ; valeurs NA ; aucun signal", {
  em_e <- function(e) homog(e, "sexe", c(50, 54), c(50, 52))
  r_ev <- em_e(rbind(EXPO_C, data.frame(sexe = "X", age = 52L, exposition_centrale = 0, evenements = 2L)))
  e2 <- EXPO_C[EXPO_C$sexe == "F", ]
  r_md <- em_e(rbind(e2, data.frame(sexe = "H", age = 53L, exposition_centrale = 100, evenements = 0L)))
  r_se <- em_e(rbind(EXPO_C, data.frame(sexe = "X", age = 40L, exposition_centrale = 500, evenements = 3L)))
  # Segment X exposé dans la seule cellule sans décès (âge 53) : V_XX = 0.
  r_vn <- em_e(rbind(EXPO_C, data.frame(sexe = "X", age = 53L, exposition_centrale = 100, evenements = 0L)))
  et <- function(r) paste(r$resultat$ecart_minimal$etat_superieur, r$resultat$ecart_minimal$etat_inferieur, sep = "/")
  sans_signal <- function(r) !"ecart_minimal_detectable" %in% r$avertissements$code
  tous(ev = if (identical(et(r_ev), rep("evenement_sans_exposition/evenement_sans_exposition", 3L))) TRUE else et(r_ev),
       md = if (identical(et(r_md), rep("moins_de_deux_segments_informatifs/moins_de_deux_segments_informatifs", 2L))) TRUE
            else et(r_md),
       se = if (identical(et(r_se), c("produite/produite", "produite/produite", "segment_sans_exposition/segment_sans_exposition")))
              TRUE else et(r_se),
       vn = if (identical(et(r_vn), c("produite/produite", "produite/produite", "variance_nulle/variance_nulle"))) TRUE
            else et(r_vn),
       na = if (all(is.na(unlist(r_ev$resultat$ecart_minimal[c("theta_80_superieur", "theta_80_inferieur")]))) &&
                all(is.na(unlist(r_md$resultat$ecart_minimal[c("theta_80_superieur", "theta_80_inferieur")]))) &&
                is.na(r_se$resultat$ecart_minimal$theta_80_superieur[3L]) &&
                is.na(r_vn$resultat$ecart_minimal$theta_80_inferieur[3L])) TRUE else "valeurs non NA",
       # Les segments F et H gardent leurs valeurs : le test global est inchangé.
       fh = proche(r_se$resultat$ecart_minimal$theta_80_superieur[1:2], RC$resultat$ecart_minimal$theta_80_superieur,
                   1e-14, 0),
       signal = if (sans_signal(r_ev) && sans_signal(r_md)) TRUE else "signal sur un état hérité")
})
verifier("I exposition à 0 ligne (#201) : ecart_minimal à 0 ligne, mêmes types de colonnes que sur une période exposée", {
  e0 <- homog(X$etapes$exposition$resultat$exposition[0L, ], "sexe", HOM$plage_ages, HOM$tranches_age)$resultat$ecart_minimal
  e1 <- H$resultat$ecart_minimal
  if (nrow(e0) == 0L && nrow(e1) > 0L && identical(vapply(e0, typeof, ""), vapply(e1, typeof, ""))) TRUE else
    paste(nrow(e0), paste(vapply(e0, typeof, ""), collapse = "/"))
})
# Signal (critère 5) : S = 2, une cellule, parts 0,2 / 0,8. À D = 1000 :
# r = 0,2215 ; segment 1 : theta- = 0,7879 < 0,8 (signal), theta+ = 1,2318 <
# 1,25 ; segment 2 : theta+ = 1,2692 > 1,25 (signal), theta- = 0,8118 > 0,8.
# À D = 2000 : aucune branche au-delà du seuil.
MESSAGE_218 <- paste("Homogénéité : 2 segment(s) pour lesquels le test global n'atteint pas la puissance 0,80 au niveau 0,05",
                     "face à un écart de force d'un facteur 1,25 ou 1/1,25 (theta_80_superieur > 1,25, theta_80_inferieur <",
                     "1/1,25 ou branche non_atteint ; etapes/homogeneite/resultat/ecart_minimal) : approximation",
                     "asymptotique, seuil conventionnel ; avertissement, ni refus ni verdict.")
verifier("I signal (critère 5, valeur calculée à la main) : de part et d'autre du seuil sur chaque branche ; code ecart_minimal_detectable, étape homogeneite, nombre de segments signalés, message littéral de la spécification ; jamais un refus", {
  r1 <- em_de(matrix(c(200, 800), 2L), matrix(c(200, 800), 2L)); em <- r1$resultat$ecart_minimal
  r2 <- em_de(matrix(c(200, 800), 2L), matrix(c(400, 1600), 2L))
  a <- r1$avertissements
  tous(valeurs = proche(c(em$theta_80_inferieur[1L], em$theta_80_superieur[1L], em$theta_80_superieur[2L],
                          em$theta_80_inferieur[2L]),
                        c(forme_close(L80_1, 1000, 0.2)[c("inferieur", "superieur")], forme_close(L80_1, 1000, 0.8)), 0, 1e-8),
       cote = if (em$theta_80_inferieur[1L] < 0.8 && em$theta_80_superieur[1L] < 1.25 && em$theta_80_superieur[2L] > 1.25 &&
                  em$theta_80_inferieur[2L] > 0.8) TRUE else "valeurs du mauvais côté du seuil",
       signal = if (identical(a$etape, "homogeneite") && identical(a$code, "ecart_minimal_detectable") &&
                    identical(a$nombre, 2L)) TRUE else "avertissement",
       message = if (identical(a$message, MESSAGE_218) && identical(Encoding(a$message), "UTF-8")) TRUE else a$message,
       sans = if (!nrow(r2$avertissements) &&
                  all(r2$resultat$ecart_minimal$theta_80_superieur < 1.25) &&
                  all(r2$resultat$ecart_minimal$theta_80_inferieur > 0.8)) TRUE else "signal à D = 2000")
})
verifier("I signal (critère 5) : un seul segment au-delà du seuil (S = 3, une cellule, parts 0,1 / 0,45 / 0,45, D = 1500) -> nombre 1 ; theta- du segment 1 sous 1/1,25, segments 2 et 3 dans le seuil", {
  r <- em_de(matrix(c(100, 450, 450), 3L), matrix(c(150, 675, 675), 3L)); em <- r$resultat$ecart_minimal
  l2 <- tablexp:::non_centralite_ecart_minimal(2L)
  sig <- em$theta_80_superieur > 1.25 | em$theta_80_inferieur < 0.8
  tous(valeurs = proche(c(em$theta_80_superieur, em$theta_80_inferieur),
                        c(vapply(c(0.1, 0.45, 0.45), function(p) forme_close(l2, 1500, p)[["superieur"]], 0),
                          vapply(c(0.1, 0.45, 0.45), function(p) forme_close(l2, 1500, p)[["inferieur"]], 0)), 0, 1e-8),
       sig = if (identical(sig, c(TRUE, FALSE, FALSE))) TRUE else paste(sig, collapse = ", "),
       nombre = if (identical(r$avertissements$nombre, 1L) &&
                    startsWith(r$avertissements$message, "Homogénéité : 1 segment(s) pour lesquels")) TRUE else "nombre")
})
verifier("I signal (identité exacte) : construction plan_h() de la batterie (20 000 têtes, sexe) sans signal", {
  !"ecart_minimal_detectable" %in% X$avertissements$code
})
# Critère 3 (calcul exact, sans graine) : S = 2, strate unique, O ~ Bin(D,
# p(theta)), p(theta) = pi theta / (1 + pi (theta - 1)) ; rejet si T =
# (O - D pi)^2 / (D pi (1 - pi)) >= quantile 0,95 du khi-deux à 1 ddl ;
# theta_80(D) du moteur (ecart_minimal_homogeneite() sur score_homogeneite()
# d'une cellule) ; D_0 = arrondi de lambda_80 / (pi (1 - pi) r_c^2), r_c =
# |c - 1| / (1 + pi (c - 1)), fenêtre D_0 - 20 à D_0 + 20.
puissance_exacte_218 <- function(pi, cible) {
  q <- stats::qchisq(0.95, 1)
  rc <- abs(cible - 1) / (1 + pi * (cible - 1))
  D0 <- round(L80_1 / (pi * (1 - pi) * rc^2))
  vapply((D0 - 20):(D0 + 20), function(Dt) {
    sc <- tablexp:::score_homogeneite(c(round(Dt * pi), Dt - round(Dt * pi)), c(pi, 1 - pi), c(1L, 1L), 1:2, 1L, 2L)
    em <- tablexp:::ecart_minimal_homogeneite(sc, "produite", c("produite", "produite"), 2L)
    th <- if (cible > 1) em$superieur[1L] else em$inferieur[1L]
    p <- pi * th / (1 + pi * (th - 1))
    k <- 0:Dt
    sum(stats::dbinom(k, Dt, p)[(k - Dt * pi)^2 / (Dt * pi * (1 - pi)) >= q])
  }, 0)
}
verifier("I critère 3 (calcul exact sans aléa) : puissance exacte en theta_80(D) dans [0,75 ; 0,85] pour chaque D de la fenêtre, moyenne dans [0,76 ; 0,84] ; sous 0,80 pour la cible 1,25 et au-dessus pour la cible 0,80 quand pi < 0,5 ; moyenne dans [0,79 ; 0,81] à pi = 0,5", {
  e <- character(0)
  for (pi in c(0.5, 0.25, 0.10, 0.02)) for (cible in c(1.25, 0.80)) {
    pw <- puissance_exacte_218(pi, cible); m <- mean(pw)
    lib <- sprintf("pi = %.2f, cible %.2f (min %.4f, max %.4f, moyenne %.4f)", pi, cible, min(pw), max(pw), m)
    if (any(pw < 0.75 | pw > 0.85)) e <- c(e, paste(lib, ": puissance hors de [0,75 ; 0,85]"))
    if (m < 0.76 || m > 0.84) e <- c(e, paste(lib, ": moyenne hors de [0,76 ; 0,84]"))
    if (pi == 0.5 && (m < 0.79 || m > 0.81)) e <- c(e, paste(lib, ": moyenne hors de [0,79 ; 0,81]"))
    if (pi < 0.5 && ((cible > 1 && !(m < 0.80)) || (cible < 1 && !(m > 0.80)))) e <- c(e, paste(lib, ": sens"))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Correction du 19/11/2026 (constat C1 de la revue finale de B30 ;
# spécification d'actuary-approfondi, § 4, T1 à T4 et T6 ; décisions du
# mainteneur du 19/11/2026, (a) et (b)) : lambda_s sans soustraction, chaque
# segment référence de sa composante, état precision_insuffisante. Formes
# closes à deux segments en strate unique ou à parts constantes, écrites sans
# soustraction : p part du segment, q part de l'autre (chacune E / E_.),
# r = sqrt(lambda_80 / (D p q)), theta+ = (1 + q r) / (1 - p r),
# theta- = (1 - q r) / (1 + p r) ; theta+ existe si et seulement si p r < 1,
# theta- si et seulement si q r < 1.
forme_close_pq <- function(lambda, D, p, q) {
  r <- sqrt(lambda / (D * p * q))
  c(superieur = (1 + q * r) / (1 - p * r), inferieur = (1 - q * r) / (1 + p * r))
}
expo_seg <- function(seg, age, E, D)
  data.frame(segment = seg, age = as.integer(age), exposition_centrale = E, evenements = as.integer(D),
             stringsAsFactors = FALSE)
verifier("I T1 scénario de l'audit (valeur calculée à la main) : A (E = 1e-6, D = 0) et X (E = 1e5, D = 9000), âges 60 à 70, deux ordres : A theta+ produite, X theta- produite, à 1e-12 de la forme close sans soustraction et des valeurs de la spécification ; A theta- et X theta+ non_atteint ; sans erreur", {
  d <- expo_seg(rep(c("A", "X"), each = 11L), rep(60:70, 2L), rep(c(1e-6, 1e5), each = 11L), rep(c(0, 9000), each = 11L))
  pA <- 1e-6 / (1e-6 + 1e5); pX <- 1e5 / (1e-6 + 1e5)
  fA <- forme_close_pq(L80_1, 99000, pA, pX); fX <- forme_close_pq(L80_1, 99000, pX, pA)
  e <- character(0)
  for (o in list(seq_len(22L), c(12:22, 1:11))) {
    r <- tryCatch(homog(d[o, ], "segment", c(60, 70), 60), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, paste("erreur :", conditionMessage(r))); next }
    em <- r$resultat$ecart_minimal
    a <- match("A", em$segment); x <- match("X", em$segment)
    if (!identical(c(em$etat_superieur[a], em$etat_inferieur[a], em$etat_superieur[x], em$etat_inferieur[x]),
                   c("produite", "non_atteint", "non_atteint", "produite")))
      { e <- c(e, "états"); next }
    v <- c(em$theta_80_superieur[a], em$theta_80_inferieur[x])
    if (!isTRUE(proche(v, c(fA[["superieur"]], fX[["inferieur"]]), 1e-12, 0)) ||
        !isTRUE(proche(v, c(2816.6957177997049, 3.5502592405727154e-4), 1e-12, 0)))
      e <- c(e, paste("valeurs :", paste(format(v, digits = 17), collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# T2 (scénario de /code-review) : âges 60 et 61 ; 60 : E 100 / 100, D 2 / 1 ;
# 61 : E 5000 / 5000 k, D 600 / 0. Recalcul indépendant à deux segments :
# lambda_A(theta) = (somme_c D_c pi_Ac pi_Bc (theta - 1) / (pi_Bc + pi_Ac
# theta))^2 / somme_c D_c pi_Ac pi_Bc (et symétrique pour B), racine par
# uniroot en log theta ; contre-vérification à 80 chiffres de la
# spécification.
lambda_deux <- function(Dc, pa, pb, theta)
  sum(Dc * pa * pb * (theta - 1) / (pb + pa * theta))^2 / sum(Dc * pa * pb)
verifier("I T2 scénario de /code-review (recalcul indépendant et contre-vérification à 80 chiffres) : k dans {1e-11, 1e-10}, deux ordres : B theta+ et A theta- produites, à 1e-10 ; A theta+ et B theta- non_atteint ; sans erreur", {
  ref80 <- list(c(154612185.82495439, 6.4677955017863807e-9), c(15461223.865043793, 6.4677932919715056e-8))
  e <- character(0)
  for (i in 1:2) {
    k <- c(1e-11, 1e-10)[i]
    d <- expo_seg(c("A", "B", "A", "B"), c(60, 60, 61, 61), c(100, 100, 5000, 5000 * k), c(2, 1, 600, 0))
    Ec <- c(200, 5000 + 5000 * k); pa <- c(100, 5000) / Ec; pb <- c(100, 5000 * k) / Ec; Dc <- c(3, 600)
    bp <- exp(stats::uniroot(function(u) lambda_deux(Dc, pb, pa, exp(u)) - L80_1, c(0, 30), tol = 1e-14)$root)
    am <- exp(stats::uniroot(function(u) lambda_deux(Dc, pa, pb, exp(u)) - L80_1, c(-30, 0), tol = 1e-14)$root)
    for (o in list(1:4, c(2L, 1L, 4L, 3L))) {
      r <- tryCatch(homog(d[o, ], "segment", c(60, 61), 60), error = function(x) x)
      if (inherits(r, "error")) { e <- c(e, sprintf("k %g : erreur %s", k, conditionMessage(r))); next }
      em <- r$resultat$ecart_minimal
      a <- match("A", em$segment); b <- match("B", em$segment)
      if (!identical(c(em$etat_superieur[b], em$etat_inferieur[a], em$etat_superieur[a], em$etat_inferieur[b]),
                     c("produite", "produite", "non_atteint", "non_atteint")))
        { e <- c(e, sprintf("k %g : états", k)); next }
      v <- c(em$theta_80_superieur[b], em$theta_80_inferieur[a])
      if (!isTRUE(proche(v, c(bp, am), 1e-10, 0)) || !isTRUE(proche(v, ref80[[i]], 1e-10, 0)))
        e <- c(e, sprintf("k %g : %s", k, paste(format(v, digits = 17), collapse = ", ")))
    }
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# T3 (invariance à l'ordre des segments) : cinq segments, deux âges ; les 120
# ordres de première apparition des segments dans la table d'exposition.
E_T3 <- rbind(c(0, 8.44e5), c(1.46e-5, 0), c(0, 3.56e4), c(1.66e-6, 6.14e-8), c(0.167, 4.99e-10))
D_T3 <- rbind(c(0, 60), c(9, 0), c(0, 4026), c(211, 2654), c(9881, 577))
permutations <- function(x) if (length(x) <= 1L) list(x) else
  do.call(c, lapply(seq_along(x), function(i) lapply(permutations(x[-i]), function(p) c(x[i], p))))
verifier("I T3 invariance à l'ordre des segments (contre-vérification à 80 chiffres) : 5 segments, 120 ordres : aucune erreur, états identiques, theta à 1e-12 de l'ordre naturel ; theta+ des segments 5 et 4 et theta- du segment 5 à 1e-12 des valeurs à 80 chiffres", {
  d <- expo_seg(rep(sprintf("s%d", 1:5), 2L), rep(c(50, 51), each = 5L), as.vector(E_T3), as.vector(D_T3))
  par_segment <- function(o) {
    r <- tryCatch(homog(d[c(o, o + 5L), ], "segment", c(50, 51), 50, B = 9), error = function(x) x)
    if (inherits(r, "error")) return(conditionMessage(r))
    em <- r$resultat$ecart_minimal
    em[match(sprintf("s%d", 1:5), em$segment), ]
  }
  ref <- par_segment(1:5)
  if (is.character(ref)) return(paste("ordre naturel : erreur", ref))
  e <- character(0)
  for (o in permutations(1:5)) {
    em <- par_segment(o)
    lib <- paste(o, collapse = "")
    if (is.character(em)) { e <- c(e, paste(lib, ": erreur", em)); next }
    if (!identical(c(em$etat_superieur, em$etat_inferieur), c(ref$etat_superieur, ref$etat_inferieur)))
      { e <- c(e, paste(lib, ": états")); next }
    v <- c(em$theta_80_superieur, em$theta_80_inferieur); v0 <- c(ref$theta_80_superieur, ref$theta_80_inferieur)
    if (!identical(is.na(v), is.na(v0)) || !isTRUE(proche(v[!is.na(v)], v0[!is.na(v0)], 1e-12, 0)))
      e <- c(e, paste(lib, ": valeurs"))
  }
  tous(ordres = if (length(e)) paste(e, collapse = " ; ") else TRUE,
       ref80 = proche(c(ref$theta_80_superieur[5:4], ref$theta_80_inferieur[5L]),
                      c(18090774.751333403, 11.904576012616003, 0.22295785153163347), 1e-12, 0))
})
verifier("I T4 balayage à deux segments en strate unique (valeur calculée à la main) : p = 1e-1 à 1e-15, D dans {1, 1e3, 1e6, 1e9, 1e12}, deux ordres : aucune erreur ; états conformes à (p r < 1, q r < 1) ; theta à 1e-12 de la forme close ; écart entre ordres au plus 1e-14", {
  e <- character(0)
  for (p0 in 10^-(1:15)) for (Dt in c(1, 1e3, 1e6, 1e9, 1e12)) {
    E <- c(p0, 1); p <- E[1L] / sum(E); q <- E[2L] / sum(E)
    r <- sqrt(L80_1 / (Dt * p * q))
    f1 <- forme_close_pq(L80_1, Dt, p, q); f2 <- forme_close_pq(L80_1, Dt, q, p)
    # Attendu pour le segment de part p puis pour l'autre : (theta+, theta-).
    att_et <- ifelse(c(p * r, q * r, q * r, p * r) < 1, "produite", "non_atteint")
    att_v <- c(f1[["superieur"]], f1[["inferieur"]], f2[["superieur"]], f2[["inferieur"]])
    att_v[att_et != "produite"] <- NA
    obt <- list()
    for (o in list(1:2, 2:1)) {
      sc <- tablexp:::score_homogeneite(c(0, Dt)[o], E[o], c(1L, 1L), 1:2, 1L, 2L)
      em <- tryCatch(tablexp:::ecart_minimal_homogeneite(sc, "produite", c("produite", "produite"), 2L), error = function(x) x)
      lib <- sprintf("p %g, D %g, ordre %s", p0, Dt, paste(o, collapse = ""))
      if (inherits(em, "error")) { e <- c(e, paste(lib, ": erreur", conditionMessage(em))); next }
      k <- match(1:2, o)
      et <- c(em$etat_superieur[k[1L]], em$etat_inferieur[k[1L]], em$etat_superieur[k[2L]], em$etat_inferieur[k[2L]])
      v <- c(em$superieur[k[1L]], em$inferieur[k[1L]], em$superieur[k[2L]], em$inferieur[k[2L]])
      if (!identical(et, att_et)) { e <- c(e, paste(lib, ": états", paste(et, collapse = "/"))); next }
      if (!identical(is.na(v), is.na(att_v)) || !isTRUE(proche(v[!is.na(v)], att_v[!is.na(att_v)], 1e-12, 0)))
        { e <- c(e, paste(lib, ": valeurs")); next }
      obt[[length(obt) + 1L]] <- v
    }
    if (length(obt) == 2L && !isTRUE(proche(obt[[1L]][!is.na(obt[[1L]])], obt[[2L]][!is.na(obt[[2L]])], 1e-14, 0)))
      e <- c(e, sprintf("p %g, D %g : écart entre ordres", p0, Dt))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# precision_insuffisante par la voie de T6 (postcondition : résidu au-delà
# de la tolérance pour une racine très proche de 1 ; en D = 1e20, résidus
# mesurés de 3,7e-7 et -3,0e-8 ; en
# strate unique 50 / 50, à partir d'un total de l'ordre de 1e17 décès dans la
# strate (entre 2,5e17 et 3e17 pour les premières branches)) est hors de
# portée de calculer_homogeneite() : les décès y sont des entiers d'au plus
# 2^31 - 1 par ligne, une ligne par segment et par cellule ; le test global
# y reste exact (T = 0, O = A à parts égales) ; appel direct de
# ecart_minimal_homogeneite(). La voie du débordement de D_c |h_c| en
# -infini est supprimée (C1 d'actuary, décision du mainteneur du 20/11/2026 ;
# T7). Motif de l'état (#340 ; Q4 d'actuary-approfondi, amendée par l'avis
# d'actuary de l'étape (2)) : racine non calculable par cette méthode en
# double précision à cette tolérance, qu'elle soit ou non représentable ;
# chaque voie a son test (C3) : pivot non fini ou non positif (« I #338 Q2
# pivots en échec ») ; limite NA, NaN ou -Inf (« I #338 Q1 limite
# indéterminée ») ; non-convergence d'uniroot (« I F339 non-convergence
# forcée ») ; postcondition non tenue, par le résidu (T6) et par theta non
# fini ou nul (« I #338 Q1 limite +Inf, racine non représentable » : signe 1,
# exp(800) = Inf ; signe -1, exp(-800) = 0). L'état reste
# atteignable par l'étape dans des cas extrêmes : racine non représentable
# (theta+ au-delà du plus grand double). Une part d'exposition dénormalisée
# n'est pas, par elle-même, une voie : la postcondition contrôle lambda
# calculée sur la part arrondie et tient (cas mesurés) ; la justesse de theta
# par rapport aux parts exactes est dégradée, l'état reste produite (avis
# d'actuary, #338 ; E_X = 5e-307 du scénario de Q1 ci-dessous). Une limite
# lambda(-+infini) qui déborde en
# +infini (b^2/d) quand la part d'un segment avoisine 1e-308 ne mène plus à
# precision_insuffisante : elle compte au-delà du seuil et la racine est
# résolue (Q1 de #338 ; test « I #338 scénario de Q1 » ci-dessous).
# D = 1e15 plutôt que 1e17, cas frontière où un ulp de décalage de la libm
# ferait dépasser 1e-8 (C2 d'actuary).
verifier("I T6 témoin precision_insuffisante (valeur calculée à la main) : strate unique 50 / 50 ; D = 1e20 : quatre branches precision_insuffisante, NA, sans erreur ; D = 1e15 : produites, postcondition |lambda(theta) / lambda_80 - 1| <= 1e-8 (recalcul indépendant)", {
  em_D <- function(Dt) {
    sc <- tablexp:::score_homogeneite(c(Dt / 2, Dt / 2), c(50, 50), c(1L, 1L), 1:2, 1L, 2L)
    tryCatch(tablexp:::ecart_minimal_homogeneite(sc, "produite", c("produite", "produite"), 2L), error = function(x) x)
  }
  a <- em_D(1e20); b <- em_D(1e15)
  if (inherits(a, "error") || inherits(b, "error")) return("erreur")
  rel <- vapply(c(b$superieur, b$inferieur), function(th) lambda_deux(1e15, 0.5, 0.5, th) / L80_1 - 1, 0)
  tous(insuffisante = if (identical(c(a$etat_superieur, a$etat_inferieur), rep("precision_insuffisante", 4L)) &&
                          all(is.na(c(a$superieur, a$inferieur)))) TRUE else paste(c(a$etat_superieur, a$etat_inferieur), collapse = "/"),
       produite = if (identical(c(b$etat_superieur, b$etat_inferieur), rep("produite", 4L)) &&
                      all(b$superieur > 1) && all(b$inferieur < 1)) TRUE else "états à D = 1e15",
       postcondition = if (all(abs(rel) <= 1e-8)) TRUE else paste(format(rel), collapse = ", "))
})
# T7 (C1 d'actuary, décision du mainteneur du 20/11/2026, (b)) : part de X
# voisine de 1e-308 ou dénormalisée à l'âge 60 (E_X dans {5e-305, 5e-307}) ;
# |h_c| y vaut environ 1e308 ou plus en -infini, et D_c |h_c| débordait
# (theta- de A precision_insuffisante alors que la racine est représentable).
# Recalcul indépendant par la forme à deux segments sans soustraction
# (lambda_deux) ; contre-vérification d'actuary (calcul décimal) : theta- de A
# voisin de 6,468e-306 pour E_X = 5e-305.
verifier("I T7 débordement de D_c |h_c| en -infini (recalcul indépendant ; contre-vérification d'actuary) : A (5000, 600 ; 100, 2), X (E_X, 0 ; 100, 1), E_X dans {5e-305, 5e-307}, deux ordres : A theta- et X theta+ produites, à 1e-10 du recalcul ; A theta+ et X theta- non_atteint ; theta- de A à 1e-4 de 6,468e-306 pour E_X = 5e-305 ; sans erreur", {
  e <- character(0)
  for (eX in c(5e-305, 5e-307)) {
    d <- expo_seg(c("A", "X", "A", "X"), c(60, 60, 61, 61), c(5000, eX, 100, 100), c(600, 0, 2, 1))
    Ec <- c(5000 + eX, 200); pa <- c(5000, 100) / Ec; px <- c(eX, 100) / Ec; Dc <- c(600, 3)
    am <- exp(stats::uniroot(function(u) lambda_deux(Dc, pa, px, exp(u)) - L80_1, c(-740, 0), tol = 1e-14)$root)
    xp <- exp(stats::uniroot(function(u) lambda_deux(Dc, px, pa, exp(u)) - L80_1, c(0, 709), tol = 1e-14)$root)
    for (o in list(1:4, c(2L, 1L, 4L, 3L))) {
      r <- tryCatch(homog(d[o, ], "segment", c(60, 61), 60), error = function(x) x)
      if (inherits(r, "error")) { e <- c(e, sprintf("E_X %g : erreur %s", eX, conditionMessage(r))); next }
      em <- r$resultat$ecart_minimal
      a <- match("A", em$segment); x <- match("X", em$segment)
      et <- c(em$etat_inferieur[a], em$etat_superieur[x], em$etat_superieur[a], em$etat_inferieur[x])
      if (!identical(et, c("produite", "produite", "non_atteint", "non_atteint")))
        { e <- c(e, sprintf("E_X %g : états %s", eX, paste(et, collapse = "/"))); next }
      v <- c(em$theta_80_inferieur[a], em$theta_80_superieur[x])
      if (!isTRUE(proche(v, c(am, xp), 1e-10, 0)))
        e <- c(e, sprintf("E_X %g : %s", eX, paste(format(v, digits = 17), collapse = ", ")))
      if (eX == 5e-305 && !isTRUE(proche(v[1L], 6.468e-306, 1e-4, 0)))
        e <- c(e, sprintf("E_X %g : contre-vérification %s", eX, format(v[1L], digits = 17)))
    }
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Strate où s est seul exposé sur deux lignes (C1 d'actuary, visa de la
# correction) : âge 60, A sur deux lignes (E 100, D 5 chacune) ; âge 61, A
# (100, 5) et X (100, 9). Avant #335, la strate 60 était informative (deux
# lignes exposées) avec a_c = 0 pour A : |h| y valait 0, sans terme 1/0 en
# -infini ; depuis #335, les deux lignes sont agrégées et la strate 60 n'est
# plus informative (un seul segment exposé). Recalcul indépendant restreint à
# la cellule où les deux segments sont exposés (âge 61 : D 14, parts 1/2 et
# 1/2).
verifier("I strate à un seul segment exposé sur deux lignes (recalcul indépendant) : deux ordres ; theta- de A et de X produites, égales, à 1e-12 du recalcul et de 0.14367128799545387 ; signal ecart_minimal_detectable", {
  d <- expo_seg(c("A", "A", "A", "X"), c(60, 60, 61, 61), rep(100, 4L), c(5, 5, 5, 9))
  att <- exp(stats::uniroot(function(u) lambda_deux(14, 0.5, 0.5, exp(u)) - L80_1, c(-30, 0), tol = 1e-14)$root)
  e <- character(0)
  for (o in list(1:4, c(4L, 3L, 1L, 2L))) {
    r <- tryCatch(homog(d[o, ], "segment", c(60, 61), 60), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, paste("erreur :", conditionMessage(r))); next }
    em <- r$resultat$ecart_minimal[match(c("A", "X"), r$resultat$ecart_minimal$segment), ]
    if (!identical(em$etat_inferieur, c("produite", "produite")))
      { e <- c(e, paste("états", paste(em$etat_inferieur, collapse = "/"))); next }
    if (!isTRUE(proche(em$theta_80_inferieur, rep(att, 2L), 1e-12, 0)) ||
        !isTRUE(proche(em$theta_80_inferieur, rep(0.14367128799545387, 2L), 1e-12, 0)))
      e <- c(e, paste("valeurs", paste(format(em$theta_80_inferieur, digits = 17), collapse = ", ")))
    if (!"ecart_minimal_detectable" %in% r$avertissements$code) e <- c(e, "signal absent")
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Invariance au découpage d'une cellule en plusieurs lignes (constat C-R1 de
# l'audit ; décision du mainteneur du 20/11/2026, (a) : pi_jc cumulé par
# (strate, segment) ; #335 : lignes saines agrégées par (segment, cellule)
# dans calculer_homogeneite(), forme F335, spécification d'actuary-approfondi,
# Q1, critères I1 et I3). Âges 60 et 61 ; une ligne : A (100, 4), X (300, 9),
# A (200, 6), X (100, 3). Découpages exacts en double (150 + 50, 30 + 70) de
# (A, 61) et de (X, 61), le segment découpé en tête ou non, dans plusieurs
# ordres. Le résultat entier (tests, dont p exacte et p de Monte-Carlo,
# segments, dont z et états, ecart_minimal, diagnostics, avertissements) est
# identical() à celui de la table à une ligne prise dans l'ordre de première
# occurrence des couples (segment, âge) de la variante (I1 ; la p de
# Monte-Carlo suit l'ordre des lignes de chaque strate, I2). Attendus
# numériques : valeurs du code de 303d6e0 sur une ligne (T, theta), p exacte
# et z remesurées à 368e110 sur une ligne. Mesure adverse (C7) : à 368e110,
# (X, 61) découpé avec A en tête (variante X_decoupe_A_en_tete) donnait T =
# 0.11369378473976757, p exacte 0.82977183901774476, p de Monte-Carlo 0.85 et
# z de X -0.33718508973524852 ; la contrainte « segment découpé en tête » de
# ce test est levée.
une_ligne_ordonnee <- function(un, variante) {
  cle <- function(d) paste(d$segment, d$age)
  un[match(unique(cle(variante)), cle(un)), ]
}
verifier("I invariance au découpage d'une cellule en plusieurs lignes (#335, I1, I3) : A puis X découpé, en tête ou non, plusieurs ordres : résultat identical() à la table à une ligne dans l'ordre de première occurrence ; T, p exacte, p de Monte-Carlo, z, états et theta de la variante sur une ligne", {
  un <- expo_seg(c("A", "X", "A", "X"), c(60, 60, 61, 61), c(100, 300, 200, 100), c(4, 9, 6, 3))
  da <- expo_seg(c("A", "X", "A", "X", "A"), c(60, 60, 61, 61, 61), c(100, 300, 150, 100, 50), c(4, 9, 4, 3, 2))
  dx <- expo_seg(c("X", "X", "A", "A", "X"), c(61, 60, 61, 60, 61), c(70, 300, 200, 100, 30), c(2, 9, 6, 4, 1))
  dxa <- expo_seg(c("A", "X", "A", "X", "X"), c(60, 60, 61, 61, 61), c(100, 300, 200, 30, 70), c(4, 9, 6, 1, 2))
  variantes <- list(une_ligne = un, A_decoupe = da, A_decoupe_permute = da[c(1L, 2L, 5L, 4L, 3L), ],
                    A_decoupe_hors_tete = da[c(2L, 1L, 4L, 3L, 5L), ], X_decoupe = dx,
                    X_decoupe_permute = dx[c(5L, 4L, 2L, 3L, 1L), ], X_decoupe_A_en_tete = dxa)
  att_v <- c(3.7393758326640887, 5.0691740146480493, 0.19727079739428313, 0.26742430949700979)
  e <- character(0)
  for (nom in names(variantes)) {
    va <- variantes[[nom]]
    r <- tryCatch(homog(va, "segment", c(60, 61), 60), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, paste(nom, ": erreur", conditionMessage(r))); next }
    r1 <- homog(une_ligne_ordonnee(un, va), "segment", c(60, 61), 60)
    if (!identical(r, r1)) e <- c(e, paste(nom, ": résultat différent de la table à une ligne"))
    em <- r$resultat$ecart_minimal[match(c("A", "X"), r$resultat$ecart_minimal$segment), ]
    if (!identical(c(em$etat_superieur, em$etat_inferieur), rep("produite", 4L)))
      { e <- c(e, paste(nom, ": états", paste(c(em$etat_superieur, em$etat_inferieur), collapse = "/"))); next }
    v <- c(em$theta_80_superieur, em$theta_80_inferieur)
    if (!isTRUE(proche(v, att_v, 1e-12, 0)))
      e <- c(e, paste(nom, ": theta", paste(format(v, digits = 17), collapse = ", ")))
    g <- ligne_test(r, "global")
    if (!isTRUE(proche(g$statistique, 0.12676056338028169, 1e-12, 0))) e <- c(e, paste(nom, ": T"))
    if (!isTRUE(proche(g$p_exacte, 0.81249998595734196, 1e-12, 0))) e <- c(e, paste(nom, ": p exacte"))
    sg <- r$resultat$segments[r$resultat$segments$portee == "global", ]
    if (!identical(sg$etat, c("produite", "produite")) ||
        !isTRUE(proche(sg$z[sg$segment == "X"], -0.35603449745815596, 1e-12, 0)))
      e <- c(e, paste(nom, ": z ou états des segments"))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
verifier("I invariance au découpage (#335, I1) : segment seul dans sa strate (Y à 62 ans, 250 / 5 deux fois contre 500 / 10), A seul à 60 ans (100 / 5 deux fois contre 200 / 10, cas de #336), ligne (A, 60, E = 0, D = 0) ajoutée : résultat identical() à la table à une ligne dans l'ordre de première occurrence", {
  un <- expo_seg(c("A", "X", "A", "X"), c(60, 60, 61, 61), c(100, 300, 200, 100), c(4, 9, 6, 3))
  y1 <- rbind(un, expo_seg("Y", 62, 500, 10))
  y2 <- rbind(un[1:2, ], expo_seg("Y", 62, 250, 5), un[3:4, ], expo_seg("Y", 62, 250, 5))
  a1 <- expo_seg(c("A", "A", "X"), c(60, 61, 61), c(200, 100, 100), c(10, 5, 9))
  a2 <- expo_seg(c("A", "A", "A", "X"), c(60, 61, 60, 61), c(100, 100, 100, 100), c(5, 5, 5, 9))
  z0 <- rbind(un, expo_seg("A", 60, 0, 0))
  cas <- list(Y_seul = list(y2, y1, c(60, 62), 0.12676056338028169, 0.81249998595734196),
              A_seul = list(a2, a1, c(60, 61), 1.1428571428571428, 0.42395019531249978),
              ligne_nulle = list(z0, un, c(60, 61), 0.12676056338028169, 0.81249998595734196))
  e <- character(0)
  for (nom in names(cas)) {
    k <- cas[[nom]]
    r <- tryCatch(homog(k[[1L]], "segment", k[[3L]], 60), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, paste(nom, ": erreur", conditionMessage(r))); next }
    if (!identical(r, homog(une_ligne_ordonnee(k[[2L]], k[[1L]]), "segment", k[[3L]], 60)))
      e <- c(e, paste(nom, ": résultat différent de la table à une ligne"))
    g <- ligne_test(r, "global")
    if (!isTRUE(proche(c(g$statistique, g$p_exacte), c(k[[4L]], k[[5L]]), 1e-12, 0)))
      e <- c(e, paste(nom, ": T ou p exacte", paste(format(c(g$statistique, g$p_exacte), digits = 17), collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
verifier("I invariance au découpage (#335, I1) : deux variables (sexe, fumeur), cellule (F, n, 61) découpée hors de tête : résultat identical() (tests par variable compris) à la table à une ligne dans l'ordre de première occurrence", {
  g <- expand.grid(fumeur = c("o", "n"), sexe = c("F", "H"), age = 60:62, stringsAsFactors = FALSE)[, c("sexe", "fumeur", "age")]
  g$exposition_centrale <- c(120, 340, 90, 410, 150, 300, 110, 380, 130, 360, 100, 400)
  g$evenements <- c(3L, 6L, 4L, 9L, 5L, 7L, 4L, 11L, 6L, 8L, 5L, 12L)
  k <- which(g$sexe == "F" & g$fumeur == "n" & g$age == 61)
  dec <- rbind(g[-k, ], g[k, ], g[k, ])
  dec$exposition_centrale[nrow(dec) - 1:0] <- c(100, 200)
  dec$evenements[nrow(dec) - 1:0] <- c(2L, 5L)
  ref <- g[c(setdiff(seq_len(nrow(g)), k), k), ]
  r <- homog(dec, c("sexe", "fumeur"), c(60, 62), 60)
  r1 <- homog(ref, c("sexe", "fumeur"), c(60, 62), 60)
  tous(variables = if (identical(sum(r$resultat$tests$test == "variable"), 2L) &&
                       all(r$resultat$tests$etat == "produite")) TRUE else "tests par variable non produits",
       identique = if (identical(r, r1)) TRUE else "résultat différent de la table à une ligne")
})
verifier("I score_homogeneite() : deux lignes de même (strate, groupe) -> erreur de programmation (précondition, #335, I4)", {
  err <- tryCatch(tablexp:::score_homogeneite(c(1, 2, 3), c(10, 20, 30), c(1L, 1L, 1L), c(1L, 2L, 2L), 1L, 2L),
                  error = function(x) x)
  inherits(err, "error") && !inherits(err, "refus_tablexp") &&
    grepl("erreur de programmation", conditionMessage(err), fixed = TRUE) &&
    grepl("(strate, groupe)", conditionMessage(err), fixed = TRUE)
})
# Strates informatives sur les lignes agrégées (#336 ; spécification
# d'actuary-approfondi, Q2 et D8, commentaire 7068050253, § 4 ; avis d'actuary
# du 21/11/2026 : mécanisme réglé par #335, critère ne >= 2 de
# score_homogeneite() inchangé). Une strate est informative si D_. > 0 et si
# au moins deux segments (groupes) distincts y sont exposés. Attendus :
# valeurs mesurées sur le moteur de f0c61e8 ; mesures adverses (C7) sur
# d1517f2 (avant #335), citées dans chaque test.
# Test 1 (cas de l'issue) : âge 60, A seul, sur deux lignes 100 / 5 ou une
# ligne 200 / 10 ; âge 61, A (100, 5) et X (100, 9). À d1517f2, la variante à
# deux lignes donnait strates_informatives 2, cellules_informatives 2,
# cellules_non_informatives 0 et p de Monte-Carlo 0.39.
verifier("I #336 test 1 : A seul à 60 ans, une ou deux lignes : diagnostics cellules 2, informatives 1, non informatives 1, sans décès 0 ; strates_informatives 1 (global, tranche, somme) ; T, p exacte, p de Monte-Carlo 0.37 ; deux lignes identical() à une ligne", {
  a1 <- expo_seg(c("A", "A", "X"), c(60, 61, 61), c(200, 100, 100), c(10, 5, 9))
  a2 <- expo_seg(c("A", "A", "A", "X"), c(60, 61, 60, 61), c(100, 100, 100, 100), c(5, 5, 5, 9))
  e <- character(0)
  for (nom in c("a1", "a2")) {
    r <- tryCatch(homog(get(nom), "segment", c(60, 61), 60), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, paste(nom, ": erreur", conditionMessage(r))); next }
    dg <- unlist(r$diagnostics[c("cellules", "cellules_informatives", "cellules_non_informatives", "cellules_sans_deces")])
    if (!identical(unname(dg), c(2L, 1L, 1L, 0L))) e <- c(e, paste(nom, ": diagnostics", paste(dg, collapse = "/")))
    t <- r$resultat$tests
    if (!identical(t$test, c("global", "tranche", "somme_tranches")) || !identical(t$strates_informatives, rep(1L, 3L)))
      e <- c(e, paste(nom, ": strates_informatives", paste(t$strates_informatives, collapse = "/")))
    if (!isTRUE(proche(c(t$statistique[1L], t$p_exacte[1L], t$p_monte_carlo),
                       c(1.1428571428571428, 0.42395019531249978, rep(0.37, 3L)), 1e-12, 0)))
      e <- c(e, paste(nom, ": T, p exacte ou p de Monte-Carlo"))
  }
  if (!identical(homog(a2, "segment", c(60, 61), 60), homog(a1, "segment", c(60, 61), 60)))
    e <- c(e, "a2 : résultat différent de a1")
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Test 2 (D8 : variance_nulle gardé, sans signal, sans état nouveau) : A et X
# aux âges 60 et 61, Y seul à 62 ans, sur une ligne 500 / 10 (y1) ou deux
# lignes 250 / 5 (y2). Y n'est exposé dans aucune strate informative : V_YY =
# 0 (résultat exact), z et theta_80 non définis. À d1517f2, y2 donnait
# groupes 3, strates_informatives 3, p exacte NA (plus_de_deux_groupes), p de
# Monte-Carlo 0.78, Y produite (z = 0), theta de Y non_atteint aux deux
# branches et un signal ecart_minimal_detectable de nombre 3.
verifier("I #336 test 2 (D8) : Y seul à 62 ans, une ou deux lignes : groupes et strates_informatives 2, p exacte produite ; Y variance_nulle (z NA) aux portées globale et tranche ; theta de Y variance_nulle aux deux branches ; signal ecart_minimal_detectable de nombre 2 (A et X) ; deux lignes identical() à une ligne", {
  un <- expo_seg(c("A", "X", "A", "X"), c(60, 60, 61, 61), c(100, 300, 200, 100), c(4, 9, 6, 3))
  y1 <- rbind(un, expo_seg("Y", 62, 500, 10))
  y2 <- rbind(un[1:2, ], expo_seg("Y", 62, 250, 5), un[3:4, ], expo_seg("Y", 62, 250, 5))
  e <- character(0)
  for (nom in c("y1", "y2")) {
    r <- tryCatch(homog(get(nom), "segment", c(60, 62), 60), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, paste(nom, ": erreur", conditionMessage(r))); next }
    g <- ligne_test(r, "global")
    if (!identical(c(g$groupes, g$strates_informatives), c(2L, 2L)) || !identical(g$etat_p_exacte, "produite") ||
        !isTRUE(proche(c(g$p_exacte, g$p_monte_carlo), c(0.81249998595734196, 0.83), 1e-12, 0)))
      e <- c(e, paste(nom, ": test global"))
    s <- r$resultat$segments[r$resultat$segments$segment == "Y", ]
    if (!identical(s$portee, c("global", "tranche")) || !identical(s$etat, rep("variance_nulle", 2L)) || !all(is.na(s$z)))
      e <- c(e, paste(nom, ": Y aux segments", paste(s$etat, collapse = "/")))
    em <- r$resultat$ecart_minimal[r$resultat$ecart_minimal$segment == "Y", ]
    if (!identical(c(em$etat_superieur, em$etat_inferieur), rep("variance_nulle", 2L)) ||
        !all(is.na(c(em$theta_80_superieur, em$theta_80_inferieur))))
      e <- c(e, paste(nom, ": theta de Y", em$etat_superieur, em$etat_inferieur))
    av <- r$avertissements
    if (!identical(av$code, "ecart_minimal_detectable") || !identical(as.integer(av$nombre), 2L))
      e <- c(e, paste(nom, ": avertissements", paste(av$code, av$nombre, collapse = "/")))
  }
  if (!identical(homog(y2, "segment", c(60, 62), 60), homog(y1, "segment", c(60, 62), 60)))
    e <- c(e, "y2 : résultat différent de y1")
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Test 3 (test par variable) : sexe x fumeur, âges 60 et 61 ; (H, o, 60) à E =
# 0, D = 0. Deux découpages, chacun comparé à la table à une ligne : (H, n, 60)
# en deux lignes 200 / 4 et 210 / 5 (au lieu de 410 / 9) ; (F, o, 60) en deux
# lignes 50 / 1 et 70 / 2 (au lieu de 120 / 3). Test fumeur : la strate
# (60, H) n'a qu'une modalité exposée (n) ; test sexe : la strate (60, o) n'a
# qu'un sexe exposé (F) ; découpées sur deux lignes, elles ne sont pas
# informatives. À d1517f2, strates_informatives (global, sexe, fumeur) valait
# 2 3 4 pour le premier découpage et 2 4 3 pour le second (constat m1 de
# l'audit léger de 8ab9193).
verifier("I #336 test 3 (test par variable) : strate cellule x modalité à une seule modalité exposée sur deux lignes non informative, (H, n, 60) ou (F, o, 60) découpée : strates_informatives 3 (sexe), 3 (fumeur), 2 (global) ; deux lignes identical() à une ligne", {
  g <- expand.grid(fumeur = c("o", "n"), sexe = c("F", "H"), age = 60:61, stringsAsFactors = FALSE)[, c("sexe", "fumeur", "age")]
  g$exposition_centrale <- c(120, 340, 0, 410, 150, 300, 110, 380)
  g$evenements <- c(3L, 6L, 0L, 9L, 5L, 7L, 4L, 11L)
  r1 <- homog(g, c("sexe", "fumeur"), c(60, 61), 60)
  decoupe <- list(Hn60 = list(sexe = "H", fumeur = "n", E = c(200, 210), D = c(4L, 5L)),
                  Fo60 = list(sexe = "F", fumeur = "o", E = c(50, 70), D = c(1L, 2L)))
  e <- character(0)
  for (nom in names(decoupe)) {
    x <- decoupe[[nom]]
    k <- which(g$sexe == x$sexe & g$fumeur == x$fumeur & g$age == 60)
    v2 <- rbind(g, g[k, ])
    v2$exposition_centrale[c(k, nrow(v2))] <- x$E
    v2$evenements[c(k, nrow(v2))] <- x$D
    r <- homog(v2, c("sexe", "fumeur"), c(60, 61), 60)
    t <- r$resultat$tests
    si <- c(global = t$strates_informatives[t$test == "global"],
            sexe = t$strates_informatives[t$test == "variable" & t$variable %in% "sexe"],
            fumeur = t$strates_informatives[t$test == "variable" & t$variable %in% "fumeur"])
    if (!identical(unname(si), c(2L, 3L, 3L)))
      e <- c(e, paste(nom, ": strates_informatives", paste(names(si), si, collapse = ", ")))
    if (!all(t$etat == "produite")) e <- c(e, paste(nom, ": tests non produits"))
    if (!identical(r, r1)) e <- c(e, paste(nom, ": résultat différent de la table à une ligne"))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Ordre de première occurrence de l'agrégation (#335 ; constat C1 de l'audit
# de l'étape (1) de B33) : sans couple (segment, cellule) répété, le vecteur
# agrégé reprend les lignes saines dans leur ordre, dont dépend le flux des
# réplications. Trois segments, âges 60 à 62, lignes mélangées ; ordre de
# première occurrence des segments A, C, B ; dans les cellules, A, B, C à 60
# ans, C, A, B à 61 ans, B, C, A à 62 ans. Attendus : valeurs remesurées
# sur d1517f2 (moteur d'avant #335, sans agrégation ; résultat identical() à
# celui de f0c61e8, mesuré). Un rangement des lignes agrégées par (cellule,
# segment) laisse T et z inchangés mais donnait p de Monte-Carlo 0.91, 0.87,
# 0.53 et 0.80 (mutant construit sur une copie).
verifier("I ordre de première occurrence de l'agrégation (#335, C1) : trois segments, ordre des segments différent d'une cellule à l'autre, sans doublon : T, p de Monte-Carlo, erreur de Monte-Carlo et z des valeurs de d1517f2", {
  d <- expo_seg(c("A", "C", "B", "B", "A", "C", "C", "A", "B"), c(60, 61, 60, 62, 61, 60, 62, 62, 61),
                c(100, 220, 250, 260, 120, 180, 150, 140, 300), c(3, 8, 9, 10, 2, 4, 3, 6, 7))
  r <- homog(d, "segment", c(60, 62), c(60, 62))
  t <- r$resultat$tests
  s <- r$resultat$segments
  tous(tests = if (identical(t$test, c("global", "tranche", "tranche", "somme_tranches")) &&
                   identical(t$etat, rep("produite", 4L))) TRUE else "lignes de test",
       T = if (isTRUE(proche(t$statistique, c(0.22215301095965140, 0.29585222839658776, 1.31405436668594544,
                                              1.60990659508253309), 1e-12, 0))) TRUE else "T",
       p_monte_carlo = if (isTRUE(proche(t$p_monte_carlo, c(0.96, 0.85, 0.48, 0.83), 1e-12, 0))) TRUE
                       else paste(t$p_monte_carlo, collapse = ", "),
       erreur = if (isTRUE(proche(t$erreur_monte_carlo, c(0.019497692171126312, 0.035528157846981032,
                                                          0.049709556425299145, 0.037374991638795055), 1e-12, 0))) TRUE
                else "erreur de Monte-Carlo",
       z = if (identical(s$segment, rep(c("A", "C", "B"), 3L)) &&
               isTRUE(proche(s$z, c(-0.014533869254678681, -0.436273554792803164, 0.417352162176897690,
                                    -0.537406223742030398, 0.265098964829306671, 0.168836372798534973,
                                    0.612840174139278604, -1.123902973898032842, 0.467870382052322953),
                             1e-12, 0))) TRUE else "z")
})
# Textes du rapport (spécification, § 8 ; M546, P1-c).
verifier("I MENTIONS_RAPPORT (littéral de la spécification, § 8) : suffisance_intro, suffisance_homogeneite, suffisance_homogeneite_absente ; homogeneite_portee : deux premières phrases inchangées, puis l'écart minimal détectable", {
  m <- tablexp:::MENTIONS_RAPPORT
  intro <- paste("Indicateurs de suffisance (E2.15 : exposition et événements par âge et par segment, largeur des intervalles,",
                 "écart minimal détectable du test d'homogénéité) : ils signalent ce que les données portent, sans rien",
                 "interdire. Le rapport n'en calcule aucun lui-même ; ceux que l'objet porte sont aux chemins suivants :")
  oui <- paste("Écart minimal détectable par segment du test d'homogénéité : etapes/homogeneite/resultat/ecart_minimal",
               "(section Étape homogeneite) ; signal ecart_minimal_detectable aux avertissements.")
  non <- "Écart minimal détectable par segment du test d'homogénéité : non demandé par le plan."
  portee <- paste(
    "Test d'homogénéité entre segments : hypothèse nulle, dans chaque cellule de la plage (âge, et année civile si",
    "par_annee), un taux commun à tous les segments, sur l'exposition centrale ; tests du score conditionnels aux décès",
    "de chaque strate : la cellule (global, par tranche d'âge déclarée) ou la cellule croisée avec la modalité de",
    "l'autre variable (par variable de segmentation conditionnellement à l'autre). Diagnostic sans verdict ni seuil :",
    "un test non significatif n'établit pas l'homogénéité, surtout sur une base modeste ; la décision de regrouper",
    "reste à l'utilisateur et se déclare par la segmentation du plan. Aucune correction de multiplicité. Écart",
    "minimal détectable (etapes/homogeneite/resultat/ecart_minimal) : pour chaque segment, le facteur theta appliqué",
    "à sa seule force dans toutes les cellules que le test global détecterait avec la puissance",
    "puissance_ecart_minimal au niveau niveau_ecart_minimal (conventions, etapes/homogeneite/parametres), en deux",
    "branches (theta > 1, theta < 1), conditionnellement aux décès de chaque strate ; approximation asymptotique ; il",
    "mesure ce que les données portent, sans rien dire de l'homogénéité réelle. Le signal ecart_minimal_detectable",
    "(seuil conventionnel seuil_ecart_minimal) est un avertissement, pas un verdict : aucune p-valeur n'est comparée",
    "à un niveau.")
  tous(intro = if (identical(m[["suffisance_intro"]], intro)) TRUE else "suffisance_intro",
       oui = if (identical(m[["suffisance_homogeneite"]], oui)) TRUE else "suffisance_homogeneite",
       non = if (identical(m[["suffisance_homogeneite_absente"]], non)) TRUE else "suffisance_homogeneite_absente",
       portee = if (identical(m[["homogeneite_portee"]], portee)) TRUE else "homogeneite_portee")
})
verifier("I rapport figé : section suffisance, élément de l'écart minimal détectable après celui du brut, avant la largeur (étape au plan, puis absente) ; section etape_homogeneite : table etapes/homogeneite/resultat/ecart_minimal", {
  m <- tablexp:::MENTIONS_RAPPORT
  li <- function(k) paste0("<li>", tablexp:::echapper_html(m[[k]]), "</li>")
  s1 <- section(lignes_rapport(X), "suffisance")
  s0 <- section(lignes_rapport(construire_table(JEU, ROLES, plan_h(hom = NULL))), "suffisance")
  # Plan de la batterie sans étape brut : élément suffisance_brut_absente.
  k1 <- match(c(li("suffisance_brut_absente"), li("suffisance_homogeneite"), li("largeur")), s1)
  k0 <- match(c(li("suffisance_brut_absente"), li("suffisance_homogeneite_absente"), li("largeur")), s0)
  tous(intro = if (any(s1 == paste0("<p>", tablexp:::echapper_html(m[["suffisance_intro"]]), "</p>")) &&
                   any(s0 == paste0("<p>", tablexp:::echapper_html(m[["suffisance_intro"]]), "</p>"))) TRUE else "intro",
       au_plan = if (!anyNA(k1) && identical(diff(k1), c(1L, 1L)) && !li("suffisance_homogeneite_absente") %in% s1) TRUE
                 else "élément (étape au plan)",
       absente = if (!anyNA(k0) && identical(diff(k0), c(1L, 1L)) && !li("suffisance_homogeneite") %in% s0) TRUE
                 else "élément (étape absente)",
       table = if (any(startsWith(section(lignes_rapport(X), "etape_homogeneite"),
                                  "<caption><code>etapes/homogeneite/resultat/ecart_minimal</code>"))) TRUE else "table")
})
# Règle d'une branche de theta_80 (forme F339, #339 ; spécification
# d'actuary-approfondi, Q3 de #339 et troncature de Q1 de #338) : appel direct
# de resoudre_branche_ecart_minimal() avec une lambda factice, limite
# 4 lambda_80, racines theta+ = 4/3 et theta- = 3/4 (valeur calculée à la
# main : 4 (1 - 1/theta) = 1). La non-convergence de uniroot() n'est pas
# atteignable par des données (5 039 résolutions, 67 itérations au plus,
# corps de #339) : seul un maxiter réduit l'exerce. Conditions captées sans
# les arrêter : valeur, erreur, avertissements sortis.
capte_branche <- function(expr) {
  av <- character(0)
  r <- tryCatch(withCallingHandlers(expr, warning = function(w) {
    av <<- c(av, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) e)
  list(valeur = if (inherits(r, "error")) NULL else r, erreur = if (inherits(r, "error")) r else NULL,
       avertissements = av)
}
SEUIL_1 <- L80_1 * (1 + tablexp:::TOLERANCE_ECART_MINIMAL)
LAMBDA_FACTICE <- function(u) 4 * L80_1 * (-expm1(-abs(u)))
branche_factice <- function(lambda, signe, maxiter = 1000L)
  capte_branche(tablexp:::resoudre_branche_ecart_minimal(lambda, signe, L80_1, SEUIL_1, TRUE, maxiter = maxiter))
verifier("I F339 (#339) : resoudre_branche_ecart_minimal(lambda, signe, lam80, seuil, pivots, maxiter = 1000L) ; ecart_minimal_homogeneite() l'appelle deux fois (signes 1 et -1) et n'appelle plus uniroot() elle-même", {
  f <- tablexp:::resoudre_branche_ecart_minimal
  corps <- gsub("\\s+", " ", paste(deparse(body(tablexp:::ecart_minimal_homogeneite), width.cutoff = 500L), collapse = " "))
  appels <- regmatches(corps, gregexpr("resoudre_branche_ecart_minimal\\(lambda, -?1, lam80, seuil, pivots\\)", corps))[[1L]]
  tous(arguments = if (identical(names(formals(f)), c("lambda", "signe", "lam80", "seuil", "pivots", "maxiter"))) TRUE
                   else paste(names(formals(f)), collapse = ", "),
       maxiter = if (identical(formals(f)$maxiter, 1000L)) TRUE else "maxiter par défaut",
       appels = if (identical(appels, c("resoudre_branche_ecart_minimal(lambda, 1, lam80, seuil, pivots)",
                                        "resoudre_branche_ecart_minimal(lambda, -1, lam80, seuil, pivots)"))) TRUE
                else paste(appels, collapse = " / "),
       uniroot = if (!grepl("uniroot", corps, fixed = TRUE)) TRUE else "uniroot dans ecart_minimal_homogeneite()")
})
verifier("I F339 non-convergence forcée (#339 ; valeur calculée à la main) : lambda factice ; maxiter de 1 à 6, signes 1 et -1 : precision_insuffisante, NA, aucune erreur, aucun avertissement sorti ; témoin, maxiter par défaut : produite, 4/3 et 3/4 à 1e-14, sans avertissement", {
  e <- character(0)
  for (m in 1:6) for (s in c(1, -1)) {
    r <- branche_factice(LAMBDA_FACTICE, s, maxiter = m)
    if (!is.null(r$erreur)) { e <- c(e, sprintf("maxiter %d, signe %d : erreur %s", m, s, conditionMessage(r$erreur))); next }
    if (length(r$avertissements))
      e <- c(e, sprintf("maxiter %d, signe %d : avertissement %s", m, s, paste(r$avertissements, collapse = " / ")))
    if (!identical(r$valeur, list(theta = NA_real_, etat = "precision_insuffisante")))
      e <- c(e, sprintf("maxiter %d, signe %d : %s %s", m, s, r$valeur$etat, format(r$valeur$theta)))
  }
  for (s in c(1, -1)) {
    r <- branche_factice(LAMBDA_FACTICE, s)
    att <- if (s > 0) 4 / 3 else 3 / 4
    if (!is.null(r$erreur) || length(r$avertissements) || !identical(r$valeur$etat, "produite") ||
        !isTRUE(proche(r$valeur$theta, att, 1e-14, 0)))
      e <- c(e, sprintf("témoin signe %d : %s", s, if (!is.null(r$erreur)) conditionMessage(r$erreur)
                                                   else paste(r$valeur$etat, format(r$valeur$theta, digits = 17))))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Messages de R attendus dans la langue de la session : reconstruits par
# gettext() dans le domaine de leur source (R 4.3.3), comme R les produit.
# uniroot() relance l'avertissement de zeroin par stop("convergence problem
# in zero finding: ", conditionMessage(w)) (corps de stats::uniroot), dont
# chaque fragment passe par gettext() dans le domaine R-stats ;
# l'avertissement « NA/Inf replaced by maximum positive value » vient du code
# C de zeroin, domaine stats ; « w-int », émis par le test, reste littéral.
MSG_CONVERGENCE_ZEROIN <- gettext("convergence problem in zero finding: ", domain = "R-stats")
verifier("I F339 cas adverses (#339) : une erreur émise dans lambda est relancée telle quelle (« boum ») ; un avertissement émis dans lambda pendant la résolution est relancé en erreur « convergence problem in zero finding: w-int », non converti, sans avertissement sorti ; un NaN de lambda en un point intérieur (0,3 < |u| < 1), signes 1 et -1, est relancé en erreur « convergence problem in zero finding: NA/Inf replaced by maximum positive value », non converti, sans avertissement sorti (messages de R dans la langue de la session)", {
  r_e <- branche_factice(function(u) if (abs(u) > 0 && abs(u) < 1) stop("boum") else LAMBDA_FACTICE(u), 1)
  r_w <- branche_factice(function(u) { if (abs(u) > 0 && abs(u) < 1) warning("w-int"); LAMBDA_FACTICE(u) }, 1)
  tous(erreur = if (!is.null(r_e$erreur) && identical(conditionMessage(r_e$erreur), "boum") && is.null(r_e$valeur)) TRUE
                else "erreur de lambda non relancée",
       avertissement = if (!is.null(r_w$erreur) &&
                           identical(conditionMessage(r_w$erreur), paste0(MSG_CONVERGENCE_ZEROIN, "w-int")) &&
                           !length(r_w$avertissements) && is.null(r_w$valeur)) TRUE
                       else "avertissement de lambda converti ou sorti",
       nan = {
         ln <- function(u) if (abs(u) > 0.3 && abs(u) < 1) NaN else LAMBDA_FACTICE(u)
         ok <- vapply(c(1, -1), function(s) {
           r <- branche_factice(ln, s)
           !is.null(r$erreur) && is.null(r$valeur) && !length(r$avertissements) &&
             identical(conditionMessage(r$erreur),
                       paste0(MSG_CONVERGENCE_ZEROIN,
                              gettext("NA/Inf replaced by maximum positive value", domain = "stats")))
         }, TRUE)
         if (all(ok)) TRUE else "NaN intérieur de lambda converti ou sorti"
       })
})
verifier("I F339 (#339, constat m1 de l'audit) : le message de non-convergence est régénéré par ngettext(..., domain = \"R-stats\") dans resoudre_branche_ecart_minimal() (structurel : aucune locale traduite installée sur l'hôte de mesure)", {
  corps <- gsub("\\s+", " ", paste(deparse(body(tablexp:::resoudre_branche_ecart_minimal), width.cutoff = 500L), collapse = " "))
  attendu <- paste0("ngettext(maxiter, \"_NOT_ converged in %d iteration\", \"_NOT_ converged in %d iterations\", ",
                    "domain = \"R-stats\")")
  if (grepl(attendu, corps, fixed = TRUE)) TRUE else "appel ngettext avec domain = \"R-stats\" absent"
})
verifier("I F339 troncature de f (Q1 de #338) : lambda = +Inf aux points intérieurs |u| > 0,3 (au-delà de la racine), limite finie : produite, 4/3 et 3/4 à 1e-14, aucune erreur ni avertissement", {
  li <- function(u) if (is.finite(u) && abs(u) > 0.3) Inf else LAMBDA_FACTICE(u)
  e <- character(0)
  for (s in c(1, -1)) {
    r <- branche_factice(li, s)
    att <- if (s > 0) 4 / 3 else 3 / 4
    if (!is.null(r$erreur)) { e <- c(e, sprintf("signe %d : erreur %s", s, conditionMessage(r$erreur))); next }
    if (length(r$avertissements)) e <- c(e, sprintf("signe %d : avertissement %s", s, paste(r$avertissements, collapse = " / ")))
    if (!identical(r$valeur$etat, "produite") || !isTRUE(proche(r$valeur$theta, att, 1e-14, 0)))
      e <- c(e, sprintf("signe %d : %s %s", s, r$valeur$etat, format(r$valeur$theta, digits = 17)))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Ordre des contrôles et limite +Inf (Q1 et Q2 de #338, spécification
# d'actuary-approfondi) : pivots en échec avant toute évaluation de lambda ;
# limite NA, NaN ou -Inf : precision_insuffisante ; limite finie au plus le
# seuil : non_atteint ; limite +Inf comprise au-delà du seuil : encadrement et
# résolution. lambda factice de limite remplacée (racines inchangées, 4/3 et
# 3/4, valeur calculée à la main).
verifier("I #338 Q1 limite indéterminée (par la seam) : limite NaN, NA ou -Inf, signes 1 et -1 : precision_insuffisante, NA, sans erreur ni avertissement", {
  e <- character(0)
  for (lim in list(NaN, NA_real_, -Inf)) for (s in c(1, -1)) {
    r <- branche_factice(function(u) if (is.infinite(u)) lim else LAMBDA_FACTICE(u), s)
    if (!is.null(r$erreur) || length(r$avertissements) ||
        !identical(r$valeur, list(theta = NA_real_, etat = "precision_insuffisante")))
      e <- c(e, sprintf("limite %s, signe %d : %s", format(lim), s,
                        if (!is.null(r$erreur)) conditionMessage(r$erreur)
                        else paste(r$valeur$etat, format(r$valeur$theta), paste(r$avertissements, collapse = " / "))))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Seuil inclusif (constat m1 de l'audit de la revue finale de B32) : limite
# exactement égale au seuil : non_atteint ; témoin, limite au-dessus du seuil
# de quelques ulp : encadrement et résolution, racines 4/3 et 3/4 (valeur
# calculée à la main).
verifier("I #338 seuil inclusif (par la seam) : limite égale à SEUIL_1, signes 1 et -1 : non_atteint, NA, sans erreur ni avertissement ; témoin, limite SEUIL_1 (1 + 4e-16) : produite, 4/3 et 3/4 à 1e-14", {
  e <- character(0)
  for (cas in list(list("égale au seuil", SEUIL_1, "non_atteint"), list("témoin", SEUIL_1 * (1 + 4e-16), "produite")))
    for (s in c(1, -1)) {
      lim <- cas[[2L]]
      r <- branche_factice(function(u) if (is.infinite(u)) lim else LAMBDA_FACTICE(u), s)
      att <- if (cas[[3L]] == "non_atteint") NA_real_ else if (s > 0) 4 / 3 else 3 / 4
      ok <- is.null(r$erreur) && !length(r$avertissements) && identical(r$valeur$etat, cas[[3L]]) &&
        (if (is.na(att)) identical(r$valeur$theta, NA_real_) else isTRUE(proche(r$valeur$theta, att, 1e-14, 0)))
      if (!ok)
        e <- c(e, sprintf("%s, signe %d : %s", cas[[1L]], s,
                          if (!is.null(r$erreur)) conditionMessage(r$erreur)
                          else paste(r$valeur$etat, format(r$valeur$theta, digits = 17),
                                     paste(r$avertissements, collapse = " / "))))
    }
  if (SEUIL_1 * (1 + 4e-16) <= SEUIL_1) e <- c(e, "témoin non distinct du seuil en double")
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
verifier("I #338 Q1 limite +Inf (par la seam) : comptée au-delà du seuil, encadrement et résolution ; signes 1 et -1 : produite, 4/3 et 3/4 à 1e-14, sans erreur ni avertissement", {
  e <- character(0)
  for (s in c(1, -1)) {
    r <- branche_factice(function(u) if (is.infinite(u)) Inf else LAMBDA_FACTICE(u), s)
    att <- if (s > 0) 4 / 3 else 3 / 4
    if (!is.null(r$erreur)) { e <- c(e, sprintf("signe %d : erreur %s", s, conditionMessage(r$erreur))); next }
    if (length(r$avertissements) || !identical(r$valeur$etat, "produite") || !isTRUE(proche(r$valeur$theta, att, 1e-14, 0)))
      e <- c(e, sprintf("signe %d : %s %s %s", s, r$valeur$etat, format(r$valeur$theta, digits = 17),
                        paste(r$avertissements, collapse = " / ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Limite +Inf, racine en u = 800 (theta = exp(800) = Inf en double) : la
# résolution aboutit, la postcondition (theta fini) rend precision_insuffisante
# (recommandation de l'audit léger de l'étape (2) de #338).
verifier("I #338 Q1 limite +Inf, racine non représentable (par la seam) : lambda_80 exp(|u| - 800), signes 1 et -1 : precision_insuffisante par la postcondition, NA, sans erreur ni avertissement", {
  e <- character(0)
  for (s in c(1, -1)) {
    r <- branche_factice(function(u) L80_1 * exp(abs(u) - 800), s)
    if (!is.null(r$erreur) || length(r$avertissements) ||
        !identical(r$valeur, list(theta = NA_real_, etat = "precision_insuffisante")))
      e <- c(e, sprintf("signe %d : %s", s, if (!is.null(r$erreur)) conditionMessage(r$erreur)
                                           else paste(r$valeur$etat, format(r$valeur$theta), paste(r$avertissements, collapse = " / "))))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Précondition lambda(+-1024) = lambda(+-infini) non tenue (constat CR2 de la
# revue finale de B32) : limite 4 lambda_80, lambda égale à 0,5 lambda_80 pour
# tout |u| fini ; la boucle de doublement atteint 1024 sous lambda_80. Avant :
# uniroot sur des bornes de même signe, erreur relancée ; après :
# precision_insuffisante, sans signal.
verifier("I #338 CR2 lambda sous lambda_80 en |u| = 1024, limite au-delà du seuil (par la seam) : limite 4 lambda_80, 0,5 lambda_80 pour tout |u| fini, signes 1 et -1 : precision_insuffisante, NA, sans erreur ni avertissement", {
  e <- character(0)
  for (s in c(1, -1)) {
    r <- branche_factice(function(u) if (is.infinite(u)) 4 * L80_1 else 0.5 * L80_1, s)
    if (!is.null(r$erreur) || length(r$avertissements) ||
        !identical(r$valeur, list(theta = NA_real_, etat = "precision_insuffisante")))
      e <- c(e, sprintf("signe %d : %s", s, if (!is.null(r$erreur)) conditionMessage(r$erreur)
                                           else paste(r$valeur$etat, format(r$valeur$theta), paste(r$avertissements, collapse = " / "))))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
verifier("I #338 Q2 pivots en échec (par la seam) : precision_insuffisante sur les deux signes, lambda jamais évaluée (compteur d'appels nul), y compris quand la limite est au plus le seuil (0,5 lambda_80 : non_atteint si le seuil passait d'abord) ou au-delà", {
  e <- character(0)
  for (cas in list(list("limite <= seuil", function(u) 0.5 * L80_1), list("limite 4 lambda_80", LAMBDA_FACTICE)))
    for (s in c(1, -1)) {
      n <- 0L
      lam <- cas[[2L]]
      r <- capte_branche(tablexp:::resoudre_branche_ecart_minimal(function(u) { n <<- n + 1L; lam(u) }, s, L80_1, SEUIL_1, FALSE))
      if (!is.null(r$erreur) || length(r$avertissements) || n != 0L ||
          !identical(r$valeur, list(theta = NA_real_, etat = "precision_insuffisante")))
        e <- c(e, sprintf("%s, signe %d : %s, %d appel(s) de lambda", cas[[1L]], s,
                          if (!is.null(r$erreur)) conditionMessage(r$erreur) else r$valeur$etat, n))
    }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Constat m2 de l'audit de 1ef94e2 (commentaire de
# resoudre_branche_ecart_minimal()) : un NaN de lambda (chemin qu'aucune
# donnée n'atteint) dans la boucle de doublement en h < 1024 l'arrête sur
# l'erreur de if ; en u = 0 ou |u| = 1024, seules bornes que la boucle n'a pas
# évaluées sans NaN, uniroot lève « f.lower (f.upper) = ... is NA ». lambda
# lente (racine en u voisin de 5,75) pour que la boucle double jusqu'à h = 8.
# Messages attendus reconstruits par gettext() dans la langue de la session :
# erreur de if du code C de l'évaluateur, domaine R ; « f.lower (f.upper) =
# ... is NA », stop() de stats::uniroot, domaine R-stats.
verifier("I #338 m2 NaN de lambda hors de zeroin (par la seam ; messages de R 4.3.3, dans la langue de la session) : en |u| = 1, 2, 4, 8 (boucle de doublement) : erreur « missing value where TRUE/FALSE needed » ; en u = 0 : « f.lower = f(lower) is NA » (signe 1) ; en |u| = 1024 (lambda finie sous lambda_80, limite au-delà) : « f.upper = f(upper) is NA » (signe 1)", {
  lente <- function(u) 4 * L80_1 * (-expm1(-abs(u) / 20))
  msg <- function(lam) { r <- branche_factice(lam, 1); if (is.null(r$erreur)) "aucune erreur" else conditionMessage(r$erreur) }
  boucle <- vapply(c(1, 2, 4, 8), function(h) msg(function(u) if (is.finite(u) && abs(u) == h) NaN else lente(u)), "")
  zero <- msg(function(u) if (u == 0) NaN else LAMBDA_FACTICE(u))
  cap <- msg(function(u) if (is.finite(u) && abs(u) == 1024) NaN else if (is.infinite(u)) 2 * L80_1 else 0.5 * L80_1)
  tous(boucle = if (all(boucle == gettext("missing value where TRUE/FALSE needed", domain = "R"))) TRUE
                else paste(boucle, collapse = " / "),
       zero = if (identical(zero, gettext("f.lower = f(lower) is NA", domain = "R-stats"))) TRUE else zero,
       cap = if (identical(cap, gettext("f.upper = f(upper) is NA", domain = "R-stats"))) TRUE else cap)
})
# Scénario de Q1 de #338 par les données : âge 60 seul ; A (5000, 600), X
# (E_X, 1), E_X dans {5e-305, 5e-307} ; lambda_X(+infini) déborde en +Inf
# (part de X voisine de 1e-308 ou dénormalisée). Avant : theta+ de X
# precision_insuffisante ; après : produite. Recalcul indépendant par
# l'inverse exacte de la forme à deux segments en strate unique (D = 601) :
# lambda = D p_X p_A g^2, g = (theta - 1) / (p_A + p_X theta), donc theta =
# (1 + g p_A) / (1 - g p_X), g = sqrt(lambda_80 / (D p_X p_A)). Tolérance 1e-10
# en relatif (celle de T7) : écart mesuré 5,2e-14 et 1,6e-14, sous la
# précision qu'assure la postcondition (1e-8 sur lambda, environ 5e-9 sur
# theta, lambda en theta^2). Attendus de A changés par #334 (spécification
# d'actuary-approfondi, Q3, § 3, point 2) : v_A formé par sommes hors
# diagonale vaut w_AX > 0, là où diag(A) - P'DP valait 0 (mesure adverse, C7 :
# A variance_nulle avant #334, sur une copie de 517469f) ; A produite, z_A =
# -z_X ; theta+ de A non_atteint ; theta- de A produite, à 1e-10 de la forme
# close (1 - pi_X r) / (1 + pi_A r), r = sqrt(lambda_80 / (601 pi_A pi_X)),
# soit 8.7505213868467016e-154 et 8.7505213868466907e-155 (écarts mesurés
# 5,2e-14 et 1,6e-14). Une ligne d'avertissement ecart_minimal_detectable, de
# nombre 2 (theta- de X et theta+ de A non_atteint ; 1 avant #334).
verifier("I #338 scénario de Q1 (recalcul indépendant) : âge 60, A (5000, 600), X (E_X, 1), ordre (A, X), E_X dans {5e-305, 5e-307} : theta+ de X produite, à 1e-10 de l'inverse exacte (1.142789047e+153 et 1.142789047e+154), postcondition <= 1e-8 ; theta- de X non_atteint ; A produite (z_A = -z_X), theta+ de A non_atteint, theta- de A produite à 1e-10 de la forme close (#334) ; une ligne ecart_minimal_detectable, nombre 2 ; aucun avertissement de R", {
  e <- character(0)
  for (k in 1:2) {
    eX <- c(5e-305, 5e-307)[k]
    d <- expo_seg(c("A", "X"), c(60, 60), c(5000, eX), c(600, 1))
    av <- character(0)
    r <- tryCatch(withCallingHandlers(homog(d, "segment", c(60, 60), 60), warning = function(w) {
      av <<- c(av, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(x) x)
    if (inherits(r, "error")) { e <- c(e, sprintf("E_X %g : erreur %s", eX, conditionMessage(r))); next }
    em <- r$resultat$ecart_minimal
    a <- match("A", em$segment); x <- match("X", em$segment)
    Ec <- 5000 + eX; px <- eX / Ec; pa <- 5000 / Ec
    g <- sqrt(L80_1 / (601 * px * pa))
    att <- (1 + g * pa) / (1 - g * px)
    th <- em$theta_80_superieur[x]
    et <- c(em$etat_superieur[x], em$etat_inferieur[x], em$etat_superieur[a], em$etat_inferieur[a])
    if (!identical(et, c("produite", "non_atteint", "non_atteint", "produite")))
      e <- c(e, sprintf("E_X %g : états %s", eX, paste(et, collapse = "/")))
    else if (!isTRUE(proche(th, att, 1e-10, 0)) || !isTRUE(proche(th, c(1.142789047e153, 1.142789047e154)[k], 1e-9, 0)) ||
             !(abs(lambda_deux(601, px, pa, th) / L80_1 - 1) <= 1e-8))
      e <- c(e, sprintf("E_X %g : theta+ de X %s, attendu %s", eX, format(th, digits = 17), format(att, digits = 17)))
    rr <- sqrt(L80_1 / (601 * pa * px))
    th_a <- em$theta_80_inferieur[a]
    if (!isTRUE(proche(th_a, (1 - px * rr) / (1 + pa * rr), 1e-10, 0)) ||
        !isTRUE(proche(th_a, c(8.7505213868467016e-154, 8.7505213868466907e-155)[k], 1e-10, 0)))
      e <- c(e, sprintf("E_X %g : theta- de A %s, attendu %s", eX, format(th_a, digits = 17),
                        format((1 - px * rr) / (1 + pa * rr), digits = 17)))
    sg <- r$resultat$segments[r$resultat$segments$portee == "global", ]
    za <- sg$z[sg$segment == "A"]; zx <- sg$z[sg$segment == "X"]
    if (!identical(sg$etat, c("produite", "produite")) || !is.finite(za) || !identical(za, -zx))
      e <- c(e, sprintf("E_X %g : segments %s, z %s", eX, paste(sg$etat, collapse = "/"),
                        paste(format(sg$z, digits = 17), collapse = "/")))
    sig <- r$avertissements[r$avertissements$code == "ecart_minimal_detectable", , drop = FALSE]
    if (nrow(sig) != 1L || !identical(sig$nombre, 2L))
      e <- c(e, sprintf("E_X %g : %d ligne(s) ecart_minimal_detectable, nombre %s", eX, nrow(sig),
                        paste(sig$nombre, collapse = ",")))
    if (length(av)) e <- c(e, sprintf("E_X %g : avertissements de R %s", eX, paste(av, collapse = " / ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Ordre (X, A) du même scénario : avant #334, le test global s'arrêtait avant
# theta_80 sur « Lapack routine dgesv: system is exactly singular » (V réduite
# par soustraction, dépendance à l'ordre), avant comme après #338 ; échec
# attendu {#334} jusqu'à #334, devenu succès inattendu (mesuré) puis test
# ordinaire. Critère (avis d'actuary, Q3 de la reprise de #338 ; commentaire
# M566 de #334) : invariance de toute la table ecart_minimal à l'ordre des
# segments : pour chaque segment (apparié par son nom), mêmes états des deux
# branches, theta égaux à 1e-10 en relatif quand l'état est produite et NA
# tous deux sinon, même nombre de signaux ecart_minimal_detectable ; plus, sur
# X, theta+ produite à 1e-10 de l'inverse exacte et theta- non_atteint ; plus
# (spécification d'actuary-approfondi de #334, § 3, point 1) T égal dans les
# deux ordres à 1e-12 en relatif (mesuré égal au bit), aucune erreur et aucun
# NaN (T, p asymptotique, z, theta).
verifier("I #338 scénario de Q1, ordre (X, A) : mêmes états et theta des deux segments qu'en ordre (A, X), T égal à 1e-12, sans arrêt ni NaN (#334)", {
  e <- character(0)
  for (eX in c(5e-305, 5e-307)) {
    d <- expo_seg(c("A", "X"), c(60, 60), c(5000, eX), c(600, 1))
    r_ax <- homog(d, "segment", c(60, 60), 60)
    r <- tryCatch(homog(d[2:1, ], "segment", c(60, 60), 60), error = function(x) conditionMessage(x))
    if (is.character(r)) { e <- c(e, sprintf("E_X %g : arrêt %s", eX, r)); next }
    em1 <- r_ax$resultat$ecart_minimal; em2 <- r$resultat$ecart_minimal
    k <- match(em1$segment, em2$segment)
    if (nrow(em1) != nrow(em2) || anyNA(k)) { e <- c(e, sprintf("E_X %g : segments différents d'un ordre à l'autre", eX)); next }
    for (b in c("superieur", "inferieur")) {
      et1 <- em1[[paste0("etat_", b)]]; et2 <- em2[[paste0("etat_", b)]][k]
      th1 <- em1[[paste0("theta_80_", b)]]; th2 <- em2[[paste0("theta_80_", b)]][k]
      if (!identical(et1, et2))
        e <- c(e, sprintf("E_X %g : états %s %s / %s", eX, b, paste(et1, collapse = ","), paste(et2, collapse = ",")))
      p <- et1 == "produite"
      if ((any(p) && !isTRUE(proche(th2[p], th1[p], 1e-10, 0))) || !all(is.na(th1[!p]) & is.na(th2[!p])))
        e <- c(e, sprintf("E_X %g : theta %s %s / %s", eX, b, paste(format(th1, digits = 17, trim = TRUE), collapse = ","),
                          paste(format(th2, digits = 17, trim = TRUE), collapse = ",")))
    }
    n1 <- sum(r_ax$avertissements$code == "ecart_minimal_detectable")
    n2 <- sum(r$avertissements$code == "ecart_minimal_detectable")
    if (n1 != n2) e <- c(e, sprintf("E_X %g : %d / %d signal(aux) ecart_minimal_detectable", eX, n1, n2))
    x <- match("X", em2$segment)
    Ec <- 5000 + eX; px <- eX / Ec; pa <- 5000 / Ec
    g <- sqrt(L80_1 / (601 * px * pa))
    if (!identical(c(em2$etat_superieur[x], em2$etat_inferieur[x]), c("produite", "non_atteint")) ||
        !isTRUE(proche(em2$theta_80_superieur[x], (1 + g * pa) / (1 - g * px), 1e-10, 0)))
      e <- c(e, sprintf("E_X %g : X %s %s", eX, em2$etat_superieur[x], format(em2$theta_80_superieur[x], digits = 17)))
    t1 <- r_ax$resultat$tests; t2 <- r$resultat$tests
    if (!identical(t1$etat, t2$etat) || !isTRUE(proche(t2$statistique, t1$statistique, 1e-12, 0)))
      e <- c(e, sprintf("E_X %g : T %s / %s", eX, format(t1$statistique[1L], digits = 17), format(t2$statistique[1L], digits = 17)))
    nan <- c(t1$statistique, t2$statistique, t1$p_asymptotique, t2$p_asymptotique, r_ax$resultat$segments$z,
             r$resultat$segments$z, em1$theta_80_superieur, em1$theta_80_inferieur, em2$theta_80_superieur,
             em2$theta_80_inferieur)
    if (any(is.nan(nan))) e <- c(e, sprintf("E_X %g : NaN", eX))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})

# --- #334 : V réduite sans soustraction, élimination partagée (F334) ----------
#
# Spécification d'actuary-approfondi de #334 (Q3, § 1 à 3) ; état
# precision_insuffisante du test (décision du mainteneur du 21/11/2026, M570,
# P1 (a)). Natures des attendus : recalcul indépendant (solve() sur des
# laplaciens construits bien conditionnés ; pseudo-inverse spectrale de V par
# eigen()) ; valeur calculée à la main (forme close à deux segments en
# strate unique) ; identité exacte (invariance à l'ordre des lignes,
# antisymétrie de z à deux segments).

verifier("I #334 eliminer_sans_soustraction() (recalcul indépendant) : laplaciens réduits construits (m = 1 à 6, poids dans [0,1 ; 10], r > 0) : pivots finis et > 0, evaluer(B) à 1e-12 de colSums(B * solve(V_g, B)) sur 5 colonnes signées ; m = 0 : 0 par colonne", {
  e <- character(0)
  tablexp:::sous_graine(20261122L, for (m in 1:6) {
    W <- matrix(stats::runif(m * m, 0.1, 10), m, m); W <- (W + t(W)) / 2; diag(W) <- 0
    r <- stats::runif(m, 0.1, 10)
    Vg <- diag(rowSums(W) + r, m) - W
    B <- matrix(stats::rnorm(5 * m, 0, 20), m, 5)
    el <- tablexp:::eliminer_sans_soustraction(W, r)
    ok <- proche(el$evaluer(B), colSums(B * solve(Vg, B)), 1e-12, 0)
    if (!isTRUE(el$pivots) || !isTRUE(ok)) e <- c(e, sprintf("m = %d : pivots %s, %s", m, el$pivots, ok))
  })
  el0 <- tablexp:::eliminer_sans_soustraction(matrix(0, 0, 0), numeric(0))
  if (!identical(el0$evaluer(matrix(0, 0, 3)), c(0, 0, 0)) || !isTRUE(el0$pivots)) e <- c(e, "m = 0")
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Forme F334 (C3 du plan de B33) : une seule élimination, appelée par
# score_homogeneite() et ecart_minimal_homogeneite() ; plus de solve() ni de
# diag(A) - P'DP dans l'étape.
verifier("I #334 F334 : eliminer_sans_soustraction() appelée une fois par score_homogeneite() et une fois par ecart_minimal_homogeneite() ; aucun solve() dans les deux fonctions", {
  corps <- function(f) gsub("\\s+", " ", paste(deparse(body(get(f, asNamespace("tablexp"))), width.cutoff = 500L),
                                                collapse = " "))
  n <- function(f, motif) lengths(regmatches(corps(f), gregexpr(motif, corps(f), fixed = TRUE)))
  tous(score = if (n("score_homogeneite", "eliminer_sans_soustraction(") == 1L) TRUE else "score_homogeneite()",
       ecart = if (n("ecart_minimal_homogeneite", "eliminer_sans_soustraction(") == 1L) TRUE
               else "ecart_minimal_homogeneite()",
       solve = if (n("score_homogeneite", "solve(") == 0L && n("ecart_minimal_homogeneite", "solve(") == 0L) TRUE
               else "solve() présent")
})
# Groupe de référence (Q3, § 1, point 3) : attendu A le plus grand, le plus
# petit indice à égalité ; la p exacte porte sur le groupe retenu (garde).
verifier("I #334 groupe de référence : attendu le plus grand (garde = groupes retenus) ; à égalité, le plus petit indice ; deux composantes, une référence chacune", {
  sc1 <- tablexp:::score_homogeneite(c(1, 9), c(10, 90), c(1L, 1L), 1:2, 1L, 2L)
  sc2 <- tablexp:::score_homogeneite(c(5, 5), c(50, 50), c(1L, 1L), 1:2, 1L, 2L)
  sc3 <- tablexp:::score_homogeneite(c(2, 1, 1, 3, 1, 5), c(20, 30, 10, 10, 10, 80), c(1L, 1L, 1L, 2L, 2L, 2L),
                                     c(1L, 2L, 3L, 4L, 5L, 6L), 2L, 6L)
  tous(max = if (identical(sc1$garde, 1L)) TRUE else paste("garde", paste(sc1$garde, collapse = ",")),
       egalite = if (identical(sc2$garde, 2L)) TRUE else paste("garde", paste(sc2$garde, collapse = ",")),
       composantes = if (identical(sc3$garde, c(1L, 3L, 4L, 5L))) TRUE else paste("garde", paste(sc3$garde, collapse = ",")),
       somme_u = if (isTRUE(proche(c(sum(sc3$U[1:3]), sum(sc3$U[4:6])), c(0, 0), 0, 1e-12))) TRUE else "U de somme non nulle")
})
# Cas du corps de #334 (spécification, § 2) : avant (copie de 517469f),
# erreur dgesv ; après, T produite, valeur calculée à la main à une strate
# informative (âge 62, 5 décès sur Y) : T = U_X^2 / v_X, U_X = -D pi_X, v_X =
# D pi_X pi_Y, soit D pi_X / pi_Y.
# Second cas (Q2 du visa d'actuary de l'étape (3)) : les 5 décès sur X ; T =
# U_X^2 / v_X, U_X = D pi_Y, soit D pi_Y / pi_X = 5.5426917510853837e+18
# (mesuré, égal à la forme close au bit) ; avant : erreur dgesv aussi.
verifier("I #334 cas du corps de l'issue : S = 2, K = 3, E = (4.82e-07, 1.21e-08, 6.91e-15 ; 0, 0, 7.66e3), 5 décès à 62 sur le second segment : T = 5 pi_X / pi_Y = 4.5104438642297647e-18 à 1e-12, sans erreur ; X et Y produite, z antisymétriques ; 5 décès sur X : T = 5 pi_Y / pi_X = 5.5426917510853837e+18 à 1e-12", {
  d2 <- expo_seg(rep(c("X", "Y"), each = 3L), rep(60:62, 2L), c(4.82e-07, 1.21e-08, 6.91e-15, 0, 0, 7.66e3), c(0, 0, 5, 0, 0, 0))
  r2 <- tryCatch(homog(d2, "segment", c(60, 62), 60), error = function(x) conditionMessage(x))
  if (is.character(r2)) return(paste("erreur (décès sur X) :", r2))
  d <- expo_seg(rep(c("X", "Y"), each = 3L), rep(60:62, 2L), c(4.82e-07, 1.21e-08, 6.91e-15, 0, 0, 7.66e3), c(0, 0, 0, 0, 0, 5))
  r <- tryCatch(homog(d, "segment", c(60, 62), 60), error = function(x) conditionMessage(x))
  if (is.character(r)) return(paste("erreur :", r))
  t <- r$resultat$tests; sg <- r$resultat$segments[r$resultat$segments$portee == "global", ]
  px <- 6.91e-15 / (6.91e-15 + 7.66e3); py <- 7.66e3 / (6.91e-15 + 7.66e3)
  tous(etat = if (identical(t$etat[1L], "produite")) TRUE else t$etat[1L],
       t = proche(t$statistique[1L], 5 * px / py, 1e-12, 0),
       t_spec = proche(t$statistique[1L], 4.5104438642297647e-18, 1e-12, 0),
       t_x = proche(r2$resultat$tests$statistique[1L], 5 * py / px, 1e-12, 0),
       t_x_spec = proche(r2$resultat$tests$statistique[1L], 5.5426917510853837e+18, 1e-12, 0),
       segments = if (identical(sg$etat, c("produite", "produite")) && identical(sg$z[1L], -sg$z[2L])) TRUE
                  else paste(sg$etat, collapse = "/"))
})
# Corps de #333 (F (1e5, 100), X (1e-11, 0)) : avant, T dépendait de l'ordre
# (9.9999999999999984e-15 et 1.4210854715202004e-14) et z_F valait
# 1.192e-07 au lieu de 1e-07 ; après, même T dans les deux ordres, z = +-1e-07
# antisymétriques (valeur calculée à la main : z_X = -sqrt(D pi_X / pi_F)).
verifier("I #334 corps de #333 : F (1e5, D_F), X (1e-11, D_X), (D_F, D_X) dans {(100, 0), (99, 1)}, deux ordres : T identique d'un ordre à l'autre, z_F = -z_X au bit, z à 1e-12 de la forme close", {
  e <- character(0)
  for (D in list(c(100, 0), c(99, 1))) {
    pf <- 1e5 / (1e5 + 1e-11); px <- 1e-11 / (1e5 + 1e-11)
    zx <- (D[2L] - 100 * px) / sqrt(100 * pf * px)
    res <- lapply(list(1:2, 2:1), function(o) homog(expo_seg(c("F", "X"), c(60, 60), c(1e5, 1e-11), D)[o, ], "segment", c(60, 60), 60))
    t <- vapply(res, function(r) r$resultat$tests$statistique[1L], 0)
    for (r in res) {
      sg <- r$resultat$segments[r$resultat$segments$portee == "global", ]
      z_f <- sg$z[sg$segment == "F"]; z_x <- sg$z[sg$segment == "X"]
      if (!identical(z_f, -z_x) || !isTRUE(proche(z_x, zx, 1e-12, 0)))
        e <- c(e, sprintf("D %s : z %s / %s, attendu z_X %s", paste(D, collapse = "/"), format(z_f, digits = 17),
                          format(z_x, digits = 17), format(zx, digits = 17)))
    }
    if (!identical(t[1L], t[2L])) e <- c(e, sprintf("D %s : T %s", paste(D, collapse = "/"), paste(format(t, digits = 17), collapse = " / ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Débordement (Q3, § 1, point 9) : pivots corrects, statistique au-delà du
# plus grand double : T = +Inf, état produite, p asymptotique 0, p de
# Monte-Carlo 1 / (B + 1) (B = 99). Avant : erreur dgesv (E_X <= 5e-308).
verifier("I #334 débordement : A (5000, 600), X (E_X, 1), E_X dans {1e-308, 5e-310}, deux ordres : T = +Inf, état produite, p asymptotique 0, p de Monte-Carlo 0,01", {
  e <- character(0)
  for (eX in c(1e-308, 5e-310)) for (o in list(1:2, 2:1)) {
    r <- tryCatch(homog(expo_seg(c("A", "X"), c(60, 60), c(5000, eX), c(600, 1))[o, ], "segment", c(60, 60), 60),
                  error = function(x) conditionMessage(x))
    if (is.character(r)) { e <- c(e, sprintf("E_X %g : erreur %s", eX, r)); next }
    t <- r$resultat$tests[1L, ]
    if (!identical(t$statistique, Inf) || !identical(t$etat, "produite") || !identical(t$p_asymptotique, 0) ||
        !isTRUE(proche(t$p_monte_carlo, 1 / 100, 1e-14, 0)))
      e <- c(e, sprintf("E_X %g ordre %s : T %s, %s, p %s, p MC %s", eX, paste(o, collapse = ""), t$statistique, t$etat,
                        t$p_asymptotique, t$p_monte_carlo))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# M570, P1 (a) : un pivot nul ou non fini (produits D_.c pi_jc pi_kc sous le
# plus petit double) rend le test precision_insuffisante, décidé avant tout
# tirage : T, p asymptotique, p exacte et p de Monte-Carlo NA, état de la p
# exacte precision_insuffisante, aucun tirage (jeu cellules à 0) ; z produit
# là où v > 0, sinon variance_nulle ; ecart_minimal hérite de l'état du test,
# sans signal. Avant (copie de 517469f) : erreur dgesv dans tous ces cas.
verifier("I #334 precision_insuffisante (M570, P1 (a)) : A (5000, 600), X (E_X, D_X), E_X dans {1.5e-323, 1e-323, 5e-324}, D_X dans {0, 1} ; et S = 3, J de part dénormalisée minimale : tests precision_insuffisante, T et p NA, aucun tirage ; z produit si v > 0 ; ecart_minimal precision_insuffisante ; aucun avertissement", {
  e <- character(0)
  cas <- c(lapply(c(1.5e-323, 1e-323, 5e-324), function(eX) lapply(0:1, function(dX)
             list(nom = sprintf("E_X %g D_X %d", eX, dX), d = expo_seg(c("A", "X"), c(60, 60), c(5000, eX), c(600, dX)),
                  z = c(A = NA, X = NA)))),
           list(list(list(nom = "S = 3", d = expo_seg(c("A", "B", "J"), rep(60, 3), c(1, 1, 4.94e-324), c(1, 0, 0)),
                           z = c(A = 1, B = -1, J = NA)))))
  for (c_ in unlist(cas, recursive = FALSE)) {
    av <- character(0)
    r <- tryCatch(withCallingHandlers(homog(c_$d, "segment", c(60, 60), 60), warning = function(w) {
      av <<- c(av, conditionMessage(w)); invokeRestart("muffleWarning")
    }), error = function(x) conditionMessage(x))
    if (is.character(r)) { e <- c(e, sprintf("%s : erreur %s", c_$nom, r)); next }
    t <- r$resultat$tests; em <- r$resultat$ecart_minimal
    sg <- r$resultat$segments[r$resultat$segments$portee == "global", ]
    z <- sg$z[match(names(c_$z), sg$segment)]
    pb <- c(if (!all(t$etat == "precision_insuffisante") || !all(t$etat_p_exacte == "precision_insuffisante")) "états des tests",
            if (!all(is.na(c(t$statistique, t$p_asymptotique, t$p_exacte, t$p_monte_carlo, t$erreur_monte_carlo)))) "T ou p non NA",
            if (!identical(r$resultat$monte_carlo$jeux$tirages_effectues, 0L)) "tirages",
            if (!isTRUE(proche(z, unname(c_$z), 1e-12, 0)) ||
                !identical(sg$etat[match(names(c_$z), sg$segment)], unname(ifelse(is.na(c_$z), "variance_nulle", "produite")))) "segments",
            if (!all(c(em$etat_superieur, em$etat_inferieur) == "precision_insuffisante") ||
                !all(is.na(c(em$theta_80_superieur, em$theta_80_inferieur)))) "ecart_minimal",
            if (nrow(r$avertissements)) "avertissement du moteur", if (length(av)) "avertissement de R")
    if (length(pb)) e <- c(e, sprintf("%s : %s", c_$nom, paste(pb, collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Quatre segments (Q3, § 2 et § 3, point 5) : six âges (60 à 65), deux
# tranches (60, 63), Z non exposé à 60-61 ; six ordres des lignes. T
# recalculé par la pseudo-inverse spectrale de V (eigen(), valeurs propres
# au-delà de 1e-10 fois la plus grande ; mesuré 6,7e-16 sur le prototype
# d'actuary) ; invariance de T, z et theta à 1e-12 et des états (p de
# Monte-Carlo exclue : flux des lignes, #335, (I2)).
EXPO_334 <- tablexp:::sous_graine(20261123L, {
  g <- expand.grid(segment = c("W", "X", "Y", "Z"), age = 60:65, stringsAsFactors = FALSE)
  g$exposition_centrale <- stats::runif(nrow(g), 200, 2000)
  g$exposition_centrale[g$segment == "Z" & g$age <= 61] <- 0
  g$evenements <- stats::rpois(nrow(g), g$exposition_centrale * 0.01 * c(W = 1, X = 1.2, Y = 0.9, Z = 1.1)[g$segment])
  g
})
verifier("I #334 quatre segments, six ordres des lignes : T à 1e-12 de la pseudo-inverse spectrale de V ; T, z, theta_80 à 1e-12 et états identiques d'un ordre à l'autre", {
  e <- character(0)
  g <- EXPO_334
  ref <- homog(g, "segment", c(60, 65), c(60, 63))
  # T indépendant : strates informatives, V complète, pseudo-inverse spectrale.
  U <- setNames(numeric(4), c("W", "X", "Y", "Z")); V <- matrix(0, 4, 4)
  for (a in 60:65) {
    l <- g[g$age == a, ]; Dc <- sum(l$evenements)
    if (Dc == 0 || sum(l$exposition_centrale > 0) < 2L) next
    pi <- l$exposition_centrale / sum(l$exposition_centrale)
    U <- U + l$evenements - Dc * pi; V <- V + Dc * (diag(pi) - tcrossprod(pi))
  }
  ev <- eigen(V, symmetric = TRUE); k <- ev$values > 1e-10 * max(ev$values)
  t_ind <- sum((crossprod(ev$vectors[, k], U))^2 / ev$values[k])
  ok <- proche(ref$resultat$tests$statistique[1L], t_ind, 1e-12, 0)
  if (!isTRUE(ok)) e <- c(e, paste("T indépendant :", ok))
  cle <- function(r) {
    s <- r$resultat$segments; o <- order(s$portee, s$tranche_debut, s$segment, method = "radix")
    m <- r$resultat$ecart_minimal; om <- order(m$segment, method = "radix")
    list(t = r$resultat$tests$statistique, et = r$resultat$tests$etat, z = s$z[o], es = s$etat[o],
         th = c(m$theta_80_superieur[om], m$theta_80_inferieur[om]), em = c(m$etat_superieur[om], m$etat_inferieur[om]))
  }
  k0 <- cle(ref)
  tablexp:::sous_graine(20261124L, for (i in 1:6) {
    k1 <- cle(homog(g[sample.int(nrow(g)), ], "segment", c(60, 65), c(60, 63)))
    pb <- c(if (!isTRUE(proche(k1$t, k0$t, 1e-12, 0))) "T", if (!isTRUE(proche(k1$z, k0$z, 1e-12, 0))) "z",
            if (!isTRUE(proche(k1$th, k0$th, 1e-12, 0))) "theta",
            if (!identical(c(k1$et, k1$es, k1$em), c(k0$et, k0$es, k0$em))) "états")
    if (length(pb)) e <- c(e, sprintf("ordre %d : %s", i, paste(pb, collapse = ", ")))
  })
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# États mêlés (m1 de l'audit de l'étape (3)) : A et B exposés de 60 à 63, J de
# part dénormalisée minimale (4.94e-324) à 62-63 ; tranches (60, 62). Pivot
# nul seulement dans la tranche 62-63 et dans le test global : global et
# tranche 2 precision_insuffisante, tranche 1 produite, somme_tranches
# precision_insuffisante (une tranche de ddl > 0 l'est ; tue le mutant all ->
# any de pivots_somme, vérifié) ; le jeu cellules est tiré (B = 99) pour la
# tranche produite, dont T est égal au bit à celui de la tranche seule.
verifier("I #334 états mêlés (m1) : global et tranche 62-63 precision_insuffisante, tranche 60-61 produite (T égal au bit à l'appel sur la tranche seule), somme_tranches precision_insuffisante et T NA, 99 tirages du jeu cellules", {
  d <- rbind(expo_seg(rep(c("A", "B"), each = 4L), rep(60:63, 2L), c(1000, 1100, 1200, 1300, 900, 950, 1000, 1050),
                      c(5, 6, 7, 8, 4, 6, 5, 9)),
             expo_seg(c("J", "J"), 62:63, c(4.94e-324, 4.94e-324), c(0, 0)))
  r <- homog(d, "segment", c(60, 63), c(60, 62))
  t <- r$resultat$tests
  seule <- homog(d[d$age <= 61, ], "segment", c(60, 61), 60)$resultat$tests
  tous(etats = if (identical(t$etat, c("precision_insuffisante", "produite", "precision_insuffisante",
                                       "precision_insuffisante"))) TRUE else paste(t$etat, collapse = "/"),
       somme = if (is.na(t$statistique[4L]) && is.na(t$p_monte_carlo[4L])) TRUE else "somme_tranches produite",
       tranche = if (identical(t$statistique[2L], seule$statistique[1L])) TRUE
                 else paste(format(t$statistique[2L], digits = 17), format(seule$statistique[1L], digits = 17)),
       tirages = if (identical(r$resultat$monte_carlo$jeux$tirages_effectues, 99L)) TRUE else "tirages")
})
# p exacte sur le groupe retenu (#334 ; recommandation (i) d'actuary au visa
# de l'étape (3)) : groupe 2 dominant (référence), plusieurs strates ;
# recalcul indépendant par convolution directe des dbinom sur le groupe
# miroir (groupe 2) : p = P(|O_2 - A_2| >= |o_2 - A_2|), égale en
# arithmétique exacte (O_1 - A_1 = -(O_2 - A_2)), à 1e-12 en relatif.
verifier("I #334 p exacte, groupe 2 dominant, quatre strates : groupe retenu 1 ; p à 1e-12 d'une convolution directe des dbinom sur le groupe miroir", {
  E <- c(120, 900, 80, 700, 150, 1100, 60, 500); D <- c(2, 9, 0, 8, 3, 10, 1, 4)
  sc <- tablexp:::score_homogeneite(D, E, rep(1:4, each = 2L), rep(1:2, 4L), 4L, 2L)
  pe <- tablexp:::p_exacte_homogeneite(sc)
  n <- D[c(1, 3, 5, 7)] + D[c(2, 4, 6, 8)]; p2 <- E[c(2, 4, 6, 8)] / (E[c(1, 3, 5, 7)] + E[c(2, 4, 6, 8)])
  loi <- 1
  for (j in seq_along(n)) {
    b <- stats::dbinom(0:n[j], n[j], p2[j]); nl <- numeric(length(loi) + n[j])
    for (k in seq_along(b)) nl[k:(k + length(loi) - 1L)] <- nl[k:(k + length(loi) - 1L)] + b[k] * loi
    loi <- nl
  }
  a2 <- sum(n * p2); o2 <- sum(D[c(2, 4, 6, 8)]); k <- seq_along(loi) - 1
  p_ind <- min(1, sum(loi[abs(k - a2) >= abs(o2 - a2) - 1e-10 * a2]))
  tous(garde = if (identical(sc$garde, 1L)) TRUE else paste("garde", sc$garde),
       etat = if (identical(pe$etat, "produite")) TRUE else pe$etat,
       p = proche(pe$p, p_ind, 1e-12, 0))
})
# Convention de la frontière (recommandation (ii) d'actuary) : l'égalité
# |k - A_1| = |o_1 - A_1| est jugée à 1e-10 A_1 près, A_1 attendu du groupe
# retenu (le plus petit). Une strate, D = 10, E = (0,25 + 2e-11 ; 0,75 -
# 2e-11), o_1 = 1 : A_1 = 2,5000000002, |4 - A_1| = 1,4999999998 <
# 1,5000000002 - 2,5e-10 : k = 4 exclu, p = 0.32215213773465651 (mesuré).
# Avant #334 (copie de 517469f), groupe retenu 2, tolérance 7,5e-10 : k = 4
# inclus, p = 0.46815013885664886 (mesuré).
verifier("I #334 p exacte à la frontière (convention 1e-10 A_garde) : une strate, D = 10, E = (0,25 + 2e-11 ; 0,75 - 2e-11), o_1 = 1 : groupe retenu 1, k = 4 exclu, p = 0.32215213773465651 à 1e-12", {
  sc <- tablexp:::score_homogeneite(c(1, 9), c(0.25 + 2e-11, 0.75 - 2e-11), c(1L, 1L), 1:2, 1L, 2L)
  pe <- tablexp:::p_exacte_homogeneite(sc)
  p_main <- sum(stats::dbinom(c(0:1, 5:10), 10, (0.25 + 2e-11) / 1))
  tous(garde = if (identical(sc$garde, 1L)) TRUE else paste("garde", sc$garde),
       p = proche(pe$p, 0.32215213773465651, 1e-12, 0),
       main = proche(pe$p, p_main, 1e-12, 0))
})

# --- #333 : variance_nulle après #334 ; part arrondie à 0 ---------------------
#
# Spécification d'actuary-approfondi de #333 (Q4) ; texte de statut de
# theta_80 (M570). Natures des attendus : valeur calculée à la main (formes
# closes à deux segments en strate unique, sur les parts telles que
# représentées : theta = (1 +- q r) / (1 -+ p r), r = sqrt(lambda_80 / (D p
# q)), p part du segment, q celle de l'autre ; z_X = (O_X - D pi_X) / sqrt(D
# pi_X pi_F)) ; constat de mesure pour les états.

# Cas du corps (Q4, § 2) : pi_F = 1 exactement en double (1e5 / (1e5 + E_X)
# == 1 pour E_X = 5e-12 et 1e-12). Mesure adverse (C7), sur une copie de
# 517469f : F variance_nulle (z NA, deux branches variance_nulle) en ordre
# (F, X), erreur dgesv en ordre (X, F) ; depuis #334 (v_F = w_FX > 0) : F
# produite, theta+ de F non_atteint, theta- de F produite ; X theta+
# produite, theta- non_atteint ; signal de nombre 2. Aucun changement au
# commit de #333 (mesuré, f18d9ec et après).
verifier("I #333 corps : F (1e5, D_F), X (E_X, D_X), E_X dans {5e-12, 1e-12}, (D_F, D_X) dans {(100, 0), (99, 1)}, deux ordres : F et X produite, z_F = -z_X à 1e-10 de la forme close ; theta- de F et theta+ de X produite à 1e-10 de la forme close, theta+ de F et theta- de X non_atteint ; signal de nombre 2", {
  e <- character(0)
  for (eX in c(5e-12, 1e-12)) for (D in list(c(100, 0), c(99, 1))) for (o in list(1:2, 2:1)) {
    nom <- sprintf("E_X %g D %s ordre %s", eX, paste(D, collapse = "/"), paste(o, collapse = ""))
    r <- tryCatch(homog(expo_seg(c("F", "X"), c(60, 60), c(1e5, eX), D)[o, ], "segment", c(60, 60), 60),
                  error = function(x) conditionMessage(x))
    if (is.character(r)) { e <- c(e, sprintf("%s : erreur %s", nom, r)); next }
    pf <- 1e5 / (1e5 + eX); px <- eX / (1e5 + eX); rr <- sqrt(L80_1 / (100 * pf * px))
    sg <- r$resultat$segments[r$resultat$segments$portee == "global", ]; em <- r$resultat$ecart_minimal
    zf <- sg$z[sg$segment == "F"]; zx <- sg$z[sg$segment == "X"]
    f <- match("F", em$segment); x <- match("X", em$segment)
    et <- c(sg$etat[match(c("F", "X"), sg$segment)], em$etat_superieur[f], em$etat_inferieur[f], em$etat_superieur[x],
            em$etat_inferieur[x])
    sig <- r$avertissements$nombre[r$avertissements$code == "ecart_minimal_detectable"]
    pb <- c(if (!identical(et, c("produite", "produite", "non_atteint", "produite", "produite", "non_atteint"))) paste(et, collapse = "/"),
            if (!isTRUE(proche(zx, (D[2L] - 100 * px) / sqrt(100 * px * pf), 1e-10, 0)) || !identical(zf, -zx)) "z",
            if (!isTRUE(proche(em$theta_80_inferieur[f], (1 - px * rr) / (1 + pf * rr), 1e-10, 0))) "theta- de F",
            if (!isTRUE(proche(em$theta_80_superieur[x], (1 + pf * rr) / (1 - px * rr), 1e-10, 0))) "theta+ de X",
            if (!identical(sig, 2L)) "signal")
    if (length(pb)) e <- c(e, sprintf("%s : %s", nom, paste(pb, collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})
# Variante (commentaire de #333 ; Q4, § 3) : A (5000, 600 ; 100, 2), X
# (E_X60, 0 ; 100, 1), âges 60 et 61. Part de X à 60 arrondie à 0 pour E_X60
# dans {4.94e-323, 9.88e-324} : la limite lambda(+-infini) n'est pas
# déterminée par le calcul (terme de la strate 60 perdu) : theta- de A et
# theta+ de X precision_insuffisante (avant, sur une copie de f18d9ec :
# non_atteint, signal à tort) ; theta+ de A et theta- de X non_atteint
# (inchangés) ; T = 1/3 (contribution vraie de la strate 60 sous 1e-320) ;
# nombre 2 (autre branche non_atteint). Témoin E_X60 = 5e-315 (part ~1e-318,
# représentable) : la règle ne joue pas ; theta- de A produite,
# 6.4677876288876102e-316 (mesuré, inchangé), theta+ de X
# precision_insuffisante par une autre cause (inchangé).
verifier("I #333 part arrondie à 0 : A (5000, 600 ; 100, 2), X (E_X60, 0 ; 100, 1), deux ordres ; E_X60 dans {4.94e-323, 9.88e-324} : theta- de A et theta+ de X precision_insuffisante, theta+ de A et theta- de X non_atteint, T = 1/3 à 1e-15, nombre 2 ; témoin 5e-315 : theta- de A produite 6.4677876288876102e-316 à 1e-10", {
  e <- character(0)
  for (eX in c(4.94e-323, 9.88e-324, 5e-315)) for (o in list(1:4, c(3L, 4L, 1L, 2L))) {
    nom <- sprintf("E_X60 %g ordre %s", eX, paste(o, collapse = ""))
    r <- homog(expo_seg(c("A", "A", "X", "X"), c(60, 61, 60, 61), c(5000, 100, eX, 100), c(600, 2, 0, 1))[o, ],
               "segment", c(60, 61), 60)
    em <- r$resultat$ecart_minimal; a <- match("A", em$segment); x <- match("X", em$segment)
    et <- c(em$etat_superieur[a], em$etat_inferieur[a], em$etat_superieur[x], em$etat_inferieur[x])
    att <- if (eX < 1e-320) c("non_atteint", "precision_insuffisante", "precision_insuffisante", "non_atteint")
           else c("non_atteint", "produite", "precision_insuffisante", "non_atteint")
    pb <- c(if (!identical(et, att)) paste(et, collapse = "/"),
            if (!isTRUE(proche(r$resultat$tests$statistique[1L], 1 / 3, 1e-15, 0))) "T",
            if (!identical(r$avertissements$nombre[r$avertissements$code == "ecart_minimal_detectable"], 2L)) "signal",
            if (eX > 1e-320 && !isTRUE(proche(em$theta_80_inferieur[a], 6.4677876288876102e-316, 1e-10, 0))) "theta- de A")
    if (length(pb)) e <- c(e, sprintf("%s : %s", nom, paste(pb, collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " ; ") else TRUE
})

# --- #348 : total des événements gardé avant tout tirage ----------------------
#
# Spécification d'actuary-approfondi de #348 (Q5 ; forme F348). Avant (copie
# de 0990bab) : erreur de R « invalid second argument 'size' » de
# rmultinom(), précédée de l'avertissement « NAs introduced by coercion to
# integer range ». Après : refus homogeneite / denombrement_hors_plage avant
# tout score et tout tirage, gardes dans leur ordre (valeur calculée à la
# main : 2e9 + 2e9 + 1 + 1).
refus_ou_erreur_348 <- function(expr) {
  av <- character(0)
  r <- tryCatch(withCallingHandlers(expr, warning = function(w) { av <<- c(av, conditionMessage(w)); invokeRestart("muffleWarning") }),
                refus_tablexp = function(x) x, error = function(e) e)
  list(r = r, av = av)
}
# controle() est masquée plus haut par une fonction de même nom (test du
# statut theta_80_superieur, évalué dans l'environnement du fichier) : forme
# propre ici.
refus_348 <- function(r, module, motif)
  if (!inherits(r, "refus_tablexp")) "aucun refus levé" else if (!identical(c(r$module, r$motif), c(module, motif)))
    sprintf("refus %s / %s au lieu de %s / %s", r$module, r$motif, module, motif) else TRUE
verifier("I #348 homogénéité : deux segments, 2e9 événements par ligne à 60 et 1 à 61 (total 4000000002) : refus homogeneite / denombrement_hors_plage, grandeur evenements_total, valeur 4000000002, limite .Machine$integer.max ; aucune erreur ni aucun avertissement de R", {
  d <- expo_seg(c("A", "B", "A", "B"), c(60, 60, 61, 61), rep(1e10, 4L), c(2e9, 2e9, 1, 1))
  x <- refus_ou_erreur_348(homog(d, "segment", c(60, 61), 60, B = 19))
  r <- x$r
  tous(refus = if (inherits(r, "refus_tablexp")) TRUE
               else if (inherits(r, "error")) paste("erreur de R :", conditionMessage(r)) else "aucun refus",
       motif = refus_348(r, "homogeneite", "denombrement_hors_plage"),
       grandeur = if (identical(r$details$grandeur, "evenements_total") && identical(r$details$valeur, 4000000002) &&
                      identical(r$details$limite, .Machine$integer.max)) TRUE else "détails du refus",
       avertissements = if (!length(x$av)) TRUE else paste(x$av, collapse = " / "))
})
verifier("I #348 homogénéité, ordre des gardes conservé : 2e9 + 2e9 événements hors de la plage (âge 70), plage 60 seule : refus evenements_hors_plage (valeur 4e9), comme avant", {
  d <- expo_seg(c("A", "B", "A", "B"), c(60, 60, 70, 70), rep(1e10, 4L), c(5, 3, 2e9, 2e9))
  r <- refus_ou_erreur_348(homog(d, "segment", c(60, 60), 60, B = 19))$r
  tous(motif = refus_348(r, "homogeneite", "denombrement_hors_plage"),
       grandeur = if (identical(r$details$grandeur, "evenements_hors_plage") && identical(r$details$valeur, 4e9)) TRUE
                  else paste(r$details$grandeur, r$details$valeur))
})
verifier("I #348 replications_multinomiales() : effectif au-delà de .Machine$integer.max -> erreur de programmation avant tout tirage (complément défensif), sans avertissement de R", {
  x <- refus_ou_erreur_348(tablexp:::replications_multinomiales(c(1, 1), c(1L, 1L), 3e9, 2, 1L,
                                                                 function(Dm) list(t = colSums(Dm))))
  tous(erreur = if (inherits(x$r, "error") && grepl("replications_multinomiales() : groupes, poids ou effectifs invalides",
                                                    conditionMessage(x$r), fixed = TRUE)) TRUE
                else if (inherits(x$r, "error")) conditionMessage(x$r) else "aucune erreur",
       avertissements = if (!length(x$av)) TRUE else paste(x$av, collapse = " / "))
})
# Garde avancée devant segments/observes (m1 de l'audit de l'étape (5) ;
# spécification, Q5, § 2) : un seul segment, 2e9 + 1e9 événements. Avant
# (copie de 0990bab) : refus sur segments/observes (portee()) ; après :
# evenements_total, même motif (valeur 3e9, calculée à la main).
verifier("I #348 garde avancée : un segment, 2e9 + 1e9 événements (âges 60 et 61) : refus homogeneite / denombrement_hors_plage, grandeur evenements_total (et non plus segments/observes), valeur 3e9", {
  r <- refus_ou_erreur_348(homog(expo_seg(c("A", "A"), 60:61, c(1e10, 1e10), c(2e9, 1e9)), "segment", c(60, 61), 60,
                                 B = 19))$r
  tous(motif = refus_348(r, "homogeneite", "denombrement_hors_plage"),
       grandeur = if (identical(r$details$grandeur, "evenements_total") && identical(r$details$valeur, 3e9)) TRUE
                  else paste(r$details$grandeur, r$details$valeur))
})
# Ordre des deux premières gardes (m2) : cellule 60 fautive (E = 0, 2e9 + 2e9
# décès) et 2e9 + 2e9 décès hors plage (âge 70) : les deux décomptes
# dépassent l'entier maximal ; la première garde dans l'ordre est citée
# (mesuré, identique à 0990bab).
verifier("I #348 ordre des gardes : événements sans exposition dans la plage (4e9) et hors plage (4e9) au-delà de l'entier maximal : refus sur evenements_cellules_evenement_sans_exposition, la première dans l'ordre", {
  d <- expo_seg(c("A", "B", "A", "B", "A", "B"), c(60, 60, 61, 61, 70, 70), c(0, 0, 1e10, 1e10, 1e10, 1e10),
                c(2e9, 2e9, 5, 3, 2e9, 2e9))
  r <- refus_ou_erreur_348(homog(d, "segment", c(60, 61), 60, B = 19))$r
  tous(motif = refus_348(r, "homogeneite", "denombrement_hors_plage"),
       grandeur = if (identical(r$details$grandeur, "evenements_cellules_evenement_sans_exposition") &&
                      identical(r$details$valeur, 4e9)) TRUE else paste(r$details$grandeur, r$details$valeur))
})
