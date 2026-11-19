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
verifier("A états : tests, p exacte, segments", {
  identical(tablexp:::ETATS_TESTS_HOMOGENEITE,
            c("produite", "evenement_sans_exposition", "moins_de_deux_segments_informatifs")) &&
    identical(tablexp:::ETATS_P_EXACTE_HOMOGENEITE,
              c("produite", "plus_de_deux_groupes", "convolution_au_dela_n_max", "non_produite_par_cette_version",
                "evenement_sans_exposition", "moins_de_deux_segments_informatifs")) &&
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
verifier("T202 STATUTS_HOMOGENEITE : p_asymptotique sans condition sur les parts des strates, à nombre de groupes fixé, D_. borné seulement quand le nombre de strates croît ; z inchangé ; aucune source citée", {
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
                       "H0 (V_ss -> infini, même régime que p_asymptotique), E tenue pour fixe ; aucune p-valeur produite ;",
                       "calcul en arithmétique flottante, approximation numérique ; aucune correction de multiplicité ;",
                       tablexp:::INDEPENDANCE_HOMOGENEITE))
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
                             c("produite", "non_atteint", "evenement_sans_exposition", "moins_de_deux_segments_informatifs",
                               "segment_sans_exposition", "variance_nulle"))) TRUE else "états",
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
# Texte de la spécification, § 7 (theta_80_superieur), et ses quatre
# substitutions pour theta_80_inferieur.
STATUT_SUP_218 <- paste(
  "approximation asymptotique : plus petit theta > 1 tel que lambda_s(theta) >= lambda_80, theta multipliant la force",
  "du seul segment s dans chaque cellule ; lambda_s(theta) = delta_s(theta)' V^- delta_s(theta), delta_s(theta) = somme",
  "sur les strates informatives du test global de D_.c (p_c(theta) - pi_c), p_jc(theta) = pi_jc w_j / somme sur k de",
  "pi_kc w_k, w_s = theta, w_j = 1 sinon : espérance exacte de U sachant les D_.c sous le modèle de Poisson à exposition",
  "tenue pour fixe ; V variance de U sous H0, même système réduit que le test global ; lambda_80 : non-centralité pour",
  "laquelle la loi du khi-deux non centrale à ddl degrés de liberté (ddl du test global) dépasse le quantile d'ordre",
  "1 - niveau_ecart_minimal de la loi centrale avec la probabilité puissance_ecart_minimal (conventions) ; la variance",
  "de U sous l'alternative diffère de V : à deux segments en strate unique, en approximation normale, la puissance",
  "réelle en theta_80 s'écarte de puissance_ecart_minimal de 0,03 au plus quand theta_80 est au plus",
  "seuil_ecart_minimal, davantage au-delà, et la valeur est optimiste pour un segment minoritaire (theta_80 réel plus",
  "éloigné de 1) ; au-delà de deux segments ou à parts différentes entre strates, aucune borne établie ; lambda_80",
  "(pchisq non centrale) et theta_80 (uniroot) par résolution numérique : approximation numérique ; état non_atteint",
  "(NA) quand lambda_s(theta) reste au plus lambda_80 (1 + 1e-8), tolérance relative de la résolution numérique,",
  "lorsque theta tend vers l'infini ; unités d'observation indépendantes ; sous l'unité contrat, les décès sont",
  "sur-dispersés et theta_80 est trop proche de 1 (écart minimal détectable sous-estimé)")
verifier("I STATUTS_HOMOGENEITE (littéral de la spécification, § 7) : deux lignes ecart_minimal en fin (theta_80_superieur, theta_80_inferieur), textes exacts, inférieur par les quatre substitutions ; 15 lignes, UTF-8", {
  s <- tablexp:::STATUTS_HOMOGENEITE
  inf <- STATUT_SUP_218
  for (r in list(c("plus petit theta > 1", "plus grand theta < 1"),
                 c("quand theta_80 est au plus seuil_ecart_minimal", "quand theta_80 est au moins 1/seuil_ecart_minimal"),
                 c("la valeur est optimiste pour un segment minoritaire (theta_80 réel plus éloigné de 1)",
                   "la valeur est prudente pour un segment minoritaire (theta_80 réel plus proche de 1)"),
                 c("lorsque theta tend vers l'infini", "lorsque theta tend vers 0"))) {
    if (lengths(regmatches(inf, gregexpr(r[1L], inf, fixed = TRUE))) != 1L) return(paste("substitution non unique :", r[1L]))
    inf <- sub(r[1L], r[2L], inf, fixed = TRUE)
  }
  tous(lignes = if (nrow(s) == 15L && identical(s$table[14:15], c("ecart_minimal", "ecart_minimal")) &&
                    identical(s$grandeur[14:15], c("theta_80_superieur", "theta_80_inferieur"))) TRUE else "lignes",
       sup = if (identical(s$statut[14L], STATUT_SUP_218)) TRUE else "texte de theta_80_superieur",
       inf = if (identical(s$statut[15L], inf)) TRUE else "texte de theta_80_inferieur",
       utf8 = if (all(Encoding(s$statut[14:15]) == "UTF-8")) TRUE else "encodage")
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
verifier("I monotonie et extrémalité (contrôle sur un cas, non démonstratif (monotonie non établie à S > 2 sur plusieurs strates)) : lambda_s(theta) < lambda_80 entre 1 et theta_80 sur chaque branche (grille de 50 points), S = 3 à parts différentes", {
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
