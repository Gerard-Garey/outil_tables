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

verifier("A ETAPES : homogeneite après positionnement, avant validation (F10), module validation, calculer_homogeneite, préalables donnees et exposition, aléatoire", {
  e <- tablexp:::ETAPES
  k <- match("homogeneite", e$etape)
  identical(k, 5L) && identical(e$etape[k + c(-1L, 1L)], c("positionnement", "validation")) &&
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
verifier("G objet : parametres, resultat (tests, segments, monte_carlo), diagnostics ; graine du plan consignée", {
  identical(names(H), c("parametres", "resultat", "diagnostics")) &&
    identical(names(H$parametres), c("plage_ages", "tranches_age", "replications", "convention", "hypothese_nulle",
                                     "statuts", "reference_methode")) &&
    identical(names(H$resultat), c("tests", "segments", "monte_carlo")) &&
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
verifier("H rapport : section etape_homogeneite entre etape_positionnement et etape_validation ; mention fixe ; tables tests et segments", {
  l <- lignes_rapport(X)
  s <- section(l, "etape_homogeneite")
  ids <- sub("^<section id=\"([a-z_]+)\">$", "\\1", grep("^<section id=", l, value = TRUE))
  k <- match("etape_homogeneite", ids)
  m <- tablexp:::MENTIONS_RAPPORT[["homogeneite_portee"]]
  length(s) > 0L && identical(ids[k + c(-1L, 1L)], c("etape_positionnement", "etape_validation")) &&
    any(s == paste0("<p>", m, "</p>")) &&
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
