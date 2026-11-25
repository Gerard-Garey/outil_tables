# -----------------------------------------------------------------------------
#  tests/unitaires/test_independance_unites.R : mention d'indépendance des
#  unités d'observation dans la colonne statut des lignes de loi (issue
#  #159, critère C3 ; spécification d'actuary-approfondi, #159, commentaire
#  7012174895 ; décisions du mainteneur du 19/11/2026 : D2 (a), P1-a (A),
#  P1-b ligne 11 incluse)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - littéral de la spécification : lignes qui portent la mention (une fois)
#    et lignes qui ne la portent pas, table par table ; textes des mentions ;
#  - identité exacte : décomptes sous LC_ALL=C (sous-processus) égaux aux
#    décomptes de la session.
#  - valeur calculée à la main : témoins d'altération sur des copies des
#    tables (marqueur ajouté, retiré ou doublé : écart signalé).
#  Marqueur : « unités d'observation indépendantes » ou « indépendance des
#  unités d'observation » (ancienne forme de la ligne 12 de STATUTS_BRUT,
#  Kaplan-Meier, remplacée par la mention brut_intervalle : avis d'actuary
#  sur #359, Q1 ; gardée au décompte, pour qu'une réintroduction à côté de la
#  mention compte comme un doublon), compté par ligne sur
#  les octets (useBytes), donc sans dépendre de la locale ni du marquage
#  (STATUTS_BRUT n'est pas marqué UTF-8, #259).
# -----------------------------------------------------------------------------

MARQUEURS_159 <- c("unités d'observation indépendantes", "indépendance des unités d'observation")
# Nombre d'occurrences du marqueur (l'une ou l'autre forme) dans chaque chaîne.
compter_159 <- function(x)
  as.integer(Reduce(`+`, lapply(MARQUEURS_159, function(m)
    vapply(gregexpr(m, x, fixed = TRUE, useBytes = TRUE), function(g) sum(g > 0L), 0L))))
cle_159 <- function(d) paste(d[[1L]], d[[2L]], sep = "/")
# Écart entre les décomptes de `d` et l'attendu : 1 sur les clés `avec`, 0
# ailleurs ; TRUE ou le texte de l'écart. Clés `k` : cle_159(d) par défaut,
# d$grandeur pour STATUTS_LISSAGE_POISSON (colonnes grandeur et statut seules,
# #329 ; cle_159() y donnerait grandeur/statut).
ecart_159 <- function(d, avec, k = cle_159(d)) {
  n <- compter_159(d$statut)
  inconnues <- setdiff(avec, k)
  if (length(inconnues)) return(paste("clés absentes :", paste(inconnues, collapse = ", ")))
  att <- as.integer(k %in% avec)
  if (identical(n, att)) TRUE
  else paste(sprintf("%s : %d au lieu de %d", k[n != att], n[n != att], att[n != att]), collapse = " ; ")
}

# Nature de la conséquence de chaque ligne marquée (#330, F330 ;
# spécification d'actuary-approfondi, #330, commentaire 7102990712, § 1 et
# § 2) : préfixe commun M_PREFIXE_330 (marqueur et cause) suivi de la
# conséquence de la nature ; khi2_pearson/variance_h0 est `variance` sous
# STATUTS_VALIDATION, `valeur_h0` sous STATUTS_VALIDATION_POSITIONNEE
# (conditionnement par O_s). Textes en échappements \u (UTF-8 en toute locale).
M_PREFIXE_330 <- paste("unit\u00e9s d'observation ind\u00e9pendantes ; sous l'unit\u00e9 contrat, les d\u00e9c\u00e8s",
                       "sont sur-dispers\u00e9s et ")
# Natures tests et intervalle qualifiées par #359 (F359, Q-A (a) ;
# spécification d'actuary-approfondi, #359, commentaire 7137095929) ;
# ecart_minimal (theta_80 de l'homogénéité) ajoutée par #359 ; credibilite
# (bornes de crédibilité bayésiennes de wh_poisson) ajoutée par #359 (décision
# du mainteneur D-b (a) du 25/11/2026 ; spécification d'actuary-approfondi) ;
# contraste_segments (segments/z de l'homogénéité, sans objet) ajoutée par
# #359 (décision du mainteneur du 25/11/2026, avis d'actuary à l'étape (9)).
CONSEQUENCES_330 <- c(
  tests = paste("les tests sont anti-conservateurs \u00e0 attendus grands (r\u00e9sultat asymptotique), l'erreur de",
                "niveau \u00e9tant de sens non \u00e9tabli en g\u00e9n\u00e9ral \u00e0 petits attendus"),
  intervalle = paste("l'intervalle est trop \u00e9troit \u00e0 attendus grands (r\u00e9sultat asymptotique), l'\u00e9cart de",
                     "couverture \u00e9tant de sens non \u00e9tabli en g\u00e9n\u00e9ral \u00e0 petits attendus"),
  credibilite = paste("l'intervalle, \u00e0 ajustement donn\u00e9, est plus \u00e9troit que sous un mod\u00e8le de",
                      "quasi-Poisson (demi-largeur sur l'\u00e9chelle de theta divis\u00e9e par la racine du facteur de",
                      "sur-dispersion), sans cons\u00e9quence \u00e9tablie sur la couverture"),
  variance_x2 = "Var(X2) est sous-estim\u00e9e",
  variance_a = "Var(A^) est sous-estim\u00e9e",
  valeur_h0 = "la valeur sous H0 (i) diff\u00e8re de celle de la loi r\u00e9elle, dans un sens non \u00e9tabli en g\u00e9n\u00e9ral",
  niveau = "le niveau n'est plus garanti",
  ecart_minimal = "theta_80 est trop proche de 1 (\u00e9cart minimal d\u00e9tectable sous-estim\u00e9)",
  contraste_segments = paste("V_ss sous-estime la variance de O_s - A_s \u00e0 attendus grands si les contrats d'une",
                             "m\u00eame t\u00eate sont dans un m\u00eame segment (r\u00e9sultat asymptotique), dans un sens",
                             "non \u00e9tabli en g\u00e9n\u00e9ral \u00e0 petits attendus, et peut la surestimer si des",
                             "contrats d'une m\u00eame t\u00eate sont dans des segments diff\u00e9rents"))
mention_330 <- function(n) paste0(M_PREFIXE_330, CONSEQUENCES_330[[n]])
# Natures brut_* (STATUTS_BRUT, #359) : préfixe des contrats d'une même tête,
# conséquence, puis renvoi au relevé tetes_multi_contrats.
M_BRUT_359 <- c(
  brut_intervalle = paste("unit\u00e9s d'observation ind\u00e9pendantes ; sous l'unit\u00e9 contrat, les contrats d'une m\u00eame",
                          "t\u00eate sont suppos\u00e9s ind\u00e9pendants (une d\u00e9pendance positive rend l'intervalle trop",
                          "\u00e9troit \u00e0 grand nombre attendu d'\u00e9v\u00e9nements (r\u00e9sultat asymptotique), l'\u00e9cart de",
                          "couverture \u00e9tant de sens non \u00e9tabli en g\u00e9n\u00e9ral \u00e0 petit nombre attendu ; relev\u00e9",
                          "tetes_multi_contrats de l'\u00e9tape donnees)"),
  brut_variance = paste("unit\u00e9s d'observation ind\u00e9pendantes ; sous l'unit\u00e9 contrat, les contrats d'une m\u00eame",
                        "t\u00eate sont suppos\u00e9s ind\u00e9pendants (une d\u00e9pendance positive rend la variance trop",
                        "faible ; relev\u00e9 tetes_multi_contrats de l'\u00e9tape donnees)"))
ESPERANCES_330 <- c("residus/esperance_au_dela_2", "residus/esperance_au_dela_3", "signes/esperance_positifs_h0",
                    "fluctuation/esperance_hors_intervalle_exact",
                    "fluctuation/esperance_hors_intervalle_asymptotique")
NIVEAU_330 <- c("signes/p_monte_carlo", "runs/p_monte_carlo", "wilcoxon/p_monte_carlo")
# STATUTS_VALIDATION : 10 + 3 + 1 + 5 + 3 = 22 lignes de loi.
NATURES_V_330 <- list(
  tests = c("khi2_pearson/p_asymptotique", "khi2_pearson/p_monte_carlo", "deviance/p_asymptotique",
            "deviance/p_monte_carlo", "smr/p_inferieure", "smr/p_superieure", "smr/p_centrale",
            "smr/z_racine_cubique", "smr/p_racine_cubique", "sous_groupes/p_valeurs"),
  intervalle = c("smr/borne_inf_exacte", "smr/borne_sup_exacte", "sous_groupes/bornes_exactes"),
  variance_x2 = "khi2_pearson/variance_h0",
  valeur_h0 = ESPERANCES_330,
  niveau = NIVEAU_330)
# STATUTS_VALIDATION_POSITIONNEE : 6 + 1 + 6 + 3 = 16 lignes de loi.
NATURES_P_330 <- list(
  tests = c("khi2_pearson/p_asymptotique", "khi2_pearson/p_monte_carlo", "deviance/p_asymptotique",
            "deviance/p_monte_carlo", "sous_groupes/p_valeurs", "sous_groupes/p_valeurs_asymptotiques"),
  intervalle = "sous_groupes/bornes_exactes",
  valeur_h0 = c(ESPERANCES_330, "khi2_pearson/variance_h0"),
  niveau = NIVEAU_330)
# STATUTS_IMPORTANCE : 3 + 1 = 4 lignes de loi.
NATURES_I_330 <- list(
  intervalle = c("smr_comparateur_1/bornes_exactes", "smr_comparateur_1/bornes_asymptotiques",
                 "smr_comparateur_2/bornes"),
  variance_a = "smr_comparateur_1/rapport_variance")
LOI_V_330 <- unlist(NATURES_V_330, use.names = FALSE)
# Lignes de `d` (clés `cle_159`) dont le statut ne contient pas la mention de
# leur nature selon `natures` ; character(0) si toutes la portent.
sans_mention_330 <- function(d, natures) {
  k <- cle_159(d); st <- enc2utf8(d$statut)
  unlist(lapply(names(natures), function(n) {
    i <- match(natures[[n]], k)
    natures[[n]][is.na(i) | !grepl(mention_330(n), st[i], fixed = TRUE)]
  }))
}
M_TESTS_159 <- mention_330("tests")

# Écart de la table `d` (forme de STATUTS_BRUT) à l'attendu par position :
# 1 sur les lignes 2, 3, 5, 6, 8, 9, 11 et 12, 0 ailleurs ; TRUE ou le texte.
ecart_brut_159 <- function(d) {
  n <- compter_159(d$statut)
  att <- as.integer(seq_len(nrow(d)) %in% c(2L, 3L, 5L, 6L, 8L, 9L, 11L, 12L))
  if (identical(n, att)) TRUE else paste("décomptes :", paste(n, collapse = " "))
}
verifier("#159 C3 STATUTS_BRUT (littéral de la spécification) : marqueur une fois sur les lignes 2, 3, 5, 6, 8, 9, 11 et 12, jamais ailleurs (estimations ponctuelles 1, 4, 7, 10 ; ligne 13)",
         if (nrow(tablexp:::STATUTS_BRUT) != 13L) "STATUTS_BRUT : 13 lignes attendues" else ecart_brut_159(tablexp:::STATUTS_BRUT))
verifier("#159 #359 C3 STATUTS_BRUT (littéral de la spécification) : mention en fin de statut (intervalles 2, 3, 5, 6, 8, 9 et 12 (Kaplan-Meier, avis d'actuary sur #359, Q1), nature brut_intervalle ; variance 11, nature brut_variance) ; loi_supposee sans marqueur ; ancienne forme de la ligne 12 absente", {
  s <- tablexp:::STATUTS_BRUT
  m_int <- paste(";", M_BRUT_359[["brut_intervalle"]])
  m_var <- paste(";", M_BRUT_359[["brut_variance"]])
  fin <- function(x, m) endsWith(enc2utf8(x), m)
  tous <- c(int = all(fin(s$statut[c(2L, 3L, 5L, 6L, 8L, 9L)], m_int)), var = fin(s$statut[11L], m_var),
            loi = all(compter_159(s$loi_supposee) == 0L),
            km = fin(s$statut[12L], paste("asymptotique (ensemble à risque -> infini sur toute la cellule) ; non",
                                          "défini à q = 0 et q = 1", m_int)) &&
                 !any(grepl(MARQUEURS_159[2L], enc2utf8(s$statut), fixed = TRUE)))
  if (all(tous)) TRUE else paste("échec :", paste(names(tous)[!tous], collapse = ", "))
})
verifier("#330 #359 C2 mention_independance() (litt\u00e9ral de la sp\u00e9cification) : pr\u00e9fixe commun suivi de la cons\u00e9quence exacte de chacune des dix natures (tests, intervalle, credibilite, variance, valeur_h0, niveau, ecart_minimal, brut_intervalle, brut_variance, contraste_segments), objet de la variance repris", {
  f <- tablexp:::mention_independance
  obt <- enc2utf8(c(f("tests"), f("intervalle"), f("credibilite"), f("variance", "Var(X2)"), f("variance", "Var(A^)"),
                    f("valeur_h0"), f("niveau"), f("ecart_minimal"), f("brut_intervalle"), f("brut_variance"),
                    f("contraste_segments")))
  att <- c(vapply(c("tests", "intervalle", "credibilite", "variance_x2", "variance_a", "valeur_h0", "niveau",
                    "ecart_minimal"),
                  mention_330, ""), M_BRUT_359, mention_330("contraste_segments"))
  if (identical(obt, unname(att))) TRUE else paste("obtenu :", paste(obt, collapse = " | "))
})
verifier("#330 C3 mention_independance() (litt\u00e9ral de la sp\u00e9cification) : nature inconnue, variance sans objet et objet hors de la nature variance refus\u00e9s (erreur de programmation)", {
  f <- tablexp:::mention_independance
  r <- c(inconnue = inherits(try(f("esperance"), silent = TRUE), "try-error"),
         variance_sans_objet = inherits(try(f("variance"), silent = TRUE), "try-error"),
         objet_hors_variance = inherits(try(f("tests", "Var(X2)"), silent = TRUE), "try-error"),
         objet_vide_hors_variance = inherits(try(f("tests", ""), silent = TRUE), "try-error"),
         objet_na_hors_variance = inherits(try(f("niveau", NA_character_), silent = TRUE), "try-error"),
         objet_numerique_hors_variance = inherits(try(f("intervalle", 1), silent = TRUE), "try-error"),
         objet_numerique_variance = inherits(try(f("variance", 1), silent = TRUE), "try-error"),
         objet_ecart_minimal = inherits(try(f("ecart_minimal", "theta_80"), silent = TRUE), "try-error"),
         objet_credibilite = inherits(try(f("credibilite", "theta"), silent = TRUE), "try-error"),
         objet_brut_variance = inherits(try(f("brut_variance", "Var(X2)"), silent = TRUE), "try-error"),
         objet_contraste_segments = inherits(try(f("contraste_segments", "V_ss"), silent = TRUE), "try-error"))
  if (all(r)) TRUE else paste("non refus\u00e9 :", paste(names(r)[!r], collapse = ", "))
})
verifier("#159 #330 C3 STATUTS_VALIDATION (litt\u00e9ral de la sp\u00e9cification) : marqueur une fois sur les 22 lignes de loi, jamais ailleurs ; cons\u00e9quence de la nature de chaque ligne (10 tests, 3 intervalles, 1 variance Var(X2), 5 valeurs sous H0 (i), 3 niveaux) ; mention en fin de statut, avant la relation ajustee_hors_outil pour les deux lignes sous_groupes",
  {
    s <- tablexp:::STATUTS_VALIDATION; k <- cle_159(s); st <- enc2utf8(s$statut)
    e <- ecart_159(s, LOI_V_330)
    sg <- c("sous_groupes/p_valeurs", "sous_groupes/bornes_exactes")
    fin <- unlist(lapply(names(NATURES_V_330), function(n) {
      g <- setdiff(NATURES_V_330[[n]], sg); g[!endsWith(st[match(g, k)], mention_330(n))]
    }))
    if (!isTRUE(e)) e
    else if (length(m <- sans_mention_330(s, NATURES_V_330))) paste("cons\u00e9quence absente :", paste(m, collapse = ", "))
    else if (length(fin)) paste("mention hors de la fin du statut :", paste(fin, collapse = ", "))
    else if (!grepl(paste(mention_330("tests"), "; relation ajustee_hors_outil"), st[k == "sous_groupes/p_valeurs"], fixed = TRUE) ||
             !grepl(paste(mention_330("intervalle"), "; relation ajustee_hors_outil"), st[k == "sous_groupes/bornes_exactes"], fixed = TRUE))
      "sous_groupes : mention avant la relation ajustee_hors_outil"
    else TRUE
  })
verifier("#159 #330 C3 STATUTS_VALIDATION_POSITIONNEE (litt\u00e9ral de la sp\u00e9cification) : marqueur une fois sur les 16 lignes (22 lignes de loi moins les 7 lignes smr non produites, plus sous_groupes/p_valeurs_asymptotiques), jamais ailleurs (aucun doublon) ; cons\u00e9quence de la nature de chaque ligne (6 tests, 1 intervalle, 6 valeurs sous H0 (i) dont khi2_pearson/variance_h0, 3 niveaux)", {
  s <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  e <- ecart_159(s, unlist(NATURES_P_330, use.names = FALSE))
  if (!isTRUE(e)) e
  else if (length(m <- sans_mention_330(s, NATURES_P_330))) paste("cons\u00e9quence absente :", paste(m, collapse = ", "))
  else TRUE
})
verifier("#159 #329 C3 STATUTS_VALIDATION_LISSEE (littéral de la spécification) : marqueur sur aucune ligne (relation lissee non retenue, #329, § 3)",
         ecart_159(tablexp:::STATUTS_VALIDATION_LISSEE, character(0)))
# STATUTS_LISSAGE_POISSON (#329 ; spécification d'actuary-approfondi, #329,
# commentaire 7102999392, § 1, § 2 et § 4) : 2 lignes de loi sur 12, nature
# credibilite (#359, décision du mainteneur D-b (a) du 25/11/2026), lues par
# grandeur seule ; fragment égal à l'octet à mention_independance("credibilite").
BORNES_329 <- c("table_lissee/borne_inf_credibilite", "table_lissee/borne_sup_credibilite")
verifier("#329 C3 STATUTS_LISSAGE_POISSON (litt\u00e9ral de la sp\u00e9cification) : 12 lignes ; marqueur une fois sur les deux bornes de cr\u00e9dibilit\u00e9, jamais ailleurs (parametre_lissage et laml comprises) ; mention de la nature credibilite (#359, D-b (a)) en fin de statut, texte exact et identique \u00e0 l'octet \u00e0 mention_independance(\"credibilite\")", {
  s <- tablexp:::STATUTS_LISSAGE_POISSON; st <- enc2utf8(s$statut[match(BORNES_329, s$grandeur)])
  e <- ecart_159(s, BORNES_329, s$grandeur)
  m <- enc2utf8(tablexp:::mention_independance("credibilite"))
  if (nrow(s) != 12L || !identical(names(s), c("grandeur", "statut"))) "STATUTS_LISSAGE_POISSON : 12 lignes, colonnes grandeur et statut attendues"
  else if (!isTRUE(e)) e
  else if (!identical(m, mention_330("credibilite"))) "mention_independance(\"credibilite\") diff\u00e8re du litt\u00e9ral"
  else if (!all(endsWith(st, paste(";", mention_330("credibilite"))))) "texte ou place de la mention (litt\u00e9ral)"
  else if (!all(endsWith(st, paste(";", m)))) "fragment diff\u00e9rent de mention_independance(\"credibilite\")"
  else TRUE
})
verifier("#159 #330 C3 STATUTS_IMPORTANCE (litt\u00e9ral de la sp\u00e9cification) : marqueur une fois sur les 4 lignes de loi (bornes du comparateur 1, rapport_variance, bornes du comparateur 2), jamais ailleurs ; cons\u00e9quence de la nature (3 intervalles, 1 variance Var(A^)) ; mention en fin de statut, avant la relation ajustee_hors_outil pour smr_comparateur_2/bornes", {
  s <- tablexp:::STATUTS_IMPORTANCE; k <- cle_159(s); st <- enc2utf8(s$statut)
  e <- ecart_159(s, unlist(NATURES_I_330, use.names = FALSE))
  if (!isTRUE(e)) e
  else if (length(m <- sans_mention_330(s, NATURES_I_330))) paste("cons\u00e9quence absente :", paste(m, collapse = ", "))
  else if (!all(endsWith(st[k %in% c("smr_comparateur_1/bornes_exactes", "smr_comparateur_1/bornes_asymptotiques")],
                         paste(";", mention_330("intervalle")))) ||
           !endsWith(st[k == "smr_comparateur_1/rapport_variance"], paste(";", mention_330("variance_a"))) ||
           !grepl(paste(mention_330("intervalle"), "; relation ajustee_hors_outil"), st[k == "smr_comparateur_2/bornes"], fixed = TRUE))
    "texte ou place de la mention"
  else TRUE
})
TABLES_159 <- c("STATUTS_BRUT", "STATUTS_VALIDATION", "STATUTS_VALIDATION_POSITIONNEE", "STATUTS_VALIDATION_LISSEE",
                "STATUTS_IMPORTANCE", "STATUTS_POSITIONNEMENT", "STATUTS_HOMOGENEITE", "STATUTS_LISSAGE_POISSON")
verifier("#159 C3 garde générale (littéral de la spécification) : marqueur au plus une fois sur toute ligne des huit tables de statuts, θ_80 de l'homogénéité compris", {
  m <- vapply(TABLES_159, function(n) max(compter_159(get(n, envir = asNamespace("tablexp"))$statut)), 0L)
  if (all(m <= 1L)) TRUE else paste(sprintf("%s : %d", names(m)[m > 1L], m[m > 1L]), collapse = " ; ")
})
verifier("#159 C3 sous LC_ALL=C (identité exacte) : décomptes du marqueur par ligne des huit tables égaux à ceux de la session (sous-processus)", {
  f <- tempfile(fileext = ".R"); on.exit(unlink(f))
  writeLines(c(
    'suppressMessages(library(tablexp, lib.loc = Sys.getenv("TABLEXP_BIBLIOTHEQUE")))',
    # Marqueurs écrits en échappements \u (deparse() sous la locale C les
    # écrirait en <U+00E9>, illisibles comme octets UTF-8).
    'm <- c("unit\\u00e9s d\'observation ind\\u00e9pendantes", "ind\\u00e9pendance des unit\\u00e9s d\'observation")',
    sprintf("tables <- c(%s)", paste(deparse(TABLES_159), collapse = "")),
    'n <- vapply(tables, function(t) paste(Reduce(`+`, lapply(m, function(u) vapply(gregexpr(u, get(t, envir = asNamespace("tablexp"))$statut, fixed = TRUE, useBytes = TRUE), function(g) sum(g > 0L), 0L))), collapse = ""), "")',
    'cat("RESULTAT", Sys.getlocale("LC_CTYPE"), paste(n, collapse = "|"), "\\n")'), f)
  s <- executer_rscript(f, env = c("LC_ALL=C", "LANG=C"))
  l <- grep("^RESULTAT", s$sortie, value = TRUE)
  att <- paste(vapply(TABLES_159, function(t) paste(compter_159(get(t, envir = asNamespace("tablexp"))$statut),
                                                    collapse = ""), ""), collapse = "|")
  if (s$statut != 0L || length(l) != 1L) paste(s$sortie, collapse = " | ")
  else if (identical(l, paste("RESULTAT C", att, ""))) TRUE else paste("obtenu :", l, "; attendu :", att)
})
# #359 (F359, D3 (a) ; spécification d'actuary-approfondi, #359, commentaire
# 7137095929) : source unique de la mention, mention_independance() de
# R/brut.R.
PREFIXE_359 <- "unit\u00e9s d'observation ind\u00e9pendantes ; sous l'unit\u00e9 contrat"
# Recherche robuste aux coupures (reprise de l'audit léger de #359, C2) :
# texte joint de chaque fichier, échappements \uXXXX décodés, jonctions de
# chaînes « ", " » (paste, paste0 coupés, y compris d'une ligne à l'autre)
# retirées, puis tous les blancs retirés ; le préfixe est cherché sans ses
# blancs, sur les octets. Rend le nombre d'occurrences par fichier (fichiers
# où il est non nul), entier nommé.
compter_prefixe_359 <- function(dossier) {
  cible <- gsub("[[:space:]]+", "", enc2utf8(PREFIXE_359), useBytes = TRUE)
  fichiers <- list.files(dossier, pattern = "\\.[Rr]$", full.names = TRUE)
  n <- vapply(fichiers, function(f) {
    x <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    for (e in unique(regmatches(x, gregexpr("\\\\u[0-9a-fA-F]{4}", x, useBytes = TRUE))[[1L]]))
      x <- gsub(e, enc2utf8(intToUtf8(strtoi(substr(e, 3L, 6L), 16L))), x, fixed = TRUE, useBytes = TRUE)
    x <- gsub("\"[[:space:]]*,[[:space:]]*\"", "", x, useBytes = TRUE)
    x <- gsub("[[:space:]]+", "", x, useBytes = TRUE)
    sum(gregexpr(cible, x, fixed = TRUE, useBytes = TRUE)[[1L]] > 0L)
  }, 0L)
  n <- stats::setNames(as.integer(n), basename(fichiers))
  n[n > 0L]
}
verifier("#359 C1-a source unique (garde sur le code) : la chaîne fixe « unités d'observation indépendantes ; sous l'unité contrat » une seule fois dans R/*.R, dans R/brut.R, coupures de ligne, jonctions de chaînes et échappements \\u compris ; INDEPENDANCE_HOMOGENEITE absente", {
  n <- compter_prefixe_359(file.path(RACINE, "R"))
  fichiers <- list.files(file.path(RACINE, "R"), pattern = "\\.[Rr]$", full.names = TRUE)
  constante <- any(vapply(fichiers, function(f)
    any(grepl("INDEPENDANCE_HOMOGENEITE", readLines(f, encoding = "UTF-8", warn = FALSE), fixed = TRUE)), NA))
  if (identical(n, c(brut.R = 1L)) && !constante &&
      !exists("INDEPENDANCE_HOMOGENEITE", envir = asNamespace("tablexp"), inherits = FALSE)) TRUE
  else paste("occurrences :", paste(names(n), n, sep = " ", collapse = ", "), "; INDEPENDANCE_HOMOGENEITE :", constante)
})
verifier("#359 C1-a témoin (valeur calculée à la main) : copie de R/ à laquelle on ajoute, dans positionnement.R, le littéral sur une ligne, puis, dans lissage.R, un paste() coupé entre « l'unité » et « contrat » sur deux lignes, puis, dans validation.R, un paste0() coupé en échappements \\u -> occurrence comptée dans chaque fichier", {
  d <- tempfile("r_359_"); dir.create(d); on.exit(unlink(d, recursive = TRUE))
  file.copy(list.files(file.path(RACINE, "R"), pattern = "\\.[Rr]$", full.names = TRUE), d)
  ajouter <- function(f, l) {
    con <- file(file.path(d, f), open = "ab"); on.exit(close(con))
    writeBin(charToRaw(enc2utf8(paste0(paste(l, collapse = "\n"), "\n"))), con)
  }
  ajouter("positionnement.R",
          "x_359 <- \"unités d'observation indépendantes ; sous l'unité contrat, les décès\"")
  n1 <- compter_prefixe_359(d)
  ajouter("lissage.R", c("y_359 <- paste(\"unités d'observation indépendantes ; sous l'unité\",",
                         "               \"contrat, les décès\")"))
  n2 <- compter_prefixe_359(d)
  ajouter("validation.R", c("z_359 <- paste0(\"unit\\u00e9s d'observation ind\\u00e9pendantes ; sous l'\",",
                            "                \"unit\\u00e9 contrat\")"))
  n3 <- compter_prefixe_359(d)
  if (identical(n1, c(brut.R = 1L, positionnement.R = 1L)) &&
      identical(n2, c(brut.R = 1L, lissage.R = 1L, positionnement.R = 1L)) &&
      identical(n3, c(brut.R = 1L, lissage.R = 1L, positionnement.R = 1L, validation.R = 1L))) TRUE
  else paste("obtenu :", paste(names(n3), n3, collapse = ", "), "| n1 :", paste(names(n1), n1, collapse = ", "),
             "| n2 :", paste(names(n2), n2, collapse = ", "))
})
# C1-b : chaque occurrence du préfixe dans les tables de statuts commence une
# sortie exacte de mention_independance(), suivie de la fin du statut, de
# « ; » ou de « : » (fragment cond de STATUTS_VALIDATION_POSITIONNEE, suivi
# de la formule de l'espérance) ; décomptes par nature de la spécification. Rend TRUE ou le texte de
# l'écart ; `tables` : liste nommée de vecteurs de statuts.
TABLES_359 <- c("STATUTS_BRUT", "STATUTS_LISSAGE_POISSON", "STATUTS_POSITIONNEMENT", "STATUTS_VALIDATION",
                "STATUTS_VALIDATION_POSITIONNEE", "STATUTS_IMPORTANCE", "STATUTS_HOMOGENEITE", "HYPOTHESE_HOMOGENEITE")
statuts_359 <- function() lapply(stats::setNames(nm = TABLES_359), function(n) {
  v <- get(n, envir = asNamespace("tablexp"))
  enc2utf8(if (is.data.frame(v)) v$statut else v)
})
natures_359 <- function(tables) {
  f <- tablexp:::mention_independance
  fixes <- c("tests", "intervalle", "credibilite", "valeur_h0", "niveau", "ecart_minimal", "brut_intervalle",
             "brut_variance", "contraste_segments")
  sorties <- stats::setNames(enc2utf8(vapply(fixes, f, "")), fixes)
  p <- enc2utf8(PREFIXE_359)
  lapply(tables, function(st) unlist(lapply(st, function(x) {
    morceaux <- strsplit(x, p, fixed = TRUE)[[1L]]
    if (endsWith(x, p)) morceaux <- c(morceaux, "")
    vapply(morceaux[-1L], function(m) {
      occ <- paste0(p, m)
      cand <- sorties
      v <- regmatches(m, regexec("^, les d\u00e9c\u00e8s sont sur-dispers\u00e9s et (\\S+) est sous-estim\u00e9e", m))[[1L]]
      if (length(v) == 2L) cand <- c(cand, variance = enc2utf8(f("variance", v[2L])))
      ok <- vapply(cand, function(s) startsWith(occ, s) &&
                     (nchar(occ) == nchar(s) || substring(occ, nchar(s) + 1L, nchar(s) + 2L) %in% c(" ;", " :")), NA)
      if (sum(ok) == 1L) names(cand)[ok] else "non_conforme"
    }, "", USE.NAMES = FALSE)
  })))
}
DECOMPTES_359 <- list(
  STATUTS_BRUT = c(brut_intervalle = 7L, brut_variance = 1L),
  STATUTS_LISSAGE_POISSON = c(credibilite = 2L),
  STATUTS_POSITIONNEMENT = c(intervalle = 8L),
  STATUTS_VALIDATION = c(intervalle = 3L, niveau = 3L, tests = 10L, valeur_h0 = 5L, variance = 1L),
  STATUTS_VALIDATION_POSITIONNEE = c(intervalle = 1L, niveau = 3L, tests = 6L, valeur_h0 = 6L),
  STATUTS_IMPORTANCE = c(intervalle = 3L, variance = 1L),
  STATUTS_HOMOGENEITE = c(contraste_segments = 1L, ecart_minimal = 2L, tests = 3L),
  HYPOTHESE_HOMOGENEITE = c(tests = 1L))
ecart_natures_359 <- function(tables) {
  n <- natures_359(tables)
  e <- unlist(lapply(names(n), function(t) {
    obt <- table(n[[t]]); obt <- stats::setNames(as.integer(obt), names(obt))
    if (identical(obt, DECOMPTES_359[[t]])) NULL
    else sprintf("%s : %s", t, paste(names(obt), obt, sep = " ", collapse = ", "))
  }))
  if (length(e)) paste(e, collapse = " ; ") else TRUE
}
verifier("#359 C1-b contr\u00f4le \u00e0 l'ex\u00e9cution (litt\u00e9ral de la sp\u00e9cification pour les d\u00e9comptes) : dans les huit tables (STATUTS_BRUT, STATUTS_LISSAGE_POISSON, STATUTS_POSITIONNEMENT, STATUTS_VALIDATION, STATUTS_VALIDATION_POSITIONNEE, STATUTS_IMPORTANCE, STATUTS_HOMOGENEITE, HYPOTHESE_HOMOGENEITE), toute occurrence du pr\u00e9fixe est une sortie exacte de mention_independance() ; d\u00e9comptes par nature (validation tests 10, intervalle 3 ; positionn\u00e9e tests 6, intervalle 1 ; importance intervalle 3 ; lissage credibilite 2 (#359, D-b (a)) ; positionnement 8 ; brut 7 + 1, Kaplan-Meier compris ; homog\u00e9n\u00e9it\u00e9 tests 3 + l'hypoth\u00e8se, contraste_segments 1 (segments/z ; d\u00e9cision du mainteneur du 25/11/2026), 2 ecart_minimal ; avis d'actuary sur #359, Q1 et Q2)",
         ecart_natures_359(statuts_359()))
verifier("#359 C1-b t\u00e9moin (valeur calcul\u00e9e \u00e0 la main) : copie dont une borne du positionnement reprend l'ancien texte non qualifi\u00e9 (\u00ab l'intervalle est trop \u00e9troit \u00bb, suivi de la fin du statut), puis copie dont l'hypoth\u00e8se de l'homog\u00e9n\u00e9it\u00e9 reprend celui d'INDEPENDANCE_HOMOGENEITE -> \u00e9cart signal\u00e9 sur la table", {
  t1 <- statuts_359(); i <- which(grepl("trop \u00e9troit \u00e0 attendus grands", t1$STATUTS_POSITIONNEMENT, fixed = TRUE))[1L]
  t1$STATUTS_POSITIONNEMENT[i] <- sub(" \u00e0 attendus grands.*$", "", t1$STATUTS_POSITIONNEMENT[i])
  t2 <- statuts_359()
  t2$HYPOTHESE_HOMOGENEITE <- sub("les tests sont anti-conservateurs.*$",
                                  "le test est anti-conservateur (p-valeurs trop petites)", t2$HYPOTHESE_HOMOGENEITE)
  e1 <- ecart_natures_359(t1); e2 <- ecart_natures_359(t2)
  if (identical(e1, "STATUTS_POSITIONNEMENT : intervalle 7, non_conforme 1") &&
      identical(e2, "HYPOTHESE_HOMOGENEITE : non_conforme 1")) TRUE
  else paste("obtenu :", format(e1), "|", format(e2))
})
# Témoins d'altération (audit de #159, constat mineur) : sur des COPIES des
# tables (l'espace de noms installé n'est jamais modifié), les contrôles
# ci-dessus renvoient un écart.
verifier("#159 C3 témoin (valeur calculée à la main) : copie de STATUTS_BRUT dont la ligne 1 (estimation ponctuelle) reçoit le marqueur -> écart signalé", {
  d <- tablexp:::STATUTS_BRUT
  d$statut[1L] <- paste(d$statut[1L], ";", MARQUEURS_159[1L])
  e <- ecart_brut_159(d)
  if (is.character(e) && startsWith(e, "décomptes : 1 1 1")) TRUE else paste("écart non signalé :", format(e))
})
verifier("#159 C3 témoin (valeur calculée à la main) : copie de STATUTS_VALIDATION dont smr/p_centrale perd sa mention, puis copie dont deviance/ddl la reçoit -> écart signalé sur la ligne", {
  d <- tablexp:::STATUTS_VALIDATION; k <- cle_159(d)
  retrait <- d; i <- which(k == "smr/p_centrale")
  retrait$statut[i] <- sub(paste(" ;", M_TESTS_159), "", enc2utf8(retrait$statut[i]), fixed = TRUE)
  ajout <- d; j <- which(k == "deviance/ddl")
  ajout$statut[j] <- paste(ajout$statut[j], ";", M_TESTS_159)
  e1 <- ecart_159(retrait, LOI_V_330); e2 <- ecart_159(ajout, LOI_V_330)
  if (identical(e1, "smr/p_centrale : 0 au lieu de 1") && identical(e2, "deviance/ddl : 1 au lieu de 0")) TRUE
  else paste("obtenu :", format(e1), "|", format(e2))
})
verifier("#159 C3 témoin (valeur calculée à la main) : copie de STATUTS_VALIDATION_POSITIONNEE dont une ligne porte la mention deux fois -> décompte par ligne (compter_159) de 2, que la garde générale (au plus 1) refuserait", {
  d <- tablexp:::STATUTS_VALIDATION_POSITIONNEE; i <- which(cle_159(d) == "deviance/p_monte_carlo")
  d$statut[i] <- paste(d$statut[i], ";", M_TESTS_159)
  if (identical(max(compter_159(d$statut)), 2L)) TRUE else "doublon non compté"
})
verifier("#330 C3 t\u00e9moin (valeur calcul\u00e9e \u00e0 la main) : copie de STATUTS_VALIDATION_POSITIONNEE dont sous_groupes/bornes_exactes reprend la cons\u00e9quence des tests (d\u00e9faut de #330) -> ligne signal\u00e9e par sans_mention_330(), d\u00e9compte du marqueur inchang\u00e9", {
  d <- tablexp:::STATUTS_VALIDATION_POSITIONNEE; i <- which(cle_159(d) == "sous_groupes/bornes_exactes")
  d$statut[i] <- sub(mention_330("intervalle"), mention_330("tests"), enc2utf8(d$statut[i]), fixed = TRUE)
  m <- sans_mention_330(d, NATURES_P_330)
  if (identical(m, "sous_groupes/bornes_exactes") && isTRUE(ecart_159(d, unlist(NATURES_P_330, use.names = FALSE)))) TRUE
  else paste("obtenu :", paste(m, collapse = ", "))
})
verifier("#329 C3 t\u00e9moin (valeur calcul\u00e9e \u00e0 la main) : copie de STATUTS_LISSAGE_POISSON dont borne_inf_credibilite perd sa mention, puis copie dont segments/parametre_lissage la re\u00e7oit -> \u00e9cart signal\u00e9 sur la ligne (cl\u00e9 grandeur)", {
  d <- tablexp:::STATUTS_LISSAGE_POISSON
  retrait <- d; i <- which(d$grandeur == BORNES_329[1L])
  retrait$statut[i] <- sub(paste(" ;", mention_330("credibilite")), "", enc2utf8(retrait$statut[i]), fixed = TRUE)
  ajout <- d; j <- which(d$grandeur == "segments/parametre_lissage")
  ajout$statut[j] <- paste(ajout$statut[j], ";", mention_330("credibilite"))
  e1 <- ecart_159(retrait, BORNES_329, retrait$grandeur); e2 <- ecart_159(ajout, BORNES_329, ajout$grandeur)
  if (identical(e1, "table_lissee/borne_inf_credibilite : 0 au lieu de 1") &&
      identical(e2, "segments/parametre_lissage : 1 au lieu de 0")) TRUE
  else paste("obtenu :", format(e1), "|", format(e2))
})
