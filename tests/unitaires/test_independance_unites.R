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
#  unités d'observation » (ligne 12 de STATUTS_BRUT), compté par ligne sur
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
# ailleurs ; TRUE ou le texte de l'écart.
ecart_159 <- function(d, avec) {
  n <- compter_159(d$statut); k <- cle_159(d)
  inconnues <- setdiff(avec, k)
  if (length(inconnues)) return(paste("clés absentes :", paste(inconnues, collapse = ", ")))
  att <- as.integer(k %in% avec)
  if (identical(n, att)) TRUE
  else paste(sprintf("%s : %d au lieu de %d", k[n != att], n[n != att], att[n != att]), collapse = " ; ")
}

TESTS_159 <- c(
  "residus/esperance_au_dela_2", "residus/esperance_au_dela_3",
  "khi2_pearson/variance_h0", "khi2_pearson/p_asymptotique", "khi2_pearson/p_monte_carlo",
  "deviance/p_asymptotique", "deviance/p_monte_carlo",
  "smr/p_inferieure", "smr/p_superieure", "smr/p_centrale", "smr/z_racine_cubique", "smr/p_racine_cubique",
  "signes/esperance_positifs_h0", "signes/p_monte_carlo", "runs/p_monte_carlo", "wilcoxon/p_monte_carlo",
  "fluctuation/esperance_hors_intervalle_exact", "fluctuation/esperance_hors_intervalle_asymptotique",
  "sous_groupes/p_valeurs")
INTERVALLES_159 <- c("smr/borne_inf_exacte", "smr/borne_sup_exacte", "sous_groupes/bornes_exactes")
SMR_NON_PRODUITS_159 <- c("smr/borne_inf_exacte", "smr/borne_sup_exacte", "smr/p_inferieure", "smr/p_superieure",
                          "smr/p_centrale", "smr/z_racine_cubique", "smr/p_racine_cubique")
M_TESTS_159 <- paste("unités d'observation indépendantes ; sous l'unité contrat, les décès",
                     "sont sur-dispersés et les tests sont anti-conservateurs")
M_INTERVALLE_159 <- paste("unités d'observation indépendantes ; sous l'unité contrat, les décès",
                          "sont sur-dispersés et l'intervalle est trop étroit")

# Écart de la table `d` (forme de STATUTS_BRUT) à l'attendu par position :
# 1 sur les lignes 2, 3, 5, 6, 8, 9, 11 et 12, 0 ailleurs ; TRUE ou le texte.
ecart_brut_159 <- function(d) {
  n <- compter_159(d$statut)
  att <- as.integer(seq_len(nrow(d)) %in% c(2L, 3L, 5L, 6L, 8L, 9L, 11L, 12L))
  if (identical(n, att)) TRUE else paste("décomptes :", paste(n, collapse = " "))
}
verifier("#159 C3 STATUTS_BRUT (littéral de la spécification) : marqueur une fois sur les lignes 2, 3, 5, 6, 8, 9, 11 et 12, jamais ailleurs (estimations ponctuelles 1, 4, 7, 10 ; ligne 13)",
         if (nrow(tablexp:::STATUTS_BRUT) != 13L) "STATUTS_BRUT : 13 lignes attendues" else ecart_brut_159(tablexp:::STATUTS_BRUT))
verifier("#159 C3 STATUTS_BRUT (littéral de la spécification) : mention en fin de statut (intervalles 2, 3, 5, 6, 8, 9 ; variance 11) ; loi_supposee sans marqueur ; ligne 12 inchangée", {
  s <- tablexp:::STATUTS_BRUT
  m_int <- paste("; unités d'observation indépendantes ; sous l'unité contrat, les contrats d'une",
                 "même tête sont supposés indépendants (une dépendance positive rend",
                 "l'intervalle trop étroit ; relevé tetes_multi_contrats de l'étape donnees)")
  m_var <- sub("l'intervalle trop étroit", "la variance trop faible", m_int, fixed = TRUE)
  fin <- function(x, m) endsWith(enc2utf8(x), m)
  tous <- c(int = all(fin(s$statut[c(2L, 3L, 5L, 6L, 8L, 9L)], m_int)), var = fin(s$statut[11L], m_var),
            loi = all(compter_159(s$loi_supposee) == 0L),
            km = endsWith(enc2utf8(s$statut[12L]), "relevé tetes_multi_contrats de l'étape donnees)") &&
                 !fin(s$statut[12L], m_int))
  if (all(tous)) TRUE else paste("échec :", paste(names(tous)[!tous], collapse = ", "))
})
verifier("#159 C3 STATUTS_VALIDATION (littéral de la spécification) : marqueur une fois sur les 22 lignes de loi, jamais ailleurs ; texte des tests ou de l'intervalle selon la ligne",
  {
    s <- tablexp:::STATUTS_VALIDATION; k <- cle_159(s); st <- enc2utf8(s$statut)
    e <- ecart_159(s, c(TESTS_159, INTERVALLES_159))
    if (!isTRUE(e)) e
    else if (!all(grepl(M_TESTS_159, st[k %in% TESTS_159], fixed = TRUE)) ||
             !all(grepl(M_INTERVALLE_159, st[k %in% INTERVALLES_159], fixed = TRUE))) "texte de la mention"
    else if (!all(grepl(paste(M_TESTS_159, "; relation ajustee_hors_outil"), st[k == "sous_groupes/p_valeurs"], fixed = TRUE)) ||
             !grepl(paste(M_INTERVALLE_159, "; relation ajustee_hors_outil"), st[k == "sous_groupes/bornes_exactes"], fixed = TRUE))
      "sous_groupes : mention avant la relation ajustee_hors_outil"
    else TRUE
  })
verifier("#159 C3 STATUTS_VALIDATION_POSITIONNEE (littéral de la spécification) : marqueur une fois sur les 16 lignes (22 lignes de loi moins les 7 lignes smr non produites, plus sous_groupes/p_valeurs_asymptotiques), jamais ailleurs (aucun doublon)",
         ecart_159(tablexp:::STATUTS_VALIDATION_POSITIONNEE,
                   c(setdiff(c(TESTS_159, INTERVALLES_159), SMR_NON_PRODUITS_159), "sous_groupes/p_valeurs_asymptotiques")))
verifier("#159 C3 STATUTS_VALIDATION_LISSEE (littéral de la spécification) : marqueur sur aucune ligne (lissage hors périmètre, #329)",
         ecart_159(tablexp:::STATUTS_VALIDATION_LISSEE, character(0)))
verifier("#159 C3 STATUTS_IMPORTANCE (littéral de la spécification) : marqueur une fois sur les 4 lignes de loi (bornes du comparateur 1, rapport_variance, bornes du comparateur 2), jamais ailleurs", {
  s <- tablexp:::STATUTS_IMPORTANCE; k <- cle_159(s); st <- enc2utf8(s$statut)
  e <- ecart_159(s, c("smr_comparateur_1/bornes_exactes", "smr_comparateur_1/bornes_asymptotiques",
                      "smr_comparateur_1/rapport_variance", "smr_comparateur_2/bornes"))
  if (!isTRUE(e)) e
  else if (!all(endsWith(st[k %in% c("smr_comparateur_1/bornes_exactes", "smr_comparateur_1/bornes_asymptotiques")],
                         paste(";", M_INTERVALLE_159))) ||
           !endsWith(st[k == "smr_comparateur_1/rapport_variance"],
                     paste("; unités d'observation indépendantes ; sous l'unité contrat, les décès",
                           "sont sur-dispersés et Var(A^) est sous-estimée")) ||
           !grepl(paste(M_INTERVALLE_159, "; relation ajustee_hors_outil"), st[k == "smr_comparateur_2/bornes"], fixed = TRUE))
    "texte ou place de la mention"
  else TRUE
})
TABLES_159 <- c("STATUTS_BRUT", "STATUTS_VALIDATION", "STATUTS_VALIDATION_POSITIONNEE", "STATUTS_VALIDATION_LISSEE",
                "STATUTS_IMPORTANCE", "STATUTS_POSITIONNEMENT", "STATUTS_HOMOGENEITE")
verifier("#159 C3 garde générale (littéral de la spécification) : marqueur au plus une fois sur toute ligne des sept tables de statuts, θ_80 de l'homogénéité compris", {
  m <- vapply(TABLES_159, function(n) max(compter_159(get(n, envir = asNamespace("tablexp"))$statut)), 0L)
  if (all(m <= 1L)) TRUE else paste(sprintf("%s : %d", names(m)[m > 1L], m[m > 1L]), collapse = " ; ")
})
verifier("#159 C3 sous LC_ALL=C (identité exacte) : décomptes du marqueur par ligne des sept tables égaux à ceux de la session (sous-processus)", {
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
  e1 <- ecart_159(retrait, c(TESTS_159, INTERVALLES_159)); e2 <- ecart_159(ajout, c(TESTS_159, INTERVALLES_159))
  if (identical(e1, "smr/p_centrale : 0 au lieu de 1") && identical(e2, "deviance/ddl : 1 au lieu de 0")) TRUE
  else paste("obtenu :", format(e1), "|", format(e2))
})
verifier("#159 C3 témoin (valeur calculée à la main) : copie de STATUTS_VALIDATION_POSITIONNEE dont une ligne porte la mention deux fois -> garde générale en échec (2)", {
  d <- tablexp:::STATUTS_VALIDATION_POSITIONNEE; i <- which(cle_159(d) == "deviance/p_monte_carlo")
  d$statut[i] <- paste(d$statut[i], ";", M_TESTS_159)
  if (identical(max(compter_159(d$statut)), 2L)) TRUE else "doublon non compté"
})
