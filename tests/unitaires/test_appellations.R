# -----------------------------------------------------------------------------
#  tests/unitaires/test_appellations.R : appellations des intervalles et
#  condition de la loi binomiale (issue #31 ; spécification d'actuary-
#  approfondi, #31, commentaire 6689740204 ; M50 Q4, repli (b) ; M55, C2) ;
#  fichier propre à B13 (ADR 0013, point 3)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - littéral de la spécification : textes de STATUTS_BRUT, de
#    STATUTS_IMPORTANCE et de la mention brut_binomial, écrits en dur
#    d'après le commentaire de spécification et la décision du mainteneur du
#    06/11/2026 ;
#  - identité exacte : statuts de l'étape brut et de la fiche, de bout en
#    bout, égaux à STATUTS_BRUT.
# -----------------------------------------------------------------------------

D <- as.Date
STATUTS_BRUT <- tablexp:::STATUTS_BRUT
HYPOTHESES_BRUT <- tablexp:::HYPOTHESES_BRUT
STATUTS_IMPORTANCE <- tablexp:::STATUTS_IMPORTANCE
STATUTS_VALIDATION <- tablexp:::STATUTS_VALIDATION
MENTIONS_RAPPORT <- tablexp:::MENTIONS_RAPPORT

ligne_brut <- function(estimateur, grandeur, intervalle) {
  k <- which(STATUTS_BRUT$estimateur == estimateur & STATUTS_BRUT$grandeur == grandeur &
               STATUTS_BRUT$intervalle == intervalle)
  if (length(k) == 1L) STATUTS_BRUT[k, , drop = FALSE] else NULL
}
egal <- function(obtenu, attendu)
  if (identical(obtenu, attendu)) TRUE else sprintf("obtenu : %s", paste(obtenu, collapse = " | "))

# --- 1. littéraux de STATUTS_BRUT ----------------------------------------------

Z_TEXTE <- "z = qnorm(alpha/2, lower.tail = FALSE)"
verifier("STATUTS_BRUT (littéral de la spécification) : statut du binomial exact (ligne 8), condition « cellule observée entière »", {
  l <- ligne_brut("binomial", "taux", "exact")
  if (!identical(rownames(l), "8")) "ligne 8 absente" else
    (egal(l$statut, paste(
      "exact sous la loi supposée pour une cellule observée entière (sans entrée ni censure, E^i entier) ;",
      "avec une entrée ou une censure dans la cellule, couverture non établie, même si E^i est entier",
      "(prolongement continu de la formule exacte si E^i n'est pas entier)")))
})
verifier("STATUTS_BRUT (littéral de la spécification) : statut du binomial asymptotique (ligne 9), condition « cellule observée entière »", {
  l <- ligne_brut("binomial", "taux", "asymptotique")
  if (!identical(rownames(l), "9")) "ligne 9 absente" else
    (egal(l$statut, paste(
      "asymptotique (E^i q(1-q) -> infini) pour une cellule observée entière ; avec une entrée ou une",
      "censure dans la cellule, couverture non établie ; non défini à D = 0 et à D = E^i")))
})
verifier("STATUTS_BRUT (littéral de la spécification) : calcul du Poisson asymptotique (ligne 3), « normal sur l'échelle de log mu »", {
  l <- ligne_brut("poisson", "taux", "asymptotique")
  if (!identical(rownames(l), "3")) "ligne 3 absente" else
    (egal(l$calcul, paste("taux exp(-+ z / sqrt(D)) : normal sur l'échelle de log mu ;", Z_TEXTE)))
})
verifier("STATUTS_BRUT (littéral de la spécification) : calcul du binomial asymptotique (ligne 9), « normal sur l'échelle de logit q »", {
  l <- ligne_brut("binomial", "taux", "asymptotique")
  (egal(l$calcul, paste("plogis(qlogis(q) -+ z sqrt(1/D + 1/(E^i - D))) : normal sur l'échelle de logit q ;",
                              Z_TEXTE)))
})
verifier("STATUTS_BRUT (littéral de la spécification) : calcul du Kaplan-Meier asymptotique (ligne 12), « normal sur l'échelle de logit q »", {
  l <- ligne_brut("kaplan_meier", "taux", "asymptotique")
  if (!identical(rownames(l), "12")) "ligne 12 absente" else
    (egal(l$calcul, paste("plogis(qlogis(q) -+ z sqrt(Sigma) / q) : normal sur l'échelle de logit q,",
                                "Var(logit q) ~ Sigma / q^2 ;", Z_TEXTE)))
})

# --- 2. littéraux de STATUTS_IMPORTANCE et de la mention brut_binomial ----------

statut_importance <- function(table, grandeur)
  STATUTS_IMPORTANCE$statut[STATUTS_IMPORTANCE$table == table & STATUTS_IMPORTANCE$grandeur == grandeur]
verifier("STATUTS_IMPORTANCE (littéral de la spécification) : smr_comparateur_1, bornes_asymptotiques, « normal sur l'échelle de log(SMR) »",
         egal(statut_importance("smr_comparateur_1", "bornes_asymptotiques"),
              "asymptotique : normal sur l'échelle de log(SMR), variance 1 / D_e ; non défini à D_e = 0"))
verifier("STATUTS_IMPORTANCE (littéral de la spécification) : smr_comparateur_2, bornes, exactes et asymptotiques distinguées",
         egal(statut_importance("smr_comparateur_2", "bornes"), paste(
           "relation independante : bornes exactes, exact sous D_e ~ Poisson(theta A), A connu (table, E^c tenue",
           "pour fixe), qgamma, approximation numérique ; bornes asymptotiques, asymptotique (normal sur l'échelle",
           "de log(SMR), variance 1 / D_e ; non défini à D_e = 0) ; relation ajustee_hors_outil : valeurs",
           "ponctuelles seules, bornes non produites (ddl_non_etabli)")))
verifier("MENTIONS_RAPPORT (littéral de la spécification) : brut_binomial couvre les deux intervalles, condition « cellule observée entière »",
         egal(MENTIONS_RAPPORT[["brut_binomial"]], paste(
           "Intervalles binomiaux (colonnes borne_inf_exacte, borne_sup_exacte, borne_inf_asymptotique et",
           "borne_sup_asymptotique de etapes/brut/resultat/binomial) : les deux formules supposent D ~ Bin(E^i, q),",
           "ce qui ne vaut que pour une cellule observée entière, sans entrée ni censure dans la cellule (E^i est",
           "alors le nombre d'unités d'observation, entier). Avec une entrée ou une censure dans la cellule, D ne",
           "suit pas Bin(E^i, q), même si E^i est entier : la couverture des deux intervalles n'est pas établie ;",
           "si E^i n'est pas entier, la formule par quantiles de lois bêta en est en outre le prolongement continu.")))

# --- 3. aucune appellation « Wald » (M50 Q4, repli (b)) -------------------------

textes <- function(x) if (is.list(x)) unlist(lapply(x, textes), use.names = FALSE) else as.character(x)
for (nom in c("STATUTS_BRUT", "HYPOTHESES_BRUT", "STATUTS_IMPORTANCE", "STATUTS_VALIDATION")) {
  t <- textes(get(nom))
  verifier(sprintf("%s (littéral de la spécification) : aucun texte ne contient « Wald », casse ignorée (%d textes)",
                   nom, length(t)),
           if (length(t) && !any(grepl("wald", t, ignore.case = TRUE))) TRUE else
             paste(grep("wald", t, ignore.case = TRUE, value = TRUE), collapse = " | "))
}

# --- 4. de bout en bout : statuts de l'étape et de la fiche ---------------------

DA <- data.frame(
  n = D(c("1960-07-01", "1945-10-15", "1950-01-10", "1940-12-05", "1955-06-30", "1964-02-29")),
  e = D(c("2020-03-01", "2016-05-20", "2010-06-01", "2022-01-01", "2023-01-01", "2021-01-01")),
  s = D(c(NA, "2019-02-10", NA, "2024-11-15", "2025-02-01", NA)),
  c = c("en_cours", "deces", "en_cours", "deces", "deces", "en_cours"),
  t = 1:6, stringsAsFactors = FALSE)
ROLES_A <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", id_tete = "t")
PLAN_A <- list(transition = list(depart = "vivant", arrivee = "deces"),
               periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0,
               segmentation = character(0),
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE),
                             brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95)))
XA <- construire_table(DA, ROLES_A, PLAN_A)
verifier("de bout en bout (identité exacte) : statuts de l'étape brut et de la fiche identical(), égaux à STATUTS_BRUT (trois estimateurs)", {
  sp <- XA$etapes$brut$parametres$statuts
  sf <- XA$fiche$hypotheses_brut$statuts
  attendu <- STATUTS_BRUT; rownames(attendu) <- NULL
  if (identical(sp, sf) && identical(sp, attendu)) TRUE else "statuts différents"
})
verifier("de bout en bout (identité exacte) : textes des lignes 3, 8, 9 et 12 restitués tels quels dans la fiche", {
  sf <- XA$fiche$hypotheses_brut$statuts
  identical(sf$statut[8:9], STATUTS_BRUT$statut[8:9]) && identical(sf$calcul[c(3L, 9L, 12L)], STATUTS_BRUT$calcul[c(3L, 9L, 12L)]) &&
    !any(grepl("wald", textes(XA$fiche$hypotheses_brut), ignore.case = TRUE))
})
