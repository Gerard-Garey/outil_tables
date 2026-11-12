# -----------------------------------------------------------------------------
#  tests/mesures/constat_couverture_lissage.R : constat de simulation hors CI
#  de la couverture des intervalles de crédibilité de Whittaker-Henderson (b)
#  (étape `lissage`, méthode wh_poisson) à mu vrai (issue #225, constat C2,
#  décision D6 du mainteneur du 11/11/2026, M413 ; spécification de forme
#  (0')(b) d'actuary-approfondi, § 7 ; corps de #225, point D)
#
#  Règle fixée avant de lancer (spécification, § 7) : R = 1 000 jeux par loi
#  vraie ; z = 3, lambda choisi par la LAML sur [1, 1e12], niveau 0,95 ;
#  plage d'âges 20 à 90. Lois vraies (décès), sur q_x, facteurs H 1 et F 0,6
#  (comme l'entrée de référence de R1, tests/reference/outils_reference.R) :
#   - Gompertz-Makeham A = 2e-4, B = 2,5e-5, c = 1,1 (k = 5) ;
#   - logistique a = 2e-4, b = 2,5e-5, c = ln 1,1 (k = 6 ; #211, loi
#     disponible depuis la fusion de [P4]) : sous Gompertz-Makeham et z = 2,
#     l'espace nul de la pénalité contient ln mu vrai à la constante de
#     Makeham près (constat favorable par construction) ; la loi logistique
#     ne l'est pas.
#  Portefeuille : celui de l'entrée de référence (mode uniforme, n = 1e5
#  têtes, moitié d'hommes, âge à l'entrée 20 à 75, origine 2005-01-01 ;
#  rachat constant 5 %), période 2015-2024 ; plan en lignes, délai 0,
#  segmentation par sexe, convention plus_fine (retenue
#  dernier_anniversaire), sans année, estimateur poisson au niveau 0,95.
#  Grandeurs, par loi, sexe et âge : couverture de mu vrai par l'intervalle
#  [borne_inf_credibilite, borne_sup_credibilite] (sur q, transformation
#  croissante de mu : q vrai couvert si et seulement si mu vrai l'est),
#  fréquence sur les jeux produits, séparée selon l'état du segment
#  (produite / lambda_a_la_borne) ; erreur de Monte-Carlo sqrt(p (1 - p) /
#  R_etat), R_etat nombre de jeux du sexe dans l'état (une cellule par jeu) ;
#  couverture moyenne sur les cellules où E^c mu vrai >= 5 (E^c du jeu),
#  par sexe et état : moyenne par jeu sur ses cellules, puis moyenne entre
#  jeux, erreur de Monte-Carlo = écart type entre jeux / sqrt(nombre de jeux)
#  (les cellules d'un même jeu ne sont pas indépendantes). Décomptes : jeux
#  refusés (motif) et segments à l'état lambda_a_la_borne. Aucune règle de verdict sur la
#  table (constat de simulation ; les intervalles sont bayésiens, la
#  couverture fréquentiste n'est pas une propriété revendiquée).
#  Statuts : couvertures : constats de simulation (erreur de Monte-Carlo,
#  fonction de R) ; l'erreur d'estimation de chaque jeu (exposition) n'est
#  pas mesurée ici.
#
#  Graines : jeu r de la loi k sous 22011200 + 2500 k + r, 1 <= r <= R
#  (Gompertz-Makeham : 22023700 + r ; logistique : 22026200 + r), plage de la
#  porteuse B19 déclarée dans tests/controles/graines_admises.csv (M416).
#  Aucune autre source d'aléa (étapes du plan non aléatoires).
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/constat_couverture_lissage.R [jeux=1000]
#  Clé : jeux (R par loi, défaut 1000 ; e.t. de Monte-Carlo 0,0069 à 0,95).
#  Durée : extrapolée après le premier jeu, durée réelle imprimée.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
installer_tablexp(RACINE)

ARGS <- commandArgs(trailingOnly = TRUE)
lire_cle <- function(nom, defaut) {
  a <- grep(paste0("^", nom, "="), ARGS, value = TRUE)
  if (length(a)) as.integer(sub(paste0("^", nom, "="), "", a[1L])) else defaut
}
R_JEUX <- lire_cle("jeux", 1000L)
if (is.na(R_JEUX) || R_JEUX < 1L || R_JEUX > 2499L) stop("jeux : entier de 1 à 2499 (plage de graines)")

GRAINE_BASE <- 22011200L
AGES_TABLE <- 0:120
LOIS <- list(
  gompertz_makeham = list(k = 5L, table = tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, AGES_TABLE)),
  logistique = list(k = 6L, table = tablexp:::table_logistique(2e-4, 2.5e-5, log(1.1), AGES_TABLE)))
FACTEURS <- c(H = 1, F = 0.6)
RACHAT <- tablexp:::table_constante(0.05, AGES_TABLE, "rachat constant 5 %")
PORTEFEUILLE <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
                     origine = as.Date("2005-01-01"))
PERIODE <- list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31"))
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
PLAGE <- c(20, 90)
PLAN <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PERIODE, delai_declaration = 0,
             segmentation = "sexe",
             etapes = list(donnees = list(exclusions = character(0)),
                           exposition = list(convention = "plus_fine", par_annee = FALSE),
                           brut = list(estimateurs = "poisson", niveau = 0.95),
                           lissage = list(methode = "wh_poisson", plage_ages = PLAGE, ordre_difference = 3,
                                          critere_lissage = "laml", niveau = 0.95, intervalle_lissage = c(1, 1e12))))

# Un jeu : cellules (sexe, âge) de la plage avec E^c, mu vrai et indicatrice
# de couverture, ou le motif du refus.
un_jeu <- function(loi, r) {
  graine <- GRAINE_BASE + 2500L * loi$k + r
  d <- tablexp::simuler_portefeuille(list(deces = list(table = loi$table, facteurs = FACTEURS),
                                          rachat = list(table = RACHAT, facteurs = c(H = 1, F = 1))),
                                     PORTEFEUILLE, PERIODE, graine)
  x <- tryCatch(construire_table(d, ROLES, PLAN), refus_tablexp = function(e) e)
  if (inherits(x, "refus_tablexp")) return(list(refus = x$motif))
  t <- x$etapes$lissage$resultat$table_lissee
  ex <- x$etapes$exposition$resultat$exposition
  # q vrai de la cellule : règle du simulateur (loi_effective(), R/simulation.R) :
  # q = min(facteur x q_table, 1), force constante par âge entier.
  q_vrai <- unname(pmin(FACTEURS[t$sexe] * loi$table$qx[match(t$age, loi$table$age)], 1))
  ec <- ex$exposition_centrale[match(paste(t$sexe, t$age), paste(ex$sexe, ex$age))]
  ec[is.na(ec)] <- 0
  s <- x$etapes$lissage$resultat$segments
  list(cellules = data.frame(jeu = r, sexe = t$sexe, age = t$age, ec = ec, mu_vrai = -log1p(-q_vrai),
                             couvert = t$borne_inf_credibilite <= q_vrai & q_vrai <= t$borne_sup_credibilite,
                             etat = s$etat[match(t$sexe, s$sexe)], stringsAsFactors = FALSE),
       bornes = sum(x$etapes$lissage$resultat$segments$etat == "lambda_a_la_borne"))
}

for (nom in names(LOIS)) {
  loi <- LOIS[[nom]]
  t0 <- proc.time()[["elapsed"]]
  cellules <- list(); refus <- character(0); bornes <- 0L
  for (r in seq_len(R_JEUX)) {
    j <- un_jeu(loi, r)
    if (!is.null(j$refus)) refus <- c(refus, j$refus)
    else { cellules[[length(cellules) + 1L]] <- j$cellules; bornes <- bornes + j$bornes }
    if (r == 1L) cat(sprintf("%s : premier jeu %.1f s, durée extrapolée %.1f min\n", nom,
                             proc.time()[["elapsed"]] - t0, (proc.time()[["elapsed"]] - t0) * R_JEUX / 60))
  }
  cc <- do.call(rbind, cellules)
  n_prod <- length(cellules)
  cat(sprintf("\n== Loi %s : %d jeu(x), %d produit(s), %d refusé(s)%s ; %d segment(s) à l'état lambda_a_la_borne ; durée %.1f min\n",
              nom, R_JEUX, n_prod, length(refus),
              if (length(refus)) sprintf(" (%s)", paste(names(table(refus)), table(refus), sep = " : ", collapse = ", ")) else "",
              bornes, (proc.time()[["elapsed"]] - t0) / 60))
  if (!n_prod) next
  agg <- stats::aggregate(cbind(couvert, attendu = ec * mu_vrai, jeux = 1) ~ etat + sexe + age, data = cc,
                          FUN = sum)
  agg$couvert <- agg$couvert / agg$jeux
  agg$attendu <- agg$attendu / agg$jeux
  agg$erreur_mc <- sqrt(agg$couvert * (1 - agg$couvert) / agg$jeux)
  agg <- agg[order(agg$etat, agg$sexe, agg$age), , drop = FALSE]
  cat("état sexe âge jeux couverture erreur_MC E^c mu (moyenne)\n")
  for (i in seq_len(nrow(agg)))
    cat(sprintf("%s %s %d %d %.4f %.4f %.1f\n", agg$etat[i], agg$sexe[i], agg$age[i], agg$jeux[i], agg$couvert[i],
                agg$erreur_mc[i], agg$attendu[i]))
  for (e in sort(unique(cc$etat))) for (s in c("H", "F")) {
    k <- cc$etat == e & cc$sexe == s & cc$ec * cc$mu_vrai >= 5
    if (!any(k)) next
    par_jeu <- tapply(cc$couvert[k], cc$jeu[k], mean)
    cat(sprintf(paste("État %s, sexe %s : couverture moyenne sur les cellules où E^c mu >= 5 : %.4f (%d jeux, %d",
                      "cellules-jeux ; erreur de Monte-Carlo %s, écart type entre jeux des moyennes par jeu / sqrt(jeux))\n"),
                e, s, mean(par_jeu), length(par_jeu), sum(k),
                if (length(par_jeu) > 1L) sprintf("%.4f", stats::sd(par_jeu) / sqrt(length(par_jeu))) else "non définie"))
  }
}
bilan(code_sortie = FALSE)
