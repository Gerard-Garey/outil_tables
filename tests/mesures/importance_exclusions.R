# -----------------------------------------------------------------------------
#  tests/mesures/importance_exclusions.R : étape `importance_exclusions`
#  (issue #69), mesures hors des batteries (aucun seuil)
#
#  Deux mesures, chacune consignée dans le commit qui la cite :
#  - duree (M185) : durée de construire_table() à la taille de R6
#    (jeu_reference() : loi, portefeuille de 10^5 contrats, période, graine ;
#    plan R6) sur un jeu à exclusions informatives (#93 : sexe_manquant sur la
#    moitié des lignes à décès et 2 % des autres, comme le jeu § 2.6 de
#    tests/unitaires/test_simulation.R), complété de motifs mesurables et non
#    mesurables pour avoir plusieurs ensembles S_k ; unité tête ; même plan
#    sans puis avec l'étape, REPETITIONS mesures chacune après gc(), médiane
#    et étendue ; puis le même portefeuille en mode uniforme_multi (têtes
#    multi-contrats, têtes touchées porteuses de lignes conservées) ;
#  - couverture (décision Q-b du 06/11/2026, #69) : constat de simulation de
#    la couverture de l'intervalle exact du SMR des lignes exclues sous le
#    comparateur 1, sous H0 (exclusion non informative), E^c tenue pour fixe :
#    les cellules (E^c_r, E^c_e) d'une construction sur un jeu à exclusion non
#    informative sont gardées ; mu_c est fixé au taux brut de Poisson du jeu
#    propre ; à chaque réplication, D_r,c ~ Poisson(E^c_r,c mu_c) et D_e,c ~
#    Poisson(E^c_e,c mu_c), puis smr_comparateur_1() du moteur ; couverture
#    de theta = 1, erreur de Monte-Carlo sqrt(p (1 - p) / B), rapport de
#    variance moyen et rapport théorique au mu_c vrai, E(D_e) ; deux
#    configurations, exclusion de 2 % puis de 10 % des lignes (f plus élevé ;
#    avis d'actuary, C3) ; version de R affichée. Statut : constat de
#    simulation (l'erreur de Monte-Carlo n'est pas une erreur d'estimation).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/mesures/importance_exclusions.R [duree] [couverture]
#  Sans argument : les deux.
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
source(file.path(RACINE, "tests", "reference", "outils_reference.R"))

ARGS <- commandArgs(trailingOnly = TRUE)
MESURES <- if (length(ARGS)) ARGS else c("duree", "couverture")
D <- as.Date
REPETITIONS <- 3L
GRAINE_DEFAUTS <- 20261025L
TOUTES <- c("deces", "rachat", "en_cours")

# Médiane, minimum et maximum de REPETITIONS mesures de f(), gc() avant chacune ;
# rend aussi la dernière valeur.
repetitions <- function(f) {
  r <- numeric(REPETITIONS)
  for (k in seq_len(REPETITIONS)) {
    invisible(gc())
    t0 <- proc.time()[["elapsed"]]
    v <- f()
    r[k] <- proc.time()[["elapsed"]] - t0
  }
  list(mediane = stats::median(r), min = min(r), max = max(r), valeur = v)
}
loi_reference <- function() {
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
}

if ("duree" %in% MESURES) {
  cat("\n== Durée (M185) : plan R6 sans et avec l'étape importance_exclusions\n")
  DECL <- list(
    list(type = "sexe_manquant", cible = "deces", taux = 0.5),
    list(type = "sexe_manquant", cible = c("rachat", "en_cours"), taux = 0.02),
    list(type = "id_contrat_manquant", cible = TOUTES, taux = 0.01),
    list(type = "naissance_manquante", cible = TOUTES, taux = 0.005),
    list(type = "cause_hors_modalites", cible = c("deces", "rachat"), taux = 0.005))
  EXC <- c("id_contrat_manquant", "naissance_manquante", "cause_hors_modalites", "sexe_manquant")
  p6 <- plans_reference()$R6$plan
  p6$etapes$donnees$exclusions <- EXC
  p6_ie <- p6
  p6_ie$etapes$importance_exclusions <- list(niveau = 0.95)
  for (mode in c("uniforme", "uniforme_multi")) {
    pf <- list(mode = mode, n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
    if (mode == "uniforme_multi") pf$loi_contrats <- c(0.6, 0.3, 0.1)
    j <- tablexp::simuler_portefeuille(loi_reference(), pf, PERIODE_REFERENCE, GRAINE_REFERENCE,
                                       defauts = list(graine = GRAINE_DEFAUTS, declarations = DECL))
    sans <- repetitions(function() construire_table(j, ROLES_REFERENCE, p6))
    avec <- repetitions(function() construire_table(j, ROLES_REFERENCE, p6_ie))
    x <- avec$valeur
    r <- x$etapes$importance_exclusions$resultat
    ta <- r$retraits[r$retraits$ensemble == "total" & r$retraits$niveau == "A", ]
    cat(sprintf(paste("  %s : %d lignes, %d exclues (%s) ; total, niveau A : %d lignes mesurables, %d têtes",
                      "touchées, %d indéterminées ; %d calculs contrefactuels\n"),
                mode, nrow(j), r$volumes$lignes_exclues,
                paste(sprintf("%s %d", r$motifs$ensemble, r$motifs$lignes), collapse = ", "),
                ta$lignes_mesurables, ta$tetes_touchees, ta$tetes_indeterminees,
                x$etapes$importance_exclusions$diagnostics$calculs_contrefactuels))
    cat(sprintf("  %s : construire_table() sans l'étape : médiane %.2f s [%.2f ; %.2f] ; avec : médiane %.2f s [%.2f ; %.2f] ; écart des médianes %.2f s\n",
                mode, sans$mediane, sans$min, sans$max, avec$mediane, avec$min, avec$max, avec$mediane - sans$mediane))
    s <- r$smr_comparateur_1[r$smr_comparateur_1$ensemble == "total", ]
    cat(sprintf("  %s : SMR des lignes exclues, comparateur 1, niveau %s : D_e = %.0f, A = %.3f, SMR = %.4f [%.4f ; %.4f]\n",
                mode, s$niveau, s$observes, s$attendus, s$smr, s$borne_inf_exacte, s$borne_sup_exacte), sep = "")
    verifier(sprintf("mesure de durée, %s, exécutée", mode), TRUE)
  }
}

if ("couverture" %in% MESURES) {
  cat("\n== Couverture (Q-b) : intervalle exact du SMR des lignes exclues, comparateur 1, sous H0\n")
  B <- 20000L
  GRAINE_COUVERTURE <- 20261105L
  PER <- list(debut = D("2015-01-01"), fin = D("2019-12-31"))
  pf <- list(mode = "uniforme", n = 20000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
  j0 <- tablexp::simuler_portefeuille(loi_reference(), pf, PER, GRAINE_REFERENCE)
  plan <- function(ex, ie) {
    p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER, delai_declaration = 0,
              segmentation = character(0),
              etapes = list(donnees = list(exclusions = ex),
                            exposition = list(convention = "plus_fine", par_annee = FALSE)))
    if (ie) p$etapes$importance_exclusions <- list(niveau = 0.95)
    p
  }
  x0 <- construire_table(j0, ROLES_REFERENCE, plan(character(0), FALSE))
  e0 <- x0$etapes$exposition$resultat$exposition
  mu_vrai <- ifelse(e0$exposition_centrale > 0, e0$evenements / e0$exposition_centrale, 0)
  # Deux configurations (avis d'actuary, C3) : exclusion non informative
  # sexe_manquant au taux de 2 %, puis de 10 % des lignes (f plus élevé) ;
  # mêmes jeu propre, B et graine.
  for (taux in c(0.02, 0.10)) {
    j <- tablexp::simuler_portefeuille(loi_reference(), pf, PER, GRAINE_REFERENCE, defauts = list(
      graine = GRAINE_DEFAUTS, declarations = list(list(type = "sexe_manquant", cible = TOUTES, taux = taux))))
    x <- construire_table(j, ROLES_REFERENCE, plan("sexe_manquant", TRUE))
    cel_r <- tablexp:::cellules_regroupees(x$etapes$exposition$resultat$exposition, FALSE)
    ret <- x$etapes$importance_exclusions$resultat$cellules_niveau_a
    ret <- ret[ret$ensemble == "total", c("age", "evenements_retires", "exposition_jours_retiree",
                                          "exposition_centrale_retiree")]
    mu_r <- mu_vrai[match(cel_r$age, e0$age)]; mu_e <- mu_vrai[match(ret$age, e0$age)]
    if (anyNA(mu_r) || anyNA(mu_e)) stop("couverture : âge sans taux du jeu propre (mesure non définie).")
    Er <- cel_r$exposition_centrale; Ee <- ret$exposition_centrale_retiree
    ok <- Ee > 0 & Er[match(ret$age, cel_r$age)] > 0
    f_vrai <- sum((Ee^2 * mu_e / Er[match(ret$age, cel_r$age)])[ok]) / sum((Ee * mu_e)[ok])
    t0 <- proc.time()[["elapsed"]]
    res <- tablexp:::sous_graine(GRAINE_COUVERTURE, {
      couvre <- logical(B); fb <- numeric(B)
      for (b in seq_len(B)) {
        cr <- cel_r; cr$evenements <- stats::rpois(nrow(cr), Er * mu_r)
        rr <- ret; rr$evenements_retires <- stats::rpois(nrow(rr), Ee * mu_e)
        s <- tablexp:::smr_comparateur_1(rr, cr, character(0), FALSE, 0.95)$ligne
        couvre[b] <- isTRUE(s$borne_inf_exacte <= 1 && 1 <= s$borne_sup_exacte)
        fb[b] <- s$rapport_variance
      }
      list(couvre = couvre, fb = fb)
    })
    p <- mean(res$couvre)
    v <- x$etapes$importance_exclusions$resultat$volumes
    cat(sprintf(paste("  constat de simulation (%s) : %d contrats (mode uniforme, graine %d), période %s - %s,",
                      "unité %s, niveau A ; exclusion non informative sexe_manquant au taux %.0f %% de toutes les",
                      "lignes (graine des défauts %d) : %d lignes exclues sur %d (%.2f %%) ; %d cellules retirées,",
                      "sum E^c_e = %.2f, E(D_e) = sum E^c_e mu_c = %.3f ; couverture à 95 %% = %.4f, erreur de",
                      "Monte-Carlo %.4f (B = %d réplications, graine %d) ; rapport de variance moyen f = %.4f, au mu",
                      "vrai %.4f ; %.1f s\n"),
                R.version.string, nrow(j), GRAINE_REFERENCE, format(PER$debut), format(PER$fin),
                x$etapes$importance_exclusions$parametres$unite_observation, 100 * taux, GRAINE_DEFAUTS,
                v$lignes_exclues, v$lignes_recues, 100 * v$lignes_exclues / v$lignes_recues, nrow(ret), sum(Ee),
                sum(Ee * mu_e), p, sqrt(p * (1 - p) / B), B, GRAINE_COUVERTURE, mean(res$fb, na.rm = TRUE), f_vrai,
                proc.time()[["elapsed"]] - t0))
    verifier(sprintf("constat de couverture exécuté (taux %.2f)", taux), TRUE)
  }
}
bilan()
