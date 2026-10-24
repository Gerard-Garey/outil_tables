# -----------------------------------------------------------------------------
#  tests/mesures/temps_construire_table.R : temps de construire_table() sur
#  des portefeuilles simulés de 10^5 et 10^6 têtes (issue #11, critère
#  « temps mesuré pour 10^6 lignes, consigné dans le commit, sans seuil »)
#
#  Hors des batteries (aucun seuil ; durée de plusieurs dizaines de
#  secondes). Mesure séparément la simulation (#10), l'empreinte des données
#  (ADR 0007, point 9.3) et la construction (étapes donnees et exposition),
#  pour les deux transitions, avec et sans découpage par année ; puis le
#  temps de l'étape `brut` (#14, #15) : construction avec brut (poisson,
#  binomial), avec Kaplan-Meier en plus, et appel direct de l'estimateur de
#  Kaplan-Meier (taux_kaplan_meier()) sur les résultats de la construction :
#  cet appel passe les observations (data.frame) et mesure donc la voie de
#  recalcul des fenêtres, non la voie de construire_table(), qui transmet
#  les fenêtres de l'étape `exposition` (#38). Empreintes (#71) : temps
#  d'empreinte_resultat() sur l'objet de la construction avec brut (poisson,
#  binomial) et d'empreinte_donnees() sur le jeu simulé, REPETITIONS mesures
#  chacune après gc(), médiane et étendue, avec la valeur de l'empreinte
#  (comparaison avant / après).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/mesures/temps_construire_table.R [n ...]
#  Sans argument : n = 1e5 puis 1e6.
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
TAILLES <- if (length(ARGS)) as.numeric(ARGS) else c(1e5, 1e6)

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
LOI <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
            rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
plan <- function(transition, par_annee)
  list(transition = transition, periode = PER, delai_declaration = 90, segmentation = "sexe",
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "plus_fine", par_annee = par_annee)))
REPETITIONS <- 5L
# Médiane, minimum et maximum de REPETITIONS mesures de f(), gc() avant chacune.
repetitions <- function(f) {
  r <- numeric(REPETITIONS)
  for (k in seq_len(REPETITIONS)) {
    invisible(gc())
    t0 <- proc.time()[["elapsed"]]
    v <- f()
    r[k] <- proc.time()[["elapsed"]] - t0
  }
  sprintf("médiane %.2f s (%.2f à %.2f, %d mesures), empreinte %s", stats::median(r), min(r), max(r), REPETITIONS, v)
}
chrono <- function(expr) {
  t0 <- proc.time()[["elapsed"]]
  v <- expr
  list(valeur = v, secondes = proc.time()[["elapsed"]] - t0)
}

for (n in TAILLES) {
  pf <- list(mode = "uniforme", n = n, proportion_hommes = 0.5, age_min = 20, age_max = 75,
             origine = D("2005-01-01"))
  s <- chrono(simuler_portefeuille(LOI, pf, PER, 20261016L))
  cat(sprintf("\nn = %.0f têtes : simulation %.1f s\n", n, s$secondes))
  e <- chrono(tablexp:::empreinte_donnees(s$valeur, ROLES))
  cat(sprintf("  empreinte des données (représentation canonique, md5) : %.1f s\n", e$secondes))
  sc <- s$valeur
  sc$id_contrat <- sprintf("C%09d", sc$id_contrat); sc$id_tete <- sprintf("T%09d", sc$id_tete)
  e <- chrono(tablexp:::empreinte_donnees(sc, ROLES))
  cat(sprintf("  empreinte des données, identifiants en chaîne : %.1f s\n", e$secondes))
  for (cas in list(list("vivant -> décédé", list(depart = "vivant", arrivee = "deces"), FALSE),
                   list("vivant -> décédé, par année", list(depart = "vivant", arrivee = "deces"), TRUE),
                   list("en cours -> racheté", list(depart = "en_cours", arrivee = "rachat"), FALSE))) {
    x <- chrono(construire_table(s$valeur, ROLES, plan(cas[[2]], cas[[3]])))
    t <- x$valeur$etapes$exposition$resultat$exposition
    cat(sprintf("  construire_table, %s : %.1f s (%d cellules, %.0f jours d'exposition, %d événements)\n",
                cas[[1]], x$secondes, nrow(t), sum(t$exposition_jours), sum(t$evenements)))
  }
  vd <- list(depart = "vivant", arrivee = "deces")
  pb <- plan(vd, FALSE)
  pb$etapes$brut <- list(estimateurs = c("poisson", "binomial"), niveau = 0.95)
  xb <- chrono(construire_table(s$valeur, ROLES, pb))
  cat(sprintf("  construire_table, vivant -> décédé, brut (poisson, binomial) : %.1f s\n", xb$secondes))
  cat(sprintf("  empreinte_resultat(), même objet : %s\n", repetitions(function() tablexp:::empreinte_resultat(xb$valeur))))
  cat(sprintf("  empreinte_donnees(), jeu simulé : %s\n", repetitions(function() tablexp:::empreinte_donnees(s$valeur, ROLES))))
  pb$etapes$brut$estimateurs <- c("poisson", "binomial", "kaplan_meier")
  xk <- chrono(construire_table(s$valeur, ROLES, pb))
  sk <- xk$valeur$etapes$brut$resultat$kaplan_meier_sauts
  cat(sprintf("  construire_table, vivant -> décédé, brut (poisson, binomial, kaplan_meier) : %.1f s (%d sauts)\n",
              xk$secondes, nrow(sk)))
  et <- xk$valeur$etapes
  k <- chrono(tablexp:::taux_kaplan_meier(et$donnees$resultat$observations, et$exposition$resultat$exposition,
                                          "sexe", vd, PER, 90, et$exposition$parametres$convention$retenue, 0.95,
                                          et$exposition$parametres))
  cat(sprintf("  taux_kaplan_meier(), appel direct sur les observations (fenêtres recalculées) : %.2f s\n", k$secondes))
  verifier(sprintf("mesure n = %.0f exécutée", n), TRUE)
}
bilan()
