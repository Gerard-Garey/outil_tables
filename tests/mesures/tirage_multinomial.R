# -----------------------------------------------------------------------------
#  tests/mesures/tirage_multinomial.R : gain mesuré d'une vectorisation du
#  tirage de replications_multinomiales() (point 4.3 du handoff de B14 du
#  07/11/2026, PR #173 ; décision du mainteneur du 07/11/2026 : boucle
#  gardée).
#
#  Hors des batteries (aucun seuil). Deux mesures :
#   1. tirage seul, sur des groupes synthétiques : la boucle du moteur (un
#      rmultinom(1, ...) par réplique et par groupe), un rmultinom(B, ...)
#      par groupe, et des binomiales conditionnelles vectorisées sur les
#      groupes et les répliques ; les deux variantes changent le flux
#      aléatoire et ne sont pas dans le moteur ;
#   2. construction complète sur l'entrée de référence (plans de R8 munis de
#      l'étape homogeneite, ou de positionnement et de la validation
#      positionnee), temps total et part de replications_multinomiales()
#      par Rprof, pour B = 999 et B = 9 999.
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/mesures/tirage_multinomial.R
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

# --- 1. tirage seul ----------------------------------------------------------
boucle <- function(poids, groupe, eff, m) {
  n <- length(poids); G <- length(eff)
  idx <- lapply(seq_len(G), function(g) which(groupe == g)); pi_g <- lapply(idx, function(i) poids[i] / sum(poids[i]))
  Dm <- matrix(0, n, m)
  for (j in seq_len(m)) for (g in seq_len(G))
    if (length(idx[[g]])) Dm[idx[[g]], j] <- stats::rmultinom(1L, eff[g], pi_g[[g]])
  Dm
}
par_groupe <- function(poids, groupe, eff, m) {
  n <- length(poids); G <- length(eff)
  idx <- lapply(seq_len(G), function(g) which(groupe == g)); pi_g <- lapply(idx, function(i) poids[i] / sum(poids[i]))
  Dm <- matrix(0, n, m)
  for (g in seq_len(G)) if (length(idx[[g]])) Dm[idx[[g]], ] <- stats::rmultinom(m, eff[g], pi_g[[g]])
  Dm
}
binom_cond <- function(poids, groupe, eff, m) {
  n <- length(poids); G <- length(eff)
  o <- order(groupe); g <- groupe[o]; w <- poids[o]
  rang <- stats::ave(seq_along(g), g, FUN = seq_along)
  reste <- stats::ave(w, g, FUN = function(x) rev(cumsum(rev(x))))
  restant <- matrix(rep(eff, m), G, m)
  Dm <- matrix(0, n, m)
  for (k in seq_len(max(rang))) {
    i <- which(rang == k); gi <- g[i]
    x <- matrix(stats::rbinom(length(i) * m, restant[gi, ], pmin(1, w[i] / reste[i])), length(i), m)
    Dm[o[i], ] <- x; restant[gi, ] <- restant[gi, ] - x
  }
  Dm
}
cas <- function(G, S, m, eff_moy) {
  set.seed(1); groupe <- rep(seq_len(G), each = S); poids <- stats::runif(G * S, 0.5, 2)
  eff <- stats::rpois(G, eff_moy)
  t <- vapply(list(boucle, par_groupe, binom_cond), function(f) {
    set.seed(2); system.time(f(poids, groupe, eff, m))[["elapsed"]] }, 0)
  cat(sprintf("G=%5d S=%4d B=%5d eff=%6d | boucle %6.2f s  par_groupe %6.2f s  binom_cond %6.2f s  rapports %4.0f / %4.0f\n",
              G, S, m, eff_moy, t[1], t[2], t[3], t[1] / t[2], t[1] / t[3]))
}
cat("\n1. Tirage seul (G groupes de S cellules, B répliques)\n")
cas(100, 2, 999, 50); cas(100, 5, 999, 200); cas(500, 5, 999, 200); cas(2000, 10, 999, 200); cas(500, 5, 9999, 200)
cas(2, 100, 999, 5000); cas(5, 500, 999, 20000); cas(10, 1000, 999, 50000)

# --- 2. construction complète ------------------------------------------------
source(file.path(RACINE, "tests", "reference", "outils_reference.R"))
pr <- plans_reference()$R8; j <- jeu_reference(); tr <- pr$tables_reference()
mesure <- function(nom, plan, B, tab = tr) {
  f <- tempfile(); Rprof(f, interval = 0.01)
  t <- system.time(suppressWarnings(construire_table(j$donnees, j$roles, plan, tables_reference = tab)))[["elapsed"]]
  Rprof(NULL); s <- summaryRprof(f)$by.total
  part <- function(fn) { k <- paste0('"', fn, '"'); if (k %in% rownames(s)) s[k, "total.time"] else 0 }
  cat(sprintf("%-40s B=%5d total %5.1f s | replications_multinomiales %4.1f s (rmultinom %4.1f s)\n",
              nom, B, t, part("replications_multinomiales"), part("stats::rmultinom") + part("rmultinom")))
}
cat("\n2. Construction complète, entrée de référence\n")
p0 <- pr$plan; p0$etapes$validation <- NULL; p0$etapes$importance_exclusions <- NULL
for (B in c(999, 9999)) {
  ph <- p0; ph$etapes$homogeneite <- list(plage_ages = c(20, 90), tranches_age = c(20, 50, 70), replications = B)
  mesure("homogeneite (#168)", ph, B, NULL)
  pp <- p0; pp$etapes$positionnement <- list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95)
  pp$etapes$validation <- utils::modifyList(pr$plan$etapes$validation, list(relation_table = "positionnee", replications = B))
  mesure("positionnement + validation positionnee", pp, B)
}
verifier("mesures exécutées", TRUE)
bilan()
