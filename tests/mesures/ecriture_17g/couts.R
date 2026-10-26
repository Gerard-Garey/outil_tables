# S1, S2, S3, S5, S7 : temps avant (ev0, sprintf) / après (ev1, prototype), deux versions recompilées
# (cmpfun), 21 répétitions alternées (ordre inversé une répétition sur deux), gc() avant chaque mesure ;
# médiane, minimum, maximum.
# Usage : Rscript couts.R <racine> <bibliothèque> <prototype> <sorties> [parties : a,b,art,tsv,table,temoins] [plans : R1,R2...]
a <- commandArgs(TRUE); racine <- a[1]; proto <- a[3]; S <- a[4]
parties <- if (length(a) >= 5) strsplit(a[5], ",")[[1]] else c("a", "b", "art", "tsv", "table", "temoins")
library(tablexp, lib.loc = a[2])
source(file.path(racine, "tests/reference/outils_reference.R"))
source(file.path(dirname(proto), "substitution.R"))
R <- 21L
mesurer <- function(f0, f1) {
  t <- matrix(NA_real_, R, 2L, dimnames = list(NULL, c("avant", "apres")))
  for (r in seq_len(R)) for (v in if (r %% 2L) 0:1 else 1:0) {
    f <- if (v == 0L) f0 else f1
    gc(); t[r, v + 1L] <- system.time(f())[["elapsed"]]
  }
  t
}
resume <- function(t, par = 1, unite = "s") {
  s <- apply(t / par, 2, function(u) c(mediane = median(u), min = min(u), max = max(u)))
  data.frame(version = colnames(s), mediane = s["mediane", ], min = s["min", ], max = s["max", ], unite = unite, row.names = NULL)
}
afficher <- function(titre, t, par = 1, unite = "s") { cat("\n==", titre, "\n"); print(resume(t, par, unite), digits = 4) }
RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(20261028L)
ecrits <- readRDS(file.path(S, "doubles_R1_R6.rds"))
e <- readRDS(file.path(S, "echantillon.rds"))
de_bits <- function(E, hi, lo) {
  n <- length(E); o <- matrix(as.raw(0), 8L, n)
  for (k in 1:4) { o[k, ] <- as.raw(lo %% 256); lo <- lo %/% 256 }
  o[5L, ] <- as.raw(hi %% 256); hi <- hi %/% 256; o[6L, ] <- as.raw(hi %% 256); hi <- hi %/% 256
  o[7L, ] <- as.raw((E %% 16) * 16 + hi); o[8L, ] <- as.raw(E %/% 16)
  readBin(as.vector(o), "double", n = n, size = 8L, endian = "little")
}
n <- 1e5
x_r <- sample(ecrits, n, replace = TRUE)                       # tirés dans R1 à R6 (non finis compris)
x_rf <- sample(ecrits[is.finite(ecrits)], n, replace = TRUE)   # tirés parmi les finis
x_u <- de_bits(floor(runif(n) * 2047), floor(runif(n) * 2^20), floor(runif(n) * 2^32))   # uniforme en exposant
x_s <- de_bits(rep(0, n), floor(runif(n) * 2^20), floor(runif(n) * 2^32))                 # sous-normaux seuls
if ("a" %in% parties) {
  for (cas in list(list("a, 1e5 tirés dans R1 à R6 (non finis compris)", x_r), list("a, 1e5 finis tirés dans R1 à R6", x_rf),
                   list("a, pire cas uniforme en exposant, 1e5", x_u), list("a, sous-normaux seuls (adverse), 1e5", x_s))) {
    v <- cas[[2]]
    afficher(cas[[1]], mesurer(function() ev0(v, "texte", "r"), function() ev1(v, "texte", "r")), n / 1e6, "µs par double")
  }
}
if ("b" %in% parties) {
  k <- 1000L
  for (cas in list(list("b, appel d'un double fini écrit par R1 à R6 (le 100e)", ecrits[is.finite(ecrits)][100]), list("b, appel d'un double sous-normal", 5e-324),
                   list("appel de 149 doubles (vecteur de rapport)", ecrits[1:149]))) {
    v <- cas[[2]]
    afficher(paste(cas[[1]], "-", k, "appels par mesure"),
             mesurer(function() for (i in seq_len(k)) ev0(v, "texte", "r"), function() for (i in seq_len(k)) ev1(v, "texte", "r")),
             k / 1e3, "ms par appel")
  }
}
if ("art" %in% parties) {
  K <- readRDS(file.path(S, "constructions_R1_R6.rds"))$constructions
  ids <- if (length(a) >= 6) strsplit(a[6], ",")[[1]] else names(K)
  for (id in ids) {
    x <- K[[id]]
    avec <- function(f, g) function() { poser(f); g(); poser(ev_installee) }
    afficher(paste(id, "ecrire_plan(), 100 écritures"),
             mesurer(avec(ev0, function() for (i in 1:100) ecrire_plan(x$fiche$plan)), avec(ev1, function() for (i in 1:100) ecrire_plan(x$fiche$plan))), 0.1, "ms par écriture")
    afficher(paste(id, "ecrire_fiche(), 100 écritures"),
             mesurer(avec(ev0, function() for (i in 1:100) ecrire_fiche(x$fiche)), avec(ev1, function() for (i in 1:100) ecrire_fiche(x$fiche))), 0.1, "ms par écriture")
    afficher(paste(id, "ecrire_rapport()"),
             mesurer(avec(ev0, function() ecrire_rapport(x)), avec(ev1, function() ecrire_rapport(x))), 1, "s")
  }
}
if ("tsv" %in% parties) {
  K <- readRDS(file.path(S, "constructions_R1_R6.rds"))$constructions
  f <- tempfile(fileext = ".tsv")
  six <- function(ev) function() { poser(ev); for (x in K) ecrire_reference(aplatir_construction(x), f); poser(ev_installee) }
  afficher("six .tsv (aplatir_construction() + ecrire_reference()), hors construction", mesurer(six(ev0), six(ev1)), 1, "s")
}
if ("table" %in% parties) {
  t <- numeric(R); for (r in seq_len(R)) { gc(); t[r] <- system.time(proto_env$table_puissances_tranches())[["elapsed"]] }
  cat("\n== c, table_puissances_tranches() : médiane", median(t), "s ; min", min(t), "; max", max(t), "; taille",
      format(object.size(proto_env$PUISSANCES_TRANCHES), units = "auto"), "\n")
}
if ("temoins" %in% parties) {
  # Témoins de batterie proposés : 1e5 doubles de l'échantillon (tirés sous graine), texte écrit,
  # MD5 comparé à une valeur attendue ; et les égalités exactes de l'échantillon (f) seules.
  i <- sort(sample.int(length(e$x), 1e5)); v <- e$x[i]; veg <- e$x[e$egalites]
  temoin <- function(v) function() { f <- tempfile(); writeBin(proto_env$octets_17g(v), f); tools::md5sum(f); unlink(f) }
  t1 <- numeric(R); t2 <- numeric(R)
  for (r in seq_len(R)) { gc(); t1[r] <- system.time(temoin(v)())[["elapsed"]]; gc(); t2[r] <- system.time(temoin(veg)())[["elapsed"]] }
  cat("\n== S7, témoin 1e5 doubles de l'échantillon + MD5 : médiane", median(t1), "s ; min", min(t1), "; max", max(t1), "\n")
  cat("== S7, témoin des", length(veg), "égalités exactes + MD5 : médiane", median(t2), "s ; min", min(t2), "; max", max(t2), "\n")
  f <- tempfile(); writeBin(proto_env$octets_17g(v), f); cat("   MD5 attendu (1e5) :", unname(tools::md5sum(f)), "\n")
}
