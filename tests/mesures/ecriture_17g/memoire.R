# S4 : mémoire maximale (gc(), colonne « max used », Ncells + Vcells, Mo) pour l'écriture de 1e6
# doubles, dans un processus neuf par cas.
# Usage : Rscript memoire.R <racine> <bibliothèque> <prototype> <sorties> <cas : uniforme|r1r6|sousnormaux> <version : 0|1>
a <- commandArgs(TRUE); racine <- a[1]; proto <- a[3]; S <- a[4]; cas <- a[5]; version <- a[6]
library(tablexp, lib.loc = a[2])
source(file.path(dirname(proto), "substitution.R"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(20261029L)
n <- 1e6
de_bits <- function(E, hi, lo) {
  n <- length(E); o <- matrix(as.raw(0), 8L, n)
  for (k in 1:4) { o[k, ] <- as.raw(lo %% 256); lo <- lo %/% 256 }
  o[5L, ] <- as.raw(hi %% 256); hi <- hi %/% 256; o[6L, ] <- as.raw(hi %% 256); hi <- hi %/% 256
  o[7L, ] <- as.raw((E %% 16) * 16 + hi); o[8L, ] <- as.raw(E %/% 16)
  readBin(as.vector(o), "double", n = n, size = 8L, endian = "little")
}
x <- switch(cas,
  uniforme = de_bits(floor(runif(n) * 2047), floor(runif(n) * 2^20), floor(runif(n) * 2^32)),
  r1r6 = sample(readRDS(file.path(S, "doubles_R1_R6.rds")), n, replace = TRUE),
  sousnormaux = de_bits(rep(0, n), floor(runif(n) * 2^20), floor(runif(n) * 2^32)))
ev <- if (version == "0") ev0 else ev1
rm(src); invisible(gc(reset = TRUE)); g0 <- gc()
tt <- system.time(o <- ev(x, "texte", "r")$octets)[["elapsed"]]
g1 <- gc()
mo <- function(g) sum(g[, ncol(g)])   # dernière colonne : max used (Mb)
cat(sprintf("cas %-11s version %s : max used avant %.1f Mo, après %.1f Mo, hausse %.1f Mo ; temps %.2f s ; octets écrits %d\n",
            cas, version, mo(g0), mo(g1), mo(g1) - mo(g0), tt, length(o)))
