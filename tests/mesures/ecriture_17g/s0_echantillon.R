# S0, échantillon : prototype contre sprintf("%.17g", x + 0) de la glibc (octets), relecture par
# as.numeric, non finis contre sprintf() de R ; écrit x.bin et le texte pour le juge Python.
# Usage : Rscript s0_echantillon.R <prototype .R> <dossier des sorties>
a <- commandArgs(TRUE); source(a[1]); S <- a[2]
e <- readRDS(file.path(S, "echantillon.rds")); x <- e$x
lignes <- function(t) charToRaw(paste0(t, "\n", collapse = ""))
cat("plateforme :", R.version.string, "|", Sys.getlocale("LC_ALL"), "| glibc", paste(unlist(extSoftVersion()["iconv"])), "\n")
tp <- system.time(proto <- octets_17g(x))[["elapsed"]]
tg <- system.time(glibc <- lignes(sprintf("%.17g", x + 0)))[["elapsed"]]
cat("finis N =", length(x), "; octets identiques au texte glibc :", identical(proto, glibc),
    "; temps prototype", tp, "s, sprintf", tg, "s\n")
tx <- strsplit(rawToChar(proto), "\n", fixed = TRUE)[[1]]
tgl <- sprintf("%.17g", x + 0)
d <- which(tx != tgl)
cat("lignes différentes de la glibc :", length(d), "\n")
if (length(d)) print(head(data.frame(i = d, famille = e$famille[d], proto = tx[d], glibc = tgl[d]), 20))
cat("par famille, écarts :\n"); print(tapply(tx != tgl, factor(e$famille, levels = unique(e$famille)), sum))
lu <- as.numeric(tx)
ol <- matrix(writeBin(lu, raw()), 8L); ox <- matrix(writeBin(x + 0, raw()), 8L)
cat("relecture as.numeric : doubles relus à des octets différents :", sum(colSums(ol != ox) > 0), "sur", length(x), "\n")
cat("égalités exactes de l'échantillon (indices) :", length(e$egalites), "; écarts sur elles :",
    sum(tx[e$egalites] != tgl[e$egalites]), "\n")
# Non finis : jetons du prototype contre sprintf() de R (forme actuelle de doubles_texte()).
nf <- e$non_finis
cat("non finis :", length(nf), "; octets identiques à sprintf de R :",
    identical(octets_17g(nf), lignes(sprintf("%.17g", nf))), "\n")
print(table(jeton_sprintf = sprintf("%.17g", nf), useNA = "always"))
# Cas isolés : -0, vecteurs mêlés, longueur 1, longueur 0.
cas <- c(-0, 0, NA, -1, NaN, 5e-324, -5e-324, Inf, -Inf, .Machine$double.xmax, -.Machine$double.xmax)
cat("cas isolés identiques :", identical(octets_17g(cas), lignes(sprintf("%.17g", ifelse(is.finite(cas), cas + 0, cas)))),
    "; longueur 1 :", identical(octets_17g(0.95), lignes("0.94999999999999996")),
    "; longueur 0 :", identical(octets_17g(numeric(0)), raw(0)), "\n")
writeBin(x + 0, file.path(S, "x.bin"), size = 8L, endian = "little")
writeBin(proto, file.path(S, "texte_proto.txt"))
cat("MD5 du texte du prototype :", unname(tools::md5sum(file.path(S, "texte_proto.txt"))), "\n")
