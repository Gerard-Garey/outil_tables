# S0, artefacts : octets de ecrire_plan(), ecrire_fiche(), ecrire_rapport() (R1 à R6) et des six .tsv
# (outillage de tests/reference/) avant (ev0) et après (ev1) ; vecteur mêlé (finis et non finis)
# par ecrire_valeurs() avant / après ; .tsv comparés aussi aux fichiers suivis tests/reference/.
# Usage : Rscript s0_artefacts.R <racine> <bibliothèque> <prototype> <sorties>
a <- commandArgs(TRUE); racine <- a[1]; proto <- a[3]; S <- a[4]
library(tablexp, lib.loc = a[2])
source(file.path(racine, "tests/reference/outils_reference.R"))
source(file.path(dirname(proto), "substitution.R"))
k <- readRDS(file.path(S, "constructions_R1_R6.rds"))
res <- list()
for (id in names(k$constructions)) {
  x <- k$constructions[[id]]
  o <- list()
  for (v in 0:1) {
    poser(if (v == 0) ev0 else ev1)
    o[[v + 1]] <- list(plan = ecrire_plan(x$fiche$plan), fiche = ecrire_fiche(x$fiche), rapport = ecrire_rapport(x))
    for (art in names(o[[v + 1]])) writeBin(o[[v + 1]][[art]], file.path(S, "artefacts", sprintf("%s_%s_%d.txt", id, art, v)))
    g <- aplatir_construction(x)
    ecrire_reference(g, file.path(S, "artefacts", sprintf("%s_%d.tsv", id, v)))
  }
  poser(ev_installee)
  md5 <- function(f) unname(tools::md5sum(file.path(S, "artefacts", f)))
  for (art in c("plan", "fiche", "rapport", "tsv")) {
    f0 <- if (art == "tsv") sprintf("%s_0.tsv", id) else sprintf("%s_%s_0.txt", id, art)
    f1 <- sub("_0\\.", "_1.", f0)
    res[[length(res) + 1L]] <- data.frame(plan = id, artefact = art, octets = file.size(file.path(S, "artefacts", f0)),
      md5_avant = md5(f0), md5_apres = md5(f1),
      identiques = identical(readBin(file.path(S, "artefacts", f0), "raw", 1e8), readBin(file.path(S, "artefacts", f1), "raw", 1e8)),
      egal_suivi = if (art == "tsv") identical(readBin(file.path(S, "artefacts", f1), "raw", 1e8),
                                              readBin(file.path(racine, "tests/reference", paste0(id, ".tsv")), "raw", 1e8)) else NA)
  }
}
r <- do.call(rbind, res); print(r, row.names = FALSE)
cat("artefacts identiques avant / après :", sum(r$identiques[r$artefact != "tsv"]), "/", sum(r$artefact != "tsv"),
    "; .tsv identiques :", sum(r$identiques[r$artefact == "tsv"]), "/", sum(r$artefact == "tsv"),
    "; .tsv (après) égaux aux fichiers suivis :", sum(r$egal_suivi, na.rm = TRUE), "/ 6\n")
# Vecteur mêlé par ecrire_valeurs() : finis de l'échantillon et non finis, à des places tirées.
e <- readRDS(file.path(S, "echantillon.rds"))
RNGkind("Mersenne-Twister", "Inversion", "Rejection"); set.seed(20261027L)
v <- c(e$x, e$non_finis, -0, 0)[sample.int(length(e$x) + length(e$non_finis) + 2L)]
b0 <- ev0(v, "texte", "r")$octets; b1 <- ev1(v, "texte", "r")$octets
cat("ecrire_valeurs(), vecteur mêlé de", length(v), "doubles (", sum(!is.finite(v)), "non finis ) : octets identiques",
    identical(b0, b1), "; MD5", unname(tools::md5sum({f <- tempfile(); writeBin(b1, f); f})), "\n")
cat("forme empreinte inchangée (même fonction) :", identical(ev0(v[1:1000], "empreinte", "r"), ev1(v[1:1000], "empreinte", "r")), "\n")
cat("durée de construction de PUISSANCES_TRANCHES (lecture du prototype) :", duree_table, "s ; taille :",
    format(object.size(proto_env$PUISSANCES_TRANCHES), units = "auto"), "; dimensions",
    dim(proto_env$PUISSANCES_TRANCHES$tranches), "\n")
