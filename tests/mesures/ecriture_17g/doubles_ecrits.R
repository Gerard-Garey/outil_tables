# Relevé (base, abda2d8) : doubles écrits en forme « texte » (Date exclues) par
# construire_table(), ecrire_plan(), ecrire_fiche(), ecrire_rapport(), R1 à R6 ;
# sauvegarde des constructions et des doubles écrits.
# Usage : Rscript doubles_ecrits.R <racine du dépôt> <bibliothèque> <dossier de sortie>
a <- commandArgs(TRUE); racine <- a[1]; lib <- a[2]; sortie <- a[3]
library(tablexp, lib.loc = lib)
source(file.path(racine, "tests/reference/outils_reference.R"))
ns <- asNamespace("tablexp")
releve <- new.env(); releve$n <- 0; releve$appels <- 0L; releve$x <- list()
trace("ecrire_valeurs", where = ns, print = FALSE,
      tracer = quote(if (identical(forme, "texte") && is.double(x) && !is.object(x)) {
        releve$n <- releve$n + length(x); releve$appels <- releve$appels + 1L
        releve$x[[length(releve$x) + 1L]] <- as.vector(x) }))
compter <- function(expr) { releve$n <- 0; releve$appels <- 0L; force(expr); c(releve$n, releve$appels) }
j <- jeu_reference()
constructions <- list(); tab <- list()
for (id in names(plans_reference())) {
  p <- plans_reference()[[id]]$plan
  ct <- compter(x <- construire_table(j$donnees, j$roles, p))
  pl <- compter(ecrire_plan(x$fiche$plan)); fi <- compter(ecrire_fiche(x$fiche)); ra <- compter(ecrire_rapport(x))
  tab[[id]] <- c(construire = ct[1], plan = pl[1], fiche = fi[1], rapport = ra[1],
                 appels_plan = pl[2], appels_fiche = fi[2], appels_rapport = ra[2])
  constructions[[id]] <- x
}
untrace("ecrire_valeurs", where = ns)
print(do.call(rbind, tab))
appels <- releve$x
tous <- unlist(appels)
cat("doubles écrits (hors construction), R1 à R6 :", length(tous), "; non finis :", sum(!is.finite(tous)),
    "; distincts :", length(unique(tous)), "; appels :", length(appels),
    "; plus long vecteur :", max(lengths(appels)), "\n")
saveRDS(tous, file.path(sortie, "doubles_R1_R6.rds"))
saveRDS(appels, file.path(sortie, "appels_R1_R6.rds"))
saveRDS(list(jeu = j, constructions = constructions), file.path(sortie, "constructions_R1_R6.rds"))
