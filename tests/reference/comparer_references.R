# -----------------------------------------------------------------------------
#  tests/reference/comparer_references.R : tableau avant / après de deux
#  fichiers de référence (issue #17 ; ADR 0009, points 2.6 et 3.5)
#
#  Usage : Rscript tests/reference/comparer_references.R <ancien.tsv> <nouveau.tsv>
#  Imprime l'identifiant, le commit et la plateforme des deux en-têtes (hors
#  comparaison), puis la comparaison du point 3.5 (ancien = avant, nouveau =
#  après). Code de sortie : 0 si la comparaison réussit, 1 sinon, 2 si un
#  fichier est illisible ou si l'appel est invalide. Aucune installation de
#  tablexp : lecture et comparaison seules.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  ici <- if (length(f)) dirname(normalizePath(f[1L])) else file.path("tests", "reference")
  # Littéraux marqués UTF-8 quelle que soit la locale (charger_utf8()).
  for (e in parse(file.path(ici, "outils_reference.R"), encoding = "UTF-8", keep.source = FALSE)) eval(e, globalenv())
})

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) != 2L) {
    cat("Usage : Rscript tests/reference/comparer_references.R <ancien.tsv> <nouveau.tsv>\n")
    quit(save = "no", status = 2L)
  }
  lus <- tryCatch(lapply(args, lire_reference), error = function(e) e)
  if (inherits(lus, "error")) { cat("ECHEC :", conditionMessage(lus), "\n"); quit(save = "no", status = 2L) }
  champ <- function(e, k) { v <- unname(e[names(e) == k]); if (length(v)) paste(v, collapse = " | ") else "(absent)" }
  for (i in 1:2) {
    e <- lus[[i]]$entete
    cat(sprintf("%s : %s ; identifiant %s ; commit %s ; exécution %s ; mode %s\n", c("Avant", "Après")[i], args[i],
                champ(e, "identifiant"), champ(e, "commit"), champ(e, "execution"), champ(e, "mode")))
  }
  pa <- unname(lus[[1L]]$entete[names(lus[[1L]]$entete) == "plateforme"])
  pb <- unname(lus[[2L]]$entete[names(lus[[2L]]$entete) == "plateforme"])
  cat(sprintf("Identifiant de plateforme : %s\n", if (identical(pa, pb)) "identique" else "DIFFÉRENT"))
  if (!identical(pa, pb)) cat(paste0("  avant : ", pa, "\n"), paste0("  après : ", pb, "\n"), sep = "")
  r <- comparer_references(lus[[1L]], lus[[2L]])
  cat(imprimer_comparaison(r), sep = "\n")
  quit(save = "no", status = if (r$ok) 0L else 1L)
}

if (sys.nframe() == 0L) principal()
