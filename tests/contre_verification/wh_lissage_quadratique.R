# -----------------------------------------------------------------------------
#  tests/contre_verification/wh_lissage_quadratique.R : valeurs de
#  contre-vérification du lissage de Whittaker-Henderson, critère
#  quadratique (étape `lissage`, méthode wh_quadratique), par WH::WH() du
#  package WH 2.0.0 (issue #223, critère C1 ; ADR 0002, point 4 et troisième
#  annotation du 16/10/2026)
#
#  HORS CI ET HORS BATTERIE : exécuté à la main, sur un poste muni de `WH`
#  (installé hors du dépôt, dans une bibliothèque propre) ; jamais sourcé par
#  une batterie (bilan() compte en échec tout espace de noms hors priorité
#  base). N'emploie pas tablexp : il ne lit que le jeu fixé
#  tests/contre_verification/jeu_wh_lissage_quadratique.csv.
#
#  Méthode (corps de #223, C1) : pour chaque jeu et chaque segment, cellules
#  dans l'ordre des âges ; q^ = D / E^i où E^i > 0, 0 sinon (WH::WH() calcule
#  tXWz = wt * y, qui ne tolère pas NA) ; w = E^i / moyenne de E^i sur les
#  cellules où E^i > 0 (ponderation exposition) ou w = 1 (uniforme), 0 où
#  E^i = 0 ; y nommé par l'âge ;
#  WH(y = y, wt = w, lambda = h, q = z, reg = TRUE, verbose = 0).
#  Écrit, par cellule : jeu, segment, age, y_hat, et ddl (somme de edf sur le
#  segment, répétée sur ses cellules), en %.17g.
#
#  Le fichier de valeurs porte en en-tête (lignes « # ») R.version.string,
#  packageVersion("WH"), la date et sessionInfo(). Il est versionné par
#  commit `tests:` de la session principale, sur ordre du mainteneur ; toute
#  régénération est accompagnée d'un tableau avant / après. Comparaison au
#  moteur : tests/unitaires/test_lissage_quadratique.R (écart relatif
#  <= 1e-10 sur y_hat et sur ddl).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/contre_verification/wh_lissage_quadratique.R [fichier]
#  Sans argument : tests/contre_verification/valeurs_wh_lissage_quadratique.csv.
# -----------------------------------------------------------------------------

local({
  args_r <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", args_r, value = TRUE))
  dossier <- if (length(f)) dirname(normalizePath(f[1L])) else file.path("tests", "contre_verification")
  args <- commandArgs(trailingOnly = TRUE)
  sortie <- if (length(args)) args[1L] else file.path(dossier, "valeurs_wh_lissage_quadratique.csv")
  jeu <- utils::read.csv(file.path(dossier, "jeu_wh_lissage_quadratique.csv"), comment.char = "#",
                         stringsAsFactors = FALSE)
  suppressPackageStartupMessages(library(WH))
  res <- NULL
  for (j in unique(jeu$jeu)) {
    for (s in unique(jeu$segment[jeu$jeu == j])) {
      t <- jeu[jeu$jeu == j & jeu$segment == s, , drop = FALSE]
      t <- t[order(t$age), , drop = FALSE]
      e <- t$exposition_initiale > 0
      y <- ifelse(e, t$evenements / ifelse(e, t$exposition_initiale, 1), 0)
      names(y) <- t$age
      w <- switch(unique(t$ponderation),
                  exposition = ifelse(e, t$exposition_initiale / mean(t$exposition_initiale[e]), 0),
                  uniforme = ifelse(e, 1, 0),
                  stop("pondération inconnue"))
      ajuste <- WH(y = y, wt = w, lambda = unique(t$parametre_lissage), q = unique(t$ordre_difference),
                   reg = TRUE, verbose = 0)
      res <- rbind(res, data.frame(jeu = j, segment = s, age = t$age,
                                   y_hat = sprintf("%.17g", unname(ajuste$y_hat)),
                                   ddl = sprintf("%.17g", sum(ajuste$edf)), stringsAsFactors = FALSE))
    }
  }
  entete <- c(paste("# R :", R.version.string), paste("# WH :", as.character(utils::packageVersion("WH"))),
              paste("# date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
              paste("#", utils::capture.output(utils::sessionInfo())))
  con <- file(sortie, open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(entete, con)
  utils::write.csv(res, con, row.names = FALSE, quote = TRUE)
  cat(sprintf("%d valeur(s) écrite(s) dans %s\n", nrow(res), sortie))
})
