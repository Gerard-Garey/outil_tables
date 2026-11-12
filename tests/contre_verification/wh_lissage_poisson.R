# -----------------------------------------------------------------------------
#  tests/contre_verification/wh_lissage_poisson.R : valeurs de
#  contre-vérification du lissage de Whittaker-Henderson par vraisemblance de
#  Poisson pénalisée (étape `lissage`, méthode wh_poisson), par WH::WH() du
#  package WH 2.0.0 (issue #225, critères C1 et C2 ; ADR 0002, point 4 et
#  troisième annotation du 16/10/2026)
#
#  HORS CI ET HORS BATTERIE : exécuté à la main, sur un poste muni de `WH`
#  (installé hors du dépôt, dans une bibliothèque propre) ; jamais sourcé par
#  une batterie (bilan() compte en échec tout espace de noms hors priorité
#  base). N'emploie pas tablexp : il ne lit que le jeu fixé
#  tests/contre_verification/jeu_wh_lissage_poisson.csv.
#
#  Méthode (corps de #225, C1 et C2) : pour chaque jeu et chaque segment,
#  cellules dans l'ordre des âges ; d et ec nommés par l'âge (une cellule
#  ec = 0, d = 0 est de poids nul dans WH : off = -Inf, init_y_hat = 0) ;
#    C1 : WH(d = d, ec = ec, lambda = lambda, q = z, reg = FALSE, verbose = 0,
#         tol_dev = 1e-12) (lambda déclaré du jeu, colonne parametre_lissage) ;
#         tol_dev = 1e-12 au lieu du défaut sqrt(.Machine$double.eps) : WH
#         calcule edf et ln |W + P| (diagnosis$REML) avec les poids du début
#         de sa dernière itération ; sous le défaut, l'écart relatif au
#         moteur atteint 1,5e-6 sur la LAML (au-delà du critère C1, 1e-8) et
#         7,3e-7 sur les ddl ; à 1e-12, 1,1e-9 et 5,3e-10 (mesures du
#         12/11/2026 sur le jeu ci-dessous, segment par_sexe F et
#         par_sexe_cellule_vide H) ;
#    C2 : WH(d = d, ec = ec, q = z, reg = FALSE, verbose = 0) (lambda choisi
#         par la LAML, critère « REML » de WH sous la vraisemblance de
#         Poisson ; optimize() sur ln lambda dans [ln 1e-6, ln 1e12], tolérance
#         par défaut de optimize()).
#  Écrit, par cellule : jeu, segment, age, y_hat (C1), puis, répétés sur les
#  cellules du segment : ddl (somme de edf, C1), reml (diagnosis$REML, C1),
#  lambda_laml (lambda choisi, C2) et reml_laml (diagnosis$REML au lambda
#  choisi, C2), en %.17g.
#
#  Le fichier de valeurs porte en en-tête (lignes « # ») R.version.string,
#  packageVersion("WH"), la date et sessionInfo(). Il est versionné par
#  commit `tests:` de la session principale, sur ordre du mainteneur ; toute
#  régénération est accompagnée d'un tableau avant / après. Comparaison au
#  moteur : tests/unitaires/test_lissage_poisson.R (C1 : écart relatif
#  <= 1e-8 sur theta^ contre y_hat, <= 1e-6 sur ddl_effectifs contre ddl,
#  <= 1e-8 sur LAML - l_sat contre -reml, l_sat = somme des [d ln(d / ec) - d] ;
#  C2 : |ln lambda^ moteur - ln lambda_laml| <= 1e-3 et LAML moteur au
#  lambda^ moteur >= LAML moteur à lambda_laml - 1e-6).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/contre_verification/wh_lissage_poisson.R [fichier]
#  Sans argument : tests/contre_verification/valeurs_wh_lissage_poisson.csv.
# -----------------------------------------------------------------------------

local({
  args_r <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", args_r, value = TRUE))
  dossier <- if (length(f)) dirname(normalizePath(f[1L])) else file.path("tests", "contre_verification")
  args <- commandArgs(trailingOnly = TRUE)
  sortie <- if (length(args)) args[1L] else file.path(dossier, "valeurs_wh_lissage_poisson.csv")
  jeu <- utils::read.csv(file.path(dossier, "jeu_wh_lissage_poisson.csv"), comment.char = "#",
                         stringsAsFactors = FALSE)
  suppressPackageStartupMessages(library(WH))
  res <- NULL
  for (j in unique(jeu$jeu)) {
    for (s in unique(jeu$segment[jeu$jeu == j])) {
      t <- jeu[jeu$jeu == j & jeu$segment == s, , drop = FALSE]
      t <- t[order(t$age), , drop = FALSE]
      d <- t$evenements
      ec <- t$exposition_centrale
      names(d) <- names(ec) <- t$age
      z <- unique(t$ordre_difference)
      c1 <- WH(d = d, ec = ec, lambda = unique(t$parametre_lissage), q = z, reg = FALSE, verbose = 0,
               tol_dev = 1e-12)
      c2 <- WH(d = d, ec = ec, q = z, reg = FALSE, verbose = 0)
      res <- rbind(res, data.frame(jeu = j, segment = s, age = t$age,
                                   y_hat = sprintf("%.17g", unname(c1$y_hat)),
                                   ddl = sprintf("%.17g", sum(c1$edf)),
                                   reml = sprintf("%.17g", c1$diagnosis$REML),
                                   lambda_laml = sprintf("%.17g", c2$lambda),
                                   reml_laml = sprintf("%.17g", c2$diagnosis$REML), stringsAsFactors = FALSE))
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
