# -----------------------------------------------------------------------------
#  tests/contre_verification/survfit_kaplan_meier.R : valeurs de
#  contre-vérification de l'estimateur de Kaplan-Meier par
#  survival::survfit() (issues #15 et #7 ; spécification d'actuary, A.12 ;
#  ADR 0002, point 4 et troisième annotation du 16/10/2026, A11)
#
#  HORS CI ET HORS BATTERIE : exécuté à la main par le mainteneur, sur son
#  poste ou sur un poste de contre-vérification muni de `survival` ; jamais
#  sourcé par une batterie (bilan() compte en échec tout espace de noms hors
#  priorité base). N'emploie pas tablexp : il ne lit que les âges rationnels
#  du jeu fixé tests/contre_verification/jeu_kaplan_meier.csv.
#
#  Méthode (A.12) : tau = (x L + j) / L (quotient correctement arrondi du
#  rationnel x + j / L) ; pour chaque segment et chaque cellule x qui porte
#  un événement : a' = max(tau_a, x), b' = min(tau_b, x + 1), e' = événement
#  et x <= tau_b < x + 1, lignes gardées si a' < b', ou e', ou tau_b = x
#  (unité censurée à l'instant x : à risque à x dans l'ensemble fermé, la
#  censure suivant l'événement ; constat C1 de l'audit, segment Z du jeu) ;
#  puis
#  survfit(Surv(a' - delta, b', e') ~ 1, timefix = FALSE, conf.type =
#  "logit", conf.int = 0.95), delta = 1e-7 < 1 / 133 590 (l'ensemble
#  {a' - delta < t <= b'} est l'ensemble à risque fermé du moteur ;
#  timefix = TRUE échoue sur les lignes de durée nulle, mesure d'actuary).
#  Écrit, par saut de la cellule : segment, cellule, time, n.risk, n.event,
#  surv, std.err, q_inf = 1 - upper, q_sup = 1 - lower, en %.17g.
#
#  Le fichier de valeurs porte en en-tête (lignes « # ») R.version.string,
#  packageVersion("survival"), la date et sessionInfo(). Il est versionné
#  par commit `tests:` de la session principale, sur ordre du mainteneur ;
#  toute régénération est accompagnée d'un tableau avant / après.
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/contre_verification/survfit_kaplan_meier.R [fichier]
#  Sans argument : tests/contre_verification/valeurs_survfit_kaplan_meier.csv.
# -----------------------------------------------------------------------------

local({
  args_r <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", args_r, value = TRUE))
  dossier <- if (length(f)) dirname(normalizePath(f[1L])) else file.path("tests", "contre_verification")
  args <- commandArgs(trailingOnly = TRUE)
  sortie <- if (length(args)) args[1L] else file.path(dossier, "valeurs_survfit_kaplan_meier.csv")
  jeu <- utils::read.csv(file.path(dossier, "jeu_kaplan_meier.csv"), comment.char = "#",
                         stringsAsFactors = FALSE)
  jeu <- jeu[jeu$retenue, , drop = FALSE]
  ta <- (jeu$debut_x * jeu$debut_L + jeu$debut_j) / jeu$debut_L
  tb <- (jeu$fin_x * jeu$fin_L + jeu$fin_j) / jeu$fin_L
  suppressPackageStartupMessages(library(survival))
  delta <- 1e-7
  res <- NULL
  for (g in sort(unique(jeu$segment), method = "radix")) {
    s <- jeu$segment == g
    cellules <- sort(unique(jeu$fin_x[s & jeu$evenement]))
    for (x in cellules) {
      a <- pmax(ta[s], x); b <- pmin(tb[s], x + 1)
      e <- jeu$evenement[s] & tb[s] >= x & tb[s] < x + 1
      k <- a < b | e | tb[s] == x
      ff <- survfit(Surv(a[k] - delta, b[k], e[k]) ~ 1, timefix = FALSE, conf.type = "logit",
                    conf.int = 0.95)
      j <- ff$n.event > 0 & ff$time < x + 1
      res <- rbind(res, data.frame(segment = g, cellule = x, time = ff$time[j], n.risk = ff$n.risk[j],
                                   n.event = ff$n.event[j], surv = ff$surv[j], std.err = ff$std.err[j],
                                   q_inf = 1 - ff$upper[j], q_sup = 1 - ff$lower[j],
                                   stringsAsFactors = FALSE))
    }
  }
  num <- c("time", "surv", "std.err", "q_inf", "q_sup")
  for (k in num) res[[k]] <- ifelse(is.na(res[[k]]), "NA", sprintf("%.17g", res[[k]]))
  entete <- c(sprintf("# R : %s", R.version.string),
              sprintf("# survival : %s", as.character(utils::packageVersion("survival"))),
              sprintf("# date : %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
              "# jeu : tests/contre_verification/jeu_kaplan_meier.csv ; script : tests/contre_verification/survfit_kaplan_meier.R",
              "# sessionInfo() :",
              paste("#  ", utils::capture.output(print(utils::sessionInfo()))))
  con <- file(sortie, open = "wb")
  on.exit(close(con))
  writeLines(entete, con, sep = "\n")
  utils::write.csv(res, con, row.names = FALSE, quote = FALSE, eol = "\n")
  cat(sprintf("%d saut(s) écrits dans %s\n", nrow(res), sortie))
})
