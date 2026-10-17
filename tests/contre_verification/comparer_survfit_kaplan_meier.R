# -----------------------------------------------------------------------------
#  tests/contre_verification/comparer_survfit_kaplan_meier.R : comparaison
#  de l'estimateur de Kaplan-Meier du moteur aux valeurs de
#  contre-vérification écrites par survfit_kaplan_meier.R (issues #15 et #7 ;
#  spécification d'actuary, A.12)
#
#  Définit seulement des fonctions, en R de base ; sourcé par
#  tests/unitaires/test_brut_kaplan_meier.R ; emploie proche() de
#  tests/harnais.R. Ne charge jamais `survival` : il lit un fichier texte.
#
#  lire_valeurs_survfit(fichier) : list(entete, valeurs) ; lignes « # » de
#  l'en-tête, puis les sauts (segment, cellule, time, n.risk, n.event, surv,
#  std.err, q_inf, q_sup).
#  comparer_survfit(valeurs, km, sauts) : TRUE ou une chaîne qui dit l'écart.
#  Comparaison sur les cellules `defini` du moteur (km : table kaplan_meier ;
#  sauts : table kaplan_meier_sauts ; segmentation "segment") ; une cellule
#  rendue par survfit que le moteur déclare ensemble_a_risque_vide (C 38)
#  est consignée, non comparée. Tolérances (A.12) : age_exact contre time
#  1e-13 absolue ; r et d égalité exacte ; survie_cellule contre surv et
#  1 - surv au dernier saut contre taux, 1e-13 relative ;
#  sqrt(variance_log_survie_cellule) contre std.err, 1e-13 relative ; bornes
#  au dernier saut, 1e-12 relative.
# -----------------------------------------------------------------------------

lire_valeurs_survfit <- function(fichier) {
  lignes <- readLines(fichier, warn = FALSE, encoding = "UTF-8")
  entete <- lignes[startsWith(lignes, "#")]
  valeurs <- utils::read.csv(text = lignes[!startsWith(lignes, "#")], stringsAsFactors = FALSE,
                             colClasses = c("character", "integer", "numeric", "integer", "integer",
                                            rep("numeric", 4L)))
  list(entete = entete, valeurs = valeurs)
}

comparer_survfit <- function(valeurs, km, sauts) {
  attendues <- c("segment", "cellule", "time", "n.risk", "n.event", "surv", "std.err", "q_inf", "q_sup")
  if (!identical(names(valeurs), attendues))
    return(sprintf("colonnes du fichier de valeurs : %s", paste(names(valeurs), collapse = ", ")))
  definies <- km[km$etat == "defini", , drop = FALSE]
  cle_v <- paste(valeurs$segment, valeurs$cellule)
  cle_d <- paste(definies$segment, definies$age)
  cellules <- unique(cle_v[cle_v %in% cle_d])
  # Cellules définies du moteur qui portent un événement : toutes rendues
  # par survfit.
  avec_evenement <- cle_d[definies$evenements > 0L]
  if (!setequal(cellules, avec_evenement))
    return(sprintf("cellules définies à événement : moteur %s ; survfit %s",
                   paste(avec_evenement, collapse = ", "), paste(cellules, collapse = ", ")))
  if (!length(cellules)) return("aucune cellule définie comparée")
  ecarts <- character(0)
  for (c in cellules) {
    v <- valeurs[cle_v == c, , drop = FALSE]
    s <- sauts[paste(sauts$segment, sauts$age) == c, , drop = FALSE]
    l <- definies[cle_d == c, , drop = FALSE]
    n <- nrow(s)
    r <- if (nrow(v) != n) sprintf("%d saut(s) contre %d", n, nrow(v))
      else if (!identical(s$ensemble_a_risque, v$n.risk)) "n.risk"
      else if (!identical(s$evenements, v$n.event)) "n.event"
      else {
        verifs <- list(
          time = proche(s$age_exact, v$time, rel = 0, abs = 1e-13),
          surv = proche(s$survie_cellule, v$surv, rel = 1e-13, abs = 0),
          q = proche(l$taux, 1 - v$surv[n], rel = 1e-13, abs = 0),
          std.err = proche(sqrt(s$variance_log_survie_cellule), v$std.err, rel = 1e-13, abs = 0),
          bornes = proche(c(l$borne_inf_asymptotique, l$borne_sup_asymptotique), c(v$q_inf[n], v$q_sup[n]),
                          rel = 1e-12, abs = 0))
        ko <- !vapply(verifs, isTRUE, logical(1L))
        if (any(ko)) paste(sprintf("%s : %s", names(verifs)[ko], unlist(verifs[ko])), collapse = " ; ")
        else TRUE
      }
    if (!isTRUE(r)) ecarts <- c(ecarts, sprintf("cellule %s : %s", c, r))
  }
  if (length(ecarts)) ecarts else TRUE
}
