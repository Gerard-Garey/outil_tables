# -----------------------------------------------------------------------------
#  tests/mesures/calculs_repetes.R : mesure des points 1 et 3 de l'issue #164
#  (calculs répétés ; étape (6) de la fiche de B14, PR #173 ; décision M281 :
#  mesurer d'abord, aucune modification de R/, aucune optimisation ; ADR 0002,
#  point 6)
#
#  Hors des batteries (aucun seuil). Deux mesures, chacune consignée dans le
#  commit qui la cite :
#   1. classification (point 1 de #164, comparateur 2 d'importance_exclusions) :
#      classer_lignes_exclues() (R/validation.R) est appelée à chaque ensemble
#      et à chaque niveau du comparateur 2, et par retrait_contrefactuel() ; à
#      la taille de R6 (jeu_reference() : loi, portefeuille de 10^5 contrats,
#      période, graine ; plan R6 muni de l'étape validation de R8 pour que le
#      comparateur 2 soit produit) sur un jeu à exclusions déclarées pour
#      k = 1, 3 et 6 motifs, plus une variante k = 6 à fort volume de lignes
#      exclues : durée de construire_table() sans puis avec l'étape
#      importance_exclusions (REPETITIONS mesures chacune après gc(), médiane et
#      étendue), nombre d'appels de classer_lignes_exclues() et de
#      retrait_contrefactuel() (trace() sur l'espace de noms du package
#      installé dans la bibliothèque temporaire du processus, retirée aussitôt),
#      durée d'un appel direct de classer_lignes_exclues() sur les lignes
#      exclues réelles, part par Rprof ;
#   2. wilcoxon (point 3 de #164, statistiques_test() de R/validation.R) :
#      construction de validation à 1 420 cellules (plan de R8 sous découpage
#      par année, estimateur de Poisson seul : 71 âges x 2 sexes x 10 années),
#      dont 1 389 retenues (C+) ; relation independante ; replications = 999, 9 999 et
#      100 000 : durée de la construction sans et avec l'étape validation, part
#      de l'étape validation, de statistiques_test() et de wilcoxon_rangs() par
#      Rprof ; puis appel direct de statistiques_test() sur des matrices de la
#      taille réelle (blocs de ELEMENTS_BLOC_REPLICATIONS éléments, comme
#      replications_poisson()) : tirage, statistiques_test() entière, et le
#      vapply de Wilcoxon seul (la différence est le temps des statistiques
#      vectorisées et de la préparation de d, déduit et non mesuré à part).
#      Le tirage direct n'est pas celui du moteur (graine locale de la mesure) :
#      il ne sert qu'à la durée. À 100 000 réplications, seule la construction
#      complète est exécutée ; l'appel direct est extrapolé linéairement en B
#      depuis 9 999 (écrit comme tel dans la sortie).
#
#  Locale : le moteur refuse la table de référence de R8 hors locale UTF-8
#  (refus chaine_codage_indetermine, mesuré sous la locale C) : lancer sous une
#  locale UTF-8, par exemple LC_ALL=C.UTF-8.
#
#  Usage (depuis la racine du dépôt) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/calculs_repetes.R [classification] [wilcoxon]
#  Sans argument : les deux (quatre minutes environ sur le poste de la mesure).
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
installer_tablexp(RACINE)
source(file.path(RACINE, "tests", "reference", "outils_reference.R"))

ARGS <- commandArgs(trailingOnly = TRUE)
MESURES <- if (length(ARGS)) ARGS else c("classification", "wilcoxon")
D <- as.Date
REPETITIONS <- 3L
GRAINE_DEFAUTS <- 20261025L
TOUTES <- c("deces", "rachat", "en_cours")
T_DEBUT <- proc.time()[["elapsed"]]
cat(sprintf("plateforme : %s ; BLAS %s ; LAPACK %s ; locale %s ; %d coeur(s) visible(s)\n",
            R.version$platform, extSoftVersion()[["BLAS"]], La_library(), Sys.getlocale("LC_CTYPE"),
            parallel::detectCores()))

# Médiane, minimum et maximum de n mesures de f(), gc() avant chacune ; rend
# aussi la dernière valeur.
repetitions <- function(f, n = REPETITIONS) {
  r <- numeric(n)
  for (k in seq_len(n)) {
    invisible(gc())
    t0 <- proc.time()[["elapsed"]]
    v <- f()
    r[k] <- proc.time()[["elapsed"]] - t0
  }
  list(mediane = stats::median(r), min = min(r), max = max(r), valeur = v)
}
# Durée de f() sous Rprof, et profil par fonction (total.time inclusif, en s).
profil <- function(f, interval = 0.01) {
  fic <- tempfile(); on.exit(unlink(fic))
  invisible(gc())
  Rprof(fic, interval = interval)
  t0 <- proc.time()[["elapsed"]]
  v <- f()
  duree <- proc.time()[["elapsed"]] - t0
  Rprof(NULL)
  s <- summaryRprof(fic)
  list(duree = duree, total = s$by.total, echantillonnage = s$sampling.time, valeur = v)
}
# Temps inclusif d'une fonction dans un profil (0 si absente : en deçà de la
# résolution de l'échantillonnage).
part <- function(p, ...) {
  for (fn in c(...)) { k <- paste0('"', fn, '"'); if (k %in% rownames(p$total)) return(p$total[k, "total.time"]) }
  0
}
loi_reference <- function() {
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
}
PLANS <- plans_reference()
TABLES_R8 <- tables_reference_r8()

# --- 1. classification ---------------------------------------------------------
if ("classification" %in% MESURES) {
  cat("\n== 1. Comparateur 2 d'importance_exclusions : classer_lignes_exclues() (#164, point 1)\n")
  # Taux par motif : ceux de tests/mesures/importance_exclusions.R, complétés
  # de contrat_duplique, sexe_hors_modalites et naissance_manquante ; `echelle`
  # multiplie tous les taux sauf celui de sexe_manquant sur les décès (0,5).
  declarations <- function(motifs, echelle = 1) {
    tous <- list(
      sexe_manquant = list(list(type = "sexe_manquant", cible = "deces", taux = 0.5),
                           list(type = "sexe_manquant", cible = c("rachat", "en_cours"), taux = 0.02 * echelle)),
      id_contrat_manquant = list(list(type = "id_contrat_manquant", cible = TOUTES, taux = 0.01 * echelle)),
      contrat_duplique = list(list(type = "contrat_duplique", cible = TOUTES, taux = 0.01 * echelle)),
      naissance_manquante = list(list(type = "naissance_manquante", cible = TOUTES, taux = 0.005 * echelle)),
      cause_hors_modalites = list(list(type = "cause_hors_modalites", cible = c("deces", "rachat"), taux = 0.005 * echelle)),
      sexe_hors_modalites = list(list(type = "sexe_hors_modalites", cible = TOUTES, taux = 0.005 * echelle)))
    unlist(tous[motifs], recursive = FALSE, use.names = FALSE)
  }
  M1 <- "id_contrat_manquant"
  M3 <- c(M1, "contrat_duplique", "sexe_manquant")
  M6 <- c(M3, "naissance_manquante", "cause_hors_modalites", "sexe_hors_modalites")
  CAS <- list(list(nom = "k=1", motifs = M1, echelle = 1), list(nom = "k=3", motifs = M3, echelle = 1),
              list(nom = "k=6", motifs = M6, echelle = 1),
              list(nom = "k=6, taux x10 (fort volume)", motifs = M6, echelle = 10))
  pf <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
  for (cas in CAS) {
    j <- tablexp::simuler_portefeuille(loi_reference(), pf, PERIODE_REFERENCE, GRAINE_REFERENCE,
                                       defauts = list(graine = GRAINE_DEFAUTS, declarations = declarations(cas$motifs, cas$echelle)))
    # Plan R6 (Kaplan-Meier compris) muni de l'étape validation de R8 : sans
    # elle le comparateur 2 n'est pas produit (non_demandee_par_le_plan).
    p <- PLANS$R6$plan
    p$etapes$donnees$exclusions <- cas$motifs
    p$etapes$validation <- PLANS$R8$plan$etapes$validation
    p$graine <- GRAINE_MONTE_CARLO_R8
    p_ie <- p; p_ie$etapes$importance_exclusions <- list(niveau = 0.95)
    lance <- function(plan) function() construire_table(j, ROLES_REFERENCE, plan, tables_reference = TABLES_R8)
    sans <- repetitions(lance(p)); avec <- repetitions(lance(p_ie))
    x <- avec$valeur
    rd <- x$etapes$donnees$resultat
    cat(sprintf("\n  [%s] %d lignes, %d exclues (%.1f %%) : %s ; unité %s ; comparateur 2 : %s ; %d calculs contrefactuels\n",
                cas$nom, nrow(j), rd$nombre_lignes_exclues, 100 * rd$nombre_lignes_exclues / nrow(j),
                paste(sprintf("%s %d", names(rd$lignes_exclues), lengths(rd$lignes_exclues)), collapse = ", "),
                rd$unite_observation, x$etapes$importance_exclusions$parametres$comparateurs$etat[2L],
                x$etapes$importance_exclusions$diagnostics$calculs_contrefactuels))
    cat(sprintf("  [%s] construire_table() sans l'étape : médiane %.2f s [%.2f ; %.2f] ; avec : médiane %.2f s [%.2f ; %.2f] ; écart des médianes %.2f s\n",
                cas$nom, sans$mediane, sans$min, sans$max, avec$mediane, avec$min, avec$max, avec$mediane - sans$mediane))
    # Appels, comptés sur une exécution à part (trace() ralentit : durée non retenue).
    ns <- asNamespace("tablexp")
    CPT <<- new.env(); CPT$classer <- 0L; CPT$retrait <- 0L
    suppressMessages({
      trace("classer_lignes_exclues", tracer = quote(CPT$classer <- CPT$classer + 1L), where = ns, print = FALSE)
      trace("retrait_contrefactuel", tracer = quote(CPT$retrait <- CPT$retrait + 1L), where = ns, print = FALSE)
    })
    invisible(construire_table(j, ROLES_REFERENCE, p_ie, tables_reference = TABLES_R8))
    suppressMessages({ untrace("classer_lignes_exclues", where = ns); untrace("retrait_contrefactuel", where = ns) })
    n_cl <- CPT$classer; n_rc <- CPT$retrait
    # Durée d'un appel direct, sur les lignes exclues réelles, aux clés `sexe`
    # (les appels du niveau A passent character(0) : effet négligeable sur l'ordre de grandeur).
    appel <- repetitions(function() { for (i in 1:200) tablexp:::classer_lignes_exclues(rd$lignes_exclues, rd$unite_observation, "sexe"); NULL }, 5L)
    par_appel <- appel$mediane / 200
    cat(sprintf("  [%s] appels : classer_lignes_exclues %d (2 au niveau des classes + 2(k+1) = %d du comparateur 2 + %d de retrait_contrefactuel, déduit et contrôlé égal au nombre mesuré d'appels de retrait_contrefactuel), retrait_contrefactuel %d ; un appel direct : %.2f ms (médiane de 5 x 200 appels) -> %d appels = %.3f s\n",
                cas$nom, n_cl, 2L * (length(cas$motifs) + 1L), n_cl - 2L - 2L * (length(cas$motifs) + 1L), n_rc,
                1000 * par_appel, n_cl, n_cl * par_appel))
    # Part par Rprof : REPETITIONS constructions avec l'étape dans un même profil.
    pr <- profil(function() { for (i in seq_len(REPETITIONS)) construire_table(j, ROLES_REFERENCE, p_ie, tables_reference = TABLES_R8); NULL })
    cat(sprintf("  [%s] Rprof (%d constructions avec l'étape, %.2f s échantillonnées, intervalle 10 ms) : calculer_importance_exclusions %.2f s (%.1f %%), retrait_contrefactuel %.2f s (%.1f %%), classer_lignes_exclues %.3f s (%.2f %%) ; par construction : %.2f s de %.2f s\n",
                cas$nom, REPETITIONS, pr$echantillonnage, part(pr, "calculer_importance_exclusions"),
                100 * part(pr, "calculer_importance_exclusions") / pr$echantillonnage, part(pr, "retrait_contrefactuel"),
                100 * part(pr, "retrait_contrefactuel") / pr$echantillonnage, part(pr, "classer_lignes_exclues"),
                100 * part(pr, "classer_lignes_exclues") / pr$echantillonnage,
                part(pr, "classer_lignes_exclues") / REPETITIONS, pr$echantillonnage / REPETITIONS))
    verifier(sprintf("[%s] répartition des appels 2 + 2(k+1) + retraits", cas$nom),
             n_cl == 2L + 2L * (length(cas$motifs) + 1L) + n_rc)
    rm(j, x, avec, sans, pr); invisible(gc())
  }
  verifier("mesure 1 (classification) exécutée", TRUE)
}

# --- 2. Wilcoxon ---------------------------------------------------------------
if ("wilcoxon" %in% MESURES) {
  cat("\n== 2. Statistique de Wilcoxon de statistiques_test() (#164, point 3)\n")
  j <- jeu_reference()
  plan_val <- function(B) {
    p <- PLANS$R8$plan
    p$etapes$exposition$par_annee <- TRUE
    p$etapes$brut$estimateurs <- "poisson"
    p$etapes$importance_exclusions <- NULL
    if (is.na(B)) { p$etapes$validation <- NULL; p$graine <- NULL } else p$etapes$validation$replications <- B
    p
  }
  lance <- function(B) function() construire_table(j$donnees, j$roles, plan_val(B),
                                                   tables_reference = if (is.na(B)) NULL else TABLES_R8)
  x <- lance(999)()
  cel <- x$etapes$validation$resultat$cellules
  ret <- cel$etat == "retenue"
  cat(sprintf("  validation : %d cellules (sexe x âge x année), %d retenues (C+), relation %s\n",
              nrow(cel), sum(ret), x$etapes$validation$parametres$relation_table))
  # Cellules retenues rangées par séquence (sexe x année) puis âge, comme
  # statistiques_validation().
  ann <- sort(unique(cel$annee))
  seq_id <- (match(cel$sexe, sort(unique(cel$sexe))) - 1L) * length(ann) + match(cel$annee, ann)
  o <- which(ret)[order(seq_id[ret], cel$age[ret], method = "radix")]
  A <- cel$attendus[o]; Ec <- cel$exposition_centrale[o]; mu <- cel$taux_table[o]; debut <- match(seq_id[o], seq_id[o])
  n <- length(o)
  taille <- max(1, floor(tablexp:::ELEMENTS_BLOC_REPLICATIONS / max(1, n)))
  sans <- repetitions(lance(NA))
  cat(sprintf("  construire_table() sans l'étape validation : médiane %.2f s [%.2f ; %.2f]\n", sans$mediane, sans$min, sans$max))
  direct <- list()
  for (B in c(999, 9999, 1e5)) {
    nrep <- if (B <= 9999) REPETITIONS else 1L
    avec <- repetitions(lance(B), nrep)
    # Profil d'une construction à part (Rprof ralentit un peu : la durée
    # retenue est celle de la ligne précédente).
    pr <- profil(lance(B))
    cat(sprintf("  replications = %6d : construire_table() avec l'étape validation : %s %.2f s%s ; écart à sans l'étape %.2f s\n",
                B, if (nrep > 1L) "médiane" else "une mesure", avec$mediane,
                if (nrep > 1L) sprintf(" [%.2f ; %.2f]", avec$min, avec$max) else "", avec$mediane - sans$mediane))
    cat(sprintf("  replications = %6d : Rprof d'une construction (%.2f s, %.2f s échantillonnées) : calculer_validation %.2f s (%.1f %%), replications_poisson %.2f s, rpois %.2f s, statistiques_test %.2f s, wilcoxon_rangs %.2f s (%.1f %% de la construction ; %.1f %% de calculer_validation)\n",
                B, pr$duree, pr$echantillonnage, part(pr, "calculer_validation"),
                100 * part(pr, "calculer_validation") / pr$echantillonnage, part(pr, "replications_poisson"),
                part(pr, "stats::rpois", "rpois"), part(pr, "statistiques_test"), part(pr, "wilcoxon_rangs"),
                100 * part(pr, "wilcoxon_rangs") / pr$echantillonnage,
                100 * part(pr, "wilcoxon_rangs") / max(part(pr, "calculer_validation"), 1e-9)))
    if (B <= 9999) {
      # Appel direct sur des matrices de la taille réelle (n x colonnes du bloc).
      set.seed(20261117L)
      t_tirage <- t_stat <- t_wil <- 0
      fait <- 0
      while (fait < B) {
        m <- min(taille, B - fait)
        t_tirage <- t_tirage + system.time(Dm <- matrix(stats::rpois(n * m, A), nrow = n))[["elapsed"]]
        t_stat <- t_stat + system.time(tablexp:::statistiques_test(Dm, A, Ec, mu, debut))[["elapsed"]]
        sg <- tablexp:::signe_residu(Dm - A, A); d <- Dm / Ec - mu; d[sg == 0] <- 0
        t_wil <- t_wil + system.time(vapply(seq_len(ncol(d)), function(jj) tablexp:::wilcoxon_rangs(d[, jj])$z, 0))[["elapsed"]]
        fait <- fait + m
      }
      direct[[as.character(B)]] <- c(tirage = t_tirage, stat = t_stat, wil = t_wil)
      cat(sprintf("  replications = %6d : appel direct (%d x %d, blocs de %d colonnes) : tirage %.2f s ; statistiques_test() entière %.2f s dont vapply de Wilcoxon %.2f s (%.1f %%) ; reste (statistiques vectorisées, préparation de d : déduit) %.2f s\n",
                  B, n, B, taille, t_tirage, t_stat, t_wil, 100 * t_wil / t_stat, t_stat - t_wil))
    } else {
      dd <- direct[["9999"]]; f <- B / 9999
      cat(sprintf("  replications = %6d : appel direct EXTRAPOLÉ linéairement en B depuis 9 999 (non exécuté) : tirage %.1f s ; statistiques_test() %.1f s dont Wilcoxon %.1f s (%.1f %%)\n",
                  B, f * dd[["tirage"]], f * dd[["stat"]], f * dd[["wil"]], 100 * dd[["wil"]] / dd[["stat"]]))
      cat(sprintf("  linéarité en B observée entre 999 et 9 999 (rapport des durées, rapport des B = %.2f) : statistiques_test() %.2f, Wilcoxon %.2f\n",
                  9999 / 999, direct[["9999"]][["stat"]] / direct[["999"]][["stat"]], direct[["9999"]][["wil"]] / direct[["999"]][["wil"]]))
    }
    rm(avec, pr); invisible(gc())
  }
  verifier("mesure 3 (Wilcoxon) exécutée", TRUE)
}
cat(sprintf("\ndurée totale du script : %.0f s\n", proc.time()[["elapsed"]] - T_DEBUT))
bilan()
