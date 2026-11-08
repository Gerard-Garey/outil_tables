# -----------------------------------------------------------------------------
#  tests/mesures/constat_positionnement.R : constat de simulation hors CI
#  (E2.19 : nombre de têtes, niveau, région, fausse alarme) des étapes
#  `positionnement` (méthode "smr"), `validation` sous la relation
#  `positionnee` (p = 1) et `homogeneite` (issues #167 et #168 ;
#  spécification d'actuary-approfondi sur #167, § 2, § 5, § 8 et § 10, et sur
#  #168, § 4 et § 7 ; étape (R) de B14, PR #173).
#
#  Hors des batteries : aucun seuil, verifier() ne contrôle que l'exécution.
#  Statut de chaque taux : constat de simulation ; son erreur de Monte-Carlo
#  (nombre R de jeux) est sqrt(p (1 - p) / R), à ne pas confondre avec
#  l'erreur d'estimation de chaque jeu (exposition, têtes et décès par
#  segment, résumés à part).
#
#  Trois lois vraies, une construction par jeu (simuler_portefeuille() puis
#  construire_table()), table de référence q~ = loi de Gompertz-Makeham du
#  jeu de référence (A = 2e-4, B = 2,5e-5, c = 1,1), H et F, âges 20 à 90 :
#   A. loi E2.19 (facteurs appliqués aux q : q_F = 0,6 q~) ; plan
#      `positionnement` ; couverture de theta par les intervalles exact et
#      asymptotique : H, cible 1 ; F, cible pseudo-vraie
#      sum E^c mu_F / sum E^c mu~, mu_F = -ln(1 - 0,6 q~), calculée sur
#      l'exposition centrale des cellules retenues de chaque jeu ;
#   B. loi sous l'échelle de la force (table du simulateur à deux sexes :
#      q_F = 1 - (1 - q~)^0,6, facteurs 1 ; mu_F = 0,6 mu~ exactement) ;
#      plan `positionnement` + `validation` (relation `positionnee`) ; H0 de
#      la validation exactement vraie ; taux de rejet à 5 % de X² et G²
#      (p asymptotiques à n - S ddl, p de Monte-Carlo multinomiales
#      conditionnelles) ; couverture de theta_H = 1 et theta_F = 0,6 en sus ;
#   C. loi commune aux deux sexes (facteurs 1, 1) ; plan `homogeneite`
#      (segmentation sexe) ; taux de rejet à 5 % de chaque test rendu
#      (global, tranches, somme des tranches ; p asymptotique, exacte et de
#      Monte-Carlo).
#  Un rejet est p <= ALPHA (1 + 1e-9) : la tolérance relative ne sert qu'à
#  compter p_MC = (1 + K)/(B + 1) = 10/200 sans dépendre de l'arrondi.
#
#  Graines : jeu r du plan k sous GRAINES_JEU[k] + r ; Monte-Carlo du même
#  jeu sous GRAINES_JEU[k] + DECALAGE_MC + r (graine du plan, distincte de
#  celle du jeu : les réplications ne réemploient pas les uniformes du jeu).
#  R est borné à moins de 10 000 jeux : au-delà, les graines de la loi A
#  rejoindraient celles de la loi B (GRAINES_JEU[["B"]] - GRAINES_JEU[["A"]]
#  = 10 000). Coïncidence connue : les jeux r = 411 à 416, 421, 431 et 441
#  de la loi A tirent sous 20261611 à 20261616, 20261621, 20261631 et
#  20261641, graines aussi de tests/unitaires/test_constats_brut.R ; elle est
#  sans effet statistique (deux expériences distinctes, sans lien entre
#  leurs conclusions), et ces graines sont gardées pour que la sortie
#  consignée reste rejouable (décision du mainteneur du 08/11/2026).
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise : hors d'une
#  locale UTF-8, le script refuse et sort en échec par bilan()) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/constat_positionnement.R [cle=valeur ...]
#  Clés : jeux (R, défaut 3000, pour chaque loi), tetes (n, défaut 2e4),
#  replications (B, défaut 199), lois (sous-ensemble de A,B,C, défaut A,B,C).
#  Choix par défaut (mesurés au pilote : environ 0,25 s par jeu, un cœur) :
#  n = 2e4 têtes donne environ 520 décès H et 340 décès F sur la plage
#  (base modeste, cellules de quelques décès) ; B = 199 rend (B + 1) alpha
#  entier, donc un niveau de Monte-Carlo exactement alpha sous H0 (B ne
#  change que la puissance) ; R = 3000 jeux par loi, erreur type 0,0040 sur
#  un taux de 0,05 ou de 0,95, pour environ 40 minutes au total.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
# Locale requise : les libellés du script et du package sont en UTF-8.
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  verifier("locale UTF-8 requise (l10n_info()[[\"UTF-8\"]])", FALSE)
  bilan()
}
installer_tablexp(RACINE)

# --- paramètres ----------------------------------------------------------------
ARGS <- commandArgs(trailingOnly = TRUE)
argument <- function(cle, defaut) {
  a <- grep(paste0("^", cle, "="), ARGS, value = TRUE)
  if (length(a)) sub("^[^=]*=", "", a[length(a)]) else defaut
}
R_JEUX <- as.integer(argument("jeux", "3000"))
N_TETES <- as.numeric(argument("tetes", "2e4"))
B_REPL <- as.numeric(argument("replications", "199"))
LOIS <- strsplit(argument("lois", "A,B,C"), ",", fixed = TRUE)[[1L]]
ALPHA <- 0.05
NIVEAU <- 0.95
PLAGE <- c(20, 90)
TRANCHES <- c(20, 50, 70)
GRAINES_JEU <- c(A = 20261200L, B = 20271200L, C = 20281200L)
DECALAGE_MC <- 500000L
stopifnot(R_JEUX < 10000L)  # au-delà, les graines de la loi A rejoignent celles de la loi B

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
PF <- list(mode = "uniforme", n = N_TETES, proportion_hommes = 0.5, age_min = 20, age_max = 75,
           origine = D("2005-01-01"))

gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
rachat <- list(table = ra, facteurs = c(H = 1, F = 1))
AGES <- PLAGE[1L]:PLAGE[2L]
q_ref <- gm$qx[match(AGES, gm$age)]
TABLES <- list(tv = table_reference(
  data.frame(age = rep(AGES, 2L), sexe = rep(c("H", "F"), each = length(AGES)), qx = rep(q_ref, 2L),
             stringsAsFactors = FALSE),
  nom = "Gompertz-Makeham du jeu de référence, H et F", statut = "importee",
  source = "tests/mesures/constat_positionnement.R : table_gompertz_makeham(2e-4, 2.5e-5, 1.1), âges 20 à 90"))
# Loi B : table du simulateur à deux sexes, force F = 0,6 force H à tout âge.
gm_force <- table_reference(
  data.frame(age = rep(gm$age, 2L), sexe = rep(c("H", "F"), each = nrow(gm)),
             qx = c(gm$qx, -expm1(0.6 * log1p(-gm$qx))), stringsAsFactors = FALSE),
  nom = "Gompertz-Makeham, F : force multipliée par 0,6", statut = "importee",
  source = "tests/mesures/constat_positionnement.R : q_F = 1 - (1 - q_GM)^0,6")
LOI <- list(A = list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)), rachat = rachat),
            B = list(deces = list(table = gm_force, facteurs = c(H = 1, F = 1)), rachat = rachat),
            C = list(deces = list(table = gm, facteurs = c(H = 1, F = 1)), rachat = rachat))
LIBELLE <- c(A = "loi E2.19 (q_F = 0,6 q~), positionnement",
             B = "loi sous la force (mu_F = 0,6 mu~), positionnement + validation positionnee",
             C = "loi commune (facteurs 1, 1), homogeneite")

etape_pos <- list(methode = "smr", table_reference = "tv", plage_ages = PLAGE, niveau = NIVEAU)
plan_loi <- function(k, graine_mc) {
  e <- list(donnees = list(exclusions = character(0)), exposition = list(convention = "plus_fine", par_annee = FALSE))
  if (k %in% c("A", "B")) e$positionnement <- etape_pos
  if (k == "B") e$validation <- list(table_reference = "tv", relation_table = "positionnee", plage_ages = PLAGE,
                                     tranches_age = TRANCHES, niveau = NIVEAU, replications = B_REPL)
  if (k == "C") e$homogeneite <- list(plage_ages = PLAGE, tranches_age = TRANCHES, replications = B_REPL)
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER, delai_declaration = 0,
            segmentation = "sexe")
  if (k %in% c("B", "C")) p$graine <- graine_mc
  p$etapes <- e
  p
}

# --- un jeu ----------------------------------------------------------------------
# Rend une liste plate de grandeurs du jeu r de la loi k ; les avertissements
# sont comptés (texte), jamais ignorés en silence.
AVERTIS <- new.env(); AVERTIS$n <- list()
un_jeu <- function(k, r) {
  gj <- GRAINES_JEU[[k]] + r
  noter <- function(w) {
    m <- substr(conditionMessage(w), 1L, 90L)
    AVERTIS$n[[m]] <- c(AVERTIS$n[[m]], 0L) [1L] + 1L
    invokeRestart("muffleWarning")
  }
  s <- withCallingHandlers(simuler_portefeuille(LOI[[k]], PF, PER, gj), warning = noter)
  x <- withCallingHandlers(construire_table(s, ROLES, plan_loi(k, gj + DECALAGE_MC),
                                            tables_reference = if (k == "C") NULL else TABLES),
                           warning = noter)
  o <- list()
  # valeur extraite de longueur 0 (ligne absente du résultat) : NA, pour que
  # chaque jeu rende les mêmes colonnes
  un <- function(v) if (length(v)) v else NA_real_
  tetes <- tapply(s$id_tete, s$sexe, function(v) length(unique(v)))
  for (sx in c("H", "F")) o[[paste0("tetes_", sx)]] <- unname(tetes[sx])
  if (k %in% c("A", "B")) {
    sm <- x$etapes$positionnement$resultat$smr
    cl <- x$etapes$positionnement$resultat$cellules
    for (sx in c("H", "F")) {
      i <- sm$sexe == sx
      c_ <- cl[cl$sexe == sx & cl$etat == "retenue", ]
      cible <- switch(k, A = if (sx == "H") 1 else
                        sum(c_$exposition_centrale * -log1p(-0.6 * c_$qx_table)) / sum(c_$exposition_centrale * c_$taux_table),
                      B = if (sx == "H") 1 else 0.6)
      v <- c(cible = cible, smr = sm$smr[i], O = sm$observes[i], A = sm$attendus[i], E = sum(c_$exposition_centrale),
             cellules = sm$cellules_retenues[i], inf_ex = sm$borne_inf_exacte[i], sup_ex = sm$borne_sup_exacte[i],
             inf_as = sm$borne_inf_asymptotique[i], sup_as = sm$borne_sup_asymptotique[i])
      for (nm in names(v)) o[[paste0(nm, "_", sx)]] <- unname(v[[nm]])
    }
  }
  if (k == "B") {
    st <- x$etapes$validation$resultat$statistiques
    for (t in c("khi2_pearson", "deviance")) for (g in c("ddl", "p_asymptotique", "p_monte_carlo"))
      o[[paste(t, g, sep = "|")]] <- un(st$valeur[st$statistique == t & st$grandeur == g])
    o$tirages_validation <- x$etapes$validation$resultat$monte_carlo$tirages_effectues
  }
  if (k == "C") {
    h <- x$etapes$homogeneite$resultat$tests
    nom <- ifelse(h$test == "tranche", sprintf("tranche %d-%d", h$tranche_debut, h$tranche_fin), h$test)
    for (i in seq_len(nrow(h))) for (g in c("ddl", "p_asymptotique", "p_exacte", "p_monte_carlo"))
      o[[paste(nom[i], g, sep = "|")]] <- un(h[[g]][i])
    seg <- x$etapes$homogeneite$resultat$segments
    for (sx in c("H", "F")) {
      i <- which(seg$portee == "global" & seg$sexe == sx)
      o[[paste0("O_", sx)]] <- seg$observes[i]; o[[paste0("E_", sx)]] <- seg$exposition_centrale[i]
    }
    o$tirages_homogeneite <- x$etapes$homogeneite$resultat$monte_carlo$jeux$tirages_effectues[1L]
  }
  o
}

# --- présentation ------------------------------------------------------------------
# Ligne de taux : cible nominale p0, R jeux où la grandeur est produite, taux
# observé, erreur type sqrt(p0 (1 - p0) / R), intervalle p0 ± 3 e.t., verdict ;
# erreur type du taux observé en dernière colonne.
ligne_taux <- function(mesure, p0, succes) {
  produit <- !is.na(succes); R <- sum(produit)
  if (!R) return(data.frame(mesure = mesure, cible = p0, R = 0L, non_produits = length(succes), taux = NA,
                            et_nominale = NA, intervalle = "", verdict = "non produit", et_observee = NA,
                            stringsAsFactors = FALSE))
  p <- mean(succes[produit]); e0 <- sqrt(p0 * (1 - p0) / R)
  data.frame(mesure = mesure, cible = p0, R = R, non_produits = sum(!produit), taux = round(p, 4),
             et_nominale = round(e0, 4), intervalle = sprintf("[%.4f ; %.4f]", p0 - 3 * e0, p0 + 3 * e0),
             verdict = if (abs(p - p0) <= 3 * e0) "dans l'intervalle" else if (p > p0) "hors (au-dessus)" else "hors (au-dessous)",
             et_observee = round(sqrt(p * (1 - p) / R), 4), stringsAsFactors = FALSE)
}
imprimer <- function(df) { o <- options(width = 250); on.exit(options(o)); print(df, row.names = FALSE, right = FALSE) }
resume <- function(v) sprintf("moyenne %.4g, min %.4g, max %.4g", mean(v), min(v), max(v))
rejet <- function(p) ifelse(is.na(p), NA, p <= ALPHA * (1 + 1e-9))

# --- exécution -----------------------------------------------------------------------
proc <- if (file.exists("/proc/cpuinfo")) sub("^.*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1L]) else NA
nproc <- suppressWarnings(tryCatch(system2("nproc", stdout = TRUE), error = function(e) NA_character_))
cat(sprintf("Plateforme : %s ; %s ; processeur %s ; cœurs disponibles (nproc) %s, détectés (detectCores) %d ; BLAS %s ; calcul séquentiel\n",
            R.version.string, R.version$platform, proc, nproc[1L], parallel::detectCores(), extSoftVersion()[["BLAS"]]))
cat(sprintf("Paramètres : R = %d jeux par loi, n = %.0f têtes par jeu, B = %.0f réplications, niveau %.2f, alpha %.2f, plage %d-%d, tranches %s, lois %s\n",
            R_JEUX, N_TETES, B_REPL, NIVEAU, ALPHA, PLAGE[1L], PLAGE[2L], paste(TRANCHES, collapse = ", "),
            paste(LOIS, collapse = ", ")))
cat(sprintf("Graines : jeu r de la loi k sous GRAINES_JEU[k] + r (%s), Monte-Carlo sous + %d\n",
            paste(names(GRAINES_JEU), GRAINES_JEU, sep = " = ", collapse = ", "), DECALAGE_MC))
t_debut <- proc.time()[["elapsed"]]

for (k in LOIS) {
  cat(sprintf("\n=== Loi %s : %s ===\n", k, LIBELLE[[k]]))
  t0 <- proc.time()[["elapsed"]]
  jeux <- vector("list", R_JEUX); erreurs <- character(0); refus <- character(0)
  # Refus du moteur (refus_tablexp, réponse documentée à un jeu sans issue,
  # par exemple un segment sans décès) comptés à part des erreurs R
  # (défaut du script ou du moteur) ; la classe refus_tablexp hérite
  # d'error (R/outils.R) : son gestionnaire est placé avant celui de error.
  for (r in seq_len(R_JEUX)) {
    jeux[[r]] <- tryCatch(un_jeu(k, r),
                          refus_tablexp = function(e) {
                            refus <<- c(refus, sprintf("jeu %d : %s (%s)", r, conditionMessage(e), e$motif)); NULL },
                          error = function(e) { erreurs <<- c(erreurs, sprintf("jeu %d : %s", r, conditionMessage(e))); NULL })
    if (r == 1L) cat(sprintf("Premier jeu : %.2f s ; durée extrapolée pour %d jeux : %.1f min\n",
                             proc.time()[["elapsed"]] - t0, R_JEUX, (proc.time()[["elapsed"]] - t0) * R_JEUX / 60))
  }
  duree <- proc.time()[["elapsed"]] - t0
  ok <- !vapply(jeux, is.null, NA)
  cat(sprintf("Durée : %.1f min (%.2f s par jeu) ; jeux refusés par le moteur : %d ; jeux en erreur : %d\n",
              duree / 60, duree / R_JEUX, length(refus), length(erreurs)))
  if (length(refus)) cat(paste("  refus :", utils::head(refus, 5L)), sep = "\n")
  if (length(erreurs)) cat(paste("  erreur :", utils::head(erreurs, 5L)), sep = "\n")
  if (sum(ok) == 0L) {
    verifier(sprintf("loi %s : aucun jeu exécuté", k), FALSE)
    bilan()
  }
  # colonnes : union des noms de tous les jeux exécutés, dans l'ordre de
  # première apparition ; une grandeur absente d'un jeu y vaut NA (non
  # produite dans les taux) et son absence est imprimée (contrôle de
  # présence), jamais ignorée en silence.
  noms_jeux <- lapply(jeux[ok], function(o) names(unlist(o)))
  noms <- unique(unlist(noms_jeux))
  absents <- vapply(noms, function(nm) sum(!vapply(noms_jeux, function(n) nm %in% n, NA)), 0L)
  if (any(absents > 0L))
    cat(sprintf("Grandeurs absentes d'une partie des jeux exécutés (NA) : %s\n",
                paste(sprintf("%s (%d jeu(x))", noms[absents > 0L], absents[absents > 0L]), collapse = " ; ")))
  m <- do.call(rbind, lapply(jeux[ok], function(o) { v <- unlist(o)[noms]; names(v) <- noms; v }))
  col <- function(nm) m[, nm]

  cat("\nErreur d'estimation (par jeu, sur les jeux exécutés) :\n")
  for (sx in c("H", "F")) {
    cat(sprintf("  %s : têtes %s\n", sx, resume(col(paste0("tetes_", sx)))))
    cat(sprintf("      décès O (plage) %s\n", resume(col(paste0("O_", sx)))))
    cat(sprintf("      exposition centrale E (plage, années) %s\n", resume(col(paste0("E_", sx)))))
    if (k %in% c("A", "B")) {
      th <- col(paste0("smr_", sx)); ci <- col(paste0("cible_", sx))
      cat(sprintf("      cellules retenues %s ; décès attendus sous q~ A %s\n",
                  resume(col(paste0("cellules_", sx))), resume(col(paste0("A_", sx)))))
      cat(sprintf("      cible theta %s ; theta^ %s\n", resume(ci), resume(th)))
      cat(sprintf("      écart-type de ln theta^ entre jeux %.4f ; 1/sqrt(O) moyen (asymptotique) %.4f ; moyenne de theta^ / cible - 1 : %.5f (e.t. MC %.5f)\n",
                  sd(log(th)), mean(1 / sqrt(col(paste0("O_", sx)))), mean(th / ci) - 1, sd(th / ci) / sqrt(nrow(m))))
    }
  }

  lignes <- list()
  if (k %in% c("A", "B")) {
    for (sx in c("H", "F")) {
      ci <- col(paste0("cible_", sx))
      cible_txt <- if (k == "A" && sx == "F") "pseudo-vraie (par jeu)" else format(ci[1L])
      for (b in c("ex", "as")) {
        inf <- col(paste0("inf_", b, "_", sx)); sup <- col(paste0("sup_", b, "_", sx))
        lignes[[length(lignes) + 1L]] <- ligne_taux(
          sprintf("couverture theta %s (cible %s), intervalle %s", sx, cible_txt, if (b == "ex") "exact" else "asymptotique"),
          NIVEAU, inf <= ci & ci <= sup)
        lignes[[length(lignes) + 1L]] <- ligne_taux(
          sprintf("  dont cible sous la borne inférieure, %s %s", sx, if (b == "ex") "exact" else "asymptotique"),
          (1 - NIVEAU) / 2, ci < inf)
      }
    }
  }
  if (k == "B") {
    cat(sprintf("\nValidation positionnee : ddl X² %s ; tirages de Monte-Carlo %s\n",
                resume(col("khi2_pearson|ddl")), resume(col("tirages_validation"))))
    for (t in c("khi2_pearson", "deviance")) for (g in c("p_asymptotique", "p_monte_carlo"))
      lignes[[length(lignes) + 1L]] <- ligne_taux(sprintf("taille à 5 %%, validation positionnee, %s, %s", t, g),
                                                  ALPHA, rejet(col(paste(t, g, sep = "|"))))
  }
  if (k == "C") {
    tests <- unique(sub("\\|.*$", "", grep("\\|p_", colnames(m), value = TRUE)))
    cat(sprintf("\nHomogénéité : tirages de Monte-Carlo %s\n", resume(col("tirages_homogeneite"))))
    for (t in tests) {
      cat(sprintf("  ddl %s : %s\n", t, resume(col(paste(t, "ddl", sep = "|")))))
      for (g in c("p_asymptotique", "p_exacte", "p_monte_carlo"))
        lignes[[length(lignes) + 1L]] <- ligne_taux(sprintf("taille à 5 %%, homogeneite, %s, %s", t, g),
                                                    ALPHA, rejet(col(paste(t, g, sep = "|"))))
    }
  }
  cat("\nTaux (constats de simulation ; R = jeux exécutés où la grandeur est produite, jeux refusés exclus (voir le décompte ci-dessus) ; e.t. nominale = sqrt(cible (1 - cible) / R), intervalle = cible ± 3 e.t.) :\n")
  imprimer(do.call(rbind, lignes))
  verifier(sprintf("loi %s : %d jeux exécutés", k, sum(ok)), sum(ok) > 0L)
}

cat("\nAvertissements reçus (texte tronqué, décompte sur toutes les lois) :\n")
if (length(AVERTIS$n)) for (nm in names(AVERTIS$n)) cat(sprintf("  %6d  %s\n", AVERTIS$n[[nm]], nm)) else cat("  aucun\n")
cat(sprintf("\nDurée totale : %.1f min\n", (proc.time()[["elapsed"]] - t_debut) / 60))
bilan()
