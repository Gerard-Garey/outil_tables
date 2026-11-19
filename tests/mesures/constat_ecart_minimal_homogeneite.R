# -----------------------------------------------------------------------------
#  tests/mesures/constat_ecart_minimal_homogeneite.R : constat court de
#  simulation hors CI de l'approximation de l'écart minimal détectable
#  theta_80 du test d'homogénéité entre segments (étape `homogeneite`,
#  table resultat/ecart_minimal ; issue #218, critère 4, règle fixée le
#  11/11/2026 avant tout passage ; spécification d'actuary-approfondi,
#  commentaire 7012205013 ; B30, étape (3)).
#
#  Question : theta_80 est défini par la loi du khi-deux non centrale de
#  non-centralité lambda_s(theta) = delta' V^- delta, V étant la variance de
#  U sous H0 ; sous l'alternative la variance de U diffère de V. Le constat
#  mesure la puissance réelle du test en theta_80, sous le modèle de Poisson
#  à exposition fixée, en strate unique.
#
#  Modèle (critère 4) : strate unique, S segments, un déviant de part pi (le
#  dernier), les autres de parts égales (1 - pi) / (S - 1) ;
#  conditionnellement à D, O ~ Multinomiale(D ; p(theta)), p_j(theta) =
#  pi_j w_j / somme_k pi_k w_k, w = theta pour le déviant, 1 sinon ;
#  T = somme_j (O_j - D pi_j)^2 / (D pi_j) (en strate unique, égale à
#  U' V^- U) ; rejet si T >= quantile 0,95 du khi-deux à S - 1 ddl.
#  Grille, cellules i = 1 à 8 dans cet ordre : (S ; pi) dans (4 ; 0,25),
#  (8 ; 0,125), (4 ; 0,05), (8 ; 0,02), et pour chacun la cible 1,25 puis
#  0,80. D = arrondi de lambda_80 / (pi (1 - pi) r_c^2), r_c = |c - 1| /
#  (1 + pi (c - 1)), lambda_80 à S - 1 ddl ; theta_80 : forme close
#  recalculée avec D arrondi, r = sqrt(lambda_80 / (D pi (1 - pi))),
#  theta+ = 1 + r / (1 - pi r) (cible 1,25), theta- = 1 - r / (1 + pi r)
#  (cible 0,80). N = 400 000 tirages par point ; par cellule, dans cet
#  ordre : theta_80, theta_80 + h, theta_80 - h, theta = 1, h = 0,02 du côté
#  de la cible (h = +0,02 pour la cible 1,25, -0,02 pour la cible 0,80).
#
#  Identité (verifier(), un échec fait échouer le script) : pour chaque
#  cellule, un jeu d'agrégats à parts constantes (un âge, exposition
#  centrale E_j = 10^6 pi_j, décès arrondis de D pi_j sommant à D) passe par
#  construire_table() (recette agrégats, étape homogeneite) ; theta_80 du
#  déviant (branche de la cible) égal à la forme close du script à 1e-8 en
#  relatif. Un refus du moteur est consigné comme question pour actuary,
#  déclarée, et fait échouer le script.
#
#  Règle de lecture (critère 4, FIXÉE LE 11/11/2026, AVANT TOUT PASSAGE ;
#  tout changement après essai se déclare et revient au mainteneur, M383,
#  M398) :
#   (K1) pour chaque cellule, puissance simulée en theta_80 dans
#        [0,76 ; 0,84] : « borne tenue » ; sinon « borne dépassée », cellules
#        nommées (retour à actuary et au mainteneur, issue non close) ;
#   (K2) puissance sous 0,80 pour la cible 1,25 et au-dessus pour la cible
#        0,80, à plus de 3 erreurs types de Monte-Carlo : « sens conforme » ;
#        sinon « sens non confirmé », cellules nommées ;
#   descriptif, sans verdict : taille en theta = 1, pente
#   (P(theta_80 + h) - P(theta_80 - h)) / (2 h), theta_80 « vrai » interpolé
#   theta_80 + (0,80 - P(theta_80)) / pente. Jamais « établie ». Un seul
#   passage.
#
#  Statut : constat de simulation. Erreur de Monte-Carlo due à N (e.t.
#  sqrt(P (1 - P) / N), environ 0,0006 sur une puissance de 0,80), distincte
#  de l'erreur d'estimation (sans objet ici : D est fixé). Porte sur
#  l'approximation sous le modèle de Poisson à exposition fixée, en strate
#  unique ; ne couvre ni le modèle de survie, ni des strates de parts
#  différentes.
#
#  Graines (critère 4 ; plage [21 991 200 ; 21 991 299] acceptée par le
#  mainteneur, M412 (3), déclarée dans tests/controles/graines_admises.csv) :
#  tirages de la cellule i sous GRAINE_BASE + i (i = 1 à 8), générateur
#  Mersenne-Twister / Inversion / Rejection, posé avant les quatre points de
#  la cellule ; graine du plan de l'identité de la cellule i (réplications
#  de Monte-Carlo du moteur, non lues) : GRAINE_BASE + DECALAGE_PLAN + i,
#  DECALAGE_PLAN = 50. Les graines 700000001 et 700000002 des étais ne sont
#  pas réemployées. Aucune autre source d'aléa.
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/constat_ecart_minimal_homogeneite.R
#  Durée attendue : moins d'une minute. Passage consigné en commentaire de
#  la PR.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
if (!isTRUE(l10n_info()[["UTF-8"]])) {
  verifier("locale UTF-8 requise (l10n_info()[[\"UTF-8\"]])", FALSE)
  bilan()
}
installer_tablexp(RACINE)

# --- paramètres (critère 4) ----------------------------------------------------
N_TIRAGES <- 400000L
ALPHA <- 0.05
PUISSANCE <- 0.80
H_PAS <- 0.02
BORNES_K1 <- c(0.76, 0.84)
Z_K2 <- 3
GRAINE_BASE <- 21991200L
DECALAGE_PLAN <- 50L
GRILLE <- do.call(rbind, lapply(list(c(4, 0.25), c(8, 0.125), c(4, 0.05), c(8, 0.02)), function(sp)
  data.frame(S = as.integer(sp[1L]), pi = sp[2L], cible = c(1.25, 0.80))))
GRILLE$i <- seq_len(nrow(GRILLE))
stopifnot(nrow(GRILLE) == 8L)

lambda_80 <- function(ddl) {
  q <- stats::qchisq(1 - ALPHA, ddl)
  stats::uniroot(function(l) stats::pchisq(q, ddl, ncp = l, lower.tail = FALSE) - PUISSANCE, c(0, 200),
                 tol = 1e-13)$root
}
parts <- function(S, pi) c(rep((1 - pi) / (S - 1), S - 1L), pi)
GRILLE$lambda_80 <- GRILLE$D <- GRILLE$theta_80 <- GRILLE$h <- NA_real_
for (k in seq_len(nrow(GRILLE))) {
  g <- GRILLE[k, ]
  l80 <- lambda_80(g$S - 1L)
  rc <- abs(g$cible - 1) / (1 + g$pi * (g$cible - 1))
  D <- round(l80 / (g$pi * (1 - g$pi) * rc^2))
  r <- sqrt(l80 / (D * g$pi * (1 - g$pi)))
  GRILLE$lambda_80[k] <- l80
  GRILLE$D[k] <- D
  GRILLE$theta_80[k] <- if (g$cible > 1) 1 + r / (1 - g$pi * r) else 1 - r / (1 + g$pi * r)
  GRILLE$h[k] <- H_PAS * sign(g$cible - 1)
}

# --- T0 : consignation -----------------------------------------------------------
proc <- if (file.exists("/proc/cpuinfo")) sub("^.*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1L]) else NA
arbre <- suppressWarnings(tryCatch(system2("git", c("-C", RACINE, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
                                   error = function(e) NA_character_))
modifs <- suppressWarnings(tryCatch(system2("git", c("-C", RACINE, "status", "--porcelain"), stdout = TRUE, stderr = FALSE),
                                    error = function(e) NA_character_))
cat("\n=== T0 : consignation ===\n")
cat(sprintf("Plateforme : %s ; %s ; processeur %s ; BLAS %s ; LAPACK %s ; calcul séquentiel\n",
            R.version.string, R.version$platform, proc, extSoftVersion()[["BLAS"]], La_library()))
cat(sprintf("Arbre : %s ; fichiers modifiés ou non suivis (git status --porcelain) : %d\n",
            if (length(arbre)) arbre[1L] else NA, length(modifs)))
cat(sprintf("Empreintes MD5 : script %s ; tests/controles/graines_admises.csv %s\n",
            unname(tools::md5sum(file.path(RACINE, "tests", "mesures", "constat_ecart_minimal_homogeneite.R"))),
            unname(tools::md5sum(file.path(RACINE, "tests", "controles", "graines_admises.csv")))))
cat(sprintf("tablexp %s ; constantes du moteur : niveau %.2f, puissance %.2f, seuil %.2f\n",
            as.character(utils::packageVersion("tablexp")), tablexp:::NIVEAU_ECART_MINIMAL,
            tablexp:::PUISSANCE_ECART_MINIMAL, tablexp:::SEUIL_ECART_MINIMAL))
cat(sprintf("Paramètres : N = %d tirages par point ; alpha = %.2f ; h = %.2f du côté de la cible ; K1 [%.2f ; %.2f] ; K2 à %d e.t. ; graines %d + i (tirages), %d + %d + i (plans de l'identité)\n",
            N_TIRAGES, ALPHA, H_PAS, BORNES_K1[1L], BORNES_K1[2L], Z_K2, GRAINE_BASE, GRAINE_BASE, DECALAGE_PLAN))
verifier("paramètres du script égaux aux constantes du moteur (niveau, puissance)",
         identical(ALPHA, tablexp:::NIVEAU_ECART_MINIMAL) && identical(PUISSANCE, tablexp:::PUISSANCE_ECART_MINIMAL))
imprimer <- function(df) { o <- options(width = 250); on.exit(options(o)); print(df, row.names = FALSE, right = FALSE) }
cat("Grille :\n")
imprimer(data.frame(i = GRILLE$i, S = GRILLE$S, pi = GRILLE$pi, cible = GRILLE$cible,
                    lambda_80 = sprintf("%.9f", GRILLE$lambda_80), D = GRILLE$D,
                    theta_80 = sprintf("%.6f", GRILLE$theta_80), h = GRILLE$h, graine = GRAINE_BASE + GRILLE$i))
t_debut <- proc.time()[["elapsed"]]

# --- T1 : identité moteur / forme close ------------------------------------------------
cat("\n=== T1 : identité theta_80 du moteur (construire_table(), recette agrégats) = forme close du script ===\n")
PER <- list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31"))
ROLES_AG <- c(age = "age", exposition_centrale = "exposition_centrale", exposition_initiale = "exposition_initiale",
              evenements = "evenements", segment = "segment")
plan_identite <- function(graine)
  list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER, delai_declaration = 0,
       segmentation = "segment", graine = graine,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "dernier_anniversaire", par_annee = FALSE),
                     homogeneite = list(plage_ages = c(60, 60), tranches_age = 60, replications = 9)))
questions <- character(0)
id_lignes <- list()
for (k in seq_len(nrow(GRILLE))) {
  g <- GRILLE[k, ]
  pj <- parts(g$S, g$pi)
  d <- floor(g$D * pj); reste <- g$D - sum(d)
  if (reste > 0) d[order(-(g$D * pj - d))[seq_len(reste)]] <- d[order(-(g$D * pj - d))[seq_len(reste)]] + 1
  ag <- data.frame(age = 60L, exposition_centrale = 1e6 * pj, exposition_initiale = 1e6 * pj, evenements = as.integer(d),
                   segment = sprintf("s%d", seq_len(g$S)), stringsAsFactors = FALSE)
  x <- tryCatch(construire_table(ag, ROLES_AG, plan_identite(GRAINE_BASE + DECALAGE_PLAN + g$i)),
                refus_tablexp = function(e) e)
  if (inherits(x, "refus_tablexp")) {
    questions <- c(questions, sprintf("cellule %d : refus du moteur (%s) ; question pour actuary", g$i, conditionMessage(x)))
    next
  }
  em <- x$etapes$homogeneite$resultat$ecart_minimal
  th <- if (g$cible > 1) em$theta_80_superieur[g$S] else em$theta_80_inferieur[g$S]
  et <- if (g$cible > 1) em$etat_superieur[g$S] else em$etat_inferieur[g$S]
  id_lignes[[k]] <- data.frame(i = g$i, etat = et, theta_moteur = sprintf("%.10f", th),
                               theta_forme_close = sprintf("%.10f", g$theta_80), ecart_relatif = sprintf("%.2e", th / g$theta_80 - 1))
  verifier(sprintf("T1 cellule %d (S = %d, pi = %.3f, cible %.2f) : theta_80 du moteur = forme close à 1e-8 en relatif", g$i, g$S, g$pi, g$cible),
           identical(et, "produite") && isTRUE(abs(th / g$theta_80 - 1) <= 1e-8))
}
if (length(id_lignes)) imprimer(do.call(rbind, id_lignes))
verifier("T1 : aucun refus du moteur sur les agrégats à parts constantes (sinon question pour actuary)",
         if (length(questions)) paste(questions, collapse = " ; ") else TRUE)

# --- T2 : tirages multinomiaux ----------------------------------------------------------
cat("\n=== T2 : puissance simulée (multinomiale, N tirages par point) ===\n")
puissance <- function(D, pj, theta, n) {
  w <- c(rep(1, length(pj) - 1L), theta)
  p <- pj * w / sum(pj * w)
  O <- stats::rmultinom(n, D, p)
  tt <- colSums((O - D * pj)^2 / (D * pj))
  mean(tt >= stats::qchisq(1 - ALPHA, length(pj) - 1L))
}
res <- list()
for (k in seq_len(nrow(GRILLE))) {
  g <- GRILLE[k, ]
  pj <- parts(g$S, g$pi)
  set.seed(GRAINE_BASE + g$i, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  th <- c(g$theta_80, g$theta_80 + g$h, g$theta_80 - g$h, 1)
  P <- vapply(th, function(t) puissance(g$D, pj, t, N_TIRAGES), 0)
  res[[k]] <- data.frame(i = g$i, P_theta80 = P[1L], P_plus_h = P[2L], P_moins_h = P[3L], P_theta1 = P[4L])
}
res <- do.call(rbind, res)
res$et <- sqrt(res$P_theta80 * (1 - res$P_theta80) / N_TIRAGES)
res$et_taille <- sqrt(res$P_theta1 * (1 - res$P_theta1) / N_TIRAGES)
res$pente <- (res$P_plus_h - res$P_moins_h) / (2 * GRILLE$h)
res$theta_80_vrai <- GRILLE$theta_80 + (PUISSANCE - res$P_theta80) / res$pente
res$z_sens <- ifelse(GRILLE$cible > 1, (PUISSANCE - res$P_theta80) / res$et, (res$P_theta80 - PUISSANCE) / res$et)
f4 <- function(v) sprintf("%.4f", v)
imprimer(data.frame(i = res$i, S = GRILLE$S, pi = GRILLE$pi, cible = GRILLE$cible, theta_80 = sprintf("%.6f", GRILLE$theta_80),
                    P_theta80 = f4(res$P_theta80), et = f4(res$et), P_plus_h = f4(res$P_plus_h), P_moins_h = f4(res$P_moins_h),
                    taille_theta1 = f4(res$P_theta1), et_taille = f4(res$et_taille), pente = sprintf("%.3f", res$pente),
                    theta_80_vrai = sprintf("%.5f", res$theta_80_vrai), ecart_theta = sprintf("%+.5f", res$theta_80_vrai - GRILLE$theta_80),
                    z_sens = sprintf("%.1f", res$z_sens)))

# --- T3 : lecture (K1, K2) ---------------------------------------------------------------
cat("\n=== T3 : lecture (règle fixée le 11/11/2026) ===\n")
nom_cellule <- function(k) sprintf("%d (S = %d, pi = %.3f, cible %.2f)", GRILLE$i[k], GRILLE$S[k], GRILLE$pi[k], GRILLE$cible[k])
hors_k1 <- which(res$P_theta80 < BORNES_K1[1L] | res$P_theta80 > BORNES_K1[2L])
sens_ko <- which(!(res$z_sens > Z_K2))
cat(sprintf("K1 : %s\n", if (!length(hors_k1)) sprintf("borne tenue (puissance simulée en theta_80 dans [%.2f ; %.2f] pour les 8 cellules)", BORNES_K1[1L], BORNES_K1[2L])
                         else paste("borne dépassée ; cellules :", paste(vapply(hors_k1, nom_cellule, ""), collapse = ", "))))
cat(sprintf("K2 : %s\n", if (!length(sens_ko)) sprintf("sens conforme (sous 0,80 pour la cible 1,25, au-dessus pour la cible 0,80, à plus de %d erreurs types de Monte-Carlo, 8 cellules)", Z_K2)
                         else paste("sens non confirmé ; cellules :", paste(vapply(sens_ko, nom_cellule, ""), collapse = ", "))))
cat("Descriptif, sans verdict : taille en theta = 1, pente, theta_80 « vrai » interpolé (tableau T2).\n")
cat("Statut : constat de simulation sous le modèle de Poisson à exposition fixée, en strate unique ; erreur de Monte-Carlo due à N, distincte de l'erreur d'estimation ; ni le modèle de survie ni des strates de parts différentes ne sont couverts. Aucune conclusion « établie ».\n")
cat(sprintf("Durée des tirages : %.1f s\n", proc.time()[["elapsed"]] - t_debut))
bilan()
