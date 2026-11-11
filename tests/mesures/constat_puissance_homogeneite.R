# -----------------------------------------------------------------------------
#  tests/mesures/constat_puissance_homogeneite.R : constat de simulation hors
#  CI de la puissance du test d'homogénéité entre segments (étape
#  `homogeneite`, #168) sur bases modestes, selon l'effectif, l'écart relatif
#  et le nombre de segments (issue #184, E2.15 ; spécification (0')(f)
#  d'actuary-approfondi, commentaire 6775962003 de #184, acceptée par le
#  mainteneur le 10/11/2026, M398 ; étape (5) de B18, PR #208).
#
#  Question (#184) : le constat de B14 (constat_positionnement.R, loi C)
#  mesure la taille du test sous H0 à deux segments ; il ne mesure pas sa
#  puissance. Sur une base modeste, un écart réel entre segments peut passer
#  inaperçu, et un non-rejet ne vaut pas homogénéité établie.
#
#  Plan (spécification, § 1). Loi commune : décès Gompertz-Makeham
#  (A = 2e-4, B = 2,5e-5, c = 1,1), rachat constant 5 % ; portefeuille
#  `uniforme`, entrée 20 à 75 ans, origine 2005-01-01, période 2015-2024,
#  proportion_hommes = 1, une tête par contrat ; un portefeuille simulé par
#  segment (forme `agregats` du simulateur, convention
#  `dernier_anniversaire`, par_annee FALSE), concaténés ; rôles age,
#  exposition_centrale, exposition_initiale, evenements, segment ; plan
#  segmentation = "segment", étape `homogeneite` âges 20 à 90, tranches 20,
#  50, 70, replications = B = 199 ; alpha = 0,05, rejet si
#  p <= alpha (1 + 1e-9). Alternative multiplicative sur la force :
#  q_s(x) = 1 - (1 - q~(x))^theta_s(x), donc mu_s = theta_s mu~ exactement
#  (force constante par âge entier, R/simulation.R) ; le segment déviant est
#  toujours le dernier, les autres à theta = 1.
#    P : n = 2 000 (theta 1 ; 1,10 ; 1,25 ; 1,50 ; 2,00), 8 000 (1 ; 1,10 ;
#        1,25 ; 1,50), 32 000 (1 ; 1,05 ; 1,10 ; 1,25) ; S = 2, 4, 8, parts
#        égales (39 cellules) ;
#    U : S = 2, parts 0,9 / 0,1, déviant = petit segment ; n = 2 000 (1 ;
#        1,5 ; 2), 8 000 (1 ; 1,25 ; 1,5), 32 000 (1 ; 1,10 ; 1,25)
#        (9 cellules) ;
#    C : S = 2, parts égales, n = 2 000, 8 000, 32 000 ; tau = 1,25 et 1,50 :
#        force du déviant x tau avant 70 ans, x 1/tau ensuite (6 cellules).
#  54 cellules ; n est le nombre total de têtes du bloc. Nombres aléatoires
#  communs par bloc (P : n x S ; U : n ; C : n) : chaque segment d'un bloc
#  est tiré sous la même graine pour toutes les cellules (theta, tau) du
#  bloc ; R jeux par cellule (défaut 1 000).
#
#  Grandeurs et statuts (spécification, § 2). Trois erreurs nommées, jamais
#  confondues : (1) erreur de Monte-Carlo du constat, due au nombre R de
#  jeux : e.t. sqrt(p (1 - p) / R) d'un taux ; (2) erreur de Monte-Carlo de
#  chaque p de Monte-Carlo, due au nombre B de réplications (colonne
#  erreur_monte_carlo du moteur, résumée par cellule) ; (3) erreur
#  d'estimation de chaque jeu (exposition, décès par segment ; T2). Taux de
#  rejet, écarts et biais groupés : constats de simulation. Prédiction
#  pi_r (hors moteur, par jeu) : pi_sc = E_sc / E_.c ;
#  p^alt_sc = E_sc mu_sc / somme_s' E_s'c mu_s'c ;
#  delta = somme_c D_.c (p^alt_c - pi_c) sur les strates informatives ;
#  V réduite (un segment de référence par composante connexe) ;
#  lambda = delta' V^-1 delta ; pi_r = P(khi2_ddl(lambda) > khi2_ddl,1-alpha).
#  E[U | marges] = delta est exact sous le modèle de Poisson à E fixée ; la
#  loi du khi-deux non centrale est une approximation asymptotique
#  (alternatives proches), dont le constat mesure la qualité (aucune source
#  citée, M388). Somme des tranches : lambda et ddl sommés sur les tranches.
#  Sections de la sortie : T0 consignation ; T1 identités ; T2 erreur
#  d'estimation ; T3 taille (theta = 1) ; T4 puissance et prédiction ;
#  T5 pertes appariées ; T6 écart minimal détectable theta_80 ; T7 verdicts ;
#  T8 avertissements, refus et erreurs.
#
#  Règle de lecture (spécification, § 3), FIXÉE LE 10/11/2026, AVANT TOUT JEU
#  DE LA PLAGE ; tout changement après essai se déclare et revient au
#  mainteneur (M383, M398) :
#   (L1) taille, par famille (theta = 1) : par cellule, global asymptotique
#        (bilatéral), global exact si S = 2 (unilatéral supérieur), global
#        Monte-Carlo (unilatéral supérieur), somme des tranches asymptotique
#        (bilatéral), somme Monte-Carlo (unilatéral supérieur) ; 39 lignes
#        (P), 15 (U) ; test binomial exact (binom.test) contre alpha, Holm à
#        5 % dans la famille ; libellés « taille compatible avec α (aucun
#        écart détecté, risque global de 5 %) » ou « écart de taille
#        détecté » (lignes et sens) ;
#   (L2) prédiction, familles P (30 cellules à theta > 1) et U (6)
#        séparément, global asymptotique : (a) par cellule, p
#        Poisson-binomiale exacte bilatérale du nombre de rejets contre les
#        pi_r (méthode de binom.test : somme des probabilités <= celle de
#        l'observé, tolérance relative 1e-7), Holm à 5 % ; (b) biais groupé
#        sur les cellules à pi-barre dans [0,10 ; 0,90] :
#        b^ = moyenne sur r de (1/K_r) somme_c (I_rc - pi_rc) (K_r cellules
#        où le test est produit au jeu r), e.t. sd_r / sqrt(R), IC à 95 %
#        (quantile normal) ; libellés « compatible avec la prédiction
#        asymptotique (aucun écart détecté, risque global de 5 % ; biais
#        groupé dans ±0,02) » si (a) ne détecte rien et l'IC de (b) est dans
#        [-0,02 ; 0,02] ; « écart détecté » si (a) détecte une cellule ;
#        « biais groupé non exclu » sinon ; jamais « établie » ;
#   (L3) descriptif sans verdict : somme des tranches face à sa prédiction,
#        famille C, tranches, T5, T6.
#  Aucun risque global entre familles ni entre L1 et L2.
#
#  Identités (verifier(), un échec fait échouer le script ; spécification,
#  C5) : T recalculée hors moteur (à partir des agrégats d'entrée) égale à
#  celle du moteur à 1e-10 près en relatif (|écart| <= 1e-10 max(1, |T|) ;
#  global, chaque tranche, somme), ddl égaux, sur chaque test produit de
#  chaque jeu ; pi_r = alpha à 1e-12 près à theta = 1 ; jeux 1 à 10 de
#  P(2 000, S = 2, theta = 1,25) et P(8 000, S = 4, theta = 1,50) (famille
#  P) : forme en lignes du simulateur (mêmes graines, convention
#  `dernier_anniversaire`) et forme agrégats donnent des décès identiques
#  par (segment, âge), une exposition centrale et une statistique (global,
#  tranches, somme) égales à 1e-10 près en relatif.
#
#  Graines (spécification, § 5 ; M398, option (a) ; plage déclarée dans
#  tests/controles/graines_admises.csv) : graine(k, r) = GRAINE_BASE +
#  10 000 k + r, r de 1 à 9 999 ; flux de jeux k = 1 à 54, un par segment de
#  bloc (blocs P par n puis S, puis U, puis C ; segments dans l'ordre, déviant
#  en dernier) ; flux de Monte-Carlo k = 60 + i, i = 1 à 54, cellules dans
#  l'ordre du plan (graine du plan de la construction). Plage
#  [20 841 200 ; 21 991 199]. Aucune autre source d'aléa.
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise : hors d'une
#  locale UTF-8, le script refuse et sort en échec par bilan()) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/constat_puissance_homogeneite.R [cle=valeur ...]
#  Clés : jeux (R, défaut 1000, de 2 à 9999), familles (sous-ensemble de
#  P,U,C, défaut P,U,C, exécutées dans cet ordre ; un passage par famille
#  possible, sections identiques), sortie (facultative : préfixe de
#  fichiers .rds où sont écrites, famille par famille, les grandeurs brutes
#  par jeu, hors dépôt). B = 199 est une constante.
#  Durées (passage par familles au-delà de 100 min extrapolées, chaque
#  passage sous 90 min, M398) : durée extrapolée imprimée après le premier
#  jeu de chaque famille, durée réelle en fin de famille ; estimation de la
#  spécification : P environ 80 min, U + C environ 32 min à R = 1 000.
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
# Clés admises seulement : une faute de frappe ne lance pas silencieusement
# toutes les familles (constat mineur 3 de l'audit).
local({
  cles <- sub("=.*$", "", ARGS)
  inconnues <- setdiff(cles, c("jeux", "familles", "sortie"))
  if (length(inconnues) || !all(grepl("=", ARGS, fixed = TRUE)))
    stop(sprintf("argument inconnu ou sans « = » : %s (clés admises : jeux, familles, sortie)",
                 paste(ARGS[!grepl("^(jeux|familles|sortie)=", ARGS)], collapse = ", ")))
})
argument <- function(cle, defaut) {
  a <- grep(paste0("^", cle, "="), ARGS, value = TRUE)
  if (length(a)) sub("^[^=]*=", "", a[length(a)]) else defaut
}
R_JEUX <- as.integer(argument("jeux", "1000"))
stopifnot(!is.na(R_JEUX), R_JEUX >= 2L, R_JEUX <= 9999L)  # r <= 9 999 : flux disjoints (pas de 10 000)
FAMILLES <- strsplit(argument("familles", "P,U,C"), ",", fixed = TRUE)[[1L]]
stopifnot(length(FAMILLES) > 0L, all(FAMILLES %in% c("P", "U", "C")), !anyDuplicated(FAMILLES))
FAMILLES <- intersect(c("P", "U", "C"), FAMILLES)
SORTIE <- argument("sortie", "")
B_REPL <- 199
ALPHA <- 0.05
PLAGE <- c(20, 90)
TRANCHES <- c(20, 50, 70)
AGE_BASCULE_C <- 70
MARGE_BIAIS <- 0.02
PI_BORNES <- c(0.10, 0.90)
GRAINE_BASE <- 20841200L
PAS_FLUX <- 10000L
DECALAGE_FLUX_MC <- 60L
graine_flux <- function(k, r) GRAINE_BASE + PAS_FLUX * as.integer(k) + as.integer(r)

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
FORME <- list(nom = "agregats", transition = list(depart = "vivant", arrivee = "deces"),
              convention = "dernier_anniversaire", par_annee = FALSE)
ROLES_AG <- c(age = "age", exposition_centrale = "exposition_centrale", exposition_initiale = "exposition_initiale",
              evenements = "evenements", segment = "segment")
ROLES_LI <- c(id_contrat = "id_contrat", id_tete = "id_tete", date_naissance = "date_naissance",
              date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie",
              segment = "segment")
portefeuille <- function(n) list(mode = "uniforme", n = n, proportion_hommes = 1, age_min = 20, age_max = 75,
                                 origine = D("2005-01-01"))
plan_cellule <- function(graine_mc)
  list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER, delai_declaration = 0,
       segmentation = "segment", graine = graine_mc,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "dernier_anniversaire", par_annee = FALSE),
                     homogeneite = list(plage_ages = PLAGE, tranches_age = TRANCHES, replications = B_REPL)))

gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
RACHAT <- list(table = ra, facteurs = c(H = 1, F = 1))
stopifnot(identical(gm$age, 0:120))
Q_REF <- gm$qx                 # indexé par âge + 1
MU_REF <- -log1p(-Q_REF)
AGES <- PLAGE[1L]:PLAGE[2L]
NA_AGES <- length(AGES)
TRANCHE_AGE <- findInterval(AGES, TRANCHES)
NB_TRANCHES <- length(TRANCHES)

# --- grille : cellules et blocs (spécification, § 1 et § 5) --------------------------
THETA_P <- list(`2000` = c(1, 1.10, 1.25, 1.50, 2.00), `8000` = c(1, 1.10, 1.25, 1.50),
                `32000` = c(1, 1.05, 1.10, 1.25))
THETA_U <- list(`2000` = c(1, 1.5, 2), `8000` = c(1, 1.25, 1.5), `32000` = c(1, 1.10, 1.25))
TAU_C <- c(1.25, 1.50)
N_GRILLE <- c(2000, 8000, 32000)
BLOCS <- list(); CELLULES <- list(); k <- 0L
ajouter_bloc <- function(famille, n, parts, valeurs) {
  S <- length(parts)
  b <- length(BLOCS) + 1L
  i0 <- length(CELLULES)
  BLOCS[[b]] <<- list(famille = famille, n = n, S = S, tetes = n * parts, ks = k + seq_len(S),
                      cellules = i0 + seq_along(valeurs))
  k <<- k + S
  for (j in seq_along(valeurs))
    CELLULES[[i0 + j]] <<- list(i = i0 + j, bloc = b, famille = famille, n = n, S = S, valeur = valeurs[j],
                                parts = if (famille == "U") "0,9/0,1" else "égales")
}
for (n in N_GRILLE) for (S in c(2L, 4L, 8L)) ajouter_bloc("P", n, rep(1 / S, S), THETA_P[[as.character(n)]])
for (n in N_GRILLE) ajouter_bloc("U", n, c(0.9, 0.1), THETA_U[[as.character(n)]])
for (n in N_GRILLE) ajouter_bloc("C", n, c(0.5, 0.5), TAU_C)
stopifnot(length(CELLULES) == 54L, k == 54L, all(vapply(BLOCS, function(b) all(b$tetes == round(b$tetes)), NA)))
# Facteur theta_s(x) du segment déviant, âges 0 à 120.
theta_age <- function(ce) {
  if (ce$famille == "C") ifelse(0:120 < AGE_BASCULE_C, ce$valeur, 1 / ce$valeur) else rep(ce$valeur, 121L)
}
libelle_cellule <- function(ce) sprintf("%s n=%d S=%d %s=%s", ce$famille, ce$n, ce$S,
                                        if (ce$famille == "C") "tau" else "theta", format(ce$valeur, nsmall = 2))
for (i in seq_along(CELLULES)) {
  ce <- CELLULES[[i]]
  th <- theta_age(ce)
  CELLULES[[i]]$libelle <- libelle_cellule(ce)
  CELLULES[[i]]$nulle <- all(th == 1)
  # Loi du déviant : table du jeu de référence à theta = 1 (même table, mêmes q),
  # sinon q = 1 - (1 - q~)^theta.
  tab <- if (all(th == 1)) gm else
    table_reference(data.frame(age = 0:120, sexe = "ensemble", qx = -expm1(th * log1p(-Q_REF)), stringsAsFactors = FALSE),
                    nom = sprintf("Gompertz-Makeham, force multipliée par theta (%s)", CELLULES[[i]]$libelle),
                    statut = "importee",
                    source = "tests/mesures/constat_puissance_homogeneite.R : q = 1 - (1 - q_GM)^theta")
  CELLULES[[i]]$loi <- list(deces = list(table = tab, facteurs = c(H = 1, F = 1)), rachat = RACHAT)
  # mu_sc sur la plage : colonnes = segments, déviant en dernier.
  CELLULES[[i]]$mu <- cbind(matrix(MU_REF[AGES + 1L], NA_AGES, ce$S - 1L), MU_REF[AGES + 1L] * th[AGES + 1L])
}
LOI_REF <- list(deces = list(table = gm, facteurs = c(H = 1, F = 1)), rachat = RACHAT)
etiquettes <- function(S) sprintf("s%d", seq_len(S))

# --- outils ----------------------------------------------------------------------------
AVERTIS <- new.env()
remise_avertis <- function() { AVERTIS$n <- list(); AVERTIS$codes <- list() }
remise_avertis()
noter <- function(w) {
  m <- substr(conditionMessage(w), 1L, 90L)
  AVERTIS$n[[m]] <- c(AVERTIS$n[[m]], 0L)[1L] + 1L
  invokeRestart("muffleWarning")
}
simuler_segment <- function(loi, n, graine, etiquette, forme = FORME) {
  s <- withCallingHandlers(simuler_portefeuille(loi, portefeuille(n), PER, graine, forme = forme), warning = noter)
  s$segment <- rep(etiquette, nrow(s))
  s$sexe <- NULL
  s
}
construire <- function(donnees, roles, plan, compter = TRUE) {
  x <- withCallingHandlers(construire_table(donnees, roles, plan), warning = noter)
  if (compter) for (cd in unique(x$avertissements$code)) AVERTIS$codes[[cd]] <- c(AVERTIS$codes[[cd]], 0L)[1L] + 1L
  x
}
imprimer <- function(df) { o <- options(width = 250); on.exit(options(o)); print(df, row.names = FALSE, right = FALSE) }
rejet <- function(p) ifelse(is.na(p), NA, p <= ALPHA * (1 + 1e-9))
CRIT <- function(ddl) stats::qchisq(1 - ALPHA, ddl)
# Loi exacte du nombre de succès de Bernoulli indépendantes de paramètres p.
poisson_binomiale <- function(p) {
  f <- 1
  for (pr in p) f <- c(f * (1 - pr), 0) + c(0, f * pr)
  f
}
# p bilatérale de k sous la loi discrète f (indices 0..), méthode de
# binom.test : somme des probabilités <= f(k) (1 + 1e-7).
p_bilaterale <- function(f, k) min(1, sum(f[f <= f[k + 1L] * (1 + 1e-7)]))

# --- recalcul hors moteur : T, ddl, lambda (spécification, § 2 ; C2) ------------------
# Matrices cellule x segment (âges de la plage) des agrégats d'entrée.
matrices <- function(d, S) {
  dans <- d$age >= PLAGE[1L] & d$age <= PLAGE[2L]
  ia <- cbind(d$age[dans] - PLAGE[1L] + 1L, match(d$segment[dans], etiquettes(S)))
  stopifnot(!anyNA(ia), !anyDuplicated(ia))
  E <- matrix(0, NA_AGES, S); Dm <- matrix(0, NA_AGES, S)
  E[ia] <- d$exposition_centrale[dans]; Dm[ia] <- as.numeric(d$evenements[dans])
  list(E = E, D = Dm)
}
# Composantes connexes (parcours en largeur) du graphe des segments présents,
# deux segments liés s'ils sont exposés dans une même strate informative.
composantes <- function(A) {
  p <- nrow(A); comp <- integer(p); c0 <- 0L
  for (g in seq_len(p)) if (!comp[g]) {
    c0 <- c0 + 1L; file_ <- g; comp[g] <- c0
    while (length(file_)) {
      h <- file_[1L]; file_ <- file_[-1L]
      v <- which(A[h, ] & !comp); comp[v] <- c0; file_ <- c(file_, v)
    }
  }
  comp
}
# Portée (lignes de la plage) : rend list(produit, fautive, T, ddl, lambda).
score_script <- function(M, lignes, mu) {
  E <- M$E[lignes, , drop = FALSE]; Dm <- M$D[lignes, , drop = FALSE]; mu <- mu[lignes, , drop = FALSE]
  if (any(E == 0 & Dm > 0)) return(list(produit = FALSE, fautive = TRUE, T = NA_real_, ddl = NA_integer_, lambda = NA_real_))
  Dc <- rowSums(Dm)
  inf <- Dc > 0 & rowSums(E > 0) >= 2L
  E <- E[inf, , drop = FALSE]; Dm <- Dm[inf, , drop = FALSE]; Dc <- Dc[inf]; mu <- mu[inf, , drop = FALSE]
  pres <- which(colSums(E > 0) > 0)
  if (!length(pres)) return(list(produit = FALSE, fautive = FALSE, T = NA_real_, ddl = 0L, lambda = NA_real_))
  P <- E / rowSums(E)
  Pa <- (E * mu) / rowSums(E * mu)
  U <- colSums(Dm) - colSums(Dc * P)
  delta <- colSums(Dc * (Pa - P))
  V <- diag(colSums(Dc * P), ncol(E)) - crossprod(P, Dc * P)
  A <- crossprod(E[, pres, drop = FALSE] > 0) > 0
  comp <- composantes(A)
  garde <- pres[!(seq_along(pres) %in% match(unique(comp), comp))]
  ddl <- length(garde)
  if (!ddl) return(list(produit = FALSE, fautive = FALSE, T = NA_real_, ddl = 0L, lambda = NA_real_))
  Vr <- V[garde, garde, drop = FALSE]
  list(produit = TRUE, fautive = FALSE, T = sum(U[garde] * solve(Vr, U[garde])), ddl = ddl,
       lambda = sum(delta[garde] * solve(Vr, delta[garde])))
}
puissance_predite <- function(lambda, ddl) stats::pchisq(CRIT(ddl), ddl, ncp = lambda, lower.tail = FALSE)

# --- une construction : grandeurs d'une cellule pour un jeu ---------------------------
TESTS <- c("global", sprintf("tranche_%d", seq_len(NB_TRANCHES)), "somme")
NOMS_TESTS <- c(global = "global", tranche_1 = "tranche 20-49", tranche_2 = "tranche 50-69", tranche_3 = "tranche 70-90",
                somme = "somme des tranches")
lire_cellule <- function(x, d, ce) {
  S <- ce$S; dev <- etiquettes(S)[S]
  h <- x$etapes$homogeneite$resultat
  tt <- h$tests
  ordre <- c(which(tt$test == "global"), which(tt$test == "tranche")[order(tt$tranche_debut[tt$test == "tranche"])],
             which(tt$test == "somme_tranches"))
  stopifnot(length(ordre) == length(TESTS))
  tt <- tt[ordre, ]
  M <- matrices(d, S)
  sc <- c(list(score_script(M, seq_len(NA_AGES), ce$mu)),
          lapply(seq_len(NB_TRANCHES), function(b) score_script(M, which(TRANCHE_AGE == b), ce$mu)))
  ddl_somme <- sum(vapply(sc[-1L], function(s) if (s$produit) s$ddl else 0L, 0L))
  l_somme <- sum(vapply(sc[-1L], function(s) if (s$produit) s$lambda else 0, 0))
  t_somme <- sum(vapply(sc[-1L], function(s) if (s$produit) s$T else 0, 0))
  fautive_somme <- any(vapply(sc[-1L], function(s) s$fautive, NA))
  sc[[length(TESTS)]] <- list(produit = !fautive_somme && ddl_somme > 0L, T = t_somme, ddl = ddl_somme,
                              lambda = l_somme, fautive = fautive_somme)
  o <- numeric(0); ecart_T <- 0; ddl_ok <- TRUE; etat_ok <- TRUE
  for (j in seq_along(TESTS)) {
    t <- TESTS[j]; prod <- identical(tt$etat[j], "produite")
    if (prod != sc[[j]]$produit) etat_ok <- FALSE
    v <- c(produit = as.numeric(prod), statistique = tt$statistique[j], ddl = tt$ddl[j],
           p_asymptotique = tt$p_asymptotique[j], p_exacte = tt$p_exacte[j], p_monte_carlo = tt$p_monte_carlo[j],
           erreur_monte_carlo = tt$erreur_monte_carlo[j],
           lambda = if (sc[[j]]$produit) sc[[j]]$lambda else NA_real_,
           pi = if (sc[[j]]$produit) puissance_predite(sc[[j]]$lambda, sc[[j]]$ddl) else NA_real_)
    if (!prod) v[c("statistique", "p_asymptotique", "p_exacte", "p_monte_carlo", "erreur_monte_carlo")] <- NA_real_
    if (prod && sc[[j]]$produit) {
      ecart_T <- max(ecart_T, abs(sc[[j]]$T - tt$statistique[j]) / max(1, abs(tt$statistique[j])))
      if (!identical(as.integer(sc[[j]]$ddl), as.integer(tt$ddl[j]))) ddl_ok <- FALSE
    }
    names(v) <- paste(t, names(v), sep = "|")
    o <- c(o, v)
  }
  sg <- h$segments
  i_dev <- which(sg$portee == "global" & sg$segment == dev)
  # Exposition centrale par segment (portée globale) du moteur contre les
  # agrégats d'entrée, hors cellules qui portent un événement sans exposition
  # (E_sc = 0, D_sc > 0 pour un segment : cellule écartée pour tous les
  # segments, statut `segments/exposition_centrale` du moteur).
  e_mot <- sg$exposition_centrale[sg$portee == "global"][match(etiquettes(S), sg$segment[sg$portee == "global"])]
  e_ent <- colSums(M$E[rowSums(M$E == 0 & M$D > 0) == 0, , drop = FALSE])
  c(o, D_total = sum(M$D), E_dev = sum(M$E[, S]), O_dev = sum(M$D[, S]), E_autres = sum(M$E[, -S]),
    O_autres = sum(M$D[, -S]), rapport_dev = sg$rapport_observes_attendus[i_dev], z_dev = sg$z[i_dev],
    `id|ecart_T` = ecart_T, `id|ddl` = as.numeric(ddl_ok), `id|etat` = as.numeric(etat_ok),
    `id|agregats` = as.numeric(!anyNA(e_mot) && all(abs(e_mot - e_ent) <= 1e-10 * pmax(1, e_ent))))
}

# Un jeu r d'un bloc : segments non déviants tirés une fois, déviant tiré par
# cellule sous la même graine (nombres aléatoires communs). Rend une liste
# (par cellule du bloc) de vecteurs, ou d'une condition (refus, erreur).
jeu_bloc <- function(bl, r) {
  S <- bl$S; et <- etiquettes(S)
  base <- tryCatch(lapply(seq_len(S - 1L), function(s) simuler_segment(LOI_REF, bl$tetes[s], graine_flux(bl$ks[s], r), et[s])),
                   error = function(e) e)
  lapply(bl$cellules, function(i) {
    if (inherits(base, "error")) return(base)
    ce <- CELLULES[[i]]
    tryCatch({
      dv <- simuler_segment(ce$loi, bl$tetes[S], graine_flux(bl$ks[S], r), et[S])
      d <- do.call(rbind, c(base, list(dv)))
      rownames(d) <- NULL
      x <- construire(d, ROLES_AG, plan_cellule(graine_flux(DECALAGE_FLUX_MC + i, r)))
      lire_cellule(x, d, ce)
    }, error = function(e) e)
  })
}

# --- identité forme en lignes / agrégats (C5) ----------------------------------------
identite_lignes <- function(i, jeux) {
  ce <- CELLULES[[i]]; bl <- BLOCS[[ce$bloc]]; S <- bl$S; et <- etiquettes(S)
  ecarts <- c(D = 0, E = 0, T = 0); n_ok <- 0L
  for (r in jeux) {
    lois <- c(rep(list(LOI_REF), S - 1L), list(ce$loi))
    ag <- lapply(seq_len(S), function(s) simuler_segment(lois[[s]], bl$tetes[s], graine_flux(bl$ks[s], r), et[s]))
    li <- lapply(seq_len(S), function(s) {
      v <- simuler_segment(lois[[s]], bl$tetes[s], graine_flux(bl$ks[s], r), et[s], forme = NULL)
      v$id_contrat <- paste(et[s], v$id_contrat); v$id_tete <- paste(et[s], v$id_tete); v
    })
    pl <- plan_cellule(graine_flux(DECALAGE_FLUX_MC + i, r))
    # Constructions de l'identité lignes / agrégats : hors décompte de T8
    # (constat mineur 2 de l'audit).
    xa <- construire(do.call(rbind, ag), ROLES_AG, pl, compter = FALSE)
    xl <- construire(do.call(rbind, li), ROLES_LI, pl, compter = FALSE)
    ea <- xa$etapes$exposition$resultat$exposition; el <- xl$etapes$exposition$resultat$exposition
    cle_a <- paste(ea$segment, ea$age); cle_l <- paste(el$segment, el$age)
    commun <- intersect(cle_a, cle_l)
    hors <- c(setdiff(cle_a, commun), setdiff(cle_l, commun))
    # cellules présentes d'un seul côté : admises si vides (E = 0, D = 0)
    vide <- function(e, cl) all(e$exposition_centrale[match(cl, paste(e$segment, e$age))] == 0 &
                                  e$evenements[match(cl, paste(e$segment, e$age))] == 0, na.rm = TRUE)
    if (length(hors) && !(vide(ea, intersect(hors, cle_a)) && vide(el, intersect(hors, cle_l)))) {
      ecarts["D"] <- Inf; next
    }
    ia <- match(commun, cle_a); il <- match(commun, cle_l)
    ecarts["D"] <- max(ecarts["D"], abs(as.numeric(ea$evenements[ia]) - as.numeric(el$evenements[il])))
    ecarts["E"] <- max(ecarts["E"], abs(ea$exposition_centrale[ia] - el$exposition_centrale[il]) /
                         pmax(1, abs(el$exposition_centrale[il])))
    ta <- xa$etapes$homogeneite$resultat$tests; tl <- xl$etapes$homogeneite$resultat$tests
    if (!identical(ta$etat, tl$etat)) { ecarts["T"] <- Inf; next }
    p <- ta$etat == "produite"
    if (any(p)) ecarts["T"] <- max(ecarts["T"], abs(ta$statistique[p] - tl$statistique[p]) / pmax(1, abs(tl$statistique[p])))
    n_ok <- n_ok + 1L
  }
  list(ecarts = ecarts, n = n_ok)
}

# --- T0 : consignation -------------------------------------------------------------------
proc <- if (file.exists("/proc/cpuinfo")) sub("^.*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1L]) else NA
nproc <- suppressWarnings(tryCatch(system2("nproc", stdout = TRUE), error = function(e) NA_character_))
arbre <- suppressWarnings(tryCatch(system2("git", c("-C", RACINE, "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
                                   error = function(e) NA_character_))
modifs <- suppressWarnings(tryCatch(system2("git", c("-C", RACINE, "status", "--porcelain"), stdout = TRUE, stderr = FALSE),
                                    error = function(e) NA_character_))
cat("\n=== T0 : consignation ===\n")
cat(sprintf("Plateforme : %s ; %s ; processeur %s ; cœurs disponibles (nproc) %s, détectés (detectCores) %d ; BLAS %s ; LAPACK %s ; calcul séquentiel\n",
            R.version.string, R.version$platform, proc, nproc[1L], parallel::detectCores(), extSoftVersion()[["BLAS"]],
            La_library()))
cat(sprintf("Arbre : %s ; fichiers modifiés ou non suivis (git status --porcelain) : %d\n",
            if (length(arbre)) arbre[1L] else NA, length(modifs)))
# Empreintes rattachant le passage à la version du script et de la liste
# blanche des graines (constat mineur 1 de l'audit), et générateur.
cat(sprintf("Empreintes MD5 : script %s ; tests/controles/graines_admises.csv %s ; générateur %s\n",
            unname(tools::md5sum(file.path(RACINE, "tests", "mesures", "constat_puissance_homogeneite.R"))),
            unname(tools::md5sum(file.path(RACINE, "tests", "controles", "graines_admises.csv"))),
            paste(RNGkind(), collapse = " / ")))
cat(sprintf("tablexp %s ; packages attachés : %s\n", as.character(utils::packageVersion("tablexp")),
            paste(sprintf("%s %s", names(sessionInfo()$otherPkgs), vapply(sessionInfo()$otherPkgs, `[[`, "", "Version")),
                  collapse = ", ")))
cat(sprintf("Paramètres : R = %d jeux par cellule ; B = %.0f réplications ; alpha = %.2f (rejet si p <= alpha (1 + 1e-9)) ; plage %d-%d ; tranches %s ; familles demandées %s\n",
            R_JEUX, B_REPL, ALPHA, PLAGE[1L], PLAGE[2L], paste(TRANCHES, collapse = ", "), paste(FAMILLES, collapse = ", ")))
cat(sprintf("Graines : graine(k, r) = %d + %d k + r ; flux de jeux k = 1 à 54, de Monte-Carlo k = %d + i ; jeux r = 1 à %d ; plage déclarée [%d ; %d]\n",
            GRAINE_BASE, PAS_FLUX, DECALAGE_FLUX_MC, R_JEUX, GRAINE_BASE, GRAINE_BASE + PAS_FLUX * 115L - 1L))
cat(sprintf("E.t. de Monte-Carlo nominale d'un taux (R = %d) : %.4f à 0,05, %.4f à 0,2, %.4f à 0,5\n",
            R_JEUX, sqrt(0.05 * 0.95 / R_JEUX), sqrt(0.2 * 0.8 / R_JEUX), sqrt(0.25 / R_JEUX)))
cat("Grille (cellule i, flux de Monte-Carlo, flux de jeux des segments du bloc) :\n")
imprimer(do.call(rbind, lapply(CELLULES, function(ce) data.frame(
  i = ce$i, cellule = ce$libelle, parts = ce$parts, tetes = paste(BLOCS[[ce$bloc]]$tetes, collapse = "/"),
  flux_mc = DECALAGE_FLUX_MC + ce$i, flux_jeux = paste(BLOCS[[ce$bloc]]$ks, collapse = ","), stringsAsFactors = FALSE))))
t_debut <- proc.time()[["elapsed"]]

# --- analyse d'une famille ------------------------------------------------------------------
fmt <- function(v, d = 4) ifelse(is.na(v), NA_character_, formatC(v, format = "f", digits = d))
# Format unique des p-valeurs d'une table à l'autre (constat mineur 5 de l'audit).
fmt_p <- function(v) ifelse(is.na(v), NA_character_, formatC(v, format = "e", digits = 2))
analyser_famille <- function(fam, res, duree) {
  idx <- which(vapply(CELLULES, `[[`, "", "famille") == fam)
  cat(sprintf("\n######## Famille %s : %d cellules ########\n", fam, length(idx)))
  # Décomptes de jeux : exécutés, refusés, en erreur.
  m <- list(); refus <- character(0); erreurs <- character(0)
  dec <- data.frame(cellule = character(0), executes = integer(0), refus = integer(0), erreurs = integer(0),
                    stringsAsFactors = FALSE)
  for (i in idx) {
    l <- res[[i]]
    ok <- !vapply(l, inherits, NA, "condition")
    rf <- vapply(l, inherits, NA, "refus_tablexp")
    for (r in which(rf)) refus <- c(refus, sprintf("%s, jeu %d : %s (%s)", CELLULES[[i]]$libelle, r, conditionMessage(l[[r]]), l[[r]]$motif))
    for (r in which(!ok & !rf)) erreurs <- c(erreurs, sprintf("%s, jeu %d : %s", CELLULES[[i]]$libelle, r, conditionMessage(l[[r]])))
    m[i] <- list(if (any(ok)) structure(do.call(rbind, l[ok]), rangs = which(ok)) else NULL)
    dec[nrow(dec) + 1L, ] <- list(CELLULES[[i]]$libelle, sum(ok), sum(rf), sum(!ok & !rf))
  }
  col <- function(i, nm) if (is.null(m[[i]])) numeric(0) else m[[i]][, nm]

  cat(sprintf("\n--- T1 : identités (famille %s) ---\n", fam))
  eT <- max(unlist(lapply(idx, function(i) col(i, "id|ecart_T"))), 0)
  verifier(sprintf("famille %s : T recalculée hors moteur = T du moteur (global, tranches, somme) à 1e-10 près en relatif", fam),
           if (eT <= 1e-10) TRUE else sprintf("écart relatif maximal %.3g", eT))
  cat(sprintf("  écart relatif maximal |T_script - T_moteur| / max(1, |T|) : %.3g\n", eT))
  verifier(sprintf("famille %s : ddl recalculés = ddl du moteur", fam),
           all(unlist(lapply(idx, function(i) col(i, "id|ddl"))) == 1))
  verifier(sprintf("famille %s : tests produits par le moteur = tests produits par le recalcul", fam),
           all(unlist(lapply(idx, function(i) col(i, "id|etat"))) == 1))
  verifier(sprintf("famille %s : exposition centrale par segment du moteur = agrégats d'entrée (1e-10)", fam),
           all(unlist(lapply(idx, function(i) col(i, "id|agregats"))) == 1))
  nulles <- idx[vapply(CELLULES[idx], `[[`, NA, "nulle")]
  if (length(nulles)) {
    ep <- max(unlist(lapply(nulles, function(i) abs(c(col(i, "global|pi"), col(i, "somme|pi"),
                                                        unlist(lapply(1:3, function(b) col(i, sprintf("tranche_%d|pi", b))))) - ALPHA))),
              0, na.rm = TRUE)
    verifier(sprintf("famille %s : pi_r = alpha à 1e-12 près à theta = 1", fam),
             if (ep <= 1e-12) TRUE else sprintf("écart maximal %.3g", ep))
    cat(sprintf("  écart maximal |pi_r - alpha| à theta = 1 : %.3g\n", ep))
  }
  if (fam == "P") {
    cibles <- which(vapply(CELLULES, function(ce) ce$famille == "P" &&
                             ((ce$n == 2000 && ce$S == 2 && ce$valeur == 1.25) || (ce$n == 8000 && ce$S == 4 && ce$valeur == 1.50)), NA))
    for (i in cibles) {
      il <- identite_lignes(i, seq_len(min(10L, R_JEUX)))
      cat(sprintf("  forme en lignes / agrégats, %s, jeux 1 à %d : %d jeux comparés ; écart maximal décès %.3g, exposition (relatif) %.3g, statistique (relatif) %.3g\n",
                  CELLULES[[i]]$libelle, min(10L, R_JEUX), il$n, il$ecarts[["D"]], il$ecarts[["E"]], il$ecarts[["T"]]))
      verifier(sprintf("famille P : %s, forme en lignes = forme agrégats (décès identiques ; exposition, statistique à 1e-10)", CELLULES[[i]]$libelle),
               il$n == min(10L, R_JEUX) && il$ecarts[["D"]] == 0 && il$ecarts[["E"]] <= 1e-10 && il$ecarts[["T"]] <= 1e-10)
    }
  }

  cat(sprintf("\n--- Jeux exécutés, refusés par le moteur, en erreur (famille %s) ---\n", fam))
  imprimer(dec)
  if (length(refus)) cat(paste("  refus :", utils::head(refus, 5L)), sep = "\n")
  if (length(erreurs)) cat(paste("  erreur :", utils::head(erreurs, 5L)), sep = "\n")

  cat(sprintf("\n--- T2 : erreur d'estimation par cellule (moyennes sur les jeux exécutés ; famille %s) ---\n", fam))
  cat("  D_tot : décès de la plage, tous segments ; O_dev, E_dev : décès et exposition centrale du segment déviant ; O_autres, E_autres : autres segments ;\n")
  cat("  O/A_dev : rapport observés / attendus interne du déviant (moyenne, écart-type entre jeux) ; 1/sqrt(O_dev) : erreur relative asymptotique moyenne.\n")
  imprimer(do.call(rbind, lapply(idx, function(i) {
    if (is.null(m[[i]])) return(data.frame(cellule = CELLULES[[i]]$libelle, R = 0L, stringsAsFactors = FALSE)[0, ])
    od <- col(i, "O_dev")
    data.frame(cellule = CELLULES[[i]]$libelle, R = nrow(m[[i]]), D_tot = round(mean(col(i, "D_total")), 1),
               D_tot_min = min(col(i, "D_total")), O_dev = round(mean(od), 1), O_dev_min = min(od),
               E_dev = round(mean(col(i, "E_dev")), 0), O_autres = round(mean(col(i, "O_autres")), 1),
               E_autres = round(mean(col(i, "E_autres")), 0),
               OA_dev = round(mean(col(i, "rapport_dev"), na.rm = TRUE), 4), OA_dev_sd = round(stats::sd(col(i, "rapport_dev"), na.rm = TRUE), 4),
               inv_sqrt_O_dev = round(mean(1 / sqrt(od[od > 0])), 4), stringsAsFactors = FALSE)
  })))

  # Taux d'un test : jeux où il est produit.
  taux <- function(i, t, g) {
    p <- col(i, paste(t, g, sep = "|")); I <- rejet(p); prod <- !is.na(I)
    list(I = I, R = sum(prod), np = length(I) - sum(prod), x = sum(I[prod]), taux = if (any(prod)) mean(I[prod]) else NA_real_)
  }

  # --- T3 / L1 : taille ---
  verdicts <- character(0)
  if (length(nulles)) {
    cat(sprintf("\n--- T3 : taille à theta = 1 (règle L1 ; famille %s) ---\n", fam))
    l1 <- list()
    for (i in nulles) {
      S <- CELLULES[[i]]$S
      lignes <- list(c("global", "p_asymptotique", "two.sided"), if (S == 2L) c("global", "p_exacte", "greater"),
                     c("global", "p_monte_carlo", "greater"), c("somme", "p_asymptotique", "two.sided"),
                     c("somme", "p_monte_carlo", "greater"))
      for (li in lignes[!vapply(lignes, is.null, NA)]) {
        tx <- taux(i, li[1L], li[2L])
        pb <- if (tx$R) stats::binom.test(tx$x, tx$R, ALPHA, alternative = li[3L])$p.value else NA_real_
        l1[[length(l1) + 1L]] <- data.frame(cellule = CELLULES[[i]]$libelle, test = NOMS_TESTS[[li[1L]]], p = li[2L],
                                            alternative = if (li[3L] == "greater") "unilatérale sup." else "bilatérale",
                                            R = tx$R, non_produits = tx$np, rejets = tx$x, taux = tx$taux,
                                            et_mc = if (tx$R) sqrt(tx$taux * (1 - tx$taux) / tx$R) else NA_real_,
                                            p_binom = pb, stringsAsFactors = FALSE)
      }
    }
    l1 <- do.call(rbind, l1)
    l1$p_holm <- stats::p.adjust(l1$p_binom, "holm")
    l1$ecart <- !is.na(l1$p_holm) & l1$p_holm <= 0.05
    aff <- l1; aff$taux <- fmt(aff$taux); aff$et_mc <- fmt(aff$et_mc); aff$p_binom <- fmt_p(aff$p_binom); aff$p_holm <- fmt_p(aff$p_holm)
    cat(sprintf("  %d lignes ; test binomial exact contre alpha = %.2f ; Holm à 5 %% dans la famille ; e.t. MC = sqrt(taux (1 - taux) / R) (erreur due au nombre R de jeux)\n",
                nrow(l1), ALPHA))
    imprimer(aff)
    v <- if (!any(l1$ecart)) "taille compatible avec α (aucun écart détecté, risque global de 5 %)" else
      paste("écart de taille détecté :", paste(sprintf("%s, %s %s (%s, taux %.4f)", l1$cellule[l1$ecart], l1$test[l1$ecart], l1$p[l1$ecart],
                                                     ifelse(l1$taux[l1$ecart] > ALPHA, "au-dessus de α", "au-dessous de α"), l1$taux[l1$ecart]),
                                             collapse = " ; "))
    cat(sprintf("  L1 (famille %s) : %s\n", fam, v))
    verdicts <- c(verdicts, sprintf("L1, famille %s : %s", fam, v))
  }

  # --- T4 : puissance et prédiction ---
  cat(sprintf("\n--- T4 : puissance (taux de rejet à 5 %%) et prédiction asymptotique (famille %s) ---\n", fam))
  cat("  taux : jeux où le test est produit ; e.t. MC = sqrt(taux (1 - taux) / R) ; pi_bar = moyenne des pi_r ; d = taux - pi_bar, IC à 95 % = d ± 1,96 sd_r(I_r - pi_r) / sqrt(R) ;\n")
  cat("  p_PB : Poisson-binomiale exacte bilatérale du nombre de rejets contre les pi_r ; e_B : moyenne de l'erreur de Monte-Carlo de p_MC due à B (moteur).\n")
  t4 <- list()
  for (i in idx) {
    ce <- CELLULES[[i]]
    for (t in TESTS) {
      ta <- taux(i, t, "p_asymptotique"); tm <- taux(i, t, "p_monte_carlo")
      te <- if (ce$S == 2L && t != "somme") taux(i, t, "p_exacte") else NULL
      pir <- col(i, paste(t, "pi", sep = "|"))
      prod <- !is.na(ta$I) & !is.na(pir)
      c_ <- ta$I[prod] - pir[prod]
      Rr <- sum(prod)
      pb <- if (Rr) p_bilaterale(poisson_binomiale(pir[prod]), sum(ta$I[prod])) else NA_real_
      t4[[length(t4) + 1L]] <- data.frame(
        i = i, cellule = ce$libelle, nulle = ce$nulle, test = NOMS_TESTS[[t]], cle = t, R = ta$R, non_produits = ta$np,
        asympt = ta$taux, et_mc = if (ta$R) sqrt(ta$taux * (1 - ta$taux) / ta$R) else NA_real_,
        cp_inf = if (ta$R) stats::binom.test(round(ta$taux * ta$R), ta$R)$conf.int[1L] else NA_real_,
        cp_sup = if (ta$R) stats::binom.test(round(ta$taux * ta$R), ta$R)$conf.int[2L] else NA_real_,
        exacte = if (is.null(te)) NA_real_ else te$taux, monte_carlo = tm$taux,
        e_B = if (tm$R) mean(col(i, paste(t, "erreur_monte_carlo", sep = "|")), na.rm = TRUE) else NA_real_,
        pi_bar = if (Rr) mean(pir[prod]) else NA_real_, d = if (Rr) mean(c_) else NA_real_,
        d_et = if (Rr > 1L) stats::sd(c_) / sqrt(Rr) else NA_real_, p_PB = pb, stringsAsFactors = FALSE)
    }
  }
  t4 <- do.call(rbind, t4)
  aff4 <- function(df) {
    a <- df[, c("cellule", "test", "R", "non_produits", "asympt", "et_mc", "exacte", "monte_carlo", "e_B", "pi_bar", "d")]
    # Intervalle exact (Clopper-Pearson) du taux asymptotique : non dégénéré à
    # 0 ou 1, contrairement à et_mc (constat mineur 4 de l'audit).
    a$IC95_asympt <- ifelse(is.na(df$cp_inf), NA, sprintf("[%.4f ; %.4f]", df$cp_inf, df$cp_sup))
    a$IC_d <- ifelse(is.na(df$d_et), NA, sprintf("[%+.4f ; %+.4f]", df$d - 1.96 * df$d_et, df$d + 1.96 * df$d_et))
    a$p_PB <- fmt_p(df$p_PB)
    for (nm in c("asympt", "et_mc", "exacte", "monte_carlo", "e_B", "pi_bar", "d")) a[[nm]] <- fmt(a[[nm]])
    a
  }
  cat("  Test global :\n")
  imprimer(aff4(t4[t4$cle == "global", ]))
  cat("  Somme des tranches (L3 : face à sa prédiction, lambda et ddl sommés sur les tranches) :\n")
  imprimer(aff4(t4[t4$cle == "somme", ]))
  cat("  Tranches (L3, descriptif) :\n")
  imprimer(aff4(t4[grepl("^tranche", t4$cle), ]))

  # --- L2 : prédiction (familles P et U) ---
  if (fam %in% c("P", "U")) {
    cat(sprintf("\n--- L2 : prédiction asymptotique, test global (famille %s, cellules à theta > 1) ---\n", fam))
    g <- t4[t4$cle == "global" & !t4$nulle, ]
    g$p_holm <- stats::p.adjust(g$p_PB, "holm")
    det <- !is.na(g$p_holm) & g$p_holm <= 0.05
    cat(sprintf("  (a) %d cellules ; Holm à 5 %% sur p_PB :\n", nrow(g)))
    a <- g[, c("cellule", "R", "asympt", "pi_bar", "d", "p_PB", "p_holm")]
    for (nm in c("asympt", "pi_bar", "d")) a[[nm]] <- fmt(a[[nm]])
    a$p_PB <- fmt_p(a$p_PB); a$p_holm <- fmt_p(a$p_holm); a$detecte <- det
    imprimer(a)
    K <- g$i[!is.na(g$pi_bar) & g$pi_bar >= PI_BORNES[1L] & g$pi_bar <= PI_BORNES[2L]]
    ic <- c(NA_real_, NA_real_); bh <- NA_real_; se <- NA_real_
    if (length(K)) {
      # b_r par rang de jeu : cellules du bloc tirées sous les mêmes graines, la
      # moyenne par jeu garde leur corrélation (nombres aléatoires communs).
      tous <- sort(unique(unlist(lapply(K, function(i) attr(m[[i]], "rangs")))))
      somme <- numeric(length(tous)); nb <- numeric(length(tous))
      for (i in K) {
        rg <- attr(m[[i]], "rangs"); I <- rejet(col(i, "global|p_asymptotique")); pr <- col(i, "global|pi")
        ok <- !is.na(I) & !is.na(pr)
        j <- match(rg[ok], tous)
        somme[j] <- somme[j] + (I[ok] - pr[ok]); nb[j] <- nb[j] + 1
      }
      br <- somme[nb > 0] / nb[nb > 0]
      bh <- mean(br); se <- stats::sd(br) / sqrt(length(br)); ic <- bh + c(-1, 1) * stats::qnorm(0.975) * se
      cat(sprintf("  (b) biais groupé sur %d cellules à pi_bar dans [%.2f ; %.2f] (%s) : b^ = %+.4f, e.t. %.4f (%d jeux), IC à 95 %% [%+.4f ; %+.4f]\n",
                  length(K), PI_BORNES[1L], PI_BORNES[2L], paste(vapply(CELLULES[K], `[[`, "", "libelle"), collapse = " ; "),
                  bh, se, length(br), ic[1L], ic[2L]))
    } else cat(sprintf("  (b) biais groupé : aucune cellule à pi_bar dans [%.2f ; %.2f]\n", PI_BORNES[1L], PI_BORNES[2L]))
    v <- if (any(det)) paste("écart détecté :", paste(g$cellule[det], collapse = " ; ")) else
      if (length(K) && ic[1L] >= -MARGE_BIAIS && ic[2L] <= MARGE_BIAIS)
        "compatible avec la prédiction asymptotique (aucun écart détecté, risque global de 5 % ; biais groupé dans ±0,02)" else
          if (!length(K)) "biais groupé non exclu (aucune cellule pour (b))" else "biais groupé non exclu"
    cat(sprintf("  L2 (famille %s) : %s\n", fam, v))
    verdicts <- c(verdicts, sprintf("L2, famille %s : %s", fam, v))
  }

  # --- T5 : pertes appariées (L3, descriptif) ---
  cat(sprintf("\n--- T5 : pertes appariées (L3, descriptif ; famille %s) ---\n", fam))
  cat("  (i) theta successifs d'un bloc, test global asymptotique, même jeu r : pertes = rejet à la valeur précédente, non-rejet à la suivante ; gains = l'inverse ;\n")
  cat("  (ii) même cellule, même jeu : global seul = rejet du global sans la somme des tranches ; somme seule = l'inverse ; MC seul / asympt. seul : test global.\n")
  t5a <- list()
  for (b in unique(vapply(CELLULES[idx], `[[`, 0L, "bloc"))) {
    ci <- BLOCS[[b]]$cellules
    if (fam == "C") next
    for (j in seq_len(length(ci) - 1L)) {
      i1 <- ci[j]; i2 <- ci[j + 1L]
      r1 <- attr(m[[i1]], "rangs"); r2 <- attr(m[[i2]], "rangs"); rc <- intersect(r1, r2)
      I1 <- rejet(col(i1, "global|p_asymptotique"))[match(rc, r1)]; I2 <- rejet(col(i2, "global|p_asymptotique"))[match(rc, r2)]
      ok <- !is.na(I1) & !is.na(I2)
      t5a[[length(t5a) + 1L]] <- data.frame(de = CELLULES[[i1]]$libelle, vers = format(CELLULES[[i2]]$valeur, nsmall = 2),
                                            jeux = sum(ok), pertes = sum(I1[ok] & !I2[ok]), gains = sum(!I1[ok] & I2[ok]),
                                            stringsAsFactors = FALSE)
    }
  }
  if (length(t5a)) imprimer(do.call(rbind, t5a)) else cat("  (i) sans objet (famille C : deux tau sans ordre de déviation monotone par âge)\n")
  imprimer(do.call(rbind, lapply(idx, function(i) {
    Ig <- rejet(col(i, "global|p_asymptotique")); Is <- rejet(col(i, "somme|p_asymptotique")); Im <- rejet(col(i, "global|p_monte_carlo"))
    o1 <- !is.na(Ig) & !is.na(Is); o2 <- !is.na(Ig) & !is.na(Im)
    data.frame(cellule = CELLULES[[i]]$libelle, global_seul = sum(Ig[o1] & !Is[o1]), somme_seule = sum(!Ig[o1] & Is[o1]),
               asympt_seul = sum(Ig[o2] & !Im[o2]), mc_seul = sum(!Ig[o2] & Im[o2]), stringsAsFactors = FALSE)
  })))

  # --- T6 : écart minimal détectable theta_80 (L3, descriptif) ---
  if (fam %in% c("P", "U")) {
    cat(sprintf("\n--- T6 : écart minimal détectable theta_80 (puissance 0,80 du test global asymptotique ; L3, descriptif ; famille %s) ---\n", fam))
    cat("  interpolation linéaire en theta entre les points de la grille ; observé : taux de rejet ; prédit : pi_bar ; « > max » si 0,80 n'est pas atteint.\n")
    interp <- function(th, y) {
      j <- which(y >= 0.8)
      if (!length(j)) return(sprintf("> %s", format(max(th), nsmall = 2)))
      j <- j[1L]; if (j == 1L) return(format(th[1L], nsmall = 2))
      sprintf("%.3f", th[j - 1L] + (0.8 - y[j - 1L]) * (th[j] - th[j - 1L]) / (y[j] - y[j - 1L]))
    }
    imprimer(do.call(rbind, lapply(unique(vapply(CELLULES[idx], `[[`, 0L, "bloc")), function(b) {
      ci <- BLOCS[[b]]$cellules; g <- t4[t4$cle == "global" & t4$i %in% ci, ]
      th <- vapply(CELLULES[ci], `[[`, 0, "valeur")
      data.frame(bloc = sprintf("%s n=%d S=%d", fam, BLOCS[[b]]$n, BLOCS[[b]]$S),
                 D_tot = round(mean(col(ci[1L], "D_total")), 1), O_dev_theta1 = round(mean(col(ci[1L], "O_dev")), 1),
                 theta80_observe = interp(th, g$asympt), theta80_predit = interp(th, g$pi_bar), stringsAsFactors = FALSE)
    })))
  }

  # --- T7, T8 ---
  cat(sprintf("\n--- T7 : verdicts (famille %s) ---\n", fam))
  if (length(verdicts)) cat(paste(" ", verdicts), sep = "\n") else cat("  aucun verdict (famille C : L3, descriptif)\n")
  cat(sprintf("\n--- T8 : avertissements (famille %s) ---\n", fam))
  cat(sprintf("  jeux refusés par le moteur : %d ; jeux en erreur : %d (hors dénominateur des taux)\n", length(refus), length(erreurs)))
  cat("  Avertissements du résultat (x$avertissements, code ; nombre de constructions qui l'émettent, hors constructions de l'identité lignes / agrégats) :\n")
  if (length(AVERTIS$codes)) for (nm in names(AVERTIS$codes)) cat(sprintf("  %6d  %s\n", AVERTIS$codes[[nm]], nm)) else cat("    aucun\n")
  cat("  Avertissements R reçus (warning(), texte tronqué) :\n")
  if (length(AVERTIS$n)) for (nm in names(AVERTIS$n)) cat(sprintf("  %6d  %s\n", AVERTIS$n[[nm]], nm)) else cat("    aucun\n")
  cat("  Statuts : taux, écarts, biais groupés = constats de simulation (erreur de Monte-Carlo due à R) ; pi_r = approximation asymptotique (khi-deux non centrale), E[U | marges] = delta exact sous Poisson à E fixée ; e_B = erreur de Monte-Carlo de chaque p_MC due à B ; erreur d'estimation en T2.\n")
  cat(sprintf("Durée de la famille %s : %.1f min (%.2f s par jeu de famille)\n", fam, duree / 60, duree / R_JEUX))
  cat(sprintf("######## Fin de la famille %s ########\n", fam))
  invisible(NULL)
}

# --- exécution, famille par famille -----------------------------------------------------
for (fam in FAMILLES) {
  remise_avertis()
  blocs <- which(vapply(BLOCS, `[[`, "", "famille") == fam)
  res <- vector("list", length(CELLULES))
  for (b in blocs) for (i in BLOCS[[b]]$cellules) res[[i]] <- vector("list", R_JEUX)
  t0 <- proc.time()[["elapsed"]]
  for (r in seq_len(R_JEUX)) {
    for (b in blocs) {
      l <- jeu_bloc(BLOCS[[b]], r)
      for (j in seq_along(l)) res[[BLOCS[[b]]$cellules[j]]][[r]] <- l[[j]]
    }
    if (r == 1L) cat(sprintf("\nPremier jeu de la famille %s : %.2f s ; durée extrapolée pour %d jeux : %.1f min\n",
                             fam, proc.time()[["elapsed"]] - t0, R_JEUX, (proc.time()[["elapsed"]] - t0) * R_JEUX / 60))
  }
  duree <- proc.time()[["elapsed"]] - t0
  if (nzchar(SORTIE)) saveRDS(list(famille = fam, jeux = R_JEUX, cellules = lapply(CELLULES, `[`, c("i", "libelle", "famille", "n", "S", "valeur")),
                                   resultats = res[unlist(lapply(blocs, function(b) BLOCS[[b]]$cellules))]),
                              sprintf("%s_%s.rds", SORTIE, fam))
  analyser_famille(fam, res, duree)
}
cat(sprintf("\nDurée totale : %.1f min\n", (proc.time()[["elapsed"]] - t_debut) / 60))
bilan()
