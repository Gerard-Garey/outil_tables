# -----------------------------------------------------------------------------
#  tests/mesures/constat_stocks_successifs.R : constat de simulation hors CI
#  (E2.19 : nombre de têtes, niveau, région, fausse alarme) de la recette
#  d'exposition `stocks_successifs` (issue #180 ; spécification
#  d'actuary-approfondi sur #180, § 5, § 12 et § 14, C15, acceptée par le
#  mainteneur, M328 ; amendement A1, M339 ; amendement A2, accepté par le
#  mainteneur le 09/11/2026 ; étape (4d) de B15, PR #188).
#
#  Hors des batteries : aucun seuil statistique ; verifier() ne contrôle que
#  l'exécution et les identités exactes (ci-dessous). Statut des chiffres :
#  constat de simulation. L'erreur de Monte-Carlo (nombre R de jeux :
#  s / sqrt(R) sur une moyenne, sqrt(p (1 - p) / R) sur une couverture) ne se
#  confond pas avec l'erreur d'estimation de chaque jeu (exposition et décès
#  par sexe, 1 / sqrt(D), largeur des intervalles), résumée à part.
#
#  Loi vraie (celle de simuler_reference(), tests/reference/outils_reference.R) :
#  décès Gompertz-Makeham (A = 2e-4, B = 2,5e-5, c = 1,1), facteurs H 1, F 0,6
#  (sur q_x) ; rachat constant 5 % ; force constante par âge entier
#  (mu = -ln(1 - q), attribut `simulation`). Période 2015-2024, délai 0,
#  segmentation par sexe, convention `dernier_anniversaire`, par_annee FALSE,
#  estimateurs poisson, binomial, kaplan_meier, niveau 0,95.
#
#  Scénarios (§ 12) :
#   S1 : mode `uniforme` (âge à l'entrée 20 à 75, origine 2005-01-01),
#        n = 2e4 têtes, grille annuelle 2014-12-31, ..., 2024-12-31 ;
#   S2 : mêmes jeux que S1 (réemploi A1 de #176 : même loi, même portefeuille,
#        même graine, projection sur une autre grille), deux inventaires
#        seulement (2014-12-31 et 2024-12-31) ;
#   S3 : comme S1 avec n = 2e5 ;
#   S4 : entrées concentrées au lendemain du premier inventaire (test de
#        résistance de `milieu` ; amendement A2.2), grille annuelle ; jeu
#        propre = concaténation de deux jeux du simulateur :
#         - fond : mode `age_exact`, n = round(n / 10), âge 45, date_entree
#           2014-12-31 (toutes ses unités présentes au premier inventaire,
#           qui n'est donc pas vide : § 7.4) ;
#         - bloc : mode `age_exact`, n, âge AGE_S4 (55), date_entree
#           2015-01-02 ;
#        identifiants du bloc décalés de 10^ceiling(log10(n_fond + 1)) ;
#        forme stocks = concaténation des deux projections du simulateur
#        (chacune sous la graine de son jeu propre, réemploi A1), même
#        décalage ; décomptes C6 sommés composante par composante.
#  Trois lectures de chaque jeu : L (forme en lignes du jeu propre, recette
#  `lignes_dates_completes`, exposition exacte) ; T (forme stocks,
#  entree_entre_inventaires = "premier_inventaire") ; M (forme stocks,
#  "milieu"). La forme stocks est celle du simulateur (forme =
#  list(nom = "stocks_successifs", inventaires), même graine que L).
#
#  Grandeurs, par jeu, sexe et lecture : D (décès comptés), E = somme des
#  E^c_x (années d'âge), A = somme des E^c_x mu_x (loi vraie), U = D - A ;
#  Delta = U_M - U_L (apparié ; D_M = D_L, identité vérifiée, donc Delta =
#  A_L - A_M).
#  Tests (E2.19), pour L et T : H0 E[U] = 0 (vraie en temps continu par
#  l'identité du compensateur, à un écart d'environ ½ jour par sortie près
#  dû aux dates entières, sous le seuil de détection) ; Z = Ubar sqrt(R) /
#  s_U, région d'acceptation |Z| <= 1,96, probabilité de fausse alarme 5 %
#  (asymptotique en R) ; le nombre total de tests et de fausses alarmes
#  attendues est imprimé.
#  Pour M (amendement A2.4) :
#   - « biais total contre la loi vraie » b = E[U_M] / E[A_M], intervalle
#     ± 1,96 s / sqrt(R) (il inclut l'écart de discrétisation du simulateur,
#     commun à L) ;
#   - « biais propre à l'hypothèse d'entrée » delta = E[Delta] / E[A_M],
#     Delta apparié, demi-largeur de Monte-Carlo h = 1,96 s_Delta / sqrt(R) /
#     E[A_M] ; erreur de Monte-Carlo bien moindre que celle de b (le bruit
#     d'estimation de U_L, commun aux deux lectures, s'annule) ;
#   - beta = |delta| sqrt(E[D_L]) ; n* = n / beta^2, taille au-delà de
#     laquelle le biais propre dépasse l'erreur d'estimation relative
#     1 / sqrt(D) du rapport A/E global par sexe ; intervalle
#     [n / ((|delta| + h)^2 E[D]) ; n / ((|delta| - h)^2 E[D])], « non borné »
#     si |delta| <= h ;
#   - décomposition par classe, par jeu et par sexe : eps_E = somme sur E de
#     (e - â) / 365,25, eps_ES de même sur ES ; résidu (E_M - E_L) - eps_E -
#     eps_ES (non nul par construction : il reflète surtout l'écart entre
#     365,25 et la durée des années d'âge du moteur, environ 0,66 année sous
#     S4, où les années d'âge du bloc durent 365 jours) ; écart apparié
#     eps_E - prédit (moyenne, e.t., z).
#  Prédiction du biais de `milieu` (§ 5, amendement A2.1 ; développement au
#  second ordre en L, asymptotique pour lambda L -> 0, dérivation propre non
#  publiée ; seul le constat fait foi des valeurs) : par unité de classe E
#  (entrée e dans ]t_{f-1}, t_f], vue à t_f), l'excès d'exposition imputée
#  est delta = e - â, d'espérance, sous entrée uniforme sur les L jours de
#  l'intervalle et force de sortie toutes causes lambda constante :
#   - au second ordre : (L + 1)/2 - ceil(L/2) + lambda (L^2 - 1)/12 jours
#     (le premier terme vaut ½ jour pour un intervalle de 366 jours, 0 sinon :
#     arrondi de â au jour supérieur) ; c'est lambda L^2 / 12 au pas annuel ;
#   - « géométrique » : espérance discrète exacte sous ces hypothèses ;
#  lambda_u = (mu_rachat + mu_deces(sexe, âge à l'entrée x)) / 365,25 par
#  jour ; unités ES (entrées et sorties entre deux inventaires) omises (biais
#  nul au premier ordre, § 5). Delta prédit, en années : - somme de
#  mu_deces(x) E[delta] (« sans gradient d'âge ») ; « avec gradient d'âge » :
#  plus somme de mu_deces'(x) E[delta^2] / 2, E[delta^2] ~ (L^2 - 1)/12 jours²
#  (l'exposition imputée [â, e) tombe à des âges antérieurs à x, de force
#  plus faible ; l'exposition perdue [e, â) à des âges postérieurs ; terme
#  du même ordre que lambda L^2 / 12, de signe opposé ; A2.1, (ii)) ; « avec
#  gradient + ES réalisé » : plus - somme sur ES de mu_deces(x) (e - â) /
#  365,25, réalisé sur le jeu. Sans objet pour S4 (entrées non uniformes) ;
#  hors du domaine du développement pour S2 (lambda L de l'ordre de 0,5,
#  déplacement d'âge de l'ordre de 2,5 ans).
#  Couvertures du q_x vrai par les intervalles de Poisson (exact et
#  asymptotique, sur la probabilité) et de Kaplan-Meier (asymptotique), pour
#  chaque lecture, aux âges 40, 60 et 80 (S1 à S3) et AGE_S4, AGE_S4 + 5 (S4).
#  La bande cible ± 3 e.t. est descriptive, non un test à niveau fixé ; quand
#  des intervalles ne sont pas produits (D = 0), la couverture est
#  conditionnelle à D >= 1. Les lignes hors bande sont lues avec L pour
#  témoin (exposition exacte) : hors bande en L, propriété de l'intervalle à
#  cet effectif, et les lignes T ou M hors bande au même sexe, âge et
#  intervalle ne sont pas propres à la recette ; L dans la bande et T ou M
#  hors bande, propre à la lecture, à examiner.
#
#  Identités exactes, vérifiées sur chaque jeu (verifier(), un échec fait
#  échouer le script) :
#   - D par cellule : M égal à L (cellules à décès appariées par clé) ;
#   - C3 : sous T, E^c_T <= E^c_L et D_T <= D_L dans chaque cellule ;
#   - C6 : relevés de la recette (P0, E, ES par cause, présents au dernier
#     inventaire) égaux aux décomptes de l'attribut `simulation` ;
#   - C1 (oracle), scénarios S1, S2 et S4 : tables d'exposition et de brut de
#     T et de M identical() à celles d'une construction en lignes
#     reconstituées dans ce script depuis le jeu propre (entrée = â selon le
#     mode, unités ES écartées sous T, gardées sous M ; unités sans ligne de
#     stocks écartées ; lignes dans l'ordre des identifiants) ; en cas
#     d'écart, tolérance relative 1e-12 (all.equal) imprimée ;
#   - S4 (A2.2), par sexe, tolérance relative 1e-10 : A_L - A_M =
#     mu_{AGE_S4} somme(l_u > 0) l_u / L_{AGE_S4} - mu_{AGE_S4 - 1} somme(l_u < 0)
#     |l_u| / L_{AGE_S4 - 1}, l_u = â_u - e_u en jours, u parcourant les
#     unités E et ES du bloc, L_a la durée en jours de l'année d'âge a du bloc
#     (365 aux défauts) ; avec le contrôle que toutes les naissances du bloc
#     sont égales ;
#   - mêmes grandeurs (noms, dans le même ordre) dans chaque jeu, avant
#     l'assemblage de la matrice des jeux.
#
#  Graines (#176 ; § 12) : jeu r du scénario k sous GRAINES_JEU[k] + r,
#  r < 10 000 : S1 et S2 sous 20301200 + r (mêmes jeux), S3 sous 20311200 + r,
#  S4 : bloc sous 20321200 + r, fond sous 20321200 + DECALAGE_FOND + r
#  (DECALAGE_FOND = 5000, R de S4 < 5000) ; plages déclarées dans
#  tests/controles/graines_admises.csv. Aucune étape aléatoire dans les plans
#  (aucune graine de plan).
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise : hors d'une locale
#  UTF-8, le script refuse et sort en échec par bilan()) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/constat_stocks_successifs.R [cle=valeur ...]
#  Clés : jeux (R de S1 et S2, défaut 1000), jeux_s3 (R de S3, défaut 200),
#  jeux_s4 (R de S4, défaut 200, < 5000), tetes (n de S1, S2 et du bloc de
#  S4, défaut 2e4), tetes_s3 (n de S3, défaut 2e5), age_s4 (défaut 55),
#  scenarios (sous-ensemble de S1,S2,S3,S4, défaut tous). Défauts de R : ceux
#  de l'amendement A2.3 (erreur type 0,0069 sur une couverture de 0,95 à
#  R = 1000). Durée mesurée au pilote (un cœur, Xeon 2,1 GHz) : 4,4 s par jeu
#  pour S1 et S2 ensemble, 20 s pour S3 ; S4 : voir la sortie ; environ
#  2 h 35 aux défauts (A2.3).
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
R_JEUX <- as.integer(argument("jeux", "1000"))
R_JEUX_S3 <- as.integer(argument("jeux_s3", "200"))
R_JEUX_S4 <- as.integer(argument("jeux_s4", "200"))
N_TETES <- as.numeric(argument("tetes", "2e4"))
N_TETES_S3 <- as.numeric(argument("tetes_s3", "2e5"))
AGE_S4 <- as.numeric(argument("age_s4", "55"))
stopifnot(AGE_S4 == round(AGE_S4))
SCENARIOS <- strsplit(argument("scenarios", "S1,S2,S3,S4"), ",", fixed = TRUE)[[1L]]
stopifnot(all(SCENARIOS %in% c("S1", "S2", "S3", "S4")))
NIVEAU <- 0.95
Z_CRIT <- 1.96
# Âges des couvertures, par groupe de tirage (S1 : S1 et S2).
AGES_COUV <- list(S1 = c(40L, 60L, 80L), S3 = c(40L, 60L, 80L), S4 = as.integer(c(AGE_S4, AGE_S4 + 5)))
SEXES <- c("H", "F")
# Groupes de tirage : S1 et S2 partagent leurs jeux (même graine).
GRAINES_JEU <- c(S1 = 20301200L, S3 = 20311200L, S4 = 20321200L)
# S4 : graine du fond décalée de DECALAGE_FOND dans la plage de S4 (A2.2).
DECALAGE_FOND <- 5000L
stopifnot(R_JEUX < 10000L, R_JEUX_S3 < 10000L)  # au-delà, une plage rejoindrait la suivante
stopifnot(R_JEUX_S4 < DECALAGE_FOND)            # au-delà, le bloc rejoindrait les graines du fond

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
G_ANNUELLE <- seq(D("2014-12-31"), D("2024-12-31"), by = "year")
G_DEUX <- D(c("2014-12-31", "2024-12-31"))
gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
FACTEURS <- c(H = 1, F = 0.6)
LOI <- list(deces = list(table = gm, facteurs = FACTEURS), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
# Loi vraie par sexe, indexée par âge + 1 (âges 0 à 120) : q et mu des décès
# (q_s = min(f_s q, 1), mu = -ln(1 - q) : loi_effective() du simulateur) ;
# force du rachat.
Q_VRAI <- lapply(FACTEURS, function(f) pmin(f * gm$qx[order(gm$age)], 1))
MU_VRAI <- lapply(Q_VRAI, function(q) -log1p(-q))
MU_RACHAT <- -log1p(-0.05)
PF <- list(
  S1 = list(mode = "uniforme", n = N_TETES, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01")),
  S3 = list(mode = "uniforme", n = N_TETES_S3, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01")),
  S4_fond = list(mode = "age_exact", n = round(N_TETES / 10), proportion_hommes = 0.5, age = 45, date_entree = D("2014-12-31")),
  S4 = list(mode = "age_exact", n = N_TETES, proportion_hommes = 0.5, age = AGE_S4, date_entree = D("2015-01-02")))
# Décalage des identifiants du bloc de S4 : 10^ceiling(log10(n_fond + 1)).
DECALAGE_ID_S4 <- as.integer(10^ceiling(log10(PF$S4_fond$n + 1)))
ROLES_L <- c(id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance", date_entree = "date_entree",
             date_sortie = "date_sortie", cause_sortie = "cause_sortie")
ROLES_S <- c(id_tete = "id_tete", type_ligne = "type_ligne", date_inventaire = "date_inventaire", sexe = "sexe",
             date_naissance = "date_naissance", date_sortie = "date_sortie", cause_sortie = "cause_sortie")

# Plan : `mode` NULL pour la lecture L (lignes), sinon le mode d'entrée des
# stocks et la grille `grille`.
plan_lecture <- function(mode = NULL, grille = NULL) {
  ex <- list(convention = "dernier_anniversaire", par_annee = FALSE)
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER, delai_declaration = 0)
  if (!is.null(mode)) { p$inventaires <- grille; ex$entree_entre_inventaires <- mode }
  c(p, list(segmentation = "sexe",
            etapes = list(donnees = list(exclusions = character(0)), exposition = ex,
                          brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = NIVEAU))))
}

# --- outils ------------------------------------------------------------------------
AVERTIS <- new.env(); AVERTIS$n <- list()
noter <- function(w) {
  m <- substr(conditionMessage(w), 1L, 90L)
  AVERTIS$n[[m]] <- c(AVERTIS$n[[m]], 0L)[1L] + 1L
  invokeRestart("muffleWarning")
}
# Construction ; les avertissements du résultat (x$avertissements, codes)
# sont décomptés par lecture (`lecture`), une fois par construction.
AVERTIS$codes <- list()
construire <- function(donnees, roles, plan, lecture) {
  x <- withCallingHandlers(construire_table(donnees, roles, plan), warning = noter)
  for (cd in unique(x$avertissements$code)) {
    nm <- paste(lecture, cd, sep = " : ")
    AVERTIS$codes[[nm]] <- c(AVERTIS$codes[[nm]], 0L)[1L] + 1L
  }
  x
}
simuler <- function(k, gj, grille = NULL)
  withCallingHandlers(simuler_portefeuille(LOI, PF[[k]], PER, gj,
                                           forme = if (!is.null(grille)) list(nom = "stocks_successifs", inventaires = grille)),
                      warning = noter)

# Jeu r du groupe k, en lignes (grille NULL) ou en stocks sur `grille` :
# list(donnees, decomptes (stocks : décomptes de l'attribut `simulation`),
# bloc (S4 : jeu du bloc, identifiants décalés)). S4 : concaténation du fond
# (graine + DECALAGE_FOND) et du bloc (A2.2), décomptes sommés composante
# par composante.
simuler_jeu <- function(k, r, grille = NULL) {
  gj <- GRAINES_JEU[[k]] + r
  dec <- function(d) if (is.null(grille)) NULL else attr(d, "simulation")$forme$decomptes
  if (k != "S4") {
    d <- simuler(k, gj, grille)
    return(list(donnees = d, decomptes = dec(d), bloc = NULL))
  }
  f <- simuler("S4_fond", gj + DECALAGE_FOND, grille); b <- simuler("S4", gj, grille)
  b$id_tete <- b$id_tete + DECALAGE_ID_S4; b$id_contrat <- b$id_contrat + DECALAGE_ID_S4
  dc <- if (is.null(grille)) NULL else Map(`+`, dec(f), dec(b))
  d <- rbind(f, b); rownames(d) <- NULL; attr(d, "simulation") <- NULL
  list(donnees = d, decomptes = dc, bloc = b)
}

# Grandeurs d'une lecture : par sexe, D, E, A ; couverture du q_x vrai aux
# âges `ages` (NA : cellule absente ou borne non produite).
lire <- function(x, prefixe, ages) {
  ex <- x$etapes$exposition$resultat$exposition
  po <- x$etapes$brut$resultat$poisson; km <- x$etapes$brut$resultat$kaplan_meier
  o <- list()
  couvre <- function(inf, sup, q) if (length(inf) == 1L && !is.na(inf) && !is.na(sup)) inf <= q && q <= sup else NA
  for (sx in SEXES) {
    i <- ex$sexe == sx
    o[[paste(prefixe, sx, "D", sep = "|")]] <- sum(ex$evenements[i])
    o[[paste(prefixe, sx, "E", sep = "|")]] <- sum(ex$exposition_centrale[i])
    o[[paste(prefixe, sx, "A", sep = "|")]] <- sum(ex$exposition_centrale[i] * MU_VRAI[[sx]][ex$age[i] + 1L])
    for (a in ages) {
      q <- Q_VRAI[[sx]][a + 1L]
      jp <- po$sexe == sx & po$age == a; jk <- km$sexe == sx & km$age == a
      o[[paste(prefixe, sx, a, "poisson_exact", sep = "|")]] <-
        couvre(po$probabilite_borne_inf_exacte[jp], po$probabilite_borne_sup_exacte[jp], q)
      o[[paste(prefixe, sx, a, "poisson_asymptotique", sep = "|")]] <-
        couvre(po$probabilite_borne_inf_asymptotique[jp], po$probabilite_borne_sup_asymptotique[jp], q)
      o[[paste(prefixe, sx, a, "kaplan_meier", sep = "|")]] <-
        couvre(km$borne_inf_asymptotique[jk], km$borne_sup_asymptotique[jk], q)
    }
  }
  o
}

# Classes du jeu propre `j` sur la grille `g` (numérique, jours) : premier et
# dernier inventaire de présence kp, kd (présence à t_i : e <= t_i < s) ;
# P0, E (vue d'abord à t_f, f >= 2), ES (sans inventaire, t_1 < s <= t_M).
classes <- function(j, g) {
  e <- as.numeric(j$date_entree); s <- as.numeric(j$date_sortie)
  s[is.na(s) | j$cause_sortie == "en_cours"] <- Inf
  kp <- findInterval(e, g, left.open = TRUE) + 1L
  kd <- findInterval(s, g, left.open = TRUE)
  vu <- kd >= kp
  list(e = e, s = s, kp = kp, p0 = vu & kp == 1L, E = vu & kp >= 2L,
       ES = !vu & is.finite(s) & s > g[1L] & s <= g[length(g)])
}

# Oracle C1 : lignes reconstituées depuis le jeu propre (§ 14, C1 ; dans ce
# script, ADR 0014, option (I) écartée pour le moteur) : P0 entrée t_1 ; E
# entrée â (milieu : t_{f-1} + ceil(L/2) ; premier_inventaire : t_f) ; ES
# gardées sous milieu (â = t_i + ceil((s - t_i)/2)), écartées sinon ; autres
# unités (aucune ligne de stocks) écartées ; présentes au dernier inventaire
# en cours ; ordre des identifiants.
# Entrée imputée â sous `milieu` (§ 5) des unités E et ES de `cl`
# (classes()) ; NA ailleurs.
entree_milieu <- function(cl, g) {
  a <- rep(NA_real_, length(cl$e))
  f <- cl$kp[cl$E]
  a[cl$E] <- g[f - 1L] + ceiling((g[f] - g[f - 1L]) / 2)
  s_es <- cl$s[cl$ES]; i <- findInterval(s_es, g, left.open = TRUE)
  a[cl$ES] <- g[i] + ceiling((s_es - g[i]) / 2)
  a
}
reconstituer <- function(j, g, mode) {
  cl <- classes(j, g)
  a <- if (mode == "milieu") entree_milieu(cl, g) else rep(NA_real_, nrow(j))
  a[cl$p0] <- g[1L]
  if (mode != "milieu") a[cl$E] <- g[cl$kp[cl$E]]
  garde <- which(!is.na(a))
  garde <- garde[order(j$id_tete[garde], method = "radix")]
  en_cours <- !is.finite(cl$s[garde])
  data.frame(id_tete = j$id_tete[garde], sexe = j$sexe[garde], date_naissance = j$date_naissance[garde],
             date_entree = structure(a[garde], class = "Date"),
             date_sortie = structure(ifelse(en_cours, NA_real_, cl$s[garde]), class = "Date"),
             cause_sortie = ifelse(en_cours, "en_cours", j$cause_sortie[garde]), stringsAsFactors = FALSE)
}

# Prédiction du biais de `milieu` (en-tête) : par sexe, excès d'exposition
# imputée (années ; second ordre et géométrique) et Delta prédit (second
# ordre, sans puis avec le gradient d'âge).
prediction_milieu <- function(j, g) {
  cl <- classes(j, g)
  o <- list()
  for (sx in SEXES) {
    i <- which(cl$E & j$sexe == sx)
    f <- cl$kp[i]; L <- g[f] - g[f - 1L]
    x <- pmin(floor((cl$e[i] - as.numeric(j$date_naissance[i])) / 365.25), 119)
    mu_d <- MU_VRAI[[sx]][x + 1L]
    # gradient de mu_deces par année d'âge : différence centrée sur la table
    # (âges à l'entrée >= 20 dans les modes uniformes : x - 1 >= 0)
    grad <- (MU_VRAI[[sx]][x + 2L] - MU_VRAI[[sx]][x]) / 2
    lam <- (MU_RACHAT + mu_d) / 365.25
    arrondi <- (L + 1) / 2 - ceiling(L / 2)
    o2 <- arrondi + lam * (L^2 - 1) / 12
    v2 <- (L^2 - 1) / 12
    # E[e - t_{f-1}] sous poids r^(L - k), k = 1..L, r = exp(-lambda) :
    # L - sum m r^m / sum r^m, m = 0..L-1.
    r <- exp(-lam)
    sm <- r * (1 - L * r^(L - 1) + (L - 1) * r^L) / (1 - r)^2
    s0 <- (1 - r^L) / (1 - r)
    og <- (L - sm / s0) - ceiling(L / 2)
    o[[paste("pred", sx, "dE2", sep = "|")]] <- sum(o2) / 365.25
    o[[paste("pred", sx, "dEg", sep = "|")]] <- sum(og) / 365.25
    o[[paste("pred", sx, "Delta2", sep = "|")]] <- -sum(mu_d * o2) / 365.25
    o[[paste("pred", sx, "Delta2grad", sep = "|")]] <- -sum(mu_d * o2) / 365.25 + sum(grad * v2) / (2 * 365.25^2)
  }
  o
}

# Décomposition par classe de l'excès d'exposition de M (A2.4), par sexe :
# eps_E et eps_ES = somme de (e - â) / 365,25 sur les unités E et ES ; part
# réalisée des unités ES dans Delta : - somme sur ES de mu_deces(x) (e - â) /
# 365,25 (x : âge à l'entrée).
decomposition <- function(j, g) {
  cl <- classes(j, g); a <- entree_milieu(cl, g)
  x <- pmin(floor((cl$e - as.numeric(j$date_naissance)) / 365.25), 119)
  o <- list()
  for (sx in SEXES) {
    iE <- cl$E & j$sexe == sx; iES <- cl$ES & j$sexe == sx
    o[[paste("dec", sx, "epsE", sep = "|")]] <- sum((cl$e - a)[iE]) / 365.25
    o[[paste("dec", sx, "epsES", sep = "|")]] <- sum((cl$e - a)[iES]) / 365.25
    o[[paste("dec", sx, "DeltaES", sep = "|")]] <- -sum((MU_VRAI[[sx]][x + 1L] * (cl$e - a))[iES]) / 365.25
  }
  o
}

# Identité exacte de S4 (A2.2), par sexe : A_L - A_M égal à la perte et au
# gain d'exposition des unités E et ES du bloc aux âges AGE_S4 et AGE_S4 - 1
# (tolérance relative 1e-10) ; contrôle : naissances du bloc toutes égales.
identite_s4 <- function(bloc, g, o) {
  nais <- unique(as.numeric(bloc$date_naissance))
  res <- list(`id|S4|naissances_bloc_egales` = length(nais) == 1L)
  if (length(nais) != 1L) return(res)
  anniv <- as.numeric(seq(bloc$date_naissance[1L], by = "year", length.out = AGE_S4 + 2L))
  duree <- diff(anniv)                         # duree[a + 1] : jours de l'année d'âge a
  cl <- classes(bloc, g); l <- entree_milieu(cl, g) - cl$e
  for (sx in SEXES) {
    lu <- l[(cl$E | cl$ES) & bloc$sexe == sx]
    pred <- MU_VRAI[[sx]][AGE_S4 + 1L] * sum(lu[lu > 0]) / duree[AGE_S4 + 1L] -
      MU_VRAI[[sx]][AGE_S4] * sum(-lu[lu < 0]) / duree[AGE_S4]
    obs <- o[[paste("L", sx, "A", sep = "|")]] - o[[paste("M", sx, "A", sep = "|")]]
    res[[paste("id|S4|A_L-A_M", sx, sep = "|")]] <- abs(obs - pred) <= 1e-10 * abs(pred)
    res[[paste("diag|S4|ecart_relatif_A_L-A_M", sx, sep = "|")]] <- abs(obs / pred - 1)
  }
  res
}

# Identités d'une lecture stocks (T, M) contre L, et C6 (décomptes `dc` du
# simulateur) ; rend une liste de logiques (nommés id|...) et de diagnostics.
identites <- function(xl, xt, xm, dc, nom) {
  el <- xl$etapes$exposition$resultat$exposition
  et <- xt$etapes$exposition$resultat$exposition
  em <- xm$etapes$exposition$resultat$exposition
  cle <- function(e) paste(e$sexe, e$age)
  k <- match(cle(et), cle(el))
  c3 <- !anyNA(k) && all(et$exposition_centrale <= el$exposition_centrale[k]) && all(et$evenements <= el$evenements[k])
  dl <- el[el$evenements > 0L, ]; dm <- em[em$evenements > 0L, ]
  dm_ok <- identical(sort(cle(dl)), sort(cle(dm))) &&
    identical(dm$evenements[match(cle(dl), cle(dm))], dl$evenements)
  r <- xm$etapes$donnees$diagnostics$releves
  c6 <- identical(r$p0, dc$p0) && identical(r$entrees, dc$entrees) &&
    identical(unname(r$entres_sortis_par_cause[names(dc$entres_sortis)]), unname(dc$entres_sortis)) &&
    identical(r$inventaires$presents[nrow(r$inventaires)], dc$presents_dernier)
  dx <- xm$etapes$exposition$diagnostics
  o <- list()
  o[[paste("id", nom, "C3", sep = "|")]] <- c3
  o[[paste("id", nom, "DM", sep = "|")]] <- dm_ok
  o[[paste("id", nom, "C6", sep = "|")]] <- c6
  o[[paste("diag", nom, "imputee", sep = "|")]] <- dx$exposition_imputee_jours / dx$exposition_jours
  o[[paste("diag", nom, "entrees", sep = "|")]] <- dc$entrees
  o[[paste("diag", nom, "es", sep = "|")]] <- sum(dc$entres_sortis)
  o[[paste("diag", nom, "es_deces", sep = "|")]] <- dc$entres_sortis[["deces"]]
  o
}
oracle <- function(j, g, x, mode) {
  xr <- construire(reconstituer(j, g, mode), ROLES_L, plan_lecture(), "oracle C1 (lignes reconstituées)")
  a <- list(x$etapes$exposition$resultat, x$etapes$brut$resultat)
  b <- list(xr$etapes$exposition$resultat, xr$etapes$brut$resultat)
  c(identique = identical(a, b), tolerance = isTRUE(all.equal(a, b, tolerance = 1e-12)))
}

# --- un jeu ------------------------------------------------------------------------
# Jeu r du groupe k (S1 : S1 et S2 ; S3 ; S4) : liste nommée par scénario de
# listes plates de grandeurs.
un_jeu <- function(k, r, scen) {
  sj <- simuler_jeu(k, r); j <- sj$donnees
  xl <- construire(j, ROLES_L, plan_lecture(), paste(k, "L"))
  tetes <- tapply(j$id_tete, j$sexe, function(v) length(unique(v)))
  ages <- AGES_COUV[[k]]
  base <- c(list(`tetes|H` = unname(tetes["H"]), `tetes|F` = unname(tetes["F"])), lire(xl, "L", ages))
  avec_oracle <- k != "S3"
  res <- list()
  for (sc in scen) {
    g <- if (sc == "S2") G_DEUX else G_ANNUELLE
    ss <- simuler_jeu(k, r, g); st <- ss$donnees
    xt <- construire(st, ROLES_S, plan_lecture("premier_inventaire", g), paste(sc, "T"))
    xm <- construire(st, ROLES_S, plan_lecture("milieu", g), paste(sc, "M"))
    o <- c(base, lire(xt, "T", ages), lire(xm, "M", ages), identites(xl, xt, xm, ss$decomptes, sc),
           decomposition(j, as.numeric(g)))
    if (k != "S4") o <- c(o, prediction_milieu(j, as.numeric(g))) else o <- c(o, identite_s4(sj$bloc, as.numeric(g), o))
    if (avec_oracle) {
      ot <- oracle(j, as.numeric(g), xt, "premier_inventaire"); om <- oracle(j, as.numeric(g), xm, "milieu")
      o[["id|C1|T"]] <- ot[["identique"]]; o[["id|C1|M"]] <- om[["identique"]]
      o[["id|C1tol|T"]] <- ot[["tolerance"]]; o[["id|C1tol|M"]] <- om[["tolerance"]]
    }
    res[[sc]] <- o
  }
  res
}

# --- présentation ------------------------------------------------------------------
ligne_taux <- function(mesure, p0, succes) {
  produit <- !is.na(succes); R <- sum(produit)
  if (!R) return(data.frame(mesure = mesure, cible = p0, R = 0L, non_produits = length(succes), taux = NA,
                            et_nominale = NA, intervalle = "", verdict = "non produit", stringsAsFactors = FALSE))
  p <- mean(succes[produit]); e0 <- sqrt(p0 * (1 - p0) / R)
  data.frame(mesure = mesure, cible = p0, R = R, non_produits = sum(!produit), taux = round(p, 4),
             et_nominale = round(e0, 4), intervalle = sprintf("[%.4f ; %.4f]", p0 - 3 * e0, p0 + 3 * e0),
             verdict = if (abs(p - p0) <= 3 * e0) "dans l'intervalle" else if (p > p0) "hors (au-dessus)" else "hors (au-dessous)",
             stringsAsFactors = FALSE)
}
imprimer <- function(df) { o <- options(width = 250); on.exit(options(o)); print(df, row.names = FALSE, right = FALSE) }
resume <- function(v) sprintf("moyenne %.5g, min %.5g, max %.5g", mean(v), min(v), max(v))
moy_mc <- function(v) sprintf("%.5g (e.t. MC %.2g)", mean(v), sd(v) / sqrt(length(v)))
TESTS <- list()

rapport <- function(sc, m, n_tetes, ages) {
  col <- function(nm) m[, nm]
  R <- nrow(m)
  cat(sprintf("\n=== Scénario %s : %d jeux, n = %.0f têtes ===\n", sc, R, n_tetes))
  cat("\nErreur d'estimation (par jeu, lecture L) :\n")
  for (sx in SEXES) {
    dl <- col(paste("L", sx, "D", sep = "|"))
    cat(sprintf("  %s : têtes %s ; décès D %s ; exposition E (années) %s ; 1/sqrt(D) moyen %.4f\n", sx,
                resume(col(paste("tetes", sx, sep = "|"))), resume(dl), resume(col(paste("L", sx, "E", sep = "|"))),
                mean(1 / sqrt(dl))))
  }
  cat(sprintf("  classes (grille du scénario) : entrées E %s ; entrées-sorties ES %s (dont décès %s)\n",
              resume(col(paste("diag", sc, "entrees", sep = "|"))), resume(col(paste("diag", sc, "es", sep = "|"))),
              resume(col(paste("diag", sc, "es_deces", sep = "|")))))
  cat(sprintf("  part de l'exposition imputée sous M (exposition_imputee_jours / exposition_jours) : %s\n",
              resume(col(paste("diag", sc, "imputee", sep = "|")))))

  cat("\nÉcarts à L (moyenne sur les jeux, erreur type de Monte-Carlo) :\n")
  for (sx in SEXES) for (x in c("T", "M")) {
    eL <- col(paste("L", sx, "E", sep = "|")); dL <- col(paste("L", sx, "D", sep = "|"))
    eX <- col(paste(x, sx, "E", sep = "|")); dX <- col(paste(x, sx, "D", sep = "|"))
    cat(sprintf("  %s, %s : E_%s/E_L - 1 = %s, écart-type %.3g ; D_%s - D_L = %s ; D_%s/D_L - 1 = %s\n",
                sx, x, x, moy_mc(eX / eL - 1), sd(eX / eL - 1), x, moy_mc(dX - dL), x, moy_mc(dX / dL - 1)))
  }

  cat(sprintf("\nTests (E2.19) de H0 E[U] = 0, U = D - A : Z = Ubar sqrt(R) / s_U, région |Z| <= %.2f, fausse alarme 5 %% (asymptotique en R) :\n",
              Z_CRIT))
  lectures <- if (sc == "S2") "T" else c("L", "T")
  for (x in lectures) for (sx in SEXES) {
    U <- col(paste(x, sx, "D", sep = "|")) - col(paste(x, sx, "A", sep = "|"))
    Z <- mean(U) * sqrt(R) / sd(U)
    acc <- abs(Z) <= Z_CRIT
    TESTS[[length(TESTS) + 1L]] <<- data.frame(scenario = sc, lecture = x, sexe = sx, R = R, Ubar = round(mean(U), 3),
                                               s_U = round(sd(U), 3), Abar = round(mean(col(paste(x, sx, "A", sep = "|"))), 2),
                                               Ubar_sur_Abar = signif(mean(U) / mean(col(paste(x, sx, "A", sep = "|"))), 3),
                                               Z = round(Z, 3), verdict = if (acc) "accepté" else "rejeté",
                                               stringsAsFactors = FALSE)
    cat(sprintf("  %s, %s : Ubar %.3f, s_U %.3f, Z %.3f -> %s\n", x, sx, mean(U), sd(U), Z, if (acc) "accepté" else "rejeté"))
  }

  cat("\nBiais de M (hypothèse « entrée au milieu ») ; prédictions : développement au second ordre en L (asymptotique pour lambda L -> 0), dérivation propre non publiée :\n")
  fmt <- function(v) format(signif(v, 3), big.mark = " ", scientific = FALSE)
  for (sx in SEXES) {
    UM <- col(paste("M", sx, "D", sep = "|")) - col(paste("M", sx, "A", sep = "|"))
    UL <- col(paste("L", sx, "D", sep = "|")) - col(paste("L", sx, "A", sep = "|"))
    AM <- mean(col(paste("M", sx, "A", sep = "|"))); ED <- mean(col(paste("L", sx, "D", sep = "|")))
    Dl <- UM - UL
    b <- mean(UM) / AM; hb <- Z_CRIT * sd(UM) / sqrt(R) / AM
    bd <- mean(Dl) / AM; hd <- Z_CRIT * sd(Dl) / sqrt(R) / AM
    beta <- abs(bd) * sqrt(ED)
    cat(sprintf("  %s : biais total contre la loi vraie b = E[U_M]/E[A_M] = %.3g [%.3g ; %.3g]\n", sx, b, b - hb, b + hb))
    cat(sprintf("      biais propre à l'hypothèse d'entrée delta = E[Delta]/E[A_M] = %.3g [%.3g ; %.3g] (e.t. MC %.2g)\n",
                bd, bd - hd, bd + hd, sd(Dl) / sqrt(R) / AM))
    cat(sprintf("      beta = |delta| sqrt(E[D_L]) = %.3g (E[D_L] = %.1f) ; n* = n / beta^2 = %s têtes, intervalle [%s ; %s]\n",
                beta, ED, if (beta > 0) fmt(n_tetes / beta^2) else "non borné",
                fmt(n_tetes / ((abs(bd) + hd)^2 * ED)),
                if (abs(bd) > hd) fmt(n_tetes / ((abs(bd) - hd)^2 * ED)) else "non borné"))
    dE <- col(paste("M", sx, "E", sep = "|")) - col(paste("L", sx, "E", sep = "|"))
    eE <- col(paste("dec", sx, "epsE", sep = "|")); eES <- col(paste("dec", sx, "epsES", sep = "|"))
    resid <- dE - eE - eES
    cat(sprintf("      E_M - E_L (années) %s ; eps_E %s ; eps_ES %s ; résidu (E_M - E_L) - eps_E - eps_ES (unité 365,25 j contre années d'âge du moteur) : moyenne %.3g, max |.| %.3g\n",
                moy_mc(dE), moy_mc(eE), moy_mc(eES), mean(resid), max(abs(resid))))
    if (paste("pred", sx, "dE2", sep = "|") %in% colnames(m)) {
      p2 <- col(paste("pred", sx, "dE2", sep = "|")); pg <- col(paste("pred", sx, "dEg", sep = "|"))
      ec <- eE - p2
      cat(sprintf("      prédit (unités E) : second ordre %.5g, géométrique %.5g ; écart apparié eps_E - prédit (second ordre) : moyenne %.4g, e.t. MC %.2g, z %.2f\n",
                  mean(p2), mean(pg), mean(ec), sd(ec) / sqrt(R), mean(ec) / (sd(ec) / sqrt(R))))
      d2 <- col(paste("pred", sx, "Delta2", sep = "|")); d2g <- col(paste("pred", sx, "Delta2grad", sep = "|"))
      dES <- col(paste("dec", sx, "DeltaES", sep = "|"))
      cat(sprintf("      Delta / E[A_M] mesuré %.3g ; prédit : sans gradient d'âge %.3g, avec gradient %.3g, avec gradient + ES réalisé %.3g%s\n",
                  bd, mean(d2) / AM, mean(d2g) / AM, mean(d2g + dES) / AM,
                  if (sc == "S2") " (hors du domaine du développement : lambda L ~ 0,5)" else ""))
    } else cat("      prédiction sans objet (entrées non uniformes)\n")
    if (paste("diag|S4|ecart_relatif_A_L-A_M", sx, sep = "|") %in% colnames(m))
      cat(sprintf("      identité exacte A_L - A_M (A2.2) : écart relatif max %.3g (tolérance 1e-10)\n",
                  max(col(paste("diag|S4|ecart_relatif_A_L-A_M", sx, sep = "|")))))
  }

  cat("\nIdentités exactes (jeux où elle tient / jeux) :\n")
  ids <- grep("^id\\|", colnames(m), value = TRUE)
  for (nm in ids) {
    v <- as.logical(col(nm))
    cat(sprintf("  %-14s %d / %d\n", sub("^id\\|", "", nm), sum(v), R))
    if (!grepl("^id\\|C1tol", nm)) verifier(sprintf("%s : identité %s sur chaque jeu", sc, sub("^id\\|", "", nm)), all(v))
  }

  lignes <- list(); cles <- character(0)
  for (x in c("L", "T", "M")) for (sx in SEXES) for (a in ages)
    for (iv in c("poisson_exact", "poisson_asymptotique", "kaplan_meier")) {
      lignes[[length(lignes) + 1L]] <- ligne_taux(sprintf("%s %s âge %d, %s", x, sx, a, iv), NIVEAU,
                                                  as.logical(col(paste(x, sx, a, iv, sep = "|"))))
      cles <- c(cles, paste(sx, a, iv))
    }
  tab <- do.call(rbind, lignes)
  tab$note <- ifelse(tab$non_produits > 0L & tab$R > 0L, "conditionnelle à D >= 1", "")
  cat(sprintf("\nCouverture du q_x vrai (constats de simulation ; %d lignes ; R = jeux où l'intervalle est produit ; e.t. nominale = sqrt(0,95 x 0,05 / R) ; bande cible ± 3 e.t. descriptive, non un test à niveau fixé) :\n",
              nrow(tab)))
  imprimer(tab)
  hors <- unique(cles[grepl("^hors", tab$verdict)])
  if (length(hors)) {
    # L (exposition exacte) sert de témoin : une ligne hors bande en L est une
    # propriété de l'intervalle à cet effectif ; une ligne T ou M hors bande
    # alors que L est dans la bande est propre à la lecture (avis d'actuary).
    cat("  Lignes hors bande, L pris pour témoin (taux L / T / M) :\n")
    for (h in hors) {
      i <- which(cles == h)                     # dans l'ordre L, T, M
      hb <- grepl("^hors", tab$verdict[i])
      np <- max(tab$non_produits[i])
      note <- if (hb[1L])
        sprintf(paste(" ; hors bande en L : propriété de l'intervalle à cet effectif%s ; T, M hors bande :",
                      "%s, hors bande aussi en L : propriété de l'intervalle, non de la recette"),
                if (np > 0L) sprintf(" (non produit dans %d jeu(x) au plus)", np) else "",
                if (any(hb[-1L])) paste(c("T", "M")[hb[-1L]], collapse = ", ") else "aucune")
      else
        sprintf(" ; L dans la bande : propre à la lecture %s, à examiner",
                paste(c("T", "M")[hb[-1L]], collapse = ", "))
      cat(sprintf("    %s : %s%s\n", h, paste(format(tab$taux[i]), collapse = " / "), note))
    }
  }
}

# --- exécution -----------------------------------------------------------------------
proc <- if (file.exists("/proc/cpuinfo")) sub("^.*:\\s*", "", grep("^model name", readLines("/proc/cpuinfo"), value = TRUE)[1L]) else NA
nproc <- suppressWarnings(tryCatch(system2("nproc", stdout = TRUE), error = function(e) NA_character_))
cat(sprintf("Plateforme : %s ; %s ; processeur %s ; cœurs disponibles (nproc) %s, détectés (detectCores) %d ; BLAS %s ; calcul séquentiel\n",
            R.version.string, R.version$platform, proc, nproc[1L], parallel::detectCores(), extSoftVersion()[["BLAS"]]))
cat(sprintf("Paramètres : R = %d jeux (S1, S2), %d (S3), %d (S4) ; n = %.0f têtes (S1, S2, bloc de S4), %.0f (S3), %.0f (fond de S4, âge 45, entré le 2014-12-31) ; âge du bloc de S4 %g (entré le 2015-01-02) ; niveau %.2f ; scénarios %s\n",
            R_JEUX, R_JEUX_S3, R_JEUX_S4, N_TETES, N_TETES_S3, PF$S4_fond$n, AGE_S4, NIVEAU, paste(SCENARIOS, collapse = ", ")))
cat(sprintf("Graines : jeu r sous GRAINES_JEU[k] + r (%s ; S2 : jeux de S1 ; fond de S4 sous GRAINES_JEU[S4] + %d + r)\n",
            paste(names(GRAINES_JEU), GRAINES_JEU, sep = " = ", collapse = ", "), DECALAGE_FOND))
t_debut <- proc.time()[["elapsed"]]

GROUPES <- list(S1 = intersect(c("S1", "S2"), SCENARIOS), S3 = intersect("S3", SCENARIOS), S4 = intersect("S4", SCENARIOS))
for (k in names(GROUPES)) {
  scen <- GROUPES[[k]]
  if (!length(scen)) next
  Rk <- switch(k, S1 = R_JEUX, S3 = R_JEUX_S3, S4 = R_JEUX_S4)
  cat(sprintf("\n##### Tirage du groupe %s (scénarios %s) : %d jeux #####\n", k, paste(scen, collapse = ", "), Rk))
  t0 <- proc.time()[["elapsed"]]
  jeux <- vector("list", Rk); erreurs <- character(0); refus <- character(0)
  for (r in seq_len(Rk)) {
    jeux[[r]] <- tryCatch(un_jeu(k, r, scen),
                          refus_tablexp = function(e) {
                            refus <<- c(refus, sprintf("jeu %d : %s (%s)", r, conditionMessage(e), e$motif)); NULL },
                          error = function(e) { erreurs <<- c(erreurs, sprintf("jeu %d : %s", r, conditionMessage(e))); NULL })
    if (r == 1L) cat(sprintf("Premier jeu : %.2f s ; durée extrapolée pour %d jeux : %.1f min\n",
                             proc.time()[["elapsed"]] - t0, Rk, (proc.time()[["elapsed"]] - t0) * Rk / 60))
  }
  duree <- proc.time()[["elapsed"]] - t0
  ok <- !vapply(jeux, is.null, NA)
  cat(sprintf("Durée : %.1f min (%.2f s par jeu) ; jeux refusés par le moteur : %d ; jeux en erreur : %d\n",
              duree / 60, duree / Rk, length(refus), length(erreurs)))
  if (length(refus)) cat(paste("  refus :", utils::head(refus, 5L)), sep = "\n")
  if (length(erreurs)) cat(paste("  erreur :", utils::head(erreurs, 5L)), sep = "\n")
  # Erreur R : défaut du script ou du moteur, échec. Refus du moteur :
  # réponse documentée, décomptée ; aucun jeu exécuté : échec (comme
  # constat_positionnement.R).
  verifier(sprintf("groupe %s : aucun jeu en erreur", k), length(erreurs) == 0L)
  verifier(sprintf("groupe %s : au moins deux jeux exécutés", k), sum(ok) >= 2L)
  if (sum(ok) < 2L) {
    cat(sprintf("Scénario(s) %s non exécuté(s) : %d jeu(x) refusé(s) par le moteur sur %d.\n",
                paste(scen, collapse = ", "), length(refus), Rk))
    next
  }
  for (sc in scen) {
    # Matrice des jeux : colonnes indexées par les noms du premier jeu, après
    # contrôle que chaque jeu rend les mêmes grandeurs dans le même ordre.
    v <- lapply(jeux[ok], function(o) unlist(o[[sc]]))
    noms <- names(v[[1L]])
    verifier(sprintf("%s : mêmes grandeurs (noms, ordre) dans chaque jeu", sc),
             all(vapply(v, function(x) identical(names(x), noms), NA)))
    m <- do.call(rbind, lapply(v, function(x) x[noms]))
    rapport(sc, m, if (k == "S3") N_TETES_S3 else N_TETES, AGES_COUV[[k]])
  }
}

if (length(TESTS)) {
  tt <- do.call(rbind, TESTS)
  cat(sprintf("\nRécapitulatif des tests de H0 E[U] = 0 (L et T ; la lecture L de S2 est celle de S1, non recomptée) : %d tests au niveau 5 %%, %.2f fausse(s) alarme(s) attendue(s) sous H0, %d rejet(s) observé(s)\n",
              nrow(tt), 0.05 * nrow(tt), sum(tt$verdict == "rejeté")))
  imprimer(tt)
}
cat("\nAvertissements du résultat (x$avertissements : lecture, code ; nombre de constructions qui l'émettent) :\n")
if (length(AVERTIS$codes)) for (nm in names(AVERTIS$codes)) cat(sprintf("  %6d  %s\n", AVERTIS$codes[[nm]], nm)) else cat("  aucun\n")
cat("\nAvertissements R reçus (warning(), texte tronqué, décompte sur tous les scénarios) :\n")
if (length(AVERTIS$n)) for (nm in names(AVERTIS$n)) cat(sprintf("  %6d  %s\n", AVERTIS$n[[nm]], nm)) else cat("  aucun\n")
cat(sprintf("\nDurée totale : %.1f min\n", (proc.time()[["elapsed"]] - t_debut) / 60))
bilan()
