# -----------------------------------------------------------------------------
#  tests/unitaires/test_validation_positionnee.R : chemin p = 1 de l'étape
#  `validation`, validation de la table positionnée de la construction
#  (relation `positionnee` ; issue #167 ; spécification d'actuary-approfondi
#  décidée, § 8, M289, commentaire 6705863583 ; avis de forme (0''), F3, F5,
#  F9, M292, commentaire 6706003047 ; critères d'actuary du 07/11/2026)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : petit jeu J1 positionné (theta = 1,2),
#    lois binomiales par sommes directes de choose() (indépendantes de
#    pbinom et qbinom), constantes, motifs et ordre des refus, états,
#    mentions du rapport ;
#  - recoupement indépendant par `stats` (priorité base, en CI) : glm() de
#    Poisson à offset (déviance, X2 de Pearson, ddl = df.residual), binom.test()
#    (p-valeurs et bornes des sous-groupes d'un seul segment), loi de la somme
#    de deux binomiales par somme directe dbinom x pbinom ;
#  - identité exacte : somme des répliques multinomiales égale à O_s,
#    découpage par blocs, ordre de tirage documenté, attendus positionnés
#    indépendants de table_positionnee$qx, grille identique à celle du
#    positionnement, rejeu identical() hors execution.
# -----------------------------------------------------------------------------

D <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
erreur_de <- function(expr) tryCatch({ force(expr); NULL }, error = function(e) e)
sv <- tablexp:::statistiques_validation
valeur <- function(s, cle) s$valeur[match(cle, paste(s$statistique, s$grandeur, sep = "/"))]
etat_de <- function(s, cle) s$etat[match(cle, paste(s$statistique, s$grandeur, sep = "/"))]
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}
programmation <- function(e) {
  if (!is.null(e) && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE))
    TRUE
  else "pas d'erreur de programmation"
}
# Loi binomiale par somme directe (choose), indépendante de pbinom/qbinom.
loi_bin <- function(n, p) choose(n, 0:n) * p^(0:n) * (1 - p)^(n - 0:n)

# --- A. Constantes et statuts (valeur calculée à la main) ---------------------------

verifier("A RELATIONS_TABLE : positionnee = 1L (un paramètre par segment positionné), égal à PARAMETRES_ESTIMES_POSITIONNEMENT ; N_MAX_PRODUITS_CONVOLUTION = 1e8", {
  r <- tablexp:::RELATIONS_TABLE
  identical(names(r), c("independante", "ajustee_hors_outil", "positionnee")) && identical(r[["positionnee"]], 1L) &&
    identical(tablexp:::PARAMETRES_ESTIMES_POSITIONNEMENT, r[["positionnee"]]) &&
    identical(tablexp:::N_MAX_PRODUITS_CONVOLUTION, 1e8) &&
    "convolution_au_dela_n_max" %in% tablexp:::ETATS_SOUS_GROUPES_VALIDATION &&
    "contraint_par_positionnement" %in% tablexp:::ETATS_GRANDEUR_VALIDATION
})
verifier("A STATUTS_VALIDATION_POSITIONNEE : mêmes (statistique, grandeur, sous_h0) que STATUTS_VALIDATION ; ddl n - S ; khi-deux à n - S ; Monte-Carlo multinomial ; aucune attribution d'auteur (M200)", {
  a <- tablexp:::STATUTS_VALIDATION; b <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  k <- function(s, g) b$statut[b$statistique == s & b$grandeur == g]
  identical(a[c("statistique", "grandeur", "sous_h0")], b[c("statistique", "grandeur", "sous_h0")]) &&
    all(nzchar(b$statut)) && startsWith(k("khi2_pearson", "ddl"), "n - S") &&
    grepl("n - S degrés de liberté", k("deviance", "p_asymptotique"), fixed = TRUE) &&
    grepl("multinomiales (O_s ; pi_c)", k("runs", "p_monte_carlo"), fixed = TRUE) &&
    grepl("contraint_par_positionnement", k("smr", "p_centrale"), fixed = TRUE) &&
    grepl("loi non corrigée de l'estimation de theta", k("wilcoxon", "p_exacte"), fixed = TRUE) &&
    !any(grepl("Liddell|Garwood|Byar|Wilson|Hilferty|Clopper|Pearson \\(", b$statut)) &&
    all(grepl("sous l'unité contrat, les décès sont sur-dispersés et les tests sont anti-conservateurs",
              c(k("residus", "esperance_au_dela_2"), k("khi2_pearson", "p_asymptotique"), k("deviance", "p_monte_carlo"),
                k("deviance", "p_asymptotique"),
                k("sous_groupes", "bornes_exactes"), k("sous_groupes", "p_valeurs")), fixed = TRUE)) &&
    grepl("couverture >= niveau (intervalle exact conservatif)", k("sous_groupes", "bornes_exactes"), fixed = TRUE) &&
    grepl("si n - S = 0 (une cellule retenue par segment)", k("khi2_pearson", "ddl"), fixed = TRUE)
})
verifier("A STATUTS_VALIDATION et STATUTS_VALIDATION_POSITIONNEE (#168, m8) : tout statut non ASCII marqué UTF-8 (texte_moteur()), quelle que soit la locale d'installation", {
  nm <- function(d) { s <- d$statut[grepl("[^\001-\177]", d$statut, useBytes = TRUE)]
                      length(s) > 0L && all(Encoding(s) == "UTF-8") && all(validUTF8(s)) }
  tous(v = if (nm(tablexp:::STATUTS_VALIDATION)) TRUE else "STATUTS_VALIDATION",
       p = if (nm(tablexp:::STATUTS_VALIDATION_POSITIONNEE)) TRUE else "STATUTS_VALIDATION_POSITIONNEE")
})
verifier("A STATUTS_VALIDATION_POSITIONNEE (M309) : Wilcoxon, R2 et distance nomment la table positionnée (mu_pos, q_pos), jamais mu^ - mu~ ni q~ ; ddl conditionnel (si n - S = 0) ; espace avant chaque point-virgule ; STATUTS_VALIDATION inchangé octet pour octet (empreinte MD5 figée)", {
  a <- tablexp:::STATUTS_VALIDATION; b <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  k <- function(s, g) b$statut[b$statistique == s & b$grandeur == g]
  pos <- c(k("wilcoxon", "effectif"), k("wilcoxon", "groupes_ex_aequo"), k("wilcoxon", "w_plus"), k("r2", "valeur"))
  # Empreinte des statuts de la relation independante (et ajustee_hors_outil)
  # avant M309, mise à jour par #179 (ligne khi2_pearson/variance_h0 et
  # textes des deux p asymptotiques ; avant #179 : 3e1b88019ff1d3a567acbd3c37aed3ec)
  # puis par #178 (ligne sous_groupes/p_valeurs_asymptotiques, texte propre à
  # chaque relation, condition 1 du visa d'actuary ; avant #178 :
  # 937ad009c51822ad825fff9a3c560527),
  # valeur écrite en dur (identité exacte) : octets bruts des
  # chaînes (charToRaw, sans conversion), champs séparés par une tabulation,
  # lignes par un saut de ligne ; indépendante de la locale d'installation
  # et de session.
  champs <- function(i) lapply(list(a$statistique[i], a$grandeur[i], if (a$sous_h0[i]) "TRUE" else "FALSE",
                                    a$statut[i]), charToRaw)
  ligne <- function(i) unlist(Map(c, champs(i), list(as.raw(9L), as.raw(9L), as.raw(9L), raw(0))))
  octets <- unlist(Map(c, lapply(seq_len(nrow(a)), ligne), c(rep(list(as.raw(10L)), nrow(a) - 1L), list(raw(0)))))
  f <- tempfile(); on.exit(unlink(f))
  writeBin(octets, f)
  tous(
    wilcoxon_r2 = if (all(grepl("mu^ - mu_pos", pos, fixed = TRUE)) &&
                      !any(grepl("mu~)", pos, fixed = TRUE) | grepl("mu^ - mu~", pos, fixed = TRUE)) &&
                      all(grepl("mu_pos = theta^_s mu~", pos[c(1L, 4L)], fixed = TRUE))) TRUE else "mu_pos absent",
    distance = if (grepl("|q^ - q_pos|, q^ = 1 - exp(-mu^), q_pos = 1 - exp(-theta^_s mu~)",
                         k("distance_probabilites", "valeur"), fixed = TRUE) &&
                   !grepl("q~", k("distance_probabilites", "valeur"), fixed = TRUE)) TRUE else "q_pos absent",
    ddl = if (all(vapply(c("khi2_pearson", "deviance"), function(s)
                grepl("S segments ; si n - S = 0 (une cellule retenue par segment) : loi conditionnelle dégénérée",
                      k(s, "ddl"), fixed = TRUE, useBytes = TRUE) && !grepl("ddl = n - S = 0", k(s, "ddl"), fixed = TRUE) &&
                grepl("de Monte-Carlo, erreurs de Monte-Carlo : état contraint_par_positionnement)", k(s, "ddl"),
                      fixed = TRUE, useBytes = TRUE), NA)))
            TRUE else "ddl non conditionnel",
    point_virgule = if (!any(grepl("[^ ];", b$statut, useBytes = TRUE)) &&
                        grepl("pbinom ; table positionnée sur ces données", k("signes", "p_exacte"), fixed = TRUE,
                              useBytes = TRUE))
                      TRUE else paste(grep("[^ ];", b$statut, useBytes = TRUE, value = TRUE), collapse = " | "),
    independante = if (identical(unname(tools::md5sum(f)), "01c6538a2b101311732eed745fcb3c05")) TRUE
                   else "STATUTS_VALIDATION modifié")
})
verifier("A statuts_validation() : switch à trois valeurs, erreur de programmation par défaut", tous(
  i = if (identical(tablexp:::statuts_validation("independante"), tablexp:::STATUTS_VALIDATION)) TRUE else "independante",
  a = if (identical(tablexp:::statuts_validation("ajustee_hors_outil"), tablexp:::STATUTS_VALIDATION)) TRUE else "ajustee",
  p = if (identical(tablexp:::statuts_validation("positionnee"), tablexp:::STATUTS_VALIDATION_POSITIONNEE)) TRUE
      else "positionnee",
  d = programmation(erreur_de(tablexp:::statuts_validation("autre")))))

# --- B. J1 positionné (valeur calculée à la main) ------------------------------------
# J1 (test_validation.R) : E^c = (128, 256, 512, 64), mu~ = (1/256, 1/128,
# 1/128, 7/128), D = (0, 3, 5, 4) ; A~ = (0,5 ; 2 ; 4 ; 3,5), O = 12, A~_s =
# 10, theta^ = 1,2 ; table positionnée : A = (0,6 ; 2,4 ; 4,8 ; 4,2), pi =
# (0,05 ; 0,2 ; 0,4 ; 0,35) ; une séquence, un segment (S = 1), ddl = 3.
J1 <- list(E = c(128, 256, 512, 64), mu = c(1 / 256, 1 / 128, 1 / 128, 7 / 128), D = c(0L, 3L, 5L, 4L))
TH <- 1.2
pos_j <- function(B = 99, graine = 1L, relation = "positionnee", segment = rep(1L, 4L))
  sv(J1$D, J1$E, -expm1(-TH * J1$mu), TH * J1$mu, TH * (J1$E * J1$mu), rep("retenue", 4L), rep(1L, 4L), 1:4, 0.95,
     relation, B, graine, segment)
SP <- pos_j()
sp <- SP$statistiques
PI <- c(0.05, 0.2, 0.4, 0.35); AP <- c(0.6, 2.4, 4.8, 4.2)
verifier("J1 positionné : X2 = 0,6 + 0,15 + 0,04/4,8 + 0,04/4,2 ; G2 = 2 somme [D ln(D/A) - (D - A)] ; ddl = 3 (n - S, S = 1) ; p asymptotiques sur 3 ddl", {
  x2 <- 0.6 + 0.15 + 0.04 / 4.8 + 0.04 / 4.2
  g2 <- 2 * sum(c(0, 3 * log(3 / 2.4), 5 * log(5 / 4.8), 4 * log(4 / 4.2)) - (J1$D - AP))
  tous(x2 = proche(valeur(sp, "khi2_pearson/statistique"), x2, 1e-14, 0),
       g2 = proche(valeur(sp, "deviance/statistique"), g2, 1e-13, 0),
       ddl = proche(valeur(sp, c("khi2_pearson/ddl", "deviance/ddl")), c(3, 3), 0, 0),
       px = proche(valeur(sp, "khi2_pearson/p_asymptotique"), stats::pchisq(x2, 3, lower.tail = FALSE), 1e-12, 0),
       pg = proche(valeur(sp, "deviance/p_asymptotique"), stats::pchisq(g2, 3, lower.tail = FALSE), 1e-12, 0))
})
verifier("J1 positionné : SMR global O = 12, A = 12, SMR = 1, écart 0 (descriptifs) ; bornes, p-valeurs, racine cubique NA, état contraint_par_positionnement", {
  ns <- c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure", "p_centrale", "z_racine_cubique",
          "p_racine_cubique")
  k <- paste0("smr/", ns)
  tous(o = proche(valeur(sp, "smr/observes"), 12, 0, 0), a = proche(valeur(sp, "smr/attendus"), 12, 1e-14, 0),
       s = proche(valeur(sp, "smr/smr"), 1, 1e-14, 0), e = proche(valeur(sp, "smr/ecart"), 0, 0, 1e-13),
       na = if (all(is.na(valeur(sp, k))) && all(etat_de(sp, k) == "contraint_par_positionnement")) TRUE else "état")
})
verifier("J1 positionné : fluctuation exacte [qbinom(0,025 ; 12 ; pi), qbinom(0,975 ; 12 ; pi)] et espérance hors intervalle, par sommes directes de la loi binomiale", {
  L <- U <- numeric(4); eh <- 0
  for (c_ in 1:4) {
    f <- cumsum(loi_bin(12, PI[c_]))
    L[c_] <- min(which(f >= 0.025)) - 1; U[c_] <- min(which(f >= 0.975)) - 1
    eh <- eh + sum(loi_bin(12, PI[c_])[-((L[c_]:U[c_]) + 1)])
  }
  cl <- SP$cellules
  tous(L = proche(cl$fluctuation_inf_exacte, L, 0, 0), U = proche(cl$fluctuation_sup_exacte, U, 0, 0),
       hors = proche(valeur(sp, "fluctuation/hors_intervalle_exact"), sum(J1$D < L | J1$D > U), 0, 0),
       esp = proche(valeur(sp, "fluctuation/esperance_hors_intervalle_exact"), eh, 1e-12, 0))
})
verifier("J1 positionné : fluctuation asymptotique A -+ z sqrt(A (1 - pi)) ; espérances binomiales (au-delà de 2 sqrt(A), signes positifs)", {
  z <- stats::qnorm(0.975)
  la <- AP - z * sqrt(AP * (1 - PI)); ua <- AP + z * sqrt(AP * (1 - PI))
  hors <- function(l, u) sum(vapply(1:4, function(c_) sum(loi_bin(12, PI[c_])[(0:12 < l[c_] | 0:12 > u[c_])]), 0))
  pos <- sum(vapply(1:4, function(c_) sum(loi_bin(12, PI[c_])[0:12 > AP[c_] * (1 + 1e-10)]), 0))
  cl <- SP$cellules
  tous(la = proche(cl$fluctuation_inf_asymptotique, la, 1e-14, 0), ua = proche(cl$fluctuation_sup_asymptotique, ua, 1e-14, 0),
       ea = proche(valeur(sp, "fluctuation/esperance_hors_intervalle_asymptotique"), hors(la, ua), 1e-12, 0),
       e2 = proche(valeur(sp, "residus/esperance_au_dela_2"), hors(AP - 2 * sqrt(AP), AP + 2 * sqrt(AP)), 1e-12, 0),
       sp = proche(valeur(sp, "signes/esperance_positifs_h0"), pos, 1e-12, 0))
})
verifier("J1 positionné : statuts de la colonne statut = STATUTS_VALIDATION_POSITIONNEE ; états dans ETATS_GRANDEUR_VALIDATION ; B tirages", {
  b <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  b <- b[b$statistique != "sous_groupes", ]
  identical(sp$statut, b$statut) && all(sp$etat %in% tablexp:::ETATS_GRANDEUR_VALIDATION) &&
    identical(SP$tirages_effectues, 99L)
})
verifier("statistiques_validation() : relation inconnue -> erreur de programmation ; segment absent sous positionnee, ou présent sous independante -> erreur de programmation", tous(
  r = programmation(erreur_de(pos_j(relation = "autre", segment = NULL))),
  a = programmation(erreur_de(pos_j(segment = NULL))),
  b = programmation(erreur_de(pos_j(relation = "independante")))))

# --- C. Réplications multinomiales (identité exacte) ---------------------------------

PO <- c(1, 2, 3, 4, 5); GR <- c(1L, 1L, 1L, 2L, 2L); EF <- c(10, 7)
f_somme <- function(Dm) list(g1 = colSums(Dm[1:3, , drop = FALSE]), g2 = colSums(Dm[4:5, , drop = FALSE]),
                             m = Dm[1L, ], negatif = colSums(Dm < 0 | Dm != trunc(Dm)))
verifier("multinomiales : dans chaque réplique, la somme de D* sur chaque groupe vaut exactement O_g (entiers, aucun négatif)", {
  r <- tablexp:::replications_multinomiales(PO, GR, EF, 500, 11L, f_somme)
  identical(r$g1, rep(10, 500)) && identical(r$g2, rep(7, 500)) && all(r$negatif == 0)
})
verifier("multinomiales : découpage par blocs sans effet ; ordre de tirage documenté (réplique par réplique, puis groupe par groupe, rmultinom(1, ...)) ; autre graine, autres tirages", {
  r1 <- tablexp:::replications_multinomiales(PO, GR, EF, 97, 11L, f_somme)
  r2 <- tablexp:::replications_multinomiales(PO, GR, EF, 97, 11L, f_somme, elements_bloc = 7 * length(PO))
  m <- tablexp:::sous_graine(11L, vapply(seq_len(97), function(j) {
    a <- stats::rmultinom(1L, 10, PO[1:3] / sum(PO[1:3]))
    stats::rmultinom(1L, 7, PO[4:5] / sum(PO[4:5]))
    a[1L]
  }, 0))
  r3 <- tablexp:::replications_multinomiales(PO, GR, EF, 97, 12L, f_somme)
  identical(r1, r2) && identical(r1$m, m) && !identical(r1$m, r3$m)
})
verifier("multinomiales : état de l'aléa de l'appelant restauré ; poids nul ou groupe hors de 1..G -> erreur de programmation", {
  set.seed(42); avant <- .Random.seed
  tablexp:::replications_multinomiales(PO, GR, EF, 9, 3L, f_somme)
  isTRUE(identical(.Random.seed, avant)) &&
    isTRUE(programmation(erreur_de(tablexp:::replications_multinomiales(c(0, PO[-1]), GR, EF, 9, 3L, f_somme)))) &&
    isTRUE(programmation(erreur_de(tablexp:::replications_multinomiales(PO, c(GR[-5], 3L), EF, 9, 3L, f_somme))))
})
verifier("multinomiales (#168) : effectif manquant ou négatif -> erreur de programmation (et non l'erreur de rmultinom())", tous(
  na = programmation(erreur_de(tablexp:::replications_multinomiales(PO, GR, c(10, NA), 9, 3L, f_somme))),
  negatif = programmation(erreur_de(tablexp:::replications_multinomiales(PO, GR, c(10, -1), 9, 3L, f_somme)))))
verifier("multinomiales (#168) : groupe sans cellule d'effectif positif -> erreur de programmation ; d'effectif nul, sauté sans tirage (tirages identiques à ceux sans le groupe)", {
  # Groupe 2 sans cellule : GR ne prend que 1 et 3 (G = 3).
  g3 <- c(1L, 1L, 1L, 3L, 3L)
  f3 <- function(Dm) list(m = Dm[1L, ], g3 = colSums(Dm[4:5, , drop = FALSE]))
  e <- erreur_de(tablexp:::replications_multinomiales(PO, g3, c(10, 2, 7), 9, 11L, f3))
  a <- tablexp:::replications_multinomiales(PO, g3, c(10, 0, 7), 97, 11L, f3)
  b <- tablexp:::replications_multinomiales(PO, GR, EF, 97, 11L, f_somme)
  tous(e = programmation(e),
       m = if (grepl("groupe d'effectif positif sans cellule", conditionMessage(e), fixed = TRUE)) TRUE
           else conditionMessage(e),
       nul = if (identical(a$m, b$m) && identical(a$g3, rep(7, 97))) TRUE else "groupe vide d'effectif nul")
})
verifier("J1 positionné : reproductible au bit près à graine égale ; Monte-Carlo par réplications multinomiales (p-valeurs dans ]0, 1])", {
  a <- pos_j(B = 199, graine = 7L); b <- pos_j(B = 199, graine = 7L)
  p <- valeur(a$statistiques, c("khi2_pearson/p_monte_carlo", "deviance/p_monte_carlo", "runs/p_monte_carlo"))
  identical(a, b) && all(p > 0 & p <= 1)
})

# --- D. Loi d'une somme de binomiales (valeur calculée à la main ; recoupement) ----

verifier("loi_somme_binomiales : Bin(30 ; 0,2) + Bin(50 ; 0,7) égale à la somme directe dbinom x dbinom, total 1 ; une seule binomiale identical() à dbinom", {
  l <- tablexp:::loi_somme_binomiales(c(30, 50), c(0.2, 0.7))
  ref <- vapply(0:80, function(o) sum(stats::dbinom(0:30, 30, 0.2) * stats::dbinom(o - 0:30, 50, 0.7)), 0)
  tous(loi = proche(c(l), ref, 1e-12, 1e-300), total = proche(sum(l), 1, 1e-14, 0),
       un = if (identical(c(tablexp:::loi_somme_binomiales(40, 0.3)), stats::dbinom(0:40, 40, 0.3))) TRUE else "dbinom")
})
verifier("cout_convolution_binomiales() (calculé avant la convolution) = nombre de produits effectués par loi_somme_binomiales(), sur deux cas", {
  cas <- list(list(c(30, 50, 20), c(0.2, 0.7, 0.5)), list(c(2000, 3000), c(0.01, 0.4)))
  all(vapply(cas, function(a) identical(tablexp:::cout_convolution_binomiales(a[[1]], a[[2]]),
                                        attr(tablexp:::loi_somme_binomiales(a[[1]], a[[2]]), "produits")), NA)) &&
    tablexp:::cout_convolution_binomiales(30, 0.2) == 0
})
# Sous-groupe « tranche » de k segments, chacun de deux cellules retenues
# (O_s = 2 x o, A égaux : pi_b = 0,5), la première dans le sous-groupe.
groupe_k <- function(k, o) {
  seg <- rep(seq_len(k), each = 2L)
  tablexp:::sous_groupes_positionnee("tranche", rep(c(1L, 2L), k), 2L, seg, rep(TRUE, 2L * k), rep(o, 2L * k),
                                     rep(o, 2L * k), c(k * o, k * o), c(k * o, k * o), 0.95)
}
verifier("A2 : 2 segments x O_s = 50 000 (7,4e7 produits) -> p-valeurs produites, état bornes_non_definies ; 8 x 12 500 (5,1e8 produits) -> p exactes NA, p asymptotiques produites (formule pnorm, p centrale plafonnée à 1 : 2 x 0,50126 > 1 ; #178), état convolution_au_dela_n_max", {
  a <- groupe_k(2L, 25000); b <- groupe_k(8L, 6250)
  # Somme de 8 Bin(12 500 ; 0,5), o = 50 000 = moyenne : sigma = sqrt(8 x 3125).
  s <- sqrt(8 * 12500 * 0.25)
  inf <- stats::pnorm(0.5 / s); sup <- stats::pnorm(-0.5 / s, lower.tail = FALSE)
  identical(a$etat, rep("bornes_non_definies", 2L)) && !anyNA(a$p_inferieure) && all(is.na(a$borne_inf_exacte)) &&
    identical(b$etat, rep("convolution_au_dela_n_max", 2L)) && all(is.na(c(b$p_inferieure, b$p_superieure, b$p_centrale))) &&
    identical(b$smr, c(1, 1)) &&
    isTRUE(proche(c(b$p_inferieure_asymptotique, b$p_superieure_asymptotique), c(inf, inf, sup, sup), 1e-15, 0)) &&
    2 * min(inf, sup) > 1 && identical(b$p_centrale_asymptotique, c(1, 1))
})
verifier("C1 : sous-groupe de deux segments dont un seul est complet -> convolution (le segment complet entre avec pi = 1, loi dégénérée en O_s) ; p = P(Bin(4 ; 0,5) <= 2)", {
  # Segment 1 : cellules 1 et 2 (D = 3, 2) ; segment 2 : cellules 3 et 4 (D =
  # 2, 2). Groupe 1 : cellules 1, 2 (segment 1 complet) et 3 (pi = 0,5).
  r <- tablexp:::sous_groupes_positionnee("tranche", c(1L, 1L, 1L, 2L), 2L, c(1L, 1L, 2L, 2L), rep(TRUE, 4L),
                                          c(3, 2, 2, 2), c(2.5, 2.5, 2, 2), c(7, 2), c(7, 2), 0.95)
  tous(etat = if (identical(r$etat[1L], "bornes_non_definies")) TRUE else r$etat[1L],
       pinf = proche(r$p_inferieure[1L], stats::pbinom(2, 4, 0.5), 1e-14, 0),
       psup = proche(r$p_superieure[1L], stats::pbinom(1, 4, 0.5, lower.tail = FALSE), 1e-14, 0))
})

# --- E. Construction : positionnement puis validation positionnee -------------------

PERIODE <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
JEU <- local({
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 20000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
  simuler_portefeuille(loi, pf, PERIODE, 20261106L)
})
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
T_HF <- table_reference(rbind(data.frame(age = GM$age, sexe = "H", qx = pmin(1, 1.2 * GM$qx)),
                              data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx)),
                        "GM par sexe (H majorée)", "importee", "test")
POS <- list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95)
VAL <- list(table_reference = "tv", relation_table = "positionnee", plage_ages = c(20, 90),
            tranches_age = c(20, 50, 70), niveau = 0.95, replications = 99)
plan_vp <- function(segmentation = "sexe", par_annee = FALSE, pos = POS, val = VAL, ie = FALSE) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PERIODE, delai_declaration = 0,
            segmentation = segmentation, graine = 20261116L,
            etapes = list(donnees = list(exclusions = character(0)),
                          exposition = list(convention = "plus_fine", par_annee = par_annee)))
  if (!is.null(pos)) p$etapes$positionnement <- pos
  p$etapes$validation <- val
  if (ie) p$etapes$importance_exclusions <- list(niveau = 0.95)
  p
}
X <- construire_table(JEU, ROLES, plan_vp(), tables_reference = list(tv = T_HF))
P <- X$etapes$positionnement
V <- X$etapes$validation
CV <- V$resultat$cellules
RV <- CV$etat == "retenue"
ST <- V$resultat$statistiques

verifier("étape : grille de la validation identique à celle du positionnement (clés, âges, E^c, D, q~, états), identité exacte", {
  colonnes <- c("sexe", "age", "exposition_centrale", "evenements", "qx_table", "taux_table", "etat")
  identical(CV[colonnes], P$resultat$cellules[colonnes])
})
verifier("étape : A_c = theta^_s A~_c (force de la référence multipliée par theta^, identité exacte) ; somme des A_c = O_s par segment (1e-12)", {
  th <- P$resultat$smr$smr[match(CV$sexe, P$resultat$smr$sexe)]
  o <- tapply(as.numeric(CV$evenements[RV]), CV$sexe[RV], sum)
  a <- tapply(CV$attendus[RV], CV$sexe[RV], sum)
  tous(a = if (identical(CV$attendus, th * P$resultat$cellules$attendus) && identical(CV$smr_positionnement, th) &&
               identical(CV$taux_positionne, th * CV$taux_table)) TRUE else "A_c",
       somme = proche(a[P$resultat$smr$sexe], P$resultat$smr$observes, 1e-12, 0),
       o = proche(o[P$resultat$smr$sexe], P$resultat$smr$observes, 0, 0))
})
local({
  # Recoupement : glm de Poisson à offset log(A~_c), un coefficient par
  # segment (theta^ par maximum de vraisemblance) : déviance = G2, somme des
  # résidus de Pearson au carré = X2, df.residual = n - S.
  d <- data.frame(D = CV$evenements[RV], A = CV$exposition_centrale[RV] * CV$taux_table[RV],
                  seg = factor(CV$sexe[RV], levels = P$resultat$smr$sexe))
  fit <- stats::glm(D ~ 0 + seg, family = stats::poisson(), data = d, offset = log(d$A),
                    control = stats::glm.control(epsilon = 1e-14, maxit = 100))
  verifier("recoupement : G2 = deviance(glm), X2 = Pearson(glm), ddl = df.residual = n - S (S = 2) ; p asymptotiques sur n - S ddl", tous(
    conv = if (isTRUE(fit$converged)) TRUE else "glm non convergé",
    g2 = proche(valeur(ST, "deviance/statistique"), stats::deviance(fit), 1e-10, 0),
    x2 = proche(valeur(ST, "khi2_pearson/statistique"), sum(stats::residuals(fit, type = "pearson")^2), 1e-10, 0),
    ddl = proche(valeur(ST, c("khi2_pearson/ddl", "deviance/ddl")), rep(fit$df.residual, 2L), 0, 0),
    n_s = proche(fit$df.residual, sum(RV) - 2, 0, 0),
    p = proche(valeur(ST, "deviance/p_asymptotique"),
               stats::pchisq(valeur(ST, "deviance/statistique"), sum(RV) - 2, lower.tail = FALSE), 1e-12, 0)))
})
verifier("dénombrements (#174) : sous_groupes$cellules_retenues en integer sous positionnee, somme par regroupement égale à celle des cellules retenues", {
  g <- V$resultat$sous_groupes
  tous(type = if (identical(typeof(g$cellules_retenues), "integer")) TRUE else typeof(g$cellules_retenues),
       diag = if (identical(typeof(V$diagnostics$cellules_retenues), "integer")) TRUE else typeof(V$diagnostics$cellules_retenues),
       somme = proche(tapply(g$cellules_retenues, g$regroupement, sum)[c("segment", "tranche", "segment_tranche")],
                      rep(sum(RV), 3L), 0, 0))
})
verifier("étape : SMR global = 1 (1e-12), bornes et p-valeurs NA (contraint_par_positionnement) ; B tirages, graine du plan", tous(
  smr = proche(valeur(ST, "smr/smr"), 1, 1e-12, 0),
  na = if (all(etat_de(ST, c("smr/borne_inf_exacte", "smr/p_centrale", "smr/z_racine_cubique")) ==
               "contraint_par_positionnement")) TRUE else "états",
  mc = if (identical(V$resultat$monte_carlo$tirages_effectues, 99L) &&
           identical(V$resultat$monte_carlo$graine, 20261116L)) TRUE else "Monte-Carlo"))
verifier("étape : paramètres (relation positionnee, p = 1 par segment, hypothèse nulle et statuts de la relation) ; statut des statistiques", {
  p <- V$parametres
  s <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  identical(p$relation_table, "positionnee") && identical(p$parametres_estimes, 1L) &&
    identical(p$hypothese_nulle, tablexp:::HYPOTHESE_VALIDATION_POSITIONNEE) &&
    identical(p$statuts_sous_groupes$statut, s$statut[s$statistique == "sous_groupes"]) &&
    identical(ST$statut, s$statut[s$statistique != "sous_groupes"])
})
local({
  sg <- V$resultat$sous_groupes
  O_s <- tapply(as.numeric(CV$evenements[RV]), CV$sexe[RV], sum)
  # pi_b recalculé de la table et de l'exposition : A~ = E^c (-log(1 - q~)).
  qt <- T_HF$qx[match(paste(CV$sexe, CV$age), paste(T_HF$sexe, T_HF$age))]
  at <- ifelse(RV, CV$exposition_centrale * -log(1 - qt), 0)
  pib <- function(s, t) sum(at[CV$sexe == s & CV$tranche_age == t]) / sum(at[CV$sexe == s])
  st <- sg[sg$regroupement == "segment_tranche", ]
  bt <- lapply(seq_len(nrow(st)), function(i) {
    n <- O_s[[st$sexe[i]]]; p <- pib(st$sexe[i], st$tranche_debut[i])
    list(inf = stats::binom.test(st$observes[i], n, p, alternative = "less")$p.value,
         sup = stats::binom.test(st$observes[i], n, p, alternative = "greater")$p.value,
         ci = stats::binom.test(st$observes[i], n, conf.level = 0.95)$conf.int / p)
  })
  verifier("recoupement : sous-groupes segment x tranche : p-valeurs = binom.test(O_b, O_s, pi_b), bornes = intervalle exact de binom.test / pi_b (1e-10) ; état produite", tous(
    inf = proche(st$p_inferieure, vapply(bt, `[[`, 0, "inf"), 1e-10, 0),
    sup = proche(st$p_superieure, vapply(bt, `[[`, 0, "sup"), 1e-10, 0),
    bi = proche(st$borne_inf_exacte, vapply(bt, function(b) b$ci[1L], 0), 1e-10, 0),
    bs = proche(st$borne_sup_exacte, vapply(bt, function(b) b$ci[2L], 0), 1e-10, 0),
    etat = if (all(st$etat == "produite")) TRUE else "état"))
  tr <- sg[sg$regroupement == "tranche", ]
  direct <- t(vapply(seq_len(nrow(tr)), function(i) {
    nf <- O_s[["F"]]; nh <- O_s[["H"]]; o <- tr$observes[i]
    pf <- pib("F", tr$tranche_debut[i]); ph <- pib("H", tr$tranche_debut[i])
    c(sum(stats::dbinom(0:nf, nf, pf) * stats::pbinom(o - 0:nf, nh, ph)),
      sum(stats::dbinom(0:nf, nf, pf) * stats::pbinom(o - 1 - 0:nf, nh, ph, lower.tail = FALSE)))
  }, numeric(2)))
  verifier("recoupement : sous-groupes tranche (deux segments) : p-valeurs = loi de Bin(O_F, pi_F) + Bin(O_H, pi_H) par somme directe (1e-10) ; bornes NA, état bornes_non_definies", tous(
    inf = proche(tr$p_inferieure, direct[, 1L], 1e-10, 0), sup = proche(tr$p_superieure, direct[, 2L], 1e-10, 0),
    cen = proche(tr$p_centrale, pmin(1, 2 * pmin(direct[, 1L], direct[, 2L])), 1e-10, 0),
    na = if (all(is.na(tr$borne_inf_exacte) & is.na(tr$borne_sup_exacte)) && all(tr$etat == "bornes_non_definies")) TRUE
         else "bornes ou état"))
  verifier("sous-groupes : tout état dans ETATS_SOUS_GROUPES_VALIDATION",
           all(sg$etat %in% tablexp:::ETATS_SOUS_GROUPES_VALIDATION))
  se <- sg[sg$regroupement == "segment", ]
  verifier("sous-groupes segment : SMR = 1 par construction (1e-12), bornes et p-valeurs NA, état contraint_par_positionnement", tous(
    smr = proche(se$smr, c(1, 1), 1e-12, 0),
    na = if (all(is.na(unlist(se[c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure",
                                   "p_centrale")]))) && all(se$etat == "contraint_par_positionnement")) TRUE else "état"))
})
verifier("critère d'actuary : les attendus positionnés ne lisent pas table_positionnee$qx (qx remplacé par NA : validation identical())", {
  e <- X$etapes
  appel <- function(pos) tablexp:::calculer_validation(e$exposition$resultat$exposition, "sexe", FALSE,
                                                       "dernier_anniversaire", T_HF, "tv", "positionnee", c(20, 90),
                                                       c(20, 50, 70), 0.95, 99, e$validation$resultat$monte_carlo$graine, pos)
  p2 <- e$positionnement
  p2$resultat$table_positionnee$qx[] <- NA_real_
  p2$resultat$table_positionnee$borne_sup_exacte[] <- NA_real_
  a <- appel(e$positionnement)
  identical(a[c("parametres", "resultat", "diagnostics")], e$validation) && identical(appel(p2), a)
})
verifier("C1 : tranches_age = 20 (une seule tranche), segmentation sexe -> segment_tranche F et H, tranche (deux segments complets) : état contraint_par_positionnement, bornes et p NA, SMR descriptif", {
  x <- construire_table(JEU, ROLES, plan_vp(val = modifyList(VAL, list(tranches_age = 20))), tables_reference = list(tv = T_HF))
  sg <- x$etapes$validation$resultat$sous_groupes
  identical(sg$regroupement, c("segment_tranche", "segment_tranche", "tranche", "segment", "segment")) &&
    all(sg$etat == "contraint_par_positionnement") &&
    all(is.na(unlist(sg[c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure", "p_centrale")]))) &&
    isTRUE(proche(sg$smr, sg$observes / sg$attendus, 0, 0))
})
verifier("C1 : sans segmentation, une seule tranche -> sous-groupe tranche contraint_par_positionnement", {
  te <- table_reference(data.frame(age = GM$age, sexe = "ensemble", qx = 0.8 * GM$qx), "ens", "importee", "test")
  x <- construire_table(JEU, ROLES, plan_vp(segmentation = character(0), val = modifyList(VAL, list(tranches_age = 20))),
                        tables_reference = list(tv = te))
  sg <- x$etapes$validation$resultat$sous_groupes
  identical(sg$etat, "contraint_par_positionnement") && is.na(sg$p_centrale) && is.na(sg$borne_inf_exacte)
})
verifier("étape : par_annee = TRUE -> ddl = n - S (S = 2 segments, et non le nombre de séquences sexe x année)", {
  x <- construire_table(JEU, ROLES, plan_vp(par_annee = TRUE), tables_reference = list(tv = T_HF))
  v <- x$etapes$validation
  n <- sum(v$resultat$cellules$etat == "retenue")
  proche(valeur(v$resultat$statistiques, "khi2_pearson/ddl"), n - 2, 0, 0)
})
verifier("étape : sans segmentation, table « ensemble » -> ddl = n - 1 ; sous-groupes tranche d'un seul segment, bornes produites", {
  te <- table_reference(data.frame(age = GM$age, sexe = "ensemble", qx = 0.8 * GM$qx), "ens", "importee", "test")
  x <- construire_table(JEU, ROLES, plan_vp(segmentation = character(0)), tables_reference = list(tv = te))
  v <- x$etapes$validation
  n <- sum(v$resultat$cellules$etat == "retenue")
  sg <- v$resultat$sous_groupes
  isTRUE(proche(valeur(v$resultat$statistiques, "deviance/ddl"), n - 1, 0, 0)) &&
    identical(sg$regroupement, rep("tranche", 3L)) && all(sg$etat == "produite") && !anyNA(sg$borne_inf_exacte)
})

# Arrondi à 1 de la table positionnée (état qx_arrondi_un, avis d'actuary du
# 07/11/2026) : jeu T1 de test_positionnement.R, theta^ voisin de 151 ;
# q~(52) = 0,99, table positionnée égale à 1 à l'âge 52 ; la cellule 52,
# exposée (E^c = 0,001), garde un attendu fini A = theta^ E^c mu~.
local({
  ex <- data.frame(age = 50:52, exposition_jours = 0, exposition_centrale = c(1, 1, 0.001), evenements = c(1L, 0L, 0L),
                   sorties_concurrentes = 0L)
  t1 <- table_reference(data.frame(age = 40:70, sexe = "ensemble", qx = replace(rep(0.01, 31L), 11:13, c(0.001, 0.001, 0.99))),
                        "T1", "importee", "test")
  pos <- tablexp:::calculer_positionnement(ex, character(0), FALSE, "dernier_anniversaire", t1, "t", "smr", c(50, 52), 0.95)
  v <- tablexp:::calculer_validation(ex, character(0), FALSE, "dernier_anniversaire", t1, "t", "positionnee", c(50, 52),
                                     50, 0.95, 19, 5L, pos[c("parametres", "resultat", "diagnostics")])
  cl <- v$resultat$cellules
  th <- pos$resultat$smr$smr
  verifier("qx_arrondi_un : table positionnée égale à 1 à l'âge 52 ; attendus finis A = theta^ E^c mu~ (identité exacte), somme A = O = 1 ; ddl = 2", tous(
    arrondi = if (identical(pos$resultat$table_positionnee$etat[3L], "qx_arrondi_un") &&
                  identical(pos$resultat$table_positionnee$qx[3L], 1)) TRUE else "état de la table positionnée",
    fini = if (all(is.finite(cl$attendus)) && identical(cl$attendus, th * (ex$exposition_centrale * -log1p(-t1$qx[11:13]))))
             TRUE else "attendus",
    somme = proche(sum(cl$attendus), 1, 1e-12, 0),
    ddl = proche(valeur(v$resultat$statistiques, "khi2_pearson/ddl"), 2, 0, 0)))
})

# ddl = n - S = 0 (audit A1) : une cellule retenue par segment, A = theta^ A~
# avec theta^ = D / A~ (S = 1 et S = 2 ; E^c = 7 et 13,7) ; témoin ddl = 1
# (un segment de deux cellules).
TESTS_A1 <- c("khi2_pearson/variance_h0",  # #179
              paste0(rep(c("khi2_pearson", "deviance"), each = 3L), "/",
                     c("p_asymptotique", "p_monte_carlo", "erreur_monte_carlo")),
              paste0("signes/", c("statistique", "p_asymptotique", "p_exacte", "p_monte_carlo", "erreur_monte_carlo")),
              paste0("runs/", c("esperance", "variance", "statistique", "p_asymptotique", "p_exacte", "p_monte_carlo",
                                "erreur_monte_carlo")),
              paste0("wilcoxon/", c("variance", "statistique", "p_asymptotique", "p_exacte", "p_monte_carlo",
                                    "erreur_monte_carlo")))
une_cellule <- function(E, D, seg, mu = rep(0.02, length(D))) {
  At <- E * mu
  th <- as.vector(tapply(D, seg, sum) / tapply(At, seg, sum))[seg]
  sv(D, E, -expm1(-th * mu), th * mu, th * At, rep("retenue", length(D)), seg, seq_along(D), 0.95, "positionnee", 99, 3L,
     seg)
}
etats_a1 <- function(S) {
  s <- S$statistiques
  cles <- paste(s$statistique, s$grandeur, sep = "/")
  list(ddl = valeur(s, "khi2_pearson/ddl"), contraints = sort(cles[s$etat == "contraint_par_positionnement"]),
       na_tests = all(is.na(valeur(s, TESTS_A1))), tirages = S$tirages_effectues,
       produits = all(!is.na(valeur(s, c("khi2_pearson/statistique", "deviance/statistique", "signes/esperance_positifs_h0",
                                         "residus/esperance_au_dela_2", "fluctuation/esperance_hors_intervalle_exact")))))
}
verifier("A1 : ddl = n - S = 0 (une cellule par segment ; S = 1 et S = 2 ; E^c = 7 et 13,7) -> mêmes états et mêmes NA : tests contraint_par_positionnement, aucun tirage ; ddl, X2, G2, espérances et fluctuation produits", {
  attendu <- sort(c(TESTS_A1, paste0("smr/", c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure",
                                              "p_centrale", "z_racine_cubique", "p_racine_cubique"))))
  cas <- list(une_cellule(7, 3L, 1L), une_cellule(13.7, 3L, 1L), une_cellule(c(7, 7), c(3L, 1L), c(1L, 2L)),
              une_cellule(c(13.7, 13.7), c(3L, 1L), c(1L, 2L)))
  e <- lapply(cas, etats_a1)
  all(vapply(e, function(a) identical(a$ddl, 0) && identical(a$contraints, attendu) && a$na_tests &&
               identical(a$tirages, 0L) && a$produits, NA))
})
verifier("A1 témoin : ddl = 1 (un segment de deux cellules) -> tests produits, B tirages", {
  a <- etats_a1(une_cellule(c(7, 13.7), c(3L, 1L), c(1L, 1L)))
  identical(a$ddl, 1) && identical(a$tirages, 99L) &&
    !any(c("khi2_pearson/p_asymptotique", "khi2_pearson/p_monte_carlo") %in% a$contraints)
})

# --- F. Refus avant exécution (valeur calculée à la main : module, motif, ordre) ----

verifier("refus valider_plan : relation positionnee sans étape positionnement -> positionnement_absent (module validation)",
         controle(refus_de(valider_plan(plan_vp(pos = NULL))), "validation", "positionnement_absent"))
verifier("refus valider_plan : plage de la validation différente du positionnement -> plage_ages_differente_positionnement ; les tranches restent libres", {
  r <- refus_de(valider_plan(plan_vp(val = modifyList(VAL, list(plage_ages = c(20, 80))))))
  ok <- controle(r, "validation", "plage_ages_differente_positionnement")
  if (!isTRUE(ok)) ok
  else if (!grepl("(20 à 80)", conditionMessage(r), fixed = TRUE) || !grepl("(20 à 90)", conditionMessage(r), fixed = TRUE))
    conditionMessage(r)
  else if (!is.null(refus_de(valider_plan(plan_vp(val = modifyList(VAL, list(tranches_age = c(20, 60)))))))) "tranches"
  else TRUE
})
verifier("refus valider_plan : clé de la validation différente du positionnement -> table_reference_differente_positionnement ; plage contrôlée avant la clé", {
  a <- refus_de(valider_plan(plan_vp(val = modifyList(VAL, list(table_reference = "autre")))))
  b <- refus_de(valider_plan(plan_vp(val = modifyList(VAL, list(table_reference = "autre", plage_ages = c(20, 80))))))
  tous(a = controle(a, "validation", "table_reference_differente_positionnement"),
       b = controle(b, "validation", "plage_ages_differente_positionnement"),
       m = if (grepl("« autre »", conditionMessage(a), fixed = TRUE) && grepl("« tv »", conditionMessage(a), fixed = TRUE))
             TRUE else conditionMessage(a))
})
verifier("refus valider_plan : relation_table_invalide cite les trois relations admises", {
  r <- refus_de(valider_plan(plan_vp(val = modifyList(VAL, list(relation_table = "positionne")))))
  isTRUE(controle(r, "validation", "relation_table_invalide")) &&
    grepl("« independante », « ajustee_hors_outil », « positionnee »", conditionMessage(r), fixed = TRUE)
})
verifier("calculer_validation() : positionnee sans étape positionnement, ou relation inconnue -> erreur de programmation ; étape transmise sous independante ignorée (identical() au calcul sans elle)", {
  e <- X$etapes
  appel <- function(rel, pos) erreur_de(tablexp:::calculer_validation(
    e$exposition$resultat$exposition, "sexe", FALSE, "dernier_anniversaire", T_HF, "tv", rel, c(20, 90), c(20, 50, 70),
    0.95, 9, 1L, pos))
  ind <- function(pos) tablexp:::calculer_validation(
    e$exposition$resultat$exposition, "sexe", FALSE, "dernier_anniversaire", T_HF, "tv", "independante", c(20, 90),
    c(20, 50, 70), 0.95, 9, 1L, pos)
  tous(a = programmation(appel("positionnee", NULL)), c = programmation(appel("autre", NULL)),
       b = if (identical(ind(e$positionnement), ind(NULL))) TRUE else "independante")
})

# --- G. importance_exclusions sous positionnee (Q6) ; rejeu --------------------------

verifier("importance_exclusions : comparateur 2 non produit sous positionnee (état non_produite_par_cette_version, smr_comparateur_2 NULL) ; smr_comparateur_2() refuse la relation (erreur de programmation)", {
  x <- construire_table(JEU, ROLES, plan_vp(ie = TRUE), tables_reference = list(tv = T_HF))
  ie <- x$etapes$importance_exclusions
  identical(ie$parametres$comparateurs$etat, c("produite", "non_produite_par_cette_version", "non_produite_par_cette_version")) &&
    is.null(ie$resultat$smr_comparateur_2) && "smr_comparateur_2" %in% names(ie$resultat) &&
    isTRUE(programmation(erreur_de(tablexp:::smr_comparateur_2(data.frame(), character(0), FALSE, T_HF, c(20, 90), "tv",
                                                             "positionnee", 0.95))))
})
verifier("rejeu : rejouer_fiche() de la construction positionnee, identical() hors execution", {
  f <- lire_fiche(ecrire_fiche(X$fiche))
  r <- rejouer_fiche(JEU, f, list(tv = T_HF))
  identical(sans_execution(r$construction), sans_execution(X))
})

# --- H. Rapport figé (valeur calculée à la main) --------------------------------------

section_validation <- function(x) {
  h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"
  list(h = h, sec = regmatches(h, regexpr("<section id=\"etape_validation\">.*?</section>", h)))
}
verifier("rapport positionnee : titre (section et table des matières), mention validation_positionnee, Monte-Carlo multinomial, multiplicité, hypothèses des tests ; sans « Aucune table positionnée n'est produite »", {
  r <- section_validation(X)
  t <- "Étape validation : statistiques de validation de la table positionnée de cette construction"
  grepl(paste0("<li><a href=\"#etape_validation\">", t, "</a></li>"), r$h, fixed = TRUE) &&
    all(vapply(c(paste0("<h2>", t, "</h2>"), "<p>Aucun verdict ni seuil : aucune p-valeur",
                 "Relation « positionnee » : la table validée est la table positionnée de cette construction",
                 "loi multinomiale (O_s ; pi_c)", "Aucune correction de multiplicité",
                 "seules les p-valeurs de Monte-Carlo ont un niveau garanti",
                 paste("<p>Hypothèse nulle : D_c ~ Poisson(theta_s A~_c) indépendants, A~_c = E^c_c mu~_c,",
                       "mu~_c = -ln(1 - q~_c), theta_s inconnu, un par segment ; attendus de la table positionnée",
                       "A_c = theta^_s A~_c (etapes/positionnement/resultat/smr). Conditionnellement aux décès observés",
                       "O_s de chaque segment, les décès des cellules retenues du segment suivent la loi multinomiale",
                       "(O_s ; pi_c), pi_c = A_c / A_s, libre de theta_s : exacte avec l'exposition centrale E^c tenue",
                       "pour fixe, asymptotique sous le modèle de survie (E^c aléatoire).</p>")),
               function(m) grepl(m, r$sec, fixed = TRUE), NA)) &&
    !grepl("Aucune table positionnée n'est produite", r$sec, fixed = TRUE) &&
    !grepl("D*_c ~ Poisson(A_c)", r$sec, fixed = TRUE) &&
    !grepl("D_c ~ Poisson(A_c) indépendants, A_c = E^c_c mu~_c", r$sec, fixed = TRUE)
})
verifier("rapport independante : titre inchangé ; première phrase et suite de validation_sans_verdict dans un seul paragraphe, texte d'avant #167", {
  x <- construire_table(JEU, ROLES, plan_vp(pos = NULL, val = modifyList(VAL, list(relation_table = "independante"))),
                        tables_reference = list(tv = T_HF))
  r <- section_validation(x)
  grepl(paste("<p>Aucune table positionnée n'est produite : la table de référence est prise telle quelle. Aucun verdict",
              "ni seuil : aucune p-valeur n'est comparée à un niveau, et le moteur ne conclut pas sur l'adéquation de la",
              "table.</p>"), r$sec, fixed = TRUE) &&
    grepl("<h2>Étape validation : statistiques de validation de la table de référence prise telle quelle</h2>", r$sec,
          fixed = TRUE)
})
verifier("rapport independante et ajustee_hors_outil avec l'étape positionnement au plan (décision du mainteneur du 08/11/2026) : mention de la table de référence prise telle quelle, une fois, dans le paragraphe de validation_sans_verdict ; « Aucune table positionnée n'est produite » absente ; titre sans table positionnée", {
  nouvelle <- paste("<p>La validation porte sur la table de référence citée par l'étape validation, prise telle quelle",
                    "(etapes/validation/parametres/table_reference) ; la table positionnée produite par l'étape",
                    "positionnement de cette construction n'est pas validée par cette étape (relation « positionnee »",
                    "pour la valider). Aucun verdict ni seuil : aucune p-valeur n'est comparée à un niveau, et le moteur",
                    "ne conclut pas sur l'adéquation de la table.</p>")
  une <- function(rel, titre) {
    x <- construire_table(JEU, ROLES, plan_vp(val = modifyList(VAL, list(relation_table = rel))),
                          tables_reference = list(tv = T_HF))
    r <- section_validation(x)
    n <- lengths(regmatches(r$h, gregexpr(nouvelle, r$h, fixed = TRUE)))
    if (!("positionnement" %in% names(x$etapes))) "étape positionnement absente"
    else if (n != 1L) sprintf("nouvelle mention présente %d fois", n)
    else if (grepl("Aucune table positionnée n'est produite", r$h, fixed = TRUE)) "ancienne mention présente"
    else if (!grepl(paste0("<h2>", titre, "</h2>"), r$sec, fixed = TRUE)) "titre"
    else TRUE
  }
  tous(independante = une("independante",
                          "Étape validation : statistiques de validation de la table de référence prise telle quelle"),
       ajustee_hors_outil = une("ajustee_hors_outil",
                                "Étape validation : statistiques de validation de la table de référence ajustée hors de l'outil"))
})
verifier("rapport ajustee_hors_outil : titre propre à la relation ; plan sans validation : titre de SECTIONS_RAPPORT", {
  x <- construire_table(JEU, ROLES, plan_vp(pos = NULL, val = modifyList(VAL, list(relation_table = "ajustee_hors_outil"))),
                        tables_reference = list(tv = T_HF))
  x0 <- construire_table(JEU, ROLES, local({ p <- plan_vp(); p$etapes$validation <- NULL; p$graine <- NULL; p }),
                         tables_reference = list(tv = T_HF))
  grepl("<h2>Étape validation : statistiques de validation de la table de référence ajustée hors de l'outil</h2>",
        section_validation(x)$sec, fixed = TRUE) &&
    grepl("<h2>Étape validation : statistiques de validation de la table de référence prise telle quelle</h2>",
          section_validation(x0)$sec, fixed = TRUE)
})
verifier("rapport : relation sans mentions (objet altéré, appel direct) -> erreur de programmation du titre et du corps (switch par défaut)", {
  x <- X
  x$etapes$validation$parametres$relation_table <- "autre"
  k <- match("etape_validation", tablexp:::SECTIONS_RAPPORT$section)
  isTRUE(programmation(erreur_de(tablexp:::titre_section(x, k)))) &&
    isTRUE(programmation(erreur_de(tablexp:::corps_section(x, k))))
})

# --- I. Attendus faibles : statuts des p asymptotiques et variance_h0 (#179) -------
# Spécification (0') de #179 (commentaire 6747033179 ; M359, M360), tests
# T179.1 à T179.3. Natures : valeur calculée à la main (textes, états) ;
# énumération exacte (loi conjointe de Poisson tronquée, compositions
# multinomiales), indépendante de la formule du moteur.

verifier("T179.1 statuts : p asymptotique de X2 avec 2 + 1/A_c (independante) et (n_s^2 + 2 n_s - 2) / O_s (positionnee) ; p asymptotique de G2 « selon la répartition des A_c », sans « conservat » ; variance_h0 sous H0 entre ddl et p_asymptotique", {
  a <- tablexp:::STATUTS_VALIDATION; b <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  k <- function(s, st, g) s$statut[s$statistique == st & s$grandeur == g]
  g2 <- c(k(a, "deviance", "p_asymptotique"), k(b, "deviance", "p_asymptotique"))
  ordre <- function(s) s$grandeur[s$statistique == "khi2_pearson"]
  tous(
    x2_i = if (grepl("2 + 1/A_c", k(a, "khi2_pearson", "p_asymptotique"), fixed = TRUE)) TRUE else "independante",
    x2_p = if (grepl("(n_s^2 + 2 n_s - 2) / O_s", k(b, "khi2_pearson", "p_asymptotique"), fixed = TRUE)) TRUE
           else "positionnee",
    # Absence de « conservat » hors de la mention `indep` (unité contrat),
    # que le statut de G2 sous positionnee porte comme les autres (visa
    # d'actuary, Q2).
    g2 = if (all(grepl("selon la répartition des A_c", g2, fixed = TRUE)) &&
             !any(grepl("conservat", sub(" ; unités d'observation indépendantes.*", "", g2), fixed = TRUE)) &&
             grepl("sous l'unité contrat, les décès sont sur-dispersés et les tests sont anti-conservateurs", g2[2L],
                   fixed = TRUE))
           TRUE else "deviance",
    # Phrases partagées entre les deux tables (PHRASES_ATTENDUS_FAIBLES, #200) :
    # identiques ; `petits` précède « : sous H0 (i) » dans les quatre p
    # asymptotiques.
    copies = local({
      p4 <- c(k(a, "khi2_pearson", "p_asymptotique"), k(a, "deviance", "p_asymptotique"),
              k(b, "khi2_pearson", "p_asymptotique"), k(b, "deviance", "p_asymptotique"))
      if (!all(grepl("approximation non garantie quand des attendus A_c sont petits : sous H0 (i)", p4, fixed = TRUE)))
        return("phrase petits absente d'une p asymptotique")
      phrase_g2 <- regmatches(g2, regexpr("l'espérance de la contribution.*garanti sous H0 \\(i\\)", g2))
      fin_x2 <- sub(".*khi2_pearson/variance_h0\\) ; ", "", k(a, "khi2_pearson", "p_asymptotique"))
      if (length(phrase_g2) == 2L && identical(phrase_g2[1L], phrase_g2[2L]) &&
          startsWith(fin_x2, "le niveau réel") &&
          grepl(paste0("sont égaux ; ", fin_x2, " ; unités d'observation"), k(b, "khi2_pearson", "p_asymptotique"),
                fixed = TRUE)) TRUE else "phrases partagées différentes entre les tables"
    }),
    ligne = if (all(vapply(list(a, b), function(s)
                  identical(ordre(s), c("statistique", "ddl", "variance_h0", "p_asymptotique", "p_monte_carlo",
                                        "erreur_monte_carlo")) &&
                    isTRUE(s$sous_h0[s$statistique == "khi2_pearson" & s$grandeur == "variance_h0"]), NA)))
              TRUE else "ligne variance_h0")
})

verifier("T179.2 Poisson : variance_h0 égale à la variance de X2 par énumération conjointe (3 cellules, A = 0,05 ; 0,7 ; 3,2 ; troncature A + 40 sqrt(A) + 60), écart relatif <= 1e-12 ; identique à sum(2 + 1/A)", {
  A <- c(0.05, 0.7, 3.2)
  s <- sv(c(0L, 1L, 3L), rep(1, 3), -expm1(-A), A, A, rep("retenue", 3), rep(1L, 3), 1:3, 0.95, "independante", 19, 7L)
  v <- valeur(s$statistiques, "khi2_pearson/variance_h0")
  # Loi conjointe des trois cellules indépendantes, X2 = somme des
  # contributions, sur la grille tronquée (masse retirée < 1e-300).
  d <- lapply(A, function(a) 0:ceiling(a + 40 * sqrt(a) + 60))
  p <- lapply(seq_along(A), function(c) stats::dpois(d[[c]], A[c]))
  x <- lapply(seq_along(A), function(c) (d[[c]] - A[c])^2 / A[c])
  P <- outer(outer(p[[1L]], p[[2L]]), p[[3L]])
  X <- outer(outer(x[[1L]], x[[2L]], `+`), x[[3L]], `+`)
  m <- sum(P * X)
  tous(var = proche(v, sum(P * (X - m)^2), 1e-12, 0), esp = proche(m, 3, 1e-12, 0),
       somme = if (identical(v, sum(2 + 1 / A))) TRUE else "différent de sum(2 + 1/A)",
       etat = if (identical(etat_de(s$statistiques, "khi2_pearson/variance_h0"), "produite")) TRUE else "état")
})

verifier("T179.2 multinomiale : variance_h0 égale à la variance de X2 par énumération de toutes les compositions (segment 1 : 3 cellules, O = 7 ; segment 2 : 2 cellules, O = 4), écart relatif <= 1e-12 ; E[X2] = n - S = 3", {
  pi1 <- c(0.1, 1, 3) / 4.1; pi2 <- c(0.2, 0.8)
  A <- c(7 * pi1, 4 * pi2); seg <- c(1L, 1L, 1L, 2L, 2L)
  s <- sv(c(1L, 2L, 4L, 1L, 3L), rep(1, 5), -expm1(-A), A, A, rep("retenue", 5), seg, 1:5, 0.95, "positionnee", 19, 7L, seg)
  v <- valeur(s$statistiques, "khi2_pearson/variance_h0")
  # Compositions de O en k parts, probabilités multinomiales par factorielles.
  compositions <- function(O, k) if (k == 1L) matrix(O, 1L) else
    do.call(rbind, lapply(0:O, function(j) cbind(j, compositions(O - j, k - 1L))))
  loi <- function(O, p) {
    m <- compositions(O, length(p))
    list(p = apply(m, 1L, function(r) factorial(O) / prod(factorial(r)) * prod(p^r)),
         x = apply(m, 1L, function(r) sum((r - O * p)^2 / (O * p))))
  }
  l1 <- loi(7, pi1); l2 <- loi(4, pi2)
  P <- outer(l1$p, l2$p); X <- outer(l1$x, l2$x, `+`)
  m <- sum(P * X)
  tous(masse = proche(sum(P), 1, 1e-13, 0), esp = proche(m, 3, 1e-12, 0), var = proche(v, sum(P * (X - m)^2), 1e-12, 0),
       etat = if (identical(etat_de(s$statistiques, "khi2_pearson/variance_h0"), "produite")) TRUE else "état")
})

verifier("T179.2 multinomiale, segments non contigus et non ordonnés (5, 2, 5, 9, 2, 5, 9, 7), cellule sans_exposition intercalée, segment 7 à une seule cellule retenue (ddl > 0) : variance_h0 égale à la somme des variances par segment énumérées, écart relatif <= 1e-12 (constat mineur 1 de l'audit)", {
  seg <- c(5L, 2L, 5L, 9L, 2L, 5L, 9L, 7L)
  etat <- c("retenue", "retenue", "retenue", "sans_exposition", "retenue", "retenue", "retenue", "retenue")
  At <- c(0.03, 1.2, 0.5, 1, 2.5, 4, 0.7, 2)
  D <- c(0L, 2L, 1L, 0L, 1L, 3L, 2L, 1L)
  ret <- etat == "retenue"
  O <- tapply(D[ret], seg[ret], sum); As <- tapply(At[ret], seg[ret], sum)
  A <- At; A[ret] <- At[ret] * (O / As)[as.character(seg[ret])]
  # Séquence constante : avec sequence = seg, le tri par séquence puis âge
  # (R/validation.R, statistiques_validation()) rendrait les segments contigus
  # et rangés avant le calcul de n_s et O_s (audit de la reprise de #179).
  s <- sv(D, rep(1, 8), -expm1(-A), A, A, etat, rep(1L, 8), seq_along(D), 0.95, "positionnee", 19, 7L, seg)
  compositions <- function(O, k) if (k == 1L) matrix(O, 1L) else
    do.call(rbind, lapply(0:O, function(j) cbind(j, compositions(O - j, k - 1L))))
  # Segments indépendants : somme des variances ; un segment d'une seule
  # cellule retenue a X2 = 0 (D_c = O_s), variance nulle.
  v_enum <- 0
  for (g in sort(unique(seg[ret]))) {
    i <- which(ret & seg == g)
    if (length(i) < 2L) next
    p <- At[i] / sum(At[i]); m <- compositions(sum(D[i]), length(i))
    pr <- apply(m, 1L, function(r) factorial(sum(D[i])) / prod(factorial(r)) * prod(p^r))
    x <- apply(m, 1L, function(r) sum((r - sum(D[i]) * p)^2 / (sum(D[i]) * p)))
    v_enum <- v_enum + sum(pr * (x - sum(pr * x))^2)
  }
  tous(var = proche(valeur(s$statistiques, "khi2_pearson/variance_h0"), v_enum, 1e-12, 0),
       ddl = proche(valeur(s$statistiques, "khi2_pearson/ddl"), 3, 0, 0),
       etat = if (identical(etat_de(s$statistiques, "khi2_pearson/variance_h0"), "produite")) TRUE else "état")
})

verifier("T179.3 états de variance_h0 : une cellule retenue par segment (ddl = n - S = 0) -> contraint_par_positionnement, NA ; ajustee_hors_outil -> ddl_non_etabli, NA ; aucune cellule retenue -> aucune_cellule_retenue, NA", {
  cle <- "khi2_pearson/variance_h0"
  d <- une_cellule(c(7, 7), c(3L, 1L), c(1L, 2L))$statistiques
  A <- c(0.5, 2, 4)
  h <- sv(c(0L, 3L, 5L), rep(1, 3), -expm1(-A), A, A, rep("retenue", 3), rep(1L, 3), 1:3, 0.95, "ajustee_hors_outil", 19, 7L)$statistiques
  z <- sv(c(0L, 3L, 5L), rep(1, 3), -expm1(-A), A, A, rep("sans_exposition", 3), rep(1L, 3), 1:3, 0.95, "independante", 19,
          7L)$statistiques
  tous(degenere = if (identical(etat_de(d, cle), "contraint_par_positionnement") && is.na(valeur(d, cle))) TRUE
                  else etat_de(d, cle),
       ajustee = if (identical(etat_de(h, cle), "ddl_non_etabli") && is.na(valeur(h, cle))) TRUE else etat_de(h, cle),
       vide = if (identical(etat_de(z, cle), "aucune_cellule_retenue") && is.na(valeur(z, cle))) TRUE else etat_de(z, cle))
})

# --- J. p asymptotique des sous-groupes au-delà du plafond (#178) ------------------
# Spécification (0') de #178 (commentaire 6747040204 ; M361), tests T178.1 à
# T178.3. Natures : formule de la spécification recalculée ici (pnorm, moyenne
# et variance écrites à la main) ; loi exacte par la convolution du moteur,
# déjà recoupée par somme directe (section D) ; valeurs mesurées de la
# spécification (m5).
P_EXACTES <- c("p_inferieure", "p_superieure", "p_centrale")
P_ASYMPTOTIQUES <- c("p_inferieure_asymptotique", "p_superieure_asymptotique", "p_centrale_asymptotique")
# Approximation normale avec correction de continuité (spécification, Méthode).
p_normale <- function(mu, sigma, o) {
  inf <- stats::pnorm((o + 0.5 - mu) / sigma)
  sup <- stats::pnorm((o - 0.5 - mu) / sigma, lower.tail = FALSE)
  c(inf, sup, min(1, 2 * min(inf, sup)))
}
# Aucune ligne ne porte à la fois une p exacte et une p asymptotique.
exclusives <- function(r) {
  e <- !is.na(do.call(cbind, r[P_EXACTES])); a <- !is.na(do.call(cbind, r[P_ASYMPTOTIQUES]))
  !any(rowSums(e) > 0 & rowSums(a) > 0)
}
verifier("T178.1 : 2 segments x 2 cellules, D = (60 000, 40 000) et (50 000, 50 000), sous-groupes croisant les segments, coût > 1e8 -> p exactes NA, p asymptotiques égales à la formule pnorm avec correction de continuité (1e-15 relatif), bornes NA, état convolution_au_dela_n_max", {
  # Attendus de la table positionnée A = (59 800, 40 200, 50 000, 50 000) (A_s =
  # O_s = 1e5) : groupe 1 (cellules 1 et 3), pi = (0,598 ; 0,5), O_b = 110 000,
  # moyenne 109 800 ; groupe 2, pi = (0,402 ; 0,5), O_b = 90 000, moyenne
  # 90 200 (p inférieure et supérieure distinctes).
  D <- c(60000, 40000, 50000, 50000)
  r <- tablexp:::sous_groupes_positionnee("tranche", c(1L, 2L, 1L, 2L), 2L, c(1L, 1L, 2L, 2L), rep(TRUE, 4L), D,
                                          c(59800, 40200, 50000, 50000), c(110000, 90000), c(109800, 90200), 0.95)
  a1 <- p_normale(1e5 * 0.598 + 1e5 * 0.5, sqrt(1e5 * 0.598 * 0.402 + 1e5 * 0.5 * 0.5), 110000)
  a2 <- p_normale(1e5 * 0.402 + 1e5 * 0.5, sqrt(1e5 * 0.402 * 0.598 + 1e5 * 0.5 * 0.5), 90000)
  asy <- do.call(cbind, r[P_ASYMPTOTIQUES])
  tous(cout = if (tablexp:::cout_convolution_binomiales(c(1e5, 1e5), c(0.598, 0.5)) > 1e8 &&
                  tablexp:::cout_convolution_binomiales(c(1e5, 1e5), c(0.402, 0.5)) > 1e8) TRUE else "coût <= 1e8",
       etat = if (identical(r$etat, rep("convolution_au_dela_n_max", 2L))) TRUE else paste(r$etat, collapse = ", "),
       exactes = if (all(is.na(unlist(r[c(P_EXACTES, "borne_inf_exacte", "borne_sup_exacte")])))) TRUE
                 else "p exactes ou bornes non NA",
       g1 = proche(unname(asy[1L, ]), a1, 1e-15, 0), g2 = proche(unname(asy[2L, ]), a2, 1e-15, 0),
       exclusives = if (exclusives(r)) TRUE else "p exacte et p asymptotique sur une même ligne")
})
verifier("T178.1 p_somme_binomiales_normale() : erreur de programmation si sigma = 0 (toutes les pi_b,s dans {0, 1})",
         programmation(erreur_de(tablexp:::p_somme_binomiales_normale(c(10, 20), c(1, 1), 30))))
# T178.2 : 3 segments x 2 cellules, O_s = (2000, 3000, 1000), pi_b = (0,2 ;
# 0,7 ; 0,4) pour le groupe 1 ; coût 5,1e6 (convolution exacte sous le plafond
# par défaut) ; n_max = 0 force l'approximation normale.
groupe_t178 <- function(o, n_max) {
  sgp <- tablexp:::sous_groupes_positionnee
  arguments <- list("tranche", rep(c(1L, 2L), 3L), 2L, rep(1:3, each = 2L), rep(TRUE, 6L),
                    c(400, 1600, o - 800, 3800 - o, 400, 600), c(0.2, 0.8, 0.7, 0.3, 0.4, 0.6), c(o, 6000 - o),
                    c(1.3, 1.7), 0.95)
  if (missing(n_max)) do.call(sgp, arguments) else do.call(sgp, c(arguments, list(n_max = n_max)))
}
verifier("T178.2 : n = (2000, 3000, 1000), pi = (0,2 ; 0,7 ; 0,4), o = 2830, 2970, 3000 : p exactes (n_max par défaut, état bornes_non_definies) et asymptotiques (n_max = 0, état convolution_au_dela_n_max) égales aux mesures m5 de la spécification (1e-10) ; écart absolu <= 5e-5 et relatif <= 3e-3 sur les trois p", {
  o <- c(2830, 2970, 3000)
  ex <- lapply(o, groupe_t178); as <- lapply(o, groupe_t178, n_max = 0)
  pe <- t(vapply(ex, function(r) unlist(lapply(r[P_EXACTES], `[`, 1L)), numeric(3)))
  pa <- t(vapply(as, function(r) unlist(lapply(r[P_ASYMPTOTIQUES], `[`, 1L)), numeric(3)))
  tous(
    cout = if (tablexp:::cout_convolution_binomiales(c(2000, 3000, 1000), c(0.2, 0.7, 0.4)) <= 1e8) TRUE else "coût",
    # Bascule stricte (coût > n_max) : coût 5 085 864 mesuré.
    bascule = if (identical(tablexp:::cout_convolution_binomiales(c(2000, 3000, 1000), c(0.2, 0.7, 0.4)), 5085864) &&
                  identical(groupe_t178(3000, n_max = 5085864)$etat[1L], "bornes_non_definies") &&
                  identical(groupe_t178(3000, n_max = 5085863)$etat[1L], "convolution_au_dela_n_max")) TRUE
              else "bascule à égalité coût = n_max",
    etats = if (all(vapply(ex, function(r) r$etat[1L], "") == "bornes_non_definies") &&
                all(vapply(as, function(r) r$etat[1L], "") == "convolution_au_dela_n_max")) TRUE else "états",
    exacte_m5 = proche(pe[, 3L], c(0.0439421860, 0.0439109126, 0.0039149526), 0, 1e-10),
    asymptotique_m5 = proche(pa[, 3L], c(0.0439356363, 0.0439356363, 0.0039220898), 0, 1e-10),
    formule = proche(pa[3L, ], p_normale(400 + 2100 + 400, sqrt(320 + 630 + 240), 3000), 1e-15, 0),
    absolu = if (all(abs(pa - pe) <= 5e-5)) TRUE else sprintf("écart absolu max %.3g", max(abs(pa - pe))),
    relatif = if (all(abs(pa - pe) / pe <= 3e-3)) TRUE else sprintf("écart relatif max %.3g", max(abs(pa - pe) / pe)),
    exclusives = if (all(vapply(c(ex, as), exclusives, NA))) TRUE else "p exacte et p asymptotique sur une même ligne")
})
verifier("T178.3 : colonnes p_*_asymptotique entre p_centrale et etat sous les trois relations, entièrement NA sous independante et ajustee_hors_outil et sur la construction positionnee (aucun sous-groupe au-delà du plafond) ; aucune ligne avec une p exacte et une p asymptotique", {
  ordre <- c("smr", "borne_inf_exacte", "borne_sup_exacte", P_EXACTES, P_ASYMPTOTIQUES, "etat")
  sg_rel <- function(relation) {
    x <- construire_table(JEU, ROLES, plan_vp(pos = NULL, val = modifyList(VAL, list(relation_table = relation))),
                          tables_reference = list(tv = T_HF))
    x$etapes$validation$resultat$sous_groupes
  }
  l <- list(positionnee = V$resultat$sous_groupes, independante = sg_rel("independante"),
            ajustee_hors_outil = sg_rel("ajustee_hors_outil"))
  for (n in names(l)) {
    g <- l[[n]]
    if (!identical(utils::tail(names(g), length(ordre)), ordre)) return(paste(n, ": ordre des colonnes"))
    if (!all(vapply(g[P_ASYMPTOTIQUES], function(v) is.double(v) && all(is.na(v)), NA))) return(paste(n, ": colonnes non NA"))
    if (!exclusives(g)) return(paste(n, ": p exacte et p asymptotique sur une même ligne"))
  }
  TRUE
})
verifier("T178.3 statuts : ligne sous_groupes/p_valeurs_asymptotiques (sous_h0) après p_valeurs, textes de la spécification ; p_valeurs (positionnee) complété ; statuts_sous_groupes de l'étape à 4 lignes", {
  a <- tablexp:::STATUTS_VALIDATION; b <- tablexp:::STATUTS_VALIDATION_POSITIONNEE
  k <- function(s, g) s$statut[s$statistique == "sous_groupes" & s$grandeur == g]
  indep <- paste("; unités d'observation indépendantes ; sous l'unité contrat, les décès sont sur-dispersés et les",
                 "tests sont anti-conservateurs")
  texte_p <- paste("asymptotiques (sigma^2 -> infini) conditionnellement aux O_s : approximation normale de la somme",
                   "de binomiales indépendantes Bin(O_s, pi_b,s) d'un sous-groupe de plusieurs segments, moyenne somme",
                   "de O_s pi_b,s, variance sigma^2 = somme de O_s pi_b,s (1 - pi_b,s), correction de continuité",
                   "(P(O_b <= o) par Phi((o + 1/2 - moyenne) / sigma), P(O_b >= o) par 1 - Phi((o - 1/2 - moyenne) /",
                   "sigma)), p centrale min(1, 2 min) ; produites seulement si le nombre de produits de la convolution",
                   "dépasse N_MAX_PRODUITS_CONVOLUTION (état convolution_au_dela_n_max), les p-valeurs exactes étant",
                   "alors NA ; jamais produites à côté d'une p-valeur exacte ; erreur relative croissante dans les",
                   "queues (asymétrie de la somme, d'ordre 1/sigma) ; aucune correction de multiplicité", indep)
  tous(
    ordre = if (all(vapply(list(a, b), function(s) identical(s$grandeur[s$statistique == "sous_groupes"],
                                                             c("smr", "bornes_exactes", "p_valeurs", "p_valeurs_asymptotiques")) &&
                             isTRUE(s$sous_h0[s$statistique == "sous_groupes" & s$grandeur == "p_valeurs_asymptotiques"]), NA)))
              TRUE else "ligne p_valeurs_asymptotiques",
    independante = if (identical(k(a, "p_valeurs_asymptotiques"),
                                 paste("relation independante : non produites, p = 0, la loi de Poisson exacte du",
                                       "sous-groupe est toujours calculée ; relation ajustee_hors_outil : non",
                                       "produites, p non établi, aucune p-valeur du sous-groupe (ddl_non_etabli) ;",
                                       "colonnes NA"))) TRUE else "texte independante",
    positionnee = if (identical(k(b, "p_valeurs_asymptotiques"), texte_p)) TRUE else "texte positionnee",
    p_valeurs = if (grepl("état convolution_au_dela_n_max sinon, p-valeurs asymptotiques produites à leur place) ;",
                          k(b, "p_valeurs"), fixed = TRUE)) TRUE else "p_valeurs (positionnee)",
    etape = if (identical(nrow(V$parametres$statuts_sous_groupes), 4L)) TRUE else "statuts_sous_groupes")
})

# --- K. Type des colonnes etat et smr sur entrée vide (#201) ------------------------
# ifelse() sur un vecteur vide rendait logical(0) : etat (character) et smr
# (double) sortaient en logical quand le regroupement n'avait aucun groupe.

verifier("K (#201) : sous_groupes_positionnee() sans groupe (n_g = 0) -> smr double et etat character de longueur 0 ; avec groupes, inchangés", {
  r <- tablexp:::sous_groupes_positionnee("tranche", integer(0), 0L, integer(0), logical(0), numeric(0), numeric(0),
                                          numeric(0), numeric(0), 0.95)
  p <- tablexp:::sous_groupes_positionnee("tranche", c(1L, 2L), 2L, c(1L, 1L), c(TRUE, TRUE), c(1, 0), c(2, 3),
                                          c(1, 0), c(2, 0), 0.95)
  tous(vide = if (identical(r$smr, numeric(0)) && identical(r$etat, character(0))) TRUE
              else paste(typeof(r$smr), typeof(r$etat)),
       pleine = if (identical(p$smr, c(0.5, NA)) && identical(p$etat[2L], "aucune_cellule_retenue") &&
                    identical(typeof(p$etat), "character")) TRUE else "groupes non vides")
})
verifier("K (#201) : sous_groupes_validation() sans groupe (tranches vides), sous les trois relations -> 0 ligne, smr double et etat character", {
  v <- function(rel) tablexp:::sous_groupes_validation(data.frame(age = integer(0)), integer(0), data.frame(), integer(0),
                                                       numeric(0), c(20, 90), numeric(0), numeric(0), logical(0), FALSE,
                                                       0.95, rel)
  rels <- c("independante", "ajustee_hors_outil", "positionnee")
  r <- lapply(stats::setNames(rels, rels), v)
  do.call(tous, lapply(r, function(x)
    if (nrow(x) == 0L && identical(typeof(x$etat), "character") && identical(typeof(x$smr), "double")) TRUE
    else paste(nrow(x), typeof(x$etat), typeof(x$smr))))
})

# --- L. Type des dénombrements (#199 ; ADR 0009, point 9.6) ------------------------
# Les chemins de dénombrement des étapes positionnement et validation sont
# integer sous la relation positionnee, y compris dans le cas dégénéré
# ddl = n - S = 0 (aucun tirage) et sans cellule retenue (séquences vides) ;
# la garde de sous_groupes_validation() refuse au-delà de
# .Machine$integer.max (garde défensive, appel direct).

verifier("L (#199) : statistiques_validation() sous positionnee, cas dégénéré (ddl = 0), témoin ddl = 1 et aucune cellule retenue -> tirages_effectues et colonnes positifs, negatifs, runs en integer", {
  deg <- une_cellule(7, 3L, 1L)
  tem <- une_cellule(c(7, 13.7), c(3L, 1L), c(1L, 1L))
  vide <- sv(0L, 0, 0.1, -log1p(-0.1), 0, "sans_exposition", 1L, 1L, 0.95, "positionnee", 9, 1L, 1L)
  ty <- function(S) paste(c(typeof(S$tirages_effectues), vapply(S$sequences[c("positifs", "negatifs", "runs")], typeof, "")),
                          collapse = " ")
  attendu <- "integer integer integer integer"
  tous(degenere = if (identical(ty(deg), attendu) && identical(deg$tirages_effectues, 0L)) TRUE else ty(deg),
       temoin = if (identical(ty(tem), attendu) && identical(tem$tirages_effectues, 99L)) TRUE else ty(tem),
       vide = if (identical(ty(vide), attendu) && nrow(vide$sequences) == 0L) TRUE else ty(vide))
})
verifier("L (#199) : construction positionnee -> dénombrements de positionnement (smr$observes, etats$evenements, evenements_sans_attendu, evenements_hors_plage, evenements_total) et de validation (sous_groupes$observes, sequences_runs, tirages_effectues, diagnostics) en integer ; identité O + sans attendu + hors plage = total", {
  # Construction refaite : P, V et X sont réaffectés plus haut dans le fichier.
  xp <- construire_table(JEU, ROLES, plan_vp(), tables_reference = list(tv = T_HF))
  P <- xp$etapes$positionnement; V <- xp$etapes$validation
  t <- c(p_observes = typeof(P$resultat$smr$observes), p_etats = typeof(P$diagnostics$etats$evenements),
         p_sans = typeof(P$diagnostics$evenements_sans_attendu), p_hors = typeof(P$diagnostics$evenements_hors_plage),
         p_total = typeof(P$diagnostics$evenements_total), v_sg = typeof(V$resultat$sous_groupes$observes),
         v_seq = unique(vapply(V$resultat$sequences_runs[c("positifs", "negatifs", "runs")], typeof, "")),
         v_tirages = typeof(V$resultat$monte_carlo$tirages_effectues), v_etats = typeof(V$diagnostics$etats$evenements),
         v_sans = typeof(V$diagnostics$evenements_sans_attendu), v_hors = typeof(V$diagnostics$evenements_hors_plage),
         v_total = typeof(V$diagnostics$evenements_total))
  d <- P$diagnostics
  tous(types = if (all(t == "integer")) TRUE else paste(names(t)[t != "integer"], collapse = ", "),
       identite = if (identical(sum(P$resultat$smr$observes) + d$evenements_sans_attendu + d$evenements_hors_plage,
                                d$evenements_total)) TRUE else "identité")
})
verifier("L (#199) : sous_groupes_validation() sans groupe -> observes integer(0) sous les trois relations ; somme des observés au-delà de .Machine$integer.max -> refus validation / denombrement_hors_plage (grandeur sous_groupes/observes), sous les trois relations", {
  rels <- c("independante", "ajustee_hors_outil", "positionnee")
  vide <- vapply(rels, function(rel) typeof(tablexp:::sous_groupes_validation(
    data.frame(age = integer(0)), integer(0), data.frame(), integer(0), numeric(0), c(20, 90), numeric(0), numeric(0),
    logical(0), FALSE, 0.95, rel)$observes), "")
  # Deux cellules retenues d'une tranche, 1,5e9 décès chacune (integer valides).
  gros <- lapply(rels, function(rel) tryCatch({
    tablexp:::sous_groupes_validation(data.frame(age = c(50L, 51L)), c(1L, 1L), data.frame(row.names = 1L), c(1L, 1L),
                                      20, c(20, 90), c(1500000000L, 1500000000L), c(1, 1), c(TRUE, TRUE), FALSE, 0.95, rel)
    NULL }, refus_tablexp = function(r) r))
  ok <- vapply(gros, function(r) !is.null(r) && identical(c(r$module, r$motif), c("validation", "denombrement_hors_plage")) &&
                 identical(r$details$grandeur, "sous_groupes/observes") && identical(r$details$valeur, 3e9), NA)
  tous(vide = if (all(vide == "integer")) TRUE else paste(vide, collapse = " "),
       refus = if (all(ok)) TRUE else paste("relation(s) sans le refus attendu :", paste(rels[!ok], collapse = ", ")))
})
