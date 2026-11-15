# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/controles_coherence_generation.R :
#  décomptes du champ Controles-coherence des tables de génération (format 2 ;
#  issue #279, spécification d'actuary-approfondi du 15/11/2026,
#  https://github.com/Gerard-Garey/outil_tables/issues/279#issuecomment-6892063963
#  et son correctif
#  https://github.com/Gerard-Garey/outil_tables/issues/279#issuecomment-6892068057)
#
#  Hors CI : lancé à la main. Lit des fichiers de table au format 2 (ADR 0019,
#  point 5.2), jamais les objets du moteur, et refuse un fichier mal formé.
#  R base seul (R 4.3.3), aucun outil externe. Déterministe : aucune horloge,
#  aucun aléa.
#
#  Sorties, par fichier :
#    S1 structure : premier et dernier âge à valeur par génération ; l_x au
#       premier âge ; « - » intercalé et l_x croissants (refusés) ; l_x nuls ;
#       q_x = 1 ; l_x = l_{x+1} > 0 ; maximum de q_x < 1 (fraction exacte).
#       q_x = d_x / l_x, d_x = l_x - l_{x+1}, défini si l_x >= 1 et l_{x+1}
#       est une valeur, éventuellement 0 (ADR 0019, points 4.1 et 4.2).
#    S2 décroissances de q_x aux âges x >= 31 (x second âge du couple
#       (x-1, x)), comparées en entiers : d_x l_{x-1} < d_{x-1} l_x ; chaque
#       couple est classé par le critère exact de compatibilité avec un
#       arrondi au plus proche, (2 l_{x+1} - 1)(2 l_{x-1} - 1) <= (2 l_x + 1)^2
#       (égalité comptée compatible, et comptée à part).
#    S3 test joint, par génération, sur la fenêtre de max(30, premier âge à
#       valeur) au dernier âge où l_x >= 1 : existe-t-il des L_x dans
#       [l_x - 1/2 ; l_x + 1/2] avec log L_x concave ? Oui si et seulement si
#       la plus petite majorante concave M de lo_x = log(l_x - 1/2) vérifie
#       M <= hi_x = log(l_x + 1/2). M : enveloppe concave supérieure de lo
#       (chaîne monotone), interpolée entre ses sommets ; marge m_x = hi_x -
#       M_x aux âges qui ne sont pas des sommets.
#         - Incompatibilité (m_x < -DELTA) : résultat exact, certifié par un
#           triplet témoin (i, x, j) de largeur j - i minimale, vérifié en
#           entiers exacts (entiers longs en base 10^4, ci-dessous) :
#           (2 l_i - 1)^(j-x) (2 l_j - 1)^(x-i) > (2 l_x + 1)^(j-i).
#           Un défaut sans témoin entier est un échec.
#         - Compatibilité (m_x > DELTA en tout âge) : approximation numérique
#           à marge certifiée (erreur de m_x de l'ordre de 1e-12 si log() est
#           juste à quelques ulps près : hypothèse sur la bibliothèque
#           mathématique, non vérifiée ici).
#         - |m_x| <= DELTA : indécis ; le script échoue en citant la cellule.
#       Contrôle croisé : tout couple incompatible de S2 est un défaut joint.
#    S4 médianes des rapports q_x / q_{x-1}, âges 53 à 70, sur les
#       générations 1940 à 2005 qui portent les âges x-1 à x+1 (minimum et
#       maximum joints) ; médiane de q_60 / q_58 sur les générations qui
#       portent les âges 58 à 61. Rapports en flottants (approximation
#       numérique), écrits à 4 décimales.
#
#  Témoin (--temoin) : grilles forgées dans un dossier temporaire, calculs à
#  la main ci-dessous (fonction temoin()) ; le script doit y trouver les
#  décomptes attendus, échouer sur un cas indécis et refuser deux fichiers
#  mal formés. Code de sortie non nul si un cas du témoin n'est pas tenu.
#
#  Usage, depuis la racine du dépôt :
#    Rscript tests/contre_verification/tables/controles_coherence_generation.R \
#      inst/extdata/tables/tgf05.csv inst/extdata/tables/tgh05.csv
#    Rscript tests/contre_verification/tables/controles_coherence_generation.R --temoin
#  Échec : « ECHEC : … » sur la sortie d'erreur, code 1.
# -----------------------------------------------------------------------------

local({

DELTA <- 1e-9               # bande d'indécision du test joint
AGE_DECROISSANCE <- 31L     # premier âge x des couples (x-1, x) de S2
AGE_FENETRE <- 30L          # premier âge de la fenêtre du test joint
L_MAX <- 1e7                # l_x plus grand : produits de S1 et S2 exacts en doubles (< 2^53)
MEDIANES_GENERATIONS <- 1940:2005
MEDIANES_AGES <- 53:70
CELLULE <- "^(0|[1-9][0-9]{0,14}|-)$"

# Sorties en octets UTF-8, quelle que soit la locale (POSIX comprise) : les
# littéraux du script (UTF-8) sont écrits tels quels, sans conversion.
ecrire <- function(x, con = stdout()) writeLines(x, con, useBytes = TRUE)
# Un échec est une condition : le témoin l'attrape, le niveau principal sort
# avec le code 1.
echec <- function(msg) stop(structure(class = c("echec_controle", "error", "condition"),
                                      list(message = msg, call = NULL)))

# Plages compactes d'entiers triés : 1938-1944, 1946.
plages <- function(v) {
  v <- sort(unique(v))
  if (!length(v)) return("aucune")
  debut <- c(TRUE, diff(v) != 1L)
  g <- cumsum(debut)
  paste(vapply(split(v, g), function(s) if (length(s) == 1L) as.character(s) else
    sprintf("%d-%d", s[1L], s[length(s)]), ""), collapse = ", ")
}

# --- SHA-256 (FIPS 180-4) en R base ------------------------------------------
# Repris d'assembler_tgf05_tgh05.R : mots de 32 bits en double (exacts) ;
# opérations bit à bit sur deux moitiés de 16 bits.
sha256 <- function(octets) {
  M32 <- 4294967296
  et <- function(a, b) bitwAnd(a %/% 65536, b %/% 65536) * 65536 + bitwAnd(a %% 65536, b %% 65536)
  oux <- function(a, b) bitwXor(a %/% 65536, b %/% 65536) * 65536 + bitwXor(a %% 65536, b %% 65536)
  non <- function(a) M32 - 1 - a
  rotd <- function(x, n) x %/% 2^n + (x %% 2^n) * 2^(32 - n)
  K <- c(0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
         0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
         0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
         0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
         0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
         0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
         0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
         0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2)
  H <- c(0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19)
  n <- length(octets)
  bits <- 8 * n
  long <- as.raw((bits %/% 256^(7:0)) %% 256)
  m <- as.integer(c(octets, as.raw(0x80), raw((55 - n) %% 64), long))
  for (b in seq_len(length(m) %/% 64) - 1L) {
    o <- m[b * 64 + 1:64]
    W <- numeric(64)
    W[1:16] <- o[seq(1, 64, 4)] * 16777216 + o[seq(2, 64, 4)] * 65536 + o[seq(3, 64, 4)] * 256 + o[seq(4, 64, 4)]
    for (t in 17:64) {
      s0 <- oux(oux(rotd(W[t - 15], 7), rotd(W[t - 15], 18)), W[t - 15] %/% 8)
      s1 <- oux(oux(rotd(W[t - 2], 17), rotd(W[t - 2], 19)), W[t - 2] %/% 1024)
      W[t] <- (W[t - 16] + s0 + W[t - 7] + s1) %% M32
    }
    a <- H[1]; bb <- H[2]; cc <- H[3]; d <- H[4]; e <- H[5]; f <- H[6]; g <- H[7]; h <- H[8]
    for (t in 1:64) {
      S1 <- oux(oux(rotd(e, 6), rotd(e, 11)), rotd(e, 25))
      ch <- oux(et(e, f), et(non(e), g))
      t1 <- (h + S1 + ch + K[t] + W[t]) %% M32
      S0 <- oux(oux(rotd(a, 2), rotd(a, 13)), rotd(a, 22))
      maj <- oux(oux(et(a, bb), et(a, cc)), et(bb, cc))
      t2 <- (S0 + maj) %% M32
      h <- g; g <- f; f <- e; e <- (d + t1) %% M32
      d <- cc; cc <- bb; bb <- a; a <- (t1 + t2) %% M32
    }
    H <- (H + c(a, bb, cc, d, e, f, g, h)) %% M32
  }
  paste(sprintf("%04x%04x", as.integer(H %/% 65536), as.integer(H %% 65536)), collapse = "")
}

# --- Entiers longs (base 10^4, chiffres de poids faible en tête) -------------
# Produits partiels < 10^8, sommes de colonnes < 10^8 x longueur : exacts en
# doubles. Suffisent au témoin entier de S3 quelle que soit la largeur.
BASE <- 10000
long_de <- function(n) {
  if (n < 0 || n != floor(n) || n >= 2^53) echec(sprintf("entier long : %s hors domaine", format(n)))
  r <- numeric(0)
  repeat { r <- c(r, n %% BASE); n <- n %/% BASE; if (n == 0) break }
  r
}
long_mul <- function(a, b) {
  r <- numeric(length(a) + length(b))
  for (i in seq_along(a)) {
    k <- i:(i + length(b) - 1L)
    r[k] <- r[k] + a[i] * b
  }
  for (k in seq_len(length(r) - 1L)) {
    r[k + 1L] <- r[k + 1L] + r[k] %/% BASE
    r[k] <- r[k] %% BASE
  }
  n <- length(r)
  while (n > 1L && r[n] == 0) n <- n - 1L
  r[seq_len(n)]
}
long_puis <- function(a, e) { r <- 1; for (k in seq_len(e)) r <- long_mul(r, a); r }
# Signe de a - b : -1, 0 ou 1.
long_cmp <- function(a, b) {
  if (length(a) != length(b)) return(if (length(a) > length(b)) 1L else -1L)
  for (k in rev(seq_along(a))) if (a[k] != b[k]) return(if (a[k] > b[k]) 1L else -1L)
  0L
}
long_chaine <- function(a) {
  n <- length(a)
  paste0(format(a[n], scientific = FALSE), paste(sprintf("%04d", as.integer(rev(a[-n]))), collapse = ""))
}
# Témoin entier du défaut au rang x (rangs dans le vecteur l) :
# (2 l_i - 1)^(j-x) (2 l_j - 1)^(x-i) > (2 l_x + 1)^(j-i).
temoin_entier <- function(l, i, x, j) {
  gauche <- long_mul(long_puis(long_de(2 * l[i] - 1), j - x), long_puis(long_de(2 * l[j] - 1), x - i))
  long_cmp(gauche, long_puis(long_de(2 * l[x] + 1), j - i)) > 0L
}

# --- Lecture d'un fichier de table au format 2 ----------------------------------
# Octets ASCII, LF, dernière ligne terminée ; en-tête age,<g_1>,…,<g_n>,
# générations et âges croissants et consécutifs ; cellule entière en chiffres
# seuls ou « - » ; au moins une valeur par ligne et par génération ; aucun
# « - » entre deux valeurs d'une génération ; l_x décroissants au sens large.
lire_grille <- function(chemin) {
  if (!file.exists(chemin)) echec(sprintf("%s : fichier absent", chemin))
  brut <- readBin(chemin, "raw", file.size(chemin))
  if (!length(brut) || brut[length(brut)] != as.raw(0x0a) || any(brut == as.raw(0x0d)))
    echec(sprintf("%s : fin de ligne (LF attendu, dernière ligne terminée)", chemin))
  if (any(brut > as.raw(0x7f))) echec(sprintf("%s : octet non ASCII", chemin))
  lignes <- strsplit(rawToChar(brut), "\n", fixed = TRUE)[[1L]]
  champs <- lapply(lignes, function(x) strsplit(paste0(x, ","), ",", fixed = TRUE)[[1L]])
  tete <- champs[[1L]]
  if (length(tete) < 2L || tete[1L] != "age" || !all(grepl("^[0-9]{4}$", tete[-1L])))
    echec(sprintf("%s : en-tête age,<générations> attendu", chemin))
  gens <- as.integer(tete[-1L])
  if (length(gens) > 1L && any(diff(gens) != 1L)) echec(sprintf("%s : générations non consécutives", chemin))
  corps <- champs[-1L]
  if (!length(corps)) echec(sprintf("%s : aucune ligne d'âge", chemin))
  larg <- vapply(corps, length, 0L)
  if (any(larg != length(tete))) echec(sprintf("%s : ligne %d de longueur anormale", chemin, which(larg != length(tete))[1L] + 1L))
  ages_txt <- vapply(corps, `[`, "", 1L)
  if (!all(grepl("^(0|[1-9][0-9]{0,2})$", ages_txt))) echec(sprintf("%s : âge mal formé", chemin))
  ages <- as.integer(ages_txt)
  if (length(ages) > 1L && any(diff(ages) != 1L)) echec(sprintf("%s : âges non consécutifs", chemin))
  cel <- do.call(rbind, lapply(corps, `[`, -1L))
  dim(cel) <- c(length(ages), length(gens))
  if (!all(grepl(CELLULE, cel))) {
    k <- which(!grepl(CELLULE, cel))[1L]
    echec(sprintf("%s : cellule mal formée « %s » (âge %d, génération %d)", chemin, cel[k],
                  ages[row(cel)[k]], gens[col(cel)[k]]))
  }
  L <- matrix(suppressWarnings(as.numeric(cel)), nrow(cel), ncol(cel))
  if (any(L > L_MAX, na.rm = TRUE)) echec(sprintf("%s : l_x supérieur à %s", chemin, format(L_MAX, scientific = FALSE)))
  if (any(rowSums(!is.na(L)) == 0L)) echec(sprintf("%s : ligne sans valeur (âge %d)", chemin, ages[which(rowSums(!is.na(L)) == 0L)[1L]]))
  for (j in seq_along(gens)) {
    v <- which(!is.na(L[, j]))
    if (!length(v)) echec(sprintf("%s : génération %d sans valeur", chemin, gens[j]))
    if (length(v) != v[length(v)] - v[1L] + 1L)
      echec(sprintf("%s : génération %d, « - » intercalé (âges %s)", chemin, gens[j],
                    plages(ages[setdiff(v[1L]:v[length(v)], v)])))
    if (length(v) > 1L && any(diff(L[v, j]) > 0))
      echec(sprintf("%s : génération %d, l_x croissant (âge %d)", chemin, gens[j], ages[v[which(diff(L[v, j]) > 0)[1L]] + 1L]))
  }
  list(chemin = chemin, sha = sha256(brut), ages = ages, gens = gens, L = L)
}

# --- Enveloppe concave supérieure (chaîne monotone) ------------------------------
# Rangs des sommets de la plus petite majorante concave de (x, y), x croissants.
enveloppe <- function(x, y) {
  h <- integer(0)
  for (k in seq_along(x)) {
    while (length(h) >= 2L) {
      o <- h[length(h) - 1L]; a <- h[length(h)]
      if ((x[a] - x[o]) * (y[k] - y[o]) - (y[a] - y[o]) * (x[k] - x[o]) >= 0) h <- h[-length(h)] else break
    }
    h <- c(h, k)
  }
  h
}

# --- Analyse d'une grille ---------------------------------------------------------
analyser <- function(g) {
  ages <- g$ages; gens <- g$gens; L <- g$L
  na <- length(ages); ng <- length(gens)
  rang <- function(a) a - ages[1L] + 1L
  sortie <- character(0)
  dire <- function(...) sortie <<- c(sortie, sprintf(...))
  dire("Fichier : %s", g$chemin)
  dire("SHA-256 : %s", g$sha)
  dire("Grille : %d générations (%d-%d), âges %d-%d", ng, gens[1L], gens[ng], ages[1L], ages[na])

  # S1. Structure.
  prem <- vapply(seq_len(ng), function(j) ages[min(which(!is.na(L[, j])))], 0L)
  dern <- vapply(seq_len(ng), function(j) ages[max(which(!is.na(L[, j])))], 0L)
  dire("S1. Structure")
  cle <- sprintf("%d-%d", prem, dern)
  for (k in unique(cle)) dire("  âges à valeur %s : générations %s", k, plages(gens[cle == k]))
  rad <- vapply(seq_len(ng), function(j) L[rang(prem[j]), j], 0)
  dire("  l_x = 100000 au premier âge à valeur : %d générations sur %d%s", sum(rad == 100000), ng,
       if (any(rad != 100000)) sprintf(" (autres : %s)", plages(gens[rad != 100000])) else "")
  dire("  « - » intercalé : aucun ; l_x décroissants au sens large : oui (refus sinon, à la lecture)")
  dire("  l_x nuls : %d", sum(L == 0, na.rm = TRUE))
  # Cellules où q_x est défini : l_x >= 1 et l_{x+1} valeur.
  lx <- L[-na, , drop = FALSE]; lx1 <- L[-1L, , drop = FALSE]
  def <- !is.na(lx) & lx >= 1 & !is.na(lx1)
  cell_age <- matrix(ages[-na], na - 1L, ng); cell_gen <- matrix(gens, na - 1L, ng, byrow = TRUE)
  un <- def & lx1 == 0
  dire("  q_x = 1 : %d%s", sum(un), if (any(un)) sprintf(" (âges %s ; générations %s)", plages(cell_age[un]), plages(cell_gen[un])) else "")
  egaux <- def & lx1 == lx
  dire("  l_x = l_{x+1} > 0 : %d%s", sum(egaux),
       if (any(egaux)) sprintf(" (%s)", paste(sprintf("%d à %d ans", cell_gen[egaux], cell_age[egaux])[order(cell_gen[egaux], cell_age[egaux])], collapse = ", ")) else "")
  # Maximum de q_x < 1, en fractions exactes (produits < 10^14).
  cand <- which(def & lx1 > 0)
  cand <- cand[order(cell_gen[cand], cell_age[cand])]
  max_q <- NULL
  if (length(cand)) {
    dd <- lx[cand] - lx1[cand]; ll <- lx[cand]
    best <- 1L; ex <- 1L
    for (k in seq_along(cand)[-1L]) {
      s <- dd[k] * ll[best] - dd[best] * ll[k]
      if (s > 0) { best <- k; ex <- k } else if (s == 0) ex <- c(ex, k)
    }
    pgcd <- function(a, b) { while (b != 0) { t <- a %% b; a <- b; b <- t }; a }
    p <- pgcd(dd[best], ll[best])
    max_q <- sprintf("%s/%s", format(dd[best] / p, scientific = FALSE), format(ll[best] / p, scientific = FALSE))
    dire("  maximum de q_x < 1 : %s, en %d cellule(s) : %s", max_q, length(ex),
         paste(sprintf("%d à %d ans (l_x = %s, l_{x+1} = %s)", cell_gen[cand[ex]], cell_age[cand[ex]],
                       format(lx[cand[ex]], scientific = FALSE), format(lx1[cand[ex]], scientific = FALSE)), collapse = ", "))
  }

  # S2. Décroissances de q_x aux âges x >= 31, couple (x-1, x).
  couples <- list()
  for (j in seq_len(ng)) for (a in ages[ages >= max(AGE_DECROISSANCE, ages[1L] + 1L) & ages < ages[na]]) {
    r <- rang(a)
    l0 <- L[r - 1L, j]; l1 <- L[r, j]; l2 <- L[r + 1L, j]
    if (is.na(l0) || is.na(l1) || is.na(l2) || l0 < 1 || l1 < 1) next
    if ((l1 - l2) * l0 < (l0 - l1) * l1) {
      gauche <- (2 * l2 - 1) * (2 * l0 - 1); droite <- (2 * l1 + 1)^2
      couples[[length(couples) + 1L]] <- data.frame(gen = gens[j], age = a, compatible = gauche <= droite,
                                                    egalite = gauche == droite)
    }
  }
  cp <- if (length(couples)) do.call(rbind, couples) else data.frame(gen = integer(0), age = integer(0), compatible = logical(0), egalite = logical(0))
  dire("S2. Décroissances de q_x (x >= %d) : %d couples, %d compatibles, %d incompatibles ; égalités du critère : %d",
       AGE_DECROISSANCE, nrow(cp), sum(cp$compatible), sum(!cp$compatible), sum(cp$egalite))
  for (a in sort(unique(cp$age))) {
    s <- cp[cp$age == a, ]
    dire("  %d ans : %d couples ; compatibles %d (%s) ; incompatibles %d (%s)", a, nrow(s),
         sum(s$compatible), plages(s$gen[s$compatible]), sum(!s$compatible), plages(s$gen[!s$compatible]))
  }

  # S3. Test joint.
  defauts <- list(); indecis <- character(0); min_m <- Inf; min_cell <- ""; min_pos <- Inf; pos_cell <- ""
  for (j in seq_len(ng)) {
    v <- which(!is.na(L[, j]) & L[, j] >= 1)
    a0 <- max(AGE_FENETRE, prem[j]); r <- v[ages[v] >= a0]
    if (length(r) < 3L) next
    l <- L[r, j]; x <- ages[r]
    lo <- log(l - 0.5); hi <- log(l + 0.5)
    h <- enveloppe(x, lo)
    M <- approx(x[h], lo[h], xout = x)$y
    m <- hi - M
    hors <- setdiff(seq_along(x), h)
    if (!length(hors)) next
    k <- hors[which.min(abs(m[hors]))]
    if (abs(m[k]) < min_m) { min_m <- abs(m[k]); min_cell <- sprintf("%d à %d ans", gens[j], x[k]) }
    # Plus petite marge positive : celle qui porte le statut des compatibilités.
    pos <- hors[m[hors] > 0]
    if (length(pos)) {
      k <- pos[which.min(m[pos])]
      if (m[k] < min_pos) { min_pos <- m[k]; pos_cell <- sprintf("%d à %d ans", gens[j], x[k]) }
    }
    for (k in hors[abs(m[hors]) <= DELTA]) indecis <- c(indecis, sprintf("%d à %d ans (m = %.3e)", gens[j], x[k], m[k]))
    for (k in hors[m[hors] < -DELTA]) {
      tem <- NULL
      for (w in 2:(length(x) - 1L)) {
        for (i in max(1L, k - w + 1L):min(k - 1L, length(x) - w)) {
          if (temoin_entier(l, i, k, i + w)) { tem <- c(i, i + w); break }
        }
        if (!is.null(tem)) break
      }
      if (is.null(tem)) echec(sprintf("%s : génération %d, âge %d : défaut en flottants sans témoin entier", g$chemin, gens[j], x[k]))
      defauts[[length(defauts) + 1L]] <- data.frame(gen = gens[j], age = x[k], i = x[tem[1L]], j = x[tem[2L]])
    }
  }
  if (length(indecis)) echec(sprintf("%s : test joint indécis (|m_x| <= %g) : %s", g$chemin, DELTA, paste(indecis, collapse = ", ")))
  df <- if (length(defauts)) do.call(rbind, defauts) else data.frame(gen = integer(0), age = integer(0), i = integer(0), j = integer(0))
  # Contrôle croisé : un couple incompatible est un défaut joint au même âge.
  inc <- cp[!cp$compatible, ]
  manque <- !paste(inc$gen, inc$age) %in% paste(df$gen, df$age)
  if (any(manque)) echec(sprintf("%s : couple incompatible sans défaut joint (%d à %d ans)", g$chemin, inc$gen[manque][1L], inc$age[manque][1L]))
  porteurs <- sum(vapply(seq_len(ng), function(j) sum(!is.na(L[, j]) & L[, j] >= 1 & ages >= AGE_FENETRE) >= 3L, NA))
  dire("S3. Test joint : %d générations en défaut sur %d testées (fenêtre de max(%d, premier âge) au dernier âge où l_x >= 1) ; indécis : 0 ; min |m_x| hors sommets : %.1e (%s)",
       length(unique(df$gen)), porteurs, AGE_FENETRE, min_m, min_cell)
  dire("  min m_x > 0 hors sommets : %.2e (%s)", min_pos, pos_cell)
  for (a in sort(unique(df$age))) {
    # Générations dont la fenêtre porte les âges a-1 à a+1 (l >= 1, âges >= 30).
    n_a <- sum(vapply(seq_len(ng), function(j) {
      r <- rang((a - 1L):(a + 1L))
      a - 1L >= AGE_FENETRE && all(r >= 1L & r <= na) && all(!is.na(L[r, j]) & L[r, j] >= 1)
    }, NA))
    dire("  âge en défaut %d : %d générations sur les %d qui portent les âges %d à %d (%s)", a, sum(df$age == a), n_a,
         a - 1L, a + 1L, plages(df$gen[df$age == a]))
  }
  tem <- sprintf("%d-%d", df$i, df$j)
  for (t in sort(unique(tem))) dire("  témoin entier %s (largeur %d) : %d défaut(s), générations %s", t,
                                    df$j[tem == t][1L] - df$i[tem == t][1L], sum(tem == t), plages(df$gen[tem == t]))
  seul <- setdiff(unique(df$gen), inc$gen)
  dire("  générations en défaut joint, compatibles couple par couple : %s", plages(seul))

  # S4. Médianes des rapports q_x / q_{x-1}.
  dire("S4. Rapports q_x / q_{x-1}, générations %d-%d qui portent les âges x-1 à x+1 (médiane [min ; max])",
       MEDIANES_GENERATIONS[1L], MEDIANES_GENERATIONS[length(MEDIANES_GENERATIONS)])
  q <- function(a, j) { r <- rang(a); (L[r, j] - L[r + 1L, j]) / L[r, j] }
  porte <- function(a, j) { r <- rang(a); r >= 1L && r <= na && !is.na(L[r, j]) && L[r, j] >= 1 }
  jj <- which(gens %in% MEDIANES_GENERATIONS)
  medianes <- c()
  for (a in MEDIANES_AGES) {
    if (a - 1L < ages[1L] || a + 1L > ages[na]) next
    sel <- jj[vapply(jj, function(j) porte(a - 1L, j) && porte(a, j) && !is.na(L[rang(a + 1L), j]), NA)]
    if (!length(sel)) next
    rr <- vapply(sel, function(j) q(a, j) / q(a - 1L, j), 0)
    medianes[as.character(a)] <- median(rr)
    dire("  %d ans : %d générations, %.4f [%.4f ; %.4f]", a, length(sel), median(rr), min(rr), max(rr))
  }
  if (58L >= ages[1L] && 62L <= ages[na] + 1L) {
    sel <- jj[vapply(jj, function(j) all(vapply(58:60, porte, NA, j = j)) && !is.na(L[rang(61L), j]), NA)]
    if (length(sel)) dire("  q_60 / q_58 : %d générations (âges 58 à 61), médiane %.4f", length(sel),
                          median(vapply(sel, function(j) q(60L, j) / q(58L, j), 0)))
  }
  list(sortie = sortie, couples = cp, defauts = df, max_q = max_q, min_m = min_m, min_pos = min_pos)
}

# --- Témoin ---------------------------------------------------------------------
# Grilles forgées, vérifiées à la main en entiers (correctif d'actuary-
# approfondi, https://github.com/Gerard-Garey/outil_tables/issues/279#issuecomment-6892068057).
#
#  T1, âges 30 à 33, générations 2001 à 2003 :
#    2001 : 1000, 990, 985, -   couple (30, 31) : décroît, 5 x 1000 = 5 000 <
#           10 x 990 = 9 900 ; incompatible, (2 x 985 - 1)(2 x 1000 - 1) =
#           1969 x 1999 = 3 936 031 > (2 x 990 + 1)^2 = 1981^2 = 3 924 361 ;
#           défaut joint à 31 ans, témoin 30-32 (largeur 2).
#    2002 : 1000, 985, 970, 957 couple (30, 31) : ne décroît pas, 15 x 1000 =
#           15 000 >= 15 x 985 = 14 775 ; couple (31, 32) : décroît, 13 x 985 =
#           12 805 < 15 x 970 = 14 550, compatible, 1913 x 1969 = 3 766 697 <=
#           1941^2 = 3 767 481 ; défaut joint à 32 ans seulement, témoin 30-33
#           (largeur 3) : 1999 x 1913^2 = 7 315 478 431 > 1941^3 =
#           7 312 680 621 ; à 31 ans, aucun témoin : (30, 31, 32) est le couple
#           compatible 1939 x 1999 = 3 876 061 <= 1971^2 = 3 884 841, et
#           (30, 31, 33) donne 1999^2 x 1913 = 7 644 349 913 <= 1971^3 =
#           7 657 021 611.
#    2003 : 1000, 990, 981, -   couple (30, 31) : décroît, 9 x 1000 = 9 000 <
#           10 x 990 = 9 900 ; compatible, 1961 x 1999 = 3 920 039 <= 1981^2 =
#           3 924 361 ; aucun défaut joint (fenêtre de trois âges).
#    Attendu : 3 couples, 2 compatibles, 1 incompatible, 0 égalité ; défauts
#    joints 2001 à 31 ans (30-32) et 2002 à 32 ans (30-33) ; défaut joint
#    seulement : 2002 ; maximum de q_x < 1 : 3/197, en 2002 à 31 ans
#    (2002 : q_30 = 15/1000 = 3/200, q_31 = 15/985 = 3/197, q_32 = 13/970 ;
#    2001 : 1/100, 1/198 ; 2003 : 1/100, 1/110 ; 3 x 200 = 600 > 3 x 197 =
#    591, 15 x 970 = 14 550 > 13 x 985 = 12 805).
#  T2 (égalité, indécis) : âges 30 à 32, génération 2001 : 13, 7, 5 ; couple
#    (30, 31) : décroît, 2 x 13 = 26 < 6 x 7 = 42 ; égalité du critère, 9 x 25
#    = 225 = 15^2 ; m_31 = 0 en réels (log 7,5 = (log 12,5 + log 4,5) / 2,
#    car 12,5 x 4,5 = 56,25 = 7,5^2) : le script doit échouer (indécis).
#  T3 et T4 (refus) : « - » intercalé ; l_x croissant.
temoin <- function() {
  dossier <- tempfile("controles_coherence_temoin_")
  dir.create(dossier)
  ici <- setwd(dossier)  # chemins relatifs : sortie du témoin indépendante du dossier temporaire
  on.exit({ setwd(ici); unlink(dossier, recursive = TRUE) }, add = TRUE)
  forger <- function(nom, texte) {
    f <- nom
    con <- file(f, "wb"); writeBin(charToRaw(texte), con); close(con)
    f
  }
  ok <- TRUE
  verifier <- function(libelle, cond) {
    ecrire(sprintf("  %s : %s", if (isTRUE(cond)) "tenu" else "NON TENU", libelle))
    if (!isTRUE(cond)) ok <<- FALSE
  }
  echoue <- function(f, motif) {
    m <- tryCatch({ analyser(lire_grille(f)); "" }, echec_controle = function(e) conditionMessage(e))
    grepl(motif, m, fixed = TRUE)
  }
  ecrire("Témoin")
  # Entiers longs : valeurs calculées à la main.
  verifier("entier long 1999 x 1913^2 = 7315478431",
           long_chaine(long_mul(long_de(1999), long_puis(long_de(1913), 2L))) == "7315478431")
  verifier("entier long 1941^3 = 7312680621", long_chaine(long_puis(long_de(1941), 3L)) == "7312680621")
  verifier("entier long 200001^4 = 1600032000240000800001 (> 2^53)",
           long_chaine(long_puis(long_de(200001), 4L)) == "1600032000240000800001")
  f1 <- forger("t1.csv", "age,2001,2002,2003\n30,1000,1000,1000\n31,990,985,990\n32,985,970,981\n33,-,957,-\n")
  r <- analyser(lire_grille(f1))
  for (x in r$sortie) ecrire(paste0("    ", x))
  cp <- r$couples; df <- r$defauts
  verifier("T1 : 3 couples décroissants, 2001 et 2003 à 31 ans, 2002 à 32 ans",
           identical(paste(cp$gen, cp$age), c("2001 31", "2002 32", "2003 31")))
  verifier("T1 : seul 2001 à 31 ans incompatible ; 0 égalité",
           identical(cp$compatible, c(FALSE, TRUE, TRUE)) && !any(cp$egalite))
  verifier("T1 : défauts joints 2001 à 31 ans (témoin 30-32), 2002 à 32 ans (témoin 30-33)",
           identical(paste(df$gen, df$age, df$i, df$j), c("2001 31 30 32", "2002 32 30 33")))
  verifier("T1 : maximum de q_x < 1 égal à 3/197", identical(r$max_q, "3/197"))
  f2 <- forger("t2.csv", "age,2001\n30,13\n31,7\n32,5\n")
  verifier("T2 : égalité du critère par couple, test joint indécis, échec", echoue(f2, "test joint indécis"))
  f3 <- forger("t3.csv", "age,2001,2002\n30,1000,1000\n31,-,990\n32,980,980\n")
  verifier("T3 : « - » intercalé refusé", echoue(f3, "intercalé"))
  f4 <- forger("t4.csv", "age,2001\n30,1000\n31,990\n32,995\n")
  verifier("T4 : l_x croissant refusé", echoue(f4, "l_x croissant"))
  ok
}

# --- Niveau principal ---------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
tryCatch({
  ecrire(sprintf("controles_coherence_generation.R (%s)", R.version.string))
  if (identical(args, "--temoin")) {
    if (!temoin()) echec("témoin non tenu")
    ecrire("OK : témoin tenu")
  } else {
    if (!length(args) || any(startsWith(args, "--"))) echec("usage : controles_coherence_generation.R <fichier.csv> [...] | --temoin")
    for (f in args) { ecrire(""); ecrire(analyser(lire_grille(f))$sortie) }
  }
}, echec_controle = function(e) {
  ecrire(paste0("ECHEC : ", conditionMessage(e)), stderr())
  quit(save = "no", status = 1L)
})

})
