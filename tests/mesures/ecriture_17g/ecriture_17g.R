# -----------------------------------------------------------------------------
#  Prototype de (3a), #104 : écriture %.17g correctement arrondie, calculée par
#  tablexp, sans la bibliothèque C (ADR 0011, point 2.6, option (v) ; M148 ;
#  spécification d'actuary-approfondi sur #104, algorithme V1, invariants I1
#  à I5). Forme visée : code de premier niveau de R/rapport.R, placé avant
#  ecrire_valeurs(), dont la fermeture doubles_texte() deviendrait
#      doubles_texte <- function(x) octets_17g(x)
#  Rien d'autre ne change. R base seulement ; aucun double n'est mis en forme
#  par sprintf(), format(), formatC() ni as.character() : les chiffres sont
#  calculés sur des entiers exacts, puis écrits en octets ASCII.
# -----------------------------------------------------------------------------

# Base des tranches des grands entiers (poids faible en colonne 1) : 10^7. Un
# produit de deux tranches est < 10^14, une somme de trois produits alignés
# < 3·10^14, un produit par 5 ou 2 < 5·10^7 : tout reste entier < 2^53, donc
# exact en double (I3).
BASE_TRANCHES <- 1e7

# Quotient et reste de v (entiers >= 0 < 2^53, vecteur ou matrice) par la base
# : q = floor(v / B), corrigé d'une unité si l'arrondi de v / B l'a décalé
# (r < 0 : q - 1 ; r >= B : q + 1), ce qui rend q et r exacts quel que soit cet
# arrondi (I4). Ni %% ni %/% sur des doubles.
diviser_tranche <- function(v) {
  q <- floor(v / BASE_TRANCHES)
  r <- v - q * BASE_TRANCHES
  k <- r < 0
  q <- q - k
  r <- r + k * BASE_TRANCHES
  k <- r >= BASE_TRANCHES
  list(q = q + k, r = r - k * BASE_TRANCHES)
}

# Report des retenues d'une matrice de grands entiers (une ligne par entier,
# tranches de poids croissant en colonnes), jusqu'à ce qu'aucune retenue ne
# reste. L'appelant donne assez de colonnes pour la valeur (Z < B^ncol) : la
# dernière colonne ne produit alors aucune retenue (garde-fou).
reporter_retenues <- function(v) {
  w <- ncol(v)
  repeat {
    d <- diviser_tranche(v)
    if (!any(d$q != 0)) return(d$r)
    if (any(d$q[, w] != 0))
      stop("reporter_retenues() : dépassement de la dernière tranche (erreur de programmation).", call. = FALSE)
    v <- d$r
    v[, -1L] <- v[, -1L] + d$q[, -w]
  }
}

# Table des puissances en tranches, constante d'installation : ligne e + 1075,
# e de -1074 à 1023, porte 5^-e si e < 0 et 2^e si e >= 0 (assez pour tout
# double fini non nul, I1), par multiplications successives par 5 et par 2 ;
# `longueur` : nombre de tranches de chaque ligne (108 au plus, 5^1074).
table_puissances_tranches <- function() {
  lignes <- vector("list", 2098L)
  suivante <- function(t, facteur) {
    t <- reporter_retenues(matrix(c(t * facteur, 0), nrow = 1L))[1L, ]
    if (t[length(t)] == 0) t[-length(t)] else t
  }
  t <- 1
  lignes[[1075L]] <- t
  for (n in 1:1074) {
    t <- suivante(t, 5)
    lignes[[1075L - n]] <- t
  }
  t <- 1
  for (e in 1:1023) {
    t <- suivante(t, 2)
    lignes[[1075L + e]] <- t
  }
  longueur <- lengths(lignes)
  tranches <- matrix(0, length(lignes), max(longueur))
  for (k in seq_along(lignes)) tranches[k, seq_len(longueur[k])] <- lignes[[k]]
  list(tranches = tranches, longueur = longueur)
}
PUISSANCES_TRANCHES <- table_puissances_tranches()

# Dix-sept chiffres de a (doubles finis > 0), arrondis au plus proche, égalité
# au pair, et exposant décimal X : a ~ d1,d2...d17 · 10^X (I5). Étapes :
#  - I1 : a = m·2^e exactement, m impair < 2^53, lus sur les octets IEEE 754
#    (champ d'exposant et fraction recomposés en entiers exacts) ;
#  - I2 : a = Z·10^-s exactement, Z = m·2^e et s = 0 si e >= 0, Z = m·5^-e et
#    s = -e sinon ; Z en tranches, produit de m (trois tranches r0, r1, q2) par
#    la ligne e de PUISSANCES_TRANCHES ;
#  - arrondi sur les quatre tranches hautes de Z (au moins 22 chiffres, dont
#    les 18 premiers) ; les chiffres au-delà du 18e et les tranches plus basses
#    disent seulement si un reste non nul existe.
# Les valeurs sont groupées par classe de longueur de la ligne de la table
# (huit tranches par classe), et chaque groupe découpé pour qu'une matrice de
# tranches compte au plus 2^18 éléments (mémoire bornée, #83).
chiffres_17 <- function(a) {
  n <- length(a)
  o <- matrix(as.integer(writeBin(a, raw(), size = 8L, endian = "little")), nrow = 8L)
  champ <- (o[8L, ] %% 128L) * 16L + o[7L, ] %/% 16L
  f <- o[7L, ] %% 16L
  for (j in 6:1) f <- f * 256 + o[j, ]
  normal <- champ > 0L
  m <- f + normal * 2^52
  e <- ifelse(normal, champ - 1075L, -1074L)
  # m impair (I1) : divisions par 2 exactes, tant que m est pair.
  k <- seq_len(n)
  repeat {
    k <- k[floor(m[k] / 2) * 2 == m[k]]
    if (!length(k)) break
    m[k] <- m[k] / 2
    e[k] <- e[k] + 1L
  }
  d1 <- diviser_tranche(m)
  d2 <- diviser_tranche(d1$q)
  r0 <- d1$r
  r1 <- d2$r
  q2 <- d2$q
  ligne <- e + 1075L
  longueur <- PUISSANCES_TRANCHES$longueur[ligne]
  classe <- (longueur + 7L) %/% 8L
  # Quatre tranches hautes de Z (de la plus haute à la plus basse), rang de la
  # plus haute, reste non nul sous elles.
  hautes <- matrix(0, n, 4L)
  rang <- integer(n)
  reste <- logical(n)
  for (cl in sort(unique(classe))) {
    i_classe <- which(classe == cl)
    lg <- max(longueur[i_classe])
    # Trois colonnes de zéros sous Z (colonnes 1 à 3), Z en colonnes 4 à lg + 6.
    largeur <- lg + 6L
    taille <- max(1L, 262144L %/% largeur)
    for (debut in seq(1L, length(i_classe), by = taille)) {
      i <- i_classe[debut:min(length(i_classe), debut + taille - 1L)]
      t <- PUISSANCES_TRANCHES$tranches[ligne[i], seq_len(lg), drop = FALSE]
      z <- matrix(0, length(i), largeur)
      z[, 3L + seq_len(lg)] <- r0[i] * t
      z[, 4L + seq_len(lg)] <- z[, 4L + seq_len(lg)] + r1[i] * t
      z[, 5L + seq_len(lg)] <- z[, 5L + seq_len(lg)] + q2[i] * t
      z <- reporter_retenues(z)
      non_nul <- (z != 0) + 0
      haut <- max.col(non_nul, ties.method = "last")
      bas <- max.col(non_nul, ties.method = "first")
      lignes_i <- seq_along(i)
      for (j in 1:4) hautes[i, j] <- z[cbind(lignes_i, haut - j + 1L)]
      rang[i] <- haut - 3L
      reste[i] <- bas < haut - 3L
    }
  }
  # Vingt-huit chiffres des quatre tranches hautes (sept par tranche, rang 6 à
  # 0), en entiers.
  p10 <- c(1000000L, 100000L, 10000L, 1000L, 100L, 10L, 1L)
  ch <- matrix(as.integer(hautes)[rep(seq_len(n), 28L) + n * rep(rep(0:3, each = 7L), each = n)], n)
  ch <- ch %/% rep(rep(p10, 4L), each = n) %% 10L
  # Premier chiffre non nul dans la tranche haute (non nulle : a > 0), N
  # chiffres de Z, puis d1...d18 et reste au-delà du 18e chiffre.
  decalage <- max.col((ch[, 1:7, drop = FALSE] != 0) + 0, ties.method = "first") - 1L
  nb_chiffres <- 7L * (rang - 1L) + 7L - decalage
  exposant <- nb_chiffres - 1L - pmax(0L, -e)
  d <- matrix(ch[cbind(rep(seq_len(n), 18L), rep(decalage, 18L) + rep(1:18, each = n))], n)
  reste <- reste | rowSums(ch * (col(ch) > decalage + 18L)) > 0
  # Arrondi au plus proche, égalité au pair : vers le haut si d18 > 5, ou
  # d18 = 5 et (reste non nul ou d17 impair) ; retenue sur les 17 chiffres ;
  # 10^17 devient 1 suivi de seize zéros, exposant + 1.
  plus <- d[, 18L] > 5L | (d[, 18L] == 5L & (reste | d[, 17L] %% 2L == 1L))
  d <- d[, 1:17, drop = FALSE]
  for (j in 17:1) {
    if (!any(plus)) break
    d[plus, j] <- d[plus, j] + 1L
    plus <- plus & d[, j] == 10L
    d[plus, j] <- 0L
  }
  d[plus, 1L] <- 1L
  exposant[plus] <- exposant[plus] + 1L
  list(chiffres = d, exposant = exposant)
}

# Octets ASCII de %.17g (conversion g de la norme C, précision 17, sans #)
# pour des doubles finis non nuls y, une colonne par valeur, une ligne par
# position, 0 aux places inoccupées : notation fixe si -4 <= X < 17 (16 - X
# décimales), sinon d.ddd...e±XX (exposant sur deux chiffres au moins) ; zéros
# de fin retirés, point retiré s'il ne reste rien après lui ; « - » en tête si
# y < 0.
positions_17g <- function(y) {
  c17 <- chiffres_17(abs(y))
  d <- c17$chiffres
  x <- c17$exposant
  n <- length(y)
  signe <- (y < 0) + 0L
  significatifs <- max.col((d != 0) + 0, ties.method = "last")
  fixe <- x >= -4L & x < 17L
  entier <- fixe & x >= 0L
  # Mantisse : `zeros` zéros puis les chiffres 1 à `nd` de d, point après la
  # position `pp` s'il reste des chiffres après lui ; puis l'exposant (« e »,
  # signe, `ne` chiffres).
  zeros <- ifelse(fixe & x < 0L, -x, 0L)
  nd <- ifelse(entier, pmax(significatifs, x + 1L), significatifs)
  pp <- ifelse(entier, x + 1L, 1L)
  point <- zeros + nd > pp
  lm <- zeros + nd + point
  ax <- abs(x)
  ne <- ifelse(fixe, 0L, ifelse(ax >= 100L, 3L, 2L))
  le <- ifelse(fixe, 0L, 2L + ne)
  largeur <- max(signe + lm + le)
  par_valeur <- function(v) rep(v, each = largeur)
  colonne <- rep(seq_len(n), each = largeur)
  j <- rep(seq_len(largeur), n) - par_valeur(signe)
  sortie <- integer(largeur * n)
  sortie[j == 0L] <- 45L
  lm_j <- par_valeur(lm)
  k <- j >= 1L & j <= lm_j
  pp_j <- par_valeur(pp) + 1L
  est_point <- k & par_valeur(point) & j == pp_j
  sortie[est_point] <- 46L
  k <- k & !est_point
  rang <- j[k] - (par_valeur(point)[k] & j[k] > pp_j[k]) - par_valeur(zeros)[k]
  sortie[k] <- 48L + ifelse(rang >= 1L, d[cbind(colonne[k], pmax(rang, 1L))], 0L)
  u <- j - lm_j
  le_j <- par_valeur(le)
  sortie[u == 1L & le_j > 0L] <- 101L
  k <- u == 2L & le_j > 0L
  sortie[k] <- ifelse(x[colonne[k]] < 0L, 45L, 43L)
  k <- u >= 3L & u <= le_j
  q <- par_valeur(ne)[k] - (u[k] - 2L)
  sortie[k] <- 48L + ax[colonne[k]] %/% c(1L, 10L, 100L)[q + 1L] %% 10L
  matrix(sortie, largeur)
}

# Octets de l'écriture « texte » d'un vecteur de doubles (une valeur par
# ligne, LF final), égaux à ceux de sprintf("%.17g") de la glibc, mesurés : un
# fini s'écrit depuis x + 0 (-0 devient 0, « 0 ») ; un non fini par son jeton,
# lu sur la valeur reçue (NA si is.na & !is.nan, NaN, Inf, -Inf), comme
# doubles_empreinte(). Par blocs de 2^14 valeurs (mémoire bornée, mesurée).
octets_17g <- function(x) {
  n <- length(x)
  if (!n) return(raw(0))
  bloc <- 16384L
  if (n > bloc) {
    debuts <- seq(1L, n, by = bloc)
    return(unlist(lapply(debuts, function(i) octets_17g(x[i:min(n, i + bloc - 1L)])), use.names = FALSE))
  }
  fini <- is.finite(x)
  y <- x[fini] + 0
  nul <- y == 0
  p <- if (any(!nul)) positions_17g(y[!nul]) else matrix(0L, 0L, 0L)
  largeur <- max(4L, nrow(p))
  m <- matrix(0L, largeur + 1L, n)
  m[largeur + 1L, ] <- 10L
  colonnes <- which(fini)
  if (any(!nul)) m[seq_len(nrow(p)), colonnes[!nul]] <- p
  m[1L, colonnes[nul]] <- 48L
  z <- x[!fini]
  jeton <- ifelse(is.nan(z), 2L, ifelse(is.na(z), 1L, ifelse(z > 0, 3L, 4L)))
  jetons <- list(c(78L, 65L), c(78L, 97L, 78L), c(73L, 110L, 102L), c(45L, 73L, 110L, 102L))
  for (k in seq_along(jetons)) {
    i <- which(!fini)[jeton == k]
    if (length(i)) m[seq_along(jetons[[k]]), i] <- jetons[[k]]
  }
  as.raw(m[m != 0L])
}
