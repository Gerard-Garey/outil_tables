# Échantillon de l'oracle de (3a), #104. Générateur et graine fixés.
# Usage : Rscript echantillon.R <dossier de sortie> (lit doubles_R1_R6.rds dans ce dossier)
# Sortie : echantillon.rds = list(x = doubles finis, famille = famille de chaque x,
#          non_finis = doubles non finis à charge utile variée, egalites = indices des égalités exactes)
sortie <- commandArgs(TRUE)[1]
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(20261026L)

# Double de champ d'exposant E (0 à 2047), fraction f = hi·2^32 + lo (hi < 2^20, lo < 2^32), signe s.
de_bits <- function(E, hi, lo, s = 0) {
  n <- max(length(E), length(hi), length(lo), length(s))
  E <- rep_len(E, n); hi <- rep_len(hi, n); lo <- rep_len(lo, n); s <- rep_len(s, n)
  o <- matrix(as.raw(0), 8L, n)
  for (k in 1:4) { o[k, ] <- as.raw(lo %% 256); lo <- lo %/% 256 }
  o[5L, ] <- as.raw(hi %% 256); hi <- hi %/% 256
  o[6L, ] <- as.raw(hi %% 256); hi <- hi %/% 256
  o[7L, ] <- as.raw((E %% 16) * 16 + hi); o[8L, ] <- as.raw(s * 128 + E %/% 16)
  readBin(as.vector(o), "double", n = n, size = 8L, endian = "little")
}
# Mots de 32 bits (non signés, en double) d'un double : list(hi, lo).
mots <- function(v) {
  o <- matrix(as.integer(writeBin(v, raw(), size = 8L, endian = "little")), 8L)
  list(hi = ((o[8L, ] * 256 + o[7L, ]) * 256 + o[6L, ]) * 256 + o[5L, ],
       lo = ((o[4L, ] * 256 + o[3L, ]) * 256 + o[2L, ]) * 256 + o[1L, ])
}
# Voisin à k ulp (k entier, |k| petit) d'un double fini > 0, par ses bits ; NA s'il sort des finis > 0.
voisin <- function(v, k) {
  w <- mots(v); lo <- w$lo + k; hi <- w$hi
  b <- lo < 0; lo[b] <- lo[b] + 2^32; hi[b] <- hi[b] - 1
  b <- lo >= 2^32; lo[b] <- lo[b] - 2^32; hi[b] <- hi[b] + 1
  ok <- hi >= 0 & hi < 2047 * 2^20
  r <- rep(NA_real_, length(v))
  r[ok] <- de_bits(hi[ok] %/% 2^20, hi[ok] %% 2^20, lo[ok])
  r
}
voisins <- function(v, ks) { u <- unlist(lapply(ks, function(k) voisin(v, k))); u[!is.na(u) & u > 0] }
fraction <- function(n) list(hi = floor(runif(n) * 2^20), lo = floor(runif(n) * 2^32))

parts <- list()
# (a) motifs de 64 bits au hasard, finis, signe au hasard
n <- 200000; f <- fraction(n)
parts$a <- de_bits(floor(runif(n) * 2047), f$hi, f$lo, floor(runif(n) * 2))
# (b) chaque champ d'exposant E de 0 à 2046, 100 mantisses chacun
E <- rep(0:2046, each = 100); f <- fraction(length(E)); parts$b <- de_bits(E, f$hi, f$lo)
# (c) sous-normaux m = 1..2000, plus grand sous-normal, plus petit normal, 5e-324
parts$c <- c(de_bits(0, 0, 1:2000), de_bits(0, 2^20 - 1, 2^32 - 1), de_bits(1, 0, 0), 5e-324)
# (d) .Machine$double.xmax et ses 21 prédécesseurs
parts$d <- c(.Machine$double.xmax, voisins(.Machine$double.xmax, -(1:21)))
# (e) puissances de 2 et de 10, voisins à ±1, ±2, ±3 ulp
p2 <- 2^(-1074:1023); p10 <- as.numeric(paste0("1e", -323:308))
parts$e <- c(p2, p10, voisins(c(p2, p10), c(-3:-1, 1:3)))
# (f) égalités exactes : m impair, m·5^n dans [1e17, 1e18), x = m·2^-n, n de 2 à 25 ; toutes les
#     valeurs si au plus 5 000, sinon les bornes (deux premiers et deux derniers m impairs) et
#     3 000 m impairs tirés ; plus leurs voisins à ±1 ulp et deux translations binaires (voisinage).
egalite <- list()
for (n in 2:25) {
  lo <- ceiling(1e17 / 5^n); hi <- min(floor((1e18 - 1) / 5^n), 2^53 - 1)
  while (lo * 5^n < 1e17) lo <- lo + 1
  while (hi * 5^n >= 1e18) hi <- hi - 1
  if (lo %% 2 == 0) lo <- lo + 1
  if (hi %% 2 == 0) hi <- hi - 1
  if (lo > hi) next
  nb <- (hi - lo) / 2 + 1
  m <- if (nb <= 5000) seq(lo, hi, by = 2) else
    unique(c(lo, lo + 2, hi - 2, hi, lo + 2 * floor(runif(3000) * nb)))
  egalite[[as.character(n)]] <- m * 2^-n
}
eg <- unlist(egalite, use.names = FALSE)
parts$f <- c(eg, voisins(eg, c(-1, 1)), eg * 2^-30, eg * 2^40)
# (g) presque-égalités : décimal à 18 chiffres finissant par 5, double le plus proche (lecture de R) et ±2 ulp
n <- 30000
d17 <- paste0(floor(runif(n) * 9 + 1), sprintf("%08.0f", floor(runif(n) * 1e8)), sprintf("%08.0f", floor(runif(n) * 1e8)))
ex <- floor(runif(n) * 600) - 300
mil <- as.numeric(paste0(substr(d17, 1, 1), ".", substr(d17, 2, 17), "5e", ex))
mil <- mil[is.finite(mil) & mil > 0]
parts$g <- c(mil, voisins(mil, c(-2, -1, 1, 2)))
# (h) vecteurs de l'ADR 0011 et de #104 (0x1.422927add0ecbp-18 et son voisin ...eccp-18)
parts$h <- c(0.95, 1, 0, -0, 1e-9, 0.1, 5e-324, 4.8005694404694591e-06,
             de_bits(1005, 0x42292, 0x7add0ecb), de_bits(1005, 0x42292, 0x7add0ecc))
# (i) entiers, frontières fixe / exponentielle (1e16, 1e17, 1e-4, 1e-5) et voisinages
parts$i <- c(0:100000, floor(runif(20000) * 1e15), 1e16 + 2 * (0:2000), 1e17 - 16 * (0:2000),
             voisins(c(1e-4, 1e-5, 1e16, 1e17, 1e15, 1e-3), c(-50:-1, 1:50)),
             99999999999999984, 99999999999999992, 1e17 - 8, 9999999999999998, 0.00010000000000000009)
# (j) doubles finis réellement écrits par R1 à R6
r <- readRDS(file.path(sortie, "doubles_R1_R6.rds")); parts$j <- r[is.finite(r)]

parts <- lapply(parts, function(p) p[is.finite(p)])
x <- unlist(parts, use.names = FALSE)
famille <- rep(names(parts), lengths(parts))
neg <- seq(1L, length(x), by = 7L)
x <- c(x, -x[neg]); famille <- c(famille, famille[neg])
egalites <- which(x %in% c(eg, -eg))

# (k) non finis à charge utile variée : NA de R, NA + 1, NaN de R, 0/0, -NaN, NaN à charge utile
#     au hasard (signe, bit silencieux, bits hauts), NA à bits hauts modifiés (mot bas 1954), Inf, -Inf.
k <- 2000
nf <- c(NA_real_, NA_real_ + 1, NaN, 0 / 0, -NaN, Inf, -Inf,
        de_bits(2047, floor(runif(k) * 2^20), floor(runif(k) * 2^32), floor(runif(k) * 2)),
        de_bits(2047, floor(runif(200) * 2^20), 1954, floor(runif(200) * 2)),
        de_bits(2047, 2^19, 1954, 0:1), de_bits(2047, 0, 1954, 0:1), de_bits(2047, 1, 0, 0:1))
nf <- nf[!is.finite(nf)]

saveRDS(list(x = x, famille = famille, non_finis = nf, egalites = egalites,
             egalites_par_n = lengths(egalite)), file.path(sortie, "echantillon.rds"))
tab <- table(factor(famille, levels = names(parts)))
print(tab)
cat("finis N =", length(x), "; dont négatifs ajoutés :", length(neg), "; égalités exactes (signe compris) :",
    length(egalites), "; non finis :", length(nf), "\n")
cat("égalités exactes par n (sans les négatifs) :\n"); print(lengths(egalite))
cat("non finis : NA", sum(is.na(nf) & !is.nan(nf)), "; NaN", sum(is.nan(nf)), "; Inf", sum(nf == Inf, na.rm = TRUE),
    "; -Inf", sum(nf == -Inf, na.rm = TRUE), "\n")
