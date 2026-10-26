# -----------------------------------------------------------------------------
#  tests/controles/ecriture_doubles_texte.R : recoupement de l'écriture des
#  doubles en forme « texte » (%.17g correctement arrondi, calculé par tablexp
#  sans la bibliothèque C, octets_17g()) avec deux juges indépendants de
#  tablexp, la glibc (sprintf("%.17g") de R) et Python (format(x, '.17g'))
#  (issue #104 ; M148 ; avis d'actuary sur l'étape (3b) ; pendant de
#  tests/controles/ecriture_doubles.R pour la forme « empreinte »)
#
#  Hors batterie (durée, dépendance à Python) : la batterie unitaire garde les
#  témoins T1 (textes écrits en dur), T2 et T3 (MD5 du texte attendu)
#  (tests/unitaires/test_construire_table.R) ; ce contrôle recalcule ces MD5
#  et vérifie qu'ils sont ceux écrits dans la batterie.
#
#  Méthode. Tous les doubles sont construits par leurs octets (readBin()) ou
#  par des opérations IEEE exactes, sous graine locale (sous_graine), sans
#  lecture d'un décimal :
#    - échantillon de n doubles (recette de la batterie et de
#      ecriture_doubles.R : motifs de 64 bits tirés au hasard, un motif non
#      fini remplacé par 0.5, valeurs de runif() transformées) ;
#    - égalités exactes tirées au hasard (x = m·2^-k, m impair < 2^53,
#      m·5^k dans [10^17, 10^18), k de 2 à 25 : le développement décimal
#      exact de x a 18 chiffres significatifs, le dernier 5), leurs voisins à
#      ±1 ulp par les bits (presque-égalités) et leurs opposés ;
#    - T1 (29 cas limites de la batterie), T2 (121 308 égalités exactes
#      énumérées et leurs voisins), T3 (10^5 doubles, graine 20261019) ;
#    - non finis : NA_real_, NA_real_ + 1, NaN, -NaN, Inf, -Inf, et NaN à
#      charge utile tirée au hasard (signe, fraction), dont des NA (mot bas
#      1954).
#  Le tout est écrit par ecrire_valeurs(x, "texte", ...), puis comparé octet
#  pour octet :
#    1. à la glibc : sprintf("%.17g", x + 0) pour un fini, le jeton de R
#       (sprintf("%.17g", x) : NA, NaN, Inf, -Inf) sinon ; sauté sous Windows
#       (la bibliothèque C de Windows n'arrondit pas toujours correctement,
#       #104) ;
#    2. à Python, sur toute plateforme : les octets des doubles sont relus par
#       struct.unpack('<d'), chaque fini écrit par format(x + 0.0, '.17g'),
#       chaque non fini par son jeton (NA si NaN de mot bas 1954, règle de
#       R_IsNA ; NaN ; Inf ; -Inf).
#  Python compte aussi les égalités exactes (module decimal : développement
#  exact à 18 chiffres significatifs, le dernier 5) par tranche, et exige
#  leur nombre dans T1 (3), T2 (40 436 ; 0 parmi les voisins) et les
#  égalités tirées (2 × N_EGALITES ; 0 parmi les voisins) ; il vérifie la table des
#  puissances PUISSANCES_TRANCHES (lue en binaire) contre les entiers exacts
#  5^-e et 2^e. Les MD5 des textes de T2 et de T3 sont imprimés et comparés
#  aux constantes de la batterie.
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/controles/ecriture_doubles_texte.R [n]   (n = 1e6 par défaut)
#  Code de sortie 1 si un écart, si Python est absent ou si un MD5 diffère de
#  la batterie.
# -----------------------------------------------------------------------------

ARGS <- commandArgs(trailingOnly = TRUE)
N <- if (length(ARGS)) as.numeric(ARGS[1L]) else 1e6
GRAINE <- 20261019L
GRAINE_EGALITES <- 20261026L
N_EGALITES <- 2e5L
# Constantes de la batterie (tests/unitaires/test_construire_table.R) : leur
# présence dans le fichier de test est vérifiée ci-dessous.
MD5_T2 <- "984842c362108e4bae2a28ee0b3332bc"
MD5_T3 <- "eb6320f37c34702827a4b66fa6f0b5c4"

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
RACINE <- if (length(f)) normalizePath(file.path(dirname(f[1L]), "..", "..")) else getwd()
moteur <- new.env()
for (m in c("outils.R", "rapport.R")) sys.source(file.path(RACINE, "R", m), moteur)

python <- Sys.which("python3")
if (!nzchar(python)) {
  cat("ECHEC : python3 absent, le juge indépendant n'est pas disponible.\n")
  quit(status = 1L)
}
GLIBC <- .Platform$OS.type != "windows"

# --- construction des doubles -------------------------------------------------

de_hex <- function(h) readBin(as.raw(strtoi(substring(h, seq(15L, 1L, -2L), seq(16L, 2L, -2L)), 16L)),
                              "double", n = 1L, size = 8L, endian = "little")
puissance <- function(b, n) { p <- 1; for (i in seq_len(n)) p <- p * b; p }
DIX17 <- puissance(10, 17); DIX18 <- puissance(10, 18); DEUX53 <- puissance(2, 53); DEUX32 <- puissance(2, 32)

# Mots de 32 bits (hi, lo) des octets petit-boutistes, et retour.
mots <- function(v) {
  o <- matrix(as.integer(writeBin(v, raw(), size = 8L, endian = "little")), 8L)
  list(hi = ((o[8L, ] * 256 + o[7L, ]) * 256 + o[6L, ]) * 256 + o[5L, ],
       lo = ((o[4L, ] * 256 + o[3L, ]) * 256 + o[2L, ]) * 256 + o[1L, ])
}
de_mots <- function(hi, lo) {
  o <- matrix(as.raw(0), 8L, length(hi))
  for (k in 1:4) { o[k, ] <- as.raw(lo %% 256); lo <- lo %/% 256 }
  for (k in 5:8) { o[k, ] <- as.raw(hi %% 256); hi <- hi %/% 256 }
  readBin(as.vector(o), "double", n = length(hi), size = 8L, endian = "little")
}
# Voisin à k ulp (k = ±1) par les bits : lo + k, report sur hi.
voisin <- function(v, k) {
  w <- mots(v); lo <- w$lo + k; hi <- w$hi
  b <- lo < 0; lo[b] <- lo[b] + DEUX32; hi[b] <- hi[b] - 1
  b <- lo >= DEUX32; lo[b] <- lo[b] - DEUX32; hi[b] <- hi[b] + 1
  de_mots(hi, lo)
}
# Bornes des m impairs tels que m·5^k soit dans [10^17, 10^18), m < 2^53.
bornes_m <- function(k) {
  p5 <- puissance(5, k)
  lo <- ceiling(DIX17 / p5); hi <- min(floor(DIX18 / p5), DEUX53 - 1)
  while (lo * p5 < DIX17) lo <- lo + 1
  while (hi * p5 >= DIX18) hi <- hi - 1
  if (lo %% 2 == 0) lo <- lo + 1
  if (hi %% 2 == 0) hi <- hi - 1
  c(lo = lo, hi = hi)
}
deux_moins <- function(k) { d <- 1; for (i in seq_len(k)) d <- d / 2; d }

# T1 : cas limites de la batterie (octets IEEE 754), plus 3·2^-25.
T1 <- c(vapply(c("0000000000000001", "8000000000000001", "000fffffffffffff", "0010000000000000",
                 "7fefffffffffffff", "ffefffffffffffff", "4341c37937e07fff", "4341c37937e08000",
                 "4376345785d89fff", "4376345785d8a000", "4376345785d8a001", "3f1a36e2eb1c432d",
                 "3f1a36e2eb1c432c", "3ee4f8b588e368f1", "54b249ad2594c37d", "2b2bff2ee48e0530",
                 "4340000000000000", "4340000000000001", "43e0000000000000", "3ed422927add0ecb",
                 "3ed422927add0ecc", "3e60000000000000", "4310000000000001", "3d06849b86a12b9b",
                 "5fb317e5ef3ab327", "009c16c5c5253575", "0000000000000000", "8000000000000000"), de_hex, 0),
        3 * deux_moins(25))

# T2 : construction de la batterie (q3/temoins.R de l'avis d'actuary).
egalites_t2 <- unlist(lapply(2:25, function(k) {
  b <- bornes_m(k); lo <- b[["lo"]]; hi <- b[["hi"]]
  nb <- (hi - lo) / 2 + 1
  m <- if (nb <= 1000) seq(lo, hi, by = 2) else unique(c(lo + 2 * floor((0:999) * ((nb - 1) / 999)), lo + 2, hi - 2))
  m * deux_moins(k)
}))
T2 <- c(egalites_t2, voisin(egalites_t2, -1), voisin(egalites_t2, 1))
T2 <- c(T2, -T2)

# T3 et échantillon de n doubles : recette de la batterie.
echantillon <- function(n, graine) moteur$sous_graine(graine, {
  m <- n %/% 2L
  octets <- as.raw(sample.int(256L, 8L * m, replace = TRUE) - 1L)
  brut <- readBin(octets, "double", n = m, size = 8L, endian = "little")
  brut[!is.finite(brut)] <- 0.5
  u <- runif(n - m)
  c(brut, u[seq_len((n - m) %/% 2L)] * 1e6 - 5e5, 1 / u[-seq_len((n - m) %/% 2L)])
})
T3 <- echantillon(1e5L, GRAINE)
ECHANTILLON <- echantillon(as.integer(N), GRAINE)

# Égalités exactes tirées au hasard : k uniforme dans 2:25, m impair uniforme
# (à un biais de modulo près, sans effet ici) entre les bornes, par un entier
# de 48 bits tiré octet par octet ; voisins et opposés.
EGALITES <- moteur$sous_graine(GRAINE_EGALITES, {
  k <- sample(2:25, N_EGALITES, replace = TRUE)
  o <- matrix(sample.int(256L, 6L * N_EGALITES, replace = TRUE) - 1L, 6L)
  a <- o[1L, ]; for (j in 2:6) a <- a * 256 + o[j, ]
  b <- t(vapply(2:25, bornes_m, c(lo = 0, hi = 0)))[k - 1L, , drop = FALSE]
  nb <- (b[, "hi"] - b[, "lo"]) / 2 + 1
  m <- b[, "lo"] + 2 * (a %% nb)
  d <- vapply(2:25, deux_moins, 0)[k - 1L]
  m * d
})
EGALITES <- c(EGALITES, voisin(EGALITES, -1), voisin(EGALITES, 1))
EGALITES <- c(EGALITES, -EGALITES)

# Non finis : jetons usuels, -NaN, et NaN à charge utile tirée (signe et
# fraction au hasard, fraction non nulle), dont des NA (mot bas 1954).
NON_FINIS <- c(NA_real_, NA_real_ + 1, NaN, de_hex("fff8000000000000"), Inf, -Inf, 0 / 0,
               moteur$sous_graine(GRAINE, {
                 n <- 2000L
                 signe <- sample(0:1, n, replace = TRUE)
                 hi <- 2146435072 + signe * 2147483648 + (sample.int(1048576L, n, replace = TRUE) - 1L)
                 lo <- sample.int(65536L, n, replace = TRUE) * 65536 - 1
                 lo[seq(1L, n, by = 4L)] <- 1954  # lo jamais nul : fraction non nulle, NaN
                 de_mots(hi, lo)
               }))

parts <- list(T1 = T1, T2 = T2, T3 = T3, echantillon = ECHANTILLON, egalites = EGALITES, non_finis = NON_FINIS)
x <- unlist(parts, use.names = FALSE)
cat(sprintf("%d doubles : %s\n", length(x), paste(sprintf("%s %d", names(parts), lengths(parts)), collapse = ", ")))
cat(sprintf("Non finis : %d (dont NA %d, NaN %d, infinis %d)\n", sum(!is.finite(x)), sum(is.na(x) & !is.nan(x)),
            sum(is.nan(x)), sum(is.infinite(x))))

# --- écriture par tablexp -----------------------------------------------------

t_r <- system.time(obtenu <- moteur$ecrire_valeurs(x, "texte", "x")$octets)[["elapsed"]]
lignes_r <- strsplit(rawToChar(obtenu), "\n", fixed = TRUE)[[1L]]
cat(sprintf("Écriture tablexp : %.2f s (%.2f µs par double)\n", t_r, 1e6 * t_r / length(x)))

dossier <- tempfile("ecriture_texte_"); dir.create(dossier)
sortir <- function(statut) {
  unlink(dossier, recursive = TRUE)
  quit(status = statut)
}
ecarts <- 0L

# --- MD5 de T2 et de T3, comparés à la batterie -----------------------------------

md5_octets <- function(o) { f <- tempfile(tmpdir = dossier); writeBin(o, f); unname(tools::md5sum(f)) }
fin <- cumsum(lengths(parts)); debut <- fin - lengths(parts) + 1L
texte_part <- function(nom) {
  l <- lignes_r[debut[[nom]]:fin[[nom]]]
  charToRaw(paste0(l, "\n", collapse = ""))
}
fichier_test <- file.path(RACINE, "tests", "unitaires", "test_construire_table.R")
source_test <- paste(readLines(fichier_test, encoding = "UTF-8", warn = FALSE), collapse = "\n")
for (cas in list(c("T2", MD5_T2), c("T3", MD5_T3))) {
  m <- md5_octets(texte_part(cas[1L]))
  dans_test <- grepl(sprintf("identical(m, \"%s\")", cas[2L]), source_test, fixed = TRUE)
  cat(sprintf("MD5 du texte de %s : %s ; constante de la batterie %s ; %s ; présente dans %s : %s\n", cas[1L], m,
              cas[2L], if (identical(m, cas[2L])) "égal" else "DIFFÉRENT", basename(fichier_test), dans_test))
  if (!identical(m, cas[2L]) || !dans_test) ecarts <- ecarts + 1L
}
n_t2 <- length(T2)
cat(sprintf("T2 : %d doubles (attendu 121308)\n", n_t2))
if (n_t2 != 121308L) ecarts <- ecarts + 1L

# --- juge 1 : glibc -------------------------------------------------------------

if (GLIBC) {
  fini <- is.finite(x)
  g <- character(length(x))
  g[fini] <- sprintf("%.17g", x[fini] + 0)
  g[!fini] <- sprintf("%.17g", x[!fini])
  d <- which(g != lignes_r)
  cat(sprintf("Juge glibc (%s) : %d ligne(s), %d écart(s)\n",
              paste(grep("glibc", extSoftVersion(), value = TRUE, ignore.case = TRUE), collapse = " "),
              length(g), length(d) + abs(length(g) - length(lignes_r))))
  for (i in head(d, 10L)) cat(sprintf("  ligne %d : tablexp %s, glibc %s\n", i, lignes_r[i], g[i]))
  if (length(d) || length(g) != length(lignes_r)) ecarts <- ecarts + 1L
} else {
  cat("Juge glibc : sauté sous Windows (bibliothèque C non correctement arrondie, #104).\n")
}

# --- juge 2 : Python ------------------------------------------------------------

f_x <- file.path(dossier, "x.bin"); f_r <- file.path(dossier, "r.txt")
f_tp <- file.path(dossier, "table.bin"); f_lg <- file.path(dossier, "longueur.bin")
script <- file.path(dossier, "juge.py")
writeBin(x, f_x, size = 8L, endian = "little")
writeBin(obtenu, f_r)
tp <- moteur$PUISSANCES_TRANCHES
writeBin(as.integer(t(tp$tranches)), f_tp, size = 4L, endian = "little")
writeBin(as.integer(tp$longueur), f_lg, size = 4L, endian = "little")
# Tranches de x où Python compte les égalités exactes, avec le décompte exigé
# (une ligne « nom début fin attendu », positions de 1 à n incluses) :
#  - T1 : 3 (2^-25, 3·2^-25 et 2^50 + 0,25) ;
#  - T2 : les 20 218 égalités énumérées et leurs opposés, soit 40 436, puis 0
#    parmi les voisins à ±1 ulp et leurs opposés ;
#  - égalités tirées : 2 × N_EGALITES (elles et leurs opposés), 0 parmi les
#    voisins.
# Disposition de T2 et des égalités tirées : c(e, voisins -1, voisins +1),
# puis les opposés du tout.
f_tr <- file.path(dossier, "tranches.txt")
tranches_egalites <- function(nom, n_e) {
  d <- debut[[nom]] - 1L
  data.frame(nom = paste(nom, c("égalités", "voisins", "égalités opposées", "voisins opposés")),
             debut = d + c(1L, n_e + 1L, 3L * n_e + 1L, 4L * n_e + 1L),
             fin = d + c(n_e, 3L * n_e, 4L * n_e, 6L * n_e),
             attendu = c(n_e, 0L, n_e, 0L))
}
TRANCHES <- rbind(data.frame(nom = "T1", debut = debut[["T1"]], fin = fin[["T1"]], attendu = 3L),
                  tranches_egalites("T2", length(egalites_t2)),
                  tranches_egalites("egalites", N_EGALITES))
stopifnot(length(egalites_t2) == 20218L, length(EGALITES) == 6L * N_EGALITES)
writeLines(enc2utf8(sprintf("%s\t%d\t%d\t%d", TRANCHES$nom, TRANCHES$debut, TRANCHES$fin, TRANCHES$attendu)), f_tr,
           useBytes = TRUE)
writeLines(c(
  "import struct, sys, hashlib",
  "from decimal import Decimal",
  "f_x, f_r, f_tp, f_lg, largeur, f_tr = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4], int(sys.argv[5]), sys.argv[6]",
  "b = open(f_x, 'rb').read()",
  "n = len(b) // 8",
  "xs = struct.unpack('<%dd' % n, b)",
  "mots_bas = struct.unpack('<%dI' % (2 * n), b)[0::2]",
  "def jeton(x, bas):",
  "    if x != x:",
  "        return 'NA' if bas == 1954 else 'NaN'",
  "    if x in (float('inf'), float('-inf')):",
  "        return 'Inf' if x > 0 else '-Inf'",
  "    return format(x + 0.0, '.17g')",
  "lignes = [jeton(x, bas) for x, bas in zip(xs, mots_bas)]",
  "r = open(f_r, 'rb').read().decode('ascii').split('\\n')",
  "if r and r[-1] == '':",
  "    r.pop()",
  "ecarts = [i for i in range(min(len(r), len(lignes))) if r[i] != lignes[i]]",
  "print('Juge Python %s : %d ligne(s), %d écart(s)' % (sys.version.split()[0], len(lignes), len(ecarts) + abs(len(r) - len(lignes))))",
  "for i in ecarts[:10]:",
  "    print('  ligne %d : tablexp %s, Python %s' % (i + 1, r[i], lignes[i]))",
  "def egalite(x):",
  "    if x != x or x in (float('inf'), float('-inf')) or x == 0:",
  "        return False",
  "    t = abs(Decimal(x)).normalize().as_tuple().digits",
  "    return len(t) == 18 and t[-1] == 5",
  "eg = [egalite(x) for x in xs]",
  "print('Égalités exactes (développement exact à 18 chiffres, le dernier 5), total pour information : %d' % sum(eg))",
  "faux_eg = 0",
  "for l in open(f_tr, encoding='utf-8').read().splitlines():",
  "    nom, d, f, att = l.split('\\t')",
  "    c = sum(eg[int(d) - 1:int(f)])",
  "    ok = c == int(att)",
  "    faux_eg += not ok",
  "    print('  %s (lignes %s à %s) : %d égalité(s), attendu %s%s' % (nom, d, f, c, att, '' if ok else ' : ÉCART'))",
  "lg = struct.unpack('<%di' % (len(open(f_lg, 'rb').read()) // 4), open(f_lg, 'rb').read())",
  "tb = open(f_tp, 'rb').read()",
  "tr = struct.unpack('<%di' % (len(tb) // 4), tb)",
  "faux = 0",
  "for k in range(len(lg)):",
  "    e = k - 1074",
  "    v = sum(tr[k * largeur + j] * 10 ** (7 * j) for j in range(lg[k]))",
  "    if v != (5 ** (-e) if e < 0 else 2 ** e) or tr[k * largeur + lg[k] - 1] == 0 or any(tr[k * largeur + j] for j in range(lg[k], largeur)):",
  "        faux += 1",
  "print('Table des puissances : %d ligne(s), %d écart(s) aux entiers exacts' % (len(lg), faux))",
  "sys.exit(1 if ecarts or len(r) != len(lignes) or faux or faux_eg or len(lg) != 2098 else 0)"),
  script, useBytes = TRUE)
t_juge <- system.time(statut <- system2(python, c("-I", script, f_x, f_r, f_tp, f_lg, ncol(tp$tranches), f_tr)))[["elapsed"]]
cat(sprintf("Juge Python : %.2f s, code %d\n", t_juge, statut))
if (statut != 0L) ecarts <- ecarts + 1L

if (ecarts) {
  cat(sprintf("ECHEC : %d contrôle(s) en écart.\n", ecarts))
  sortir(1L)
}
cat("OK\n")
sortir(0L)
