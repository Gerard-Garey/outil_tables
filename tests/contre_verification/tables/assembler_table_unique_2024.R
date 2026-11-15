# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/assembler_table_unique_2024.R : assemblage
#  des transcriptions par blocs de la table unique 2024 (JO n° 276 du
#  22/11/2024, texte 17, annexe à l'article A. 132-18-1 du code des
#  assurances) en grille au format 2 (issue #275 ; ADR 0019, points 5.2 et
#  7.2 ; E2.26 ; D10)
#
#  Hors CI : lancé à la main, après le gel des transcriptions (gel.sha256).
#  La CI compare le fichier de table et les relevés versés
#  (tests/controles/tables_embarquees.R), sans les blocs.
#
#  Sur le modèle d'assembler_tgf05_tgh05.R (#274), pour une seule table, sans
#  écart déclaré : aucune cellule VIDE n'est admise.
#
#  Emplacement des blocs. Les blocs des deux transcripteurs, leur gel et leurs
#  notes ont été versés au commit 272f34e sous
#  tests/contre_verification/transcriptions_b23/, dossier provisoire retiré
#  par le commit donnees: qui embarque la table (étape (6) de B23) ; ils
#  restent lisibles dans l'historique à 272f34e. Le relevé
#  table_unique_2024.transcription_b.csv a les octets de la sortie de ce
#  script (table_unique_2024.csv) ; il n'en est pas l'entrée : le rejeu
#  demande le gel et les blocs de A et de B. Le script ne
#  lit aucun chemin en dur : le dossier des transcriptions (celui qui porte
#  gel.sha256) est son premier argument, le dossier de sortie le second. Les
#  blocs lus sont ceux que nomme le gel
#  (`transcription_<A|B>/blocs/unique2024_<g1>-<g2>.csv`). Rejeu, depuis la
#  racine du dépôt :
#    d=$(mktemp -d)
#    git archive 272f34e tests/contre_verification/transcriptions_b23 | tar -x -C "$d"
#    Rscript tests/contre_verification/tables/assembler_table_unique_2024.R \
#      "$d/tests/contre_verification/transcriptions_b23" "$d"
#  R base seul (R 4.3.3), aucun outil externe. Déterministe : aucune horloge,
#  aucun aléa ; l'ordre des blocs se déduit de leurs générations (dix blocs
#  par transcription, triés octet à octet).
#
#  Contrôles, dans l'ordre :
#    1. gel : chaque fichier nommé par gel.sha256 a le SHA-256 consigné
#       (équivalent de `sha256sum -c gel.sha256`, calculé en R base) ;
#    2. A = B : les dix blocs des transcriptions A et B sont identiques octet
#       pour octet ; la grille est assemblée sur la transcription B ;
#    3. blocs : ASCII, LF, fin de ligne finale ; en-tête `age,<g1>,…,<g2>` ;
#       colonne des âges 0 à 121, identique d'un bloc à l'autre ; nombre de
#       cellules par ligne ; cellule `0`, chiffres sans zéro initial ou `-` ;
#       aucune cellule VIDE ;
#    4. ligne 121 : ne porte que des `-` ; c'est la seule ligne sans valeur ;
#       elle n'est pas transcrite (ADR 0019, point 5.2) ;
#    5. format 2 sur la grille assemblée : générations 1900 à 2005 et âges
#       consécutifs, aucune ligne ni colonne sans valeur, colonnes
#       décroissantes au sens large, aucun `-` entre deux valeurs d'une
#       colonne.
#  Écrit <sortie>/table_unique_2024.csv (ASCII, LF, fin de ligne finale,
#  en-tête `age,1900,…,2005`) et son SHA-256 sur la sortie standard.
#  Échec : « ECHEC : … » sur la sortie d'erreur, code 1.
#
#  Usage : Rscript tests/contre_verification/tables/assembler_table_unique_2024.R
#            <dossier des transcriptions> <dossier de sortie>
# -----------------------------------------------------------------------------

local({

TABLE <- "table_unique_2024"
PREFIXE <- "unique2024"
GENS <- 1900:2005
AGES <- 0:121
AGE_RETIRE <- 121L
CELLULE <- "^(0|[1-9][0-9]{0,14}|-)$"
BLOC <- paste0("^transcription_([AB])/blocs/(", PREFIXE, "_[0-9]{4}-[0-9]{4}\\.csv)$")

# Sorties en octets UTF-8, quelle que soit la locale (POSIX comprise).
ecrire <- function(x, con = stdout()) writeLines(enc2utf8(x), con, useBytes = TRUE)
echec <- function(msg) {
  ecrire(paste0("ECHEC : ", msg), stderr())
  quit(save = "no", status = 1L)
}

# --- SHA-256 (FIPS 180-4) en R base ------------------------------------------
# Mots de 32 bits en double (exacts) ; opérations bit à bit sur deux moitiés de
# 16 bits (bitwAnd et bitwXor travaillent sur des entiers signés).
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


lire_octets <- function(chemin) {
  if (!file.exists(chemin)) echec(sprintf("%s : fichier absent", chemin))
  readBin(chemin, "raw", file.size(chemin))
}

# --- Gel -----------------------------------------------------------------------
# Lignes `<empreinte>  <chemin relatif>` ; rend les chemins, après contrôle.
verifier_gel <- function(dossier) {
  lignes <- strsplit(rawToChar(lire_octets(file.path(dossier, "gel.sha256"))), "\n", fixed = TRUE)[[1L]]
  m <- regmatches(lignes, regexec("^([0-9a-f]{64})  (.+)$", lignes))
  if (any(lengths(m) != 3L)) echec("gel.sha256 : ligne mal form\u00e9e")
  attendu <- vapply(m, `[`, "", 2L); chemins <- vapply(m, `[`, "", 3L)
  for (i in seq_along(chemins)) {
    lu <- sha256(lire_octets(file.path(dossier, chemins[i])))
    if (lu != attendu[i]) echec(sprintf("gel : %s, SHA-256 %s au lieu de %s", chemins[i], lu, attendu[i]))
  }
  ecrire(sprintf("gel : %d fichiers conformes \u00e0 gel.sha256", length(chemins)))
  chemins
}

# --- Lecture d'un bloc ---------------------------------------------------------
# Octets ASCII, fin de ligne LF, dernière ligne terminée ; cellules séparées par
# des virgules, cellules vides conservées (y compris en fin de ligne).
lire_bloc <- function(nom, brut) {
  if (!length(brut) || brut[length(brut)] != as.raw(0x0a) || any(brut == as.raw(0x0d)))
    echec(sprintf("%s : fin de ligne", nom))
  if (any(brut > as.raw(0x7f))) echec(sprintf("%s : octet non ASCII", nom))
  lignes <- strsplit(rawToChar(brut), "\n", fixed = TRUE)[[1L]]
  lapply(lignes, function(l) strsplit(paste0(l, ","), ",", fixed = TRUE)[[1L]])
}

assembler <- function(dossier, chemins) {
  m <- regmatches(chemins, regexec(BLOC, chemins))
  m <- m[lengths(m) == 3L]
  qui <- vapply(m, `[`, "", 2L); nom <- vapply(m, `[`, "", 3L)
  noms <- sort(nom[qui == "B"], method = "radix")
  if (length(noms) != 10L) echec(sprintf("transcription B : %d blocs au gel au lieu de 10", length(noms)))
  if (!identical(sort(nom[qui == "A"], method = "radix"), noms))
    echec("transcriptions A et B : blocs diff\u00e9rents au gel")
  generations <- integer(0)
  colonnes <- rep(list(character(0)), length(AGES))
  ages_ref <- NULL
  for (n in noms) {
    a <- lire_octets(file.path(dossier, "transcription_A", "blocs", n))
    b <- lire_octets(file.path(dossier, "transcription_B", "blocs", n))
    if (!identical(a, b)) echec(sprintf("%s : transcriptions A et B diff\u00e9rentes", n))
    g <- as.integer(strsplit(substr(n, nchar(PREFIXE) + 2L, nchar(n) - 4L), "-", fixed = TRUE)[[1L]])
    g1 <- g[1L]; g2 <- g[2L]
    if (g2 < g1) echec(sprintf("%s : g\u00e9n\u00e9rations", n))
    lignes <- lire_bloc(n, b)
    if (!identical(lignes[[1L]], c("age", as.character(g1:g2))))
      echec(sprintf("%s : en-t\u00eate %s", n, paste(lignes[[1L]], collapse = ",")))
    ages <- vapply(lignes[-1L], `[`, "", 1L)
    if (is.null(ages_ref)) ages_ref <- ages
    if (!identical(ages, ages_ref)) echec(sprintf("%s : colonne des \u00e2ges diff\u00e9rente du premier bloc", n))
    if (!identical(ages, as.character(AGES))) echec(sprintf("%s : \u00e2ges", n))
    for (l in lignes[-1L]) {
      age <- as.integer(l[1L])
      cell <- l[-1L]
      if (length(cell) != g2 - g1 + 1L)
        echec(sprintf("%s : \u00e2ge %d, %d cellules au lieu de %d", n, age, length(cell), g2 - g1 + 1L))
      ko <- which(!grepl(CELLULE, cell))
      if (length(ko))
        echec(sprintf("%s : cellule invalide '%s', \u00e2ge %d, g\u00e9n\u00e9ration %d", n, cell[ko[1L]], age, g1 + ko[1L] - 1L))
      colonnes[[age + 1L]] <- c(colonnes[[age + 1L]], cell)
    }
    generations <- c(generations, g1:g2)
  }
  ecrire(sprintf("A = B : %d blocs identiques octet pour octet", length(noms)))
  list(generations = generations, colonnes = colonnes)
}

# `grille` : liste de list(age, cell).
controler <- function(generations, grille) {
  ages <- vapply(grille, `[[`, 0L, "age")
  if (!identical(generations, GENS)) echec(sprintf("%s : g\u00e9n\u00e9rations autres que 1900 \u00e0 2005", TABLE))
  if (!identical(ages, seq.int(ages[1L], length.out = length(ages))))
    echec(sprintf("%s : \u00e2ges non cons\u00e9cutifs", TABLE))
  for (r in grille) {
    if (length(r$cell) != length(generations)) echec(sprintf("%s : \u00e2ge %d, longueur", TABLE, r$age))
    if (all(r$cell == "-")) echec(sprintf("%s : \u00e2ge %d sans valeur", TABLE, r$age))
  }
  for (j in seq_along(generations)) {
    col <- vapply(grille, function(r) r$cell[j], "")
    a_val <- ages[col != "-"]
    val <- as.double(col[col != "-"])
    if (!length(val)) echec(sprintf("%s : g\u00e9n\u00e9ration %d sans valeur", TABLE, generations[j]))
    k <- which(diff(val) > 0)
    if (length(k)) {
      k <- k[1L]
      echec(sprintf("%s : g\u00e9n\u00e9ration %d, hausse entre %d (%s) et %d (%s)", TABLE, generations[j],
                    a_val[k], col[col != "-"][k], a_val[k + 1L], col[col != "-"][k + 1L]))
    }
    trous <- ages[ages > a_val[1L] & ages < a_val[length(a_val)] & col == "-"]
    if (length(trous))
      echec(sprintf("%s : g\u00e9n\u00e9ration %d, '-' intercal\u00e9 aux \u00e2ges %s", TABLE, generations[j],
                    paste(trous, collapse = ", ")))
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) echec("usage : assembler_table_unique_2024.R <dossier des transcriptions> <dossier de sortie>")
entree <- args[1L]; sortie <- args[2L]
r <- assembler(entree, verifier_gel(entree))
vides <- vapply(r$colonnes, function(x) all(x == "-"), NA)
if (!identical(AGES[vides], AGE_RETIRE))
  echec(sprintf("%s : lignes sans valeur aux \u00e2ges [%s] ; attendu : la seule ligne %d, enti\u00e8rement en '-'",
                TABLE, paste(AGES[vides], collapse = ", "), AGE_RETIRE))
ecrire(sprintf("%s : ligne %d enti\u00e8rement en '-' (%d cellules), non transcrite", TABLE, AGE_RETIRE,
               length(r$colonnes[[AGE_RETIRE + 1L]])))
grille <- lapply(which(!vides), function(i) list(age = AGES[i], cell = r$colonnes[[i]]))
controler(r$generations, grille)
texte <- paste0("age,", paste(r$generations, collapse = ","), "\n",
                paste0(vapply(grille, function(x) paste0(x$age, ",", paste(x$cell, collapse = ","), "\n"), ""),
                       collapse = ""))
donnees <- charToRaw(texte)
con <- file(file.path(sortie, paste0(TABLE, ".csv")), "wb")
writeBin(donnees, con)
close(con)
toutes <- unlist(lapply(grille, `[[`, "cell"))
ecrire(sprintf("%s : %d g\u00e9n\u00e9rations (%d-%d), %d \u00e2ges (%d-%d), %d cellules \u00e0 valeur, %d cellules '-'",
               TABLE, length(r$generations), r$generations[1L], r$generations[length(r$generations)],
               length(grille), grille[[1L]]$age, grille[[length(grille)]]$age,
               sum(toutes != "-"), sum(toutes == "-")))
ecrire(sprintf("%s  %s.csv", sha256(donnees), TABLE))

})
