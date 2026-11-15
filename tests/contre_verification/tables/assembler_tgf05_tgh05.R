# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/assembler_tgf05_tgh05.R : assemblage des
#  transcriptions par blocs de TGF05 et TGH05 (JO n° 301 du 29/12/2015,
#  texte 35) en grilles au format 2 (issue #274 ; ADR 0019, points 5.2 et
#  7.2 ; D10)
#
#  Hors CI : lancé à la main, après le gel des transcriptions (gel.sha256).
#  La CI compare le fichier de table et les relevés versés
#  (tests/controles/tables_embarquees.R), sans les blocs.
#
#  Emplacement des blocs. Les blocs de pages des deux transcripteurs (blocs_a/
#  et blocs_b/), leur gel (gel.sha256) et leurs notes ont été versés au commit
#  b949ced sous tests/contre_verification/transcriptions_b22/generation/,
#  dossier provisoire à retirer au dernier commit donnees: de #274 (M474) ;
#  ils resteront lisibles dans l'historique à ce commit. Le script ne lit aucun chemin en dur : le
#  dossier des blocs est son premier argument. Rejeu, depuis la racine du
#  dépôt, contre les pièces de b949ced (le dossier provisoire, tant qu'il
#  existe, peut être donné directement) :
#    d=$(mktemp -d)
#    git archive b949ced tests/contre_verification/transcriptions_b22/generation | tar -x -C "$d"
#    g="$d/tests/contre_verification/transcriptions_b22/generation"
#    (cd "$g" && sha256sum -c gel.sha256)
#    Rscript tests/contre_verification/tables/assembler_tgf05_tgh05.R "$g/blocs_a" "$d"
#  puis la même commande sur "$g/blocs_b" (transcription B), dans un autre
#  dossier de sortie. Attendu : tgf05.csv égal à inst/extdata/tables/tgf05.csv
#  (SHA-256 écrit sur la sortie standard et au fichier de provenance).
#  R base seul (R 4.3.3), aucun outil externe. Déterministe : aucune horloge,
#  aucun aléa ; l'ordre des blocs se déduit de leurs générations (noms
#  `<table>_<g1>-<g2>.csv`, dix blocs par table, triés octet à octet).
#  Réécriture en R d'assembler.py (commit b949ced), à sortie identique
#  (décision du mainteneur du 14/11/2026, M478) : mêmes fichiers octet pour octet, mêmes lignes sur la
#  sortie standard, mêmes motifs d'échec (« ECHEC : … » sur la sortie
#  d'erreur, code 1).
#
#  Décisions du mainteneur appliquées (M466) :
#    A1 : TGF05, cellule (120, 1921) imprimée vide (VIDE) -> écrite `-` ;
#    A2 : TGF05, bloc 1955-1965, ligne 117 : douze cellules transcrites, les
#         onze nombres imprimés sont attribués dans l'ordre imprimé aux onze
#         générations 1955 à 1965, chiffres inchangés ; le VIDE final
#         disparaît.
#  Tout autre VIDE, toute autre ligne de longueur anormale, l'absence de A1
#  ou de A2 dans TGF05 ou leur présence dans TGH05 est un échec. Contrôles du
#  format 2 sur la grille assemblée : générations et âges consécutifs, cellule
#  `0`, chiffres sans zéro initial ou `-`, aucune ligne ni colonne sans
#  valeur, colonnes décroissantes au sens large, aucun `-` entre deux valeurs
#  d'une colonne. Les lignes entièrement en `-` ne sont pas transcrites
#  (ADR 0019, point 5.2 ; E2.26) ; elles sont listées sur la sortie standard,
#  avec la position de celles qui tomberaient au milieu de la grille.
#  Écrit <sortie>/tgf05.csv et <sortie>/tgh05.csv, et le SHA-256 de chacun
#  (calculé en R base ci-dessous, sans outil externe) sur la sortie standard.
#
#  Usage : Rscript tests/contre_verification/tables/assembler_tgf05_tgh05.R
#            <dossier_des_blocs> <dossier_de_sortie>
# -----------------------------------------------------------------------------

local({

TABLES <- c("tgf05", "tgh05")
AGES <- 0:120
CELLULE <- "^(0|[1-9][0-9]{0,14}|-)$"
# Écarts déclarés : table, première génération du bloc, âge
A1 <- list(table = "tgf05", g1 = 1911L, age = 120L)
A2 <- list(table = "tgf05", g1 = 1955L, age = 117L)

# Sorties en octets UTF-8, quelle que soit la locale (POSIX comprise).
ecrire <- function(x, con = stdout()) writeLines(enc2utf8(x), con, useBytes = TRUE)
echec <- function(msg) {
  ecrire(paste0("ECHEC : ", msg), stderr())
  quit(save = "no", status = 1L)
}

# Représentations reprises de Python, pour des messages identiques :
# repr() d'une chaîne, et d'une liste d'entiers ou de chaînes.
repr_chaine <- function(s) {
  q <- if (grepl("'", s, fixed = TRUE) && !grepl("\"", s, fixed = TRUE)) "\"" else "'"
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub(q, paste0("\\", q), s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  paste0(q, s, q)
}
repr_liste <- function(x) {
  if (is.character(x)) x <- vapply(x, repr_chaine, "", USE.NAMES = FALSE)
  paste0("[", paste(x, collapse = ", "), "]")
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

# --- Lecture d'un bloc ---------------------------------------------------------
# Octets ASCII, fin de ligne LF, dernière ligne terminée ; cellules séparées par
# des virgules, cellules vides conservées (y compris en fin de ligne).
lire_bloc <- function(chemin) {
  brut <- readBin(chemin, "raw", file.size(chemin))
  if (!length(brut) || brut[length(brut)] != as.raw(0x0a) || any(brut == as.raw(0x0d)))
    echec(sprintf("%s : fin de ligne", chemin))
  if (any(brut > as.raw(0x7f))) echec(sprintf("%s : octet non ASCII", chemin))
  lignes <- strsplit(rawToChar(brut), "\n", fixed = FALSE)[[1L]]
  lapply(lignes, function(l) strsplit(paste0(l, ","), ",", fixed = TRUE)[[1L]])
}

assembler <- function(dossier, table) {
  fichiers <- list.files(dossier)
  fichiers <- sort(fichiers[grepl(paste0("^", table, "_[0-9]{4}-[0-9]{4}\\.csv$"), fichiers)], method = "radix")
  if (length(fichiers) != 10L) echec(sprintf("%s : %d blocs au lieu de 10", table, length(fichiers)))
  generations <- integer(0)
  colonnes <- rep(list(character(0)), length(AGES))
  journal <- character(0)
  for (nom in fichiers) {
    g <- as.integer(strsplit(substr(nom, nchar(table) + 2L, nchar(nom) - 4L), "-", fixed = TRUE)[[1L]])
    g1 <- g[1L]; g2 <- g[2L]
    lignes <- lire_bloc(file.path(dossier, nom))
    entete <- lignes[[1L]]
    attendu <- c("age", if (g2 >= g1) as.character(g1:g2))
    if (!identical(entete, attendu)) echec(sprintf("%s : en-t\u00eate %s", nom, repr_liste(entete)))
    n <- g2 - g1 + 1L
    if (!identical(vapply(lignes[-1L], `[`, "", 1L), as.character(AGES))) echec(sprintf("%s : \u00e2ges", nom))
    for (l in lignes[-1L]) {
      age <- as.integer(l[1L])
      cell <- l[-1L]
      if (table == A2$table && g1 == A2$g1 && age == A2$age) {
        if (length(cell) != n + 1L || cell[length(cell)] != "VIDE" || "VIDE" %in% cell[-length(cell)])
          echec(sprintf("%s : ligne %d (A2) n'a pas la forme attendue", nom, age))
        cell <- cell[-length(cell)]
        journal <- c(journal, sprintf(paste0("A2 : %s \u00e2ge %d, %d nombres attribu\u00e9s dans l'ordre imprim\u00e9 ",
                                             "aux g\u00e9n\u00e9rations %d \u00e0 %d ; VIDE final retir\u00e9"),
                                      table, age, n, g1, g2))
      } else if (length(cell) != n) {
        echec(sprintf("%s : \u00e2ge %d, %d cellules au lieu de %d", nom, age, length(cell), n))
      }
      for (i in seq_along(cell)) {
        gen <- g1 + i - 1L
        if (cell[i] == "VIDE") {
          if (table == A1$table && g1 == A1$g1 && age == A1$age && gen == 1921L) {
            cell[i] <- "-"
            journal <- c(journal, sprintf("A1 : %s cellule (%d, %d) VIDE \u00e9crite -", table, age, gen))
          } else {
            echec(sprintf("%s : VIDE non d\u00e9clar\u00e9, \u00e2ge %d, g\u00e9n\u00e9ration %d", nom, age, gen))
          }
        }
        if (!grepl(CELLULE, cell[i]))
          echec(sprintf("%s : cellule invalide %s, \u00e2ge %d, g\u00e9n\u00e9ration %d", nom, repr_chaine(cell[i]), age, gen))
      }
      colonnes[[age + 1L]] <- c(colonnes[[age + 1L]], cell)
    }
    if (g2 >= g1) generations <- c(generations, g1:g2)
  }
  # Contrôle que A1 et A2 ont bien été rencontrés pour TGF05
  if (table == "tgf05" && length(journal) != 2L)
    echec(sprintf("%s : \u00e9carts A1/A2 non rencontr\u00e9s (%s)", table, repr_liste(journal)))
  if (table == "tgh05" && length(journal))
    echec(sprintf("%s : \u00e9cart inattendu %s", table, repr_liste(journal)))
  list(generations = generations, colonnes = colonnes, journal = journal)
}

# `grille` : liste de list(age, cell).
controler <- function(table, generations, grille) {
  ages <- vapply(grille, `[[`, 0L, "age")
  if (!identical(generations, seq.int(generations[1L], length.out = length(generations))))
    echec(sprintf("%s : g\u00e9n\u00e9rations non cons\u00e9cutives", table))
  if (!identical(ages, seq.int(ages[1L], length.out = length(ages))))
    echec(sprintf("%s : \u00e2ges non cons\u00e9cutifs apr\u00e8s retrait des lignes sans valeur (%d \u00e0 %d, %d lignes)",
                  table, ages[1L], ages[length(ages)], length(ages)))
  for (r in grille) {
    if (length(r$cell) != length(generations)) echec(sprintf("%s : \u00e2ge %d, longueur", table, r$age))
    if (all(r$cell == "-")) echec(sprintf("%s : \u00e2ge %d sans valeur", table, r$age))
    for (c in r$cell)
      if (!grepl(CELLULE, c)) echec(sprintf("%s : \u00e2ge %d, cellule %s", table, r$age, repr_chaine(c)))
  }
  for (j in seq_along(generations)) {
    col <- vapply(grille, function(r) r$cell[j], "")
    a_val <- ages[col != "-"]
    val <- as.double(col[col != "-"])
    if (!length(val)) echec(sprintf("%s : g\u00e9n\u00e9ration %d sans valeur", table, generations[j]))
    k <- which(diff(val) > 0)
    if (length(k)) {
      k <- k[1L]
      echec(sprintf("%s : g\u00e9n\u00e9ration %d, hausse entre %d (%s) et %d (%s)", table, generations[j],
                    a_val[k], col[col != "-"][k], a_val[k + 1L], col[col != "-"][k + 1L]))
    }
    trous <- ages[ages > a_val[1L] & ages < a_val[length(a_val)] & col == "-"]
    if (length(trous))
      echec(sprintf("%s : g\u00e9n\u00e9ration %d, '-' intercal\u00e9 aux \u00e2ges %s", table, generations[j], repr_liste(trous)))
  }
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2L) echec("usage : assembler_tgf05_tgh05.R <dossier_des_blocs> <dossier_de_sortie>")
entree <- args[1L]; sortie <- args[2L]
for (table in TABLES) {
  r <- assembler(entree, table)
  generations <- r$generations
  for (j in r$journal) ecrire(j)
  vides <- vapply(r$colonnes, function(x) all(x == "-"), NA)
  retirees <- AGES[vides]
  grille <- lapply(which(!vides), function(i) list(age = AGES[i], cell = r$colonnes[[i]]))
  # Position des lignes retirées : en tête, en queue, ou au milieu
  gardes <- AGES[!vides]
  milieu <- if (length(gardes)) retirees[retirees > gardes[1L] & retirees < gardes[length(gardes)]] else integer(0)
  ecrire(paste0(sprintf("%s : lignes sans valeur non transcrites, \u00e2ges %s", table, repr_liste(retirees)),
                if (length(milieu)) sprintf(" ; AU MILIEU de la grille : %s", repr_liste(milieu)) else " ; aucune au milieu"))
  controler(table, generations, grille)
  texte <- paste0("age,", paste(generations, collapse = ","), "\n",
                  paste0(vapply(grille, function(x) paste0(x$age, ",", paste(x$cell, collapse = ","), "\n"), ""),
                         collapse = ""))
  donnees <- charToRaw(texte)
  con <- file(file.path(sortie, paste0(table, ".csv")), "wb")
  writeBin(donnees, con)
  close(con)
  toutes <- unlist(lapply(grille, `[[`, "cell"))
  ecrire(sprintf("%s : %d g\u00e9n\u00e9rations (%d-%d), %d \u00e2ges (%d-%d), %d cellules \u00e0 valeur, %d cellules '-'",
                 table, length(generations), generations[1L], generations[length(generations)],
                 length(grille), grille[[1L]]$age, grille[[length(grille)]]$age,
                 sum(toutes != "-"), sum(toutes == "-")))
  ecrire(sprintf("%s  %s.csv", sha256(donnees), table))
}

})
