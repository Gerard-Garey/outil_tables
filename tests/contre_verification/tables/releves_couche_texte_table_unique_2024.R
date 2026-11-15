# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/releves_couche_texte_table_unique_2024.R :
#  relevé de la table unique 2024 par la couche texte du JO n° 276 du
#  22/11/2024, texte 17, pour la seule comparaison (contrôle de lecture ;
#  issue #275 ; ADR 0019, point 7.2 ; E2.26 ; M445) : la valeur retenue reste
#  celle des transcriptions visuelles assemblées
#  (assembler_table_unique_2024.R). Un relevé par la couche texte n'est
#  jamais une source de transcription (E2.26) ; aucun écart n'y corrige la
#  grille.
#
#  Hors CI : la CI ne lit ni la pièce ni l'outil d'extraction. Lancé à la
#  main, après le gel des transcriptions et leur assemblage. Sur le modèle de
#  releves_couche_texte_tgf05_tgh05.R (#274), mêmes règles de reconstruction
#  et de normalisation, pour une seule pièce et une seule table.
#
#  Pièce (docs/sources/) : jo_2024-11-22_n276_texte17.pdf (SHA-256
#  1a2f79c347620af519bd13f681c7cee63e3eedf545e12e8d7256c973e9ceef77, contrôlé
#  en R base avant lecture) ; table p. 2 à 34 (annexe à l'article
#  A. 132-18-1 du code des assurances, seule table de l'annexe).
#
#  Outil : pdftotext (poppler), mode -bbox (mots et coordonnées), version
#  écrite au journal. La ligne est reconstruite par l'ordonnée des mots
#  (tolérance 2 pt, rapportée au premier mot de la ligne), la colonne par
#  l'abscisse du centre du mot, rapportée au centre de l'année d'en-tête du
#  bloc la plus proche (en-tête courant, repris sur les pages qui ne le
#  répètent pas ; un mot à plus d'une demi-largeur de colonne est consigné
#  au journal). Une ligne n'est lue qu'après un en-tête `Age <années>`. Les
#  groupes de milliers (« 100 000 ») sont réunis quand un mot d'un à trois
#  chiffres est suivi d'un mot de trois chiffres à moins de 4 pt (un seul
#  groupe : les l_x imprimés sont au plus 100 000). Normalisation consignée :
#  « – » (U+2013), « − » (U+2212), « — » (U+2014), « - » et cellule sans mot
#  -> `-`. Deux mots dans une même colonne donnent une cellule
#  `?<mot1>/<mot2>`, un mot illisible `?<mot>` (hors format 2, consignés).
#  Âges lus : 0 à 121 (la ligne 121, imprimée en « - », n'est pas transcrite
#  au format 2).
#
#  Sorties, dans <dossier de sortie> :
#    table_unique_2024.couche_texte.csv : relevé au format 2 (ADR 0019,
#        point 5.2), lignes sans aucune valeur non écrites ;
#    table_unique_2024.couche_texte.ecarts.csv : `<âge>,<génération>,
#        <cellule du fichier de table>,<cellule du relevé>` (ADR 0019,
#        point 7.2), un côté vide quand la ligne n'existe que de l'autre ;
#    table_unique_2024.couche_texte.journal.txt : source, empreinte, outil,
#        normalisations, anomalies de lecture, décomptes.
#  Sur la sortie standard : le décompte des cellules comparées et des écarts.
#  Échec (« ECHEC : … » sur la sortie d'erreur, code 1) si la pièce n'a pas
#  l'empreinte attendue, si pdftotext échoue ou rend autre chose que de
#  l'UTF-8, si une cellule du relevé n'est pas en ASCII, si une ligne de la
#  grille assemblée n'a pas une cellule par génération de son en-tête ou y
#  répète un âge, ou si elle n'a pas une génération 1900 à 2005 à chaque âge.
#
#  Emplacement des entrées. Le script ne lit aucun chemin en dur : dossier
#  des pièces, fichier de table assemblé et dossier de sortie sont ses
#  arguments. Rejeu, depuis la racine du dépôt, dans un dossier hors du
#  dépôt :
#    d=$(mktemp -d)
#    Rscript tests/contre_verification/tables/releves_couche_texte_table_unique_2024.R \
#      docs/sources inst/extdata/tables/table_unique_2024.csv "$d"
#  La sortie d'assembler_table_unique_2024.R peut remplacer le fichier de
#  table embarqué (mêmes octets).
#
#  Usage : Rscript tests/contre_verification/tables/releves_couche_texte_table_unique_2024.R
#            <dossier docs/sources> <fichier de table assemblé> <dossier de sortie>
# -----------------------------------------------------------------------------

local({

TABLE <- "table_unique_2024"
PIECE <- "jo_2024-11-22_n276_texte17.pdf"
EMPREINTE <- "1a2f79c347620af519bd13f681c7cee63e3eedf545e12e8d7256c973e9ceef77"
PAGES <- c(2L, 34L)
GENS <- 1900:2005
AGES <- 0:121
MOT <- "<word xMin=\"([0-9.]+)\" yMin=\"([0-9.]+)\" xMax=\"([0-9.]+)\" yMax=\"([0-9.]+)\">(.*?)</word>"
TIRETS <- c("\u2013", "-", "\u2212", "\u2014")
TIRET_SEUL <- "^[\u2013\u2212\u2014-]$"

# Sorties en octets UTF-8, quelle que soit la locale (POSIX comprise) ; les
# chaînes non ASCII du script sont écrites par \u, donc marquées UTF-8.
ecrire <- function(x, con = stdout()) writeLines(enc2utf8(x), con, useBytes = TRUE)
echec <- function(msg) {
  ecrire(paste0("ECHEC : ", msg), stderr())
  quit(save = "no", status = 1L)
}
ecrire_fichier <- function(lignes, chemin, ascii) {
  octets <- charToRaw(paste0(enc2utf8(lignes), "\n", collapse = ""))
  if (!length(lignes)) octets <- raw(0)
  if (ascii && any(octets > as.raw(0x7f))) echec(sprintf("%s : cellule non ASCII", chemin))
  con <- file(chemin, "wb")
  writeBin(octets, con)
  close(con)
}

# repr() d'une chaîne, à la manière de Python (même règle que
# releves_couche_texte_tgf05_tgh05.R), et d'une liste d'entiers.
NON_IMPRIMABLES <- c(0:31, 127:160, 173, 0x1680, 0x2000:0x200F, 0x2028:0x202F, 0x205F:0x2064, 0x3000, 0xFEFF)
repr_chaine <- function(s) {
  q <- if (grepl("'", s, fixed = TRUE) && !grepl("\"", s, fixed = TRUE)) "\"" else "'"
  cp <- utf8ToInt(enc2utf8(s))
  morceaux <- vapply(cp, function(u) {
    if (u == 92L) "\\\\"
    else if (u == utf8ToInt(q)) paste0("\\", q)
    else if (u == 9L) "\\t" else if (u == 10L) "\\n" else if (u == 13L) "\\r"
    else if (u %in% NON_IMPRIMABLES) {
      if (u < 256L) sprintf("\\x%02x", u) else sprintf("\\u%04x", u)
    } else intToUtf8(u)
  }, "")
  paste0(q, paste(morceaux, collapse = ""), q)
}
repr_liste <- function(x) paste0("[", paste(x, collapse = ", "), "]")

deentite <- function(t) {
  if (!grepl("&", t, fixed = TRUE)) return(t)
  t <- gsub("&lt;", "<", t, fixed = TRUE)
  t <- gsub("&gt;", ">", t, fixed = TRUE)
  t <- gsub("&quot;", "\"", t, fixed = TRUE)
  t <- gsub("&apos;", "'", t, fixed = TRUE)
  for (m in unique(regmatches(t, gregexpr("&#[0-9]+;|&#[xX][0-9a-fA-F]+;", t))[[1L]])) {
    v <- sub("^&#", "", sub(";$", "", m))
    u <- if (grepl("^[xX]", v)) strtoi(substring(v, 2L), 16L) else strtoi(v, 10L)
    t <- gsub(m, intToUtf8(u), t, fixed = TRUE)
  }
  gsub("&amp;", "&", t, fixed = TRUE)
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


# Mots de chaque page : liste (numéro de page, data.frame xmin, ymin, xmax,
# ymax, texte), dans l'ordre de pdftotext.
mots_par_page <- function(pdf, p1, p2) {
  sortie <- suppressWarnings(system2("pdftotext", c("-bbox", "-f", p1, "-l", p2, shQuote(pdf), "-"),
                                     stdout = TRUE, stderr = FALSE))
  if (!is.null(attr(sortie, "status"))) echec(sprintf("pdftotext en \u00e9chec sur %s", pdf))
  Encoding(sortie) <- "UTF-8"
  if (!all(validUTF8(sortie))) echec(sprintf("%s : sortie de pdftotext non UTF-8", pdf))
  page <- cumsum(grepl("<page ", sortie, fixed = TRUE))
  lapply(seq_len(max(page, 0L)), function(i) {
    l <- sortie[page == i]
    m <- unlist(regmatches(l, gregexpr(MOT, l, perl = TRUE)))
    g <- regmatches(m, regexec(MOT, m, perl = TRUE))
    champ <- function(k) vapply(g, `[`, "", k + 1L)
    list(page = p1 + i - 1L,
         mots = data.frame(xmin = as.double(champ(1L)), ymin = as.double(champ(2L)),
                           xmax = as.double(champ(3L)), ymax = as.double(champ(4L)),
                           texte = vapply(champ(5L), deentite, "", USE.NAMES = FALSE),
                           stringsAsFactors = FALSE))
  })
}

# Regroupe les mots par ordonnée (tolérance 2 pt par rapport au premier mot
# du groupe, mots triés par ordonnée puis abscisse) ; chaque ligne triée par
# abscisse (tri stable).
lignes <- function(mots) {
  mots <- mots[order(mots$ymin, mots$xmin), , drop = FALSE]
  groupe <- integer(nrow(mots)); cle <- NA_real_; k <- 0L
  for (i in seq_len(nrow(mots))) {
    if (k == 0L || !(abs(cle - mots$ymin[i]) < 2)) { k <- k + 1L; cle <- mots$ymin[i] }
    groupe[i] <- k
  }
  lapply(split(seq_len(nrow(mots)), groupe), function(j) {
    l <- mots[j, , drop = FALSE]
    l[order(l$xmin), , drop = FALSE]
  })
}

# Réunit les groupes de milliers ; rend (texte, centre, xmin, xmax).
jetons <- function(mots) {
  tx <- character(0); a <- numeric(0); b <- numeric(0)
  for (i in seq_len(nrow(mots))) {
    t <- mots$texte[i]; k <- length(tx)
    if (k && grepl("^[0-9]{3}$", t) && grepl("^[0-9]{1,3}$", tx[k]) &&
        mots$xmin[i] - b[k] >= 0 && mots$xmin[i] - b[k] < 4) {
      tx[k] <- paste(tx[k], t); b[k] <- mots$xmax[i]
    } else {
      tx <- c(tx, t); a <- c(a, mots$xmin[i]); b <- c(b, mots$xmax[i])
    }
  }
  data.frame(texte = tx, centre = (a + b) / 2, xmin = a, xmax = b, stringsAsFactors = FALSE)
}

relever <- function(pdf) {
  journal <- character(0)
  stats <- integer(0)
  compter <- function(k) stats[k] <<- if (is.na(stats[k])) 1L else stats[k] + 1L
  normaliser <- function(t) {
    if (t %in% TIRETS) { compter(paste("tiret", repr_chaine(t))); return("-") }
    if (grepl("^[0-9]{1,3}( [0-9]{3})*$", t)) {
      v <- gsub(" ", "", t, fixed = TRUE)
      if (nchar(v) > 1L && substr(v, 1L, 1L) == "0") return(NULL)
      return(v)
    }
    NULL
  }
  grille <- new.env(hash = TRUE)
  entete <- NULL           # data.frame g\u00e9n\u00e9ration, centre
  age_xmax <- NULL
  for (pg in mots_par_page(pdf, PAGES[1L], PAGES[2L])) {
    page <- pg$page
    for (ligne in lignes(pg$mots)) {
      textes <- ligne$texte
      if (grepl("^(?:[aA]|\u00e2|\u00c2)[gG][eE]$", textes[1L], perl = TRUE) && length(textes) > 1L &&
          all(grepl("^(19|20)[0-9][0-9]$", textes[-1L]))) {
        entete <- data.frame(g = as.integer(textes[-1L]), centre = (ligne$xmin[-1L] + ligne$xmax[-1L]) / 2)
        age_xmax <- ligne$xmax[1L] + 15
        next
      }
      if (is.null(entete)) next
      if (!grepl("^[0-9]{1,3}$", textes[1L]) || ligne$xmax[1L] > age_xmax) next
      # en-tête ou pied de page (date, « sur », titre courant)
      if (!all(grepl("^[0-9]+$", textes) | grepl(TIRET_SEUL, textes, perl = TRUE))) next
      age <- as.integer(textes[1L])
      if (age > AGES[length(AGES)]) next
      toks <- jetons(ligne[-1L, , drop = FALSE])
      ecart_col <- if (nrow(entete) > 1L) min(diff(entete$centre)) else 40
      cellules <- list()
      for (i in seq_len(nrow(toks))) {
        mot <- toks$texte[i]; ctr <- toks$centre[i]
        j <- which.min(abs(entete$centre - ctr))
        g <- entete$g[j]; cg <- entete$centre[j]
        if (abs(cg - ctr) > ecart_col / 2) {
          journal <- c(journal, sprintf("p.%d \u00e2ge %d : mot %s hors colonne (centre %.1f)",
                                        page, age, repr_chaine(mot), ctr))
          next
        }
        cle <- as.character(g)
        if (!is.null(cellules[[cle]])) {
          journal <- c(journal, sprintf(paste0("p.%d \u00e2ge %d : deux mots dans la colonne %d : %s et %s ; ",
                                               "cellule \u00e9crite '?<mot1>/<mot2>' (hors format 2)"),
                                        page, age, g, repr_chaine(cellules[[cle]]), repr_chaine(mot)))
          cellules[[cle]] <- paste0(cellules[[cle]], "|", mot)
          next
        }
        cellules[[cle]] <- mot
      }
      # La pièce imprime un tiret dans les cellules sans valeur.
      if (nrow(toks) != nrow(entete))
        journal <- c(journal, paste0(sprintf("p.%d \u00e2ge %d : %d mots pour %d colonnes (%d-%d) : ",
                                             page, age, nrow(toks), nrow(entete),
                                             entete$g[1L], entete$g[nrow(entete)]),
                                     paste(toks$texte, collapse = " ; ")))
      for (g in entete$g) {
        brut <- cellules[[as.character(g)]]
        if (is.null(brut)) {
          compter("cellule sans mot")
          v <- "-"
        } else if (grepl("|", brut, fixed = TRUE)) {
          v <- paste0("?", gsub("|", "/", gsub(" ", "", brut, fixed = TRUE), fixed = TRUE))
        } else {
          v <- normaliser(brut)
          if (is.null(v)) {
            journal <- c(journal, sprintf("p.%d \u00e2ge %d g\u00e9n. %d : mot illisible %s",
                                          page, age, g, repr_chaine(brut)))
            v <- paste0("?", brut)
          }
        }
        cle <- paste(age, g)
        if (!is.null(grille[[cle]]))
          journal <- c(journal, sprintf("p.%d \u00e2ge %d g\u00e9n. %d : cellule lue deux fois", page, age, g))
        assign(cle, v, envir = grille)
      }
    }
  }
  a <- rep(AGES, each = length(GENS)); g <- rep(GENS, length(AGES))
  lue <- vapply(paste(a, g), exists, NA, envir = grille, inherits = FALSE)
  if (any(!lue)) {
    ex <- head(which(!lue), 5L)
    journal <- c(journal, sprintf("%d cellules non lues, ex. %s", sum(!lue),
                                  repr_liste(sprintf("(%d, %d)", a[ex], g[ex]))))
  }
  journal <- c(journal, sprintf("cellules lues : %d sur %d (\u00e2ges %d \u00e0 %d, g\u00e9n\u00e9rations %d \u00e0 %d)",
                                sum(lue), length(lue), AGES[1L], AGES[length(AGES)], GENS[1L], GENS[length(GENS)]))
  for (k in sort(names(stats), method = "radix"))
    journal <- c(sprintf("normalisation : %s -> '-' : %d", k, stats[[k]]), journal)
  list(grille = grille, journal = journal)
}

# Grille au format 2 : liste nommée par l'âge de vecteurs nommés par la
# génération. Échec sur une ligne qui n'a pas une cellule par génération de
# l'en-tête (ni tronquée, ni complétée) ou sur un âge présent deux fois (ni
# l'une ni l'autre ligne retenue), comme la lecture du contrôle
# (tests/controles/tables_embarquees.R, lire_grille()).
lire_grille <- function(chemin) {
  brut <- readBin(chemin, "raw", file.size(chemin))
  l <- strsplit(rawToChar(brut), "\n", fixed = TRUE)[[1L]]
  gens <- strsplit(l[1L], ",", fixed = TRUE)[[1L]][-1L]
  r <- lapply(seq_along(l)[-1L], function(i) {
    x <- strsplit(paste0(l[i], ","), ",", fixed = TRUE)[[1L]]
    if (length(x) - 1L != length(gens))
      echec(sprintf("%s : ligne %d : %d cellule(s) au lieu de %d", chemin, i, length(x) - 1L, length(gens)))
    list(age = as.character(as.integer(x[1L])), cel = setNames(x[-1L], gens))
  })
  ages <- vapply(r, `[[`, "", "age")
  if (anyDuplicated(ages))
    echec(sprintf("%s : \u00e2ge %s pr\u00e9sent plus d'une fois", chemin, ages[anyDuplicated(ages)]))
  setNames(lapply(r, `[[`, "cel"), ages)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3L)
  echec("usage : releves_couche_texte_table_unique_2024.R <dossier docs/sources> <fichier de table assembl\u00e9> <dossier de sortie>")
sources <- args[1L]; table_assemblee <- args[2L]; sortie <- args[3L]
pdf <- file.path(sources, PIECE)
if (!file.exists(pdf)) echec(sprintf("%s : pi\u00e8ce absente", pdf))
empreinte <- sha256(readBin(pdf, "raw", file.size(pdf)))
if (empreinte != EMPREINTE) echec(sprintf("%s : SHA-256 %s au lieu de %s", PIECE, empreinte, EMPREINTE))
version <- suppressWarnings(system2("pdftotext", "-v", stdout = TRUE, stderr = TRUE))[1L]
r <- relever(pdf)
journal <- c(sprintf("source : %s (SHA-256 %s), p. %d \u00e0 %d ; outil : %s -bbox",
                     PIECE, empreinte, PAGES[1L], PAGES[2L], version), r$journal)
rel <- list(); vides <- integer(0)
for (a in AGES) {
  cel <- vapply(GENS, function(g) {
    v <- r$grille[[paste(a, g)]]
    if (is.null(v)) "?" else v
  }, "")
  if (all(cel == "-")) vides <- c(vides, a) else rel[[as.character(a)]] <- setNames(cel, GENS)
}
journal <- c(journal, sprintf("lignes sans valeur non \u00e9crites au relev\u00e9 : %s", repr_liste(vides)))
ecrire_fichier(c(paste0("age,", paste(GENS, collapse = ",")),
                 vapply(names(rel), function(a) paste0(a, ",", paste(rel[[a]], collapse = ",")), "", USE.NAMES = FALSE)),
               file.path(sortie, paste0(TABLE, ".couche_texte.csv")), ascii = TRUE)
ref <- lire_grille(table_assemblee)
ecarts <- character(0); n <- 0L
for (a in as.character(AGES)) {
  ra <- ref[[a]]; rb <- rel[[a]]
  if (is.null(ra) && is.null(rb)) next
  if (!is.null(ra) && !all(as.character(GENS) %in% names(ra)))
    echec(sprintf("%s : \u00e2ge %s, g\u00e9n\u00e9rations incompl\u00e8tes", table_assemblee, a))
  for (g in as.character(GENS)) {
    ca <- if (is.null(ra)) "" else ra[[g]]
    cb <- if (is.null(rb)) "" else rb[[g]]
    n <- n + 1L
    if (ca != cb) ecarts <- c(ecarts, paste(a, g, ca, cb, sep = ","))
  }
}
ecrire_fichier(ecarts, file.path(sortie, paste0(TABLE, ".couche_texte.ecarts.csv")), ascii = TRUE)
journal <- c(journal, sprintf("%s : %d cellules compar\u00e9es, %d \u00e9carts", TABLE, n, length(ecarts)))
ecrire_fichier(journal, file.path(sortie, paste0(TABLE, ".couche_texte.journal.txt")), ascii = FALSE)
ecrire(sprintf("jo2024 %s : %d cellules compar\u00e9es, %d \u00e9carts", TABLE, n, length(ecarts)))

})
