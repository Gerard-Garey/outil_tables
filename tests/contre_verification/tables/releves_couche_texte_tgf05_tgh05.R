# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/releves_couche_texte_tgf05_tgh05.R :
#  relevés de TGF05 et TGH05 par la couche texte de trois pièces,
#  pour la seule comparaison (issue #274 ; ADR 0019, point 7.2 ; M445, M462) :
#  la valeur retenue reste celle des transcriptions visuelles assemblées
#  (assembler_tgf05_tgh05.R). Un relevé par la couche texte n'est jamais une source de
#  transcription (E2.26).
#
#  Hors CI : la CI ne lit ni les pièces ni l'outil d'extraction. Lancé à la
#  main, après le gel des transcriptions et leur assemblage. Réécriture en R
#  de releves.py (commit b949ced), à sortie identique (décision du mainteneur
#  du 14/11/2026, M478) :
#  mêmes relevés, mêmes écarts, même journal, octet pour octet, mêmes lignes
#  sur la sortie standard.
#
#  Pièces (docs/sources/) et pages de l'extrait :
#    - jo2015 : JO n° 301 du 29/12/2015, texte 35 (contrôle de lecture) ;
#      TGF05 p. 19 à 51, TGH05 p. 51 à 83 ;
#    - bo2007 : BO Santé 2007-1, annexe II (seconde source) ; TGF05 p. 10 à
#      39, TGH05 p. 39 à 68 ;
#    - jo2006 : JO n° 197 du 26/08/2006, texte 11 (comparaison à l'acte,
#      M462) ; TGF05 p. 3 à 20, TGH05 p. 20 à 38.
#
#  Outil : pdftotext (poppler), mode -bbox (mots et coordonnées), version
#  écrite au journal. La ligne est reconstruite par l'ordonnée des mots
#  (tolérance 2 pt, rapportée au premier mot de la ligne), la colonne par
#  l'abscisse du centre du mot, rapportée au centre de l'année d'en-tête du
#  bloc la plus proche (en-tête courant, repris sur les pages qui ne le
#  répètent pas ; un mot à plus d'une demi-largeur de colonne est consigné
#  au journal). La table en cours est donnée par une ligne qui contient le
#  mot « TABLE », « TGF05 » ou « TGH05 ». Les groupes de milliers
#  (« 100 000 ») sont réunis quand un mot d'un à trois chiffres est suivi
#  d'un mot de trois chiffres à moins de 4 pt (un seul groupe : les l_x
#  imprimés sont au plus 100 000 ; sans cette borne, deux colonnes serrées
#  du BO, p. 36 et 65, âge 0, se réunissaient). Normalisation consignée :
#  « – » (U+2013), « − » (U+2212), « — » (U+2014), « - » et cellule sans mot
#  -> `-`. Deux mots dans une même colonne donnent une cellule
#  `?<mot1>/<mot2>`, un mot illisible `?<mot>` (hors format 2, consignés).
#
#  Sorties, par source s et table t, dans <dossier de sortie> :
#    releve_<s>_<t>.csv : grille au format 2 (ADR 0019, point 5.2), lignes
#                         sans aucune valeur non écrites ;
#    ecarts_<s>_<t>.csv : `<âge>,<génération>,<cellule du fichier de table>,
#                         <cellule du relevé>` (ADR 0019, point 7.2), un côté
#                         vide quand la ligne n'existe que de l'autre ;
#    journal_<s>.txt    : source, outil, normalisations, anomalies de
#                         lecture, décomptes.
#  Sur la sortie standard : le décompte des cellules comparées et des écarts,
#  par source et par table. Échec (« ECHEC : … » sur la sortie d'erreur,
#  code 1) si pdftotext échoue ou rend autre chose que de l'UTF-8, si une
#  cellule d'un relevé n'est pas en ASCII, ou si une grille assemblée n'a pas
#  une génération 1900 à 2005.
#
#  Emplacement des entrées. Le script ne lit aucun chemin en dur : pièces,
#  grilles assemblées et dossier de sortie sont ses arguments. Les grilles
#  assemblées sont les fichiers de table inst/extdata/tables/tgf05.csv et
#  tgh05.csv, une fois les deux tables embarquées (ou leur assemblage par
#  assembler_tgf05_tgh05.R, ou le dossier provisoire tant qu'il existe) ; les
#  relevés, écarts et journaux qu'il écrit ont été versés au commit b949ced sous
#  tests/contre_verification/transcriptions_b22/generation/ (dossier
#  provisoire à retirer au dernier commit donnees: de #274, M474), où ils
#  resteront lisibles. Les relevés
#  versés sous tests/contre_verification/tables/ en sont des copies, au
#  nommage de l'ADR 0018, point 5 : releve_bo2007_<t>.csv ->
#  <t>.seconde_source_bo.csv ; releve_jo2006_<t>.csv ->
#  <t>.comparaison_acte.csv ; releve_jo2015_<t>.csv n'est pas versé (TGF05 :
#  cellule `?66/72` de la ligne 117, hors format 2 ; anomalie A2).
#  Rejeu, depuis la racine du dépôt, dans un dossier hors du dépôt, sur les
#  fichiers de table embarqués (TGF05 et TGH05) :
#    d=$(mktemp -d)
#    Rscript tests/contre_verification/tables/releves_couche_texte_tgf05_tgh05.R \
#      docs/sources inst/extdata/tables "$d"
#  Le dossier de sortie d'assembler_tgf05_tgh05.R peut remplacer
#  inst/extdata/tables (mêmes octets).
#
#  Usage : Rscript tests/contre_verification/tables/releves_couche_texte_tgf05_tgh05.R
#            <dossier docs/sources> <dossier des grilles assemblées> <dossier de sortie>
# -----------------------------------------------------------------------------

local({

SOURCES <- list(
  # nom = list(fichier, pages par table : première, dernière)
  jo2015 = list(fichier = "jo_2015-12-29_n301_texte35.pdf", tables = list(tgf05 = c(19L, 51L), tgh05 = c(51L, 83L))),
  bo2007 = list(fichier = "bo_sante_2007-1_annonce58_impression_2026-11-13.pdf", tables = list(tgf05 = c(10L, 39L), tgh05 = c(39L, 68L))),
  jo2006 = list(fichier = "jo_2006-08-26_n197_texte11.pdf", tables = list(tgf05 = c(3L, 20L), tgh05 = c(20L, 38L)))
)
GENS <- 1900:2005
AGES <- 0:120
MOT <- "<word xMin=\"([0-9.]+)\" yMin=\"([0-9.]+)\" xMax=\"([0-9.]+)\" yMax=\"([0-9.]+)\">(.*?)</word>"
# La pièce imprime-t-elle un tiret dans les cellules sans valeur ?  Sinon
# (BO), une cellule sans mot est la forme normale d'une cellule sans valeur.
TIRETS_IMPRIMES <- c(jo2015 = TRUE, bo2007 = FALSE, jo2006 = TRUE)
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

# Représentations reprises de Python, pour un journal identique : repr()
# d'une chaîne (guillemets, échappements ; caractères non imprimables : C0,
# DEL, C1 et les séparateurs et formats Unicode usuels, liste fermée ci-
# dessous, et non toute la catégorie Unicode), d'une liste d'entiers.
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

relever <- function(dossier, nom) {
  pdf <- file.path(dossier, SOURCES[[nom]]$fichier)
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
  grilles <- list()
  for (table in names(SOURCES[[nom]]$tables)) {
    pp <- SOURCES[[nom]]$tables[[table]]
    grille <- new.env(hash = TRUE)
    courante <- NULL         # table en cours de lecture sur la page
    entete <- NULL           # data.frame g\u00e9n\u00e9ration, centre
    age_xmax <- NULL
    for (pg in mots_par_page(pdf, pp[1L], pp[2L])) {
      page <- pg$page
      for (ligne in lignes(pg$mots)) {
        textes <- ligne$texte
        if (any(textes %in% c("TABLE", "TGF05", "TGH05"))) {
          if ("TGF05" %in% textes) courante <- "tgf05" else if ("TGH05" %in% textes) courante <- "tgh05"
          entete <- NULL
          next
        }
        if (grepl("^(?:[aA]|\u00e2|\u00c2)[gG][eE]$", textes[1L], perl = TRUE) && length(textes) > 1L &&
            all(grepl("^(19|20)[0-9][0-9]$", textes[-1L]))) {
          entete <- data.frame(g = as.integer(textes[-1L]), centre = (ligne$xmin[-1L] + ligne$xmax[-1L]) / 2)
          age_xmax <- ligne$xmax[1L] + 15
          next
        }
        if (is.null(courante) || courante != table || is.null(entete)) next
        if (!grepl("^[0-9]{1,3}$", textes[1L]) || ligne$xmax[1L] > age_xmax) next
        # en-tête ou pied de page (date, « sur », titre courant)
        if (!all(grepl("^[0-9]+$", textes) | grepl(TIRET_SEUL, textes, perl = TRUE))) next
        age <- as.integer(textes[1L])
        if (age > 120L) next
        toks <- jetons(ligne[-1L, , drop = FALSE])
        ecart_col <- if (nrow(entete) > 1L) min(diff(entete$centre)) else 40
        cellules <- list()
        for (i in seq_len(nrow(toks))) {
          mot <- toks$texte[i]; ctr <- toks$centre[i]
          j <- which.min(abs(entete$centre - ctr))
          g <- entete$g[j]; cg <- entete$centre[j]
          if (abs(cg - ctr) > ecart_col / 2) {
            journal <- c(journal, sprintf("%s p.%d \u00e2ge %d : mot %s hors colonne (centre %.1f)",
                                          table, page, age, repr_chaine(mot), ctr))
            next
          }
          cle <- as.character(g)
          if (!is.null(cellules[[cle]])) {
            journal <- c(journal, sprintf(paste0("%s p.%d \u00e2ge %d : deux mots dans la colonne %d : %s et %s ; ",
                                                 "cellule \u00e9crite '?<mot1>/<mot2>' (hors format 2)"),
                                          table, page, age, g, repr_chaine(cellules[[cle]]), repr_chaine(mot)))
            cellules[[cle]] <- paste0(cellules[[cle]], "|", mot)
            next
          }
          cellules[[cle]] <- mot
        }
        if (TIRETS_IMPRIMES[[nom]] && nrow(toks) != nrow(entete))
          journal <- c(journal, paste0(sprintf("%s p.%d \u00e2ge %d : %d mots pour %d colonnes (%d-%d) : ",
                                               table, page, age, nrow(toks), nrow(entete),
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
              journal <- c(journal, sprintf("%s p.%d \u00e2ge %d g\u00e9n. %d : mot illisible %s",
                                            table, page, age, g, repr_chaine(brut)))
              v <- paste0("?", brut)
            }
          }
          cle <- paste(age, g)
          if (!is.null(grille[[cle]]))
            journal <- c(journal, sprintf("%s p.%d \u00e2ge %d g\u00e9n. %d : cellule lue deux fois", table, page, age, g))
          assign(cle, v, envir = grille)
        }
      }
    }
    a <- rep(AGES, each = length(GENS)); g <- rep(GENS, length(AGES))
    lue <- vapply(paste(a, g), exists, NA, envir = grille, inherits = FALSE)
    if (any(!lue)) {
      ex <- head(which(!lue), 5L)
      journal <- c(journal, sprintf("%s : %d cellules non lues, ex. %s", table, sum(!lue),
                                    repr_liste(sprintf("(%d, %d)", a[ex], g[ex]))))
    }
    grilles[[table]] <- grille
  }
  for (k in sort(names(stats), method = "radix"))
    journal <- c(if (startsWith(k, "tiret") || startsWith(k, "cellule"))
                   sprintf("normalisation : %s -> '-' : %d", k, stats[[k]])
                 else sprintf("%s : %d", k, stats[[k]]), journal)
  list(grilles = grilles, journal = journal)
}

# Grille au format 2 : liste nommée par l'âge de vecteurs nommés par la
# génération.
lire_grille <- function(chemin) {
  brut <- readBin(chemin, "raw", file.size(chemin))
  l <- strsplit(rawToChar(brut), "\n", fixed = TRUE)[[1L]]
  gens <- strsplit(l[1L], ",", fixed = TRUE)[[1L]][-1L]
  r <- lapply(l[-1L], function(x) {
    x <- strsplit(paste0(x, ","), ",", fixed = TRUE)[[1L]]
    k <- seq_len(min(length(gens), length(x) - 1L))   # zip() de Python
    list(age = as.character(as.integer(x[1L])), cel = setNames(x[-1L][k], gens[k]))
  })
  r <- setNames(lapply(r, `[[`, "cel"), vapply(r, `[[`, "", "age"))
  r[!duplicated(names(r), fromLast = TRUE)]           # dict() : la derni\u00e8re ligne l'emporte
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L)
  echec("usage : releves_couche_texte_tgf05_tgh05.R <dossier docs/sources> <dossier des grilles assembl\u00e9es> <dossier de sortie>")
sources <- args[1L]; assemblees <- args[2L]; sortie <- args[3L]
version <- suppressWarnings(system2("pdftotext", "-v", stdout = TRUE, stderr = TRUE))[1L]
for (nom in names(SOURCES)) {
  r <- relever(sources, nom)
  journal <- c(sprintf("source : %s ; outil : %s -bbox", SOURCES[[nom]]$fichier, version), r$journal)
  for (table in names(r$grilles)) {
    grille <- r$grilles[[table]]
    rel <- list(); vides <- integer(0)
    for (a in AGES) {
      cel <- vapply(GENS, function(g) {
        v <- grille[[paste(a, g)]]
        if (is.null(v)) "?" else v
      }, "")
      if (all(cel == "-")) vides <- c(vides, a) else rel[[as.character(a)]] <- setNames(cel, GENS)
    }
    journal <- c(journal, sprintf("%s : lignes sans valeur non \u00e9crites au relev\u00e9 : %s", table, repr_liste(vides)))
    ecrire_fichier(c(paste0("age,", paste(GENS, collapse = ",")),
                     vapply(names(rel), function(a) paste0(a, ",", paste(rel[[a]], collapse = ",")), "", USE.NAMES = FALSE)),
                   file.path(sortie, sprintf("releve_%s_%s.csv", nom, table)), ascii = TRUE)
    ref <- lire_grille(file.path(assemblees, paste0(table, ".csv")))
    ecarts <- character(0); n <- 0L
    for (a in as.character(AGES)) {
      ra <- ref[[a]]; rb <- rel[[a]]
      if (is.null(ra) && is.null(rb)) next
      if (!is.null(ra) && !all(as.character(GENS) %in% names(ra)))
        echec(sprintf("%s.csv : \u00e2ge %s, g\u00e9n\u00e9rations incompl\u00e8tes", table, a))
      for (g in as.character(GENS)) {
        ca <- if (is.null(ra)) "" else ra[[g]]
        cb <- if (is.null(rb)) "" else rb[[g]]
        n <- n + 1L
        if (ca != cb) ecarts <- c(ecarts, paste(a, g, ca, cb, sep = ","))
      }
    }
    ecrire_fichier(ecarts, file.path(sortie, sprintf("ecarts_%s_%s.csv", nom, table)), ascii = TRUE)
    journal <- c(journal, sprintf("%s : %d cellules compar\u00e9es, %d \u00e9carts", table, n, length(ecarts)))
    ecrire(sprintf("%s %s : %d cellules compar\u00e9es, %d \u00e9carts", nom, table, n, length(ecarts)))
  }
  ecrire_fichier(journal, file.path(sortie, sprintf("journal_%s.txt", nom)), ascii = FALSE)
}

})
