# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/releves_couche_texte_th_tf_00-02.R :
#  relevés par la couche texte de TH 00-02 et TF 00-02 (issue #145 ; ADR 0018,
#  points 5.3 et 5.5 ; M445)
#
#  Hors CI : la CI compare les relevés versés (tests/controles/
#  tables_embarquees.R), sans les pièces ni l'outil d'extraction. Lancé à la
#  main, depuis la racine du dépôt, après le gel des deux transcriptions
#  visuelles : un relevé par la couche texte n'est jamais une source de
#  transcription (E2.26) :
#    - contrôle de lecture : couche texte du fac-similé source, JO n° 301 du
#      29/12/2015, texte 35, annexe à l'article A132-18
#      (docs/sources/jo_2015-12-29_n301_texte35.pdf, p. 13 à 19 de l'extrait)
#      -> <identifiant>.couche_texte.csv ;
#    - seconde source : couche texte du BO Santé 2007-1, annexe I
#      (docs/sources/bo_sante_2007-1_annonce58_impression_2026-11-13.pdf,
#      p. 4 à 9) -> <identifiant>.seconde_source_bo.csv.
#  Une ligne imprimée sans valeur (TH 00-02, âges 111 et 112, dans les deux
#  pièces) n'est pas relevée ; elle est écrite sur la sortie standard.
#  Outil : pdftotext -layout (poppler), version écrite sur la sortie standard
#  et à consigner dans le champ de méthode du fichier de provenance.
#
#  Lecture des lignes (un couple âge / l_x par ligne dans les deux pièces) :
#    - JO : chaque ligne de tableau se découpe aux suites d'au moins deux
#      espaces ; les morceaux sont l'âge puis l_x (une espace seule est le
#      séparateur de milliers, retiré) ; TF entre « TABLE TF 00-02 » et
#      « TABLE TH 00-02 », TH entre « TABLE TH 00-02 » et « Décalage d’âge
#      TF 00-02 » (motif « calage d ») ;
#    - BO : ligne « âge l_x » dont les jetons sont séparés par des espaces de
#      largeur variable ; l_x est un jeton de 1 à 3 chiffres, éventuellement
#      suivi d'un jeton d'exactement 3 chiffres (milliers) ; toutes les
#      lectures des jetons sont énumérées, et une ligne qui en admet plus
#      d'une est un échec, jamais tranchée en silence ; TF entre « Table TF
#      00-02 » et « Table TH 00-02 », TH entre « Table TH 00-02 » et
#      « Décalage d’âge - TF 00-02 » (motif « calage d »).
#  Un âge en double ou une ligne illisible arrête le script (code 1).
#
#  Usage : Rscript tests/contre_verification/tables/releves_couche_texte_th_tf_00-02.R [sortie]
#  `sortie` : dossier des relevés (tests/contre_verification/tables par défaut).
# -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
SORTIE <- if (length(args)) args[1L] else file.path("tests", "contre_verification", "tables")
JO <- file.path("docs", "sources", "jo_2015-12-29_n301_texte35.pdf")
BO <- file.path("docs", "sources", "bo_sante_2007-1_annonce58_impression_2026-11-13.pdf")

arreter <- function(...) { cat("ECHEC :", sprintf(...), "\n"); quit(save = "no", status = 1L) }

couche_texte <- function(pdf, premiere, derniere) {
  r <- system2("pdftotext", c("-layout", "-f", premiere, "-l", derniere, shQuote(pdf), "-"), stdout = TRUE)
  if (!is.null(attr(r, "status"))) arreter("pdftotext en échec sur %s", pdf)
  Encoding(r) <- "UTF-8"
  r
}

# Ajoute les couples (âge, l_x) à `releve` (environnement), refuse un doublon.
ajouter <- function(releve, age, lx, quoi) {
  for (k in seq_along(age)) {
    cle <- as.character(age[k])
    if (!is.null(releve[[cle]])) arreter("%s : âge %d en double", quoi, age[k])
    assign(cle, lx[k], envir = releve)
  }
}

ecrire_releve <- function(releve, fichier) {
  age <- sort(as.integer(ls(releve)))
  lx <- vapply(as.character(age), function(a) releve[[a]], 0)
  if (any(diff(age) != 1L)) arreter("%s : âges non consécutifs", fichier)
  f <- file.path(SORTIE, fichier)
  con <- file(f, "wb"); on.exit(close(con))
  writeBin(charToRaw(paste0(c("age,lx", sprintf("%d,%.0f", age, lx)), "\n", collapse = "")), con)
  cat(sprintf("  %s : âges %d à %d, %d lignes\n", f, min(age), max(age), length(age)))
}

# Lignes comprises entre l'intitulé `debut` (exclu) et la première ligne
# suivante qui contient `fin` (exclue) ; motifs en ASCII (la locale du script
# n'est pas fixée), comparés octet à octet.
entre <- function(l, debut, fin, quoi) {
  d <- grep(debut, l, fixed = TRUE, useBytes = TRUE)
  if (length(d) != 1L) arreter("%s : intitulé « %s » introuvable ou répété", quoi, debut)
  f <- grep(fin, l, fixed = TRUE, useBytes = TRUE)
  f <- f[f > d][1L]
  if (is.na(f)) arreter("%s : « %s » introuvable après « %s »", quoi, fin, debut)
  l[(d + 1L):(f - 1L)]
}

cat(system2("pdftotext", "-v", stdout = TRUE, stderr = TRUE)[1L], "\n")

# --- JO n° 301 du 29/12/2015 (contrôle de lecture) -----------------------------
l <- couche_texte(JO, 13L, 19L)
lire_jo <- function(lignes, quoi) {
  releve <- new.env()
  for (s in lignes) {
    m <- strsplit(trimws(s), " {2,}")[[1L]]
    if (!length(m) || !all(grepl("^([0-9]+|[0-9]{1,3}( [0-9]{3})+)$", m))) next
    if (length(m) == 1L && grepl("^[0-9]+$", m)) { cat(sprintf("  %s : âge %s imprimé sans valeur, non relevé\n", quoi, m)); next }
    if (length(m) != 2L) arreter("%s : ligne illisible « %s »", quoi, s)
    v <- as.double(gsub(" ", "", m, fixed = TRUE))
    ajouter(releve, v[1L], v[2L], quoi)
  }
  releve
}
ecrire_releve(lire_jo(entre(l, "TABLE TF 00-02", "TABLE TH 00-02", "JO"), "JO, TF 00-02"), "tf_00-02.couche_texte.csv")
ecrire_releve(lire_jo(entre(l, "TABLE TH 00-02", "calage d", "JO"), "JO, TH 00-02"), "th_00-02.couche_texte.csv")

# --- BO Santé 2007-1, annexe I (seconde source) ---------------------------------
l <- couche_texte(BO, 4L, 9L)
partitions <- function(jetons, k) {
  if (k == 0L) return(if (length(jetons)) list() else list(numeric(0)))
  r <- list()
  if (length(jetons) && grepl("^[0-9]{1,3}$", jetons[1L])) {
    r <- c(r, lapply(partitions(jetons[-1L], k - 1L), function(p) c(as.double(jetons[1L]), p)))
    if (length(jetons) > 1L && grepl("^[0-9]{3}$", jetons[2L]))
      r <- c(r, lapply(partitions(jetons[-(1:2)], k - 1L), function(p) c(as.double(paste0(jetons[1L], jetons[2L])), p)))
  }
  r
}
lire_bo <- function(lignes, quoi) {
  releve <- new.env()
  for (s in lignes) {
    j <- strsplit(trimws(s), " +")[[1L]]
    if (!length(j) || !all(grepl("^[0-9]+$", j))) next
    if (length(j) == 1L) { cat(sprintf("  %s : âge %s imprimé sans valeur, non relevé\n", quoi, j)); next }
    p <- partitions(j[-1L], 1L)
    if (length(p) != 1L) arreter("%s : ligne « %s » : %d lecture(s) possible(s)", quoi, trimws(s), length(p))
    ajouter(releve, as.double(j[1L]), p[[1L]][1L], quoi)
  }
  releve
}
ecrire_releve(lire_bo(entre(l, "Table TF 00-02", "Table TH 00-02", "BO"), "BO, TF 00-02"), "tf_00-02.seconde_source_bo.csv")
ecrire_releve(lire_bo(entre(l, "Table TH 00-02", "calage d", "BO"), "BO, TH 00-02"), "th_00-02.seconde_source_bo.csv")
