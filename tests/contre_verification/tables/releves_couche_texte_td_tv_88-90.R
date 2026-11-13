# -----------------------------------------------------------------------------
#  tests/contre_verification/tables/releves_couche_texte_td_tv_88-90.R :
#  relevés par la couche texte de TD 88-90 et TV 88-90 (issue #264 ; ADR 0018,
#  points 5.3 et 5.5 ; M445)
#
#  Hors CI : la CI compare les relevés versés (tests/controles/
#  tables_embarquees.R), sans les pièces ni l'outil d'extraction. Lancé à la
#  main, depuis la racine du dépôt, après le gel des deux transcriptions
#  visuelles : un relevé par la couche texte n'est jamais une source de
#  transcription (E2.26) :
#    - contrôle de lecture : couche texte du fac-similé source, JO n° 2 du
#      03/01/2016, texte 39 (docs/sources/jo_2016-01-03_n2_texte39.pdf,
#      p. 19 à 23 de l'extrait) -> <identifiant>.couche_texte.csv ;
#    - seconde source : couche texte du BO Santé 2007-1, annexe III
#      (docs/sources/bo_sante_2007-1_annonce58_impression_2026-11-13.pdf,
#      p. 68 à 71) -> <identifiant>.seconde_source_bo.csv, zéros du BO
#      compris (une ligne par ligne imprimée avec une valeur, point 5.2).
#  Outil : pdftotext -layout (poppler), version écrite sur la sortie standard
#  et à consigner dans le champ de méthode du fichier de provenance.
#
#  Lecture des lignes :
#    - JO : chaque ligne de tableau se découpe aux suites d'au moins deux
#      espaces ; les morceaux alternent âge et l_x (une espace seule est le
#      séparateur de milliers, retiré ; un nombre sans séparateur est admis,
#      TV 88-90, âge 94, « 10636 ») ; TD entre « Art. 600-5 » et « Art.
#      600-6 » (un couple par ligne), TV après « Art. 600-6 » (trois couples
#      par ligne, colonnes 0-46, 47-93, 94-110) ;
#    - BO : après « ANNEXE III », ligne « âge l_x(TD) l_x(TV) » dont les
#      jetons sont séparés par des espaces de largeur variable ; un nombre est
#      un jeton de 1 à 3 chiffres, éventuellement suivi d'un jeton
#      d'exactement 3 chiffres (milliers) ; toutes les partitions des jetons
#      en deux nombres sont énumérées, et une ligne qui en admet plus d'une
#      est un échec, jamais tranchée en silence.
#  Un âge en double ou une ligne illisible arrête le script (code 1).
#
#  Usage : Rscript tests/contre_verification/tables/releves_couche_texte_td_tv_88-90.R [sortie]
#  `sortie` : dossier des relevés (tests/contre_verification/tables par défaut).
# -----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
SORTIE <- if (length(args)) args[1L] else file.path("tests", "contre_verification", "tables")
JO <- file.path("docs", "sources", "jo_2016-01-03_n2_texte39.pdf")
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

cat(system2("pdftotext", "-v", stdout = TRUE, stderr = TRUE)[1L], "\n")

# --- JO n° 2 du 03/01/2016 (contrôle de lecture) -------------------------------
l <- couche_texte(JO, 19L, 23L)
td_debut <- grep("Art\\. 600-5", l); tv_debut <- grep("Art\\. 600-6", l)
if (length(td_debut) != 1L || length(tv_debut) != 1L) arreter("intitulés des articles 600-5 et 600-6 introuvables")
lire_jo <- function(lignes, couples, quoi) {
  releve <- new.env()
  for (s in lignes) {
    m <- strsplit(trimws(s), " {2,}")[[1L]]
    if (!length(m) || !all(grepl("^([0-9]+|[0-9]{1,3}( [0-9]{3})+)$", m))) next
    if (length(m) %% 2L || length(m) / 2L > couples) arreter("%s : ligne illisible « %s »", quoi, s)
    v <- as.double(gsub(" ", "", m, fixed = TRUE))
    ajouter(releve, v[c(TRUE, FALSE)], v[c(FALSE, TRUE)], quoi)
  }
  releve
}
ecrire_releve(lire_jo(l[(td_debut + 1L):(tv_debut - 1L)], 1L, "JO, TD 88-90"), "td_88-90.couche_texte.csv")
ecrire_releve(lire_jo(l[(tv_debut + 1L):length(l)], 3L, "JO, TV 88-90"), "tv_88-90.couche_texte.csv")

# --- BO Santé 2007-1, annexe III (seconde source) -------------------------------
l <- couche_texte(BO, 68L, 71L)
debut <- grep("ANNEXE III", l, fixed = TRUE)
if (length(debut) != 1L) arreter("BO : « ANNEXE III » introuvable")
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
td <- new.env(); tv <- new.env()
for (s in l[(debut + 1L):length(l)]) {
  j <- strsplit(trimws(s), " +")[[1L]]
  if (length(j) < 2L || !all(grepl("^[0-9]+$", j))) next
  p <- partitions(j[-1L], 2L)
  if (length(p) != 1L) arreter("BO : ligne « %s » : %d lecture(s) possible(s)", trimws(s), length(p))
  ajouter(td, as.double(j[1L]), p[[1L]][1L], "BO, TD 88-90")
  ajouter(tv, as.double(j[1L]), p[[1L]][2L], "BO, TV 88-90")
}
ecrire_releve(td, "td_88-90.seconde_source_bo.csv")
ecrire_releve(tv, "tv_88-90.seconde_source_bo.csv")
