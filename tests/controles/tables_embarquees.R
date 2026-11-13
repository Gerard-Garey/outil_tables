# -----------------------------------------------------------------------------
#  tests/controles/tables_embarquees.R : tables embarquées, fichiers de
#  provenance et contre-vérification (issue #263 ; ADR 0018, points 2, 3, 5
#  et 6 ; ADR 0011, annotation du 05/11/2026, (d) ; M447)
#
#  Installe l'arbre `racine` dans une bibliothèque temporaire (sous une locale
#  UTF-8), puis vérifie, pour chaque table déclarée
#  (DECLARATIONS_TABLES_EMBARQUEES, R/donnees.R) :
#    (a) complétude : fichier de table et fichier de provenance présents ;
#        aucun fichier de inst/extdata/tables/ ni de
#        tests/contre_verification/tables/ qui ne soit rattaché à une table
#        déclarée (relevé <identifiant>.<nature>.csv, nature transcription_b,
#        seconde_source, seconde_source_<pièce> ou couche_texte, cité par sa
#        provenance ; script cité dans un champ Empreintes-fichiers) ;
#    (b) format du fichier de table et des relevés, sur les octets (ADR 0018,
#        point 2.2 : ASCII, LF, fin de ligne finale, en-tête age,lx, entiers
#        en chiffres seuls, âges croissants et consécutifs) ; forme du
#        fichier de provenance (point 3.1 : UTF-8 valide sans BOM, LF, aucune
#        ligne vide, un seul enregistrement, premier champ Format, chaque
#        champ une seule fois, champs requis présents, aucun champ inconnu),
#        sur les lignes (read.dcf() fusionne les doublons sans message) ;
#        Nom, Fichier, Sexe (première ligne), Forme-origine égaux à la
#        déclaration ; chemins des relevés conformes ; Visa daté ;
#    (c) identité des fichiers : SHA-256 (outil sha256sum ; son absence est
#        un échec) de chaque chemin d'Empreintes-fichiers égal à la valeur
#        écrite ; fichier de table et relevés cités présents dans le champ ;
#    (d) identité moteur : table_embarquee(nom) rend un objet dont
#        l'empreinte, lue dans tables_embarquees(), égale Empreinte-table
#        (sous Representation), ou lève le refus de motif Refus-attendu dont
#        les paires d'âges (details$ages, âge suivant) égalent Refus-paires ;
#    (e) contre-vérification : fichier de table et transcription_b
#        identiques ligne à ligne ; écarts aux secondes sources et à la
#        couche texte égaux, dans les deux sens, aux écarts déclarés (point
#        5.4 : « aucun » ou une ligne <âge>,<l_x table>,<l_x relevé> par
#        écart, côté vide si la ligne manque) ;
#    (f) catalogue : tables_embarquees() a une ligne par table déclarée, dans
#        l'ordre, sexe et source de la déclaration, états conformes à (d) ;
#    (g) installation (point 1.5) : R CMD build de l'arbre, installation de
#        l'archive sous LC_ALL=C et sous une locale UTF-8, catalogue identique
#        à celui de (f) et tables accessibles après suppression du dossier
#        extdata de la bibliothèque (aucun fichier lu à l'exécution).
#
#  Témoin (--temoin, ADR 0018, point 6.2) : copie de l'arbre munie de deux
#  tables d'essai déclarées (l'une admise, l'autre refusée par le moteur,
#  champ Refus-attendu), contrôlée une première fois (succès attendu, mesure
#  (g) comprise : elle porte sur ces tables même si le catalogue est vide),
#  puis une seconde fois après qu'un l_x de la première table admise a été
#  changé d'une unité : échec attendu, avec des écarts sur (c), (d) et (e) ;
#  enfin, ce l_x rétabli, une troisième fois après que les paires attendues
#  de la table d'essai refusée (Refus-paires) ont été changées : échec
#  attendu, avec un écart (d) sur cette table (un refus autre que celui que
#  déclare la provenance n'est pas admis).
#
#  Usage : Rscript tests/controles/tables_embarquees.R [--sans-installation] [racine]
#          Rscript tests/controles/tables_embarquees.R --temoin [racine]
#  Code de sortie 1 en cas d'écart (lignes « ECART (x) » sur la sortie
#  standard, ligne ::error sur la CI). Fonctions de R de base, outil
#  sha256sum du système.
# -----------------------------------------------------------------------------

for (e_ in parse(encoding = "UTF-8", keep.source = FALSE, file = local({
  controle_ <- "tables_embarquees.R"
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1L]
  d <- c(if (!is.na(f)) c(dirname(f), file.path(dirname(f), "controles")), file.path("tests", "controles"))
  d <- d[file.exists(file.path(d, controle_))]
  if (!length(d) || !file.exists(file.path(d[1L], "orchestration_locale.R")))
    stop("orchestration_locale.R introuvable à côté de ", controle_)
  file.path(d[1L], "orchestration_locale.R")
}))) eval(e_, environment())
rm(e_)

LOCALES_UTF8 <- c("C.UTF-8", "C.utf8", "en_US.UTF-8")  # comme tests/harnais.R
DOSSIER_TABLES <- "inst/extdata/tables"
DOSSIER_RELEVES <- "tests/contre_verification/tables"
MOTIF_LIGNE <- "^(0|[1-9][0-9]{0,14}),(0|[1-9][0-9]{0,14})$"
MOTIF_PIECE <- "[a-z0-9]+"
SUFFIXES_GROUPE <- c("-methode", "-releve", "-ecarts")

# Champs du fichier de provenance (ADR 0018, point 3.2). Les champs de
# seconde source se déclinent par pièce : Seconde-source-<pièce>[-methode|
# -releve|-ecarts], relevé <identifiant>.seconde_source_<pièce>.csv ; sans
# pièce, Seconde-source[-methode|-releve|-ecarts], relevé
# <identifiant>.seconde_source.csv (M447, note 2).
CHAMPS_REQUIS <- c("Format", "Nom", "Fichier", "Sexe", "Forme-origine", "Intitule", "Ages", "Homologation",
                   "Porteurs-usages", "Source", "Source-nature", "Source-URL", "Source-consultation", "Source-licence",
                   "Source-pages", "Transcription-methode", "Transcription-date", "Transcription-B-releve",
                   "Transcription-B-methode", "Controles-coherence", "Ecarts-signales", "Empreintes-fichiers",
                   "Empreintes-hors-depot", "Visa")
CHAMPS_FACULTATIFS <- c("Controle-lecture-releve", "Controle-lecture-ecarts", "Controle-lecture-methode", "Historique",
                        "Representation", "Empreinte-table", "Refus-attendu", "Refus-paires")
FORMAT_PROVENANCE <- "tablexp provenance 1"
MOTIF_VISA <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}, mainteneur, [^ ].*$"

ecarts <- character(0)
ecart <- function(section, texte) {
  ecarts <<- c(ecarts, sprintf("(%s) %s", section, texte))
  cat(sprintf("  ECART (%s) %s\n", section, texte))
}

# Première locale UTF-8 disponible (sondée, comme tests/harnais.R).
locale_utf8 <- function() {
  ancienne <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ancienne))
  for (l in LOCALES_UTF8) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) return(l)
  NA_character_
}
environnement_locale <- function(locale) c(LC_ALL = locale, LANG = locale, LC_CTYPE = locale)

# SHA-256 des fichiers `chemins` (relatifs à `racine`) par l'outil sha256sum ;
# NA pour un fichier absent ; NULL si l'outil manque.
sha256 <- function(racine, chemins) {
  outil <- Sys.which("sha256sum")
  if (!nzchar(outil)) return(NULL)
  r <- rep(NA_character_, length(chemins))
  presents <- file.exists(file.path(racine, chemins))
  if (any(presents)) {
    s <- suppressWarnings(system2(outil, shQuote(file.path(racine, chemins[presents])), stdout = TRUE, stderr = TRUE))
    r[presents] <- substr(s, 1L, 64L)
  }
  r
}

# Format 1 d'un fichier de table ou d'un relevé, sur les octets (ADR 0018,
# point 2.2). Rend list(defaut = texte ou NULL, age, lx, lignes).
lire_format <- function(chemin) {
  o <- readBin(chemin, "raw", file.info(chemin)$size)
  defaut <- function(t) list(defaut = t)
  if (!length(o)) return(defaut("fichier vide"))
  if (any(o > as.raw(0x7e) | (o < as.raw(0x20) & o != as.raw(0x0a)))) return(defaut("octet hors ASCII imprimable et LF (BOM, CR, tabulation...)"))
  if (o[length(o)] != as.raw(0x0a)) return(defaut("pas de fin de ligne après la dernière ligne"))
  l <- strsplit(rawToChar(o), "\n", fixed = TRUE)[[1L]]
  if (!identical(l[1L], "age,lx")) return(defaut(sprintf("en-tête « %s » au lieu de « age,lx »", l[1L])))
  if (length(l) < 2L) return(defaut("aucune ligne de valeurs"))
  corps <- l[-1L]
  k <- which(!grepl(MOTIF_LIGNE, corps))
  if (length(k)) return(defaut(sprintf("ligne %d « %s » mal formée", k[1L] + 1L, corps[k[1L]])))
  age <- as.double(sub(",.*$", "", corps)); lx <- as.double(sub("^.*,", "", corps))
  k <- which(age != age[1L] + seq_along(age) - 1L)
  if (length(k)) return(defaut(sprintf("ligne %d : âge %s hors de la suite croissante et consécutive", k[1L] + 1L, age[k[1L]])))
  list(defaut = NULL, age = age, lx = lx, lignes = l)
}

# Fichier de provenance (ADR 0018, point 3.1), sur les octets puis les lignes.
# Rend list(defauts, champs) ; champs : liste nommée des valeurs (lignes de
# suite jointes par « \n », indentation retirée ; une première ligne vide,
# « Champ: » suivi de lignes de suite, n'est pas une ligne de la valeur).
lire_provenance <- function(chemin) {
  o <- readBin(chemin, "raw", file.info(chemin)$size)
  d <- character(0)
  if (length(o) >= 3L && identical(o[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) d <- c(d, "BOM en tête")
  if (any(o == as.raw(0x0d))) d <- c(d, "retour chariot (CR)")
  if (!length(o) || o[length(o)] != as.raw(0x0a)) d <- c(d, "pas de fin de ligne après la dernière ligne")
  texte <- rawToChar(o)
  if (!validUTF8(texte)) return(list(defauts = c(d, "octets qui ne sont pas de l'UTF-8 valide"), champs = list()))
  Encoding(texte) <- "UTF-8"
  l <- strsplit(texte, "\n", fixed = TRUE)[[1L]]
  if (any(!nzchar(trimws(l)))) d <- c(d, sprintf("ligne vide (ligne %s)", paste(which(!nzchar(trimws(l))), collapse = ", ")))
  champ <- grepl("^[A-Za-z][A-Za-z0-9-]*: ?", l)
  suite <- grepl("^[ \t]+[^ \t]", l)
  k <- which(!champ & !suite & nzchar(trimws(l)))
  if (length(k)) d <- c(d, sprintf("ligne %d ni champ ni ligne de suite", k[1L]))
  if (length(l) && !champ[1L]) d <- c(d, "la première ligne n'est pas un champ")
  champs <- list()
  noms <- character(0)
  for (i in seq_along(l)) {
    if (champ[i]) {
      n <- sub(":.*$", "", l[i])
      noms <- c(noms, n)
      champs[[length(champs) + 1L]] <- sub("^[A-Za-z][A-Za-z0-9-]*: ?", "", l[i])
    } else if (suite[i] && length(champs)) {
      v <- champs[[length(champs)]]
      champs[[length(champs)]] <- paste0(v, if (nzchar(v)) "\n", sub("^[ \t]+", "", l[i]))
    }
  }
  names(champs) <- noms
  if (anyDuplicated(noms)) d <- c(d, sprintf("champ(s) écrit(s) plus d'une fois : %s", paste(unique(noms[duplicated(noms)]), collapse = ", ")))
  if (!length(noms) || noms[1L] != "Format") d <- c(d, "premier champ autre que Format")
  # Tout ce que read.dcf() lit doit être ce que lisent les lignes : un seul
  # enregistrement, mêmes champs.
  r <- tryCatch(read.dcf(chemin, all = TRUE), error = function(e) NULL)
  if (is.null(r) || nrow(r) != 1L || !setequal(colnames(r), noms)) d <- c(d, "read.dcf() ne lit pas un seul enregistrement aux mêmes champs")
  list(defauts = d, champs = champs[!duplicated(noms)])
}

valeur <- function(champs, n) { v <- champs[[n]]; if (is.null(v)) NA_character_ else v }

# Écarts entre deux relevés (vecteurs nommés par âge) : « âge,a,b », côté
# vide si la ligne manque, triés par âge.
ecarts_releves <- function(a, b) {
  ages <- sort(unique(c(a$age, b$age)))
  va <- a$lx[match(ages, a$age)]; vb <- b$lx[match(ages, b$age)]
  k <- which(is.na(va) | is.na(vb) | va != vb)
  f <- function(v) ifelse(is.na(v), "", sprintf("%.0f", v))
  sprintf("%.0f,%s,%s", ages[k], f(va[k]), f(vb[k]))
}
ecarts_declares <- function(v) {
  if (is.na(v)) return(NA_character_)
  l <- trimws(strsplit(v, "\n", fixed = TRUE)[[1L]])
  if (identical(l, "aucun")) character(0) else l
}

# --- contrôle d'un arbre ------------------------------------------------------------

controler <- function(racine, installation = TRUE) {
  racine <- normalizePath(racine)
  cat(sprintf("Tables embarquées (ADR 0018, point 6) : %s\n", racine))
  if (!nzchar(Sys.which("sha256sum"))) ecart("c", "outil sha256sum introuvable : contrôle d'identité impossible (échec, non saut)")
  lib <- tempfile("tablexp_lib_"); dir.create(lib)
  utf8 <- locale_utf8()
  if (is.na(utf8)) { ecart("g", "aucune locale UTF-8 disponible"); return(invisible()) }
  i <- installer(racine, lib, environnement_locale(utf8))
  cat(sprintf("  R CMD INSTALL (LC_ALL=%s) : %s (%s)\n", utf8, if (i$ok) "ok" else "ECHEC", i$duree))
  if (!i$ok) { cat(i$sortie, sep = "\n"); ecart("b", "installation de l'arbre en échec"); return(invisible()) }
  library(tablexp, lib.loc = lib)
  on.exit(try(unloadNamespace("tablexp"), silent = TRUE), add = TRUE)
  decl <- get("DECLARATIONS_TABLES_EMBARQUEES", envir = asNamespace("tablexp"))
  cat(sprintf("  tables déclarées : %d (%s)\n", nrow(decl), if (nrow(decl)) paste(decl$nom, collapse = ", ") else "aucune"))
  catalogue <- tryCatch(tables_embarquees(), refus_tablexp = function(r) r)
  if (inherits(catalogue, "refus_tablexp")) {
    ecart("f", sprintf("tables_embarquees() refuse : %s (%s)", catalogue$motif, conditionMessage(catalogue)))
    catalogue <- NULL
  }

  # (a) complétude : fichiers non rattachés.
  ids <- decl$identifiant
  dans_tables <- list.files(file.path(racine, DOSSIER_TABLES), all.files = TRUE, no.. = TRUE, recursive = TRUE)
  attendus_tables <- c(paste0(ids, ".csv"), paste0(ids, ".provenance.dcf"))
  for (f in setdiff(dans_tables, attendus_tables)) ecart("a", sprintf("%s/%s rattaché à aucune table déclarée", DOSSIER_TABLES, f))
  dans_releves <- list.files(file.path(racine, DOSSIER_RELEVES), all.files = TRUE, no.. = TRUE, recursive = TRUE)
  cites <- character(0)         # chemins cités par les champs *-releve
  empreintes_citees <- character(0)  # chemins d'Empreintes-fichiers, toutes tables

  etats <- list()
  for (k in seq_len(nrow(decl))) {
    nom <- decl$nom[k]; id <- decl$identifiant[k]
    cat(sprintf("  -- %s (%s)\n", nom, id))
    f_table <- sprintf("%s/%s.csv", DOSSIER_TABLES, id)
    f_prov <- sprintf("%s/%s.provenance.dcf", DOSSIER_TABLES, id)
    if (!file.exists(file.path(racine, f_table))) ecart("a", sprintf("%s : fichier de table %s absent", nom, f_table))
    if (!file.exists(file.path(racine, f_prov))) { ecart("a", sprintf("%s : fichier de provenance %s absent", nom, f_prov)); next }

    # (b) format de la table, forme de la provenance.
    tab <- if (file.exists(file.path(racine, f_table))) lire_format(file.path(racine, f_table)) else list(defaut = "absent")
    if (!is.null(tab$defaut)) ecart("b", sprintf("%s : %s : %s", nom, f_table, tab$defaut))
    p <- lire_provenance(file.path(racine, f_prov))
    for (d in p$defauts) ecart("b", sprintf("%s : %s : %s", nom, f_prov, d))
    ch <- p$champs
    n <- names(ch)
    groupes <- unique(sub("(-methode|-releve|-ecarts)$", "", grep("^Seconde-source", n, value = TRUE)))
    groupe_ok <- grepl(sprintf("^Seconde-source(-%s)?$", MOTIF_PIECE), groupes) &
      !groupes %in% paste0("Seconde-source", SUFFIXES_GROUPE)
    for (g in groupes[!groupe_ok]) ecart("b", sprintf("%s : champ de seconde source mal nommé : %s", nom, g))
    groupes <- groupes[groupe_ok]
    if (!length(groupes)) ecart("b", sprintf("%s : aucune seconde source (champs Seconde-source*)", nom))
    connus <- c(CHAMPS_REQUIS, CHAMPS_FACULTATIFS, groupes, outer(groupes, SUFFIXES_GROUPE, paste0))
    for (x in setdiff(n, connus)) ecart("b", sprintf("%s : champ inconnu %s", nom, x))
    requis <- c(CHAMPS_REQUIS, groupes, outer(groupes, SUFFIXES_GROUPE, paste0))
    for (x in setdiff(requis, n)) ecart("b", sprintf("%s : champ requis absent %s", nom, x))
    controle_lecture <- c("Controle-lecture-releve", "Controle-lecture-ecarts", "Controle-lecture-methode")
    if (any(controle_lecture %in% n) && !all(controle_lecture %in% n))
      ecart("b", sprintf("%s : champs Controle-lecture-* incomplets (releve, ecarts et methode vont ensemble)", nom))
    admise <- c("Representation", "Empreinte-table"); refusee <- c("Refus-attendu", "Refus-paires")
    a_admise <- any(admise %in% n); a_refusee <- any(refusee %in% n)
    if (a_admise == a_refusee || (a_admise && !all(admise %in% n)) || (a_refusee && !all(refusee %in% n)))
      ecart("b", sprintf("%s : il faut Representation et Empreinte-table, ou bien Refus-attendu et Refus-paires (point 3.3)", nom))
    egal <- function(champ, attendu)
      if (!identical(valeur(ch, champ), attendu)) ecart("b", sprintf("%s : %s « %s » au lieu de « %s »", nom, champ, valeur(ch, champ), attendu))
    egal("Format", FORMAT_PROVENANCE)
    egal("Nom", nom)
    egal("Fichier", f_table)
    egal("Forme-origine", "lx")
    sexe1 <- strsplit(valeur(ch, "Sexe"), "\n", fixed = TRUE)[[1L]][1L]
    if (!identical(sexe1, decl$sexe[k])) ecart("b", sprintf("%s : Sexe (première ligne) « %s » au lieu de « %s »", nom, sexe1, decl$sexe[k]))
    if (!is.na(valeur(ch, "Visa")) && !grepl(MOTIF_VISA, valeur(ch, "Visa")))
      ecart("b", sprintf("%s : Visa « %s » : « AAAA-MM-JJ, mainteneur, <renvoi au commentaire du visa> » attendu", nom, valeur(ch, "Visa")))
    releves <- c(transcription_b = valeur(ch, "Transcription-B-releve"))
    for (g in groupes) {
      piece <- sub("^Seconde-source-?", "", g)
      releves[[paste0("seconde_source", if (nzchar(piece)) paste0("_", piece) else "")]] <- valeur(ch, paste0(g, "-releve"))
    }
    if ("Controle-lecture-releve" %in% n) releves[["couche_texte"]] <- valeur(ch, "Controle-lecture-releve")
    for (nature in names(releves)) {
      attendu <- sprintf("%s/%s.%s.csv", DOSSIER_RELEVES, id, nature)
      if (!identical(unname(releves[[nature]]), attendu))
        ecart("b", sprintf("%s : relevé %s cité « %s » au lieu de « %s »", nom, nature, releves[[nature]], attendu))
    }
    releves <- releves[!is.na(releves)]
    cites <- c(cites, releves)

    # (c) identité des fichiers.
    lignes_e <- if (is.na(valeur(ch, "Empreintes-fichiers"))) character(0) else strsplit(valeur(ch, "Empreintes-fichiers"), "\n", fixed = TRUE)[[1L]]
    forme_e <- grepl("^[0-9a-f]{64}  [^ ./][^ ]*$", lignes_e) & !grepl("(^|/)\\.\\.(/|$)", substring(lignes_e, 67L))
    for (x in lignes_e[!forme_e]) ecart("c", sprintf("%s : ligne d'Empreintes-fichiers mal formée « %s »", nom, x))
    lignes_e <- lignes_e[forme_e]
    chemins_e <- substring(lignes_e, 67L); attendus_e <- substr(lignes_e, 1L, 64L)
    empreintes_citees <- c(empreintes_citees, chemins_e)
    for (x in setdiff(c(f_table, releves), chemins_e)) ecart("c", sprintf("%s : %s absent d'Empreintes-fichiers", nom, x))
    mesures <- sha256(racine, chemins_e)
    if (!is.null(mesures)) for (j in seq_along(chemins_e)) {
      if (is.na(mesures[j])) ecart("c", sprintf("%s : %s cité dans Empreintes-fichiers, absent de l'arbre", nom, chemins_e[j]))
      else if (mesures[j] != attendus_e[j])
        ecart("c", sprintf("%s : SHA-256 de %s : %s mesuré, %s écrit", nom, chemins_e[j], mesures[j], attendus_e[j]))
      else cat(sprintf("     sha256 ok  %s\n", chemins_e[j]))
    }

    # (d) identité moteur.
    t <- tryCatch(table_embarquee(nom), refus_tablexp = function(r) r)
    ligne_cat <- if (!is.null(catalogue)) catalogue[catalogue$nom == nom, , drop = FALSE] else NULL
    if (a_admise) {
      if (inherits(t, "refus_tablexp"))
        ecart("d", sprintf("%s : objet attendu (Empreinte-table), refus %s levé : %s", nom, t$motif, conditionMessage(t)))
      else if (!is.null(ligne_cat) && nrow(ligne_cat) == 1L) {
        if (!identical(ligne_cat$representation, valeur(ch, "Representation")))
          ecart("d", sprintf("%s : Representation « %s », « %s » dans tables_embarquees()", nom, valeur(ch, "Representation"), ligne_cat$representation))
        if (!identical(ligne_cat$empreinte, valeur(ch, "Empreinte-table")))
          ecart("d", sprintf("%s : empreinte de table %s rendue par le moteur, %s écrite (Empreinte-table)", nom, ligne_cat$empreinte, valeur(ch, "Empreinte-table")))
        else cat(sprintf("     empreinte de table ok  %s (%d lignes, âges %d à %d)\n", ligne_cat$empreinte, nrow(t), min(t$age), max(t$age)))
      }
      etats[[nom]] <- "admise"
    } else if (a_refusee) {
      if (!inherits(t, "refus_tablexp"))
        ecart("d", sprintf("%s : refus %s attendu (Refus-attendu), objet rendu", nom, valeur(ch, "Refus-attendu")))
      else {
        paires <- paste(sprintf("%d-%d", t$details$ages, t$details$ages + 1L), collapse = ", ")
        if (!identical(t$motif, valeur(ch, "Refus-attendu")))
          ecart("d", sprintf("%s : refus %s levé, %s attendu (Refus-attendu)", nom, t$motif, valeur(ch, "Refus-attendu")))
        else if (!identical(paires, valeur(ch, "Refus-paires")))
          ecart("d", sprintf("%s : paires citées « %s », « %s » attendues (Refus-paires)", nom, paires, valeur(ch, "Refus-paires")))
        else cat(sprintf("     refus attendu ok  %s, paires %s\n", t$motif, paires))
      }
      etats[[nom]] <- "refusee"
    }

    # (e) contre-vérification.
    if (is.null(tab$defaut)) for (nature in names(releves)) {
      f <- file.path(racine, releves[[nature]])
      if (!file.exists(f)) { ecart("a", sprintf("%s : relevé %s absent", nom, releves[[nature]])); next }
      r <- lire_format(f)
      if (!is.null(r$defaut)) { ecart("b", sprintf("%s : %s : %s", nom, releves[[nature]], r$defaut)); next }
      if (nature == "transcription_b") {
        if (!identical(r$lignes, tab$lignes)) {
          e <- ecarts_releves(tab, r)
          ecart("e", sprintf("%s : transcription_b différente du fichier de table (%d écart(s) : %s)", nom, length(e), paste(utils::head(e, 5L), collapse = " ; ")))
        } else cat(sprintf("     transcription_b identique (%d lignes)\n", length(r$lignes) - 1L))
        next
      }
      champ <- if (nature == "couche_texte") "Controle-lecture-ecarts"
               else paste0("Seconde-source", if (nature == "seconde_source") "" else paste0("-", sub("^seconde_source_", "", nature)), "-ecarts")
      declares <- ecarts_declares(valeur(ch, champ))
      if (length(declares) == 1L && is.na(declares)) next  # champ absent, signalé en (b)
      constates <- ecarts_releves(tab, r)
      for (x in setdiff(constates, declares)) ecart("e", sprintf("%s : écart %s à %s constaté, non déclaré (%s)", nom, x, nature, champ))
      for (x in setdiff(declares, constates)) ecart("e", sprintf("%s : écart %s à %s déclaré (%s), non constaté", nom, x, nature, champ))
      if (setequal(constates, declares))
        cat(sprintf("     %s : %d écart(s), égaux aux écarts déclarés\n", nature, length(constates)))
    }
  }

  # (a) relevés et scripts rattachés.
  for (f in dans_releves) {
    chemin <- sprintf("%s/%s", DOSSIER_RELEVES, f)
    if (grepl("[.]csv$", f)) {
      if (!chemin %in% cites) ecart("a", sprintf("%s rattaché à aucune table déclarée (cité par aucun champ *-releve)", chemin))
    } else if (!chemin %in% empreintes_citees) ecart("a", sprintf("%s cité par aucun champ Empreintes-fichiers", chemin))
  }

  # (f) catalogue.
  if (!is.null(catalogue)) {
    if (!identical(catalogue$nom, decl$nom)) ecart("f", sprintf("tables_embarquees()$nom « %s », déclaration « %s »", paste(catalogue$nom, collapse = ", "), paste(decl$nom, collapse = ", ")))
    else {
      if (!identical(catalogue$sexe, decl$sexe)) ecart("f", "tables_embarquees()$sexe différent de la déclaration")
      if (!identical(catalogue$source, sprintf("%s/%s.provenance.dcf", DOSSIER_TABLES, decl$identifiant))) ecart("f", "tables_embarquees()$source différent des chemins de provenance")
      for (nom in names(etats)) {
        e <- catalogue$etat[catalogue$nom == nom]
        if (!identical(e, etats[[nom]])) ecart("f", sprintf("%s : état « %s » dans tables_embarquees(), « %s » attendu par sa provenance", nom, e, etats[[nom]]))
      }
    }
  }

  # (g) installation depuis R CMD build, accès après suppression d'extdata.
  if (installation) controler_installation(racine, catalogue, utf8)
  else cat("  (g) installation depuis R CMD build : non mesurée (--sans-installation)\n")
  invisible()
}

# Texte comparable du catalogue et de l'accès à chaque table, sous la session
# courante (aussi exécuté dans les sous-processus de (g)).
releve_acces <- function() {
  t <- tables_embarquees()
  acces <- vapply(t$nom, function(n) {
    x <- tryCatch(table_embarquee(n), refus_tablexp = function(r) r)
    if (inherits(x, "refus_tablexp")) paste("refus", x$motif) else sprintf("objet %d lignes", nrow(x))
  }, "")
  c(sprintf("%s|%s|%s|%s|%s|%s|%s|%s", t$nom, t$sexe, t$forme_origine, t$source, t$etat, t$motif_refus,
            t$representation, t$empreinte), unname(acces))
}

controler_installation <- function(racine, catalogue, utf8) {
  if (is.null(catalogue)) { ecart("g", "catalogue illisible : mesure d'installation sans objet"); return(invisible()) }
  attendu <- releve_acces()
  if (!nrow(catalogue)) cat("  (g) catalogue vide : la mesure porte sur l'arbre témoin (--temoin)\n")
  construction <- tempfile("tablexp_build_"); dir.create(construction)
  t0 <- Sys.time()
  b <- local({
    ancien <- setwd(construction); on.exit(setwd(ancien))
    suppressWarnings(system2(file.path(R.home("bin"), "R"), c("CMD", "build", "--no-build-vignettes", "--no-manual", shQuote(racine)),
                             stdout = TRUE, stderr = TRUE))
  })
  archive <- list.files(construction, pattern = "^tablexp_.*[.]tar[.]gz$", full.names = TRUE)
  cat(sprintf("  (g) R CMD build : %s (%s)\n", if (length(archive) == 1L) basename(archive) else "ECHEC", duree(t0)))
  if (length(archive) != 1L) { cat(b, sep = "\n"); ecart("g", "R CMD build en échec"); return(invisible()) }
  contenu <- utils::untar(archive, list = TRUE)
  for (k in seq_len(nrow(catalogue))) {
    for (f in sprintf("tablexp/%s/%s", DOSSIER_TABLES, c(sub("^.*/", "", catalogue$source[k]),
                                                        sub("[.]provenance[.]dcf$", ".csv", sub("^.*/", "", catalogue$source[k])))))
      if (!f %in% contenu) ecart("g", sprintf("%s absent de l'archive", f))
  }
  for (loc in c("C", utf8)) {
    lib <- tempfile("tablexp_lib_g_"); dir.create(lib)
    # Répertoire courant ailleurs que l'arbre : l'archive est dépaquetée par
    # R CMD INSTALL dans un dossier temporaire.
    i <- local({ ancien <- setwd(tempdir()); on.exit(setwd(ancien)); installer(archive, lib, environnement_locale(loc)) })
    if (!i$ok) { cat(i$sortie, sep = "\n"); ecart("g", sprintf("installation de l'archive sous LC_ALL=%s en échec", loc)); next }
    extdata <- file.path(lib, "tablexp", "extdata")
    if (nrow(catalogue) && !dir.exists(file.path(extdata, "tables"))) ecart("g", sprintf("LC_ALL=%s : extdata/tables non installé", loc))
    unlink(extdata, recursive = TRUE)
    script <- tempfile("acces_", fileext = ".R")
    writeLines(c(sprintf("suppressPackageStartupMessages(library(tablexp, lib.loc = %s))", deparse(lib)),
                 paste(c("releve_acces <- ", deparse(releve_acces, width.cutoff = 500L)), collapse = "\n"),
                 "writeLines(enc2utf8(releve_acces()), stdout(), useBytes = TRUE)"), script)
    r <- avec_environnement(environnement_locale(loc),
                            suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(script), stdout = TRUE, stderr = TRUE)))
    if (!is.null(attr(r, "status")) && attr(r, "status") != 0L) {
      cat(r, sep = "\n"); ecart("g", sprintf("LC_ALL=%s : accès aux tables après suppression d'extdata en échec", loc)); next
    }
    r <- as.character(r)
    Encoding(r) <- "UTF-8"
    if (!identical(r, attendu)) {
      ecart("g", sprintf("LC_ALL=%s : catalogue ou accès après suppression d'extdata différents de l'installation de référence", loc))
      cat("    attendu :", attendu, sep = "\n      "); cat("\n    obtenu :", r, sep = "\n      "); cat("\n")
    } else cat(sprintf("  (g) LC_ALL=%s : archive installée (%s), extdata supprimé, %d table(s) accessible(s), catalogue identique\n",
                       loc, i$duree, nrow(catalogue)))
  }
  invisible()
}

# --- témoin (point 6.2) ------------------------------------------------------------

# Tables d'essai du témoin : l'une admise, l'autre refusée (paire 2-3).
ESSAIS <- list(
  list(nom = "ESSAI ADMISE", identifiant = "essai_admise", sexe = "ensemble", age = 50:55,
       lx = c(90000, 89500, 88900, 88200, 87400, 86500)),
  list(nom = "ESSAI REFUSEE", identifiant = "essai_refusee", sexe = "F", age = 0:4,
       lx = c(100000, 99500, 99400, 99450, 99300)))

ecrire_lignes <- function(lignes, f) { con <- file(f, "wb"); on.exit(close(con)); writeBin(charToRaw(paste0(lignes, "\n", collapse = "")), con) }

preparer_temoin <- function(racine, arbre, empreintes, decl) {
  dir.create(arbre)
  for (x in c("DESCRIPTION", "NAMESPACE", "LICENSE", ".Rbuildignore", "R", "inst", "docs/sources", DOSSIER_RELEVES)) {
    s <- file.path(racine, x)
    if (!file.exists(s)) next
    dir.create(dirname(file.path(arbre, x)), recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(s)) file.copy(s, dirname(file.path(arbre, x)), recursive = TRUE) else file.copy(s, file.path(arbre, x))
  }
  dir.create(file.path(arbre, DOSSIER_TABLES), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(arbre, DOSSIER_RELEVES), recursive = TRUE, showWarnings = FALSE)
  # Déclaration : celle de l'arbre, plus les deux tables d'essai.
  f <- file.path(arbre, "R", "donnees.R")
  s <- readLines(f, encoding = "UTF-8", warn = FALSE)
  debut <- grep("^DECLARATIONS_TABLES_EMBARQUEES <- data.frame\\($", s)
  fin <- debut - 1L + grep("stringsAsFactors = FALSE\\)$", s[debut:length(s)])[1L]
  if (length(debut) != 1L || is.na(fin)) stop("déclaration introuvable dans R/donnees.R de l'arbre témoin")
  q <- function(v) paste0("c(", paste0("\"", v, "\"", collapse = ", "), ")")
  s <- c(s[seq_len(debut - 1L)],
         "DECLARATIONS_TABLES_EMBARQUEES <- data.frame(",
         sprintf("  nom = %s,", q(c(decl$nom, vapply(ESSAIS, `[[`, "", "nom")))),
         sprintf("  identifiant = %s,", q(c(decl$identifiant, vapply(ESSAIS, `[[`, "", "identifiant")))),
         sprintf("  sexe = %s,", q(c(decl$sexe, vapply(ESSAIS, `[[`, "", "sexe")))),
         "  stringsAsFactors = FALSE)", s[-seq_len(fin)])
  writeLines(enc2utf8(s), f, useBytes = TRUE)
  for (k in seq_along(ESSAIS)) {
    e <- ESSAIS[[k]]
    lignes <- c("age,lx", sprintf("%d,%.0f", e$age, e$lx))
    f_table <- sprintf("%s/%s.csv", DOSSIER_TABLES, e$identifiant)
    f_b <- sprintf("%s/%s.transcription_b.csv", DOSSIER_RELEVES, e$identifiant)
    f_s <- sprintf("%s/%s.seconde_source_essai.csv", DOSSIER_RELEVES, e$identifiant)
    ecrire_lignes(lignes, file.path(arbre, f_table))
    ecrire_lignes(lignes, file.path(arbre, f_b))
    seconde <- c(lignes, sprintf("%d,0", max(e$age) + 1L))  # une ligne de plus, déclarée en écart
    ecrire_lignes(seconde, file.path(arbre, f_s))
    sha <- sha256(arbre, c(f_table, f_b, f_s))
    identite <- if (k == 1L) c("Representation: tablexp table 1", sprintf("Empreinte-table: %s", empreintes[[e$nom]]))
                else c("Refus-attendu: table_lx_non_decroissants", "Refus-paires: 2-3")
    prov <- c("Format: tablexp provenance 1", sprintf("Nom: %s", e$nom), sprintf("Fichier: %s", f_table),
              sprintf("Sexe: %s", e$sexe), " table d'essai du témoin", "Forme-origine: lx",
              "Intitule: table d'essai", sprintf("Ages: %d à %d", min(e$age), max(e$age)),
              "Homologation: aucune (témoin)", "Porteurs-usages: aucun (témoin)", "Source: témoin",
              "Source-nature: témoin", "Source-URL: aucune", "Source-consultation: aucune", "Source-licence: aucune",
              "Source-pages: aucune", "Transcription-methode: écrite par le script témoin",
              "Transcription-date: 2026-11-13", sprintf("Transcription-B-releve: %s", f_b),
              "Transcription-B-methode: copie (témoin)", "Seconde-source-essai: témoin",
              "Seconde-source-essai-methode: copie augmentée d'une ligne (témoin)",
              sprintf("Seconde-source-essai-releve: %s", f_s),
              sprintf("Seconde-source-essai-ecarts: %d,,0", max(e$age) + 1L),
              "Controles-coherence: aucun (témoin)", "Ecarts-signales: ligne nulle propre à la seconde source (témoin)",
              "Empreintes-fichiers:", sprintf(" %s  %s", sha, c(f_table, f_b, f_s)),
              "Empreintes-hors-depot: aucune", identite, "Visa: 2026-11-13, mainteneur, témoin du script")
    ecrire_lignes(enc2utf8(prov), file.path(arbre, sprintf("%s/%s.provenance.dcf", DOSSIER_TABLES, e$identifiant)))
  }
  invisible(arbre)
}

temoin <- function(racine) {
  racine <- normalizePath(racine)
  cat(sprintf("Témoin du contrôle des tables embarquées (ADR 0018, point 6.2) : %s\n", racine))
  lib <- tempfile("tablexp_lib_"); dir.create(lib)
  utf8 <- locale_utf8()
  i <- installer(racine, lib, environnement_locale(utf8))
  if (!i$ok) { cat(i$sortie, sep = "\n"); cat("ECHEC : installation de l'arbre\n"); return(FALSE) }
  library(tablexp, lib.loc = lib)
  # Empreinte de table de l'essai admis, calculée par le moteur de l'arbre
  # (même q_x, même sexe que la table qui sera lue dans l'arbre témoin).
  e <- ESSAIS[[1L]]
  t <- table_reference(data.frame(age = e$age, sexe = e$sexe, lx = e$lx, stringsAsFactors = FALSE), e$nom, "embarquee",
                       "s", forme_origine = "lx")
  empreintes <- stats::setNames(list(get("empreinte_table", envir = asNamespace("tablexp"))(t)), e$nom)
  decl <- get("DECLARATIONS_TABLES_EMBARQUEES", envir = asNamespace("tablexp"))
  unloadNamespace("tablexp")
  arbre <- file.path(tempfile("tablexp_temoin_"))
  preparer_temoin(racine, arbre, empreintes, decl)
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1L])
  lancer <- function(args) {
    r <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), args, shQuote(arbre)),
                                  stdout = TRUE, stderr = TRUE))
    list(statut = if (is.null(attr(r, "status"))) 0L else attr(r, "status"), sortie = as.character(r))
  }
  ok <- TRUE
  cat("\n== 1. arbre témoin intact (tables d'essai admise et refusée) : succès attendu, mesure (g) comprise\n")
  r1 <- lancer(character(0)); cat(r1$sortie, sep = "\n")
  if (r1$statut != 0L) { cat("ECHEC du témoin : l'arbre témoin intact est refusé\n"); ok <- FALSE }
  # Un l_x changé d'une unité : première table admise déclarée (table réelle
  # si le catalogue en porte une, sinon l'essai admis), troisième ligne.
  admises <- vapply(c(decl$identifiant, e$identifiant), function(id)
    any(grepl("^Empreinte-table:", readLines(file.path(arbre, DOSSIER_TABLES, paste0(id, ".provenance.dcf")), warn = FALSE))), NA)
  id <- names(admises)[admises][1L]
  f <- file.path(arbre, DOSSIER_TABLES, paste0(id, ".csv"))
  l <- readLines(f)
  ancienne <- l[3L]
  l[3L] <- sprintf("%s,%.0f", sub(",.*$", "", l[3L]), as.double(sub("^.*,", "", l[3L])) + 1)
  ecrire_lignes(l, f)
  cat(sprintf("\n== 2. %s.csv, ligne 3 : « %s » devient « %s » : échec attendu sur (c), (d) et (e)\n", id, ancienne, l[3L]))
  r2 <- lancer("--sans-installation")
  # Échec attendu : sa ligne ::error n'est pas réécrite telle quelle, la CI en
  # ferait une annotation d'erreur.
  cat(sub("^::error", "(échec attendu) error", r2$sortie), sep = "\n")
  manquants <- c("c", "d", "e")[!vapply(c("c", "d", "e"), function(s) any(grepl(sprintf("ECART (%s) ", s), r2$sortie, fixed = TRUE)), NA)]
  if (r2$statut != 1L || length(manquants)) {
    cat(sprintf("ECHEC du témoin : code %d (1 attendu)%s\n", r2$statut,
                if (length(manquants)) sprintf(", aucun écart sur %s", paste(manquants, collapse = ", ")) else ""))
    ok <- FALSE
  }
  # Refus attendu discordant : l_x rétabli, Refus-paires de l'essai refusé
  # changé (« 2-3 » devient « 3-4 »).
  l[3L] <- ancienne
  ecrire_lignes(l, f)
  e2 <- ESSAIS[[2L]]
  f_p <- file.path(arbre, DOSSIER_TABLES, paste0(e2$identifiant, ".provenance.dcf"))
  p <- readLines(f_p, encoding = "UTF-8", warn = FALSE)
  p[p == "Refus-paires: 2-3"] <- "Refus-paires: 3-4"
  ecrire_lignes(p, f_p)
  cat(sprintf("\n== 3. %s.csv rétabli ; %s.provenance.dcf, « Refus-paires: 2-3 » devient « Refus-paires: 3-4 » : échec attendu sur (d)\n",
              id, e2$identifiant))
  r3 <- lancer("--sans-installation")
  cat(sub("^::error", "(échec attendu) error", r3$sortie), sep = "\n")
  ecarts3 <- grep("ECART (", r3$sortie, fixed = TRUE, value = TRUE)
  if (r3$statut != 1L || !length(ecarts3) || !all(grepl(sprintf("ECART (d) %s : ", e2$nom), ecarts3, fixed = TRUE))) {
    cat(sprintf("ECHEC du témoin : code %d (1 attendu), %d écart(s), tous attendus en (d) sur %s\n", r3$statut,
                length(ecarts3), e2$nom))
    ok <- FALSE
  }
  unlink(arbre, recursive = TRUE)
  if (ok) cat(paste("\nTémoin : arbre intact admis ; l_x changé d'une unité détecté sur (c), (d) et (e), code 1 ;",
                    "paires d'un refus attendu discordantes détectées sur (d), code 1.\nOK\n"))
  ok
}

# --- point d'entrée -------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  options_ <- args[startsWith(args, "--")]
  racine_ <- c(args[!startsWith(args, "--")], ".")[1L]
  if ("--temoin" %in% options_) {
    ok_ <- temoin(racine_)
    if (!ok_) { cat("::error title=Tables embarquées::témoin en échec\n"); quit(save = "no", status = 1L) }
  } else {
    controler(racine_, installation = !"--sans-installation" %in% options_)
    if (length(ecarts)) {
      cat(sprintf("\nECHEC : %d écart(s)\n", length(ecarts)))
      cat(sprintf("::error title=Tables embarquées::%d écart(s), dont %s\n", length(ecarts), ecarts[1L]))
      quit(save = "no", status = 1L)
    }
    cat("\nOK : tables embarquées conformes à l'ADR 0018\n")
  }
}
