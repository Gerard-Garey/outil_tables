# -----------------------------------------------------------------------------
#  tests/controles/commits_empreinte_code.R : retrouver le commit d'une fiche
#  par l'empreinte de son code (ADR 0011, annotation du 27/10/2026, (e),
#  point 11.4 ; issue #89 ; M126, M166), en R de base, hors de R/
#
#  Une fiche porte fiche$empreintes$code (MD5 des sources R/*.R, option (iv),
#  M70). Ce script la recalcule dans l'historique git, sur les blobs, avec la
#  formule et la garde d'empreinte_sources() (R/rapport.R) et de
#  STATUT_EMPREINTE_CODE :
#    - fichiers de premier niveau de R/ dont le nom finit par « .R » (ni
#      cachés, ni dossiers), triés par octets ;
#    - MD5 de chaque fichier ; relevé « <md5><2 espaces>R/<fichier> », une
#      ligne par fichier, LF ; MD5 du relevé ;
#    - garde : un commit sans DESCRIPTION de « Package: tablexp », ou sans
#      R/rapport.R, n'a pas d'empreinte (NA) ; il n'est jamais deviné.
#  Il ne charge pas tablexp : son recalcul recoupe empreinte_sources().
#
#  Usage (depuis la racine du dépôt ; --racine <dossier> pour une autre) :
#      Rscript tests/controles/commits_empreinte_code.R historique <empreinte>
#      Rscript tests/controles/commits_empreinte_code.R arbre
#      Rscript tests/controles/commits_empreinte_code.R references
#
#  Modes
#    historique <empreinte> : <empreinte> = 32 hexadécimaux minuscules (refus
#        sinon, avant tout appel à git). Parcourt git rev-list --all ; liste,
#        du plus ancien au plus récent, les commits dont R/*.R a cette
#        empreinte : SHA complet, date de validation, sujet. Calcul sur les
#        blobs (git cat-file), indépendant de l'arbre de travail. refs/stash
#        est exclue du parcours : un code remisé (git stash) n'est pas commité.
#    arbre : empreinte des R/*.R de la racine courante, sans git (test de
#        batterie : égale à EMPREINTE_CODE installée). Une ligne : l'empreinte.
#    references : hors batteries et hors CI. Pour chaque tests/reference/*.tsv,
#        lit « # fiche$empreintes/code » et « # commit » de l'en-tête et
#        vérifie que ce commit est dans la classe de cette empreinte.
#
#  Codes de sortie (distincts)
#    0  commits trouvés / empreinte rendue / références concordantes ;
#    1  refus nommé SEULEMENT (argument, git absent, racine hors d'un dépôt,
#       sources introuvables, aucun .tsv) ;
#    2  conclusion négative : aucun commit sur un historique complet (mode
#       historique : le code était non commité, ou sa branche est absente du
#       clone), ou écart (mode références : commit absent d'un historique
#       complet, ambigu, empreinte différente de l'en-tête, en-tête
#       inexploitable ; un écart sur un commit présent d'un clone superficiel
#       est concluant aussi) ;
#    3  NON DÉTERMINABLE : aucun commit sur un clone superficiel, ou commit
#       d'un .tsv absent d'un clone superficiel ; ou aucun commit (resp.
#       empreinte différente), alors qu'au moins un commit lu n'a pas
#       d'empreinte parce qu'une entrée de R/ n'est pas un fichier ordinaire
#       (lien symbolique, sous-module, dossier nommé *.R) : ces commits sont
#       nommés. Une absence n'est jamais concluante sur un historique tronqué
#       ni devant un commit non évaluable. Mode historique, clone superficiel :
#       une liste de commits est valable mais peut être incomplète (dit).
#    Mode références : écart (2) avant non déterminable (3).
#
#  Limites (ADR 0011, annotation du 27/10/2026, (e) 4 à 6)
#    - Une empreinte désigne une CLASSE de commits : tout commit qui ne touche
#      pas R/*.R garde l'empreinte de son parent. La classe identifie le code R,
#      non le package entier : DESCRIPTION, NAMESPACE ou inst/ peuvent différer
#      entre deux commits d'une classe (hypothèse, non mesurée, qu'aucun
#      résultat n'en dépend) ; lire l'écart par git diff entre deux commits.
#    - Un commit perdu (branche non fusionnée et supprimée, historique réécrit)
#      rend sa fiche introuvable : le script le dit, il ne le répare pas.
#    - Équivalence blob / fichier extrait : .gitattributes impose eol=lf ; un
#      arbre extrait avec d'autres fins de ligne donnerait une autre empreinte.
#    - Les fichiers R/ qui ne sont pas des fichiers ordinaires (lien
#      symbolique, sous-module, dossier nommé *.R) rendent NA (rien n'est
#      deviné).
#    - Les noms de R/ sont lus par git ls-tree -z, en octets, sans guillemets
#      (git cite sinon les noms à ", \, tabulation ou saut de ligne).
# -----------------------------------------------------------------------------

ARGS <- commandArgs(trailingOnly = TRUE)

sortie_avec <- function(statut, ...) {
  cat(..., sep = "")
  quit(save = "no", status = statut)
}
refus <- function(...) sortie_avec(1L, "REFUS : ", ..., "\n")

# Arguments : mode, argument éventuel, --racine <dossier>.
racine_option <- NULL
if ("--racine" %in% ARGS) {
  i <- match("--racine", ARGS)
  if (i == length(ARGS)) refus("--racine demande un dossier")
  racine_option <- ARGS[i + 1L]
  ARGS <- ARGS[-c(i, i + 1L)]
}
MODES <- c("historique", "arbre", "references")
if (!length(ARGS) || !ARGS[1L] %in% MODES)
  refus("mode attendu : ", paste(MODES, collapse = ", "), " (historique <empreinte> ; arbre ; references)")
MODE <- ARGS[1L]
if (MODE == "historique") {
  if (length(ARGS) != 2L || !grepl("^[0-9a-f]{32}$", ARGS[2L]))
    refus("le mode historique demande une empreinte de 32 caractères hexadécimaux minuscules")
  EMPREINTE <- ARGS[2L]
} else if (length(ARGS) != 1L) refus("le mode ", MODE, " ne prend pas d'argument")

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
RACINE <- if (!is.null(racine_option)) racine_option else
  if (length(f)) file.path(dirname(f[1L]), "..", "..") else getwd()
if (!dir.exists(RACINE)) refus("racine introuvable : ", RACINE)
RACINE <- normalizePath(RACINE, winslash = "/")

# --- formule (R/rapport.R, empreinte_sources()) -------------------------------

md5_texte <- function(texte) {
  t <- tempfile(); on.exit(unlink(t))
  writeBin(charToRaw(texte), t)
  unname(tools::md5sum(t))
}
releve_md5 <- function(md5, noms) md5_texte(paste0(md5, "  R/", noms, "\n", collapse = ""))

# Mode « arbre » : NA si sources introuvables ou fichier illisible.
empreinte_arbre <- function(racine) {
  description <- file.path(racine, "DESCRIPTION")
  paquet <- if (file.exists(description))
    tryCatch(read.dcf(description, fields = "Package")[1L, 1L], error = function(e) NA_character_)
  if (!identical(unname(paquet), "tablexp") || !file.exists(file.path(racine, "R", "rapport.R")))
    return(NA_character_)
  fichiers <- sort(list.files(file.path(racine, "R"), pattern = "[.]R$"), method = "radix")
  md5 <- unname(tools::md5sum(file.path(racine, "R", fichiers)))
  if (anyNA(md5)) return(NA_character_)
  releve_md5(md5, fichiers)
}

if (MODE == "arbre") {
  e <- empreinte_arbre(RACINE)
  if (is.na(e)) refus("sources introuvables sous ", RACINE, " (DESCRIPTION de « Package: tablexp », R/rapport.R, fichiers R/*.R lisibles)")
  sortie_avec(0L, e, "\n")
}

# --- git ------------------------------------------------------------------------

if (!nzchar(Sys.which("git"))) refus("git absent : les modes historique et references lisent l'historique")
git <- function(args, input = NULL, fichier = NULL) {
  r <- suppressWarnings(system2("git", c("-C", shQuote(RACINE), args),
                                stdout = if (is.null(fichier)) TRUE else fichier,
                                stderr = FALSE, input = input))
  st <- attr(r, "status")
  list(sortie = if (is.null(fichier)) as.character(r) else character(0),
       statut = if (is.null(st)) 0L else as.integer(st))
}
dessus <- git(c("rev-parse", "--show-toplevel"))
if (dessus$statut != 0L || !length(dessus$sortie))
  refus("racine hors d'un dépôt git : ", RACINE)
if (!identical(normalizePath(dessus$sortie[1L], winslash = "/"), RACINE))
  refus("la racine doit être la racine du dépôt git (", dessus$sortie[1L], ") : ", RACINE)
SUPERFICIEL <- identical(git(c("rev-parse", "--is-shallow-repository"))$sortie, "true")

# Lecture de git cat-file --batch : « <oid> <type> <taille>\n<contenu>\n » par
# objet, dans l'ordre demandé ; rend la liste des octets (NULL si absent).
lire_blobs <- function(oids) {
  if (!length(oids)) return(list())
  t <- tempfile(); on.exit(unlink(t))
  r <- git(c("cat-file", "--batch"), input = oids, fichier = t)
  if (r$statut != 0L) refus("git cat-file --batch a échoué")
  tampon <- readBin(t, "raw", file.size(t))
  n <- length(tampon); pos <- 1L
  res <- stats::setNames(vector("list", length(oids)), oids)
  for (oid in oids) {
    if (pos > n) refus("sortie de git cat-file tronquée")
    fin <- pos - 1L + which(tampon[pos:min(pos + 200L, n)] == as.raw(10L))[1L]
    entete <- strsplit(rawToChar(tampon[pos:(fin - 1L)]), " ", fixed = TRUE)[[1L]]
    if (length(entete) == 3L && identical(entete[2L], "blob")) {
      taille <- as.integer(entete[3L])
      res[oid] <- list(if (taille) tampon[(fin + 1L):(fin + taille)] else raw(0))
      pos <- fin + taille + 2L
    } else pos <- fin + 1L
  }
  res
}

# Empreinte de chaque commit de rev-list --all, sur les blobs. data.frame
# (sha, empreinte), du plus ancien au plus récent (parents avant enfants).
empreintes_historique <- function() {
  # --exclude=refs/stash avant --all : le remisage (git stash) crée des commits
  # « WIP on … » qui portent du code non commité.
  rl <- git(c("rev-list", "--exclude=refs/stash", "--all", "--topo-order", "--reverse"))
  if (rl$statut != 0L || !length(rl$sortie)) refus("git rev-list --all n'a rendu aucun commit")
  commits <- rl$sortie
  oid_de <- function(suffixe) {
    r <- git(c("cat-file", "--batch-check"), input = paste0(commits, suffixe))
    if (r$statut != 0L || length(r$sortie) != length(commits)) refus("git cat-file --batch-check a échoué")
    p <- strsplit(r$sortie, " ", fixed = TRUE)
    vapply(p, function(x) if (length(x) == 3L && x[2L] == (if (suffixe == ":R") "tree" else "blob")) x[1L]
                          else NA_character_, "")
  }
  arbre_r <- oid_de(":R")
  desc <- oid_de(":DESCRIPTION")

  # Contenu de chaque arbre R/ distinct : (mode, type, oid, nom) des entrées.
  arbres <- unique(stats::na.omit(arbre_r))
  entrees <- stats::setNames(lapply(arbres, function(a) {
    # -z : entrées « <mode> <type> <oid>\t<nom> » séparées par NUL, noms bruts
    # (ni guillemets ni échappements) lus en octets.
    t <- tempfile()
    l <- git(c("ls-tree", "-z", a), fichier = t)
    if (l$statut != 0L) refus("git ls-tree a échoué sur ", a)
    tampon <- readBin(t, "raw", file.size(t)); unlink(t)
    fins <- c(0L, which(tampon == as.raw(0L)))
    if (length(tampon) && fins[length(fins)] != length(tampon)) refus("sortie de git ls-tree -z tronquée sur ", a)
    morceaux <- lapply(seq_len(length(fins) - 1L), function(k) tampon[(fins[k] + 1L):(fins[k + 1L] - 1L)])
    lire <- function(x) {
      tab <- which(x == as.raw(9L))[1L]
      if (is.na(tab)) refus("entrée illisible de git ls-tree sur ", a)
      champs <- strsplit(rawToChar(x[seq_len(tab - 1L)]), " ", fixed = TRUE)[[1L]]
      if (length(champs) != 3L) refus("entrée illisible de git ls-tree sur ", a)
      c(champs, rawToChar(x[-seq_len(tab)]))
    }
    m <- lapply(morceaux, lire)
    d <- data.frame(mode = vapply(m, `[`, "", 1L), type = vapply(m, `[`, "", 2L),
                    oid = vapply(m, `[`, "", 3L), nom = vapply(m, `[`, "", 4L))
    d <- d[grepl("[.]R$", d$nom, useBytes = TRUE) & !grepl("^[.]", d$nom, useBytes = TRUE), , drop = FALSE]
    d[order(d$nom, method = "radix"), , drop = FALSE]
  }), arbres)
  ordinaire <- function(d) d$type == "blob" & d$mode %in% c("100644", "100755")

  descriptions <- unique(stats::na.omit(desc))
  a_lire <- unique(c(descriptions, unlist(lapply(entrees, function(d) d$oid[ordinaire(d)]))))
  contenus <- lire_blobs(a_lire)

  paquet <- stats::setNames(vapply(descriptions, function(o) {
    p <- tryCatch(read.dcf(textConnection(rawToChar(contenus[[o]])), fields = "Package")[1L, 1L],
                  error = function(e) NA_character_)
    if (is.na(p)) NA_character_ else unname(p)
  }, ""), descriptions)

  # Tout blob entrée de R/ est haché, même s'il est aussi une DESCRIPTION : un
  # même oid (même contenu) peut être l'un et l'autre.
  entrees_r <- unique(unlist(lapply(entrees, function(d) d$oid[ordinaire(d)])))
  fichiers <- character(length(a_lire))
  for (i in seq_along(a_lire)) {
    if (is.null(contenus[[a_lire[i]]]) || !a_lire[i] %in% entrees_r) next
    fichiers[i] <- tempfile(); writeBin(contenus[[a_lire[i]]], fichiers[i])
  }
  md5 <- stats::setNames(rep(NA_character_, length(a_lire)), a_lire)
  ok <- nzchar(fichiers)
  md5[ok] <- unname(tools::md5sum(fichiers[ok]))
  unlink(fichiers[ok])

  par_arbre <- vapply(entrees, function(d) {
    # Garde : R/rapport.R fichier ordinaire ; tout *.R non ordinaire ou illisible : NA.
    if (!nrow(d) || !any(d$nom == "rapport.R" & ordinaire(d)) || !all(ordinaire(d))) return(NA_character_)
    m <- unname(md5[d$oid])
    if (anyNA(m)) NA_character_ else releve_md5(m, d$nom)
  }, "")
  # Commit non évaluable : paquet tablexp, R/rapport.R présent, mais une entrée
  # de R/ qui n'est pas un fichier ordinaire (NA qui n'est pas celui de la garde).
  suspect_arbre <- vapply(entrees, function(d) any(d$nom == "rapport.R") && any(!ordinaire(d)), NA)
  e <- unname(par_arbre[arbre_r])
  garde <- !is.na(desc) & unname(paquet[desc]) %in% "tablexp"
  e[!garde | is.na(desc)] <- NA_character_
  indetermine <- garde & unname(suspect_arbre[arbre_r]) %in% TRUE
  data.frame(sha = commits, empreinte = e, indetermine = indetermine)
}

metadonnees <- function(shas) {
  r <- git(c("show", "-s", "--format=%H%x09%cI%x09%s", shas))
  if (r$statut != 0L) refus("git show a échoué")
  r$sortie
}

H <- empreintes_historique()
INDETERMINES <- H$sha[H$indetermine]
phrase_indetermines <- function() sprintf("%d commit(s) lu(s) sans empreinte à cause d'une entrée de R/ qui n'est pas un fichier ordinaire (lien symbolique, sous-module, dossier *.R) : %s%s",
  length(INDETERMINES), paste(utils::head(INDETERMINES, 10L), collapse = ", "),
  if (length(INDETERMINES) > 10L) sprintf(" et %d autre(s)", length(INDETERMINES) - 10L) else "")
NOTE_CLASSE <- paste("Une empreinte désigne une classe de commits : ceux qui ne touchent pas R/*.R gardent celle de leur parent.",
                     "La classe identifie le code R, non le package entier (DESCRIPTION, NAMESPACE, inst/ peuvent différer).")
NOTE_SUPERFICIEL <- sprintf("ATTENTION : clone superficiel (%d commit(s) lus) ; %s Pour l'historique complet : git fetch --unshallow.",
  nrow(H), if (MODE == "historique") "la liste est valable mais peut être incomplète."
           else "un commit absent est non déterminable (code 3) ; un écart sur un commit présent reste concluant (code 2).")

if (MODE == "historique") {
  trouves <- H$sha[!is.na(H$empreinte) & H$empreinte == EMPREINTE]
  if (!length(trouves)) {
    if (SUPERFICIEL)
      sortie_avec(3L, "NON DÉTERMINABLE : aucun commit lu n'a l'empreinte ", EMPREINTE, ", mais le clone est superficiel (",
                  nrow(H), " commit(s) lus) : une absence n'est pas concluante. Récupérer l'historique complet (git fetch --unshallow).\n")
    if (length(INDETERMINES))
      sortie_avec(3L, "NON DÉTERMINABLE : aucun commit lu n'a l'empreinte ", EMPREINTE, ", mais ", phrase_indetermines(), ".\n")
    sortie_avec(2L, "aucun commit des références locales n'a l'empreinte ", EMPREINTE,
                " : le code était non commité, ou sa branche est absente du clone.\n")
  }
  cat(sprintf("empreinte %s : %d commit(s) sur %d lus, du plus ancien au plus récent (SHA, date, sujet)\n",
              EMPREINTE, length(trouves), nrow(H)))
  l <- metadonnees(trouves)
  l <- l[order(match(sub("\t.*$", "", l), trouves))]
  writeLines(sub("\t", "  ", sub("\t", "  ", l, fixed = TRUE), fixed = TRUE), useBytes = TRUE)
  cat(NOTE_CLASSE, "\n", sep = "")
  if (SUPERFICIEL) cat(NOTE_SUPERFICIEL, "\n", sep = "")
  if (length(INDETERMINES)) cat("ATTENTION : ", phrase_indetermines(), " ; la liste peut être incomplète.\n", sep = "")
  quit(save = "no", status = 0L)
}

# --- mode références ------------------------------------------------------------

tsvs <- sort(list.files(file.path(RACINE, "tests", "reference"), pattern = "[.]tsv$", full.names = TRUE))
if (!length(tsvs)) refus("aucun tests/reference/*.tsv sous ", RACINE)
ecart <- FALSE; indeterminable <- FALSE
for (t in tsvs) {
  tete <- readLines(t, n = 100L, warn = FALSE, encoding = "UTF-8")
  tete <- tete[cumsum(!startsWith(tete, "#")) == 0L]
  code <- sub("^# fiche\\$empreintes/code : ", "", grep("^# fiche\\$empreintes/code : [0-9a-f]{32}$", tete, value = TRUE))
  commit <- sub("^# commit : ", "", grep("^# commit : [0-9a-f]{7,40}$", tete, value = TRUE))
  nom <- basename(t)
  if (length(code) != 1L || length(commit) != 1L) {
    cat(sprintf("ECART %s : en-tête sans « # fiche$empreintes/code » ou sans « # commit » exploitable\n", nom)); ecart <- TRUE; next
  }
  i <- which(startsWith(H$sha, commit))
  if (length(i) != 1L) {
    if (SUPERFICIEL) {
      cat(sprintf("NON DÉTERMINABLE %s : commit %s absent d'un clone superficiel\n", nom, commit)); indeterminable <- TRUE
    } else {
      cat(sprintf("ECART %s : commit %s %s\n", nom, commit,
                  if (length(i)) "ambigu" else "absent de l'historique des références locales")); ecart <- TRUE
    }
    next
  }
  if (identical(H$empreinte[i], code)) {
    cat(sprintf("OK %s : le commit %s est dans la classe de l'empreinte %s\n", nom, substr(H$sha[i], 1L, 7L), code))
  } else {
    if (H$indetermine[i]) {
      cat(sprintf("NON DÉTERMINABLE %s : le commit %s n'a pas d'empreinte (entrée de R/ qui n'est pas un fichier ordinaire)\n",
                  nom, substr(H$sha[i], 1L, 7L))); indeterminable <- TRUE
    } else {
      cat(sprintf("ECART %s : le commit %s a l'empreinte %s, l'en-tête dit %s\n", nom, substr(H$sha[i], 1L, 7L),
                  if (is.na(H$empreinte[i])) "NA" else H$empreinte[i], code)); ecart <- TRUE
    }
  }
}
if (SUPERFICIEL) cat(NOTE_SUPERFICIEL, "\n", sep = "")
if (indeterminable && length(INDETERMINES)) cat("NON DÉTERMINABLE : ", phrase_indetermines(), ".\n", sep = "")
quit(save = "no", status = if (ecart) 2L else if (indeterminable) 3L else 0L)
