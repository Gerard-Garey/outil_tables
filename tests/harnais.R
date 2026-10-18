# -----------------------------------------------------------------------------
#  tests/harnais.R : harnais commun des batteries, en R de base, sans testthat
#  (ADR 0007, point 11 ; repris de la forme d'outil_USP)
#
#  Assertions :
#    verifier(nom, expr)              expr vaut TRUE, ou une chaîne qui dit
#                                     l'écart ; toute autre valeur ou une
#                                     erreur est un échec.
#    echec_attendu(nom, issue, expr)  défaut connu, renvoi obligatoire à
#                                     l'issue ("#N") ; expr décrit le
#                                     comportement CORRECT. Tant qu'elle
#                                     échoue : ATTENDU ; si elle passe :
#                                     succès inattendu, la batterie échoue et
#                                     la marque est à retirer (test ordinaire).
#  Outils :
#    proche(a, b, rel, abs)           égalité numérique à tolérance déclarée
#                                     (|a - b| <= abs + rel * |b|), sans défaut.
#    leve_refus(expr, motif)          TRUE si expr lève un refus_tablexp du
#                                     motif donné.
#    sans_execution(x)                retire le champ `execution` (ADR 0007,
#                                     point 6.1) avant identical().
#    installer_tablexp()              installe l'arbre de travail dans une
#                                     bibliothèque temporaire et attache
#                                     tablexp depuis elle.
#    executer_rscript(script, args, env)
#                                     exécute un script dans un sous-processus
#                                     Rscript ; rend le code de sortie et la
#                                     sortie.
#    fichier_courant(nom), bilan(code_sortie)
#
#  Contrôle de fin de batterie : bilan() compte un échec si un espace de noms
#  hors priorité `base` (autre que tablexp) est chargé (ADR 0008,
#  « Conséquences », proposition retenue par la session pour #8).
#
#  Code de sortie de bilan() : 1 si au moins un échec ou un succès inattendu,
#  0 sinon ; bilan(code_sortie = FALSE) n'en change jamais (mode rapport).
#
#  Variable d'environnement TABLEXP_BIBLIOTHEQUE : installer_tablexp() ne
#  réutilise la bibliothèque qu'elle désigne que si l'empreinte de l'arbre
#  source inscrite à l'installation (empreinte_source() : MD5 de
#  DESCRIPTION, NAMESPACE et, récursivement, R/, inst/, data/ et src/ s'ils
#  existent) égale celle de l'arbre courant ; sinon il
#  réinstalle (et le dit). executer_rscript() la pose pour le seul
#  sous-processus, avec la bibliothèque installée par la batterie : une
#  installation par batterie au lieu d'une par sous-processus.
#
#  Locale de l'installation (décision M54 du 17/10/2026) : si la session
#  n'est pas en UTF-8, installer_tablexp() lance R CMD INSTALL sous la
#  première locale UTF-8 disponible parmi LOCALES_UTF8 (testée par
#  Sys.setlocale puis restaurée), passée par system2(env = "LC_ALL=...") ;
#  aucune disponible : ECHEC. La locale d'exécution ne change pas. Après
#  library(), sonde_utf8() vérifie un littéral non ASCII du moteur ; en
#  échec : ECHEC « tablexp installé hors locale UTF-8 » (un paquet installé
#  en locale C garde ses littéraux non marqués, qu'enc2utf8() change en
#  « <c3><a9> » dans une session C : mesuré).
#
#  Garde de sortie (.Last) : un processus qui a chargé le harnais sort avec
#  le code 1 s'il se termine avant bilan(), même par quit(status = 0). Les
#  sorties délibérées du harnais et des batteries (installation en échec,
#  option invalide, bilan en échec) passent runLast = FALSE : leur propre
#  message d'échec n'est pas suivi de celui de la garde.
#
#  echec_attendu(nom, issue, expr, motif = NULL, erreur_quelconque = FALSE) :
#  une erreur qui n'est pas un refus_tablexp (du motif donné) est un ECHEC,
#  sauf erreur_quelconque = TRUE ; bilan() décompte les natures.
# -----------------------------------------------------------------------------

.harnais <- new.env(parent = emptyenv())
.harnais$lignes <- data.frame(fichier = character(0), nom = character(0),
                              statut = character(0), message = character(0),
                              nature = character(0), stringsAsFactors = FALSE)
.harnais$fichier <- "(batterie)"
.harnais$bilan_fait <- FALSE

# Garde de sortie : un processus qui a chargé le harnais ne sort avec le code
# 0 qu'après la ligne de bilan. Un quit() prématuré (y compris base::quit()
# dans un fichier de test) ou une fin de script sans bilan() sort avec 1.
# .Last est exécutée par R à la sortie, normale ou par quit() (mesuré).
.Last <- function() {
  if (!isTRUE(.harnais$bilan_fait)) {
    cat("\nECHEC : sortie du processus avant la ligne de bilan (quit() prématuré ou bilan() absent)\n")
    quit(save = "no", status = 1L, runLast = FALSE)
  }
}

MOTIF_ISSUE <- "^#[0-9]+$"

fichier_courant <- function(nom) {
  .harnais$fichier <- nom
  cat(sprintf("\n== %s\n", nom))
  invisible(nom)
}

.enregistrer <- function(nom, statut, message = "", nature = "") {
  if (!is.character(nom) || length(nom) != 1L || is.na(nom))
    stop("nom d'assertion : une chaîne de longueur 1 est exigée")
  if (!is.character(message) || length(message) != 1L) message <- paste(message, collapse = " ; ")
  i <- nrow(.harnais$lignes) + 1L
  .harnais$lignes[i, ] <- list(.harnais$fichier, nom, statut, message, nature)
  marque <- switch(statut, OK = "ok", ECHEC = "ECHEC", ATTENDU = "attendu",
                   INATTENDU = "INATTENDU")
  if (statut != "OK" || isTRUE(getOption("harnais.verbeux")))
    cat(sprintf("  [%s] %s%s\n", marque, nom,
                if (nzchar(message)) paste0("\n      -> ", message) else ""))
  invisible(statut)
}

# Évalue expr dans l'environnement de l'appelant ; rend list(ok, message,
# erreur, ecart_decrit) ; `erreur` est la condition levée, NULL sinon ;
# `ecart_decrit` vaut TRUE si expr rend FALSE ou une chaîne (non vide), la
# seule forme d'échec par valeur qu'echec_attendu() admet.
.evaluer <- function(expr, env) {
  r <- tryCatch(eval(expr, env), error = function(e) e)
  if (inherits(r, "error"))
    return(list(ok = FALSE, message = paste("erreur :", conditionMessage(r)), erreur = r,
                ecart_decrit = FALSE))
  if (isTRUE(r)) return(list(ok = TRUE, message = "", ecart_decrit = FALSE))
  list(ok = FALSE,
       message = if (is.character(r)) paste(r, collapse = " ; ")
                 else paste("valeur obtenue :",
                            paste(utils::capture.output(utils::str(r)), collapse = " ")),
       ecart_decrit = isFALSE(r) || (is.character(r) && length(r) >= 1L))
}

verifier <- function(nom, expr) {
  r <- .evaluer(substitute(expr), parent.frame())
  .enregistrer(nom, if (r$ok) "OK" else "ECHEC", r$message)
  invisible(r$ok)
}

# Natures d'un échec attendu (colonne `nature`, décomptées par bilan()) :
#   valeur            expr rend FALSE ou une chaîne ; toute autre valeur
#                     (NA, 1, liste...) est un ECHEC, comme pour verifier() :
#                     une expression qui ne rend ni TRUE, ni FALSE, ni un
#                     écart décrit ne dit rien du défaut connu ;
#   refus             expr lève un refus_tablexp (du motif `motif` s'il est
#                     donné) ;
#   erreur_quelconque expr lève une autre erreur, admise seulement si
#                     erreur_quelconque = TRUE.
# Toute autre erreur (erreur qui n'est pas un refus, refus d'un autre motif)
# est un ECHEC : un défaut connu ne masque pas une panne différente.
echec_attendu <- function(nom, issue, expr, motif = NULL, erreur_quelconque = FALSE) {
  if (!is.character(issue) || length(issue) != 1L || is.na(issue) ||
      !grepl(MOTIF_ISSUE, issue)) {
    .enregistrer(nom, "ECHEC",
                 "échec attendu sans renvoi d'issue de la forme \"#N\"")
    return(invisible(FALSE))
  }
  r <- .evaluer(substitute(expr), parent.frame())
  if (r$ok) {
    .enregistrer(nom, "INATTENDU",
                 sprintf("succès inattendu (%s) : le défaut semble corrigé, retirer la marque et en faire un test ordinaire",
                         issue))
    return(invisible(FALSE))
  }
  e <- r$erreur
  if (is.null(e) && !isTRUE(r$ecart_decrit)) {
    .enregistrer(sprintf("%s {%s}", nom, issue), "ECHEC",
                 paste("valeur qui n'est ni TRUE, ni FALSE, ni une chaîne ;", r$message))
    return(invisible(FALSE))
  }
  nature <- if (is.null(e)) "valeur"
    else if (inherits(e, "refus_tablexp") && (is.null(motif) || identical(e$motif, motif))) "refus"
    else if (isTRUE(erreur_quelconque)) "erreur_quelconque"
    else NA_character_
  if (is.na(nature)) {
    .enregistrer(sprintf("%s {%s}", nom, issue), "ECHEC",
                 paste0(if (inherits(e, "refus_tablexp"))
                          sprintf("refus de motif '%s' au lieu de '%s'", e$motif, motif)
                        else "erreur qui n'est pas un refus_tablexp (erreur_quelconque = FALSE)",
                        " ; ", r$message))
    return(invisible(FALSE))
  }
  .enregistrer(sprintf("%s {%s}", nom, issue), "ATTENDU", r$message, nature)
  invisible(TRUE)
}

proche <- function(a, b, rel, abs) {
  if (missing(rel) || missing(abs))
    stop("proche() : tolérances 'rel' et 'abs' à déclarer explicitement.")
  tolerance <- function(t) is.numeric(t) && length(t) == 1L && !is.na(t) && t >= 0
  if (!tolerance(rel) || !tolerance(abs))
    stop("proche() : tolérances 'rel' et 'abs' numériques, uniques, non manquantes et >= 0.")
  # Valeurs numériques (ou logiques) seulement : une chaîne "1" ou un NULL
  # convertis par as.numeric() donneraient une égalité trompeuse.
  numerique <- function(x) (is.numeric(x) || is.logical(x)) && !is.object(x)
  if (!numerique(a) || !numerique(b))
    return(sprintf("valeurs non numériques : %s et %s", paste(class(a), collapse = "/"),
                   paste(class(b), collapse = "/")))
  if (!length(a) && !length(b)) return("deux vecteurs de longueur 0 : rien n'est comparé")
  a <- unname(as.numeric(a)); b <- unname(as.numeric(b))
  if (length(a) != length(b))
    return(sprintf("longueurs différentes : %d et %d", length(a), length(b)))
  if (any(is.nan(a) != is.nan(b))) return("NaN à des positions différentes")
  if (any(is.na(a) != is.na(b))) return("valeurs manquantes (NA) à des positions différentes")
  # Valeurs non finies : égalité exacte exigée (Inf avec Inf, -Inf avec -Inf),
  # avant toute soustraction (Inf - Inf vaut NaN).
  infinis <- !is.na(a) & (is.infinite(a) | is.infinite(b))
  if (any(a[infinis] != b[infinis]))
    return(sprintf("valeur infinie différente : obtenu %s, attendu %s",
                   a[infinis][a[infinis] != b[infinis]][1L], b[infinis][a[infinis] != b[infinis]][1L]))
  k <- !is.na(a) & !infinis
  if (!any(k)) return(TRUE)
  ecart <- base::abs(a[k] - b[k]); limite <- abs + rel * base::abs(b[k])
  if (all(ecart <= limite)) return(TRUE)
  i <- which.max(ecart - limite)
  sprintf("écart max %.3g (obtenu %.17g, attendu %.17g, tolérance %.3g)",
          ecart[i], a[k][i], b[k][i], limite[i])
}

leve_refus <- function(expr, motif) {
  r <- tryCatch({ force(expr); NULL }, refus_tablexp = function(c) c,
                error = function(e) e)
  if (is.null(r)) return("aucun refus levé")
  if (!inherits(r, "refus_tablexp"))
    return(paste("erreur qui n'est pas un refus_tablexp :", conditionMessage(r)))
  if (!identical(r$motif, motif))
    return(sprintf("refus de motif '%s' au lieu de '%s'", r$motif, motif))
  TRUE
}

sans_execution <- function(x) {
  x$execution <- NULL
  x
}

# Racine du dépôt : remonte depuis le script lancé par Rscript (--file=)
# jusqu'au dossier dont DESCRIPTION déclare Package: tablexp.
racine_depot <- function() {
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  d <- if (length(f)) dirname(normalizePath(f[1L])) else normalizePath(getwd())
  repeat {
    desc <- file.path(d, "DESCRIPTION")
    if (file.exists(desc) &&
        identical(unname(read.dcf(desc, fields = "Package")[1L, 1L]), "tablexp"))
      return(d)
    parent <- dirname(d)
    if (identical(parent, d)) stop("racine du dépôt (DESCRIPTION de tablexp) introuvable")
    d <- parent
  }
}

executer_rscript <- function(script, args = character(0), env = character(0)) {
  rscript <- file.path(R.home("bin"), "Rscript")
  if (!is.null(.harnais$bibliotheque) && !any(startsWith(env, "TABLEXP_BIBLIOTHEQUE=")))
    env <- c(env, paste0("TABLEXP_BIBLIOTHEQUE=", .harnais$bibliotheque))
  if (length(env)) {
    # Posées dans ce processus puis restaurées : le sous-processus en hérite
    # (system2(env =) n'est pas pris en charge sous Windows).
    noms <- sub("=.*$", "", env)
    anciens <- Sys.getenv(noms, unset = NA, names = TRUE)
    valeurs <- sub("^[^=]*=", "", env)
    do.call(Sys.setenv, stats::setNames(as.list(valeurs), noms))
    on.exit({
      for (n in noms) if (is.na(anciens[[n]])) Sys.unsetenv(n)
                      else do.call(Sys.setenv, stats::setNames(list(anciens[[n]]), n))
    }, add = TRUE)
  }
  sortie <- suppressWarnings(system2(rscript, c(shQuote(script), args),
                                     stdout = TRUE, stderr = TRUE))
  statut <- attr(sortie, "status")
  list(statut = if (is.null(statut)) 0L else as.integer(statut),
       sortie = as.character(sortie))
}

# Empreinte de l'arbre source installé : MD5 (tools::md5sum) de tout ce que
# R CMD INSTALL installe depuis l'arbre — DESCRIPTION, NAMESPACE et,
# récursivement, R/, inst/, data/, src/ s'ils existent —, chemins relatifs
# compris, triés par octets (radix, indépendant de la locale), résumés en une
# seule MD5.
FICHIER_EMPREINTE_SOURCE <- "tablexp_empreinte_source.txt"
DOSSIERS_INSTALLES <- c("R", "inst", "data", "src")
empreinte_source <- function(racine) {
  dossiers <- DOSSIERS_INSTALLES[dir.exists(file.path(racine, DOSSIERS_INSTALLES))]
  contenus <- unlist(lapply(dossiers, function(d)
    file.path(d, list.files(file.path(racine, d), all.files = TRUE, no.. = TRUE, recursive = TRUE))))
  fichiers <- c("DESCRIPTION", "NAMESPACE", sort(contenus, method = "radix"))
  fichiers <- fichiers[file.exists(file.path(racine, fichiers))]
  md5 <- unname(tools::md5sum(file.path(racine, fichiers)))
  f <- tempfile(); on.exit(unlink(f))
  writeBin(charToRaw(paste0(fichiers, " ", md5, "\n", collapse = "")), f)
  unname(tools::md5sum(f))
}

LOCALES_UTF8 <- c("C.UTF-8", "C.utf8", "en_US.UTF-8")

# NA si la session est déjà en UTF-8 (rien à poser) ; sinon la première
# locale UTF-8 de LOCALES_UTF8 que Sys.setlocale accepte, "" si aucune.
locale_installation <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) return(NA_character_)
  ancienne <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ancienne))
  for (l in LOCALES_UTF8) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) return(l)
  ""
}

# Sonde : STATUT_EMPREINTES_NON_CALCULEES (R/rapport.R, « non calculée »),
# littéral non ASCII du moteur écrit dans la fiche, donc dans les
# références ; il doit être de l'UTF-8 valide, inchangé par enc2utf8().
sonde_utf8 <- function() {
  x <- get("STATUT_EMPREINTES_NON_CALCULEES", envir = asNamespace("tablexp"))
  grepl("[^\001-\177]", x, useBytes = TRUE) && validUTF8(x) &&
    identical(charToRaw(enc2utf8(x)), charToRaw(x))
}

installer_tablexp <- function(racine = racine_depot()) {
  empreinte <- empreinte_source(racine)
  lib <- Sys.getenv("TABLEXP_BIBLIOTHEQUE", "")
  inscrite <- if (nzchar(lib) && file.exists(file.path(lib, FICHIER_EMPREINTE_SOURCE)))
    readLines(file.path(lib, FICHIER_EMPREINTE_SOURCE), warn = FALSE)[1L] else NA_character_
  if (nzchar(lib) && dir.exists(file.path(lib, "tablexp")) && identical(inscrite, empreinte)) {
    cat(sprintf("tablexp : bibliothèque fournie, empreinte de l'arbre identique (TABLEXP_BIBLIOTHEQUE = %s)\n", lib))
  } else {
    if (nzchar(lib))
      cat(sprintf("tablexp : bibliothèque fournie écartée (%s : empreinte %s, arbre %s) ; réinstallation\n",
                  lib, if (is.na(inscrite)) "absente" else inscrite, empreinte))
    lib <- tempfile("tablexp_lib_")
    dir.create(lib)
    locale <- locale_installation()
    if (identical(locale, "")) {
      cat(sprintf("\nECHEC : session hors UTF-8 et aucune locale UTF-8 disponible parmi %s pour R CMD INSTALL\n",
                  paste(LOCALES_UTF8, collapse = ", ")))
      quit(save = "no", status = 1L, runLast = FALSE)  # sortie délibérée : sans la garde .Last
    }
    r <- suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                  c("CMD", "INSTALL", "--no-docs", "-l",
                                    shQuote(lib), shQuote(racine)),
                                  stdout = TRUE, stderr = TRUE,
                                  env = if (is.na(locale)) character(0) else paste0("LC_ALL=", locale)))
    if (!is.null(attr(r, "status")) && attr(r, "status") != 0L) {
      cat(r, sep = "\n")
      cat("\nECHEC : installation de tablexp depuis l'arbre de travail\n")
      quit(save = "no", status = 1L, runLast = FALSE)  # sortie délibérée : sans la garde .Last
    }
    writeLines(empreinte, file.path(lib, FICHIER_EMPREINTE_SOURCE))
    cat(sprintf("tablexp : arbre de travail installé dans %s%s\n", lib,
                if (is.na(locale)) "" else sprintf(" (R CMD INSTALL sous LC_ALL=%s)", locale)))
  }
  library(tablexp, lib.loc = lib)
  chemin <- normalizePath(getNamespaceInfo("tablexp", "path"))
  # dirname() rend des « / » sous Windows, normalizePath() des « \ » : les deux
  # côtés passent par normalizePath().
  if (!identical(normalizePath(dirname(chemin)), normalizePath(lib))) {
    cat(sprintf("ECHEC : tablexp chargé depuis %s, pas depuis %s\n", chemin, lib))
    quit(save = "no", status = 1L, runLast = FALSE)  # sortie délibérée : sans la garde .Last
  }
  if (!isTRUE(sonde_utf8())) {
    cat(sprintf("ECHEC : tablexp installé hors locale UTF-8 (sonde sur STATUT_EMPREINTES_NON_CALCULEES, %s)\n", lib))
    quit(save = "no", status = 1L, runLast = FALSE)  # sortie délibérée : sans la garde .Last
  }
  .harnais$bibliotheque <- lib  # transmise aux sous-processus par executer_rscript()
  invisible(lib)
}

# Espaces de noms chargés hors priorité `base` (tablexp excepté).
espaces_hors_base <- function(ns = loadedNamespaces()) {
  base <- rownames(utils::installed.packages(lib.loc = .Library, priority = "base"))
  sort(setdiff(ns, c(base, "tablexp")))
}

bilan <- function(code_sortie = TRUE) {
  hors_base <- espaces_hors_base()
  .harnais$fichier <- "(fin de batterie)"
  .enregistrer("aucun espace de noms hors priorité base chargé",
               if (length(hors_base)) "ECHEC" else "OK",
               if (length(hors_base)) paste("chargés :", paste(hors_base, collapse = ", ")) else "")
  d <- .harnais$lignes
  n_echec <- sum(d$statut == "ECHEC"); n_inattendu <- sum(d$statut == "INATTENDU")
  cat(sprintf("\nBilan : %s ; %d tests ; %d échec(s) ; %d échec(s) attendu(s) ; %d succès inattendu(s)\n",
              R.version.string, nrow(d), n_echec, sum(d$statut == "ATTENDU"),
              n_inattendu))
  att <- d[d$statut == "ATTENDU", , drop = FALSE]
  if (nrow(att))
    cat(sprintf("Échecs attendus par nature : %d par valeur ; %d par refus ; %d par erreur quelconque\n",
                sum(att$nature == "valeur"), sum(att$nature == "refus"),
                sum(att$nature == "erreur_quelconque")))
  ko <- n_echec + n_inattendu > 0L
  .harnais$bilan_fait <- TRUE
  cat(if (ko) "ECHEC\n" else "OK\n")
  if (ko && code_sortie) quit(save = "no", status = 1L, runLast = FALSE)
  invisible(!ko)
}
