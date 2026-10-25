# -----------------------------------------------------------------------------
#  tests/controles/locale_latin1.R : tablexp installé et exécuté sous une
#  locale latin-1 (issue #115 ; ADR 0007, point 9.3 ; suite de #106 et de #66)
#
#  octets_chaines() (R/rapport.R) et ordre_octets(), fonction locale de
#  segments_observations() (R/exposition.R), traitent une session latin-1
#  comme une session UTF-8 (`locale_connue`) : toutes les chaînes passent par
#  enc2utf8(). Aucune batterie ne s'exécute sous une locale latin-1 et
#  locale_c.R ne compare que C et UTF-8 : sans ce contrôle, une régression de
#  cette branche, qui toucherait l'empreinte des données, l'ordre des
#  segments, la fiche ou le rapport figé, passerait la CI.
#
#  Déroulement :
#    1. la locale latin-1 est générée par `localedef -i fr_FR -f ISO-8859-1`
#       dans un dossier temporaire (LOCPATH), sans droits particuliers ; si
#       elle ne peut pas l'être, le contrôle échoue (aucun saut silencieux) ;
#    2. R CMD INSTALL de l'arbre de travail dans deux bibliothèques
#       temporaires : l'une sous cette locale latin-1 (LC_ALL, LANG et
#       LC_CTYPE), l'autre sous la première locale UTF-8 disponible (comme
#       locale_c.R et le harnais) ;
#    3. dans un sous-processus par bibliothèque, sous la locale de son
#       installation, qui vérifie d'abord que la session est bien latin-1
#       (resp. UTF-8) : R1 à R6 (jeu_reference() et plans_reference() de
#       tests/reference/outils_reference.R) puis, pour quatre jeux dont les
#       valeurs de segment non ASCII sont natives non marquées, marquées
#       latin-1, marquées UTF-8, ou mêlées (ÿ natif et € UTF-8 : l'ordre
#       diffère selon que les octets natifs sont traduits ou non),
#       segments_observations() puis construire_table() ; fiche, plan et
#       rapport figé sont écrits, avec l'ordre des segments et les indices de
#       segment de chaque ligne ;
#    4. les textes écrits sous latin-1 et sous UTF-8 sont comparés octet à
#       octet. Aucune différence n'est admise : l'empreinte des données, dans
#       la fiche, et l'ordre des segments ne dépendent pas de la locale. Les
#       jeux se construisent à partir de leurs octets (rawToChar()) avec un
#       marquage explicite : un échappement non ASCII écrit dans un script lu
#       sous latin-1 a donné une chaîne non marquée (mesure de l'issue #115).
#       Dans chaque session, l'ordre des segments est aussi comparé à un
#       ordre de référence calculé ici, par octets UTF-8 (charToRaw), hors du
#       code testé.
#
#  Usage : Rscript tests/controles/locale_latin1.R [racine du dépôt]
#  Code de sortie 1 au moindre écart (message ECHEC sur la sortie standard,
#  ligne ::error sur la CI). Sourcé, le fichier ne fait que définir les
#  fonctions.
# -----------------------------------------------------------------------------

LOCALES_UTF8 <- c("C.UTF-8", "C.utf8", "en_US.UTF-8")  # comme tests/harnais.R
LOCALE_LATIN1 <- "fr_FR.ISO-8859-1"
IDENTIFIANTS <- paste0("R", 1:6)
JEUX_NON_ASCII <- c("natif", "latin1", "utf8", "mixte")

# Messages du sous-processus rendus en ASCII (octets non ASCII en <xx>) : la
# sortie d'une session latin-1 n'est pas de l'UTF-8, et ce fichier est lu
# sous latin-1 par le sous-processus.
ascii <- function(x) iconv(enc2utf8(x), "UTF-8", "ASCII", sub = "byte")

# Première locale UTF-8 que Sys.setlocale accepte, "" si aucune.
locale_utf8_disponible <- function() {
  ancienne <- Sys.getlocale("LC_CTYPE")
  on.exit(Sys.setlocale("LC_CTYPE", ancienne))
  for (l in LOCALES_UTF8) if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) return(l)
  ""
}

# Variables « NOM=valeur » posées dans ce processus le temps de l'appel (le
# sous-processus en hérite) ; restaurées ensuite.
avec_environnement <- function(env, expr) {
  noms <- names(env)
  anciens <- Sys.getenv(noms, unset = NA, names = TRUE)
  do.call(Sys.setenv, as.list(env))
  on.exit(for (n in noms) if (is.na(anciens[[n]])) Sys.unsetenv(n) else
    do.call(Sys.setenv, stats::setNames(list(anciens[[n]]), n)))
  force(expr)
}

environnement_locale <- function(locale, locpath = NULL)
  c(LC_ALL = locale, LANG = locale, LC_CTYPE = locale, if (!is.null(locpath)) c(LOCPATH = locpath))

# Génère la locale latin-1 dans `dossier` (LOCPATH). Rend list(ok, sortie).
generer_locale_latin1 <- function(dossier) {
  r <- tryCatch(suppressWarnings(system2("localedef",
                                         c("-i", "fr_FR", "-f", "ISO-8859-1", shQuote(file.path(dossier, LOCALE_LATIN1))),
                                         stdout = TRUE, stderr = TRUE)),
                error = function(e) structure(conditionMessage(e), status = 127L))
  statut <- attr(r, "status")
  list(ok = (is.null(statut) || statut == 0L) && dir.exists(file.path(dossier, LOCALE_LATIN1)), sortie = r)
}

# --- jeux de segments non ASCII, construits à partir de leurs octets ----------

# Chaîne de la session pour le point de code `cp` (< 256), codage `unknown` :
# octets UTF-8 sous une session UTF-8, octet latin-1 sous une session latin-1.
natif <- function(cp) rawToChar(if (isTRUE(l10n_info()[["UTF-8"]])) charToRaw(intToUtf8(cp)) else as.raw(cp))
marque <- function(octets, codage) { x <- rawToChar(as.raw(octets)); Encoding(x) <- codage; x }
utf8_ <- function(cp) marque(charToRaw(intToUtf8(cp)), "UTF-8")
latin1_ <- function(cp) marque(cp, "latin1")

valeurs_segment <- function(jeu) {
  # ÿ = 0xff, é = 0xe9, à = 0xe0, € = U+20AC (UTF-8 seulement : absent de latin-1)
  switch(jeu,
    natif  = c(natif(0xff), "a", natif(0xe9), natif(0xe0)),
    latin1 = c(latin1_(0xff), "a", latin1_(0xe9), latin1_(0xe0)),
    utf8   = c(utf8_(0xff), "a", utf8_(0xe9), utf8_(0x20ac)),
    mixte  = c(natif(0xff), "a", natif(0xe9), utf8_(0x20ac), latin1_(0xe0)),
    stop("jeu inconnu : ", jeu))
}

# Observations : 60 lignes, trois profils de dates croisés avec les segments.
donnees_segments <- function(seg) {
  D <- as.Date
  base <- data.frame(n = D(c("1960-07-01", "1945-10-15", "1950-01-10")),
                     e = D(c("2020-03-01", "2016-05-20", "2010-06-01")),
                     s = D(c(NA, "2019-02-10", NA)), c = c("en_cours", "deces", "en_cours"),
                     stringsAsFactors = FALSE)
  i <- 0:59
  d <- base[(i %/% 3 + i) %% 3 + 1L, ]
  rownames(d) <- NULL
  d$sg <- rep_len(seg, 60L)
  d
}
ROLES_SEGMENTS <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c", segment = "sg")
plan_segments <- function() {
  list(transition = list(depart = "vivant", arrivee = "deces"),
       periode = list(debut = as.Date("2015-01-01"), fin = as.Date("2024-12-31")),
       delai_declaration = 0, segmentation = "segment",
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "plus_fine", par_annee = FALSE),
                     brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95)))
}

hex_octets <- function(x) vapply(x, function(s) paste(sprintf("%02x", as.integer(charToRaw(s))), collapse = ""),
                                 "", USE.NAMES = FALSE)
# Ordre de référence des segments : octets UTF-8 des valeurs distinctes,
# classés comme des chaînes hexadécimales de même longueur de paire (radix).
ordre_reference <- function(v) { h <- unique(hex_octets(enc2utf8(v))); h[order(h, method = "radix")] }

# --- sous-processus : R1 à R6 et jeux non ASCII --------------------------------

travailleur <- function(args) {
  lib <- args[1L]; attendu <- args[2L]; sortie <- args[3L]; racine <- args[4L]
  library(tablexp, lib.loc = lib)
  for (e in parse(file.path(racine, "tests", "reference", "outils_reference.R"),
                  encoding = "UTF-8", keep.source = FALSE)) eval(e, globalenv())
  info <- l10n_info()
  cat(sprintf("  session : LC_CTYPE = %s ; UTF-8 : %s ; Latin-1 : %s\n",
              Sys.getlocale("LC_CTYPE"), isTRUE(info[["UTF-8"]]), isTRUE(info[["Latin-1"]])))
  conforme <- if (identical(attendu, "latin1")) isTRUE(info[["Latin-1"]]) && !isTRUE(info[["UTF-8"]])
              else isTRUE(info[["UTF-8"]])
  if (!conforme) {
    cat(sprintf("ECHEC : session non %s : le controle ne porterait pas sur la locale visee\n", attendu))
    quit(save = "no", status = 1L)
  }
  defauts <- character(0)
  ecrire_textes <- function(id, x) {
    f <- x$fiche
    tf <- ecrire_fiche(f); tp <- ecrire_plan(f$plan); tr <- ecrire_rapport(x)
    if (!is.raw(tf) || !is.raw(tp) || !is.raw(tr)) stop("texte non ecrit en octets (raw)")
    writeBin(tf, file.path(sortie, paste0(id, ".fiche.txt")))
    writeBin(tp, file.path(sortie, paste0(id, ".plan.txt")))
    writeBin(tr, file.path(sortie, paste0(id, ".rapport.txt")))
    list(fiche = f, tf = tf, tp = tp)
  }
  aller_retour <- function(d, t) {
    if (!identical(lire_fiche(t$tf), t$fiche)) d("fiche : lire_fiche(ecrire_fiche(f)) n'est pas identical() a f")
    if (!identical(lire_plan(t$tp), t$fiche$plan)) d("plan : lire_plan(ecrire_plan(p)) n'est pas identical() au plan")
    if (!identical(ecrire_fiche(lire_fiche(t$tf)), t$tf)) d("fiche : reecriture de la fiche relue differente du texte ecrit")
    if (!identical(ecrire_plan(lire_plan(t$tp)), t$tp)) d("plan : reecriture du plan relu differente du texte ecrit")
  }
  jeu <- jeu_reference()
  for (p in plans_reference()) {
    id <- p$identifiant
    d <- function(m) defauts <<- c(defauts, sprintf("%s : %s", id, ascii(m)))
    r <- tryCatch({
      t <- ecrire_textes(id, construire_table(jeu$donnees, jeu$roles, p$plan))
      aller_retour(d, t)
      NULL
    }, error = function(e) e)
    if (inherits(r, "error")) d(sprintf("erreur [%s] : %s", paste(class(r)[1L], collapse = "/"), conditionMessage(r)))
    cat(sprintf("  %s : %s\n", id, if (any(startsWith(defauts, paste0(id, " :")))) "ECHEC" else "ok"))
  }
  for (j in JEUX_NON_ASCII) {
    id <- paste0("seg_", j)
    d <- function(m) defauts <<- c(defauts, sprintf("%s : %s", id, ascii(m)))
    r <- tryCatch({
      v <- valeurs_segment(j)
      # Segments et indices de segment (segments_observations() directement).
      sg <- tablexp:::segments_observations(data.frame(segment = rep_len(v, 60L), stringsAsFactors = FALSE), "segment")
      ordre <- hex_octets(enc2utf8(sg$segments$segment))
      if (!identical(ordre, ordre_reference(v))) d(sprintf("ordre des segments (octets UTF-8) %s, attendu %s",
                                                           paste(ordre, collapse = " "), paste(ordre_reference(v), collapse = " ")))
      writeBin(c(unlist(lapply(enc2utf8(sg$segments$segment), function(s) c(charToRaw(s), as.raw(10L)))),
                 charToRaw(paste0(paste(sg$g, collapse = " "), "\n"))),
               file.path(sortie, paste0(id, ".segments.txt")))
      t <- ecrire_textes(id, construire_table(donnees_segments(v), ROLES_SEGMENTS, plan_segments()))
      aller_retour(d, t)
      NULL
    }, error = function(e) e)
    if (inherits(r, "error")) d(sprintf("erreur [%s] : %s", paste(class(r)[1L], collapse = "/"), conditionMessage(r)))
    cat(sprintf("  %s : %s\n", id, if (any(startsWith(defauts, paste0(id, " :")))) "ECHEC" else "ok"))
  }
  if (length(defauts)) { cat(paste0("  DEFAUT ", defauts, "\n"), sep = ""); quit(save = "no", status = 1L) }
  invisible(TRUE)
}

# --- orchestration ------------------------------------------------------------

duree <- function(depuis) sprintf("%.1f s", as.numeric(difftime(Sys.time(), depuis, units = "secs")))

installer <- function(racine, lib, env) {
  t0 <- Sys.time()
  r <- avec_environnement(env,
    suppressWarnings(system2(file.path(R.home("bin"), "R"),
                             c("CMD", "INSTALL", "--no-docs", "-l", shQuote(lib), shQuote(racine)),
                             stdout = TRUE, stderr = TRUE)))
  statut <- attr(r, "status")
  list(ok = is.null(statut) || statut == 0L, sortie = r, duree = duree(t0))
}

executer <- function(script, racine, lib, env, attendu, sortie) {
  t0 <- Sys.time()
  r <- avec_environnement(env,
    suppressWarnings(system2(file.path(R.home("bin"), "Rscript"),
                             c(shQuote(script), "--travailleur", shQuote(lib), attendu, shQuote(sortie), shQuote(racine)),
                             stdout = TRUE, stderr = TRUE)))
  statut <- attr(r, "status")
  list(ok = is.null(statut) || statut == 0L, sortie = r, duree = duree(t0))
}

# Premier octet différent, avec le numéro de ligne (LF) : message d'écart.
ecart_octets <- function(a, b) {
  n <- min(length(a), length(b))
  k <- which(a[seq_len(n)] != b[seq_len(n)])[1L]
  if (is.na(k)) k <- n + 1L
  ligne <- 1L + sum(a[seq_len(k - 1L)] == as.raw(10L))
  sprintf("longueurs %d et %d octets ; premier octet différent : %d (ligne %d)", length(a), length(b), k, ligne)
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L]))
  if (length(args) && identical(args[1L], "--travailleur")) return(travailleur(args[-1L]))
  racine <- normalizePath(if (length(args)) args[1L] else dirname(dirname(dirname(script))))
  t_debut <- Sys.time()
  defauts <- character(0)
  echec <- function() {
    cat(paste0("  ECHEC : ", defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Locale latin-1::", paste(gsub("\n", " ", defauts), collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("tablexp sous une locale latin-1 (installation et exécution) :", racine, "\n")
  utf8 <- locale_utf8_disponible()
  if (!nzchar(utf8)) {
    defauts <- sprintf("aucune locale UTF-8 disponible parmi %s : comparaison impossible", paste(LOCALES_UTF8, collapse = ", "))
    echec()
  }
  tmp <- tempfile("locale_latin1_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  t0 <- Sys.time()
  locpath <- file.path(tmp, "locales"); dir.create(locpath)
  g <- generer_locale_latin1(locpath)
  cat(sprintf("  localedef -i fr_FR -f ISO-8859-1 : %s (%s)\n", if (g$ok) "ok" else "ECHEC", duree(t0)))
  if (!g$ok) {
    cat(g$sortie, sep = "\n")
    defauts <- "locale latin-1 non générée par localedef (le contrôle ne saute pas : installer le paquet locales)"
    echec()
  }
  cas <- list(list(nom = LOCALE_LATIN1, attendu = "latin1", env = environnement_locale(LOCALE_LATIN1, locpath)),
              list(nom = utf8, attendu = "utf8", env = environnement_locale(utf8)))
  for (k in seq_along(cas)) {
    cas[[k]]$lib <- file.path(tmp, paste0("lib", k)); cas[[k]]$sortie <- file.path(tmp, paste0("sortie", k))
    dir.create(cas[[k]]$lib); dir.create(cas[[k]]$sortie)
  }
  for (c_ in cas) {
    i <- installer(racine, c_$lib, c_$env)
    cat(sprintf("  R CMD INSTALL sous LC_ALL=%s : %s (%s)\n", c_$nom, if (i$ok) "ok" else "ECHEC", i$duree))
    if (!i$ok) { cat(i$sortie, sep = "\n"); defauts <- c(defauts, sprintf("installation sous LC_ALL=%s en échec", c_$nom)) }
  }
  if (length(defauts)) echec()
  for (c_ in cas) {
    cat(sprintf("  R1 à R6 et jeux de segments non ASCII (%s), sous LC_ALL=%s :\n", paste(JEUX_NON_ASCII, collapse = ", "), c_$nom))
    e <- executer(script, racine, c_$lib, c_$env, c_$attendu, c_$sortie)
    cat(paste0("  ", e$sortie), sep = "\n")
    cat(sprintf("  durée : %s\n", e$duree))
    if (!e$ok) defauts <- c(defauts, sprintf("sous-processus sous LC_ALL=%s en échec (voir ci-dessus)", c_$nom))
  }
  if (length(defauts)) echec()
  # Comparaison octet à octet : textes écrits sous latin-1 contre sous UTF-8.
  noms <- c(sprintf("%s.%s.txt", rep(IDENTIFIANTS, each = 3L), c("fiche", "plan", "rapport")),
            sprintf("seg_%s.%s.txt", rep(JEUX_NON_ASCII, each = 4L), c("fiche", "plan", "rapport", "segments")))
  n_comparees <- 0L
  for (nom in noms) {
    f <- file.path(vapply(cas, `[[`, "", "sortie"), nom)
    if (!all(file.exists(f))) { defauts <- c(defauts, sprintf("%s : texte absent (%s)", nom, paste(vapply(cas, `[[`, "", "nom")[!file.exists(f)], collapse = ", "))); next }
    a <- readBin(f[1L], "raw", file.size(f[1L])); b <- readBin(f[2L], "raw", file.size(f[2L]))
    n_comparees <- n_comparees + 1L
    if (!length(a) || !identical(a, b))
      defauts <- c(defauts, sprintf("%s : texte écrit sous %s différent de celui écrit sous %s (%s)", nom, LOCALE_LATIN1, utf8, ecart_octets(a, b)))
  }
  if (n_comparees != length(noms) && !length(defauts)) defauts <- "nombre de textes comparés inattendu"
  if (length(defauts)) echec()
  cat(sprintf("  OK : %d textes (fiche, plan et rapport figé de %s et de %d jeux de segments non ASCII, ordre des segments) identiques octet à octet sous %s et sous %s ; aller-retour identical() ; durée totale %s.\n",
              n_comparees, paste(range(IDENTIFIANTS), collapse = " à "), length(JEUX_NON_ASCII), LOCALE_LATIN1, utf8, duree(t_debut)))
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
