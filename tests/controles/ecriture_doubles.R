# -----------------------------------------------------------------------------
#  tests/controles/ecriture_doubles.R : recoupement de l'écriture des doubles
#  en forme « empreinte » (octets IEEE 754 en hexadécimal) avec un juge
#  indépendant, Python (struct.pack('<d')) (ADR 0011, points 2.1 et 2.4 ;
#  issue #35)
#
#  Hors batterie (durée, dépendance à Python) : la batterie unitaire garde un
#  échantillon déterministe de 10^5 doubles dont l'empreinte (MD5 du texte
#  attendu) a été produite par ce même juge (tests/unitaires/
#  test_construire_table.R).
#
#  Méthode. R tire n doubles sous graine locale (générateur fixé, sous_graine)
#  — motifs de 64 bits tirés au hasard (un motif non fini est remplacé par 0.5) et valeurs de
#  runif() transformées par des opérations IEEE exactes —, plus des cas
#  limites construits octet par octet (zéros, sous-normaux, plus petit
#  normal, maximum, puissances de deux), plus NA, NaN, Inf, -Inf. Il les
#  passe à Python en %.17g (arrondi correct de la glibc ; la bibliothèque C
#  de Windows n'arrondit pas toujours correctement, #35 : ce contrôle n'est
#  donc pas exécuté sous Windows). Python lit chaque texte (float(), arrondi
#  correct), calcule x + 0.0, puis les seize hexadécimaux de
#  struct.pack('<d', y).hex() ; les quatre jetons passent tels quels. Le texte
#  de R (ecrire_valeurs(x, "empreinte", ...)) doit être identique ligne à ligne.
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/controles/ecriture_doubles.R [n]      (n = 1e6 par défaut)
#  Code de sortie 1 si un écart ou si aucun Python >= 3.8 n'est trouvé
#  (python3, python, py -3 : trouver_python(), orchestration_locale.R).
# -----------------------------------------------------------------------------

ARGS <- commandArgs(trailingOnly = TRUE)
N <- if (length(ARGS)) as.numeric(ARGS[1L]) else 1e6
GRAINE <- 20261019L

f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
RACINE <- if (length(f)) normalizePath(file.path(dirname(f[1L]), "..", "..")) else getwd()
moteur <- new.env()
for (m in c("outils.R", "rapport.R")) sys.source(file.path(RACINE, "R", m), moteur)

# Orchestration commune (trouver_python, appeler_python, message_python_absent :
# tests/controles/orchestration_locale.R, #117 et #123), chargée dans
# l'environnement courant. Dossiers cherchés : celui du script lancé (Rscript,
# --file=), son sous-dossier controles/, puis tests/controles/ sous le
# répertoire courant ; n'est retenu que celui qui contient aussi ce contrôle :
# on charge son voisin, jamais un homonyme placé à côté du script lanceur. Lue en
# UTF-8 quelle que soit la locale.
for (e_ in parse(encoding = "UTF-8", keep.source = FALSE, file = local({
  controle_ <- "ecriture_doubles.R"
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))[1L]
  d <- c(if (!is.na(f)) c(dirname(f), file.path(dirname(f), "controles")), file.path("tests", "controles"))
  d <- d[file.exists(file.path(d, controle_))]
  if (!length(d) || !file.exists(file.path(d[1L], "orchestration_locale.R")))
    stop("orchestration_locale.R introuvable à côté de ", controle_)
  file.path(d[1L], "orchestration_locale.R")
}))) eval(e_, environment())
rm(e_)

if (.Platform$OS.type == "windows") {
  cat("Contrôle non exécuté sous Windows (%.17g de la bibliothèque C, #35).\n")
  quit(status = 1L)
}
python <- trouver_python()
if (is.null(python)) {
  cat(message_python_absent())
  quit(status = 1L)
}

# Échantillon (même recette que la batterie unitaire, n = 1e5 : MD5 du texte
# attendu écrit en dur dans tests/unitaires/test_construire_table.R).
echantillon_doubles <- function(n, graine) moteur$sous_graine(graine, {
  m <- n %/% 2L
  octets <- as.raw(sample.int(256L, 8L * m, replace = TRUE) - 1L)
  brut <- readBin(octets, "double", n = m, size = 8L, endian = "little")
  brut[!is.finite(brut)] <- 0.5
  u <- runif(n - m)
  c(brut, u[seq_len((n - m) %/% 2L)] * 1e6 - 5e5,
    1 / u[-seq_len((n - m) %/% 2L)])
})

depuis_octets <- function(...) readBin(as.raw(c(...)), "double", n = 1L, size = 8L, endian = "little")
CAS_LIMITES <- c(0, -0, 1, -1, 0.1, 0.95, 1e-9, 1 / 3, pi, 2^53, 2^53 + 2, 2^-1022, 2^1023,
                 depuis_octets(1, 0, 0, 0, 0, 0, 0, 0), depuis_octets(1, 0, 0, 0, 0, 0, 0, 0x80),
                 depuis_octets(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x0f, 0),
                 depuis_octets(0, 0, 0, 0, 0, 0, 0x10, 0),
                 depuis_octets(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xef, 0x7f),
                 depuis_octets(0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xef, 0xff),
                 NA_real_, NA_real_ + 1, NaN, 0 / 0, Inf, -Inf, .Machine$double.xmax)

x <- c(CAS_LIMITES, echantillon_doubles(as.integer(N), GRAINE))
cat(sprintf("%d doubles (dont %d cas limites)\n", length(x), length(CAS_LIMITES)))

entree <- tempfile("doubles_", fileext = ".txt"); sortie <- tempfile("hex_", fileext = ".txt")
script <- tempfile("juge_", fileext = ".py")
# Nettoyage explicite : on.exit() est sans effet au niveau supérieur d'un
# Rscript, et quit() ne l'exécuterait pas ; toute sortie après la création des
# fichiers temporaires passe par sortir().
sortir <- function(statut) {
  unlink(c(entree, sortie, script))
  quit(status = statut)
}
writeLines(sprintf("%.17g", x), entree)
writeLines(c("import struct, sys",
             "sys.stdout.reconfigure(encoding='utf-8', line_buffering=True); sys.stderr.reconfigure(encoding='utf-8', line_buffering=True)",
             "jetons = ('NA', 'NaN', 'Inf', '-Inf')",
             "with open(sys.argv[1]) as e, open(sys.argv[2], 'w') as s:",
             "    for l in e:",
             "        l = l.rstrip('\\n')",
             "        s.write((l if l in jetons else struct.pack('<d', float(l) + 0.0).hex()) + '\\n')"),
           script)
t_juge <- system.time(statut <- appeler_python(python, c("-I", shQuote(c(script, entree, sortie))))$statut)[["elapsed"]]
if (statut != 0L) { cat("ECHEC : le juge Python a échoué.\n"); sortir(1L) }
attendu <- readLines(sortie)

t_r <- system.time(obtenu <- strsplit(rawToChar(moteur$ecrire_valeurs(x, "empreinte", "x")$octets), "\n", fixed = TRUE)[[1L]])[["elapsed"]]
ecarts <- which(obtenu != attendu)
cat(sprintf("Écriture R : %.2f s ; juge Python : %.2f s ; %s\n", t_r, t_juge,
            appeler_python(python, "--version", capturer = TRUE)$sortie))
# MD5 du texte attendu de l'échantillon seul (sans les cas limites) : pour
# N = 1e5, la valeur écrite en dur dans la batterie unitaire.
ech <- tempfile("echantillon_"); writeBin(charToRaw(paste0(attendu[-seq_along(CAS_LIMITES)], "\n", collapse = "")), ech)
cat(sprintf("MD5 du texte attendu de l'échantillon (juge Python) : %s\n", unname(tools::md5sum(ech)))); unlink(ech)
cat(sprintf("Lignes : %d (R) contre %d (Python) ; écarts : %d\n", length(obtenu), length(attendu), length(ecarts)))
if (length(obtenu) != length(attendu) || length(ecarts)) {
  for (i in head(ecarts, 10L)) cat(sprintf("  ligne %d : %.17g -> R %s, Python %s\n", i, x[i], obtenu[i], attendu[i]))
  sortir(1L)
}
cat("OK\n")
sortir(0L)
