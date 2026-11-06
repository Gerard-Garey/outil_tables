# -----------------------------------------------------------------------------
#  tests/controles/orchestration_locale.R : utilitaire commun des scripts de
#  tests/controles/ (issue #117 ; suite de #66 et de #115 ; issue #123)
#
#  Deux familles de code que se partageaient, copiées, plusieurs contrôles :
#  l'orchestration « installer puis exécuter sous un environnement donné »
#  (locale_c.R, locale_latin1.R) et la recherche et l'appel de l'interpréteur
#  Python du juge indépendant (ecriture_doubles.R, ecriture_doubles_texte.R).
#  Sourcé par chaque contrôle ; ne lance rien, ne fait que définir des
#  fonctions. Un utilitaire de plus d'un contrôle s'ajoute ici plutôt que
#  d'être recopié ; n'ajouter ici que du code sans contenu propre à un
#  contrôle (les jeux, les comparaisons, l'environnement de locale et le script
#  du juge restent dans le contrôle).
#
#    avec_environnement(env, expr)  variables « NOM=valeur » posées le temps de
#                                   l'appel, restaurées ensuite ;
#    duree(depuis)                  durée écoulée, en texte ;
#    installer(racine, lib, env)    R CMD INSTALL de `racine` dans `lib` sous
#                                   l'environnement `env` (vecteur nommé
#                                   complet) ; list(ok, sortie, duree) ;
#    executer(script, racine, lib, env, attendu, sortie)
#                                   `script` relancé en sous-processus
#                                   (--travailleur) sous `env` ; list(ok,
#                                   sortie, duree) ;
#    ecart_octets(a, b)             premier octet différent, avec sa ligne ;
#    trouver_python(candidats, chercher, version_min, delai)
#                                   premier interpréteur Python >= 3.8 parmi
#                                   python3, python, « py -3 » (alias du
#                                   Microsoft Store, chemin contenant
#                                   WindowsApps, écarté ; chaque candidat est
#                                   exécuté pour vérifier sa version) ;
#                                   list(commande, prefixe) ou NULL ;
#    appeler_python(python, args, capturer)
#                                   appel d'un descripteur de trouver_python() ;
#                                   list(statut, sortie), la sortie (standard
#                                   et erreur, capturées si `capturer`) lue
#                                   en UTF-8 ;
#    message_python_absent()        message d'échec commun des contrôles à
#                                   juge Python.
#
#  Chargement par le contrôle : parse(encoding = "UTF-8") puis eval() dans son
#  environnement (le fichier est lu sous la locale du sous-processus, latin-1
#  comprise). Le contrôle le cherche dans le dossier du script lancé, puis dans
#  son sous-dossier controles/ (tests/unitaires/test_controles.R le source
#  depuis tests/test_unitaires.R), puis dans tests/controles/ sous le
#  répertoire courant, et ne retient que le dossier qui contient aussi le
#  contrôle : jamais un homonyme placé à côté du script lanceur.
# -----------------------------------------------------------------------------

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

# Interpréteur Python du juge indépendant (#123). Candidats essayés dans
# l'ordre : python3, python (l'installeur python.org pour Windows ne fournit
# que python.exe), puis le lanceur « py -3 ». Un chemin qui contient WindowsApps
# (alias du Microsoft Store, qui n'est pas un interpréteur ; un vrai Python du
# Store est écarté de même) est écarté sans être exécuté. Le test porte aussi
# sur la forme longue du chemin (normalizePath) : Sys.which() rend sous Windows
# des chemins courts 8.3 (…\MICROS~1\WINDOW~1\python3.exe) où « WindowsApps »
# n'apparaît pas. Tout autre candidat est exécuté (-I) et n'est retenu que si
# sys.version_info >= version_min, un échec d'exécution l'écartant sans erreur ;
# l'exécution est bornée à `delai` secondes (30 par défaut, arrondies à
# l'entier supérieur : system2 ignore une fraction de seconde) ; un candidat qui
# ne rend pas la main (statut 124) est écarté.
# `candidats` : liste de list(nom, prefixe) ; `chercher` : nom -> chemin ou ""
# (Sys.which ; injectable pour les tests). Rend list(commande, prefixe) à passer
# à appeler_python(), ou NULL si aucun candidat ne convient.
trouver_python <- function(candidats = list(list(nom = "python3", prefixe = character(0)),
                                            list(nom = "python", prefixe = character(0)),
                                            list(nom = "py", prefixe = "-3")),
                           chercher = Sys.which, version_min = c(3L, 8L), delai = 30) {
  code <- sprintf("import sys; sys.exit(0 if sys.version_info >= (%d, %d) else 1)", version_min[1L], version_min[2L])
  for (cand in candidats) {
    chemin <- unname(chercher(cand$nom))[1L]
    if (is.na(chemin) || !nzchar(chemin)) next
    # Forme donnée, puis forme longue (chemin inexistant : rendu tel quel, sans avertissement).
    if (grepl("WindowsApps", chemin, ignore.case = TRUE) ||
        grepl("WindowsApps", normalizePath(chemin, winslash = "/", mustWork = FALSE), ignore.case = TRUE)) next
    statut <- tryCatch(suppressWarnings(system2(chemin, c(cand$prefixe, "-I", "-c", shQuote(code)),
                                                stdout = FALSE, stderr = FALSE, timeout = ceiling(delai))),
                       error = function(e) 1L)
    if (identical(as.integer(statut), 0L)) return(list(commande = chemin, prefixe = cand$prefixe))
  }
  NULL
}

# Appel de l'interpréteur décrit par trouver_python() avec les arguments `args`
# (déjà protégés par shQuote si besoin). Sans `capturer`, les sorties sont
# héritées du processus. Avec `capturer`, les sorties standard et d'erreur sont
# rendues dans `sortie`, marquées UTF-8 (le juge s'impose cet encodage : -I écarte
# PYTHONIOENCODING, et Python écrirait sinon dans la page de code ANSI de
# Windows), fins de ligne CRLF ramenées à LF ; octets tels quels sous la locale C.
appeler_python <- function(python, args, capturer = FALSE) {
  r <- suppressWarnings(system2(python$commande, c(python$prefixe, args),
                                stdout = if (capturer) TRUE else "", stderr = if (capturer) TRUE else ""))
  statut <- if (capturer) attr(r, "status") else r
  sortie <- character(0)
  if (capturer) {
    sortie <- sub("\r$", "", as.character(r), useBytes = TRUE)
    if (!all(validUTF8(sortie))) sortie <- iconv(sortie, "UTF-8", "UTF-8", sub = "byte")
    # Sous la locale C d'un poste non Windows, R n'a pas de page où convertir
    # (cat() écrirait « <U+00E9> ») : octets tels quels, comme le texte des
    # contrôles ; ailleurs, marque UTF-8 et R convertit vers la console.
    if (.Platform$OS.type == "windows" || !Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX"))
      Encoding(sortie) <- "UTF-8"
  }
  list(statut = if (is.null(statut)) 0L else as.integer(statut), sortie = sortie)
}

# Message commun des contrôles lorsque trouver_python() rend NULL (octets tels
# quels sous la locale C d'un poste non Windows, comme la sortie du juge).
message_python_absent <- function() {
  m <- "ECHEC : aucun interpréteur Python >= 3.8 (python3, python, py -3 essayés ; alias WindowsApps écarté), le juge indépendant n'est pas disponible.\n"
  if (.Platform$OS.type != "windows" && Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) Encoding(m) <- "unknown"
  m
}
