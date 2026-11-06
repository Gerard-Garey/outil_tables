# -----------------------------------------------------------------------------
#  tests/controles/orchestration_locale.R : utilitaire commun des scripts de
#  tests/controles/ (issue #117 ; suite de #66 et de #115)
#
#  Orchestration « installer puis exécuter sous un environnement donné », que
#  partageaient, copiée, locale_c.R et locale_latin1.R. Sourcé par chaque
#  contrôle (locale_c.R, locale_latin1.R) ; ne lance rien, ne fait que définir
#  des fonctions. Un utilitaire de plus d'un contrôle s'ajoute ici plutôt que
#  d'être recopié ; n'ajouter ici que du code sans contenu propre à un
#  contrôle (les jeux, les comparaisons et l'environnement de locale restent
#  dans le contrôle).
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
#    ecart_octets(a, b)             premier octet différent, avec sa ligne.
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
