# -----------------------------------------------------------------------------
#  R/outils.R : services transverses sans contenu métier (ADR 0007, point 1.2)
#
#  - GENERATEUR et sous_graine() : exécution sous graine locale, générateur
#    fixé, état de l'appelant restauré (ADR 0007, point 8) ;
#  - refus_tablexp() et refuser() : un refus est une condition R typée, jamais
#    un objet retourné (ADR 0007, point 6.4).
#
#  Ce fichier n'appelle aucun module (graphe d'appels, ADR 0007, point 2.1 ;
#  contrôlé par tests/controles/graphe_modules.R).
# -----------------------------------------------------------------------------

# Générateur fixé : valeurs par défaut de R 4.3.3 (ADR 0007, point 8.1).
# Tout changement change tout résultat simulé : visa (ADR 0007, Conséquences).
GENERATEUR <- c(kind = "Mersenne-Twister", normal.kind = "Inversion",
                sample.kind = "Rejection")

# Motif d'un code de refus : code court stable, minuscules ASCII et soulignés.
MOTIF_CODE_REFUS <- "^[a-z][a-z0-9_]*$"

# Construit la condition de refus (classe c("refus_tablexp", "error",
# "condition")) sans la lever. module : module qui refuse ; motif : code court
# stable ; message : phrase qui dit ce qui manque (E5.5) ; details : liste
# (rôle, colonne, étape, paramètre, décompte, premiers numéros de ligne).
# Un argument mal formé est une erreur de programmation du package : stop().
refus_tablexp <- function(module, motif, message, details = list()) {
  chaine_unique <- function(x) is.character(x) && length(x) == 1L &&
    !is.na(x) && nzchar(x)
  if (!chaine_unique(module))
    stop("refus_tablexp() : 'module' doit être une chaîne non vide.", call. = FALSE)
  if (!chaine_unique(motif) || !grepl(MOTIF_CODE_REFUS, motif))
    stop("refus_tablexp() : 'motif' doit être un code court (minuscules, chiffres, '_').",
         call. = FALSE)
  if (!chaine_unique(message))
    stop("refus_tablexp() : 'message' doit être une chaîne non vide.", call. = FALSE)
  if (!is.list(details))
    stop("refus_tablexp() : 'details' doit être une liste.", call. = FALSE)
  structure(
    class = c("refus_tablexp", "error", "condition"),
    list(message = message, call = NULL, module = module, motif = motif,
         details = details)
  )
}

# Lève le refus (ne retourne jamais).
refuser <- function(module, motif, message, details = list()) {
  stop(refus_tablexp(module, motif, message, details))
}

# Évalue expr sous la graine `graine` et le générateur GENERATEUR, puis
# restaure l'état de l'appelant (.Random.seed de l'environnement global, ou
# son absence, et RNGkind()), y compris en cas d'erreur dans expr.
# La graine est toujours explicite : aucune valeur par défaut.
# Limite (reprise d'outil_USP) : la réserve normale de Box-Muller d'un
# appelant n'est pas dans .Random.seed et n'est pas restaurable ; sans effet
# sur le moteur, qui tire sous Inversion.
sous_graine <- function(graine, expr) {
  if (missing(graine) || !is.numeric(graine) || length(graine) != 1L ||
      is.na(graine) || !is.finite(graine) || graine != trunc(graine) ||
      abs(graine) > .Machine$integer.max)
    refuser("outils", "graine_invalide",
            "La graine doit être un nombre entier unique, explicite et fini.",
            list(graine = if (missing(graine)) NULL else graine))
  env <- globalenv()
  avait_graine <- exists(".Random.seed", envir = env, inherits = FALSE)
  graine_appelant <- if (avait_graine) env[[".Random.seed"]] else NULL
  reglage_appelant <- RNGkind()
  on.exit({
    # Le réglage d'abord : RNGkind() réécrit .Random.seed.
    RNGkind(kind = reglage_appelant[1L], normal.kind = reglage_appelant[2L],
            sample.kind = reglage_appelant[3L])
    if (avait_graine) {
      assign(".Random.seed", graine_appelant, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(list = ".Random.seed", envir = env)
    }
  }, add = TRUE)
  set.seed(as.integer(graine), kind = GENERATEUR[["kind"]],
           normal.kind = GENERATEUR[["normal.kind"]],
           sample.kind = GENERATEUR[["sample.kind"]])
  expr
}
