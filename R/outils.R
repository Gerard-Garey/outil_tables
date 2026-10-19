# -----------------------------------------------------------------------------
#  R/outils.R : services transverses sans contenu métier (ADR 0007, point 1.2)
#
#  - GENERATEUR et sous_graine() : exécution sous graine locale, générateur
#    fixé, état de l'appelant restauré (ADR 0007, point 8) ;
#  - refus_tablexp() et refuser() : un refus est une condition R typée, jamais
#    un objet retourné (ADR 0007, point 6.4) ;
#  - avertissements_vides() et avertir() : les avertissements métier sont
#    inscrits dans l'objet, jamais émis par warning() (ADR 0007, point 6.5) ;
#  - forme_exacte() : type de base exact, aucun attribut hors de ceux admis
#    (forme canonique des valeurs du plan, #23) ;
#  - liste_aux_noms() et chaine_unique() : les deux contrôles de forme qui s'y
#    appuient, partagés par les modules (#48).
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

# Forme exacte d'une valeur reçue (#23) : TRUE si son type de base (typeof)
# est pris dans `types` et si elle ne porte aucun attribut hors des noms
# d'attributs `attributs` (aucun par défaut : ni names, ni dim, ni classe, ni
# attribut quelconque). Un attribut admis n'est pas requis, et sa valeur n'est
# pas contrôlée : c'est l'affaire de l'appelant, qui refuse aussi sous son
# propre motif. Rend TRUE ou FALSE, jamais NA.
forme_exacte <- function(x, types, attributs = character(0)) {
  typeof(x) %in% types && all(names(attributes(x)) %in% attributs)
}

# Liste nue aux noms exactement `noms` (#48) : forme_exacte() de type list, seul
# attribut names (ni data.frame, ni classe), autant d'éléments que de noms, et
# mêmes noms une fois triés par sort(method = "radix"), indépendant de la
# locale. La longueur est contrôlée à part : sort() retire un nom NA, qui
# passerait sinon avec un élément de plus. as.character() : sort(NULL) est une
# erreur, une liste sans noms doit être refusée (FALSE), pas lever. Les
# éléments se lisent par [[ ]] (jamais $, qui apparie les noms partiels). Rend
# TRUE ou FALSE ; le contenu des éléments est l'affaire de l'appelant.
liste_aux_noms <- function(x, noms) {
  forme_exacte(x, "list", "names") && length(x) == length(noms) &&
    identical(sort(as.character(names(x)), method = "radix"), sort(noms, method = "radix"))
}

# Chaîne unique (#48) : forme exacte character (aucun attribut, names compris),
# longueur 1, ni NA ni vide. Rend TRUE ou FALSE, jamais NA.
chaine_unique <- function(v) {
  forme_exacte(v, "character") && length(v) == 1L && !is.na(v) && nzchar(v)
}

# Accumulateur d'avertissements métier (ADR 0007, points 6.1 et 6.5) :
# data.frame (etape, code, message, nombre), restitué dans l'objet. avertir()
# rend l'accumulateur augmenté d'une ligne ; aucun warning() n'est émis.
# nombre : dénombrement, stocké en integer (#24) ; une valeur non entière, NA,
# infinie, négative, supérieure à .Machine$integer.max ou de longueur
# différente de 1 est une erreur de programmation du package : stop(), sans
# conversion.
avertissements_vides <- function() {
  data.frame(etape = character(0), code = character(0), message = character(0),
             nombre = integer(0), stringsAsFactors = FALSE)
}

avertir <- function(acc, etape, code, message, nombre) {
  if (!is.character(code) || length(code) != 1L || !grepl(MOTIF_CODE_REFUS, code))
    stop("avertir() : 'code' doit être un code court (minuscules, chiffres, '_').",
         call. = FALSE)
  if (!is.numeric(nombre) || length(nombre) != 1L || is.na(nombre) ||
      !is.finite(nombre) || nombre != trunc(nombre) ||
      nombre < 0 || nombre > .Machine$integer.max)
    stop("avertir() : 'nombre' doit être un entier unique, fini, positif ou nul.", call. = FALSE)
  rbind(acc, data.frame(etape = etape, code = code, message = message,
                        nombre = as.integer(nombre), stringsAsFactors = FALSE))
}

# Évalue expr sous la graine `graine` et le générateur GENERATEUR, puis
# restaure l'état de l'appelant (.Random.seed de l'environnement global, ou
# son absence, et RNGkind()), y compris en cas d'erreur dans expr.
# La graine est toujours explicite : aucune valeur par défaut.
# Limite (reprise d'outil_USP) : la réserve normale de Box-Muller d'un
# appelant n'est pas dans .Random.seed et n'est pas restaurable ; sans effet
# sur le moteur, qui tire sous Inversion.
sous_graine <- function(graine, expr) {
  # Forme exacte (#43, forme_exacte()) : integer ou double, aucun attribut (ni
  # names, ni dim, ni classe) ; une graine non canonique ne doit pas être
  # consignée.
  if (missing(graine) || !forme_exacte(graine, c("double", "integer")) || length(graine) != 1L ||
      is.na(graine) || !is.finite(graine) || graine != trunc(graine) ||
      abs(graine) > .Machine$integer.max)
    refuser("outils", "graine_invalide",
            "La graine doit être un nombre entier unique, explicite et fini, sans attribut.",
            list(graine = if (missing(graine)) NULL else graine))
  env <- globalenv()
  avait_graine <- exists(".Random.seed", envir = env, inherits = FALSE)
  graine_appelant <- if (avait_graine) env[[".Random.seed"]] else NULL
  reglage_appelant <- RNGkind()
  on.exit({
    # Le réglage d'abord : RNGkind() réécrit .Random.seed. suppressWarnings :
    # rétablir sample.kind = "Rounding" d'un appelant émet l'avertissement R
    # « non-uniform 'Rounding' sampler used », qui ne concerne que lui.
    suppressWarnings(RNGkind(kind = reglage_appelant[1L], normal.kind = reglage_appelant[2L],
                             sample.kind = reglage_appelant[3L]))
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
