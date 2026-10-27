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
#    appuient, partagés par les modules (#48) ;
#  - noms_complets() : la règle des noms complets, écrite une seule fois et
#    partagée par les modules et l'orchestration (#22, notes de B6) ;
#  - noms_bytes() : positions des noms d'encodage « bytes » d'un objet (#114) ;
#  - texte_moteur() : texte produit par le moteur marqué UTF-8, quelle que
#    soit la locale d'installation ou d'exécution (#50) ;
#  - neutraliser_citation() et citation_a_neutraliser() : règle unique de
#    neutralisation d'une valeur citée dans un message de refus (caractères
#    de contrôle et séparateurs de ligne, encodage ; #94).
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
# mêmes noms, sans NA ni doublon, comparés sans tri. Pas de sort() : le tri
# radix lève « Character encoding must be UTF-8, Latin-1 or bytes » sur un nom
# non ASCII d'encodage `unknown` ou aux octets invalides, selon sa position
# (#100). Précondition : noms attendus ASCII (tous les appelants) ; la
# comparaison ne dépend alors pas de la locale et ne lève pas, quel que soit
# l'encodage d'un nom reçu (natif, octets invalides, bytes : FALSE). Avec des
# noms attendus non ASCII, setequal() passe par match(), qui traduit les
# encodages : le résultat peut dépendre de la locale, et un nom marqué `bytes`
# lève sous C.UTF-8 (mesuré par l'audit). La longueur est contrôlée à part ;
# avec NA et doublons écartés, setequal() donne l'égalité des ensembles de
# noms. as.character() : une liste sans noms (names(x) NULL) donne
# character(0), que setequal() refuse (FALSE) si des noms sont attendus. Les
# éléments se lisent par [[ ]] (jamais $, qui apparie les noms partiels). Rend
# TRUE ou FALSE ; le contenu des éléments est l'affaire de l'appelant.
liste_aux_noms <- function(x, noms) {
  forme_exacte(x, "list", "names") && length(x) == length(noms) && {
    n <- as.character(names(x))
    !anyNA(n) && !anyDuplicated(n) && setequal(n, noms)
  }
}

# Chaîne unique (#48) : forme exacte character (aucun attribut, names compris),
# longueur 1, ni NA ni vide. Rend TRUE ou FALSE, jamais NA.
chaine_unique <- function(v) {
  forme_exacte(v, "character") && length(v) == 1L && !is.na(v) && nzchar(v)
}

# Noms complets (#60, #62 ; #22, notes de B6) : présents (non NULL), sans NA et
# non vides. Prédicat sur les noms seuls, `names(x)` : la forme de l'objet, les
# doublons et la liste vide sans noms sont l'affaire de l'appelant. Rend TRUE ou
# FALSE, jamais NA.
noms_complets <- function(noms) !is.null(noms) && !anyNA(noms) && all(nzchar(noms))

# Positions des noms d'encodage « bytes » de x (#114) : integer(0) si x n'a pas
# de noms ou aucun de ce codage. Une lecture par nom, x[["champ"]] ou
# x$champ, lève « translating strings with "bytes" encoding is not allowed »
# dès qu'elle rencontre un tel nom placé avant le nom cherché ou à sa place
# (mesuré, #111, #114) : l'appelant qui lit par nom un objet reçu contrôle
# d'abord ses noms, pour refuser sous un motif nommé. Encoding() ne traduit
# rien et ne lève pas. Les noms se lisent par attr(x, "names", exact = TRUE) et
# non par names() : names() est un générique interne qui dispatche sur la
# classe de x, et lève la même erreur sur une classe d'encodage « bytes »
# (rejouer_fiche(data.frame(a = 1), structure(list(a = 1), class = b)), mesuré
# sous C et C.UTF-8) ; attr() ne dispatche pas. Les appelants qui citent ou
# restituent les noms repérés les lisent de même. Rend un integer, jamais NA.
noms_bytes <- function(x) {
  n <- attr(x, "names", exact = TRUE)
  if (is.null(n)) integer(0) else which(Encoding(n) == "bytes")
}

# Accumulateur d'avertissements métier (ADR 0007, points 6.1 et 6.5) :
# data.frame (etape, code, message, nombre), restitué dans l'objet. avertir()
# rend l'accumulateur augmenté d'une ligne ; aucun warning() n'est émis.
# nombre : dénombrement, stocké en integer (#24) ; une valeur non entière, NA,
# infinie, négative, supérieure à .Machine$integer.max ou de longueur
# différente de 1 est une erreur de programmation du package : stop(), sans
# conversion ; de même un code qui n'est pas une chaîne unique de forme exacte
# (chaine_unique() : ni nom, ni attribut, ni NA ; #60).
avertissements_vides <- function() {
  data.frame(etape = character(0), code = character(0), message = character(0),
             nombre = integer(0), stringsAsFactors = FALSE)
}

avertir <- function(acc, etape, code, message, nombre) {
  if (!chaine_unique(code) || !grepl(MOTIF_CODE_REFUS, code))
    stop("avertir() : 'code' doit être un code court (minuscules, chiffres, '_').",
         call. = FALSE)
  if (!is.numeric(nombre) || length(nombre) != 1L || is.na(nombre) ||
      !is.finite(nombre) || nombre != trunc(nombre) ||
      nombre < 0 || nombre > .Machine$integer.max)
    stop("avertir() : 'nombre' doit être un entier unique, fini, positif ou nul.", call. = FALSE)
  rbind(acc, data.frame(etape = etape, code = code, message = texte_moteur(message),
                        nombre = as.integer(nombre), stringsAsFactors = FALSE))
}

# Texte produit par le moteur (littéraux du package, et ce que paste() ou
# sprintf() en tirent), marqué UTF-8 quelle que soit la locale d'installation
# ou d'exécution (#50). Les sources sont en UTF-8 (DESCRIPTION, Encoding) ;
# installé sous une locale UTF-8, le package a ses littéraux non ASCII marqués
# UTF-8, mais paste() et sprintf() en rendent du natif non marqué sous une
# locale UTF-8 ; installé sous la locale C, il garde leurs octets UTF-8 sans
# marque (mesuré, #50). Une chaîne non ASCII non marquée dont les octets sont de
# l'UTF-8 valide est donc marquée UTF-8 ; une chaîne marquée latin-1 est
# convertie par enc2utf8() ; une autre est laissée telle quelle (l'écriture la
# refuse ensuite). x : character, ou liste (data.frame compris) dont les
# feuilles character sont traitées ; noms traités aussi ; attributs conservés.
# Réservé au texte du moteur : jamais aux données, aux rôles ni au plan de
# l'utilisateur, dont un codage indéterminé reste refusé (module rapport).
texte_moteur <- function(x) {
  if (is.list(x)) {
    x[] <- lapply(x, texte_moteur)
  } else if (is.character(x)) {
    latin1 <- Encoding(x) == "latin1"
    if (any(latin1)) x[latin1] <- enc2utf8(x[latin1])
    inconnu <- !is.na(x) & Encoding(x) == "unknown" & grepl("[^\001-\177]", x, useBytes = TRUE) & validUTF8(x)
    if (any(inconnu)) {
      y <- x[inconnu]
      Encoding(y) <- "UTF-8"
      x[inconnu] <- y
    }
  }
  if (!is.null(names(x))) names(x) <- texte_moteur(names(x))
  x
}

# Valeur citée dans un message de refus (#94) : règle unique de
# neutralisation, appliquée à chaque élément d'un vecteur (texte des refus,
# jamais des valeurs écrites ni empreintées ; la valeur exacte reste dans les
# `details` du refus). Ensemble neutralisé : catégories Unicode Cc, Zl et Zp
# (67 points de code, dont U+0000, qu'aucune chaîne R ne contient) : U+0000 à
# U+001F (C0), U+007F (DEL), U+0080 à U+009F (C1, dont NEL), U+2028 et
# U+2029, points de code d'une chaîne UTF-8 (une chaîne latin-1 est d'abord
# convertie, voir plus bas) ; il contient tous les séparateurs de ligne
# (LF, VT, FF, CR, U+001C à U+001E, NEL, U+2028, U+2029) : un message
# neutralisé tient sur une ligne. Les caractères de format (Cf : U+200B à
# U+200F, U+202A à U+202E, U+2066 à U+2069, U+FEFF) ne sont pas neutralisés :
# ils ne coupent pas une ligne et sont légitimes dans certains noms (ZWJ,
# ZWNJ) ; un contrôle bidirectionnel peut réordonner l'affichage d'une valeur
# citée (limite acceptée). Par élément :
#   - NA reste NA (l'appelant l'écrit) ;
#   - une chaîne d'encodage « bytes » devient le repère « <chaîne d'encodage
#     bytes> », sans réinterprétation de ses octets (sprintf() lèverait
#     « translating strings with "bytes" encoding is not allowed », #111) ;
#   - une chaîne marquée latin-1 est convertie par enc2utf8(), qui la lit
#     selon CP1252 depuis R 3.5.0 (?Encoding, Details ; mesuré sous C et
#     C.UTF-8) : les octets 0x80 à 0x9F y deviennent des caractères
#     imprimables (€, …, œ, Œ, ’…), non des C1, et ne sont pas neutralisés ;
#     les cinq positions non définies (0x81, 0x8D, 0x8F, 0x90, 0x9D) sont
#     écrites <81>… par enc2utf8(), comme un octet invalide et avec le même
#     sens ; une chaîne marquée UTF-8, ou d'encodage `unknown` aux octets
#     UTF-8 valides, est lue en UTF-8 (convention de texte_moteur()) ; chaque
#     caractère de l'ensemble s'écrit <U+XXXX> (quatre chiffres hexadécimaux
#     majuscules : l'ensemble est dans le premier plan) ;
#   - une chaîne qui n'est pas de l'UTF-8 valide est citée octet par octet :
#     un octet >= 0x80 s'écrit <xx> (hexadécimal minuscule), un octet C0 ou
#     DEL <U+00XX>, les autres tels quels.
# Notations de ?iconv (R 4.3.3) : <xx> pour sub = "byte", <U+xxxx> pour
# sub = "Unicode". Travail sur les octets (charToRaw(), utf8ToInt()) : même
# sortie sous les locales C et UTF-8. Le résultat est marqué UTF-8 s'il
# contient un caractère non ASCII. Limite acceptée : « < » n'est pas échappé,
# un « <U+000A> » tapé tel quel se cite comme un LF.
neutraliser_citation <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  repere <- texte_moteur("<chaîne d'encodage bytes>")
  une <- function(s) {
    if (is.na(s)) return(NA_character_)
    if (Encoding(s) == "bytes") return(repere)
    if (Encoding(s) == "latin1") s <- enc2utf8(s)
    if (validUTF8(s)) {
      p <- utf8ToInt(s)
      m <- p <= 0x1F | (p >= 0x7F & p <= 0x9F) | p == 0x2028 | p == 0x2029
      if (!any(m)) {
        o <- charToRaw(s)
      } else {
        morceaux <- lapply(p, function(k) charToRaw(intToUtf8(k)))
        morceaux[m] <- lapply(sprintf("<U+%04X>", p[m]), charToRaw)
        o <- unlist(morceaux, use.names = FALSE)
      }
    } else {
      b <- as.integer(charToRaw(s))
      morceaux <- lapply(b, function(k) as.raw(k))
      haut <- b >= 0x80
      ctrl <- b <= 0x1F | b == 0x7F
      morceaux[haut] <- lapply(sprintf("<%02x>", b[haut]), charToRaw)
      morceaux[ctrl] <- lapply(sprintf("<U+%04X>", b[ctrl]), charToRaw)
      o <- unlist(morceaux, use.names = FALSE)
    }
    r <- rawToChar(o)
    Encoding(r) <- if (any(o >= as.raw(0x80))) "UTF-8" else "unknown"
    r
  }
  vapply(x, une, "", USE.NAMES = FALSE)
}

# Prédicat de la règle (#94) : TRUE pour un élément que neutraliser_citation()
# ne cite pas tel quel (chaîne « bytes », octets qui ne sont pas de l'UTF-8
# valide, caractère de l'ensemble neutralisé) ; FALSE pour NA et pour une
# chaîne citée telle quelle (conversion latin-1 et marque UTF-8 mises à part).
# L'ensemble n'est défini qu'une fois, dans neutraliser_citation().
citation_a_neutraliser <- function(x) {
  if (!is.character(x)) x <- as.character(x)
  n <- neutraliser_citation(x)
  vapply(seq_along(x), function(i) {
    s <- x[i]
    if (is.na(s)) return(FALSE)
    if (Encoding(s) == "bytes") return(TRUE)
    if (Encoding(s) == "latin1") s <- enc2utf8(s)
    !identical(charToRaw(s), charToRaw(n[i]))
  }, NA)
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
