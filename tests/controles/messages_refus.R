# -----------------------------------------------------------------------------
#  tests/controles/messages_refus.R : règle de rédaction des messages de
#  refus (ADR 0007, point 6.4, annotation du 05/11/2026, (c) et (f) ; #127,
#  critère 1), en R de base, sans charger tablexp
#
#  Méthode : chaque fichier de R/ est lu par parse(keep.source = TRUE), en UTF-8
#  quelle que soit la locale (analyser_fichier(), #213) ; les
#  appels sont repérés et situés (ligne) par utils::getParseData()
#  (SYMBOL_FUNCTION_CALL), puis analysés sur l'arbre syntaxique, chacun avec
#  sa fonction de premier niveau et la chaîne de ses fonctions englobantes.
#  Règles vérifiées :
#    1. gabarit : le premier argument de chaque appel de formuler() est une
#       constante chaîne, ou un paste() ou paste0() de constantes chaînes
#       (sans sep ni collapse non constants) ; ses conversions (%s, %d...,
#       %% écarté) sont en même nombre que les autres arguments ; un gabarit
#       non constant n'est admis que par exception, et ses valeurs ne sont
#       alors pas contrôlées ;
#    2. valeurs insérées : chaque argument de formuler() placé sous une
#       conversion %s est une forme admise :
#         - appel de citer(), neutraliser_citation() ou sans_controle()
#           (valeur citée), de liste_guillemets() ou de formuler() (texte
#           formé), ou d'une aide de message (AIDES) ;
#         - constante chaîne ASCII, ou constante numérique (une constante
#           non ASCII passe par formuler() : sous la locale C, sprintf()
#           traduirait ses octets non marqués) ;
#         - texte_moteur(X), X chaîne constante ou constante du moteur
#           (affectation de premier niveau de R/, éventuellement indexée par
#           [, [[ ou $) : texte du moteur marqué UTF-8 avant l'insertion ;
#           jamais une valeur reçue. Une constante dont l'expression contient
#           une chaîne non ASCII hors texte_moteur() (constantes_moteur()) est
#           de codage natif non marqué : elle n'est admise que marquée ainsi ;
#         - if (...) A else B, A et B admis ;
#         - paste() ou paste0() de formes admises, sep et collapse
#           constantes ASCII ;
#         - symbole : chacune de ses affectations dans les fonctions
#           englobantes est admise (suivi à travers une affectation locale ;
#           une affectation qui le reprend, s <- paste0(s, ...), est admise
#           pour ce qui la concerne) ; ou paramètre d'un relais (RELAIS) ;
#         - c(), character(0), ifelse() et vapply(), lapply() ou sapply()
#           de formes admises (fonction appliquée admise par son nom, ou
#           fonction anonyme dont la valeur rendue est admise) ;
#         - exception de la liste fermée EXCEPTIONS (fichier, fonction de
#           premier niveau, expression déparsée, raison) : texte produit par
#           le moteur seul (constante, catalogue, nombre, type R), ASCII, ou
#           texte du moteur marqué par texte_moteur() / passé comme gabarit à
#           formuler() ; une exception ne couvre jamais une constante non
#           ASCII non marquée (échec nommé : cas de M1, revue finale de #127) ;
#    3. message : l'argument message de chaque refus (refuser(),
#       refus_tablexp() et les relais locaux de RELAIS, appariés comme le
#       ferait R) est un appel de formuler() ou d'une aide de message, un
#       symbole dont toutes les affectations le sont, un paramètre de relais,
#       ou if (...) A else B de telles formes : aucun sprintf(), paste() ni
#       paste0() ne forme plus un message de refus ;
#    4. aides de message : chaque expression rendue (dernière expression du
#       corps, arguments de return()) est une forme admise de la règle 2 ;
#    5. exceptions : chacune sert au moins une fois (sinon elle est périmée).
#  Le script imprime le décompte : refus, appels de formuler(), citations
#  relevées (une citation atteinte par plusieurs chemins de suivi compte plus
#  d'une fois), exceptions employées (une ligne d'exception couvre toutes les
#  occurrences de son expression dans sa fonction).
#
#  Limites : les messages d'avertissement (avertir()) et les erreurs de
#  programmation (stop()) sont hors de la règle (annotation, (c) 4 et 5) ;
#  le contenu d'une valeur citée n'est pas contrôlé (citer(x) cite x, quel
#  qu'il soit) ; un relais ou une aide est reconnu par son nom dans sa
#  fonction de premier niveau.
#
#  Usage : Rscript tests/controles/messages_refus.R [dossier R/ [exceptions]]
#  Sans argument : R/ à la racine du dépôt et la liste EXCEPTIONS ci-dessous ;
#  `exceptions` (témoins) : fichier CSV de mêmes colonnes qui la remplace.
#  Code de sortie 1 en cas d'écart. Sourcé (tests), le fichier ne fait que
#  définir les fonctions.
# -----------------------------------------------------------------------------

# Fonctions qui citent une valeur reçue (règle de #94).
CITATIONS <- c("citer", "neutraliser_citation", "sans_controle")
# Fonctions qui rendent un texte formé selon la règle.
FORMES <- c("liste_guillemets", "formuler")

# Refus : nom -> fonction modèle, pour l'appariement des arguments.
REFUS <- list(refuser = function(module, motif, message, details) NULL,
              refus_tablexp = function(module, motif, message, details) NULL)

# Relais locaux d'un refus : fonction (fichier, fonction de premier niveau,
# nom du relais) dont le paramètre `parametre` porte le message (ou une
# partie de message insérée par %s). `modele` sert à apparier ses appels ;
# NULL : le relais n'est pas appelé sous ce nom (fonction anonyme, ou
# refuser() lui-même). Dans le corps du relais, le paramètre est admis.
RELAIS <- list(
  list(fichier = "outils.R", haut = "refus_tablexp", nom = "refus_tablexp", parametre = "message",
       modele = NULL),
  list(fichier = "outils.R", haut = "refuser", nom = "refuser", parametre = "message", modele = NULL),
  list(fichier = "construire_table.R", haut = "valider_plan", nom = "refus_plan", parametre = "message",
       modele = function(motif, message, details) NULL),
  list(fichier = "construire_table.R", haut = "rejouer_fiche", nom = "refus_rejeu", parametre = "message",
       modele = function(motif, message, details) NULL),
  list(fichier = "construire_table.R", haut = "controler_forme_tables", nom = "mal_formee", parametre = "texte",
       modele = function(texte, lu, attendu, motif, chemin) NULL),
  list(fichier = "rapport.R", haut = "texte_objet", nom = "impossible", parametre = "message",
       modele = function(chemin, message) NULL),
  list(fichier = "rapport.R", haut = "valeurs_texte", nom = "refus", parametre = "message",
       modele = function(motif, message) NULL),
  list(fichier = "rapport.R", haut = "valeurs_texte", nom = "invalide", parametre = "quoi",
       modele = function(quoi, i, motif) NULL),
  list(fichier = "rapport.R", haut = "lire_texte", nom = "refus", parametre = "message",
       modele = function(motif, message, ligne, details) NULL),
  list(fichier = "rapport.R", haut = "lire_texte", nom = "<anonyme>", parametre = "message",
       modele = NULL),
  list(fichier = "rapport.R", haut = "controler_rapport", nom = "refus", parametre = "texte",
       modele = function(motif, texte, details) NULL),
  list(fichier = "simulation.R", haut = "controler_portefeuille", nom = "invalide", parametre = "msg",
       modele = function(msg) NULL),
  list(fichier = "simulation.R", haut = "controler_defauts", nom = "invalide", parametre = "msg",
       modele = function(msg, details) NULL),
  list(fichier = "simulation.R", haut = "controler_defauts", nom = "inv", parametre = "msg",
       modele = function(msg) NULL),
  list(fichier = "simulation.R", haut = "controler_forme", nom = "invalide", parametre = "msg",
       modele = function(msg, details) NULL)
)

# Aides de message : (fichier, nom) d'une fonction, de premier niveau ou
# locale, qui rend un texte formé (règle 4, contrôlée dans son fichier) ; son
# appel est admis en tout fichier.
AIDES <- data.frame(
  fichier = c("donnees.R", "donnees.R", "donnees.R", "simulation.R", "construire_table.R", "rapport.R", "rapport.R",
              "positionnement.R", "lissage.R"),
  nom = c("message_aucune_recette", "groupes_cites_table", "lignes_citees_table", "ecart_noms", "decrire_forme",
          "decrire_valeur", "valeur_message", "segments_cites", "segments_cites"),
  stringsAsFactors = FALSE)

# Exceptions : une ligne par site (une expression peut revenir plusieurs fois
# dans la même fonction), avec sa raison. Texte produit par le moteur seul,
# ASCII, ou texte du moteur marqué par texte_moteur() / passé comme gabarit à
# formuler() ; jamais une valeur reçue.
exception_lignes <- function(...) {
  l <- list(...)
  data.frame(fichier = vapply(l, `[`, "", 1L), fonction = vapply(l, `[`, "", 2L),
             expression = vapply(l, `[`, "", 3L), raison = vapply(l, `[`, "", 4L), stringsAsFactors = FALSE)
}
EXCEPTIONS <- exception_lignes(
  c("construire_table.R", "decrire_forme", "typeof(v)", "type R (typeof()), ASCII"),
  c("construire_table.R", "rejouer_fiche", "typeof(fiche)", "type R (typeof()), ASCII"),
  c("construire_table.R", "rejouer_fiche", "attendus[champ]", "formes attendues, texte du moteur (texte_moteur())"),
  c("construire_table.R", "rejouer_fiche", "paste(sprintf(\"%d\", VERSIONS_FICHE_REJEU), collapse = \", \")", "versions de fiche admises, entiers du moteur"),
  c("construire_table.R", "rejouer_fiche", "as.character(utils::packageVersion(\"tablexp\"))", "version du paquet installé"),
  c("construire_table.R", "rejouer_fiche", "ALGORITHME_EMPREINTE", "constante du moteur"),
  c("construire_table.R", "rejouer_fiche", "ENTETE_CANONIQUE", "constante du moteur"),
  c("construire_table.R", "rejouer_fiche", "empreinte", "empreinte recalculée par le moteur (hexadécimal)"),
  c("donnees.R", "controler_roles", "vapply(hors, function(h) paste(utils::head(h, NB_LIGNES_CITEES), collapse = \", \"), character(1L))", "numéros de ligne (which())"),
  c("donnees.R", "controler_transition_periode", "paste(TRANSITIONS$depart, \"->\", TRANSITIONS$arrivee, collapse = \", \")", "catalogue des transitions"),
  c("donnees.R", "controler_transition_periode", "paste(names(hors_p)[hors_p], collapse = \" et \")", "noms des bornes (debut, fin), texte du moteur"),
  c("donnees.R", "preparer_donnees", "non_declares", "codes de motif du moteur (MOTIFS_LIGNES, MOTIFS_TETE)"),
  c("donnees.R", "preparer_donnees", "vapply(premieres, paste, character(1L), collapse = \", \")", "numéros de ligne (which())"),
  c("donnees.R", "table_reference", "paste(utils::head(hors, NB_LIGNES_CITEES), collapse = \", \")", "numéros de ligne (which())"),
  c("donnees.R", "table_reference", "paste(utils::head(melange, NB_LIGNES_CITEES), collapse = \", \")", "années ou générations (as.integer())"),
  c("donnees.R", "table_reference", "paste(utils::head(non_fini, NB_LIGNES_CITEES), collapse = \", \")", "numéros de ligne (which())"),
  c("donnees.R", "table_reference", "paste(utils::head(doublons, NB_LIGNES_CITEES), collapse = \", \")", "numéros de ligne (which())"),
  c("exposition.R", "resoudre_convention", "native", "convention native de la recette (catalogue du moteur)"),
  c("exposition.R", "nombre_cellules", "libelle", "gabarit passé par l'appelant, texte du moteur passé comme gabarit à formuler() (défaut ou appel de exposition.R) ; valeurs : un nombre mis en forme par format(), un %d"),
  c("rapport.R", "ecrire_valeurs", "paste(format(utils::head(u[ni], 3L), digits = 15L), collapse = \", \")", "doubles mis en forme par format()"),
  c("rapport.R", "ecrire_valeurs", "typeof(x)", "type R (typeof()), ASCII"),
  c("rapport.R", "texte_objet", "typeof(v)", "type R (typeof()), ASCII"),
  c("rapport.R", "lire_texte", "nature", "nature du texte (plan, fiche), passée par le moteur"),
  c("rapport.R", "lire_texte", "premiere", "première ligne attendue, constante du moteur"),
  c("rapport.R", "lire_texte", "paste(TYPES_TEXTE, collapse = \", \")", "types de la grammaire du texte, constante du moteur"),
  c("rapport.R", "decrire_valeur", "w$type", "type écrit par texte_valeurs(), ASCII"),
  c("rapport.R", "decrire_valeur", "typeof(v)", "type R (typeof()), ASCII"),
  c("rapport.R", "controler_rapport", "typeof(x)", "type R (typeof()), ASCII"),
  c("rapport.R", "controler_rapport", "a", "empreinte recalculée par le moteur (hexadécimal)"),
  c("simulation.R", "loi_effective", "cause", "cause du catalogue (names(CAUSES_SIMULEES))"),
  c("simulation.R", "loi_effective", "s", "sexe du catalogue (MODALITES_SEXE)"),
  c("simulation.R", "controler_couverture", "cause", "cause du catalogue (names(CAUSES_SIMULEES))"),
  c("simulation.R", "controler_couverture", "s", "sexe du catalogue (MODALITES_SEXE)"),
  c("simulation.R", "controler_defauts", "paste(unique(vides[vide]), collapse = \" ; \")",
    "raisons des conditions vides, formées par formuler() dans conditions_vides() (mode cité par citer() ; #112)")
)

# Conversions d'un gabarit sprintf() (%% écarté) : vecteur des lettres de
# conversion, dans l'ordre.
MOTIF_CONVERSION <- "%(%|[-+ 0#]*([0-9]+|\\*)?(\\.([0-9]+|\\*))?[a-zA-Z])"
conversions <- function(g) {
  m <- regmatches(g, gregexpr(MOTIF_CONVERSION, g, useBytes = TRUE))[[1L]]
  m <- m[m != "%%"]
  substring(m, nchar(m, type = "bytes"))
}

# Élément i d'un appel ou d'une liste d'arguments, ou NULL s'il est vide
# (argument manquant, paramètre sans défaut).
element <- function(l, i) if (identical(l[[i]], quote(expr = ))) NULL else l[[i]]
ascii <- function(s) !grepl("[^\001-\177]", s, useBytes = TRUE)
texte_expr <- function(e) paste(deparse(e, width.cutoff = 500L), collapse = " ")
nom_appel <- function(e) {
  if (!is.call(e)) return("")
  t <- e[[1L]]
  if (is.symbol(t)) return(as.character(t))
  if (is.call(t) && is.symbol(t[[1L]]) && as.character(t[[1L]]) %in% c("::", ":::")) return(as.character(t[[3L]]))
  ""
}
# Argument `arg` de l'appel e apparié sur `modele` ; NULL s'il est absent.
apparier <- function(e, modele, arg) {
  e[[1L]] <- as.name("f_modele")
  mc <- tryCatch(match.call(modele, e), error = function(err) NULL)
  if (is.null(mc)) return(NULL)
  l <- as.list(mc)
  if (!(arg %in% names(l))) return(NULL)
  v <- l[[arg]]
  if (missing(v)) NULL else v
}
# Affectations `sym <- valeur` (ou `=`) de l'arbre e, fonctions imbriquées
# comprises, et boucles for (sym in valeurs) : liste des valeurs.
affectations <- function(e, sym) {
  res <- list()
  visiter <- function(x) {
    if (is.call(x)) {
      n <- nom_appel(x)
      if (n %in% c("<-", "=") && length(x) == 3L && is.symbol(x[[2L]]) && as.character(x[[2L]]) == sym)
        res[[length(res) + 1L]] <<- x[[3L]]
      # for (sym in valeurs) : sym prend les éléments de valeurs.
      if (n == "for" && is.symbol(x[[2L]]) && as.character(x[[2L]]) == sym)
        res[[length(res) + 1L]] <<- x[[3L]]
      for (i in seq_along(x)[-1L]) { v <- element(x, i); if (!is.null(v)) visiter(v) }
    } else if (is.pairlist(x)) {
      for (i in seq_along(x)) { v <- element(x, i); if (!is.null(v)) visiter(v) }
    }
  }
  visiter(e)
  res
}

# Lecture d'un fichier de R/ en UTF-8 quelle que soit la locale (#206, #213) :
# analyser_fichier() et ses auxiliaires (source_ascii(), controler_non_ascii()...)
# sont dans tests/controles/lecture_utf8.R, chargé ici dans l'environnement du
# script (#228), où la description complète de la lecture se lit.
# Aucun nom non ASCII ne change un relevé de ce script (mesuré : sortie
# identique sous LC_ALL=C et sous C.UTF-8).
local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  d <- c(if (length(f)) c(dirname(f[1L]), file.path(dirname(f[1L]), "controles")),
         if (exists("RACINE")) file.path(RACINE, "tests", "controles"),
         file.path("tests", "controles"))
  p <- file.path(d, "lecture_utf8.R")
  p <- p[file.exists(p)]
  if (!length(p)) stop("lecture_utf8.R introuvable (à côté du script, ou tests/controles du répertoire courant)", call. = FALSE)
  sys.source(p[1L], envir = parent.env(environment()))
})

# Constantes du moteur (affectations de premier niveau qui ne sont pas des
# fonctions), tous fichiers de `dossier` : liste(noms, non_ascii), où non_ascii
# est le sous-ensemble des constantes dont l'expression contient une chaîne
# non ASCII hors d'un appel de texte_moteur(). Une telle constante est de
# codage natif non marqué : insérée par %s sans texte_moteur(), sprintf()
# la traduirait sous la locale C (#127, M1 de la revue finale).
constantes_moteur <- function(dossier) {
  non_ascii_nues <- function(x) {
    if (is.character(x)) return(any(!ascii(x[!is.na(x)])))
    if (!is.call(x)) return(FALSE)
    if (nom_appel(x) %in% c("texte_moteur", "function")) return(FALSE)
    for (i in seq_along(x)) { v <- element(x, i); if (!is.null(v) && non_ascii_nues(v)) return(TRUE) }
    FALSE
  }
  noms <- character(0); non_ascii <- character(0)
  for (f in sort(list.files(dossier, pattern = "\\.[Rr]$", full.names = TRUE))) {
    ex <- tryCatch(analyser_fichier(f, keep.source = FALSE), error = function(e) NULL)
    for (e in ex) {
      if (!(is.call(e) && nom_appel(e) %in% c("<-", "=") && length(e) == 3L && is.symbol(e[[2L]]))) next
      if (is.call(e[[3L]]) && nom_appel(e[[3L]]) == "function") next
      noms <- c(noms, as.character(e[[2L]]))
      if (non_ascii_nues(e[[3L]])) non_ascii <- c(non_ascii, as.character(e[[2L]]))
    }
  }
  list(noms = unique(noms), non_ascii = unique(non_ascii))
}

analyser <- function(dossier, exceptions) {
  ecarts <- character(0)
  cst <- constantes_moteur(dossier)
  compte <- c(refus = 0L, formuler = 0L, citees = 0L, exceptions = 0L)
  exceptions$employee <- rep(FALSE, nrow(exceptions))
  fichiers <- sort(list.files(dossier, pattern = "\\.[Rr]$", full.names = TRUE))
  if (!length(fichiers)) return(list(ecarts = sprintf("aucun fichier R dans %s", dossier), compte = compte,
                                     exceptions = exceptions))
  # Une exception ne couvre jamais une constante non ASCII non marquée : elle
  # se marque par texte_moteur() à l'insertion (règle 2).
  for (i in seq_len(nrow(exceptions))) {
    v <- tryCatch(all.vars(parse(text = exceptions$expression[i], keep.source = FALSE)[[1L]]),
                  error = function(e) character(0))
    for (k in intersect(v, cst$non_ascii))
      ecarts <- c(ecarts, sprintf("%s, %s : exception sur la constante non ASCII non marquée %s (texte_moteur() requis) : %s",
                                  exceptions$fichier[i], exceptions$fonction[i], k, exceptions$expression[i]))
  }
  for (f in fichiers) {
    fichier <- basename(f)
    # Lecture en UTF-8 quelle que soit la locale (analyser_fichier(), #213) ;
    # l'arbre et les données d'analyse restent complets.
    ex <- tryCatch(analyser_fichier(f, keep.source = TRUE), error = function(e) e)
    if (inherits(ex, "error")) {
      ecarts <- c(ecarts, sprintf("%s : illisible (%s)", fichier, conditionMessage(ex)))
      next
    }
    pd <- utils::getParseData(ex)
    pd <- pd[order(pd$line1, pd$col1), ]
    appels <- pd[pd$token == "SYMBOL_FUNCTION_CALL", c("line1", "text")]
    rang <- list()  # nom -> nombre d'appels déjà vus (parcours dans l'ordre du texte)
    ligne_de <- function(nom) {
      k <- if (is.null(rang[[nom]])) 1L else rang[[nom]] + 1L
      rang[[nom]] <<- k
      l <- appels$line1[appels$text == nom]
      if (k <= length(l)) l[k] else NA_integer_
    }
    ecart <- function(ligne, haut, texte)
      ecarts <<- c(ecarts, sprintf("%s:%s (%s) : %s", fichier, ligne, haut, texte))

    relais_de <- function(haut) Filter(function(r) r$fichier == fichier && r$haut == haut, RELAIS)
    aide <- function(n) n %in% AIDES$nom
    # Paramètre de relais : symbole `s` paramètre de la fonction englobante
    # la plus proche, déclarée relais pour ce paramètre.
    param_relais <- function(s, ctx) {
      if (!length(ctx$pile)) return(FALSE)
      p <- ctx$pile[[length(ctx$pile)]]
      s %in% p$formels && any(vapply(relais_de(ctx$haut), function(r)
        r$nom == p$nom && r$parametre == s, logical(1L)))
    }
    exception <- function(e, ctx) {
      t <- texte_expr(e)
      i <- which(exceptions$fichier == fichier & exceptions$fonction == ctx$haut & exceptions$expression == t)
      if (length(i)) { exceptions$employee[i] <<- TRUE; compte[["exceptions"]] <<- compte[["exceptions"]] + 1L }
      length(i) > 0L
    }
    # Symbole suivi : toutes ses affectations dans les fonctions englobantes
    # (de la plus proche à la fonction de premier niveau), au moins une. Un
    # paramètre formel d'une fonction de la pile, jusqu'à celle où les
    # affectations sont trouvées, n'est pas suivi : sa valeur reçue peut
    # atteindre le message par un chemin sans affectation (if sans else) ;
    # seul param_relais(), appelé avant suivre(), l'admet.
    suivre <- function(s, ctx, regle, vus) {
      if (s %in% vus) return(TRUE)
      vals <- list()
      for (p in rev(ctx$pile)) {
        if (s %in% p$formels) return(FALSE)
        vals <- affectations(p$corps, s); if (length(vals)) break
      }
      if (!length(vals)) return(FALSE)
      all(vapply(vals, function(v) regle(v, ctx, c(vus, s)), logical(1L)))
    }
    # Règle 2 : forme admise sous %s.
    admise <- function(e, ctx, vus = character(0)) {
      if (is.character(e) && length(e) == 1L) return(ascii(e))
      if ((is.numeric(e) && length(e) == 1L) || is.null(e)) return(TRUE)
      if (is.symbol(e)) {
        s <- as.character(e)
        return(param_relais(s, ctx) || suivre(s, ctx, admise, vus))
      }
      if (!is.call(e)) return(FALSE)
      n <- nom_appel(e)
      if (n %in% CITATIONS) { compte[["citees"]] <<- compte[["citees"]] + 1L; return(TRUE) }
      if (n %in% FORMES || aide(n)) return(TRUE)
      # texte_moteur(X) marque UTF-8 un texte du moteur : admis pour une
      # chaîne constante ou une constante du moteur (éventuellement indexée
      # par [, [[ ou $), jamais pour une valeur reçue.
      if (n == "texte_moteur" && length(e) == 2L) {
        racine <- e[[2L]]
        while (is.call(racine) && nom_appel(racine) %in% c("[", "[[", "$")) racine <- racine[[2L]]
        return((is.character(racine) && length(racine) == 1L) ||
               (is.symbol(racine) && as.character(racine) %in% cst$noms))
      }
      if (n == "(") return(admise(e[[2L]], ctx, vus))
      if (n %in% c("<-", "=") && length(e) == 3L) return(admise(e[[3L]], ctx, vus))
      if (n == "if" && length(e) == 4L) return(admise(e[[3L]], ctx, vus) && admise(e[[4L]], ctx, vus))
      if (n == "ifelse" && length(e) == 4L) return(admise(e[[3L]], ctx, vus) && admise(e[[4L]], ctx, vus))
      if (n == "character" && length(e) == 2L && is.numeric(e[[2L]]) && e[[2L]] == 0) return(TRUE)
      if (n == "c") return(all(vapply(as.list(e)[-1L], admise, logical(1L), ctx = ctx, vus = vus)))
      if (n %in% c("vapply", "lapply", "sapply")) {
        # Fonction appliquée : forme admise par son nom, ou fonction anonyme
        # dont la valeur rendue est admise.
        fun <- apparier(e, function(X, FUN, ...) NULL, "FUN")
        if (is.symbol(fun) && (as.character(fun) %in% c(CITATIONS, FORMES) || aide(as.character(fun)))) {
          if (as.character(fun) %in% CITATIONS) compte[["citees"]] <<- compte[["citees"]] + 1L
          return(TRUE)
        }
        if (is.call(fun) && nom_appel(fun) == "function") {
          ctx2 <- ctx
          ctx2$pile <- c(ctx$pile, list(list(nom = "<anonyme>", formels = names(fun[[2L]]), corps = fun[[3L]])))
          corps <- fun[[3L]]
          if (is.call(corps) && nom_appel(corps) == "{") corps <- corps[[length(corps)]]
          return(admise(corps, ctx2, vus))
        }
        return(FALSE)
      }
      if (n %in% c("paste", "paste0")) {
        a <- as.list(e)[-1L]
        nm <- if (is.null(names(a))) rep("", length(a)) else names(a)
        ok_sep <- all(vapply(a[nm %in% c("sep", "collapse")], function(v)
          is.character(v) && length(v) == 1L && ascii(v), logical(1L)))
        ok_val <- all(vapply(a[!(nm %in% c("sep", "collapse"))], admise, logical(1L), ctx = ctx, vus = vus))
        if (ok_sep && ok_val) return(TRUE)
      }
      FALSE
    }
    # Règle 3 : message d'un refus.
    message_forme <- function(e, ctx, vus = character(0)) {
      if (is.symbol(e)) {
        s <- as.character(e)
        return(param_relais(s, ctx) || suivre(s, ctx, message_forme, vus))
      }
      if (!is.call(e)) return(FALSE)
      n <- nom_appel(e)
      if (n == "formuler" || aide(n)) return(TRUE)
      if (n == "(") return(message_forme(e[[2L]], ctx, vus))
      if (n == "if" && length(e) == 4L) return(message_forme(e[[3L]], ctx, vus) && message_forme(e[[4L]], ctx, vus))
      FALSE
    }
    # Règle 1 et 2 : un appel de formuler().
    controler_formuler <- function(e, ctx, ligne) {
      compte[["formuler"]] <<- compte[["formuler"]] + 1L
      a <- as.list(e)[-1L]
      if (!length(a)) return(ecart(ligne, ctx$haut, "formuler() sans gabarit"))
      g <- a[[1L]]
      constante <- function(v) is.character(v) && length(v) == 1L
      if (is.call(g) && nom_appel(g) %in% c("paste", "paste0") && all(vapply(as.list(g)[-1L], constante, logical(1L))))
        g <- eval(g, baseenv())
      if (!constante(g)) {
        if (!exception(a[[1L]], ctx))
          ecart(ligne, ctx$haut, sprintf("gabarit de formuler() non constant : %s", texte_expr(a[[1L]])))
        return(invisible())
      }
      conv <- conversions(g)
      valeurs <- a[-1L]
      if (length(conv) != length(valeurs))
        return(ecart(ligne, ctx$haut, sprintf("formuler() : %d conversion(s) pour %d valeur(s) : %s",
                                              length(conv), length(valeurs), substr(g, 1L, 60L))))
      for (i in which(conv == "s")) {
        v <- valeurs[[i]]
        if (!admise(v, ctx) && !exception(v, ctx))
          ecart(ligne, ctx$haut, sprintf("valeur insérée par %%s sans citation ni forme admise : %s",
                                         substr(texte_expr(v), 1L, 120L)))
      }
    }
    controler_refus <- function(e, ctx, ligne, modele, nom) {
      compte[["refus"]] <<- compte[["refus"]] + 1L
      arg <- names(formals(modele))[names(formals(modele)) %in% c("message", "texte", "msg", "quoi")][1L]
      m <- apparier(e, modele, arg)
      if (is.null(m)) return(ecart(ligne, ctx$haut, sprintf("%s() sans message apparié", nom)))
      if (!message_forme(m, ctx))
        ecart(ligne, ctx$haut, sprintf("message de %s() non formé par formuler() : %s", nom,
                                       substr(texte_expr(m), 1L, 120L)))
    }
    # Règle 4 : expressions rendues d'une aide.
    controler_aide <- function(fn, ctx, ligne) {
      corps <- fn[[3L]]
      rendus <- list()
      chercher <- function(x) {
        if (is.call(x)) {
          if (nom_appel(x) == "function") return(invisible())
          if (nom_appel(x) == "return" && length(x) == 2L) rendus[[length(rendus) + 1L]] <<- x[[2L]]
          for (i in seq_along(x)[-1L]) { v <- element(x, i); if (!is.null(v)) chercher(v) }
        }
      }
      chercher(corps)
      dernier <- if (is.call(corps) && nom_appel(corps) == "{") corps[[length(corps)]] else corps
      rendus[[length(rendus) + 1L]] <- dernier
      for (r in rendus) {
        if (is.call(r) && nom_appel(r) == "return") next
        if (!admise(r, ctx) && !exception(r, ctx))
          ecart(ligne, ctx$haut, sprintf("aide de message : expression rendue non formée : %s",
                                         substr(texte_expr(r), 1L, 120L)))
      }
    }
    visiter <- function(x, ctx, nom_fn = "<anonyme>") {
      if (!is.call(x)) {
        if (is.pairlist(x)) for (i in seq_along(x)) { v <- element(x, i); if (!is.null(v)) visiter(v, ctx) }
        return(invisible())
      }
      n <- nom_appel(x)
      if (n == "function") {
        p <- list(nom = nom_fn, formels = names(x[[2L]]), corps = x[[3L]])
        ctx2 <- ctx; ctx2$pile <- c(ctx$pile, list(p))
        if (nom_fn %in% AIDES$nom[AIDES$fichier == fichier]) controler_aide(x, ctx2, NA)
        f <- x[[2L]]
        for (i in seq_along(f)) { v <- element(f, i); if (!is.null(v)) visiter(v, ctx) }
        visiter(x[[3L]], ctx2)
        return(invisible())
      }
      if (nzchar(n) && is.symbol(x[[1L]])) {
        ligne <- if (n %in% appels$text) ligne_de(n) else NA_integer_
        if (n == "formuler") controler_formuler(x, ctx, ligne)
        if (n %in% names(REFUS)) controler_refus(x, ctx, ligne, REFUS[[n]], n)
        for (r in relais_de(ctx$haut)) if (!is.null(r$modele) && r$nom == n) controler_refus(x, ctx, ligne, r$modele, n)
      }
      if (n %in% c("<-", "=") && length(x) == 3L && is.symbol(x[[2L]]) && is.call(x[[3L]]) &&
          nom_appel(x[[3L]]) == "function") {
        visiter(x[[3L]], ctx, as.character(x[[2L]]))
        return(invisible())
      }
      for (i in seq_along(x)) { v <- element(x, i); if (!is.null(v)) visiter(v, ctx) }
    }
    for (e in ex) {
      haut <- if (is.call(e) && nom_appel(e) %in% c("<-", "=") && is.symbol(e[[2L]])) as.character(e[[2L]]) else "(premier niveau)"
      visiter(e, list(haut = haut, pile = list()))
    }
  }
  perimees <- !exceptions$employee
  for (i in which(perimees))
    ecarts <- c(ecarts, sprintf("exception périmée (aucun site) : %s, %s, %s", exceptions$fichier[i],
                                exceptions$fonction[i], exceptions$expression[i]))
  list(ecarts = ecarts, compte = compte, exceptions = exceptions)
}

lire_exceptions <- function(chemin) {
  x <- utils::read.csv(chemin, colClasses = "character", encoding = "UTF-8")
  attendues <- c("fichier", "fonction", "expression", "raison")
  if (!identical(names(x), attendues))
    stop(sprintf("%s : colonnes %s attendues", chemin, paste(attendues, collapse = ", ")), call. = FALSE)
  x
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  racine <- dirname(dirname(dirname(normalizePath(script))))
  dossier <- if (length(args) >= 1L) args[1L] else file.path(racine, "R")
  exceptions <- if (length(args) >= 2L) lire_exceptions(args[2L]) else EXCEPTIONS
  cat("Messages de refus (ADR 0007, annotation du 05/11/2026) :", dossier, "\n")
  if (anyDuplicated(exceptions[c("fichier", "fonction", "expression")]) || any(!nzchar(exceptions$raison)) ||
      any(!ascii(exceptions$expression))) {
    cat("  ECHEC : liste d'exceptions : ligne en double, sans raison ou expression non ASCII\n")
    quit(save = "no", status = 1L)
  }
  r <- analyser(dossier, exceptions)
  cat(sprintf("  %d refus (refuser(), refus_tablexp(), relais) ; %d appel(s) de formuler() ; %d citation(s) relevée(s) ; %d emploi(s) d'exception, %d exception(s)\n",
              r$compte[["refus"]], r$compte[["formuler"]], r$compte[["citees"]], r$compte[["exceptions"]],
              nrow(r$exceptions)))
  if (length(r$ecarts)) {
    cat(paste0("  ECHEC : ", r$ecarts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Messages de refus::", paste(r$ecarts, collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("  OK : tout message de refus est formé par formuler(), toute valeur insérée par %s est citée ou formée.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
