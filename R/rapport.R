# -----------------------------------------------------------------------------
#  R/rapport.R : module `rapport`, fiche de construction en mémoire et
#  empreintes (issue #11 ; ADR 0005, point 6 ; ADR 0007, points 6.2, 9 et
#  12 ; spécification d'actuary, A.3, « Fiche »)
#
#  - texte_canonique() : représentation textuelle canonique des données
#    reçues (ADR 0007, point 9.3), en octets ;
#  - empreinte_donnees(), empreintes_construction() : MD5 par
#    tools::md5sum() sur un fichier temporaire écrit en binaire ; algorithme
#    nommé ; jamais serialize(), dont l'en-tête porte la version de R
#    (point 9.4) ;
#  - controler_provenance() : argument `provenance` de construire_table()
#    (point 3.1) ;
#  - assembler_fiche() : fiche en mémoire (point 6.2), sans horodatage ;
#  - informations_execution() : horodatage, durée, session abrégée, hors de
#    toute comparaison (point 6.1).
#
#  Empreintes du résultat et du code : non calculées dans cette branche
#  (point 9.2 ; branche fiche-rapport). Sérialisation de la fiche : reportée
#  (point 12).
#  Ce module n'appelle que les modules qui le précèdent (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# Version de la fiche (ADR 0007, point 6.2) et de la représentation
# canonique (point 9.3) : tout changement de l'une ou de l'autre est un
# changement de résultat (visa, incrément).
VERSION_FICHE <- 1L
ENTETE_CANONIQUE <- "tablexp canonique 1"
ALGORITHME_EMPREINTE <- "md5"
STATUT_EMPREINTES_NON_CALCULEES <- "non calculée (branche fiche-rapport)"

# Octets UTF-8 des chaînes x (character), entre guillemets droits, avec les
# cinq échappements \\, \n, \r, \t, \" ; NA sans guillemets ; une valeur
# par ligne, LF final. Refuse (module rapport) une chaîne au codage
# `bytes`, non UTF-8 valide, ou non ASCII non marquée dans une session dont
# la locale n'est ni UTF-8 ni latin-1 (ADR 0007, point 9.3).
octets_chaines <- function(x, role) {
  enc <- Encoding(x)
  if (any(enc == "bytes"))
    refuser("rapport", "chaine_codage_bytes",
            sprintf("Rôle « %s » : chaîne au codage « bytes », non convertible en UTF-8 sans deviner.", role),
            list(role = role, lignes = which(enc == "bytes")))
  non_ascii <- !is.na(x) & grepl("[^\001-\177]", x, useBytes = TRUE)
  locale <- l10n_info()
  locale_connue <- isTRUE(locale[["UTF-8"]]) || isTRUE(locale[["Latin-1"]])
  indetermine <- non_ascii & enc == "unknown" & !locale_connue
  if (any(indetermine))
    refuser("rapport", "chaine_codage_indetermine",
            sprintf(paste("Rôle « %s » : chaîne non ASCII sans codage déclaré dans une session dont la",
                          "locale n'est ni UTF-8 ni latin-1 : son codage est indéterminé."), role),
            list(role = role, lignes = which(indetermine)))
  y <- enc2utf8(x)
  invalide <- !is.na(y) & !validUTF8(y)
  if (any(invalide))
    refuser("rapport", "chaine_non_utf8",
            sprintf("Rôle « %s » : chaîne qui n'est pas de l'UTF-8 valide.", role),
            list(role = role, lignes = which(invalide)))
  # Échappements sur les valeurs distinctes, vectorisés (cinq gsub), puis
  # octets par charToRaw() (indépendant du marquage) ; chaque ligne est
  # assemblée en trois morceaux : guillemet ouvrant, octets, guillemet
  # fermant et LF ; NA s'écrit NA, LF, sans guillemets.
  if (!length(y)) return(raw(0))
  valeurs <- unique(y)
  na <- is.na(valeurs)
  esc <- valeurs[!na]
  for (r in list(c("\\", "\\\\"), c("\n", "\\n"), c("\r", "\\r"), c("\t", "\\t"), c("\"", "\\\"")))
    esc <- gsub(r[1L], r[2L], esc, fixed = TRUE, useBytes = TRUE)
  corps <- vector("list", length(valeurs))
  corps[!na] <- lapply(esc, charToRaw)
  corps[na] <- list(charToRaw("NA"))
  i <- match(y, valeurs)
  na_i <- na[i]
  morceaux <- vector("list", 3L * length(y))
  morceaux[seq.int(1L, by = 3L, length.out = length(y))] <-
    ifelse(na_i, list(raw(0)), list(as.raw(0x22)))
  morceaux[seq.int(2L, by = 3L, length.out = length(y))] <- corps[i]
  morceaux[seq.int(3L, by = 3L, length.out = length(y))] <-
    ifelse(na_i, list(as.raw(0x0a)), list(as.raw(c(0x22, 0x0a))))
  unlist(morceaux, use.names = FALSE)
}

# Octets ASCII d'un vecteur de textes (une valeur par ligne, LF final).
octets_lignes <- function(textes) {
  if (!length(textes)) return(raw(0))
  charToRaw(paste0(textes, "\n", collapse = ""))
}

# Type canonique et octets des valeurs d'une colonne (ADR 0007, point 9.3).
colonne_canonique <- function(x, role) {
  if (is.factor(x)) return(list(type = "factor", octets = octets_chaines(as.character(x), role)))
  if (identical(class(x), "Date")) {
    j <- unclass(x)
    u <- unique(j)
    txt <- format(structure(u, class = "Date"), "%Y-%m-%d")
    txt[is.na(u)] <- "NA"
    return(list(type = "Date", octets = octets_lignes(txt[match(j, u)])))
  }
  if (is.object(x))
    refuser("rapport", "type_non_canonique",
            sprintf("Rôle « %s » : classe « %s » sans représentation canonique.", role,
                    paste(class(x), collapse = "/")),
            list(role = role))
  if (is.character(x)) return(list(type = "character", octets = octets_chaines(x, role)))
  if (is.integer(x)) return(list(type = "integer", octets = octets_lignes(sprintf("%d", x))))
  if (is.double(x)) return(list(type = "double", octets = octets_lignes(sprintf("%.17g", x + 0))))
  if (is.logical(x)) {
    txt <- ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE"))
    return(list(type = "logical", octets = octets_lignes(txt)))
  }
  refuser("rapport", "type_non_canonique",
          sprintf("Rôle « %s » : type « %s » sans représentation canonique.", role, typeof(x)),
          list(role = role))
}

# Représentation canonique des données reçues (ADR 0007, point 9.3) :
# colonnes à rôle déclaré, toutes les lignes reçues dans l'ordre reçu,
# colonnes par nom de rôle en ordre d'octets ; première ligne
# « tablexp canonique 1<TAB><lignes><TAB><colonnes> », puis par colonne
# « #<rôle><TAB><type> » et une valeur par ligne ; LF, aucun CR. Écrit les
# octets dans `fichier` (binaire, writeBin) ; rend `fichier`, invisible.
texte_canonique <- function(donnees, roles, fichier) {
  r <- sort(names(roles), method = "radix")
  con <- file(fichier, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(sprintf("%s\t%d\t%d\n", ENTETE_CANONIQUE, nrow(donnees), length(r))), con)
  for (role in r) {
    cc <- colonne_canonique(donnees[[roles[[role]]]], role)
    writeBin(charToRaw(sprintf("#%s\t%s\n", role, cc$type)), con)
    writeBin(cc$octets, con)
  }
  invisible(fichier)
}

# Empreinte MD5 de la représentation canonique (fichier temporaire supprimé
# à la sortie ; tempfile() ne touche pas .Random.seed, ADR 0007, point 8.2).
empreinte_donnees <- function(donnees, roles) {
  f <- tempfile("tablexp_canonique_")
  on.exit(unlink(f))
  texte_canonique(donnees, roles, f)
  unname(tools::md5sum(f))
}

# Contrôle l'argument `provenance` : NULL, ou list(fichier, empreinte,
# algorithme) de trois chaînes non vides.
controler_provenance <- function(provenance) {
  if (is.null(provenance)) return(invisible(TRUE))
  chaine <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  if (!is.list(provenance) || is.null(names(provenance)) ||
      !identical(sort(names(provenance), method = "radix"), c("algorithme", "empreinte", "fichier")) ||
      !all(vapply(provenance, chaine, logical(1L))))
    refuser("rapport", "provenance_invalide",
            paste("La provenance doit être NULL (données construites en mémoire) ou une liste",
                  "(fichier, empreinte, algorithme) de trois chaînes non vides (ADR 0007, point 3.1)."),
            list(provenance = provenance))
  invisible(TRUE)
}

# Empreintes de la fiche (ADR 0007, point 9.2).
empreintes_construction <- function(donnees, roles, provenance) {
  list(algorithme = ALGORITHME_EMPREINTE, representation = ENTETE_CANONIQUE,
       donnees = empreinte_donnees(donnees, roles),
       fichier = if (is.null(provenance)) NA_character_ else provenance$empreinte,
       algorithme_fichier = if (is.null(provenance)) NA_character_ else provenance$algorithme,
       resultat = NA_character_, code = NA_character_,
       statut_resultat_code = STATUT_EMPREINTES_NON_CALCULEES)
}

# Fiche de construction en mémoire (ADR 0007, point 6.2 ; A.3) : aucun
# horodatage. `plan` est le plan reçu, inchangé (rejeu :
# construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance)).
assembler_fiche <- function(roles, plan, recette, etapes, avertissements, empreintes, provenance) {
  pd <- etapes[["donnees"]][["parametres"]]
  pe <- etapes[["exposition"]][["parametres"]]
  periode <- plan[["periode"]]
  delai <- plan[["delai_declaration"]]
  unite <- pd$unite_observation
  list(
    version_fiche = VERSION_FICHE,
    versions = list(tablexp = as.character(utils::packageVersion("tablexp")), R = R.version.string),
    plateforme = list(sysname = Sys.info()[["sysname"]], release = Sys.info()[["release"]],
                      machine = Sys.info()[["machine"]], blas = extSoftVersion()[["BLAS"]],
                      lapack = La_library()),
    roles = roles,
    plan = plan,
    transition = plan[["transition"]],
    transition_code = pd$transition,
    periode = periode,
    delai_declaration = delai,
    fin_effective = pd$fin_effective,
    segmentation = plan[["segmentation"]],
    unite_observation = unite,
    avertissement_unite = if (any(avertissements$code == "unite_contrat"))
      avertissements$message[avertissements$code == "unite_contrat"][1L] else NA_character_,
    recette = recette$recette,
    convention = if (is.null(pe)) NULL else pe$convention,
    unite_exposition = if (is.null(pe)) NA_character_ else pe$unite_exposition,
    par_annee = if (is.null(pe)) NA else pe$par_annee,
    exclusions = etapes[["donnees"]][["resultat"]][["exclusions"]],
    hypothese_censure = if (is.null(pe)) NA_character_ else pe$hypothese_censure,
    graine = if ("graine" %in% names(plan)) plan[["graine"]] else NA_integer_,
    graine_mention = if ("graine" %in% names(plan)) "graine du plan" else "aucune étape aléatoire",
    generateur = GENERATEUR,
    provenance = provenance,
    provenance_mention = if (is.null(provenance)) "aucun fichier (données construites en mémoire)"
                         else "fichier lu par la couche de lecture",
    empreintes = empreintes,
    avertissements = avertissements
  )
}

# Informations d'exécution, hors de toute comparaison (ADR 0007, point 6.1).
informations_execution <- function(debut) {
  list(debut = debut, duree_secondes = as.numeric(difftime(Sys.time(), debut, units = "secs")),
       session = list(R = R.version.string, plateforme = R.version$platform,
                      systeme = utils::osVersion, locale = Sys.getlocale(),
                      paquets_attaches = .packages()))
}
