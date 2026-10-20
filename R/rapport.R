# -----------------------------------------------------------------------------
#  R/rapport.R : module `rapport`, fiche de construction en mémoire et
#  empreintes (issue #11 ; ADR 0005, point 6 ; ADR 0007, points 6.2, 9 et
#  12 ; spécification d'actuary, A.3, « Fiche »)
#
#  - ecrire_valeurs() : écrivain unique des valeurs d'une colonne, en octets,
#    en deux formes, « empreinte » (doubles en octets IEEE 754) et « texte »
#    (doubles en %.17g) (ADR 0011, points 2 et 3 ; #35) ; texte_date() : écriture
#    ISO d'une date, partagée avec le lecteur (#19, #50) ;
#  - texte_canonique() : représentation textuelle canonique des données
#    reçues (ADR 0007, point 9.3 ; ADR 0011, point 2), en octets ;
#  - empreinte_donnees(), empreintes_construction() : MD5 par
#    tools::md5sum() sur un fichier temporaire écrit en binaire ; algorithme
#    nommé ; jamais serialize(), dont l'en-tête porte la version de R
#    (point 9.4) ;
#  - controler_provenance() : argument `provenance` de construire_table()
#    (point 3.1) ;
#  - canoniser_plan() : plan canonique (ADR 0011, point 5.1 ; #42), appelé
#    une fois par construire_table(), après valider_plan() ; valeurs des
#    champs ensembles (estimateurs, exclusions, segmentation) dans l'ordre de
#    l'argument `ordres` (quatrième annotation du 20/10/2026 ; #63) ;
#  - assembler_fiche() : fiche en mémoire (point 6.2), sans horodatage ;
#    plan canonique, rôles dans l'ordre du catalogue, provenance dans l'ordre
#    (fichier, empreinte, algorithme) (ADR 0011, points 5.2 à 5.4) ;
#    champ hypotheses_brut, présent seulement si l'étape `brut` est exécutée,
#    à sa place dans ORDRE_CHAMPS_FICHE (#64 ; #14, spécification d'actuary,
#    A.5), avec la convention d'ensemble à risque et le dénominateur des
#    instants si Kaplan-Meier est retenu (#15, A.9) ;
#  - ordonner_fiche() : réordonnancement pur de la fiche selon un ordre de
#    champs, erreur de programmation si un champ présent n'y figure pas (#64) ;
#  - informations_execution() : horodatage, durée, session abrégée, hors de
#    toute comparaison (point 6.1) ;
#  - ecrire_plan(), lire_plan(), ecrire_fiche(), lire_fiche() : sérialisation
#    du plan et de la fiche, texte en octets UTF-8 <-> objet, lecture sans
#    évaluation, fichier chez l'appelant (ADR 0011, points 6, 8 et 9 ; #50) ;
#    exportées (E5.4, E3.7) ; lire_plan() ne valide pas le plan (point 9.2).
#
#  Empreintes du résultat et du code : non calculées dans cette branche
#  (point 9.2 ; branche fiche-rapport).
#  Ce module n'appelle que les modules qui le précèdent (ADR 0007, point 2.1).
# -----------------------------------------------------------------------------

# Version de la fiche (ADR 0007, point 6.2) et de la représentation
# canonique (point 9.3) : tout changement de l'une ou de l'autre est un
# changement de résultat (visa, incrément). « tablexp canonique 2 » : doubles
# de l'empreinte écrits en octets IEEE 754 (ADR 0011, points 2 et 10 ; #35).
# Fiche 2 : plan canonique, rôles et provenance dans l'ordre canonique, copies
# de premier niveau du plan retirées (ADR 0011, points 5.1 à 5.5 ; #42, #22).
# Fiche 3 : champ nombre_lignes_exclues (#57) ; premier commit de la cinquième
# branche qui change la définition de la fiche (point 10 (a)).
VERSION_FICHE <- 3L
ENTETE_CANONIQUE <- "tablexp canonique 2"
ALGORITHME_EMPREINTE <- "md5"
# Littéral laissé tel quel : sonde de l'installation en UTF-8 des batteries
# (tests/harnais.R, sonde_utf8(), M54) ; marqué UTF-8 à l'emploi (#50).
STATUT_EMPREINTES_NON_CALCULEES <- "non calculée (branche fiche-rapport)"

# Bornes des blocs de collage d'octets_chaines() (#22, point 1) : une ligne
# occupe, dans le texte passé à paste(), guillemet, corps, guillemet et LF,
# soit nchar(corps, "bytes") + 3 octets (une ligne NA, de corps guillemet,
# y compte 4 octets pour les 3 écrits après le gsub() : la borne porte sur
# le texte avant ce gsub(), la plus grande chaîne construite). Découpage
# glouton dans l'ordre : chaque bloc de plusieurs lignes fait au plus
# `limite` octets (une chaîne R plafonne à 2^31 - 1 octets) et la ligne
# suivante ne tiendrait pas ; une ligne plus longue que `limite` forme seule
# son bloc (seule = TRUE). Rend debut, fin (indices de lignes) et seule.
bornes_blocs <- function(corps, limite) {
  fin_ligne <- cumsum(as.numeric(nchar(corps, type = "bytes")) + 3)
  debut <- fin <- integer(0)
  seule <- logical(0)
  s <- 1L
  base <- 0
  while (s <= length(corps)) {
    e <- findInterval(base + limite, fin_ligne)
    debut <- c(debut, s)
    seule <- c(seule, e < s)
    if (e < s) e <- s
    fin <- c(fin, e)
    base <- fin_ligne[e]
    s <- e + 1L
  }
  list(debut = debut, fin = fin, seule = seule)
}

# Octets UTF-8 des chaînes x (character), entre guillemets droits, avec les
# cinq échappements \\, \n, \r, \t, \" ; NA sans guillemets ; une valeur
# par ligne, LF final. Refuse (module rapport) une chaîne au codage
# `bytes`, non UTF-8 valide, ou non ASCII non marquée dans une session dont
# la locale n'est ni UTF-8 ni latin-1 (ADR 0007, point 9.3).
octets_chaines <- function(x, role, limite = 2^30) {
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
  # Échappements sur les valeurs distinctes, vectorisés (cinq gsub). Les
  # corps, marqués `bytes` pour que paste() les copie sans traduction en toute
  # locale, sont collés avec le séparateur guillemet, LF, guillemet, entre un
  # guillemet ouvrant et un guillemet fermant et LF, puis un seul charToRaw()
  # par bloc, sans liste d'un vecteur raw par ligne (#22, point 1). NA prend
  # le corps guillemet : sa ligne s'écrit trois guillemets et LF, puis devient
  # NA et LF par un gsub(). Trois guillemets de suite n'apparaissent dans
  # aucune autre ligne : un corps échappé ne contient de guillemet que précédé
  # d'une barre oblique inverse, donc jamais en tête, et deux au plus de suite
  # avec le guillemet fermant. Collage par blocs (bornes_blocs()) ; une
  # ligne plus longue que `limite` forme son bloc à elle seule et s'écrit en
  # raw, sans paste(). `limite` n'est un argument que pour les tests du
  # découpage.
  if (!length(y)) return(raw(0))
  valeurs <- unique(y)
  na <- is.na(valeurs)
  esc <- valeurs
  for (r in list(c("\\", "\\\\"), c("\n", "\\n"), c("\r", "\\r"), c("\t", "\\t"), c("\"", "\\\"")))
    esc <- gsub(r[1L], r[2L], esc, fixed = TRUE, useBytes = TRUE)
  esc[na] <- "\""
  Encoding(esc) <- "bytes"
  i <- match(y, valeurs)
  corps <- esc[i]
  b <- bornes_blocs(corps, limite)
  blocs <- vector("list", length(b$debut))
  for (k in seq_along(blocs)) {
    s <- b$debut[k]
    e <- b$fin[k]
    blocs[[k]] <- if (b$seule[k]) {
      if (na[i[s]]) charToRaw("NA\n") else c(as.raw(0x22), charToRaw(corps[s]), as.raw(c(0x22, 0x0a)))
    } else {
      texte <- paste0("\"", paste(if (s == 1L && e == length(corps)) corps else corps[s:e],
                                  collapse = "\"\n\""), "\"\n")
      if (any(na)) texte <- gsub("\"\"\"\n", "NA\n", texte, fixed = TRUE, useBytes = TRUE)
      charToRaw(texte)
    }
  }
  if (length(blocs) == 1L) blocs[[1L]] else unlist(blocs, use.names = FALSE)
}

# Octets ASCII d'un vecteur de textes (une valeur par ligne, LF final).
octets_lignes <- function(textes) {
  if (!length(textes)) return(raw(0))
  charToRaw(paste0(textes, "\n", collapse = ""))
}

# Écriture ISO 8601 AAAA-MM-JJ, année sur quatre chiffres (ADR 0011, point 4 ;
# #19), de jours `u` depuis 1970-01-01 entiers, finis et dans le domaine
# (contrôlés par l'appelant) ; partagée par l'écrivain (ecrire_valeurs()) et
# par le contrôle de canonicité du lecteur (valeurs_texte()) (#19, #50).
texte_date <- function(u) {
  cd <- composantes_date(u)
  sprintf("%04d-%02d-%02d", cd$an, cd$mois + 1L, cd$jour)
}

# Écrivain unique des valeurs d'une colonne (ADR 0011, point 3.1) : type
# canonique et octets, selon la forme, « empreinte » (texte empreinté : doubles
# en octets IEEE 754) ou « texte » (fiche, références de non-régression :
# doubles en %.17g). Les doubles seuls diffèrent d'une forme à l'autre ;
# integer, logical, character, factor et Date s'écrivent de même (ADR 0007,
# point 9.3 ; dates : ADR 0011, point 4, #19).
#
# Double : ramené d'abord à un vecteur nu (as.vector() ; writeBin refuse un
# attribut dim, dim et names n'entrent pas dans l'écriture) ; un fini est écrit
# depuis x + 0 (-0 devient 0) ; un non fini n'est jamais passé par une
# opération, qui n'en garantit pas les octets (?NA ; NA_real_ + 0 change les
# octets de NA_real_) : son jeton NA (is.na & !is.nan), NaN, Inf ou -Inf se lit
# sur la valeur reçue (ADR 0011, point 2.2 ; décision d'actuary, #35).
# Forme « empreinte » (point 2.1), une valeur par ligne, LF final : un fini
# s'écrit en 16 hexadécimaux minuscules des 8 octets de
# writeBin(x + 0, raw(), size = 8, endian = "little"), un non fini par son
# jeton. Vectorisé sur raw : aucune boucle R par élément (la seule boucle
# porte sur les quatre jetons). Forme « texte » : %.17g, un non fini s'écrit
# comme R l'écrit (NA, NaN, Inf, -Inf).
ecrire_valeurs <- function(x, forme, role) {
  formes <- c("empreinte", "texte")
  if (!is.character(forme) || length(forme) != 1L || !forme %in% formes)
    refuser("rapport", "forme_ecriture_inconnue",
            sprintf("Forme d'écriture « %s » inconnue (attendu : %s).",
                    paste(forme, collapse = "/"), paste(formes, collapse = ", ")),
            list(forme = forme))
  # Seize hexadécimaux minuscules et LF par double fini : un octet = deux
  # caractères, quartet fort puis quartet faible.
  hexadecimaux <- function(y) {
    o <- as.integer(writeBin(y, raw(), size = 8L, endian = "little"))
    h <- charToRaw("0123456789abcdef")[as.vector(rbind(o %/% 16L, o %% 16L)) + 1L]
    as.vector(rbind(matrix(h, nrow = 16L), as.raw(0x0a)))
  }
  doubles_empreinte <- function(x) {
    fini <- is.finite(x)
    if (all(fini)) return(hexadecimaux(x + 0))
    # Jeton des non finis, lu sur x : 1 NA, 2 NaN, 3 Inf, 4 -Inf. Les jetons
    # sont posés par leur position, les lignes finies, dans l'ordre, ensuite.
    jetons <- c("NA", "NaN", "Inf", "-Inf")
    z <- x[!fini]
    jeton <- ifelse(is.nan(z), 2L, ifelse(is.na(z), 1L, ifelse(z > 0, 3L, 4L)))
    longueur <- rep.int(17, length(x))
    longueur[!fini] <- c(3, 4, 4, 5)[jeton]
    debut <- cumsum(longueur) - longueur
    sortie <- raw(sum(longueur))
    pos <- debut[!fini]
    pris <- vector("list", length(jetons))
    for (k in seq_along(jetons)) {
      i <- which(jeton == k)
      if (length(i)) {
        t <- charToRaw(paste0(jetons[k], "\n"))
        pris[[k]] <- rep(pos[i], each = length(t)) + seq_along(t)
        sortie[pris[[k]]] <- t
      }
    }
    if (any(fini)) sortie[-unlist(pris, use.names = FALSE)] <- hexadecimaux(x[fini] + 0)
    sortie
  }
  doubles_texte <- function(x) {
    fini <- is.finite(x)
    if (all(fini)) return(octets_lignes(sprintf("%.17g", x + 0)))
    t <- character(length(x))
    t[fini] <- sprintf("%.17g", x[fini] + 0)
    t[!fini] <- sprintf("%.17g", x[!fini])
    octets_lignes(t)
  }
  if (is.factor(x)) return(list(type = "factor", octets = octets_chaines(as.character(x), role)))
  if (identical(class(x), "Date")) {
    # Date : sprintf("%04d-%02d-%02d") sur composantes_date() (année sur quatre
    # chiffres, ADR 0011, point 4), sur les valeurs distinctes seulement ; jeton
    # lu sur la valeur reçue : NA, ou NaN (une Date stockée NaN est admise par
    # les données et ne se confond pas avec NA, comme pour un double, point 2.2).
    # Garde-fou : une date hors du domaine n'a aucune écriture à quatre chiffres
    # (refusée en amont par `donnees`, motif commun).
    j <- as.vector(unclass(x))
    u <- unique(j)
    # Une valeur finie non entière n'a pas d'écriture : 100 et 100,5 s'écriraient
    # de même et l'aller-retour perdrait la fraction (aucune correction silencieuse).
    # Les infinis relèvent du refus de domaine ci-dessous.
    ni <- is.finite(u) & u != trunc(u)
    if (any(ni))
      refuser("rapport", "date_non_entiere",
              sprintf("Rôle « %s » : date non entière (%s), sans écriture canonique.", role,
                      paste(format(utils::head(u[ni], 3L), digits = 15L), collapse = ", ")),
              list(role = role, lignes = which(is.finite(j) & j != trunc(j))))
    hors <- dates_hors_domaine(u)
    if (any(hors))
      refuser("rapport", "date_hors_domaine",
              sprintf("Rôle « %s » : date hors du domaine [0001-01-01, 9999-12-31], sans écriture canonique.", role),
              list(role = role, lignes = which(dates_hors_domaine(j))))
    txt <- ifelse(is.nan(u), "NaN", "NA")
    k <- !is.na(u)
    if (any(k)) txt[k] <- texte_date(u[k])
    return(list(type = "Date", octets = octets_lignes(txt[match(j, u)])))
  }
  if (is.object(x))
    refuser("rapport", "type_non_canonique",
            sprintf("Rôle « %s » : classe « %s » sans représentation canonique.", role,
                    paste(class(x), collapse = "/")),
            list(role = role))
  if (is.character(x)) return(list(type = "character", octets = octets_chaines(x, role)))
  if (is.integer(x)) return(list(type = "integer", octets = octets_lignes(sprintf("%d", x))))
  if (is.double(x)) {
    x <- as.vector(x)
    return(list(type = "double", octets = if (!length(x)) raw(0) else if (forme == "empreinte") doubles_empreinte(x)
                                          else doubles_texte(x)))
  }
  if (is.logical(x)) {
    txt <- ifelse(is.na(x), "NA", ifelse(x, "TRUE", "FALSE"))
    return(list(type = "logical", octets = octets_lignes(txt)))
  }
  refuser("rapport", "type_non_canonique",
          sprintf("Rôle « %s » : type « %s » sans représentation canonique.", role, typeof(x)),
          list(role = role))
}

# Représentation canonique des données reçues (ADR 0007, point 9.3 ; ADR
# 0011, point 2) : colonnes à rôle déclaré, toutes les lignes reçues dans
# l'ordre reçu, colonnes par nom de rôle en ordre d'octets ; première ligne
# « tablexp canonique 2<TAB><lignes><TAB><colonnes> », puis par colonne
# « #<rôle><TAB><type> » et une valeur par ligne, écrite en forme
# « empreinte » ; LF, aucun CR. Écrit les octets dans `fichier` (binaire,
# writeBin) ; rend `fichier`, invisible.
texte_canonique <- function(donnees, roles, fichier) {
  r <- sort(names(roles), method = "radix")
  con <- file(fichier, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(sprintf("%s\t%d\t%d\n", ENTETE_CANONIQUE, nrow(donnees), length(r))), con)
  for (role in r) {
    cc <- ecrire_valeurs(donnees[[roles[[role]]]], "empreinte", role)
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
# algorithme) de trois chaînes non vides. Forme exacte (#21, forme_exacte()) :
# liste nue (seul l'attribut names ; ni data.frame, ni classe), exactement ces
# trois noms, chaque valeur une chaîne de longueur 1 sans attribut.
controler_provenance <- function(provenance) {
  if (is.null(provenance)) return(invisible(TRUE))
  if (!liste_aux_noms(provenance, c("fichier", "empreinte", "algorithme")) ||
      !all(vapply(provenance, chaine_unique, logical(1L))))
    refuser("rapport", "provenance_invalide",
            paste("La provenance doit être NULL (données construites en mémoire) ou une liste nue",
                  "(fichier, empreinte, algorithme) de trois chaînes non vides, sans attribut",
                  "(ni data.frame, ni classe ; ADR 0007, point 3.1)."),
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
       statut_resultat_code = texte_moteur(STATUT_EMPREINTES_NON_CALCULEES))
}

# Plan canonique (ADR 0011, point 5.1 ; #42 ; quatrième annotation du
# 20/10/2026, #63), à partir d'un plan validé par valider_plan() ; `champs`,
# `parametres` et `ordres` sont CHAMPS_PLAN, PARAMETRES_ETAPES et les vecteurs
# d'ordre des trois champs ensembles, passés par construire_table() (point 9.3 :
# le module ne lit pas les constantes des autres modules). Champs dans l'ordre
# de `champs` ; transition (depart, arrivee) ; periode (debut, fin) ; étapes
# dans l'ordre reçu (imposé par valider_plan()), paramètres de chaque étape
# dans l'ordre de parametres[[étape]]$parametre ; valeurs des champs ensembles
# dans l'ordre de `ordres`, liste nommée par le chemin du champ dans le plan
# (« segmentation », « etapes/donnees/exclusions », « etapes/brut/estimateurs ») ;
# un chemin dont l'étape est absente du plan n'a rien à canoniser ; une valeur
# hors du vecteur d'ordre, un doublon ou un attribut sont une erreur de
# programmation, jamais une valeur retirée en silence ; toute autre valeur
# inchangée. Stockages admis, liste fermée, conversions sans perte : borne de
# période Date à stockage double ou integer -> Date double ; délai integer ou
# double -> double ; graine integer, ou double entier dans l'étendue des
# integer -> integer (graine NA refusée). Un autre stockage, une étape absente
# de `parametres` ou en double, ou aux noms de paramètres en double ou autres
# que ceux du catalogue, sont une erreur de programmation (le plan n'a pas été
# validé ; un nom en double serait sinon réduit en silence à sa première
# occurrence, #42). Idempotente : un plan canonique est rendu
# identical(). Ce n'est pas un plan résolu (point 5.2 : convention demandée
# inchangée).
canoniser_plan <- function(plan, champs, parametres, ordres) {
  non_canonique <- function(champ)
    stop(sprintf("canoniser_plan() : valeur ou stockage non canonisable pour « %s » (erreur de programmation : plan non validé).",
                 champ), call. = FALSE)
  borne <- function(x, champ) {
    if (!identical(class(x), "Date") || !typeof(x) %in% c("double", "integer")) non_canonique(champ)
    structure(as.double(unclass(x)), class = "Date")
  }
  p <- plan[intersect(champs, names(plan))]
  p[["transition"]] <- p[["transition"]][c("depart", "arrivee")]
  per <- p[["periode"]]
  p[["periode"]] <- list(debut = borne(per[["debut"]], "periode$debut"), fin = borne(per[["fin"]], "periode$fin"))
  delai <- p[["delai_declaration"]]
  if (!typeof(delai) %in% c("double", "integer")) non_canonique("delai_declaration")
  p[["delai_declaration"]] <- as.double(delai)
  if ("graine" %in% names(p)) {
    g <- p[["graine"]]
    if (is.double(g) && length(g) == 1L && is.finite(g) && g == trunc(g) && abs(g) <= .Machine$integer.max)
      g <- as.integer(g)
    if (!is.integer(g) || anyNA(g)) non_canonique("graine")
    p[["graine"]] <- g
  }
  e <- p[["etapes"]]
  if (anyDuplicated(names(e))) non_canonique("etapes")
  for (n in names(e))
    if (is.null(parametres[[n]]) || anyDuplicated(names(e[[n]])) || !setequal(names(e[[n]]), parametres[[n]]$parametre))
      non_canonique(sprintf("etapes$%s", n))
  p[["etapes"]] <- stats::setNames(lapply(names(e), function(n) e[[n]][parametres[[n]]$parametre]), names(e))
  # Champs ensembles : valeurs dans l'ordre du vecteur d'ordre (contrôle avant
  # de réordonner : o[o %in% x] seul perdrait une valeur inconnue sans le dire).
  ordonner <- function(q, cle, o, chemin) {
    if (!cle[1L] %in% names(q)) return(q)
    if (length(cle) > 1L) {
      q[[cle[1L]]] <- ordonner(q[[cle[1L]]], cle[-1L], o, chemin)
      return(q)
    }
    x <- q[[cle]]
    if (!is.character(x) || !is.null(attributes(x)) || anyNA(x) || anyDuplicated(x) || !all(x %in% o))
      non_canonique(chemin)
    q[[cle]] <- o[o %in% x]
    q
  }
  for (chemin in names(ordres))
    p <- ordonner(p, strsplit(chemin, "/", fixed = TRUE)[[1L]], ordres[[chemin]], chemin)
  p
}

# Fiche de construction en mémoire (ADR 0007, point 6.2 ; A.3) : aucun
# horodatage. `plan` est le plan canonique (canoniser_plan(), ADR 0011,
# point 5.2), qui fait foi : la fiche n'en porte pas de copie au premier
# niveau (point 5.4 ; #22, point 6). Rôles dans l'ordre du catalogue
# (CATALOGUE_ROLES$role), provenance dans l'ordre (fichier, empreinte,
# algorithme) (point 5.3). Rejeu : construire_table(donnees, fiche$roles,
# fiche$plan, fiche$provenance). Le champ hypotheses_brut n'existe que si
# l'étape `brut` est exécutée : un plan sans cette étape garde les autres
# champs.
assembler_fiche <- function(roles, plan, recette, etapes, avertissements, empreintes, provenance) {
  pd <- etapes[["donnees"]][["parametres"]]
  pe <- etapes[["exposition"]][["parametres"]]
  unite <- pd$unite_observation
  fiche <- list(
    version_fiche = VERSION_FICHE,
    versions = list(tablexp = as.character(utils::packageVersion("tablexp")), R = R.version.string),
    plateforme = list(sysname = Sys.info()[["sysname"]], release = Sys.info()[["release"]],
                      machine = Sys.info()[["machine"]], blas = extSoftVersion()[["BLAS"]],
                      lapack = La_library()),
    roles = roles[order(match(names(roles), CATALOGUE_ROLES$role))],
    plan = plan,
    transition_code = pd$transition,
    fin_effective = pd$fin_effective,
    unite_observation = unite,
    avertissement_unite = if (any(avertissements$code == "unite_contrat"))
      avertissements$message[avertissements$code == "unite_contrat"][1L] else NA_character_,
    recette = recette$recette,
    convention = if (is.null(pe)) NULL else pe$convention,
    unite_exposition = if (is.null(pe)) NA_character_ else pe$unite_exposition,
    par_annee = if (is.null(pe)) NA else pe$par_annee,
    exclusions = etapes[["donnees"]][["resultat"]][["exclusions"]],
    # Lignes distinctes exclues (une ligne à deux motifs déclarés n'y compte
    # qu'une fois, `exclusions` la compte sous chaque motif ; #57).
    nombre_lignes_exclues = etapes[["donnees"]][["resultat"]][["nombre_lignes_exclues"]],
    hypothese_censure = if (is.null(pe)) NA_character_ else pe$hypothese_censure,
    graine = if ("graine" %in% names(plan)) plan[["graine"]] else NA_integer_,
    graine_mention = texte_moteur(if ("graine" %in% names(plan)) "graine du plan" else "aucune étape aléatoire"),
    generateur = GENERATEUR,
    provenance = if (is.null(provenance)) NULL else provenance[c("fichier", "empreinte", "algorithme")],
    provenance_mention = texte_moteur(if (is.null(provenance)) "aucun fichier (données construites en mémoire)"
                                      else "fichier lu par la couche de lecture"),
    empreintes = empreintes,
    avertissements = avertissements
  )
  pb <- etapes[["brut"]][["parametres"]]
  if (!is.null(pb))
    fiche$hypotheses_brut <- pb[c("estimateurs", "niveau", "exposition_initiale", "tolerance_effectif",
                                  "hypotheses", "statuts",
                                  # Kaplan-Meier retenu seulement (#15, A.9).
                                  intersect(c("ensemble_a_risque", "denominateur_instants"), names(pb)))]
  # Ordre de lecture (#64) : la fiche est construite, puis réordonnée selon la
  # constante unique ORDRE_CHAMPS_FICHE.
  ordonner_fiche(fiche, ORDRE_CHAMPS_FICHE)
}

# Réordonne `fiche` selon `ordre`, intersecté avec les champs présents
# (hypotheses_brut reste absent sans l'étape `brut`). Un champ présent hors de
# `ordre` est une erreur de programmation, jamais retiré en silence (#64).
ordonner_fiche <- function(fiche, ordre) {
  hors_ordre <- setdiff(names(fiche), ordre)
  if (length(hors_ordre))
    stop(sprintf("ordonner_fiche() : champ(s) de la fiche absent(s) de l'ordre des champs : %s",
                 paste(hors_ordre, collapse = ", ")), call. = FALSE)
  fiche[intersect(ordre, names(fiche))]
}

# Ordre des champs de la fiche (#64 ; avis d'actuary du 20/10/2026, étape
# (1)), en huit blocs :
# identification ; données (rôles, provenance, empreintes) ; plan ; paramètres
# retenus, dans l'ordre du calcul ; exclusions ; hypothèses ; aléa ;
# avertissements en dernier. Source unique de l'ordre du corps de la fiche
# écrite (ecrire_fiche()) ; BLOCS_LECTURE_FICHE en est une partition.
ORDRE_CHAMPS_FICHE <- c(
  "version_fiche", "versions", "plateforme",
  "roles", "provenance", "provenance_mention", "empreintes",
  "plan",
  "transition_code", "fin_effective", "unite_observation", "recette", "convention", "unite_exposition", "par_annee",
  "exclusions", "nombre_lignes_exclues",
  "avertissement_unite", "hypothese_censure", "hypotheses_brut",
  "graine", "graine_mention", "generateur",
  "avertissements")

# Informations d'exécution, hors de toute comparaison (ADR 0007, point 6.1).
informations_execution <- function(debut) {
  list(debut = debut, duree_secondes = as.numeric(difftime(Sys.time(), debut, units = "secs")),
       session = list(R = R.version.string, plateforme = R.version$platform,
                      systeme = utils::osVersion, locale = Sys.getlocale(),
                      paquets_attaches = .packages()))
}

# --- Sérialisation du plan et de la fiche (ADR 0011, points 6, 8, 9 et 10, troisième annotation ; #50) ---
#
# Texte ↔ objet, sans fichier (E4.4) : les écrivains rendent des octets (raw)
# UTF-8, LF, LF final, sans CR ni marque d'ordre des octets ; les lecteurs
# reçoivent des octets ; l'appelant écrit et lit le fichier (writeBin,
# readBin ; point 8). Première ligne « tablexp plan 1 » ou « tablexp fiche
# 1 » (version de la grammaire, portée par le texte seul, jamais par un champ
# de l'objet : point 10) ; puis des lignes d'en-tête « # <clé> : <valeur> »,
# hors aller-retour (le lecteur les ignore) ; puis le corps, une ligne par
# feuille, chemin<TAB>type<TAB>longueur<TAB>valeur_1<TAB>…<TAB>valeur_n
# (forme de ligne de l'ADR 0009, point 2.3), dans l'ordre de l'objet
# (parcours en profondeur), jamais trié. Chaque conteneur déclare son effectif
# (troisième annotation, (d)), si bien qu'un texte coupé à une frontière de
# ligne est toujours refusé :
#   - liste nommée (attribut names présent, même de longueur 0) : ligne
#     chemin/@noms (character, les noms dans l'ordre), puis ses éléments ;
#   - liste sans attribut names : ligne chemin/@longueur (integer, 1 valeur,
#     n), puis exactement ses éléments chemin/[1] à chemin/[n] ;
#   - data.frame : chemin/@colonnes (character), chemin/@lignes (integer),
#     puis ses colonnes ; noms de lignes automatiques seulement ;
#   - vecteur atomique nommé : chemin/@noms, puis la ligne chemin ; de
#     longueur 0, refusé à l'écriture (texte coupé après @noms relu sinon comme
#     une liste nommée vide) ;
#   - NULL : chemin<TAB>NULL<TAB>0.
# Segment de chemin d'un élément : son nom, ou [i] (rang) si le nom est
# absent, vide, NA, en double, ou s'il contient / , une tabulation ou une fin
# de ligne, ou commence par @, # ou [ ; les noms eux-mêmes sont portés par la
# ligne @noms, si bien que tout nom fait l'aller-retour. Profondeur d'une
# ligne : nombre de segments de son chemin, sans le dernier s'il commence par
# @ (racine : 0) ; au plus PROFONDEUR_MAX_TEXTE, contrôlée à l'écriture avant
# toute récursion plus profonde et à la lecture avant la reconstruction
# (troisième annotation, (g)). Types : double, integer, logical, character,
# Date, NULL ; valeurs écrites par ecrire_valeurs() en forme « texte »
# (doubles en %.17g ; point 3). Une Date à stockage integer s'écrit comme la
# même Date en double (mêmes octets) et se relit en double (troisième
# annotation, (c)). Facteur : refusé à l'écriture, type inconnu à la lecture
# (le texte de la grammaire 1 n'en porte pas les niveaux ; (a)).
#
# Lecture stricte, sans évaluation (point 6.2) : ni parse(), ni eval(), ni
# source(), ni dget() ; découpage sur LF puis sur la tabulation, conversions
# par as.numeric(), as.integer(), as.Date() sur la forme ISO contrôlée,
# désechappement des chaînes. Un double est tout décimal que as.numeric() lit
# en un double fini, ou l'un des jetons NA, NaN, Inf, -Inf ; la valeur retenue
# est le double lu (point 6.3, Q3 « lecteur de forme ») ; un décimal dont la
# mantisse porte un chiffre non nul et relu 0 est refusé (sous-dépassement,
# M66, troisième annotation, (b)) ; les autres types exigent leur écriture
# canonique. Refus nommés (module rapport), avec le numéro de la ligne fautive
# dans details$ligne. lire_plan() ne valide pas le plan (point 9.2) :
# valider_plan() par l'appelant, ou construire_table() ; lire_fiche() ne
# contrôle pas version_fiche (troisième annotation, (i)).

PREMIERE_LIGNE_PLAN <- "tablexp plan 1"
PREMIERE_LIGNE_FICHE <- "tablexp fiche 1"
TYPES_TEXTE <- c("double", "integer", "logical", "character", "Date", "NULL")
PROFONDEUR_MAX_TEXTE <- 32L

# Conventions rappelées au relecteur, en tête du texte (hors aller-retour ;
# troisième annotation, (h)) ; texte du moteur marqué UTF-8 (#50). Doubles :
# la relecture au bit près est un constat de mesure sous glibc (ADR 0011,
# point 3.2), pas une garantie.
CONVENTIONS_TEXTE <- texte_moteur(c(
  grammaire = paste(
    "une ligne par feuille : chemin<TAB>type<TAB>longueur<TAB>valeurs ; chemins joints par / ; [i] pour un élément",
    "sans nom ou dont le nom ne peut être un segment (nom porté par @noms) ; liste nommée : chemin/@noms puis ses",
    "éléments ; liste sans noms : chemin/@longueur puis [1] à [n] ; vecteur nommé : chemin/@noms puis la ligne",
    "chemin ; data.frame : chemin/@colonnes et chemin/@lignes puis ses colonnes ; NULL : chemin NULL 0 ;",
    sprintf("profondeur au plus %d", PROFONDEUR_MAX_TEXTE)),
  doubles = paste(
    "le double relu fait foi ; 17 chiffres significatifs (%.17g) ; relecture au bit près constatée sous glibc, non",
    "garantie ailleurs ; à l'import, toute écriture décimale est admise (0.95 comme 0.94999999999999996) ; refusés :",
    "décimal non fini, décimal non nul relu 0 ; jetons NA, NaN, Inf, -Inf"),
  dates = "ISO 8601, AAAA-MM-JJ, année sur quatre chiffres, domaine [0001-01-01, 9999-12-31]",
  "chaînes" = "UTF-8 entre guillemets droits, échappements \\\\ \\n \\r \\t \\\" ; NA sans guillemets, distinct de \"NA\"",
  "délais" = "en jours",
  codage = "UTF-8, fins de ligne LF"))

# Blocs de lecture de la fiche (E3.7 ; troisième annotation, (h)) : ce qui a
# été calculé, avec quelles hypothèses, de quoi rejouer ; chaque champ de la
# fiche dans un seul bloc (partition contrôlée par les tests), dans l'ordre
# des champs de chaque bloc, qui suit ORDRE_CHAMPS_FICHE (#64). Affectation validée par actuary le 20/10/2026
# (ADR 0011, point 6.5) ; nombre_lignes_exclues à côté d'exclusions, dans les
# hypothèses (avis d'actuary, M69 ; #57). Noms des blocs marqués UTF-8 (#50).
BLOCS_LECTURE_FICHE <- texte_moteur(list(
  "calculé" = c("plan", "transition_code", "fin_effective", "unite_observation", "recette", "convention",
               "unite_exposition", "par_annee"),
  "hypothèses" = c("exclusions", "nombre_lignes_exclues", "avertissement_unite", "hypothese_censure",
                  "hypotheses_brut", "avertissements"),
  rejeu = c("version_fiche", "versions", "plateforme", "roles", "provenance", "provenance_mention", "empreintes",
            "graine", "graine_mention", "generateur")))

joindre_chemin <- function(chemin, segment) if (nzchar(chemin)) paste0(chemin, "/", segment) else segment

# Segments de chemin des éléments de noms `noms` (NULL : aucun nom), de
# longueur `n` (règle ci-dessus ; même règle à l'écriture et à la lecture).
segments_texte <- function(noms, n) {
  if (is.null(noms)) return(sprintf("[%d]", seq_len(n)))
  ok <- !is.na(noms) & nzchar(noms) & validUTF8(noms) &
    !grepl("[/\t\n\r]", noms, useBytes = TRUE) & !grepl("^[@#[]", noms, useBytes = TRUE)
  ok <- ok & !noms %in% noms[ok][duplicated(noms[ok])]
  ifelse(ok, noms, sprintf("[%d]", seq_along(noms)))
}

# Octets du texte de l'objet `x` (liste), de première ligne `premiere`, avec
# les lignes d'en-tête `entete` (vecteur de chaînes nommé clé -> valeur).
texte_objet <- function(x, premiere, entete) {
  tab <- as.raw(0x09); lf <- as.raw(0x0a)
  impossible <- function(chemin, message)
    refuser("rapport", "serialisation_impossible",
            sprintf("Écriture du texte, « %s » : %s.", if (nzchar(chemin)) chemin else "(racine)", message),
            list(chemin = chemin))
  morceaux <- list()
  ajouter <- function(chemin, type, n, octets) {
    tete <- c(charToRaw(enc2utf8(chemin)), tab, charToRaw(type), tab, charToRaw(sprintf("%d", n)))
    if (length(octets)) {
      octets[octets == lf] <- tab
      octets[length(octets)] <- lf
      morceaux[[length(morceaux) + 1L]] <<- c(tete, tab, octets)
    } else morceaux[[length(morceaux) + 1L]] <<- c(tete, lf)
  }
  # Ligne @noms (attribut names présent) ; liste sans noms : ligne @longueur.
  noms_ligne <- function(v, chemin) {
    n <- names(v)
    if (is.null(n)) ajouter(joindre_chemin(chemin, "@longueur"), "integer", 1L, octets_lignes(sprintf("%d", length(v))))
    else ajouter(joindre_chemin(chemin, "@noms"), "character", length(n),
                 octets_chaines(n, joindre_chemin(chemin, "@noms")))
  }
  profonde <- function(chemin, profondeur)
    if (profondeur > PROFONDEUR_MAX_TEXTE)
      impossible(chemin, sprintf("profondeur %d au-delà de %d", profondeur, PROFONDEUR_MAX_TEXTE))
  atome <- function(v, chemin, profondeur) {
    profonde(chemin, profondeur)
    a <- names(attributes(v))
    if (is.factor(v)) impossible(chemin, "facteur, sans écriture de ses niveaux dans le texte de la grammaire 1")
    if (identical(class(v), "Date")) {
      # Date à stockage double ou integer seulement (sinon : erreur R brute, ou
      # écriture relue d'un autre type ; #50).
      if (!typeof(v) %in% c("double", "integer"))
        impossible(chemin, sprintf("Date à stockage %s (double ou integer attendu)", typeof(v)))
      if (!all(a %in% c("names", "class"))) impossible(chemin, sprintf("attribut(s) %s", paste(a, collapse = ", ")))
    } else if (is.object(v) || !typeof(v) %in% c("double", "integer", "logical", "character") ||
               !all(a %in% "names")) {
      impossible(chemin, sprintf("valeur de type %s, de classe %s ou d'attribut(s) %s, sans écriture dans le texte",
                                 typeof(v), paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
    }
    # Vecteur atomique nommé de longueur 0 : refusé (décision de la session
    # principale, revue finale de #50) ; écrit chemin/@noms character 0 puis
    # chemin, son texte coupé après la ligne @noms se relirait comme une liste
    # nommée vide (préfixe accepté, contraire à la troisième annotation, (d)).
    if (!is.null(names(v)) && !length(v))
      impossible(chemin, "vecteur atomique nommé de longueur 0, sans écriture distincte d'une liste nommée vide")
    if (!is.null(names(v))) noms_ligne(v, chemin)
    cc <- ecrire_valeurs(v, "texte", chemin)
    ajouter(chemin, cc$type, length(v), cc$octets)
  }
  noeud <- function(v, chemin, profondeur) {
    profonde(chemin, profondeur)
    if (is.null(v)) return(ajouter(chemin, "NULL", 0L, raw(0)))
    if (is.data.frame(v)) {
      a <- names(attributes(v))
      if (!identical(class(v), "data.frame") || !all(a %in% c("names", "class", "row.names")))
        impossible(chemin, sprintf("data.frame de classe %s ou d'attribut(s) %s",
                                   paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
      if (!identical(attr(v, "row.names"), seq_len(nrow(v))))
        impossible(chemin, "data.frame aux noms de lignes non automatiques")
      ajouter(joindre_chemin(chemin, "@colonnes"), "character", length(v),
              octets_chaines(names(v), joindre_chemin(chemin, "@colonnes")))
      ajouter(joindre_chemin(chemin, "@lignes"), "integer", 1L, octets_lignes(sprintf("%d", nrow(v))))
      s <- segments_texte(enc2utf8(names(v)), length(v))
      for (i in seq_along(v)) {
        if (is.null(v[[i]]) || !is.atomic(v[[i]])) impossible(joindre_chemin(chemin, s[i]), "colonne non atomique")
        atome(v[[i]], joindre_chemin(chemin, s[i]), profondeur + 1L)
      }
      return(invisible())
    }
    if (is.list(v)) {
      a <- names(attributes(v))
      if (is.object(v) || !all(a %in% "names"))
        impossible(chemin, sprintf("liste de classe %s ou d'attribut(s) %s",
                                   paste(class(v), collapse = "/"), paste(a, collapse = ", ")))
      noms_ligne(v, chemin)
      s <- segments_texte(if (is.null(names(v))) NULL else enc2utf8(names(v)), length(v))
      for (i in seq_along(v)) noeud(v[[i]], joindre_chemin(chemin, s[i]), profondeur + 1L)
      return(invisible())
    }
    if (!is.atomic(v)) impossible(chemin, sprintf("objet de type %s, sans écriture dans le texte", typeof(v)))
    atome(v, chemin, profondeur)
  }
  if (!is.list(x) || is.data.frame(x)) impossible("", "l'objet écrit doit être une liste")
  noeud(x, "", 0L)
  propre <- function(t) gsub("[\001-\037\177]", " ", enc2utf8(t), useBytes = TRUE)
  tete <- c(premiere, if (length(entete)) sprintf("# %s : %s", propre(names(entete)), propre(entete)))
  c(unlist(lapply(tete, function(l) c(charToRaw(l), lf)), use.names = FALSE),
    unlist(morceaux, use.names = FALSE))
}

# Valeurs d'une ligne lue, de type `type`, textes `s` (UTF-8) ; `refus(motif,
# message)` lève le refus de la ligne. Rend le vecteur, sans noms.
valeurs_texte <- function(type, s, refus) {
  invalide <- function(quoi, i, motif = "texte_valeur_invalide")
    refus(motif, sprintf("%s (valeur %d : %s)", quoi, i[1L], s[i[1L]]))
  switch(type,
    character = {
      na <- s == "NA"
      bien <- grepl('^"(?:[^"\\\\]|\\\\[\\\\nrt"])*+"$', s, perl = TRUE, useBytes = TRUE)
      if (any(!na & !bien))
        invalide("chaîne sans guillemets droits, guillemet nu ou échappement inconnu", which(!na & !bien))
      y <- sub('^"(.*)"$', "\\1", s[!na], perl = TRUE, useBytes = TRUE)
      # Désechappement : les paires \\ d'abord, vers l'octet 0xFF (absent de
      # tout texte UTF-8 valide), puis \n, \r, \t, \", puis 0xFF vers \.
      ff <- rawToChar(as.raw(0xff))
      y <- gsub("\\\\", ff, y, fixed = TRUE, useBytes = TRUE)
      for (r in list(c("\\n", "\n"), c("\\r", "\r"), c("\\t", "\t"), c("\\\"", "\"")))
        y <- gsub(r[1L], r[2L], y, fixed = TRUE, useBytes = TRUE)
      y <- gsub(ff, "\\", y, fixed = TRUE, useBytes = TRUE)
      Encoding(y) <- "UTF-8"
      v <- rep(NA_character_, length(s))
      v[!na] <- y
      v
    },
    double = {
      jeton <- s %in% c("NA", "NaN", "Inf", "-Inf")
      decimal <- grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", s)
      v <- suppressWarnings(as.numeric(ifelse(jeton | decimal, s, NA_character_)))
      v[s == "NA"] <- NA_real_
      faux <- !jeton & (!decimal | !is.finite(v))
      if (any(faux)) invalide("double illisible (ni décimal lu en un double fini, ni NA, NaN, Inf, -Inf)", which(faux))
      # Sous-dépassement (M66) : mantisse non nulle relue 0 (-0 compris).
      sous <- decimal & grepl("[1-9]", sub("[eE].*$", "", s)) & v == 0
      if (any(sous))
        invalide("décimal non nul relu 0 (sous-dépassement des doubles)", which(sous), "texte_double_sous_depassement")
      v
    },
    integer = {
      v <- suppressWarnings(as.integer(ifelse(grepl("^(0|-?[1-9][0-9]*)$", s), s, NA_character_)))
      faux <- s != "NA" & is.na(v)
      if (any(faux)) invalide("entier non canonique ou hors de l'étendue des integer", which(faux))
      v
    },
    logical = {
      v <- c("TRUE" = TRUE, "FALSE" = FALSE, "NA" = NA)[s]
      if (anyNA(names(v))) invalide("logique autre que TRUE, FALSE ou NA", which(is.na(names(v))))
      unname(v)
    },
    Date = {
      jeton <- s %in% c("NA", "NaN")
      forme <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)
      u <- as.double(unclass(as.Date(ifelse(forme, s, NA_character_), format = "%Y-%m-%d")))
      faux <- !jeton & (!forme | is.na(u) | dates_hors_domaine(u))
      if (!any(faux) && any(!jeton)) faux[!jeton] <- texte_date(u[!jeton]) != s[!jeton]
      if (any(faux)) invalide("date hors de la forme AAAA-MM-JJ, inexistante ou hors du domaine [0001-01-01, 9999-12-31]",
                              which(faux))
      u[s == "NaN"] <- NaN
      structure(u, class = "Date")
    },
    stop(sprintf("valeurs_texte() : type « %s » non prévu (erreur de programmation).", type), call. = FALSE))
}

# Objet (liste) relu des octets `octets` de première ligne attendue
# `premiere` ; `nature` (« plan », « fiche ») pour les messages.
lire_texte <- function(octets, premiere, nature) {
  refus <- function(motif, message, ligne = NA_integer_)
    refuser("rapport", motif,
            sprintf("Lecture du texte de %s%s : %s.", nature,
                    if (is.na(ligne)) "" else sprintf(", ligne %d", ligne), message),
            list(ligne = ligne))
  if (!is.raw(octets))
    refus("texte_non_octets", sprintf(paste("le texte doit être reçu en octets (raw), pas en %s ; lire un",
                                            "fichier par readBin(fichier, \"raw\", file.size(fichier))"),
                                      paste(class(octets), collapse = "/")))
  if (!length(octets)) refus("texte_version_inconnue", sprintf("texte vide (première ligne « %s » attendue)", premiere))
  lf <- as.raw(0x0a)
  ligne_de <- function(i) 1L + sum(octets[seq_len(i - 1L)] == lf)
  if (length(octets) >= 3L && identical(octets[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
    refus("texte_marque_ordre_octets", "marque d'ordre des octets (BOM) en tête du texte, non admise", 1L)
  cr <- which(octets == as.raw(0x0d))
  if (length(cr))
    refus("texte_retour_chariot", "retour chariot (CR) : fins de ligne CRLF ou CR non admises (LF seul)", ligne_de(cr[1L]))
  nul <- which(octets == as.raw(0x00))
  if (length(nul)) refus("texte_octet_nul", "octet nul", ligne_de(nul[1L]))
  if (octets[length(octets)] != lf) refus("texte_sans_lf_final", "dernière ligne sans LF final", ligne_de(length(octets)))
  l <- strsplit(rawToChar(octets), "\n", fixed = TRUE, useBytes = TRUE)[[1L]]
  Encoding(l) <- "UTF-8"
  faux <- which(!validUTF8(l))
  if (length(faux)) refus("texte_utf8_invalide", "octets qui ne sont pas de l'UTF-8 valide", faux[1L])
  if (!identical(l[1L], premiere))
    refus("texte_version_inconnue",
          sprintf("première ligne « %s » au lieu de « %s » (nature ou version de grammaire inconnue, aucune migration)",
                  gsub("[\001-\037\177]", " ", l[1L], useBytes = TRUE), premiere), 1L)
  k <- 2L
  while (k <= length(l) && startsWith(l[k], "#")) k <- k + 1L
  numeros <- seq.int(k, length.out = length(l) - k + 1L)
  corps <- l[numeros]
  if (!length(corps)) refus("texte_ligne_mal_formee", "texte sans corps", length(l))
  champs <- strsplit(paste0(corps, "\t"), "\t", fixed = TRUE, useBytes = TRUE)
  for (i in seq_along(champs)) {
    f <- champs[[i]]
    if (length(f) < 3L || any(!nzchar(f)) || startsWith(f[1L], "#") || !grepl("^(0|[1-9][0-9]{0,9})$", f[3L]))
      refus("texte_ligne_mal_formee",
            "ligne hors de la forme chemin<TAB>type<TAB>longueur<TAB>valeurs (champ vide, longueur illisible, ligne d'en-tête dans le corps)",
            numeros[i])
    if (!f[2L] %in% TYPES_TEXTE)
      refus("texte_type_inconnu", sprintf("type « %s » inconnu (types admis : %s)", f[2L], paste(TYPES_TEXTE, collapse = ", ")),
            numeros[i])
    if (f[2L] == "NULL" && f[3L] != "0")
      refus("texte_longueur_incoherente", sprintf("NULL : longueur 0 attendue, longueur %s déclarée", f[3L]), numeros[i])
    if (length(f) - 3L != as.numeric(f[3L]))
      refus("texte_longueur_incoherente", sprintf("longueur déclarée %s, %d valeur(s) lue(s)", f[3L], length(f) - 3L),
            numeros[i])
  }
  chemins <- vapply(champs, `[`, "", 1L)
  Encoding(chemins) <- "UTF-8"
  double_ <- which(duplicated(chemins))
  if (length(double_)) refus("texte_chemin_en_double", sprintf("chemin « %s » en double", chemins[double_[1L]]),
                             numeros[double_[1L]])
  # Profondeur, avant toute reconstruction (troisième annotation, (g)) :
  # segments du chemin, sans le dernier s'il commence par @.
  segs <- strsplit(chemins, "/", fixed = TRUE, useBytes = TRUE)
  profondeur <- lengths(segs) - vapply(segs, function(x) startsWith(x[length(x)], "@"), NA)
  trop <- which(profondeur > PROFONDEUR_MAX_TEXTE)
  if (length(trop))
    refus("texte_profondeur_excessive", sprintf("profondeur %d au-delà de %d", profondeur[trop[1L]], PROFONDEUR_MAX_TEXTE),
          numeros[trop[1L]])
  n <- length(champs)
  k <- 1L
  inattendu <- function(attendu)
    refus("texte_chemin_inattendu",
          if (k > n) sprintf("fin du texte, « %s » attendu", attendu)
          else sprintf("chemin « %s » au lieu de « %s » (structure incohérente)", chemins[k], attendu),
          if (k > n) length(l) else numeros[k])
  lue <- function(i) {
    f <- champs[[i]]
    if (f[2L] == "NULL") return(NULL)
    s <- f[-(1:3)]
    Encoding(s) <- "UTF-8"
    valeurs_texte(f[2L], s, function(motif, message) refus(motif, message, numeros[i]))
  }
  type_de <- function(i) champs[[i]][2L]
  noeud <- function(chemin) {
    if (k > n) inattendu(chemin)
    if (identical(chemins[k], chemin)) { k <<- k + 1L; return(lue(k - 1L)) }
    if (identical(chemins[k], joindre_chemin(chemin, "@longueur"))) {
      # Liste sans noms : exactement n éléments [1] à [n].
      nb <- if (type_de(k) == "integer" && champs[[k]][3L] == "1") lue(k)
      if (length(nb) != 1L || is.na(nb) || nb < 0L)
        refus("texte_ligne_mal_formee", "ligne @longueur autre qu'un integer unique positif ou nul", numeros[k])
      # Borne de l'effectif avant toute allocation : chaque élément occupe au
      # moins une ligne, il ne peut y en avoir plus que de lignes restantes.
      if (nb > n - k)
        refus("texte_longueur_incoherente",
              sprintf("@longueur %d au-delà des %d ligne(s) restantes (une ligne au moins par élément)", nb, n - k),
              numeros[k])
      k <<- k + 1L
      v <- vector("list", nb)
      for (i in seq_len(nb)) v[i] <- list(noeud(joindre_chemin(chemin, sprintf("[%d]", i))))
      return(v)
    }
    if (identical(chemins[k], joindre_chemin(chemin, "@noms"))) {
      if (type_de(k) != "character")
        refus("texte_ligne_mal_formee", sprintf("ligne @noms de type %s (character attendu)", type_de(k)), numeros[k])
      noms <- lue(k)
      k <<- k + 1L
      if (k <= n && identical(chemins[k], chemin)) {
        # Vecteur atomique nommé.
        if (type_de(k) == "NULL") refus("texte_ligne_mal_formee", "NULL nommé", numeros[k])
        v <- lue(k)
        if (length(v) != length(noms))
          refus("texte_longueur_incoherente", sprintf("%d nom(s) pour %d valeur(s)", length(noms), length(v)), numeros[k])
        k <<- k + 1L
        names(v) <- noms
        return(v)
      }
      s <- segments_texte(noms, length(noms))
      v <- vector("list", length(noms))
      for (i in seq_along(noms)) v[i] <- list(noeud(joindre_chemin(chemin, s[i])))
      names(v) <- noms
      return(v)
    }
    if (identical(chemins[k], joindre_chemin(chemin, "@colonnes")) && nzchar(chemin)) {
      if (type_de(k) != "character")
        refus("texte_ligne_mal_formee", sprintf("ligne @colonnes de type %s (character attendu)", type_de(k)), numeros[k])
      colonnes <- lue(k)
      k <<- k + 1L
      if (k > n || !identical(chemins[k], joindre_chemin(chemin, "@lignes"))) inattendu(joindre_chemin(chemin, "@lignes"))
      nl <- if (type_de(k) == "integer") lue(k)
      if (length(nl) != 1L || is.na(nl) || nl < 0L)
        refus("texte_ligne_mal_formee", "ligne @lignes autre qu'un entier positif ou nul", numeros[k])
      k <<- k + 1L
      s <- segments_texte(colonnes, length(colonnes))
      v <- vector("list", length(colonnes))
      for (i in seq_along(colonnes)) {
        cc <- joindre_chemin(chemin, s[i])
        if (k > n || !chemins[k] %in% c(cc, joindre_chemin(cc, "@noms"))) inattendu(cc)
        ligne <- numeros[k]
        x <- noeud(cc)
        if (!is.atomic(x) || is.null(x))
          refus("texte_ligne_mal_formee", sprintf("colonne « %s » non atomique", cc), ligne)
        if (length(x) != nl)
          refus("texte_longueur_incoherente", sprintf("colonne « %s » de %d valeur(s) pour %d ligne(s)", cc, length(x), nl),
                ligne)
        v[i] <- list(x)
      }
      names(v) <- colonnes
      attr(v, "row.names") <- .set_row_names(nl)
      class(v) <- "data.frame"
      return(v)
    }
    inattendu(chemin)
  }
  if (!chemins[1L] %in% c("@noms", "@longueur")) inattendu("@noms ou @longueur")
  x <- noeud("")
  if (k <= n) refus("texte_chemin_inattendu", sprintf("chemin « %s » après la fin de l'objet", chemins[k]), numeros[k])
  x
}

# Texte du plan (ADR 0011, points 6 et 8) : octets UTF-8 du plan `plan`, écrit
# tel qu'il est reçu (ordre des noms conservé ; aucune validation ni
# canonisation : le plan de la fiche est déjà canonique, point 5.2 ; une Date
# integer s'écrit comme en double, troisième annotation, (c)).
ecrire_plan <- function(plan) {
  texte_objet(plan, PREMIERE_LIGNE_PLAN, CONVENTIONS_TEXTE)
}

# Plan relu des octets `octets` (raw), non validé (point 9.2) : le passer à
# valider_plan(), ou à construire_table() qui le valide.
lire_plan <- function(octets) {
  lire_texte(octets, PREMIERE_LIGNE_PLAN, "plan")
}

# Texte de la fiche de construction `fiche` (x$fiche d'un objet
# `construction`), en octets ; l'en-tête rappelle aussi version_fiche, la
# représentation canonique de l'empreinte des données, les blocs de lecture
# (BLOCS_LECTURE_FICHE, champs présents dans la fiche) et le rejeu (E3.7).
ecrire_fiche <- function(fiche) {
  if (inherits(fiche, "construction"))
    refuser("rapport", "serialisation_impossible",
            "Écriture du texte de fiche : objet `construction` reçu ; écrire sa fiche, x$fiche.",
            list(chemin = ""))
  rappel <- function(v) if (is.atomic(v) && length(v) == 1L && !is.na(v)) as.character(v) else "absente"
  rep_ <- if (is.list(fiche) && is.list(fiche[["empreintes"]])) fiche[["empreintes"]][["representation"]]
  presents <- if (is.list(fiche)) names(fiche)
  blocs <- vapply(names(BLOCS_LECTURE_FICHE), function(b)
    sprintf("%s : %s", b, paste(intersect(BLOCS_LECTURE_FICHE[[b]], presents), collapse = ", ")), "")
  texte_objet(fiche, PREMIERE_LIGNE_FICHE,
              c(CONVENTIONS_TEXTE,
                version_fiche = rappel(if (is.list(fiche)) fiche[["version_fiche"]]),
                representation = rappel(rep_),
                lecture = paste(blocs, collapse = " | "),
                # Note de lecture d'E3.7 (docs/exigences.md), en une ligne.
                rejeu = texte_moteur(paste(
                  "vérifier version_fiche, empreintes/representation et empreintes/donnees ; exécuter",
                  "construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance) sur le plan relu",
                  "de ce texte ; comparer à tolérance par classes (ADR 0009) sur une autre machine, au bit",
                  "près sur la même (E4.6)"))))
}

# Fiche relue des octets `octets` (raw). Ne contrôle pas version_fiche :
# c'est l'affaire du rejeu (troisième annotation, (i) ; point 12 (1)). Rejeu
# (E3.7) : construire_table(donnees, f$roles, f$plan, f$provenance).
lire_fiche <- function(octets) {
  lire_texte(octets, PREMIERE_LIGNE_FICHE, "fiche")
}
