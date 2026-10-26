# -----------------------------------------------------------------------------
#  R/rapport.R : module `rapport`, fiche de construction en mémoire et
#  empreintes (issue #11 ; ADR 0005, point 6 ; ADR 0007, points 6.2, 9 et
#  12 ; spécification d'actuary, A.3, « Fiche »)
#
#  - BASE_TRANCHES, diviser_tranche(), reporter_retenues(),
#    table_puissances_tranches(), PUISSANCES_TRANCHES (constante
#    d'installation), chiffres_17(), positions_17g(), octets_17g() : écriture
#    %.17g correctement arrondie des doubles, calculée par tablexp sur des
#    entiers exacts, sans la bibliothèque C (#104 ; M148) ;
#  - ecrire_valeurs() : écrivain unique des valeurs d'une colonne, en octets,
#    en deux formes, « empreinte » (doubles en octets IEEE 754) et « texte »
#    (doubles en %.17g, par octets_17g()) (ADR 0011, points 2 et 3 ; #35 ;
#    #104) ; texte_date() : écriture ISO d'une date, partagée avec le lecteur
#    (#19, #50) ;
#  - texte_canonique() : représentation textuelle canonique des données
#    reçues (ADR 0007, point 9.3 ; ADR 0011, point 2), en octets ;
#  - empreinte_donnees(), empreintes_construction() : MD5 par
#    tools::md5sum() sur un fichier temporaire écrit en binaire ; algorithme
#    nommé ; jamais serialize(), dont l'en-tête porte la version de R
#    (point 9.4) ;
#  - empreinte_resultat() : empreinte du résultat, MD5 du texte de l'objet en
#    forme « empreinte », première ligne « tablexp resultat 1 » (ADR 0011,
#    point 11 ; cinquième et sixième annotations du 20/10/2026 ; septième,
#    M71 : périmètre CHEMINS_HORS_RESULTAT ; #67) ;
#  - empreinte_sources(), EMPREINTE_CODE : empreinte du code, MD5 des sources
#    R/*.R calculé à l'installation par le code de premier niveau (option
#    (iv), M70 ; #67) ;
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
#  - bibliotheque_c() : bibliothèque C de fiche$plateforme$bibliotheque_c,
#    lue par extSoftVersion(), sinon mention « non relevée » (#77) ;
#  - ordonner_fiche() : réordonnancement pur de la fiche selon un ordre de
#    champs, erreur de programmation si un champ présent n'y figure pas (#64) ;
#  - informations_execution() : horodatage, durée, session abrégée, hors de
#    toute comparaison (point 6.1) ;
#  - ecrire_plan(), lire_plan(), ecrire_fiche(), lire_fiche() : sérialisation
#    du plan et de la fiche, texte en octets UTF-8 <-> objet, lecture sans
#    évaluation, fichier chez l'appelant (ADR 0011, points 6, 8 et 9 ; #50) ;
#    exportées (E5.4, E3.7) ; lire_plan() ne valide pas le plan (point 9.2) ;
#  - ecrire_rapport() : rapport figé HTML en octets, sans calcul, après
#    contrôle d'intégrité de l'objet (ADR 0012, point 2 ; #87) ; exportée ;
#    section « Rapport figé » en fin de fichier.
#
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
# Fiche 4 : champ bibliotheque_c sous plateforme (#77) ; sans l'incrément, son
# absence dans une fiche 3 se confondrait avec la mention « non relevée »
# (point 10 (c)). Le champ est hors empreinte du résultat (fiche/plateforme est
# dans CHEMINS_HORS_RESULTAT), l'incrément la change (fiche/version_fiche
# n'y est pas).
VERSION_FICHE <- 4L
# Versions de fiche admises au rejeu outillé (rejouer_fiche(), ADR 0012,
# point 3.5 ; #86) : la version courante seule ; ici, et non dans
# R/construire_table.R, évalué avant ce fichier. Élargir demande, version par
# version, une annotation qui montre que les champs lus par le rejeu ont la
# même structure et le même sens.
VERSIONS_FICHE_REJEU <- VERSION_FICHE
ENTETE_CANONIQUE <- "tablexp canonique 2"
ALGORITHME_EMPREINTE <- "md5"
# Première ligne du texte empreinté du résultat, consignée dans
# fiche$empreintes$representation_resultat : cinquième version du point 10
# (ADR 0011, cinquième annotation du 20/10/2026, (a) ; #67).
REPRESENTATION_RESULTAT <- "tablexp resultat 1"
# Statut des empreintes du résultat et du code (cinquième annotation du
# 20/10/2026, (d)), réunis dans fiche$empreintes$statut_resultat_code par
# « ; ». Littéraux laissés tels quels, marqués UTF-8 à l'emploi (#50) ;
# STATUT_EMPREINTE_RESULTAT est la sonde de l'installation en UTF-8 des
# batteries (tests/harnais.R, sonde_utf8(), M54).
STATUT_EMPREINTE_RESULTAT <- "résultat : témoin d'identité sur la même machine seulement (E3.6, E4.6), jamais critère de rejeu chez un tiers"
STATUT_EMPREINTE_CODE <- paste("code : MD5 du relevé des MD5 des fichiers R/*.R, fait à l'installation (recalcul depuis",
                               "la racine des sources, avec le md5sum de GNU coreutils : LC_ALL=C md5sum -t R/*.R | md5sum) ;",
                               "identifie le texte source, non le comportement numérique, qui dépend aussi de R, du BLAS et de la libm")
STATUT_CODE_INTROUVABLE <- "code : non calculée, sources introuvables à l'installation"

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

# Octets des lignes textes[i] (une valeur par ligne, LF final), égaux à
# octets_lignes(textes[i]) : chaque valeur de `textes` est écrite une fois,
# puis ses octets sont recopiés par indice (sequence()), sans une chaîne par
# ligne (#71). `textes` : ASCII (dates) ; `i` : indices dans `textes`, sans NA.
octets_lignes_indices <- function(textes, i) {
  if (!length(i)) return(raw(0))
  octets <- octets_lignes(textes)
  longueur <- nchar(textes, type = "bytes") + 1L
  debut <- cumsum(longueur) - longueur + 1L
  octets[sequence(longueur[i], from = debut[i])]
}

# Octets de sprintf("%d", x) (integer), une valeur par ligne, LF final, écrits
# en raw sans chaîne intermédiaire (#71) : une colonne d'octets par valeur
# (signe, nc chiffres, LF ; nc chiffres de max(|x|), deux au moins pour
# « NA »), les places inoccupées à 0x00, retirées à la fin (aucun octet 0x00
# dans l'écriture). Chiffre de rang k > 0 écrit si le quotient par 10^k est
# non nul (aucun zéro de tête) ; NA s'écrit « NA ».
octets_entiers <- function(x) {
  if (!length(x)) return(raw(0))
  x <- as.vector(x)
  na <- is.na(x)
  q <- abs(x)
  q[na] <- 0L
  nc <- max(2L, nchar(sprintf("%d", max(q))))
  rangs <- vector("list", nc + 2L)
  rangs[[nc + 2L]] <- rep.int(as.raw(0x0a), length(x))
  unite <- q %% 10L + 48L
  unite[na] <- 0x41L
  rangs[[nc + 1L]] <- as.raw(unite)
  for (r in nc:2) {
    q <- q %/% 10L
    rangs[[r]] <- as.raw((q %% 10L + 48L) * (q > 0L))
  }
  rangs[[nc]][na] <- as.raw(0x4e)
  rangs[[1L]] <- as.raw(0x2dL * (!na & x < 0L))
  m <- do.call(rbind, rangs)
  m[m != as.raw(0L)]
}

# Écriture ISO 8601 AAAA-MM-JJ, année sur quatre chiffres (ADR 0011, point 4 ;
# #19), de jours `u` depuis 1970-01-01 entiers, finis et dans le domaine
# (contrôlés par l'appelant) ; partagée par l'écrivain (ecrire_valeurs()) et
# par le contrôle de canonicité du lecteur (valeurs_texte()) (#19, #50).
texte_date <- function(u) {
  cd <- composantes_date(u)
  sprintf("%04d-%02d-%02d", cd$an, cd$mois + 1L, cd$jour)
}

# --- Écriture %.17g des doubles, calculée par tablexp (#104 ; M148) ---------
#
# Forme « texte » des doubles finis : 17 chiffres significatifs (conversion g
# de la norme C, précision 17), arrondi au plus proche, égalité vers le chiffre
# pair, calculés sans la bibliothèque C (ADR 0011, point 2.6, option (v) ;
# spécification d'actuary-approfondi sur #104, algorithme V1, invariants I1 à
# I5). Aucun double n'est mis en forme par sprintf(), format(), formatC() ni
# as.character() : les chiffres sont calculés sur des entiers exacts (< 2^53),
# puis écrits en octets ASCII ; %% et %/% portent sur des entiers seulement.
# Dérivé du prototype de l'étape (3a), tests/mesures/ecriture_17g/
# ecriture_17g.R. Témoins : batterie unitaire (T1 à T3) et contrôle hors
# batterie tests/controles/ecriture_doubles_texte.R (juges glibc et Python).

# Base des tranches des grands entiers (poids faible en colonne 1) : 10^7. Un
# produit de deux tranches est < 10^14, une somme de trois produits alignés
# < 3·10^14, un produit par 5 ou 2 < 5·10^7 : tout reste entier < 2^53, donc
# exact en double (I3).
BASE_TRANCHES <- 1e7

# Quotient et reste de v (entiers >= 0 < 2^53, vecteur ou matrice) par la base
# : q = floor(v / B), corrigé d'une unité si l'arrondi de v / B l'a décalé
# (r < 0 : q - 1 ; r >= B : q + 1), ce qui rend q et r exacts quel que soit cet
# arrondi (I4). Ni %% ni %/% sur des doubles.
diviser_tranche <- function(v) {
  q <- floor(v / BASE_TRANCHES)
  r <- v - q * BASE_TRANCHES
  k <- r < 0
  q <- q - k
  r <- r + k * BASE_TRANCHES
  k <- r >= BASE_TRANCHES
  list(q = q + k, r = r - k * BASE_TRANCHES)
}

# Report des retenues d'une matrice de grands entiers (une ligne par entier,
# tranches de poids croissant en colonnes), jusqu'à ce qu'aucune retenue ne
# reste. L'appelant donne assez de colonnes pour la valeur (Z < B^ncol) : la
# dernière colonne ne produit alors aucune retenue (garde-fou).
reporter_retenues <- function(v) {
  w <- ncol(v)
  repeat {
    d <- diviser_tranche(v)
    if (!any(d$q != 0)) return(d$r)
    if (any(d$q[, w] != 0))
      stop("reporter_retenues() : dépassement de la dernière tranche (erreur de programmation).", call. = FALSE)
    v <- d$r
    v[, -1L] <- v[, -1L] + d$q[, -w]
  }
}

# Table des puissances en tranches, constante d'installation : ligne e + 1075,
# e de -1074 à 1023, porte 5^-e si e < 0 et 2^e si e >= 0 (assez pour tout
# double fini non nul, I1), par multiplications successives par 5 et par 2 ;
# `longueur` : nombre de tranches de chaque ligne (108 au plus, 5^1074).
table_puissances_tranches <- function() {
  lignes <- vector("list", 2098L)
  suivante <- function(t, facteur) {
    t <- reporter_retenues(matrix(c(t * facteur, 0), nrow = 1L))[1L, ]
    if (t[length(t)] == 0) t[-length(t)] else t
  }
  t <- 1
  lignes[[1075L]] <- t
  for (n in 1:1074) {
    t <- suivante(t, 5)
    lignes[[1075L - n]] <- t
  }
  t <- 1
  for (e in 1:1023) {
    t <- suivante(t, 2)
    lignes[[1075L + e]] <- t
  }
  longueur <- lengths(lignes)
  tranches <- matrix(0, length(lignes), max(longueur))
  for (k in seq_along(lignes)) tranches[k, seq_len(longueur[k])] <- lignes[[k]]
  list(tranches = tranches, longueur = longueur)
}
PUISSANCES_TRANCHES <- table_puissances_tranches()

# Dix-sept chiffres de a (doubles finis > 0), arrondis au plus proche, égalité
# au pair, et exposant décimal X : a ~ d1,d2...d17 · 10^X (I5). Étapes :
#  - I1 : a = m·2^e exactement, m impair < 2^53, lus sur les octets IEEE 754
#    (champ d'exposant et fraction recomposés en entiers exacts) ;
#  - I2 : a = Z·10^-s exactement, Z = m·2^e et s = 0 si e >= 0, Z = m·5^-e et
#    s = -e sinon ; Z en tranches, produit de m (trois tranches r0, r1, q2) par
#    la ligne e de PUISSANCES_TRANCHES ;
#  - arrondi sur les quatre tranches hautes de Z (au moins 22 chiffres, dont
#    les 18 premiers) ; les chiffres au-delà du 18e et les tranches plus basses
#    disent seulement si un reste non nul existe.
# Les valeurs sont groupées par classe de longueur de la ligne de la table
# (huit tranches par classe), et chaque groupe découpé pour qu'une matrice de
# tranches compte au plus 2^18 éléments (mémoire bornée, #83).
chiffres_17 <- function(a) {
  n <- length(a)
  o <- matrix(as.integer(writeBin(a, raw(), size = 8L, endian = "little")), nrow = 8L)
  champ <- (o[8L, ] %% 128L) * 16L + o[7L, ] %/% 16L
  f <- o[7L, ] %% 16L
  for (j in 6:1) f <- f * 256 + o[j, ]
  normal <- champ > 0L
  m <- f + normal * 2^52
  e <- ifelse(normal, champ - 1075L, -1074L)
  # m impair (I1) : divisions par 2 exactes, tant que m est pair.
  k <- seq_len(n)
  repeat {
    k <- k[floor(m[k] / 2) * 2 == m[k]]
    if (!length(k)) break
    m[k] <- m[k] / 2
    e[k] <- e[k] + 1L
  }
  d1 <- diviser_tranche(m)
  d2 <- diviser_tranche(d1$q)
  r0 <- d1$r
  r1 <- d2$r
  q2 <- d2$q
  ligne <- e + 1075L
  longueur <- PUISSANCES_TRANCHES$longueur[ligne]
  classe <- (longueur + 7L) %/% 8L
  # Quatre tranches hautes de Z (de la plus haute à la plus basse), rang de la
  # plus haute, reste non nul sous elles.
  hautes <- matrix(0, n, 4L)
  rang <- integer(n)
  reste <- logical(n)
  for (cl in sort(unique(classe))) {
    i_classe <- which(classe == cl)
    lg <- max(longueur[i_classe])
    # Trois colonnes de zéros sous Z (colonnes 1 à 3), Z en colonnes 4 à lg + 6.
    largeur <- lg + 6L
    taille <- max(1L, 262144L %/% largeur)
    for (debut in seq(1L, length(i_classe), by = taille)) {
      i <- i_classe[debut:min(length(i_classe), debut + taille - 1L)]
      t <- PUISSANCES_TRANCHES$tranches[ligne[i], seq_len(lg), drop = FALSE]
      z <- matrix(0, length(i), largeur)
      z[, 3L + seq_len(lg)] <- r0[i] * t
      z[, 4L + seq_len(lg)] <- z[, 4L + seq_len(lg)] + r1[i] * t
      z[, 5L + seq_len(lg)] <- z[, 5L + seq_len(lg)] + q2[i] * t
      z <- reporter_retenues(z)
      non_nul <- (z != 0) + 0
      haut <- max.col(non_nul, ties.method = "last")
      bas <- max.col(non_nul, ties.method = "first")
      lignes_i <- seq_along(i)
      for (j in 1:4) hautes[i, j] <- z[cbind(lignes_i, haut - j + 1L)]
      rang[i] <- haut - 3L
      reste[i] <- bas < haut - 3L
    }
  }
  # Vingt-huit chiffres des quatre tranches hautes (sept par tranche, rang 6 à
  # 0), en entiers.
  p10 <- c(1000000L, 100000L, 10000L, 1000L, 100L, 10L, 1L)
  ch <- matrix(as.integer(hautes)[rep(seq_len(n), 28L) + n * rep(rep(0:3, each = 7L), each = n)], n)
  ch <- ch %/% rep(rep(p10, 4L), each = n) %% 10L
  # Premier chiffre non nul dans la tranche haute (non nulle : a > 0), N
  # chiffres de Z, puis d1...d18 et reste au-delà du 18e chiffre.
  decalage <- max.col((ch[, 1:7, drop = FALSE] != 0) + 0, ties.method = "first") - 1L
  nb_chiffres <- 7L * (rang - 1L) + 7L - decalage
  exposant <- nb_chiffres - 1L - pmax(0L, -e)
  d <- matrix(ch[cbind(rep(seq_len(n), 18L), rep(decalage, 18L) + rep(1:18, each = n))], n)
  reste <- reste | rowSums(ch * (col(ch) > decalage + 18L)) > 0
  # Arrondi au plus proche, égalité au pair : vers le haut si d18 > 5, ou
  # d18 = 5 et (reste non nul ou d17 impair) ; retenue sur les 17 chiffres ;
  # 10^17 devient 1 suivi de seize zéros, exposant + 1.
  plus <- d[, 18L] > 5L | (d[, 18L] == 5L & (reste | d[, 17L] %% 2L == 1L))
  d <- d[, 1:17, drop = FALSE]
  for (j in 17:1) {
    if (!any(plus)) break
    d[plus, j] <- d[plus, j] + 1L
    plus <- plus & d[, j] == 10L
    d[plus, j] <- 0L
  }
  d[plus, 1L] <- 1L
  exposant[plus] <- exposant[plus] + 1L
  list(chiffres = d, exposant = exposant)
}

# Octets ASCII de %.17g (conversion g de la norme C, précision 17, sans #)
# pour des doubles finis non nuls y, une colonne par valeur, une ligne par
# position, 0 aux places inoccupées : notation fixe si -4 <= X < 17 (16 - X
# décimales), sinon d.ddd...e±XX (exposant sur deux chiffres au moins) ; zéros
# de fin retirés, point retiré s'il ne reste rien après lui ; « - » en tête si
# y < 0.
positions_17g <- function(y) {
  c17 <- chiffres_17(abs(y))
  d <- c17$chiffres
  x <- c17$exposant
  n <- length(y)
  signe <- (y < 0) + 0L
  significatifs <- max.col((d != 0) + 0, ties.method = "last")
  fixe <- x >= -4L & x < 17L
  entier <- fixe & x >= 0L
  # Mantisse : `zeros` zéros puis les chiffres 1 à `nd` de d, point après la
  # position `pp` s'il reste des chiffres après lui ; puis l'exposant (« e »,
  # signe, `ne` chiffres).
  zeros <- ifelse(fixe & x < 0L, -x, 0L)
  nd <- ifelse(entier, pmax(significatifs, x + 1L), significatifs)
  pp <- ifelse(entier, x + 1L, 1L)
  point <- zeros + nd > pp
  lm <- zeros + nd + point
  ax <- abs(x)
  ne <- ifelse(fixe, 0L, ifelse(ax >= 100L, 3L, 2L))
  le <- ifelse(fixe, 0L, 2L + ne)
  largeur <- max(signe + lm + le)
  par_valeur <- function(v) rep(v, each = largeur)
  colonne <- rep(seq_len(n), each = largeur)
  j <- rep(seq_len(largeur), n) - par_valeur(signe)
  sortie <- integer(largeur * n)
  sortie[j == 0L] <- 45L
  lm_j <- par_valeur(lm)
  k <- j >= 1L & j <= lm_j
  pp_j <- par_valeur(pp) + 1L
  est_point <- k & par_valeur(point) & j == pp_j
  sortie[est_point] <- 46L
  k <- k & !est_point
  rang <- j[k] - (par_valeur(point)[k] & j[k] > pp_j[k]) - par_valeur(zeros)[k]
  sortie[k] <- 48L + ifelse(rang >= 1L, d[cbind(colonne[k], pmax(rang, 1L))], 0L)
  u <- j - lm_j
  le_j <- par_valeur(le)
  sortie[u == 1L & le_j > 0L] <- 101L
  k <- u == 2L & le_j > 0L
  sortie[k] <- ifelse(x[colonne[k]] < 0L, 45L, 43L)
  k <- u >= 3L & u <= le_j
  q <- par_valeur(ne)[k] - (u[k] - 2L)
  sortie[k] <- 48L + ax[colonne[k]] %/% c(1L, 10L, 100L)[q + 1L] %% 10L
  matrix(sortie, largeur)
}

# Octets de l'écriture « texte » d'un vecteur de doubles (une valeur par
# ligne, LF final), appelée par ecrire_valeurs() (doubles_texte()) ; égaux à
# ceux de sprintf("%.17g") de la glibc et de format(x, '.17g') de Python sur
# les témoins (constat de mesure, tests/controles/ecriture_doubles_texte.R) :
# un fini s'écrit depuis x + 0 (-0 devient 0, « 0 ») ; un non fini par son
# jeton, lu sur la valeur reçue (NA si is.na & !is.nan, NaN, Inf, -Inf), comme
# doubles_empreinte(). Par blocs de 2^14 valeurs (mémoire bornée, mesurée à
# l'étape (3a)).
octets_17g <- function(x) {
  n <- length(x)
  if (!n) return(raw(0))
  bloc <- 16384L
  if (n > bloc) {
    debuts <- seq(1L, n, by = bloc)
    return(unlist(lapply(debuts, function(i) octets_17g(x[i:min(n, i + bloc - 1L)])), use.names = FALSE))
  }
  fini <- is.finite(x)
  y <- x[fini] + 0
  nul <- y == 0
  p <- if (any(!nul)) positions_17g(y[!nul]) else matrix(0L, 0L, 0L)
  largeur <- max(4L, nrow(p))
  m <- matrix(0L, largeur + 1L, n)
  m[largeur + 1L, ] <- 10L
  colonnes <- which(fini)
  if (any(!nul)) m[seq_len(nrow(p)), colonnes[!nul]] <- p
  m[1L, colonnes[nul]] <- 48L
  z <- x[!fini]
  jeton <- ifelse(is.nan(z), 2L, ifelse(is.na(z), 1L, ifelse(z > 0, 3L, 4L)))
  jetons <- list(c(78L, 65L), c(78L, 97L, 78L), c(73L, 110L, 102L), c(45L, 73L, 110L, 102L))
  for (k in seq_along(jetons)) {
    i <- which(!fini)[jeton == k]
    if (length(i)) m[seq_along(jetons[[k]]), i] <- jetons[[k]]
  }
  as.raw(m[m != 0L])
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
# porte sur les quatre jetons). Forme « texte » : %.17g correctement arrondi,
# calculé par tablexp (octets_17g(), #104), un non fini par son jeton (NA, NaN,
# Inf, -Inf), lu sur la valeur reçue comme en forme « empreinte ».
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
  doubles_texte <- function(x) octets_17g(x)
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
    return(list(type = "Date", octets = octets_lignes_indices(txt, match(j, u))))
  }
  if (is.object(x))
    refuser("rapport", "type_non_canonique",
            sprintf("Rôle « %s » : classe « %s » sans représentation canonique.", role,
                    paste(class(x), collapse = "/")),
            list(role = role))
  if (is.character(x)) return(list(type = "character", octets = octets_chaines(x, role)))
  if (is.integer(x)) return(list(type = "integer", octets = octets_entiers(x)))
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

# Empreintes de la fiche (ADR 0007, point 9.2). Précondition : roles et
# provenance ont passé controler_roles() et controler_provenance().
# Les chaînes de l'appelant que la fiche écrit (valeurs des rôles, c'est-à-dire
# noms de colonnes, et provenance) ne passent pas par empreinte_donnees(), qui
# n'écrit que les valeurs des colonnes : une chaîne non ASCII de codage
# indéterminé (nom de colonne ou chemin natif sous la locale C) n'était
# refusée qu'à l'écriture de la fiche, par empreinte_resultat(), en erreur de
# programmation après tous les calculs (#106). Elles sont contrôlées ici, avant
# tout calcul, par octets_chaines(), mêmes refus et mêmes chemins
# (« fiche/roles », « fiche/provenance/<champ> »).
empreintes_construction <- function(donnees, roles, provenance) {
  octets_chaines(unname(roles), "fiche/roles")
  for (champ in names(provenance))
    octets_chaines(provenance[[champ]], joindre_chemin("fiche/provenance", champ))
  list(algorithme = ALGORITHME_EMPREINTE, representation = ENTETE_CANONIQUE,
       donnees = empreinte_donnees(donnees, roles),
       fichier = if (is.null(provenance)) NA_character_ else provenance$empreinte,
       algorithme_fichier = if (is.null(provenance)) NA_character_ else provenance$algorithme,
       representation_resultat = REPRESENTATION_RESULTAT,
       # Posée par construire_table() sur l'objet assemblé (empreinte_resultat()).
       resultat = NA_character_,
       code = EMPREINTE_CODE,
       statut_resultat_code = statut_empreintes(EMPREINTE_CODE))
}

# Statut des empreintes du résultat et du code (cinquième annotation du
# 20/10/2026, (d)), selon l'empreinte du code `code` (NA : sources
# introuvables à l'installation) ; texte du moteur marqué UTF-8 (#50).
statut_empreintes <- function(code) {
  texte_moteur(paste(STATUT_EMPREINTE_RESULTAT, if (is.na(code)) STATUT_CODE_INTROUVABLE else STATUT_EMPREINTE_CODE,
                     sep = " ; "))
}

# MD5 (ALGORITHME_EMPREINTE) d'un vecteur d'octets, par un fichier temporaire
# écrit en binaire et supprimé à la sortie (ADR 0007, point 9.1).
md5_octets <- function(octets) {
  f <- tempfile("tablexp_empreinte_")
  on.exit(unlink(f))
  writeBin(octets, f)
  unname(tools::md5sum(f))
}

# Périmètre de l'empreinte du résultat (ADR 0011, point 11.1 ; #67) : l'objet
# `construction` entier moins ces chemins, liste fermée. execution,
# fiche$versions, fiche$plateforme et les trois derniers champs de
# fiche$empreintes : point 11.1 (les deux empreintes qui ne portent pas sur le
# résultat et le statut qui les décrit). etapes/donnees/resultat/
# lignes_conservees (décision du mainteneur M71, Q1, option (a)) : ce sont
# les lignes de l'entrée telles que reçues (l'entrée elle-même sans
# exclusion), toutes colonnes, classe, attributs et noms de lignes de
# l'utilisateur compris ; la grammaire 1 en
# refuse une partie admise par le moteur (facteur, POSIXct, noms de lignes non
# automatiques dès qu'une exclusion retire une ligne, classe tbl_df, attribut
# quelconque, dont `simulation`, sixième annotation du 20/10/2026, point 2),
# mesuré. Copie de l'entrée, non calculée : l'entrée est identifiée, dans ses
# colonnes de rôle, par l'empreinte des données (M71, point 1 ; les colonnes
# hors rôle ne sont pas couvertes, ADR 0007, point 9.3) et les lignes
# retenues par numeros_conserves, dans le périmètre. L'attribut `simulation`
# sort avec elle (son exclusion par son nom, M70 point 2, devient sans objet).
CHEMINS_HORS_RESULTAT <- c("execution", "fiche/versions", "fiche/plateforme", "fiche/empreintes/resultat",
                           "fiche/empreintes/code", "fiche/empreintes/statut_resultat_code",
                           "etapes/donnees/resultat/lignes_conservees")

# Empreinte du résultat (ADR 0011, point 11.1 ; cinquième annotation du
# 20/10/2026, (a) et (b) ; #67) : MD5 du texte de l'objet `construction` x
# (texte_objet(), grammaire de ligne du point 6.1, valeurs en forme
# « empreinte », première ligne REPRESENTATION_RESULTAT, sans ligne d'en-tête
# #), privé des chemins CHEMINS_HORS_RESULTAT. La classe `construction` de la
# racine est vérifiée, non écrite (la première ligne en tient lieu). Tout
# attribut hors de ceux de la grammaire, et toute valeur sans écriture dans le
# texte, est une erreur de programmation : rien n'est ignoré en silence.
# Statut : même machine seulement (point 11.2), témoin d'identité.
empreinte_resultat <- function(x) {
  programmation <- function(message)
    stop(sprintf("empreinte_resultat() : %s (erreur de programmation).", message), call. = FALSE)
  if (!is.list(x) || !identical(class(x), "construction"))
    programmation(sprintf("objet de classe %s au lieu de construction", paste(class(x), collapse = "/")))
  retirer <- function(v, segments) {
    if (!is.list(v) || !segments[1L] %in% names(v)) return(v)
    if (length(segments) == 1L) v[[segments]] <- NULL
    else v[[segments[1L]]] <- retirer(v[[segments[1L]]], segments[-1L])
    v
  }
  x <- unclass(x)
  for (chemin in CHEMINS_HORS_RESULTAT) x <- retirer(x, strsplit(chemin, "/", fixed = TRUE)[[1L]])
  octets <- tryCatch(texte_objet(x, REPRESENTATION_RESULTAT, character(0), "empreinte"),
                     refus_tablexp = function(e) programmation(conditionMessage(e)))
  md5_octets(octets)
}

# Empreinte des sources du package (option (iv), M70 ; cinquième annotation
# du 20/10/2026, (c)) : MD5 du texte « <md5><2 espaces>R/<fichier> », une
# ligne par fichier R/*.R, fichiers triés par octets, LF ; égale à
# `LC_ALL=C md5sum -t R/*.R | md5sum` (-t : mode texte ; sans lui, le md5sum de
# Rtools écrit « * » devant les noms, rapporté en #107). `racine` doit être la racine des sources
# de tablexp (DESCRIPTION lisible, Package: tablexp, R/rapport.R présent) ;
# sinon, ou si un fichier de R/ est illisible, NA : rien n'est deviné.
# Fonctions de base seulement, aucune constante d'un autre module (ordre
# alphabétique d'évaluation des fichiers de R/).
empreinte_sources <- function(racine) {
  description <- file.path(racine, "DESCRIPTION")
  paquet <- if (file.exists(description))
    tryCatch(read.dcf(description, fields = "Package")[1L, 1L], error = function(e) NA_character_)
  if (!identical(unname(paquet), "tablexp") || !file.exists(file.path(racine, "R", "rapport.R")))
    return(NA_character_)
  fichiers <- sort(list.files(file.path(racine, "R"), pattern = "[.]R$"), method = "radix")
  md5 <- unname(tools::md5sum(file.path(racine, "R", fichiers)))
  # Fichier illisible (md5sum rend NA) : sources introuvables, jamais un
  # relevé haché avec « NA » (#67, audit).
  if (anyNA(md5)) return(NA_character_)
  md5_octets(charToRaw(paste0(md5, "  R/", fichiers, "\n", collapse = "")))
}

# Empreinte du code (option (iv), M70) : évaluée une seule fois, par R CMD
# INSTALL, qui exécute le code de premier niveau depuis la racine des sources
# (répertoire courant ; constat de plateforme sous Linux et R 4.3.3, non
# documenté par R, surveillé par le test d'égalité au recalcul de la batterie)
# et range la valeur dans la base de chargement différé : à l'exécution, le
# moteur lit cette chaîne et n'ouvre aucun fichier (E4.4 ; ADR 0011, point
# 8.1). NA si les sources sont introuvables : l'installation n'échoue pas, le
# statut le dit (STATUT_CODE_INTROUVABLE).
EMPREINTE_CODE <- empreinte_sources(".")

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

# Bibliothèque C consignée dans fiche$plateforme$bibliotheque_c (#77) : valeur
# de extSoftVersion()[["iconv"]], que R 4.3.3 construit dans do_eSoftVersion()
# (src/main/platform.c, l. 3592-3610) selon quatre formes, testées dans cet
# ordre : « GNU libiconv X.Y » (ou « Apple or GNU libiconv X.Y » sous macOS)
# si R est lié à GNU libiconv, même sur un système glibc ; « win_iconv »
# sous Windows ; « glibc X.Y » (gnu_get_libc_version(), version de la glibc
# chargée à l'exécution) sous glibc sans libiconv ; « unknown » sinon. Le
# champ n'identifie donc la glibc, d'où vient la libm, que si sa valeur
# commence par « glibc » ; autrement il désigne l'implémentation d'iconv.
# « unknown » est la valeur brute de R, distincte de la mention « non
# relevée » (MENTION_BIBLIOTHEQUE_C_NON_RELEVEE), rendue quand la valeur est
# vide, absente ou non texte : rien n'est deviné.
# `versions` : extSoftVersion(), paramètre pour les tests.
MENTION_BIBLIOTHEQUE_C_NON_RELEVEE <- "non relevée"
bibliotheque_c <- function(versions = extSoftVersion()) {
  v <- if (is.character(versions) && "iconv" %in% names(versions)) versions[["iconv"]] else NA_character_
  texte_moteur(if (length(v) == 1L && !is.na(v) && nzchar(trimws(v))) v else MENTION_BIBLIOTHEQUE_C_NON_RELEVEE)
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
                      lapack = La_library(), bibliotheque_c = bibliotheque_c()),
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
    "le double relu fait foi ; 17 chiffres significatifs (forme %.17g de la norme C), arrondi au plus proche,",
    "égalité vers le chiffre pair, calculé par tablexp sans la bibliothèque C ; relecture au bit près constatée",
    "sous glibc, non garantie ailleurs ; à l'import, toute écriture décimale est admise (0.95 comme",
    "0.94999999999999996) ; refusés : décimal non fini, décimal non nul relu 0 ; jetons NA, NaN, Inf, -Inf"),
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
# les lignes d'en-tête `entete` (vecteur de chaînes nommé clé -> valeur) ;
# valeurs écrites en forme `forme` : « texte » (plan, fiche), « empreinte »
# (empreinte du résultat, empreinte_resultat()).
texte_objet <- function(x, premiere, entete, forme = "texte") {
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
    if (is.factor(v)) {
      # Facteur : refusé dans le texte (grammaire 1, niveaux non écrits ;
      # troisième annotation, (a)) ; admis dans le texte empreinté (aucune
      # relecture), type factor et libellés en chaînes, comme dans l'empreinte
      # des données (point 2.3) : le moteur recopie dans l'objet le type d'une
      # colonne de rôle catégorie reçue en facteur (#67, M71, Q2 : niveaux sans
      # effet, condition de validité testée).
      if (forme != "empreinte") impossible(chemin, "facteur, sans écriture de ses niveaux dans le texte de la grammaire 1")
      if (!(identical(class(v), "factor") || identical(class(v), c("ordered", "factor"))) ||
          !all(a %in% c("names", "levels", "class")))
        impossible(chemin, sprintf("facteur de classe %s ou d'attribut(s) %s", paste(class(v), collapse = "/"),
                                   paste(a, collapse = ", ")))
    } else if (identical(class(v), "Date")) {
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
    cc <- ecrire_valeurs(v, forme, chemin)
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
                rejeu = TEXTE_REJEU_FICHE))
}

# Note de lecture d'E3.7 (docs/exigences.md), en une ligne : ligne « # rejeu »
# de l'en-tête de la fiche écrite (ecrire_fiche()), reprise telle quelle par la
# section « Rejeu » du rapport figé (ADR 0012, seconde annotation du 24/10/2026,
# (b) 7 ; #87) ; texte du moteur marqué UTF-8 (#50). Extraite du littéral
# d'ecrire_fiche() sans changer un octet de la fiche.
TEXTE_REJEU_FICHE <- texte_moteur(paste(
  "vérifier version_fiche, empreintes/representation et empreintes/donnees ; exécuter",
  "construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance) sur le plan relu",
  "de ce texte ; comparer à tolérance par classes (ADR 0009) sur une autre machine, au bit",
  "près sur la même (E4.6)"))

# Fiche relue des octets `octets` (raw). Ne contrôle pas version_fiche :
# c'est l'affaire du rejeu (troisième annotation, (i) ; point 12 (1)). Rejeu
# (E3.7) : construire_table(donnees, f$roles, f$plan, f$provenance).
lire_fiche <- function(octets) {
  lire_texte(octets, PREMIERE_LIGNE_FICHE, "fiche")
}

# --- Rapport figé (ADR 0012, point 2 ; #87) ---
#
# ecrire_rapport(x) : octets UTF-8 d'un document HTML qui restitue, sans
# recalcul, l'objet `construction` x rendu par construire_table() (E3.5, E3.6 ;
# ADR 0012, point 2, et seconde annotation du 24/10/2026, (b) ; spécification
# d'actuary du 24/10/2026). Le package rend des octets, l'appelant écrit le
# fichier (writeBin ; ADR 0011, point 8.1) ; LF seul, LF final, sans BOM.
#
# Règles de la section, contrôlées par les tests (tests/unitaires/test_rapport.R) :
#  - aucun calcul : chaque valeur est lue dans l'objet, dans l'ordre de l'objet,
#    et écrite par l'écrivain unique ecrire_valeurs(·, "texte", ·) (doubles en
#    %.17g), puis échappée par echapper_html() ; nrow(), length() et names() ne
#    sont que des lectures de structure ;
#  - aucune lecture de `execution`, d'une horloge, de la session ni d'une
#    option : deux constructions identiques hors `execution` donnent les mêmes
#    octets ;
#  - codage (locale C comprise, contrôle tests/controles/locale_c.R) : tout
#    texte non ASCII assemblé est marqué UTF-8 avant paste() (texte_moteur()
#    pour les textes du moteur, Encoding<- pour la sortie de l'écrivain,
#    enc2utf8() pour les noms), échappement par gsub(fixed = TRUE) sans
#    useBytes (qui retirerait la marque), aucun accès à CONVENTIONS_TEXTE par
#    un nom non ASCII ;
#  - aucun appel à format, formatC, sprintf, as.character, cat ni à une
#    fonction d'horloge ou d'environnement dans cette section (contrôle
#    statique du test) ; paste() et paste0() n'y reçoivent que des
#    littéraux, des sorties d'echapper_html() ou de l'écrivain, et des textes
#    du moteur.

# Sections du rapport, dans l'ordre du document (ADR 0012, point 2.6) :
# identifiant (ancre), titre, nature (texte, objet, etape, renvoi,
# non_produite), étape du catalogue (nature etape), racines (chemins dans
# l'objet, ordre d'affichage) et renvois (chemins cités, nature renvoi).
# Chaque élément de names(x) hors execution, chaque etapes/<e> et chaque
# fiche/<f> est couvert par une racine ; les racines fiche/* des sections 2 à 6
# sont ORDRE_CHAMPS_FICHE. Le module ne lit pas ETAPES (graphe d'appels) : la
# concordance des sections `etape` avec ETAPES$etape est un test.
SECTIONS_RAPPORT <- data.frame(
  section = c("lecture", "identification", "alea", "donnees_recues", "plan", "fiche_hypotheses",
              "etape_donnees", "etape_exposition", "etape_brut", "suffisance", "avertissements",
              "validations", "table_finale", "rejeu"),
  titre = texte_moteur(c(
    "Lecture du rapport",
    "Identification : empreintes, versions et plateforme",
    "Graine et générateur",
    "Rôles, provenance, recette et unité d'observation",
    "Plan de construction (plan canonique), transition et fin effective de la période",
    "Paramètres retenus et hypothèses consignés dans la fiche",
    "Étape donnees : exclusions, unité d'observation et relevés",
    "Étape exposition : convention d'âge, exposition et événements par cellule",
    "Étape brut : taux bruts et intervalles",
    "Indicateurs de suffisance",
    "Avertissements",
    "Validations",
    "Table finale et comparaisons",
    "Rejeu")),
  nature = c("texte", "objet", "objet", "objet", "objet", "objet", "etape", "etape", "etape", "renvoi",
             "objet", "non_produite", "non_produite", "texte"),
  etape = c(NA, NA, NA, NA, NA, NA, "donnees", "exposition", "brut", NA, NA, NA, NA, NA),
  racines = I(list(
    character(0),
    c("fiche/version_fiche", "fiche/empreintes", "fiche/versions", "fiche/plateforme"),
    c("fiche/graine", "fiche/graine_mention", "fiche/generateur"),
    c("fiche/roles", "fiche/provenance", "fiche/provenance_mention", "fiche/recette", "fiche/unite_observation",
      "fiche/avertissement_unite"),
    c("fiche/plan", "fiche/transition_code", "fiche/fin_effective"),
    c("fiche/convention", "fiche/unite_exposition", "fiche/par_annee", "fiche/exclusions",
      "fiche/nombre_lignes_exclues", "fiche/hypothese_censure", "fiche/hypotheses_brut", "fiche/avertissements"),
    "etapes/donnees",
    "etapes/exposition",
    "etapes/brut",
    character(0),
    "avertissements",
    "etapes/validation",
    "table",
    character(0))),
  renvois = I(list(
    character(0), character(0), character(0), character(0), character(0), character(0), character(0),
    character(0), character(0), c("etapes/exposition/resultat/exposition", "etapes/brut/resultat"),
    character(0), character(0), character(0), character(0))),
  stringsAsFactors = FALSE)

# Tables dont le volume croît avec la base : chemin, type et dimension (lus
# dans la structure de l'objet), valeurs non reproduites (ADR 0012, point 2.7 ;
# seconde annotation du 24/10/2026, (b) 1 et 2).
CHEMINS_STRUCTURE_SEULE <- c("etapes/donnees/resultat/lignes_conservees", "etapes/donnees/resultat/numeros_conserves",
                             "etapes/donnees/resultat/lignes_exclues", "etapes/donnees/resultat/observations",
                             "etapes/donnees/resultat/observation_des_lignes",
                             "etapes/brut/resultat/kaplan_meier_sauts")

# Textes fixes du rapport (spécification d'actuary, § 2), marqués UTF-8 ; noms
# ASCII. Les morceaux qui encadrent une valeur se terminent ou commencent par
# l'espace qui les en sépare.
MENTIONS_RAPPORT <- texte_moteur(c(
  titre_document = "Rapport figé de construction (tablexp)",
  lecture_1 = paste(
    "Ce document est le rapport figé d'une construction de tablexp : il restitue, sans recalcul, l'objet rendu",
    "par construire_table() (E3.5, E3.6). Aucune valeur n'y est calculée : chaque valeur est lue dans l'objet et",
    "écrite telle quelle, dans l'ordre de l'objet ; chaque tableau cite le chemin de ses valeurs dans l'objet."),
  lecture_2 = paste(
    "Ce rapport ne porte pas de date ; la construction est identifiée par ses empreintes. Le rapport ne lit rien",
    "dans execution (date, durée, session) : deux constructions identiques hors execution donnent le même",
    "rapport, octet pour octet."),
  lecture_3 = "Conventions d'écriture, identiques à celles de l'en-tête de la fiche de construction :",
  lecture_4 = paste(
    "Aucun arrondi : 0,95 s'écrit 0.94999999999999996, écriture à 17 chiffres significatifs du double le plus",
    "proche de 0,95."),
  lecture_5 = paste(
    "Un data.frame est présenté en table de colonnes (une colonne par colonne de l'objet, avec son type ; valeurs",
    "dans l'ordre des lignes de l'objet) ; tout autre élément en table de lignes (chemin, type, longueur,",
    "valeurs), comme le corps de la fiche ; plusieurs valeurs d'une même ligne sont séparées par un retour à la",
    "ligne. Les tables dont le volume croît avec la base sont citées par leur chemin, leur type et leur",
    "dimension, sans leurs valeurs."),
  lecture_6 = paste(
    "Statuts des résultats (E2.20) : résultat exact, résultat asymptotique, approximation numérique, constat de",
    "simulation ; ils ne se confondent pas. Le statut de chaque grandeur des taux bruts est donné par le tableau",
    "etapes/brut/parametres/statuts."),
  lecture_7_debut = "Écrit par ecrire_rapport() ; empreinte du code de tablexp qui a écrit ce rapport : ",
  lecture_7_fin = ". Elle peut différer de fiche/empreintes/code, empreinte du code qui a construit l'objet.",
  empreinte_algorithme = paste(
    "algorithme : MD5 (fiche/empreintes/algorithme), sans rôle de sécurité : une empreinte identifie un objet,",
    "elle ne le protège pas contre une modification délibérée."),
  empreinte_donnees = paste(
    "données (fiche/empreintes/donnees) : représentation canonique (fiche/empreintes/representation) des",
    "colonnes des rôles telles que reçues par le moteur, toutes les lignes dans l'ordre reçu ; les colonnes hors",
    "rôles n'y entrent pas ; portable : exacte sous glibc, non mesurée ailleurs."),
  empreinte_fichier = paste(
    "fichier (fiche/empreintes/fichier, fiche/empreintes/algorithme_fichier) : calculée par la couche de lecture",
    "sur les octets du fichier lu et recopiée de la provenance ; non vérifiée par le moteur."),
  empreinte_resultat_code = paste(
    "résultat et code : statut consigné dans fiche/empreintes/statut_resultat_code (tableau ci-dessus)."),
  plateforme = paste(
    "L'empreinte du résultat dépend de la plateforme (R, BLAS, LAPACK, bibliothèque C) ; elle n'est un témoin",
    "d'identité que sur la même machine (E3.6, E4.6)."),
  integrite = paste(
    "Contrôle d'intégrité : avant l'écriture de ce rapport, l'empreinte du résultat a été recalculée sur",
    "l'objet et trouvée égale à fiche/empreintes/resultat."),
  # Suite de la même phrase (décision d'actuary, reprise de #87) : chemins de
  # setdiff(CHEMINS_HORS_RESULTAT, "execution") insérés entre les deux morceaux.
  integrite_hors_1 = " Hors du périmètre de cette empreinte, et donc de ce contrôle : ",
  integrite_hors_2 = " ; ce que le rapport en restitue (valeurs, ou structure seule pour une table dont le volume croît avec la base) est lu dans l'objet, sans vérification.",
  etape_absente_1 = "Étape « ",
  etape_absente_2 = " » : non demandée par le plan (chemin etapes/",
  etape_absente_3 = " absent de l'objet).",
  validations = "Validations : non produites par cette version (chemin etapes/validation absent de l'objet).",
  table_finale = "Table finale et comparaisons : non produites par cette version (chemin table : NULL).",
  largeur = "Largeur des intervalles : non produite par cette version.",
  racine_absente = "absent de l'objet",
  structure_seule = "non reproduites (volume croissant avec la base)",
  structure_colonnes = " ; colonnes : ",
  structure_exclues = " ; nombre de lignes par motif : etapes/donnees/resultat/exclusions",
  brut_note = paste(
    "La colonne taux porte une force de transition pour l'estimateur poisson (D / E^c) et une probabilité pour",
    "binomial (D / E^i) et kaplan_meier ; un taux brut se désigne avec son estimateur."),
  brut_legende = paste(
    "Les mots « exact » et « asymptotique » des noms de colonnes désignent la formule de l'intervalle, non une",
    "couverture garantie : le statut de chaque grandeur et de chaque intervalle est celui du tableau ci-dessus",
    "(colonnes statut, loi_supposee et calcul), qui fait foi."),
  brut_binomial = paste(
    "Intervalle binomial « exact » (colonnes borne_inf_exacte et borne_sup_exacte de",
    "etapes/brut/resultat/binomial) : la formule par quantiles de lois bêta suppose D ~ Bin(E^i, q) avec E^i",
    "entier ; sous troncature à gauche et censure, l'exposition initiale E^i n'est en général pas entière, et la",
    "formule en est alors le prolongement continu, de couverture non établie."),
  suffisance_intro = paste(
    "Indicateurs de suffisance (E2.15 : exposition et événements par âge et par segment, largeur des",
    "intervalles) : ils signalent ce que les données portent, sans rien interdire. Le rapport n'en calcule",
    "aucun ; ceux que l'objet porte sont aux chemins suivants :"),
  suffisance_exposition = paste(
    "Exposition et événements par cellule : etapes/exposition/resultat/exposition (section Étape exposition)."),
  suffisance_exposition_absente = "Exposition et événements par cellule : non demandée par le plan.",
  suffisance_brut = paste(
    "Événements, exposition et bornes d'intervalle par estimateur : etapes/brut/resultat (section Étape brut)."),
  suffisance_brut_absente = "Événements, exposition et bornes d'intervalle par estimateur : non demandée par le plan.",
  rejeu_1 = "Ligne de rejeu de la fiche de construction (en-tête de la fiche écrite par ecrire_fiche()) : ",
  rejeu_2 = paste(
    "Rejeu outillé : rejouer_fiche(donnees, fiche) reçoit le data.frame des données et la fiche relue de son",
    "texte par lire_fiche() ; elle contrôle la fiche et les données (version de fiche, algorithme et",
    "représentation des empreintes, contrôles d'entrée, empreinte des données), refuse à la première",
    "discordance, puis appelle construire_table() une seule fois ; elle rend la construction et un bilan de",
    "rejeu. Le rapport de la construction rejouée s'écrit par ecrire_rapport() appliquée à l'élément",
    "construction de ce résultat."),
  rejeu_3 = "Le bilan d'un rejeu n'entre pas dans l'objet : ce rapport ne le porte pas."))

# En-tête du document (forme fixe, spécification d'actuary, § 4) ; le titre
# est inséré par ecrire_rapport().
STYLE_RAPPORT <- c(
  "<style>",
  "body{font-family:sans-serif;margin:1em 2em;line-height:1.4}",
  "table{border-collapse:collapse;margin:0.5em 0 1em 0}",
  "th,td{border:1px solid #999;padding:0.15em 0.4em;vertical-align:top;text-align:left}",
  "td,code{font-family:monospace}",
  "caption{text-align:left;font-weight:bold;padding:0.2em 0}",
  "</style>")

# Échappement HTML de tout texte inséré (valeurs, chemins, noms, titres, textes
# fixes) : & d'abord, puis <, >, ". gsub(fixed = TRUE) sans useBytes : la
# marque UTF-8 est conservée (useBytes la retire, mesuré).
echapper_html <- function(s) {
  s <- gsub("&", "&amp;", s, fixed = TRUE)
  s <- gsub("<", "&lt;", s, fixed = TRUE)
  s <- gsub(">", "&gt;", s, fixed = TRUE)
  gsub("\"", "&quot;", s, fixed = TRUE)
}

# Valeurs de v écrites par l'écrivain unique en forme « texte » (chemin : rôle
# des refus) : type canonique et texte, une valeur par ligne séparée par LF,
# sans LF final ("" pour aucune valeur), marqué UTF-8.
texte_valeurs <- function(v, chemin) {
  w <- ecrire_valeurs(v, "texte", chemin)
  o <- w$octets
  s <- if (length(o)) rawToChar(o[-length(o)]) else ""
  Encoding(s) <- "UTF-8"
  list(type = w$type, texte = s)
}

# Entier n (nrow(), length()) écrit par l'écrivain, pour une cellule HTML.
texte_entier <- function(n, chemin) texte_valeurs(n, chemin)$texte

# Valeur v décrite pour un message de refus : NULL -> NULL ; atomique de
# longueur 1 de type double, integer, logical ou character sans attribut ->
# « <écriture> (<type>) », ex. « 3 (integer) » ; sinon, ou si l'écrivain
# refuse, « objet de type <typeof>, de classe <classes> ».
decrire_valeur <- function(v) {
  if (is.null(v)) return(NULL)
  if (is.atomic(v) && length(v) == 1L && is.null(attributes(v)) &&
      typeof(v) %in% c("double", "integer", "logical", "character")) {
    w <- tryCatch(texte_valeurs(v, "valeur"), refus_tablexp = function(e) NULL)
    if (!is.null(w)) return(paste0(w$texte, " (", w$type, ")"))
  }
  paste0(texte_moteur("objet de type "), typeof(v), texte_moteur(", de classe "), paste(class(v), collapse = "/"))
}

# Valeur v pour un message de refus : chaîne unique non NA sans attribut ni
# caractère de contrôle -> elle-même ; NULL -> « absente » ; sinon
# decrire_valeur(v) (message sur une seule ligne ; décision d'actuary, reprise
# de #87). Contrôle : U+0000 à U+001F, U+007F à U+009F (NEL compris), U+2028,
# U+2029, lus par utf8ToInt() sur les octets UTF-8 (même résultat sous les
# locales C et UTF-8 ; NA si la chaîne n'est pas de l'UTF-8 valide). Seule une
# chaîne latin-1 est convertie : enc2utf8() d'une chaîne non marquée sous la
# locale C en échappe les octets non ASCII (« <e2><80><a8> »), mesuré.
valeur_message <- function(v) {
  if (is.null(v)) return(texte_moteur("absente"))
  if (chaine_unique(v)) {
    p <- utf8ToInt(if (identical(Encoding(v), "latin1")) enc2utf8(v) else v)
    if (!anyNA(p) && !any(p <= 0x1F | (p >= 0x7F & p <= 0x9F) | p == 0x2028 | p == 0x2029)) return(v)
  }
  decrire_valeur(v)
}

# Élément de chemin `chemin` (segments joints par /) de l'objet x : present
# FALSE si un segment manque (ou si son parent n'est pas une liste).
lire_racine <- function(x, chemin) {
  v <- x
  for (s in strsplit(chemin, "/", fixed = TRUE)[[1L]]) {
    if (!is.list(v) || !(s %in% names(v))) return(list(present = FALSE, valeur = NULL))
    v <- v[[s]]
  }
  list(present = TRUE, valeur = v)
}

# Contrôles avant le rendu (ADR 0012, point 2.5, et seconde annotation du
# 24/10/2026, (b) 3 et 4), dans l'ordre ; au premier refus, rien n'est rendu.
controler_rapport <- function(x) {
  refus <- function(motif, texte, details) refuser("rapport", motif, texte, details)
  # 1. Classe exacte, et fiche qui est une liste ; un objet qui n'est pas une
  #    liste est décrit aussi par son type (classe « construction » posée sur
  #    un vecteur, par exemple).
  origine <- texte_moteur(" (objet rendu par construire_table(), ou élément construction du résultat de rejouer_fiche()).")
  if (!is.list(x))
    refus("rapport_objet_invalide",
          paste0(texte_moteur("Rapport figé : objet de type "), typeof(x),
                 texte_moteur(" au lieu d'une liste de classe « construction »"), origine),
          list(lu = class(x), attendu = "construction"))
  if (!identical(class(x), "construction"))
    refus("rapport_objet_invalide",
          paste0(texte_moteur("Rapport figé : objet de classe « "), paste(class(x), collapse = "/"),
                 texte_moteur(" » au lieu de « construction »"), origine),
          list(lu = class(x), attendu = "construction"))
  f <- x[["fiche"]]
  if (!is.list(f)) {
    d <- decrire_valeur(f)
    refus("rapport_objet_invalide",
          paste0(texte_moteur("Rapport figé : objet de classe « construction » dont la fiche est "),
                 if (is.null(d)) texte_moteur("absente") else d, texte_moteur(" au lieu d'une liste.")),
          list(chemin = "fiche", lu = f, attendu = "list"))
  }
  # 2. Version de la fiche : la version courante seule (même motif que le rejeu).
  v <- f[["version_fiche"]]
  if (!identical(v, VERSION_FICHE)) {
    ve <- f[["versions"]]
    em <- f[["empreintes"]]
    d <- decrire_valeur(v)
    refus("version_fiche_non_admise",
          paste0(texte_moteur("Rapport figé : version de fiche "), if (is.null(d)) texte_moteur("absente") else d,
                 texte_moteur(" au lieu de "), decrire_valeur(VERSION_FICHE),
                 texte_moteur(" (seule version admise par ce code) ; objet construit par tablexp "),
                 valeur_message(if (is.list(ve)) ve[["tablexp"]]), texte_moteur(", empreinte du code "),
                 valeur_message(if (is.list(em)) em[["code"]]), texte_moteur(" ; code installé : empreinte "),
                 valeur_message(EMPREINTE_CODE),
                 texte_moteur(paste(". Aucune conversion n'est faite : le rapport d'un objet d'une autre version",
                                    "s'écrit avec le code qui l'a construit."))),
          list(lu = v, attendu = VERSION_FICHE))
  }
  # 3. Intégrité : empreinte du résultat recalculée égale à celle consignée ;
  #    une erreur du recalcul (attribut inconnu, structure altérée) est un objet
  #    modifié, non une erreur de programmation.
  em <- f[["empreintes"]]
  b <- if (is.list(em)) em[["resultat"]]
  a <- tryCatch(empreinte_resultat(x), error = function(e) e)
  if (inherits(a, "error"))
    refus("objet_modifie",
          paste0(texte_moteur("Rapport figé : objet modifié après sa construction : empreinte du résultat non recalculable ("),
                 conditionMessage(a), texte_moteur(") ; fiche/empreintes/resultat vaut "), valeur_message(b), "."),
          list(lu = b, attendu = NA_character_))
  if (!identical(a, b))
    refus("objet_modifie",
          paste0(texte_moteur("Rapport figé : objet modifié après sa construction : empreinte du résultat recalculée "),
                 a, texte_moteur(" au lieu de "), valeur_message(b),
                 texte_moteur(paste(" (fiche/empreintes/resultat). Le rapport ne restitue un objet que tel que",
                                    "construire_table() l'a rendu."))),
          list(lu = b, attendu = a))
  # 4. Aucun élément hors des sections : rien n'est omis en silence.
  non_prevu <- function(chemin)
    refus("rapport_objet_invalide",
          paste0(texte_moteur("Rapport figé : élément « "), chemin,
                 texte_moteur(paste(" » de l'objet non prévu par les sections de ce rapport ; un objet de cette",
                                    "version de fiche n'en porte pas, et le rapport n'omet aucun élément en silence."))),
          list(chemin = chemin))
  hors <- setdiff(names(x), c("table", "etapes", "avertissements", "fiche", "execution"))
  if (length(hors)) non_prevu(hors[1L])
  racines <- unlist(SECTIONS_RAPPORT$racines, use.names = FALSE)
  couvert <- function(chemin) any(racines == chemin | startsWith(chemin, paste0(racines, "/")))
  for (e in names(x[["etapes"]])) if (!couvert(joindre_chemin("etapes", e))) non_prevu(joindre_chemin("etapes", e))
  for (n in names(f)) if (!couvert(joindre_chemin("fiche", n))) non_prevu(joindre_chemin("fiche", n))
  for (r in unlist(SECTIONS_RAPPORT$racines[SECTIONS_RAPPORT$nature == "non_produite"], use.names = FALSE)) {
    l <- lire_racine(x, r)
    if (l$present && !is.null(l$valeur)) non_prevu(r)
  }
  invisible(TRUE)
}

# Table de colonnes d'un data.frame v de chemin `chemin` : légende (chemin,
# lignes, colonnes), en-tête (segment du nom de colonne, type), une ligne <tr>
# par ligne de l'objet, dans son ordre ; valeurs par l'écrivain.
table_colonnes <- function(v, chemin) {
  s <- segments_texte(enc2utf8(names(v)), length(v))
  n <- nrow(v)
  entetes <- character(length(v))
  colonnes <- vector("list", length(v))
  for (i in seq_along(v)) {
    w <- texte_valeurs(v[[i]], joindre_chemin(chemin, s[i]))
    entetes[i] <- paste0("<th>", echapper_html(s[i]), "<br>", echapper_html(w$type), "</th>")
    colonnes[[i]] <- if (n) strsplit(echapper_html(w$texte), "\n", fixed = TRUE)[[1L]] else character(0)
    if (length(colonnes[[i]]) != n)
      stop(paste0("table_colonnes() : colonne ", joindre_chemin(chemin, s[i]),
                  " sans une valeur par ligne (erreur de programmation)."),
           call. = FALSE)
  }
  corps <- if (length(v) && n) paste0("<tr><td>", do.call(paste, c(unname(colonnes), sep = "</td><td>")), "</td></tr>")
  c("<table>",
    paste0("<caption><code>", echapper_html(chemin), "</code> : data.frame ; lignes : ", texte_entier(n, chemin),
           " ; colonnes : ", texte_entier(length(v), chemin), "</caption>"),
    "<thead>", paste0("<tr>", paste(entetes, collapse = ""), "</tr>"), "</thead>",
    "<tbody>", corps, "</tbody>", "</table>")
}

# Lignes HTML des racines `racines` de l'objet x, parcourues dans l'ordre de
# l'objet (segments de chemin de la fiche, segments_texte() et
# joindre_chemin()) : table de lignes chemin | type | longueur | valeurs (les
# lignes du texte de la grammaire 1, texte_objet()), interrompue par une table
# de colonnes à chaque data.frame ; structure seule pour CHEMINS_STRUCTURE_SEULE ;
# racine absente : « absent de l'objet ». `apres` : lignes HTML insérées après
# la table de colonnes du chemin qui les nomme.
lignes_racines <- function(x, racines, apres = list()) {
  sortie <- character(0)
  en_cours <- character(0)
  fermer <- function() {
    if (length(en_cours))
      sortie <<- c(sortie, "<table>", "<thead>", "<tr><th>chemin</th><th>type</th><th>longueur</th><th>valeurs</th></tr>",
                   "</thead>", "<tbody>", en_cours, "</tbody>", "</table>")
    en_cours <<- character(0)
  }
  # Une ligne de la table de lignes ; `valeurs` est déjà échappé.
  ligne <- function(chemin, type, longueur, valeurs)
    en_cours <<- c(en_cours, paste0("<tr><td>", echapper_html(chemin), "</td><td>", echapper_html(type), "</td><td>",
                                    longueur, "</td><td>", valeurs, "</td></tr>"))
  vecteur <- function(v, chemin) {
    w <- texte_valeurs(v, chemin)
    ligne(chemin, w$type, texte_entier(length(v), chemin), gsub("\n", "<br>", echapper_html(w$texte), fixed = TRUE))
  }
  noms <- function(v, chemin) {
    if (is.null(names(v))) ligne(joindre_chemin(chemin, "@longueur"), "integer", texte_entier(1L, chemin),
                                 texte_entier(length(v), chemin))
    else vecteur(names(v), joindre_chemin(chemin, "@noms"))
  }
  structure_seule <- function(v, chemin) {
    mention <- echapper_html(MENTIONS_RAPPORT[["structure_seule"]])
    if (is.data.frame(v)) {
      colonnes <- gsub("\n", " ", echapper_html(texte_valeurs(names(v), joindre_chemin(chemin, "@colonnes"))$texte),
                       fixed = TRUE)
      ligne(chemin, "data.frame", texte_entier(nrow(v), chemin),
            paste0(mention, echapper_html(MENTIONS_RAPPORT[["structure_colonnes"]]), colonnes))
    } else {
      ligne(chemin, typeof(v), texte_entier(length(v), chemin),
            paste0(mention, if (identical(chemin, "etapes/donnees/resultat/lignes_exclues"))
                              echapper_html(MENTIONS_RAPPORT[["structure_exclues"]])))
    }
  }
  noeud <- function(v, chemin) {
    if (chemin %in% CHEMINS_STRUCTURE_SEULE) return(structure_seule(v, chemin))
    if (is.null(v)) return(ligne(chemin, "NULL", texte_entier(0L, chemin), ""))
    if (is.data.frame(v)) {
      fermer()
      sortie <<- c(sortie, table_colonnes(v, chemin), apres[[chemin]])
      return(invisible())
    }
    if (is.list(v)) {
      noms(v, chemin)
      s <- segments_texte(if (is.null(names(v))) NULL else enc2utf8(names(v)), length(v))
      for (i in seq_along(v)) noeud(v[[i]], joindre_chemin(chemin, s[i]))
      return(invisible())
    }
    if (!is.null(names(v))) noms(v, chemin)
    vecteur(v, chemin)
  }
  for (r in racines) {
    l <- lire_racine(x, r)
    if (l$present) noeud(l$valeur, r)
    else ligne(r, "", "", echapper_html(MENTIONS_RAPPORT[["racine_absente"]]))
  }
  fermer()
  sortie
}

# Un paragraphe, un élément de liste : texte échappé.
paragraphe <- function(...) paste0("<p>", echapper_html(paste0(...)), "</p>")
element_liste <- function(...) paste0("<li>", echapper_html(paste0(...)), "</li>")

# Corps HTML de la section k de SECTIONS_RAPPORT pour l'objet x (contrôlé).
corps_section <- function(x, k) {
  m <- MENTIONS_RAPPORT
  id <- SECTIONS_RAPPORT$section[k]
  nature <- SECTIONS_RAPPORT$nature[k]
  racines <- SECTIONS_RAPPORT$racines[[k]]
  etapes <- names(x[["etapes"]])
  if (identical(id, "lecture")) {
    conventions <- vapply(seq_along(CONVENTIONS_TEXTE), function(i)
      element_liste(names(CONVENTIONS_TEXTE)[i], " : ", CONVENTIONS_TEXTE[i]), "")
    return(c(paragraphe(m[["lecture_1"]]), paragraphe(m[["lecture_2"]]), paragraphe(m[["lecture_3"]]),
             "<ul>", conventions, "</ul>",
             paragraphe(m[["lecture_4"]]), paragraphe(m[["lecture_5"]]), paragraphe(m[["lecture_6"]]),
             paragraphe(m[["lecture_7_debut"]], texte_valeurs(EMPREINTE_CODE, "EMPREINTE_CODE")$texte,
                        m[["lecture_7_fin"]])))
  }
  if (identical(id, "identification"))
    return(c(lignes_racines(x, racines),
             "<ul>", element_liste(m[["empreinte_algorithme"]]), element_liste(m[["empreinte_donnees"]]),
             element_liste(m[["empreinte_fichier"]]), element_liste(m[["empreinte_resultat_code"]]), "</ul>",
             paragraphe(m[["plateforme"]]),
             paragraphe(m[["integrite"]], m[["integrite_hors_1"]],
                        paste(setdiff(CHEMINS_HORS_RESULTAT, "execution"), collapse = ", "), m[["integrite_hors_2"]])))
  if (identical(nature, "objet")) return(lignes_racines(x, racines))
  if (identical(nature, "etape")) {
    e <- SECTIONS_RAPPORT$etape[k]
    if (!(e %in% etapes))
      return(paragraphe(m[["etape_absente_1"]], e, m[["etape_absente_2"]], e, m[["etape_absente_3"]]))
    if (!identical(e, "brut")) return(lignes_racines(x, racines))
    # Étape brut : note sur la colonne taux en tête ; légendes immédiatement
    # après le tableau des statuts (spécification d'actuary, § 2.4).
    legendes <- c(paragraphe(m[["brut_legende"]]),
                  if ("binomial" %in% x[["etapes"]][["brut"]][["parametres"]][["estimateurs"]])
                    paragraphe(m[["brut_binomial"]]))
    return(c(paragraphe(m[["brut_note"]]),
             lignes_racines(x, racines, list("etapes/brut/parametres/statuts" = legendes))))
  }
  if (identical(id, "suffisance"))
    return(c(paragraphe(m[["suffisance_intro"]]), "<ul>",
             element_liste(m[[if ("exposition" %in% etapes) "suffisance_exposition" else "suffisance_exposition_absente"]]),
             element_liste(m[[if ("brut" %in% etapes) "suffisance_brut" else "suffisance_brut_absente"]]),
             element_liste(m[["largeur"]]), "</ul>"))
  if (identical(id, "validations")) return(paragraphe(m[["validations"]]))
  if (identical(id, "table_finale")) return(paragraphe(m[["table_finale"]]))
  if (identical(id, "rejeu"))
    return(c(paragraphe(m[["rejeu_1"]], TEXTE_REJEU_FICHE), paragraphe(m[["rejeu_2"]]), paragraphe(m[["rejeu_3"]])))
  stop(paste0("corps_section() : section ", id, " sans rendu (erreur de programmation)."), call. = FALSE)
}

# Rapport figé de l'objet `construction` x (ADR 0012, point 2) : contrôles
# (controler_rapport()), puis octets UTF-8 du document HTML, LF seul, LF final,
# sans BOM. Aucune option : le rapport a une forme. L'appelant écrit le fichier
# (writeBin(ecrire_rapport(x), "rapport.html")).
ecrire_rapport <- function(x) {
  controler_rapport(x)
  titre <- echapper_html(MENTIONS_RAPPORT[["titre_document"]])
  ids <- SECTIONS_RAPPORT$section
  titres <- echapper_html(SECTIONS_RAPPORT$titre)
  sections <- lapply(seq_along(ids), function(k)
    c(paste0("<section id=\"", ids[k], "\">"), paste0("<h2>", titres[k], "</h2>"), corps_section(x, k), "</section>"))
  lignes <- c("<!DOCTYPE html>", "<html lang=\"fr\">", "<head>", "<meta charset=\"utf-8\">",
              paste0("<title>", titre, "</title>"), STYLE_RAPPORT, "</head>", "<body>",
              paste0("<h1>", titre, "</h1>"),
              "<nav><ol>", paste0("<li><a href=\"#", ids, "\">", titres, "</a></li>"), "</ol></nav>",
              unlist(sections, use.names = FALSE),
              "</body>", "</html>")
  charToRaw(enc2utf8(paste0(lignes, "\n", collapse = "")))
}
