# -----------------------------------------------------------------------------
#  tests/controles/graines.R : unicité des graines littérales (issue #176 ;
#  B15, avis de forme G13 ; spécification d'actuary : réemplois admis A1 à A5,
#  refus X1 à X3), en R de base
#
#  Pourquoi : deux sources d'aléa distinctes sous une même valeur donnent des
#  flux identiques ; les données et la statistique de test deviennent
#  dépendantes, ce qui invalide la probabilité de fausse alarme d'un constat
#  (E2.19). Une recherche d'une graine doit désigner un usage unique, sauf
#  réemploi admis et consigné.
#
#  Méthode :
#    1. chaque *.R de tests/, R/ et inst/ est lu par parse(keep.source =
#       TRUE), en UTF-8 quelle que soit la locale (analyser_fichier(), #213),
#       et utils::getParseData() ; sont relevés les littéraux entiers
#       (7, 7L) qui sont, directement :
#         - un argument de set.seed() ;
#         - le premier argument de sous_graine() (positionnel, ou nommé
#           graine) ;
#         - la valeur d'un argument, d'un paramètre par défaut ou d'un
#           élément nommé `graine` (graine = 5L), ou la valeur affectée à
#           x$graine ou x[["graine"]] ;
#         - un argument positionnel (ou apparié partiellement) qui tombe,
#           selon la correspondance des arguments de R (match.call()), sur un
#           paramètre formel nommé `graine` : simuler_portefeuille(loi, pf,
#           per, 20261016L). Les formels sont lus dans les définitions de
#           fonctions (nom <- function(...)) de tous les fichiers relevés, R/
#           comme tests ; un même nom défini plusieurs fois compte pour chaque
#           signature ;
#         - le membre droit d'une affectation à une constante GRAINE,
#           GRAINE_* ou GRAINES_* (vecteur c() compris, éléments nommés ou
#           non).
#       Chaque occurrence donne (valeur, fichier, ligne, contexte) ; le
#       contexte est « set.seed », « sous_graine », « graine= (appelée) »
#       (forme nommée ou positionnelle ; « formals » pour un paramètre par
#       défaut), « $graine », le nom de la constante, etc. ;
#    2. règle A5 : une valeur < 1000 est une graine de garde, exemptée
#       d'unicité, sauf sous une constante GRAINE* (une graine à usage
#       statistique ne prend jamais une valeur de garde : écart) ;
#    3. une valeur >= 1000 qui apparaît dans plusieurs fichiers, ou sous deux
#       contextes d'un même fichier, doit figurer dans la liste blanche
#       (graines_admises.csv, classes A1 à A4) ou dans HERITAGE_176, avec
#       pour fichiers exactement l'ensemble observé : tout fichier nouveau
#       (croissance, X3) ou disparu (la mention est à retirer) est un écart ;
#       une entrée (liste blanche ou héritage) dont la valeur n'a aucune
#       occurrence est un écart, sauf entrée d'héritage marquée « pour
#       mémoire » (colonne pour_memoire de HERITAGE_176) ; une entrée dont la
#       valeur n'apparaît plus que dans un contexte donne une note ;
#    4. une plage (classe `plage` de la liste blanche : valeurs valeur à
#       valeur + etendue - 1, déclarées par le ou les fichiers de la ligne) :
#       une valeur relevée dans une plage, hors des fichiers qui la
#       déclarent, est un écart, sauf entrée d'HERITAGE_176 nommant ce
#       fichier (une entrée de liste blanche, A1 à A4, ne vaut pas exemption) ;
#       deux plages qui se recouvrent sont un écart ; les fichiers
#       déclarés existent.
#
#  Liste blanche : tests/controles/graines_admises.csv, colonnes valeur,
#  etendue, classe, fichiers (séparés par |, chemins relatifs à la racine),
#  motif, decision. Héritage figé : HERITAGE_176 ci-dessous, réemplois
#  antérieurs à M300 (07/11/2026) ; aucun membre nouveau, toute modification
#  est visible au diff et soumise au mainteneur (décision du 08/11/2026,
#  point P1, Q7). Retirer d'HERITAGE_176 un fichier ou une entrée devenus sans
#  occurrence est autorisé d'avance (décision du 08/11/2026, M338) ; la
#  mention est notée dans la PR.
#
#  Limites : (i) seuls les littéraux directs sont lus. Ne sont pas vus :
#    - l'arithmétique (graine = GRAINE + 1L, graine <- 20261610L + co$id,
#      1000L + i) ;
#    - les variables (g <- 20261621L ; simuler(..., G_C2)), les constantes
#      qui ne portent pas un nom GRAINE* ;
#    - assign(), les valeurs négatives ou décimales, withr::with_seed() ;
#    - le réemploi par le nom d'une constante, y compris X2 : une graine de
#      référence reprise par son nom (GRAINE_REFERENCE dans un autre fichier)
#      n'est pas vue, seul l'est son littéral ;
#    - un appel dont la fonction n'est définie dans aucun fichier relevé
#      (fonction d'un package, appel par do.call()) ;
#    - un appel avec un argument omis (f(a, , b, 2026xxxxL)) ou une virgule
#      finale (f(1, 2, 3, 2026xxxxL,)) : il n'est pas apparié, bien que le
#      littéral tombe sur le formel graine pour R ;
#  (ii) deux occurrences d'un même contexte dans un même fichier
#  ne sont pas distinguées (même usage supposé) ; (iii) X4 (deux constats
#  présentés comme indépendants qui partagent la graine de leur jeu) n'est
#  pas détectable mécaniquement : règle de rédaction des constats ;
#  (iv) l'égalité de valeur est testée, pas l'indépendance des flux.
#
#  Usage : Rscript tests/controles/graines.R [racine du dépôt [liste.csv]]
#  [--sans-heritage]. Sans argument : la racine du dépôt et sa liste blanche.
#  Code de sortie 1 en cas d'écart. --sans-heritage (arbres témoins des
#  tests) écarte HERITAGE_176, propre au dépôt. Sourcé (tests), le fichier ne
#  fait que définir les fonctions.
# -----------------------------------------------------------------------------

DOSSIERS_RELEVES <- c("tests", "R", "inst")
SEUIL_GARDE <- 1000
COLONNES_ADMISES <- c("valeur", "etendue", "classe", "fichiers", "motif", "decision")
CLASSES_ADMISES <- c("A1", "A2", "A3", "A4", "plage")
CONSTANTE_GRAINE <- "^GRAINES?(_|$)"
OPERATEURS_AFFECTATION <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")

# Réemplois antérieurs à M300 (commit 9c5e4da, 07/11/2026), figés par décision
# du mainteneur du 08/11/2026 (point P1, Q7) : aucune valeur nouvelle, aucun
# fichier nouveau. Les ensembles de fichiers sont exacts ; un fichier ou une
# entrée devenus sans occurrence se retirent ici, au diff : retrait autorisé
# d'avance et noté dans la PR (décision du 08/11/2026, M338) ; seul un ajout est
# soumis au mainteneur.
HERITAGE_176 <- data.frame(
  valeur = c(20261016, 20261019, 20261025, 20261026, 20261106,
             20261611, 20261612, 20261613, 20261614, 20261615, 20261616, 20261621, 20261631, 20261641),
  fichiers = c(
    paste("tests/mesures/temps_construire_table.R", "tests/reference/outils_reference.R", "tests/test_reproductibilite.R",
          "tests/unitaires/test_brut_kaplan_meier.R", "tests/unitaires/test_construire_table.R",
          "tests/unitaires/test_importance_exclusions.R", "tests/unitaires/test_rapport.R",
          "tests/unitaires/test_simulation.R", sep = "|"),
    paste("tests/controles/ecriture_doubles.R", "tests/controles/ecriture_doubles_texte.R", "tests/test_reproductibilite.R",
          "tests/unitaires/test_construire_table.R", sep = "|"),
    "tests/mesures/calculs_repetes.R|tests/mesures/importance_exclusions.R|tests/unitaires/test_importance_exclusions.R|tests/unitaires/test_simulation.R",
    "tests/controles/ecriture_doubles_texte.R|tests/mesures/ecriture_17g/echantillon.R",
    paste("tests/unitaires/test_graine_plan.R", "tests/unitaires/test_homogeneite.R", "tests/unitaires/test_importance_exclusions.R",
          "tests/unitaires/test_positionnement.R", "tests/unitaires/test_tables_reference.R", "tests/unitaires/test_validation.R",
          "tests/unitaires/test_validation_positionnee.R", sep = "|"),
    rep("tests/unitaires/test_constats_brut.R", 9L)),
  motif = c(
    "GRAINE_REFERENCE (jeu propre de R1 à R12, de l'entrée de R9 et de celles de R13 et R14 : A1) ; même valeur reprise comme jeu de tests distincts (test_simulation.R, test_brut_kaplan_meier.R, test_construire_table.R, test_importance_exclusions.R, test_rapport.R, mesure temps_construire_table.R) ou comme tirage d'essai de sous_graine (test_reproductibilite.R) : X2 antérieurs à M300 ; corriger obligerait à recalculer des attentes écrites en dur (D = 878)",
    "échantillon de doubles de la forme empreinte : même recette (corps identiques, vérifié) dans ecriture_doubles.R, ecriture_doubles_texte.R et test_construire_table.R (A2 : même objet à n = 1e5) ; réemployée aussi comme GRAINE_PERMUTATION (ordre de permutation des lignes de test_reproductibilite.R), autre objet, antérieur à M300",
    "jeu altéré de test_importance_exclusions.R et test_simulation.R (GRAINE_A, GRAINE_DEF), défauts des mesures importance_exclusions.R et calculs_repetes.R : mêmes valeurs pour des objets distincts, antérieures à M300",
    "deux contrôles d'écriture exacte des doubles sans lien entre eux, antérieurs à M300 : ecriture_doubles_texte.R:57 (GRAINE_EGALITES, égalités tirées) et tests/mesures/ecriture_17g/echantillon.R:7 (oracle de #104) ; aucune probabilité de fausse alarme n'en dépend (décision du mainteneur du 08/11/2026, M336)",
    "jeu simulé, graine du plan (Monte-Carlo) et échantillon d'essai sous la même valeur dans des tests déterministes (X1) ; assertions structurelles ou de recoupement, hormis le constat J5 de test_validation.R (taille des tests, données sous set.seed(20261106), Monte-Carlo sous 1001 à 1400, sans partage de flux) ; le jeu de test_validation_positionnee.R:279 est le même objet que le jeu de test_positionnement.R:255-261 (même appel : loi, portefeuille, période, graine ; vérifié par actuary) ; antérieures à M300",
    rep("graines de test_constats_brut.R comprises dans la plage des jeux de la loi A du constat de positionnement (r = 411 à 416, 421, 431, 441) : coïncidence connue, sans effet statistique, gardée pour que la sortie consignée reste rejouable (décision du mainteneur du 08/11/2026, commentaire de constat_positionnement.R) ; pour mémoire : graine en arithmétique (20261610L + co$id) ou en variable (G_C2), hors du relevé (limite (i))", 9L)),
  pour_memoire = c(rep(FALSE, 5L), rep(TRUE, 9L)),
  stringsAsFactors = FALSE
)

# --- relevé --------------------------------------------------------------------

relever_aucun <- function()
  data.frame(valeur = numeric(0), fichier = character(0), ligne = integer(0),
             contexte = character(0), constante = logical(0), stringsAsFactors = FALSE)

# Lecture d'un fichier de tests/, R/ ou inst/ en UTF-8 quelle que soit la locale (#206, #213) :
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

# Arbre d'un fichier : parse(keep.source = TRUE) et getParseData(), avec les
# accès par identifiant (ix : id -> ligne de pd ; par : id -> parent ; fils :
# lignes de pd des enfants d'un id, dans l'ordre du source).
lire_arbre <- function(chemin) {
  # Lecture en UTF-8 quelle que soit la locale (analyser_fichier(), #213) ;
  # seuls les nombres, symboles et lignes sont lus.
  ex <- analyser_fichier(chemin, keep.source = TRUE)
  pd <- utils::getParseData(ex)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  pd <- pd[order(pd$line1, pd$col1), ]
  taille <- max(pd$id, pd$parent) + 1L
  ix <- integer(taille); ix[pd$id] <- seq_len(nrow(pd))
  par <- integer(taille); par[pd$id] <- pd$parent
  parents <- unique(pd$parent[pd$parent >= 0L])  # parent < 0 : commentaire, hors arbre
  pos_parent <- integer(taille); pos_parent[parents + 1L] <- seq_along(parents)
  enfants <- split(seq_len(nrow(pd)), factor(pd$parent, levels = parents))
  list(pd = pd, ix = ix, par = par, jeton = pd$token, texte = pd$text,
       fils = function(id) {
         p <- pos_parent[id + 1L]
         if (!p) integer(0) else enfants[[p]]
       })
}

# Définitions de fonctions d'un arbre : liste nom -> liste de signatures
# (vecteurs des noms des formels, dans l'ordre). Une définition est
# `nom <- function(...)`, `nom = function(...)`, `x$nom <- function(...)`.
formels_de <- function(a) {
  sorties <- list()
  for (r in which(a$jeton == "FUNCTION")) {
    noeud <- a$par[a$pd$id[r]]
    if (!noeud) next
    pere <- a$par[noeud]
    if (!pere) next
    k <- a$fils(pere)
    if (length(k) != 3L || !(a$jeton[k[2L]] %in% OPERATEURS_AFFECTATION)) next
    droite <- if (a$jeton[k[2L]] == "RIGHT_ASSIGN") k[1L] else k[3L]
    gauche <- if (a$jeton[k[2L]] == "RIGHT_ASSIGN") k[3L] else k[1L]
    if (droite != a$ix[noeud]) next
    g <- a$fils(a$pd$id[gauche])
    nom <- if (length(g) == 1L && a$jeton[g] == "SYMBOL") a$texte[g] else
      if (length(g) == 3L && a$jeton[g[2L]] %in% c("'$'", "'@'") && a$jeton[g[3L]] == "SYMBOL") a$texte[g[3L]] else NULL
    if (is.null(nom)) next
    kf <- a$fils(noeud)
    sig <- a$texte[kf[a$jeton[kf] == "SYMBOL_FORMALS"]]
    sorties[[nom]] <- c(sorties[[nom]], list(sig))
  }
  sorties
}

# Le littéral de ligne `r_lit` (argument numéro k de l'appel) tombe-t-il, selon
# match.call(), sur le formel `graine` de l'une des signatures `sigs` ?
# `noms` : nom de chaque argument de l'appel ("" si positionnel).
tombe_sur_graine <- function(sigs, noms, k) {
  for (sig in sigs) {
    if (!("graine" %in% sig) || anyDuplicated(sig)) next
    f <- function() NULL
    formals(f) <- stats::setNames(rep(list(quote(expr = )), length(sig)), sig)
    appel <- as.call(c(list(as.name("f")),
                       stats::setNames(lapply(seq_along(noms), function(i) as.name(paste0(".p", i))), noms)))
    mc <- tryCatch(match.call(f, appel), error = function(e) NULL)
    if (is.null(mc)) next
    m <- as.list(mc)[-1L]
    i <- which(vapply(m, function(x) is.symbol(x) && identical(as.character(x), paste0(".p", k)), NA))
    if (length(i) == 1L && identical(names(m)[i], "graine")) return(TRUE)
  }
  FALSE
}

# Relevé d'un arbre : une ligne par littéral entier en contexte de graine.
# `formels` : définitions de fonctions de tous les fichiers (formels_de()).
releve_arbre <- function(a, nom, formels) {
  ix <- a$ix; par <- a$par; jeton <- a$jeton; texte <- a$texte; fils <- a$fils; pd <- a$pd
  est_expr <- function(r) startsWith(jeton[r], "expr")
  # Nom de la fonction appelée par un appel dont la tête est l'expression de
  # ligne r : dernier SYMBOL_FUNCTION_CALL de la tête (f, pkg::f, x$f).
  nom_appelee <- function(r) {
    trouve <- ""
    visiter <- function(rr) {
      if (jeton[rr] == "SYMBOL_FUNCTION_CALL") trouve <<- texte[rr]
      for (k in fils(pd$id[rr])) visiter(k)
    }
    visiter(r)
    trouve
  }
  nom_litteral <- function(r) gsub("^[\"'`]|[\"'`]$", "", texte[r])
  # Arguments d'un appel (k2 : enfants de l'appel) : noms, lignes de pd des
  # expressions ; vide = TRUE si un argument est omis (f(a, , b)), cas où
  # l'appariement n'est pas tenté.
  arguments_appel <- function(k2) {
    noms <- character(0); lignes <- integer(0); vide <- FALSE
    nom_en_attente <- ""; apres_virgule <- TRUE; attente_valeur <- FALSE
    for (j in seq_along(k2)[-c(1L, 2L, length(k2))]) {
      t <- jeton[k2[j]]
      if (t %in% c("SYMBOL_SUB", "STR_CONST")) { nom_en_attente <- nom_litteral(k2[j]); next }
      if (t == "EQ_SUB") { attente_valeur <- TRUE; next }
      if (t == "','") {
        if (apres_virgule || attente_valeur) vide <- TRUE
        apres_virgule <- TRUE; attente_valeur <- FALSE; nom_en_attente <- ""; next
      }
      noms <- c(noms, nom_en_attente); lignes <- c(lignes, k2[j])
      nom_en_attente <- ""; apres_virgule <- FALSE; attente_valeur <- FALSE
    }
    if (apres_virgule && length(k2) > 3L || attente_valeur) vide <- TRUE
    list(noms = noms, lignes = lignes, vide = vide)
  }
  # Contexte de l'affectation dont `r_noeud` (ligne de pd d'une expression)
  # est le membre droit : liste(contexte, constante), ou NULL.
  contexte_affectation <- function(r_noeud) {
    pere <- par[pd$id[r_noeud]]
    if (!pere) return(NULL)
    k <- fils(pere)
    if (length(k) != 3L || !(jeton[k[2L]] %in% OPERATEURS_AFFECTATION)) return(NULL)
    droite <- if (jeton[k[2L]] == "RIGHT_ASSIGN") k[1L] else k[3L]
    gauche <- if (jeton[k[2L]] == "RIGHT_ASSIGN") k[3L] else k[1L]
    if (droite != r_noeud) return(NULL)
    g <- fils(pd$id[gauche])
    if (length(g) == 1L && jeton[g] == "SYMBOL") {
      if (grepl(CONSTANTE_GRAINE, texte[g])) return(list(contexte = texte[g], constante = TRUE))
      return(NULL)
    }
    if (length(g) == 3L && jeton[g[2L]] %in% c("'$'", "'@'") && jeton[g[3L]] == "SYMBOL" &&
        texte[g[3L]] == "graine")
      return(list(contexte = "$graine", constante = FALSE))
    if (length(g) >= 4L && jeton[g[2L]] == "LBB" &&
        any(vapply(fils(pd$id[g[3L]]), function(s) jeton[s] == "STR_CONST" && nom_litteral(s) == "graine", NA)))
      return(list(contexte = "[[graine]]", constante = FALSE))
    NULL
  }
  contexte_de <- function(r) {
    e1 <- ix[par[pd$id[r]]]
    if (!length(e1) || !e1 || !est_expr(e1) || length(fils(pd$id[e1])) != 1L) return(NULL)
    p2 <- par[pd$id[e1]]
    if (!p2) return(NULL)
    k2 <- fils(p2)
    pos <- match(e1, k2)
    nom_arg <- NULL
    if (pos >= 3L && jeton[k2[pos - 1L]] %in% c("EQ_SUB", "EQ_FORMALS") &&
        jeton[k2[pos - 2L]] %in% c("SYMBOL_SUB", "SYMBOL_FORMALS", "STR_CONST"))
      nom_arg <- nom_litteral(k2[pos - 2L])
    est_appel <- length(k2) >= 3L && est_expr(k2[1L]) && jeton[k2[2L]] == "'('" && pos >= 3L
    if (est_appel) {
      f <- nom_appelee(k2[1L])
      if (f == "set.seed") return(list(contexte = "set.seed", constante = FALSE))
      if (f == "sous_graine") {
        positionnels <- Filter(function(i) est_expr(k2[i]) && i >= 3L && jeton[k2[i - 1L]] != "EQ_SUB",
                               seq_along(k2))
        if (identical(nom_arg, "graine") || (is.null(nom_arg) && length(positionnels) && positionnels[1L] == pos))
          return(list(contexte = "sous_graine", constante = FALSE))
      }
      if (f == "c") {
        a <- contexte_affectation(ix[p2])
        if (!is.null(a)) return(a)
      }
      etiquette <- list(contexte = sprintf("graine= (%s)", f), constante = FALSE)
      if (identical(nom_arg, "graine")) return(etiquette)
      # Positionnel (ou nom partiel) tombant sur un formel `graine`.
      sigs <- formels[[f]]
      if (nzchar(f) && !is.null(sigs)) {
        args <- arguments_appel(k2)
        k <- match(e1, args$lignes)
        if (!args$vide && !is.na(k) && tombe_sur_graine(sigs, args$noms, k)) return(etiquette)
      }
      return(NULL)
    }
    if (identical(nom_arg, "graine") && jeton[k2[pos - 1L]] == "EQ_FORMALS")
      return(list(contexte = "graine= (formals)", constante = FALSE))
    contexte_affectation(e1)
  }
  cand <- which(jeton == "NUM_CONST" & grepl("^[0-9]+L?$", texte))
  sorties <- list()
  for (r in cand) {
    cx <- contexte_de(r)
    if (is.null(cx)) next
    sorties[[length(sorties) + 1L]] <-
      data.frame(valeur = as.numeric(sub("L$", "", texte[r])), fichier = nom, ligne = pd$line1[r],
                 contexte = cx$contexte, constante = cx$constante, stringsAsFactors = FALSE)
  }
  if (!length(sorties)) relever_aucun() else do.call(rbind, sorties)
}

# Relevé de l'arbre `racine` : rend list(occurrences, defauts, fichiers). Deux
# passes : lecture de tous les fichiers et de leurs définitions de fonctions,
# puis relevé des littéraux.
relever_graines <- function(racine) {
  defauts <- character(0)
  fichiers <- character(0)
  for (d in DOSSIERS_RELEVES) {
    dossier <- file.path(racine, d)
    if (!dir.exists(dossier)) next
    f <- list.files(dossier, pattern = "\\.[Rr]$", recursive = TRUE)
    # Dossier sans fichier R (inst/ ne porte que des tables, ADR 0018) :
    # paste() sur character(0) rendrait « inst/ », lu comme un fichier.
    if (length(f)) fichiers <- c(fichiers, paste(d, gsub("\\\\", "/", f), sep = "/"))
  }
  fichiers <- sort(fichiers)
  arbres <- list(); formels <- list()
  for (nom in fichiers) {
    a <- tryCatch(lire_arbre(file.path(racine, nom)), error = function(e) e)
    if (inherits(a, "error")) {
      defauts <- c(defauts, sprintf("%s : illisible (%s)", nom, conditionMessage(a)))
      next
    }
    if (is.null(a)) next
    arbres[[nom]] <- a
    fo <- formels_de(a)
    for (n in names(fo)) formels[[n]] <- c(formels[[n]], fo[[n]])
  }
  occ <- lapply(names(arbres), function(nom) releve_arbre(arbres[[nom]], nom, formels))
  list(occurrences = if (length(occ)) do.call(rbind, occ) else relever_aucun(),
       defauts = defauts, fichiers = fichiers)
}

# --- liste blanche ---------------------------------------------------------------

liste_fichiers <- function(x) {
  v <- strsplit(x, "|", fixed = TRUE)[[1L]]
  sort(unique(trimws(v[nzchar(trimws(v))])))
}

# Lit la liste blanche CSV ; rend list(entrees, defauts). Une entrée :
# valeur, etendue, classe, fichiers (vecteur trié), motif, decision.
lire_admises <- function(chemin) {
  if (!file.exists(chemin))
    return(list(entrees = NULL, defauts = sprintf("liste blanche introuvable : %s", chemin)))
  t <- tryCatch(utils::read.csv(chemin, colClasses = "character", encoding = "UTF-8",
                                na.strings = character(0), stringsAsFactors = FALSE),
                error = function(e) e)
  if (inherits(t, "error"))
    return(list(entrees = NULL, defauts = sprintf("liste blanche illisible (%s)", conditionMessage(t))))
  if (!identical(names(t), COLONNES_ADMISES))
    return(list(entrees = NULL, defauts = sprintf("liste blanche : colonnes attendues %s, lues %s",
                                                   paste(COLONNES_ADMISES, collapse = ","),
                                                   paste(names(t), collapse = ","))))
  defauts <- character(0)
  entrees <- list()
  for (i in seq_len(nrow(t))) {
    v <- suppressWarnings(as.numeric(t$valeur[i])); e <- suppressWarnings(as.numeric(t$etendue[i]))
    ligne <- sprintf("liste blanche, ligne %d (valeur %s)", i + 1L, t$valeur[i])
    if (is.na(v) || v != round(v) || v < SEUIL_GARDE) {
      defauts <- c(defauts, sprintf("%s : valeur entière >= %d attendue", ligne, SEUIL_GARDE)); next
    }
    if (is.na(e) || e != round(e) || e < 1) {
      defauts <- c(defauts, sprintf("%s : etendue entière >= 1 attendue", ligne)); next
    }
    if (!(t$classe[i] %in% CLASSES_ADMISES)) {
      defauts <- c(defauts, sprintf("%s : classe %s inconnue (attendues : %s)", ligne, t$classe[i],
                                    paste(CLASSES_ADMISES, collapse = ", "))); next
    }
    if (t$classe[i] == "plage" && e < 2) {
      defauts <- c(defauts, sprintf("%s : une plage a une etendue >= 2", ligne)); next
    }
    if (t$classe[i] != "plage" && e != 1) {
      defauts <- c(defauts, sprintf("%s : une classe %s est une valeur isolée (etendue 1)", ligne, t$classe[i])); next
    }
    fch <- liste_fichiers(t$fichiers[i])
    if (!length(fch)) {
      defauts <- c(defauts, sprintf("%s : aucun fichier", ligne)); next
    }
    if (!nzchar(trimws(t$motif[i])) || !nzchar(trimws(t$decision[i]))) {
      defauts <- c(defauts, sprintf("%s : motif et decision requis", ligne)); next
    }
    entrees[[length(entrees) + 1L]] <- list(valeur = v, etendue = e, classe = t$classe[i], fichiers = fch,
                                            motif = t$motif[i], decision = t$decision[i], origine = "liste blanche",
                                            pour_memoire = FALSE)
  }
  list(entrees = entrees, defauts = defauts)
}

entrees_heritage <- function(heritage) {
  lapply(seq_len(nrow(heritage)), function(i)
    list(valeur = heritage$valeur[i], etendue = 1, classe = "heritage",
         fichiers = liste_fichiers(heritage$fichiers[i]), motif = heritage$motif[i],
         decision = "héritage antérieur à M300", origine = "HERITAGE_176",
         pour_memoire = isTRUE(heritage$pour_memoire[i])))
}

# --- analyse ---------------------------------------------------------------------

decrire <- function(o) {
  cle <- paste(o$fichier, o$contexte, sep = "\r")
  vapply(unique(cle), function(k) {
    s <- o[cle == k, , drop = FALSE]
    sprintf("%s:%s [%s]", s$fichier[1L], paste(s$ligne, collapse = ","), s$contexte[1L])
  }, "")
}

# Analyse les occurrences `occ` (relever_graines()$occurrences) contre les
# entrées `entrees` ; `fichiers_arbre` : fichiers relevés ; rend les écarts.
analyser_graines <- function(occ, entrees, fichiers_arbre = NULL, racine = NULL) {
  defauts <- character(0)
  notes <- character(0)
  # A5 : garde < 1000 exemptée, sauf sous une constante GRAINE*.
  for (i in which(occ$valeur < SEUIL_GARDE & occ$constante))
    defauts <- c(defauts, sprintf(
      "%s:%d : constante %s = %d : une graine à usage statistique ne prend pas une valeur de garde (< %d ; A5)",
      occ$fichier[i], occ$ligne[i], occ$contexte[i], as.integer(occ$valeur[i]), SEUIL_GARDE))
  occ <- occ[occ$valeur >= SEUIL_GARDE, , drop = FALSE]
  # Entrées : unicité d'une valeur isolée entre liste blanche et héritage.
  ponctuelles <- Filter(function(e) e$classe != "plage", entrees)
  plages <- Filter(function(e) e$classe == "plage", entrees)
  vp <- vapply(ponctuelles, function(e) e$valeur, 0)
  for (v in unique(vp[duplicated(vp)]))
    defauts <- c(defauts, sprintf("%d : plusieurs entrées (liste blanche, héritage) pour la même valeur", as.integer(v)))
  # Recouvrement des plages entre elles.
  if (length(plages) > 1L) for (i in seq_along(plages)[-1L]) for (j in seq_len(i - 1L)) {
    a <- plages[[i]]; b <- plages[[j]]
    if (a$valeur < b$valeur + b$etendue && b$valeur < a$valeur + a$etendue)
      defauts <- c(defauts, sprintf("plages qui se recouvrent : %d..%d (%s) et %d..%d (%s)",
                                    as.integer(a$valeur), as.integer(a$valeur + a$etendue - 1),
                                    paste(a$fichiers, collapse = "|"),
                                    as.integer(b$valeur), as.integer(b$valeur + b$etendue - 1),
                                    paste(b$fichiers, collapse = "|")))
  }
  if (!is.null(racine)) for (e in plages)
    for (f in e$fichiers[!file.exists(file.path(racine, e$fichiers))])
      defauts <- c(defauts, sprintf("plage %d..%d : fichier déclaré introuvable : %s", as.integer(e$valeur),
                                    as.integer(e$valeur + e$etendue - 1), f))
  ent_de <- function(v) { k <- which(vp == v); if (length(k)) ponctuelles[[k[1L]]] else NULL }
  # Valeurs >= 1000 : plusieurs fichiers ou plusieurs contextes d'un fichier.
  for (v in sort(unique(occ$valeur))) {
    o <- occ[occ$valeur == v, , drop = FALSE]
    fich <- sort(unique(o$fichier))
    ctx_par_fichier <- tapply(o$contexte, o$fichier, function(x) length(unique(x)))
    reemploi <- length(fich) > 1L || any(ctx_par_fichier > 1L)
    e <- ent_de(v)
    if (!reemploi) {
      if (!is.null(e) && !identical(e$fichiers, fich))
        defauts <- c(defauts, sprintf(
          "%d : entrée %s (%s) pour une valeur qui n'est plus réemployée : fichiers déclarés %s, observés %s ; retirer l'entrée",
          as.integer(v), e$classe, e$origine, paste(e$fichiers, collapse = "|"), paste(fich, collapse = "|")))
      else if (!is.null(e))
        notes <- c(notes, sprintf("%d : entrée %s (%s) sans réemploi observé (un fichier, un contexte)",
                                  as.integer(v), e$classe, e$origine))
      next
    }
    if (is.null(e)) {
      defauts <- c(defauts, sprintf(
        "%d : réemploi non admis (X1 à X3), à corriger par une valeur propre : %s",
        as.integer(v), paste(decrire(o), collapse = " ; ")))
      next
    }
    if (!identical(e$fichiers, fich)) {
      nouveaux <- setdiff(fich, e$fichiers); disparus <- setdiff(e$fichiers, fich)
      defauts <- c(defauts, sprintf(
        "%d : entrée %s (%s) : ensemble de fichiers différent de l'ensemble observé (X2 ou X3)%s%s",
        as.integer(v), e$classe, e$origine,
        if (length(nouveaux)) sprintf(" ; croissance, fichier(s) nouveau(x) : %s", paste(nouveaux, collapse = ", ")) else "",
        if (length(disparus)) sprintf(" ; fichier(s) sans occurrence, à retirer de l'entrée : %s", paste(disparus, collapse = ", ")) else ""))
    }
  }
  # Entrées sans aucune occurrence : écart, sauf entrée d'héritage pour mémoire
  # (décision du 08/11/2026, M338).
  sans <- Filter(function(e) !(e$valeur %in% occ$valeur), ponctuelles)
  for (e in Filter(function(e) !e$pour_memoire, sans))
    defauts <- c(defauts, sprintf(
      "%d : entrée %s (%s) sans aucune occurrence dans l'arbre relevé ; la retirer (autorisé d'avance pour l'héritage, à noter dans la PR) ou, pour l'héritage seul, la marquer pour mémoire",
      as.integer(e$valeur), e$classe, e$origine))
  memoire <- Filter(function(e) e$pour_memoire, sans)
  if (length(memoire))
    notes <- c(notes, sprintf("%d entrée(s) d'héritage pour mémoire, sans occurrence relevée : %s", length(memoire),
                              paste(vapply(memoire, function(e) as.character(as.integer(e$valeur)), ""), collapse = ", ")))
  # Plages : une valeur relevée dans une plage, hors des fichiers déclarants.
  for (e in plages) {
    dans <- occ[occ$valeur >= e$valeur & occ$valeur < e$valeur + e$etendue & !(occ$fichier %in% e$fichiers), , drop = FALSE]
    for (v in sort(unique(dans$valeur))) {
      o <- dans[dans$valeur == v, , drop = FALSE]
      h <- ent_de(v)
      if (!is.null(h) && h$origine == "HERITAGE_176" && all(o$fichier %in% h$fichiers)) next
      defauts <- c(defauts, sprintf(
        "%d : valeur dans la plage %d..%d déclarée par %s : %s",
        as.integer(v), as.integer(e$valeur), as.integer(e$valeur + e$etendue - 1),
        paste(e$fichiers, collapse = "|"), paste(decrire(o), collapse = " ; ")))
    }
  }
  list(defauts = defauts, notes = notes, occurrences = occ)
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  sans_heritage <- "--sans-heritage" %in% args
  args <- setdiff(args, "--sans-heritage")
  racine <- if (length(args)) args[1L] else dirname(dirname(dirname(normalizePath(script))))
  chemin <- if (length(args) > 1L) args[2L] else file.path(racine, "tests", "controles", "graines_admises.csv")
  cat("Unicité des graines :", racine, "\n")
  rel <- relever_graines(racine)
  lb <- lire_admises(chemin)
  defauts <- c(rel$defauts, lb$defauts)
  occ <- rel$occurrences
  cat(sprintf("  %d littéral(aux) entier(s) de graine dans %d fichier(s) sur %d lu(s) ; %d valeur(s) distincte(s) >= %d\n",
              nrow(occ), length(unique(occ$fichier)), length(rel$fichiers),
              length(unique(occ$valeur[occ$valeur >= SEUIL_GARDE])), SEUIL_GARDE))
  if (!is.null(lb$entrees)) {
    entrees <- c(lb$entrees, if (!sans_heritage) entrees_heritage(HERITAGE_176))
    r <- analyser_graines(occ, entrees, rel$fichiers, racine)
    defauts <- c(defauts, r$defauts)
    cat(sprintf("  %d entrée(s) de liste blanche, %d d'héritage figé%s\n", length(lb$entrees),
                if (sans_heritage) 0L else nrow(HERITAGE_176), if (sans_heritage) " (écarté : --sans-heritage)" else ""))
    cat(paste0("  note : ", r$notes, "\n"), sep = "")
  }
  if (length(defauts)) {
    cat(paste0("  ECHEC : ", defauts, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Unicité des graines::", paste(defauts, collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("  OK : aucune graine >= 1000 réemployée hors liste blanche et héritage figé ; aucune constante GRAINE* < 1000.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
