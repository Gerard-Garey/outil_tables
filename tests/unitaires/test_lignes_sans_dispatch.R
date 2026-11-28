# -----------------------------------------------------------------------------
#  tests/unitaires/test_lignes_sans_dispatch.R : nombre de lignes de l'entrée
#  lu sans dispatch aux sites de #354 (suite de #343 ; avis de forme F354,
#  décision D3 (a) du plan de B36 ; CONTEXT.md, « Lecture sans dispatch »)
#
#  Sites : texte_canonique() (R/rapport.R, en-tête de la représentation
#  canonique) ; evaluer_motifs(), evaluer_motifs_agregats(),
#  evaluer_motifs_stocks(), preparer_donnees() (R/donnees.R) ; rejouer_fiche()
#  (R/construire_table.R : message et details$lignes du refus
#  empreinte_donnees_discordante, ligne « lu » du bilan, nombre de colonnes
#  compris). Chaque site lit .row_names_info(donnees, 2L) (et
#  longueur_lue(donnees) pour les colonnes du bilan) ; seule
#  controler_lignes_donnees() lit nrow() avec dispatch, à dessein (garde C2).
#
#  Classe menteuse k354 : sa méthode dim() ajoute une ligne et une colonne
#  seulement quand elle est appelée (par nrow() ou ncol()) depuis la fonction
#  dont le corps est CIBLE354$corps, honnête ailleurs ; l'appelant est
#  reconnu par son cadre d'évaluation (parent.frame(2L) dans la méthode :
#  cadre où nrow() ou ncol() est évaluée, promesse comprise, puis
#  sys.function() de ce cadre), non par la profondeur de pile du test k343
#  (tests/unitaires/test_importance_exclusions.R), qui ne voit pas
#  texte_canonique() (mesuré le 24/11/2026 : aucun mensonge compté ; nrow()
#  y est un argument de sprintf(), sous charToRaw() et writeBin()).
#  Contrôle de la reconnaissance, par
#  site : une copie de la fonction du package qui relit nrow(donnees) (état
#  d'avant #354), appelée sur k354, atteint le mensonge et en montre l'effet
#  mesuré avant correction ; sans lui, un test qui ne compte aucun mensonge
#  ne prouverait rien.
#
#  Suite #367 (B41) : empiler_stocks() (nombre de lignes de `inventaires` et
#  de `evenements`) et table_reference() (refus table_vide) lisent
#  .row_names_info(x, 2L), non nrow(x). Même classe k354, dont la méthode dim()
#  ajoute une ligne (empiler_stocks) ou rend zéro ligne (table_reference,
#  CIBLE354$mode « zero ») depuis la fonction cible seulement ; contrôle par
#  copie d'avant, garde mécanique : aucun nrow() ni ncol() dans ces deux
#  fonctions (critère C6 du plan de B41).
#
#  Natures des attendus (ADR 0007, point 11.4) : identité exacte (résultat et
#  fiche sur k354 identical() à ceux de la classe k354 sans cible ; empreinte
#  des données égale à celle du data.frame nu) ; valeurs écrites à la main
#  (nombres de lignes et de colonnes cités, lectures relevées par la garde).
#  Jeux écrits à la main, repris de test_donnees.R (lignes), test_agregats.R
#  (agrégats) et test_stocks_successifs.R (stocks) ; aucune graine.
# -----------------------------------------------------------------------------

D <- as.Date
VD <- list(depart = "vivant", arrivee = "deces")
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
BRUT_P <- list(brut = list(estimateurs = "poisson", niveau = 0.95))

BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
BV <- data.frame(sexe = c("H", "H", "F", "F"), seg = c("a", "b", "a", "b"), age = c(60, 61, 60, 61),
                 annee = c(2016, 2017, 2016, 2017), ec = c(10, 11, 12, 13), ei = c(10.5, 11, 12.25, 13),
                 d = c(1, 0, 1, 0), stringsAsFactors = FALSE)
R_BV <- c(sexe = "sexe", segment = "seg", age = "age", annee = "annee", exposition_centrale = "ec",
          exposition_initiale = "ei", evenements = "d")
PER_S <- list(debut = D("2015-01-01"), fin = D("2016-12-31"))
G3 <- D(c("2014-12-31", "2015-12-31", "2016-12-31"))
NA_D <- D(NA)
BS <- data.frame(
  id = c("A", "A", "A", "B", "B", "B", "C", "C", "D", "E", "F", "F"),
  t = c("inventaire", "inventaire", "inventaire", "inventaire", "inventaire", "evenement", "inventaire", "inventaire",
        "evenement", "evenement", "inventaire", "evenement"),
  di = c(G3, G3[1:2], NA_D, G3[2:3], NA_D, NA_D, G3[1L], NA_D),
  sx = c("H", "H", "H", "F", "F", "F", "H", "H", "F", "H", "F", "F"),
  seg = c("a", "a", "a", "b", "b", "b", "a", "a", "b", "a", "b", "b"),
  n = D(c(rep("1950-03-01", 3), rep("1960-07-15", 3), rep("1970-01-01", 2), "1955-05-05", "1945-02-02",
          rep("1980-10-10", 2))),
  an = c(rep(1950L, 3), rep(1960L, 3), rep(1970L, 2), 1955L, 1945L, rep(1980L, 2)),
  s = D(c(NA, NA, NA, NA, NA, "2016-05-01", NA, NA, "2015-06-01", "2014-06-01", NA, "2015-03-01")),
  c = c(NA, NA, NA, NA, NA, "deces", NA, NA, "deces", "rachat", NA, "rachat"),
  stringsAsFactors = FALSE)
RS <- c(id_tete = "id", sexe = "sx", date_naissance = "n", annee_naissance = "an", type_ligne = "t",
        date_inventaire = "di", date_sortie = "s", cause_sortie = "c", segment = "seg")

# Un jeu par recette : données, rôles, segmentation, plan (étapes donnees,
# exposition, brut), valeurs p du module donnees, perturbation d'une valeur
# d'un rôle (fiche discordante au rejeu, données restées valides).
plan_354 <- function(periode, segmentation, exposition, inventaires = NULL) {
  p <- list(transition = VD, periode = periode, delai_declaration = 0)
  if (!is.null(inventaires)) p$inventaires <- inventaires
  c(p, list(segmentation = segmentation,
            etapes = c(list(donnees = list(exclusions = character(0)), exposition = exposition), BRUT_P)))
}
JEUX <- list(
  lignes = list(d = BASE, r = ROLES, seg = "sexe",
                plan = plan_354(PER, "sexe", list(convention = "plus_fine", par_annee = FALSE)),
                p = tablexp:::valeurs_parametres_donnees(VD, PER, 0),
                modif = function(d) { d$naissance[1L] <- d$naissance[1L] + 1; d }),
  agregats = list(d = BV, r = R_BV, seg = "sexe",
                  plan = plan_354(PER, "sexe", list(convention = "dernier_anniversaire", par_annee = FALSE)),
                  p = tablexp:::valeurs_parametres_donnees(VD, PER, 0),
                  modif = function(d) { d$ec[1L] <- 10.25; d }),
  stocks = list(d = BS, r = RS, seg = "segment",
                plan = plan_354(PER_S, "segment", list(convention = "plus_fine", par_annee = FALSE,
                                                       entree_entre_inventaires = "milieu"), G3),
                p = tablexp:::valeurs_parametres_donnees(VD, PER_S, 0, G3),
                modif = function(d) { d$n[1:3] <- d$n[1:3] + 1; d }))

local({
  CIBLE354 <- new.env(); CIBLE354$corps <- NULL; CIBLE354$mensonges <- 0L; CIBLE354$mode <- "plus"
  m354 <- list(
    dim.k354 = function(x) {
      d <- c(.row_names_info(x, 2L), length(unclass(x)))
      if (!is.null(CIBLE354$corps)) {
        # Cadre où nrow() ou ncol() est évaluée (parent.frame(1L) est celui
        # de nrow() ou ncol()), puis fonction de ce cadre.
        cadre <- parent.frame(2L)
        i <- which(vapply(sys.frames(), identical, NA, cadre))
        if (length(i) && identical(body(sys.function(i[length(i)])), CIBLE354$corps)) {
          CIBLE354$mensonges <- CIBLE354$mensonges + 1L
          # « plus » : une ligne et une colonne de trop ; « zero » (#367) :
          # aucune ligne, colonnes comptées honnêtement.
          d <- if (identical(CIBLE354$mode, "zero")) c(0L, d[2L]) else d + 1L
        }
      }
      d })
  for (n in names(m354)) assign(n, m354[[n]], envir = globalenv())
  on.exit(rm(list = names(m354), envir = globalenv()))
  k354 <- function(d) structure(d, class = c("k354", "data.frame"))
  attrape <- function(expr) tryCatch(expr, refus_tablexp = function(r) r, error = function(e) e)
  # Avertissements de R relevés pendant expr (rendus avec la valeur).
  avec_avertissements <- function(expr) {
    w <- character(0)
    v <- withCallingHandlers(attrape(expr), warning = function(c) {
      w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning") })
    list(valeur = v, avertissements = w)
  }
  # Appel sous la cible `corps` ; rend la valeur, les mensonges comptés et les
  # avertissements de R.
  sous_cible <- function(corps, expr, mode = "plus") {
    CIBLE354$corps <- corps; CIBLE354$mensonges <- 0L; CIBLE354$mode <- mode
    on.exit({ CIBLE354$corps <- NULL; CIBLE354$mode <- "plus" })
    a <- avec_avertissements(expr)
    c(a, list(mensonges = CIBLE354$mensonges))
  }
  comparable <- function(x) x[c("table", "etapes", "avertissements", "fiche")]
  # Copie de la fonction du package qui relit nrow(donnees) (et ncol(donnees)
  # pour longueur_lue(donnees)), même environnement (espace de noms du
  # package) ; remplacement fait sur le langage (appels identical() à
  # .row_names_info(donnees, 2L) ou longueur_lue(donnees)), sans deparse() ;
  # rend NULL si le nombre d'appels remplacés n'est pas `attendu`.
  # `symboles` : arguments dont la lecture .row_names_info(s, 2L) est
  # remplacée par nrow(s) (« donnees » par défaut ; #367 : inventaires et
  # evenements, ou x). `gardees` : nombre de premières lectures (ordre du texte)
  # laissées intactes, comptées à part, qui ne sont pas des lectures d'avant
  # (#368 : table_reference() lit d'abord .row_names_info(x, 2L) pour comparer
  # la longueur des colonnes, puis pour table_vide ; seule la seconde est celle
  # d'avant #367).
  copie_avant <- function(nom, attendu, symboles = "donnees", gardees = 0L) {
    reel <- get(nom, envir = asNamespace("tablexp"))
    k <- 0L; vues <- 0L
    remplacer <- function(e) {
      for (s in symboles)
        if (identical(e, call(".row_names_info", as.name(s), 2L))) {
          vues <<- vues + 1L
          if (vues <= gardees) return(e)
          k <<- k + 1L; return(call("nrow", as.name(s)))
        }
      if (identical(e, quote(longueur_lue(donnees)))) { k <<- k + 1L; return(quote(ncol(donnees))) }
      for (i in seq_along(e)) if (is.call(e[[i]])) e[[i]] <- remplacer(e[[i]])
      e
    }
    f <- reel
    body(f) <- remplacer(body(reel))
    if (k != attendu || identical(body(f), body(reel))) NULL else f
  }
  AVANT <- list(texte_canonique = copie_avant("texte_canonique", 1L),
                evaluer_motifs = copie_avant("evaluer_motifs", 1L),
                evaluer_motifs_agregats = copie_avant("evaluer_motifs_agregats", 1L),
                evaluer_motifs_stocks = copie_avant("evaluer_motifs_stocks", 1L),
                preparer_donnees = copie_avant("preparer_donnees", 1L),
                rejouer_fiche = copie_avant("rejouer_fiche", 4L),
                empiler_stocks = copie_avant("empiler_stocks", 2L, c("inventaires", "evenements")),
                table_reference = copie_avant("table_reference", 1L, "x", gardees = 1L))
  verifier("#354 copies d'avant (nrow(donnees), ncol(donnees)) formées : une lecture remplacée par fonction, quatre dans rejouer_fiche() (deux lignes, deux colonnes), deux dans empiler_stocks() et une dans table_reference() (#367 ; la première lecture de table_reference(), ajoutée par #368, est gardée), corps différents",
           !any(vapply(AVANT, is.null, NA)))
  reel <- function(nom) get(nom, envir = asNamespace("tablexp"))
  NU <- lapply(JEUX, function(J) construire_table(J$d, J$r, J$plan))
  HONNETE <- lapply(JEUX, function(J) construire_table(k354(J$d), J$r, J$plan))
  # Appel direct de preparer_donnees() (ou de sa copie) sous les contrôles de
  # construire_table() (rôles par controler_roles()).
  preparer <- function(f, J) {
    d <- k354(J$d)
    f(d, J$r, J$p, J$seg, character(0), tablexp:::controler_roles(d, J$r, J$seg))
  }

  # --- texte_canonique() (R/rapport.R) : en-tête de la représentation canonique ---
  for (j in names(JEUX)) {
    J <- JEUX[[j]]
    verifier(sprintf("#354 texte_canonique, recette %s, contrôle (copie d'avant, nrow(donnees)) : mensonge atteint, octets différents de ceux du data.frame nu (en-tête faux)", j), {
      f0 <- tempfile(); f1 <- tempfile()
      reel("texte_canonique")(J$d, J$r, f0)
      a <- sous_cible(body(AVANT$texte_canonique), AVANT$texte_canonique(k354(J$d), J$r, f1))
      o0 <- readBin(f0, "raw", 1e6); o1 <- if (file.exists(f1)) readBin(f1, "raw", 1e6) else raw(0)
      unlink(c(f0, f1))
      a$mensonges >= 1L && length(o1) > 0L && !identical(o0, o1)
    })
    verifier(sprintf("#354 texte_canonique, recette %s, construire_table() sur k354 qui ment à texte_canonique() (identité exacte) : aucun mensonge, empreinte des données égale à celle du data.frame nu, fiche rejouée sur le data.frame nu sans refus", j), {
      a <- sous_cible(body(reel("texte_canonique")), construire_table(k354(J$d), J$r, J$plan))
      x <- a$valeur
      if (inherits(x, "condition")) conditionMessage(x)
      else if (a$mensonges != 0L) sprintf("%d mensonge(s)", a$mensonges)
      else identical(x$fiche$empreintes$donnees, NU[[j]]$fiche$empreintes$donnees) &&
        identical(comparable(x), comparable(HONNETE[[j]])) &&
        !inherits(attrape(rejouer_fiche(J$d, lire_fiche(ecrire_fiche(x$fiche)))), "condition")
    })
  }

  # --- evaluer_motifs*() et preparer_donnees() (R/donnees.R) ---
  # Effet d'avant mesuré par site et par recette ; « aucun » : mensonge
  # atteint sans effet observé sur ce jeu (constat sur un jeu, non preuve
  # d'innocuité).
  SITES_DONNEES <- list(
    list(nom = "evaluer_motifs", jeu = "lignes", effet = "erreur de R « NAs are not allowed in subscripted assignments »",
         avant = function(f, J) f(k354(J$d), J$r, J$p$debut, J$p$fin, J$p$borne, J$seg),
         controle = function(a, J) inherits(a$valeur, "error") && !inherits(a$valeur, "refus_tablexp") &&
           grepl("NAs are not allowed", conditionMessage(a$valeur), fixed = TRUE)),
    list(nom = "evaluer_motifs_agregats", jeu = "agregats", effet = "aucun",
         avant = function(f, J) f(k354(J$d), J$r, J$p),
         controle = function(a, J) !inherits(a$valeur, "condition") &&
           identical(a$valeur, tablexp:::evaluer_motifs_agregats(J$d, J$r, J$p))),
    list(nom = "evaluer_motifs_stocks", jeu = "stocks", effet = "avertissements de R, motifs inchangés",
         avant = function(f, J) f(k354(J$d), J$r, J$p, J$seg),
         controle = function(a, J) !inherits(a$valeur, "condition") && length(a$avertissements) >= 1L &&
           identical(a$valeur, tablexp:::evaluer_motifs_stocks(J$d, J$r, J$p, J$seg))),
    list(nom = "preparer_donnees", jeu = "lignes", effet = "numeros_conserves 1:5 pour 4 lignes (résultat faux silencieux)",
         avant = function(f, J) preparer(f, J),
         controle = function(a, J) !inherits(a$valeur, "condition") &&
           identical(a$valeur$resultat$numeros_conserves, 1:5)),
    list(nom = "preparer_donnees", jeu = "agregats", effet = "erreur de R « replacement has 4 rows, data has 5 »",
         avant = function(f, J) preparer(f, J),
         controle = function(a, J) inherits(a$valeur, "error") && !inherits(a$valeur, "refus_tablexp") &&
           grepl("replacement has 4 rows, data has 5", conditionMessage(a$valeur), fixed = TRUE)),
    list(nom = "preparer_donnees", jeu = "stocks", effet = "numeros_conserves 1:13 pour 12 lignes (résultat faux silencieux)",
         avant = function(f, J) preparer(f, J),
         controle = function(a, J) !inherits(a$valeur, "condition") &&
           identical(a$valeur$resultat$numeros_conserves, 1:13)))
  for (s in SITES_DONNEES) {
    J <- JEUX[[s$jeu]]
    verifier(sprintf("#354 %s, recette %s, contrôle (copie d'avant, nrow(donnees)) : mensonge atteint, effet d'avant : %s", s$nom, s$jeu, s$effet), {
      a <- sous_cible(body(AVANT[[s$nom]]), s$avant(AVANT[[s$nom]], J))
      a$mensonges >= 1L && isTRUE(s$controle(a, J))
    })
  }
  # Couples (fonction, recette) où preparer_donnees() appelle la fonction :
  # une évaluation des motifs par recette, preparer_donnees() sous les trois.
  APPELS <- list(c("evaluer_motifs", "lignes"), c("evaluer_motifs_agregats", "agregats"),
                 c("evaluer_motifs_stocks", "stocks"), c("preparer_donnees", "lignes"),
                 c("preparer_donnees", "agregats"), c("preparer_donnees", "stocks"))
  for (cp in APPELS) {
    nom <- cp[1L]; j <- cp[2L]
    J <- JEUX[[j]]
    verifier(sprintf("#354 %s, recette %s, construire_table() sur k354 qui ment à %s() (identité exacte) : aucun mensonge, aucun avertissement de R, résultat et fiche identical() à ceux de k354 sans cible", nom, j, nom), {
      a <- sous_cible(body(reel(nom)), construire_table(k354(J$d), J$r, J$plan))
      x <- a$valeur
      if (inherits(x, "condition")) conditionMessage(x)
      else if (a$mensonges != 0L) sprintf("%d mensonge(s)", a$mensonges)
      else !length(a$avertissements) && identical(comparable(x), comparable(HONNETE[[j]]))
    })
  }

  # --- rejouer_fiche() (R/construire_table.R) : bilan et refus empreinte_donnees_discordante ---
  for (j in names(JEUX)) {
    J <- JEUX[[j]]
    fi <- lire_fiche(ecrire_fiche(NU[[j]]$fiche))
    nl <- .row_names_info(J$d, 2L); nc <- length(J$d)
    bilan_lu <- function(r) r$bilan$lu[r$bilan$controle == "nature_arguments"]
    attendu_lu <- function(l, c) sprintf("donnees : classe k354/data.frame, %d lignes, %d colonnes", l, c)
    verifier(sprintf("#354 rejouer_fiche, recette %s, contrôle (copie d'avant, nrow(donnees), ncol(donnees)) : mensonge atteint ; bilan « %d lignes, %d colonnes » pour %d et %d ; refus empreinte_donnees_discordante citant %d lignes (message et details$lignes)", j, nl + 1L, nc + 1L, nl, nc, nl + 1L), {
      a <- sous_cible(body(AVANT$rejouer_fiche), AVANT$rejouer_fiche(k354(J$d), fi))
      b <- sous_cible(body(AVANT$rejouer_fiche), AVANT$rejouer_fiche(k354(J$modif(J$d)), fi))
      a$mensonges >= 1L && b$mensonges >= 1L && !inherits(a$valeur, "condition") &&
        grepl(attendu_lu(nl + 1L, nc + 1L), bilan_lu(a$valeur), fixed = TRUE) &&
        inherits(b$valeur, "refus_tablexp") && identical(b$valeur$motif, "empreinte_donnees_discordante") &&
        identical(b$valeur$details$lignes, nl + 1L) &&
        grepl(sprintf("(%d lignes)", nl + 1L), conditionMessage(b$valeur), fixed = TRUE)
    })
    a <- sous_cible(body(rejouer_fiche), rejouer_fiche(k354(J$d), fi))
    verifier(sprintf("#354 rejouer_fiche, recette %s, fiche concordante, k354 qui ment à rejouer_fiche() (valeur écrite à la main) : rejeu accepté, ligne « lu » du bilan « %d lignes, %d colonnes »", j, nl, nc), {
      if (inherits(a$valeur, "condition")) conditionMessage(a$valeur)
      else grepl(attendu_lu(nl, nc), bilan_lu(a$valeur), fixed = TRUE)
    })
    b <- sous_cible(body(rejouer_fiche), rejouer_fiche(k354(J$modif(J$d)), fi))
    verifier(sprintf("#354 rejouer_fiche, recette %s, fiches concordante et discordante, k354 qui ment à rejouer_fiche() : aucun mensonge (aucune lecture de dim() depuis rejouer_fiche())", j),
             a$mensonges == 0L && b$mensonges == 0L)
    verifier(sprintf("#354 rejouer_fiche, recette %s, fiche discordante, k354 qui ment à rejouer_fiche() (valeur écrite à la main) : refus empreinte_donnees_discordante, message « (%d lignes) »", j, nl), {
      inherits(b$valeur, "refus_tablexp") &&
        identical(b$valeur$motif, "empreinte_donnees_discordante") &&
        grepl(sprintf("(%d lignes)", nl), conditionMessage(b$valeur), fixed = TRUE)
    })
    verifier(sprintf("#354 rejouer_fiche, recette %s, fiche discordante, k354 qui ment à rejouer_fiche() (valeur écrite à la main) : details$lignes = %d (entier)", j, nl), {
      inherits(b$valeur, "refus_tablexp") && identical(b$valeur$details$lignes, nl)
    })
  }

  # --- empiler_stocks() et table_reference() (R/donnees.R) : #367 ---
  # Jeux écrits à la main : trois lignes d'inventaire et trois lignes
  # d'événement (six lignes empilées) ; table de référence de six lignes.
  I3 <- data.frame(id = c("A", "B", "C"), di = G3, sx = c("H", "F", "H"), stringsAsFactors = FALSE)
  E3 <- data.frame(id = c("A", "D", "E"), sx = c("H", "F", "H"),
                   s = D(c("2016-05-01", "2015-06-01", "2014-06-01")), stringsAsFactors = FALSE)
  EMPILE <- data.frame(id = c("A", "B", "C", "A", "D", "E"), di = D(c(as.character(G3), NA, NA, NA)),
                       sx = c("H", "F", "H", "H", "F", "H"), s = D(c(NA, NA, NA, "2016-05-01", "2015-06-01", "2014-06-01")),
                       t = c(rep("inventaire", 3L), rep("evenement", 3L)), stringsAsFactors = FALSE)
  TREF <- data.frame(age = c(60L, 61L, 62L, 60L, 61L, 62L), sexe = c("H", "H", "H", "F", "F", "F"),
                     qx = c(0.01, 0.011, 0.012, 0.006, 0.0066, 0.0072), stringsAsFactors = FALSE)
  verifier("#367 empiler_stocks, jeu de contrôle (valeurs écrites à la main) : data.frame nu rendu identique à la table de six lignes écrite à la main",
           identical(empiler_stocks(I3, E3, "t"), EMPILE))
  for (cas in list(list(nom = "inventaires", i = TRUE, e = FALSE), list(nom = "evenements", i = FALSE, e = TRUE))) {
    arg_i <- function() if (cas$i) k354(I3) else I3
    arg_e <- function() if (cas$e) k354(E3) else E3
    verifier(sprintf("#367 empiler_stocks, `%s` de classe k354, contrôle (copie d'avant, nrow(inventaires), nrow(evenements)) : mensonge atteint, une ligne de trop rendue (sept lignes au lieu de six) sans refus", cas$nom), {
      a <- sous_cible(body(AVANT$empiler_stocks), AVANT$empiler_stocks(arg_i(), arg_e(), "t"))
      v <- a$valeur
      a$mensonges >= 1L && !inherits(v, "condition") && is.data.frame(v) && .row_names_info(v, 2L) == 7L
    })
    verifier(sprintf("#367 empiler_stocks, `%s` de classe k354 qui ment à empiler_stocks() (identité exacte) : aucun mensonge, aucun avertissement de R, table de six lignes identique à celle du data.frame nu", cas$nom), {
      a <- sous_cible(body(reel("empiler_stocks")), empiler_stocks(arg_i(), arg_e(), "t"))
      if (inherits(a$valeur, "condition")) conditionMessage(a$valeur)
      else if (a$mensonges != 0L) sprintf("%d mensonge(s)", a$mensonges)
      else !length(a$avertissements) && identical(a$valeur, EMPILE)
    })
  }
  verifier("#367 empiler_stocks, les deux tables de classe k354 qui mentent à empiler_stocks() (identité exacte) : aucun mensonge, table de six lignes identique à celle du data.frame nu", {
    a <- sous_cible(body(reel("empiler_stocks")), empiler_stocks(k354(I3), k354(E3), "t"))
    if (inherits(a$valeur, "condition")) conditionMessage(a$valeur)
    else if (a$mensonges != 0L) sprintf("%d mensonge(s)", a$mensonges)
    else identical(a$valeur, EMPILE)
  })
  tref <- function(x) table_reference(x, "essai", "importee", "test unitaire")
  verifier("#367 table_reference, jeu de contrôle : table de six lignes admise, classe table_reference",
           inherits(tref(TREF), "table_reference"))
  verifier("#367 table_reference, table de trois lignes de classe k354 dont dim() rend zéro ligne, contrôle (copie d'avant, nrow(x)) : mensonge atteint, refus table_vide à tort (« aucune ligne »)", {
    a <- sous_cible(body(AVANT$table_reference), AVANT$table_reference(k354(TREF[1:3, ]), "essai", "importee", "test unitaire"),
                    mode = "zero")
    v <- a$valeur
    a$mensonges >= 1L && inherits(v, "refus_tablexp") && identical(v$motif, "table_vide")
  })
  verifier("#367 table_reference, table de six lignes de classe k354 dont dim() rend zéro ligne, table_reference() réelle (identité exacte) : aucun mensonge, table admise, colonnes et nombre de lignes identiques à ceux du data.frame nu", {
    a <- sous_cible(body(reel("table_reference")), tref(k354(TREF)), mode = "zero")
    v <- a$valeur
    if (inherits(v, "condition")) conditionMessage(v)
    else if (a$mensonges != 0L) sprintf("%d mensonge(s)", a$mensonges)
    else inherits(v, "table_reference") && identical(unclass(v), unclass(tref(TREF)))
  })
  verifier("#367 table_reference, table vraiment vide (colonnes de longueur 0), table_reference() réelle : refus table_vide conservé",
           {
             a <- sous_cible(body(reel("table_reference")), tref(TREF[0L, ]))
             inherits(a$valeur, "refus_tablexp") && identical(a$valeur$motif, "table_vide") && a$mensonges == 0L
           })
})
verifier("#354 la méthode dim de session est retirée de globalenv()",
         !("dim.k354" %in% ls(globalenv(), all.names = TRUE)))

# --- garde mécanique (C2 du plan de B36) ----------------------------------------
# Règle : dans R/*.R, toute lecture nrow(donnees ou ncol(donnees est relevée
# sur l'arbre du langage (parse(), en UTF-8 par analyser_fichier() de
# tests/controles/lecture_utf8.R, puis descente récursive, valeurs par défaut
# des paramètres comprises) : appel dont la fonction est nrow ou ncol
# (backquotes comprises, base::nrow, base:::nrow...), dont l'argument nommé x,
# à défaut le premier argument non nommé, est le symbole donnees ou commence
# par lui (parenthèses, `[`, `[[`, `$` ou `@` appliqués à donnees). Les
# commentaires et les textes de message ne sont pas du langage : jamais
# relevés ; l'écriture en tube (donnees |> nrow()) est un appel nrow(donnees)
# après l'analyse. Une autre écriture (dim(donnees), NROW(donnees),
# nrow(base_donnees), objet renommé) n'est pas vue. Seule lecture admise :
# nrow(donnees) dans controler_lignes_donnees() (lecture avec dispatch
# voulue, CONTEXT.md, « Lecture sans dispatch », point (1)), reconnue par la
# fonction englobante (nom affecté par l'expression de premier niveau), non
# par un numéro de ligne.
lecture_354 <- local({
  sys.source(file.path(RACINE, "tests", "controles", "lecture_utf8.R"), envir = environment())
  list(analyser_fichier = analyser_fichier)
})
# Rend un data.frame (ligne, lecture, fonction) des lectures relevées dans
# l'expression analysée `ex` (keep.source = TRUE) ; ligne : première ligne de
# l'instruction qui contient la lecture (srcref des blocs `{`) ; fonction : nom
# affecté par l'expression de premier niveau qui la contient. `symboles` :
# symboles dont la lecture est relevée (« donnees » par défaut) ; NULL : toute
# lecture nrow() ou ncol(), quel que soit l'argument (garde de #367).
lectures_dispatch <- function(ex, symboles = "donnees") {
  sr <- attr(ex, "srcref")
  rel <- list()
  est_base <- function(f, noms) {
    if (is.name(f)) return(as.character(f) %in% noms)
    is.call(f) && (identical(f[[1L]], as.name("::")) || identical(f[[1L]], as.name(":::"))) &&
      identical(f[[2L]], as.name("base")) && as.character(f[[3L]]) %in% noms
  }
  commence_par_donnees <- function(a) {
    if (is.null(symboles)) return(TRUE)
    repeat {
      if (is.name(a)) return(as.character(a) %in% symboles)
      if (!is.call(a) || length(a) < 2L) return(FALSE)
      if (!(is.name(a[[1L]]) && as.character(a[[1L]]) %in% c("(", "[", "[[", "$", "@"))) return(FALSE)
      a <- a[[2L]]
    }
  }
  argument_lu <- function(e) {
    noms <- names(e); if (is.null(noms)) noms <- rep("", length(e))
    k <- which(noms[-1L] == "x") + 1L
    if (!length(k)) k <- which(noms[-1L] == "") + 1L
    if (length(k)) e[[k[1L]]] else NULL
  }
  descendre <- function(e, ligne, fonction) {
    if (is.pairlist(e)) {
      for (i in seq_along(e)) if (is.call(e[[i]]) || is.pairlist(e[[i]])) descendre(e[[i]], ligne, fonction)
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    if (est_base(e[[1L]], c("nrow", "ncol")) && length(e) >= 2L) {
      a <- argument_lu(e)
      if (!is.null(a) && commence_par_donnees(a)) {
        f <- e[[1L]]
        rel[[length(rel) + 1L]] <<- data.frame(ligne = ligne, lecture = as.character(if (is.name(f)) f else f[[3L]]),
                                               fonction = fonction, stringsAsFactors = FALSE)
      }
    }
    s <- if (identical(e[[1L]], as.name("{"))) attr(e, "srcref") else NULL
    for (i in seq_along(e)) {
      if (is.call(e[[i]]) || is.pairlist(e[[i]])) {
        l <- if (!is.null(s) && i <= length(s)) as.integer(s[[i]][1L]) else ligne
        descendre(e[[i]], l, fonction)
      }
    }
    invisible()
  }
  for (k in seq_along(ex)) {
    e <- ex[[k]]
    nom <- if (is.call(e) && is.name(e[[1L]]) && as.character(e[[1L]]) %in% c("<-", "=") && is.name(e[[2L]]))
      as.character(e[[2L]]) else NA_character_
    descendre(e, as.integer(sr[[k]][1L]), nom)
  }
  if (length(rel)) do.call(rbind, rel)
  else data.frame(ligne = integer(0), lecture = character(0), fonction = character(0), stringsAsFactors = FALSE)
}
verifier("#354 garde C2, contrôle de la règle sur un texte écrit à la main : lectures du code relevées (positionnelle, base::, base:::, espace, x =, backquotes, commentaire entre parenthèse et argument, tube, parenthèses, indiçage), valeur par défaut d'un paramètre comprise ; commentaire, texte de message, NROW() et autre objet ignorés ; fonction englobante nommée", {
  t <- c("f <- function(donnees) {",
         "  a <- nrow(donnees); b <- base::ncol( donnees ); c <- nrow(x = donnees)",
         "  # nrow(donnees) en commentaire",
         "  stop(\"dans 1..nrow(donnees) (erreur de programmation)\")",
         "  nrow(base_donnees) + NROW(donnees)",
         "}",
         "g <- function(donnees) ncol(donnees)",
         "h <- function(donnees, n = nrow(donnees)) {",
         "  `nrow`(donnees); nrow(`donnees`); nrow(`x` = donnees)",
         "  nrow( # c",
         "    donnees)",
         "  donnees |> nrow(); nrow((donnees)); base:::ncol(donnees[1:2, ]); ncol(donnees$a)",
         "}")
  r <- lectures_dispatch(parse(text = t, keep.source = TRUE))
  att <- data.frame(ligne = c(2L, 2L, 2L, 7L, 8L, 9L, 9L, 9L, 10L, 12L, 12L, 12L, 12L),
                    lecture = c("nrow", "ncol", "nrow", "ncol", "nrow", "nrow", "nrow", "nrow", "nrow", "nrow", "nrow",
                                "ncol", "ncol"),
                    fonction = c("f", "f", "f", "g", rep("h", 9L)), stringsAsFactors = FALSE)
  if (identical(r, att)) TRUE
  else paste("relevé :", paste(sprintf("%d %s %s", r$ligne, r$lecture, r$fonction), collapse = " ; "))
})
verifier("#354 garde C2 : dans R/*.R, une seule lecture nrow(donnees) ou ncol(donnees), nrow() dans controler_lignes_donnees() (R/donnees.R)", {
  fichiers <- sort(list.files(file.path(RACINE, "R"), pattern = "\\.R$"))
  r <- do.call(rbind, lapply(fichiers, function(f) {
    x <- lectures_dispatch(lecture_354$analyser_fichier(file.path(RACINE, "R", f), keep.source = TRUE))
    if (nrow(x)) cbind(fichier = f, x, stringsAsFactors = FALSE) else NULL
  }))
  admis <- !is.null(r) && nrow(r) == 1L && identical(r$fichier, "donnees.R") && identical(r$lecture, "nrow") &&
    identical(r$fonction, "controler_lignes_donnees")
  if (admis) TRUE
  else paste("lectures relevées :", paste(sprintf("R/%s:%d %s() dans %s", r$fichier, r$ligne, r$lecture, r$fonction),
                                          collapse = " ; "))
})

# --- garde mécanique de #367 (critère C6 du plan de B41) -------------------------
# empiler_stocks() et table_reference() (R/donnees.R) : aucune lecture nrow() ni
# ncol(), quel que soit l'argument (alias, boucle sur les tables comprises) ;
# le nombre de lignes se lit par .row_names_info(x, 2L). Même relevé sur
# l'arbre du langage que la garde C2 (commentaires et textes de message ignorés).
verifier("#367 garde, contrôle de la règle sur un texte écrit à la main : nrow() et ncol() relevés quel que soit l'argument (symbole, indiçage, alias), seulement dans la fonction englobante ; .row_names_info(), commentaire et texte de message ignorés", {
  t <- c("f <- function(a, b) {",
         "  n <- .row_names_info(a, 2L)  # nrow(a) en commentaire",
         "  stop(\"nrow(b)\")",
         "  tab <- b; nrow(tab) + ncol(a[[1L]])",
         "}",
         "g <- function(x) !nrow(x)")
  r <- lectures_dispatch(parse(text = t, keep.source = TRUE), symboles = NULL)
  att <- data.frame(ligne = c(4L, 4L, 6L), lecture = c("nrow", "ncol", "nrow"), fonction = c("f", "f", "g"),
                    stringsAsFactors = FALSE)
  if (identical(r, att)) TRUE
  else paste("relevé :", paste(sprintf("%d %s %s", r$ligne, r$lecture, r$fonction), collapse = " ; "))
})
verifier("#367 garde : aucune lecture nrow() ni ncol() dans empiler_stocks() et table_reference() (R/donnees.R)", {
  r <- lectures_dispatch(lecture_354$analyser_fichier(file.path(RACINE, "R", "donnees.R"), keep.source = TRUE),
                         symboles = NULL)
  r <- r[r$fonction %in% c("empiler_stocks", "table_reference"), ]
  if (!nrow(r)) TRUE
  else paste("lectures relevées :", paste(sprintf("R/donnees.R:%d %s() dans %s", r$ligne, r$lecture, r$fonction),
                                          collapse = " ; "))
})
