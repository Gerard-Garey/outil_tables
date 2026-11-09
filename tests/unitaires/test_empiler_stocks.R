# -----------------------------------------------------------------------------
#  tests/unitaires/test_empiler_stocks.R : adapter empiler_stocks() et
#  provenance de k fichiers (issue #181 ; ADR 0014, point 11 et question 5,
#  annotation du 08/11/2026, (d) ; ADR 0016, point 8.5 ; ADR 0007, annotation
#  du 08/11/2026, (a) ; avis de forme (0'') de B15, G9, G10, G12 ; M332)
#
#  Commit (5) : empiler_stocks(inventaires, evenements, colonne_type),
#  exportée, structure seule (ordre des lignes et des colonnes, complétion de
#  structure par NA de la classe de la colonne présente, aucune conversion),
#  refus nommés chacun levé seul ; forme rendue consommée telle quelle par la
#  recette `stocks_successifs` ; provenance de k >= 1 fichiers en vecteurs
#  parallèles : contrôle, fiche, écriture et relecture de la fiche, rejeu,
#  rapport figé.
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  valeurs écrites à la main (tables construites à la main, refus, motifs,
#  détails, champs de fiche) ; identité exacte (empilement de la forme
#  « stocks successifs » du simulateur scindée dans le test, construction sur
#  la table empilée contre la construction sur la forme du simulateur,
#  aller-retour de la fiche, rejeu). Graine du jeu simulé : 186L (< 1000,
#  exemptée du contrôle de #176, règle A5).
# -----------------------------------------------------------------------------

D <- as.Date
rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
rn <- function(x) { rownames(x) <- NULL; x }

# --- signature et export (G10) ---------------------------------------------------

verifier("empiler_stocks : exportée, signature (inventaires, evenements, colonne_type) sans défaut (valeur écrite à la main)",
         "empiler_stocks" %in% getNamespaceExports("tablexp") &&
           identical(names(formals(empiler_stocks)), c("inventaires", "evenements", "colonne_type")) &&
           all(vapply(formals(empiler_stocks), function(a) identical(a, quote(expr = )), logical(1L))))

# --- structure (valeurs écrites à la main) ---------------------------------------

INV <- data.frame(id = c(1L, 2L, 1L), di = D(c("2014-12-31", "2014-12-31", "2015-12-31")), sx = c("H", "F", "H"),
                  n = D(c("1960-01-01", "1970-05-05", "1960-01-01")), stringsAsFactors = FALSE)
EVT <- data.frame(id = c(2L, 3L), sx = c("F", "H"), n = D(c("1970-05-05", "1955-02-02")),
                  s = D(c("2015-06-01", "2015-09-30")), c = c("deces", "rachat"), stringsAsFactors = FALSE)
verifier("structure (valeurs écrites à la main) : inventaires puis événements dans l'ordre reçu ; colonnes des inventaires, puis des seuls événements, puis colonne_type ; NA de la classe de la colonne présente ; data.frame nu, row.names automatiques", {
  attendu <- data.frame(id = c(1L, 2L, 1L, 2L, 3L),
                        di = D(c("2014-12-31", "2014-12-31", "2015-12-31", NA, NA)),
                        sx = c("H", "F", "H", "F", "H"),
                        n = D(c("1960-01-01", "1970-05-05", "1960-01-01", "1970-05-05", "1955-02-02")),
                        s = D(c(NA, NA, NA, "2015-06-01", "2015-09-30")),
                        c = c(NA, NA, NA, "deces", "rachat"),
                        t = c("inventaire", "inventaire", "inventaire", "evenement", "evenement"),
                        stringsAsFactors = FALSE)
  identical(empiler_stocks(INV, EVT, "t"), attendu)
})
verifier("structure (valeurs écrites à la main) : table d'inventaires ou d'événements sans ligne admise ; aucune ligne créée", {
  a <- empiler_stocks(INV[0L, ], EVT, "t")
  b <- empiler_stocks(INV, EVT[0L, ], "t")
  identical(nrow(a), 2L) && identical(a$t, rep("evenement", 2L)) && identical(a$di, D(c(NA_character_, NA_character_))) &&
    identical(names(a), c("id", "di", "sx", "n", "s", "c", "t")) &&
    identical(nrow(b), 3L) && identical(b$t, rep("inventaire", 3L)) && identical(b$c, rep(NA_character_, 3L)) &&
    identical(nrow(empiler_stocks(INV[0L, ], EVT[0L, ], "t")), 0L)
})
verifier("structure (valeurs écrites à la main) : aucune conversion ; facteur commun aux mêmes niveaux, Date à stockage integer, colonne propre à une table de classe facteur : classe, niveaux et stockage gardés", {
  inv <- transform(INV, sx = factor(sx, levels = c("F", "H")))
  inv$di <- structure(as.integer(unclass(inv$di)), class = "Date")
  inv$n <- structure(as.integer(unclass(inv$n)), class = "Date")
  evt <- transform(EVT, sx = factor(sx, levels = c("F", "H")), c = factor(c, levels = c("deces", "rachat", "autre")))
  evt$n <- structure(as.integer(unclass(evt$n)), class = "Date")
  x <- empiler_stocks(inv, evt, "t")
  identical(x$sx, factor(c("H", "F", "H", "F", "H"), levels = c("F", "H"))) &&
    identical(typeof(x$di), "integer") && identical(class(x$di), "Date") && identical(x$di[4:5], x$di[c(NA_integer_, NA_integer_)]) &&
    identical(x$n, structure(c(as.integer(unclass(INV$n)), as.integer(unclass(EVT$n))), class = "Date")) &&
    identical(x$c, factor(c(NA, NA, NA, "deces", "rachat"), levels = c("deces", "rachat", "autre"))) &&
    identical(x$t, c(rep("inventaire", 3L), rep("evenement", 2L)))
})
verifier("structure (valeurs écrites à la main) : colonnes hors rôles recopiées comme les autres ; tables d'entrée inchangées", {
  inv <- INV; inv$extra <- c(10, 20, 30)
  inv0 <- inv; evt0 <- EVT
  x <- empiler_stocks(inv, EVT, "t")
  identical(x$extra, c(10, 20, 30, NA, NA)) && identical(inv, inv0) && identical(EVT, evt0) &&
    identical(names(x), c("id", "di", "sx", "n", "extra", "s", "c", "t"))
})

# --- refus (G10 ; chacun levé seul, valeurs écrites à la main) ---------------------

CAS_REFUS <- list(
  list("inventaires hors data.frame", list(as.list(INV), EVT, "t"), "empilement_table_invalide",
       function(r) identical(r$details$table, "inventaires") && identical(r$details$classe, list(inventaires = "list"))),
  list("evenements hors data.frame (matrice)", list(INV, as.matrix(EVT), "t"), "empilement_table_invalide",
       function(r) identical(r$details$table, "evenements")),
  list("les deux tables hors data.frame", list(NULL, 1, "t"), "empilement_table_invalide",
       function(r) identical(r$details$table, c("inventaires", "evenements"))),
  list("colonne_type de longueur 2", list(INV, EVT, c("a", "b")), "empilement_colonne_type_invalide",
       function(r) identical(r$details$colonne_type, c("a", "b"))),
  list("colonne_type NA", list(INV, EVT, NA_character_), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type vide", list(INV, EVT, ""), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type nommée", list(INV, EVT, c(a = "t")), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type numérique", list(INV, EVT, 1), "empilement_colonne_type_invalide", function(r) TRUE),
  list("colonne_type d'encodage bytes", list(INV, EVT, `Encoding<-`("t\xe9", "bytes")),
       "empilement_colonne_type_invalide", function(r) TRUE),
  list("nom de colonne en double dans les inventaires", list(setNames(INV, c("id", "di", "id", "n")), EVT, "t"),
       "empilement_noms_invalides", function(r) identical(r$details, list(table = "inventaires", colonne = "id"))),
  list("nom de colonne vide dans les événements", list(INV, setNames(EVT, c("id", "", "n", "s", "c")), "t"),
       "empilement_noms_invalides", function(r) identical(r$details, list(table = "evenements", colonne = ""))),
  list("nom de colonne NA dans les événements", list(INV, setNames(EVT, c("id", NA, "n", "s", "c")), "t"),
       "empilement_noms_invalides", function(r) identical(r$details, list(table = "evenements", colonne = NA_character_))),
  list("colonne_type déjà présente dans les événements", list(INV, EVT, "c"), "empilement_colonne_type_presente",
       function(r) identical(r$details, list(colonne = "c", table = "evenements"))),
  list("colonne_type déjà présente dans les deux tables", list(INV, EVT, "sx"), "empilement_colonne_type_presente",
       function(r) identical(r$details, list(colonne = "sx", table = c("inventaires", "evenements")))),
  list("colonne commune integer contre double", list(transform(INV, id = as.numeric(id)), EVT, "t"),
       "empilement_colonnes_discordantes", function(r) identical(r$details, list(colonne = "id"))),
  list("colonne commune Date contre character, et facteur contre character", list(transform(INV, sx = factor(sx)),
       transform(EVT, n = as.character(n)), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = c("sx", "n")))),
  list("colonne commune Date à stockage double contre integer",
       list(INV, within(EVT, n <- structure(as.integer(unclass(n)), class = "Date")), "t"),
       "empilement_colonnes_discordantes", function(r) identical(r$details, list(colonne = "n")) &&
         grepl("stockage double", conditionMessage(r), fixed = TRUE)),
  list("facteur commun aux niveaux différents", list(transform(INV, sx = factor(sx, levels = c("F", "H"))),
       transform(EVT, sx = factor(sx, levels = c("H", "F"))), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "sx")) && grepl("attribut(s) diff", conditionMessage(r), fixed = TRUE) &&
         grepl("levels", conditionMessage(r), fixed = TRUE)),
  list("POSIXct commun de fuseaux différents", list(within(INV, h <- as.POSIXct("2015-01-01 12:00", tz = "UTC") + 0:2),
       within(EVT, h <- as.POSIXct("2015-01-01 12:00", tz = "Europe/Paris") + 0:1), "t"),
       "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "h")) && grepl("tzone", conditionMessage(r), fixed = TRUE)),
  list("difftime commun d'unités différentes", list(within(INV, u <- as.difftime(1:3, units = "days")),
       within(EVT, u <- as.difftime(1:2, units = "hours")), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "u")) && grepl("units", conditionMessage(r), fixed = TRUE)),
  list("facteur ordonné contre facteur simple, mêmes niveaux", list(transform(INV, sx = factor(sx, levels = c("F", "H"),
       ordered = TRUE)), transform(EVT, sx = factor(sx, levels = c("F", "H"))), "t"), "empilement_colonnes_discordantes",
       function(r) identical(r$details, list(colonne = "sx")) && grepl("class", conditionMessage(r), fixed = TRUE)),
  list("colonne matrice dans les inventaires", list(within(INV, m <- matrix(1:6, 3L)), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "m"))),
  list("colonne liste dans les inventaires", list(within(INV, l <- I(list(1, 2, 3))), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "l"))),
  list("colonne data.frame dans les inventaires", list(local({ x <- INV; x$d <- data.frame(a = 1:3); x }), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "d"))),
  list("colonne raw dans les inventaires", list(within(INV, r <- as.raw(1:3)), EVT, "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "inventaires", colonne = "r"))),
  list("colonne matrice dans les événements", list(INV, within(EVT, m <- matrix(1:4, 2L)), "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "evenements", colonne = "m"))),
  list("colonne liste dans les événements", list(INV, within(EVT, l <- I(list(1, 2))), "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "evenements", colonne = "l"))),
  list("colonne data.frame dans les événements", list(INV, local({ x <- EVT; x$d <- data.frame(a = 1:2); x }), "t"),
       "empilement_colonne_non_atomique", function(r) identical(r$details, list(table = "evenements", colonne = "d"))),
  list("colonnes raw et matrice dans les événements", list(INV, within(EVT, { r <- as.raw(1:2); m <- matrix(1:4, 2L) }), "t"),
       "empilement_colonne_non_atomique",
       function(r) identical(r$details$table, "evenements") && setequal(r$details$colonne, c("r", "m"))))
for (cas in CAS_REFUS)
  verifier(sprintf("refus (valeur écrite à la main) : %s -> donnees/%s", cas[[1L]], cas[[3L]]), {
    r <- rf(do.call(empiler_stocks, cas[[2L]]))
    k <- controle(r, "donnees", cas[[3L]])
    if (!isTRUE(k)) k else if (!isTRUE(cas[[4L]](r))) "détails ou message inattendus" else TRUE
  })
verifier("structure (valeurs écrites à la main) : mêmes attributs conservés admis (POSIXct de même fuseau, difftime de même unité, facteur ordonné aux mêmes niveaux) ; classe, fuseau, unité et niveaux gardés", {
  h <- as.POSIXct("2015-01-01 12:00", tz = "Europe/Paris") + 0:4
  inv <- within(INV, { h <- h[1:3]; u <- as.difftime(1:3, units = "hours"); o <- factor(sx, levels = c("F", "H"), ordered = TRUE) })
  evt <- within(EVT, { h <- h[4:5]; u <- as.difftime(4:5, units = "hours"); o <- factor(sx, levels = c("F", "H"), ordered = TRUE) })
  x <- empiler_stocks(inv, evt, "t")
  identical(x$h, h) && identical(x$u, as.difftime(1:5, units = "hours")) &&
    identical(x$o, factor(c("H", "F", "H", "F", "H"), levels = c("F", "H"), ordered = TRUE))
})
verifier("refus (valeur écrite à la main) : ordre table, colonne_type, noms, colonne non atomique, colonne_type présente, discordance (premier défaut seul)", {
  identical(rf(empiler_stocks(1, EVT, 1))$motif, "empilement_table_invalide") &&
    identical(rf(empiler_stocks(setNames(INV, c("a", "a", "b", "c")), EVT, 1))$motif, "empilement_colonne_type_invalide") &&
    identical(rf(empiler_stocks(setNames(INV, c("a", "a", "sx", "c")), EVT, "sx"))$motif, "empilement_noms_invalides") &&
    identical(rf(empiler_stocks(within(INV, m <- matrix(1:6, 3L)), setNames(EVT, c("a", "a", "b", "c", "d")), "sx"))$motif,
              "empilement_noms_invalides") &&
    identical(rf(empiler_stocks(within(INV, m <- matrix(1:6, 3L)), EVT, "sx"))$motif, "empilement_colonne_non_atomique") &&
    identical(rf(empiler_stocks(transform(INV, id = as.numeric(id)), EVT, "sx"))$motif, "empilement_colonne_type_presente")
})

# --- forme consommée par la recette `stocks_successifs` (identité exacte) ------------

PER_E <- list(debut = D("2015-01-01"), fin = D("2017-12-31"))
G_E <- D(c("2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31"))
LOI_E <- local({
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
})
PF_E <- list(mode = "uniforme", n = 2000, proportion_hommes = 0.5, age_min = 30, age_max = 80, origine = D("2010-01-01"))
SIM_E <- simuler_portefeuille(LOI_E, PF_E, PER_E, 186L, forme = list(nom = "stocks_successifs", inventaires = G_E))
attr(SIM_E, "simulation") <- NULL
EST_INV <- SIM_E$type_ligne == "inventaire"
INV_E <- rn(SIM_E[EST_INV, c("id_contrat", "id_tete", "date_inventaire", "sexe", "date_naissance")])
EVT_E <- rn(SIM_E[!EST_INV, c("id_contrat", "id_tete", "sexe", "date_naissance", "date_sortie", "cause_sortie")])
EMP_E <- empiler_stocks(INV_E, EVT_E, "type_ligne")
RS_E <- c(id_tete = "id_tete", type_ligne = "type_ligne", date_inventaire = "date_inventaire", sexe = "sexe",
          date_naissance = "date_naissance", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
PLAN_E <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PER_E, delai_declaration = 0,
               inventaires = G_E, segmentation = "sexe",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE,
                                               entree_entre_inventaires = "milieu"),
                             brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95)))
verifier("forme (identité exacte) : la forme stocks du simulateur, scindée par type_ligne dans le test puis empilée, est identical() à ses lignes rangées inventaires puis événements, colonnes dans l'ordre de l'empilement", {
  o <- c(which(EST_INV), which(!EST_INV))
  attendu <- rn(SIM_E[o, c("id_contrat", "id_tete", "date_inventaire", "sexe", "date_naissance", "date_sortie",
                           "cause_sortie", "type_ligne")])
  sum(EST_INV) > 0L && sum(!EST_INV) > 0L && identical(EMP_E, attendu)
})
X_SIM <- construire_table(SIM_E, RS_E, PLAN_E)
X_EMP <- construire_table(EMP_E, RS_E, PLAN_E)
verifier("forme (identité exacte) : construire_table() sur la table empilée, recette stocks_successifs, tables d'exposition et de brut et table finale identical() à celles de la forme du simulateur ; seules les empreintes des données et du résultat diffèrent dans la fiche", {
  fs <- X_SIM$fiche; fe <- X_EMP$fiche
  ch <- setdiff(names(fs), "empreintes")
  identical(fe$recette, "stocks_successifs") &&
    identical(X_EMP$etapes$exposition$resultat, X_SIM$etapes$exposition$resultat) &&
    identical(X_EMP$etapes$brut$resultat, X_SIM$etapes$brut$resultat) && identical(X_EMP$table, X_SIM$table) &&
    identical(fe[ch], fs[ch]) &&
    identical(fe$empreintes[setdiff(names(fe$empreintes), c("donnees", "resultat"))],
              fs$empreintes[setdiff(names(fs$empreintes), c("donnees", "resultat"))]) &&
    !identical(fe$empreintes$donnees, fs$empreintes$donnees)
})

# --- provenance de k fichiers (G9 ; M332) ------------------------------------------

cp <- tablexp:::controler_provenance
PROV1 <- list(fichier = "stocks.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
PROV2 <- list(fichier = c("inventaires.csv", "evenements.csv"),
              empreinte = c("00112233445566778899aabbccddeeff", "ffeeddccbbaa99887766554433221100"),
              algorithme = c("md5", "md5"))
verifier("provenance (valeur écrite à la main) : k = 1, k = 2 et k = 3 admises, noms dans un ordre quelconque ; NULL admis",
         isTRUE(cp(PROV1)) && isTRUE(cp(PROV2)) && isTRUE(cp(PROV2[c("algorithme", "fichier", "empreinte")])) &&
           isTRUE(cp(lapply(PROV2, function(v) c(v, v[1L])))) && isTRUE(cp(NULL)))
# Clés en ASCII (une clé accentuée émet un avertissement sous LC_ALL=C, comme
# dans tests/unitaires/test_construire_table.R).
PROV_K_REFUSEES <- list(
  "longueurs inegales (2, 2, 1)" = modifyList(PROV2, list(algorithme = "md5")),
  "longueurs inegales (1, 2, 2)" = modifyList(PROV2, list(fichier = "a.csv")),
  "vecteurs de longueur 0" = lapply(PROV2, function(v) character(0)),
  "NA dans un vecteur" = modifyList(PROV2, list(empreinte = c("0123", NA))),
  "chaine vide dans un vecteur" = modifyList(PROV2, list(fichier = c("a.csv", ""))),
  "vecteur nomme" = modifyList(PROV2, list(fichier = c(inventaires = "a.csv", evenements = "b.csv"))),
  "vecteur a attribut dim" = modifyList(PROV2, list(algorithme = matrix(c("md5", "md5")))),
  "liste a la place d'un vecteur" = modifyList(PROV2, list(fichier = list("a.csv", "b.csv"))),
  "facteur" = modifyList(PROV2, list(algorithme = factor(c("md5", "md5")))))
for (k in names(PROV_K_REFUSEES))
  verifier(sprintf("provenance (valeur écrite à la main) : %s -> rapport/provenance_invalide", k),
           controle(rf(cp(PROV_K_REFUSEES[[k]])), "rapport", "provenance_invalide"))

X_P1 <- construire_table(EMP_E, RS_E, PLAN_E, PROV1)
X_P2 <- construire_table(EMP_E, RS_E, PLAN_E, PROV2)
verifier("provenance k = 2 (valeur écrite à la main) : fiche$provenance garde ses trois noms et ses vecteurs dans l'ordre reçu ; empreintes$fichier et algorithme_fichier de longueur 2 ; mention du nombre de fichiers", {
  f <- X_P2$fiche
  identical(f$provenance, PROV2) && identical(f$empreintes$fichier, PROV2$empreinte) &&
    identical(f$empreintes$algorithme_fichier, PROV2$algorithme) &&
    identical(f$provenance_mention, "2 fichiers lus par la couche de lecture, un élément par fichier dans fiche/provenance, dans l'ordre de la lecture") &&
    identical(X_P1$fiche$provenance_mention, "fichier lu par la couche de lecture") &&
    identical(X_P1$fiche$empreintes$fichier, PROV1$empreinte)
})
verifier("provenance k = 2 (identité exacte) : objet identical() à celui de k = 1 hors execution, fiche/provenance, fiche/provenance_mention, empreintes de fichier et du résultat", {
  a <- sans_execution(X_P1); b <- sans_execution(X_P2)
  for (o in c("a", "b")) {
    v <- get(o)
    v$fiche[c("provenance", "provenance_mention")] <- NULL
    v$fiche$empreintes[c("fichier", "algorithme_fichier", "resultat")] <- NULL
    assign(o, v)
  }
  identical(a, b) && !identical(X_P1$fiche$empreintes$resultat, X_P2$fiche$empreintes$resultat)
})
verifier("provenance k = 2 (identité exacte) : fiche écrite puis relue identical() à la fiche ; construire_table() sur ses rôles, plan et provenance identical() hors execution", {
  lue <- lire_fiche(ecrire_fiche(X_P2$fiche))
  identical(lue, X_P2$fiche) &&
    identical(sans_execution(construire_table(EMP_E, lue$roles, lue$plan, lue$provenance)), sans_execution(X_P2))
})
verifier("provenance k = 2 (identité exacte) : rejouer_fiche() sur la fiche relue rend la construction identical() hors execution, bilan de 14 lignes sans écart ; ligne 6 et témoin 14 citent chacun des deux fichiers entre guillemets, dans l'ordre", {
  r <- rejouer_fiche(EMP_E, lire_fiche(ecrire_fiche(X_P2$fiche)))
  identical(sans_execution(r$construction), sans_execution(X_P2)) && identical(nrow(r$bilan), 14L) &&
    identical(r$bilan$verdict, c(rep("conforme", 8L), rep("identique", 5L), "non vérifiée")) &&
    endsWith(r$bilan$lu[6L], "; provenance : « inventaires.csv », « evenements.csv »") &&
    identical(r$bilan$lu[14L], paste0("« ", PROV2$empreinte, " »", collapse = ", "))
})
verifier("provenance k = 1 (valeur écrite à la main) : rejeu, ligne 6 et témoin 14 au texte d'un seul fichier", {
  r <- rejouer_fiche(EMP_E, lire_fiche(ecrire_fiche(X_P1$fiche)))
  endsWith(r$bilan$lu[6L], "; provenance : stocks.csv") && identical(r$bilan$lu[14L], PROV1$empreinte)
})
verifier("provenance k = 2 (valeur écrite à la main) : rapport figé rendu ; les deux fichiers, les deux empreintes et la mention y figurent, chemins de longueur 2", {
  h <- rawToChar(ecrire_rapport(X_P2)); Encoding(h) <- "UTF-8"
  all(vapply(c(PROV2$fichier, PROV2$empreinte, "2 fichiers lus par la couche de lecture"),
             function(s) grepl(s, h, fixed = TRUE), logical(1L))) &&
    grepl("<tr><td>fiche/provenance/fichier</td><td>character</td><td>2</td><td>&quot;inventaires.csv&quot;<br>&quot;evenements.csv&quot;</td></tr>",
          h, fixed = TRUE) &&
    grepl("<tr><td>fiche/empreintes/fichier</td><td>character</td><td>2</td>", h, fixed = TRUE)
})

# --- identité de la fiche à provenance d'un fichier (m4 de l'audit de #181) ---------

# Octets de la fiche écrite d'une construction à provenance k = 1 (étapes
# donnees et exposition seules : aucun calcul dépendant de la bibliothèque
# mathématique), lignes versions/, plateforme/, empreintes/code,
# empreintes/resultat et empreintes/statut_resultat_code retirées (propres au
# poste et au code installé ; l'empreinte du résultat n'est qu'un témoin
# d'identité sur la même machine, E3.6, E4.6 : audit de la reprise de #181,
# n1). MD5 écrit à la main, relevé sur f00d701 (avant #181) et sur l'arbre de
# #181, sous C et C.UTF-8 : égaux (015a20128fdfbd3aea2169926a73518b). Règle
# de mise à jour : au commit qui change la grammaire ou les champs de la
# fiche, avec tableau avant / après.
verifier("provenance k = 1 (valeur écrite à la main) : MD5 de la fiche écrite hors versions, plateforme, empreintes/code, resultat et statut_resultat_code inchangé depuis f00d701", {
  base <- data.frame(contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
                     naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
                     entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
                     sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
                     cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
  roles <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
             date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
  plan <- list(transition = list(depart = "vivant", arrivee = "deces"),
               periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0, segmentation = "sexe",
               etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE)))
  prov <- list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
  l <- strsplit(rawToChar(ecrire_fiche(construire_table(base, roles, plan, prov)$fiche)), "\n", fixed = TRUE)[[1L]]
  l <- l[!grepl("^(versions|plateforme|empreintes/(code|resultat|statut_resultat_code))(/|\t)", l)]
  f <- tempfile(); on.exit(unlink(f))
  writeBin(charToRaw(paste0(paste(l, collapse = "\n"), "\n")), f)
  identical(unname(tools::md5sum(f)), "015a20128fdfbd3aea2169926a73518b")
})
