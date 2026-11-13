# -----------------------------------------------------------------------------
#  tests/unitaires/test_tables_embarquees.R : catalogue des tables embarquées,
#  table construite à l'appel, nom réservé, tables_embarquees() (issue #263 ;
#  ADR 0018, points 1, 7, 8 et 11 ; M447 à M449)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : motifs, modules, lignes et fichiers cités,
#    details des refus, colonnes et états du catalogue, q_x d'un contenu
#    injecté ((l_x - l_{x+1}) / l_x écrit en clair) ;
#  - identité exacte : objet construit depuis un contenu injecté identical()
#    à celui de table_reference() appelé à la main ; tables_embarquees()
#    identical() au catalogue construit sur TABLES_EMBARQUEES ;
#  - littéral de la source : nombre de lignes, âges extrêmes, l_0 et l_1 de
#    chaque table déclarée, lus sur le fac-similé de sa source (fichier de
#    provenance) ; une ligne de TABLES_ATTENDUES par commit `donnees:`.
#  Les contenus sont injectés (seam M247) : le cas d'une table refusée (TF
#  00-02, E2.26) est couvert ici par un contenu aux l_x non décroissants,
#  quelles que soient les tables déclarées. Le format des fichiers versés,
#  leurs empreintes et leur contre-vérification sont contrôlés par
#  tests/controles/tables_embarquees.R (étape de CI, ADR 0018, point 6).
# -----------------------------------------------------------------------------

DECLARATIONS <- tablexp:::DECLARATIONS_TABLES_EMBARQUEES
TABLES_EMBARQUEES <- tablexp:::TABLES_EMBARQUEES
table_du_catalogue <- tablexp:::table_du_catalogue
catalogue_tables_embarquees <- tablexp:::catalogue_tables_embarquees
controler_nom_reserve <- tablexp:::controler_nom_reserve
lire_tables_embarquees <- tablexp:::lire_tables_embarquees
empreinte_table <- tablexp:::empreinte_table
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
COLONNES <- c("nom", "sexe", "forme_origine", "source", "etat", "motif_refus", "representation", "empreinte")

# Refus attendu : module, motif, extraits du message (fixed = TRUE) et
# champs de details donnés. Rend TRUE ou la liste des écarts.
controle_refus <- function(r, module, motif, contient = character(0), details = list()) {
  if (is.null(r)) return("aucun refus levé")
  e <- character(0)
  if (!identical(r$module, module)) e <- c(e, sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) e <- c(e, sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  for (s in contient) if (!grepl(s, conditionMessage(r), fixed = TRUE)) e <- c(e, sprintf("message sans « %s »", s))
  for (n in names(details)) if (!identical(r$details[[n]], details[[n]]))
    e <- c(e, sprintf("details$%s : %s", n, paste(deparse(r$details[[n]]), collapse = " ")))
  if (length(e)) e else TRUE
}

# Contenu injecté, forme de TABLES_EMBARQUEES (ADR 0018, point 1.2).
element <- function(lignes, identifiant = "essai", sexe = "ensemble") list(identifiant = identifiant, sexe = sexe, lignes = lignes)
lignes_lx <- function(age, lx) c("age,lx", sprintf("%d,%d", as.integer(age), as.integer(lx)))
CAT <- list(ADMISE = element(lignes_lx(0:3, c(100000, 99129, 99057, 99010)), "admise", "H"),
            REFUSEE = element(lignes_lx(9:13, c(99500, 99478, 99647, 99456, 99430)), "refusee", "F"))

# --- déclaration et lecture à l'installation (points 1.1 et 1.2) ---------------------

verifier("déclaration : data.frame nu nom, identifiant, sexe (character) ; noms et identifiants uniques ; identifiant [a-z0-9_-]+ ; sexe dans SEXES_TABLE (valeur calculée à la main)", {
  identical(names(DECLARATIONS), c("nom", "identifiant", "sexe")) && identical(class(DECLARATIONS), "data.frame") &&
    all(vapply(DECLARATIONS, is.character, NA)) && !anyDuplicated(DECLARATIONS$nom) &&
    !anyDuplicated(DECLARATIONS$identifiant) && all(grepl("^[a-z0-9_-]+$", DECLARATIONS$identifiant)) &&
    all(DECLARATIONS$sexe %in% tablexp:::SEXES_TABLE)
})
verifier("TABLES_EMBARQUEES : une entrée par table déclarée, dans l'ordre ; identifiant et sexe de la déclaration ; lignes lues (non NULL) à l'installation", {
  identical(names(TABLES_EMBARQUEES), DECLARATIONS$nom) &&
    identical(unname(vapply(TABLES_EMBARQUEES, function(e) e$identifiant, "")), DECLARATIONS$identifiant) &&
    identical(unname(vapply(TABLES_EMBARQUEES, function(e) e$sexe, "")), DECLARATIONS$sexe) &&
    all(vapply(TABLES_EMBARQUEES, function(e) is.character(e$lignes), NA))
})
verifier("lire_tables_embarquees() : déclaration vide -> liste nommée vide identical(structure(list(), names = character(0)))",
         identical(lire_tables_embarquees(tempdir(), DECLARATIONS[0L, ]), structure(list(), names = character(0))))
verifier("lire_tables_embarquees() : lignes du fichier telles quelles (sans analyse, ligne mal formée comprise) ; fichier absent -> NULL ; sans DESCRIPTION de tablexp -> NULL", {
  r <- tempfile("racine_"); dir.create(file.path(r, "inst", "extdata", "tables"), recursive = TRUE)
  on.exit(unlink(r, recursive = TRUE))
  writeLines(c("age,lx", "0,10", "1;x"), file.path(r, "inst", "extdata", "tables", "a.csv"))
  d <- data.frame(nom = c("A", "B"), identifiant = c("a", "b"), sexe = c("H", "F"), stringsAsFactors = FALSE)
  sans <- lire_tables_embarquees(r, d)
  writeLines("Package: autre", file.path(r, "DESCRIPTION"))
  autre <- lire_tables_embarquees(r, d)
  writeLines("Package: tablexp", file.path(r, "DESCRIPTION"))
  avec <- lire_tables_embarquees(r, d)
  identical(avec, list(A = list(identifiant = "a", sexe = "H", lignes = c("age,lx", "0,10", "1;x")),
                       B = list(identifiant = "b", sexe = "F", lignes = NULL))) &&
    is.null(sans$A$lignes) && is.null(autre$A$lignes)
})

# --- construction à l'appel (point 1.3) ------------------------------------------------

verifier("table_du_catalogue() : contenu admis -> objet identical() à table_reference(data.frame(age, sexe, lx), nom, « embarquee », source, forme_origine = « lx ») ; source = inst/extdata/tables/<identifiant>.provenance.dcf", {
  a <- table_du_catalogue("ADMISE", CAT)
  m <- table_reference(data.frame(age = 0:3, sexe = "H", lx = c(100000, 99129, 99057, 99010), stringsAsFactors = FALSE),
                       "ADMISE", "embarquee", "inst/extdata/tables/admise.provenance.dcf", forme_origine = "lx")
  identical(a, m) && identical(a$age, 0:2) && identical(a$qx[1L], (100000 - 99129) / 100000)
})
verifier("table_du_catalogue() : nom absent -> table_embarquee_absente (module donnees), catalogue cité", {
  controle_refus(refus_de(table_du_catalogue("AUTRE", CAT)), "donnees", "table_embarquee_absente",
                 "(tables embarquées : « ADMISE », « REFUSEE »)", list(lu = "AUTRE", attendu = c("ADMISE", "REFUSEE")))
})
CAS_ILLISIBLES <- list(
  list("contenu NULL (fichier non lu)", NULL, NA_integer_, "fichier inst/extdata/tables/essai.csv non lu à l'installation"),
  list("contenu vide", character(0), 1L, "ligne 1, fichier vide, sans en-tête « age,lx »"),
  list("en-tête faux (« age;lx »)", c("age;lx", "0;10"), 1L, "ligne 1, en-tête « age;lx » au lieu de « age,lx »"),
  list("en-tête « age,qx »", c("age,qx", "0,1"), 1L, "en-tête « age,qx »"),
  list("en-tête seul", "age,lx", 2L, "ligne 2, aucune ligne de valeurs après l'en-tête"),
  list("séparateur de milliers", c("age,lx", "0,100 000", "1,99129"), 2L, "ligne 2, ligne « 0,100 000 » mal formée"),
  list("valeur décimale", c("age,lx", "0,10", "1,9.5"), 3L, "ligne 3, ligne « 1,9.5 » mal formée"),
  list("zéro de tête", c("age,lx", "0,10", "01,9"), 3L, "ligne « 01,9 » mal formée"),
  list("cellule vide (ligne imprimée sans valeur transcrite)", c("age,lx", "0,10", "1,"), 3L, "ligne « 1, » mal formée"),
  list("espace", c("age,lx", "0, 10"), 2L, "ligne « 0, 10 » mal formée"),
  list("seize chiffres", c("age,lx", "0,1000000000000000"), 2L, "mal formée"),
  list("retour chariot", c("age,lx", "0,10\r"), 2L, "ligne « 0,10<U+000D> » mal formée"),
  list("âge manquant (0, 2)", c("age,lx", "0,10", "2,5"), 3L, "ligne 3, âge 2 hors de la suite croissante et consécutive commencée à l'âge 0"),
  list("âges décroissants (1, 0)", c("age,lx", "1,10", "0,5"), 3L, "âge 0 hors de la suite"),
  list("âge en double", c("age,lx", "0,10", "1,9", "1,8"), 4L, "ligne 4, âge 1 hors de la suite"),
  list("âge au-delà des entiers R", c("age,lx", "2147483647,10", "2147483648,5"), 3L,
       "ligne 3, âge « 2147483648 » supérieur au plus grand entier du moteur (2147483647)"),
  list("âge au-delà des entiers R, hors de la suite", c("age,lx", "0,10", "2147483648,5"), 3L,
       "ligne 3, âge « 2147483648 » supérieur au plus grand entier du moteur (2147483647)"),
  list("âge au plus le plus grand entier, hors de la suite", c("age,lx", "0,10", "2147483647,5"), 3L,
       "ligne 3, âge 2147483647 hors de la suite croissante et consécutive commencée à l'âge 0"))
for (cas in CAS_ILLISIBLES)
  verifier(sprintf("table_du_catalogue() : %s -> table_embarquee_illisible (module donnees), fichier et ligne %s cités (valeur calculée à la main)",
                   cas[[1L]], cas[[3L]]),
           controle_refus(refus_de(table_du_catalogue("X", list(X = element(cas[[2L]])))), "donnees",
                          "table_embarquee_illisible", c("Table embarquée « X » : fichier", cas[[4L]]),
                          list(nom = "X", fichier = "inst/extdata/tables/essai.csv", ligne = cas[[3L]])))
verifier("table_du_catalogue() : contenu vide -> raison « fichier vide », ligne 1, lu NULL (valeur calculée à la main)",
         controle_refus(refus_de(table_du_catalogue("X", list(X = element(character(0))))), "donnees",
                        "table_embarquee_illisible", "fichier vide, sans en-tête « age,lx »",
                        list(ligne = 1L, lu = NULL)))
verifier("table_du_catalogue() : âge 2147483648 (au-delà de .Machine$integer.max) -> raison propre, avant celle de la suite ; ligne lue en details (valeur calculée à la main)", {
  r <- refus_de(table_du_catalogue("X", list(X = element(c("age,lx", "2147483646,10", "2147483647,9", "2147483648,5")))))
  a <- controle_refus(r, "donnees", "table_embarquee_illisible",
                      "ligne 4, âge « 2147483648 » supérieur au plus grand entier du moteur (2147483647)",
                      list(ligne = 4L, lu = "2147483648,5"))
  if (isTRUE(a) && !grepl("hors de la suite", conditionMessage(r), fixed = TRUE)) TRUE else c(a, "raison de la suite citée")
})
verifier("table_du_catalogue() : ligne mal formée non ASCII citée neutralisée (octets invalides écrits <xx>)", {
  l <- c("age,lx", "0,10", "1,\xe9"); Encoding(l) <- "unknown"
  controle_refus(refus_de(table_du_catalogue("X", list(X = element(l)))), "donnees", "table_embarquee_illisible",
                 "ligne « 1,<e9> » mal formée", list(ligne = 3L))
})
verifier("table_du_catalogue() : l_x non décroissants (paire (10, 11) d'un contenu de type TF 00-02) -> refus table_lx_non_decroissants de table_reference(), sans enveloppe ; paires en details (ADR 0018, point 7.2)", {
  r <- refus_de(table_du_catalogue("REFUSEE", CAT))
  controle_refus(r, "donnees", "table_lx_non_decroissants",
                       c("Table « REFUSEE » : l_x non décroissants", "pour 1 paire(s)", "F, âges 10 et 11 (l_x = 99478 puis 99647)"),
                       list(sexes = "F", ages = 10L, lx = 99478, lx_suivants = 99647))
})
verifier("table_du_catalogue() : trois paires (10-11, 23-24, 61-62), toutes citées et en details$ages", {
  age <- 0:70; lx <- 100000 - 100 * age
  lx[age == 11] <- lx[age == 10] + 1; lx[age == 24] <- lx[age == 23] + 1; lx[age == 62] <- lx[age == 61] + 1
  r <- refus_de(table_du_catalogue("X", list(X = element(lignes_lx(age, lx)))))
  controle_refus(r, "donnees", "table_lx_non_decroissants", c("pour 3 paire(s)", "âges 10 et 11", "âges 23 et 24", "âges 61 et 62"),
                 list(ages = c(10L, 23L, 61L)))
})
verifier("table_embarquee() : signature (nom) ; nom absent du catalogue installé -> table_embarquee_absente",
         identical(names(formals(table_embarquee)), "nom") &&
           identical(refus_de(table_embarquee("TABLE INCONNUE"))$motif, "table_embarquee_absente"))

# --- nom réservé (point 7.4 ; M447, option (a)) -----------------------------------------

for (statut in c("importee", "synthetique"))
  verifier(sprintf("controler_nom_reserve() : statut « %s », nom réservé à l'identique -> table_nom_reserve (module donnees), nom, statut et noms réservés en details", statut),
           controle_refus(refus_de(controler_nom_reserve("TF 00-02", statut, c("TD 88-90", "TF 00-02"))), "donnees",
                          "table_nom_reserve",
                          c(sprintf("Table « TF 00-02 » de statut « %s » : ce nom est réservé à une table embarquée dans le package", statut),
                            "(tables embarquées : « TD 88-90 », « TF 00-02 »), lue par table_embarquee() et dont tables_embarquees() donne l'état ;",
                            "une table corrigée ou modifiée est une autre table, qui porte un autre nom et ne se déclare pas homologuée."),
                          list(nom = "TF 00-02", statut = statut, lu = "TF 00-02", attendu = c("TD 88-90", "TF 00-02"))))
verifier("controler_nom_reserve() : statut embarquee admis ; casse, espaces ou tiret différents admis (correspondance exacte, sans normalisation) ; aucun nom réservé admis", {
  r <- c("TD 88-90", "TF 00-02")
  is.null(controler_nom_reserve("TF 00-02", "embarquee", r)) && is.null(controler_nom_reserve("tf 00-02", "importee", r)) &&
    is.null(controler_nom_reserve("TF 00-02 ", "importee", r)) && is.null(controler_nom_reserve("TF 00 02", "importee", r)) &&
    is.null(controler_nom_reserve("TF 00-02", "importee", character(0)))
})
verifier("table_reference() : chaque nom déclaré, de statut importee ou synthetique, refusé (table_nom_reserve) ; de statut embarquee, admis", {
  x <- data.frame(age = 0:1, sexe = "H", qx = c(0.01, 0.02))
  all(vapply(DECLARATIONS$nom, function(n)
    identical(refus_de(table_reference(x, n, "importee", "s"))$motif, "table_nom_reserve") &&
      identical(refus_de(table_reference(x, n, "synthetique", "s", parametres = list(a = 1)))$motif, "table_nom_reserve") &&
      inherits(table_reference(x, n, "embarquee", "s"), "table_reference"), NA))
})
verifier("table_reference() : le refus table_nom_reserve suit table_statut_inconnu et précède table_source_manquante", {
  x <- data.frame(age = 0:1, sexe = "H", qx = c(0.01, 0.02))
  n <- if (nrow(DECLARATIONS)) DECLARATIONS$nom[1L] else NULL
  identical(refus_de(table_reference(x, "a", "autre", NA))$motif, "table_statut_inconnu") &&
    (is.null(n) || identical(refus_de(table_reference(x, n, "importee", NA))$motif, "table_nom_reserve"))
})

# --- tables_embarquees() (point 8) --------------------------------------------------

verifier("tables_embarquees() : sans argument, exportée ; identical() au catalogue construit sur TABLES_EMBARQUEES ; huit colonnes character ; une ligne par table déclarée, dans l'ordre", {
  t <- tables_embarquees()
  identical(names(formals(tables_embarquees)), NULL) && "tables_embarquees" %in% getNamespaceExports("tablexp") &&
    identical(t, catalogue_tables_embarquees(TABLES_EMBARQUEES)) && identical(names(t), COLONNES) &&
    identical(class(t), "data.frame") && all(vapply(t, is.character, NA)) && identical(t$nom, DECLARATIONS$nom) &&
    identical(t$sexe, DECLARATIONS$sexe)
})
verifier("catalogue_tables_embarquees() : catalogue vide -> data.frame de 0 ligne aux huit colonnes character", {
  t <- catalogue_tables_embarquees(structure(list(), names = character(0)))
  identical(names(t), COLONNES) && nrow(t) == 0L && all(vapply(t, is.character, NA))
})
verifier("catalogue_tables_embarquees() : table admise (empreinte de table, motif NA) et table refusée (état refusee, motif table_lx_non_decroissants, empreinte NA), dans l'ordre du catalogue (valeur calculée à la main)", {
  t <- catalogue_tables_embarquees(CAT)
  identical(t, data.frame(nom = c("ADMISE", "REFUSEE"), sexe = c("H", "F"), forme_origine = "lx",
                          source = c("inst/extdata/tables/admise.provenance.dcf", "inst/extdata/tables/refusee.provenance.dcf"),
                          etat = c("admise", "refusee"), motif_refus = c(NA, "table_lx_non_decroissants"),
                          representation = "tablexp table 1",
                          empreinte = c(empreinte_table(table_du_catalogue("ADMISE", CAT)), NA), stringsAsFactors = FALSE))
})
verifier("catalogue_tables_embarquees() : contenu illisible -> le refus table_embarquee_illisible remonte (pas un état, point 8.3)", {
  r <- refus_de(catalogue_tables_embarquees(c(CAT, list(X = element(c("age,lx", "0,1 0"))))))
  controle_refus(r, "donnees", "table_embarquee_illisible", "ligne « 0,1 0 » mal formée")
})

# --- contrôle 7 bis du rejeu par le chemin unique (point 1.4) ----------------------

verifier("controler_tables_rejeu() : table embarquée dont le contenu injecté est refusé -> refus table_lx_non_decroissants tel quel ; contenu changé d'un l_x -> empreinte_table_discordante", {
  el <- tablexp:::element_table_fiche(table_du_catalogue("ADMISE", CAT))
  r1 <- refus_de(tablexp:::controler_tables_rejeu(list(t1 = el), NULL, "code",
                                                  list(ADMISE = CAT$REFUSEE)))
  cat2 <- CAT; cat2$ADMISE$lignes[3L] <- "1,99130"
  r2 <- refus_de(tablexp:::controler_tables_rejeu(list(t1 = el), NULL, "code", cat2))
  a <- controle_refus(r1, "donnees", "table_lx_non_decroissants", "Table « ADMISE »")
  b <- controle_refus(r2, "construire_table", "empreinte_table_discordante",
                      details = list(lu = el$empreinte, attendu = empreinte_table(table_du_catalogue("ADMISE", cat2))))
  if (isTRUE(a) && isTRUE(b)) TRUE else c(a, b)
})

# --- tables déclarées (littéral de la source ; une ligne par commit donnees:) ------

# nom, nombre de lignes de l'objet (âges transcrits moins le dernier, #146,
# § 1.8), premier et dernier âge de l'objet, l_0 et l_1 imprimés.
TABLES_ATTENDUES <- data.frame(
  nom = c("TD 88-90", "TV 88-90"),
  lignes = c(106L, 110L),
  age_min = c(0L, 0L),
  age_max = c(105L, 109L),
  l0 = c(100000, 100000),
  l1 = c(99129, 99352),
  stringsAsFactors = FALSE)
verifier("tables attendues : une ligne par table déclarée, dans l'ordre de la déclaration",
         identical(TABLES_ATTENDUES$nom, DECLARATIONS$nom))
for (k in seq_len(nrow(TABLES_ATTENDUES))) local({
  a <- TABLES_ATTENDUES[k, ]
  verifier(sprintf("table_embarquee(« %s ») : %d lignes, âges %d à %d, sexe déclaré, statut embarquee, forme lx, q_0 = (l_0 - l_1) / l_0 (littéral de la source)",
                   a$nom, a$lignes, a$age_min, a$age_max), {
    t <- table_embarquee(a$nom)
    d <- DECLARATIONS[DECLARATIONS$nom == a$nom, ]
    inherits(t, "table_reference") && nrow(t) == a$lignes && identical(t$age, a$age_min:a$age_max) &&
      all(t$sexe == d$sexe) && identical(attr(t, "statut"), "embarquee") && identical(attr(t, "forme_origine"), "lx") &&
      identical(attr(t, "source"), sprintf("inst/extdata/tables/%s.provenance.dcf", d$identifiant)) &&
      identical(t$qx[1L], (a$l0 - a$l1) / a$l0)
  })
})
