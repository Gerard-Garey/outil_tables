# -----------------------------------------------------------------------------
#  tests/unitaires/test_decalages_age.R : décalages d'âge, objet
#  decalages_age, format 3, empreinte « tablexp decalages 1 » et
#  decalages_embarques() (issue #273 ; ADR 0019, points 6.1 à 6.5 ;
#  spécification d'actuary de #273, § 1 à 4 et § 6 : cas D1 à D5, F1 à F3,
#  P1 à P4, A1 à A4)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : motifs, modules, bandes citées, details,
#    colonnes, types et ordre du catalogue ;
#  - identité exacte : objets identical() sous un autre ordre ou un autre type
#    reçu, contenu au format 3 identical() au constructeur appelé à la main,
#    empreinte du catalogue égale à empreinte_decalages() ;
#  - littéral de la spécification : texte « tablexp decalages 1 » du § 3.1 à
#    l'octet près et son MD5 (6d87d1e7d9adbe8bf5a7248e525da865), écrits dans
#    la spécification d'actuary avant ce code.
#  Cas construits à la main, aucun tirage, valeurs d'essai seulement (aucune
#  valeur de décalage d'une table homologuée). Les contenus sont injectés
#  (seam M247) : le catalogue embarqué est vide tant qu'aucun commit
#  `donnees:` n'y a déclaré de décalages.
# -----------------------------------------------------------------------------

decalages_age <- tablexp:::decalages_age
analyser_decalages_format3 <- tablexp:::analyser_decalages_format3
catalogue_decalages_embarques <- tablexp:::catalogue_decalages_embarques
empreinte_decalages <- tablexp:::empreinte_decalages
lire_decalages_embarques <- tablexp:::lire_decalages_embarques
DECLARATIONS <- tablexp:::DECLARATIONS_DECALAGES_EMBARQUES
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
COLONNES <- c("nom", "table", "age_min", "age_max", "decalage", "source", "representation", "empreinte")

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

# Jeu D1 de la spécification (§ 6) : [10 ; 19] -2, [20 ; 29] -1, [30 ; +inf[ 0.
d1 <- function(o = 1:3, f = as.integer)
  data.frame(age_min = f(c(10, 20, 30))[o], age_max = f(c(19, 29, NA))[o], decalage = f(c(-2, -1, 0))[o])
dec <- function(x, nom = "Essai", table = "TABLE ESSAI", source = "essai.provenance.dcf") decalages_age(x, nom, table, source)
bandes <- function(age_min, age_max, decalage = rep(0L, length(age_min)))
  data.frame(age_min = as.integer(age_min), age_max = as.integer(age_max), decalage = as.integer(decalage))
D1 <- dec(d1())
EN_TETE <- "age_min,age_max,decalage"
LIGNES_D1 <- c(EN_TETE, "10,19,-2", "20,29,-1", "30,,0")
element <- function(lignes, identifiant = "essai", table = "TABLE ESSAI")
  list(identifiant = identifiant, table = table, lignes = lignes)

# --- D1, D2, D4 : objets admis (§ 1.1 et 1.2) ---------------------------------------

verifier("D1 : classe c(decalages_age, data.frame), colonnes age_min, age_max, decalage integer, NA sur la seule bande ouverte, attributs nom, table, source, noms de lignes automatiques (valeur calculée à la main)", {
  identical(class(D1), c("decalages_age", "data.frame")) && !inherits(D1, "table_reference") &&
    identical(names(D1), c("age_min", "age_max", "decalage")) &&
    all(vapply(D1, is.integer, NA)) && identical(D1$age_min, c(10L, 20L, 30L)) &&
    identical(D1$age_max, c(19L, 29L, NA)) && identical(D1$decalage, c(-2L, -1L, 0L)) &&
    identical(attr(D1, "nom"), "Essai") && identical(attr(D1, "table"), "TABLE ESSAI") &&
    identical(attr(D1, "source"), "essai.provenance.dcf") &&
    identical(attr(D1, "row.names"), 1:3) &&
    setequal(names(attributes(D1)), c("names", "class", "row.names", "nom", "table", "source"))
})
verifier("D1 : objet identical() sous l'ordre inverse et sous un ordre entrelacé (identité exacte)",
         identical(dec(d1(3:1)), D1) && identical(dec(d1(c(2L, 3L, 1L))), D1) && identical(dec(d1(c(3L, 1L, 2L))), D1))
verifier("D1 : entrée en doubles à valeur entière, et décalage -0, donnent le même objet ; -0 devient 0L (identité exacte)", {
  x <- d1(f = as.double); x$decalage[3L] <- -0
  o <- dec(x)
  identical(o, D1) && identical(o$decalage[3L], 0L) && identical(1 / as.double(o$decalage[3L]), Inf)
})
verifier("D1 : noms de lignes reçus non automatiques et data.frame sous-classé : objet identical() (forme seule corrigée ; identité exacte)", {
  x <- d1(); row.names(x) <- c("a", "b", "c")
  y <- d1(); class(y) <- c("autre", "data.frame")
  identical(dec(x), D1) && identical(dec(y), D1)
})
verifier("D2 : une seule bande, ouverte, au décalage positif, admise (valeur calculée à la main)", {
  o <- dec(bandes(5, NA, 3))
  identical(o$age_min, 5L) && identical(o$age_max, NA_integer_) && identical(o$decalage, 3L) && nrow(o) == 1L
})
verifier("D2 : décalages non monotones et de signes mêlés admis (aucune propriété des valeurs exigée, § 1.3)", {
  o <- dec(bandes(c(0, 10, 20), c(9, 19, NA), c(-4, 7, -1)))
  identical(o$decalage, c(-4L, 7L, -1L))
})
verifier("D4 : bornes extrêmes [0 ; .Machine$integer.max - 1] puis [.Machine$integer.max ; +inf[ admises, sans avertissement ni débordement (valeur calculée à la main)", {
  m <- .Machine$integer.max
  a <- NULL
  o <- withCallingHandlers(dec(data.frame(age_min = c(0L, m), age_max = c(m - 1L, NA), decalage = c(-m, m))),
                           warning = function(w) { a <<- c(a, conditionMessage(w)); invokeRestart("muffleWarning") })
  is.null(a) && identical(o$age_min, c(0L, m)) && identical(o$age_max, c(m - 1L, NA)) && identical(o$decalage, c(-m, m))
})
verifier("D4 : un double entier égal à .Machine$integer.max est admis (valeur absolue au plus la borne)",
         identical(dec(data.frame(age_min = c(0, 10), age_max = c(9, NA), decalage = c(-2147483647, 2147483647)))$decalage,
                   c(-2147483647L, 2147483647L)))

# --- D3 : refus, un par motif (§ 1.3), dans l'ordre ------------------------------------

for (a in c("nom", "table", "source")) {
  for (v in list(NULL, "", NA_character_, c("a", "b"), 1, c(x = "a"))) {
    arg <- list(x = d1(), nom = "Essai", table = "TABLE ESSAI", source = "s")
    arg[a] <- list(v)
    verifier(sprintf("D3 decalages_nom_manquant : %s = %s (valeur calculée à la main)", a, paste(deparse(v), collapse = "")),
             controle_refus(refus_de(do.call(decalages_age, arg)), "donnees", "decalages_nom_manquant",
                            sprintf("« %s »", a), list(argument = a)))
  }
  b <- "\xe9"; Encoding(b) <- "bytes"
  arg <- list(x = d1(), nom = "Essai", table = "TABLE ESSAI", source = "s")
  arg[[a]] <- b
  verifier(sprintf("D3 decalages_nom_manquant : %s d'encodage bytes (valeur calculée à la main)", a),
           controle_refus(refus_de(do.call(decalages_age, arg)), "donnees", "decalages_nom_manquant",
                          character(0), list(argument = a)))
}
verifier("D3 : nom, table et source contrôlés dans cet ordre, avant x (premier argument fautif cité ; valeur calculée à la main)",
         controle_refus(refus_de(decalages_age(list(), "Essai", "", NA_character_)), "donnees", "decalages_nom_manquant",
                        character(0), list(argument = "table")))
verifier("D3 decalages_mal_formes : liste, matrice, data.frame sans ligne, colonnes en double (valeur calculée à la main)", {
  doubles <- data.frame(age_min = 10L, age_max = NA_integer_, age_min = 1L, check.names = FALSE)
  r <- list(refus_de(dec(as.list(d1()))), refus_de(dec(as.matrix(d1()))), refus_de(dec(d1()[0L, ])),
            refus_de(dec(doubles)))
  e <- c(vapply(r[1:3], function(x) isTRUE(controle_refus(x, "donnees", "decalages_mal_formes", "« Essai »")), NA),
         isTRUE(controle_refus(r[[4L]], "donnees", "decalages_mal_formes", "« age_min »", list(colonne = "age_min"))))
  if (all(e)) TRUE else sprintf("cas %s", paste(which(!e), collapse = ", "))
})
verifier("D3 decalages_colonne_manquante : sans decalage (valeur calculée à la main)",
         controle_refus(refus_de(dec(d1()[c("age_min", "age_max")])), "donnees", "decalages_colonne_manquante",
                        "« decalage »", list(colonne = "decalage")))
verifier("D3 decalages_colonne_inconnue : colonne accentuée « décalage » à côté des trois (valeur calculée à la main)", {
  x <- d1(); x[["décalage"]] <- x$decalage
  controle_refus(refus_de(dec(x)), "donnees", "decalages_colonne_inconnue", "« décalage »", list(colonne = "décalage"))
})
verifier("D3 : colonne accentuée « âge_min » à la place d'age_min : manquante d'abord (age_min), l'ordre manquante puis inconnue (valeur calculée à la main)", {
  x <- d1(); names(x)[1L] <- "âge_min"
  controle_refus(refus_de(dec(x)), "donnees", "decalages_colonne_manquante", "« age_min »", list(colonne = "age_min"))
})
for (cas in list(list("age_min NA", "age_min", function(x) { x$age_min[1L] <- NA; x }),
                 list("age_min négatif", "age_min", function(x) { x$age_min[1L] <- -10L; x }),
                 list("age_min 1.5", "age_min", function(x) { x$age_min <- c(10.5, 20, 30); x }),
                 list("age_min facteur", "age_min", function(x) { x$age_min <- factor(x$age_min); x }),
                 list("age_min chaîne", "age_min", function(x) { x$age_min <- as.character(x$age_min); x }),
                 list("age_min non fini", "age_min", function(x) { x$age_min <- c(10, 20, Inf); x }),
                 list("age_min au-delà de .Machine$integer.max", "age_min", function(x) { x$age_min <- c(10, 20, 2^31); x }),
                 list("age_max négatif", "age_max", function(x) { x$age_max[1L] <- -1L; x }),
                 list("age_max logique (NA nus)", "age_max", function(x) { x$age_max <- c(NA, NA, NA); x }),
                 list("decalage NA", "decalage", function(x) { x$decalage[2L] <- NA; x }),
                 list("decalage 1.5", "decalage", function(x) { x$decalage <- c(-2, -1.5, 0); x }),
                 list("decalage logique", "decalage", function(x) { x$decalage <- c(TRUE, FALSE, TRUE); x }),
                 list("decalage au-delà de -.Machine$integer.max", "decalage", function(x) { x$decalage <- c(-2^31, -1, 0); x })))
  verifier(sprintf("D3 decalages_colonne_mal_typee : %s (valeur calculée à la main)", cas[[1L]]),
           controle_refus(refus_de(dec(cas[[3L]](d1()))), "donnees", "decalages_colonne_mal_typee",
                          sprintf("« %s »", cas[[2L]]), list(colonne = cas[[2L]])))
verifier("D3 decalages_bande_inversee : bande [20 ; 10] (valeur calculée à la main)",
         controle_refus(refus_de(dec(bandes(c(20, 5, 30), c(10, 19, NA)))), "donnees",
                        "decalages_bande_inversee", "[20 ; 10]",
                        list(bandes = "[20 ; 10]", age_min = 20L, age_max = 10L)))
verifier("D3 decalages_bande_ouverte : deux bandes ouvertes (valeur calculée à la main)",
         controle_refus(refus_de(dec(bandes(c(10, 20), c(NA, NA)))), "donnees", "decalages_bande_ouverte",
                        "2 bandes ouvertes", list(bandes = c("10 et plus", "20 et plus"))))
verifier("D3 decalages_bande_ouverte : bande ouverte en premier (valeur calculée à la main)",
         controle_refus(refus_de(dec(bandes(c(10, 20), c(NA, 29)))), "donnees", "decalages_bande_ouverte",
                        c("« 10 et plus »", "« [20 ; 29] »"), list(bandes = "10 et plus")))
verifier("D3 decalages_bande_ouverte : dernière bande fermée (aucune bande ouverte ; valeur calculée à la main)",
         controle_refus(refus_de(dec(bandes(c(10, 20), c(19, 29)))), "donnees", "decalages_bande_ouverte",
                        c("aucune bande ouverte", "« [20 ; 29] »"), list(bandes = "[20 ; 29]")))
verifier("D3 decalages_bandes_chevauchantes : [10 ; 20] puis [20 ; 29] (valeur calculée à la main)",
         controle_refus(refus_de(dec(bandes(c(10, 20, 30), c(20, 29, NA)))), "donnees", "decalages_bandes_chevauchantes",
                        "« [10 ; 20] » et « [20 ; 29] »", list(bandes = "[10 ; 20]", bandes_suivantes = "[20 ; 29]")))
verifier("D3 decalages_bandes_chevauchantes : doublon d'age_min, refus et details identiques sous les deux ordres reçus (valeur calculée à la main)", {
  x <- bandes(c(10, 10), c(19, NA))
  r1 <- refus_de(dec(x)); r2 <- refus_de(dec(x[2:1, ]))
  ok <- controle_refus(r1, "donnees", "decalages_bandes_chevauchantes", character(0),
                       list(bandes = "[10 ; 19]", bandes_suivantes = "10 et plus"))
  if (!isTRUE(ok)) ok else if (!identical(r1$details, r2$details) || !identical(conditionMessage(r1), conditionMessage(r2)))
    "refus différents selon l'ordre reçu" else TRUE
})
verifier("D3 decalages_bandes_non_contigues : [10 ; 19] puis [21 ; +inf[, âges 20 à 20 (valeur calculée à la main)",
         controle_refus(refus_de(dec(bandes(c(10, 21), c(19, NA)))), "donnees", "decalages_bandes_non_contigues",
                        c("« [10 ; 19] » et « 21 et plus »", "âges 20 à 20"),
                        list(bandes = "[10 ; 19]", bandes_suivantes = "21 et plus", ages_debut = 20L, ages_fin = 20L)))
verifier("D3 : au-delà de dix trous, dix paires citées puis le total ; details complets (valeur calculée à la main)", {
  r <- refus_de(dec(bandes(c(seq(0, 22, by = 2), 24), c(seq(0, 22, by = 2), NA))))
  ok <- controle_refus(r, "donnees", "decalages_bandes_non_contigues", "(12 en tout ; liste complète dans les détails du refus)")
  if (!isTRUE(ok)) ok else if (length(r$details$bandes) != 12L || grepl("« [20 ; 20] » et « [22 ; 22] »", conditionMessage(r), fixed = TRUE))
    "details ou citation hors de la règle" else TRUE
})
verifier("D3 : ordre des refus (§ 1.3) : bande inversée avant bande ouverte, bande ouverte avant chevauchement (valeur calculée à la main)", {
  a <- refus_de(dec(bandes(c(10, 20), c(5, 29))))       # inversée et aucune ouverte
  b <- refus_de(dec(bandes(c(10, 15), c(19, 29))))      # aucune ouverte et chevauchement
  identical(a$motif, "decalages_bande_inversee") && identical(b$motif, "decalages_bande_ouverte")
})

# --- D5 : l'objet n'est pas une table de référence ---------------------------------------

verifier("D5 : decalages_age passé dans l'argument tables_reference (controler_forme_tables(), appelée par construire_table()) : refus tables_reference_mal_formees, contrôle de classe exacte (valeur calculée à la main)",
         controle_refus(refus_de(tablexp:::controler_forme_tables(list(decalages = D1))), "construire_table",
                        "tables_reference_mal_formees", "au lieu de table_reference",
                        list(cle = "decalages", lu = c("decalages_age", "data.frame"))))

# --- format 3 (§ 2) ------------------------------------------------------------------------

verifier("F1 : contenu « age_min,age_max,decalage », 10,19,-2, 20,29,-1, 30,,0 : objet identical() au constructeur appelé à la main (identité exacte)", {
  o <- tablexp:::decalages_du_catalogue("Essai", list(Essai = element(LIGNES_D1)))
  identical(o, decalages_age(d1(), "Essai", "TABLE ESSAI", "inst/extdata/tables/essai.provenance.dcf"))
})
verifier("F1 : analyser_decalages_format3() rend age_min, age_max (NA pour le champ vide), decalage, en doubles (valeur calculée à la main)",
         identical(analyser_decalages_format3(LIGNES_D1, "Essai", "f.csv"),
                   list(age_min = c(10, 20, 30), age_max = c(19, 29, NA), decalage = c(-2, -1, 0))))
refus_f3 <- function(l) refus_de(analyser_decalages_format3(l, "Essai", "inst/extdata/tables/essai.csv"))
for (cas in list(list("en-tête faux", c("age_min,age_max,décalage", "10,,0"), 1L, "en-tête"),
                 list("fichier vide", character(0), 1L, "fichier vide"),
                 list("en-tête seul", EN_TETE, 2L, "aucune bande"),
                 list("+3", c(EN_TETE, "10,,+3"), 2L, "mal formée"),
                 list("-0", c(EN_TETE, "10,,-0"), 2L, "mal formée"),
                 list("- 3 (espace)", c(EN_TETE, "10,,- 3"), 2L, "mal formée"),
                 list("tiret U+2013", c(EN_TETE, "10,,–3"), 2L, "mal formée"),
                 list("moins U+2212", c(EN_TETE, "10,,−3"), 2L, "mal formée"),
                 list("03", c(EN_TETE, "10,,03"), 2L, "mal formée"),
                 list("decalage vide", c(EN_TETE, "10,19,", "20,,0"), 2L, "mal formée"),
                 list("age_min vide", c(EN_TETE, "10,19,0", ",,0"), 3L, "mal formée"),
                 list("deux champs", c(EN_TETE, "10,0"), 2L, "mal formée"),
                 list("quatre champs", c(EN_TETE, "10,19,0,1", "20,,0"), 2L, "mal formée"),
                 list("espace", c(EN_TETE, "10, 19,0", "20,,0"), 2L, "mal formée"),
                 list("age_min égal", c(EN_TETE, "10,19,0", "10,,0"), 3L, "non supérieur"),
                 list("age_min décroissant", c(EN_TETE, "10,19,0", "5,,0"), 3L, "non supérieur"),
                 list("entier au-delà de .Machine$integer.max", c(EN_TETE, "10,2147483648,0", "2147483649,,0"), 2L,
                      "plus grand entier du moteur (2147483647)"),
                 list("décalage au-delà de -.Machine$integer.max", c(EN_TETE, "10,,-2147483648"), 2L,
                      "plus grand entier du moteur")))
  verifier(sprintf("F2 decalages_embarques_illisibles : %s, ligne %d citée avec le fichier (valeur calculée à la main)", cas[[1L]], cas[[3L]]),
           controle_refus(refus_f3(cas[[2L]]), "donnees", "decalages_embarques_illisibles",
                          c("« Essai »", "inst/extdata/tables/essai.csv", sprintf("ligne %d,", cas[[3L]]), cas[[4L]]),
                          list(nom = "Essai", fichier = "inst/extdata/tables/essai.csv", ligne = cas[[3L]])))
verifier("F2 : entier égal à .Machine$integer.max admis par l'analyse (borne comprise ; valeur calculée à la main)",
         identical(analyser_decalages_format3(c(EN_TETE, "10,,2147483647"), "Essai", "f.csv")$decalage, 2147483647))
verifier("F2 : contenu non lu à l'installation (lignes NULL) : decalages_embarques_illisibles, ligne NA (valeur calculée à la main)",
         controle_refus(refus_de(catalogue_decalages_embarques(list(Essai = element(NULL)))), "donnees",
                        "decalages_embarques_illisibles", c("non lu", "inst/extdata/tables/essai.csv"),
                        list(ligne = NA_integer_, lu = NULL)))
verifier("F3 : age_max vide en ligne 2 sur 3 : refus du constructeur (decalages_bande_ouverte), pas de l'analyse (valeur calculée à la main)",
         controle_refus(refus_de(catalogue_decalages_embarques(list(Essai = element(c(EN_TETE, "10,,0", "20,29,0", "30,,0"))))),
                        "donnees", "decalages_bande_ouverte", "2 bandes ouvertes"))
verifier("F3 : trou entre deux lignes : refus du constructeur (decalages_bandes_non_contigues), sans enveloppe (valeur calculée à la main)",
         controle_refus(refus_de(catalogue_decalages_embarques(list(Essai = element(c(EN_TETE, "10,19,0", "25,,0"))))),
                        "donnees", "decalages_bandes_non_contigues", "âges 20 à 24",
                        list(ages_debut = 20L, ages_fin = 24L)))

# --- empreinte (§ 3) ------------------------------------------------------------------------

verifier("P1 : texte « tablexp decalages 1 » de D1 égal, à l'octet près, à celui du § 3.1 de la spécification ; MD5 6d87d1e7d9adbe8bf5a7248e525da865 (littéral de la spécification)", {
  t <- tablexp:::texte_objet(list(decalages_age = data.frame(age_min = D1$age_min, age_max = D1$age_max, decalage = D1$decalage)),
                             tablexp:::REPRESENTATION_DECALAGES, character(0), "empreinte")
  attendu <- paste0(paste(c("tablexp decalages 1",
                            "@noms\tcharacter\t1\t\"decalages_age\"",
                            "decalages_age/@colonnes\tcharacter\t3\t\"age_min\"\t\"age_max\"\t\"decalage\"",
                            "decalages_age/@lignes\tinteger\t1\t3",
                            "decalages_age/age_min\tinteger\t3\t10\t20\t30",
                            "decalages_age/age_max\tinteger\t3\t19\t29\tNA",
                            "decalages_age/decalage\tinteger\t3\t-2\t-1\t0"), collapse = "\n"), "\n")
  identical(t, charToRaw(attendu)) && identical(empreinte_decalages(D1), "6d87d1e7d9adbe8bf5a7248e525da865")
})
verifier("P2 : même empreinte sous des lignes permutées et sous une entrée en doubles (identité exacte)",
         identical(empreinte_decalages(dec(d1(c(3L, 1L, 2L)))), "6d87d1e7d9adbe8bf5a7248e525da865") &&
           identical(empreinte_decalages(dec(d1(f = as.double))), "6d87d1e7d9adbe8bf5a7248e525da865"))
verifier("P3 : empreinte différente si une borne, un décalage change d'une unité, ou si la bande ouverte devient fermée (valeur calculée à la main)", {
  e <- c(empreinte_decalages(dec(bandes(c(10, 20, 30), c(19, 29, NA), c(-2, -1, 0)))),
         empreinte_decalages(dec(bandes(c(10, 21, 30), c(20, 29, NA), c(-2, -1, 0)))),
         empreinte_decalages(dec(bandes(c(10, 20, 30), c(19, 29, NA), c(-2, -1, 1)))),
         empreinte_decalages(dec(bandes(c(10, 20), c(19, NA), c(-2, -1)))))
  e2 <- tryCatch(empreinte_decalages(structure(data.frame(age_min = c(10L, 20L, 30L), age_max = c(19L, 29L, 40L),
                                                          decalage = c(-2L, -1L, 0L)), class = c("decalages_age", "data.frame"))),
                 error = function(x) NA_character_)
  !anyDuplicated(c(e, e2)) && !anyNA(e2)
})
verifier("P4 : changer nom, table ou source ne change pas l'empreinte (métadonnées hors empreinte ; identité exacte)",
         identical(empreinte_decalages(decalages_age(d1(), "Autre", "AUTRE TABLE", "ailleurs.dcf")),
                   "6d87d1e7d9adbe8bf5a7248e525da865"))

# --- accesseur (§ 4) ------------------------------------------------------------------------

CAT2 <- list(Premier = element(LIGNES_D1, "premier", "TABLE A"),
             Second = element(c(EN_TETE, "5,,3"), "second", "TABLE B"))
verifier("A1 : catalogue injecté de deux déclarations : colonnes, types, une ligne par bande dans l'ordre de la déclaration puis des bandes ; empreinte égale à empreinte_decalages(), répétée sur chaque bande (valeur calculée à la main, identité exacte)", {
  x <- catalogue_decalages_embarques(CAT2)
  o2 <- tablexp:::decalages_du_catalogue("Second", CAT2)
  identical(class(x), "data.frame") && identical(names(x), COLONNES) && identical(attr(x, "row.names"), 1:4) &&
    identical(vapply(x, typeof, ""), stats::setNames(c("character", "character", "integer", "integer", "integer", "character",
                                                       "character", "character"), COLONNES)) &&
    identical(x$nom, c(rep("Premier", 3L), "Second")) && identical(x$table, c(rep("TABLE A", 3L), "TABLE B")) &&
    identical(x$age_min, c(10L, 20L, 30L, 5L)) && identical(x$age_max, c(19L, 29L, NA, NA)) &&
    identical(x$decalage, c(-2L, -1L, 0L, 3L)) &&
    identical(x$source, c(rep("inst/extdata/tables/premier.provenance.dcf", 3L), "inst/extdata/tables/second.provenance.dcf")) &&
    identical(x$representation, rep("tablexp decalages 1", 4L)) &&
    identical(x$empreinte, c(rep("6d87d1e7d9adbe8bf5a7248e525da865", 3L), empreinte_decalages(o2))) &&
    all(vapply(x, function(v) is.null(attributes(v)), NA))
})
verifier("A2 : contenu illisible, ou refus du constructeur, remonte tel quel (aucun état refusee ; valeur calculée à la main)", {
  a <- refus_de(catalogue_decalages_embarques(c(CAT2, list(Illisible = element(c(EN_TETE, "10,,+1"))))))
  b <- refus_de(catalogue_decalages_embarques(c(list(Refuse = element(c(EN_TETE, "10,19,0", "15,,0"))), CAT2)))
  identical(a$motif, "decalages_embarques_illisibles") && identical(a$details$nom, "Illisible") &&
    identical(b$motif, "decalages_bandes_chevauchantes") && identical(b$details$bandes, "[10 ; 19]")
})
verifier("A3 : catalogue vide : zéro ligne, mêmes colonnes et mêmes types (valeur calculée à la main)", {
  x <- catalogue_decalages_embarques(structure(list(), names = character(0)))
  y <- catalogue_decalages_embarques(list())
  identical(x, y) && nrow(x) == 0L && identical(names(x), COLONNES) &&
    identical(unname(vapply(x, typeof, "")), c("character", "character", "integer", "integer", "integer", "character",
                                               "character", "character"))
})
verifier("A4 : tables_embarquees() ne porte aucun décalage ; table_embarquee() sur un nom de décalages lève table_embarquee_absente ; nom de décalages non réservé aux tables (valeur calculée à la main)", {
  r <- refus_de(table_embarquee("Décalages d'âge TH 00-02"))
  t <- table_reference(data.frame(age = 0:1, sexe = "H", qx = c(0.1, 0.2)), "Décalages d'âge TH 00-02", "importee", "s")
  identical(names(tables_embarquees()), c("nom", "sexe", "forme_origine", "source", "etat", "motif_refus", "representation",
                                          "empreinte")) &&
    !any(grepl("calage", tables_embarquees()$nom)) && identical(r$motif, "table_embarquee_absente") &&
    inherits(t, "table_reference")
})

# --- déclaration, lecture à l'installation, accesseur exporté --------------------------------

verifier("déclaration : data.frame nu nom, identifiant, table (character) ; noms et identifiants uniques ; identifiant [a-z0-9_-]+ ; table déclarée au catalogue des tables ; identifiants distincts de ceux des tables (valeur calculée à la main)", {
  identical(names(DECLARATIONS), c("nom", "identifiant", "table")) && identical(class(DECLARATIONS), "data.frame") &&
    all(vapply(DECLARATIONS, is.character, NA)) && !anyDuplicated(DECLARATIONS$nom) &&
    !anyDuplicated(DECLARATIONS$identifiant) && all(grepl("^[a-z0-9_-]+$", DECLARATIONS$identifiant)) &&
    all(DECLARATIONS$table %in% tablexp:::DECLARATIONS_TABLES_EMBARQUEES$nom) &&
    !any(DECLARATIONS$identifiant %in% tablexp:::DECLARATIONS_TABLES_EMBARQUEES$identifiant)
})
verifier("DECALAGES_EMBARQUES : une entrée par déclaration, dans l'ordre ; identifiant et table de la déclaration ; lignes lues (non NULL) à l'installation", {
  e <- tablexp:::DECALAGES_EMBARQUES
  identical(names(e), DECLARATIONS$nom) &&
    identical(unname(vapply(e, function(x) x$identifiant, "")), DECLARATIONS$identifiant) &&
    identical(unname(vapply(e, function(x) x$table, "")), DECLARATIONS$table) &&
    all(vapply(e, function(x) is.character(x$lignes), NA))
})
verifier("lire_decalages_embarques() : lignes du fichier telles quelles (sans analyse) ; fichier absent -> NULL ; sans DESCRIPTION de tablexp -> NULL ; déclaration vide -> liste nommée vide", {
  r <- tempfile("racine_"); dir.create(file.path(r, "inst", "extdata", "tables"), recursive = TRUE)
  on.exit(unlink(r, recursive = TRUE))
  writeLines(c(EN_TETE, "10,,+1"), file.path(r, "inst", "extdata", "tables", "a.csv"))
  d <- data.frame(nom = c("A", "B"), identifiant = c("a", "b"), table = c("TA", "TB"), stringsAsFactors = FALSE)
  sans <- lire_decalages_embarques(r, d)
  writeLines("Package: tablexp", file.path(r, "DESCRIPTION"))
  avec <- lire_decalages_embarques(r, d)
  identical(lire_decalages_embarques(r, d[0L, ]), structure(list(), names = character(0))) &&
    is.null(sans$A$lignes) && identical(avec$A, list(identifiant = "a", table = "TA", lignes = c(EN_TETE, "10,,+1"))) &&
    identical(avec$B, list(identifiant = "b", table = "TB", lignes = NULL))
})
verifier("decalages_embarques() : exportée, sans argument, identical() au catalogue construit sur DECALAGES_EMBARQUES (identité exacte)",
         "decalages_embarques" %in% getNamespaceExports("tablexp") && is.null(names(formals(decalages_embarques))) &&
           identical(decalages_embarques(), catalogue_decalages_embarques(tablexp:::DECALAGES_EMBARQUES)))
