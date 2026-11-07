# -----------------------------------------------------------------------------
#  tests/unitaires/test_tables_reference.R : tables de référence reçues par
#  construire_table() et rejouer_fiche() (issue #73 ; ADR 0015 ; spécification
#  de #73, commentaire 6679912499, § 6, A à F ; M235 à M248)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : ordre canonique rendu, numéros de ligne,
#    constantes, champs et ordre de la fiche, motifs, modules, details et
#    extraits des refus, ordre des contrôles, nombre d'appels (trace()) ;
#  - témoin calculé hors R : empreintes de table de deux petites tables,
#    constantes littérales (recette `printf | md5sum`, doubles en
#    struct.pack('<d', v).hex() ; trois calculs concordants, spécification,
#    § 2) ;
#  - identité exacte : objets identical() (permutations, rejeu hors
#    execution, aller-retour de la fiche).
#  Le contrôle 1.3 est testé par injection de `parametres_cle` et
#  `embarquees`, le contrôle 7 bis par injection de `embarquees` (M247 ; le
#  1.3 n'est plus fait par le 7 bis, #82) : au commit (1), aucune étape ne
#  citait de clé ; depuis le commit (3) (#4), PARAMETRES_CLE_TABLE vaut
#  « etapes/validation/table_reference », et les plans de ce fichier, sans
#  étape validation, ne citent aucune clé ; aucune table n'est embarquée
#  (TABLES_EMBARQUEES vide). Le chemin de bout en bout avec une table
#  (construction puis rejeu) est dans tests/unitaires/test_validation.R.
# -----------------------------------------------------------------------------

D <- as.Date
ns <- asNamespace("tablexp")
empreinte_table <- tablexp:::empreinte_table
element_table_fiche <- tablexp:::element_table_fiche
controler_forme_tables <- tablexp:::controler_forme_tables
controler_tables_reference <- tablexp:::controler_tables_reference
controler_tables_rejeu <- tablexp:::controler_tables_rejeu
CHAMPS_TABLE_FICHE <- tablexp:::CHAMPS_TABLE_FICHE
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)

# Refus attendu : module, motif, details$lu et details$attendu (si donnés), et
# extraits du message (fixed = TRUE). Rend TRUE ou la liste des écarts.
controle_refus <- function(r, module, motif, lu, attendu, contient = character(0)) {
  if (is.null(r)) return("aucun refus levé")
  e <- character(0)
  if (!identical(r$module, module)) e <- c(e, sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) e <- c(e, sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!missing(lu) && !identical(r$details$lu, lu)) e <- c(e, paste("details$lu :", paste(deparse(r$details$lu), collapse = " ")))
  if (!missing(attendu) && !identical(r$details$attendu, attendu))
    e <- c(e, paste("details$attendu :", paste(deparse(r$details$attendu), collapse = " ")))
  for (s in contient) if (!grepl(s, conditionMessage(r), fixed = TRUE)) e <- c(e, sprintf("message sans « %s »", s))
  if (length(e)) e else TRUE
}

# Appels de construire_table() et de preparer_donnees() pendant expr (trace()
# sur l'espace de noms installé, retiré à la sortie) ; un refus est rattrapé
# (comme tests/unitaires/test_rejeu.R).
compter_appels <- function(expr) {
  n <- new.env()
  n$construire_table <- 0L; n$preparer_donnees <- 0L
  for (f in c("construire_table", "preparer_donnees"))
    suppressMessages(trace(f, where = ns, print = FALSE,
                           tracer = bquote(assign(.(f), get(.(f), envir = .(n)) + 1L, envir = .(n)))))
  on.exit(for (f in c("construire_table", "preparer_donnees")) suppressMessages(untrace(f, where = ns)))
  tryCatch(force(expr), refus_tablexp = function(r) NULL)
  c(construire_table = n$construire_table, preparer_donnees = n$preparer_donnees)
}

# Un ulp au-dessus d'un double positif (bit de poids faible de la mantisse).
ulp_suivant <- function(v) {
  b <- writeBin(v, raw(), size = 8L, endian = "little")
  b[1L] <- as.raw(bitwXor(as.integer(b[1L]), 1L))
  readBin(b, "double", size = 8L, endian = "little")
}

TR <- data.frame(age = c(60L, 61L, 62L, 60L, 61L, 62L), sexe = c("H", "H", "H", "F", "F", "F"),
                 qx = c(0.01, 0.011, 0.012, 0.006, 0.0066, 0.0072), stringsAsFactors = FALSE)
importee <- function(x = TR, nom = "essai", source = "test unitaire") table_reference(x, nom, "importee", source)
T1 <- importee()

# --- A. Tri canonique (M235 ; ADR 0015, point 2.1) ------------------------------

verifier("A tri : table permutée (F puis H, entrelacée, aléatoire) : objet identical() et même empreinte (identité exacte)", {
  set.seed(20261106)
  perms <- list(c(4:6, 1:3), c(1L, 4L, 2L, 5L, 3L, 6L), sample(6L))
  e <- character(0)
  for (p in perms) {
    x <- TR[p, ]; rownames(x) <- NULL
    t <- importee(x)
    if (!identical(t, T1)) e <- c(e, sprintf("objet différent (%s)", paste(p, collapse = ",")))
    if (!identical(empreinte_table(t), empreinte_table(T1))) e <- c(e, "empreinte différente")
  }
  if (length(e)) e else TRUE
})
verifier("A tri : ordre rendu H, F (rang dans SEXES_TABLE), âges croissants, q_x suivant leur âge (valeur calculée à la main)",
         identical(T1$sexe, c("H", "H", "H", "F", "F", "F")) && identical(T1$age, c(60L, 61L, 62L, 60L, 61L, 62L)) &&
           identical(T1$qx, TR$qx))
verifier("A tri : sexe facteur aux niveaux (F, H) : même objet que le character (rang de SEXES_TABLE, non des niveaux)", {
  x <- TR; x$sexe <- factor(x$sexe, levels = c("F", "H"))
  identical(importee(x), T1)
})
verifier("A tri : par annee puis age au sein du sexe ; colonnes age, sexe, qx, annee (valeur calculée à la main)", {
  t <- importee(data.frame(age = c(61L, 60L, 61L, 60L), sexe = "H", annee = c(2021L, 2021L, 2020L, 2020L),
                           qx = c(4, 3, 2, 1) / 100))
  identical(names(t), c("age", "sexe", "qx", "annee")) && identical(t$annee, c(2020L, 2020L, 2021L, 2021L)) &&
    identical(t$age, c(60L, 61L, 60L, 61L)) && identical(t$qx, c(1, 2, 3, 4) / 100)
})
verifier("A tri : row.names automatiques après une permutation (attr(t, \"row.names\") == 1:n, jamais x[o, ])", {
  t <- importee(TR[6:1, ])
  identical(attr(t, "row.names"), seq_len(nrow(t)))
})
verifier("A tri : ancien cas « âges non croissants » (H 60, 62, 61) admis, rendu dans l'ordre canonique", {
  t <- importee(transform(TR, age = c(60L, 62L, 61L, 60L, 61L, 62L)))
  identical(t$age[1:3], 60:62) && identical(t$qx[1:3], c(0.01, 0.012, 0.011))
})
verifier("A tri : doublon non adjacent (H 60 / F 60 / H 60) : table_age_double, ligne reçue 3", {
  r <- refus_de(importee(data.frame(age = c(60L, 60L, 60L), sexe = c("H", "F", "H"), qx = 0.01)))
  ok <- controle_refus(r, "donnees", "table_age_double", contient = "lignes 3.")
  if (isTRUE(ok) && !identical(r$details$lignes, 3L)) "details$lignes" else ok
})
verifier("A tri : trou cité en numérotation reçue (H reçu 63, 60, 61 : « entre les lignes 3 et 1 », ligne suivante 1)", {
  r <- refus_de(importee(transform(TR, age = c(63L, 60L, 61L, 60L, 61L, 62L))))
  ok <- controle_refus(r, "donnees", "table_ages_non_contigus", contient = "entre les lignes 3 et 1.")
  if (isTRUE(ok) && !identical(r$details$lignes, 1L)) "details$lignes" else ok
})
verifier("A tri : tables synthétiques de jeu_reference() (Gompertz-Makeham, constante) : tri identité, objet reconstruit identical()", {
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  refait <- function(t) table_reference(data.frame(age = t$age, sexe = t$sexe, qx = t$qx, stringsAsFactors = FALSE),
                                        attr(t, "nom"), attr(t, "statut"), attr(t, "source"), attr(t, "parametres"),
                                        attr(t, "forme_origine"))
  identical(gm$age, 0:120) && identical(ra$age, 0:120) && identical(refait(gm), gm) && identical(refait(ra), ra)
})

# --- B. Empreinte de table (ADR 0015, point 2.2) --------------------------------

verifier("B empreinte : témoin hors R sans annee (F puis H reçus, 4 lignes) = fba5befb8439f89d56cc095a159f7605",
         identical(empreinte_table(importee(data.frame(age = c(60L, 61L, 60L, 61L), sexe = c("F", "F", "H", "H"),
                                                       qx = c(0.006, 0.0066, 0.01, 0.011)))),
                   "fba5befb8439f89d56cc095a159f7605"))
verifier("B empreinte : témoin hors R avec annee (colonnes reçues qx, annee, sexe, age) = d1f94fa332c4a430de6bfb4680870120",
         identical(empreinte_table(importee(data.frame(qx = c(0.011, 0.01), annee = 2020L, sexe = "H", age = c(61L, 60L)))),
                   "d1f94fa332c4a430de6bfb4680870120"))
verifier("B empreinte : texte empreinté, première ligne REPRESENTATION_TABLE, racine table_reference (valeur calculée à la main)", {
  d <- data.frame(age = T1$age, sexe = T1$sexe, qx = T1$qx, stringsAsFactors = FALSE)
  o <- tablexp:::texte_objet(list(table_reference = d), "tablexp table 1", character(0), "empreinte")
  l <- strsplit(rawToChar(o), "\n", fixed = TRUE)[[1L]]
  identical(l[1:3], c("tablexp table 1", "@noms\tcharacter\t1\t\"table_reference\"",
                      "table_reference/@colonnes\tcharacter\t3\t\"age\"\t\"sexe\"\t\"qx\"")) &&
    identical(tablexp:::md5_octets(o), empreinte_table(T1))
})
verifier("B empreinte : un ulp sur un q_x la change", {
  x <- TR; x$qx[2L] <- ulp_suivant(x$qx[2L])
  x$qx[2L] != TR$qx[2L] && !identical(empreinte_table(importee(x)), empreinte_table(T1))
})
verifier("B empreinte : nom, source, statut et parametres ne la changent pas", {
  e <- empreinte_table(T1)
  identical(empreinte_table(importee(nom = "autre", source = "autre source")), e) &&
    identical(empreinte_table(table_reference(TR, "essai", "synthetique", "s", parametres = list(A = 1))), e) &&
    identical(empreinte_table(table_reference(TR, "essai", "embarquee", "s")), e)
})

# --- C. Fiche (ADR 0015, point 3 ; VERSION_FICHE 5) ------------------------------

BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
PLAN <- list(
  transition = list(depart = "vivant", arrivee = "deces"),
  periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0, segmentation = "sexe",
  etapes = list(donnees = list(exclusions = character(0)),
                exposition = list(convention = "plus_fine", par_annee = FALSE),
                brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95)))
X <- construire_table(BASE, ROLES, PLAN)

verifier("C constantes : VERSION_FICHE 5L, VERSIONS_FICHE_REJEU 5L, REPRESENTATION_TABLE « tablexp table 1 », PARAMETRES_CLE_TABLE « etapes/validation/table_reference » (#4), TABLES_EMBARQUEES liste nommée vide",
         identical(tablexp:::VERSION_FICHE, 5L) && identical(tablexp:::VERSIONS_FICHE_REJEU, 5L) &&
           identical(tablexp:::REPRESENTATION_TABLE, "tablexp table 1") &&
           identical(tablexp:::PARAMETRES_CLE_TABLE, "etapes/validation/table_reference") &&
           identical(tablexp:::TABLES_EMBARQUEES, structure(list(), names = character(0))))
verifier("C constantes : tables_reference après empreintes dans ORDRE_CHAMPS_FICHE et dans BLOCS_LECTURE_FICHE$rejeu ; fiche/tables_reference après fiche/provenance_mention dans la section donnees_recues", {
  o <- tablexp:::ORDRE_CHAMPS_FICHE; b <- tablexp:::BLOCS_LECTURE_FICHE$rejeu; s <- tablexp:::SECTIONS_RAPPORT
  r <- s$racines[[match("donnees_recues", s$section)]]
  identical(match("tables_reference", o), match("empreintes", o) + 1L) &&
    identical(match("tables_reference", b), match("empreintes", b) + 1L) &&
    identical(match("fiche/tables_reference", r), match("fiche/provenance_mention", r) + 1L)
})
verifier("C fiche sans table : champ tables_reference présent, NULL, après empreintes ; version_fiche 5L", {
  n <- names(X$fiche)
  "tables_reference" %in% n && is.null(X$fiche$tables_reference) &&
    identical(match("tables_reference", n), match("empreintes", n) + 1L) && identical(X$fiche$version_fiche, 5L)
})
T_SYNTH <- table_reference(transform(TR, qx = TR$qx * 2), "gm", "synthetique", "loi de test",
                           parametres = list(A = 2e-4, ages = c(0L, 120L)))
PLAN_AB <- list(etapes = list(essai = list(cle = "a", cle2 = "b")))
CLES_AB <- c("etapes/essai/cle", "etapes/essai/cle2")
CHAMP_AB <- controler_tables_reference(list(b = T1, a = T_SYNTH), PLAN_AB, CLES_AB)
verifier("C élément de fiche : list(b = importée, a = synthétique) -> clés a, b ; sept champs dans l'ordre ; parametres NULL hors synthetique ; empreinte de table", {
  identical(names(CHAMP_AB), c("a", "b")) &&
    all(vapply(CHAMP_AB, function(e) identical(names(e), CHAMPS_TABLE_FICHE), NA)) &&
    identical(CHAMPS_TABLE_FICHE, c("nom", "statut", "source", "forme_origine", "parametres", "representation", "empreinte")) &&
    identical(CHAMP_AB$a, list(nom = "gm", statut = "synthetique", source = "loi de test", forme_origine = "qx",
                               parametres = list(A = 2e-4, ages = c(0L, 120L)), representation = "tablexp table 1",
                               empreinte = empreinte_table(T_SYNTH))) &&
    identical(CHAMP_AB$b, element_table_fiche(T1)) && "parametres" %in% names(CHAMP_AB$b) && is.null(CHAMP_AB$b$parametres)
})
FICHE_AB <- X$fiche; FICHE_AB["tables_reference"] <- list(CHAMP_AB)
verifier("C aller-retour lire_fiche(ecrire_fiche()) identical() avec deux tables (importée, synthétique à paramètres)",
         identical(lire_fiche(ecrire_fiche(FICHE_AB)), FICHE_AB))
verifier("C empreinte du résultat : changée par le champ tables_reference (dans son périmètre)", {
  y <- X; y$fiche <- FICHE_AB
  !identical(tablexp:::empreinte_resultat(y), X$fiche$empreintes$resultat) &&
    identical(tablexp:::empreinte_resultat(X), X$fiche$empreintes$resultat)
})

# --- D. Entrée de construire_table() (contrôle 1.3) ------------------------------

verifier("D signatures : construire_table(donnees, roles, plan, provenance = NULL, tables_reference = NULL), rejouer_fiche(donnees, fiche, tables_reference = NULL), table_embarquee(nom)",
         identical(names(formals(construire_table)), c("donnees", "roles", "plan", "provenance", "tables_reference")) &&
           is.null(formals(construire_table)$tables_reference) &&
           identical(names(formals(rejouer_fiche)), c("donnees", "fiche", "tables_reference")) &&
           is.null(formals(rejouer_fiche)$tables_reference) &&
           identical(names(formals(table_embarquee)), "nom") && "table_embarquee" %in% getNamespaceExports("tablexp"))
verifier("D NULL et list() : même objet qu'un appel sans l'argument (identité exacte, hors execution)",
         identical(sans_execution(construire_table(BASE, ROLES, PLAN, tables_reference = NULL)), sans_execution(X)) &&
           identical(sans_execution(construire_table(BASE, ROLES, PLAN, tables_reference = list())), sans_execution(X)))

nom_bytes <- local({ b <- "t\xff"; Encoding(b) <- "bytes"; b })
ATTENDU_ARG <- "NULL ou liste nommée d'objets table_reference"
CLASSES_TR <- c("table_reference", "data.frame")
CAS_MAL_FORMEES <- list(
  list("data.frame seul (objet table_reference hors liste)", T1,
       "list de longueur 3, attribut(s) class, row.names, nom, statut, source, forme_origine", ATTENDU_ARG,
       "reçu : objet de classe table_reference/data.frame (type list, longueur 3)"),
  list("liste sans noms", list(T1), "list de longueur 1", ATTENDU_ARG, "reçu : objet de classe list (type list, longueur 1)"),
  list("nom en double", list(a = T1, a = T1), "list de longueur 2, noms en double : a", ATTENDU_ARG, "longueur 2"),
  list("nom vide", structure(list(T1), names = ""), "list de longueur 1, noms incomplets", ATTENDU_ARG, "liste nommée"),
  list("nom NA", structure(list(T1), names = NA_character_), "list de longueur 1, noms incomplets", ATTENDU_ARG,
       "liste nommée"),
  list("nom d'encodage bytes", structure(list(T1), names = nom_bytes),
       "list de longueur 1, nom(s) d'encodage bytes (n° 1 : <chaîne d'encodage bytes>)", ATTENDU_ARG, "liste nommée"),
  list("attribut en trop", structure(list(a = T1), x = 1), "list de longueur 1, attribut(s) x", ATTENDU_ARG,
       "sans autre attribut que ses noms"),
  list("data.frame simple", list(a = as.data.frame(T1)), "data.frame", CLASSES_TR,
       "Table de clé « a » : objet de classe data.frame au lieu de table_reference."),
  list("t[3:1, ]", list(a = T1[3:1, ]), "objet reçu", "objet reconstruit par table_reference()",
       "Table de clé « a » : objet modifié après table_reference()."),
  list("q_x porté à 2", list(a = within(T1, qx[1L] <- 2)), "table_qx_hors_bornes", "aucun refus",
       "refusé par table_reference() (motif table_qx_hors_bornes)"),
  list("nom de table d'encodage bytes", list(a = structure(T1, nom = nom_bytes)), "chaine_codage_bytes",
       "écriture du texte de fiche sans refus",
       "métadonnée sans écriture exacte dans la fiche (refus chaine_codage_bytes, a/nom)"),
  list("fonction dans parametres", list(a = table_reference(TR, "s", "synthetique", "x", parametres = list(f = identity))),
       "serialisation_impossible", "écriture du texte de fiche sans refus",
       "métadonnée sans écriture exacte dans la fiche (refus serialisation_impossible, a/parametres/f)"))
for (cas in CAS_MAL_FORMEES)
  verifier(sprintf("D tables_reference_mal_formees, %s : module construire_table, details$lu et $attendu, message", cas[[1L]]),
           controle_refus(refus_de(construire_table(BASE, ROLES, PLAN, tables_reference = cas[[2L]])),
                          "construire_table", "tables_reference_mal_formees", cas[[3L]], cas[[4L]],
                          c(cas[[5L]], if (identical(cas[[4L]], ATTENDU_ARG)) "(ADR 0015, point 1.1)"
                                       else "doit être l'objet rendu par table_reference(), sans modification")))
# Clé non écrivable dans la fiche (audit de #73, m2) : message de l'argument
# inchangé, clé en cause et motif de l'écrivain dans les details seulement.
verifier("D tables_reference_mal_formees, clé aux octets non UTF-8 : details$cle, $chemin et $motif_ecriture (chaine_non_utf8), message de l'argument", {
  # Marquée UTF-8 (octets invalides) : même motif sous toute locale (une
  # chaîne `unknown` serait chaine_codage_indetermine sous C).
  k <- rawToChar(as.raw(c(0x74, 0xff))); Encoding(k) <- "UTF-8"
  r <- refus_de(controler_forme_tables(stats::setNames(list(T1, T1), c("a", k))))
  ok <- controle_refus(r, "construire_table", "tables_reference_mal_formees",
                       contient = "reçu : objet de classe list (type list, longueur 2).")
  if (!isTRUE(ok)) ok else identical(r$details$cle, k) && identical(r$details$chemin, "tables_reference/@noms") &&
    identical(r$details$motif_ecriture, "chaine_non_utf8") && !grepl("chaine_non_utf8", conditionMessage(r), fixed = TRUE)
})
verifier("D tables_reference_mal_formees sous LC_ALL=C, clé non ASCII d'encodage indéterminé : details$motif_ecriture chaine_codage_indetermine (sous-processus)", {
  f <- tempfile(fileext = ".R"); on.exit(unlink(f))
  writeLines(c(
    'suppressMessages(library(tablexp, lib.loc = Sys.getenv("TABLEXP_BIBLIOTHEQUE")))',
    't1 <- table_reference(data.frame(age = 60:61, sexe = "H", qx = c(0.01, 0.011)), "e", "importee", "s")',
    'k <- rawToChar(as.raw(c(0x74, 0xc3, 0xa9)))',
    'r <- tryCatch(tablexp:::controler_forme_tables(stats::setNames(list(t1), k)), refus_tablexp = function(e) e)',
    'cat("RESULTAT", Sys.getlocale("LC_CTYPE"), r$motif, r$details$motif_ecriture, identical(r$details$cle, k),',
    '    Encoding(conditionMessage(r)), "\\n")'), f)
  s <- executer_rscript(f, env = c("LC_ALL=C", "LANG=C"))
  l <- grep("^RESULTAT", s$sortie, value = TRUE)
  if (s$statut != 0L || length(l) != 1L) paste(s$sortie, collapse = " | ")
  else identical(l, "RESULTAT C tables_reference_mal_formees chaine_codage_indetermine TRUE UTF-8 ")
})
verifier("D tables_reference_mal_formees : motif du constructeur et chemin dans les details", {
  r1 <- refus_de(controler_forme_tables(list(a = within(T1, qx[1L] <- 2))))
  r2 <- refus_de(controler_forme_tables(list(a = structure(T1, nom = nom_bytes))))
  identical(r1$details$cle, "a") && identical(r1$details$motif_constructeur, "table_qx_hors_bornes") &&
    identical(r2$details$chemin, "a/nom") && identical(names(r1$details), c("cle", "lu", "attendu", "motif_constructeur", "chemin"))
})
verifier("D tables_reference_mal_formees : éléments contrôlés dans l'ordre des clés triées par octets (« B » avant « a »)", {
  r <- refus_de(controler_forme_tables(list(a = 1, B = 2)))
  identical(r$details$cle, "B")
})
verifier("D table_reference_inattendue de bout en bout (plan sans étape validation : toute table reçue est inattendue), préparation non appelée", {
  n <- compter_appels(construire_table(BASE, ROLES, PLAN, tables_reference = list(t1 = T1)))
  ok <- controle_refus(refus_de(construire_table(BASE, ROLES, PLAN, tables_reference = list(t1 = T1))),
                       "construire_table", "table_reference_inattendue", "t1", character(0),
                       c("Table(s) de clé « t1 » reçue(s)", "(clés citées : aucune)"))
  if (isTRUE(ok) && n[["preparer_donnees"]] != 0L) "preparer_donnees() appelée" else ok
})

# Injection (M247) : un plan minimal qui cite la clé t1 par le paramètre
# etapes/essai/cle.
PLAN_T1 <- list(etapes = list(essai = list(cle = "t1")))
CLE_ESSAI <- "etapes/essai/cle"
T_EMB <- table_reference(TR, "TABLE_E", "embarquee", "inst/extdata/table_e.provenance.md")
verifier("D injection : table citée et reçue -> élément de fiche rendu", {
  r <- controler_tables_reference(list(t1 = T1), PLAN_T1, CLE_ESSAI)
  identical(r, list(t1 = element_table_fiche(T1)))
})
verifier("D injection : table_reference_absente (argument NULL), paramètre cité",
         controle_refus(refus_de(controler_tables_reference(NULL, PLAN_T1, CLE_ESSAI)), "construire_table",
                        "table_reference_absente", character(0), "t1",
                        c("Le plan cite la table de clé « t1 » (paramètre etapes/essai/cle)", "ne porte aucune table (NULL)",
                          "ne cherche aucune table par son nom")))
verifier("D injection : table_reference_absente (autre clé reçue), clés reçues citées",
         controle_refus(refus_de(controler_tables_reference(list(t2 = T1), PLAN_T1, CLE_ESSAI)), "construire_table",
                        "table_reference_absente", "t2", "t1", "ne porte pas cette clé (clés reçues : « t2 »)"))
verifier("D injection : table_reference_inattendue (clé t2 en trop), clés citées",
         controle_refus(refus_de(controler_tables_reference(list(t1 = T1, t2 = T1), PLAN_T1, CLE_ESSAI)),
                        "construire_table", "table_reference_inattendue", "t2", "t1", "(clés citées : « t1 »)"))
verifier("D injection : clé citée par deux paramètres, admise",
         identical(names(controler_tables_reference(list(t1 = T1), list(etapes = list(essai = list(cle = "t1", cle2 = "t1"))),
                                                    CLES_AB)), "t1"))
verifier("D injection : table embarquee, catalogue vide -> table_embarquee_absente",
         controle_refus(refus_de(controler_tables_reference(list(t1 = T_EMB), PLAN_T1, CLE_ESSAI)), "construire_table",
                        "table_embarquee_absente", "TABLE_E", character(0),
                        c("de statut « embarquee » et de nom « TABLE_E »", "(tables embarquées : aucune)")))
verifier("D injection : table embarquee égale à celle du catalogue -> admise",
         identical(controler_tables_reference(list(t1 = T_EMB), PLAN_T1, CLE_ESSAI, list(TABLE_E = T_EMB)),
                   list(t1 = element_table_fiche(T_EMB))))
verifier("D injection : table embarquee discordante (un ulp au catalogue) -> table_embarquee_discordante, empreintes en details", {
  x <- TR; x$qx[1L] <- ulp_suivant(x$qx[1L])
  cat_ <- list(TABLE_E = table_reference(x, "TABLE_E", "embarquee", "s"))
  controle_refus(refus_de(controler_tables_reference(list(t1 = T_EMB), PLAN_T1, CLE_ESSAI, cat_)), "construire_table",
                 "table_embarquee_discordante", empreinte_table(T_EMB), empreinte_table(cat_$TABLE_E),
                 c("Obtenir la table par table_embarquee()", empreinte_table(cat_$TABLE_E)))
})
verifier("D ordre des quatre contrôles : mal formées, puis absente, puis inattendue, puis embarquées (absente avant discordante)", {
  m <- function(...) refus_de(controler_tables_reference(...))$motif
  x <- TR; x$qx[1L] <- ulp_suivant(x$qx[1L])
  cat_ <- list(TABLE_E = table_reference(x, "TABLE_E", "embarquee", "s"))
  T_EMB2 <- table_reference(TR, "TABLE_F", "embarquee", "s")
  plan2 <- list(etapes = list(essai = list(cle = "t1", cle2 = "t2")))
  identical(c(m(list(t9 = 1), PLAN_T1, CLE_ESSAI),
              m(list(t9 = T1), PLAN_T1, CLE_ESSAI),
              m(list(t1 = T1, t9 = T_EMB), PLAN_T1, CLE_ESSAI),
              m(list(t1 = T_EMB, t2 = T_EMB2), plan2, CLES_AB, cat_)),
            c("tables_reference_mal_formees", "table_reference_absente", "table_reference_inattendue",
              "table_embarquee_absente"))
})
verifier("D table_embarquee() : catalogue vide, refus table_embarquee_absente (module donnees), nom cité ; nom non chaîne refusé de même", {
  a <- controle_refus(refus_de(table_embarquee("TD 88-90")), "donnees", "table_embarquee_absente", "TD 88-90",
                      character(0), c("« TD 88-90 »", "(tables embarquées : aucune)"))
  b <- controle_refus(refus_de(table_embarquee(1)), "donnees", "table_embarquee_absente", 1, character(0), "nom illisible")
  if (isTRUE(a) && isTRUE(b)) TRUE else c(a, b)
})

# --- E. Rejeu (contrôle 7 bis, ligne 8 du bilan) ---------------------------------

FICHE <- lire_fiche(ecrire_fiche(X$fiche))
R0 <- rejouer_fiche(BASE, FICHE)
verifier("E fiche sans table rejouée avec NULL et list() : objet identical() hors execution, 14 lignes, ligne 8 (valeur calculée à la main)", {
  r1 <- rejouer_fiche(BASE, FICHE, list())
  b <- R0$bilan
  identical(sans_execution(R0$construction), sans_execution(X)) &&
    identical(sans_execution(r1$construction), sans_execution(X)) && identical(r1$bilan, b) && nrow(b) == 14L &&
    identical(unlist(b[8L, ], use.names = FALSE),
              c("tables_reference", "aucune table de référence (fiche/tables_reference : NULL)",
                "aucune table reçue ni relue", "conforme", "exacte ; sans rôle de sécurité"))
})
verifier("E fiche sans table, liste non vide reçue : table_reference_inattendue (texte du rejeu), construire_table() non appelée", {
  n <- compter_appels(rejouer_fiche(BASE, FICHE, list(t1 = T1)))
  ok <- controle_refus(refus_de(rejouer_fiche(BASE, FICHE, list(t1 = T1))), "construire_table",
                       "table_reference_inattendue", "t1", character(0),
                       c("Rejeu : clé(s) « t1 » reçue(s)", "(clés attendues : aucune)", "jamais reçue"))
  if (isTRUE(ok) && n[["construire_table"]] != 0L) "construire_table() appelée" else ok
})
verifier("E fiche de version 4 (sans tables_reference) : version_fiche_non_admise (lu 4L, admises 5L), pas fiche_non_conforme", {
  f4 <- FICHE; f4$version_fiche <- 4L; f4$tables_reference <- NULL
  r <- refus_de(rejouer_fiche(BASE, f4))
  if (is.null(r)) "aucun refus" else identical(r$motif, "version_fiche_non_admise") && identical(r$details$lu, 4L) &&
    identical(r$details$admises, 5L)
})
FICHE_SANS <- FICHE; FICHE_SANS$tables_reference <- NULL
FORME_ATTENDUE <- paste("NULL, ou liste nommée non vide aux noms uniques, par clé triée par octets, d'éléments aux sept",
                        "champs nom, statut, source, forme_origine, parametres, representation, empreinte (dans cet",
                        "ordre) ; nom, representation, empreinte : chaînes uniques ; statut : synthetique, embarquee ou",
                        "importee")
champ_fiche <- function(v) { f <- FICHE; f["tables_reference"] <- list(v); f }
EL <- element_table_fiche(T1)
CAS_FORME <- list(
  list("champ absent (version 5)", FICHE_SANS, "champ absent"),
  list("chaîne « x »", champ_fiche("x"), "character de longueur 1"),
  list("liste vide", champ_fiche(list()), "list de longueur 0"),
  list("liste sans noms", champ_fiche(list(EL)), "list de longueur 1"),
  list("élément sans le champ empreinte", champ_fiche(list(t1 = EL[-7L])),
       "élément « t1 » : list de longueur 6, champs « nom », « statut », « source », « forme_origine », « parametres », « representation »"),
  list("statut hors STATUTS_TABLE", champ_fiche(list(t1 = replace(EL, "statut", list("officielle")))),
       "élément « t1 », champ « statut » : character de longueur 1"),
  list("empreinte NA", champ_fiche(list(t1 = replace(EL, "empreinte", list(NA_character_)))),
       "élément « t1 », champ « empreinte » : character de longueur 1, NA"))
for (cas in CAS_FORME)
  verifier(sprintf("E contrôle 2, tables_reference %s : fiche_non_conforme, details (champ, lu, attendu)", cas[[1L]]), {
    r <- refus_de(rejouer_fiche(BASE, cas[[2L]]))
    ok <- controle_refus(r, "construire_table", "fiche_non_conforme", cas[[3L]], FORME_ATTENDUE,
                         paste0("« tables_reference » : ", cas[[3L]], " au lieu de NULL"))
    if (isTRUE(ok) && !identical(r$details$champ, "tables_reference")) "details$champ" else ok
  })

# Fiche de version 5 construite à la main, qui porte la table t1 (importée) ;
# le plan ne cite aucune clé (pas d'étape validation) : le contrôle 1.3,
# fait par construire_table() que le rejeu appelle après le contrôle 7 bis
# (#82 ; #164, point 2), refuse ensuite par table_reference_inattendue.
fiche_t1 <- function(el = EL) champ_fiche(list(t1 = el))
F_T1 <- fiche_t1()
verifier("E fiche t1 : représentation fausse -> representation_table_non_admise",
         controle_refus(refus_de(rejouer_fiche(BASE, fiche_t1(replace(EL, "representation", list("tablexp table 0"))), list(t1 = T1))),
                        "construire_table", "representation_table_non_admise", "tablexp table 0", "tablexp table 1",
                        c("Table de clé « t1 » (nom « essai ») : représentation « tablexp table 0 » lue dans la fiche, « tablexp table 1 » attendue.",
                          "aucune conversion n'est faite")))
verifier("E fiche t1 : clé manquante -> table_reference_absente (texte du rejeu)",
         controle_refus(refus_de(rejouer_fiche(BASE, F_T1)), "construire_table", "table_reference_absente", character(0), "t1",
                        c(sprintf("Rejeu : la fiche porte la table de clé « t1 » (nom « essai », statut « importee », empreinte %s)",
                                  EL$empreinte), "(aucune table reçue)", "comme les données")))
verifier("E fiche t1 : clé en trop -> table_reference_inattendue (texte du rejeu)",
         controle_refus(refus_de(rejouer_fiche(BASE, F_T1, list(t1 = T1, t2 = T1))), "construire_table",
                        "table_reference_inattendue", "t2", "t1", "(clés attendues : « t1 »)"))
verifier("E fiche t1 de statut embarquee, table reçue -> table_reference_inattendue (une table embarquée n'est jamais reçue)",
         controle_refus(refus_de(rejouer_fiche(BASE, fiche_t1(replace(EL, "statut", list("embarquee"))), list(t1 = T1))),
                        "construire_table", "table_reference_inattendue", "t1", character(0),
                        "Une table embarquée est relue par son nom dans le package"))
verifier("E fiche t1 de statut embarquee hors catalogue -> table_embarquee_absente, empreinte du code de la fiche citée",
         controle_refus(refus_de(rejouer_fiche(BASE, fiche_t1(replace(EL, "statut", list("embarquee"))))),
                        "construire_table", "table_embarquee_absente", "essai", character(0),
                        c("(tables embarquées : aucune)", sprintf("empreinte du code de la fiche : %s", FICHE$empreintes$code))))
verifier("E fiche t1 : un ulp sur un q_x de la table reçue -> empreinte_table_discordante, construire_table() non appelée", {
  x <- TR; x$qx[3L] <- ulp_suivant(x$qx[3L]); t <- importee(x)
  n <- compter_appels(rejouer_fiche(BASE, F_T1, list(t1 = t)))
  ok <- controle_refus(refus_de(rejouer_fiche(BASE, F_T1, list(t1 = t))), "construire_table", "empreinte_table_discordante",
                       EL$empreinte, empreinte_table(t),
                       c(sprintf("Empreinte de la table de clé « t1 » discordante : %s lue dans fiche/tables_reference", EL$empreinte),
                         sprintf("%s recalculée sur la table reçue.", empreinte_table(t)), "représentation « tablexp table 1 »",
                         "non par le rejeu."))
  if (isTRUE(ok) && n[["construire_table"]] != 0L) "construire_table() appelée" else ok
})
verifier("E fiche t1 synthétique discordante : complément libm et chemin des paramètres", {
  el <- element_table_fiche(T_SYNTH); el$empreinte <- "0"
  controle_refus(refus_de(rejouer_fiche(BASE, fiche_t1(el), list(t1 = T_SYNTH))), "construire_table",
                 "empreinte_table_discordante", "0", empreinte_table(T_SYNTH),
                 "ses paramètres sont dans fiche/tables_reference/t1/parametres.")
})
verifier("E fiche t1 : table permutée reçue, admise par le contrôle 7 bis, puis table_reference_inattendue (texte de la construction), levé par construire_table() (contrôle 1.3, une fois, #82) avant toute étape", {
  x <- TR[6:1, ]; rownames(x) <- NULL
  n <- compter_appels(rejouer_fiche(BASE, F_T1, list(t1 = importee(x))))
  ok <- controle_refus(refus_de(rejouer_fiche(BASE, F_T1, list(t1 = importee(x)))), "construire_table",
                       "table_reference_inattendue", "t1", character(0), "citée(s) par aucun paramètre du plan")
  if (isTRUE(ok) && !identical(n, c(construire_table = 1L, preparer_donnees = 0L))) "appels" else ok
})
# Bout en bout par injection dans l'espace de noms (audit de #73, m1) :
# PARAMETRES_CLE_TABLE posé à deux paramètres character du catalogue
# (convention, estimateurs), dont les valeurs servent de clés, puis restauré.
# Deux tables aux clés « poisson » et « plus_fine » : fiche aux clés triées
# par octets ; rejeu admis ; fiche aux clés dans un autre ordre refusée
# fiche_non_conforme (et non arrêtée par la garde d'erreur de programmation).
local({
  ancien <- ns$PARAMETRES_CLE_TABLE
  unlockBinding("PARAMETRES_CLE_TABLE", ns)
  assign("PARAMETRES_CLE_TABLE", c("etapes/exposition/convention", "etapes/brut/estimateurs"), envir = ns)
  on.exit({ assign("PARAMETRES_CLE_TABLE", ancien, envir = ns); lockBinding("PARAMETRES_CLE_TABLE", ns) })
  p <- PLAN; p$etapes$brut$estimateurs <- "poisson"
  tb <- importee(transform(TR, qx = TR$qx * 2), nom = "B")
  tabs <- list(poisson = tb, plus_fine = T1)
  x <- construire_table(BASE, ROLES, p, tables_reference = tabs)
  f <- lire_fiche(ecrire_fiche(x$fiche))
  r <- rejouer_fiche(BASE, f, tabs)
  verifier("E injection de bout en bout : fiche aux clés triées (plus_fine, poisson), rejeu identical() hors execution, ligne 8 « (reçue) »",
           identical(names(x$fiche$tables_reference), c("plus_fine", "poisson")) &&
             identical(sans_execution(r$construction), sans_execution(x)) &&
             identical(r$bilan$lu[8L], sprintf("plus_fine : %s (reçue) ; poisson : %s (reçue)",
                                               empreinte_table(T1), empreinte_table(tb))) &&
             identical(r$bilan$verdict[1:8], rep("conforme", 8L)))
  f2 <- f; f2$tables_reference <- f$tables_reference[c("poisson", "plus_fine")]
  verifier("E contrôle 2 : clés de fiche$tables_reference hors de l'ordre par octets -> fiche_non_conforme (pas d'erreur R)", {
    r2 <- tryCatch(rejouer_fiche(BASE, f2, tabs), error = function(e) e)
    if (!inherits(r2, "refus_tablexp")) sprintf("condition %s : %s", class(r2)[1L], conditionMessage(r2))
    else controle_refus(r2, "construire_table", "fiche_non_conforme",
                        "clés « poisson », « plus_fine » hors de l'ordre par octets « plus_fine », « poisson »",
                        FORME_ATTENDUE)
  })
})
ns_cle <- ns$PARAMETRES_CLE_TABLE
verifier("E injection de bout en bout : PARAMETRES_CLE_TABLE restauré et verrouillé",
         identical(ns_cle, "etapes/validation/table_reference") && bindingIsLocked("PARAMETRES_CLE_TABLE", ns))
verifier("E injection : origine « relue dans le package » d'une table embarquée, liste assemblée et textes de la ligne 8", {
  el <- element_table_fiche(T_EMB)
  r <- controler_tables_rejeu(list(t1 = el), NULL, "code", list(TABLE_E = T_EMB))
  identical(r$tables, list(t1 = T_EMB)) && identical(unname(r$empreintes), el$empreinte) &&
    identical(r$attendu, paste0("t1 : ", el$empreinte)) && identical(r$lu, paste0("t1 : ", el$empreinte, " (relue dans le package)"))
})
verifier("E injection : origine « reçue », deux tables dans l'ordre de la fiche, textes joints par « ; »", {
  r <- controler_tables_rejeu(CHAMP_AB, list(b = T1, a = T_SYNTH), "code")
  identical(names(r$tables), c("a", "b")) &&
    identical(r$lu, sprintf("a : %s (reçue) ; b : %s (reçue)", CHAMP_AB$a$empreinte, CHAMP_AB$b$empreinte))
})
# Le verdict cite le chemin sous la fiche, sans préfixe « fiche/ » (comme
# « différente : convention », tests/unitaires/test_rejeu.R, T11) ; le rejeu
# de bout en bout avec une table est dans tests/unitaires/test_validation.R
# (M247).
verifier("E témoin « fiche » : nom de table changé -> « différente : tables_reference/t1/nom » (verdict du témoin, sans refus)", {
  f2 <- fiche_t1(replace(EL, "nom", list("autre nom")))
  identical(tablexp:::verdict_chemins(tablexp:::perimetre_temoin_fiche(F_T1), tablexp:::perimetre_temoin_fiche(f2), "fiche"),
            "différente : tables_reference/t1/nom")
})

# --- F. Contrôles mécaniques -----------------------------------------------------

verifier("F PARAMETRES_CLE_TABLE : chaque chemin désigne un paramètre character de PARAMETRES_ETAPES (etapes/<étape>/<paramètre>)", {
  p <- tablexp:::PARAMETRES_CLE_TABLE
  ok <- vapply(strsplit(p, "/", fixed = TRUE), function(s) {
    length(s) == 3L && s[1L] == "etapes" && !is.null(tablexp:::PARAMETRES_ETAPES[[s[2L]]]) && {
      cat_ <- tablexp:::PARAMETRES_ETAPES[[s[2L]]]
      identical(cat_$type[match(s[3L], cat_$parametre)], "character")
    }
  }, NA)
  is.character(p) && all(ok)
})
verifier("F graphe d'appels : aucune arête de donnees vers rapport (empreinte_table() hors de donnees)", {
  g <- new.env()
  sys.source(file.path(RACINE, "tests", "controles", "graphe_modules.R"), envir = g)
  a <- g$analyser_modules(file.path(RACINE, "R"))
  !length(a$defauts) && !any(a$aretes$de == "donnees" & a$aretes$vers == "rapport")
})
