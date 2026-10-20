# -----------------------------------------------------------------------------
#  tests/unitaires/test_serialisation.R : sérialisation du plan et de la fiche
#  de construction, texte en octets <-> objet (issue #50 ; ADR 0011, points 6,
#  8, 9 et 10 ; ADR 0007, point 12.2 (a) à (e))
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité exacte : aller-retour lire(ecrire(x)) identical() à x ; rejeu
#    construire_table() sur la fiche relue identical() hors `execution` ;
#  - valeur calculée à la main : codes de refus, lignes du texte écrites à la
#    main (forme attendue d'une ligne, texte importé) ;
#  - constat de mesure : relecture d'un double écrit sous Windows (ADR 0011,
#    point 6.3, Q3), valeur notée dans l'ADR 0011, contexte, mesure 1.
# -----------------------------------------------------------------------------

D <- as.Date
# Octets des lignes reçues, sans conversion : littéraux des tests marqués UTF-8
# (parse(encoding = "UTF-8") du lanceur), lignes relues marquées par lignes_de() ;
# jamais enc2utf8(), qui change en « <c3><a9> » une chaîne non marquée sous la
# locale C (M54 : la locale d'exécution reste libre).
octets <- function(...) charToRaw(paste0(paste(c(...), collapse = "\n"), "\n"))
# Lignes d'un texte en octets, marquées UTF-8 (comparables aux littéraux en
# toute locale).
lignes_de <- function(o) { l <- strsplit(rawToChar(o), "\n", fixed = TRUE)[[1L]]; Encoding(l) <- "UTF-8"; l }
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
aller_retour_plan <- function(x) lire_plan(ecrire_plan(x))
aller_retour_fiche <- function(x) lire_fiche(ecrire_fiche(x))
sans_execution_ <- function(x) { x$execution <- NULL; x }

BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
plan_complet <- function() list(
  transition = list(depart = "vivant", arrivee = "deces"),
  periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0, segmentation = "sexe",
  etapes = list(donnees = list(exclusions = character(0)),
                exposition = list(convention = "plus_fine", par_annee = FALSE),
                brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95)))

# --- exports et forme des octets (valeur calculée à la main) --------------------

verifier("ecrire_plan, lire_plan, ecrire_fiche, lire_fiche : exportées, un argument chacune",
         all(c("ecrire_plan", "lire_plan", "ecrire_fiche", "lire_fiche") %in% getNamespaceExports("tablexp")) &&
           identical(names(formals(ecrire_plan)), "plan") && identical(names(formals(lire_plan)), "octets") &&
           identical(names(formals(ecrire_fiche)), "fiche") && identical(names(formals(lire_fiche)), "octets"))
local({
  o <- ecrire_plan(plan_complet())
  verifier("ecrire_plan : octets (raw), UTF-8 valide, LF final, aucun CR, aucune marque d'ordre des octets",
           is.raw(o) && validUTF8(rawToChar(o)) && o[length(o)] == as.raw(0x0a) && !any(o == as.raw(0x0d)) &&
             !identical(o[1:3], as.raw(c(0xef, 0xbb, 0xbf))))
  l <- lignes_de(o)
  verifier("ecrire_plan : première ligne « tablexp plan 1 », puis en-tête « # clé : valeur », puis corps",
           identical(l[1L], "tablexp plan 1") && startsWith(l[2L], "# ") &&
             identical(l[match(FALSE, startsWith(l[-1L], "#")) + 1L], "@noms\tcharacter\t5\t\"transition\"\t\"periode\"\t\"delai_declaration\"\t\"segmentation\"\t\"etapes\""))
  verifier("ecrire_plan : lignes écrites à la main (Date ISO, double %.17g, character vide, logique)",
           all(c("plan/periode/debut\tDate\t1\t2015-01-01", "periode/debut\tDate\t1\t2015-01-01",
                 "etapes/brut/niveau\tdouble\t1\t0.94999999999999996", "etapes/donnees/exclusions\tcharacter\t0",
                 "etapes/exposition/par_annee\tlogical\t1\tFALSE") %in% c(l, paste0("plan/", l))))
  verifier("ecrire_fiche : première ligne « tablexp fiche 1 », distincte de celle du plan",
           identical(strsplit(rawToChar(ecrire_fiche(list(version_fiche = 2L))), "\n")[[1L]][1L], "tablexp fiche 1"))
  verifier("ecrire_plan : l'en-tête est hors aller-retour (lignes # retirées ou modifiées : même objet relu)",
           identical(lire_plan(octets(l[!startsWith(l, "#")])), plan_complet()) &&
             identical(lire_plan(octets(c(l[1L], "# commentaire libre : é", l[!startsWith(l, "#")][-1L]))), plan_complet()))
})

# --- aller-retour identical(), cas adverses (identité exacte) -------------------

adverse <- list(
  na_et_chaine_na = c(NA, "NA", "", " NA", "\"NA\""),
  non_ascii = c("é", "naïveté", "日本語", "\U0001F600", "Ω≈ç√"),
  latin1 = iconv("été", "UTF-8", "latin1"),
  echappements = c("a\\b", "\\n littéral", "ligne\nsuivante", "tab\tulation", "retour\rchariot", "guillemet \" droit",
                   "\\\\\"", "\\", "\"", "fin\\"),
  controles = "\001\037\177",
  doubles = c(.Machine$double.xmax, -.Machine$double.xmax, .Machine$double.xmin, 5e-324, -5e-324,
              2.2250738585072009e-308, 4.9406564584124654e-320, 1e-300, 1e300, 0.1, 1 / 3, pi, -0, 0,
              Inf, -Inf, NaN, NA, 0 / 0, NA_real_ + 1, 4.8005694404694591e-06, 0.95, 1e-9, 123456789012345678),
  entiers = c(.Machine$integer.max, -.Machine$integer.max, 0L, NA),
  logiques = c(TRUE, FALSE, NA),
  dates = structure(c(-719162, 2932896, NA, NaN, 0, 19782), class = "Date"),
  vides = list(character(0), integer(0), double(0), logical(0), structure(double(0), class = "Date")),
  # Vecteur atomique nommé de longueur 0 : refusé à l'écriture (revue finale
  # de #50), voir les refus de l'écriture.
  nommes_vides = list(setNames(list(), character(0)), list()),
  null = list(a = NULL, b = list(NULL), c = list()),
  nommes = list(v = c(a = 1, b = 2), w = c(x = "é", "sans nom"), z = c("NA" = NA, "a/b" = TRUE)),
  noms_inadmissibles = list(1, "a/b" = 2, "@noms" = 3, "#x" = 4, "[1]" = 5, d = 6, d = 7, "tab\tnom" = 8,
                            "é" = 9, "lf\nnom" = 10),
  noms_na = setNames(list(1, 2), c("a", NA)),
  noms_vides_tous = setNames(list(1, 2), c("", "")),
  sans_noms = list(list(1, list(2, "x")), list()),
  permutes = list(z = 1, a = 2, m = list(fin = D("2024-12-31"), debut = D("2015-01-01"))),
  tableaux = list(vide = data.frame(motif = character(0), nombre = integer(0), stringsAsFactors = FALSE),
                  plein = data.frame(etape = c("donnees", NA), nombre = c(1L, NA), x = c(NaN, -Inf),
                                     d = D(c("0001-01-01", NA)), l = c(NA, TRUE), stringsAsFactors = FALSE),
                  sans_colonne = data.frame()))
for (n in names(adverse)) {
  x <- list(valeur = adverse[[n]])
  verifier(sprintf("aller-retour lire_plan(ecrire_plan(x)) identical() : %s", n), identical(aller_retour_plan(x), x))
}
verifier("aller-retour : objet adverse entier (toutes les valeurs ci-dessus dans une liste) identical(), plan et fiche",
         identical(aller_retour_plan(adverse), adverse) && identical(aller_retour_fiche(adverse), adverse))
verifier("aller-retour : NA et la chaîne « NA » restent distincts ; NA, NaN distincts (double et Date)", {
  y <- aller_retour_plan(adverse)
  identical(is.na(y$na_et_chaine_na), c(TRUE, FALSE, FALSE, FALSE, FALSE)) &&
    identical(is.nan(y$doubles), is.nan(adverse$doubles)) && is.na(y$dates[3L]) && !is.nan(unclass(y$dates)[3L]) &&
    is.nan(unclass(y$dates)[4L])
})
verifier("aller-retour : noms permutés conservés tels qu'écrits (ni tri, ni ordre du schéma)", {
  p <- plan_complet()[c("etapes", "segmentation", "periode", "transition", "delai_declaration")]
  p$periode <- p$periode[c("fin", "debut")]
  y <- aller_retour_plan(p)
  identical(y, p) && identical(names(y), names(p)) && identical(names(y$periode), c("fin", "debut"))
})
verifier("aller-retour : stockage des doubles entiers et des integer conservé (1 et 1L distincts)", {
  y <- aller_retour_plan(list(a = 1, b = 1L, c = D("2020-01-01")))
  is.double(y$a) && is.integer(y$b) && is.double(unclass(y$c))
})

# --- un texte qui contient du code est une donnée ou un refus (identité exacte) --

local({
  temoin <- file.path(tempdir(), "temoin_serialisation_50")
  unlink(temoin)
  code <- sprintf("system(\"touch %s\")", temoin)
  # Chemin sans « / » (sinon le segment est [i]) : témoin relatif au dossier
  # temporaire, dossier de travail du bloc.
  ancien <- setwd(tempdir()); on.exit(setwd(ancien))
  unlink("temoin_serialisation_50b")
  code_relatif <- "system(\"touch temoin_serialisation_50b\")"
  x <- list(valeur = code, `system("touch x")` = c(code, NA))
  names(x)[2L] <- code
  y <- aller_retour_plan(x)
  verifier("code R en valeur et en nom : relu comme chaîne, identical(), jamais évalué (témoin non créé)",
           identical(y, x) && !file.exists(temoin))
  pieges <- list(
    valeur_nue = c("tablexp plan 1", "@noms\tcharacter\t1\t\"a\"", sprintf("a\tcharacter\t1\t%s", code)),
    double_code = c("tablexp plan 1", "@noms\tcharacter\t1\t\"a\"", sprintf("a\tdouble\t1\t%s", code)),
    chemin_code = c("tablexp plan 1", sprintf("@noms\tcharacter\t1\t\"%s\"", gsub("\"", "\\\"", code_relatif, fixed = TRUE)),
                    sprintf("%s\tinteger\t1\t1", code_relatif)),
    chemin_barre = c("tablexp plan 1", sprintf("@noms\tcharacter\t1\t\"%s\"", gsub("\"", "\\\"", code, fixed = TRUE)),
                     sprintf("%s\tinteger\t1\t1", code)),
    type_code = c("tablexp plan 1", "@noms\tcharacter\t1\t\"a\"", sprintf("a\t%s\t1\t1", code)))
  r <- lapply(pieges, function(t) refus_de(lire_plan(octets(t))))
  verifier("code R hors guillemets (valeur, double, type) : refus nommé, témoin non créé",
           identical(vapply(r[c("valeur_nue", "double_code", "type_code")], function(e) e$motif, ""),
                     c(valeur_nue = "texte_valeur_invalide", double_code = "texte_valeur_invalide",
                       type_code = "texte_type_inconnu")) && !file.exists(temoin))
  verifier("chemin piégé (code R comme nom, déclaré dans @noms) : lu comme donnée, témoin non créé",
           is.null(r$chemin_code) &&
             identical(lire_plan(octets(pieges$chemin_code)), setNames(list(1L), code_relatif)) &&
             !file.exists("temoin_serialisation_50b"))
  verifier("chemin piégé contenant « / » (segment attendu [1]) : refus texte_chemin_inattendu, témoin non créé",
           identical(r$chemin_barre$motif, "texte_chemin_inattendu") && !file.exists(temoin))
  corps <- c(body(tablexp:::lire_texte), body(tablexp:::valeurs_texte), body(lire_plan), body(lire_fiche))
  verifier("lecteurs : aucun appel à parse, eval, evalq, source, dget, str2lang, str2expression, do.call, match.fun, get",
           !any(c("parse", "eval", "evalq", "source", "dget", "str2lang", "str2expression", "do.call", "match.fun", "get") %in%
                  unlist(lapply(corps, all.names))))
})

# --- refus nommés de la lecture (valeur calculée à la main : codes) --------------

local({
  ok <- c("tablexp plan 1", "@noms\tcharacter\t2\t\"a\"\t\"b\"", "a\tdouble\t1\t1", "b\tcharacter\t1\t\"x\"")
  verifier("texte de référence des refus : lu sans refus", identical(lire_plan(octets(ok)), list(a = 1, b = "x")))
  remplacer <- function(i, ligne) { t <- ok; t[i] <- ligne; t }
  cas <- list(
    list("version inconnue (tablexp plan 2)", "texte_version_inconnue", octets(remplacer(1L, "tablexp plan 2"))),
    list("fiche lue par lire_plan", "texte_version_inconnue", octets(remplacer(1L, "tablexp fiche 1"))),
    list("première ligne absente (texte vide)", "texte_version_inconnue", raw(0)),
    list("CR (fins de ligne CRLF)", "texte_retour_chariot", charToRaw(paste0(paste(ok, collapse = "\r\n"), "\r\n"))),
    list("CR isolé dans une valeur", "texte_retour_chariot", octets(remplacer(4L, "b\tcharacter\t1\t\"x\ry\""))),
    list("UTF-8 invalide (octet 0xE9 latin-1)", "texte_utf8_invalide",
         c(octets(ok[1:3]), charToRaw("b\tcharacter\t1\t\""), as.raw(0xe9), charToRaw("\"\n"))),
    list("marque d'ordre des octets", "texte_marque_ordre_octets", c(as.raw(c(0xef, 0xbb, 0xbf)), octets(ok))),
    list("octet nul", "texte_octet_nul", c(octets(ok[1:3]), charToRaw("b\tcharacter\t1\t\""), as.raw(0), charToRaw("\"\n"))),
    list("sans LF final", "texte_sans_lf_final", utils::head(octets(ok), -1L)),
    list("chemin en double", "texte_chemin_en_double", octets(c(ok, "b\tcharacter\t1\t\"y\""))),
    list("type inconnu (complex)", "texte_type_inconnu", octets(remplacer(3L, "a\tcomplex\t1\t1"))),
    list("type factor (hors version 1)", "texte_type_inconnu", octets(remplacer(4L, "b\tfactor\t1\t\"x\""))),
    list("longueur incohérente (2 déclarées, 1 valeur)", "texte_longueur_incoherente", octets(remplacer(3L, "a\tdouble\t2\t1"))),
    list("longueur incohérente (@noms de 2, 1 élément)", "texte_chemin_inattendu", octets(ok[1:3])),
    list("longueur incohérente (NULL de longueur 1)", "texte_longueur_incoherente", octets(remplacer(3L, "a\tNULL\t1\t1"))),
    list("ligne mal formée (deux champs)", "texte_ligne_mal_formee", octets(remplacer(3L, "a\tdouble"))),
    list("ligne mal formée (champ vide)", "texte_ligne_mal_formee", octets(remplacer(3L, "a\tdouble\t1\t1\t"))),
    list("ligne mal formée (longueur illisible)", "texte_ligne_mal_formee", octets(remplacer(3L, "a\tdouble\tun\t1"))),
    list("ligne mal formée (ligne vide)", "texte_ligne_mal_formee", octets(c(ok[1:2], "", ok[3:4]))),
    list("ligne mal formée (en-tête dans le corps)", "texte_ligne_mal_formee", octets(c(ok[1:3], "# x : y", ok[4L]))),
    list("liste sans @noms", "texte_chemin_inattendu", octets(ok[-2L])),
    list("chemin inattendu (enfant manquant)", "texte_chemin_inattendu", octets(remplacer(4L, "c\tcharacter\t1\t\"x\""))),
    list("chemin en trop après l'objet", "texte_chemin_inattendu", octets(c(ok, "c\tdouble\t1\t1"))),
    list("data.frame sans @lignes", "texte_chemin_inattendu",
         octets(c(ok[1:3], "b/@colonnes\tcharacter\t1\t\"u\"", "b/u\tinteger\t1\t1"))),
    list("data.frame, colonne de mauvaise longueur", "texte_longueur_incoherente",
         octets(c(ok[1:3], "b/@colonnes\tcharacter\t1\t\"u\"", "b/@lignes\tinteger\t1\t2", "b/u\tinteger\t1\t1"))),
    list("échappement inconnu (\\x)", "texte_valeur_invalide", octets(remplacer(4L, "b\tcharacter\t1\t\"a\\xb\""))),
    list("guillemet nu dans une chaîne", "texte_valeur_invalide", octets(remplacer(4L, "b\tcharacter\t1\t\"a\"b\""))),
    list("chaîne sans guillemets", "texte_valeur_invalide", octets(remplacer(4L, "b\tcharacter\t1\tx"))),
    list("jeton inconnu (nan)", "texte_valeur_invalide", octets(remplacer(3L, "a\tdouble\t1\tnan"))),
    list("jeton inconnu (+Inf)", "texte_valeur_invalide", octets(remplacer(3L, "a\tdouble\t1\t+Inf"))),
    list("double hexadécimal (0x1p-1)", "texte_valeur_invalide", octets(remplacer(3L, "a\tdouble\t1\t0x1p-1"))),
    list("double avec espace", "texte_valeur_invalide", octets(remplacer(3L, "a\tdouble\t1\t 0.5"))),
    list("double en dépassement (1e999)", "texte_valeur_invalide", octets(remplacer(3L, "a\tdouble\t1\t1e999"))),
    list("entier non canonique (007)", "texte_valeur_invalide", octets(remplacer(3L, "a\tinteger\t1\t007"))),
    list("entier hors étendue (2147483648)", "texte_valeur_invalide", octets(remplacer(3L, "a\tinteger\t1\t2147483648"))),
    list("logique T", "texte_valeur_invalide", octets(remplacer(3L, "a\tlogical\t1\tT"))),
    list("date sans zéros (999-01-01)", "texte_valeur_invalide", octets(remplacer(3L, "a\tDate\t1\t999-01-01"))),
    list("date inexistante (2023-02-30)", "texte_valeur_invalide", octets(remplacer(3L, "a\tDate\t1\t2023-02-30"))),
    list("date hors domaine (0000-12-31)", "texte_valeur_invalide", octets(remplacer(3L, "a\tDate\t1\t0000-12-31"))),
    list("date non ISO (01/02/2020)", "texte_valeur_invalide", octets(remplacer(3L, "a\tDate\t1\t01/02/2020"))),
    list("@noms de type double", "texte_ligne_mal_formee", octets(remplacer(2L, "@noms\tdouble\t1\t1"))),
    list("racine data.frame", "texte_chemin_inattendu",
         octets(c(ok[1L], "@colonnes\tcharacter\t1\t\"a\"", "@lignes\tinteger\t1\t1", "a\tdouble\t1\t1"))))
  for (c_ in cas) {
    r <- refus_de(lire_plan(c_[[3L]]))
    verifier(sprintf("lire_plan, refus : %s -> %s", c_[[1L]], c_[[2L]]),
             if (is.null(r)) "aucun refus" else if (!identical(r$motif, c_[[2L]])) sprintf("motif %s", r$motif)
             else identical(r$module, "rapport"))
  }
  verifier("lire_plan, refus : texte reçu en character -> texte_non_octets", {
    r <- refus_de(lire_plan(paste(ok, collapse = "\n")))
    !is.null(r) && identical(r$motif, "texte_non_octets")
  })
  verifier("lire_plan, refus : le numéro de ligne est dans details$ligne (CR en ligne 4, chemin en double en ligne 5)",
           identical(refus_de(lire_plan(octets(remplacer(4L, "b\tcharacter\t1\t\"x\ry\""))))$details$ligne, 4L) &&
             identical(refus_de(lire_plan(octets(c(ok, "b\tcharacter\t1\t\"y\""))))$details$ligne, 5L))
  verifier("lire_fiche, refus : texte de plan lu par lire_fiche -> texte_version_inconnue",
           identical(refus_de(lire_fiche(octets(ok)))$motif, "texte_version_inconnue"))
})

# --- refus nommés de l'écriture (valeur calculée à la main : codes) --------------

local({
  cas <- list(
    list("facteur", "serialisation_impossible", list(a = factor("x"))),
    list("fonction", "serialisation_impossible", list(a = identity)),
    list("environnement", "serialisation_impossible", list(a = globalenv())),
    list("matrice (attribut dim)", "serialisation_impossible", list(a = matrix(1:4, 2))),
    list("POSIXct", "serialisation_impossible", list(a = as.POSIXct("2020-01-01", tz = "UTC"))),
    list("liste de classe", "serialisation_impossible", list(a = structure(list(1), class = "x"))),
    list("data.frame aux noms de lignes", "serialisation_impossible", list(a = data.frame(u = 1:2, row.names = c("p", "q")))),
    list("data.frame à colonne liste", "serialisation_impossible", list(a = data.frame(u = I(list(1, 2))))),
    list("racine non liste", "serialisation_impossible", c(a = 1)),
    list("racine data.frame", "serialisation_impossible", data.frame(a = 1)),
    list("date hors domaine", "date_hors_domaine", list(a = structure(-719163, class = "Date"))),
    list("date non entière", "date_non_entiere", list(a = structure(0.5, class = "Date"))),
    list("chaîne au codage bytes", "chaine_codage_bytes", list(a = `Encoding<-`("\xe9", "bytes"))),
    list("Date à stockage character", "serialisation_impossible", list(d = structure("2020-01-01", class = "Date"))),
    list("Date à stockage logical", "serialisation_impossible", list(d = structure(TRUE, class = "Date"))),
    list("Date à stockage character, colonne de data.frame", "serialisation_impossible",
         list(t = structure(list(d = structure("2020-01-01", class = "Date")), class = "data.frame", row.names = 1L))),
    list("vecteur atomique nommé de longueur 0 (character)", "serialisation_impossible",
         list(a = 1, v = setNames(character(0), character(0)))),
    list("vecteur atomique nommé de longueur 0 (double, cas de l'audit)", "serialisation_impossible",
         list(a = 1, v = setNames(numeric(0), character(0)))),
    list("Date nommée de longueur 0", "serialisation_impossible",
         list(v = setNames(structure(double(0), class = "Date"), character(0)))))
  for (c_ in cas) {
    r <- refus_de(ecrire_plan(c_[[3L]]))
    verifier(sprintf("ecrire_plan, refus : %s -> %s", c_[[1L]], c_[[2L]]),
             if (is.null(r)) "aucun refus" else identical(r$motif, c_[[2L]]) || sprintf("motif %s", r$motif))
  }
  verifier("ecrire_fiche, refus : objet construction entier (au lieu de x$fiche) -> serialisation_impossible",
           identical(refus_de(ecrire_fiche(structure(list(fiche = list()), class = "construction")))$motif,
                     "serialisation_impossible"))
  verifier("ecrire_plan, refus : le chemin fautif est dans details$chemin",
           identical(refus_de(ecrire_plan(list(b = list(c = factor("x")))))$details$chemin, "b/c"))
  verifier("ecrire_plan, refus : vecteur atomique nommé de longueur 0, chemin cité ; le même vecteur sans noms est écrit",
           identical(refus_de(ecrire_plan(list(a = 1, v = setNames(numeric(0), character(0)))))$details$chemin, "v") &&
             identical(aller_retour_plan(list(a = 1, v = numeric(0))), list(a = 1, v = numeric(0))))
})

# --- doubles relus par as.numeric (Q3, « lecteur de forme ») ---------------------

local({
  p <- plan_complet()
  p$etapes$brut$niveau <- 4.8005694404694591e-06
  t <- rawToChar(ecrire_plan(p))
  verifier("Q3 : le niveau 4.8005694404694591e-06 s'écrit en %.17g (forme glibc)",
           grepl("etapes/brut/niveau\tdouble\t1\t4.8005694404694591e-06\n", t, fixed = TRUE))
  w <- sub("4.8005694404694591e-06", "4.800569440469459e-06", t, fixed = TRUE)
  verifier("Q3 (constat de mesure) : 4.800569440469459e-06 (forme Windows) se relit comme 4.8005694404694591e-06, même double",
           identical(lire_plan(charToRaw(w)), p) &&
             identical(lire_plan(charToRaw(w))$etapes$brut$niveau, as.numeric("4.8005694404694591e-06")))
  m <- sub("0.94999999999999996", "0.95", rawToChar(ecrire_plan(plan_complet())), fixed = TRUE)
  verifier("Q3 : plan édité à la main (niveau 0.95, 9.5e-1, +.95) relu identical() au plan d'origine",
           all(vapply(c("0.95", "9.5e-1", "+.95", "0.950"), function(s)
             identical(lire_plan(charToRaw(sub("\t0.95\n", paste0("\t", s, "\n"), m, fixed = TRUE))), plan_complet()), NA)))
})

# --- plan importé : valider_plan(), mêmes refus qu'en mémoire (identité exacte) ---

local({
  p <- plan_complet()
  verifier("plan importé valide : valider_plan(lire_plan(ecrire_plan(p))) passe et rend le plan",
           identical(valider_plan(aller_retour_plan(p)), p))
  mauvais <- list(
    champ_inconnu = c(p, list(unite_observation = "tete")),
    etapes_hors_ordre = within(p, etapes <- etapes[c("exposition", "donnees", "brut")]),
    convention = within(p, etapes$exposition$convention <- "inconnue"),
    niveau = within(p, etapes$brut$niveau <- 2),
    niveau_integer = within(p, etapes$brut$niveau <- 1L),
    delai = within(p, delai_declaration <- -1),
    transition = within(p, transition$arrivee <- "rachat"),
    periode = within(p, periode$fin <- D("2010-01-01")),
    graine = c(p, list(graine = 1L)),
    parametre_manquant = within(p, etapes$brut$niveau <- NULL),
    exclusions_na = within(p, etapes$donnees$exclusions <- NA_character_))
  for (n in names(mauvais)) {
    en_memoire <- refus_de(valider_plan(mauvais[[n]]))
    importe <- refus_de(valider_plan(aller_retour_plan(mauvais[[n]])))
    verifier(sprintf("plan importé, %s : même refus que le plan en mémoire (motif et module)", n),
             !is.null(en_memoire) && !is.null(importe) && identical(importe$motif, en_memoire$motif) &&
               identical(importe$module, en_memoire$module) || sprintf("%s / %s", en_memoire$motif, importe$motif))
  }
  r <- refus_de(construire_table(BASE, ROLES, aller_retour_plan(mauvais$convention)))
  verifier("plan importé invalide : construire_table() le refuse comme en mémoire (valider_plan à l'entrée)",
           !is.null(r) && identical(r$motif, refus_de(valider_plan(mauvais$convention))$motif))
})

# --- rejeu depuis la fiche écrite (identité exacte, E3.7) --------------------------

local({
  pr <- list(algorithme = "md5", fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef")
  for (cas in list(list("avec brut et Kaplan-Meier, sans provenance", plan_complet(), NULL),
                   list("sans exposition ni brut, avec provenance", within(plan_complet(), etapes <- etapes["donnees"]), pr))) {
    x <- construire_table(BASE, ROLES, cas[[2L]], cas[[3L]])
    f <- tempfile(fileext = ".txt")
    writeBin(ecrire_fiche(x$fiche), f)
    lue <- lire_fiche(readBin(f, "raw", file.size(f)))
    unlink(f)
    verifier(sprintf("fiche (%s) : lire_fiche(ecrire_fiche()) par un fichier identical() à la fiche", cas[[1L]]),
             identical(lue, x$fiche))
    verifier(sprintf("rejeu (%s) : construire_table(donnees, f$roles, f$plan, f$provenance) identical() hors execution",
                     cas[[1L]]),
             identical(sans_execution_(construire_table(BASE, lue$roles, lue$plan, lue$provenance)), sans_execution_(x)))
  }
  verifier("aucun champ ajouté à l'objet construction ni à la fiche par l'écriture (même objet avant et après)", {
    x <- construire_table(BASE, ROLES, plan_complet())
    avant <- x
    ecrire_fiche(x$fiche)
    identical(x, avant) && !any(grepl("tablexp (plan|fiche)", unlist(x$fiche), fixed = FALSE))
  })
})

# --- reprise de #50 (ADR 0011, troisième annotation) ------------------------------

# C1 (d) : liste sans noms déclarée par @longueur ; tout texte tronqué refusé
# (valeur calculée à la main : codes ; identité exacte : aller-retour).
local({
  x <- list(a = 1, b = list(10, 20, 30))
  t <- lignes_de(ecrire_plan(x))
  verifier("C1 : liste sans noms écrite par b/@longueur integer 1 3 puis b/[1] à b/[3], sans ligne @noms",
           "b/@longueur\tinteger\t1\t3" %in% t && all(sprintf("b/[%d]\tdouble\t1\t%d0", 1:3, 1:3) %in% t) &&
             !any(startsWith(t, "b/@noms")))
  r <- refus_de(lire_plan(octets(t[t != "b/[3]\tdouble\t1\t30"])))
  # b est le dernier élément : 3 déclarés pour 2 lignes restantes, refus de
  # la borne de l'effectif (correctif R1 d'audit), avant toute lecture.
  verifier("C1 (cas de l'audit) : texte sans la ligne b/[3] -> texte_longueur_incoherente (effectif au-delà du reste)",
           identical(r$motif, "texte_longueur_incoherente"))
  verifier("C1 : élément manquant suivi d'une autre ligne (a/[2] absent, b présent) -> texte_chemin_inattendu",
           identical(refus_de(lire_plan(octets(c("tablexp plan 1", "@noms\tcharacter\t2\t\"a\"\t\"b\"",
                                                 "a/@longueur\tinteger\t1\t2", "a/[1]\tdouble\t1\t1",
                                                 "b\tdouble\t1\t2"))))$motif, "texte_chemin_inattendu"))
  tronque <- function(o, nom) {
    l <- lignes_de(o)
    entete <- l[seq_len(match(FALSE, startsWith(l[-1L], "#")))]
    corps <- l[-seq_along(entete)]
    acceptes <- Filter(function(m) is.null(refus_de(lire_plan(octets(c(entete, corps[seq_len(m)]))))),
                       0:(length(corps) - 1L))
    verifier(sprintf("C1 : chacun des %d préfixes stricts du corps (%s), coupé à une frontière de ligne, refusé",
                     length(corps), nom),
             !length(acceptes) || sprintf("préfixe(s) accepté(s) : %s lignes", paste(acceptes, collapse = ", ")))
  }
  y <- list(a = list(list(), list(1, list(2, list())), list()), b = list(), c = list(list(list())))
  verifier("C1 : objet adverse (listes sans noms imbriquées, vides, en fin d'objet) : aller-retour identical()",
           identical(aller_retour_plan(y), y))
  tronque(ecrire_plan(y), "listes sans noms imbriquées")
  tronque(ecrire_plan(adverse), "objet adverse entier")
  base <- c("tablexp plan 1", "@noms\tcharacter\t1\t\"b\"")
  cas <- list(list("@longueur de type double", "b/@longueur\tdouble\t1\t1"),
              list("@longueur de longueur 2", "b/@longueur\tinteger\t2\t1\t1"),
              list("@longueur NA", "b/@longueur\tinteger\t1\tNA"),
              list("@longueur -1", "b/@longueur\tinteger\t1\t-1"))
  for (c_ in cas)
    verifier(sprintf("C1 : %s -> texte_ligne_mal_formee", c_[[1L]]),
             identical(refus_de(lire_plan(octets(c(base, c_[[2L]], "b/[1]\tdouble\t1\t1"))))$motif, "texte_ligne_mal_formee"))
  verifier("C1 : élément b/[2] en trop pour @longueur 1 -> texte_chemin_inattendu",
           identical(refus_de(lire_plan(octets(c(base, "b/@longueur\tinteger\t1\t1", "b/[1]\tdouble\t1\t1",
                                                 "b/[2]\tdouble\t1\t2"))))$motif, "texte_chemin_inattendu"))
  verifier("C1 : ligne @longueur écrite à la main lue (list(b = list(1, \"x\")), racine sans noms list(1))",
           identical(lire_plan(octets(c(base, "b/@longueur\tinteger\t1\t2", "b/[1]\tdouble\t1\t1", "b/[2]\tcharacter\t1\t\"x\""))),
                     list(b = list(1, "x"))) &&
             identical(lire_plan(octets(c("tablexp plan 1", "@longueur\tinteger\t1\t1", "[1]\tdouble\t1\t1"))), list(1)))
  verifier("C2 : ligne @noms de type NULL (forme abandonnée) -> texte_ligne_mal_formee",
           identical(refus_de(lire_plan(octets(c(base, "b/@noms\tNULL\t0"))))$motif, "texte_ligne_mal_formee"))
})

# C2 (e) : list() et liste nommée vide distinguées (identité exacte).
local({
  a <- list(v = list()); b <- list(v = setNames(list(), character(0)))
  ta <- rawToChar(ecrire_plan(a)); tb <- rawToChar(ecrire_plan(b))
  verifier("C2 : list() et setNames(list(), character(0)) : aller-retour identical() chacun",
           identical(aller_retour_plan(a), a) && identical(aller_retour_plan(b), b) && !identical(a, b))
  verifier("C2 : leurs textes diffèrent par v/@longueur integer 1 0 contre v/@noms character 0",
           grepl("\nv/@longueur\tinteger\t1\t0\n", ta, fixed = TRUE) && grepl("\nv/@noms\tcharacter\t0\n", tb, fixed = TRUE) &&
             !grepl("v/@noms", ta, fixed = TRUE) && !grepl("v/@longueur", tb, fixed = TRUE))
})

# C3 (g) : profondeur au plus 32 (valeur calculée à la main : codes, lignes).
local({
  imbrique <- function(d) { x <- 1; for (i in seq_len(d)) x <- list(a = x); x }
  verifier("C3 : PROFONDEUR_MAX_TEXTE vaut 32", identical(tablexp:::PROFONDEUR_MAX_TEXTE, 32L))
  verifier("C3 : profondeur 32 (feuille a/…/a de 32 segments) : aller-retour identical()",
           identical(aller_retour_plan(imbrique(32L)), imbrique(32L)))
  r <- refus_de(ecrire_plan(imbrique(33L)))
  verifier("C3 : profondeur 33 refusée à l'écriture (serialisation_impossible, chemin de 33 segments)",
           identical(r$motif, "serialisation_impossible") && identical(r$details$chemin, paste(rep("a", 33L), collapse = "/")))
  a_ <- function(i) paste(rep("a", i), collapse = "/")
  texte <- function(d) octets(c("tablexp plan 1", "@noms\tcharacter\t1\t\"a\"",
                                vapply(seq_len(d - 1L), function(i) sprintf("%s/@noms\tcharacter\t1\t\"a\"", a_(i)), ""),
                                sprintf("%s\tdouble\t1\t1", a_(d))))
  verifier("C3 : texte écrit à la main de profondeur 32 lu identical()", identical(lire_plan(texte(32L)), imbrique(32L)))
  for (d in c(33L, 1000L)) {
    r <- tryCatch({ lire_plan(texte(d)); NULL }, refus_tablexp = function(e) e, error = function(e) e)
    verifier(sprintf("C3 : texte écrit à la main de profondeur %d -> texte_profondeur_excessive en ligne 35, sans erreur de pile", d),
             inherits(r, "refus_tablexp") && identical(r$motif, "texte_profondeur_excessive") && identical(r$details$ligne, 35L) ||
               if (is.null(r)) "aucun refus" else conditionMessage(r))
  }
})

# M66 (b) : sous-dépassement refusé (valeur calculée à la main : codes ;
# valeurs relues mesurées dans l'ADR 0011, troisième annotation, (b)).
local({
  lu <- function(s) lire_plan(octets(c("tablexp plan 1", "@noms\tcharacter\t1\t\"a\"", paste0("a\tdouble\t1\t", s))))$a
  for (s in c("1e-400", "-1e-400", "2e-324"))
    verifier(sprintf("M66 : %s -> texte_double_sous_depassement", s),
             identical(refus_de(lu(s))$motif, "texte_double_sous_depassement"))
  admis <- list("0" = 0, "0.0" = 0, "0e-400" = 0, "-0" = 0, "3e-324" = 5e-324, "5e-324" = 5e-324,
                "2.2250738585072014e-308" = 2.2250738585072014e-308)
  for (s in names(admis))
    verifier(sprintf("M66 : %s admis, relu %s", s, format(admis[[s]], digits = 17L)), identical(lu(s), admis[[s]]))
  verifier("M66 : 1e400 reste texte_valeur_invalide (non fini)", identical(refus_de(lu("1e400"))$motif, "texte_valeur_invalide"))
  t <- sub("delai_declaration\tdouble\t1\t0\n", "delai_declaration\tdouble\t1\t1e-400\n", rawToChar(ecrire_plan(plan_complet())),
           fixed = TRUE)
  r <- refus_de(lire_plan(charToRaw(t)))
  verifier("M66 : plan importé avec delai_declaration 1e-400 refusé (texte_double_sous_depassement, numéro de ligne)",
           grepl("\t1e-400\n", t, fixed = TRUE) && identical(r$motif, "texte_double_sous_depassement") && is.integer(r$details$ligne))
})

# (c) : Date à stockage integer écrite, relue en double (identité exacte).
local({
  p_int <- plan_complet()
  p_int$periode$debut <- structure(16436L, class = "Date")
  verifier("Date integer : témoin bien formé (typeof integer, 2015-01-01)",
           identical(typeof(p_int$periode$debut), "integer") && identical(format(p_int$periode$debut), "2015-01-01"))
  o <- ecrire_plan(p_int)
  verifier("Date integer : ecrire_plan() réussit ; octets identiques à ceux du même plan en double",
           identical(o, ecrire_plan(plan_complet())))
  x <- construire_table(BASE, ROLES, p_int)
  verifier("Date integer : plan relu identical() à fiche$plan de la construction ; valider_plan() passe",
           identical(lire_plan(o), x$fiche$plan) && identical(valider_plan(lire_plan(o)), lire_plan(o)))
})

# (a) : facteur hors de la grammaire 1 (valeur calculée à la main : codes).
verifier("factor : refusé à l'écriture (serialisation_impossible) et type inconnu à la lecture",
         identical(refus_de(ecrire_fiche(list(a = factor("x"))))$motif, "serialisation_impossible") &&
           identical(refus_de(lire_fiche(octets(c("tablexp fiche 1", "@noms\tcharacter\t1\t\"a\"",
                                                  "a\tfactor\t1\t\"x\""))))$motif, "texte_type_inconnu") &&
           !"factor" %in% tablexp:::TYPES_TEXTE)

# (h) : en-tête, ordre des lignes et blocs de lecture (valeur calculée à la main).
local({
  cles <- function(o) { l <- lignes_de(o); sub(" : .*$", "", sub("^# ", "", l[startsWith(l, "# ")])) }
  conv <- c("grammaire", "doubles", "dates", "chaînes", "délais", "codage")
  verifier("en-tête du plan : grammaire, doubles, dates, chaînes, délais, codage, dans cet ordre (ni lecture ni rejeu)",
           identical(cles(ecrire_plan(plan_complet())), conv))
  x <- construire_table(BASE, ROLES, within(plan_complet(), etapes <- etapes["donnees"]))
  o <- ecrire_fiche(x$fiche)
  verifier("en-tête de la fiche : les six lignes du plan, puis version_fiche, representation, lecture, rejeu",
           identical(cles(o), c(conv, "version_fiche", "representation", "lecture", "rejeu")))
  l <- lignes_de(o)
  verifier("en-tête : grammaire (@longueur, profondeur 32), doubles (sous-dépassement), rejeu (construire_table)",
           any(grepl("@longueur", l, fixed = TRUE) & startsWith(l, "# grammaire") & grepl("profondeur au plus 32", l, fixed = TRUE)) &&
             any(startsWith(l, "# doubles") & grepl("décimal non nul relu 0", l, fixed = TRUE)) &&
             any(startsWith(l, "# doubles") &
                   grepl("17 chiffres significatifs (%.17g) ; relecture au bit près constatée sous glibc, non garantie ailleurs",
                         l, fixed = TRUE)) &&
             !any(grepl("aller-retour exact", l, fixed = TRUE)) &&
             any(startsWith(l, "# rejeu") & grepl("construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance)", l, fixed = TRUE)))
  b <- tablexp:::BLOCS_LECTURE_FICHE
  verifier("BLOCS_LECTURE_FICHE : trois blocs (calculé, hypothèses, rejeu), aucun champ en double",
           identical(names(b), c("calculé", "hypothèses", "rejeu")) && !anyDuplicated(unlist(b)))
  verifier("BLOCS_LECTURE_FICHE : affectation validée par actuary le 20/10/2026 (ADR 0011, point 6.5 ; nombre_lignes_exclues après exclusions, #57), ordre des champs compris",
           identical(unname(b), list(
             c("plan", "transition_code", "fin_effective", "unite_observation", "unite_exposition", "par_annee", "recette",
               "convention"),
             c("exclusions", "nombre_lignes_exclues", "hypothese_censure", "hypotheses_brut", "avertissement_unite",
               "avertissements"),
             c("version_fiche", "roles", "provenance", "provenance_mention", "empreintes", "graine", "graine_mention",
               "generateur", "versions", "plateforme"))))
  presents <- intersect(unlist(b), names(x$fiche))
  verifier("BLOCS_LECTURE_FICHE : partition de names(fiche) sans étape brut (aucun manquant ; seul hypotheses_brut hors fiche)",
           setequal(presents, names(x$fiche)) && length(presents) == length(x$fiche) &&
             identical(setdiff(unlist(b), names(x$fiche)), "hypotheses_brut"))
  xb <- construire_table(BASE, ROLES, plan_complet())
  verifier("BLOCS_LECTURE_FICHE : partition exacte de names(fiche) avec étape brut (aucun manquant, doublon ni étranger)",
           setequal(unlist(b), names(xb$fiche)) && length(unlist(b)) == length(xb$fiche))
  verifier("en-tête : ligne lecture sans hypotheses_brut pour la fiche sans brut, avec pour la fiche avec brut",
           !grepl("hypotheses_brut", l[startsWith(l, "# lecture")], fixed = TRUE) &&
             grepl("hypotheses_brut", grep("^# lecture", lignes_de(ecrire_fiche(xb$fiche)), value = TRUE), fixed = TRUE))
  m <- l; m[startsWith(m, "# lecture")] <- "# lecture : modifiée à la main"; m <- m[!startsWith(m, "# rejeu")]
  verifier("en-tête de la fiche hors aller-retour : ligne lecture modifiée, ligne rejeu retirée -> même fiche relue",
           identical(lire_fiche(octets(m)), x$fiche))
})

# Revue finale de #50 : texte du moteur marqué UTF-8 en toute locale (identité
# exacte : marques de codage ; valeur calculée à la main : codes de refus). Le
# lanceur installe tablexp sous une locale UTF-8 (M54) ; la locale d'exécution
# est celle de la batterie (C ou C.UTF-8), puis la locale C posée ici.
local({
  # Chaînes de l'objet, valeurs et noms à tous les niveaux.
  chaines <- function(x) c(names(x), if (is.list(x)) unlist(lapply(x, chaines), use.names = FALSE) else if (is.character(x)) x)
  non_ascii_non_utf8 <- function(x) {
    s <- chaines(x)
    s[!is.na(s) & grepl("[^\001-\177]", s, useBytes = TRUE) & Encoding(s) != "UTF-8"]
  }
  sous_locale_c <- function(expr) {
    ancienne <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", ancienne))
    Sys.setlocale("LC_CTYPE", "C")
    force(expr)
  }
  x <- construire_table(BASE, ROLES, plan_complet())
  verifier(sprintf("#50 : toutes les chaînes non ASCII de la fiche marquées UTF-8 (locale %s)", Sys.getlocale("LC_CTYPE")),
           !length(non_ascii_non_utf8(x$fiche)) || sprintf("non marquées : %s", paste(utils::head(non_ascii_non_utf8(x$fiche), 3L),
                                                                                       collapse = " | ")))
  verifier("#50 : la fiche compte des chaînes non ASCII (témoin : le contrôle ci-dessus porte)",
           any(grepl("[^\001-\177]", chaines(x$fiche), useBytes = TRUE)))
  verifier("#50 : CONVENTIONS_TEXTE et BLOCS_LECTURE_FICHE (valeurs et noms) marqués UTF-8",
           !length(non_ascii_non_utf8(list(tablexp:::CONVENTIONS_TEXTE, tablexp:::BLOCS_LECTURE_FICHE))))
  xc <- sous_locale_c(construire_table(BASE, ROLES, plan_complet()))
  verifier("#50 : sous la locale C, fiche aux chaînes non ASCII marquées UTF-8, ecrire_fiche() réussit sans avertissement, aller-retour identical()",
           sous_locale_c({
             a <- NULL
             o <- withCallingHandlers(ecrire_fiche(xc$fiche), warning = function(w) { a <<- c(a, conditionMessage(w))
                                                                                      invokeRestart("muffleWarning") })
             !length(non_ascii_non_utf8(xc$fiche)) && is.null(a) && identical(lire_fiche(o), xc$fiche) &&
               identical(o, ecrire_fiche(x$fiche)) || if (length(a)) a[1L] else "fiche non marquée ou texte différent"
           }))
  verifier("#50 : sous la locale C, ecrire_plan() sans avertissement (en-tête marqué UTF-8), mêmes octets",
           sous_locale_c({
             a <- NULL
             o <- withCallingHandlers(ecrire_plan(plan_complet()), warning = function(w) { a <<- c(a, conditionMessage(w))
                                                                                          invokeRestart("muffleWarning") })
             is.null(a) && identical(o, ecrire_plan(plan_complet())) || if (length(a)) a[1L] else "octets différents"
           }))
  # Données de l'utilisateur : refus inchangé (texte_moteur() ne les touche pas).
  inconnue <- rawToChar(as.raw(c(0x61, 0xc3, 0xa9)))
  verifier("#50 : témoin, chaîne non ASCII non marquée (octets UTF-8, codage unknown)",
           identical(Encoding(inconnue), "unknown") && grepl("[^\001-\177]", inconnue, useBytes = TRUE))
  verifier("#50 : sous la locale C, chaîne non ASCII non marquée de l'utilisateur toujours refusée (chaine_codage_indetermine : plan, données)",
           sous_locale_c(identical(refus_de(ecrire_plan(list(a = inconnue)))$motif, "chaine_codage_indetermine") &&
                           identical(refus_de(tablexp:::empreinte_donnees(data.frame(x = c("a", inconnue), stringsAsFactors = FALSE),
                                                                           c(segment = "x")))$motif,
                                     "chaine_codage_indetermine")))
})
verifier("#50 : texte_moteur() : non ASCII non marqué valide -> UTF-8 ; latin-1 -> UTF-8 ; ASCII, NA et UTF-8 invalide inchangés ; noms et attributs conservés", {
  tm <- tablexp:::texte_moteur
  u <- rawToChar(as.raw(c(0xc3, 0xa9))); l1 <- iconv("\u00e9", "UTF-8", "latin1"); inv <- rawToChar(as.raw(0xe9))
  y <- tm(c(a = u, b = l1, c = "x", d = NA, e = inv))
  z <- tm(data.frame(v = c(u, "x"), n = 1:2, stringsAsFactors = FALSE))
  identical(Encoding(l1), "latin1") && identical(Encoding(y), c("UTF-8", "UTF-8", "unknown", "unknown", "unknown")) &&
    identical(unname(y[1:2]), c("\u00e9", "\u00e9")) && identical(names(y), letters[1:5]) && is.na(y[["d"]]) &&
    identical(charToRaw(y[["e"]]), as.raw(0xe9)) && is.data.frame(z) && identical(Encoding(z$v), c("UTF-8", "unknown")) &&
    identical(z$n, 1:2) && identical(attr(z, "row.names"), 1:2)
})

# (i) : lire_fiche() ne contrôle pas version_fiche (identité exacte).
local({
  f <- construire_table(BASE, ROLES, plan_complet())$fiche
  f$version_fiche <- 99L
  verifier("(i) : fiche de version_fiche 99 relue sans refus, identical() (contrôle au rejeu, troisième annotation, (i))",
           identical(lire_fiche(ecrire_fiche(f)), f))
})

# C5 : message propre à une ligne NULL de longueur non nulle.
verifier("C5 : ligne NULL de longueur 1 -> texte_longueur_incoherente, message « NULL : longueur 0 attendue »", {
  r <- refus_de(lire_plan(octets(c("tablexp plan 1", "@noms\tcharacter\t1\t\"a\"", "a\tNULL\t1\tx"))))
  identical(r$motif, "texte_longueur_incoherente") && grepl("NULL : longueur 0 attendue", conditionMessage(r), fixed = TRUE)
})

# Correctif R1 d'audit : effectif @longueur borné par les lignes restantes avant
# toute allocation (valeur calculée à la main : codes, lignes ; constat de
# mesure : temps et pic mémoire par gc()).
local({
  texte <- function(nb, n_el) octets(c("tablexp plan 1", sprintf("@longueur\tinteger\t1\t%s", nb),
                                       sprintf("[%d]\tdouble\t1\t1", seq_len(n_el))))
  for (c_ in list(list("1e9", "1000000000", 0L), list(".Machine$integer.max", "2147483647", 0L),
                  list("reste du texte + 1", "3", 2L))) {
    invisible(gc(reset = TRUE))
    duree <- system.time(r <- tryCatch({ lire_plan(texte(c_[[2L]], c_[[3L]])); NULL },
                                       refus_tablexp = function(e) e, error = function(e) e))[["elapsed"]]
    pic <- sum(gc()[, 6L])
    verifier(sprintf("R1 d'audit : @longueur %s -> texte_longueur_incoherente en ligne 2, en moins d'1 s, pic gc sous 200 Mo", c_[[1L]]),
             inherits(r, "refus_tablexp") && identical(r$motif, "texte_longueur_incoherente") &&
               identical(r$details$ligne, 2L) && duree < 1 && pic < 200 ||
               sprintf("%s ; %.2f s ; pic %.1f Mo", if (is.null(r)) "aucun refus" else conditionMessage(r), duree, pic))
  }
  verifier("R1 d'audit : @longueur égal au reste du texte (2 éléments, 2 lignes) lu",
           identical(lire_plan(texte("2", 2L)), list(1, 1)))
})
