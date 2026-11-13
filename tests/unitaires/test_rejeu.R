# -----------------------------------------------------------------------------
#  tests/unitaires/test_rejeu.R : rejeu outillé rejouer_fiche() (issue #86 ;
#  ADR 0012, point 3 ; spécification d'actuary du 24/10/2026, tests T1 à T8)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : contrat (signature, export, constante),
#    structure et libellés du bilan, motifs, modules, details et textes des
#    refus, ordre des contrôles, verdicts des témoins, nombre d'appels de
#    construire_table() (mesuré par trace() sur l'espace de noms installé) ;
#  - identité exacte : objet rendu par le rejeu identical() hors `execution` à
#    l'objet original ; motif d'un refus délégué identique à celui de
#    construire_table() appelée directement sur les champs de la fiche ;
#  - constat de mesure : cas adverses de l'ADR 0012, point 3.9 (colonne hors
#    rôles, colonne de rôle renommée, provenance, fiche de version 3), testés
#    tels que constatés.
# -----------------------------------------------------------------------------

D <- as.Date
# Octets des lignes reçues et lignes d'un texte marquées UTF-8 (comme
# tests/unitaires/test_serialisation.R).
octets <- function(...) charToRaw(paste0(paste(c(...), collapse = "\n"), "\n"))
lignes_de <- function(o) { l <- strsplit(rawToChar(o), "\n", fixed = TRUE)[[1L]]; Encoding(l) <- "UTF-8"; l }
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)

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

X <- construire_table(BASE, ROLES, plan_complet())
TEXTE <- lignes_de(ecrire_fiche(X$fiche))
FICHE <- lire_fiche(octets(TEXTE))
R0 <- rejouer_fiche(BASE, FICHE)
EMPREINTE_CODE <- tablexp:::EMPREINTE_CODE

# Ligne 8 « tables_reference » (contrôle 7 bis, ADR 0015, point 4.3 ; #73) :
# témoins aux lignes 9 à 14.
CONTROLES <- c("nature_arguments", "forme_fiche", "version_fiche", "algorithme_empreinte", "representation",
               "entrees_construction", "empreinte_donnees", "tables_reference", "temoin_code", "temoin_versions",
               "temoin_plateforme", "temoin_fiche", "temoin_empreinte_resultat", "temoin_empreinte_fichier")
STATUTS <- c("contrôle de forme", "contrôle de forme", "comparaison exacte", "comparaison exacte",
             "comparaison exacte", "contrôles délégués aux modules", "exacte ; sans rôle de sécurité",
             "exacte ; sans rôle de sécurité", "exacte ; sans rôle de sécurité", "comparaison exacte",
             "comparaison exacte", "comparaison exacte", "témoin, même machine seulement",
             "reprise de la fiche, non vérifiée par le moteur")
VERDICTS_SUCCES <- c(rep("conforme", 8L), rep("identique", 5L), "non vérifiée")

# Fiche FICHE modifiée : chaque argument nommé est un chemin « a/b/c » de la
# fiche, posé à sa valeur (NULL retire le champ).
fiche_modifiee <- function(..., f = FICHE) {
  m <- list(...)
  for (ch in names(m)) {
    p <- strsplit(ch, "/", fixed = TRUE)[[1L]]
    if (is.null(m[[ch]])) {
      if (length(p) == 1L) f[[p]] <- NULL else f[[p[-length(p)]]][[p[length(p)]]] <- NULL
    } else f[[p]] <- m[[ch]]
  }
  f
}
# Fiche relue du texte TEXTE dont la ligne de début `de` est remplacée par `par`.
fiche_texte <- function(de, par) {
  k <- startsWith(TEXTE, de)
  if (sum(k) != 1L) stop(sprintf("ligne « %s » trouvée %d fois", de, sum(k)))
  lire_fiche(octets(replace(TEXTE, k, par)))
}
BASE_PERMUTEE <- BASE[4:1, ]; rownames(BASE_PERMUTEE) <- NULL

# Refus attendu : module, motif, details (si donnés) et fragments du message
# (fixed = TRUE). Rend TRUE ou la liste des écarts.
controle_refus <- function(r, module, motif, details = NULL, contient = character(0)) {
  if (is.null(r)) return("aucun refus levé")
  e <- character(0)
  if (!identical(r$module, module)) e <- c(e, sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) e <- c(e, sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!is.null(details) && !identical(r$details, details))
    e <- c(e, paste("details :", paste(deparse(r$details), collapse = " ")))
  for (s in contient) if (!grepl(s, conditionMessage(r), fixed = TRUE)) e <- c(e, sprintf("message sans « %s »", s))
  if (length(e)) e else TRUE
}

# Appels de construire_table() et de preparer_donnees() pendant expr (trace()
# sur l'espace de noms installé, retiré à la sortie) ; un refus est rattrapé.
compter_appels <- function(expr) {
  ns <- asNamespace("tablexp")
  n <- new.env()
  n$construire_table <- 0L; n$preparer_donnees <- 0L
  for (f in c("construire_table", "preparer_donnees"))
    suppressMessages(trace(f, where = ns, print = FALSE,
                           tracer = bquote(assign(.(f), get(.(f), envir = .(n)) + 1L, envir = .(n)))))
  on.exit(for (f in c("construire_table", "preparer_donnees")) suppressMessages(untrace(f, where = ns)))
  tryCatch(force(expr), refus_tablexp = function(r) NULL)
  c(construire_table = n$construire_table, preparer_donnees = n$preparer_donnees)
}

# --- T1 : contrat (valeur calculée à la main) -----------------------------------

verifier("T1 rejouer_fiche : exportée, arguments (donnees, fiche, tables_reference = NULL) (ADR 0015, point 1.4 ; #73)",
         "rejouer_fiche" %in% getNamespaceExports("tablexp") &&
           identical(names(formals(rejouer_fiche)), c("donnees", "fiche", "tables_reference")) &&
           is.null(formals(rejouer_fiche)$tables_reference))
verifier("T1 VERSIONS_FICHE_REJEU : integer sans attribut ni NA, identical() à VERSION_FICHE (6L, #73, #169)", {
  v <- tablexp:::VERSIONS_FICHE_REJEU
  is.integer(v) && is.null(attributes(v)) && !anyNA(v) && identical(v, tablexp:::VERSION_FICHE) && identical(v, 6L)
})

# --- T2 : structure d'un rejeu réussi (valeur calculée à la main ; identité exacte)

verifier("T2 rejeu réussi : classe rejeu, éléments construction et bilan",
         identical(class(R0), "rejeu") && identical(names(R0), c("construction", "bilan")))
verifier("T2 bilan : data.frame de 14 lignes, 5 colonnes character dans l'ordre, sans NA, noms de lignes automatiques", {
  b <- R0$bilan
  is.data.frame(b) && identical(names(b), c("controle", "attendu", "lu", "verdict", "statut")) &&
    all(vapply(b, is.character, NA)) && nrow(b) == 14L && !anyNA(b) &&
    identical(attr(b, "row.names"), 1:14) && identical(b$controle, CONTROLES)
})
verifier("T2 bilan : verdicts (1 à 8 « conforme », 9 à 13 « identique », 14 « non vérifiée ») et statuts exacts",
         identical(R0$bilan$verdict, VERDICTS_SUCCES) && identical(R0$bilan$statut, STATUTS))
verifier("T2 bilan : attendu et lu des lignes 3, 4, 5, 7, 9 et 13 égaux aux constantes et aux valeurs de la fiche", {
  b <- R0$bilan; e <- FICHE$empreintes
  identical(b$attendu[c(3, 4, 5, 7, 9, 13)],
            c("6", "md5", "tablexp canonique 2", tablexp:::empreinte_donnees(BASE, ROLES), EMPREINTE_CODE,
              R0$construction$fiche$empreintes$resultat)) &&
    identical(b$lu[c(3, 4, 5, 7, 9, 13)], c("6", e$algorithme, e$representation, e$donnees, e$code, e$resultat))
})
verifier("T2 bilan : lignes 1, 2, 6, 8, 12 et 14 (textes fixes et composés ; ligne 6 : controler_matrice_recette() citée, #169)", {
  b <- R0$bilan
  identical(b$attendu[1L], "fiche : liste nommée ; donnees : data.frame") &&
    identical(b$attendu[6L], paste("valider_plan(), controler_roles(), recette et convention,",
                                   "controler_matrice_recette(), controler_provenance() : aucun refus")) &&
    identical(b$lu[1L], "fiche : liste nommée de 25 champs ; donnees : classe data.frame, 4 lignes, 7 colonnes") &&
    identical(b$lu[2L], "champs lus de la forme attendue") &&
    identical(b$lu[6L], paste("étapes : donnees, exposition, brut ; rôles : id_contrat, id_tete, sexe, date_naissance,",
                              "date_entree, date_sortie, cause_sortie ; recette : lignes_dates_completes ; provenance : NULL")) &&
    identical(b$attendu[8L], "aucune table de référence (fiche/tables_reference : NULL)") &&
    identical(b$lu[8L], "aucune table reçue ni relue") &&
    identical(b$lu[12L], "fiche reçue, même périmètre") && identical(b$lu[14L], "NA") &&
    identical(b$attendu[14L], "non recalculée par le moteur (couche de lecture, E4.4)")
})
verifier("T2 construction : identical() hors execution à l'objet original (identité exacte)",
         identical(sans_execution(R0$construction), sans_execution(X)))
verifier("T2 donnees et fiche inchangées par le rejeu", {
  d <- BASE; f <- FICHE
  rejouer_fiche(d, f)
  identical(d, BASE) && identical(f, FICHE)
})
verifier("T2 même rejeu avec x$fiche qu'avec la fiche relue de son texte", {
  r <- rejouer_fiche(BASE, X$fiche)
  identical(sans_execution(r$construction), sans_execution(R0$construction)) && identical(r$bilan, R0$bilan)
})

# --- T3 et T6 : un test par motif ; construire_table() non appelée --------------
# (valeur calculée à la main)

R_MATRICE <- refus_de(construire_table(as.matrix(BASE), ROLES, plan_complet()))
CAS_REFUS <- list(
  list(nom = "étape 1 : objet construction", f = X, module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "(fiche)", lu = "construction", attendu = "liste nommée"),
       contient = "objet `construction` reçu à la place de sa fiche"),
  list(nom = "étape 1 : objet rejeu", f = R0, module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "(fiche)", lu = "rejeu", attendu = "liste nommée"),
       contient = "passer r$construction$fiche"),
  list(nom = "étape 1 : chaîne « fiche.txt »", f = "fiche.txt", module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "(fiche)", lu = "character", attendu = "liste nommée"),
       contient = c("objet de classe character (type character)", "Le rejeu ne lit aucun fichier")),
  list(nom = "étape 1 : données en matrice", d = as.matrix(BASE), module = "donnees", motif = "donnees_mal_formees",
       details = R_MATRICE$details, contient = conditionMessage(R_MATRICE)),
  list(nom = "étape 1 : données « donnees.csv »", d = "donnees.csv", module = "donnees", motif = "donnees_mal_formees",
       details = list(classe = "character")),
  list(nom = "étape 2 : version_fiche double 3 (texte édité puis relu)",
       f = fiche_texte("version_fiche\tinteger\t1\t6", "version_fiche\tdouble\t1\t3"),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "version_fiche", lu = "double de longueur 1",
                      attendu = "integer de longueur 1, non NA, sans attribut"),
       contient = "« version_fiche » : double de longueur 1 au lieu de integer de longueur 1, non NA, sans attribut"),
  list(nom = "étape 2 : version_fiche NA", f = fiche_modifiee(version_fiche = NA_integer_),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "version_fiche", lu = "integer de longueur 1, NA",
                      attendu = "integer de longueur 1, non NA, sans attribut")),
  list(nom = "étape 2 : version_fiche nommée", f = fiche_modifiee(version_fiche = c(a = 4L)),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "version_fiche", lu = "integer de longueur 1, attribut(s) names",
                      attendu = "integer de longueur 1, non NA, sans attribut")),
  list(nom = "étape 2 : empreintes/donnees et roles retirés, cités dans un seul refus",
       f = fiche_modifiee("empreintes/donnees" = NULL, roles = NULL),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = c("empreintes/donnees", "roles"), lu = c("champ absent", "champ absent"),
                      attendu = c("chaîne unique, non vide, non NA, sans attribut", "champ présent")),
       contient = "aucun n'est complété ni converti"),
  list(nom = "étape 2 : empreintes = « x »", f = fiche_modifiee(empreintes = "x"),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "empreintes", lu = "character de longueur 1",
                      attendu = "liste nommée aux noms uniques, sans autre attribut que ses noms")),
  list(nom = "étape 2 : empreintes à deux « algorithme » (décision Q-A)",
       f = fiche_modifiee(empreintes = c(FICHE$empreintes, list(algorithme = "md5"))),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "empreintes",
                      lu = sprintf("list de longueur %d, noms en double : algorithme", length(FICHE$empreintes) + 1L),
                      attendu = "liste nommée aux noms uniques, sans autre attribut que ses noms")),
  list(nom = "étape 2 : empreintes aux noms incomplets (décision Q-A)",
       f = fiche_modifiee(empreintes = c(FICHE$empreintes, list("x"))),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "empreintes",
                      lu = sprintf("list de longueur %d, noms incomplets", length(FICHE$empreintes) + 1L),
                      attendu = "liste nommée aux noms uniques, sans autre attribut que ses noms")),
  list(nom = "étape 2 : empreintes/algorithme vide", f = fiche_modifiee("empreintes/algorithme" = ""),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "empreintes/algorithme", lu = "character de longueur 1, vide",
                      attendu = "chaîne unique, non vide, non NA, sans attribut")),
  list(nom = "étape 2 : provenance retirée", f = fiche_modifiee(provenance = NULL),
       module = "construire_table", motif = "fiche_non_conforme",
       details = list(champ = "provenance", lu = "champ absent", attendu = "champ présent (NULL admis)")),
  list(nom = "étape 3 : version_fiche 99", f = fiche_modifiee(version_fiche = 99L),
       module = "construire_table", motif = "version_fiche_non_admise",
       details = list(lu = 99L, admises = 6L, tablexp = FICHE$versions$tablexp, code = FICHE$empreintes$code,
                      code_installe = EMPREINTE_CODE),
       contient = c("version_fiche 99 lue", "admise(s) par ce code : 6.",
                    sprintf("Fiche produite par tablexp %s, empreinte du code %s", FICHE$versions$tablexp,
                            FICHE$empreintes$code),
                    sprintf("empreinte du code %s. Aucune", EMPREINTE_CODE), "Aucune conversion de fiche n'est faite")),
  list(nom = "étape 3 : version_fiche 99, versions retiré", f = fiche_modifiee(version_fiche = 99L, versions = NULL),
       module = "construire_table", motif = "version_fiche_non_admise", contient = "tablexp absente"),
  list(nom = "étape 3 : version_fiche 99, versions liste sans tablexp (décision Q-E)",
       f = fiche_modifiee(version_fiche = 99L, "versions/tablexp" = NULL),
       module = "construire_table", motif = "version_fiche_non_admise", contient = "Fiche produite par tablexp absente,"),
  list(nom = "étape 3 : version_fiche 99, versions = c(tablexp = \"9.9\") (décision Q-E)",
       f = fiche_modifiee(version_fiche = 99L, versions = c(tablexp = "9.9")),
       module = "construire_table", motif = "version_fiche_non_admise",
       contient = "Fiche produite par tablexp illisible (character de longueur 1),"),
  list(nom = "étape 4 : algorithme sha256", f = fiche_modifiee("empreintes/algorithme" = "sha256"),
       module = "construire_table", motif = "algorithme_empreinte_inconnu",
       details = list(lu = "sha256", attendu = "md5"), contient = c("« sha256 » lu", "« md5 » attendu")),
  list(nom = "étape 5 : représentation « tablexp canonique 1 »",
       f = fiche_modifiee("empreintes/representation" = "tablexp canonique 1"),
       module = "construire_table", motif = "representation_non_admise",
       details = list(lu = "tablexp canonique 1", attendu = "tablexp canonique 2"),
       contient = c("« tablexp canonique 1 » lue", "« tablexp canonique 2 » attendue")),
  list(nom = "étape 7 : lignes permutées", d = BASE_PERMUTEE,
       module = "construire_table", motif = "empreinte_donnees_discordante",
       details = list(lu = FICHE$empreintes$donnees, attendu = tablexp:::empreinte_donnees(BASE_PERMUTEE, ROLES),
                      lignes = 4L, colonnes = unname(FICHE$roles)),
       contient = c(FICHE$empreintes$donnees, tablexp:::empreinte_donnees(BASE_PERMUTEE, ROLES), "(4 lignes)",
                    "représentation « tablexp canonique 2 »",
                    "un autre ordre des lignes, une valeur ou un type de colonne différents suffisent à la changer",
                    "les colonnes hors rôles n'y entrent pas"))
)
for (cas in CAS_REFUS) {
  d <- if (is.null(cas[["d"]])) BASE else cas[["d"]]
  f <- if (is.null(cas[["f"]])) FICHE else cas[["f"]]
  verifier(sprintf("T3 refus, %s : %s/%s", cas$nom, cas$module, cas$motif),
           controle_refus(refus_de(rejouer_fiche(d, f)), cas$module, cas$motif, cas$details,
                          if (is.null(cas[["contient"]])) character(0) else cas[["contient"]]))
  verifier(sprintf("T6 refus, %s : construire_table() et preparer_donnees() non appelées", cas$nom),
           identical(compter_appels(rejouer_fiche(d, f)), c(construire_table = 0L, preparer_donnees = 0L)))
}
verifier("T3 étape 1 : message des données en matrice identical() à celui de construire_table()",
         identical(conditionMessage(refus_de(rejouer_fiche(as.matrix(BASE), FICHE))), conditionMessage(R_MATRICE)))
verifier("T6 rejeu réussi : construire_table() et preparer_donnees() appelées exactement une fois",
         identical(compter_appels(rejouer_fiche(BASE, FICHE)), c(construire_table = 1L, preparer_donnees = 1L)))

# --- T4 : ordre des contrôles (valeur calculée à la main) -----------------------

BASE_SANS_SEXE <- BASE[setdiff(names(BASE), "sexe")]
for (cas in list(
  list("fiche non liste et données non data.frame", "x", "fiche.txt", "fiche_non_conforme"),
  list("données non data.frame et version double", "x", fiche_modifiee(version_fiche = 3), "donnees_mal_formees"),
  list("version 3 et algorithme sha256", BASE, fiche_modifiee(version_fiche = 3L, "empreintes/algorithme" = "sha256"),
       "version_fiche_non_admise"),
  list("algorithme et représentation faux", BASE,
       fiche_modifiee("empreintes/algorithme" = "sha256", "empreintes/representation" = "autre"),
       "algorithme_empreinte_inconnu"),
  list("représentation fausse et colonne de rôle absente", BASE_SANS_SEXE,
       fiche_modifiee("empreintes/representation" = "autre"), "representation_non_admise"),
  list("colonne de rôle absente et lignes permutées", BASE_PERMUTEE[setdiff(names(BASE), "sexe")], FICHE,
       "colonne_absente")))
  verifier(sprintf("T4 ordre, %s : %s", cas[[1L]], cas[[4L]]),
           { r <- refus_de(rejouer_fiche(cas[[2L]], cas[[3L]])); if (is.null(r)) "aucun refus" else
               if (identical(r$motif, cas[[4L]])) TRUE else sprintf("motif « %s »", r$motif) })

# --- T5 et T6 : contrôles délégués, mêmes refus que construire_table() ----------
# (identité exacte). convention_plus_fine n'est pas testée : inaccessible
# aujourd'hui, la seule recette ayant pour convention native la plus fine des
# conventions mises en oeuvre (R/exposition.R, ORDRE_CONVENTIONS et
# resoudre_convention()).

BASE_DATE_TEXTE <- BASE; BASE_DATE_TEXTE$sortie <- as.character(BASE_DATE_TEXTE$sortie)
for (cas in list(
  list("colonne de rôle absente", BASE_SANS_SEXE, FICHE, "donnees", "colonne_absente"),
  list("date de sortie en character", BASE_DATE_TEXTE, FICHE, "donnees", "type_incompatible"),
  list("plan vide", BASE, fiche_modifiee(plan = list()), "construire_table", "plan_mal_forme"),
  list("convention inconnue", BASE, fiche_modifiee("plan/etapes/exposition/convention" = "inconnue"),
       "exposition", "convention_invalide"),
  list("provenance mal formée", BASE, fiche_modifiee(provenance = list(fichier = "a.csv")),
       "rapport", "provenance_invalide"))) {
  r <- refus_de(rejouer_fiche(cas[[2L]], cas[[3L]]))
  r_ct <- refus_de(construire_table(cas[[2L]], cas[[3L]]$roles, cas[[3L]]$plan, cas[[3L]]$provenance))
  verifier(sprintf("T5 délégué, %s : %s/%s, module, motif, message et details égaux à ceux de construire_table()",
                   cas[[1L]], cas[[4L]], cas[[5L]]),
           if (is.null(r) || is.null(r_ct)) "aucun refus" else
             if (!identical(unclass(r), unclass(r_ct))) "refus différent de celui de construire_table()" else
               controle_refus(r, cas[[4L]], cas[[5L]]))
  verifier(sprintf("T6 délégué, %s : construire_table() non appelée", cas[[1L]]),
           identical(compter_appels(rejouer_fiche(cas[[2L]], cas[[3L]]))[["construire_table"]], 0L))
}

# --- T7 : témoins (valeur calculée à la main) -----------------------------------

verdicts <- function(f, d = BASE) rejouer_fiche(d, f)$bilan$verdict
attendus_temoins <- function(...) { v <- VERDICTS_SUCCES; m <- list(...); for (k in names(m)) v[as.integer(k)] <- m[[k]]; v }
ecart_verdicts <- function(obtenu, attendu) if (identical(obtenu, attendu)) TRUE else
  sprintf("ligne %d : « %s » au lieu de « %s »", which(obtenu != attendu), obtenu[obtenu != attendu], attendu[obtenu != attendu])
FAUX_MD5 <- strrep("0", 32L)
for (cas in list(
  list("empreinte du résultat altérée : pas de refus, ligne 13 « différente »",
       fiche_modifiee("empreintes/resultat" = FAUX_MD5), list("13" = "différente")),
  list("empreintes du résultat et du code NA : « non déterminable »",
       fiche_modifiee("empreintes/resultat" = NA_character_, "empreintes/code" = NA_character_),
       list("9" = "non déterminable", "13" = "non déterminable")),
  list("empreinte du code altérée", fiche_modifiee("empreintes/code" = FAUX_MD5), list("9" = "différente")),
  list("versions$R modifiée", fiche_modifiee("versions/R" = "R version 0"), list("10" = "différente : R")),
  list("plateforme$blas modifiée", fiche_modifiee("plateforme/blas" = "autre"), list("11" = "différente : blas")),
  list("versions retiré", fiche_modifiee(versions = NULL), list("10" = "non déterminable")),
  list("champ x ajouté à plateforme", fiche_modifiee("plateforme/x" = "y"),
       list("11" = "différente : x (absent de la fiche rendue)")),
  list("transition_code modifié", fiche_modifiee(transition_code = "autre"),
       list("12" = "différente : transition_code")),
  list("estimateurs inversés dans le plan : résultat identique",
       fiche_modifiee("plan/etapes/brut/estimateurs" = rev(FICHE$plan$etapes$brut$estimateurs)),
       list("12" = "différente : plan/etapes/brut/estimateurs")),
  list("niveau 0,99 : résultat différent", fiche_modifiee("plan/etapes/brut/niveau" = 0.99),
       list("12" = "différente : hypotheses_brut/niveau", "13" = "différente")),
  list("deux champs de premier niveau permutés",
       FICHE[replace(names(FICHE), match(c("transition_code", "fin_effective"), names(FICHE)),
                     c("fin_effective", "transition_code"))],
       list("12" = "différente : fiche (ordre des champs ou attributs)"))))
  verifier(sprintf("T7 témoins, %s", cas[[1L]]), ecart_verdicts(verdicts(cas[[2L]]), do.call(attendus_temoins, cas[[3L]])))

# --- T8 : cas adverses de l'ADR 0012, point 3.9 (constat de mesure) -------------

sans_lignes_conservees <- function(x) { x <- sans_execution(x); x$etapes$donnees$resultat$lignes_conservees <- NULL; x }
BASE_EXTRA <- cbind(BASE, extra = 1:4)
verifier("T8 lignes permutées : refus empreinte_donnees_discordante",
         controle_refus(refus_de(rejouer_fiche(BASE_PERMUTEE, FICHE)), "construire_table", "empreinte_donnees_discordante"))
verifier("T8 colonne hors rôles ajoutée : pas de refus, témoins 9 à 13 « identique », objet identical() hors lignes_conservees seulement", {
  r <- rejouer_fiche(BASE_EXTRA, FICHE)
  identical(r$bilan$verdict, VERDICTS_SUCCES) && !identical(sans_execution(r$construction), sans_execution(X)) &&
    identical(sans_lignes_conservees(r$construction), sans_lignes_conservees(X))
})
verifier("T8 colonne hors rôles retirée : même constat", {
  x2 <- construire_table(BASE_EXTRA, ROLES, plan_complet())
  r <- rejouer_fiche(BASE, x2$fiche)
  identical(r$bilan$verdict, VERDICTS_SUCCES) && !identical(sans_execution(r$construction), sans_execution(x2)) &&
    identical(sans_lignes_conservees(r$construction), sans_lignes_conservees(x2))
})
BASE_GENRE <- BASE; names(BASE_GENRE)[names(BASE_GENRE) == "sexe"] <- "genre"
verifier("T8 colonne de rôle renommée, rôles de la fiche adaptés : passe, témoin fiche « identique », ligne 13 « différente »",
         ecart_verdicts(verdicts(fiche_modifiee("roles/sexe" = "genre"), BASE_GENRE), attendus_temoins("13" = "différente")))
verifier("T8 colonne de rôle renommée sans adaptation : refus colonne_absente",
         controle_refus(refus_de(rejouer_fiche(BASE_GENRE, FICHE)), "donnees", "colonne_absente"))
verifier("T8 fiche sans puis avec provenance : rejouées, identical() hors execution, empreintes du résultat distinctes, ligne 14", {
  prov <- list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
  xp <- construire_table(BASE, ROLES, plan_complet(), prov)
  rp <- rejouer_fiche(BASE, lire_fiche(ecrire_fiche(xp$fiche)))
  identical(sans_execution(rp$construction), sans_execution(xp)) &&
    identical(sans_execution(R0$construction), sans_execution(X)) &&
    !identical(xp$fiche$empreintes$resultat, X$fiche$empreintes$resultat) &&
    identical(R0$bilan$lu[14L], "NA") && identical(rp$bilan$lu[14L], prov$empreinte) &&
    identical(c(R0$bilan$verdict[14L], rp$bilan$verdict[14L]), rep("non vérifiée", 2L)) &&
    identical(rp$bilan$verdict, VERDICTS_SUCCES) && endsWith(rp$bilan$lu[6L], "; provenance : base.csv") &&
    endsWith(R0$bilan$lu[6L], "; provenance : NULL")
})
verifier("T8 fiche de version 3 fabriquée d'une fiche 6 écrite : lue sans refus, rejeu refusé, construire_table() directe passe", {
  noms6 <- TEXTE[startsWith(TEXTE, "plateforme/@noms\t")]
  noms5 <- sub("\t\"bibliotheque_c\"$", "", sub("^plateforme/@noms\tcharacter\t6\t", "plateforme/@noms\tcharacter\t5\t", noms6))
  l <- TEXTE[!startsWith(TEXTE, "plateforme/bibliotheque_c\t")]
  l[l == "version_fiche\tinteger\t1\t6"] <- "version_fiche\tinteger\t1\t3"
  l[l == noms6] <- noms5
  f3 <- tryCatch(lire_fiche(octets(l)), error = function(e) e)
  !inherits(f3, "error") && identical(f3$version_fiche, 3L) && !("bibliotheque_c" %in% names(f3$plateforme)) &&
    isTRUE(controle_refus(refus_de(rejouer_fiche(BASE, f3)), "construire_table", "version_fiche_non_admise")) &&
    inherits(construire_table(BASE, f3$roles, f3$plan, f3$provenance), "construction")
})

# --- T11 : valeurs citées dans les refus du rejeu (#94) -------------------------
# Règle unique neutraliser_citation() (R/outils.R) : VT, FF, NEL et U+2028
# cités <U+XXXX>, message sur une ligne et marqué UTF-8, valeur exacte dans les
# details ; une valeur « bytes » donne le refus nommé au lieu de l'erreur R
# « translating strings with "bytes" encoding is not allowed » (avant #94).
for (k in list(list("VT", 0x0B, "<U+000B>"), list("FF", 0x0C, "<U+000C>"), list("NEL", 0x85, "<U+0085>"),
               list("U+2028", 0x2028, "<U+2028>"))) {
  s <- intToUtf8(c(0x6d, 0x64, k[[2L]], 0x35))
  verifier(sprintf("T11 empreintes/algorithme « md<%s>5 » : refus algorithme_empreinte_inconnu, cité « md%s5 », une ligne, UTF-8",
                   k[[1L]], k[[3L]]), {
    r <- refus_de(rejouer_fiche(BASE, fiche_modifiee("empreintes/algorithme" = s)))
    ok <- controle_refus(r, "construire_table", "algorithme_empreinte_inconnu", list(lu = s, attendu = "md5"),
                         paste0("« md", k[[3L]], "5 »"))
    if (isTRUE(ok) && grepl(intToUtf8(k[[2L]]), conditionMessage(r), fixed = TRUE)) "caractère écrit en clair"
    else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
  })
  verifier(sprintf("T11 versions/tablexp « a<%s>b », version_fiche 3 : refus version_fiche_non_admise, cité « a%sb »",
                   k[[1L]], k[[3L]]),
           controle_refus(refus_de(rejouer_fiche(BASE, fiche_modifiee(version_fiche = 3L,
                                                                      "versions/tablexp" = intToUtf8(c(0x61, k[[2L]], 0x62))))),
                          "construire_table", "version_fiche_non_admise", NULL, paste0("tablexp a", k[[3L]], "b,")))
}
verifier("T11 empreintes/algorithme d'encodage bytes : refus algorithme_empreinte_inconnu (pas une erreur R), repère cité", {
  b <- "md\xe95"; Encoding(b) <- "bytes"
  r <- tryCatch(rejouer_fiche(BASE, fiche_modifiee("empreintes/algorithme" = b)), error = function(e) e)
  if (!inherits(r, "refus_tablexp")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
  else controle_refus(r, "construire_table", "algorithme_empreinte_inconnu", list(lu = b, attendu = "md5"),
                      "« <chaîne d'encodage bytes> »")
})
# Nom d'encodage bytes au premier niveau de la fiche (#114), en première
# position, au milieu et en dernière : refus fiche_non_conforme au lieu de
# l'erreur R « translating strings with "bytes" encoding is not allowed »
# (fiche[["version_fiche"]]... la lève), nom cité par neutraliser_citation()
# (repère), valeur brute dans details$lu.
for (k in c(1L, 4L, length(FICHE))) {
  verifier(sprintf("T11 nom d'encodage bytes en position %d de la fiche : refus fiche_non_conforme (pas une erreur R), repère cité, une ligne, UTF-8", k), {
    b <- "\xe9t\xe9"; Encoding(b) <- "bytes"
    fb <- FICHE; n <- names(fb); n[k] <- b; names(fb) <- n
    r <- tryCatch(rejouer_fiche(BASE, fb), error = function(e) e)
    if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
    ok <- controle_refus(r, "construire_table", "fiche_non_conforme",
                         list(champ = "(fiche)", lu = b, attendu = "noms de champ sans encodage bytes"),
                         sprintf("(champ n° %d : <chaîne d'encodage bytes>)", k))
    if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
    else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
  })
}
# Classe de la fiche citée par neutraliser_citation() dans le refus
# fiche_non_conforme (reprise de #94) : classe « a<VT>b » citée <U+000B> ;
# classe d'encodage bytes : refus nommé (inherits() levait « translating
# strings with "bytes" encoding is not allowed »), repère cité ; details$lu
# garde la classe exacte.
local({
  b <- "md\xe95"; Encoding(b) <- "bytes"
  for (k in list(list("a<VT>b", "a\u000bb", "objet de classe a<U+000B>b (type double)"),
                 list("d'encodage bytes", b, "objet de classe <chaîne d'encodage bytes> (type double)")))
    verifier(sprintf("T11 fiche structure(1, class = %s) : refus fiche_non_conforme (pas une erreur R), classe citée, une ligne, UTF-8",
                     k[[1L]]), {
      r <- tryCatch(rejouer_fiche(data.frame(a = 1), structure(1, class = k[[2L]])), error = function(e) e)
      if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
      ok <- controle_refus(r, "construire_table", "fiche_non_conforme",
                           list(champ = "(fiche)", lu = k[[2L]], attendu = "liste nommée"), k[[3L]])
      if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
      else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
    })
  # Même classe sur une liste (#114) : names() est un générique interne qui
  # dispatche sur la classe et levait la même erreur R (call names(x)) ; les
  # noms se lisent par attr(). Le cas structure(1, ...) ci-dessus n'est pas une liste.
  verifier("T11 fiche structure(list(a = 1), class = <encodage bytes>) : refus fiche_non_conforme (pas une erreur R), classe citée, une ligne, UTF-8", {
    r <- tryCatch(rejouer_fiche(data.frame(a = 1), structure(list(a = 1), class = b)), error = function(e) e)
    if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
    ok <- controle_refus(r, "construire_table", "fiche_non_conforme",
                         list(champ = "(fiche)", lu = b, attendu = "liste nommée"),
                         "objet de classe <chaîne d'encodage bytes> (type list)")
    if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
    else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
  })
})
# Classe d'encodage bytes en position quelconque de la classe d'une colonne de
# rôle ou des données reçues (#150), contrôle 6 (entrees_construction) : colonne
# sexe de classe c(<bytes>, "factor") ou c("factor", <bytes>) : refus
# type_incompatible du module donnees (avant #150 : type_non_canonique du module
# rapport) ; données de classe c(<bytes>, "data.frame") : refus
# donnees_mal_formees (avant #150 : erreur R « translating strings with "bytes"
# encoding is not allowed », dans is.data.frame()). Une erreur R est rendue, non
# propagée ; les data.frame sont assemblés sans `$<-`, qui lèverait la même erreur.
local({
  b <- "\xe9"; Encoding(b) <- "bytes"
  attrape <- function(expr) tryCatch(expr, error = function(e) e)
  une_ligne_utf8 <- function(r, ok) {
    if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
    else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
  }
  for (cl in list(list(c(b, "factor"), "<chaîne d'encodage bytes>/factor"),
                  list(c("factor", b), "factor/<chaîne d'encodage bytes>"))) {
    l <- unclass(BASE)
    l$sexe <- structure(as.integer(factor(l$sexe)), levels = c("F", "H"), class = cl[[1L]])
    d <- structure(l, class = "data.frame", row.names = seq_len(nrow(BASE)))
    verifier(sprintf("T11 rejeu, colonne sexe de classe %s : refus type_incompatible du module donnees (pas type_non_canonique), classe citée, une ligne, UTF-8", cl[[2L]]), {
      r <- attrape(rejouer_fiche(d, FICHE))
      if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
      une_ligne_utf8(r, controle_refus(r, "donnees", "type_incompatible", NULL,
                                       c("rôle « sexe », colonne « sexe »", paste0("attendu, ", cl[[2L]], " reçu"))))
    })
  }
  for (cl in list(list(c(b, "data.frame"), "<chaîne d'encodage bytes>/data.frame"),
                  list(c("data.frame", b), "data.frame/<chaîne d'encodage bytes>"))) {
    d <- structure(unclass(BASE), class = cl[[1L]], row.names = seq_len(nrow(BASE)))
    verifier(sprintf("T11 rejeu, données de classe %s : refus donnees_mal_formees du module donnees (pas une erreur R), classe citée, une ligne, UTF-8", cl[[2L]]), {
      r <- attrape(rejouer_fiche(d, FICHE))
      if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
      une_ligne_utf8(r, controle_refus(r, "donnees", "donnees_mal_formees", list(classe = cl[[1L]]),
                                       paste0("Les données portent une classe d'encodage « bytes » (classe ", cl[[2L]], ")")))
    })
  }
})
# Nom ou classe d'encodage bytes au deuxième niveau de la fiche, valeur de champ
# de classe bytes (#128) : refus fiche_non_conforme ou rejeu rendu, au lieu de
# l'erreur R « translating strings with "bytes" encoding is not allowed »
# (contre-épreuve : ces tests échouent sur le code antérieur, qui lève cette
# erreur sauf pour roles et plan, déjà refusés sous leurs motifs). Une erreur R
# est rendue, non propagée. ADR 0012, points 3.4 et 3.6 : `empreintes`, champ lu
# par le rejeu lui-même, est refusé à l'étape 2, avec les autres défauts de
# forme ; `versions` et `plateforme`, témoins, ne sont jamais motifs de refus :
# le rejeu est rendu, le témoin est « non déterminable ».
local({
  b <- "\xe9t\xe9"; Encoding(b) <- "bytes"
  attrape <- function(expr) tryCatch(expr, error = function(e) e)
  une_ligne_utf8 <- function(r, ok) {
    if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
    else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
  }
  verdict <- function(r, details, contient) {
    if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
    une_ligne_utf8(r, controle_refus(r, "construire_table", "fiche_non_conforme", details, contient))
  }
  # `empreintes` : nom bytes en position 1, 2, 3 (un des trois champs lus
  # remplacé : « champ absent » signalé avec `empreintes`) et en dernière
  # (empreintes seul) ; classe bytes ; avec un autre défaut de forme, un seul
  # refus les signale ensemble.
  attendu_e <- "liste nommée aux noms uniques, sans autre attribut que ses noms"
  attendu_c <- "chaîne unique, non vide, non NA, sans attribut"
  lu_nom <- function(k) sprintf("list de longueur 9, nom(s) d'encodage bytes (n° %d : <chaîne d'encodage bytes>)", k)
  trois <- c("algorithme", "representation", "donnees")
  for (k in c(1:3, length(FICHE$empreintes))) {
    fb <- FICHE; n <- names(fb$empreintes); n[k] <- b; attr(fb$empreintes, "names") <- n
    perdu <- k <= 3L
    verifier(sprintf("T11 nom d'encodage bytes en position %d de fiche$empreintes : refus fiche_non_conforme à l'étape 2 (pas une erreur R), champ empreintes%s, repère cité, une ligne, UTF-8",
                     k, if (perdu) sprintf(" avec empreintes/%s absent", trois[k]) else " seul"),
             verdict(attrape(rejouer_fiche(BASE, fb)),
                     if (perdu) list(champ = c("empreintes", paste0("empreintes/", trois[k])),
                                     lu = c(lu_nom(k), "champ absent"), attendu = c(attendu_e, attendu_c))
                     else list(champ = "empreintes", lu = lu_nom(k), attendu = attendu_e),
                     c("Fiche non conforme au rejeu : « empreintes » : ", lu_nom(k), "au lieu de liste nommée")))
  }
  fb <- FICHE; fb$empreintes <- structure(unclass(FICHE$empreintes), class = c("a", b))
  verifier("T11 fiche$empreintes de classe bytes : refus fiche_non_conforme à l'étape 2 (pas une erreur R), attribut class cité, une ligne, UTF-8",
           verdict(attrape(rejouer_fiche(BASE, fb)),
                   list(champ = "empreintes", lu = "list de longueur 9, attribut(s) class", attendu = attendu_e),
                   "« empreintes » : list de longueur 9, attribut(s) class au lieu de"))
  fb <- FICHE; n <- names(fb$empreintes); n[2L] <- b; attr(fb$empreintes, "names") <- n; fb$version_fiche <- 4.5
  verifier("T11 version_fiche 4.5 et fiche$empreintes à nom bytes : un seul refus fiche_non_conforme, les deux champs signalés ensemble",
           verdict(attrape(rejouer_fiche(BASE, fb)),
                   list(champ = c("version_fiche", "empreintes", "empreintes/representation"),
                        lu = c("double de longueur 1", lu_nom(2L), "champ absent"),
                        attendu = c("integer de longueur 1, non NA, sans attribut", attendu_e, attendu_c)),
                   c("« version_fiche » : double de longueur 1 au lieu de", "« empreintes » : ", lu_nom(2L))))
  # `versions` et `plateforme` : témoins, jamais motifs de refus. Nom bytes en
  # position 1, 2 et dernière, classe bytes : rejeu rendu, témoin « non
  # déterminable », valeur lue citée sous le repère (nom) ou « illisible »
  # (classe), sur une ligne, marquée UTF-8 si non ASCII ; les autres lignes du bilan sont
  # celles du rejeu d'une fiche saine.
  for (ch in c("versions", "plateforme")) {
    ligne <- if (ch == "versions") 10L else 11L
    ok_temoin <- function(r, lu_attendu) {
      if (inherits(r, "error")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
      lu <- r$bilan$lu[ligne]
      if (!inherits(r, "rejeu")) "objet non rejeu"
      else if (!identical(r$bilan$verdict[ligne], "non déterminable")) sprintf("verdict « %s »", r$bilan$verdict[ligne])
      else if (!identical(r$bilan$verdict[-ligne], R0$bilan$verdict[-ligne])) "autres verdicts changés"
      else if (!lu_attendu(lu)) sprintf("lu « %s »", lu)
      else if (grepl("[\n\v\f]", lu)) "valeur sur plusieurs lignes"
      else if (grepl("[^\001-\177]", lu, useBytes = TRUE) && !identical(Encoding(lu), "UTF-8")) "valeur non ASCII non marquée UTF-8"
      else TRUE
    }
    sain <- paste(names(FICHE[[ch]]), vapply(FICHE[[ch]], function(v) v, ""), sep = "=")
    for (k in unique(c(1L, 2L, length(FICHE[[ch]])))) {
      fb <- FICHE; n <- names(fb[[ch]]); n[k] <- b; attr(fb[[ch]], "names") <- n
      attendu_lu <- paste(replace(sain, k, paste0("<chaîne d'encodage bytes>=", FICHE[[ch]][[k]])), collapse = " ; ")
      verifier(sprintf("T11 nom d'encodage bytes en position %d de fiche$%s : rejeu rendu (aucun refus), témoin « non déterminable », nom cité sous le repère, une ligne, UTF-8", k, ch),
               ok_temoin(attrape(rejouer_fiche(BASE, fb)), function(lu) identical(lu, attendu_lu)))
    }
    fb <- FICHE; fb[[ch]] <- structure(unclass(FICHE[[ch]]), class = c("a", b))
    verifier(sprintf("T11 fiche$%s de classe bytes : rejeu rendu (aucun refus), témoin « non déterminable », valeur « illisible (list de longueur %d) »", ch, length(FICHE[[ch]])),
             ok_temoin(attrape(rejouer_fiche(BASE, fb)),
                       function(lu) identical(lu, sprintf("illisible (list de longueur %d)", length(FICHE[[ch]])))))
  }
  # Valeur de champ de classe bytes (decrire_forme(), m4) : décrite sans sa
  # classe, attribut `class` cité, au lieu de l'erreur R de length().
  verifier("T11 version_fiche structure(1L, class = <bytes>) : refus fiche_non_conforme (pas une erreur R), attribut class cité, une ligne, UTF-8",
           verdict(attrape(rejouer_fiche(BASE, fiche_modifiee(version_fiche = structure(4L, class = b)))),
                   list(champ = "version_fiche", lu = "integer de longueur 1, attribut(s) class",
                        attendu = "integer de longueur 1, non NA, sans attribut"),
                   "« version_fiche » : integer de longueur 1, attribut(s) class au lieu de"))
  verifier("T11 empreintes/algorithme structure(\"md5\", class = <bytes>) : refus fiche_non_conforme (pas une erreur R), attribut class cité",
           verdict(attrape(rejouer_fiche(BASE, fiche_modifiee("empreintes/algorithme" = structure("md5", class = b)))),
                   list(champ = "empreintes/algorithme", lu = "character de longueur 1, attribut(s) class",
                        attendu = "chaîne unique, non vide, non NA, sans attribut"),
                   "« empreintes/algorithme » : character de longueur 1, attribut(s) class au lieu de"))
  verifier("T11 version_fiche liste de classe bytes aux noms en double : refus fiche_non_conforme (pas une erreur R), noms cités un à un",
           verdict(attrape(rejouer_fiche(BASE, fiche_modifiee(version_fiche = structure(list(1, 2, 3, 4), names = c("a", "a", b, b), class = b)))),
                   list(champ = "version_fiche",
                        lu = "list de longueur 4, attribut(s) class, noms en double : a, <chaîne d'encodage bytes>, nom(s) d'encodage bytes (n° 3 : <chaîne d'encodage bytes>)",
                        attendu = "integer de longueur 1, non NA, sans attribut"),
                   "noms en double : a, <chaîne d'encodage bytes>, nom(s) d'encodage bytes (n° 3 : <chaîne d'encodage bytes>) au lieu de"))
  # Noms en double : un nom bytes ne remplace plus toute la liste par le repère.
  verifier("T11 noms en double a, a, bytes, bytes (vecteur nommé) : « a, <chaîne d'encodage bytes> », non le repère seul",
           verdict(attrape(rejouer_fiche(BASE, fiche_modifiee(version_fiche = structure(1:4, names = c("a", "a", b, b))))),
                   list(champ = "version_fiche",
                        lu = "integer de longueur 4, attribut(s) names, noms en double : a, <chaîne d'encodage bytes>, nom(s) d'encodage bytes (n° 3 : <chaîne d'encodage bytes>)",
                        attendu = "integer de longueur 1, non NA, sans attribut"),
                   "noms en double : a, <chaîne d'encodage bytes>, nom(s) d'encodage bytes (n° 3 : <chaîne d'encodage bytes>) au lieu de"))
  verifier("T11 noms en double a puis noms 'x<VT>y' en double : chaque nom cité séparément (a, x<U+000B>y)",
           verdict(attrape(rejouer_fiche(BASE, fiche_modifiee(version_fiche = structure(1:4, names = c("a", "a", "x\u000by", "x\u000by"))))),
                   list(champ = "version_fiche",
                        lu = "integer de longueur 4, attribut(s) names, noms en double : a, x<U+000B>y",
                        attendu = "integer de longueur 1, non NA, sans attribut"),
                   "noms en double : a, x<U+000B>y au lieu de"))
  # texte_lu() : valeur de classe bytes d'un champ lu par le bilan ou le refus
  # de version, « illisible » au lieu de l'erreur R de length() ; les témoins ne
  # sont jamais motifs de refus.
  verifier("T11 empreintes/code de classe bytes : rejeu sans refus, témoin du code illisible et non déterminable",
           {
             r <- attrape(rejouer_fiche(BASE, fiche_modifiee("empreintes/code" = structure("abc", class = b))))
             if (inherits(r, "error")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else identical(r$bilan$lu[9L], "illisible (character de longueur 1)") && identical(r$bilan$verdict[9L], "non déterminable")
           })
  verifier("T11 versions/tablexp de classe bytes : rejeu sans refus, témoin des versions « tablexp=illisible (character de longueur 1) »",
           {
             r <- attrape(rejouer_fiche(BASE, fiche_modifiee("versions/tablexp" = structure("abc", class = b))))
             if (inherits(r, "error")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else startsWith(r$bilan$lu[10L], "tablexp=illisible (character de longueur 1) ; ")
           })
  verifier("T11 version_fiche 3 et empreintes/code de classe bytes : refus version_fiche_non_admise, code « illisible (character de longueur 2) »",
           {
             r <- attrape(rejouer_fiche(BASE, fiche_modifiee(version_fiche = 3L, "empreintes/code" = structure(c("a", "b"), class = b))))
             if (!inherits(r, "refus_tablexp")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else une_ligne_utf8(r, controle_refus(r, "construire_table", "version_fiche_non_admise", NULL,
                                                   "empreinte du code illisible (character de longueur 2)"))
           })
  # Version non admise : la version de tablexp de la fiche se lit par
  # lire_champ() (versions de classe bytes, ou nom bytes avant `tablexp`), au
  # lieu de l'erreur R de names() ou de `[[`.
  verifier("T11 version_fiche 3 et versions de classe bytes : refus version_fiche_non_admise, tablexp lu cité",
           {
             fb <- fiche_modifiee(version_fiche = 3L); fb$versions <- structure(unclass(FICHE$versions), class = c("a", b))
             r <- attrape(rejouer_fiche(BASE, fb))
             if (!inherits(r, "refus_tablexp")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else une_ligne_utf8(r, controle_refus(r, "construire_table", "version_fiche_non_admise", NULL,
                                                   sprintf("Fiche produite par tablexp %s,", FICHE$versions$tablexp)))
           })
  verifier("T11 version_fiche 3 et versions à nom bytes avant tablexp : refus version_fiche_non_admise, tablexp lu cité",
           {
             fb <- fiche_modifiee(version_fiche = 3L); fb$versions <- structure(list("zzz", "9.9"), names = c(b, "tablexp"))
             r <- attrape(rejouer_fiche(BASE, fb))
             if (!inherits(r, "refus_tablexp")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else une_ligne_utf8(r, controle_refus(r, "construire_table", "version_fiche_non_admise", NULL,
                                                   "Fiche produite par tablexp 9.9,"))
           })
  # Nom d'encodage bytes plus bas dans la fiche (champs de la fiche comparés par
  # le témoin « fiche », champs_differents()) : `[[` et %in% sur ses noms
  # lèveraient l'erreur R ; la liste est une feuille, citée « différente » à
  # son chemin de premier niveau (ou de niveau inférieur si les parents se
  # descendent). Position 1 de convention, position 1 de hypotheses_brut, et
  # troisième niveau (hypotheses_brut/hypotheses) : rejeu rendu, ligne 12 du
  # bilan « différente : <chemin> », sur une ligne, marquée UTF-8.
  nom_bytes_en <- function(f, chemin, k) {
    n <- names(f[[chemin]]); n[k] <- b
    attr(f[[chemin]], "names") <- n
    f
  }
  for (cas in list(list("convention", "convention", 1L), list("hypotheses_brut", "hypotheses_brut", 1L),
                   list(c("hypotheses_brut", "hypotheses"), "hypotheses_brut/hypotheses", 3L))) {
    verifier(sprintf("T11 nom d'encodage bytes en position %d de fiche$%s : rejeu rendu (pas une erreur R), ligne 12 « différente : %s », une ligne, UTF-8",
                     cas[[3L]], paste(cas[[1L]], collapse = "$"), cas[[2L]]),
             {
               fb <- nom_bytes_en(FICHE, cas[[1L]], cas[[3L]])
               r <- attrape(rejouer_fiche(BASE, fb))
               if (inherits(r, "error")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
               else if (!inherits(r, "rejeu")) "objet non rejeu"
               else if (!identical(r$bilan$controle[12L], "temoin_fiche")) "ligne 12 : autre témoin"
               else if (!identical(r$bilan$verdict[12L], paste("différente :", cas[[2L]]))) sprintf("ligne 12 : « %s »", r$bilan$verdict[12L])
               else if (!identical(Encoding(r$bilan$verdict[12L]), "UTF-8")) "verdict non marqué UTF-8"
               else TRUE
             })
  }
  # Colonne de rôle de nom bytes (texte_canonique(), lire_champ()) : l'empreinte
  # des données se calcule, la construction refuse ensuite le rôle bytes
  # (chaine_codage_bytes, module rapport) au lieu de l'erreur R de `[[`.
  verifier("T11 colonne de rôle sexe de nom bytes : refus chaine_codage_bytes de la fiche (pas une erreur R), empreinte des données calculée",
           {
             d <- BASE; names(d)[names(d) == "sexe"] <- b
             r <- attrape(rejouer_fiche(d, fiche_modifiee(roles = replace(ROLES, "sexe", b))))
             if (!inherits(r, "refus_tablexp")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else controle_refus(r, "rapport", "chaine_codage_bytes", NULL, c("« fiche/roles »", "« bytes »"))
           })
})
# Noms d'attributs d'un champ cités par neutraliser_citation() dans le refus
# fiche_non_conforme (decrire_forme(), revue finale de #94) : attribut « a<LF>b »
# cité a<U+000A>b, « a<VT>b » a<U+000B>b ; message sur une ligne, marqué UTF-8.
for (k in list(list("LF", "a\nb", "a<U+000A>b"), list("VT", "a\u000bb", "a<U+000B>b")))
  verifier(sprintf("T11 version_fiche à l'attribut « a<%s>b » : refus fiche_non_conforme, attribut cité %s, une ligne, UTF-8",
                   k[[1L]], k[[3L]]), {
    v <- 4L; attr(v, k[[2L]]) <- 1
    lu <- paste0("integer de longueur 1, attribut(s) ", k[[3L]])
    r <- refus_de(rejouer_fiche(BASE, fiche_modifiee(version_fiche = v)))
    ok <- controle_refus(r, "construire_table", "fiche_non_conforme",
                         list(champ = "version_fiche", lu = lu, attendu = "integer de longueur 1, non NA, sans attribut"),
                         paste0("« version_fiche » : ", lu, " au lieu de"))
    if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
    else if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else ok
  })
