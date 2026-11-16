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
# témoins aux lignes 9 à 15 (ligne 15 « temoin_catalogue », #267 ; M498).
CONTROLES <- c("nature_arguments", "forme_fiche", "version_fiche", "algorithme_empreinte", "representation",
               "entrees_construction", "empreinte_donnees", "tables_reference", "temoin_code", "temoin_versions",
               "temoin_plateforme", "temoin_fiche", "temoin_empreinte_resultat", "temoin_empreinte_fichier",
               "temoin_catalogue")
STATUTS <- c("contrôle de forme", "contrôle de forme", "comparaison exacte", "comparaison exacte",
             "comparaison exacte", "contrôles délégués aux modules", "exacte ; sans rôle de sécurité",
             "exacte ; sans rôle de sécurité", "exacte ; sans rôle de sécurité", "comparaison exacte",
             "comparaison exacte", "comparaison exacte", "témoin, même machine seulement",
             "reprise de la fiche, non vérifiée par le moteur", "comparaison exacte")
VERDICTS_SUCCES <- c(rep("conforme", 8L), rep("identique", 5L), "non vérifiée", "identique")

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
verifier("T2 bilan : data.frame de 15 lignes, 5 colonnes character dans l'ordre, sans NA, noms de lignes automatiques", {
  b <- R0$bilan
  is.data.frame(b) && identical(names(b), c("controle", "attendu", "lu", "verdict", "statut")) &&
    all(vapply(b, is.character, NA)) && nrow(b) == 15L && !anyNA(b) &&
    identical(attr(b, "row.names"), 1:15) && identical(b$controle, CONTROLES)
})
verifier("T2 bilan : verdicts (1 à 8 « conforme », 9 à 13 « identique », 14 « non vérifiée », 15 « identique ») et statuts exacts",
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
    identical(b$attendu[8L], "aucune empreinte recalculée (aucune table reçue ni relue)") &&
    identical(b$lu[8L], "aucune table de référence (fiche/tables_reference : NULL)") &&
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
# Valeurs et noms de la fiche dans le bilan du rejeu (#142) : une règle unique,
# neutraliser_citation() (R/outils.R), pour toute valeur ou tout nom de la
# fiche écrit dans une cellule du bilan. Balayage de toutes les feuilles
# `character` de FICHE, chacune remplacée successivement par une chaîne
# d'encodage bytes, par « a<LF>b » et par des octets UTF-8 invalides : aucune
# erreur R (avant #142 : « translating strings with "bytes" encoding is not
# allowed », levée par sprintf() de paires_lues() pour les feuilles de versions
# et de plateforme), un rejeu ou un refus nommé, jamais une condition autre ;
# pour les rejeux rendus, 15 lignes et 5 colonnes, aucune cellule d'encodage
# bytes (avant #142 : empreintes/code, empreintes/resultat et empreintes/fichier
# la recopiaient dans bilan$lu), toutes de l'UTF-8 valide, aucune sur plusieurs
# lignes (avant #142 : une valeur à LF ou à octets invalides était recopiée
# brute), sprintf("%s") et write.csv() du bilan sans erreur. Noms : un nom de
# champ à LF ou à octets invalides, ajouté ou substitué dans versions,
# plateforme, empreintes, convention ou au premier niveau, est cité dans le
# chemin du verdict « différente : <chemin> » (avant #142 : écrit brut).
# Verdicts : règle V2 pour versions et plateforme (« différente : <chemin> »,
# chemin de la feuille sous la liste) ; V1 pour code et résultat ; ligne 14
# « non vérifiée ». Provenance à nom de fichier à LF : ligne 6 sur une ligne.
# Contre-épreuve : le balayage échoue sur l'état antérieur.
local({
  b <- "\xe9t\xe9"; Encoding(b) <- "bytes"
  repere <- "<chaîne d'encodage bytes>"
  valeurs <- list(bytes = b, controle = "a\nb", invalide = "\xe9t\xe9")
  feuilles <- function(x, chemin = character(0)) {
    if (is.list(x)) do.call(c, lapply(names(x), function(n) feuilles(x[[n]], c(chemin, n))))
    else if (is.character(x) && is.null(attributes(x))) list(chemin) else list()
  }
  attrape <- function(expr) tryCatch(expr, error = function(e) e)
  cellules <- function(bilan) unlist(lapply(bilan, as.character), use.names = FALSE)
  # Défauts du bilan d'un rejeu rendu (character(0) : aucun). Lectures par
  # octets : une cellule invalide ne doit pas lever dans le contrôle lui-même.
  defauts <- function(r) {
    if (!inherits(r, "rejeu")) return(character(0))
    k <- cellules(r$bilan)
    c(if (!identical(dim(r$bilan), c(15L, 5L))) "dimensions",
      if (any(Encoding(k) == "bytes")) "cellule d'encodage bytes",
      if (!all(validUTF8(k))) "cellule qui n'est pas de l'UTF-8 valide",
      if (any(grepl("[\n\v\f\r]", k, useBytes = TRUE))) "cellule sur plusieurs lignes",
      if (any(grepl("[^\001-\177]", k, useBytes = TRUE) & Encoding(k) != "UTF-8")) "cellule non ASCII non marquée UTF-8",
      if (inherits(attrape(sprintf("%s", k)), "error")) "sprintf() en erreur",
      if (inherits(attrape(write.csv(r$bilan, tempfile())), "error")) "write.csv() en erreur")
  }
  F <- feuilles(FICHE)
  cles <- vapply(F, paste, "", collapse = "/")
  verifier("T11 balayage des feuilles character de la fiche : les feuilles de versions, de plateforme, empreintes/code, empreintes/resultat et empreintes/fichier y figurent",
           all(c(paste0("versions/", names(FICHE$versions)), paste0("plateforme/", names(FICHE$plateforme)),
                 "empreintes/code", "empreintes/resultat", "empreintes/fichier") %in% cles) && length(cles) > 40L)
  for (nv in names(valeurs)) {
    rj <- lapply(F, function(ch) {
      f <- FICHE; f[[ch]] <- rep(valeurs[[nv]], length(FICHE[[ch]]))
      attrape(rejouer_fiche(BASE, f))
    })
    names(rj) <- cles
    if (nv == "bytes") rejeux <- rj
    libelle <- c(bytes = "chaîne d'encodage bytes", controle = "chaîne « a<LF>b »", invalide = "octets UTF-8 invalides")[[nv]]
    mal <- !vapply(rj, function(r) inherits(r, "rejeu") || inherits(r, "refus_tablexp"), NA)
    verifier(sprintf("T11 balayage, feuille remplacée par %s : aucune erreur R (rejeu rendu ou refus nommé)", libelle),
             if (any(mal)) paste(cles[mal], collapse = ", ") else TRUE)
    d <- lapply(rj, defauts)
    verifier(sprintf("T11 balayage, feuille remplacée par %s : rejeux rendus, bilan de 15 lignes et 5 colonnes, cellules en UTF-8 valide et d'une ligne, aucune d'encodage bytes, sprintf(\"%%s\") et write.csv() sans erreur", libelle),
             if (any(lengths(d) > 0L)) paste(sprintf("%s (%s)", cles[lengths(d) > 0L], vapply(d[lengths(d) > 0L], paste, "", collapse = ", ")), collapse = " ; ") else TRUE)
    verifier(sprintf("T11 balayage, feuille remplacée par %s : au moins 40 des rejeux sont rendus (le balayage porte sur des bilans)", libelle),
             sum(vapply(rj, inherits, NA, "rejeu")) >= 40L)
  }
  # Noms de champs (champs_differents()) : un nom à LF ou à octets invalides
  # ajouté dans versions, plateforme, empreintes, convention ou au premier
  # niveau, ou substitué au dernier nom de versions ou de plateforme, est cité
  # dans le chemin du verdict ; le chemin de versions et de plateforme n'a pas
  # le nom de la liste en préfixe (règle V2 en place).
  for (cas in list(list("controle", "x\ny", "x<U+000A>y"), list("invalide", "x\xe9y", "x<e9>y"))) {
    for (cont in list(character(0), "versions", "plateforme", "empreintes", "convention")) {
      ligne <- if (identical(cont, "versions")) 10L else if (identical(cont, "plateforme")) 11L else 12L
      pref <- if (length(cont) && !(cont %in% c("versions", "plateforme"))) paste0(cont, "/") else ""
      fb <- FICHE
      if (length(cont)) { z <- fb[[cont]]; z[[cas[[2L]]]] <- "1"; fb[[cont]] <- z } else fb[[cas[[2L]]]] <- "1"
      r <- attrape(rejouer_fiche(BASE, fb))
      verifier(sprintf("T11 nom de champ %s (%s) ajouté à %s : rejeu rendu, ligne %d « différente : %s%s (absent de la fiche rendue) », cellules d'une ligne en UTF-8 valide",
                       cas[[1L]], cas[[3L]], if (length(cont)) cont else "la fiche", ligne, pref, cas[[3L]]),
               if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
               else if (!identical(r$bilan$verdict[ligne], paste0("différente : ", pref, cas[[3L]], " (absent de la fiche rendue)")))
                 sprintf("verdict « %s »", r$bilan$verdict[ligne])
               else if (length(dd <- defauts(r))) paste(dd, collapse = ", ")
               else TRUE)
    }
    for (cont in c("versions", "plateforme")) {
      ligne <- if (cont == "versions") 10L else 11L
      nn <- names(FICHE[[cont]]); k <- length(nn)
      fb <- FICHE; n2 <- nn; n2[k] <- cas[[2L]]; attr(fb[[cont]], "names") <- n2
      r <- attrape(rejouer_fiche(BASE, fb))
      verifier(sprintf("T11 dernier nom de fiche$%s remplacé par le nom %s (%s) : rejeu rendu, verdict « différente : %s (absent de la fiche reçue), %s (absent de la fiche rendue) », lu cite le nom",
                       cont, cas[[1L]], cas[[3L]], nn[k], cas[[3L]]),
               if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
               else if (!identical(r$bilan$verdict[ligne], paste0("différente : ", nn[k], " (absent de la fiche reçue), ", cas[[3L]], " (absent de la fiche rendue)")))
                 sprintf("verdict « %s »", r$bilan$verdict[ligne])
               else if (!grepl(paste0(cas[[3L]], "="), r$bilan$lu[ligne], fixed = TRUE)) sprintf("lu « %s »", r$bilan$lu[ligne])
               else if (length(dd <- defauts(r))) paste(dd, collapse = ", ")
               else TRUE)
    }
  }
  # Nom de fichier de la provenance (ligne 6) : controler_provenance() ne
  # refuse pas un caractère de contrôle ; la ligne 6 le cite, sur une ligne,
  # fiche reçue sous forme d'objet ou relue de son texte.
  for (mode in c("objet", "texte")) {
    pr <- list(fichier = "a\nb.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
    xp <- construire_table(BASE, ROLES, plan_complet(), pr)
    r <- attrape(rejouer_fiche(BASE, if (mode == "objet") xp$fiche else lire_fiche(ecrire_fiche(xp$fiche))))
    verifier(sprintf("T11 provenance à nom de fichier « a<LF>b.csv », fiche en %s : rejeu rendu, ligne 6 terminée par « provenance : a<U+000A>b.csv », bilan d'une ligne par cellule", mode),
             if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else if (!endsWith(r$bilan$lu[6L], "; provenance : a<U+000A>b.csv")) sprintf("lu « %s »", r$bilan$lu[6L])
             else if (length(dd <- defauts(r))) paste(dd, collapse = ", ")
             else TRUE)
  }
  # Feuille de versions ou de plateforme : rejeu rendu, valeur citée sous le
  # repère à sa place, témoin « différente : <feuille> », autres lignes de
  # verdict inchangées.
  for (ch in c("versions", "plateforme")) {
    ligne <- if (ch == "versions") 10L else 11L
    sain <- paste(names(FICHE[[ch]]), vapply(FICHE[[ch]], function(v) v, ""), sep = "=")
    for (k in seq_along(FICHE[[ch]])) {
      nom <- names(FICHE[[ch]])[k]
      r <- rejeux[[paste0(ch, "/", nom)]]
      verifier(sprintf("T11 fiche$%s$%s de classe d'encodage bytes : rejeu rendu, lu « %s=%s », verdict « différente : %s », autres verdicts inchangés",
                       ch, nom, nom, repere, nom),
               if (!inherits(r, "rejeu")) sprintf("condition %s", class(r)[1L])
               else if (!identical(r$bilan$lu[ligne], paste(replace(sain, k, paste0(nom, "=", repere)), collapse = " ; "))) sprintf("lu « %s »", r$bilan$lu[ligne])
               else if (!identical(r$bilan$verdict[ligne], paste("différente :", nom))) sprintf("verdict « %s »", r$bilan$verdict[ligne])
               else if (!identical(r$bilan$verdict[-ligne], R0$bilan$verdict[-ligne])) "autres verdicts changés"
               else TRUE)
    }
    # Liste remplacée en bloc par une chaîne bytes : rejeu rendu, « non déterminable ».
    fb <- FICHE; fb[[ch]] <- b
    r <- attrape(rejouer_fiche(BASE, fb))
    verifier(sprintf("T11 fiche$%s chaîne d'encodage bytes (non une liste) : rejeu rendu, lu « %s », verdict « non déterminable », write.csv() du bilan sans erreur", ch, repere),
             if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else if (!identical(r$bilan$lu[ligne], repere)) sprintf("lu « %s »", r$bilan$lu[ligne])
             else if (!identical(r$bilan$verdict[ligne], "non déterminable")) sprintf("verdict « %s »", r$bilan$verdict[ligne])
             else if (inherits(attrape(write.csv(r$bilan, tempfile())), "error")) "write.csv() en erreur"
             else TRUE)
  }
  # Empreinte du code, du résultat et du fichier : repère à la ligne du témoin.
  for (cas in list(list("code", 9L, "différente"), list("resultat", 13L, "différente"), list("fichier", 14L, "non vérifiée"))) {
    r <- rejeux[[paste0("empreintes/", cas[[1L]])]]
    verifier(sprintf("T11 empreintes/%s de classe d'encodage bytes : rejeu rendu, ligne %d, lu « %s », verdict « %s »",
                     cas[[1L]], cas[[2L]], repere, cas[[3L]]),
             if (!inherits(r, "rejeu")) sprintf("condition %s", class(r)[1L])
             else identical(c(r$bilan$lu[cas[[2L]]], r$bilan$verdict[cas[[2L]]]), c(repere, cas[[3L]])))
  }
  # Plusieurs empreintes de fichier (k >= 2), dont une bytes : citées une à une.
  r <- attrape(rejouer_fiche(BASE, fiche_modifiee("empreintes/fichier" = c(b, "abc"))))
  verifier("T11 empreintes/fichier de deux éléments dont un d'encodage bytes : rejeu rendu, ligne 14 « <repère> », « abc » citées une à une",
           if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
           else identical(r$bilan$lu[14L], paste0("« ", repere, " », « abc »")))
  # Valeur de version non bytes mais à caractère de contrôle : même règle, une
  # ligne par cellule, valeur exacte non perdue (écrite <U+000A>).
  r <- attrape(rejouer_fiche(BASE, fiche_modifiee("versions/R" = "a\nb")))
  verifier("T11 versions/R « a<LF>b » : rejeu rendu, valeur citée « R=a<U+000A>b » (même règle que la valeur bytes), une ligne",
           if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
           else grepl("R=a<U+000A>b", r$bilan$lu[10L], fixed = TRUE) && !any(grepl("[\n\v\f\r]", cellules(r$bilan))))
})

# --- T11 bis : classe des données et gabarits non ASCII du bilan (revue finale
#     de B24, constats mineurs ; #142) ------------------------------------------
# Ligne 1 : la classe des données est citée par la règle unique de #142 (avant :
# collée brute, d'où un LF dans la cellule pour « ma<LF>classe » et une cellule
# qui n'est pas de l'UTF-8 valide pour des octets invalides). Verdicts
# « différente : » et « (absent de la fiche reçue / rendue) » : texte exact avec
# un nom non ASCII, marqué UTF-8 et non marqué (sous la locale C, avec le
# paquet installé sous C, ces deux cas sont contrôlés par tests/controles/
# locale_c.R : la bibliothèque des batteries est installée en UTF-8).
local({
  attrape <- function(expr) tryCatch(expr, error = function(e) e)
  u <- function(...) intToUtf8(c(...))
  intact <- R0$bilan$lu[1L]
  verifier("T11 bis ligne 1 : fiche intacte, « classe data.frame » ; classe seule citée, aucun LF",
           grepl("; donnees : classe data.frame, 4 lignes, 7 colonnes$", intact) && !grepl("\n", intact))
  for (cas in list(list("non ASCII", paste0("cl", u(0xe9)), paste0("cl", u(0xe9))),
                   list("à LF", "ma\nclasse", "ma<U+000A>classe"),
                   list("à octets invalides", "cl\xe9", "cl<e9>"),
                   list("à deux éléments de contrôle", c("a\vb", "c\rd"), "a<U+000B>b/c<U+000D>d"))) {
    dn <- BASE; class(dn) <- c(cas[[2L]], "data.frame")
    r <- attrape(rejouer_fiche(dn, FICHE))
    verifier(sprintf("T11 bis ligne 1 : classe des données %s, rejeu rendu, classe citée « %s », cellule d'une ligne en UTF-8 valide, autres lignes inchangées",
                     cas[[1L]], cas[[3L]]),
             if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
             else if (!identical(r$bilan$lu[1L], sub("classe data.frame", paste0("classe ", cas[[3L]], "/data.frame"), intact, fixed = TRUE)))
               sprintf("lu « %s »", r$bilan$lu[1L])
             else if (!validUTF8(r$bilan$lu[1L]) || grepl("[\n\v\f\r]", r$bilan$lu[1L], useBytes = TRUE))
               "cellule invalide ou sur plusieurs lignes"
             else if (!identical(r$bilan$verdict, VERDICTS_SUCCES)) "verdicts changés"
             else TRUE)
  }
  # Noms non ASCII : texte exact des verdicts, marqué UTF-8 (nom marqué, puis
  # mêmes octets sans marque).
  E <- u(0xe9)
  nom_brut <- rawToChar(as.raw(c(0xc3, 0xa9)))
  for (cas in list(list("marqué UTF-8", E), list("non marqué", nom_brut))) {
    for (cont in c("versions", "empreintes")) {
      fb <- FICHE; z <- fb[[cont]]; z[[cas[[2L]]]] <- "1"; fb[[cont]] <- z
      ligne <- if (cont == "versions") 10L else 12L
      r <- attrape(rejouer_fiche(BASE, fb))
      attendu <- paste0("diff", E, "rente : ", if (cont == "empreintes") "empreintes/", E, " (absent de la fiche rendue)")
      verifier(sprintf("T11 bis nom non ASCII %s ajouté à fiche$%s : ligne %d « différente : … %s (absent de la fiche rendue) », octets exacts, marqué UTF-8",
                       cas[[1L]], cont, ligne, E),
               if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
               else identical(charToRaw(r$bilan$verdict[ligne]), charToRaw(attendu)) &&
                 identical(Encoding(r$bilan$verdict[ligne]), "UTF-8"))
    }
  }
  verifier("T11 bis verdict_chemins() : nom non ASCII absent de la fiche reçue ; « re<ç>ue » en UTF-8 valide, octets exacts",
           {
             v <- tablexp:::verdict_chemins(setNames(list(1, 2), c("a", E)), list(a = 1), "fiche")
             identical(charToRaw(v), charToRaw(paste0("diff", E, "rente : ", E, " (absent de la fiche re", u(0xe7), "ue)"))) &&
               identical(Encoding(v), "UTF-8")
           })
})

# --- T12 : témoin « catalogue », ligne 15 (#267 ; ADR 0015, seconde annotation
#     du 15/11/2026, (b) ; ADR 0012, annotation du 15/11/2026, (b) ; DF1 à DF7,
#     M498). Critères B1 à B8 de la spécification d'actuary-approfondi,
#     reformulés sur la ligne 15, et critère de forme (B9, invariant des
#     métadonnées : tests/unitaires/test_tables_embarquees.R). Valeur calculée
#     à la main (textes, verdicts, statuts) ; identité exacte (construction
#     rejouée identical() hors execution ; lignes 1 à 14).

# Construction de BASE avec la table embarquée TV 88-90 sous la clé « tv »
# (positionnement, sans segmentation) ; fiche relue de son texte.
PLAN_TV <- local({
  p <- plan_complet()
  p$segmentation <- character(0)
  p$etapes$brut$estimateurs <- "poisson"
  p$etapes$positionnement <- list(methode = "smr", table_reference = "tv", plage_ages = c(60, 80), niveau = 0.95)
  p
})
X_TV <- construire_table(BASE, ROLES, PLAN_TV, tables_reference = list(tv = table_embarquee("TV 88-90")))
FICHE_TV <- lire_fiche(ecrire_fiche(X_TV$fiche))
SOURCE_TV <- tables_embarquees()$source[tables_embarquees()$nom == "TV 88-90"]
# Fiche FICHE_TV dont le champ `champ` de l'élément tv vaut `v` (NULL gardé :
# le champ reste présent, de valeur NULL).
fiche_tv <- function(champ, v, f = FICHE_TV) { f$tables_reference$tv[champ] <- list(v); f }
STATUT_REGIME <- c(
  identique = "comparaison exacte ; empreinte du code identique (temoin_code) : écart non imputable à une évolution du catalogue",
  differente = paste("comparaison exacte ; empreinte du code différente (temoin_code) : écart imputable à la fiche ou à",
                     "une évolution du catalogue, non départagées"),
  non_determinable = paste("comparaison exacte ; empreinte du code non déterminable (temoin_code) : origine de l'écart",
                           "non déterminable"))
ligne15 <- function(r) unlist(r$bilan[15L, ], use.names = FALSE)
attrape <- function(expr) tryCatch(expr, error = function(e) e)
R_TV <- rejouer_fiche(BASE, FICHE_TV)

verifier("T12 forme : bilan de 15 lignes ; controle[15] « temoin_catalogue » ; lignes 1 à 14 inchangées en place et en nom, statuts 1 à 14 inchangés", {
  b <- R_TV$bilan
  identical(nrow(b), 15L) && identical(b$controle[15L], "temoin_catalogue") &&
    identical(b$controle[1:14],
              c("nature_arguments", "forme_fiche", "version_fiche", "algorithme_empreinte", "representation",
                "entrees_construction", "empreinte_donnees", "tables_reference", "temoin_code", "temoin_versions",
                "temoin_plateforme", "temoin_fiche", "temoin_empreinte_resultat", "temoin_empreinte_fichier")) &&
    identical(b$statut[1:14], STATUTS[1:14]) && identical(R0$bilan$controle, b$controle)
})
verifier("T12 B5 fiche intacte à table embarquée : construction identical() hors execution ; ligne 15 « identique », « comparaison exacte », textes de la clé", {
  identical(sans_execution(R_TV$construction), sans_execution(X_TV)) && identical(R_TV$bilan$verdict, VERDICTS_SUCCES) &&
    identical(ligne15(R_TV), c("temoin_catalogue", "tv : métadonnées du catalogue du package installé",
                               "tv : métadonnées de la fiche", "identique", "comparaison exacte"))
})
verifier("T12 B5 fiche intacte, empreinte du code NA : ligne 9 « non déterminable », ligne 15 « identique », « comparaison exacte » (régime dit seulement en cas d'écart, DF2)", {
  r <- rejouer_fiche(BASE, fiche_modifiee("empreintes/code" = NA_character_, f = FICHE_TV))
  identical(r$bilan$verdict[9L], "non déterminable") && identical(ligne15(r)[4:5], c("identique", "comparaison exacte"))
})
verifier("T12 B8 bornes : fiche sans table de référence (R0) -> ligne 15 « identique », textes « aucune métadonnée du catalogue opposée » et « aucune table de statut « embarquee » »", {
  identical(ligne15(R0), c("temoin_catalogue", "aucune métadonnée du catalogue opposée (aucune table de statut « embarquee »)",
                           "aucune table de statut « embarquee » dans fiche/tables_reference", "identique",
                           "comparaison exacte"))
})
verifier("T12 B1 fausse source de la table embarquée : rejeu rendu, sans refus ; ligne 15 cite clé, champ, valeur de la fiche (lu) et du catalogue installé (attendu), régime « identique » ; construction identical() hors execution", {
  r <- attrape(rejouer_fiche(BASE, fiche_tv("source", "ma source")))
  if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
  else identical(ligne15(r), c("temoin_catalogue",
                               sprintf("tv/source : « %s » (catalogue du package installé)", SOURCE_TV),
                               "tv/source : « ma source »", "différente : tables_reference/tv/source",
                               STATUT_REGIME[["identique"]])) &&
    identical(SOURCE_TV, "inst/extdata/tables/tv_88-90.provenance.dcf") &&
    identical(sans_execution(r$construction), sans_execution(X_TV))
})
verifier("T12 B1 contre-épreuve : le témoin « fiche » (ligne 12) nommait déjà le champ, sans ses valeurs ; ni la valeur de la fiche ni celle du catalogue n'y figurent", {
  b <- rejouer_fiche(BASE, fiche_tv("source", "ma source"))$bilan
  identical(b$verdict[12L], "différente : tables_reference/tv/source") &&
    !any(grepl("ma source", b[12L, ], fixed = TRUE)) && !any(grepl(SOURCE_TV, b[12L, ], fixed = TRUE))
})
verifier("T12 B8 bornes : deux champs en écart (source, forme_origine) -> textes joints par « ; » dans l'ordre de CHAMPS_TABLE_FICHE, chemins par « , »", {
  r <- rejouer_fiche(BASE, fiche_tv("forme_origine", "qx", fiche_tv("source", "ma source")))
  identical(ligne15(r)[2:4],
            c(sprintf("tv/source : « %s » (catalogue du package installé) ; tv/forme_origine : « lx » (catalogue du package installé)",
                      SOURCE_TV),
              "tv/source : « ma source » ; tv/forme_origine : « qx »",
              "différente : tables_reference/tv/source, tables_reference/tv/forme_origine"))
})
verifier("T12 B4 régime lu sur le verdict de la ligne 9 (verdict_chaines(), non identical()) : code identique, différent, NA, absent -> trois statuts", {
  f <- fiche_tv("source", "ma source")
  st <- function(...) { r <- rejouer_fiche(BASE, fiche_modifiee(..., f = f)); c(r$bilan$verdict[9L], r$bilan$statut[15L]) }
  identical(rbind(st(), st("empreintes/code" = FAUX_MD5), st("empreintes/code" = NA_character_), st("empreintes/code" = NULL)),
            rbind(c("identique", STATUT_REGIME[["identique"]]), c("différente", STATUT_REGIME[["differente"]]),
                  c("non déterminable", STATUT_REGIME[["non_determinable"]]),
                  c("non déterminable", STATUT_REGIME[["non_determinable"]])))
})
verifier("T12 B8 bornes : table importée de source modifiée -> ligne 12 « différente », ligne 15 « identique », sans comparaison au catalogue (hors de #267)", {
  ti <- table_reference(data.frame(age = 0:110, sexe = "ensemble", qx = c(rep(0.01, 110L), 1)), "imp", "importee", "test")
  x <- construire_table(BASE, ROLES, PLAN_TV, tables_reference = list(tv = ti))
  f <- lire_fiche(ecrire_fiche(x$fiche)); f$tables_reference$tv$source <- "autre"
  r <- rejouer_fiche(BASE, f, list(tv = ti))
  identical(r$bilan$verdict[12L], "différente : tables_reference/tv/source") &&
    identical(ligne15(r)[4:5], c("identique", "comparaison exacte")) &&
    identical(ligne15(r)[2L], "aucune métadonnée du catalogue opposée (aucune table de statut « embarquee »)")
})
# Balayage de 23 formes altérées de source, forme_origine et parametres : les
# dix-sept de l'avis (F7), dont quatre sous une valeur de même classe (chaîne
# « bytes » de source, liste à nom « bytes » de source et de parametres, chaîne
# de parametres), et six propres au test (liste écrite ici ; ADR 0015, seconde
# annotation du 15/11/2026, (b)) : valeurs illisibles, NA, vides,
# NULL, de type ou de classe autre, « bytes » (chaîne, classe, nom de liste),
# caractères de contrôle et séparateur U+2028, octets invalides.
local({
  b <- function(s) { Encoding(s) <- "bytes"; s }
  lb <- list(1); names(lb) <- b("n\xe9")
  repere <- "« <chaîne d'encodage bytes> »"
  formes <- list(
    list("source", "ma source", "« ma source »"), list("source", NA_character_, "NA"),
    list("source", character(0), "illisible (character de longueur 0)"),
    list("source", c("a", "b"), "illisible (character de longueur 2)"), list("source", NULL, "absente"),
    list("source", 1, "illisible (double de longueur 1)"), list("source", b("x\xe9"), repere),
    list("source", "a\nb c", "« a<U+000A>b<U+2028>c »"), list("source", "\xe9t\xe9", "« <e9>t<e9> »"),
    list("source", list(a = 1), "illisible (list de longueur 1)"),
    list("source", structure("x", class = "bytes"), "illisible (character de longueur 1)"),
    list("forme_origine", "qx", "« qx »"), list("forme_origine", NA_character_, "NA"),
    list("forme_origine", lb, "illisible (list de longueur 1)"),
    list("parametres", list(a = 1), "illisible (list de longueur 1)"),
    list("parametres", lb, "illisible (list de longueur 1)"), list("parametres", "x", "« x »"),
    # Formes de l'avis (F7) ajoutées à la reprise de #267 (visa d'actuary-approfondi).
    list("source", 3L, "illisible (integer de longueur 1)"), list("source", "", "«  »"),
    list("source", "a\nb\u0007c", "« a<U+000A>b<U+0007>c »"),
    list("source", structure("x", class = "k"), "illisible (character de longueur 1)"),
    list("source", lb, "illisible (list de longueur 1)"),
    list("forme_origine", b("abc\xe9"), repere))
  attendu_catalogue <- c(source = sprintf("« %s »", SOURCE_TV), forme_origine = "« lx »", parametres = "absente")
  cellules <- function(bilan) unlist(lapply(bilan, as.character), use.names = FALSE)
  ecarts <- character(0)
  for (i in seq_along(formes)) {
    ch <- formes[[i]][[1L]]
    r <- attrape(rejouer_fiche(BASE, fiche_tv(ch, formes[[i]][[2L]])))
    e <- if (!inherits(r, "rejeu")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)) else {
      k <- cellules(r$bilan)
      chemin <- paste0("tables_reference/tv/", ch)
      c(if (!identical(nrow(r$bilan), 15L)) "lignes",
        if (!identical(ligne15(r), c("temoin_catalogue",
                                     sprintf("tv/%s : %s (catalogue du package installé)", ch, attendu_catalogue[[ch]]),
                                     sprintf("tv/%s : %s", ch, formes[[i]][[3L]]), paste("différente :", chemin),
                                     STATUT_REGIME[["identique"]]))) "ligne 15",
        # B2 : chaque chemin de la ligne 15 est cité par le témoin « fiche ».
        if (!grepl(chemin, r$bilan$verdict[12L], fixed = TRUE)) "ligne 12 sans le chemin",
        if (!identical(sans_execution(r$construction), sans_execution(X_TV))) "construction",
        if (!all(validUTF8(k)) || any(Encoding(k) == "bytes")) "cellule non UTF-8",
        if (any(grepl("[\n\v\f\r]", k, useBytes = TRUE))) "cellule sur plusieurs lignes",
        if (inherits(attrape(write.csv(r$bilan, tempfile())), "error")) "write.csv() en erreur")
    }
    if (length(e)) ecarts <- c(ecarts, sprintf("forme %d (%s) : %s", i, ch, paste(e, collapse = ", ")))
  }
  verifier("T12 B3, B2, B6 balayage de 23 formes : les dix-sept de l'avis (F7), dont quatre sous une valeur de même classe, et six propres au test, altérées de source, forme_origine et parametres : aucune erreur R, aucun refus ; ligne 15 au texte attendu (valeurs citées par la règle de #142) ; chemin cité aussi par la ligne 12 ; construction identical() hors execution ; cellules UTF-8 sur une ligne, write.csv() sans erreur",
           if (length(formes) != 23L) "23 formes" else if (length(ecarts)) ecarts else TRUE)
})
verifier("T12 B6, B7 aucun refus nouveau : MENTIONS_RAPPORT et TEXTE_REJEU_FICHE inchangés (MD5 de leurs octets UTF-8 relevés à 8a81102)", {
  o <- function(x) tablexp:::md5_octets(charToRaw(enc2utf8(paste(x, collapse = "\n"))))
  m <- tablexp:::MENTIONS_RAPPORT
  identical(c(o(c(names(m), unname(m))), o(tablexp:::TEXTE_REJEU_FICHE)),
            c("aa6024502e80e71c23a8e24106122012", "ddbaf36e1e6fa260b51e57ca46e6604f"))
})
verifier("T12 B8 bornes, par injection (catalogue à deux tables) : écarts de deux clés dans l'ordre de la fiche ; textes joints par « ; » ; clé intacte citée (option (b) du visa d'actuary-approfondi)", {
  cat_ <- list(A = list(identifiant = "a", sexe = "H", format = 1L, lignes = c("age,lx", "0,1000", "1,990", "2,970")),
               B = list(identifiant = "b", sexe = "F", format = 1L, lignes = c("age,lx", "0,1000", "1,995", "2,985")))
  el <- function(n) tablexp:::element_table_fiche(tablexp:::table_du_catalogue(n, cat_))
  ft <- list(a = replace(el("A"), "source", list("s1")), b = el("B"), c = replace(el("B"), "forme_origine", list("qx")))
  tr <- tablexp:::controler_tables_rejeu(ft, NULL, "code", cat_)
  t15 <- tablexp:::temoin_catalogue(tr$ecarts, "différente")
  identical(tr$ecarts, list(cles = c("a", "b", "c"), cle = c("a", "c"), champ = c("source", "forme_origine"), lu = list("s1", "qx"),
                            attendu = list("inst/extdata/tables/a.provenance.dcf", "lx"))) &&
    identical(unname(t15), c(paste("a/source : « inst/extdata/tables/a.provenance.dcf » (catalogue du package installé) ;",
                                   "b : métadonnées du catalogue du package installé ;",
                                   "c/forme_origine : « lx » (catalogue du package installé)"),
                             "a/source : « s1 » ; b : métadonnées de la fiche ; c/forme_origine : « qx »",
                             "différente : tables_reference/a/source, tables_reference/c/forme_origine",
                             STATUT_REGIME[["differente"]]))
})
verifier("T12 ecarts_catalogue() : élément du catalogue -> aucun écart ; valeurs brutes rendues (NULL compris), dans l'ordre de CHAMPS_TABLE_FICHE", {
  t <- table_embarquee("TV 88-90")
  e <- tablexp:::element_table_fiche(t)
  e2 <- e; e2["parametres"] <- list(list(a = 1)); e2$source <- "s"
  identical(tablexp:::ecarts_catalogue(e, t), list(champ = character(0), lu = list(), attendu = list())) &&
    identical(tablexp:::ecarts_catalogue(e2, t),
              list(champ = c("source", "parametres"), lu = list("s", list(a = 1)), attendu = list(SOURCE_TV, NULL)))
})
