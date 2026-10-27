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

CONTROLES <- c("nature_arguments", "forme_fiche", "version_fiche", "algorithme_empreinte", "representation",
               "entrees_construction", "empreinte_donnees", "temoin_code", "temoin_versions", "temoin_plateforme",
               "temoin_fiche", "temoin_empreinte_resultat", "temoin_empreinte_fichier")
STATUTS <- c("contrôle de forme", "contrôle de forme", "comparaison exacte", "comparaison exacte",
             "comparaison exacte", "contrôles délégués aux modules", "exacte ; sans rôle de sécurité",
             "exacte ; sans rôle de sécurité", "comparaison exacte", "comparaison exacte", "comparaison exacte",
             "témoin, même machine seulement", "reprise de la fiche, non vérifiée par le moteur")
VERDICTS_SUCCES <- c(rep("conforme", 7L), rep("identique", 5L), "non vérifiée")

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

verifier("T1 rejouer_fiche : exportée, arguments (donnees, fiche)",
         "rejouer_fiche" %in% getNamespaceExports("tablexp") &&
           identical(names(formals(rejouer_fiche)), c("donnees", "fiche")))
verifier("T1 VERSIONS_FICHE_REJEU : integer sans attribut ni NA, identical() à VERSION_FICHE (4L)", {
  v <- tablexp:::VERSIONS_FICHE_REJEU
  is.integer(v) && is.null(attributes(v)) && !anyNA(v) && identical(v, tablexp:::VERSION_FICHE) && identical(v, 4L)
})

# --- T2 : structure d'un rejeu réussi (valeur calculée à la main ; identité exacte)

verifier("T2 rejeu réussi : classe rejeu, éléments construction et bilan",
         identical(class(R0), "rejeu") && identical(names(R0), c("construction", "bilan")))
verifier("T2 bilan : data.frame de 13 lignes, 5 colonnes character dans l'ordre, sans NA, noms de lignes automatiques", {
  b <- R0$bilan
  is.data.frame(b) && identical(names(b), c("controle", "attendu", "lu", "verdict", "statut")) &&
    all(vapply(b, is.character, NA)) && nrow(b) == 13L && !anyNA(b) &&
    identical(attr(b, "row.names"), 1:13) && identical(b$controle, CONTROLES)
})
verifier("T2 bilan : verdicts (1 à 7 « conforme », 8 à 12 « identique », 13 « non vérifiée ») et statuts exacts",
         identical(R0$bilan$verdict, VERDICTS_SUCCES) && identical(R0$bilan$statut, STATUTS))
verifier("T2 bilan : attendu et lu des lignes 3, 4, 5, 7, 8 et 12 égaux aux constantes et aux valeurs de la fiche", {
  b <- R0$bilan; e <- FICHE$empreintes
  identical(b$attendu[c(3, 4, 5, 7, 8, 12)],
            c("4", "md5", "tablexp canonique 2", tablexp:::empreinte_donnees(BASE, ROLES), EMPREINTE_CODE,
              R0$construction$fiche$empreintes$resultat)) &&
    identical(b$lu[c(3, 4, 5, 7, 8, 12)], c("4", e$algorithme, e$representation, e$donnees, e$code, e$resultat))
})
verifier("T2 bilan : lignes 1, 2, 6, 11 et 13 (textes fixes et composés)", {
  b <- R0$bilan
  identical(b$attendu[1L], "fiche : liste nommée ; donnees : data.frame") &&
    identical(b$lu[1L], "fiche : liste nommée de 24 champs ; donnees : classe data.frame, 4 lignes, 7 colonnes") &&
    identical(b$lu[2L], "champs lus de la forme attendue") &&
    identical(b$lu[6L], paste("étapes : donnees, exposition, brut ; rôles : id_contrat, id_tete, sexe, date_naissance,",
                              "date_entree, date_sortie, cause_sortie ; recette : lignes_dates_completes ; provenance : NULL")) &&
    identical(b$lu[11L], "fiche reçue, même périmètre") && identical(b$lu[13L], "NA") &&
    identical(b$attendu[13L], "non recalculée par le moteur (couche de lecture, E4.4)")
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
       f = fiche_texte("version_fiche\tinteger\t1\t4", "version_fiche\tdouble\t1\t3"),
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
       details = list(lu = 99L, admises = 4L, tablexp = FICHE$versions$tablexp, code = FICHE$empreintes$code,
                      code_installe = EMPREINTE_CODE),
       contient = c("version_fiche 99 lue", "admise(s) par ce code : 4.",
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
  list("empreinte du résultat altérée : pas de refus, ligne 12 « différente »",
       fiche_modifiee("empreintes/resultat" = FAUX_MD5), list("12" = "différente")),
  list("empreintes du résultat et du code NA : « non déterminable »",
       fiche_modifiee("empreintes/resultat" = NA_character_, "empreintes/code" = NA_character_),
       list("8" = "non déterminable", "12" = "non déterminable")),
  list("empreinte du code altérée", fiche_modifiee("empreintes/code" = FAUX_MD5), list("8" = "différente")),
  list("versions$R modifiée", fiche_modifiee("versions/R" = "R version 0"), list("9" = "différente : R")),
  list("plateforme$blas modifiée", fiche_modifiee("plateforme/blas" = "autre"), list("10" = "différente : blas")),
  list("versions retiré", fiche_modifiee(versions = NULL), list("9" = "non déterminable")),
  list("champ x ajouté à plateforme", fiche_modifiee("plateforme/x" = "y"),
       list("10" = "différente : x (absent de la fiche rendue)")),
  list("transition_code modifié", fiche_modifiee(transition_code = "autre"),
       list("11" = "différente : transition_code")),
  list("estimateurs inversés dans le plan : résultat identique",
       fiche_modifiee("plan/etapes/brut/estimateurs" = rev(FICHE$plan$etapes$brut$estimateurs)),
       list("11" = "différente : plan/etapes/brut/estimateurs")),
  list("niveau 0,99 : résultat différent", fiche_modifiee("plan/etapes/brut/niveau" = 0.99),
       list("11" = "différente : hypotheses_brut/niveau", "12" = "différente")),
  list("deux champs de premier niveau permutés",
       FICHE[replace(names(FICHE), match(c("transition_code", "fin_effective"), names(FICHE)),
                     c("fin_effective", "transition_code"))],
       list("11" = "différente : fiche (ordre des champs ou attributs)"))))
  verifier(sprintf("T7 témoins, %s", cas[[1L]]), ecart_verdicts(verdicts(cas[[2L]]), do.call(attendus_temoins, cas[[3L]])))

# --- T8 : cas adverses de l'ADR 0012, point 3.9 (constat de mesure) -------------

sans_lignes_conservees <- function(x) { x <- sans_execution(x); x$etapes$donnees$resultat$lignes_conservees <- NULL; x }
BASE_EXTRA <- cbind(BASE, extra = 1:4)
verifier("T8 lignes permutées : refus empreinte_donnees_discordante",
         controle_refus(refus_de(rejouer_fiche(BASE_PERMUTEE, FICHE)), "construire_table", "empreinte_donnees_discordante"))
verifier("T8 colonne hors rôles ajoutée : pas de refus, témoins 8 à 12 « identique », objet identical() hors lignes_conservees seulement", {
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
verifier("T8 colonne de rôle renommée, rôles de la fiche adaptés : passe, témoin fiche « identique », ligne 12 « différente »",
         ecart_verdicts(verdicts(fiche_modifiee("roles/sexe" = "genre"), BASE_GENRE), attendus_temoins("12" = "différente")))
verifier("T8 colonne de rôle renommée sans adaptation : refus colonne_absente",
         controle_refus(refus_de(rejouer_fiche(BASE_GENRE, FICHE)), "donnees", "colonne_absente"))
verifier("T8 fiche sans puis avec provenance : rejouées, identical() hors execution, empreintes du résultat distinctes, ligne 13", {
  prov <- list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef", algorithme = "md5")
  xp <- construire_table(BASE, ROLES, plan_complet(), prov)
  rp <- rejouer_fiche(BASE, lire_fiche(ecrire_fiche(xp$fiche)))
  identical(sans_execution(rp$construction), sans_execution(xp)) &&
    identical(sans_execution(R0$construction), sans_execution(X)) &&
    !identical(xp$fiche$empreintes$resultat, X$fiche$empreintes$resultat) &&
    identical(R0$bilan$lu[13L], "NA") && identical(rp$bilan$lu[13L], prov$empreinte) &&
    identical(c(R0$bilan$verdict[13L], rp$bilan$verdict[13L]), rep("non vérifiée", 2L)) &&
    identical(rp$bilan$verdict, VERDICTS_SUCCES) && endsWith(rp$bilan$lu[6L], "; provenance : base.csv") &&
    endsWith(R0$bilan$lu[6L], "; provenance : NULL")
})
verifier("T8 fiche de version 3 fabriquée d'une fiche 4 écrite : lue sans refus, rejeu refusé, construire_table() directe passe", {
  noms6 <- TEXTE[startsWith(TEXTE, "plateforme/@noms\t")]
  noms5 <- sub("\t\"bibliotheque_c\"$", "", sub("^plateforme/@noms\tcharacter\t6\t", "plateforme/@noms\tcharacter\t5\t", noms6))
  l <- TEXTE[!startsWith(TEXTE, "plateforme/bibliotheque_c\t")]
  l[l == "version_fiche\tinteger\t1\t4"] <- "version_fiche\tinteger\t1\t3"
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
})
