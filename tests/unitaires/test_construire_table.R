# -----------------------------------------------------------------------------
#  tests/unitaires/test_construire_table.R : point d'entrée, plan, objet
#  résultat, fiche en mémoire et empreinte (issue #11 ; ADR 0005, points 3
#  et 6 ; ADR 0007, points 3.1, 3.4, 5, 6 et 9, annotation du 17/10/2026 ;
#  spécification d'actuary, A.3)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : codes de refus, structure de l'objet et de
#    la fiche, empreinte MD5 d'un petit data.frame dont le texte canonique a
#    été écrit par un autre outil (printf et md5sum de GNU coreutils, hors R) ;
#  - identité exacte : sur un portefeuille simulé (#10), exposition totale =
#    somme des durées dans la période, décès et rachats comptés = événements
#    simulés dans la période réduite du délai ;
#  - recoupement : somme des années d'âge recalculée ligne par ligne, les
#    anniversaires par as.Date() et les âges extrêmes de chaque ligne par
#    age_revolu() du module donnees (comparaison, pas recoupement), dont
#    l'exposition ne se sert pas.
# -----------------------------------------------------------------------------

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")

BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"), tete = c("T1", "T2", "T3", "T3"), sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"), stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")
plan_type <- function() list(
  transition = VD, periode = PER, delai_declaration = 0, segmentation = "sexe",
  etapes = list(donnees = list(exclusions = character(0)),
                exposition = list(convention = "plus_fine", par_annee = FALSE)))
modifier <- function(plan, ...) { m <- list(...); for (n in names(m)) plan[n] <- list(m[[n]]); plan }
refus <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)

# --- exports et signature -------------------------------------------------------

verifier("NAMESPACE : exports = catalogue_roles, construire_table, simuler_portefeuille, table_reference, valider_plan (ADR 0007, point 3.4 ; #9, #10, #11)",
         identical(sort(getNamespaceExports("tablexp")),
                   c("catalogue_roles", "construire_table", "simuler_portefeuille", "table_reference",
                     "valider_plan")))
verifier("construire_table : signature (donnees, roles, plan, provenance = NULL)",
         identical(names(formals(construire_table)), c("donnees", "roles", "plan", "provenance")) &&
           is.null(formals(construire_table)$provenance))
verifier("valider_plan : signature (plan) ; plan valide rendu inchangé",
         identical(names(formals(valider_plan)), "plan") && identical(valider_plan(plan_type()), plan_type()))

# --- refus de valider_plan (valeur calculée à la main : codes) ----------------

cas_refus <- list(
  list("plan non liste", "plan_mal_forme", "a"),
  list("plan sans noms", "plan_mal_forme", list(1, 2)),
  list("plan vide", "plan_mal_forme", list()),
  list("champ inconnu (unite_observation, M48)", "champ_inconnu",
       modifier(plan_type(), unite_observation = "tete")),
  list("période absente", "champ_manquant", plan_type()[setdiff(names(plan_type()), "periode")]),
  list("segmentation absente (requise, M49)", "champ_manquant",
       plan_type()[setdiff(names(plan_type()), "segmentation")]),
  list("délai absent (zéro explicite requis)", "champ_manquant",
       plan_type()[setdiff(names(plan_type()), "delai_declaration")]),
  list("graine déclarée sans étape aléatoire", "graine_inattendue", modifier(plan_type(), graine = 1L)),
  list("etapes non nommée", "etapes_mal_formees", modifier(plan_type(), etapes = list(list()))),
  list("aucune étape", "aucune_etape", modifier(plan_type(), etapes = list())),
  list("étape inconnue", "etape_inconnue",
       modifier(plan_type(), etapes = c(plan_type()$etapes, list(lissage = list())))),
  list("étape en double", "etape_en_double",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           donnees = list(exclusions = character(0))))),
  list("étapes hors ordre", "etapes_hors_ordre",
       modifier(plan_type(), etapes = rev(plan_type()$etapes))),
  list("exposition sans donnees", "etape_prealable_manquante",
       modifier(plan_type(), etapes = plan_type()$etapes["exposition"])),
  list("paramètres non liste", "parametres_mal_formes",
       modifier(plan_type(), etapes = list(donnees = "aucune"))),
  list("paramètre inconnu (segmentation dans exposition, M49)", "parametre_inconnu",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "plus_fine", par_annee = FALSE,
                                                             segmentation = "sexe")))),
  list("paramètre manquant (par_annee)", "parametre_manquant",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "plus_fine")))),
  list("paramètre manquant (exclusions)", "parametre_manquant",
       modifier(plan_type(), etapes = list(donnees = list()))),
  list("période vide (début > fin - délai)", "periode_vide", modifier(plan_type(), delai_declaration = 4000)),
  list("délai négatif", "delai_invalide", modifier(plan_type(), delai_declaration = -1)),
  list("délai non entier", "delai_invalide", modifier(plan_type(), delai_declaration = 1.5)),
  list("période mal formée", "periode_invalide", modifier(plan_type(), periode = list(debut = "2015-01-01", fin = PER$fin))),
  list("période : nom partiel debut_obs (aucun appariement partiel)", "periode_invalide",
       modifier(plan_type(), periode = list(debut_obs = PER$debut, fin = PER$fin))),
  list("période : champ en trop", "periode_invalide",
       modifier(plan_type(), periode = list(debut = PER$debut, fin = PER$fin, origine = PER$debut))),
  list("transition : nom partiel departement (aucun appariement partiel)", "transition_inconnue",
       modifier(plan_type(), transition = list(departement = "vivant", arrivee = "deces"))),
  list("transition : champ en trop", "transition_inconnue",
       modifier(plan_type(), transition = list(depart = "vivant", arrivee = "deces", etat = "x"))),
  list("transition inconnue", "transition_inconnue",
       modifier(plan_type(), transition = list(depart = "vivant", arrivee = "rachat"))),
  list("segmentation hors catalogue", "segmentation_invalide", modifier(plan_type(), segmentation = "region")),
  list("code d'exclusion inconnu", "exclusion_inconnue",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = "motif_imaginaire"),
                                           exposition = list(convention = "plus_fine", par_annee = FALSE)))),
  list("forçage vers age_fourni", "convention_non_mise_en_oeuvre",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "age_fourni", par_annee = FALSE)))),
  list("convention inconnue", "convention_invalide",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "deduite", par_annee = FALSE)))),
  list("par_annee non logique", "par_annee_invalide",
       modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                           exposition = list(convention = "plus_fine", par_annee = 1))))
)
for (k in cas_refus)
  verifier(sprintf("valider_plan : %s -> refus %s", k[[1]], k[[2]]), leve_refus(valider_plan(k[[3]]), k[[2]]))

verifier("valider_plan : refus structurels portés par le module construire_table, valeurs par le module qui les contrôle (donnees, exposition)", {
  r1 <- refus(valider_plan(modifier(plan_type(), etapes = list(donnees = list()))))
  r2 <- refus(valider_plan(modifier(plan_type(), delai_declaration = -1)))
  r3 <- refus(valider_plan(modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                                            exposition = list(convention = "age_fourni", par_annee = FALSE)))))
  identical(c(r1$module, r2$module, r3$module), c("construire_table", "donnees", "exposition"))
})
verifier("valider_plan : le refus de paramètre manquant nomme l'étape et le paramètre", {
  r <- refus(valider_plan(modifier(plan_type(), etapes = list(donnees = list(exclusions = character(0)),
                                                           exposition = list(convention = "plus_fine")))))
  identical(r$details, list(etape = "exposition", parametre = "par_annee")) &&
    grepl("par_annee", conditionMessage(r), fixed = TRUE) && grepl("exposition", conditionMessage(r), fixed = TRUE)
})
verifier("valider_plan : plan réduit à l'étape donnees admis",
         identical(valider_plan(modifier(plan_type(), etapes = plan_type()$etapes["donnees"])),
                   modifier(plan_type(), etapes = plan_type()$etapes["donnees"])))

# --- construire_table : refus et validation avant exécution ------------------

verifier("construire_table : plan validé avant toute exécution (données invalides et plan invalide : refus du plan)", {
  d <- BASE; d$naissance[1] <- NA
  leve_refus(construire_table(d, ROLES, modifier(plan_type(), delai_declaration = -1)), "delai_invalide")
})
verifier("construire_table : rôle requis absent (date_sortie) -> refus aucune_recette, message qui nomme le rôle et la recette", {
  r <- refus(construire_table(BASE, ROLES[names(ROLES) != "date_sortie"], plan_type()))
  !is.null(r) && identical(r$motif, "aucune_recette") && identical(r$details$role, "date_sortie") &&
    grepl("date_sortie", conditionMessage(r), fixed = TRUE) &&
    grepl("lignes_dates_completes", conditionMessage(r), fixed = TRUE)
})
verifier("construire_table : rôle employé par la segmentation absent -> refus role_requis_manquant",
         leve_refus(construire_table(BASE, ROLES[names(ROLES) != "sexe"], plan_type()), "role_requis_manquant"))
verifier("construire_table : provenance mal formée -> refus provenance_invalide",
         leve_refus(construire_table(BASE, ROLES, plan_type(), provenance = list(fichier = "a.csv")),
                    "provenance_invalide"))
verifier("construire_table : ligne invalide sans exclusion déclarée -> refus ligne_invalide (étape donnees)", {
  d <- BASE; d$naissance[2] <- NA
  leve_refus(construire_table(d, ROLES, plan_type()), "ligne_invalide")
})

verifier("construire_table : base sans ligne -> refus base_vide (empreinte calculée sans erreur avant l'étape donnees)",
         leve_refus(construire_table(BASE[0, ], ROLES, plan_type()), "base_vide"))
verifier("ETAPES et construire_table() concordent : une branche du switch par étape, appelant la fonction d'étape déclarée (valeur calculée à la main)", {
  corps <- body(construire_table)
  appels <- list()
  chercher <- function(e) {
    if (is.call(e)) {
      if (identical(e[[1L]], as.name("switch"))) appels[[length(appels) + 1L]] <<- e
      for (k in as.list(e)[-1L]) if (!missing(k)) chercher(k)
    }
  }
  chercher(corps)
  if (length(appels) != 1L) return("un seul switch attendu dans construire_table()")
  br <- as.list(appels[[1L]])[-(1:2)]
  nommees <- br[nzchar(names(br))]
  fonctions <- vapply(nommees, function(b) as.character(b[[1L]]), character(1L))
  et <- tablexp:::ETAPES
  if (!identical(names(nommees), et$etape)) sprintf("branches %s", paste(names(nommees), collapse = ", "))
  else if (!identical(unname(fonctions), et$fonction)) sprintf("fonctions %s", paste(fonctions, collapse = ", "))
  else if (!identical(names(tablexp:::PARAMETRES_ETAPES), et$etape)) "PARAMETRES_ETAPES"
  else TRUE
})

# --- objet résultat et fiche --------------------------------------------------

x <- construire_table(BASE, ROLES, plan_type())
verifier("objet : classe construction, champs table, etapes, avertissements, fiche, execution (ADR 0007, point 6.1)",
         inherits(x, "construction") &&
           identical(names(x), c("table", "etapes", "avertissements", "fiche", "execution")) &&
           is.null(x$table) && identical(names(x$etapes), c("donnees", "exposition")) &&
           all(vapply(x$etapes, function(e) identical(names(e), c("parametres", "resultat", "diagnostics")),
                      logical(1L))))
verifier("objet : avertissements data.frame (etape, code, message, nombre)",
         is.data.frame(x$avertissements) &&
           identical(names(x$avertissements), c("etape", "code", "message", "nombre")))
verifier("objet : exposition par (sexe, âge), colonnes de la spécification (A.3)",
         identical(names(x$etapes$exposition$resultat$exposition),
                   c("sexe", "age", "exposition_jours", "exposition_centrale", "complement_initial_cellule",
                     "complement_initial_fenetre", "evenements", "sorties_concurrentes")))
CHAMPS_FICHE <- c("version_fiche", "versions", "plateforme", "roles", "plan", "transition", "transition_code",
                  "periode", "delai_declaration", "fin_effective", "segmentation", "unite_observation",
                  "avertissement_unite", "recette", "convention", "unite_exposition", "par_annee", "exclusions",
                  "hypothese_censure", "graine", "graine_mention", "generateur", "provenance",
                  "provenance_mention", "empreintes", "avertissements")
f <- x$fiche
verifier("fiche : champs de l'ADR 0007 (point 6.2) et de la spécification (A.3)", identical(names(f), CHAMPS_FICHE))
verifier("fiche : plan reçu inchangé, rôles, recette, convention retenue non forcée, unité tête",
         identical(f$plan, plan_type()) && identical(f$roles, ROLES) &&
           identical(f$recette, "lignes_dates_completes") &&
           identical(f$convention, list(demandee = "plus_fine", native = "dernier_anniversaire",
                                        retenue = "dernier_anniversaire", forcee = FALSE)) &&
           identical(f$unite_observation, "tete") && is.na(f$avertissement_unite) &&
           identical(f$transition_code, "vivant_decede") && identical(f$fin_effective, D("2024-12-31")))
verifier("fiche : graine absente (NA, « aucune étape aléatoire »), générateur consigné, version 1",
         identical(f$graine, NA_integer_) && identical(f$graine_mention, "aucune étape aléatoire") &&
           identical(f$generateur, tablexp:::GENERATEUR) && identical(f$version_fiche, 1L))
verifier("fiche : provenance NULL, mention « aucun fichier », empreinte du fichier NA",
         "provenance" %in% names(f) && is.null(f$provenance) && startsWith(f$provenance_mention, "aucun fichier") &&
           is.na(f$empreintes$fichier))
verifier("fiche : empreintes (algorithme md5 nommé, données en 32 chiffres hexadécimaux ; résultat et code non calculés)",
         identical(f$empreintes$algorithme, "md5") && grepl("^[0-9a-f]{32}$", f$empreintes$donnees) &&
           is.na(f$empreintes$resultat) && is.na(f$empreintes$code) &&
           identical(f$empreintes$statut_resultat_code, "non calculée (branche fiche-rapport)"))
verifier("fiche : versions (tablexp, R) et plateforme (sysname, release, machine, blas, lapack)",
         identical(names(f$versions), c("tablexp", "R")) && identical(f$versions$R, R.version.string) &&
           identical(names(f$plateforme), c("sysname", "release", "machine", "blas", "lapack")))
verifier("fiche : aucun horodatage (aucun POSIXct ni POSIXlt dans la fiche)", {
  classes <- unlist(rapply(f, function(v) class(v)[1L], how = "unlist"))
  !any(classes %in% c("POSIXct", "POSIXlt"))
})
verifier("execution : horodatage, durée, session abrégée",
         inherits(x$execution$debut, "POSIXct") && is.numeric(x$execution$duree_secondes) &&
           identical(x$execution$session$R, R.version.string))
xp <- construire_table(BASE, ROLES, plan_type(),
                       provenance = list(fichier = "base.csv", empreinte = "0123456789abcdef0123456789abcdef",
                                         algorithme = "md5"))
verifier("fiche : provenance reçue consignée (fichier, empreinte, algorithme)",
         identical(xp$fiche$provenance$fichier, "base.csv") &&
           identical(xp$fiche$empreintes$fichier, "0123456789abcdef0123456789abcdef") &&
           identical(xp$fiche$empreintes$algorithme_fichier, "md5"))
verifier("unité contrat sans id_tete : avertissement inscrit dans l'objet et dans la fiche", {
  y <- construire_table(BASE, ROLES[names(ROLES) != "id_tete"], plan_type())
  identical(y$fiche$unite_observation, "contrat") && !is.na(y$fiche$avertissement_unite) &&
    "unite_contrat" %in% y$fiche$avertissements$code
})
verifier("fiche : fin effective lue dans les paramètres effectifs de l'étape donnees",
         identical(x$etapes$donnees$parametres$fin_effective, D("2024-12-31")) &&
           identical(x$fiche$fin_effective, x$etapes$donnees$parametres$fin_effective))
verifier("plan réduit à l'étape donnees : une seule étape exécutée, fiche sans convention", {
  y <- construire_table(BASE, ROLES, modifier(plan_type(), etapes = plan_type()$etapes["donnees"]))
  identical(names(y$etapes), "donnees") && is.null(y$fiche$convention) && is.na(y$fiche$unite_exposition)
})
verifier("construire_table ne touche pas .Random.seed de l'appelant (absent : reste absent ; présent : inchangé)", {
  env <- globalenv()
  avait <- exists(".Random.seed", envir = env, inherits = FALSE)
  sauve <- if (avait) env[[".Random.seed"]] else NULL
  if (avait) rm(".Random.seed", envir = env)
  invisible(construire_table(BASE, ROLES, plan_type()))
  absent <- !exists(".Random.seed", envir = env, inherits = FALSE)
  set.seed(5L); avant <- env[[".Random.seed"]]
  invisible(construire_table(BASE, ROLES, plan_type()))
  inchange <- identical(env[[".Random.seed"]], avant)
  if (avait) assign(".Random.seed", sauve, envir = env) else rm(".Random.seed", envir = env)
  absent && inchange
})

# --- empreinte (valeur calculée à la main, autre outil) ----------------------

# Texte canonique attendu (ADR 0007, point 9.3), écrit par printf et haché par
# md5sum (GNU coreutils), hors de R :
#   tablexp canonique 1<TAB>3<TAB>8
#   #cause_sortie<TAB>character / "deces" / "en_cours" / NA
#   #date_entree<TAB>Date / 2020-03-01 / 2016-05-20 / 2024-02-29
#   #date_naissance<TAB>Date / 1960-07-01 / 1945-10-15 / 1964-02-29
#   #date_sortie<TAB>Date / NA / 2019-02-10 / 2024-12-31
#   #id_contrat<TAB>double / 1 / 2 / 30000000000
#   #id_tete<TAB>integer / 1 / NA / 3
#   #segment<TAB>character / "a\"b" / "c\\d\te" / "été" (UTF-8)
#   #sexe<TAB>factor / "H" / "F" / "H"
# (346 octets, LF, md5 80e1ed7c22b2c1b6086260ce4490d64c).
EMP <- data.frame(c = c("deces", "en_cours", NA), e = D(c("2020-03-01", "2016-05-20", "2024-02-29")),
                  n = D(c("1960-07-01", "1945-10-15", "1964-02-29")), s = D(c(NA, "2019-02-10", "2024-12-31")),
                  ic = c(1, 2, 3e10), it = c(1L, NA, 3L), sg = c("a\"b", "c\\d\te", "été"),
                  sx = factor(c("H", "F", "H")), autre = 1:3, stringsAsFactors = FALSE)
Encoding(EMP$sg) <- "UTF-8"
ROLES_EMP <- c(sexe = "sx", segment = "sg", id_tete = "it", id_contrat = "ic", date_sortie = "s",
               date_naissance = "n", date_entree = "e", cause_sortie = "c")
empreinte_donnees <- tablexp:::empreinte_donnees
verifier("empreinte (valeur calculée à la main par printf et md5sum, hors R) : 80e1ed7c22b2c1b6086260ce4490d64c",
         identical(empreinte_donnees(EMP, ROLES_EMP), "80e1ed7c22b2c1b6086260ce4490d64c"))
verifier("empreinte : texte canonique de 346 octets, sans CR", {
  fch <- tempfile()
  tablexp:::texte_canonique(EMP, ROLES_EMP, fch)
  o <- readBin(fch, "raw", 1000L)
  unlink(fch)
  length(o) == 346L && !any(o == as.raw(0x0d))
})
verifier("empreinte : indépendante du nom des colonnes et des colonnes non déclarées, sensible à l'ordre des lignes et aux valeurs", {
  e2 <- EMP; names(e2)[names(e2) == "ic"] <- "contrat"; r2 <- ROLES_EMP; r2[["id_contrat"]] <- "contrat"
  e2$autre <- 7:9
  e3 <- EMP[3:1, ]; e4 <- EMP; e4$ic[1] <- 1 + 2^-40
  identical(empreinte_donnees(e2, r2), "80e1ed7c22b2c1b6086260ce4490d64c") &&
    !identical(empreinte_donnees(e3, ROLES_EMP), "80e1ed7c22b2c1b6086260ce4490d64c") &&
    !identical(empreinte_donnees(e4, ROLES_EMP), "80e1ed7c22b2c1b6086260ce4490d64c")
})
verifier("empreinte : zéro négatif normalisé (-0 et 0 donnent la même empreinte)", {
  a <- data.frame(x = c(0, 1)); b <- data.frame(x = c(-0, 1))
  identical(empreinte_donnees(a, c(id_contrat = "x")), empreinte_donnees(b, c(id_contrat = "x")))
})
verifier("empreinte : chaîne au codage bytes -> refus chaine_codage_bytes", {
  z <- "é"; Encoding(z) <- "bytes"
  leve_refus(empreinte_donnees(data.frame(x = c("a", z), stringsAsFactors = FALSE), c(segment = "x")),
             "chaine_codage_bytes")
})
verifier("empreinte : chaîne marquée UTF-8 invalide -> refus chaine_non_utf8", {
  z <- rawToChar(as.raw(c(0x61, 0xff))); Encoding(z) <- "UTF-8"
  leve_refus(empreinte_donnees(data.frame(x = c("a", z), stringsAsFactors = FALSE), c(segment = "x")),
             "chaine_non_utf8")
})
verifier("empreinte de la fiche = empreinte_donnees des données reçues (avant exclusions)",
         identical(x$fiche$empreintes$donnees, empreinte_donnees(BASE, ROLES)))

# --- portefeuille simulé (#10) : identités exactes et recoupement -----------

gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
LOI <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
            rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
PF <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75,
           origine = D("2005-01-01"))
SIM <- simuler_portefeuille(LOI, PF, PER, 20261016L)
ROLES_SIM <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
               date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
DELAI <- 90
plan_sim <- function(transition, par_annee = FALSE)
  list(transition = transition, periode = PER, delai_declaration = DELAI, segmentation = "sexe",
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = "plus_fine", par_annee = par_annee)))
t0 <- Sys.time()
XD <- construire_table(SIM, ROLES_SIM, plan_sim(VD))
duree <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("  construire_table, %d têtes simulées, vivant -> décédé : %.1f s\n", nrow(SIM), duree))
XR <- construire_table(SIM, ROLES_SIM, plan_sim(RA))
XA <- construire_table(SIM, ROLES_SIM, plan_sim(VD, par_annee = TRUE))
TD <- XD$etapes$exposition$resultat$exposition
TR <- XR$etapes$exposition$resultat$exposition
TA <- XA$etapes$exposition$resultat$exposition

borne <- as.numeric(PER$fin) - DELAI + 1
deb <- as.numeric(PER$debut)
ent <- as.numeric(SIM$date_entree); sor <- as.numeric(SIM$date_sortie); sor[is.na(sor)] <- Inf
a <- pmax(ent, deb); b <- pmin(sor, borne)
verifier("portefeuille simulé (identité exacte) : exposition totale en jours = somme des max(0, b - a), égalité entière",
         identical(sum(TD$exposition_jours), sum(pmax(0, b - a))) &&
           identical(sum(TR$exposition_jours), sum(pmax(0, b - a))))
verifier("portefeuille simulé (identité exacte) : décès comptés = décès simulés avec debut <= s <= fin - délai",
         identical(sum(TD$evenements),
                   sum(SIM$cause_sortie == "deces" & !is.na(sor) & sor >= deb & sor < borne)))
verifier("portefeuille simulé (identité exacte) : rachats comptés = rachats simulés avec debut <= s <= fin - délai, décès censurés",
         identical(sum(TR$evenements),
                   sum(SIM$cause_sortie == "rachat" & !is.na(sor) & sor >= deb & sor < borne)) &&
           identical(sum(TR$sorties_concurrentes),
                     sum(SIM$cause_sortie == "deces" & !is.na(sor) & sor >= deb & sor < borne)))
verifier("portefeuille simulé (identité exacte) : décès par sexe et par âge = décès simulés par âge au dernier anniversaire (age_revolu)", {
  k <- SIM$cause_sortie == "deces" & !is.na(sor) & sor >= deb & sor < borne
  att <- table(SIM$sexe[k], tablexp:::age_revolu(as.numeric(SIM$date_naissance[k]), sor[k]))
  obs <- TD[TD$evenements > 0, ]
  identical(as.integer(att[cbind(obs$sexe, as.character(obs$age))]), obs$evenements) &&
    sum(att) == sum(obs$evenements)
})
verifier("portefeuille simulé (recoupement : anniversaires par as.Date, âges extrêmes comparés à age_revolu() du module donnees, ligne par ligne) : somme des années d'âge à 1e-12 relatif", {
  i <- which(a < b)
  nai <- as.numeric(SIM$date_naissance)[i]
  xa <- tablexp:::age_revolu(nai, a[i]); xb <- tablexp:::age_revolu(nai, b[i] - 1)
  k <- xb - xa + 1
  r <- rep(seq_along(i), k)
  x <- xa[r] + sequence(k) - 1
  nd <- structure(nai, class = "Date")
  an_n <- as.integer(format(nd, "%Y"))[r]; md <- format(nd, "%m-%d")[r]
  anniv <- function(y) {
    v <- as.Date(sprintf("%04d-%s", y, md), optional = TRUE)
    v[is.na(v)] <- as.Date(sprintf("%04d-03-01", y[is.na(v)]))
    as.numeric(v)
  }
  bas <- anniv(an_n + x); haut <- anniv(an_n + x + 1)
  j <- pmin(b[i][r], haut) - pmax(a[i][r], bas)
  if (!identical(sum(j), sum(TD$exposition_jours))) "jours ligne par ligne différents"
  else proche(sum(TD$exposition_centrale), sum(j / (haut - bas)), rel = 1e-12, abs = 0)
})
verifier("portefeuille simulé (identité exacte) : par_annee, somme sur les années = exposition par âge seul, en jours", {
  s <- aggregate(exposition_jours ~ sexe + age, data = TA, FUN = sum)
  m <- merge(s, TD[c("sexe", "age", "exposition_jours")], by = c("sexe", "age"), all = TRUE)
  !anyNA(m) && identical(m$exposition_jours.x, m$exposition_jours.y) &&
    identical(sum(TA$evenements), sum(TD$evenements))
})
verifier("portefeuille simulé : unité tête, aucune exclusion, aucune cellule d'âge hors de la plage contiguë",
         identical(XD$fiche$unite_observation, "tete") && XD$etapes$donnees$resultat$nombre_lignes_exclues == 0L &&
           all(tapply(TD$age, TD$sexe, function(v) identical(v, seq(min(v), max(v))))))
