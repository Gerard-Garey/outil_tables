# -----------------------------------------------------------------------------
#  tests/test_reproductibilite.R : batterie de reproductibilité (E4.6 ;
#  ADR 0007, points 8 et 11.1)
#
#  Socle (#8) : reproductibilité de l'aléa sous graine locale (sous_graine,
#  générateur GENERATEUR) — tirages identiques au bit près à graine égale,
#  dans un même processus, quel que soit l'état du générateur de l'appelant,
#  et entre deux processus R de la même machine ; simulateur (#10) : jeu
#  simulé identical() à graine égale, dans le processus et dans un autre ;
#  construction (#11) : deux constructions identical() hors `execution`,
#  rejeu de la fiche, même objet dans un autre processus R, empreinte des
#  données identique sous LANG=C et LANG=C.UTF-8 et égale à la valeur
#  calculée hors R ; taux bruts (#14) : construction avec l'étape `brut`
#  identical() hors `execution`, rejeu de la fiche, même objet dans un autre
#  processus R, invariance du plan sans `brut` (R1 et R4) ; Kaplan-Meier
#  (#15) : plan R6 (R1 avec kaplan_meier) identical() hors `execution`,
#  rejeu de la fiche, même objet dans un autre processus R, invariance du
#  plan sans kaplan_meier ; non-régression (#17, ADR 0009, points 3.6 à
#  3.8) : plans R1 à R6 sur l'entrée de référence (jeu_reference()),
#  aplatis, aller-retour exact du format, comparés à leur fichier de
#  tests/reference/ (échec attendu #17 tant que le fichier n'existe pas,
#  liste SANS_REFERENCE_17), aucune référence orpheline ; témoins sur des
#  copies temporaires (jamais sur tests/reference/) ; règles des modes du
#  workflow et lignes de commande produire_references.R (refus d'appel) et
#  comparer_references.R ; permutation des lignes de l'entrée de référence
#  (#39) : plans R6, R4, R5, R3 et R2, deux permutations, identical() hors
#  sommes par cellule, celles-ci à la tolérance TOLERANCE_PERMUTATION, bilan
#  des écarts et témoins ; chaînes non ASCII et locale (T1 à T4, M54) ;
#  transitions de changement de résultat annoncé (ADR 0009, point 9.5, #47) :
#  liste unique TRANSITIONS_EN_COURS (vide hors transition), utilitaire
#  générique et témoins de chaque nature, de l'union et du succès inattendu.
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/test_reproductibilite.R
#  Code de sortie 1 si au moins un échec ou un succès inattendu.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(normalizePath(f[1L])), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
BIBLIOTHEQUE <- installer_tablexp(RACINE)
sous_graine <- tablexp:::sous_graine

# Tirages de référence de l'essai : trois lois, pour couvrir les trois
# réglages de GENERATEUR (kind, normal.kind, sample.kind).
tirer <- function() list(u = runif(1000), z = rnorm(1000), s = sample.int(1e6, 1000))

fichier_courant("aléa sous graine locale (sous_graine)")

a <- sous_graine(20261016L, tirer())
b <- sous_graine(20261016L, tirer())
verifier("même graine, même processus : tirages identical()", identical(a, b))
verifier("graines différentes : tirages différents",
         !identical(a, sous_graine(20261017L, tirer())))

# L'état de l'appelant (générateur et position) n'influence pas les tirages.
set.seed(1L, kind = "L'Ecuyer-CMRG"); invisible(runif(17))
c_ <- sous_graine(20261016L, tirer())
RNGkind("default", "default", "default")
verifier("appelant sous L'Ecuyer-CMRG, après 17 tirages : tirages identical()",
         identical(a, c_))

# Même graine dans un autre processus R : mêmes tirages au bit près.
# Écriture en %.17g (aller-retour exact des doubles, ADR 0007, mesures).
script <- tempfile(fileext = ".R")
sortie <- tempfile(fileext = ".txt")
writeLines(c(
  sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)),
  "x <- tablexp:::sous_graine(20261016L, list(u = runif(1000), z = rnorm(1000), s = sample.int(1e6, 1000)))",
  sprintf("writeLines(c(sprintf('%%.17g', c(x$u, x$z)), sprintf('%%d', x$s)), %s)", deparse(sortie))
), script)
r <- executer_rscript(script)
verifier("sous-processus R : exécution sans erreur",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
verifier("autre processus, même graine : tirages identical()", {
  if (!file.exists(sortie)) "fichier de sortie du sous-processus absent" else {
    lu <- readLines(sortie)
    identical(a, list(u = as.numeric(lu[1:1000]), z = as.numeric(lu[1001:2000]),
                      s = as.integer(lu[2001:3000])))
  }
})
unlink(c(script, sortie))

# Simulateur (#10) : même loi, même portefeuille, même graine dans un autre
# processus R : jeu simulé identical() au bit près, attribut `simulation`
# compris (saveRDS / readRDS : aller-retour exact).
fichier_courant("simulateur sous graine locale (simuler_portefeuille)")

sim_code <- c(
  "gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)",
  "ra <- tablexp:::table_constante(0.05, 0:120, 'rachat constant 5 %')",
  "loi <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),",
  "            rachat = list(table = ra, facteurs = c(H = 1, F = 1)))",
  "pf <- list(mode = 'uniforme', n = 1e4, proportion_hommes = 0.5, age_min = 20, age_max = 75,",
  "           origine = as.Date('2005-01-01'))",
  "per <- list(debut = as.Date('2015-01-01'), fin = as.Date('2024-12-31'))",
  "s <- tablexp::simuler_portefeuille(loi, pf, per, 20261016L)")
eval(parse(text = sim_code))
verifier("simulateur, même processus, même graine : identical()",
         identical(s, eval(parse(text = sim_code[length(sim_code)]))))
script <- tempfile(fileext = ".R")
sortie <- tempfile(fileext = ".rds")
writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)),
             "set.seed(99L); invisible(runif(5))  # état de l'appelant différent",
             sim_code, sprintf("saveRDS(s, %s)", deparse(sortie))), script)
r <- executer_rscript(script)
verifier("simulateur, sous-processus R : exécution sans erreur",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
verifier("simulateur, autre processus, même graine : jeu simulé identical() (attribut compris)",
         if (!file.exists(sortie)) "fichier de sortie du sous-processus absent" else
           identical(s, readRDS(sortie)))
unlink(c(script, sortie))

# Construction (#11) : identical() hors `execution` (ADR 0007, point 6.1),
# rejeu de la fiche (point 6.2), autre processus, empreinte indépendante de
# la locale (point 9.3) ; l'empreinte ne passe pas par serialize() (point
# 9.4), dont l'en-tête porte la version de R.
fichier_courant("construction (construire_table) : identical(), rejeu, autre processus, empreinte")

constr_code <- c(
  "roles <- c(id_contrat = 'id_contrat', id_tete = 'id_tete', sexe = 'sexe',",
  "           date_naissance = 'date_naissance', date_entree = 'date_entree',",
  "           date_sortie = 'date_sortie', cause_sortie = 'cause_sortie')",
  "plan <- list(transition = list(depart = 'vivant', arrivee = 'deces'), periode = per,",
  "             delai_declaration = 90, segmentation = 'sexe',",
  "             etapes = list(donnees = list(exclusions = character(0)),",
  "                           exposition = list(convention = 'plus_fine', par_annee = TRUE)))",
  "x <- tablexp::construire_table(s, roles, plan)",
  "x$execution <- NULL")
eval(parse(text = constr_code))
y <- sans_execution(construire_table(s, roles, plan))
verifier("construction, même processus : deux exécutions identical() hors execution", identical(x, y))
verifier("construction : rejeu construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance) identical() hors execution",
         identical(x, sans_execution(construire_table(s, x$fiche$roles, x$fiche$plan, x$fiche$provenance))))
verifier("construction : seul le champ execution diffère entre deux exécutions (horodatage)", {
  a1 <- construire_table(s, roles, plan); Sys.sleep(0.01); a2 <- construire_table(s, roles, plan)
  !identical(a1$execution$debut, a2$execution$debut) && identical(sans_execution(a1), sans_execution(a2))
})
script <- tempfile(fileext = ".R")
sortie <- tempfile(fileext = ".rds")
writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)),
             sim_code, constr_code, sprintf("saveRDS(x, %s)", deparse(sortie))), script)
r <- executer_rscript(script)
verifier("construction, sous-processus R : exécution sans erreur",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
verifier("construction, autre processus : objet identical() hors execution (fiche et empreinte comprises)",
         if (!file.exists(sortie)) "fichier de sortie du sous-processus absent" else identical(x, readRDS(sortie)))
unlink(c(script, sortie))

# Empreinte d'un petit data.frame (chaînes marquées UTF-8, non ASCII
# compris) : valeur calculée hors R (printf, md5sum de GNU coreutils ; voir
# tests/unitaires/test_construire_table.R ; doubles en octets IEEE 754 depuis
# #35, ADR 0011), recalculée dans deux processus de locales différentes.
EMPREINTE_ATTENDUE <- "2264ac10aa5504caec70eb1f97b2fb76"
emp_code <- c(
  "D <- as.Date",
  "d <- data.frame(c = c('deces', 'en_cours', NA), e = D(c('2020-03-01', '2016-05-20', '2024-02-29')),",
  "  n = D(c('1960-07-01', '1945-10-15', '1964-02-29')), s = D(c(NA, '2019-02-10', '2024-12-31')),",
  "  ic = c(1, 2, 3e10), it = c(1L, NA, 3L), sg = c('a\"b', 'c\\\\d\\te', '\\u00e9t\\u00e9'),",
  "  sx = factor(c('H', 'F', 'H')), stringsAsFactors = FALSE)",
  "Encoding(d$sg) <- 'UTF-8'",
  "roles <- c(sexe = 'sx', segment = 'sg', id_tete = 'it', id_contrat = 'ic', date_sortie = 's',",
  "           date_naissance = 'n', date_entree = 'e', cause_sortie = 'c')")
# Locale UTF-8 réellement disponible sur la plateforme (sondée par le harnais,
# issue #34) : C.UTF-8 n'existe pas partout (Windows). Aucune : échec explicite.
LOCALE_UTF8 <- locale_utf8_disponible()
verifier("empreinte, sous-processus : une locale UTF-8 est disponible",
         if (nzchar(LOCALE_UTF8)) TRUE
         else sprintf("aucune locale UTF-8 disponible parmi %s : cas UTF-8 non exécuté", paste(LOCALES_UTF8, collapse = ", ")))
for (lang in c("C", if (nzchar(LOCALE_UTF8)) LOCALE_UTF8)) {
  script <- tempfile(fileext = ".R")
  sortie <- tempfile(fileext = ".txt")
  writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)), emp_code,
               sprintf("writeLines(c(Sys.getlocale('LC_CTYPE'), l10n_info()[['UTF-8']], tablexp:::empreinte_donnees(d, roles)), %s)",
                       deparse(sortie))), script)
  r <- executer_rscript(script, env = c(paste0("LANG=", lang), paste0("LC_ALL=", lang)))
  verifier(sprintf("empreinte, sous-processus LANG=%s : égale à la valeur calculée hors R (%s)", lang,
                   EMPREINTE_ATTENDUE),
           if (r$statut != 0L) paste(r$sortie, collapse = " | ")
           else if (!file.exists(sortie)) "fichier de sortie du sous-processus absent"
           else {
             lu <- readLines(sortie)  # locale, session UTF-8 (TRUE / FALSE), empreinte
             # « C » : nom exact ; locale UTF-8 : session effectivement en UTF-8
             # (le nom rendu par Sys.getlocale() varie selon la plateforme).
             effective <- if (identical(lang, "C")) identical(lu[1L], "C") else identical(lu[2L], "TRUE")
             if (!effective) sprintf("locale effective %s (UTF-8 : %s) au lieu de %s", lu[1L], lu[2L], lang)
             else if (identical(lu[3L], EMPREINTE_ATTENDUE)) TRUE
             else sprintf("locale %s : empreinte %s", lu[1L], lu[3L])
           })
  unlink(c(script, sortie))
}

# Taux bruts (#14) : plan R1 de la spécification d'actuary (B, plans de
# référence) sur le portefeuille simulé ; identical() hors `execution`,
# rejeu de la fiche, autre processus ; invariance : le plan sans `brut` donne
# l'objet du plan avec `brut` privé des seuls ajouts de l'étape.
fichier_courant("construction avec l'étape brut (construire_table) : identical(), rejeu, autre processus")

brut_code <- c(
  "plan_b <- plan",
  "plan_b$delai_declaration <- 0",
  "plan_b$etapes$exposition$par_annee <- FALSE",
  "plan_b$etapes$brut <- list(estimateurs = c('poisson', 'binomial'), niveau = 0.95)",
  "xb <- tablexp::construire_table(s, roles, plan_b)",
  "xb$execution <- NULL")
eval(parse(text = brut_code))
verifier("construction avec brut, même processus : deux exécutions identical() hors execution",
         identical(xb, sans_execution(construire_table(s, roles, plan_b))))
verifier("construction avec brut : rejeu construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance) identical() hors execution",
         identical(xb, sans_execution(construire_table(s, xb$fiche$roles, xb$fiche$plan, xb$fiche$provenance))))
verifier("construction avec brut : fiche$plan porte l'étape brut, fiche$hypotheses_brut en fin de liste",
         identical(xb$fiche$plan, plan_b) && identical(tail(names(xb$fiche), 1L), "hypotheses_brut"))
# Invariance du plan sans `brut` (la comparaison à l'état de f9ab269 relève
# des références de non-régression, #17).
invariance_sans_brut <- function(xb, plan_b) {
  p0 <- plan_b; p0$etapes$brut <- NULL
  y0 <- sans_execution(construire_table(s, roles, p0))
  z <- xb
  z$etapes$brut <- NULL; z$fiche$hypotheses_brut <- NULL; z$fiche$plan <- p0
  garde <- z$avertissements$etape != "brut"
  z$avertissements <- z$avertissements[garde, , drop = FALSE]; rownames(z$avertissements) <- NULL
  z$fiche$avertissements <- z$avertissements
  identical(y0, z)
}
verifier("invariance du plan sans brut (R1) : objet identical() à celui du plan avec brut, hors etapes$brut, fiche$plan, fiche$hypotheses_brut et avertissements de l'étape brut",
         invariance_sans_brut(xb, plan_b))
verifier("invariance du plan sans brut (R4 : sexe, par_annee = TRUE, poisson seul, 0,95) : même contrôle", {
  plan_4 <- plan_b
  plan_4$etapes$exposition$par_annee <- TRUE
  plan_4$etapes$brut$estimateurs <- "poisson"
  invariance_sans_brut(sans_execution(construire_table(s, roles, plan_4)), plan_4)
})
script <- tempfile(fileext = ".R")
sortie <- tempfile(fileext = ".rds")
writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)),
             "set.seed(7L); invisible(runif(3))  # état de l'appelant différent",
             sim_code, constr_code, brut_code, sprintf("saveRDS(xb, %s)", deparse(sortie))), script)
r <- executer_rscript(script)
verifier("construction avec brut, sous-processus R : exécution sans erreur",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
verifier("construction avec brut, autre processus : objet identical() hors execution (taux, bornes, fiche)",
         if (!file.exists(sortie)) "fichier de sortie du sous-processus absent" else identical(xb, readRDS(sortie)))
unlink(c(script, sortie))

# Kaplan-Meier (#15) : plan R6 de l'ADR 0009 (annotation du 17/10/2026, (b)),
# R1 avec les trois estimateurs ; invariance : le plan R1 donne l'objet de R6
# privé des seuls ajouts de Kaplan-Meier (tables, hypothèse, statuts, états,
# diagnostics, paramètres, avertissement, fiche).
fichier_courant("construction avec Kaplan-Meier (plan R6) : identical(), rejeu, autre processus, invariance")

km_code <- c(
  "plan_6 <- plan_b",
  "plan_6$etapes$brut$estimateurs <- c('poisson', 'binomial', 'kaplan_meier')",
  "x6 <- tablexp::construire_table(s, roles, plan_6)",
  "x6$execution <- NULL")
eval(parse(text = km_code))
verifier("R6, même processus : deux exécutions identical() hors execution",
         identical(x6, sans_execution(construire_table(s, roles, plan_6))))
verifier("R6 : rejeu construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance) identical() hors execution",
         identical(x6, sans_execution(construire_table(s, x6$fiche$roles, x6$fiche$plan, x6$fiche$provenance))))
verifier("R6 : tables kaplan_meier et kaplan_meier_sauts présentes, somme de d par cellule = événements de l'exposition ; avertissement kaplan_meier_ensemble_a_risque_vide réservé aux cellules de cet état qui portent un événement (absent s'il n'y en a aucune)", {
  km <- x6$etapes$brut$resultat$kaplan_meier
  a <- x6$avertissements[x6$avertissements$code == "kaplan_meier_ensemble_a_risque_vide", , drop = FALSE]
  n_perdus <- sum(km$etat == "ensemble_a_risque_vide" & km$evenements > 0L)
  nrow(x6$etapes$brut$resultat$kaplan_meier_sauts) > 0L &&
    identical(km$evenements, x6$etapes$exposition$resultat$exposition$evenements) &&
    (if (n_perdus) identical(a$nombre, n_perdus) else !nrow(a))
})
verifier("invariance du plan sans kaplan_meier (R1 contre R6) : objet identical() à celui de R6 privé des ajouts de Kaplan-Meier", {
  z <- x6
  b <- z$etapes$brut
  b$resultat <- b$resultat[c("poisson", "binomial")]
  b$parametres$estimateurs <- c("poisson", "binomial")
  b$parametres$hypotheses$kaplan_meier <- NULL
  b$parametres$ensemble_a_risque <- NULL; b$parametres$denominateur_instants <- NULL
  b$parametres$statuts <- b$parametres$statuts[b$parametres$statuts$estimateur != "kaplan_meier", , drop = FALSE]
  b$diagnostics$kaplan_meier <- NULL
  b$diagnostics$etats <- b$diagnostics$etats[b$diagnostics$etats$estimateur != "kaplan_meier", , drop = FALSE]
  z$etapes$brut <- b
  garde <- z$avertissements$code != "kaplan_meier_ensemble_a_risque_vide"
  z$avertissements <- z$avertissements[garde, , drop = FALSE]; rownames(z$avertissements) <- NULL
  z$fiche$avertissements <- z$avertissements
  z$fiche$plan <- plan_b
  z$fiche$hypotheses_brut <- b$parametres[names(xb$fiche$hypotheses_brut)]
  identical(xb, z)
})
script <- tempfile(fileext = ".R")
sortie <- tempfile(fileext = ".rds")
writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)),
             "set.seed(11L); invisible(runif(2))  # état de l'appelant différent",
             sim_code, constr_code, brut_code, km_code, sprintf("saveRDS(x6, %s)", deparse(sortie))), script)
r <- executer_rscript(script)
verifier("R6, sous-processus R : exécution sans erreur",
         if (r$statut == 0L) TRUE else paste(r$sortie, collapse = " | "))
verifier("R6, autre processus : objet identical() hors execution (Kaplan-Meier compris)",
         if (!file.exists(sortie)) "fichier de sortie du sous-processus absent" else identical(x6, readRDS(sortie)))
unlink(c(script, sortie))

# Non-régression (#17 ; ADR 0009, points 3.6 à 3.8) : chaque plan de
# référence sur l'entrée de référence, aplati et comparé à son fichier ;
# témoins sur des copies temporaires.
fichier_courant("références de non-régression (ADR 0009) : plans R1 à R6, aller-retour, comparaison aux fichiers")

# Chargement par parse(encoding = "UTF-8") : littéraux marqués UTF-8 en
# toute locale (charger_utf8() d'outils_reference.R, recopié ici).
local(for (e in parse(file.path(RACINE, "tests", "reference", "outils_reference.R"), encoding = "UTF-8",
                      keep.source = FALSE)) eval(e, globalenv()))
DOSSIER_REFERENCES <- file.path(RACINE, "tests", "reference")
# Plans sans fichier de référence (point 3.7) : échec attendu #17. Le commit
# de référence de la session principale retire de cette liste les plans
# qu'il crée (succès inattendu sinon).
SANS_REFERENCE_17 <- character(0)
# Transitions en cours (ADR 0009, point 9.5 et troisième annotation du
# 19/10/2026 ; #47) : liste unique des changements de résultat annoncés dont
# les références ne sont pas encore régénérées. Vide hors transition ; le
# commit de référence de la session principale la vide. Une transition (une
# par issue) :
#   list(issue = "#N", plans = c("R1", ...), changements = list(<changement>, ...))
# Un changement nomme un chemin et la nature exacte du changement admis ; il
# doit être OBSERVÉ sur la référence commitée telle qu'elle est lue, avant
# tout alignement (sinon : « changement déclaré non observé ») :
#   list(chemin = , nature = "type", de = "double", vers = "integer")
#       conversion de type sans perte (double <-> integer) ; la référence est
#       du type `de` ;
#   list(chemin = , nature = "valeur", valeurs = , positions = )
#       valeurs numériques nouvelles. Sans `positions`, `valeurs` remplace
#       tout le vecteur, de n'importe quelle longueur (changement de
#       longueur compris : il n'y a pas de nature « longueur ») et diffère de
#       la référence ; avec `positions` (rangs à partir de 1), la longueur
#       reste celle de la référence et chaque position déclarée diffère ;
#   list(chemin = , nature = "valeur de chaîne", valeurs = , positions = )
#       idem pour des chaînes (version, empreinte) ;
#   list(chemin = , nature = "ajout", type = , longueur = )
#       chemin absent de la référence, présent dans l'objet seul, de ce type
#       et de cette longueur (les valeurs relèvent du tableau avant / après) ;
#   list(chemin = , nature = "retrait", type = , longueur = )
#       chemin présent dans la référence seule, de ce type et de cette longueur.
# Plusieurs transitions peuvent porter sur un même plan : leurs changements
# s'unissent. Deux déclarations identiques s'appliquent une fois ; sont des
# erreurs : deux valeurs différentes à une même position, un vecteur entier et
# une autre déclaration de valeurs sur un même chemin, deux conversions de
# type, un ajout ou un retrait combiné à une autre nature sur un même chemin.
# Un changement de fiche/empreintes/donnees n'est admis qu'en « valeur de
# chaîne » sans `positions`, avec un changement de
# fiche/empreintes/representation et la garde 3.2 (entree_inchangee()), qui
# prouve que l'entrée est inchangée sous la représentation de la référence.
# Pour chaque plan listé : un test ordinaire « seul écart » aligne la
# référence sur les changements déclarés (aligner_reference()) puis exige une
# comparaison sans aucun autre écart ; la comparaison stricte est un échec
# attendu renvoyant à l'issue de la première transition de la liste qui porte
# le plan : après la régénération, elle réussit et fait échouer la batterie
# (succès inattendu), le test « seul écart » échouant aussi (changement
# déclaré non observé). Un plan non listé passe par le test ordinaire de la
# comparaison au fichier.
# #35 (ADR 0011, points 2.5 et 10) : « tablexp canonique 2 », doubles de
# l'empreinte des données écrits en octets IEEE 754 ; l'entrée de référence n'a
# aucune colonne double, donc son texte ne change que par sa première ligne
# (garde 3.2 : MD5 du texte de l'entrée sous « tablexp canonique 1 » =
# f3519cc1b73ab0d1fcc6ad89dc6d595c, vérifié aussi hors R par printf, tail et
# md5sum). Valeur de l'empreinte : mesurée, identique pour R1 à R6 (même entrée).
TRANSITIONS_EN_COURS <- list(
  list(issue = "#35", plans = c("R1", "R2", "R3", "R4", "R5", "R6"),
       changements = list(
         list(chemin = "fiche/empreintes/representation", nature = "valeur de chaîne",
              valeurs = "tablexp canonique 2"),
         list(chemin = "fiche/empreintes/donnees", nature = "valeur de chaîne",
              valeurs = "35ca55371e72f0f0103993d70e3a64d3"))))

NATURES_TRANSITION <- c("type", "valeur", "valeur de chaîne", "ajout", "retrait")
CHAMPS_CHANGEMENT <- list(type = c("de", "vers"), valeur = c("valeurs", "positions"),
                          "valeur de chaîne" = c("valeurs", "positions"),
                          ajout = c("type", "longueur"), retrait = c("type", "longueur"))
CHAMPS_EXIGES <- list(type = c("de", "vers"), valeur = "valeurs", "valeur de chaîne" = "valeurs",
                      ajout = c("type", "longueur"), retrait = c("type", "longueur"))

# Problèmes de forme d'une liste de transitions (character(0) si elle est
# bien formée).
valider_transitions <- function(transitions, plans = names(plans_reference())) {
  if (!is.list(transitions)) return("la liste des transitions n'est pas une liste")
  p <- character(0)
  issues <- vapply(transitions, function(t) if (is.character(t$issue) && length(t$issue) == 1L) t$issue else NA_character_, "")
  for (k in seq_along(transitions)) {
    t <- transitions[[k]]; e <- sprintf("transition %d : ", k)
    if (!is.list(t) || !identical(sort(names(t)), c("changements", "issue", "plans"))) {
      p <- c(p, paste0(e, "champs issue, plans et changements exigés, et eux seuls")); next
    }
    if (is.na(issues[k]) || !grepl(MOTIF_ISSUE, issues[k])) p <- c(p, paste0(e, "issue de la forme \"#N\" exigée"))
    else if (sum(issues == issues[k], na.rm = TRUE) > 1L) p <- c(p, paste0(e, "issue ", issues[k], " en double"))
    if (!is.character(t$plans) || !length(t$plans) || anyNA(t$plans) || !all(t$plans %in% plans))
      p <- c(p, paste0(e, "plans : identifiants de plans de référence exigés"))
    if (!is.list(t$changements) || !length(t$changements)) {
      p <- c(p, paste0(e, "au moins un changement exigé")); next
    }
    for (j in seq_along(t$changements)) {
      ch <- t$changements[[j]]; e2 <- sprintf("%schangement %d : ", e, j)
      if (!is.list(ch) || !is.character(ch$chemin) || length(ch$chemin) != 1L || is.na(ch$chemin) || !nzchar(ch$chemin) ||
          !is.character(ch$nature) || length(ch$nature) != 1L || !ch$nature %in% NATURES_TRANSITION) {
        p <- c(p, paste0(e2, "chemin et nature (parmi ", paste(NATURES_TRANSITION, collapse = ", "), ") exigés")); next
      }
      inconnus <- setdiff(names(ch), c("chemin", "nature", CHAMPS_CHANGEMENT[[ch$nature]]))
      if (length(inconnus)) p <- c(p, paste0(e2, "champ(s) hors de la nature « ", ch$nature, " » : ", paste(inconnus, collapse = ", ")))
      absents <- setdiff(CHAMPS_EXIGES[[ch$nature]], names(ch))
      if (length(absents)) { p <- c(p, paste0(e2, "champ(s) exigé(s) par la nature « ", ch$nature, " » : ", paste(absents, collapse = ", "))); next }
      if (identical(ch$chemin, CHEMIN_EMPREINTE_DONNEES) && !(ch$nature == "valeur de chaîne" && is.null(ch$positions)))
        p <- c(p, paste0(e2, CHEMIN_EMPREINTE_DONNEES, " : seule la nature « valeur de chaîne » sans positions est admise (garde 3.2)"))
      if (ch$nature == "type" && !(is.character(ch$de) && is.character(ch$vers) && length(ch$de) == 1L && length(ch$vers) == 1L &&
                                   paste(ch$de, ch$vers) %in% c("double integer", "integer double")))
        p <- c(p, paste0(e2, "type : de et vers parmi double -> integer ou integer -> double"))
      if (ch$nature %in% c("ajout", "retrait")) {
        if (!(is.character(ch$type) && length(ch$type) == 1L && ch$type %in% TYPES_REFERENCE))
          p <- c(p, paste0(e2, "type : un type du format des références exigé"))
        if (!(is.numeric(ch$longueur) && length(ch$longueur) == 1L && !is.na(ch$longueur) && ch$longueur >= 0 && ch$longueur == floor(ch$longueur)))
          p <- c(p, paste0(e2, "longueur : entier >= 0 exigé"))
      }
      if (ch$nature %in% c("valeur", "valeur de chaîne")) {
        ok_v <- if (ch$nature == "valeur") is.numeric(ch$valeurs) && !is.object(ch$valeurs) else is.character(ch$valeurs)
        if (!ok_v || !length(ch$valeurs)) p <- c(p, paste0(e2, "valeurs ", if (ch$nature == "valeur") "numériques" else "de type character", " non vides exigées"))
        else if (!is.null(ch$positions) && !(is.numeric(ch$positions) && length(ch$positions) == length(ch$valeurs) &&
                                              !anyNA(ch$positions) && all(ch$positions >= 1 & ch$positions == floor(ch$positions)) &&
                                              !anyDuplicated(ch$positions)))
          p <- c(p, paste0(e2, "positions : entiers distincts >= 1, en nombre égal à celui des valeurs"))
      }
    }
  }
  p
}

# Changements (union) et issues des transitions qui portent sur le plan `id`.
changements_du_plan <- function(id, transitions) {
  t <- Filter(function(t) id %in% t$plans, transitions)
  list(issues = vapply(t, function(x) x$issue, ""), changements = unlist(lapply(t, `[[`, "changements"), recursive = FALSE))
}
plans_en_transition <- function(transitions) unique(unlist(lapply(transitions, `[[`, "plans")))

decrire_changement <- function(ch) switch(ch$nature,
  type = sprintf("type de %s (%s -> %s)", ch$chemin, ch$de, ch$vers),
  sprintf("%s de %s", ch$nature, ch$chemin))

NON_OBSERVE <- "changement déclaré non observé (référence déjà régénérée ? retirer la transition)"

# Aligne `ref` (référence lue) sur les changements déclarés et rend la
# référence alignée. Les déclarations identiques s'appliquent une fois ; les
# déclarations contradictoires sont une erreur (voir TRANSITIONS_EN_COURS). Un
# changement déclaré doit être observé sur `ref` avant alignement (type de la
# référence égal à `de` ; valeurs différentes de celles de la référence, à
# chaque position déclarée ou sur le vecteur entier ; ajout absent de la
# référence ; retrait présent dans la référence), sinon erreur NON_OBSERVE.
# Ordre d'application : type, valeurs, valeurs de chaîne, ajouts, retraits ;
# l'observation des valeurs se juge sur la référence après conversion de type.
# Les autres erreurs (stop) disent pourquoi le changement n'est pas celui qu'on
# observe : chemin absent, type ou longueur inattendus, conversion avec perte,
# positions hors du vecteur, ajout absent de l'objet, retrait encore présent.
aligner_reference <- function(ref, g, changements) {
  changements <- lapply(changements, function(ch) {
    ch <- ch[sort(names(ch), method = "radix")]
    if (!is.null(ch$positions)) ch$positions <- as.integer(ch$positions)
    ch
  })
  changements <- changements[!duplicated(changements)]
  for (c_ in unique(vapply(changements, `[[`, "", "chemin"))) {
    cs <- Filter(function(x) x$chemin == c_, changements)
    if (length(cs) < 2L) next
    natures <- vapply(cs, `[[`, "", "nature")
    valeurs_ <- natures %in% c("valeur", "valeur de chaîne")
    if (sum(natures == "type") > 1L) stop(sprintf("%s : deux conversions de type différentes", c_))
    if (any(natures %in% c("ajout", "retrait"))) stop(sprintf("%s : ajout ou retrait combiné à une autre déclaration sur le même chemin", c_))
    if (sum(valeurs_) > 1L && any(vapply(cs[valeurs_], function(x) is.null(x$positions), NA)))
      stop(sprintf("%s : vecteur entier et autre déclaration de valeurs sur le même chemin", c_))
  }
  rang <- match(vapply(changements, `[[`, "", "nature"), NATURES_TRANSITION)
  ordonnes <- changements[order(rang)]
  ref_t <- NULL  # référence après conversion de type : base d'observation des valeurs
  fixees <- list()  # par chemin : positions et valeurs déjà imposées (conflit)
  for (ch in ordonnes) {
    if (is.null(ref_t) && !ch$nature %in% "type") ref_t <- ref
    c_ <- ch$chemin
    if (ch$nature %in% c("type", "valeur", "valeur de chaîne", "retrait") && !c_ %in% names(ref$types))
      stop(sprintf("%s : absent de la référence%s", c_, if (ch$nature == "retrait") paste0(" ; ", NON_OBSERVE) else ""))
    if (ch$nature == "type") {
      t <- ref$types[[c_]]
      if (identical(t, ch$vers)) stop(sprintf("%s : type déjà %s ; %s", c_, t, NON_OBSERVE))
      if (!identical(t, ch$de)) stop(sprintf("%s : type %s de la référence, %s déclaré", c_, t, ch$de))
      v <- ref$valeurs[[c_]]
      w <- if (ch$vers == "integer") suppressWarnings(as.integer(v)) else as.double(v)
      if (!identical(if (ch$de == "double") as.double(w) else as.integer(w), v))
        stop(sprintf("%s : conversion %s -> %s avec perte", c_, ch$de, ch$vers))
      ref$types[[c_]] <- ch$vers; ref$valeurs[[c_]] <- w
    } else if (ch$nature %in% c("valeur", "valeur de chaîne")) {
      t <- ref$types[[c_]]; v0 <- ref_t$valeurs[[c_]]
      if (!t %in% (if (ch$nature == "valeur") c("double", "integer") else "character"))
        stop(sprintf("%s : type %s de la référence incompatible avec la nature « %s »", c_, t, ch$nature))
      nouveau <- switch(t, double = as.double(ch$valeurs), integer = as.integer(ch$valeurs), ch$valeurs)
      if (t == "integer" && !identical(as.double(nouveau), as.double(ch$valeurs))) stop(sprintf("%s : valeurs non entières pour un chemin integer", c_))
      if (is.null(ch$positions)) {
        if (length(nouveau) == length(v0) && all(identiques_elements(v0, nouveau))) stop(sprintf("%s : %s", c_, NON_OBSERVE))
        ref$valeurs[[c_]] <- nouveau
      } else {
        pos <- ch$positions
        if (length(pos) != length(nouveau) || any(pos > length(v0)))
          stop(sprintf("%s : %d valeur(s) déclarée(s) à des positions d'un vecteur de %d élément(s) de la référence", c_, length(nouveau), length(v0)))
        if (any(identiques_elements(v0[pos], nouveau))) stop(sprintf("%s : %s", c_, NON_OBSERVE))
        prec <- fixees[[c_]]
        if (!is.null(prec)) {
          commun <- intersect(pos, prec$pos)
          if (length(commun) && !identical(nouveau[match(commun, pos)], prec$val[match(commun, prec$pos)]))
            stop(sprintf("%s : deux transitions déclarent des valeurs différentes à la position %d", c_, commun[1L]))
        }
        fixees[[c_]] <- list(pos = c(prec$pos, pos), val = c(prec$val, nouveau))
        v <- ref$valeurs[[c_]]; v[pos] <- nouveau; ref$valeurs[[c_]] <- v
      }
    } else if (ch$nature == "ajout") {
      if (c_ %in% names(ref$types)) stop(sprintf("%s : %s", c_, NON_OBSERVE))
      if (!c_ %in% names(g$types)) stop(sprintf("%s : ajout déclaré d'un chemin absent de l'objet", c_))
      if (!identical(g$types[[c_]], ch$type)) stop(sprintf("%s : type %s de l'objet, %s déclaré", c_, g$types[[c_]], ch$type))
      if (length(g$valeurs[[c_]]) != ch$longueur)
        stop(sprintf("%s : longueur %d de l'objet, %d déclarée", c_, length(g$valeurs[[c_]]), as.integer(ch$longueur)))
      ref$types[[c_]] <- g$types[[c_]]; ref$valeurs[c_] <- list(g$valeurs[[c_]])
    } else {
      if (c_ %in% names(g$types)) stop(sprintf("%s : retrait déclaré d'un chemin encore présent dans l'objet", c_))
      if (!identical(ref$types[[c_]], ch$type)) stop(sprintf("%s : type %s de la référence, %s déclaré", c_, ref$types[[c_]], ch$type))
      if (length(ref$valeurs[[c_]]) != ch$longueur)
        stop(sprintf("%s : longueur %d de la référence, %d déclarée", c_, length(ref$valeurs[[c_]]), as.integer(ch$longueur)))
      garde <- names(ref$types) != c_
      ref$types <- ref$types[garde]; ref$valeurs <- ref$valeurs[garde]
    }
  }
  ref
}

# TRUE si `g` ne diffère de `ref` que par les changements déclarés, tous
# observés, sinon la raison (chaîne) : la référence alignée est comparée à `g`
# comme dans le test ordinaire, aux tolérances près.
seul_ecart <- function(ref, g, changements) {
  r <- tryCatch(aligner_reference(ref, g, changements), error = function(e) e)
  if (inherits(r, "error")) return(conditionMessage(r))
  if (comparer_references(r, g)$ok) TRUE
  else "écart autre que les changements déclarés (référence alignée, comparaison en échec)"
}

# Garde 3.2 (ADR 0009, troisième annotation du 19/10/2026, (f)) : prouve que
# l'entrée est inchangée quand l'empreinte des données change avec la
# représentation canonique. Le texte canonique de `donnees` (le package, une
# seule fois par couple donnees / roles) voit l'en-tête de sa première ligne
# (avant la première tabulation, égal à fiche/empreintes/representation de
# `g`) remplacé par celui de la référence ; son MD5 doit égaler
# fiche/empreintes/donnees de la référence. Réserve M2 de la même annotation :
# l'objet doit avoir été construit sur l'entrée passée, c'est-à-dire que le MD5
# du texte de `donnees` sous la représentation courante égale
# fiche/empreintes/donnees de `g` (le texte et ce MD5 sont dans le cache).
# Rend TRUE ou la raison.
CACHE_ENTREE <- new.env(parent = emptyenv())
entree_inchangee <- function(ref, g, donnees, roles) {
  chaine <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  rep_r <- ref$valeurs[[CHEMIN_REPRESENTATION]]; rep_g <- g$valeurs[[CHEMIN_REPRESENTATION]]
  emp_r <- ref$valeurs[[CHEMIN_EMPREINTE_DONNEES]]; emp_g <- g$valeurs[[CHEMIN_EMPREINTE_DONNEES]]
  if (!chaine(rep_r) || !chaine(rep_g) || !chaine(emp_r) || !chaine(emp_g))
    return(sprintf("%s et %s exigés, chaînes non vides, dans la référence et l'objet", CHEMIN_REPRESENTATION, CHEMIN_EMPREINTE_DONNEES))
  if (identical(rep_r, rep_g))
    return(sprintf("représentation canonique identique des deux côtés (%s) : la différence d'empreinte ne vient pas d'une nouvelle définition, l'entrée est différente", rep_g))
  if (is.null(CACHE_ENTREE$octets) || !identical(CACHE_ENTREE$donnees, donnees) || !identical(CACHE_ENTREE$roles, roles)) {
    f <- tempfile("texte_canonique_")
    tablexp:::texte_canonique(donnees, roles, f)
    CACHE_ENTREE$octets <- readBin(f, "raw", file.info(f)$size)
    CACHE_ENTREE$md5 <- unname(tools::md5sum(f))
    CACHE_ENTREE$donnees <- donnees; CACHE_ENTREE$roles <- roles
    unlink(f)
  }
  o <- CACHE_ENTREE$octets
  tab <- which(o == as.raw(9L))[1L]
  if (is.na(tab) || !identical(o[seq_len(tab - 1L)], charToRaw(rep_g)))
    return(sprintf("en-tête du texte canonique différent de %s de l'objet (%s)", CHEMIN_REPRESENTATION, rep_g))
  if (!identical(CACHE_ENTREE$md5, emp_g))
    return(sprintf("objet non construit sur l'entrée passée : MD5 %s du texte canonique de l'entrée, %s dans l'objet (%s)",
                   CACHE_ENTREE$md5, emp_g, CHEMIN_EMPREINTE_DONNEES))
  f2 <- tempfile("texte_canonique_ref_"); on.exit(unlink(f2))
  writeBin(c(charToRaw(rep_r), o[-seq_len(tab - 1L)]), f2)
  m <- unname(tools::md5sum(f2))
  if (identical(m, emp_r)) TRUE
  else sprintf("empreinte %s sous l'en-tête « %s » au lieu de %s : l'entrée a changé", m, rep_r, emp_r)
}

# Contrôles d'un plan : listé dans `transitions` -> test « seul écart », garde
# 3.2 si les changements du plan touchent l'empreinte des données, et échec
# attendu de la comparaison stricte ; sinon test ordinaire. `ref` et `strict`
# (TRUE ou la raison de l'échec de la comparaison au fichier) ne sont évalués
# que si le plan les exige. `donnees` et `roles` : l'entrée de référence, pour
# la garde.
controler_plan <- function(id, nom, ref, g, strict, transitions, donnees = NULL, roles = NULL) {
  t <- changements_du_plan(id, transitions)
  if (!length(t$changements)) return(verifier(nom, strict))
  chemins <- vapply(t$changements, `[[`, "", "chemin")
  verifier(sprintf("%s : seul écart à tests/reference/%s.tsv : %s (%s)", id, id,
                   paste(vapply(t$changements, decrire_changement, ""), collapse = " ; "),
                   paste(unique(t$issues), collapse = ", ")),
           seul_ecart(ref, g, t$changements))
  if (CHEMIN_EMPREINTE_DONNEES %in% chemins)
    verifier(sprintf("%s : garde 3.2 : entrée inchangée sous la représentation de la référence (%s)", id,
                     paste(unique(t$issues), collapse = ", ")),
             if (!CHEMIN_REPRESENTATION %in% chemins)
               sprintf("%s non déclaré dans les changements du plan : l'empreinte des données ne change pas sans nouvelle représentation", CHEMIN_REPRESENTATION)
             else entree_inchangee(ref, g, donnees, roles))
  echec_attendu(nom, t$issues[1L], strict)
}

# Doubles de la référence commitée qu'aucune clé ne classe, hors ceux que les
# transitions du plan admettent : chemin dont le type est converti depuis
# double, ou retiré avec le type déclaré double (ADR 0009, troisième
# annotation du 19/10/2026, (e)). Un chemin à valeurs nouvelles, ajouté, ou
# retiré avec un autre type n'en fait pas partie.
admet_reste <- function(ch) (ch$nature == "type" && ch$de == "double") || (ch$nature == "retrait" && ch$type == "double")
restes_hors_transitions <- function(restes, transitions) {
  for (plan_id in intersect(names(restes), plans_en_transition(transitions))) {
    ch <- changements_du_plan(plan_id, transitions)$changements
    restes[[plan_id]] <- setdiff(restes[[plan_id]], vapply(Filter(admet_reste, ch), `[[`, "", "chemin"))
  }
  restes
}
# Première issue dont une transition admet un reste (à défaut, la première).
issue_des_restes <- function(transitions) {
  t <- Filter(function(t) any(vapply(t$changements, admet_reste, NA)), transitions)
  (if (length(t)) t else transitions)[[1L]]$issue
}

jeu_ref <- jeu_reference()
aplatis <- list()
for (p in plans_reference()) {
  id <- p$identifiant
  x_ref <- construire_table(jeu_ref$donnees, jeu_ref$roles, p$plan)
  g <- tryCatch(aplatir_construction(x_ref), refus_reference = function(e) e)
  verifier(sprintf("%s : aplatissement sans refus d'écriture", id),
           if (inherits(g, "refus_reference")) conditionMessage(g) else TRUE)
  if (inherits(g, "refus_reference")) next
  aplatis[[id]] <- g
  f <- tempfile(fileext = ".tsv")
  ecrire_reference(g, f, c(identifiant = id))
  verifier(sprintf("%s : aller-retour lire(ecrire(aplatir(x))) identical() (types et valeurs)", id), {
    lu <- lire_reference(f)
    identical(lu$types, g$types) && identical(lu$valeurs, g$valeurs)
  })
  unlink(f)
  comparer_au_fichier <- function(id, g) {
    f <- file.path(DOSSIER_REFERENCES, paste0(id, ".tsv"))
    if (!file.exists(f)) return(sprintf("fichier de référence absent : tests/reference/%s.tsv", id))
    r <- comparer_references(lire_reference(f), g)
    cat(imprimer_comparaison(r, sprintf("  -- %s : tests/reference/%s.tsv contre l'objet recalculé", id, id)), sep = "\n")
    if (r$ok) TRUE else sprintf("comparaison en échec (%d écart(s) structurel(s), %d grandeur(s) en écart ou sans tolérance%s)",
                                length(r$structure), sum(!r$grandeurs$verdict %in% c("identique", "sous tolérance")),
                                if (r$entree$differente) ", entrée différente" else "")
  }
  nom <- sprintf("%s (%s) : comparaison à tests/reference/%s.tsv", id, p$description, id)
  present <- file.exists(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv")))
  # Échec attendu seulement si le fichier est absent : un plan encore listé
  # dont le fichier existe échoue (marque à retirer ; la comparaison devient
  # alors un test ordinaire).
  if (id %in% SANS_REFERENCE_17 && !present) echec_attendu(nom, "#17", comparer_au_fichier(id, g))
  else if (id %in% SANS_REFERENCE_17)
    verifier(nom, sprintf("tests/reference/%s.tsv existe : retirer %s de SANS_REFERENCE_17", id, id))
  else controler_plan(id, nom, lire_reference(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv"))), g,
                      comparer_au_fichier(id, g), TRANSITIONS_EN_COURS, jeu_ref$donnees, jeu_ref$roles)
}
verifier("aucune référence orpheline sous tests/reference/ (point 3.7)", {
  o <- if (dir.exists(DOSSIER_REFERENCES)) references_orphelines(DOSSIER_REFERENCES) else character(0)
  if (length(o)) paste("orphelines :", paste(o, collapse = ", ")) else TRUE
})

# Permutation des lignes de l'entrée de référence (#39) : une base dont les
# lignes sont permutées a une autre empreinte (E4.6 ne porte que sur des
# données identiques). Les sommes par cellule (somme_par_cellule(),
# R/exposition.R) suivent l'ordre des lignes : les doubles qui en sortent
# peuvent différer dans les derniers bits. Tout le reste est identical().
fichier_courant("permutation des lignes de l'entrée de référence (#39) : identité hors sommes par cellule, tolérance sur celles-ci")

GRAINE_PERMUTATION <- 20261019L
# STATUT : tolérance déclarée, appuyée sur un constat de mesure (écart relatif
# maximal 1,6e-14 par actuary le 19/10/2026, remesuré par le bilan imprimé
# ci-dessous), pas une borne démontrée. Elle est de la classe 1e-12 existante ;
# l'écart croît avec le nombre de termes sommés par cellule (1,73e-14 à 3e6
# lignes, plan R1) ; les grandeurs de la classe 1e-14 de TOLERANCES (exposition,
# taux) dépassent 1e-14 dès 1e6 lignes. À 1e5 lignes, borne_sup_exacte (classe
# 1e-12) le dépasse déjà (1,6e-14), tandis que taux et exposition_centrale
# restent à 5,73e-15 au plus. La borne de la sommation récursive n'est pas
# citée, faute d'avoir été vérifiée à la source.
# Ce test contrôle l'invariance à l'ordre des lignes ; la précision des sommes
# dans l'ordre reçu est gardée par la comparaison aux références (classe 1e-14).
# Un écart dépendant de l'ordre inférieur à 1e-12 n'y est pas détecté (témoin
# « taux altéré de 1e-13 »). R1 est couvert par R6, qui en contient tous les
# chemins aux mêmes valeurs, hors paramètres et diagnostics du brut liés à
# kaplan_meier ; R3 est le seul plan sous age_atteint_annee.
TOLERANCE_PERMUTATION <- CLASSES_TOLERANCE[["1e-12"]]
EMPREINTES_PERMUTATION <- c(CHEMIN_EMPREINTE_DONNEES, "etapes/donnees/resultat/lignes_conservees/@empreinte",
                            "etapes/donnees/resultat/observations/@empreinte")
# Doubles sommés par cellule, comparés à tolérance (nom de la grandeur).
SOMMES_PERMUTATION <- paste0("^(exposition_centrale|complement_initial_cellule|complement_initial_fenetre|",
                             "exposition_initiale|taux|probabilite|borne_(inf|sup)_(exacte|asymptotique)|",
                             "probabilite_borne_(inf|sup)_(exacte|asymptotique))$")
# Kaplan-Meier (tables et sauts) et diagnostics du brut : identical().
IDENTIQUES_PERMUTATION <- "^etapes/brut/(resultat/(kaplan_meier|kaplan_meier_sauts)|diagnostics)/"

# « identique » : identical() ; « tolérance » : somme par cellule ; « non
# classé » : fait échouer le contrôle (aucun double n'est admis sans classe).
classe_permutation <- function(chemin) {
  if (grepl(IDENTIQUES_PERMUTATION, chemin)) return("identique")
  if (identical(tolerance_de(chemin)$tol, 0)) return("identique")
  if (grepl(SOMMES_PERMUTATION, basename(chemin))) return("tolérance")
  "non classé"
}

# Compare l'aplati `g` de la construction sur lignes permutées à `g0` (ordre
# d'origine). Rend les défauts (character) et, pour chaque double comparé à
# tolérance, son écart relatif maximal.
controle_permutation <- function(g0, g, tolerance = TOLERANCE_PERMUTATION) {
  if (!identical(g$types, g0$types)) return(list(defauts = "structure : chemins ou types différents", ecarts = numeric(0)))
  ch <- names(g0$types)
  classe <- vapply(ch, function(c_) if (g0$types[[c_]] == "double") classe_permutation(c_) else "identique", "")
  defauts <- character(0)
  for (c_ in ch[classe == "non classé"]) defauts <- c(defauts, paste("double non classé :", c_))
  for (c_ in ch[classe == "identique"]) {
    egal <- identical(g$valeurs[[c_]], g0$valeurs[[c_]])
    if (c_ %in% EMPREINTES_PERMUTATION) { if (egal) defauts <- c(defauts, paste("empreinte identique, devrait différer :", c_)) }
    else if (!egal) defauts <- c(defauts, paste("diffère :", c_))
  }
  tol <- ch[classe == "tolérance"]
  # Règle de comparer_references() (ADR 0009, point 3.3) ; l'empreinte des
  # données, exigée des deux côtés, est posée identique.
  emp <- list(types = setNames("character", CHEMIN_EMPREINTE_DONNEES),
              valeurs = setNames(list(strrep("a", 32L)), CHEMIN_EMPREINTE_DONNEES))
  sous <- function(h) list(types = c(emp$types, h$types[tol]), valeurs = c(emp$valeurs, h$valeurs[tol]))
  feuilles <- unique(basename(tol))
  r <- comparer_references(sous(g0), sous(g), setNames(rep(tolerance, length(feuilles)), feuilles))
  for (s in r$structure) defauts <- c(defauts, paste("structure :", s))
  # Verdict comparé sur son motif ASCII : un littéral non ASCII de ce fichier
  # n'est pas marqué en locale C, le verdict l'est (charger_utf8()).
  admis <- r$grandeurs$verdict == "identique" | grepl("^sous tol", r$grandeurs$verdict, useBytes = TRUE)
  # Une grandeur déjà signalée par un défaut de structure (position des NA,
  # NaN ou Inf) ne l'est pas une seconde fois : son écart relatif porte sur
  # les seules paires finies et ne dit rien du défaut.
  couvert <- vapply(r$grandeurs$chemin, function(c_) any(startsWith(r$structure, paste0(c_, " : "))), NA)
  for (k in which(!admis & !couvert))
    defauts <- c(defauts, sprintf("hors tolérance %.3g (écart %.3g) : %s", tolerance, r$grandeurs$ecart_relatif_max[k],
                                  r$grandeurs$chemin[k]))
  e <- r$grandeurs$ecart_relatif_max[match(tol, r$grandeurs$chemin)]
  list(defauts = defauts, ecarts = setNames(e, tol))
}

# Deux permutations, les mêmes pour chaque plan : une tirée sous graine locale,
# l'ordre inverse (l'entrée de référence est triée par id_tete : l'ordre inverse
# la trie en sens contraire, le tirage la désordonne). rownames(d) <- NULL : le
# nom des lignes n'est pas celui de l'entrée d'origine.
n_perm <- nrow(jeu_ref$donnees)
ordres_permutation <- c(sous_graine(GRAINE_PERMUTATION, list(sample.int(n_perm))), list(n_perm:1))
names(ordres_permutation) <- c("tirage 1", "ordre inverse")
permuter <- function(o) { d <- jeu_ref$donnees[o, ]; rownames(d) <- NULL; d }

bilan_permutation <- data.frame(plan = character(0), permutation = character(0), grandeur = character(0), ecart = numeric(0))
t_perm <- system.time(for (id in c("R6", "R4", "R5", "R3", "R2")) {
  p <- plans_reference()[[id]]
  for (nom_o in names(ordres_permutation)) {
    g_p <- aplatir_construction(construire_table(permuter(ordres_permutation[[nom_o]]), jeu_ref$roles, p$plan))
    if (id == "R6" && nom_o == names(ordres_permutation)[1L]) g_permute_r6 <- g_p  # témoin de la garde 3.2 (#47)
    if (id == "R6" && nom_o == "tirage 1") G_PERMUTE_R6 <- g_p
    r <- controle_permutation(aplatis[[id]], g_p)
    verifier(sprintf("permutation (%s) : %s : structure identique, identical() hors sommes par cellule, sommes à la tolérance %g",
                     nom_o, id, TOLERANCE_PERMUTATION),
             if (length(r$defauts)) utils::head(r$defauts, 5L) else TRUE)
    bilan_permutation <- rbind(bilan_permutation, data.frame(plan = id, permutation = nom_o, grandeur = basename(names(r$ecarts)),
                                                              ecart = unname(r$ecarts)))
  }
})[["elapsed"]]
# Bilan : écart relatif maximal par plan et par grandeur, sur les permutations.
cat(sprintf("  permutation : écart relatif maximal par plan et par grandeur (%d permutations ; tolérance %g ; %.1f s)\n",
            length(ordres_permutation), TOLERANCE_PERMUTATION, t_perm))
for (id in unique(bilan_permutation$plan)) {
  b <- bilan_permutation[bilan_permutation$plan == id, ]
  m <- tapply(b$ecart, b$grandeur, max)
  cat(sprintf("    %s : %s\n", id, paste(sprintf("%s %.3g", names(m), m), collapse = " ; ")))
}
cat(sprintf("    maximum, tous plans : %.3g\n", max(bilan_permutation$ecart)))

# Témoins : le contrôle détecte un défaut (sur l'aplati de R6, tirage 1).
chemin_p <- "etapes/brut/resultat/poisson/taux"
chemin_j <- "etapes/exposition/resultat/exposition/exposition_jours"
chemin_c <- "etapes/exposition/resultat/exposition/exposition_centrale"
verifier("permutation, témoin : appariement des cellules cassé (colonne exposition_jours permutée) -> défaut", {
  g <- G_PERMUTE_R6; v <- g$valeurs[[chemin_j]]; g$valeurs[[chemin_j]] <- c(v[-1L], v[1L])
  any(grepl(chemin_j, controle_permutation(aplatis[["R6"]], g)$defauts, fixed = TRUE))
})
verifier("permutation, témoin : appariement des cellules cassé (colonne exposition_centrale permutée, grandeur à tolérance) -> défaut", {
  g <- G_PERMUTE_R6; v <- g$valeurs[[chemin_c]]; g$valeurs[[chemin_c]] <- rev(v)
  any(grepl(chemin_c, controle_permutation(aplatis[["R6"]], g)$defauts, fixed = TRUE))
})
# Écart de 1e-13 (sous 1e-12) puis de 1e-11 (au-delà) sur un taux ; tolérance 0.
# Le volet « tolérance 0 » compare R6 altéré à R6 lui-même (G_PERMUTE_R6 diffère
# déjà de R6 sous la permutation, ce qui signalerait le taux sans altération).
altere_taux <- function(facteur, g = G_PERMUTE_R6) {
  i <- which(is.finite(g$valeurs[[chemin_p]]) & g$valeurs[[chemin_p]] != 0)[1L]
  g$valeurs[[chemin_p]][i] <- g$valeurs[[chemin_p]][i] * facteur; g
}
verifier("permutation, témoin : taux altéré de 1e-13 -> accepté à 1e-12 ; altéré de 1e-11 -> défaut ; altéré de 1e-13 à la tolérance 0 -> défaut", {
  a13 <- altere_taux(1 + 1e-13)
  !length(controle_permutation(aplatis[["R6"]], a13)$defauts) &&
    any(grepl(chemin_p, controle_permutation(aplatis[["R6"]], altere_taux(1 + 1e-11))$defauts, fixed = TRUE)) &&
    !any(grepl(chemin_p, controle_permutation(aplatis[["R6"]], aplatis[["R6"]], tolerance = 0)$defauts, fixed = TRUE)) &&
    any(grepl(chemin_p, controle_permutation(aplatis[["R6"]], altere_taux(1 + 1e-13, aplatis[["R6"]]), tolerance = 0)$defauts, fixed = TRUE))
})
verifier("permutation, témoin : double non classé ajouté des deux côtés -> défaut « double non classé »", {
  c_ <- "etapes/brut/resultat/poisson/grandeur_inconnue"
  g0 <- aplatis[["R6"]]; g <- G_PERMUTE_R6
  for (nom in c("g0", "g")) { h <- get(nom); h$types[[c_]] <- "double"; h$valeurs[[c_]] <- c(0.5, 1); assign(nom, h) }
  identical(controle_permutation(g0, g)$defauts, paste("double non classé :", c_))
})
verifier("permutation, témoin : entier altéré (evenements) -> défaut ; empreintes identiques (aucune permutation) -> trois défauts", {
  g <- G_PERMUTE_R6; c_ <- "etapes/brut/resultat/poisson/evenements"; g$valeurs[[c_]][1L] <- g$valeurs[[c_]][1L] + 1L
  d1 <- controle_permutation(aplatis[["R6"]], g)$defauts
  d2 <- controle_permutation(aplatis[["R6"]], aplatis[["R6"]])$defauts
  any(grepl(c_, d1, fixed = TRUE)) && setequal(d2, paste("empreinte identique, devrait différer :", EMPREINTES_PERMUTATION)) && length(d2) == 3L
})

# Témoins (point 3.6) : base = R6 aplati (Kaplan-Meier, Inf compris), écrit
# dans un fichier temporaire et relu ; les doubles qu'aucune classe ne range
# encore (point 3.4) en sont retirés des deux côtés, pour que chaque témoin
# n'exerce que sa propre règle (le témoin « sans tolérance déclarée » les
# remet sous un autre nom).
fichier_courant("références de non-régression (ADR 0009) : témoins d'échec, règles des modes, ligne de commande")

retirer <- function(g, chemins) {
  garde <- !names(g$types) %in% chemins
  list(types = g$types[garde], valeurs = g$valeurs[garde])
}
base <- retirer(aplatis[["R6"]], doubles_sans_tolerance(aplatis[["R6"]]))
F_BASE <- tempfile("reference_temoin_", fileext = ".tsv")
# En-tête ASCII : un littéral non ASCII de ce fichier n'est pas marqué en
# locale C et serait refusé (règle de l'ADR 0007, point 9.3).
ecrire_reference(base, F_BASE, c(identifiant = "R6 (base des temoins)"))
ref0 <- lire_reference(F_BASE)
# Copie altérée de la référence, repassée par le fichier (écriture, lecture).
par_fichier <- function(g) {
  f <- tempfile(fileext = ".tsv"); on.exit(unlink(f))
  ecrire_reference(g, f); lire_reference(f)
}
alterer <- function(chemin, facteur, ref = ref0) {
  v <- ref$valeurs[[chemin]]
  i <- which(is.finite(v) & v != 0)[1L]
  ref$valeurs[[chemin]][i] <- v[i] * facteur
  par_fichier(ref)
}
# Chaînes comparées octet par octet : les littéraux de ce fichier ne sont
# pas marqués en locale C, ceux de l'outillage le sont (charger_utf8()).
meme <- function(a, b) length(a) == 1L && length(b) == 1L && identical(charToRaw(a), charToRaw(b))
verdict <- function(r, chemin) r$grandeurs$verdict[r$grandeurs$chemin == chemin]
attendu <- function(r, ok, chemin = NULL, v = NULL) {
  if (!identical(r$ok, ok)) return(sprintf("ok = %s au lieu de %s", r$ok, ok))
  if (!is.null(chemin) && !meme(verdict(r, chemin), v))
    return(sprintf("verdict « %s » au lieu de « %s » pour %s", paste(verdict(r, chemin), collapse = ""), v, chemin))
  TRUE
}

verifier("témoin : la base relue se compare à elle-même sans écart (ok, aucune grandeur différente)", {
  r <- comparer_references(ref0, base)
  isTRUE(attendu(r, TRUE)) && all(r$grandeurs$verdict == "identique")
})
P_TAUX <- "etapes/brut/resultat/poisson/taux"
P_BORNE <- "etapes/brut/resultat/poisson/borne_sup_exacte"
P_KM <- "etapes/brut/resultat/kaplan_meier/taux"
P_JOURS <- "etapes/exposition/resultat/exposition/exposition_jours"
for (cas in list(
  list(chemin = P_TAUX, facteur = 1 + 2e-14, ok = FALSE, v = "écart", classe = "1e-14, au-delà"),
  list(chemin = P_TAUX, facteur = 1 + 0.5e-14, ok = TRUE, v = "sous tolérance", classe = "1e-14, juste en dessous"),
  list(chemin = P_BORNE, facteur = 1 + 2e-12, ok = FALSE, v = "écart", classe = "1e-12, au-delà"),
  list(chemin = P_BORNE, facteur = 1 + 0.5e-12, ok = TRUE, v = "sous tolérance", classe = "1e-12, juste en dessous"),
  list(chemin = P_JOURS, facteur = 1 + .Machine$double.eps, ok = FALSE, v = "écart", classe = "exacte, un ulp"),
  # Clé qualifiée prioritaire : kaplan_meier/taux (1e-12) l'emporte sur taux
  # (1e-14) ; la même altération sur poisson/taux est un écart.
  list(chemin = P_KM, facteur = 1 + 0.5e-12, ok = TRUE, v = "sous tolérance", classe = "clé qualifiée kaplan_meier/taux (1e-12)"),
  list(chemin = P_TAUX, facteur = 1 + 0.5e-12, ok = FALSE, v = "écart", classe = "même altération sur poisson/taux (1e-14)")
)) {
  verifier(sprintf("témoin : %s altéré, classe %s -> %s", cas$chemin, cas$classe, if (cas$ok) "succès" else "échec"),
           attendu(comparer_references(alterer(cas$chemin, cas$facteur), base), cas$ok, cas$chemin, cas$v))
}
verifier("témoin : clé retenue pour kaplan_meier/taux = kaplan_meier/taux (aussi : taux), imprimée", {
  t <- tolerance_de(P_KM)
  r <- comparer_references(ref0, base)
  identical(t$cle, "kaplan_meier/taux") && identical(t$tol, 1e-12) && identical(t$autres, "taux") &&
    any(grepl("clé retenue : etapes/brut/resultat/kaplan_meier/taux -> kaplan_meier/taux (classe 1e-12 ; aussi : taux)",
              imprimer_comparaison(r), fixed = TRUE, useBytes = TRUE))
})
verifier("témoin : impression d'un écart (chemin, avant et après en %.17g, tolérance, verdict) et des trois classes", {
  avant <- alterer(P_TAUX, 1 + 2e-14)
  r <- comparer_references(avant, base)
  l <- imprimer_comparaison(r)
  e <- r$grandeurs[r$grandeurs$chemin == P_TAUX, ]
  sum(grepl("Écart relatif maximal, classe", l, fixed = TRUE, useBytes = TRUE)) == 3L &&
    any(grepl(sprintf("%s | 149 | 1 | ", P_TAUX), l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl(sprintf("| %s | %s | 1e-14 | écart", e$avant, e$apres), l, fixed = TRUE, useBytes = TRUE)) &&
    identical(e$avant, sprintf("%.17g", avant$valeurs[[P_TAUX]][e$position])) &&
    identical(e$apres, sprintf("%.17g", base$valeurs[[P_TAUX]][e$position])) && e$avant != e$apres
})
verifier("témoin : grandeur retirée -> écart structurel « manquant »", {
  r <- comparer_references(ref0, retirer(base, P_BORNE))
  isTRUE(attendu(r, FALSE)) && identical(r$structure, paste(P_BORNE, ": manquant"))
})
verifier("témoin : grandeur ajoutée -> écart structurel « en trop »", {
  g <- base; g$types[["etapes/brut/resultat/nouvelle"]] <- "integer"; g$valeurs[["etapes/brut/resultat/nouvelle"]] <- 1L
  r <- comparer_references(ref0, par_fichier(g))
  isTRUE(attendu(r, FALSE)) && identical(r$structure, "etapes/brut/resultat/nouvelle : en trop")
})
verifier("témoin : type changé (integer -> double) -> écart structurel « type »", {
  g <- base; c_ <- "etapes/brut/resultat/poisson/evenements"
  g$types[[c_]] <- "double"; g$valeurs[[c_]] <- as.double(g$valeurs[[c_]])
  r <- comparer_references(ref0, par_fichier(g))
  isTRUE(attendu(r, FALSE)) && identical(r$structure, paste(c_, ": type double au lieu de integer"))
})
verifier("témoin : longueur changée -> écart structurel « longueur »", {
  g <- base; g$valeurs[[P_TAUX]] <- g$valeurs[[P_TAUX]][-1L]
  r <- comparer_references(ref0, par_fichier(g))
  isTRUE(attendu(r, FALSE)) && identical(r$structure, paste(P_TAUX, ": longueur 148 au lieu de 149"))
})
verifier("témoin : empreinte des données altérée -> « entrée différente », comparaison poursuivie et imprimée", {
  g <- base; g$valeurs[[CHEMIN_EMPREINTE_DONNEES]] <- strrep("0", 32L)
  r <- comparer_references(ref0, par_fichier(g))
  l <- imprimer_comparaison(r)
  isTRUE(attendu(r, FALSE)) && isTRUE(r$entree$differente) && identical(r$entree$obtenu, strrep("0", 32L)) &&
    any(grepl("ENTRÉE DIFFÉRENTE", l, fixed = TRUE, useBytes = TRUE)) && any(grepl("non interprétables", l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("Bilan : ", l, fixed = TRUE, useBytes = TRUE)) &&
    meme(verdict(r, CHEMIN_EMPREINTE_DONNEES), "écart")
})
verifier("témoin : double de nom inconnu, identique des deux côtés -> « sans tolérance déclarée », échec", {
  g <- base; c_ <- "etapes/brut/resultat/poisson/grandeur_inconnue"
  g$types[[c_]] <- "double"; g$valeurs[[c_]] <- c(0.5, 1)
  g <- par_fichier(g)
  r <- comparer_references(g, g)
  isTRUE(attendu(r, FALSE, c_, "sans tolérance déclarée")) && identical(r$sans_tolerance, c_) &&
    any(grepl(paste("grandeur sans tolérance déclarée :", c_), imprimer_comparaison(r), fixed = TRUE, useBytes = TRUE)) &&
    identical(doubles_sans_tolerance(g), c_)
})
# Aucune clé pour une grandeur integer (ADR 0009, quatrième annotation, (b)) :
# copie en mémoire de R6, rien n'est écrit sous tests/reference/.
verifier("témoin : kaplan_meier_sauts/numerateur remis en double -> relevé par doubles_sans_tolerance(), que la clé retirée aurait masqué", {
  g <- aplatis[["R6"]]
  c_ <- grep("kaplan_meier_sauts/numerateur$", names(g$types), value = TRUE)
  if (length(c_) != 1L || !identical(unname(g$types[[c_]]), "integer")) return("numerateur de R6 : un chemin integer attendu")
  masque <- c(TOLERANCES, "kaplan_meier_sauts/numerateur" = 0)
  g$types[[c_]] <- "double"; g$valeurs[[c_]] <- as.double(g$valeurs[[c_]])
  c_ %in% doubles_sans_tolerance(g) && !c_ %in% doubles_sans_tolerance(g, masque)
})
# Les trois chemins que la clé nue `cellules` classait (#37) : le dénombrement
# de l'étape exposition (integer depuis #37) et les deux de l'étape brut. Pour
# chacun : integer dans R6 (sinon le moteur est revenu au double, et seul le
# témoin de ce chemin échoue), puis remis en double en mémoire, il est relevé
# par doubles_sans_tolerance() alors que la clé nue le classerait.
for (chemin_cellules in c("etapes/exposition/diagnostics/cellules", "etapes/brut/diagnostics/cellules",
             "etapes/brut/diagnostics/etats/cellules")) {
  verifier(sprintf("témoin : %s remis en double -> relevé par doubles_sans_tolerance(), que la clé nue cellules aurait masqué", chemin_cellules), {
    g <- aplatis[["R6"]]
    if (!identical(unname(g$types[chemin_cellules]), "integer"))
      return(sprintf("%s de R6 : un chemin integer attendu, type %s", chemin_cellules, unname(g$types[chemin_cellules])))
    masque <- c(TOLERANCES, cellules = 0)
    g$types[[chemin_cellules]] <- "double"; g$valeurs[[chemin_cellules]] <- as.double(g$valeurs[[chemin_cellules]])
    chemin_cellules %in% doubles_sans_tolerance(g) && !chemin_cellules %in% doubles_sans_tolerance(g, masque)
  })
}
restes_commites <- function() lapply(plans_reference(), function(p) {
  f <- file.path(DOSSIER_REFERENCES, paste0(p$identifiant, ".tsv"))
  if (!file.exists(f)) return(paste("absent :", f))
  doubles_sans_tolerance(lire_reference(f))
})
restes_vides <- function(restes)
  if (all(lengths(restes) == 0L)) TRUE else paste(names(restes)[lengths(restes) > 0L], collapse = ", ")
nom_restes <- "tolérances : aucun double sans tolérance déclarée dans les six références commitées (table sans clé integer)"
# Plans en transition (#47) : tant que la référence commitée porte un double
# que les transitions admettent (type converti depuis double, ou retrait), ce
# reste est admis par un test ordinaire et le test strict est un échec
# attendu. Une fois les références régénérées, il n'y a plus de reste : le test
# strict est ordinaire (un plan resté listé est relevé par controler_plan()).
restes <- restes_commites()
if (length(plans_en_transition(TRANSITIONS_EN_COURS)) && any(lengths(restes) > 0L)) {
  verifier("tolérances : dans les références à régénérer, seuls les doubles des chemins admis par les transitions (type converti depuis double, retrait) restent sans tolérance déclarée",
           restes_vides(restes_hors_transitions(restes, TRANSITIONS_EN_COURS)))
  echec_attendu(nom_restes, issue_des_restes(TRANSITIONS_EN_COURS), restes_vides(restes))
} else verifier(nom_restes, restes_vides(restes))
# Règle élément par élément (seconde annotation, (c)) : mesure d'actuary,
# a = c(Inf, 0, 0.1 + 0.2), b = c(Inf, 0, 0.3), tol = 1e-14 -> égaux ;
# Inf contre -Inf et NA contre NaN restent des écarts (structure).
# Grandeurs minimales, empreinte des données comprise (exigée des deux côtés).
mini <- function(v) list(types = c("fiche/empreintes/donnees" = "character", x = "double"),
                         valeurs = list("fiche/empreintes/donnees" = strrep("a", 32L), x = v))
verifier("témoin : empreinte des données absente d'un côté ou des deux -> échec (point 3.2)", {
  sans <- function(g) list(types = g$types[-1L], valeurs = g$valeurs[-1L])
  r1 <- comparer_references(mini(1), sans(mini(1)), c(x = 0)); r2 <- comparer_references(sans(mini(1)), sans(mini(1)), c(x = 0))
  !r1$ok && !r2$ok && any(grepl("empreinte des donn", r1$structure, fixed = TRUE, useBytes = TRUE)) &&
    sum(grepl("empreinte des donn", r2$structure, fixed = TRUE, useBytes = TRUE)) == 2L &&
    comparer_references(mini(1), mini(1), c(x = 0))$ok
})
verifier("témoin : Inf contre Inf égal, 0 contre 0 égal, 0.1 + 0.2 contre 0.3 sous tolérance (élément par élément)", {
  r <- comparer_references(mini(c(Inf, 0, 0.1 + 0.2)), mini(c(Inf, 0, 0.3)), c(x = 1e-14))
  isTRUE(attendu(r, TRUE, "x", "sous tolérance")) && identical(r$grandeurs$n_differents[r$grandeurs$chemin == "x"], 1L) &&
    identical(r$grandeurs$position[r$grandeurs$chemin == "x"], 3L)
})
verifier("témoin : Inf de la base (variance_log_survie_cellule) comparé à lui-même : identique", {
  c_ <- "etapes/brut/resultat/kaplan_meier_sauts/variance_log_survie_cellule"
  any(is.infinite(ref0$valeurs[[c_]])) && meme(verdict(comparer_references(ref0, base), c_), "identique")
})
for (cas in list(list(a = Inf, b = -Inf, nom = "Inf contre -Inf"), list(a = NA_real_, b = NaN, nom = "NA contre NaN"),
                 list(a = 1, b = Inf, nom = "fini contre Inf"), list(a = 0, b = 1e-300, nom = "zéro contre non nul"),
                 list(a = 1, b = -1, nom = "signes opposés"))) {
  verifier(sprintf("témoin : %s -> écart, même à la tolérance 1e-12", cas$nom), {
    r <- comparer_references(mini(cas$a), mini(cas$b), c(x = 1e-12))
    attendu(r, FALSE, "x", "écart")
  })
}
# Refus d'écriture (point 2.3) : classe ou attribut hors format, avec le
# chemin fautif ; rien n'est converti en silence.
x6_ref <- construire_table(jeu_ref$donnees, jeu_ref$roles, plans_reference()$R6$plan)
refus_ecriture <- function(x, chemin) {
  e <- tryCatch({ aplatir_construction(x); NULL }, refus_reference = function(e) e)
  if (is.null(e)) return("aucun refus")
  if (!identical(e$chemin, chemin)) return(sprintf("chemin fautif %s au lieu de %s", e$chemin, chemin))
  TRUE
}
verifier("témoin : grandeur POSIXct -> refus d'écriture, chemin fautif cité", {
  x <- x6_ref; x$etapes$brut$diagnostics$horodatage <- as.POSIXct("2026-10-17 12:00:00", tz = "UTC")
  refus_ecriture(x, "etapes/brut/diagnostics/horodatage")
})
verifier("témoin : attribut inattendu sur une colonne -> refus d'écriture, chemin fautif cité", {
  x <- x6_ref; attr(x$etapes$brut$resultat$poisson$taux, "unite") <- "an"
  refus_ecriture(x, P_TAUX)
})
verifier("témoin : difftime, matrice, liste classée, noms de lignes -> refus d'écriture", {
  x1 <- x6_ref; x1$fiche$duree <- as.difftime(1, units = "secs")
  x2 <- x6_ref; x2$etapes$brut$diagnostics$m <- matrix(1:4, 2L)
  x3 <- x6_ref; x3$etapes$brut$diagnostics$l <- structure(list(a = 1L), class = "autre")
  x4 <- x6_ref; rownames(x4$avertissements) <- paste0("l", seq_len(nrow(x4$avertissements)))
  isTRUE(refus_ecriture(x1, "fiche/duree")) && isTRUE(refus_ecriture(x2, "etapes/brut/diagnostics/m")) &&
    isTRUE(refus_ecriture(x3, "etapes/brut/diagnostics/l")) && isTRUE(refus_ecriture(x4, "avertissements"))
})
verifier("témoin : l'attribut simulation de lignes_conservees n'entre pas dans l'empreinte ; une valeur changée, si", {
  x <- x6_ref; attr(x$etapes$donnees$resultat$lignes_conservees, "simulation") <- NULL
  y <- x6_ref; y$etapes$donnees$resultat$lignes_conservees$sexe[1L] <- "X"
  ch <- "etapes/donnees/resultat/lignes_conservees/@empreinte"
  identical(aplatir_construction(x)$valeurs[[ch]], aplatis[["R6"]]$valeurs[[ch]]) &&
    !identical(aplatir_construction(y)$valeurs[[ch]], aplatis[["R6"]]$valeurs[[ch]])
})
verifier("témoin : execution, fiche$versions et fiche$plateforme hors référence ; fiche$version_fiche comparée", {
  ch <- names(aplatis[["R6"]]$types)
  !any(grepl("^execution|^fiche/versions|^fiche/plateforme", ch)) && "fiche/version_fiche" %in% ch &&
    !"execution" %in% aplatis[["R6"]]$valeurs[["@noms"]]
})
# Lecture stricte (point 2.4) : valeur non canonique, CR, longueur fausse.
lecture_refusee <- function(remplacer) {
  l <- readLines(F_BASE, encoding = "UTF-8")
  f <- tempfile(fileext = ".tsv"); on.exit(unlink(f))
  writeBin(remplacer(charToRaw(paste0(l, "\n", collapse = ""))), f)
  e <- tryCatch({ lire_reference(f); NULL }, error = function(e) e)
  if (is.null(e)) "aucune erreur de lecture" else TRUE
}
verifier("témoin : lecture refusée sur un double hors de la forme %.17g (zéro terminal), un CR, une longueur fausse", {
  sub_txt <- function(motif, remplacement) function(o)
    charToRaw(sub(motif, remplacement, rawToChar(o), fixed = TRUE, useBytes = TRUE))
  isTRUE(lecture_refusee(sub_txt("fiche/plan/etapes/brut/niveau\tdouble\t1\t0.94999999999999996",
                                 "fiche/plan/etapes/brut/niveau\tdouble\t1\t0.949999999999999960"))) &&
    isTRUE(lecture_refusee(sub_txt("\n@classe", "\r\n@classe"))) &&
    isTRUE(lecture_refusee(sub_txt("table\tNULL\t0", "table\tNULL\t1")))
})
# Lecture des doubles (point 2.4 : mêmes valeurs au bit près), deux branches de
# double_lu() choisies par `windows` et testées ici sur toute plateforme.
# Branche Windows : forme de %.17g seulement, car la bibliothèque C de Windows
# n'arrondit pas toujours correctement le 17e chiffre ; les textes glibc et
# Windows d'un même double se relisent en le même double. Branche stricte
# (hors Windows) : preuve d'aller-retour, texte == sprintf("%.17g") de la
# valeur lue ; elle s'appuie sur le sprintf de l'hôte, donc, pour passer à
# l'identique sous Linux et sous Windows, les cas à sprintf réel sont des
# textes que le sprintf de Windows écrit correctement (mesuré), et le texte
# glibc de 0x1.422927add0ecbp-18 (que Windows écrit à 16 chiffres) est testé
# par un formateur simulé qui renvoie le texte de glibc (Python, hors R).
# Doubles construits depuis leurs octets (petit-boutiste), aucun littéral.
double_bits <- function(...) readBin(as.raw(c(...)), "double", 1L, size = 8L, endian = "little")
meme_double <- function(a, b) identical(a, b, num.eq = FALSE)
X_ECB <- double_bits(0xcb, 0x0e, 0xdd, 0x7a, 0x92, 0x22, 0xd4, 0x3e)  # 0x1.422927add0ecbp-18
T_GLIBC <- "4.8005694404694591e-06"; T_WINDOWS <- "4.800569440469459e-06"
formateur_glibc <- function(v) { w <- sprintf("%.17g", v); w[which(v == X_ECB)] <- T_GLIBC; w }
lu_windows <- function(s) double_lu(s, "(témoin)", "x", windows = TRUE)
lu_stricte <- function(s) double_lu(s, "(témoin)", "x", windows = FALSE, formater = formateur_glibc)
lu_stricte_reelle <- function(s) double_lu(s, "(témoin)", "x", windows = FALSE)  # sprintf de l'hôte
refuse_lecture <- function(lire, s) vapply(s, function(t) inherits(tryCatch(lire(t), error = function(e) e), "error"), NA)
TEXTES_HORS_FORME <- c("1.0", "+0.5", " 0.5", "0.5 ", "00.5", ".5", "5.", "0x1p-3", "1e5", "1E+05", "1e+05", "1e-4", "0.00001", "0.10",
                       "1.50", "1e-006", "1e+017", "0e+00", "inf", "nan", "na", "NA ", "+Inf", "TRUE", "1,5", "", "-", "1e+17.5",
                       "1.2345678901234567890", "123456789012345678", "1e+400", "1e-400")
verifier("lecture des doubles, branche Windows : textes glibc et Windows de 0x1.422927add0ecbp-18 acceptés, même double", {
  meme_double(lu_windows(T_GLIBC), X_ECB) && meme_double(lu_windows(T_WINDOWS), X_ECB) &&
    meme_double(lu_windows(c(T_GLIBC, "NA", "-Inf")), c(X_ECB, NA_real_, -Inf))
})
verifier("lecture des doubles, branche Windows : le texte du double voisin est un autre double (aucune confusion)", {
  voisin <- double_bits(0xcc, 0x0e, 0xdd, 0x7a, 0x92, 0x22, 0xd4, 0x3e)
  t_voisin <- "4.8005694404694599e-06"  # %.17g du voisin 0x1.422927add0eccp-18 (Python, hors R)
  meme_double(lu_windows(t_voisin), voisin) && !meme_double(lu_windows(t_voisin), X_ECB)
})
verifier("lecture des doubles, branche Windows : formes de %.17g acceptées (NA, NaN, Inf, notations fixe et à exposant, bornes)", {
  ok <- c("NA", "NaN", "Inf", "-Inf", "0", "-0", "0.5", "-1.5", "100000", "0.0001", "1e-05", "9.9999999999999995e-07",
          "1e+17", "1.2345678901234568e+17", "5e-324", "1.7976931348623157e+308", "0.94999999999999996")
  mal <- ok[refuse_lecture(lu_windows, ok)]
  if (length(mal)) paste("refusés à tort :", paste(mal, collapse = " ")) else TRUE
})
verifier("lecture des doubles, branche Windows : textes hors de la forme %.17g refusés (zéro terminal, espace, +, zéro de tête, 0x, exposant, 18 chiffres, dépassement)", {
  mal <- TEXTES_HORS_FORME[!refuse_lecture(lu_windows, TEXTES_HORS_FORME)]
  if (length(mal)) paste("acceptés à tort :", paste(mal, collapse = " ")) else TRUE
})
verifier("lecture des doubles, branche stricte : textes que sprintf(\"%.17g\") reproduit acceptés (texte glibc simulé, NA, NaN, Inf, bornes)", {
  ok <- c("0.94999999999999996", "NA", "NaN", "Inf", "-Inf", "0", "-0", "0.5", "-1.5", "100000", "0.10000000000000001",
          "1.7976931348623157e+308")
  mal <- ok[refuse_lecture(lu_stricte_reelle, ok)]
  if (length(mal)) paste("refusés à tort :", paste(mal, collapse = " ")) else
    meme_double(lu_stricte(T_GLIBC), X_ECB) && meme_double(lu_stricte(c(T_GLIBC, "NA", "-Inf")), c(X_ECB, NA_real_, -Inf))
})
verifier("lecture des doubles, branche stricte : 0.95, le texte à 16 chiffres de Windows et tout texte hors de la forme refusés", {
  # 0.95 est un texte plus court que sprintf("%.17g", 0.95) = 0.94999999999999996
  r <- c(refuse_lecture(lu_stricte_reelle, "0.95"), refuse_lecture(lu_stricte, T_WINDOWS))
  mal <- TEXTES_HORS_FORME[!refuse_lecture(lu_stricte_reelle, TEXTES_HORS_FORME)]
  if (!all(r)) "0.95 ou le texte Windows accepté" else if (length(mal)) paste("acceptés à tort :", paste(mal, collapse = " ")) else TRUE
})
verifier("lecture des doubles, branche stricte : 0.001673229520730213, que as.numeric() lit de travers, refusé", {
  # as.numeric() lit 9ac77e84086a5b3f (0.0016732295207302129) au lieu du double le plus proche, 9bc77e84086a5b3f ;
  # le %.17g correct de ce dernier est 0.0016732295207302131 (Python, hors R) : aucun sprintf correct ne produit le texte.
  refuse_lecture(lu_stricte_reelle, "0.001673229520730213")
})
verifier("lecture des doubles : les messages de refus distinguent les deux branches", {
  msg <- function(...) tryCatch(double_lu("0.95", "(f)", "x", ...), error = function(e) conditionMessage(e))
  m_s <- msg(windows = FALSE); m_w <- tryCatch(double_lu("1.0", "(f)", "x", windows = TRUE), error = function(e) conditionMessage(e))
  grepl("sprintf", m_s, fixed = TRUE) && grepl("Windows", m_w, fixed = TRUE) && !grepl("Windows", m_s, fixed = TRUE)
})
verifier("lecture des doubles : aller-retour exact d'un fichier, branche de l'hôte (valeurs usuelles, bornes, NA, NaN, Inf)", {
  v <- c(0.1, 1 / 3, 0.94999999999999996, 1e-300, 5e-324, .Machine$double.xmax, .Machine$double.xmin, 123456789.123456789, -2^52,
         2^-18 * 1.2587401, 1e15, 1e16, 1e17, 1e-4, 1e-5, 0, NA, NaN, Inf, -Inf)
  g <- list(types = c(x = "double"), valeurs = list(x = v))
  f <- tempfile(fileext = ".tsv"); on.exit(unlink(f))
  ecrire_reference(g, f, c(identifiant = "doubles"))
  meme_double(lire_reference(f)$valeurs$x, v)
})
verifier("témoin : référence orpheline détectée (R7.tsv dans un dossier temporaire)", {
  d <- tempfile("references_"); dir.create(d)
  file.copy(F_BASE, file.path(d, "R6.tsv")); file.copy(F_BASE, file.path(d, "R7.tsv"))
  o <- references_orphelines(d); unlink(d, recursive = TRUE)
  identical(o, "R7.tsv")
})
# Règles des modes du workflow (point 5.2), table de vérité.
verifier("règles des modes : creation, regeneration, bascule, releve (table de vérité)", {
  cas <- expand.grid(mode = c("creation", "regeneration", "bascule", "releve"), vise = c(TRUE, FALSE),
                     present = c(TRUE, FALSE), reussie = c(TRUE, FALSE), stringsAsFactors = FALSE)
  cas <- cas[cas$present | !cas$reussie, ]
  attendu_ <- with(cas, ifelse(mode == "releve", TRUE, ifelse(mode == "bascule", present,
    ifelse(vise, ifelse(mode == "creation", !present, present), present & reussie))))
  obtenu <- is.na(mapply(regle_mode, cas$mode, cas$vise, cas$present, cas$reussie))
  if (identical(unname(obtenu), attendu_)) TRUE else
    paste("écart :", paste(apply(cas[obtenu != attendu_, ], 1L, paste, collapse = "/"), collapse = " ; "))
})
verifier("bascule : refusée si l'identifiant commité est celui de l'exécution, s'il manque ou s'il diffère d'un fichier à l'autre ; admise sinon", {
  a <- c("Image : x", "R : 4.3.3"); b <- c("Image : y", "R : 4.3.3")
  !is.na(controle_bascule(list(R1 = a, R2 = a), a)) && !is.na(controle_bascule(list(), a)) &&
    !is.na(controle_bascule(list(R1 = a, R2 = b), b)) && is.na(controle_bascule(list(R1 = a, R2 = a), b))
})
verifier("garde du dossier de sortie : tests/reference/ et ses sous-dossiers interdits", {
  sortie_interdite(DOSSIER_REFERENCES, DOSSIER_REFERENCES) &&
    sortie_interdite(file.path(DOSSIER_REFERENCES, "x", "y"), DOSSIER_REFERENCES) &&
    sortie_interdite(paste0(DOSSIER_REFERENCES, "/"), DOSSIER_REFERENCES) &&
    !sortie_interdite(file.path(tempdir(), "sortie"), DOSSIER_REFERENCES) &&
    !sortie_interdite(paste0(DOSSIER_REFERENCES, "_bis"), DOSSIER_REFERENCES) &&
    sortie_interdite(file.path("tests", "reference", "essai"), DOSSIER_REFERENCES, courant = RACINE)
})
# Hôte Windows (formes réelles du système de fichiers) : antislash, casse et
# nom court 8.3 désignent le même dossier.
if (.Platform$OS.type == "windows") {
  verifier("garde du dossier de sortie, hôte Windows : antislash, casse et nom court (8.3) de tests/reference/ interdits", {
    sortie_interdite(gsub("/", "\\\\", file.path(DOSSIER_REFERENCES, "x")), DOSSIER_REFERENCES) &&
      sortie_interdite(toupper(file.path(DOSSIER_REFERENCES, "x")), DOSSIER_REFERENCES) &&
      sortie_interdite(file.path(utils::shortPathName(DOSSIER_REFERENCES), "x"), DOSSIER_REFERENCES)
  })
} else cat("  [sauté] garde du dossier de sortie, hôte Windows : plateforme POSIX (formes Windows couvertes par le texte seul ci-dessous)\n")
# Hôte POSIX : « .. » après un lien. `<lien vers tests/reference/sous>/../x`
# désigne physiquement tests/reference/x (le système résout le lien avant
# « .. ») : le texte seul le classe hors du dossier, la résolution du texte
# brut l'interdit. Arbre factice dans un dossier temporaire.
lien_ok <- FALSE
if (.Platform$OS.type != "windows") {
  d_lien <- tempfile("garde_lien_"); dir.create(d_lien)
  commis_f <- file.path(normalizePath(d_lien), "depot", "tests", "reference"); dir.create(file.path(commis_f, "sous"), recursive = TRUE)
  dir.create(file.path(d_lien, "ext", "autre", "sous"), recursive = TRUE)
  lien_ok <- file.symlink(file.path(commis_f, "sous"), file.path(d_lien, "ext", "lien")) &&
    file.symlink(file.path(d_lien, "ext", "autre", "sous"), file.path(d_lien, "ext", "lien_autre"))
}
if (lien_ok) {
  verifier("garde du dossier de sortie, hôte POSIX : « .. » après un lien vers un sous-dossier de tests/reference/", {
    ext <- file.path(normalizePath(d_lien), "ext")
    interdits <- c(file.path(ext, "lien", "..", "x"), file.path(ext, "lien", "..", "neuf", "y"), file.path(ext, "lien", ".."),
                   file.path(ext, "lien", "..", "sous", "z"), file.path(ext, "lien", "z"))
    autorises <- c(file.path(ext, "x"), file.path(ext, "lien_autre", "..", "x"), file.path(ext, "lien", "..", "..", "x"),
                   file.path(ext, "lien", "..", "..", "ext", "y"), file.path(ext, "lien_autre", "..", ".."))
    est <- function(s) vapply(s, sortie_interdite, NA, commis = commis_f)
    mal <- c(interdits[!est(interdits)], autorises[est(autorises)])
    if (length(mal)) paste("mal classés :", paste(mal, collapse = " | ")) else TRUE
  })
} else cat("  [sauté] garde du dossier de sortie, hôte POSIX : liens symboliques indisponibles (hôte Windows ou système de fichiers sans liens)\n")
if (exists("d_lien", inherits = FALSE)) unlink(d_lien, recursive = TRUE)
# Formes de chemin (texte seul, `resoudre = FALSE` : exécutée sur toute
# plateforme, `courant` et `windows` posés) ; sous Windows, `/` et `\`
# séparent, un chemin absolu est `X:/`, `X:\`, UNC ou `/` (lecteur courant),
# la casse est ignorée, `X:rel` est relatif au lecteur X.
verifier("garde du dossier de sortie : formes de chemin Windows (séparateurs, lecteur, casse, UNC, relatifs)", {
  cw <- "C:/Depot/outil_tables"; rw <- "C:/Depot/outil_tables/tests/reference"
  interdits <- c("tests/reference/essai", "tests\\reference\\essai", "tests/reference", "tests/reference/", "./tests/../tests/reference/x",
                 "C:/Depot/outil_tables/tests/reference/x", "C:\\Depot\\outil_tables\\tests\\reference\\x",
                 "c:/depot/OUTIL_TABLES/Tests/Reference/x", "c:\\Depot\\outil_tables\\tests\\reference",
                 "/Depot/outil_tables/tests/reference/x", "C:tests/reference/x", "\\\\?\\C:\\Depot\\outil_tables\\tests\\reference\\x",
                 "C:/Depot/outil_tables/tests/reference/../reference/x", "C:\\Depot\\outil_tables\\tests\\reference\\\\x\\")
  autorises <- c("C:/Temp/sortie", "D:/Depot/outil_tables/tests/reference/x", "tests/reference_bis",
                 "C:/Depot/outil_tables/tests/referenceX", "C:/Depot/outil_tables/tests", "..", "tests/reference/../autre",
                 "C:autre", "\\Temp\\x", "E:\\", "C:\\")
  est <- function(s) vapply(s, sortie_interdite, NA, commis = rw, courant = cw, windows = TRUE, resoudre = FALSE)
  mal <- c(interdits[!est(interdits)], autorises[est(autorises)])
  if (length(mal)) paste("mal classés :", paste(mal, collapse = " | ")) else TRUE
})
verifier("garde du dossier de sortie : formes UNC (\\\\serveur\\partage, préfixe \\\\?\\UNC\\) et formes POSIX (casse, antislash)", {
  u <- function(s) sortie_interdite(s, "//serveur/partage/depot/tests/reference", "//serveur/partage/depot",
                                    windows = TRUE, resoudre = FALSE)
  p <- function(s) sortie_interdite(s, "/home/u/depot/tests/reference", "/home/u/depot", windows = FALSE, resoudre = FALSE)
  isTRUE(u("\\\\SERVEUR\\Partage\\depot\\tests\\reference\\x")) && isTRUE(u("\\\\?\\UNC\\serveur\\partage\\depot\\tests\\reference\\x")) &&
    isTRUE(u("tests/reference/x")) && !u("\\\\serveur\\autre\\depot\\tests\\reference\\x") && !u("\\\\serveur\\partage\\depot\\tests") &&
    all(vapply(c("tests/reference/x", "/home/u/depot/tests/reference", "/home/u/depot/tests/reference/",
                 "tests/./reference/../reference/x"), p, NA)) &&
    !any(vapply(c("/home/u/depot/tests/reference_bis", "/tmp/x", "/home/u/depot/Tests/reference", "tests\\reference", "/"), p, NA))
})
# Scripts en sous-processus : refus d'appel du producteur (avant toute
# installation ni écriture) ; ligne de commande de comparaison. Les modes
# creation, regeneration et bascule ne s'exécutent que sur la CI (issue #36) :
# les témoins qui les exercent posent GITHUB_ACTIONS=true dans leur
# sous-processus (env), faute de quoi la garde les refuserait avant le contrôle
# visé ; le cas sans CI de chaque mode est testé plus bas. Sur un poste, ces
# témoins contournent donc la garde « CI seule » (issue #46) : chacun doit
# s'arrêter sur le contrôle visé avant toute écriture, et le test échoue si le
# dossier de sortie demandé existe après l'appel (le producteur le crée avant
# toute production : une production non refusée d'emblée est détectée même
# après un réordonnancement des contrôles).
PRODUCTEUR <- file.path(RACINE, "tests", "reference", "produire_references.R")
COMPARATEUR <- file.path(RACINE, "tests", "reference", "comparer_references.R")
EN_CI <- "GITHUB_ACTIONS=true"
for (cas in list(
  list(args = c("--mode", "inconnu", "--plans", "tous", "--sortie", tempfile()), motif = "--mode parmi"),
  list(args = c("--mode", "creation", "--plans", "tous", "--sortie", tempfile()), motif = "--motif exigé", env = EN_CI),
  list(args = c("--mode", "bascule", "--plans", "R1", "--sortie", tempfile(), "--motif", "x"), motif = "--plans tous obligatoire",
       env = EN_CI),
  list(args = c("--mode", "creation", "--plans", "R9", "--sortie", tempfile(), "--motif", "x"), motif = "--plans : ", env = EN_CI),
  list(args = c("--mode", "regeneration", "--plans", "R9", "--sortie", tempfile(), "--motif", "x"), motif = "--plans : ", env = EN_CI),
  list(args = c("--mode", "creation", "--plans", "tous", "--sortie", shQuote(file.path(DOSSIER_REFERENCES, "essai")), "--motif", "x"),
       motif = "ne peut être tests/reference/", env = EN_CI),
  list(args = c("--mode", "releve", "--plans", "tous", "--sortie", tempfile()), motif = "mode releve refusé sur la CI",
       env = EN_CI))) {
  verifier(sprintf("producteur : appel refusé (%s%s), code 1, sans rien écrire sous tests/reference/", cas$motif,
                   if (is.null(cas$env)) "" else sprintf(" ; mode %s en CI", cas$args[2L])), {
    contenu <- function() if (dir.exists(DOSSIER_REFERENCES))
      list.files(DOSSIER_REFERENCES, recursive = TRUE, all.files = TRUE, include.dirs = TRUE) else NA
    avant <- contenu()
    sortie_t <- gsub("^'|'$", "", cas$args[match("--sortie", cas$args) + 1L])
    r <- executer_rscript(PRODUCTEUR, cas$args, env = if (is.null(cas$env)) "GITHUB_ACTIONS=false" else cas$env)
    if (file.exists(sortie_t) && !startsWith(normalizePath(sortie_t), normalizePath(DOSSIER_REFERENCES))) {
      fichiers <- list.files(sortie_t, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
      unlink(sortie_t, recursive = TRUE)
      sprintf("le dossier de sortie a été créé (%s)", if (length(fichiers)) paste(fichiers, collapse = ", ") else "vide")
    }
    else if (r$statut != 1L) sprintf("code %d", r$statut)
    else if (!any(grepl(cas$motif, r$sortie, fixed = TRUE, useBytes = TRUE))) paste("sortie :", paste(r$sortie, collapse = " | "))
    else if (!identical(avant, contenu())) "tests/reference/ modifié" else TRUE
  })
}
# Garde de #36 : hors CI (variable fausse ou vide), chacun des trois modes qui
# produisent des références sort avec le code 1 et le message renvoyant à
# l'ADR 0009, avant toute écriture (le dossier de sortie n'est pas créé,
# tests/reference/ est intact), même avec des arguments par ailleurs valides.
for (mode_ci in c("creation", "regeneration", "bascule")) for (ga in c("GITHUB_ACTIONS=false", "GITHUB_ACTIONS=")) {
  verifier(sprintf("producteur : mode %s refusé hors CI (%s), code 1, message ADR 0009, rien écrit", mode_ci, ga), {
    sortie_t <- tempfile("sortie_hors_ci_")
    contenu <- function() if (dir.exists(DOSSIER_REFERENCES))
      list.files(DOSSIER_REFERENCES, recursive = TRUE, all.files = TRUE, include.dirs = TRUE) else NA
    avant <- contenu()
    r <- executer_rscript(PRODUCTEUR, c("--mode", mode_ci, "--plans", "tous", "--sortie", shQuote(sortie_t), "--motif", "x"), env = ga)
    if (r$statut != 1L) sprintf("code %d", r$statut)
    else if (!any(grepl(sprintf("mode %s refus", mode_ci), r$sortie, fixed = TRUE, useBytes = TRUE)) ||
             !any(grepl("hors CI", r$sortie, fixed = TRUE, useBytes = TRUE)) ||
             !any(grepl("ADR 0009", r$sortie, fixed = TRUE, useBytes = TRUE))) paste("sortie :", paste(r$sortie, collapse = " | "))
    else if (file.exists(sortie_t)) { unlink(sortie_t, recursive = TRUE); "le dossier de sortie a été créé" }
    else if (!identical(avant, contenu())) "tests/reference/ modifié" else TRUE
  })
}
# Un plan dont la construction échoue est consigné comme non produit : les
# autres plans sont produits et comparés, ecarts.txt est écrit, code 1.
# Copie de l'outillage (même arbre installable : bibliothèque de la batterie
# réutilisée) dont plans_reference() ne garde que R1 et un R2 invalide
# (niveau 2), sans toucher tests/reference/. GITHUB_STEP_SUMMARY est vidée pour
# le sous-processus (le producteur n'écrit le résumé que si elle est non vide) :
# le résumé du job CI ne doit pas recevoir l'échec du témoin ; posée ici sur un
# fichier, elle prouve que le sous-processus ne l'écrit pas.
verifier("producteur : plan dont la construction échoue -> non produit, autres plans produits, ecarts.txt écrit, code 1", local({
  d <- tempfile("producteur_temoin_"); dir.create(file.path(d, "tests", "reference"), recursive = TRUE)
  dir.create(file.path(d, "tests", "controles"))
  on.exit(unlink(d, recursive = TRUE))
  file.copy(file.path(RACINE, c("DESCRIPTION", "NAMESPACE", "LICENSE")), d)
  for (s in DOSSIERS_INSTALLES[dir.exists(file.path(RACINE, DOSSIERS_INSTALLES))])
    file.copy(file.path(RACINE, s), d, recursive = TRUE)
  file.copy(file.path(RACINE, "tests", "harnais.R"), file.path(d, "tests"))
  file.copy(file.path(RACINE, "tests", "controles", "plateforme.R"), file.path(d, "tests", "controles"))
  file.copy(file.path(DOSSIER_REFERENCES, c("produire_references.R", "outils_reference.R")),
            file.path(d, "tests", "reference"))
  cat("plans_reference <- function() list(",
      "R1 = list(identifiant = 'R1', description = 'temoin valide', plan = plan_reference()),",
      "R2 = list(identifiant = 'R2', description = 'temoin invalide', plan = plan_reference(niveau = 2)))",
      sep = "\n", file = file.path(d, "tests", "reference", "outils_reference.R"), append = TRUE)
  sortie_t <- file.path(d, "sortie")
  resume_t <- file.path(d, "resume_job.md")
  ancien_resume <- Sys.getenv("GITHUB_STEP_SUMMARY", unset = NA)
  Sys.setenv(GITHUB_STEP_SUMMARY = resume_t)
  on.exit(if (is.na(ancien_resume)) Sys.unsetenv("GITHUB_STEP_SUMMARY") else Sys.setenv(GITHUB_STEP_SUMMARY = ancien_resume), add = TRUE)
  r <- executer_rscript(file.path(d, "tests", "reference", "produire_references.R"),
                        c("--mode", "releve", "--plans", "tous", "--sortie", shQuote(sortie_t)),
                        env = c("GITHUB_ACTIONS=false", "GITHUB_STEP_SUMMARY="))
  ecarts <- file.path(sortie_t, "ecarts.txt")
  texte <- if (file.exists(ecarts)) rawToChar(readBin(ecarts, "raw", file.size(ecarts))) else ""
  if (r$statut != 1L) sprintf("code %d : %s", r$statut, paste(utils::tail(r$sortie, 3L), collapse = " | "))
  else if (!file.exists(ecarts)) "ecarts.txt absent"
  else if (file.exists(resume_t) && file.size(resume_t) > 0) "le sous-processus a écrit dans GITHUB_STEP_SUMMARY"
  else if (!grepl("R2 : construction en .+chec : ", texte, useBytes = TRUE)) "R2 non consigné comme non produit dans ecarts.txt"
  else if (!grepl("R2 : fichier commit.+ non ; comparaison non produit ; admis NON", texte, useBytes = TRUE)) "ligne de bilan de R2 absente"
  else if (!file.exists(file.path(sortie_t, "references", "R1.tsv"))) "R1 non produit"
  else if (file.exists(file.path(sortie_t, "references", "R2.tsv"))) "R2.tsv produit malgré l'échec"
  else if (!grepl("R1 : fichier commit.+ non ; comparaison sans fichier commit.+ ; admis oui", texte, useBytes = TRUE)) "R1 non comparé"
  else TRUE
}))
# Témoin de #28 : en LC_ALL=C, les libellés non ASCII du producteur ne sont pas
# abîmés dans ecarts.txt (« commit<c3><a9> »). Le plan témoin porte une
# description marquée UTF-8 (échappement \u), faute de quoi aucune chaîne
# marquée ne se mêle aux libellés et l'altération n'apparaîtrait pas.
verifier("producteur, sous-processus LC_ALL=C : libellés non ASCII d'ecarts.txt intacts (UTF-8, pas de <c3>)", local({
  d <- tempfile("producteur_c_"); dir.create(file.path(d, "tests", "reference"), recursive = TRUE)
  dir.create(file.path(d, "tests", "controles"))
  on.exit(unlink(d, recursive = TRUE))
  file.copy(file.path(RACINE, c("DESCRIPTION", "NAMESPACE", "LICENSE")), d)
  for (s in DOSSIERS_INSTALLES[dir.exists(file.path(RACINE, DOSSIERS_INSTALLES))])
    file.copy(file.path(RACINE, s), d, recursive = TRUE)
  file.copy(file.path(RACINE, "tests", "harnais.R"), file.path(d, "tests"))
  file.copy(file.path(RACINE, "tests", "controles", "plateforme.R"), file.path(d, "tests", "controles"))
  file.copy(file.path(DOSSIER_REFERENCES, c("produire_references.R", "outils_reference.R")),
            file.path(d, "tests", "reference"))
  cat("plans_reference <- function() list(",
      "R1 = list(identifiant = 'R1', description = 'd\\u00e9c\\u00e8s', plan = plan_reference()))",
      sep = "\n", file = file.path(d, "tests", "reference", "outils_reference.R"), append = TRUE)
  sortie_t <- file.path(d, "sortie")
  r <- executer_rscript(file.path(d, "tests", "reference", "produire_references.R"),
                        c("--mode", "releve", "--plans", "tous", "--sortie", shQuote(sortie_t)),
                        env = c("LC_ALL=C", "GITHUB_ACTIONS=false", "GITHUB_STEP_SUMMARY="))
  ecarts <- file.path(sortie_t, "ecarts.txt")
  octets <- if (file.exists(ecarts)) readBin(ecarts, "raw", file.size(ecarts)) else raw(0)
  texte <- rawToChar(octets)
  attendu <- c(charToRaw("d"), as.raw(c(0xc3, 0xa9)), charToRaw("c"), as.raw(c(0xc3, 0xa8)), charToRaw("s : aucun fichier commit"),
               as.raw(c(0xc3, 0xa9)), charToRaw(" (cr"), as.raw(c(0xc3, 0xa9)), charToRaw("ation)"))
  contient <- function(x, motif) {
    i <- seq_len(length(x) - length(motif) + 1L)
    any(vapply(i, function(k) identical(x[k:(k + length(motif) - 1L)], motif), NA))
  }
  if (r$statut != 0L) sprintf("code %d : %s", r$statut, paste(utils::tail(r$sortie, 3L), collapse = " | "))
  else if (!file.exists(ecarts)) "ecarts.txt absent"
  else if (grepl("<c3>", texte, fixed = TRUE, useBytes = TRUE)) "ecarts.txt contient <c3> (littéraux non marqués UTF-8)"
  else if (!validUTF8(texte)) "ecarts.txt n'est pas de l'UTF-8 valide"
  else if (!contient(octets, attendu)) "libellé « décès : aucun fichier commité (création) » absent ou altéré"
  else TRUE
}))
verifier("comparer_references.R : deux fichiers identiques -> code 0, verdict OK", {
  r <- executer_rscript(COMPARATEUR, c(shQuote(F_BASE), shQuote(F_BASE)))
  r$statut == 0L && any(grepl("Verdict : OK", r$sortie, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("Identifiant de plateforme : identique", r$sortie, fixed = TRUE, useBytes = TRUE))
})
verifier("comparer_references.R : référence altérée au-delà de la tolérance -> code 1, ligne d'écart imprimée", {
  f <- tempfile(fileext = ".tsv"); ecrire_reference(alterer(P_TAUX, 1 + 2e-14), f)
  r <- executer_rscript(COMPARATEUR, c(shQuote(F_BASE), shQuote(f))); unlink(f)
  r$statut == 1L && any(grepl(paste0(P_TAUX, " | 149 | 1 | "), r$sortie, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("Verdict : ECHEC", r$sortie, fixed = TRUE, useBytes = TRUE))
})
verifier("comparer_references.R : appel invalide -> code 2", executer_rscript(COMPARATEUR, F_BASE)$statut == 2L)
unlink(F_BASE)

# Chaînes et locale (ADR 0007, point 9.3 ; décision M54) : T1 aller-retour
# de chaînes non ASCII, T2 refus d'une chaîne non ASCII non marquée en
# locale C, T3 sonde de l'installation, T4 grandeurs de R1 identiques octet
# pour octet en LC_ALL=C et sous une locale UTF-8.
fichier_courant("références de non-régression (ADR 0009) : chaînes non ASCII et locale (T1 à T4)")

# LOCALE_UTF8 : locale UTF-8 disponible, sondée plus haut (empreinte, sous-processus).
# Code commun aux sous-processus : tablexp de la batterie, outillage chargé
# en UTF-8, chaînes construites depuis leurs octets (aucun littéral non
# ASCII dans le script, dont le marquage dépendrait de la locale).
code_chaines <- c(
  sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)),
  sprintf("for (e in parse(%s, encoding = 'UTF-8', keep.source = FALSE)) eval(e, globalenv())",
          deparse(file.path(RACINE, "tests", "reference", "outils_reference.R"))),
  "u8 <- function(o) { x <- rawToChar(as.raw(o)); Encoding(x) <- 'UTF-8'; x }",
  # « é », « à », « « », « » », et « « été » à\\t\"x\" » (échappements compris)
  "NON_ASCII <- c(u8(c(0xc3, 0xa9)), u8(c(0xc3, 0xa0)), u8(c(0xc2, 0xab)), u8(c(0xc2, 0xbb)),",
  "  u8(c(0xc2, 0xab, 0x20, 0xc3, 0xa9, 0x74, 0xc3, 0xa9, 0x20, 0xc2, 0xbb, 0x20, 0xc3, 0xa0, 0x09, 0x22, 0x78, 0x22, 0x5c)), NA)",
  "octets <- function(v) lapply(v, function(s) if (is.na(s)) NA else charToRaw(s))",
  "aller_retour <- function(v) {",
  "  x <- structure(list(fiche = list(m = v, f = factor(v[!is.na(v)]))), class = 'construction')",
  "  g <- aplatir_construction(x); f <- tempfile(); ecrire_reference(g, f); l <- lire_reference(f); unlink(f)",
  "  identical(octets(l$valeurs[['fiche/m']]), octets(v)) && identical(l$types, g$types) &&",
  "    identical(octets(l$valeurs[['fiche/f']]), octets(v[!is.na(v)])) &&",
  "    all(validUTF8(l$valeurs[['fiche/m']][!is.na(v)])) && all(Encoding(l$valeurs[['fiche/m']][!is.na(v)]) == 'UTF-8')",
  "}")
eval(parse(text = code_chaines[-(1:2)]))
verifier("T1 : aller-retour au bit près de chaînes non ASCII marquées UTF-8 (é, à, « , », échappements)",
         aller_retour(NON_ASCII))
if (isTRUE(l10n_info()[["UTF-8"]])) {
  verifier("T1 : même aller-retour, chaînes non marquées (session UTF-8)", {
    v <- NON_ASCII; Encoding(v) <- "unknown"; aller_retour(v)
  })
} else cat("  [sauté] T1, chaînes non marquées : session hors UTF-8 (le cas relève de T2)\n")
script <- tempfile(fileext = ".R")
writeLines(c(code_chaines,
  "cat('T1', aller_retour(NON_ASCII), '\\n')",
  "v <- NON_ASCII[1L]; Encoding(v) <- 'unknown'",
  "r <- tryCatch({ atome(v, 'fiche/m'); 'aucun refus' }, refus_reference = function(e) e$chemin)",
  "cat('T2', identical(r, 'fiche/m'), '\\n')",
  "cat('T2t', identical(atome(NON_ASCII[1L], 'fiche/m')$type, 'character'), '\\n')",
  "cat('LOCALE', Sys.getlocale('LC_CTYPE'), '\\n')"), script)
r_c <- executer_rscript(script, env = "LC_ALL=C")
unlink(script)
ligne <- function(r, cle) sub(paste0("^", cle, " "), "", grep(paste0("^", cle, " "), r$sortie, value = TRUE))
verifier("T1, sous-processus LC_ALL=C : aller-retour au bit près des chaînes marquées UTF-8",
         if (r_c$statut != 0L) paste(r_c$sortie, collapse = " | ")
         else identical(trimws(ligne(r_c, "LOCALE")), "C") && identical(trimws(ligne(r_c, "T1")), "TRUE"))
verifier("T2, sous-processus LC_ALL=C : chaîne non ASCII non marquée -> refus_reference au chemin fautif",
         identical(trimws(ligne(r_c, "T2")), "TRUE"))
verifier("T2, témoin : la même chaîne marquée UTF-8 passe", identical(trimws(ligne(r_c, "T2t")), "TRUE"))

# T3 : sonde de l'installation de la batterie ; témoin : installation forcée
# en LC_ALL=C, sonde lue dans une session LC_ALL=C (une session UTF-8 ne
# convertit rien et la sonde y passerait, mesuré).
verifier("T3 : sonde UTF-8 de l'installation de la batterie (STATUT_EMPREINTES_NON_CALCULEES)", isTRUE(sonde_utf8()))
t3 <- system.time({
  lib_c <- tempfile("tablexp_lib_C_"); dir.create(lib_c)
  # LC_ALL posée dans ce processus le temps de l'installation : sous Windows,
  # system2(env =) n'est pas pris en charge (la variable n'atteint pas
  # R CMD INSTALL, qui installe alors en UTF-8 et la sonde passe : mesuré),
  # comme dans executer_rscript().
  ancien_lc_all <- Sys.getenv("LC_ALL", unset = NA)
  Sys.setenv(LC_ALL = "C")
  inst <- suppressWarnings(system2(file.path(R.home("bin"), "R"),
                                   c("CMD", "INSTALL", "--no-docs", "-l", shQuote(lib_c), shQuote(RACINE)),
                                   stdout = TRUE, stderr = TRUE))
  if (is.na(ancien_lc_all)) Sys.unsetenv("LC_ALL") else Sys.setenv(LC_ALL = ancien_lc_all)
  script <- tempfile(fileext = ".R")
  writeLines(c("h <- new.env()", sprintf("sys.source(%s, envir = h)", deparse(file.path(RACINE, "tests", "harnais.R"))),
               sprintf("library(tablexp, lib.loc = %s)", deparse(lib_c)),
               "cat('SONDE', h$sonde_utf8(), '\\n')"), script)
  r3 <- executer_rscript(script, env = "LC_ALL=C")
  unlink(c(script, lib_c), recursive = TRUE)
})[["elapsed"]]
cat(sprintf("  T3, témoin : installation en LC_ALL=C et sonde, %.1f s\n", t3))
verifier("T3, témoin : installation forcée en LC_ALL=C -> sonde en échec (session LC_ALL=C)",
         if (!is.null(attr(inst, "status"))) paste(inst, collapse = " | ")
         else identical(trimws(ligne(r3, "SONDE")), "FALSE"))

# T4 : R1 aplati et écrit en LC_ALL=C et sous une locale UTF-8, sur la
# bibliothèque de la batterie : corps identiques octet pour octet.
if (nzchar(LOCALE_UTF8)) {
  sorties <- c(C = tempfile(fileext = ".tsv"), U = tempfile(fileext = ".tsv"))
  script <- tempfile(fileext = ".R")
  writeLines(c(code_chaines[1:2], "j <- jeu_reference()",
               "x <- construire_table(j$donnees, j$roles, plans_reference()$R1$plan)",
               "ecrire_reference(aplatir_construction(x), commandArgs(TRUE)[1L], c(identifiant = 'R1'))"), script)
  t4 <- system.time({
    r4 <- list(C = executer_rscript(script, sorties[["C"]], env = "LC_ALL=C"),
               U = executer_rscript(script, sorties[["U"]], env = paste0("LC_ALL=", LOCALE_UTF8)))
  })[["elapsed"]]
  cat(sprintf("  T4 : R1 en LC_ALL=C et LC_ALL=%s, %.1f s\n", LOCALE_UTF8, t4))
  verifier(sprintf("T4 : R1 aplati en LC_ALL=C et en LC_ALL=%s : fichiers identiques octet pour octet", LOCALE_UTF8), {
    if (r4$C$statut != 0L || r4$U$statut != 0L) paste(c(r4$C$sortie, r4$U$sortie), collapse = " | ")
    else identical(readBin(sorties[["C"]], "raw", file.info(sorties[["C"]])$size),
                   readBin(sorties[["U"]], "raw", file.info(sorties[["U"]])$size))
  })
  unlink(c(script, sorties))
} else cat("  [sauté] T4 : aucune locale UTF-8 disponible parmi", paste(LOCALES_UTF8, collapse = ", "), "\n")

# Mécanisme générique des transitions (ADR 0009, point 9.5 ; #47) : témoins
# sur des objets de référence en mémoire (rien n'est écrit sous
# tests/reference/). `nouveau` est ce que produit le moteur ; chaque témoin en
# tire la référence « commitée » (état d'avant) en défaisant le seul
# changement qu'il teste. Deux témoins rejouent des cas réels : celui de #37
# sur R1 recalculé (aplatis), remis en double en mémoire, et le constat de la
# permutation (g_permute_r6, construit plus haut) pour la garde 3.2.
fichier_courant("références de non-régression (ADR 0009, point 9.5) : mécanisme générique des transitions, témoins (#47)")

verifier("transitions en cours : liste bien formée (champs, issues, plans de référence, natures)", {
  p <- valider_transitions(TRANSITIONS_EN_COURS)
  if (length(p)) p else TRUE
})

local({
  nouveau <- list(
    types = c("fiche/empreintes/donnees" = "character", "fiche/version" = "character", "s/taux" = "double",
              "s/cellules" = "integer", "s/n" = "integer", "s/nouveau" = "integer"),
    valeurs = list("fiche/empreintes/donnees" = strrep("a", 32L), "fiche/version" = "v2", "s/taux" = c(0.1, 0.2, 0.3),
                   "s/cellules" = c(149L, 75L), "s/n" = c(1L, 2L, 3L), "s/nouveau" = 7L))
  # États d'avant, un par nature.
  def_type <- function(r) { r$types[["s/cellules"]] <- "double"; r$valeurs[["s/cellules"]] <- c(149, 75); r }
  def_valeur <- function(r) { r$valeurs[["s/taux"]][3L] <- 0.35; r }
  def_chaine <- function(r) { r$valeurs[["fiche/version"]] <- "v1"; r }
  def_ajout <- function(r) { r$types <- r$types[names(r$types) != "s/nouveau"]; r$valeurs <- r$valeurs[names(r$valeurs) != "s/nouveau"]; r }
  def_retrait <- function(r) { r$types[["s/ancien"]] <- "integer"; r$valeurs[["s/ancien"]] <- 5L; r }
  derive <- function(r) { r$valeurs[["s/n"]][2L] <- 9L; r }  # dérive non déclarée
  ch <- list(
    type = list(chemin = "s/cellules", nature = "type", de = "double", vers = "integer"),
    valeur = list(chemin = "s/taux", nature = "valeur", valeurs = 0.3, positions = 3L),
    "valeur de chaîne" = list(chemin = "fiche/version", nature = "valeur de chaîne", valeurs = "v2"),
    ajout = list(chemin = "s/nouveau", nature = "ajout", type = "integer", longueur = 1L),
    retrait = list(chemin = "s/ancien", nature = "retrait", type = "integer", longueur = 1L))
  avant <- list(type = def_type, valeur = def_valeur, "valeur de chaîne" = def_chaine, ajout = def_ajout, retrait = def_retrait)
  strict <- function(ref, g) { r <- comparer_references(ref, g); if (r$ok) TRUE else "comparaison en échec" }
  fixe <- function(...) list(...)
  # Statuts enregistrés par un appel du harnais, sans les conserver au bilan.
  lignes_de <- function(expr) {
    anciennes <- .harnais$lignes; fichier <- .harnais$fichier
    on.exit({ .harnais$lignes <- anciennes; .harnais$fichier <- fichier })
    utils::capture.output(expr)
    .harnais$lignes[seq_len(nrow(.harnais$lignes)) > nrow(anciennes), , drop = FALSE]
  }
  tr <- function(issue, plans, ...) list(issue = issue, plans = plans, changements = list(...))

  for (n in NATURES_TRANSITION) {
    old <- avant[[n]](nouveau)
    verifier(sprintf("témoin 9.5, nature « %s » : la comparaison stricte échoue, le seul écart déclaré passe", n),
             !comparer_references(old, nouveau)$ok && isTRUE(seul_ecart(old, nouveau, fixe(ch[[n]]))))
    verifier(sprintf("témoin 9.5, nature « %s » : un écart non déclaré en plus échoue", n),
             is.character(seul_ecart(derive(old), nouveau, fixe(ch[[n]]))))
    verifier(sprintf("témoin 9.5, nature « %s » : le changement non déclaré échoue", n),
             is.character(seul_ecart(old, nouveau, list())))
  }
  mauvais <- function(old, changement, motif) {
    r <- seul_ecart(old, nouveau, fixe(changement))
    if (is.character(r) && grepl(motif, r, fixed = TRUE, useBytes = TRUE)) TRUE else sprintf("résultat : %s", paste(r, collapse = " "))
  }
  verifier("témoin 9.5, valeur : valeur déclarée différente de celle de l'objet -> échec",
           is.character(seul_ecart(def_valeur(nouveau), nouveau, fixe(modifyList(ch$valeur, list(valeurs = 0.31))))))
  verifier("témoin 9.5, valeur de chaîne : chaîne déclarée différente de celle de l'objet -> échec",
           is.character(seul_ecart(def_chaine(nouveau), nouveau, fixe(modifyList(ch[["valeur de chaîne"]], list(valeurs = "v3"))))))
  verifier("témoin 9.5, type : conversion avec perte (valeurs non entières) refusée",
           mauvais({ o <- def_type(nouveau); o$valeurs[["s/cellules"]][1L] <- 149.5; o }, ch$type, "avec perte"))
  verifier("témoin 9.5, type : type de la référence autre que le type déclaré refusé",
           mauvais(def_type(nouveau), modifyList(ch$type, list(chemin = "fiche/version")), "type character de la référence"))
  verifier("témoin 9.5, type : chemin absent de la référence refusé",
           mauvais(def_type(nouveau), modifyList(ch$type, list(chemin = "s/inconnu")), "absent de la référence"))
  verifier("témoin 9.5, valeur : positions hors du vecteur refusées",
           mauvais(def_valeur(nouveau), modifyList(ch$valeur, list(positions = 4L)), "valeur(s) déclarée(s)"))
  verifier("témoin 9.5, valeur : nature « valeur » sur un chemin character refusée",
           mauvais(def_chaine(nouveau), modifyList(ch$valeur, list(chemin = "fiche/version")), "incompatible"))
  verifier("témoin 9.5, ajout : chemin absent de l'objet refusé",
           mauvais(def_ajout(nouveau), modifyList(ch$ajout, list(chemin = "s/inconnu")), "absent de l'objet"))
  verifier("témoin 9.5, retrait : chemin encore présent dans l'objet refusé",
           mauvais(def_ajout(nouveau), modifyList(ch$retrait, list(chemin = "s/n")), "encore présent"))

  # Plusieurs transitions simultanées sur un même plan : union.
  ref_tout <- def_retrait(def_ajout(def_chaine(def_valeur(def_type(nouveau)))))
  t_a <- tr("#901", c("R1", "R2"), ch$type, ch$valeur)
  t_b <- tr("#902", "R1", ch[["valeur de chaîne"]], ch$ajout, ch$retrait)
  verifier("témoin 9.5, union : deux transitions (cinq natures, issues différentes) sur un même plan -> seul écart", {
    u <- changements_du_plan("R1", list(t_a, t_b))
    length(u$changements) == 5L && identical(u$issues, c("#901", "#902")) &&
      isTRUE(seul_ecart(ref_tout, nouveau, u$changements)) && !comparer_references(ref_tout, nouveau)$ok
  })
  verifier("témoin 9.5, union : la transition sur un autre plan ne s'ajoute pas ; chaque transition est nécessaire", {
    u2 <- changements_du_plan("R2", list(t_a, t_b))$changements
    length(u2) == 2L && is.character(seul_ecart(ref_tout, nouveau, u2)) &&
      is.character(seul_ecart(ref_tout, nouveau, changements_du_plan("R1", list(t_a))$changements))
  })
  # Même chemin : positions disjointes (deux issues), natures différentes
  # (type puis valeur) dans l'un ou l'autre ordre, conflit, idempotence.
  n2 <- nouveau; n2$valeurs[["s/cellules"]] <- c(149L, 80L); n2$valeurs[["s/taux"]] <- c(0.15, 0.2, 0.3)
  old2 <- def_type(n2); old2$valeurs[["s/taux"]][1L] <- 0.1; old2$valeurs[["s/taux"]][3L] <- 0.35
  v1 <- list(chemin = "s/taux", nature = "valeur", valeurs = 0.15, positions = 1L)
  v3 <- list(chemin = "s/taux", nature = "valeur", valeurs = 0.3, positions = 3L)
  vc <- list(chemin = "s/cellules", nature = "valeur", valeurs = 80L, positions = 2L)
  old2$valeurs[["s/cellules"]] <- c(149, 75)
  verifier("témoin 9.5, union sur un même chemin : positions disjointes de deux issues -> seul écart", {
    u <- changements_du_plan("R1", list(tr("#903", "R1", v1, ch$type), tr("#904", "R1", v3, vc)))$changements
    isTRUE(seul_ecart(old2, n2, u)) && is.character(seul_ecart(old2, n2, fixe(v1, ch$type, vc)))
  })
  verifier("témoin 9.5, union sur un même chemin : type et valeur du même chemin, dans l'un ou l'autre ordre", {
    isTRUE(seul_ecart(old2, n2, fixe(v1, v3, ch$type, vc))) && isTRUE(seul_ecart(old2, n2, fixe(vc, v3, v1, ch$type)))
  })
  verifier("témoin 9.5, union sur un même chemin : deux valeurs différentes à une même position -> conflit", {
    r <- seul_ecart(old2, n2, fixe(ch$type, vc, v1, v3, modifyList(v3, list(valeurs = 0.31))))
    is.character(r) && grepl("deux transitions", r, fixed = TRUE, useBytes = TRUE)
  })
  verifier("témoin 9.5, union sur un même chemin : même valeur déclarée deux fois (deux issues) -> sans conflit",
           isTRUE(seul_ecart(old2, n2, fixe(ch$type, ch$type, vc, vc, v1, v3, v3))))

  # Branchement par plan : plan listé, plan resté listé après la
  # régénération (succès inattendu), plan non listé.
  statuts <- function(id, ref, transitions, g = nouveau, donnees = NULL, roles = NULL) {
    l <- lignes_de(controler_plan(id, sprintf("%s : comparaison stricte", id), ref, g, strict(ref, g), transitions, donnees, roles))
    stats::setNames(l$statut, NULL)
  }
  verifier("témoin 9.5, plan listé : test « seul écart » réussi et comparaison stricte en échec attendu renvoyant à l'issue", {
    l <- lignes_de(controler_plan("R1", "R1 : stricte", def_type(nouveau), nouveau, strict(def_type(nouveau), nouveau), list(tr("#901", "R1", ch$type))))
    identical(l$statut, c("OK", "ATTENDU")) && grepl("{#901}", l$nom[2L], fixed = TRUE, useBytes = TRUE) &&
      grepl("#901", l$nom[1L], fixed = TRUE, useBytes = TRUE)
  })
  verifier("témoin 9.5, plans de deux issues : l'échec attendu renvoie à la première issue de la liste",
           grepl("{#901}", lignes_de(controler_plan("R1", "R1 : stricte", ref_tout, nouveau, strict(ref_tout, nouveau), list(t_a, t_b)))$nom[2L],
                 fixed = TRUE, useBytes = TRUE))
  verifier("témoin 9.5, plan resté listé après la régénération : « seul écart » en échec (changement non observé) et succès inattendu (la batterie échouerait)",
           identical(statuts("R1", nouveau, list(t_a)), c("ECHEC", "INATTENDU")))
  verifier("témoin 9.5, plan listé et écart non déclaré : « seul écart » en échec, comparaison stricte en échec attendu",
           identical(statuts("R1", derive(def_type(nouveau)), list(tr("#901", "R1", ch$type))), c("ECHEC", "ATTENDU")))
  verifier("témoin 9.5, plan non listé : test ordinaire, en échec sur un écart (R2 absent de la transition de R1)",
           identical(statuts("R2", def_type(nouveau), list(tr("#901", "R1", ch$type))), "ECHEC"))
  verifier("témoin 9.5, plan non listé : test ordinaire, réussi sans écart",
           identical(statuts("R2", nouveau, list(tr("#901", "R1", ch$type))), "OK") && identical(statuts("R1", nouveau, list()), "OK"))

  # Mal formé : chaque défaut de forme est relevé.
  bien <- list(tr("#901", "R1", ch$type, ch$valeur, ch[["valeur de chaîne"]], ch$ajout, ch$retrait))
  verifier("témoin 9.5, forme : une transition bien formée et la liste vide sont acceptées",
           !length(valider_transitions(bien)) && !length(valider_transitions(list())))
  for (cas in list(
    list("nature inconnue", list(tr("#901", "R1", list(chemin = "s/n", nature = "quelconque")))),
    list("issue sans #", list(tr("37", "R1", ch$ajout))),
    list("issue en double", list(tr("#901", "R1", ch$ajout), tr("#901", "R2", ch$retrait))),
    list("plan de référence inconnu", list(tr("#901", "R9", ch$ajout))),
    list("aucun changement", list(tr("#901", "R1"))),
    list("champ hors de la nature (valeurs sur un type)", list(tr("#901", "R1", modifyList(ch$type, list(valeurs = 1))))),
    list("conversion de type hors double / integer", list(tr("#901", "R1", modifyList(ch$type, list(vers = "character"))))),
    list("valeurs sans positions de même longueur", list(tr("#901", "R1", modifyList(ch$valeur, list(positions = 1:2))))),
    list("positions dupliquées", list(tr("#901", "R1", modifyList(ch$valeur, list(valeurs = c(1, 2), positions = c(1L, 1L)))))),
    list("valeur de chaîne numérique", list(tr("#901", "R1", modifyList(ch[["valeur de chaîne"]], list(valeurs = 1)))))))
    verifier(sprintf("témoin 9.5, forme : %s -> relevé par valider_transitions()", cas[[1L]]), length(valider_transitions(cas[[2L]])) > 0L)

  # Doubles sans tolérance déclarée des références à régénérer.
  verifier("témoin 9.5, restes : type converti depuis double et retrait de type double admis ; retrait d'un autre type, valeur, ajout et autre plan non admis", {
    restes <- list(R1 = c("s/cellules", "s/vieux", "s/ancien", "s/taux"), R2 = "s/cellules")
    vieux <- list(chemin = "s/vieux", nature = "retrait", type = "double", longueur = 1L)
    r <- restes_hors_transitions(restes, list(tr("#901", "R1", ch$type, vieux, ch$retrait, ch$valeur, ch$ajout)))
    identical(r$R1, c("s/ancien", "s/taux")) && identical(r$R2, "s/cellules") &&
      identical(issue_des_restes(list(tr("#902", "R1", ch$valeur), tr("#901", "R1", ch$type))), "#901")
  })

  # Changement de longueur : « valeur » sans positions remplace tout le vecteur.
  n3 <- nouveau; n3$valeurs[["s/n"]] <- c(1L, 2L)
  v_entier <- list(chemin = "s/n", nature = "valeur", valeurs = c(1L, 2L))
  verifier("témoin 9.5, longueur : vecteur entier de longueur 3 -> 2 déclaré sans positions -> seul écart ; sans déclaration -> échec", {
    isTRUE(seul_ecart(nouveau, n3, fixe(v_entier))) && is.character(seul_ecart(nouveau, n3, list())) &&
      is.character(seul_ecart(nouveau, n3, fixe(modifyList(v_entier, list(valeurs = c(1L, 3L))))))
  })
  verifier("témoin 9.5, longueur : longueur changée avec positions -> refusée", {
    r <- seul_ecart(nouveau, n3, fixe(modifyList(v_entier, list(positions = 1:3))))
    r2 <- seul_ecart(nouveau, n3, fixe(modifyList(v_entier, list(positions = 1:2))))
    is.character(r) && is.character(r2)
  })
  verifier("témoin 9.5, longueur : chaîne, 3 -> 1 élément sans positions -> seul écart", {
    a <- nouveau; a$valeurs[["fiche/version"]] <- c("v2", "v3", "v4")
    isTRUE(seul_ecart(a, nouveau, fixe(list(chemin = "fiche/version", nature = "valeur de chaîne", valeurs = "v2"))))
  })

  # Ajout et retrait : type et longueur déclarés, vérifiés.
  for (cas in list(
    list("ajout, type faux", def_ajout(nouveau), modifyList(ch$ajout, list(type = "double"))),
    list("ajout, longueur fausse", def_ajout(nouveau), modifyList(ch$ajout, list(longueur = 2L))),
    list("retrait, type faux", def_retrait(nouveau), modifyList(ch$retrait, list(type = "double"))),
    list("retrait, longueur fausse", def_retrait(nouveau), modifyList(ch$retrait, list(longueur = 3L)))))
    verifier(sprintf("témoin 9.5, %s -> refusé", cas[[1L]]), is.character(seul_ecart(cas[[2L]], nouveau, fixe(cas[[3L]]))))

  # Règle « observé » : jugée sur la référence lue, avant alignement.
  non_observe <- function(old, changements) {
    r <- seul_ecart(old, nouveau, changements)
    if (is.character(r) && grepl("changement déclaré non observé", r, fixed = TRUE, useBytes = TRUE)) TRUE else sprintf("résultat : %s", paste(r, collapse = " "))
  }
  verifier("témoin 9.5, observé : retrait d'un chemin absent des deux côtés -> refusé", non_observe(nouveau, fixe(ch$retrait)))
  verifier("témoin 9.5, observé : ajout d'un chemin déjà présent dans la référence -> refusé", non_observe(nouveau, fixe(ch$ajout)))
  verifier("témoin 9.5, observé : type déjà converti dans la référence -> refusé", non_observe(nouveau, fixe(ch$type)))
  verifier("témoin 9.5, observé : valeur déclarée égale à celle de la référence (une position, vecteur entier) -> refusé",
           isTRUE(non_observe(nouveau, fixe(ch$valeur))) && isTRUE(non_observe(nouveau, fixe(ch[["valeur de chaîne"]]))) &&
             isTRUE(non_observe(nouveau, fixe(modifyList(ch$valeur, list(positions = NULL, valeurs = c(0.1, 0.2, 0.3)))))))
  verifier("témoin 9.5, observé : une position déclarée sur deux égale à la référence -> refusé",
           isTRUE(non_observe(def_valeur(nouveau), fixe(modifyList(ch$valeur, list(positions = c(1L, 3L), valeurs = c(0.1, 0.3)))))))
  verifier("témoin 9.5, observé : un seul des changements d'une union non observé -> refusé",
           isTRUE(non_observe(def_valeur(nouveau), fixe(ch$valeur, ch$type))))

  # Union : déclarations contradictoires.
  verifier("témoin 9.5, union : vecteur entier et autre déclaration de valeurs sur le même chemin -> erreur", {
    r <- seul_ecart(def_valeur(nouveau), nouveau, fixe(modifyList(ch$valeur, list(positions = NULL, valeurs = c(0.1, 0.2, 0.3))), ch$valeur))
    is.character(r) && grepl("vecteur entier", r, fixed = TRUE, useBytes = TRUE)
  })
  verifier("témoin 9.5, union : deux conversions de type différentes sur le même chemin -> erreur", {
    r <- seul_ecart(def_type(nouveau), nouveau, fixe(ch$type, modifyList(ch$type, list(de = "integer", vers = "double"))))
    is.character(r) && grepl("deux conversions", r, fixed = TRUE, useBytes = TRUE)
  })
  verifier("témoin 9.5, union : ajout ou retrait combiné à une autre nature sur le même chemin -> erreur", {
    a <- seul_ecart(def_ajout(nouveau), nouveau, fixe(ch$ajout, modifyList(ch$valeur, list(chemin = "s/nouveau"))))
    b <- seul_ecart(def_retrait(nouveau), nouveau, fixe(ch$retrait, ch$retrait, modifyList(ch$type, list(chemin = "s/ancien"))))
    is.character(a) && grepl("ajout ou retrait combiné", a, fixed = TRUE, useBytes = TRUE) &&
      is.character(b) && grepl("ajout ou retrait combiné", b, fixed = TRUE, useBytes = TRUE)
  })
  verifier("témoin 9.5, union : deux déclarations identiques (ajout, retrait, type) s'appliquent une fois", {
    isTRUE(seul_ecart(def_retrait(def_ajout(def_type(nouveau))), nouveau, fixe(ch$ajout, ch$retrait, ch$type, ch$ajout, ch$retrait, ch$type)))
  })

  # Forme : champs de l'ajout et du retrait ; empreinte des données.
  for (cas in list(
    list("ajout sans type ni longueur", list(chemin = "s/nouveau", nature = "ajout")),
    list("retrait de type hors du format", modifyList(ch$retrait, list(type = "complexe"))),
    list("retrait de longueur négative", modifyList(ch$retrait, list(longueur = -1))),
    list("empreinte des données en nature « valeur »", list(chemin = CHEMIN_EMPREINTE_DONNEES, nature = "valeur", valeurs = 1)),
    list("empreinte des données avec positions", list(chemin = CHEMIN_EMPREINTE_DONNEES, nature = "valeur de chaîne", valeurs = "a", positions = 1L)),
    list("empreinte des données retirée", list(chemin = CHEMIN_EMPREINTE_DONNEES, nature = "retrait", type = "character", longueur = 1L))))
    verifier(sprintf("témoin 9.5, forme : %s -> relevé par valider_transitions()", cas[[1L]]),
             length(valider_transitions(list(tr("#901", "R1", cas[[2L]])))) > 0L)
  verifier("témoin 9.5, forme : l'empreinte des données en « valeur de chaîne » sans positions est acceptée",
           !length(valider_transitions(list(tr("#901", "R1", list(chemin = CHEMIN_EMPREINTE_DONNEES, nature = "valeur de chaîne", valeurs = "a"))))))

  # Garde 3.2 : l'entrée est inchangée sous la représentation de la référence.
  d <- data.frame(c = c("deces", "en_cours", NA), e = as.Date(c("2020-03-01", "2016-05-20", "2024-02-29")),
                  it = c(1L, 2L, 3L), stringsAsFactors = FALSE)
  rl <- c(cause_sortie = "c", date_entree = "e", id_tete = "it")
  rep_g <- tablexp:::ENTETE_CANONIQUE; emp_g <- tablexp:::empreinte_donnees(d, rl)
  # Empreinte de la référence, calculée ici : texte canonique complet, en-tête
  # remplacé par une chaîne, MD5 du fichier.
  f_t <- tempfile(); tablexp:::texte_canonique(d, rl, f_t)
  texte <- rawToChar(readBin(f_t, "raw", file.info(f_t)$size)); unlink(f_t)
  rep_r <- "tablexp canonique 0"
  stopifnot(startsWith(texte, rep_g))
  writeBin(charToRaw(paste0(rep_r, substring(texte, nchar(rep_g) + 1L))), f_t)
  emp_r <- unname(tools::md5sum(f_t)); unlink(f_t)
  objet_emp <- function(rep, emp) list(types = stats::setNames(c("character", "character"), c(CHEMIN_EMPREINTE_DONNEES, CHEMIN_REPRESENTATION)),
                                       valeurs = stats::setNames(list(emp, rep), c(CHEMIN_EMPREINTE_DONNEES, CHEMIN_REPRESENTATION)))
  g_s <- objet_emp(rep_g, emp_g); ref_s <- objet_emp(rep_r, emp_r)
  ch_emp <- list(chemin = CHEMIN_EMPREINTE_DONNEES, nature = "valeur de chaîne", valeurs = emp_g)
  ch_rep <- list(chemin = CHEMIN_REPRESENTATION, nature = "valeur de chaîne", valeurs = rep_g)
  t_e <- tr("#905", "R1", ch_emp, ch_rep)
  verifier("témoin 9.5, garde 3.2 (a) : entrée inchangée sous la représentation de la référence -> TRUE ; plan complet : seul écart, garde, échec attendu",
           isTRUE(entree_inchangee(ref_s, g_s, d, rl)) && identical(statuts("R1", ref_s, list(t_e), g_s, d, rl), c("OK", "OK", "ATTENDU")))
  verifier("témoin 9.5, garde 3.2 (b) : une valeur de l'entrée altérée (objet construit sur elle) -> échec (raison citant l'empreinte) ; plan complet : garde en échec", {
    d2 <- d; d2$it[2L] <- 9L
    g_s2 <- objet_emp(rep_g, tablexp:::empreinte_donnees(d2, rl))
    t_e2 <- tr("#905", "R1", modifyList(ch_emp, list(valeurs = g_s2$valeurs[[CHEMIN_EMPREINTE_DONNEES]])), ch_rep)
    r <- entree_inchangee(ref_s, g_s2, d2, rl)
    is.character(r) && grepl("l'entrée a changé", r, fixed = TRUE, useBytes = TRUE) &&
      identical(statuts("R1", ref_s, list(t_e2), g_s2, d2, rl), c("OK", "ECHEC", "ATTENDU"))
  })
  verifier("témoin 9.5, garde 3.2 (c) : même représentation des deux côtés -> échec ; plan complet : garde en échec", {
    ref_c <- objet_emp(rep_g, emp_r)
    r <- entree_inchangee(ref_c, g_s, d, rl)
    is.character(r) && grepl("identique des deux côtés", r, fixed = TRUE, useBytes = TRUE) &&
      identical(statuts("R1", ref_c, list(tr("#905", "R1", ch_emp)), g_s, d, rl), c("OK", "ECHEC", "ATTENDU"))
  })
  verifier("témoin 9.5, garde 3.2 (d) : empreinte déclarée sans changement de représentation déclaré -> garde en échec, citant la représentation", {
    # Représentation identique des deux côtés : « seul écart » réussit (seule l'empreinte diffère), la garde échoue.
    ref_d <- objet_emp(rep_g, strrep("0", 32L))
    l <- lignes_de(controler_plan("R1", "R1 : stricte", ref_d, g_s, strict(ref_d, g_s), list(tr("#905", "R1", ch_emp)), d, rl))
    identical(l$statut, c("OK", "ECHEC", "ATTENDU")) && grepl(CHEMIN_REPRESENTATION, l$message[2L], fixed = TRUE, useBytes = TRUE) &&
      grepl("non déclaré", l$message[2L], fixed = TRUE, useBytes = TRUE)
  })
  verifier("témoin 9.5, garde 3.2 (e) : constat de la permutation rejoué sur R6 permuté (déjà construit) : les trois chaînes déclarées -> seul écart réussi, garde en échec", {
    g0 <- aplatis[["R6"]]
    chaines <- c(CHEMIN_EMPREINTE_DONNEES, "etapes/donnees/resultat/lignes_conservees/@empreinte",
                 "etapes/donnees/resultat/observations/@empreinte")
    ch3 <- lapply(chaines, function(c_) list(chemin = c_, nature = "valeur de chaîne", valeurs = g_permute_r6$valeurs[[c_]]))
    l <- lignes_de(controler_plan("R6", "R6 : stricte", g0, g_permute_r6, strict(g0, g_permute_r6), list(tr("#905", "R6", ch3[[1L]], ch3[[2L]], ch3[[3L]])),
                                  jeu_ref$donnees, jeu_ref$roles))
    identical(l$statut[1:2], c("OK", "ECHEC")) &&
      is.character(entree_inchangee(g0, g_permute_r6, jeu_ref$donnees, jeu_ref$roles))
  })
  verifier("témoin 9.5, garde 3.2 (f) : objet construit sur une autre entrée que celle passée (réserve M2) -> échec nommant les deux MD5, dans les deux sens ; plan complet : garde en échec", {
    d2 <- d; d2$it[2L] <- 9L
    g_s2 <- objet_emp(rep_g, tablexp:::empreinte_donnees(d2, rl))
    r1 <- entree_inchangee(ref_s, g_s, d2, rl)   # objet construit sur d, entrée passée d2
    r2 <- entree_inchangee(ref_s, g_s2, d, rl)   # objet construit sur d2, entrée passée d
    motif <- "objet non construit sur l'entrée passée"
    is.character(r1) && is.character(r2) && grepl(motif, r1, fixed = TRUE, useBytes = TRUE) && grepl(motif, r2, fixed = TRUE, useBytes = TRUE) &&
      grepl(emp_g, r1, fixed = TRUE, useBytes = TRUE) && grepl(g_s2$valeurs[[CHEMIN_EMPREINTE_DONNEES]], r1, fixed = TRUE, useBytes = TRUE) &&
      identical(statuts("R1", ref_s, list(t_e), g_s, d2, rl), c("OK", "ECHEC", "ATTENDU"))
  })
  verifier("témoin 9.5, garde 3.2 : le texte canonique de l'entrée est calculé une seule fois pour une même entrée (cache)", {
    entree_inchangee(ref_s, g_s, d, rl)
    o <- CACHE_ENTREE$octets; m <- CACHE_ENTREE$md5
    entree_inchangee(ref_s, g_s, d, rl)
    f <- tempfile(); writeBin(o, f); m_o <- unname(tools::md5sum(f)); unlink(f)
    identical(o, CACHE_ENTREE$octets) && identical(m, CACHE_ENTREE$md5) && identical(CACHE_ENTREE$donnees, d) && identical(m, m_o)
  })

  # Impression de la troisième cause d'une entrée différente.
  verifier("témoin 9.5, impression : représentation différente -> troisième cause nommée avec les deux valeurs ; représentation identique ou empreinte égale -> absente", {
    cause <- function(r) any(grepl("la représentation canonique a changé", imprimer_comparaison(r), fixed = TRUE, useBytes = TRUE))
    r1 <- comparer_references(ref_s, g_s)
    l1 <- imprimer_comparaison(r1)
    r2 <- comparer_references(objet_emp(rep_g, emp_r), g_s)
    r3 <- comparer_references(objet_emp(rep_r, emp_g), objet_emp(rep_g, emp_g))
    identical(r1$entree$representation_reference, rep_r) && identical(r1$entree$representation_obtenue, rep_g) && cause(r1) &&
      any(grepl(sprintf("« %s » (référence) contre « %s » (obtenu)", rep_r, rep_g), l1, fixed = TRUE, useBytes = TRUE)) &&
      !cause(r2) && !cause(r3)
  })

  # Harnais : lignes_de() sans aucune ligne déjà enregistrée.
  verifier("témoin 9.5, lignes_de() : harnais sans ligne enregistrée -> toutes les nouvelles lignes rendues", {
    essai <- function() {
      sauve <- .harnais$lignes; on.exit(.harnais$lignes <- sauve)
      .harnais$lignes <- sauve[0L, ]
      lignes_de(verifier("x", TRUE))
    }
    identical(essai()$statut, "OK") && identical(nrow(lignes_de(NULL)), 0L)
  })

  # Cas de #37 rejoué sur R1 : l'objet recalculé, remis au type double en
  # mémoire comme référence d'avant, ne diffère de l'objet que par le type de
  # ce chemin.
  verifier("témoin 9.5, cas de #37 rejoué sur R1 : objet recalculé remis en double en mémoire, seul écart déclaré, reste admis", {
    c_ <- "etapes/exposition/diagnostics/cellules"
    g <- aplatis[["R1"]]
    if (!identical(unname(g$types[c_]), "integer")) return("cellules de R1 : un chemin integer attendu")
    ref <- g
    ref$types[[c_]] <- "double"; ref$valeurs[[c_]] <- as.double(ref$valeurs[[c_]])
    t <- tr("#901", "R1", list(chemin = c_, nature = "type", de = "double", vers = "integer"))
    !comparer_references(ref, g)$ok && isTRUE(seul_ecart(ref, g, t$changements)) && identical(doubles_sans_tolerance(ref), c_) &&
      !length(restes_hors_transitions(list(R1 = doubles_sans_tolerance(ref)), list(t))$R1)
  })
})

bilan()
