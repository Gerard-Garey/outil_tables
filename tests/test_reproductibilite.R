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
#  3.8) : plans R1 à R17 sur l'entrée de chaque plan (jeu_reference(),
#  jeu_reference_defauts() pour R9, agregats_reference() pour R13 et R17,
#  stocks_reference() pour R14), aplatis, aller-retour exact du format,
#  comparés à leur fichier de tests/reference/ (échec attendu renvoyant à
#  l'issue de création tant que le fichier n'existe pas, liste
#  SANS_REFERENCE : R15 à R17 jusqu'à leur création au (R) de B19), aucune
#  référence orpheline ;
#  R7 : aucun double sans classe de tolérance, Σd par cellule égale aux
#  evenements de l'exposition (#32) ; R8 : validation et importance des
#  exclusions, table importée passée par tables_reference (#166) ; R9 : le
#  même plan sur une entrée à défauts injectés, lignes exclues par motif et
#  retraits non nuls (#166) ; R10 : positionnement par SMR et validation de
#  la table positionnée (#167) ; R11 : test d'homogénéité entre segments
#  (#168) ; R12 : le même test sous H0 vraie, transition en_cours -> rachat
#  (#168, M307) ; R13 : recette agrégats sur la forme agrégats de l'entrée de
#  référence (#169) ; R14 : recette stocks successifs sur la forme stocks de
#  l'entrée de référence (#180) ; R15 à R17 : lissage de Whittaker-Henderson
#  (a) et (b), coherence_regularite et validation de la table lissée (#223,
#  #225, #144, #224) ; témoins sur des copies temporaires (jamais sur
#  tests/reference/) ; règles des modes du workflow et lignes de commande
#  produire_references.R (refus d'appel) et comparer_references.R ;
#  permutation des lignes de l'entrée de référence
#  (#39) : plans R6, R4, R5, R3 et R2, deux permutations, identical() hors
#  sommes par cellule, celles-ci à la tolérance TOLERANCE_PERMUTATION, bilan
#  des écarts et témoins ; chaînes non ASCII et locale (T1 à T4, M54) ;
#  transitions de changement de résultat annoncé (ADR 0009, point 9.5, #47) :
#  liste unique TRANSITIONS_EN_COURS (vide hors transition), utilitaire
#  générique et témoins de chaque nature, de l'union et du succès inattendu.
#  Rapport figé (#87, U24, U25) : ecrire_rapport() sur R1 à R12 (sections et
#  leur ordre, cellules décimales en %.17g, textes fixes, section brut ; taille
#  et durée imprimées), ligne « # rejeu » de la fiche égale à TEXTE_REJEU_FICHE.
#  Table embarquée (#303, M520, sans référence nouvelle) : plan R10 sur
#  TV 88-90 lue par son nom, fiche, rejeu (ligne 15 du bilan), rapport figé.
#  Règle absolue à une échelle déclarée et régime de comparaison (#243 ; ADR
#  0009, annotation du 26/11/2026) : décompte de TOLERANCES par classe,
#  invariants d'ECHELLES_ABSOLUES et de CONSTATS_HORS_PLATEFORME, régime de
#  chaque plan (regime_comparaison(), d'après les lignes « plateforme : » de
#  l'en-tête commité) passé à la comparaison au fichier, régimes et verdicts
#  imprimés par plan, verrou du régime strict (TABLEXP_IMAGE définie ou
#  GITHUB_ACTIONS = true), témoins de la règle absolue et du régime sur des
#  copies temporaires de R15 et de R16.
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
verifier("construction avec brut : fiche$plan porte l'étape brut, fiche$hypotheses_brut juste après fiche$hypothese_censure (#64)",
         identical(xb$fiche$plan, plan_b) &&
           identical(names(xb$fiche)[match("hypothese_censure", names(xb$fiche)) + 1L], "hypotheses_brut"))
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
  # Empreinte du résultat (#67) : recalculée sur l'objet privé des ajouts.
  z$fiche$empreintes$resultat <- tablexp:::empreinte_resultat(z)
  identical(y0, z)
}
verifier("invariance du plan sans brut (R1) : objet identical() à celui du plan avec brut, hors etapes$brut, fiche$plan, fiche$hypotheses_brut et avertissements de l'étape brut (empreinte du résultat recalculée, #67)",
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
verifier("invariance du plan sans kaplan_meier (R1 contre R6) : objet identical() à celui de R6 privé des ajouts de Kaplan-Meier (empreinte du résultat recalculée, #67)", {
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
  # Empreinte du résultat (#67) : recalculée sur l'objet privé des ajouts.
  z$fiche$empreintes$resultat <- tablexp:::empreinte_resultat(z)
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
# référence sur son entrée (champ `entree`, point 4.1), aplati et comparé à son fichier ;
# témoins sur des copies temporaires.
fichier_courant("références de non-régression (ADR 0009) : plans R1 à R17, aller-retour, comparaison aux fichiers")

# Chargement par parse(encoding = "UTF-8") : littéraux marqués UTF-8 en
# toute locale (charger_utf8() d'outils_reference.R, recopié ici).
local(for (e in parse(file.path(RACINE, "tests", "reference", "outils_reference.R"), encoding = "UTF-8",
                      keep.source = FALSE)) eval(e, globalenv()))
DOSSIER_REFERENCES <- file.path(RACINE, "tests", "reference")
# Plans sans fichier de référence (point 3.7) : échec attendu renvoyant à
# l'issue de création du plan (#17 pour R1 à R6, créés en `1019d8e`). Vecteur
# nommé : identifiant du plan -> issue de création. Le commit de référence de
# la session principale retire de cette liste les plans qu'il crée (succès
# inattendu sinon).
# R13 (#169, commit (3c) de B15 ; ADR 0016, point 8.4) : recette agrégats ;
# R14 (#180, commit (4c) de B15 ; spécification de #180, § 13) : recette
# stocks successifs ; créés par le commit de référence (R) de B15, après visa
# d'actuary (M319), run CI 42637989632 (756fbfe).
# R15 (#223, #144), R16 et R17 (#225) (B19, claude/lissage-whittaker ;
# M415) : lissage de Whittaker-Henderson, coherence_regularite et validation
# de la table lissée ; création au (R) de B19, sur la CI, après visa (issue
# de création : celle de la méthode de lissage du plan) ; créés par le commit
# de référence (R) de B19, run CI 43184987915 (e8d7910), visa délégué M420.
SANS_REFERENCE <- character(0)
# Transitions en cours (ADR 0009, point 9.5 et troisième annotation du
# 19/10/2026 ; #47) : liste unique des changements de résultat annoncés dont
# les références ne sont pas encore régénérées. Vide hors transition ; le
# commit de référence de la session principale la vide. Une transition (une
# issue peut en porter plusieurs, à plans deux à deux disjoints, ADR 0009,
# quatrième annotation du 19/10/2026, (b)) :
#   list(issue = "#N", plans = c("R1", ...), changements = list(<changement>, ...))
# Un changement nomme un chemin et la nature exacte du changement admis ; il
# doit être OBSERVÉ sur la référence commitée telle qu'elle est lue, avant
# tout alignement (sinon : « changement déclaré non observé ») :
#   list(chemin = , nature = "type", de = "double", vers = "integer")
#       conversion de type sans perte (double <-> integer) ; la référence est
#       du type `de` ; seconde conversion admise, logical -> character
#       (#177), d'un vecteur vide seulement (logical(0) -> character(0)) ;
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
# Transitions #35 et #42 closes par la régénération unique de B4 (run CI
# 39315172327, d18c269) ; transitions #57, #64 et #67 closes ensemble par la
# régénération unique de B5 (run CI 39435673257, 5757c30 ; ADR 0009, point
# 9.5). Transitions #73 (commit (1) de B13, ADR 0015, point 3.4 : fiche 5,
# champ tables_reference) et #31 (commit (5) de B13 : textes des lignes 3,
# 8, 9 et 12 de STATUTS_BRUT) closes par la régénération unique (R) de B13
# (run CI 42257221627).
# Transition #169 (commit (3b) de B15, ADR 0016, point 8.2 ; M333 : fiche 6,
# champ conditionnel hypotheses_recette, absent de R1 à R12 ; seule
# fiche/version_fiche changeait dans le corps, M334) close par la
# régénération (R) de B15 (run CI 42635335347, bd63b73).
# Transitions #174, #177, #179, #178 et #185 (B16 ; types des dénombrements,
# etat_cellule, ligne khi2_pearson/variance_h0, colonnes p_*_asymptotique des
# sous-groupes, statuts du positionnement et de l'homogénéité) closes par la
# régénération unique (R) de B16 (run CI 42771679323, ecaead0).
# Transitions #199, #200 et #202 (B18 ; types des 27 chemins de dénombrement,
# textes de statut de sous_groupes/bornes_exactes et sous_groupes/p_valeurs,
# texte de tests/p_asymptotique de l'homogénéité) closes par la régénération
# unique (R) de B18 (run CI 42982198441, 140a41b) ; transition_199(),
# transition_200(), transition_202() et les constantes *_200 et *_202
# retirées. CHEMINS_199 reste : liste des 27 chemins de dénombrement de #199
# (ADR 0009, seconde annotation du 10/11/2026, point 9.6 et (e)), lue par le
# contrôle des clés retirées de TOLERANCES ; quatre jeux de plans disjoints :
# {R8, R9} 17 chemins (validation 9, importance_exclusions 8) ; {R10} 16
# (positionnement 5, validation 9, importance_exclusions 2) ; {R11, R12} 5
# (homogeneite) ; {R13} 5 (positionnement).
CHEMINS_199 <- list(
  validation = paste0("etapes/validation/", c(
    "resultat/sous_groupes/observes", "resultat/sequences_runs/positifs", "resultat/sequences_runs/negatifs",
    "resultat/sequences_runs/runs", "resultat/monte_carlo/tirages_effectues", "diagnostics/etats/evenements",
    "diagnostics/evenements_sans_attendu", "diagnostics/evenements_hors_plage", "diagnostics/evenements_total")),
  importance_exclusions_1 = paste0("etapes/importance_exclusions/resultat/smr_comparateur_1/", c(
    "observes", "evenements_sans_exposition_retenue")),
  importance_exclusions_2 = paste0("etapes/importance_exclusions/resultat/smr_comparateur_2/", c(
    "observes", "evenements_sans_valeur_table", "evenements_taux_table_egal_un", "evenements_sans_attendu",
    "evenements_hors_plage", "observes_conserves")),
  positionnement = paste0("etapes/positionnement/", c(
    "resultat/smr/observes", "diagnostics/etats/evenements", "diagnostics/evenements_sans_attendu",
    "diagnostics/evenements_hors_plage", "diagnostics/evenements_total")),
  homogeneite = paste0("etapes/homogeneite/", c(
    "resultat/segments/observes", "resultat/monte_carlo/jeux/tirages_effectues",
    "diagnostics/evenements_cellules_evenement_sans_exposition", "diagnostics/evenements_hors_plage",
    "diagnostics/evenements_total")))
# Transition #215 (porteuse claude/lissage-whittaker, commit (2c) ; tournure
# S5 de #203 des lignes sous_groupes/bornes_exactes, p_valeurs et
# p_valeurs_asymptotiques de STATUTS_VALIDATION, R8 et R9) close par la
# régénération (R) de B19 ; transition_215(), utf8_215() et les constantes
# *_215 retirées.
# Transitions de la reprise des textes de statut et des mentions (#144,
# #244, #245, #246 ; avis d'actuary-approfondi du 12/11/2026) closes par la
# régénération de R15 à R17 (exécution 43203864780) ; transition_textes_245(),
# utf8_245(), apres_245() et les constantes *_245 retirées.
# Transitions #159 (mention d'indépendance des unités en fin de statut des
# lignes de loi, R1 à R17, textes seuls) et #218 (écart minimal détectable
# theta_80 de l'homogénéité, R11 et R12, par ajouts) closes par la régénération
# unique (R) de B30 (exécution 44586537589, b29caa5) ; transition_159(),
# apres_159(), transition_218() et les constantes *_159 et *_218 retirées. Le
# bloc R11 et R12 de #218 (C4, recalcul indépendant, états, aucun signal) reste,
# lu sur les références régénérées.
TRANSITIONS_EN_COURS <- list()
# Transition #242 (B39, étape (3) ; ADR 0017, annotation du 26/11/2026, point
# 5.7 ; remesure (0') d'actuary-approfondi, § 2) : colonne integer
# segments/dimension_non_penalisee du résultat du lissage (R15 à R17), ligne de
# statut segments/dimension_non_penalisee insérée après segments/ddl_effectifs
# dans etapes/lissage/parametres/statuts (une ligne de plus), colonne ajoutée à
# @colonnes de segments, texte du statut du ddl de la validation (relation
# lissee, deux lignes de etapes/validation/resultat/statistiques) ; aucune
# valeur double. Une transition par plan, textes écrits ici en échappements \u
# (UTF-8 en toute locale), sans lire le package ; vecteurs et positions tirés
# de la référence. À clore par la régénération unique (R) de B39, avec
# transition_242() et les constantes *_242.
STATUT_DIM_242_A <- paste("r\u00e9sultat exact : dimension du noyau de la p\u00e9nalit\u00e9 S (polyn\u00f4mes de degr\u00e9 au",
                          "plus z - 1 en l'\u00e2ge, \u00e9chelle q), \u00e9gale \u00e0 l'ordre des diff\u00e9rences z ;",
                          "entier, sans calcul flottant ; lue par la validation (relation lissee) pour reconna\u00eetre, par la",
                          "structure, un segment o\u00f9 le lissage interpole (nombre de cellules pond\u00e9r\u00e9es (levier >",
                          "0) \u00e9gal \u00e0 cette dimension)")
STATUT_DIM_242_B <- paste("r\u00e9sultat exact : dimension du noyau de la p\u00e9nalit\u00e9 S (polyn\u00f4mes de degr\u00e9 au",
                          "plus z - 1 en l'\u00e2ge, \u00e9chelle ln mu), \u00e9gale \u00e0 l'ordre des diff\u00e9rences z ;",
                          "entier, sans calcul flottant ; lue par la validation (relation lissee) pour reconna\u00eetre, par la",
                          "structure, un segment o\u00f9 le lissage interpole (nombre de cellules expos\u00e9es (levier > 0)",
                          "\u00e9gal \u00e0 cette dimension)")
DDL_AVANT_242 <- paste("contribution d'un segment qui compte exactement z cellules pond\u00e9r\u00e9es (z ordre",
                       "des diff\u00e9rences du lissage, etapes/lissage/parametres/ordre_difference ; le lissage",
                       "y interpole, chaque")
DDL_APRES_242 <- paste("contribution d'un segment qui compte exactement d_s cellules pond\u00e9r\u00e9es (levier",
                       "> 0), d_s dimension de l'espace non p\u00e9nalis\u00e9 du lissage",
                       "(etapes/lissage/resultat/segments/dimension_non_penalisee, \u00e9gale \u00e0 l'ordre des",
                       "diff\u00e9rences z sous Whittaker-Henderson, cas normal comme Poisson) ; le lissage y",
                       "interpole, chaque")
SEGMENTS_242 <- "etapes/lissage/resultat/segments/"
STATUTS_242 <- "etapes/lissage/parametres/statuts/"
STAT_VALIDATION_242 <- "etapes/validation/resultat/statistiques/statut"
# Transition #249, points 1 et 2 (B39, étape (4) ; ADR 0017, annotation du
# 26/11/2026, point 9.5 ; remesure (0') d'actuary-approfondi, § 3) : forme
# close de ln |K'K|+ et arrêt après demi-pas. Textes : hypothèse du lissage
# (règle d'arrêt après un pas raccourci, insérée avant « à l'épuisement des
# divisions du pas »), statut de segments/laml (terme ln |K'K|+ par forme
# close), R16 et R17. Valeurs : R16, 22 grandeurs double (lambda^ et
# grandeurs dérivées, au plancher de bruit de lambda^) ; R17,
# segments/laml seule (lambda déclaré). Valeurs nouvelles écrites ici en
# %.17g (aller-retour exact), relevées sur l'objet du commit de #249
# (R 4.3.3, relevé de produire_references.R --mode releve ; doubles de R16
# et R17 identiques au bit à tests/reference avant #249, sur ce poste) ;
# qx_table de la validation identique à qx de la table lissée, ddl_effectifs
# et ddl_effectifs_retenus de la validation à ddl_effectifs du lissage
# (identical(), mesuré), déclarés par renvoi. Composition avec #242 : le
# vecteur des statuts du lissage, déclaré entier par #242 (insertion), porte
# aussi le texte de laml ; les deux transitions le déclarent à la même valeur
# (statuts_lissage_b39()), appliquée une fois (aligner_reference()). À clore
# par la régénération unique (R) de B39, avec transition_249(),
# statuts_lissage_b39() et les constantes *_249.
SUBST_HYPOTHESE_249 <- c(
  "quand m est arrondi \u00e0 0) ; \u00e0 l'\u00e9puisement",
  paste("quand m est arrondi \u00e0 0) ; arr\u00eat apr\u00e8s un pas raccourci seulement si le d\u00e9cr\u00e9ment de",
        "Newton du pas complet d'(W + P)d est au plus EPSILON_DEVIANCE_LISSAGE |dev_P| + epsilon machine x somme",
        "des D, poursuite sinon ; \u00e0 l'\u00e9puisement"))
AJOUT_LAML_249 <- paste(" ; terme ln |K'K|+ (constant en lambda) par forme close exacte, somme sur j de 0 \u00e0 z - 1",
                        "de [2 ln j! - ln (2j)! - ln (2j + 1)! + somme sur k de -j \u00e0 j de ln(n + k)] (r\u00e9sultat",
                        "exact ; valeur calcul\u00e9e par lgamma et log, \u00e0 quelques unit\u00e9s de dernier rang pr\u00e8s)")
CHEMIN_HYPOTHESE_249 <- "etapes/lissage/parametres/hypothese"
# Statuts du lissage après #242 et #249 : texte de segments/laml, s'il
# existe (wh_poisson ; absent sous wh_quadratique, R15), complété par
# AJOUT_LAML_249, puis `statut` inséré après segments/ddl_effectifs.
statuts_lissage_b39 <- function(ref, statut) {
  gr <- ref[[paste0(STATUTS_242, "grandeur")]]
  st <- ref[[paste0(STATUTS_242, "statut")]]
  k <- match("segments/ddl_effectifs", gr); l <- match("segments/laml", gr)
  if (!is.na(l)) {
    if (grepl("par forme close exacte", st[l], fixed = TRUE))
      stop("statuts_lissage_b39() : texte de #249 d\u00e9j\u00e0 pr\u00e9sent ; r\u00e9f\u00e9rence d\u00e9j\u00e0 r\u00e9g\u00e9n\u00e9r\u00e9e ? retirer transition_249()")
    st[l] <- paste0(st[l], AJOUT_LAML_249)
  }
  st <- append(st, statut, after = k)
  Encoding(st) <- "UTF-8"
  st
}
APRES_R16_249 <- list(
  "lissage/resultat/table_lissee/qx" =
    c(0.0001358420752273579, 0.00014906005455876968, 0.00016354215748735663, 0.00017940709860201633, 0.00019678452793643183,
    0.00021581601892619429, 0.00023665614532352924, 0.00025947365103312089, 0.00028445272550909207, 0.00031179439609685611,
    0.00034171804416182261, 0.00037446305668936415, 0.00041029061879063674, 0.00044948564887387662, 0.00049235890131526106,
    0.00053924924359361175, 0.00059052614502361125, 0.00064659237095419366, 0.00070788691157427453, 0.00077488815665275863,
    0.00084811733867278232, 0.0009281423261430033, 0.0010155817968537987, 0.0011111098550932142, 0.0012154611515425497,
    0.0013294364674914638, 0.0014539089085927079, 0.0015898307234398954, 0.0017382408239032116, 0.0019002731791938001,
    0.0020771662548513792, 0.0022702736308182372, 0.0024810758326924342, 0.0027111934936308348, 0.0029624019714073473,
    0.0032366471683808922, 0.0035360632045372711, 0.0038629920119014759, 0.004220004737420689, 0.004609925169959438,
    0.0050358555440668503, 0.0055012043686041741, 0.0060097171828220807, 0.0065655099036983321, 0.0071731054848681941,
    0.0078374741943084119, 0.0085640776926157354, 0.0093589173141007485, 0.010228583158648261, 0.011180306769975975,
    0.012222019478727751, 0.01336241771419004, 0.014611034804327511, 0.015978316264901225, 0.017475698554608947,
    0.019115691497827061, 0.020911965680431788, 0.022879444988166617, 0.025034409150380951, 0.027394606345006907,
    0.029979374626132985, 0.032809774806930571, 0.035908732684249052, 0.039301186156836136, 0.04301423600917139,
    0.047077299633602659, 0.051522264442343091, 0.056383637143024419, 0.061698690412570097, 0.067507599157306766,
    0.073853552309853299, 0.00032227698693676319, 0.00034272261991930394, 0.00036486427864160768, 0.00038886154269423694,
    0.00041489066173455279, 0.00044314640505397393, 0.00047384411137691405, 0.00050722193049223987, 0.00054354330446960584,
    0.00058309971847271785, 0.00062621379846659322, 0.00067324273558390889, 0.00072458203129182176, 0.00078066974180844291,
    0.00084199111299738689, 0.00090908363352114633, 0.00098254247341814527, 0.0010630265139134445, 0.0011512648534712764,
    0.0012480638882320272, 0.0013543151237278568, 0.0014710036734821321, 0.0015992173219511626, 0.0017401560293976103,
    0.001895142039750325, 0.0020656306472723054, 0.0022532212867965866, 0.0024596693260290304, 0.0026868981015358119,
    0.002937012292168906, 0.0032123123549096903, 0.0035153112342207319, 0.0038487534067674157, 0.0042156346289861065,
    0.0046192243871860054, 0.0050630894091157333, 0.0055511182278853302, 0.0060875486811113018, 0.0066769975142503338,
    0.0073244954896941996, 0.0080355279224881091, 0.0088160767706336584, 0.009672669264367259, 0.010612432557765623,
    0.011643145692436066, 0.012773288143773091, 0.014012085708114123, 0.015369552165218214, 0.016856532332935409,
    0.01848474764615439, 0.020266839116309512, 0.022216411095298406, 0.024348073549803867, 0.026677485191422989,
    0.029221395554987706, 0.031997712983967024, 0.035025561026666999, 0.038325354520112119, 0.041918854355029962,
    0.045829199144052202, 0.050080930804043126, 0.054699974625138315, 0.059713644828054968, 0.065150676377020753,
    0.071041271240101928, 0.077417122250795473, 0.084311380770997785, 0.09175859921729021, 0.09979458707494572,
    0.10845624284708826, 0.1177813368857262),
  "lissage/resultat/table_lissee/levier" =
    c(0.0032382369676394553, 0.0090575155221753901, 0.014806527179292489, 0.01962877437731789, 0.024212478613064602,
    0.028424217368223616, 0.032223916271539699, 0.035658333038317988, 0.038028918526305501, 0.040018351789196063,
    0.042232789881091692, 0.043383917364691126, 0.043999616559613451, 0.043839618426206109, 0.043672655899930121,
    0.042971644982797715, 0.041990428738042936, 0.040680052432796862, 0.039665023994094391, 0.038479661763587927,
    0.036802462599838021, 0.035442176482877882, 0.034437894764020573, 0.033383363882383887, 0.032591395496226437,
    0.031671747138930208, 0.0313916030400248, 0.03112105778016952, 0.030915337703676132, 0.030403738401922956,
    0.030301438228116193, 0.030582755626944558, 0.030874683065533936, 0.031522493847263945, 0.032089696089496099,
    0.032646859322331467, 0.033171437560385507, 0.034514339992377595, 0.035466037123959883, 0.036610717107180828,
    0.038084832976660037, 0.0392115664698197, 0.040701572124815504, 0.042290674945245049, 0.044677503305291529,
    0.046131713745320074, 0.04744117812517102, 0.049385747059826866, 0.051601902903420482, 0.053811635568920171,
    0.055201443439526883, 0.056231781820112035, 0.057836207213646691, 0.059970077338723816, 0.062946986491898949,
    0.063852905792633557, 0.062091200861572098, 0.061699515024000409, 0.062380913296474695, 0.064048201711783617,
    0.067975715622535002, 0.073029364580854544, 0.077690552461432152, 0.08274535947718363, 0.086828601521063489,
    0.087998840458094238, 0.088634395965325408, 0.082900065077199547, 0.076248877176449861, 0.067467013242567028,
    0.053561107641379689, 0.0077003694871624097, 0.020704114043662427, 0.030127969302676666, 0.038337539133560201,
    0.044128336400935129, 0.047445872927864854, 0.049294186318099086, 0.050272971980574066, 0.051388801598307055,
    0.051995074247825318, 0.0518338858098795, 0.05032546115999223, 0.049589138400608544, 0.048477595563687442,
    0.046659083001751221, 0.044669283450222752, 0.042772045300101827, 0.041150370634442164, 0.039696524529583624,
    0.038197138450351435, 0.036904705806093459, 0.035598189163329165, 0.035009986870119983, 0.034683221976825655,
    0.03439966809923669, 0.034925024036758893, 0.035192661404690538, 0.03572258582578984, 0.036469092384580848,
    0.037268314460304569, 0.037760774847320273, 0.038699594731196045, 0.039106493026928549, 0.0393141662792484,
    0.040230203432390331, 0.041045257431506943, 0.041532727240218378, 0.042423332226210753, 0.043141832208968098,
    0.043566482016682867, 0.044674965271651275, 0.045337381091889645, 0.046216369144885083, 0.047069523481833596,
    0.048059566923908378, 0.049409672019434796, 0.051059705793426523, 0.052511466380946344, 0.054065445376560627,
    0.056493967208584614, 0.059504645732481237, 0.062912246873615865, 0.066276807734706039, 0.070578326701748073,
    0.074522937639383585, 0.07585731717098694, 0.07268649446645882, 0.070832343459759264, 0.070232457950169733,
    0.07120912085737903, 0.073276540419467143, 0.076999401092103292, 0.082075639295854083, 0.08617650191269767,
    0.09034181651630728, 0.09383888667598124, 0.093366495666993662, 0.09290891566923587, 0.087578042830283559,
    0.080100357665235891, 0.066764636275805908),
  "lissage/resultat/table_lissee/borne_inf_credibilite" =
    c(7.1688552922596986e-05, 8.143219799341828e-05, 9.2358790133927957e-05, 0.00010459339895242185, 0.00011827220037562196,
    0.00013354327399030168, 0.00015056745203672226, 0.00016951922357193487, 0.00019058770352886457, 0.0002139776772820505,
    0.00023991072987682291, 0.00026862647397592849, 0.0003003838883413325, 0.00033546277782629721, 0.00037416538540262581,
    0.00041681817785886485, 0.00046377385359950001, 0.00051541359242288532, 0.00057214959680137035, 0.00063442796314148553,
    0.00070273193088572587, 0.00077758560555583433, 0.00085955820723437166, 0.00094926891658064208, 0.0010473923755835624,
    0.001154664801942524, 0.0012718908165594374, 0.0013999509606381498, 0.0015398099222468933, 0.0016925255751275385,
    0.0018592589420877038, 0.0020412851796448072, 0.0022400056138521121, 0.0024569609576088202, 0.0026938458747513736,
    0.0029525247385953348, 0.003235049268758939, 0.0035436782172815957, 0.0038808991007358669, 0.0042494522496238146,
    0.0046523575237716452, 0.0050929433120750888, 0.005574878461531603, 0.0061022064434639841, 0.0066793817524945769,
    0.0073113077287535666, 0.0080033742591733709, 0.0087614931280647328, 0.0095921239968566017, 0.010502288074353331,
    0.011499563803265393, 0.012592055840282515, 0.013788325078736275, 0.015097265386417697, 0.016527921090179809,
    0.018089254530095907, 0.019789901292311902, 0.02163798314748212, 0.023641070688992714, 0.025806351849186882,
    0.028140972050787045, 0.030652419142733176, 0.033348799232703751, 0.036238919626484359, 0.039332198950987031,
    0.042638480156958085, 0.046167816441915983, 0.049930269583109928, 0.053935737685269367, 0.058193806224030448,
    0.062713607253575845, 0.00017269041269754454, 0.00019165998439399427, 0.00021246035326454078, 0.00023524660073487817,
    0.00026018606865944427, 0.00028745951082819084, 0.00031726241887425387, 0.00034980654525624616, 0.00038532169351165944,
    0.00042405785260970174, 0.00046628780569917932, 0.00051231029334730216, 0.00056245382927120799, 0.00061708141370924612,
    0.00067659616586087442, 0.00074144795858853679, 0.00081214103036406538, 0.00088924266313894127, 0.00097339266553546594,
    0.0010653134881152225, 0.0011658208067098742, 0.0012758342510487916, 0.0013963879516965439, 0.0015286406847979316,
    0.0016738857660106438, 0.0018335608598881597, 0.0020092575783961079, 0.0022027313742757231, 0.00241591145379294,
    0.0026509117596876471, 0.0029100427973672389, 0.003195825359395161, 0.003511006142271659, 0.0038585736782344093,
    0.0042417762970857666, 0.0046641405560072461, 0.0051294901013999193, 0.0056419667615964153, 0.006206053314272866,
    0.0068266014729364182, 0.0075088656490274464, 0.0082585396997972241, 0.0090818023038358123, 0.0099853715682228818,
    0.010976561585194371, 0.012063340571483937, 0.013254390701495637, 0.014559166065808107, 0.015987949835208085,
    0.017551904603999097, 0.01926310012872642, 0.02113450578496618, 0.023179923030997689, 0.025413829438727682,
    0.027851093549856241, 0.030506543639304367, 0.033394323099769425, 0.036527074015709246, 0.039915013233719328,
    0.043565129153660932, 0.047480822533612972, 0.051662155611042718, 0.056106658996422351, 0.060810263187731264,
    0.065767897474351189, 0.070973566923921955, 0.076420021699472582, 0.082098291019658071, 0.087997204594603931,
    0.094103039277640785, 0.10039928182762317),
  "lissage/resultat/table_lissee/borne_sup_credibilite" =
    c(0.00025739869049055828, 0.00027284386753511605, 0.00028958048772272451, 0.00030772540152710325, 0.00032740695626449394,
    0.00034876614905307373, 0.00037195789455355619, 0.00039715241077723466, 0.00042453673969147023, 0.00045431641513574324,
    0.00048671728239779911, 0.00052198747992297518, 0.00056039958291142654, 0.00060225290265288284, 0.00064787596469803967,
    0.00069762915978962116, 0.00075190759743513414, 0.00081114413054161297, 0.00087581256363726643, 0.00094643103120573143,
    0.0010235655461978161, 0.001107833790755572, 0.0011999091582850939, 0.0013005251048729223, 0.0014104798714014673,
    0.0015306415393738398, 0.0016619536141298081, 0.0018054411874184385, 0.0019622178117156643, 0.0021334933256942435,
    0.0023205828568565903, 0.0025249171668131729, 0.0027480543690155336, 0.0029916931156595008, 0.0032576873288933913,
    0.003548062112128028, 0.0038650314552492321, 0.004211017691396938, 0.0045886724773990572, 0.0050008994607818758,
    0.0054508790020197373, 0.0059420946489640893, 0.0064783625681593847, 0.0070638640122946926, 0.0077031823662155659,
    0.0084013463315088452, 0.009163881347754553, 0.0099968725475707738, 0.010907039817682325, 0.011901833934555817,
    0.012989564217260432, 0.01417956980499672, 0.015482446825582738, 0.016910340996056528, 0.018477313085830057,
    0.020199770156297092, 0.022096929778432417, 0.024191250134382954, 0.026508746756963523, 0.029079143272257125,
    0.031935891766845234, 0.035116199158799814, 0.038661213802879145, 0.042616451969866784, 0.047032446378498217,
    0.051965543947125409, 0.057478779619456612, 0.063642780824537087, 0.070536687638223003, 0.078249073838347616,
    0.086878844051898468, 0.0006013983291061481, 0.00061281336316849997, 0.00062655766402081037, 0.00064275408111073731,
    0.00066155095128900103, 0.00068312387047124597, 0.00070767777378913637, 0.00073544927447917832, 0.00076670928848565504,
    0.00080176592258504836, 0.00084096764930880244, 0.00088470663600570792, 0.00093342211486838287, 0.00098760391493474113,
    0.0010477959019764248, 0.0011145993008338684, 0.0011886758579624263, 0.001270751173997278, 0.0013616182274771346,
    0.0014621414606206507, 0.0015732619052554156, 0.0016960035770905166, 0.0018314812152437317, 0.0019809093395373152,
    0.0021456127977503669, 0.0023270387457511362, 0.0025267695083297497, 0.002746536575121437, 0.0029882350810074448,
    0.0032539399172180497, 0.003545923138823744, 0.0038666740400477308, 0.0042189220148263831, 0.0046056604886131995,
    0.0050301742132120922, 0.0054960681753385516, 0.0060072981219208912, 0.0065682046627765379, 0.007183549813228096,
    0.0078585592230771169, 0.0085989693363609874, 0.0094110744713791009, 0.010301778137825141, 0.011278646922501736,
    0.012349956768112872, 0.013524730596536676, 0.014812768791788224, 0.016224673183861702, 0.017771875072419403,
    0.01946667618368688, 0.021322308971335718, 0.023353037168616302, 0.025574318664834114, 0.028003066531432416,
    0.030658048694576355, 0.03356050174619938, 0.036734956851052665, 0.040210305749590176, 0.044020968514947779,
    0.048207940247876419, 0.052819437107260669, 0.057910908252391827, 0.063544615768532681, 0.069789225373725672,
    0.076719847276052022, 0.084418644668472445, 0.092975823960635459, 0.10249078445646913, 0.11307315754070746,
    0.12484368296558318, 0.13793482013993741),
  "lissage/resultat/segments/parametre_lissage" =
    c(51156764.038528062, 15310743.23488079),
  "lissage/resultat/segments/ddl_effectifs" =
    c(3.2268313683885332, 3.7206980705094632),
  "lissage/resultat/segments/regularite" =
    c(4.2665319280504108e-09, 4.8732983110223029e-08),
  "lissage/resultat/segments/laml" =
    c(-8824.8414251290978, -12270.116128901132),
  "lissage/resultat/segments/deviance_penalisee" =
    c(64.911937005084312, 60.180161780077533),
  "lissage/resultat/segments/residu_conservation_0" =
    c(-1.4406741979944293e-09, 2.49255279888283e-07),
  "lissage/resultat/segments/residu_conservation_1" =
    c(-9.406982159898547e-08, 7.9833369001081067e-06),
  "validation/resultat/cellules/taux_table" =
    c(0.00013585130259771182, 0.00014907116511280911, 0.00016355553196420819, 0.00017942319398064297, 0.00019680389255212892,
    0.00021583931055440479, 0.00023668415280793829, 0.00025950732014520204, 0.00028449318985926191, 0.000311843014075713,
    0.00034177644307702983, 0.00037453318548741918, 0.00041037481101621162, 0.00044958669782931975, 0.00049248014975924711,
    0.00053939469075750231, 0.00059070057426098652, 0.00064680150195448038, 0.00070813758181850132, 0.00077518853766490251,
    0.00084847719366341911, 0.00092857331693295864, 0.0010160978494721986, 0.0011117275952754538, 0.0012162004235463199,
    0.0013303209521496931, 0.0014549668597173319, 0.0015910958453687657, 0.0017397533174569703, 0.0019020809888560964,
    0.0020793265567323718, 0.0022728546090888414, 0.0024841588017786758, 0.002714875435187203, 0.0029667985692678287,
    0.0032418964405821846, 0.0035423298532358022, 0.0038704726368208962, 0.0042289340875333939, 0.0046205836441227501,
    0.0050485781955267872, 0.0055163917181085915, 0.0060278482111960066, 0.0065871576682783994, 0.0071989558986915586,
    0.0078683486193245299, 0.0086009601330076998, 0.0094029871603019764, 0.010281254592624054, 0.011243276184537422,
    0.0122973225582956, 0.01345249817963861, 0.014718827234475981, 0.016107345856522386, 0.017630201249044984,
    0.019300758581137327, 0.021133717798241792, 0.023145241499620686, 0.025353100043335419, 0.02777683542327046,
    0.030437944438740861, 0.033360086025975594, 0.036569313214301076, 0.040094328246956766, 0.043966763302492586,
    0.04822149051023502, 0.052896963200802845, 0.058035590716205017, 0.063684155984847765, 0.069894276727727384,
    0.076722905987195306, 0.00032232892932510844, 0.00034278136273845143, 0.00036493085780792085, 0.00038893716894999398,
    0.00041497675267819256, 0.00044324462343995165, 0.00047395641097426041, 0.00050735061105053325, 0.00054369107768137545,
    0.00058326978722813509, 0.00062640995222102141, 0.00067346946524285899, 0.00072484466772728198, 0.00078097462311606529,
    0.00084234578661656918, 0.0009094971006506407, 0.00098302548468615666, 0.0010635919273332399, 0.0011519280679252214,
    0.001248843368594942, 0.0013552330373114696, 0.0014720866615690932, 0.0016004974349405256, 0.0017416718596769802,
    0.0018969400934963664, 0.002067767004715026, 0.002255763609541982, 0.0024626992821054836, 0.0026905142912478047,
    0.0029413337763467265, 0.0032174829061621265, 0.0035215044590925434, 0.0038561789169221212, 0.0042245454686958891,
    0.0046299259722210008, 0.0050759502751400046, 0.005566582942200313, 0.0061061533485575843, 0.0066993883870100869,
    0.0073514513126418943, 0.0080679867766086013, 0.0088551683009953988, 0.0097197533952043391, 0.010669146022244735,
    0.011711457876749799, 0.01285556799587791, 0.01411118176508365, 0.015488888070787118, 0.017000220687853031,
    0.018657725545461072, 0.020475029206320102, 0.02246691267172906, 0.024649388913077063, 0.027039787372385375,
    0.029656844469163873, 0.032520829088482243, 0.035653666104503073, 0.039079091882963428, 0.042822801419922576,
    0.046912587022345645, 0.051378488336497344, 0.056252914707614954, 0.061570816947704103, 0.06736991388507052,
    0.073690966601964544, 0.080578066724318428, 0.088078907438638748, 0.096245075822470341, 0.10513230511643924,
    0.11480076026661543, 0.12531533633865469),
  "validation/resultat/cellules/attendus" =
    c(0.030446644318633094, 0.09518121583062196, 0.1741927753334466, 0.25895372610523459, 0.35877906332000081,
    0.47383430526562959, 0.6052521704155982, 0.75575483291047241, 0.91075544479532689, 1.0843675180562231,
    1.2962954189981899, 1.5099491055482934, 1.7379102305161258, 1.9663735952833732, 2.2253706241253202,
    2.4878458481942531, 2.7616212204944395, 3.0377047742201913, 3.3600164990865471, 3.6930415805506236,
    3.9950340466303911, 4.3425304964762805, 4.7505363997118932, 5.1694663097922469, 5.6465887092885421,
    6.1168747903328526, 6.7316796606633078, 7.3789295505154575, 8.0695382750716256, 8.6977584046100151,
    9.4583751126707316, 10.370508566193646, 11.325388295484153, 12.457791871092345, 13.61149892245653,
    14.810696784821674, 16.043620390905566, 17.745859471879037, 19.336409782937221, 21.120206478930626,
    23.205276806102784, 25.19821564081585, 27.556345368255975, 30.143878687894318, 33.513951229676515,
    36.416030274151368, 39.417691661794876, 43.205507744968266, 47.552445613766352, 52.243413035091052,
    56.44136056007401, 60.467277086550766, 65.207875951751632, 70.496334067631295, 76.44827080801403,
    79.025537818639236, 76.823966187171777, 74.455060927904853, 71.291585615122102, 67.135463674405955,
    63.305350180685743, 58.715037463378856, 52.667366332468951, 46.473696326831877, 39.932010217259567,
    32.916544227518202, 26.891879634083519, 20.404631205264373, 15.259062240551973, 11.017135727199912,
    7.1688587697006545, 0.07597285384443711, 0.23539156691125093, 0.39565655817327683, 0.58285651614686185,
    0.77829950160660621, 0.97258457621807237, 1.1762796946365695, 1.3981998128373043, 1.6671326738412005,
    1.9681328797030422, 2.2885363415694093, 2.589075082605572, 2.967443885786146, 3.365429512725965,
    3.7447824690766587, 4.1266969442340304, 4.5249759995570571, 4.9561601686196921, 5.4082213964680346,
    5.8469576266571535, 6.3037511681759222, 6.7397810861935454, 7.3005498657961789, 7.9199733112770989,
    8.5587378939674146, 9.4278134568904086, 10.272751716170427, 11.247306455248145, 12.363968367888889,
    13.59143885276402, 14.807715750600467, 16.319828187029511, 17.742624518028098, 19.203370775386137,
    21.173164682612672, 23.293709418552083, 25.43212586771854, 28.039699618246718, 30.777926679891813,
    33.531433949077282, 37.056254507885036, 40.458821705979176, 44.267113146988599, 48.242676530540891,
    52.515259282951305, 57.320912084165101, 62.602716117658304, 67.719785863295002, 72.988858087288278,
    79.471077220004886, 86.841776389400223, 94.858578382264938, 102.81588294423688, 112.13103537373019,
    120.55324948948336, 123.9398517858738, 118.54877554350142, 113.39191933531293, 107.82926640528673,
    101.78783612277729, 94.18178466737038, 85.76355505524883, 76.493031006268211, 65.237491455492503,
    54.32609816185871, 44.173296686066145, 34.136289003563526, 26.317561453739227, 19.246422122852849,
    13.712224232195851, 8.9541424032410166),
  "validation/resultat/cellules/residu_reponse" =
    c(-0.030446644318633094, -0.09518121583062196, -0.1741927753334466, -0.25895372610523459, 0.64122093667999924,
    -0.47383430526562959, -0.6052521704155982, 0.24424516708952759, 0.089244555204673115, 0.91563248194377689,
    1.7037045810018101, -0.50994910554829342, 0.26208976948387419, -1.9663735952833732, 0.77462937587467984,
    -0.48784584819425314, 0.23837877950556052, -0.037704774220191251, -3.3600164990865471, -1.6930415805506236,
    -1.9950340466303911, -1.3425304964762805, 3.2494636002881068, -1.1694663097922469, 1.3534112907114579,
    0.88312520966714736, -1.7316796606633078, -2.3789295505154575, -1.0695382750716256, 2.3022415953899849,
    2.5416248873292684, 2.6294914338063542, 8.6746117045158471, -2.4577918710923452, 0.38850107754347007,
    4.1893032151783256, 1.9563796090944336, -0.74585947187903656, 4.6635902170627794, -4.1202064789306263,
    1.7947231938972159, -3.1982156408158495, -2.5563453682559754, -1.1438786878943183, -1.5139512296765147,
    17.583969725848632, -4.4176916617948763, -12.205507744968266, -11.552445613766352, -3.2434130350910522,
    9.5586394399259902, 5.5327229134492342, 1.7921240482483682, -3.4963340676312953, -3.4482708080140299,
    -13.025537818639236, -6.8239661871717772, -0.45506092790485297, -3.2915856151221021, 2.8645363255940453,
    8.6946498193142574, 6.2849625366211441, 3.3326336675310486, 3.5263036731681225, 3.0679897827404332,
    -2.9165442275182016, 1.1081203659164807, 6.5953687947356272, -3.2590622405519731, -4.0171357271999124,
    -2.1688587697006545, -0.07597285384443711, 0.76460843308874904, 0.60434344182672317, 0.41714348385313815,
    1.2217004983933939, 0.027415423781927628, -0.17627969463656945, -1.3981998128373043, 0.33286732615879955,
    1.0318671202969578, -2.2885363415694093, 1.410924917394428, 1.032556114213854, 1.634570487274035,
    -1.7447824690766587, 0.87330305576596956, -0.52497599955705709, -1.9561601686196921, -0.40822139646803457,
    1.1530423733428465, 1.6962488318240778, -0.73978108619354543, -1.3005498657961789, 1.0800266887229011,
    -2.5587378939674146, 0.57218654310959138, -6.2727517161704274, -2.247306455248145, -6.363968367888889,
    -2.5914388527640195, 6.1922842493995329, -2.3198281870295112, 5.2573754819719021, 3.7966292246138629,
    -2.173164682612672, 1.7062905814479166, -5.4321258677185398, -1.0396996182467184, 8.222073320108187,
    -2.5314339490772824, 0.94374549211496372, 13.541178294020824, 7.7328868530114008, 1.7573234694591093,
    0.48474071704869459, -7.3209120841651014, -6.6027161176583036, -0.7197858632950016, -3.9888580872882784,
    -1.4710772200048865, -2.8417763894002235, -0.85857838226493755, -13.815882944236876, 10.868964626269815,
    -6.5532494894833633, 13.060148214126201, 7.4512244564985792, -2.3919193353129344, 9.1707335947132691,
    -15.787836122777293, -9.1817846673703798, -1.7635550552488297, 7.5069689937317889, 10.762508544507497,
    -0.32609816185870955, 8.8267033139338551, -6.136289003563526, -1.3175614537392271, -2.2464221228528487,
    -2.712224232195851, -0.95414240324101662),
  "validation/resultat/cellules/residu_pearson" =
    c(-0.1744896682289043, -0.30851453098779963, -0.41736408007092152, -0.50887496116947484, 1.0705184296055668,
    -0.68835623427526949, -0.77797954369996014, 0.28095418946685707, 0.0935149059078004, 0.87929163351458473,
    1.4963813887493904, -0.41499769044988, 0.19880916394297565, -1.4022744365078375, 0.51926946722387846,
    -0.30929356167637601, 0.14344504010934297, -0.021633339339760665, -1.8330347784716325, -0.88099922350968374,
    -0.99813680334009014, -0.64424746927856291, 1.4908719761733487, -0.51435724375321068, 0.56955637536309056,
    0.35707339018260559, -0.66743017180650122, -0.87575958456913294, -0.37650607511384165, 0.78063357458686988,
    0.82642450720058491, 0.81652922414834483, 2.5776473056449563, -0.69634517071183122, 0.10530263800982322,
    1.0885642055417342, 0.48842955893300982, -0.17705512011989569, 1.0605532080712365, -0.89654047489479372,
    0.37256675463155148, -0.63712236250286625, -0.48697730067981082, -0.20834371405341975, -0.26151648383225445,
    2.91387304933341, -0.70363884407792676, -1.85689049851868, -1.6752803860718979, -0.44873142758288653,
    1.2723228968575169, 0.71150624213093139, 0.22193103606834502, -0.41641815454437131, -0.39438240613694653,
    -1.4652506883643655, -0.77855357610970222, -0.05273785191000542, -0.38983965915808638, 0.34960556541108201,
    1.0927778601632547, 0.82021559448988302, 0.45921575174756141, 0.51726858281547583, 0.48550456994211455,
    -0.50834838661489301, 0.21368614162664568, 1.4600734716243549, -0.83431246526802749, -1.2102696862432671,
    -0.81003961861846974, -0.2756317359166704, 1.5759543515815828, 0.96078149377845978, 0.54639238229510267,
    1.3848137749343497, 0.027799134482435449, -0.16253503512849035, -1.1824549940007461, 0.25780188340271298,
    0.73552352169931545, -1.5127909113851157, 0.87686266838323501, 0.59940782073282661, 0.89101169386856416,
    -0.90162924819016921, 0.429896284479305, -0.24679213159840785, -0.87868203064031147, -0.17553694390467672,
    0.47684836107453565, 0.67560060646756137, -0.28495765510822157, -0.4813369732988001, 0.38377141947958782,
    -0.87462308822435342, 0.18635118094155095, -1.9571075938810041, -0.67009755761940726, -1.8098770359952565,
    -0.70292379965531993, 1.6091882373378315, -0.57424606764789299, 1.2481306970943271, 0.86638206379425076,
    -0.47228021331909914, 0.35353601863160228, -1.0771557206275875, -0.19634561477532822, 1.4820459363487666,
    -0.43716012056371784, 0.15503299247142119, 2.1288734273505945, 1.1622540155654648, 0.25300902430034411,
    0.066890854501447911, -0.96696030344621886, -0.83449941620218704, -0.087467260540852881, -0.46689615883166447,
    -0.1650178484825941, -0.30494783356861932, -0.088153922922176636, -1.3625377398066909, 1.0264203625858315,
    -0.59685280844548327, 1.1731204873411558, 0.68435132070322779, -0.22462364806674145, 0.88315260093671577,
    -1.5648570428417907, -0.94611438473985554, -0.19043099414734818, 0.85832880431326175, 1.3324928359840589,
    -0.044242951725830156, 1.3280628340123792, -1.0502619997122156, -0.25683136376940296, -0.51205470859896762,
    -0.7324392994223875, -0.31886084918703944),
  "validation/resultat/cellules/residu_deviance" =
    c(-0.24676565530329822, -0.43630543391212068, -0.59024194248366757, -0.71965787163795347, 0.87615930717077195,
    -0.97348272225615751, -1.1002292219493157, 0.26755591340902468, 0.092046830932047741, 0.78570746524139456,
    1.2756180689112451, -0.44243227781071665, 0.19410225485720528, -1.9831155262784734, 0.49282261914060771,
    -0.32033039082112341, 0.1414522426496013, -0.021678325491844459, -2.5923026440161445, -0.96585211236546042,
    -1.1056438891120988, -0.68263553493474693, 1.3564722110995233, -0.53584687108202711, 0.54881434170419252,
    0.34896033376962238, -0.69963671960586171, -0.93056564411558185, -0.38531924124859246, 0.74951676349395957,
    0.79302920281450895, 0.78522785593524258, 2.323400361280036, -0.72135982501318352, 0.10480757609313474,
    1.0424908207180776, 0.47897570871779305, -0.17831763229885003, 1.0217099873685924, -0.92835265772752018,
    0.36791274533726392, -0.65136712284451415, -0.49481353168459707, -0.20968267683285222, -0.26352344527721011,
    2.7168654795416241, -0.71743686475480661, -1.9565805202913629, -1.7510747975615324, -0.45349890827477807,
    1.2387301204241667, 0.70104971165772567, 0.22092593893636339, -0.41993341213855595, -0.39740439090514679,
    -1.508549359157237, -0.79052836317592523, -0.052791710481834217, -0.39289875382580236, 0.34716258726006066,
    1.069094752225962, 0.80619914406834536, 0.45449617387799102, 0.51092561239786827, 0.47947860940107606,
    -0.51614690603886926, 0.21224323543433576, 1.3904354884030472, -0.86701692706371014, -1.2979551548771915,
    -0.85710671352489398, -0.38980213915379458, 1.1678154616334555, 0.80357361771492208, 0.49531959788587043,
    1.1540204447786826, 0.027670043570529913, -0.16687143472276408, -1.6722438894116518, 0.24986659233945907,
    0.68222253866668614, -2.1394094239155859, 0.81123328729652855, 0.56888940813056321, 0.83047106392642789,
    -0.99030275271746515, 0.4159325145695752, -0.25181084705187484, -0.94879206417393447, -0.17781774739877892,
    0.4623384799768907, 0.64826609325001205, -0.29042463373796645, -0.49680713515150687, 0.37551027419064864,
    -0.92474676252377241, 0.18451232367322171, -2.2360453748547049, -0.69450642506026927, -2.0128618373269456,
    -0.72724929849650255, 1.5129910607271961, -0.58872624791688177, 1.1930495101764886, 0.83995686838617467,
    -0.48072568025472134, 0.34934649896137471, -1.1193818679408705, -0.19757815695558062, 1.4224752884972385,
    -0.44284126033990079, 0.15438181759134698, 2.0241471988482367, 1.1306641592550069, 0.25149582987246938,
    0.066788342558661049, -0.98872302151992042, -0.84985354477354547, -0.0876228974023259, -0.4712486943995865,
    -0.16553091927440172, -0.30663409559218385, -0.088287409232276459, -1.3949004951316375, 1.0104742566544138,
    -0.6023865696204137, 1.153372736152229, 0.67736379339284825, -0.22542038302555886, 0.87105792146611993,
    -1.6081736395975927, -0.96214756079864083, -0.19108928877904044, 0.8448342513903192, 1.2981652758413529,
    -0.044287324966195182, 1.2871548236016088, -1.0843495284799443, -0.25902034542131891, -0.52253405002951236,
    -0.7588245794009163, -0.32479109652523075),
  "validation/resultat/sous_groupes/attendus" =
    c(89.722583236461162, 504.7331117398889, 1072.5443050222093, 142.7948582355904, 792.96507399389986,
    1605.240068019765, 232.51744147205156, 1297.6981857337887, 2677.7843730419745, 1666.9999999985594,
    2541.0000002492552),
  "validation/resultat/sous_groupes/smr" =
    c(0.92507367717284739, 0.99458503578164814, 1.0088161346188818, 0.88238471298538379, 1.0202214782617569,
    1.0004734070594017, 0.89885730152901955, 1.0102503142968406, 1.0038149550280706, 1.0000000000008642,
    0.99999999990190669))
STATISTIQUES_R16_249 <- c(
  "khi2_pearson/statistique" = 117.06271655820485,
  "khi2_pearson/ddl" = 135.05247056110201,
  "khi2_pearson/p_asymptotique" = 0.8657080803219297,
  "deviance/statistique" = 124.12769862658533,
  "deviance/ddl" = 135.05247056110201,
  "deviance/p_asymptotique" = 0.73978837736347636,
  "smr/attendus" = 4208.0000002478146,
  "smr/smr" = 0.99999999994110866,
  "smr/ecart" = -2.4781456886557862e-07,
  "mape/valeur" = 22.185671242340039,
  "r2/valeur" = 0.95644139001072725,
  "distance_probabilites/valeur" = 0.29382628462063443)
APRES_R17_249 <- list(
  "lissage/resultat/segments/laml" =
    c(-8824.9101923078524, -12270.130115622425))
APRES_R16_249[["validation/resultat/cellules/qx_table"]] <- APRES_R16_249[["lissage/resultat/table_lissee/qx"]]
APRES_R16_249[["validation/parametres/ddl_effectifs/ddl_effectifs"]] <- APRES_R16_249[["lissage/resultat/segments/ddl_effectifs"]]
APRES_R16_249[["validation/parametres/ddl_effectifs/ddl_effectifs_retenus"]] <- APRES_R16_249[["lissage/resultat/segments/ddl_effectifs"]]
# Transition #249 du plan `id` : hypothèse (substitution unique), statuts du
# lissage (même déclaration que #242, `statut` : statut de la ligne ajoutée
# par #242), doubles de `apres` (vecteurs entiers) et, sous R16, lignes
# (statistique, grandeur) de la table statistiques de la validation
# (positions lues dans la référence).
transition_249 <- function(id, statut, apres, statistiques = NULL) {
  ref <- lire_reference(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv")))$valeurs
  h <- ref[[CHEMIN_HYPOTHESE_249]]
  if (length(h) != 1L || lengths(regmatches(h, gregexpr(SUBST_HYPOTHESE_249[1L], h, fixed = TRUE))) != 1L)
    stop(sprintf("transition_249(%s) : fragment de l'hypoth\u00e8se absent ou r\u00e9p\u00e9t\u00e9 (r\u00e9f\u00e9rence d\u00e9j\u00e0 r\u00e9g\u00e9n\u00e9r\u00e9e ?)", id))
  if (!"segments/laml" %in% ref[[paste0(STATUTS_242, "grandeur")]])
    stop(sprintf("transition_249(%s) : segments/laml absent des statuts de la r\u00e9f\u00e9rence (erreur de programmation)", id))
  h <- sub(SUBST_HYPOTHESE_249[1L], SUBST_HYPOTHESE_249[2L], h, fixed = TRUE)
  Encoding(h) <- "UTF-8"
  ch <- c(list(list(chemin = CHEMIN_HYPOTHESE_249, nature = "valeur de chaîne", valeurs = h),
               list(chemin = paste0(STATUTS_242, "statut"), nature = "valeur de chaîne",
                    valeurs = statuts_lissage_b39(ref, statut))),
          lapply(names(apres), function(c_) list(chemin = paste0("etapes/", c_), nature = "valeur", valeurs = apres[[c_]])))
  if (length(statistiques)) {
    st <- "etapes/validation/resultat/statistiques/"
    pos <- match(names(statistiques), paste0(ref[[paste0(st, "statistique")]], "/", ref[[paste0(st, "grandeur")]]))
    if (anyNA(pos)) stop(sprintf("transition_249(%s) : ligne de statistiques absente de la r\u00e9f\u00e9rence", id))
    ch <- c(ch, list(list(chemin = paste0(st, "valeur"), nature = "valeur", positions = pos,
                          valeurs = unname(statistiques))))
  }
  list(issue = "#249", plans = id, changements = ch)
}
# Transition #242 du plan `id` (statut de la ligne ajoutée : `statut`) :
# insertion après segments/ddl_effectifs (grandeur et statut des statuts,
# @colonnes de segments), @lignes des statuts augmenté de 1, remplacement de DDL_AVANT_242
# dans les deux statuts du ddl de la validation (positions lues dans la
# référence, deux exactement) ; statuts du lissage composés avec #249
# (statuts_lissage_b39() ; sous R15, wh_quadratique sans segments/laml,
# insertion seule).
transition_242 <- function(id, statut) {
  ref <- lire_reference(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv")))$valeurs
  apres <- function(v, avant, nouveau) {
    k <- match(avant, v)
    if (is.na(k)) stop(sprintf("transition_242(%s) : %s absent de la r\u00e9f\u00e9rence (erreur de programmation)", id, avant))
    append(v, nouveau, after = k)
  }
  gr <- ref[[paste0(STATUTS_242, "grandeur")]]
  k <- match("segments/ddl_effectifs", gr)
  st <- ref[[STAT_VALIDATION_242]]
  pos <- which(grepl(DDL_AVANT_242, st, fixed = TRUE, useBytes = TRUE))
  if (length(pos) != 2L) stop(sprintf("transition_242(%s) : deux statuts du ddl attendus, %d trouv\u00e9(s)", id, length(pos)))
  list(issue = "#242", plans = id, changements = list(
    list(chemin = paste0(SEGMENTS_242, "dimension_non_penalisee"), nature = "ajout", type = "integer", longueur = 2L),
    list(chemin = paste0(SEGMENTS_242, "@colonnes"), nature = "valeur de chaîne",
         valeurs = apres(ref[[paste0(SEGMENTS_242, "@colonnes")]], "ddl_effectifs", "dimension_non_penalisee")),
    list(chemin = paste0(STATUTS_242, "@lignes"), nature = "valeur", valeurs = ref[[paste0(STATUTS_242, "@lignes")]] + 1L),
    list(chemin = paste0(STATUTS_242, "grandeur"), nature = "valeur de chaîne",
         valeurs = append(gr, "segments/dimension_non_penalisee", after = k)),
    list(chemin = paste0(STATUTS_242, "statut"), nature = "valeur de chaîne",
         valeurs = statuts_lissage_b39(ref, statut)),
    list(chemin = STAT_VALIDATION_242, nature = "valeur de chaîne", positions = pos,
         valeurs = vapply(st[pos], function(x) sub(DDL_AVANT_242, DDL_APRES_242, x, fixed = TRUE), "",
                          USE.NAMES = FALSE))))
}
TRANSITIONS_EN_COURS <- c(TRANSITIONS_EN_COURS, list(
  transition_242("R15", STATUT_DIM_242_A), transition_242("R16", STATUT_DIM_242_B), transition_242("R17", STATUT_DIM_242_B),
  transition_249("R16", STATUT_DIM_242_B, APRES_R16_249, STATISTIQUES_R16_249),
  transition_249("R17", STATUT_DIM_242_B, APRES_R17_249)))
# Transition #218 de la correction de theta_80 (B30, constat C1 de la revue
# finale, c35eb9d) close par la seconde régénération (R′) de R1 à R17
# (exécution 44698750703, sur c35eb9d) : transition_218c(), apres_218c() et
# les constantes *_218C retirées.
# Transitions #340 (motif de precision_insuffisante) et #331 (sens de l'erreur
# de la branche +) de B32, sur les deux statuts ecart_minimal de R11 et R12,
# closes par la régénération (R) de B32 (exécution 44894726135, sur 0a29756) :
# transition_340(), transition_331(), apres_340(), apres_331(),
# statuts_apres_b32(), substituer_b32() et les constantes *_340, *_331 et
# CHEMIN_STATUTS_B32 retirées.
# Transitions #334 (V réduite sans soustraction : statuts de statistique,
# p_exacte et z) et #333 (cause « part arrondie à 0 » des deux statuts
# ecart_minimal) de B33, sur R11 et R12, closes par la régénération (R) de
# B33 (exécution 45021813451, sur 5aa23d7) : transition_334(),
# transition_333(), apres_334() et les constantes SUBST_334, SUBST_333 et
# CHEMIN_STATUTS_334 retirées.
# Transitions #330 (conséquence de la mention d'indépendance adaptée à la
# nature de la grandeur : statuts de validation/resultat/statistiques de R8,
# R9 et R10, et de statuts_sous_groupes/bornes_exactes de R10) et #329
# (mention d'indépendance en fin de statut des deux bornes de crédibilité de
# wh_poisson, R16 et R17) de B35, closes par la régénération (R) de B35
# (exécution 45258747737, sur cac4b84) : transition_330(), apres_330(),
# transition_329() et les constantes M_330, CONSEQUENCES_330,
# NATURES_STATISTIQUES_330 et GRANDEURS_329 retirées.
# Transitions #359 (mention d'indépendance qualifiée, résultat asymptotique, et
# source unique mention_independance() : textes de statut de R1 à R17, 278
# chaînes) et #361 (mention d'unité de l'hypothèse nulle de la validation,
# R8, R9 et R10) closes par la régénération (R) de B37 (exécution
# 45496372907, sur 8bc0117) : transition_359(), apres_359(), transition_361(),
# apres_361() et les constantes P_359, B_359, R_359, D_359, TESTS_359,
# SUBST_359, CHEMINS_359, SUBST_361 et CHEMIN_361 retirées.
# Transitions #360 (état non calculable attendus_hors_plage du comparateur 2 de
# importance_exclusions, R8, R9 et R10), #359 (conséquence de la mention
# d'indépendance des deux bornes de crédibilité de wh_poisson, nature
# credibilite, R16 et R17) et #361 (fin de l'avertissement unite_agregat alignée
# sur la qualification de la mention d'indépendance, R13 et R17) de la revue
# finale de B37 closes par la régénération (R′) de B37 (exécution 45591042044,
# sur 2158990) : transition_360b(), transition_359b(), transition_361b() et les
# constantes SUBST_360B, SUBST_359B, GRANDEURS_359B, SUBST_361B et CHEMINS_361B
# retirées.

# Transition #359 (nature contraste_segments de la mention d'indépendance de
# segments/z de l'homogénéité, R11 et R12 ; validation de la documentation de
# B37, étape (9), décision du mainteneur du 25/11/2026) close par la
# régénération (R″) de B37 (exécution 45607820898, sur 8094e39) :
# transition_359c() et la constante SUBST_359C retirées.

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
  # Élément non liste (ex. list("#35")) : issue NA ici, problème nommé par la
  # boucle (jamais t$issue sur un vecteur atomique, erreur R brute).
  issues <- vapply(transitions, function(t)
    if (is.list(t) && is.character(t[["issue"]]) && length(t[["issue"]]) == 1L) t[["issue"]] else NA_character_, "")
  for (k in seq_along(transitions)) {
    t <- transitions[[k]]; e <- sprintf("transition %d : ", k)
    if (!is.list(t) || !identical(sort(names(t)), c("changements", "issue", "plans"))) {
      p <- c(p, paste0(e, "champs issue, plans et changements exigés, et eux seuls")); next
    }
    if (is.na(issues[k]) || !grepl(MOTIF_ISSUE, issues[k])) p <- c(p, paste0(e, "issue de la forme \"#N\" exigée"))
    else {
      # Une issue peut porter plusieurs transitions, à plans deux à deux
      # disjoints (ADR 0009, quatrième annotation du 19/10/2026, (b)) ; un plan
      # commun à deux transitions d'une même issue est refusé.
      autres <- setdiff(which(issues == issues[k]), k)
      communs <- unique(unlist(lapply(transitions[autres], function(u) if (is.list(u)) intersect(u$plans, t$plans))))
      if (length(communs))
        p <- c(p, paste0(e, "issue ", issues[k], " : deux transitions de cette issue sur le(s) même(s) plan(s) ",
                         paste(communs, collapse = ", ")))
    }
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
                                   paste(ch$de, ch$vers) %in% c("double integer", "integer double", "logical character")))
        p <- c(p, paste0(e2, "type : de et vers parmi double -> integer, integer -> double ou logical -> character"))
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
      if (identical(ch$de, "logical")) {
        # logical -> character : seul le vecteur vide est sans valeur à convertir.
        if (length(v)) stop(sprintf("%s : conversion logical -> character d'un vecteur non vide refusée (%d valeur(s))", c_, length(v)))
        w <- character(0)
      } else {
        w <- if (ch$vers == "integer") suppressWarnings(as.integer(v)) else as.double(v)
        if (!identical(if (ch$de == "double") as.double(w) else as.integer(w), v))
          stop(sprintf("%s : conversion %s -> %s avec perte", c_, ch$de, ch$vers))
      }
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
# `constats` : chemins de constat du plan en régime de constat (ADR 0009,
# annotation du 26/11/2026, (f) 5), vide par défaut (strict).
seul_ecart <- function(ref, g, changements, constats = character(0)) {
  r <- tryCatch(aligner_reference(ref, g, changements), error = function(e) e)
  if (inherits(r, "error")) return(conditionMessage(r))
  if (comparer_references(r, g, constats = constats)$ok) TRUE
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
# la garde. `constats` : chemins de constat du plan, passés à seul_ecart() en
# régime de constat (vide : strict).
controler_plan <- function(id, nom, ref, g, strict, transitions, donnees = NULL, roles = NULL, constats = character(0)) {
  t <- changements_du_plan(id, transitions)
  if (!length(t$changements)) return(verifier(nom, strict))
  chemins <- vapply(t$changements, `[[`, "", "chemin")
  verifier(sprintf("%s : seul écart à tests/reference/%s.tsv : %s (%s)", id, id,
                   paste(vapply(t$changements, decrire_changement, ""), collapse = " ; "),
                   paste(unique(t$issues), collapse = ", ")),
           seul_ecart(ref, g, t$changements, constats))
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

# Oracle de l'empreinte du résultat (#71) : empreinte_resultat(), texte_objet()
# et ecrire_valeurs() du package, réévalués dans un environnement où les
# écrivains des entiers et des dates sont ceux d'avant #71 (R/rapport.R à
# 5a95f9c : octets_lignes(sprintf("%d", x)) et octets_lignes(txt[match(j,
# u)])). Les références ne comparent que le jeton de l'empreinte (ADR 0009,
# seconde annotation du 20/10/2026) : l'égalité à l'oracle en garde la valeur,
# sans valeur figée propre à une machine (ADR 0011, point 11.2). R1 à R12 ne
# portent aucun entier négatif (les dates s'écrivent en ISO, sans signe) : le
# signe des entiers est gardé par les tests unitaires des écrivains, non par
# cet oracle.
oracle_empreinte_resultat <- local({
  ns <- asNamespace("tablexp")
  e <- new.env(parent = ns)
  e$octets_entiers <- function(x) ns$octets_lignes(sprintf("%d", x))
  e$octets_lignes_indices <- function(textes, i) ns$octets_lignes(textes[i])
  for (f in c("ecrire_valeurs", "texte_objet", "empreinte_resultat")) {
    g <- get(f, envir = ns)
    environment(g) <- e
    assign(f, g, envir = e)
  }
  e$empreinte_resultat
})

# Rapport figé (#87, U24) : contrôles du document de R1 à R12 à 10^5 lignes
# (spécification d'actuary du 24/10/2026, U14, U18, U21, U22, repris de
# tests/unitaires/test_rapport.R) ; rend TRUE ou la liste des écarts.
controler_rapport_fige <- function(x, l, id) {
  html <- function(s) {
    for (r in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c("\"", "&quot;"))) s <- gsub(r[1L], r[2L], s, fixed = TRUE)
    s
  }
  section_de <- function(id) {
    d <- match(paste0("<section id=\"", id, "\">"), l)
    if (is.na(d)) character(0) else l[d:(d - 1L + match("</section>", l[d:length(l)]))]
  }
  S <- tablexp:::SECTIONS_RAPPORT
  e <- character(0)
  # U14 : sections et nav dans l'ordre de SECTIONS_RAPPORT.
  sec <- sub("^<section id=\"([a-z_]+)\">$", "\\1", grep("^<section id=", l, value = TRUE))
  nav <- sub("^<li><a href=\"#([a-z_]+)\">.*$", "\\1", grep("^<li><a href=\"#", l, value = TRUE))
  if (!identical(sec, S$section) || !identical(nav, S$section)) e <- c(e, "U14 : sections ou nav")
  # U18 : cellules décimales égales à l'écriture « texte » de l'écrivain
  # unique du moteur (ecrire_valeurs(), #104) : U18 vérifie que les cellules
  # passent par cet écrivain ; son exactitude (%.17g correctement arrondi)
  # relève des témoins T1 à T3 de la batterie unitaire et du contrôle
  # tests/controles/ecriture_doubles_texte.R. Aucune condition de plateforme.
  td <- unlist(regmatches(l, gregexpr("<td>[^<]*(<br>[^<]*)*</td>", l)))
  c_ <- unlist(strsplit(sub("^<td>(.*)</td>$", "\\1", td), "<br>", fixed = TRUE))
  d <- c_[grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", c_)]
  ecrit <- if (length(d)) strsplit(rawToChar(tablexp:::ecrire_valeurs(as.numeric(d), "texte", "x")$octets), "\n",
                                   fixed = TRUE)[[1L]]
  if (!length(d) || any(d != ecrit)) e <- c(e, "U18 : cellule décimale hors de l'écriture du moteur")
  if (id == "R2") { if (!any(grepl(">0.98999999999999999<", l, fixed = TRUE))) e <- c(e, "U18 : niveau 0,99") }
  else if (!any(grepl(">0.94999999999999996<", l, fixed = TRUE)) || any(grepl(">0.95<", l, fixed = TRUE)))
    e <- c(e, "U18 : niveau 0,95")
  # U21 : textes fixes, plateforme, rejeu, sections 12 et 13.
  if (sum(grepl("L'empreinte du r\u00e9sultat d\u00e9pend de la plateforme", l, fixed = TRUE)) != 1L) e <- c(e, "U21 : plateforme")
  if (!any(grepl("Ce rapport ne porte pas de date", l, fixed = TRUE))) e <- c(e, "U21 : pas de date")
  ct <- tablexp:::CONVENTIONS_TEXTE
  if (!all(paste0("<li>", html(names(ct)), " : ", html(unname(ct)), "</li>") %in% l)) e <- c(e, "U21 : conventions")
  ident <- section_de("identification")
  if (sum(grepl("^<li>(algorithme|donn\u00e9es|fichier|r\u00e9sultat et code) ", ident)) != 4L) e <- c(e, "U21 : empreintes")
  if (!all(vapply(names(x$fiche$plateforme), function(n) any(startsWith(ident, paste0("<tr><td>fiche/plateforme/", n, "</td>"))), NA)) ||
      !"bibliotheque_c" %in% names(x$fiche$plateforme)) e <- c(e, "U21 : fiche/plateforme")
  integ <- grep("^<p>Contr\u00f4le d'int\u00e9grit\u00e9 : ", ident, value = TRUE)
  hors <- paste(html(setdiff(tablexp:::CHEMINS_HORS_RESULTAT, c("execution", "fiche/empreintes/resultat"))), collapse = ", ")
  if (length(integ) != 1L ||
      !grepl(html("trouv\u00e9e \u00e9gale \u00e0 fiche/empreintes/resultat, la valeur consign\u00e9e \u00e0 laquelle ce contr\u00f4le la compare (exclue du texte empreint\u00e9, puisqu'elle en est l'empreinte). Hors du p\u00e9rim\u00e8tre"), integ, fixed = TRUE) ||
      !grepl(paste0("de ce contr\u00f4le : ", hors, " ; ce que le rapport en restitue (valeurs, ou structure seule pour une table dont le volume cro\u00eet avec la base) est lu dans l'objet, sans v\u00e9rification de son origine.",
                      " Avant l'\u00e9criture sont contr\u00f4l\u00e9es : pour etapes/donnees/resultat/lignes_conservees, copie des lignes conserv\u00e9es de l'entr\u00e9e, sa coh\u00e9rence avec le p\u00e9rim\u00e8tre de l'empreinte",
                      " (un data.frame dont le nombre de lignes est la longueur de etapes/donnees/resultat/numeros_conserves et qui porte les colonnes nomm\u00e9es par fiche/roles)",
                      " ; pour les autres chemins, leur forme, soumise \u00e0 l'\u00e9crivain de la fiche (listes et vecteurs sans classe ni attribut autre que les noms, ou dates).</p>"), integ, fixed = TRUE))
    e <- c(e, "U21 : phrase du contrôle d'intégrité")
  rj <- section_de("rejeu")
  if (!any(grepl("rejouer_fiche(", rj, fixed = TRUE)) || !any(grepl(html(tablexp:::TEXTE_REJEU_FICHE), rj, fixed = TRUE)))
    e <- c(e, "U21 : rejeu")
  # Étape validation demandée (R8, #166) : le paragraphe « non demandée par le
  # plan » est absent, la section porte la légende des cellules et des
  # statistiques, et l'étape importance_exclusions, si le plan la demande
  # (R8 à R10 ; pas R15 à R17), a sa section.
  valid <- !is.null(x$etapes$validation)
  importance <- !is.null(x$etapes$importance_exclusions)
  if (any(grepl("<p>\u00c9tape \u00ab validation \u00bb : non demand\u00e9e par le plan", l, fixed = TRUE)) == valid ||
      !any(grepl("<p>Table finale et comparaisons : non produites par cette version", l, fixed = TRUE))) e <- c(e, "U21 : sections 12 et 13")
  legende <- function(sec, chemin) any(startsWith(section_de(sec), paste0("<caption><code>", chemin, "</code>")))
  if (valid && !(legende("etape_validation", "etapes/validation/resultat/cellules") &&
                 legende("etape_validation", "etapes/validation/resultat/statistiques") &&
                 (!importance || legende("etape_importance_exclusions", "etapes/importance_exclusions/resultat/retraits"))))
    e <- c(e, "U21 : sections validation et importance_exclusions")
  # U22 : section brut.
  b <- section_de("etape_brut")
  st <- grep("<caption><code>etapes/brut/parametres/statuts</code>", b, fixed = TRUE)
  re <- grep("<caption><code>etapes/brut/resultat/", b, fixed = TRUE)
  if (length(st) != 1L || !length(re) || st > re[1L]) e <- c(e, "U22 : ordre des légendes")
  if (any(startsWith(b, "<p>Intervalles binomiaux (colonnes borne_inf_exacte")) != ("binomial" %in% x$etapes$brut$parametres$estimateurs))
    e <- c(e, "U22 : légende binomiale")
  if (id %in% c("R6", "R7") && !any(grepl("^<tr><td>etapes/brut/resultat/kaplan_meier_sauts</td><td>data.frame</td><td>[0-9]+</td><td>non reproduites", b)))
    e <- c(e, "U22 : kaplan_meier_sauts")
  if (length(e)) e else TRUE
}

# Une entrée par plan (`entree`, ADR 0009, point 4.1 amendé), chaque entrée
# distincte tirée une seule fois ; jeu_ref est l'entrée `reference` (R1 à R8,
# R10 à R12, R15, R16), que les contrôles propres à un plan de cette entrée
# (permutation, R6) lisent.
entrees_ref <- entrees_des_plans(plans_reference())
jeu_ref <- entrees_ref[["reference"]]
verifier("entrées des plans : R1 à R8, R10 à R12, R15, R16 sur `reference`, R9 sur `reference_defauts`, R13 et R17 sur `agregats_reference`, R14 sur `stocks_reference` ; une entrée tirée par nom distinct ; `reference` identical() à jeu_reference()", {
  e <- unname(vapply(plans_reference(), function(p) p$entree, ""))
  attendu <- c(rep("reference", 8L), "reference_defauts", rep("reference", 3L), "agregats_reference", "stocks_reference",
               rep("reference", 2L), "agregats_reference")
  if (!identical(e, attendu))
    paste0("entrées des plans obtenues (", paste(e, collapse = ", "), ") contre attendues (", paste(attendu, collapse = ", "), ")")
  else if (!identical(names(entrees_ref), c("reference", "reference_defauts", "agregats_reference", "stocks_reference")))
    paste0("entrées tirées obtenues (", paste(names(entrees_ref), collapse = ", "), ") contre attendues (reference, reference_defauts, agregats_reference, stocks_reference)")
  else if (!identical(jeu_ref, jeu_reference())) "entrée `reference` différente de jeu_reference()"
  else TRUE
})
# Entrée de R13 (#169 ; spécification (0') de #169, § 12) : forme agrégats de
# simuler_reference(), même graine que jeu_reference() ; attribut `simulation`
# celui du jeu propre plus le champ `forme` en fin ; colonnes aux noms des rôles.
verifier("R13, entrée : forme agrégats de l'entrée de référence (colonnes sexe, age, exposition_centrale, exposition_initiale, evenements ; attribut simulation de jeu_reference() plus forme en fin ; événements = décès de jeu_reference() dans la période)", {
  a13 <- entrees_ref[["agregats_reference"]]$donnees
  s13 <- attr(a13, "simulation"); s0 <- attr(jeu_ref$donnees, "simulation")
  dc <- jeu_ref$donnees$cause_sortie == "deces" & jeu_ref$donnees$date_sortie >= PERIODE_REFERENCE$debut &
    jeu_ref$donnees$date_sortie <= PERIODE_REFERENCE$fin
  if (!identical(names(a13), unname(ROLES_AGREGATS_R13))) paste("colonnes :", paste(names(a13), collapse = ", "))
  else if (!identical(names(s13), c(names(s0), "forme")) || !identical(s13[names(s0)], s0)) "attribut simulation différent de celui du jeu propre plus forme"
  else if (!identical(s13$forme, FORME_AGREGATS_R13)) "champ forme différent de FORME_AGREGATS_R13"
  else if (sum(a13$evenements) != sum(dc, na.rm = TRUE)) "total des événements différent des décès de la période"
  else TRUE
})
# Entrée de R14 (#180 ; spécification de #180, § 13) : forme stocks de
# simuler_reference(), même graine que jeu_reference() ; attribut
# `simulation` celui du jeu propre plus le champ `forme` en fin (nom, grille,
# décomptes) ; colonnes de la spécification, § 11 ; décomptes de lignes
# remesurés sur la table ; lignes d'événement = sorties du jeu propre dans
# ]t_1, t_M].
verifier("R14, entrée : forme stocks de l'entrée de référence (colonnes id_contrat, id_tete, type_ligne, date_inventaire, sexe, date_naissance, date_sortie, cause_sortie ; attribut simulation de jeu_reference() plus forme en fin ; lignes d'événement = sorties de jeu_reference() dans la période)", {
  a14 <- entrees_ref[["stocks_reference"]]$donnees
  s14 <- attr(a14, "simulation"); s0 <- attr(jeu_ref$donnees, "simulation")
  j0 <- jeu_ref$donnees
  dans <- !is.na(j0$date_sortie) & j0$date_sortie >= PERIODE_REFERENCE$debut & j0$date_sortie <= PERIODE_REFERENCE$fin
  if (!identical(names(a14), c("id_contrat", "id_tete", "type_ligne", "date_inventaire", "sexe", "date_naissance",
                               "date_sortie", "cause_sortie"))) paste("colonnes :", paste(names(a14), collapse = ", "))
  else if (!identical(names(s14), c(names(s0), "forme")) || !identical(s14[names(s0)], s0)) "attribut simulation différent de celui du jeu propre plus forme"
  else if (!identical(s14$forme[c("nom", "inventaires")], FORME_STOCKS_R14)) "champ forme (nom, inventaires) différent de FORME_STOCKS_R14"
  else if (!identical(unname(s14$forme$decomptes$lignes), c(sum(a14$type_ligne == "inventaire"), sum(a14$type_ligne == "evenement"))))
    "décomptes de lignes différents de la table"
  else if (sum(a14$type_ligne == "evenement") != sum(dans)) "lignes d'événement différentes des sorties de la période"
  else TRUE
})
verifier("entrées des plans : tirer_entree() d'un nom inconnu -> erreur",
         inherits(tryCatch(tirer_entree("inconnue"), error = function(e) e), "error"))
# Refus nommé d'un plan sans champ `entree` exploitable (pas de valeur par
# défaut). Le message se compare sur sa partie ASCII : sous une locale non UTF-8,
# stop() rend « î » en « <U+00EE> ».
verifier("entrées des plans : entrees_des_plans() d'un plan sans champ entree, ou d'un champ non chaîne -> refus nommant le plan", {
  msg <- function(plans) tryCatch({ entrees_des_plans(plans); "aucune erreur" }, error = function(e) conditionMessage(e))
  debut <- "plan X : champ entree absent ou non cha"
  obt <- c(sans = msg(list(list(identifiant = "X"))),
           nul = msg(list(list(identifiant = "X", entree = NULL))),
           numerique = msg(list(list(identifiant = "X", entree = 1))),
           vecteur = msg(list(list(identifiant = "X", entree = c("reference", "reference")))),
           na = msg(list(list(identifiant = "X", entree = NA_character_))),
           sans_identifiant = msg(list(list(entree = NULL))))
  attendu_debut <- ifelse(names(obt) == "sans_identifiant", "plan (sans identifiant) : champ entree absent ou non cha", debut)
  mal <- !startsWith(obt, attendu_debut)
  if (!any(mal)) TRUE
  else paste0("messages obtenus (", paste(names(obt)[mal], ": ", obt[mal], collapse = " ; "), ") contre attendus (", paste(unique(attendu_debut), collapse = " ; "), "...)")
})
# Entrée de R9 : le jeu de jeu_reference() altéré après son tirage complet. Les
# lignes que les défauts n'altèrent pas sont celles de jeu_reference() (même
# loi, même portefeuille, même graine du jeu) ; les défauts sont ceux déclarés
# (graine des défauts, nombre fixe par motif, un seul motif par ligne).
verifier("R9, entrée : défauts de graine GRAINE_DEFAUTS_R9, NOMBRE_PAR_MOTIF_R9 lignes par motif de MOTIFS_DEFAUTS_R9, lignes non altérées identical() à jeu_reference()", {
  d9 <- entrees_ref[["reference_defauts"]]$donnees
  da <- attr(d9, "defauts")
  l <- da$lignes
  nus <- function(d) { attributes(d) <- attributes(d)[c("names", "class", "row.names")]; d }
  alt <- unique(l$ligne)
  if (is.null(da)) "attribut defauts absent"
  else if (!identical(da$graine, GRAINE_DEFAUTS_R9)) "graine des défauts différente de GRAINE_DEFAUTS_R9"
  else if (!identical(sort(unique(l$defaut)), sort(MOTIFS_DEFAUTS_R9)) ||
           !all(table(l$defaut) == NOMBRE_PAR_MOTIF_R9)) "motifs ou effectifs par motif différents des déclarés"
  else if (length(alt) != length(MOTIFS_DEFAUTS_R9) * NOMBRE_PAR_MOTIF_R9) "une ligne porte plusieurs motifs"
  else if (nrow(d9) != nrow(jeu_ref$donnees)) "nombre de lignes différent de jeu_reference()"
  else if (!identical(nus(d9)[-alt, ], nus(jeu_ref$donnees)[-alt, ])) "lignes non altérées différentes de jeu_reference()"
  else TRUE
})
# Invariants de structure de TOLERANCES (#175), fonctions des chemins doubles
# et d'une table de clés (témoins plus bas) ; chacune rend les chemins en défaut.
# Les clés qui classent un chemin sont toutes celles de cles_de() : une clé plus
# courte que la clé qualifiée est relevée, même si elle ne l'emporte pas.
REGEX_CHEMIN_ETAPE <- "^(fiche/plan/)?etapes/[^/]+/"
cles_nues <- function(tolerances = TOLERANCES) names(tolerances)[!grepl("/", names(tolerances), fixed = TRUE)]
doubles_etape_mal_classes <- function(doubles, tolerances = TOLERANCES) {
  d <- doubles[grepl(REGEX_CHEMIN_ETAPE, doubles)]
  etape <- sub("/.*$", "", sub("^(fiche/plan/)?etapes/", "", d))
  d[vapply(seq_along(d), function(i) {
    k <- cles_de(d[i], tolerances); !length(k) || !all(startsWith(k, paste0(etape[i], "/"))) }, NA)]
}
doubles_fiche_mal_classes <- function(doubles, tolerances = TOLERANCES) {
  d <- doubles[startsWith(doubles, "fiche/") & !grepl(REGEX_CHEMIN_ETAPE, doubles)]
  d[vapply(d, function(c_) { k <- cles_de(c_, tolerances); !length(k) || !all(startsWith(k, "fiche/")) }, NA)]
}
doubles_multi_cles <- function(doubles, tolerances = TOLERANCES)
  doubles[vapply(doubles, function(c_) length(cles_de(c_, tolerances)) > 1L, NA)]
verifier("TOLERANCES : toute tolérance est une classe de CLASSES_TOLERANCE (#168)", {
  hors <- names(TOLERANCES)[!(TOLERANCES %in% CLASSES_TOLERANCE)]
  if (length(hors)) paste("tolérances hors classe :", paste(hors, collapse = ", ")) else TRUE
})
verifier("TOLERANCES : aucune clé nue, sans « / » (point 3.4, #175)", {
  nues <- cles_nues()
  if (length(nues)) paste("clés nues :", paste(nues, collapse = ", ")) else TRUE
})
# Dénombrements en integer, sans clé (ADR 0009, seconde annotation du
# 10/11/2026, (e) ; #199) : les 27 clés exactes des chemins de CHEMINS_199
# sont retirées ; 148 clés, dont 41 exactes, restaient. Étape (R) de B19
# (R15 à R17) : 21 clés de l'annotation du 12/11/2026, (e) (6 exactes) et 22
# clés classées par actuary (13 exactes : 5 échos de lissage et 8 clés de
# coherence_regularite ; avis du 12/11/2026) : 191 clés, dont 60 exactes.
# #218 : cinq clés ajoutées (trois constantes exactes, deux colonnes theta_80
# à 1e-10) : 191 et 60 -> 196 et 63.
# #243 (ADR 0009, annotation du 26/11/2026, (d) 2) : trois clés de
# coherence_regularite reclassées de 1e-14 à 1e-10, aucune clé ajoutée ni
# retirée : 196 clés, exacte 63, 1e-14 39, 1e-12 75, 1e-10 8, optimisation 11.
verifier("TOLERANCES : 27 clés des dénombrements de #199 retirées, 196 clés dont 63 exactes, 39 à 1e-14, 75 à 1e-12, 8 à 1e-10, 11 en optimisation (ADR 0009, point 9.6, (e) ; annotation du 12/11/2026, (e) ; #218 ; annotation du 26/11/2026, (d) 2)", {
  cles_199 <- sub("^etapes/", "", unlist(CHEMINS_199, use.names = FALSE))
  restantes <- intersect(cles_199, names(TOLERANCES))
  par_classe <- vapply(CLASSES_TOLERANCE, function(v) sum(TOLERANCES == v), 0L)
  attendu_ <- c(exacte = 63L, "1e-14" = 39L, "1e-12" = 75L, "1e-10" = 8L, optimisation = 11L)
  if (length(cles_199) != 27L || anyDuplicated(cles_199)) "CHEMINS_199 : 27 chemins distincts attendus"
  else if (length(restantes)) paste("clés encore présentes :", paste(restantes, collapse = ", "))
  else if (length(TOLERANCES) != 196L || anyDuplicated(names(TOLERANCES)) || !identical(par_classe, attendu_))
    sprintf("%d clés (%s), au lieu de 196 (%s)", length(TOLERANCES), paste(names(par_classe), par_classe, collapse = ", "),
            paste(names(attendu_), attendu_, collapse = ", "))
  else if (!all(TOLERANCES[paste0("coherence_regularite/resultat/regularite/", c("regularite", "r_estime", "regularite_r"))] == 1e-10))
    "coherence_regularite/resultat/regularite/{regularite, r_estime, regularite_r} : classe 1e-10 attendue"
  else TRUE
})
# Règle absolue à une échelle déclarée (ADR 0009, annotation du 26/11/2026,
# (b) 4 ; #243) : invariants de la déclaration, indépendants de toute donnée,
# sauf (v), qui lit les fichiers commités. Rend les défauts (character(0) si
# conforme) ; `references` : liste nommée des références lues.
REFERENCES_LUES <- local({
  f <- list.files(DOSSIER_REFERENCES, pattern = "\\.tsv$", full.names = TRUE)
  stats::setNames(lapply(f, lire_reference), sub("\\.tsv$", "", basename(f)))
})
defauts_echelles <- function(echelles = ECHELLES_ABSOLUES, tolerances = TOLERANCES, references = REFERENCES_LUES,
                             classes = CLASSES_ABSOLUES) {
  d <- defauts_forme_echelles(echelles, classes)
  if (length(d) && startsWith(d[1L], "table :")) return(d)
  hors <- echelles$cle[!echelles$cle %in% names(tolerances)]
  if (length(hors)) d <- c(d, sprintf("cle %s absente de TOLERANCES (i)", hors))
  doubles <- unique(unlist(lapply(references, function(r) names(r$types)[r$types == "double"]), use.names = FALSE))
  mortes <- echelles$cle[!vapply(echelles$cle, function(k) any(cle_couvre(k, doubles)), NA)]
  if (length(mortes)) d <- c(d, sprintf("déclaration morte : %s ne couvre aucun chemin double des références (v)", mortes))
  d
}
CLES_ECHELLES_ATTENDUES <- c("lissage/resultat/segments/residu_conservation_0", "lissage/resultat/segments/residu_conservation_1",
                             "validation/resultat/statistiques/valeur", "coherence_regularite/resultat/ages/delta1",
                             "coherence_regularite/resultat/ages/delta2", "coherence_regularite/resultat/ordre_segments/ecart")
verifier("ECHELLES_ABSOLUES : invariants (i) à (v) de la déclaration (clés de TOLERANCES, classes de CLASSES_ABSOLUES, facteurs finis > 0, couples (cle, selection) uniques et grammaire, aucune déclaration morte) (ADR 0009, annotation du 26/11/2026, (b) 4)", {
  d <- defauts_echelles()
  if (length(d)) d else TRUE
})
verifier("ECHELLES_ABSOLUES : (vi) six lignes, clés fixées ; CLASSES_ABSOLUES : deux classes, abs_1e-10 = 1e-10 et abs_1e-12 = 1e-12, distinctes de CLASSES_TOLERANCE (ADR 0009, annotation du 26/11/2026, (b) 1 et 4)", {
  if (nrow(ECHELLES_ABSOLUES) != 6L || !identical(sort(ECHELLES_ABSOLUES$cle), sort(CLES_ECHELLES_ATTENDUES)))
    sprintf("%d ligne(s), clés : %s", nrow(ECHELLES_ABSOLUES), paste(ECHELLES_ABSOLUES$cle, collapse = ", "))
  else if (!identical(CLASSES_ABSOLUES, c("abs_1e-10" = 1e-10, "abs_1e-12" = 1e-12))) "CLASSES_ABSOLUES différente de la déclaration"
  else if (any(names(CLASSES_ABSOLUES) %in% names(CLASSES_TOLERANCE))) "nom de classe absolue commun avec CLASSES_TOLERANCE"
  else if (!identical(ECHELLES_ABSOLUES$facteur[match(CLES_ECHELLES_ATTENDUES, ECHELLES_ABSOLUES$cle)], c(1, 120, 1, 2, 4, 2)) ||
           !identical(ECHELLES_ABSOLUES$classe[match(CLES_ECHELLES_ATTENDUES, ECHELLES_ABSOLUES$cle)], rep(c("abs_1e-10", "abs_1e-12"), each = 3L)))
    "facteurs ou classes différents de l'annotation du 26/11/2026, (b) 2"
  else TRUE
})
verifier("ECHELLES_ABSOLUES : (b) 5, échelles valides du côté de la référence dans chaque fichier commité qui porte un chemin couvert (aucune déclaration d'échelle invalide)", {
  inv <- unlist(lapply(names(REFERENCES_LUES), function(id) {
    r <- REFERENCES_LUES[[id]]
    ch <- names(r$types)[r$types == "double"]
    unlist(lapply(ch, function(c_) { e <- echelle_de(c_, r); if (!is.null(e) && length(e$invalide)) paste(id, c_, e$invalide) }))
  }))
  if (length(inv)) inv else TRUE
})
# Constat hors plateforme de référence (annotation du 26/11/2026, (f) 2) :
# invariants de CONSTATS_HORS_PLATEFORME, dont la complétude pour R16 et R17.
defauts_constats <- function(constats = CONSTATS_HORS_PLATEFORME, references = REFERENCES_LUES,
                             hors_table = VALIDATION_HORS_TABLE_LISSEE) {
  if (!is.data.frame(constats) || !identical(names(constats), c("plan", "chemin")) ||
      !identical(vapply(constats, typeof, ""), c(plan = "character", chemin = "character")))
    return("table : data.frame de colonnes plan et chemin (character) exigé")
  d <- character(0)
  inconnus <- setdiff(constats$plan, names(plans_reference()))
  if (length(inconnus)) d <- c(d, sprintf("plan %s hors de plans_reference()", inconnus))
  if (anyDuplicated(constats)) d <- c(d, "ligne (plan, chemin) répétée")
  for (i in seq_len(nrow(constats))) {
    r <- references[[constats$plan[i]]]
    if (is.null(r) || !identical(unname(r$types[constats$chemin[i]]), "double"))
      d <- c(d, sprintf("%s : %s n'est pas un chemin double du fichier du plan", constats$plan[i], constats$chemin[i]))
  }
  for (id in c("R16", "R17")) {
    r <- references[[id]]
    if (is.null(r)) { d <- c(d, sprintf("%s : référence absente", id)); next }
    ch <- names(r$types)[r$types == "double" & startsWith(names(r$types), "etapes/validation/")]
    finis <- ch[vapply(ch, function(c_) any(is.finite(r$valeurs[[c_]])), NA)]
    non_classes <- setdiff(finis, c(constats$chemin[constats$plan == id], hors_table))
    if (length(non_classes))
      d <- c(d, sprintf("%s : double fini de etapes/validation/ ni déclaré ni indépendant de la table lissée : %s", id, non_classes))
  }
  d
}
verifier("CONSTATS_HORS_PLATEFORME : plans de plans_reference(), chemins double du fichier de leur plan, complétude pour R16 et R17 (tout double fini de etapes/validation/ déclaré ou dans VALIDATION_HORS_TABLE_LISSEE) (ADR 0009, annotation du 26/11/2026, (f) 2)", {
  d <- defauts_constats()
  if (length(d)) d else TRUE
})
verifier("CONSTATS_HORS_PLATEFORME : plans R15, R16, R17, vingt-six lignes (4, 11, 11), chemins de R16 et R17 identiques ; VALIDATION_HORS_TABLE_LISSEE : onze chemins, disjoints des déclarés (ADR 0009, annotation du 26/11/2026, (f) 2)", {
  n <- table(CONSTATS_HORS_PLATEFORME$plan)
  if (nrow(CONSTATS_HORS_PLATEFORME) != 26L || !identical(names(n), c("R15", "R16", "R17")) || !identical(as.integer(n), c(4L, 11L, 11L)))
    sprintf("%d ligne(s), plans %s", nrow(CONSTATS_HORS_PLATEFORME), paste(names(n), n, collapse = ", "))
  else if (!identical(CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == "R16"],
                      CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == "R17"]))
    "chemins de R16 et de R17 différents"
  else if (length(VALIDATION_HORS_TABLE_LISSEE) != 11L || anyDuplicated(VALIDATION_HORS_TABLE_LISSEE) ||
           length(intersect(VALIDATION_HORS_TABLE_LISSEE, CONSTATS_HORS_PLATEFORME$chemin)))
    "VALIDATION_HORS_TABLE_LISSEE : onze chemins distincts, disjoints des chemins déclarés, attendus"
  else TRUE
})
verifier("CONSTATS_HORS_PLATEFORME, témoins : chemin absent, chemin non double, plan inconnu, double fini nouveau non classé de la validation de R16 -> défauts", {
  avec <- function(plan, chemin) rbind(CONSTATS_HORS_PLATEFORME, data.frame(plan = plan, chemin = chemin, stringsAsFactors = FALSE))
  r16 <- REFERENCES_LUES[["R16"]]
  nouveau <- "etapes/validation/resultat/cellules/grandeur_nouvelle"
  r16$types[[nouveau]] <- "double"; r16$valeurs[[nouveau]] <- c(1, NA)
  refs <- REFERENCES_LUES; refs[["R16"]] <- r16
  d1 <- defauts_constats(avec("R16", "etapes/validation/resultat/cellules/inexistant"))
  d2 <- defauts_constats(avec("R16", "etapes/validation/resultat/cellules/etat"))
  d3 <- defauts_constats(avec("R99", "etapes/validation/resultat/cellules/qx_table"))
  d4 <- defauts_constats(references = refs)
  d5 <- defauts_constats(hors_table = VALIDATION_HORS_TABLE_LISSEE[-1L])
  length(d1) == 1L && length(d2) == 1L && length(d3) == 2L && length(d4) == 1L && grepl(nouveau, d4, fixed = TRUE) &&
    length(d5) == 2L && all(grepl(VALIDATION_HORS_TABLE_LISSEE[1L], d5, fixed = TRUE))
})
# Contrôle des types des dénombrements (ADR 0009, point 9.6, (f) ; #199, M396,
# Q6) : tout chemin dont le dernier segment (nom de colonne ou de
# diagnostic, jamais un nom de table) est nommé comme un dénombrement est
# integer, dans chaque plan de référence, sauf la liste fermée
# EXCEPTIONS_DENOMBREMENT, dont chaque exception est annotée à l'ADR 0009.
MOTIF_DENOMBREMENT <- "^(observes|evenements.*|cellules.*|lignes.*|tetes.*|tirages_effectues|runs|positifs|negatifs)$"
EXCEPTIONS_DENOMBREMENT <- c(
  # (c) 1 : somme d'événements de cellules exclues, non gardées (M396, Q3 (b)).
  "etapes/donnees/diagnostics/releves/poids_exclusions/evenements")
denombrements_non_entiers <- function(g) {
  ch <- names(g$types)
  d <- ch[grepl(MOTIF_DENOMBREMENT, sub("^.*/", "", ch)) & !(ch %in% EXCEPTIONS_DENOMBREMENT)]
  d[g$types[d] != "integer"]
}
# Commit préparatoire (1a) de B19 (#223 ; ADR 0017, points 1.5 et 10.1) :
# apparier_table() appelle grille_plage() (module exposition). Ancien corps
# d'apparier_table(), recopié de R/positionnement.R à 993068c (garde d'étape
# retirée, sans effet sur la valeur rendue) : la valeur rendue doit être
# identical() à celle de l'appel du package (identité exacte).
apparier_table_993068c <- function(exposition, segmentation, par_annee, table, plage_ages) {
  cles_seg <- if (length(segmentation))
    do.call(paste, c(unname(as.list(exposition[segmentation])), list(sep = "\r"))) else rep("", nrow(exposition))
  premiers <- !duplicated(cles_seg)
  segments <- exposition[premiers, segmentation, drop = FALSE]
  rownames(segments) <- NULL
  g_expo <- match(cles_seg, cles_seg[premiers])
  n_seg <- sum(premiers)
  ages <- seq(as.integer(plage_ages[1L]), as.integer(plage_ages[2L]))
  annees <- if (isTRUE(par_annee)) sort(unique(exposition$annee)) else NA_integer_
  n_age <- length(ages); n_an <- length(annees)
  n <- n_seg * n_age * n_an
  g <- rep(seq_len(n_seg), each = n_age * n_an)
  cellules <- segments[g, , drop = FALSE]
  rownames(cellules) <- NULL
  cellules$age <- rep(rep(ages, each = n_an), times = n_seg)
  if (isTRUE(par_annee)) cellules$annee <- as.integer(rep(annees, times = n_seg * n_age))
  dans <- exposition$age >= plage_ages[1L] & exposition$age <= plage_ages[2L]
  i <- ((g_expo[dans] - 1) * n_age + (exposition$age[dans] - ages[1L])) * n_an +
    (if (isTRUE(par_annee)) match(exposition$annee[dans], annees) else 1)
  Ec <- numeric(n); D <- integer(n)
  Ec[i] <- exposition$exposition_centrale[dans]
  D[i] <- exposition$evenements[dans]
  cellules$exposition_centrale <- Ec
  cellules$evenements <- D
  sexes <- unique(table$sexe)
  ensemble <- identical(sexes, "ensemble")
  sexe_cle <- if (ensemble || !("sexe" %in% segmentation)) rep("ensemble", n) else as.character(cellules$sexe)
  k <- match(paste(sexe_cle, cellules$age, sep = "\r"), paste(table$sexe, table$age, sep = "\r"))
  cellules$qx_table <- table$qx[k]
  list(cellules = cellules, segment = g,
       hors_plage = list(cellules = sum(!dans), evenements = sum(as.numeric(exposition$evenements[!dans])),
                         exposition = sum(exposition$exposition_centrale[!dans])),
       appariement_sexe = if (!("sexe" %in% segmentation)) "sans_sexe"
                          else if (ensemble) "ensemble_par_sexe" else "par_sexe")
}
# `ligne` de grille_plage() : chaque cellule à ligne non NA a les clés, l'âge
# [, l'année], E^c et D de cette ligne de la table d'exposition ; chaque
# ligne de la plage est désignée une fois ; une cellule sans ligne a E^c = 0
# et D = 0. TRUE ou le texte de l'écart.
controler_ligne_grille <- function(gr, ex, cles, par_annee, plage) {
  l <- gr$ligne; c_ <- gr$cellules; a <- !is.na(l)
  dans <- which(ex$age >= plage[1L] & ex$age <= plage[2L])
  colonnes <- c(cles, "age", if (isTRUE(par_annee)) "annee", "exposition_centrale", "evenements")
  if (!is.integer(l) || length(l) != nrow(c_)) "ligne : integer de la longueur de la grille attendu"
  else if (!identical(sort(l[a]), dans)) "lignes désignées différentes des lignes de la plage"
  else if (!isTRUE(all.equal(c_[a, colonnes], ex[l[a], colonnes], check.attributes = FALSE)) ||
           !identical(c_$exposition_centrale[a], ex$exposition_centrale[l[a]]) ||
           !identical(c_$evenements[a], ex$evenements[l[a]])) "cellule différente de sa ligne d'exposition"
  else if (any(c_$exposition_centrale[!a] != 0) || any(c_$evenements[!a] != 0L)) "cellule sans ligne non nulle"
  else TRUE
}
# Contrat de la table construite (ADR 0017, point 1.2) : table_construite()
# lève une erreur de programmation (stop(), pas un refus) sur chaque
# violation, appliquée à la table positionnée d'une construction. TRUE ou le
# texte de l'écart.
controler_violations_table_construite <- function(x, segmentation) {
  et <- x$etapes
  tp <- et$positionnement$resultat$table_positionnee
  essai <- function(t) {
    et$positionnement$resultat$table_positionnee <- t
    tryCatch({ tablexp:::table_construite(et, segmentation, "positionnement"); "aucune erreur" },
             refus_tablexp = function(r) "refus au lieu d'une erreur de programmation",
             error = function(e) if (grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)) "erreur"
                                 else conditionMessage(e))
  }
  attr_en_trop <- tp; attr(attr_en_trop, "source") <- "x"
  age_double <- tp; age_double$age <- as.double(age_double$age)
  qx_15 <- tp; qx_15$qx[1L] <- 1.5
  qx_na <- tp; qx_na$qx[1L] <- NA_real_
  incomplet <- tp[-nrow(tp), , drop = FALSE]
  avec_annee <- tp; avec_annee$annee <- 2020L
  cas <- list(colonne_qx_absente = tp[setdiff(names(tp), "qx")], colonne_age_absente = tp[setdiff(names(tp), "age")],
              colonne_etat_absente = tp[setdiff(names(tp), "etat")], age_double = age_double, qx_1_5 = qx_15,
              qx_na = qx_na, segment_incomplet = incomplet, colonne_annee = avec_annee,
              attribut_en_trop = attr_en_trop)
  if (length(segmentation)) cas$cle_absente <- tp[setdiff(names(tp), segmentation[1L])]
  v <- vapply(cas, essai, "")
  if (all(v == "erreur")) TRUE else paste(sprintf("%s : %s", names(v)[v != "erreur"], v[v != "erreur"]), collapse = " ; ")
}
aplatis <- list()
# Relevé de la plateforme d'exécution, une fois (ADR 0009, annotation du
# 26/11/2026, (f) 3) ; régime et comparaison de chaque plan.
PLATEFORME <- identifiant_plateforme(RACINE)
REGIMES <- list(); COMPARAISONS <- list()
for (p in plans_reference()) {
  id <- p$identifiant
  entree_p <- entrees_ref[[p$entree]]
  x_ref <- construire_table(entree_p$donnees, entree_p$roles, p$plan, tables_reference = tables_du_plan(p))
  verifier(sprintf("%s : empreinte du résultat égale à celle de l'oracle d'avant #71 (écrivains des entiers et des dates par une chaîne par ligne)", id),
           grepl("^[0-9a-f]{32}$", x_ref$fiche$empreintes$resultat) &&
             identical(x_ref$fiche$empreintes$resultat, oracle_empreinte_resultat(x_ref)))
  # Rejeu outillé de la fiche écrite puis relue (#86, critère 4 ; identité
  # exacte hors execution ; verdicts des témoins, valeur calculée à la main).
  # Ligne 15 (temoin_catalogue, #267) : « identique », statut « comparaison
  # exacte » ; aucun plan de référence ne porte de table de statut embarquee,
  # la ligne y vaut le cas sans table de statut embarquee (mesuré, R1 à R17) ;
  # le cas « table embarquée » est couvert par le bloc qui suit la boucle (#303).
  verifier(sprintf("%s : rejouer_fiche() sur la fiche écrite puis relue, identical() hors execution, témoins 9 à 13 et 15 « identique » (15 : « comparaison exacte »), 14 « non vérifiée »", id), {
    r <- rejouer_fiche(entree_p$donnees, lire_fiche(ecrire_fiche(x_ref$fiche)), tables_reference = tables_du_plan(p))
    identical(sans_execution(r$construction), sans_execution(x_ref)) &&
      identical(r$bilan$verdict[9:15], c(rep("identique", 5L), "non v\u00e9rifi\u00e9e", "identique")) &&
      identical(r$bilan$controle[15L], "temoin_catalogue") && identical(r$bilan$statut[15L], "comparaison exacte")
  })
  # Rapport figé (#87, U24, U25) : contrôles du document ; taille (octets) et
  # durée du rendu imprimées (constat, sans seuil ; critère 7 de #87).
  t_rapport <- system.time(o_rapport <- ecrire_rapport(x_ref))[["elapsed"]]
  cat(sprintf("  -- %s : rapport figé, %d octets, rendu en %.2f s\n", id, length(o_rapport), t_rapport))
  verifier(sprintf("%s : rapport figé (U24) : sections et nav, cellules décimales en %%.17g, textes fixes, section brut", id), {
    l <- strsplit(rawToChar(o_rapport), "\n", fixed = TRUE)[[1L]]
    Encoding(l) <- "UTF-8"
    controler_rapport_fige(x_ref, l, id)
  })
  verifier(sprintf("%s : ligne « # rejeu » de la fiche écrite égale à TEXTE_REJEU_FICHE (U25)", id), {
    l <- strsplit(rawToChar(ecrire_fiche(x_ref$fiche)), "\n", fixed = TRUE)[[1L]]
    Encoding(l) <- "UTF-8"
    identical(l[startsWith(l, "# rejeu : ")], paste0("# rejeu : ", tablexp:::TEXTE_REJEU_FICHE))
  })
  g <- tryCatch(aplatir_construction(x_ref), refus_reference = function(e) e)
  verifier(sprintf("%s : aplatissement sans refus d'écriture", id),
           if (inherits(g, "refus_reference")) conditionMessage(g) else TRUE)
  if (inherits(g, "refus_reference")) next
  aplatis[[id]] <- g
  # Règle de complétude (point 3.4), avant toute création : un double sans
  # classe ferait échouer le workflow de création (critère de #32).
  verifier(sprintf("%s : aucun double sans classe de tolérance dans l'objet aplati (point 3.4)", id), {
    sans <- doubles_sans_tolerance(g)
    if (length(sans)) paste("sans tolérance :", paste(utils::head(sans, 5L), collapse = ", ")) else TRUE
  })
  # Invariants de structure de TOLERANCES (#175 ; point 3.4, seconde annotation,
  # (a)), pour tous les plans : (b) tout double d'une étape (etapes/<e>/ ou
  # fiche/plan/etapes/<e>/) n'est classé que par des clés qui commencent par
  # <e>/ ; (c) tout double de la fiche hors fiche/plan/etapes/ n'est classé que
  # par des clés qui commencent par fiche/ ; (d) aucun chemin n'est classé par
  # deux clés. Une étape nouvelle (#167, #168) ne peut ainsi hériter la classe
  # d'une colonne homonyme d'une autre étape, ni d'une clé nue.
  # Limite connue (audit de #175, constat mineur) : un double de premier
  # niveau hors etapes/ et fiche/ (avertissements/, table/ à venir) n'est
  # couvert par aucun invariant ; il hériterait en silence d'une clé d'étape si
  # un segment intermédiaire portait le nom de l'étape. Aucun tel double dans
  # R1 à R12 (mesuré : 0) ; à couvrir quand un premier apparaîtra.
  verifier(sprintf("%s : tout double d'une étape est classé par des clés qui commencent par l'étape (point 3.4)", id), {
    d <- names(g$types)[g$types == "double"]
    mal <- doubles_etape_mal_classes(d)
    if (!any(grepl(REGEX_CHEMIN_ETAPE, d))) "aucun double d'étape trouvé"
    else if (length(mal)) paste("clé non qualifiée par l'étape :", paste(utils::head(mal, 5L), collapse = ", "))
    else TRUE
  })
  verifier(sprintf("%s : tout double de la fiche hors fiche/plan/etapes/ est classé par des clés qui commencent par fiche/ (point 3.4)", id), {
    d <- names(g$types)[g$types == "double"]
    mal <- doubles_fiche_mal_classes(d)
    if (!any(startsWith(d, "fiche/") & !grepl(REGEX_CHEMIN_ETAPE, d))) "aucun double de fiche trouvé"
    else if (length(mal)) paste("clé non qualifiée par fiche/ :", paste(utils::head(mal, 5L), collapse = ", "))
    else TRUE
  })
  verifier(sprintf("%s : aucun chemin classé par deux clés", id), {
    conflit <- doubles_multi_cles(names(g$types)[g$types == "double"])
    if (length(conflit)) paste("classé par plusieurs clés :", paste(utils::head(conflit, 5L), collapse = ", ")) else TRUE
  })
  verifier(sprintf("%s : tout chemin nommé comme un dénombrement est integer, hors EXCEPTIONS_DENOMBREMENT (ADR 0009, point 9.6, (f) ; #199)", id), {
    mal <- denombrements_non_entiers(g)
    if (length(mal)) paste("dénombrement non integer :", paste(sprintf("%s (%s)", mal, g$types[mal]), collapse = ", "))
    else TRUE
  })
  # T178.3 (#178 ; spécification (0'), M361) : aucun sous-groupe de R8, R9,
  # R10 ne dépasse N_MAX_PRODUITS_CONVOLUTION ; les trois colonnes des p
  # asymptotiques sont entièrement NA (valeurs des ajouts de la transition
  # #178, que l'alignement ne contrôle pas).
  if (id %in% c("R8", "R9", "R10")) verifier(sprintf("%s : T178.3 colonnes p_*_asymptotique des sous-groupes de la validation entièrement NA, aucun état convolution_au_dela_n_max (#178)", id), {
    sg <- x_ref$etapes$validation$resultat$sous_groupes
    a <- c("p_inferieure_asymptotique", "p_superieure_asymptotique", "p_centrale_asymptotique")
    if (!all(a %in% names(sg))) "colonnes absentes"
    else if (!all(vapply(sg[a], function(v) is.double(v) && all(is.na(v)), NA))) "valeurs non NA"
    else if (any(sg$etat == "convolution_au_dela_n_max")) "état convolution_au_dela_n_max"
    else TRUE
  })
  # #218 (critère 2 de l'issue, C4 du § 1.33) : theta_80 de R11 et R12 égaux
  # aux valeurs remesurées par actuary-approfondi (commentaire 7012205013 ;
  # étai du corps de #218 à la cinquième décimale), à 5e-7 près (sixième
  # décimale) ; recalcul indépendant du moteur à partir de la table
  # d'exposition (cellules d'âge de la plage 20-90, segments sexe ; strates
  # informatives D_.c > 0 et deux segments exposés ; V complète,
  # pseudo-inverse de Moore-Penrose par svd() ; lambda_80 par uniroot sur
  # pchisq ; racines par uniroot) : |lambda(theta_80) / lambda_80 - 1| <= 1e-8
  # et theta_80 égal à la racine indépendante à 1e-8 en relatif ; tous les
  # états produite ; aucun signal ecart_minimal_detectable.
  if (id %in% c("R11", "R12")) verifier(sprintf("%s : #218 theta_80 (C4 : valeurs d'actuary-approfondi à 5e-7 ; recalcul indépendant à 1e-8 à partir de la table d'exposition), états produite, aucun signal", id), local({
    # local() : les noms ci-dessous (g, D, E, k…) ne doivent pas masquer ceux
    # de la boucle des plans.
    em <- x_ref$etapes$homogeneite$resultat$ecart_minimal
    attendu <- list(R11 = c(1.090574, 1.090240, 0.917229, 0.916948), R12 = c(1.035880, 1.035860, 0.965381, 0.965363))[[id]]
    ex <- x_ref$etapes$exposition$resultat$exposition
    ex <- ex[ex$age >= 20 & ex$age <= 90, ]
    ages <- sort(unique(ex$age)); segs <- em$sexe
    E <- D <- matrix(0, length(segs), length(ages))
    ij <- cbind(match(ex$sexe, segs), match(ex$age, ages))
    E[ij] <- ex$exposition_centrale; D[ij] <- ex$evenements
    Dc <- colSums(D); inf <- Dc > 0 & colSums(E > 0) >= 2
    V <- matrix(0, length(segs), length(segs))
    for (k in which(inf)) { pk <- E[, k] / sum(E[, k]); V <- V + Dc[k] * (diag(pk) - tcrossprod(pk)) }
    sv <- svd(V); g <- sv$d > max(sv$d) * 1e-12
    Vp <- sv$v[, g, drop = FALSE] %*% (t(sv$u[, g, drop = FALSE]) / sv$d[g])
    ddl <- sum(g); q <- stats::qchisq(0.95, ddl)
    l80 <- stats::uniroot(function(l) stats::pchisq(q, ddl, ncp = l, lower.tail = FALSE) - 0.80, c(0, 100), tol = 1e-13)$root
    lam <- function(s, th) {
      de <- numeric(length(segs))
      for (k in which(inf)) { pk <- E[, k] / sum(E[, k]); w <- rep(1, length(segs)); w[s] <- th
        de <- de + Dc[k] * (pk * w / sum(pk * w) - pk) }
      drop(crossprod(de, Vp %*% de))
    }
    obt <- c(em$theta_80_superieur, em$theta_80_inferieur)
    seg_br <- rep(seq_along(segs), 2L)
    rel <- vapply(seq_along(obt), function(i) lam(seg_br[i], obt[i]) / l80 - 1, 0)
    rac <- vapply(seq_along(obt), function(i) {
      bornes <- if (i <= length(segs)) c(1, 2) else c(0.5, 1)
      stats::uniroot(function(th) lam(seg_br[i], th) - l80, bornes, tol = 1e-14)$root }, 0)
    if (!all(c(em$etat_superieur, em$etat_inferieur) == "produite")) "états"
    else if (any(abs(obt - attendu) > 5e-7)) paste("C4 :", paste(sprintf("%.7f", obt), collapse = ", "))
    else if (any(abs(rel) > 1e-8)) paste("lambda(theta_80) / lambda_80 - 1 :", paste(sprintf("%.2e", rel), collapse = ", "))
    else if (any(abs(rac / obt - 1) > 1e-8)) paste("racine indépendante :", paste(sprintf("%.10f", rac), collapse = ", "))
    else if ("ecart_minimal_detectable" %in% x_ref$avertissements$code) "signal"
    else TRUE
  }))
  # Commit préparatoire (1a) de B19 (#223 ; ADR 0017, point 10.1), R1 et R4
  # (par année) : apparier_table() sur la grille extraite rend un objet
  # identical() à l'ancien corps (993068c), table de R8, plage 20 à 90 ;
  # `ligne` de grille_plage() indexe la table d'exposition.
  if (id %in% c("R1", "R4")) {
    ex1 <- x_ref$etapes$exposition$resultat$exposition
    pa1 <- x_ref$etapes$exposition$parametres$par_annee
    tv1 <- tables_reference_r8()$tv
    verifier(sprintf("%s : apparier_table() (grille_plage(), module exposition) identical() à l'ancien corps de 993068c (#223, ADR 0017, point 10.1)", id),
             identical(tablexp:::apparier_table(ex1, p$plan$segmentation, pa1, tv1, c(20, 90), "tv", "validation"),
                       apparier_table_993068c(ex1, p$plan$segmentation, pa1, tv1, c(20, 90))))
    verifier(sprintf("%s : grille_plage() : ligne indexe la table d'exposition, cellules sans ligne nulles (#223)", id),
             controler_ligne_grille(tablexp:::grille_plage(ex1, p$plan$segmentation, pa1, c(20, 90)), ex1,
                                    p$plan$segmentation, pa1, c(20, 90)))
  }
  # Table construite (ADR 0017, points 1.2 et 1.5), R10 et R13 : la table
  # positionnée tient le contrat (table_construite() la rend telle quelle,
  # plage du positionnement, ddl_effectifs NULL faute de colonne) ; chaque
  # violation lève une erreur de programmation.
  if (id %in% c("R10", "R13")) {
    verifier(sprintf("%s : table_construite() sur table_positionnee : table rendue identical(), plage_ages du positionnement, ddl_effectifs NULL (#223, ADR 0017, point 1.5)", id), {
      tc <- tablexp:::table_construite(x_ref$etapes, p$plan$segmentation, "positionnement")
      identical(names(tc), c("table", "plage_ages", "ddl_effectifs")) &&
        identical(tc$table, x_ref$etapes$positionnement$resultat$table_positionnee) &&
        identical(tc$plage_ages, p$plan$etapes$positionnement$plage_ages) && is.null(tc$ddl_effectifs)
    })
    verifier(sprintf("%s : table_construite() : colonne absente, age double, qx à 1,5 ou NA, segment incomplet, colonne annee, attribut en trop -> erreur de programmation (#223, ADR 0017, point 1.2)", id),
             controler_violations_table_construite(x_ref, p$plan$segmentation))
  }
  # R14 (#180, C6 à l'échelle de l'entrée de référence) : relevés de la
  # construction de référence égaux aux décomptes de l'attribut simulation
  # de son entrée (P0, E, ES par cause, présents au dernier inventaire) ;
  # aucune construction supplémentaire.
  if (id == "R14") verifier("R14 : relevés de l'étape donnees égaux aux décomptes de l'attribut simulation de l'entrée (P0, E, ES par cause, présents au dernier inventaire) (C6)", {
    r <- x_ref$etapes$donnees$diagnostics$releves
    dc <- attr(entree_p$donnees, "simulation")$forme$decomptes
    if (!identical(r$p0, dc$p0)) "P0 différent"
    else if (!identical(r$entrees, dc$entrees)) "E différent"
    else if (!identical(r$entres_sortis_par_cause[names(dc$entres_sortis)], dc$entres_sortis) ||
             !identical(r$entres_sortis, sum(dc$entres_sortis))) "ES par cause différents"
    else if (!identical(r$inventaires$presents[nrow(r$inventaires)], dc$presents_dernier)) "présents au dernier inventaire différents"
    else TRUE
  })
  # R7 (#32) : Kaplan-Meier sous age_atteint_annee. Σd par cellule des sauts =
  # evenements de la cellule Kaplan-Meier = evenements de l'exposition (même
  # cellules, mêmes effectifs d'événements).
  if (id == "R7") verifier("R7 : Σ des evenements des sauts par cellule = evenements de Kaplan-Meier = evenements de l'exposition (#32)", {
    ex <- x_ref$etapes$exposition$resultat$exposition
    km <- x_ref$etapes$brut$resultat$kaplan_meier
    sa <- x_ref$etapes$brut$resultat$kaplan_meier_sauts
    cle <- function(d) paste(d$sexe, d$age)
    som <- tapply(sa$evenements, cle(sa), sum)
    d_sauts <- unname(ifelse(is.na(som[cle(km)]), 0L, som[cle(km)]))
    if (!identical(cle(km), cle(ex))) "cellules de Kaplan-Meier et de l'exposition différentes"
    else if (!identical(as.integer(d_sauts), km$evenements)) "Σ des sauts différente des evenements de Kaplan-Meier"
    else if (!identical(km$evenements, ex$evenements)) "evenements de Kaplan-Meier différents de ceux de l'exposition"
    else if (sum(sa$evenements) != sum(ex$evenements)) "totaux différents" else TRUE
  })
  # R9 (#166), non-vacuité : le retrait contrefactuel porte sur des lignes
  # réellement exclues, une classe de mesurabilité par motif (id_contrat_manquant :
  # mesurable aux niveaux A et B ; sexe_manquant : mesurable en A, non calculable
  # en B faute de clé sous la segmentation `sexe` ; entree_manquante : non
  # mesurable), des retraits non nuls et un SMR du comparateur 1 défini. Sans
  # défauts dans l'entrée, ces contrôles échouent (R8 les passerait à vide).
  if (id == "R9") {
    ex9 <- x_ref$etapes$donnees$resultat
    ie9 <- x_ref$etapes$importance_exclusions$resultat
    verifier("R9 : lignes exclues pour chacun des motifs de MOTIFS_DEFAUTS_R9 (NOMBRE_PAR_MOTIF_R9), nombre_lignes_exclues = leur somme", {
      n <- vapply(ex9$lignes_exclues[MOTIFS_DEFAUTS_R9], length, 1L)
      if (!identical(unname(n), rep(NOMBRE_PAR_MOTIF_R9, length(MOTIFS_DEFAUTS_R9)))) paste("effectifs par motif :", paste(names(n), n, collapse = ", "))
      else if (!identical(ex9$nombre_lignes_exclues, sum(n))) "nombre_lignes_exclues différent de la somme"
      else TRUE
    })
    ligne9 <- function(tab, ens, niveau) tab[tab$ensemble == ens & tab$niveau == niveau, , drop = FALSE]
    verifier("R9 : une classe de mesurabilité par motif (id_contrat_manquant : retrait aux niveaux A et B, sexe_manquant : A seulement, clé manquante en B, entree_manquante : aucune ligne mesurable)", {
      r <- ie9$retraits
      retire <- function(ens, niveau) { l <- ligne9(r, ens, niveau)
        nrow(l) == 1L && identical(l$etat, "produite") && isTRUE(l$evenements_retires > 0) &&
          isTRUE(l$exposition_jours_retiree > 0) && isTRUE(l$exposition_centrale_retiree > 0) }
      etat <- function(ens, niveau) { e <- ligne9(r, ens, niveau)$etat; if (length(e) == 1L) e else sprintf("(%d ligne(s))", length(e)) }
      ecarts <- c(
        if (!retire("id_contrat_manquant", "A")) "id_contrat_manquant, niveau A : pas de retrait produit non nul",
        if (!retire("id_contrat_manquant", "B")) "id_contrat_manquant, niveau B : pas de retrait produit non nul",
        if (!retire("sexe_manquant", "A")) "sexe_manquant, niveau A : pas de retrait produit non nul",
        if (!identical(etat("sexe_manquant", "B"), "cle_segmentation_manquante")) paste("sexe_manquant, niveau B : état", etat("sexe_manquant", "B")),
        if (!identical(etat("entree_manquante", "A"), "aucune_ligne_mesurable")) paste("entree_manquante, niveau A : état", etat("entree_manquante", "A")),
        if (!identical(etat("entree_manquante", "B"), "aucune_ligne_mesurable")) paste("entree_manquante, niveau B : état", etat("entree_manquante", "B")))
      if (length(ecarts)) ecarts else TRUE
    })
    verifier("R9 : retraits non nuls (total des niveaux A et B) et SMR du comparateur 1 défini, fini et positif, intervalle exact non dégénéré", {
      r <- ie9$retraits; sm <- ie9$smr_comparateur_1
      def <- function(niveau) { l <- ligne9(sm, "total", niveau)
        nrow(l) == 1L && l$etat == "produite" && l$observes > 0 && is.finite(l$attendus) && l$attendus > 0 &&
          is.finite(l$smr) && l$smr > 0 && is.finite(l$borne_inf_exacte) && is.finite(l$borne_sup_exacte) &&
          l$borne_inf_exacte < l$smr && l$smr < l$borne_sup_exacte }
      ecarts <- c(
        if (!isTRUE(ligne9(r, "total", "A")$evenements_retires > 0)) "total, niveau A : aucun événement retiré",
        if (!isTRUE(ligne9(r, "total", "B")$evenements_retires > 0)) "total, niveau B : aucun événement retiré",
        if (!isTRUE(ligne9(r, "total", "A")$exposition_centrale_retiree > 0)) "total, niveau A : aucune exposition centrale retirée",
        if (!isTRUE(def("A"))) "SMR du comparateur 1, total, niveau A : non défini, non fini, non positif ou intervalle exact dégénéré",
        if (!isTRUE(def("B"))) "SMR du comparateur 1, total, niveau B : non défini, non fini, non positif ou intervalle exact dégénéré")
      if (length(ecarts)) ecarts else TRUE
    })
  }
  f <- tempfile(fileext = ".tsv")
  ecrire_reference(g, f, c(identifiant = id))
  verifier(sprintf("%s : aller-retour lire(ecrire(aplatir(x))) identical() (types et valeurs)", id), {
    lu <- lire_reference(f)
    identical(lu$types, g$types) && identical(lu$valeurs, g$valeurs)
  })
  unlink(f)
  # Régime de comparaison du plan (ADR 0009, annotation du 26/11/2026, (f) 3
  # et 5) : calculé une fois, d'après les lignes « plateforme : » de l'en-tête
  # du fichier commité et le relevé de la plateforme d'exécution ; en régime
  # de constat, les chemins du plan de CONSTATS_HORS_PLATEFORME sont passés à
  # la comparaison ordinaire et, sous transition, à seul_ecart().
  f_p <- file.path(DOSSIER_REFERENCES, paste0(id, ".tsv"))
  regime_p <- if (file.exists(f_p)) regime_comparaison(PLATEFORME, lire_reference(f_p)$entete)
              else list(regime = "strict", raison = "fichier de référence absent", mixte = FALSE)
  REGIMES[[id]] <- regime_p$regime
  constats_p <- if (identical(regime_p$regime, "constat"))
    CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == id] else character(0)
  comparer_au_fichier <- function(id, g) {
    f <- file.path(DOSSIER_REFERENCES, paste0(id, ".tsv"))
    if (!file.exists(f)) return(sprintf("fichier de référence absent : tests/reference/%s.tsv", id))
    r <- comparer_references(lire_reference(f), g, constats = constats_p)
    COMPARAISONS[[id]] <<- r
    cat(imprimer_comparaison(r, sprintf("  -- %s : tests/reference/%s.tsv contre l'objet recalculé (régime du plan : %s ; %s%s)", id, id,
                                        regime_p$regime, regime_p$raison,
                                        if (isTRUE(regime_p$mixte)) " ; mention : régime strict, aucun échec nouveau" else "")), sep = "\n")
    if (r$ok) TRUE else sprintf("comparaison en échec (%d écart(s) structurel(s), %d grandeur(s) en écart ou sans tolérance%s)",
                                length(r$structure), sum(!(r$grandeurs$verdict %in% c("identique", VERDICT_CONSTAT) |
                                                           grepl("^sous tol", r$grandeurs$verdict, useBytes = TRUE))),
                                if (r$entree$differente) ", entrée différente" else "")
  }
  nom <- sprintf("%s (%s) : comparaison à tests/reference/%s.tsv", id, p$description, id)
  present <- file.exists(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv")))
  # Échec attendu seulement si le fichier est absent : un plan encore listé
  # dont le fichier existe échoue (marque à retirer ; la comparaison devient
  # alors un test ordinaire).
  if (id %in% names(SANS_REFERENCE) && !present) echec_attendu(nom, SANS_REFERENCE[[id]], comparer_au_fichier(id, g))
  else if (id %in% names(SANS_REFERENCE))
    verifier(nom, sprintf("tests/reference/%s.tsv existe : retirer %s de SANS_REFERENCE", id, id))
  else controler_plan(id, nom, lire_reference(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv"))), g,
                      comparer_au_fichier(id, g), TRANSITIONS_EN_COURS, entree_p$donnees, entree_p$roles, constats_p)
}
# Régimes et verdicts par plan (ADR 0009, annotation du 26/11/2026, (g) 1).
cat("  Régimes de comparaison et verdicts par plan (identique / sous tolérance / constat / écart) :\n")
for (id in names(COMPARAISONS)) {
  # Verdicts comparés sur leur motif ASCII ou aux constantes de l'outillage :
  # un littéral non ASCII de ce fichier n'est pas marqué en locale C.
  v <- COMPARAISONS[[id]]$grandeurs$verdict
  sous <- grepl("^sous tol", v, useBytes = TRUE)
  cat(sprintf("    %s : régime %s ; %d / %d / %d / %d\n", id, REGIMES[[id]], sum(v == "identique"), sum(sous),
              sum(v == VERDICT_CONSTAT), sum(!(v == "identique" | sous | v == VERDICT_CONSTAT))))
}
# Verrou en conditions réelles ((g) 3) : une exécution qui se déclare sur
# l'image de référence (TABLEXP_IMAGE définie) ou sur la CI (GITHUB_ACTIONS =
# true) exige le régime strict pour chaque plan ; il ne peut que faire
# échouer. Sans objet sur un poste.
verrou <- verrou_regimes(unlist(REGIMES), vapply(COMPARAISONS, function(r) length(r$constats), 0L))
if (is.na(verrou)) {
  cat("  [sans objet] verrou du régime strict : ni TABLEXP_IMAGE définie (même vide), ni GITHUB_ACTIONS = true (poste)\n")
} else verifier(sprintf("verrou : exécution déclarée sur la plateforme de référence -> régime strict pour chacun des %d plans (ADR 0009, annotation du 26/11/2026, (g) 3)",
                        length(REGIMES)), verrou)
verifier("aucune référence orpheline sous tests/reference/ (point 3.7)", {
  o <- if (dir.exists(DOSSIER_REFERENCES)) references_orphelines(DOSSIER_REFERENCES) else character(0)
  if (length(o)) paste("orphelines :", paste(o, collapse = ", ")) else TRUE
})

# Plan R10 sur une table embarquée (#303 ; avis d'actuary du 16/11/2026, M520 ;
# aucune référence nouvelle : identités exactes et textes). Aucun plan de
# référence ne porte de table de statut embarquee ; la ligne 15 du bilan du
# rejeu (temoin_catalogue, #267) n'y vaut que dans le cas « aucune table de
# statut embarquee ». Le plan de R10 (positionnement par SMR sur la clé tv,
# validation en relation positionnee, segmentation par sexe), construit ici et
# non dans plans_reference(), lit TV 88-90 par table_embarquee() sur l'entrée
# `reference`. Le rejeu ne reçoit aucune table : une table embarquée est relue
# par son nom dans le package installé, et sa réception est refusée (ADR 0015).
# Textes de la ligne 15 : valeurs de tests/unitaires/test_rejeu.R, T12.
p_emb <- plans_reference()[["R10"]]
entree_emb <- entrees_ref[[p_emb$entree]]
NOM_EMBARQUEE <- "TV 88-90"
x_emb <- tryCatch(construire_table(entree_emb$donnees, entree_emb$roles, p_emb$plan,
                                   tables_reference = list(tv = table_embarquee(NOM_EMBARQUEE))),
                  refus_tablexp = function(e) e)
verifier("table embarqu\u00e9e (R10 sur TV 88-90) : construction sans refus", {
  if (inherits(x_emb, "refus_tablexp")) conditionMessage(x_emb) else TRUE
})
if (!inherits(x_emb, "refus_tablexp")) {
  cat_emb <- tablexp::tables_embarquees()
  cat_emb <- cat_emb[cat_emb$nom == NOM_EMBARQUEE, ]
  verifier("table embarqu\u00e9e : fiche/tables_reference/tv de statut embarquee, source, forme_origine et empreinte \u00e9gales \u00e0 celles du catalogue install\u00e9", {
    t <- x_emb$fiche$tables_reference$tv
    if (!identical(names(x_emb$fiche$tables_reference), "tv")) "tables_reference : cl\u00e9 tv seule attendue"
    else if (!identical(t$nom, NOM_EMBARQUEE) || !identical(t$statut, "embarquee")) "nom ou statut diff\u00e9rents"
    else if (nrow(cat_emb) != 1L || !grepl("^[0-9a-f]{32}$", cat_emb$empreinte)) "catalogue : une ligne et une empreinte de 32 hexad\u00e9cimaux attendues"
    else if (!identical(t$source, cat_emb$source) || !nzchar(t$source)) "source diff\u00e9rente de celle du catalogue"
    else if (!identical(t$forme_origine, cat_emb$forme_origine) || !identical(t$forme_origine, "lx")) "forme_origine diff\u00e9rente"
    else if (!identical(t$empreinte, cat_emb$empreinte)) "empreinte diff\u00e9rente de celle du catalogue"
    else TRUE
  })
  # Fiche écrite puis relue, rejeu sans tables_reference : la table est relue
  # par son nom. Lignes 9 à 15 comme pour R1 à R17 ; ligne 15, textes de
  # l'attendu et du lu, verdict « identique », statut « comparaison exacte ».
  f_emb <- tryCatch(lire_fiche(ecrire_fiche(x_emb$fiche)), error = function(e) e)
  r_emb <- if (inherits(f_emb, "error")) f_emb
           else tryCatch(rejouer_fiche(entree_emb$donnees, f_emb), error = function(e) e)
  verifier("table embarqu\u00e9e : fiche relue, \u00e9l\u00e9ment tables_reference identical() \u00e0 celui de la construction", {
    if (inherits(f_emb, "error")) conditionMessage(f_emb) else identical(f_emb$tables_reference, x_emb$fiche$tables_reference)
  })
  verifier("table embarqu\u00e9e : rejouer_fiche() sans tables_reference, identical() hors execution, t\u00e9moins 9 \u00e0 13 et 15 \u00ab identique \u00bb, 14 \u00ab non v\u00e9rifi\u00e9e \u00bb", {
    if (inherits(r_emb, "error")) conditionMessage(r_emb)
    else identical(sans_execution(r_emb$construction), sans_execution(x_emb)) &&
      identical(r_emb$bilan$verdict[9:15], c(rep("identique", 5L), "non v\u00e9rifi\u00e9e", "identique"))
  })
  verifier("table embarqu\u00e9e : ligne 15 \u00ab tv : m\u00e9tadonn\u00e9es du catalogue du package install\u00e9 \u00bb contre \u00ab tv : m\u00e9tadonn\u00e9es de la fiche \u00bb, \u00ab identique \u00bb, \u00ab comparaison exacte \u00bb", {
    if (inherits(r_emb, "error")) conditionMessage(r_emb)
    else identical(unlist(r_emb$bilan[15L, ], use.names = FALSE),
                   c("temoin_catalogue", "tv : m\u00e9tadonn\u00e9es du catalogue du package install\u00e9",
                     "tv : m\u00e9tadonn\u00e9es de la fiche", "identique", "comparaison exacte"))
  })
  # Rapport figé (U24) : mêmes contrôles que pour R1 à R17.
  t_emb <- system.time(o_emb <- tryCatch(ecrire_rapport(x_emb), error = function(e) e))[["elapsed"]]
  if (!inherits(o_emb, "error"))
    cat(sprintf("  -- table embarqu\u00e9e (R10 sur %s) : rapport fig\u00e9, %d octets, rendu en %.2f s\n", NOM_EMBARQUEE, length(o_emb), t_emb))
  verifier("table embarqu\u00e9e : rapport fig\u00e9 (U24) : sections et nav, cellules d\u00e9cimales en %.17g, textes fixes, section brut", {
    if (inherits(o_emb, "error")) conditionMessage(o_emb)
    else {
      l <- strsplit(rawToChar(o_emb), "\n", fixed = TRUE)[[1L]]
      Encoding(l) <- "UTF-8"
      controler_rapport_fige(x_emb, l, "R10")
    }
  })
}

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
# kaplan_meier ; R3 et R7 sont les plans sous age_atteint_annee : R3 est
# permuté, R7 (R3 avec kaplan_meier) ne l'est pas, son chemin Kaplan-Meier
# l'étant par R6, sous la convention plus_fine.
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
  # Deux colonnes `taux` de classes différentes : brut/resultat/kaplan_meier/taux
  # (1e-12) et brut/resultat/poisson/taux (1e-14) ; la même altération sur
  # poisson/taux est un écart.
  list(chemin = P_KM, facteur = 1 + 0.5e-12, ok = TRUE, v = "sous tolérance", classe = "clé qualifiée brut/resultat/kaplan_meier/taux (1e-12)"),
  list(chemin = P_TAUX, facteur = 1 + 0.5e-12, ok = FALSE, v = "écart", classe = "même altération sur poisson/taux (1e-14)")
)) {
  verifier(sprintf("témoin : %s altéré, classe %s -> %s", cas$chemin, cas$classe, if (cas$ok) "succès" else "échec"),
           attendu(comparer_references(alterer(cas$chemin, cas$facteur), base), cas$ok, cas$chemin, cas$v))
}
# Clé la plus longue retenue, les autres clés qui classent le chemin imprimées :
# la table TOLERANCES n'en a plus (invariant (d), #175), une clé plus courte est
# ajoutée en mémoire.
verifier("témoin : clé retenue pour kaplan_meier/taux = brut/resultat/kaplan_meier/taux (aussi : kaplan_meier/taux), imprimée", {
  masque <- c(TOLERANCES, "kaplan_meier/taux" = 1e-14)
  t <- tolerance_de(P_KM, masque)
  r <- comparer_references(ref0, base, masque)
  identical(tolerance_de(P_KM)$autres, character(0)) &&
    identical(t$cle, "brut/resultat/kaplan_meier/taux") && identical(t$tol, 1e-12) && identical(t$autres, "kaplan_meier/taux") &&
    any(grepl("clé retenue : etapes/brut/resultat/kaplan_meier/taux -> brut/resultat/kaplan_meier/taux (classe 1e-12 ; aussi : kaplan_meier/taux)",
              imprimer_comparaison(r), fixed = TRUE, useBytes = TRUE))
})
verifier("témoin : impression d'un écart (chemin, avant et après en %.17g, tolérance, verdict), de chaque classe de CLASSES_TOLERANCE (cinq) et de CLASSES_ABSOLUES (deux), du régime et des compteurs de constats et de déclarations d'échelle invalides", {
  avant <- alterer(P_TAUX, 1 + 2e-14)
  r <- comparer_references(avant, base)
  l <- imprimer_comparaison(r)
  e <- r$grandeurs[r$grandeurs$chemin == P_TAUX, ]
  sum(grepl("Écart relatif maximal, classe", l, fixed = TRUE, useBytes = TRUE)) == length(CLASSES_TOLERANCE) &&
    length(CLASSES_TOLERANCE) == 5L &&
    sum(grepl("Écart rapporté à facteur x échelle maximal, classe abs_", l, fixed = TRUE, useBytes = TRUE)) == length(CLASSES_ABSOLUES) &&
    length(CLASSES_ABSOLUES) == 2L &&
    any(grepl("Régime de la comparaison : strict", l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("; 0 constat(s) hors plateforme de référence ; 0 déclaration(s) d'échelle invalide(s)", l, fixed = TRUE, useBytes = TRUE)) &&
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
# Mêmes témoins pour R8 (#166, audit m1) : les chemins integer que les clés
# nues cellules_retenues et evenements_retires auraient classés ; les clés de R8
# sont qualifiées (`<table>/<colonne>`) et n'atteignent que des doubles. Les
# deux derniers sont integer depuis #174 (B16).
for (chemin_ie in c("etapes/validation/diagnostics/cellules_retenues",
                    "etapes/importance_exclusions/resultat/cellules_niveau_a/evenements_retires",
                    "etapes/importance_exclusions/resultat/cellules_niveau_b/evenements_retires",
                    "etapes/validation/resultat/sous_groupes/cellules_retenues",
                    "etapes/importance_exclusions/resultat/retraits/evenements_retires")) {
  verifier(sprintf("témoin : %s remis en double -> relevé par doubles_sans_tolerance(), que la clé nue %s aurait masqué", chemin_ie, basename(chemin_ie)), {
    g <- aplatis[["R8"]]
    if (!identical(unname(g$types[chemin_ie]), "integer"))
      return(sprintf("%s de R8 : un chemin integer attendu, type %s", chemin_ie, unname(g$types[chemin_ie])))
    masque <- c(TOLERANCES, stats::setNames(0, basename(chemin_ie)))
    g$types[[chemin_ie]] <- "double"; g$valeurs[[chemin_ie]] <- as.double(g$valeurs[[chemin_ie]])
    chemin_ie %in% doubles_sans_tolerance(g) && !chemin_ie %in% doubles_sans_tolerance(g, masque)
  })
}
# Témoins de #199 (ADR 0009, seconde annotation du 10/11/2026, (e) et (f) ;
# critère 8 de la spécification) : un chemin par famille de clés retirées,
# integer dans le plan qui le porte, remis en double en mémoire, est relevé
# par doubles_sans_tolerance(), que la clé exacte retirée aurait masqué, et
# par denombrements_non_entiers() (contrôle des types, (f)). Rien n'est écrit
# sous tests/reference/.
for (t199 in list(c("R8", "etapes/validation/diagnostics/evenements_total"),
                  c("R8", "etapes/importance_exclusions/resultat/smr_comparateur_2/observes"),
                  c("R10", "etapes/positionnement/resultat/smr/observes"),
                  c("R11", "etapes/homogeneite/resultat/monte_carlo/jeux/tirages_effectues"))) {
  verifier(sprintf("témoin (#199) : %s de %s remis en double -> relevé par doubles_sans_tolerance() (que sa clé retirée aurait masqué) et par le contrôle des dénombrements", t199[2], t199[1]), {
    g <- aplatis[[t199[1]]]; c_ <- t199[2]
    if (!identical(unname(g$types[c_]), "integer"))
      return(sprintf("%s de %s : un chemin integer attendu, type %s", c_, t199[1], unname(g$types[c_])))
    masque <- c(TOLERANCES, stats::setNames(0, sub("^etapes/", "", c_)))
    g$types[[c_]] <- "double"; g$valeurs[[c_]] <- as.double(g$valeurs[[c_]])
    c_ %in% doubles_sans_tolerance(g) && !c_ %in% doubles_sans_tolerance(g, masque) &&
      identical(denombrements_non_entiers(g), c_)
  })
}
verifier("témoin (#199) : l'exception poids_exclusions/evenements est double dans R13 et n'est pas relevée ; sans l'exception, elle le serait", {
  g <- aplatis[["R13"]]; c_ <- EXCEPTIONS_DENOMBREMENT[1L]
  if (!identical(unname(g$types[c_]), "double")) return(sprintf("%s de R13 : double attendu", c_))
  !length(denombrements_non_entiers(g)) &&
    identical(local({ EXCEPTIONS_DENOMBREMENT <- character(0); environment(denombrements_non_entiers) <- environment()
                      denombrements_non_entiers(g) }), c_)
})
# Une table de même nom sous une autre étape n'hérite d'aucune classe (#166,
# point 3.4) : cellules, statistiques, retraits de R8 recopiés sous
# `etapes/etape_non_listee` et `etapes/homogeneite` (copies en mémoire ;
# `positionnement` a ses clés depuis #167, d'où une étape fictive ;
# `homogeneite` a les siennes depuis #168, qui ne classent aucune de ces
# tables) sont relevés par doubles_sans_tolerance(), que des clés
# `<table>/<colonne>` auraient masqués.
for (chemin_etape in c("etapes/validation/resultat/cellules/attendus",
                       "etapes/validation/resultat/cellules/taux_table",
                       "etapes/validation/resultat/statistiques/valeur",
                       "etapes/validation/resultat/sous_groupes/smr",
                       "etapes/importance_exclusions/resultat/retraits/exposition_centrale_retiree")) {
  for (autre in c("etape_non_listee", "homogeneite")) {
    chemin_autre <- sub("^etapes/[a-z_]+/", paste0("etapes/", autre, "/"), chemin_etape)
    verifier(sprintf("témoin : %s simulé sous une autre étape -> relevé par doubles_sans_tolerance()", chemin_autre), {
      g <- aplatis[["R8"]]
      if (!identical(unname(g$types[chemin_etape]), "double"))
        return(sprintf("%s de R8 : un chemin double attendu", chemin_etape))
      g$types[[chemin_autre]] <- g$types[[chemin_etape]]; g$valeurs[[chemin_autre]] <- g$valeurs[[chemin_etape]]
      !chemin_etape %in% doubles_sans_tolerance(aplatis[["R8"]]) && chemin_autre %in% doubles_sans_tolerance(g) &&
        !chemin_etape %in% doubles_sans_tolerance(g)
    })
  }
}
# Étape homogeneite (#168) : R11 et R12 la portent (plans de méthode ; références
# créées sur la CI, exécution 42465708495) ; ses clés `homogeneite/…` de TOLERANCES sont
# contrôlées ici sur une construction témoin (plan de R1 et étape homogeneite,
# segmentation sexe, entrée `reference`), en mémoire, sans écriture sous
# tests/reference/ : complétude (point 3.4), invariants (b) et (d) de #175,
# et chaque clé `homogeneite/…` classe au moins un chemin (aucune clé morte).
local({
  p <- plan_reference(graine = GRAINE_MONTE_CARLO_R8)
  p$etapes$homogeneite <- list(plage_ages = c(20, 90), tranches_age = c(20, 50, 70), replications = 99)
  g <- aplatir_construction(construire_table(jeu_ref$donnees, jeu_ref$roles, p))
  d <- names(g$types)[g$types == "double"]
  dh <- d[grepl("(^|/)etapes/homogeneite/", d)]
  cles_h <- names(TOLERANCES)[startsWith(names(TOLERANCES), "homogeneite/")]
  utilisees <- unique(unlist(lapply(dh, cles_de)))
  verifier("homogeneite (#168), construction témoin : tout double classé, par des clés homogeneite/ seules, une clé par chemin, aucune clé morte (point 3.4, #175)", {
    sans <- doubles_sans_tolerance(g)
    if (!length(dh)) "aucun double de l'étape homogeneite"
    else if (length(sans)) paste("sans tolérance :", paste(utils::head(sans, 5L), collapse = ", "))
    else if (length(mal <- doubles_etape_mal_classes(dh))) paste("mal classés :", paste(utils::head(mal, 5L), collapse = ", "))
    else if (length(m2 <- doubles_multi_cles(dh))) paste("plusieurs clés :", paste(utils::head(m2, 5L), collapse = ", "))
    else if (length(morte <- setdiff(cles_h, utilisees))) paste("clés mortes :", paste(morte, collapse = ", "))
    else TRUE
  })
})
# Colonnes qu'une clé nue classait avant #175 : pour chacune des 36 clés qui les
# remplacent, un chemin double qu'elle classe dans R6 est recopié en mémoire
# sous une étape non listée (etape_non_listee, fictive : `positionnement` a ses
# clés depuis #167 ; fiche/plan/etapes/etape_non_listee/ ou
# fiche/hypotheses_etape_non_listee/ pour la fiche). Le chemin recopié est
# relevé par doubles_sans_tolerance(), l'original ne l'est pas, et avec
# l'ancienne clé ajoutée en mémoire (sa classe est sans objet ici) le chemin
# recopié ne l'est plus : ce que la clé nue masquait. Rien n'est écrit sous
# tests/reference/.
ANCIENNES_CLES_NUES <- c(
  "etapes/exposition/resultat/exposition/exposition_jours" = "exposition_jours",
  "etapes/exposition/diagnostics/exposition_jours" = "exposition_jours",
  "etapes/exposition/parametres/delai_declaration" = "delai_declaration",
  "fiche/plan/delai_declaration" = "delai_declaration",
  "etapes/brut/parametres/niveau" = "niveau",
  "fiche/plan/etapes/brut/niveau" = "niveau",
  "fiche/hypotheses_brut/niveau" = "niveau",
  "etapes/brut/parametres/tolerance_effectif" = "tolerance_effectif",
  "fiche/hypotheses_brut/tolerance_effectif" = "tolerance_effectif",
  "etapes/exposition/resultat/exposition/exposition_centrale" = "exposition_centrale",
  "etapes/exposition/resultat/exposition/complement_initial_cellule" = "complement_initial_cellule",
  "etapes/exposition/resultat/exposition/complement_initial_fenetre" = "complement_initial_fenetre",
  "etapes/brut/resultat/poisson/exposition_centrale" = "exposition_centrale",
  "etapes/brut/resultat/poisson/taux" = "taux",
  "etapes/brut/resultat/binomial/exposition_initiale" = "exposition_initiale",
  "etapes/brut/resultat/binomial/taux" = "taux",
  "etapes/brut/resultat/kaplan_meier_sauts/age_exact" = "kaplan_meier_sauts/age_exact",
  "etapes/brut/resultat/kaplan_meier_sauts/variance_log_survie_cellule" = "kaplan_meier_sauts/variance_log_survie_cellule",
  stats::setNames(paste0("borne_", c("inf_exacte", "sup_exacte", "inf_asymptotique", "sup_asymptotique")),
                  paste0("etapes/brut/resultat/poisson/borne_", c("inf_exacte", "sup_exacte", "inf_asymptotique", "sup_asymptotique"))),
  stats::setNames(c("probabilite", paste0("probabilite_borne_", c("inf_exacte", "sup_exacte", "inf_asymptotique", "sup_asymptotique"))),
                  paste0("etapes/brut/resultat/poisson/", c("probabilite", paste0("probabilite_borne_", c("inf_exacte", "sup_exacte", "inf_asymptotique", "sup_asymptotique"))))),
  stats::setNames(paste0("borne_", c("inf_exacte", "sup_exacte", "inf_asymptotique", "sup_asymptotique")),
                  paste0("etapes/brut/resultat/binomial/borne_", c("inf_exacte", "sup_exacte", "inf_asymptotique", "sup_asymptotique"))),
  "etapes/brut/resultat/kaplan_meier/borne_inf_asymptotique" = "borne_inf_asymptotique",
  "etapes/brut/resultat/kaplan_meier/borne_sup_asymptotique" = "borne_sup_asymptotique",
  "etapes/brut/resultat/kaplan_meier/taux" = "kaplan_meier/taux",
  "etapes/brut/resultat/kaplan_meier/variance" = "kaplan_meier/variance",
  "etapes/brut/resultat/kaplan_meier_sauts/survie_cellule" = "kaplan_meier_sauts/survie_cellule")
chemin_autre_etape <- function(chemin) {
  if (startsWith(chemin, "fiche/plan/etapes/")) return(sub("^fiche/plan/etapes/[^/]+/", "fiche/plan/etapes/etape_non_listee/", chemin))
  if (identical(chemin, "fiche/plan/delai_declaration")) return("fiche/plan/etapes/etape_non_listee/delai_declaration")
  if (startsWith(chemin, "fiche/hypotheses_")) return(sub("^fiche/hypotheses_[^/]+/", "fiche/hypotheses_etape_non_listee/", chemin))
  sub("^etapes/[^/]+/", "etapes/etape_non_listee/", chemin)
}
verifier("témoins des clés nues retirées : 36 chemins (un par clé qualifiée) de R6, 23 anciennes clés", {
  length(ANCIENNES_CLES_NUES) == 36L && length(unique(ANCIENNES_CLES_NUES)) == 23L && !anyDuplicated(names(ANCIENNES_CLES_NUES)) &&
    !any(ANCIENNES_CLES_NUES %in% names(TOLERANCES)[!grepl("/", names(TOLERANCES), fixed = TRUE)])
})
for (chemin_ancien in names(ANCIENNES_CLES_NUES)) {
  ancienne <- ANCIENNES_CLES_NUES[[chemin_ancien]]
  chemin_autre <- chemin_autre_etape(chemin_ancien)
  verifier(sprintf("témoin : %s simulé sous une étape non listée (%s) -> relevé par doubles_sans_tolerance(), que la clé %s aurait masqué", chemin_ancien, chemin_autre, ancienne), {
    g <- aplatis[["R6"]]
    if (!identical(unname(g$types[chemin_ancien]), "double")) return(sprintf("%s de R6 : un chemin double attendu", chemin_ancien))
    if (chemin_autre %in% names(g$types)) return(sprintf("%s déjà présent dans R6", chemin_autre))
    g$types[[chemin_autre]] <- g$types[[chemin_ancien]]; g$valeurs[[chemin_autre]] <- g$valeurs[[chemin_ancien]]
    masque <- c(TOLERANCES, stats::setNames(0, ancienne))
    !chemin_ancien %in% doubles_sans_tolerance(g) && chemin_autre %in% doubles_sans_tolerance(g) &&
      !chemin_autre %in% doubles_sans_tolerance(g, masque)
  })
}
# Témoins des invariants de structure (#175), sur des copies en mémoire : une
# clé nue fait échouer (a) ; une clé non qualifiée par l'étape ou une clé d'une
# autre étape classant un chemin d'une étape fait échouer (b) ; une clé non
# qualifiée par fiche/ classant un chemin de la fiche, (c) ; deux clés pour un
# même chemin, (d). Les invariants sont d'abord vrais sur TOLERANCES.
verifier("témoin : une clé nue ajoutée en mémoire à TOLERANCES -> (a) l'identifie", {
  identical(cles_nues(), character(0)) && identical(cles_nues(c(TOLERANCES, taux = 1e-14)), "taux")
})
verifier("témoin : une clé non qualifiée par l'étape, ou d'une autre étape, classant un chemin de brut -> (b) le relève", {
  d <- names(aplatis[["R6"]]$types)[aplatis[["R6"]]$types == "double"]
  c1 <- "etapes/brut/resultat/poisson/taux"
  c2 <- "etapes/brut/positionnement/resultat/cellules/exposition_centrale"
  if (!c1 %in% d) return(sprintf("%s absent de R6", c1))
  # chemin seul classé par la clé d'une autre étape (positionnement/…), sans clé de brut
  c2_cle <- c(TOLERANCES, "positionnement/resultat/cellules/exposition_centrale" = 1e-14)
  # clé nue et clé non qualifiée qui s'ajoutent à la clé qualifiée du chemin
  identical(doubles_etape_mal_classes(d), character(0)) &&
    identical(doubles_etape_mal_classes(c1, c(TOLERANCES, taux = 1e-14)), c1) &&
    identical(doubles_etape_mal_classes(c1, c(TOLERANCES, "poisson/taux" = 1e-14)), c1) &&
    identical(doubles_etape_mal_classes(c2, TOLERANCES), c2) &&
    identical(doubles_etape_mal_classes(c2, c2_cle), c2)
})
verifier("témoin : une clé non qualifiée par fiche/ classant un chemin de la fiche -> (c) le relève", {
  d <- names(aplatis[["R6"]]$types)[aplatis[["R6"]]$types == "double"]
  c1 <- "fiche/hypotheses_brut/niveau"
  if (!c1 %in% d) return(sprintf("%s absent de R6", c1))
  identical(doubles_fiche_mal_classes(d), character(0)) &&
    identical(doubles_fiche_mal_classes(c1, c(TOLERANCES, "hypotheses_brut/niveau" = 0)), c1) &&
    identical(doubles_fiche_mal_classes(c1, c(TOLERANCES, niveau = 0)), c1)
})
verifier("témoin : un chemin classé par deux clés -> (d) le relève", {
  d <- names(aplatis[["R6"]]$types)[aplatis[["R6"]]$types == "double"]
  c1 <- "etapes/brut/resultat/poisson/taux"
  identical(doubles_multi_cles(d), character(0)) &&
    identical(doubles_multi_cles(c1, c(TOLERANCES, taux = 1e-14)), c1)
})
# Un plan listé dans SANS_REFERENCE et sans fichier n'a pas de référence
# commitée à lire : son objet aplati est contrôlé par la boucle des plans
# (aucun double sans classe de tolérance).
restes_commites <- function() lapply(Filter(function(p) file.exists(file.path(DOSSIER_REFERENCES, paste0(p$identifiant, ".tsv"))) ||
                                                    !p$identifiant %in% names(SANS_REFERENCE), plans_reference()), function(p) {
  f <- file.path(DOSSIER_REFERENCES, paste0(p$identifiant, ".tsv"))
  if (!file.exists(f)) return(paste("absent :", f))
  doubles_sans_tolerance(lire_reference(f))
})
restes_vides <- function(restes)
  if (all(lengths(restes) == 0L)) TRUE else paste(names(restes)[lengths(restes) > 0L], collapse = ", ")
nom_restes <- "tolérances : aucun double sans tolérance déclarée dans les références commitées (table sans clé integer)"
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
# Règle absolue à une échelle déclarée et régime de comparaison (ADR 0009,
# annotation du 26/11/2026, (g) 2 et 3 ; #243) : témoins sur des copies
# temporaires de R15 et de R16 (fichiers copiés puis lus ; rien n'est écrit
# sous tests/reference/). Chaque copie altérée repasse par le fichier
# (par_fichier()). S est lue du côté de la référence ; la borne d'un élément
# est tau x facteur x S.
copie_reference <- function(id) {
  f <- tempfile(paste0("reference_", id, "_"), fileext = ".tsv"); on.exit(unlink(f))
  file.copy(file.path(DOSSIER_REFERENCES, paste0(id, ".tsv")), f)
  lire_reference(f)
}
REF15 <- copie_reference("R15"); REF16 <- copie_reference("R16")
modifier <- function(ref, chemin, i, valeur) { ref$valeurs[[chemin]][i] <- valeur; ref }
CH_R0 <- "etapes/lissage/resultat/segments/residu_conservation_0"
CH_R1 <- "etapes/lissage/resultat/segments/residu_conservation_1"
CH_STAT <- "etapes/validation/resultat/statistiques/valeur"
CH_D1 <- "etapes/coherence_regularite/resultat/ages/delta1"
CH_D2 <- "etapes/coherence_regularite/resultat/ages/delta2"
CH_ECART <- "etapes/coherence_regularite/resultat/ordre_segments/ecart"
ligne_stat <- function(ref, stat, grandeur) {
  d <- dirname_chemin(CH_STAT)
  which(ref$valeurs[[joindre(d, "statistique")]] == stat & ref$valeurs[[joindre(d, "grandeur")]] == grandeur)
}
# Un élément couvert par cas : premier élément fini (ligne smr/ecart pour
# statistiques/valeur), sa borne tau x facteur x S lue sur la référence.
CAS_ABSOLUS <- list(
  list(ref = REF16, chemin = CH_R0, i = 1L), list(ref = REF16, chemin = CH_R1, i = 2L),
  list(ref = REF16, chemin = CH_STAT, i = ligne_stat(REF16, "smr", "ecart")),
  list(ref = REF15, chemin = CH_D1, i = which(is.finite(REF15$valeurs[[CH_D1]]))[1L]),
  list(ref = REF15, chemin = CH_D2, i = which(is.finite(REF15$valeurs[[CH_D2]]))[1L]),
  list(ref = REF15, chemin = CH_ECART, i = which(is.finite(REF15$valeurs[[CH_ECART]]))[1L]))
borne_de <- function(cas) {
  e <- echelle_de(cas$chemin, cas$ref)
  e$tau[cas$i] * e$facteur[cas$i] * e$S[cas$i]
}
for (cas in CAS_ABSOLUS) for (q in c(0.9, 1.1)) {
  attendu_v <- if (q < 1) "sous tolérance" else "écart"
  verifier(sprintf("témoin (règle absolue) : %s, élément %d, altéré de %.1f tau x facteur x S -> « %s »", cas$chemin, cas$i, q, attendu_v), {
    a <- cas$ref$valeurs[[cas$chemin]][cas$i]
    r <- comparer_references(cas$ref, par_fichier(modifier(cas$ref, cas$chemin, cas$i, a + q * borne_de(cas))))
    attendu(r, q < 1, cas$chemin, attendu_v)
  })
}
for (cas in CAS_ABSOLUS[c(1L, 3L, 6L)]) {
  verifier(sprintf("témoin (règle absolue) : %s, changement de signe sous la borne (0,3 contre -0,3 tau x facteur x S) -> « sous tolérance »", cas$chemin), {
    B <- borne_de(cas)
    ref <- par_fichier(modifier(cas$ref, cas$chemin, cas$i, 0.3 * B))
    attendu(comparer_references(ref, par_fichier(modifier(cas$ref, cas$chemin, cas$i, -0.3 * B))), TRUE, cas$chemin, "sous tolérance")
  })
  verifier(sprintf("témoin (règle absolue) : %s, zéro contre non nul sous la borne (0 contre 0,5 tau x facteur x S) -> « sous tolérance »", cas$chemin), {
    B <- borne_de(cas)
    ref <- par_fichier(modifier(cas$ref, cas$chemin, cas$i, 0))
    attendu(comparer_references(ref, par_fichier(modifier(cas$ref, cas$chemin, cas$i, 0.5 * B))), TRUE, cas$chemin, "sous tolérance")
  })
}
verifier("témoin (règle absolue) : autre ligne de statistiques/valeur (smr/smr), non couverte, altérée de 2e-12 en relatif -> « écart » (règle relative, classe 1e-12)", {
  i <- ligne_stat(REF16, "smr", "smr")
  r <- comparer_references(REF16, par_fichier(modifier(REF16, CH_STAT, i, REF16$valeurs[[CH_STAT]][i] * (1 + 2e-12))))
  isTRUE(attendu(r, FALSE, CH_STAT, "écart")) && identical(r$grandeurs$position[r$grandeurs$chemin == CH_STAT], i)
})
verifier("témoin (règle absolue) : valeur non finie (NaN) à un élément couvert -> écart structurel inchangé (position des valeurs non finies)", {
  r <- comparer_references(REF16, par_fichier(modifier(REF16, CH_R0, 1L, NaN)))
  isTRUE(attendu(r, FALSE, CH_R0, "écart")) && any(startsWith(r$structure, paste0(CH_R0, " : position des valeurs non finies")))
})
# Déclarations d'échelle invalides : (ix) sélection vide ou ambiguë, (vii)
# échelle absente, (viii) échelle nulle, négative ou non finie, (x) plage
# d'âges au-delà de 120 ; verdict « déclaration d'échelle invalide », plan en
# échec, même si le vecteur est identique ; jamais de repli.
avec_declaration <- function(ligne, selection, echelle) {
  e <- ECHELLES_ABSOLUES; k <- match(ligne, e$cle); e$selection[k] <- selection; e$echelle[k] <- echelle; e
}
invalide_attendu <- function(r, chemin) {
  if (r$ok) return("ok = TRUE au lieu de FALSE")
  v <- verdict(r, chemin)
  if (!identical(v, VERDICT_ECHELLE_INVALIDE)) return(sprintf("verdict « %s » pour %s", paste(v, collapse = ""), chemin))
  if (!any(startsWith(r$echelles_invalides, paste0(chemin, " : ")))) return("raison non relevée dans echelles_invalides")
  if (!any(grepl("DÉCLARATION D'ÉCHELLE INVALIDE", imprimer_comparaison(r), fixed = TRUE, useBytes = TRUE))) return("raison non imprimée")
  TRUE
}
CLE_STAT <- "validation/resultat/statistiques/valeur"
CLE_R0 <- "lissage/resultat/segments/residu_conservation_0"
for (cas in list(
  list(nom = "sélection vide (statistique=smr;grandeur=inexistante)", ech = avec_declaration(CLE_STAT, "statistique=smr;grandeur=inexistante", "statistique=smr;grandeur=observes"), chemin = CH_STAT),
  list(nom = "sélection ambiguë (statistique=smr)", ech = avec_declaration(CLE_STAT, "statistique=smr", "statistique=smr;grandeur=observes"), chemin = CH_STAT),
  list(nom = "échelle d'une sélection absente (grandeur=absente)", ech = avec_declaration(CLE_STAT, "statistique=smr;grandeur=ecart", "statistique=smr;grandeur=absente"), chemin = CH_STAT),
  list(nom = "échelle égale à la sélection", ech = avec_declaration(CLE_STAT, "statistique=smr;grandeur=ecart", "statistique=smr;grandeur=ecart"), chemin = CH_STAT),
  list(nom = "colonne de sélection absente (colonne_absente=smr)", ech = avec_declaration(CLE_STAT, "colonne_absente=smr", "statistique=smr;grandeur=observes"), chemin = CH_STAT),
  list(nom = "colonne soeur d'échelle absente (evenements_absents)", ech = avec_declaration(CLE_R0, NA_character_, "evenements_absents"), chemin = CH_R0),
  list(nom = "colonne soeur d'échelle de type character (sexe)", ech = avec_declaration(CLE_R0, NA_character_, "sexe"), chemin = CH_R0))) {
  verifier(sprintf("témoin (règle absolue) : %s -> « déclaration d'échelle invalide », plan en échec (vecteur identique)", cas$nom),
           invalide_attendu(comparer_references(REF16, REF16, echelles = cas$ech), cas$chemin))
}
CH_EVEN <- "etapes/lissage/resultat/segments/evenements"
CH_QX15 <- "etapes/coherence_regularite/resultat/ages/qx"
CH_GSUP <- "etapes/coherence_regularite/resultat/ordre_segments/g_superieure"
for (cas in list(
  list(nom = "evenements nul", ref = modifier(REF16, CH_EVEN, 1L, 0L), chemins = c(CH_R0, CH_R1)),
  list(nom = "evenements négatif", ref = modifier(REF16, CH_EVEN, 1L, -5L), chemins = c(CH_R0, CH_R1)),
  list(nom = "evenements manquant (NA, non fini)", ref = modifier(REF16, CH_EVEN, 1L, NA_integer_), chemins = c(CH_R0, CH_R1)),
  list(nom = "ligne smr/observes nulle", ref = modifier(REF16, CH_STAT, ligne_stat(REF16, "smr", "observes"), 0), chemins = CH_STAT),
  list(nom = "ligne smr/observes négative", ref = modifier(REF16, CH_STAT, ligne_stat(REF16, "smr", "observes"), -1), chemins = CH_STAT),
  list(nom = "qx non fini (Inf)", ref = modifier(REF15, CH_QX15, which(is.finite(REF15$valeurs[[CH_D1]]) & is.finite(REF15$valeurs[[CH_D2]]))[1L], Inf),
       chemins = c(CH_D1, CH_D2)),
  list(nom = "g_superieure non finie (NaN)", ref = modifier(REF15, CH_GSUP, which(is.finite(REF15$valeurs[[CH_ECART]]))[1L], NaN), chemins = CH_ECART),
  list(nom = "plage_ages du lissage au-delà de 120 (20 à 121)", ref = modifier(REF16, CHEMIN_PLAGE_LISSAGE, 2L, 121), chemins = CH_R1))) {
  verifier(sprintf("témoin (règle absolue) : échelle invalide côté référence (%s) -> « déclaration d'échelle invalide » sur %s, plan en échec", cas$nom,
                   paste(basename(cas$chemins), collapse = " et ")), {
    ref <- par_fichier(cas$ref)
    r <- comparer_references(ref, ref)
    v <- vapply(cas$chemins, function(ch) isTRUE(invalide_attendu(r, ch)), NA)
    autres <- r$grandeurs$verdict == VERDICT_ECHELLE_INVALIDE & !r$grandeurs$chemin %in% cas$chemins
    if (all(v) && !any(autres)) TRUE else
      sprintf("invalides : %s", paste(r$grandeurs$chemin[r$grandeurs$verdict == VERDICT_ECHELLE_INVALIDE], collapse = ", "))
  })
}
verifier("témoin (règle absolue) : plage_ages du lissage à 120 -> déclaration valide (borne incluse) ; residu_conservation_0 non concerné par (x)", {
  ref <- par_fichier(modifier(REF16, CHEMIN_PLAGE_LISSAGE, 2L, 120))
  r <- comparer_references(ref, ref)
  ref2 <- par_fichier(modifier(REF16, CHEMIN_PLAGE_LISSAGE, 2L, 121))
  r2 <- comparer_references(ref2, ref2)
  isTRUE(r$ok) && !length(r$echelles_invalides) && identical(verdict(r2, CH_R0), "identique")
})
verifier("témoin (invariants de la déclaration) : clé absente de TOLERANCES (i), classe inconnue (ii), facteur nul (iii), couple répété et grammaire (iv), déclaration morte (v) -> défauts", {
  e <- ECHELLES_ABSOLUES
  e_i <- rbind(e, data.frame(cle = "lissage/resultat/segments/inexistante", selection = NA_character_, echelle = "evenements",
                             facteur = 1, classe = "abs_1e-10", stringsAsFactors = FALSE))
  e_ii <- e; e_ii$classe[1L] <- "abs_1e-11"
  e_iii <- e; e_iii$facteur[2L] <- 0
  e_iv <- rbind(e, e[1L, ])
  e_g <- e; e_g$selection[3L] <- "statistique==smr"
  d_i <- defauts_echelles(e_i); d_ii <- defauts_echelles(e_ii); d_iii <- defauts_echelles(e_iii)
  d_iv <- defauts_echelles(e_iv); d_g <- defauts_echelles(e_g)
  d_v <- defauts_echelles(references = REFERENCES_LUES["R1"])
  any(grepl("(i)", d_i, fixed = TRUE)) && any(grepl("(ii)", d_ii, fixed = TRUE)) && any(grepl("(iii)", d_iii, fixed = TRUE)) &&
    any(grepl("répété", d_iv, fixed = TRUE, useBytes = TRUE)) && any(grepl("grammaire", d_g, fixed = TRUE)) &&
    length(d_v) == 6L && all(grepl("(v)", d_v, fixed = TRUE)) && !length(defauts_echelles())
})
verifier("témoin (impression) : grandeur jugée par la règle absolue imprimée avec S, facteur et classe ; ligne de sa classe absolue non nulle", {
  cas <- CAS_ABSOLUS[[1L]]
  r <- comparer_references(cas$ref, par_fichier(modifier(cas$ref, cas$chemin, cas$i, cas$ref$valeurs[[cas$chemin]][cas$i] + 0.5 * borne_de(cas))))
  l <- imprimer_comparaison(r)
  S <- echelle_de(cas$chemin, cas$ref)$S[cas$i]
  any(grepl(sprintf("règle absolue : |a - b| / (facteur x S) max 5e-11, S = %.17g, facteur = 1, classe abs_1e-10", S), l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("classe abs_1e-10 : 5e-11 (3 grandeur(s) double)", l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("classe abs_1e-12 : 0 (0 grandeur(s) double)", l, fixed = TRUE, useBytes = TRUE))
})

# Régime de comparaison ((f) 3) : table de vérité de regime_comparaison().
LIGNES_A <- c("Image                     : x", "R                         : 4.3.3")
LIGNES_B <- c("Image                     : y", "R                         : 4.3.3")
entete_de <- function(l) c(identifiant = "R16", stats::setNames(l, rep("plateforme", length(l))), mode = "creation")
verifier("régime : table de vérité de regime_comparaison() (défauts vides et en-tête identique ou différent, défauts non vides et en-tête identique ou sans ligne « plateforme : » -> strict ; défauts non vides et en-tête différent -> constat ; indétermination -> strict)", {
  conf <- list(controle = LIGNES_A, defauts = character(0))
  hors <- list(controle = LIGNES_A, defauts = "La_version() : 3.12.0 au lieu de 3.10.0")
  cas <- list(
    list(conf, entete_de(LIGNES_A), "strict", FALSE), list(conf, entete_de(LIGNES_B), "strict", TRUE),
    list(hors, entete_de(LIGNES_A), "strict", FALSE), list(hors, c(identifiant = "R16", mode = "creation"), "strict", FALSE),
    list(hors, entete_de(LIGNES_B), "constat", FALSE), list(hors, entete_de(LIGNES_A[1L]), "constat", FALSE),
    list(hors, NULL, "strict", FALSE), list(list(controle = LIGNES_A, defauts = NA_character_), entete_de(LIGNES_B), "strict", FALSE),
    list(NULL, entete_de(LIGNES_B), "strict", FALSE))
  r <- vapply(cas, function(k) { x <- regime_comparaison(k[[1L]], k[[2L]]); identical(x$regime, k[[3L]]) && identical(x$mixte, k[[4L]]) }, NA)
  if (all(r)) TRUE else paste("cas en défaut :", paste(which(!r), collapse = ", "))
})
verifier("régime : verrou_regimes() (TABLEXP_IMAGE définie, même vide, ou GITHUB_ACTIONS = true : tout plan strict exigé, sinon échec ; ni l'une ni l'autre : sans objet)", {
  st <- c(R1 = "strict", R16 = "strict"); co <- c(R1 = "strict", R16 = "constat")
  is.na(verrou_regimes(co, image = NA, github_actions = "")) && is.na(verrou_regimes(co, image = NA, github_actions = "false")) &&
    isTRUE(verrou_regimes(st, image = "rocker/r-ver:4.3.3@sha256:0", github_actions = "")) &&
    isTRUE(verrou_regimes(st, image = NA, github_actions = "true")) &&
    is.character(verrou_regimes(co, image = "", github_actions = "")) &&
    is.character(verrou_regimes(co, image = NA, github_actions = "true")) &&
    grepl("R16", verrou_regimes(co, image = "x", github_actions = "true"), fixed = TRUE)
})
verifier("régime : verrou_regimes() avec les chemins de constat passés (tous plans strict, mais une comparaison a reçu des chemins de constat ; TABLEXP_IMAGE vide ou GITHUB_ACTIONS = true -> échec ; aucun chemin passé -> TRUE ; poste -> sans objet) (constat mineur 2 de l'audit léger)", {
  st <- c(R1 = "strict", R16 = "strict")
  avec <- c(R1 = 0L, R16 = 11L); sans <- c(R1 = 0L, R16 = 0L)
  v1 <- verrou_regimes(st, avec, image = "", github_actions = "")
  v2 <- verrou_regimes(st, avec, image = NA, github_actions = "true")
  is.character(v1) && grepl("R16", v1, fixed = TRUE) && is.character(v2) &&
    isTRUE(verrou_regimes(st, sans, image = "", github_actions = "")) &&
    isTRUE(verrou_regimes(st, sans, image = NA, github_actions = "true")) &&
    is.na(verrou_regimes(st, avec, image = NA, github_actions = ""))
})
# Régime de constat ((f) 4, (g) 3) sur une copie de R16 : chemins déclarés du
# plan ; la condition de cause est remplie par un double de
# etapes/lissage/resultat/ altéré sous sa tolérance (table_lissee/qx, classe
# optimisation, x (1 + 1e-6)).
CONSTATS_R16 <- CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == "R16"]
CH_QXT <- "etapes/validation/resultat/cellules/qx_table"
CH_LISSE <- "etapes/lissage/resultat/table_lissee/qx"
lissage_altere <- function(ref = REF16) modifier(ref, CH_LISSE, 1L, ref$valeurs[[CH_LISSE]][1L] * (1 + 1e-6))
qxt_altere <- function(ref) modifier(ref, CH_QXT, 1L, ref$valeurs[[CH_QXT]][1L] * (1 + 1e-9))
verifier("régime : sur la plateforme de référence (strict), qx_table de R16 (déclaré, classe exacte) altéré de 1e-9 -> « écart », plan en échec", {
  attendu(comparer_references(REF16, par_fichier(qxt_altere(lissage_altere()))), FALSE, CH_QXT, "écart")
})
verifier("régime : en régime de constat, même altération, un double du lissage altéré sous sa tolérance -> « constat hors plateforme de référence », compté et imprimé à part, plan non en échec", {
  r <- comparer_references(REF16, par_fichier(qxt_altere(lissage_altere())), constats = CONSTATS_R16)
  l <- imprimer_comparaison(r)
  isTRUE(attendu(r, TRUE, CH_QXT, VERDICT_CONSTAT)) && meme(verdict(r, CH_LISSE), "sous tolérance") &&
    any(grepl("; 1 constat(s) hors plateforme de référence ;", l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("NON JUGÉ hors plateforme de référence sur 1 chemin(s)", l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl("Régime de la comparaison : constat hors plateforme de référence (11 chemin(s)", l, fixed = TRUE, useBytes = TRUE)) &&
    any(grepl(sprintf("    %s | 142 | 1 | ", CH_QXT), l, fixed = TRUE, useBytes = TRUE))
})
verifier("régime : en régime de constat, lissage identique au bit près (condition de cause non remplie), même altération -> « écart », plan en échec", {
  attendu(comparer_references(REF16, par_fichier(qxt_altere(REF16)), constats = CONSTATS_R16), FALSE, CH_QXT, "écart")
})
verifier("régime : en régime de constat, sous_groupes/smr (déclaré) altéré sous sa classe (5e-13) -> « sous tolérance », non constat", {
  c_ <- "etapes/validation/resultat/sous_groupes/smr"; i <- which(is.finite(REF16$valeurs[[c_]]) & REF16$valeurs[[c_]] != 0)[1L]
  g <- modifier(lissage_altere(), c_, i, REF16$valeurs[[c_]][i] * (1 + 5e-13))
  attendu(comparer_references(REF16, par_fichier(g), constats = CONSTATS_R16), TRUE, c_, "sous tolérance")
})
for (cas in list(
  list(nom = "table_lissee/qx (non déclaré) au-delà de 1e-3", chemin = CH_LISSE, facteur = 1 + 2e-3),
  list(nom = "écho du plan validation/parametres/niveau (non déclaré)", chemin = "etapes/validation/parametres/niveau", facteur = 1 + 1e-9),
  list(nom = "validation/resultat/cellules/taux_brut (double non déclaré de la validation)", chemin = "etapes/validation/resultat/cellules/taux_brut", facteur = 1 + 1e-12))) {
  verifier(sprintf("régime : en régime de constat, lissage altéré, %s -> « écart », plan en échec", cas$nom), {
    g <- lissage_altere(); v <- g$valeurs[[cas$chemin]]; i <- which(is.finite(v) & v != 0)[1L]
    attendu(comparer_references(REF16, par_fichier(modifier(g, cas$chemin, i, v[i] * cas$facteur)), constats = CONSTATS_R16),
            FALSE, cas$chemin, "écart")
  })
}
verifier("régime : en régime de constat, clé d'un double du lissage (table_lissee/qx) retirée de TOLERANCES, lissage non altéré (« sans tolérance déclarée », valeurs identiques), qx_table (déclaré) altéré -> « écart », non constat : la cause exige une différence de valeur (constat mineur 1 de l'audit léger)", {
  masque <- TOLERANCES[names(TOLERANCES) != "lissage/resultat/table_lissee/qx"]
  r <- comparer_references(REF16, par_fichier(qxt_altere(REF16)), tolerances = masque, constats = CONSTATS_R16)
  isTRUE(attendu(r, FALSE, CH_QXT, "écart")) && meme(verdict(r, CH_LISSE), "sans tolérance déclarée") &&
    !any(r$grandeurs$verdict == VERDICT_CONSTAT)
})
jamais_constat <- function(r, motif) {
  if (r$ok) return("ok = TRUE au lieu de FALSE")
  if (any(r$grandeurs$verdict == VERDICT_CONSTAT)) return("constat relevé")
  if (!is.null(motif) && !any(grepl(motif, r$structure, fixed = TRUE, useBytes = TRUE))) return(sprintf("structure sans « %s »", motif))
  TRUE
}
verifier("régime : en régime de constat, lissage altéré, sur qx_table (déclaré) : type changé, chemin retiré, chemin ajouté, position d'une valeur non finie déplacée -> échec structurel, jamais constat", {
  g <- qxt_altere(lissage_altere())
  g_type <- g; g_type$types[[CH_QXT]] <- "character"; g_type$valeurs[[CH_QXT]] <- sprintf("%.17g", g$valeurs[[CH_QXT]])
  g_nf <- modifier(g, CH_QXT, 2L, NA_real_)
  c(type = jamais_constat(comparer_references(REF16, par_fichier(g_type), constats = CONSTATS_R16), paste(CH_QXT, ": type")),
    retire = jamais_constat(comparer_references(REF16, par_fichier(retirer(g, CH_QXT)), constats = CONSTATS_R16), paste(CH_QXT, ": manquant")),
    ajoute = jamais_constat(comparer_references(par_fichier(retirer(REF16, CH_QXT)), par_fichier(g), constats = CONSTATS_R16), paste(CH_QXT, ": en trop")),
    non_fini = jamais_constat(comparer_references(REF16, par_fichier(g_nf), constats = CONSTATS_R16), paste(CH_QXT, ": position des valeurs non finies"))) -> v
  if (all(v == "TRUE")) TRUE else paste(names(v)[v != "TRUE"], v[v != "TRUE"], sep = " : ")
})
verifier("régime : en régime de constat, lissage altéré, empreinte des données altérée -> « entrée différente », échec, qx_table altéré en « écart », jamais constat", {
  g <- qxt_altere(lissage_altere()); g$valeurs[[CHEMIN_EMPREINTE_DONNEES]] <- strrep("0", 32L)
  r <- comparer_references(REF16, par_fichier(g), constats = CONSTATS_R16)
  isTRUE(jamais_constat(r, NULL)) && isTRUE(r$entree$differente) && meme(verdict(r, CH_QXT), "écart")
})
verifier("régime : en régime de constat, lissage altéré, élément couvert de statistiques/valeur (smr/ecart, déclaré) altéré au-delà de 1,1 tau x facteur x S -> constat ; même altération en régime strict -> écart", {
  cas <- CAS_ABSOLUS[[3L]]
  g <- modifier(lissage_altere(), CH_STAT, cas$i, REF16$valeurs[[CH_STAT]][cas$i] + 1.1 * borne_de(cas))
  isTRUE(attendu(comparer_references(REF16, par_fichier(g), constats = CONSTATS_R16), TRUE, CH_STAT, VERDICT_CONSTAT)) &&
    isTRUE(attendu(comparer_references(REF16, par_fichier(g)), FALSE, CH_STAT, "écart"))
})
verifier("régime : seul_ecart() strict par défaut, constat sur demande (référence alignée sans changement de valeur sur ce chemin)", {
  g <- qxt_altere(lissage_altere())
  ch <- list(list(chemin = "etapes/validation/resultat/temoin_ajout", nature = "ajout", type = "integer", longueur = 1))
  g2 <- g; g2$types[[ch[[1L]]$chemin]] <- "integer"; g2$valeurs[[ch[[1L]]$chemin]] <- 1L
  g2 <- par_fichier(g2)
  is.character(seul_ecart(REF16, g2, ch)) && isTRUE(seul_ecart(REF16, g2, ch, CONSTATS_R16))
})
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
# Comparaison de forme par jeton (ADR 0009, annotation du 20/10/2026, seconde,
# (i) ; #67) : témoins 1 à 5, sur des copies en mémoire de R6.
CH_RESULTAT <- "fiche/empreintes/resultat"
CH_CODE <- "fiche/empreintes/code"
verifier("jeton (i) 5 : R6 aplati porte le jeton <md5> aux deux seuls chemins, l'objet les valeurs réelles ; aller-retour lire(ecrire(aplatir(x))) identical()", {
  g <- aplatis[["R6"]]
  lu <- par_fichier(g)
  meme(g$valeurs[[CH_RESULTAT]], JETON_MD5) && meme(g$valeurs[[CH_CODE]], JETON_MD5) &&
    identical(unname(g$types[c(CH_RESULTAT, CH_CODE)]), c("character", "character")) &&
    grepl("^[0-9a-f]{32}$", x6_ref$fiche$empreintes$resultat) && grepl("^[0-9a-f]{32}$", x6_ref$fiche$empreintes$code) &&
    identical(lu$types, g$types) && identical(lu$valeurs, g$valeurs)
})
verifier("jeton (i) 1 : empreinte de résultat ou de code NA, en majuscules ou de longueur 2 -> gardée telle quelle, écart sur son chemin", {
  essai <- function(chemin, valeur) {
    y <- x6_ref
    y$fiche$empreintes[[sub("^fiche/empreintes/", "", chemin)]] <- valeur
    v <- aplatir_construction(y)$valeurs[[chemin]]
    g <- base; g$valeurs[[chemin]] <- v
    r <- comparer_references(ref0, par_fichier(g))
    identical(v, valeur) && !r$ok &&
      (length(valeur) != 1L || meme(verdict(r, chemin), "écart")) &&
      (length(valeur) == 1L || identical(r$structure, paste(chemin, ": longueur 2 au lieu de 1")))
  }
  essai(CH_RESULTAT, NA_character_) && essai(CH_CODE, NA_character_) &&
    essai(CH_RESULTAT, toupper(x6_ref$fiche$empreintes$resultat)) &&
    essai(CH_CODE, rep(x6_ref$fiche$empreintes$code, 2L))
})
verifier("jeton (i) 2 : empreinte des données altérée (32 hexadécimaux) -> non remplacée par le jeton, « entrée différente »", {
  y <- x6_ref; y$fiche$empreintes$donnees <- strrep("a", 32L)
  v <- aplatir_construction(y)$valeurs[[CHEMIN_EMPREINTE_DONNEES]]
  g <- base; g$valeurs[[CHEMIN_EMPREINTE_DONNEES]] <- v
  r <- comparer_references(ref0, par_fichier(g))
  meme(v, strrep("a", 32L)) && !r$ok && isTRUE(r$entree$differente)
})
verifier("jeton (i) 3 : lignes @empreinte des tables résumées en valeur (32 hexadécimaux), une ligne altérée reste un écart", {
  ch <- grep("/@empreinte$", names(base$types), value = TRUE)
  g <- base; g$valeurs[[ch[1L]]] <- strrep("0", 32L)
  r <- comparer_references(ref0, par_fichier(g))
  length(ch) >= 1L && all(vapply(base$valeurs[ch], function(v) grepl("^[0-9a-f]{32}$", v), NA)) &&
    !r$ok && meme(verdict(r, ch[1L]), "écart")
})
verifier("jeton (i) 4 : l'en-tête porte les deux valeurs réelles ; une autre valeur dans l'en-tête ne change pas la comparaison", {
  e <- entete_reference(list(identifiant = "R6", description = "temoin"), list(controle = c(a = "x"), consigne = character(0)),
                        x6_ref, list(commit = "c", execution = "e", adresse = "a"), "releve", "2026-10-20 00:00:00 UTC")
  e2 <- e
  e2[names(e2) %in% c("fiche$empreintes/resultat", "fiche$empreintes/code")] <- strrep("f", 32L)
  f1 <- tempfile(fileext = ".tsv"); f2 <- tempfile(fileext = ".tsv")
  ecrire_reference(base, f1, e); ecrire_reference(base, f2, e2)
  r1 <- lire_reference(f1); r2 <- lire_reference(f2); unlink(c(f1, f2))
  c1 <- comparer_references(r1, base); c2 <- comparer_references(r2, base)
  meme(unname(r1$entete[["fiche$empreintes/resultat"]]), x6_ref$fiche$empreintes$resultat) &&
    meme(unname(r1$entete[["fiche$empreintes/code"]]), x6_ref$fiche$empreintes$code) &&
    meme(unname(r2$entete[["fiche$empreintes/code"]]), strrep("f", 32L)) &&
    c1$ok && c2$ok && identical(c1$grandeurs, c2$grandeurs)
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
verifier("témoin : référence orpheline détectée (R99.tsv dans un dossier temporaire)", {
  d <- tempfile("references_"); dir.create(d)
  file.copy(F_BASE, file.path(d, "R6.tsv")); file.copy(F_BASE, file.path(d, "R99.tsv"))
  o <- references_orphelines(d); unlink(d, recursive = TRUE)
  identical(o, "R99.tsv")
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
  list(args = c("--mode", "creation", "--plans", "R99", "--sortie", tempfile(), "--motif", "x"), motif = "--plans : ", env = EN_CI),
  list(args = c("--mode", "regeneration", "--plans", "R99", "--sortie", tempfile(), "--motif", "x"), motif = "--plans : ", env = EN_CI),
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
      "R1 = list(identifiant = 'R1', description = 'temoin valide', plan = plan_reference(), entree = 'reference'),",
      "R2 = list(identifiant = 'R2', description = 'temoin invalide', plan = plan_reference(niveau = 2), entree = 'reference'))",
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
      "R1 = list(identifiant = 'R1', description = 'd\\u00e9c\\u00e8s', plan = plan_reference(), entree = 'reference'))",
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
verifier("T3 : sonde UTF-8 de l'installation de la batterie (STATUT_EMPREINTE_RESULTAT)", isTRUE(sonde_utf8()))
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
    types = c("fiche/empreintes/donnees" = "character", "fiche/version" = "character", "brut/resultat/poisson/taux" = "double",
              "s/cellules" = "integer", "s/n" = "integer", "s/nouveau" = "integer"),
    valeurs = list("fiche/empreintes/donnees" = strrep("a", 32L), "fiche/version" = "v2", "brut/resultat/poisson/taux" = c(0.1, 0.2, 0.3),
                   "s/cellules" = c(149L, 75L), "s/n" = c(1L, 2L, 3L), "s/nouveau" = 7L))
  # États d'avant, un par nature.
  def_type <- function(r) { r$types[["s/cellules"]] <- "double"; r$valeurs[["s/cellules"]] <- c(149, 75); r }
  def_valeur <- function(r) { r$valeurs[["brut/resultat/poisson/taux"]][3L] <- 0.35; r }
  def_chaine <- function(r) { r$valeurs[["fiche/version"]] <- "v1"; r }
  def_ajout <- function(r) { r$types <- r$types[names(r$types) != "s/nouveau"]; r$valeurs <- r$valeurs[names(r$valeurs) != "s/nouveau"]; r }
  def_retrait <- function(r) { r$types[["s/ancien"]] <- "integer"; r$valeurs[["s/ancien"]] <- 5L; r }
  derive <- function(r) { r$valeurs[["s/n"]][2L] <- 9L; r }  # dérive non déclarée
  ch <- list(
    type = list(chemin = "s/cellules", nature = "type", de = "double", vers = "integer"),
    valeur = list(chemin = "brut/resultat/poisson/taux", nature = "valeur", valeurs = 0.3, positions = 3L),
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
  # logical -> character (#177) : vecteur vide seulement.
  vide_logique <- function(r) { r$types[["s/etat"]] <- "logical"; r$valeurs[["s/etat"]] <- logical(0); r }
  nouveau_etat <- nouveau
  nouveau_etat$types[["s/etat"]] <- "character"; nouveau_etat$valeurs[["s/etat"]] <- character(0)
  ch_etat <- list(chemin = "s/etat", nature = "type", de = "logical", vers = "character")
  verifier("témoin 9.5, type logical -> character (#177) : vecteur vide, la comparaison stricte échoue, le seul écart déclaré passe, un écart non déclaré en plus échoue",
           !comparer_references(vide_logique(nouveau_etat), nouveau_etat)$ok &&
             isTRUE(seul_ecart(vide_logique(nouveau_etat), nouveau_etat, fixe(ch_etat))) &&
             is.character(seul_ecart(derive(vide_logique(nouveau_etat)), nouveau_etat, fixe(ch_etat))))
  verifier("témoin 9.5, type logical -> character (#177) : vecteur non vide refusé ; type déjà character non observé ; type logical -> integer refusé par valider_transitions()", {
    plein <- vide_logique(nouveau_etat); plein$valeurs[["s/etat"]] <- c(TRUE, NA)
    r1 <- seul_ecart(plein, nouveau_etat, fixe(ch_etat))
    r2 <- seul_ecart(nouveau_etat, nouveau_etat, fixe(ch_etat))
    if (!(is.character(r1) && grepl("non vide", r1, fixed = TRUE, useBytes = TRUE))) paste("vecteur non vide :", paste(r1, collapse = " "))
    else if (!(is.character(r2) && grepl("type déjà character", r2, fixed = TRUE, useBytes = TRUE))) paste("type déjà character :", paste(r2, collapse = " "))
    else if (length(valider_transitions(list(tr("#901", "R1", ch_etat))))) "logical -> character refusé par valider_transitions()"
    else if (!length(valider_transitions(list(tr("#901", "R1", modifyList(ch_etat, list(vers = "integer"))))))) "logical -> integer accepté"
    else if (!length(valider_transitions(list(tr("#901", "R1", modifyList(ch_etat, list(de = "character", vers = "logical"))))))) "character -> logical accepté"
    else TRUE
  })
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
  n2 <- nouveau; n2$valeurs[["s/cellules"]] <- c(149L, 80L); n2$valeurs[["brut/resultat/poisson/taux"]] <- c(0.15, 0.2, 0.3)
  old2 <- def_type(n2); old2$valeurs[["brut/resultat/poisson/taux"]][1L] <- 0.1; old2$valeurs[["brut/resultat/poisson/taux"]][3L] <- 0.35
  v1 <- list(chemin = "brut/resultat/poisson/taux", nature = "valeur", valeurs = 0.15, positions = 1L)
  v3 <- list(chemin = "brut/resultat/poisson/taux", nature = "valeur", valeurs = 0.3, positions = 3L)
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
    list("élément non liste (list(\"#35\"), au lieu d'une erreur R brute)", list("#35")),
    list("issue en double sur un plan commun", list(tr("#901", c("R1", "R2"), ch$ajout), tr("#901", "R2", ch$retrait))),
    list("plan de référence inconnu", list(tr("#901", "R99", ch$ajout))),
    list("aucun changement", list(tr("#901", "R1"))),
    list("champ hors de la nature (valeurs sur un type)", list(tr("#901", "R1", modifyList(ch$type, list(valeurs = 1))))),
    list("conversion de type hors double / integer", list(tr("#901", "R1", modifyList(ch$type, list(vers = "character"))))),
    list("valeurs sans positions de même longueur", list(tr("#901", "R1", modifyList(ch$valeur, list(positions = 1:2))))),
    list("positions dupliquées", list(tr("#901", "R1", modifyList(ch$valeur, list(valeurs = c(1, 2), positions = c(1L, 1L)))))),
    list("valeur de chaîne numérique", list(tr("#901", "R1", modifyList(ch[["valeur de chaîne"]], list(valeurs = 1)))))))
    verifier(sprintf("témoin 9.5, forme : %s -> relevé par valider_transitions()", cas[[1L]]), length(valider_transitions(cas[[2L]])) > 0L)

  # Doubles sans tolérance déclarée des références à régénérer.
  verifier("témoin 9.5, restes : type converti depuis double et retrait de type double admis ; retrait d'un autre type, valeur, ajout et autre plan non admis", {
    restes <- list(R1 = c("s/cellules", "s/vieux", "s/ancien", "brut/resultat/poisson/taux"), R2 = "s/cellules")
    vieux <- list(chemin = "s/vieux", nature = "retrait", type = "double", longueur = 1L)
    r <- restes_hors_transitions(restes, list(tr("#901", "R1", ch$type, vieux, ch$retrait, ch$valeur, ch$ajout)))
    identical(r$R1, c("s/ancien", "brut/resultat/poisson/taux")) && identical(r$R2, "s/cellules") &&
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

# --- dates : aller-retour des références, années sur quatre chiffres, jeton NaN (#19) ---

local({
  J <- function(j) structure(j, class = "Date")
  v <- J(c(-354650, -719162, 2932896, NA, NaN, 19782, 0))   # 0999-01-01, 0001-01-01, 9999-12-31, NA, NaN
  g <- list(types = c(x = "Date"), valeurs = list(x = v))
  f <- tempfile(fileext = ".tsv"); on.exit(unlink(f))
  ecrire_reference(g, f)
  texte <- rawToChar(readBin(f, "raw", file.info(f)$size))
  verifier("références, Date : 0999-01-01, 0001-01-01, 9999-12-31, NA, NaN écrits en ISO sur quatre chiffres, jetons NA et NaN distincts",
           grepl("\tDate\t7\t0999-01-01\t0001-01-01\t9999-12-31\tNA\tNaN\t2024-02-29\t1970-01-01\n", texte, fixed = TRUE))
  verifier("références, Date : aller-retour lire(ecrire()) identical() sur l'objet relu, NA et NaN distingués",
           identical(lire_reference(f)$valeurs$x, v) && !identical(lire_reference(f)$valeurs$x, J(c(-354650, -719162, 2932896, NA, NA, 19782, 0))))
  refus_lecture <- function(motif_ecrit) {
    f2 <- tempfile(fileext = ".tsv"); on.exit(unlink(f2))
    writeBin(charToRaw(motif_ecrit(texte)), f2)
    tryCatch({ lire_reference(f2); "aucune erreur" }, error = function(e) conditionMessage(e))
  }
  verifier("références, Date : lecture d'une année sans zéros (999-01-01) ou de NaN écrit nan -> erreur « non canonique »",
           grepl("non canonique", refus_lecture(function(t) sub("\t0999-01-01\t", "\t999-01-01\t", t, fixed = TRUE)), fixed = TRUE) &&
             grepl("non canonique", refus_lecture(function(t) sub("\tNaN\t", "\tnan\t", t, fixed = TRUE)), fixed = TRUE))
  verifier("références, Date : une date hors domaine ou non entière est refusée à l'écriture (refus_reference)",
           all(vapply(list(J(-719163), J(2932897), J(100.5)), function(w)
             inherits(tryCatch(ecrire_reference(list(types = c(x = "Date"), valeurs = list(x = w)), tempfile()),
                               refus_reference = function(e) e), "refus_reference"), NA)))
  # Lecteur : même domaine que l'écrivain du moteur (#65).
  verifier("références, Date : lecture de 0000-12-31 (fichier ou valeurs_lues) -> erreur « date_hors_domaine »",
           grepl("date_hors_domaine", refus_lecture(function(t) sub("\t0001-01-01\t", "\t0000-12-31\t", t, fixed = TRUE)), fixed = TRUE) &&
             grepl("date_hors_domaine", tryCatch(valeurs_lues("Date", "0000-12-31", "(f)", "x"), error = function(e) conditionMessage(e)), fixed = TRUE))
  verifier("références, Date : 0001-01-01 et 9999-12-31 relues à l'identique par valeurs_lues (témoins des bornes)",
           identical(valeurs_lues("Date", c("0001-01-01", "9999-12-31"), "(f)", "x"), J(c(-719162, 2932896))))
  verifier("références, Date : bornes JOURS_DATES_REFERENCE du lecteur = dates_hors_domaine() du moteur (bornes dans le domaine, bornes -1 et +1 hors domaine)",
           !any(tablexp:::dates_hors_domaine(JOURS_DATES_REFERENCE)) &&
             all(tablexp:::dates_hors_domaine(JOURS_DATES_REFERENCE + c(-1, 1))))
})

# --- plan canonique : plans de référence déjà canoniques ; transitions d'une même issue (#42) ---

local({
  cn <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES, tablexp:::ordres_canoniques())
  # Exception unique : R15 déclare ordres = c(3, 2, 1), non trié exprès
  # (consigne d'actuary, étape (R) de B19), pour épingler la canonisation par
  # tri croissant (TRI_NUMERIQUE ; #144) : canoniser_plan() ne change que ce
  # paramètre, rendu c(1, 2, 3).
  for (id in names(plans_reference())) {
    p <- plans_reference()[[id]]$plan
    if (id == "R15") {
      q <- p
      q$etapes$coherence_regularite$ordres <- c(1, 2, 3)
      verifier("R15 : plan de référence canonique hors ordres déclaré c(3, 2, 1), que canoniser_plan() trie en c(1, 2, 3) (TRI_NUMERIQUE, #144)",
               identical(p$etapes$coherence_regularite$ordres, c(3, 2, 1)) && identical(cn(p), q) && identical(cn(q), q))
    } else
      verifier(sprintf("%s : plan de référence déjà canonique (canoniser_plan() le rend identical())", id), identical(cn(p), p))
  }
  ch <- list(chemin = "s/n", nature = "ajout", type = "integer", longueur = 1L)
  t <- function(issue, plans) list(issue = issue, plans = plans, changements = list(ch))
  verifier("témoin 9.5, forme : deux transitions d'une même issue à plans disjoints -> liste bien formée",
           !length(valider_transitions(list(t("#901", c("R1", "R3")), t("#901", "R2")))))
  verifier("témoin 9.5, forme : deux transitions d'une même issue partageant un plan -> refus nommé citant le plan commun",
           any(grepl("même(s) plan(s) R3", valider_transitions(list(t("#901", c("R1", "R3")), t("#901", c("R2", "R3")))),
                     fixed = TRUE)))
})


# --- sérialisation (#50 ; ADR 0011, points 6.4 et 12) : plans et fiches de R1 à R17 ---

# Aller-retour texte <-> objet identical() du plan et de la fiche ; rejeu
# construire_table() sur le plan relu de la fiche écrite, identical() hors
# execution à l'objet comparé aux références ci-dessus (même entrée, même
# plan) ; le plan relu passe par valider_plan() (ADR 0011, point 9.2).
fichier_courant("sérialisation (#50) : plans et fiches de R1 à R17, aller-retour identical(), rejeu depuis le texte")
local({
  for (p in plans_reference()) {
    id <- p$identifiant
    entree_p <- entrees_ref[[p$entree]]
    x <- construire_table(entree_p$donnees, entree_p$roles, p$plan, tables_reference = tables_du_plan(p))
    verifier(sprintf("%s : lire_plan(ecrire_plan(plan)) identical() au plan", id),
             identical(lire_plan(ecrire_plan(p$plan)), p$plan))
    f <- lire_fiche(ecrire_fiche(x$fiche))
    verifier(sprintf("%s : lire_fiche(ecrire_fiche(fiche)) identical() à la fiche", id), identical(f, x$fiche))
    # #57 : lignes reçues = lignes conservées + lignes distinctes exclues ;
    # aucune exclusion déclarée dans R1 à R7, et R8 et R10 en déclarent deux dont aucune règle ne
    # touche l'entrée de référence : 0L et aucune ligne écartée. R9 déclare trois
    # exclusions sur une entrée qui porte NOMBRE_PAR_MOTIF_R9 lignes de chacun des
    # motifs correspondants : MOTIFS_DEFAUTS_R9 x NOMBRE_PAR_MOTIF_R9 lignes exclues.
    exclues_attendues <- if (id == "R9") length(MOTIFS_DEFAUTS_R9) * NOMBRE_PAR_MOTIF_R9 else 0L
    verifier(sprintf("%s : lignes reçues = lignes conservées + nombre_lignes_exclues (%dL, integer) (#57)", id, exclues_attendues),
             identical(x$fiche$nombre_lignes_exclues, as.integer(exclues_attendues)) &&
               nrow(entree_p$donnees) == nrow(x$etapes$donnees$resultat$lignes_conservees) + x$fiche$nombre_lignes_exclues)
    y <- construire_table(entree_p$donnees, f$roles, valider_plan(f$plan), f$provenance, tables_reference = tables_du_plan(p))
    verifier(sprintf("%s : rejeu sur le plan relu de la fiche écrite identical() hors execution", id),
             identical(sans_execution(y), sans_execution(x)))
    if (id != "R1") next
    # Fiche de R1 (ADR 0011, troisième annotation, (d) et (h)) : tout préfixe
    # strict du corps, coupé à une frontière de ligne, est refusé ; les blocs de
    # lecture partitionnent les champs de la fiche.
    l <- strsplit(rawToChar(ecrire_fiche(x$fiche)), "\n", fixed = TRUE)[[1L]]
    entete <- l[seq_len(match(FALSE, startsWith(l[-1L], "#")))]
    corps <- l[-seq_along(entete)]
    acceptes <- Filter(function(m) !inherits(tryCatch(lire_fiche(charToRaw(enc2utf8(paste0(paste(c(entete, corps[seq_len(m)]),
                                                                                                collapse = "\n"), "\n")))),
                                                      refus_tablexp = function(e) e), "refus_tablexp"),
                       0:(length(corps) - 1L))
    verifier(sprintf("R1 : chacun des %d préfixes stricts du corps de la fiche écrite refusé par lire_fiche()", length(corps)),
             if (!length(acceptes)) TRUE else sprintf("préfixe(s) accepté(s) : %s lignes", paste(acceptes, collapse = ", ")))
    b <- unlist(tablexp:::BLOCS_LECTURE_FICHE)
    # hypotheses_recette : champ conditionnel, absent sous la recette des
    # lignes (ADR 0016, point 8.1 ; #169).
    verifier("R1 : BLOCS_LECTURE_FICHE partitionne names(fiche) (aucun champ manquant, en double ni étranger ; seul hypotheses_recette, conditionnel, absent)",
             setequal(setdiff(b, "hypotheses_recette"), names(x$fiche)) && length(b) == length(x$fiche) + 1L &&
               !anyDuplicated(b))
  }
})

bilan()
