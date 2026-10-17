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
#  plan sans kaplan_meier. À venir : comparaison aux références de
#  non-régression (#17).
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
# tests/unitaires/test_construire_table.R), recalculée dans deux processus
# de locales différentes.
EMPREINTE_ATTENDUE <- "80e1ed7c22b2c1b6086260ce4490d64c"
emp_code <- c(
  "D <- as.Date",
  "d <- data.frame(c = c('deces', 'en_cours', NA), e = D(c('2020-03-01', '2016-05-20', '2024-02-29')),",
  "  n = D(c('1960-07-01', '1945-10-15', '1964-02-29')), s = D(c(NA, '2019-02-10', '2024-12-31')),",
  "  ic = c(1, 2, 3e10), it = c(1L, NA, 3L), sg = c('a\"b', 'c\\\\d\\te', '\\u00e9t\\u00e9'),",
  "  sx = factor(c('H', 'F', 'H')), stringsAsFactors = FALSE)",
  "Encoding(d$sg) <- 'UTF-8'",
  "roles <- c(sexe = 'sx', segment = 'sg', id_tete = 'it', id_contrat = 'ic', date_sortie = 's',",
  "           date_naissance = 'n', date_entree = 'e', cause_sortie = 'c')")
for (lang in c("C", "C.UTF-8")) {
  script <- tempfile(fileext = ".R")
  sortie <- tempfile(fileext = ".txt")
  writeLines(c(sprintf("library(tablexp, lib.loc = %s)", deparse(BIBLIOTHEQUE)), emp_code,
               sprintf("writeLines(c(Sys.getlocale('LC_CTYPE'), tablexp:::empreinte_donnees(d, roles)), %s)",
                       deparse(sortie))), script)
  r <- executer_rscript(script, env = c(paste0("LANG=", lang), paste0("LC_ALL=", lang)))
  verifier(sprintf("empreinte, sous-processus LANG=%s : égale à la valeur calculée hors R (%s)", lang,
                   EMPREINTE_ATTENDUE),
           if (r$statut != 0L) paste(r$sortie, collapse = " | ")
           else if (!file.exists(sortie)) "fichier de sortie du sous-processus absent"
           else {
             lu <- readLines(sortie)
             if (!identical(lu[1L], lang)) sprintf("locale effective %s au lieu de %s", lu[1L], lang)
             else if (identical(lu[2L], EMPREINTE_ATTENDUE)) TRUE
             else sprintf("locale %s : empreinte %s", lu[1L], lu[2L])
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
    (if (n_perdus) identical(a$nombre, as.numeric(n_perdus)) else !nrow(a))
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

bilan()
