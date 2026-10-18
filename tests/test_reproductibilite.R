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
#  comparer_references.R ; chaînes non ASCII et locale (T1 à T4, M54).
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
  else verifier(nom, comparer_au_fichier(id, g))
}
verifier("aucune référence orpheline sous tests/reference/ (point 3.7)", {
  o <- if (dir.exists(DOSSIER_REFERENCES)) references_orphelines(DOSSIER_REFERENCES) else character(0)
  if (length(o)) paste("orphelines :", paste(o, collapse = ", ")) else TRUE
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
  identical(doubles_sans_tolerance(g), c_) && !length(doubles_sans_tolerance(g, masque))
})
verifier("tolérances : aucun double sans tolérance déclarée dans les six références commitées (table sans clé integer)", {
  restes <- lapply(plans_reference(), function(p) {
    f <- file.path(DOSSIER_REFERENCES, paste0(p$identifiant, ".tsv"))
    if (!file.exists(f)) return(paste("absent :", f))
    doubles_sans_tolerance(lire_reference(f))
  })
  if (all(lengths(restes) == 0L)) TRUE else paste(names(restes)[lengths(restes) > 0L], collapse = ", ")
})
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
# installation ni écriture) ; ligne de commande de comparaison.
PRODUCTEUR <- file.path(RACINE, "tests", "reference", "produire_references.R")
COMPARATEUR <- file.path(RACINE, "tests", "reference", "comparer_references.R")
for (cas in list(
  list(args = c("--mode", "inconnu", "--plans", "tous", "--sortie", tempfile()), motif = "--mode parmi"),
  list(args = c("--mode", "creation", "--plans", "tous", "--sortie", tempfile()), motif = "--motif exigé"),
  list(args = c("--mode", "bascule", "--plans", "R1", "--sortie", tempfile(), "--motif", "x"), motif = "--plans tous obligatoire"),
  list(args = c("--mode", "creation", "--plans", "R9", "--sortie", tempfile(), "--motif", "x"), motif = "--plans : "),
  list(args = c("--mode", "creation", "--plans", "tous", "--sortie", shQuote(file.path(DOSSIER_REFERENCES, "essai")), "--motif", "x"),
       motif = "ne peut être tests/reference/"),
  list(args = c("--mode", "releve", "--plans", "tous", "--sortie", tempfile()), motif = "mode releve refusé sur la CI",
       env = "GITHUB_ACTIONS=true"))) {
  verifier(sprintf("producteur : appel refusé (%s), code 1, sans rien écrire sous tests/reference/", cas$motif), {
    contenu <- function() if (dir.exists(DOSSIER_REFERENCES))
      list.files(DOSSIER_REFERENCES, recursive = TRUE, all.files = TRUE, include.dirs = TRUE) else NA
    avant <- contenu()
    r <- executer_rscript(PRODUCTEUR, cas$args, env = if (is.null(cas$env)) "GITHUB_ACTIONS=false" else cas$env)
    if (r$statut != 1L) sprintf("code %d", r$statut)
    else if (!any(grepl(cas$motif, r$sortie, fixed = TRUE, useBytes = TRUE))) paste("sortie :", paste(r$sortie, collapse = " | "))
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

# Locale UTF-8 disponible (même liste que le harnais) ; "" si aucune.
LOCALE_UTF8 <- local({
  ancienne <- Sys.getlocale("LC_CTYPE"); on.exit(Sys.setlocale("LC_CTYPE", ancienne))
  ok <- vapply(LOCALES_UTF8, function(l) nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l))), NA)
  if (any(ok)) LOCALES_UTF8[ok][1L] else ""
})
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

bilan()
