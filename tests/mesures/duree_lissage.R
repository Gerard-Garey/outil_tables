# -----------------------------------------------------------------------------
#  tests/mesures/duree_lissage.R : durée de l'étape `lissage` sous
#  Whittaker-Henderson (b) (issue #225, commit (3) de B19 ; ADR 0017, point
#  9.2 : « mesure de la durée réelle au commit (3) par un script de
#  tests/mesures/ (hors CI), citée dans le commit »)
#
#  Hors des batteries (aucun seuil). Plans : étapes donnees, exposition, brut
#  et lissage des plans prévus pour R16 (entrée `reference`, lambda choisi par
#  la LAML sur [1, 1e12], z = 3, plage 20 à 90) et R17 (entrée
#  `agregats_reference`, lambda déclaré 1e7) (spécification de forme (0')(b),
#  § 5), sans l'étape validation (aucune graine) ; référence : les mêmes
#  plans sans l'étape lissage. REPETITIONS mesures de chaque construction
#  après gc(), médiane et étendue ; durée de l'étape = différence des
#  médianes.
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/duree_lissage.R [repetitions=5]
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
installer_tablexp(RACINE)
local(for (e in parse(file.path(RACINE, "tests", "reference", "outils_reference.R"), encoding = "UTF-8",
                      keep.source = FALSE)) eval(e, globalenv()))

ARGS <- commandArgs(trailingOnly = TRUE)
a <- grep("^repetitions=", ARGS, value = TRUE)
REPETITIONS <- if (length(a)) as.integer(sub("^repetitions=", "", a[1L])) else 5L

avec_lissage <- function(plan, lissage) {
  plan$etapes$lissage <- lissage
  plan
}
LISSAGE_R16 <- list(methode = "wh_poisson", plage_ages = c(20, 90), ordre_difference = 3, critere_lissage = "laml",
                    niveau = 0.95, intervalle_lissage = c(1, 1e12))
LISSAGE_R17 <- list(methode = "wh_poisson", plage_ages = c(20, 90), ordre_difference = 3,
                    critere_lissage = "declare", niveau = 0.95, parametre_lissage = 1e7)
cas <- list(
  list(nom = "R16 (laml)", entree = jeu_reference(), sans = plan_reference(),
       avec = avec_lissage(plan_reference(), LISSAGE_R16)),
  list(nom = "R17 (declare)", entree = agregats_reference(), sans = plan_reference(convention = "dernier_anniversaire"),
       avec = avec_lissage(plan_reference(convention = "dernier_anniversaire"), LISSAGE_R17)))

mesurer <- function(entree, plan) vapply(seq_len(REPETITIONS), function(i) {
  gc()
  t0 <- proc.time()[["elapsed"]]
  construire_table(entree$donnees, entree$roles, plan)
  proc.time()[["elapsed"]] - t0
}, 0)

for (k in cas) {
  t_sans <- mesurer(k$entree, k$sans)
  t_avec <- mesurer(k$entree, k$avec)
  cat(sprintf("%s : construction sans lissage %.3f s [%.3f ; %.3f], avec lissage %.3f s [%.3f ; %.3f] ; étape lissage %.3f s (%d répétitions)\n",
              k$nom, stats::median(t_sans), min(t_sans), max(t_sans), stats::median(t_avec), min(t_avec), max(t_avec),
              stats::median(t_avec) - stats::median(t_sans), REPETITIONS))
}
bilan(code_sortie = FALSE)
