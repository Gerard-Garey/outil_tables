# -----------------------------------------------------------------------------
#  tests/mesures/tolerance_optimisation.R : mesure de la classe de tolérance
#  `optimisation` des grandeurs issues d'un calcul itératif ou d'une
#  optimisation (issue #225, commit (3) de B19 ; ADR 0017, points 8.2 et 8.3 ;
#  spécification de forme (0')(b) d'actuary-approfondi, § 6 ; décision du
#  mainteneur du 11/11/2026, option (delta), M416)
#
#  Hors des batteries et hors CI (aucun seuil ; mesure consignée dans le
#  commit). Plans : ceux prévus pour R16 (entrée `reference`, WH (b), lambda
#  choisi par la LAML sur [1, 1e12], z = 3, validation lissee, graine
#  22011202) et R17 (entrée `agregats_reference`, convention
#  dernier_anniversaire, WH (b), lambda déclaré 1e7, validation lissee,
#  graine 22011203), recopiés de la spécification (§ 5) ; ils ne sont pas
#  encore dans plans_reference() (création des références à l'étape (R)).
#
#  Perturbations, sans aucun crochet de test dans le code de production :
#   (i)   borne a de intervalle_lissage (R16) multipliée par (1 + 2^-52) et
#         par (1 - 2^-52) ;
#   (ii)  borne b de même ;
#   (iii) lambda déclaré de R17 multiplié par (1 ± 2^-52) ;
#   (iv)  lignes de `donnees` permutées (R16 et R17 ; l'agrégation de
#         l'exposition en dépend : sommes dans un autre ordre), graines de
#         permutation 22028701 (R16) et 22028702 (R17), réserve k = 7 de la
#         plage de la porteuse (tests/controles/graines_admises.csv).
#  Mesure : pour chaque grandeur double de etapes/lissage et de
#  etapes/validation (feuilles des listes, colonnes des data.frame), écart
#  relatif maximal |a - b| / max(|a|, |b|) entre l'exécution nominale et
#  chaque exécution perturbée (définition de comparer_references(),
#  tests/reference/outils_reference.R) ; changement de signe compté à part
#  (la comparaison des références l'exige). Les résidus de conservation,
#  nuls à la convergence près, sont rapportés à part : leur écart relatif
#  n'a pas de sens (valeurs de l'ordre de l'arrêt de l'algorithme).
#  Valeur proposée (spécification, § 6) : plus petite classe décimale >= 10 x
#  l'écart relatif maximal des chemins `lissage/*` (hors résidus) ; les
#  chemins de la validation gardent 1e-12 (option (delta)) : leur écart est
#  imprimé pour contrôle.
#
#  Usage (depuis la racine du dépôt, locale UTF-8 requise) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/tolerance_optimisation.R
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

# --- plans de R16 et R17 (spécification de forme, § 5) ---------------------------

VALIDATION_LISSEE <- list(relation_table = "lissee", plage_ages = c(20, 90), tranches_age = c(20, 50, 70),
                          niveau = 0.95, replications = 999)
avec_lissage <- function(plan, lissage) {
  plan$etapes <- c(plan$etapes[c("donnees", "exposition", "brut")], list(lissage = lissage),
                   plan$etapes["validation"])
  plan
}
LISSAGE_R16 <- list(methode = "wh_poisson", plage_ages = c(20, 90), ordre_difference = 3, critere_lissage = "laml",
                    niveau = 0.95, intervalle_lissage = c(1, 1e12))
LISSAGE_R17 <- list(methode = "wh_poisson", plage_ages = c(20, 90), ordre_difference = 3,
                    critere_lissage = "declare", niveau = 0.95, parametre_lissage = 1e7)
PLAN_R16 <- avec_lissage(plan_reference(validation = VALIDATION_LISSEE, graine = 22011202L), LISSAGE_R16)
PLAN_R17 <- avec_lissage(plan_reference(convention = "dernier_anniversaire", validation = VALIDATION_LISSEE,
                                        graine = 22011203L), LISSAGE_R17)
ENTREE_R16 <- jeu_reference()
ENTREE_R17 <- agregats_reference()

# --- grandeurs doubles d'une étape -----------------------------------------------

# Feuilles doubles de `x` (liste ou data.frame), nommées par leur chemin.
feuilles <- function(x, chemin) {
  if (is.data.frame(x) || is.list(x)) {
    r <- list()
    for (n in names(x)) r <- c(r, feuilles(x[[n]], paste(chemin, n, sep = "/")))
    return(r)
  }
  if (is.double(x)) stats::setNames(list(as.vector(x)), chemin) else list()
}
grandeurs <- function(objet) c(feuilles(objet$etapes$lissage, "lissage"), feuilles(objet$etapes$validation, "validation"))

# Écart relatif maximal par chemin (comparer_references()) et changements de
# signe ; NA si les longueurs diffèrent.
ecarts <- function(a, b) {
  t(vapply(names(a), function(k) {
    u <- a[[k]]; v <- b[[k]]
    if (length(u) != length(v)) return(c(NA_real_, NA_real_))
    d <- which(!(u == v) & is.finite(u) & is.finite(v))
    rel <- if (length(d)) max(abs(u[d] - v[d]) / pmax(abs(u[d]), abs(v[d]))) else 0
    c(rel, sum(sign(u[d]) != sign(v[d])))
  }, c(0, 0)))
}

# --- exécutions -------------------------------------------------------------------

executer <- function(entree, plan) {
  t0 <- proc.time()[["elapsed"]]
  x <- construire_table(entree$donnees, entree$roles, plan)
  list(objet = x, duree = proc.time()[["elapsed"]] - t0)
}
permuter <- function(entree, graine) {
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  entree$donnees <- entree$donnees[sample.int(nrow(entree$donnees)), , drop = FALSE]
  rownames(entree$donnees) <- NULL
  entree
}
u <- 2^-52
perturbe <- function(plan, nom, facteur, k = NULL) {
  v <- plan$etapes$lissage[[nom]]
  if (is.null(k)) v <- v * facteur else v[k] <- v[k] * facteur
  plan$etapes$lissage[[nom]] <- v
  plan
}

N16 <- executer(ENTREE_R16, PLAN_R16)
N17 <- executer(ENTREE_R17, PLAN_R17)
cat(sprintf("Nominal : R16 %.2f s, R17 %.2f s\n", N16$duree, N17$duree))
s16 <- N16$objet$etapes$lissage$resultat$segments
cat("R16, lambda choisi par segment :", paste(sprintf("%s = %.17g (%s)", s16$sexe, s16$parametre_lissage, s16$etat),
                                             collapse = " ; "), "\n")

cas <- list(
  list(nom = "(i) a x (1 + 2^-52), R16", ref = N16, entree = ENTREE_R16,
       plan = perturbe(PLAN_R16, "intervalle_lissage", 1 + u, 1L)),
  list(nom = "(i) a x (1 - 2^-52), R16", ref = N16, entree = ENTREE_R16,
       plan = perturbe(PLAN_R16, "intervalle_lissage", 1 - u, 1L)),
  list(nom = "(ii) b x (1 + 2^-52), R16", ref = N16, entree = ENTREE_R16,
       plan = perturbe(PLAN_R16, "intervalle_lissage", 1 + u, 2L)),
  list(nom = "(ii) b x (1 - 2^-52), R16", ref = N16, entree = ENTREE_R16,
       plan = perturbe(PLAN_R16, "intervalle_lissage", 1 - u, 2L)),
  list(nom = "(iii) lambda x (1 + 2^-52), R17", ref = N17, entree = ENTREE_R17,
       plan = perturbe(PLAN_R17, "parametre_lissage", 1 + u)),
  list(nom = "(iii) lambda x (1 - 2^-52), R17", ref = N17, entree = ENTREE_R17,
       plan = perturbe(PLAN_R17, "parametre_lissage", 1 - u)),
  list(nom = "(iv) lignes permutées, R16", ref = N16, entree = permuter(ENTREE_R16, 22028701L), plan = PLAN_R16),
  list(nom = "(iv) lignes permutées, R17", ref = N17, entree = permuter(ENTREE_R17, 22028702L), plan = PLAN_R17))

resultats <- NULL
for (k in cas) {
  p <- executer(k$entree, k$plan)
  e <- ecarts(grandeurs(k$ref$objet), grandeurs(p$objet))
  sp <- p$objet$etapes$lissage$resultat$segments
  cat(sprintf("%s : %.2f s ; lambda %s ; itérations %s\n", k$nom, p$duree,
              paste(sprintf("%.17g", sp$parametre_lissage), collapse = ", "),
              paste(sp$iterations, collapse = ", ")))
  resultats <- rbind(resultats, data.frame(cas = k$nom, chemin = rownames(e), ecart = e[, 1L], signes = e[, 2L],
                                           stringsAsFactors = FALSE))
}

# --- restitution ------------------------------------------------------------------

residu <- grepl("residu_conservation", resultats$chemin)
lissage <- startsWith(resultats$chemin, "lissage/")
par_chemin <- function(r) {
  a <- stats::aggregate(cbind(ecart, signes) ~ chemin, data = r, FUN = max, na.action = na.pass)
  a[order(-a$ecart), , drop = FALSE]
}
afficher <- function(titre, r) {
  cat("\n", titre, "\n", sep = "")
  a <- par_chemin(r)
  a <- a[a$ecart > 0 | a$signes > 0, , drop = FALSE]
  if (!nrow(a)) { cat("  (aucun écart)\n"); return(invisible(NULL)) }
  for (i in seq_len(nrow(a))) cat(sprintf("  %-70s %.3g%s\n", a$chemin[i], a$ecart[i],
                                          if (a$signes[i] > 0) sprintf(" (%d changement(s) de signe)", a$signes[i]) else ""))
}
afficher("Chemins lissage/* (hors résidus), écart relatif maximal sur les huit perturbations :",
         resultats[lissage & !residu, , drop = FALSE])
afficher("Résidus de conservation (rapportés à part) :", resultats[residu, , drop = FALSE])
afficher("Chemins validation/* (classe 1e-12 conservée, option (delta)) :", resultats[!lissage, , drop = FALSE])
cat("\nÉcart maximal par perturbation (lissage/*, hors résidus) :\n")
m <- tapply(resultats$ecart[lissage & !residu], resultats$cas[lissage & !residu], max, na.rm = TRUE)
for (n in names(m)) cat(sprintf("  %-34s %.3g\n", n, m[[n]]))
ecart_max <- max(resultats$ecart[lissage & !residu], na.rm = TRUE)
classe <- if (ecart_max > 0) 10^ceiling(log10(10 * ecart_max)) else 0
cat(sprintf("\nÉcart relatif maximal (lissage/*, hors résidus) : %.3g ; classe proposée (plus petite classe décimale >= 10 x l'écart) : %g\n",
            ecart_max, classe))
cat(sprintf("Écart relatif maximal (validation/*) : %.3g\n", max(resultats$ecart[!lissage], na.rm = TRUE)))
bilan(code_sortie = FALSE)
