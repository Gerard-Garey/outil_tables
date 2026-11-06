# -----------------------------------------------------------------------------
#  tests/unitaires/test_graine_plan.R : graine du plan, refus graine_invalide
#  et domaine de canoniser_plan() (issue #61 ; spécification d'actuary, #61,
#  commentaire 6681823944 ; M243, M247)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : motifs, module et details des refus, ordre
#    des contrôles, graine canonique rendue ;
#  - identité exacte : concordance forme_graine() <=> canoniser_plan(),
#    idempotence, tirages identical() de sous_graine() pour 7 et 7L.
#  Les trois refus de la graine sont testés par injection de `aleatoire`
#  dans controler_graine_plan() (M247) ; valider_plan() garde sa signature
#  (plan). Les plans de ce fichier n'ont pas d'étape aléatoire ; le bout en
#  bout avec l'étape aléatoire `validation` (commit (3) de B13, #4) est dans
#  tests/unitaires/test_validation.R.
# -----------------------------------------------------------------------------

D <- as.Date
controler_graine_plan <- tablexp:::controler_graine_plan
forme_graine <- tablexp:::forme_graine
canoniser <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                                  tablexp:::ordres_canoniques())
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)

PLAN <- list(
  transition = list(depart = "vivant", arrivee = "deces"),
  periode = list(debut = D("2015-01-01"), fin = D("2024-12-31")), delai_declaration = 0, segmentation = "sexe",
  etapes = list(donnees = list(exclusions = character(0)),
                exposition = list(convention = "plus_fine", par_annee = FALSE)))
# Plan muni d'une graine (NULL compris : champ présent de valeur NULL).
avec_graine <- function(g, p = PLAN) { p["graine"] <- list(g); p }

# Grille de la spécification, § 1 (valeurs mesurées), plus 1:2 (admis par la
# canonisation avant #61, mesuré) et integer(0).
INVALIDES <- list("2^31" = 2^31, "-2^31" = -2^31, "1.5" = 1.5, "NA_integer_" = NA_integer_,
                  "NA_real_" = NA_real_, "NaN" = NaN, "Inf" = Inf, "-Inf" = -Inf, "c(1, 2)" = c(1, 2),
                  "1:2" = 1:2, "integer(0)" = integer(0), "\"7\"" = "7", "TRUE" = TRUE, "NULL" = NULL,
                  "c(a = 7L)" = c(a = 7L), "structure(7, class = \"x\")" = structure(7, class = "x"),
                  "7+0i" = 7 + 0i)
# Admises, avec leur graine canonique attendue.
ADMISES <- list(list("1L", 1L, 1L), list("7", 7, 7L), list("-0", -0, 0L),
                list("2147483647", 2147483647, 2147483647L), list("-2147483647", -2147483647, -2147483647L),
                list("2147483647L", 2147483647L, 2147483647L), list("-2147483647L", -2147483647L, -2147483647L))

# Refus attendu : module construire_table, motif, et (si demandé) details$graine
# identical() à la graine reçue (élément présent, NULL compris).
controle <- function(r, motif, graine) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, "construire_table")) return(sprintf("module « %s »", r$module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!missing(graine) && !("graine" %in% names(r$details) && identical(r$details[["graine"]], graine)))
    return(paste("details$graine :", paste(deparse(r$details[["graine"]]), collapse = " ")))
  TRUE
}

# --- domaine : forme_graine() ---------------------------------------------------

verifier("forme_graine : valeurs de la grille refusées (FALSE, jamais NA), admises acceptées (valeur calculée à la main)",
         all(vapply(INVALIDES, function(g) identical(forme_graine(g), FALSE), logical(1))) &&
           all(vapply(ADMISES, function(a) identical(forme_graine(a[[2L]]), TRUE), logical(1))))

# --- controler_graine_plan() par injection de `aleatoire` (M247) -----------------

for (n in names(INVALIDES)) {
  g <- INVALIDES[[n]]
  verifier(sprintf("injection : graine %s -> graine_invalide, details$graine reçue, avec ou sans étape aléatoire", n),
           isTRUE(controle(refus_de(controler_graine_plan(avec_graine(g), FALSE)), "graine_invalide", g)) &&
             isTRUE(controle(refus_de(controler_graine_plan(avec_graine(g), TRUE)), "graine_invalide", g)))
}
for (a in ADMISES)
  verifier(sprintf("injection : graine %s admise -> graine_inattendue sans étape aléatoire, rien avec", a[[1L]]), {
    r <- controle(refus_de(controler_graine_plan(avec_graine(a[[2L]]), FALSE)), "graine_inattendue", a[[2L]])
    v <- withVisible(controler_graine_plan(avec_graine(a[[2L]]), TRUE))
    if (!isTRUE(r)) r else identical(v, list(value = NULL, visible = FALSE))
  })
verifier("injection : sans graine, étape aléatoire -> graine_manquante ; sans graine ni étape aléatoire -> rien", {
  r <- controle(refus_de(controler_graine_plan(PLAN, TRUE)), "graine_manquante")
  if (!isTRUE(r)) r else is.null(controler_graine_plan(PLAN, FALSE))
})

# --- valider_plan() : même grille, ordre amont et aval -----------------------------

for (n in names(INVALIDES)) {
  g <- INVALIDES[[n]]
  verifier(sprintf("valider_plan : graine %s -> graine_invalide (et non graine_inattendue)", n),
           controle(refus_de(valider_plan(avec_graine(g))), "graine_invalide", g))
}
for (a in ADMISES)
  verifier(sprintf("valider_plan : graine %s admise -> graine_inattendue (aucune étape aléatoire)", a[[1L]]),
           controle(refus_de(valider_plan(avec_graine(a[[2L]]))), "graine_inattendue", a[[2L]]))
verifier("valider_plan : signature (plan) inchangée ; seule l'étape validation est aléatoire au catalogue (#4)",
         identical(names(formals(valider_plan)), "plan") &&
           identical(tablexp:::ETAPES$etape[tablexp:::ETAPES$aleatoire], "validation"))
verifier("ordre amont : etape_prealable_manquante avant graine_invalide", {
  p <- avec_graine(1.5); p$etapes <- p$etapes["exposition"]
  controle(refus_de(valider_plan(p)), "etape_prealable_manquante")
})
verifier("ordre aval : graine_invalide avant parametres_mal_formes ; graine_inattendue aussi", {
  p <- PLAN; p$etapes$exposition <- list(1)
  r1 <- controle(refus_de(valider_plan(avec_graine(NA_integer_, p))), "graine_invalide", NA_integer_)
  r2 <- controle(refus_de(valider_plan(avec_graine(7L, p))), "graine_inattendue", 7L)
  if (!isTRUE(r1)) r1 else r2
})
verifier("ordre aval : graine_invalide avant les contrôles de valeurs du module donnees", {
  p <- avec_graine("7"); p$delai_declaration <- -1
  controle(refus_de(valider_plan(p)), "graine_invalide", "7")
})

# --- canoniser_plan() : même domaine que forme_graine() (C2) -------------------------

for (n in names(INVALIDES))
  verifier(sprintf("canoniser_plan : graine %s -> erreur de programmation (non canonisable)", n), {
    r <- tryCatch({ canoniser(avec_graine(INVALIDES[[n]])); NULL }, error = function(e) e)
    !is.null(r) && !inherits(r, "refus_tablexp") &&
      startsWith(conditionMessage(r), "canoniser_plan() :") && grepl("graine", conditionMessage(r), fixed = TRUE)
  })
for (a in ADMISES)
  verifier(sprintf("canoniser_plan : graine %s -> %s integer, idempotente (identical)", a[[1L]], deparse(a[[3L]])), {
    c1 <- canoniser(avec_graine(a[[2L]]))
    identical(c1$graine, a[[3L]]) && identical(canoniser(c1), c1)
  })
verifier("canoniser_plan : plan sans graine rendu sans graine",
         !("graine" %in% names(canoniser(PLAN))))

# --- sous_graine() : 7 et 7L ---------------------------------------------------

verifier("sous_graine : tirages identical() pour la graine 7 et 7L (identité exacte)",
         identical(tablexp:::sous_graine(7, stats::runif(5L)), tablexp:::sous_graine(7L, stats::runif(5L))))
