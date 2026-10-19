# -----------------------------------------------------------------------------
#  tests/unitaires/test_outils.R : R/outils.R (ADR 0007, points 6.4 et 8)
#  Nature des attendus : valeurs exactes (classes, champs, identical()).
# -----------------------------------------------------------------------------

refus_tablexp <- tablexp:::refus_tablexp
refuser <- tablexp:::refuser
sous_graine <- tablexp:::sous_graine
GENERATEUR <- tablexp:::GENERATEUR

# --- refus typé ---------------------------------------------------------------

r <- refus_tablexp("donnees", "role_absent", "Le rôle « date_naissance » est absent.",
                   list(role = "date_naissance"))
verifier("refus_tablexp : classe c(refus_tablexp, error, condition)",
         identical(class(r), c("refus_tablexp", "error", "condition")))
verifier("refus_tablexp : champs module, motif, message, details",
         identical(r$module, "donnees") && identical(r$motif, "role_absent") &&
           identical(conditionMessage(r), "Le rôle « date_naissance » est absent.") &&
           identical(r$details, list(role = "date_naissance")))
attrape <- tryCatch(refuser("donnees", "role_absent", "absent", list(n = 3L)),
                    refus_tablexp = function(c) c)
verifier("refuser() lève une condition attrapable par refus_tablexp =",
         inherits(attrape, "refus_tablexp") && identical(attrape$details$n, 3L))
verifier("refuser() est aussi une erreur ordinaire (tryCatch error =)",
         inherits(tryCatch(refuser("m", "motif", "msg"), error = function(e) e), "error"))
verifier("leve_refus() reconnaît le motif",
         leve_refus(refuser("m", "motif_a", "msg"), "motif_a"))
verifier("leve_refus() distingue un autre motif",
         is.character(leve_refus(refuser("m", "motif_a", "msg"), "motif_b")))
verifier("leve_refus() distingue une erreur qui n'est pas un refus",
         is.character(leve_refus(stop("autre"), "motif_a")))
verifier("refus_tablexp : motif mal formé -> erreur de programmation, pas un refus", {
  e <- tryCatch(refus_tablexp("m", "Motif Libre", "msg"), error = function(e) e)
  inherits(e, "error") && !inherits(e, "refus_tablexp")
})
verifier("refus_tablexp : details non liste -> erreur",
         inherits(tryCatch(refus_tablexp("m", "motif", "msg", "x"), error = function(e) e), "error"))

# --- avertissements métier (ADR 0007, point 6.5) ------------------------------

avertir <- tablexp:::avertir
avertissements_vides <- tablexp:::avertissements_vides
a0 <- avertissements_vides()
verifier("avertissements_vides() : data.frame (etape, code, message, nombre) sans ligne",
         identical(names(a0), c("etape", "code", "message", "nombre")) && nrow(a0) == 0L)
verifier("avertissements_vides() : nombre en integer (#24)", identical(typeof(a0$nombre), "integer"))
a <- avertir(avertir(a0, "donnees", "unite_contrat", "msg 1", 3L), "donnees", "exclusion", "msg 2", 2)
verifier("avertir() : lignes ajoutées dans l'ordre, nombre en integer (double entier converti, #24)",
         identical(a, data.frame(etape = c("donnees", "donnees"), code = c("unite_contrat", "exclusion"),
                                 message = c("msg 1", "msg 2"), nombre = c(3L, 2L), stringsAsFactors = FALSE)))
# Nombre mal formé : erreur de programmation (stop()), pas un refus typé (#24).
erreur_programmation <- function(expr) {
  e <- tryCatch({ expr; NULL }, error = function(e) e)
  inherits(e, "error") && !inherits(e, "refus_tablexp") &&
    grepl("'nombre'", conditionMessage(e), fixed = TRUE)
}
for (cas in list(list("non entier", 2.5), list("NA entier", NA_integer_), list("NA double", NA_real_),
                 list("Inf", Inf), list("-Inf", -Inf), list("NaN", NaN), list("longueur 0", integer(0)),
                 list("longueur 2", c(1L, 2L)), list("caractère", "3"), list("logique", TRUE),
                 list("hors étendue des entiers", 2^31), list("négatif (double)", -1),
                 list("négatif (integer)", -1L)))
  verifier(sprintf("avertir() : nombre %s -> erreur de programmation (#24)", cas[[1L]]),
           erreur_programmation(avertir(a0, "donnees", "code", "msg", cas[[2L]])))
verifier("avertir() : nombre 0 et .Machine$integer.max admis",
         identical(avertir(a0, "e", "c", "m", 0)$nombre, 0L) &&
           identical(avertir(a0, "e", "c", "m", .Machine$integer.max)$nombre, .Machine$integer.max))
verifier("avertir() : aucun warning() émis",
         is.null(tryCatch({ avertir(a0, "donnees", "code", "msg", 1); NULL }, warning = function(w) w)))
verifier("avertir() : code mal formé -> erreur de programmation",
         inherits(tryCatch(avertir(a0, "donnees", "Code Libre", "msg", 1), error = function(e) e), "error"))

# --- générateur et graine locale ---------------------------------------------

verifier("GENERATEUR : Mersenne-Twister / Inversion / Rejection (valeurs de R 4.3.3)",
         identical(GENERATEUR, c(kind = "Mersenne-Twister", normal.kind = "Inversion",
                                 sample.kind = "Rejection")))

# État de l'appelant : générateur et .Random.seed sauvegardés pour l'essai.
etat_initial <- if (exists(".Random.seed", globalenv())) globalenv()$.Random.seed else NULL
reglage_initial <- RNGkind()

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
set.seed(42L); attendu <- runif(5)
verifier("sous_graine(42) : mêmes tirages que set.seed(42) sous le générateur par défaut",
         identical(sous_graine(42L, runif(5)), attendu))
verifier("sous_graine : graine double entière admise (42 et 42L)",
         identical(sous_graine(42, runif(5)), attendu))

set.seed(7L, kind = "L'Ecuyer-CMRG")
avant <- globalenv()$.Random.seed
x <- sous_graine(42L, runif(5))
verifier("appelant sous L'Ecuyer-CMRG : tirages de GENERATEUR", identical(x, attendu))
verifier("appelant sous L'Ecuyer-CMRG : RNGkind() restauré",
         identical(RNGkind()[1L], "L'Ecuyer-CMRG"))
verifier("appelant sous L'Ecuyer-CMRG : .Random.seed restauré",
         identical(globalenv()$.Random.seed, avant))
suite_avec <- runif(3)
set.seed(7L, kind = "L'Ecuyer-CMRG"); suite_sans <- runif(3)
verifier("appelant : tirage suivant identique à celui qu'il aurait eu sans l'appel",
         identical(suite_avec, suite_sans))

RNGkind("Mersenne-Twister", "Inversion", "Rejection")
if (exists(".Random.seed", globalenv())) rm(".Random.seed", envir = globalenv())
invisible(sous_graine(1L, runif(1)))
verifier("appelant sans .Random.seed : absence restaurée",
         !exists(".Random.seed", globalenv(), inherits = FALSE))

# Appelant sous L'Ecuyer-CMRG sans .Random.seed : seul RNGkind() porte son
# réglage ; il doit lui être rendu.
RNGkind("L'Ecuyer-CMRG")
rm(".Random.seed", envir = globalenv())
invisible(sous_graine(1L, runif(1)))
verifier("appelant sans .Random.seed, sous L'Ecuyer-CMRG : RNGkind() restauré",
         identical(RNGkind()[1L], "L'Ecuyer-CMRG") &&
           !exists(".Random.seed", globalenv(), inherits = FALSE))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

set.seed(3L); avant <- globalenv()$.Random.seed
e <- tryCatch(sous_graine(1L, { runif(2); stop("erreur dans expr") }), error = function(e) e)
verifier("erreur dans expr : l'erreur est propagée", inherits(e, "error"))
verifier("erreur dans expr : .Random.seed restauré",
         identical(globalenv()$.Random.seed, avant))

# Appelant sous sample.kind = "Rounding" : la restauration ne doit émettre
# aucun avertissement (R en émet un à chaque RNGkind(sample.kind =
# "Rounding"), mesuré avant la correction).
suppressWarnings(RNGkind(sample.kind = "Rounding"))
set.seed(3L); avant <- globalenv()$.Random.seed; reglage_avant <- RNGkind()
avertissements <- character(0)
x <- withCallingHandlers(sous_graine(1L, runif(1)), warning = function(w) {
  avertissements <<- c(avertissements, conditionMessage(w)); invokeRestart("muffleWarning")
})
verifier("appelant sous sample.kind = \"Rounding\" : aucun avertissement émis",
         if (length(avertissements)) paste("avertissements :", paste(avertissements, collapse = " ; ")) else TRUE)
verifier("appelant sous sample.kind = \"Rounding\" : RNGkind() et .Random.seed restaurés",
         identical(RNGkind(), reglage_avant) && identical(RNGkind()[3L], "Rounding") &&
           identical(globalenv()$.Random.seed, avant))
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

for (g in list(NA_integer_, 1.5, "1", c(1L, 2L), Inf, 2^31))
  verifier(sprintf("sous_graine(%s) : refus graine_invalide", deparse(g)),
           leve_refus(sous_graine(g, runif(1)), "graine_invalide"))
verifier("sous_graine() sans graine : refus graine_invalide (aucune graine par défaut)",
         leve_refus(sous_graine(expr = runif(1)), "graine_invalide"))

# Restauration de l'état de l'appelant de l'essai.
RNGkind(reglage_initial[1L], reglage_initial[2L], reglage_initial[3L])
if (is.null(etat_initial)) {
  if (exists(".Random.seed", globalenv())) rm(".Random.seed", envir = globalenv())
} else assign(".Random.seed", etat_initial, envir = globalenv())

# --- forme exacte (#23) -------------------------------------------------------

forme_exacte <- tablexp:::forme_exacte
cas_forme <- list(
  list("double nu", 0.95, "double", character(0), TRUE),
  list("double nommé", c(a = 0.95), "double", character(0), FALSE),
  list("double en matrix", matrix(0.95), "double", character(0), FALSE),
  list("double à attribut quelconque", structure(0.95, foo = "x"), "double", character(0), FALSE),
  list("integer sous types double seul", 1L, "double", character(0), FALSE),
  list("integer sous types integer ou double", 1L, c("integer", "double"), character(0), TRUE),
  list("facteur sous type integer (levels, class)", factor("a"), "integer", character(0), FALSE),
  list("chaîne vide character(0)", character(0), "character", character(0), TRUE),
  list("character(0) aux noms character(0)", { x <- character(0); names(x) <- character(0); x },
       "character", character(0), FALSE),
  list("NULL sous type character", NULL, "character", character(0), FALSE),
  list("liste nommée, names admis", list(a = 1), "list", "names", TRUE),
  list("liste nommée, aucun attribut admis", list(a = 1), "list", character(0), FALSE),
  list("liste sans noms, names admis (admis non requis)", list(1), "list", "names", TRUE),
  list("data.frame, names admis", data.frame(a = 1), "list", "names", FALSE),
  list("liste à classe, names admis", structure(list(a = 1), class = "x"), "list", "names", FALSE),
  list("Date, class admis", as.Date("2015-01-01"), "double", "class", TRUE),
  list("Date nommée, class admis", structure(16436, class = "Date", names = "a"), "double", "class", FALSE))
for (k in cas_forme)
  verifier(sprintf("forme_exacte : %s -> %s (valeur calculée à la main)", k[[1]], k[[5]]),
           identical(forme_exacte(k[[2]], k[[3]], k[[4]]), k[[5]]))
