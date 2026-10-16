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
