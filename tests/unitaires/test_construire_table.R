# -----------------------------------------------------------------------------
#  tests/unitaires/test_construire_table.R : point d'entrée et exports
#  (ADR 0007, points 3.1 et 3.4) ; socle #8, orchestration à venir (#11).
#  Nature des attendus : valeurs exactes.
# -----------------------------------------------------------------------------

verifier("NAMESPACE : exports = catalogue_roles, construire_table, table_reference (ADR 0007, point 3.4 ; #9)",
         identical(sort(getNamespaceExports("tablexp")),
                   c("catalogue_roles", "construire_table", "table_reference")))
verifier("construire_table : signature (donnees, roles, plan, provenance = NULL)",
         identical(names(formals(construire_table)), c("donnees", "roles", "plan", "provenance")) &&
           is.null(formals(construire_table)$provenance))
verifier("construire_table : refus construction_indisponible tant qu'aucune étape n'existe",
         leve_refus(construire_table(data.frame(), c(a = "a"), list()),
                    "construction_indisponible"))
verifier("construire_table : refus porté par le module construire_table, détails listés", {
  r <- tryCatch(construire_table(data.frame(), character(0), list()),
                refus_tablexp = function(c) c)
  identical(r$module, "construire_table") &&
    identical(r$details$etapes_disponibles, character(0))
})
