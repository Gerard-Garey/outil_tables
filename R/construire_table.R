# -----------------------------------------------------------------------------
#  R/construire_table.R : point d'entrée unique, orchestration seule
#  (ADR 0004 ; ADR 0007, point 3.1)
#
#  Contiendra : construire_table(), le validateur du plan valider_plan()
#  (#11) et la correspondance étape -> fonction d'étape. Aucun calcul.
#  Peut appeler tous les modules sauf `simulation` (ADR 0007, point 2.1).
#
#  État (#8, socle) : aucune étape n'existe encore. Les étapes `donnees`
#  (#9) et `exposition` (#11) inscriront leur fonction dans FONCTIONS_ETAPES ;
#  d'ici là, toute construction est refusée par une condition typée
#  (refus_tablexp), jamais par un objet partiel.
# -----------------------------------------------------------------------------

# Correspondance nom d'étape -> fonction d'étape, dans l'ordre des modules.
# Vide dans le socle.
FONCTIONS_ETAPES <- list()

construire_table <- function(donnees, roles, plan, provenance = NULL) {
  # Orchestration (validation des rôles et du plan, exécution des étapes de
  # FONCTIONS_ETAPES, assemblage de l'objet `construction`) : #11.
  refuser("construire_table", "construction_indisponible",
          paste("Aucune étape de construction n'est encore disponible dans",
                "tablexp (étapes `donnees` et `exposition` : issues #9 et",
                "#11) : la construction est refusée."),
          list(etapes_disponibles = as.character(names(FONCTIONS_ETAPES))))
}
