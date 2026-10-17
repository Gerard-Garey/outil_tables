# -----------------------------------------------------------------------------
#  R/construire_table.R : point d'entrée unique, orchestration seule
#  (ADR 0004 ; ADR 0005, points 3 et 6 ; ADR 0007, points 3.1, 5 et 6 et
#  annotation du 17/10/2026 ; issues #11, #14 et #15)
#
#  - ETAPES, PARAMETRES_ETAPES : catalogue des étapes (ordre, fonction
#    d'étape, étapes préalables, aléa) et de leurs paramètres (nom, type,
#    domaine), recensé par la documentation ;
#  - valider_plan() : plan validé entièrement avant toute exécution (codes
#    structurels du plan ; valeurs contrôlées par les modules qui les
#    emploient, sans réimplémentation : controler_parametres_donnees(),
#    controler_parametres_exposition(), controler_parametres_brut()) ;
#    exporté (contrôle de saisie à sens métier, E4.3) ;
#  - construire_table() : valide le plan, les rôles et la provenance, calcule
#    l'empreinte des données, exécute les étapes dans l'ordre du plan en
#    transmettant les champs du plan en arguments nommés (une fonction
#    d'étape ne reçoit jamais le plan entier), assemble l'objet
#    `construction` et la fiche (module `rapport`).
#
#  Aucun calcul. Peut appeler tous les modules sauf `simulation` (ADR 0007,
#  point 2.1).
# -----------------------------------------------------------------------------

# Champs de premier niveau du plan (ADR 0007, point 5.1 ; M48, M49). `graine`
# est requise dès qu'une étape du plan tire un aléa, interdite sinon.
CHAMPS_PLAN <- c("transition", "periode", "delai_declaration", "segmentation", "graine", "etapes")
CHAMPS_PLAN_REQUIS <- c("transition", "periode", "delai_declaration", "segmentation", "etapes")

# Étapes de la branche, dans l'ordre des modules : fonction d'étape (module),
# étapes préalables dont les résultats sont employés (colonne liste, vecteur
# vide sans préalable ; #15, spécification d'actuary, A.9 : `brut` emploie
# les observations de `donnees` pour l'estimateur de Kaplan-Meier), tirage
# d'un aléa.
ETAPES <- data.frame(
  etape = c("donnees", "exposition", "brut"),
  module = c("donnees", "exposition", "brut"),
  fonction = c("preparer_donnees", "calculer_exposition", "calculer_taux_bruts"),
  prealable = I(list(character(0), "donnees", c("donnees", "exposition"))),
  aleatoire = c(FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE
)

# Catalogue des paramètres par étape (ADR 0007, point 5.2 et annotation du
# 17/10/2026) : tous requis, aucun défaut. Les valeurs sont contrôlées par le
# module de l'étape.
PARAMETRES_ETAPES <- list(
  donnees = data.frame(
    parametre = "exclusions",
    type = "character",
    domaine = "codes de motif de ligne ou de tête, sans doublon ; vide explicite admis",
    stringsAsFactors = FALSE),
  exposition = data.frame(
    parametre = c("convention", "par_annee"),
    type = c("character", "logical"),
    domaine = c("plus_fine, dernier_anniversaire ou age_atteint_annee", "TRUE ou FALSE"),
    stringsAsFactors = FALSE),
  brut = data.frame(
    parametre = c("estimateurs", "niveau"),
    type = c("character", "double"),
    domaine = c(paste("sous-ensemble non vide, sans doublon ni NA, de poisson, binomial et kaplan_meier ;",
                      "binomial et kaplan_meier refusés sous par_annee = TRUE ; résultats dans l'ordre",
                      "canonique"),
                "nombre unique, fini, 0 < niveau < 1"),
    stringsAsFactors = FALSE)
)

# Liste nommée aux noms uniques, non vides, non manquants (ni data.frame).
liste_nommee <- function(x) is.list(x) && !is.data.frame(x) && is.null(dim(x)) &&
  (!length(x) || (!is.null(names(x)) && !anyNA(names(x)) && all(nzchar(names(x))) &&
                    !anyDuplicated(names(x))))

# Valide le plan entier, sans rien exécuter (ADR 0007, point 5.4). Rend le
# plan, inchangé et invisible ; refuse (module construire_table pour les
# codes structurels ; module donnees, exposition ou brut pour les valeurs).
valider_plan <- function(plan) {
  refus_plan <- function(motif, message, details = list())
    refuser("construire_table", motif, message, details)
  if (!liste_nommee(plan) || !length(plan))
    refus_plan("plan_mal_forme",
               "Le plan doit être une liste nommée non vide, aux noms uniques (ADR 0007, point 5.1).")
  inconnus <- setdiff(names(plan), CHAMPS_PLAN)
  if (length(inconnus))
    refus_plan("champ_inconnu",
               sprintf("Champ(s) inconnu(s) du plan : %s (champs admis : %s).",
                       liste_guillemets(inconnus), liste_guillemets(CHAMPS_PLAN)),
               list(champ = inconnus))
  manquants <- setdiff(CHAMPS_PLAN_REQUIS, names(plan))
  if (length(manquants))
    refus_plan("champ_manquant",
               sprintf("Champ(s) requis absent(s) du plan : %s (aucun défaut implicite).",
                       liste_guillemets(manquants)),
               list(champ = manquants))
  etapes <- plan[["etapes"]]
  if (!is.list(etapes) || is.data.frame(etapes) ||
      (length(etapes) && (is.null(names(etapes)) || anyNA(names(etapes)) || !all(nzchar(names(etapes))))))
    refus_plan("etapes_mal_formees",
               "Le champ « etapes » doit être une liste nommée (nom d'étape -> liste de ses paramètres).")
  if (!length(etapes))
    refus_plan("aucune_etape", "Le plan ne déclare aucune étape.")
  noms <- names(etapes)
  doubles <- unique(noms[duplicated(noms)])
  if (length(doubles))
    refus_plan("etape_en_double",
               sprintf("Étape(s) déclarée(s) plusieurs fois : %s.", liste_guillemets(doubles)),
               list(etape = doubles))
  inconnues <- setdiff(noms, ETAPES$etape)
  if (length(inconnues))
    refus_plan("etape_inconnue",
               sprintf("Étape(s) inconnue(s) : %s (étapes disponibles : %s).",
                       liste_guillemets(inconnues), liste_guillemets(ETAPES$etape)),
               list(etape = inconnues))
  rang <- match(noms, ETAPES$etape)
  if (is.unsorted(rang, strictly = TRUE))
    refus_plan("etapes_hors_ordre",
               sprintf("Étapes hors de l'ordre des modules : %s reçu, ordre attendu %s.",
                       liste_guillemets(noms), liste_guillemets(ETAPES$etape[sort(rang)])),
               list(etape = noms))
  # Chaque préalable de chaque étape est contrôlé ; seuls les préalables
  # absents sont cités.
  absents <- lapply(ETAPES$prealable[rang], function(p) setdiff(p, noms))
  sans_prealable <- lengths(absents) > 0L
  if (any(sans_prealable)) {
    manquants <- unique(unlist(absents))
    refus_plan("etape_prealable_manquante",
               sprintf("Étape(s) %s sans %s %s.", liste_guillemets(noms[sans_prealable]),
                       if (length(manquants) == 1L) "son étape préalable" else "ses étapes préalables",
                       liste_guillemets(manquants)),
               list(etape = noms[sans_prealable], prealable = manquants))
  }
  aleatoire <- any(ETAPES$aleatoire[rang])
  if (!aleatoire && "graine" %in% names(plan))
    refus_plan("graine_inattendue",
               "Le plan déclare une graine alors qu'aucune de ses étapes ne tire d'aléa.",
               list(graine = plan[["graine"]]))
  if (aleatoire && !("graine" %in% names(plan)))
    refus_plan("graine_manquante", "Une étape du plan tire un aléa : la graine est requise.")
  for (nom in noms) {
    pe <- etapes[[nom]]
    attendus <- PARAMETRES_ETAPES[[nom]]$parametre
    if (!liste_nommee(pe))
      refus_plan("parametres_mal_formes",
                 sprintf("Étape « %s » : ses paramètres doivent être une liste nommée.", nom),
                 list(etape = nom))
    inconnus <- setdiff(names(pe), attendus)
    if (length(inconnus))
      refus_plan("parametre_inconnu",
                 sprintf("Étape « %s » : paramètre(s) inconnu(s) %s (paramètres admis : %s).",
                         nom, liste_guillemets(inconnus), liste_guillemets(attendus)),
                 list(etape = nom, parametre = inconnus))
    manquants <- setdiff(attendus, names(pe))
    if (length(manquants))
      refus_plan("parametre_manquant",
                 sprintf("Étape « %s » : paramètre(s) requis absent(s) %s (aucun défaut implicite).",
                         nom, liste_guillemets(manquants)),
                 list(etape = nom, parametre = manquants))
  }
  # Valeurs : contrôlées par les modules (codes identiques par construction,
  # ADR 0007, annotation du 17/10/2026, point d).
  controler_parametres_donnees(plan[["transition"]], plan[["periode"]], plan[["delai_declaration"]],
                               plan[["segmentation"]], etapes[["donnees"]][["exclusions"]])
  if ("exposition" %in% noms)
    controler_parametres_exposition(etapes[["exposition"]][["convention"]],
                                    etapes[["exposition"]][["par_annee"]])
  # `brut` a pour préalables `donnees` et `exposition` (contrôlés ci-dessus) :
  # par_annee est déjà validé.
  if ("brut" %in% noms)
    controler_parametres_brut(etapes[["brut"]][["estimateurs"]], etapes[["brut"]][["niveau"]],
                              etapes[["exposition"]][["par_annee"]])
  invisible(plan)
}

# Point d'entrée unique (ADR 0007, point 3.1).
#   donnees    : data.frame (le moteur ne lit pas de fichier) ;
#   roles      : vecteur de chaînes nommé rôle -> colonne ;
#   plan       : liste nommée (valider_plan()) ;
#   provenance : NULL (données construites en mémoire) ou list(fichier,
#                empreinte, algorithme) transmise par la couche de lecture.
# Rend un objet de classe `construction` : table, etapes, avertissements,
# fiche, execution (seul champ qui change entre deux exécutions identiques).
construire_table <- function(donnees, roles, plan, provenance = NULL) {
  debut <- Sys.time()
  # Validation entière avant toute exécution : plan, rôles, recette et
  # convention, provenance.
  valider_plan(plan)
  controler_roles(donnees, roles, plan[["segmentation"]])
  recette <- deduire_recette(roles)
  etapes_plan <- plan[["etapes"]]
  if ("exposition" %in% names(etapes_plan))
    resoudre_convention(etapes_plan[["exposition"]][["convention"]], recette$convention_native)
  controler_provenance(provenance)
  empreintes <- empreintes_construction(donnees, roles, provenance)

  etapes <- list()
  avertissements <- avertissements_vides()
  for (nom in names(etapes_plan)) {
    pe <- etapes_plan[[nom]]
    r <- switch(nom,
      donnees = preparer_donnees(donnees, roles,
                                 transition = plan[["transition"]], periode = plan[["periode"]],
                                 delai_declaration = plan[["delai_declaration"]],
                                 segmentation = plan[["segmentation"]],
                                 exclusions = pe[["exclusions"]]),
      exposition = calculer_exposition(etapes[["donnees"]][["resultat"]][["observations"]], roles,
                                       transition = plan[["transition"]], periode = plan[["periode"]],
                                       delai_declaration = plan[["delai_declaration"]],
                                       segmentation = plan[["segmentation"]],
                                       convention = pe[["convention"]], par_annee = pe[["par_annee"]]),
      brut = calculer_taux_bruts(etapes[["exposition"]][["resultat"]][["exposition"]],
                                 segmentation = plan[["segmentation"]],
                                 par_annee = etapes[["exposition"]][["parametres"]][["par_annee"]],
                                 convention = etapes[["exposition"]][["parametres"]][["convention"]][["retenue"]],
                                 unite_exposition = etapes[["exposition"]][["parametres"]][["unite_exposition"]],
                                 estimateurs = pe[["estimateurs"]], niveau = pe[["niveau"]],
                                 observations = etapes[["donnees"]][["resultat"]][["observations"]],
                                 transition = plan[["transition"]], periode = plan[["periode"]],
                                 delai_declaration = plan[["delai_declaration"]],
                                 parametres_exposition = etapes[["exposition"]][["parametres"]]),
      stop(sprintf("construire_table() : étape « %s » sans fonction d'étape (erreur de programmation).", nom),
           call. = FALSE))
    etapes[[nom]] <- r[c("parametres", "resultat", "diagnostics")]
    avertissements <- rbind(avertissements, r$avertissements)
  }
  rownames(avertissements) <- NULL

  structure(
    list(table = NULL, etapes = etapes, avertissements = avertissements,
         fiche = assembler_fiche(roles, plan, recette, etapes, avertissements, empreintes, provenance),
         execution = informations_execution(debut)),
    class = "construction")
}
