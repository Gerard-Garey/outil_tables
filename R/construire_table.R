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
#    controler_parametres_exposition(), controler_parametres_brut(),
#    controler_parametres_positionnement(), controler_parametres_homogeneite(),
#    controler_parametres_validation(),
#    controler_parametres_importance_exclusions()) ;
#    exporté (contrôle de saisie à sens métier, E4.3) ;
#  - PARAMETRES_CLE_TABLE, controler_forme_tables(), cles_citees(),
#    controler_tables_reference(), controler_tables_rejeu() : tables de
#    référence reçues par l'argument tables_reference, contrôles 1.3 (entrée
#    de construire_table()) et 7 bis (rejeu) de l'ADR 0015 (#73) ;
#    table_citee() : table de la clé citée par une étape (`positionnement`,
#    #167 ; `validation`, #4) ;
#  - controler_plan_roles(), controler_entrees() : contrôle des entrées en
#    deux couches (#82) ; la première (plan, rôles, recette, convention,
#    matrice recette x étape x estimateur, provenance) est partagée par
#    construire_table() et rejouer_fiche() ; la
#    seconde y ajoute le contrôle 1.3 et les contrôles d'entrée des étapes
#    qui lisent une table (controler_entrees_validation(), module
#    positionnement, pour `positionnement` puis `validation`), et n'est
#    appelée que par construire_table() ; toutes deux
#    rendent les valeurs validées transmises aux étapes ;
#  - construire_table() : contrôle les entrées une fois (controler_entrees()),
#    calcule l'empreinte des données, exécute les étapes dans l'ordre du plan
#    en transmettant les champs du plan et les valeurs validées (p, recette,
#    convention) en arguments nommés (une fonction d'étape ne reçoit jamais
#    le plan entier et ne contrôle rien), assemble l'objet `construction` et
#    la fiche (module `rapport`) ;
#  - rejouer_fiche() : rejeu outillé d'une fiche (E3.7 ; ADR 0012, point 3 ;
#    #86) : contrôles de la fiche et des données, puis un seul appel de
#    construire_table() sur les rôles, le plan et la provenance de la fiche,
#    et les tables de référence reçues ou relues ; le contrôle 1.3 et les
#    contrôles d'entrée des étapes qui lisent une table (positionnement,
#    validation) sont faits une fois, par construire_table() (#82) ;
#    bilan des contrôles et des témoins, hors de l'objet ; exporté.
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
# vide sans préalable ; #15, spécification d'actuary, A.9, et #38 : `brut`
# emploie les fenêtres transmises par `exposition`, et non plus les
# observations de `donnees`, pour l'estimateur de Kaplan-Meier ; `donnees`
# reste dans les préalables de `brut`, préalable transitif par `exposition` :
# les détails du refus etape_prealable_manquante sont figés par les témoins
# de tests/unitaires/test_brut_kaplan_meier.R, #27), tirage d'un aléa.
# `validation` (#4 ; M242) : préalables `donnees` et `exposition` (`brut`
# n'en est pas un : mu^ s'obtient par taux_poisson()), aléatoire (Monte-Carlo,
# graine requise ; ADR 0015, point 7.2). `importance_exclusions` (#69 ;
# M242) : module `validation`, préalables `donnees` et `exposition`, non
# aléatoire ; après `validation`, dont elle lit le résultat (comparateur 2)
# si et seulement si `validation` est au plan. `positionnement` (#167 ; F4) :
# module `positionnement`, après `brut`, préalables `donnees` et
# `exposition`, non aléatoire. `homogeneite` (#168 ; F10) : module
# `validation`, après `positionnement` et avant `validation` (premier rang que
# permet l'ordre des modules), préalables `donnees` et `exposition`,
# aléatoire (Monte-Carlo conditionnel ; chaque étape aléatoire tire sous la
# graine du plan, flux propre, F11).
ETAPES <- data.frame(
  etape = c("donnees", "exposition", "brut", "positionnement", "homogeneite", "validation", "importance_exclusions"),
  module = c("donnees", "exposition", "brut", "positionnement", "validation", "validation", "validation"),
  fonction = c("preparer_donnees", "calculer_exposition", "calculer_taux_bruts", "calculer_positionnement",
               "calculer_homogeneite", "calculer_validation", "calculer_importance_exclusions"),
  prealable = I(list(character(0), "donnees", c("donnees", "exposition"), c("donnees", "exposition"),
                     c("donnees", "exposition"), c("donnees", "exposition"), c("donnees", "exposition"))),
  aleatoire = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

# Catalogue des paramètres par étape (ADR 0007, point 5.2 et annotation du
# 17/10/2026) : tous requis, aucun défaut. Les valeurs sont contrôlées par le
# module de l'étape. Règle commune (#23) : chaque paramètre a exactement le
# type de base de la colonne `type` et ne porte aucun attribut (ni names, ni
# dim, ni classe), sinon refus sous le motif propre au paramètre.
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
                "nombre double unique, fini, 0 < niveau < 1"),
    stringsAsFactors = FALSE),
  # Méthode requise, sans défaut (M293) ; plage_ages à valeur entière en
  # double (M246, M257).
  positionnement = data.frame(
    parametre = c("methode", "table_reference", "plage_ages", "niveau"),
    type = c("character", "character", "double", "double"),
    domaine = c("smr (méthode 1, seule mise en oeuvre)",
                "clé de la table dans l'argument tables_reference, chaîne unique non vide",
                "deux valeurs entières, 0 <= min <= max, couvertes par la table",
                "nombre double unique, fini, 0 < niveau < 1"),
    stringsAsFactors = FALSE),
  # Test d'homogénéité entre segments (#168 ; F10) : paramètres à valeur
  # entière en double (M246), un integer est refusé (M257) ; segmentation du
  # plan non vide (refus homogeneite_sans_segmentation).
  homogeneite = data.frame(
    parametre = c("plage_ages", "tranches_age", "replications"),
    type = c("double", "double", "double"),
    domaine = c("deux valeurs entières, 0 <= min <= max",
                "bornes inférieures entières strictement croissantes, la première égale au minimum de plage_ages",
                "nombre entier unique, au moins 1 (réplications de Monte-Carlo)"),
    stringsAsFactors = FALSE),
  # Paramètres à valeur entière en double (M246) ; un integer est refusé
  # (M257).
  validation = data.frame(
    parametre = c("table_reference", "relation_table", "plage_ages", "tranches_age", "niveau", "replications"),
    type = c("character", "character", "double", "double", "double", "double"),
    domaine = c("clé de la table dans l'argument tables_reference, chaîne unique non vide",
                "independante, ajustee_hors_outil ou positionnee (étape positionnement au plan, même clé, même plage)",
                "deux valeurs entières, 0 <= min <= max, couvertes par la table",
                "bornes inférieures entières strictement croissantes, la première égale au minimum de plage_ages",
                "nombre double unique, fini, 0 < niveau < 1",
                "nombre entier unique, au moins 1 (réplications de Monte-Carlo)"),
    stringsAsFactors = FALSE),
  importance_exclusions = data.frame(
    parametre = "niveau",
    type = "double",
    domaine = "nombre double unique, fini, 0 < niveau < 1",
    stringsAsFactors = FALSE)
)

# Paramètres du plan qui citent une table de référence par sa clé (ADR 0015,
# points 1.2 et 1.3 ; #73) : chemins « etapes/<étape>/<paramètre> », chacun
# un paramètre `character` de PARAMETRES_ETAPES, dans l'ordre des étapes : la
# clé de la table de l'étape `positionnement` (#167 ; F4) et celle de l'étape
# `validation` (#4). Les contrôles des tables reçues la prennent en argument
# (défaut), pour être testés par injection (M247).
PARAMETRES_CLE_TABLE <- c("etapes/positionnement/table_reference", "etapes/validation/table_reference")

# Champs d'un élément de fiche$tables_reference, dans l'ordre (ADR 0015,
# point 3.1 ; element_table_fiche(), module rapport).
CHAMPS_TABLE_FICHE <- c("nom", "statut", "source", "forme_origine", "parametres", "representation", "empreinte")

# Liste aux noms complets (noms_complets() de R/outils.R ; liste vide admise
# sans noms), sans autre attribut que names (ni data.frame, ni dim, ni
# classe ; #23). Les doublons ne sont pas contrôlés : le contrôle de `etapes`
# les admet, pour que le refus nommé etape_en_double les signale ensuite
# (#60).
liste_noms_complets <- function(x) forme_exacte(x, "list", "names") &&
  (!length(x) || noms_complets(names(x)))

# Liste nommée aux noms uniques, non vides, non manquants, sans autre
# attribut que names (#23) : liste_noms_complets() sans doublon.
liste_nommee <- function(x) liste_noms_complets(x) && !anyDuplicated(names(x))

# Graine du plan (#61 ; M243), appelée par valider_plan() après le contrôle
# des étapes préalables : `aleatoire` dit si une étape du plan tire un aléa
# (paramètre, pour tester les trois refus par injection tant qu'aucune étape
# n'est aléatoire). Ordre : graine_invalide (champ présent, NULL compris, et
# forme_graine() faux : même domaine que canoniser_plan()), puis
# graine_inattendue, puis graine_manquante. Rend NULL, invisible.
controler_graine_plan <- function(plan, aleatoire) {
  presente <- "graine" %in% names(plan)
  if (presente && !forme_graine(plan[["graine"]]))
    refuser("construire_table", "graine_invalide",
            formuler("La graine du plan doit être un nombre entier unique, fini, sans attribut, de valeur absolue au plus 2147483647."),
            list(graine = plan[["graine"]]))
  if (!aleatoire && presente)
    refuser("construire_table", "graine_inattendue",
            formuler("Le plan déclare une graine alors qu'aucune de ses étapes ne tire d'aléa."),
            list(graine = plan[["graine"]]))
  if (aleatoire && !presente)
    refuser("construire_table", "graine_manquante", formuler("Une étape du plan tire un aléa : la graine est requise."))
  invisible(NULL)
}

# Valide le plan entier, sans rien exécuter (ADR 0007, point 5.4). Rend le
# plan, inchangé et invisible ; refuse (module construire_table pour les
# codes structurels ; module donnees, exposition ou brut pour les valeurs).
valider_plan <- function(plan) {
  refus_plan <- function(motif, message, details = list())
    refuser("construire_table", motif, message, details)
  if (!liste_nommee(plan) || !length(plan))
    refus_plan("plan_mal_forme",
               formuler(paste("Le plan doit être une liste nommée non vide, aux noms uniques, sans autre attribut",
                              "que ses noms (ADR 0007, point 5.1).")))
  inconnus <- setdiff(names(plan), CHAMPS_PLAN)
  if (length(inconnus))
    refus_plan("champ_inconnu",
               formuler("Champ(s) inconnu(s) du plan : %s (champs admis : %s).",
                        liste_guillemets(inconnus), liste_guillemets(CHAMPS_PLAN)),
               list(champ = inconnus))
  manquants <- setdiff(CHAMPS_PLAN_REQUIS, names(plan))
  if (length(manquants))
    refus_plan("champ_manquant",
               formuler("Champ(s) requis absent(s) du plan : %s (aucun défaut implicite).",
                        liste_guillemets(manquants)),
               list(champ = manquants))
  etapes <- plan[["etapes"]]
  if (!liste_noms_complets(etapes))
    refus_plan("etapes_mal_formees",
               formuler(paste("Le champ « etapes » doit être une liste nommée (nom d'étape -> liste de ses paramètres),",
                              "sans autre attribut que ses noms.")))
  if (!length(etapes))
    refus_plan("aucune_etape", formuler("Le plan ne déclare aucune étape."))
  noms <- names(etapes)
  doubles <- unique(noms[duplicated(noms)])
  if (length(doubles))
    refus_plan("etape_en_double",
               formuler("Étape(s) déclarée(s) plusieurs fois : %s.", liste_guillemets(doubles)),
               list(etape = doubles))
  inconnues <- setdiff(noms, ETAPES$etape)
  if (length(inconnues))
    refus_plan("etape_inconnue",
               formuler("Étape(s) inconnue(s) : %s (étapes disponibles : %s).",
                        liste_guillemets(inconnues), liste_guillemets(ETAPES$etape)),
               list(etape = inconnues))
  rang <- match(noms, ETAPES$etape)
  if (is.unsorted(rang, strictly = TRUE))
    refus_plan("etapes_hors_ordre",
               formuler("Étapes hors de l'ordre des modules : %s reçu, ordre attendu %s.",
                        liste_guillemets(noms), liste_guillemets(ETAPES$etape[sort(rang)])),
               list(etape = noms))
  # Chaque préalable de chaque étape est contrôlé ; seuls les préalables
  # absents sont cités. `etape` et `prealable` sont les vecteurs aplatis ;
  # `prealables_par_etape` dit quel préalable manque à quelle étape (#27).
  absents <- lapply(ETAPES$prealable[rang], function(p) setdiff(p, noms))
  names(absents) <- noms
  sans_prealable <- lengths(absents) > 0L
  if (any(sans_prealable)) {
    manquants <- unique(unlist(absents))
    refus_plan("etape_prealable_manquante",
               formuler("Étape(s) %s sans %s %s.", liste_guillemets(noms[sans_prealable]),
                        if (length(manquants) == 1L) formuler("son étape préalable") else formuler("ses étapes préalables"),
                        liste_guillemets(manquants)),
               list(etape = noms[sans_prealable], prealable = manquants,
                    prealables_par_etape = absents[sans_prealable]))
  }
  controler_graine_plan(plan, any(ETAPES$aleatoire[rang]))
  for (nom in noms) {
    pe <- etapes[[nom]]
    attendus <- PARAMETRES_ETAPES[[nom]]$parametre
    if (!liste_nommee(pe))
      refus_plan("parametres_mal_formes",
                 formuler("Étape « %s » : ses paramètres doivent être une liste nommée, sans autre attribut que ses noms.",
                          citer(nom)),
                 list(etape = nom))
    inconnus <- setdiff(names(pe), attendus)
    if (length(inconnus))
      refus_plan("parametre_inconnu",
                 formuler("Étape « %s » : paramètre(s) inconnu(s) %s (paramètres admis : %s).",
                          citer(nom), liste_guillemets(inconnus), liste_guillemets(attendus)),
                 list(etape = nom, parametre = inconnus))
    manquants <- setdiff(attendus, names(pe))
    if (length(manquants))
      refus_plan("parametre_manquant",
                 formuler("Étape « %s » : paramètre(s) requis absent(s) %s (aucun défaut implicite).",
                          citer(nom), liste_guillemets(manquants)),
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
  if ("positionnement" %in% noms) {
    pp <- etapes[["positionnement"]]
    controler_parametres_positionnement(pp[["methode"]], pp[["table_reference"]], pp[["plage_ages"]], pp[["niveau"]])
  }
  if ("homogeneite" %in% noms) {
    ph <- etapes[["homogeneite"]]
    controler_parametres_homogeneite(ph[["plage_ages"]], ph[["tranches_age"]], ph[["replications"]],
                                     plan[["segmentation"]])
  }
  if ("validation" %in% noms) {
    pv <- etapes[["validation"]]
    controler_parametres_validation(pv[["table_reference"]], pv[["relation_table"]], pv[["plage_ages"]],
                                    pv[["tranches_age"]], pv[["niveau"]], pv[["replications"]],
                                    positionnement = etapes[["positionnement"]])
  }
  if ("importance_exclusions" %in% noms)
    controler_parametres_importance_exclusions(etapes[["importance_exclusions"]][["niveau"]])
  invisible(plan)
}

# Vecteurs d'ordre des trois champs du plan dont les valeurs forment un
# ensemble (ADR 0011, point 5.1 (h), quatrième annotation du 20/10/2026 ; #63),
# liste nommée par le chemin du champ dans le plan. Une fonction, non une
# constante de premier niveau : les fichiers de R/ sont évalués par ordre
# alphabétique à l'installation, `construire_table.R` avant `donnees.R`,
# dont les constantes (ROLES_SEGMENTATION, MOTIFS_*) ne sont pas encore
# définies ; `brut.R` (ESTIMATEURS_BRUT) l'est déjà, mais la fonction les
# lit toutes trois au même endroit. Exclusions : union des motifs des
# recettes, motifs des lignes en tête (motifs_recettes(), ADR 0016,
# point 6.4).
ordres_canoniques <- function()
  list(segmentation = ROLES_SEGMENTATION,
       "etapes/donnees/exclusions" = motifs_recettes(),
       "etapes/brut/estimateurs" = ESTIMATEURS_BRUT)

# --- tables de référence reçues (ADR 0015, points 1.3 et 4 ; #73) --------------

# Forme de l'argument `tables_reference` (contrôle 1.3, point 1 ; même contrôle
# au rejeu, 7 bis, point 0) : NULL, ou liste nommée (liste_nommee()) aux noms
# sans encodage « bytes » et écrivables dans la fiche (octets_chaines(),
# module rapport) ; list() équivaut à NULL. Puis, pour chaque élément, dans
# l'ordre des clés converties en UTF-8 et triées par octets : (a) classe
# table_reference exacte et colonnes du constructeur ; (b) reconstruction par
# table_reference() sur le data.frame nu de ses colonnes et ses métadonnées,
# sans refus ; (c) objet identical() à sa reconstruction (ordre canonique,
# valeurs contrôlées, attributs) ; (d) élément de fiche (element_table_fiche(),
# module rapport) écrit en forme « texte » sous sa clé, puis relu identical()
# (décision Q1 du mainteneur, 06/11/2026). Premier défaut : refus
# tables_reference_mal_formees (module construire_table). Rend NULL sans
# table, sinon list(tables = objets, champ = éléments de fiche), tous deux
# nommés par clé, dans l'ordre des clés.
controler_forme_tables <- function(tables) {
  if (is.null(tables)) return(NULL)
  # Clés écrivables dans la fiche : refus d'octets_chaines() (module rapport)
  # rattrapé ; la clé en cause et le motif de l'écrivain vont dans les details
  # (audit de #73, m2), le message reste celui de l'argument.
  ecriture <- if (liste_nommee(tables) && !length(noms_bytes(tables)) && length(tables))
    tryCatch({ octets_chaines(attr(tables, "names", exact = TRUE), "tables_reference/@noms"); NULL },
             refus_tablexp = function(e) e)
  if (!liste_nommee(tables) || length(noms_bytes(tables)) || !is.null(ecriture))
    refuser("construire_table", "tables_reference_mal_formees",
            formuler(paste("L'argument « tables_reference » doit être NULL (aucune table de référence) ou une liste",
                           "nommée aux noms uniques, non vides, sans autre attribut que ses noms, d'objets rendus par",
                           "table_reference() (ADR 0015, point 1.1) ; reçu : objet de classe %s (type %s, longueur %d)."),
                     paste(citer(class(tables)), collapse = "/"), citer(typeof(tables)),
                     length(if (classe_bytes(tables)) unclass(tables) else tables)),
            list(cle = if (is.null(ecriture)) NA_character_
                       else attr(tables, "names", exact = TRUE)[ecriture$details$lignes],
                 lu = decrire_forme(list(tables_reference = tables), "tables_reference"),
                 attendu = texte_moteur("NULL ou liste nommée d'objets table_reference"), motif_constructeur = NA_character_,
                 chemin = if (is.null(ecriture)) NA_character_ else "tables_reference/@noms",
                 motif_ecriture = if (is.null(ecriture)) NA_character_ else ecriture$motif))
  if (!length(tables)) return(NULL)
  cles <- enc2utf8(attr(tables, "names", exact = TRUE))
  o <- order(cles, method = "radix")
  objets <- champ <- stats::setNames(vector("list", length(o)), cles[o])
  classe_attendue <- c("table_reference", "data.frame")
  for (i in o) {
    k <- cles[i]
    x <- .subset2(tables, i)
    mal_formee <- function(texte, lu, attendu, motif = NA_character_, chemin = NA_character_)
      refuser("construire_table", "tables_reference_mal_formees",
              formuler(paste("Table de clé « %s » : %s. Chaque élément de « tables_reference » doit être l'objet rendu",
                             "par table_reference(), sans modification (classe, attributs, ordre canonique des lignes,",
                             "valeurs contrôlées), et ses métadonnées doivent s'écrire dans la fiche."),
                       citer(k), texte),
              list(cle = k, lu = lu, attendu = attendu, motif_constructeur = motif, chemin = chemin))
    modifie <- formuler("objet modifié après table_reference()")
    # (a) Classe exacte, lue par attr() (une classe « bytes » ne lève pas).
    cl <- attr(x, "class", exact = TRUE)
    if (!identical(cl, classe_attendue))
      mal_formee(formuler("objet de classe %s au lieu de table_reference", paste(citer(class(x)), collapse = "/")),
                 class(x), classe_attendue)
    colonnes <- attr(x, "names", exact = TRUE)
    if (!identical(colonnes, c("age", "sexe", "qx")) && !identical(colonnes, c("age", "sexe", "qx", "annee")))
      mal_formee(modifie, colonnes, "age, sexe, qx [, annee] (colonnes de table_reference())")
    # (b) Reconstruction : data.frame nu des colonnes, métadonnées lues dans
    # les attributs (un attribut absent est passé NULL, que le constructeur
    # refuse) ; une erreur R du data.frame nu vaut objet modifié.
    r <- tryCatch({
      nu <- data.frame(lapply(stats::setNames(colonnes, colonnes), function(n) .subset2(x, n)),
                       stringsAsFactors = FALSE)
      table_reference(nu, attr(x, "nom", exact = TRUE), attr(x, "statut", exact = TRUE),
                      attr(x, "source", exact = TRUE), attr(x, "parametres", exact = TRUE),
                      attr(x, "forme_origine", exact = TRUE))
    }, refus_tablexp = function(e) e, error = function(e) NULL)
    if (is.null(r)) mal_formee(modifie, "data.frame nu non reconstructible", "colonnes de table_reference()")
    if (inherits(r, "refus_tablexp"))
      mal_formee(formuler("refusé par table_reference() (motif %s)", citer(r$motif)), r$motif, "aucun refus",
                 motif = r$motif)
    # (c) Identité avec la reconstruction.
    if (!identical(x, r)) mal_formee(modifie, texte_moteur("objet reçu"), "objet reconstruit par table_reference()")
    # (d) Écriture et relecture de l'élément de fiche, sous sa clé.
    el <- element_table_fiche(x)
    ecrit <- stats::setNames(list(el), k)
    octets <- tryCatch(texte_objet(ecrit, PREMIERE_LIGNE_FICHE, character(0)), refus_tablexp = function(e) e)
    if (inherits(octets, "refus_tablexp")) {
      ch <- octets$details$chemin
      if (is.null(ch)) ch <- octets$details$role
      if (!is.character(ch) || length(ch) != 1L) ch <- NA_character_
      mal_formee(formuler("métadonnée sans écriture exacte dans la fiche (%s)",
                          formuler("refus %s, %s", citer(octets$motif), citer(ch))),
                 octets$motif, texte_moteur("écriture du texte de fiche sans refus"), chemin = ch)
    }
    relu <- tryCatch(lire_texte(octets, PREMIERE_LIGNE_FICHE, "fiche"), refus_tablexp = function(e) e)
    if (!identical(relu, ecrit))
      mal_formee(formuler("métadonnée sans écriture exacte dans la fiche (%s)", formuler("relecture différente")),
                 texte_moteur("élément relu différent"), texte_moteur("élément relu identical() à l'élément écrit"))
    objets[[k]] <- x
    champ[k] <- list(el)
  }
  list(tables = objets, champ = champ)
}

# Clés citées par le plan (ADR 0015, point 1.2) : pour chaque chemin
# « etapes/<étape>/<paramètre> » de `parametres_cle` dont l'étape est au plan,
# la valeur du paramètre si c'est une chaîne unique (sa forme est contrôlée
# par le module de l'étape). Rend un data.frame (cle, parametre), dans l'ordre
# de `parametres_cle` ; une clé citée par deux paramètres y figure deux fois.
cles_citees <- function(plan, parametres_cle) {
  cle <- parametre <- character(0)
  for (chemin in parametres_cle) {
    s <- strsplit(chemin, "/", fixed = TRUE)[[1L]]
    v <- plan
    for (n in s) v <- if (is.list(v)) lire_champ(v, n)
    if (chaine_unique(v)) {
      cle <- c(cle, v); parametre <- c(parametre, chemin)
    }
  }
  data.frame(cle = cle, parametre = parametre, stringsAsFactors = FALSE)
}

# Contrôle 1.3 (ADR 0015, point 1.3 ; spécification de #73, § 3), à l'entrée
# de construire_table(), après controler_provenance() et avant toute étape ;
# refus de module construire_table, dans cet ordre : forme
# (controler_forme_tables()) ; clé citée par le plan et non reçue
# (table_reference_absente, premier paramètre dans l'ordre de
# `parametres_cle`) ; clé reçue citée par aucun paramètre
# (table_reference_inattendue, toutes citées) ; table de statut embarquee
# dont le nom est absent du catalogue `embarquees` (table_embarquee_absente),
# puis dont l'empreinte diffère de celle de la table du catalogue
# (table_embarquee_discordante). `parametres_cle` et `embarquees` :
# PARAMETRES_CLE_TABLE et TABLES_EMBARQUEES par défaut, injectés par les
# tests (M247). Rend la valeur de fiche$tables_reference : NULL, ou liste des
# éléments par clé.
controler_tables_reference <- function(tables_reference, plan, parametres_cle = PARAMETRES_CLE_TABLE,
                                       embarquees = TABLES_EMBARQUEES) {
  f <- controler_forme_tables(tables_reference)
  champ <- f$champ
  recues <- if (is.null(champ)) character(0) else names(champ)
  citees <- cles_citees(plan, parametres_cle)
  absentes <- which(!citees$cle %in% recues)
  if (length(absentes)) {
    a <- absentes[1L]
    refuser("construire_table", "table_reference_absente",
            formuler(paste("Le plan cite la table de clé « %s » (paramètre %s) ; « tables_reference » %s. Passer la",
                           "table, rendue par table_reference(), sous cette clé : le moteur ne lit aucun fichier et ne",
                           "cherche aucune table par son nom."),
                     citer(citees$cle[a]), citer(citees$parametre[a]),
                     if (length(recues)) formuler("ne porte pas cette clé (clés reçues : %s)", liste_guillemets(recues))
                     else formuler("ne porte aucune table (NULL)")),
            list(cle = citees$cle[a], parametre = citees$parametre[a], lu = recues, attendu = citees$cle[a]))
  }
  inattendues <- recues[!recues %in% citees$cle]
  if (length(inattendues)) {
    attendues <- unique(citees$cle)
    refuser("construire_table", "table_reference_inattendue",
            formuler(paste("Table(s) de clé %s reçue(s) dans « tables_reference » et citée(s) par aucun paramètre du",
                           "plan (clés citées : %s) : une table de référence n'entre dans la construction que si une",
                           "étape la lit."),
                     liste_guillemets(inattendues),
                     if (length(attendues)) liste_guillemets(attendues) else formuler("aucune")),
            list(cle = inattendues, lu = inattendues, attendu = attendues))
  }
  catalogue <- attr(embarquees, "names", exact = TRUE)
  if (is.null(catalogue)) catalogue <- character(0)
  embarquees_recues <- recues[vapply(champ, function(e) identical(e$statut, "embarquee"), NA)]
  for (k in embarquees_recues) {
    nom <- champ[[k]]$nom
    if (!nom %in% catalogue)
      refuser("construire_table", "table_embarquee_absente",
              formuler(paste("Table de clé « %s », de statut « embarquee » et de nom « %s » : ce package n'embarque",
                             "aucune table de ce nom (tables embarquées : %s). Le statut « embarquee » est vérifié",
                             "contre le package ; une table importée se déclare de statut « importee »."),
                       citer(k), citer(nom), if (length(catalogue)) liste_guillemets(catalogue) else formuler("aucune")),
              list(cle = k, nom = nom, lu = nom, attendu = catalogue))
  }
  for (k in embarquees_recues) {
    nom <- champ[[k]]$nom
    attendue <- empreinte_table(embarquees[[nom]])
    if (!identical(champ[[k]]$empreinte, attendue))
      refuser("construire_table", "table_embarquee_discordante",
              formuler(paste("Table de clé « %s », de statut « embarquee » et de nom « %s » : empreinte %s, différente",
                             "de celle de la table de ce nom embarquée dans le package installé (%s). Obtenir la table",
                             "par table_embarquee(), ou la déclarer de statut « importee »."),
                       citer(k), citer(nom), citer(champ[[k]]$empreinte), citer(attendue)),
              list(cle = k, nom = nom, lu = champ[[k]]$empreinte, attendu = attendue))
  }
  champ
}

# Contrôle 7 bis du rejeu (ADR 0015, point 4.2 ; spécification de #73, § 3),
# après le contrôle de l'empreinte des données et avant construire_table() :
# 0. forme de l'argument (controler_forme_tables()) ; 1. clés reçues absentes
# de fiche$tables_reference ou de statut embarquee dans la fiche
# (table_reference_inattendue, toutes citées) ; 2. pour chaque clé de la
# fiche, dans son ordre : (a) représentation (representation_table_non_admise),
# (b) origine, table relue dans `embarquees` par son nom (sinon
# table_embarquee_absente) ou table reçue sous la clé (sinon
# table_reference_absente), (c) empreinte recalculée par empreinte_table()
# égale à celle de la fiche (empreinte_table_discordante). Le contrôle 1.3
# sur la liste assemblée et le plan de la fiche n'est pas fait ici : il l'est
# une fois, par construire_table(), que rejouer_fiche() appelle ensuite avec
# cette liste (#82 ; #164, point 2). `fiche_tables` : champ
# fiche$tables_reference, de forme contrôlée (contrôle 2) ; `code_fiche` :
# empreinte du code lue dans la fiche, citée. Rend la liste assemblée (NULL
# sans table), les empreintes recalculées et les textes de la ligne
# « tables_reference » du bilan.
controler_tables_rejeu <- function(fiche_tables, tables_reference, code_fiche, embarquees = TABLES_EMBARQUEES) {
  f <- controler_forme_tables(tables_reference)
  recues <- if (is.null(f)) character(0) else names(f$tables)
  cles <- if (is.null(fiche_tables)) character(0) else names(fiche_tables)
  statuts <- vapply(fiche_tables, function(e) e$statut, "")
  attendues <- cles[statuts != "embarquee"]
  trop <- recues[!recues %in% attendues]
  if (length(trop))
    refuser("construire_table", "table_reference_inattendue",
            formuler(paste("Rejeu : clé(s) %s reçue(s) dans « tables_reference », absente(s) de fiche/tables_reference",
                           "ou de statut « embarquee » (clés attendues : %s). Une table embarquée est relue par son nom",
                           "dans le package, jamais reçue."),
                     liste_guillemets(trop), if (length(attendues)) liste_guillemets(attendues) else formuler("aucune")),
            list(cle = trop, lu = trop, attendu = attendues))
  catalogue <- attr(embarquees, "names", exact = TRUE)
  if (is.null(catalogue)) catalogue <- character(0)
  assemblee <- list()
  empreintes <- origines <- character(0)
  for (k in cles) {
    e <- fiche_tables[[k]]
    # (a) Représentation de l'empreinte de table.
    if (!identical(e$representation, REPRESENTATION_TABLE))
      refuser("construire_table", "representation_table_non_admise",
              formuler(paste("Table de clé « %s » (nom « %s ») : représentation « %s » lue dans la fiche, « %s »",
                             "attendue. Ce code calcule l'empreinte d'une table sous cette seule définition, et une",
                             "empreinte d'une autre définition ne lui est jamais égale ; aucune conversion n'est faite."),
                       citer(k), citer(e$nom), citer(e$representation), texte_moteur(REPRESENTATION_TABLE)),
              list(cle = k, nom = e$nom, lu = e$representation, attendu = REPRESENTATION_TABLE))
    # (b) Origine : relue dans le package, ou reçue.
    if (identical(e$statut, "embarquee")) {
      if (!e$nom %in% catalogue)
        refuser("construire_table", "table_embarquee_absente",
                formuler(paste("Table de clé « %s », de statut « embarquee » et de nom « %s » : ce package n'embarque",
                               "aucune table de ce nom (tables embarquées : %s) ; rejouer cette fiche avec le package",
                               "qui l'embarquait (empreinte du code de la fiche : %s)."),
                         citer(k), citer(e$nom), if (length(catalogue)) liste_guillemets(catalogue) else formuler("aucune"),
                         citer(code_fiche)),
                list(cle = k, nom = e$nom, lu = e$nom, attendu = catalogue))
      t <- embarquees[[e$nom]]
      origine <- texte_moteur("relue dans le package")
    } else {
      if (!k %in% recues)
        refuser("construire_table", "table_reference_absente",
                formuler(paste("Rejeu : la fiche porte la table de clé « %s » (nom « %s », statut « %s », empreinte %s),",
                               "absente de l'argument « tables_reference » (%s). Une table importée ou synthétique est",
                               "une entrée du rejeu, comme les données : la passer sous cette clé."),
                         citer(k), citer(e$nom), citer(e$statut), citer(e$empreinte),
                         if (length(recues)) formuler("clés reçues : %s", liste_guillemets(recues))
                         else formuler("aucune table reçue")),
                list(cle = k, nom = e$nom, statut = e$statut, lu = recues, attendu = k))
      t <- f$tables[[k]]
      origine <- texte_moteur("reçue")
    }
    # (c) Empreinte recalculée.
    calculee <- empreinte_table(t)
    if (!identical(calculee, e$empreinte)) {
      complement <- if (identical(e$statut, "embarquee"))
        formuler(paste(" Le package installé a pu corriger cette table depuis la construction (tableau avant / après",
                       "et visa, ADR 0006) ; rejouer avec le package de la fiche (empreinte du code %s)."),
                 citer(code_fiche))
      else if (identical(e$statut, "synthetique"))
        formuler(paste(" Une table synthétique calculée sur une autre plateforme peut différer au dernier bit",
                       "(fonctions de la libm) ; ses paramètres sont dans fiche/tables_reference/%s/parametres."),
                 citer(k))
      else ""
      refuser("construire_table", "empreinte_table_discordante",
              formuler(paste("Empreinte de la table de clé « %s » discordante : %s lue dans fiche/tables_reference",
                             "(nom « %s », statut « %s »), %s recalculée sur la table %s. L'empreinte porte sur la",
                             "représentation « %s » du contenu de la table (âge, sexe, année si présente, q_x ; lignes",
                             "dans l'ordre canonique) : une seule valeur de q_x différente au dernier bit suffit à la",
                             "changer. La table est une entrée de la construction : l'effet d'une autre table se mesure",
                             "par construire_table(), non par le rejeu.%s"),
                       citer(k), citer(e$empreinte), citer(e$nom), citer(e$statut), citer(calculee), origine,
                       texte_moteur(REPRESENTATION_TABLE), complement),
              list(cle = k, nom = e$nom, statut = e$statut, lu = e$empreinte, attendu = calculee, origine = origine))
    }
    assemblee[k] <- list(t)
    empreintes <- c(empreintes, calculee)
    origines <- c(origines, origine)
  }
  names(empreintes) <- cles
  tables <- if (length(assemblee)) assemblee
  list(tables = tables, empreintes = empreintes,
       attendu = texte_moteur(if (!length(cles)) "aucune table de référence (fiche/tables_reference : NULL)"
                              else paste0(sans_controle(cles), " : ", vapply(fiche_tables, function(e) e$empreinte, ""),
                                          collapse = " ; ")),
       lu = texte_moteur(if (!length(cles)) "aucune table reçue ni relue"
                         else paste0(sans_controle(cles), " : ", empreintes, " (", origines, ")", collapse = " ; ")))
}

# Table de clé `cle` dans la liste `tables` (contrôlée par
# controler_forme_tables() : noms uniques, sans encodage « bytes ») ; clés
# comparées en UTF-8, comme le contrôle 1.3. Rend l'objet table_reference.
table_citee <- function(tables, cle) {
  k <- match(enc2utf8(cle), enc2utf8(attr(tables, "names", exact = TRUE)))
  if (is.na(k)) stop("table_citee() : clé citée absente des tables reçues (erreur de programmation).", call. = FALSE)
  .subset2(tables, k)
}

# --- contrôle des entrées (#82 ; ADR 0007, point 3.1 ; ADR 0012, point 3) ------

# Première couche, partagée par construire_table() (par controler_entrees())
# et rejouer_fiche() (contrôle 6), dans cet ordre : plan (valider_plan(),
# puis canoniser_plan()), rôles et recette (controler_roles(), qui rend la
# recette de deduire_recette() ; module donnees), convention
# (resoudre_convention(), si `exposition` est au plan ; module exposition),
# matrice recette x étape x estimateur (controler_matrice_recette(), module
# donnees ; ADR 0016, point 5.3), provenance (controler_provenance()).
# Rend les valeurs validées : list(plan (canonique), p (transition, période
# et délai : valeurs_parametres_donnees(), module donnees, la valeur que rend
# controler_parametres_donnees() appelée par valider_plan()), recette, conv
# (NULL sans exposition)). Une seule source de la recette et de la convention
# pour les étapes et la fiche.
controler_plan_roles <- function(donnees, roles, plan, provenance) {
  plan <- canoniser_plan(valider_plan(plan), CHAMPS_PLAN, PARAMETRES_ETAPES, ordres_canoniques())
  recette <- controler_roles(donnees, roles, plan[["segmentation"]])
  etapes_plan <- plan[["etapes"]]
  conv <- if ("exposition" %in% names(etapes_plan))
    resoudre_convention(etapes_plan[["exposition"]][["convention"]], recette$convention_native)
  controler_matrice_recette(recette, roles, plan)
  controler_provenance(provenance)
  # p recalculé par la fonction qui forme la valeur de
  # controler_parametres_donnees() dans valider_plan(), ici sur le plan
  # canonique : valeurs identical() mesurées sur R1 à R12 (revue finale de
  # B14). Un chemin unique demanderait de changer le contrat de valider_plan()
  # (exportée) ou les comptes de contrôles du test « contrôles non répétés »
  # (#22, #82 ; tests/unitaires/test_construire_table.R) : renvoyé à une issue.
  list(plan = plan, p = valeurs_parametres_donnees(plan[["transition"]], plan[["periode"]], plan[["delai_declaration"]]),
       recette = recette, conv = conv)
}

# Seconde couche, appelée par construire_table() seule, une fois, avant toute
# étape : controler_plan_roles(), puis le contrôle 1.3 des tables de
# référence (controler_tables_reference(), ADR 0015, point 1.3), puis le
# contrôle d'entrée de chaque étape qui lit une table (ADR 0015, point 7.3 ;
# M253) : controler_entrees_validation() (module positionnement) sur la table
# citée par l'étape `positionnement` (#167), puis sur celle citée par l'étape
# `validation`. Rend les valeurs de controler_plan_roles(),
# plus tables_fiche (valeur de fiche$tables_reference) et tables (table citée
# par étape, liste nommée par étape ; vide sans table citée). Au rejeu, ces
# contrôles ne sont faits qu'ici, par l'unique appel de construire_table()
# (#82 ; #164, point 2).
controler_entrees <- function(donnees, roles, plan, provenance, tables_reference) {
  e <- controler_plan_roles(donnees, roles, plan, provenance)
  etapes_plan <- e$plan[["etapes"]]
  tables_fiche <- controler_tables_reference(tables_reference, e$plan)
  tables <- list()
  if ("positionnement" %in% names(etapes_plan)) {
    pp <- etapes_plan[["positionnement"]]
    tables$positionnement <- table_citee(tables_reference, pp[["table_reference"]])
    controler_entrees_validation(tables$positionnement, pp[["table_reference"]], e$conv$retenue,
                                 e$plan[["segmentation"]], pp[["plage_ages"]], "positionnement")
  }
  # Sous la relation positionnee, valider_plan() impose à la validation la
  # clé de table et la plage d'âges du positionnement : ce second contrôle
  # refait alors le premier, à l'identique. Il est gardé pour l'uniformité
  # (un contrôle par étape qui lit une table).
  if ("validation" %in% names(etapes_plan)) {
    pv <- etapes_plan[["validation"]]
    tables$validation <- table_citee(tables_reference, pv[["table_reference"]])
    controler_entrees_validation(tables$validation, pv[["table_reference"]], e$conv$retenue,
                                 e$plan[["segmentation"]], pv[["plage_ages"]], "validation")
  }
  c(e, list(tables_fiche = tables_fiche, tables = tables))
}

# Point d'entrée unique (ADR 0007, point 3.1).
#   donnees    : data.frame (le moteur ne lit pas de fichier) ;
#   roles      : vecteur de chaînes nommé rôle -> colonne ;
#   plan       : liste nommée (valider_plan()) ;
#   provenance : NULL (données construites en mémoire) ou list(fichier,
#                empreinte, algorithme) transmise par la couche de lecture ;
#   tables_reference : NULL, ou liste nommée par clé d'objets rendus par
#                table_reference(), chaque clé citée par un paramètre du plan
#                (ADR 0015, point 1 ; #73) ; identité de chaque table dans
#                fiche$tables_reference, aucune copie dans l'objet.
# Rend un objet de classe `construction` : table, etapes, avertissements,
# fiche, execution (seul champ qui change entre deux exécutions identiques).
construire_table <- function(donnees, roles, plan, provenance = NULL, tables_reference = NULL) {
  debut <- Sys.time()
  # Validation entière avant toute exécution (controler_entrees()) : plan,
  # rôles, recette et convention, provenance, tables de référence (1.3),
  # entrées des étapes qui lisent une table (7.3). Chaque contrôle s'exécute
  # ici une seule fois ; les fonctions d'étape reçoivent les valeurs validées
  # et n'en refont aucun (#22, #82).
  entrees <- controler_entrees(donnees, roles, plan, provenance, tables_reference)
  plan <- entrees$plan
  p <- entrees$p
  recette <- entrees$recette
  conv <- entrees$conv
  etapes_plan <- plan[["etapes"]]
  table_validation <- entrees$tables[["validation"]]
  empreintes <- empreintes_construction(donnees, roles, provenance)

  etapes <- list()
  avertissements <- avertissements_vides()
  for (nom in names(etapes_plan)) {
    pe <- etapes_plan[[nom]]
    r <- switch(nom,
      donnees = preparer_donnees(donnees, roles, p = p, segmentation = plan[["segmentation"]],
                                 exclusions = pe[["exclusions"]]),
      exposition = calculer_exposition(etapes[["donnees"]][["resultat"]][["observations"]], p = p,
                                       segmentation = plan[["segmentation"]], recette = recette, conv = conv,
                                       par_annee = pe[["par_annee"]]),
      brut = calculer_taux_bruts(etapes[["exposition"]][["resultat"]][["exposition"]],
                                 segmentation = plan[["segmentation"]],
                                 par_annee = etapes[["exposition"]][["parametres"]][["par_annee"]],
                                 convention = etapes[["exposition"]][["parametres"]][["convention"]][["retenue"]],
                                 unite_exposition = etapes[["exposition"]][["parametres"]][["unite_exposition"]],
                                 estimateurs = pe[["estimateurs"]], niveau = pe[["niveau"]],
                                 observations = fenetres, p = p,
                                 parametres_exposition = etapes[["exposition"]][["parametres"]]),
      positionnement = calculer_positionnement(etapes[["exposition"]][["resultat"]][["exposition"]],
                                               segmentation = plan[["segmentation"]],
                                               par_annee = etapes[["exposition"]][["parametres"]][["par_annee"]],
                                               convention = etapes[["exposition"]][["parametres"]][["convention"]][["retenue"]],
                                               table = entrees$tables[["positionnement"]],
                                               table_reference = pe[["table_reference"]], methode = pe[["methode"]],
                                               plage_ages = pe[["plage_ages"]], niveau = pe[["niveau"]]),
      homogeneite = calculer_homogeneite(etapes[["exposition"]][["resultat"]][["exposition"]],
                                         segmentation = plan[["segmentation"]],
                                         par_annee = etapes[["exposition"]][["parametres"]][["par_annee"]],
                                         convention = etapes[["exposition"]][["parametres"]][["convention"]][["retenue"]],
                                         plage_ages = pe[["plage_ages"]], tranches_age = pe[["tranches_age"]],
                                         replications = pe[["replications"]], graine = plan[["graine"]]),
      validation = calculer_validation(etapes[["exposition"]][["resultat"]][["exposition"]],
                                       segmentation = plan[["segmentation"]],
                                       par_annee = etapes[["exposition"]][["parametres"]][["par_annee"]],
                                       convention = etapes[["exposition"]][["parametres"]][["convention"]][["retenue"]],
                                       table = table_validation, table_reference = pe[["table_reference"]],
                                       relation_table = pe[["relation_table"]], plage_ages = pe[["plage_ages"]],
                                       tranches_age = pe[["tranches_age"]], niveau = pe[["niveau"]],
                                       replications = pe[["replications"]], graine = plan[["graine"]],
                                       positionnement = etapes[["positionnement"]]),
      importance_exclusions = calculer_importance_exclusions(donnees, roles, etapes[["donnees"]],
                                                             etapes[["exposition"]], etapes[["validation"]],
                                                             table = table_validation, p = p,
                                                             recette = recette, conv = conv,
                                                             segmentation = plan[["segmentation"]],
                                                             niveau = pe[["niveau"]]),
      stop(sprintf("construire_table() : étape « %s » sans fonction d'étape (erreur de programmation).", nom),
           call. = FALSE))
    etapes[[nom]] <- r[c("parametres", "resultat", "diagnostics")]
    # Transmission entre étapes hors de l'objet (#38) : fenêtres des
    # observations rendues par `exposition` (préalable de `brut`, contrôlé
    # par valider_plan()), passées à `brut` (argument observations, évalué
    # pour Kaplan-Meier seulement) puis libérées ; ni dans etapes, ni dans
    # la fiche. Conservées seulement si le plan retient Kaplan-Meier ; r est
    # vidé en fin de tour, sinon il garde les fenêtres vivantes pendant l'étape
    # suivante.
    if (nom == "exposition")
      fenetres <- if ("kaplan_meier" %in% etapes_plan[["brut"]][["estimateurs"]]) r[["fenetres"]]
    if (nom == "brut") fenetres <- NULL
    avertissements <- rbind(avertissements, r$avertissements)
    r <- NULL
  }
  rownames(avertissements) <- NULL

  x <- structure(
    list(table = NULL, etapes = etapes, avertissements = avertissements,
         fiche = assembler_fiche(roles, plan, recette, etapes, avertissements, empreintes, provenance,
                                 entrees$tables_fiche)),
    class = "construction")
  # Empreinte du résultat sur l'objet assemblé, avant execution, hors de son
  # périmètre (ADR 0011, point 11.1 ; #67).
  x$fiche$empreintes$resultat <- empreinte_resultat(x)
  x$execution <- informations_execution(debut)
  x
}

# --- rejeu outillé (ADR 0012, point 3 ; #86) -----------------------------------

# Valeur lue dans la fiche, pour un message ou le bilan du rejeu : NULL ->
# « absente » ; chaîne de longueur 1 sans attribut -> la chaîne (NA -> « NA ») ;
# autre -> « illisible (<type> de longueur <n>) ». La valeur est rendue telle
# quelle (sans_controle() pour un message).
texte_lu <- function(v) {
  if (is.null(v)) return("absente")
  if (is.character(v) && is.null(attributes(v)) && length(v) == 1L) return(if (is.na(v)) "NA" else v)
  # length() dispatche sur la classe et lève sur une classe d'encodage
  # « bytes » (#128) : les attributs sont contrôlés avant length(), qui lit
  # la valeur sans sa classe.
  sprintf("illisible (%s de longueur %d)", typeof(v), length(if (classe_bytes(v)) unclass(v) else v))
}

# Valeur lue dans la fiche, citée dans un message de refus du rejeu : règle
# unique neutraliser_citation() (R/outils.R, #94) : caractères de contrôle et
# séparateurs de ligne écrits <U+XXXX>, chaîne « bytes » citée sous un repère
# (au lieu de l'erreur R de sprintf()), marque UTF-8.
sans_controle <- function(s) neutraliser_citation(s)

# Forme du champ `nom` de la liste `conteneur`, pour le refus
# fiche_non_conforme : « champ absent » (nom absent, ou conteneur qui n'est pas
# une liste), sinon « <type> de longueur <n> », complété au besoin par « , NA »,
# « , vide », « , attribut(s) <noms> » (names non cité pour une liste, dont
# les noms sont admis), « , noms en double : <noms> » et « , noms incomplets »
# (nom vide ou NA ; décision Q-A d'actuary, reprise de #86). Une valeur de
# classe d'encodage « bytes » est décrite sans sa classe (unclass()) pour
# length(), is.na() et names(), qui dispatchent et lèveraient une erreur R
# (#128) ; ses attributs, `class` compris, sont lus avant. Les noms en double
# sont cités un à un par sans_controle() : un nom « bytes » n'efface pas les
# autres (#128), et le premier nom « bytes » est cité par sa position, sous un
# repère (« nom(s) d'encodage bytes (n° k : <repère>) »).
decrire_forme <- function(conteneur, nom) {
  if (!is.list(conteneur) || !(nom %in% attr(conteneur, "names", exact = TRUE))) return("champ absent")
  v <- lire_champ(conteneur, nom)
  a <- names(attributes(v))
  if (classe_bytes(v)) v <- unclass(v)
  s <- formuler("%s de longueur %d", typeof(v), length(v))
  if (is.atomic(v) && length(v) == 1L && is.na(v)) s <- paste0(s, ", NA")
  else if (is.character(v) && length(v) == 1L && !nzchar(v)) s <- paste0(s, ", vide")
  if (is.list(v)) a <- setdiff(a, "names")
  if (length(a)) s <- paste0(s, ", attribut(s) ", paste(sans_controle(a), collapse = ", "))
  n <- names(v)
  if (!is.null(n)) {
    complets <- n[!is.na(n) & nzchar(n)]
    doubles <- unique(complets[duplicated(complets)])
    if (length(doubles)) s <- paste0(s, ", noms en double : ", paste(sans_controle(doubles), collapse = ", "))
    if (anyNA(n) || !all(nzchar(n[!is.na(n)]))) s <- paste0(s, ", noms incomplets")
    b <- noms_bytes(v)
    if (length(b)) s <- formuler("%s, nom(s) d'encodage bytes (n° %d : %s)", s, b[1L], sans_controle(n[b[1L]]))
  }
  s
}

# Paires « nom=valeur » d'une liste nommée de la fiche (versions,
# plateforme), jointes par « ; » ; texte_lu() d'une autre valeur, dont une
# liste de classe « bytes » (names() lèverait une erreur R, #128). Les noms
# sont cités par neutraliser_citation() : un nom « bytes » l'est sous un
# repère, au lieu de l'erreur R de sprintf() ; ces champs sont des témoins,
# jamais motifs de refus (ADR 0012, point 3.6).
paires_lues <- function(l) {
  n <- attr(l, "names", exact = TRUE)
  if (!is.list(l) || classe_bytes(l) || is.null(n)) return(texte_lu(l))
  paste(sprintf("%s=%s", neutraliser_citation(n), vapply(l, texte_lu, "")), collapse = " ; ")
}

# Chemins des feuilles qui diffèrent entre la fiche rendue (`rendu`) et la
# fiche reçue (`recu`) (descente D de la spécification de #86) : deux listes
# nommées sans classe, aux noms complets et uniques, sont parcourues sur
# l'union de leurs noms (ordre de la fiche rendue, puis noms propres à la fiche
# reçue) ; toute autre valeur est une feuille, dont une liste à nom
# d'encodage « bytes » (#128) : `[[` et %in% sur ses noms lèveraient une erreur
# R (mesuré) ; la liste est citée « différente » à son chemin. (Une classe
# « bytes » est déjà une feuille par is.object().) Un nœud qui diffère sans
# qu'aucun enfant ne diffère est cité « (ordre des champs ou attributs) » ;
# `racine` nomme le nœud de départ. Rend character(0) si identical().
champs_differents <- function(rendu, recu, racine, chemin = character(0)) {
  if (identical(rendu, recu)) return(character(0))
  ici <- if (length(chemin)) paste(chemin, collapse = "/") else racine
  descend <- function(v) is.list(v) && !is.object(v) && !length(noms_bytes(v)) &&
    noms_complets(names(v)) && !anyDuplicated(names(v))
  if (!descend(rendu) || !descend(recu)) return(ici)
  d <- character(0)
  for (n in union(names(rendu), names(recu))) {
    p <- paste(c(chemin, n), collapse = "/")
    d <- c(d, if (!(n %in% names(recu))) paste(p, "(absent de la fiche reçue)")
              else if (!(n %in% names(rendu))) paste(p, "(absent de la fiche rendue)")
              else champs_differents(rendu[[n]], recu[[n]], racine, c(chemin, n)))
  }
  if (!length(d)) d <- paste(ici, "(ordre des champs ou attributs)")
  d
}

# Verdict d'un témoin par chemins (V2, V3) : « identique », ou « différente : »
# suivi des chemins de champs_differents().
verdict_chemins <- function(rendu, recu, racine) {
  d <- champs_differents(rendu, recu, racine)
  texte_moteur(if (!length(d)) "identique" else paste("différente :", paste(d, collapse = ", ")))
}

# Verdict d'un témoin par chaînes (V1) : « non déterminable » si l'un des deux
# côtés n'est pas une chaîne unique (deux NA ne prouvent pas une identité),
# sinon « identique » ou « différente ».
verdict_chaines <- function(attendu, lu) {
  texte_moteur(if (!chaine_unique(attendu) || !chaine_unique(lu)) "non déterminable"
               else if (identical(attendu, lu)) "identique" else "différente")
}

# Verdict d'un témoin de liste nommée de la fiche (V2 : versions,
# plateforme) : « non déterminable » si le côté de la fiche reçue n'est pas une
# liste sans classe aux noms complets, ou porte un nom d'encodage « bytes »
# (#128 ; une classe « bytes » est déjà un objet), sinon verdict_chemins().
verdict_liste <- function(rendu, recu, racine) {
  if (!is.list(recu) || is.object(recu) || length(noms_bytes(recu)) || !noms_complets(names(recu)))
    return(texte_moteur("non déterminable"))
  verdict_chemins(rendu, recu, racine)
}

# Périmètre du témoin « fiche » : la fiche sans versions, plateforme,
# empreintes/resultat, empreintes/code, empreintes/statut_resultat_code.
perimetre_temoin_fiche <- function(f) {
  f[c("versions", "plateforme")] <- NULL
  if (is.list(f[["empreintes"]])) f[["empreintes"]][c("resultat", "code", "statut_resultat_code")] <- NULL
  f
}

# Rejeu outillé d'une fiche (E3.7 ; ADR 0012, point 3 ; #86).
#   donnees : data.frame (le moteur ne lit pas de fichier) ;
#   fiche   : liste de la fiche, relue de son texte par lire_fiche() ou prise
#             dans x$fiche ; rôles, plan et provenance en sont lus ;
#   tables_reference : NULL, ou liste nommée par clé des tables de statut
#             importee ou synthetique de fiche$tables_reference, reçues
#             comme les données ; une table embarquée est relue par son nom
#             dans le package, jamais reçue (ADR 0015, point 1.4 ; #73).
# Contrôle, dans l'ordre, la nature des arguments, la forme des champs lus,
# version_fiche (VERSIONS_FICHE_REJEU), l'algorithme et la représentation de
# l'empreinte des données, les entrées de construire_table() de la première
# couche (controler_plan_roles() : mêmes contrôles, mêmes refus), l'empreinte
# des données, puis les tables de référence (contrôle 7 bis,
# controler_tables_rejeu() : forme, clés, représentation, origine, empreinte) ;
# s'arrête au premier refus, sans appeler construire_table(). Appelle ensuite
# construire_table() une seule fois : le contrôle 1.3 sur la liste assemblée
# et les contrôles d'entrée des étapes qui lisent une table (positionnement,
# validation) y sont faits une fois, avant toute étape (mêmes motifs, mêmes
# messages ; #82 ; #164, point 2). Rend un objet de classe `rejeu` :
# construction, l'objet rendu par construire_table() sans retouche ; bilan,
# data.frame de 14 lignes (controle, attendu, lu, verdict, statut) :
# 8 contrôles, tous « conforme » dans un bilan rendu, puis 6 témoins, jamais
# motifs de refus.
rejouer_fiche <- function(donnees, fiche, tables_reference = NULL) {
  # Message marqué UTF-8 sous toute locale (#94) : chaque message est formé
  # par formuler(), qui marque le gabarit avant sprintf() (#127).
  refus_rejeu <- function(motif, message, details = list())
    refuser("construire_table", motif, message, details)

  # 1. Nature des arguments : la fiche d'abord, puis les données (refus
  #    délégué à controler_roles(), motif donnees_mal_formees). Classe lue
  #    par %in% (match()), qui ne lève pas sur une classe d'encodage « bytes »,
  #    là où inherits() lève (« translating strings with "bytes" encoding is
  #    not allowed », mesuré sous C et C.UTF-8 ; #94).
  if ("construction" %in% class(fiche))
    refus_rejeu("fiche_non_conforme",
                formuler(paste("Rejeu : objet `construction` reçu à la place de sa fiche ; passer x$fiche, ou la fiche",
                               "relue de son texte par lire_fiche().")),
                list(champ = "(fiche)", lu = "construction", attendu = "liste nommée"))
  if ("rejeu" %in% class(fiche))
    refus_rejeu("fiche_non_conforme",
                formuler("Rejeu : objet `rejeu` reçu à la place d'une fiche ; passer r$construction$fiche."),
                list(champ = "(fiche)", lu = "rejeu", attendu = "liste nommée"))
  # Un nom du premier niveau d'encodage « bytes » : toute lecture par nom
  # (fiche[["version_fiche"]]...) lèverait une erreur R (#114), position
  # quelconque du nom ; cité par neutraliser_citation(), valeur brute dans
  # les details. Noms lus par attr(), non par names() : names() dispatche sur
  # la classe et lève sur une classe d'encodage bytes (noms_bytes(), mesuré).
  nf <- attr(fiche, "names", exact = TRUE)
  b <- if (is.list(fiche)) noms_bytes(fiche)
  if (length(b))
    refus_rejeu("fiche_non_conforme",
                formuler(paste("Rejeu : la fiche porte un nom de champ d'encodage « bytes » (champ n° %d : %s),",
                                 "non convertible en UTF-8 sans deviner ; aucun champ du premier niveau n'est lu par nom. Une fiche",
                                 "produite par le moteur ou relue par lire_fiche() n'en porte pas."),
                         b[1L], neutraliser_citation(nf[b[1L]])),
                list(champ = "(fiche)", lu = nf[b], attendu = "noms de champ sans encodage bytes"))
  if (!liste_nommee(fiche))
    refus_rejeu("fiche_non_conforme",
                formuler(paste("Rejeu : la fiche doit être une liste nommée aux noms uniques, sans autre attribut",
                                 "que ses noms ; reçu : objet de classe %s (type %s). Le rejeu ne lit aucun fichier :",
                                 "relire la fiche de ses octets par lire_fiche(readBin(fichier, \"raw\",",
                                 "file.size(fichier))), ou passer x$fiche."),
                         paste(neutraliser_citation(class(fiche)), collapse = "/"), typeof(fiche)),
                list(champ = "(fiche)", lu = class(fiche), attendu = "liste nommée"))
  if (!is.data.frame(donnees)) controler_roles(donnees, character(0), character(0))

  # 2. Forme des champs lus avant construire_table() : tous les champs en
  #    défaut dans un seul refus ; aucun n'est complété ni converti.
  attendus <- texte_moteur(c(version_fiche = "integer de longueur 1, non NA, sans attribut",
                             empreintes = "liste nommée aux noms uniques, sans autre attribut que ses noms",
                             "empreintes/algorithme" = "chaîne unique, non vide, non NA, sans attribut",
                             "empreintes/representation" = "chaîne unique, non vide, non NA, sans attribut",
                             "empreintes/donnees" = "chaîne unique, non vide, non NA, sans attribut",
                             roles = "champ présent", plan = "champ présent", provenance = "champ présent (NULL admis)",
                             tables_reference = paste(
                               "NULL, ou liste nommée non vide aux noms uniques, par clé triée par octets, d'éléments aux sept champs",
                               "nom, statut, source, forme_origine, parametres, representation, empreinte (dans cet",
                               "ordre) ; nom, representation, empreinte : chaînes uniques ; statut : synthetique,",
                               "embarquee ou importee")))
  champ <- lu <- character(0)
  v <- fiche[["version_fiche"]]
  if (!(forme_exacte(v, "integer") && length(v) == 1L && !is.na(v))) {
    champ <- c(champ, "version_fiche"); lu <- c(lu, decrire_forme(fiche, "version_fiche"))
  }
  # `empreintes` : liste_nommee() et anyDuplicated(names()) ne lèvent pas sur un
  # nom « bytes » (mesuré), qui est signalé sous le champ `empreintes` avec les
  # autres défauts (#128) ; ses trois champs se lisent par lire_champ(), qui ne
  # traduit pas les noms (`[[` lèverait).
  e <- fiche[["empreintes"]]
  if (!liste_nommee(e) || length(noms_bytes(e))) {
    champ <- c(champ, "empreintes"); lu <- c(lu, decrire_forme(fiche, "empreintes"))
  }
  if (liste_nommee(e)) for (n in c("algorithme", "representation", "donnees")) if (!chaine_unique(lire_champ(e, n))) {
    champ <- c(champ, paste0("empreintes/", n)); lu <- c(lu, decrire_forme(e, n))
  }
  for (n in c("roles", "plan", "provenance")) if (!(n %in% names(fiche))) {
    champ <- c(champ, n); lu <- c(lu, "champ absent")
  }
  # `tables_reference` (ADR 0015, point 4.1) : contrôlé seulement si
  # version_fiche est bien formée et admise ; sinon le contrôle 3 refuse la
  # version (une fiche 4, sans ce champ, est refusée version_fiche_non_admise).
  if (forme_exacte(v, "integer") && length(v) == 1L && !is.na(v) && v %in% VERSIONS_FICHE_REJEU) {
    ft <- fiche[["tables_reference"]]
    if (!("tables_reference" %in% names(fiche))) {
      champ <- c(champ, "tables_reference"); lu <- c(lu, "champ absent")
    } else if (!is.null(ft)) {
      if (!liste_nommee(ft) || !length(ft) || length(noms_bytes(ft)) || !all(validUTF8(names(ft)))) {
        champ <- c(champ, "tables_reference"); lu <- c(lu, decrire_forme(fiche, "tables_reference"))
      } else if (!identical(enc2utf8(names(ft)), sort(enc2utf8(names(ft)), method = "radix"))) {
        # Clés dans l'ordre de la fiche écrite par le moteur (tri par octets,
        # ADR 0015, point 3.1 ; audit de #73, m1).
        champ <- c(champ, "tables_reference")
        lu <- c(lu, formuler("clés %s hors de l'ordre par octets %s", liste_guillemets(names(ft)),
                             liste_guillemets(sort(enc2utf8(names(ft)), method = "radix"))))
      } else for (k in names(ft)) {
        el <- ft[[k]]
        if (!forme_exacte(el, "list", "names") || !identical(attr(el, "names", exact = TRUE), CHAMPS_TABLE_FICHE)) {
          champ <- c(champ, "tables_reference")
          lu <- c(lu, formuler("élément « %s » : %s, champs %s", sans_controle(k), decrire_forme(ft, k),
                               if (is.null(attr(el, "names", exact = TRUE))) formuler("sans noms")
                               else liste_guillemets(attr(el, "names", exact = TRUE))))
          break
        }
        mal <- c(nom = !chaine_unique(el[["nom"]]),
                 statut = !chaine_unique(el[["statut"]]) || !(el[["statut"]] %in% STATUTS_TABLE),
                 representation = !chaine_unique(el[["representation"]]),
                 empreinte = !chaine_unique(el[["empreinte"]]))
        if (any(mal)) {
          m <- names(mal)[mal][1L]
          champ <- c(champ, "tables_reference")
          lu <- c(lu, formuler("élément « %s », champ « %s » : %s", sans_controle(k), sans_controle(m),
                               decrire_forme(el, m)))
          break
        }
      }
    }
  }
  if (length(champ))
    refus_rejeu("fiche_non_conforme",
                formuler(paste("Fiche non conforme au rejeu : %s. Le rejeu ne lit que ces champs avant",
                                 "construire_table() ; aucun n'est complété ni converti."),
                         paste(formuler("« %s » : %s au lieu de %s", champ, lu, attendus[champ]), collapse = " ; ")),
                list(champ = champ, lu = lu, attendu = unname(attendus[champ])))

  # 3. version_fiche admise : la version donne le sens des autres champs.
  if (!(v %in% VERSIONS_FICHE_REJEU)) {
    # Version de tablexp de la fiche (décision Q-E d'actuary, reprise de
    # #86) : « absente » si versions manque ou est une liste sans tablexp ;
    # versions[["tablexp"]] d'une liste qui l'a ; versions entier sinon.
    versions <- fiche[["versions"]]
    lus <- list(tablexp = texte_lu(if (is.list(versions)) lire_champ(versions, "tablexp") else versions),
                code = texte_lu(e[["code"]]),
                code_installe = texte_lu(EMPREINTE_CODE))
    refus_rejeu("version_fiche_non_admise",
                formuler(paste("Version de fiche non admise au rejeu : version_fiche %d lue, version(s) admise(s)",
                                 "par ce code : %s. Fiche produite par tablexp %s, empreinte du code %s ; code installé :",
                                 "tablexp %s, empreinte du code %s. Aucune conversion de fiche n'est faite : rejouer",
                                 "cette fiche avec le code dont l'empreinte est celle de la fiche."),
                         v, paste(sprintf("%d", VERSIONS_FICHE_REJEU), collapse = ", "),
                         sans_controle(lus$tablexp), sans_controle(lus$code),
                         as.character(utils::packageVersion("tablexp")), sans_controle(lus$code_installe)),
                c(list(lu = v, admises = VERSIONS_FICHE_REJEU), lus))
  }

  # 4. et 5. Algorithme et représentation de l'empreinte des données.
  if (!identical(e[["algorithme"]], ALGORITHME_EMPREINTE))
    refus_rejeu("algorithme_empreinte_inconnu",
                formuler(paste("Algorithme d'empreinte « %s » lu dans la fiche, « %s » attendu : ce code ne calcule",
                                 "l'empreinte des données que par cet algorithme ; aucune comparaison n'est possible."),
                         sans_controle(e[["algorithme"]]), ALGORITHME_EMPREINTE),
                list(lu = e[["algorithme"]], attendu = ALGORITHME_EMPREINTE))
  if (!identical(e[["representation"]], ENTETE_CANONIQUE))
    refus_rejeu("representation_non_admise",
                formuler(paste("Représentation canonique « %s » lue dans la fiche, « %s » attendue : ce code calcule",
                                 "l'empreinte des données sous cette seule définition, et une empreinte d'une autre",
                                 "définition ne lui est jamais égale ; aucune conversion n'est faite."),
                         sans_controle(e[["representation"]]), ENTETE_CANONIQUE),
                list(lu = e[["representation"]], attendu = ENTETE_CANONIQUE))

  # 6. Entrées de construire_table() : première couche de ses contrôles
  #    (controler_plan_roles(), partagée, #82) ; les refus gardent leur
  #    module et leur message. Requise avant l'empreinte des données (7) et
  #    les tables de la fiche (7 bis).
  roles <- fiche[["roles"]]
  entrees <- controler_plan_roles(donnees, roles, fiche[["plan"]], fiche[["provenance"]])
  plan <- entrees$plan
  recette <- entrees$recette

  # 7. Empreinte des données reçues contre celle de la fiche.
  empreinte <- empreinte_donnees(donnees, roles)
  if (!identical(empreinte, e[["donnees"]]))
    refus_rejeu("empreinte_donnees_discordante",
                formuler(paste("Empreinte des données discordante : %s lue dans la fiche, %s recalculée sur les",
                                 "données reçues (%d lignes). L'empreinte porte sur la représentation « %s » des",
                                 "colonnes des rôles de la fiche (%s), lignes dans l'ordre reçu : un autre ordre des",
                                 "lignes, une valeur ou un type de colonne différents suffisent à la changer ; les",
                                 "colonnes hors rôles n'y entrent pas."),
                         sans_controle(e[["donnees"]]), empreinte, nrow(donnees), ENTETE_CANONIQUE,
                         liste_guillemets(unname(roles))),
                list(lu = e[["donnees"]], attendu = empreinte, lignes = nrow(donnees), colonnes = unname(roles)))

  # 7 bis. Tables de référence (ADR 0015, point 4.2) : reçues ou relues dans
  #    le package, empreinte de chacune contre la fiche. Le contrôle 1.3 sur
  #    la liste assemblée et les contrôles d'entrée des étapes qui lisent une
  #    table (positionnement, validation ; ancien 7 ter) sont faits une fois,
  #    par construire_table() (8), avant toute étape (#82 ; #164, point 2).
  tr <- controler_tables_rejeu(fiche[["tables_reference"]], tables_reference, texte_lu(e[["code"]]))

  # 8. Construction, une seule fois, sur les rôles, le plan et la provenance
  #    de la fiche, et les tables assemblées.
  x <- construire_table(donnees, roles, fiche[["plan"]], fiche[["provenance"]], tr$tables)
  r <- x$fiche
  if (!identical(r$empreintes$donnees, empreinte))
    stop("rejouer_fiche() : empreinte des données de la construction différente de celle du rejeu (erreur de programmation).",
         call. = FALSE)
  # Comparaison par clé (audit de #73, m1) : la fiche rendue range les clés
  # triées, tr$empreintes suit la fiche reçue.
  emp_rendues <- vapply(r$tables_reference, function(t) t$empreinte, "")
  if (!setequal(names(emp_rendues), names(tr$empreintes)) ||
      !identical(unname(emp_rendues[names(tr$empreintes)]), unname(tr$empreintes)))
    stop("rejouer_fiche() : empreintes des tables de la construction différentes de celles du rejeu (erreur de programmation).",
         call. = FALSE)

  # Bilan : 8 contrôles (passés), puis 6 témoins. texte_moteur() s'applique
  # aussi au texte composé (lignes 1 et 6, verdicts « différente : … ») :
  # sprintf() d'un gabarit marqué UTF-8 avec un argument non ASCII non marqué
  # écrit « <c3><a9> » sous la locale C (mesuré, #86). Les comparaisons sont
  # faites sur les valeurs brutes, avant tout texte_moteur() ; les valeurs
  # lues viennent de lire_fiche() ou de x$fiche, au codage déjà déterminé ;
  # l'effet est mesuré identique sous C et C.UTF-8 (tests/controles/
  # locale_c.R, bilan de R1) ; l'écart ne s'étend à aucune comparaison ni à
  # aucune écriture de valeur lue.
  conforme <- texte_moteur("conforme")
  forme <- texte_moteur("contrôle de forme")
  exacte <- texte_moteur("comparaison exacte")
  securite <- texte_moteur("exacte ; sans rôle de sécurité")
  bilan <- data.frame(
    controle = c("nature_arguments", "forme_fiche", "version_fiche", "algorithme_empreinte", "representation",
                 "entrees_construction", "empreinte_donnees", "tables_reference", "temoin_code", "temoin_versions",
                 "temoin_plateforme", "temoin_fiche", "temoin_empreinte_resultat", "temoin_empreinte_fichier"),
    attendu = c(texte_moteur("fiche : liste nommée ; donnees : data.frame"),
                texte_moteur(paste("version_fiche : integer de longueur 1, non NA, sans attribut ;",
                                   "empreintes/algorithme, empreintes/representation, empreintes/donnees :",
                                   "chaînes uniques ; roles, plan, provenance : présents ; tables_reference : NULL ou",
                                   "liste nommée par clé (sept champs)")),
                paste(sprintf("%d", VERSIONS_FICHE_REJEU), collapse = ", "), ALGORITHME_EMPREINTE, ENTETE_CANONIQUE,
                texte_moteur(paste("valider_plan(), controler_roles(), recette et convention, controler_provenance() :",
                                   "aucun refus")),
                empreinte, tr$attendu, texte_lu(EMPREINTE_CODE), paires_lues(r$versions), paires_lues(r$plateforme),
                texte_moteur(paste("fiche rendue par construire_table(), hors versions, plateforme,",
                                   "empreintes/resultat, empreintes/code, empreintes/statut_resultat_code")),
                texte_lu(r$empreintes$resultat),
                texte_moteur("non recalculée par le moteur (couche de lecture, E4.4)")),
    lu = c(texte_moteur(sprintf("fiche : liste nommée de %d champs ; donnees : classe %s, %d lignes, %d colonnes",
                                length(fiche), paste(class(donnees), collapse = "/"), nrow(donnees), ncol(donnees))),
           texte_moteur("champs lus de la forme attendue"),
           sprintf("%d", v), e[["algorithme"]], e[["representation"]],
           texte_moteur(sprintf("étapes : %s ; rôles : %s ; recette : %s ; provenance : %s",
                                paste(names(plan[["etapes"]]), collapse = ", "), paste(names(roles), collapse = ", "),
                                recette$recette,
                                if (is.null(fiche[["provenance"]])) "NULL" else fiche[["provenance"]][["fichier"]])),
           e[["donnees"]], tr$lu, texte_lu(e[["code"]]), paires_lues(fiche[["versions"]]),
           paires_lues(fiche[["plateforme"]]), texte_moteur("fiche reçue, même périmètre"),
           texte_lu(e[["resultat"]]), texte_lu(e[["fichier"]])),
    verdict = c(rep(conforme, 8L),
                verdict_chaines(EMPREINTE_CODE, e[["code"]]),
                verdict_liste(r$versions, fiche[["versions"]], "versions"),
                verdict_liste(r$plateforme, fiche[["plateforme"]], "plateforme"),
                verdict_chemins(perimetre_temoin_fiche(r), perimetre_temoin_fiche(fiche), "fiche"),
                verdict_chaines(r$empreintes$resultat, e[["resultat"]]),
                texte_moteur("non vérifiée")),
    statut = c(forme, forme, exacte, exacte, exacte, texte_moteur("contrôles délégués aux modules"), securite,
               securite, securite, exacte, exacte, exacte, texte_moteur("témoin, même machine seulement"),
               texte_moteur("reprise de la fiche, non vérifiée par le moteur")),
    stringsAsFactors = FALSE)
  structure(list(construction = x, bilan = bilan), class = "rejeu")
}
