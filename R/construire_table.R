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
#    `construction` et la fiche (module `rapport`) ;
#  - rejouer_fiche() : rejeu outillé d'une fiche (E3.7 ; ADR 0012, point 3 ;
#    #86) : contrôles de la fiche et des données, puis un seul appel de
#    construire_table() sur les rôles, le plan et la provenance de la fiche ;
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
    stringsAsFactors = FALSE)
)

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

# Valide le plan entier, sans rien exécuter (ADR 0007, point 5.4). Rend le
# plan, inchangé et invisible ; refuse (module construire_table pour les
# codes structurels ; module donnees, exposition ou brut pour les valeurs).
valider_plan <- function(plan) {
  refus_plan <- function(motif, message, details = list())
    refuser("construire_table", motif, message, details)
  if (!liste_nommee(plan) || !length(plan))
    refus_plan("plan_mal_forme",
               paste("Le plan doit être une liste nommée non vide, aux noms uniques, sans autre attribut",
                     "que ses noms (ADR 0007, point 5.1)."))
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
  if (!liste_noms_complets(etapes))
    refus_plan("etapes_mal_formees",
               paste("Le champ « etapes » doit être une liste nommée (nom d'étape -> liste de ses paramètres),",
                     "sans autre attribut que ses noms."))
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
  # absents sont cités. `etape` et `prealable` sont les vecteurs aplatis ;
  # `prealables_par_etape` dit quel préalable manque à quelle étape (#27).
  absents <- lapply(ETAPES$prealable[rang], function(p) setdiff(p, noms))
  names(absents) <- noms
  sans_prealable <- lengths(absents) > 0L
  if (any(sans_prealable)) {
    manquants <- unique(unlist(absents))
    refus_plan("etape_prealable_manquante",
               sprintf("Étape(s) %s sans %s %s.", liste_guillemets(noms[sans_prealable]),
                       if (length(manquants) == 1L) "son étape préalable" else "ses étapes préalables",
                       liste_guillemets(manquants)),
               list(etape = noms[sans_prealable], prealable = manquants,
                    prealables_par_etape = absents[sans_prealable]))
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
                 sprintf("Étape « %s » : ses paramètres doivent être une liste nommée, sans autre attribut que ses noms.",
                         nom),
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

# Vecteurs d'ordre des trois champs du plan dont les valeurs forment un
# ensemble (ADR 0011, point 5.1 (h), quatrième annotation du 20/10/2026 ; #63),
# liste nommée par le chemin du champ dans le plan. Une fonction, non une
# constante de premier niveau : les fichiers de R/ sont évalués par ordre
# alphabétique à l'installation, `construire_table.R` avant `donnees.R`,
# dont les constantes (ROLES_SEGMENTATION, MOTIFS_*) ne sont pas encore
# définies ; `brut.R` (ESTIMATEURS_BRUT) l'est déjà, mais la fonction les
# lit toutes trois au même endroit.
ordres_canoniques <- function()
  list(segmentation = ROLES_SEGMENTATION,
       "etapes/donnees/exclusions" = c(MOTIFS_LIGNES, MOTIFS_TETE),
       "etapes/brut/estimateurs" = ESTIMATEURS_BRUT)

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
  # convention, provenance. Chaque contrôle s'exécute ici une seule fois : les
  # fonctions d'étape, appelées avec controler = FALSE, n'en refont aucun (#22).
  plan <- canoniser_plan(valider_plan(plan), CHAMPS_PLAN, PARAMETRES_ETAPES, ordres_canoniques())
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
                                 exclusions = pe[["exclusions"]], controler = FALSE),
      exposition = calculer_exposition(etapes[["donnees"]][["resultat"]][["observations"]], roles,
                                       transition = plan[["transition"]], periode = plan[["periode"]],
                                       delai_declaration = plan[["delai_declaration"]],
                                       segmentation = plan[["segmentation"]],
                                       convention = pe[["convention"]], par_annee = pe[["par_annee"]],
                                       controler = FALSE),
      brut = calculer_taux_bruts(etapes[["exposition"]][["resultat"]][["exposition"]],
                                 segmentation = plan[["segmentation"]],
                                 par_annee = etapes[["exposition"]][["parametres"]][["par_annee"]],
                                 convention = etapes[["exposition"]][["parametres"]][["convention"]][["retenue"]],
                                 unite_exposition = etapes[["exposition"]][["parametres"]][["unite_exposition"]],
                                 estimateurs = pe[["estimateurs"]], niveau = pe[["niveau"]],
                                 observations = fenetres,
                                 transition = plan[["transition"]], periode = plan[["periode"]],
                                 delai_declaration = plan[["delai_declaration"]],
                                 parametres_exposition = etapes[["exposition"]][["parametres"]],
                                 controler = FALSE),
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
         fiche = assembler_fiche(roles, plan, recette, etapes, avertissements, empreintes, provenance)),
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
  if (is.character(v) && length(v) == 1L && is.null(attributes(v))) return(if (is.na(v)) "NA" else v)
  sprintf("illisible (%s de longueur %d)", typeof(v), length(v))
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
# (nom vide ou NA ; décision Q-A d'actuary, reprise de #86).
decrire_forme <- function(conteneur, nom) {
  if (!is.list(conteneur) || !(nom %in% names(conteneur))) return("champ absent")
  v <- conteneur[[nom]]
  s <- sprintf("%s de longueur %d", typeof(v), length(v))
  if (is.atomic(v) && length(v) == 1L && is.na(v)) s <- paste0(s, ", NA")
  else if (is.character(v) && length(v) == 1L && !nzchar(v)) s <- paste0(s, ", vide")
  a <- names(attributes(v))
  if (is.list(v)) a <- setdiff(a, "names")
  if (length(a)) s <- paste0(s, ", attribut(s) ", paste(a, collapse = ", "))
  n <- names(v)
  if (!is.null(n)) {
    complets <- n[!is.na(n) & nzchar(n)]
    doubles <- unique(complets[duplicated(complets)])
    if (length(doubles)) s <- paste0(s, ", noms en double : ", sans_controle(paste(doubles, collapse = ", ")))
    if (anyNA(n) || !all(nzchar(n[!is.na(n)]))) s <- paste0(s, ", noms incomplets")
  }
  s
}

# Paires « nom=valeur » d'une liste nommée de la fiche (versions,
# plateforme), jointes par « ; » ; texte_lu() d'une autre valeur.
paires_lues <- function(l) {
  if (!is.list(l) || is.null(names(l))) return(texte_lu(l))
  paste(sprintf("%s=%s", names(l), vapply(l, texte_lu, "")), collapse = " ; ")
}

# Chemins des feuilles qui diffèrent entre la fiche rendue (`rendu`) et la
# fiche reçue (`recu`) (descente D de la spécification de #86) : deux listes
# nommées sans classe, aux noms complets et uniques, sont parcourues sur
# l'union de leurs noms (ordre de la fiche rendue, puis noms propres à la fiche
# reçue) ; toute autre valeur est une feuille. Un nœud qui diffère sans
# qu'aucun enfant ne diffère est cité « (ordre des champs ou attributs) » ;
# `racine` nomme le nœud de départ. Rend character(0) si identical().
champs_differents <- function(rendu, recu, racine, chemin = character(0)) {
  if (identical(rendu, recu)) return(character(0))
  ici <- if (length(chemin)) paste(chemin, collapse = "/") else racine
  descend <- function(v) is.list(v) && !is.object(v) && noms_complets(names(v)) && !anyDuplicated(names(v))
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
# liste sans classe aux noms complets, sinon verdict_chemins().
verdict_liste <- function(rendu, recu, racine) {
  if (!is.list(recu) || is.object(recu) || !noms_complets(names(recu)))
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
#             dans x$fiche ; rôles, plan et provenance en sont lus.
# Contrôle, dans l'ordre, la nature des arguments, la forme des champs lus,
# version_fiche (VERSIONS_FICHE_REJEU), l'algorithme et la représentation de
# l'empreinte des données, les entrées de construire_table() (mêmes contrôles,
# mêmes refus), puis l'empreinte des données ; s'arrête au premier refus, sans
# appeler construire_table(). Appelle ensuite construire_table() une seule
# fois. Rend un objet de classe `rejeu` : construction, l'objet rendu par
# construire_table() sans retouche ; bilan, data.frame de 13 lignes
# (controle, attendu, lu, verdict, statut) : 7 contrôles, tous « conforme »
# dans un bilan rendu, puis 6 témoins, jamais motifs de refus.
rejouer_fiche <- function(donnees, fiche) {
  # Message marqué UTF-8 sous toute locale (#94) ; les gabarits sont marqués
  # par texte_moteur() avant sprintf() : sous la locale C, un gabarit non
  # marqué (paquet installé sous C) mêlé à une citation marquée UTF-8 verrait
  # ses octets non ASCII échappés (« <c2><ab> »).
  refus_rejeu <- function(motif, message, details = list())
    refuser("construire_table", motif, texte_moteur(message), details)
  gabarit <- function(...) texte_moteur(paste(...))

  # 1. Nature des arguments : la fiche d'abord, puis les données (refus
  #    délégué à controler_roles(), motif donnees_mal_formees). Classe lue
  #    par %in% (match()), qui ne lève pas sur une classe d'encodage « bytes »,
  #    là où inherits() lève (« translating strings with "bytes" encoding is
  #    not allowed », mesuré sous C et C.UTF-8 ; #94).
  if ("construction" %in% class(fiche))
    refus_rejeu("fiche_non_conforme",
                paste("Rejeu : objet `construction` reçu à la place de sa fiche ; passer x$fiche, ou la fiche",
                      "relue de son texte par lire_fiche()."),
                list(champ = "(fiche)", lu = "construction", attendu = "liste nommée"))
  if ("rejeu" %in% class(fiche))
    refus_rejeu("fiche_non_conforme",
                "Rejeu : objet `rejeu` reçu à la place d'une fiche ; passer r$construction$fiche.",
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
                sprintf(gabarit("Rejeu : la fiche porte un nom de champ d'encodage « bytes » (champ n° %d : %s),",
                                "non convertible en UTF-8 sans deviner ; aucun champ du premier niveau n'est lu par nom. Une fiche",
                                "produite par le moteur ou relue par lire_fiche() n'en porte pas."),
                        b[1L], neutraliser_citation(nf[b[1L]])),
                list(champ = "(fiche)", lu = nf[b], attendu = "noms de champ sans encodage bytes"))
  if (!liste_nommee(fiche))
    refus_rejeu("fiche_non_conforme",
                sprintf(gabarit("Rejeu : la fiche doit être une liste nommée aux noms uniques, sans autre attribut",
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
                             roles = "champ présent", plan = "champ présent", provenance = "champ présent (NULL admis)"))
  champ <- lu <- character(0)
  v <- fiche[["version_fiche"]]
  if (!(forme_exacte(v, "integer") && length(v) == 1L && !is.na(v))) {
    champ <- c(champ, "version_fiche"); lu <- c(lu, decrire_forme(fiche, "version_fiche"))
  }
  e <- fiche[["empreintes"]]
  if (!liste_nommee(e)) {
    champ <- c(champ, "empreintes"); lu <- c(lu, decrire_forme(fiche, "empreintes"))
  } else for (n in c("algorithme", "representation", "donnees")) if (!chaine_unique(e[[n]])) {
    champ <- c(champ, paste0("empreintes/", n)); lu <- c(lu, decrire_forme(e, n))
  }
  for (n in c("roles", "plan", "provenance")) if (!(n %in% names(fiche))) {
    champ <- c(champ, n); lu <- c(lu, "champ absent")
  }
  if (length(champ))
    refus_rejeu("fiche_non_conforme",
                sprintf(gabarit("Fiche non conforme au rejeu : %s. Le rejeu ne lit que ces champs avant",
                                "construire_table() ; aucun n'est complété ni converti."),
                        paste(sprintf(gabarit("« %s » : %s au lieu de %s"), champ, lu, attendus[champ]), collapse = " ; ")),
                list(champ = champ, lu = lu, attendu = unname(attendus[champ])))

  # 3. version_fiche admise : la version donne le sens des autres champs.
  if (!(v %in% VERSIONS_FICHE_REJEU)) {
    # Version de tablexp de la fiche (décision Q-E d'actuary, reprise de
    # #86) : « absente » si versions manque ou est une liste sans tablexp ;
    # versions[["tablexp"]] d'une liste qui l'a ; versions entier sinon.
    versions <- fiche[["versions"]]
    lus <- list(tablexp = texte_lu(if (is.list(versions)) { if ("tablexp" %in% names(versions)) versions[["tablexp"]] }
                                   else versions),
                code = texte_lu(e[["code"]]),
                code_installe = texte_lu(EMPREINTE_CODE))
    refus_rejeu("version_fiche_non_admise",
                sprintf(gabarit("Version de fiche non admise au rejeu : version_fiche %d lue, version(s) admise(s)",
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
                sprintf(gabarit("Algorithme d'empreinte « %s » lu dans la fiche, « %s » attendu : ce code ne calcule",
                                "l'empreinte des données que par cet algorithme ; aucune comparaison n'est possible."),
                        sans_controle(e[["algorithme"]]), ALGORITHME_EMPREINTE),
                list(lu = e[["algorithme"]], attendu = ALGORITHME_EMPREINTE))
  if (!identical(e[["representation"]], ENTETE_CANONIQUE))
    refus_rejeu("representation_non_admise",
                sprintf(gabarit("Représentation canonique « %s » lue dans la fiche, « %s » attendue : ce code calcule",
                                "l'empreinte des données sous cette seule définition, et une empreinte d'une autre",
                                "définition ne lui est jamais égale ; aucune conversion n'est faite."),
                        sans_controle(e[["representation"]]), ENTETE_CANONIQUE),
                list(lu = e[["representation"]], attendu = ENTETE_CANONIQUE))

  # 6. Entrées de construire_table() : la séquence de contrôles de
  #    construire_table(), dupliquée ici pour ne pas la modifier ; les refus
  #    gardent leur module et leur message.
  roles <- fiche[["roles"]]
  plan <- canoniser_plan(valider_plan(fiche[["plan"]]), CHAMPS_PLAN, PARAMETRES_ETAPES, ordres_canoniques())
  controler_roles(donnees, roles, plan[["segmentation"]])
  recette <- deduire_recette(roles)
  if ("exposition" %in% names(plan[["etapes"]]))
    resoudre_convention(plan[["etapes"]][["exposition"]][["convention"]], recette$convention_native)
  controler_provenance(fiche[["provenance"]])

  # 7. Empreinte des données reçues contre celle de la fiche.
  empreinte <- empreinte_donnees(donnees, roles)
  if (!identical(empreinte, e[["donnees"]]))
    refus_rejeu("empreinte_donnees_discordante",
                sprintf(gabarit("Empreinte des données discordante : %s lue dans la fiche, %s recalculée sur les",
                                "données reçues (%d lignes). L'empreinte porte sur la représentation « %s » des",
                                "colonnes des rôles de la fiche (%s), lignes dans l'ordre reçu : un autre ordre des",
                                "lignes, une valeur ou un type de colonne différents suffisent à la changer ; les",
                                "colonnes hors rôles n'y entrent pas."),
                        sans_controle(e[["donnees"]]), empreinte, nrow(donnees), ENTETE_CANONIQUE,
                        sans_controle(liste_guillemets(unname(roles)))),
                list(lu = e[["donnees"]], attendu = empreinte, lignes = nrow(donnees), colonnes = unname(roles)))

  # 8. Construction, une seule fois, sur les rôles, le plan et la provenance
  #    de la fiche.
  x <- construire_table(donnees, roles, fiche[["plan"]], fiche[["provenance"]])
  r <- x$fiche
  if (!identical(r$empreintes$donnees, empreinte))
    stop("rejouer_fiche() : empreinte des données de la construction différente de celle du rejeu (erreur de programmation).",
         call. = FALSE)

  # Bilan : 7 contrôles (passés), puis 6 témoins. texte_moteur() s'applique
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
                 "entrees_construction", "empreinte_donnees", "temoin_code", "temoin_versions",
                 "temoin_plateforme", "temoin_fiche", "temoin_empreinte_resultat", "temoin_empreinte_fichier"),
    attendu = c(texte_moteur("fiche : liste nommée ; donnees : data.frame"),
                texte_moteur(paste("version_fiche : integer de longueur 1, non NA, sans attribut ;",
                                   "empreintes/algorithme, empreintes/representation, empreintes/donnees :",
                                   "chaînes uniques ; roles, plan, provenance : présents")),
                paste(sprintf("%d", VERSIONS_FICHE_REJEU), collapse = ", "), ALGORITHME_EMPREINTE, ENTETE_CANONIQUE,
                texte_moteur(paste("valider_plan(), controler_roles(), recette et convention, controler_provenance() :",
                                   "aucun refus")),
                empreinte, texte_lu(EMPREINTE_CODE), paires_lues(r$versions), paires_lues(r$plateforme),
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
           e[["donnees"]], texte_lu(e[["code"]]), paires_lues(fiche[["versions"]]),
           paires_lues(fiche[["plateforme"]]), texte_moteur("fiche reçue, même périmètre"),
           texte_lu(e[["resultat"]]), texte_lu(e[["fichier"]])),
    verdict = c(rep(conforme, 7L),
                verdict_chaines(EMPREINTE_CODE, e[["code"]]),
                verdict_liste(r$versions, fiche[["versions"]], "versions"),
                verdict_liste(r$plateforme, fiche[["plateforme"]], "plateforme"),
                verdict_chemins(perimetre_temoin_fiche(r), perimetre_temoin_fiche(fiche), "fiche"),
                verdict_chaines(r$empreintes$resultat, e[["resultat"]]),
                texte_moteur("non vérifiée")),
    statut = c(forme, forme, exacte, exacte, exacte, texte_moteur("contrôles délégués aux modules"), securite,
               securite, exacte, exacte, exacte, texte_moteur("témoin, même machine seulement"),
               texte_moteur("reprise de la fiche, non vérifiée par le moteur")),
    stringsAsFactors = FALSE)
  structure(list(construction = x, bilan = bilan), class = "rejeu")
}
