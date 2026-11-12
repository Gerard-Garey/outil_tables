# -----------------------------------------------------------------------------
#  tests/unitaires/test_validation_lissee.R : relation `lissee` de l'étape
#  `validation`, validation de la table lissée de la construction (issue
#  #224, commit (2b) de B19 ; corps de #224, option L-b, décisions D1 à D13
#  du mainteneur du 11/11/2026, M413 ; spécification de forme (0')(b)
#  d'actuary-approfondi, commentaire 6799594124 ; ADR 0017, points 1.5, 3.4,
#  5, 10.2, 12 et 15)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : constantes, catalogue, motifs, modules,
#    messages et details des refus, états, structure des paramètres, mentions
#    du rapport ;
#  - identité mathématique : ddl = n - somme des nu_s quand toute cellule
#    pondérée par le lissage est retenue, n - (somme des nu_s - h_ii) quand
#    la cellule (H, 50) pondérée n'est pas retenue (E^c = 0 ; ADR 0017,
#    point 5.5, M417) (critère C1, 1e-12 en relatif), SMR sous pondération exposition (identité de conservation
#    somme E^i g = somme D, critère C1) ;
#  - identité exacte : attendus A = E^c (-ln(1 - g)) recalculés sur la table
#    lissée, p asymptotiques par pchisq(), ddl effectifs recopiés de l'étape
#    lissage, appel sous independante indépendant des arguments de la
#    relation lissee.
#  Jeux : agrégats écrits à la main (aucune graine) ; jeu en lignes simulé
#  pour importance_exclusions (graine 22011204, plage de la porteuse B19,
#  k = 0, M416). Graine du plan 22011205 (étape aléatoire, aucun tirage sous
#  lissee). Le critère C1 sous (b) (SMR = 1 à 1e-10) est ajouté avec #225.
# -----------------------------------------------------------------------------

D_ <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
erreur_programmation <- function(expr) {
  e <- tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e)
  !is.null(e) && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
}
# Refus attendu : module, motif, message (NULL : non comparé), details
# (NULL : non comparés) ; TRUE ou le texte de l'écart.
controle <- function(r, module, motif, message = NULL, details = NULL) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s »", r$module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  if (!is.null(message) && !identical(conditionMessage(r), message)) return(sprintf("message « %s »", conditionMessage(r)))
  if (!is.null(details) && !identical(r$details, details)) return("details différents")
  TRUE
}
tous <- function(l) { k <- vapply(l, isTRUE, NA); if (all(k)) TRUE else paste(names(l)[!k], unlist(l[!k]), collapse = " ; ") }
valeur <- function(s, cle) s$valeur[match(cle, paste(s$statistique, s$grandeur, sep = "/"))]
etat_de <- function(s, cle) s$etat[match(cle, paste(s$statistique, s$grandeur, sep = "/"))]

# --- jeu agrégats écrit à la main et plans ----------------------------------------

AGES <- 40:60
EH <- round(1000 * exp(-0.01 * (AGES - 40)), 3)
DH <- round(EH * 0.002 * exp(0.08 * (AGES - 40)))
# Exposition centrale voisine de E^i - D / 2 (forme usuelle), décès des
# femmes à 60 % de ceux des hommes.
JEU <- local({
  dh <- c(DH, round(DH * 0.6)); ei <- c(EH, EH * 1.1)
  data.frame(sexe = rep(c("H", "F"), each = length(AGES)), age = as.double(c(AGES, AGES)), evenements = as.double(dh),
             exposition_initiale = ei, exposition_centrale = ei - dh / 2, stringsAsFactors = FALSE)
})
ROLES <- c(sexe = "sexe", age = "age", evenements = "evenements", exposition_initiale = "exposition_initiale",
           exposition_centrale = "exposition_centrale")
LISSAGE <- list(methode = "wh_quadratique", plage_ages = c(40, 60), ordre_difference = 2, critere_lissage = "declare",
                ponderation = "exposition", parametre_lissage = 100)
VAL <- list(relation_table = "lissee", plage_ages = c(40, 60), tranches_age = c(40, 50), niveau = 0.95,
            replications = 99)
plan_l <- function(lissage = LISSAGE, val = VAL, positionnement = NULL) {
  etapes <- list(donnees = list(exclusions = character(0)),
                 exposition = list(convention = "dernier_anniversaire", par_annee = FALSE),
                 brut = list(estimateurs = "binomial", niveau = 0.95))
  if (!is.null(lissage)) etapes$lissage <- lissage
  if (!is.null(positionnement)) etapes$positionnement <- positionnement
  etapes$validation <- val
  list(transition = list(depart = "vivant", arrivee = "deces"),
       periode = list(debut = D_("2015-01-01"), fin = D_("2024-12-31")), delai_declaration = 0,
       segmentation = "sexe", graine = 22011205L, etapes = etapes)
}
avec <- function(...) { l <- VAL; v <- list(...); for (n in names(v)) l[n] <- list(v[[n]]); l }
rv <- function(...) refus_de(valider_plan(plan_l(...)))
X <- construire_table(JEU, ROLES, plan_l())
V <- X$etapes$validation
L <- X$etapes$lissage
ST <- V$resultat$statistiques
CV <- V$resultat$cellules

# --- A. Constantes, statuts et catalogue (valeur calculée à la main) -----------------

verifier("A RELATIONS_TABLE : lissee = NA_integer_, après les trois relations existantes, vecteur integer (ADR 0017, point 5.1)",
         identical(tablexp:::RELATIONS_TABLE,
                   c(independante = 0L, ajustee_hors_outil = NA_integer_, positionnee = 1L, lissee = NA_integer_)))
verifier("A états nouveaux à la fin : contraint_par_lissage puis non_etabli_sous_lissage (ETATS_GRANDEUR_VALIDATION, ETATS_SOUS_GROUPES_VALIDATION)", {
  g <- tablexp:::ETATS_GRANDEUR_VALIDATION; s <- tablexp:::ETATS_SOUS_GROUPES_VALIDATION
  identical(utils::tail(g, 3L), c("contraint_par_positionnement", "contraint_par_lissage", "non_etabli_sous_lissage")) &&
    identical(utils::tail(s, 3L), c("convolution_au_dela_n_max", "contraint_par_lissage", "non_etabli_sous_lissage"))
})
verifier("A STATUTS_VALIDATION_LISSEE : mêmes (statistique, grandeur, sous_h0) que STATUTS_VALIDATION ; textes de la spécification (ddl n - somme des leviers retenus, M417 ; p asymptotiques, Monte-Carlo, SMR, complément des tests, sous-groupes S5) ; statuts_validation(\"lissee\")", {
  a <- tablexp:::STATUTS_VALIDATION; b <- tablexp:::STATUTS_VALIDATION_LISSEE
  k <- function(s, g) b$statut[b$statistique == s & b$grandeur == g]
  cpl <- paste(" ; table lissée sur ces données : résidus contraints par le lissage (somme pondérée des résidus voisine",
               "de 0, corrélation locale négative), niveau non établi")
  ne <- paste("non produit : A_c dépend des décès observés par le lissage, aucune loi de D_c sous une hypothèse nulle",
              "n'est établie ; état non_etabli_sous_lissage")
  tous(list(
    lignes = identical(a[c("statistique", "grandeur", "sous_h0")], b[c("statistique", "grandeur", "sous_h0")]),
    statuts = identical(tablexp:::statuts_validation("lissee"), b),
    ddl = all(vapply(c("khi2_pearson", "deviance"), function(s) startsWith(k(s, "ddl"), paste(
      "n - somme sur les cellules retenues des éléments diagonaux de la matrice de lissage (leviers, colonne levier de",
      "la table lissée ; = n - somme des nu_s quand toute cellule pondérée par le lissage est retenue) : n cellules")),
      NA)),
    p_asym = identical(k("khi2_pearson", "p_asymptotique"), k("deviance", "p_asymptotique")) &&
      startsWith(k("deviance", "p_asymptotique"), paste("approximation non établie : loi du khi-deux à n - somme des",
                                                        "leviers des cellules retenues degrés de liberté")),
    mc = all(grepl("aucune réplication sous la relation lissee", b$statut[b$grandeur %in% c("p_monte_carlo",
                                                                                            "erreur_monte_carlo")],
                   fixed = TRUE)) && sum(b$grandeur %in% c("p_monte_carlo", "erreur_monte_carlo")) == 10L,
    ne = identical(c(k("residus", "esperance_au_dela_2"), k("residus", "esperance_au_dela_3"),
                     k("khi2_pearson", "variance_h0"), k("signes", "esperance_positifs_h0"),
                     b$statut[b$statistique == "fluctuation"]), rep(ne, 8L)),
    smr = identical(k("smr", "p_centrale"), "non produit : table lissée ajustée sur ces décès ; état contraint_par_lissage") &&
      startsWith(k("smr", "smr"), "descriptif : O / A ; égal à 1 par construction sous wh_poisson"),
    cpl = all(vapply(list(c("signes", "p_exacte"), c("runs", "esperance"), c("wilcoxon", "p_asymptotique")), function(x)
      identical(k(x[1L], x[2L]), paste0(a$statut[a$statistique == x[1L] & a$grandeur == x[2L]], cpl)), NA)),
    mu_lis = grepl("mu_lis = -ln(1 - g)", k("r2", "valeur"), fixed = TRUE) &&
      grepl("q_lis = g", k("distance_probabilites", "valeur"), fixed = TRUE),
    s5 = all(vapply(c("bornes_exactes", "p_valeurs", "p_valeurs_asymptotiques"), function(g)
      startsWith(k("sous_groupes", g), "relation lissee : non produites,") && !grepl(" p ", k("sous_groupes", g), fixed = TRUE),
      NA)),
    attribution = !any(grepl("Liddell|Garwood|Byar|Wilson|Hilferty|Clopper|Biessy", b$statut))))
})
verifier("A STATUTS_VALIDATION_LISSEE et HYPOTHESE_VALIDATION_LISSEE : tout texte non ASCII marqué UTF-8 (texte_moteur()), quelle que soit la locale d'installation", {
  s <- tablexp:::STATUTS_VALIDATION_LISSEE$statut
  s <- c(s[grepl("[^\001-\177]", s, useBytes = TRUE)], tablexp:::HYPOTHESE_VALIDATION_LISSEE)
  length(s) > 1L && all(Encoding(s) == "UTF-8") && all(validUTF8(s))
})
verifier("A HYPOTHESE_VALIDATION_LISSEE égale à la mention validation_h0_lissee du rapport (copie de R/rapport.R)",
         identical(tablexp:::HYPOTHESE_VALIDATION_LISSEE, unname(tablexp:::MENTIONS_RAPPORT[["validation_h0_lissee"]])))
verifier("A PARAMETRES_ETAPES$validation : table_reference conditionnel à relation_table (independante, ajustee_hors_outil, positionnee) ; autres paramètres sans condition ; domaine de relation_table cite lissee (ADR 0017, point 3.4)", {
  t <- tablexp:::PARAMETRES_ETAPES$validation
  identical(t$selon, c("relation_table", NA, NA, NA, NA, NA)) &&
    identical(t$valeurs[[1L]], c("independante", "ajustee_hors_outil", "positionnee")) &&
    all(lengths(t$valeurs[-1L]) == 0L) && grepl("lissee", t$domaine[t$parametre == "relation_table"], fixed = TRUE)
})

# --- B. Refus de valider_plan(), chacun levé seul -----------------------------------

verifier("B table_reference sous lissee : parametre_inattendu (module construire_table), message exact (« a », « b » ou « c »), details", controle(
  rv(val = c(list(table_reference = "tv"), VAL)), "construire_table", "parametre_inattendu",
  paste("Étape « validation » : paramètre(s) « table_reference » sans objet sous relation_table = « lissee » (employé(s)",
        "sous relation_table = « independante », « ajustee_hors_outil » ou « positionnee ») ; un paramètre sans effet",
        "n'est pas admis dans le plan."),
  list(etape = "validation", parametre = "table_reference", gouvernant = "relation_table", valeur = "lissee")))
verifier("B table_reference absent sous independante : parametre_manquant conditionnel, message exact, details", controle(
  rv(lissage = NULL, val = avec(relation_table = "independante")), "construire_table", "parametre_manquant",
  paste("Étape « validation » : paramètre(s) requis absent(s) « table_reference » sous relation_table = « independante »",
        "(aucun défaut implicite)."),
  list(etape = "validation", parametre = "table_reference", gouvernant = "relation_table", valeur = "independante")))
verifier("B relation_table invalide : relation_table_invalide avant la présence et la forme de table_reference ; message cite les quatre relations", {
  m <- paste("La relation entre la table et les observations (relation_table) doit être une chaîne unique, sans",
             "attribut, prise dans « independante », « ajustee_hors_outil », « positionnee », « lissee », sans défaut",
             "(ADR 0015, point 7.1).")
  tous(list(sans = controle(rv(val = avec(relation_table = "lisse")), "validation", "relation_table_invalide", m,
                            list(relation_table = "lisse")),
            cle = controle(rv(val = c(list(table_reference = 1), avec(relation_table = "lisse"))), "validation",
                           "relation_table_invalide")))
})
verifier("B lissage_absent (module validation) : relation lissee sans étape lissage, message exact, details", controle(
  rv(lissage = NULL), "validation", "lissage_absent",
  paste("La relation « lissee » valide la table lissée de cette construction : l'étape lissage doit figurer au plan",
        "(avant l'étape validation)."),
  list(relation_table = "lissee")))
verifier("B plage_ages_differente_lissage (module validation) : message exact, details", controle(
  rv(val = avec(plage_ages = c(45, 60), tranches_age = 45)), "validation", "plage_ages_differente_lissage",
  paste("Relation « lissee » : la plage d'âges de la validation (45 à 60) doit être celle du lissage (40 à 60) : les",
        "degrés de liberté effectifs sont ceux du lissage sur ses cellules (les tranches d'âge restent libres)."),
  list(plage_ages = c(45, 60), plage_ages_lissage = c(40, 60))))
verifier("B controler_parametres_validation() en appel direct : table_reference absent admis (missing()), forme contrôlée s'il est passé", {
  cpv <- tablexp:::controler_parametres_validation
  is.null(refus_de(cpv(relation_table = "lissee", plage_ages = c(40, 60), tranches_age = 40, niveau = 0.95,
                       replications = 9, positionnement = NULL, lissage = LISSAGE))) &&
    isTRUE(controle(refus_de(cpv(1, "independante", c(40, 60), 40, 0.95, 9, NULL)), "validation", "cle_table_invalide"))
})
verifier("B controler_parametres_validation() en appel direct (C-1 d'actuary, m2 de l'audit) : lissage omis (défaut NULL) admis hors lissee, refus lissage_absent sous lissee ; table_reference omis sous positionnee : erreur de programmation préfixée, jamais une erreur R générique", {
  cpv <- tablexp:::controler_parametres_validation
  pos <- list(methode = "smr", table_reference = "tv", plage_ages = c(40, 60), niveau = 0.95)
  e <- tryCatch({ cpv(relation_table = "positionnee", plage_ages = c(40, 60), tranches_age = 40, niveau = 0.95,
                      replications = 9, positionnement = pos); NULL }, error = function(e) e)
  tous(list(
    independante = isTRUE(cpv("tv", "independante", c(40, 60), 40, 0.95, 9, NULL)),
    lissee = controle(refus_de(cpv(relation_table = "lissee", plage_ages = c(40, 60), tranches_age = 40, niveau = 0.95,
                                   replications = 9, positionnement = NULL)), "validation", "lissage_absent"),
    positionnee = !is.null(e) && !inherits(e, "refus_tablexp") &&
      startsWith(conditionMessage(e), "controler_parametres_validation() : ") &&
      grepl("erreur de programmation", conditionMessage(e), fixed = TRUE),
    positionnee_avec = isTRUE(cpv("tv", "positionnee", c(40, 60), 40, 0.95, 9, pos))))
})

# --- C. Étape validation sous lissee ------------------------------------------------

verifier("C parametres : table_reference et table_nom absents ; parametres_estimes NA_integer_ ; ddl_effectifs (clés, ddl_effectifs double recopiés de l'étape lissage, ddl_effectifs_retenus double, identical() à ddl_effectifs, toute cellule étant retenue) ; appariement_sexe par_segment ; hypothèse nulle propre (valeur calculée à la main, identité exacte)", {
  p <- V$parametres
  seg <- L$resultat$segments
  tous(list(
    noms = identical(names(p), c("relation_table", "parametres_estimes", "ddl_effectifs", "plage_ages", "tranches_age",
                                 "niveau", "replications", "convention", "appariement_sexe", "n_max_wilcoxon_exact",
                                 "n_max_runs_exact", "hypothese_nulle", "reference_methode", "statuts_sous_groupes")),
    pe = identical(p$parametres_estimes, NA_integer_),
    ddl = identical(p$ddl_effectifs, data.frame(sexe = seg$sexe, ddl_effectifs = seg$ddl_effectifs,
                                                ddl_effectifs_retenus = seg$ddl_effectifs, stringsAsFactors = FALSE)) &&
      is.double(p$ddl_effectifs$ddl_effectifs) && all(CV$etat == "retenue"),
    sexe = identical(p$appariement_sexe, "par_segment"),
    h0 = identical(p$hypothese_nulle, tablexp:::HYPOTHESE_VALIDATION_LISSEE),
    sg = identical(p$statuts_sous_groupes$statut,
                   tablexp:::STATUTS_VALIDATION_LISSEE$statut[tablexp:::STATUTS_VALIDATION_LISSEE$statistique == "sous_groupes"])))
})
verifier("C1 ddl = n - somme des nu_s, à 1e-12 près en relatif, en double, contre la somme des ddl effectifs du résultat de lissage, toute cellule pondérée étant retenue (identité mathématique) ; ddl_effectifs_retenus == ddl_effectifs", {
  n <- sum(CV$etat == "retenue")
  att <- n - sum(L$resultat$segments$ddl_effectifs)
  tous(list(x2 = proche(valeur(ST, "khi2_pearson/ddl"), att, rel = 1e-12, abs = 0),
            g2 = proche(valeur(ST, "deviance/ddl"), att, rel = 1e-12, abs = 0),
            non_entier = valeur(ST, "deviance/ddl") != round(valeur(ST, "deviance/ddl")),
            retenus = all(V$parametres$ddl_effectifs$ddl_effectifs_retenus == V$parametres$ddl_effectifs$ddl_effectifs)))
})
# Cas limite de la recette agregats (ADR 0017, point 5.5, M417) : (H, 50)
# pondérée par le lissage (E^i > 0, D = 0) mais non retenue par la
# validation (E^c = 0, état sans_exposition).
local({
  j <- JEU; k <- j$sexe == "H" & j$age == 50
  j$evenements[k] <- 0; j$exposition_centrale[k] <- 0
  x <- construire_table(j, ROLES, plan_l())
  st <- x$etapes$validation$resultat$statistiques
  cv <- x$etapes$validation$resultat$cellules
  tl <- x$etapes$lissage$resultat$table_lissee
  sg <- x$etapes$lissage$resultat$segments
  de <- x$etapes$validation$parametres$ddl_effectifs
  i <- which(tl$sexe == "H" & tl$age == 50L)
  h <- tl$levier[i]
  verifier("C1 cas limite agregats, (H, 50) à E^c = 0, E^i > 0, D = 0 : cellule pondérée (observee, levier > 0) non retenue (sans_exposition) ; ddl = n - (somme des nu_s - h_ii) à 1e-12 près en relatif ; ddl_effectifs_retenus < ddl_effectifs pour H, égaux pour F (identité mathématique ; ADR 0017, point 5.5, M417)", {
    n <- sum(cv$etat == "retenue")
    att <- n - (sum(sg$ddl_effectifs) - h)
    tous(list(
      cellule = identical(tl$etat[i], "observee") && h > 0 && identical(cv$etat[i], "sans_exposition") && n == 41L,
      x2 = proche(valeur(st, "khi2_pearson/ddl"), att, rel = 1e-12, abs = 0),
      g2 = proche(valeur(st, "deviance/ddl"), att, rel = 1e-12, abs = 0),
      ancien = abs(valeur(st, "deviance/ddl") - (n - sum(sg$ddl_effectifs))) > 0.1,
      h = de$ddl_effectifs_retenus[de$sexe == "H"] < de$ddl_effectifs[de$sexe == "H"] &&
        proche(de$ddl_effectifs_retenus[de$sexe == "H"], de$ddl_effectifs[de$sexe == "H"] - h, rel = 1e-12, abs = 0),
      f = identical(de$ddl_effectifs_retenus[de$sexe == "F"], de$ddl_effectifs[de$sexe == "F"]),
      somme = identical(valeur(st, "deviance/ddl"), n - sum(de$ddl_effectifs_retenus))))
  })
})
verifier("C p asymptotiques de X2 et G2 produites (état produite) bien que parametres_estimes soit NA : pchisq(statistique, n - somme des nu_s, queue supérieure) (identité exacte ; ADR 0017, point 5.1)", {
  ddl <- valeur(ST, "khi2_pearson/ddl")
  identical(V$parametres$parametres_estimes, NA_integer_) &&
    identical(etat_de(ST, c("khi2_pearson/p_asymptotique", "deviance/p_asymptotique")), c("produite", "produite")) &&
    identical(valeur(ST, "khi2_pearson/p_asymptotique"),
              stats::pchisq(valeur(ST, "khi2_pearson/statistique"), ddl, lower.tail = FALSE)) &&
    identical(valeur(ST, "deviance/p_asymptotique"),
              stats::pchisq(valeur(ST, "deviance/statistique"), ddl, lower.tail = FALSE))
})
verifier("C attendus : qx_table = g de la table lissée, taux_table = -log1p(-g), A = E^c (-log1p(-g)) (identité exacte ; règle unique, ADR 0017, point 1.3)", {
  tl <- L$resultat$table_lissee
  identical(CV$qx_table, tl$qx) && identical(CV$taux_table, -log1p(-tl$qx)) &&
    identical(CV$attendus, CV$exposition_centrale * -log1p(-tl$qx)) && identical(CV[c("sexe", "age")], tl[c("sexe", "age")])
})
verifier("C états : bornes, p et racine cubique du SMR contraint_par_lissage ; espérances, variance_h0, fluctuation, p et erreurs de Monte-Carlo non_etabli_sous_lissage ; colonnes fluctuation_* NA ; aucun tirage", {
  smr_h0 <- paste0("smr/", c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure", "p_centrale",
                             "z_racine_cubique", "p_racine_cubique"))
  mc <- c("p_monte_carlo", "erreur_monte_carlo")
  ne <- c("residus/esperance_au_dela_2", "residus/esperance_au_dela_3", "khi2_pearson/variance_h0",
          "signes/esperance_positifs_h0", paste0("fluctuation/", c("hors_intervalle_exact", "esperance_hors_intervalle_exact",
                                                                    "hors_intervalle_asymptotique",
                                                                    "esperance_hors_intervalle_asymptotique")),
          as.vector(outer(c("khi2_pearson", "deviance", "signes", "runs", "wilcoxon"), mc, paste, sep = "/")))
  cles <- paste(ST$statistique, ST$grandeur, sep = "/")
  autres <- setdiff(cles, c(smr_h0, ne))
  tous(list(
    smr = all(etat_de(ST, smr_h0) == "contraint_par_lissage") && all(is.na(valeur(ST, smr_h0))),
    ne = all(etat_de(ST, ne) == "non_etabli_sous_lissage") && all(is.na(valeur(ST, ne))),
    autres = all(etat_de(ST, autres) == "produite") && !anyNA(valeur(ST, autres)),
    fluctuation = all(vapply(CV[grep("^fluctuation_", names(CV))], function(v) all(is.na(v)), NA)) &&
      length(grep("^fluctuation_", names(CV))) == 4L,
    tirages = identical(V$resultat$monte_carlo$tirages_effectues, 0L),
    statuts = identical(ST$statut, tablexp:::STATUTS_VALIDATION_LISSEE$statut[
      tablexp:::STATUTS_VALIDATION_LISSEE$statistique != "sous_groupes"])))
})
verifier("C sous-groupes : SMR descriptif O / A ; bornes et p-valeurs NA ; segment contraint_par_lissage ; segment x tranche et tranche (deux tranches) non_etabli_sous_lissage ; une seule tranche : tranche contraint_par_lissage (valeur calculée à la main)", {
  sg <- V$resultat$sous_groupes
  x1 <- construire_table(JEU, ROLES, plan_l(val = avec(tranches_age = 40)))
  sg1 <- x1$etapes$validation$resultat$sous_groupes
  colonnes <- c("borne_inf_exacte", "borne_sup_exacte", "p_inferieure", "p_superieure", "p_centrale",
                "p_inferieure_asymptotique", "p_superieure_asymptotique", "p_centrale_asymptotique")
  tous(list(
    smr = identical(sg$smr, sg$observes / sg$attendus),
    na = all(vapply(sg[colonnes], function(v) all(is.na(v)) && is.double(v), NA)),
    etats = identical(sg$etat[sg$regroupement == "segment"], c("contraint_par_lissage", "contraint_par_lissage")) &&
      all(sg$etat[sg$regroupement %in% c("segment_tranche", "tranche")] == "non_etabli_sous_lissage"),
    une_tranche = identical(sg1$etat[sg1$regroupement == "tranche"], "contraint_par_lissage") &&
      all(sg1$etat[sg1$regroupement == "segment_tranche"] == "contraint_par_lissage")))
})
verifier("C1 SMR sous pondération exposition (type B) : O - A égal à l'écart somme E^i g - somme E^c (-ln(1 - g)) calculé, à 1e-8 somme D près (conservation somme E^i g = somme D, LD p. 32-33) ; |SMR - 1| égal à |écart| / A (identité mathématique)", {
  tl <- L$resultat$table_lissee
  b <- X$etapes$brut$resultat$binomial
  ex <- X$etapes$exposition$resultat$exposition
  ei <- b$exposition_initiale[match(paste(tl$sexe, tl$age), paste(ex$sexe, ex$age))]
  ecart <- sum(ei * tl$qx) - sum(CV$exposition_centrale * -log1p(-tl$qx))
  O <- valeur(ST, "smr/observes"); A <- valeur(ST, "smr/attendus")
  tous(list(retenues = all(CV$etat == "retenue"),
            ecart = proche(O - A, ecart, rel = 0, abs = 1e-8 * sum(JEU$evenements)),
            smr = proche(abs(valeur(ST, "smr/smr") - 1), abs(ecart) / A, rel = 0, abs = 1e-8 * sum(JEU$evenements) / A),
            voisin = abs(valeur(ST, "smr/smr") - 1) < 0.01))
})
verifier("C n - somme des nu_s <= 0 : p asymptotiques de X2 et G2 NA, état contraint_par_lissage (statistiques_validation(), cellules fabriquées)", {
  s <- tablexp:::statistiques_validation(c(3, 5, 2), c(100, 100, 100), rep(0.03, 3), rep(-log1p(-0.03), 3),
                                         100 * rep(-log1p(-0.03), 3), rep("retenue", 3), rep(1L, 3), 40:42, 0.95,
                                         "lissee", 9, 1L, NULL, 3)$statistiques
  identical(valeur(s, "khi2_pearson/ddl"), 0) &&
    identical(etat_de(s, c("khi2_pearson/p_asymptotique", "deviance/p_asymptotique")),
              c("contraint_par_lissage", "contraint_par_lissage")) &&
    all(is.na(valeur(s, c("khi2_pearson/p_asymptotique", "deviance/p_asymptotique"))))
})
verifier("C statistiques_validation() : somme_ddl requise sous la seule relation lissee (erreur de programmation)", {
  sv <- function(rel, somme) tablexp:::statistiques_validation(c(3, 5), c(100, 100), rep(0.03, 2), rep(-log1p(-0.03), 2),
                                                               100 * rep(-log1p(-0.03), 2), rep("retenue", 2), rep(1L, 2),
                                                               40:41, 0.95, rel, 9, 1L, NULL, somme)
  erreur_programmation(sv("lissee", NULL)) && erreur_programmation(sv("independante", 1))
})

# Appel direct de calculer_validation() sous lissee, table construite
# fabriquée à partir de celle de X.
cv_l <- function(construite = L$resultat$table_lissee, ddl = V$parametres$ddl_effectifs, relation = "lissee",
                 table = NULL, cle = NULL)
  tablexp:::calculer_validation(X$etapes$exposition$resultat$exposition, "sexe", FALSE, "dernier_anniversaire", table, cle,
                                relation, c(40, 60), c(40, 50), 0.95, 99, V$resultat$monte_carlo$graine, NULL,
                                construite, ddl)
verifier("C calculer_validation() appelé directement sur la table construite de X : identical() à l'étape de X (identité exacte)",
         identical(cv_l()[c("parametres", "resultat", "diagnostics")], V[c("parametres", "resultat", "diagnostics")]))
verifier("C taux_table_egal_un (module validation) : q = 1 dans une cellule exposée de la table construite, message propre, details", {
  t1 <- L$resultat$table_lissee; t1$qx[3L] <- 1
  controle(refus_de(cv_l(construite = t1)), "validation", "taux_table_egal_un",
           paste("Validation : 1 cellule(s) exposée(s) où la table construite donne q_x = 1 (arrondi) : l'attendu",
                 "A = E^c (-ln(1 - q_x)) est infini, le modèle de Poisson ne s'applique pas."),
           list(cellules = 1L))
})
verifier("C calculer_validation() sous lissee : table construite ou ddl effectifs NULL, table construite sans colonne levier (ADR 0017, point 5.5), grille différente (âges permutés, segments permutés), ddl effectifs integer -> erreur de programmation", {
  t1 <- L$resultat$table_lissee
  perm <- t1[c(22:42, 1:21), ]; rownames(perm) <- NULL
  ages <- t1; ages$age <- rev(ages$age)
  d_int <- V$parametres$ddl_effectifs; d_int$ddl_effectifs <- c(3L, 4L)
  sans_lev <- t1[setdiff(names(t1), "levier")]
  tous(list(nul_t = erreur_programmation(cv_l(construite = NULL)), nul_d = erreur_programmation(cv_l(ddl = NULL)),
            sans_levier = erreur_programmation(cv_l(construite = sans_lev)),
            perm = erreur_programmation(cv_l(construite = perm)), ages = erreur_programmation(cv_l(construite = ages)),
            d_int = erreur_programmation(cv_l(ddl = d_int))))
})
verifier("C relation independante : table construite et ddl effectifs passés ignorés (identical() à l'appel sans eux) (critère C2)", {
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  t_ref <- table_reference(data.frame(age = gm$age, sexe = "ensemble", qx = gm$qx), "GM", "importee", "test")
  identical(cv_l(relation = "independante", table = t_ref, cle = "tv"),
            cv_l(construite = NULL, ddl = NULL, relation = "independante", table = t_ref, cle = "tv"))
})

# --- D. Rapport figé, plan, importance_exclusions -----------------------------------

verifier("D rapport figé : titre propre à la relation lissee, mentions validation_lissee, validation_h0_lissee et validation_monte_carlo_lissee, sans la phrase de la table de référence prise telle quelle (valeur calculée à la main)", {
  h <- rawToChar(ecrire_rapport(X)); Encoding(h) <- "UTF-8"
  m <- tablexp:::MENTIONS_RAPPORT
  e <- tablexp:::echapper_html
  tous(list(
    titre = grepl("<h2>Étape validation : statistiques de validation de la table lissée de cette construction</h2>", h,
                  fixed = TRUE),
    relation = grepl(e(m[["validation_lissee"]]), h, fixed = TRUE),
    h0 = grepl(e(m[["validation_h0_lissee"]]), h, fixed = TRUE),
    mc = grepl(e(m[["validation_monte_carlo_lissee"]]), h, fixed = TRUE),
    sans_table = !grepl(e(m[["validation_sans_verdict_table"]]), h, fixed = TRUE)))
})
verifier("D rapport figé : colonne levier de la table lissée rendue (table etapes/lissage/resultat/table_lissee, en-tête levier double, première valeur à 17 chiffres), colonne ddl_effectifs_retenus de etapes/validation/parametres/ddl_effectifs (valeur calculée à la main)", {
  h <- rawToChar(ecrire_rapport(X)); Encoding(h) <- "UTF-8"
  tl <- L$resultat$table_lissee
  de <- V$parametres$ddl_effectifs
  tous(list(
    levier = grepl(paste0("<caption><code>etapes/lissage/resultat/table_lissee</code> : data.frame ; lignes : 42 ;",
                          " colonnes : 6</caption>\n<thead>\n<tr><th>sexe<br>character</th><th>age<br>integer</th>",
                          "<th>qx<br>double</th><th>etat<br>character</th><th>levier<br>double</th><th>poids<br>double",
                          "</th></tr>\n</thead>\n<tbody>\n<tr><td>&quot;", tl$sexe[1L], "&quot;</td><td>", tl$age[1L],
                          "</td><td>", sprintf("%.17g", tl$qx[1L]), "</td><td>&quot;observee&quot;</td><td>",
                          sprintf("%.17g", tl$levier[1L]), "</td>"), h, fixed = TRUE),
    retenus = grepl(paste0("<caption><code>etapes/validation/parametres/ddl_effectifs</code> : data.frame ; lignes : 2 ;",
                           " colonnes : 3</caption>\n<thead>\n<tr><th>sexe<br>character</th><th>ddl_effectifs<br>",
                           "double</th><th>ddl_effectifs_retenus<br>double</th></tr>\n</thead>\n<tbody>\n<tr><td>&quot;",
                           de$sexe[1L], "&quot;</td><td>", sprintf("%.17g", de$ddl_effectifs[1L]), "</td><td>",
                           sprintf("%.17g", de$ddl_effectifs_retenus[1L]), "</td></tr>"), h, fixed = TRUE)))
})
verifier("D plan sous lissee : canoniser_plan() idempotente, aller-retour ecrire_plan() / lire_plan() identique, fiche$plan canonique sans table_reference (identité exacte)", {
  cn <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                             tablexp:::ordres_canoniques())
  c1 <- cn(plan_l(val = VAL[c(5L, 4L, 3L, 2L, 1L)]))
  identical(names(c1$etapes$validation), names(VAL)) && identical(cn(c1), c1) &&
    identical(lire_plan(ecrire_plan(c1)), c1) && identical(X$fiche$plan, c1) && is.null(X$fiche$tables_reference)
})

local({
  GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 20000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D_("2005-01-01"))
  jeu <- simuler_portefeuille(loi, pf, list(debut = D_("2015-01-01"), fin = D_("2024-12-31")), 22011204L)
  roles <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
             date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
  p <- plan_l(lissage = modifyList(LISSAGE, list(plage_ages = c(40, 80), parametre_lissage = 1e3)),
              val = avec(plage_ages = c(40, 80), tranches_age = c(40, 60)))
  p$etapes$importance_exclusions <- list(niveau = 0.95)
  x <- construire_table(jeu, roles, p)
  ie <- x$etapes$importance_exclusions
  verifier("D importance_exclusions sous lissee : comparateur 2 non produit (non_produite_par_cette_version, M415, point H), smr_comparateur_2 NULL ; aucune table de référence (jeu en lignes simulé, graine 22011204)",
           identical(ie$parametres$comparateurs$etat,
                     c("produite", "non_produite_par_cette_version", "non_produite_par_cette_version")) &&
             is.null(ie$resultat$smr_comparateur_2) && "smr_comparateur_2" %in% names(ie$resultat) &&
             identical(x$etapes$validation$parametres$relation_table, "lissee"))
  verifier("D calculer_importance_exclusions() : sous lissee, une table reçue est incohérente (erreur de programmation)", {
    t_ref <- table_reference(data.frame(age = GM$age, sexe = "ensemble", qx = GM$qx), "GM", "importee", "test")
    erreur_programmation(tablexp:::calculer_importance_exclusions(jeu, roles, x$etapes$donnees, x$etapes$exposition,
                                                                  x$etapes$validation, t_ref, NULL, NULL, NULL, "sexe",
                                                                  0.95))
  })
})
