# -----------------------------------------------------------------------------
#  tests/unitaires/test_stocks_successifs.R : recette d'exposition « stocks
#  successifs » (issue #180 ; spécification d'actuary-approfondi acceptée,
#  M328 ; avis de forme (0''), G6, G7 et G14 ; ADR 0016, points 6 et 7)
#
#  Commit (4a) : champ conditionnel `inventaires` du plan (validation,
#  canonisation, présence selon la recette), p$inventaires, rôle type_ligne,
#  motifs (§ 7.2, § 7.3) et observations (§ 4) des stocks, refus
#  inventaire_sans_ligne (§ 7.4). La recette n'est pas encore au catalogue
#  (ligne `stocks_successifs` au commit (4b)) : motifs et observations sont
#  testés en appel direct, la présence du champ par une recette fabriquée.
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  valeurs écrites à la main (motifs levés seuls sur une base construite à la
#  main, faits par unité et décomptes, refus, motifs et messages) ; identité
#  exacte (permutation des lignes reçues, aller-retour du plan). Aucune
#  graine.
# -----------------------------------------------------------------------------

D <- as.Date
VD <- list(depart = "vivant", arrivee = "deces")
rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}

# Période 2015-2016, délai 0 : grille alignée 2014-12-31, 2015-12-31,
# 2016-12-31 (t_1 = début - 1, t_M = fin effective ; § 2.2).
PER <- list(debut = D("2015-01-01"), fin = D("2016-12-31"))
G3 <- D(c("2014-12-31", "2015-12-31", "2016-12-31"))
P3 <- tablexp:::valeurs_parametres_donnees(VD, PER, 0, G3)

# --- champ `inventaires` du plan (ADR 0016, point 7.1 ; § 2) ---------------------

plan_s <- function(inventaires = NULL, delai = 0, etapes = list(donnees = list(exclusions = character(0)))) {
  p <- list(transition = VD, periode = PER, delai_declaration = delai)
  if (!is.null(inventaires)) p$inventaires <- inventaires
  c(p, list(segmentation = character(0), etapes = etapes))
}
CN <- function(p) tablexp:::canoniser_plan(p, tablexp:::CHAMPS_PLAN, tablexp:::PARAMETRES_ETAPES,
                                           tablexp:::ordres_canoniques())

verifier("inventaires : champ de CHAMPS_PLAN après delai_declaration, hors de CHAMPS_PLAN_REQUIS (valeur écrite à la main)",
         identical(tablexp:::CHAMPS_PLAN, c("transition", "periode", "delai_declaration", "inventaires", "segmentation",
                                            "graine", "etapes")) &&
           !("inventaires" %in% tablexp:::CHAMPS_PLAN_REQUIS))
verifier("inventaires : grille alignée admise par valider_plan() (Date double ou integer), plan rendu inchangé", {
  gi <- structure(as.integer(unclass(G3)), class = "Date")
  identical(valider_plan(plan_s(G3)), plan_s(G3)) && identical(valider_plan(plan_s(gi)), plan_s(gi))
})
# Défaut attendu (texte du message, élément et non nom de liste : un nom non
# ASCII est traduit dans le codage natif sous la locale C) et valeurs reçues.
CAS_INVALIDES <- list(
  list("classe autre que Date exacte, stockage autre que double ou integer, ou autre attribut",
       list(as.numeric(G3), as.character(G3), as.POSIXct(G3), structure(G3, names = c("a", "b", "c")),
            structure(unclass(G3), class = c("Date", "x")), list(G3))),
  list("moins de deux dates", list(G3[1L], G3[0L])),
  list("valeur manquante", list(c(G3, NA))),
  list("date non entière ou non finie", list(structure(c(unclass(G3), Inf), class = "Date"),
                                             structure(c(Inf, Inf), class = "Date"),
                                             structure(c(unclass(G3)[1L], Inf, Inf), class = "Date"),
                                             structure(c(-Inf, -Inf), class = "Date"))),
  list("date hors du domaine [0001-01-01, 9999-12-31]", list(structure(c(-800000, unclass(G3)), class = "Date"))),
  list("dates non strictement croissantes", list(rev(G3), G3[c(1L, 2L, 2L, 3L)])))
verifier("inventaires_invalides : chaque défaut de forme ou de domaine refusé (module construire_table), le défaut nommé dans le message (valeur écrite à la main)", {
  e <- character(0)
  for (cas in CAS_INVALIDES) for (x in cas[[2L]]) {
    d <- cas[[1L]]
    r <- rf(valider_plan(plan_s(x)))
    if (!isTRUE(controle(r, "construire_table", "inventaires_invalides")) ||
        !grepl(paste0("défaut(s) : ", d, "."), conditionMessage(r), fixed = TRUE)) e <- c(e, d)
  }
  if (length(e)) paste("écart :", paste(unique(e), collapse = " | ")) else TRUE
})
verifier("inventaires_invalides : plusieurs défauts cités ensemble (non entière, non croissante), details$inventaires = valeur reçue", {
  x <- structure(c(unclass(G3), 16500.5), class = "Date")
  r <- rf(valider_plan(plan_s(x)))
  isTRUE(controle(r, "construire_table", "inventaires_invalides")) && identical(r$details$inventaires, x) &&
    grepl("défaut(s) : date non entière ou non finie ; dates non strictement croissantes.", conditionMessage(r), fixed = TRUE)
})
verifier("inventaires_periode : premier inventaire autre que la veille du début, dernier autre que la fin effective (délai pris en compte) ; message exact (valeur écrite à la main)", {
  r1 <- rf(valider_plan(plan_s(G3[-1L])))
  r2 <- rf(valider_plan(plan_s(G3, delai = 31)))
  r3 <- rf(valider_plan(plan_s(D(c("2014-12-31", "2015-12-31", "2016-11-30")), delai = 31)))
  r4 <- rf(valider_plan(plan_s(D(c("2015-01-01", "2016-01-01")))))
  isTRUE(controle(r1, "construire_table", "inventaires_periode")) &&
    identical(conditionMessage(r1), paste(
      "Grille des inventaires non alignée sur la période d'observation : premier inventaire 2015-12-31 au lieu de",
      "2014-12-31 ; la première date doit être la veille du début (2014-12-31) et la dernière la fin effective",
      "(2016-12-31 : fin moins le délai de déclaration), une date d'inventaire désignant l'état en fin de journée.")) &&
    identical(r1$details$bornes, "premier") &&
    isTRUE(controle(r2, "construire_table", "inventaires_periode")) && identical(r2$details$bornes, "dernier") &&
    grepl("dernier inventaire 2016-12-31 au lieu de 2016-11-30", conditionMessage(r2), fixed = TRUE) &&
    is.null(r3) &&
    identical(r4$details$bornes, c("premier", "dernier")) &&
    identical(r4$details$attendu, D(c("2014-12-31", "2016-12-31")))
})
verifier("inventaires : forme contrôlée après les paramètres de l'étape donnees (période invalide refusée d'abord : periode_invalide)",
         isTRUE(controle(rf(valider_plan(modifyList(plan_s(G3[1L]), list(periode = list(debut = 1, fin = 2))))),
                         "donnees", "periode_invalide")))

verifier("canoniser_plan : inventaires en Date à stockage double, à sa place dans l'ordre de CHAMPS_PLAN ; idempotente ; plan sans inventaires sans champ (valeur écrite à la main)", {
  gi <- structure(as.integer(unclass(G3)), class = "Date")
  p <- plan_s(G3)
  p <- p[c("etapes", setdiff(names(p), "etapes"))]
  p$inventaires <- gi
  c1 <- CN(p)
  identical(names(c1), c("transition", "periode", "delai_declaration", "inventaires", "segmentation", "etapes")) &&
    identical(c1$inventaires, structure(as.double(unclass(G3)), class = "Date")) && identical(CN(c1), c1) &&
    !("inventaires" %in% names(CN(plan_s())))
})
verifier("canoniser_plan : inventaires sans classe Date -> erreur de programmation (plan non validé)",
         inherits(tryCatch(CN(c(plan_s(), list(inventaires = 1:3))), error = function(e) e), "error") &&
           !inherits(tryCatch(CN(c(plan_s(), list(inventaires = 1:3))), error = function(e) e), "refus_tablexp"))
verifier("ecrire_plan() / lire_plan() : aller-retour identical() d'un plan canonique qui porte la grille (identité exacte)", {
  c1 <- CN(plan_s(G3))
  identical(lire_plan(ecrire_plan(c1)), c1)
})

# Présence selon la recette (ADR 0016, point 7.1), propriété de la recette :
# rôle date_inventaire parmi ses rôles requis.
BL <- data.frame(n = D("1950-01-01"), e = D("2016-01-01"), s = D("2016-06-01"), c = "deces", stringsAsFactors = FALSE)
RL <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
REC_STOCKS <- list(recette = "stocks_successifs", roles_requis = c("type_ligne", "date_inventaire", "date_naissance",
                                                                   "date_sortie", "cause_sortie"))
verifier("inventaires_inattendus : plan en lignes qui porte la grille, refusé par construire_table() (module construire_table) ; message exact (valeur écrite à la main)", {
  r <- rf(construire_table(BL, RL, plan_s(G3)))
  isTRUE(controle(r, "construire_table", "inventaires_inattendus")) &&
    identical(conditionMessage(r), paste(
      "Le plan déclare une grille des inventaires (champ « inventaires ») alors que la recette",
      "« lignes_dates_completes » n'emploie pas de date d'inventaire ; la construction est refusée.")) &&
    identical(r$details$recette, "lignes_dates_completes") && identical(r$details$inventaires, G3)
})
verifier("inventaires_manquants : recette dont les rôles requis comptent date_inventaire, plan sans grille (appel direct, recette fabriquée) ; message exact ; grille présente admise", {
  r <- rf(tablexp:::controler_inventaires_recette(CN(plan_s()), REC_STOCKS))
  isTRUE(controle(r, "construire_table", "inventaires_manquants")) &&
    identical(conditionMessage(r), paste(
      "La recette « stocks_successifs » exige la grille des inventaires (champ « inventaires » du plan : dates",
      "des inventaires, la première la veille du début de la période, la dernière la fin effective) ; aucun",
      "défaut implicite, la construction est refusée.")) &&
    is.null(rf(tablexp:::controler_inventaires_recette(CN(plan_s(G3)), REC_STOCKS))) &&
    is.null(rf(tablexp:::controler_inventaires_recette(CN(plan_s()), list(recette = "lignes_dates_completes",
                                                                          roles_requis = RL[0L]))))
})
verifier("p$inventaires : présent (grille canonique) si valeurs_parametres_donnees() la reçoit, absent sinon ; p d'un plan en lignes sans élément inventaires (controler_plan_roles())", {
  e <- tablexp:::controler_plan_roles(BL, RL, plan_s(), NULL)
  identical(P3$inventaires, G3) &&
    identical(names(P3), c("transition", "debut", "fin", "borne", "periode", "delai_declaration", "inventaires")) &&
    !("inventaires" %in% names(tablexp:::valeurs_parametres_donnees(VD, PER, 0))) &&
    !("inventaires" %in% names(e$p)) && identical(e$p, tablexp:::valeurs_parametres_donnees(VD, PER, 0))
})
verifier("champ_inconnu : la liste des champs admis cite inventaires (valeur écrite à la main)", {
  r <- rf(valider_plan(c(plan_s(), list(foo = 1))))
  isTRUE(controle(r, "construire_table", "champ_inconnu")) &&
    grepl("(champs admis : « transition », « periode », « delai_declaration », « inventaires », « segmentation », « graine », « etapes »)",
          conditionMessage(r), fixed = TRUE)
})

# --- rôle type_ligne (§ 3) -----------------------------------------------------

verifier("type_ligne : rôle du catalogue, famille observation, type categorie, modalités inventaire et evenement ; aucune recette ne l'emploie avant le commit (4b) (valeur écrite à la main)", {
  cr <- catalogue_roles()
  k <- which(cr$role == "type_ligne")
  length(k) == 1L && identical(cr$famille[k], "observation") && identical(cr$type_admis[k], "character, factor") &&
    identical(cr$recettes[k], "") && identical(tablexp:::MODALITES_TYPE_LIGNE, c("inventaire", "evenement")) &&
    identical(cr$role[k + 1L], "date_inventaire")
})
verifier("type_ligne déclaré sur une base en lignes : role_non_pris_en_charge (refus 4, aucune recette ne l'emploie) ; message exact", {
  b <- BL; b$t <- "inventaire"
  r <- rf(construire_table(b, c(RL, type_ligne = "t"), plan_s()))
  isTRUE(controle(r, "donnees", "role_non_pris_en_charge")) &&
    identical(conditionMessage(r), paste(
      "Rôle déclaré qu'aucune recette d'exposition du catalogue n'emploie : « type_ligne » (colonne « t ») ;",
      "l'ignorer changerait le sens du résultat, la construction est refusée."))
})

# --- motifs des stocks (§ 7.2, § 7.3) -----------------------------------------------

verifier("MOTIFS_LIGNES_STOCKS et MOTIFS_UNITE_STOCKS : listes fermées de la spécification de #180, § 7.2 et § 7.3, dans l'ordre (valeur écrite à la main)",
         identical(tablexp:::MOTIFS_LIGNES_STOCKS,
                   c("type_ligne_manquant", "type_ligne_hors_modalites", "id_contrat_manquant", "id_tete_manquant",
                     "naissance_manquante", "sexe_manquant", "sexe_hors_modalites", "segment_manquant",
                     "annee_naissance_incoherente", "age_aberrant", "inventaire_sans_date", "inventaire_hors_grille",
                     "inventaire_avec_sortie", "naissance_apres_inventaire", "inventaire_duplique", "sortie_manquante",
                     "cause_manquante", "cause_hors_modalites", "cause_en_cours", "evenement_avec_date_inventaire",
                     "naissance_apres_sortie")) &&
           identical(tablexp:::MOTIFS_UNITE_STOCKS,
                     c("unite_naissance_incoherente", "unite_sexe_incoherent", "unite_segment_incoherent",
                       "evenements_multiples", "presence_apres_evenement", "presence_interrompue",
                       "sortie_non_renseignee", "evenement_apres_absence")))
verifier("motifs des stocks : hors du catalogue avant le commit (4b) (exclusion d'un code des stocks : exclusion_inconnue)", {
  r <- rf(valider_plan(plan_s(etapes = list(donnees = list(exclusions = "presence_interrompue")))))
  propres <- setdiff(c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS),
                     c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE, tablexp:::MOTIFS_LIGNES_AGREGATS))
  isTRUE(controle(r, "donnees", "exclusion_inconnue")) && length(propres) == 18L &&
    !any(propres %in% tablexp:::motifs_recettes())
})

# Base valide, six unités sur la grille G3 (une ligne par unité et par
# inventaire où elle est présente, une ligne par événement) :
#   A : P0, présente aux trois inventaires, en cours (fin C) ;
#   B : P0, présente à t_1 et t_2, décès le 2016-05-01 (fin S, intervalle 2) ;
#   C : vue pour la première fois à t_2 (E), présente à t_3 (fin C) ;
#   D : entrée et sortie entre deux inventaires, décès le 2015-06-01 (ES) ;
#   E : événement avant t_1, sans inventaire (HF) ;
#   F : P0, rachat le 2015-03-01 (fin S, intervalle 1).
NA_D <- D(NA)
BS <- data.frame(
  id = c("A", "A", "A", "B", "B", "B", "C", "C", "D", "E", "F", "F"),
  t = c("inventaire", "inventaire", "inventaire", "inventaire", "inventaire", "evenement", "inventaire", "inventaire",
        "evenement", "evenement", "inventaire", "evenement"),
  di = c(G3, G3[1:2], NA_D, G3[2:3], NA_D, NA_D, G3[1L], NA_D),
  sx = c("H", "H", "H", "F", "F", "F", "H", "H", "F", "H", "F", "F"),
  seg = c("a", "a", "a", "b", "b", "b", "a", "a", "b", "a", "b", "b"),
  n = D(c(rep("1950-03-01", 3), rep("1960-07-15", 3), rep("1970-01-01", 2), "1955-05-05", "1945-02-02",
          rep("1980-10-10", 2))),
  an = c(rep(1950L, 3), rep(1960L, 3), rep(1970L, 2), 1955L, 1945L, rep(1980L, 2)),
  s = D(c(NA, NA, NA, NA, NA, "2016-05-01", NA, NA, "2015-06-01", "2014-06-01", NA, "2015-03-01")),
  c = c(NA, NA, NA, NA, NA, "deces", NA, NA, "deces", "rachat", NA, "rachat"),
  stringsAsFactors = FALSE)
RS <- c(id_tete = "id", sexe = "sx", date_naissance = "n", annee_naissance = "an", type_ligne = "t",
        date_inventaire = "di", date_sortie = "s", cause_sortie = "c", segment = "seg")
motifs_s <- function(d, roles = RS, segmentation = "segment", p = P3) {
  m <- tablexp:::evaluer_motifs_stocks(d, roles, p, segmentation)$motifs
  m[lengths(m) > 0L]
}
modif <- function(col, valeur, ligne) { d <- BS; d[[col]][ligne] <- valeur; d }
ajout <- function(...) { d <- BS; l <- d[1L, ]; v <- list(...); for (k in names(v)) l[[k]] <- v[[k]]; rbind(d, l) }

verifier("motifs des stocks, base valide : aucun motif ; liste nommée dans l'ordre MOTIFS_LIGNES_STOCKS puis MOTIFS_UNITE_STOCKS",
         !length(motifs_s(BS)) &&
           identical(names(tablexp:::evaluer_motifs_stocks(BS, RS, P3, "segment")$motifs),
                     c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS)))
# Chaque cas : base modifiée, lignes attendues. Motifs de toute ligne et de
# la ligne d'événement sur la ligne 9 (unité D, seule ligne : son retrait ne
# crée aucun motif d'unité) ; de la ligne d'inventaire sur la ligne 7 (C à
# t_2 : C reste vue pour la première fois à t_3).
RS_C <- c(id_contrat = "id", RS[names(RS) != "id_tete"])
CAS_S <- list(
  type_ligne_manquant = list(modif("t", NA, 9L), 9L),
  type_ligne_hors_modalites = list(modif("t", "Evenement", 9L), 9L),
  id_contrat_manquant = list(modif("id", NA, 9L), 9L, RS_C),
  id_tete_manquant = list(modif("id", NA, 9L), 9L),
  naissance_manquante = list(modif("n", NA_D, 9L), 9L),
  sexe_manquant = list(modif("sx", NA, 9L), 9L),
  sexe_hors_modalites = list(modif("sx", "X", 9L), 9L),
  segment_manquant = list(modif("seg", NA, 9L), 9L),
  annee_naissance_incoherente = list(modif("an", 1956L, 9L), 9L),
  age_aberrant = list({ d <- modif("n", D("1885-06-01"), 9L); d$an[9L] <- 1885L; d }, 9L),
  inventaire_sans_date = list(modif("di", NA_D, 7L), 7L),
  inventaire_hors_grille = list(modif("di", D("2015-06-30"), 7L), 7L),
  inventaire_avec_sortie = list(modif("c", "en_cours", 7L), 7L),
  naissance_apres_inventaire = list({ d <- modif("n", D("2016-01-01"), 7L); d$an[7L] <- 2016L; d }, 7L),
  inventaire_duplique = list(rbind(BS, BS[7L, ]), c(7L, 13L)),
  sortie_manquante = list(modif("s", NA_D, 9L), 9L),
  cause_manquante = list(modif("c", NA, 9L), 9L),
  cause_hors_modalites = list(modif("c", "Deces", 9L), 9L),
  cause_en_cours = list(modif("c", "en_cours", 9L), 9L),
  evenement_avec_date_inventaire = list(modif("di", G3[2L], 9L), 9L),
  naissance_apres_sortie = list({ d <- modif("n", D("2015-07-01"), 9L); d$an[9L] <- 2015L; d }, 9L),
  unite_naissance_incoherente = list({ d <- modif("n", D("1970-01-02"), 8L); d }, 7:8),
  unite_sexe_incoherent = list(modif("sx", "F", 8L), 7:8),
  unite_segment_incoherent = list(modif("seg", "b", 8L), 7:8),
  evenements_multiples = list(ajout(id = "B", t = "evenement", s = D("2016-06-01"), c = "deces", sx = "F", seg = "b",
                                    n = D("1960-07-15"), an = 1960L, di = NA_D), c(4:6, 13L)),
  presence_apres_evenement = list(ajout(id = "B", t = "inventaire", di = G3[3L], sx = "F", seg = "b",
                                        n = D("1960-07-15"), an = 1960L), c(4:6, 13L)),
  presence_interrompue = list(BS[-2L, ], c(1L, 2L)),
  sortie_non_renseignee = list(BS[-6L, ], 4:5),
  evenement_apres_absence = list(modif("s", D("2016-03-01"), 12L), 11:12))
verifier("motifs des stocks : chaque motif de la spécification (§ 7.2, § 7.3) levé seul sur une base construite à la main ; motifs d'unité marqués sur les lignes propres de l'unité", {
  e <- character(0)
  for (m in names(CAS_S)) {
    cas <- CAS_S[[m]]
    roles <- if (length(cas) > 2L) cas[[3L]] else RS
    o <- motifs_s(cas[[1L]], roles)
    if (!identical(o, stats::setNames(list(as.integer(cas[[2L]])), m))) e <- c(e, m)
  }
  if (identical(names(CAS_S), c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS)) && !length(e)) TRUE
  else paste("écart :", paste(e, collapse = ", "))
})
verifier("motifs des stocks : unite_segment_incoherent seulement si le segment est employé par la segmentation",
         !length(motifs_s(CAS_S$unite_segment_incoherent[[1L]], segmentation = character(0))))
verifier("motifs des stocks : le retrait d'une ligne par un motif de ligne se répercute sur les motifs d'unité (ligne de C à t_3 hors modalités : C, vue à t_2 seulement, sans événement -> sortie_non_renseignee sur sa ligne propre)",
         identical(motifs_s(modif("sx", "X", 8L)), list(sexe_hors_modalites = 8L, sortie_non_renseignee = 7L)))
verifier("motifs des stocks : aucun motif de comparaison levé sur une valeur manquante (naissance manquante : ni age_aberrant, ni naissance_apres_inventaire ; sortie manquante : ni naissance_apres_sortie)", {
  identical(names(motifs_s(modif("n", NA_D, 7L))), "naissance_manquante") &&
    identical(names(motifs_s({ d <- modif("s", NA_D, 9L); d$n[9L] <- D("2016-01-01"); d$an[9L] <- 2016L; d })),
              "sortie_manquante")
})
verifier("motifs des stocks : ligne d'événement « en_cours » sans date de sortie -> cause_en_cours seul (sortie_manquante garde le sens des lignes) ; ligne sans type valide : aucun motif propre à un type", {
  identical(names(motifs_s({ d <- modif("c", "en_cours", 9L); d$s[9L] <- NA_D; d })), "cause_en_cours") &&
    identical(names(motifs_s({ d <- modif("t", "x", 7L); d$c[7L] <- "deces"; d })), "type_ligne_hors_modalites")
})
verifier("C7 (instant d'une date d'inventaire) : sortie le jour t_i, absente à t_i -> aucun motif ; sortie le jour t_i, présente à t_i -> presence_apres_evenement (lignes de l'unité)", {
  !length(motifs_s(modif("s", G3[2L], 12L))) &&
    identical(motifs_s(modif("s", G3[2L], 6L)), list(presence_apres_evenement = 4:6))
})
verifier("motifs des stocks : deux identifiants déclarés -> erreur de programmation (refus identifiants_concurrents en amont, matrice)", {
  e <- tryCatch(motifs_s(BS, c(RS, id_contrat = "id")), error = function(e) e)
  inherits(e, "error") && !inherits(e, "refus_tablexp")
})

# --- observations des stocks (§ 4) et inventaire_sans_ligne (§ 7.4) ---------------

obs_s <- function(d, roles = RS, segmentation = "segment", donnees = d)
  tablexp:::observations_stocks(d, seq_len(nrow(d)), roles, segmentation, P3, donnees)
OB <- obs_s(BS)
verifier("observations des stocks : une observation par unité, rangées par identifiant ; faits par unité (indices f, l, événement, intervalle, classe P0 / E / ES / HF, fin S / C), sans hypothèse (valeur écrite à la main)", {
  o <- OB$observations
  identical(OB$unite, "tete") &&
    identical(names(o), c("unite", "ligne", "nb_lignes", "date_naissance", "sexe", "segment", "premier_inventaire",
                          "dernier_inventaire", "date_sortie", "cause", "intervalle", "classe", "fin")) &&
    identical(o$unite, c("A", "B", "C", "D", "E", "F")) && identical(o$ligne, c(1L, 4L, 7L, 9L, 10L, 11L)) &&
    identical(o$nb_lignes, c(3L, 3L, 2L, 1L, 1L, 2L)) &&
    identical(o$date_naissance, D(c("1950-03-01", "1960-07-15", "1970-01-01", "1955-05-05", "1945-02-02", "1980-10-10"))) &&
    identical(o$sexe, c("H", "F", "H", "F", "H", "F")) && identical(o$segment, c("a", "b", "a", "b", "a", "b")) &&
    identical(o$premier_inventaire, c(1L, 1L, 2L, NA, NA, 1L)) && identical(o$dernier_inventaire, c(3L, 2L, 3L, NA, NA, 1L)) &&
    identical(o$date_sortie, D(c(NA, "2016-05-01", NA, "2015-06-01", "2014-06-01", "2015-03-01"))) &&
    identical(o$cause, c(NA, "deces", NA, "deces", "rachat", "rachat")) &&
    identical(o$intervalle, c(NA, 2L, NA, 1L, NA, 1L)) &&
    identical(o$classe, c("P0", "P0", "E", "ES", "HF", "P0")) && identical(o$fin, c("C", "S", "C", NA, NA, "S")) &&
    identical(OB$observation_des_lignes, c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 4L, 5L, 6L, 6L))
})
verifier("observations des stocks : relevés (décompte des classes, ES par cause, présents par inventaire, entrants, ES et sorties par cause par intervalle ; valeur écrite à la main)", {
  r <- OB$releves
  identical(r[c("unites", "p0", "entrees", "entres_sortis", "hors_fenetre", "fin_sortie", "fin_censure")],
            list(unites = 6L, p0 = 3L, entrees = 1L, entres_sortis = 1L, hors_fenetre = 1L, fin_sortie = 2L,
                 fin_censure = 2L)) &&
    identical(r$entres_sortis_par_cause, c(deces = 1L, rachat = 0L, terme = 0L, autre = 0L)) &&
    identical(r$inventaires, data.frame(date_inventaire = G3 + 0, presents = c(3L, 3L, 2L))) &&
    identical(r$intervalles, data.frame(debut = G3[1:2] + 0, fin = G3[2:3] + 0, entrants = c(1L, 0L),
                                        entres_sortis = c(1L, 0L), sorties_deces = c(0L, 1L), sorties_rachat = c(1L, 0L),
                                        sorties_terme = c(0L, 0L), sorties_autre = c(0L, 0L)))
})
verifier("observations des stocks : unité contrat sous id_contrat ; identifiants numériques rangés par valeur (9 < 10 < 100), non par chaîne", {
  d <- BS; d$id <- c(10, 10, 10, 9, 9, 9, 100, 100, 5, 1000, 20, 20)
  o <- obs_s(d, RS_C)
  identical(o$unite, "contrat") && identical(o$observations$unite, c(5, 9, 10, 20, 100, 1000)) &&
    identical(o$observations$classe, c("ES", "P0", "P0", "P0", "E", "HF"))
})
verifier("observations des stocks (C14, partiel) : une permutation des lignes reçues donne les mêmes observations (hors premier numéro de ligne) et les mêmes relevés (identité exacte)", {
  perm <- c(12L, 3L, 9L, 1L, 6L, 10L, 2L, 8L, 5L, 11L, 4L, 7L)
  o2 <- obs_s(BS[perm, ])
  a <- OB$observations; b <- o2$observations
  identical(a[names(a) != "ligne"], b[names(b) != "ligne"]) && identical(o2$releves, OB$releves) &&
    identical(o2$observation_des_lignes, OB$observation_des_lignes[perm])
})
verifier("observations des stocks : unité incohérente (motifs d'unité non appliqués) -> erreur de programmation", {
  e <- tryCatch(obs_s(BS[-6L, ]), error = function(e) e)
  inherits(e, "error") && !inherits(e, "refus_tablexp")
})
# Grille G3, une unité présente à t_1 et t_2, décès entre t_2 et t_3 : aucune
# unité présente à t_3.
B1 <- data.frame(id = c("A", "A", "A"), t = c("inventaire", "inventaire", "evenement"), di = c(G3[1:2], NA_D),
                 sx = "H", seg = "a", n = D("1950-03-01"), an = 1950L, s = D(c(NA, NA, "2016-06-01")),
                 c = c(NA, NA, "deces"), stringsAsFactors = FALSE)
verifier("inventaire_sans_ligne : date de la grille sans unité présente, aucune ligne d'inventaire reçue (module donnees) ; message exact (valeur écrite à la main)", {
  r <- rf(obs_s(B1))
  isTRUE(controle(r, "donnees", "inventaire_sans_ligne")) &&
    identical(conditionMessage(r), paste(
      "Inventaire(s) sans aucune unité présente sur les lignes retenues : 2016-12-31 (aucune ligne reçue à",
      "cette date d'inventaire) ; la grille du plan suppose un inventaire exhaustif à chacune de ses dates (un portefeuille vide à une",
      "date d'inventaire n'est pas pris en charge), la construction est refusée.")) &&
    identical(r$details$dates, G3[3L]) && identical(r$details$lignes_recues, 0L)
})
verifier("inventaire_sans_ligne : date dont toutes les lignes d'inventaire reçues sont exclues ; message qui les décompte", {
  recues <- rbind(B1, transform(B1[1L, ], id = "Z", di = G3[3L]))
  r <- rf(obs_s(B1, donnees = recues))
  isTRUE(controle(r, "donnees", "inventaire_sans_ligne")) &&
    grepl("2016-12-31 (1 ligne(s) reçue(s) à cette date d'inventaire, toutes exclues)", conditionMessage(r), fixed = TRUE) &&
    identical(r$details$lignes_recues, 1L)
})
verifier("inventaire_sans_ligne : ligne reçue datée de la date de grille, à type_ligne manquant (exclue) : comptée parmi les lignes reçues, quel que soit son type (réserve d'actuary)", {
  # Unité B présente à t_2 et t_3, décès hors période ; la seule ligne reçue
  # à t_1 (unité A) a un type manquant et n'est pas retenue.
  retenues <- data.frame(id = "B", t = c("inventaire", "inventaire"), di = G3[2:3], sx = "H", seg = "a",
                         n = D("1950-03-01"), an = 1950L, s = NA_D, c = NA_character_, stringsAsFactors = FALSE)
  recues <- rbind(transform(retenues[1L, ], id = "A", t = NA_character_, di = G3[1L]), retenues)
  r <- rf(tablexp:::observations_stocks(retenues, 2:3, RS, "segment", P3, recues))
  isTRUE(controle(r, "donnees", "inventaire_sans_ligne")) &&
    grepl("2014-12-31 (1 ligne(s) reçue(s) à cette date d'inventaire, toutes exclues)", conditionMessage(r), fixed = TRUE) &&
    identical(r$details$lignes_recues, 1L) && identical(r$details$dates, G3[1L])
})
