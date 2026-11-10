# -----------------------------------------------------------------------------
#  tests/unitaires/test_stocks_successifs.R : recette d'exposition « stocks
#  successifs » (issue #180 ; spécification d'actuary-approfondi acceptée,
#  M328 ; avis de forme (0''), G6, G7 et G14 ; ADR 0016, points 6 et 7)
#
#  Commit (4a) : champ conditionnel `inventaires` du plan (validation,
#  canonisation, présence selon la recette), p$inventaires, rôle type_ligne,
#  motifs (§ 7.2, § 7.3) et observations (§ 4) des stocks, refus
#  inventaire_sans_ligne (§ 7.4), testés en appel direct.
#  Commit (4b) : ligne `stocks_successifs` du catalogue et de la matrice,
#  paramètre conditionnel entree_entre_inventaires (ADR 0016, point 7.2),
#  preparer_donnees() sous les stocks avec l'exclusion par unité (amendement
#  A1 de la spécification, M339 : C5 amendé, C16 et son oracle), fenêtres et
#  hypothèses de la recette (§ 5, § 6), refus du § 7.1, relevés du § 7.5,
#  fiche (§ 10), oracle C1 (lignes reconstituées dans le test), C3, C6, C8,
#  C9, C14.
#  Commit (4c) : forme « stocks successifs » du simulateur (§ 11 ; C11) et
#  compléments de couverture demandés par actuary au visa de (4b) : fenêtres
#  sur une grille irrégulière avec délai, écrites à la main ; C2 en test
#  direct ; C6 contre les décomptes de l'attribut `simulation` ; C16-oracle
#  sur des jeux simulés, plusieurs graines (A1.8).
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  valeurs écrites à la main (motifs levés seuls sur une base construite à la
#  main, faits par unité et décomptes, refus, motifs et messages) ; identité
#  exacte (permutation des lignes reçues, aller-retour du plan, oracle des
#  lignes reconstituées, oracle de la base privée des unités exclues) ;
#  inégalité exacte (C3). Graines des jeux simulés de l'oracle C1 : 180L et
#  181L ; du C16-oracle sur jeux simulés : 180L, 181L et 182L ; du test
#  d'instant de la forme du simulateur : 183L (< 1000, exemptées du contrôle
#  de #176, règle A5).
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

verifier("type_ligne : rôle du catalogue, famille observation, type categorie, modalités inventaire et evenement ; employé par la recette stocks_successifs depuis le commit (4b) (valeur écrite à la main)", {
  cr <- catalogue_roles()
  k <- which(cr$role == "type_ligne")
  length(k) == 1L && identical(cr$famille[k], "observation") && identical(cr$type_admis[k], "character, factor") &&
    identical(cr$recettes[k], "stocks_successifs") && identical(tablexp:::MODALITES_TYPE_LIGNE, c("inventaire", "evenement")) &&
    identical(cr$role[k + 1L], "date_inventaire")
})
verifier("type_ligne déclaré sur une base en lignes : recettes_concurrentes depuis le commit (4b) (refus 5, discriminants des lignes et des stocks ; auparavant role_non_pris_en_charge) ; message exact", {
  b <- BL; b$t <- "inventaire"
  r <- rf(construire_table(b, c(RL, type_ligne = "t"), plan_s()))
  isTRUE(controle(r, "donnees", "recettes_concurrentes")) &&
    identical(conditionMessage(r), paste(
      "Les rôles déclarés relèvent de plusieurs recettes d'exposition (rôles propres à chacune :",
      "recette « lignes_dates_completes » : « date_entree » (colonnes « e ») ; recette « stocks_successifs » :",
      "« type_ligne » (colonnes « t »)) ; la recette n'est pas choisie à la place de l'utilisateur : retirer les",
      "rôles de la recette qui ne s'applique pas, la construction est refusée.")) &&
    identical(r$details, list(recette = c("lignes_dates_completes", "stocks_successifs"),
                              role = c("date_entree", "type_ligne"), colonne = c("e", "t")))
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
verifier("motifs des stocks : au catalogue depuis le commit (4b) (union des motifs des recettes : valider_plan() admet un code des stocks ; sous les lignes, construire_table() le refuse : exclusion_hors_recette)", {
  pl <- plan_s(etapes = list(donnees = list(exclusions = "presence_interrompue")))
  propres <- setdiff(c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS),
                     c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE, tablexp:::MOTIFS_LIGNES_AGREGATS))
  is.null(rf(valider_plan(pl))) && length(propres) == 18L && all(propres %in% tablexp:::motifs_recettes()) &&
    isTRUE(controle(rf(construire_table(BL, RL, pl)), "donnees", "exclusion_hors_recette"))
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
verifier("motifs des stocks (C5 amendé, amendement A1) : une unité porteuse d'un motif de ligne n'est soumise à aucun motif d'unité ; ses lignes propres sont des lignes de propagation (ligne de C à t_3 hors modalités ; valeur écrite à la main)", {
  ev <- tablexp:::evaluer_motifs_stocks(modif("sx", "X", 8L), RS, P3, "segment")
  identical(motifs_s(modif("sx", "X", 8L)), list(sexe_hors_modalites = 8L)) &&
    identical(ev$lignes_propagation, 7L) && identical(ev$unites_propagation, 1L)
})
verifier("motifs des stocks (amendement A1) : lignes de propagation vides sans motif de ligne, ou si le motif est porté par une ligne sans identifiant valide ou par l'unique ligne de l'unité (valeur écrite à la main)", {
  vide <- function(d) { ev <- tablexp:::evaluer_motifs_stocks(d, RS, P3, "segment")
                        identical(ev$lignes_propagation, integer(0)) && identical(ev$unites_propagation, 0L) }
  vide(BS) && vide(modif("id", NA, 1L)) && vide(modif("c", "Deces", 9L)) && vide(BS[-6L, ])
})
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

# =============================================================================
# Commit (4b) : recette au catalogue, paramètre conditionnel, preparer_donnees(),
# fenêtres, hypothèses, oracles.
# =============================================================================

# Plan des stocks sur la grille G3 (période 2015-2016, délai 0). `mode` NULL :
# paramètre entree_entre_inventaires absent ; `exposition` FALSE : plan réduit
# à l'étape donnees.
plan_st <- function(mode = "milieu", exclusions = character(0), sup = NULL, segmentation = "segment",
                    convention = "plus_fine", par_annee = FALSE, transition = VD, inventaires = G3,
                    exposition = TRUE, graine = NULL) {
  p <- list(transition = transition, periode = PER, delai_declaration = 0)
  if (!is.null(inventaires)) p$inventaires <- inventaires
  p$segmentation <- segmentation
  if (!is.null(graine)) p$graine <- graine
  ex <- list(convention = convention, par_annee = par_annee)
  if (!is.null(mode)) ex$entree_entre_inventaires <- mode
  p$etapes <- c(list(donnees = list(exclusions = exclusions)), if (exposition) list(exposition = ex), sup)
  p
}
BRUT3 <- list(brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95))
# Avertissements hors role_non_employe (rôle sexe déclaré, segmentation
# « segment » des plans de la base BS).
avert_s <- function(x) {
  a <- x$avertissements
  a <- a[a$code != "role_non_employe", , drop = FALSE]
  rownames(a) <- NULL
  a
}
codes <- function(x, etape = NULL) {
  a <- avert_s(x)
  if (!is.null(etape)) a <- a[a$etape == etape, , drop = FALSE]
  a$code
}

# --- catalogue des recettes et matrice (ADR 0016, points 1 et 5 ; § 3, § 8) -------

CAT <- tablexp:::CATALOGUE_RECETTES
MAT <- tablexp:::MATRICE_RECETTES
verifier("catalogue des recettes, (4b) : ligne stocks_successifs (§ 3 : type_ligne, date_inventaire, naissance, date et cause de sortie requises ; un identifiant parmi id_tete, id_contrat ; annee_naissance, sexe, segment facultatifs ; discriminants type_ligne et date_inventaire ; native dernier_anniversaire, § 9 ; seam fenetres ; motifs des stocks ; valeur écrite à la main)", {
  k <- match("stocks_successifs", CAT$recette)
  identical(CAT$recette, c("lignes_dates_completes", "agregats", "stocks_successifs")) &&
    identical(CAT$requis[[k]], c("type_ligne", "date_inventaire", "date_naissance", "date_sortie", "cause_sortie")) &&
    identical(CAT$un_parmi[[k]], list(c("id_tete", "id_contrat"))) &&
    identical(CAT$facultatifs[[k]], c("annee_naissance", "sexe", "segment")) &&
    identical(CAT$discriminants[[k]], c("type_ligne", "date_inventaire")) &&
    identical(CAT$convention_native[k], "dernier_anniversaire") && identical(CAT$seam[k], "fenetres") &&
    identical(CAT$motifs_ligne[[k]], tablexp:::MOTIFS_LIGNES_STOCKS) &&
    identical(CAT$motifs_unite[[k]], tablexp:::MOTIFS_UNITE_STOCKS)
})
verifier("matrice, (4b) : lignes stocks_successifs (§ 8 : tout admis, sauf importance_exclusions, motif importance_exclusions_stocks ; valeur écrite à la main)", {
  m <- MAT[MAT$recette == "stocks_successifs", -1L]; rownames(m) <- NULL
  identical(m, data.frame(
    etape = c("donnees", "exposition", "brut", "brut", "brut", "brut", "positionnement", "homogeneite", "validation",
              "importance_exclusions"),
    estimateur = c(NA, NA, NA, "poisson", "binomial", "kaplan_meier", NA, NA, NA, NA),
    admis = c(rep("oui", 9L), "non"), role_requis = NA_character_,
    motif = c(rep(NA_character_, 9L), "importance_exclusions_stocks"), stringsAsFactors = FALSE))
})
verifier("deduire_recette(), (4b) : rôles des stocks -> recette stocks_successifs, convention native dernier_anniversaire, rôles employés, seam fenetres, motifs ligne puis unité (valeur écrite à la main)", {
  r <- tablexp:::deduire_recette(RS)
  identical(r$recette, "stocks_successifs") && identical(r$convention_native, "dernier_anniversaire") &&
    identical(r$roles_employes, c("type_ligne", "date_inventaire", "date_naissance", "date_sortie", "cause_sortie",
                                  "id_tete", "id_contrat", "annee_naissance", "sexe", "segment")) &&
    identical(r$seam, "fenetres") &&
    identical(r$motifs, c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS))
})
verifier("deduire_recette(), (4b) : rôles des stocks sans identifiant -> aucune_recette, seule la recette des stocks listée, « un rôle parmi » (message écrit à la main)", {
  r <- rf(tablexp:::deduire_recette(RS[names(RS) != "id_tete"]))
  isTRUE(controle(r, "donnees", "aucune_recette")) &&
    identical(conditionMessage(r), paste("Aucune recette d'exposition ne s'applique ; rôles requis absents, recette par recette :",
                                         "recette « stocks_successifs » : un rôle parmi « id_tete », « id_contrat ».")) &&
    identical(r$details$recette, "stocks_successifs")
})
verifier("catalogue_roles(), (4b) : colonne recettes des rôles des stocks (valeur écrite à la main)", {
  v <- stats::setNames(catalogue_roles()$recettes, catalogue_roles()$role)
  identical(unname(v[c("type_ligne", "date_inventaire", "id_tete", "id_contrat", "date_naissance", "annee_naissance",
                       "date_sortie", "cause_sortie", "sexe", "segment", "date_entree")]),
            c("stocks_successifs", "stocks_successifs", rep("lignes_dates_completes, stocks_successifs", 6L),
              rep("lignes_dates_completes, agregats, stocks_successifs", 2L), "lignes_dates_completes"))
})

# --- paramètre conditionnel entree_entre_inventaires (ADR 0016, point 7.2) ------

verifier("PARAMETRES_ETAPES, (4b) : colonne recette (NA pour tout paramètre existant) ; exposition : entree_entre_inventaires, character, conditionnel à stocks_successifs (valeur écrite à la main)", {
  pe <- tablexp:::PARAMETRES_ETAPES
  all(vapply(pe, function(t) identical(names(t), c("parametre", "type", "domaine", "recette")), NA)) &&
    identical(pe$exposition$parametre, c("convention", "par_annee", "entree_entre_inventaires")) &&
    identical(pe$exposition$type, c("character", "logical", "character")) &&
    identical(pe$exposition$recette, c(NA, NA, "stocks_successifs")) &&
    all(is.na(unlist(lapply(pe[names(pe) != "exposition"], function(t) t$recette)))) &&
    identical(tablexp:::VALEURS_ENTREE_ENTRE_INVENTAIRES, c("premier_inventaire", "milieu"))
})
verifier("parametre_manquant (stocks, paramètre absent ; module construire_table, contrôlé après la recette) : message exact, details ; valider_plan() admet le plan", {
  pl <- plan_st(mode = NULL)
  r <- rf(construire_table(BS, RS, pl))
  is.null(rf(valider_plan(pl))) && isTRUE(controle(r, "construire_table", "parametre_manquant")) &&
    identical(conditionMessage(r), paste("Étape « exposition » : paramètre(s) requis absent(s) « entree_entre_inventaires »",
                                         "sous la recette « stocks_successifs » (aucun défaut implicite).")) &&
    identical(r$details, list(etape = "exposition", parametre = "entree_entre_inventaires", recette = "stocks_successifs"))
})
verifier("parametre_inattendu (lignes, paramètre présent ; module construire_table) : message visé par actuary, details ; valider_plan() admet le plan", {
  pl <- plan_s(etapes = list(donnees = list(exclusions = character(0)),
                             exposition = list(convention = "plus_fine", par_annee = FALSE,
                                               entree_entre_inventaires = "milieu")))
  r <- rf(construire_table(BL, RL, pl))
  is.null(rf(valider_plan(pl))) && isTRUE(controle(r, "construire_table", "parametre_inattendu")) &&
    identical(conditionMessage(r), paste("Étape « exposition » : paramètre(s) « entree_entre_inventaires » sans objet pour la",
                                         "recette « lignes_dates_completes » (employé(s) par la recette « stocks_successifs ») ;",
                                         "un paramètre sans effet n'est pas admis dans le plan.")) &&
    identical(r$details, list(etape = "exposition", parametre = "entree_entre_inventaires",
                              recette = "lignes_dates_completes"))
})
verifier("entree_entre_inventaires_invalide (valider_plan(), module exposition) : valeur hors des deux modes, NA, longueur 2, attribut, autre type ; message exact", {
  ko <- list("trapeze", NA_character_, c("milieu", "milieu"), c(a = "milieu"), 1, TRUE, factor("milieu"))
  rs <- lapply(ko, function(x) rf(valider_plan(plan_st(mode = x))))
  all(vapply(rs, function(r) isTRUE(controle(r, "exposition", "entree_entre_inventaires_invalide")), NA)) &&
    identical(conditionMessage(rs[[1L]]), paste("L'hypothèse d'entrée entre inventaires (entree_entre_inventaires) doit être une",
                                                "chaîne unique, sans attribut, prise dans « premier_inventaire », « milieu » (aucun",
                                                "défaut).")) &&
    is.null(rf(valider_plan(plan_st(mode = "premier_inventaire")))) && is.null(rf(valider_plan(plan_st(mode = "milieu"))))
})
verifier("entree_entre_inventaires_invalide (valider_plan()) : paramètre présent à NULL -> refus (le plan le porte), alors que l'absence du paramètre est admise (#196, point 3 ; cohérence avec les autres paramètres)", {
  p <- plan_st(); p$etapes$exposition["entree_entre_inventaires"] <- list(NULL)
  r <- rf(valider_plan(p))
  "entree_entre_inventaires" %in% names(p$etapes$exposition) && isTRUE(controle(r, "exposition", "entree_entre_inventaires_invalide")) &&
    is.null(rf(valider_plan(plan_st(mode = NULL))))
})
verifier("valider_plan() n'évalue aucune valeur du plan (#196, point 3) : objet de langage sous entree_entre_inventaires -> refus entree_entre_inventaires_invalide ; sous convention -> refus convention_invalide, sans que l'expression soit évaluée", {
  p1 <- plan_st(); p1$etapes$exposition$entree_entre_inventaires <- quote(paste0("mil", "ieu"))
  p2 <- plan_st(); p2$etapes$exposition$convention <- quote(stop("évalué"))
  r1 <- rf(valider_plan(p1))
  r2 <- tryCatch(valider_plan(p2), refus_tablexp = function(e) e, error = function(e) e)
  isTRUE(controle(r1, "exposition", "entree_entre_inventaires_invalide")) &&
    inherits(r2, "refus_tablexp") && identical(r2$motif, "convention_invalide")
})
verifier("canoniser_plan(), (4b) : paramètres d'étape dans l'ordre du catalogue restreint aux présents ; idempotente avec et sans paramètre conditionnel ; paramètre sans recette absent -> erreur de programmation ; aller-retour ecrire_plan() / lire_plan() (identité exacte)", {
  p <- plan_st()
  p$etapes$exposition <- p$etapes$exposition[c("entree_entre_inventaires", "par_annee", "convention")]
  c1 <- CN(p); c0 <- CN(plan_st(mode = NULL))
  manque <- plan_st(); manque$etapes$exposition$par_annee <- NULL
  e <- tryCatch(CN(manque), error = function(e) e)
  identical(names(c1$etapes$exposition), c("convention", "par_annee", "entree_entre_inventaires")) &&
    identical(CN(c1), c1) && identical(CN(c0), c0) && identical(names(c0$etapes$exposition), c("convention", "par_annee")) &&
    inherits(e, "error") && !inherits(e, "refus_tablexp") && identical(lire_plan(ecrire_plan(c1)), c1)
})

# --- refus du § 7.1 propres aux stocks (C4, C12) ----------------------------------

verifier("recettes_concurrentes (C12) : rôles des stocks avec date_entree -> refus 5, module donnees", {
  b <- BS; b$e <- b$n
  isTRUE(controle(rf(construire_table(b, c(RS, date_entree = "e"), plan_st())), "donnees", "recettes_concurrentes"))
})
verifier("identifiants_concurrents (C12, QS3) : id_tete et id_contrat déclarés -> refus, module donnees, message exact, details", {
  b <- BS; b$ct <- b$id
  r <- rf(construire_table(b, c(RS, id_contrat = "ct"), plan_st()))
  isTRUE(controle(r, "donnees", "identifiants_concurrents")) &&
    identical(conditionMessage(r), paste("Sous la recette « stocks_successifs », les rôles « id_tete », « id_contrat » sont déclarés",
                                         "ensemble : l'unité d'observation des stocks successifs est un seul identifiant (la fusion",
                                         "par tête de contrats inventoriés n'est pas prise en charge) ; déclarer un seul des deux",
                                         "(« id_tete » pour une transition sur la tête, « id_contrat » pour une transition sur le",
                                         "contrat), la construction est refusée.")) &&
    identical(r$details, list(recette = "stocks_successifs", role = c("id_tete", "id_contrat"), colonne = c("id", "ct")))
})
RA <- list(depart = "en_cours", arrivee = "rachat")
verifier("identifiant_inadapte (C12) : rachat (transition portée par le contrat) avec id_tete seul -> refus, message exact ; avec id_contrat seul : admis, unité contrat, sans avertissement unite_contrat", {
  r <- rf(construire_table(BS, RS, plan_st(transition = RA)))
  x <- construire_table(BS, RS_C, plan_st(transition = RA))
  isTRUE(controle(r, "donnees", "identifiant_inadapte")) &&
    identical(conditionMessage(r), paste("Sous la recette « stocks_successifs », la transition « en_cours_rachete » porte sur le",
                                         "contrat : le rôle « id_tete » seul ne désigne pas l'unité de la transition (les contrats",
                                         "d'une même tête ne se distinguent pas) ; déclarer le rôle « id_contrat », la construction",
                                         "est refusée.")) &&
    identical(r$details, list(recette = "stocks_successifs", transition = "en_cours_rachete", role = "id_tete")) &&
    identical(x$fiche$unite_observation, "contrat") && !("unite_contrat" %in% codes(x))
})
verifier("unite_contrat (§ 3) : décès avec id_contrat seul -> admis, unité contrat, avertissement unite_contrat (même texte que sous les lignes), repris par fiche$avertissement_unite", {
  x <- construire_table(BS, RS_C, plan_st())
  a <- x$avertissements
  identical(x$fiche$unite_observation, "contrat") && identical(sum(a$code == "unite_contrat"), 1L) &&
    identical(a$message[a$code == "unite_contrat"], paste("Unité d'observation : contrat (aucun identifiant de tête déclaré) ;",
                                                          "une tête titulaire de plusieurs contrats est comptée plusieurs fois,",
                                                          "ce qui surestime l'exposition et peut compter plusieurs fois un même décès.")) &&
    identical(x$fiche$avertissement_unite, a$message[a$code == "unite_contrat"])
})
verifier("importance_exclusions_stocks (C12, QS4) : étape refusée par la matrice, module donnees, message exact", {
  r <- rf(construire_table(BS, RS, plan_st(sup = list(importance_exclusions = list(niveau = 0.95)))))
  isTRUE(controle(r, "donnees", "importance_exclusions_stocks")) &&
    identical(conditionMessage(r), paste("Sous la recette « stocks_successifs », l'étape « importance_exclusions » n'est pas",
                                         "admise ; la construction est refusée."))
})
verifier("inventaires_manquants (C9) : rôles des stocks, plan sans grille -> refus par construire_table() (module construire_table)",
         isTRUE(controle(rf(construire_table(BS, RS, plan_st(inventaires = NULL))), "construire_table", "inventaires_manquants")))
verifier("exclusion_hors_recette : code des lignes (contrat_duplique) exclu sous les stocks -> refus, module donnees",
         isTRUE(controle(rf(construire_table(BS, RS, plan_st(exclusions = "contrat_duplique"))), "donnees",
                         "exclusion_hors_recette")))
verifier("rôles non employés par les stocks : date_evenement refusé (role_non_pris_en_charge, refus 4)", {
  b <- BS; b$de <- b$s
  isTRUE(controle(rf(construire_table(b, c(RS, date_evenement = "de"), plan_st())), "donnees", "role_non_pris_en_charge"))
})

# --- preparer_donnees() sous les stocks (C5 amendé ; amendement A1) -------------

PROP <- list(inventaire_sans_date = 8L, inventaire_hors_grille = 8L, inventaire_avec_sortie = 8L,
             naissance_apres_inventaire = 8L, inventaire_duplique = 8L)
verifier("C5 : chaque motif des § 7.2 et 7.3, levé seul, est refusé par défaut (ligne_invalide qui le nomme, décompte et premières lignes) et excluable (lignes portant le motif, lignes de propagation exclues avec elles) ; aucun autre motif levé (valeur écrite à la main)", {
  e <- character(0)
  for (m in names(CAS_S)) {
    cas <- CAS_S[[m]]
    roles <- if (length(cas) > 2L) cas[[3L]] else RS
    att <- as.integer(cas[[2L]])
    r <- rf(construire_table(cas[[1L]], roles, plan_st(exposition = FALSE)))
    ok_refus <- isTRUE(controle(r, "donnees", "ligne_invalide")) && identical(names(r$details$lignes), m) &&
      identical(r$details$lignes[[m]], att) && identical(unname(r$details$decompte), length(att)) &&
      grepl(sprintf("« %s » : %d ligne(s), premières lignes %s", m, length(att), paste(att, collapse = ", ")),
            conditionMessage(r), fixed = TRUE)
    x <- tryCatch(construire_table(cas[[1L]], roles, plan_st(exposition = FALSE, exclusions = m)), error = function(e) NULL)
    prop <- if (is.null(PROP[[m]])) integer(0) else PROP[[m]]
    rd <- x$etapes$donnees
    ok_excl <- !is.null(x) && identical(rd$resultat$lignes_exclues, stats::setNames(list(att), m)) &&
      identical(rd$resultat$numeros_conserves, setdiff(seq_len(nrow(cas[[1L]])), c(att, prop))) &&
      identical(rd$resultat$nombre_lignes_exclues, length(c(att, prop))) &&
      identical(rd$diagnostics$releves$lignes_exclues_par_propagation, length(prop)) &&
      identical(rd$diagnostics$motifs$motif[rd$diagnostics$motifs$nombre > 0L], m)
    if (!ok_refus || !ok_excl) e <- c(e, m)
  }
  if (length(e)) paste("écart :", paste(e, collapse = ", ")) else TRUE
})
verifier("diagnostics des motifs sous les stocks : un code par motif des stocks, nature ligne ou unite, dans l'ordre MOTIFS_LIGNES_STOCKS puis MOTIFS_UNITE_STOCKS (valeur écrite à la main)", {
  d <- construire_table(BS, RS, plan_st(exposition = FALSE))$etapes$donnees$diagnostics$motifs
  identical(d$motif, c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS)) &&
    identical(d$nature, rep(c("ligne", "unite"), c(21L, 8L))) && all(d$nombre == 0L) && !any(d$exclu)
})
dn <- function(d, exclusions, roles = RS) construire_table(d, roles, plan_st(exposition = FALSE, exclusions = exclusions))
B_P1 <- modif("t", NA, 1L)
X_P1 <- dn(B_P1, "type_ligne_manquant")
verifier("C16, P1 : inventaire t_1 de A à type_ligne manquant, exclu -> lignes 1 à 3 exclues, A absente des observations ; relevé 1 unité, 2 lignes ; avertissements exclusion puis unites_exclues_par_propagation (texte exact) ; aucune entrée ajoutée par rapport à la base privée de A (valeur écrite à la main)", {
  rd <- X_P1$etapes$donnees
  pr <- dn(BS[-(1:3), ], character(0))$etapes$donnees
  a <- avert_s(X_P1)
  identical(rd$resultat$numeros_conserves, 4:12) && !("A" %in% rd$resultat$observations$unite) &&
    identical(rd$diagnostics$releves$unites_exclues_par_propagation, 1L) &&
    identical(rd$diagnostics$releves$lignes_exclues_par_propagation, 2L) &&
    identical(a$code[1:2], c("exclusion", "unites_exclues_par_propagation")) && identical(a$nombre[1:2], c(1L, 1L)) &&
    identical(a$message[2L], paste("Relevé : 1 unité(s) écartée(s) par propagation d'une exclusion de ligne (2 ligne(s) sans",
                                   "motif, écartée(s) avec la ligne exclue de leur unité, pour ne pas tronquer son histoire).")) &&
    identical(rd$diagnostics$releves$entrees, pr$diagnostics$releves$entrees) &&
    identical(rd$resultat$observations[names(rd$resultat$observations) != "ligne"],
              pr$resultat$observations[names(pr$resultat$observations) != "ligne"])
})
verifier("C16, P2 et P3 : inventaire t_2, puis t_3, de A à type_ligne manquant ; le plan qui ne déclare que type_ligne_manquant n'est pas refusé (ni presence_interrompue, ni sortie_non_renseignee) ; A exclue", {
  ok <- function(l) {
    x <- tryCatch(dn(modif("t", NA, l), "type_ligne_manquant"), refus_tablexp = function(e) NULL)
    !is.null(x) && identical(x$etapes$donnees$resultat$numeros_conserves, 4:12) &&
      all(x$etapes$donnees$diagnostics$motifs$nombre[x$etapes$donnees$diagnostics$motifs$nature == "unite"] == 0L)
  }
  ok(2L) && ok(3L)
})
verifier("C16, P4 : événement de B à cause « Deces », exclu -> pas de sortie_non_renseignee ; lignes 4 à 6 exclues ; relevé 1 unité, 2 lignes", {
  rd <- dn(modif("c", "Deces", 6L), "cause_hors_modalites")$etapes$donnees
  identical(rd$resultat$numeros_conserves, c(1:3, 7:12)) &&
    identical(rd$diagnostics$releves$unites_exclues_par_propagation, 1L) &&
    identical(rd$diagnostics$releves$lignes_exclues_par_propagation, 2L) &&
    identical(rd$diagnostics$motifs$nombre[rd$diagnostics$motifs$motif == "sortie_non_renseignee"], 0L)
})
verifier("C16, P5 : événement de D (unité ES, ligne unique) à cause « Deces », exclu -> ligne 9 seule exclue ; relevé 0, sans avertissement de propagation", {
  x <- dn(modif("c", "Deces", 9L), "cause_hors_modalites")
  rd <- x$etapes$donnees
  identical(rd$resultat$numeros_conserves, c(1:8, 10:12)) &&
    identical(rd$diagnostics$releves$unites_exclues_par_propagation, 0L) &&
    identical(rd$diagnostics$releves$lignes_exclues_par_propagation, 0L) &&
    !("unites_exclues_par_propagation" %in% codes(x))
})
verifier("C16, P6 : id_tete manquant sur l'inventaire t_1 de A, exclu -> ligne 1 seule exclue ; A de classe E ; relevé 0 (limite A1.7.1 encodée, pas un défaut)", {
  rd <- dn(modif("id", NA, 1L), "id_tete_manquant")$etapes$donnees
  o <- rd$resultat$observations
  identical(rd$resultat$numeros_conserves, 2:12) && identical(o$classe[o$unite == "A"], "E") &&
    identical(rd$diagnostics$releves$unites_exclues_par_propagation, 0L)
})
verifier("C16, P7 : t_1 de A à type_ligne manquant (déclaré) et t_2 de A à sexe « X » (non déclaré) -> ligne_invalide qui nomme sexe_hors_modalites seul", {
  d <- modif("t", NA, 1L); d$sx[2L] <- "X"
  r <- rf(dn(d, "type_ligne_manquant"))
  isTRUE(controle(r, "donnees", "ligne_invalide")) && identical(names(r$details$lignes), "sexe_hors_modalites") &&
    identical(r$details$lignes$sexe_hors_modalites, 2L)
})
verifier("C16, P8 : n = lignes conservées + nombre_lignes_exclues ; lignes de propagation = setdiff(1:n, conservées et lignes listées par motif) ; resultat$exclusions compte les lignes qui portent le motif", {
  rd <- X_P1$etapes$donnees$resultat
  identical(nrow(B_P1), length(rd$numeros_conserves) + rd$nombre_lignes_exclues) &&
    identical(setdiff(seq_len(nrow(B_P1)), c(rd$numeros_conserves, unlist(rd$lignes_exclues))), 2:3) &&
    identical(rd$exclusions, data.frame(motif = "type_ligne_manquant", nombre = 1L, stringsAsFactors = FALSE)) &&
    identical(rd$lignes_exclues, list(type_ligne_manquant = 1L))
})
verifier("C16, P9 : une propagation qui vide une date de grille lève inventaire_sans_ligne, variante « toutes exclues »", {
  d <- BS[-(7:8), ]; d$t[1L] <- NA
  r <- rf(dn(d, "type_ligne_manquant"))
  isTRUE(controle(r, "donnees", "inventaire_sans_ligne")) &&
    grepl("2016-12-31 (1 ligne(s) reçue(s) à cette date d'inventaire, toutes exclues)", conditionMessage(r), fixed = TRUE)
})
verifier("C14 étendu : une permutation des lignes d'une base avec propagation donne les mêmes tables, les mêmes relevés et les mêmes lignes exclues (en ensemble) (identité exacte)", {
  perm <- c(12L, 3L, 9L, 1L, 6L, 10L, 2L, 8L, 5L, 11L, 4L, 7L)
  pl <- plan_st(exclusions = "type_ligne_manquant", sup = BRUT3)
  x1 <- construire_table(B_P1, RS, pl); x2 <- construire_table(B_P1[perm, ], RS, pl)
  ex <- function(x) setdiff(seq_len(12L), x$etapes$donnees$resultat$numeros_conserves)
  identical(x1$etapes$exposition$resultat, x2$etapes$exposition$resultat) &&
    identical(x1$etapes$brut$resultat, x2$etapes$brut$resultat) &&
    identical(x1$etapes$donnees$diagnostics$releves, x2$etapes$donnees$diagnostics$releves) &&
    identical(sort(perm[ex(x2)]), ex(x1)) && identical(x1$avertissements, x2$avertissements)
})

# --- relevés (§ 7.5 ; C6 sur la base construite à la main) ------------------------

verifier("relevés des stocks, base BS : entrees_entre_inventaires et evenements_hors_fenetre (donnees), entres_sortis_entre_inventaires (exposition), décomptes et textes selon le mode ; aucun relevé de propagation (valeur écrite à la main)", {
  x1 <- construire_table(BS, RS, plan_st("premier_inventaire"))
  x2 <- construire_table(BS, RS, plan_st("milieu"))
  a1 <- avert_s(x1); a2 <- avert_s(x2)
  identical(a1$code, c("entrees_entre_inventaires", "evenements_hors_fenetre", "entres_sortis_entre_inventaires")) &&
    identical(a1$etape, c("donnees", "donnees", "exposition")) && identical(a1$nombre, c(1L, 1L, 1L)) &&
    identical(a1$message[3L], paste("Relevé : 1 unité(s) entrée(s) et sortie(s) entre deux inventaires, sans ligne",
                                    "d'inventaire : sans fenêtre sous l'hypothèse d'entrée « premier_inventaire », sorties",
                                    "écartées, non comptées (deces : 1, rachat : 0, terme : 0, autre : 0).")) &&
    identical(a2$message[3L], paste("Relevé : 1 unité(s) entrée(s) et sortie(s) entre deux inventaires, sans ligne",
                                    "d'inventaire : fenêtre imputée sous l'hypothèse d'entrée « milieu » (entrée au",
                                    "milieu de l'intervalle des instants compatibles), sorties prises en compte,",
                                    "événement de la transition ou sortie concurrente (deces : 1, rachat : 0, terme : 0,",
                                    "autre : 0).")) &&
    identical(a1$message[1L], paste("Relevé : 1 unité(s) vue(s) pour la première fois à un inventaire postérieur au premier",
                                    "(entrée entre deux inventaires, instant d'entrée inconnu : fixé par l'hypothèse",
                                    "d'entrée de l'étape exposition).")) &&
    identical(a1$message[2L], paste("Relevé : 1 unité(s) sans ligne d'inventaire dont l'événement est daté au plus tard du",
                                    "premier inventaire ou après le dernier (hors de la période d'observation, sans",
                                    "exposition ni événement compté).")) &&
    identical(x1$etapes$donnees$diagnostics$releves[c("unites_exclues_par_propagation", "lignes_exclues_par_propagation")],
              list(unites_exclues_par_propagation = 0L, lignes_exclues_par_propagation = 0L))
})
verifier("intervalle_inventaires_long : sous « milieu » seulement, un intervalle de plus de 366 jours (grille de deux dates sur deux ans) ; texte exact", {
  g2 <- G3[c(1L, 3L)]
  d <- BS[!(BS$di %in% G3[2L]) | is.na(BS$di), ]
  d <- d[d$id %in% c("A", "D", "E"), ]
  x2 <- construire_table(d, RS, plan_st("milieu", inventaires = g2))
  x1 <- construire_table(d, RS, plan_st("premier_inventaire", inventaires = g2))
  a <- avert_s(x2)
  identical(a$nombre[a$code == "intervalle_inventaires_long"], 1L) && !("intervalle_inventaires_long" %in% codes(x1)) &&
    identical(a$message[a$code == "intervalle_inventaires_long"],
              paste("Relevé : 1 intervalle(s) entre inventaires de plus de 366 jours sous l'hypothèse d'entrée « milieu »",
                    "(l'écart à l'instant d'entrée inconnu, borné par la moitié de l'intervalle, croît avec sa longueur)."))
})

# --- fenêtres (§ 5 ; C8) et hypothèses (§ 6), fiche (§ 10 ; C9, C10) ------------

verifier("C8 : milieu, t_1 = 2014-12-31, t_2 = 2015-12-31 -> début imputé 2015-07-02 pour l'unité C (vue à t_2) ; premier_inventaire -> 2015-12-31 ; exposition imputée (jours de [â, t_f) et de [â, s)) : 182 + 76 (valeur écrite à la main)", {
  x2 <- construire_table(BS, RS, plan_st("milieu", segmentation = "sexe"))
  x1 <- construire_table(BS, RS, plan_st("premier_inventaire", segmentation = "sexe"))
  e2 <- x2$etapes$exposition$resultat$exposition; e1 <- x1$etapes$exposition$resultat$exposition
  # C, né le 1970-01-01, homme : âge 45 du 2015-07-02 (ou du 2015-12-31) au 2016-01-01.
  identical(e2$exposition_jours[e2$sexe == "H" & e2$age == 45L], 183) &&
    identical(e1$exposition_jours[e1$sexe == "H" & e1$age == 45L], 1) &&
    identical(x2$etapes$exposition$diagnostics$exposition_imputee_jours, 258) &&
    identical(x1$etapes$exposition$diagnostics$exposition_imputee_jours, 0) &&
    identical(x1$etapes$exposition$diagnostics$unites_sans_fenetre, 2L) &&
    identical(x2$etapes$exposition$diagnostics$unites_sans_fenetre, 1L)
})
verifier("C8 : unité ES de sortie s = t_1 + 1 sous « milieu » -> début imputé s, fenêtre vide, décès compté, avertissement evenement_sans_exposition", {
  k <- data.frame(id = "K", t = "evenement", di = NA_D, sx = "H", seg = "a", n = D("1900-06-01"), an = 1900L,
                  s = D("2015-01-01"), c = "deces", stringsAsFactors = FALSE)
  x <- construire_table(rbind(BS, k), RS, plan_st("milieu", segmentation = "sexe"))
  e <- x$etapes$exposition$resultat$exposition
  j <- e$sexe == "H" & e$age == 114L
  identical(e$exposition_jours[j], 0) && identical(e$evenements[j], 1L) && "evenement_sans_exposition" %in% codes(x, "exposition")
})
# Ligne d'une unité ajoutée à une base de test (segment « a »).
lig <- function(id, t, di, sx, n, s = NA_D, c = NA_character_)
  data.frame(id = id, t = t, di = di, sx = sx, seg = "a", n = D(n), an = as.integer(substr(n, 1L, 4L)), s = s, c = c,
             stringsAsFactors = FALSE)
verifier("fenetres_vides (diagnostic de l'étape exposition, sans avertissement ; reprise de (4b), C-5) : P0 sortie le jour D et ES de sortie t_1 + 1 sous « milieu » -> 2 ; sous « premier_inventaire » (ES écartée) -> 1 ; base BS -> 0 (valeur écrite à la main)", {
  z <- rbind(lig("P", "inventaire", G3[1L], "H", "1950-01-01"), lig("P", "evenement", NA_D, "H", "1950-01-01", PER$debut, "rachat"),
             lig("Q", "evenement", NA_D, "F", "1960-01-01", G3[1L] + 1, "deces"))
  b <- rbind(BS, z)
  x2 <- construire_table(b, RS, plan_st("milieu"))
  x1 <- construire_table(b, RS, plan_st("premier_inventaire"))
  identical(x2$etapes$exposition$diagnostics$fenetres_vides, 2L) &&
    identical(x1$etapes$exposition$diagnostics$fenetres_vides, 1L) &&
    identical(construire_table(BS, RS, plan_st("milieu"))$etapes$exposition$diagnostics$fenetres_vides, 0L) &&
    !any(grepl("fenetres_vides", c(codes(x1), codes(x2))))
})
verifier("hypothèses de la recette (§ 6 et A1.6) : neuf lignes, codes dans l'ordre, colonnes code, enonce, nature, statut ; énoncé de l'hypothèse d'entrée selon le mode ; inscrites dans les paramètres de l'étape exposition et dans la fiche (valeur écrite à la main)", {
  x1 <- construire_table(BS, RS, plan_st("premier_inventaire"))
  x2 <- construire_table(BS, RS, plan_st("milieu"))
  h1 <- x1$fiche$hypotheses_recette; h2 <- x2$fiche$hypotheses_recette
  identical(names(h1), c("code", "enonce", "nature", "statut")) &&
    identical(h1$code, c("instant_inventaire", "grille_periode", "inventaires_exhaustifs", "evenements_exhaustifs",
                         "presence_continue", "entree_entre_inventaires", "entres_sortis", "fin_au_dernier_inventaire",
                         "exclusion_par_unite")) &&
    identical(h1$nature, c("convention", "convention", "hypothèse de données", "hypothèse de données",
                           "hypothèse de données", "hypothèse de modèle", "conséquence du mode",
                           "exact sous hypothèses", "convention")) &&
    grepl("« premier_inventaire »", h1$enonce[6L], fixed = TRUE) && grepl("« milieu »", h2$enonce[6L], fixed = TRUE) &&
    identical(h1$statut[9L], "convention ; relevé unites_exclues_par_propagation") &&
    identical(x2$etapes$exposition$parametres$hypotheses_recette, h2) &&
    identical(x2$etapes$exposition$parametres$entree_entre_inventaires, "milieu") &&
    identical(h1[-c(6L, 7L), ], h2[-c(6L, 7L), ]) &&
    identical(h1$statut[5L], paste("hypothèse de données, non vérifiable : une remise en vigueur visible est relevée par les",
                                   "motifs presence_apres_evenement et presence_interrompue (refus par défaut) ; une remise",
                                   "en vigueur entre deux inventaires ne laisse aucune trace")) &&
    identical(h1$enonce[6L], paste("entree_entre_inventaires = « premier_inventaire » : une unité vue pour la première fois à",
                                   "l'inventaire t_f (f >= 2) entre à t_f (troncature à gauche) ; une unité présente à t_1",
                                   "entre à t_1")) &&
    identical(h1$statut[6L], paste("hypothèse de modèle : troncature à gauche non informative à t_f, la même que la",
                                   "troncature au début de la période sous la recette des lignes ; exposition exacte, en",
                                   "jours entiers, pour ce schéma d'observation ; contrôle : constat de simulation")) &&
    identical(h2$statut[6L], paste("approximation de modélisation : écart à l'instant d'entrée inconnu au plus égal à la",
                                   "partie entière de L / 2 jours par unité (L : longueur de l'intervalle des instants",
                                   "compatibles ; majoration certaine, atteinte pour une unité vue pour la première fois",
                                   "à un inventaire) ; contrôle : constat de simulation (effet moyen sur les taux)")) &&
    identical(h2$enonce[7L], paste("unités entrées et sorties entre deux inventaires (sans ligne d'inventaire) : fenêtre",
                                   "imputée sous l'hypothèse « milieu », sorties prises en compte (événement de la",
                                   "transition ou sortie concurrente)"))
})
verifier("C10 : fiche sous les stocks : recette, convention native dernier_anniversaire, unité tete, grille et hypothèse d'entrée dans le plan ; hypotheses_recette absent sous les lignes", {
  x <- construire_table(BS, RS, plan_st("milieu"))
  xl <- construire_table(BL, RL, plan_s(etapes = list(donnees = list(exclusions = character(0)),
                                                      exposition = list(convention = "plus_fine", par_annee = FALSE))))
  f <- x$fiche
  identical(f$recette, "stocks_successifs") && identical(f$convention$native, "dernier_anniversaire") &&
    identical(f$convention$retenue, "dernier_anniversaire") && identical(f$unite_observation, "tete") &&
    identical(f$plan$inventaires, G3 + 0) && identical(f$plan$etapes$exposition$entree_entre_inventaires, "milieu") &&
    !("hypotheses_recette" %in% names(xl$fiche))
})
verifier("C9 : rejouer_fiche() sur une construction en stocks (deux modes, Kaplan-Meier compris) : bilan sans écart", {
  ok <- function(mode) {
    x <- construire_table(BS, RS, plan_st(mode, sup = BRUT3))
    sx <- function(y) { y$execution <- NULL; y }
    r <- rejouer_fiche(BS, lire_fiche(ecrire_fiche(x$fiche)))
    identical(sx(r$construction), sx(x)) && all(r$bilan$verdict[1:8] == "conforme") &&
      all(r$bilan$verdict[9:13] == "identique")
  }
  ok("premier_inventaire") && ok("milieu")
})
verifier("convention forcée age_atteint_annee admise sous les stocks (forçage vers une convention plus grossière, avertissement convention_forcee) ; par_annee admis", {
  x <- construire_table(BS, RS, plan_st("milieu", convention = "age_atteint_annee", par_annee = TRUE,
                                        sup = list(brut = list(estimateurs = "poisson", niveau = 0.95))))
  isTRUE(x$fiche$convention$forcee) && "convention_forcee" %in% codes(x, "exposition") &&
    "annee" %in% names(x$etapes$exposition$resultat$exposition)
})

# --- C16-oracle : base privée des unités exclues (base construite à la main) -----

# Base BO : BS augmentée de cinq unités (G : P0 en cours ; H : vue à t_3 ; I : P0,
# décès entre t_2 et t_3 ; J : vue à t_2, en cours ; K : ES, rachat entre t_2
# et t_3). Défauts injectés sur U = {A, B, G, I} : inventaire initial (A),
# intermédiaire (B), terminal (G), événement (I) ; aucune date de grille
# vidée (t_1 : F ; t_2 : C, J ; t_3 : C, H, J).
BO <- rbind(BS,
            lig("G", "inventaire", G3, "F", "1948-04-04"),
            lig("H", "inventaire", G3[3L], "H", "1975-11-30"),
            lig("I", "inventaire", G3[1:2], "H", "1940-08-08"), lig("I", "evenement", NA_D, "H", "1940-08-08", D("2016-09-09"), "deces"),
            lig("J", "inventaire", G3[2:3], "F", "1965-02-28"),
            lig("K", "evenement", NA_D, "F", "1958-12-12", D("2016-04-04"), "rachat"))
BO_D <- BO
BO_D$t[BO_D$id == "A" & BO_D$di %in% G3[1L]] <- NA                                     # inventaire initial
BO_D$sx[which(BO_D$id == "B" & BO_D$di %in% G3[2L])] <- "X"                              # intermédiaire
BO_D$di[which(BO_D$id == "G" & BO_D$di %in% G3[3L])] <- D("2016-06-30")                  # terminal (hors grille)
BO_D$c[BO_D$id == "I" & BO_D$t %in% "evenement"] <- "Deces"                                # événement
EXC_O <- c("type_ligne_manquant", "sexe_hors_modalites", "inventaire_hors_grille", "cause_hors_modalites")
PRIVEE <- BO[!(BO$id %in% c("A", "B", "G", "I")), ]
verifier("C16-oracle : défauts de ligne injectés sur U = {A, B, G, I} et exclus -> tables d'exposition et de brut identical() à celles de la base privée des lignes de U, sans exclusion ; deux modes, deux conventions, par_annee TRUE et FALSE ; relevé 4 unités (identité exacte)", {
  e <- character(0)
  for (mode in c("premier_inventaire", "milieu")) for (cv in c("plus_fine", "age_atteint_annee")) for (pa in c(FALSE, TRUE)) {
    sup <- list(brut = list(estimateurs = if (pa) "poisson" else c("poisson", "binomial", "kaplan_meier"), niveau = 0.95))
    xa <- construire_table(BO_D, RS, plan_st(mode, EXC_O, sup, convention = cv, par_annee = pa))
    xb <- construire_table(PRIVEE, RS, plan_st(mode, character(0), sup, convention = cv, par_annee = pa))
    if (!identical(xa$etapes$exposition$resultat, xb$etapes$exposition$resultat) ||
        !identical(xa$etapes$brut$resultat, xb$etapes$brut$resultat) ||
        !identical(xa$etapes$donnees$diagnostics$releves$unites_exclues_par_propagation, 4L))
      e <- c(e, paste(mode, cv, pa))
  }
  if (length(e)) paste("écart :", paste(e, collapse = " | ")) else TRUE
})

# --- oracle C1 (lignes reconstituées dans le test), C3, C6 : jeux simulés ---------

# Projection en stocks d'un jeu simulé en lignes (dans le test, indépendante du
# moteur ; § 11) : une ligne d'inventaire par (unité, t_i) avec e <= t_i < s
# (s = +Inf en cours), une ligne d'événement par sortie finie t_1 < s <= t_M.
PER_C1 <- list(debut = D("2015-01-01"), fin = D("2017-12-31"))
G_C1 <- D(c("2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31"))
LOI_C1 <- local({
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
})
PF_C1 <- list(mode = "uniforme", n = 3000, proportion_hommes = 0.5, age_min = 30, age_max = 80, origine = D("2010-01-01"))
projeter <- function(j, g) {
  e <- as.numeric(j$date_entree); s <- as.numeric(j$date_sortie); s[j$cause_sortie == "en_cours"] <- Inf
  gi <- as.numeric(g); M <- length(gi)
  inv <- do.call(rbind, lapply(seq_len(M), function(k) {
    i <- which(e <= gi[k] & gi[k] < s)
    data.frame(id = j$id_tete[i], t = "inventaire", di = g[k], sx = j$sexe[i], n = j$date_naissance[i], s = NA_D,
               c = NA_character_, stringsAsFactors = FALSE)
  }))
  i <- which(is.finite(s) & s > gi[1L] & s <= gi[M])
  evt <- data.frame(id = j$id_tete[i], t = "evenement", di = NA_D, sx = j$sexe[i], n = j$date_naissance[i],
                    s = j$date_sortie[i], c = j$cause_sortie[i], stringsAsFactors = FALSE)
  d <- rbind(inv, evt)
  d[order(d$id, d$t == "evenement", d$di, method = "radix"), , drop = FALSE]
}
RS_C1 <- c(id_tete = "id", sexe = "sx", date_naissance = "n", type_ligne = "t", date_inventaire = "di",
           date_sortie = "s", cause_sortie = "c")
RL_C1 <- c(id_tete = "id", sexe = "sx", date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
# Reconstitution des lignes depuis la forme stocks (dans le test, ADR 0014,
# option (I) écartée pour le moteur) : entrée = â selon le mode (§ 5), unités
# ES écartées (premier_inventaire) ou gardées (milieu), unités HF écartées,
# unités présentes au dernier inventaire en cours ; lignes rangées par
# identifiant (ordre des unités de la recette).
reconstituer <- function(st, g, mode) {
  gi <- as.numeric(g); M <- length(gi)
  ids <- unique(st$id); ids <- ids[order(ids, method = "radix")]
  res <- lapply(ids, function(u) {
    l <- st[st$id == u, ]
    di <- sort(as.numeric(l$di[l$t == "inventaire"]))
    ev <- l[l$t == "evenement", ]
    s <- if (nrow(ev)) as.numeric(ev$s) else NA_real_
    if (length(di)) {
      f <- match(di[1L], gi)
      a <- if (f == 1L) gi[1L] else if (mode == "milieu") gi[f - 1L] + ceiling((gi[f] - gi[f - 1L]) / 2) else gi[f]
    } else {
      if (!(s > gi[1L] && s <= gi[M]) || mode != "milieu") return(NULL)
      ii <- max(which(gi < s)); a <- gi[ii] + ceiling((s - gi[ii]) / 2)
    }
    data.frame(id = u, sx = l$sx[1L], n = l$n[1L], e = structure(a, class = "Date"),
               s = if (nrow(ev)) ev$s else NA_D, c = if (nrow(ev)) ev$c else "en_cours", stringsAsFactors = FALSE)
  })
  do.call(rbind, res)
}
plan_c1 <- function(roles_stocks, mode, cv, pa, inventaires) {
  sup <- list(brut = list(estimateurs = if (pa) "poisson" else c("poisson", "binomial", "kaplan_meier"), niveau = 0.95))
  p <- list(transition = VD, periode = PER_C1, delai_declaration = 0)
  if (roles_stocks) p$inventaires <- inventaires
  ex <- list(convention = cv, par_annee = pa)
  if (roles_stocks) ex$entree_entre_inventaires <- mode
  c(p, list(segmentation = "sexe", etapes = c(list(donnees = list(exclusions = character(0)), exposition = ex), sup)))
}
JEUX_C1 <- lapply(c(180L, 181L), function(gr) simuler_portefeuille(LOI_C1, PF_C1, PER_C1, gr))
ST_C1 <- lapply(JEUX_C1, projeter, g = G_C1)
verifier("oracle C1 : tables d'exposition et de brut d'une construction en stocks identical() à celles d'une construction en lignes reconstituées dans le test (deux jeux simulés, graines 180L et 181L ; deux modes ; deux conventions ; par_annee TRUE et FALSE)", {
  e <- character(0)
  for (k in seq_along(ST_C1)) for (mode in c("premier_inventaire", "milieu"))
    for (cv in c("plus_fine", "age_atteint_annee")) for (pa in c(FALSE, TRUE)) {
      xs <- construire_table(ST_C1[[k]], RS_C1, plan_c1(TRUE, mode, cv, pa, G_C1))
      xl <- construire_table(reconstituer(ST_C1[[k]], G_C1, mode), RL_C1, plan_c1(FALSE, mode, cv, pa, G_C1))
      if (!identical(xs$etapes$exposition$resultat, xl$etapes$exposition$resultat) ||
          !identical(xs$etapes$brut$resultat, xl$etapes$brut$resultat))
        e <- c(e, paste(k, mode, cv, pa))
    }
  if (length(e)) paste("écart :", paste(e, collapse = " | ")) else TRUE
})
verifier("C3 : sous premier_inventaire, dans chaque cellule, E^c et D de la construction en stocks au plus ceux de la construction en lignes du jeu propre (inégalité exacte, cellules appariées par clé)", {
  ok <- vapply(seq_along(JEUX_C1), function(k) {
    xs <- construire_table(ST_C1[[k]], RS_C1, plan_c1(TRUE, "premier_inventaire", "plus_fine", FALSE, G_C1))
    xl <- construire_table(transform(JEUX_C1[[k]], id = id_tete, sx = sexe, n = date_naissance, e = date_entree,
                                     s = date_sortie, c = cause_sortie), RL_C1,
                           plan_c1(FALSE, "premier_inventaire", "plus_fine", FALSE, G_C1))
    ts <- xs$etapes$exposition$resultat$exposition; tl <- xl$etapes$exposition$resultat$exposition
    k_l <- match(paste(ts$sexe, ts$age), paste(tl$sexe, tl$age))
    !anyNA(k_l) && all(ts$exposition_centrale <= tl$exposition_centrale[k_l]) && all(ts$evenements <= tl$evenements[k_l]) &&
      sum(ts$exposition_centrale) < sum(tl$exposition_centrale)
  }, NA)
  all(ok)
})
verifier("C6 : relevés de la recette sur la forme stocks propre égaux aux décomptes tirés du jeu en lignes (P0, E, ES par cause, présents au dernier inventaire) ; propagation 0 unité et 0 ligne, sans avertissement (valeur calculée dans le test, indépendamment du moteur)", {
  ok <- vapply(seq_along(JEUX_C1), function(k) {
    j <- JEUX_C1[[k]]; gi <- as.numeric(G_C1); M <- length(gi)
    e <- as.numeric(j$date_entree); s <- as.numeric(j$date_sortie); s[j$cause_sortie == "en_cours"] <- Inf
    present <- sapply(gi, function(t) e <= t & t < s)
    p0 <- present[, 1L]; vu <- rowSums(present) > 0
    es <- !vu & is.finite(s) & s > gi[1L] & s <= gi[M]
    x <- construire_table(ST_C1[[k]], RS_C1, plan_c1(TRUE, "milieu", "plus_fine", FALSE, G_C1))
    r <- x$etapes$donnees$diagnostics$releves
    causes <- c("deces", "rachat", "terme", "autre")
    identical(r$p0, sum(p0)) && identical(r$entrees, sum(vu & !p0)) && identical(r$entres_sortis, sum(es)) &&
      identical(unname(r$entres_sortis_par_cause), vapply(causes, function(cz) sum(es & j$cause_sortie == cz), integer(1L),
                                                          USE.NAMES = FALSE)) &&
      identical(r$inventaires$presents[M], sum(present[, M])) && identical(r$inventaires$presents, as.integer(colSums(present))) &&
      identical(r$unites_exclues_par_propagation, 0L) && identical(r$lignes_exclues_par_propagation, 0L) &&
      !("unites_exclues_par_propagation" %in% codes(x)) && r$entres_sortis > 0L && r$entrees > 0L
  }, NA)
  all(ok)
})

# =============================================================================
# Commit (4c) : forme « stocks successifs » du simulateur ; compléments de
# couverture (visa d'actuary de (4b)).
# =============================================================================

# --- forme « stocks successifs » du simulateur (§ 11 ; C11) ---------------------

FORME_C1 <- list(nom = "stocks_successifs", inventaires = G_C1)
RS_C1_SIM <- c(id_tete = "id_tete", type_ligne = "type_ligne", date_inventaire = "date_inventaire", sexe = "sexe",
               date_naissance = "date_naissance", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
sim_st <- function(forme = FORME_C1, graine = 180L, portefeuille = PF_C1, periode = PER_C1, defauts = NULL)
  simuler_portefeuille(LOI_C1, portefeuille, periode, graine, defauts = defauts, forme = forme)
SIM_C1 <- lapply(c(180L, 181L), function(gr) sim_st(graine = gr))
COLS_SIM <- c("id_contrat", "id_tete", "type_ligne", "date_inventaire", "sexe", "date_naissance", "date_sortie",
              "cause_sortie")
verifier("C11 (identité exacte) : deux appels en forme stocks identical() ; attribut simulation = celui du jeu propre (forme lignes, même graine) plus le champ forme en fin (nom, grille en Date à stockage double, décomptes)", {
  s0 <- attr(JEUX_C1[[1L]], "simulation"); s1 <- attr(SIM_C1[[1L]], "simulation")
  gi <- structure(as.integer(unclass(G_C1)), class = "Date")
  identical(sim_st(), SIM_C1[[1L]]) &&
    identical(names(s1), c(names(s0), "forme")) && identical(s1[names(s0)], s0) &&
    identical(names(s1$forme), c("nom", "inventaires", "decomptes")) && identical(s1$forme$nom, "stocks_successifs") &&
    identical(s1$forme$inventaires, G_C1 + 0) &&
    identical(sim_st(list(nom = "stocks_successifs", inventaires = gi)), SIM_C1[[1L]])
})
verifier("C11 : colonnes, types et ordre de la spécification, § 11 (id_contrat, id_tete égaux ; type_ligne ; dates manquantes en NA de classe Date à stockage double, causes manquantes en NA caractère ; ordre par id_contrat puis date de ligne ; lignes numérotées 1..n)", {
  st <- SIM_C1[[1L]]
  inv <- st$type_ligne == "inventaire"
  date_ligne <- ifelse(inv, as.numeric(st$date_inventaire), as.numeric(st$date_sortie))
  identical(names(st), COLS_SIM) && is.integer(st$id_contrat) && identical(st$id_contrat, st$id_tete) &&
    is.character(st$type_ligne) && all(st$type_ligne %in% c("inventaire", "evenement")) &&
    identical(class(st$date_inventaire), "Date") && is.double(unclass(st$date_inventaire)) &&
    identical(class(st$date_sortie), "Date") && is.double(unclass(st$date_sortie)) &&
    identical(class(st$date_naissance), "Date") && is.character(st$sexe) && is.character(st$cause_sortie) &&
    all(is.na(st$date_sortie[inv])) && all(is.na(st$cause_sortie[inv])) && !anyNA(st$date_inventaire[inv]) &&
    all(is.na(st$date_inventaire[!inv])) && !anyNA(st$date_sortie[!inv]) && !anyNA(st$cause_sortie[!inv]) &&
    identical(order(st$id_contrat, date_ligne, method = "radix"), seq_len(nrow(st))) &&
    identical(attr(st, "row.names"), seq_len(nrow(st)))
})
verifier("C11 (identité exacte) : forme stocks du simulateur identical() à la projection du jeu propre écrite dans le test (projeter(), comparaisons de dates e <= t_i < s et t_1 < s <= t_M), deux graines", {
  ok <- vapply(seq_along(SIM_C1), function(k) {
    st <- SIM_C1[[k]]
    a <- data.frame(id = st$id_tete, t = st$type_ligne, di = st$date_inventaire, sx = st$sexe, n = st$date_naissance,
                    s = st$date_sortie, c = st$cause_sortie, stringsAsFactors = FALSE)
    b <- ST_C1[[k]]; rownames(b) <- NULL
    identical(a, b)
  }, NA)
  all(ok)
})
verifier("C11 : décomptes de l'attribut simulation (lignes de chaque type, P0, E, ES par cause, présents au dernier inventaire) égaux à ceux recomptés dans le test sur le jeu propre (valeur calculée dans le test) ; aucune unité HF écrite (sortie au plus tard t_1 : non écrite)", {
  ok <- vapply(seq_along(SIM_C1), function(k) {
    j <- JEUX_C1[[k]]; st <- SIM_C1[[k]]; dc <- attr(st, "simulation")$forme$decomptes
    gi <- as.numeric(G_C1); M <- length(gi)
    e <- as.numeric(j$date_entree); s <- as.numeric(j$date_sortie); s[is.na(s)] <- Inf
    present <- sapply(gi, function(t) e <= t & t < s)
    vu <- rowSums(present) > 0
    es <- !vu & is.finite(s) & s > gi[1L] & s <= gi[M]
    ecrits <- unique(st$id_contrat)
    identical(names(dc), c("lignes", "p0", "entrees", "entres_sortis", "presents_dernier")) &&
      identical(dc$lignes, c(inventaire = sum(present), evenement = sum(st$type_ligne == "evenement"))) &&
      identical(dc$p0, sum(present[, 1L])) && identical(dc$entrees, sum(vu & !present[, 1L])) &&
      identical(dc$entres_sortis, c(deces = sum(es & j$cause_sortie == "deces"), rachat = sum(es & j$cause_sortie == "rachat"))) &&
      identical(dc$presents_dernier, sum(present[, M])) &&
      identical(sort(ecrits), j$id_contrat[vu | (is.finite(s) & s > gi[1L])]) && sum(es) > 0L
  }, NA)
  all(ok)
})
verifier("C11 : instant d'une date d'inventaire dans la forme du simulateur (§ 2.1) : unités entrées le jour t_2 présentes à t_2 si en vigueur ce soir-là, aucune présente à t_1 (mode age_exact, date_entree = t_2) ; la sortie le jour t_i est couverte par le test direct de projeter_stocks()", {
  pf <- list(mode = "age_exact", n = 400, proportion_hommes = 0.5, age = 60, date_entree = G_C1[2L])
  st <- sim_st(portefeuille = pf, graine = 183L)
  j <- simuler_portefeuille(LOI_C1, pf, PER_C1, 183L)
  inv <- st[st$type_ligne == "inventaire", ]
  # Toute unité entrée le 2015-12-31 et en vigueur ce soir-là est présente à
  # t_2 ; aucune n'est présente à t_1.
  vig <- j$id_contrat[is.na(j$date_sortie) | j$date_sortie > G_C1[2L]]
  length(vig) > 0L && identical(sort(inv$id_contrat[inv$date_inventaire == G_C1[2L]]), vig) &&
    !any(inv$date_inventaire == G_C1[1L])
})
# Jeu propre écrit à la main, grille G_C1 (t_1 = 2014-12-31 ... t_4 =
# 2017-12-31) : 1, sortie le jour t_1 (aucune ligne) ; 2, sortie le jour t_M
# (inventaires t_1 à t_3, puis l'événement) ; 3, sortie le jour t_2
# (inventaire t_1, puis l'événement) ; 4, entrée le jour t_2, en cours
# (inventaires t_2 à t_4) ; 5, entrée et sortie entre t_2 et t_3 (événement
# seul) ; 6, entrée entre t_1 et t_2, en cours (inventaires t_2 à t_4).
verifier("projeter_stocks() en appel direct (valeurs écrites à la main) : s = t_1 sans ligne, s = t_i intermédiaire, s = t_M, e = t_i présent à t_i, entrée-sortie entre deux inventaires (événement seul), unité en cours ; lignes, ordre et décomptes", {
  j <- data.frame(id_contrat = 1:6, id_tete = 1:6, sexe = "H", date_naissance = D("1960-01-01"),
                  date_entree = D(c("2010-01-01", "2010-01-01", "2010-01-01", "2015-12-31", "2016-01-01", "2015-03-03")),
                  date_sortie = D(c("2014-12-31", "2017-12-31", "2015-12-31", NA, "2016-06-01", NA)),
                  cause_sortie = c("deces", "rachat", "deces", "en_cours", "deces", "en_cours"), stringsAsFactors = FALSE)
  r <- tablexp:::projeter_stocks(j, G_C1)
  l <- r$lignes
  identical(l$id_contrat, c(2L, 2L, 2L, 2L, 3L, 3L, 4L, 4L, 4L, 5L, 6L, 6L, 6L)) &&
    identical(l$type_ligne, c("inventaire", "inventaire", "inventaire", "evenement", "inventaire", "evenement",
                              "inventaire", "inventaire", "inventaire", "evenement", "inventaire", "inventaire", "inventaire")) &&
    identical(l$date_inventaire, D(c("2014-12-31", "2015-12-31", "2016-12-31", NA, "2014-12-31", NA,
                                     "2015-12-31", "2016-12-31", "2017-12-31", NA, "2015-12-31", "2016-12-31", "2017-12-31"))) &&
    identical(l$date_sortie, D(c(NA, NA, NA, "2017-12-31", NA, "2015-12-31", NA, NA, NA, "2016-06-01", NA, NA, NA))) &&
    identical(l$cause_sortie, c(NA, NA, NA, "rachat", NA, "deces", NA, NA, NA, "deces", NA, NA, NA)) &&
    identical(r$decomptes, list(lignes = c(inventaire = 10L, evenement = 3L), p0 = 2L, entrees = 2L,
                                entres_sortis = c(deces = 1L, rachat = 0L), presents_dernier = 2L))
})
verifier("C11 (refus écrits à la main) : forme stocks avec defauts, mode multi-contrats, grille non alignée (premier, dernier), grille mal formée (classe, une date, NA, non croissante, non entière, attribut), champ manquant ou inconnu -> forme_invalide, module simulation, avant tout tirage (aucun appel de sous_graine())", {
  dft <- list(graine = 170L, declarations = list(list(type = "sexe_manquant", cible = "deces", nombre = 1L)))
  pf_multi <- c(PF_C1, list(loi_contrats = c(0.5, 0.5))); pf_multi$mode <- "uniforme_multi"
  fg <- function(g) list(nom = "stocks_successifs", inventaires = g)
  ns <- asNamespace("tablexp"); cpt <- new.env(); cpt$n <- 0L
  suppressMessages(trace("sous_graine", tracer = bquote(assign("n", get("n", envir = .(cpt)) + 1L, envir = .(cpt))),
                         where = ns, print = FALSE))
  cas <- tryCatch(list(
    defauts = rf(sim_st(defauts = dft)),
    multi = rf(sim_st(portefeuille = pf_multi)),
    premier = rf(sim_st(fg(G_C1[-1L]))),
    dernier = rf(sim_st(fg(G_C1[-4L]))),
    classe = rf(sim_st(fg(as.numeric(G_C1)))),
    posixct = rf(sim_st(fg(as.POSIXct(G_C1)))),
    une_date = rf(sim_st(fg(G_C1[1L]))),
    na = rf(sim_st(fg(c(G_C1[1:2], NA, G_C1[4L])))),
    croissance = rf(sim_st(fg(G_C1[c(1L, 3L, 2L, 4L)]))),
    entiere = rf(sim_st(fg(structure(c(unclass(G_C1)[1:2], unclass(G_C1)[3L] + 0.5, unclass(G_C1)[4L]), class = "Date")))),
    attribut = rf(sim_st(fg(structure(G_C1, names = letters[1:4])))),
    champ_manquant = rf(sim_st(list(nom = "stocks_successifs"))),
    champ_inconnu = rf(sim_st(c(FORME_C1, list(par_annee = FALSE))))),
    finally = suppressMessages(untrace("sous_graine", where = ns)))
  ko <- names(cas)[!vapply(cas, function(r) isTRUE(controle(r, "simulation", "forme_invalide")), NA)]
  msg <- function(k) conditionMessage(cas[[k]])
  if (!identical(cpt$n, 0L)) sprintf("%d appel(s) de sous_graine() pendant les refus", cpt$n)
  else if (length(ko)) paste("sans le refus attendu :", paste(ko, collapse = ", "))
  else if (!grepl("defauts", msg("defauts"), fixed = TRUE) ||
           !grepl("(tête ou contrat) serait ambiguë ; la fusion par tête des contrats inventoriés", msg("multi"), fixed = TRUE) ||
           !identical(msg("premier"), paste("Forme du jeu simulé : inventaires : grille non alignée sur la période (premier",
                                            "inventaire 2015-12-31, dernier 2017-12-31) ; la première date doit être la veille",
                                            "du début (2014-12-31) et la dernière la fin de la période (2017-12-31), le simulateur",
                                            "n'ayant pas de délai de déclaration.")) ||
           !identical(cas$dernier$details$attendu, D(c("2014-12-31", "2017-12-31"))) ||
           !grepl("strictement croissantes", msg("croissance"), fixed = TRUE)) "message ou details sans la raison nommée"
  else TRUE
})

# --- fenêtres sur une grille irrégulière, délai de 31 jours (valeurs écrites à la main)

# Grille 2014-12-31, 2015-12-31, 2016-12-31, 2018-11-30 (intervalles de 365,
# 366 et 699 jours) ; période 2015-01-01 à 2018-12-31, délai 31 jours : fin
# effective 2018-11-30 = t_4, D = 2015-01-01, B = 2018-12-01. Unités :
#   P0a : t_1 à t_4, en cours ; P0b : t_1, t_2, décès 2016-03-15 ;
#   P0c : t_1 à t_4, décès 2018-12-10 (après t_M : fin C, non compté) ;
#   P0d : t_1, décès 2015-01-01 (sortie le jour D : fenêtre vide, compté) ;
#   E1 : t_2 à t_4, en cours ; E2 : t_3, décès 2017-05-05 ; E3 : t_4 ;
#   ES1 : décès 2016-01-01 = t_2 + 1 ; ES2 : rachat 2017-02-10 ;
#   ES3 : décès 2015-06-01 ; ES4 : rachat 2018-11-30 = t_M ;
#   HF1 : décès 2014-12-31 = t_1 ; HF2 : décès 2018-12-05 > t_M.
G_IR <- D(c("2014-12-31", "2015-12-31", "2016-12-31", "2018-11-30"))
PER_IR <- list(debut = D("2015-01-01"), fin = D("2018-12-31"))
inv_ir <- function(id, k, n) data.frame(id = id, t = "inventaire", di = G_IR[k], sx = "H", n = D(n), s = NA_D,
                                        c = NA_character_, stringsAsFactors = FALSE)
evt_ir <- function(id, s, c, n) data.frame(id = id, t = "evenement", di = NA_D, sx = "H", n = D(n), s = D(s), c = c,
                                           stringsAsFactors = FALSE)
B_IR <- rbind(inv_ir("P0a", 1:4, "1950-01-01"),
              inv_ir("P0b", 1:2, "1951-02-02"), evt_ir("P0b", "2016-03-15", "deces", "1951-02-02"),
              inv_ir("P0c", 1:4, "1952-03-03"), evt_ir("P0c", "2018-12-10", "deces", "1952-03-03"),
              inv_ir("P0d", 1L, "1953-04-04"), evt_ir("P0d", "2015-01-01", "deces", "1953-04-04"),
              inv_ir("E1", 2:4, "1960-01-01"),
              inv_ir("E2", 3L, "1961-01-01"), evt_ir("E2", "2017-05-05", "deces", "1961-01-01"),
              inv_ir("E3", 4L, "1962-01-01"),
              evt_ir("ES1", "2016-01-01", "deces", "1970-01-01"), evt_ir("ES2", "2017-02-10", "rachat", "1971-01-01"),
              evt_ir("ES3", "2015-06-01", "deces", "1972-01-01"), evt_ir("ES4", "2018-11-30", "rachat", "1973-01-01"),
              evt_ir("HF1", "2014-12-31", "deces", "1980-01-01"), evt_ir("HF2", "2018-12-05", "deces", "1981-01-01"))
plan_ir <- function(mode) list(transition = VD, periode = PER_IR, delai_declaration = 31, inventaires = G_IR,
                               segmentation = "sexe",
                               etapes = c(list(donnees = list(exclusions = character(0)),
                                               exposition = list(convention = "plus_fine", par_annee = FALSE,
                                                                 entree_entre_inventaires = mode)), BRUT3))
# Fenêtres (a, b) et indicateur d'événement des observations gardées, par
# l'aiguillage de l'étape exposition (objet fenetres_exposition).
fenetres_st <- function(x, roles, plan) {
  ob <- x$etapes$donnees$resultat$observations
  pp <- tablexp:::valeurs_parametres_donnees(plan$transition, plan$periode, plan$delai_declaration, plan$inventaires)
  rc <- tablexp:::deduire_recette(roles)
  f <- tablexp:::calculer_exposition(ob, pp, plan$segmentation, rc, x$etapes$exposition$parametres$convention,
                                     plan$etapes$exposition$par_annee, plan$etapes$exposition$entree_entre_inventaires)$fenetres
  list(observations = ob, a = f$a, b = f$b, evenement = f$evenement)
}
verifier("grille irrégulière, délai 31 jours (valeurs écrites à la main) : fenêtres a = max(â, D), b = min(ŝ, B) des unités gardées, événements comptés, dans les deux modes ; E sous milieu : 2015-07-02, 2016-07-01, 2017-12-16 ; ES à s = t_M ; P0 sortie le jour D ; P0 à événement après t_M", {
  x2 <- construire_table(B_IR, RS_C1, plan_ir("milieu")); x1 <- construire_table(B_IR, RS_C1, plan_ir("premier_inventaire"))
  f2 <- fenetres_st(x2, RS_C1, plan_ir("milieu")); f1 <- fenetres_st(x1, RS_C1, plan_ir("premier_inventaire"))
  dd <- function(...) as.numeric(D(c(...)))
  # milieu : E1, E2, E3, ES1, ES2, ES3, ES4, P0a, P0b, P0c, P0d.
  a2 <- dd("2015-07-02", "2016-07-01", "2017-12-16", "2016-01-01", "2017-01-21", "2015-03-17", "2017-12-16",
           "2015-01-01", "2015-01-01", "2015-01-01", "2015-01-01")
  b2 <- dd("2018-12-01", "2017-05-05", "2018-12-01", "2016-01-01", "2017-02-10", "2015-06-01", "2018-11-30",
           "2018-12-01", "2016-03-15", "2018-12-01", "2015-01-01")
  e2 <- c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  # premier_inventaire : E1, E2, E3, P0a, P0b, P0c, P0d (ES écartées).
  a1 <- dd("2015-12-31", "2016-12-31", "2018-11-30", "2015-01-01", "2015-01-01", "2015-01-01", "2015-01-01")
  b1 <- dd("2018-12-01", "2017-05-05", "2018-12-01", "2018-12-01", "2016-03-15", "2018-12-01", "2015-01-01")
  e1 <- c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)
  ob <- f2$observations
  identical(ob$unite, c("E1", "E2", "E3", "ES1", "ES2", "ES3", "ES4", "HF1", "HF2", "P0a", "P0b", "P0c", "P0d")) &&
    identical(ob$classe, c("E", "E", "E", "ES", "ES", "ES", "ES", "HF", "HF", "P0", "P0", "P0", "P0")) &&
    identical(ob$fin, c("C", "S", "C", NA, NA, NA, NA, NA, NA, "C", "S", "C", "S")) &&
    identical(f2$a, a2) && identical(f2$b, b2) && identical(f2$evenement, e2) &&
    identical(f1$a, a1) && identical(f1$b, b1) && identical(f1$evenement, e1)
})
verifier("grille irrégulière, délai 31 jours (valeurs écrites à la main) : diagnostics de l'étape exposition (exposition imputée 182 + 183 + 349 + 0 + 20 + 76 + 349 = 1159 jours sous milieu ; unités sans fenêtre ; fenêtres vides ; événements) et relevés de l'étape donnees ; un intervalle de plus de 366 jours relevé sous milieu", {
  x2 <- construire_table(B_IR, RS_C1, plan_ir("milieu")); x1 <- construire_table(B_IR, RS_C1, plan_ir("premier_inventaire"))
  d2 <- x2$etapes$exposition$diagnostics; d1 <- x1$etapes$exposition$diagnostics
  r <- x2$etapes$donnees$diagnostics$releves
  identical(d2$exposition_imputee_jours, 1159) && identical(d1$exposition_imputee_jours, 0) &&
    identical(d2$unites_sans_fenetre, 2L) && identical(d1$unites_sans_fenetre, 6L) &&
    identical(d2$fenetres_vides, 2L) && identical(d1$fenetres_vides, 1L) &&
    identical(d2$evenements, 5L) && identical(d1$evenements, 3L) &&
    identical(r[c("unites", "p0", "entrees", "entres_sortis", "hors_fenetre", "fin_sortie", "fin_censure")],
              list(unites = 13L, p0 = 4L, entrees = 3L, entres_sortis = 4L, hors_fenetre = 2L, fin_sortie = 3L,
                   fin_censure = 4L)) &&
    identical(r$entres_sortis_par_cause, c(deces = 2L, rachat = 2L, terme = 0L, autre = 0L)) &&
    identical(r$inventaires$presents, c(4L, 4L, 4L, 4L)) &&
    identical(r$intervalles$entrants, c(1L, 1L, 1L)) && identical(r$intervalles$entres_sortis, c(1L, 1L, 2L)) &&
    identical(x2$avertissements$nombre[x2$avertissements$code == "intervalle_inventaires_long"], 1L) &&
    !("intervalle_inventaires_long" %in% x1$avertissements$code)
})

# --- C2 en test direct (simulateur, deux modes) ----------------------------------

verifier("C2 (identité exacte) : fenêtres (a, b) et événements des unités P0 de la forme stocks du simulateur identical() à ceux des mêmes unités dans la forme en lignes du même jeu propre, deux graines, deux modes", {
  e <- character(0)
  for (k in seq_along(SIM_C1)) {
    j <- JEUX_C1[[k]]
    dl <- data.frame(id = j$id_tete, sx = j$sexe, n = j$date_naissance, e = j$date_entree, s = j$date_sortie,
                     c = j$cause_sortie, stringsAsFactors = FALSE)
    pl_l <- plan_c1(FALSE, "milieu", "plus_fine", FALSE, G_C1)
    fl <- fenetres_st(construire_table(dl, RL_C1, pl_l), RL_C1, pl_l)
    for (mode in c("premier_inventaire", "milieu")) {
      pl_s <- plan_c1(TRUE, mode, "plus_fine", FALSE, G_C1)
      fs <- fenetres_st(construire_table(SIM_C1[[k]], RS_C1_SIM, pl_s), RS_C1_SIM, pl_s)
      ob <- fs$observations
      garde <- ob$classe %in% c("P0", "E", if (mode == "milieu") "ES")
      u_g <- ob$unite[garde]; p0 <- ob$classe[garde] == "P0"
      i_l <- match(u_g[p0], fl$observations$unite)
      if (anyNA(i_l) || sum(p0) < 100L || !identical(fs$a[p0], fl$a[i_l]) || !identical(fs$b[p0], fl$b[i_l]) ||
          !identical(fs$evenement[p0], fl$evenement[i_l]))
        e <- c(e, paste(k, mode))
    }
  }
  if (length(e)) paste("écart :", paste(e, collapse = " | ")) else TRUE
})

# --- C6 contre les décomptes de l'attribut simulation -----------------------------

verifier("C6 : relevés de la recette sur la forme stocks du simulateur égaux aux décomptes de son attribut simulation (P0, E, ES par cause, présents au dernier inventaire, lignes de chaque type) ; causes non simulées à 0 ; propagation 0 unité et 0 ligne, sans avertissement ; deux graines", {
  ok <- vapply(SIM_C1, function(st) {
    dc <- attr(st, "simulation")$forme$decomptes
    x <- construire_table(st, RS_C1_SIM, plan_c1(TRUE, "milieu", "plus_fine", FALSE, G_C1))
    r <- x$etapes$donnees$diagnostics$releves
    M <- length(G_C1)
    identical(r$p0, dc$p0) && identical(r$entrees, dc$entrees) && identical(r$entres_sortis, sum(dc$entres_sortis)) &&
      identical(r$entres_sortis_par_cause[names(dc$entres_sortis)], dc$entres_sortis) &&
      identical(unname(r$entres_sortis_par_cause[c("terme", "autre")]), c(0L, 0L)) &&
      identical(r$inventaires$presents[M], dc$presents_dernier) &&
      identical(sum(r$inventaires$presents), dc$lignes[["inventaire"]]) &&
      identical(r$fin_sortie + r$entres_sortis + r$hors_fenetre, dc$lignes[["evenement"]]) && identical(r$hors_fenetre, 0L) &&
      identical(r$unites_exclues_par_propagation, 0L) && identical(r$lignes_exclues_par_propagation, 0L) &&
      !("unites_exclues_par_propagation" %in% x$avertissements$code)
  }, NA)
  all(ok)
})

# --- C16-oracle sur jeux simulés (A1.8), trois graines -----------------------------

# Défauts de ligne injectés sur la forme stocks du simulateur, deux unités par
# type : inventaire initial (type_ligne manquant sur la ligne t_1 d'une unité
# P0 présente à t_1 et t_2), intermédiaire (sexe « X » sur la ligne t_2 d'une
# unité présente de t_1 à t_3), terminal (date d'inventaire hors grille sur la
# ligne t_M d'une unité présente à t_{M-1} et t_M), événement (cause « Deces »
# sur la ligne d'événement d'une unité P0 décédée). Unités distinctes, choisies
# dans l'ordre des identifiants ; aucune date de grille vidée (plusieurs
# centaines d'unités présentes à chaque inventaire).
injecter_c16 <- function(st, g) {
  u_inv <- function(k) unique(st$id_tete[st$type_ligne == "inventaire" & st$date_inventaire %in% g[k]])
  M <- length(g)
  pris <- integer(0)
  choisir <- function(cands) { c2 <- setdiff(cands, pris)[1:2]; pris <<- c(pris, c2); c2 }
  u1 <- choisir(intersect(u_inv(1L), u_inv(2L)))
  u2 <- choisir(intersect(intersect(u_inv(1L), u_inv(2L)), u_inv(3L)))
  u3 <- choisir(intersect(u_inv(M - 1L), u_inv(M)))
  u4 <- choisir(intersect(u_inv(1L), st$id_tete[st$type_ligne == "evenement" & st$cause_sortie == "deces"]))
  d <- st
  l_inv <- function(u, k) which(d$id_tete %in% u & d$type_ligne == "inventaire" & d$date_inventaire %in% g[k])
  d$type_ligne[l_inv(u1, 1L)] <- NA
  d$sexe[l_inv(u2, 2L)] <- "X"
  d$date_inventaire[l_inv(u3, M)] <- g[M] - 100
  d$cause_sortie[which(d$id_tete %in% u4 & d$type_ligne %in% "evenement")] <- "Deces"
  list(donnees = d, unites = c(u1, u2, u3, u4))
}
EXC_C16 <- c("type_ligne_manquant", "sexe_hors_modalites", "inventaire_hors_grille", "cause_hors_modalites")
verifier("C16-oracle sur jeux simulés (A1.8 ; identité exacte) : défauts de ligne injectés sur U (inventaire initial, intermédiaire, terminal, événement ; 8 unités) et exclus -> tables d'exposition et de brut identical() à celles de la base privée de toutes les lignes de U, sans exclusion ; trois graines, deux modes, deux conventions, par_annee TRUE et FALSE ; relevé de propagation 8 unités", {
  e <- character(0)
  for (gr in c(180L, 181L, 182L)) {
    st <- if (gr == 182L) sim_st(graine = gr) else SIM_C1[[gr - 179L]]
    inj <- injecter_c16(st, G_C1)
    privee <- st[!(st$id_tete %in% inj$unites), ]
    if (length(unique(inj$unites)) != 8L) { e <- c(e, paste(gr, ": U incomplet")); next }
    for (mode in c("premier_inventaire", "milieu")) for (cv in c("plus_fine", "age_atteint_annee")) for (pa in c(FALSE, TRUE)) {
      pl_a <- plan_c1(TRUE, mode, cv, pa, G_C1); pl_a$etapes$donnees$exclusions <- EXC_C16
      xa <- construire_table(inj$donnees, RS_C1_SIM, pl_a)
      xb <- construire_table(privee, RS_C1_SIM, plan_c1(TRUE, mode, cv, pa, G_C1))
      if (!identical(xa$etapes$exposition$resultat, xb$etapes$exposition$resultat) ||
          !identical(xa$etapes$brut$resultat, xb$etapes$brut$resultat) ||
          !identical(xa$etapes$donnees$diagnostics$releves$unites_exclues_par_propagation, 8L))
        e <- c(e, paste(gr, mode, cv, pa))
    }
  }
  if (length(e)) paste("écart :", paste(e, collapse = " | ")) else TRUE
})
