# -----------------------------------------------------------------------------
#  tests/unitaires/test_simulateur_stocks.R : défauts paramétrés de la forme
#  « stocks successifs » du simulateur, graine propre des défauts (issue
#  #191 ; #176 ; avis de forme d'architect-approfondi, partie #191, et
#  spécification d'actuary-approfondi de #191, critères B1 à B7 ; décision du
#  mainteneur du 11/11/2026 : inventaire perdu, toutes les dates éligibles ;
#  ADR 0007, points 7, 8 et 11.4)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité exacte : appels répétés, lignes non altérées et attribut
#    `simulation` inchangés par les défauts (B1) ;
#  - valeur écrite à la main : motifs levés et lignes exclues (B2, B3,
#    attendus lus dans l'attribut `defauts`, écrits en dur dans le catalogue
#    de la forme, jamais calculés par `donnees`), jeu de six unités écrit à
#    la main (B4), refus (B5), présents sains (B6), clôture (B7).
#  Aucun constat Monte-Carlo (spécification, § 4). Graines dans la sous-plage
#  22 001 265 - 22 001 299 (spécification, § 2, (iii)), une valeur par
#  contexte.
# -----------------------------------------------------------------------------

D <- as.Date
simuler <- tablexp::simuler_portefeuille
GRAINE_JEU <- 22001265L
GRAINES_DEFAUTS <- c(detection = 22001266L, identite = 22001267L, refus = 22001268L, main = 22001269L,
                     perdu_plusieurs = 22001270L, perdu_premier = 22001271L, perdu_milieu = 22001272L,
                     perdu_dernier = 22001281L)

PER <- list(debut = D("2015-01-01"), fin = D("2017-12-31"))
G4 <- D(c("2014-12-31", "2015-12-31", "2016-12-31", "2017-12-31"))
G2 <- G4[c(1L, 4L)]
VD <- list(depart = "vivant", arrivee = "deces")
GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
LOI <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)),
            rachat = list(table = tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %"), facteurs = c(H = 1, F = 1)))
PF <- list(mode = "uniforme", n = 3000, proportion_hommes = 0.5, age_min = 30, age_max = 80, origine = D("2010-01-01"))
FS <- list(nom = "stocks_successifs", inventaires = G4)
RS <- c(id_contrat = "id_contrat", type_ligne = "type_ligne", date_inventaire = "date_inventaire", sexe = "sexe",
        date_naissance = "date_naissance", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
TYPES <- c("sortie_non_renseignee", "presence_interrompue", "inventaire_hors_grille", "inventaire_duplique",
           "inventaire_perdu")
CIBLES <- list(sortie_non_renseignee = c("deces", "rachat"), presence_interrompue = c("deces", "rachat", "en_cours"),
               inventaire_hors_grille = c("deces", "rachat", "en_cours"),
               inventaire_duplique = c("deces", "rachat", "en_cours"))
CAT_ST <- tablexp:::DEFAUTS_FORMES$stocks_successifs$catalogue
CASCADE <- c("presence_interrompue", "evenement_apres_absence", "sortie_non_renseignee")

rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
dft <- function(declarations, graine = GRAINES_DEFAUTS[["detection"]]) list(graine = graine, declarations = declarations)
dc <- function(t, ...) list(type = t, cible = CIBLES[[t]], ...)
plan_st <- function(exclusions = character(0), mode = "milieu")
  list(transition = VD, periode = PER, delai_declaration = 0, inventaires = G4, segmentation = "sexe",
       etapes = list(donnees = list(exclusions = exclusions),
                     exposition = list(convention = "plus_fine", par_annee = FALSE, entree_entre_inventaires = mode),
                     brut = list(estimateurs = "poisson", niveau = 0.95)))
PROPRE <- simuler(LOI, PF, PER, GRAINE_JEU, forme = FS)

# --- B1 : identité exacte -------------------------------------------------------------

verifier("B1 (identité exacte) : deux appels avec défauts identical() ; attribut simulation identique à la forme propre ; lignes ni retirées ni altérées identiques à la projection propre ; copies = valeurs propres des originaux ; .Random.seed de l'appelant inchangé", {
  e <- character(0)
  for (t in TYPES[1:4]) {
    d <- dft(list(dc(t, nombre = 2L)), GRAINES_DEFAUTS[["identite"]])
    set.seed(3L); avant <- .Random.seed
    a1 <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = d, forme = FS)
    graine_ok <- identical(.Random.seed, avant)
    a2 <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = d, forme = FS)
    ad <- attr(a1, "defauts"); l <- ad$lignes
    N <- nrow(PROPRE)
    gardees <- setdiff(seq_len(N), ad$retirees$ligne)
    alt <- l$ligne[l$role == "alteree"]; org <- l$ligne[l$role == "original"]; cop <- l$ligne[l$role == "copie"]
    i_autres <- setdiff(seq_along(gardees), alt)
    lst <- function(x, i) lapply(x, `[`, i)
    ok <- c(appels = identical(a1, a2), graine = graine_ok,
            simulation = identical(attr(a1, "simulation"), attr(PROPRE, "simulation")),
            autres = identical(lst(a1, i_autres), lst(PROPRE, gardees[i_autres])),
            copies = identical(lst(a1, cop), lst(PROPRE, gardees[org])),
            nombre = nrow(a1) == length(gardees) + length(cop),
            colonnes = identical(names(a1), names(PROPRE)),
            attributs = setequal(names(attributes(a1)), c(names(attributes(PROPRE)), "defauts")))
    if (!all(ok)) e <- c(e, paste(t, ":", paste(names(ok)[!ok], collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " | ") else TRUE
})

# --- B2 : détection par le module donnees ---------------------------------------------

# Pour chaque type à motif (nombre = 2) : sans exclusion, refus ligne_invalide
# nommant ce seul motif, aux lignes porteuses de l'attribut ; sous exclusion
# du motif, dans les deux modes d'entrée entre inventaires,
# lignes_exclues[[motif]] = porteuses, nombre_lignes_exclues = porteuses et
# lignes de propagation, unites_exclues_par_propagation =
# decomptes$unites_propagation ; evaluer_motifs_stocks() : seul motif déclaré.
DETECTION <- local({
  e <- character(0); n <- 0L
  p <- tablexp:::valeurs_parametres_donnees(VD, PER, 0, G4)
  for (t in TYPES[1:4]) {
    a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(dc(t, nombre = 2L))), forme = FS)
    ad <- attr(a, "defauts"); l <- ad$lignes
    porteuses <- sort(unique(l$ligne[!is.na(l$motif_attendu)]))
    prop <- sort(l$ligne[l$role == "propagation"])
    r <- rf(construire_table(a, RS, plan_st()))
    m <- tablexp:::evaluer_motifs_stocks(a, RS, p, "sexe")
    ok <- c(attendu = identical(unique(l$motif_attendu[!is.na(l$motif_attendu)]), t) && length(porteuses) >= 2L,
            refus = !is.null(r) && identical(r$motif, "ligne_invalide") && identical(names(r$details$lignes), t) &&
              identical(r$details$lignes[[t]], porteuses),
            motifs = identical(names(m$motifs)[lengths(m$motifs) > 0L], t) &&
              identical(m$lignes_propagation, prop))
    for (mode in c("premier_inventaire", "milieu")) {
      x <- tryCatch(construire_table(a, RS, plan_st(t, mode)), refus_tablexp = function(e) e)
      rs <- if (inherits(x, "construction")) x$etapes$donnees$resultat
      ok[[paste("exclusion", mode)]] <- !is.null(rs) && identical(rs$lignes_exclues[[t]], porteuses) &&
        rs$nombre_lignes_exclues == length(union(porteuses, prop)) &&
        identical(x$etapes$donnees$diagnostics$releves$unites_exclues_par_propagation, ad$decomptes$unites_propagation)
    }
    n <- n + 1L
    if (!all(ok)) e <- c(e, sprintf("%s : %s", t, paste(names(ok)[!ok], collapse = ", ")))
  }
  list(ecarts = e, cas = n)
})
verifier(sprintf("B2 (valeur écrite à la main) : chaque type à motif, nombre = 2 (%d cas) : ligne_invalide nommant ce seul motif aux lignes porteuses ; sous exclusion (premier_inventaire, milieu), lignes_exclues = porteuses, nombre_lignes_exclues = porteuses et propagation, unites_exclues_par_propagation ; evaluer_motifs_stocks() : seul motif déclaré", DETECTION$cas), {
  if (length(DETECTION$ecarts)) paste(DETECTION$ecarts, collapse = " | ") else DETECTION$cas == 4L
})
verifier("B2 (valeur écrite à la main) : propagation A1 seulement sous les motifs de ligne (inventaire_hors_grille, inventaire_duplique) ; aucune sous les motifs d'unité, dont toutes les lignes restantes portent le motif (rôle unite)", {
  u <- vapply(TYPES[1:4], function(t) {
    a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(dc(t, nombre = 2L))), forme = FS)
    attr(a, "defauts")$decomptes$unites_propagation
  }, integer(1L))
  a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(dc("presence_interrompue", nombre = 2L))), forme = FS)
  l <- attr(a, "defauts")$lignes
  identical(unname(u), c(0L, 0L, 2L, 2L)) && identical(unique(l$role), "unite") &&
    identical(sort(l$ligne), which(a$id_contrat %in% a$id_contrat[l$ligne]))
})

# --- B3 : inventaire perdu --------------------------------------------------------------

# Date perdue tirée sous chaque graine (sample.int(4, 1)), lue dans retirees :
# première, deuxième et dernière date de la grille.
PERDU <- lapply(c(premier = "perdu_premier", milieu = "perdu_milieu", dernier = "perdu_dernier"), function(k)
  simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(list(type = "inventaire_perdu", nombre = 1L)), GRAINES_DEFAUTS[[k]]),
          forme = FS))
verifier("B3 (valeur écrite à la main) : inventaire perdu, dates t_1, t_2, t_4 : toutes les lignes d'inventaire de la date retirées (retirees : rang dans la projection propre, date perdue, motif NA, refus inventaire_sans_ligne), aucune ligne citée dans lignes, présents sains nuls à la date seule", {
  dates <- G4[c(1L, 2L, 4L)]
  ok <- vapply(seq_along(PERDU), function(k) {
    a <- PERDU[[k]]; ad <- attr(a, "defauts"); rt <- ad$retirees
    i_prop <- which(PROPRE$type_ligne == "inventaire" & PROPRE$date_inventaire == dates[k])
    ps <- ad$decomptes$presents_sains
    identical(rt$ligne, i_prop) && identical(unique(rt$date_inventaire), dates[k]) &&
      identical(unique(rt$type_ligne), "inventaire") && all(is.na(rt$motif_attendu)) &&
      identical(unique(rt$refus_attendu), "inventaire_sans_ligne") && identical(unique(rt$unite), "inventaire") &&
      identical(rt$id_contrat, PROPRE$id_contrat[i_prop]) &&
      !any(a$date_inventaire == dates[k], na.rm = TRUE) && nrow(a) == nrow(PROPRE) - length(i_prop) &&
      nrow(ad$lignes) == 0L && identical(ad$decomptes$retirees, length(i_prop)) &&
      identical(ps$date_inventaire, G4) && identical(which(ps$presents_sains == 0L), match(dates[k], G4))
  }, NA)
  all(ok)
})
verifier("B3 (valeur écrite à la main) : cascade de l'inventaire perdu (décision du mainteneur du 11/11/2026) : t_1, refus inventaire_sans_ligne direct ; t_2, presence_interrompue et evenement_apres_absence puis, exclus, le refus ; t_4, sortie_non_renseignee puis, exclu, le refus ; details$dates = date perdue, details$lignes_recues = 0", {
  r0 <- lapply(PERDU, function(a) rf(construire_table(a, RS, plan_st())))
  r1 <- lapply(PERDU, function(a) rf(construire_table(a, RS, plan_st(CASCADE))))
  sl <- function(r, d) !is.null(r) && identical(r$motif, "inventaire_sans_ligne") && identical(r$module, "donnees") &&
    identical(r$details$dates, d) && identical(r$details$lignes_recues, 0L)
  sl(r0$premier, G4[1L]) && sl(r1$premier, G4[1L]) &&
    identical(r0$milieu$motif, "ligne_invalide") &&
    identical(names(r0$milieu$details$lignes), c("presence_interrompue", "evenement_apres_absence")) &&
    sl(r1$milieu, G4[2L]) &&
    identical(r0$dernier$motif, "ligne_invalide") && identical(names(r0$dernier$details$lignes), "sortie_non_renseignee") &&
    sl(r1$dernier, G4[4L])
})
verifier("B3 (valeur écrite à la main) : deux dates perdues, t_1 et t_3 (segment non initial ; graine perdu_plusieurs) : sans exclusion, ligne_invalide nommant exactement presence_interrompue et evenement_apres_absence ; sous les exclusions de la cascade, refus inventaire_sans_ligne aux dates à présents sains nuls (t_1, t_3), aucune ligne reçue", {
  a <- simuler(LOI, PF, PER, GRAINE_JEU, forme = FS,
               defauts = dft(list(list(type = "inventaire_perdu", nombre = 2L)), GRAINES_DEFAUTS[["perdu_plusieurs"]]))
  ad <- attr(a, "defauts"); ps <- ad$decomptes$presents_sains
  r0 <- rf(construire_table(a, RS, plan_st()))
  r1 <- rf(construire_table(a, RS, plan_st(CASCADE)))
  identical(sort(unique(ad$retirees$date_inventaire)), D(c("2014-12-31", "2016-12-31"))) &&
    identical(ps$date_inventaire[ps$presents_sains == 0L], D(c("2014-12-31", "2016-12-31"))) &&
    !is.null(r0) && identical(r0$motif, "ligne_invalide") &&
    identical(names(r0$details$lignes), c("presence_interrompue", "evenement_apres_absence")) &&
    !is.null(r1) && identical(r1$motif, "inventaire_sans_ligne") &&
    identical(r1$details$dates, D(c("2014-12-31", "2016-12-31"))) && identical(r1$details$lignes_recues, c(0L, 0L))
})
verifier("B3 (valeur écrite à la main) : inventaire_perdu exclusif, avec toute autre déclaration (avant ou après) -> defauts_invalides", {
  a <- rf(simuler(LOI, PF, PER, GRAINE_JEU, forme = FS,
                  defauts = dft(list(list(type = "inventaire_perdu", nombre = 1L), dc("inventaire_duplique", nombre = 1L)),
                                GRAINES_DEFAUTS[["refus"]])))
  b <- rf(simuler(LOI, PF, PER, GRAINE_JEU, forme = FS,
                  defauts = dft(list(dc("inventaire_duplique", nombre = 1L), list(type = "inventaire_perdu", taux = 0)),
                                GRAINES_DEFAUTS[["refus"]])))
  !is.null(a) && identical(a$motif, "defauts_invalides") && !is.null(b) && identical(b$motif, "defauts_invalides") &&
    identical(conditionMessage(a), paste("Défauts : déclaration 1 : type « inventaire_perdu » exclusif : il doit être la",
                                         "seule déclaration (2 déclarations reçues).")) &&
    identical(b$details$declaration, 2L)
})

# --- B4 : valeurs écrites à la main sur un jeu de six unités ----------------------------

# Jeu propre écrit à la main, grille G4 (celui du test direct de
# projeter_stocks(), test_stocks_successifs.R) ; projection (13 lignes) :
# 1-3 unité 2, inventaires t_1 à t_3 ; 4 unité 2, rachat 2017-12-31 ;
# 5 unité 3, inventaire t_1 ; 6 unité 3, décès 2015-12-31 ; 7-9 unité 4,
# inventaires t_2 à t_4, en cours ; 10 unité 5, décès 2016-06-01 (événement
# seul) ; 11-13 unité 6, inventaires t_2 à t_4, en cours. Unité 1 : aucune
# ligne. Sélection sous nombre = nombre d'éligibles ou taux = 1 : toutes les
# unités éligibles retenues, quel que soit le tirage.
J6 <- data.frame(id_contrat = 1:6, id_tete = 1:6, sexe = "H", date_naissance = D("1960-01-01"),
                 date_entree = D(c("2010-01-01", "2010-01-01", "2010-01-01", "2015-12-31", "2016-01-01", "2015-03-03")),
                 date_sortie = D(c("2014-12-31", "2017-12-31", "2015-12-31", NA, "2016-06-01", NA)),
                 cause_sortie = c("deces", "rachat", "deces", "en_cours", "deces", "en_cours"), stringsAsFactors = FALSE)
ST6 <- tablexp:::projeter_stocks(J6, G4)$lignes
FS6 <- list(nom = "stocks_successifs", inventaires = G4 + 0)
inj6 <- function(declarations) {
  d <- dft(declarations, GRAINES_DEFAUTS[["main"]])
  ctl <- tablexp:::controler_defauts(d, NULL, tablexp:::conditions_vides("uniforme", PF, PER, FS6),
                                     tablexp:::DEFAUTS_FORMES$stocks_successifs)
  tablexp:::injecter_defauts_stocks(ST6, ctl, d, PER, FS6)
}
verifier("B4 (valeur écrite à la main) : sortie_non_renseignee (cible deces, rachat) : unités 2 et 3 éligibles (unité 5 sans inventaire), lignes d'événement 4 et 6 retirées ; lignes restantes des unités en rôle unite ; présents sains 0, 2, 2, 2", {
  a <- inj6(list(dc("sortie_non_renseignee", nombre = 2L))); ad <- attr(a, "defauts")
  identical(nrow(a), 11L) && identical(a$id_contrat, c(2L, 2L, 2L, 3L, 4L, 4L, 4L, 5L, 6L, 6L, 6L)) &&
    identical(ad$lignes$ligne, 1:4) && identical(unique(ad$lignes$role), "unite") &&
    identical(unique(ad$lignes$motif_attendu), "sortie_non_renseignee") &&
    identical(ad$retirees$ligne, c(4L, 6L)) && identical(ad$retirees$id_contrat, 2:3) &&
    identical(ad$retirees$type_ligne, c("evenement", "evenement")) && all(is.na(ad$retirees$date_inventaire)) &&
    identical(ad$decomptes$declarations$eligibles, 2L) &&
    identical(ad$decomptes$presents_sains$presents_sains, c(0L, 2L, 2L, 2L))
})
verifier("B4 (valeur écrite à la main) : presence_interrompue : unités 2, 4, 6 (au moins trois inventaires), inventaires t_(f+1) retirés (lignes 2, 8, 12) ; lignes renumérotées 1, 2, 3, 6, 7, 9, 10 ; présents sains 1, 0, 0, 0", {
  a <- inj6(list(dc("presence_interrompue", nombre = 3L))); ad <- attr(a, "defauts")
  identical(ad$retirees$ligne, c(2L, 8L, 12L)) && identical(ad$retirees$date_inventaire, G4[c(2L, 3L, 3L)]) &&
    identical(ad$lignes$ligne, c(1L, 2L, 3L, 6L, 7L, 9L, 10L)) && identical(nrow(a), 10L) &&
    identical(a$date_inventaire[a$id_contrat == 4L & a$type_ligne == "inventaire"], G4[c(2L, 4L)]) &&
    identical(ad$decomptes$presents_sains$presents_sains, c(1L, 0L, 0L, 0L))
})
verifier("B4 (valeur écrite à la main) : inventaire_hors_grille (taux = 1) : premiers inventaires 1, 5, 7, 11 décalés au lendemain hors grille (2015-01-01, 2016-01-01) ; autres lignes des unités en propagation (8 lignes, 4 unités) ; aucune ligne retirée", {
  a <- inj6(list(dc("inventaire_hors_grille", taux = 1))); ad <- attr(a, "defauts"); l <- ad$lignes
  autres <- setdiff(1:13, c(1L, 5L, 7L, 11L))
  identical(a$date_inventaire[c(1L, 5L, 7L, 11L)], D(c("2015-01-01", "2015-01-01", "2016-01-01", "2016-01-01"))) &&
    identical(lapply(a, `[`, autres), lapply(ST6, `[`, autres)) &&
    identical(l$ligne, setdiff(1:13, 10L)) && identical(l$ligne[l$role == "alteree"], c(1L, 5L, 7L, 11L)) &&
    identical(l$ligne[l$role == "propagation"], c(2L, 3L, 4L, 6L, 8L, 9L, 12L, 13L)) &&
    all(is.na(l$motif_attendu[l$role == "propagation"])) && all(is.na(l$refus_attendu[l$role == "propagation"])) &&
    identical(ad$decomptes[c("lignes_en_defaut", "lignes_alterees", "retirees", "lignes_propagation", "unites_propagation")],
              list(lignes_en_defaut = 4L, lignes_alterees = 4L, retirees = 0L, lignes_propagation = 8L,
                   unites_propagation = 4L)) &&
    identical(ad$decomptes$presents_sains$presents_sains, rep(0L, 4L))
})
verifier("B4 (valeur écrite à la main) : date d'inventaire hors grille : min{d > t_f : d hors de la grille}, grille de dates consécutives (t_1 et t_1 + 1 dans la grille : t_1 + 2)", {
  g <- D(c("2014-12-31", "2015-01-01", "2017-12-31"))
  j <- J6[J6$id_contrat == 2L, ]
  st <- tablexp:::projeter_stocks(j, g)$lignes
  f <- list(nom = "stocks_successifs", inventaires = g + 0)
  d <- dft(list(dc("inventaire_hors_grille", nombre = 1L)), GRAINES_DEFAUTS[["main"]])
  ctl <- tablexp:::controler_defauts(d, NULL, tablexp:::conditions_vides("uniforme", PF, PER, f),
                                     tablexp:::DEFAUTS_FORMES$stocks_successifs)
  a <- tablexp:::injecter_defauts_stocks(st, ctl, d, PER, f)
  identical(a$date_inventaire[1L], D("2015-01-02"))
})
verifier("B4 (valeur écrite à la main) : inventaire_duplique (cible deces) : unité 3 seule éligible, copie de la ligne 5 en ligne 14 ; ligne 6 en propagation", {
  a <- inj6(list(list(type = "inventaire_duplique", cible = "deces", nombre = 1L))); ad <- attr(a, "defauts")
  identical(nrow(a), 14L) && identical(lapply(a, `[`, 14L), lapply(ST6, `[`, 5L)) &&
    identical(ad$lignes$ligne, c(5L, 6L, 14L)) && identical(ad$lignes$role, c("original", "propagation", "copie")) &&
    identical(ad$decomptes$declarations$eligibles, 1L) && identical(ad$decomptes$copies, 1L) &&
    identical(ad$decomptes$lignes, 14L)
})
verifier("B4 (valeur écrite à la main) : inventaire_perdu (taux = 1) : les quatre dates perdues, dix lignes d'inventaire retirées, restent les événements 4, 6, 10 ; présents sains nuls", {
  a <- inj6(list(list(type = "inventaire_perdu", taux = 1))); ad <- attr(a, "defauts")
  identical(lapply(a, identity), lapply(ST6, `[`, c(4L, 6L, 10L))) && identical(attr(a, "row.names"), 1:3) &&
    identical(ad$retirees$ligne, setdiff(1:13, c(4L, 6L, 10L))) &&
    identical(ad$retirees$date_inventaire, G4[c(1:3, 1L, 2:4, 2:4)]) && nrow(ad$lignes) == 0L &&
    identical(ad$decomptes$declarations$eligibles, 4L) && identical(ad$decomptes$declarations$unite, "inventaire") &&
    identical(ad$decomptes$presents_sains$presents_sains, rep(0L, 4L))
})
verifier("B4 (disjonction, valeur écrite à la main) : une unité altérée au plus une fois, tous types confondus : presence_interrompue retient 2, 4, 6 ; inventaire_hors_grille (taux = 1) n'a plus que l'unité 3", {
  a <- inj6(list(dc("presence_interrompue", nombre = 3L), dc("inventaire_hors_grille", taux = 1)))
  ad <- attr(a, "defauts"); dcl <- ad$decomptes$declarations
  identical(dcl$eligibles, c(3L, 1L)) && identical(dcl$retenues, c(3L, 1L)) &&
    identical(ad$lignes$ligne[ad$lignes$declaration == 2L], c(4L, 5L)) &&
    identical(a$date_inventaire[4L], D("2015-01-01"))
})
verifier("B4 (valeur écrite à la main) : attribut defauts des stocks : sept champs puis retirees ; validite (roles, identifiant id_contrat seul, periode, délai 0, texte avec la cascade, inventaires) ; decomptes, champs propres aux stocks en fin", {
  a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(dc("inventaire_duplique", nombre = 2L))), forme = FS)
  ad <- attr(a, "defauts")
  identical(names(ad), c("parametre", "graine", "generateur", "ordre_tirages", "validite", "lignes", "decomptes",
                         "retirees")) &&
    identical(ad$graine, GRAINES_DEFAUTS[["detection"]]) &&
    identical(names(ad$validite), c("roles", "periode", "delai_declaration", "texte", "inventaires")) &&
    identical(ad$validite$roles, RS[c("id_contrat", "type_ligne", "date_inventaire", "sexe", "date_naissance",
                                      "date_sortie", "cause_sortie")]) &&
    identical(ad$validite$periode, PER) && identical(ad$validite$delai_declaration, 0) &&
    identical(ad$validite$inventaires, G4 + 0) &&
    grepl("t_M, dernière date de la grille, motif sortie_non_renseignee, puis, exclu, refus inventaire_sans_ligne",
          ad$validite$texte, fixed = TRUE) &&
    identical(names(ad$lignes), c("ligne", "declaration", "defaut", "unite", "role", "motif_attendu", "refus_attendu",
                                  "premier_deces")) &&
    identical(names(ad$decomptes), c("lignes_jeu_propre", "copies", "lignes", "lignes_en_defaut", "lignes_alterees",
                                     "declarations", "retirees", "lignes_propagation", "unites_propagation",
                                     "presents_sains")) &&
    identical(names(ad$retirees), c("ligne", "declaration", "defaut", "unite", "id_contrat", "type_ligne",
                                    "date_inventaire", "motif_attendu", "refus_attendu")) &&
    identical(ad$ordre_tirages[3:4], tablexp:::ORDRE_TIRAGES_DEFAUTS[3:4])
})

# --- B5 : refus avant tout tirage --------------------------------------------------------

verifier("B5 (refus écrits à la main) : presence_interrompue sous une grille de deux dates, cible disjointe, cible sous inventaire_perdu, cible absente d'un type d'unité, type d'une autre forme -> defauts_invalides ; mode multi-contrats -> forme_invalide, message identique à l'octet avec ou sans defauts ; avant tout tirage (aucun appel de sous_graine())", {
  ns <- asNamespace("tablexp"); cpt <- new.env(); cpt$n <- 0L
  suppressMessages(trace("sous_graine", tracer = bquote(assign("n", get("n", envir = .(cpt)) + 1L, envir = .(cpt))),
                         where = ns, print = FALSE))
  pfm <- c(PF, list(loi_contrats = c(0.5, 0.5))); pfm$mode <- "uniforme_multi"
  s <- function(d = NULL, f = FS, pf = PF) simuler(LOI, pf, PER, GRAINE_JEU, defauts = d, forme = f)
  dr <- function(...) dft(list(list(...)), GRAINES_DEFAUTS[["refus"]])
  cas <- tryCatch(list(
    grille2 = rf(s(dr(type = "presence_interrompue", cible = "deces", nombre = 1L), list(nom = "stocks_successifs",
                                                                                        inventaires = G2))),
    disjointe = rf(s(dr(type = "sortie_non_renseignee", cible = "en_cours", nombre = 1L))),
    cible_perdu = rf(s(dr(type = "inventaire_perdu", cible = "deces", nombre = 1L))),
    sans_cible = rf(s(dr(type = "presence_interrompue", nombre = 1L))),
    lignes = rf(s(dr(type = "sexe_manquant", cible = "deces", nombre = 1L))),
    agregats = rf(s(dr(type = "cellule_dupliquee", nombre = 1L))),
    stocks_en_lignes = rf(s(dr(type = "inventaire_perdu", nombre = 1L), NULL)),
    multi = rf(s(NULL, FS, pfm)),
    multi_defauts = rf(s(dr(type = "inventaire_perdu", nombre = 1L), FS, pfm))),
    finally = suppressMessages(untrace("sous_graine", where = ns)))
  attendu <- c(grille2 = "defauts_invalides", disjointe = "defauts_invalides", cible_perdu = "defauts_invalides",
               sans_cible = "defauts_invalides", lignes = "defauts_invalides", agregats = "defauts_invalides",
               stocks_en_lignes = "defauts_invalides", multi = "forme_invalide", multi_defauts = "forme_invalide")
  ko <- names(attendu)[!vapply(names(attendu), function(k)
    !is.null(cas[[k]]) && identical(cas[[k]]$module, "simulation") && identical(cas[[k]]$motif, attendu[[k]]), NA)]
  msg <- function(k) conditionMessage(cas[[k]])
  if (!identical(cpt$n, 0L)) sprintf("%d appel(s) de sous_graine() pendant les refus", cpt$n)
  else if (length(ko)) paste("sans le refus attendu :", paste(ko, collapse = ", "))
  else if (!identical(msg("grille2"), paste("Défauts : déclaration 1 : type presence_interrompue : aucune unité éligible",
                                            "par construction (grille de 2 dates d'inventaire, moins de trois), quel que",
                                            "soit le tirage.")) ||
           !identical(msg("disjointe"), paste("Défauts : déclaration 1 : cible « en_cours » disjointe des causes admises",
                                              "pour sortie_non_renseignee (« deces », « rachat ») : aucune unité éligible",
                                              "par construction.")) ||
           !identical(msg("cible_perdu"), paste("Défauts : déclaration 1 : type « inventaire_perdu » (unité inventaire) :",
                                                "champ cible interdit.")) ||
           !identical(msg("sans_cible"), paste("Défauts : déclaration 1 : type d'unité « presence_interrompue » : champ",
                                               "cible requis (sous-ensemble de « deces », « rachat », « en_cours »).")) ||
           !grepl("type « sexe_manquant » inconnu ; types admis : « sortie_non_renseignee »", msg("lignes"), fixed = TRUE) ||
           !grepl("type « inventaire_perdu » inconnu ; types admis : « contrat_duplique »", msg("stocks_en_lignes"),
                  fixed = TRUE) ||
           !identical(msg("multi"), msg("multi_defauts")) ||
           !grepl("(tête ou contrat) serait ambiguë ; la fusion par tête des contrats inventoriés", msg("multi"), fixed = TRUE))
    "message différent de l'attendu"
  else TRUE
})
verifier("B5 : defauts_ineligibles après la projection (deux appels de sous_graine() : jeu, puis défauts), état .Random.seed de l'appelant inchangé", {
  ns <- asNamespace("tablexp"); cpt <- new.env(); cpt$n <- 0L
  suppressMessages(trace("sous_graine", tracer = bquote(assign("n", get("n", envir = .(cpt)) + 1L, envir = .(cpt))),
                         where = ns, print = FALSE))
  set.seed(9L); avant <- .Random.seed
  r <- tryCatch(rf(simuler(LOI, PF, PER, GRAINE_JEU, forme = FS,
                           defauts = dft(list(list(type = "inventaire_perdu", nombre = 5L)), GRAINES_DEFAUTS[["refus"]]))),
                finally = suppressMessages(untrace("sous_graine", where = ns)))
  !is.null(r) && identical(r$motif, "defauts_ineligibles") && identical(cpt$n, 2L) && identical(.Random.seed, avant) &&
    identical(r$details$eligibles, 4L)
})

# --- B6 : présents sains ------------------------------------------------------------------

verifier("B6 (valeur écrite à la main) : sous exclusion de tous les motifs, refus inventaire_sans_ligne aux dates sans présent sain : jeu de six unités, inventaire_hors_grille sur les unités 2 et 3 (cible deces, rachat) : présents sains 0, 2, 2, 2, refus à t_1 seule (aucune ligne reçue à t_1)", {
  a <- inj6(list(list(type = "inventaire_hors_grille", cible = c("deces", "rachat"), taux = 1)))
  ps <- attr(a, "defauts")$decomptes$presents_sains$presents_sains
  r <- rf(construire_table(a, RS, plan_st("inventaire_hors_grille")))
  identical(ps, c(0L, 2L, 2L, 2L)) && !is.null(r) && identical(r$motif, "inventaire_sans_ligne") &&
    identical(r$details$dates, G4[ps == 0L]) && identical(r$details$lignes_recues, 0L)
})
verifier("B6 (valeur écrite à la main) : jeu simulé, chaque type à motif (nombre = 2) : présents sains tous positifs, construction sous exclusion acceptée ; inventaire perdu : dates du refus = dates sans présent sain (t_1, t_2, t_4)", {
  ok1 <- vapply(TYPES[1:4], function(t) {
    a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(dc(t, nombre = 2L))), forme = FS)
    ps <- attr(a, "defauts")$decomptes$presents_sains$presents_sains
    all(ps > 0L) && inherits(construire_table(a, RS, plan_st(t)), "construction")
  }, NA)
  ok2 <- vapply(PERDU, function(a) {
    ps <- attr(a, "defauts")$decomptes$presents_sains
    r <- rf(construire_table(a, RS, plan_st(CASCADE)))
    !is.null(r) && identical(r$details$dates, ps$date_inventaire[ps$presents_sains == 0L])
  }, NA)
  all(ok1) && all(ok2)
})

# --- B7 : clôture -------------------------------------------------------------------------

verifier("B7 (valeur écrite à la main) : quatre motifs du catalogue des stocks et 25 motifs hors périmètre = MOTIFS_LIGNES_STOCKS et MOTIFS_UNITE_STOCKS ; inventaire_perdu : refus inventaire_sans_ligne, motif NA ; catalogues des lignes et des agrégats inchangés", {
  hors <- c("type_ligne_manquant", "type_ligne_hors_modalites", "id_contrat_manquant", "id_tete_manquant",
            "naissance_manquante", "sexe_manquant", "sexe_hors_modalites", "segment_manquant",
            "annee_naissance_incoherente", "age_aberrant", "inventaire_sans_date", "inventaire_avec_sortie",
            "naissance_apres_inventaire", "sortie_manquante", "cause_manquante", "cause_hors_modalites", "cause_en_cours",
            "evenement_avec_date_inventaire", "naissance_apres_sortie", "unite_naissance_incoherente",
            "unite_sexe_incoherent", "unite_segment_incoherent", "evenements_multiples", "presence_apres_evenement",
            "evenement_apres_absence")
  m <- c(tablexp:::MOTIFS_LIGNES_STOCKS, tablexp:::MOTIFS_UNITE_STOCKS)
  mc <- CAT_ST$motif_attendu[!is.na(CAT_ST$motif_attendu)]
  identical(CAT_ST$defaut, TYPES) && identical(mc, TYPES[1:4]) && length(hors) == 25L &&
    setequal(c(mc, hors), m) && length(c(mc, hors)) == length(m) && !any(hors %in% mc) &&
    identical(CAT_ST$refus_attendu, c(rep("ligne_invalide", 4L), "inventaire_sans_ligne")) &&
    is.na(CAT_ST$motif_attendu[5L]) && identical(CAT_ST$unite, c("unite", "unite", "ligne", "ligne", "inventaire")) &&
    identical(tablexp:::DEFAUTS_FORMES$stocks_successifs$exclusifs, "inventaire_perdu") &&
    identical(tablexp:::DEFAUTS_FORMES$lignes$catalogue, tablexp:::CATALOGUE_DEFAUTS) &&
    nrow(tablexp:::CATALOGUE_DEFAUTS) == 18L && nrow(tablexp:::DEFAUTS_FORMES$agregats$catalogue) == 5L &&
    is.null(tablexp:::DEFAUTS_FORMES$lignes$exclusifs) && is.null(tablexp:::DEFAUTS_FORMES$agregats$exclusifs) &&
    setequal(names(tablexp:::DEFAUTS_FORMES), c("lignes", names(tablexp:::CHAMPS_FORMES)))
})
verifier("B7 : conditions_vides() des stocks : grille de deux dates, condition grille_trois_dates vide ; trois dates ou plus, aucune ; controler_forme() à quatre arguments (paramètre avec_defauts retiré)", {
  v2 <- tablexp:::conditions_vides("uniforme", PF, PER, list(nom = "stocks_successifs", inventaires = G2))
  v4 <- tablexp:::conditions_vides("uniforme", PF, PER, FS6)
  identical(v2, c(grille_trois_dates = "grille de 2 dates d'inventaire, moins de trois")) &&
    identical(v4, stats::setNames(character(0), character(0))) &&
    identical(names(formals(tablexp:::controler_forme)), c("forme", "mode", "debut", "fin"))
})
