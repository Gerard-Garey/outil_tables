# -----------------------------------------------------------------------------
#  tests/unitaires/test_simulateur_agregats.R : forme « agrégats » du
#  simulateur, défauts paramétrés propres aux agrégats, modes multi-contrats
#  (unite_observation) et convention age_atteint_annee (issue #189 ; avis de
#  forme d'architect-approfondi et spécification d'actuary-approfondi de #189,
#  critères A1 à A8 ; ADR 0007, points 7, 8 et 11.4)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité exacte : appels répétés, jeu propre et projection inchangés par
#    les défauts, cellules non altérées (A1) ; constructions en agrégats
#    age_atteint_annee et en lignes (A6) ;
#  - valeur écrite à la main : motifs levés et lignes exclues (A2, attendus
#    lus dans l'attribut `defauts`, écrits en dur dans le catalogue de la
#    forme, jamais calculés par `donnees`), petit jeu écrit à la main (A3),
#    refus (A4), clôture (A7) ;
#  - recoupement, approximation numérique à tolérance relative 1e-10 :
#    modes multi-contrats et age_atteint_annee contre la table d'exposition
#    du moteur en lignes (A5) ; somme sur les années de E^i à 1e-12 ; un
#    dépassement est un constat, jamais une tolérance à élargir.
#  Aucun constat Monte-Carlo (spécification, § 6). Graines dans la sous-plage
#  22 001 230 - 22 001 264 (spécification, § 6, A8), une valeur par contexte.
# -----------------------------------------------------------------------------

D <- as.Date
simuler <- tablexp::simuler_portefeuille
GRAINE_JEU <- 22001230L
GRAINES_DEFAUTS <- c(detection = 22001231L, identite = 22001232L, refus = 22001233L, disjonction = 22001234L,
                     main = 22001235L)

PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
LOI <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)),
            rachat = list(table = tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %"), facteurs = c(H = 1, F = 1)))
PF <- list(mode = "uniforme", n = 3000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
PFM <- c(PF, list(loi_contrats = c(0.5, 0.3, 0.2))); PFM$mode <- "uniforme_multi"
ROLES_LIGNES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
                  date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
TYPES <- c("cellule_dupliquee", "exposition_negative", "evenements_non_entiers", "evenements_sans_exposition",
           "annee_hors_periode")
CAT_AG <- tablexp:::DEFAUTS_FORMES$agregats$catalogue

rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
fa <- function(transition = VD, convention = "dernier_anniversaire", par_annee = FALSE, unite = NULL) {
  f <- list(nom = "agregats", transition = transition, convention = convention, par_annee = par_annee)
  if (!is.null(unite)) f$unite_observation <- unite
  f
}
dft <- function(declarations, graine = GRAINES_DEFAUTS[["detection"]]) list(graine = graine, declarations = declarations)
plan_ag <- function(f, exclusions = character(0), etapes = list(brut = list(estimateurs = "poisson", niveau = 0.95)))
  list(transition = f$transition, periode = PER, delai_declaration = 0, segmentation = "sexe",
       etapes = c(list(donnees = list(exclusions = exclusions),
                       exposition = list(convention = f$convention, par_annee = f$par_annee)), etapes))
roles_de <- function(x) stats::setNames(names(x), names(x))
rel <- function(u, v) if (!length(u)) 0 else max(abs(u - v) / pmax(abs(v), .Machine$double.xmin))

# Configurations d'A2 : deux conventions, avec et sans année (mode uniforme) ;
# modes multi-contrats, unités tête et contrat.
CONFIGS <- list(
  da = list(pf = PF, f = fa()), da_annee = list(pf = PF, f = fa(par_annee = TRUE)),
  aa = list(pf = PF, f = fa(convention = "age_atteint_annee")),
  aa_annee = list(pf = PF, f = fa(convention = "age_atteint_annee", par_annee = TRUE)),
  multi_tete = list(pf = PFM, f = fa(unite = "tete")),
  multi_contrat = list(pf = PFM, f = fa(RA, convention = "age_atteint_annee", par_annee = TRUE, unite = "contrat")))
PROPRES <- lapply(CONFIGS, function(cf) simuler(LOI, cf$pf, PER, GRAINE_JEU, forme = cf$f))

# --- A1 : identité exacte ------------------------------------------------------------

verifier("A1 (identité exacte) : deux appels avec défauts identical() ; attribut simulation et cellules non altérées identiques à la forme propre ; copies = valeurs propres des originaux", {
  e <- character(0)
  for (k in names(CONFIGS)) {
    cf <- CONFIGS[[k]]; propre <- PROPRES[[k]]
    decl <- lapply(TYPES[if (cf$f$par_annee) 1:5 else 1:4], function(t) list(type = t, nombre = 2L))
    d <- dft(decl, GRAINES_DEFAUTS[["identite"]])
    a1 <- simuler(LOI, cf$pf, PER, GRAINE_JEU, defauts = d, forme = cf$f)
    a2 <- simuler(LOI, cf$pf, PER, GRAINE_JEU, defauts = d, forme = cf$f)
    l <- attr(a1, "defauts")$lignes
    N <- nrow(propre)
    autres <- setdiff(seq_len(N), l$ligne)
    org <- l$ligne[l$role == "original"]; cop <- l$ligne[l$role == "copie"]
    # Colonnes des lignes i (liste nue ; evenements en double, la colonne
    # passant en double sous evenements_non_entiers).
    num <- function(x, i) { l <- lapply(x, `[`, i); l$evenements <- as.numeric(l$evenements); l }
    ok <- c(appels = identical(a1, a2),
            simulation = identical(attr(a1, "simulation"), attr(propre, "simulation")),
            non_alterees = identical(num(a1, autres), num(propre, autres)),
            copies = identical(num(a1, cop), num(propre, org)),
            nombre = nrow(a1) == N + length(cop) && length(cop) == 2L,
            originaux = identical(num(a1, org), num(propre, org)),
            attributs = setequal(names(attributes(a1)), c(names(attributes(propre)), "defauts")) &&
              identical(names(a1), names(propre)))
    if (!all(ok)) e <- c(e, paste(k, ":", paste(names(ok)[!ok], collapse = ", ")))
  }
  if (length(e)) paste(e, collapse = " | ") else TRUE
})
verifier("A1 (identité exacte) : evenements reste integer sans evenements_non_entiers retenu ; passe en double si une cellule au moins est retenue", {
  a0 <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(list(type = "cellule_dupliquee", nombre = 2L))), forme = fa())
  a1 <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(list(type = "evenements_non_entiers", taux = 0))), forme = fa())
  a2 <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(list(type = "evenements_non_entiers", nombre = 1L))), forme = fa())
  is.integer(a0$evenements) && is.integer(a1$evenements) && is.double(a2$evenements) &&
    identical(a1[names(a1)], PROPRES$da[names(a1)]) && attr(a1, "defauts")$decomptes$declarations$retenues == 0L
})

# --- A2 : détection par le module donnees --------------------------------------------

# Pour chaque configuration et chaque type (nombre = 2) : sans exclusion,
# refus ligne_invalide nommant ce seul motif, aux lignes de l'attribut ; sous
# exclusion du motif, lignes_exclues[[motif]] = lignes de l'attribut ;
# evaluer_motifs_agregats() : motifs non vides = motif déclaré.
DETECTION <- local({
  e <- character(0); n <- 0L
  for (k in names(CONFIGS)) {
    cf <- CONFIGS[[k]]
    p <- tablexp:::valeurs_parametres_donnees(cf$f$transition, PER, 0)
    for (t in TYPES[if (cf$f$par_annee) 1:5 else 1:4]) {
      a <- simuler(LOI, cf$pf, PER, GRAINE_JEU, defauts = dft(list(list(type = t, nombre = 2L))), forme = cf$f)
      lg <- attr(a, "defauts")$lignes
      attendu <- sort(unique(lg$ligne))
      r <- rf(construire_table(a, roles_de(a), plan_ag(cf$f)))
      x <- tryCatch(construire_table(a, roles_de(a), plan_ag(cf$f, exclusions = t)), refus_tablexp = function(e) e)
      m <- tablexp:::evaluer_motifs_agregats(a, roles_de(a), p)$motifs
      ok <- c(attendu = identical(unique(lg$motif_attendu), t) && length(attendu) == if (t == "cellule_dupliquee") 4L else 2L,
              refus = !is.null(r) && identical(r$motif, "ligne_invalide") && identical(names(r$details$lignes), t) &&
                identical(r$details$lignes[[t]], attendu),
              exclusion = inherits(x, "construction") &&
                identical(x$etapes$donnees$resultat$lignes_exclues[[t]], attendu) &&
                x$etapes$donnees$resultat$nombre_lignes_exclues == length(attendu),
              motifs = identical(names(m)[lengths(m) > 0L], t))
      n <- n + 1L
      if (!all(ok)) e <- c(e, sprintf("%s, %s : %s", k, t, paste(names(ok)[!ok], collapse = ", ")))
    }
  }
  list(ecarts = e, cas = n)
})
verifier(sprintf("A2 (valeur écrite à la main) : chaque type, nombre = 2, deux conventions avec et sans année, modes multi tête et contrat (%d cas) : ligne_invalide nommant ce seul motif ; lignes_exclues = lignes de l'attribut ; evaluer_motifs_agregats() : seul motif déclaré", DETECTION$cas), {
  if (length(DETECTION$ecarts)) paste(DETECTION$ecarts, collapse = " | ") else DETECTION$cas == 27L
})

# --- A3 : valeurs écrites à la main -----------------------------------------------------

# Jeu écrit à la main (période 2015-2024, B = 2025-01-01) :
#   1 : H, né le 01/07/1950, entré le 01/04/2019, décédé le 01/03/2020 ;
#   2 : F, née le 31/12/1960, entrée le 01/06/2023, en cours.
# age_atteint_annee : ligne 1, 2019 âge 69, 275 jours / 365 ; 2020 âge 70,
# 60 / 366, événement, complément (01/01/2021 - 01/03/2020) / 366 = 306 / 366 ;
# ligne 2, 2023 âge 63, 214 / 365 ; 2024 âge 64, 366 / 366.
JEU_MAIN <- data.frame(id_contrat = 1:2, id_tete = 1:2, sexe = c("H", "F"),
                       date_naissance = D(c("1950-07-01", "1960-12-31")), date_entree = D(c("2019-04-01", "2023-06-01")),
                       date_sortie = D(c("2020-03-01", NA)), cause_sortie = c("deces", "en_cours"), stringsAsFactors = FALSE)
DEB <- as.numeric(PER$debut); BORNE <- as.numeric(PER$fin) + 1
verifier("A3 (valeur écrite à la main) : age_atteint_annee sans année, cellules (F 63, 64 ; H 69, 70), E^c, E^i et D", {
  a <- tablexp:::projeter_agregats(JEU_MAIN, "deces", FALSE, DEB, BORNE, "age_atteint_annee")
  identical(names(a), c("sexe", "age", "exposition_centrale", "exposition_initiale", "evenements")) &&
    identical(a$sexe, c("F", "F", "H", "H")) && identical(a$age, c(63L, 64L, 69L, 70L)) &&
    isTRUE(all.equal(a$exposition_centrale, c(214 / 365, 1, 275 / 365, 60 / 366), tolerance = 1e-15)) &&
    isTRUE(all.equal(a$exposition_initiale, c(214 / 365, 1, 275 / 365, 1), tolerance = 1e-15)) &&
    identical(a$evenements, c(0L, 0L, 0L, 1L))
})
verifier("A3 (valeur écrite à la main) : age_atteint_annee par année, 40 cellules (2 sexes x 2 âges x 10 années), quatre non nulles, E^i produite", {
  a <- tablexp:::projeter_agregats(JEU_MAIN, "deces", TRUE, DEB, BORNE, "age_atteint_annee")
  cle <- paste(a$sexe, a$age, a$annee)
  nz <- a$exposition_centrale > 0
  identical(names(a), c("sexe", "age", "annee", "exposition_centrale", "exposition_initiale", "evenements")) &&
    nrow(a) == 40L && identical(sort(cle[nz]), c("F 63 2023", "F 64 2024", "H 69 2019", "H 70 2020")) &&
    isTRUE(all.equal(a$exposition_centrale[match(c("F 63 2023", "F 64 2024", "H 69 2019", "H 70 2020"), cle)],
                     c(214 / 365, 1, 275 / 365, 60 / 366), tolerance = 1e-15)) &&
    isTRUE(all.equal(a$exposition_initiale[cle == "H 70 2020"], 1, tolerance = 1e-15)) &&
    identical(a$evenements[cle == "H 70 2020"], 1L) && sum(a$evenements) == 1L &&
    identical(a$exposition_initiale[!nz], rep(0, 36L))
})
# Têtes écrites à la main : tête 3, trois contrats [01/01/2016, 01/01/2018)
# rachat, [01/06/2017, 01/01/2019) rachat, [01/01/2019, +Inf) en cours,
# contigus : une composante [01/01/2016, +Inf) en cours ; tête 4,
# [01/01/2016, 01/01/2017) rachat puis [01/01/2018, 01/05/2020) décès :
# deux composantes, la seconde à décès.
JEU_TETES <- data.frame(id_contrat = 1:5, id_tete = c(3L, 3L, 3L, 4L, 4L), sexe = c("H", "H", "H", "F", "F"),
                        date_naissance = D(rep(c("1955-02-10", "1948-11-30"), c(3L, 2L))),
                        date_entree = D(c("2016-01-01", "2017-06-01", "2019-01-01", "2016-01-01", "2018-01-01")),
                        date_sortie = D(c("2018-01-01", "2019-01-01", NA, "2017-01-01", "2020-05-01")),
                        cause_sortie = c("rachat", "rachat", "en_cours", "rachat", "deces"), stringsAsFactors = FALSE)
verifier("A3 (valeur écrite à la main) : fusionner_tetes(), contiguïté réunie (tête 3 : une composante en cours), disjonction gardée (tête 4 : rachat puis décès)", {
  f <- tablexp:::fusionner_tetes(JEU_TETES)
  identical(f$id_tete, c(3L, 4L, 4L)) && identical(f$date_entree, D(c("2016-01-01", "2016-01-01", "2018-01-01"))) &&
    identical(f$date_sortie, structure(c(NA, as.numeric(D(c("2017-01-01", "2020-05-01")))), class = "Date")) &&
    identical(f$cause_sortie, c("en_cours", "rachat", "deces")) && identical(f$sexe, c("H", "F", "F"))
})
verifier("A3 (valeur écrite à la main) : altérations des cinq défauts sur la table propre (-x sur chaque exposition, D + 0.5, expositions à 0, année - 10, copie en fin)", {
  propre <- PROPRES$aa_annee
  decl <- lapply(TYPES, function(t) list(type = t, nombre = 2L))
  a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(decl, GRAINES_DEFAUTS[["main"]]), forme = fa(convention = "age_atteint_annee", par_annee = TRUE))
  l <- attr(a, "defauts")$lignes
  li <- function(t, role = "alteree") l$ligne[l$defaut == t & l$role == role]
  i1 <- li("exposition_negative"); i2 <- li("evenements_non_entiers"); i3 <- li("evenements_sans_exposition")
  i4 <- li("annee_hors_periode"); org <- li("cellule_dupliquee", "original"); cop <- li("cellule_dupliquee", "copie")
  N <- nrow(propre)
  identical(a$exposition_centrale[i1], -propre$exposition_centrale[i1]) &&
    identical(a$exposition_initiale[i1], -propre$exposition_initiale[i1]) && all(propre$evenements[i1] == 0L) &&
    identical(a$evenements[i2], propre$evenements[i2] + 0.5) && all(propre$exposition_centrale[i2] > 0) &&
    identical(a$exposition_centrale[i3], c(0, 0)) && identical(a$exposition_initiale[i3], c(0, 0)) &&
    all(propre$evenements[i3] >= 1L) && identical(a$annee[i4], propre$annee[i4] - 10L) &&
    identical(cop, N + 1:2) && identical(a$age[cop], propre$age[org]) && identical(a$annee[cop], propre$annee[org]) &&
    identical(attr(a, "defauts")$decomptes[c("lignes_jeu_propre", "copies", "lignes", "lignes_en_defaut", "lignes_alterees")],
              list(lignes_jeu_propre = N, copies = 2L, lignes = N + 2L, lignes_en_defaut = 12L, lignes_alterees = 10L))
})
verifier("A3 (valeur écrite à la main) : attribut defauts des agrégats, sept champs dans l'ordre ; validite (roles, periode, délai 0, texte, convention, par_annee) ; lignes en unité cellule", {
  a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(list(type = "cellule_dupliquee", nombre = 2L))), forme = fa())
  ad <- attr(a, "defauts")
  identical(names(ad), c("parametre", "graine", "generateur", "ordre_tirages", "validite", "lignes", "decomptes")) &&
    identical(ad$graine, GRAINES_DEFAUTS[["detection"]]) &&
    identical(names(ad$validite), c("roles", "periode", "delai_declaration", "texte", "convention", "par_annee")) &&
    identical(ad$validite$roles, roles_de(PROPRES$da)) && identical(ad$validite$periode, PER) &&
    identical(ad$validite$delai_declaration, 0) && identical(ad$validite$convention, "dernier_anniversaire") &&
    identical(ad$validite$par_annee, FALSE) && grepl("ages_manquants", ad$validite$texte, fixed = TRUE) &&
    identical(names(ad$lignes), c("ligne", "declaration", "defaut", "unite", "role", "motif_attendu", "refus_attendu",
                                  "premier_deces")) &&
    identical(unique(ad$lignes$unite), "cellule") && identical(ad$lignes$role, c("original", "original", "copie", "copie")) &&
    identical(ad$ordre_tirages[3:4], tablexp:::ORDRE_TIRAGES_DEFAUTS[3:4]) &&
    identical(ad$decomptes$declarations$unite, "cellule")
})
verifier("A3 (valeur écrite à la main) : attribut simulation sous mode multi-contrats, unite_observation en fin de forme ; absente sous mode simple", {
  identical(attr(PROPRES$multi_tete, "simulation")$forme, fa(unite = "tete")) &&
    identical(attr(PROPRES$multi_contrat, "simulation")$forme,
              fa(RA, convention = "age_atteint_annee", par_annee = TRUE, unite = "contrat")) &&
    identical(names(attr(PROPRES$da, "simulation")$forme), c("nom", "transition", "convention", "par_annee"))
})

# --- A4 : refus avant tout tirage -------------------------------------------------------

verifier("A4 (refus écrits à la main) : annee_hors_periode sans annee, type du catalogue des lignes, cible déclarée, combinaison, type des agrégats sous forme = NULL -> defauts_invalides ; unite_observation manquante, inattendue, hors liste, tête sous rachat -> forme_invalide ; avant tout tirage (aucun appel de sous_graine())", {
  ns <- asNamespace("tablexp"); cpt <- new.env(); cpt$n <- 0L
  suppressMessages(trace("sous_graine", tracer = bquote(assign("n", get("n", envir = .(cpt)) + 1L, envir = .(cpt))),
                         where = ns, print = FALSE))
  s <- function(pf = PF, f = fa(), d = NULL) simuler(LOI, pf, PER, GRAINE_JEU, defauts = d, forme = f)
  dr <- function(...) dft(list(list(...)), GRAINES_DEFAUTS[["refus"]])
  cas <- tryCatch(list(
    annee = rf(s(d = dr(type = "annee_hors_periode", nombre = 1L))),
    annee_aa = rf(s(f = fa(convention = "age_atteint_annee"), d = dr(type = "annee_hors_periode", taux = 0.1))),
    lignes = rf(s(d = dr(type = "sexe_manquant", cible = "deces", nombre = 1L))),
    cible = rf(s(d = dr(type = "exposition_negative", cible = "deces", nombre = 1L))),
    combinaison = rf(s(d = dr(type = c("exposition_negative", "cellule_dupliquee"), nombre = 1L))),
    graine = rf(s(d = dft(list(list(type = "cellule_dupliquee", nombre = 1L)), GRAINE_JEU))),
    unite_manquante = rf(s(PFM)),
    unite_inattendue = rf(s(f = fa(unite = "contrat"))),
    unite_hors_liste = rf(s(PFM, fa(unite = "tetes"))),
    unite_na = rf(s(PFM, fa(unite = NA_character_))),
    tete_rachat = rf(s(PFM, fa(RA, unite = "tete"))),
    tete_rachat_defauts = rf(s(PFM, fa(RA, unite = "tete"), dr(type = "cellule_dupliquee", nombre = 1L))),
    agregats_en_lignes = rf(s(f = NULL, d = dr(type = "exposition_negative", nombre = 1L)))),
    finally = suppressMessages(untrace("sous_graine", where = ns)))
  attendu <- c(annee = "defauts_invalides", annee_aa = "defauts_invalides", lignes = "defauts_invalides",
               cible = "defauts_invalides", combinaison = "defauts_invalides", graine = "defauts_invalides",
               unite_manquante = "forme_invalide", unite_inattendue = "forme_invalide", unite_hors_liste = "forme_invalide",
               unite_na = "forme_invalide", tete_rachat = "forme_invalide", tete_rachat_defauts = "forme_invalide",
               agregats_en_lignes = "defauts_invalides")
  ko <- names(attendu)[!vapply(names(attendu), function(k)
    !is.null(cas[[k]]) && identical(cas[[k]]$module, "simulation") && identical(cas[[k]]$motif, attendu[[k]]), NA)]
  msg <- function(k) conditionMessage(cas[[k]])
  if (!identical(cpt$n, 0L)) sprintf("%d appel(s) de sous_graine() pendant les refus", cpt$n)
  else if (length(ko)) paste("sans le refus attendu :", paste(ko, collapse = ", "))
  else if (!identical(msg("annee"), paste("Défauts : déclaration 1 : type annee_hors_periode : aucune unité éligible par",
                                          "construction (aucune colonne annee dans les agrégats : par_annee = FALSE), quel que",
                                          "soit le tirage.")) ||
           !grepl("type « sexe_manquant » inconnu ; types admis : « cellule_dupliquee »", msg("lignes"), fixed = TRUE) ||
           !identical(msg("cible"), "Défauts : déclaration 1 : type « exposition_negative » (unité cellule) : champ cible interdit.") ||
           !grepl("combinaisons admises : aucune.", msg("combinaison"), fixed = TRUE) ||
           !grepl("type « exposition_negative » inconnu ; types admis : « contrat_duplique »", msg("agregats_en_lignes"),
                  fixed = TRUE) ||
           !grepl("manquants : « unite_observation »", msg("unite_manquante"), fixed = TRUE) ||
           !grepl("inconnus : « unite_observation »", msg("unite_inattendue"), fixed = TRUE) ||
           !identical(msg("tete_rachat"), paste("Forme du jeu simulé : unite_observation « tete » sous la transition",
                                                "« en_cours -> rachat » : le rachat est une transition du contrat ; unité",
                                                "admise : « contrat ».")))
    "message différent de l'attendu"
  else TRUE
})
verifier("A4 : defauts_ineligibles après la projection (deux appels de sous_graine() : jeu, puis défauts), état .Random.seed de l'appelant inchangé", {
  ns <- asNamespace("tablexp"); cpt <- new.env(); cpt$n <- 0L
  suppressMessages(trace("sous_graine", tracer = bquote(assign("n", get("n", envir = .(cpt)) + 1L, envir = .(cpt))),
                         where = ns, print = FALSE))
  set.seed(5L); avant <- .Random.seed
  r <- tryCatch(rf(simuler(LOI, PF, PER, GRAINE_JEU, forme = fa(),
                           defauts = dft(list(list(type = "exposition_negative", nombre = 100000L)), GRAINES_DEFAUTS[["refus"]]))),
                finally = suppressMessages(untrace("sous_graine", where = ns)))
  !is.null(r) && identical(r$motif, "defauts_ineligibles") && identical(cpt$n, 2L) && identical(.Random.seed, avant) &&
    identical(r$details$declaration, 1L) && r$details$eligibles > 0L && r$details$eligibles < 100000L
})
# Disjonction : sous taux = 1, exposition_negative retient toutes les cellules
# exposées sans événement et evenements_non_entiers toutes les autres cellules
# exposées : evenements_sans_exposition n'a plus d'éligible ;
# annee_hors_periode reçoit toutes les cellules restantes.
verifier("A4 (disjonction) : une cellule altérée au plus une fois ; taux = 1 sur tous les types : chaque cellule propre au plus une fois dans lignes, copies jamais retenues ensuite ; éligibles épuisés par les déclarations précédentes", {
  decl <- lapply(c("cellule_dupliquee", "exposition_negative", "evenements_non_entiers", "evenements_sans_exposition",
                   "annee_hors_periode"), function(t) list(type = t, taux = 1))
  decl[[1L]] <- list(type = "cellule_dupliquee", nombre = 3L)
  a <- simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(decl, GRAINES_DEFAUTS[["disjonction"]]),
               forme = fa(convention = "age_atteint_annee", par_annee = TRUE))
  l <- attr(a, "defauts")$lignes; N <- nrow(PROPRES$aa_annee)
  dc <- attr(a, "defauts")$decomptes$declarations
  !anyDuplicated(l$ligne) && all(l$ligne[l$role != "copie"] <= N) && all(l$ligne[l$role == "copie"] > N) &&
    sum(l$role != "copie") == N && all(dc$retenues[c(1L, 2L, 3L, 5L)] > 0L) && identical(dc$eligibles[4L], 0L) &&
    identical(dc$retenues[1L], 3L) && identical(dc$eligibles[5L], N - sum(dc$retenues[1:4]))
})

# --- A5 : recoupement contre le moteur en lignes -------------------------------------------

# Projection du simulateur contre la table d'exposition d'une construction en
# lignes sur le jeu propre (recette lignes_dates_completes) : unité contrat,
# rôles sans id_tete ; unité tête, id_tete et id_contrat déclarés.
RECOUPEMENT <- local({
  e <- character(0); ecart <- 0
  cas <- list(
    list(pf = PF, f = fa(convention = "age_atteint_annee"), roles = ROLES_LIGNES),
    list(pf = PF, f = fa(RA, convention = "age_atteint_annee"), roles = ROLES_LIGNES),
    list(pf = PF, f = fa(convention = "age_atteint_annee", par_annee = TRUE), roles = ROLES_LIGNES),
    list(pf = PFM, f = fa(unite = "tete"), roles = ROLES_LIGNES),
    list(pf = PFM, f = fa(unite = "contrat"), roles = ROLES_LIGNES[names(ROLES_LIGNES) != "id_tete"]),
    list(pf = PFM, f = fa(RA, unite = "contrat"), roles = ROLES_LIGNES),
    list(pf = PFM, f = fa(convention = "age_atteint_annee", unite = "tete"), roles = ROLES_LIGNES),
    list(pf = PFM, f = fa(par_annee = TRUE, unite = "tete"), roles = ROLES_LIGNES))
  jeux <- list(PF = simuler(LOI, PF, PER, GRAINE_JEU), PFM = simuler(LOI, PFM, PER, GRAINE_JEU))
  for (cs in cas) {
    a <- simuler(LOI, cs$pf, PER, GRAINE_JEU, forme = cs$f)
    j <- jeux[[if (identical(cs$pf, PF)) "PF" else "PFM"]]
    el <- construire_table(j, cs$roles, plan_ag(cs$f))$etapes$exposition$resultat$exposition
    cles <- c("sexe", "age", if (cs$f$par_annee) "annee")
    lab <- sprintf("%s -> %s, %s, par_annee = %s, unité %s", cs$f$transition$depart, cs$f$transition$arrivee,
                   cs$f$convention, cs$f$par_annee, if (is.null(cs$f$unite_observation)) "simple" else cs$f$unite_observation)
    if (!identical(a[cles], el[cles])) { e <- c(e, paste(lab, ": cellules différentes")); next }
    if (!identical(a$evenements, el$evenements)) e <- c(e, paste(lab, ": D différents"))
    ecart <- max(ecart, rel(a$exposition_centrale, el$exposition_centrale))
    if (rel(a$exposition_centrale, el$exposition_centrale) > 1e-10) e <- c(e, paste(lab, ": E^c au-delà de 1e-10"))
    if (!is.null(el$complement_initial_cellule)) {
      ecart <- max(ecart, rel(a$exposition_initiale, el$exposition_centrale + el$complement_initial_cellule))
      if (rel(a$exposition_initiale, el$exposition_centrale + el$complement_initial_cellule) > 1e-10)
        e <- c(e, paste(lab, ": E^i au-delà de 1e-10"))
    }
    if (sum(a$evenements) < 50L) e <- c(e, paste(lab, ": moins de 50 événements (cas vide)"))
  }
  list(ecarts = e, ecart = ecart, cas = length(cas))
})
verifier(sprintf("A5 (approximation numérique, tolérance relative 1e-10) : simulateur <-> moteur en lignes, age_atteint_annee et modes multi-contrats (%d cas) : mêmes cellules, D identical(), E^c et E^i à 1e-10 (écart maximal mesuré : %.3g)", RECOUPEMENT$cas, RECOUPEMENT$ecart), {
  if (length(RECOUPEMENT$ecarts)) paste(RECOUPEMENT$ecarts, collapse = " | ") else TRUE
})
verifier("A5 (approximation numérique, 1e-12) : age_atteint_annee, somme sur les années de E^c, E^i et D par année = valeurs sans année (cellules (sexe, âge))", {
  a <- PROPRES$aa; b <- PROPRES$aa_annee
  s <- function(v) { r <- rowsum(v, paste(b$sexe, b$age), reorder = FALSE); r[match(paste(a$sexe, a$age), rownames(r)), 1L] }
  rel(s(b$exposition_initiale), a$exposition_initiale) <= 1e-12 &&
    rel(s(b$exposition_centrale), a$exposition_centrale) <= 1e-12 && identical(as.integer(s(b$evenements)), a$evenements)
})
verifier("A5 : multi-contrats, unité tête : moins d'événements qu'en unité contrat (une tête décédée à K contrats compte K événements en contrats)", {
  t <- simuler(LOI, PFM, PER, GRAINE_JEU, forme = fa(unite = "tete"))
  k <- simuler(LOI, PFM, PER, GRAINE_JEU, forme = fa(unite = "contrat"))
  j <- simuler(LOI, PFM, PER, GRAINE_JEU)
  dans <- j$cause_sortie == "deces" & !is.na(j$date_sortie) & j$date_sortie >= PER$debut & j$date_sortie <= PER$fin
  sum(k$evenements) == sum(dans) && sum(t$evenements) == length(unique(j$id_tete[dans])) && sum(t$evenements) < sum(k$evenements)
})

# --- A6 : constructions en agrégats age_atteint_annee ----------------------------------------

verifier("A6 (identité exacte) : construction en agrégats age_atteint_annee = construction en lignes (brut identical(), sans année : poisson et binomial ; par année : poisson) ; positionnement refusé dans les deux cas", {
  j <- simuler(LOI, PF, PER, GRAINE_JEU)
  T_HF <- table_reference(rbind(data.frame(age = GM$age, sexe = "H", qx = GM$qx),
                                data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx)),
                          "GM vraie par sexe", "importee", "loi vraie du jeu simulé (test)")
  POS <- list(positionnement = list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95))
  ok <- logical(0)
  for (pa in c(FALSE, TRUE)) {
    f <- fa(convention = "age_atteint_annee", par_annee = pa)
    a <- if (pa) PROPRES$aa_annee else PROPRES$aa
    br <- list(brut = list(estimateurs = c("poisson", if (!pa) "binomial"), niveau = 0.95))
    xl <- construire_table(j, ROLES_LIGNES, plan_ag(f, etapes = br))
    xa <- construire_table(a, roles_de(a), plan_ag(f, etapes = br))
    ra <- rf(construire_table(a, roles_de(a), plan_ag(f, etapes = c(br, POS)), tables_reference = list(tv = T_HF)))
    ok <- c(ok, identical(xa$etapes$brut$resultat, xl$etapes$brut$resultat),
            !is.null(ra) && identical(ra$motif, "convention_positionnement_non_admise"))
  }
  all(ok)
})

# --- A7 : clôture --------------------------------------------------------------------------

verifier("A7 (valeur écrite à la main) : motifs du catalogue des agrégats et 13 motifs hors périmètre = MOTIFS_LIGNES_AGREGATS ; catalogue des lignes inchangé (18 types)", {
  hors <- c("age_manquant", "age_non_entier", "age_negatif", "age_aberrant", "annee_manquante", "annee_non_entiere",
            "sexe_manquant", "sexe_hors_modalites", "segment_manquant", "exposition_manquante", "exposition_non_finie",
            "evenements_manquants", "evenements_negatifs")
  m <- tablexp:::MOTIFS_LIGNES_AGREGATS
  identical(CAT_AG$defaut, TYPES) && identical(CAT_AG$motif_attendu, TYPES) &&
    setequal(c(CAT_AG$motif_attendu, hors), m) && length(c(CAT_AG$motif_attendu, hors)) == length(m) &&
    !any(hors %in% CAT_AG$motif_attendu) && identical(unique(CAT_AG$refus_attendu), "ligne_invalide") &&
    identical(tablexp:::DEFAUTS_FORMES$lignes$catalogue, tablexp:::CATALOGUE_DEFAUTS) &&
    nrow(tablexp:::CATALOGUE_DEFAUTS) == 18L &&
    identical(tablexp:::DEFAUTS_FORMES$lignes$ordre_tirages, tablexp:::ORDRE_TIRAGES_DEFAUTS) &&
    identical(tablexp:::DEFAUTS_FORMES$lignes$combinaisons, tablexp:::COMBINAISONS_DEFAUTS)
})
verifier("A7 : stocks successifs avec defauts toujours refusés (forme_invalide, #191) ; conditions_vides() et controler_defauts() aux appels existants inchangés", {
  r <- rf(simuler(LOI, PF, PER, GRAINE_JEU, defauts = dft(list(list(type = "sexe_manquant", cible = "deces", nombre = 1L))),
                  forme = list(nom = "stocks_successifs", inventaires = D(c("2014-12-31", "2024-12-31")))))
  v <- tablexp:::conditions_vides("uniforme", PF, PER)
  !is.null(r) && identical(r$motif, "forme_invalide") && grepl("defauts", conditionMessage(r), fixed = TRUE) &&
    identical(v, tablexp:::conditions_vides("uniforme", PF, PER, NULL)) &&
    identical(tablexp:::conditions_vides("uniforme", PF, PER, list(par_annee = TRUE)), stats::setNames(character(0), character(0)))
})
