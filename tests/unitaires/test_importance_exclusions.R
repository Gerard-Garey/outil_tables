# -----------------------------------------------------------------------------
#  tests/unitaires/test_importance_exclusions.R : importance des exclusions
#  (issue #69 ; spécification d'actuary-approfondi, #69, commentaire
#  6663938080 ; M215, M238, M242) ; fichier propre à B13 (ADR 0013, point 3)
#
#  Commit (4a) : factorisation à résultats identiques (avis de forme (0''),
#  #4, commentaire 6679524403) :
#  - construire_observations(), extraite de preparer_donnees() ;
#  - argument facultatif lignes_tete d'evaluer_motifs().
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : observations, relevés et motifs des petits
#    jeux écrits ici ;
#  - identité exacte : construire_observations() et champs de
#    preparer_donnees() sur un jeu simulé à exclusions ; lignes_tete = NULL,
#    omis, ou égal aux lignes exemptes de motif de ligne.
#  Les mêmes objets identical() à l'état d'avant la factorisation (e9cbf53)
#  sont mesurés hors batterie (compte rendu du commit).
# -----------------------------------------------------------------------------

D <- as.Date
construire_observations <- tablexp:::construire_observations
evaluer_motifs <- tablexp:::evaluer_motifs
preparer_donnees <- tablexp:::preparer_donnees
valeurs <- tablexp:::valeurs_parametres_donnees

PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))  # borne 2025-01-01 = 20089
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
P_VD <- valeurs(VD, PER, 0)

# Quatre contrats, trois têtes ; T3 : deux contrats chevauchants, décès.
BASE <- data.frame(
  contrat = c("C1", "C2", "C3", "C4"),
  tete = c("T1", "T2", "T3", "T3"),
  sexe = c("H", "F", "H", "H"),
  naissance = D(c("1950-03-15", "1960-07-01", "1945-06-06", "1945-06-06")),
  entree = D(c("2016-01-01", "2020-03-01", "2016-01-01", "2019-01-01")),
  sortie = D(c("2019-12-31", NA, "2022-03-01", "2022-03-01")),
  cause = c("rachat", "en_cours", "deces", "deces"),
  stringsAsFactors = FALSE)
ROLES <- c(id_contrat = "contrat", id_tete = "tete", sexe = "sexe", date_naissance = "naissance",
           date_entree = "entree", date_sortie = "sortie", cause_sortie = "cause")

# --- construire_observations() : valeurs calculées à la main -------------------

ob <- construire_observations(BASE, 1:4, ROLES, "sexe", P_VD)
verifier("construire_observations, unité tête (valeur calculée à la main) : T3 fusionnée [2016-01-01, 2022-03-01) décès ; T1 censure, T2 en cours",
         identical(ob$unite, "tete") &&
           identical(ob$observations$unite, c("T1", "T2", "T3")) &&
           identical(ob$observations$ligne, c(1L, 2L, 3L)) &&
           identical(ob$observations$nb_lignes, c(1L, 1L, 2L)) &&
           identical(ob$observations$sexe, c("H", "F", "H")) &&
           identical(ob$observations$debut, D(c("2016-01-01", "2020-03-01", "2016-01-01"))) &&
           identical(ob$observations$fin, D(c("2019-12-31", NA, "2022-03-01"))) &&
           identical(ob$observations$cause, c("censure", "en_cours", "deces")) &&
           identical(ob$observation_des_lignes, c(1L, 2L, 3L, 3L)))
verifier("construire_observations, relevés (valeur calculée à la main) : 3 têtes, 1 multi-contrats, 1 chevauchement, 3 observations",
         identical(ob$releves, list(tetes = 3L, tetes_multi_contrats = 1L, chevauchements = 1L, contiguites = 0L,
                                    disjoints = 0L, ecarts_un_jour = 0L, fenetres_vides = 0L, observations = 3L,
                                    observations_apres_deces_hors_periode = 0L)))
verifier("construire_observations, numéros reçus (valeur calculée à la main) : lignes 2 et 4 de l'entrée, colonne ligne = c(2, 4)", {
  o <- construire_observations(BASE[c(2L, 4L), ], c(2L, 4L), ROLES, "sexe", P_VD)
  identical(o$observations$ligne, c(2L, 4L)) && identical(o$observations$unite, c("T2", "T3")) &&
    identical(o$observation_des_lignes, c(1L, 2L)) && identical(o$releves$tetes_multi_contrats, 0L)
})
verifier("construire_observations, unité contrat (en_cours -> rachat ; valeur calculée à la main) : 4 observations, causes de l'entrée, id_tete recopié", {
  o <- construire_observations(BASE, 1:4, ROLES, "sexe", valeurs(RA, PER, 0))
  identical(o$unite, "contrat") && identical(o$observations$unite, c("C1", "C2", "C3", "C4")) &&
    identical(o$observations$id_tete, c("T1", "T2", "T3", "T3")) &&
    identical(o$observations$cause, c("rachat", "en_cours", "deces", "deces")) &&
    identical(o$observation_des_lignes, 1:4) && identical(o$releves$chevauchements, 1L)
})
verifier("construire_observations, sans id_tete (valeur calculée à la main) : unité contrat, relevés de fusion NA", {
  o <- construire_observations(BASE, 1:4, ROLES[names(ROLES) != "id_tete"], "sexe", P_VD)
  identical(o$unite, "contrat") && is.na(o$releves$tetes) && is.na(o$releves$observations_apres_deces_hors_periode) &&
    identical(o$releves$observations, 4L) && is.null(o$observations$id_tete)
})

# Contrefactuel : ligne 3 (première ligne de T3) sans sexe, exclue par
# sexe_manquant ; sur toutes les lignes, mêmes observations que le jeu
# complet, hors colonne sexe (lue sur la ligne 3).
B_NA <- BASE; B_NA$sexe[3L] <- NA
verifier("construire_observations, contrefactuel (identité exacte) : lignes reçues = toutes, mêmes observations que le jeu sans défaut hors colonne sexe", {
  o <- construire_observations(B_NA, 1:4, ROLES, character(0), P_VD)
  r <- preparer_donnees(BASE, ROLES, P_VD, character(0), character(0), tablexp:::deduire_recette(ROLES))$resultat
  ex <- preparer_donnees(B_NA, ROLES, P_VD, character(0), "sexe_manquant", tablexp:::deduire_recette(ROLES))$resultat
  garde <- setdiff(names(o$observations), "sexe")
  identical(o$observations[garde], r$observations[garde]) && identical(o$observation_des_lignes, c(1L, 2L, 3L, 3L)) &&
    identical(ex$numeros_conserves, c(1L, 2L, 4L)) && identical(ex$observations$ligne, c(1L, 2L, 4L)) &&
    identical(o$observations$sexe, c("H", "F", NA))
})

# --- identité avec preparer_donnees() sur un jeu simulé à exclusions -----------

TOUS_MOTIFS <- c(tablexp:::MOTIFS_LIGNES, tablexp:::MOTIFS_TETE)
CAT <- tablexp:::CATALOGUE_DEFAUTS
TOUTES <- c("deces", "rachat", "en_cours")
LOI <- list(deces = list(table = tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120), facteurs = c(H = 1, F = 0.6)),
            rachat = list(table = tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %"),
                          facteurs = c(H = 1, F = 1)))
PF <- list(mode = "uniforme_multi", n = 2000, proportion_hommes = 0.5, age_min = 20, age_max = 75,
           origine = D("2005-01-01"), loi_contrats = c(0.6, 0.3, 0.1))
DECL <- lapply(setdiff(CAT$defaut, "date_hors_domaine"), function(t)
  if (CAT$unite[CAT$defaut == t] == "ligne") list(type = t, cible = TOUTES, nombre = 10) else list(type = t, nombre = 10))
SC <- tablexp::simuler_portefeuille(LOI, PF, PER, 20261016L, defauts = list(graine = 20261025L, declarations = DECL))
ROLES_SIM <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
               date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
CHAMPS <- c("unite_observation", "observations", "observation_des_lignes")
for (cas in list(list("vivant -> deces, unité tête, délai 30", VD, ROLES_SIM, 30, TOUS_MOTIFS),
                 list("en_cours -> rachat, unité contrat", RA, ROLES_SIM, 0, TOUS_MOTIFS),
                 list("vivant -> deces, sans id_tete", VD, ROLES_SIM[names(ROLES_SIM) != "id_tete"], 0,
                      tablexp:::MOTIFS_LIGNES))) {
  d <- if (identical(cas[[3]], ROLES_SIM)) SC else SC[!(seq_len(nrow(SC)) %in% attr(SC, "defauts")$lignes$ligne[
    attr(SC, "defauts")$lignes$unite == "tete"]), ]
  r <- preparer_donnees(d, cas[[3]], valeurs(cas[[2]], PER, cas[[4]]), "sexe", cas[[5]], tablexp:::deduire_recette(cas[[3]]))
  o <- construire_observations(r$resultat$lignes_conservees, r$resultat$numeros_conserves, cas[[3]], "sexe",
                               valeurs(cas[[2]], PER, cas[[4]]))
  verifier(sprintf("construire_observations = champs de preparer_donnees() (identité exacte), jeu simulé à %d lignes exclues : %s",
                   r$resultat$nombre_lignes_exclues, cas[[1]]),
           r$resultat$nombre_lignes_exclues > 0L &&
             identical(unname(o[c("unite", "observations", "observation_des_lignes")]), unname(r$resultat[CHAMPS])) &&
             identical(o$releves, r$diagnostics$releves))
}

# --- evaluer_motifs(), argument lignes_tete --------------------------------------

P <- valeurs(VD, PER, 0)
mot <- function(d, ...) evaluer_motifs(d, ROLES_SIM, P$debut, P$fin, P$borne, "sexe", ...)$motifs
M0 <- mot(SC)
verifier("evaluer_motifs, lignes_tete = NULL (identité exacte) : identical() à l'appel sans l'argument (jeu simulé à défauts)",
         identical(mot(SC, lignes_tete = NULL), M0) && sum(lengths(M0[tablexp:::MOTIFS_TETE])) > 0L)
verifier("evaluer_motifs, lignes_tete = lignes exemptes de motif de ligne (identité exacte) : identical() au défaut", {
  propres <- setdiff(seq_len(nrow(SC)), unlist(M0[tablexp:::MOTIFS_LIGNES], use.names = FALSE))
  identical(mot(SC, lignes_tete = propres), M0)
})

# T1 : ligne 1 décès en 2018 ; ligne 2 de même tête, sexe hors modalités
# (motif de ligne), en cours ; T2 seule.
B_T <- data.frame(id_contrat = c("C1", "C2", "C3"), id_tete = c("T1", "T1", "T2"), sexe = c("H", "X", "F"),
                  date_naissance = D(c("1950-03-15", "1950-03-15", "1960-07-01")),
                  date_entree = D(c("2016-01-01", "2019-01-01", "2016-01-01")),
                  date_sortie = D(c("2018-06-30", NA, NA)), cause_sortie = c("deces", "en_cours", "en_cours"),
                  stringsAsFactors = FALSE)
vides <- function(m, codes) all(lengths(m[codes]) == 0L)
verifier("evaluer_motifs, défaut (valeur calculée à la main) : ligne 2 sexe_hors_modalites, non propre ; aucun motif de tête", {
  m <- mot(B_T)
  identical(m$sexe_hors_modalites, 2L) && vides(m, tablexp:::MOTIFS_TETE) &&
    vides(m, setdiff(tablexp:::MOTIFS_LIGNES, "sexe_hors_modalites"))
})
verifier("evaluer_motifs, lignes_tete = 1:3 (valeur calculée à la main) : T1 tete_observee_apres_deces sur les lignes 1 et 2 ; sexes (H, \"X\") cohérents (hors modalités traité comme manquant) ; motifs de ligne inchangés", {
  m <- mot(B_T, lignes_tete = 1:3)
  identical(m$tete_observee_apres_deces, 1:2) &&
    vides(m, c("tete_sexe_incoherent", "tete_naissance_incoherente", "tete_segment_incoherent")) &&
    identical(m[tablexp:::MOTIFS_LIGNES], mot(B_T)[tablexp:::MOTIFS_LIGNES])
})
verifier("evaluer_motifs, lignes_tete = c(3L, 1L) (valeur calculée à la main) : T1 réduite à la ligne 1, aucun motif de tête",
         vides(mot(B_T, lignes_tete = c(3L, 1L)), tablexp:::MOTIFS_TETE))
for (k in list(list("double", c(1, 2)), list("NA", c(1L, NA)), list("doublon", c(1L, 1L)), list("0", 0:1),
               list("hors de nrow", 3:4), list("attribut", c(a = 1L))))
  verifier(sprintf("evaluer_motifs, lignes_tete hors de la forme admise (%s) : erreur de programmation, pas un refus", k[[1]]), {
    e <- tryCatch({ mot(B_T, lignes_tete = k[[2]]); NULL }, error = function(e) e)
    !is.null(e) && !inherits(e, "refus_tablexp") && grepl("lignes_tete", conditionMessage(e), fixed = TRUE)
  })

# Valeurs manquantes dans les lignes désignées (décision d'actuary, #69) :
# une tête est incohérente si et seulement si elle porte au moins deux
# valeurs valides distinctes ; sexe hors modalités traité comme manquant.
# T1 : lignes 1 à 3 ; T2 : ligne 4 ; aucun décès.
B_V <- data.frame(id_contrat = paste0("C", 1:4), id_tete = c("T1", "T1", "T1", "T2"), sexe = c("H", "F", NA, "F"),
                  date_naissance = D(c("1950-03-15", "1950-03-15", "1950-03-15", "1960-07-01")),
                  date_entree = D(c("2016-01-01", "2017-01-01", "2018-01-01", "2016-01-01")),
                  date_sortie = D(c("2016-12-31", "2017-12-31", NA, NA)),
                  cause_sortie = c("rachat", "rachat", "en_cours", "en_cours"), stringsAsFactors = FALSE)
avec <- function(d, colonne, valeurs) { d[[colonne]] <- valeurs; d }
verifier("evaluer_motifs, lignes_tete = 1:4, sexes de T1 (H, F, NA) (valeur calculée à la main) : tete_sexe_incoherent sur les lignes 1 à 3",
         identical(mot(B_V, lignes_tete = 1:4)$tete_sexe_incoherent, 1:3))
verifier("evaluer_motifs, lignes_tete = c(1L, 3L, 4L), sexes de T1 (H, NA) (valeur calculée à la main) : aucun motif de tête",
         vides(mot(B_V, lignes_tete = c(1L, 3L, 4L)), tablexp:::MOTIFS_TETE))
verifier("evaluer_motifs, lignes_tete = 1:4, sexes de T1 (H, \"X\", NA) (valeur calculée à la main) : aucun motif de tête ; ligne 2 sexe_hors_modalites", {
  m <- mot(avec(B_V, "sexe", c("H", "X", NA, "F")), lignes_tete = 1:4)
  vides(m, tablexp:::MOTIFS_TETE) && identical(m$sexe_hors_modalites, 2L) && identical(m$sexe_manquant, 3L)
})
verifier("evaluer_motifs, lignes_tete = 1:4, sexes de T1 (H, \"X\", F) (valeur calculée à la main) : tete_sexe_incoherent sur les lignes 1 à 3 (H et F valides)",
         identical(mot(avec(B_V, "sexe", c("H", "X", "F", "F")), lignes_tete = 1:4)$tete_sexe_incoherent, 1:3))
B_N <- avec(avec(B_V, "sexe", c("H", "H", "H", "F")), "date_naissance",
            D(c("1950-03-15", NA, "1950-03-15", "1960-07-01")))
verifier("evaluer_motifs, lignes_tete = 1:4, naissances de T1 (j, NA, j) (valeur calculée à la main) : aucun motif de tête ; ligne 2 naissance_manquante", {
  m <- mot(B_N, lignes_tete = 1:4)
  vides(m, tablexp:::MOTIFS_TETE) && identical(m$naissance_manquante, 2L)
})
verifier("evaluer_motifs, lignes_tete = 1:4, naissances de T1 (j, NA, j + 1) (valeur calculée à la main) : tete_naissance_incoherente sur les lignes 1 à 3",
         identical(mot(avec(B_N, "date_naissance", D(c("1950-03-15", NA, "1950-03-16", "1960-07-01"))),
                       lignes_tete = 1:4)$tete_naissance_incoherente, 1:3))
# Lignes non mesurables (#69, § 2) désignées : erreur de programmation, pas
# un refus ni une erreur R brute. Défaut : la ligne est exclue des lignes
# propres, sans erreur.
for (k in list(list("id_tete NA", avec(B_V, "id_tete", c("T1", NA, "T1", "T2")), "id_tete_manquant"),
               list("cause NA, tête avec décès", avec(avec(B_V, "cause_sortie", c("deces", NA, "en_cours", "en_cours")),
                                                     "date_sortie", D(c("2016-12-31", "2017-12-31", NA, NA))),
                    "cause_manquante"),
               list("sortie NA, cause deces", avec(B_V, "cause_sortie", c("rachat", "rachat", "deces", "en_cours")),
                    "sortie_manquante")))
  verifier(sprintf("evaluer_motifs, lignes_tete désigne une ligne non mesurable (%s) : erreur de programmation ; sans lignes_tete, motif de ligne, aucune erreur", k[[1]]), {
    e <- tryCatch({ mot(k[[2]], lignes_tete = 1:4); NULL }, error = function(e) e)
    m <- mot(k[[2]])
    !is.null(e) && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE) &&
      grepl("lignes_tete", conditionMessage(e), fixed = TRUE) && length(m[[k[[3]]]]) == 1L
  })

# Jeu simulé à défauts, lignes_tete = lignes du contrefactuel (lignes
# exemptes de motif de ligne non mesurable : ni id_tete, ni cause, ni sortie
# manquants) : aucune erreur, motifs de ligne inchangés, motifs de tête
# évalués.
verifier("evaluer_motifs, jeu simulé à défauts, lignes_tete = lignes sans id_tete, cause ni sortie manquants (constat d'exécution) : aucune erreur, motifs de ligne identiques au défaut", {
  nm <- unlist(M0[c("id_tete_manquant", "cause_manquante", "sortie_manquante")], use.names = FALSE)
  ct <- setdiff(seq_len(nrow(SC)), nm)
  m <- tryCatch(mot(SC, lignes_tete = ct), error = function(e) e)
  length(ct) < nrow(SC) && length(ct) > length(setdiff(seq_len(nrow(SC)), unlist(M0[tablexp:::MOTIFS_LIGNES]))) &&
    !inherits(m, "error") && identical(m[tablexp:::MOTIFS_LIGNES], M0[tablexp:::MOTIFS_LIGNES]) &&
    sum(lengths(m[tablexp:::MOTIFS_TETE])) > 0L
})

# =============================================================================
#  Commit (4b) : étape importance_exclusions (spécification, § 1 à § 4 ;
#  décisions du 06/11/2026, commentaire 6686457325 de #69 ; M242, M255,
#  M259). Natures des attendus, nommées dans chaque test :
#  - valeur calculée à la main : T1, T2 (§ 4 de la spécification ; jeu de 8
#    lignes écrit ici pour les donner), T5 à T8, états du SMR ;
#  - identité exacte : T3 (jeu § 2.6 de test_simulation.R : construction du
#    jeu propre moins construction du jeu altéré), T4 (local = global),
#    comparateur 1 = taux de Poisson de `brut`, reproductibilité ;
#  - constat d'exécution : garde-fous « erreur de programmation ».
# =============================================================================

refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}
de <- function(t, ensemble, niveau) t[t$ensemble == ensemble & t$niveau == niveau, , drop = FALSE]

# Jeu T1 (§ 4, point 1) : naissance 1950-01-01 (âge 65 toute l'année 2015,
# cellule [2015-01-01, 2016-01-01), 365 jours), période 2015, délai 0.
# Conservées : C (décès 2015-10-01, 273 j), D (en cours, 365 j), E (décès
# 2015-03-30, 88 j), G (sortie 2014, 0 j) : 726 jours, D_r = 2. Exclues
# sexe_manquant : A (décès 2015-04-01, 90 j), B (décès 2015-04-03, 92 j), C
# (second contrat [2015-05-06, 2015-10-01), décès le même jour que le
# premier : 148 j contenus dans le premier) ; exclue entree_manquante : H
# (décès 2015-06-01). S seule : 330 j et 3 décès ; retirés en unité tête :
# 182 j et 2 décès.
T1 <- data.frame(
  id_contrat = paste0("C", 1:8),
  id_tete = c("C", "D", "E", "G", "A", "B", "C", "H"),
  sexe = c("H", "F", "H", "F", NA, NA, NA, "H"),
  date_naissance = D("1950-01-01"),
  date_entree = D(c("2015-01-01", "2015-01-01", "2015-01-01", "2014-01-01", "2015-01-01", "2015-01-01",
                    "2015-05-06", NA)),
  date_sortie = D(c("2015-10-01", NA, "2015-03-30", "2014-12-31", "2015-04-01", "2015-04-03", "2015-10-01",
                    "2015-06-01")),
  cause_sortie = c("deces", "en_cours", "deces", "rachat", "deces", "deces", "deces", "deces"),
  stringsAsFactors = FALSE)
P15 <- list(debut = D("2015-01-01"), fin = D("2015-12-31"))
plan_ie <- function(exclusions = c("entree_manquante", "sexe_manquant"), segmentation = character(0), val = NULL,
                    brut = FALSE, par_annee = FALSE, niveau = 0.95, periode = P15) {
  p <- list(transition = VD, periode = periode, delai_declaration = 0, segmentation = segmentation,
            etapes = list(donnees = list(exclusions = exclusions),
                          exposition = list(convention = "plus_fine", par_annee = par_annee)))
  if (brut) p$etapes$brut <- list(estimateurs = "poisson", niveau = 0.95)
  if (!is.null(val)) { p$etapes$validation <- val; p$graine <- 20261106L }
  p$etapes$importance_exclusions <- list(niveau = niveau)
  p
}
ROLES_SANS_TETE <- ROLES_SIM[names(ROLES_SIM) != "id_tete"]
X1 <- construire_table(T1, ROLES_SIM, plan_ie())
I1 <- X1$etapes$importance_exclusions$resultat
qg <- function(p, k, A) stats::qgamma(p, k) / A

verifier("T1 (valeur calculée à la main) : reçues 8, conservées 4, exclues 4 ; événements déclarés 4 lignes, 4 têtes dont 1 déjà porteuse", {
  e <- I1$evenements_declares[I1$evenements_declares$ensemble == "total", ]
  identical(I1$volumes, data.frame(lignes_recues = 8L, lignes_conservees = 4L, lignes_exclues = 4L)) &&
    e$lignes == 4L && e$evenement_dans_fenetre == 4L && e$tetes_evenement_dans_fenetre == 4L &&
    e$tetes_deja_porteuses == 1L && e$lignes_evenement_sans_identifiant_tete == 0L
})
verifier("T1 (valeur calculée à la main) : retirés 182 jours et 2 décès (niveaux A et B) ; motif entree_manquante aucune_ligne_mesurable", {
  r <- I1$retraits
  a <- de(r, "total", "A"); b <- de(r, "total", "B"); s <- de(r, "sexe_manquant", "A")
  tous(jours = proche(c(a$exposition_jours_retiree, b$exposition_jours_retiree, s$exposition_jours_retiree),
                      rep(182, 3), 0, 0),
       deces = proche(c(a$evenements_retires, b$evenements_retires, s$evenements_retires), rep(2, 3), 0, 0),
       ec = proche(a$exposition_centrale_retiree, 182 / 365, 1e-15, 0),
       etat = if (identical(de(r, "entree_manquante", "A")$etat, "aucune_ligne_mesurable") &&
                  identical(a$etat, "produite") && identical(a$tetes_touchees, 3L) &&
                  identical(a$tetes_indeterminees, 0L)) TRUE else "états")
})
verifier("T1 (valeur calculée à la main) : A = 182/363, SMR = 363/91, exact [0,4830877369 ; 14,4096792494] (qgamma), asymptotique (échelle log) [0,9976422872 ; 15,9498137508], f = 182/726", {
  s <- de(I1$smr_comparateur_1, "total", "A")
  tous(A = proche(s$attendus, 182 / 363, 1e-14, 0), smr = proche(s$smr, 363 / 91, 1e-14, 0),
       exact = proche(c(s$borne_inf_exacte, s$borne_sup_exacte), c(0.4830877369, 14.4096792494), 1e-10, 0),
       qgamma = proche(c(s$borne_inf_exacte, s$borne_sup_exacte),
                       c(qg(0.025, 2, 182 / 363), stats::qgamma(0.025, 3, lower.tail = FALSE) / (182 / 363)), 1e-14, 0),
       asymptotique = proche(c(s$borne_inf_asymptotique, s$borne_sup_asymptotique), c(0.9976422872, 15.9498137508), 1e-10, 0),
       f = proche(s$rapport_variance, 182 / 726, 1e-14, 0),
       etat = if (identical(s$etat, "produite") && identical(de(I1$smr_comparateur_1, "entree_manquante", "A")$etat,
                                                              "aucune_ligne_mesurable")) TRUE else "états")
})
X2 <- construire_table(T1, ROLES_SANS_TETE, plan_ie())
verifier("T2, unité contrat (valeur calculée à la main) : D_e = 3, E_e = 330/365, A = 10/11, SMR = 3,3, exact [0,6805393 ; 9,6440004] ; têtes NA", {
  r <- de(X2$etapes$importance_exclusions$resultat$retraits, "total", "A")
  s <- de(X2$etapes$importance_exclusions$resultat$smr_comparateur_1, "total", "A")
  tous(De = proche(r$evenements_retires, 3, 0, 0), jours = proche(r$exposition_jours_retiree, 330, 0, 0),
       Ee = proche(r$exposition_centrale_retiree, 330 / 365, 1e-15, 0), A = proche(s$attendus, 10 / 11, 1e-14, 0),
       smr = proche(s$smr, 3.3, 1e-14, 0),
       exact = proche(c(s$borne_inf_exacte, s$borne_sup_exacte), c(0.6805393, 9.6440004), 1e-7, 0),
       tetes = if (is.na(r$tetes_touchees) && is.na(r$tetes_indeterminees) &&
                   identical(X2$etapes$importance_exclusions$parametres$unite_observation, "contrat")) TRUE else "têtes")
})

# --- condition 1 du visa : classement M / N, seule source des ensembles S -----

verifier("classement (valeur calculée à la main) : table du § 2 par unité et clés", {
  cm <- tablexp:::classe_motifs
  m <- c("id_contrat_manquant", "contrat_duplique", "sexe_manquant", "sexe_hors_modalites", "segment_manquant",
         "id_tete_manquant", "naissance_manquante", "cause_hors_modalites", "age_aberrant",
         "annee_naissance_incoherente", "tete_sexe_incoherent")
  identical(cm(m, "tete", character(0)), c("M", "M", "M", "M", "M", "N", "N", "N", "N", "N", "N")) &&
    identical(cm(m, "contrat", c("sexe", "segment")), c("M", "N", "cle", "cle", "cle", "M", "N", "N", "N", "N", "N")) &&
    identical(cm(m, "tete", "sexe"), c("M", "M", "cle", "cle", "M", "N", "N", "N", "N", "N", "N"))
})
verifier("classement des lignes (valeur calculée à la main) : une ligne est M si tous ses motifs le sont, N si l'un est N, sinon cle", {
  k <- tablexp:::classer_lignes_exclues(list(sexe_manquant = c(1L, 2L, 3L), naissance_manquante = 2L,
                                             id_contrat_manquant = c(3L, 4L)), "tete", "sexe")
  identical(k$lignes, 1:4) && identical(k$classe, c("cle", "N", "cle", "M"))
})
# Lignes 9 et 10 : cause hors modalités (et sexe manquant) ; naissance
# manquante (et sexe manquant) : non mesurables, hors du contrefactuel.
T1N <- rbind(T1, data.frame(id_contrat = c("C9", "C10"), id_tete = c("M", "N"), sexe = NA_character_,
                            date_naissance = D(c("1950-01-01", NA)), date_entree = D("2015-01-01"),
                            date_sortie = D(c("2015-05-01", NA)), cause_sortie = c("DECES", "en_cours"),
                            stringsAsFactors = FALSE))
verifier("condition 1 (valeur calculée à la main) : cause_hors_modalites et naissance_manquante comptées en N, hors du contrefactuel (retirés de T1 inchangés)", {
  x <- construire_table(T1N, ROLES_SIM, plan_ie(c("entree_manquante", "naissance_manquante", "cause_hors_modalites",
                                                   "sexe_manquant")))
  i <- x$etapes$importance_exclusions$resultat
  m <- i$motifs[i$motifs$ensemble == "sexe_manquant", ]
  e <- i$evenements_declares[i$evenements_declares$ensemble == "total", ]
  m$lignes == 5L && m$lignes_mesurables_niveau_a == 3L && m$lignes_non_mesurables == 2L &&
    identical(unlist(de(i$retraits, "total", "A")[c("evenements_retires", "exposition_jours_retiree")]),
              unlist(de(I1$retraits, "total", "A")[c("evenements_retires", "exposition_jours_retiree")])) &&
    identical(de(i$retraits, "cause_hors_modalites", "A")$etat, "aucune_ligne_mesurable") &&
    identical(de(i$retraits, "naissance_manquante", "B")$etat, "aucune_ligne_mesurable") &&
    e$cause_inconnue_sortie_dans_fenetre == 1L && e$sans_evenement_dans_fenetre == 1L
})
verifier("condition 1 (constat d'exécution) : S contenant une ligne N -> erreur de programmation, pas un refus", {
  ctx <- list(lignes_exclues = list(naissance_manquante = 2L, sexe_manquant = 3L), unite = "tete", par_annee = FALSE)
  e <- tryCatch({ tablexp:::retrait_contrefactuel(2L, character(0), ctx); NULL }, error = function(e) e)
  !is.null(e) && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
})
verifier("condition 1 (constat d'exécution) : ligne N d'une tête touchée qui entrerait dans lignes_tete -> erreur de programmation", {
  ctx <- list(lignes_exclues = list(naissance_manquante = 2L, sexe_manquant = 3L), unite = "tete", par_annee = FALSE,
              id_tete = c("a", "b", "b"), R = c(1L, 2L))
  e <- tryCatch({ tablexp:::retrait_contrefactuel(3L, character(0), ctx); NULL }, error = function(e) e)
  !is.null(e) && !inherits(e, "refus_tablexp") && grepl("lignes_tete", conditionMessage(e), fixed = TRUE)
})

# --- condition 2 : lignes_tete en unité tête seulement ---------------------------

ns <- asNamespace("tablexp")
appels_lignes_tete <- function(expr) {
  n <- new.env(); n$k <- 0L
  suppressMessages(trace("evaluer_motifs", where = ns, print = FALSE,
                         tracer = bquote(if (!is.null(lignes_tete)) assign("k", get("k", envir = .(n)) + 1L, envir = .(n)))))
  on.exit(suppressMessages(untrace("evaluer_motifs", where = ns)))
  force(expr)
  n$k
}
verifier("condition 2 (constat d'exécution) : evaluer_motifs() appelée avec lignes_tete en unité tête, jamais en unité contrat", {
  a <- appels_lignes_tete(construire_table(T1, ROLES_SIM, plan_ie()))
  b <- appels_lignes_tete(construire_table(T1, ROLES_SANS_TETE, plan_ie()))
  if (a > 0L && b == 0L) TRUE else sprintf("unité tête %d appel(s), unité contrat %d", a, b)
})

# --- T5 : garde de réintégration (valeur calculée à la main) --------------------

# G1 : décès 2015-03-01, ligne exclue en cours depuis 2015-04-01 (observée
# après décès) ; G2 : ligne exclue de naissance différente ; G3 : ligne
# exclue (sexe NA face à H : cohérente) contenue dans la conservée ; G4 :
# exclue seule, décès 2015-07-01 (181 j).
T5 <- data.frame(id_contrat = paste0("K", 1:7), id_tete = c("G1", "G2", "G3", "G1", "G2", "G3", "G4"),
                 sexe = c("H", "F", "H", NA, NA, NA, NA),
                 date_naissance = D(c(rep("1950-01-01", 4), "1950-06-01", "1950-01-01", "1950-01-01")),
                 date_entree = D(c("2015-01-01", "2015-01-01", "2015-01-01", "2015-04-01", "2015-01-01", "2015-01-01",
                                   "2015-01-01")),
                 date_sortie = D(c("2015-03-01", NA, NA, NA, "2015-06-30", "2015-02-01", "2015-07-01")),
                 cause_sortie = c("deces", "en_cours", "en_cours", "en_cours", "rachat", "rachat", "deces"),
                 stringsAsFactors = FALSE)
verifier("T5 (valeur calculée à la main) : 4 têtes touchées, 2 indéterminées (observée après décès, naissance), 2 lignes ; retirés 181 j et 1 décès ; (H, NA) cohérente", {
  r <- de(construire_table(T5, ROLES_SIM, plan_ie("sexe_manquant"))$etapes$importance_exclusions$resultat$retraits,
          "total", "A")
  tous(t = proche(c(r$tetes_touchees, r$tetes_indeterminees, r$lignes_indeterminees), c(4, 2, 2), 0, 0),
       r = proche(c(r$evenements_retires, r$exposition_jours_retiree), c(1, 181), 0, 0),
       etat = if (identical(r$etat, "produite")) TRUE else r$etat)
})
verifier("T5 (valeur calculée à la main) : toutes les têtes touchées indéterminées -> tete_indeterminee_apres_reintegration, valeurs NA, SMR non calculable", {
  i <- construire_table(T5[c(1, 2, 4, 5), ], ROLES_SIM, plan_ie("sexe_manquant"))$etapes$importance_exclusions$resultat
  r <- de(i$retraits, "total", "A"); s <- de(i$smr_comparateur_1, "total", "A")
  identical(r$etat, "tete_indeterminee_apres_reintegration") && is.na(r$evenements_retires) &&
    identical(r$tetes_indeterminees, 2L) && identical(s$etat, "tete_indeterminee_apres_reintegration") && is.na(s$smr)
})
verifier("condition 4 (valeur calculée à la main) : le statut de la garde dit qu'une valeur manquante ne rend pas une tête indéterminée", {
  s <- tablexp:::STATUTS_IMPORTANCE
  grepl("une valeur manquante (naissance, sexe, sexe hors modalités, segment) ne rend pas une tête indéterminée",
        s$statut[s$table == "retraits" & s$grandeur == "tetes_indeterminees"], fixed = TRUE)
})

# --- T6 : niveaux A et B, segmentation sexe (valeur calculée à la main) ---------

# T1 plus K (id_contrat manquant, F, décès 2015-02-01, 31 j) et L (sexe hors
# modalités « X », en cours, 365 j).
T6 <- rbind(T1, data.frame(id_contrat = c(NA, "C10"), id_tete = c("K", "L"), sexe = c("F", "X"),
                           date_naissance = D("1950-01-01"), date_entree = D("2015-01-01"),
                           date_sortie = D(c("2015-02-01", NA)), cause_sortie = c("deces", "en_cours"),
                           stringsAsFactors = FALSE))
EXC6 <- c("id_contrat_manquant", "entree_manquante", "sexe_manquant", "sexe_hors_modalites")
X6 <- construire_table(T6, ROLES_SIM, plan_ie(EXC6, segmentation = "sexe"))
I6 <- X6$etapes$importance_exclusions$resultat
verifier("T6 (valeur calculée à la main) : niveau A, 5 lignes mesurables, 578 j et 3 décès ; niveau B, K seule (F, 65 : 31 j, 1 décès), 4 lignes non calculables", {
  a <- de(I6$retraits, "total", "A"); b <- de(I6$retraits, "total", "B")
  cb <- I6$cellules_niveau_b[I6$cellules_niveau_b$ensemble == "total", ]
  tous(a = proche(c(a$lignes_mesurables, a$exposition_jours_retiree, a$evenements_retires), c(5, 578, 3), 0, 0),
       b = proche(c(b$lignes_mesurables, b$lignes_non_calculables, b$exposition_jours_retiree, b$evenements_retires),
                  c(1, 4, 31, 1), 0, 0),
       cel = if (identical(cb$sexe, "F") && identical(cb$age, 65L)) TRUE else "cellule B",
       etat = if (identical(de(I6$retraits, "sexe_manquant", "B")$etat, "cle_segmentation_manquante") &&
                  identical(de(I6$retraits, "sexe_manquant", "A")$etat, "produite")) TRUE else "états")
})
# Type des dénombrements (#174) : evenements_retires de la table retraits en
# integer, comme celui des tables de cellules, que des lignes soient produites,
# non produites (NA) ou absentes (total vide).
verifier("T6 (#174) : retraits$evenements_retires en integer, lignes produites et non produites mêlées (valeur calculée à la main : 3, 1, NA aux états de clé manquante) ; total vide (aucune exclusion) de même ; tables de cellules A et B integer", {
  r6 <- I6$retraits
  v0 <- construire_table(T1[1:4, ], ROLES_SANS_TETE, plan_ie("id_contrat_manquant"))$etapes$importance_exclusions$resultat
  tous(type = if (identical(typeof(r6$evenements_retires), "integer") &&
                  identical(typeof(v0$retraits$evenements_retires), "integer")) TRUE
              else paste(typeof(r6$evenements_retires), typeof(v0$retraits$evenements_retires)),
       na = if (anyNA(r6$evenements_retires) && any(r6$etat == "produite")) TRUE else "lignes non produites et produites attendues",
       valeurs = proche(c(de(r6, "total", "A")$evenements_retires, de(r6, "total", "B")$evenements_retires), c(3, 1), 0, 0),
       vide = if (nrow(v0$retraits) > 0L && all(v0$retraits$evenements_retires == 0L)) TRUE else "total vide : zéros attendus",
       cellules = if (identical(typeof(I6$cellules_niveau_a$evenements_retires), "integer") &&
                      identical(typeof(I6$cellules_niveau_b$evenements_retires), "integer")) TRUE else "cellules")
})
# Garde du total (#174, reprise) : aucune cellule ne dépasse .Machine$integer.max,
# mais leur somme oui. Garde défensive : sous la forme fenêtres, le total est
# majoré par le nombre de lignes des observations (décompte par tabulate()) ;
# inatteignable par construire_table(), d'où l'appel direct.
verifier("total_evenements_retires (#174) : total > .Machine$integer.max sans cellule au-delà de la borne -> refus importance_exclusions / evenements_retires_hors_plage (message cite le total, l'ensemble, le niveau) ; total égal à la borne et vide -> integer", {
  f <- tablexp:::total_evenements_retires
  imax <- .Machine$integer.max
  r <- tryCatch({ f(c(imax, 1L, 5L), "sexe_manquant", "B"); NULL }, refus_tablexp = function(r) r)
  tous(refus = if (is.null(r)) "aucun refus levé" else if (identical(c(r$module, r$motif),
                    c("importance_exclusions", "evenements_retires_hors_plage"))) TRUE else paste(r$module, r$motif),
       message = if (!is.null(r) && grepl("2147483653", r$message, fixed = TRUE) && grepl("« sexe_manquant »", r$message, fixed = TRUE) &&
                     grepl("niveau B", r$message, fixed = TRUE) &&
                     grepl(as.character(imax), r$message, fixed = TRUE)) TRUE else "message",
       details = if (!is.null(r) && identical(r$details$evenements_retires, as.numeric(imax) + 6)) TRUE else "détails",
       borne = if (identical(f(c(imax - 1L, 1L), "total", "A"), imax)) TRUE else "total égal à la borne",
       vide = if (identical(f(integer(0), "total", "A"), 0L)) TRUE else "total vide")
})
verifier("T6 (valeur calculée à la main) : classes_motifs du plan, niveau B (sexe employé)", {
  cm <- X6$etapes$importance_exclusions$parametres$classes_motifs
  identical(cm$motif, EXC6) && identical(cm$classe_niveau_a, c("M", "N", "M", "M")) &&
    identical(cm$classe_niveau_b, c("M", "N", "cle", "cle"))
})
verifier("T6, par_annee (valeur calculée à la main) : cellules du niveau A par âge et année ; retirés inchangés", {
  i <- construire_table(T6, ROLES_SIM, plan_ie(EXC6, segmentation = "sexe", par_annee = TRUE))$etapes$importance_exclusions$resultat
  ca <- i$cellules_niveau_a[i$cellules_niveau_a$ensemble == "total", ]
  identical(names(ca)[2:3], c("age", "annee")) && identical(ca$annee, 2015L) &&
    identical(de(i$retraits, "total", "A")$exposition_jours_retiree, 578) &&
    any(grepl("et l'année si par_annee", tablexp:::STATUTS_IMPORTANCE$statut, fixed = TRUE))
})

# --- T7 : états du SMR (valeur calculée à la main) --------------------------------

s1 <- function(De, Ee, Dr, Er, ages_r = 65L, ages_e = 65L)
  tablexp:::smr_comparateur_1(data.frame(age = ages_e, evenements_retires = De, exposition_jours_retiree = Ee * 365,
                                         exposition_centrale_retiree = Ee),
                              data.frame(age = ages_r, evenements = Dr, exposition_jours = Er * 365,
                                         exposition_centrale = Er), character(0), FALSE, 0.95)$ligne
verifier("T7 (valeur calculée à la main) : A = 0 et D = 0 -> aucun_attendu ; A = 0 et D > 0 -> observe_sans_attendu ; SMR NA", {
  a <- s1(0L, 0.5, 0L, 1); b <- s1(1L, 0.5, 0L, 1)
  identical(a$etat, "aucun_attendu") && identical(b$etat, "observe_sans_attendu") && is.na(a$smr) && is.na(b$smr) &&
    is.na(a$rapport_variance)
})
verifier("T7 (valeur calculée à la main) : D = 0 et A > 0 -> SMR 0, borne exacte [0 ; qgamma(0,975 ; 1) / A], asymptotique (échelle log) NA", {
  s <- s1(0L, 0.5, 2L, 1)
  tous(A = proche(s$attendus, 1, 1e-15, 0), smr = proche(s$smr, 0, 0, 0),
       b = proche(c(s$borne_inf_exacte, s$borne_sup_exacte), c(0, stats::qgamma(0.025, 1, lower.tail = FALSE)), 1e-15, 0),
       w = if (is.na(s$borne_inf_asymptotique) && identical(s$etat, "produite")) TRUE else "asymptotique (échelle log)")
})
verifier("T7 (valeur calculée à la main) : cellule sans exposition retenue restituée à part, hors du SMR", {
  s <- s1(c(1L, 1L), c(0.5, 0.3), 2L, 1, ages_e = 65:66)
  tous(O = proche(s$observes, 1, 0, 0), A = proche(s$attendus, 1, 1e-15, 0),
       a_part = proche(c(s$cellules_sans_exposition_retenue, s$evenements_sans_exposition_retenue,
                         s$exposition_centrale_sans_exposition_retenue), c(1, 1, 0.3), 1e-15, 0))
})
verifier("T7 (valeur calculée à la main) : aucune cellule retirée avec exposition retenue -> sans_exposition_retenue, SMR NA", {
  s <- s1(1L, 0.3, 2L, 1, ages_e = 66L)
  identical(s$etat, "sans_exposition_retenue") && is.na(s$smr) && identical(s$cellules_sans_exposition_retenue, 1L)
})
# Type de etat_cellule (#177) : character que la table de cellules soit vide,
# mêle des cellules produites et sans exposition retenue, ou n'en ait aucune
# de produite ; ifelse() sur un vecteur vide rendait logical(0).
verifier("T7 (#177) : etat_cellule en character, cellules retirées vides (0 ligne), mêlées (produite, sans_exposition_retenue) ou toutes sans exposition retenue ; tables de cellules A et B de la construction vides ou non, character", {
  c1 <- function(De, Ee, Dr, Er, ages_r = 65L, ages_e = 65L)
    tablexp:::smr_comparateur_1(data.frame(age = ages_e, evenements_retires = De, exposition_jours_retiree = Ee * 365,
                                           exposition_centrale_retiree = Ee),
                                data.frame(age = ages_r, evenements = Dr, exposition_jours = Er * 365,
                                           exposition_centrale = Er), character(0), FALSE, 0.95)$cellules
  vide <- c1(integer(0), numeric(0), 2L, 1, ages_e = integer(0))
  melee <- c1(c(1L, 1L), c(0.5, 0.3), 2L, 1, ages_e = 65:66)
  sans <- c1(1L, 0.3, 2L, 1, ages_e = 66L)
  v0 <- construire_table(T1[1:4, ], ROLES_SANS_TETE, plan_ie("id_contrat_manquant"))$etapes$importance_exclusions$resultat
  tous(vide = if (nrow(vide) == 0L && identical(vide$etat_cellule, character(0))) TRUE else paste(typeof(vide$etat_cellule), nrow(vide)),
       melee = if (identical(melee$etat_cellule, c("produite", "sans_exposition_retenue"))) TRUE else paste(melee$etat_cellule, collapse = " "),
       sans = if (identical(sans$etat_cellule, "sans_exposition_retenue")) TRUE else paste(sans$etat_cellule, collapse = " "),
       table_vide = if (nrow(v0$cellules_niveau_a) == 0L && nrow(v0$cellules_niveau_b) == 0L &&
                        identical(typeof(v0$cellules_niveau_a$etat_cellule), "character") &&
                        identical(typeof(v0$cellules_niveau_b$etat_cellule), "character")) TRUE
                    else "tables de cellules vides attendues, etat_cellule character",
       table_remplie = if (identical(typeof(I6$cellules_niveau_a$etat_cellule), "character") &&
                           identical(typeof(I6$cellules_niveau_b$etat_cellule), "character") &&
                           nrow(I6$cellules_niveau_a) > 0L && nrow(I6$cellules_niveau_b) > 0L) TRUE else "tables remplies attendues, character")
})

# --- T8 : comparateur 2 (valeur calculée à la main) --------------------------------

TQ <- table_reference(data.frame(age = 0:120, sexe = "ensemble", qx = 0.01), "q constant", "importee", "test")
VALQ <- list(table_reference = "tq", relation_table = "independante", plage_ages = c(60, 70), tranches_age = 60,
             niveau = 0.95, replications = 9)
mu01 <- -log1p(-0.01)
verifier("T8, independante (valeur calculée à la main) : A = (182/365) mu~, SMR = 2 / A, bornes exactes qgamma ; SMR des conservées de l'étape validation côte à côte", {
  x <- construire_table(T1, ROLES_SIM, plan_ie(val = VALQ), tables_reference = list(tq = TQ))
  s <- de(x$etapes$importance_exclusions$resultat$smr_comparateur_2, "total", "A")
  st <- x$etapes$validation$resultat$statistiques
  A <- 182 / 365 * mu01
  tous(A = proche(s$attendus, A, 1e-14, 0), smr = proche(s$smr, 2 / A, 1e-14, 0),
       b = proche(c(s$borne_inf_exacte, s$borne_sup_exacte),
                  c(qg(0.025, 2, A), stats::qgamma(0.025, 3, lower.tail = FALSE) / A), 1e-14, 0),
       cons = proche(s$smr_conserves, st$valeur[st$statistique == "smr" & st$grandeur == "smr"], 0, 0),
       etat = if (identical(c(s$etat, s$etat_intervalle), c("produite", "produite")) &&
                  identical(x$etapes$importance_exclusions$parametres$comparateurs$etat,
                            c("produite", "produite", "non_produite_par_cette_version"))) TRUE else "états")
})
verifier("T8, ajustee_hors_outil (valeur calculée à la main) : valeurs ponctuelles seules, bornes NA, état ddl_non_etabli (M255)", {
  v <- VALQ; v$relation_table <- "ajustee_hors_outil"
  x <- construire_table(T1, ROLES_SIM, plan_ie(val = v), tables_reference = list(tq = TQ))
  s <- de(x$etapes$importance_exclusions$resultat$smr_comparateur_2, "total", "A")
  tous(smr = proche(s$smr, 2 / (182 / 365 * mu01), 1e-14, 0),
       b = if (all(is.na(unlist(s[c("borne_inf_exacte", "borne_sup_exacte", "borne_inf_asymptotique",
                                     "borne_sup_asymptotique")]))) && identical(s$etat_intervalle, "ddl_non_etabli"))
             TRUE else "bornes")
})
verifier("T8, sans validation (valeur calculée à la main) : comparateur 2 non demandé par le plan, smr_comparateur_2 NULL", {
  p <- X1$etapes$importance_exclusions
  is.null(p$resultat$smr_comparateur_2) && "smr_comparateur_2" %in% names(p$resultat) &&
    identical(p$parametres$comparateurs$etat[2L], "non_demandee_par_le_plan") &&
    identical(p$parametres$comparateurs$libelle[2L], "non demandée par le plan")
})
TQS <- table_reference(rbind(data.frame(age = 0:120, sexe = "H", qx = 0.01), data.frame(age = 0:120, sexe = "F", qx = 0.02)),
                       "q par sexe", "importee", "test")
verifier("condition 3 (valeur calculée à la main) : table par sexe, niveau A : S(2) écarte sexe_manquant et sexe_hors_modalites (4 lignes non calculables), K seule (F) ; table ensemble : 5 lignes", {
  x <- construire_table(T6, ROLES_SIM, plan_ie(EXC6, "sexe", val = VALQ), tables_reference = list(tq = TQS))
  s <- de(x$etapes$importance_exclusions$resultat$smr_comparateur_2, "total", "A")
  y <- construire_table(T6, ROLES_SIM, plan_ie(EXC6, "sexe", val = VALQ), tables_reference = list(tq = TQ))
  t <- de(y$etapes$importance_exclusions$resultat$smr_comparateur_2, "total", "A")
  ss <- de(x$etapes$importance_exclusions$resultat$smr_comparateur_2, "sexe_manquant", "A")
  tous(s = proche(c(s$lignes_mesurables, s$lignes_non_calculables, s$observes, s$attendus),
                  c(1, 4, 1, 31 / 365 * -log1p(-0.02)), 1e-14, 0),
       t = proche(c(t$lignes_mesurables, t$lignes_non_calculables, t$observes, t$attendus),
                  c(5, 0, 3, 578 / 365 * mu01), 1e-14, 0),
       etat = if (identical(ss$etat, "cle_segmentation_manquante")) TRUE else ss$etat)
})
verifier("comparateur 2 (valeur calculée à la main) : sexe absent de la table (K, F, face à une table H) -> cellule_sans_valeur_table, sans refus", {
  d <- T6[!(T6$sexe %in% "F") | T6$id_tete == "K", ]
  th <- table_reference(data.frame(age = 0:120, sexe = "H", qx = 0.01), "q H", "importee", "test")
  x <- construire_table(d, ROLES_SIM, plan_ie(EXC6, "sexe", val = VALQ), tables_reference = list(tq = th))
  s <- de(x$etapes$importance_exclusions$resultat$smr_comparateur_2, "id_contrat_manquant", "B")
  identical(s$etat, "cellule_sans_valeur_table") && identical(s$cellules_sans_valeur_table, 1L) &&
    identical(s$evenements_sans_valeur_table, 1L) && is.na(s$smr)
})

# Comparateur 2, tout ou rien (avis d'actuary, C2 et Q2) : deux lignes
# exclues id_contrat_manquant, K1 (H, décès 2015-02-01) et K2 (F, décès
# 2015-03-01), face à une table H seule ; conservées C et E (H) de T1.
verifier("comparateur 2, tout ou rien (valeur calculée à la main) : cellule H avec valeur et cellule F sans valeur -> cellule_sans_valeur_table, O, A, SMR et bornes NA ; 1 cellule et 1 événement restitués", {
  d <- rbind(T1[c(1L, 3L), ], data.frame(id_contrat = NA_character_, id_tete = c("K1", "K2"), sexe = c("H", "F"),
                                         date_naissance = D("1950-01-01"), date_entree = D("2015-01-01"),
                                         date_sortie = D(c("2015-02-01", "2015-03-01")), cause_sortie = "deces",
                                         stringsAsFactors = FALSE))
  th <- table_reference(data.frame(age = 0:120, sexe = "H", qx = 0.01), "q H", "importee", "test")
  x <- construire_table(d, ROLES_SIM, plan_ie("id_contrat_manquant", "sexe", val = VALQ), tables_reference = list(tq = th))
  s <- x$etapes$importance_exclusions$resultat$smr_comparateur_2
  s <- s[s$ensemble == "total", ]
  na <- unlist(s[c("observes", "attendus", "smr", "borne_inf_exacte", "borne_sup_exacte", "borne_inf_asymptotique",
                   "borne_sup_asymptotique")])
  tous(niveaux = identical(s$niveau, c("A", "B")),
       etat = if (all(s$etat == "cellule_sans_valeur_table") && all(s$etat_intervalle == "cellule_sans_valeur_table"))
                TRUE else paste(s$etat, collapse = ", "),
       na = if (all(is.na(na))) TRUE else "valeurs non NA",
       restitues = if (identical(s$cellules_sans_valeur_table, c(1L, 1L)) &&
                       identical(s$evenements_sans_valeur_table, c(1L, 1L)) &&
                       identical(s$cellules_taux_table_egal_un, c(0L, 0L))) TRUE else "décomptes")
})
# T1 plus Z (C9 : sexe manquant, née le 1949-01-01, 66 ans en 2015, décès
# 2015-02-01, 31 j) ; aucune exposition conservée à 66 ans ; table ensemble
# à q_66 = 1 (0,01 ailleurs) : l'étape validation aboutit (E^c = 0 à 66).
verifier("comparateur 2, q~ = 1 (valeur calculée à la main) : construction aboutie, taux_table_egal_un en unité tête et en unité contrat, O, A, SMR NA ; 1 cellule et 1 événement restitués", {
  d <- rbind(T1, data.frame(id_contrat = "C9", id_tete = "Z", sexe = NA_character_, date_naissance = D("1949-01-01"),
                            date_entree = D("2015-01-01"), date_sortie = D("2015-02-01"), cause_sortie = "deces",
                            stringsAsFactors = FALSE))
  t66 <- table_reference(data.frame(age = 0:120, sexe = "ensemble", qx = ifelse(0:120 == 66, 1, 0.01)), "q66",
                         "importee", "test")
  ok <- vapply(list(ROLES_SIM, ROLES_SANS_TETE), function(ro) {
    x <- tryCatch(construire_table(d, ro, plan_ie(val = VALQ), tables_reference = list(tq = t66)),
                  refus_tablexp = function(r) r)
    if (inherits(x, "refus_tablexp")) return(FALSE)
    s <- x$etapes$importance_exclusions$resultat$smr_comparateur_2
    s <- s[s$ensemble == "total", ]
    all(s$etat == "taux_table_egal_un") && all(is.na(unlist(s[c("observes", "attendus", "smr", "borne_inf_exacte")]))) &&
      identical(s$cellules_taux_table_egal_un, c(1L, 1L)) && identical(s$evenements_taux_table_egal_un, c(1L, 1L)) &&
      identical(s$cellules_sans_valeur_table, c(0L, 0L))
  }, TRUE)
  if (all(ok)) TRUE else sprintf("unité tête %s, unité contrat %s", ok[1L], ok[2L])
})
verifier("comparateur 2 (valeur calculée à la main) : priorité de cellule_sans_valeur_table sur taux_table_egal_un ; libellés des deux états", {
  ret <- data.frame(sexe = c("H", "F"), age = c(66L, 65L), evenements_retires = c(1L, 1L),
                    exposition_jours_retiree = c(31, 59), exposition_centrale_retiree = c(31, 59) / 365)
  th <- table_reference(data.frame(age = 0:120, sexe = "H", qx = ifelse(0:120 == 66, 1, 0.01)), "q H66", "importee",
                        "test")
  v <- tablexp:::smr_comparateur_2(ret, "sexe", FALSE, th, c(60, 70), "tq", "independante", 0.95)
  e <- tablexp:::ETATS_IMPORTANCE
  identical(v$etat, "cellule_sans_valeur_table") && is.na(v$smr) && is.na(v$observes) &&
    identical(c(v$cellules_sans_valeur_table, v$cellules_taux_table_egal_un), c(1L, 1L)) &&
    identical(e$libelle[match(c("taux_table_egal_un", "contrat_duplique_unite_contrat"), e$etat)],
              c("non calculable : taux_table_egal_un", "non calculable : contrat_duplique_unite_contrat"))
})

# Motif déclaré sans ligne (avis d'actuary, C1) : id_contrat_manquant n'a
# aucune ligne dans T1 ; T1[1:4, ] n'a aucune ligne exclue (total vide).
verifier("motif déclaré sans ligne (valeur calculée à la main) : produite, retraits nuls, têtes 0 en unité tête ; SMR aucun_attendu (comparateurs 1 et 2) ; entree_manquante (lignes toutes N) reste aucune_ligne_mesurable", {
  x <- construire_table(T1, ROLES_SIM, plan_ie(c("id_contrat_manquant", "entree_manquante", "sexe_manquant"), val = VALQ),
                        tables_reference = list(tq = TQ))
  i <- x$etapes$importance_exclusions
  r <- i$resultat$retraits[i$resultat$retraits$ensemble == "id_contrat_manquant", ]
  tous(etat = if (all(r$etat == "produite")) TRUE else paste(r$etat, collapse = ", "),
       valeurs = proche(unlist(r[c("lignes_mesurables", "tetes_touchees", "tetes_indeterminees", "lignes_indeterminees",
                                   "evenements_retires", "exposition_jours_retiree", "exposition_centrale_retiree")]),
                        rep(0, 14), 0, 0),
       smr = if (all(c(i$resultat$smr_comparateur_1$etat[i$resultat$smr_comparateur_1$ensemble == "id_contrat_manquant"],
                       i$resultat$smr_comparateur_2$etat[i$resultat$smr_comparateur_2$ensemble == "id_contrat_manquant"])
                     == "aucun_attendu")) TRUE else "SMR",
       n = if (identical(de(i$resultat$retraits, "entree_manquante", "A")$etat, "aucune_ligne_mesurable")) TRUE
           else "entree_manquante",
       calculs = if (identical(i$diagnostics$calculs_contrefactuels, 1L)) TRUE else "calculs contrefactuels")
})
verifier("aucune exclusion déclarée (valeur calculée à la main) : total vide -> produite à zéro, unité contrat : têtes NA ; SMR aucun_attendu", {
  i <- construire_table(T1[1:4, ], ROLES_SANS_TETE, plan_ie("id_contrat_manquant"))$etapes$importance_exclusions$resultat
  r <- de(i$retraits, "total", "B")
  identical(r$etat, "produite") && identical(c(r$evenements_retires, r$exposition_jours_retiree,
                                               r$exposition_centrale_retiree), c(0, 0, 0)) &&
    is.na(r$tetes_touchees) && all(i$smr_comparateur_1$etat == "aucun_attendu")
})

# Type des dénombrements (#199 ; ADR 0009, point 9.6) : colonnes observes et
# evenements_* de smr_comparateur_1 (2) et smr_comparateur_2 (6) en integer,
# lignes produites et lignes NA_integer_ (ensemble non produit :
# aucune_ligne_mesurable ; comparateur 2 non calculable :
# cellule_sans_valeur_table) ; gardes défensives au-delà de
# .Machine$integer.max (appel direct : le total de l'étape exposition est
# gardé en amont, M397).
COLS_C1_199 <- c("observes", "evenements_sans_exposition_retenue")
COLS_C2_199 <- c("observes", "evenements_sans_valeur_table", "evenements_taux_table_egal_un", "evenements_sans_attendu",
                 "evenements_hors_plage", "observes_conserves")
verifier("dénombrements (#199) : smr_comparateur_1 (observes, evenements_sans_exposition_retenue) et smr_comparateur_2 (observes, observes_conserves, evenements_*) en integer, lignes produites et lignes NA (aucune_ligne_mesurable, cellule_sans_valeur_table)", {
  x <- construire_table(T1, ROLES_SIM, plan_ie(c("id_contrat_manquant", "entree_manquante", "sexe_manquant"), val = VALQ),
                        tables_reference = list(tq = TQ))
  i <- x$etapes$importance_exclusions$resultat
  t1 <- vapply(i$smr_comparateur_1[COLS_C1_199], typeof, "")
  t2 <- vapply(i$smr_comparateur_2[COLS_C2_199], typeof, "")
  na1 <- de(i$smr_comparateur_1, "entree_manquante", "A")
  na2 <- de(i$smr_comparateur_2, "entree_manquante", "A")
  ret <- data.frame(sexe = c("H", "F"), age = c(66L, 65L), evenements_retires = c(1L, 1L),
                    exposition_jours_retiree = c(31, 59), exposition_centrale_retiree = c(31, 59) / 365)
  th <- table_reference(data.frame(age = 0:120, sexe = "H", qx = 0.01), "q H", "importee", "test")
  nc <- tablexp:::smr_comparateur_2(ret, "sexe", FALSE, th, c(60, 70), "tq", "independante", 0.95)
  t_nc <- vapply(nc[setdiff(COLS_C2_199, "observes_conserves")], typeof, "")
  tous(c1 = if (all(t1 == "integer")) TRUE else paste(names(t1), t1, collapse = ", "),
       c2 = if (all(t2 == "integer")) TRUE else paste(names(t2), t2, collapse = ", "),
       na = if (identical(na1$etat, "aucune_ligne_mesurable") && identical(na1$observes, NA_integer_) &&
                identical(na1$evenements_sans_exposition_retenue, NA_integer_) &&
                identical(unname(unlist(na2[COLS_C2_199[-6L]])), rep(NA_integer_, 5L))) TRUE else "lignes NA",
       nc = if (identical(nc$etat, "cellule_sans_valeur_table") && identical(nc$observes, NA_integer_) &&
                all(t_nc == "integer") && identical(nc$evenements_sans_valeur_table, 1L)) TRUE else "non calculable")
})
verifier("dénombrements (#199) : comparateurs 1 et 2 en appel direct, sommes au-delà de .Machine$integer.max (deux cellules de 1,5e9 événements retirés) -> refus importance_exclusions / denombrement_hors_plage, grandeur nommée", {
  big <- c(1500000000L, 1500000000L)
  r_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
  c1 <- function(ages_r) tablexp:::smr_comparateur_1(
    data.frame(age = 65:66, evenements_retires = big, exposition_jours_retiree = c(365, 365), exposition_centrale_retiree = c(1, 1)),
    data.frame(age = ages_r, evenements = 2L, exposition_jours = 365, exposition_centrale = 1), character(0), FALSE, 0.95)
  c2 <- function(ages) tablexp:::smr_comparateur_2(
    data.frame(sexe = "H", age = ages, evenements_retires = big, exposition_jours_retiree = c(365, 365),
               exposition_centrale_retiree = c(1, 1)),
    "sexe", FALSE, TQ, c(60, 70), "tq", "independante", 0.95)
  attendu <- function(r, g) if (is.null(r)) "aucun refus levé" else
    if (identical(c(r$module, r$motif), c("importance_exclusions", "denombrement_hors_plage")) &&
        identical(r$details$grandeur, g) && identical(r$details$valeur, 3e9)) TRUE else paste(r$module, r$motif, r$details$grandeur)
  tous(c1_observes = attendu(r_de(c1(c(65L, 66L))), "smr_comparateur_1/observes"),
       c1_sans_exposition = attendu(r_de(c1(c(70L, 71L))), "smr_comparateur_1/evenements_sans_exposition_retenue"),
       c2_observes = attendu(r_de(c2(65:66)), "smr_comparateur_2/observes"),
       c2_hors_plage = attendu(r_de(c2(75:76)), "smr_comparateur_2/evenements_hors_plage"))
})

# contrat_duplique (décision du mainteneur du 06/11/2026, option (b)) : C1
# (tête P, décès 2015-10-01, 273 j) et C2 (tête Q, en cours, 365 j), chacun
# en deux lignes identiques ; C3 (tête R) conservée.
TD <- data.frame(id_contrat = c("C1", "C1", "C2", "C2", "C3"), id_tete = c("P", "P", "Q", "Q", "R"),
                 sexe = c("H", "H", "F", "F", "H"), date_naissance = D("1950-01-01"), date_entree = D("2015-01-01"),
                 date_sortie = D(c("2015-10-01", "2015-10-01", NA, NA, NA)),
                 cause_sortie = c("deces", "deces", "en_cours", "en_cours", "en_cours"), stringsAsFactors = FALSE)
ns_ie <- asNamespace("tablexp")
appels_retrait <- function(expr) {
  n <- new.env(); n$k <- 0L
  suppressMessages(trace("retrait_contrefactuel", where = ns_ie, print = FALSE,
                         tracer = bquote(assign("k", get("k", envir = .(n)) + 1L, envir = .(n)))))
  on.exit(suppressMessages(untrace("retrait_contrefactuel", where = ns_ie)))
  force(expr)
  n$k
}
verifier("contrat_duplique, unité contrat (valeur calculée à la main) : motif N, état contrat_duplique_unite_contrat, retrait_contrefactuel() non appelé ; 2 lignes à événement comptées", {
  x <- NULL
  k <- appels_retrait(x <- construire_table(TD, ROLES_SANS_TETE, plan_ie("contrat_duplique")))
  i <- x$etapes$importance_exclusions
  r <- de(i$resultat$retraits, "contrat_duplique", "A")
  e <- i$resultat$evenements_declares[i$resultat$evenements_declares$ensemble == "contrat_duplique", ]
  tous(classe = identical(unlist(i$parametres$classes_motifs[1L, c("classe_niveau_a", "classe_niveau_b")], use.names = FALSE),
                          c("N", "N")),
       etat = if (identical(r$etat, "contrat_duplique_unite_contrat") &&
                  identical(de(i$resultat$smr_comparateur_1, "contrat_duplique", "B")$etat,
                            "contrat_duplique_unite_contrat")) TRUE else r$etat,
       trace = if (k == 0L && identical(i$diagnostics$calculs_contrefactuels, 0L)) TRUE else sprintf("%d appel(s)", k),
       evenements = if (e$lignes == 4L && e$evenement_dans_fenetre == 2L && is.na(e$tetes_evenement_dans_fenetre))
                      TRUE else "événements déclarés")
})
verifier("contrat_duplique, unité contrat avec id_tete (en_cours -> rachat ; valeur calculée à la main) : 2 lignes et 1 tête à événement comptées", {
  d <- TD; d$cause_sortie[1:2] <- "rachat"
  p <- plan_ie("contrat_duplique"); p$transition <- RA
  i <- construire_table(d, ROLES_SIM, p)$etapes$importance_exclusions
  e <- i$resultat$evenements_declares[i$resultat$evenements_declares$ensemble == "contrat_duplique", ]
  identical(i$parametres$unite_observation, "contrat") &&
    identical(de(i$resultat$retraits, "contrat_duplique", "A")$etat, "contrat_duplique_unite_contrat") &&
    e$evenement_dans_fenetre == 2L && identical(e$tetes_evenement_dans_fenetre, 1L)
})
verifier("contrat_duplique, unité tête (valeur calculée à la main) : motif M, 2 têtes touchées, 1 décès et 273 + 365 = 638 jours retirés, E^c = 638/365", {
  i <- construire_table(TD, ROLES_SIM, plan_ie("contrat_duplique"))$etapes$importance_exclusions
  r <- i$resultat$retraits[i$resultat$retraits$ensemble == "contrat_duplique", ]
  tous(classe = identical(unlist(i$parametres$classes_motifs[1L, c("classe_niveau_a", "classe_niveau_b")], use.names = FALSE),
                          c("M", "M")),
       r = proche(c(r$evenements_retires, r$exposition_jours_retiree, r$tetes_touchees), c(1, 1, 638, 638, 2, 2), 0, 0),
       ec = proche(r$exposition_centrale_retiree, rep(638 / 365, 2), 1e-15, 0),
       etat = if (all(r$etat == "produite")) TRUE else paste(r$etat, collapse = ", "))
})

# --- T3 : identité exacte sur le jeu § 2.6 (tests/unitaires/test_simulation.R) ----

# Jeu de jeu_reference() (mode uniforme, 10^5 contrats), période 2015-2019 ;
# sexe_manquant sur les lignes à décès (taux 0,5) et sur les autres (0,02).
# Une tête par contrat : les têtes touchées n'ont aucune ligne conservée, et
# les retirés sont, cellule par cellule, la construction du jeu propre moins
# celle du jeu altéré.
PER_26 <- list(debut = D("2015-01-01"), fin = D("2019-12-31"))
PF_26 <- list(mode = "uniforme", n = 1e5, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
DEF_26 <- list(graine = 20261025L, declarations = list(
  list(type = "sexe_manquant", cible = "deces", taux = 0.5),
  list(type = "sexe_manquant", cible = c("rachat", "en_cours"), taux = 0.02)))
t0 <- proc.time()[["elapsed"]]
s26_0 <- tablexp::simuler_portefeuille(LOI, PF_26, PER_26, 20261016L)
s26 <- tablexp::simuler_portefeuille(LOI, PF_26, PER_26, 20261016L, defauts = DEF_26)
p26 <- function(ex) { p <- plan_ie(ex, periode = PER_26, brut = TRUE); if (!length(ex)) p$etapes$importance_exclusions <- NULL; p }
x26_0 <- construire_table(s26_0, ROLES_SIM, p26(character(0)))
x26 <- construire_table(s26, ROLES_SIM, p26("sexe_manquant"))
duree_26 <- proc.time()[["elapsed"]] - t0
I26 <- x26$etapes$importance_exclusions$resultat
verifier("T3 (identité exacte) : retirés = construction du jeu propre - construction du jeu altéré, par âge (décès et jours identical(), E^c à 1e-12 relatif)", {
  e0 <- x26_0$etapes$exposition$resultat$exposition; e1 <- x26$etapes$exposition$resultat$exposition
  c_ <- I26$cellules_niveau_a[I26$cellules_niveau_a$ensemble == "total", ]
  k1 <- match(e0$age, e1$age)
  dD <- e0$evenements - ifelse(is.na(k1), 0L, e1$evenements[k1])
  dJ <- e0$exposition_jours - ifelse(is.na(k1), 0, e1$exposition_jours[k1])
  dE <- e0$exposition_centrale - ifelse(is.na(k1), 0, e1$exposition_centrale[k1])
  g <- dD > 0L | dJ > 0
  tous(ages = if (identical(c_$age, e0$age[g])) TRUE else "âges",
       D = if (identical(c_$evenements_retires, dD[g])) TRUE else "décès",
       J = if (identical(c_$exposition_jours_retiree, dJ[g])) TRUE else "jours",
       E = proche(c_$exposition_centrale_retiree, dE[g], 1e-12, 0),
       S_k = if (identical(as.list(I26$cellules_niveau_a[I26$cellules_niveau_a$ensemble == "sexe_manquant", -1L]),
                           as.list(I26$cellules_niveau_a[I26$cellules_niveau_a$ensemble == "total", -1L]))) TRUE else "S_k")
})
s26r <- de(I26$smr_comparateur_1, "total", "A")
ligne <- sprintf(paste("T3 (identité exacte) : SMR = D_e / somme E^c_e mu^_r recalculé sur les cellules ; D_e = %d ;",
                       "A = %.3f ; SMR = %.2f [%.2f ; %.2f] ; f = %.3f (spécification : 1 105 / 55,581 = 19,88,",
                       "[18,73 ; 21,09], f = 0,053) ; %.1f s"),
                 as.integer(s26r$observes), s26r$attendus, s26r$smr, s26r$borne_inf_exacte, s26r$borne_sup_exacte,
                 s26r$rapport_variance, duree_26)
cat("  ", ligne, "\n", sep = "")
verifier(ligne, {
  c_ <- I26$cellules_niveau_a[I26$cellules_niveau_a$ensemble == "total", ]
  b <- x26$etapes$brut$resultat$poisson
  mu <- b$taux[match(c_$age, b$age)]
  ok <- !is.na(mu)
  tous(O = proche(s26r$observes, sum(c_$evenements_retires[ok]), 0, 0),
       A = proche(s26r$attendus, sum(c_$exposition_centrale_retiree[ok] * mu[ok]), 1e-12, 0),
       spec_O = proche(s26r$observes, 1105, 0, 0), spec_A = proche(s26r$attendus, 55.581, 0, 5e-4),
       spec_smr = proche(s26r$smr, 19.88, 0, 5e-3), spec_f = proche(s26r$rapport_variance, 0.053, 0, 5e-4),
       spec_ic = proche(c(s26r$borne_inf_exacte, s26r$borne_sup_exacte), c(18.73, 21.09), 0, 5e-3))
})

# --- T4 : local = global en unité tête (identité exacte) -------------------------

# Têtes multi-contrats (uniforme_multi, 3 000 têtes) ; sexe_manquant (taux
# 0,05) et id_contrat_manquant (taux 0,02) sur toutes les lignes. Global :
# exposition de toutes les lignes de R u S (toutes les têtes), moins celle de
# la construction ; local : têtes touchées seules (étape).
PF4 <- list(mode = "uniforme_multi", n = 3000, proportion_hommes = 0.5, age_min = 20, age_max = 75,
            origine = D("2005-01-01"), loi_contrats = c(0.6, 0.3, 0.1))
S4 <- tablexp::simuler_portefeuille(LOI, PF4, PER, 20261016L, defauts = list(graine = 20261025L, declarations = list(
  list(type = "sexe_manquant", cible = TOUTES, taux = 0.05), list(type = "id_contrat_manquant", cible = TOUTES, taux = 0.02))))
EXC4 <- c("id_contrat_manquant", "sexe_manquant")
global <- function(x, cles, niveau) {
  rd <- x$etapes$donnees$resultat
  cl <- tablexp:::classer_lignes_exclues(rd$lignes_exclues, "tete", cles)
  L <- sort(c(rd$numeros_conserves, cl$lignes[cl$classe == "M"]))
  ob <- construire_observations(S4[L, ], L, ROLES_SIM, cles, P)
  rec <- tablexp:::deduire_recette(ROLES_SIM)
  t_rs <- tablexp:::calculer_exposition(ob$observations, P, cles, rec,
                                        tablexp:::resoudre_convention("plus_fine", rec$convention_native),
                                        FALSE)$resultat$exposition
  t_r <- if (length(cles)) x$etapes$exposition$resultat$exposition
         else tablexp:::cellules_regroupees(x$etapes$exposition$resultat$exposition, FALSE)
  k <- match(tablexp:::cle_cellules(t_rs, cles, FALSE), tablexp:::cle_cellules(t_r, cles, FALSE))
  d <- data.frame(cle = tablexp:::cle_cellules(t_rs, cles, FALSE),
                  D = t_rs$evenements - ifelse(is.na(k), 0L, t_r$evenements[k]),
                  J = t_rs$exposition_jours - ifelse(is.na(k), 0, t_r$exposition_jours[k]),
                  E = t_rs$exposition_centrale - ifelse(is.na(k), 0, t_r$exposition_centrale[k]),
                  Etot = t_rs$exposition_centrale, stringsAsFactors = FALSE)
  d <- d[d$D > 0L | d$J > 0, ]
  c_ <- x$etapes$importance_exclusions$resultat[[paste0("cellules_niveau_", tolower(niveau))]]
  c_ <- c_[c_$ensemble == "total", ]
  r <- de(x$etapes$importance_exclusions$resultat$retraits, "total", niveau)
  tous(indeterminees = proche(r$tetes_indeterminees, 0, 0, 0),
       cles = if (identical(tablexp:::cle_cellules(c_, cles, FALSE), d$cle)) TRUE else "cellules",
       D = if (identical(c_$evenements_retires, d$D)) TRUE else "décès",
       J = if (identical(c_$exposition_jours_retiree, d$J)) TRUE else "jours",
       E = proche((c_$exposition_centrale_retiree - d$E) / d$Etot, rep(0, nrow(d)), 0, 1e-12),
       positifs = if (all(c_$evenements_retires >= 0L) && all(c_$exposition_jours_retiree >= 0) &&
                      all(c_$evenements_retires > 0L | c_$exposition_jours_retiree > 0) &&
                      all(c_$exposition_centrale_retiree[c_$exposition_jours_retiree > 0] > 0) &&
                      all(c_$exposition_centrale_retiree[c_$exposition_jours_retiree == 0] == 0)) TRUE else "signes")
}
x4a <- construire_table(S4, ROLES_SIM, plan_ie(EXC4, periode = PER))
x4b <- construire_table(S4, ROLES_SIM, plan_ie(EXC4, "sexe", periode = PER, brut = TRUE))
verifier(sprintf("T4 (identité exacte) : local = global en unité tête, niveau A, %d lignes mesurables, %d têtes touchées ; décès et jours identical(), E^c à 1e-12 du total de la cellule ; cellules >= 0",
                 de(x4a$etapes$importance_exclusions$resultat$retraits, "total", "A")$lignes_mesurables,
                 de(x4a$etapes$importance_exclusions$resultat$retraits, "total", "A")$tetes_touchees),
         global(x4a, character(0), "A"))
verifier(sprintf("T4 (identité exacte) : local = global en unité tête, niveau B (sexe), %d lignes mesurables",
                 de(x4b$etapes$importance_exclusions$resultat$retraits, "total", "B")$lignes_mesurables),
         global(x4b, "sexe", "B"))
# Deuxième recette fictive (#82 ; avis (P) rafraîchi, critère 2) :
# deduire_recette() remplacée dans l'espace de noms par une recette de
# convention native age_atteint_annee, puis restaurée (nom gardé :
# lignes_dates_completes ; un nom hors du catalogue est une erreur de
# programmation depuis l'aiguillage sur le nom, ADR 0016, points 4.1 et 6.2) ;
# chaque appel de calculer_exposition() (étape exposition et expositions
# contrefactuelles de exposition_lignes()) est relevé par trace() : recette
# et convention retenue reçues. Toutes égalent celles de la fiche (une seule
# source ; avant #82, les contrefactuelles relisaient la recette de la
# branche et résolvaient la convention demandée).
verifier("deuxième recette fictive (#82) : étape exposition et expositions contrefactuelles de importance_exclusions sous la recette et la convention de la fiche (convention native age_atteint_annee, retenue age_atteint_annee) ; au moins un calcul contrefactuel ; deduire_recette() restaurée", {
  ns <- asNamespace("tablexp")
  ancienne <- get("deduire_recette", envir = ns)
  fictive <- function(roles) {
    r <- ancienne(roles)
    r$convention_native <- "age_atteint_annee"
    r
  }
  releve <- new.env(); releve$l <- character(0)
  unlockBinding("deduire_recette", ns)
  assign("deduire_recette", fictive, envir = ns)
  suppressMessages(trace("calculer_exposition", where = ns, print = FALSE,
                         tracer = bquote(assign("l", c(get("l", envir = .(releve)), paste(recette$convention_native, conv$retenue)),
                                                envir = .(releve)))))
  x <- tryCatch(construire_table(S4, ROLES_SIM, plan_ie(EXC4, periode = PER)), finally = {
    suppressMessages(untrace("calculer_exposition", where = ns))
    assign("deduire_recette", ancienne, envir = ns); lockBinding("deduire_recette", ns)
  })
  fiche <- paste(x$fiche$convention$native, x$fiche$convention$retenue)
  identical(fiche, "age_atteint_annee age_atteint_annee") && length(releve$l) >= 2L && all(releve$l == fiche) &&
    x$etapes$importance_exclusions$diagnostics$calculs_contrefactuels >= 1L &&
    identical(tablexp:::deduire_recette(ROLES_SIM)$recette, "lignes_dates_completes") &&
    bindingIsLocked("deduire_recette", ns)
})
verifier("comparateur 1 (identité exacte) : taux_conserves = taux de Poisson de l'étape brut, cellule par cellule (niveau B)", {
  c_ <- x4b$etapes$importance_exclusions$resultat$cellules_niveau_b
  b <- x4b$etapes$brut$resultat$poisson
  k <- match(paste(c_$sexe, c_$age), paste(b$sexe, b$age))
  ok <- c_$etat_cellule == "produite"
  nrow(c_) > 0L && all(ok) && identical(c_$taux_conserves[ok], b$taux[k[ok]])
})

# --- plan, refus, rapport, reproductibilité --------------------------------------

verifier("plan (valeur calculée à la main) : niveau invalide -> refus niveau_invalide (module importance_exclusions) ; sans exposition -> etape_prealable_manquante ; graine sans étape aléatoire -> graine_inattendue", {
  p <- plan_ie()
  a <- refus_de(valider_plan(local({ q <- plan_ie(); q$etapes$importance_exclusions$niveau <- 1L; q })))
  b <- refus_de(valider_plan(local({ q <- plan_ie(); q$etapes$exposition <- NULL; q })))
  g <- refus_de(valider_plan(local({ q <- plan_ie(); q$graine <- 1L; q })))
  !is.null(a) && identical(c(a$module, a$motif), c("importance_exclusions", "niveau_invalide")) &&
    identical(b$motif, "etape_prealable_manquante") && identical(g$motif, "graine_inattendue") &&
    isTRUE(tryCatch({ valider_plan(p); TRUE }, refus_tablexp = function(r) FALSE))
})
# Valeurs validées de X1 (#82) : p, recette et convention transmises par
# construire_table().
P_X1 <- valeurs(VD, P15, 0)
REC_X1 <- tablexp:::deduire_recette(ROLES_SIM)
CONV_X1 <- tablexp:::resoudre_convention("plus_fine", REC_X1$convention_native)
verifier("appel direct (valeur calculée à la main) : validation sans table -> erreur de programmation ; la fonction d'étape ne contrôle plus le niveau (#82 : refus niveau_invalide levé par valider_plan(), ci-dessus)", {
  e <- X1$etapes
  b <- tryCatch({ tablexp:::calculer_importance_exclusions(T1, ROLES_SIM, e$donnees, e$exposition, list(), NULL, P_X1,
                                                           REC_X1, CONV_X1, character(0), 0.95); NULL },
                error = function(e) e)
  !inherits(b, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(b), fixed = TRUE) &&
    !("controler" %in% names(formals(tablexp:::calculer_importance_exclusions)))
})
verifier("appel direct = construire_table() (identité exacte) : même résultat, aucun avertissement", {
  e <- X1$etapes
  r <- tablexp:::calculer_importance_exclusions(T1, ROLES_SIM, e$donnees, e$exposition, NULL, NULL, P_X1, REC_X1,
                                                CONV_X1, character(0), 0.95)
  identical(r[c("parametres", "resultat", "diagnostics")], e$importance_exclusions) && !nrow(r$avertissements)
})
verifier("reproductibilité (identité exacte) : deux constructions identical() hors execution ; fiche sans champ nouveau",
         identical(sans_execution(construire_table(T1, ROLES_SIM, plan_ie())), sans_execution(X1)) &&
           identical(names(X1$fiche), names(construire_table(T1, ROLES_SIM, local({ p <- plan_ie(); p$etapes$importance_exclusions <- NULL; p }))$fiche)))
verifier("rejeu (identité exacte) : rejouer_fiche() redonne l'objet hors execution", {
  r <- rejouer_fiche(T1, lire_fiche(ecrire_fiche(X1$fiche)))
  identical(sans_execution(r$construction), sans_execution(X1))
})
texte_rapport <- function(x) { h <- rawToChar(ecrire_rapport(x)); Encoding(h) <- "UTF-8"; h }
MENTION_M259 <- paste("Ces grandeurs mesurent le poids des lignes exclues par les règles d'exclusion déclarées (volumes,",
                      "événements, exposition, SMR) et permettent d'apprécier l'importance des exclusions. Elles ne disent",
                      "ni si les données exclues sont pertinentes, ni si leur exclusion est justifiée.")
verifier("rapport (valeur calculée à la main) : section etape_importance_exclusions, mention fixe de M259 mot pour mot, tables de l'étape ; pas de mention d'unité contrat en unité tête", {
  h <- texte_rapport(X1)
  all(vapply(c("<section id=\"etape_importance_exclusions\">", paste0("<p>", MENTION_M259, "</p>"),
               "<caption><code>etapes/importance_exclusions/resultat/smr_comparateur_1</code>",
               "<caption><code>etapes/importance_exclusions/resultat/retraits</code>",
               "<caption><code>etapes/importance_exclusions/parametres/statuts</code>"),
             function(m) grepl(m, h, fixed = TRUE), NA)) &&
    !grepl("Unité d'observation contrat, sans identifiant de tête", h, fixed = TRUE)
})
verifier("rapport (valeur calculée à la main) : unité contrat -> mention validation_unite_contrat reprise en entier (Q-c ; sens de l'erreur propre à chaque grandeur, #361) ; plan sans l'étape -> « non demandée par le plan »", {
  h2 <- texte_rapport(X2)
  h0 <- texte_rapport(construire_table(T1, ROLES_SIM, local({ p <- plan_ie(); p$etapes$importance_exclusions <- NULL; p })))
  s <- regmatches(h2, regexpr("<section id=\"etape_importance_exclusions\">.*?</section>", h2))
  m <- tablexp:::echapper_html(tablexp:::MENTIONS_RAPPORT[["validation_unite_contrat"]])
  length(s) == 1L && grepl(paste0("<p>", m, "</p>"), s, fixed = TRUE) &&
    grepl("<p>Étape « importance_exclusions » : non demandée par le plan (chemin etapes/importance_exclusions absent de l'objet).</p>",
          h0, fixed = TRUE)
})

# Période sans exposition (#210, M399) : le refus de l'étape exposition porte
# sur la table de la construction, non sur le sous-ensemble de lignes exclues
# dont le contrefactuel recalcule l'exposition (exposition_lignes(),
# refuser_vide = FALSE).
verifier("sous-ensemble exclu sans exposition ni événement dans la période (#210 ; valeur calculée à la main) : construction produite, retraits nuls (1 ligne mesurable, 0 événement retiré, 0 jour retiré), aucun refus periode_sans_exposition, unités tête et contrat", {
  d <- rbind(T1[1:4, ], data.frame(id_contrat = "C9", id_tete = "Z", sexe = NA_character_, date_naissance = D("1950-01-01"),
                                   date_entree = D("2013-01-01"), date_sortie = D("2013-12-31"), cause_sortie = "deces",
                                   stringsAsFactors = FALSE))
  ok <- vapply(list(ROLES_SIM, ROLES_SANS_TETE), function(ro) {
    x <- tryCatch(construire_table(d, ro, plan_ie("sexe_manquant")), error = function(e) e)
    if (inherits(x, "error")) return(FALSE)
    r <- de(x$etapes$importance_exclusions$resultat$retraits, "sexe_manquant", "A")
    identical(r$lignes_mesurables, 1L) && r$evenements_retires == 0 && r$exposition_jours_retiree == 0
  }, NA)
  if (all(ok)) TRUE else sprintf("unité tête %s, unité contrat %s", ok[1L], ok[2L])
})

# Extractions de lignes de l'étape (#320 ; spécification d'actuary, Q3, et
# outil de #322, point 4) : exposition_lignes() et retrait_contrefactuel()
# extraient par la méthode « [ » de la classe de l'entrée (M499 (a)), puis
# controler_lignes_extraites() contrôle l'extraction avant
# construire_observations() et evaluer_motifs() ; une méthode qui ne ment que
# sous indices positifs (preparer_donnees() extrait par indices négatifs : sa
# postcondition passe) fait refuser donnees_mal_formees du module
# importance_exclusions (avant #320, mesuré sur 7abf58f : erreur R
# « lignes_tete hors de la forme admise » en unité tête, « arguments imply
# differing number of rows: 2, 3 » en unité contrat, objet construit sous
# k320perm et k320cpos). Natures des attendus : valeur calculée à la main
# (lignes relues : L = R_t u S = 1, 5, 6, 7 en unité tête, S = 5, 6, 7 en
# unité contrat ; colonnes de rôle dont les valeurs diffèrent une fois ces
# lignes inversées) ; identité exacte pour les classes à « [ » correcte.
# Méthodes S3 définies dans globalenv() puis retirées.
local({
  m320 <- list(
    `[.k320pos` = function(x, i, j, ..., drop = TRUE) {
      r <- NextMethod()
      if (!missing(i) && is.numeric(i) && all(i > 0)) {
        cl <- class(r); class(r) <- "data.frame"; r <- r[-1L, , drop = FALSE]; class(r) <- cl
      }
      r },
    `[.k320perm` = function(x, i, j, ..., drop = TRUE) {
      r <- NextMethod()
      if (!missing(i) && is.numeric(i) && all(i > 0)) {
        cl <- class(r); class(r) <- "data.frame"; r <- r[rev(seq_len(.row_names_info(r, 2L))), , drop = FALSE]
        class(r) <- cl
      }
      r },
    # Ne ment que sur deux lignes ou moins demandées par indices positifs (cas ksel de l'audit léger de #320).
    `[.k320sel` = function(x, i, j, ..., drop = TRUE) {
      r <- NextMethod()
      if (!missing(i) && is.numeric(i) && all(i > 0) && length(i) <= 2L) {
        cl <- class(r); class(r) <- "data.frame"; r <- r[-1L, , drop = FALSE]; class(r) <- cl
      }
      r },
    # Méthode de colonne (facteur sous-classé) qui inverse les valeurs sous indices positifs.
    `[.k320cpos` = function(x, i, ...) {
      r <- NextMethod()
      if (is.numeric(i) && all(i > 0)) {
        u <- unclass(r); r <- structure(rev(as.vector(u)), levels = attr(u, "levels"), class = oldClass(r))
      }
      r },
    `[.k320ok` = function(x, i, j, ..., drop = TRUE) NextMethod(),
    `[.k320reset` = function(x, i, j, ..., drop = TRUE) {
      r <- NextMethod(); attr(r, "row.names") <- seq_len(.row_names_info(r, 2L)); r },
    `[.k320attr` = function(x, i, j, ..., drop = TRUE) { r <- NextMethod(); attr(r, "histo") <- "extrait"; r })
  for (n in names(m320)) assign(n, m320[[n]], envir = globalenv())
  on.exit(rm(list = names(m320), envir = globalenv()))
  attrape <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e)
  SUJET <- paste("Les lignes relues par l'étape « importance_exclusions » (lignes exclues et, sous l'unité tête,",
                 "lignes conservées des mêmes têtes), extraites des données par la méthode « [ » de la classe ")
  SUFF <- paste(" : le moteur ne corrige pas une extraction fausse ; passer un data.frame dont la méthode « [ » rend",
                "exactement les lignes demandées (ou qui n'a pas de méthode « [ » propre).")
  SUFF_COL <- function(cl) paste0(" : le moteur ne corrige pas une extraction fausse ; passer un data.frame dont les ",
                                  "colonnes citées (classe ", cl, ") ont une méthode « [ » qui rend exactement les ",
                                  "lignes demandées (ou n'ont pas de méthode « [ » propre).")
  RV <- "comptent %d ligne(s) comme demandé mais ne reprennent pas les valeurs des lignes demandées dans la ou les colonne(s) de rôle %s"
  T1c <- T1; T1c$cause_sortie[5L] <- "rachat"
  colonne <- function(d, nom) { d[[nom]] <- structure(factor(d[[nom]]), class = c("k320cpos", "factor")); d }
  # nom, données, rôles, message attendu, lignes, attendu, colonnes, cause (#369)
  cas <- list(
    list("méthode qui rend une ligne de moins sous indices positifs, unité tête", structure(T1, class = c("k320pos", "data.frame")),
         ROLES_SIM, paste0(SUJET, "k320pos/data.frame, comptent 3 ligne(s) au lieu de 4", SUFF), 3L, 4L, character(0), "1"),
    list("méthode qui rend une ligne de moins sous indices positifs, unité contrat", structure(T1, class = c("k320pos", "data.frame")),
         ROLES_SANS_TETE, paste0(SUJET, "k320pos/data.frame, comptent 2 ligne(s) au lieu de 3", SUFF), 2L, 3L, character(0), "1"),
    # Sens d'`attendu` (QD2, Q-A1) : sous l'unité tête, la troisième extraction (lignes conservées des têtes
    # touchées, ici la ligne 1 seule, après R_t u S = 1, 5, 6, 7 et ses lignes hors têtes indéterminées, justes)
    # est la première fausse : attendu 1, inférieur à |S| = 3 comme à |R_t u S| = 4.
    list("méthode qui ne ment que sur deux lignes ou moins demandées, unité tête (attendu = lignes de la première extraction fausse)",
         structure(T1, class = c("k320sel", "data.frame")), ROLES_SIM,
         paste0(SUJET, "k320sel/data.frame, comptent 0 ligne(s) au lieu de 1", SUFF), 0L, 1L, character(0), "1"),
    list("méthode qui inverse les lignes sous indices positifs, unité tête", structure(T1, class = c("k320perm", "data.frame")),
         ROLES_SIM, paste0(SUJET, "k320perm/data.frame, ",
                           sprintf(RV, 4L, "« id_contrat », « id_tete », « sexe », « date_entree », « date_sortie »"), SUFF),
         4L, 4L, c("id_contrat", "id_tete", "sexe", "date_entree", "date_sortie"), "3 b"),
    list("méthode qui inverse les lignes sous indices positifs, unité contrat", structure(T1, class = c("k320perm", "data.frame")),
         ROLES_SANS_TETE, paste0(SUJET, "k320perm/data.frame, ",
                                 sprintf(RV, 3L, "« id_contrat », « date_entree », « date_sortie »"), SUFF),
         3L, 3L, c("id_contrat", "date_entree", "date_sortie"), "3 b"),
    list("entrée data.frame, colonne sexe dont la méthode « [ » inverse les valeurs sous indices positifs, unité tête",
         colonne(T1, "sexe"), ROLES_SIM,
         paste0(SUJET, "data.frame, qui applique à chaque colonne la méthode « [ » de sa classe, ",
                sprintf(RV, 4L, "« sexe » (classe k320cpos/factor)"), SUFF_COL("k320cpos/factor")),
         4L, 4L, "sexe", "3 b"),
    list("entrée data.frame, colonne cause_sortie dont la méthode « [ » inverse les valeurs sous indices positifs, unité contrat",
         colonne(T1c, "cause_sortie"), ROLES_SANS_TETE,
         paste0(SUJET, "data.frame, qui applique à chaque colonne la méthode « [ » de sa classe, ",
                sprintf(RV, 3L, "« cause_sortie » (classe k320cpos/factor)"), SUFF_COL("k320cpos/factor")),
         3L, 3L, "cause_sortie", "3 b"))
  for (c1 in cas) {
    x <- attrape(construire_table(c1[[2L]], c1[[3L]], plan_ie()))
    verifier(sprintf("#320 %s -> refus donnees_mal_formees du module importance_exclusions (valeur calculée à la main), message attendu, une ligne UTF-8 valide, details de #319", c1[[1L]]), {
      cl <- class(c1[[2L]])
      if (is.null(x)) "aucun refus (objet construit)"
      else if (!inherits(x, "refus_tablexp")) sprintf("condition %s : %s", class(x)[1L], conditionMessage(x))
      else if (!identical(x$module, "importance_exclusions") || !identical(x$motif, "donnees_mal_formees"))
        sprintf("%s / %s", x$module, x$motif)
      else if (!identical(conditionMessage(x), c1[[4L]])) paste("message :", conditionMessage(x))
      else if (grepl("[\n\v\f]", conditionMessage(x)) || !validUTF8(conditionMessage(x)) ||
               !identical(Encoding(conditionMessage(x)), "UTF-8")) "message sur plusieurs lignes ou non UTF-8"
      else if (!identical(x$details, list(cause = c1[[8L]], classe = cl, classe_rendue = cl, lignes = c1[[5L]], attendu = c1[[6L]],
                                          colonnes = c1[[7L]], colonne = character(0), position = integer(0),
                                          lignes_colonne = numeric(0))))
        paste("details :", paste(deparse(x$details), collapse = " "))
      else TRUE
    })
  }
  # Le même jeu sans l'étape importance_exclusions est construit : seule l'étape relit des lignes par indices positifs.
  verifier("#320 les mêmes entrées, plan sans importance_exclusions : objet construit (preparer_donnees() extrait par indices négatifs, sa postcondition passe)", {
    p <- plan_ie(); p$etapes$importance_exclusions <- NULL
    ok <- vapply(cas, function(c1) is.null(attrape(construire_table(c1[[2L]], c1[[3L]], p))), NA)
    if (all(ok)) TRUE else paste("refus ou erreur :", paste(which(!ok), collapse = ", "))
  })
  for (k in c("k320ok", "k320reset", "k320attr")) for (u in c("tête", "contrat")) {
    ro <- if (u == "tête") ROLES_SIM else ROLES_SANS_TETE
    ref <- if (u == "tête") X1 else X2
    verifier(sprintf("#320 classe %s dont la méthode « [ » est correcte, unité %s (identité exacte) : étapes exposition et importance_exclusions et empreintes identical() au data.frame nu", k, u), {
      x <- tryCatch(construire_table(structure(T1, class = c(k, "data.frame")), ro, plan_ie()),
                    condition = function(e) e)
      if (inherits(x, "condition")) conditionMessage(x)
      else identical(x$etapes$importance_exclusions, ref$etapes$importance_exclusions) &&
        identical(x$etapes$exposition, ref$etapes$exposition) && identical(x$empreintes, ref$empreintes)
    })
  }
})
verifier("#320 les méthodes `[` de session sont retirées de globalenv()",
         !any(c("[.k320pos", "[.k320perm", "[.k320sel", "[.k320cpos", "[.k320ok", "[.k320reset", "[.k320attr") %in% ls(globalenv(), all.names = TRUE)))

# Colonnes de rôle de nom d'encodage « bytes » (#255) : etats_evenements_exclus()
# et calculer_importance_exclusions() lisent cause_sortie, date_sortie et
# id_tete par lire_champ() ; avant (mesuré sur 7abf58f), « [[ » levait
# « translating strings with "bytes" encoding is not allowed ». Appel direct :
# construire_table() refuse ce nom avant la première étape (chaine_codage_bytes).
# Identité exacte avec les mêmes appels sous des noms ASCII.
local({
  octets <- function(x) { Encoding(x) <- "bytes"; x }
  NOMS <- c(id_tete = octets("id_t\xeate"), cause_sortie = octets("cause\xe9"), date_sortie = octets("sortie\xe9"))
  d <- T1; ro <- ROLES_SIM
  for (r in names(NOMS)) { names(d)[names(d) == ROLES_SIM[[r]]] <- NOMS[[r]]; ro[[r]] <- NOMS[[r]] }
  e <- X1$etapes
  verifier("#255 etats_evenements_exclus(), cause_sortie et date_sortie de noms bytes (identité exacte) : aucune erreur R, états identical() à ceux des noms ASCII", {
    a <- tryCatch(tablexp:::etats_evenements_exclus(d, ro, 5:8, P_X1$debut, P_X1$borne, "deces"), error = function(e) e)
    if (inherits(a, "error")) conditionMessage(a)
    else identical(a, tablexp:::etats_evenements_exclus(T1, ROLES_SIM, 5:8, P_X1$debut, P_X1$borne, "deces")) &&
      identical(a, rep("evenement_dans_fenetre", 4L))
  })
  verifier("#255 calculer_importance_exclusions(), id_tete, cause_sortie et date_sortie de noms bytes (identité exacte) : aucune erreur R, résultat identical() à celui de construire_table() sous les noms ASCII", {
    r <- tryCatch(tablexp:::calculer_importance_exclusions(d, ro, e$donnees, e$exposition, NULL, NULL, P_X1, REC_X1,
                                                           CONV_X1, character(0), 0.95), error = function(e) e)
    if (inherits(r, "error")) conditionMessage(r)
    else identical(r[c("parametres", "resultat", "diagnostics")], e$importance_exclusions)
  })
})
# Lecture de cause_sortie sans dispatch (#320, Q1 de #322) : facteur à NA et
# niveau inutilisé, facteur sans niveau (codes NA) : mêmes états que la chaîne.
verifier("#320 etats_evenements_exclus(), cause_sortie facteur (NA, niveau inutilisé ; puis facteur sans niveau) (identité exacte) : états identical() à ceux de la même colonne en chaîne", {
  et <- function(v) { d <- T1; d$cause_sortie <- v; tablexp:::etats_evenements_exclus(d, ROLES_SIM, 1:8, P_X1$debut, P_X1$borne, "deces") }
  f <- factor(c(NA, T1$cause_sortie[-1L]), levels = c("deces", "en_cours", "rachat", "zz"))
  f0 <- factor(rep(NA_character_, 8L))
  identical(et(f), et(as.character(f))) && identical(et(f0), et(rep(NA_character_, 8L)))
})

# Lecture du nombre de lignes reçues sans dispatch (#343, suite de #322, C2) :
# calculer_importance_exclusions() lisait nrow(donnees), qui dispatche sur la
# classe de l'entrée ; une dim() à état qui ne ment qu'à cette lecture levait le
# stop() « lignes reçues différentes de conservées + exclues (erreur de
# programmation) ». La classe k343 ment (nombre de lignes + 1) seulement quand
# nrow() est appelée par la fonction désignée par REF343$corps (pile :
# calculer_importance_exclusions, nrow, dim, dim.k343, d'où sys.function(k - 3L)),
# honnête ailleurs (evaluer_motifs(), controler_lignes_donnees()). Contrôle de
# la profondeur de pile : sur une copie de la fonction qui relit nrow(donnees),
# le mensonge est atteint et le stop() levé ; sans lui, le test ne prouverait
# rien. k343h : dim() honnête, témoin.
local({
  REF343 <- new.env(); REF343$corps <- NULL; REF343$mensonges <- 0L
  m343 <- list(
    dim.k343 = function(x) {
      d <- c(.row_names_info(x, 2L), length(unclass(x)))
      k <- sys.nframe()
      if (k >= 4L && !is.null(REF343$corps) && identical(body(sys.function(k - 3L)), REF343$corps)) {
        REF343$mensonges <- REF343$mensonges + 1L
        d[1L] <- d[1L] + 1L
      }
      d },
    dim.k343h = function(x) c(.row_names_info(x, 2L), length(unclass(x))))
  for (n in names(m343)) assign(n, m343[[n]], envir = globalenv())
  on.exit(rm(list = names(m343), envir = globalenv()))
  reel <- tablexp:::calculer_importance_exclusions
  # Copie qui relit nrow(donnees) à la place de .row_names_info(donnees, 2L)
  # (état d'avant #343), dans l'espace de noms du package.
  src <- deparse(reel)
  i_n <- grep(".row_names_info(donnees, 2L)", src, fixed = TRUE)
  ancien <- reel
  if (length(i_n) == 1L) {
    src[i_n] <- sub(".row_names_info(donnees, 2L)", "nrow(donnees)", src[i_n], fixed = TRUE)
    ancien <- eval(parse(text = src), envir = asNamespace("tablexp"))
    environment(ancien) <- environment(reel)
  }
  corps_reel <- body(reel); corps_ancien <- body(ancien)
  attrape <- function(expr) tryCatch(expr, refus_tablexp = function(r) r, error = function(e) e)
  D1 <- structure(T1, class = c("k343", "data.frame"))
  e <- X1$etapes
  appel <- function(f, d) f(d, ROLES_SIM, e$donnees, e$exposition, NULL, NULL, P_X1, REC_X1, CONV_X1, character(0), 0.95)
  verifier("#343 la copie d'avant (nrow(donnees)) se distingue de la fonction du package : une seule ligne remplacée, corps différents",
           length(i_n) == 1L && !identical(corps_reel, corps_ancien))
  REF343$corps <- corps_ancien
  verifier("#343 contrôle de la profondeur de pile (constat d'exécution) : copie d'avant (nrow(donnees)) appelée sur la classe k343 : dim() ment, stop() « lignes reçues différentes de conservées + exclues (erreur de programmation) »", {
    REF343$mensonges <- 0L
    x <- attrape(appel(ancien, D1))
    inherits(x, "error") && !inherits(x, "refus_tablexp") && REF343$mensonges >= 1L &&
      grepl("+ exclues (erreur de programmation)", conditionMessage(x), fixed = TRUE)
  })
  REF343$corps <- corps_reel
  verifier("#343 appel direct, classe k343 dont dim() ment à nrow() depuis calculer_importance_exclusions() (identité exacte) : aucune erreur, résultat identical() à celui du data.frame nu", {
    x <- attrape(appel(reel, D1))
    if (inherits(x, "condition")) conditionMessage(x)
    else identical(x[c("parametres", "resultat", "diagnostics")], e$importance_exclusions)
  })
  for (u in c("tete", "contrat")) {
    ro <- if (u == "tete") ROLES_SIM else ROLES_SANS_TETE
    ref <- if (u == "tete") X1 else X2
    for (k in c("k343", "k343h")) {
      verifier(sprintf("#343 construire_table(), classe %s (mensonge de dim() à nrow() depuis calculer_importance_exclusions() pour k343), unité %s (identité exacte) : aucune erreur, étape importance_exclusions identical() à celle du data.frame nu", k, u), {
        x <- attrape(construire_table(structure(T1, class = c(k, "data.frame")), ro, plan_ie()))
        if (inherits(x, "condition")) conditionMessage(x)
        else identical(x$etapes$importance_exclusions, ref$etapes$importance_exclusions) &&
          identical(x$etapes$importance_exclusions$resultat, ref$etapes$importance_exclusions$resultat)
      })
    }
  }
  verifier("#343 témoin : le data.frame nu donne les mêmes étapes importance_exclusions qu'au premier appel (identité exacte)", {
    identical(construire_table(T1, ROLES_SIM, plan_ie())$etapes$importance_exclusions, X1$etapes$importance_exclusions)
  })
})
verifier("#343 les méthodes dim de session sont retirées de globalenv()",
         !any(c("dim.k343", "dim.k343h") %in% ls(globalenv(), all.names = TRUE)))

# Bonne forme d'un facteur extrait au contrôle (3) de l'extraction (#357, F357 ;
# spécification d'actuary, commentaire 7124474462 de #357) : contrôle (3 a) de
# controler_lignes_extraites(), facteur_bien_forme() (F342) exigé de la colonne
# de rôle extraite quand celle de l'entrée est un facteur, sur toutes les
# colonnes de rôle, avant l'identité des libellés (3 b). Classe de colonne k357
# (facteur sous-classé) dont la méthode « [ » remplace les codes NA par 99L.
# Avant #357 (mesuré sur dd17d0c) : construire_table() levait l'erreur R
# « malformed factor » à l'étape importance_exclusions ; l'appel direct de
# l'outil, étapes donnees et importance_exclusions, ne refusait pas (lignes 5 à
# 8 : codes 99 99 99 2, libellés NA NA NA H, égaux à ceux de l'entrée). Le
# chemin par construire_table() vers l'étape donnees n'est pas atteint (une
# ligne conservée à sexe NA est exclue par sexe_manquant, sinon ligne_invalide ;
# avis d'actuary) : l'étape donnees est éprouvée par l'appel direct. Natures des
# attendus : valeur écrite à la main (motif, module, message, details) ;
# littéraux non ASCII en \u00XX ; avertissements de R comptés.
local({
  `[.k357` <- function(x, i, ...) {
    r <- NextMethod(); u <- unclass(r); u[is.na(u)] <- 99L
    structure(u, levels = attr(r, "levels"), class = oldClass(r)) }
  `[.k357rev` <- function(x, i, ...) {
    r <- NextMethod(); u <- unclass(r); structure(rev(as.vector(u)), levels = attr(u, "levels"), class = oldClass(r)) }
  m357 <- list(`[.k357` = `[.k357`, `[.k357rev` = `[.k357rev`)
  for (n in names(m357)) assign(n, m357[[n]], envir = globalenv())
  on.exit(rm(list = names(m357), envir = globalenv()))
  # Condition rattrapée et nombre d'avertissements de R.
  mesurer <- function(expr) {
    n <- 0L
    r <- withCallingHandlers(tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r, error = function(e) e),
                             warning = function(w) { n <<- n + 1L; invokeRestart("muffleWarning") })
    list(r = r, avertissements = n)
  }
  d <- T1; d$sexe <- structure(factor(T1$sexe, levels = c("F", "H")), class = c("k357", "factor"))
  # (3 a) avant (3 b) : id_contrat extrait inversé (C8 C7 C6 C5 : défaut (3 b), qui seul ferait citer id_contrat),
  # sexe mal formé (défaut (3 a)).
  d2 <- d; d2$id_contrat <- structure(factor(T1$id_contrat), class = c("k357rev", "factor"))
  # Deux colonnes mal formées, toutes citées dans l'ordre des rôles.
  d3 <- d; d3$cause_sortie <- structure(factor(replace(T1$cause_sortie, 6L, NA)), class = c("k357", "factor"))
  dk <- structure(d, class = c("kdf357", "data.frame"))
  SUJ <- c(donnees = "Les lignes conserv\u00e9es",
           importance_exclusions = paste("Les lignes relues par l'\u00e9tape \u00ab importance_exclusions \u00bb (lignes",
                                         "exclues et, sous l'unit\u00e9 t\u00eate, lignes conserv\u00e9es des m\u00eames t\u00eates)"))
  CONSTAT <- paste("comptent 4 ligne(s) comme demand\u00e9 mais leur(s) colonne(s) de r\u00f4le %s sont des facteurs mal",
                   "form\u00e9s (niveaux absents ou non character, ou code qui n'est ni NA ni un integer de 1 au nombre",
                   "de niveaux)")
  message_col <- function(e, cites, cl)
    paste0(SUJ[[e]], ", extraites des donn\u00e9es par la m\u00e9thode \u00ab [ \u00bb de la classe data.frame, qui applique",
           " \u00e0 chaque colonne la m\u00e9thode \u00ab [ \u00bb de sa classe, ", sprintf(CONSTAT, cites),
           " : le moteur ne corrige pas une extraction fausse ; passer un data.frame dont les colonnes cit\u00e9es (classe ",
           cl, ") ont une m\u00e9thode \u00ab [ \u00bb qui rend exactement les lignes demand\u00e9es (ou n'ont pas de",
           " m\u00e9thode \u00ab [ \u00bb propre).")
  message_entree <- function(e, cites, cl)
    paste0(SUJ[[e]], ", extraites des donn\u00e9es par la m\u00e9thode \u00ab [ \u00bb de la classe ", cl, ", ",
           sprintf(CONSTAT, cites), " : le moteur ne corrige pas une extraction fausse ; passer un data.frame dont la",
           " m\u00e9thode \u00ab [ \u00bb rend exactement les lignes demand\u00e9es (ou qui n'a pas de m\u00e9thode",
           " \u00ab [ \u00bb propre).")
  details <- function(cl, colonnes) list(cause = "3 a", classe = cl, classe_rendue = cl, lignes = 4L, attendu = 4L, colonnes = colonnes,
                                         colonne = character(0), position = integer(0), lignes_colonne = numeric(0))
  juger <- function(m, module, attendu, det) {
    r <- m$r
    if (is.null(r)) "aucun refus"
    else if (!inherits(r, "refus_tablexp")) sprintf("condition %s : %s", class(r)[1L], conditionMessage(r))
    else if (!identical(r$module, module) || !identical(r$motif, "donnees_mal_formees")) sprintf("%s / %s", r$module, r$motif)
    else if (!identical(conditionMessage(r), attendu)) paste("message :", conditionMessage(r))
    else if (grepl("[\n\v\f]", conditionMessage(r)) || !validUTF8(conditionMessage(r)) ||
             !identical(Encoding(conditionMessage(r)), "UTF-8")) "message sur plusieurs lignes ou non UTF-8"
    else if (!identical(r$details, det)) paste("details :", paste(deparse(r$details), collapse = " "))
    else if (m$avertissements != 0L) sprintf("%d avertissement(s) de R", m$avertissements)
    else TRUE
  }
  S1 <- "\u00ab sexe \u00bb (classe k357/factor)"
  verifier("#357 construire_table(), colonne sexe k357 (codes NA rendus 99 par \u00ab [ \u00bb), \u00e9tape importance_exclusions -> refus importance_exclusions/donnees_mal_formees (valeur \u00e9crite \u00e0 la main), message, details, 0 avertissement de R (avant : erreur R \u00ab malformed factor \u00bb)",
           juger(mesurer(construire_table(d, ROLES_SIM, plan_ie())), "importance_exclusions",
                 message_col("importance_exclusions", S1, "k357/factor"), details("data.frame", "sexe")))
  for (e in names(SUJ)) {
    verifier(sprintf("#357 appel direct, \u00e9tape %s, lignes 5 \u00e0 8, colonne sexe k357 -> refus %s/donnees_mal_formees (valeur \u00e9crite \u00e0 la main), message, details, 0 avertissement de R (avant : aucun refus)", e, e),
             juger(mesurer(tablexp:::controler_lignes_extraites(d, d[5:8, ], 5:8, ROLES_SIM, e)), e,
                   message_col(e, S1, "k357/factor"), details("data.frame", "sexe")))
    verifier(sprintf("#357 appel direct, \u00e9tape %s, sexe k357 mal form\u00e9 et id_contrat invers\u00e9 (k357rev) -> le contr\u00f4le (3 a) pr\u00e9c\u00e8de le (3 b) : seule sexe cit\u00e9e (valeur \u00e9crite \u00e0 la main)", e),
             juger(mesurer(tablexp:::controler_lignes_extraites(d2, d2[5:8, ], 5:8, ROLES_SIM, e)), e,
                   message_col(e, S1, "k357/factor"), details("data.frame", "sexe")))
    verifier(sprintf("#357 appel direct, \u00e9tape %s, sexe et cause_sortie k357 mal form\u00e9es -> les deux colonnes cit\u00e9es, dans l'ordre des r\u00f4les (valeur \u00e9crite \u00e0 la main)", e),
             juger(mesurer(tablexp:::controler_lignes_extraites(d3, d3[5:8, ], 5:8, ROLES_SIM, e)), e,
                   message_col(e, paste(S1, "\u00ab cause_sortie \u00bb (classe k357/factor)", sep = ", "),
                               "k357/factor"),
                   details("data.frame", c("sexe", "cause_sortie"))))
    verifier(sprintf("#357 appel direct, \u00e9tape %s, entr\u00e9e de sous-classe kdf357 (sans m\u00e9thode \u00ab [ \u00bb propre), sexe k357 -> message de l'entr\u00e9e, colonne cit\u00e9e sans classe (valeur \u00e9crite \u00e0 la main)", e),
             juger(mesurer(tablexp:::controler_lignes_extraites(dk, dk[5:8, ], 5:8, ROLES_SIM, e)), e,
                   message_entree(e, "\u00ab sexe \u00bb", "kdf357/data.frame"), details(c("kdf357", "data.frame"), "sexe")))
  }
  # Témoin : la même entrée, plan sans importance_exclusions, est construite (lignes conservées sans NA dans sexe).
  verifier("#357 t\u00e9moin : colonne sexe k357, plan sans importance_exclusions -> objet construit, 0 avertissement de R", {
    p <- plan_ie(); p$etapes$importance_exclusions <- NULL
    m <- mesurer(construire_table(d, ROLES_SIM, p))
    if (!is.null(m$r)) paste("condition :", conditionMessage(m$r)) else identical(m$avertissements, 0L)
  })
})
verifier("#357 les m\u00e9thodes `[` de session sont retir\u00e9es de globalenv()",
         !any(c("[.k357", "[.k357rev") %in% ls(globalenv(), all.names = TRUE)))
