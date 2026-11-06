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
  r <- preparer_donnees(BASE, ROLES, VD, PER, 0, character(0), character(0))$resultat
  ex <- preparer_donnees(B_NA, ROLES, VD, PER, 0, character(0), "sexe_manquant")$resultat
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
  r <- preparer_donnees(d, cas[[3]], cas[[2]], PER, cas[[4]], "sexe", cas[[5]])
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
