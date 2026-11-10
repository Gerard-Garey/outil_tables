# -----------------------------------------------------------------------------
#  tests/unitaires/test_agregats.R : recette d'exposition « agrégats » (issue
#  #169, commits (3b) et (3c), forme « agrégats » du simulateur ; spécification
#  d'actuary-approfondi acceptée, M327 ; ADR 0016, points 4.3, 4.5, 5, 6 et
#  8 ; messages visés par actuary, avis du 08/11/2026 sur #169)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité exacte : agrégats formés depuis la table d'exposition d'une
#    construction en lignes (critère C1 : exhaustivité de (D, E^c), résultat
#    exact) ; permutation des lignes sans somme (C6) ; sommes dyadiques (C5) ;
#  - valeur écrite à la main : motifs levés seuls (C3), relevés (C4), refus,
#    motifs et messages (C2, C7, C8), champs de la fiche (C9) ;
#  - contre-exemple écrit à la main : deux jeux de lignes aux mêmes agrégats,
#    Kaplan-Meier 0,5 et 1 (C8, spécification, § 4.3) ;
#  - simulateur (C10, C11) : identité exacte du jeu propre et des appels
#    répétés, refus écrits à la main ; recoupement simulateur <-> exposition,
#    approximation numérique à tolérance relative 1e-10.
#  Graines inférieures à 1000 (exemptées, tests/controles/graines.R, règle
#  A5) ; aucune graine de 1000 ou plus.
# -----------------------------------------------------------------------------

D <- as.Date
PER <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
VD <- list(depart = "vivant", arrivee = "deces")
RA <- list(depart = "en_cours", arrivee = "rachat")
rf <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(e) e)
# Refus attendu : module, motif ; TRUE ou le texte de l'écart.
controle <- function(r, module, motif) {
  if (is.null(r)) return("aucun refus levé")
  if (!identical(r$module, module)) return(sprintf("module « %s » au lieu de « %s »", r$module, module))
  if (!identical(r$motif, motif)) return(sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  TRUE
}
plan_ag <- function(etapes = NULL, segmentation = "sexe", transition = VD, graine = NULL, convention = "dernier_anniversaire",
                    par_annee = FALSE, exclusions = character(0)) {
  p <- list(transition = transition, periode = PER, delai_declaration = 0, segmentation = segmentation)
  if (!is.null(graine)) p$graine <- graine
  p$etapes <- c(list(donnees = list(exclusions = exclusions),
                     exposition = list(convention = convention, par_annee = par_annee)), etapes)
  p
}
BRUT <- list(brut = list(estimateurs = c("poisson", "binomial"), niveau = 0.95))

# --- jeu simulé, construction en lignes et agrégats formés depuis sa table --------

GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
JEU <- local({
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 4000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
  simuler_portefeuille(loi, pf, PER, 169L)
})
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
T_HF <- table_reference(rbind(data.frame(age = GM$age, sexe = "H", qx = GM$qx),
                              data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx)),
                        "GM vraie par sexe", "importee", "loi vraie du jeu simulé (test)")
POS <- list(positionnement = list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95))
VAL <- list(validation = list(table_reference = "tv", relation_table = "independante", plage_ages = c(20, 90),
                              tranches_age = c(20, 50, 70), niveau = 0.95, replications = 199))
# Étape homogeneite ajoutée à C1 (recommandation d'actuary, commit (3c)) : la
# matrice l'admet sous exposition_centrale (#169, § 4 et § 7).
HOM <- list(homogeneite = list(plage_ages = c(20, 90), tranches_age = c(20, 50, 70), replications = 199))
PLAN_C1 <- plan_ag(c(BRUT, POS, HOM, VAL), graine = 169L)
XL <- construire_table(JEU, ROLES, PLAN_C1, tables_reference = list(tv = T_HF))
EL <- XL$etapes$exposition$resultat$exposition
AGG <- data.frame(sexe = EL$sexe, age = EL$age, ec = EL$exposition_centrale,
                  ei = EL$exposition_centrale + EL$complement_initial_cellule, d = EL$evenements,
                  stringsAsFactors = FALSE)
R_AG <- c(sexe = "sexe", age = "age", exposition_centrale = "ec", exposition_initiale = "ei", evenements = "d")
XA <- construire_table(AGG, R_AG, PLAN_C1, tables_reference = list(tv = T_HF))

verifier("C1 (identité exacte) : agrégats formés depuis la table d'exposition des lignes (E^c, E^c + complément, D, zéros compris) -> tables poisson et binomial, positionnement, homogeneite et validation identical() à la construction en lignes", {
  ok <- c(brut = identical(XA$etapes$brut$resultat, XL$etapes$brut$resultat),
          positionnement = identical(XA$etapes$positionnement$resultat, XL$etapes$positionnement$resultat),
          homogeneite = !is.null(XA$etapes$homogeneite) &&
            identical(XA$etapes$homogeneite$resultat, XL$etapes$homogeneite$resultat),
          validation = identical(XA$etapes$validation$resultat, XL$etapes$validation$resultat),
          cellules = nrow(EL) > 100L && sum(EL$evenements) > 50L)
  if (all(ok)) TRUE else paste("écart :", paste(names(ok)[!ok], collapse = ", "))
})
verifier("C1 : table d'exposition des agrégats = clés, age, expositions déclarées et evenements de la table des lignes, sans colonne propre aux lignes (ADR 0016, point 4.3)", {
  ea <- XA$etapes$exposition$resultat$exposition
  identical(names(ea), c("sexe", "age", "exposition_centrale", "exposition_initiale", "evenements")) &&
    identical(ea[c("sexe", "age", "exposition_centrale", "evenements")], EL[c("sexe", "age", "exposition_centrale", "evenements")]) &&
    identical(ea$exposition_initiale, AGG$ei) && is.integer(ea$evenements) && is.integer(ea$age)
})
# C1 sous par_annee (recommandation d'actuary, commit (3c)) : agrégats formés
# depuis les cellules de Lexis (sexe, âge, année) des lignes, E^c et D seuls
# (binomial refusé sous par_annee et dernier_anniversaire) ; Poisson seul.
verifier("C1 sous par_annee = TRUE (identité exacte) : agrégats formés depuis les cellules de Lexis des lignes (sexe, age, annee, E^c, D) -> table poisson identical() à la construction en lignes", {
  pl <- plan_ag(list(brut = list(estimateurs = "poisson", niveau = 0.95)), par_annee = TRUE)
  xl <- construire_table(JEU, ROLES, pl)
  el <- xl$etapes$exposition$resultat$exposition
  ag <- data.frame(sexe = el$sexe, age = el$age, annee = el$annee, ec = el$exposition_centrale, d = el$evenements,
                   stringsAsFactors = FALSE)
  xa <- construire_table(ag, c(sexe = "sexe", age = "age", annee = "annee", exposition_centrale = "ec", evenements = "d"), pl)
  ok <- c(poisson = identical(xa$etapes$brut$resultat$poisson, xl$etapes$brut$resultat$poisson),
          annees = length(unique(el$annee)) == 10L, evenements = sum(el$evenements) > 50L)
  if (all(ok)) TRUE else paste("écart :", paste(names(ok)[!ok], collapse = ", "))
})
verifier("hypothèses de brut sous le seam cellules (textes visés par actuary) : Poisson, clause de troncature remplacée ; binomial, phrase ajoutée ; seam fenetres : HYPOTHESES_BRUT inchangées", {
  h <- XA$etapes$brut$parametres$hypotheses; hl <- XL$etapes$brut$parametres$hypotheses
  hb <- tablexp:::HYPOTHESES_BRUT
  identical(hl$poisson, hb$poisson) && identical(hl$binomial, hb$binomial) &&
    identical(h$poisson, sub("troncature à gauche traitée par la fenêtre d'observation",
                             paste("E^c et D reçus du producteur de l'agrégat ; troncature à gauche, censure et sorties",
                                   "concurrentes traitées par lui (hypothese_censure, hypotheses_recette)"),
                             hb$poisson, fixed = TRUE)) && !identical(h$poisson, hb$poisson) &&
    identical(h$binomial, paste(hb$binomial, "E^i reçue, déclarée de cette définition (exposition_initiale = « declaree »,",
                                "hypotheses_recette), non vérifiée par le moteur."))
})
verifier("brut sous agrégats : exposition_initiale déclarée (hypotheses_brut$exposition_initiale = declaree), lignes : cellule (ADR 0016, point 4.5)",
         identical(XA$etapes$brut$parametres$exposition_initiale, "declaree") &&
           identical(XA$fiche$hypotheses_brut$exposition_initiale, "declaree") &&
           identical(XL$etapes$brut$parametres$exposition_initiale, "cellule"))
verifier("exposition_initiale_brut() : seam fenetres E^c + complément (définition cellule) ; seam cellules colonne lue telle quelle (définition declaree) ; seam inconnu -> erreur de programmation", {
  f <- tablexp:::exposition_initiale_brut
  e <- data.frame(exposition_centrale = c(1.5, 0.25), complement_initial_cellule = c(0.5, 0.125), exposition_initiale = c(9, 8))
  err <- tryCatch(f(e, "autre"), error = function(c) c)
  identical(f(e, "fenetres"), list(valeurs = c(2, 0.375), definition = "cellule")) &&
    identical(f(e, "cellules"), list(valeurs = c(9, 8), definition = "declaree")) &&
    inherits(err, "error") && !inherits(err, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(err), fixed = TRUE)
})
verifier("calculer_taux_bruts(), seam cellules, exposition initiale seule : binomial calculé sans colonne exposition_centrale (colonnes requises selon l'estimateur et le seam)", {
  rec <- tablexp:::deduire_recette(c(age = "a", evenements = "d", exposition_initiale = "i"))
  e <- data.frame(age = 60:61, exposition_initiale = c(10, 4), evenements = c(1L, 2L))
  b <- tablexp:::calculer_taux_bruts(e, character(0), FALSE, "dernier_anniversaire", "u", "binomial", 0.95, rec)
  identical(b$resultat$binomial$taux, c(0.1, 0.5)) && identical(b$parametres$exposition_initiale, "declaree")
})

# --- C9 : fiche, rejeu, rapport figé ----------------------------------------------

verifier("C9 fiche (valeur écrite à la main) : recette agregats, convention déclarée (native NA, non forcée), unité non_determinee, avertissement_unite = unite_agregat, unité d'exposition et hypothèse de censure déclarées, version 6", {
  f <- XA$fiche
  identical(f$recette, "agregats") &&
    identical(f$convention, list(demandee = "dernier_anniversaire", native = NA_character_, retenue = "dernier_anniversaire",
                                 forcee = FALSE)) &&
    identical(f$unite_observation, "non_determinee") &&
    identical(f$avertissement_unite, XA$avertissements$message[XA$avertissements$code == "unite_agregat"]) &&
    startsWith(f$avertissement_unite, "Unité d'observation : non déterminée (agrégats") &&
    identical(f$unite_exposition, "déclarée : années de la convention déclarée, définition des jours non vérifiable") &&
    identical(f$hypothese_censure, "traitement des sorties concurrentes fait par le producteur, non vérifiable") &&
    identical(f$version_fiche, 6L)
})
verifier("C9 hypotheses_recette : champ présent sous les agrégats, entre hypothese_censure et hypotheses_brut, data.frame (code, enonce, nature, statut), statut « hypothèse déclarée, non vérifiable par le moteur » ; absent sous les lignes (champ conditionnel)", {
  f <- XA$fiche; h <- f$hypotheses_recette; n <- names(f)
  is.data.frame(h) && identical(names(h), c("code", "enonce", "nature", "statut")) &&
    identical(h$code, c("exposition_centrale_declaree", "exposition_initiale_declaree", "unite_exposition",
                        "convention_declaree", "une_annee_d_age", "periode_delai_producteur", "evenements_transition",
                        "unite_observation")) &&
    all(startsWith(h$statut, "hypothèse déclarée, ")) &&
    all(startsWith(h$statut[h$code != "periode_delai_producteur"], "hypothèse déclarée, non vérifiable par le moteur")) &&
    identical(match("hypotheses_recette", n), match("hypothese_censure", n) + 1L) &&
    identical(match("hypotheses_brut", n), match("hypotheses_recette", n) + 1L) &&
    identical(h, XA$etapes$exposition$parametres$hypotheses_recette) &&
    !("hypotheses_recette" %in% names(XL$fiche)) && !("hypotheses_recette" %in% names(XL$etapes$exposition$parametres))
})
verifier("hypotheses_recette : hypothèse de l'exposition non déclarée omise ; statut « vérifiable en partie » (annee_hors_periode) avec le rôle annee, textes visés par actuary", {
  h1 <- tablexp:::hypotheses_agregats("exposition_centrale", FALSE, "dernier_anniversaire")
  h2 <- tablexp:::hypotheses_agregats(c("exposition_centrale", "exposition_initiale"), TRUE, "age_atteint_annee")
  !("exposition_initiale_declaree" %in% h1$code) && nrow(h1) == 7L && nrow(h2) == 8L &&
    identical(h2$statut[h2$code == "periode_delai_producteur"],
              paste("hypothèse déclarée, vérifiable en partie par le moteur : motif annee_hors_periode (années civiles",
                    "hors de la période ; troncature et délai à l'intérieur d'une année non vérifiables)")) &&
    identical(h2$enonce[h2$code == "convention_declaree"],
              paste("âge des cellules (et année, si le rôle annee est déclaré) formés par le producteur sous la",
                    "convention déclarée « age_atteint_annee », retenue telle quelle ; aucune convention n'est déduite",
                    "des données ni forcée")) &&
    identical(h1$enonce[h1$code == "unite_exposition"],
              paste("exposition reçue en années de la convention déclarée (jours / L, L nombre de jours de l'année d'âge",
                    "sous dernier_anniversaire, de l'année civile sous age_atteint_annee : 365 ou 366), sans conversion ;",
                    "un décompte en jours / 365 ou jours / 365,25 l'écarte d'au plus 1/365 en relatif (borne exacte)")) &&
    !grepl("annee_hors_periode", h1$statut[h1$code == "periode_delai_producteur"], fixed = TRUE) &&
    grepl("« age_atteint_annee »", h2$enonce[h2$code == "convention_declaree"], fixed = TRUE)
})
verifier("C9 rejeu : rejouer_fiche() sur une construction en agrégats -> construction identical() hors execution, bilan sans écart (8 contrôles conformes, témoins identiques)", {
  sx <- function(x) { x$execution <- NULL; x }
  r <- rejouer_fiche(AGG, lire_fiche(ecrire_fiche(XA$fiche)), tables_reference = list(tv = T_HF))
  identical(sx(r$construction), sx(XA)) && all(r$bilan$verdict[1:8] == "conforme") &&
    all(r$bilan$verdict[9:13] == "identique")
})
verifier("rapport figé sous les agrégats : mention d'unité non déterminée (positionnement, homogeneite, validation), jamais la mention d'unité contrat (avis d'actuary, point 3) ; hypotheses_recette restituée", {
  h <- rawToChar(ecrire_rapport(XA)); Encoding(h) <- "UTF-8"
  m <- tablexp:::MENTIONS_RAPPORT
  esc <- tablexp:::echapper_html
  n_nd <- lengths(regmatches(h, gregexpr(esc(m[["validation_unite_non_determinee"]]), h, fixed = TRUE)))
  n_nd == 3L && !grepl(esc(m[["validation_unite_contrat"]]), h, fixed = TRUE) &&
    grepl("fiche/hypotheses_recette", h, fixed = TRUE)
})
verifier("rapport figé : texte de validation_unite_non_determinee visé par actuary (événements, et non décès, sous toute transition)",
         identical(unname(tablexp:::MENTIONS_RAPPORT[["validation_unite_non_determinee"]]), paste(
           "Unité d'observation non déterminée (agrégats ; fiche/avertissement_unite) : si les effectifs comptent des",
           "contrats, les événements des contrats d'une même tête ne sont pas indépendants ; la variance des événements",
           "est sur-dispersée d'un facteur au plus phi = somme k^2 E / somme k E >= 1 (k nombre de contrats d'une tête),",
           "atteint quand les événements d'une tête sont simultanés, comme un décès compté sur chacun de ses contrats ;",
           "les tests sont alors anti-conservateurs (p-valeurs trop petites, intervalles trop étroits) ; phi n'est pas",
           "mesurable sur des agrégats.")))
verifier("rapport figé : mention_unite() rend la mention contrat sous l'unité contrat, la mention non déterminée sous non_determinee, rien sous l'unité tête ou sans avertissement d'unité", {
  mu <- tablexp:::mention_unite
  m <- tablexp:::MENTIONS_RAPPORT
  x <- function(unite, au) list(fiche = list(unite_observation = unite, avertissement_unite = au))
  identical(mu(x("contrat", "a")), tablexp:::paragraphe(m[["validation_unite_contrat"]])) &&
    identical(mu(x("non_determinee", "a")), tablexp:::paragraphe(m[["validation_unite_non_determinee"]])) &&
    is.null(mu(x("tete", "a"))) && is.null(mu(x("contrat", NA_character_))) && is.null(mu(x("non_determinee", NA_character_)))
})

# --- C2 : refus de la matrice et règles conditionnelles -----------------------------

AG2 <- data.frame(sexe = c("H", "F"), age = c(60, 61), annee = c(2016, 2017), ec = c(10, 12), ei = c(10.5, 12),
                  d = c(1, 0), seg = c("a", "b"), ct = c("C1", "C2"), stringsAsFactors = FALSE)
R_EC <- c(sexe = "sexe", age = "age", exposition_centrale = "ec", evenements = "d")
R_EI <- c(sexe = "sexe", age = "age", exposition_initiale = "ei", evenements = "d")
msg <- function(r) if (is.null(r)) "" else conditionMessage(r)
verifier("C2 poisson sans exposition_centrale -> poisson_sans_exposition_centrale, module donnees, message de la matrice (estimateur si_role sans son rôle)", {
  r <- rf(construire_table(AG2, R_EI, plan_ag(BRUT)))
  ok <- controle(r, "donnees", "poisson_sans_exposition_centrale")
  if (!isTRUE(ok)) ok else identical(msg(r), paste("Sous la recette « agregats », l'estimateur « poisson » de l'étape « brut » n'est pas",
                                                   "admis sans le rôle « exposition_centrale » ; la construction est refusée.")) &&
    identical(r$details, list(recette = "agregats", etape = "brut", estimateur = "poisson", role = "exposition_centrale"))
})
verifier("C2 binomial sans exposition_initiale -> binomial_sans_exposition_initiale (message de la matrice)", {
  r <- rf(construire_table(AG2, R_EC, plan_ag(BRUT)))
  ok <- controle(r, "donnees", "binomial_sans_exposition_initiale")
  if (!isTRUE(ok)) ok else identical(msg(r), paste("Sous la recette « agregats », l'estimateur « binomial » de l'étape « brut » n'est pas",
                                                   "admis sans le rôle « exposition_initiale » ; la construction est refusée."))
})
verifier("C8 Kaplan-Meier -> kaplan_meier_agregats dans tous les cas (les deux expositions déclarées), message de la matrice (estimateur non admis, sans condition)", {
  r <- rf(construire_table(AG2, c(R_EC, exposition_initiale = "ei"),
                           plan_ag(list(brut = list(estimateurs = c("poisson", "kaplan_meier"), niveau = 0.95)))))
  ok <- controle(r, "donnees", "kaplan_meier_agregats")
  if (!isTRUE(ok)) ok else identical(msg(r), paste("Sous la recette « agregats », l'estimateur « kaplan_meier » de l'étape « brut »",
                                                   "n'est pas admis ; la construction est refusée.")) &&
    identical(r$details, list(recette = "agregats", etape = "brut", estimateur = "kaplan_meier"))
})
verifier("C2 positionnement, homogeneite et validation sans exposition_centrale -> etape_exige_exposition_centrale, message de la matrice (étape si_role sans son rôle), avant le contrôle des tables", {
  pos <- rf(construire_table(AG2, R_EI, plan_ag(POS), tables_reference = list(tv = T_HF)))
  hom <- rf(construire_table(AG2, R_EI, plan_ag(list(homogeneite = list(plage_ages = c(60, 61), tranches_age = 60,
                                                                       replications = 9)), graine = 7L)))
  val <- rf(construire_table(AG2, R_EI, plan_ag(VAL, graine = 7L), tables_reference = list(tv = T_HF)))
  ok <- c(controle(pos, "donnees", "etape_exige_exposition_centrale"), controle(hom, "donnees", "etape_exige_exposition_centrale"),
          controle(val, "donnees", "etape_exige_exposition_centrale"))
  if (!all(ok == "TRUE")) paste(ok, collapse = " ; ")
  else identical(msg(pos), paste("Sous la recette « agregats », l'étape « positionnement » n'est pas admise sans le rôle",
                                 "« exposition_centrale » ; la construction est refusée.")) &&
    identical(hom$details$etape, "homogeneite") && identical(val$details$etape, "validation")
})
verifier("C2 importance_exclusions -> importance_exclusions_agregats, message de la matrice (étape non admise, sans condition)", {
  r <- rf(construire_table(AG2, R_EC, plan_ag(list(importance_exclusions = list(niveau = 0.95)))))
  ok <- controle(r, "donnees", "importance_exclusions_agregats")
  if (!isTRUE(ok)) ok else identical(msg(r), paste("Sous la recette « agregats », l'étape « importance_exclusions » n'est pas admise ;",
                                                   "la construction est refusée.")) &&
    identical(r$details, list(recette = "agregats", etape = "importance_exclusions", estimateur = NA_character_))
})
verifier("C2 par_annee = TRUE sans le rôle annee -> par_annee_sans_annee, module donnees ; avec annee -> admis", {
  r <- rf(construire_table(AG2, R_EC, plan_ag(par_annee = TRUE)))
  ok <- controle(r, "donnees", "par_annee_sans_annee")
  if (!isTRUE(ok)) ok else identical(r$details, list(recette = "agregats", etape = "exposition", role = "annee")) &&
    grepl("exige le rôle « annee »", msg(r), fixed = TRUE) &&
    is.null(rf(construire_table(AG2, c(R_EC, annee = "annee"), plan_ag(par_annee = TRUE))))
})
verifier("C2 binomial avec annee sous dernier_anniversaire -> binomial_annee_dernier_anniversaire, message visé par actuary ; sous age_atteint_annee, ou sans annee -> admis", {
  ro <- c(R_EC, exposition_initiale = "ei", annee = "annee")
  r <- rf(construire_table(AG2, ro, plan_ag(BRUT)))
  ok <- controle(r, "donnees", "binomial_annee_dernier_anniversaire")
  if (!isTRUE(ok)) ok else identical(r$details$convention, "dernier_anniversaire") &&
    identical(msg(r), paste("Sous la recette « agregats », l'estimateur « binomial » n'est pas admis avec le rôle « annee »",
                            "sous la convention « dernier_anniversaire » : l'exposition initiale d'une année d'âge n'est pas",
                            "la somme de celles des carrés de Lexis (le complément d'un événement court jusqu'à",
                            "l'anniversaire suivant, à travers la fin de l'année civile) ; retenir l'estimateur",
                            "« poisson », ou fournir des cellules par âge seul dont l'exposition initiale a été formée",
                            "par le producteur sur l'année d'âge ; la construction est refusée.")) &&
    is.null(rf(construire_table(AG2, ro, plan_ag(BRUT, convention = "age_atteint_annee")))) &&
    is.null(rf(construire_table(AG2, ro[names(ro) != "annee"], plan_ag(BRUT))))
})
verifier("C7 convention plus_fine -> convention_non_declaree, module exposition ; convention déclarée retenue telle quelle (native NA, non forcée) avec l'avertissement convention_declaree", {
  r <- rf(construire_table(AG2, R_EC, plan_ag(convention = "plus_fine")))
  x <- construire_table(AG2, R_EC, plan_ag(convention = "age_atteint_annee"))
  ok <- controle(r, "exposition", "convention_non_declaree")
  if (!isTRUE(ok)) ok else
    identical(x$etapes$exposition$parametres$convention,
              list(demandee = "age_atteint_annee", native = NA_character_, retenue = "age_atteint_annee", forcee = FALSE)) &&
      identical(x$avertissements$message[x$avertissements$code == "convention_declaree"],
                "Convention d'âge déclarée « age_atteint_annee », non vérifiable sur des agrégats.")
})
verifier("C2 exclusion sans objet pour les agrégats (motif des lignes) -> exclusion_hors_recette, message visé par actuary", {
  r <- rf(construire_table(AG2, R_EC, plan_ag(exclusions = "contrat_duplique")))
  ok <- controle(r, "donnees", "exclusion_hors_recette")
  if (!isTRUE(ok)) ok else startsWith(msg(r), "Code d'exclusion sans objet pour la recette « agregats » : « contrat_duplique » (motifs de cette recette : « age_manquant »")
})
verifier("C2 rejeu : un plan non admis sous la recette des données est refusé par rejouer_fiche() avant construire_table() (couche partagée, ADR 0016, point 5.3)", {
  f <- construire_table(AG2, R_EC, plan_ag())$fiche
  f$plan$etapes$brut <- list(estimateurs = "binomial", niveau = 0.95)
  isTRUE(controle(rf(rejouer_fiche(AG2, f)), "donnees", "binomial_sans_exposition_initiale"))
})

# --- C3 : motifs de ligne, levés seuls, refusés par défaut, excluables ---------------

# Base valide de quatre cellules (deux sexes, deux segments, deux âges, deux
# années), puis une ligne modifiée par motif.
BV <- data.frame(sexe = c("H", "H", "F", "F"), seg = c("a", "b", "a", "b"), age = c(60, 61, 60, 61),
                 annee = c(2016, 2017, 2016, 2017), ec = c(10, 11, 12, 13), ei = c(10.5, 11, 12.25, 13),
                 d = c(1, 0, 1, 0), stringsAsFactors = FALSE)
R_BV <- c(sexe = "sexe", segment = "seg", age = "age", annee = "annee", exposition_centrale = "ec",
          exposition_initiale = "ei", evenements = "d")
P_BV <- tablexp:::valeurs_parametres_donnees(VD, PER, 0)
motifs_de <- function(d) { m <- tablexp:::evaluer_motifs_agregats(d, R_BV, P_BV)$motifs; m[lengths(m) > 0L] }
modif <- function(col, valeur, ligne = 2L) { d <- BV; d[[col]][ligne] <- valeur; d }
verifier("C3 base valide : aucun motif ; liste nommée dans l'ordre MOTIFS_LIGNES_AGREGATS",
         !length(motifs_de(BV)) &&
           identical(names(tablexp:::evaluer_motifs_agregats(BV, R_BV, P_BV)$motifs), tablexp:::MOTIFS_LIGNES_AGREGATS))
CAS_C3 <- list(
  age_manquant = modif("age", NA), age_non_entier = modif("age", 60.5), age_negatif = modif("age", -1),
  age_aberrant = modif("age", 130), annee_manquante = modif("annee", NA), annee_non_entiere = modif("annee", 2016.5),
  annee_hors_periode = modif("annee", 2025), sexe_manquant = modif("sexe", NA), sexe_hors_modalites = modif("sexe", "X"),
  segment_manquant = modif("seg", NA), exposition_manquante = modif("ei", NA), exposition_non_finie = modif("ec", Inf),
  exposition_negative = modif("ei", -0.5), evenements_manquants = modif("d", NA), evenements_non_entiers = modif("d", 0.5),
  evenements_negatifs = modif("d", -1),
  evenements_sans_exposition = { d <- BV; d$ec[2L] <- 0; d$ei[2L] <- 0; d$d[2L] <- 2; d },
  cellule_dupliquee = { d <- BV; d$annee[3L] <- 2017; d$age[3L] <- 61; d$seg[3L] <- "b"; d$sexe[3L] <- "H"; d })
verifier("C3 chaque motif de la spécification levé seul sur une ligne construite à la main (cellule_dupliquee : les deux lignes de même clé)", {
  e <- character(0)
  for (m in names(CAS_C3)) {
    o <- motifs_de(CAS_C3[[m]])
    attendu <- if (m == "cellule_dupliquee") list(2:3) else list(2L)
    if (!identical(o, stats::setNames(attendu, m))) e <- c(e, m)
  }
  if (identical(names(CAS_C3), tablexp:::MOTIFS_LIGNES_AGREGATS) && !length(e)) TRUE else paste("écart :", paste(e, collapse = ", "))
})
verifier("C3 valeurs infinies : âge, année et événements infinis sont non entiers (non finis compris) ; exposition -Inf non finie et négative ; NaN manquant", {
  identical(names(motifs_de(modif("age", Inf))), c("age_non_entier", "age_aberrant")) &&
    identical(names(motifs_de(modif("d", Inf))), "evenements_non_entiers") &&
    identical(names(motifs_de(modif("ec", -Inf))), c("exposition_non_finie", "exposition_negative")) &&
    identical(names(motifs_de(modif("ec", NaN))), "exposition_manquante")
})
verifier("C3 evenements_sans_exposition : non levé si une exposition déclarée est > 0 (E^c = 0 < E^i), ni si une exposition est manquante (aucun motif de comparaison sur NA)", {
  d1 <- BV; d1$ec[2L] <- 0; d1$d[2L] <- 1
  d2 <- BV; d2$ec[2L] <- NA; d2$ei[2L] <- 0; d2$d[2L] <- 1
  !length(motifs_de(d1)) && identical(names(motifs_de(d2)), "exposition_manquante")
})
verifier("C3 refus par défaut : ligne_invalide, décompte et premières lignes ; exclusion déclarée : cellule écartée, comptée et listée", {
  pl <- plan_ag(segmentation = character(0))
  r <- rf(construire_table(CAS_C3$age_negatif, R_BV, pl))
  x <- construire_table(CAS_C3$age_negatif, R_BV, plan_ag(segmentation = character(0), exclusions = "age_negatif"))
  isTRUE(controle(r, "donnees", "ligne_invalide")) && identical(r$details$lignes, list(age_negatif = 2L)) &&
    identical(x$etapes$donnees$resultat$numeros_conserves, c(1L, 3L, 4L)) &&
    identical(x$etapes$donnees$resultat$exclusions, data.frame(motif = "age_negatif", nombre = 1L, stringsAsFactors = FALSE)) &&
    identical(x$fiche$nombre_lignes_exclues, 1L)
})
verifier("C3 diagnostic Q6 (calculé à la main) : par motif exclu, cellules exclues, puis cellules sans motif de valeur sur D et E, Σ D et Σ E de celles-ci", {
  d <- BV; d$age[1L] <- 140; d$d[2L] <- -1; d$age[2L] <- 140
  x <- construire_table(d, R_BV, plan_ag(segmentation = character(0), exclusions = c("age_aberrant", "evenements_negatifs")))
  pe <- x$etapes$donnees$diagnostics$releves$poids_exclusions
  identical(pe, data.frame(motif = c("age_aberrant", "evenements_negatifs"), cellules = c(2L, 1L), cellules_sommees = c(1L, 0L),
                           evenements = c(1, 0), exposition_centrale = c(10, 0), exposition_initiale = c(10.5, 0),
                           stringsAsFactors = FALSE))
})

# --- C4 : relevés et avertissements ----------------------------------------------------

verifier("C4 relevés de cohérence des deux expositions : E^i < E^c et E^i - E^c > D comptés et avertis, sans refus", {
  d <- BV; d$ei[1L] <- 9.5; d$ei[3L] <- 14
  x <- construire_table(d, R_BV, plan_ag(segmentation = c("sexe", "segment")))
  a <- x$avertissements; rl <- x$etapes$donnees$diagnostics$releves
  identical(rl$exposition_initiale_inferieure_centrale, 1L) && identical(rl$complement_superieur_aux_evenements, 1L) &&
    identical(a$nombre[a$code == "exposition_initiale_inferieure_centrale"], 1L) &&
    identical(a$nombre[a$code == "complement_superieur_aux_evenements"], 1L)
})
verifier("C4 une seule exposition déclarée : relevés de cohérence NA, aucun avertissement", {
  x <- construire_table(AG2, R_EC, plan_ag())
  rl <- x$etapes$donnees$diagnostics$releves
  is.na(rl$exposition_initiale_inferieure_centrale) && is.na(rl$complement_superieur_aux_evenements) &&
    !any(c("exposition_initiale_inferieure_centrale", "complement_superieur_aux_evenements") %in% x$avertissements$code)
})
# --- C4 : tolérance d'arrondi des relevés de cohérence (#195, M381) --------------------
# Approximation numérique : écart relevé au-delà de 1e-10 * max(|E^i|, |E^c|)
# (TOLERANCE_RELEVES_AGREGATS). Attendus écrits à la main ; la dérive d'arrondi
# est vérifiée présente avant de conclure à son absorption par la tolérance.
R_REL <- c(age = "age", exposition_centrale = "ec", exposition_initiale = "ei", evenements = "d")
CODES_REL <- c("exposition_initiale_inferieure_centrale", "complement_superieur_aux_evenements")
releves_de <- function(ec, ei, d, age = seq_along(ec) + 19, roles = R_REL, plan = plan_ag(segmentation = character(0))) {
  x <- construire_table(data.frame(age = age, ec = ec, ei = ei, d = d), roles, plan)
  list(rl = unlist(x$etapes$donnees$diagnostics$releves[CODES_REL]),
       codes = x$avertissements$code[x$avertissements$code %in% CODES_REL],
       textes = x$avertissements$message[x$avertissements$code %in% CODES_REL])
}
verifier("C4 #195 (CA1) : 2,14 - 1,14 = 1 + 2,2e-16 > D = 1 en double, mais cellule 1,14 / 2,14 / 1 non relevée (relevés 0 et 0, aucun avertissement de relevé)", {
  r <- releves_de(1.14, 2.14, 1)
  derive <- 2.14 - 1.14 > 1
  if (!derive) "témoin : la dérive d'arrondi 2,14 - 1,14 > 1 n'existe plus" else
    if (identical(unname(r$rl), c(0L, 0L)) && !length(r$codes)) TRUE else
      sprintf("relevés %s, avertissements %s", paste(r$rl, collapse = " et "), paste(r$codes, collapse = ", "))
})
verifier("C4 #195 (CA1) : grille de 500 cellules distinctes (âge 20 à 69 x année 2015 à 2024 ; E^c = k/100, E^i = E^c + 1, D = 1) sous par_annee : relevés 0 et 0, aucun avertissement de relevé ; la comparaison exacte en relèverait 48", {
  k <- 1:500; ec <- k / 100; ei <- ec + 1
  exacts <- sum(ei - ec > 1) + sum(ei < ec)
  g <- expand.grid(age = 20:69, annee = 2015:2024)
  x <- construire_table(data.frame(age = g$age, annee = g$annee, ec = ec, ei = ei, d = 1),
                        c(R_REL, annee = "annee"), plan_ag(segmentation = character(0), par_annee = TRUE))
  rl <- unlist(x$etapes$donnees$diagnostics$releves[CODES_REL])
  if (!identical(exacts, 48L)) sprintf("témoin : %d relevé(s) exact(s) au lieu de 48", exacts) else
    if (identical(unname(rl), c(0L, 0L)) && !any(CODES_REL %in% x$avertissements$code) &&
        identical(x$etapes$donnees$diagnostics$releves$cellules, 500L)) TRUE else
      sprintf("relevés %s", paste(rl, collapse = " et "))
})
verifier("C4 #195 (CA1, sens E^i < E^c) : E^c = 0,1 + 0,2 et E^i = 0,3 (E^i < E^c en double, égaux en décimal) : non relevée", {
  r <- releves_de(0.1 + 0.2, 0.3, 1)
  if (!(0.3 < 0.1 + 0.2)) "témoin : la dérive 0,3 < 0,1 + 0,2 n'existe plus" else
    if (identical(unname(r$rl), c(0L, 0L)) && !length(r$codes)) TRUE else paste("relevés", paste(r$rl, collapse = " et "))
})
verifier("C4 #195 (CA2) : écarts réels (E^i = 9,5 < E^c = 10 ; E^i - E^c = 2 > D = 1) toujours relevés, 1 et 1, avec les deux avertissements et leurs textes complétés de la tolérance", {
  r <- releves_de(c(10, 12), c(9.5, 14), c(1, 1))
  mention <- "au-delà d'une tolérance d'arrondi de 1e-10 fois la plus grande des deux expositions déclarées de la cellule"
  if (identical(unname(r$rl), c(1L, 1L)) && identical(r$codes, CODES_REL) && length(r$textes) == 2L &&
      all(grepl(mention, r$textes, fixed = TRUE))) TRUE else
    sprintf("relevés %s, codes %s", paste(r$rl, collapse = " et "), paste(r$codes, collapse = ", "))
})
verifier("C4 #195 (m1) : le nombre de la tolérance dans les deux textes d'avertissement est lu dans TOLERANCE_RELEVES_AGREGATS (constante remplacée par 1e-3 dans l'espace de noms le temps d'un appel, puis restaurée), mention complète « 0.001 » attendue à la main, aucune trace de « 1e-10 » ; valeur d'origine « 1e-10 » rendue à l'identique", {
  ns <- asNamespace("tablexp"); nom <- "TOLERANCE_RELEVES_AGREGATS"
  ancienne <- get(nom, envir = ns)
  unlockBinding(nom, ns); assign(nom, 1e-3, envir = ns)
  r <- tryCatch(releves_de(c(10, 12), c(9.5, 14), c(1, 1)), finally = { assign(nom, ancienne, envir = ns); lockBinding(nom, ns) })
  mention <- "au-delà d'une tolérance d'arrondi de 0.001 fois la plus grande des deux expositions déclarées de la cellule"
  if (!identical(ancienne, 1e-10) || !bindingIsLocked(nom, ns) || !identical(get(nom, envir = ns), 1e-10))
    "constante non restaurée ou valeur d'origine différente de 1e-10" else
    if (identical(r$codes, CODES_REL) && length(r$textes) == 2L && all(grepl(mention, r$textes, fixed = TRUE)) &&
        !any(grepl("1e-10", r$textes, fixed = TRUE))) TRUE else
      sprintf("codes %s ; textes : %s", paste(r$codes, collapse = ", "), paste(substr(r$textes, 1L, 200L), collapse = " | "))
})
verifier("C4 #195 (CA3) : un jour (1/366 an) relevé dans les deux sens à E = 1e5 ans : E^i = E^c + D + 1/366 (supérieur aux événements) et E^i = E^c - 1/366, D = 0 (inférieure)", {
  sup <- releves_de(1e5, 1e5 + 1 + 1 / 366, 1)
  inf <- releves_de(1e5, 1e5 - 1 / 366, 0)
  if (identical(unname(sup$rl), c(0L, 1L)) && identical(unname(inf$rl), c(1L, 0L))) TRUE else
    sprintf("supérieur : %s ; inférieure : %s", paste(sup$rl, collapse = " et "), paste(inf$rl, collapse = " et "))
})
verifier("C4 #195 (CA4) : deux chemins de sommation d'une cellule sans événement (1e4 fractions j/366, sous sous_graine(195L)) : E^c = sum(x), E^i = Reduce(+, rev(x)), E^i != E^c, écart relatif < 1e-10 : non relevée (0 et 0)", {
  x <- tablexp:::sous_graine(195L, sample(366L, 1e4, replace = TRUE)) / 366
  ec <- sum(x); ei <- Reduce(`+`, rev(x))
  r <- releves_de(ec, ei, 0)
  if (identical(ei, ec)) "témoin : les deux sommes sont identiques" else
    if (!(abs(ei - ec) / ec < 1e-10)) "témoin : écart relatif des deux sommes >= 1e-10" else
      if (identical(unname(r$rl), c(0L, 0L)) && !length(r$codes)) TRUE else paste("relevés", paste(r$rl, collapse = " et "))
})
verifier("C4 #195 (relative, non absolue) : E^c = 8388607,14, E^i = 8388608,14, D = 1 : E^i - E^c - D = 9,3e-10 (> 1e-10 absolu, < 1e-10 * E^i = 8,4e-4) : non relevée", {
  ec <- 8388607.14; ei <- 8388608.14
  ecart <- ei - ec - 1
  r <- releves_de(ec, ei, 1)
  if (!(ecart > 1e-10 && ecart < 1e-10 * ei)) sprintf("témoin : écart %g hors de (1e-10, 1e-10 * E^i)", ecart) else
    if (identical(unname(r$rl), c(0L, 0L)) && !length(r$codes)) TRUE else paste("relevés", paste(r$rl, collapse = " et "))
})
verifier("C4 #195 (pmax, non pmin) : E^c = 0, E^i = 1e6 + 1e-5, D = 1e6 : E^i - E^c - D = 1e-5 < 1e-10 * E^i = 1e-4 (pmax) mais > 0 (pmin donnerait tau = 0) : non relevée", {
  ei <- 1e6 + 1e-5
  ecart <- ei - 0 - 1e6
  r <- releves_de(0, ei, 1e6)
  if (!(ecart > 9.9e-6 && ecart < 1.01e-5)) sprintf("témoin : écart %g au lieu de 1e-5 environ", ecart) else
    if (identical(unname(r$rl), c(0L, 0L)) && !length(r$codes)) TRUE else paste("relevés", paste(r$rl, collapse = " et "))
})
verifier("C4 #195 (CA6) : une exposition manquante laisse les deux relevés NA (pas de na.rm), comme avant la tolérance ; E^i = E^c = 0 : tolérance nulle, rien relevé", {
  ro <- R_REL
  l <- data.frame(age = 60:62, ec = c(1, NA, 3), ei = c(2, 2, 3), d = c(0, 0, 0))
  rl <- tablexp:::observations_agregats(l, 1:3, ro, character(0), list(), character(0), 3L, l)$releves
  r0 <- releves_de(0, 0, 0)
  if (!(is.na(rl$exposition_initiale_inferieure_centrale) && is.na(rl$complement_superieur_aux_evenements)))
    "relevés non NA avec une exposition manquante" else
    if (identical(unname(r0$rl), c(0L, 0L))) TRUE else paste("E^i = E^c = 0 :", paste(r0$rl, collapse = " et "))
})
verifier("C4 avertissements des agrégats, dans l'ordre (valeur écrite à la main) : unite_agregat (transition sur la tête), role_non_employe, convention_declaree, annees_sommees, ages_manquants", {
  d <- BV; d$age[4L] <- 63
  x <- construire_table(d, R_BV, plan_ag(segmentation = "sexe"))
  a <- x$avertissements
  identical(a$code, c("unite_agregat", "role_non_employe", "convention_declaree", "annees_sommees", "ages_manquants")) &&
    identical(a$nombre, c(4L, 1L, 1L, 1L, 2L)) &&
    identical(a$message[1L], tablexp:::UNITE_AGREGAT[["tete"]]) &&
    identical(x$etapes$exposition$diagnostics$cellules_absentes_de_la_plage, 2L)
})
verifier("C4 unite_agregat sous une transition portée par le contrat : texte propre (avis d'actuary, point 3)", {
  x <- construire_table(AG2, R_EC, plan_ag(transition = RA))
  a <- x$avertissements
  identical(a$message[a$code == "unite_agregat"], tablexp:::UNITE_AGREGAT[["contrat"]]) &&
    grepl("la transition porte sur le contrat", tablexp:::UNITE_AGREGAT[["contrat"]], fixed = TRUE)
})
verifier("C4 ages_manquants sous par_annee (spécification, § 6.3, révisée par actuary) : trous de la plage d'âges de chaque (segment, année) ; une année absente n'est pas un trou", {
  d <- data.frame(age = c(60, 62, 60, 61, 65), annee = c(2016, 2016, 2017, 2017, 2019), ec = 1, d = 0)
  ro <- c(age = "age", annee = "annee", exposition_centrale = "ec", evenements = "d")
  x <- construire_table(d, ro, plan_ag(segmentation = character(0), par_annee = TRUE))
  y <- construire_table(d, ro, plan_ag(segmentation = character(0), par_annee = FALSE))
  a <- x$avertissements
  # par année : 2016 -> 61 manque ; 2017, 2019 complètes : 1. Sans par_annee :
  # âges 60, 61, 62, 65 -> 63 et 64 manquent : 2.
  identical(x$etapes$exposition$diagnostics$cellules_absentes_de_la_plage, 1L) &&
    identical(a$message[a$code == "ages_manquants"],
              paste("Relevé : 1 cellule(s) absente(s) de la plage d'âges de leur segment et de leur année civile",
                    "(classes d'âge groupées possibles ; aucune cellule n'est créée).")) &&
    identical(y$etapes$exposition$diagnostics$cellules_absentes_de_la_plage, 2L) &&
    identical(y$avertissements$message[y$avertissements$code == "ages_manquants"],
              paste("Relevé : 2 cellule(s) absente(s) de la plage d'âges de leur segment (classes d'âge groupées",
                    "possibles ; aucune cellule n'est créée)."))
})
verifier("C4 annees_sommees : texte propre sous dernier_anniversaire avec exposition_initiale déclarée, texte commun sinon (textes visés par actuary)", {
  ro <- c(R_EC, annee = "annee")
  t1 <- function(x) x$avertissements$message[x$avertissements$code == "annees_sommees"]
  a <- construire_table(AG2, c(ro, exposition_initiale = "ei"), plan_ag())
  b <- construire_table(AG2, ro, plan_ag())
  c_ <- construire_table(AG2, c(ro, exposition_initiale = "ei"), plan_ag(convention = "age_atteint_annee"))
  commun <- paste("Rôle « annee » déclaré sous par_annee = FALSE : exposition et événements sommés sur les années de",
                  "chaque cellule (somme exacte pour l'exposition centrale et les événements).")
  identical(t1(a), paste("Rôle « annee » déclaré sous par_annee = FALSE : exposition et événements sommés sur les années",
                         "de chaque cellule (somme exacte pour l'exposition centrale et les événements ; sous",
                         "« dernier_anniversaire », la somme des expositions initiales des carrés de Lexis n'est pas",
                         "l'exposition initiale de l'année d'âge : la colonne exposition_initiale est la somme des",
                         "valeurs reçues, l'estimateur « binomial » est refusé).")) &&
    identical(t1(b), commun) && identical(t1(c_), commun)
})
verifier("C4 role_non_employe sous les agrégats : texte propre (cellules sommées sur ses modalités) ; sous les lignes, texte inchangé", {
  x <- construire_table(AG2, R_EC, plan_ag(segmentation = character(0)))
  identical(x$avertissements$message[x$avertissements$code == "role_non_employe"],
            paste("Rôle « sexe » déclaré, non employé par la segmentation du plan : contrôlé (motifs, clé des",
                  "cellules), puis cellules de même clé sommées sur ses modalités à l'étape exposition.")) &&
    identical(XL$avertissements$message[XL$avertissements$code == "role_non_employe"], character(0))
})
verifier("evenements_hors_plage (appel direct du seam cellules) : somme au-delà de .Machine$integer.max -> refus, module exposition, message visé par actuary", {
  rec <- tablexp:::deduire_recette(c(age = "a", evenements = "d", exposition_centrale = "e"))
  o <- data.frame(age = c(60, 60), exposition_centrale = c(1, 1), evenements = c(2e9, 2e9))
  r <- rf(tablexp:::exposition_cellules(o, P_BV, character(0), rec,
                                        tablexp:::resoudre_convention("dernier_anniversaire", NA_character_), FALSE))
  ok <- controle(r, "exposition", "evenements_hors_plage")
  if (!isTRUE(ok)) ok else
    identical(msg(r), sprintf(paste("Somme des événements hors de la plage des entiers du moteur (au plus %d) dans 1",
                                    "cellule(s) ; vérifier la colonne du rôle « evenements » ; la construction est refusée."),
                              .Machine$integer.max)) && identical(r$details, list(cellules = 1L))
})

# Garde du total des événements (#199, M397 ; ADR 0009, point 9.6, (d) 3) :
# agrégats du jeu simulé (forme de l'entrée de R13), deux cellules (âges 40
# et 60, sexe H) portées à 1,5e9 événements, chacune sous la borne de la
# garde par cellule : construire_table() refuse à l'étape exposition, avant
# toute étape en aval ; un total exactement égal à .Machine$integer.max
# n'est pas refusé et reste integer dans les diagnostics des étapes.
P_TOTAL <- plan_ag(c(BRUT, POS))
tous <- function(...) {
  l <- list(...)
  for (n in names(l)) if (!isTRUE(l[[n]])) return(paste(n, ":", l[[n]]))
  TRUE
}
verifier("garde du total (#199, M397) : deux cellules à 1,5e9 événements (sous la garde par cellule) -> refus exposition / denombrement_hors_plage, message et détails (grandeur evenements, valeur, limite) ; total égal à .Machine$integer.max -> construction aboutie, totaux integer", {
  k <- which(AGG$sexe == "H" & AGG$age %in% c(40, 60))
  g <- AGG; g$d[k] <- 1500000000L
  total <- sum(as.numeric(g$d))
  r <- rf(construire_table(g, R_AG, P_TOTAL, tables_reference = list(tv = T_HF)))
  b <- AGG; b$d[k[1L]] <- as.integer(b$d[k[1L]] + (.Machine$integer.max - sum(as.numeric(AGG$d))))
  x <- tryCatch(construire_table(b, R_AG, P_TOTAL, tables_reference = list(tv = T_HF)), refus_tablexp = function(e) e)
  ok <- controle(r, "exposition", "denombrement_hors_plage")
  tous(cellules = if (length(k) == 2L) TRUE else "deux cellules attendues",
       refus = ok,
       message = if (isTRUE(ok) && identical(msg(r), sprintf(paste(
         "Le dénombrement « evenements » (%.0f) est hors de la plage des entiers du moteur (au plus %d) : la",
         "construction est refusée."), total, .Machine$integer.max))) TRUE else "message",
       details = if (isTRUE(ok) && identical(r$details, list(grandeur = "evenements", valeur = total,
                                                             limite = .Machine$integer.max))) TRUE else "détails",
       borne = if (!inherits(x, "refus_tablexp") &&
                   identical(x$etapes$exposition$diagnostics$evenements, .Machine$integer.max) &&
                   identical(x$etapes$brut$diagnostics$evenements, .Machine$integer.max) &&
                   identical(x$etapes$positionnement$diagnostics$evenements_total, .Machine$integer.max)) TRUE
               else if (inherits(x, "refus_tablexp")) paste("refus inattendu :", x$motif) else "total à la borne")
})

# --- C5 et C6 : sommes et ordre ------------------------------------------------------

verifier("C5 sommes sur un segment non employé et sur les années (par_annee = FALSE), valeurs dyadiques : D et E égaux aux sommes, identical()", {
  x <- construire_table(BV, R_BV, plan_ag(segmentation = character(0)))
  e <- x$etapes$exposition$resultat$exposition
  identical(e, data.frame(age = c(60L, 61L), exposition_centrale = c(22, 24), exposition_initiale = c(22.75, 24),
                          evenements = c(2L, 0L)))
})
verifier("C5 sous par_annee = TRUE : une cellule par (segment, âge, année), sans somme, âge puis année croissants", {
  x <- construire_table(BV, R_BV, plan_ag(segmentation = "sexe", par_annee = TRUE))
  e <- x$etapes$exposition$resultat$exposition
  identical(names(e), c("sexe", "age", "annee", "exposition_centrale", "exposition_initiale", "evenements")) &&
    identical(e$sexe, c("F", "F", "H", "H")) && identical(e$age, c(60L, 61L, 60L, 61L)) &&
    identical(e$annee, c(2016L, 2017L, 2016L, 2017L)) && identical(e$exposition_centrale, c(12, 13, 10, 11))
})
verifier("C6 permutation des lignes reçues sans somme : table d'exposition et brut identical()", {
  o <- c(3L, 1L, 4L, 2L)
  pl <- plan_ag(BRUT, segmentation = c("sexe", "segment"))
  ro <- R_BV[names(R_BV) != "annee"]  # binomial avec annee refusé sous dernier_anniversaire
  a <- construire_table(BV, ro, pl); b <- construire_table(BV[o, ], ro, pl)
  identical(a$etapes$exposition$resultat, b$etapes$exposition$resultat) && identical(a$etapes$brut$resultat, b$etapes$brut$resultat)
})
verifier("C6 permutation du jeu agrégé de C1 : tables identical() (aucune somme)", {
  o <- rev(seq_len(nrow(AGG)))
  y <- construire_table(AGG[o, ], R_AG, plan_ag(BRUT))
  identical(y$etapes$brut$resultat, XA$etapes$brut$resultat)
})

# --- C8 : contre-exemple de Kaplan-Meier (spécification, § 4.3) ----------------------

# Naissance 1955-06-01, cellule 60 = [2015-06-01, 2016-06-01), 366 jours.
# A : une tête observée toute la cellule ; une tête décédée au 183e jour
#     (ensemble à risque 2 au décès : q_KM = 0,5).
# B : la même tête décédée ; une censurée la veille du décès (182 jours) ; une
#     entrée le lendemain du décès (182 jours) ; deux d'un jour en fin de
#     cellule (ensemble à risque 1 au décès : q_KM = 1).
# Mêmes agrégats de la cellule 60 : E^c = 549/366 = 1,5, D = 1, E^i = 2.
ligne <- function(e, s, c) data.frame(n = D("1955-06-01"), e = D(e), s = D(s), c = c, stringsAsFactors = FALSE)
KA <- rbind(ligne("2015-06-01", "2016-06-01", "rachat"), ligne("2015-06-01", "2015-12-01", "deces"))
KB <- rbind(ligne("2015-06-01", "2015-12-01", "deces"), ligne("2015-06-01", "2015-11-30", "rachat"),
            ligne("2015-12-02", "2016-06-01", "rachat"), ligne("2016-05-30", "2016-05-31", "rachat"),
            ligne("2016-05-31", "2016-06-01", "rachat"))
RK <- c(date_naissance = "n", date_entree = "e", date_sortie = "s", cause_sortie = "c")
PK <- plan_ag(list(brut = list(estimateurs = c("poisson", "binomial", "kaplan_meier"), niveau = 0.95)), segmentation = character(0))
verifier("C8 contre-exemple (deux jeux de lignes, mêmes agrégats de la cellule 60) : Kaplan-Meier 0,5 et 1, Poisson et binomial égaux ; sur ces agrégats, Kaplan-Meier refusé (kaplan_meier_agregats)", {
  xa <- construire_table(KA, RK, PK); xb <- construire_table(KB, RK, PK)
  ca <- function(x, est, col) { t <- x$etapes$brut$resultat[[est]]; t[[col]][t$age == 60L] }
  ea <- xa$etapes$exposition$resultat$exposition; eb <- xb$etapes$exposition$resultat$exposition
  agr <- function(e) { k <- e$age == 60L; c(e$exposition_centrale[k], e$evenements[k],
                                           e$exposition_centrale[k] + e$complement_initial_cellule[k]) }
  ag <- data.frame(age = 60, ec = 1.5, ei = 2, d = 1)
  r <- rf(construire_table(ag, c(age = "age", exposition_centrale = "ec", exposition_initiale = "ei", evenements = "d"), PK))
  isTRUE(all.equal(agr(ea), c(1.5, 1, 2), tolerance = 1e-14)) && isTRUE(all.equal(agr(eb), c(1.5, 1, 2), tolerance = 1e-14)) &&
    identical(ca(xa, "kaplan_meier", "taux"), 0.5) && identical(ca(xb, "kaplan_meier", "taux"), 1) &&
    isTRUE(all.equal(ca(xa, "poisson", "taux"), ca(xb, "poisson", "taux"), tolerance = 1e-14)) &&
    isTRUE(all.equal(ca(xa, "binomial", "taux"), ca(xb, "binomial", "taux"), tolerance = 1e-14)) &&
    isTRUE(controle(r, "donnees", "kaplan_meier_agregats"))
})

# --- C10, C11 : forme « agrégats » du simulateur (commit (3c)) ----------------------
#
# Natures : C10, identité exacte (jeu propre, appels répétés) et refus écrits à
# la main ; C11, recoupement simulateur <-> exposition, approximation numérique
# (deux arithmétiques de dates indépendantes), tolérance relative de classe
# 1e-10 (spécification (0') de #169, § 13). Écart maximal mesuré le 08/11/2026
# sur le jeu de ce fichier (graine 169, 4000 têtes), les deux transitions, avec
# et sans année : 0 (expositions identical()) ; un dépassement est un constat
# à remonter, jamais une tolérance à élargir.

LOI_SIM <- local({
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)), rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
})
PF_SIM <- list(mode = "uniforme", n = 4000, proportion_hommes = 0.5, age_min = 20, age_max = 75, origine = D("2005-01-01"))
forme_ag <- function(transition = VD, convention = "dernier_anniversaire", par_annee = FALSE)
  list(nom = "agregats", transition = transition, convention = convention, par_annee = par_annee)
sim_ag <- function(forme, portefeuille = PF_SIM, defauts = NULL)
  simuler_portefeuille(LOI_SIM, portefeuille, PER, 169L, defauts = defauts, forme = forme)
AG_SIM <- sim_ag(forme_ag())

verifier("C10 (identité exacte) : forme = NULL explicite identical() à l'appel sans forme ; deux appels identiques en agrégats identical()", {
  identical(simuler_portefeuille(LOI_SIM, PF_SIM, PER, 169L, forme = NULL), JEU) && identical(sim_ag(forme_ag()), AG_SIM)
})
verifier("C10 : attribut simulation des agrégats = celui du jeu propre (forme lignes, même graine), plus le champ forme en fin (forme validée) ; colonnes aux noms des rôles (sans annee : exposition_initiale ; avec annee : sans exposition_initiale)", {
  s0 <- attr(JEU, "simulation"); s1 <- attr(AG_SIM, "simulation")
  a2 <- sim_ag(forme_ag(RA, par_annee = TRUE))
  identical(names(s1), c(names(s0), "forme")) && identical(s1[names(s0)], s0) && identical(s1$forme, forme_ag()) &&
    identical(names(AG_SIM), c("sexe", "age", "exposition_centrale", "exposition_initiale", "evenements")) &&
    identical(names(a2), c("sexe", "age", "annee", "exposition_centrale", "evenements")) &&
    is.integer(AG_SIM$age) && is.integer(AG_SIM$evenements) && is.integer(a2$annee) && is.character(AG_SIM$sexe) &&
    identical(attr(a2, "simulation")[names(s0)], s0)
})
verifier("C10 : événements des agrégats = sorties de la cause comptée dans [D, B) du jeu propre (décès ; rachats), sans année et par année", {
  dans <- function(cause) sum(JEU$cause_sortie == cause & !is.na(JEU$date_sortie) & JEU$date_sortie >= PER$debut &
                                JEU$date_sortie <= PER$fin)
  sum(AG_SIM$evenements) == dans("deces") && sum(sim_ag(forme_ag(RA))$evenements) == dans("rachat") &&
    sum(sim_ag(forme_ag(par_annee = TRUE))$evenements) == dans("deces")
})
verifier("C10 (refus écrits à la main) : forme avec defauts, mode multi-contrats, convention age_atteint_annee, forme mal formée -> forme_invalide, module simulation, avant tout tirage (aucun appel de sous_graine(), état .Random.seed de l'appelant inchangé)", {
  dft <- list(graine = 170L, declarations = list(list(type = "sexe_manquant", cible = "deces", nombre = 1L)))
  pf_multi <- c(PF_SIM, list(loi_contrats = c(0.5, 0.5))); pf_multi$mode <- "uniforme_multi"
  # « Avant tout tirage » : tous les tirages du simulateur passent par
  # sous_graine() (outils), qui rétablit l'état de l'appelant même après un
  # tirage ; .Random.seed seul ne le prouve donc pas. Les appels de
  # sous_graine() sont comptés par trace() pendant les refus, retiré ensuite.
  # Témoin : un appel sans forme compte un appel.
  ns <- asNamespace("tablexp"); cpt <- new.env(); cpt$n <- 0L
  suppressMessages(trace("sous_graine", tracer = bquote(assign("n", get("n", envir = .(cpt)) + 1L, envir = .(cpt))),
                         where = ns, print = FALSE))
  set.seed(5L); graine_avant <- .Random.seed
  cas <- tryCatch({
    invisible(simuler_portefeuille(LOI_SIM, PF_SIM, PER, 169L))
    temoin <- cpt$n
    cpt$n <- 0L; graine_temoin <- .Random.seed
    list(
      defauts = rf(sim_ag(forme_ag(), defauts = dft)),
      multi = rf(sim_ag(forme_ag(), portefeuille = pf_multi)),
      age_atteint_annee = rf(sim_ag(forme_ag(convention = "age_atteint_annee"))),
      convention = rf(sim_ag(forme_ag(convention = "plus_fine"))),
      nom = rf(sim_ag(list(nom = "stocks", transition = VD, convention = "dernier_anniversaire", par_annee = FALSE))),
      champ_manquant = rf(sim_ag(forme_ag()[c("nom", "transition", "convention")])),
      champ_inconnu = rf(sim_ag(c(forme_ag(), list(segment = "x")))),
      transition = rf(sim_ag(forme_ag(transition = list(depart = "vivant", arrivee = "rachat")))),
      par_annee = rf(sim_ag(forme_ag(par_annee = NA))),
      classe = rf(sim_ag(structure(forme_ag(), class = "forme"))),
      data_frame = rf(sim_ag(data.frame(nom = "agregats"))))
  }, finally = suppressMessages(untrace("sous_graine", where = ns)))
  tirages_refus <- cpt$n
  graine_inchangee <- identical(.Random.seed, graine_temoin) && identical(graine_temoin, graine_avant)
  ko <- names(cas)[!vapply(cas, function(r) isTRUE(controle(r, "simulation", "forme_invalide")), NA)]
  msg <- function(k) cas[[k]]$message
  if (!identical(temoin, 1L)) sprintf("témoin : %d appel(s) de sous_graine() sans forme au lieu de 1", temoin)
  else if (!identical(tirages_refus, 0L)) sprintf("%d appel(s) de sous_graine() pendant les refus", tirages_refus)
  else if (!graine_inchangee) ".Random.seed de l'appelant modifié"
  else if (length(ko)) paste("sans le refus attendu :", paste(ko, collapse = ", "))
  else if (!grepl("defauts", msg("defauts"), fixed = TRUE) || !grepl("multi-contrats", msg("multi"), fixed = TRUE) ||
           !grepl("age_atteint_annee", msg("age_atteint_annee"), fixed = TRUE)) "message sans la raison nommée"
  else TRUE
})
# C11 : la table d'exposition des lignes (recette lignes_dates_completes,
# segmentation sexe, convention dernier_anniversaire, délai nul, période du
# jeu) et la projection du simulateur ont les mêmes cellules.
verifier("C11 (approximation numérique, tolérance relative 1e-10) : simulateur <-> exposition sur le jeu simulé, deux transitions, sans et avec année : mêmes cellules, D identical(), E^c et E^i à 1e-10 (écart maximal mesuré : 0)", {
  ecarts <- character(0)
  rel <- function(u, v) if (!length(u)) 0 else max(abs(u - v) / pmax(abs(v), .Machine$double.xmin))
  for (tr in list(VD, RA)) for (pa in c(FALSE, TRUE)) {
    a <- sim_ag(forme_ag(tr, par_annee = pa))
    pl <- plan_ag(list(brut = list(estimateurs = "poisson", niveau = 0.95)), transition = tr, par_annee = pa)
    el <- construire_table(JEU, ROLES, pl)$etapes$exposition$resultat$exposition
    cles <- c("sexe", "age", if (pa) "annee")
    lab <- sprintf("%s -> %s, par_annee = %s", tr$depart, tr$arrivee, pa)
    if (!identical(a[cles], el[cles])) { ecarts <- c(ecarts, paste(lab, ": cellules différentes")); next }
    if (!identical(a$evenements, el$evenements)) ecarts <- c(ecarts, paste(lab, ": D différents"))
    if (rel(a$exposition_centrale, el$exposition_centrale) > 1e-10) ecarts <- c(ecarts, paste(lab, ": E^c au-delà de 1e-10"))
    if (!pa && rel(a$exposition_initiale, el$exposition_centrale + el$complement_initial_cellule) > 1e-10)
      ecarts <- c(ecarts, paste(lab, ": E^i au-delà de 1e-10"))
    if (sum(a$evenements) < 50L) ecarts <- c(ecarts, paste(lab, ": moins de 50 événements (cas vide)"))
  }
  if (length(ecarts)) ecarts else TRUE
})
verifier("C10 : construction en agrégats sur la forme du simulateur = construction en lignes sur le jeu propre (poisson, binomial, positionnement, homogeneite, validation identical() ; rôles aux noms des colonnes)", {
  rs <- stats::setNames(names(AG_SIM), names(AG_SIM))
  x <- construire_table(AG_SIM, rs, PLAN_C1, tables_reference = list(tv = T_HF))
  identical(x$etapes$brut$resultat, XL$etapes$brut$resultat) &&
    identical(x$etapes$positionnement$resultat, XL$etapes$positionnement$resultat) &&
    !is.null(x$etapes$homogeneite) && identical(x$etapes$homogeneite$resultat, XL$etapes$homogeneite$resultat) &&
    !is.null(x$etapes$validation) && identical(x$etapes$validation$resultat, XL$etapes$validation$resultat)
})

# --- garde-fous internes ------------------------------------------------------------

verifier("importance_exclusions en appel direct sous une recette sans fenêtres -> erreur de programmation (refus utilisateur levé par la matrice ; ADR 0016, point 4.6)", {
  rec <- tablexp:::deduire_recette(R_EC)
  e <- tryCatch(tablexp:::calculer_importance_exclusions(AG2, R_EC, list(), list(), NULL, NULL, P_BV, rec, NULL,
                                                         character(0), 0.95), error = function(c) c)
  inherits(e, "error") && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
})
verifier("importance_exclusions en appel direct : ligne de matrice de l'étape non admise (recette des lignes, ligne mise à non) -> erreur de programmation, lue sur la matrice et non sur le nom (ADR 0016, points 4.4 et 4.6)", {
  rec <- tablexp:::deduire_recette(ROLES)
  k <- rec$matrice$etape == "importance_exclusions"
  rec$matrice$admis[k] <- "non"; rec$matrice$motif[k] <- "motif_test"
  e <- tryCatch(tablexp:::calculer_importance_exclusions(JEU, ROLES, list(), list(), NULL, NULL, P_BV, rec, NULL,
                                                         character(0), 0.95), error = function(c) c)
  inherits(e, "error") && !inherits(e, "refus_tablexp") &&
    grepl("non admise par la matrice de la recette (erreur de programmation)", conditionMessage(e), fixed = TRUE)
})
verifier("calculer_exposition() en appel direct : seam inconnu -> erreur de programmation (aiguillage, ADR 0016, point 4.1)", {
  rec <- tablexp:::deduire_recette(R_EC); rec$seam <- "autre"
  e <- tryCatch(tablexp:::calculer_exposition(data.frame(), P_BV, character(0), rec, NULL, FALSE), error = function(c) c)
  inherits(e, "error") && !inherits(e, "refus_tablexp") && grepl("erreur de programmation", conditionMessage(e), fixed = TRUE)
})
