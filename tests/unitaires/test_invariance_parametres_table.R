# -----------------------------------------------------------------------------
#  tests/unitaires/test_invariance_parametres_table.R : garde d'invariance,
#  aucune étape ne lit les paramètres d'une table de référence (issue #257 ;
#  B30 ; suite de #163, M279, option (a))
#
#  Affirmation gardée : la mention MENTIONS_RAPPORT$tables_reference_parametres
#  du rapport figé dit qu'« aucune étape de la construction ne lit ces
#  paramètres ». L'objet table_reference est transmis avec ses attributs aux
#  étapes `positionnement`, `validation` et `coherence_regularite`
#  (table_citee(), R/construire_table.R) ; une lecture par attributes() ou par
#  attr() en correspondance partielle échappe à une recherche textuelle.
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - identité exacte : constructions de deux tables de mêmes valeurs, dont
#    l'attribut `parametres` diffère (modifié après le constructeur), sous un
#    plan par étape qui lit une table : étapes, avertissements et empreinte de
#    la table identical() ;
#  - valeur calculée à la main : chemins de la fiche qui diffèrent (seuls
#    `tables_reference/<clé>/parametres` et `empreintes/resultat`, empreinte
#    du résultat entier, qui porte la fiche) ; liste fermée des sites de
#    lecture de l'attribut dans R/, repérés par fichier et fonction (pas de
#    numéro de ligne), chacun lu une seule fois ;
#  - témoins adverses : une étape qui lit `parametres` (fonction injectée par
#    trace()) fait échouer l'invariance ; un site non admis, ou un second
#    site dans une fonction admise, ajouté à une copie de R/ fait échouer la
#    recherche.
#  Étapes : celles de l'issue (positionnement, validation,
#  coherence_regularite), complétées par importance_exclusions, qui reçoit
#  aussi la table de la validation (table = table_validation) : sans elle au
#  plan, une lecture injectée dans cette étape passe inaperçue (mesuré par
#  actuary).
#  Critère 1 lu avec fiche/empreintes/resultat : l'empreinte du résultat
#  porte sur la fiche entière, donc sur `parametres`.
#  Limites de la recherche (appoint, l'invariance est la garde) : elle lit
#  l'arbre syntaxique de R/ ; elle relève attr() dont l'argument `which`
#  (nom exact ou partiel, ou position) est un littéral préfixe de
#  « parametres » ou n'est pas un littéral, et attributes(x)$p,
#  attributes(x)[["p"]] ou attributes(x)["p"] (p préfixe), en lecture : le
#  membre gauche d'une affectation (attr(x, "parametres") <- v) n'est pas
#  compté. Restent hors d'atteinte : do.call("attr", ...),
#  getElement(attributes(x), ...), un attributes(x) rangé dans une variable.
#  Graine 257 < 1000 : graine de garde, exemptée d'unicité (tests/controles/
#  graines.R, règle A5).
# -----------------------------------------------------------------------------

D <- as.Date
ns <- asNamespace("tablexp")
CONTROLES_ <- file.path(RACINE, "tests", "controles")
# Lecture d'un fichier R en UTF-8 quelle que soit la locale (#213) : fonctions
# gardées dans ce local(), hors de l'environnement du fichier de test.
analyser_ <- local({
  sys.source(file.path(CONTROLES_, "lecture_utf8.R"), envir = environment())
  analyser_fichier
})

# --- A. Invariance : deux tables de mêmes valeurs, paramètres différents --------

PERIODE <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
GM <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
JEU <- local({
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = GM, facteurs = c(H = 1, F = 0.6)),
              rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 3000, proportion_hommes = 0.5, age_min = 20, age_max = 75,
             origine = D("2005-01-01"))
  simuler_portefeuille(loi, pf, PERIODE, 257L)
})
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
DF_TABLE <- rbind(data.frame(age = GM$age, sexe = "H", qx = GM$qx),
                  data.frame(age = GM$age, sexe = "F", qx = 0.6 * GM$qx))
PARAM_A <- list(loi = "gompertz_makeham", A = 2e-4, B = 2.5e-5, c = 1.1, ages = c(0L, 120L))
# Deux variantes modifiées après le constructeur : valeurs et noms changés,
# longueur et types changés.
PARAM_B <- list(loi = "autre", A = 99, B = -1, c = 7, ages = c(5L, 50L), supplement = 1:3)
PARAM_C <- list(commentaire = "sans rapport")
table_synth <- function(nom, parametres) {
  t <- table_reference(DF_TABLE, nom, "synthetique", "essai #257", parametres = PARAM_A)
  attr(t, "parametres") <- parametres
  t
}
verifier("A témoin de construction : les trois tables ont les mêmes valeurs et des paramètres différents ; la modification après le constructeur est admise par le contrôle de forme (aucun refus)", {
  l <- lapply(list(PARAM_A, PARAM_B, PARAM_C), function(p) table_synth("gm synthetique", p))
  v <- lapply(l, function(t) unclass(t)[c("age", "sexe", "qx")])
  r <- tryCatch(tablexp:::controler_forme_tables(list(tv = l[[2L]], tw = l[[3L]])),
                refus_tablexp = function(r) r)
  identical(v[[1L]], v[[2L]]) && identical(v[[1L]], v[[3L]]) &&
    !identical(attr(l[[1L]], "parametres"), attr(l[[2L]], "parametres")) &&
    !identical(attr(l[[1L]], "parametres"), attr(l[[3L]], "parametres")) &&
    !inherits(r, "refus_tablexp")
})

plan_table <- function(...) {
  p <- list(transition = list(depart = "vivant", arrivee = "deces"), periode = PERIODE, delai_declaration = 0,
            segmentation = "sexe",
            etapes = c(list(donnees = list(exclusions = character(0)),
                            exposition = list(convention = "plus_fine", par_annee = FALSE)), list(...)))
  if ("validation" %in% names(p$etapes)) p$graine <- 257L
  p
}
POS <- list(methode = "smr", table_reference = "tv", plage_ages = c(20, 90), niveau = 0.95)
VAL <- list(table_reference = "tv", relation_table = "independante", plage_ages = c(20, 90),
            tranches_age = c(20, 50, 70), niveau = 0.95, replications = 19)
COH <- list(tables_reference = "tw", plage_ages = c(30, 70), ordres = c(1, 2), ordre_variable = "sexe",
            ordre_superieur = "H", ordre_inferieur = "F")
# Un plan par étape qui lit une table. Pour coherence_regularite, le
# positionnement cite `tv` (jamais modifiée) et l'étape cite `tw` : seule `tw`
# change entre les deux constructions, l'écart ne peut venir que d'elle.
CAS <- list(
  positionnement = list(plan = plan_table(positionnement = POS), cle = "tv", cles = "tv"),
  validation = list(plan = plan_table(validation = VAL), cle = "tv", cles = "tv"),
  importance_exclusions = list(plan = plan_table(validation = VAL, importance_exclusions = list(niveau = 0.95)),
                               cle = "tv", cles = "tv"),
  coherence_regularite = list(plan = plan_table(positionnement = POS, coherence_regularite = COH),
                              cle = "tw", cles = c("tv", "tw")))

# Chemins (séparés par « / ») des feuilles qui diffèrent entre deux listes
# nommées : une liste de mêmes noms se parcourt, toute autre valeur est une
# feuille comparée par identical().
chemins_ecart <- function(a, b, chemin = character(0)) {
  if (is.list(a) && is.list(b) && !is.null(names(a)) && identical(names(a), names(b)))
    return(as.character(unlist(lapply(names(a), function(n) chemins_ecart(a[[n]], b[[n]], c(chemin, n))),
                               use.names = FALSE)))
  if (identical(a, b)) character(0) else paste(chemin, collapse = "/")
}

# Construit avec la table `cle` portant PARAM_A (x), puis portant `parametres`
# (y), toutes les autres tables identiques. Rend TRUE, ou le texte de l'écart
# au critère : étapes, avertissements et empreintes de table identiques ; seuls
# fiche/tables_reference/<clé>/parametres et fiche/empreintes/resultat
# diffèrent, ce dernier seulement par la fiche (même valeur recalculée une
# fois le paramètre remis).
invariance <- function(cas, parametres) {
  tables <- function(p) {
    l <- lapply(cas$cles, function(k) table_synth(paste("gm synthetique", k), PARAM_A))
    names(l) <- cas$cles
    attr(l[[cas$cle]], "parametres") <- p
    l
  }
  x <- construire_table(JEU, ROLES, cas$plan, tables_reference = tables(PARAM_A))
  y <- construire_table(JEU, ROLES, cas$plan, tables_reference = tables(parametres))
  e <- character(0)
  if (!identical(y$etapes, x$etapes)) e <- c(e, "etapes differentes")
  if (!identical(y$avertissements, x$avertissements)) e <- c(e, "avertissements differents")
  emp <- function(z) lapply(z$fiche$tables_reference, function(t) t$empreinte)
  if (!identical(emp(y), emp(x))) e <- c(e, "empreintes de table differentes")
  ch <- chemins_ecart(x$fiche, y$fiche)
  attendu <- c("empreintes/resultat", paste0("tables_reference/", cas$cle, "/parametres"))
  if (!identical(sort(ch), sort(attendu)))
    e <- c(e, paste("chemins de fiche differents :", paste(ch, collapse = ", "), "(attendu :",
                    paste(attendu, collapse = ", "), ")"))
  y2 <- y
  y2$fiche$tables_reference[[cas$cle]]$parametres <- x$fiche$tables_reference[[cas$cle]]$parametres
  if (!identical(tablexp:::empreinte_resultat(y2), x$fiche$empreintes$resultat))
    e <- c(e, "empreinte du resultat differente a parametres remis")
  if (length(e)) paste(e, collapse = " ; ") else TRUE
}
for (id in names(CAS))
  for (v in list(list("PARAM_B", PARAM_B), list("PARAM_C", PARAM_C)))
    local({
      cas <- CAS[[id]]; nom <- v[[1L]]; p <- v[[2L]]
      verifier(sprintf("A %s, parametres %s : etapes, avertissements, empreintes de table identical() ; seuls tables_reference/%s/parametres et empreintes/resultat diffèrent",
                       id, nom, cas$cle),
               invariance(cas, p))
    })
verifier("A témoin de comptage : sans modification de parametres, aucun chemin de fiche ne diffère ; avec, le chemin de parametres de la table modifiée seulement, aucun autre ", {
  a <- list(f = list(p = 1, tables_reference = list(tv = list(parametres = 1, nom = "a"), tw = list(parametres = 2))))
  b <- a; b$f$tables_reference$tv$parametres <- list(1, 2)
  length(chemins_ecart(a, a)) == 0L && identical(chemins_ecart(a, b), "f/tables_reference/tv/parametres")
})

# --- B. Témoins adverses : une étape qui lit `parametres` ----------------------
# trace() à l'entrée de la fonction d'étape de l'espace de noms installé (comme
# tests/unitaires/test_tables_reference.R) : l'étape lit l'attribut `parametres`
# de la table qu'elle reçoit et en fait dépendre un de ses paramètres, donc son
# résultat. Un témoin par étape ; trace retirée à la sortie.
avec_lecture <- function(fonction, tracer, expr) {
  suppressMessages(trace(fonction, where = ns, tracer = tracer, print = FALSE))
  on.exit(suppressMessages(untrace(fonction, where = ns)))
  force(expr)
}
LECTURES <- list(
  positionnement = list(fonction = "calculer_positionnement",
                        tracer = quote({ k <- attr(table, "parametres", exact = TRUE)
                                         if (!is.null(k)) niveau <- niveau - 0.01 * (length(k) %% 3L) })),
  validation = list(fonction = "calculer_validation",
                    tracer = quote({ k <- attr(table, "parametres", exact = TRUE)
                                     if (!is.null(k)) niveau <- niveau - 0.01 * (length(k) %% 3L) })),
  importance_exclusions = list(fonction = "calculer_importance_exclusions",
                               tracer = quote({ k <- attr(table, "parametres", exact = TRUE)
                                                if (!is.null(k)) niveau <- niveau - 0.01 * (length(k) %% 3L) })),
  coherence_regularite = list(fonction = "calculer_coherence_regularite",
                              tracer = quote({ k <- attr(tables[[1L]], "parametres", exact = TRUE)
                                               if (!is.null(k)) ordres <- ordres + length(k) %% 3L })))
# Longueurs de parametres : PARAM_A 5, PARAM_B 6, PARAM_C 1 ; modulo 3 : 2, 0, 1 :
# chaque témoin sépare PARAM_A de PARAM_B et de PARAM_C.
for (id in names(LECTURES))
  local({
    cas <- CAS[[id]]; l <- LECTURES[[id]]
    verifier(sprintf("B témoin : l'étape %s lit parametres (fonction injectée) : l'invariance échoue sur les étapes, pour PARAM_B et PARAM_C ; la trace retirée, elle passe de nouveau", id), {
      r <- lapply(list(PARAM_B, PARAM_C), function(p) avec_lecture(l$fonction, l$tracer, invariance(cas, p)))
      apres <- invariance(cas, PARAM_B)
      if (!all(vapply(r, function(z) is.character(z) && grepl("etapes differentes", z, fixed = TRUE), NA)))
        paste("l'invariance ne constate pas l'ecart :", paste(unlist(r), collapse = " | "))
      else if (!isTRUE(apres)) paste("apres retrait de la trace :", apres)
      else TRUE
    })
  })

# --- C. Recherche des lectures de l'attribut dans R/ (appoint) -----------------

# Sites de lecture de l'attribut `parametres` dans les fichiers R de `dossier` :
# vecteur « fichier::fonction » (fonction de premier niveau qui contient
# l'expression), une entrée par lecture trouvée.
sites_parametres <- function(dossier) {
  acc <- character(0)
  prefixe <- function(s) is.character(s) && length(s) == 1L && !is.na(s) && nzchar(s) && startsWith("parametres", s)
  nom_appel <- function(e) {
    f <- e[[1L]]
    if (is.name(f)) as.character(f)
    else if (is.call(f) && length(f) == 3L && is.name(f[[1L]]) && as.character(f[[1L]]) %in% c("::", ":::"))
      as.character(f[[3L]])
    else ""
  }
  # Argument `which` de attr(x, which, exact) selon la correspondance des
  # arguments de R : noms exacts, puis partiels (pmatch), puis position pour
  # les formels restants ; NULL si absent.
  argument_which <- function(e) {
    a <- as.list(e)[-1L]
    n <- names(a); if (is.null(n)) n <- rep("", length(a))
    formels <- c("x", "which", "exact")
    m <- rep(NA_integer_, length(a))
    m[n != ""] <- pmatch(n[n != ""], formels, duplicates.ok = TRUE)
    if (2L %in% m) return(list(a[[match(2L, m)]]))
    libres <- formels[!(seq_along(formels) %in% m)]
    positionnels <- a[n == ""]
    i <- match("which", libres)
    if (!is.na(i) && length(positionnels) >= i) list(positionnels[[i]]) else NULL
  }
  est_site <- function(e) {
    f <- nom_appel(e)
    if (f == "attr") {
      w <- argument_which(e)
      if (is.null(w)) return(FALSE)
      w <- w[[1L]]
      if (is.character(w)) prefixe(w) else TRUE
    } else if (f %in% c("$", "[[", "[") && length(e) >= 3L && is.call(e[[2L]]) && nom_appel(e[[2L]]) == "attributes") {
      k <- e[[3L]]
      prefixe(if (is.name(k)) as.character(k) else k)
    } else FALSE
  }
  parcourir <- function(e, courant, fichier) {
    if (is.pairlist(e)) {
      for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) parcourir(e[[i]], courant, fichier)
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    # Membre gauche d'une affectation : une écriture, non comptée ; ses
    # sous-expressions (l'objet écrit, les indices) restent parcourues.
    gauche <- if (nom_appel(e) %in% c("<-", "=", "<<-") && length(e) == 3L && is.call(e[[2L]])) e[[2L]]
    if (!is.null(gauche)) {
      for (i in seq_along(gauche)) if (!identical(gauche[[i]], quote(expr = ))) parcourir(gauche[[i]], courant, fichier)
      parcourir(e[[3L]], courant, fichier)
      return(invisible())
    }
    if (est_site(e)) acc <<- c(acc, paste0(fichier, "::", courant))
    for (i in seq_along(e)) if (!identical(e[[i]], quote(expr = ))) parcourir(e[[i]], courant, fichier)
    invisible()
  }
  for (f in sort(list.files(dossier, pattern = "\\.[Rr]$", full.names = TRUE))) {
    fichier <- file.path(basename(dossier), basename(f))
    for (e in analyser_(f, keep.source = FALSE)) {
      courant <- if (is.call(e) && length(e) == 3L && as.character(e[[1L]])[1L] %in% c("<-", "=") && is.name(e[[2L]]))
        as.character(e[[2L]]) else "<hors fonction>"
      parcourir(e, courant, fichier)
    }
  }
  acc
}
SITES_ADMIS <- c("R/donnees.R::etat_table_reference", "R/rapport.R::element_table_fiche")
verifier("C R/ : les lectures de l'attribut parametres sont exactement les deux sites admis, chacune une seule fois (fichier::fonction ; reconstruction de etat_table_reference() (module donnees, #253 ; lue par controler_forme_tables() et loi_effective()), element_table_fiche())", {
  s <- sites_parametres(file.path(RACINE, "R"))
  if (identical(sort(s), SITES_ADMIS)) TRUE else paste("sites relevés :", paste(s, collapse = " ; "))
})

# Copie de R/donnees.R et R/rapport.R (sites admis) dans un dossier R/
# temporaire, avec les lignes de `ajout` en fichier supplémentaire ; `modifier`
# remplace, dans R/rapport.R, une ligne (motif fixe) par un texte.
copie_temoin <- function(ajout = character(0), modifier = NULL) {
  d <- file.path(tempfile("copie_R_"), "R"); dir.create(d, recursive = TRUE)
  for (f in c("donnees.R", "rapport.R")) file.copy(file.path(RACINE, "R", f), d)
  if (!is.null(modifier)) {
    ch <- file.path(d, "rapport.R")
    l <- readLines(ch, encoding = "UTF-8", warn = FALSE)
    i <- which(grepl(modifier[[1L]], l, fixed = TRUE))
    if (length(i) != 1L) stop("motif de modification : ", length(i), " ligne(s) au lieu d'une")
    l[i] <- sub(modifier[[1L]], modifier[[2L]], l[i], fixed = TRUE)
    writeLines(l, ch, useBytes = TRUE)
  }
  if (length(ajout)) writeLines(ajout, file.path(d, "zz_temoin.R"))
  d
}
verifier("C témoin de la copie : sans ajout, la recherche relève les deux sites admis seulement (la copie reproduit R/)", {
  d <- copie_temoin(); on.exit(unlink(dirname(d), recursive = TRUE))
  identical(sort(sites_parametres(d)), SITES_ADMIS)
})
CAS_SITES <- list(
  list("attr() exact", "lire_a <- function(t) attr(t, \"parametres\", exact = TRUE)"),
  list("attr() partiel, p", "lire_a <- function(t) attr(t, \"p\")"),
  list("attr() partiel, param", "lire_a <- function(t) attr(t, \"param\", exact = FALSE)"),
  list("attr() which nommé", "lire_a <- function(t) attr(x = t, which = \"par\")"),
  list("attr() which nommé partiellement", "lire_a <- function(t) attr(t, wh = \"parametres\")"),
  list("attr() x nommé partiellement, which positionnel", "lire_a <- function(t) attr(ex = TRUE, \"parametres\", x = t)"),
  list("base::attr()", "lire_a <- function(t) base::attr(t, \"parametres\")"),
  list("attr() which non littéral", "lire_a <- function(t, cle) attr(t, cle, exact = TRUE)"),
  list("attributes()$", "lire_a <- function(t) attributes(t)$parametres"),
  list("attributes()$ partiel", "lire_a <- function(t) attributes(t)$param"),
  list("attributes()[[ ]]", "lire_a <- function(t) attributes(t)[[\"parametres\"]]"),
  list("attributes()[ ]", "lire_a <- function(t) attributes(t)[\"parametres\"]"),
  list("lecture à droite d'une écriture", "lire_a <- function(t, u) { attr(t, \"parametres\") <- attr(u, \"parametres\"); t }"),
  list("dans une fonction imbriquée", "lire_a <- function(t) { f <- function() attr(t, \"parametres\"); f() }"),
  list("dans un paramètre par défaut", "lire_a <- function(t, p = attr(t, \"parametres\")) p"))
for (cas in CAS_SITES)
  local({
    nom <- cas[[1L]]; code <- cas[[2L]]
    verifier(sprintf("C témoin : un site non admis ajouté à une copie (%s) est relevé, et lui seul", nom), {
      d <- copie_temoin(code); on.exit(unlink(dirname(d), recursive = TRUE))
      s <- sites_parametres(d)
      identical(sort(s), sort(c(SITES_ADMIS, "R/zz_temoin.R::lire_a")))
    })
  })
verifier("C témoin : un second site dans une fonction admise (element_table_fiche) fait différer la liste (une lecture par site)", {
  d <- copie_temoin(modifier = list("source = attr(t, \"source\", exact = TRUE),",
                                    "source = attr(t, \"parametres\", exact = TRUE),"))
  on.exit(unlink(dirname(d), recursive = TRUE))
  s <- sites_parametres(d)
  identical(sort(s), sort(c(SITES_ADMIS, "R/rapport.R::element_table_fiche"))) && !identical(sort(s), SITES_ADMIS)
})
verifier("C contre-épreuve : ni les autres attributs de table (nom, statut, source, forme_origine, class, names), ni un élément de liste nommé parametres, ni un commentaire, ni attributes() sans indice, ni une écriture (membre gauche d'une affectation) ne sont relevés", {
  d <- copie_temoin(c(
    "autres <- function(t) list(attr(t, \"nom\", exact = TRUE), attr(t, \"statut\"), attr(t, \"source\", exact = TRUE),",
    "                           attr(t, \"forme_origine\"), attr(t, \"class\"), attr(t, \"names\"), attributes(t)$names)",
    "liste <- function(e) list(e$parametres, e[[\"parametres\"]], attributes(e), list(parametres = 1))",
    "# attr(t, \"parametres\", exact = TRUE)",
    "ecrit <- function(t) { texte <- \"attr(t, 'parametres')\"; texte }",
    "ecriture <- function(t, p) { attr(t, \"parametres\") <- p; attr(t, wh = \"param\") <- p; attributes(t)$parametres <- p; t }"))
  on.exit(unlink(dirname(d), recursive = TRUE))
  identical(sort(sites_parametres(d)), SITES_ADMIS)
})
