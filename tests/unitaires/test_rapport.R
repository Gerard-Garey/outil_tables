# -----------------------------------------------------------------------------
#  tests/unitaires/test_rapport.R : rapport figé HTML ecrire_rapport() (issue
#  #87 ; ADR 0012, point 2, et seconde annotation du 24/10/2026, (b) ;
#  spécification d'actuary du 24/10/2026, tests U1 à U23 ; U24 et U25 dans
#  tests/test_reproductibilite.R, C1 dans tests/controles/locale_c.R)
#
#  Natures des attendus (ADR 0007, point 11.4), nommées dans chaque test :
#  - valeur calculée à la main : forme du document (en-tête, balises,
#    attributs, sections et leur ordre, textes fixes), motifs, modules et
#    details des refus, liste des noms interdits du contrôle statique ;
#  - identité exacte : octets de deux rendus, de deux constructions identiques
#    hors `execution`, sous d'autres options et une autre locale, après
#    saveRDS()/readRDS() ; objet inchangé par le rendu ; lignes de la table de
#    lignes égales aux lignes du corps de la fiche écrite ; cellules décimales
#    égales à leur écriture %.17g.
#
#  Jeu réduit : 3 000 têtes simulées sous la loi de jeu_reference()
#  (tests/reference/outils_reference.R), plans de même forme que R1 à R6, et un
#  plan réduit à l'étape donnees.
# -----------------------------------------------------------------------------

D <- as.Date
refus_de <- function(expr) tryCatch({ force(expr); NULL }, refus_tablexp = function(r) r)
texte_de <- function(o) { s <- rawToChar(o); Encoding(s) <- "UTF-8"; s }
lignes_de <- function(o) { l <- strsplit(rawToChar(o), "\n", fixed = TRUE)[[1L]]; Encoding(l) <- "UTF-8"; l }
# Échappement HTML du test, écrit à la main (indépendant d'echapper_html()).
html <- function(s) {
  for (r in list(c("&", "&amp;"), c("<", "&lt;"), c(">", "&gt;"), c("\"", "&quot;"))) s <- gsub(r[1L], r[2L], s, fixed = TRUE)
  s
}
# Lignes de la section `id` du document `l` (lignes), balises de section comprises.
section_de <- function(l, id) {
  d <- match(paste0("<section id=\"", id, "\">"), l)
  if (is.na(d)) return(character(0))
  f <- d - 1L + match("</section>", l[d:length(l)])
  l[d:f]
}

PERIODE <- list(debut = D("2015-01-01"), fin = D("2024-12-31"))
JEU <- local({
  gm <- tablexp:::table_gompertz_makeham(2e-4, 2.5e-5, 1.1, 0:120)
  ra <- tablexp:::table_constante(0.05, 0:120, "rachat constant 5 %")
  loi <- list(deces = list(table = gm, facteurs = c(H = 1, F = 0.6)),
              rachat = list(table = ra, facteurs = c(H = 1, F = 1)))
  pf <- list(mode = "uniforme", n = 3000, proportion_hommes = 0.5, age_min = 20, age_max = 75,
             origine = D("2005-01-01"))
  simuler_portefeuille(loi, pf, PERIODE, 20261016L)
})
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete", sexe = "sexe", date_naissance = "date_naissance",
           date_entree = "date_entree", date_sortie = "date_sortie", cause_sortie = "cause_sortie")
plan_forme <- function(transition = c("vivant", "deces"), segmentation = "sexe", delai = 0,
                       convention = "plus_fine", par_annee = FALSE, estimateurs = c("poisson", "binomial"),
                       niveau = 0.95)
  list(transition = list(depart = transition[1L], arrivee = transition[2L]), periode = PERIODE,
       delai_declaration = delai, segmentation = segmentation,
       etapes = list(donnees = list(exclusions = character(0)),
                     exposition = list(convention = convention, par_annee = par_annee),
                     brut = list(estimateurs = estimateurs, niveau = niveau)))
PLANS <- list(
  R1 = plan_forme(),
  R2 = plan_forme(segmentation = character(0), delai = 90, niveau = 0.99),
  R3 = plan_forme(convention = "age_atteint_annee"),
  R4 = plan_forme(par_annee = TRUE, estimateurs = "poisson"),
  R5 = plan_forme(transition = c("en_cours", "rachat")),
  R6 = plan_forme(estimateurs = c("poisson", "binomial", "kaplan_meier")),
  donnees_seule = { p <- plan_forme(); p$etapes <- p$etapes["donnees"]; p })
X <- lapply(PLANS, function(p) construire_table(JEU, ROLES, p))
O <- lapply(X, ecrire_rapport)
L <- lapply(O, lignes_de)
x1 <- X$R1
o1 <- O$R1

# --- U1 : forme des octets (valeur calculée à la main) -------------------------

verifier("U1 : raw, UTF-8 valide, ni BOM ni CR, LF final, <!DOCTYPE html> en tête, <meta charset=\"utf-8\">", {
  e <- character(0)
  for (id in names(O)) {
    o <- O[[id]]
    if (!is.raw(o) || !length(o)) { e <- c(e, paste(id, ": pas un raw non vide")); next }
    if (!validUTF8(rawToChar(o))) e <- c(e, paste(id, ": UTF-8 invalide"))
    if (identical(o[1:3], as.raw(c(0xef, 0xbb, 0xbf)))) e <- c(e, paste(id, ": BOM"))
    if (any(o == as.raw(0x0d))) e <- c(e, paste(id, ": CR"))
    if (o[length(o)] != as.raw(0x0a)) e <- c(e, paste(id, ": sans LF final"))
    if (!identical(o[1:16], charToRaw("<!DOCTYPE html>\n"))) e <- c(e, paste(id, ": en-tête"))
    if (!"<meta charset=\"utf-8\">" %in% L[[id]]) e <- c(e, paste(id, ": meta charset"))
  }
  if (length(e)) e else TRUE
})

# --- U2 à U5, U9, U23 : déterminisme (identité exacte) -------------------------

verifier("U2 : deux rendus du même objet identical()", identical(ecrire_rapport(x1), o1))
verifier("U3 : deux constructions identiques hors execution (execution forcée différente) : octets identical()", {
  y <- construire_table(JEU, ROLES, PLANS$R1)
  y$execution <- list(debut = "autre", duree_secondes = -1)
  identical(ecrire_rapport(y), o1)
})
verifier("U4 : octets identiques sous options(OutDec = \",\", scipen = 100, digits = 3) et (OutDec = \",\", scipen = -100, digits = 22) ; options restaurées", {
  avant <- options(OutDec = ",", scipen = 100, digits = 3)
  a <- tryCatch(ecrire_rapport(x1), finally = options(avant))
  avant <- options(OutDec = ",", scipen = -100, digits = 22)
  b <- tryCatch(ecrire_rapport(x1), finally = options(avant))
  identical(a, o1) && identical(b, o1) && identical(getOption("OutDec"), ".")
})
verifier("U5 : octets identiques après Sys.setlocale(LC_CTYPE et LC_COLLATE = C) en cours de session ; locale restaurée", {
  ct <- Sys.getlocale("LC_CTYPE"); co <- Sys.getlocale("LC_COLLATE")
  a <- tryCatch({ Sys.setlocale("LC_CTYPE", "C"); Sys.setlocale("LC_COLLATE", "C"); ecrire_rapport(X$R6) },
                finally = { Sys.setlocale("LC_CTYPE", ct); Sys.setlocale("LC_COLLATE", co) })
  identical(a, O$R6) && identical(Sys.getlocale("LC_CTYPE"), ct)
})
verifier("U9 : objet sauvegardé puis rechargé (saveRDS, readRDS) : rendu sans refus, octets identical()", {
  f <- tempfile(fileext = ".rds"); on.exit(unlink(f))
  saveRDS(x1, f)
  identical(ecrire_rapport(readRDS(f)), o1)
})
verifier("U23 : objet identical() avant et après le rendu", {
  copie <- x1
  ecrire_rapport(x1)
  identical(x1, copie)
})

# --- U6 à U8, U10 : refus (valeur calculée à la main) --------------------------

# Refus attendu : module, motif, details (si donnés) et fragments du message.
controle_refus <- function(r, motif, details = NULL, contient = character(0)) {
  if (is.null(r)) return("aucun refus levé")
  e <- character(0)
  if (!identical(r$module, "rapport")) e <- c(e, sprintf("module « %s »", r$module))
  if (!identical(r$motif, motif)) e <- c(e, sprintf("motif « %s » au lieu de « %s »", r$motif, motif))
  for (n in names(details))
    if (!identical(r$details[[n]], details[[n]])) e <- c(e, sprintf("details$%s inattendu", n))
  for (s in contient) if (!grepl(s, conditionMessage(r), fixed = TRUE)) e <- c(e, sprintf("message sans « %s »", s))
  if (length(e)) e else TRUE
}
# Objet altéré par f, empreinte consignée recalculée (le contrôle 3 passe).
reconsigne <- function(x, f) {
  x <- f(x)
  x$fiche$empreintes$resultat <- tablexp:::empreinte_resultat(x)
  x
}

for (cas in list(list("unclass(x)", unclass(x1), "list"), list("list()", list(), "list"), list("NULL", NULL, "NULL"),
                 list("1", 1, "numeric"),
                 list("classe c(construction, x)", structure(list(), class = c("construction", "x")),
                      c("construction", "x"))))
  verifier(sprintf("U6 : refus 1, objet %s : rapport_objet_invalide, details lu et attendu", cas[[1L]]),
           controle_refus(refus_de(ecrire_rapport(cas[[2L]])), "rapport_objet_invalide",
                          list(lu = cas[[3L]], attendu = "construction")))
verifier("U6 : refus 1, vecteur de classe construction : rapport_objet_invalide, message « objet de type double au lieu d'une liste de classe « construction » »", {
  controle_refus(refus_de(ecrire_rapport(structure(1, class = "construction"))), "rapport_objet_invalide",
                 list(lu = "construction", attendu = "construction"),
                 paste("Rapport figé : objet de type double au lieu d'une liste de classe « construction » (objet rendu par",
                       "construire_table(), ou élément construction du résultat de rejouer_fiche())."))
})
verifier("U6 : refus 1 b, fiche qui vaut \"a\" : rapport_objet_invalide, details chemin, lu (valeur brute), attendu", {
  y <- x1; y$fiche <- "a"
  controle_refus(refus_de(ecrire_rapport(y)), "rapport_objet_invalide",
                 list(chemin = "fiche", lu = "a", attendu = "list"), "\"a\" (character)")
})

# Classes citées par neutraliser_citation() (reprise de #94) : refus 1 (classe
# de l'objet) et refus 1 b (repli de decrire_valeur()) ; classe « a<VT>b »
# citée <U+000B>, classe d'encodage bytes citée sous le repère (auparavant :
# erreur R « translating strings with "bytes" encoding is not allowed ») ;
# details : valeur exacte ; message sur une ligne, marqué UTF-8.
une_ligne_utf8 <- function(r, ok) {
  if (!isTRUE(ok)) ok
  else if (grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes"
  else if (!identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8" else TRUE
}
CLASSE_BYTES <- local({ b <- "ab\xff"; Encoding(b) <- "bytes"; b })
for (k in list(list("a<VT>b", "a\u000bb", "a<U+000B>b"), list("d'encodage bytes", CLASSE_BYTES, "<chaîne d'encodage bytes>"))) {
  verifier(sprintf("U6 : refus 1, objet de classe %s : rapport_objet_invalide (pas une erreur R), classe citée « %s », une ligne, UTF-8",
                   k[[1L]], k[[3L]]), {
    r <- tryCatch(ecrire_rapport(structure(list(), class = k[[2L]])), error = function(e) e)
    if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
    une_ligne_utf8(r, controle_refus(r, "rapport_objet_invalide", list(lu = k[[2L]], attendu = "construction"),
                                     paste0("objet de classe « ", k[[3L]], " » au lieu de « construction »")))
  })
  verifier(sprintf("U6 : refus 1 b, fiche structure(1, class = %s) : repli de decrire_valeur(), classe citée « %s », une ligne, UTF-8",
                   k[[1L]], k[[3L]]), {
    y <- x1; y$fiche <- structure(1, class = k[[2L]])
    r <- tryCatch(ecrire_rapport(y), error = function(e) e)
    if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
    une_ligne_utf8(r, controle_refus(r, "rapport_objet_invalide", list(chemin = "fiche", lu = y$fiche, attendu = "list"),
                                     paste0("dont la fiche est objet de type double, de classe ", k[[3L]], " au lieu d'une liste.")))
  })
}

verifier("U7 : refus 2, version_fiche 3L : version_fiche_non_admise (pas objet_modifie), message « 3 (integer) » et « 4 (integer) »", {
  y <- x1; y$fiche$version_fiche <- 3L
  controle_refus(refus_de(ecrire_rapport(y)), "version_fiche_non_admise", list(lu = 3L, attendu = 4L),
                 c("3 (integer)", "4 (integer)", tablexp:::EMPREINTE_CODE))
})
verifier("U7 : refus 2, version_fiche 4 (double) : version_fiche_non_admise, message « 4 (double) »", {
  y <- x1; y$fiche$version_fiche <- 4
  controle_refus(refus_de(ecrire_rapport(y)), "version_fiche_non_admise", list(lu = 4, attendu = 4L),
                 c("4 (double)", "4 (integer)"))
})
verifier("U7 : refus 2, versions$tablexp « a<LF>b » : valeur décrite par l'écrivain, message sur une seule ligne", {
  y <- x1; y$fiche$version_fiche <- 3L; y$fiche$versions$tablexp <- "a\nb"
  r <- refus_de(ecrire_rapport(y))
  ok <- controle_refus(r, "version_fiche_non_admise", list(lu = 3L, attendu = 4L), "\"a\\nb\" (character)")
  if (isTRUE(ok) && grepl("\n", conditionMessage(r), fixed = TRUE)) "message sur plusieurs lignes" else ok
})
# NEL et U+2028 : décrits par l'écrivain, puis neutralisés <U+XXXX> (règle
# unique neutraliser_citation(), #94) ; aucun n'est écrit en clair.
verifier("U7 : refus 2, versions$tablexp « a<U+0085>b » (NEL) : décrite par l'écrivain, NEL cité <U+0085>, pas en clair", {
  y <- x1; y$fiche$version_fiche <- 3L; y$fiche$versions$tablexp <- "a\u0085b"
  r <- refus_de(ecrire_rapport(y))
  ok <- controle_refus(r, "version_fiche_non_admise", list(lu = 3L, attendu = 4L), "\"a<U+0085>b\" (character)")
  if (isTRUE(ok) && grepl("\u0085", conditionMessage(r), fixed = TRUE)) "NEL écrit en clair" else ok
})
verifier("U7 : refus 2, versions$tablexp « a<U+2028>b » : décrite par l'écrivain, U+2028 cité <U+2028>, pas en clair", {
  y <- x1; y$fiche$version_fiche <- 3L; y$fiche$versions$tablexp <- "a\u2028b"
  r <- refus_de(ecrire_rapport(y))
  ok <- controle_refus(r, "version_fiche_non_admise", list(lu = 3L), "\"a<U+2028>b\" (character)")
  if (isTRUE(ok) && grepl("\u2028", conditionMessage(r), fixed = TRUE)) "U+2028 écrit en clair" else ok
})
verifier("U7 : refus 2, versions$tablexp « a<VT>b » et « a<FF>b » : décrites par l'écrivain, citées <U+000B> et <U+000C> (#94)", {
  e <- character(0)
  for (k in list(c("\u000b", "<U+000B>"), c("\u000c", "<U+000C>"))) {
    y <- x1; y$fiche$version_fiche <- 3L; y$fiche$versions$tablexp <- paste0("a", k[1L], "b")
    r <- refus_de(ecrire_rapport(y))
    ok <- controle_refus(r, "version_fiche_non_admise", list(lu = 3L), paste0("\"a", k[2L], "b\" (character)"))
    if (!isTRUE(ok)) e <- c(e, ok) else if (grepl(k[1L], conditionMessage(r), fixed = TRUE)) e <- c(e, "caractère écrit en clair")
  }
  if (length(e)) e else TRUE
})
verifier("U7 : valeur_message() et decrire_valeur() : même règle (neutraliser_citation()) ; \\n de la grammaire inchangé ; octets non UTF-8 décrits par le type (#94)", {
  vm <- tablexp:::valeur_message; dv <- tablexp:::decrire_valeur
  inv <- rawToChar(as.raw(c(0x61, 0xe9)))
  identical(vm("a\u000bb"), "\"a<U+000B>b\" (character)") && identical(dv("a b"), "\"a<U+2029>b\" (character)") &&
    identical(vm("a\nb"), "\"a\\nb\" (character)") && identical(vm("a b"), "a b") &&
    identical(vm(inv), "objet de type character, de classe character")
})
verifier("U8 : refus 3, empreinte non recalculable : message de l'erreur cité par neutraliser_citation(), une ligne, marqué UTF-8 (#94)", {
  y <- x1; attr(y$etapes, "truc") <- 1
  r <- refus_de(ecrire_rapport(y))
  ok <- controle_refus(r, "objet_modifie", NULL, "empreinte du résultat non recalculable (")
  if (isTRUE(ok) && !identical(Encoding(conditionMessage(r)), "UTF-8")) "message non marqué UTF-8"
  else if (isTRUE(ok) && grepl("[\n\v\f]", conditionMessage(r))) "message sur plusieurs lignes" else ok
})
verifier("U7 : refus 2, versions$tablexp « é » : écrite en clair (caractère non ASCII ordinaire)", {
  y <- x1; y$fiche$version_fiche <- 3L; y$fiche$versions$tablexp <- "\u00e9"
  controle_refus(refus_de(ecrire_rapport(y)), "version_fiche_non_admise", list(lu = 3L), "tablexp \u00e9, empreinte")
})
verifier("U7 : refus 2, version_fiche retirée : version_fiche_non_admise", {
  y <- x1; y$fiche$version_fiche <- NULL
  controle_refus(refus_de(ecrire_rapport(y)), "version_fiche_non_admise", list(attendu = 4L), "4 (integer)")
})

verifier("U8 : refus 3, taux altéré d'un ulp (octet de poids faible) : objet_modifie, message avec les deux empreintes", {
  y <- x1
  t <- y$etapes$brut$resultat$poisson$taux
  i <- which(is.finite(t) & t > 0)[1L]
  b <- writeBin(t[i], raw(), size = 8L, endian = "little")
  b[1L] <- as.raw(bitwXor(as.integer(b[1L]), 1L))
  t[i] <- readBin(b, "double", size = 8L, endian = "little")
  y$etapes$brut$resultat$poisson$taux <- t
  a <- tablexp:::empreinte_resultat(y)
  controle_refus(refus_de(ecrire_rapport(y)), "objet_modifie", list(lu = x1$fiche$empreintes$resultat, attendu = a),
                 c(a, x1$fiche$empreintes$resultat))
})
verifier("U8 : refus 3, attribut ajouté à un data.frame : objet_modifie (refus, non erreur de programmation), attendu NA", {
  y <- x1; attr(y$etapes$brut$resultat$poisson, "foo") <- 1
  controle_refus(refus_de(ecrire_rapport(y)), "objet_modifie", list(lu = x1$fiche$empreintes$resultat, attendu = NA_character_),
                 c("non recalculable", x1$fiche$empreintes$resultat))
})
verifier("U8 : refus 3, empreinte consignée NA : objet_modifie, message avec les deux valeurs", {
  y <- x1; y$fiche$empreintes$resultat <- NA_character_
  controle_refus(refus_de(ecrire_rapport(y)), "objet_modifie", list(lu = NA_character_, attendu = x1$fiche$empreintes$resultat),
                 c(x1$fiche$empreintes$resultat, "NA (character)"))
})
verifier("U8 : refus 3, empreinte consignée autre MD5 : objet_modifie, message avec les deux valeurs", {
  y <- x1; y$fiche$empreintes$resultat <- "0123456789abcdef0123456789abcdef"
  controle_refus(refus_de(ecrire_rapport(y)), "objet_modifie", list(attendu = x1$fiche$empreintes$resultat),
                 c(x1$fiche$empreintes$resultat, "0123456789abcdef0123456789abcdef"))
})

for (cas in list(list("etapes$validation <- list()", function(x) { x$etapes$validation <- list(); x }, "etapes/validation"),
                 list("table <- data.frame(a = 1)", function(x) { x$table <- data.frame(a = 1); x }, "table"),
                 list("fiche$nouveau <- 1", function(x) { x$fiche$nouveau <- 1; x }, "fiche/nouveau"),
                 list("autre <- 1", function(x) { x$autre <- 1; x }, "autre")))
  verifier(sprintf("U10 : refus 4, %s (empreinte reconsignée) : rapport_objet_invalide, details$chemin « %s »",
                   cas[[1L]], cas[[3L]]),
           controle_refus(refus_de(ecrire_rapport(reconsigne(x1, cas[[2L]]))), "rapport_objet_invalide",
                          list(chemin = cas[[3L]]), cas[[3L]]))

# Chemin cité par neutraliser_citation() dans le refus 4 (reprise de #94) :
# élément « a<VT>b » hors des sections, à la racine et sous etapes, cité
# <U+000B> ; details$chemin exact. Un nom d'encodage bytes n'atteint pas le
# refus 4 : l'empreinte n'est pas recalculable (refus 3, objet_modifie),
# refus nommé, pas une erreur R.
for (cas in list(list("racine", function(x) x, ""), list("etapes", function(x) x$etapes, "etapes/"))) {
  verifier(sprintf("U10 : refus 4, élément « a<VT>b » (%s, empreinte reconsignée) : chemin cité <U+000B>, une ligne, UTF-8", cas[[1L]]), {
    y <- x1
    if (cas[[1L]] == "racine") y[["a\u000bb"]] <- 1 else y$etapes[["a\u000bb"]] <- list()
    r <- refus_de(ecrire_rapport(reconsigne(y, identity)))
    une_ligne_utf8(r, controle_refus(r, "rapport_objet_invalide", list(chemin = paste0(cas[[3L]], "a\u000bb")),
                                     paste0("élément « ", cas[[3L]], "a<U+000B>b » de l'objet non prévu")))
  })
  verifier(sprintf("U10 : élément de nom d'encodage bytes (%s) : refus objet_modifie (pas une erreur R), une ligne, UTF-8", cas[[1L]]), {
    y <- x1
    if (cas[[1L]] == "racine") {
      y$autre <- 1; n <- names(y); n[length(n)] <- CLASSE_BYTES; names(y) <- n
    } else {
      y$etapes$autre <- list(); n <- names(y$etapes); n[length(n)] <- CLASSE_BYTES; names(y$etapes) <- n
    }
    r <- tryCatch(ecrire_rapport(y), error = function(e) e)
    if (!inherits(r, "refus_tablexp")) return(sprintf("condition %s : %s", class(r)[1L], conditionMessage(r)))
    une_ligne_utf8(r, controle_refus(r, "objet_modifie", NULL, "empreinte du résultat non recalculable ("))
  })
}

# --- U11, U12 : ressources, balises, attributs, entités (valeur calculée à la main) ---

BALISES <- c("!DOCTYPE", "html", "head", "meta", "title", "style", "body", "h1", "h2", "nav", "ol", "ul", "li", "a",
             "section", "p", "table", "caption", "thead", "tbody", "tr", "th", "td", "code", "br")
verifier("U11 : aucune ressource externe ; chaque href de forme #[a-z_]+ et de cible id existante", {
  e <- character(0)
  for (id in names(O)) {
    s <- texte_de(O[[id]])
    for (m in c("<link", "<script", "<img", "<iframe", "<object", "<embed", "@import", "url(", "src=", "://", "@font-face"))
      if (grepl(m, s, fixed = TRUE)) e <- c(e, sprintf("%s : « %s »", id, m))
    h <- regmatches(s, gregexpr("href=\"[^\"]*\"", s))[[1L]]
    cibles <- regmatches(s, gregexpr("id=\"[^\"]*\"", s))[[1L]]
    if (!length(h) || !all(grepl("^href=\"#[a-z_]+\"$", h))) e <- c(e, paste(id, ": href hors forme"))
    if (!all(sub("^href=\"#", "id=\"", h) %in% cibles)) e <- c(e, paste(id, ": href sans cible"))
  }
  if (length(e)) e else TRUE
})
verifier("U12 : chaque < ouvre une balise de la liste fermée (attributs lang, charset, id, href seuls) ; chaque & ouvre &amp; &lt; &gt; &quot;", {
  e <- character(0)
  # Ligne par ligne : regmatches() sur le document d'un seul tenant est
  # quadratique (indices de caractères UTF-8), mesuré à 56 s.
  compte <- function(l, motif, fixed = FALSE) sum(lengths(regmatches(l, gregexpr(motif, l, fixed = fixed))))
  for (id in names(L)) {
    s <- L[[id]]
    b <- unlist(regmatches(s, gregexpr("<[^>]*>", s)))
    if (compte(s, "<", TRUE) != length(b)) e <- c(e, paste(id, ": < hors balise"))
    noms <- sub("^</?([!A-Za-z0-9]+).*$", "\\1", b)
    if (!all(noms %in% BALISES)) e <- c(e, paste(id, ": balises", paste(unique(setdiff(noms, BALISES)), collapse = ", ")))
    attributs <- unlist(regmatches(b, gregexpr(" [a-z-]+=", b)))
    if (!all(attributs %in% c(" lang=", " charset=", " id=", " href="))) e <- c(e, paste(id, ": attribut hors liste"))
    if (compte(s, "&", TRUE) != compte(s, "&(amp|lt|gt|quot);")) e <- c(e, paste(id, ": & hors entité"))
  }
  if (length(e)) e else TRUE
})

# --- U13 : échappement d'une modalité et d'un nom de colonne <>&" --------------

verifier("U13 : segment aux modalités a<b et c&\"d\">, colonne seg<>&\"x : valeurs échappées, aucun a<b brut", {
  j <- JEU[1:600, ]
  j[["seg<>&\"x"]] <- rep(c("a<b", "c&\"d\">"), length.out = nrow(j))
  r <- c(ROLES, segment = "seg<>&\"x")
  p <- plan_forme(segmentation = "segment")
  s <- texte_de(ecrire_rapport(construire_table(j, r, p)))
  e <- character(0)
  for (m in c("&quot;a&lt;b&quot;", "&quot;c&amp;\\&quot;d\\&quot;&gt;&quot;", "&quot;seg&lt;&gt;&amp;\\&quot;x&quot;"))
    if (!grepl(m, s, fixed = TRUE)) e <- c(e, sprintf("absent : %s", m))
  if (grepl("a<b", s, fixed = TRUE)) e <- c(e, "a<b brut présent")
  if (length(e)) e else TRUE
})

# --- U14 à U17 : sections (valeur calculée à la main) ---------------------------

S <- tablexp:::SECTIONS_RAPPORT
verifier("U14 : <section id> de chaque section, une fois, dans l'ordre de SECTIONS_RAPPORT ; <nav> dans le même ordre", {
  e <- character(0)
  for (id in names(L)) {
    l <- L[[id]]
    sec <- sub("^<section id=\"([a-z_]+)\">$", "\\1", grep("^<section id=", l, value = TRUE))
    nav <- sub("^<li><a href=\"#([a-z_]+)\">.*$", "\\1", grep("^<li><a href=\"#", l, value = TRUE))
    if (!identical(sec, S$section)) e <- c(e, paste(id, ": sections"))
    if (!identical(nav, S$section)) e <- c(e, paste(id, ": nav"))
  }
  if (length(e)) e else TRUE
})
verifier("U15 : sections etape = ETAPES$etape (même ordre) ; aucune racine non_produite sous etapes/<e> du catalogue ; identifiants uniques ^[a-z_]+$ ; nature dans la liste fermée", {
  et <- tablexp:::ETAPES$etape
  np <- unlist(S$racines[S$nature == "non_produite"])
  identical(S$etape[S$nature == "etape"], et) &&
    !any(vapply(et, function(e) any(np == paste0("etapes/", e) | startsWith(np, paste0("etapes/", e, "/"))), NA)) &&
    !anyDuplicated(S$section) && all(grepl("^[a-z_]+$", S$section)) &&
    all(S$nature %in% c("texte", "objet", "etape", "renvoi", "non_produite"))
})
verifier("U16 : couverture : chaque élément de l'objet (hors execution) couvert par exactement une racine, sur R1 à R6 et « donnees seule » ; racines fiche/* = ORDRE_CHAMPS_FICHE", {
  r <- unlist(S$racines)
  e <- character(0)
  for (id in names(X)) {
    x <- X[[id]]
    chemins <- c(setdiff(names(x), c("execution", "etapes", "fiche")), paste0("etapes/", names(x$etapes)),
                 paste0("fiche/", names(x$fiche)))
    n <- vapply(chemins, function(p) sum(r == p | startsWith(p, paste0(r, "/"))), 0L)
    if (any(n != 1L)) e <- c(e, sprintf("%s : %s", id, paste(chemins[n != 1L], collapse = ", ")))
  }
  f <- sub("^fiche/", "", grep("^fiche/", r, value = TRUE))
  if (!setequal(f, tablexp:::ORDRE_CHAMPS_FICHE) || anyDuplicated(f)) e <- c(e, "racines fiche/* différentes de ORDRE_CHAMPS_FICHE")
  if (length(e)) e else TRUE
})
verifier("U17 : « donnees seule » : « non demandée par le plan » dans etape_exposition, etape_brut et suffisance ; fiche/hypotheses_brut « absent de l'objet »", {
  l <- L$donnees_seule
  all(vapply(c("etape_exposition", "etape_brut", "suffisance"),
             function(id) any(grepl("non demandée par le plan", section_de(l, id), fixed = TRUE)), NA)) &&
    "<tr><td>fiche/hypotheses_brut</td><td></td><td></td><td>absent de l'objet</td></tr>" %in% section_de(l, "fiche_hypotheses") &&
    any(grepl("chemin etapes/exposition absent de l'objet", section_de(l, "etape_exposition"), fixed = TRUE))
})

# --- tables de colonnes : cellules égales aux valeurs de l'objet (identité exacte ; audit, reprise de #87) ---

# Pour chaque légende <caption><code>chemin</code>, les nrow lignes <tr> qui
# suivent <tbody>, découpées en cellules, comparées matrice contre matrice à
# l'écriture de chaque colonne de l'objet par l'écrivain, échappée.
verifier("tables de colonnes : chaque cellule égale à l'écriture échappée de la valeur de l'objet, même ligne et même colonne (data.frame des étapes et de la fiche)", {
  e <- character(0)
  n_tables <- 0L
  for (id in names(X)) {
    l <- L[[id]]
    k <- grep("^<caption><code>", l)
    for (i in k) {
      chemin <- sub("^<caption><code>([^<]*)</code>.*$", "\\1", l[i])
      v <- unclass(X[[id]])
      for (s_ in strsplit(chemin, "/", fixed = TRUE)[[1L]]) v <- v[[s_]]
      if (!is.data.frame(v) || l[i + 4L] != "<tbody>") { e <- c(e, sprintf("%s : %s introuvable", id, chemin)); next }
      lignes <- l[i + 4L + seq_len(nrow(v))]
      if (l[i + 5L + nrow(v)] != "</tbody>") { e <- c(e, sprintf("%s : %s, nombre de lignes", id, chemin)); next }
      lu <- do.call(rbind, strsplit(sub("^<tr><td>(.*)</td></tr>$", "\\1", lignes), "</td><td>", fixed = TRUE))
      attendu <- vapply(seq_along(v), function(j) {
        o <- tablexp:::ecrire_valeurs(v[[j]], "texte", "colonne")$octets
        html(lignes_de(o))
      }, character(nrow(v)))
      # Ligne d'en-tête : nom de colonne échappé, <br>, type rendu par
      # l'écrivain, dans l'ordre des colonnes de l'objet.
      types <- vapply(v, function(col) tablexp:::ecrire_valeurs(col, "texte", "colonne")$type, "")
      entete <- paste0("<tr>", paste0("<th>", html(names(v)), "<br>", types, "</th>", collapse = ""), "</tr>")
      if (l[i + 2L] != entete) e <- c(e, sprintf("%s : %s, en-tête", id, chemin))
      if (nrow(v) && !identical(unname(lu), matrix(attendu, nrow = nrow(v)))) e <- c(e, sprintf("%s : %s", id, chemin))
      n_tables <- n_tables + 1L
    }
  }
  if (n_tables < 50L) e <- c(e, sprintf("%d tables seulement", n_tables))
  if (length(e)) utils::head(e, 5L) else TRUE
})

# --- U18 : nombres, écriture %.17g (identité exacte) ---------------------------

# Cellules <td> découpées sur <br> ; chaque cellule décimale (regex de
# valeurs_texte()) égale à son écriture par l'écrivain unique du moteur
# (ecrire_valeurs(), forme « texte », #104) : U18 vérifie que les cellules
# passent par cet écrivain ; son exactitude (%.17g correctement arrondi)
# relève des témoins T1 à T3 (test_construire_table.R) et du contrôle
# tests/controles/ecriture_doubles_texte.R. Aucune condition de plateforme.
ecriture_texte <- function(v) strsplit(rawToChar(tablexp:::ecrire_valeurs(v, "texte", "x")$octets), "\n", fixed = TRUE)[[1L]]
cellules_decimales <- function(l) {
  td <- unlist(regmatches(l, gregexpr("<td>[^<]*(<br>[^<]*)*</td>", l)))
  c_ <- unlist(strsplit(sub("^<td>(.*)</td>$", "\\1", td), "<br>", fixed = TRUE))
  c_[grepl("^[-+]?([0-9]+[.]?[0-9]*|[.][0-9]+)([eE][-+]?[0-9]+)?$", c_)]
}
verifier("U18 : toute cellule décimale égale à l'écriture « texte » du moteur (ecrire_valeurs) de sa valeur ; 0.94999999999999996 présent et >0.95< absent (R1) ; 0.98999999999999999 (R2)", {
  e <- character(0)
  for (id in names(L)) {
    d <- cellules_decimales(L[[id]])
    faux <- d[d != ecriture_texte(as.numeric(d))]
    if (!length(d) || length(faux)) e <- c(e, sprintf("%s : %s", id, paste(utils::head(faux, 3L), collapse = ", ")))
  }
  s1 <- texte_de(o1)
  if (!grepl(">0.94999999999999996<", s1, fixed = TRUE) || grepl(">0.95<", s1, fixed = TRUE)) e <- c(e, "R1 : niveau")
  if (!grepl(">0.98999999999999999<", texte_de(O$R2), fixed = TRUE)) e <- c(e, "R2 : niveau")
  if (length(e)) e else TRUE
})

# --- U19 : contrôle statique de la section « Rapport figé » ---------------------

MARQUEUR <- "# --- Rapport figé (ADR 0012, point 2 ; #87) ---"
INTERDITS <- c("format", "formatC", "prettyNum", "format.Date", "format.POSIXct", "strftime", "as.character",
               "as.vector", "toString", "sprintf", "gettextf", "cat", "print", "message", "deparse", "dput",
               "encodeString", "storage.mode<-", "mode<-", "writeLines", "writeBin", "file", "Sys.time", "Sys.Date",
               "date", "Sys.getenv", "Sys.info", "sessionInfo")
noms_interdits <- function(exprs) intersect(unique(unlist(lapply(exprs, all.names))), INTERDITS)
verifier("U19 : témoin du contrôle statique : function(x) base::format(x) détecté",
         identical(noms_interdits(parse(text = "f <- function(x) base::format(x)", keep.source = FALSE)), "format"))
verifier("U19 : R/rapport.R : marqueur « Rapport figé » une seule fois ; après lui, des définitions de premier niveau seulement, aucun nom interdit (format, sprintf, as.character, cat, Sys.time...)", {
  l <- readLines(file.path(RACINE, "R", "rapport.R"), encoding = "UTF-8", warn = FALSE)
  k <- which(l == MARQUEUR)
  if (length(k) != 1L) return(sprintf("marqueur trouvé %d fois", length(k)))
  ex <- parse(text = l[(k + 1L):length(l)], keep.source = FALSE, encoding = "UTF-8")
  definitions <- vapply(ex, function(e) is.call(e) && identical(e[[1L]], as.name("<-")) && is.symbol(e[[2L]]), NA)
  n <- noms_interdits(ex)
  noms <- vapply(ex[definitions], function(e) as.character(e[[2L]]), "")
  e <- c(if (!all(definitions)) "expression de premier niveau qui n'est pas une définition",
         if (length(n)) paste("noms interdits :", paste(n, collapse = ", ")),
         if (!all(c("ecrire_rapport", "SECTIONS_RAPPORT") %in% noms)) "ecrire_rapport ou SECTIONS_RAPPORT hors de la section")
  if (length(e)) e else TRUE
})

# --- U20 : table de lignes = corps de la fiche écrite (identité exacte) --------

verifier("U20 : chaque ligne du corps de la fiche écrite (hors data.frame et @noms racine) présente en <tr> fiche/<chemin> | type | longueur | valeurs", {
  e <- character(0)
  for (id in names(X)) {
    t <- lignes_de(ecrire_fiche(X[[id]]$fiche))
    corps <- t[-1L][!startsWith(t[-1L], "#")]
    ch <- sub("\t.*$", "", corps)
    df <- grepl("/@(colonnes|lignes)$", ch) | grepl("^(exclusions|avertissements|hypotheses_brut/statuts)/", ch) |
      ch == "@noms"
    for (c_ in corps[!df]) {
      f <- strsplit(c_, "\t", fixed = TRUE)[[1L]]
      attendu <- paste0("<tr><td>", html(paste0("fiche/", f[1L])), "</td><td>", f[2L], "</td><td>", f[3L], "</td><td>",
                        paste(html(f[-(1:3)]), collapse = "<br>"), "</td></tr>")
      if (!attendu %in% L[[id]]) e <- c(e, sprintf("%s : %s", id, f[1L]))
    }
  }
  if (length(e)) utils::head(e, 5L) else TRUE
})

# --- U21, U22 : textes fixes et section brut (valeur calculée à la main) --------

verifier("U21 : phrase de plateforme une fois ; « pas de date » ; conventions ; quatre mentions d'empreinte ; fiche/plateforme/<n> ; rejeu ; mentions des sections 12 et 13", {
  e <- character(0)
  ct <- tablexp:::CONVENTIONS_TEXTE
  for (id in names(L)) {
    l <- L[[id]]
    if (sum(grepl("L'empreinte du résultat dépend de la plateforme (R, BLAS, LAPACK, bibliothèque C)", l, fixed = TRUE)) != 1L)
      e <- c(e, paste(id, ": phrase de plateforme"))
    if (!any(grepl("Ce rapport ne porte pas de date", l, fixed = TRUE))) e <- c(e, paste(id, ": pas de date"))
    if (!all(paste0("<li>", html(names(ct)), " : ", html(unname(ct)), "</li>") %in% l)) e <- c(e, paste(id, ": conventions"))
    ul <- section_de(l, "identification")
    if (sum(grepl("^<li>(algorithme|données|fichier|résultat et code) ", ul)) != 4L) e <- c(e, paste(id, ": mentions d'empreinte"))
    pf <- paste0("<tr><td>fiche/plateforme/", names(X[[id]]$fiche$plateforme), "</td>")
    if (!all(vapply(pf, function(p) any(startsWith(ul, p)), NA)) || !"bibliotheque_c" %in% names(X[[id]]$fiche$plateforme))
      e <- c(e, paste(id, ": plateforme"))
    rj <- section_de(l, "rejeu")
    if (!any(grepl("rejouer_fiche(", rj, fixed = TRUE)) || !any(grepl(html(tablexp:::TEXTE_REJEU_FICHE), rj, fixed = TRUE)))
      e <- c(e, paste(id, ": rejeu"))
    integ <- grep("^<p>Contrôle d'intégrité : ", ul, value = TRUE)
    hors <- setdiff(tablexp:::CHEMINS_HORS_RESULTAT, c("execution", "fiche/empreintes/resultat"))
    if (length(integ) != 1L ||
        !grepl(html("trouvée égale à fiche/empreintes/resultat, la valeur consignée à laquelle ce contrôle la compare (exclue du texte empreinté, puisqu'elle en est l'empreinte). Hors du périmètre"), integ, fixed = TRUE) ||
        !grepl(paste0("Hors du périmètre de cette empreinte, et donc de ce contrôle : ", paste(html(hors), collapse = ", "),
                      " ; ce que le rapport en restitue (valeurs, ou structure seule pour une table dont le volume croît avec la base) est lu dans l'objet, sans vérification.</p>"), integ, fixed = TRUE))
      e <- c(e, paste(id, ": phrase du contrôle d'intégrité"))
    if (!"<p>Validations : non produites par cette version (chemin etapes/validation absent de l'objet).</p>" %in% l ||
        !"<p>Table finale et comparaisons : non produites par cette version (chemin table : NULL).</p>" %in% l)
      e <- c(e, paste(id, ": sections 12 et 13"))
  }
  if (length(e)) e else TRUE
})
verifier("U22 : etape_brut : légende de etapes/brut/parametres/statuts avant la première légende etapes/brut/resultat/ ; légende binomiale R1 et R6, absente R4 ; kaplan_meier_sauts en structure seule (R6)", {
  e <- character(0)
  for (id in c("R1", "R2", "R3", "R4", "R5", "R6")) {
    b <- section_de(L[[id]], "etape_brut")
    st <- grep("<caption><code>etapes/brut/parametres/statuts</code>", b, fixed = TRUE)
    re <- grep("<caption><code>etapes/brut/resultat/", b, fixed = TRUE)
    if (length(st) != 1L || !length(re) || st > re[1L]) e <- c(e, paste(id, ": ordre des légendes"))
    # Légende des mots « exact » et « asymptotique » immédiatement après le
    # tableau des statuts (spécification d'actuary, § 2.4).
    fin <- st - 1L + match("</table>", b[st:length(b)])
    if (is.na(fin) || !startsWith(b[fin + 1L], "<p>Les mots « exact » et « asymptotique »"))
      e <- c(e, paste(id, ": légende hors de sa place"))
    bin <- any(startsWith(b, "<p>Intervalle binomial « exact »"))
    if (bin != (id != "R4")) e <- c(e, paste(id, ": légende binomiale"))
  }
  b <- section_de(L$R6, "etape_brut")
  if (!any(grepl("^<tr><td>etapes/brut/resultat/kaplan_meier_sauts</td><td>data.frame</td><td>[0-9]+</td><td>non reproduites", b)) ||
      any(grepl("<code>etapes/brut/resultat/kaplan_meier_sauts</code>", b, fixed = TRUE)))
    e <- c(e, "R6 : kaplan_meier_sauts")
  if (length(e)) e else TRUE
})

# --- export et constante TEXTE_REJEU_FICHE (valeur calculée à la main) ----------

verifier("ecrire_rapport : exportée, signature (x)",
         "ecrire_rapport" %in% getNamespaceExports("tablexp") && identical(names(formals(ecrire_rapport)), "x"))
verifier("U25 : la ligne « # rejeu : » de la fiche écrite égale TEXTE_REJEU_FICHE", {
  t <- lignes_de(ecrire_fiche(x1$fiche))
  identical(t[startsWith(t, "# rejeu : ")], paste0("# rejeu : ", tablexp:::TEXTE_REJEU_FICHE))
})
