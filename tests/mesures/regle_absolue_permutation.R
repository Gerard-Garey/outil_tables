# -----------------------------------------------------------------------------
#  tests/mesures/regle_absolue_permutation.R : mesure hors CI de la règle de
#  comparaison absolue à une échelle déclarée et du régime de constat hors
#  plateforme de référence, sous permutation des lignes de l'entrée (issue
#  #243, étape (1) de B39 ; ADR 0009, annotation du 26/11/2026, (c) 2, (c) 4
#  et (g) 4 ; décision du mainteneur M606)
#
#  Hors des batteries et hors CI (aucun seuil ; sortie citée au commit, puis
#  rejouée après la régénération (R), annotation du 26/11/2026, (h) 5).
#  Protocole de la remesure (0′) d'actuary-approfondi ((c) 2) : lignes de
#  l'entrée `reference` permutées, comparées à l'objet nominal ; plans R15
#  (Whittaker-Henderson (a), coherence_regularite, validation lissee) et R16
#  (Whittaker-Henderson (b), lambda par la LAML, validation lissee) de
#  plans_reference(). Constat de simulation : la permutation tient lieu de
#  changement de plateforme, elle n'en est pas un ; elle est sans effet sur
#  R17 (entrée d'agrégats), qui n'est pas permuté.
#
#  Graines de permutation (GRAINES_PERMUTATION) : 22028701 à 22028703
#  (remesure (0′), (c) 2) et 22028711 à 22028730 (vingt permutations de plus,
#  (c) 4), réserve k = 7 de la plage de la porteuse B19
#  (tests/controles/graines_admises.csv) ; tirage par sample.int() sous
#  set.seed(graine, "Mersenne-Twister", "Inversion", "Rejection"), comme
#  tests/mesures/tolerance_optimisation.R (même fonction permuter()). La
#  méthode de tirage du script hors dépôt de la remesure n'est pas connue :
#  les maxima coïncident à trois chiffres avec ceux de (c) 4 : indice fort,
#  non preuve, de l'identité des permutations.
#
#  Comparaison : objet nominal = fichier commité tests/reference/<plan>.tsv
#  (contrôlé identique à l'objet recalculé sur la même machine : premier
#  bloc de la sortie) ; objet permuté aplati par aplatir_construction(). Les
#  empreintes qui dépendent de l'ordre des lignes (fiche/empreintes/donnees
#  et lignes @empreinte de etapes/donnees/resultat/) sont recopiées du
#  nominal dans l'objet permuté, et elles seules, pour que la comparaison
#  porte sur les valeurs (sinon : « entrée différente », point 3.2) ; les
#  chemins neutralisés sont imprimés. Deux comparaisons par permutation, par
#  comparer_references() de tests/reference/outils_reference.R (règle codée) :
#  régime strict (constats = character(0)) et régime de constat (chemins du
#  plan de CONSTATS_HORS_PLATEFORME). Imprimés : chemins en écart dans chaque
#  régime, verdicts et maxima de |a - b| / S des six chemins de la règle
#  absolue et écart relatif des trois clés reclassées à 1e-10, marges
#  tau x facteur / max(|a - b| / S) à confronter à celles de (c) 4 (64, 124,
#  132, 33, 598, 24 ; 104 pour les trois clés reclassées), et conformité aux
#  attendus de (g) 4.
#
#  Usage (depuis la racine du dépôt, locale UTF-8 recommandée) :
#      LC_ALL=C.UTF-8 Rscript tests/mesures/regle_absolue_permutation.R [n]
#  n (facultatif) : nombre de graines lues dans GRAINES_PERMUTATION, dans
#  l'ordre (défaut : les vingt-trois).
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
installer_tablexp(RACINE)
local(for (e in parse(file.path(RACINE, "tests", "reference", "outils_reference.R"), encoding = "UTF-8",
                      keep.source = FALSE)) eval(e, globalenv()))

GRAINES_PERMUTATION <- c(22028701L, 22028702L, 22028703L,
                         22028711L, 22028712L, 22028713L, 22028714L, 22028715L, 22028716L, 22028717L,
                         22028718L, 22028719L, 22028720L, 22028721L, 22028722L, 22028723L, 22028724L,
                         22028725L, 22028726L, 22028727L, 22028728L, 22028729L, 22028730L)
args <- commandArgs(trailingOnly = TRUE)
n_graines <- if (length(args)) as.integer(args[1L]) else length(GRAINES_PERMUTATION)
stopifnot(!is.na(n_graines), n_graines >= 1L, n_graines <= length(GRAINES_PERMUTATION))
graines <- GRAINES_PERMUTATION[seq_len(n_graines)]

# --- plateforme et graines consignées ----------------------------------------------

plateforme <- identifiant_plateforme(RACINE)
cat(R.version.string, "\n")
cat("La_version() :", La_version(), "\n")
cat("La_library() :", La_library(), "\n")
cat("extSoftVersion()[\"BLAS\"] :", unname(extSoftVersion()["BLAS"]), "\n")
cat("Plateforme (lignes contrôlées) :\n"); cat(paste0("  ", plateforme$controle), sep = "\n")
cat("Écarts à la plateforme de référence :", if (length(plateforme$defauts)) paste(plateforme$defauts, collapse = " ; ") else "aucun", "\n")
cat("Graines de permutation :", paste(graines, collapse = ", "), "\n")
cat("Générateur des permutations : set.seed(graine, \"Mersenne-Twister\", \"Inversion\", \"Rejection\"), sample.int()\n\n")

# --- entrée, plans et objets nominaux ----------------------------------------------

ENTREE <- jeu_reference()
PLANS <- plans_reference()[c("R15", "R16")]
permuter <- function(entree, graine) {
  set.seed(graine, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  entree$donnees <- entree$donnees[sample.int(nrow(entree$donnees)), , drop = FALSE]
  rownames(entree$donnees) <- NULL
  entree
}
aplatir_plan <- function(entree, p)
  aplatir_construction(construire_table(entree$donnees, entree$roles, p$plan, tables_reference = tables_du_plan(p)))
NOMINAUX <- lapply(PLANS, function(p) lire_reference(file.path(RACINE, "tests", "reference", paste0(p$identifiant, ".tsv"))))
for (id in names(PLANS)) {
  t0 <- proc.time()[["elapsed"]]
  r <- comparer_references(NOMINAUX[[id]], aplatir_plan(ENTREE, PLANS[[id]]))
  cat(sprintf("Contrôle d'entrée, %s : objet recalculé contre le fichier commité : %s (%d grandeurs, %d identiques ; %.1f s)\n", id,
              if (r$ok) "OK" else "ECHEC", nrow(r$grandeurs), sum(r$grandeurs$verdict == "identique"),
              proc.time()[["elapsed"]] - t0))
}
cat("\n")

# Empreintes qui dépendent de l'ordre des lignes : recopiées du nominal.
neutraliser <- function(g, nominal) {
  ch <- c(CHEMIN_EMPREINTE_DONNEES,
          grep("^etapes/donnees/resultat/.*/@empreinte$", names(nominal$types), value = TRUE))
  ch <- intersect(ch, names(g$types))
  change <- ch[!vapply(ch, function(c_) identical(g$valeurs[[c_]], nominal$valeurs[[c_]]), NA)]
  for (c_ in change) g$valeurs[[c_]] <- nominal$valeurs[[c_]]
  attr(g, "neutralises") <- change
  g
}

# Verdicts comparés sur leur motif ASCII ou aux constantes de l'outillage (un
# littéral non ASCII de ce fichier n'est pas marqué en locale C).
admis <- function(v) v == "identique" | grepl("^sous tol", v, useBytes = TRUE)
CLES_RECLASSEES <- paste0("etapes/coherence_regularite/resultat/regularite/", c("regularite", "r_estime", "regularite_r"))
chemins_absolus <- function(nominal) {
  d <- names(nominal$types)[nominal$types == "double"]
  d[vapply(d, function(c_) any(cle_couvre(ECHELLES_ABSOLUES$cle, c_)), NA)]
}

# --- permutations -------------------------------------------------------------------

lignes <- list()
for (id in names(PLANS)) {
  constats <- CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == id]
  abs_ch <- chemins_absolus(NOMINAUX[[id]])
  for (graine in graines) {
    t0 <- proc.time()[["elapsed"]]
    g <- neutraliser(aplatir_plan(permuter(ENTREE, graine), PLANS[[id]]), NOMINAUX[[id]])
    rs <- comparer_references(NOMINAUX[[id]], g)
    rc <- comparer_references(NOMINAUX[[id]], g, constats = constats)
    gs <- rs$grandeurs; gc <- rc$grandeurs
    ecart_s <- gs$chemin[!admis(gs$verdict)]
    ecart_c <- gc$chemin[!(admis(gc$verdict) | gc$verdict == VERDICT_CONSTAT)]
    cause <- sum(gs$type == "double" & startsWith(gs$chemin, PREFIXE_CAUSE_CONSTAT) & gs$n_differents > 0L)
    cat(sprintf("%s, graine %d (%.1f s) : neutralisés : %s ; structure : %d ; entrée différente : %s\n", id, graine,
                proc.time()[["elapsed"]] - t0, paste(attr(g, "neutralises"), collapse = ", "), length(rs$structure),
                rs$entree$differente))
    cat(sprintf("  condition de cause : %d double(s) de %s non identique(s)\n", cause, PREFIXE_CAUSE_CONSTAT))
    cat(sprintf("  strict : %s ; %d chemin(s) en écart : %s\n", if (rs$ok) "OK" else "ECHEC", length(ecart_s), paste(ecart_s, collapse = ", ")))
    cat(sprintf("  constat : %s ; %d constat(s) ; %d chemin(s) en écart : %s\n", if (rc$ok) "OK" else "ECHEC",
                sum(gc$verdict == VERDICT_CONSTAT), length(ecart_c), paste(ecart_c, collapse = ", ")))
    for (c_ in c(abs_ch, intersect(CLES_RECLASSEES, gs$chemin))) {
      k <- match(c_, gs$chemin)
      absolu <- !is.na(gs$classe_absolue[k])
      # Éléments jugés : pour un chemin de la règle absolue, ses éléments
      # couverts (|a - b| <= tau x facteur x S ; le verdict du vecteur porte
      # aussi ses éléments non couverts, ainsi des 61 autres lignes de
      # statistiques/valeur de R16, chemin déclaré de (f) 2) ; pour une clé
      # reclassée, le vecteur.
      elements_ok <- if (absolu) gs$ecart_absolu_max[k] <= CLASSES_ABSOLUES[[gs$classe_absolue[k]]] else admis(gs$verdict[k])
      lignes[[length(lignes) + 1L]] <- data.frame(plan = id, graine = graine, chemin = c_, verdict = gs$verdict[k],
                                                  elements_ok = elements_ok,
                                                  regle = if (absolu) gs$classe_absolue[k] else "1e-10 (relative)",
                                                  mesure = if (absolu) gs$ecart_absolu_max[k] * ECHELLES_ABSOLUES$facteur[cle_couvre(ECHELLES_ABSOLUES$cle, c_)]
                                                           else gs$ecart_relatif_max[k],
                                                  stringsAsFactors = FALSE)
    }
    lignes[[length(lignes) + 1L]] <- data.frame(plan = id, graine = graine, chemin = "(écarts)", verdict = NA_character_,
                                                elements_ok = NA,
                                                regle = paste(c(paste("S:", paste(ecart_s, collapse = "|")),
                                                                paste("C:", paste(ecart_c, collapse = "|"))), collapse = " ## "),
                                                mesure = NA_real_, stringsAsFactors = FALSE)
  }
}
tab <- do.call(rbind, lignes)

# --- synthèse : maxima, marges et attendus de (g) 4 ------------------------------------

cat(sprintf("\nSynthèse sur %d permutation(s) par plan\n", length(graines)))
cat("Règle absolue : max |a - b| / S (échelle seule, comme (c) 2) ; marge = tau x facteur / max ; verdicts\n")
m <- tab[tab$chemin != "(écarts)", ]
for (c_ in unique(m$chemin)) for (id in unique(m$plan[m$chemin == c_])) {
  x <- m[m$chemin == c_ & m$plan == id, ]
  if (grepl("^abs_", x$regle[1L])) {
    l <- ECHELLES_ABSOLUES[cle_couvre(ECHELLES_ABSOLUES$cle, c_), ]
    tau <- CLASSES_ABSOLUES[[l$classe]]
    cat(sprintf("  %s, %s : max |a - b| / S = %.3g ; classe %s, facteur %g ; marge %s ; verdicts : %s\n", id, c_, max(x$mesure),
                l$classe, l$facteur, if (max(x$mesure) > 0) sprintf("%.0f", tau * l$facteur / max(x$mesure)) else "infinie (aucun écart)",
                paste(names(table(x$verdict)), table(x$verdict), collapse = ", ")))
    if (!all(admis(x$verdict)))
      cat("    (verdict du vecteur : il porte aussi des éléments non couverts, jugés par la règle relative ; éléments couverts sous leur borne :",
          if (all(x$elements_ok)) "oui)\n" else "NON)\n")
  } else {
    cat(sprintf("  %s, %s : écart relatif max = %.3g ; classe 1e-10 (relative) ; marge %s ; verdicts : %s\n", id, c_, max(x$mesure),
                if (max(x$mesure) > 0) sprintf("%.0f", 1e-10 / max(x$mesure)) else "infinie (aucun écart)",
                paste(names(table(x$verdict)), table(x$verdict), collapse = ", ")))
  }
}
cat("Marges de l'annotation du 26/11/2026, (c) 4 (vingt-trois permutations, rapportées) : residu_conservation_0 64, residu_conservation_1 124, smr/ecart 132, delta1 33, delta2 598, ordre_segments/ecart 24 ; clés reclassées 104\n")

# Attendus de (g) 4 (hypothèse tirée de la remesure (0′)) : les six chemins
# de (b) et les trois clés de (d) identiques ou sous tolérance ; strict : R16
# en écart sur les onze chemins de (f) 1 et eux seuls, R15 sur les quatre
# recopies et les trois résidus ; constat : R16 sans écart, R15 en écart sur
# les trois résidus seuls.
e <- tab[tab$chemin == "(écarts)", ]
morceaux <- function(s, cle) {
  p <- sub(paste0("^", cle, ": "), "", strsplit(s, " ## ", fixed = TRUE)[[1L]][if (cle == "S") 1L else 2L])
  if (nzchar(p)) strsplit(p, "|", fixed = TRUE)[[1L]] else character(0)
}
residus <- paste0("etapes/validation/resultat/cellules/residu_", c("reponse", "pearson", "deviance"))
attendus <- list(
  R16 = list(S = CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == "R16"], C = character(0)),
  R15 = list(S = c(CONSTATS_HORS_PLATEFORME$chemin[CONSTATS_HORS_PLATEFORME$plan == "R15"], residus), C = residus))
# Deux lectures, imprimées l'une et l'autre (la décision revient à actuary,
# (g) 4) : égalité exacte de l'ensemble des chemins en écart avec l'attendu
# (lecture stricte, à chaque permutation) et inclusion (aucun chemin en écart
# hors de l'attendu). Les graines de la remesure (0′), (c) 2, sont
# distinguées des vingt de (c) 4.
GRAINES_C2 <- GRAINES_PERMUTATION[1:3]
cat("\nConformité aux attendus de (g) 4, par permutation (égalité exacte ; inclusion) :\n")
ok_absolus <- all(m$elements_ok)
cat(sprintf("  éléments couverts des six chemins de (b) sous leur borne, trois clés de (d) identiques ou sous tolérance : %s%s\n",
            if (ok_absolus) "oui" else "NON", if (ok_absolus) "" else paste0(" (", paste(unique(m$chemin[!m$elements_ok]), collapse = ", "), ")")))
e$egal <- NA; e$inclus <- NA
for (k in seq_len(nrow(e))) {
  a <- attendus[[e$plan[k]]]
  s <- morceaux(e$regle[k], "S"); c_ <- morceaux(e$regle[k], "C")
  e$egal[k] <- setequal(s, a$S) && setequal(c_, a$C)
  e$inclus[k] <- all(s %in% a$S) && all(c_ %in% a$C)
  ecrire <- function(x, att) if (setequal(x, att)) "égal" else sprintf("%s (en plus : %s ; non en écart : %s)",
    if (all(x %in% att)) "inclus" else "HORS ATTENDU", paste(setdiff(x, att), collapse = ", "), paste(setdiff(att, x), collapse = ", "))
  cat(sprintf("  %s, graine %d%s : strict %s ; constat %s\n", e$plan[k], e$graine[k], if (e$graine[k] %in% GRAINES_C2) " ((c) 2)" else "",
              ecrire(s, a$S), ecrire(c_, a$C)))
}
c2 <- e$graine %in% GRAINES_C2
cat(sprintf("\nGraines de (c) 2 (%s) : égalité exacte %d / %d ; inclusion %d / %d\n", paste(GRAINES_C2, collapse = ", "),
            sum(e$egal[c2]), sum(c2), sum(e$inclus[c2]), sum(c2)))
cat(sprintf("Vingt graines de (c) 4 : égalité exacte %d / %d ; inclusion %d / %d\n", sum(e$egal[!c2]), sum(!c2),
            sum(e$inclus[!c2]), sum(!c2)))
# Verdict unique, lecture de (g) 4 visée par actuary le 26/11/2026 (trois
# conditions) : égalité exacte sur les graines de (c) 2, inclusion sur toutes,
# chaque chemin attendu en écart au moins une fois ; plus les éléments
# couverts sous leur borne. Les décomptes d'égalité ci-dessus sont imprimés à
# titre d'information.
couv <- vapply(names(attendus), function(id) {
  x <- e[e$plan == id, ]
  s_tous <- unique(unlist(lapply(x$regle, morceaux, "S"))); c_tous <- unique(unlist(lapply(x$regle, morceaux, "C")))
  setequal(s_tous, attendus[[id]]$S) && setequal(c_tous, attendus[[id]]$C)
}, NA)
conforme <- ok_absolus && all(e$egal[c2]) && all(e$inclus) && all(couv)
cat(sprintf("Résultat (lecture de (g) 4 visée par actuary le 26/11/2026) : égalité sur les graines de (c) 2 %d / %d ; inclusion %d / %d ; chaque chemin attendu en écart au moins une fois : %s ; éléments couverts sous leur borne : %s => %s\n",
            sum(e$egal[c2]), sum(c2), sum(e$inclus), nrow(e), if (all(couv)) "oui" else paste("NON (", paste(names(couv)[!couv], collapse = ", "), ")"),
            if (ok_absolus) "oui" else "NON",
            if (conforme) "conforme aux attendus de (g) 4" else "DIFFÉRENT des attendus de (g) 4 : à soumettre à actuary (annotation du 26/11/2026, (g) 4)"))
bilan(code_sortie = FALSE)
