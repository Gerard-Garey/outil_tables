# -----------------------------------------------------------------------------
#  tests/reference/produire_references.R : production des références de
#  non-régression (issue #17 ; ADR 0009, points 2.2, 5.2 et 5.3)
#
#  Script du workflow .github/workflows/references.yml ; exécutable aussi sur
#  un poste en mode `releve`, pour essai : ses fichiers ne sont alors JAMAIS
#  commités (CLAUDE.md, « Changements de résultats »).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/reference/produire_references.R --mode <mode> \
#          --plans <tous | R1,R2,...> --sortie <dossier> [--motif <texte>]
#  Modes (point 5.2) ; le script produit TOUJOURS tous les plans et compare
#  chaque fichier produit au fichier commité de tests/reference/ s'il existe ;
#  le mode fixe ce qui est admis :
#    creation      plans visés : aucun fichier commité ; autres : fichier
#                  commité présent et comparaison réussie ;
#    regeneration  plans visés : fichier commité présent, écarts admis ;
#                  autres : comparaison réussie ;
#    bascule       --plans tous ; écarts admis sur tous ; l'identifiant de
#                  plateforme de l'exécution diffère de celui, unique, des
#                  en-têtes commités ;
#    releve        hors CI seulement (refusé si GITHUB_ACTIONS = true) :
#                  produit, compare, imprime, n'applique aucune règle de mode.
#  Dans tous les modes : un double sans classe de tolérance (point 3.4), une
#  référence commitée orpheline (point 3.7) ou un refus d'écriture font
#  échouer. --motif est exigé hors du mode releve.
#
#  Sorties : <sortie>/references/<id>.tsv (en-tête du point 2.2),
#  <sortie>/ecarts.txt (impression du point 3.5) et, s'il existe,
#  $GITHUB_STEP_SUMMARY. Le dossier de sortie ne peut être tests/reference/
#  ni s'y trouver. Code de sortie 1 en cas d'échec.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  ici <- if (length(f)) dirname(normalizePath(f[1L])) else file.path("tests", "reference")
  # Harnais dans un environnement propre : installer_tablexp() sans la garde
  # de sortie .Last des batteries.
  assign(".h", new.env(), envir = globalenv())
  sys.source(file.path(dirname(ici), "harnais.R"), envir = .h)
  # Littéraux de l'outillage marqués UTF-8 quelle que soit la locale (même
  # chargeur que charger_utf8() d'outils_reference.R).
  for (e in parse(file.path(ici, "outils_reference.R"), encoding = "UTF-8", keep.source = FALSE)) eval(e, globalenv())
})

MODES <- c("creation", "regeneration", "bascule", "releve")

echouer <- function(message) {
  cat("ECHEC :", message, "\n")
  if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
    cat("::error title=Références::", gsub("\n", " ", message), "\n", sep = "")
  quit(save = "no", status = 1L)
}

option <- function(args, nom) {
  i <- match(nom, args)
  if (is.na(i) || i == length(args)) NA_character_ else args[i + 1L]
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  racine <- .h$racine_depot()
  mode <- option(args, "--mode"); plans_demandes <- option(args, "--plans")
  sortie <- option(args, "--sortie"); motif <- option(args, "--motif")
  if (is.na(mode) || !mode %in% MODES) echouer(sprintf("--mode parmi %s", paste(MODES, collapse = ", ")))
  if (mode == "releve" && identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
    echouer("mode releve refusé sur la CI (essai sur un poste seulement)")
  if (mode != "releve" && (is.na(motif) || !nzchar(trimws(motif))))
    echouer("--motif exigé (issue et raison)")
  if (is.na(sortie)) echouer("--sortie exigé")
  plans <- plans_reference()
  vises <- if (is.na(plans_demandes)) character(0) else if (identical(plans_demandes, "tous")) names(plans)
           else trimws(strsplit(plans_demandes, ",", fixed = TRUE)[[1L]])
  if (!length(vises) || !all(vises %in% names(plans)) || anyDuplicated(vises))
    echouer(sprintf("--plans : « tous » ou identifiants parmi %s, séparés par des virgules", paste(names(plans), collapse = ", ")))
  if (mode == "bascule" && !identical(plans_demandes, "tous")) echouer("mode bascule : --plans tous obligatoire")
  commis <- file.path(racine, "tests", "reference")
  if (sortie_interdite(sortie, commis))
    echouer("le dossier de sortie ne peut être tests/reference/ ni s'y trouver (ADR 0009, point 9.4)")
  dir.create(sortie, showWarnings = FALSE, recursive = TRUE)
  dossier <- file.path(sortie, "references")
  if (dir.exists(dossier) && length(list.files(dossier))) echouer(sprintf("%s n'est pas vide", dossier))
  dir.create(dossier, showWarnings = FALSE)

  .h$installer_tablexp(racine)
  plateforme <- identifiant_plateforme(racine)
  ci <- function(v) { x <- Sys.getenv(v); if (nzchar(x)) x else "(hors CI)" }
  adresse <- if (nzchar(Sys.getenv("GITHUB_RUN_ID")))
    sprintf("%s/%s/actions/runs/%s", Sys.getenv("GITHUB_SERVER_URL", "https://github.com"),
            Sys.getenv("GITHUB_REPOSITORY"), Sys.getenv("GITHUB_RUN_ID")) else "(hors CI)"
  date_utc <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC", tz = "UTC")

  defauts <- character(0); impression <- character(0)
  resume <- data.frame(plan = character(0), commis = character(0), comparaison = character(0),
                       admis = character(0), stringsAsFactors = FALSE)
  orphelines <- if (dir.exists(commis)) references_orphelines(commis, plans) else character(0)
  for (o in orphelines) defauts <- c(defauts, sprintf("référence orpheline : tests/reference/%s", o))
  plateformes_commises <- list()

  jeu <- jeu_reference()
  for (p in plans) {
    id <- p$identifiant
    x <- construire_table(jeu$donnees, jeu$roles, p$plan)
    g <- tryCatch(aplatir_construction(x), refus_reference = function(e) e)
    non_produit <- function(e) {
      defauts <<- c(defauts, sprintf("%s : %s", id, conditionMessage(e)))
      resume[nrow(resume) + 1L, ] <<- list(id, if (file.exists(file.path(commis, paste0(id, ".tsv")))) "oui" else "non",
                                           "non produit", "NON")
    }
    if (inherits(g, "refus_reference")) { non_produit(g); next }
    sans <- doubles_sans_tolerance(g)
    for (c_ in sans) defauts <- c(defauts, sprintf("%s : grandeur sans tolérance déclarée : %s", id, c_))
    entete <- entete_reference(p, plateforme, x, list(commit = ci("GITHUB_SHA"), execution = ci("GITHUB_RUN_ID"),
                                                     adresse = adresse), mode, date_utc)
    fichier <- file.path(dossier, paste0(id, ".tsv"))
    e_ecr <- tryCatch({ ecrire_reference(g, fichier, entete); NULL }, refus_reference = function(e) e)
    if (!is.null(e_ecr)) { non_produit(e_ecr); next }
    produit <- tryCatch(lire_reference(fichier), error = function(e) e)
    if (inherits(produit, "error")) { non_produit(produit); next }
    if (!identical(produit$types, g$types) || !identical(produit$valeurs, g$valeurs))
      defauts <- c(defauts, sprintf("%s : aller-retour du fichier produit non identique", id))
    f_commis <- file.path(commis, paste0(id, ".tsv"))
    present <- file.exists(f_commis)
    comparaison <- "sans fichier commité"
    ref <- if (present) tryCatch(lire_reference(f_commis), error = function(e) e)
    if (inherits(ref, "error")) {
      defauts <- c(defauts, sprintf("%s : fichier commité illisible : %s", id, conditionMessage(ref)))
      impression <- c(impression, sprintf("== %s : fichier commité illisible : %s", id, conditionMessage(ref)), "")
      comparaison <- "illisible"
    } else if (present) {
      plateformes_commises[[id]] <- unname(ref$entete[names(ref$entete) == "plateforme"])
      r <- comparer_references(ref, produit)
      impression <- c(impression, imprimer_comparaison(r, sprintf("== %s : %s (commité contre produit)", id, p$description)), "")
      comparaison <- if (r$ok) "réussie" else "en écart"
    } else impression <- c(impression, sprintf("== %s : %s : aucun fichier commité (création)", id, p$description), "")
    vise <- id %in% vises
    refus_mode <- regle_mode(mode, vise, present, comparaison == "réussie")
    admis <- is.na(refus_mode)
    if (!admis) defauts <- c(defauts, sprintf("%s : mode %s, plan %s : %s", id, mode,
                                              if (vise) "visé" else "non visé", refus_mode))
    resume[nrow(resume) + 1L, ] <- list(id, if (present) "oui" else "non", comparaison, if (admis) "oui" else "NON")
  }
  if (mode == "bascule") {
    b <- controle_bascule(plateformes_commises, plateforme$controle)
    if (!is.na(b)) defauts <- c(defauts, paste("mode bascule :", b))
  }
  if (length(plateforme$defauts))
    impression <- c(impression, paste0("Plateforme non conforme (relevé) : ", plateforme$defauts), "")

  bilan_txt <- c(sprintf("Références : mode %s ; plans visés %s ; %s", mode, paste(vises, collapse = ","), date_utc),
                 sprintf("  %s : fichier commité %s ; comparaison %s ; admis %s", resume$plan, resume$commis,
                         resume$comparaison, resume$admis),
                 if (length(defauts)) paste0("  ECHEC : ", defauts) else "  OK", "")
  writeBin(octets_texte(c(bilan_txt, impression)), file.path(sortie, "ecarts.txt"))
  cat(bilan_txt, impression, sep = "\n")
  s <- Sys.getenv("GITHUB_STEP_SUMMARY")
  if (nzchar(s))
    cat(c(sprintf("### Références de non-régression (mode %s)", mode), "",
          "| Plan | Fichier commité | Comparaison | Admis |", "|---|---|---|---|",
          sprintf("| %s | %s | %s | %s |", resume$plan, resume$commis, resume$comparaison, resume$admis), "",
          if (length(defauts)) paste0("- ECHEC : ", defauts) else "- OK", "",
          "```", impression, "```", ""), file = s, sep = "\n", append = TRUE)
  if (length(defauts)) echouer(paste(defauts, collapse = " ; "))
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
