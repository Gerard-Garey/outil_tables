# -----------------------------------------------------------------------------
#  tests/controles/identite_batteries.R : identité des batteries entre
#  CLAUDE.md, BATTERIES et ci.yml, et de la chaîne d'image des jobs R
#  (issue #8 ; CLAUDE.md, « Commandes » ; ADR 0008, point 1)
#
#  Contrôles :
#    1. les batteries sont les mêmes, dans le même ordre, dans :
#       - CLAUDE.md : premier bloc de code délimité par ``` de la section
#         « ## Commandes », une commande par ligne non vide ;
#       - .claude/workflows/circuit-technique.js : chaînes entre apostrophes
#         de `const BATTERIES = [ ... ]`, lignes de commentaire exclues ;
#       - .github/workflows/ci.yml : `run:` des étapes du seul job `tests:`
#         dont le nom commence par « Batterie : » ; ce job déclare
#         `container:` et aucune étape de batterie n'a continue-on-error ;
#       aucune des trois listes n'est vide ;
#    2. la marque « À ADAPTER » est absente de ces trois fichiers ;
#    3. dans .github/workflows/*.yml, toutes les valeurs `image:` et
#       `TABLEXP_IMAGE:` sont une même chaîne, et il y a autant de
#       TABLEXP_IMAGE que d'image (ADR 0008, point 1 : chaîne unique).
#
#  Limites : lecture ligne à ligne, sans analyseur YAML ni JavaScript (R de
#  base) ; une commande `run:` sur plusieurs lignes (bloc |) n'est pas lue ;
#  un `image:` écrit hors d'une ligne propre n'est pas vu.
#
#  Usage : Rscript tests/controles/identite_batteries.R [racine du dépôt]
#  Code de sortie 1 en cas d'écart. Sourcé (tests), le fichier ne fait que
#  définir les fonctions.
# -----------------------------------------------------------------------------

MARQUE_A_ADAPTER <- "\u00c0 ADAPTER"
JOB_TESTS <- "tests"  # clé du job « Tests (R 4.3.3) » dans ci.yml

# Lignes lues en octets (sans conversion d'encodage) : les comparaisons
# portent sur des motifs ASCII, et la marque est cherchée octet par octet
# (useBytes), pour un verdict indépendant de la locale.
lire_lignes <- function(chemin) readLines(chemin, warn = FALSE)

batteries_claude <- function(chemin) {
  l <- lire_lignes(chemin)
  debut <- which(l == "## Commandes")
  if (length(debut) != 1L) return(character(0))
  suite <- l[(debut + 1L):length(l)]
  fin_section <- which(grepl("^## ", suite))
  if (length(fin_section)) suite <- suite[seq_len(fin_section[1L] - 1L)]
  clotures <- which(grepl("^```", suite))
  if (length(clotures) < 2L) return(character(0))
  bloc <- trimws(suite[(clotures[1L] + 1L):(clotures[2L] - 1L)])
  bloc[nzchar(bloc)]
}

batteries_js <- function(chemin) {
  l <- lire_lignes(chemin)
  debut <- grep("^const BATTERIES = \\[", l)
  if (length(debut) != 1L) return(character(0))
  fin <- debut[1L] - 1L + which(grepl("^\\]", l[debut:length(l)]))[1L]
  if (is.na(fin)) return(character(0))
  corps <- trimws(l[seq.int(debut + 1L, length.out = max(0L, fin - debut - 1L))])
  corps <- corps[nzchar(corps) & !startsWith(corps, "//")]
  m <- regmatches(corps, regexpr("'[^']*'", corps))
  gsub("^'|'$", "", m)
}

# Lignes du job `job` (clé à deux espaces sous `jobs:`) de ci.yml.
lignes_job <- function(l, job = JOB_TESTS) {
  debut <- which(l == sprintf("  %s:", job))
  if (length(debut) != 1L) return(character(0))
  suite <- l[(debut + 1L):length(l)]
  fin <- which(grepl("^  [A-Za-z0-9_-]+:[[:space:]]*$", suite) | grepl("^[^[:space:]#]", suite))
  if (length(fin)) suite[seq_len(fin[1L] - 1L)] else suite
}

# Étapes du job de tests : list(nom, lignes) pour chaque `- ` de premier
# niveau sous `steps:`.
etapes_job <- function(lj) {
  debuts <- grep("^      - ", lj)
  lapply(seq_along(debuts), function(k) {
    fin <- if (k < length(debuts)) debuts[k + 1L] - 1L else length(lj)
    lj[debuts[k]:fin]
  })
}

batteries_ci <- function(chemin) {
  etapes <- etapes_job(lignes_job(lire_lignes(chemin)))
  nommees <- Filter(function(e) grepl("^[[:space:]]*- name:[[:space:]]*\"?Batterie :", e[1L]), etapes)
  vapply(nommees, function(e) {
    run <- grep("^[[:space:]]*run:[[:space:]]*[^|>[:space:]]", e, value = TRUE)
    if (length(run) != 1L) return(NA_character_)
    trimws(sub("^[[:space:]]*run:", "", run))
  }, character(1L))
}

# Écarts propres au job de tests : job absent, sans `container:`, étape de
# batterie avec continue-on-error.
defauts_job_tests <- function(chemin) {
  lj <- lignes_job(lire_lignes(chemin))
  if (!length(lj)) return(sprintf("ci.yml : job `%s:` absent", JOB_TESTS))
  d <- character(0)
  if (!any(grepl("^    container:", lj)))
    d <- c(d, sprintf("ci.yml : job `%s:` sans `container:` (ADR 0008, point 1)", JOB_TESTS))
  for (e in etapes_job(lj))
    if (grepl("Batterie :", e[1L], fixed = TRUE) && any(grepl("continue-on-error", e, fixed = TRUE)))
      d <- c(d, sprintf("ci.yml : continue-on-error interdit sur l'étape %s", trimws(e[1L])))
  d
}

valeurs_cle <- function(lignes, cle) {
  m <- grep(sprintf("^[[:space:]]*%s:[[:space:]]*[^[:space:]]", cle), lignes, value = TRUE)
  v <- trimws(sub("#.*$", "", sub(sprintf("^[[:space:]]*%s:", cle), "", m)))
  gsub("^[\"']|[\"']$", "", v)
}

defauts_identite <- function(racine) {
  chemins <- c(claude = file.path(racine, "CLAUDE.md"),
               js = file.path(racine, ".claude", "workflows", "circuit-technique.js"),
               ci = file.path(racine, ".github", "workflows", "ci.yml"))
  absents <- chemins[!file.exists(chemins)]
  if (length(absents)) return(sprintf("fichier absent : %s", absents))
  listes <- list("CLAUDE.md" = batteries_claude(chemins[["claude"]]),
                 "BATTERIES" = batteries_js(chemins[["js"]]),
                 "ci.yml" = batteries_ci(chemins[["ci"]]))
  defauts <- character(0)
  for (n in names(listes)) {
    if (!length(listes[[n]])) defauts <- c(defauts, sprintf("%s : aucune batterie lue", n))
    if (anyNA(listes[[n]])) defauts <- c(defauts, sprintf("%s : étape « Batterie : » sans run: d'une ligne", n))
  }
  if (!identical(listes[[1L]], listes[[2L]]) || !identical(listes[[1L]], listes[[3L]]))
    defauts <- c(defauts, paste0("batteries différentes :\n",
                                 paste(sprintf("      %s : %s", names(listes),
                                               vapply(listes, paste, character(1L), collapse = " | ")),
                                       collapse = "\n")))
  defauts <- c(defauts, defauts_job_tests(chemins[["ci"]]))
  for (k in names(chemins))
    if (any(grepl(MARQUE_A_ADAPTER, lire_lignes(chemins[[k]]), fixed = TRUE, useBytes = TRUE)))
      defauts <- c(defauts, sprintf("%s : marque « %s » présente", basename(chemins[[k]]), MARQUE_A_ADAPTER))
  workflows <- list.files(file.path(racine, ".github", "workflows"), pattern = "\\.ya?ml$", full.names = TRUE)
  images <- character(0); variables <- character(0)
  for (w in workflows) {
    l <- lire_lignes(w)
    images <- c(images, valeurs_cle(l, "image")); variables <- c(variables, valeurs_cle(l, "TABLEXP_IMAGE"))
  }
  if (length(unique(c(images, variables))) > 1L)
    defauts <- c(defauts, sprintf("chaînes d'image différentes : %s",
                                  paste(unique(c(images, variables)), collapse = " | ")))
  if (length(images) != length(variables))
    defauts <- c(defauts, sprintf("%d image(s) de conteneur pour %d TABLEXP_IMAGE", length(images), length(variables)))
  attr(defauts, "batteries") <- listes[[1L]]
  attr(defauts, "images") <- unique(images)
  defauts
}

principal <- function(args = commandArgs(trailingOnly = TRUE)) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  racine <- if (length(args)) args[1L] else dirname(dirname(dirname(normalizePath(script))))
  cat("Identité des batteries et de l'image :", racine, "\n")
  d <- defauts_identite(racine)
  cat(paste0("  batterie : ", attr(d, "batteries"), "\n"), sep = "")
  cat(paste0("  image    : ", attr(d, "images"), "\n"), sep = "")
  if (length(d)) {
    cat(paste0("  ECHEC : ", d, "\n"), sep = "")
    if (identical(Sys.getenv("GITHUB_ACTIONS"), "true"))
      cat("::error title=Identité des batteries::", paste(gsub("\n", " ", d), collapse = " ; "), "\n", sep = "")
    quit(save = "no", status = 1L)
  }
  cat("  OK : CLAUDE.md, BATTERIES et ci.yml identiques ; chaîne d'image unique.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
