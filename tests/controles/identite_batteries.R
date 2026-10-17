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
#         `container:` ; aucune ligne `continue-on-error:` n'y figure
#         (niveau du job ou d'une étape, quelle qu'elle soit) ; aucun
#         `if:` au niveau du job ; chaque
#         étape de batterie a un `if:` absent ou égal à
#         `${{ !cancelled() }}` (écrit tel quel, sans guillemets : toute
#         autre condition, dont `false`, pourrait sauter la batterie sans
#         faire échouer le job) ;
#       aucune des trois listes n'est vide ;
#    2. la marque « À ADAPTER » est absente de ces trois fichiers ;
#    3. dans .github/workflows/*.yml, toutes les valeurs `image:` et
#       `TABLEXP_IMAGE:` sont une même chaîne, et il y a autant de
#       TABLEXP_IMAGE que d'image (ADR 0008, point 1 : chaîne unique) ;
#    4. workflow de régénération .github/workflows/references.yml (ADR 0009,
#       points 5.1 et 6.1 ; ADR 0008, point 8) : le fichier existe ; son
#       seul déclencheur est `workflow_dispatch` ; une seule clé
#       `permissions:`, au niveau du fichier, qui vaut `contents: read` ;
#       aucun secret (`secrets.`, `github.token`) ; aucun
#       `continue-on-error` ; un job unique, qui déclare `container:`,
#       `timeout-minutes:`, `shell: bash` et OPENBLAS_NUM_THREADS et
#       OMP_NUM_THREADS à "1" ; le bloc `run: |` de l'étape « BLAS et LAPACK
#       de référence » est identique, ligne pour ligne, à celui du job
#       `tests:` de ci.yml ; cette étape précède l'étape de plateforme
#       (ligne `Rscript tests/controles/plateforme.R --exiger`), qui précède
#       l'étape de production (ligne `Rscript
#       tests/reference/produire_references.R`), les trois sans `if:` ;
#       chaque `uses:` est actions/checkout ou actions/upload-artifact,
#       épinglé par SHA, au SHA unique de la même action dans ci.yml ;
#       `persist-credentials: false` est présent ; aucun `run:` n'interpole
#       `${{ inputs.` ni `${{ github.event.` (injection : les entrées passent
#       par `env:`).
#
#  Limites : lecture ligne à ligne, sans analyseur YAML ni JavaScript (R de
#  base) ; un bloc `run: |` est lu par run_etape() (lignes plus indentées
#  que la clé, indentation commune retirée, lignes vides finales omises) ;
#  les étapes « Batterie : » restent des `run:` d'une ligne ; un `image:`
#  écrit hors d'une ligne propre n'est pas vu ; les formes YAML de flux
#  (`on: [push]`, `{...}`) ne sont pas analysées, mais toute valeur de
#  `on:` sur la même ligne autre que `workflow_dispatch` est un écart.
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

# Condition admise sur une étape « Batterie : » (absente, ou celle-ci).
CONDITION_BATTERIE <- "${{ !cancelled() }}"

# Écarts propres au job de tests : job absent, sans `container:`,
# `if:` au niveau du job, continue-on-error n'importe où dans le job, étape
# de batterie dont le
# `if:` n'est ni absent ni CONDITION_BATTERIE.
defauts_job_tests <- function(chemin) {
  lj <- lignes_job(lire_lignes(chemin))
  if (!length(lj)) return(sprintf("ci.yml : job `%s:` absent", JOB_TESTS))
  d <- character(0)
  if (!any(grepl("^    container:", lj)))
    d <- c(d, sprintf("ci.yml : job `%s:` sans `container:` (ADR 0008, point 1)", JOB_TESTS))
  # Une condition au niveau du job peut le sauter entièrement (constat m4).
  for (l in grep("^    if[[:space:]]*:", lj, value = TRUE))
    d <- c(d, sprintf("ci.yml : condition interdite au niveau du job `%s:` (%s)", JOB_TESTS, trimws(l)))
  for (l in grep("^[[:space:]]*(- )?continue-on-error[[:space:]]*:", lj, value = TRUE))
    d <- c(d, sprintf("ci.yml : continue-on-error interdit dans le job `%s:` (%s)", JOB_TESTS, trimws(l)))
  for (e in etapes_job(lj)) {
    if (!grepl("Batterie :", e[1L], fixed = TRUE)) next
    conditions <- trimws(sub("^[[:space:]]*(- )?if[[:space:]]*:", "",
                             grep("^[[:space:]]*(- )?if[[:space:]]*:", e, value = TRUE)))
    if (length(conditions) > 1L || (length(conditions) == 1L && !identical(conditions, CONDITION_BATTERIE)))
      d <- c(d, sprintf("ci.yml : condition `if: %s` interdite sur l'étape %s (admis : absente ou `if: %s`)",
                        paste(conditions, collapse = " / "), trimws(e[1L]), CONDITION_BATTERIE))
  }
  d
}

valeurs_cle <- function(lignes, cle) {
  m <- grep(sprintf("^[[:space:]]*%s:[[:space:]]*[^[:space:]]", cle), lignes, value = TRUE)
  v <- trimws(sub("#.*$", "", sub(sprintf("^[[:space:]]*%s:", cle), "", m)))
  gsub("^[\"']|[\"']$", "", v)
}

# --- contrôle 4 : workflow de régénération (ADR 0009, point 6.1) ------------

WORKFLOW_REFERENCES <- "references.yml"
ETAPE_BASCULE <- "BLAS et LAPACK de r\u00e9f\u00e9rence"
COMMANDE_PLATEFORME <- "Rscript tests/controles/plateforme.R --exiger"
COMMANDE_PRODUCTION <- "Rscript tests/reference/produire_references.R"
ACTIONS_ADMISES <- c("actions/checkout", "actions/upload-artifact")

indentation <- function(l) nchar(l) - nchar(sub("^ +", "", l))

# Commande d'une étape : valeur d'un `run:` d'une ligne, ou lignes d'un bloc
# `run: |` (plus indentées que la clé, indentation commune retirée, lignes
# vides finales omises) ; NULL sans `run:`.
run_etape <- function(e) {
  i <- grep("^[[:space:]]*(- )?run:", e)
  if (length(i) != 1L) return(NULL)
  valeur <- trimws(sub("^[[:space:]]*(- )?run:", "", e[i]))
  if (!valeur %in% c("|", "|-", "|+")) return(valeur)
  cle <- indentation(sub("- ", "  ", e[i], fixed = TRUE))
  suite <- e[seq.int(i + 1L, length.out = length(e) - i)]
  dedans <- !nzchar(trimws(suite)) | indentation(suite) > cle
  n <- if (all(dedans)) length(suite) else which(!dedans)[1L] - 1L
  bloc <- suite[seq_len(n)]
  while (length(bloc) && !nzchar(trimws(bloc[length(bloc)]))) bloc <- bloc[-length(bloc)]
  if (!length(bloc)) return(character(0))
  marge <- min(indentation(bloc[nzchar(trimws(bloc))]))
  substring(bloc, marge + 1L)
}

nom_etape <- function(e) {
  n <- grep("^[[:space:]]*(- )?name:", e, value = TRUE)
  if (!length(n)) return(NA_character_)
  gsub("^[\"']|[\"']$", "", trimws(sub("^[[:space:]]*(- )?name:", "", n[1L])))
}

# Égalité octet par octet (lignes lues sans conversion, constante en UTF-8 :
# verdict indépendant de la locale).
egal_octets <- function(a, b) !is.na(a) && identical(charToRaw(a), charToRaw(enc2utf8(b)))

a_condition <- function(e) any(grepl("^[[:space:]]*(- )?if[[:space:]]*:", e))

# Clés de premier niveau sous la ligne `cle:` de niveau 0 (indentation 2).
sous_cles <- function(l, cle) {
  debut <- which(l == sprintf("%s:", cle))
  if (length(debut) != 1L) return(NULL)
  suite <- l[seq.int(debut + 1L, length.out = length(l) - debut)]
  fin <- which(grepl("^[^[:space:]#]", suite))
  if (length(fin)) suite <- suite[seq_len(fin[1L] - 1L)]
  sub(":.*$", "", trimws(grep("^  [A-Za-z0-9_-]+:", suite, value = TRUE)))
}

sha_actions <- function(l) {
  u <- trimws(sub("#.*$", "", sub("^[[:space:]]*(- )?uses:", "", grep("^[[:space:]]*(- )?uses:", l, value = TRUE))))
  data.frame(uses = u, action = sub("@.*$", "", u),
             sha = ifelse(grepl("@[0-9a-f]{40}$", u), sub("^.*@", "", u), NA_character_), stringsAsFactors = FALSE)
}

defauts_references <- function(racine) {
  w <- file.path(racine, ".github", "workflows", WORKFLOW_REFERENCES)
  ci <- file.path(racine, ".github", "workflows", "ci.yml")
  if (!file.exists(w)) return(sprintf("%s absent (ADR 0009, point 5)", WORKFLOW_REFERENCES))
  l <- lire_lignes(w); lci <- lire_lignes(ci)
  d <- character(0); ecart <- function(m) d <<- c(d, sprintf("%s : %s", WORKFLOW_REFERENCES, m))
  # Déclencheur.
  on_ <- grep("^\"?on\"?:", l, value = TRUE)
  if (length(on_) != 1L) ecart("une seule clé `on:` attendue")
  else {
    v <- trimws(sub("#.*$", "", sub("^\"?on\"?:", "", on_)))
    declencheurs <- if (nzchar(v)) v else sous_cles(l, "on")
    if (!identical(declencheurs, "workflow_dispatch"))
      ecart(sprintf("déclencheur(s) %s au lieu de workflow_dispatch seul", paste(declencheurs, collapse = ", ")))
  }
  # Permissions.
  p <- grep("^[[:space:]]*permissions:", l, value = TRUE)
  if (!identical(p, "permissions:") || !identical(sous_cles(l, "permissions"), "contents") ||
      !any(grepl("^  contents:[[:space:]]*read[[:space:]]*$", l)))
    ecart("`permissions:` unique, au niveau du fichier, égal à `contents: read` attendu")
  if (any(grepl("secrets[.[]|github\\.token", l))) ecart("secret employé")
  if (any(grepl("continue-on-error", l, fixed = TRUE))) ecart("continue-on-error interdit")
  if (!any(grepl("^[[:space:]]*persist-credentials:[[:space:]]*false[[:space:]]*$", l)))
    ecart("persist-credentials: false absent")
  # Job unique.
  jobs <- sous_cles(l, "jobs")
  if (length(jobs) != 1L) { ecart(sprintf("%d job(s) au lieu d'un", length(jobs))); return(d) }
  lj <- lignes_job(l, jobs)
  if (!any(grepl("^    container:", lj))) ecart(sprintf("job `%s:` sans `container:`", jobs))
  if (!any(grepl("^    timeout-minutes:", lj))) ecart(sprintf("job `%s:` sans `timeout-minutes:`", jobs))
  if (!any(grepl("^[[:space:]]+shell:[[:space:]]*bash[[:space:]]*$", lj))) ecart("`shell: bash` absent")
  for (v in c("OPENBLAS_NUM_THREADS", "OMP_NUM_THREADS"))
    if (!identical(valeurs_cle(lj, v), "1")) ecart(sprintf("%s : %s au lieu de \"1\"", v, paste(valeurs_cle(lj, v), collapse = ", ")))
  # Étapes : bascule, plateforme, production.
  etapes <- etapes_job(lj)
  noms <- vapply(etapes, nom_etape, "")
  runs <- lapply(etapes, run_etape)
  contient <- function(cmd) which(vapply(runs, function(r) any(startsWith(trimws(r), cmd)), NA))
  i_b <- which(vapply(noms, egal_octets, NA, ETAPE_BASCULE)); i_p <- contient(COMMANDE_PLATEFORME); i_r <- contient(COMMANDE_PRODUCTION)
  etapes_ci <- etapes_job(lignes_job(lci))
  b_ci <- Filter(function(e) egal_octets(nom_etape(e), ETAPE_BASCULE), etapes_ci)
  if (length(i_b) != 1L || length(b_ci) != 1L)
    ecart(sprintf("étape « %s » absente ou en double (ici ou dans le job `tests:` de ci.yml)", ETAPE_BASCULE))
  else {
    a <- runs[[i_b]]; b <- run_etape(b_ci[[1L]])
    if (!identical(a, b)) {
      k <- which(c(a, rep(NA, max(0L, length(b) - length(a)))) != c(b, rep(NA, max(0L, length(a) - length(b)))) |
                 is.na(c(a, rep(NA, max(0L, length(b) - length(a))))) != is.na(c(b, rep(NA, max(0L, length(a) - length(b))))))[1L]
      ecart(sprintf("bloc run: de l'étape « %s » différent de celui de ci.yml (ligne %s du bloc : « %s » au lieu de « %s »)",
                    ETAPE_BASCULE, k, a[k], b[k]))
    }
  }
  for (k in seq_along(runs))
    if (any(grepl("\\$\\{\\{[[:space:]]*(inputs|github\\.event)\\.", runs[[k]])))
      ecart(sprintf("interpolation d'entrée dans un run: (étape « %s ») : passer par env:", noms[k]))
  if (length(i_p) != 1L) ecart(sprintf("étape de plateforme (`%s`) absente ou en double", COMMANDE_PLATEFORME))
  if (length(i_r) != 1L) ecart(sprintf("étape de production (`%s`) absente ou en double", COMMANDE_PRODUCTION))
  if (length(i_b) == 1L && length(i_p) == 1L && length(i_r) == 1L) {
    if (!(i_b < i_p && i_p < i_r))
      ecart("ordre attendu : bascule BLAS et LAPACK, puis plateforme en mode strict, puis production")
    for (i in c(i_b, i_p, i_r))
      if (a_condition(etapes[[i]])) ecart(sprintf("condition `if:` interdite sur l'étape « %s »", noms[i]))
  }
  # Actions épinglées aux SHA de ci.yml.
  ua <- sha_actions(l); uci <- sha_actions(lci)
  for (k in seq_len(nrow(ua))) {
    ref <- unique(uci$sha[uci$action == ua$action[k]])
    if (!ua$action[k] %in% ACTIONS_ADMISES) ecart(sprintf("action non admise : %s", ua$uses[k]))
    else if (is.na(ua$sha[k])) ecart(sprintf("action non épinglée par SHA : %s", ua$uses[k]))
    else if (length(ref) != 1L || !identical(ua$sha[k], ref))
      ecart(sprintf("SHA de %s différent de celui de ci.yml (%s)", ua$uses[k], paste(ref, collapse = ", ")))
  }
  d
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
  defauts <- c(defauts, defauts_references(racine))
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
  cat("  OK : CLAUDE.md, BATTERIES et ci.yml identiques ; chaîne d'image unique ; references.yml conforme.\n")
  invisible(TRUE)
}

if (sys.nframe() == 0L) principal()
