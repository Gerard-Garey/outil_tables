# -----------------------------------------------------------------------------
#  tests/unitaires/test_controles_portables.R : utilitaire commun des scripts
#  de tests/controles/ (tests/controles/orchestration_locale.R, issue #117).
#  Cas bon marché (moins de deux secondes) : le contrôle complet de chaque
#  locale est une étape de la CI. Nature des attendus : valeurs exactes.
# -----------------------------------------------------------------------------

CONTROLES_ <- file.path(RACINE, "tests", "controles")
charger_ <- function(script, env) {
  for (e in suppressWarnings(parse(file.path(CONTROLES_, script), encoding = "UTF-8", keep.source = FALSE))) eval(e, env)
  env
}
orch <- charger_("orchestration_locale.R", new.env())

verifier("orchestration : l'utilitaire ne définit que les cinq fonctions communes",
         identical(sort(ls(orch)), c("avec_environnement", "duree", "ecart_octets", "executer", "installer")))

# --- avec_environnement ---------------------------------------------------------

noms_ <- c("TABLEXP_ORCH_ABSENTE", "TABLEXP_ORCH_PRESENTE")
Sys.unsetenv(noms_[1L])
Sys.setenv(TABLEXP_ORCH_PRESENTE = "avant")
vu_ <- orch$avec_environnement(c(TABLEXP_ORCH_ABSENTE = "x", TABLEXP_ORCH_PRESENTE = "y"),
                               c(Sys.getenv(noms_[1L]), Sys.getenv(noms_[2L])))
verifier("avec_environnement : variables posées pendant l'appel, valeur de l'expression rendue",
         identical(vu_, c("x", "y")))
verifier("avec_environnement : variable absente supprimée, variable présente restaurée",
         identical(Sys.getenv(noms_[1L], unset = NA), NA_character_) &&
           identical(Sys.getenv(noms_[2L], unset = NA), "avant"))
r_ <- tryCatch(orch$avec_environnement(c(TABLEXP_ORCH_ABSENTE = "x", TABLEXP_ORCH_PRESENTE = "y"), stop("boum")),
               error = function(e) conditionMessage(e))
verifier("avec_environnement : restauration aussi après une erreur de l'expression",
         identical(r_, "boum") && identical(Sys.getenv(noms_[1L], unset = NA), NA_character_) &&
           identical(Sys.getenv(noms_[2L], unset = NA), "avant"))
Sys.unsetenv(noms_)

# --- ecart_octets, duree ----------------------------------------------------------

verifier("ecart_octets : premier octet différent et ligne ; préfixe commun",
         identical(orch$ecart_octets(charToRaw("ab\ncd\n"), charToRaw("ab\nXd\n")),
                   "longueurs 6 et 6 octets ; premier octet différent : 4 (ligne 2)") &&
           identical(orch$ecart_octets(charToRaw("ab\ncd"), charToRaw("ab\ncd\nef")),
                     "longueurs 5 et 8 octets ; premier octet différent : 6 (ligne 2)"))
t0_ <- Sys.time() - 2.5
verifier("duree : texte « n.n s » (2,5 s écoulées au moins)", grepl("^2\\.[5-9] s$", orch$duree(t0_)))

# --- executer, installer : statut de sortie et environnement transmis -------------

script_ <- tempfile("orch_", fileext = ".R")
writeLines(c("a <- commandArgs(trailingOnly = TRUE)",
             "cat('TABLEXP_ORCH=', Sys.getenv('TABLEXP_ORCH'), ' args=', length(a), '\\n', sep = '')",
             "if (identical(a[3L], 'echec')) quit(save = 'no', status = 3L)"), script_)
ex_ok <- orch$executer(script_, "racine", "lib", c(TABLEXP_ORCH = "oui"), "attendu", "sortie")
ex_ko <- orch$executer(script_, "racine", "lib", c(TABLEXP_ORCH = "oui"), "echec", "sortie")
verifier("executer : sous-processus lancé sous l'environnement donné (--travailleur et 5 arguments), ok = TRUE",
         isTRUE(ex_ok$ok) && identical(as.vector(ex_ok$sortie), "TABLEXP_ORCH=oui args=5") &&
           grepl("^[0-9]+\\.[0-9] s$", ex_ok$duree))
verifier("executer : statut de sortie non nul -> ok = FALSE",
         identical(ex_ko$ok, FALSE) && identical(attr(ex_ko$sortie, "status"), 3L))
unlink(script_)

sans_package_ <- tempfile("orch_racine_"); dir.create(sans_package_)
lib_ <- tempfile("orch_lib_"); dir.create(lib_)
ins_ <- orch$installer(sans_package_, lib_, c(LC_ALL = "C"))
verifier("installer : racine sans package -> ok = FALSE, sortie et durée rendues",
         identical(ins_$ok, FALSE) && length(ins_$sortie) > 0L && grepl("^[0-9]+\\.[0-9] s$", ins_$duree))
unlink(c(sans_package_, lib_), recursive = TRUE)

# --- sourçage par les deux contrôles de locale -----------------------------------
# Les contrôles, lus comme le fait tests/unitaires/test_controles.R (parse en
# UTF-8, eval() dans un environnement), retrouvent l'utilitaire (mêmes
# fonctions, signatures à env = vecteur d'environnement complet). La
# comparaison de corps ne voit pas une recopie mot pour mot : le texte des
# contrôles est donc lu aussi, qui ne doit définir aucune des cinq fonctions.

for (script_ in c("locale_c.R", "locale_latin1.R")) {
  e_ <- charger_(script_, new.env())
  verifier(sprintf("%s : orchestration chargée depuis l'utilitaire (mêmes fonctions, signatures à env)", script_),
           all(vapply(names(orch), function(n) identical(body(e_[[n]]), body(orch[[n]])) &&
                        identical(formals(e_[[n]]), formals(orch[[n]])), TRUE)) &&
             identical(names(formals(e_$installer)), c("racine", "lib", "env")) &&
             identical(names(formals(e_$executer)), c("script", "racine", "lib", "env", "attendu", "sortie")))
  l_ <- readLines(file.path(CONTROLES_, script_), encoding = "UTF-8")
  redefinies_ <- grep("^(avec_environnement|duree|installer|executer|ecart_octets) *<- *function", l_, value = TRUE)
  verifier(sprintf("%s : aucune des cinq fonctions communes n'est redéfinie dans le contrôle", script_),
           if (length(redefinies_)) paste("redéfinies :", paste(redefinies_, collapse = " | ")) else TRUE)
}

# Un orchestration_locale.R homonyme à côté du script lanceur (ici D/, où
# tests/test_unitaires.R sourcerait le contrôle) n'est pas chargé : seul le
# dossier qui contient aussi le contrôle est retenu (D/controles/) ; le lanceur
# s'exécute hors du dépôt (le repli sur tests/controles du répertoire courant
# ne joue pas). Sans
# utilitaire à côté du contrôle, le chargement refuse (aucun repli sur
# l'homonyme).
for (script_ in c("locale_c.R", "locale_latin1.R")) {
  d_ <- tempfile("homonyme_"); dir.create(file.path(d_, "controles"), recursive = TRUE)
  file.copy(file.path(CONTROLES_, c(script_, "orchestration_locale.R")), file.path(d_, "controles"))
  writeLines('installer <- function(...) "FAUX"', file.path(d_, "orchestration_locale.R"))
  lanceur_ <- file.path(d_, "lanceur.R")
  writeLines(c("setwd(tempdir())  # hors du dépôt : pas de repli sur tests/controles du répertoire courant",
               sprintf('e <- new.env(); for (x in parse(file.path(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))), "controles", "%s"), encoding = "UTF-8")) eval(x, e)', script_),
               'cat("installer:", names(formals(e$installer)), "\n")'), lanceur_)
  r_ <- executer_rscript(lanceur_)
  verifier(sprintf("%s : homonyme à côté du script lanceur non chargé (installer à env, pas « FAUX »)", script_),
           identical(r_$statut, 0L) && any(r_$sortie == "installer: racine lib env "))
  file.remove(file.path(d_, "controles", "orchestration_locale.R"))
  r_ <- executer_rscript(lanceur_)
  verifier(sprintf("%s : sans utilitaire à côté du contrôle, refus nommé (pas de repli sur l'homonyme)", script_),
           r_$statut != 0L && any(grepl("orchestration_locale.R introuvable", r_$sortie, fixed = TRUE)) &&
             any(grepl(script_, r_$sortie, fixed = TRUE)))
  unlink(d_, recursive = TRUE)
}
