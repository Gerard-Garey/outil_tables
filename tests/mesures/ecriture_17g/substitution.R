# Deux versions de ecrire_valeurs(), lues dans le source R/rapport.R du dépôt et recompilées
# (compiler::cmpfun) : ev0, texte inchangé ; ev1, doubles_texte() remplacée par
# function(x) octets_17g(x), dans un environnement enfant de l'espace de noms qui porte le
# prototype (fonctions recompilées, PUISSANCES_TRANCHES construite à la lecture, durée relevée).
# À sourcer après library(tablexp) ; variables attendues : racine, proto.
ns <- asNamespace("tablexp")
src <- readLines(file.path(racine, "R/rapport.R"), encoding = "UTF-8")
debut <- grep("^ecrire_valeurs <- function", src)
fin <- debut - 1L + which(src[debut:length(src)] == "}")[1L]
texte0 <- src[debut:fin]
d0 <- grep("^  doubles_texte <- function\\(x\\) \\{$", texte0)
d1 <- d0 - 1L + which(texte0[d0:length(texte0)] == "  }")[1L]
stopifnot(length(d0) == 1L, grepl("sprintf", paste(texte0[d0:d1], collapse = "")))
texte1 <- c(texte0[seq_len(d0 - 1L)], "  doubles_texte <- function(x) octets_17g(x)", texte0[(d1 + 1L):length(texte0)])
cat("ecrire_valeurs() : lignes", debut, "à", fin, "de R/rapport.R ; doubles_texte() : lignes",
    debut + d0 - 1L, "à", debut + d1 - 1L, "remplacées par une ligne\n")
proto_env <- new.env(parent = ns)
duree_table <- system.time(sys.source(proto, envir = proto_env, keep.source = FALSE))[["elapsed"]]
for (nom in ls(proto_env)) if (is.function(proto_env[[nom]])) {
  f <- compiler::cmpfun(proto_env[[nom]]); environment(f) <- proto_env; assign(nom, f, envir = proto_env)
}
ev_installee <- get("ecrire_valeurs", ns)
ev0 <- eval(parse(text = texte0, keep.source = FALSE)[[1L]][[3L]], ns); environment(ev0) <- ns
ev0 <- compiler::cmpfun(ev0)
stopifnot(identical(deparse(ev0), deparse(ev_installee)))
ev1 <- eval(parse(text = texte1, keep.source = FALSE)[[1L]][[3L]], proto_env); environment(ev1) <- proto_env
ev1 <- compiler::cmpfun(ev1)
poser <- function(f) {
  unlockBinding("ecrire_valeurs", ns); assign("ecrire_valeurs", f, envir = ns); lockBinding("ecrire_valeurs", ns)
}
