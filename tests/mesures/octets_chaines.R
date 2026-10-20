# -----------------------------------------------------------------------------
#  tests/mesures/octets_chaines.R : durée et mémoire de l'écriture des
#  chaînes en octets (octets_chaines(), R/rapport.R) et de l'empreinte des
#  données à deux rôles caractère, sur un jeu déterministe de n lignes
#  (issue #22, point 1 ; #67)
#
#  Hors des batteries (aucun seuil). Jeu sans aléa : id_contrat
#  « C%09d » (une ligne sur dix reprend le contrat de la précédente),
#  id_tete « T%09d », toutes deux avec des NA, des valeurs non ASCII
#  marquées UTF-8 et les cinq échappements. Affiche, pour chaque mesure, la
#  MD5 des octets produits (comparaison avant / après), la durée (médiane de
#  trois exécutions), le pic de mémoire au-dessus de l'état initial
#  (gc(reset = TRUE), puis gc(), colonnes « max used ») et, si R le permet,
#  le volume alloué relevé par Rprofmem() (vecteurs de plus de 128 octets ;
#  pages de petits vecteurs comptées à part).
#
#  Usage (depuis la racine du dépôt) :
#      Rscript tests/mesures/octets_chaines.R [n]
#  Sans argument : n = 1e6.
# -----------------------------------------------------------------------------

local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  harnais <- if (length(f)) file.path(dirname(dirname(normalizePath(f[1L]))), "harnais.R")
             else file.path("tests", "harnais.R")
  source(harnais, local = globalenv())
})

RACINE <- racine_depot()
cat(R.version.string, "\n")
cat(sprintf("locale LC_CTYPE : %s\n", Sys.getlocale("LC_CTYPE")))
installer_tablexp(RACINE)

ARGS <- commandArgs(trailingOnly = TRUE)
N <- if (length(ARGS)) as.numeric(ARGS[1L]) else 1e6

# Chaîne marquée UTF-8 construite depuis ses octets (aucun littéral non ASCII
# dans le script, dont le marquage dépendrait de la locale).
u8 <- function(o) { x <- rawToChar(as.raw(o)); Encoding(x) <- "UTF-8"; x }
E_AIGU <- u8(c(0xc3, 0xa9))
jeu <- function(n) {
  i <- seq_len(n)
  contrat <- i - (i %% 10L == 0L)
  d <- data.frame(id_contrat = sprintf("C%09d", contrat), id_tete = sprintf("T%09d", i),
                  stringsAsFactors = FALSE)
  na <- i %% 10007L == 0L
  ascii_non <- i %% 9973L == 0L
  echap <- i %% 99991L == 0L
  d$id_contrat[ascii_non] <- paste0(d$id_contrat[ascii_non], E_AIGU)
  d$id_tete[ascii_non] <- paste0(E_AIGU, d$id_tete[ascii_non])
  d$id_contrat[echap] <- paste0(d$id_contrat[echap], "\\\n\r\t\"")
  d$id_tete[echap] <- ""
  d$id_contrat[na] <- NA
  d$id_tete[rev(na)] <- NA
  d
}
md5_octets <- function(o) {
  f <- tempfile(); on.exit(unlink(f))
  writeBin(o, f)
  unname(tools::md5sum(f))
}
mesurer <- function(nom, expr_fn) {
  durees <- numeric(3L)
  for (k in 1:3) {
    t0 <- proc.time()[["elapsed"]]
    v <- expr_fn()
    durees[k] <- proc.time()[["elapsed"]] - t0
    rm(v)
  }
  invisible(gc())
  g0 <- gc(reset = TRUE)
  v <- expr_fn()
  g1 <- gc()
  pic <- sum(g1[, 6L]) - sum(g0[, 2L])
  profil <- if (capabilities("profmem")) {
    f <- tempfile()
    rm(v); invisible(gc())
    utils::Rprofmem(f, threshold = 128)
    v <- expr_fn()
    utils::Rprofmem(NULL)
    l <- readLines(f, warn = FALSE); unlink(f)
    l <- unlist(strsplit(l, "(?<=\\S) (?=[0-9]+ ?:|new page)", perl = TRUE))
    octets <- suppressWarnings(as.numeric(sub("^([0-9]+) ?:.*", "\\1", grep("^[0-9]+ ?:", l, value = TRUE))))
    sprintf("Rprofmem : %.1f Mo en %d vecteurs > 128 octets, %d pages de petits vecteurs",
            sum(octets) / 2^20, length(octets), sum(grepl("^new page", l)))
  } else "Rprofmem indisponible"
  empreinte <- if (is.raw(v)) md5_octets(v) else v
  cat(sprintf("  %s : médiane %.2f s (%s), pic mémoire %.1f Mo ; %s ; %s\n", nom, stats::median(durees),
              paste(sprintf("%.2f", durees), collapse = " / "), pic, profil,
              if (is.raw(v)) sprintf("%.0f octets, md5 %s", length(v), empreinte) else sprintf("empreinte %s", v)))
}

D <- jeu(N)
cat(sprintf("\nn = %.0f lignes : %d id_contrat distincts, %d id_tete distincts, %d NA par colonne\n",
            N, length(unique(D$id_contrat)), length(unique(D$id_tete)), sum(is.na(D$id_contrat))))
ROLES <- c(id_contrat = "id_contrat", id_tete = "id_tete")
octets_chaines <- tablexp:::octets_chaines
mesurer("octets_chaines(id_contrat)", function() octets_chaines(D$id_contrat, "id_contrat"))
mesurer("octets_chaines(id_tete)", function() octets_chaines(D$id_tete, "id_tete"))
mesurer("empreinte_donnees(id_contrat, id_tete)", function() tablexp:::empreinte_donnees(D, ROLES))
verifier(sprintf("mesure n = %.0f exécutée", N), TRUE)
bilan()
