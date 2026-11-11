#!/usr/bin/env bash
###############################################################################
#  .claude/outils/branche_de_travail_temoins.sh  --  temoins de
#  .claude/outils/branche_de_travail.sh (issue #137, critere d'acceptation 1)
#
#  Simule l'API GitHub (fichier JSON de PR, BRANCHE_TRAVAIL_PRS_JSON ; gh et
#  curl factices pour le repli et la page pleine), la feuille de route
#  (BRANCHE_TRAVAIL_FEUILLE) et un depot git temporaire
#  (BRANCHE_TRAVAIL_RACINE) ; n'ecrit que sous un repertoire temporaire,
#  supprime en sortie. Chaque temoin donne le code de sortie et un extrait
#  du message attendus ; code de sortie non nul si un temoin echoue.
#
#  Usage : bash .claude/outils/branche_de_travail_temoins.sh
###############################################################################
set -uo pipefail
script="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/branche_de_travail.sh"
tmp="$(mktemp -d "${TMPDIR:-/tmp}/temoins_branche.XXXXXX")" || exit 2
trap 'rm -rf "$tmp"' EXIT

# ---- depot git temporaire ----------------------------------------------------
depot="$tmp/depot"; mkdir -p "$depot"
g() { git -C "$depot" -c user.name=temoin -c user.email=temoin@example.org -c commit.gpgsign=false "$@"; }
g init -q; g checkout -q -b main
mkdir -p "$depot/R" "$depot/tests" "$depot/docs/latex" "$depot/docs/adr"
echo base > "$depot/R/a.R"; echo base > "$depot/tests/t.R"; echo base > "$depot/docs/latex/doc_outil_tables.tex"
g add -A; g commit -q -m base
# b1 : R/, un test propre, le fichier partage .tex
g checkout -q -b claude/b1 main
echo 1 > "$depot/R/b1.R"; echo 1 > "$depot/tests/test_b1.R"; echo 1 >> "$depot/docs/latex/doc_outil_tables.tex"
g add -A; g commit -q -m b1
# b2 : un test propre, le fichier partage .tex (autre section)
g checkout -q -b claude/b2 main
echo 2 > "$depot/tests/test_b2.R"; echo 2 >> "$depot/docs/latex/doc_outil_tables.tex"
g add -A; g commit -q -m b2
# b3 : R/b1.R comme b1 (intersection), tests/commun.R comme b5, README.md (hors perimetre)
g checkout -q -b claude/b3 main
echo 3 > "$depot/R/b1.R"; echo 3 > "$depot/README.md"; echo 3 > "$depot/tests/commun.R"
g add -A; g commit -q -m b3
# b4 : tests/test_b4.R, un ADR, NAMESPACE (hors perimetre)
g checkout -q -b claude/b4 main
echo 4 > "$depot/tests/test_b4.R"; echo 4 > "$depot/docs/adr/0099.md"; echo 4 > "$depot/NAMESPACE"
g add -A; g commit -q -m b4
# b5 : tests/commun.R comme b3
g checkout -q -b claude/b5 main
echo 5 > "$depot/tests/commun.R"
g add -A; g commit -q -m b5
# b6 : local PERIME par rapport a origin/claude/b6 (origin porte le commit, local reste a main)
g checkout -q -b claude/b6 main
echo 6 > "$depot/tests/test_b6.R"
g add -A; g commit -q -m b6
g update-ref refs/remotes/origin/claude/b6 claude/b6
g checkout -q main
g branch -q -f claude/b6 main

# ---- JSON de PR et feuilles de route simules --------------------------------
pr() {  # pr <numero> <branche> <brouillon true|false> ; objet au format de l'API
  printf '{"number":%s,"state":"open","draft":%s,"base":{"ref":"main"},"head":{"ref":"%s","repo":{"full_name":"Gerard-Garey/outil_tables"}}}' "$1" "$3" "$2"
}
json() { printf '[%s]' "$(IFS=,; echo "$*")"; }
prs_brouillons() {  # prs_brouillons <n1> <n2> ... : PR brouillon claude/b<n>, numero n
  l=""; for k in "$@"; do l="$l${l:+,}$(pr "$k" "claude/b$k" true)"; done; printf '[%s]' "$l"
}
MARQUEUR='Vague autorisée : `vague 9`, 11/11/2026'

feuille_sans_tableau() {
  printf '# Feuille de route\n\n## 1. Branche de travail en cours\n\nAucune vague autorisée.\n\n### 1.1 Une branche\n\n| Branche | PR |\n|---|---|\n| `claude/b9` | #9 |\n\n## 2. Suite\n'
}
feuille_vague_prose() {   # forme de la vague 2 : colonne de partition en prose
  printf '# F\n\n## 1. Branche de travail en cours\n\n%s\n\n| Branche | PR | Rôle | Partition (fichiers que la branche peut modifier) | Ordre de fusion |\n|---|---|---|---|---|\n| `claude/b1` (B1) | #1 | porteuse | `R/` ; `tests/` | 2 |\n| `claude/b2` (P2) | #2 | parallèle | `tests/test_b2.R` | 1 |\n\n### 1.1 X\n' "$MARQUEUR"
}
tableau_motifs() {  # $1 = motifs de b1, $2 = motifs de b2, $3 = motifs de b4 (defaut : tests/test_b4.R)
  b4m=${3:-'`tests/test_b4.R`'}
  printf '| Branche | PR | Partition (motifs) | Ordre |\n|---|---|---|---|\n| `claude/b1` (B1) | #1 | %s | 2 |\n| `claude/b2` (P2) | #2 | %s | 1 |\n| `claude/b3` (P3) | #3 | `R/*`, `tests/commun.R` | 3 |\n| `claude/b4` (P4) | #4 | %s | 4 |\n| `claude/b5` (P5) | #5 | `tests/commun.R` | 5 |\n| `claude/b6` (P6) | #6 | `tests/test_b6.R` | 6 |\n' "$1" "$2" "$b4m"
}
feuille_vague_motifs() {  # avec marqueur de vague
  printf '# F\n\n## 1. Branche de travail en cours\n\n%s\n\n%s\n### 1.1 X\n' "$MARQUEUR" "$(tableau_motifs "$@")"
}
feuille_motifs_sans_marqueur() {
  printf '# F\n\n## 1. Branche de travail en cours\n\n%s\n\n### 1.1 X\n' "$(tableau_motifs "$@")"
}

# ---- harnais -----------------------------------------------------------------
echec=0; n=0
temoin() {  # temoin <libelle> <code attendu> <extrait attendu du message ou -> <arguments du script>
  libelle="$1"; attendu="$2"; extrait="$3"; shift 3
  n=$((n + 1))
  sortie="$(PATH="${PATH_TEMOIN:-$PATH}" BRANCHE_TRAVAIL_RACINE="$depot" BRANCHE_TRAVAIL_FEUILLE="$FEUILLE" BRANCHE_TRAVAIL_PRS_JSON="$PRS" \
    bash "$script" "$@" 2>&1)"; code=$?
  if [ "$code" -eq "$attendu" ] && { [ "$extrait" = "-" ] || printf '%s' "$sortie" | grep -Fq -- "$extrait"; }; then
    printf 'OK    %2d  %s (code %s)\n' "$n" "$libelle" "$code"
  else
    printf 'ECHEC %2d  %s : code %s (attendu %s), extrait attendu « %s »\n' "$n" "$libelle" "$code" "$attendu" "$extrait"
    printf '%s\n' "$sortie" | sed 's/^/        | /'
    echec=$((echec + 1))
  fi
}

PRS="$tmp/prs.json"; FEUILLE="$tmp/feuille.md"

echo "== Contrôles 1 et 2 : hors vague =="
feuille_sans_tableau > "$FEUILLE"
prs_brouillons 1 > "$PRS"
temoin "hors vague, une seule PR brouillon claude/ : conforme" 0 "Conforme" claude/b1
temoin "branche absente des PR ouvertes" 1 "n'est la tête d'aucune PR ouverte" claude/inconnue
json "$(pr 1 claude/b1 false)" > "$PRS"
temoin "PR non brouillon, sans --avant-fusion" 1 "n'est pas un brouillon" claude/b1
temoin "PR non brouillon, avec --avant-fusion : conforme" 0 "admis par --avant-fusion" --avant-fusion claude/b1
prs_brouillons 1 2 > "$PRS"
temoin "plusieurs PR brouillon sans vague déclarée" 1 "sans vague déclarée" claude/b1
json "$(pr 1 claude/b1 true)" "$(pr 2 claude/b2 false)" > "$PRS"
temoin "une PR brouillon et une PR non brouillon, hors vague : conforme" 0 "Conforme" claude/b1
json "$(pr 1 feature/x true)" > "$PRS"
temoin "hors vague, branche ne commençant pas par claude/" 1 "doit commencer par claude/" feature/x
feuille_motifs_sans_marqueur '`R/`' '`tests/test_b2.R`' > "$FEUILLE"
prs_brouillons 1 > "$PRS"
temoin "tableau sans marqueur de vague : hors vague (tableau ignoré)" 0 "le tableau présent est ignoré" claude/b1
prs_brouillons 1 2 > "$PRS"
temoin "tableau sans marqueur, deux PR brouillon : écart (le tableau ne déclare pas de vague)" 1 "sans vague déclarée" claude/b1

echo "== Contrôles 1 et 2 : vague =="
feuille_vague_prose > "$FEUILLE"
prs_brouillons 1 2 > "$PRS"
temoin "vague : branche au tableau, PR brouillon : conforme" 0 "Vague déclarée (« Vague autorisée : vague 9, 11/11/2026 »" claude/b1
prs_brouillons 1 2 3 > "$PRS"
temoin "vague : branche de travail absente du tableau" 1 "la branche « claude/b3 » ne figure pas" claude/b3
prs_brouillons 1 2 5 > "$PRS"
temoin "vague : PR brouillon claude/ hors tableau (b5) : écart" 1 "absente(s) du tableau des branches ouvertes : « claude/b5 » (PR #5)" claude/b1
json "$(pr 1 claude/b1 true)" "$(pr 2 claude/b2 true)" "$(pr 5 claude/b5 false)" > "$PRS"
temoin "vague : PR non brouillon hors tableau : pas d'écart" 0 "Conforme" claude/b1
prs_brouillons 1 2 > "$PRS"
printf '# F\n\n## 1. Branche de travail en cours\n\n%s\n\n| Branche | PR |\n|---|---|\n| `claude/b1` | #1 |\n\n| Branche | PR |\n|---|---|\n| `claude/b2` | #2 |\n\n### 1.1 X\n' "$MARQUEUR" > "$FEUILLE"
temoin "deux tableaux : seul le premier est lu (b2 absente)" 1 "« claude/b2 » (PR #2)" claude/b1
printf '# F\n\n## 1. Branche de travail en cours\n\nVague autorisée : sans nom\n\n### 1.1 X\n' > "$FEUILLE"
temoin "marqueur de vague sans « nom, date »" 1 "marqueur de vague sans" claude/b1
printf '# F\n\n## 1. Branche de travail en cours\n\n%s\n\n### 1.1 X\n' "$MARQUEUR" > "$FEUILLE"
temoin "marqueur de vague sans tableau lisible" 1 "aucun tableau « Branche » lisible" claude/b1

echo "== Contrôle 3 : partitions =="
prs_brouillons 1 2 > "$PRS"
feuille_vague_prose > "$FEUILLE"
temoin "colonne de partition absente : contrôle sauté ; intersection vide (.tex partagé exclu)" 0 "SAUTÉ" claude/b1 --partition claude/b1 claude/b2
prs_brouillons 1 2 3 4 5 6 > "$PRS"
feuille_vague_motifs '`R/`, `tests/test_b1.R`' '`tests/test_b2.R`' > "$FEUILLE"
temoin "partition déclarée respectée, fichier partagé hors partition admis" 0 "aucun fichier hors de sa partition" claude/b1 --partition claude/b1 claude/b2
temoin "base de repli : avertissement main local" 0 "repli sur la branche main LOCALE" claude/b1 --partition claude/b1 claude/b2
temoin "sortie : périmètre R/, tests/, docs/adr/ annoncé" 0 "les autres sont ignorés" claude/b1 --partition claude/b1 claude/b2
temoin "intersection de partitions non vide" 1 "modifient les mêmes fichiers (hors fichiers partagés) : R/b1.R" claude/b1 --partition claude/b1 claude/b3
temoin "fichier hors partition déclarée (docs/adr/, NAMESPACE hors périmètre non cité)" 1 "hors de sa partition déclarée (tests/test_b4.R) : docs/adr/0099.md." claude/b4 --partition claude/b4 claude/b2
feuille_vague_motifs '`R/`, `tests/test_b1.R`' '`tests/test_b2.R`' '`tests/test_b4.R`, `docs/adr/`' > "$FEUILLE"
temoin "fichier hors R/ tests/ docs/adr/ (NAMESPACE) ignoré : conforme" 0 "3 fichier(s) modifié(s), 1 hors R/, tests/, docs/adr/ ignoré(s)" claude/b4 --partition claude/b4 claude/b2
feuille_vague_motifs '`R/`' '`tests/test_b2.R`' > "$FEUILLE"
temoin "fichier hors partition (b1 déclare R/ seulement : tests/test_b1.R)" 1 "hors de sa partition déclarée (R/) : tests/test_b1.R" claude/b1 --partition claude/b1 claude/b2
feuille_vague_motifs '' '`tests/test_b2.R`' > "$FEUILLE"
temoin "cellule de partition vide" 1 "partition non déclarée" claude/b1 --partition claude/b1 claude/b2
feuille_vague_motifs '`R/`, `tests/test_b1.R`' '`tests/test_b2.R`' > "$FEUILLE"
temoin "intersection sur tests/commun.R (b3, b5), non déclaré partagé" 1 "modifient les mêmes fichiers (hors fichiers partagés) : tests/commun.R" claude/b3 --partition claude/b3 claude/b5
feuille_vague_motifs '`R/`, `tests/test_b1.R`' '`tests/test_b2.R`' | awk '/^### 1.1/ { print "Fichiers partagés admis : `tests/commun.R`"; print "" } { print }' > "$FEUILLE"
temoin "même intersection, tests/commun.R déclaré fichier partagé au tableau" 0 "intersection vide entre « claude/b3 » et « claude/b5 »" claude/b3 --partition claude/b3 claude/b5
feuille_vague_motifs '`R/`, `tests/test_b1.R`' '`tests/test_b2.R`' > "$FEUILLE"
temoin "branche locale périmée par rapport à origin/ (b6) : écart" 1 "la branche locale" claude/b2 --partition claude/b2 claude/b6
temoin "référence lue pour une branche de la partition : origin/ en priorité" 1 "claude/b6 (lu : origin/claude/b6) : 1 fichier(s) modifié(s)" claude/b2 --partition claude/b2 claude/b6
temoin "écart local / origin aussi pour <branche> elle-même" 1 "« claude/b6 » : la branche locale" claude/b6 --partition claude/b6 claude/b2
json "$(pr 1 claude/b1 false)" "$(pr 2 claude/b2 false)" "$(prs_brouillons 3 4 5 6 | sed 's/^\[//; s/\]$//')" > "$PRS"
temoin "partition avec PR non brouillon, sans --avant-fusion : écart" 1 "n'est pas un brouillon" claude/b1 --partition claude/b1 claude/b2
temoin "partition avec PR non brouillon, avec --avant-fusion : conforme" 0 "Conforme" --avant-fusion claude/b1 --partition claude/b1 claude/b2
json "$(pr 1 claude/b1 true)" > "$PRS"
feuille_vague_motifs '`R/`' '`tests/test_b2.R`' > "$FEUILLE"
temoin "branche de la partition absente des PR ouvertes" 1 "« claude/b2 » n'est la tête d'aucune PR" claude/b1 --partition claude/b1 claude/b2
prs_brouillons 1 2 3 4 5 6 > "$PRS"
temoin "branche de la partition introuvable dans le dépôt git" 1 "introuvable" claude/b1 --partition claude/b1 claude/b7

echo "== Usage, API, données =="
temoin "usage : branche manquante" 2 "Branche manquante"
temoin "usage : --partition avec une seule branche" 2 "au moins deux branches distinctes" claude/b1 --partition claude/b1
temoin "usage : --partition avec un doublon (une seule branche distincte)" 2 "après dédoublonnage" claude/b1 --partition claude/b1 claude/b1
temoin "usage : option inconnue" 2 "Option non reconnue" --x claude/b1
PRS="$tmp/absent.json"
temoin "JSON de PR illisible (fichier absent)" 3 "illisible" claude/b1
PRS="$tmp/mauvais.json"; echo '{pas du json' > "$PRS"
temoin "réponse de l'API qui n'est pas du JSON" 3 "illisible" claude/b1
# API : gh et curl factices, aucune variable de PR injectee (PRS vide)
mkdir -p "$tmp/bin"
printf '#!/bin/sh\nexit 1\n' > "$tmp/bin/gh"; printf '#!/bin/sh\nexit 1\n' > "$tmp/bin/curl"
chmod +x "$tmp/bin/gh" "$tmp/bin/curl"
PRS=""; PATH_TEMOIN="$tmp/bin:$PATH"
temoin "API inaccessible (gh et curl en échec)" 3 "API GitHub inaccessible" claude/b1
prs_brouillons 1 > "$tmp/page_courte.json"
printf '#!/bin/sh\ncat "%s"\n' "$tmp/page_courte.json" > "$tmp/bin/curl"
feuille_sans_tableau > "$FEUILLE"
temoin "repli curl, page courte : conforme" 0 "Conforme" claude/b1
l=""; k=0; while [ "$k" -lt 100 ]; do k=$((k + 1)); l="$l${l:+,}$(pr "$k" "claude/x$k" true)"; done
printf '[%s]' "$l" > "$tmp/page_pleine.json"
printf '#!/bin/sh\ncat "%s"\n' "$tmp/page_pleine.json" > "$tmp/bin/curl"
temoin "repli curl, page pleine (100 PR) : code 3" 3 "page pleine" claude/x1
unset PATH_TEMOIN

echo
if [ "$echec" -eq 0 ]; then echo "Tous les témoins passent ($n)."; else echo "$echec témoin(s) en échec sur $n." >&2; exit 1; fi
