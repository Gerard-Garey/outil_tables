#!/usr/bin/env bash
###############################################################################
#  .claude/outils/branche_de_travail.sh  --  controle de la branche de travail
#  et des partitions de vague (CLAUDE.md, « Git et GitHub », puces 2 et 3 ;
#  ADR 0013, points 3 et 5 ; issue #137)
#
#  Usage : bash .claude/outils/branche_de_travail.sh [--avant-fusion] <branche>
#                [--partition <b1> <b2> ...]
#
#  PREREQUIS : un « git fetch origin » prealable (les branches et la base des
#  diffs se lisent sous refs/remotes/origin/ ; le script n'ecrit rien et ne
#  fait aucun fetch).
#
#  Controles (chacun signale ses ecarts, le code de sortie est non nul des
#  qu'un ecart existe ; le script n'ecrit ni dans le depot ni sur GitHub) :
#   1. <branche> est la tete d'une PR ouverte vers main, BROUILLON par defaut
#      (demarrage de session) ; avec --avant-fusion, une PR ouverte non
#      brouillon est admise (pour <branche> et pour les branches de
#      --partition) ;
#   2. vague : une ligne « Vague autorisee : <nom>, <date> » dans la zone du
#      § 1 de docs/feuille-de-route.md (voir plus bas) declare une vague ;
#      alors <branche> figure au tableau des branches ouvertes, et toute PR
#      brouillon claude/... ouverte vers main figure au tableau. Sans ce
#      marqueur : hors vague, meme si un tableau existe (il est ignore) ;
#      alors <branche> est la seule PR brouillon claude/... ouverte vers main ;
#   3. --partition : pour les branches nommees (au moins deux distinctes ; les
#      doublons sont ecartes), sur les seuls fichiers de R/, tests/ et
#      docs/adr/ (ADR 0013, point 3 : les autres fichiers sont ignores, et la
#      sortie le dit),
#        a. intersections deux a deux de « git diff --name-only <base>...<bi> »
#           vides, hors fichiers partages ;
#        b. si le tableau declare des partitions : aucun fichier modifie par
#           une branche hors de sa partition declaree (hors fichiers
#           partages). Sans colonne de partition, ce controle est SAUTE,
#           avec un message : le script ne devine pas.
#      Les controles 1 et 2 s'appliquent aussi a chaque branche nommee apres
#      --partition (une branche de vague est une PR du tableau).
#   4. toute branche nommee (<branche> comprise) dont la reference locale ET
#      origin/<b> existent mais different : ecart explicite (avance/retard
#      donnes) ; relancer apres push ou fetch.
#
#  Forme du tableau lue (zone du § 1 : entre le titre « ## 1. » et la premiere
#  sous-section « ### ») :
#   - marqueur de vague : une ligne commencant par « Vague autorisee » (avec
#     ou sans accent), suivie de « : <nom>, <date> » (nom avec ou sans accents
#     graves) ; sans nom et date : ecart ;
#   - seul le PREMIER tableau dont la premiere cellule d'en-tete est « Branche »
#     est lu (les suivants sont ignores) ; vague 2 : colonnes Branche, PR,
#     Role, Partition, Ordre de fusion ;
#   - premiere colonne : la branche, premier mot entre accents graves de la
#     cellule (ex. `claude/b13` (B13)) ; une ligne sans nom en accents
#     graves est un ecart ;
#   - colonne de partition : cellule d'en-tete EXACTEMENT « Partition
#     (motifs) » ; chaque cellule liste des motifs glob separes par des
#     virgules (accents graves facultatifs), ex. `R/*`, `tests/unitaires/test_x.R`.
#     Un motif est un motif de « case » de bash : « * » couvre aussi « / » ;
#     un motif terminant par « / » vaut tout le repertoire. Une virgule ne
#     peut pas figurer dans un motif. Cellule vide pour une branche alors
#     que la colonne existe : ecart (partition non declaree).
#     (Une colonne en prose, telle « Partition (fichiers que la branche peut
#     modifier) » de la vague 2, n'est pas lue : controle 3b saute.)
#   - fichiers partages supplementaires, facultatifs : une ligne de cette
#     zone commencant par « Fichiers partages » (avec ou sans accent), suivie
#     des chemins entre accents graves. Toujours admis : docs/latex/
#     doc_outil_tables.tex, docs/latex/doc_outil_tables.pdf,
#     tests/concordance_attente.csv (ADR 0013, point 3).
#
#  Sources : base des diffs = origin/main si elle existe ; sinon repli sur
#  main LOCAL, avec avertissement (il peut etre perime). Une branche autre que
#  <branche> se lit sous origin/ en priorite, sinon en local ; <branche> en
#  local en priorite, sinon sous origin/. PR : « gh api » si gh est installe,
#  sinon curl sur l'API publique, sans jeton.
#  Limite de pagination (mesuree le 11/11/2026 par le proxy de session :
#  « gh api ...pulls?per_page=5 --paginate » rend la page 1 puis echoue,
#  HTTP 403, « Numeric-ID repository paths ... not supported through this
#  proxy », car l'en-tete Link de la page suivante vise repositories/{id}) :
#  au-dela de 100 PR ouvertes vers main, la lecture complete est impossible.
#  Le repli curl ne lit que la premiere page : s'il rend exactement 100 PR
#  (page pleine), le script sort en code 3 sans conclure. Sans acces a l'API
#  (ni repli), il le dit et sort en echec (code 3), sans deviner.
#
#  Variables d'environnement (temoins, .claude/outils/branche_de_travail_temoins.sh) :
#   BRANCHE_TRAVAIL_PRS_JSON     fichier JSON (liste de PR au format de
#                                l'API) a la place de l'appel a l'API
#   BRANCHE_TRAVAIL_FEUILLE      feuille de route (defaut : docs/feuille-de-route.md)
#   BRANCHE_TRAVAIL_RACINE       racine du depot git (defaut : celle du script)
#   BRANCHE_TRAVAIL_BASE         reference de la base des diffs (defaut : voir plus haut)
#   BRANCHE_TRAVAIL_DEPOT        proprietaire/depot (defaut : Gerard-Garey/outil_tables)
#
#  Codes de sortie : 0 conforme ; 1 ecart ; 2 usage ; 3 API ou donnees
#  indisponibles.
#
#  Portabilite : bash, awk, grep, sort, comm, git, python3 ou python (JSON,
#  comme bilan_journal.sh) ; ni jq, ni tableau associatif, ni mapfile, ni
#  option GNU.
###############################################################################
set -uo pipefail
set -f   # pas d'expansion de chemins : les motifs de partition sont des globs
usage="Usage : bash .claude/outils/branche_de_travail.sh [--avant-fusion] <branche> [--partition <b1> <b2> ...]"

racine="${BRANCHE_TRAVAIL_RACINE:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
feuille="${BRANCHE_TRAVAIL_FEUILLE:-$racine/docs/feuille-de-route.md}"
depot="${BRANCHE_TRAVAIL_DEPOT:-Gerard-Garey/outil_tables}"

# ---- arguments --------------------------------------------------------------
branche=""; liste_partition=""; n_partition=0; en_partition=0; avant_fusion=0
while [ $# -gt 0 ]; do
  case "$1" in
    --partition)
      [ "$en_partition" -eq 0 ] || { echo "Option --partition donnée deux fois. $usage" >&2; exit 2; }
      en_partition=1; shift ;;
    --avant-fusion)
      [ "$avant_fusion" -eq 0 ] || { echo "Option --avant-fusion donnée deux fois. $usage" >&2; exit 2; }
      avant_fusion=1; shift ;;
    -*)
      echo "Option non reconnue « $1 ». $usage" >&2; exit 2 ;;
    *)
      if [ "$en_partition" -eq 1 ]; then
        # Doublons ecartes (une branche nommee deux fois ne fait pas une partition).
        case "
$liste_partition" in
          *"
$1
"*) ;;
          *) liste_partition="$liste_partition$1
"
             n_partition=$((n_partition + 1)) ;;
        esac
      elif [ -z "$branche" ]; then
        branche="$1"
      else
        echo "Argument en trop « $1 » (les branches de la partition suivent --partition). $usage" >&2; exit 2
      fi
      shift ;;
  esac
done
[ -n "$branche" ] || { echo "Branche manquante. $usage" >&2; exit 2; }
if [ "$en_partition" -eq 1 ] && [ "$n_partition" -lt 2 ]; then
  echo "--partition demande au moins deux branches distinctes ($n_partition après dédoublonnage). $usage" >&2; exit 2
fi

ecarts=0
ecart() { echo "ÉCART : $*" >&2; ecarts=$((ecarts + 1)); }
ok() { echo "ok : $*"; }

# ---- python (JSON) ----------------------------------------------------------
py=""
for c in python3 python; do
  if command -v "$c" >/dev/null 2>&1 && "$c" -c "import json" >/dev/null 2>&1; then py="$c"; break; fi
done
[ -n "$py" ] || { echo "python3 (ou python) introuvable : lecture JSON impossible, aucun contrôle fait." >&2; exit 3; }

# ---- PR ouvertes vers main (une seule lecture) ------------------------------
source_api=""
if [ -n "${BRANCHE_TRAVAIL_PRS_JSON:-}" ]; then
  [ -r "$BRANCHE_TRAVAIL_PRS_JSON" ] || { echo "BRANCHE_TRAVAIL_PRS_JSON illisible : $BRANCHE_TRAVAIL_PRS_JSON" >&2; exit 3; }
  json="$(cat "$BRANCHE_TRAVAIL_PRS_JSON")"; source_api="fichier"
else
  url="repos/$depot/pulls?state=open&base=main&per_page=100"
  json=""
  if command -v gh >/dev/null 2>&1; then
    json="$(gh api "$url" --paginate 2>/dev/null)" && source_api="gh" || json=""
  fi
  if [ -z "$json" ] && command -v curl >/dev/null 2>&1; then
    json="$(curl -fsS "https://api.github.com/$url" 2>/dev/null)" && source_api="curl" || json=""
  fi
  if [ -z "$json" ]; then
    echo "API GitHub inaccessible (gh api, puis curl, ont échoué pour $depot) : aucun contrôle de la branche de travail possible." >&2
    exit 3
  fi
fi
# Sortie : « #<nombre d'elements lus> », puis une ligne « tete <TAB> brouillon
# (oui/non) <TAB> numero » par PR ouverte vers main issue du depot (les listes
# de plusieurs pages, mises bout a bout par gh --paginate, sont lues l'une
# apres l'autre).
prs="$(printf '%s' "$json" | "$py" -c '
import json, sys
txt = sys.stdin.read()
dec = json.JSONDecoder()
i, n, out, total = 0, len(txt), [], 0
try:
    while True:
        while i < n and txt[i].isspace():
            i += 1
        if i >= n:
            break
        obj, i = dec.raw_decode(txt, i)
        if not isinstance(obj, list):
            raise ValueError("liste de PR attendue")
        total += len(obj)
        for p in obj:
            if p.get("state", "open") != "open":
                continue
            if p["base"]["ref"] != "main":
                continue
            repo = (p["head"].get("repo") or {}).get("full_name")
            if repo is not None and repo != sys.argv[1]:
                continue
            out.append("%s\t%s\t%s" % (p["head"]["ref"], "oui" if p.get("draft") else "non", p["number"]))
except Exception as e:
    sys.stderr.write("Réponse de l API illisible : %s\n" % e)
    sys.exit(1)
print("#%d" % total)
if out:
    print("\n".join(out))
' "$depot")" || { echo "Réponse de l'API illisible : aucun contrôle possible." >&2; exit 3; }
n_lus="$(printf '%s\n' "$prs" | head -n 1 | tr -d '#')"
prs="$(printf '%s\n' "$prs" | tail -n +2)"
if [ "$source_api" = "curl" ] && [ "$n_lus" -ge 100 ]; then
  echo "Repli curl : $n_lus PR lues, page pleine (100) : la pagination n'est pas lue (gh --paginate échoue par le proxy de session) ; liste des PR ouvertes potentiellement incomplète, aucun contrôle possible." >&2
  exit 3
fi

# ---- tableau des branches ouvertes (§ 1 de la feuille de route) -------------
[ -r "$feuille" ] || { echo "Feuille de route illisible : $feuille" >&2; exit 3; }
# Lignes produites : VAGUE<TAB>texte, TABLEAU, COLPART, ROW<TAB>branche<TAB>motifs,
# PARTAGE<TAB>chemin, ERR<TAB>message.
tableau="$(awk '
function trim(s) { gsub(/\r/, "", s); sub(/^[ \t]+/, "", s); sub(/[ \t]+$/, "", s); return s }
/^## 1\./ { zone = 1; next }
zone && (/^### / || /^## /) { zone = 0 }
!zone { next }
!/^\|/ && entete { entete = 0; colp = 0 }   # fin du tableau
/^Vague autoris/ {
  t = $0; sub(/^[^:]*:/, "", t); gsub(/`/, "", t); t = trim(t)
  if (t ~ /^[^,]+,[ \t]*[^ \t]/) print "VAGUE\t" t
  else print "ERR\tmarqueur de vague sans « : <nom>, <date> » : " trim($0)
  next
}
/^Fichiers partag/ {
  l = $0
  while (match(l, /`[^`]+`/)) {
    print "PARTAGE\t" substr(l, RSTART + 1, RLENGTH - 2)
    l = substr(l, RSTART + RLENGTH)
  }
  next
}
/^\|/ {
  n = split($0, c, "|")
  cell1 = trim(c[2])
  if (!entete) {
    if (cell1 == "Branche" && !lu) {
      lu = 1; entete = 1; print "TABLEAU"
      for (k = 2; k < n; k++) if (trim(c[k]) == "Partition (motifs)") { colp = k; print "COLPART" }
    }
    next
  }
  if (cell1 ~ /^:?-+:?$/) next   # ligne de separation
  if (!match(cell1, /`[^`]+`/)) { print "ERR\tligne de tableau sans nom de branche entre accents graves : " cell1; next }
  b = substr(cell1, RSTART + 1, RLENGTH - 2)
  m = ""
  if (colp) {
    cnt = split(c[colp], mo, ",")
    for (k = 1; k <= cnt; k++) {
      x = trim(mo[k]); gsub(/`/, "", x)
      if (x != "") m = m (m == "" ? "" : ",") x
    }
  }
  print "ROW\t" b "\t" m
}' "$feuille")" || { echo "Lecture de $feuille impossible." >&2; exit 3; }

lignes_tableau() { printf '%s\n' "$tableau" | awk -F'\t' -v t="$1" '$1 == t'; }
while IFS="$(printf '\t')" read -r _ msg; do
  [ -z "$msg" ] || ecart "$feuille, § 1 : $msg"
done <<EOF_ERR
$(lignes_tableau ERR)
EOF_ERR
n_lignes="$(lignes_tableau ROW | grep -c . || true)"
a_tableau=0; [ -n "$(lignes_tableau TABLEAU)" ] && a_tableau=1
a_colonne=0; [ -n "$(lignes_tableau COLPART)" ] && a_colonne=1
marqueur="$(lignes_tableau VAGUE | head -n 1 | cut -f2)"
partages="docs/latex/doc_outil_tables.tex
docs/latex/doc_outil_tables.pdf
tests/concordance_attente.csv
$(lignes_tableau PARTAGE | cut -f2)"

en_vague=0
if [ -n "$marqueur" ]; then
  en_vague=1
  echo "Vague déclarée (« Vague autorisée : $marqueur », $feuille, § 1) ; $n_lignes branche(s) au tableau des branches ouvertes."
  [ "$n_lignes" -gt 0 ] || ecart "vague déclarée, mais aucun tableau « Branche » lisible (ou sans ligne) au § 1 de $feuille."
else
  if [ "$a_tableau" -eq 1 ]; then
    echo "Hors vague : aucun marqueur « Vague autorisée : <nom>, <date> » au § 1 de $feuille ; le tableau présent est ignoré."
  else
    echo "Hors vague : aucun marqueur « Vague autorisée : <nom>, <date> » au § 1 de $feuille."
  fi
fi

# ---- controles 1 et 2 -------------------------------------------------------
brouillons_claude="$(printf '%s\n' "$prs" | awk -F'\t' '$2 == "oui" && $1 ~ /^claude\// { print $1 "\t" $3 }')"

if [ "$en_vague" -eq 1 ]; then
  # En vague, toute PR brouillon claude/... ouverte vers main figure au tableau.
  hors_tableau=""
  while IFS="$(printf '\t')" read -r bb numpr; do
    [ -n "$bb" ] || continue
    [ -n "$(lignes_tableau ROW | awk -F'\t' -v b="$bb" '$2 == b')" ] || hors_tableau="$hors_tableau « $bb » (PR #$numpr)"
  done <<EOF_BR
$brouillons_claude
EOF_BR
  if [ -n "$hors_tableau" ]; then
    ecart "en vague, PR brouillon claude/... ouverte(s) vers main absente(s) du tableau des branches ouvertes :$hors_tableau."
  fi
fi

controler_branche() {
  b="$1"
  lignes="$(printf '%s\n' "$prs" | awk -F'\t' -v b="$b" '$1 == b')"
  if [ -z "$lignes" ]; then
    ecart "la branche « $b » n'est la tête d'aucune PR ouverte vers main (PR ouvertes vers main : $(printf '%s\n' "$prs" | cut -f1 | tr '\n' ' ' | sed 's/ *$//'))."
    return
  fi
  if [ "$(printf '%s\n' "$lignes" | wc -l | tr -d ' ')" -gt 1 ]; then
    ecart "la branche « $b » est la tête de plusieurs PR ouvertes vers main."
    return
  fi
  num="$(printf '%s' "$lignes" | cut -f3)"
  if [ "$(printf '%s' "$lignes" | cut -f2)" = "oui" ]; then
    ok "« $b » est la tête de la PR brouillon #$num ouverte vers main."
  elif [ "$avant_fusion" -eq 1 ]; then
    ok "« $b » est la tête de la PR #$num ouverte vers main, non brouillon (admis par --avant-fusion)."
  else
    ecart "la PR #$num de « $b » n'est pas un brouillon (--avant-fusion admet une PR non brouillon)."
    return
  fi
  if [ "$en_vague" -eq 1 ]; then
    if [ -n "$(lignes_tableau ROW | awk -F'\t' -v b="$b" '$2 == b')" ]; then
      ok "« $b » figure au tableau des branches ouvertes."
    else
      ecart "la branche « $b » ne figure pas au tableau des branches ouvertes de $feuille, § 1."
    fi
  else
    case "$b" in
      claude/*) ;;
      *) ecart "hors vague, la branche de travail doit commencer par claude/ (« $b »)." ;;
    esac
    autres="$(printf '%s\n' "$brouillons_claude" | awk -F'\t' -v b="$b" '$1 != "" && $1 != b { print $1 }' | tr '\n' ' ' | sed 's/ *$//')"
    if [ -n "$autres" ]; then
      ecart "hors vague, d'autres PR brouillon claude/... sont ouvertes vers main ($autres) sans vague déclarée : s'arrêter et demander au mainteneur."
    else
      ok "hors vague, « $b » est la seule PR brouillon claude/... ouverte vers main."
    fi
  fi
}

controles_faits="
"
for b in "$branche" $(printf '%s' "$liste_partition"); do
  case "$controles_faits" in *"
$b
"*) continue ;; esac
  controles_faits="$controles_faits$b
"
  controler_branche "$b"
done

# ---- controle 4 : references locale et origin/ ------------------------------
sha_ref() { git -C "$racine" rev-parse --verify -q "$1^{commit}" 2>/dev/null; }

resoudre() {  # $1 branche : reference lue ; <branche> en local d'abord, les autres sous origin/ d'abord
  loc="$(sha_ref "refs/heads/$1")"; ori="$(sha_ref "refs/remotes/origin/$1")"
  if [ "$1" = "$branche" ]; then premier="$loc"; second="$ori"; npremier="$1"; nsecond="origin/$1"
  else premier="$ori"; second="$loc"; npremier="origin/$1"; nsecond="$1"; fi
  if [ -n "$premier" ]; then echo "$npremier"
  elif [ -n "$second" ]; then echo "$nsecond"
  else return 1; fi
}

for b in "$branche" $(printf '%s' "$liste_partition"); do
  loc="$(sha_ref "refs/heads/$b")"; ori="$(sha_ref "refs/remotes/origin/$b")"
  if [ -n "$loc" ] && [ -n "$ori" ] && [ "$loc" != "$ori" ]; then
    cmp="$(git -C "$racine" rev-list --left-right --count "$loc...$ori" 2>/dev/null | tr '\t' ' ')"
    ecart "« $b » : la branche locale ($loc) et origin/$b ($ori) diffèrent (local seul / origin seul : ${cmp:-?}) : git fetch origin, puis pousser ou mettre à jour la branche locale avant de contrôler."
  fi
done

# ---- controle 3 : partitions ------------------------------------------------
dans_partition() {  # $1 fichier, $2 motifs separes par des virgules
  f="$1"; motifs="$2"
  oldifs="$IFS"; IFS=','
  for m in $motifs; do
    IFS="$oldifs"
    case "$m" in */) m="$m*" ;; esac
    case "$f" in $m) return 0 ;; esac
    IFS=','
  done
  IFS="$oldifs"
  return 1
}

if [ "$en_partition" -eq 1 ]; then
  base="${BRANCHE_TRAVAIL_BASE:-}"
  if [ -z "$base" ]; then
    if [ -n "$(sha_ref refs/remotes/origin/main)" ]; then base="origin/main"
    elif [ -n "$(sha_ref refs/heads/main)" ]; then
      base="main"
      echo "AVERTISSEMENT : origin/main absente, repli sur la branche main LOCALE, peut-être périmée (git fetch origin) : les diffs peuvent inclure des fichiers déjà fusionnés dans main." >&2
    else echo "Ni origin/main ni main dans $racine : partitions non contrôlables (git fetch origin)." >&2; exit 3; fi
  fi
  echo "Partitions : base des diffs = $base (un git fetch origin préalable est requis) ; seuls les fichiers de R/, tests/ et docs/adr/ sont contrôlés (ADR 0013, point 3), les autres sont ignorés."
  noms=""; fichiers_des=""   # fichiers_des : lignes « branche <TAB> fichier » (perimetre, hors fichiers partages)
  for b in $(printf '%s' "$liste_partition"); do
    ref="$(resoudre "$b")" || { ecart "la branche « $b » est introuvable (ni locale ni origin/) : git fetch origin $b."; continue; }
    liste="$(git -C "$racine" -c core.quotepath=off diff --name-only "$base...$ref")" \
      || { ecart "git diff --name-only $base...$ref a échoué."; continue; }
    liste="$(printf '%s\n' "$liste" | grep -v '^$' || true)"
    total="$(printf '%s\n' "$liste" | grep -c . || true)"
    liste="$(printf '%s\n' "$liste" | grep -E '^(R/|tests/|docs/adr/)' || true)"
    perim="$(printf '%s\n' "$liste" | grep -c . || true)"
    liste="$(printf '%s\n' "$liste" | grep -v '^$' | grep -Fvx -f <(printf '%s\n' "$partages") || true)"
    controles="$(printf '%s\n' "$liste" | grep -c . || true)"
    noms="$noms$b
"
    fichiers_des="$fichiers_des$(printf '%s\n' "$liste" | grep -v '^$' | awk -v b="$b" '{ print b "\t" $0 }' || true)
"
    echo "  $b (lu : $ref) : $total fichier(s) modifié(s), $((total - perim)) hors R/, tests/, docs/adr/ ignoré(s), $((perim - controles)) fichier(s) partagé(s) écarté(s), $controles contrôlé(s)."
  done

  # 3a. intersections deux a deux
  for b1 in $(printf '%s' "$noms"); do
    for b2 in $(printf '%s' "$noms"); do
      [ "$b1" \< "$b2" ] || continue
      commun="$(printf '%s\n' "$fichiers_des" | awk -F'\t' -v b="$b1" '$1 == b { print $2 }' | sort \
        | comm -12 - <(printf '%s\n' "$fichiers_des" | awk -F'\t' -v b="$b2" '$1 == b { print $2 }' | sort))"
      if [ -n "$commun" ]; then
        ecart "« $b1 » et « $b2 » modifient les mêmes fichiers (hors fichiers partagés) : $(printf '%s\n' "$commun" | tr '\n' ' ' | sed 's/ *$//')."
      else
        ok "intersection vide entre « $b1 » et « $b2 »."
      fi
    done
  done

  # 3b. partition declaree
  if [ "$a_colonne" -eq 0 ]; then
    echo "SAUTÉ : contrôle de la partition déclarée (le tableau de $feuille, § 1, n'a pas de colonne « Partition (motifs) » ; sans marqueur de vague le tableau est ignoré ; pas de devinette)."
  elif [ "$en_vague" -eq 0 ]; then
    echo "SAUTÉ : contrôle de la partition déclarée (hors vague : tableau ignoré)."
  else
    for b in $(printf '%s' "$noms"); do
      ligne="$(lignes_tableau ROW | awk -F'\t' -v b="$b" '$2 == b' | head -n 1)"
      if [ -z "$ligne" ]; then
        ecart "« $b » ne figure pas au tableau : sa partition déclarée est inconnue."; continue
      fi
      motifs="$(printf '%s' "$ligne" | cut -f3)"
      if [ -z "$motifs" ]; then
        ecart "« $b » : cellule « Partition (motifs) » vide au tableau (partition non déclarée)."; continue
      fi
      hors=""
      for f in $(printf '%s\n' "$fichiers_des" | awk -F'\t' -v b="$b" '$1 == b { print $2 }' | tr ' ' '\001'); do
        f="$(printf '%s' "$f" | tr '\001' ' ')"
        dans_partition "$f" "$motifs" || hors="$hors $f"
      done
      if [ -n "$hors" ]; then
        ecart "« $b » modifie des fichiers hors de sa partition déclarée ($motifs) :$hors."
      else
        ok "« $b » : aucun fichier hors de sa partition déclarée."
      fi
    done
  fi
fi

if [ "$ecarts" -gt 0 ]; then
  echo "ÉCHEC : $ecarts écart(s)." >&2
  exit 1
fi
echo "Conforme."
