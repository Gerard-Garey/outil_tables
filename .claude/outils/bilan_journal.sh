#!/usr/bin/env bash
###############################################################################
#  .claude/outils/bilan_journal.sh  --  bilan du journal des sous-agents
#
#  Agrege .claude/journal-agents.jsonl (hook SubagentStop) par agent et par
#  modele servi : nombre de sous-agents, reprises, appels au modele, contexte
#  au dernier appel (moyenne et maximum), duree. Une reprise (SendMessage)
#  ajoute une ligne cumulative pour le meme (session, id) : seule la derniere
#  est retenue, les precedentes sont comptees comme reprises ; sa duree
#  inclut l'attente entre les executions. Sert a la calibration de
#  docs/agents/routage.md, § 8. Aucune donnee de consommation n'est estimee :
#  seules les grandeurs lues dans les transcripts sont restituees.
#
#  Une ligne dont la cle « diagnostic » porte "transcript": "absent" (hook
#  SubagentStop sans fichier de transcript, issue #95) n'est pas un agent du
#  projet : elle n'entre pas dans le tableau et est comptee a part, en pied.
#  « ? » ne designe plus que les lignes sans agent dont le transcript est
#  present ou non renseigne (lignes anterieures a #95 comprises).
#
#  Option --depuis <date> (#95, commentaire du 26/10/2026) : ne retient que
#  les lignes dont le champ « date » (UTC, ISO 8601, ecrit par le hook) est
#  posterieur ou egal a <date>, pour borner le bilan a la periode d'une
#  branche ou d'une session (le journal local est cumulatif). <date> s'ecrit
#  AAAA-MM-JJ ou AAAA-MM-JJTHH:MM[:SS][Z], en UTC ; tout autre format est
#  refuse, de meme qu'une date inexistante (#153). Les lignes sans date sont
#  alors exclues et comptees en pied. Options et journal en toute position ;
#  option inconnue ou argument en trop : code 2 (#153).
#
#  Usage : bash .claude/outils/bilan_journal.sh [--depuis <date>] [journal]
###############################################################################
set -euo pipefail
racine="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
usage="Usage : bash .claude/outils/bilan_journal.sh [--depuis <date>] [journal]"
format="AAAA-MM-JJ ou AAAA-MM-JJTHH:MM[:SS][Z], UTC"
# Options en toute position (#153) : --depuis au plus une fois, un seul
# journal ; tout autre argument commencant par « - » est refuse (code 2).
depuis=""; vu_depuis=0; journal=""
while [ $# -gt 0 ]; do
  case "$1" in
    --depuis)
      [ "$vu_depuis" -eq 0 ] || { echo "Option --depuis donnee deux fois. $usage" >&2; exit 2; }
      [ $# -ge 2 ] || { echo "Option --depuis : date manquante ($format)." >&2; exit 2; }
      depuis="$2"; vu_depuis=1; shift 2 ;;
    -*)
      echo "Option non reconnue « $1 ». $usage" >&2; exit 2 ;;
    *)
      [ -z "$journal" ] || { echo "Argument en trop « $1 ». $usage" >&2; exit 2; }
      journal="$1"; shift ;;
  esac
done
py=""
for c in python3 python; do
  if command -v "$c" >/dev/null 2>&1 && "$c" -c "import json" >/dev/null 2>&1; then py="$c"; break; fi
done
if [ "$vu_depuis" -eq 1 ]; then
  if ! printf '%s' "$depuis" | grep -Eq '^[0-9]{4}-[0-9]{2}-[0-9]{2}(T[0-9]{2}:[0-9]{2}(:[0-9]{2})?Z?)?$'; then
    echo "Option --depuis : date non admise « $depuis » ($format)." >&2; exit 2
  fi
  # Controle calendaire (#153) : la forme seule admettait 2026-13-45.
  if [ -n "$py" ] && ! "$py" -c '
import sys, datetime
s = sys.argv[1].rstrip("Z")
f = {10: "%Y-%m-%d", 16: "%Y-%m-%dT%H:%M", 19: "%Y-%m-%dT%H:%M:%S"}[len(s)]
datetime.datetime.strptime(s, f)' "$depuis" >/dev/null 2>&1; then
    echo "Option --depuis : date inexistante « $depuis » ($format)." >&2; exit 2
  fi
fi
journal="${journal:-$racine/.claude/journal-agents.jsonl}"
[ -s "$journal" ] || { echo "Journal vide ou absent : $journal"; exit 0; }
[ -n "$py" ] || { echo "Python introuvable : bilan impossible."; exit 1; }
"$py" - "$journal" "$depuis" <<'PY'
import json, sys
from collections import defaultdict
depuis = sys.argv[2].rstrip("Z")
der, nb = {}, defaultdict(int)
sans_transcript = 0
sans_date = 0
for l in open(sys.argv[1], encoding="utf-8"):
    try:
        d = json.loads(l)
    except Exception:
        continue
    if not isinstance(d, dict):
        continue
    if depuis:
        dt = d.get("date")
        if not isinstance(dt, str) or not dt:
            sans_date += 1
            continue
        if dt.rstrip("Z") < depuis:
            continue
    diag = d.get("diagnostic")
    if isinstance(diag, dict) and diag.get("transcript") == "absent":
        sans_transcript += 1
        continue
    k = (d.get("session"), d.get("id")) if d.get("id") else (None, len(der))
    der[k] = d
    nb[k] += 1
g = defaultdict(list)
for k, d in der.items():
    d["_reprises"] = nb[k] - 1
    g[(d.get("agent") or "?", ",".join(d.get("modeles") or ["?"]))].append(d)
print("%-24s %-28s %5s %8s %7s %12s %12s %9s" % ("agent", "modele(s) servi(s)", "n", "reprises", "appels", "ctx moyen", "ctx max", "duree moy"))
for (a, m), v in sorted(g.items()):
    ctx = [x["contexte_final"] for x in v if x.get("contexte_final") is not None]
    du = [x["duree_s"] for x in v if x.get("duree_s") is not None]
    print("%-24s %-28s %5d %8d %7d %12s %12s %9s" % (a, m[:28], len(v), sum(x["_reprises"] for x in v),
          sum(x.get("appels") or 0 for x in v),
          round(sum(ctx)/len(ctx)) if ctx else "-", max(ctx) if ctx else "-", "%ds" % round(sum(du)/len(du)) if du else "-"))
if depuis:
    print("periode : lignes datees a partir de %s (UTC) ; lignes sans date exclues : %d" % (sys.argv[2], sans_date))
print("evenements SubagentStop sans transcript au moment du hook (hors tableau) : %d" % sans_transcript)
PY
