#!/usr/bin/env bash
###############################################################################
#  .claude/hooks/installer_r.sh  --  HOOK SessionStart DE CLAUDE CODE
#
#  Installe, en session cloud seulement, R 4.3.3 (version de DESCRIPTION,
#  « Depends: R (>= 4.3.3) », M44, et de la CI) et les dependances de
#  l'outil : aucune pour le moteur (packages de priorite base, ADR 0002) ;
#  pour l'application, les packages de l'ensemble « application » de
#  tests/controles/packages_hors_moteur.csv (shiny, htmltools, openxlsx au
#  18/10/2026). Les packages de contre-verification ne sont pas installes :
#  leurs valeurs sont produites hors CI (ADR 0002, troisieme annotation du
#  16/10/2026).
#
#  Source : depot apt d'Ubuntu (r-base-core et r-cran-*), sans depot CRAN.
#  Le conteneur cloud repart neuf a chaque session : sans ce hook, Rscript
#  est absent et les batteries de CLAUDE.md ne tournent pas.
#
#  Le hook n'agit pas hors session cloud (CLAUDE_CODE_REMOTE different de
#  true) : sur le poste du mainteneur, R s'installe a la main. Il n'installe
#  que ce qui manque et n'echoue jamais : une session sans reseau demarre
#  normalement, sans R. La plateforme obtenue n'est pas la plateforme de
#  reference (LAPACK, image) : les references se regenerent sur la CI seule
#  (CLAUDE.md, « Changements de resultats et reproductibilite »).
###############################################################################

VERSION_R="4.3.3"

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Packages de l'application, lus dans la liste versionnee ; paquet apt
# correspondant : r-cran-<nom en minuscules, points en tirets>
LISTE="${CLAUDE_PROJECT_DIR:-.}/tests/controles/packages_hors_moteur.csv"
PACKAGES_R="$(awk -F, '$1=="application" {print $2}' "$LISTE" 2>/dev/null | tr '\n' ' ')"
PACKAGES_R="${PACKAGES_R% }"
if [ -z "$PACKAGES_R" ]; then
  echo "ATTENTION : liste des packages de l'application introuvable ou vide ($LISTE) : seul R est installe et verifie."
fi
PAQUETS_APT=""
for p in $PACKAGES_R; do
  PAQUETS_APT="$PAQUETS_APT r-cran-$(printf '%s' "$p" | tr 'A-Z.' 'a-z-')"
done

# Fils de calcul fixes, comme sur la CI (ADR 0008)
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  printf 'export OPENBLAS_NUM_THREADS=1\nexport OMP_NUM_THREADS=1\n' >> "$CLAUDE_ENV_FILE"
fi

version_r() {
  command -v Rscript >/dev/null 2>&1 \
    && Rscript -e 'cat(as.character(getRversion()))' 2>/dev/null
}

manquants() {
  Rscript -e 'p <- commandArgs(TRUE); cat(p[!vapply(p, requireNamespace, logical(1), quietly = TRUE)])' \
    $PACKAGES_R 2>/dev/null
}

if [ "$(version_r)" = "$VERSION_R" ] && [ -z "$(manquants)" ]; then
  exit 0
fi

sudo_=""
[ "$(id -u)" -eq 0 ] || sudo_="sudo -n"

echo "Installation de R $VERSION_R et des dependances de l'outil..." >&2
export DEBIAN_FRONTEND=noninteractive
journal="${TMPDIR:-/tmp}/installer_r.$$.log"
timeout 120 $sudo_ apt-get update -qq > "$journal" 2>&1
# Verrou de dpkg attendu plutot que refuse ; 4.3.3 impose meme si une autre
# version de R est deja presente (--allow-downgrades)
timeout 420 $sudo_ apt-get install -y -qq --no-install-recommends --allow-downgrades \
  -o DPkg::Lock::Timeout=60 "r-base-core=$VERSION_R*" $PAQUETS_APT >> "$journal" 2>&1
code=$?

v="$(version_r)"
if [ "$v" != "$VERSION_R" ]; then
  echo "ATTENTION : R $VERSION_R non installe (Rscript : ${v:-absent} ; apt-get : code $code ; journal : $journal). Les batteries de CLAUDE.md ne tourneront pas dans cette session."
  tail -n 5 "$journal"
  [ "$code" -eq 124 ] && echo "apt-get interrompu par timeout : lancer « sudo dpkg --configure -a » avant toute autre installation."
  exit 0
fi
m="$(manquants)"
if [ -n "$m" ]; then
  echo "R $VERSION_R installe ; ATTENTION : packages de l'application absents : $m (journal : $journal)."
  tail -n 5 "$journal"
else
  echo "R $VERSION_R installe, avec les packages de l'application : $PACKAGES_R"
fi
exit 0
