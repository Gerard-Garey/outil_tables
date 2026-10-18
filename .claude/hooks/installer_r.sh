#!/usr/bin/env bash
###############################################################################
#  .claude/hooks/installer_r.sh  --  HOOK SessionStart DE CLAUDE CODE
#
#  Installe, en session cloud seulement, R 4.3.3 (version de DESCRIPTION,
#  « Depends: R (>= 4.3.3) », M44, et de la CI) et les dependances de
#  l'outil : aucune pour le moteur (packages de priorite base, ADR 0002) ;
#  shiny, htmltools et openxlsx pour l'application (ensemble « application »
#  de tests/controles/packages_hors_moteur.csv). Les packages de
#  contre-verification ne sont pas installes : leurs valeurs sont produites
#  hors CI (ADR 0002, troisieme annotation du 16/10/2026).
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
PAQUETS_APT="r-cran-shiny r-cran-htmltools r-cran-openxlsx"
PACKAGES_R="shiny htmltools openxlsx"

if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

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
timeout 120 $sudo_ apt-get update -qq >/dev/null 2>&1
timeout 420 $sudo_ apt-get install -y -qq --no-install-recommends \
  "r-base-core=$VERSION_R*" $PAQUETS_APT >/dev/null 2>&1

v="$(version_r)"
if [ "$v" != "$VERSION_R" ]; then
  echo "ATTENTION : R $VERSION_R non installe (Rscript : ${v:-absent}). Les batteries de CLAUDE.md ne tourneront pas dans cette session."
  exit 0
fi
m="$(manquants)"
if [ -n "$m" ]; then
  echo "R $VERSION_R installe ; ATTENTION : packages de l'application absents : $m."
else
  echo "R $VERSION_R installe, avec shiny, htmltools et openxlsx."
fi
exit 0
