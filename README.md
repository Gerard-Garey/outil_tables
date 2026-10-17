# outil_tables — construction de tables d'expérience

Outil R (package `tablexp` + application Shiny locale) qui construit des **tables d'expérience** à partir d'une base d'observation d'un portefeuille d'assurés : estimation des taux d'une transition (noyau multi-états), lissage ou positionnement sur une table de référence, fermeture aux âges extrêmes, validation, suivi d'expérience. Cas principal : la mortalité (tables du moment, puis prospectives) ; ensuite le rachat, l'incapacité-invalidité, la dépendance, le chômage des emprunteurs. Les résultats servent d'hypothèses biométriques de best estimate (Solvabilité II) et de support à la certification d'une table d'expérience (Code des assurances, A132-18).

**État : cadrage terminé le 14 octobre 2026 ; première branche de travail de code en cours (`claude/socle-donnees-exposition`, PR #12).** Décisions : `docs/cadrage/2026-10-14-entretien-cadrage.md` ; faits sourcés : `docs/recherche/2026-10-14-cadre-tables-experience.md` ; cahier des charges : `docs/exigences.md` ; ADR : `docs/adr/`. Plan de la branche : `docs/feuille-de-route.md`, § 1.

Le dépôt ne contient **aucune donnée réelle** : tous les jeux sont simulés sous une loi vraie connue et une graine. Licence : PolyForm Noncommercial 1.0.0 (`LICENSE`).

## Contenu

| Chemin | Rôle |
|---|---|
| `CLAUDE.md` | Règles lues par Claude Code à chaque session : contexte, Git et GitHub, architecture, changements de résultats, rigueur, sous-agents, circuits, workflows |
| `CONTEXT.md` | Glossaire du domaine (table d'expérience, transition, rôle, recette d'exposition, tête, surface de mortalité…) et de l'organisation |
| `docs/exigences.md` | Cahier des charges, exigences numérotées E1.1 à E5.5 |
| `docs/cadrage/` | Comptes rendus des entretiens de cadrage (décisions du mainteneur) |
| `docs/recherche/` | Rapports de recherche d'agents, avec leurs statuts vérifié / incertain / non trouvé |
| `docs/adr/` | Décisions consignées (0001 : routage ; 0002 à 0006 : décisions du cadrage) |
| `docs/feuille-de-route.md` | Périmètre des branches de travail, décisions du mainteneur numérotées (M1, M2…) |
| `docs/agents/` | Suivi des issues (GitHub, `gh` ou MCP), libellés de tri, documentation du domaine, routage du modèle et de l'effort |
| `.claude/agents/` | Sept sous-agents : `architect` (pilotage), `actuary` et `regulatory` (fond), `coder`, `docwriter` (réalisation), `audit`, `app-review` (vérification) ; variantes `-approfondi` générées par `.claude/outils/fiches_jumelles.sh` |
| `.claude/workflows/circuit-technique.js` | Circuit `coder` → batteries → `audit` léger, une reprise au plus, sans commit ni push |
| `.claude/settings.json`, `.claude/hooks/` | Permissions, hooks (installation des plugins, journal des sous-agents) |
| `.github/` | Modèles d'issue et de PR, CI, Dependabot, ruleset de `main` |
| `LICENSE` | PolyForm Noncommercial 1.0.0 |

En place depuis la branche `claude/socle-donnees-exposition` : `DESCRIPTION`, `NAMESPACE`, `R/` (package `tablexp`, un module par famille, `R/outils.R`, point d'entrée `construire_table(donnees, roles, plan)` ; ADR 0007), `tests/` (harnais, trois batteries, scripts de contrôle sous `tests/controles/`). À venir : `app/app.R` et `app/affichage.R` (Shiny, sans calcul ; ADR 0007, M46), références de non-régression produites par la CI et contre-vérifications hors CI sous `tests/`, `inst/extdata/` (jeux simulés, tables de référence et provenances), `docs/latex/` (documentation méthodologique, charte d'`outil_USP`).

## Prérequis et commandes

- **R 4.3.3** (CI et références ; poste aligné ; ADR 0002, quatrième annotation du 16/10/2026), packages de priorité `base` seulement pour le moteur, sauf package admis par ADR de méthode (ADR 0002 et son annotation du 16/10/2026) ; `shiny` pour l'application ; `openxlsx` facultatif pour les fichiers xlsx ; packages de contre-vérification (`survival`, `mgcv`, `WH`, `StMoMo`, `demography`, `MortalityLaws`, `lifecontingencies`, `survPen`, `MortCast`, `locfit`, `flexsurv` facultatif) seulement pour régénérer les valeurs externes des tests, hors CI, éventuellement sur un autre poste muni d'un R récent (versions de R et des packages consignées ; ADR 0002, troisième annotation du 16/10/2026). **Aucune dépendance à Java**, directe ou indirecte (d'où l'exclusion du package `ELT` ; sa source s'exécute seulement hors du dépôt, sans Java, en appoint).
- **Batteries** (depuis la racine du dépôt ; chacune installe l'arbre de travail dans une bibliothèque temporaire par `R CMD INSTALL`, n'installe aucun package, affiche une ligne de bilan et sort avec un code non nul en cas d'échec ; ADR 0007, point 11) :

  ```
  Rscript tests/test_unitaires.R
  Rscript tests/test_reproductibilite.R
  Rscript tests/concordance_doc_moteur.R --strict
  ```

  Détail de toutes les assertions : `Rscript -e "options(harnais.verbeux = TRUE); source('tests/test_unitaires.R')"`.
- **Contrôles** (scripts en R de base, exécutés aussi par la CI : job « Absence de Java » pour `absence_java.R`, job « Tests (R 4.3.3) » pour les autres) : `Rscript tests/controles/plateforme.R` (relevé de la plateforme ; `--exiger` en CI : R 4.3.3, BLAS et LAPACK de référence, monothread, image épinglée ; ADR 0008), `Rscript tests/controles/liste_blanche_description.R` (dépendances de `DESCRIPTION`, ADR 0002), `Rscript tests/controles/graphe_modules.R` (ordre des modules, ADR 0007), `Rscript tests/controles/identite_batteries.R` (batteries identiques entre `CLAUDE.md`, `BATTERIES` et `ci.yml` ; chaîne d'image unique), `Rscript tests/controles/absence_java.R` (aucun package Java, `rJava`, `xlsx` ou `xlsxjars`, dans les dépendances récursives de l'application, de la contre-vérification et du moteur, listées dans `tests/controles/packages_hors_moteur.csv` ; instantané daté du CRAN chez Posit Package Manager, réseau requis ; `--ajouter ELT` pour le cas témoin, qui doit échouer ; ADR 0002, A1 ; ADR 0008, point 7).
- Lancement de l'application et appel du moteur seul : à venir avec les branches qui les créent.

## Façon de travailler

- **Une branche de travail à la fois**, au périmètre fermé d'issues, PR brouillon dès la création ; fusion par le mainteneur, par commit de fusion, CI verte.
- **Ceux qui écrivent ne vérifient pas, ceux qui vérifient n'écrivent pas** ; un agent n'entre dans le circuit que si la modification touche son domaine.
- **Toute affirmation sur le code s'adosse à une mesure** exécutée ; tout changement de résultat a son tableau avant / après et son visa.
- **Audit léger en cours, revue finale complète avant la sortie du brouillon** ; documentation écrite une seule fois, en fin de branche.
- **Les issues ne sont créées qu'avec l'accord du mainteneur** ; les workflows ne commitent, ne poussent et ne créent rien.

Détail : `CLAUDE.md`.

## Routage du modèle et de l'effort

`architect`, `actuary` et `regulatory` tournent sur Opus par défaut : effort `medium` pour la routine et `high` pour le jugement, Fable réservé à une liste fermée de cas ou à l'accord du mainteneur, avec des plafonds d'escalade (`docs/agents/routage.md`, ADR 0001 annoté le 14/10/2026). Les contrats partagés du projet (signature de `construire_table()`, catalogue des rôles et recettes, format du plan et de la fiche, objet résultat, surface de mortalité, tables de référence et provenances, jeux simulés, critère de non-régression) et le seuil « macro » (un module touché suffit) sont fixés au § 4 du routage.

- **Vérification** : `bash .claude/outils/fiches_jumelles.sh --verifier` (aussi en CI) ; puis, dans une **session neuve** (les fiches ne sont pas rechargées en cours de session), une consultation de chaque fiche et `bash .claude/outils/bilan_journal.sh`, qui affiche le modèle réellement servi ; les escalades, relances ciblées et arrêts sont notés dans la PR.
- **Limites** : un plafond de tours n'est pas un plafond de tokens, une réponse courte ne borne pas le raisonnement, l'effort effectif n'est pas observable dans le journal, et la politique ne supprime pas les angles morts des modèles.

## Sécurité du dépôt

Réglages à reproduire sur le dépôt (`Gerard-Garey/outil_tables`) ; indispensables puisque le dépôt est public :

- fusion par **commit de fusion seulement** (squash et rebase désactivés), wiki désactivé ;
- **secret scanning** et **push protection** activés ; **alertes et correctifs de sécurité Dependabot** activés ;
- Actions limitées à celles de GitHub (`github_owned_allowed`), jeton des workflows en lecture seule, actions épinglées par SHA ;
- ruleset **« Protection main »** : suppression et force-push interdits, PR obligatoire (fusion par commit de fusion, fils de discussion résolus), contrôles requis « Contrôles du dépôt », « Tests (R 4.3.3) » et « Absence de Java » à jour avec `main`, sans contournement.

```bash
R=Gerard-Garey/outil_tables
gh api -X PATCH repos/$R -F allow_squash_merge=false -F allow_rebase_merge=false -F allow_merge_commit=true -F has_wiki=false \
  -f 'security_and_analysis[secret_scanning][status]=enabled' -f 'security_and_analysis[secret_scanning_push_protection][status]=enabled'
gh api -X PUT repos/$R/vulnerability-alerts
gh api -X PUT repos/$R/automated-security-fixes
gh api -X PUT repos/$R/actions/permissions -F enabled=true -f allowed_actions=selected
gh api -X PUT repos/$R/actions/permissions/selected-actions -F github_owned_allowed=true -F verified_allowed=false
gh api -X PUT repos/$R/actions/permissions/workflow -f default_workflow_permissions=read -F can_approve_pull_request_reviews=false
gh api -X POST repos/$R/rulesets --input .github/ruleset-main.json
```

Le ruleset est dans `.github/ruleset-main.json`. L'appliquer **après** le premier push sur `main`, qu'il bloque ensuite. **Appliqué le 14/10/2026** (id 25116580), avec les autres réglages ci-dessus et les libellés de `.github/creer_labels.sh`.

**Mise à jour du ruleset** (par exemple pour ajouter aux contrôles requis les jobs de tests que la première branche de travail créera dans `ci.yml`) : modifier `.github/ruleset-main.json` (liste `required_status_checks`, un `context` par nom de job), puis `gh api -X PUT repos/$R/rulesets/25116580 --input .github/ruleset-main.json` ; vérifier avec `gh api repos/$R/rulesets/25116580 --jq '.rules[] | select(.type=="required_status_checks")'`. Un job absent de cette liste n'est pas bloquant, même s'il échoue.
