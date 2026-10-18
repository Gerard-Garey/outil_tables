---
status: accepted
date: 2026-10-18
---

# Sonnet `high` pour coder, coder-approfondi sur Opus en circuits 1 et 2 ou sur accord du mainteneur, Opus pour audit et docwriter, effort fixé dans chaque fiche de réalisation et de vérification (reprise de l'ADR 0002 du dépôt modèle)

## Contexte

L'ADR 0001 (annoté le 14/10/2026) règle le modèle et l'effort d'`architect`, d'`actuary` et de `regulatory` seulement. Avant cette décision, `coder`, `audit` et `docwriter` avaient `model: opus` sans `effort`, `app-review` `model: sonnet` sans `effort` (frontmatter de `.claude/agents/*.md` sur `origin/main`, lu par `git diff origin/main`) : leur effort était celui de la session, et leur consommation montait avec lui.

Le dépôt modèle `Gerard-Garey/Modele_vibe_code` a tranché la question dans son ADR 0002, « Sonnet `high` pour coder, coder-approfondi sur Opus pour le fond ou sur accord du mainteneur, Opus pour audit et docwriter, effort fixé dans chaque fiche de réalisation et de vérification » (PR Gerard-Garey/Modele_vibe_code#3, commit de fusion `930b22186f2da8eac7b44f373c5de2a5771bcede`, `930b221`). Son contexte établit, sources citées, les faits sur lesquels repose la décision ; ils ne sont pas recopiés ici, ni revérifiés dans ce projet :

- une fiche sans `effort` prend celui de la session ; les limites d'usage de l'abonnement sont comptées aussi par famille de modèles, et Opus coûte plusieurs fois plus par tour que Sonnet (documentation et support de Claude Code, sans ratio chiffré publié) ;
- depuis Claude Code 2.1.292, l'appel d'un sous-agent non-fork accepte un paramètre `effort` qui l'emporte sur la fiche, ce qui rend caduque une prémisse de l'ADR 0001 (annotation du 18/10/2026 de cet ADR) ; ordre de choix du modèle : appel, fiche, `CLAUDE_CODE_SUBAGENT_MODEL`, session ;
- bancs publiés par Anthropic : sur du codage délimité, Sonnet 5.5 en effort `high` atteint le niveau d'Opus 5.5 en effort `low` pour un coût moindre, Opus restant plus fort sur le travail ouvert qui demande un jugement soutenu ; faiblesses documentées de Sonnet 5.5 (débordement de périmètre, arrêt avant la fin aux efforts bas, bilan factuel inférieur sans recherche). Chiffres et citations : ADR 0002 du dépôt modèle, section « Contexte ».

Aucune mesure de qualité ni de consommation propre à ce projet : le mainteneur a écarté l'essai préalable, dans le dépôt modèle puis ici.

Numéro : 0010, le 0009 étant pris par `docs/adr/0009-references-de-non-regression.md` sur la branche de travail `claude/taux-bruts-non-regression` (PR Gerard-Garey/outil_tables#18, non fusionnée).

Branche : `claude/modele-agents-execution`, PR brouillon Gerard-Garey/outil_tables#29, ouverte sur instruction du mainteneur hors de la branche de travail, sans issue ; le mainteneur a préféré une branche dédiée à l'intégration dans la PR #18.

**Mesures faites dans ce projet le 18/10/2026** (Claude Code 2.1.302, API Anthropic : `ANTHROPIC_BASE_URL=https://api.anthropic.com`, aucune variable Bedrock, Vertex ni Foundry) :

- `bash .claude/outils/fiches_jumelles.sh`, puis `diff -q` des fiches avant et après : `architect-approfondi.md`, `actuary-approfondi.md` et `regulatory-approfondi.md` identiques (le nouveau code du script ne change pas les fiches de jugement) ; `bash .claude/outils/fiches_jumelles.sh --verifier` : code de sortie 0 (« Fiches -approfondi conformes a leur fiche de base. »).
- Modèle servi, dans la session qui venait de modifier les fiches (`bash .claude/outils/bilan_journal.sh`) : `app-review` → `claude-sonnet-5-5` ; `audit` (appelé avec `effort: "high"`) → `claude-opus-5-5` ; `coder` → `claude-opus-5-5` ; `docwriter` → `claude-opus-5-5` ; `coder-approfondi` : « Agent type 'coder-approfondi' not found ». Le modèle d'`app-review`, d'`audit` et de `docwriter` ne change pas avec cette décision : seul `coder` départage. Servi en Opus alors que sa fiche porte `model: sonnet`, il montre que la fiche modifiée n'a **pas** été rechargée en cours de session, contrairement au constat du dépôt modèle (son ADR 0002, « Contexte ») et conformément au constat 3 de `docs/agents/issue-tracker.md` ; une fiche créée en cours de session est inconnue de l'outil `Agent`. L'effort n'est pas journalisé.
- Test en session neuve (session cloud démarrée le 18/10/2026 sur `claude/modele-agents-execution` à `f227d05`, mêmes appels sans paramètre `model`, `audit` avec `effort: "high"` ; `bash .claude/outils/bilan_journal.sh`) : `coder` → `claude-sonnet-5-5` ; `coder-approfondi` → `claude-opus-5-5` ; `audit` → `claude-opus-5-5` ; `docwriter` → `claude-opus-5-5` ; `app-review` → `claude-sonnet-5-5` ; aucune erreur d'appel. Conforme à la décision ; bilan complet en commentaire de la PR #29.

## Décision

Arrêtée par le mainteneur le 18 octobre 2026, dans la session qui a ouvert la PR #29 : « Porte dans ce projet le changement de modèle et d'effort des agents de réalisation et de vérification décidé dans le dépôt modèle Gerard-Garey/Modele_vibe_code (PR Gerard-Garey/Modele_vibe_code#3, fusionnée ; ADR 0002 de ce dépôt). C'est ma décision de mainteneur : pas d'essai préalable. »

La décision du dépôt modèle est reprise dans son état final (ADR 0002 du modèle, points 1 à 8, à `930b221`), avec les adaptations au projet que voici.

1. **`coder` et `coder-approfondi`** : `coder`, fiche `model: sonnet`, `effort: high`, par défaut ; sa fiche reçoit une consigne explicite de périmètre (ni test, ni fichier, ni documentation que la tâche ne demande pas ; règle 9). `coder-approfondi` : même corps et mêmes outils, générée par `bash .claude/outils/fiches_jumelles.sh` ; `coder` est ajouté aux rôles dédoublés (`ROLES="architect actuary regulatory coder"`) ; la fonction `parametres` du script lui donne le modèle `opus`, l'effort `medium` et aucun plafond de tours propre (celui de `coder.md`, aucun aujourd'hui) ; ne jamais la modifier à la main. En circuits 1 et 2 de `CLAUDE.md` (**évolution de fond**, **correction de conformité**), chaque appel de `coder`, reprises comprises, va **d'office** à `coder-approfondi`.
2. **`coder-approfondi` ailleurs, sur accord du mainteneur** : la session le **propose** pour une tâche de codage longue ou complexe (exemples : `docs/agents/routage.md`, § 10), ou pour reprendre une issue de circuit 3 finie en `arrete` parce que `coder` n'a pas corrigé les constats après la reprise ; elle ne l'emploie qu'avec l'accord du mainteneur, au cas par cas, sans plafond chiffré. L'effort `high` pour `coder-approfondi` (paramètre `effort` de l'appel) demande aussi son accord. Dans le workflow `circuit-technique`, l'option `--approfondi` (`--approfondi=high` pour l'effort `high`), que seul le mainteneur donne en lançant le workflow, vaut accord ; le rapport indique la fiche employée ; une option mal formée arrête le workflow ; une relance avec `--approfondi` après un arrêt exige un arbre de travail propre (procédure au § 10 de `routage.md`). Chaque emploi de `coder-approfondi` hors circuits 1 et 2, et chaque passage à l'effort `high`, est noté d'une ligne dans la PR (`fiche / modèle / critère déclenché / statut obtenu / suite`, § 7 de `routage.md`). Ce choix de fiche n'est pas une escalade : ni dossier d'escalade ni plafond par question (§ 5 de `routage.md`) ; la validation du mainteneur en tient lieu.
3. **`audit`** : fiche `model: opus`, `effort: medium` ; la **revue finale complète** (règle 10) est appelée avec le paramètre `effort: "high"`.
4. **`docwriter`** : fiche `model: opus`, `effort: medium`.
5. **`app-review`** : fiche `model: sonnet` (inchangé), `effort: high`. Le dossier `app/` n'existe pas encore (application sous `app/` : ADR 0007, point 1.3, accepté par la décision M46) ; la fiche existe, son réglage s'appliquera quand l'interface arrivera.
6. **Étapes mécaniques du workflow** (état du dépôt, batteries : `agentType: 'audit'`, `effort: 'low'`) : inchangées, de même que les listes `BATTERIES` et `ZONES_PROTEGEES` de `.claude/workflows/circuit-technique.js`, propres au projet. Haiku pour ces étapes est **reporté**, comme dans le modèle.
7. **`architect`, `actuary`, `regulatory`** : inchangés (ADR 0001 annoté) ; la notion « routine / jugement » ne vaut que pour eux.
8. **Principe 8 des workflows** (`CLAUDE.md`), correction factuelle reprise du modèle : « aucun `model` fixé dans un workflow : un agent lancé avec `agentType` prend le modèle de sa fiche, les autres celui de la session (ordre documenté par Claude Code, pas encore mesuré dans un workflow : le confirmer par le modèle servi, `bash .claude/outils/bilan_journal.sh`, au premier passage) ».
9. **Renvois** : dans ce projet, les mentions « ADR 0002 » des fichiers repris du modèle deviennent « ADR 0010 ». L'ADR 0002 du projet porte sur R et la politique de dépendances : les deux ne se confondent pas, et l'ADR 0002 du modèle se cite toujours avec le nom du dépôt modèle.

## Options écartées

Les options écartées par le dépôt modèle valent ici ; leurs motifs, avec les chiffres des bancs publiés, sont dans son ADR 0002 (« Options écartées », `930b221`). En résumé :

- **Essai préalable** sur quelques issues : écarté par le mainteneur, dans le modèle puis ici (« pas d'essai préalable ») ; la calibration (§ 8 et § 10 de `routage.md`) en tient lieu, après coup.
- **Statu quo** (Opus et effort hérité de la session) : consommation non maîtrisée, toute la réalisation sur la limite de la famille Opus.
- **Opus `low` pour `coder`** : rapport qualité / coût voisin de Sonnet `high` sur le banc cité par le modèle, mais sur la limite Opus, déjà la plus sollicitée.
- **`model: "opus"` passé à l'appel de `coder`** (première version de la décision du modèle) : remplacé par une fiche nommée, visible dans le journal et le rapport du workflow, générée du même corps, sélectionnable par `agentType` sans paramètre `model` (principe 8).
- **Plafond chiffré pour `coder-approfondi`**, comme pour Fable : écarté par le mainteneur du modèle au profit d'une validation au cas par cas ; possible à la calibration si les emplois se multiplient.
- **`coder-approfondi` en effort `high` par défaut** : gain mesuré sur le banc cité, jugé insuffisant pour le surcoût sur chaque appel de fond ; `high` sur accord.
- **Sonnet `medium` pour `coder`** : nettement moins bon qu'en `high` sur le banc de codage agentique cité, et arrêts avant la fin plus fréquents.
- **Haiku pour `coder`, `audit` ou `docwriter`** : moins bon en codage agentique complexe et sujet à l'arrêt avant la fin dans un long prompt d'agent ; le `CLAUDE.md` de ce projet fait 24 376 octets (`wc -c CLAUDE.md`, arbre de travail de la branche, 18/10/2026), plus que celui du modèle.
- **Haiku pour les étapes mécaniques** : reporté, non rejeté (gain faible en `low`, amendement du principe 8 nécessaire).
- **Sonnet pour `audit`** : `audit` est le dernier filet avant le commit ; même modèle que `coder`, angles morts probablement partagés (hypothèse du modèle, non mesurée).
- **Sonnet pour `docwriter`** : un seul passage par branche (règle 9), donc gain faible ; dans ce projet, le livrable est relu par des tiers (actuaire certificateur, ACPR).
- **Porter le changement dans la branche de travail `claude/taux-bruts-non-regression`** (PR #18) : écarté par le mainteneur, consulté par la session, au profit d'une branche dédiée (option recommandée par la session : le changement ne touche aucun résultat et sort du périmètre de la PR #18).

## Conséquences

- Fichiers : `.claude/agents/coder.md` (frontmatter, mention « Fiche de routine », consigne de périmètre), `audit.md`, `docwriter.md`, `app-review.md` (frontmatter) ; `.claude/agents/coder-approfondi.md` (générée) ; `.claude/outils/fiches_jumelles.sh` (`ROLES`, fonction `parametres`, contrôle de `maxTurns` selon le rôle) ; `.claude/workflows/circuit-technique.js` (option `--approfondi`, choix de la fiche de `coder`, arrêt sur une option mal formée, fiche indiquée au rapport, commentaire du principe 8) ; `CLAUDE.md` (décompte des fiches passé de dix à onze, paragraphe « Modèle et effort des agents de réalisation et de vérification », principe 8) ; `docs/agents/routage.md` (titre, introduction, § 2, § 8, § 9, nouveau § 10) ; `CONTEXT.md` (entrée « Routine / jugement » précisée, entrée « `coder` / `coder-approfondi` ») ; `README.md` ; `.github/pull_request_template.md` (ligne de trace de `coder-approfondi`) ; annotation datée de l'ADR 0001.
- Effet sur les résultats : **aucun** (organisation des agents seulement ; ni `R/`, ni `tests/`, ni données, ni références touchés).
- À faire : au premier passage du workflow `circuit-technique`, vérifier que `agentType` charge bien le modèle de la fiche (principe 8) ; calibrer à chaque point d'étape d'`architect` (§ 10 de `routage.md`).
- Suite proposée au mainteneur (issue à rédiger, non créée) : réexaminer le mécanisme des deux fiches d'`architect`, d'`actuary` et de `regulatory` (ADR 0001, décision 1), puisque l'effort peut désormais être passé à l'appel ; le dépôt modèle a fait la même proposition pour son `architect` et son `expert`.
- Ce que l'ADR ne règle pas : la décision repose sur des bancs publiés, non sur le travail du projet ; l'effort effectif reste non observable dans le journal, si bien que la prise en compte du paramètre `effort` à l'appel ne se vérifie pas directement ; le rechargement des fiches en cours de session diverge entre le dépôt modèle et ce projet (mesure ci-dessus) : le constat 3 de `docs/agents/issue-tracker.md` reste valable ici ; hors de l'API Anthropic, l'alias `sonnet` peut désigner une version antérieure (ADR 0002 du modèle, « Conséquences ») : un changement de fournisseur rouvre le point 1 ; « tâche de codage longue ou complexe » n'a pas de seuil mesurable (les exemples du § 10 de `routage.md` en tiennent lieu, l'accord du mainteneur tranche) ; le réglage d'`app-review` reste sans emploi tant que `app/` n'existe pas. Retour arrière, complet ou partiel : procédure du § 10 de `routage.md` (de préférence `git revert -m 1` du commit de fusion de la PR #29), puis annotation des ADR 0001 et 0010.

Issues : aucune (branche `claude/modele-agents-execution`, PR #29, ouverte sur instruction du mainteneur hors branche de travail).
