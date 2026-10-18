# CLAUDE.md

Ce fichier guide Claude Code (claude.ai/code) dans ce dépôt.

## Contexte

`outil_tables` construit des **tables d'expérience** (cas principal : tables de mortalité, du moment puis prospectives ; ensuite rachat, incapacité-invalidité, dépendance, chômage des emprunteurs) à partir d'une base d'observation d'un portefeuille d'assurés : estimation des taux d'une transition (noyau multi-états), lissage ou positionnement sur une table de référence, fermeture, validation, suivi d'expérience. Les tables et les rapports produits servent d'**hypothèses biométriques du best estimate** Solvabilité II (revues par l'ACPR) et de support à la **certification** d'une table d'expérience par un actuaire indépendant agréé (Code des assurances, A132-18 ; règlement ANC 2015-11). Ce qui prime : la rigueur (chaque chiffre a sa provenance et son statut), la traçabilité documentation ↔ code ↔ résultats, et la reproductibilité (une construction se rejoue à l'identique à partir de sa fiche de construction). Le dépôt ne contient **aucune donnée réelle** : tous les jeux sont simulés sous une loi vraie connue et une graine. Contraintes fortes : des millions de lignes sur des dizaines d'années, et des bases modestes à traiter honnêtement ; R 4.3.3 ; moteur en R base.

Textes qui font foi, par régime (détail : `docs/exigences.md` § 2.4) : best estimate — directive 2009/138/CE art. 77, règlement délégué (UE) 2015/35 art. 22 (chocs art. 137, 138, 152, 153 ; art. 218 : **aucun paramètre propre à l'entreprise ne porte sur la mortalité**), orientations EIOPA, notice ACPR du 17/12/2015 ; comptes sociaux et certification — Code des assurances A132-18, A132-19 et annexe (**A335-1 est abrogé** : ne jamais le citer), arrêtés d'homologation, règlement ANC 2015-11, Lignes directrices mortalité de l'Institut des actuaires (2006), rapport de la Commission d'agrément (2015), notes II1291-11 à -15. Les tables homologuées se lisent sur le fac-similé du JO, jamais par extraction de texte. Les faits sourcés du cadrage sont dans `docs/recherche/2026-10-14-cadre-tables-experience.md` (statuts vérifié / incertain / non trouvé) ; les décisions du mainteneur dans `docs/cadrage/2026-10-14-entretien-cadrage.md` et `docs/feuille-de-route.md` § 4.

Le dépôt de référence est `https://github.com/Gerard-Garey/outil_tables` (public, licence PolyForm Noncommercial 1.0.0), utilisé depuis le poste local et depuis des sessions cloud. `docs/exigences.md` contient le cahier des charges : le lire avant toute évolution de fond ou de l'interface.

## Commandes

Les commandes courantes (lancer, tester, compiler) sont dans `README.md` : s'y reporter plutôt que de les recopier ici.

Batteries de vérification, lancées depuis la racine du dépôt par `coder`, `audit`, le workflow `circuit-technique` et le job « Tests (R 4.3.3) » de la CI (contrôle requis du ruleset de `main`) ; la liste `BATTERIES` de `.claude/workflows/circuit-technique.js` et les étapes « Batterie : » de `.github/workflows/ci.yml` sont identiques à ce bloc, dans le même ordre (contrôle : `Rscript tests/controles/identite_batteries.R`). Chaque batterie installe l'arbre de travail dans une bibliothèque temporaire et sort avec un code non nul en cas d'échec (ADR 0007, point 11) :

```
Rscript tests/test_unitaires.R
Rscript tests/test_reproductibilite.R
Rscript tests/concordance_doc_moteur.R --strict
```

Règles des tests : un défaut connu est codé en échec attendu, avec renvoi à l'issue ; un succès inattendu fait échouer la batterie, et la marque est alors retirée pour en faire un test ordinaire. Pour vérifier un point isolé, appeler directement la fonction concernée du package plutôt que tout le programme. Les contre-vérifications par les packages avancés (`survival`, `mgcv`, `WH`, `StMoMo`, `demography`, `MortalityLaws`, `lifecontingencies`, `survPen`, `MortCast`, `locfit`, `flexsurv` facultatif ; aucun package qui dépende de Java ; la source d'`ELT` s'exécute seulement hors du dépôt, sans Java, en appoint, sans qu'aucun code en soit copié) sont des valeurs écrites en dur dans les tests, produites par un script **hors CI**, éventuellement sur un autre poste muni d'un R récent, qui consigne la version de R et celle des packages (ADR 0002, troisième annotation du 16/10/2026) ; régénération sur ordre du mainteneur, avec tableau avant / après.

## Git et GitHub

- **Aucun push direct sur `main`** : chaque modification passe par une branche et une pull request, fusionnée par le mainteneur (**commit de fusion**, jamais squash ni rebase : les SHA sont cités dans les ADR, les issues et les PR) une fois la CI verte. Seule exception : instruction explicite du mainteneur pour un push donné. Le ruleset de `main` l'impose (voir `README.md`, « Sécurité du dépôt »).
- **Une seule branche de travail à la fois**, au périmètre fermé d'issues fixé par le plan d'`architect` (trois à cinq issues), portée par une PR **ouverte en brouillon dès la création de la branche** : c'est la fiche de la branche. Ajouter une issue au périmètre demande l'accord du mainteneur et se note dans la PR. Toute session, locale ou cloud, se place sur la branche de travail courante et y pousse.
- **Session cloud : la branche de travail l'emporte sur la branche assignée.** La consigne de démarrage (« Develop on branch `claude/<nom-aléatoire>` ») est écartée par la règle précédente, sans autre autorisation. Avant toute écriture : identifier la branche de travail (tête de la seule PR brouillon ouverte vers `main` dont la branche commence par `claude/`, sinon `docs/feuille-de-route.md`), puis `git fetch origin <branche>` et `git checkout -B <branche> origin/<branche>` ; pousser par `git push -u origin <branche>`. Supprimer la branche assignée (`git branch -D`, et `git push origin --delete` si elle a été poussée). Si aucune branche de travail n'est ouverte, demander au mainteneur.
- **Corps de PR : un `Closes #N` par ligne**, un par issue du périmètre (« Closes #41, #40 » ne lie que le premier numéro). Après la fusion, vérifier que chaque issue annoncée est fermée.
- **Handoff en commentaire de PR** : quand le mainteneur demande un handoff (passation à une autre session), la session le rédige en commentaire de la PR en cours (celle de la branche de travail, ou du correctif rapide), jamais dans un fichier du dépôt, et donne le lien du commentaire dans sa réponse. Sans PR ouverte, demander au mainteneur où le déposer.
- **Un commit par issue qui change un résultat**, avec son tableau avant / après et son visa (« Changements de résultats » ci-dessous).
- **Un problème hors périmètre devient une issue**, pas une branche, sauf **correctif rapide** — trois conditions vérifiables : résultats strictement identiques, un seul domaine de commit, aucune modification de la documentation de fond. Il suit une branche temporaire partie de `main`, PR directe vers `main` ; `main` est ensuite fusionnée dans la branche de travail.
- **Création d'issue sur accord du mainteneur** : agents et sessions rédigent l'issue proposée (titre, libellés, corps) dans leur compte rendu ; elle n'est créée qu'une fois approuvée, sauf autorisation explicite du brief. Le corps d'une issue rédigée par un agent commence par `> *Rédigé par l'agent <nom> (IA).*`.
- **Messages de commit en français**, avec accents, préfixés par le domaine et renvoyant à l'issue (`#3`) quand elle existe. Domaines : `moteur:` (package `tablexp`), `app:` (`app/`), `tests:`, `docs:` (`docs/`, README, documentation LaTeX), `donnees:` (jeux simulés, tables de référence et fichiers de provenance), `claude:` (`CLAUDE.md`, `.claude/`), `repo:` (`.github/`, `.gitignore`, `DESCRIPTION`, licence).
- **Auteur des commits faits par Claude** (sessions, sous-agents, workflows ; poste local comme cloud) : `Claude <noreply@anthropic.com>` (`git config user.name Claude` et `git config user.email noreply@anthropic.com` dans le dépôt), jamais l'identité du mainteneur ; le pied de message garde `Co-Authored-By` et, en session cloud, `Claude-Session`.

## Architecture (contrainte impérative)

Décidée au cadrage (ADR 0002 à 0006) ; `docs/exigences.md` § 4 fait foi.

- **Package `tablexp`** (`R/`) : **toute** la logique quantitative — lecture des rôles et validation des données, recettes d'exposition, estimation brute, lissage, positionnement, prospective, fermeture, validation et suivi d'expérience, simulation, quantités des rapports et empreintes —, dans un module par famille (`donnees`, `exposition`, `brut`, `lissage`, `positionnement`, `prospective`, `fermeture`, `validation`, `simulation`, `rapport`), plus deux fichiers transversaux : `R/construire_table.R` (orchestration seule) et `R/outils.R` (graine locale, refus et avertissements, constantes de plateforme ; sans contenu métier, n'appelle aucun module) ; graphe d'appels des modules contrôlé par script (ADR 0007) ; avec un **point d'entrée unique** `construire_table(donnees, roles, plan)` (plus l'argument facultatif `provenance`, ADR 0007, point 3.1) qui exécute un plan de construction d'un bloc et retourne un objet complet. Le moteur reçoit un **data.frame et des rôles, jamais un fichier**. Dépendances : **packages de priorité `base` seulement** (`stats`, `splines`, `parallel`, `utils`, `tools`… ; ni CRAN, ni `recommended` : `survival`, `mgcv` et `Matrix` sont exclus du moteur, ADR 0002) ; exception par ADR propre à chaque méthode, pour un package disponible sur le poste de travail qui fait gagner significativement en temps de développement ou en efficacité (ADR 0002, annotation du 16/10/2026) ; **aucune dépendance à Java**, directe ou indirecte, dans tout le projet ; R pur, mesuré, avant tout code compilé (interface native `.Call` ou C++ par `Rcpp`), sur mesure de lenteur documentée et par ADR (ADR 0002, point 6 et A8). Utilisable sans Shiny : aucun `input$`, `reactive()`, `render*()`.
- **Noyau multi-états** : une table est l'estimation d'une transition ; chaque risque est une configuration du noyau, pas un moteur à part.
- **Lecture des fichiers** (CSV en R base, xlsx par `openxlsx` optionnel sans dépendance du calcul) : couche mince hors moteur.
- **`app/affichage.R`** : formatage, tables HTML, tracés à partir des quantités du résultat uniquement. Aucun calcul.
- **`app/app.R`** : interface Shiny locale, hors du package (dossier `app/`, lancée par `shiny::runApp("app")` ; n'appelle que des fonctions exportées de `tablexp`, jamais `tablexp:::` ; ADR 0007, point 1.3) ; zone principale, panneau de paramètres à droite ; déclaration des rôles, édition du plan, appel de `construire_table()` **uniquement** sur clic du bouton « Relancer les calculs », stockage du résultat, affichage. Aucun calcul, aucune édition des données en ligne. Tout contrôle de saisie ayant un sens métier existe aussi dans le package.
- **Données** : jeux simulés (loi vraie et graine consignées) et tables de référence embarquées avec leur fichier de provenance, sous `inst/extdata/` ; références de non-régression sous `tests/reference/`.

Si un résultat peut être calculé indépendamment de l'interface, il va dans le package.

## Changements de résultats et reproductibilité

- À entrées, rôles, plan et graine identiques, le programme produit des résultats identiques au bit près sur une même machine ; toute source d'aléa passe par une graine locale explicite sous un générateur fixé, consignée dans le résultat.
- Toute modification qui change un résultat est **identifiée, quantifiée et expliquée** : un **tableau avant / après** (grandeur, avant, après, écart, explication), une ligne par grandeur modifiée, chaque ligne expliquée par la modification ; une ligne inexpliquée est une régression à corriger, pas une référence à régénérer.
- **Visa** : tout changement d'une **table finale**, d'un **verdict de validation** ou d'une **valeur de table homologuée embarquée** est soumis au mainteneur ; les autres changements sont validés par `actuary`. Sans visa, rien n'est commité.
- Les références de non-régression ne sont régénérées qu'après visa, par la session principale (jamais par un agent ni un workflow), **sur la CI seule** (workflow de régénération, `ubuntu`, R 4.3.3, BLAS et LAPACK de référence monothread, plateforme consignée dans le message du commit de référence) ; aucune régénération sur un poste, celui du mainteneur compris. Comparaison aux références valeur par valeur, à tolérance, structure contrôlée à part.

## Rigueur

- **Une affirmation sur le comportement du code s'adosse à une mesure exécutée** (commande et sortie), citée dans le compte rendu. Une explication plausible non vérifiée est la façon la plus sûre d'introduire une erreur qui survit aux relectures.
- **Un chiffre ne se recopie pas, il se remesure** ; un chiffre qui vient d'une source (texte, table homologuée, publication) se vérifie contre cette source, citée.
- Ne fabriquer aucune référence, aucun numéro d'article, aucune valeur de table ni résultat ; si la source ne permet pas de conclure, l'écrire.
- Faire évoluer les livrables existants plutôt que les réécrire ; ne jamais remplacer silencieusement une méthode ni réintroduire une formule déjà corrigée.
- Quatre statuts ne se confondent jamais : résultat exact, asymptotique, approximation numérique, constat de simulation ; l'erreur Monte-Carlo (nombre de réplications) se distingue de l'erreur d'estimation (exposition). Une recette d'exposition ne devine jamais : elle refuse. Rien n'est corrigé en silence dans les données (`docs/exigences.md` § 2).

Le vocabulaire du projet est défini dans `CONTEXT.md` : l'employer tel quel dans le code, la documentation et les issues.

## Sous-agents

Sept sous-agents de projet (`.claude/agents/` ; onze fiches : `architect`, `actuary` et `regulatory` ont une variante `-approfondi`, `coder` sa variante Opus `coder-approfondi`), orchestrés par la session principale. Règle de séparation : **ceux qui écrivent ne vérifient pas, ceux qui vérifient n'écrivent pas**.

| Famille | Agent | Écrit | Question |
|---|---|---|---|
| Pilotage | `architect` | `docs/adr/`, `CONTEXT.md`, `docs/feuille-de-route.md` | Dans quel ordre, avec quels agents, sous quelle forme ? |
| Fond | `actuary` | rien (avis, plans, issues proposées) | Est-ce juste actuariellement et statistiquement, dans les conditions du projet ? |
| Fond | `regulatory` | rien (matrice de conformité, issues proposées) | Est-ce exactement ce que prescrivent les textes et les règles de certification ? |
| Réalisation | `coder` | code du package et de l'application, tests (pas `docs/latex/`) ; surface d'impact documentaire dans le commit proposé | Comment l'implémenter ? |
| Réalisation | `docwriter` | `docs/latex/` | Documentation juste, rigoureuse, conforme à `docs/latex/CONVENTIONS.md`, concordante avec le code ? |
| Vérification | `audit` | rien (rapport) | Code correct et reproductible ? |
| Vérification | `app-review` | rien (rapport) | Interface conforme à `docs/exigences.md` § 5 ? |

**Routage du modèle et de l'effort** (`docs/agents/routage.md`, ADR 0001 annoté) — `architect`, `actuary` et `regulatory` existent en deux fiches au même corps : la fiche de base (Opus, effort `medium`, routine) et la fiche `-approfondi` (Opus, effort `high`, jugement), générée par `bash .claude/outils/fiches_jumelles.sh` et contrôlée par la CI ; ne jamais modifier une fiche `-approfondi` à la main. La session principale choisit la fiche selon la matrice de `docs/agents/routage.md` (§ 3) et ne passe `model: "fable"` à l'appel que dans les cas du § 4.1 (échec documenté d'Opus `high`, désaccord entre agents, rédaction d'un ADR d'architecture) ou sur accord du mainteneur ; au plus une relance ciblée, une hausse d'effort et une consultation Fable par question, trois consultations Fable par branche sans nouvel accord ; un modèle indisponible arrête le circuit (aucun remplacement silencieux). Les agents signalent les critères rencontrés dans leur bloc « Retour », ils ne décident pas de leur escalade ; la session note chaque escalade, relance ciblée ou arrêt d'une ligne dans la PR de la branche de travail.

**Modèle et effort des agents de réalisation et de vérification** (`docs/agents/routage.md`, § 10, ADR 0010) — fixés par leur fiche : `coder` Sonnet, effort `high` ; `coder-approfondi` (même corps, générée par `fiches_jumelles.sh`) Opus, effort `medium` ; `audit` et `docwriter` Opus, effort `medium` ; `app-review` Sonnet, effort `high`. En circuits 1 et 2 (évolution ou correction de fond), la session appelle `coder-approfondi` d'office, reprises comprises. Ailleurs — tâche de codage longue ou complexe, issue de circuit 3 finie en `arrete` parce que `coder` n'a pas corrigé les constats après la reprise —, elle le **propose** au mainteneur et ne l'emploie qu'avec son accord, au cas par cas, sans plafond chiffré (dans le workflow `circuit-technique`, option `--approfondi`). L'effort `high` pour `coder-approfondi` (paramètre `effort` de l'appel ; `--approfondi=high`) demande aussi son accord. La revue finale complète d'`audit` (règle 10) est appelée avec `effort: "high"`. Chaque emploi de `coder-approfondi` hors circuits 1 et 2, et chaque passage de `coder-approfondi` à l'effort `high`, est noté d'une ligne dans la PR.

**Déclencheurs** — un agent n'entre dans le circuit que si la modification touche son domaine : plusieurs issues ou forme du package → `architect` en amont ; méthode, estimateur, test ou critère de validation → `actuary` (spécification en amont, validation en aval) ; texte, règle de certification, table homologuée ou sa provenance → `regulatory` (lecture du texte en amont, contrôle de conformité en aval) ; `R/` du package ou `tests/` → `audit` ; `app/` (`app/app.R`, `app/affichage.R`) → `app-review` ; fond de `docs/latex/` → `docwriter`, en dernier, une seule fois par branche (règle 9).

**Circuits types** (chacun se termine par un ou plusieurs commits de la session principale sur la branche de travail) :

1. **Évolution de fond** : `actuary` spécifie → `coder` → `audit` (+ `app-review` si l'interface change) → `docwriter` (en fin de branche) → `actuary` valide.
2. **Correction de conformité** (écart à un texte, à une règle de certification ou à une table homologuée) : `regulatory` établit la lecture du texte → `actuary` éclaire l'interprétation → le mainteneur tranche si une table finale ou un verdict change → `coder` → `audit` → `regulatory` contrôle la conformité → `docwriter`.
3. **Correction technique** : `coder` → `audit` (workflow `circuit-technique`).
4. **Documentation seule** : `docwriter` (+ `actuary` si le fond change, `regulatory` si un texte ou une table homologuée est touché).

Un constat bloquant ou majeur d'un vérificateur renvoie à l'étape de réalisation. Quand un texte admet deux lectures, `regulatory` les décrit, `actuary` donne son avis, le mainteneur tranche.

**Workflows** (`.claude/workflows/`). Un workflow met en œuvre un circuit type et en fixe les appels d'agents, les sorties structurées et les conditions d'arrêt. Huit principes :

1. lancement **sur commande explicite du mainteneur** uniquement ;
2. **aucune écriture dans l'historique ni l'état partagé** par le workflow ou ses agents : ni `git commit`, ni `git push`, ni régénération de référence, ni création d'issue — interdiction portée par les consignes et les fiches, les permissions de `.claude/settings.json` ne l'imposant pas : la session principale vérifie `git status` et `git log` après chaque workflow ;
3. **arrêt avec les rapports à tout point de décision** (question de fond, visa du mainteneur, contradiction entre vérificateurs) ;
4. **au plus une reprise** `coder` → `audit` ;
5. **vérificateurs ciblés sur le diff** (audit léger) ;
6. **constats structurés** : gravité (bloquant / majeur / mineur), `fichier:ligne`, mesure exécutée ; un constat sans emplacement ni mesure n'est pas recevable ;
7. **tout contrôle mécanique est un script**, jamais un agent : un agent lit la sortie d'un script, il ne refait pas le décompte ; un contrôle récurrent sans script en appelle un (issue) ;
8. **effort réduit** pour les étapes mécaniques ; aucun `model` fixé dans un workflow : un agent lancé avec `agentType` prend le modèle de sa fiche, les autres celui de la session (ordre documenté par Claude Code, pas encore mesuré dans un workflow : le confirmer par le modèle servi, `bash .claude/outils/bilan_journal.sh`, au premier passage).

Un **constat** (défaut du code) conduit à la reprise ou à l'arrêt ; une **question pour `actuary`** ou pour `regulatory` (doute de `coder`, question de l'audit) ne relance pas `coder`. Trois statuts de fin : `termine`, `termine avec questions` (les seules remontées sont des questions : la session principale les porte à l'expert concerné avant tout commit), `arrete` ; une batterie en échec à la dernière vérification finit toujours par `arrete`.

**Règle 9 — un seul passage de `docwriter` par branche** : `docwriter` intervient une fois, en fin de branche, sur l'état final du code, avec **un commit `docs:` par issue** ; `coder` ne modifie pas `docs/latex/` et liste dans chaque commit proposé la **surface d'impact documentaire** (sections, tableaux, décomptes, fonctions citées), dont `docwriter` part ; `actuary` valide le diff de la documentation une fois, en fin de branche. Exceptions : commit `docs:` préparatoire qui protège la suite, branche où le document porte la décision et précède le code (lecture réglementaire, circuit 2), écart de concordance qui ferait échouer la CI (commit `docs:` minimal, aussitôt).

**Règle 10 — audit léger en cours, revue finale complète avant la sortie du brouillon** : pendant l'implémentation, `audit` (et `app-review`) se limitent au diff et aux fonctions touchées avec leurs appelants et appelés ; avant la sortie du brouillon, revue finale complète du `git diff main...HEAD` entier (fonctions touchées avec appelants et appelés, batteries complètes, scénarios adverses, cohérence code ↔ tests ↔ documentation) par les vérificateurs que désignent les déclencheurs, plus `/code-review` ; toute correction postérieure est revue à son tour, sur son diff. La relecture intégrale hors diff est réservée aux jalons de livraison.

**Revues périodiques**, hors de tout changement : `architect` (point d'étape et feuille de route, après chaque série de PR fusionnées) ; `actuary` (revue de fond avant livraison, et à chaque jalon de méthodes v1 / v2 / v3) ; `regulatory` (matrice de conformité complète avant livraison et après toute transcription de table homologuée) ; `app-review` et `docwriter` (relecture intégrale avant démonstration ou livraison).

## Décisions et vocabulaire

- **ADR** (`docs/adr/NNNN-titre-court.md`, gabarit `docs/adr/0000-gabarit.md`) : toute décision d'architecture ou d'organisation, et toute piste rejetée pour une raison qu'un futur relecteur devrait connaître. Rédigés par `architect` ; une décision du mainteneur y est datée. Une proposition qui contredit un ADR le signale explicitement et dit pourquoi le rouvrir.
- **`CONTEXT.md`** : glossaire du domaine et de l'organisation.
- **`docs/feuille-de-route.md`** : périmètre de la branche de travail en cours et des suivantes, décisions du mainteneur numérotées (M1, M2…).
- **`docs/cadrage/`** : comptes rendus des entretiens de cadrage (décisions Q, suppositions A) ; **`docs/recherche/`** : rapports de recherche d'agents, avec leurs statuts de vérification.

## Agent skills

### Issue tracker

Issues et specs dans les GitHub Issues du dépôt : outils `mcp__github__*` en session cloud, CLI `gh` sur le poste local. Voir `docs/agents/issue-tracker.md`.

### Triage labels

Les cinq libellés canoniques (`needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, `wontfix`). Voir `docs/agents/triage-labels.md`.

### Domain docs

Mono-contexte : un `CONTEXT.md` et `docs/adr/` à la racine. Voir `docs/agents/domain.md`.
