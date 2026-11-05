---
status: accepted
date: 2026-11-05
---

# Branches de travail parallèles bornées, sur autorisation explicite du mainteneur pour une vague nommée et datée : au plus deux branches de code et une branche documentaire, partition des fichiers déclarée au plan, une seule branche porteuse

*Proposé par `architect-approfondi` sur Fable le 05/11/2026, à la demande expresse du mainteneur (consultation Fable 1/3 de B12, `claude/dette-regles`, PR #134 ; décision M199). Accepté par le mainteneur le 05/11/2026 (« J'accepte tout ; par contre il faut que cette exception ne soit permise que sur autorisation du mainteneur. »), avec la condition d'autorisation intégrée aux points 0, 7 et 8 et aux Conséquences (décision M193). Transcrit par `architect`.*

Numéros de ligne et mesures à `e007ebb` (`main`) et `1e40608` (`claude/dette-regles`) ; le nom de l'objet fait foi (M77, (b)).

## Contexte

`CLAUDE.md` (« Git et GitHub ») impose une seule branche de travail à la fois, identifiée en session cloud par « la seule PR brouillon ouverte vers `main` dont la branche commence par `claude/` ». Une exception unique (M63, 19/10/2026 ; règles à `2146717`, § 1.4.5 de `docs/feuille-de-route.md`) a fait avancer B4, P1 et P2 en parallèle, fusionnées en deux jours dans l'ordre prévu, sans régénération hors de B4.

Mesures du 05/11/2026 :

- douze branches depuis le 14/10/2026, aucune méthode actuarielle nouvelle depuis B2 (18/10/2026) ;
- le chemin critique (A) attend des décisions et des avis hors code depuis le 19/10/2026 (#6, #124) ;
- la branche ouverte B12 touche les sept fichiers de `R/` et les fichiers centraux `tests/unitaires/test_controles.R` et `tests/controles/locale_c.R` (`git diff --name-only main...origin/claude/dette-regles`) : aucun lot de code ne lui est disjoint ;
- chaque branche close a changé 230 à 976 lignes de `docs/latex/doc_outil_tables.tex` et le PDF versionné : deux branches qui documentent en même temps entrent en conflit binaire certain sur le PDF ;
- parmi les lots prévus, seule (A) régénérera les références.

Le mainteneur envisage de faire avancer plusieurs branches en parallèle (sessions cloud simultanées). Il en garde la maîtrise : l'exception n'existe que sur son autorisation.

## Décision

Arrêtée par le mainteneur le 05/11/2026 (décisions M193 et M194), sur proposition d'`architect-approfondi` sur Fable, à sa demande :

0. **Autorisation du mainteneur** : aucune vague parallèle ne s'ouvre, ne se prolonge, ni ne reçoit de branche supplémentaire sans **autorisation explicite du mainteneur, donnée pour une vague nommée** (liste fermée de branches) **et datée**, consignée par une décision M-n qui cite le présent ADR. Ni `architect`, ni une session, ni un agent, ni un workflow ne peuvent la décider, ni l'inférer d'une autorisation antérieure, d'un plan qui « prévoit » une vague, ou du silence du mainteneur. Une vague prévue au plan n'est pas une vague autorisée. Hors d'une vague autorisée, la règle ordinaire de `CLAUDE.md` (une seule branche de travail) s'applique sans exception.
1. **Plafond** : au plus **deux branches de code** et **une branche documentaire** ouvertes en même temps. Une branche documentaire n'écrit ni dans `R/`, ni dans `tests/`, ni dans `docs/latex/` ; elle écrit dans `docs/recherche/`, `docs/exigences.md`, dans les seuls fichiers de `docs/adr/` que le plan de la vague lui attribue (point 2), et dans les commentaires d'issues. Chaque branche garde un périmètre fermé de trois à cinq issues ; une issue n'appartient qu'à une branche.
2. **Une seule branche porteuse** par vague, nommée au plan. Elle seule peut :
   - changer un résultat (tableau avant / après, visa) ;
   - régénérer ou créer une référence (`references.yml`) ;
   - incrémenter `VERSION_FICHE` ;
   - écrire `docs/feuille-de-route.md` et `CONTEXT.md` ;
   - attribuer les numéros M et les numéros d'ADR.

   Les ADR suivent la partition des fichiers (point 3) : la porteuse écrit les ADR nouveaux et les annotations, sauf les fichiers de `docs/adr/` que le plan de la vague attribue nommément à une autre branche ; pour un ADR nouveau, la porteuse réserve le numéro au plan. Un fichier d'ADR n'est écrit que par une seule branche pendant la vague.

   Les autres branches rendent des résultats strictement identiques (empreinte du code exceptée, M70). Devant un changement de résultat, un contrat partagé ou un ADR non attribué par le plan, elles s'arrêtent et rendent la main au mainteneur (M63, règle 7). Elles proposent leurs décisions et leurs termes nouveaux dans le corps de leur PR ; la porteuse les consigne.
3. **Partition des fichiers déclarée au plan** : pour chaque branche, la liste des fichiers de `R/`, de `tests/` et de `docs/adr/` qu'elle peut modifier ; aucune intersection entre branches ouvertes.
   - Fichiers partagés admis, nommés : `docs/latex/doc_outil_tables.tex` et `.pdf` (sections disjointes déclarées), `tests/concordance_attente.csv`, `NAMESPACE` (porteuse seule).
   - Contrôle à chaque point d'étape et avant chaque fusion : `git diff --name-only main...<branche>` pour chaque branche ouverte ; intersection vide hors fichiers partagés.
   - Tests nouveaux dans un fichier `tests/unitaires/test_<sujet>.R` propre à la branche, jamais ajoutés en fin d'un fichier existant (abroge la règle 9 de M63).
4. **Ordre de fusion fixé au plan**, porteuse en dernier. Après chaque fusion dans `main` (branche ou correctif rapide), `main` est fusionnée dans chaque branche ouverte. La branche qui fusionne :
   - recompile le PDF et met `\datedoc` à jour dans le commit de fusion ;
   - rejoue les trois batteries, dont la concordance `--strict` ;
   - remesure son tableau avant / après, s'il existe, contre `main` fusionnée.
5. **Identification de la branche** : tant que dure une vague autorisée, la règle « seule PR brouillon » de `CLAUDE.md` est suspendue.
   - Le brief de chaque session nomme sa branche.
   - Le titre de la PR porte `[B<n>]` (porteuse), `[P<n>]` (branche de code parallèle) ou `[D<n>]` (branche documentaire).
   - Le § 1 de `docs/feuille-de-route.md` tient un tableau des branches ouvertes (branche, PR, rôle, partition, ordre de fusion).
   - Une session vérifie que sa branche figure dans ce tableau avant toute écriture ; sans brief qui nomme une branche listée, elle s'arrête et demande ; jamais de choix par défaut.
   - Une branche = une session active ; jamais de push forcé.
6. **Agents et plafonds** : les règles 9 et 10 de `CLAUDE.md` valent par branche. La revue finale complète suit la dernière fusion de `main` dans la branche ; une fusion postérieure n'est admise que si sa résolution ne touche aucun fichier de la partition de la branche, sinon audit léger de la résolution. Une consultation Fable faite hors branche est imputée à la branche nommée par le brief ; les plafonds de `docs/agents/routage.md`, § 5.2, valent par branche.
7. **Durée** : l'autorisation vaut pour la seule vague nommée et datée (point 0) et expire à la fusion de la dernière de ses branches, ou plus tôt sur décision du mainteneur ; la règle ordinaire reprend alors de plein droit. Une nouvelle vague, la prolongation d'une vague ou l'ajout d'une branche à une vague ouverte demandent chacun une nouvelle autorisation explicite du mainteneur, consignée par une décision M-n qui cite cet ADR, sans nouvel ADR.
8. **Première vague** (autorisée le 05/11/2026, M194) : B12 `claude/dette-regles` (porteuse) et `claude/sources-v1` (documentaire, `[D1]`). Vagues suivantes **prévues, non autorisées** (M198) : vague 2, après la fusion de B12, `claude/controles-portables` (parallèle) et (A) ou `claude/recette-stocks-successifs` (porteuse), jamais les deux ; vague 3, `claude/rapport-libelles`, après la session Windows (M184). Chacune attend l'autorisation du mainteneur au moment de son ouverture.

## Options écartées

- **Parallélisme général (n branches, pas de porteuse)** : `R/rapport.R`, `R/construire_table.R`, `R/exposition.R` et `test_controles.R` sont touchés par presque tous les lots ; les tableaux avant / après et les régénérations ne se composent pas ; le mainteneur est le goulet (visas, fusions).
- **Deux branches de code dès maintenant** : aucune n'est disjointe de B12 (mesure ci-dessus) ; le gain serait absorbé par les conflits de `locale_c.R`, `test_controles.R`, `rapport.R` et du PDF.
- **(A) et `claude/recette-stocks-successifs` en parallèle** : #82 et `construire_table.R` leur sont communs ; M168 fixe leur ordre.
- **Reconduire M63 tel quel par décision M-n** : M63 se dit « unique et non reconductible » ; une règle stable doit être consignée là où un relecteur la cherchera.
- **ADR d'une branche documentaire rédigés en projet (corps de PR, commentaire d'issue) puis recopiés par la porteuse** : double transcription, risque d'écart entre projet et texte consigné, et la porteuse écrirait un fichier qu'elle ne relit pas sur le fond. L'attribution nominative d'un fichier de `docs/adr/` au plan (point 2) garde la règle d'écrivain unique par fichier sans recopie.
- **Exception permanente, ouverte à l'appréciation d'`architect` ou de la session** : écartée par le mainteneur (point 0) ; le coût d'attention d'une vague lui revient, la décision aussi.
- **Scinder `docs/latex/doc_outil_tables.tex` en un fichier par module pour supprimer le conflit** : utile mais hors de cet ADR (concordance, `\input`, PDF unique restent) ; à proposer en issue si les conflits se répètent.

## Conséquences

- **Autorisation** : toute vague, sa prolongation et toute branche ajoutée sont des décisions du mainteneur, nommées et datées, consignées en M-n au § 4 de `docs/feuille-de-route.md` ; le plan d'`architect` peut proposer une vague, jamais l'ouvrir. Une session qui ne trouve pas sa branche dans le tableau du § 1, ou dont la vague n'est pas autorisée, s'arrête et demande.
- `CLAUDE.md`, « Git et GitHub » : puces 2 et 3 amendées par la session principale (exception bornée, uniquement sur autorisation explicite du mainteneur pour une vague nommée).
- `CONTEXT.md` : « Branche de travail » amendée ; entrées « Vague », « Branche porteuse », « Branche documentaire ».
- `docs/feuille-de-route.md`, § 1 : tableau des branches ouvertes.
- `docs/agents/routage.md`, § 5.2 et § 7 : plafonds par branche ; imputation des consultations hors branche à la branche nommée par le brief.
- `.github/pull_request_template.md` : case « branche de la vague : partition respectée » (à faire par la porteuse, `repo:`).
- Contrôle mécanique à créer (issue à proposer au mainteneur) : script `.claude/outils/branche_de_travail.sh` (vérifie que la branche nommée est tête d'une PR brouillon ouverte et listée au § 1 ; code non nul sinon) et contrôle d'intersection des partitions.
- Effet sur les résultats : aucun.
- Ce que l'ADR ne règle pas : le coût d'attention du mainteneur ; la scission éventuelle du `.tex` ; la fusion des correctifs rapides dans chaque branche (point 4, charge proportionnelle au nombre de branches).

Issues : #112 à #136 (lots), #6, #13, #124 (chemin critique), #82 (seam partagé). Décisions : M63 (remplacée pour sa clause « non reconductible »), M168, M184, M193 à M199.
