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

## Annotation du 11 novembre 2026 — points 3 et 5 : forme lue par le contrôle mécanique `.claude/outils/branche_de_travail.sh` ; feuille qui fait foi pendant une vague (#137, M403, M404)

Rédigée par `architect-approfondi` au commit (0) de la porteuse de la vague lissage (`claude/lissage-whittaker`, partie de `main` à `9697f0c`), sur les décisions de forme du mainteneur du 11/11/2026 (M403) et à la demande de la réserve r2 de l'audit du correctif rapide #137 (PR #219, `8183c8e`, fusion `9697f0c`). Annotation de **précision** : le texte ci-dessus n'est pas réécrit ; elle fixe la forme que le script lit, qui est désormais la forme due du § 1 de `docs/feuille-de-route.md` pendant une vague. Le nom de l'objet fait foi (M77, (b)) ; les règles ci-dessous sont lues dans l'en-tête et le code du script à `9697f0c` (*vérifié*, lecture).

**(a) Marqueur de vague (point 5, troisième tiret).** Le script ne reconnaît une vague que par une ligne qui commence par « Vague autorisée », suivie de « : <nom>, <date> », dans la **zone du § 1** : entre le titre `## 1.` et la première sous-section `###` (ou le titre `##` suivant). Sans cette ligne, il conclut « hors vague », même si un tableau existe (le tableau est alors ignoré), et applique la règle ordinaire (seule PR brouillon `claude/` ouverte vers `main`). Une ligne sans nom ni date est un écart. La ligne **transcrit** l'autorisation du mainteneur ; elle ne la remplace pas (point 0) : elle est écrite par la porteuse (point 2), qui cite la décision M-n.

**(b) Tableau des branches ouvertes (point 5).** Seul le **premier** tableau de la zone dont la première cellule d'en-tête est « Branche » est lu. La branche est le premier mot entre accents graves de la première cellule ; une ligne sans tel mot est un écart. En vague, la branche contrôlée doit figurer au tableau, et **toute PR brouillon `claude/` ouverte vers `main` absente du tableau est un écart**.

**(c) Partition déclarée (point 3).** La colonne dont l'en-tête est **exactement** « Partition (motifs) » est lue ; une colonne en prose ne l'est pas, et le contrôle de la partition déclarée est alors sauté, avec un message. Chaque cellule liste des motifs glob séparés par des virgules (accents graves facultatifs ; une virgule ne peut pas figurer dans un motif). Ce sont des motifs de `case` de bash : `*` couvre aussi `/`, et `[!…]` exclut des caractères ; un motif terminé par `/` vaut tout le répertoire. Une cellule vide est un écart (partition non déclarée) ; une branche qui ne doit écrire aucun fichier contrôlé (branche documentaire) déclare donc les motifs de ce qu'elle écrit (par exemple `docs/exigences.md`, `docs/recherche/`), qui ne désignent aucun fichier contrôlé. Le contrôle se limite aux fichiers de `R/`, de `tests/` et de `docs/adr/` (point 3) ; les autres sont ignorés, et la sortie le dit. Il porte sur `git diff --name-only <base>...<branche>` (base : `origin/main`) : intersections deux à deux vides, et aucun fichier hors de la partition déclarée.

**(d) Fichiers partagés (point 3, premier tiret).** Toujours admis : `docs/latex/doc_outil_tables.tex`, `docs/latex/doc_outil_tables.pdf`, `tests/concordance_attente.csv`. D'autres s'ajoutent par une ligne facultative de la zone qui commence par « Fichiers partagés admis : », suivie des chemins entre accents graves. `NAMESPACE` (porteuse seule) est hors du périmètre contrôlé : la règle reste vérifiée à la revue.

**(e) Avant fusion.** Par défaut, la branche doit être la tête d'une PR **brouillon** ; l'option `--avant-fusion` admet une PR ouverte non brouillon (pour la branche et pour celles de `--partition`), pour le contrôle qui précède chaque fusion (point 3, deuxième tiret).

**(f) Feuille qui fait foi pendant une vague.** Le script lit la feuille de l'arbre de travail. Or seule la porteuse écrit `docs/feuille-de-route.md` (point 2) : les branches sœurs portent la copie de `main`, sans marqueur ni tableau tant que la porteuse n'est pas fusionnée (elle fusionne en dernier, point 4). Pendant une vague, la feuille qui fait foi pour le point 5 est donc celle de la **tête de la porteuse** (`origin/<porteuse>`). Une session de branche sœur lance le script sur cette copie, par la variable `BRANCHE_TRAVAIL_FEUILLE` (`git show origin/<porteuse>:docs/feuille-de-route.md` dans un fichier temporaire). C'est une conséquence du point 2, non une règle nouvelle ; le mainteneur peut en décider autrement.

**(g) Limites (en-tête du script).** Le script ne fait aucun `git fetch` (préalable requis) et n'écrit rien. Sans accès à l'API GitHub, ou au-delà de 100 PR ouvertes vers `main` par le repli `curl`, il sort en code 3 sans conclure.

**Effet sur les résultats : aucun.**

Issues : #137. Décisions M193, M194, M403, M404. Commits `8183c8e`, `9697f0c`.

## Annotation du 12 novembre 2026 — point 7 : retrait du marqueur de vague à la fin d'une vague (M428)

Rédigée par `architect` au commit (0) de la vingtième branche `claude/suites-bytes` (partie de `main` à `16131cb`), sur la décision du mainteneur du 12/11/2026 (M428, D7 du point d'étape qui suit B19). Annotation de **précision** du point 7 et de l'annotation du 11/11/2026, (a) : le texte ci-dessus n'est pas réécrit.

**Constat** (*vérifié* le 12/11/2026 à `16131cb`). La porteuse fusionne en dernier (point 4) et elle seule écrit la feuille de route (point 2) : le marqueur de vague et le tableau des branches ouvertes qu'elle porte arrivent donc sur `main` par sa fusion, alors que la vague expire à cette même fusion (point 7). À `16131cb`, `bash .claude/outils/branche_de_travail.sh claude/lissage-consolidation` concluait encore à une vague déclarée (marqueur de la vague lissage, trois branches au tableau). Conséquences : une branche nouvelle y est signalée « absente du tableau » ; une session qui lit la feuille de `main` peut croire la vague ouverte.

**Précision.**
1. Quand aucune autre branche de la vague n'est ouverte, la porteuse retire le marqueur et le tableau des branches ouvertes à sa consignation de fin, avant la sortie du brouillon ; la feuille garde une phrase qui date la fin de la vague et renvoie au SHA où le tableau se lit. Le contrôle `--avant-fusion` de la porteuse s'applique alors hors vague (règle ordinaire : seule PR brouillon `claude/` ouverte vers `main`, ou PR non brouillon admise par `--avant-fusion`).
2. À défaut, le commit (0) de la branche suivante retire le marqueur et le tableau ; la session ne lance `branche_de_travail.sh` sur cette branche qu'après ce commit (sur l'arbre de travail de la branche), puis ouvre la PR brouillon.
3. Le retrait ne vaut ni prolongation, ni clôture anticipée : la vague a expiré à la fusion de sa dernière branche (point 7) ; il met la forme lue par le script en accord avec cet état.

**Application.** Vague lissage : marqueur et tableau retirés par le commit (0) de `claude/suites-bytes` (cas 2) ; texte intégral à `16131cb` (`git show 16131cb:docs/feuille-de-route.md`, § 1).

**Effet sur les résultats : aucun.**

Issues : #137, #230. Décisions M404, M422, M428. Commit de référence `16131cb`.
