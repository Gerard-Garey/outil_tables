---
status: accepted
date: 2026-10-14
---

# Le moteur exécute un plan de construction d'un bloc ; la fiche de construction (plan, empreintes, versions) suffit à rejouer la table

## Contexte

Construire une table enchaîne des étapes (exposition, taux bruts, lissage ou positionnement, fermeture, validation, table) dont chacune a des paramètres, et le dossier (Q1) doit montrer la chaîne entière, pas la dernière étape. `outil_USP` a résolu le problème analogue par un bouton unique qui appelle le moteur une fois et retourne un objet complet, et par un rapport figé construit sur cet objet avec ses empreintes (son `docs/exigences.md`, § 5.1 et § 5.6 ; Q8, points 3 à 5, repris). Deux formes étaient possibles ici : (A) un plan exécuté d'un bloc ; (B) une interface par étapes, chaque onglet lançant son étape sur le résultat du précédent. Le mainteneur a retenu (A) en se disant ouvert (Q22) ; Q11, Q27, Q32 et Q35 en précisent le contenu.

## Décision

Arrêtée par le mainteneur le 14/10/2026 (Q8 points 3 à 5, Q11, Q22, Q27, Q32, Q35 ; Q36 pour le bouton unique et l'export du plan) :

1. **Plan de construction** (`CONTEXT.md`, « Plan de construction ») : suite ordonnée d'étapes et de leurs paramètres, passée entière à `construire_table(donnees, roles, plan)` (ADR 0004) et exécutée **d'un seul bouton**. Aucun onglet ne lance d'étape ; les onglets affichent l'objet retourné. Le plan est édité dans l'application et **exporté / importé** (Q36).
2. **Comparer des méthodes, c'est exécuter plusieurs plans** sur les mêmes données ; la comparaison (Q32) porte sur les q_x, les espérances de vie résiduelles et les **facteurs de rente à un taux technique déclaré** ; elle figure dans le rapport.
3. **Étape « marge » explicite** (Q32) : la prudence est une étape du plan dont le paramètre est **toujours déclaré, jamais un défaut** ; une table sans marge l'est par un paramètre explicite (marge nulle), inscrit dans la fiche.
4. **Table unique** (Q27 ; « Table unique ») : voie (A), combinaison des tables par sexe à pondération **déclarée** (voie par défaut), ou voie (B), estimation sur la population regroupée ; voie et pondération sont des paramètres du plan et figurent dans la fiche. *(16/10/2026, décision M46, amendement B8 de la relecture M36, E2.10 amendée : la voie (A) n'est plus une « voie par défaut » mais la voie **recommandée** ; la **voie**, la **grandeur combinée** (q_x ou l_x) et la **pondération** sont des paramètres déclarés du plan, **sans valeur par défaut**, et figurent dans la fiche, comme la marge (point 3). Le texte ci-dessus n'est pas réécrit.)*
5. **Exclusions** (Q35) : les règles d'exclusion de lignes sont des paramètres du plan, par motif ; le rapport de validation les compte et les liste (ADR 0003, point 7).
6. **Fiche de construction** (« Fiche de construction ») : écrite par le moteur avec le résultat, lisible par machine ; contient le plan tel qu'exécuté (paramètres effectifs, recette et convention retenues ou forcées, ADR 0003), les rôles, l'unité d'observation, la période d'observation et le délai de déclaration, la graine (Q8, point 4), les versions (R, `tablexp`, plateforme), les deux empreintes des données (Q37), l'empreinte du résultat et celle du code (E3.6). **Rejouer la fiche sur les mêmes données redonne la même table** : c'est un test.
7. **Livrables** (Q11) : (a) la table en CSV et xlsx avec en-tête de métadonnées (résumé de la fiche) ; (b) le rapport figé HTML autonome (« Rapport figé » ; `outil_USP` § 5.6 repris : construit sur l'objet retourné, jamais sur la saisie courante, sans calcul, empreintes en en-tête) ; (c) la fiche ; (d) PDF hors v1. Le suivi d'expérience a son propre rapport figé (Q26).
8. **Non-régression** (Q8, point 3) : références produites par la CI sur la plateforme de référence (R 4.3.1, ADR 0002 ; *R 4.3.3 depuis le 16/10/2026, décision M43, ADR 0002, quatrième annotation*), comparées valeur par valeur à tolérance ; un plan de référence par méthode de la v1 ; tout changement passe par le tableau avant / après et le visa (`CLAUDE.md`).

## Options écartées

- **(B) Interface par étapes** (chaque onglet exécute son étape, l'état vit dans l'application) : un état réparti entre onglets n'est ni un objet ni reproductible ; la fiche ne pourrait être dérivée qu'en reconstituant l'historique des clics ; comparer deux méthodes obligerait à rejouer les onglets à la main ; contraire au bouton unique (`outil_USP` § 5.1) et à la règle « l'application ne reconstruit pas de résultats à partir de calculs dispersés » (§ 4.3). Le mainteneur reste ouvert (Q22) : rouvrir (B) demande un ADR qui montre une fiche aussi complète et un objet aussi reproductible.
- **Marge par défaut** (« 0 % » implicite, ou une marge « usuelle ») : une prudence non déclarée est indéfendable devant un certificateur ; la valeur est choisie et visible (Q32).
- **Pondération implicite de la table unique** (par l'exposition observée, sans le dire) : le résultat dépendrait du portefeuille sans que la fiche le dise ; la pondération est déclarée (Q27), la pondération 0,6 / 0,4 de l'arrêté du 18/11/2024 (recherche § 1.a, vérifié) n'étant qu'une valeur possible.
- **Fiche reconstituée par l'application** (journal des saisies) : l'application ignore quelle recette ou convention le moteur a retenue ; seule la fiche écrite par le moteur reflète ce qui a été exécuté.
- **Rapport construit sur la saisie courante** : exclu par `outil_USP` § 5.6, repris ; un rapport ne décrit que ce qui a été calculé.

## Conséquences

- Fichiers : module `rapport` (fiche, rapport figé, en-têtes de métadonnées) ; signature de `construire_table` (ADR 0004) ; format du plan et de la fiche (contrat partagé ; sérialisable en R de base, l'ADR 0002 excluant `jsonlite` du moteur ; format fixé au plan de la première branche) ; tests de rejeu de la fiche et de non-régression ; `docs/exigences.md` : E3.5 et E3.6 (livrables, contenu du rapport), E4.5 à E4.7, E5.1 et E5.4.
- Effet sur les résultats : aucun à ce jour. Règle durable : le plan transmis au moteur est complet, de sorte qu'une valeur proposée par l'application ne change aucun résultat du moteur tant qu'elle n'a pas été inscrite dans le plan ; un changement dans l'exécution d'une étape change des résultats et suit le tableau avant / après.
- Ce que l'ADR ne règle pas : le format de sérialisation du plan et de la fiche ; la liste des étapes de la v1 et de leurs paramètres (spécification d'`actuary`, Q30) ; le contenu détaillé du rapport figé et la disposition des onglets (Q36, ouverte) ; la place du bootstrap (Q31) comme étape ou comme option de l'étape « taux bruts ».
- Vocabulaire : `CONTEXT.md`, « Plan de construction », « Fiche de construction », « Rapport figé », « Table unique », « Tableau avant / après », « Visa ».

Issues : aucune à ce jour.

## Annotation du 16 octobre 2026 — format de sérialisation du plan et de la fiche reporté à la branche `claude/fiche-rapport` ; structure en mémoire fixée par l'ADR 0007

Décision d'`architect` du 16/10/2026 dans le cadre du plan de la première branche accepté par le mainteneur (décision M45, `docs/feuille-de-route.md` § 1 et § 2 : la branche `claude/fiche-rapport` porte « sérialisation du plan et de la fiche (export, import, rejeu) ») ; consignée par l'ADR 0007, point 12. Le texte du 14/10/2026 n'est pas réécrit.

**Écart déclaré.** « Conséquences » ci-dessus veut le format du plan et de la fiche « fixé au plan de la première branche ». La première branche (`claude/socle-donnees-exposition`, PR #12) ne fixe **que la structure en mémoire** : le plan est une liste R nommée validée sans défaut implicite (ADR 0007, point 5 : `transition`, `periode`, `delai_declaration`, `graine`, `etapes`), la fiche une liste nommée dans l'objet résultat de classe `construction` (ADR 0007, point 6), et le rejeu se teste sur l'objet (`construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance)` redonne un résultat `identical` hors `execution`). Le **format de sérialisation** (export et import du plan, E5.4 ; écriture de la fiche, livrable (c) du point 7) est **reporté** à la branche `claude/fiche-rapport`, première branche qui aura un consommateur du format (export, import, rapport figé) et pourra donc le tester : le fixer sans consommateur serait le fixer sans test.

**Contraintes posées dès maintenant** (ADR 0007, point 12.2), que le format devra satisfaire : (a) texte lisible par un relecteur ; (b) aller-retour identique — relire ce qui a été écrit redonne un objet `identical`, nombres compris (doubles en `%.17g`, dates en ISO 8601, chaînes en UTF-8, `NA` distingué de la chaîne « NA ») ; (c) **aucune évaluation de code à l'import** : ni `dget()`, ni `source()`, ni `eval(parse())` ; un plan importé est une donnée, validée comme un plan construit en mémoire ; (d) écrit et lu en R de base (l'ADR 0002 exclut `jsonlite` du moteur) ; (e) versionné (`version_fiche`).

**Points 6 et 8 précisés par l'ADR 0007** : l'empreinte des données reçues est un MD5 de `tools::md5sum` sur une représentation textuelle canonique définie au point 9.3 de l'ADR 0007, l'algorithme étant nommé dans la fiche (P8, M45) ; les empreintes du résultat et du code sont des champs présents à `NA` jusqu'à la branche `fiche-rapport` ; la graine et le générateur (`Mersenne-Twister`, `Inversion`, `Rejection`) figurent dans la fiche (ADR 0007, point 8).

**Effet sur les résultats** : aucun (aucun code).

Issues : #11 (objet résultat et fiche en mémoire) ; branche `claude/fiche-rapport` (à rédiger).

## Annotation du 19 octobre 2026 — point 6 : le plan de la fiche est le plan reçu canonique ; « plan effectif » = plan canonique + paramètres retenus (ADR 0011)

Rédigée par `architect-approfondi` (Fable, consultation 3/3 de la branche B4, M62, Q3). Statut : **proposé, sous réserve de l'acceptation de l'ADR 0011** par le mainteneur. Le texte du 14/10/2026 et l'annotation du 16/10/2026 ne sont pas réécrits.

Le point 6 veut que la fiche contienne « le plan tel qu'exécuté (paramètres effectifs, recette et convention retenues ou forcées) » ; E4.5 dit « le plan effectif » ; l'ADR 0007, point 6.2, dit « le plan reçu, inchangé » (écart (iii) relevé le 19/10/2026, `docs/feuille-de-route.md`). L'ADR 0011 (point 5) fixe la lecture :

- `fiche$plan` est le **plan reçu mis sous forme canonique** — ordre du schéma, types canoniques, sans conversion de sens —, canonisé une seule fois à l'entrée de `construire_table()` après `valider_plan()` ; deux plans de même sens donnent le même `fiche$plan`, les mêmes `etapes` et le même objet hors `execution` ; c'est lui qu'on rejoue ;
- la fiche **n'écrit jamais un plan résolu** : une déclaration `plus_fine` reste `plus_fine` ; la convention retenue, la recette, l'unité d'observation, la fin effective et les hypothèses de chaque étape sont les **paramètres retenus** (`etapes[[étape]]$parametres` et les champs correspondants de la fiche) ;
- le **plan effectif** du point 6 et d'E4.5 est la **paire** (plan canonique, paramètres retenus) ; la certification lit la paire, le rejeu n'emploie que le premier terme.

Le format de sérialisation reporté par l'annotation du 16/10/2026 est fixé par l'ADR 0011 (points 6 à 10), qui remplace le point 12 de l'ADR 0007 ; les livrables (c) du point 7 et l'export-import d'E5.4 en relèvent. Une note de lecture d'E4.5 est soumise au mainteneur (ADR 0011, question 6).

**Effet sur les résultats** : aucun par cette annotation (voir ADR 0011, « Conséquences »).

Issues : #42, #22 (point 6), #50 ; ADR 0011.

## Annotation du 24 octobre 2026 — point 7 (b) et « Conséquences » : le rapport figé est rendu par le module `rapport` en octets, écart déclaré avec E4.1 ; point 6 : le rejeu a une fonction (ADR 0012)

Rédigée par `architect-approfondi` (Fable, consultation 1/3 de la branche `claude/rapport-fige`, B8, PR #88 ; `docs/agents/routage.md`, § 4.1.3 ; décision M97). Statut : **proposé, sous réserve de l'acceptation de l'ADR 0012** par le mainteneur. Le texte du 14/10/2026 et les annotations des 16 et 19/10/2026 ne sont pas réécrits.

**(a) Point 7 (b) et « Conséquences » (« module `rapport` (fiche, rapport figé, en-têtes de métadonnées) ») : confirmés, précisés.** Le rapport figé HTML autonome est produit par le module `rapport`, dans `R/rapport.R`, par la fonction exportée `ecrire_rapport(x)` qui rend des **octets** UTF-8 ; le fichier est écrit par l'appelant, comme la fiche (ADR 0011, point 8.1), ce qui est la lecture que l'ADR 0012 donne de l'ADR 0004, point 3 (« le moteur fournit l'objet et ses métadonnées, l'application les écrit » : l'application écrit le **fichier**). Le document est construit sur l'objet retourné, sans calcul, chaque nombre écrit par l'écrivain unique en `%.17g`, sans horodatage, avec un contrôle d'intégrité par l'empreinte du résultat avant le rendu (ADR 0012, points 1 et 2).

**(b) Écart déclaré avec E4.1.** E4.1 range dans le module `rapport` les « quantités du rapport, pas son rendu ». La présente annotation et l'ADR 0012 s'en écartent si « rendu » désigne le rapport figé ; ils y sont conformes si « rendu » désigne l'affichage interactif (`app/affichage.R`), lecture que l'ADR 0012 recommande (question 1) et qui rend E4.1, l'ADR 0004 et le présent ADR cohérents entre eux. La décision revient au mainteneur, par note de lecture ou amendement de `docs/exigences.md`.

**(c) Point 6 (« Rejouer la fiche sur les mêmes données redonne la même table : c'est un test »).** Le rejeu reste un test (`tests/test_reproductibilite.R`) et devient aussi une **fonction exportée**, `rejouer_fiche(donnees, fiche)`, dans `R/construire_table.R`, qui applique les contrôles du point 12 de l'ADR 0011 dans un ordre fixé, refuse par un motif nommé à la première discordance, appelle `construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance)` une seule fois, et rend l'objet avec un bilan séparé (contrôles et témoins), hors de l'objet (ADR 0012, point 3). « Redonne la même table » se lit toujours au sens d'E4.6 : au bit près sur la même machine, à tolérance par classes ailleurs ; l'empreinte du résultat n'est qu'un témoin.

**(d) Point 7 (a) et (d), inchangés** : tables CSV et xlsx à en-tête reportées à la première branche qui produit une table finale (M93) ; PDF hors v1. Le rapport figé du suivi d'expérience (Q26) rouvrira la question d'un module `rendu` (ADR 0012, point 1.4).

**Effet sur les résultats** : aucun (voir ADR 0012, « Conséquences »).

Issues : #86, #87 ; ADR 0012. Branche `claude/rapport-fige`, PR #88.
