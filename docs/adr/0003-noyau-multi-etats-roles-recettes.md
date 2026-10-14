---
status: accepted
date: 2026-10-14
---

# Un noyau multi-états unique : une table estime une transition, les rôles déclarés choisissent la recette d'exposition

## Contexte

L'outil doit couvrir, dans l'ordre, mortalité / longévité, rachat, incapacité-invalidité, dépendance et chômage des emprunteurs (Q2), à partir de lignes individuelles, d'agrégats, de stocks successifs « et au-delà » (Q6), sur des bases allant de modestes à des millions de lignes (Q7, Q21). Les méthodes de référence partagent le même squelette : exposition au risque et comptage des événements avec troncature à gauche et censure à droite (Lignes directrices mortalité de l'Institut des actuaires, 2006, chap. 2 ; recherche § 1.b, vérifié), puis estimation, lissage, validation. Les lois de maintien ajoutent un second axe (âge à l'entrée × ancienneté) et des sorties concurrentes (reprise, invalidité, décès), format du règlement ANC 2015-11, art. 600-2 à 600-5 (recherche § 1.a et § 2.g, vérifié).

Sans cadre commun, chaque risque reçoit son propre calcul d'exposition, sa propre gestion de la censure et ses propres contrôles : n implémentations à relire, des conventions qui divergent en silence, et la versatilité des entrées (Q6) à refaire pour chacun. Le point sensible n'est pas l'estimation, c'est la lecture des données : quelles colonnes existent, ce qu'elles signifient, ce qu'elles permettent de calculer.

## Décision

Arrêtée par le mainteneur le 14/10/2026 (Q13, Q15, Q17, Q18, Q19, Q33, Q34, Q35) :

1. **Une table d'expérience est l'estimation d'une transition** (Q13 ; `CONTEXT.md`, « Transition ») : état de départ, état d'arrivée, axe principal (âge ou ancienneté), axe secondaire facultatif, année calendaire facultative, sorties concurrentes, censure. Chaque risque de Q2 est une configuration du noyau (états, axes, sorties concurrentes), pas un moteur à part ; le module `exposition` (ADR 0004) implémente ce noyau une fois.
2. **Rôles déclarés, catalogue fermé** (Q15, Q17 ; « Rôle ») : l'utilisateur déclare la nature de chaque colonne parmi les familles identification, état civil, observation, agrégats, pondération, segmentation, multi-états ; le moteur ne lit une colonne que par son rôle. Le catalogue est figé en v1 ; toute extension passe par l'avis d'`actuary`.
3. **Recette d'exposition la plus fine, nommée, sinon refus** (Q17 ; « Recette d'exposition », « Convention d'âge ») : le moteur retient la recette la plus fine que les rôles permettent, l'inscrit dans le rapport et la fiche de construction (ADR 0005), et **refuse** quand aucune ne s'applique ; il ne devine ni colonne ni convention. L'utilisateur peut **forcer** une convention plus grossière que celle déduite ; le forçage figure dans la fiche.
4. **Pondération par les montants dès la v1** (Q17) : rôle de pondération admis ; une table pondérée est nommée comme telle dans le rapport et la fiche.
5. **Unité d'observation : la tête** (Q18 ; « Tête ») dès qu'un identifiant de tête est déclaré, les intervalles d'observation de ses contrats étant fusionnés ; sinon le contrat, avec avertissement. Les incohérences entre contrats d'une même tête sont relevées et listées, jamais corrigées en silence.
6. **Période d'observation et délai de déclaration déclarés** (Q19 ; « Période d'observation », « Délai de déclaration ») : jamais déduits des données ; le délai tronque la fin de période ; l'estimation des événements tardifs est hors v1. **Toute recette traite explicitement la troncature à gauche et la censure à droite** (« Troncature à gauche », « Censure à droite ») et le documente.
7. **Lignes invalides refusées** (Q35) : par défaut, une ligne invalide arrête la construction ; des règles d'exclusion déclarées dans le plan, par motif, permettent d'écarter des lignes, comptées et listées dans le rapport de validation ; aucune correction automatique.
8. **Deux transitions dès la v1** (Q34) : l'interface n'expose que vivant → décédé ; la transition en cours → racheté est couverte par les tests sur données simulées, preuve que le noyau n'est pas propre au décès.
9. **Le simulateur est le même modèle, exécuté en avant** (Q33, Q10) : loi vraie = table de référence × facteur par segment, dérive calendaire optionnelle, sorties concurrentes, censure ; il produit les trois formes de données (Q6) avec défauts paramétrés ; chaque jeu versionné porte sa loi vraie et sa graine ; la validation par simulation est une batterie de tests.

## Options écartées

- **Un moteur par risque** (mortalité d'abord, rachat ensuite…) : duplication de l'exposition, de la censure et des contrôles ; divergences de conventions ; versatilité des entrées à refaire à chaque risque. Test de suppression : retirer un risque du noyau retire une configuration, pas du code ; avec un moteur par risque, il laisserait du code mort et des conventions orphelines.
- **Catalogue de rôles ouvert** (colonnes interprétées par nom ou par type) : le moteur devinerait ; une erreur de lecture est invisible et survit aux relectures. Le refus explicite coûte un message, pas un résultat faux.
- **Déduction automatique de la convention d'âge ou de la période d'observation** : une convention déduite change avec les données ; une période déduite masque les trous d'observation. L'une et l'autre doivent figurer, déclarées, dans la fiche pour qu'un certificateur les relise (Lignes directrices, chap. 1 : validation des fichiers initiaux, période d'observation ; vérifié).
- **Contrat comme unité par défaut** : surestime l'exposition et compte plusieurs fois le décès d'une tête multi-contrats ; admis seulement faute d'identifiant, avec avertissement.
- **Correction automatique des lignes invalides** (dates inversées, âges aberrants) : interdite (Q35) ; toute exclusion est une règle déclarée, comptée et relue.
- **Simulateur indépendant du noyau** (générateur ad hoc par risque) : ne prouverait rien sur le noyau ; le simulateur en avant partage états, axes et sorties concurrentes, et sa loi vraie est le seul « vrai » disponible (Q10 : aucune donnée réelle).

## Conséquences

- Fichiers : modules `donnees` (rôles, contrôles, unité d'observation, exclusions), `exposition` (recettes, conventions, troncature et censure) et `simulation` de `tablexp` (ADR 0004) ; catalogue des rôles et des recettes dans la documentation LaTeX avec, pour chaque recette, les rôles requis et la convention appliquée ; `docs/exigences.md` : E2.1 à E2.8 et E2.11 (points 1 à 8), E2.19 (point 9).
- Effet sur les résultats : aucun à ce jour. Règle durable : deux bases qui portent les mêmes rôles produisent la même recette ; une recette modifiée est un changement de résultat, avec tableau avant / après et visa.
- Tests : pour chaque recette, un jeu simulé à loi vraie connue (point 9) ; la seconde transition (point 8) sur le même noyau ; le refus (point 3) et les exclusions (point 7) testés comme des résultats (message, décompte).
- Ce que l'ADR ne règle pas : le contenu exact du catalogue de rôles et des recettes de la v1 (spécification d'`actuary`, Q30) ; la représentation interne d'une transition et d'un état (plan de la première branche) ; les seuils des indicateurs de suffisance (Q21, `actuary`) ; la segmentation (Q20 : sexe et une variable libre en v1), paramètre du plan (ADR 0005) et non du noyau ; les tables à deux axes des lois de maintien (v3+), pour lesquelles le noyau prévoit déjà l'axe secondaire.
- Vocabulaire : `CONTEXT.md`, « Transition », « Rôle », « Recette d'exposition », « Convention d'âge », « Tête », « Période d'observation », « Délai de déclaration », « Troncature à gauche / censure à droite », « Segment » ; « Surface de mortalité » (Q16 : objet commun de la prospective, v2, produit par le même noyau).

Issues : aucune à ce jour.
