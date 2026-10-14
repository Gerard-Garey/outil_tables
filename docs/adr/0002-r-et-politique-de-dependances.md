---
status: accepted
date: 2026-10-14
---

# R 4.3.1, moteur en R de base, packages avancés cantonnés à la contre-vérification hors CI, R pur mesuré avant tout code compilé

## Contexte

Le projet construit des tables d'expérience destinées à des dossiers ACPR (`docs/cadrage/2026-10-14-entretien-cadrage.md`, Q1) : la traçabilité et la relecture priment sur la vitesse de développement. Le langage et la politique de dépendances conditionnent la forme du moteur (ADR 0004), les batteries de tests, la reproductibilité (`CLAUDE.md`, « Changements de résultats ») et la durée de vie du code.

Faits qui pèsent (`docs/recherche/2026-10-14-cadre-tables-experience.md`, § 3, pages CRAN et PyPI lues le 14/10/2026) :

- l'écosystème R couvre tout le périmètre visé (Q30), mais ses packages de référence sont fragiles : `MortalitySmooth` est **archivé du CRAN depuis le 10/12/2020** (dépendance `svcm` archivée) [vérifié], sans successeur trouvé [non trouvé] ; `StMoMo` 0.4.1 date du **13/04/2018**, dernier commit GitHub le 02/11/2019, 17 issues ouvertes [vérifié] ; `ELT` 1.7 (2023) dépend de `xlsx` (Java) [vérifié] ; `WH` 2.0.0 (2025) s'appuie sur `Rcpp` et LAPACK [vérifié] ;
- en Python, `lifelines`, `scikit-survival` et `statsmodels` couvrent l'estimation brute et les GLM [vérifié] ; la recherche n'a trouvé **aucun équivalent mûr** de `StMoMo` ni de `MortalitySmooth` après trois recherches ciblées [non trouvé : les candidats sont pédagogiques ou en version 0.x] ; prospective et lissage 2D seraient à réécrire (§ 3.b) ;
- un package tiers dans le moteur fait dépendre un résultat soumis à l'ACPR d'un code que l'équipe ne relit pas, dont la version peut changer ou disparaître ; la revue indépendante (`outil_USP`, `docs/exigences.md` § 4.1) suppose que tout calcul soit lisible dans le dépôt ;
- **contrainte non visible dans le code** (mainteneur, 14/10/2026, interprétation I1) : le **poste de travail** du mainteneur pourrait ne pas permettre l'installation de packages CRAN — ni les packages avancés, ni les accélérateurs en C / C++ (`Rcpp`), ni peut-être Rtools. C'est la raison première de la règle « R de base » ; ce qui est réellement installable là-bas n'est pas encore mesuré (issue #2 : relevé par le mainteneur, groupe par groupe).

Mesure du 14/10/2026 sur le poste du mainteneur (`Rscript -e 'installed.packages()'`, R 4.3.1 ucrt, seule version présente dans `C:/Program Files/R`) : packages de priorité `base` = `base compiler datasets graphics grDevices grid methods parallel splines stats stats4 tcltk tools utils` ; `survival` et `mgcv` sont de priorité `recommended` (avec `boot class cluster codetools foreign KernSmooth lattice MASS Matrix nlme nnet rpart spatial`). Q14 range `survival` et `mgcv` parmi les packages de contre-vérification : « packages livrés avec R » s'entend donc **priorité `base`**, et non `recommended`.

## Décision

Arrêtée par le mainteneur le 14/10/2026 (Q4, Q14, Q24, Q38, Q39) :

1. **Langage : R** (Q4 : Python jugé meilleur en vitesse et en interface, mais plus long à développer). Interface : application Shiny locale (Q5 ; ADR 0004).
2. **Version : R 4.3.1**, celle d'`outil_USP` (Q39), **contre la recommandation de la session** (« dernière version stable »). La CI et la plateforme de référence des non-régressions fixent 4.3.1 ; `DESCRIPTION` déclare `R (>= 4.3.1)`. Un changement de version est une décision du mainteneur, consignée par annotation datée du présent ADR, avec les écarts de résultats mesurés.
3. **Moteur en R de base** (Q14, option a) : le package `tablexp` n'importe que les packages de priorité `base` listés ci-dessus (`Imports` et `Depends` de `DESCRIPTION` ; vérification par script en CI). Les packages `recommended` (dont `survival`, `mgcv`, `Matrix`, `MASS`) en sont exclus au même titre que les packages CRAN.
4. **Packages avancés en contre-vérification, hors moteur et hors CI** (Q14 ; Q24, option A) : `survival`, `mgcv`, `WH`, `ELT`, `StMoMo`, `demography`, `MortalityLaws`, `lifecontingencies`, `survPen` (liste étendue sur avis d'`actuary`) produisent des **valeurs de référence externes**, écrites en dur dans les tests avec la version de chaque package et la date, par un script hors CI exécuté à la main. La CI n'installe aucun d'eux. Régénération sur ordre du mainteneur seulement, avec tableau avant / après.
5. **Passage à un package tiers enveloppé dans le moteur** (Q14, option b : un package par méthode, derrière un adapter) : uniquement par un nouvel ADR, méthode par méthode, après démonstration qu'une réimplémentation en R de base est impossible ou déraisonnable. **Voie de réouverture prévue** (I1) : une fois le relevé de l'issue #2 rendu, un package **disponible sur le poste de travail** qui apporte une accélération (C / C++) ou une méthode peut être admis dans le moteur — ou, pour les packages de contre-vérification, dans les tests exécutables sur ce poste — par annotation datée du présent ADR qui cite le relevé, puis ADR par méthode s'il entre dans le moteur. Tant que le relevé n'est pas rendu, le point 3 s'applique sans exception.
6. **R pur d'abord, mesuré** (Q38) : chaque méthode est écrite en R de base et chronométrée sur le simulateur (Q33) à la volumétrie cible (Q7 : millions de lignes, dizaines d'années). Du code C n'est admis que sur **mesure de lenteur documentée** (commande, temps, machine), par l'**interface native de R** (`.Call`, en-têtes `R.h` et `Rinternals.h`), **sans `Rcpp`**, et par ADR.
7. **Lecture et écriture des fichiers hors moteur** (Q37, Q11) : `openxlsx` est admis dans l'application, jamais dans `tablexp` ; le CSV passe par R de base.

## Options écartées

- **Python** (`lifelines`, `statsmodels`, interface Streamlit, Dash ou NiceGUI, § 3.b et § 3.c) : couverture incomplète du périmètre (prospective et lissage 2D à réécrire), préférence et pratique du mainteneur (Q4), continuité avec `outil_USP` dont les dispositifs sont repris (Q8). La vitesse de Python n'est pas déterminante tant que le R pur n'a pas été mesuré trop lent (point 6).
- **Dernière version stable de R** (recommandation de la session) : écartée par le mainteneur (Q39). Risque assumé : un package de contre-vérification récent peut exiger un R plus récent ; le script hors CI consigne alors la version installable sous 4.3.1, ou la contre-vérification se fait sur un autre poste avec sa version de R consignée.
- **Packages tiers dans le moteur, option (b)** : garde une dépendance par méthode vers un code non relu, dont `MortalitySmooth` montre qu'il peut disparaître ; réservée à un ADR ultérieur (point 5). **Option (c)**, usage libre du CRAN dans le moteur : rend la revue indépendante impossible et les résultats tributaires des versions installées.
- **Contre-vérification en CI** (Q24, option B) : la CI installerait les packages avancés à chaque exécution ; une archive CRAN, une mise à jour ou une panne réseau ferait échouer la batterie sans qu'aucun résultat du moteur ait changé, et la valeur de référence ne serait plus figée. Incompatible avec « un chiffre ne se recopie pas, il se remesure » appliqué à une référence : elle doit être écrite, datée et versionnée.
- **`Rcpp`** : package tiers dans le moteur (contredit le point 3), chaîne C++ qu'un relecteur R ne lit pas ; l'interface native suffit pour une boucle numérique.

## Conséquences

- Fichiers : `DESCRIPTION` de `tablexp` (`Depends: R (>= 4.3.1)`, `Imports` limités aux packages `base`) ; script de CI qui échoue si `Imports` ou `Depends` sort de la liste ; scripts de contre-vérification hors CI, versions et dates consignées dans chaque fichier de valeurs ; `README.md` (installation sous 4.3.1) ; `CLAUDE.md`, « Architecture » et « Changements de résultats » (plateforme de régénération : R 4.3.1) ; `docs/exigences.md` : E4.1, E4.2, E4.7, E4.8.
- Effet sur les résultats : aucun à ce jour (aucun code). À terme, chaque méthode est une réimplémentation : les écarts aux packages de référence sont attendus, bornés par une tolérance déclarée dans le test, jamais masqués.
- Coût accepté : réécrire en R de base les lissages pénalisés (Whittaker-Henderson, P-splines : matrices denses sans `Matrix`, bases B-splines par `splines`), les estimateurs de survie et, en v2, les modèles prospectifs ; `parallel` (base) reste disponible pour le bootstrap (Q31).
- Ce que l'ADR ne règle pas : la liste des packages de contre-vérification par méthode (avis d'`actuary`, Q30) ; le seuil de lenteur qui justifie du C (fixé à la première mesure, Q38) ; la politique de montée de version de R (annotation ici) ; **ce qui est installable sur le poste de travail** — packages, Rtools, accès au CRAN — et donc où les contre-vérifications peuvent s'exécuter et si le C du point 6 y est compilable (issue #2 ; tant que Rtools n'y est pas établi, le point 6 reste théorique sur ce poste).
- Vocabulaire : `CONTEXT.md`, « Mesure », « Tableau avant / après », « Visa », « Contre-vérification ».

Issues : #2 (relevé des packages installables sur le poste de travail).
