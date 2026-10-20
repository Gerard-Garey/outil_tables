---
status: accepted
date: 2026-10-17
---

# Références de non-régression : un fichier texte aplati par plan de référence (une ligne par vecteur, doubles en `%.17g`, structure contrôlée à part), tolérance déclarée par grandeur, produites par le workflow `references.yml` sur la plateforme de référence et commitées par la seule session principale après visa

*Statut : `proposed`, en attente de la décision du mainteneur sur les points de la liste « Points soumis au mainteneur » en fin d'ADR. Rédigé par `architect-approfondi` sur Fable (`docs/agents/routage.md`, § 4.1.3 : ADR d'architecture ; première des trois consultations Fable de la branche `claude/taux-bruts-non-regression`, § 5.2), d'après l'avis de tolérance d'`actuary` (issue #14, commentaire 6235182191, partie B) et la décision M50 (Q6). Il fixe le contrat partagé « critère de non-régression » (`docs/agents/routage.md`, § 4.2).*

## Contexte

**Règles déjà posées.**

- `CLAUDE.md`, « Changements de résultats et reproductibilité », et E4.7 : les références de non-régression sont produites **sur la CI seule** (workflow de régénération, `ubuntu`, R 4.3.3, BLAS et LAPACK de référence monothread, plateforme consignée dans le message du commit de référence), **jamais sur un poste**, régénérées **après visa** par la **session principale** (jamais par un agent ni un workflow), et comparées **valeur par valeur, à tolérance, structure contrôlée à part**. Tout changement d'un résultat passe par le tableau avant / après et le visa.
- ADR 0005, point 8 : un plan de référence par méthode de la v1.
- ADR 0007 : `execution` est hors de toute comparaison (point 6.1) ; représentation textuelle canonique des données, doubles en `%.17g`, chaînes UTF-8 échappées, `NA` distingué de la chaîne « NA » (point 9.3) ; la batterie `tests/test_reproductibilite.R` compare « aux références de non-régression à partir de la deuxième branche » (point 11.1) ; le format de sérialisation de la fiche est **reporté** à la branche `claude/fiche-rapport`, avec des contraintes (point 12.2 : texte lisible, aller-retour identique, aucune évaluation de code à l'import, R de base, versionné).
- ADR 0008, point 8 : invariants du workflow de régénération (`workflow_dispatch` seul, `permissions: contents: read`, aucun secret, même `image@empreinte` et même script de plateforme en mode strict que « Tests (R 4.3.3) », checkout jetable, artefact avec les références, leurs MD5 et l'identifiant de plateforme, commit par la session principale après visa) ; le fichier du workflow entre sur `main` par un **correctif rapide** avant son premier déclenchement ; les modes du workflow, la tolérance et le format sont « réglés à la deuxième branche ». Annotation du 16/10/2026 (M47) : l'identifiant de plateforme de référence (image épinglée suivie de la bascule `update-alternatives` vers `libblas3` et `liblapack3` 3.10.0-2ubuntu1, R 4.3.3, BLAS et LAPACK Netlib 3.10.0, monothread) ; la réserve sur la `libm` de la glibc (variantes d'`exp`, `log` et `pow` selon le processeur, modèle qui varie d'une exécution à l'autre) reste **ouverte, non mesurée**.
- Décision M50 (Q6, 17/10/2026) : l'entrée des références est un jeu **tiré par `simuler_portefeuille()` à graine fixée**, son empreinte des données étant la **première grandeur de référence** ; les jeux simulés versionnés sous `inst/extdata/` sont renvoyés à `claude/simulateur-formes`.
- Issue #17 : périmètre et critères d'acceptation ; issue #14 (critère) : « un plan sans étape `brut` donne un objet `identical` (hors `execution`) à celui de `f9ab269` », mesuré ponctuellement par `coder`, `audit` et `actuary` (message du commit `125f3a3` : 6, 9 et 6 plans, toutes les comparaisons `TRUE`) et dont la **comparaison permanente est renvoyée aux références** (avis d'`actuary` sur #14, Q1 ; `tests/test_reproductibilite.R`, l. 195-196).

**Avis de tolérance d'`actuary`** (#14, commentaire 6235182191, partie B), repris tel quel au point 3 : égalité exacte si `identical` ; sinon deux valeurs finies non nulles de même signe avec |a − b| ≤ tol · max(|a|, |b|) ; nombre de lignes, clés et position des `NA` : structure, exacte ; empreinte des données **comparée en premier** ; entiers, chaînes, états, codes, plan, rôles, hypothèses : exacte ; `exposition_centrale`, compléments, `exposition_initiale`, `taux` : **1e-14** ; bornes, `probabilite` et ses bornes : **1e-12** ; `fiche$versions`, `fiche$plateforme`, `execution` : hors comparaison, écrits en en-tête ; la comparaison imprime l'**écart relatif maximal observé par grandeur** ; plans R1 à R6 ; jeu recommandé (Gompertz-Makeham A = 2e-4, B = 2,5e-5, c = 1,1, facteurs H 1 et F 0,6, rachat constant 0,05 ; n = 10^5, p_H = 0,5, âges 20 à 75, origine 2005-01-01, période 2015-01-01 → 2024-12-31, graine 20261016).

**État du dépôt** (branche `claude/taux-bruts-non-regression` à `3c8f21b`, *vérifié*) : `tests/reference/` n'existe pas ; `.github/workflows/` ne contient que `ci.yml` ; `tests/test_reproductibilite.R` compare par `identical()` deux exécutions d'un même commit (même processus, autre processus, rejeu de la fiche) et vérifie l'invariance du plan sans `brut` pour R1 et R4 (l. 195-214), sans aucune comparaison à un état antérieur ; `tests/controles/identite_batteries.R` contrôle déjà que toutes les valeurs `image:` et `TABLEXP_IMAGE:` de **tous** les fichiers `.github/workflows/*.yml` forment une chaîne unique (contrôle 3, l. 156-166), mais ne lit pas les blocs `run: |` multilignes (limite déclarée en en-tête) ; `tests/harnais.R` offre `proche(a, b, rel, abs)` avec la règle |a − b| ≤ abs + rel · |b|, qui n'est pas la règle symétrique d'`actuary` ; `R/rapport.R` porte `colonne_canonique()` (type canonique et octets d'une colonne, ADR 0007, point 9.3) et `empreinte_donnees()`.

**Mesures de la session** (17/10/2026, R 4.3.3 (2024-02-29), Linux x86_64, conteneur de la session cloud — BLAS 3.12.0, **hors plateforme de référence** ; package installé depuis `3c8f21b` dans une bibliothèque temporaire ; scripts du bloc-notes de la session, `structure.R`, `plans.R`, `rt.R`) :

- objet retourné pour R1 : champs `table` (`NULL`), `etapes` (`donnees`, `exposition`, `brut`), `avertissements`, `fiche` (27 champs, `hypotheses_brut` en dernier), `execution` ; `fiche$versions` = `tablexp 0.0.0.9000`, `R version 4.3.3 (2024-02-29)` ; `fiche$plateforme` = `sysname`, `release`, `machine`, `blas`, `lapack` ; `execution` = `debut` (`POSIXct`), `duree_secondes`, `session` (R, plateforme, système, locale, paquets attachés) ;
- **tables de lignes** de l'étape `donnees` : `lignes_conservees` (100 000 × 7, qui porte l'attribut `simulation` hérité du jeu simulé), `numeros_conserves` (100 000), `lignes_exclues` (liste vide), `observations` (100 000 × 8 ; × 9 sous R5), `observation_des_lignes` (100 000) ; toutes de types `integer`, `Date` et `character` ; `object.size` de l'objet R1 : 10,7 Mo ; MD5 de la représentation canonique d'`observations` (100 000 × 8, par `colonne_canonique()`) : 0,39 s ;
- **hors ces tables, hors `execution`, `fiche$versions` et `fiche$plateforme`** : R1 167 vecteurs / 5 241 valeurs ; R2 164 / 2 578 ; R3 167 / 5 249 ; R4 155 / 34 544 (1 490 cellules) ; R5 167 / 5 241 ; aucun facteur ; aucun attribut hors `names`, `class`, `row.names` ; les doubles de R4 écrits en `%.17g` pèsent 290 476 octets ;
- simulation (n = 10^5, graine 20261016) : 0,21 s ; construction R1 à R5 : 1,0 à 1,8 s chacune ; R1 : 149 cellules, 4 228 décès ; R2 : 75 cellules, 4 086 ; R3 : 149, 4 228 ; R4 : 1 490, 4 228 ; R5 (en cours → racheté, unité `contrat`) : 149 cellules, 25 312 rachats ;
- **empreinte des données** du jeu (graine 20261016, n = 10^5) : `f3519cc1b73ab0d1fcc6ad89dc6d595c`, identique pour les cinq plans (mêmes rôles, mêmes données) ;
- **aller-retour `%.17g`** sur les 19 370 doubles des tables `poisson` et `exposition` de R4 : `NA` appariés, valeurs finies identiques au bit près (`TRUE`, `TRUE`) ;
- **ancrage** : `git diff --stat f9ab269 3c8f21b -- R/donnees.R R/exposition.R R/simulation.R R/outils.R` est **vide** ; les seuls fichiers de `R/` modifiés depuis `f9ab269` sont `R/brut.R` (créé), `R/construire_table.R` et `R/rapport.R` (commit `125f3a3`).

**Ce qui rend la décision nécessaire maintenant.** Les premières références doivent naître dans cette branche, après #14, #15 et #16, sur la plateforme de référence fixée par l'ADR 0008 ; le code qui les produit, les compare et les protège (workflow, batterie, témoins, contrôle d'identité) doit être écrit avant. Le critère de non-régression est un **contrat partagé** : toute branche ultérieure qui change un résultat sera jugée contre lui.

## Décision

Proposée par `architect-approfondi` le 17/10/2026 ; les points 3.3 et 3.4 reprennent l'avis d'`actuary` ; le point 4.1 reprend M50 (Q6) ; la décision du mainteneur est attendue sur la liste en fin d'ADR.

### 1. Ce qu'est une référence : périmètre des grandeurs

1.1. **Une référence est l'aplatissement de l'objet `construction`** retourné par `construire_table()` pour **un plan de référence** (point 4) sur **le jeu d'entrée de référence** (point 4.1), **moins** trois champs : `execution` (ADR 0007, point 6.1 : hors de toute comparaison), `fiche$versions` et `fiche$plateforme` (métadonnées, écrites dans l'en-tête du fichier, point 2.2, et jamais comparées). `fiche$version_fiche` reste comparé (c'est un contrat, pas une métadonnée).

1.2. **Les tables de lignes de l'étape `donnees` sont résumées, non recopiées** : `etapes/donnees/resultat/lignes_conservees`, `numeros_conserves`, `lignes_exclues`, `observations` et `observation_des_lignes` (liste fermée `RESUMES_PAR_EMPREINTE`, constante de l'outillage) sont remplacées chacune par **deux grandeurs** : le **nombre de lignes** (entier, exact) et l'**empreinte MD5** (chaîne, exacte) de leur représentation canonique — colonnes dans l'ordre de la table, chacune écrite par `tablexp:::colonne_canonique()` (ADR 0007, point 9.3 ; `:::` admis dans `tests/`, ADR 0007, point 11.3), précédée de la ligne `#<nom><TAB><type>`, dans un fichier temporaire passé à `tools::md5sum()` ; pour une liste (`lignes_exclues`), l'empreinte porte sur ses éléments dans l'ordre, chacun traité comme ci-dessus, précédé de son nom. Les attributs autres que `names`, `class` et `row.names` de ces tables (l'attribut `simulation` hérité du jeu, mesuré) **n'entrent pas** dans l'empreinte : l'entrée est identifiée par `fiche/empreintes/donnees`. Raisons : 100 000 lignes par table et par plan (mesuré) feraient des fichiers de plusieurs dizaines de mégaoctets par plan, inutilisables en revue ; ces tables ne contiennent que des entiers, des dates et des chaînes (mesuré), donc **aucune arithmétique flottante** : l'égalité attendue est exacte, et un résumé exact (MD5) ne perd aucune information de non-régression ; en cas d'écart, la table « avant » se **reconstitue sur n'importe quelle machine** en rejouant le plan sur le commit de référence, puisque ces grandeurs ne dépendent pas de la plateforme (*hypothèse* : aucune dépendance à la locale n'y subsiste ; l'ordre des lignes est celui de l'entrée). Le coût est de 0,4 s par table de 100 000 × 8 (mesuré).

1.3. **Tout le reste est comparé valeur par valeur** : `etapes/<étape>/parametres`, `resultat` (hors 1.2) et `diagnostics` de chaque étape, `avertissements`, `fiche` (hors 1.1), `table` (`NULL` tant que l'étape `table` n'existe pas). Cela inclut `fiche/plan` et `fiche/roles` : **le plan de référence est lui-même une grandeur**, et une modification du plan ou de sa représentation est détectée comme un écart exact.

1.4. **Première grandeur** : `fiche/empreintes/donnees` (M50, Q6) ; son rôle dans la comparaison est fixé au point 3.2.

### 2. Format et emplacement des fichiers

2.1. **Un fichier texte par plan**, `tests/reference/<identifiant>.tsv` (`R1.tsv` … `R6.tsv`), UTF-8, lignes terminées par LF, **aucun CR**, écrit octet par octet (`writeBin`, comme la représentation canonique de l'ADR 0007). `tests/` est hors du package installé (`.Rbuildignore`, ADR 0007, point 1.1). Aucun format binaire (voir « Options écartées »).

2.2. **En-tête** : des lignes `# <clé> : <valeur>` au début du fichier, **hors comparaison** : `tablexp reference 1` (version du format, première ligne), identifiant et description du plan, **identifiant de plateforme** (bloc du point 5 de l'ADR 0008 tel que l'écrit `tests/controles/plateforme.R`), `fiche$versions` et `fiche$plateforme` de l'objet, commit calculé (`GITHUB_SHA`), identifiant et adresse de l'exécution (`GITHUB_RUN_ID`), mode du workflow (point 5.2), date UTC, et, en rappel lisible, l'empreinte des données. Hors de la CI (exécution locale de l'outillage pour un essai), les champs de la CI valent `(hors CI)`.

2.3. **Corps : une ligne par vecteur atomique**, de la forme `chemin<TAB>type<TAB>longueur<TAB>valeur_1<TAB>…<TAB>valeur_n`, dans l'ordre de parcours de l'objet (parcours en profondeur, éléments dans l'ordre de la liste) :

- **chemin** : noms joints par `/` (`etapes/brut/resultat/poisson/taux`) ; un élément sans nom s'écrit `[i]` ;
- **structure explicitée par des lignes `@`** : pour chaque liste, `chemin/@noms` (type `character`, les noms dans l'ordre ; longueur 0 pour une liste vide ou sans noms) ; pour chaque `data.frame`, `chemin/@colonnes` (noms des colonnes dans l'ordre) et `chemin/@lignes` (entier, nombre de lignes) ; pour l'objet lui-même, `@classe` (`construction`) ; un `NULL` s'écrit `chemin<TAB>NULL<TAB>0` ; une table résumée (point 1.2) s'écrit `chemin/@lignes` et `chemin/@empreinte` ;
- **types** : `double`, `integer`, `logical`, `character`, `Date`, `factor`, `NULL` ; **valeurs** selon les règles de l'ADR 0007, point 9.3, réutilisées par `tablexp:::colonne_canonique()` : double `sprintf("%.17g", x + 0)` (`NA`, `NaN`, `Inf`, `-Inf`), entier `%d`, logique `TRUE` / `FALSE` / `NA`, date `%Y-%m-%d`, chaîne entre guillemets droits avec les cinq échappements `\\`, `\n`, `\r`, `\t`, `\"` (`NA` sans guillemets), facteur converti en chaînes sous le type `factor` ; la tabulation est le séparateur parce que les chaînes l'échappent ;
- **refus d'écriture** : toute autre classe (`POSIXct`, `difftime`, …) et tout attribut autre que `names`, `class` et `row.names` sur un élément comparé (hors 1.2) **font échouer l'écriture**, avec le chemin fautif ; rien n'est converti ni ignoré en silence. Une grandeur nouvelle d'une classe nouvelle appelle donc une décision (extension du format, version 2) et non un ajustement.

2.4. **Aller-retour** : lire un fichier redonne, pour chaque chemin, un vecteur du même type et des mêmes valeurs au bit près (`%.17g`, mesuré sur 19 370 doubles) ; le lecteur est en R de base, **sans évaluation de code** (`readLines`, `strsplit` sur la tabulation, `as.numeric`, `as.integer`, `as.Date`, désechappement des chaînes ; ni `source()`, ni `eval(parse())`, ni `dget()` ; ADR 0007, point 12.2 (c)).

2.5. **Ce format n'est pas le format de sérialisation de la fiche** (ADR 0007, point 12, reporté à `claude/fiche-rapport`) : c'est un artefact de test, lu et écrit par l'outillage de `tests/reference/`, dont les contraintes (texte lisible, aller-retour exact, aucune évaluation de code, R de base, versionné) sont celles du point 12.2. Quand la branche `fiche-rapport` fixera le format de la fiche, elle dira si les deux convergent (l'écrivain de valeurs pourrait alors passer dans `rapport` et l'outillage l'appeler) ; le présent ADR ne l'impose pas.

2.6. **Outillage, sous `tests/reference/`**, en R de base, sourcé par la batterie et par le workflow (`CLAUDE.md`, « Workflows », principe 7 : tout contrôle mécanique est un script) : `outils_reference.R` — `plans_reference()` (point 4.3), `jeu_reference()` (point 4.1), `aplatir_construction(x)` (points 1 et 2.3), `ecrire_reference(lignes, fichier, entete)`, `lire_reference(fichier)`, `comparer_references(reference, obtenu, tolerances)` (point 3), `TOLERANCES` (point 3.4), `RESUMES_PAR_EMPREINTE` (point 1.2) ; `produire_references.R` — script du workflow (point 5.3), exécutable aussi sur le poste, en mode relevé, pour essai (ses fichiers ne sont alors **jamais** commités : `CLAUDE.md`) ; `comparer_references.R` — ligne de commande `Rscript tests/reference/comparer_references.R <ancien.tsv> <nouveau.tsv>` qui imprime le tableau avant / après de deux fichiers (point 3.5), ce qui sert à rédiger le tableau d'une régénération. Les noms de fichiers sont indicatifs (`coder` peut regrouper les deux scripts) ; les fonctions et leurs rôles sont le contrat.

### 3. Règle de comparaison, tolérances, impression, témoins

3.1. **Structure, exacte, d'abord** : l'ensemble des chemins, avec le type et la longueur de chacun, doit être identique entre la référence et l'objet obtenu ; un chemin **manquant**, **en trop**, de **type** ou de **longueur** différents est un **écart structurel**, imprimé un par ligne (`chemin : manquant | en trop | type a au lieu de b | longueur n au lieu de m`) et fait échouer la comparaison du plan. Les lignes `@` (noms, colonnes, nombre de lignes, classe) sont comparées exactement comme des grandeurs. La position des `NA`, `NaN`, `Inf` relève de la structure : positions différentes = écart structurel sur ce chemin.

3.2. **Entrée, en premier** : `fiche/empreintes/donnees` est comparée avant tout le reste. Si elle diffère, le plan est en échec « **entrée différente** » (les deux empreintes imprimées) ; la comparaison des autres grandeurs **se poursuit et s'imprime** sous la mention « entrée différente : écarts non interprétables comme un tableau avant / après », parce qu'une entrée différente explique tout écart aval sans qu'aucun ne mesure un changement de méthode. Les deux causes possibles sont nommées dans la sortie : le simulateur a changé pour cette graine (point 4.2), ou la plateforme a produit un tirage différent (hypothèse de l'ADR 0008 sur la `libm`).

3.3. **Valeurs** (règle d'`actuary`) : deux vecteurs de même chemin sont égaux si `identical()` ; sinon, élément par élément, deux valeurs finies, non nulles, de même signe sont égales à la tolérance `tol` de la grandeur si |a − b| ≤ tol · max(|a|, |b|) ; tout autre cas (zéro contre non nul, signes opposés, non fini contre fini) est un écart. Cette règle, **symétrique et relative**, est celle des références ; `proche()` du harnais (|a − b| ≤ abs + rel · |b|) reste celle des tests unitaires, où « b » est un attendu.

3.4. **Classes de tolérance, déclarées par grandeur** (table `TOLERANCES`, clé : dernier segment du chemin ; avis d'`actuary`) :

| Classe | Grandeurs | Justification (`actuary`) |
|---|---|---|
| **exacte** | toute grandeur de type `integer`, `logical`, `character`, `Date`, `factor`, `NULL` et les lignes `@` ; et les doubles à valeur entière ou déclarée : `exposition_jours`, `cellules`, `niveau`, `delai_declaration`, `tolerance_effectif`, `version_fiche`, `graine` | entiers, chaînes, paramètres : aucun bruit possible |
| **1e-14** | `exposition_centrale`, `complement_initial_cellule`, `complement_initial_fenetre`, `exposition_initiale`, `taux` | arithmétique IEEE de base, ordre des opérations fixé par le code ; plus petit changement significatif mesuré 5,8e-7 (un jour dans la cellule la plus exposée, n = 10^5) |
| **1e-12** | `borne_inf_exacte`, `borne_sup_exacte`, `borne_inf_asymptotique`, `borne_sup_asymptotique`, `probabilite`, `probabilite_borne_inf_exacte`, `probabilite_borne_sup_exacte`, `probabilite_borne_inf_asymptotique`, `probabilite_borne_sup_asymptotique` | fonctions spéciales (`qgamma`, `qbeta` itératifs ; `exp`, `log` de la `libm`, variante selon le processeur : hypothèse non mesurée) ; plus petit changement significatif 7,3e-3 (un événement de plus) |

Règle de complétude : **une grandeur de type `double` dont le nom n'est dans aucune classe est un échec** (« grandeur sans tolérance déclarée : <chemin> »), jamais une comparaison exacte par défaut ni une tolérance devinée : chaque grandeur nouvelle du moteur reçoit sa classe d'`actuary` avant d'entrer dans une référence (c'est une ligne de la surface d'impact du commit qui la crée). Les grandeurs de Kaplan-Meier (R6, #15) sont classées par `actuary` dans la spécification de #15, avant la création. Une modification de `TOLERANCES` est annotée dans le présent ADR, datée, avec l'avis d'`actuary`.

3.5. **Impression**, pour chaque plan, dans la batterie comme dans le workflow : (a) une ligne de bilan : nombre de grandeurs comparées, nombre identiques au bit près, nombre à écart non nul sous tolérance, nombre en écart, écarts structurels ; (b) **l'écart relatif maximal observé par classe de tolérance** (trois lignes, toujours imprimées, même nulles : c'est la mesure qui confirmera ou réfutera l'hypothèse de bruit nul sur la plateforme de référence) ; (c) **une ligne par grandeur dont l'écart n'est pas nul**, sous tolérance ou non : `chemin`, longueur, écart relatif maximal, position, **avant** et **après** en `%.17g`, tolérance, verdict. Les lignes (c) en verdict « écart » et les écarts structurels sont **la matière du tableau avant / après** (`CONTEXT.md`) : la session principale les recopie dans le commit avec, pour chaque ligne, l'explication ; une ligne inexpliquée est une régression (`CLAUDE.md`). `comparer_references.R` (point 2.6) produit la même impression à partir de deux fichiers.

3.6. **Témoins d'échec** dans `tests/test_reproductibilite.R`, sur des copies temporaires d'une référence (jamais sur `tests/reference/`) : une valeur altérée au-delà de sa tolérance → échec, la même altérée juste en dessous → succès (borne de la classe) ; une grandeur retirée → écart structurel « manquant » ; une grandeur ajoutée → « en trop » ; un type changé ; l'empreinte des données altérée → « entrée différente » ; un double de nom inconnu → « sans tolérance déclarée » ; l'aller-retour `lire(ecrire(aplatir(x)))` redonne des valeurs `identical` ; un objet portant un attribut inattendu ou une classe `POSIXct` → refus d'écriture. Chaque témoin est un test qui doit **échouer** pour passer.

3.7. **Plan de référence sans fichier** : tant qu'un plan de `plans_reference()` n'a pas son fichier sous `tests/reference/`, la batterie l'enregistre en **échec attendu** renvoyant à l'issue de création (`#17` pour R1 à R6), par `echec_attendu()` du harnais ; le commit de référence retire la marque, ce que le harnais impose (succès inattendu). La batterie reste verte entre l'écriture de l'outillage et la création. Inversement, un fichier de `tests/reference/` dont l'identifiant n'est dans `plans_reference()` est un **échec** (référence orpheline).

3.8. **Exécution de la comparaison dans la batterie** : `tests/test_reproductibilite.R` tire le jeu une fois, exécute chaque plan de référence, aplatit, compare au fichier et enregistre un `verifier()` par plan (plus les témoins). Le jeu de l'étape « taux bruts » de la batterie (n = 10^4 aujourd'hui) peut rester distinct ; la comparaison aux références emploie `jeu_reference()`.

### 4. Entrée et plans de référence

4.1. **Jeu d'entrée de référence** (M50, Q6 (a)), défini une seule fois dans `jeu_reference()` : `simuler_portefeuille()` avec la loi vraie Gompertz-Makeham (A = 2e-4, B = 2,5e-5, c = 1,1 ; facteurs H 1, F 0,6), rachat constant 0,05 (facteurs H 1, F 1), portefeuille `uniforme`, n = 10^5, proportion d'hommes 0,5, âges 20 à 75, origine 2005-01-01, période 2015-01-01 → 2024-12-31, **graine 20261016** ; rôles `id_contrat`, `id_tete`, `sexe`, `date_naissance`, `date_entree`, `date_sortie`, `cause_sortie`. Empreinte des données attendue, **mesurée hors plateforme de référence** : `f3519cc1b73ab0d1fcc6ad89dc6d595c` ; *hypothèse* à trancher par la première exécution du workflow : le tirage est identique sur la plateforme de référence (le générateur est entier, l'inversion est du code de R ; une dépendance à la `libm` dans la transformation des tirages en dates n'est pas exclue). Si l'empreinte diffère, c'est un point de décision du mainteneur (jeu versionné, M50 Q6 (b), avancé ?), **pas** un ajustement de la valeur attendue.

4.2. **Le simulateur est ainsi figé pour cette graine** : toute modification de `simuler_portefeuille()` qui change le jeu tiré change l'empreinte, met **toutes** les références en « entrée différente » (point 3.2) et constitue un **changement de l'entrée de référence** : régénération de tous les plans en mode `regeneration` (point 5.2), tableau avant / après des grandeurs agrégées (par `comparer_references.R` sur les anciens et nouveaux fichiers), visa d'`actuary`, et mention dans le commit que l'entrée a changé. Le simulateur ne peut donc plus évoluer en silence, ce qui est voulu (`CLAUDE.md`, « Données »).

4.3. **Plans de référence**, constante `plans_reference()` (identifiant, description, plan), tous sur la transition vivant → décédé sauf R5, période et délai du jeu sauf mention, `etapes$donnees$exclusions = character(0)` :

| Id | Description | Segmentation | Délai | Convention | `par_annee` | `estimateurs` | `niveau` |
|---|---|---|---|---|---|---|---|
| R1 | décès, référence principale | `sexe` | 0 | `plus_fine` (→ `dernier_anniversaire`) | `FALSE` | `poisson`, `binomial` | 0,95 |
| R2 | sans segmentation, délai, niveau 0,99 | aucune | 90 | `plus_fine` | `FALSE` | `poisson`, `binomial` | 0,99 |
| R3 | convention forcée | `sexe` | 0 | `age_atteint_annee` | `FALSE` | `poisson`, `binomial` | 0,95 |
| R4 | par année (surface) | `sexe` | 0 | `plus_fine` | `TRUE` | `poisson` | 0,95 |
| R5 | en cours → racheté, unité `contrat` | `sexe` | 0 | `plus_fine` | `FALSE` | `poisson`, `binomial` | 0,95 |
| R6 | Kaplan-Meier (#15) | à fixer par la spécification de #15 | | | | `kaplan_meier` (+ autres si la spécification le veut) | |

Un plan par méthode présente (ADR 0005, point 8) : les méthodes ultérieures (lissage, positionnement, fermeture, validation) ajoutent des plans par création (point 5.2). **Un identifiant n'est jamais réemployé** : un plan modifié reçoit un nouvel identifiant et l'ancien fichier est retiré dans le même commit (de toute façon, `fiche/plan` étant une grandeur, un plan modifié sous le même identifiant est détecté comme écart exact, point 1.3).

### 5. Workflow `references.yml` : modes, artefact, commit

5.1. **Invariants** (ADR 0008, point 8, repris) : `on: workflow_dispatch` **seul** ; `permissions: contents: read` ; aucun secret ; un job unique sous `container:` avec **la même chaîne `image@empreinte`**, **la même étape « BLAS et LAPACK de référence »** (bascule M47, octet pour octet) et **la même étape `Rscript tests/controles/plateforme.R --exiger`** que le job « Tests (R 4.3.3) », avant tout calcul ; `env` `TABLEXP_IMAGE`, `OPENBLAS_NUM_THREADS: "1"`, `OMP_NUM_THREADS: "1"` ; `defaults.run.shell: bash` ; `actions/checkout` et `actions/upload-artifact` épinglés par les **mêmes SHA** que `ci.yml` ; `persist-credentials: false` ; `timeout-minutes` ; aucun `continue-on-error`. Le job ne commite rien, ne pousse rien, ne crée aucune issue (`CLAUDE.md`, « Workflows », principe 2).

5.2. **Trois modes**, saisis à l'appel (`inputs.mode`, choix), avec `inputs.plans` (identifiants séparés par des virgules, ou `tous`) et `inputs.motif` (texte requis : issue et raison). Le script **produit toujours tous les plans** (sept secondes de calcul, mesuré) et **compare toujours chaque fichier produit au fichier commité** s'il existe ; **le mode fixe ce qui est admis**, et tout le reste fait échouer le job :

| Mode | Plans de `inputs.plans` | Autres plans | Condition propre |
|---|---|---|---|
| `creation` | aucun fichier commité ne doit exister | fichier commité présent et comparaison **réussie** | — |
| `regeneration` | écarts admis (c'est l'objet du visa) | comparaison **réussie** | — |
| `bascule` | `tous` obligatoire ; écarts admis sur tous | — | l'identifiant de plateforme de l'exécution **diffère** de celui des en-têtes commités (sinon ce n'est pas une bascule) ; ADR 0008, point 9 : annotation datée et constantes de `plateforme.R` mises à jour dans la même PR |

Ainsi, une création ne peut pas modifier une référence existante en silence, une régénération ciblée ne peut pas emporter un plan non visé, et une bascule se déclare. Le job en échec **publie quand même l'artefact** (`if: ${{ !cancelled() }}` sur l'étape de publication) pour que l'écart soit lisible.

5.3. **Étapes du job** : checkout jetable → bascule BLAS et LAPACK → plateforme en mode strict (sortie copiée dans `plateforme.txt`) → `Rscript tests/reference/produire_references.R --mode <mode> --plans <plans> --sortie <dossier>` (installe l'arbre de travail dans une bibliothèque temporaire comme les batteries, tire le jeu, exécute les plans, écrit `<dossier>/references/<id>.tsv` avec en-tête, compare, écrit `ecarts.txt` (impression du point 3.5) et le résumé du job (`$GITHUB_STEP_SUMMARY`), applique les règles du mode, sort avec un code non nul en cas d'échec) → `md5sum references/*.tsv > MD5SUMS` → `execution.txt` (`GITHUB_SHA`, `GITHUB_RUN_ID`, adresse `https://github.com/<dépôt>/actions/runs/<id>`, mode, plans, motif, date UTC) → publication de l'artefact `references-<run_id>` (`references/`, `MD5SUMS`, `plateforme.txt`, `execution.txt`, `ecarts.txt`), rétention 90 jours.

5.4. **Entrée sur `main`** : le fichier `.github/workflows/references.yml` entre sur `main` par un **correctif rapide** (branche temporaire partie de `main`, PR directe, domaine `repo:` seul, résultats identiques, aucune documentation de fond ; ADR 0008, point 8), **avant** le premier déclenchement, puis `main` est fusionnée dans la branche de travail. Déclenché sur `main` à ce stade, le workflow échoue faute de `tests/reference/` : c'est attendu et dit dans l'en-tête du fichier ; il se déclenche **sur la branche de travail** (`workflow_dispatch` prend la référence choisie). Préalable R1 de la feuille de route (ruleset appliqué) : avant cette fusion.

5.5. **Commit des références, par la session principale seule**, après visa (`actuary` pour les taux bruts ; mainteneur dès qu'une table finale ou un verdict change) : téléchargement de l'artefact (`*.blob.core.windows.net` autorisé depuis la session cloud), `md5sum -c MD5SUMS`, copie des fichiers sous `tests/reference/` **sans modification** (le critère « MD5 des fichiers commités égaux à ceux de l'artefact » se vérifie par `md5sum` sur l'arbre commité), retrait des marques d'échec attendu (point 3.7), commit `tests:` dont le message contient : mode, plans, identifiant et adresse de l'exécution, commit calculé, **identifiant de plateforme** (bloc de `plateforme.txt`, `CLAUDE.md`), MD5 par fichier, **tableau avant / après** (ou « création : aucune valeur existante changée »), visa (qui, quand), et, pour la création, la preuve d'ancrage du point 8. La CI de la PR doit ensuite être verte : la batterie compare l'objet recalculé aux fichiers sur la plateforme de référence. **Aucun fichier de `tests/reference/` ne provient d'un poste**, ni d'une édition manuelle : un fichier sans artefact d'origine est invalide, et la revue finale vérifie l'appariement MD5 ↔ artefact cité.

### 6. Extension de `tests/controles/identite_batteries.R`

6.1. Le contrôle 3 (chaîne `image@empreinte` et `TABLEXP_IMAGE` uniques sur tous les `.github/workflows/*.yml`) couvre déjà `references.yml` dès qu'il existe (*vérifié* : glob sur le dossier, l. 156). Le script est **étendu** d'un contrôle 4, « workflow de régénération » : `references.yml` existe ; son déclencheur est `workflow_dispatch` seul ; `permissions: contents: read` ; son job déclare `container:` et les deux variables de fils à `"1"` ; **le bloc `run: |` de l'étape « BLAS et LAPACK de référence » est identique, ligne pour ligne, à celui du job `tests:` de `ci.yml`** (la plateforme de référence est l'image **suivie de la bascule**, M47) ; une étape `Rscript tests/controles/plateforme.R --exiger` précède l'étape de production ; les SHA d'`actions/checkout` et d'`actions/upload-artifact` sont ceux de `ci.yml` ; aucun `continue-on-error`. Pour cela, le script apprend à lire un bloc `run: |` multiligne (limite déclarée en en-tête, levée) ; témoins sur des copies altérées (bascule modifiée, `on: push` ajouté, étape de plateforme retirée). Le contrôle reste dans l'étape « Identité des batteries et de l'image » du job « Tests (R 4.3.3) » et dans le job « Contrôles du dépôt » s'il y est déjà appelé (aucun R requis pour le lire ? non : il est en R ; il reste dans le job de tests).

### 7. `fiche$versions`, `fiche$plateforme`, `execution`

7.1. `execution` **n'est jamais écrit** dans une référence (il change à chaque exécution, ADR 0007, point 6.1). `fiche$versions` et `fiche$plateforme` sont **écrits dans l'en-tête**, hors comparaison, aux côtés de l'identifiant de plateforme du script, qui fait foi (ADR 0008, point 5) : ils documentent d'où vient la référence sans jamais la faire échouer (une montée de version de `tablexp` n'est pas un changement de résultat). Un changement de **l'identifiant de plateforme** entre l'en-tête commité et l'exécution courante est, lui, une bascule (point 5.2) : le workflow le détecte ; la batterie, elle, ne lit pas l'en-tête (la plateforme de la batterie est garantie par `plateforme.R --exiger` en amont dans le même job).

### 8. Ancrage à `f9ab269` et comparaison permanente « plan sans `brut` »

8.1. Le critère de #14 (« un plan sans étape `brut` donne un objet `identical` hors `execution` à celui de `f9ab269` ») devient **permanent par transitivité**, en trois pièces : (i) **ancrage** : à la création, la session cite dans le commit de référence la sortie **vide** de `git diff --stat f9ab269 <commit calculé> -- R/donnees.R R/exposition.R R/simulation.R R/outils.R` (vide à `3c8f21b`, mesuré) : les grandeurs des étapes `donnees` et `exposition` et le jeu tiré sont, **par construction du code**, ceux que `f9ab269` produit sur la même plateforme ; pour `R/rapport.R` et `R/construire_table.R`, modifiés par `125f3a3`, l'ancrage repose sur les mesures `identical()` de ce commit (trois agents, 21 plans) et sur la revue finale d'`audit` du diff, limité aux ajouts conditionnels à l'étape `brut` ; (ii) **invariance** : la batterie garde les tests « plan sans `brut` = plan avec `brut` moins ses ajouts » (R1, R4 ; `tests/test_reproductibilite.R`, l. 195-214), qui lient un plan sans `brut` aux références ; (iii) **références** : dès leur création, R1 à R5 figent les grandeurs de `donnees` et d'`exposition` sous les conventions `dernier_anniversaire` et `age_atteint_annee`, avec et sans `par_annee`, délai 0 et 90, avec et sans segmentation, unité tête et contrat. Toute modification ultérieure d'`exposition` ou de `donnees` qui change une valeur est détectée par (iii), et son effet sur un plan sans `brut` l'est par (ii). La mesure ponctuelle de `125f3a3` n'a plus à être refaite. **Limite** : la couverture est celle des plans ; une recette ou une convention absente des plans (par exemple les formes « agrégats » et « stocks successifs », à venir) n'est protégée qu'à partir du plan qui l'introduira.

### 9. Règles durables

9.1. **Ajouter une grandeur à l'objet résultat** (champ, colonne, diagnostic) est compatible pour les lecteurs (ADR 0007, point 6.3) mais **visible par la non-régression** : écart structurel « en trop », batterie en échec, régénération en mode `regeneration` du ou des plans concernés avec un tableau avant / après dont les lignes nouvelles sont marquées « création » et expliquées ; si la grandeur est un double, sa classe de tolérance est déclarée dans le même commit (point 3.4). Retirer ou renommer une grandeur : idem, plus la règle du contrat partagé « objet résultat ».

9.2. **Changer le format** (point 2.3 : nouveau type, nouvelle ligne `@`, autre échappement) incrémente la version de la première ligne (`tablexp reference 2`) et régénère tous les plans ; une annotation datée le consigne.

9.3. **Tolérances** : toute modification de `TOLERANCES` est annotée ici, datée, avec l'avis d'`actuary` ; élargir une tolérance pour « faire passer » un écart est interdit sans tableau avant / après et visa (c'est un changement de résultat déguisé).

9.4. **Un fichier de `tests/reference/` ne change que par un commit de la session principale citant une exécution du workflow et un visa** ; agents et workflows ne l'écrivent jamais (`CLAUDE.md`).

## Options écartées

- **`saveRDS()` / `serialize()` des objets** : binaire illisible en revue, en-tête portant la version de R (ADR 0007, mesuré), aucune tolérance par grandeur possible sans tout relire, `git diff` muet ; `identical()` sur un RDS serait de plus faux au premier bit de `libm` différent.
- **`dput()` / `dget()`** : évaluation de code à la lecture (ADR 0007, point 12.2 (c)), représentation des doubles non garantie à 17 chiffres.
- **Un fichier CSV par table et un fichier clé-valeur pour le reste** : deux formats, deux lecteurs, et la structure (noms, ordre des colonnes, classe) dispersée ; le format aplati n'a qu'un lecteur et porte la structure dans les lignes `@`. Son seul coût est la lisibilité d'une ligne longue, compensée par l'impression par grandeur (point 3.5), qui est la vraie interface de revue.
- **Une ligne par valeur (`chemin[i]<TAB>valeur`)** : plus lisible dans `git diff`, mais le chemin répété pèse trois à quatre fois la valeur (environ 70 octets contre 22 mesurés) : R4 passerait d'environ 0,3 à plus d'un mégaoctet par régénération ; le tableau avant / après vient de l'impression, pas du diff.
- **Recopier les tables de lignes** (point 1.2) : des dizaines de mégaoctets par plan, pour des entiers, dates et chaînes exacts, reconstituables partout ; le résumé par empreinte perd la lisibilité de l'écart mais pas sa détection.
- **Exclure les tables de lignes sans résumé** : une permutation ou une fusion de têtes différente sans effet sur l'exposition passerait inaperçue ; le résumé coûte 0,4 s par table.
- **`all.equal()`** : tolérance unique de 1,5e-8 par défaut, qui cache tout écart de méthode à cet ordre et ne distingue pas les classes d'`actuary` ; verdict textuel non structuré.
- **Une tolérance unique pour tous les doubles** (1e-12 partout) : simple, mais elle laisserait passer sur `taux` et `exposition_centrale` un bruit cent fois supérieur à celui que l'arithmétique de base peut produire ; deux classes nommées rendent chaque écart interprétable (où est le bruit, où est la méthode).
- **Tolérance par défaut pour un double non déclaré** (exacte ou 1e-12) : une grandeur nouvelle entrerait sans jugement d'`actuary` ; l'échec « sans tolérance déclarée » force la décision.
- **Comparaison par `identical()` seule** : contraire à `CLAUDE.md` (« à tolérance ») ; fragile à la `libm` entre exécuteurs (ADR 0008, hypothèse ouverte) ; et surtout sans tableau avant / après.
- **Références produites par le job « Tests (R 4.3.3) » à chaque exécution** (artefact de chaque PR comparé à `main`) : ce serait une régénération sans visa ; les références sont des faits commités, datés, visés.
- **Références gardées en artefact seulement, non versionnées** : rétention limitée, aucune trace dans l'historique, aucune revue possible dans une PR.
- **Jeu d'entrée versionné sous `inst/extdata/`** : décision M50 (Q6 (b)), renvoyée à `claude/simulateur-formes` ; le jeu tiré à graine fixée a le même effet de gel (point 4.2) tant que le simulateur est figé, et l'empreinte en première grandeur détecte tout glissement.
- **Un workflow par mode** (trois fichiers) : trois copies de la plateforme à tenir identiques ; un fichier à trois modes et des règles mécaniques par mode (point 5.2) suffit.
- **Ancrage à `f9ab269` par exécution du workflow sur `f9ab269`** : ce commit n'a ni le workflow ni l'outillage ; il faudrait greffer l'outillage sur une branche temporaire partie de `f9ab269`, pour prouver ce que le diff vide des modules producteurs prouve déjà (point 8).
- **Un test « plan sans `brut` identique à `f9ab269` » codé en dur dans la batterie** (valeurs de `f9ab269` recopiées) : c'est une référence de non-régression sous un autre nom, hors du circuit visa et plateforme ; les références R1 à R5 la remplacent (point 8).
- **Écriture des valeurs par `format(digits = 17)` ou `as.character()`** : `as.character` arrondit à 15 chiffres (ADR 0007, mesuré) ; `%.17g` fait l'aller-retour (mesuré ici sur 19 370 doubles).
- **JSON ou YAML** : aucun lecteur en R de base (ADR 0002 ; ADR 0007, « Options écartées »).

## Conséquences

- **Fichiers** : `.github/workflows/references.yml` (commit `repo:`, correctif rapide, point 5.4) ; `tests/reference/outils_reference.R`, `produire_references.R`, `comparer_references.R` (commits `tests:`, `coder`) ; `tests/test_reproductibilite.R` (comparaison, échecs attendus `#17`, témoins ; en-tête mis à jour) ; `tests/controles/identite_batteries.R` (contrôle 4, lecture des blocs `run: |`, témoins) ; `tests/reference/R1.tsv` à `R6.tsv` (commit `tests:` de la session principale, point 5.5) ; `CONTEXT.md` (`architect`, commit distinct) : « Référence de non-régression », « Grandeur de référence », « Classe de tolérance », « Plan de référence », « Entrée de référence » ; `README.md` (session, `docs:`) : commande de déclenchement du workflow et de téléchargement de l'artefact, place de `tests/reference/` ; `docs/latex/` (`docwriter`, fin de branche) : section tests et non-régression, surface d'impact listée par `coder` ; `CLAUDE.md` est cohérent tel quel (il peut citer l'ADR 0009 à côté de l'ADR 0008 dans « Changements de résultats », alignement `claude:` par la session principale). ADR 0005 (point 8) et ADR 0007 (point 11.1) : mentions datées « réglé par l'ADR 0009 » ; ADR 0008, point 8 : mention datée.
- **Effet sur les résultats** : **aucun**. Création de références ; aucune valeur existante ne change ; aucun code du moteur n'est touché par cet ADR.
- **Pour `coder`** : l'outillage (points 1 à 3), le workflow (point 5), l'extension du contrôle (point 6), les témoins (point 3.6) ; chaque commit liste sa surface d'impact documentaire. **Pour `audit`** : témoins exécutés, scénarios adverses de la feuille de route (référence altérée, grandeur manquante ou en trop, image différente), lecture du workflow contre les invariants 5.1. **Pour `actuary`** : classes de tolérance des grandeurs de R6 (#15), visa des premières références (bilan et écarts maximaux par classe imprimés), confirmation ou réfutation de l'hypothèse de bruit nul après les premières exécutions. **Pour la session principale** : correctif rapide, déclenchement, téléchargement, vérification MD5, commit avec identifiant de plateforme, tableau et ancrage ; `CONTEXT.md` par `architect`.
- **Hypothèses et risques acceptés** : (a) empreinte des données identique sur la plateforme de référence (point 4.1 ; tranchée au premier déclenchement) ; (b) bruit nul de l'arithmétique sur la plateforme de référence, et variantes de `libm` entre processeurs (ADR 0008) : les tolérances les absorbent sans les masquer, l'écart maximal par classe est imprimé à chaque exécution ; (c) taille : environ 1,3 Mo de valeurs pour R1 à R5 (estimé d'après R4 mesuré à 0,29 Mo de doubles), réécrits à chaque régénération, rares par construction ; (d) durée du job : image, `apt-get`, installation du package, sept secondes de calcul ; (e) téléchargement de l'artefact depuis une session cloud soumis à l'autorisation de `*.blob.core.windows.net` (ADR 0008) ; (f) indépendance des tables de lignes à la plateforme (point 1.2), non mesurée sur la plateforme de référence.
- **Tests qui protègent l'ADR** : témoins du point 3.6 ; échec attendu `#17` par plan sans fichier (point 3.7) ; référence orpheline ; contrôle 4 d'identité et ses témoins (point 6) ; aller-retour ; comparaison aux références dans la batterie (point 3.8).
- **Ce que l'ADR ne règle pas** : les grandeurs et tolérances de Kaplan-Meier (R6 ; `actuary`, #15) ; le format de la fiche sérialisée et son éventuelle convergence avec le point 2.3 (`claude/fiche-rapport`) ; la tolérance propre au code compilé (ADR 0002, A8 : ADR de méthode) ; les références du suivi d'expérience et du bootstrap sous `parallel` (branches ultérieures ; même mécanisme, nouveaux plans) ; les plans de référence des formes « agrégats » et « stocks successifs » (`claude/simulateur-formes`) ; la transmission d'`ImageOS` et `ImageVersion` au conteneur (ADR 0008, annotation du 16/10/2026, point mineur laissé à cette branche : le workflow peut les passer en `env` depuis `${{ runner.os }}` et les variables de l'hôte si `coder` trouve la forme, sans que l'ADR l'impose).

### Points soumis au mainteneur

1. **Format et emplacement** (points 2.1 à 2.3) : un fichier texte aplati par plan sous `tests/reference/<id>.tsv`, une ligne par vecteur, structure en lignes `@`, valeurs selon l'ADR 0007 (recommandé) ; alternative écartée : un CSV par table.
2. **Tables de lignes résumées par nombre de lignes et empreinte MD5** (point 1.2) plutôt que recopiées ou exclues (recommandé).
3. **Tolérances d'`actuary`** (point 3.4) avec la règle « double sans classe déclarée = échec » (recommandé).
4. **Entrée** (point 4.1) : n = 10^5, graine 20261016 (recommandé ; `actuary` a mesuré avec 20261017 : la graine des batteries est préférée pour n'avoir qu'un tirage de référence dans le projet) ; conséquence : simulateur figé pour cette graine (point 4.2).
5. **Un workflow, trois modes à règles mécaniques** (point 5.2), artefact de 90 jours, production systématique de tous les plans (recommandé).
6. **Ancrage à `f9ab269`** par le diff vide des modules producteurs, l'invariance en batterie et les références (point 8), sans exécution du workflow sur `f9ab269` (recommandé).
7. **Plan sans fichier = échec attendu `#17`** jusqu'à la création (point 3.7) (recommandé).
8. **Toute grandeur ajoutée à l'objet résultat passe par une régénération visée** (point 9.1), ce qui rend chaque extension de l'objet visible (recommandé ; alternative : ignorer les chemins « en trop », écartée parce qu'une grandeur non comparée n'est pas une grandeur de référence).

Issues : #17 (périmètre) ; #14 (critère d'invariance, point 8) ; #15 (R6) ; #16 (constats de simulation, hors références). Décisions M50 (Q6) ; ADR 0005 (point 8), ADR 0007 (points 6.1, 9.3, 11.1, 12), ADR 0008 (points 5, 8, 9 ; annotation M47).

## Annotation du 17 octobre 2026 — ADR accepté par le mainteneur (décision M52) ; plan R6 et tolérances de Kaplan-Meier fixés par la spécification de #15

Rédigée par `architect` d'après la décision du mainteneur du 17/10/2026 (M52, `docs/feuille-de-route.md`, § 4) et la spécification d'`actuary` sur #15 (https://github.com/Gerard-Garey/outil_tables/issues/15#issuecomment-6236222949, fin de A.14). Le texte ci-dessus n'est pas réécrit ; seul l'en-tête `status` passe de `proposed` à `accepted`. La mention « Statut : `proposed` » du premier paragraphe se lit désormais à la lumière de la présente annotation.

**(a) Statut : `accepted`.** Le mainteneur a accepté le 17/10/2026 (M52) les huit points de la liste « Points soumis au mainteneur », chacun selon la recommandation de l'ADR :

1. format et emplacement : un fichier texte aplati par plan sous `tests/reference/<id>.tsv`, une ligne par vecteur, structure en lignes `@` (points 2.1 à 2.3) ;
2. tables de lignes de l'étape `donnees` résumées par nombre de lignes et empreinte MD5 (point 1.2) ;
3. tolérances d'`actuary` et règle « double sans classe déclarée = échec » (point 3.4) ;
4. entrée : n = 10^5, graine 20261016 (point 4.1), d'où le simulateur figé pour cette graine (point 4.2) ;
5. un workflow à trois modes à règles mécaniques, artefact de 90 jours, production systématique de tous les plans (point 5.2) ;
6. ancrage à `f9ab269` par transitivité (diff vide des modules producteurs, invariance en batterie, références), sans exécution du workflow sur `f9ab269` (point 8) ;
7. plan sans fichier = échec attendu `#17` jusqu'à la création (point 3.7) ;
8. toute grandeur ajoutée à l'objet résultat passe par une régénération visée (point 9.1).

**(b) Plan R6, fixé par la spécification de #15.** La ligne R6 du tableau du point 4.3, laissée « à fixer par la spécification de #15 », se lit : **R6 = R1 avec `etapes$brut$estimateurs` = `c("poisson", "binomial", "kaplan_meier")`**, c'est-à-dire décès, segmentation `sexe`, délai 0, convention `plus_fine` (→ `dernier_anniversaire`), `par_annee = FALSE`, niveau 0,95. R6 couvre ainsi l'estimateur de Kaplan-Meier sans `par_annee`, que la spécification refuse (`kaplan_meier_par_annee`, A.8). Les décisions du mainteneur sur les questions A.14 de #15 (M52, (b)) fixent la forme de ce que R6 compare : ensemble à risque fermé, tables `kaplan_meier` et `kaplan_meier_sauts` sans survie cumulée par segment.

**(c) Tolérances de Kaplan-Meier, avis d'`actuary` (#15, fin de A.14).** Au titre de la règle de complétude du point 3.4 :

- **1e-12** pour q̂ (`taux` de la table `kaplan_meier`), sa `variance` et ses bornes asymptotiques (`borne_inf_asymptotique`, `borne_sup_asymptotique`) : calcul par `log1p`, `expm1` et `plogis`, donc fonctions de la `libm`, comme la classe 1e-12 existante ;
- **exacte** pour r (`ensemble_a_risque`), d (`evenements`) et `numerateur`, entiers.

Ces deux lignes complètent la table `TOLERANCES` ; la présente annotation est l'annotation datée que le point 3.4 exige pour toute modification de cette table.

**(d) Points de forme laissés ouverts, à régler avant la création de R6** (constat de l'architecte sur la lecture conjointe du point 3.4 et de A.9 de la spécification ; *non vérifié* contre le code, que `coder` écrit en parallèle) :

1. **Clé de tolérance en collision.** La table `TOLERANCES` est indexée par le dernier segment du chemin (point 3.4). Or la colonne q̂ de Kaplan-Meier s'appelle `taux` (A.9), nom déjà classé en 1e-14 pour Poisson et binomial. La clé du dernier segment ne peut donc porter 1e-12 pour `etapes/brut/resultat/kaplan_meier/taux` sans changer la classe des taux de Poisson et binomial. Deux formes possibles : une clé plus spécifique (`<table>/<colonne>`) prioritaire sur le dernier segment, ou un nom de colonne distinct. Le choix revient à `coder` sous l'avis d'`actuary` ; il est consigné par une annotation datée de cet ADR.
2. **Doubles de `kaplan_meier_sauts` sans classe.** L'avis de (c) ne classe ni `age_exact`, ni `survie_cellule`, ni `variance_log_survie_cellule` (qui vaut `Inf` après d = r, position relevant de la structure, point 3.1). La table `kaplan_meier_sauts` est comparée valeur par valeur (point 1.3). Par la règle de complétude, ces grandeurs feraient échouer R6 tant qu'`actuary` ne les a pas classées.

**Effet sur les résultats** : aucun. L'annotation consigne une décision et une spécification ; aucune référence n'existe encore (création après #14 à #16, point 3.7).

Issues : #17, #15. Décisions M52 ; M50 (Q6).

## Annotation du 17 octobre 2026 (seconde) — points ouverts de la section (d) réglés : clé qualifiée `<table>/<colonne>`, classes de Kaplan-Meier, comparaison élément par élément

Rédigée par `architect` d'après l'avis d'`actuary` du 17/10/2026, retenu par le mainteneur le même jour. Le texte ci-dessus, première annotation comprise, n'est pas réécrit ; la présente annotation règle les deux points de sa section (d) et clarifie le point 3.3. C'est aussi l'annotation datée que le point 3.4 exige pour toute modification de `TOLERANCES`.

**(a) Collision du nom `taux` (point (d) 1) : clé qualifiée prioritaire, remède (a).** La table `TOLERANCES` admet, outre la clé du dernier segment du chemin (point 3.4), une clé qualifiée `<table>/<colonne>`, **prioritaire** sur le nom nu. La recherche se fait par le **suffixe le plus long** du chemin : pour `etapes/brut/resultat/kaplan_meier/taux`, la clé `kaplan_meier/taux` d'abord, `taux` ensuite. Le nom de colonne `taux` reste commun aux trois estimateurs (Poisson, binomial, Kaplan-Meier). Raisons d'`actuary` :

- la tolérance dépend du calcul qui produit la grandeur, pas du sens de la grandeur ;
- le nom commun fait partie du contrat « objet résultat » (`CONTEXT.md`) ;
- d'autres collisions viendront avec le lissage et le positionnement ; la clé qualifiée les absorbe sans renommage.

Les grandeurs propres à Kaplan-Meier se déclarent sous clé qualifiée. Le script de comparaison (point 2.6) imprime, pour chaque chemin que deux clés pourraient classer, la clé retenue et la classe. Option écartée : un nom de colonne distinct pour q̂ de Kaplan-Meier, qui romprait le nom commun du contrat et ne règlerait pas les collisions à venir.

**(b) Classes de tolérance de Kaplan-Meier (point (d) 2), avis d'`actuary`.** Ces lignes complètent la table `TOLERANCES` du point 3.4 et précisent la section (c) de la première annotation :

| Classe | Clés qualifiées | Justification (`actuary`) |
|---|---|---|
| **1e-12** | `kaplan_meier/taux`, `kaplan_meier/variance`, `kaplan_meier_sauts/survie_cellule` | `taux` et `variance` : `log1p`, `expm1`, `plogis`, fonctions de la `libm` (première annotation, (c)) ; `survie_cellule` passe par la `libm` (`log1p`, `expm1`) et vaut 1 − `taux` au dernier saut ; plus petit changement significatif ≥ 1e-5 |
| **1e-14** | `kaplan_meier_sauts/age_exact`, `kaplan_meier_sauts/variance_log_survie_cellule` | `age_exact` : une division et une addition IEEE, plus petit changement significatif ≥ 5,8e-8 en relatif ; `variance_log_survie_cellule` : somme cumulée en arithmétique de base, plus petit changement significatif ≥ 2e-5. *Hypothèse non mesurée* : l'accumulation de `cumsum` en `long double` dépend de l'architecture, mais à quelques 1e-16 près seulement |
| **exacte** | `numerateur`, `ensemble_a_risque`, `instants_evenement`, `denominateur_instants` (sous clés qualifiées de leur table) et les cinq diagnostics `diagnostics/kaplan_meier/instants`, `diagnostics/kaplan_meier/evenements`, `diagnostics/kaplan_meier/evenements_a_l_instant_d_entree`, `diagnostics/kaplan_meier/cellules_survie_nulle`, `diagnostics/kaplan_meier/ensemble_a_risque_maximal` | dénombrements ; le moteur les stocke en `integer`, ce qui les place aussi dans la classe exacte par leur type (point 3.4) |

Les bornes asymptotiques de la table `kaplan_meier` (première annotation, (c)) restent en 1e-12 par leur nom nu, déjà classé (point 3.4). La valeur `Inf` de `variance_log_survie_cellule` après d = r relève de la structure (point 3.1) et de la règle (c) ci-dessous.

**(c) Clarification du point 3.3 : la règle « égalité exacte si `identical` » s'applique élément par élément.** Qualifiée de clarification par le mainteneur, sans autre visa. Pour deux vecteurs de même chemin, chaque paire d'éléments (a, b) se juge ainsi :

1. si `identical(a, b)`, les éléments sont égaux, y compris `Inf`, `-Inf`, 0, `NA` et `NaN` — sans confondre `NA` avec `NaN`, ni `Inf` avec `-Inf` ;
2. sinon, la règle de tolérance du point 3.3 s'applique (deux valeurs finies, non nulles, de même signe, |a − b| ≤ tol · max(|a|, |b|)) ;
3. tout autre cas est un écart.

C'était le sens de l'avis d'`actuary` sur #14 (partie B) ; aucun écart réel n'est absorbé. Mesure d'`actuary` sous R 4.3.3, avec `a = c(Inf, 0, 0.1 + 0.2)`, `b = c(Inf, 0, 0.3)`, `tol = 1e-14` : la lecture au niveau du vecteur (`identical` sur le vecteur entier, puis tolérance) donne `FALSE FALSE TRUE` ; la lecture élément par élément donne `TRUE TRUE TRUE`.

**Effet sur les résultats** : aucun. Aucune référence n'existe encore (création après #14 à #16, point 3.7) ; l'annotation fixe la règle avant la création de R6.

Issues : #17, #15, #14. Décision du mainteneur du 17/10/2026 (avis d'`actuary` retenu) ; M52.

## Annotation du 17 octobre 2026 (troisième) — clé `avertissements/nombre`, ligne `@noms` des vecteurs nommés, représentation de l'empreinte des tables résumées, limites admises, hypothèse de locale corrigée

Rédigée par `architect` d'après les avis d'`actuary` et d'`architect` et l'audit de l'outillage de #17, tous du 17/10/2026. Le texte ci-dessus, annotations comprises, n'est pas réécrit. C'est aussi l'annotation datée que le point 3.4 exige pour toute modification de `TOLERANCES`.

**(a) `TOLERANCES` : clé qualifiée `avertissements/nombre`, classe exacte.** Par la recherche au suffixe le plus long (seconde annotation, (a)), elle couvre aussi `fiche/avertissements/nombre`. Motif, avis d'`actuary` : ces valeurs sont des dénombrements, convertis en `double` par `as.numeric(nombre)` dans `avertir()` (`R/outils.R:65`), sans arithmétique flottante. La clé est qualifiée, et non le nom nu `nombre`, pour respecter la règle de complétude du point 3.4 : un autre double nommé `nombre` resterait sans classe et ferait échouer la comparaison. Cette clé deviendra redondante quand l'issue #24 (stockage en `integer`) passera, le type `integer` étant exact par lui-même ; son retrait s'annotera ici.

**(b) Format, point 2.3, version 1 : ligne `chemin/@noms` des vecteurs atomiques nommés.** Tout vecteur atomique dont `names` n'est pas `NULL` porte une ligne `chemin/@noms` (type `character`, les noms dans l'ordre) juste avant sa ligne de valeurs. Motif : sans cette ligne, renommer un élément de `fiche/roles` ou de `fiche/generateur` passerait inaperçu, les valeurs restant égales. La règle entre dans la **version 1** du format (`tablexp reference 1`), sans incrément, parce qu'aucune référence n'existe encore (point 3.7).

**(c) Représentation de l'empreinte des tables résumées (point 1.2), octet pour octet.** Le point 1.2 la décrivait en une phrase ; elle se lit ainsi :

- **data.frame** : pour chaque colonne, dans l'ordre de la table, la ligne `#<nom>\t<type>\n`, puis les octets produits par `tablexp:::colonne_canonique()` ;
- **vecteur** : même règle, sous son nom ;
- **liste** : pour chaque élément, dans l'ordre, la ligne `@<nom>\n` (ou `[i]` si l'élément n'a pas de nom), puis sa représentation, récursivement ;
- texte en UTF-8 ; les attributs autres que `names`, `class` et `row.names` sont ignorés (point 1.2) ;
- une liste vide donne un texte vide, de MD5 `d41d8cd98f00b204e9800998ecf8427e`.

**(d) Limites admises.**

1. **Niveaux des facteurs non écrits.** Sans portée aujourd'hui : l'objet ne contient aucun facteur. Si un facteur y apparaît, il faudra une ligne `@niveaux`, donc un changement de format (version 2, point 2.3).
2. **Noms de lignes.** Un data.frame comparé dont les noms de lignes ne sont pas automatiques entraîne un refus d'écriture, avec le chemin fautif (point 2.3 : rien n'est ignoré en silence).
3. **Position des `NA`.** Une différence de position d'un `NA` est un écart structurel, quel que soit le type (point 3.1, étendu à tous les types).
4. **Plateforme en mode bascule.** L'identifiant de plateforme comparé ne retient que les lignes contrôlées par `tests/controles/plateforme.R`. Les lignes seulement consignées (processeur, drapeaux) sont écrites dans l'en-tête mais pas comparées.

**(e) Correction de l'hypothèse du point 1.2 « aucune dépendance à la locale ».**

- **Vérifiée pour l'objet du moteur**, mesure d'`architect` du 17/10/2026 : 400 977 chaînes, mêmes octets dans les quatre combinaisons installation × exécution (C ou UTF-8), MD5 `942c0745b937aa2327442f4980e974c4`.
- **Fausse pour l'outillage**, qui appelait `enc2utf8()` sur des chaînes non marquées : en session C, une chaîne non ASCII non marquée est de codage indéterminé et la conversion la dénature (ADR 0007, point 9.3).
- **Corrections** : les chaînes de l'outillage suivent la règle d'`octets_chaines()` (`R/rapport.R:41` ; ADR 0007, point 9.3), par **refus** et non par conversion ; le lecteur traite les octets ; les tests T1 à T4 sont ajoutés.
- **Mesure de l'audit** : R1 produit sous `C` et sous `C.UTF-8` donne le même MD5, `7b0f55f1fe2b7265d21548f493973c7b`.

La locale d'installation du package relève de l'ADR 0007 (annotation M54).

**Effet sur les résultats** : aucun. Aucune référence n'existe encore (point 3.7) ; l'annotation fixe le format et la classe avant la création.

Issues : #17, #24, #25. Décision M54 (ADR 0007) ; M52.

## Annotation du 17 octobre 2026 (quatrième) — retrait de la clé `avertissements/nombre` de `TOLERANCES` après #24

Rédigée par `architect` d'après l'avis d'`actuary` et la mesure de `coder`, tous deux du 17/10/2026. Le texte ci-dessus, annotations comprises, n'est pas réécrit. C'est l'annotation datée que le point 3.4 exige pour cette modification de `TOLERANCES`, comme l'annonçait la troisième annotation, (a).

**(a) Type `integer` du `nombre` des avertissements (#24).** `avertissements_vides()` déclare la colonne `nombre` en `integer(0)`. `avertir()` stocke `as.integer(nombre)` après un contrôle qui refuse par `stop()` toute valeur non numérique, de longueur autre que 1, `NA`, infinie, non entière, négative ou supérieure à `.Machine$integer.max`. Le refus porte sur une erreur de programmation du package : il n'y a aucune conversion silencieuse.

**(b) Retrait de la clé.** La clé qualifiée `"avertissements/nombre" = 0`, introduite par la troisième annotation, (a), est retirée de `TOLERANCES` (`tests/reference/outils_reference.R`). Motif, avis d'`actuary` : la tolérance n'est cherchée que pour un `double`. Une fois le type en `integer`, la clé ne sert plus. Si on la laissait en place, elle neutraliserait la règle de complétude du point 3.4 au cas où une régression remettrait le type en `double`, car un tel `double` trouverait encore sa classe. Sans la clé, cette régression est relevée deux fois :
- par le contrôle de type, comme écart structurel ;
- par `doubles_sans_tolerance()`.

**(c) Témoin**, mesuré par `coder` le 17/10/2026 : sur R1 produit par `1906ae9`, où le type est encore `double`, `doubles_sans_tolerance()` relève `avertissements/nombre` et `fiche/avertissements/nombre`.

**(d) Texte périmé de la troisième annotation.** Le motif de la troisième annotation, (a), cite la conversion par `as.numeric(nombre)` dans `avertir()` (`R/outils.R:65`). Cette mention est désormais **périmée** : la conversion est `as.integer(nombre)`, précédée du contrôle décrit en (a), et la ligne citée a changé. Elle reste vraie pour l'état antérieur à #24 (`1906ae9`). La remarque de la même annotation sur le nom nu `nombre` garde sa portée : un autre `double` nommé `nombre` resterait sans classe et ferait échouer la comparaison.

**Effet sur les résultats** : le type de `avertissements$nombre` et de `fiche$avertissements$nombre` passe de `double` à `integer` (deux chemins par plan). **Aucune valeur ne change.** Le visa d'`actuary` est accordé le 17/10/2026. Il n'y a aucun tableau avant / après de références, car aucune référence n'existe encore (point 3.7) : le type `integer` entre dans la version 1 dès la création.

Issues : #24, #17.

## Annotation du 18 octobre 2026 — premières références créées ; lecture des doubles selon la plateforme (point 2.4) ; garde du dossier de sortie portable (point 9.4) ; ancrage par transitivité (point 8.1 (i)) ; bruit nul constaté sur la plateforme de référence

Rédigée par `architect` d'après les messages des commits `ddf77bd` et `1019d8e` (branche `claude/taux-bruts-non-regression`) et le journal du job « Tests (R 4.3.3) » de l'exécution 39148101369 de la CI (commit `1019d8e`, job 117735985035), relu le 18/10/2026. Le texte ci-dessus, annotations comprises, n'est pas réécrit. Chaque chiffre est cité avec sa source ; aucun n'a été remesuré par `architect`, sauf le décompte du journal de la CI en (d) et les maillons de l'ancrage en (c).

**(a) Point 2.4, aller-retour : la lecture des doubles dépend de la plateforme.** Le point 2.4 affirmait l'aller-retour au bit près, mesuré sur 19 370 doubles dans un conteneur Linux. Le passage en condition réelle sur le poste Windows du mainteneur (R 4.3.3 ucrt) l'a contredit : `lire_reference()` refusait `R6.tsv` produit par la CI (exécution 39140635363 ; message de `ddf77bd`). Depuis `ddf77bd`, `double_lu()` (`tests/reference/outils_reference.R`) applique deux règles.

- **Hors Windows, plateforme de référence comprise** : le texte lu doit être égal à `sprintf("%.17g")` de la valeur lue. Statut : **preuve d'aller-retour**. La glibc arrondit correctement, donc un texte reproduit est le seul texte de la valeur lue. Un texte lu en un double voisin n'est pas reproduit : il est refusé.
- **Sous Windows** : le texte doit seulement avoir la forme de `%.17g` (`forme_double_17g()`). Statut : **constat de mesure**, pas preuve. Deux défauts mesurés (message de `ddf77bd`) :
  - le `sprintf` de Windows n'arrondit pas toujours correctement le 17ᵉ chiffre (exemple : 0x1.422927add0ecbp-18 s'écrit 4.8005694404694591e-06 sous glibc et 4.800569440469459e-06 sous Windows) ;
  - `R_strtod` ne lit pas toujours le double désigné par le texte.

  Ce qui est constaté : les textes correctement arrondis se relisent exactement. Les six fichiers de la CI, lus sous Windows, donnent 42 952 textes de doubles, et aucun n'est lu en un autre double que celui de Python (arrondi correct). Les autres mesures sont dans le commentaire de `double_lu()`.

La branche stricte de `ddf77bd` a été comparée au code de `8ca8ce4` : 0 désaccord sur 46 089 textes, en acceptation et en valeur (46 060 des six fichiers de la CI et 29 textes piégés ; message de `ddf77bd`). Les deux branches sont testées sur toute plateforme : plateforme et formateur sont injectables.

Conséquences :

1. Une référence ne se produit jamais hors de la CI. C'était déjà la règle (`CLAUDE.md` ; points 2.6 et 5.5) ; elle a désormais aussi une raison technique. Un texte écrit sous Windows peut différer au 17ᵉ chiffre de celui de la plateforme de référence.
2. Une comparaison aux références exécutée sous Windows a le statut de constat de mesure. Seule celle de la plateforme de référence (et de tout hôte glibc) repose sur une preuve d'aller-retour.

Remarque de vocabulaire : « constat de mesure » s'entend ici au sens des statuts de `CLAUDE.md`, « Rigueur ». Ce n'est pas un « Constat » au sens de `CONTEXT.md` (défaut relevé par un vérificateur).

**(b) Point 9.4, garde du dossier de sortie : portable depuis `ddf77bd`.** Le point 9.4 interdit à tout agent et à tout workflow d'écrire sous `tests/reference/`. `produire_references.R`, exécutable sur le poste en mode relevé (point 2.6), le garantit par `sortie_interdite()`. Avant `ddf77bd`, cette garde ne tenait pas sous Windows : `--sortie tests/reference/essai` était accepté et le producteur écrivait sous `tests/reference/` (constat sur le poste du mainteneur ; message de `ddf77bd`).

Depuis `ddf77bd`, `forme_chemin()` normalise les points suivants : séparateurs, lecteur et casse, chemins UNC, préfixe `\\?\` et chemins relatifs. La garde résout ensuite par le système l'ancêtre existant du texte brut, avant toute réduction des « .. ». Les liens POSIX, les noms courts 8.3 et les jonctions sont donc suivis avant la comparaison.

Mesure (message de `ddf77bd`) : 35 scénarios adverses sur 35 (jonctions, 8.3, casse, UNC, « .. »). Une réserve mineure restait ouverte : sur un hôte POSIX où `file.symlink()` échoue, le test des liens est sauté avec trace. Elle est levée pour la plateforme de référence. Le journal de l'exécution 39148101369 ne contient pas la trace de saut de ce test (`tests/test_reproductibilite.R:682`) ; la seule trace de saut de la garde est celle du test réservé à un hôte Windows (`tests/test_reproductibilite.R:658`).

Dans la même revue, le témoin T3 passait à tort sous Windows (`system2(env =)` n'y est pas appliqué). Il est corrigé par `Sys.setenv` (message de `ddf77bd`). Cela relève de la troisième annotation, (e).

**(c) Point 8.1 (i), ancrage à `f9ab269` : la forme transitive est admise et doit être citée comme telle.** Le point 8.1 (i) prévoyait de citer un diff **vide** des modules producteurs entre `f9ab269` et le commit calculé. À la création, ce diff n'était plus vide : `git diff --stat f9ab269 8ca8ce4 -- R/donnees.R R/exposition.R R/simulation.R R/outils.R` touche `R/exposition.R` (factorisation de #15) et `R/outils.R` (#24). Le commit `1019d8e` établit donc l'ancrage par une **chaîne de maillons**, chacun mesuré :

1. `f9ab269 → 103d396` : diff des quatre modules vide ;
2. `103d396 → aa76b1d` : objets `identical()` hors `execution` pour les plans sans `kaplan_meier` (56 constructions pour `coder`, 530 pour `audit` ; message de `aa76b1d`) ;
3. `aa76b1d → 8ca8ce4` : seul `R/outils.R` change (#24, `f50f63d` : type `integer` de `avertissements$nombre`, valeurs identiques, reste de l'objet `identical`).

Règle durable :

- L'ancrage par transitivité est une forme de preuve **admise** pour le point 8.1 (i).
- Le commit de référence le cite **sous cette forme** : un maillon par ligne, sa mesure (diff vide, ou `identical()` avec le nombre de constructions et le commit qui porte la mesure), et le changement de type ou de valeur qu'il admet.
- Un maillon sans mesure citée rompt l'ancrage.
- Un changement de valeur rompt la chaîne : il ne s'admet pas dans un maillon, il relève du tableau avant / après et du visa. Un changement de type sans changement de valeur (maillon 3) s'admet s'il a été visé (quatrième annotation).

Maillons recontrôlés par `architect` le 18/10/2026 : `git diff --stat f9ab269 103d396 -- R/donnees.R R/exposition.R R/simulation.R R/outils.R` est vide ; `git diff --stat aa76b1d 8ca8ce4 -- R/` ne touche que `R/outils.R` ; le message de `aa76b1d` porte bien les 56 et 530 constructions.

**(d) Hypothèse de bruit nul sur la plateforme de référence (« Conséquences », (b) ; ADR 0008) : confirmée par mesure, au statut de constat.** La CI de `1019d8e` a recalculé R1 à R6 dans une seconde exécution de la plateforme de référence (39148101369, « Plateforme conforme à la plateforme de référence »). Elle les a comparés aux fichiers produits par l'exécution 39140635363.

Les lignes « Bilan : … grandeur(s) comparée(s) » du journal (relues par `architect`) donnent 234 (R1), 231 (R2), 234 (R3), 220 (R4), 234 (R5) et 267 (R6) grandeurs comparées. Toutes sont identiques au bit près, soit **1 420 grandeurs**. Une grandeur est ici un chemin, c'est-à-dire un vecteur, et non une valeur. On compte 0 écart sous tolérance, 0 écart, 0 grandeur sans tolérance déclarée et 0 écart structurel. L'écart relatif maximal vaut 0 dans les trois classes, pour chaque plan. La batterie compte 111 tests, 0 échec, 0 échec attendu.

Hors plateforme de référence, sur le poste Windows du mainteneur (message de `1019d8e`) :

- R1 à R6 : verdict OK ;
- classes exacte et 1e-14 : écart nul ;
- classe 1e-12 : écart relatif de 1,84e-15 au plus (R6), soit plus de deux ordres de grandeur sous la tolérance.

Ce que la mesure établit, et ce qu'elle n'établit pas :

1. **Constat sur deux exécutions, pas preuve.** Les deux exécutions de la plateforme de référence ont tourné sur le même modèle de processeur : AMD EPYC 7763 (consigné dans `1019d8e` pour 39140635363 ; journal de 39148101369). La réserve de l'ADR 0008 sur les variantes de la `libm` d'un processeur à l'autre (AMD EPYC 9V45 déjà observé, ADR 0008) n'est donc **pas exercée**. Elle reste **ouverte**.
2. **Tolérances bien dimensionnées.** Le seul écart non nul mesuré est celui du poste Windows, qui diffère de la plateforme de référence par la `libm` comme par le système. Il entre dans la classe 1e-12 avec une marge supérieure à 500. Aucune tolérance ne change.
3. **Hypothèses (a) et (f) de « Conséquences » : réglées par constat.**
   - (a) L'empreinte des données mesurée sur la plateforme de référence est `f3519cc1b73ab0d1fcc6ad89dc6d595c` : c'est la valeur mesurée hors plateforme au point 4.1 (`fiche/empreintes/donnees` des six fichiers ; en-têtes).
   - (f) Les tables de lignes résumées (point 1.2) ont donné les mêmes empreintes sous Windows, d'après le verdict OK du poste. Leur indépendance à la plateforme est constatée entre deux systèmes.

**Effet sur les résultats** : aucun. `ddf77bd` ne touche ni `R/` ni les références. `1019d8e` crée les références sans changer aucune valeur existante (mode `creation`, visa d'`actuary` du 18/10/2026, message de `1019d8e`).

Issues : #17 ; #15, #24 (maillons de l'ancrage). ADR 0008 (réserve sur la `libm`, maintenue). PR #30 : les mesures sous Windows ont été faites avec le correctif du harnais de cette PR (messages de `ddf77bd` et `1019d8e`).

**Complément du 18/10/2026 (revue finale complète de la PR #18, règle 10).** Les points (e) et (f) s'ajoutent à la présente annotation après sa rédaction ; les points (a) à (d) ne sont pas réécrits.

**(e) Point 3.3 : 0 et −0 sont confondus, ce qui est voulu.** L'écriture canonique des doubles, `sprintf("%.17g", x + 0)` (`R/rapport.R:109`, `colonne_canonique()`, employée par `octets_valeurs()` de l'outillage), écrit −0 comme 0 : en arrondi au plus proche, −0 + 0 vaut +0. La comparaison les confond aussi : `identical(0, -0)` vaut `TRUE`, et `identiques_elements()` compare par `==`.

- Mesure d'`audit` (revue finale) : `taux[1]` passé de 0 à −0 donne « identique(s) au bit près », verdict OK.
- Mesure d'`architect` (18/10/2026, R 4.3.3 ucrt, poste Windows, hors plateforme de référence) : `sprintf("%.17g", -0)` donne `-0` et `sprintf("%.17g", -0 + 0)` donne `0` ; `identical(0, -0)` vaut `TRUE` et `identical(0, -0, num.eq = FALSE)` vaut `FALSE`. `comparer_references()`, appliquée à deux jeux de grandeurs qui ne diffèrent que par le signe d'un zéro de `etapes/brut/resultat/poisson/taux`, rend `ok = TRUE` et le verdict « identique », alors que `1 / x` vaut `-Inf` du côté obtenu.

±0 confondus par la représentation canonique, voulu. Motif : aucune grandeur actuelle du moteur (taux, borne, exposition, probabilité) ne porte de sens par le signe d'un zéro, et la même écriture fait l'empreinte (ADR 0007, point 9.3). Limite admise : la non-régression ne détecte pas un passage de 0 à −0. Une grandeur future dont le signe du zéro aurait un sens appellerait un changement de format (point 9.2).

**(f) Point 3.4 : clés inertes de la classe exacte retirées de `TOLERANCES`.** La seconde annotation, (b), et la quatrième, (b), se contredisaient pour les grandeurs `integer`. La contradiction est résolue en faveur de la quatrième, sur avis d'`actuary` (réponse à la question Q1 de la revue finale, 18/10/2026).

- **La contradiction.** La seconde annotation, (b), range les dénombrements de Kaplan-Meier dans la classe exacte par des clés qualifiées de `TOLERANCES`, tout en notant qu'ils sont `integer`, donc exacts par leur type. La quatrième, (b), pose la règle inverse pour `avertissements/nombre` : la tolérance n'est cherchée que pour un `double`, une clé sur un `integer` ne sert donc à rien, et elle masquerait à `doubles_sans_tolerance()` (règle de complétude du point 3.4) un retour au `double`.
- **La résolution.** La règle de la quatrième annotation, (b), vaut pour toute clé dont la grandeur est `integer`. Douze clés de la classe exacte sont retirées : `kaplan_meier_sauts/numerateur`, `kaplan_meier_sauts/ensemble_a_risque`, `kaplan_meier/instants_evenement`, `parametres/denominateur_instants`, `hypotheses_brut/denominateur_instants`, les cinq `diagnostics/kaplan_meier/*` (`instants`, `evenements`, `evenements_a_l_instant_d_entree`, `cellules_survie_nulle`, `ensemble_a_risque_maximal`), `version_fiche` et `graine`. Le point 3.4 range ces deux dernières parmi les doubles ; elles sont `integer` dans les six références.
- **Types vérifiés par `architect`** (18/10/2026, deuxième colonne des fichiers commités) : les douze chemins sont `integer` dans `tests/reference/R6.tsv` ; `fiche/version_fiche` et `fiche/graine` le sont dans `R1.tsv` à `R5.tsv`.
- **Témoin d'`actuary`** sur R6 : `kaplan_meier_sauts/numerateur` remis en `double` n'est relevé par `doubles_sans_tolerance()` que sans la clé.
- **Ce qui reste.** La clé nue `cellules` est conservée : elle classe `etapes/exposition/diagnostics/cellules`, de type `double` dans `R1.tsv` à `R6.tsv` (vérifié). Son passage en `integer`, sur le modèle de #24, est proposé en issue ; la clé se retirera alors, avec une annotation.
- **Mise en œuvre.** Retrait par `coder`, commit `tests:` `ff6279d` (#17). Le présent point décrit l'état cible.

**Effet sur les résultats de (e) et (f)** : aucun. (e) constate une propriété de l'écriture et de la comparaison, sans changement de code. (f) ne touche que `TOLERANCES`, dans le code de test : les références ne portent aucune tolérance, aucun fichier de `tests/reference/` ne change, et une grandeur `integer` reste comparée exactement par son type.

Issues : #17 ; #24 (règle de la quatrième annotation, (b)).

## Annotation du 19 octobre 2026 — constat 1 de l'annotation du 18/10/2026, (d), dépassé : réserve sur la `libm` exercée sur trois modèles de processeur

Rédigée par `architect` au point d'étape qui suit la fusion de la PR #18 (`2c8cce4`), d'après les journaux des jobs « Tests (R 4.3.3) » relus le 19/10/2026. Décision du mainteneur du 19/10/2026 (M58, Q7). Le texte ci-dessus, annotations comprises, n'est pas réécrit.

Le constat 1 de l'annotation du 18/10/2026, (d), écrit que les deux exécutions de la plateforme de référence alors disponibles (39140635363 et 39148101369) ont tourné sur un AMD EPYC 7763, et que la réserve de l'ADR 0008 sur la `libm` n'est **pas exercée**. C'était exact à sa date. Les exécutions postérieures de la CI ont comparé R1 à R6 sur d'autres processeurs : AMD EPYC 9V74 (39156288666, 39162270569) et AMD EPYC 9V45 (39161760255), tous deux avec `avx512f`, puis de nouveau un EPYC 7763 (39164433640, `main` à `2c8cce4`). Chaque fois, les **1 420 grandeurs** sont identiques au bit près (tableau de l'annotation du 19/10/2026 de l'ADR 0008).

Conséquences :

1. L'hypothèse de bruit nul sur la plateforme de référence (« Conséquences », (b)) tient désormais sur trois modèles de processeur, au statut de **constat**. La réserve sur la `libm` reste **ouverte pour un exécuteur sans FMA** (ADR 0008, annotation du 19/10/2026).
2. Les tolérances ne changent pas. La classe 1e-12 (fonctions de la `libm`) garde sa justification, puisque le cas sans FMA et les postes hors plateforme de référence (Windows : 1,84e-15 au plus, annotation du 18/10/2026, (d)) restent possibles.
3. La mention « variante selon le processeur : hypothèse non mesurée » du tableau des classes (point 3) se lit désormais : mesurée sans écart sur trois modèles munis de FMA, non mesurée sans FMA.

**Effet sur les résultats** : aucun.

Issues : #17 ; ADR 0008 (réserve sur la `libm`).

## Annotation du 19 octobre 2026 (seconde) — #37 : `cellules` en integer, clé nue retirée, première régénération ; mécanisme transitoire `A_REGENERER_<n>` érigé en règle (point 9.5) ; `TOLERANCE_PERMUTATION` (#39) ; garde « CI seule » du producteur (#36)

Rédigée par `architect` sur la branche `claude/consolidation-controles-references` (tête `763da20`), d'après les messages des commits cités, relus par `git show`, les en-têtes des six `.tsv` commités et le code de `tests/`. C'est l'annotation datée que le point 9.3 exige pour le retrait de la clé `cellules`, comme l'annonçait le complément du 18/10/2026, (f). Le texte ci-dessus, annotations comprises, n'est pas réécrit.

**(a) #37 : `etapes/exposition/diagnostics/cellules` en integer, retrait de la clé nue `cellules`, régénération de R1 à R6.**

1. **Type.** Depuis `5bf62e6`, `nombre_cellules()` (`R/exposition.R`) rend le nombre de cellules en `integer`. Elle refuse, sous le motif `trop_de_cellules`, un nombre supérieur à `.Machine$integer.max`. Les valeurs ne changent pas : 149, 75, 149, 1490, 149 et 149 pour R1 à R6 (tableau avant / après du message de `5bf62e6`). Le visa d'`actuary` est du 19/10/2026, avec une réserve : la régénération ne doit porter que le type.
2. **Clé.** `99272a4` retire la clé nue `cellules` de `TOLERANCES` (`tests/reference/outils_reference.R`), avec l'avis favorable d'`actuary` au titre du point 9.3. La règle appliquée est celle de la quatrième annotation du 17/10/2026, (b). La clé classait aussi `etapes/brut/diagnostics/cellules` et `etapes/brut/diagnostics/etats/cellules`, déjà `integer`, et aurait masqué leur retour au double. Trois témoins, un par chemin, font échouer `doubles_sans_tolerance()` sur un retour au double (message de `99272a4`). *Vérifié le 19/10/2026* : `TOLERANCES` ne contient plus aucune clé `cellules`.
3. **Régénération sur la CI.** Mode `regeneration`, plans `tous`, exécution 39248539956 de `references.yml`. La CI a calculé sur `99272a4` ; la session principale a commité les références en `763da20`. Les en-têtes portent `commit : 99272a4…`, `mode : regeneration` et `execution : 39248539956` (*vérifié*). La plateforme est celle de l'ADR 0008, sur un AMD EPYC 9V45 (`fma`, `avx2`, `avx512f`), consigné dans l'en-tête des six fichiers. La réserve du visa est vérifiée (message de `763da20`) :
   - par plan, `ecarts.txt` relève **un seul écart structurel**, le type de `etapes/exposition/diagnostics/cellules` ;
   - toutes les autres grandeurs sont identiques au bit près (233, 230, 233, 219, 233 et 266, soit 1 414 ; avec les six chemins changés de type, on retrouve les 1 420 grandeurs) ;
   - aucune grandeur n'est sous tolérance.
4. **Diff des `.tsv`** (*vérifié* : `git diff 99272a4 763da20 -- tests/reference/`). Par fichier, le corps ne change que d'une ligne, celle du type (`double` → `integer`, valeur inchangée). L'en-tête change sur huit lignes : processeur et drapeaux, `GITHUB_SHA`, commit, exécution, adresse, mode et date (*vérifié* : `git diff -U0 99272a4 763da20 -- tests/reference/Rk.tsv | grep '^+#' | wc -l` donne 8 pour chacun des six fichiers). L'identifiant de plateforme, image, R, BLAS et LAPACK, ne change pas.
5. **Première régénération du projet.** Avant `763da20`, les fichiers de `tests/reference/` n'avaient été écrits qu'une fois, en mode `creation` (`1019d8e` ; *vérifié* par `git log -- tests/reference/R1.tsv`). La chaîne de régénération a donc été exercée pour la première fois sur un changement minimal, un type et aucune valeur, comme le prévoyait l'arbitrage (C) de la feuille de route (§ 1.3) : mode `regeneration`, tableau avant / après, visa, commit par la session principale.
6. **Texte périmé.** Le complément du 18/10/2026, (f), « Ce qui reste », écrit : « La clé nue `cellules` est conservée : elle classe `etapes/exposition/diagnostics/cellules`, de type `double` dans `R1.tsv` à `R6.tsv` ». Ce passage est **périmé**, comme la quatrième annotation, (d), l'avait été pour #24. Il reste vrai pour l'état antérieur à #37, jusqu'à `9f957ba`.

**Effet sur les résultats de (a)** : le type d'une grandeur par plan passe de `double` à `integer`, soit six lignes. **Aucune valeur ne change.** Visa d'`actuary` du 19/10/2026. Le tableau avant / après figure dans `5bf62e6` et dans `763da20`.

**(b) Mécanisme transitoire `A_REGENERER_37`, et règle durable pour les régénérations suivantes (point 9.5, nouveau).**

Entre `5bf62e6` et `763da20`, le code rendait le type `integer`, alors que les références commitées portaient encore `double`. `tests/test_reproductibilite.R` a tenu cette transition par une liste `A_REGENERER_37` des plans à régénérer et par le chemin `CHEMIN_TYPE_37`. Pour chaque plan listé :

- un **test ordinaire « seul écart »** aligne le type de la référence sur celui de l'objet. Il vérifie d'abord que la conversion est exacte (`as.double(as.integer(v))` identique à `v`), puis exige une comparaison sans aucun écart. Tout autre écart de grandeur ou de structure, et toute entrée différente, le font échouer ;
- la **comparaison stricte** au fichier commité est un **échec attendu `#37`**. Il en va de même du test des doubles sans tolérance, qui admet pour seul reste le chemin de #37.

Après la régénération, l'échec attendu devient un succès inattendu, ce qui fait échouer la batterie. Le commit des références vide donc la liste (`A_REGENERER_37 <- character(0)`, `763da20`). Les tests « seul écart » disparaissent alors, et les comparaisons strictes redeviennent des tests ordinaires. La batterie de reproductibilité compte ensuite 143 tests en C.UTF-8, avec 0 échec, 0 échec attendu et 0 succès inattendu (message de `763da20`).

**Avis d'`architect` : oui, c'est une règle.** Le mécanisme est sûr dans les deux sens :

- une dérive autre que celle qui est annoncée fait échouer le test ordinaire ;
- une liste oubliée après la régénération fait échouer la batterie par succès inattendu.

Il comble l'intervalle entre le commit qui change un résultat (un commit par issue, `CLAUDE.md`) et le commit des références, intervalle où la batterie était sinon soit rouge, soit aveugle. L'avis d'`audit` sur sa sûreté (scénarios M7 et M8) est rapporté par la session principale ; il n'est retrouvé ni dans le dépôt ni sur la PR #40 (*non vérifié par `architect`*). Le constat de six scénarios dans le message de `5bf62e6` et le comportement du code décrit ci-dessus, relu, vont dans le même sens.

> **9.5. Transition d'un changement de résultat annoncé.** Quand un commit change un résultat avant la régénération des références, il déclare, dans `tests/test_reproductibilite.R`, la liste `A_REGENERER_<n>` des plans concernés, où `<n>` est l'issue, ainsi que le ou les chemins admis à changer et la nature du changement admis. Pour chaque plan listé :
> (i) un test ordinaire compare la référence à l'objet, après n'avoir corrigé que le changement annoncé, et exige l'absence de tout autre écart ;
> (ii) la comparaison stricte, et tout contrôle des références que le changement fait échouer, sont des échecs attendus `#<n>`.
> Le commit des références de la session principale vide la liste. Un plan resté listé après la régénération fait échouer la batterie par succès inattendu.
> Le changement admis par (i) doit être décrit **exactement** : type converti sans perte, ou valeurs nouvelles nommées chemin par chemin. Une tolérance élargie, ou un chemin admis « quel que soit l'écart », n'est pas une transition : c'est un changement de résultat déguisé (point 9.3).
> La règle ne remplace ni le tableau avant / après, ni le visa, ni le point 9.4.
>
> *Règle retenue par le mainteneur le 19/10/2026 (M60, `docs/feuille-de-route.md` § 4).*

**Effet sur les résultats de (b)** : aucun. La règle n'organise que les tests.

**(c) `TOLERANCE_PERMUTATION` (#39, `9f957ba`).** Le test de permutation des lignes de l'entrée de référence (`tests/test_reproductibilite.R`) compare à tolérance les doubles issus de sommes par cellule. La constante qu'il emploie vaut `CLASSES_TOLERANCE[["1e-12"]]` (*vérifié*, l. 402). Ce n'est **pas une modification de `TOLERANCES`**, au sens du point 9.3 : aucune clé n'est ajoutée ni retirée, et aucune référence ne porte de tolérance. C'est l'emploi d'une **classe existante hors des références**, consigné ici à la recommandation d'`audit`, par traçabilité.

- **Statut** : tolérance déclarée, appuyée sur un constat de mesure, et non sur une borne démontrée. Les mesures sont les suivantes :
  - à 10⁵ lignes (entrée de référence), l'écart relatif maximal vaut 1,6e-14 (R2, `borne_sup_exacte`), et taux et exposition centrale restent à 5,73e-15 au plus (message de `9f957ba`) ;
  - à 10⁶ et 3·10⁶ lignes, il vaut 1,37e-14 et 1,73e-14 (plan R1, une permutation ; avis d'`actuary` sur #39, commentaire de l'issue).
- **Pourquoi pas la classe 1e-14.** L'écart croît avec le nombre de termes sommés par cellule (`somme_par_cellule()`, `R/exposition.R`). La classe 1e-14 de `TOLERANCES` (exposition, taux) vise l'arithmétique IEEE de base pour un **même** ordre des opérations, sur la plateforme de référence. Elle ne s'applique pas sous permutation : l'exposition et les taux la dépassent dès 10⁶ lignes.
- **Ce que le test fixe par ailleurs.** Tout double non classé fait échouer le test. Les doubles de classe exacte, les tables de Kaplan-Meier et les diagnostics du brut sont comparés par `identical()`.
- **Règle.** Un emploi d'une classe de `CLASSES_TOLERANCE` hors des références n'appelle pas d'annotation au titre du point 9.3. Il se cite, avec son statut et sa mesure, dans le test qui l'emploie. Une valeur hors des trois classes demanderait l'avis d'`actuary` et une annotation.

**Effet sur les résultats de (c)** : aucun. Aucune référence ni aucun résultat ne change (message de `9f957ba`).

**(d) Garde du producteur : la règle « CI seule » passe des consignes au script (#36, `4f182e3`).** Les points 2.6, 5.2, 5.5 et 9.4 réservent l'écriture des références à la CI, de même que `CLAUDE.md` (« Changements de résultats et reproductibilité ») et l'annotation du 18/10/2026, (a), conséquence 1. Jusqu'à `4f182e3`, la règle n'était portée que par les consignes. Depuis ce commit, `principal()` (`tests/reference/produire_references.R`) refuse par `echouer()` les modes `creation`, `regeneration` et `bascule` quand `GITHUB_ACTIONS` ne vaut pas `"true"` (*vérifié* : `git show 4f182e3`). Le refus intervient avant le contrôle de `--motif`, `--plans` et `--sortie`, et avant toute écriture. Le message renvoie au présent ADR et à `CLAUDE.md`. Cette garde est symétrique de celle du mode `releve`, refusé sur la CI. Sept témoins l'exercent en sous-processus : six refus hors CI, avec code 1, message, dossier de sortie non créé et `tests/reference/` intact, et un passage en CI pour `regeneration`. `references.yml` est inchangé.

**Limite** : la garde protège contre l'**erreur**, non contre la malveillance. La variable se pose à la main. Le point 5.5 reste donc la garantie de fond : appariement MD5 ↔ artefact cité, vérifié en revue finale.

**Effet sur les résultats de (d)** : aucun. R1 à R6 donnent le verdict OK (message de `4f182e3`).

Issues : #37, #39, #36 ; #24 (règle de la quatrième annotation, (b)). Branche `claude/consolidation-controles-references`, PR #40.

## Annotation du 19 octobre 2026 (troisième) — #47 : règle 9.5 étendue (liste unique `TRANSITIONS_EN_COURS`, cinq natures, exactitude dans les deux sens, union, rattachement de l'échec attendu, restes) ; garde « entrée différente » (point 3.2) sous transition

Rédigée par `architect-approfondi` sur la branche `claude/fiche-serialisation` (B4, PR #51), d'après le diff de `coder` sur #47 (`tests/test_reproductibilite.R`, non commité à cette date), le rapport d'`audit` (conforme avec réserves, trois constats mineurs, rapporté par la session principale) et des mesures d'`architect` citées en (f). Elle décrit l'état du code **après la reprise unique de `coder`** que demande le même avis ; le commit `tests:` de #47 doit y être conforme. Le texte ci-dessus, annotations comprises, n'est pas réécrit. Le point 9.5 (M60) reste la règle ; la présente annotation fixe sa mise en œuvre générique et précise deux de ses phrases.

**(a) Liste unique.** Le mécanisme propre à #37 (`A_REGENERER_37`, `CHEMIN_TYPE_37`, deux branches dédiées) est retiré. Les transitions en cours sont déclarées dans une **liste unique** `TRANSITIONS_EN_COURS` de `tests/test_reproductibilite.R`, vide hors transition. Chaque élément porte trois champs, et eux seuls : `issue` (`"#N"`, une transition par issue), `plans` (identifiants de `plans_reference()`) et `changements` (au moins un). Un test ordinaire, `valider_transitions()`, refuse une liste mal formée. Dans le point 9.5, « la liste `A_REGENERER_<n>` des plans concernés » se lit désormais : « la transition de l'issue `<n>` dans `TRANSITIONS_EN_COURS` ».

**(b) Cinq natures, liste fermée.** Un changement nomme un chemin et une nature. Chaque nature se déclare exactement et doit être **observée** sur la référence commitée, telle qu'elle est lue, avant tout alignement :

| Nature | Champs déclarés | Admis | Condition d'observation |
|---|---|---|---|
| `type` | `de`, `vers` | `double` → `integer` ou `integer` → `double`, conversion sans perte | type de la référence égal à `de` |
| `valeur` | `valeurs` (numériques), `positions` facultatif | sans `positions` : le vecteur déclaré remplace tout le vecteur de la référence, de longueur nouvelle quelconque mais au moins 1 ; avec `positions` : même longueur, positions dans le vecteur | référence `double` ou `integer` ; vecteur différent (sans `positions`) ou chaque position déclarée différente |
| `valeur de chaîne` | `valeurs` (`character`), `positions` facultatif | comme `valeur` | référence `character` ; comme `valeur` |
| `ajout` | `type`, `longueur` | chemin présent dans l'objet seul, de ce type et de cette longueur | chemin absent de la référence |
| `retrait` | `type`, `longueur` | chemin présent dans la référence seule, de ce type et de cette longueur | chemin présent dans la référence |

1. **Pas de nature « longueur ».** Un changement de longueur se déclare par `valeur` ou `valeur de chaîne` **sans `positions`**, avec le vecteur nouveau entier. C'est le cas de `fiche/@noms` si la question Q2 de l'ADR 0011 est retenue (25 → 21 noms). Une longueur déclarée sans ses valeurs admettrait des valeurs « quelles qu'elles soient » : c'est ce que le point 9.5 exclut. **La longueur nouvelle est au moins 1** : `valider_transitions()` exige des valeurs non vides (`tests/test_reproductibilite.R:411`) et l'union refuse un `ajout` et un `retrait` sur un même chemin. Le passage d'un vecteur à la longueur 0 n'est donc pas déclarable, et il est exclu jusqu'à décision contraire. Aucun cas n'est prévu dans B4 : les huit retraits de #42 sont des retraits de chemins, non des vecteurs vidés (*vérifié* : les huit chemins figurent dans `R1.tsv` à `R6.tsv`).
2. **Précision du point 9.5, « valeurs nouvelles nommées chemin par chemin ».** Pour `valeur` et `valeur de chaîne`, les valeurs sont déclarées. Pour `ajout` et `retrait`, la structure est déclarée (type et longueur). Les valeurs d'un chemin ajouté n'ont pas d'« avant » à comparer : elles entrent dans le tableau avant / après de la régénération, sous visa. Les valeurs d'un chemin retiré y figurent aussi, lues dans la référence.

**(c) Exactitude dans les deux sens.** Le test « seul écart » (point 9.5 (i)) vérifie que tout écart observé est déclaré. La condition d'observation de (b) vérifie la réciproque : **tout changement déclaré est observé**. Un changement déclaré mais non observé fait échouer le test « seul écart » (« changement déclaré non observé »). Trois cas en relèvent : un chemin absent de la référence et de l'objet (constat 1 d'`audit`, `tests/test_reproductibilite.R:417` du diff), une valeur déclarée égale à celle de la référence, et un type déjà converti. Conséquence pour un plan resté listé après la régénération : le test « seul écart » échoue et la comparaison stricte est un succès inattendu. La batterie échoue donc, comme l'exige le point 9.5. Le code soumis ne relevait pas ces cas : il traitait un changement déjà appliqué comme un succès. La reprise corrige ce point.

**(d) Union et rattachement de l'échec attendu.**

1. Les changements de toutes les transitions qui portent sur un plan **s'unissent**. Ils s'appliquent dans l'ordre `type`, `valeur`, `valeur de chaîne`, `ajout`, `retrait`, et l'observation se juge sur la référence commitée, après la seule conversion de type sans perte, non sur l'état intermédiaire des autres changements (commentaire de `aligner_reference()`, `tests/test_reproductibilite.R`, `7ada1f1`). Une conversion sans perte ne change aucune valeur, si bien que juger avant ou après elle donne le même résultat (mesures X5 et X6 d'`audit`, rapportées par la session principale). Deux déclarations identiques s'appliquent une fois. Des déclarations contradictoires sont une erreur : deux valeurs différentes à une même position ; un remplacement entier et une autre déclaration de valeurs sur le même chemin ; deux conversions de type différentes ; un `ajout` ou un `retrait` combiné à une autre nature sur le même chemin.
2. **Un test « seul écart » par plan**, qui cite toutes les issues du plan.
3. **Un échec attendu par plan** pour la comparaison stricte, rattaché à **la première transition de la liste qui porte le plan**, et non un échec attendu par issue. Trois raisons :
   - une comparaison est un seul test, et la compter une fois par issue fausserait le bilan ;
   - la régénération porte sur un plan entier, donc sur toutes ses transitions à la fois, et ne lève jamais une issue seule ;
   - le harnais attend une seule issue par échec attendu (`MOTIF_ISSUE`, `tests/harnais.R:87`), contrat partagé avec la batterie de concordance.

**(e) Restes de doubles sans tolérance (point 9.5 (ii)).** Pour un plan listé, un double de la référence commitée qu'aucune clé ne classe n'est admis que dans deux cas : son type est converti depuis `double`, ou il est retiré avec le type déclaré `double`. Le test strict des restes n'est un échec attendu **que si des restes existent** dans les références commitées ; sinon il reste ordinaire. Cet écart au code de #37 est voulu. Une transition sans reste en ferait sinon un succès inattendu, et c'est le cas de B4 : `delai_declaration` est classé exact (`tests/reference/outils_reference.R:76`). Une liste oubliée après la régénération reste relevée par (c).

**(f) Garde « entrée différente » (point 3.2) sous transition.**

1. **Constat** (mesure d'`architect`, *vérifiée*, script hors dépôt sur le diff de `coder`, environnement cloud). On permute les lignes de l'entrée de référence (`set.seed(1)`, `sample.int`) et on construit R1. L'objet diffère de `tests/reference/R1.tsv` sur 23 chemins. Trois sont des chaînes : `fiche/empreintes/donnees` et les deux `@empreinte` des tables résumées. Les vingt autres sont des doubles sous tolérance. On déclare ces trois chemins en `valeur de chaîne`, avec les valeurs de l'objet : `seul_ecart()` rend `TRUE`. Une entrée **réellement différente** passe donc le test « seul écart » : sans garde, la transition absorbe le point 3.2.
2. **Règle.** Une transition sur `fiche/empreintes/donnees` est admise à trois conditions :
   - (i) sa nature est `valeur de chaîne`, sans `positions` ;
   - (ii) l'union du plan déclare aussi un changement de `fiche/empreintes/representation`, car seule une nouvelle définition de la représentation canonique change l'empreinte d'une entrée inchangée (ADR 0011, point 10) ;
   - (iii) un **test ordinaire de garde**, distinct du test « seul écart », prouve que l'entrée est inchangée. Il calcule le texte canonique de l'entrée de référence par le package, remplace l'en-tête de la première ligne (texte avant la première tabulation, égal à l'en-tête courant) par la valeur `fiche/empreintes/representation` de la référence, et exige que le MD5 obtenu soit égal à `fiche/empreintes/donnees` de la référence. Il exige aussi que les deux représentations diffèrent.
3. **Faisabilité** (mesure d'`architect`, *vérifiée*). La première ligne du texte actuel est `tablexp canonique 1<TAB>100000<TAB>7`. Son MD5 redonne `f3519cc1b73ab0d1fcc6ad89dc6d595c`. Le même texte avec l'en-tête `tablexp canonique 0` donne `fe300026706602afcd22e97ecb08c5fd` : l'en-tête entre bien dans l'empreinte.
4. **Si la preuve échoue**, aucune transition n'est possible. Le changement est un **changement de l'entrée de référence** au sens du point 4.2 : régénération de tous les plans, tableau des grandeurs agrégées, visa. C'est notamment le cas d'une définition nouvelle qui changerait d'autres octets du texte de l'entrée que la première ligne.
   **Réserve** (M2 d'`audit`) : `entree_inchangee()` ne vérifie pas que l'objet a été construit sur l'entrée qu'on lui passe. C'est sans effet dans la batterie, qui construit chaque plan sur `jeu_reference()` et passe ce même jeu à la garde. Le contrôle est à compléter avec #35, premier usage réel : le MD5 du texte de l'entrée, sous la représentation courante, doit être égal à l'empreinte des données de l'objet.
5. **Trois causes au lieu de deux.** Le point 3.2 nomme deux causes d'une entrée différente. Il en compte désormais une troisième : la définition de la représentation canonique a changé. `imprimer_comparaison()` la nomme quand `fiche/empreintes/representation` diffère entre la référence et l'objet, avec les deux valeurs, et précise qu'une entrée inchangée se prouve par la garde.
6. **Accord avec l'ADR 0011** (point 2.5, statut proposé). La règle réalise ce que cet ADR demande : la garde de la batterie « honore » la transition de #35, déclarée en `valeur de chaîne` sur les deux chemins pour les six plans, sans s'y dissoudre. La preuve « hors R » qu'il exige dans le commit de #35 reste due : elle ne dépend pas de `tools::md5sum`. La garde de la batterie en est la forme mécanique et permanente pendant la transition (`CLAUDE.md`, « Workflows », principe 7).
7. **Lignes `@empreinte` des tables résumées** (`RESUMES_PAR_EMPREINTE`). Ce sont des résultats de l'étape `donnees`, non l'entrée : elles relèvent de la `valeur de chaîne` ordinaire. Le constat de (f) 1 montre que c'est la garde, et non elles, qui distingue une entrée permutée.

**(g) Transitions attendues de B4**, à titre d'illustration et sous réserve de la décision du mainteneur sur l'ADR 0011 (Q2 (a)). Elles sont déclarées dans les commits de #35 et #42 :

- **#35, plans R1 à R6** : `fiche/empreintes/representation` en `valeur de chaîne` (`tablexp canonique 2`) et `fiche/empreintes/donnees` en `valeur de chaîne`, sous la garde de (f) ;
- **#42, plans R1 à R6** :
  - `fiche/version_fiche` en `valeur` (2) ;
  - `fiche/@noms` en `valeur de chaîne`, sans `positions` (21 noms) ;
  - huit `retrait`, chacun avec son type et sa longueur : `fiche/transition/@noms`, `…/depart`, `…/arrivee`, `fiche/periode/@noms`, `…/debut`, `…/fin`, `fiche/delai_declaration` et `fiche/segmentation`.

Toute autre ligne du tableau avant / après de la régénération unique est une régression, conformément à l'ADR 0011 (« Effet sur les résultats »).

> **Lecture du point 9.5 après la présente annotation.** Quand un commit change un résultat avant la régénération des références, il déclare sa transition dans `TRANSITIONS_EN_COURS` (`tests/test_reproductibilite.R`). Une transition porte l'issue, les plans et les changements admis. Chaque changement nomme un chemin et l'une des cinq natures `type`, `valeur`, `valeur de chaîne`, `ajout`, `retrait`, déclarée exactement. Pour chaque plan listé, les changements de toutes ses transitions s'unissent, puis :
> (i) un test ordinaire exige que tout écart soit déclaré et que tout changement déclaré soit observé sur la référence commitée ;
> (ii) la comparaison stricte est un échec attendu, rattaché à la première transition de la liste qui porte le plan ; le test des doubles sans tolérance l'est aussi, si la référence commitée en porte ;
> (iii) un changement de `fiche/empreintes/donnees` n'est admis qu'avec un changement de `fiche/empreintes/representation` et la preuve, par un test ordinaire, que l'entrée est inchangée ; sinon, c'est un changement de l'entrée de référence (point 4.2).
> Le commit des références de la session principale retire la transition. Un plan resté listé fait échouer la batterie.

**Effet sur les résultats** : aucun. La règle n'organise que les tests. Le point (f) 5 change une ligne de l'impression de la comparaison, non une grandeur.

**Soumis au mainteneur** : (f) étend le point 3.2, qui est un contrat partagé (critère de non-régression). Cette extension lui est soumise avec l'ADR 0011, dont elle réalise le point 2.5.

Issues : #47 ; #35 et #42 (premiers emplois) ; #37 (mécanisme d'origine). ADR 0011 (proposé). Branche `claude/fiche-serialisation`, PR #51.

## Annotation du 19 octobre 2026 (quatrième) — extension du point 3.2 actée par le mainteneur (décision M64) ; plusieurs transitions d'une même issue à plans disjoints

Rédigée par `architect` d'après la décision du mainteneur du 19/10/2026 (M64, `docs/feuille-de-route.md`, § 4), rapportée par la session principale de la branche `claude/fiche-serialisation` (B4, PR #51). Le texte ci-dessus, annotations comprises, n'est pas réécrit.

**(a) Point 3.2 étendu : acté.** L'extension de la garde « entrée différente » sous transition, soumise par la troisième annotation du 19/10/2026 (point (f) ; mise en œuvre dans `tests/test_reproductibilite.R` au commit `7ada1f1`, annotation au commit `4f59467`), est **actée par le mainteneur** avec l'acceptation de l'ADR 0011 (M64). La mention « Soumis au mainteneur » de la troisième annotation se lit désormais « acté (M64) ». La **réserve M2** d'`audit` reste ouverte : la garde est à compléter **avec #35**, premier usage réel (le MD5 du texte de l'entrée, sous la représentation courante, doit être égal à `fiche/empreintes/donnees` de l'objet), dans le commit de #35.

**(b) Plusieurs transitions d'une même issue, à plans disjoints.** La troisième annotation, (a), admet « une transition par issue » ; `valider_transitions()` refuse aujourd'hui une issue en double, même sur des plans disjoints (`tests/test_reproductibilite.R:382`, témoin « issue en double » `:1615`, *vérifié* par lecture à `4f59467`). Or le retrait de `fiche/segmentation` par #42 n'a pas la même structure sur tous les plans : `character` de longueur 0 dans R2, de longueur 1 dans R1, R3, R4, R5 et R6 (*vérifié* : `grep -n 'fiche/segmentation' tests/reference/R*.tsv`) ; une nature `retrait` déclare un type et une longueur exacts ((b) de la troisième annotation), si bien qu'une seule transition sur R1 à R6 ne peut pas le déclarer. Décision : une issue **peut porter plusieurs transitions, à condition que leurs plans soient deux à deux disjoints**. Conséquences :

1. `valider_transitions()` refuse deux transitions d'une même issue qui partagent un plan (motif nommé), et non plus toute issue en double ; témoins : deux transitions d'une même issue à plans disjoints → liste bien formée ; deux transitions d'une même issue à plans communs → refus. Le témoin actuel « issue en double » (`#901` sur R1 puis sur R2, plans disjoints) deviendrait une liste bien formée : il est à réécrire sur des plans communs.
2. L'union par plan ((d) 1 de la troisième annotation) et le rattachement de l'échec attendu à la première transition qui porte le plan ((d) 3) sont inchangés : un plan n'est porté que par une transition de chaque issue, et l'issue citée est la même.
3. Dans (a) de la troisième annotation, « une transition par issue » se lit « une transition par issue et par plan ».

La modification et ses témoins entrent dans le commit de #42 (`coder-approfondi` → `audit`), qui en est le premier usage. Le point 9.5 reste la règle.

**Effet sur les résultats** : aucun. La décision n'organise que les tests.

Issues : #42, #35, #47. ADR 0011 (accepté, M64). Décision M64.

## Annotation du 20 octobre 2026 — garde « entrée différente » (point 3.2) : limite de `entree_inchangee()` quand une nouvelle représentation change le corps du texte de l'entrée

Rédigée par `architect` d'après un constat de `/code-review` lors de la revue finale de B4 (règle 10), branche `claude/fiche-serialisation`, PR #51, tête `da565c7`. Le texte ci-dessus, annotations comprises, n'est pas réécrit. **Rien n'est implémenté** : l'annotation consigne une limite et ce qu'il faudra faire le jour où elle sera atteinte.

**(a) Constat.** `entree_inchangee()` (`tests/test_reproductibilite.R:558-586`) reconstruit le texte canonique de l'entrée de référence sous l'ancienne représentation de la façon suivante : elle prend le texte courant, produit par le package, et remplace **seulement** l'en-tête de sa première ligne (les octets avant la première tabulation) par la valeur `fiche/empreintes/representation` de la référence. Le reste du texte est gardé tel quel, y compris la fin de la première ligne (nombre de lignes, nombre de colonnes). La garde **suppose** donc qu'une nouvelle représentation ne change rien d'autre que cet en-tête pour l'entrée de référence.

**(b) Pourquoi c'était vrai pour B4.**
- **#35** change l'écriture des doubles. Or les colonnes à rôle de l'entrée de référence sont `integer` (`id_contrat`, `id_tete`), `character` (`sexe`, `cause_sortie`) et `Date` (`date_naissance`, `date_entree`, `date_sortie`, de stockage double mais écrites en dates ISO, non en doubles). Aucune colonne n'est un double nu (*vérifié* à `da565c7`, `typeof` des colonnes de `jeu_reference()`).
- **#19** ne change l'écriture des dates qu'hors de [1000, 9999], alors que les dates de l'entrée de référence vont de 1930 à 2024.

Mesure à `da565c7` (*vérifiée*) : première ligne `tablexp canonique 2<TAB>100000<TAB>7`, MD5 `35ca55371e72f0f0103993d70e3a64d3`, égal à `fiche/empreintes/donnees` de `tests/reference/R1.tsv`.

**(c) Nature de la limite.**
1. **Déjà prévue en principe.** La troisième annotation du 19/10/2026, (f) 4, range dans les échecs de la preuve « une définition nouvelle qui changerait d'autres octets du texte de l'entrée que la première ligne ». Elle renvoie alors au changement de l'entrée de référence du point 4.2.
2. **L'erreur est conservatrice** (*raisonnement*). Un futur changement de représentation qui modifie l'écriture d'un type présent dans l'entrée (`integer`, `character`, `Date`) ou la forme de la première ligne ferait échouer la garde **à tort** : elle conclurait que l'entrée a changé (message « l'entrée a changé », `tests/test_reproductibilite.R:585`). La garde ne peut pas, à l'inverse, conclure à tort que l'entrée est inchangée, sauf collision MD5, hypothèse non retenue.
3. **Le diagnostic serait inexact.** Dans ce cas, l'entrée n'a pas changé : seul son texte a changé. Suivre la voie du point 4.2 (régénération de tous les plans, tableau des grandeurs agrégées, mention « l'entrée a changé » dans le commit) consignerait une cause fausse et ferait perdre le tableau avant / après grandeur par grandeur de la fiche.

**(d) Ce qu'il faudra faire à ce moment-là.** La branche qui modifie l'écriture d'un type présent dans l'entrée de référence, ou la forme de la première ligne du texte canonique, choisit par annotation du présent ADR, **avant** son commit `moteur:`, l'une des voies suivantes :
1. **Écrivain de la représentation précédente conservé le temps de la transition**, dans les tests et non dans le package : une copie figée, indépendante du moteur, à la manière de `texte_dates()` (`tests/reference/outils_reference.R`, #19). La garde calcule alors le texte de l'entrée sous l'ancienne représentation avec cet écrivain, au lieu de remplacer l'en-tête, et exige son MD5 égal à `fiche/empreintes/donnees` de la référence. Le contrôle de la réserve M2 (MD5 du texte courant égal à l'empreinte de l'objet) reste inchangé. L'écrivain figé est retiré par le commit des références qui ferme la transition. C'est la voie mécanique (`CLAUDE.md`, « Workflows », principe 7) : **recommandée**.
2. **Preuve hors de R de l'entrée inchangée**, comme pour #35 (ADR 0011, point 2.5), consignée dans le commit. Elle complète la voie 1 sans la remplacer : seule, elle laisserait la garde de la batterie en échec.
3. **Voie du point 4.2 assumée**, quand le changement de représentation coïncide avec un vrai changement de l'entrée (par exemple une modification du simulateur) : régénération de tous les plans, tableau des grandeurs agrégées, visa.

Dans tous les cas, le message de la garde devra nommer cette cause possible. Une garde qui échoue alors que seule `representation` est déclarée changée signale une nouvelle définition qui touche le corps du texte, et non une entrée changée : c'est le diagnostic à poser d'abord.

**Effet sur les résultats : aucun.** Aucun code n'est modifié ; la garde est inchangée et reste juste pour B4 ((b)).

Issues : #35, #19, #47 (garde de la troisième annotation, (f)). ADR 0011, points 2.5 et 10. Branche `claude/fiche-serialisation`, PR #51.

## Annotation du 20 octobre 2026 (seconde) — `fiche/empreintes/resultat` et `fiche/empreintes/code` : comparaison de forme par jeton, limitée à ces deux chemins, valeurs réelles dans l'en-tête (#67)

Rédigée par `architect-approfondi` d'après la décision du mainteneur du 20/10/2026 (M69, point 4 ; `docs/feuille-de-route.md`, § 4). Branche `claude/fiche-rapport`, PR #68, à `af54361`. Le texte ci-dessus, annotations comprises, n'est pas réécrit. Elle tranche le risque (1) du § 1.5 de la feuille de route **avant** le commit de #67, comme le prévoit l'ordre des commits.

**(a) Problème.** #67 calcule deux grandeurs qui valent `NA` dans R1 à R6 depuis leur création (ADR 0011, point 11, et annotation du 20/10/2026, cinquième) :
- `fiche/empreintes/resultat` n'est valable que **sur la même machine** (ADR 0011, point 11.2). R4 et R5 diffèrent d'un ulp entre la plateforme de référence et un autre hôte (ADR 0011, contexte, mesure 6) ;
- `fiche/empreintes/code` change à **chaque commit `moteur:`**.

Comparées en valeur, la première ferait échouer la batterie sur tout autre exécuteur, et la seconde demanderait une régénération à chaque commit `moteur:`.

**(b) Décision : option (a), jeton.**
- **Où.** La substitution se fait dans `aplatir_construction()` (`tests/reference/outils_reference.R:238`) et nulle part ailleurs. Le producteur et la batterie passent tous deux par cette fonction : la substitution est donc la même des deux côtés.
- **Quoi.** La valeur de chacun des deux chemins `fiche/empreintes/resultat` et `fiche/empreintes/code` est remplacée par le jeton `"<md5>"` (chaîne, longueur 1) **si et seulement si** c'est un `character` de longueur 1 de la forme `^[0-9a-f]{32}$`.
- **Toute autre valeur est gardée telle quelle** : `NA`, majuscules, longueur ou type différents. Elle est donc comparée exactement au jeton de la référence, et la comparaison relève un écart. Une empreinte redevenue `NA` par régression, ou mal formée, est ainsi détectée.
- **Structure inchangée.** Le chemin, le type et la longueur restent comparés (point 3.1), et le jeton relève de la classe exacte (point 3.4).

**(c) Condition 1 — portée fermée.** Le jeton ne s'applique qu'aux **deux chemins nommés**, jamais par une règle générique sur la forme hexadécimale. Une telle règle masquerait deux sortes de grandeurs exactes et portables :
- `fiche/empreintes/donnees`, comparée en premier par la garde « entrée différente » (point 3.2) ;
- les lignes `@empreinte` des tables résumées (point 1.2 ; `tests/reference/R1.tsv:47-61`, *vérifié* : cinq lignes `@empreinte` de 32 caractères hexadécimaux).

**(d) Condition 2 — valeurs réelles dans l'en-tête, hors comparaison**, comme `fiche$versions` et `fiche$plateforme` (points 2.2 et 7.1).
- `entete_reference()` (`tests/reference/outils_reference.R:750-755`) écrit deux clés, `fiche$empreintes/resultat` et `fiche$empreintes/code`, sur le modèle des clés `fiche$versions/…`.
- Ces valeurs disent de quel code et de quel résultat la référence a été produite sur la plateforme de référence.
- La comparaison ne les lit jamais.

**(e) Ce qui reste comparé exactement** :
- `fiche/empreintes/statut_resultat_code` ;
- `fiche/empreintes/representation_resultat`, si l'annotation de l'ADR 0011 est retenue ;
- `algorithme`, `representation`, `donnees`, `fichier` et `algorithme_fichier` ;
- toute autre grandeur.

**(f) Ce que le jeton ne teste pas.** Les propriétés des deux empreintes se testent dans les **batteries** (tests unitaires de #67), pas par les références :
- pour le résultat : identité sur deux constructions et au rejeu sur la même machine ; sensibilité à un ulp ; insensibilité à `execution`, `fiche$versions` et `fiche$plateforme` ; recalcul hors R ;
- pour le code : `NA` exclu ; égalité au recalcul sur `R/*.R` de l'arbre installé.

La référence ne vérifie que la **forme**.

**(g) Format inchangé : `tablexp reference 1`.**
- Le point 9.2 réserve l'incrément aux changements de grammaire (type, ligne `@`, échappement), et le jeton n'en introduit aucun : c'est une chaîne valide.
- La présente annotation modifie le **périmètre** (point 1.1 : deux valeurs remplacées par un jeton) et l'**en-tête** (points 2.2 et 7.1 : deux clés).
- Les six fichiers sont de toute façon régénérés par la régénération unique de la branche (étape (9) du § 1.5).
- Option écartée : `tablexp reference 2`, une version de format sans changement de grammaire.

**(h) Options écartées.**
- **Retrait des deux chemins de l'aplatissement**, comme `fiche$versions` et `fiche$plateforme` (`tests/reference/outils_reference.R:242`). On perdrait le contrôle de structure (présence, type, longueur) et la détection d'une empreinte redevenue `NA`.
- **Règle générique sur la forme hexadécimale** : exclue par la condition 1.
- **Comparaison en valeur** : voir (a).

**(i) Témoins** (point 3.6), dans `tests/test_reproductibilite.R`, sur des copies temporaires :
1. une empreinte de résultat ou de code `NA` ou mal formée donne un écart sur son chemin ;
2. une altération de `fiche/empreintes/donnees` reste « entrée différente » : le jeton ne s'y applique pas ;
3. une ligne `@empreinte` altérée reste un écart ;
4. l'en-tête porte les deux valeurs réelles, et une modification de ces valeurs dans l'en-tête ne change pas la comparaison ;
5. l'aller-retour `lire(ecrire(aplatir(x)))` reste `identical`.

**(j) Transition #47** (troisième annotation du 19/10/2026), pour les six plans :
- `fiche/empreintes/resultat` et `fiche/empreintes/code`, de `NA` à `"<md5>"` : nature « valeur de chaîne » ;
- `fiche/empreintes/statut_resultat_code` : « valeur de chaîne » ;
- la création de `representation_resultat` et le changement de `fiche/empreintes/@noms` suivent l'annotation de l'ADR 0011.

**Effet sur les résultats : aucun** par cette annotation. Elle fixe la forme de comparaison avant le code de #67.

Issues : #67 ; #47. Décision M69. ADR 0011, point 11 et annotation du 20/10/2026 (cinquième). Branche `claude/fiche-rapport`, PR #68.

## Annotation du 20 octobre 2026 (troisième) — renvois périmés au code (constat m6 de la revue finale de `claude/fiche-rapport`)

Rédigée par `architect` en fin de branche `claude/fiche-rapport` (PR #68, à `63b4bc5`), d'après le constat mineur m6 de la revue finale d'`audit` (effort `high`). Le texte ci-dessus, annotations comprises, n'est pas réécrit : la présente annotation dit seulement où se lisent aujourd'hui les objets que deux passages désignent.

**(a) Troisième annotation du 17/10/2026, (e), « Corrections »** : `octets_chaines()` n'est plus en `R/rapport.R:41`. Elle est définie en `R/rapport.R:106` à `63b4bc5` (en `:57` à `2146717`). Depuis #22, point 1 (`b1ed03b`), elle écrit par blocs bornés par `bornes_blocs()` (`R/rapport.R:83`), aux mêmes octets. La règle que suit l'outillage est inchangée : refus des chaînes indéterminées, jamais de conversion. L'outillage l'applique par `utf8_ou_refus()` (`tests/reference/outils_reference.R:13`, `:162`).

**(b) Annotation du 18/10/2026, complément (e), point 3.3** : `colonne_canonique()` (`R/rapport.R:109`) n'existe plus. Elle a été retirée par l'écriture canonique de B4 (`4610d17`, ADR 0011, point 7). L'écriture `sprintf("%.17g", x + 0)` se lit aujourd'hui dans la fonction locale `doubles_texte()` d'`ecrire_valeurs()`, forme « texte » (`R/rapport.R:240-246` à `63b4bc5`). L'outillage des références l'appelle par `tablexp:::ecrire_valeurs(v, "texte", n)` (`tests/reference/outils_reference.R:223`). Le constat est inchangé : −0 s'écrit 0. Mesure d'`architect` du 20/10/2026 (*vérifiée*) : `ecrire_valeurs(c(-0, 0), "texte", "t")$octets` vaut `30 0a 30 0a`, soit « 0 », « 0 ».

**Mesures** (`architect`, 20/10/2026, *vérifié*) :
- `grep -n '^octets_chaines\|^bornes_blocs\|sprintf("%.17g", x + 0)' R/rapport.R` à `63b4bc5` rend les lignes 106, 83, 242 et 244 ;
- `git log -S'colonne_canonique <- function' -- R/` rend `4610d17` (dernier commit qui touche la définition) et `125f3a3` (création).

Les numéros de ligne cités ici datent de `63b4bc5` : le nom de l'objet fait foi.

**Effet sur les résultats : aucun.** Aucun code n'est modifié par cette annotation.

Issues : #22 (point 1) ; #67. ADR 0007, annotation du 20/10/2026 (seconde). Branche `claude/fiche-rapport`, PR #68.
