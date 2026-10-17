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
