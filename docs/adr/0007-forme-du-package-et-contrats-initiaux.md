---
status: accepted
date: 2026-10-16
---

# Package `tablexp` à la racine, graphe d'appels des modules contrôlé par script, contrats initiaux (rôles, plan, objet résultat, refus, table de référence, aléa, empreintes, dates, tests) ; sérialisation du plan et de la fiche reportée

*Statut : `accepted` au titre du plan de la première branche accepté par le mainteneur le 16/10/2026 (décision M45, `docs/feuille-de-route.md` § 1), qui annonce le contenu du présent ADR ; comme les ADR 0002 à 0006 (Q40), la relecture du mainteneur est attendue. Les points marqués **[nouveau]** ne figuraient pas dans le plan accepté : ils sont proposés par `architect` et soumis à confirmation ; leur liste est reprise en fin d'ADR. Rédigé par `architect-approfondi` sur Fable (`docs/agents/routage.md`, § 4.1.3 ; décision M39 : aucun plafond pour cette branche).*

## Contexte

Les ADR du cadrage ont fixé l'architecture et laissé à « la première branche » la forme concrète du package : l'ADR 0004 ne règle ni l'emplacement du package (racine ou sous-dossier), ni la place d'`affichage.R` vis-à-vis de l'espace de noms, ni les noms des points d'entrée du simulateur et du suivi d'expérience, ni le format de l'objet retourné, et propose, « à confirmer », un ordre de dépendance des modules contrôlé par script (ADR 0004, « Conséquences ») ; l'ADR 0003 renvoie la représentation interne d'une transition et d'un état ; l'ADR 0005 veut le format du plan et de la fiche « fixé au plan de la première branche » ; l'ADR 0006 laisse ouverte la représentation interne commune des tables de référence. Les issues #8 à #11 citent l'ADR 0007 comme préalable (#8 : ordre des modules ; #10 : nom de `simuler_portefeuille` ; #11 : objet résultat, fiche en mémoire, empreinte sans `serialize()`), et le plan M45 tranche déjà P8 (empreintes : MD5 de `tools::md5sum` sur une représentation textuelle canonique, algorithme nommé dans la fiche) et P4 (comparaison à `stats` par appel direct dans la batterie, sous un terme distinct de « contre-vérification »).

Sans ces contrats, les trois issues de code de la branche (#9, #10, #11) seraient spécifiées par `actuary` et implémentées par `coder` sur des formes implicites : chaque module inventerait sa manière de refuser, de consigner un avertissement, de tirer un aléa, et l'objet résultat se fixerait par accrétion — exactement ce que l'ADR 0004 a voulu éviter en écartant le fichier unique.

**Précédent.** Le dépôt `outil_USP` (même mainteneur ; dispositifs repris par M8), lu le 16/10/2026 à `/home/user/gerard-garey/outil_usp` : harnais de tests en R de base sans `testthat` (`tests/unitaires/outils_unitaires.R` : `verifier()`, `echec_attendu(nom, constat, expr)`, statuts `OK` / `ECHEC` / `ATTENDU` / `INATTENDU`, code de sortie 1 sur échec ou succès inattendu) ; exécution sous graine locale avec générateur fixé et état de l'appelant restauré (`R/engine.R`, `engine_sous_graine()`, `ENGINE_RNG_KIND`, son ADR 0004 et l'annotation du 25/09/2026) ; empreinte des données par `tools::md5sum` sur un texte canonique écrit en `%.17g`, `NA`, LF, octet par octet (`engine_empreinte()`, son ADR 0009, point 3). Ce qui n'est pas repris : le moteur en fichier unique, la structure `Type: Shiny` de `DESCRIPTION` (l'application y est le package), et l'objet refusé `res$ok = FALSE` (voir « Options écartées »).

**Mesures sur R 4.3.3** (`Rscript`, R 4.3.3 (2024-02-29), Linux, 16/10/2026 ; scripts du bac à sable de la session, commandes citées dans le corps du texte) :

- `exists("sha256sum", asNamespace("tools"))` → `FALSE` ; `names(formals(tools::md5sum))` → `files` seulement : ni SHA-256, ni `md5sum(bytes =)` en R 4.3.3 ;
- `serialize(list(1), NULL)[1:14]` → `58 0a 00 00 00 03 00 04 03 03 00 03 05 00` : les octets 7 à 10 (`00 04 03 03`) sont la version de R qui écrit (4.3.3), les octets 11 à 14 la version minimale de lecture ; l'en-tête ASCII (`serialize(ascii = TRUE)`) porte `262915` = 4·65536 + 3·256 + 3. Une empreinte de `serialize()` change donc avec la version de R à données identiques ;
- `RNGkind()` par défaut → `Mersenne-Twister / Inversion / Rejection` ;
- `sprintf("%.17g", x)` suivi d'`as.numeric()` redonne `x` au bit près sur 10^5 doubles tirés dans [−5·10^5, 5·10^5] (`all(... == z)` → `TRUE`) ; `%.15g` ne le fait pas (`FALSE`) ; `sprintf("%.17g", c(NA_real_, NaN, Inf, -0))` → `NA NaN Inf -0`, et `-0 + 0` → `0` (`identical(0, -0)` vaut `TRUE` en R : la normalisation `x + 0` évite qu'une même valeur donne deux empreintes) ;
- `Sys.getlocale("LC_NUMERIC")` → `C` : le séparateur décimal de `sprintf` ne dépend pas de la locale ;
- `format(as.Date("2024-02-29"), "%Y-%m-%d")` → `2024-02-29` ; `as.integer(as.Date("2024-02-29"))` → `19782` ; la classe `Date` n'a pas de fuseau ;
- représentation canonique d'un `data.frame` de 10^6 lignes × 6 colonnes (entier, deux dates, deux chaînes, un double) : 7,8 s, 58,4 Mo de texte, empreinte identique sur deux appels, différente quand l'ordre des lignes est inversé ;
- échappement des chaînes : `encodeString()` dépend de la locale (en locale `C`, « é » devient `\303\251` ; en `C.UTF-8`, il reste « é ») ; `enc2utf8()` sur des octets non ASCII **non marqués** en locale `C` les transforme en `<c3><a9>` ; en revanche, pour des chaînes **marquées UTF-8**, la suite `enc2utf8()` → `gsub(fixed = TRUE, useBytes = TRUE)` → `charToRaw()` par élément → `writeBin()` donne la **même empreinte** (`6bf144403eadd9c2721959eb4f2a9326`, 27 octets) sous `LANG=C` et sous `LANG=C.UTF-8` ; `validUTF8()` et `l10n_info()` existent pour le contrôle ; `paste()` d'une chaîne au codage `bytes` la réécrit en `<xx>` (à proscrire dans la chaîne de calcul de l'empreinte) ;
- `tempfile()` et `tools::md5sum()` ne créent pas `.Random.seed` (`exists(".Random.seed", globalenv())` → `FALSE` avant et après) ;
- une fonction `sous_graine()` qui sauvegarde `.Random.seed` et `RNGkind()`, pose `set.seed(graine, "Mersenne-Twister", "Inversion", "Rejection")`, évalue puis restaure (sous `on.exit`) : deux appels donnent des tirages `identical` ; mêmes tirages que `set.seed(graine)` nu ; appelant sous `L'Ecuyer-CMRG` : tirages identiques, `RNGkind()` et `.Random.seed` de l'appelant restaurés, tirage suivant de l'appelant identique à celui qu'il aurait eu sans l'appel ; état restauré aussi après une erreur dans l'expression ;
- graphe d'appels par `parse()` et `all.names()` sur cinq fichiers témoins : les arêtes entre modules sont retrouvées (y compris un appel interdit `exposition → brut`), et un appel par chaîne `do.call("estimer_brut", …)` échappe à `all.names()` mais se détecte en parcourant les appels dont le premier argument est une chaîne ;
- `.Rbuildignore` n'est **pas** appliqué par `R CMD INSTALL <dossier>` (un fichier `R/b.R` exclu par `.Rbuildignore` est installé : `exists("b")` → `TRUE`) ; il l'est après `R CMD build` (`FALSE`). Tout fichier de `R/` à la racine fait donc partie du package installé depuis l'arbre de travail.

## Décision

Arrêtée par `architect` le 16/10/2026 dans le cadre du plan accepté M45 ; les points **[nouveau]** sont soumis au mainteneur.

### 1. Emplacement et fichiers

1.1. **Le package `tablexp` est à la racine du dépôt** (#8) : `DESCRIPTION` (`Package: tablexp`, `Depends: R (>= 4.3.3)` — M44 —, `Imports` limités à la priorité `base`, ni `Suggests` ni `LinkingTo` tant qu'aucun ADR de méthode ne l'exige, `Encoding: UTF-8`, `License: file LICENSE`), `NAMESPACE` écrit à la main, `R/`, `inst/extdata/` (jeux simulés, tables de référence et fichiers de provenance, ADR 0006), `tests/` (batteries, hors `R CMD check`), `.Rbuildignore` (`^app$`, `^docs$`, `^tests$`, `^\.claude$`, `^\.github$`, `^CLAUDE\.md$`, `^CONTEXT\.md$`, `^README\.md$`, `^AGENTS\.md$`, `^LICENSE$` si `License: file LICENSE` le permet, etc.). Aucun fichier `Rd`, aucun `roxygen2` : la documentation de référence est le document LaTeX (ADR 0004, point 6 d) et le test de concordance (E3.3).

1.2. **`R/` ne contient que du code du package**, conséquence de la mesure sur `.Rbuildignore` : dix fichiers de module `R/<module>.R` (`donnees`, `exposition`, `brut`, `lissage`, `positionnement`, `prospective`, `fermeture`, `validation`, `simulation`, `rapport` ; ADR 0004, point 1 ; seuls ceux que la branche crée existent) et deux fichiers transversaux : `R/construire_table.R`, limité à l'orchestration (point 3.1), et **`R/outils.R` [nouveau]**, qui porte les services sans contenu métier partagés par tous les modules — exécution sous graine locale (point 8), conditions de refus et avertissements (point 6.4), constantes de plateforme — et n'appelle aucun module. Le test de suppression le justifie : sans `outils`, `sous_graine()` serait dupliquée dans `simulation` et `brut` (bootstrap, v1) et la forme d'un refus dans chaque module.

1.3. **L'application vit hors du package, sous `app/` [nouveau]** : `app/app.R` et `app/affichage.R`, lancés par `shiny::runApp("app")`. Raison : avec le package à la racine, `R/affichage.R` serait installé dans l'espace de noms de `tablexp` (mesure ci-dessus), ce qui ferait entrer dans le moteur un fichier qui n'en est pas et rendrait le contrôle de frontière (ADR 0004, point 6 b) circulaire ; de plus, Shiny charge automatiquement les fichiers d'un dossier `R/` adjacent à `app.R` [non vérifié ici : documentation Shiny non accessible depuis la session ; `coder` le vérifie à l'implémentation de l'application], ce qui donnerait à l'application un accès direct aux fonctions internes du moteur si `app.R` était à la racine. Cette décision règle le point laissé ouvert par l'ADR 0004 (« place d'`affichage.R` vis-à-vis de l'espace de noms ») ; `CLAUDE.md` (« Architecture », domaine de commit `app:`) et l'ADR 0004 (point 5, mention datée) sont à aligner par la session principale. L'application n'appelle que des fonctions **exportées** de `tablexp` ; `tablexp:::` y est interdit et contrôlé par le script de frontière.

### 2. Graphe d'appels des modules, contrôlé par script

2.1. **Ordre des modules confirmé** (ADR 0004, « Conséquences ») et rendu explicite sous forme de **graphe d'appels autorisés** : un module ne peut appeler que des fonctions définies dans les modules situés **strictement avant lui** dans l'ordre `outils` → `donnees` → `exposition` → `brut` → `lissage` → `positionnement` → `prospective` → `fermeture` → `validation` → `rapport`, avec les précisions suivantes :

| Module appelant | Peut appeler |
|---|---|
| `outils` | rien (fonctions de `base`, `stats`, `utils`, `tools` seulement) |
| `donnees` | `outils` |
| `exposition` | `donnees`, `outils` |
| `brut` | `exposition`, `donnees`, `outils` |
| `lissage` | `brut` et amont |
| `positionnement` | `lissage` et amont (la méthode 4 de E2.12 est un lissage avec dérive de la référence) ; **`lissage` n'appelle jamais `positionnement`** |
| `prospective` | `positionnement` et amont |
| `fermeture` | `prospective` et amont |
| `validation` | `fermeture` et amont |
| `rapport` | tous les modules ci-dessus (quantités des rapports, fiche, empreintes) |
| `simulation` | **`donnees` et `outils` seulement** ; **aucun module du moteur n'appelle `simulation`** |
| `construire_table.R` | tous les modules **sauf `simulation`** |

Les tests appellent n'importe quel module (`tablexp:::` admis dans `tests/`, point 11.3).

2.2. **Contrôle par script, en CI** (#8, critère d'acceptation « ordre des modules »), en R de base, sans `codetools` (priorité `recommended`) : pour chaque fichier de `R/`, `parse(keep.source = FALSE)` ; les définitions de premier niveau de la forme `nom <- function(...)` donnent la table « fonction → module » ; pour chaque fonction, `all.names(corps)` intersecté avec cette table donne les arêtes ; toute arête hors du tableau 2.1 fait échouer le script, qui imprime `de`, `vers`, fonction appelante et fonction appelée. Le script est testé sur deux témoins (#8 : `exposition` qui appelle `brut` ; `donnees` qui appelle `simulation`).

2.3. **Appels dynamiques interdits dans `R/`**, parce qu'ils échappent à `all.names()` (mesure ci-dessus) : `do.call()` dont le premier argument est une chaîne, `match.fun()`, `get()`, `mget()`, `getFromNamespace()`, `eval(parse(...))`, `:::` et `::` vers `tablexp` lui-même. `do.call(f, args)` avec `f` un objet fonction reste permis. Le script le vérifie en parcourant les appels (premier argument de type chaîne). Limite connue : une fonction passée en argument puis appelée (`FUN(x)`) crée une arête que le script attribue à l'appelant qui a passé l'argument ; c'est le comportement voulu (la dépendance existe bien là).

### 3. Points d'entrée et règle d'export

3.1. **`construire_table(donnees, roles, plan, provenance = NULL)`** dans `R/construire_table.R` : valide les rôles (par `donnees`), valide le plan (point 5.4), exécute les étapes dans l'ordre du plan en appelant une fonction d'étape par module, assemble l'objet résultat et la fiche (par `rapport`), et retourne l'objet. Le fichier contient cette fonction, le validateur du plan (`valider_plan()`, contrôle de saisie à sens métier que l'application pourra appeler, E4.3) et la table de correspondance étape → fonction d'étape ; **aucun calcul**. **`provenance` [nouveau]** est le quatrième argument, facultatif, par lequel la couche de lecture transmet au moteur, comme entrée déclarée, le nom du fichier lu, son empreinte et l'algorithme (ADR 0004, point 3) ; `NULL` signifie « données construites en mémoire, aucun fichier », ce que la fiche écrit. L'ajout d'un argument nommé facultatif ne change pas la signature `construire_table(donnees, roles, plan)` des ADR 0004 et 0005 ni les appels existants.

3.2. **`simuler_portefeuille(...)`** dans `R/simulation.R` (#10), entrées explicites (loi vraie, paramètres du portefeuille, période d'observation, graine), sortie : un `data.frame` de lignes individuelles portant les rôles du catalogue, avec en attribut `simulation` la loi vraie, les paramètres, la graine et le générateur consignés.

3.3. **`suivre_experience(...)`** est **réservé** pour le point d'entrée du suivi d'expérience (ADR 0004, point 2 ; ADR 0006, point 5) : aucune fonction de ce nom n'est créée dans cette branche et aucun autre usage du nom n'est admis ; sa signature sera fixée par la branche de suivi d'expérience, sur le même modèle (entrées explicites, objet complet).

3.4. **`NAMESPACE` écrit à la main**, un `export()` par fonction, sans `exportPattern`. Sont exportés : les points d'entrée (3.1, 3.2), et les fonctions **que l'application appelle** — contrôles de saisie à sens métier (E4.3) et constructeurs d'objets partagés — chacune citée dans la documentation LaTeX (concordance, E3.3 ; en attente dans la liste de `tests/` tant que `docwriter` n'est pas passé). Liste initiale de la branche : `construire_table`, `simuler_portefeuille`, `catalogue_roles` (le catalogue fermé, lu par l'application pour la déclaration des rôles), `table_reference` (point 7). Tout export ajouté ensuite est motivé dans le commit par son appelant hors package. Aucune méthode S3 n'est exportée dans cette branche (pas de `print` : la restitution est l'affaire de l'application et du rapport).

### 4. Rôles

4.1. `roles` est un **vecteur de chaînes nommé** : noms = noms de rôles du catalogue fermé (ADR 0003, point 2 ; E2.2), valeurs = noms de colonnes de `donnees`. Noms et valeurs sont uniques ; un rôle inconnu, une colonne absente, une colonne déclarée deux fois, un type incompatible ou un rôle requis manquant sont des refus (#9). Les modalités attendues d'un rôle catégoriel (cause de sortie, sexe) sont fixées par la spécification d'`actuary` (#9) ; la forme (vecteur nommé) est le contrat.

### 5. Plan de construction

5.1. **Le plan est une liste R nommée**, construite en mémoire par l'appelant (tests, application), **validée entièrement avant toute exécution**, sans aucun défaut implicite (ADR 0005, point 3 étendu à tous les paramètres). Champs de premier niveau :

- `transition` : liste `list(depart = <chaîne>, arrivee = <chaîne>)`, les deux requis ; `arrivee` est une modalité du rôle « cause de sortie », `depart` l'état d'observation (modalité du rôle « état d'entrée » quand il est déclaré, sinon l'unique état de départ nommé par la spécification) ; les autres modalités de sortie sont les **sorties concurrentes**, traitées en censure (ADR 0003, points 1 et 6). C'est la représentation interne d'une transition renvoyée par l'ADR 0003 : deux noms d'états, pas de code numérique ;
- `periode` : liste `list(debut = <Date>, fin = <Date>)` (point 10) ;
- `delai_declaration` : entier de jours, **requis** (zéro explicite admis) ;
- `graine` : entier, **requis dès qu'une étape du plan tire un aléa**, interdit sinon (la liste des étapes qui tirent est une constante du package) ; absent dans cette branche, la fiche l'écrit `NA` avec la mention « aucune étape aléatoire » ;
- `etapes` : **liste nommée ordonnée** ; les noms sont les noms d'étapes (dans cette branche : `donnees`, `exposition` ; ensuite `brut`, `lissage` ou `positionnement`, `prospective`, `fermeture`, `marge`, `validation`, `table`), chaque nom au plus une fois, dans un ordre compatible avec l'ordre des modules ; la valeur de chaque entrée est la **liste nommée des paramètres de l'étape**, tous requis.

5.2. **Catalogue des paramètres par étape** : constante du package (`PARAMETRES_ETAPES`), une entrée par étape avec le nom, le type et le domaine de chaque paramètre ; c'est elle que `valider_plan()` applique et que la documentation LaTeX recense (concordance). Le contenu de la branche — pour `donnees` : unité d'observation (`tete` ou `contrat`), règles d'exclusion par motif (liste, éventuellement vide, mais présente) ; pour `exposition` : convention d'âge forcée (`deduite` explicite ou une convention plus grossière), segmentation (vecteur de rôles, éventuellement vide, mais présent), pondération (`aucune` ou le rôle de montant), découpage par année (`TRUE`/`FALSE`) — est fixé par la spécification d'`actuary` (#9, #11) ; l'ADR fixe la forme et la règle « présent ou refusé ».

5.3. **La recette d'exposition n'est pas un paramètre du plan** : le moteur la déduit des rôles (ADR 0003, point 3) et l'inscrit dans la fiche ; seul un forçage vers une convention plus grossière est un paramètre.

5.4. **Validation** (`valider_plan()`) : champ de premier niveau inconnu ou manquant, étape inconnue, étape en double, ordre incompatible, paramètre inconnu, paramètre manquant, type ou domaine hors catalogue → refus (point 6.4) qui nomme l'étape et le paramètre. Une valeur « proposée » par l'application n'existe pas pour le moteur tant qu'elle n'est pas dans le plan (ADR 0005, règle durable).

### 6. Objet résultat, fiche en mémoire, refus et avertissements

6.1. **L'objet retourné est une liste nommée de classe S3 `construction`** (noms sans accents). Classe S3 sur une liste, et non liste nue : l'attribut de classe permet un contrôle d'entrée (`inherits(x, "construction")`) dans le rapport, l'application et `suivre_experience`, sans rien coûter à la lecture par `$` ; ni S4, ni R6, ni environnement (« Options écartées »). Champs :

- `table` : la table finale (`NULL` tant que le plan ne va pas jusqu'à l'étape `table`) ;
- `etapes` : liste nommée, dans l'ordre d'exécution, une entrée par étape exécutée : `parametres` (paramètres effectifs, après résolution par le moteur : recette retenue, convention retenue ou forcée), `resultat` (les quantités calculées par l'étape : pour `donnees`, lignes conservées, exclusions comptées et listées par motif, unité retenue ; pour `exposition`, exposition et événements par âge et segment), `diagnostics` (indicateurs, décomptes) ;
- `avertissements` : `data.frame` (`etape`, `code`, `message`, `nombre`) des avertissements de toutes les étapes (« unité : contrat », incohérences entre contrats d'une tête relevées…) ;
- `fiche` : la fiche de construction (6.2) ;
- `execution` : horodatage de début, durée, `sessionInfo()` abrégé — **hors de toute comparaison** (rejeu, reproductibilité, non-régression) ; c'est le seul champ qui change entre deux exécutions identiques.

6.2. **Fiche de construction en mémoire** (`fiche`, ADR 0005, point 6), liste nommée : `version_fiche` (entier, 1), `versions` (`tablexp` depuis `DESCRIPTION`, `R.version.string`), `plateforme` (`sysname`, `release`, `machine`, `extSoftVersion()[["BLAS"]]`, `La_library()`), `roles`, `plan` (le plan **reçu**, inchangé : c'est lui qu'on rejoue), `transition`, `periode`, `delai_declaration`, `unite_observation`, `recette`, `convention` (`retenue`, `forcee`), `graine`, `generateur` (point 8), `provenance` (argument reçu, ou « aucun fichier »), `empreintes` (point 9). **Rejeu** : `construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance)` redonne un objet `identical` hors `execution` ; c'est un test de #11.

6.3. **Contrat de lecture** : application, rapport figé et tests lisent l'objet par `$` sur les noms ci-dessus et ne recalculent rien (ADR 0004, point 2). Ajouter un champ est compatible ; renommer ou retirer un champ touche le contrat partagé « objet résultat » (`docs/agents/routage.md`, § 4.2).

6.4. **Un refus est une condition R typée, jamais un objet retourné [nouveau]** : `refuser(module, motif, message, details)` (dans `outils`) lève une erreur de classe `c("refus_tablexp", "error", "condition")` portant `module`, `motif` (code court stable, par exemple `role_absent`, `recette_impossible`, `ligne_invalide`, `etape_inconnue`), `message` (phrase qui dit ce qui manque, E5.5) et `details` (liste : rôle, colonne, étape, paramètre, décompte, premiers numéros de ligne). Les tests vérifient motif, décompte et numéros par `tryCatch(…, refus_tablexp = function(r) …)` ; l'application attrape la condition et affiche le message. Raison : un refus n'est pas un résultat (ADR 0003, point 3 : « elle refuse ») ; un objet `ok = FALSE` (forme d'`outil_USP`) peut être lu par mégarde comme une construction partielle, une condition ne le peut pas.

6.5. **Les avertissements métier sont inscrits dans l'objet, non émis par `warning()` [nouveau]** : `avertir()` (dans `outils`) ajoute une ligne à l'accumulateur de l'exécution, restitué dans `avertissements` (6.1). Raison : l'objet est la seule source du rapport figé et de l'application ; des `warning()` seraient perdus par l'application, pollueraient les batteries et ne figureraient pas dans la fiche. L'issue #9 dit « émis et restitué dans l'objet » : la présente décision lit « émis » comme « inscrit dans l'objet » ; à confirmer.

### 7. Représentation commune d'une table de référence

7.1. **Classe S3 `table_reference` sur un `data.frame` en format long**, colonnes `age` (entier), `sexe` (chaîne, modalités fixées par la spécification d'`actuary`, dont une modalité pour une table sans distinction de sexe), `qx` (double), et, à partir de la v2, `annee` (entier) pour les tables prospectives ; attributs `nom`, `statut` (`synthetique`, `embarquee`, `importee`), `source` (texte libre : arrêté, URL, fichier de provenance), `forme_origine` (`qx` ou `lx`, pour une table transcrite en l_x et convertie). Constructeur `table_reference()` dans `donnees` (exporté : l'application importe des tables), qui **refuse** `qx` hors [0, 1], des âges non contigus ou en double au sein d'un même (`sexe`, `annee`), une colonne manquante ou mal typée. La loi vraie du simulateur (#10, P5 : Gompertz-Makeham synthétique) et les tables embarquées (ADR 0006, troisième branche) prennent cette forme ; `positionnement`, `validation`, `suivre_experience` et le simulateur ne lisent une table de référence que sous cette classe.

### 8. Aléa

8.1. **Générateur fixé** : constante `GENERATEUR <- c(kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")` dans `outils` (valeurs par défaut de R 4.3.3, mesurées) ; **une fonction unique `sous_graine(graine, expr)`** sauvegarde `.Random.seed` de l'environnement global (ou son absence) et `RNGkind()`, pose `set.seed(graine, kind, normal.kind, sample.kind)`, évalue, puis restaure l'état et le réglage de l'appelant, y compris en cas d'erreur (comportement mesuré ci-dessus). **Toute simulation du package passe par elle** (`simuler_portefeuille` dans cette branche ; bootstrap par têtes ensuite), avec une graine explicite reçue en argument (jamais une graine par défaut), consignée dans l'attribut `simulation` du jeu simulé ou dans la fiche, avec le générateur. Limite dite, reprise d'`outil_USP` : la réserve normale de Box-Muller d'un appelant, gardée par R hors `.Random.seed`, n'est pas restaurable ; sans effet sur le moteur, qui tire toujours sous Inversion.

8.2. **Aucun aléa hors calcul** : fichiers temporaires par `tempfile()` (mesuré : ne touche pas `.Random.seed`), supprimés par `on.exit(unlink())`.

### 9. Empreintes

9.1. **Algorithme** : MD5, par `tools::md5sum()` sur un **fichier temporaire** écrit en mode binaire (P8). Raison : R 4.3.3 n'offre ni SHA-256 ni `md5sum(bytes =)` (mesuré), et le moteur n'admet aucun package hors priorité `base` (ADR 0002). L'empreinte identifie une entrée ; elle n'a pas de rôle de sécurité. **L'algorithme est nommé dans la fiche** (`empreintes$algorithme = "md5"`) pour qu'un changement ultérieur soit visible et daté.

9.2. **Ce que porte la fiche** (`empreintes`) : `algorithme` ; `donnees` (9.3) ; `fichier` (reçue par `provenance`, `NA` sinon, avec l'algorithme déclaré par la couche de lecture) ; `resultat` et `code` : champs présents, à `NA` dans cette branche avec le statut « non calculée (branche `fiche-rapport`) », parce que l'empreinte du résultat dépend de la plateforme (E3.6) et appelle la sérialisation reportée (point 12), et que celle du code suppose de fixer ce qu'est « le code » (fichiers de `R/` ? version installée ?).

9.3. **Représentation textuelle canonique des données reçues** — définie indépendamment de la manière de l'écrire, de sorte qu'un tiers la recalcule avec n'importe quel outil MD5 :

- **périmètre** : les seules colonnes de `donnees` auxquelles un rôle est déclaré, **toutes les lignes reçues** (avant exclusions), dans l'ordre reçu ; une base dont les lignes sont permutées a une autre empreinte, à résultats égaux : l'empreinte identifie l'entrée, pas le résultat ;
- **ordre des colonnes** : par **nom de rôle**, ordre d'octets (`sort(method = "radix")`, indépendant de la locale, mesuré) ; le nom de colonne n'entre pas dans le texte (il est dans `fiche$roles`) : renommer une colonne ne change pas l'empreinte des données ;
- **structure** : première ligne `tablexp canonique 1<TAB><nb lignes><TAB><nb colonnes>` ; puis, par colonne, une ligne d'en-tête `#<rôle><TAB><type>` suivie d'**une valeur par ligne** ; lignes séparées par LF (`0x0A`), fichier terminé par LF, **aucun CR**, écrit octet par octet (`writeBin`) — jamais `writeLines` en mode texte, qui produirait CRLF sous Windows [hypothèse : comportement documenté de R sous Windows, non mesuré ici] ;
- **types et format des valeurs** — `double` : `sprintf("%.17g", x + 0)` (`+ 0` normalise le zéro négatif), manquant `NA`, `NaN`, `Inf`, `-Inf` ; `integer` : `sprintf("%d")`, manquant `NA` ; `logical` : `TRUE`, `FALSE`, `NA` ; `Date` : `%Y-%m-%d` (ISO 8601), manquant `NA` ; `character` et `factor` (converti en chaînes, les niveaux n'entrent pas) : octets UTF-8 de la chaîne entre guillemets droits, avec cinq échappements `\\`, `\n`, `\r`, `\t`, `\"`, manquant `NA` sans guillemets (`"NA"` avec guillemets est la chaîne « NA ») ; **toute autre classe** (`POSIXct`, `difftime`, colonne-liste…) est **refusée** par `donnees` ;
- **codage des chaînes** : le texte est en UTF-8 ; une chaîne est convertie par `enc2utf8()` puis traitée en octets (`gsub(fixed = TRUE, useBytes = TRUE)`, `charToRaw()` par valeur ; jamais `encodeString()` ni `paste()` sur des chaînes au codage `bytes`, mesurés dépendants de la locale) ; le moteur **refuse** une chaîne qui n'est pas de l'UTF-8 valide (`validUTF8()`), une chaîne au codage `bytes`, et, dans une session dont la locale n'est ni UTF-8 ni latin-1 (`l10n_info()`), une chaîne non ASCII non marquée (son codage est indéterminé et `enc2utf8()` la dénaturerait silencieusement en `<xx>`, mesuré) ; sous ces règles, l'empreinte est identique sous `LANG=C` et `LANG=C.UTF-8` (mesuré) ;
- **mise en œuvre libre** : l'implémentation peut écrire par morceaux et n'échapper que les valeurs distinctes d'une colonne catégorielle, pourvu que les octets produits soient ceux de la définition ; un test écrit l'empreinte attendue d'un petit `data.frame` calculée à la main (`md5sum` du texte écrit par un autre outil) ;
- **coût** : 7,8 s pour 10^6 × 6 (mesuré), sans seuil ; à remesurer avec #11.

9.4. **Pourquoi pas `serialize()`** : l'en-tête porte la version de R (mesuré) et l'encodage natif ; une empreinte identique à données identiques sous deux versions de R, exigée par #11, est impossible par cette voie.

### 10. Dates

10.1. Toute colonne de date reçue par le moteur est de **classe `Date`** ; `POSIXct`, `POSIXlt`, chaînes et nombres sont refusés par `donnees` (la couche de lecture convertit, avec un format déclaré ; le moteur ne devine pas un format). Pas de fuseau, pas d'heure. Les durées sont des **décomptes de jours entiers** (`as.integer(fin - debut)`), et les âges se calculent par comparaison d'anniversaires (convention de l'âge au dernier anniversaire), jamais par division d'un nombre de jours par 365,25. La fraction d'année retenue pour l'exposition et le traitement du 29 février sont fixés par la spécification d'`actuary` (#11) et documentés ; l'ADR ne fixe que le type et l'unité.

### 11. Tests

11.1. **Trois batteries en R de base, sans `testthat`** (#8) : `tests/test_unitaires.R` (lance chaque `tests/unitaires/test_*.R` dans son environnement), `tests/test_reproductibilite.R` (deux exécutions `identical` hors `execution` ; rejeu de la fiche ; comparaison aux références de non-régression à partir de la deuxième branche), `tests/concordance_doc_moteur.R --strict`. Chaque batterie **installe l'arbre de travail dans une bibliothèque temporaire** (`R CMD INSTALL --no-docs -l <tmp> .`, puis `library(tablexp, lib.loc = <tmp>)`), affiche `R.version.string` et une ligne de bilan (nombre de tests, échecs, échecs attendus, succès inattendus), et sort avec un code non nul en cas d'échec.

11.2. **Harnais commun `tests/harnais.R`**, repris de la forme d'`outil_USP` : `verifier(nom, expr)` (l'expression vaut `TRUE` ou un message) ; `echec_attendu(nom, issue, expr)` — **défaut connu, codé en échec attendu avec renvoi obligatoire à l'issue** (`"#N"`) ; l'expression décrit le comportement correct ; tant qu'elle échoue, le test est `ATTENDU` ; **si elle passe, succès inattendu, la batterie échoue** et la marque est retirée pour en faire un test ordinaire (`CLAUDE.md`, « Commandes ») ; `proche(a, b, rel, abs)` (tolérance déclarée) ; `leve_refus(expr, motif)` (vrai si l'expression lève un `refus_tablexp` du motif donné) ; `sans_execution(x)` (retire `execution` avant `identical`) ; `bilan()`. Le harnais est lui-même testé (#8 : une assertion fausse donne un code non nul ; un échec attendu qui réussit fait échouer).

11.3. **Accès aux fonctions internes** : les tests appellent les fonctions non exportées par `tablexp:::` ; c'est le seul endroit du dépôt où `:::` est admis (interdit dans `R/`, point 2.3, et dans `app/`, point 1.3).

11.4. **Quatre natures d'attendu**, nommées dans chaque test (E2.20) : valeur calculée à la main (identité exacte, tolérance 1e-12 dans #11), **recoupement** (appel direct d'une fonction de `stats` dans la batterie, terme de `CONTEXT.md` issu de P4 ; le test dit si le moteur repose sur la même fonction), **contre-vérification** (valeur en dur produite hors CI, `CONTEXT.md`), constat de simulation (n, α, probabilité de fausse alarme écrits).

### 12. Sérialisation du plan et de la fiche : reportée, contraintes posées

12.1. **Le format de sérialisation (export / import du plan, écriture de la fiche) est reporté à la branche `claude/fiche-rapport`** (`docs/feuille-de-route.md`, § 2), **en écart avec l'ADR 0005** (« Conséquences » : « format fixé au plan de la première branche »), annoté à cette date. Raison : cette branche n'a ni application, ni rapport, ni export ; le plan et la fiche y vivent en mémoire (points 5 et 6) et le rejeu se teste sur l'objet ; fixer un format sans consommateur, c'est le fixer sans le tester.

12.2. **Contraintes posées dès maintenant**, que le format devra satisfaire : (a) texte lisible par un relecteur (un certificateur lit la fiche, E3.7) ; (b) **aller-retour identique** : lire ce qui a été écrit redonne un plan (resp. une fiche) `identical` à l'original, nombres compris (écriture des doubles en `%.17g`, dates en ISO 8601, chaînes en UTF-8, `NA` distingué de la chaîne « NA ») ; (c) **aucune évaluation de code à l'import** : ni `dget()`, ni `source()`, ni `eval(parse())` ; un plan importé est une donnée, validée par `valider_plan()` comme un plan construit en mémoire ; (d) écrit et lu en R de base (l'ADR 0002 exclut `jsonlite` du moteur) ; (e) versionné (`version_fiche`).

## Options écartées

- **Package dans un sous-dossier `tablexp/`, application à la racine** : garde `app.R` et `R/affichage.R` à la racine (forme d'`outil_USP`), mais toute commande (`R CMD INSTALL`, batteries, CI, concordance) doit viser le sous-dossier, et `R/` à la racine désignerait alors l'application, à rebours de `CLAUDE.md`. Le plan accepté M45 (#8) a retenu la racine.
- **`R/affichage.R` dans le package, non exporté** : l'affichage entrerait dans l'espace de noms du moteur ; le contrôle de frontière vérifierait l'absence de symboles Shiny dans un fichier qui existe pour Shiny ; et `R CMD INSTALL` l'installe quoi qu'en dise `.Rbuildignore` (mesuré).
- **`app.R` à la racine avec `options(shiny.autoload.r = FALSE)`** : repose sur une option posée au bon moment ; une session qui l'oublie charge tout `R/` dans l'application [non vérifié ici]. Le dossier `app/` ne dépend d'aucune option.
- **`testthat`** : package CRAN, hors priorité `base` ; la CI devrait l'installer ; le harnais d'`outil_USP` montre qu'un harnais en R de base de cent lignes suffit, avec l'échec attendu renvoyant à une issue, que `testthat` n'impose pas.
- **`roxygen2` et fichiers `Rd`** : dépendance de génération et seconde documentation à tenir concordante avec le LaTeX ; le test de concordance (E3.3) joue le rôle de `R CMD check` pour les noms.
- **`codetools` pour le graphe d'appels** : priorité `recommended`, exclu par l'ADR 0002 ; `parse()` et `all.names()` suffisent (mesuré).
- **Graphe de dépendances par convention de préfixe** (`exp_`, `brut_`) au lieu du fichier : le préfixe est une déclaration, le fichier est un fait que le script lit ; un préfixe oublié crée une arête invisible.
- **`simulation` autorisé à appeler `exposition`** (pour « simuler en avant avec la même recette ») : créerait un cycle de dépendance de fait entre le simulateur et ce qu'il doit tester ; le simulateur produit des données portant des rôles, et c'est le moteur, seul, qui en calcule l'exposition (ADR 0003, point 9 : « même modèle », pas « même code »).
- **Refus comme objet retourné (`ok = FALSE`)**, forme d'`outil_USP` : un refus partiel pourrait être lu comme un résultat ; une condition typée ne le peut pas et transporte les mêmes champs (6.4).
- **Avertissements par `warning()`** : perdus par l'application, absents de la fiche, bruit dans les batteries (6.5).
- **Objet résultat en S4, R6 ou environnement** : S4 et R6 ajoutent une machinerie que ni l'application ni le rapport ne lisent (ils lisent par `$`) ; un environnement est mutable et se copie par référence, ce qui rend `identical` et le rejeu trompeurs. Liste nue : écartée aussi, faute de contrôle d'entrée par `inherits()`.
- **Plan en `data.frame` d'étapes, en YAML ou en JSON dès maintenant** : un `data.frame` ne porte pas des paramètres de types hétérogènes ; YAML et JSON exigent un lecteur (hors R de base) et sont précisément la question reportée (point 12).
- **Transition codée par des entiers d'états** : illisible dans une fiche ; deux noms d'états se relisent.
- **Table de référence en format large** (une colonne par sexe) ou en `l_x` : le format large ne s'étend pas à l'année (v2) ni aux tables sans sexe ; `l_x` et `q_x` coexisteraient dans le code ; la forme d'origine est gardée en attribut.
- **Graine par défaut dans `simuler_portefeuille()`** : un défaut est une graine non déclarée ; la graine est toujours un argument reçu (ADR 0005, esprit du point 3).
- **Empreinte par `serialize()` / `saveRDS()`** : version de R dans l'en-tête (mesuré) ; incompatible avec #11.
- **SHA-256 réimplémenté en R, ou package `digest`** : `digest` est hors priorité `base` ; une réimplémentation en R de base est possible mais lente et n'apporte rien à l'identification ; l'algorithme étant nommé dans la fiche, le passage à SHA-256 (disponible dans `tools` à partir de R 4.5, hors de notre version) sera une décision datée.
- **`format(x, digits = 17)` ou `as.character()` pour les doubles** : `as.character` arrondit à 15 chiffres en R 4.3 ; `%.15g` ne fait pas l'aller-retour (mesuré) ; `%.17g` le fait.
- **`encodeString()` pour les chaînes** : résultat dépendant de la locale (mesuré).
- **Empreinte des données après exclusions** : l'empreinte doit identifier ce que l'utilisateur a fourni ; les exclusions sont des paramètres du plan et des décomptes du résultat.
- **`dget()` / `dput()` pour le plan et la fiche** : `dget()` évalue du code (`eval(parse())`) ; contraire à la contrainte 12.2 (c).

## Conséquences

- **Fichiers** : `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore`, `R/outils.R`, `R/construire_table.R`, `R/donnees.R`, `R/simulation.R`, `R/exposition.R`, `R/rapport.R` (fiche et empreintes de la branche), `tests/harnais.R`, les trois batteries, les scripts de contrôle (liste blanche, graphe d'appels, frontière, identité des batteries) ; `app/` (vide dans cette branche, créé par la branche d'application).
- **`CLAUDE.md`** (session principale, commit `claude:`) : « Architecture » — `R/affichage.R` → `app/affichage.R`, `app.R` → `app/app.R`, ajout de `R/outils.R` et `R/construire_table.R` à la description du package, signature `construire_table(donnees, roles, plan, provenance = NULL)` ; domaine de commit `app:` (`app/`). **ADR 0004** : mention datée au point 5 et dans « Ce que l'ADR ne règle pas » (emplacement, `affichage.R`, noms des points d'entrée, format de l'objet : réglés ici). **ADR 0005** : annotation datée du 16/10/2026 (point 12). **ADR 0003** : la représentation de la transition (5.1) répond à son point ouvert ; mention datée à prévoir. **`docs/agents/routage.md`** § 4.2 : les contrats partagés restent ceux qui y sont listés ; le présent ADR en fixe la première forme. **`CONTEXT.md`** : entrée « Recoupement » (P4).
- **Pour `actuary`** (spécifications de #9, #10, #11) : les formes des points 4 à 10 sont le cadre ; le contenu (catalogue des rôles et modalités, motifs de refus, paramètres par étape, fraction d'année, 29 février, loi vraie) lui revient.
- **Pour `coder`** : les scripts de contrôle de #8 appliquent les points 2.2, 2.3, 1.3 (frontière) et 11 ; les témoins exigés par #8 valent pour le graphe d'appels.
- **Effet sur les résultats** : aucun (aucun code n'existe). Règles durables : un changement de la représentation canonique (9.3) change l'empreinte des données de toutes les fiches, c'est un changement de résultat au sens de `CLAUDE.md` (tableau avant / après, visa d'`actuary`), et appelle l'incrément de `version_fiche` ; un changement du générateur (8.1) change tout résultat simulé (visa).
- **Tests qui protègent l'ADR** : graphe d'appels et témoins (#8) ; harnais testé (#8) ; refus par motif (#9, #11) ; `identical` à graine égale et `.Random.seed` global inchangé (#10) ; empreinte indépendante de la version de R et calculée à la main sur un petit cas (#11) ; rejeu de la fiche (#11).
- **Ce que l'ADR ne règle pas** : le format de sérialisation (point 12 ; branche `fiche-rapport`) ; les empreintes du résultat et du code (même branche) ; la signature de `suivre_experience()` ; la représentation de la surface de mortalité (v2) ; la place de l'étape `marge` dans les modules ; les méthodes S3 d'affichage (`print`, `format`) et leur côté de la frontière ; le passage éventuel à SHA-256 ; le coût de la représentation canonique au-delà de 10^6 lignes (à mesurer avec #11, puis à la volumétrie cible) ; la vérification du chargement automatique de `R/` par Shiny (à faire par `coder` à la branche d'application).

### Points nouveaux soumis à confirmation du mainteneur

1. `R/outils.R`, fichier transversal sans contenu métier, en plus des dix modules et de `R/construire_table.R` (1.2).
2. Application sous `app/` (`app/app.R`, `app/affichage.R`) et non `app.R` / `R/affichage.R` ; `CLAUDE.md` à aligner (1.3).
3. Quatrième argument facultatif `provenance = NULL` de `construire_table()` (3.1).
4. Refus = condition typée `refus_tablexp`, jamais un objet retourné (6.4).
5. Avertissements métier inscrits dans l'objet, non émis par `warning()` ; lecture de « émis » dans #9 (6.5).
6. `positionnement` autorisé à appeler `lissage` (méthode 4), sens unique (2.1).
7. Périmètre de l'empreinte des données : colonnes à rôle déclaré, clé par nom de rôle, toutes les lignes reçues dans l'ordre reçu ; règles de refus sur le codage des chaînes (9.3).
8. Champs de premier niveau du plan (`transition`, `periode`, `delai_declaration`, `graine`, `etapes`) et `etapes` en liste nommée ordonnée (5.1).

Issues : #8, #3, #9, #10, #11 ; branche `claude/socle-donnees-exposition` (PR #12).

## Annotation du 16 octobre 2026 — points nouveaux acceptés (décision M46) ; mise en œuvre de #8 : appels dynamiques interdits élargis, scripts de contrôle sous `tests/controles/`, harnais

Rédigée par `architect` ; le texte ci-dessus n'est pas réécrit.

**(a) Points nouveaux acceptés.** Le 16/10/2026, le mainteneur a accepté les points marqués **[nouveau]** (décision M46, `docs/feuille-de-route.md` § 4). Ce sont les huit points de la liste « Points nouveaux soumis à confirmation du mainteneur » :

- `R/outils.R` ;
- l'application sous `app/` ;
- l'argument `provenance` ;
- le refus par condition typée `refus_tablexp` ;
- les avertissements inscrits dans l'objet, avec la lecture de « émis » dans #9 ;
- `positionnement` autorisé à appeler `lissage` ;
- le périmètre et le codage de l'empreinte des données ;
- les champs de premier niveau du plan.

L'énumération de M46 ne nomme pas le point 8 (champs du plan) ; il est lu comme compris dans l'acceptation des « points nouveaux de l'ADR 0007 ». Le mainteneur le dira si cette lecture est contestée. Les mentions **[nouveau]** et la phrase « soumis à confirmation » du corps valent désormais historique. La relecture générale de l'ADR par le mainteneur, annoncée dans le statut, reste attendue comme pour les ADR 0002 à 0006 (Q40).

Alignements faits à cette date, en conséquence (« Conséquences » ci-dessus) :

- ADR 0003, point 5 : mention datée ;
- ADR 0004, point 5 et « Ce que l'ADR ne règle pas » : mentions datées ;
- ADR 0005, point 4 : mention datée ;
- `CLAUDE.md` (« Architecture », domaine de commit `app:`, « Déclencheurs ») : alignement en commit `claude:`.

Reste à aligner `README.md` l. 27, qui cite encore `app.R` et `R/affichage.R` (commit `docs:`, hors du périmètre d'écriture d'`architect`).

**(b) Appels dynamiques interdits : liste élargie (point 2.3).** L'implémentation de #8 et son audit ont élargi la liste du point 2.3 (`tests/controles/graphe_modules.R`, en-tête et constantes `APPELS_DYNAMIQUES`, `CONSTRUCTEURS_EVAL`, `PACKAGES_QUALIFIES`, commit `0139a16`). Sont désormais interdits dans `R/` :

- les formes **qualifiées** `base::`, `utils::` et `methods::` des appels interdits, lues comme la forme nue ;
- `do.call()` dont l'argument `what`, apparié par `match.call()` (position ou nom), n'est **ni un symbole ni une fonction anonyme** (chaîne, `paste0(...)`, etc.). Cela remplace « premier argument chaîne » ;
- `get0()` et `getExportedValue()`, en plus de `match.fun()`, `get()`, `mget()` et `getFromNamespace()` ;
- `eval()` **et `evalq()`** d'un `parse()`, `str2lang()`, `str2expression()`, **`call()` ou `as.call()`**.

Un nouvel élargissement est **en cours** dans la branche, non commité à la date de l'annotation : toute occurrence des symboles d'appel dynamique, et non leur seul appel, ainsi que `getFunction`, `asNamespace` et `getNamespace`. Il se constatera sur le commit `tests:` qui l'introduira. La règle d'interprétation est inchangée : `do.call(f, args)`, avec `f` un objet fonction désigné par un symbole, reste permis. La limite connue « fonction passée en argument » vaut toujours. L'élargissement **ne change aucun résultat** (contrôle statique).

**(c) Scripts de contrôle sous `tests/controles/`.** Les contrôles mécaniques du point 2.2 et de l'ADR 0008 sont rangés sous `tests/controles/` (commit `0139a16`) :

- `graphe_modules.R` ;
- `liste_blanche_description.R` ;
- `identite_batteries.R` (batteries de `CLAUDE.md` et de `circuit-technique.js`, chaîne `image@empreinte`) ;
- `plateforme.R` (ADR 0008, point 3).

Le **script de frontière** (point 1.3 ; ADR 0004, point 6 b) n'existe pas encore : `app/` est vide dans cette branche. Il est dû par la branche d'application.

**(d) Harnais (point 11.2), forme effective** (`tests/harnais.R`, commit `0139a16`).

- **Signature** : `echec_attendu(nom, issue, expr, motif = NULL, erreur_quelconque = FALSE)`. Une erreur qui n'est pas un `refus_tablexp` du motif donné est un `ECHEC`, sauf `erreur_quelconque = TRUE` : un défaut connu ne masque pas une panne différente. La nature de l'échec attendu (`valeur`, `refus`, `erreur_quelconque`) est décomptée par `bilan()`. Le renvoi d'issue de la forme `"#N"` reste obligatoire.
- **Garde de sortie avant bilan** : une fonction `.Last` fait sortir avec le code 1 tout processus qui a chargé le harnais et se termine avant la ligne de `bilan()`, même par `quit(status = 0)`.
- **Bibliothèque réutilisée** : `installer_tablexp()` ne réutilise la bibliothèque désignée par `TABLEXP_BIBLIOTHEQUE` que si l'**empreinte de l'arbre source** inscrite à l'installation égale celle de l'arbre courant. Cette empreinte est un MD5 par `tools::md5sum` de `DESCRIPTION`, `NAMESPACE` et `R/*`, chemins compris. Sinon le harnais réinstalle et le dit. C'est un complément au point 11.1 (une installation par batterie), qui empêche de tester un package installé périmé.
- **Contrôle de fin de batterie** : `bilan()` compte un échec si un espace de noms hors priorité `base`, autre que `tablexp`, est chargé. C'est la proposition de l'ADR 0008 (« Conséquences »), retenue par la session pour #8.

Remarque pour `audit`, sans effet sur les résultats : `empreinte_source()` trie les noms de fichiers par `sort()`, donc selon la locale, et non par `method = "radix"` comme le point 9.3. L'empreinte ne sert qu'à décider d'une réinstallation sur une même machine, mais une différence de locale entre la batterie et le sous-processus provoquerait une réinstallation superflue. Cette conséquence est une *hypothèse*, non mesurée.

**Effet sur les résultats** : aucun.

Issues : #8 ; #9 (avertissements, refus). Décisions M45, M46.

## Annotation du 17 octobre 2026 — unité d'observation déduite des rôles, attribut `parametres` des tables synthétiques, transmission des champs du plan aux étapes, segmentation en champ de premier niveau, codes de refus de l'étape `donnees` (décisions M48, M49)

Rédigée par `architect` ; le texte ci-dessus n'est pas réécrit. Elle consigne la mise en œuvre de #9 (`R/donnees.R`, commit `dc9e32a`, état mesuré à `ab437b4`), la décision M48 (b) du 16/10/2026 et la décision M49 du 17/10/2026 (`docs/feuille-de-route.md` § 4), d'après la spécification d'`actuary` versée dans #11 (commentaire 6216628746, A.1 et A.3). Les mesures citées ont été exécutées sur le package installé depuis `ab437b4` (R 4.3.3).

**(a) Point 5.2 : l'unité d'observation n'est pas un paramètre du plan (M48).** Le catalogue `donnees` du point 5.2 prévoyait un paramètre « unité d'observation (`tete` ou `contrat`) ». Ce paramètre est **retiré**. Le moteur déduit l'unité des rôles et de la transition, comme il déduit la recette (point 5.3) :

- **tête** si l'état de départ de la transition est un état de la personne (vivant → décédé) et qu'un rôle `id_tete` est déclaré ;
- **contrat** sinon : transition dont l'état de départ est un état du contrat (en cours → racheté, M46), ou transition de tête sans `id_tete`. Dans ce second cas, l'avertissement `unite_contrat` est inscrit dans l'objet (point 6.5).

Emplacements : `R/donnees.R:87-93` (constante `TRANSITIONS`, colonne `etat_de`), `:525` (déduction), `:581-586` (avertissement). Mesure sur deux lignes d'une même tête (cas F1 de la spécification) : unité `tete` pour vivant → décédé, `contrat` pour en cours → racheté, `contrat` sans `id_tete` (*vérifié*).

L'unité retenue est un **paramètre effectif** : elle figure dans `etapes$donnees$parametres$unite_observation` (point 6.1, `R/donnees.R:613`) et dans le champ `unite_observation` de la fiche (point 6.2, inchangé). Un plan qui déclare `unite_observation` est refusé comme « paramètre inconnu » (point 5.4). Aucun forçage de l'unité n'existe dans cette branche.

Raison : un paramètre d'unité serait soit redondant avec les rôles, soit en contradiction avec eux. Une tête déclarée « contrat » alors que `id_tete` est présent compterait plusieurs fois un même décès (spécification, A.1 : cas F4, 2 décès au lieu de 1). Le moteur ne devine pas, mais il n'a pas non plus à recevoir ce que les rôles établissent.

Autres précisions du contenu de la branche, apportées par la spécification (le point 5.2 lui en laissait le soin) ; les deux dernières sont confirmées par le mainteneur le 17/10/2026 (M49) :

- **étape `donnees`** : le seul paramètre propre à l'étape est `exclusions`. C'est un **vecteur de chaînes** (codes de motif sans doublon, vide explicite admis), et non une liste (`R/donnees.R:303-310`) ;
- **étape `exposition`** (#11) : la segmentation n'y figure plus (voir (c)) ; le paramètre de convention prend ses valeurs dans `plus_fine`, `dernier_anniversaire` et `age_atteint_annee` (A.3). La valeur `plus_fine` remplace le `deduite` du point 5.2 ;
- **pondération** : elle n'entre pas au catalogue de la branche, car le rôle `montant` y est refusé (`role_non_pris_en_charge`, `R/donnees.R:205` ; A.1). Elle y entrera avec E2.8.

**(b) Point 7.1 : attribut `parametres` d'une table de référence.** Aux attributs `nom`, `statut`, `source` et `forme_origine` s'ajoute **`parametres`** (`R/donnees.R:651`, `:754`) :

- une **liste non vide est requise** pour une table de statut `synthetique` (refus `table_parametres_manquants`, `:662-664`) ;
- l'attribut est **interdit** pour une table `embarquee` ou `importee` (refus `table_parametres_inattendus`, `:665-667`).

Mesures *vérifiées* : une table synthétique sans paramètres lève `table_parametres_manquants` ; une table embarquée qui en porte lève `table_parametres_inattendus` ; une table synthétique valide porte les attributs `nom statut source forme_origine parametres`.

Raison : une table synthétique n'a d'autre provenance que sa loi. Sans ses paramètres, elle ne se reconstruit pas, et la loi vraie d'un jeu simulé n'est plus consignée (`CLAUDE.md`, « Données » ; A.1, « pour une table synthétique, ses paramètres »). À l'inverse, des paramètres de loi joints à une table transcrite ou importée laisseraient croire qu'elle en dérive. Le simulateur (#10) construit sa loi Gompertz-Makeham sous cette forme et la consigne à partir de l'attribut. La modalité `ensemble` de `sexe` (constante `SEXES_TABLE`, `:638`) ne coexiste jamais avec `H` ou `F` : voir le code `table_sexes_melanges` en (e).

**(c) Transmission des champs du plan aux étapes (M48, M49).** Les champs de premier niveau `transition`, `periode` et `delai_declaration` (point 5.1, acceptés par M48) sont déclarés **une seule fois**. `construire_table()` les transmet **en arguments nommés** à chaque fonction d'étape qui en a besoin. La segmentation, sixième champ de premier niveau depuis M49 (ci-dessous), est déclarée et transmise de la même manière.

Une fonction d'étape ne reçoit **jamais le plan entier**. Son interface se réduit aux arguments qu'elle emploie. Exemple : `preparer_donnees(donnees, roles, transition, periode, delai_declaration, segmentation, exclusions)`, `R/donnees.R:478-479`.

Ce choix a deux effets :

- **Localité** : le chemin d'un champ dans le plan n'est connu que de `construire_table()` et de `valider_plan()`. Déplacer un champ ne touche ni les modules ni leurs tests, qui appellent la fonction d'étape directement (point 11.3).
- **Pas de seconde source** : une étape ne peut pas lire une période ou une segmentation différente de celle d'une autre étape.

La fonction d'étape inscrit dans `parametres` (point 6.1) les valeurs qu'elle a effectivement employées (`R/donnees.R:613-614`). Pour la transition, elle inscrit le code interne `vivant_decede` ou `en_cours_rachete` (constante `TRANSITIONS`). La représentation du plan reste `list(depart, arrivee)`, et la fiche garde le plan reçu (point 6.2).

**Emplacement de la segmentation dans le plan : sixième champ de premier niveau `segmentation` (décision M49 du mainteneur, 17/10/2026).** M48 (b) disait « segmentation déclarée une fois et transmise aux étapes », sans la placer parmi les champs de premier niveau acceptés. Le point 5.2 la plaçait parmi les paramètres de l'étape `exposition`, et la spécification aussi (A.3). Pourtant l'étape `donnees`, qui précède, en a besoin pour trois contrôles : rôle requis par la segmentation (`R/donnees.R:249-253`), motif `segment_manquant`, motif `tete_segment_incoherent`.

Le plan porte donc **six champs de premier niveau** : `transition`, `periode`, `delai_declaration`, `segmentation`, `graine`, `etapes`. `segmentation` est **requis** : c'est un vecteur de chaînes sans doublon, pris dans `sexe` et `segment`, vide explicite admis (E2.9, M16). `construire_table()` le transmet à toutes les étapes qui l'emploient. Il **sort du catalogue de l'étape `exposition`** du point 5.2, et un plan qui le déclare dans `etapes$exposition` est refusé comme « paramètre inconnu » (point 5.4).

Raisons :

- la segmentation est consommée par toutes les étapes, de `donnees` à `validation` (test d'homogénéité, E2.9). Elle est de même nature que `transition` et `periode` ;
- un plan réduit à l'étape `donnees` (contrôle de la base seule) reste défini sans défaut implicite (point 5.1).

Option écartée : **paramètre de l'étape `exposition`**, lu par `construire_table()` et transmis aussi à `donnees`. Une étape antérieure dépendrait alors d'un paramètre d'une étape postérieure. Un plan sans `exposition` n'aurait plus de segmentation, et il faudrait soit exiger la présence de l'étape `exposition`, soit admettre un défaut, ce que le point 5.1 interdit.

La décision laisse inchangées les fonctions d'étape, où la segmentation entre en argument (`preparer_donnees`, `R/donnees.R:478-479`). Elle touche seulement `valider_plan()`, `PARAMETRES_ETAPES` et l'orchestration, que #11 écrit. Le champ `segmentation` s'ajoute au plan reçu que garde la fiche (point 6.2). La décision modifie le contrat partagé du plan, sans aucun code existant à reprendre.

**(d) Codes de refus : un code par condition, quelle que soit la fonction qui la détecte (#11).** `valider_plan()` (point 5.4) **ne réimplémente pas** les contrôles des champs que `donnees` contrôle déjà. Il appelle `controler_parametres_donnees()` (`R/donnees.R:263-312`), ce que le graphe du point 2.1 permet (`construire_table` peut appeler `donnees`). De même, la validation des rôles passe par `controler_roles()` (`:179-256`, point 3.1).

Les codes sont donc **identiques par construction**, et le champ `module` du refus reste `donnees`, le module qui détecte. Ces codes sont : `transition_inconnue` (`:267`, `:272`), `periode_invalide` (`:280`), `delai_invalide` (`:286`), `periode_vide` (`:292`), `segmentation_invalide` (`:299`), `exclusions_invalides` (`:304`) et `exclusion_inconnue` (`:309`). Le refus de « rôle requis absent » de la spécification (A.3) est `role_requis_manquant` (`:251`).

Ces contrôles s'exécutent dans `valider_plan()`, **avant toute étape** (point 5.1 : plan validé entièrement avant exécution). Leur second appel, dans `preparer_donnees()` (`:480`), est conservé : il protège la fonction d'étape appelée directement, et son coût est négligeable.

`valider_plan()` n'ajoute que les codes **structurels** du plan : champ de premier niveau inconnu ou manquant, étape inconnue, en double ou hors ordre, paramètre inconnu, manquant ou hors domaine. Leurs noms sont fixés par #11 et recensés avec les autres dans la documentation, sous le contrôle de la concordance.

Les motifs cités en exemple au point 6.4 (`role_absent`, `recette_impossible`) étaient illustratifs. Les codes effectifs sont `role_requis_manquant`, `aucune_recette` (`:244`) et `ligne_invalide` (`:496`). Le refus provisoire `construction_indisponible` (`R/construire_table.R:22`) disparaît avec #11.

**(e) Codes de refus de `donnees` absents des listes fermées de la spécification (A.1).** Relevé par `grep -on 'refuser("[a-z_]*", "[a-z_]*"' R/*.R` à `ab437b4` (*vérifié*). Ces codes viennent de l'avis d'`actuary` du 16/10, mentionné en tête de la spécification versée dans #11 :

| Code | Emplacement | Condition | Raison |
|---|---|---|---|
| `colonne_ambigue` | `R/donnees.R:218` | la colonne déclarée pour un rôle porte un nom présent plusieurs fois dans `names(donnees)` | `donnees[[colonne]]` lirait la première sans le dire ; le moteur refuse au lieu de choisir |
| `base_vide` | `:484` | la base reçue n'a aucune ligne | aucune exposition n'est calculable ; mesuré sur `d[0, ]` : refus `base_vide` (*vérifié*) |
| `aucune_ligne_conservee` | `:514` | toutes les lignes sont écartées par les exclusions déclarées | une construction sur zéro ligne n'est pas un résultat ; le message nomme les règles en cause |
| `dates_hors_plage` | `:440` | (nombre de têtes) × (étendue des dates d'entrée et de sortie, en jours, + 1) ≥ 2^52, dans la fusion par tête | limite d'exactitude du calcul en `double` par `cummax` décalé. Le moteur refuse plutôt que de fusionner de façon inexacte. Pour 10^6 têtes, il faut une étendue d'environ 4,5·10^9 jours, soit 1,2·10^7 ans (calcul *vérifié*) : le seuil n'est atteint que par des dates aberrantes. C'est une **limite d'implémentation**, non un motif de ligne : elle n'est pas excluable |
| `table_sexes_melanges` | `:708` | la modalité `ensemble` coexiste avec `H` ou `F` dans la table, ou au sein d'une même année pour une table qui en a | A.1 : `ensemble` n'est « jamais mêlé à `H`/`F` » ; mesuré sur une table `H` + `ensemble` (*vérifié*) |

Deux autres codes, `donnees_mal_formees` (`:181`, entrée qui n'est pas un `data.frame`) et `roles_mal_formes` (`:186`, rôles qui ne respectent pas la forme du point 4.1), sont des refus de forme. Ils sont également hors de la liste fermée A.1. Les codes `table_*` (`:654-746`) répondent à A.1, « chaque contrôle a un message de refus nommé ». **Tous ces codes sont des refus de la branche** : la documentation les recense (règle 9, surface d'impact de `dc9e32a`).

**(f) Point 9.3 : liste fermée des libellés de type de l'en-tête `#<rôle><TAB><type>` (#11).** Le point 9.3 donne le format des valeurs par type, mais ne dit pas quel libellé l'en-tête de colonne porte, ni dans quel ordre les tests sont faits. Or un tiers qui recalcule l'empreinte doit écrire le même libellé, octet pour octet. Relevé `actuary` sur #11. Lu dans `R/rapport.R` (non commité, état de la branche au 17/10/2026), fonction `colonne_canonique()` (`:89-113`) ; l'en-tête de colonne est écrit en `:128`, dans `texte_canonique()` (`:121-132`).

Les libellés forment une **liste fermée de six chaînes ASCII**, en minuscules sauf `Date`. Les tests sont faits **dans l'ordre ci-dessous** ; le premier qui réussit fixe le libellé :

| Ordre | Libellé | Test R | Ligne |
|---|---|---|---|
| 1 | `factor` | `is.factor(x)` (un facteur ordonné, de classe `ordered`/`factor`, reçoit aussi `factor`, mesuré) | `:90` |
| 2 | `Date` | `identical(class(x), "Date")` (une classe `Date` augmentée, par exemple `c("Date", "x")`, ne passe pas ce test et elle est refusée en 3, mesuré) | `:91-97` |
| 3 | refus | `is.object(x)` : toute autre classe (`POSIXct`, `difftime`, `AsIs`…) | `:98-102` |
| 4 | `character` | `is.character(x)` | `:103` |
| 5 | `integer` | `is.integer(x)` | `:104` |
| 6 | `double` | `is.double(x)` | `:105` |
| 7 | `logical` | `is.logical(x)` | `:106-109` |
| 8 | refus | tout autre type de base (`list`, `complex`, `raw`…) | `:110-112` |

**Refus.** Un type hors de la liste est refusé avec le code **`type_non_canonique`**, module `rapport`, sans valeur par défaut ni conversion. Le message nomme le rôle et la classe (`:99-102`, condition `is.object`) ou le type de base (`:110-112`, `typeof`). Le point 9.3 attribue ce refus à `donnees`. En fait, il y a **deux barrières**. La première est `controler_roles()` (`R/donnees.R`, code `type_incompatible`), appelée par `construire_table()` (`R/construire_table.R:164`) avant le calcul des empreintes (`:170`) : par `type_conforme()` (`R/donnees.R:147-165`), elle n'admet par rôle que `character`, `factor`, `integer`, `double` à valeurs entières ou réelles selon le catalogue, et `Date`. Le refus `type_non_canonique` est la seconde barrière : il protège la définition de l'empreinte si le catalogue s'élargit. Par ailleurs, aucun type de rôle du catalogue n'admet aujourd'hui `logical` (`R/donnees.R:43-47`). Le libellé `logical` existe dans la définition (point 9.3), mais `construire_table()` ne le produit pas tant que le catalogue n'admet pas de rôle logique (*hypothèse* tirée de la lecture du catalogue, non mesurée par un appel de `construire_table()`).

Mesure, *vérifiée* : sous R 4.3.3, après `sys.source()` de `R/outils.R` et `R/rapport.R`, `colonne_canonique()` rend `double`, `integer`, `logical`, `Date`, `character`, `factor` sur les six types admis. Elle rend `factor` sur un facteur ordonné. Elle refuse `POSIXct`, `difftime`, `AsIs`, `Date`/`x`, `list`, `complex` et `raw` avec la condition de classe `refus_tablexp`.

**Pourquoi `factor` reste distinct de `character`** (avis d'`actuary`, #11). Les valeurs s'écrivent de la même façon (point 9.3 : le facteur est converti en chaînes, ses niveaux n'entrent pas). Seul le libellé distingue donc une colonne `factor` d'une colonne `character` aux mêmes valeurs. On le garde, parce que l'empreinte **identifie l'entrée, pas le résultat** (point 9.3, périmètre) : deux `data.frame` dont une colonne n'a pas la même classe sont deux entrées différentes, et la fiche doit permettre à un tiers de dire laquelle a été reçue. Un seul libellé ferait confondre les deux entrées. Ce choix ne fait pas dépendre l'empreinte des niveaux. Mesuré, *vérifié* : `factor(c("x","y"))` et le même facteur aux niveaux `c("y","x","w")` ont la même empreinte (`b5f64b38…`), distincte de celle de `c("x","y")` en `character` (`5ca89468…`). Cette indépendance aux niveaux n'est sûre que si **aucun résultat ne dépend des niveaux**. C'est l'objet de la correction en cours, demandée par `actuary` : l'ordre et la classe des clés de segmentation de la table d'exposition ne doivent plus dépendre des niveaux d'un facteur. Tant qu'elle n'est pas faite, deux entrées de même empreinte peuvent donner des tables d'exposition ordonnées différemment (*hypothèse*, non mesurée ici).

**Règle durable.** Ajouter, retirer ou renommer un libellé, ou changer l'ordre des tests, change la représentation canonique. C'est donc un changement de résultat qui appelle l'incrément de `version_fiche` (Conséquences, « Effet sur les résultats »). Le test « empreinte calculée à la main » du point 9.3 doit couvrir au moins une colonne `factor` et une colonne `character`.

**Effet sur les résultats** : aucun. L'annotation consigne l'état du code de `dc9e32a` et des règles de forme pour #11. Elle ne modifie aucune valeur calculée.

Issues : #9, #11 ; décisions M48, M49. Aucun point ouvert.
