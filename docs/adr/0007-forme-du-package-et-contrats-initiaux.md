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

## Annotation du 17 octobre 2026 (seconde) — locale d'installation UTF-8 et sonde du harnais, chargement des tests en UTF-8 (décision M54)

Rédigée par `architect` d'après la décision du mainteneur du 17/10/2026 (M54, `docs/feuille-de-route.md`, § 4), qui retient la recommandation d'`architect`, et d'après l'audit de l'outillage de #17 du même jour. Le texte ci-dessus, annotations comprises, n'est pas réécrit ; la présente annotation complète les points 11.1 et 9.3.

**(a) Point 11.1 : installation sous une locale UTF-8, contrôlée par une sonde.**

- Le harnais (`installer_tablexp()`, `tests/harnais.R`) installe `tablexp` sous une locale UTF-8, prise dans l'ordre `C.UTF-8`, `C.utf8`, `en_US.UTF-8` (`LOCALES_UTF8`). Chaque locale est testée, puis la première disponible est passée à `R CMD INSTALL` par `system2(env = "LC_ALL=…")`.
- Une **sonde** lit le littéral non ASCII `STATUT_EMPREINTES_NON_CALCULEES` (`R/rapport.R:34`) dans l'espace de noms installé. Si l'installation a été faite hors UTF-8, la batterie échoue.
- La **locale d'exécution reste libre** : les batteries tournent sous la locale de la session.

**(b) Extension rendue nécessaire par la mesure.**

- Les fichiers de test sont chargés par `parse(encoding = "UTF-8")` puis `eval`, et non plus par `source()`. Sans cela, 16 échecs apparaissent en session C : les messages du moteur sont marqués UTF-8, les littéraux des tests ne le sont pas, et les comparaisons de chaînes échouent.
- Les recherches dans les sorties de sous-processus se font en octets (`useBytes = TRUE`, 26 appels selon le décompte de l'audit).
- Mesures de l'audit : les décomptes d'assertions sont identiques, fichier par fichier, entre l'ancien et le nouveau chargeur (900 au total) ; avec `fixed = TRUE`, `useBytes` ne peut que rendre une recherche plus stricte, jamais en faire réussir une qui échouait.

**(c) Point 9.3 : la règle de refus des chaînes indéterminées vaut aussi pour l'outillage des tests.** Une chaîne non ASCII non marquée, dans une session dont la locale n'est ni UTF-8 ni latin-1, est refusée par l'outillage de `tests/` comme par le moteur, et jamais convertie par `enc2utf8()`. L'outillage des références de non-régression suit la règle d'`octets_chaines()` (`R/rapport.R:41`) ; voir ADR 0009, troisième annotation, (e).

**(d) Limite admise, mesurée.** La sonde laisse passer une installation faite en C si la session est en UTF-8 : le littéral se lit alors correctement. C'est sans effet : R1 produit sous C et sous `C.UTF-8` a le même MD5 (`7b0f55f1fe2b7265d21548f493973c7b`, mesure de l'audit).

**Hors périmètre** : une garde au chargement du package (`.onLoad`) qui refuserait un `tablexp` installé hors UTF-8 quel que soit le harnais ; issue #25.

**Effet sur les résultats** : aucun. L'annotation porte sur l'installation et le chargement des tests ; l'objet du moteur a les mêmes octets dans les quatre combinaisons installation × exécution (ADR 0009, troisième annotation, (e)).

Issues : #17, #25. Décision M54.

## Annotation du 19 octobre 2026 — forme exacte des valeurs du plan (#23) et de la provenance (#21) ; locale d'installation posée par `Sys.setenv` (PR #41)

Rédigée par `architect` ; le texte ci-dessus, annotations comprises, n'est pas réécrit. Elle consigne l'état du code à `a311c85` (branche `claude/consolidation-controles-references`) : commits `1537475` et `406cd74` (#23), tests `f04355d` (#23), commit `a311c85` (#21) ; et, pour le harnais, la PR #41 (`99c7e3f`, fusion `badaf91`). Elle complète les points 3.1, 5.1, 5.4 et 11.1, ainsi que la seconde annotation du 17/10/2026. Les mesures citées ont été exécutées sous R 4.3.3, sur `tablexp` installé depuis `a311c85` et depuis `4f182e3` (état avant #23).

**(a) Points 5.1 et 5.4 : contrat « format du plan » précisé, pas changé (#23).** Le point 5.1 fixait les champs et leur nature, sans dire si une valeur pouvait porter des attributs. La règle est désormais la suivante.

- Toute valeur du plan a exactement le **type de base** (`typeof`) que son contrôle attend. Elle ne porte **aucun attribut** : ni `names`, ni `dim`, ni classe, ni attribut quelconque (`comment` compris).
- **Exceptions.**
  - Les listes du plan portent leur seul attribut `names` : le plan, `transition`, `periode`, `etapes`, et la liste des paramètres de chaque étape. Leurs noms sont uniques, non vides et non `NA`. Il y a donc autant d'éléments que de noms.
  - Les noms de `transition`, de `periode` et de chaque liste de paramètres d'étape sont exactement les noms attendus. Les noms du plan sont les champs du point 5.1, `graine` selon l'aléa (annotation du 17/10/2026, (c)). Les noms d'`etapes` sont des étapes connues, chacune au plus une fois, dans l'ordre des modules (point 5.1).
  - Les bornes de `periode` portent leur seul attribut `class = "Date"`, exactement cette classe, stockage double ou integer.
- **Types.**
  - `delai_declaration` : integer ou double.
  - character : `segmentation`, `exclusions`, `convention`, `estimateurs` et les deux états de `transition`.
  - `par_annee` : logical.
  - `niveau` : double (un integer est refusé, comme avant #23).
  - La colonne `type` de `PARAMETRES_ETAPES` porte ces types.
- **Refus.** Une valeur hors de cette forme est refusée sous le **motif propre au paramètre**, et **aucun motif n'est créé**. Pour les valeurs, ce sont `transition_inconnue`, `periode_invalide`, `delai_invalide`, `segmentation_invalide`, `exclusions_invalides`, `convention_invalide`, `par_annee_invalide`, `estimateurs_invalides` et `niveau_invalide`. Pour les listes, ce sont `plan_mal_forme`, `etapes_mal_formees` et `parametres_mal_formes`. La forme est contrôlée avant la valeur. Exemple : `exclusions = c(a = "zzz")` passe de `exclusion_inconnue` à `exclusions_invalides`.
- **Utilitaire.** La règle s'écrit par un utilitaire sans contenu métier, `forme_exacte(x, types, attributs = character(0))` (`R/outils.R:60-62`). Il rend `TRUE` si `typeof(x)` est dans `types` et si `x` ne porte aucun attribut hors de `attributs`. Il ne contrôle ni la présence ni la valeur des attributs admis : les noms et la classe sont contrôlés par l'appelant, sous son propre motif. Emplois :
  - `R/donnees.R:273-326` : `chaine`, `noms_exacts`, `date1`, délai, segmentation, exclusions ;
  - `R/exposition.R:81`, `:97` ;
  - `R/brut.R:167`, `:181` ;
  - `R/construire_table.R:71` (`liste_nommee`) et `:98` (`etapes`).
  - `R/outils.R` n'appelle toujours aucun module (point 2.1).
- **Garde de longueur** (`406cd74`). `noms_exacts()` exige `length(x) == length(noms)`. En effet, `sort()` retire un nom `NA`, si bien qu'une `transition` ou une `periode` portant un élément de plus nommé `NA` passait le contrôle, et la fiche la consignait.

Raison : la fiche garde le **plan reçu** (point 6.2). Un attribut y entrerait sans être une donnée de la construction. De plus, une dimension pouvait produire des erreurs R hors de `refuser()` (point 6.4).

Mesures, *vérifiées* (`4f182e3` → `a311c85`, appel de `valider_plan()`) :

| Valeur | Avant | Après |
|---|---|---|
| `transition$depart` en `matrix` | erreur R « dims [product 1] do not match the length of object [2] » | `transition_inconnue` |
| `transition` en `data.frame` | admis | `transition_inconnue` |
| `transition` avec un élément de plus nommé `NA` | admis | `transition_inconnue` |
| borne de `periode` nommée | admis | `periode_invalide` |
| `segmentation = c(a = "sexe")` | admis | `segmentation_invalide` |
| `par_annee` à attribut `comment` | admis | `par_annee_invalide` |
| plan à classe | admis | `plan_mal_forme` |
| `etapes` à attribut | admis | `etapes_mal_formees` |
| liste de paramètres d'étape à classe | admis | `parametres_mal_formes` |
| borne de `periode` en Date à stockage integer ; `delai_declaration` integer | admis | admis |

L'ensemble des codes de refus de `R/` est identique avant et après : 74 motifs distincts, soit 76 paires (module, motif), `aucune_recette` (`donnees`, `exposition`) et `periode_invalide` (`donnees`, `simulation`) étant émis par deux modules chacun. Relevé des arguments littéraux des appels `refuser()`, `refus_tablexp()` et `refus_plan()` dans les jetons de `getParseData(parse(text = git show <sha>:R/<fichier>))`, pour chaque fichier de `R/`, à `4f182e3` et à `a311c85` (*vérifié* le 19/10/2026 ; ensembles identiques). Le même relevé donne 75 motifs et 77 paires depuis `5bf62e6` (#37), qui ajoute `trop_de_cellules` (`exposition`).

**Effet sur les résultats** : aucun pour un plan valide. `tests/test_reproductibilite.R` à `a311c85` donne 122 tests, 0 échec, comparaison à R1 à R6 OK. Les objets R1 à R6 sont `identical` hors `execution` (mesure de l'audit, messages de `1537475` et `406cd74`). `tests/reference/` est inchangé (`git diff --stat 4f182e3 a311c85 -- tests/reference`, vide). Les refus nouveaux sont des **changements de comportement**, non de résultat.

**(b) Point 3.1 : forme exacte de l'argument `provenance` (#21, `a311c85`).** Le point 3.1 disait « le nom du fichier lu, son empreinte et l'algorithme », sans en fixer la forme. `provenance` est soit `NULL`, soit une **liste nue** qui obéit aux règles suivantes (`controler_provenance()`, `R/rapport.R:151-164`) :

- elle ne porte que l'attribut `names` ;
- elle a exactement trois éléments, nommés exactement `fichier`, `empreinte` et `algorithme`, dans un ordre quelconque ;
- chaque élément est une chaîne de longueur 1, sans attribut, non `NA` et non vide.

Sont donc refusés : un `data.frame`, un objet de classe, une liste à attribut autre que `names`, une chaîne nommée ou à attribut, et trois noms attendus plus un nom `NA` (garde `length(provenance) == 3`, même défaut de `sort()` qu'en (a)). Le motif reste `provenance_invalide`, et aucun motif n'est créé.

Mesures, *vérifiées* (`4f182e3` → `a311c85`, appel de `construire_table()`) :

| Valeur | Avant | Après |
|---|---|---|
| `data.frame` | admis | refusé |
| objet de classe | admis | refusé |
| trois noms attendus plus un nom `NA` | admis | refusé |
| chaîne nommée | admis | refusé |
| chaîne vide, `NA`, longueur 2 | refusé | refusé |
| provenance valide | admis | admis |

**Effet sur les résultats** : aucun pour une provenance valide ou `NULL` (`identical` hors `execution`, mesure de `coder` et d'`audit`, message de `a311c85`).

**(c) Point 11.1 et seconde annotation du 17/10/2026, (a) : la locale d'installation est posée par `Sys.setenv`, non par `system2(env =)` (constat B de l'audit de la PR #41, décision du mainteneur du 19/10/2026).** La seconde annotation du 17/10/2026, (a), écrit que la locale retenue « est passée à `R CMD INSTALL` par `system2(env = "LC_ALL=…")` ». M54 (`docs/feuille-de-route.md`, § 4) dit de même. Ce n'est plus exact depuis la PR #41 (`99c7e3f`, fusion `badaf91`), parce que `system2(env =)` est sans effet sous Windows.

- `installer_tablexp()` (`tests/harnais.R:322`) pose `LC_ALL` dans le processus par `Sys.setenv`, le temps de l'installation. Elle le restaure ensuite, y compris en cas d'erreur : `poser_environnement()` (`:247-258`) rend la fonction de restauration, appelée en `finally` (`:344-349`). Le sous-processus `R CMD INSTALL` hérite de la variable. C'est la mécanique d'`executer_rscript()`.
- La liste des locales candidates, `LOCALES_UTF8` (`:294`), est inchangée : `C.UTF-8`, `C.utf8`, `en_US.UTF-8`.
- La locale retenue est la première que `locale_utf8_disponible()` (`:299-304`) fait accepter par `Sys.setlocale("LC_CTYPE", …)`, avant de restaurer la locale. Aucune n'est posée si la session est déjà en UTF-8 (`locale_installation()`). Si aucune n'est disponible, la batterie échoue (`:336-339`).
- La sonde de l'installation (`sonde_utf8()`, M54) est inchangée.

Avant la PR #41, `tests/harnais.R` passait la locale par `env =` (`git show 99c7e3f^:tests/harnais.R`, l. 329). **Effet sur les résultats** : aucun. `R/` et `tests/reference/` sont inchangés par la PR #41 (message de `99c7e3f`). Le comportement sous Windows n'est pas mesuré : #33 et #34 restent ouvertes.

**(d) Points ouverts** (renvois, sans décision) :

- **#42**. Le **type de stockage** d'une borne de `periode` (double ou integer) et celui de `delai_declaration` sont admis tous deux à l'entrée. Ils sont consignés tels quels dans la fiche, si bien que deux plans de même sens donnent deux fiches. De même, mesuré à `a311c85` et *vérifié* : l'**ordre des noms** du plan, de `transition`, de `periode` et de la provenance est consigné tel qu'il est reçu. Un plan et une provenance aux noms permutés donnent les mêmes `etapes` mais des fiches non `identical`. Ce second fait n'est pas dans le corps de #42 ; il est à y verser. Dans les deux cas, la question est de savoir si la fiche canonise. Elle relève du contrat de sérialisation (point 12.2 ; branche `claude/fiche-rapport`, avec #19 et #35).
- **#43**. La forme exacte reste à étendre hors du plan : une graine nommée ou en `matrix` est admise par `sous_graine()`, et `calculer_taux_bruts()` appelée directement ne contrôle pas la forme de `segmentation`. La graine du plan est aujourd'hui toujours refusée (`graine_inattendue`, aucune étape aléatoire), et sa forme n'est pas couverte par (a).

**Note sur les emplacements cités.** L'annotation du 17/10/2026 cite des numéros de ligne de `R/donnees.R` mesurés à `ab437b4`. Exemples : en (d), `controler_parametres_donnees()` en `:263-312`, `controler_roles()` en `:179-256`, et les lignes des codes de refus ; en (a) et (c), `preparer_donnees()` en `:478-479`. Ces numéros ne correspondent plus au code : à `4f182e3`, avant #23, les fonctions commencent déjà en `:267`, `:186` et `:492`, et à `a311c85` en `:271`, `:186` et `:503`. Ils ne sont pas corrigés ici, car un ADR accepté ne se réécrit pas. Les noms de fonctions et de codes font foi, et les lignes valent pour le commit cité.

Issues : #21, #23 ; #33, #34 (PR #41) ; points ouverts #42, #43. Décision du mainteneur du 19/10/2026 (constat B de l'audit de la PR #41).

## Annotation du 19 octobre 2026 (seconde) — transmission entre étapes : fenêtres de l'étape `exposition` passées à l'étape `brut` hors de l'objet résultat (#38, M63)

Rédigée par `architect` ; le texte ci-dessus, annotations comprises, n'est pas réécrit. C'est la seule annotation d'ADR admise sur la branche `claude/exposition-brut-fenetres` (P2, PR #53 ; décision M63, `docs/feuille-de-route.md`, § 1.4.3). Elle consigne l'état du code au commit `858b359` (#38). La tête de branche `a733dae`, fusion de `main` (PR #55), ne modifie pas `R/` (`git diff --stat 858b359 a733dae` : `tests/test_reproductibilite.R` seul). Elle complète les points 2.1, 6.1 et 6.3, et l'annotation du 17/10/2026, (c). Les numéros de ligne valent pour `858b359`.

**(a) Ce qui est transmis.** `calculer_exposition()` rend, à côté de `parametres`, `resultat`, `diagnostics` et `avertissements`, un cinquième élément `fenetres` (`R/exposition.R:486-489`). C'est un objet de classe S3 `fenetres_exposition`, liste qui contient :

- les fenêtres des observations (`composantes`, `calendrier`, `a`, `b`, `evenement`, de `fenetres_observations()`) et leurs segments (`g`, `segments`, de `segments_observations()`), sans copie des vecteurs ;
- le début D (`debut`) et la borne B (`borne`) de la période ;
- les paramètres sous lesquels l'objet est calculé : le code de la transition (`transition`), la convention retenue (`convention`) et la segmentation (`segmentation`).

**(b) Comment.** `construire_table()` place l'objet dans une variable locale de la boucle des étapes (`fenetres`, `R/construire_table.R:232`). Elle le passe à `calculer_taux_bruts()` par l'argument existant `observations` (`:220`) ; aucune signature ne change. Elle le libère après l'étape `brut` (`:233`). L'étape `exposition` précède toujours l'étape `brut` : `valider_plan()` contrôle cette dépendance (point 5.4).

Seuls `parametres`, `resultat` et `diagnostics` entrent dans `etapes` (`:226`, inchangé). Le point 6.1 et le contrat de lecture 6.3 sont donc **inchangés** : l'élément `fenetres` ne figure ni dans l'objet résultat ni dans la fiche (point 6.2). Le test « fenêtres transmises » le vérifie en cherchant la classe `fenetres_exposition` dans tout l'objet (`tests/unitaires/test_brut_kaplan_meier.R`, section 8).

**(c) Côté destinataire.** `taux_kaplan_meier()` (`R/brut.R:299-312`) accepte deux entrées :

- un objet `fenetres_exposition` : elle le reprend tel quel après avoir contrôlé à l'**égalité exacte** (`identical()`) les cinq paramètres de (a) (début, borne, transition, convention, segmentation). Un écart arrête l'étape comme « erreur de programmation ». Elle **ne contrôle pas les observations elles-mêmes**, puisque seul `construire_table()` produit l'objet, à partir des observations de l'étape `donnees` de la même exécution ;
- un `data.frame` d'observations (appel direct de la fonction d'étape, point 11.3) : elle recalcule les fenêtres et les segments par les mêmes fonctions du module `exposition`.

Les deux voies donnent des résultats `identical()` (test « fenêtres transmises », section 8). Un test couvre chacun des cinq contrôles, plus un témoin sans arrêt.

**(d) Ce qui n'est pas transmis : les positions dans la cellule.** L'avis de forme d'`architect` (`docs/feuille-de-route.md`, § 3, ligne #38) prévoyait de transmettre aussi les positions dans la cellule. Le code ne le fait pas : `taux_kaplan_meier()` les calcule une fois (`position_dans_cellule()`, `R/brut.R`), à partir des fenêtres transmises. `architect` a accepté cet écart le 19/10/2026, pour deux raisons :

- seul Kaplan-Meier emploie ces positions. Les calculer dans `exposition` coûterait à chaque construction, y compris pour les plans R1 à R5, qui n'ont pas cet estimateur ;
- le module `exposition` n'a pas à connaître les besoins d'un estimateur du module `brut`. Transmettre les positions l'y obligerait (couplage `exposition` → estimateurs).

**(e) Appariement par clé.** La table d'exposition est appariée par clé (segment, âge), sans hypothèse sur l'ordre de ses lignes (`R/brut.R`, `match()` sur le début de cellule ; sommes cumulées par plage contiguë d'instants). Une clé en double ou un instant hors des cellules arrête l'étape. Le test « ordre des lignes » applique trois permutations de la table d'exposition (l'inverse et deux tirages sous graine locale), par les deux voies de (c).

**(f) Graphe d'appels inchangé (point 2.1).** `brut` appelait déjà `fenetres_observations()`, `segments_observations()`, `extraire_composantes()` et `position_dans_cellule()` du module `exposition` avant `858b359` (lignes retirées du diff de `R/brut.R`). Aucun arc n'est ajouté. `Rscript tests/controles/graphe_modules.R` à `a733dae` : « OK : graphe d'appels conforme à l'ADR 0007, aucun appel dynamique interdit » (*vérifié* le 19/10/2026).

**(g) Parallèle avec M49.** Le cas est le même que celui de la segmentation (annotation du 17/10/2026, (c)). Une donnée produite ou déclarée une fois est transmise en argument nommé par `construire_table()`, seule à connaître l'enchaînement des étapes. Aucune étape ne la recalcule ni n'en lit une seconde source. La différence est que la donnée vient ici d'une étape, et non du plan ; elle n'entre donc pas dans la fiche, qui garde le plan reçu.

**(h) Remarque, sans condition : le nom de l'argument `observations`.** L'argument `observations` de `calculer_taux_bruts()` et de `taux_kaplan_meier()` reçoit désormais deux types : un `fenetres_exposition` ou un `data.frame`. Son nom ne décrit plus exactement le premier. Le garder évite de changer les signatures ; un renommage éventuel se traitera avec la table étape → fonction (#22, point 7).

**(i) Mesures**, toutes tirées du message du commit `858b359` (*non remesurées ici*) :

- **Résultats** : R1 à R6 et empreintes identiques (`tests/test_reproductibilite.R`, 140 tests, 0 échec, sans régénération). L'objet résultat est `identical()` avant / après sur `jeu_reference()` (R1 à R6, 10⁵ lignes), sur le simulateur à 10⁶ lignes (R6) et sur 216 scénarios adverses de l'audit.
- **Durée** (n = 10⁶, plan R6, médianes de 5, machine à 1 cœur, R 4.3.3 ; scripts de mesure de `coder-approfondi`, confirmés par `audit`) : étape `brut` 0,86 s → 0,50 s (audit : 0,862 → 0,474) ; `construire_table()` 10,05 s → 9,11 s ; `calculer_exposition()` inchangée (2,62-2,71 s → 2,61-2,72 s).
- **Ordre des lignes** : avant, une permutation des lignes de la table d'exposition arrêtait l'étape `brut` (« instant d'événement hors des cellules… (erreur de programmation) ») ; après, la table `kaplan_meier` est `identical()` à l'originale permutée.

**Effet sur les résultats** : aucun. Le changement porte sur le chemin des données entre deux étapes ; aucune valeur calculée ne change, et `tests/reference/` est inchangé.

Issues : #38 ; #22 (point 7). Décision M63.

## Annotation du 19 octobre 2026 (troisième) — point 12 remplacé par l'ADR 0011 ; mention « mêmes `etapes` » de l'annotation du 19/10/2026, (d), inexacte pour `periode` ; point 6.2 précisé (plan canonique)

Rédigée par `architect-approfondi` (Fable, consultation 3/3 de la branche B4, M62, Q3) ; intitulée « seconde » à sa rédaction (`1f785b0`), renumérotée « troisième » le 19/10/2026 (décision du mainteneur M65), la seconde annotation du même jour étant celle de la branche P2 (PR #53, `ad6bc43`). Statut : **proposé, sous réserve de l'acceptation de l'ADR 0011** par le mainteneur. Le texte ci-dessus, annotations comprises, n'est pas réécrit.

1. **Point 12 remplacé.** Le format de sérialisation du plan et de la fiche, reporté par le point 12.1, est fixé par l'**ADR 0011** (points 6, 8 et 9 : grammaire de ligne aplatie, octets UTF-8 et LF chez l'appelant, module `rapport`, lecteur sans évaluation, `lire_plan()` sans `valider_plan()`). Les cinq contraintes du point 12.2 sont **maintenues** telles quelles par l'ADR 0011 (point 6.4 pour (b), 6.2 pour (c), 10 pour (e)) ; le point 12 ne fait plus foi que par leur intermédiaire.

2. **Points 9.2 et 9.3 complétés, non réécrits.** L'écriture des doubles de la représentation canonique (`sprintf("%.17g", x + 0)`, point 9.3) est remplacée, pour l'**empreinte**, par les octets IEEE 754 petit-boutistes en hexadécimal avec jetons pour les non-finis (ADR 0011, point 2), et l'année d'une `Date` s'écrit sur quatre chiffres dans [0001, 9999], hors domaine refusée (point 4) ; la première ligne devient `tablexp canonique 2`. Le `%.17g` subsiste comme **forme texte** (point 3). L'empreinte du résultat (point 9.2, « non calculée ») reçoit son principe et son périmètre (ADR 0011, point 11), à calculer en cinquième branche.

3. **Annotation du 19/10/2026, (d), #42 : « les mêmes `etapes` » est inexact pour `periode`.** Elle écrit qu'« un plan et une provenance aux noms permutés donnent les mêmes `etapes` mais des fiches non `identical` ». *Vérifié* le 19/10/2026 (script `m7.R` d'`actuary`, remesuré par `architect`, 10⁴ lignes, plan R1, `periode` reçue dans l'ordre `fin`, `debut`) : `etapes$exposition$parametres$periode` porte les noms dans l'ordre reçu, et `etapes` n'est **pas** `identical`. La phrase reste vraie pour `transition` et pour la provenance. C'est une raison de plus de canoniser le plan **à l'entrée** de `construire_table()` et non dans la seule fiche (ADR 0011, point 5.1).

4. **Point 6.2 précisé** : `fiche$plan` est le plan reçu **mis sous forme canonique** (ordre du schéma, bornes de `periode` en `Date` à stockage double, `delai_declaration` en double, paramètres d'étape dans l'ordre du catalogue), sans autre changement ; c'est lui qu'on rejoue. Le « plan effectif » d'E4.5 est la paire (plan canonique, paramètres retenus) : ADR 0011, point 5.2 ; ADR 0005, annotation du même jour. `version_fiche` passe de 1 à 2.

**Effet sur les résultats** : aucun par cette annotation ; ceux de l'ADR 0011 sont consignés dans ses « Conséquences » (tableau attendu : `fiche/version_fiche`, `fiche/empreintes/representation`, `fiche/empreintes/donnees` ; retraits si la question 2 de l'ADR 0011 est acceptée).

Issues : #35, #19, #42, #22 (point 6), #50 ; ADR 0011.

## Annotation du 20 octobre 2026 — seconde annotation du 19/10/2026, (h) : renvoi du renommage de l'argument `observations` à #56

Rédigée par `architect` au point d'étape qui suit la fusion de la PR #51 (décision M68 ; `docs/feuille-de-route.md`, § 1.5), commit (0) de la branche `claude/fiche-rapport`, partie de `main` à `2146717`. Le texte ci-dessus, annotations comprises, n'est pas réécrit.

**(a) Constat.** La seconde annotation du 19/10/2026, (h), renvoie le renommage éventuel de l'argument `observations` à « la table étape → fonction (#22, point 7) ». Or ce point a été clos par constat, sans code, sur décision du mainteneur (branche P2, 19/10/2026 ; `docs/feuille-de-route.md`, § 3, ligne #22). Le renvoi ne mène donc plus à rien, comme le relève #56. *Vérifié* sur `2146717` : l'argument s'appelle toujours `observations` dans `taux_kaplan_meier()` (`R/brut.R:284`) et dans `calculer_taux_bruts()` (`R/brut.R:481`, défaut `NULL`) ; `construire_table()` lui passe l'objet `fenetres` (`R/construire_table.R:223`) ; aucune des deux fonctions n'est exportée (`NAMESPACE`, 0 occurrence).

**(b) Renvoi.** Le renommage éventuel est renvoyé à #56, rattachée à la branche `claude/forme-exacte-suite` (M68, C.2). Cette issue porte déjà les commentaires devenus inexacts sur le même argument (`R/brut.R:267`, `:279` ; `R/construire_table.R:30-32`). Deux issues sont admises :
1. ou bien l'argument garde son nom, et les commentaires corrigés par #56 disent qu'il reçoit soit un objet `fenetres_exposition`, soit un `data.frame` ;
2. ou bien il est renommé dans le même commit que ces commentaires.

Les deux fonctions étant internes, un renommage ne touche aucun contrat partagé (`docs/agents/routage.md`, § 4.2) : la signature de `construire_table()` est inchangée. Il touche seulement ses appelants internes et les tests. Le choix revient à `coder`, avec l'audit léger de #56.

**Effet sur les résultats : aucun.** Rien n'est modifié dans le code par cette annotation.

Issues : #56 ; #38 ; #22 (point 7, clos). Décision M68.

## Annotation du 20 octobre 2026 (seconde) — renvois périmés au code après #67 et #22, point 1 (constat m6 de la revue finale de `claude/fiche-rapport`)

Rédigée par `architect` en fin de branche `claude/fiche-rapport` (PR #68, à `63b4bc5`), d'après le constat mineur m6 de la revue finale d'`audit` (effort `high`). Le texte ci-dessus, annotations comprises, n'est pas réécrit : la présente annotation dit seulement où se lisent aujourd'hui les objets que trois passages désignent.

**(a) Seconde annotation du 17/10/2026, (a), deuxième tiret** : la sonde UTF-8 du harnais ne lit plus `STATUT_EMPREINTES_NON_CALCULEES` (`R/rapport.R:34`). Cette constante a été retirée par le commit de #67 (`5422404`) : les empreintes du résultat et du code sont désormais calculées. La sonde lit le littéral non ASCII `STATUT_EMPREINTE_RESULTAT`, défini en `R/rapport.R:68` à `63b4bc5` (`tests/harnais.R:313-320` et `:370` ; critère « sonde UTF-8 reportée » de #67, `docs/feuille-de-route.md`, § 1.5). Le principe de la sonde, son contrôle et sa limite admise ((d)) sont inchangés.

**(b) Même annotation, (c)** : `octets_chaines()` n'est plus en `R/rapport.R:41`. Elle est définie en `R/rapport.R:106` à `63b4bc5` (en `:57` à `2146717`). Depuis #22, point 1 (`b1ed03b`), elle écrit les chaînes par blocs bornés par `bornes_blocs()` (`R/rapport.R:83`). Les octets écrits sont identiques à l'écriture précédente (mesure du commit) ; la règle de refus des chaînes indéterminées, que suit l'outillage des tests, est inchangée.

**(c) Point 9.2, `resultat` et `code`** : le texte « à `NA` dans cette branche avec le statut « non calculée (branche `fiche-rapport`) » » décrit l'état de la première branche. Ce report a pris fin avec #67 (ADR 0011, point 11, et annotations du 20/10/2026, cinquième à huitième). Les deux champs sont calculés. Leur statut porte les libellés de M71, point 3 (`statut_empreintes()`, `R/rapport.R:359`). Un champ `representation_resultat` s'ajoute (`fiche$empreintes`, neuf champs ; `tests/reference/R1.tsv:191`).

**Mesures** (`architect`, 20/10/2026, *vérifié*) :
- `grep -n 'STATUT_EMPREINTE\|^octets_chaines\|^bornes_blocs\|^statut_empreintes' R/rapport.R` à `63b4bc5` rend les lignes 68, 106, 83 et 359 ;
- `git show 2146717:R/rapport.R` rend `STATUT_EMPREINTES_NON_CALCULEES` en ligne 50 et `octets_chaines` en ligne 57 ;
- `git log -S'STATUT_EMPREINTES_NON_CALCULEES <-' -- R/rapport.R` rend `5422404` (retrait).

Les numéros de ligne cités ici datent de `63b4bc5` et vieilliront à leur tour : le nom de l'objet fait foi.

**Effet sur les résultats : aucun.** Aucun code n'est modifié par cette annotation.

Issues : #67 ; #22 (point 1). Décision M54 (sonde) ; M71, point 3 (libellés). Branche `claude/fiche-rapport`, PR #68.

## Annotation du 20 octobre 2026 (troisième) — seconde annotation du 19/10/2026, (h) : issue retenue par #56, l'argument `observations` garde son nom

Rédigée par `architect-approfondi` en fin de branche `claude/forme-exacte-suite` (PR #75, à `1acc6e8`). Les numéros de ligne cités ici datent de `1acc6e8` ; le nom de l'objet fait foi (gabarit des ADR, convention du 20/10/2026, M77, (b)). Le texte ci-dessus, annotations comprises, n'est pas réécrit.

**(a) Constat.** La seconde annotation du 19/10/2026, (h), renvoyait un renommage éventuel de l'argument `observations` à « la table étape → fonction (#22, point 7) ». Ce point a été clos par constat, sans code (P2). L'annotation du 20/10/2026 (première), (b), a donc renvoyé le choix à #56, avec deux issues admises, au choix de `coder`. `coder` l'a relevé de nouveau sur #56 (message de `5a6dd1b`, « Non fait ») : le renvoi « #22 pt 7 » de (h) est périmé, et (h) reste muet sur l'issue retenue.

**(b) Issue retenue : la première de l'annotation du 20/10/2026, (b).** L'argument garde son nom ; aucune signature ne change. Les commentaires corrigés par #56 (`5a6dd1b`) disent les deux types reçus. *Vérifié* sur `1acc6e8` (`git show HEAD:R/brut.R`, `git show HEAD:NAMESPACE`) :
- en-tête de `taux_kaplan_meier()`, `R/brut.R:268-271` : « fenêtres transmises par l'étape `exposition` (objet fenetres_exposition, construire_table(), #38) ou, en appel direct, observations de l'étape `donnees` (data.frame) » ;
- en-tête de `calculer_taux_bruts()`, `R/brut.R:477-478` : même lecture ;
- signatures inchangées, `R/brut.R:293` et `:490` ;
- `construire_table()` passe toujours `observations = fenetres` (`R/construire_table.R:244`) ;
- aucune des deux fonctions n'est exportée (0 occurrence dans `NAMESPACE`).

Les arbres syntaxiques de `R/` sont identiques à la tête précédente (`parse`, `keep.source = FALSE`, mesure du commit `5a6dd1b`, *non remesurée ici*). Ni le renommage ni aucune autre suite n'est prévu. Une demande ultérieure de renommage deviendrait une issue, sans contrat partagé touché, puisque les deux fonctions sont internes.

**Effet sur les résultats : aucun.** Rien n'est modifié dans le code par cette annotation ; #56 n'a changé que des commentaires et un libellé de mesure.

Issues : #56 (close à la fusion de la PR #75) ; #38 ; #22 (point 7, clos). Décisions M68, M74.

## Annotation du 21 octobre 2026 — annotation du 17/10/2026, (d) : contrôles de `donnees` exécutés une seule fois par construction, argument interne `controler` (#22, point 2 ; constat m6 de la revue finale de `claude/dette-moteur`, m2 de l'audit de #22 pt 2)

Rédigée par `architect-approfondi` en fin de branche `claude/dette-moteur` (PR #80, à `a303e5b`). Les numéros de ligne cités ici datent de `a303e5b` ; le nom de l'objet fait foi (gabarit des ADR, M77, (b)). Le texte ci-dessus, annotations comprises, n'est pas réécrit.

**(a) Constat.** L'annotation du 17/10/2026, (d), troisième alinéa, écrit des contrôles de `donnees` appelés par `valider_plan()` : « Leur second appel, dans `preparer_donnees()`, est conservé : il protège la fonction d'étape appelée directement, et son coût est négligeable. » Ce n'est plus exact depuis `fa4ca6f` (#22, point 2). Les trois fonctions d'étape `preparer_donnees()` (`R/donnees.R:583-591`), `calculer_exposition()` (`R/exposition.R:387-399`) et `calculer_taux_bruts()` (`R/brut.R:502-511`) portent un dernier argument interne `controler`, de valeur par défaut `TRUE`. `construire_table()` les appelle avec `controler = FALSE` (`R/construire_table.R:233`, `:239`, `:250`), après avoir tout contrôlé une fois en amont : `valider_plan()` (qui appelle `controler_parametres_donnees()`, `controler_parametres_exposition()` et `controler_parametres_brut()`, `R/construire_table.R:176-184`), `controler_roles()`, `deduire_recette()` et `resoudre_convention()` (`:215-220`). Seule la valeur `FALSE` omet les contrôles (`isFALSE()`).

Le second appel n'a donc lieu qu'en **appel direct** de la fonction d'étape, sous la valeur par défaut. Il garde son rôle : protéger la fonction d'étape appelée hors de `construire_table()` (point 11.3). Les codes de refus restent ceux de (d), « un code par condition, quelle que soit la fonction qui la détecte » : seuls les appels redondants sont retirés.

**(b) Mesure.** Nombre d'exécutions de chaque contrôle pendant un appel de `construire_table()` (plan à trois étapes `donnees`, `exposition`, `brut`, estimateurs Poisson, binomial et Kaplan-Meier ; base de quatre lignes de `tests/unitaires/test_construire_table.R`), compté par `trace()` sur l'espace de noms installé, `a7091c4` → `a303e5b` (*vérifié* le 21/10/2026 par `architect-approfondi`, R 4.3.3, `LC_ALL=C.UTF-8`, deux bibliothèques temporaires installées depuis `git archive`) :

| Contrôle | `a7091c4` | `a303e5b` |
|---|---|---|
| `controler_segmentation()` | 5 | 1 |
| `controler_parametres_donnees()` | 4 | 1 |
| `controler_roles()` | 2 | 1 |
| `controler_parametres_exposition()` | 2 | 1 |
| `controler_parametres_brut()` | 2 | 1 |
| `deduire_recette()` | 2 | 1 |
| `resoudre_convention()` | 2 | 1 |
| `controler_transition_periode()` | absente | 1 |

`controler_transition_periode()` est née de la scission de `controler_parametres_donnees()` par `fa4ca6f`. Ces comptes sont figés par les tests de `tests/unitaires/test_construire_table.R` (`compter_controles()`, `88a5984`), sur les chemins avec et sans Kaplan-Meier, et en appel direct de chaque fonction d'étape.

**(c) Contrats de l'ADR inchangés.** L'argument `controler` ne touche aucun contrat de cet ADR ni aucun contrat partagé (`docs/agents/routage.md`, § 4.2) :
- les fonctions d'étape sont internes : `NAMESPACE` est identique à `a7091c4` (`git diff --stat a7091c4 a303e5b -- NAMESPACE` vide, *vérifié*) ;
- la signature de `construire_table(donnees, roles, plan, provenance = NULL)` est inchangée (`formals()` sur les deux SHA installés, *vérifié*) ;
- la table étape → fonction (point 3.1, constante `ETAPES`, `R/construire_table.R:37`) est inchangée, et le test « ETAPES et construire_table() concordent » (`tests/unitaires/test_construire_table.R:373`) passe (constat d'`audit` sur #22 pt 2, *non remesuré ici*) ;
- le graphe d'appels du point 2.1 est inchangé : `LC_ALL=C.UTF-8 Rscript tests/controles/graphe_modules.R` à `a303e5b` rend « OK : graphe d'appels conforme à l'ADR 0007, aucun appel dynamique interdit » (*vérifié*). Le prédicat `noms_complets()`, déplacé dans `R/outils.R` (note (3) de fin de B6), n'ajoute aucune arête.

Option écartée par `coder` (message de `fa4ca6f`) : découper chaque fonction d'étape en une fonction qui contrôle et une fonction interne qui ne contrôle pas. Elle aurait changé `ETAPES` et la table étape → fonction du point 3.1.

**(d) Réserve : la recette relue sous `controler = FALSE`.** Relevée par `/code-review` et par `actuary-approfondi` à la revue finale. Sous `controler = FALSE`, `calculer_exposition()` relit la recette par `recette_branche()` (`R/exposition.R:391`), sans regarder les rôles. Elle n'emploie pas la recette que `construire_table()` a déduite (`R/construire_table.R:217`) et qu'elle inscrit dans la fiche (`:271`). La convention retenue est recalculée de même par `convention_retenue()`.

Les deux sources coïncident tant que la branche n'a qu'une recette : `deduire_recette()` rend `recette_branche()` après le seul contrôle des rôles requis (`R/exposition.R:82-89`, lecture du code, *vérifié*). Avec une deuxième recette, l'étape retiendrait la recette de la branche sans refus, tandis que la fiche en déclarerait une autre. C'est une **seconde source** au sens de l'annotation du 17/10/2026, (c), deuxième effet, que cet ADR écarte. La réserve est portée par **#82** (créée le 21/10/2026, M92), qui évalue aussi la forme plus profonde proposée par `/code-review` : transmettre aux fonctions d'étape les valeurs validées par `construire_table()` (`p`, `recette`, `conv`). Elle supprimerait les fonctions jumelles sans contrôle (`valeurs_parametres_donnees()`, `recette_branche()`, `convention_retenue()`) et l'argument `controler`. #82 doit être résolue avant l'ajout d'une deuxième recette (`docs/feuille-de-route.md`, § 3).

**(e) Piste écartée consignée : table étape → fonction appelée par nom (#22, point 7).** Le commentaire de #22 du 19/10/2026 (6273510518) annonçait que cette piste serait consignée par annotation de cet ADR ; elle ne l'avait pas été. La piste : remplacer le `switch()` de `construire_table()` par un appel à la fonction nommée dans `ETAPES$fonction`. Elle est écartée pour deux raisons. Elle ne factorise rien, car les arguments diffèrent d'une étape à l'autre. Et elle demanderait `get()`, `match.fun()` ou `do.call()` sur un nom, que le contrôle du graphe interdit (point 2.1). Le risque visé (étape déclarée mais absente du `switch`) est couvert par le test « ETAPES et construire_table() concordent ». Le point 3.1 (« table de correspondance étape → fonction d'étape ») reste tenu par `ETAPES` et ce test. Ni `fa4ca6f` ni #82 ne rouvrent la question.

**Effet sur les résultats : aucun.** R1 à R6 identiques hors `execution` et hors empreinte du code (message de `fa4ca6f` ; batterie de reproductibilité de la branche, *non remesurée ici*). Cette annotation ne modifie aucun code.

Issues : #22 (points 2 et 7) ; #82. Décisions M82, M92. Branche `claude/dette-moteur`, PR #80.

## Annotation du 25 octobre 2026 — contrat « format des jeux simulés » : têtes multi-contrats (#92), défauts paramétrés (#93), signature étendue de `simuler_portefeuille()` (point 3.2), refus `defauts_invalides` et `defauts_ineligibles` (décisions M131 à M136)

Rédigée par `architect-approfondi` (fiche lue dans l'en-tête de la consigne de l'agent, à confirmer par `bash .claude/outils/bilan_journal.sh`), étape (2') de la branche `claude/simulateur-formes` (PR #96), avant tout code de #92 et #93. Les numéros de ligne cités ici datent de `3eb4848` ; le nom de l'objet fait foi (gabarit des ADR, M77, (b)). Le texte ci-dessus, annotations comprises, n'est pas réécrit. Elle complète le point 3.2 et le point 8.1, et porte le contrat partagé « format des jeux simulés » (`docs/agents/routage.md`, § 4.2), dont l'ADR 0003, point 9, pose le principe (« le même modèle, exécuté en avant », « défauts paramétrés » ; mention datée du même jour dans l'ADR 0003).

**Sources.** Spécification commune de #92 et #93 par `actuary-approfondi`, versée sur #92 le 25/10/2026 (https://github.com/Gerard-Garey/outil_tables/issues/92#issuecomment-6396647808, renvoi sur #93), ci-après « la spécification » ; décisions du mainteneur du 25/10/2026 sur son § 5, toutes conformes à la recommandation d'`actuary` (M131 à M136, `docs/feuille-de-route.md`, § 4). Les chiffres du prototype cités ci-dessous viennent de la spécification (prototype hors dépôt sur `3eb4848`) : *non remesurés ici* ; les attentes des tests seront mesurées sur le code réel, par `coder-approfondi` puis `audit`.

**(a) Contrainte dure : rien ne change pour les modes existants.** Pour `uniforme` et `age_exact` : l'attribut `simulation` (`R/simulation.R:471-480`) reste identique, sans champ ajouté, pas même `NULL` ; la constante `ORDRE_TIRAGES` (`:39-43`) reste intouchée, libellés compris ; l'appel de `sous_graine()` est le même ; `CHAMPS_PORTEFEUILLE` (`:46-48`) garde ses deux entrées à l'identique, les modes nouveaux s'y ajoutant. `jeu_reference()` (`tests/reference/outils_reference.R:114`, mode `uniforme`) rend donc un jeu `identical()` à celui de `3eb4848`, attribut compris (critère 1 de #92 dans la spécification, § 4). Si le code ne peut pas tenir cette contrainte, le circuit s'arrête et revient au mainteneur (`docs/feuille-de-route.md`, § 1.9, « Risque sur l'ordre des tirages »).

**(b) Point 3.2 : signature étendue.** La signature devient

```
simuler_portefeuille(loi, portefeuille, periode, graine, defauts = NULL)
```

(à `3eb4848` : `simuler_portefeuille(loi, portefeuille, periode, graine)`, `R/simulation.R:384`, *vérifié* par lecture). Lecture au regard du point 3.2 et de l'option écartée « Graine par défaut dans `simuler_portefeuille()` » :

- `graine` reste **requise, sans valeur par défaut** ; l'option écartée tient ;
- `defauts = NULL` **n'est pas une graine par défaut** : `NULL` signifie « **aucun défaut injecté** » (jeu propre), et le jeu ne porte alors **aucun attribut `defauts`**. Quand `defauts` n'est pas `NULL`, sa graine `defauts$graine` est **requise**, sans défaut (M133) ;
- c'est la forme de `provenance = NULL` (point 3.1) : un argument nommé facultatif, dont la valeur `NULL` désigne l'absence de l'objet et non une valeur choisie à la place de l'appelant. Les appels existants, à quatre arguments, sont inchangés ; le test « `defauts = NULL` donne un jeu `identical()` à l'appel sans cet argument » le vérifie (spécification, § 4, #93, critère 1) ;
- argument plutôt que fonction séparée (M132) : voir « Options écartées » en (h) ;
- **aucun export nouveau** : `NAMESPACE` est inchangé (`simuler_portefeuille` y est déjà, *vérifié* à `3eb4848`). Les contrôles 1 et 2 de la concordance (fonctions citées, exports cités) ne sont pas touchés ; la documentation de la signature relève du passage de `docwriter` (règle 9).

**(c) Contrat « format des jeux simulés ».** Onze points, qui reprennent la liste du § 3 de la spécification. Un point modifié ensuite est un changement du contrat partagé (routage, § 4.2).

1. **Modes multi-contrats** (M131). Deux modes nouveaux, aux champs **tous requis, sans défaut implicite** : `uniforme_multi` = `mode`, `n`, `proportion_hommes`, `age_min`, `age_max`, `origine`, `loi_contrats` ; `age_exact_multi` = `mode`, `n`, `proportion_hommes`, `age`, `date_entree`, `loi_contrats`. Dans **tous** les modes, `n` est le nombre de **têtes** (dans les modes existants, une tête porte un contrat). `loi_contrats` = probabilités `p_1, …, p_m` du nombre K de contrats par tête sur {1, …, m} : forme exacte double ou integer **sans aucun attribut**, `m ≥ 1`, valeurs finies `≥ 0`, `p_m > 0`, `|Σp − 1| ≤ 1e-12`, sinon refus `portefeuille_invalide` au message propre au champ. Garde de débordement : `n × length(loi_contrats) > .Machine$integer.max` → `portefeuille_invalide`. La loi effectivement tirée, `(p_1, …, p_{m−1}, 1 − F_{m−1})`, s'écarte de la loi déclarée de moins de 1e-12 : **approximation numérique**, documentée. K est indépendant de la mortalité et du rachat (aucune antisélection).
2. **Modèle** (M135). Par **tête** : sexe ; naissance (en `uniforme_multi`, âge à la **première** entrée `e_(1) = min_k e_k` uniforme sur `[age_min, age_max)`, `naissance = e_(1) − round(âge × 365,25)` ; en `age_exact_multi`, comme en `age_exact`) ; décès, temps latent tiré à partir de `e_(1)` par `temps_latent()` (`:339`). Par **contrat** : entrée (en `uniforme_multi`, iid uniforme discrète sur les jours de `[origine, fin]` ; en `age_exact_multi`, `date_entree`) ; rachat, temps latent à partir de `e_k` à l'âge de la tête (risques concurrents indépendants). **Contrat non souscrit** : un contrat n'est écrit que si `e_k <` l'instant de décès de la tête (comparaison sur le double de `temps_latent()`) ; le premier contrat l'est donc toujours. **Sortie** : règle actuelle, avec un instant de décès commun à tous les contrats de la tête (`B = fin + 1` ; décès si `t_d ≤ t_r,k` et `t_d < B` ; rachat si `t_r,k < t_d` et `t_r,k < B` ; en cours sinon ; à égalité, le décès l'emporte) ; les contrats en cours au décès portent tous `deces` à la date de la tête. Assertion « date de décès ≥ entrée de chaque contrat écrit » : erreur de programmation (`stop()`), jamais un refus. `age_bas`, `age_haut` inchangés ; `x_borne` par tête.
3. **Ordre des tirages.** `ORDRE_TIRAGES` est **intouchée**. Chaque mode nouveau a sa constante (forme proposée : `ORDRE_TIRAGES_MULTI$<mode>`), consignée dans le champ `ordre_tirages` de l'attribut ; les défauts ont la leur (`ORDRE_TIRAGES_DEFAUTS`, point 7). Un seul `sous_graine()` par mode. `uniforme_multi` : (1) sexe `runif(n) < proportion_hommes` ; (2) K par inversion explicite, `1 + findInterval(runif(n), cumsum(loi_contrats)[-m])`, et non `sample(prob =)` ; (3) entrées par contrat **tiré**, têtes puis contrats, `sample.int(nombre de jours, N, replace = TRUE)` ; (4) âge à la première entrée ; (5) décès `rexp(n)` ; (6) rachat `rexp(N)` pour chaque contrat **tiré**, souscrit ou non. `age_exact_multi` : sexe, K, décès `rexp(n)`, rachat `rexp(N)`. **Règle de la loi dégénérée** : aucun tirage de K quand la loi n'a qu'une probabilité non nulle. Conséquence voulue : avec `loi_contrats = 1`, un mode multi tire exactement les mêmes nombres que le mode existant, et rend un jeu `identical()` hors attribut (constat du prototype pour `uniforme` ; test à écrire pour les deux paires de modes).
4. **Colonnes et identifiants.** Les sept colonnes actuelles, dans le même ordre et du même type : `id_contrat`, `id_tete`, `sexe`, `date_naissance`, `date_entree`, `date_sortie`, `cause_sortie`. `id_tete` de 1 à n ; lignes triées par (`id_tete`, `date_entree`, rang de tirage), tri stable `radix` ; `id_contrat` de 1 au nombre de lignes, dans cet ordre ; noms de lignes automatiques. Une tête dont aucun contrat n'est écrit n'existe pas : le premier contrat est toujours souscrit (point 2).
5. **Attribut `simulation`.** Modes existants : strictement inchangé (a). Modes multi : mêmes champs dans le même ordre, puis **ajouts en fin seulement** : `ordre_tirages` porte la constante du mode ; `decomptes` garde ses six champs (`lignes` compte les contrats **écrits**) suivis de `tetes`, `contrats_tires`, `contrats_non_souscrits`, `tetes_decedees_observees`, `tetes_decedees_sans_contrat` ; un champ final `contrats = list(loi = <probabilités, double>, tires_par_tete = tabulate(K, m))`. Ajouter un champ en fin est compatible (point 6.3, par analogie) ; renommer ou retirer un champ est un changement du contrat.
6. **Argument `defauts`** (M132 ; (b)). `NULL`, ou une **liste nue** `list(graine = <entier>, declarations = list(<déclaration>, …))`. Chaque déclaration : `list(type, cible, taux)` ou `list(type, cible, nombre)` pour un type de **ligne**, sans `cible` pour un type de **tête** (`cible` y est interdite) ; `cible` ⊂ `c("deces", "rachat", "en_cours")` ; `taux` réel de [0, 1] (tirage de Bernoulli) ; `nombre` entier ≥ 0 (tirage sans remise). Le **catalogue** est celui de la spécification, § 2.2 : quinze types de ligne (quatorze motifs de ligne de `MOTIFS_LIGNES`, `R/donnees.R:109-114`, plus `date_hors_domaine`, qui mène au refus du même nom et non à un motif) et trois types de tête (`MOTIFS_TETE`, `:115-116`, moins `tete_segment_incoherent`), avec, pour chacun, l'unité, les éligibles (intersection avec `cible`), l'altération **déterministe** et le motif ou le refus attendu. Une seule combinaison de motifs par ligne est admise au départ, `c("cause_manquante", "sexe_manquant")` (#57 ; liste fermée) ; toute combinaison nouvelle demande l'avis d'`actuary`. Les noms des listes se comparent **sans tri** (`setdiff`, `anyDuplicated`, `anyNA`), comme `controler_portefeuille()` depuis #78 : `liste_aux_noms()` lève une erreur R sur un nom non ASCII placé en tête (#100, mesure de la session du 25/10/2026, *non remesurée ici*).
7. **Graine des défauts** (M133). Déclarée dans `defauts$graine`, sans défaut, de la forme exigée par `sous_graine()` (#43). Ordre : (i) le jeu propre est entièrement tiré, sans aucun changement ; (ii) **un seul** `sous_graine(defauts$graine, …)` traite les déclarations **dans l'ordre reçu** ; pour chacune, les éligibles sont les unités **non encore altérées**, en ordre croissant ; avec `taux`, une unité est retenue si `runif(nombre d'éligibles) < taux` ; avec `nombre`, les retenues sont `sort(éligibles[sample.int(nombre d'éligibles, nombre)])`. **Disjonction** : une ligne est altérée au plus une fois ; une tête altérée rend ses lignes inéligibles ensuite ; une tête n'est éligible à un défaut de tête que si aucune de ses lignes n'a été altérée. Les copies (`contrat_duplique`) prennent les numéros de ligne N + 1, …, en fin de jeu.
8. **Attribut `defauts`** (M134). **Séparé** de `simulation`, qui reste identique à celui du jeu propre ; **absent** si `defauts = NULL`. Champs : `parametre` (l'argument reçu), `graine`, `generateur`, `ordre_tirages` (`ORDRE_TIRAGES_DEFAUTS`), `validite`, `lignes`, `decomptes`. `lignes` : un `data.frame` d'une ligne par couple (ligne, motif), colonnes `ligne`, `declaration`, `defaut`, `unite`, `role` (`alteree`, `copie`, `original` ou `tete`), `motif_attendu`, `refus_attendu`, `premier_deces`. **Conditions de validité** consignées (`validite`) : les sept rôles déclarés, un plan qui porte la période de la simulation, un délai de déclaration nul ; pour `tete_observee_apres_deces`, le délai `s_d` sous lequel le motif est attendu (le motif en dépend). L'attribut survit à l'exclusion dans `lignes_conservees`, et `construire_table()` comme `ecrire_rapport()` l'acceptent (constat du prototype). Il n'entre ni dans l'empreinte des données (colonnes à rôle seules, point 9.3) ni dans celle du résultat (`etapes/donnees/resultat/lignes_conservees` est dans `CHEMINS_HORS_RESULTAT`, `R/rapport.R:446-448`, *vérifié* par lecture).
9. **Refus.** Deux codes nouveaux, module `simulation` : **`defauts_invalides`**, levé **avant tout tirage** (forme ou valeur de `defauts` ou d'une déclaration, type inconnu, `cible` manquante ou interdite, `taux` et `nombre` tous deux ou aucun) ; **`defauts_ineligibles`**, levé **après le tirage du jeu propre** quand `nombre` dépasse le nombre d'éligibles (refus qui dépend de la graine du jeu, à la différence du premier). `loi_contrats` et la garde de débordement **réutilisent `portefeuille_invalide`** : aucun code pour #92. Effet sur les décomptes cités en (e).
10. **Statut des constats et non-circularité** : voir (d).
11. **Hors périmètre** (M136 et spécification, § 3, point 11) : les motifs `annee_naissance_incoherente`, `segment_manquant` et `tete_segment_incoherent`, faute de colonne `annee_naissance` et `segment` dans le jeu simulé ; toute dépendance entre K et la mortalité ; les formes « agrégats » et « stocks successifs » (branches suivantes, `docs/feuille-de-route.md`, § 2) ; les jeux versionnés sous `inst/extdata/` (M121). Un motif ajouté plus tard à `MOTIFS_LIGNES` ou `MOTIFS_TETE` sans type de défaut correspondant doit faire échouer un test (point (d), dernier tiret).

**(d) Statut des constats et non-circularité** (E2.20 ; point 11.4 : quatre natures d'attendu).

- **Statut.** Les tests de #92 et #93 sur les jeux tirés sont des **constats de simulation** ; ce qui distingue les familles est le statut de leur **probabilité de fausse alarme** :
  - **exacte** : famille A (`age_exact_multi`, sept tests binomiaux, Bonferroni, Σ FA = 9,566·10⁻⁴ ≤ α = 10⁻³ selon la spécification, § 1.6) ; décès exclus du jeu du § 2.6, K_D ~ Bin(D_w, 0,5) exactement ;
  - **borne exacte** : exposition exclue du jeu du § 2.6, par l'inégalité de Hoeffding (FA ≤ a garantie, sans approximation) ;
  - **asymptotique** : famille B (`uniforme_multi`, statistique Z, |Z| ≤ 3,4808), acceptée comme telle par le mainteneur (M131) et **présentée comme telle** dans les tests et la documentation ; les valeurs informatives (X²/ddl, φ̂) sont imprimées sans verdict.
  - Les **identités** (D en unité tête = Σ N_c, exposition par union et par somme d'intervalles, chevauchements + contiguïtés = lignes − têtes, écarts entre jeu propre et jeu altéré…) sont des **identités exactes**, déterministes, sans probabilité de fausse alarme. L'écart de < 1e-12 entre loi déclarée et loi tirée de K est une **approximation numérique** (point 1). Erreur Monte-Carlo et erreur d'estimation restent distinguées : chaque test écrit n, α, la région et la probabilité de fausse alarme.
- **Non-circularité.** Les attentes viennent de formules fermées (recoupées par `stats::integrate`, appel direct dans le test : un **recoupement**, `CONTEXT.md`), de décomptes faits **directement sur les lignes** et de l'attribut. Deux règles de forme en découlent :
  - le simulateur **n'appelle aucun contrôle de `donnees`** (`preparer_donnees()`, `controler_*()`) pour établir `motif_attendu` ou `refus_attendu` : ces colonnes sont **lues dans le catalogue** des défauts (constante du module `simulation`), jamais calculées en exécutant ce qu'elles servent à tester. Le graphe du point 2.1 admettrait l'appel (`simulation` → `donnees`) ; c'est la règle de non-circularité, non le graphe, qui l'interdit ;
  - les tests confrontent `preparer_donnees()` à l'attribut (`lignes_exclues` égal à l'attribut quand toutes les exclusions sont déclarées ; `ligne_invalide` sans exclusion ; `date_hors_domaine` sur un jeu à part) : c'est la vérification, non une source de l'attente. Un test de clôture vérifie que l'ensemble des motifs attendus du catalogue, réuni aux trois motifs hors périmètre, égale `c(MOTIFS_LIGNES, MOTIFS_TETE)` : un motif ajouté à `donnees` sans décision sur le simulateur le fait échouer.

**(e) Codes de refus : décompte remesuré.** L'annotation du 19/10/2026, (a), dernier alinéa, donne 74 motifs distincts et 76 paires (module, motif) à `a311c85`, puis 75 et 77 depuis `5bf62e6`, par relevé des arguments **littéraux** des appels `refuser()`, `refus_tablexp()` et `refus_plan()`. Remesure par `architect` le 25/10/2026 (*vérifié* ; script du bac à sable : `parse()` de `git show <sha>:R/<fichier>` pour chaque fichier de `R/`, appariement des arguments par `match.call()`) :

| Méthode | `a311c85` | `5bf62e6` | `9c28cd3` | `3eb4848` |
|---|---|---|---|---|
| arguments littéraux (méthode de l'annotation du 19/10/2026) | 74 motifs, 76 paires | 75, 77 | 78, 81 | 78, 81 |
| relevé complet (aides locales comprises) | — | — | — | **101 motifs, 106 paires** |

- La méthode littérale retrouve exactement les chiffres de l'annotation du 19/10/2026 à `a311c85` et `5bf62e6` : elle est étalonnée. Elle **sous-compte** désormais : six appels n'ont plus d'argument littéral à `3eb4848`. Ce sont les aides locales `refus()` et `invalide()` du module `rapport` (`R/rapport.R` : `refus()` dans `lire_texte()`, `:960-961`, et dans `controler_rapport()`, `:1431-1432` ; `invalide()` dans `valeurs_texte()`, `:898` ; 18 motifs `texte_*`, `objet_modifie`, `rapport_objet_invalide`, `version_fiche_non_admise`) ; `refus_rejeu()` de `rejouer_fiche()` (`R/construire_table.R:406`, cinq motifs) ; et `refuser(etape, "segmentation_invalide", …)` (`R/donnees.R:305`, module `donnees` ou `brut` selon l'appelant de `controler_segmentation()`). Le relevé complet les ajoute.
- À `3eb4848`, cinq motifs sont émis par deux modules : `aucune_recette` (`donnees`, `exposition`), `date_hors_domaine` (`donnees`, `rapport`), `periode_invalide` (`donnees`, `simulation`), `segmentation_invalide` (`donnees`, `brut`), `version_fiche_non_admise` (`construire_table`, `rapport`). Module `simulation` : six motifs, `entree_29_fevrier`, `loi_invalide`, `periode_invalide`, `portefeuille_invalide`, `qx_egal_a_un`, `table_trop_courte`. `9c28cd3` → `3eb4848` : ensemble littéral identique (#58 et #78 n'ajoutent aucun code, *vérifié*).
- **Effet attendu de #92 et #93** (*hypothèse*, à remesurer par la même méthode sur les commits (3) et (4)) : #92, aucun code (`portefeuille_invalide` réutilisé) ; #93, `defauts_invalides` et `defauts_ineligibles`, module `simulation`, soit **103 motifs et 108 paires** en relevé complet (80 et 83 en relevé littéral, si les deux codes sont écrits en appels littéraux de `refuser()`). Les motifs **de ligne** du catalogue (`contrat_duplique`…) ne sont pas des codes de refus et ne changent pas ces décomptes. Désormais, tout décompte de codes cité dans cet ADR nomme sa méthode.

**(f) Question ouverte pour `actuary`, avant le code de #93 : graine des défauts égale à la graine du jeu.** La spécification ne dit pas si `defauts$graine` peut égaler `graine`. Or `sous_graine()` repose `set.seed(graine, …)` à chaque appel : sous la même graine, les tirages de sélection des défauts reprennent **les mêmes nombres** que les premiers tirages du jeu propre. Mesure (*vérifié*, 25/10/2026, R 4.3.3, `sys.source()` de `R/outils.R` à `3eb4848`) : n = 10⁵, graine 20261016 (celle de `jeu_reference()`, `proportion_hommes = 0,5`) ; `runif(n)` sous `sous_graine()` deux fois → `identical()` vrai ; une sélection `u < 0,02` de toutes les lignes, dans l'ordre des identifiants comme le prescrit le point 7, retient 2 039 lignes, **toutes d'hommes** ; sous la graine 20261017, 1 982 lignes dont 971 hommes. En mode `uniforme`, où chaque ligne est éligible dans l'ordre de tirage, une même graine rend donc la sélection dépendante du sexe : c'est une exclusion informative non déclarée, exactement ce que le jeu du § 2.6 doit maîtriser. Recommandation d'`architect` : refus `defauts_invalides` quand `defauts$graine == graine` (aucun code nouveau), consigné dans l'ordre des tirages des défauts. Le fond revient à `actuary` ; la question ne relance pas `coder` tant qu'elle n'est pas tranchée.

**(g) Ce qui ne change pas.** Le graphe du point 2.1 : `simulation` n'appelle que `donnees` et `outils` (`LC_ALL=C.UTF-8 Rscript tests/controles/graphe_modules.R` à `3eb4848` : « OK : graphe d'appels conforme à l'ADR 0007, aucun appel dynamique interdit », *vérifié*) ; tout le code de #92 et #93 va dans `R/simulation.R`, puisque le contrôle déduit le module du nom de fichier et tient pour un écart tout fichier qui n'est pas un module connu (`tests/controles/graphe_modules.R`, en-tête, point 1, *vérifié* par lecture). Le point 8.1 : toute simulation passe par `sous_graine()`, sous `GENERATEUR` ; les défauts aussi. `construire_table()` n'appelle pas `simulation`. Aucun champ de l'objet résultat ni de la fiche ne change.

**(h) Options écartées** (spécification, § 1.1 et § 5 ; décisions M131 à M135).

- **Champ facultatif `loi_contrats` ajouté aux modes existants**, son absence valant K ≡ 1 : défaut implicite (point 5.1, esprit), et risque de toucher l'attribut des modes existants.
- **Fonction séparée pour injecter les défauts** (`alterer_portefeuille(jeu, defauts)`) : elle recevrait un jeu dont rien ne garantit qu'il soit propre ni tiré par le simulateur ; l'attribut `defauts` ne pourrait plus attester la disjonction avec le jeu propre ni l'ordre des tirages ; un second export à documenter (M132).
- **Graine des défauts implicite** (dérivée de `graine`, ou absente) : graine non déclarée, contraire au point 8.1 (M133).
- **Défauts consignés dans l'attribut `simulation`** : l'attribut des modes existants changerait dès qu'un défaut est déclaré, et la contrainte (a) ne serait plus vérifiable par simple `identical()` (M134).
- **Tête qui survit jusqu'à sa dernière entrée** (décès tiré à partir de la dernière entrée) : biais de temps immortel, Z = −16,3 sur le prototype (M135).
- **K tiré par `sample(prob =)`** : l'ordre et le nombre de tirages dépendraient de l'implémentation de `sample()` ; l'inversion explicite les fixe et donne la règle de la loi dégénérée.

**Effet sur les résultats : aucun** par cette annotation, qui ne modifie aucun code. Effet attendu des commits (3) et (4) : aucun sur R1 à R6 (empreintes des données et du résultat identiques, aucune régénération) ; seul le jeton de l'empreinte du code change. Les constats de #92 et #93 sont des tests nouveaux, non des références.

**Mention datée du 25/10/2026 (fin de la branche `claude/simulateur-formes`, `architect`) : (c), point 6, dernière phrase, périmée depuis `f0593a7` (#100).** Le texte de (c) n'est pas réécrit. La phrase « `liste_aux_noms()` lève une erreur R sur un nom non ASCII placé en tête » décrivait le code de `3eb4848`, où la fonction triait les noms (`sort(method = "radix")`). Depuis `f0593a7` (#100, commit (2'') de la branche), elle les compare sans tri (`anyNA()`, `anyDuplicated()`, `setequal()`, `R/outils.R:84-89` à `cdfca7c`) et rend `FALSE`, sans erreur, sur un nom non ASCII en tête, quel que soit son encodage (natif, octets invalides, latin-1, UTF-8, `bytes`), sous C comme sous C.UTF-8 : mesure de la revue finale d'`audit` (effort `high`) sur `cdfca7c`, remesurée par `architect` (*vérifié*, 25/10/2026, R 4.3.3, `sys.source()` de `R/outils.R` à `cdfca7c`, noms attendus `c("debut", "fin")`, nom reçu `début` en première position, quatre encodages, `LC_ALL=C` puis `LC_ALL=C.UTF-8` : huit fois `FALSE`, aucune erreur). Une erreur ne subsiste que **hors de la précondition** écrite en commentaire de la fonction, « noms attendus ASCII », vraie pour tous ses appelants : avec des noms attendus non ASCII, `setequal()` passe par `match()`, qui traduit les encodages, et un nom reçu marqué `bytes` lève sous C.UTF-8 (mesure d'`audit` sur `f0593a7`, citée par le commentaire, *non remesurée ici*). La règle de forme de point 6 (noms des listes de `defauts` comparés sans tri) reste celle du code de #93 (`63adaf9`) ; sa justification par #100 n'a plus d'objet, sans effet sur le contrat. Un nom de champ du portefeuille marqué `bytes` qui fait lever `sprintf()` dans le **message** de refus de `controler_portefeuille()` est un autre défaut, préexistant sur `main` (#111 ; constat C1 de la revue finale), sans lien avec `liste_aux_noms()`. Même correction dans `CONTEXT.md`, « Forme exacte ».

Issues : #92, #93 ; #69 (jeux préalables, M119 ; la règle d'exclusion d'un décès en unité tête sur un jeu multi-contrats, « toutes ses lignes à décès exclues », est à trancher dans la spécification de #69) ; #100 (`liste_aux_noms()`) ; #57 (combinaisons de motifs). Décisions M116, M131 à M136. Branche `claude/simulateur-formes`, PR #96.

## Annotation du 5 novembre 2026 — point 6.4 : règle de rédaction des messages de refus, un seul point de passage `formuler()` (#127) ; point 9.3 : conversion des chaînes selon la locale en un seul lieu, `vers_utf8_locale()` (#113, point 1) (décision M182)

Rédigée par `architect-approfondi` (modèle servi à confirmer par `bash .claude/outils/bilan_journal.sh`), étape (0') de la branche `claude/dette-regles` (PR #134), avant tout code de #127 et de #113. Elle consigne l'avis de forme d'`architect` que le mainteneur a accepté le 05/11/2026 (M182 ; `docs/feuille-de-route.md`, § 1.12 et § 4). Le texte ci-dessus, annotations comprises, n'est pas réécrit. Elle complète le point 6.4 (champ `message` d'un refus), le point 1.2 (contenu d'`outils`) et le point 9.3 (codage des chaînes). Les numéros de ligne datent de `8cb46b3`, dont `R/`, `tests/` et `.github/` sont ceux de `e007ebb` (`git diff --stat e007ebb 8cb46b3 -- R/ tests/ .github/` vide, *vérifié*) ; le nom de l'objet fait foi (M77, (b)). Mesures : R 4.3.3 ; `tablexp` installé depuis l'arbre à `8cb46b3`, et depuis `git archive 8bd29dd`, par `R CMD INSTALL` dans des bibliothèques temporaires hors du dépôt, sous `LC_ALL=C` ou `LC_ALL=C.UTF-8` ; scripts du bac à sable de l'agent.

**(a) Constat : la régression de #127, remesurée.** Appel `tablexp:::controler_roles(data.frame(x = 1), c(sexe = "âge"), character(0))`, refus `colonne_absente` (`R/donnees.R:241-244`), message lu par `tryCatch(…, refus_tablexp = …)`, octets lus par `charToRaw()` (*vérifié*, 05/11/2026) :

| installation / exécution | `8cb46b3` (même `R/` que `5060410`) | `8bd29dd` (avant #94) |
|---|---|---|
| C / C | marqué UTF-8 ; « `Colonne absente des donn<c3><a9>es : <c2><ab> âge <c2><bb> (r<c3><b4>le <c2><ab> sexe <c2><bb>).` » : les octets non ASCII du gabarit et des guillemets sont remplacés par le texte ASCII `<xx>` ; la valeur citée garde ses octets (`c3 a2 67 65`) | `unknown` ; « Colonne absente des données : « âge » (rôle « sexe »). », octets UTF-8 intacts |
| UTF-8 / C | marqué UTF-8, octets intacts (R avertit « strings not representable in native encoding will be translated to UTF-8 ») | non mesuré |
| C / C.UTF-8 et UTF-8 / C.UTF-8 | marqué UTF-8, octets intacts | non mesuré |

La première ligne retrouve la mesure de la feuille de route (§ 3, ligne #127, 05/11/2026, à `e007ebb`), qui affiche la valeur sous la forme `<U+00E2>ge` : c'est la traduction faite par `cat()` sous C, les octets du message sont `c3 a2` (*vérifié*). La régression ne se produit donc que si le paquet est **installé** sous une locale qui n'est pas UTF-8 : les littéraux y gardent leurs octets UTF-8 sans marque (convention de `texte_moteur()`, `R/outils.R:150-162`, #50). Les batteries installent toujours en UTF-8 (`installer_tablexp()`, `tests/harnais.R:324`) ; seul `tests/controles/locale_c.R` installe sous C (en-tête du script, *vérifié* par lecture) : c'est le constat 3 de #127.

**(b) Cause : `sprintf()`, `paste()` et `paste0()` traduisent leurs arguments non marqués dès que l'un d'eux est marqué UTF-8, avant tout marquage ultérieur.** Sous C, paquet installé sous C (*vérifié*, 05/11/2026), gabarit `"rôle %s"` (octets `72 c3 b4 6c 65 20 25 73`, `unknown`), valeur `neutraliser_citation("âge")` (`c3 a2 67 65`, marquée UTF-8) :

- `texte_moteur(sprintf(g, v))` rend `72 3c 63 33 3e 3c 62 34 3e 6c 65 20 c3 a2 67 65`, soit « r<c3><b4>le âge » : marquer **après** `sprintf()` vient trop tard, les octets du gabarit sont déjà devenus le texte ASCII `<c3><b4>`, que rien ne distingue plus d'une saisie ;
- `sprintf(texte_moteur(g), v)` rend `72 c3 b4 6c 65 20 c3 a2 67 65`, marqué UTF-8 : marquer **avant** donne le texte attendu ; avec une valeur ASCII, le résultat est aussi marqué UTF-8 ;
- sous C.UTF-8, les deux formes rendent les mêmes octets.

`liste_guillemets()` (`R/donnees.R:190`) a le même défaut par `paste0()` : à `8cb46b3`, `liste_guillemets("âge")` rend `3c 63 32 3e 3c 61 62 3e 20 c3 a2 67 65 20 …`, soit `<c2><ab> âge <c2><bb>` ; avec les guillemets marqués avant le collage, `paste0(texte_moteur("« "), citer(x), texte_moteur(" »"), collapse = ", ")` rend `c2 ab 20 c3 a2 67 65 20 c2 bb`, marqué UTF-8, et `c2 ab 20 78 20 c2 bb` pour la valeur ASCII `x` (*vérifié*, même session). Même mécanisme, par lecture du code (*hypothèse*, non mesuré) : `paste("Défauts :", msg)` dans l'aide `invalide()` de `controler_defauts()` (`R/simulation.R:510-511`), dès que `msg` porte une citation marquée.

**(c) Règle de rédaction des messages de refus (point 6.4 précisé).** Elle vaut pour le champ `message` de tout refus, dans tous les modules, `outils` compris (`graine_invalide`, `R/outils.R:279`), et dans les aides locales qui lèvent un refus : `refus_plan()` et `refus_rejeu()` (`R/construire_table.R:92`, `:407`) ; `invalide()` et `refus()` (`R/rapport.R:1184`, `:1250`, `:1751`) ; `invalide()` (`R/simulation.R:354`, `:510`).

1. **Un seul point de passage** : `formuler(gabarit, ...)`, défini dans `R/outils.R`, vaut `sprintf(texte_moteur(gabarit), ...)`. Tout message de refus est formé par `formuler()`, qu'il insère des valeurs ou non (un message sans valeur s'écrit `formuler("…")`, `%` littéral écrit `%%`). Aucun `sprintf()`, `paste()` ni `paste0()` d'un module ne combine plus un littéral non ASCII de message avec une valeur. Les dix lignes `sprintf(texte_moteur(` de `R/rapport.R` (`:1185` à `:1410`, `grep -n`, *vérifié*) sont cette règle écrite à la main ; elles deviennent des appels de `formuler()`.
2. **Toute valeur reçue est citée** : une valeur qui vient des données, des rôles, du plan, de la provenance, d'une fiche ou d'un texte lu (chaîne, nom, classe, chemin) entre dans le message par `citer()` (`R/donnees.R:188`) ou `neutraliser_citation()` (`R/outils.R:215`), règle unique de #94. Ce qui est déjà formé selon la règle n'est pas cité une seconde fois : le résultat de `liste_guillemets()` et celui de `formuler()`. Un nombre inséré par `%d`, `%g` ou `%f` n'est pas cité (`sprintf()` refuse une chaîne sous ces conversions). La règle couvre les sites que la documentation déclare aujourd'hui non couverts (`sec:refus`, « Limite ») : `type_incompatible`, `type_non_canonique`, `serialisation_impossible` (constat 2 de #127).
3. **`liste_guillemets()` rend un texte marqué** : ses guillemets passent par `texte_moteur()` avant le collage (forme mesurée en (b)) ; son résultat est un argument admis de `formuler()`, sans nouvelle citation. La double citation `sans_controle(liste_guillemets(...))` (`R/construire_table.R:535`) devient superflue : `neutraliser_citation()` laisse inchangée sa propre sortie (*hypothèse*, lecture du code : ni `<U+XXXX>` ni les guillemets ne contiennent de caractère de l'ensemble neutralisé).
4. **Signature de `refuser()` inchangée** : `refuser(module, motif, message, details)` (`R/outils.R:58`) et `refus_tablexp()` (`:40`) gardent leurs quatre champs et leurs contrôles ; la condition (classe, champs `module`, `motif`, `message`, `details`) reste celle du point 6.4. Ce qui change est une **garantie sur `message`** : une chaîne qui contient un caractère non ASCII est marquée UTF-8, a les mêmes octets sous toute locale d'installation et d'exécution (C et UTF-8 ; latin-1 non mesurée ici), et tient sur une ligne (#94). Le contrôle de cette garantie est le critère 2 de #127 (`locale_c.R`). Les erreurs de programmation (`stop()`, 29 lignes hors de `R/outils.R`, `grep`, *vérifié*) ne sont pas des refus et restent hors de la règle.
5. **Avertissements hors de la règle (point 6.5).** `avertir()` marque son message après sa construction (`texte_moteur(message)`, `R/outils.R:145`). C'est suffisant tant que les valeurs insérées sont du texte du moteur sans marque (noms de rôles du catalogue, codes de convention et de recette, libellés de relevés) : `sprintf()` n'a alors rien à traduire. Or le message d'un avertissement entre dans l'objet résultat, et le critère 4 de #127 exige des avertissements identiques, marques d'encodage comprises. Faire passer un avertissement par `formuler()` n'est donc admis que si ce critère est vérifié sur R1 à R6 ; un avertissement qui citerait une valeur reçue relèverait de la règle, avec un tableau avant / après.

**(d) Pourquoi `formuler()` et non le marquage dans `refuser()`.** Le corps de #127 admet deux voies, la seconde étant « les littéraux du moteur sont marqués à un seul endroit (`refuser()`) ». La mesure (b) l'écarte : `refuser()` reçoit le message **après** `sprintf()`, qui a déjà échappé le gabarit, et `texte_moteur()` n'y trouve plus que de l'ASCII. Marquer dans `refuser()` ne corrigerait que les messages littéraux sans valeur, et laisserait croire que la règle y est tenue. Une signature `refuser(module, motif, gabarit, ..., details)` déplacerait le `sprintf()` dans `refuser()`, mais changerait le contrat du point 6.4 (le champ `message` est reçu formé ; `refus_tablexp()` est appelée ainsi par `tests/unitaires/test_outils.R:13`) ; `details`, placé après `...`, devrait toujours être nommé, et les aides locales de (c) suivre. Écartée par M182.

**(e) Test de suppression.** Sans `formuler()`, la règle « marquer le gabarit avant `sprintf()` » se répète sur chaque site : 93 lignes d'appel `refuser(` hors de `R/outils.R` (`grep -c` par fichier à `8cb46b3` : `donnees` 43, `simulation` 22, `rapport` 14, `exposition` 7, `brut` 5, `construire_table` 2 ; *vérifié*), plus les aides locales, et dix sites l'écrivent déjà à la main. Un site oublié ne se voit ni dans les batteries, qui installent en UTF-8, ni dans `locale_c.R` si celui-ci n'exerce pas ce refus. Avec `formuler()`, la connaissance tient en un lieu (localité), l'interface de chaque site se réduit à « un gabarit, des valeurs citées », et le script de (f) la vérifie partout : la fonction est profonde, sa suppression répandrait sa complexité chez ses appelants. À l'inverse, `citer()` (`R/donnees.R:188`) et `sans_controle()` (`R/construire_table.R:300`) sont deux alias de `neutraliser_citation()` sans autre contenu ; leur suppression ne répandrait rien. Les garder ou les remplacer par `neutraliser_citation()` reste au choix de `coder` : le script de (f) reconnaît les trois noms.

**(f) Contrôle par script (critère 1 de #127 ; principe 7 des workflows).** Un script de `tests/controles/` lit `R/*.R` par `parse()` et `utils::getParseData()`, en R de base et sans charger `tablexp` (comme `graphe_modules.R`). Il vérifie au moins :

- chaque appel de `formuler()` a pour gabarit une constante chaîne, ou un `paste()` ou `paste0()` de constantes chaînes ;
- chaque argument de `formuler()` placé sous une conversion `%s` de son gabarit est un appel de `citer()`, `neutraliser_citation()`, `sans_controle()`, `liste_guillemets()` ou `formuler()`, ou une constante ;
- l'argument `message` de chaque refus (`refuser()`, `refus_tablexp()` et les aides locales de (c)) est un appel de `formuler()` ou un texte formé par lui ; aucun `sprintf()`, `paste()` ni `paste0()` n'y combine une constante non ASCII avec une valeur ;
- toute exception figure dans une liste fermée du script, une ligne par site, avec sa raison ; le script imprime le décompte de ce qu'il recense (refus, appels de `formuler()`, valeurs citées, exceptions).

La grammaire exacte revient à `coder` : messages suivis à travers une affectation locale, aides de message comme `message_aucune_recette()` (`R/donnees.R:194`). `audit` vérifie que chaque règle a un témoin qui la fait échouer. Le script est **appelé par un test de batterie**, dans `tests/unitaires/test_controles.R`, qui exécute déjà chaque script de `tests/controles/` dans un sous-processus `Rscript`, le juge sur son code de sortie, sur le dépôt et sur des témoins (en-tête du fichier, *vérifié* par lecture). Il n'est pas une étape nouvelle de `.github/workflows/ci.yml` : la clé du cache TeX du job « Documentation » est `hashFiles('.github/workflows/ci.yml')` (`ci.yml:263`, *vérifié*), que toute modification du fichier change (M187). La liste `BATTERIES` et `identite_batteries.R` ne changent pas. Les cas du critère 2 (un refus par module sous `LC_ALL=C`, cas de `lire_texte()`) vont dans `tests/controles/locale_c.R`, déjà lancé par `ci.yml:133`.

**(g) Points 9.3 et 1.2 : `vers_utf8_locale()` dans `outils` (#113, point 1).** La conversion des chaînes en UTF-8 du point 9.3 (`enc2utf8()`, sauf une chaîne non ASCII non marquée dans une session dont la locale n'est ni UTF-8 ni latin-1) est écrite deux fois :

- dans `octets_chaines()` (`R/rapport.R:128`), après deux refus (`chaine_codage_bytes`, `chaine_codage_indetermine`) et avant un troisième (`chaine_non_utf8`) ;
- dans `ordre_octets()` (fonction locale, `R/exposition.R:335`), qui ne refuse pas et range les clés de segmentation en ordre d'octets (#106).

M182 la place une fois, dans `outils`, sous le nom `vers_utf8_locale(x)` : sous une locale UTF-8 ou latin-1 (`l10n_info()`), `enc2utf8()` sur tout ; sinon, sur les seules chaînes marquées latin-1. C'est un service de plateforme, sans contenu métier (point 1.2). `octets_chaines()` l'appelle **après** ses deux premiers refus, qui restent dans `rapport` (ils sont la règle de l'empreinte) ; `ordre_octets()` l'appelle sans refus.

Équivalence avec la conversion actuelle d'`octets_chaines()` (`enc2utf8()` sur tout) sur les entrées qu'elle ne refuse pas. Par lecture : sous une locale UTF-8 ou latin-1, les deux règles sont `enc2utf8()` ; sous C, une chaîne non ASCII `unknown` est refusée avant la conversion, et `enc2utf8()` laisse inchangées les chaînes ASCII et marquées UTF-8. Par mesure, sur un échantillon de formes (ASCII, `NA`, chaîne vide, tabulation, UTF-8 marquée, latin-1 marquée, latin-1 avec l'octet 0x80 ; sous C.UTF-8, en plus, une chaîne `unknown` aux octets UTF-8) : `identical()` vrai, octets et marques égaux, sous C et sous C.UTF-8 (*vérifié*, 05/11/2026, sur une fonction candidate écrite dans le bac à sable, non sur le code de #113). Locale latin-1 non mesurée (absente de l'hôte, `locale -a`) ; les deux règles y sont `enc2utf8()` par construction. La preuve demandée reste le test d'égalité des deux ordres de #106 prévu par #113. La règle du point 9.3 n'est pas modifiée ; elle n'a plus qu'un lieu.

**(h) Options écartées.**

- **Marquer dans `refuser()`** ; **signature `refuser(module, motif, gabarit, ..., details)`** : voir (d).
- **Littéraux écrits avec des échappements `\u`** : un littéral `"données"` est marqué UTF-8 même lu sous C (*vérifié*, `Rscript` d'un fichier sous `LC_ALL=C` : `UTF-8`, `64 6f 6e 6e c3 a9 65 73`). Cette voie corrigerait les gabarits sans fonction nouvelle. Elle est écartée pour trois raisons : la règle serait répartie sur chaque littéral au lieu d'un point de passage ; les messages, que la documentation cite mot pour mot, deviendraient illisibles dans le source ; et un littéral écrit en clair la contournerait sans bruit.
- **Exiger l'installation sous une locale UTF-8** (garde au chargement) : elle reporte le défaut sur l'utilisateur. Elle relève de #25, qui demande son propre ADR (`docs/feuille-de-route.md`, § 3), hors du périmètre de la branche.

*Mention datée du 5 novembre 2026 (fin de l'implémentation de B12, `architect` ; l'étape (6) du handoff de la PR #134 la désigne comme « mention de l'ADR 0007 (h) ») — portée mesurée de la garantie du champ `message` énoncée en (c) 4, sur l'avis d'`actuary` rapporté par ce handoff (texte de l'avis non relu par `architect`) et la surface d'impact documentaire de `1e40608` (« garantie du champ message (marque UTF-8) »).* Mesure : `tablexp` installé depuis l'arbre de `ac1a24a` (même `R/` que `6241333`, *vérifié* : `git diff --quiet 6241333 ac1a24a -- R/`) par `R CMD INSTALL` dans deux bibliothèques temporaires hors du dépôt, l'une sous `LC_ALL=C.UTF-8`, l'autre sous `LC_ALL=C` ; R 4.3.3, hôte cloud Linux (glibc) ; script exécuté sous `LC_ALL=C.UTF-8` puis `LC_ALL=C` pour chacune ; `Encoding()` et `charToRaw()` du message lu par `tryCatch(…, refus_tablexp = …)` (*vérifié*, 05/11/2026) :

| Message | Marque, quatre combinaisons installation × session | Octets |
|---|---|---|
| `colonne_absente` de `controler_roles()`, gabarit non ASCII, valeur citée UTF-8 (`"âge"`), ASCII (`"age"`) ou marquée latin-1 (`"\xe2ge"`) | `UTF-8` dans les douze cas | identiques d'une combinaison à l'autre (MD5 de la sortie du script égal dans les quatre : `16bf4d85…`) |
| `formuler("données %s", "x")` | `UTF-8` | identiques |
| `formuler("Refus %s.", "x")`, texte ASCII | `unknown` | identiques |

Portée : (1) la garantie vaut pour tout message **non ASCII** : il est marqué UTF-8 et a les mêmes octets sous les locales d'installation et d'exécution C et C.UTF-8 ; (2) un message **entièrement ASCII** reste `unknown`, R ne marquant jamais une chaîne ASCII : ce n'est pas un écart à la garantie, ses octets sont les mêmes sous toute locale ; (3) une locale latin-1 n'est **pas mesurée** (aucune sur l'hôte, `locale -a`) ; `tests/controles/locale_latin1.R` ne contrôle pas les messages de refus ; (4) la garantie porte sur le champ `message` des refus formés par `formuler()`, pas sur les erreurs de programmation (`stop()`) ni sur les avertissements (point 6.5, (c) 5). Le contrôle durable est celui de `tests/controles/locale_c.R` (un refus par module, octet à octet sous C et sous UTF-8 ; message de `1e40608`). Aucun code, aucun résultat changé.

*Mention datée du 5 novembre 2026 (seconde, `architect`, sur `c053695`) — la mention précédente (`b892a84`), contredite au même état par la revue finale.* La mesure de `ac1a24a` ne portait que sur `colonne_absente` et `formuler()` ; son point (1) était trop large. À ce même état, la revue finale d'`audit` a trouvé un contre-exemple : le refus `type_incompatible` sur un rôle date, paquet installé et session sous C, rendait un message marqué UTF-8 qui contenait le texte ASCII « enti<c3><a8>res » : le libellé « Date (valeurs entières finies) » de `TYPES_ADMIS` n'était pas marqué et était inséré par `%s` (cause de (b), sur une constante du moteur et non sur le gabarit). Corrigé par `63f029c` (`texte_moteur()` sur le libellé avant `sprintf()`) ; `tests/controles/messages_refus.R` détecte désormais une chaîne non ASCII insérée hors `texte_moteur()` (`constantes_moteur()`) ; `tests/controles/locale_c.R` couvre ce refus (23 messages comparés au lieu de 21 ; message de `63f029c`). Locale latin-1 : constat d'`actuary`, repris de `docs/latex/doc_outil_tables.tex:680-705` (à `c053695`) : paquet installé et exécuté sous `fr_FR.ISO-8859-1` (locale générée par `localedef`), le message de `colonne_absente` sort `unknown`, n'est pas de l'UTF-8 valide et porte l'octet `e9` pour « é », comme à `3ed0301` ; les autres combinaisons mesurées (installation latin-1 exécutée sous C ou C.UTF-8, installation C.UTF-8 exécutée sous latin-1) donnent un message marqué UTF-8. **Une installation exécutée sous latin-1 n'est donc pas couverte par la garantie de (c) 4** ; la portée de la garantie est une question soumise au mainteneur. La règle de (c) n'est pas modifiée ; aucun résultat ne change.

**Aucune arête nouvelle** du graphe du point 2.1 : `formuler()` et `vers_utf8_locale()` sont dans `outils`, que tout module peut appeler (`tests/controles/graphe_modules.R:46-58` : `ORDRE_MODULES` commence par `outils`, `simulation` admet `donnees` et `outils` ; *vérifié* par lecture) ; `citer()` et `liste_guillemets()` restent dans `donnees`.

**Effet sur les résultats : aucun** par cette annotation, qui ne modifie aucun code. Effet attendu de #127 et de #113 (critère 4 de #127 ; `docs/feuille-de-route.md`, § 1.12) : plans, objets, empreintes du résultat et `.tsv` de R1 à R6 identiques ; fiches et rapports figés différents aux seules lignes de l'empreinte du code (M70) ; avertissements identiques, marques comprises ; aucune régénération. Seuls changent les octets des messages de refus d'un paquet installé sous une locale non UTF-8, et la marque de certains messages d'un paquet installé en UTF-8 (*hypothèse*, à mesurer par `coder`). Un message de refus n'entre ni dans l'objet ni dans la fiche.

Issues : #127 ; #113 (point 1) ; #94 (règle de citation) ; #106 (ordre d'octets) ; #128 (après #127) ; #25 (hors périmètre). Décisions M181, M182, M183. Branche `claude/dette-regles`, PR #134.

## Annotation du 5 novembre 2026 (seconde) — contrat « format des jeux simulés » (annotation du 25/10/2026, (c), points 6 et 9) : refus `defauts_invalides` avant tout tirage d'une déclaration dont la cible est structurellement vide (#112, #135, M195) ; précédent de la cible disjointe consigné

Rédigée par `architect` (fiche de base ; modèle servi à confirmer par `bash .claude/outils/bilan_journal.sh`), étape (5) de la branche `claude/dette-regles` (PR #134), avant tout code de #112 et de #135. Les « points 6 et 9 » visés sont ceux du contrat « format des jeux simulés » de l'annotation du 25/10/2026, (c) (argument `defauts` ; refus), non les points 6 et 9 de la décision initiale. Le texte ci-dessus, annotations comprises, n'est pas réécrit. Les numéros de ligne datent de `c253f65` ; le nom de l'objet fait foi (M77, (b)).

**Sources.** Décisions du mainteneur du 05/11/2026 et avis d'`actuary-approfondi` (étape (0') de B12), versés en commentaire de #112 (https://github.com/Gerard-Garey/outil_tables/issues/112#issuecomment-6660735086), ci-après « l'avis » ; issue #135, ajoutée au périmètre par M195. Les décisions du mainteneur sur #112 (lecture (a), portée) ne portent pas encore de numéro M (dernier numéro au § 4 de `docs/feuille-de-route.md` : M205) ; leur numérotation revient à la porteuse. Les chiffres du prototype de l'avis (76 cas, 3,26 s à n = 10⁶) sont *non remesurés ici*.

**(a) Constat remesuré à `c253f65`.** R 4.3.3, `sys.source()` de `R/*.R` dans un environnement (paquet non installé), n = 2 000, période 01/01/2010 – 31/12/2014, graine du jeu 1, graine des défauts 2, une déclaration (*vérifié*, 05/11/2026, script du bac à sable) :

| portefeuille | déclaration | résultat à `c253f65` |
|---|---|---|
| `uniforme` | `tete_sexe_incoherent`, `taux = 0,5` | jeu rendu, `eligibles = 0` |
| `uniforme` | `tete_sexe_incoherent`, `nombre = 3` | refus `defauts_ineligibles` (après le tirage du jeu propre) |
| `age_exact`, `date_entree = fin` | `en_cours_avant_fin`, `cible = "en_cours"`, `taux = 0,5` | jeu rendu, `eligibles = 0` |
| `age_exact`, `date_entree = fin` | `en_cours_avant_fin`, `cible = "en_cours"`, `nombre = 1` | refus `defauts_ineligibles` (après le tirage du jeu propre) |
| `uniforme` | `en_cours_avant_fin`, `cible = "deces"`, `taux = 0,5` | refus `defauts_invalides` (avant tout tirage : cible disjointe) |

La dernière ligne est le précédent de (b) ; les quatre premières, les cas de #112 et de #135.

**(b) Précédent de la cible disjointe, consigné.** Le code de #93 (`63adaf9`, 25/10/2026) lève `defauts_invalides` avant tout tirage quand la `cible` d'une déclaration de ligne est disjointe des causes que le catalogue admet pour son type (pour une combinaison, pour tous ses types) : `controler_defauts()`, `R/simulation.R:587-598`, sur un « complément de spécification d'`actuary`, 25/10/2026 » cité en commentaire. Cette condition ne figure pas dans la liste du point 9 de l'annotation du 25/10/2026, (c) ; elle y est ajoutée par la présente annotation, sans changement du code. De même, le refus `defauts_invalides` quand `defauts$graine` égale la graine du jeu (`R/simulation.R:542-547`), recommandé par l'annotation du 25/10/2026, (f), et retenu par la réponse d'`actuary` (commentaire d'`ORDRE_TIRAGES_DEFAUTS`, `R/simulation.R:143-145`), est un refus avant tout tirage que la liste du point 9 ne nomme pas ; il y est ajouté au même titre (*vérifié* par lecture du code).

**(c) Règle (décision du mainteneur du 05/11/2026, lecture (a) ; avis).** Une déclaration dont la cible est **structurellement vide** — vide connu avant tout tirage, par le seul portefeuille déjà validé (`controler_portefeuille()`), la période et le catalogue, quelle que soit la graine — est refusée par **`defauts_invalides`, avant tout tirage**, en `taux` comme en `nombre`, valeur zéro comprise. **Aucun code de refus nouveau.** Deux cas aujourd'hui, en plus de la cible disjointe de (b) :

1. **#112 — type de tête dans un portefeuille à un seul contrat par tête.** Types `tete_naissance_incoherente`, `tete_sexe_incoherent`, `tete_observee_apres_deces` (condition du catalogue `tete_multi` ou `tete_multi_deces`, `R/simulation.R:118`), dans les modes `uniforme` et `age_exact`, et dans les modes `uniforme_multi` et `age_exact_multi` dont `length(loi_contrats) == 1`. Les modes multi dont `loi_contrats` a au moins deux probabilités sont inchangés : une tête n'y a qu'une ligne selon le tirage (K, contrats non souscrits), vide qui dépend de la graine. Équivalence : sous le point 1 de l'annotation du 25/10/2026, (c) (`p_m > 0`), K ≡ 1 si et seulement si `length(loi_contrats) == 1` ; `loi_contrats = c(0, 1)` donne K ≡ 2 et n'est pas un cas de refus.
2. **#135 — `en_cours_avant_fin` quand aucune entrée ne précède la fin** (condition `entree_avant_fin`, `R/simulation.R:118` et `:660`) : `date_entree == fin` en `age_exact` et `age_exact_multi` ; `origine == fin` en `uniforme` et `uniforme_multi`. Ces égalités sont exactement les cas vides : `controler_portefeuille()` impose déjà `date_entree <= fin` et `origine <= debut <= fin` (`R/simulation.R:331`, `:381`, `:402` ; *vérifié* par lecture).

Raisons (avis) : c'est la règle déjà appliquée à la cible disjointe ; le refus ne dépend pas de la graine ; un jeu rendu avec `eligibles = 0` laisserait passer un test vide (fausse assurance dans un dossier de certification) ; le simulateur n'a pas d'accumulateur d'avertissements, et un signal changerait la structure de l'attribut `defauts`.

Un vide qui **dépend du tirage** (cause absente du jeu propre, aucune tête à deux lignes en mode multi, aucune entrée tirée avant `fin` quand `origine < fin`) reste traité comme au point 9 de l'annotation du 25/10/2026 : refus `defauts_ineligibles` après le tirage du jeu propre si `nombre` dépasse les éligibles, jeu rendu avec `eligibles = 0` en `taux`. Le commentaire de `R/simulation.R:587-591`, qui range « condition entree < fin, conditions de tête » parmi les vides qui dépendent du jeu, devient inexact et se corrige avec le code.

**(d) Emplacement et forme.**

- Dans `controler_defauts()`, dans la boucle des déclarations, prises dans l'ordre reçu : **après** le contrôle « champ cible interdit » (`:575-576`), le contrôle de forme de `cible` (`:580-583`) et le refus de la cible disjointe (`:592-598`), **avant** le contrôle `taux` / `nombre` (`:600`). La première déclaration fautive lève le refus ; une déclaration de forme `taux` ou `nombre` invalide sur une cible vide reçoit donc le refus de la cible vide.
- `details = list(parametre = "defauts$declarations", declaration = k)`, forme des autres refus de la boucle (aide locale `inv()`, `:555-556`).
- Message formé par `formuler()`, valeurs reçues citées (annotation du 05/11/2026, (c)) ; il nomme le type (critère 1 de l'avis) et la raison (« un seul contrat par tête », « aucune entrée avant la fin de la période »).
- **Information** : lue dans le portefeuille déjà validé et la période, passés à `controler_defauts()` par `simuler_portefeuille()` (`:836`, `:848`), sans nouvelle validation de `loi_contrats` ni de `periode`. Forme recommandée par `architect` (le détail revient à `coder`) : un seul calcul, dans `simulation`, de l'ensemble des **conditions du catalogue structurellement vides** pour ce portefeuille (`tete_multi` et `tete_multi_deces` si un seul contrat par tête ; `entree_avant_fin` si aucune entrée ne précède la fin), puis un seul test par déclaration, « une condition de son type (ou d'un type de sa combinaison) est vide ». La règle s'adosse ainsi à la colonne `condition` du catalogue, celle que lit `injecter_defauts()` pour les éligibles (`:660-664`) : un type ajouté avec l'une de ces conditions hérite du refus sans site nouveau (localité). Aujourd'hui, ce test est équivalent à la liste des types de (c) (catalogue, `:107-118`, et `COMBINAISONS_DEFAUTS`, `:141`, qui ne combine aucun type à condition ; *vérifié* par lecture).
- Antériorité : `controler_defauts()` est appelée avant `sous_graine()` (`:848`, `:859`) ; le refus précède donc le refus `graine_invalide` d'une graine du jeu invalide (critère 2 de l'avis), et l'aléa de l'appelant est inchangé.

**(e) Points du contrat modifiés** (annotation du 25/10/2026, (c) ; changement étroit du contrat partagé, routage, § 4.2) :

- **point 6** (domaine admis) : une déclaration n'est admise que si sa cible n'est pas structurellement vide au sens de (c) ; le domaine de `taux` (réel de [0, 1]) et de `nombre` (entier ≥ 0) n'est pas modifié, mais il ne s'applique plus qu'aux déclarations admises ;
- **point 9** (refus) : la liste des conditions de `defauts_invalides` levé avant tout tirage s'augmente de la graine des défauts égale à celle du jeu et de la cible disjointe (constat, (b)), et de la cible structurellement vide ((c), #112 et #135). `defauts_ineligibles` ne couvre plus que les vides qui dépendent du tirage.

Inchangés : les points 1 à 5, 7, 8, 10 et 11 ; l'attribut `defauts` (aucun champ ajouté) ; les tirages et l'ordre des tirages des jeux admis ; le graphe du point 2.1 (aucune arête nouvelle : tout le code reste dans `R/simulation.R`) ; le décompte des codes de refus de l'annotation du 25/10/2026, (e) (aucun code nouveau).

**(f) Options écartées** (avis).

- **Signal (avertissement ou champ de l'attribut) au lieu d'un refus** : le simulateur n'a pas d'accumulateur d'avertissements (point 6.5 vaut pour `construire_table()`) ; un champ nouveau changerait l'attribut `defauts` ; un test vide passerait.
- **Garder `defauts_ineligibles` après tirage pour `nombre`** : le refus dépendrait du tirage alors que le vide ne dépend que du portefeuille ; il coûte le tirage complet du jeu (3,26 s à n = 10⁶ selon l'avis, *non remesuré ici*).
- **Code de refus nouveau** (`defauts_cible_vide`…) : écarté par le mainteneur ; `defauts_invalides` désigne déjà toute déclaration irrecevable connue avant tirage, cible disjointe comprise.
- **Refuser aussi les vides qui dépendent du tirage avant tirage** : impossible sans tirer ; ce serait deviner.

**Effet sur les résultats : aucun** par cette annotation, qui ne modifie aucun code. Effet attendu des commits (5) et (5') (avis, mesuré sur prototype hors dépôt, *non remesuré ici*) : aucune table finale, aucun verdict, aucune régénération ; R1 à R6 et `jeu_reference()` identiques ; seul le jeton de l'empreinte du code change (M70). Des appels auparavant rendus (`taux` ou `nombre = 0` sur une cible structurellement vide) deviennent des refus `defauts_invalides`, et des refus `defauts_ineligibles` après tirage deviennent des refus `defauts_invalides` avant tirage. Surface d'impact documentaire (avis, critère 8) : `docs/latex/doc_outil_tables.tex:8194-8199` et `:8353-8367`, au passage de `docwriter` (règle 9).

*Mention datée du 5 novembre 2026 (`architect`, sur `c053695`) — (c) 1, portée de l'équivalence, constat d'`actuary` en revue finale.* L'équivalence « K ≡ 1 si et seulement si `length(loi_contrats) == 1` » est exacte sur la **loi déclarée**, pas sur la **loi tirée**. `loi_contrats = c(1, 1e-13)` passe le contrôle (`p_m > 0`) et donne K ≡ 1 quelle que soit la graine — résultat exact établi par `actuary` par lecture du code (`cumsum` en double ; `runif()` ne rend jamais 1), *non remesuré par `architect`* —, mais n'est pas refusée avant tirage : une déclaration de type tête y est refusée par `defauts_ineligibles` **après** tirage en `nombre`, et rendue avec 0 éligible en `taux`. Même chose pour une transition de loi nulle : c'est un vide connu de la loi, hors de la règle de (c), qui ne lit que le portefeuille validé, la période et le catalogue. La présente mention signale l'écart ; elle ne change pas la décision (M206, M207) ni le code. Suites : deux issues proposées au mainteneur par `actuary` (création sur son accord).

Issues : #112, #135 ; #93 (refus de la cible disjointe, `63adaf9`). Décisions M195 et décisions du mainteneur du 05/11/2026 sur #112 (commentaire cité). Branche `claude/dette-regles`, PR #134.

## Annotation du 6 novembre 2026 — points 3.1, 6.2 et 7.1 : entrée d'une table de référence (argument `tables_reference`), champ `fiche$tables_reference`, ordre canonique rendu par `table_reference()` (ADR 0015, M239 ; décision A, M235 ; #73)

Rédigée par `architect` sur la branche porteuse `claude/validation-smr-reference` (B13, PR #155), à `b146631`, après l'acceptation de l'ADR 0015 par le mainteneur le 06/11/2026 (M239). Le texte ci-dessus, annotations comprises, n'est pas réécrit ; le nom de l'objet fait foi (M77, (b)). Aucun code n'est encore écrit : la mise en œuvre est le commit (1) de B13 (#73).

**(a) Point 3.1 : cinquième argument.** `construire_table(donnees, roles, plan, provenance = NULL, tables_reference = NULL)`. `tables_reference` est `NULL` ou une liste nommée d'objets `table_reference` ; ses noms sont des **clés**, que le plan cite (un plan cite, il ne contient pas). Par la règle du présent point (« l'ajout d'un argument nommé facultatif ne change pas la signature `construire_table(donnees, roles, plan)` des ADR 0004 et 0005 ni les appels existants »), la signature des ADR 0004 et 0005 est inchangée. Les contrôles de l'argument (forme, clé absente, clé inattendue, table de statut `embarquee` vérifiée contre le package) s'exécutent après `valider_plan()` et avant toute étape ; l'ensemble des paramètres du plan qui citent une clé est une constante du catalogue de `R/construire_table.R` (ADR 0015, points 1.1 à 1.3). L'argument ne porte jamais une seconde table d'observations (ADR 0014, point 9). Le point 5.1 est inchangé : `ETAPES$aleatoire` reste une constante par étape, l'étape `validation` est aléatoire, `B ≥ 1` sans défaut (Q7 (a) de l'ADR 0015, point 7.2).

**(b) Point 6.2 : fiche.** Champ nouveau `tables_reference`, après `empreintes` et avant `plan` : `NULL` sans table, sinon, par clé, `nom`, `statut`, `source`, `forme_origine`, `parametres`, `representation`, `empreinte` (ADR 0015, points 3.1 et 3.2 ; `version_fiche` 5 au commit (1) de B13). La phrase « Rejeu » se lit désormais : `construire_table(donnees, fiche$roles, fiche$plan, fiche$provenance, tables_reference)`, avec les tables dont la fiche porte l'empreinte ; le rejeu outillé les assemble (tables reçues plus tables embarquées relues par leur `nom`) et contrôle chaque empreinte avant de construire (ADR 0012, annotation du même jour).

**(c) Point 7.1 : représentation commune.**
- `table_reference()` rend l'objet **trié** par rang de `sexe` dans `SEXES_TABLE` (`H`, `F`, `ensemble`), puis `annee`, puis `age` ; le refus d'un âge non croissant (`table_ages_non_croissants`) est retiré ; les refus d'âges en double et non contigus sont maintenus et s'évaluent après le tri (décision A, M235 ; ADR 0015, point 2.1).
- `validation` et `suivre_experience()` reçoivent leurs tables par l'argument `tables_reference`, sous la classe `table_reference` (ADR 0015, points 1 et 7.5). Une table synthétique entre en valeurs, statut `synthetique` (décision B, M236 ; point 2.3).
- Tables embarquées : accesseurs exportés `table_embarquee(nom)` et `tables_embarquees()` dans `donnees` ; mise en œuvre par la branche de transcription (ADR 0015, point 6).
- `annee` : son sens (année calendaire ou génération) n'est pas fixé ; s'il doit être déclaré, ce sera une métadonnée de `table_reference()`, par annotation du présent point et de l'ADR 0015 (point 7.4).

**Effet sur les résultats : aucun** par la présente annotation. Le tri est l'identité sur les deux tables de `jeu_reference()` (*vérifié* par `architect`, ADR 0015, « Contexte ») ; l'effet de B13 sur R1 à R6 est celui de l'ADR 0015, point 3.4.

Issues : #73, #61 (Q7). Décisions M235, M236, M239. ADR 0015 ; ADR 0011 et ADR 0012 (annotations du même jour) ; ADR 0014 (point 9). Branche `claude/validation-smr-reference`, PR #155.

## Annotation du 6 novembre 2026 (seconde) — points 2.1, 5.1 et 5.2 : étapes `validation` et `importance_exclusions`, paramètres `double` à valeur entière, transmission par l'orchestration (M242, M246 ; #4, #69)

Rédigée par `architect` sur la branche porteuse `claude/validation-smr-reference` (B13, PR #155), à `78b6052`, d'après l'avis de forme (0'') et les décisions du mainteneur du 06/11/2026, versés en commentaire de #4 (https://github.com/Gerard-Garey/outil_tables/issues/4#issuecomment-6679524403). Le texte ci-dessus, annotations comprises, n'est pas réécrit ; noms indicatifs, le nom de l'objet fait foi (M77, (b)). Mise en œuvre : commits (3) (#4) et (4b) (#69) de B13.

**(a) Point 5.1 : deux noms d'étapes nouveaux (M242).** `ETAPES` reçoit `validation` (préalables `donnees` et `exposition`, `aleatoire = TRUE`) puis `importance_exclusions` (mêmes préalables, `aleatoire = FALSE`), toutes deux du module `validation`. `brut` n'est **pas** un préalable de l'une ni de l'autre. `ETAPES$aleatoire` reste une constante par étape (ADR 0015, point 7.2) ; la forme de la graine est contrôlée dans `valider_plan()` avant `graine_inattendue` et `graine_manquante` (#61, M243). La liste indicative des noms d'étapes du point 5.1 se lit avec `importance_exclusions` en plus.

**(b) Point 5.2 : paramètres, tous requis et sans défaut.** `validation` : `table_reference` (clé de table), `relation_table`, `plage_ages`, `tranches_age`, `niveau`, `replications` (B ≥ 1) ; `importance_exclusions` : `niveau`. Les paramètres à valeur entière (`replications`, `plage_ages`, `tranches_age`) sont des **`double` à valeur entière** (M246) ; la canonisation ne change pas (ADR 0011). Le sort d'un `integer` reçu (refus, comme pour `niveau`, ou admission, comme pour `delai_declaration` ; annotation du 19/10/2026) relève de la spécification d'`actuary-approfondi` et de `coder`. Règle « présent ou refusé » inchangée.

**(c) Transmission (point 6 ; seconde annotation du 19/10/2026).** Rien hors objet : l'orchestration passe à `validation` les cellules d'`etapes$exposition`, et à `importance_exclusions` l'**entrée `donnees`**, `lignes_exclues` et `etapes$validation` (comparateur 2, produit si et seulement si `validation` est au plan). Aucune structure sur le modèle de `fenetres`. L'identité de la note II1291-14 entre dans `etapes$validation$parametres$reference_methode` (M244) ; `etapes$importance_exclusions` ne porte que des agrégats, aucun vecteur par ligne. Ajout compatible au sens du point 6.3.

**(d) Point 2.1 : arêtes effectives du module `validation`.** Le tableau autorise « `fermeture` et amont » ; le module `validation` de B13 n'appelle que `outils`, `donnees`, `exposition` et `brut` (`exposition_retiree()` appelle `calculer_exposition()`, `taux_poisson()` et deux fonctions de `donnees`, factorisées à résultats identiques au commit (4a)). Aucune arête vers `rapport`, `construire_table` ni `simulation`. *Vérifié* (avis de forme) : `graphe_modules.R` OK à `78b6052`, avant tout code du module ; le script reste le contrôle.

**Effet sur les résultats : aucun** par la présente annotation ; pour R1 à R6, aucune étape nouvelle au plan.

Issues : #4, #69, #61. Décisions M242, M243, M244, M246 ; M215, M217. ADR 0015 et ADR 0012 (annotations du même jour, secondes) ; ADR 0011. Branche `claude/validation-smr-reference`, PR #155.

## Annotation du 7 novembre 2026 — forme (P) de #82 (annotations du 17/10/2026, (c) et (d), du 21/10/2026, (a), (c) et (d), seconde du 06/11/2026, (c)) ; point 2.1 : module `positionnement` ; points 5.1 et 5.2 : étapes `positionnement` et `homogeneite` (M216, M291, M292)

Rédigée par `architect` à l'étape (A) de la branche `claude/positionnement-smr` (B14, PR #173), à `0d3e45f`, d'après M216, l'avis (P) rafraîchi (M291 ; https://github.com/Gerard-Garey/outil_tables/issues/82#issuecomment-6705782605), l'avis de forme (0'') (M292) et le code de `4ef26bd`, `e266f46`, `7389e8e`, `5efa338`, `35da74b`. Le texte ci-dessus, annotations comprises, n'est pas réécrit. Numéros de ligne à `0d3e45f` ; le nom de l'objet fait foi (M77, (b)).

**(a) Forme (P) livrée par `4ef26bd` (#82).**
1. **Annotation du 17/10/2026, (c)** : les fonctions d'étape reçoivent, outre des champs du plan, les **valeurs validées** `p` (transition, période, délai), `recette` et `conv`, par `construire_table()`. Le motif « pas de seconde source » s'étend à la recette et à la convention. Contrôle des entrées en deux couches dans `R/construire_table.R` : `controler_plan_roles()` (l. 636 : `valider_plan()`, `canoniser_plan()`, `controler_roles()`, `deduire_recette()`, `resoudre_convention()`, `controler_provenance()`), partagée avec `rejouer_fiche()` ; `controler_entrees()` (l. 659 : plus le contrôle 1.3 et le contrôle d'entrée de chaque étape qui lit une table), appelée une fois par `construire_table()`.
2. **Forme décidée par `architect` pendant (2)** (message de `4ef26bd`) : `p` est recalculé par la fonction pure `valeurs_parametres_donnees()` dans `controler_plan_roles()` ; `valider_plan()`, exportée, est inchangée (l'avis (P) prévoyait qu'elle rende `p`) ; argument `etape` retiré de `controler_segmentation()` (seul appelant : `controler_parametres_donnees()`, module `donnees`) ; **un seul commit `moteur:`** au lieu des deux du critère 7 de l'avis (dérogation notée au corps de la PR) ; **D7 fermé par construction**, sans retouche d'`assembler_fiche()` : la convention de la fiche est celle que l'étape `exposition` a reçue de l'orchestration.
3. **Annotation du 17/10/2026, (d), et du 21/10/2026, (a)** : le second appel des contrôles dans les fonctions d'étape **n'existe plus**, ni l'argument `controler`. **Fin des refus en appel direct** (M216), étendue par M291 aux variantes `segmentation_invalide` des modules `brut`, `validation` et `importance_exclusions` ; les codes restent « un par condition », levés par `valider_plan()` sous le module qui détecte. `recette_branche()` est intégrée à `deduire_recette()`, `convention_retenue()` à `resoudre_convention()`.
4. **Annotation du 21/10/2026, (c)** : contrats inchangés, *vérifiés* par `architect` à `0d3e45f` : `git diff --stat 2247794 HEAD -- NAMESPACE` vide ; `formals(construire_table)` = `donnees, roles, plan, provenance, tables_reference`, `formals(rejouer_fiche)` = `donnees, fiche, tables_reference` ; `tests/controles/graphe_modules.R` OK. L'option écartée de `fa4ca6f` reste écartée. La table des comptes de (b) est remplacée par les tests de comptes de `tests/unitaires/test_construire_table.R` et `test_validation.R:583-612` (M281 : au rejeu, couche plan-rôles 2, contrôle 1.3 et `controler_entrees_validation()` 1, `construire_table()` 1).
5. **Annotation du 21/10/2026, (d) : réserve levée**, D5 compris : `calculer_exposition()` reçoit `recette` et `conv` ; `calculer_importance_exclusions()` aussi, et `exposition_lignes()` les transmet aux expositions contrefactuelles (test « deuxième recette fictive »).
6. **Seconde annotation du 06/11/2026, (c)** : `importance_exclusions` reçoit en plus `p`, `recette` et `conv` ; `validation` reçoit `etapes$positionnement` (`NULL` en son absence), lu sous la seule relation `positionnee`. Rien ne passe hors de l'objet.

**(b) Point 2.1 : module `positionnement` (`e266f46`, F1).** `R/positionnement.R` existe ; arêtes effectives (*vérifiées*, `graphe_modules.R` à `0d3e45f` : 323 symboles, 9 modules, 353 arêtes, « OK ») : `positionnement → donnees` (12), `→ outils` (22) ; `validation → positionnement` (11) ; `construire_table → positionnement` (3). `positionnement` n'appelle ni `brut` ni `exposition` ; `rapport` ne l'appelle pas. Règle de placement retenue (F1) : une fonction partagée vit dans le module le plus en aval qui précède tous ses appelants, ou qui en est un.

**(c) Points 5.1 et 5.2 : deux étapes nouvelles.** `ETAPES` vaut, dans l'ordre : `donnees`, `exposition`, `brut`, `positionnement`, `homogeneite`, `validation`, `importance_exclusions` (`R/construire_table.R:74`).
- `positionnement` (#167, F4) : module `positionnement`, `calculer_positionnement()`, préalables `donnees` et `exposition`, non aléatoire ; paramètres requis sans défaut `methode` (`"smr"` seule en v1, M293), `table_reference` (clé, ajoutée à `PARAMETRES_CLE_TABLE`), `plage_ages`, `niveau` ; refus de paramètres sous le module `positionnement` (`methode_invalide`, `cle_table_invalide`, `plage_ages_invalide`, `niveau_invalide`), d'entrée (`convention_positionnement_non_admise`, …), d'étape (`positionnement_sans_attendu`, `positionnement_sans_deces`).
- `homogeneite` (#168, F10) : module `validation`, `calculer_homogeneite()`, préalables `donnees` et `exposition`, **aléatoire** (la graine est requise dès qu'elle est au plan) ; paramètres `plage_ages`, `tranches_age`, `replications`, `double` à valeur entière, sans défaut ; refus inter-champs `homogeneite_sans_segmentation`, levé par `controler_parametres_homogeneite()` appelée par `valider_plan()` ; aucune table citée.
- Refus inter-étapes de `validation` sous `positionnee`, levés par `valider_plan()` : ADR 0015, annotation du même jour, (a) 3.
- La liste indicative des noms d'étapes du point 5.1 se lit avec `positionnement` et `homogeneite`. *Vérifié* : `ETAPES$aleatoire` = `FALSE FALSE FALSE FALSE TRUE TRUE FALSE`.

**Effet sur les résultats : aucun** par la présente annotation. R1 à R9 identiques hors jeton du code (messages de `4ef26bd` à `35da74b`).

Issues : #82, #164 (point 2), #167, #168. Décisions M216, M281, M291 à M294. ADR 0012, 0014, 0015 (annotations du même jour). Branche `claude/positionnement-smr`, PR #173.

## Annotation du 8 novembre 2026 — points 3.1, 3.2, 4.1, 5.1 à 5.4 ; annotation du 25/10/2026, (c) 4 et (c) 11 : provenance de k fichiers, argument `forme` du simulateur, rôles des recettes nouvelles, paramètre conditionnel à la recette, recette déduite dans `donnees` (ADR 0016, M335 ; M327 à M334 ; #169, #180, #181)

Rédigée par `architect` à l'étape (0''') de la branche `claude/recette-stocks-successifs` (B15, PR #188), à `9308adc`, après l'acceptation de l'ADR 0016 par le mainteneur le 08/11/2026 (points A à E ; F accordé ; M335). Sources : l'ADR 0016 (« ADR remplacés en partie », « Conséquences ») ; l'avis de forme (0'') accepté (G1 à G15, M329 à M334 ; https://github.com/Gerard-Garey/outil_tables/issues/169#issuecomment-6723637706) ; les spécifications (0') acceptées de #169 (M327 ; https://github.com/Gerard-Garey/outil_tables/issues/169#issuecomment-6723164336) et de #180 (M328 ; https://github.com/Gerard-Garey/outil_tables/issues/180#issuecomment-6723399774). Le texte ci-dessus, annotations comprises, n'est pas réécrit ; noms indicatifs, le nom de l'objet fait foi (M77, (b)). Aucun code n'est écrit par la présente annotation : mise en œuvre par les commits (3) à (5) de B15.

**(a) Point 3.1 : provenance de k fichiers (#181 ; M332 ; ADR 0016, point 8.5).** `provenance` est `NULL` ou `list(fichier, empreinte, algorithme)`, trois vecteurs `character` **parallèles** de même longueur k ≥ 1, sans noms ni `NA`, dans l'ordre de la couche de lecture (inventaires puis événements pour les stocks successifs). La forme « liste de trois chaînes uniques » (annotation du 19/10/2026, #21) est le cas k = 1, inchangé. Signature de `construire_table()` inchangée. `fiche$empreintes$fichier` et `fiche$empreintes$algorithme_fichier` deviennent des vecteurs de longueur k ; aucun incrément de `version_fiche` à ce titre (M333 ; ADR 0011, annotation du même jour). R1 à R12 inchangés (`provenance = NULL`).

**(b) Point 3.2 : argument `forme` de `simuler_portefeuille()` (G11 ; ADR 0003, point 9, annotation du même jour).** La signature devient `simuler_portefeuille(loi, portefeuille, periode, graine, defauts = NULL, forme = NULL)`. `forme = NULL` donne les lignes individuelles et n'est pas une valeur choisie à la place de l'appelant (même lecture que `defauts = NULL`, annotation du 25/10/2026, (b)) ; sinon `list(nom = "agregats", transition = list(depart, arrivee), convention = "dernier_anniversaire", par_annee)` ou `list(nom = "stocks_successifs", inventaires = <Date>)`. La forme est contrôlée avant tout tirage (refus `forme_invalide`, module `simulation`) ; le jeu propre est tiré sans changement, puis projeté sans appel à `exposition`. La sortie n'est plus toujours « un `data.frame` de lignes individuelles » : c'est la table de la forme demandée, colonnes aux noms des rôles. Aucun export nouveau.

**(c) Point 4.1 : rôles (ADR 0003, point 2, annotation du même jour).** Le catalogue reçoit `exposition_centrale`, `exposition_initiale` (famille `agregats`, Q1 de #169) et `type_ligne` (famille `observation`, modalités `inventaire` et `evenement`, § 3 de #180) ; le rôle `exposition` est retiré ; type nouveau `numerique`. La forme de `roles` (vecteur de chaînes nommé) est inchangée. La liste des refus du point se lit avec la sélection par catalogue (ADR 0016, point 2.2) : rôle qu'aucune recette n'emploie ou non employé par la recette retenue (`role_non_pris_en_charge`), discriminants de deux recettes (`recettes_concurrentes`), aucune recette satisfaite (`aucune_recette`, par recette) ; pour les stocks successifs, exactement un identifiant (`identifiants_concurrents`, `identifiant_inadapte` ; QS3 de #180), refus de la matrice (ADR 0016, point 5.2).

**(d) Point 5.1 : champ conditionnel `inventaires` et paramètres conditionnels (ADR 0014, point 4 ; ADR 0016, point 7 ; M331).**
1. `inventaires` entre dans `CHAMPS_PLAN` après `delai_declaration`, hors de `CHAMPS_PLAN_REQUIS` (modèle de `graine`) : vecteur de `Date`, au moins deux valeurs, strictement croissantes, sans `NA` ni attribut autre que la classe, aligné sur la période (t₀ = début − 1, tₘ = fin effective ; QS2 de #180, M328). Forme et alignement contrôlés par `valider_plan()` (`inventaires_invalides`, `inventaires_periode`) ; présence contrôlée après la recette dans `controler_plan_roles()` (`inventaires_manquants` sous `stocks_successifs`, `inventaires_inattendus` sinon), module `construire_table`.
2. « La liste nommée des paramètres de l'étape, tous requis » se lit : **tous requis sous la recette retenue** (ADR 0016, point 7.3). Un paramètre d'étape est requis sous les recettes que le catalogue lui attribue et interdit sous les autres ; un paramètre sans recette est requis sous toutes. Seul paramètre conditionnel en B15 : `entree_entre_inventaires` de l'étape `exposition` (`premier_inventaire` ou `milieu`, sans défaut ; QS1 de #180), attribué à `stocks_successifs`. « Aucun défaut implicite » (ADR 0005, point 3) est conservé : l'absence du paramètre sous sa recette est un refus, jamais un défaut.

**(e) Points 5.2 et 5.4 : catalogue et refus (M331 ; ADR 0016, points 7.2 et 7.3).** `PARAMETRES_ETAPES` gagne une colonne `recette` (`NA_character_` pour les paramètres existants, `"stocks_successifs"` pour `entree_entre_inventaires`) ; la règle « présent ou refusé » du point 5.2 reste vraie, la condition de présence étant lue dans le catalogue. Au point 5.4 : `parametre_inconnu` inchangé (nom hors du catalogue de l'étape) ; `parametre_manquant` levé par `valider_plan()` pour les paramètres sans recette, et par `controler_plan_roles()`, après la recette, pour un paramètre conditionnel absent sous sa recette (même motif, message qui nomme la recette) ; motif nouveau **`parametre_inattendu`** (module `construire_table`, nom à viser par `actuary`) pour un paramètre conditionnel présent sous une autre recette. `valider_plan()`, exportée, ne connaît pas les rôles : elle ne peut lever ni `parametre_inattendu` ni l'absence d'un paramètre conditionnel.

**(f) Point 5.3 : recette déduite dans `donnees` (ADR 0016, points 1, 2 et 5).** La recette reste déduite des rôles, jamais un paramètre du plan ; elle l'est désormais par `deduire_recette(roles, catalogue = CATALOGUE_RECETTES)`, **module `donnees`**, appelée par `controler_roles()`, qui rend la recette (le refus `aucune_recette` porte le module `donnees` dans tous les cas) ; `controler_plan_roles()` ne l'appelle plus à part. Après `resoudre_convention()`, `controler_plan_roles()` appelle `controler_matrice_recette()` (module `donnees`) : un plan non admis sous la recette des données (estimateur, étape, exclusion) est refusé avant toute étape, dans la couche partagée avec `rejouer_fiche()` (contrôle 6 du rejeu). Sous la recette `agregats`, « seul un forçage vers une convention plus grossière est un paramètre » se lit avec la convention **déclarée** (`plus_fine` refusé, aucun forçage ; Q2 de #169). Graphe d'appels inchangé : `deduire_recette()` passe à un module inférieur (point 2.1).

**(g) Annotation du 25/10/2026, (c) 4 et (c) 11 : format des jeux simulés.** (c) 11 : les formes « agrégats » et « stocks successifs » **ne sont plus hors périmètre** ; elles entrent par B15 (commits (3c) et (4c)), avec les refus de (b) ; les défauts paramétrés propres à ces formes restent hors périmètre (#189, #191). (c) 4 : les « sept colonnes actuelles » restent celles de la forme lignes, inchangées ; la forme « agrégats » rend `sexe`, `age`, [`annee`], `exposition_centrale`, `exposition_initiale` (sans `annee` seulement), `evenements` (#169, § 11) ; la forme « stocks successifs » rend la table empilée de l'ADR 0014, point 1, aux colonnes des rôles de la recette, dont `type_ligne` et `date_inventaire` (#180, § 11). (c) 5 : l'attribut `simulation` des formes nouvelles est celui du jeu propre, plus un champ `forme` ajouté en fin (ajout compatible) ; celui de la forme lignes est inchangé, et `jeu_reference()` reste `identical()`. Le contrat « format des jeux simulés » est touché (routage, § 4.2) : ADR 0016, « Conséquences ».

**Effet sur les résultats : aucun** par la présente annotation. Par B15, celui de l'ADR 0016, « Conséquences » (pour R1 à R12 : `fiche/version_fiche` dans le corps, empreintes et métadonnées d'exécution dans l'en-tête ; messages et motifs de refus visés par `actuary`).

Issues : #169, #180, #181 ; #189, #191 (défauts des formes nouvelles) ; #193 (export du catalogue). Décisions M327, M328, M331, M332, M335. ADR 0016 ; ADR 0003, 0011 et 0014 (annotations du même jour). Branche `claude/recette-stocks-successifs`, PR #188.

## Annotation du 11 novembre 2026 — point 5.1 : étapes `lissage` et `coherence_regularite`, alternative exclusive par propriété, paramètres conditionnels à la valeur d'un paramètre ; point 6.1 : table construite ; annotation du 08/11/2026, (e) (ADR 0017, M415 ; #223, #224, #225, #144)

Rédigée par `architect` à l'étape (0')(d) de la porteuse `claude/lissage-whittaker` (B19, PR #221), tête `fedd350`, après l'acceptation de l'ADR 0017 par le mainteneur le 11/11/2026 (M415 ; points A, B, C et I retenus). Le texte ci-dessus, annotations comprises, n'est pas réécrit ; noms indicatifs, le nom de l'objet fait foi (M77, (b)). Aucun code n'est écrit par la présente annotation : mise en œuvre aux commits (1a), (1b) et (4) de B19. L'annotation de ce point au titre du format des jeux simulés (#189, écart à l'annotation du 08/11/2026, (g), M414) est distincte et suivra la fusion de `[P4]`.

**(a) Point 5.1 : liste des étapes (ADR 0017, points 2 et 13).**
1. `lissage` entre dans `ETAPES` après `brut` et avant `positionnement` (module `lissage`, préalables `donnees`, `exposition`, `brut`, `aleatoire = FALSE`). « `lissage` ou `positionnement` » est tenu **par propriété** : `ETAPES` gagne une colonne `table_construite` (nom de l'élément du `resultat` qui porte la table construite, `NA` pour les autres étapes) et `valider_plan()` refuse un plan à plus d'une étape productrice (`tables_construites_multiples`, module `construire_table` ; D9 de M413). La règle vaut pour tout producteur futur.
2. `coherence_regularite` entre dans `ETAPES` après `homogeneite` et avant `validation` (module `validation`, préalables `donnees`, `exposition`, `aleatoire = FALSE`) ; elle exige une étape productrice au plan (`coherence_sans_table_construite`, M415).

**(b) Point 5.1, « tous requis » : condition sur la valeur d'un paramètre (ADR 0017, point 3.3 ; ADR 0016, annotation du même jour).** Après la lecture « tous requis sous la recette retenue » (annotation du 08/11/2026, (d) 2), la liste des paramètres d'une étape se lit : **tous requis quand leur condition est remplie, interdits sinon**, la condition portant sur la recette ou sur la valeur d'un paramètre gouvernant de la même étape. Exemples de B19 : `ponderation` (sous `methode = "wh_quadratique"`), `niveau` (sous `wh_poisson`), `parametre_lissage` (sous `critere_lissage = "declare"`), `intervalle_lissage` (sous `laml`) ; `table_reference` de l'étape `validation`, requis sous `independante`, `ajustee_hors_outil` et `positionnee`, interdit sous `lissee`. Aucun défaut implicite (ADR 0005, point 3).

**(c) Annotation du 08/11/2026, (e), dernière phrase.** « `valider_plan()`, exportée, ne connaît pas les rôles : elle ne peut lever ni `parametre_inattendu` ni l'absence d'un paramètre conditionnel » reste vrai pour la condition sur la **recette** ; pour la condition sur la **valeur d'un gouvernant**, qui ne dépend pas des rôles, `valider_plan()` lève `parametre_manquant` et `parametre_inattendu` (ADR 0017, point 3.2). Le point 5.4 se lit avec cette précision.

**(d) Point 6.1 : table construite (ADR 0017, point 1).** `table` reste la table finale, `NULL` tant que le plan ne va pas jusqu'à l'étape `table` : sans changement. La notion de **table construite** s'ajoute : table de taux annuels de décès de l'étape productrice (dans B19 : `etapes$lissage$resultat$table_lissee` ou `etapes$positionnement$resultat$table_positionnee`), `data.frame` nu, colonnes communes clés de segmentation, `age`, `qx`, `etat`, puis colonnes propres du producteur. Elle vit dans le `resultat` de l'étape productrice (ADR 0015, point 5.2), jamais dans `x$table` ; l'orchestration la sélectionne par la colonne `ETAPES$table_construite`, en contrôle le contrat et la passe en valeur aux lecteurs (validation `lissee`, `coherence_regularite`, puis la fermeture). Au sens du visa, un changement de table construite est soumis au mainteneur comme un changement de table finale (M308, M413). Ajouts compatibles au sens du point 6.3 (étapes nouvelles ; champ conditionnel `ddl_effectifs` de `etapes$validation$parametres` sous `lissee`, ADR 0017, point 5).

**Effet sur les résultats : aucun** par la présente annotation. Par B19, celui de l'ADR 0017, « Conséquences » (R1 à R14 identiques hors jeton du code, sauf R8 et R9 par #215 ; messages `etape_inconnue` et `etapes_hors_ordre` changés, visa d'`actuary` ; HTML des rapports).

Issues : #223, #224, #225, #144. Décisions M413, M414, M415. ADR 0017 ; ADR 0015, 0016 (annotations du même jour). Branche `claude/lissage-whittaker`, PR #221.

## Annotation du 12 novembre 2026 — contrat « format des jeux simulés » (annotations du 25/10/2026, (c), et du 08/11/2026, (b) et (g)) : défauts des formes agrégats et stocks successifs (`[P4]`, #189, #191 ; M412, M414, M419)

Rédigée par `architect` sur la porteuse `claude/lissage-whittaker` (B19, PR #221, tête `019bcfe`), après la fusion de `[P4]` (PR #226, `8ba2db4`), selon le corps de la PR #226 (« Décisions et termes proposés ») et l'étape (0')(d) du § 1.20 de la feuille de route. Le texte ci-dessus, annotations comprises, n'est pas réécrit ; le nom de l'objet fait foi (M77, (b)). Décrit le code fusionné, lu à `019bcfe` (`R/simulation.R`, *vérifié* par lecture).

**(a) Champ `unite_observation` de la forme agrégats (#189).** Sous les modes multi-contrats (`uniforme_multi`, `age_exact_multi`), la forme agrégats exige `unite_observation`, « tete » (fenêtres réunies par tête avant la projection) ou « contrat » ; interdit sous les modes simples ; « tete » refusée sous la transition de rachat (transition du contrat, M46) ; refus `forme_invalide`. Le champ est ajouté en fin de `simulation$forme`, sous les modes multi-contrats seulement (ajout compatible). Liste des formes admises sous mode multi-contrats : `CHAMPS_FORMES_MULTI` (stocks successifs toujours refusés, #190).

**(b) E^i sous `age_atteint_annee` : écart à l'annotation du 08/11/2026, (g).** (g) écrit « `exposition_initiale` (sans `annee` seulement) ». Sous la convention `age_atteint_annee` (admise par #189), `exposition_initiale` est produite **avec et sans** `annee` : la cellule (année civile, âge atteint dans l'année) finit au 1er janvier suivant, le complément de l'événement est défini. Sous `dernier_anniversaire`, (g) reste vrai. Écart accepté par `actuary` dans `[P4]` (M414, (3)).

**(c) Constante `DEFAUTS_FORMES`** (catalogue des défauts d'une forme, `CONTEXT.md`) : une entrée par forme (`lignes`, `agregats`, `stocks_successifs`), avec catalogue (unité, éligibilité, altération, motif et refus attendus écrits en dur), combinaisons, cible, unités ciblables, ordre des tirages et texte de validité ; l'entrée `lignes` reprend les constantes existantes telles quelles. Toute forme a son entrée ; l'attendu n'est jamais calculé par `donnees` (non-circularité, annotation du 25/10/2026).

**(d) Champ `retirees` de l'attribut `defauts` (#191).** Sous les stocks successifs, l'attribut `defauts` garde les sept champs de l'annotation du 25/10/2026, puis `retirees` en fin (ajout compatible) : une ligne par ligne retirée (rang dans le jeu propre de la forme, déclaration, défaut, unité, `id_contrat`, `type_ligne`, `date_inventaire`, motif et refus attendus) ; décompte `retirees` dans `decomptes`. « Inventaire perdu » : toutes les dates de la grille éligibles (décision du mainteneur du 11/11/2026, M414).

**(e) Paramètre `avec_defauts` de `controler_forme()` retiré.** Les défauts ne sont plus refusés par forme : toute forme a son catalogue (refus `forme_invalide` « forme avec l'argument defauts » levés ; M412, V1). Fonction interne, aucune signature exportée touchée.

**(f) Graines.** Plage de `[P4]` [22 001 200 ; 22 001 299] acceptée par le mainteneur (M412, (3)) ; valeurs employées 22 001 200 à 207, 230 à 235, 265 à 272 et 281 (*vérifié* par `grep`), dans `tests/unitaires/test_simulateur_logistique.R`, `test_simulateur_agregats.R` et `test_simulateur_stocks.R`. Déclaration dans `tests/controles/graines_admises.csv` par la session (fichier de `tests/`).

**Effet sur les résultats : aucun** par la présente annotation ; par `[P4]`, R1 à R14 identiques au bit près (CI à `05ed733`, corps de la PR #226).

Issues : #189, #191, #211 ; #190, #234, #235, #238. Décisions M412, M414, M419. Branche `claude/simulateur-additif`, PR #226 ; porteuse PR #221.

## Annotation du 13 novembre 2026 — point 7.1 : colonnes admises par `table_reference()` (`qx` ou `lx`), conversion des l_x en q_x, refus nouveaux, nom réservé (spécification de #146 ; ADR 0018, M447, M448)

Rédigée par `architect` à l'étape (0')(c) de la branche `claude/transcription-tables-homologuees` (B21, PR #262), tête `d3fcdaf`, après l'acceptation de l'ADR 0018 par le mainteneur le 13/11/2026 (M447). Le texte ci-dessus, annotations comprises, n'est pas réécrit ; le nom de l'objet fait foi (M77, (b)). Décrit la spécification amont d'`actuary-approfondi` (https://github.com/Gerard-Garey/outil_tables/issues/146#issuecomment-6848954642), à mettre en œuvre par #146 ; noms de motifs et messages fixés par `coder-approfondi`, visés par `actuary`. **La signature de `table_reference()` ne change pas** (`x, nom, statut, source, parametres = NULL, forme_origine = "qx"`).

**(a) Colonnes admises.** `age`, `sexe`, `annee` (facultative) et **exactement une** des colonnes `qx` ou `lx`. Une table en l_x se déclare par `forme_origine = "lx"` ; une colonne `qx` déclarée `forme_origine = "lx"` reste admise (reconstruction de l'objet par `controler_forme_tables()`). L'attribut `forme_origine` (`qx` ou `lx`) du point 7.1 cesse d'être une simple étiquette : sous `lx`, le constructeur convertit.

**(b) Conversion, dans le constructeur.** Par groupe (sexe, et année si présente), après le tri canonique et les contrôles de doublon et de contiguïté : d = l_x − l_{x+1}, puis q_x = d / l_x (opérations IEC 60559 ; formes `1 - l[i+1]/l[i]`, `diff()`, logarithme, normalisation par l_0 et toute tolérance interdites). Le dernier âge de chaque groupe ne donne pas de q_x (aucun q_ω = 1 posé) ; l'égalité l_{x+1} = l_x est admise (q_x = 0). L'objet ne garde pas les l_x ; l'empreinte de table porte sur les q_x dérivés. Statut **exact** pour des l_x entiers (arrondi correct du rapport).

**(c) Refus nouveaux ou changés** (valables pour tout statut) : `table_forme_incoherente` (`qx` et `lx` reçues ensemble, ou `lx` sous `forme_origine = "qx"`) ; `table_lx_non_fini`, `table_lx_negatif` (lignes citées avec groupe et âge) ; `table_lx_age_unique` ; **`table_lx_non_decroissants`**, comparaison exacte, qui cite des **paires d'âges consécutifs** — jusqu'à `NB_LIGNES_CITEES` (10), au-delà le total, liste complète dans `details` (décision Q2 du mainteneur, M448) ; `table_lx_nul` (l_x nul qui n'est pas le dernier âge du groupe). Messages de `table_colonne_manquante` et `table_colonne_inconnue` élargis à `lx` ; `table_qx_hors_bornes` cite désormais l'âge et le groupe de chaque ligne. La liste des refus du point 7.1 (« `qx` hors [0, 1], des âges non contigus ou en double, une colonne manquante ou mal typée ») s'en trouve complétée.

**(d) Nom réservé** (ADR 0018, point 7.4 ; question Q4 (c) de #146, option (a), M447). `table_reference()` refuse une table de statut `importee` ou `synthetique` dont le `nom` est, à l'identique (sans normalisation), celui d'une table déclarée au catalogue des tables embarquées (motif nouveau, nom indicatif `table_nom_reserve`). Mise en œuvre avec le catalogue (#263), le constructeur et la déclaration étant dans le même module `donnees`.

**Effet sur les résultats : aucun** par la présente annotation. Par #146 : R1 à R17 identiques au bit près attendus (aucune référence ne lit une table en l_x) ; `VERSION_FICHE` inchangée (6) ; changements de messages visés par `actuary`.

Issues : #146, #263, #145. Décisions M443, M447, M448. ADR 0018 ; ADR 0015 (annotation du même jour). Branche `claude/transcription-tables-homologuees`, PR #262.

## Annotation du 13 novembre 2026 (seconde) — point 7.1 : axe de génération (`generation`, exclusive d'`annee`), groupe (sexe, axe) ; annotation du 06/11/2026, (c), dernier alinéa : sens d'`annee` fixé, sans métadonnée ; annotation du 13/11/2026, (a) et (b) : colonnes et clôture au premier l_x nul ; **(c) rouverte** : `table_lx_nul` restreint (ADR 0019, M468 ; M465 ; #272)

Rédigée par `architect` à l'étape (0')(d) de la branche `claude/transcription-tables-generation` (B22, PR #271), tête `fa980e8`, après l'acceptation de l'ADR 0019 par le mainteneur le 13/11/2026 (M468). Le texte ci-dessus, annotations comprises, n'est pas réécrit ; noms de motifs indicatifs (fixés par `coder-approfondi`, messages visés par `actuary`), le nom de l'objet fait foi (M77, (b)). Aucun code n'est écrit par la présente annotation : mise en œuvre par #272. **La signature de `table_reference()` ne change pas** (`x, nom, statut, source, parametres = NULL, forme_origine = "qx"`). Mesures d'`architect` à `fa980e8` (R 4.3.3, hôte cloud Linux, hors plateforme de référence, `tablexp` installé depuis `git archive fa980e8` dans une bibliothèque temporaire hors dépôt), marquées *vérifié* ; `R/` et `tests/` sont identiques entre `4bca3da`, où l'ADR 0019 a mesuré, et `fa980e8` (*vérifié*, `git diff --stat`, vide).

**(a) Point 7.1 : deux axes temporels, exclusifs** (ADR 0019, points 1.1 à 1.3 ; D5 (a), M459). « à partir de la v2, `annee` (entier) pour les tables prospectives » se lit : `annee` est l'**année calendaire**, et elle seule. Une colonne facultative **`generation`** (entier, sans valeur manquante, nom sans accent) porte l'**année de naissance** des têtes auxquelles une **table de génération** s'applique. `table_reference()` admet `age`, `sexe`, exactement une de `qx` ou `lx`, et au plus un axe temporel, `annee` ou `generation` ; les deux reçues ensemble sont refusées (motif nouveau, indicatif `table_annee_et_generation`, module `donnees`, après `table_colonne_inconnue` et avant tout contrôle de valeurs). L'objet rendu porte `age`, `sexe`, `qx`, puis l'axe. Tri, doublons et contiguïté : par (sexe, axe), ordre canonique rang du sexe dans `SEXES_TABLE`, puis axe, puis âge. Aujourd'hui, une colonne `generation` est refusée par `table_colonne_inconnue`, module `donnees` (*vérifié*).

**(b) Annotation du 06/11/2026, (c), dernier alinéa** (« `annee` : son sens […] n'est pas fixé ; s'il doit être déclaré, ce sera une métadonnée »). Le sens est fixé **par le nom de la colonne** ; la condition « s'il doit être déclaré » ne se réalise pas, et la voie de la métadonnée est écartée (ADR 0019, point 1.6 et « Options écartées »). Aucun attribut, aucun argument, aucun champ de fiche n'est ajouté : `CHAMPS_TABLE_FICHE` et `VERSION_FICHE` (6) restent inchangés (*vérifié* à `fa980e8` : `R/construire_table.R:284`, `R/rapport.R:87`, non modifiés par l'ADR 0019).

**(c) Annotation du 13/11/2026, (a) : colonnes.** « `age`, `sexe`, `annee` (facultative) et exactement une des colonnes `qx` ou `lx` » devient : `age`, `sexe`, **au plus un axe temporel** (`annee` ou `generation`, point (a)) et exactement une des colonnes `qx` ou `lx`. Le reste de (a) est inchangé.

**(d) Annotation du 13/11/2026, (b) : groupe (sexe, axe) ; clôture au premier l_x nul** (ADR 0019, points 1.4 et 4.2 ; M465). « Par groupe (sexe, et année si présente) » se lit « par groupe (sexe, axe) », l'axe étant `annee` ou `generation` : aucun q_x n'enchaîne deux générations, et le premier âge à valeur de chaque génération ouvre son groupe. Règle nouvelle, pour tout statut et toute table en l_x, avec ou sans axe : dans un groupe, un l_x nul n'est suivi que de l_x nuls (une valeur non nulle après un zéro reste refusée par `table_lx_non_decroissants`) ; l'âge qui précède le premier l_x nul reçoit q_x = (l_x − 0) / l_x = 1, **calculé** par la formule commune et non posé ; les âges à l_x nul ne donnent ni q_x ni ligne ; −0 se comporte comme 0. « Le dernier âge de chaque groupe ne donne pas de q_x (aucun q_ω = 1 posé) » reste vrai pour un groupe que n'éteint aucun zéro imprimé, ce qui est le cas des quatre tables de B21 (aucun l_x nul, ADR 0019, (m3)). La formule et son statut **exact** pour des l_x entiers sont inchangés.

**(e) Annotation du 13/11/2026, (c) : rouverte pour `table_lx_nul`** (M465 ; ADR 0019, points 4.2, 4.3 et 9.1). Le refus « `table_lx_nul` (l_x nul qui n'est pas le dernier âge du groupe) » est **restreint au groupe dont le premier âge porte un l_x nul**, pour lequel aucun q_x n'existe. Motifs de la réouverture (ADR 0019, point 4.3) : une suite de l_x nuls en fin de groupe est une fonction de survie valide ; c'est une forme d'impression ordinaire de l'extinction (BO Santé 2007-1 pour TD 88-90 ; table unique 2024, *rapporté*) ; sans elle, la table unique 2024, transcrite telle qu'imprimée (E2.26), serait refusée pour sa forme d'impression. Mesure à `fa980e8` (*vérifié*), une table `H` d'âges consécutifs depuis 0, `forme_origine = "lx"` :

| l_x reçus | À `fa980e8` | Après #272 (*hypothèse*, critère de #272) |
|---|---|---|
| 5, 0 | admise, un q_x égal à 1 | inchangé |
| 5, 0, 0 | refus `table_lx_nul` | admise, objet `identical` à celui de 5, 0 |
| 5, 3, 0, 0, 0 | refus `table_lx_nul` | admise, q_x 0,4 puis 1 |
| 0, 0 | refus `table_lx_nul` | refus `table_lx_nul` (premier âge nul) |

Les autres refus de (c) sont inchangés. **Refus nouveaux** de l'ADR 0019 (indicatifs) : `table_annee_et_generation` (point (a)) ; `table_generation_non_admise`, dans le module de l'étape qui reçoit une table de génération (`positionnement`, `validation`, `coherence_regularite`), et `loi_invalide` du simulateur avec un message propre (ADR 0019, point 2.3) ; motifs propres du constructeur des décalages d'âge, module `donnees` (ADR 0019, point 6.1). **Messages changés** (ADR 0019, point 1.5) : `table_colonne_inconnue`, `table_colonne_mal_typee`, `table_lx_age_unique`, `table_lx_nul`, `table_ages_non_contigus` (sexe, axe et âges qui encadrent le trou, dans le message et les `details`), variantes « génération » des refus du constructeur ; tableau avant / après et visa d'`actuary` au commit.

**(f) Annotation du 13/11/2026, (d) : nom réservé.** La réserve s'étend aux trois noms déclarés par B22 au catalogue des tables (« TGF05 », « TGH05 », « Table unique 2024 » ; ADR 0019, point 8.1, M468, question 6). Les noms des décalages d'âge n'entrent pas dans la réserve des tables (ADR 0019, point 6.3).

**(g) Point 2 : graphe des appels inchangé.** Le prédicat d'axe temporel est une fonction interne de `donnees` (ADR 0019, point 2) ; ses appelants (`rapport`, `construire_table`, `positionnement`, `validation`, `simulation`) peuvent déjà appeler `donnees` (`tests/controles/graphe_modules.R`, relevé de l'ADR 0019 à `4bca3da`, *vérifié* par lui ; fichier inchangé à `fa980e8`). Après #272, aucun module autre que `donnees` ne teste par un littéral la présence d'une colonne d'axe sur une table de référence (critère relevé par `grep` au commit, vérifié par `audit`).

**Effet sur les résultats : aucun** par la présente annotation. Par #272 : R1 à R17 identiques au bit près attendus (*hypothèse*, à mesurer à chaque commit) ; aucune référence ne lit une table en l_x ni une table à axe ; `VERSION_FICHE` inchangée (6). Ce qui change sans être un résultat de référence : des tables en l_x refusées aujourd'hui (zéros terminaux) sont admises ; messages et `details` ci-dessus.

Issues : #272, #275. Décisions M235, M239, M448, M459 (D5), M465, M468. ADR 0019 (points 1, 2, 4, 6.1, 6.3, 8.1, 9.1, 9.3) ; ADR 0015 et 0011 (annotations du même jour). Branche `claude/transcription-tables-generation`, PR #271.
