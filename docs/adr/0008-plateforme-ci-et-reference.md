---
status: accepted
date: 2026-10-16
---

# R 4.3.3 en CI dans le conteneur `rocker/r-ver:4.3.3` épinglé par empreinte, BLAS et LAPACK de référence contrôlées par script ; la même image teste et produit les références

## Contexte

**Règles déjà posées.**

- `CLAUDE.md` (« Changements de résultats ») et E4.7 : les références de non-régression sont produites **sur la CI seule**, sous `ubuntu`, R 4.3.3, BLAS et LAPACK de référence monothread. La plateforme est consignée dans le message du commit de référence.
- ADR 0002 : la version vient de M43 (A13). La quatrième annotation laisse ouverte « la manière dont la CI installe exactement R 4.3.3 (action et version figée, BLAS et LAPACK de référence monothread) ». A8 renvoie à la plateforme de CI trois points ouverts : la chaîne de compilation, la version figée des packages compilés et la reproductibilité numérique du code compilé.
- ADR 0005, point 8 : un plan de référence par méthode de la v1, comparé valeur par valeur à tolérance.

**État du dépôt** (`main` à `8ec1077`, branche `claude/socle-donnees-exposition` à `039c6f7`).

- `.github/workflows/ci.yml` n'a qu'un job, « Contrôles du dépôt » (`ubuntu-24.04`, sans R).
- `.github/ruleset-main.json` n'exige que ce contrôle (`integration_id` 15368).
- La politique des Actions n'admet que les actions de GitHub : `github_owned_allowed`, `verified_allowed=false` (`README.md`, l. 57 et 67), actions épinglées par SHA.
- Issue #8 : demande le job « Tests (R 4.3.3) » et un script de plateforme. Ce script échoue hors de R 4.3.3 ou hors de la BLAS et de la LAPACK de référence.
- Issue #3, avec le commentaire de précisions du 16/10/2026 : demande le job « Absence de Java », sur un instantané daté des métadonnées CRAN (P6).

**Précédent d'`outil_USP`** (dépôt lu le 16/10/2026 : `.github/workflows/ci.yml`, `.github/workflows/references.yml`, `.github/actions/blas-reference/`, ADR 0011). R y est installé par `r-lib/actions/setup-r` (une action tierce) sur `runs-on: ubuntu-22.04`. Son ADR 0011 (point 2) constate qu'un libellé d'exécuteur **n'est pas une image figée** : GitHub la met à jour chaque semaine, BLAS et bibliothèques système comprises. Deux exécutions sur la même image et le même R différaient de 3,508e-07 sur 5 526 feuilles par cas (exécution 36055472513). Cause : R chargeait OpenBLAS, qui choisit ses noyaux selon le processeur de l'exécuteur. Le projet a dû ajouter une action composite. Elle impose `libblas3` et `liblapack3` par `update-alternatives`, pose `OPENBLAS_NUM_THREADS=1` et échoue si R ne charge pas ces bibliothèques ou si `La_version()` diffère de 3.10.0. Le même ADR 0011 avait écarté le conteneur épinglé « à ce stade ». Ce rejet tenait à deux motifs : une couche de plus à maintenir pour un gain non mesuré, une dérive entre images Linux de l'ordre du dernier bit. Trois différences font ici retenir ce qu'`outil_USP` avait écarté :

- la politique des Actions du présent dépôt exclut `setup-r` ;
- la dérive OpenBLAS mesurée par `outil_USP` montre qu'épingler un libellé ne fixe pas la plateforme de calcul ;
- aucune référence n'existe encore, donc choisir la plateforme maintenant ne coûte aucune bascule.

**Mesure de la session** (16/10/2026, conteneur de la session cloud) :

```
Ubuntu 24.04.5 LTS ; R version 4.3.3 (2024-02-29)
extSoftVersion()["BLAS"] : /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.12.0
La_library()             : /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.12.0
La_version()             : 3.12.0 ; libblas3, liblapack3 3.12.0-3build1.1
```

`outil_USP` a mesuré la même famille Netlib sous Ubuntu 22.04, avec LAPACK 3.10.0. Ainsi, « R 4.3.3 avec BLAS et LAPACK de référence » ne désigne pas une plateforme unique : la version de LAPACK suit la distribution. L'identifiant de plateforme doit nommer les versions, pas seulement la famille.

**Ce qui rend la décision nécessaire maintenant.** Le premier job de tests et le script de plateforme sont dans le périmètre de #8. Les premières références de non-régression viennent à la deuxième branche (`claude/taux-bruts-non-regression`, feuille de route § 2). Elles doivent naître sur la plateforme qui les testera.

## Décision

Arrêtée par le mainteneur le 16/10/2026 (M45, point P3) : R 4.3.3 en CI par le **conteneur `rocker/r-ver:4.3.3` épinglé par empreinte**. La BLAS et la LAPACK sont contrôlées par un script, qui échoue si R n'est pas 4.3.3 ou si la BLAS ou la LAPACK n'est pas celle de référence. **Repli** : compilation de R 4.3.3 depuis la source CRAN, avec BLAS et LAPACK internes. Les points 1, 3 et 10 reprennent cette décision, le point 4 reprend `CLAUDE.md` (monothread). Les autres en sont la mise en œuvre, rédigée par `architect` et signalée comme nouvelle dans le bloc « Retour » de la consultation.

### Plateforme

1. **Conteneur de job.** Tout job qui exécute R déclare `container:` au niveau du job, sur un exécuteur Linux hébergé (`runs-on: ubuntu-24.04`). Cette clé est une fonctionnalité native de GitHub Actions, pas une action. La politique `github_owned_allowed` n'est donc pas touchée.
   - **Image** : `rocker/r-ver:4.3.3@sha256:<empreinte>`. L'empreinte retenue est celle de l'index multi-architecture que désigne l'étiquette au jour du relevé. Elle est immuable, et le contenu de l'index fixe celle du manifeste `linux/amd64`.
   - **Shell** : dans un conteneur, les étapes `run` passent par `sh` par défaut. Chaque job conteneurisé déclare `defaults: run: shell: bash`.
   - **Chaîne unique** : la même chaîne `image@empreinte` figure dans tous les jobs R de `ci.yml` et dans le workflow de régénération (point 8). Un contrôle mécanique échoue si elles divergent (`CLAUDE.md`, « Workflows », principe 7). Il peut rejoindre le script d'identité des batteries de #8 (point 4).
2. **Relevé de l'empreinte.** Le réseau de la session de rédaction refuse Docker Hub (mesure ci-dessous). L'empreinte est donc relevée par `coder` dans #8, depuis un poste ou une session qui atteint le registre, par l'une des deux commandes suivantes :

   ```
   docker buildx imagetools inspect rocker/r-ver:4.3.3      # ligne « Digest: sha256:… » (index)
   ```

   ```
   T=$(curl -s "https://auth.docker.io/token?service=registry.docker.io&scope=repository:rocker/r-ver:pull" | python3 -c 'import sys,json;print(json.load(sys.stdin)["token"])')
   curl -sI -H "Authorization: Bearer $T" \
     -H "Accept: application/vnd.oci.image.index.v1+json" \
     -H "Accept: application/vnd.docker.distribution.manifest.list.v2+json" \
     https://registry-1.docker.io/v2/rocker/r-ver/manifests/4.3.3 | grep -i docker-content-digest
   ```

   Commande, date et sortie sont citées dans le commit `repo:` qui introduit le job. Elles sont reportées, avec la première sortie du script de plateforme (point 3), dans une **annotation datée du présent ADR**. Le corps de l'ADR n'est pas réécrit.
3. **Script de plateforme.** `coder` en choisit le nom et l'emplacement dans #8. C'est un script R de base, exécuté en première étape de chaque job R, avant tout calcul. Il a deux modes :
   - **mode strict** (`--exiger`), sur la CI et dans le workflow de régénération : il **échoue** (code non nul, ligne `::error`) si l'une des conditions ci-dessous n'est pas remplie ;
   - **mode relevé**, sur le poste ou en session : il affiche les mêmes grandeurs sans échouer. Sans ce mode, les batteries ne pourraient pas passer sur le poste Windows (#8, premier critère : « en local et en CI »), dont la BLAS est celle de R.

   Conditions contrôlées :
   - `R.version.string` vaut exactement `R version 4.3.3 (2024-02-29)` ;
   - `extSoftVersion()["BLAS"]`, `sessionInfo()$BLAS`, `sessionInfo()$LAPACK` et `La_library()` ne sont pas vides, ne désignent aucune BLAS optimisée (motif `openblas|mkl|atlas|blis|flexiblas`) et désignent les chemins de référence attendus ;
   - `La_version()` vaut la version attendue ;
   - `OPENBLAS_NUM_THREADS` et `OMP_NUM_THREADS` valent `1` (point 4).

   **Valeurs attendues.** Elles sont écrites en constantes dans le script. Hypothèse de départ, non vérifiée (voir « Conséquences ») : R est compilé par rocker avec `--with-blas --with-lapack` et charge la BLAS et la LAPACK Netlib du système (`/usr/lib/x86_64-linux-gnu/blas/libblas.so.3*`, `/usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3*`), dans la version de la distribution de base de l'image. La première exécution de la CI tranche :
   - si l'image est conforme, les constantes sont confirmées et l'exécution citée dans l'annotation du point 2 ;
   - si le script échoue, il **dit ce qu'il voit**, et c'est un point de décision, jamais un ajustement silencieux.
     - L'image charge une Netlib d'un autre chemin ou d'une autre version : `coder` corrige les constantes et la PR cite l'exécution. Aucune référence n'existe encore, aucun résultat ne bouge.
     - L'image charge une BLAS optimisée : le mainteneur choisit entre l'imposition de `libblas3` et `liblapack3` dans le job (forme d'`outil_USP`, au prix d'un `apt-get` à chaque exécution) et le repli (point 10).
4. **Monothread.** `OPENBLAS_NUM_THREADS=1` et `OMP_NUM_THREADS=1` sont posés dans l'`env` de chaque job R. La BLAS Netlib est monothread par construction. Ces variables sont des garde-fous contre une bibliothèque multithread chargée à l'insu du script. Le script de plateforme les contrôle.
5. **Identifiant de plateforme.** Le script de plateforme l'écrit en un bloc, dans le journal et dans le résumé du job (`$GITHUB_STEP_SUMMARY`) :
   - `image@empreinte`, lue dans une variable d'environnement que le job renseigne avec la même chaîne que `container.image` ;
   - `R.version.string` ;
   - BLAS (`extSoftVersion()["BLAS"]`) ;
   - LAPACK (`La_library()`, `La_version()`) ;
   - `sessionInfo()`.

   Le workflow de régénération y ajoute, dans son artefact, le commit calculé et le lien de l'exécution. Les références n'en deviennent pas plus portables, mais tout écart futur a un suspect nommé. Il **reporte ce bloc dans le message de chaque commit de référence**, comme le demande `CLAUDE.md`. Sont **consignés sans faire échouer** :
   - le modèle et les drapeaux du processeur (`fma`, `avx2`, `avx512f`) ;
   - `ImageOS` et `ImageVersion` de l'hôte ;
   - `Sys.getlocale("LC_COLLATE")` et `Sys.timezone()` ;
   - la présence d'un exécutable `java` dans l'image (`Sys.which("java")`).

### Jobs requis

6. **Quatre contrôles requis** par le ruleset de `main`, désignés par leur nom de job exact. Aucun job n'installe de package CRAN : ni `install.packages`, ni dépôt CRAN configuré pour l'installation (#8). La question d'un package admis dans le moteur reste à l'ADR de sa méthode (ADR 0002, A3).

   | Job | Exécution | Contenu |
   |---|---|---|
   | « Contrôles du dépôt » | hôte, sans R (existant) | contrôles qui n'exigent pas R, dont ceux qui lisent l'historique git (point 11) |
   | « Tests (R 4.3.3) » | conteneur (point 1) | `actions/checkout` (`persist-credentials: false`) ; script de plateforme en mode strict ; les trois batteries de `CLAUDE.md` (« Commandes ») ; liste blanche de `DESCRIPTION` ; ordre des modules |
   | « Absence de Java » | conteneur (point 1), réseau | #3 (point 7) |
   | « Documentation » | conteneur TeX épinglé par empreinte, ou paquets TeX de l'hôte par `apt-get` | compilation du PDF de `docs/latex/` sans action tierce (l'`xu-cheng/latex-action` d'`outil_USP` est exclue par la politique des Actions) ; PDF publié par `actions/upload-artifact` (action de GitHub) et versionné par la session principale (P7) ; choix de l'image ou des paquets dans #8, point 7 |

   `.github/ruleset-main.json` liste les quatre `context`, avec le même `integration_id`. Le mainteneur applique le ruleset (`README.md`, « Mise à jour du ruleset »). Un job absent du ruleset n'est pas bloquant, même en échec (`README.md`, l. 74).
7. **« Absence de Java » (#3).**
   - **Exécution** : le job s'exécute dans la même image que les tests et lit les métadonnées d'un **instantané daté** du CRAN chez Posit Package Manager. L'adresse a la forme `https://packagemanager.posit.co/cran/<AAAA-MM-JJ>` ; la date est choisie un jour ouvré, puisque Posit ne publie pas d'instantané les week-ends et jours fériés.
   - **Contrôle** : le script calcule les dépendances par `available.packages(repos = <instantané>)`, puis `tools::package_dependencies(..., recursive = TRUE, which = c("Depends", "Imports", "LinkingTo"))`, sans rien installer.
   - **Consignation** : la date de l'instantané est écrite dans le script et ne change que par un commit `repo:`.
   - **Panne réseau** : elle fait échouer le job sans qu'aucun résultat ait changé. Une relance la lève, l'échec n'est jamais masqué.

   Ce job n'entre pas dans la plateforme de référence : il ne produit aucun résultat. Il s'exécute dans l'image épinglée par commodité, R y étant présent.

### Références, changement de plateforme, repli

8. **Workflow de régénération** (deuxième branche, prévu ici dans ses invariants seulement) :
   - déclenchement manuel seul (`workflow_dispatch`), sur ordre du mainteneur, par la session principale ;
   - `permissions: contents: read`, aucun secret ;
   - **même `image@empreinte`** et même script de plateforme en mode strict que « Tests (R 4.3.3) » : les références sont testées sur la plateforme qui les produit ;
   - calcul sur un checkout jetable ; l'artefact publié contient les références, leurs MD5 et l'identifiant de plateforme ;
   - commit des références par la session principale après visa, avec l'identifiant de plateforme dans le message (`CLAUDE.md`).

   Deux contraintes viennent d'`outil_USP` (ADR 0011, points 8 et 9). GitHub ne déclenche un `workflow_dispatch` que si le fichier du workflow existe sur `main`. Il y entre par une PR courte avant le premier déclenchement : c'est un correctif rapide (résultats identiques, domaine `repo:` seul, aucune documentation de fond). Le téléchargement d'un artefact depuis une session cloud passe par `*.blob.core.windows.net`, à autoriser dans l'environnement de la session. Les modes du workflow (création d'une référence, régénération ordinaire, bascule de plateforme), la tolérance et le format des références sont réglés à la deuxième branche.
9. **Tout changement de plateforme est une décision du mainteneur.** Changer d'empreinte, d'image, de BLAS ou de LAPACK, ou passer au repli, se consigne par annotation datée du présent ADR. Une fois des références existantes, le changement s'accompagne d'une **bascule** : tableau avant / après de toutes les références recalculées sur la nouvelle plateforme contre les anciennes, et visa. Ni Dependabot ni personne ne met l'empreinte à jour en silence ; une PR qui la modifie est traitée comme un changement de plateforme.
10. **Repli : R 4.3.3 compilé depuis la source CRAN.** Il entre en jeu dans trois cas :
    - l'empreinte n'est plus servie par le registre ;
    - l'image n'est pas conforme et le mainteneur écarte la correction dans le job ;
    - le mainteneur en décide.

    Mise en œuvre :
    - archive `R-4.3.3.tar.gz` du CRAN, empreinte SHA-256 consignée ;
    - compilation **dans une image de base épinglée par empreinte** (même règle qu'au point 1 ; le compilateur est ainsi figé, ce que l'hôte ne garantit pas), `configure` sans `--with-blas` ni `--with-lapack` : BLAS et LAPACK internes de R (`libRblas`, `libRlapack`) ;
    - R compilé mis en cache par `actions/cache` (action de GitHub), clé : empreinte de l'image de base, version de R, options de `configure`.

    Le script de plateforme reçoit alors d'autres valeurs attendues : les chemins de `libRblas` et `libRlapack`, et la version de LAPACK de R 4.3.3, à relever. Passer au repli est un changement de plateforme (point 9).
11. **Git dans les jobs conteneurisés.** Quand git manque à l'image, `actions/checkout` télécharge l'arbre par l'API REST et le répertoire `.git` est absent. Règles :
    - les batteries et les scripts des jobs R ne dépendent pas de git ;
    - l'empreinte du code se calcule sur les fichiers (`tools::md5sum`, P8), pas sur un SHA git ;
    - le commit calculé est lu dans `GITHUB_SHA` ;
    - un contrôle qui lit l'historique reste dans « Contrôles du dépôt », sur l'hôte.
12. **Code compilé** (ADR 0002, A8, points ouverts). L'image figée par son empreinte fixe aussi le compilateur et les options de compilation de R (`R CMD config`), à condition que l'image contienne un compilateur ; hypothèse : c'est le cas de rocker. `tablexp` serait alors compilable dans le conteneur, sous un compilateur constant d'une exécution à l'autre. La version figée de `Rcpp` et la tolérance propre au code compilé restent à l'ADR de méthode qui introduit du code compilé. Le présent ADR fixe seulement que ce code se compile **dans l'image de référence**, jamais sur l'hôte.

## Options écartées

- **`r-lib/actions/setup-r`** (forme d'`outil_USP`). C'est une action tierce : l'admettre élargirait la politique des Actions (`README.md`, l. 57 et 67) à des actions sélectionnées, pour un dépôt public. De plus, elle installe R sur l'image de l'exécuteur, dont la BLAS dépend : `outil_USP` a dû imposer la BLAS Netlib par une action supplémentaire après avoir mesuré une dérive OpenBLAS de 3,508e-07 entre deux exécutions (Contexte).
- **Binaire `.deb` de R 4.3.3 de Posit installé par `apt-get` sur l'hôte.** Aucune action tierce, mais le paquet se lie aux bibliothèques de l'image de l'exécuteur : BLAS et LAPACK, `libm`, bibliothèques système. GitHub met cette image à jour chaque semaine sans commit dans le dépôt (`outil_USP`, ADR 0011, point 2). On pourrait imposer la BLAS par `update-alternatives`, mais le reste de la plateforme bougerait sans trace.
- **Compilation de R comme voie principale.** Elle demande un temps de compilation à chaque cache invalidé (non mesuré) et une image de base à épingler de toute façon. Elle est gardée comme repli (point 10), sa seule valeur propre étant de ne dépendre d'aucune image tierce de R.
- **`rocker/r-ver:4.3.3` sans empreinte.** L'étiquette est mobile : rocker peut reconstruire l'image sous la même étiquette (hypothèse, non vérifiée) et la plateforme changerait sans commit. C'est exactement le défaut du libellé d'exécuteur.
- **Image construite et publiée par le projet** (par exemple sur GHCR). Il faudrait un jeton `packages: write`, contraire au jeton en lecture seule des workflows (`README.md`, l. 68), et un Dockerfile à maintenir. Option à rouvrir si l'empreinte rocker disparaît et que le repli s'avère trop lent.
- **Métadonnées CRAN versionnées dans le dépôt** pour « Absence de Java » (fichier `PACKAGES` copié, contrôle hors réseau). Ce fichier fait plusieurs mégaoctets et se met à jour à la main ; P6 a retenu un instantané daté consulté en ligne. Option à rouvrir si les échecs réseau de ce job se répètent.
- **Exécuteur Windows ou matrice de plateformes en CI.** La plateforme de référence est unique (`CLAUDE.md`) ; le poste Windows exécute les batteries en local, et la tolérance des comparaisons absorbe la dérive entre plateformes, sans la masquer.

## Conséquences

- **Fichiers** (issue #8, puis #3) :
  - `.github/workflows/ci.yml` : jobs du point 6 ;
  - `.github/ruleset-main.json` : quatre `context` ;
  - script de plateforme ;
  - script et liste des packages de « Absence de Java » ;
  - contrôle d'identité de la chaîne `image@empreinte`, avec celui des batteries ;
  - `README.md` : « Sécurité du dépôt », liste des contrôles requis, ligne 58.

  `CLAUDE.md` (« Changements de résultats ») est cohérent tel quel ; il peut citer le présent ADR (alignement `claude:` par la session principale). `docs/exigences.md` E4.7 est inchangé.
- **Effet sur les résultats** : aucun. Aucun code, aucune référence n'existe ; les premières références naîtront sur cette plateforme (deuxième branche).
- **Risques acceptés, non mesurés** :
  - temps de téléchargement de l'image à chaque job ;
  - disparition possible d'une empreinte qui n'est plus désignée par aucune étiquette, selon la politique de rétention du registre (non vérifiée) ; le repli (point 10) y répond.

  Les limites de débit de Docker Hub ne s'appliquent pas aux exécuteurs hébergés par GitHub, selon la documentation de GitHub (extrait lu par moteur de recherche, page non ouverte).
- **Packages `recommended` dans l'image.** Hypothèse : rocker compile R avec `--with-recommended-packages`, donc `survival`, `mgcv` et `Matrix` sont présents dans le conteneur, comme sur le poste. La liste blanche de `DESCRIPTION` (ADR 0002, point 3) ne protège pas les tests : un test qui chargerait `survival` passerait en CI. **Proposition pour #8**, à trancher par la session principale avec `coder` : le harnais des batteries échoue si un espace de noms hors priorité `base` est chargé en fin de batterie (`loadedNamespaces()` comparé à `installed.packages(priority = "base")`). La règle est mécanique et bon marché, et protège ADR 0002, points 3 et 4.
- **Identité au bit près entre exécuteurs non garantie.** Hypothèse : la `libm` de la glibc choisit à l'exécution des variantes de `exp`, `log` et `pow` selon le processeur (FMA, AVX2). Deux exécuteurs de processeurs différents pourraient alors différer au dernier bit, même dans la même image. E4.6 exige l'identité « sur une même machine » ; la non-régression compare à tolérance (ADR 0005, point 8). C'est pourquoi le processeur est consigné (point 5).
- **Paramètres régionaux et fuseau horaire.** Hypothèse : rocker fixe `LANG=en_US.UTF-8` et `TZ=Etc/UTC` ; le poste est un Windows en français (A13). Un tri de chaînes ou un calcul en `POSIXct` du moteur pourrait dépendre de l'un ou de l'autre. Le point est **signalé à l'ADR 0007** (forme du package et contrats) et à `audit` : tri par `method = "radix"` ou en paramètres régionaux `C`, dates en `Date`. Le présent ADR se borne à consigner `LC_COLLATE` et le fuseau horaire (point 5).
- **Hypothèses sur l'image, non vérifiées** (réseau de la session fermé à `raw.githubusercontent.com` et à Docker Hub ; script `install_R_source.sh` de `rocker-org/rocker-versioned2` non lu) :
  - options de `configure` (`--with-blas --with-lapack --with-recommended-packages`) ;
  - présence de `g++`, `gfortran` et `make` ;
  - absence de git et de Java dans l'image finale ;
  - distribution de base (Ubuntu 22.04, donc LAPACK 3.10.0 si la Netlib est chargée) ;
  - `LANG` et `TZ`.

  Chacune est tranchée par la première exécution du script de plateforme (points 3 et 5) et consignée dans l'annotation du point 2.
- **Ce que l'ADR ne règle pas** :
  - la tolérance des comparaisons et le format des références (deuxième branche, `actuary` pour la tolérance) ;
  - les modes du workflow de régénération (deuxième branche) ;
  - l'image ou les paquets TeX du job « Documentation » (#8, point 7) ;
  - la version figée de `Rcpp` ou d'un package admis dans le moteur (ADR de méthode, ADR 0002, A3 et A8) ;
  - d'éventuels filtres de chemins sur les jobs ; un job requis sauté doit alors l'être à coup sûr à raison, `outil_USP` faisant tout exécuter en cas de doute ;
  - la reproductibilité sous `parallel` (bootstrap, Q31).
- **Vocabulaire** : deux entrées à ajouter à `CONTEXT.md` par `architect`, dans un commit distinct : « Plateforme de référence » (image `@empreinte`, R, BLAS et LAPACK nommées par version) et « Identifiant de plateforme » (bloc du point 5).

**Sources consultées.**

- **Dépôt `outil_USP`** (lecture locale, 16/10/2026) : ADR 0011 ; `.github/actions/blas-reference/action.yml` et `controle_blas.R` ; `.github/workflows/ci.yml` et `references.yml`.
- **Documentation de GitHub, « Run jobs in a container »** (`https://docs.github.com/en/actions/how-tos/write-workflows/choose-where-workflows-run/run-jobs-in-a-container`), extraits par moteur de recherche, page non ouverte : clé `container:` du job, shell `sh` par défaut, Docker Hub sans limite de débit pour les exécuteurs hébergés. Le format `image@sha256:…` n'y figure pas dans les extraits lus. Il est admis par Docker, à confirmer par la première exécution.
- **`actions/checkout`, README**, copies secondaires lues par moteur de recherche : repli sur l'API REST sans git 2.18 ou plus dans le `PATH` du conteneur.
- **Posit Package Manager** (`https://docs.posit.co/rspm/user/get-repo-url.html`), extrait par moteur de recherche : date d'instantané au format AAAA-MM-JJ, aucun instantané les week-ends et jours fériés. Les sources se contredisent sur la date servie un jour sans instantané (antérieure ou suivante), d'où le jour ouvré du point 7.
- **Accès réseau tenté depuis la session**, refusé par le proxy (`CONNECT tunnel failed, response 403`) : `auth.docker.io`, `registry-1.docker.io`, `hub.docker.com`, `ghcr.io`, `raw.githubusercontent.com`, `packagemanager.posit.co` ; `docker manifest inspect rocker/r-ver:4.3.3` donne `Forbidden`.

Issues : #8 (socle, batteries et CI sous R 4.3.3), #3 (absence de Java). Décisions M43 (R 4.3.3), M44 (`Depends: R (>= 4.3.3)`), M45 (P3, P6, P7, P8 ; `docs/feuille-de-route.md`, § 1 et § 4). Voir ADR 0002 (quatrième annotation, A13 ; seconde annotation, A8, points ouverts) et ADR 0005 (point 8).

## Annotation du 16 octobre 2026 — empreinte relevée, OpenBLAS constaté, bascule vers la BLAS et la LAPACK Netlib (décision M47) ; identifiant de plateforme de référence fixé

Annotation prévue par les points 2, 3 et 9 et par « Conséquences » (hypothèses sur l'image) ; rédigée par `architect` à partir des commits et des exécutions cités. Le texte ci-dessus n'est pas réécrit.

**Relevé de l'empreinte (point 2).** Exécution GitHub Actions 38903692466 (commit `df21528`), job temporaire « Relevé de l'empreinte de l'image », `docker buildx imagetools inspect rocker/r-ver:4.3.3` sur l'exécuteur, le 16/10/2026 : empreinte de l'**index multi-architecture**

```
rocker/r-ver:4.3.3@sha256:732d15020af326da9e919c07f70ca32bf5d3e409220af32e0a4b6d0a89437309
```

reportée dans `container.image` et `TABLEXP_IMAGE` du job « Tests (R 4.3.3) » (`.github/workflows/ci.yml`) par le commit `1d49e17`, qui retire aussi le job temporaire.

**Constat (point 3, cas « BLAS optimisée »).** La même exécution montre que l'image charge **OpenBLAS 0.3.20** (chemins `/usr/lib/x86_64-linux-gnu/openblas-pthread/...`), sur **Ubuntu 22.04.5**, `La_version()` = `3.10.0`. L'hypothèse de départ (Netlib chargée par défaut) est donc fausse ; la distribution de base (Ubuntu 22.04, LAPACK 3.10.0) est confirmée. Le script de plateforme a échoué en mode strict en disant ce qu'il voyait : aucune constante n'a été ajustée en silence.

**Décision du mainteneur M47 (16/10/2026)** : des deux voies du point 3, la **bascule dans le job** est retenue, comme dans `outil_USP`, et non le repli (point 10). Une étape « BLAS et LAPACK de référence », placée avant le script de plateforme, pointe par `update-alternatives` les alternatives `libblas.so.3-x86_64-linux-gnu` et `liblapack.so.3-x86_64-linux-gnu` vers les paquets Netlib d'Ubuntu, `libblas3` et `liblapack3` en version **3.10.0-2ubuntu1**. Ces paquets sont déjà présents dans l'image ; l'étape les passe à `apt-get install`, qui ne fait que confirmer leur présence, puis affiche les cibles résolues et les versions (`dpkg-query`). Le coût d'un `apt-get update` à chaque exécution, prévu au point 3, est accepté. La chaîne `image@empreinte` ne change pas : la plateforme de référence est **l'image épinglée suivie de cette bascule**. Une modification de l'étape est un changement de plateforme (point 9).

**Première exécution conforme.** Exécution 38904463401 (commit `23ac813`), job « Tests (R 4.3.3) » vert, script de plateforme en mode strict :

- R `R version 4.3.3 (2024-02-29)` ;
- BLAS `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0` ;
- LAPACK `/usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0`, `La_version()` `3.10.0` ;
- `OPENBLAS_NUM_THREADS` et `OMP_NUM_THREADS` à `1`.

Les constantes de `tests/controles/plateforme.R` (`R_ATTENDU`, `LA_VERSION_ATTENDUE`, `MOTIF_BLAS`, `MOTIF_LAPACK`) sont ainsi confirmées sans modification.

**Grandeurs consignées sans faire échouer (point 5).**

- **Processeur** : AMD EPYC 9V45 dans l'exécution 38904463401, AMD EPYC 7763 dans l'exécution 38903692466. Le modèle varie donc d'une exécution à l'autre sur le même libellé d'exécuteur. La BLAS Netlib n'a pas de noyau choisi selon le processeur, contrairement à OpenBLAS ; la réserve de « Conséquences » sur la `libm` de la glibc, qui pourrait différer au dernier bit d'un processeur à l'autre, reste en revanche **ouverte, non mesurée**.
- **`ImageOS` et `ImageVersion`** de l'hôte ne sont **pas transmises au conteneur** : le bloc les écrit vides. Pour les consigner, le job devrait les transmettre explicitement au conteneur (moyen à établir et à mesurer). C'est un point mineur, sans effet sur les résultats, laissé à la deuxième branche (workflow de régénération).
- **Java** : aucun exécutable `java` dans l'image (`Sys.which("java")` vide), ce qui confirme l'hypothèse correspondante de « Conséquences ».

Hypothèses de « Conséquences » non consignées par la présente annotation, faute de relevé rapporté : options exactes de `configure` de rocker, présence de `g++`, `gfortran` et `make`, `LANG` et `TZ`. Elles restent ouvertes. Le bloc du point 5 consigne déjà `LC_COLLATE` et le fuseau horaire ; les autres seront relevées quand elles serviront (code compilé, tri de chaînes).

**Identifiant de plateforme de référence** (point 5), fixé à cette date :

- image `rocker/r-ver:4.3.3@sha256:732d15020af326da9e919c07f70ca32bf5d3e409220af32e0a4b6d0a89437309`, suivie de la bascule `update-alternatives` vers `libblas3` et `liblapack3` 3.10.0-2ubuntu1 ;
- `R version 4.3.3 (2024-02-29)` ;
- BLAS `/usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0` ;
- LAPACK `/usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0`, version 3.10.0 ;
- monothread.

Les premières références de non-régression naîtront sur cette plateforme (deuxième branche) ; tout changement d'un de ces éléments relève du point 9.

**Effet sur les résultats** : aucun (aucune référence n'existe).

**Preuves.**

- *Vérifié par `architect`* : le SHA de tête de chaque exécution (`gh api repos/Gerard-Garey/outil_tables/actions/runs/<id>` : 38903692466 → `df21528`, conclusion `failure` ; 38904463401 → `23ac813`, conclusion `success`) ; le contenu du commit `1d49e17` (`git show`) ; les constantes de `tests/controles/plateforme.R`.
- *Rapporté par la session principale, non revérifié* : les valeurs lues dans les journaux des jobs (OpenBLAS 0.3.20, Ubuntu 22.04.5, chemins, processeurs, version des paquets, absence de `java`, `ImageOS` vide). Le téléchargement des journaux depuis la session de rédaction est refusé (`results-receiver.actions.githubusercontent.com`, `Forbidden`).

Issues : #8. Décision M47 (`docs/feuille-de-route.md` § 4).
