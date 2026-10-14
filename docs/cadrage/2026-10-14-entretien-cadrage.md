# Entretien de cadrage du 14 octobre 2026

Compte rendu de la phase de cadrage (`/grill-with-docs`, session principale sur Fable 5.1, effort xhigh, sur demande du mainteneur), menée en quatre tours de questions numérotées, chacune avec une recommandation ; **chaque décision ci-dessous est celle du mainteneur**, prise sur la recommandation de la session (« recommandation suivie ») ou contre elle (signalé). Les faits qui ont alimenté l'entretien viennent de `docs/recherche/2026-10-14-cadre-tables-experience.md` (agent de recherche, statuts vérifié / incertain / non trouvé) et de la lecture du dépôt `Gerard-Garey/outil_USP` (cahier des charges, glossaire, `CLAUDE.md`, fiches `actuary` et `regulatory`, conventions LaTeX).

Les numéros Q1 à Q40 et A1 à A5 sont cités tels quels par les ADR, la feuille de route (décisions M) et le cahier des charges. Le vocabulaire est celui de `CONTEXT.md`.

## Énoncé initial du mainteneur

Créer un outil versatile, avec interface graphique, en R ou Python, qui construit une **table d'expérience** (cas principal : table de mortalité) à partir d'une base de données au format versatile (variables plus ou moins détaillées). Domaine : l'actuariat ; l'outil doit couvrir la majorité des méthodes actuarielles, y compris les plus puissantes si leur coût de calcul n'est pas excessif. Aucune donnée réelle ne sera jamais disponible : des données fictives sont à produire. Les résultats serviront à soutenir des dossiers devant l'ACPR : rigueur maximale. S'appuyer sur `outil_USP` pour le niveau d'exigence.

## Tour 1 — racine

| N° | Question | Décision |
|---|---|---|
| Q1 | Finalité du dossier ACPR | Cibles par priorité : **(b)** hypothèses de best estimate Solvabilité II, puis **(a)** certification de table d'expérience, puis **(c)** modèle interne. Les tables de rachat, d'entrée / sortie d'incapacité-invalidité et de dépendance relèvent des mêmes dossiers. |
| Q2 | Risques couverts et ordre | Tous, dans l'ordre : mortalité / longévité → rachat (comportementales) → prévoyance (incapacité, invalidité) → dépendance → chômage des emprunteurs. |
| Q3 | Nature des tables de mortalité | Tables **du moment et prospectives** ; inclure les modèles d'amélioration de la mortalité américains (échelles MP de la SOA) et britanniques (CMI) en plus de la famille Lee-Carter. Granularité d'âge paramétrable et adaptée aux données (date de naissance souvent absente). |
| Q4 | Langage | **R** (préférence du mainteneur ; Python jugé meilleur en vitesse et en interface mais plus long à développer). Contrainte : **s'appuyer peu sur les packages** (règle en Q14). |
| Q5 | Utilisateurs et interface | Le mainteneur lance l'outil ; application web locale lancée depuis R, comme `outil_usp` : **Shiny**. |
| Q6 | Forme des données d'entrée | Les trois formes (lignes individuelles, agrégats, stocks successifs) **et au-delà** : la versatilité est un critère majeur (principe unificateur en Q17). |
| Q7 | Volumétrie | Des **millions de lignes** et des **dizaines d'années** d'observation ; étudier aussi ce qui reste faisable, statistiquement et actuariellement, avec des bases modestes (Q21). |
| Q8 | Dispositifs repris d'`outil_USP` | Repris : (1) frontière stricte calcul / affichage, moteur utilisable sans interface, contrôles métier aussi dans le moteur ; (3) références de non-régression produites par la CI, comparaison valeur par valeur à tolérance, tableau avant / après et visa ; (4) graine locale et générateur fixé ; (5) rapport figé HTML autonome avec empreintes ; (6) documentation méthodologique **LaTeX** avec test de concordance doc ↔ code ; (7) quatre statuts de rigueur. **Écarté : (2) moteur dans un fichier unique** → package modulaire (Q23, ADR). |
| Q9 | Experts de fond | Deux, comme dans `outil_USP` : `actuary` (méthodes, statistique) et `regulatory` (textes). |
| Q10 | Données fictives | Le simulateur est une composante du moteur ; il reproduit aussi les défauts des données réelles. Les jeux simulés versionnés sont les seules données du projet. |
| Q11 | Livrables | (a) tables (CSV et xlsx, en-tête de métadonnées), (b) rapport figé HTML, (c) fiche de construction ; (d) PDF hors v1. |
| Q12 | Dépôt | **Public, voulu** ; licence **PolyForm Noncommercial 1.0.0** comme `outil_usp` (fait : `LICENSE`, commit `19cd6d4`). |

## Tour 2 — débloqué par le tour 1

| N° | Question | Décision |
|---|---|---|
| Q13 | Cadre commun | **Noyau multi-états** : une table est l'estimation d'une transition (état de départ, état d'arrivée, axe principal, axe secondaire facultatif, année calendaire facultative, sorties concurrentes, censure) ; chaque risque de Q2 est une configuration du noyau. ADR. |
| Q14 | Règle sur les packages | **(a)** : le moteur n'emploie que R base et les packages livrés avec R ; les packages avancés (`survival`, `mgcv`, `WH`, `ELT`, `StMoMo`, `demography`, `MortalityLaws`, `lifecontingencies`, `survPen`) servent **en contre-vérification dans les tests**, hors moteur ; passage à (b) (un package tiers enveloppé, par méthode) uniquement par ADR. |
| Q15 | Conventions d'âge | Principe : l'utilisateur **déclare la nature de chaque variable** ; l'outil vérifie qu'il a ce qu'il faut et **déduit la convention** (formalisé en Q17). |
| Q16 | Amélioration de la mortalité | Objet commun **surface de mortalité**, dont les modèles conjoints et l'approche base + échelle sont deux producteurs ; base et échelle conservées à part dans le résultat. |

## Tour 3 — données, organisation, textes

| N° | Question | Décision |
|---|---|---|
| Q17 | Catalogue de rôles et recettes d'exposition | Rôles déclarés dans un catalogue fermé (identification, état civil, observation, agrégats, pondération, segmentation, multi-états) ; le moteur retient la **recette d'exposition la plus fine** que les rôles permettent, la nomme dans le rapport, **refuse** sinon. Rien à ajouter au catalogue avant l'avis d'`actuary` ; **tables pondérées par les montants en v1** ; l'utilisateur peut **forcer** une convention plus grossière, inscrite dans la fiche. |
| Q18 | Unité d'observation | **Tête** dès qu'un identifiant de tête est déclaré (fusion des intervalles d'observation), sinon contrat avec avertissement ; incohérences entre contrats d'une tête relevées, jamais corrigées en silence. |
| Q19 | Période d'observation | Déclarée, jamais déduite ; **délai de déclaration** déclaré qui tronque la fin de période ; estimation des événements tardifs (IBNR) hors v1. Précision du mainteneur : **bonne prise en compte de la censure et de la troncature**. |
| Q20 | Segmentation | Sexe toujours, plus une variable catégorielle libre en v1 ; tables par segment et test d'homogénéité en diagnostic ; **l'absence de segmentation est un choix possible** (précision du mainteneur) ; plusieurs variables libres en v2. |
| Q21 | Suffisance des données | Indicateurs de suffisance qui signalent sans interdire ; positionnement sur table de référence avec crédibilité comme voie des bases modestes ; tables de référence embarquées si librement redistribuables, sinon importées ; seuils à fixer par `actuary`. |
| Q22 | Plan de construction | **(A)** : le moteur exécute un plan (étapes ordonnées et paramètres) d'un seul bouton ; comparer des méthodes = plusieurs plans ; fiche de construction = plan + empreintes. Le mainteneur « préfère (A) mais reste ouvert ». |
| Q23 | Structure et noms | Package R **`tablexp`**, point d'entrée **`construire_table(donnees, roles, plan)`**, modules `donnees`, `exposition`, `brut`, `lissage`, `positionnement`, `prospective`, `fermeture`, `validation`, `simulation`, `rapport` ; application Shiny à part (`app.R`, `R/affichage.R`), sans calcul ; noms français sans accents dans le code. |
| Q24 | Contre-vérifications | **(A)** : valeurs de référence externes écrites en dur dans les tests, produites par un script hors CI avec versions de packages consignées ; régénération manuelle sur ordre, avec tableau avant / après. |
| Q25 | Textes qui font foi | **Best estimate** : directive 2009/138/CE art. 77 ; règlement délégué (UE) 2015/35 art. 22 ; orientations EIOPA sur les provisions techniques (révision 2022, à obtenir) ; notice ACPR du 17/12/2015 (orientations 25 et 87). **Comptes sociaux et certification** : Code des assurances A132-18, A132-19 et annexe (A223-8, A932-3-11) ; règlement ANC 2015-11 (art. 142-4, 143-12, 600-2 à 600-5) ; Lignes directrices mortalité de l'Institut des actuaires (2006) ; rapport de la Commission d'agrément 2015 ; notes II1291-11 à -15. Versionnement : textes sous licence etalab (Légifrance) dans le dépôt avec mention de source ; recueil ANC et documents IA en lien seulement, sauf accord écrit. `regulatory` vérifie A132-18 alinéa par alinéa sur le fac-similé avant toute citation. |
| Q26 | Suivi d'expérience | Le module de validation accepte **n'importe quelle table** en entrée ; le **suivi d'expérience** (suivi annuel de certification = backtesting de l'orientation 87) est un livrable de la v1, avec rapport figé dédié. |
| Q27 | Tables uniques | Les deux voies : (A) combinaison des tables par sexe avec pondération **déclarée** (par défaut), (B) estimation sur la population regroupée ; voie et pondération dans la fiche. |
| Q28 | Tables de référence embarquées | Embarquées sous licence ouverte : TD/TV 88-90, TH/TF 00-02 et décalages, TGH05/TGF05, table unique 2024, tables INSEE ; chacune avec un **fichier de provenance** (source, URL, date, licence, empreinte, méthode de transcription, contre-vérification) ; transcription = un commit par table avec sa contre-vérification. HMD, tables IA et lois ANC 600-x par import tant que la licence n'est pas établie. |
| Q29 | Rapport de recherche | Versé tel quel sous `docs/recherche/` (fait : commit `fae1489`). |

## Tour 4 — méthodes, architecture interne, interface

| N° | Question | Décision |
|---|---|---|
| Q30 | Jalons de méthodes | **v1 (mortalité, table du moment)** : exposition et taux bruts (Hoem ; Kaplan-Meier ; binomial et Poisson ; hypothèses de répartition dans l'année) ; lissage (Whittaker-Henderson, moyennes mobiles, Gompertz-Makeham, Heligman-Pollard, Kannisto, P-splines Poisson) ; positionnement (SMR, Brass, GLM Poisson avec référence, lissage + dérive) ; fermeture (Coale-Kisker, Denuit-Goderniaux, Kannisto) ; validation complète (Lignes directrices + II1291-14) ; suivi d'expérience ; simulateur. **v2** : prospective (Lee-Carter log-Poisson, CBD, APC, Renshaw-Haberman, Plat ; échelles MP et CMI ; surface commune ; bootstrap des projections). **v3+** : rachat, prévoyance (deux dimensions), dépendance, chômage. `actuary` amende la liste de la v1 avant le plan d'`architect`. |
| Q31 | Incertitude | (a) IC analytiques sur les taux bruts et (b) bootstrap par têtes sous graine (risque d'estimation) en v1 ; (c) approche bayésienne en v2, en complément. |
| Q32 | Prudence et comparaison | Étape **« marge »** explicite du plan (toujours un paramètre déclaré, jamais un défaut) ; comparaison de deux tables sur les q_x, les espérances de vie résiduelles et les **facteurs de rente** à un taux technique déclaré. |
| Q33 | Simulateur | Modèle multi-états exécuté en avant (loi vraie = table de référence × facteur par segment, dérive calendaire optionnelle, sorties concurrentes, censure) ; produit les trois formes de données ; défauts paramétrés ; chaque jeu versionné porte sa loi vraie et sa graine ; validation par simulation = batterie de tests. |
| Q34 | Transitions en v1 | Interface : vivant → décédé seulement ; une seconde transition (en cours → racheté) couverte par les tests sur données simulées dès la v1. |
| Q35 | Lignes invalides | Refus par défaut ; règles d'exclusion déclarées dans le plan, par motif, comptées et listées dans le rapport de validation ; jamais de correction automatique. |
| Q36 | Interface | Disposition d'`outil_USP` (zone principale, panneau de paramètres à droite, bouton unique), onglets Données, Exposition, Taux bruts, Lissage / Positionnement, Fermeture, Validation, Table, Comparaison, Suivi d'expérience, Rapport ; **pas d'édition des données en ligne** ; plan édité et exporté / importé. **Décision ouverte** : le mainteneur tranchera sur pièces (« sujet à discussion lorsque je verrai le résultat en pratique »). |
| Q37 | Contrat d'entrée du moteur | Le moteur reçoit un data.frame et des rôles, jamais un fichier ; lecture des fichiers (CSV en R base, xlsx par `openxlsx` optionnel) hors moteur ; deux empreintes dans la fiche (fichier, données reçues). |
| Q38 | Code compilé | R pur d'abord, mesuré sur le simulateur à la volumétrie cible ; C par l'interface native de R (sans `Rcpp`) seulement sur mesure de lenteur documentée, par ADR. |
| Q39 | Version de R | **R 4.3.1**, comme `outil_usp` (décision du mainteneur **contre** la recommandation « dernière version stable »). |
| Q40 | ADR à rédiger | Cinq : (1) R et politique de dépendances ; (2) noyau multi-états, rôles et recettes ; (3) package modulaire à point d'entrée unique, en dérogation au fichier unique ; (4) plan de construction et fiche ; (5) tables de référence embarquées et provenance. Rédigés par `architect-approfondi` sur Fable (accord du mainteneur). |

## Consigne hors question

**Documentation LaTeX** : suivre la **même charte graphique** qu'`outil_USP` et une approche similaire — diagramme représentant le code, fonctions R distinguables (`\code{}`) et **tableaux les recensant** —, pour relier facilement la théorie à l'implémentation.

## Points supposés par la session, confirmés par le mainteneur

| N° | Supposition confirmée |
|---|---|
| A1 | Un seul document LaTeX (`docs/latex/`), une partie par module, préambule et macros repris d'`outil_USP` (`\code{}`, `\refl{}`, `encadre`, couleurs), `CONVENTIONS.md` adapté, diagramme du code et index des fonctions par module, test de concordance doc ↔ code dès la première branche, PDF compilé et versionné (skill `compiler-doc` et hook LaTeX repris). |
| A2 | Domaines de commit : `moteur:` (package), `app:` (Shiny), `tests:`, `docs:`, `donnees:` (jeux simulés, tables de référence et provenances), `claude:`, `repo:`. |
| A3 | `docs/exigences.md` rédigé par la session à partir de cet entretien, numéroté ; relu par `actuary` et `regulatory` dans la première branche ; validé par le mainteneur. |
| A4 | `expert.md` dédoublé en `actuary.md` et `regulatory.md` (variantes `-approfondi` régénérées, `ROLES` mis à jour) ; `CLAUDE.md` et `README.md` suivent. |
| A5 | Les décisions de cet entretien sont consignées, numérotées M1…, dans `docs/feuille-de-route.md` § 4, avec renvoi aux ADR. |

## Faits établis pendant l'entretien (mesurés ou sourcés)

- Dépôt `outil_tables` public, sans licence avant `19cd6d4` (`gh repo view`).
- Licence d'`outil_usp` : PolyForm Noncommercial 1.0.0 avec préambule bilingue (fichier lu).
- Pas d'USP de mortalité (règlement délégué 2015/35, art. 218, version anglaise lue) ; siège réglementaire A132-18 (A335-1 abrogé au 01/01/2016) ; certification encadrée par l'Institut des actuaires (suivi annuel, validité 5 ans / 4 ans) ; unisexe généralisé depuis le 24/10/2024 ; `MortalitySmooth` archivé (2020), `StMoMo` figé (2018) ; aucun équivalent Python mûr — détail et statuts dans `docs/recherche/2026-10-14-cadre-tables-experience.md`.
- Conventions LaTeX d'`outil_USP` : `docs/latex/CONVENTIONS.md` (68 lignes, quatre sections : structure, contenu, typographie, manière de modifier) ; la charte elle-même (préambule, macros, couleurs) est dans `doc_tests_usp.tex`.

## Glossaire

Vingt-trois entrées ajoutées à `CONTEXT.md` au fil de l'entretien (section « Domaine »).
