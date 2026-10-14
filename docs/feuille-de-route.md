# Feuille de route

Tenue par `architect`, après chaque série de PR fusionnées. Chaque mise à jour est datée et cite le SHA de `main` et de la branche de travail.

Mise à jour du 14/10/2026 (`architect-approfondi` sur Fable, décision Q40 ; complétée par la session principale après les réponses I1 à I4 du mainteneur) : `main` à `54af049` ; branche `claude/setup-depot` à `c09b27c` au moment de la rédaction (entretien de cadrage, glossaire et cahier des charges commités en `5b8e027`, fiches d'agents en `fe56182` ; ADR 0002-0006 et cette feuille commités ensemble, dans le commit `docs:` qui suit). Aucune branche de travail de code n'est ouverte : le plan de la première branche est une mission ultérieure, après l'avis d'`actuary` sur la v1 (Q30, E2.12) et le relevé de l'issue #2.

## 1. Branche de travail en cours

- **Branche** : `claude/<nom>` — PR #<n> (brouillon)
- **Périmètre** (fermé) :
  - [ ] #<issue> — <titre> — agent : <coder…> — circuit : <1-4> — résultats : <aucun / lesquels>
- **Ordre des commits** : <les commits qui changent un résultat, dans l'ordre>
- **Revue finale complète** : <vérificateurs désignés>

## 2. Branches suivantes

| Branche | Issues | Motif du regroupement | Dépend de |
|---|---|---|---|

## 3. Issues hors plan

Issues ouvertes non rattachées à une branche, avec la raison.

## 4. Décisions du mainteneur

Décisions de l'entretien de cadrage du 14/10/2026 (`docs/cadrage/2026-10-14-entretien-cadrage.md`), numérotées dans l'ordre des questions ; les questions qui ne forment qu'une décision sont regroupées. Toutes sont du mainteneur, sur recommandation de la session sauf mention « contre la recommandation ». Les `E<n>` renvoient à `docs/exigences.md`.

| N° | Date | Décision | Où elle est consignée |
|---|---|---|---|
| M1 | 14/10/2026 | Q1 — Finalité des dossiers : hypothèses de best estimate Solvabilité II (b), puis certification de table (a), puis modèle interne (c) ; rachat, incapacité-invalidité et dépendance relèvent des mêmes dossiers | E1.2 ; ADR 0006, point 4 (textes par régime) |
| M2 | 14/10/2026 | Q2 — Tous les risques, dans l'ordre mortalité / longévité → rachat → prévoyance → dépendance → chômage des emprunteurs | E1.4 ; ADR 0003, point 1 (chaque risque = configuration du noyau) |
| M3 | 14/10/2026 | Q3 + Q16 — Tables du moment et prospectives ; échelles MP (SOA) et CMI en plus de la famille Lee-Carter ; surface de mortalité objet commun, base et échelle conservées à part ; granularité d'âge paramétrable | E1.4, E2.13 ; `CONTEXT.md` (« Table du moment », « Table prospective », « Amélioration de la mortalité », « Surface de mortalité ») |
| M4 | 14/10/2026 | Q4 + Q14 + Q24 + Q38 — R ; moteur en R de base (packages de priorité `base`) ; packages avancés en contre-vérification hors moteur et hors CI, valeurs en dur avec versions consignées, régénération manuelle sur ordre ; R pur mesuré avant tout C, par l'interface native sans `Rcpp`, par ADR | ADR 0002 ; E4.2, E4.8 |
| M5 | 14/10/2026 | Q5 — Le mainteneur est l'utilisateur ; application web locale lancée depuis R (Shiny), comme `outil_USP` | ADR 0004, point 5 ; E1.5, E5.1 |
| M6 | 14/10/2026 | Q6 — Les trois formes de données (lignes individuelles, agrégats, stocks successifs) « et au-delà » ; versatilité critère majeur | E2.1 ; ADR 0003 (rôles et recettes) |
| M7 | 14/10/2026 | Q7 — Volumétrie cible : millions de lignes, dizaines d'années ; faisabilité sur bases modestes étudiée (Q21) | E1.3, E2.15, E4.8 ; ADR 0002, point 6 (mesure du R pur) |
| M8 | 14/10/2026 | Q8 — Dispositifs repris d'`outil_USP` : (1) frontière calcul / affichage, (3) non-régression par la CI avec tableau avant / après et visa, (4) graine locale, (5) rapport figé avec empreintes, (6) documentation LaTeX avec test de concordance, (7) quatre statuts de rigueur ; **écarté** : (2) moteur en fichier unique | ADR 0004 (1, 2, 6) ; ADR 0005 (3, 4, 5) ; E4.3, E4.7, E4.6, E3.6, E3.3, E2.20, E4.9 |
| M9 | 14/10/2026 | Q9 + A4 — Deux experts de fond, `actuary` et `regulatory` (`expert.md` dédoublé, variantes `-approfondi` régénérées, `ROLES` mis à jour) | `.claude/agents/`, `CLAUDE.md`, `README.md` (commit `fe56182`) ; annotation du 14/10/2026 de l'ADR 0001 ; `CONTEXT.md` |
| M10 | 14/10/2026 | Q10 + Q33 — Simulateur composante du moteur : modèle multi-états exécuté en avant (loi vraie = table de référence × facteur par segment, dérive optionnelle, sorties concurrentes, censure), défauts paramétrés, trois formes de données ; jeux simulés versionnés avec loi vraie et graine, seules données du projet ; validation par simulation = batterie de tests | ADR 0003, point 9 ; E1.5, E2.19 |
| M11 | 14/10/2026 | Q11 — Livrables : tables CSV et xlsx avec en-tête de métadonnées, rapport figé HTML, fiche de construction ; PDF hors v1 | ADR 0005, point 7 ; E3.5 |
| M12 | 14/10/2026 | Q12 — Dépôt public, voulu ; licence PolyForm Noncommercial 1.0.0 | `LICENSE` (commit `19cd6d4`) ; ADR 0006, point 2 (licence propre des tables) |
| M13 | 14/10/2026 | Q13 + Q15 + Q17 — Noyau multi-états : une table = l'estimation d'une transition ; rôles déclarés dans un catalogue fermé ; recette d'exposition la plus fine, nommée, refus sinon ; forçage d'une convention plus grossière inscrit dans la fiche ; pondération par les montants en v1 ; catalogue figé jusqu'à l'avis d'`actuary` | ADR 0003, points 1 à 4 ; E2.2, E2.3, E2.8, E2.11 ; `CONTEXT.md` (« Transition », « Rôle », « Recette d'exposition », « Convention d'âge ») |
| M14 | 14/10/2026 | Q18 — Unité d'observation : la tête dès qu'un identifiant est déclaré (fusion des intervalles), sinon le contrat avec avertissement ; incohérences relevées, jamais corrigées en silence | ADR 0003, point 5 ; E2.4 ; `CONTEXT.md` (« Tête ») |
| M15 | 14/10/2026 | Q19 — Période d'observation et délai de déclaration déclarés, jamais déduits ; troncature à gauche et censure à droite traitées explicitement ; estimation des événements tardifs hors v1 | ADR 0003, point 6 ; E2.5, E2.6 ; `CONTEXT.md` |
| M16 | 14/10/2026 | Q20 — Segmentation : sexe toujours, plus une variable catégorielle libre en v1 ; l'absence de segmentation est un choix explicite ; tables par segment et test d'homogénéité en diagnostic ; plusieurs variables en v2 | E2.9 ; `CONTEXT.md` (« Segment ») |
| M17 | 14/10/2026 | Q21 — Indicateurs de suffisance qui signalent sans interdire ; positionnement sur table de référence avec crédibilité pour les bases modestes ; tables de référence embarquées si librement redistribuables, sinon importées ; seuils fixés par `actuary` | E2.15, E2.26 ; ADR 0006, point 1 ; `CONTEXT.md` (« Indicateur de suffisance ») |
| M18 | 14/10/2026 | Q22 — Plan de construction exécuté d'un bloc (option A ; le mainteneur « préfère (A) mais reste ouvert ») ; comparaison = plusieurs plans ; fiche de construction = plan + empreintes | ADR 0005 ; E4.5, E5.4 |
| M19 | 14/10/2026 | Q23 — Package `tablexp`, point d'entrée `construire_table(donnees, roles, plan)`, dix modules ; application Shiny à part (`app.R`, `R/affichage.R`), sans calcul ; noms français sans accents | ADR 0004 ; E4.1 |
| M20 | 14/10/2026 | Q25 — Textes qui font foi par régime (best estimate ; comptes sociaux et certification) ; versionnement : textes etalab dans le dépôt, recueil ANC et documents IA par lien sauf accord écrit ; `regulatory` vérifie A132-18 alinéa par alinéa sur le fac-similé | ADR 0006, point 4 ; E2.23 à E2.25 |
| M21 | 14/10/2026 | Q26 — Module de validation ouvert à toute table ; suivi d'expérience livrable de la v1 avec rapport figé dédié | ADR 0006, point 5 ; E2.18, E3.5 ; `CONTEXT.md` (« Suivi d'expérience ») |
| M22 | 14/10/2026 | Q27 — Table unique : combinaison des tables par sexe à pondération déclarée (défaut) ou estimation sur la population regroupée ; voie et pondération dans la fiche | ADR 0005, point 4 ; E2.10 ; `CONTEXT.md` (« Table unique ») |
| M23 | 14/10/2026 | Q28 — Tables embarquées sous licence ouverte (TD/TV 88-90, TH/TF 00-02 et décalages, TGH05/TGF05, table unique 2024, tables INSEE), fichier de provenance par table, un commit par table avec contre-vérification ; HMD, tables IA et lois ANC 600-x importées tant que la licence n'est pas établie | ADR 0006, points 1 à 3 ; E2.26 |
| M24 | 14/10/2026 | Q29 — Rapport de recherche versé tel quel sous `docs/recherche/` | commit `fae1489` |
| M25 | 14/10/2026 | Q30 — Jalons : v1 mortalité, table du moment (exposition et taux bruts, lissage, positionnement, fermeture, validation complète, suivi d'expérience, simulateur) ; v2 prospective ; v3+ rachat, prévoyance, dépendance, chômage ; `actuary` amende la liste de la v1 avant le plan d'`architect` | E1.4, E2.12, E2.13 |
| M26 | 14/10/2026 | Q31 — Incertitude : IC analytiques sur les taux bruts et bootstrap par têtes sous graine en v1 ; approche bayésienne en v2 | E2.14, E2.13 |
| M27 | 14/10/2026 | Q32 — Étape « marge » explicite, paramètre toujours déclaré, jamais un défaut ; comparaison de deux tables sur les q_x, les espérances de vie résiduelles et les facteurs de rente à taux technique déclaré | ADR 0005, points 2 et 3 ; E2.16, E2.17 |
| M28 | 14/10/2026 | Q34 — Interface v1 : transition vivant → décédé seulement ; seconde transition (en cours → racheté) couverte par les tests sur données simulées dès la v1 | ADR 0003, point 8 ; E2.11 |
| M29 | 14/10/2026 | Q35 — Lignes invalides refusées par défaut ; règles d'exclusion déclarées dans le plan, par motif, comptées et listées dans le rapport de validation ; jamais de correction automatique | ADR 0003, point 7 ; ADR 0005, point 5 ; E2.7, E5.3 |
| M30 | 14/10/2026 | Q36 — Interface : disposition d'`outil_USP` (zone principale, panneau de paramètres à droite, bouton unique), dix onglets, pas d'édition des données en ligne, plan édité et exporté / importé — **décision ouverte** : le mainteneur tranchera sur pièces (« lorsque je verrai le résultat en pratique ») | ADR 0004, point 5 (signalée ouverte) ; ADR 0005, point 1 (bouton unique, export du plan) ; E5.1 à E5.5 (§ 5 marqué ouvert) |
| M31 | 14/10/2026 | Q37 — Contrat d'entrée du moteur : data.frame et rôles, jamais un fichier ; lecture des fichiers hors moteur ; deux empreintes dans la fiche (fichier, données reçues) | ADR 0004, point 3 ; E4.4, E3.6 |
| M32 | 14/10/2026 | Q39 — **R 4.3.1**, comme `outil_USP` — décision **prise contre la recommandation** de la session (« dernière version stable ») | ADR 0002, point 2 ; E4.1, E4.7 |
| M33 | 14/10/2026 | Q40 — Cinq ADR (0002 à 0006) rédigés par `architect-approfondi` sur Fable, sur accord du mainteneur | ADR 0002 à 0006 ; § 5 ci-dessous |
| M34 | 14/10/2026 | Consigne hors question + A1 — Documentation LaTeX : charte graphique d'`outil_USP`, un seul document (`docs/latex/`), une partie par module, diagramme du code, tableaux des fonctions (`\code{}`), test de concordance doc ↔ code dès la première branche, PDF compilé et versionné | ADR 0004, point 6 ; E3.1 à E3.3 |
| M35 | 14/10/2026 | A2 — Domaines de commit : `moteur:`, `app:`, `tests:`, `docs:`, `donnees:`, `claude:`, `repo:` | `CLAUDE.md` (commit `fe56182`) ; ADR 0006, point 3 (`donnees:`) |
| M36 | 14/10/2026 | A3 — `docs/exigences.md` rédigé par la session à partir de l'entretien, numéroté ; relu par `actuary` et `regulatory` dans la première branche ; validé par le mainteneur | `docs/exigences.md` (commit `5b8e027`) ; relecture prévue par le modèle de PR (commit `c09b27c`) |
| M37 | 14/10/2026 | A5 — Décisions de l'entretien consignées ici (M1…), avec renvoi aux ADR | cette feuille, § 4 |
| M38 | 14/10/2026 | I1 à I4 (interprétations des ADR, confirmées) — I1 : moteur en priorité `base` **confirmé**, pour une raison supplémentaire que le mainteneur révèle : son **poste de travail pourrait ne pas permettre d'installer des packages CRAN** ; il mesure ce qui y est installable (R, CRAN, Rtools, packages par groupe) et rend son relevé dans l'**issue #2** ; un package disponible et utile (accélération C / C++, méthode, contre-vérification) pourra ensuite être admis par annotation de l'ADR 0002 puis ADR par méthode. I2 : points d'entrée propres au suivi d'expérience et au simulateur. I3 : fiche écrite par le moteur. I4 : A335-1 à titre historique seulement | ADR 0002 (contexte, point 5, « ce que l'ADR ne règle pas », issue #2) ; ADR 0004, point 2 ; ADR 0005, point 6 ; ADR 0006, point 4 ; E4.2, E2.24 ; `CLAUDE.md`, « Architecture » |
| M39 | 14/10/2026 | **Pas de plafond de consultations Fable pour la première branche de travail** (exception propre au cadrage, qui lève pour cette branche la limite de trois du § 5.2 de `docs/agents/routage.md`) ; la recherche d'agent et la rédaction des ADR du 14/10 ne sont donc imputées à aucun plafond | cette feuille, § 5 ; à rappeler dans la PR de la première branche |

## 5. Escalades, relances et arrêts hors branche

Consultations faites hors de toute branche ouverte (`docs/agents/routage.md`, § 7) ; une consultation Fable compte pour la branche suivante.

| Date | Fiche / modèle / critère déclenché / statut obtenu / suite |
|---|---|
| 14/10/2026 | Agent général de recherche (hors matrice § 3) / Fable, effort `high` / collecte de faits pour le cadrage (cadre réglementaire, méthodes, écosystèmes R et Python, licences des tables), sur demande du mainteneur / statut : rapport rendu avec statuts vérifié / incertain / non trouvé, versé sans retouche (`docs/recherche/2026-10-14-cadre-tables-experience.md`, commit `fae1489`, Q29) / suite : les faits « incertain » et « non trouvé » sont à établir par `regulatory` ou `actuary` avant toute citation dans la documentation ou un dossier ; imputation au plafond du § 5.2 : sans objet, décision M39 (aucun plafond Fable pour la première branche) |
| 14/10/2026 | `architect-approfondi` / Fable, effort `high` / § 4.1.3 rédaction d'ADR d'architecture, sur accord explicite du mainteneur (Q40) / statut : `complet` — ADR 0002 à 0006 proposés (`status: accepted` au titre de Q40, relecture du mainteneur attendue), § 4 ci-dessus renseigné / suite : relecture des ADR par le mainteneur ; relecture d'`E` par `actuary` et `regulatory` (A3) ; puis plan de la première branche (consultation `architect-approfondi`, jugement) après l'avis d'`actuary` sur la v1 (Q30). Imputation au plafond : sans objet, décision M39 |
