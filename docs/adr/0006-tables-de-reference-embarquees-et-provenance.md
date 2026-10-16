---
status: accepted
date: 2026-10-14
---

# Tables de référence embarquées sous licence ouverte avec fichier de provenance, les autres importées ; textes qui font foi par régime

## Contexte

Le positionnement (Q21, Q30), la fermeture, le suivi d'expérience (Q26), la comparaison (Q32) et le simulateur (Q33 : loi vraie = table de référence × facteur) ont besoin de tables de référence dans le dépôt ; un dépôt public (Q12) ne peut embarquer que ce qu'il a le droit de redistribuer. Les dossiers citent des textes dont le siège a changé : A335-1 est abrogé depuis le 01/01/2016 et le siège est A132-18 (recherche § 1.a, vérifié) ; le texte intégral de A132-18 n'a pas été relu alinéa par alinéa (recherche, « Points que je n'ai pas pu établir », 2).

Licences établies par la recherche (§ 1.d, consultation du 14/10/2026) :

- Légifrance : « Sauf mention contraire, tous les contenus de ce site sont sous licence etalab-2.0 » [vérifié] — TD/TV 88-90 (arrêté du 27/04/1993, l_x lisibles sur la page), TH/TF 00-02 et décalages d'âge (annexe de A132-18, fac-similé du JO du 29/12/2015), TGH05/TGF05 (arrêté du 1er août 2006, fac-similé du JO du 26/08/2006), table unique 2024 (annexe de l'arrêté du 18/11/2024, texte visible) [vérifié pour chaque source ; version tabulaire officielle de TGH05/TGF05 après 2015 : incertain] ;
- INSEE : Licence Ouverte / Open Licence 2.0, réutilisation libre avec mention « Source : Insee » et date [vérifié] ;
- HMD : estimations sous CC BY 4.0, mais Input Data sous la licence du fournisseur, non republiables, et recommandation de ne pas diffuser de copies [vérifié] ;
- tables et notes de l'Institut des actuaires (xlsx TH/TF, INSEE 2060 lissée, IA 2013, notes II1291) : aucune licence indiquée [non trouvé] ;
- lois de maintien du règlement ANC 2015-11, art. 600-2 à 600-5 : source vérifiée, réutilisation du recueil « © ANC » non précisée [incertain] ;
- tables de génération 1993 (TPRV / TPG 93) : non trouvées en ligne [non trouvé].

## Décision

Arrêtée par le mainteneur le 14/10/2026 (Q21, Q25, Q26, Q28) :

1. **Embarquées** dans `tablexp`, sous licence ouverte vérifiée : TD/TV 88-90, TH/TF 00-02 et décalages d'âge, TGH05/TGF05, table unique 2024, tables INSEE (Q28). **Importées** par l'utilisateur, tant que leur licence n'est pas établie par écrit : HMD, tables de l'Institut des actuaires, lois de maintien ANC 600-x (Q28, Q21). Une table passe d'« importée » à « embarquée » par annotation datée du présent ADR, avec la licence obtenue.
2. **Un fichier de provenance par table embarquée** (Q28) : source, URL, date de consultation, licence, empreintes du fichier source et de la table transcrite, méthode de transcription, contre-vérification (seconde source indépendante ou seconde transcription ; contrôles de cohérence : l_x décroissants, bornes, valeurs connues). La licence du dépôt (PolyForm Noncommercial 1.0.0, Q12) ne couvre pas les tables : chacune reste sous la licence de sa provenance, rappelée dans le fichier et dans la documentation.
3. **Transcription : un commit `donnees:` par table** (A2), contenant la table, sa provenance et sa contre-vérification ; aucune table n'entre sans contre-vérification. Une table relue depuis un fac-similé (TH/TF, TGH/TGF) est contre-vérifiée contre une seconde source indépendante ou une seconde transcription.
4. **Textes qui font foi** (Q25), par régime :
   - **best estimate** : directive 2009/138/CE, art. 77 ; règlement délégué (UE) 2015/35, art. 22 ; orientations EIOPA sur les provisions techniques, révision 2022 (à obtenir) ; notice ACPR du 17/12/2015 (orientations 25 et 87) ;
   - **comptes sociaux et certification** : Code des assurances A132-18, A132-19 et annexe (A223-8 mutualité, A932-3-11 sécurité sociale) ; règlement ANC 2015-11, art. 142-4, 143-12, 600-2 à 600-5 ; Lignes directrices mortalité de l'Institut des actuaires (2006) ; rapport de la Commission d'agrément 2015 ; notes II1291-11 à -15.
   Versionnement : les textes sous licence etalab (Légifrance) sont versés dans le dépôt avec mention de source et date ; le recueil ANC et les documents de l'Institut des actuaires sont cités par lien seulement, sauf accord écrit. **`regulatory` vérifie A132-18 alinéa par alinéa sur le fac-similé avant toute citation** ; A335-1 n'est cité qu'à titre historique.
   *(16/10/2026, décision M46, amendements `regulatory` A1-A9 de la relecture M36 : la liste ci-dessus est complétée par `docs/exigences.md` E2.23 et E2.24 amendées (commit `2e8b4e9`), qui font foi pour l'énumération des textes. Pour le best estimate, E2.23 cite en outre le règlement délégué (UE) 2015/35, art. 19 et 20 (qualité des données) ; les **chocs** appliqués aux taux de mortalité, art. 137 et 138, et en santé SLT art. 152 et 153 (art. 139, 154, 142 et 159 pour les risques de la v3) ; l'**art. 218**, dont la liste fermée des paramètres propres à l'entreprise ne contient aucun paramètre de mortalité, de longévité, d'invalidité-morbidité ni de cessation ; et les orientations EIOPA révisées par le rapport final EIOPA-BoS-22/217 du 21 avril 2022. Pour les comptes sociaux, E2.24 ajoute A132-18-1, A223-8-1, A932-3-12, L111-7 et les références des arrêtés au JO. Les références des textes français restent à confirmer sur la source (issue #13). Le texte ci-dessus n'est pas réécrit.)*
5. **Suivi d'expérience sur toute table** (Q26 ; `CONTEXT.md`, « Suivi d'expérience ») : le module `validation` accepte n'importe quelle table — construite, embarquée, importée, homologuée ou certifiée — dans une représentation interne commune ; le suivi (ratio observés / attendus global et par sous-groupes, SMR, signes, runs, intervalles) est un livrable de la v1 avec rapport figé dédié ; il répond au suivi annuel de certification (rapport 2015 : suivi annuel, validité 5 ans / 4 ans ; vérifié) et au backtesting de l'orientation 87 (vérifié).

## Options écartées

- **Tout embarquer** (HMD, tables IA, lois ANC comprises) : les Input Data de la HMD ne sont pas republiables et l'Institut des actuaires n'affiche aucune licence ; un dépôt public ne prend pas ce risque pour s'épargner un import.
- **N'embarquer aucune table** : les tests de positionnement, de fermeture et de simulation perdraient leur référence versionnée ; chaque utilisateur retranscrirait le JO sans contre-vérification partagée.
- **Télécharger à l'exécution** depuis Légifrance, l'INSEE ou la HMD : non reproductible (contenu et format mouvants, inscription HMD), dépendance réseau, et Légifrance a bloqué l'accès programmatique pendant la recherche (« Points que je n'ai pas pu établir », 2) ; une table de référence est une donnée versionnée, pas une ressource distante.
- **Reproduire les tables ANC 600-x ou les notes IA dans le dépôt sans accord** : réutilisation non établie ; lien seulement (point 4).
- **Citer A335-1** comme siège réglementaire : abrogé ; erreur relevée par la recherche (§ 1.a ; « Faits qui changent le cadrage », 4).

## Conséquences

- Fichiers : données du package (emplacement et format fixés au plan de la première branche), un fichier de provenance à côté de chaque table, scripts de contre-vérification, dossier des textes etalab versés, bibliographie LaTeX ; `docs/exigences.md` : E2.23 à E2.26.
- Effet sur les résultats : aucun à ce jour. Règle durable : une table embarquée corrigée (erreur de transcription) change des résultats de positionnement, de comparaison et de simulation : tableau avant / après et visa du mainteneur (résultat final).
- Ce que l'ADR ne règle pas : la représentation interne commune des tables (axes âge, sexe, génération ; q_x ou l_x ; contrat partagé, `docs/agents/routage.md` § 4.2) ; l'obtention des orientations EIOPA 2022 et du texte intégral de A132-18 (`regulatory`) ; la demande écrite de licence pour les tables IA et les lois ANC (mainteneur) ; les tables TPRV / TPG 93, non trouvées.
- Vocabulaire : `CONTEXT.md`, « Table de référence », « Table homologuée », « Table certifiée », « Suivi d'expérience », « Table unique ».

Issues : aucune à ce jour.
