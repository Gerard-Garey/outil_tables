# Transcriptions gelées de B22 : pièces de contrôle, non embarquées

> *Rédigé par la session principale (IA), 13/11/2026, branche `claude/transcription-tables-generation` (PR #271).*

## Statut

Ce dossier rassemble les transcriptions et les relevés produits pour #273 (décalages d'âge) et #274 (TGF05, TGH05). On les a versés comme **pièces de contrôle** : aucune valeur de ce dossier n'est embarquée, déclarée au catalogue, ni lue par le package ou par un contrôle de la CI.

Décision du mainteneur du 13/11/2026 : verser ces pièces avant le code du format 2 (#272, pièces 3 à 5), pour ne pas perdre le travail. L'ordre du plan prévoyait « #272 avant tout commit `donnees:` » ; cet écart est consigné dans la PR #271.

Le dossier est **provisoire**. Les commits `donnees:` de #273 et de #274 en reprendront le contenu :
- les fichiers de table iront sous `inst/extdata/tables/` ;
- les relevés iront sous `tests/contre_verification/tables/`, au format et au nommage de l'ADR 0018, point 5, et de l'ADR 0019, point 7 ;
- les fichiers de provenance seront rédigés à ce moment-là.

Ensuite, le dossier sera supprimé. Il est placé hors de `tests/contre_verification/tables/`, parce que le contrôle (a) de `tests/controles/tables_embarquees.R` refuse tout fichier qui n'y est rattaché à aucune table déclarée.

**Visa du mainteneur (13/11/2026)** : accordé aux valeurs de TGF05 et de TGH05 contenues dans `generation/tgf05.csv` et `generation/tgh05.csv` (SHA-256 ci-dessous), comme valeurs de tables homologuées à embarquer. Pour les décalages d'âge (#273), aucun visa n'a encore été demandé.

## Méthode commune

La méthode suit E2.26, M445 et M462, ainsi que la consigne du handoff de la PR #271, § 3.2.
- **Deux transcriptions aveugles et indépendantes**, A et B, faites chacune par un agent distinct, sur le seul rendu image (`pdftoppm`) du fac-similé du JO n° 301 du 29/12/2015, texte 35 (`docs/sources/jo_2015-12-29_n301_texte35.pdf`, SHA-256 `2e348901…9b56`).
- **Interdits des transcripteurs** : aucune extraction de texte, aucune autre pièce, aucune issue ni aucun commentaire contenant des valeurs. Aucune consigne ne portait de valeur.
- **Gel** : chaque fichier rendu a été gelé par son SHA-256 (`gel.sha256`) avant toute comparaison.
- **Comparaisons** : ensuite seulement, comparaison par script de A et de B, puis aux autres pièces, la couche texte ne servant qu'à la comparaison.

## `decalages/` : décalages d'âge de TF 00-02 et de TH 00-02 (#273)

**Source.** p. 19 de l'extrait du JO de 2015 ; format 3 de l'ADR 0019, point 6.2.

| Pièce | Contenu | Résultat de la comparaison |
|---|---|---|
| `transcription_a/`, `transcription_b/` | les deux transcriptions aveugles ; `gel.sha256` | identiques octet pour octet |
| `releve_jo2005/` | relevé aveugle sur le rendu de la p. 6 de `docs/sources/jo_2005-12-29_n302_texte40.pdf` : comparaison à l'acte, arrêté du 20/12/2005, art. 5 | identique octet pour octet au gel |
| `releve_bo/` | relevé par couche texte du BO Santé 2007-1, annexe I, p. 9 et 10 : seconde source, transcrite telle qu'imprimée | les trois écarts déjà annoncés par `docs/sources/README.md`, et aucun autre (voir ci-dessous) |

Les trois écarts du BO, à déclarer au fichier de provenance :
- bornes basses imprimées 0 au lieu de 16 (TF et TH) ;
- ligne « [33 - 34] » de TF imprimée deux fois ;
- « - 0 » pour « 94 et plus » (TF).

Le relevé du BO garde la ligne en double et `-0` : il n'est donc **pas** au format 3. Il faudra choisir sa forme à verser avant le commit `donnees:` de #273.

**Reste à faire, après le gel.** Le commentaire B d'`actuary` (exemples chiffrés sur TH 00-02) peut maintenant être reproduit et versé sur #273 (handoff, § 3.2).

## `generation/` : TGF05 et TGH05 (#274)

**Source.** p. 19 à 83 de l'extrait du JO de 2015.

**Transcriptions.** Une transcription par bloc de onze générations et par transcripteur (D10), soit 20 blocs par transcripteur, sous `blocs_a/` et `blocs_b/`. Les notes de chaque agent sont sous `notes/`, et les SHA-256 du gel dans `gel.sha256`.
- Dans les blocs, une cellule imprimée vide est notée `VIDE`, et une ligne mal composée est transcrite dans l'ordre imprimé.
- Les deux transcripteurs ont relevé chacun A1 et A2, sans en avoir été prévenus.
- **A et B sont identiques sur les 40 fichiers**, soit 25 410 cellules (A2 compte pour une cellule de plus).

**Assemblage.** `assembler.py` assemble les blocs en grilles au format 2 (ADR 0019, point 5.2) et applique M466 :
- **A1** : la cellule (120, 1921) de TGF05, imprimée vide, est écrite `-` ;
- **A2** : les onze nombres de la ligne 117 du bloc TGF05 1955-1965 sont attribués dans l'ordre imprimé, chiffres inchangés.

Aucune ligne n'est entièrement sans valeur. Les blocs de B, assemblés de même, donnent les mêmes octets.

| Fichier | SHA-256 |
|---|---|
| `tgf05.csv` | `a1f43cc182f3cddd176cc9213d8e5d411e23ee54ee59fae69b9eee885b43542e` |
| `tgh05.csv` | `cb6ad2b740f151b3e8ee6689dafb979b630e4ab6209873249445edba0a7d9dc4` |

**Relevés par couche texte** (`releves.py`, `pdftotext -bbox`, poppler 24.02.0) : 12 826 cellules par table et par source. La méthode et les difficultés d'extraction sont dans `rapport.md`.

| Source | Rôle | TGF05 | TGH05 |
|---|---|---|---|
| BO Santé 2007-1, annexe II | seconde source | 0 écart | 0 écart |
| JO n° 197 du 26/08/2006 | comparaison à l'acte (M462) | 0 écart | 0 écart |
| JO de 2015 (couche texte) | contrôle de lecture seulement | 11 écarts, tous dus à A2 | 0 écart |

- Les relevés du BO et du JO de 2006 sont identiques octet pour octet aux grilles assemblées.
- A1 et A2 ont été vérifiés sur le rendu des trois pièces. En 2006 et au BO, la ligne 117 est alignée, avec les mêmes nombres ; la cellule (120, 1921) porte « – » en 2006 et est vide au BO.
- Le relevé du JO de 2015 porte un marqueur `?66/72` sur la ligne 117. Il n'est pas au format 2, et il n'est pas destiné à être versé sous `tables/`.

## Points ouverts pour les commits `donnees:`

1. **Langage des scripts.** `assembler.py` et `releves.py` sont en Python. L'ADR 0019, point 7.2, demande un script d'assemblage hors CI, versé à côté des relevés, sans fixer de langage, alors que les précédents de B21 sont en R. Il faut décider entre les garder ou les réécrire en R, avec un résultat identique au bit près, à vérifier par SHA-256.
2. **Forme du relevé des décalages du BO** (ligne en double, `-0`) : voir la section `decalages/` ci-dessus.
3. **Fichiers de provenance** : champs de l'ADR 0018 et de l'ADR 0019, point 7.1, notamment :
   - `Generations` ;
   - `Comparaison-acte*` ;
   - `Ecarts-signales` (A1, A2, glyphe « – » écrit `-`) ;
   - `Transcription-methode` (blocs D10, assemblage).
