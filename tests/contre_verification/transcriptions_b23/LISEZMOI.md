# Transcriptions gelées de B23 : pièces de contrôle, non embarquées

> *Rédigé par la session principale (IA), 15/11/2026, branche `claude/transcription-table-unique-2024` (PR #289).*

## Statut

Ce dossier contient les deux transcriptions aveugles de la table unique 2024, faites pour #275 à l'étape (2) du plan de B23. Ce sont des **pièces de contrôle** : aucune de leurs valeurs n'est embarquée, déclarée au catalogue, ni lue par le package ou par un contrôle de la CI.

Le mainteneur a décidé le 15/11/2026 de les verser tout de suite, parce que le scratchpad de la session n'est pas durable. Le précédent est B22 (`b949ced`, M474). L'ordre du plan n'est pas respecté : il prévoyait de les verser avec le commit `donnees:` de l'étape (6). Cet écart est consigné dans la PR #289.

Le dossier est **provisoire**. Au commit `donnees:` de l'étape (6) :
- la grille assemblée ira sous `inst/extdata/tables/` ;
- les relevés iront sous `tests/contre_verification/tables/` ;
- ce dossier sera ensuite retiré, et restera lisible dans l'historique.

Il se trouve hors de `tests/contre_verification/tables/`, parce que le contrôle (a) de `tests/controles/tables_embarquees.R` y refuse tout fichier qui n'est rattaché à aucune table déclarée.

## Méthode

Méthode suivie : E2.26 ; D10 de B22 ; D3 (b) de M485.

- **Agents** : 20 consultations distinctes de `coder-approfondi`, à l'effort `medium`, soit dix blocs par transcripteur. Chaque bloc compte onze générations, sauf le dernier, 1999-2005, qui en compte sept.
- **Pièce lue** : `docs/sources/jo_2024-11-22_n276_texte17.pdf` (SHA-256 `1a2f79c347620af519bd13f681c7cee63e3eedf545e12e8d7256c973e9ceef77`), uniquement sur le rendu image (`pdftoppm -r 300`). Aucune extraction de texte n'a été faite.
- **Consigne commune** : `brief_transcription.md`. Elle ne contient aucune valeur. Elle interdit l'extraction de texte, l'accès aux autres pièces, à l'historique git, aux issues et au dossier de l'autre transcripteur.
- **Pages par bloc** : la session les a repérées sur les seuls en-têtes de colonnes.

| Bloc | Pages de l'extrait |
|---|---|
| 1900-1910 | 2 à 5 |
| 1911-1921 | 5 à 8 |
| 1922-1932 | 8 à 11 |
| 1933-1943 | 12 à 15 |
| 1944-1954 | 15 à 18 |
| 1955-1965 | 18 à 21 |
| 1966-1976 | 21 à 24 |
| 1977-1987 | 24 à 28 |
| 1988-1998 | 28 à 31 |
| 1999-2005 | 31 à 34 |

- **Format des blocs** : `age,<générations>`, âges 0 à 121. Une cellule « - » est transcrite `-` ; une cellule imprimée vide serait transcrite `VIDE`, et aucune ne l'est.
- **Notes** : chaque bloc a ses notes de transcripteur sous `notes/`. Elles citent les images et les scripts de travail, qui ne sont pas versés.
- **Gel** : `gel.sha256` porte le SHA-256 des 20 blocs et des 20 notes. Il a été écrit avant toute comparaison. Le SHA-256 du fichier `gel.sha256` lui-même est `6f0222227323e2748476921238dafc46e4e36099feae45412760984fbbfc6605`. Pour le vérifier, depuis ce dossier : `sha256sum -c gel.sha256`.

## Résultats

- **A = B octet pour octet sur les 10 blocs**, par `cmp`, soit 12 932 cellules.
- Aucune cellule `VIDE`, aucune ligne mal composée, aucune cellule douteuse.
- **Écart à la consigne** : le transcripteur A du bloc 1977-1987 a fait sa seconde lecture par des contrôles de régularité et des zooms ciblés, et non par une seconde lecture visuelle complète. Le bloc reste couvert par A = B.
- **Identité** : `actuary-approfondi` a comparé ces blocs, avec son propre script, à la grille calculée par 0,6 l_x TGH05 + 0,4 l_x TGF05. Il trouve 0 écart sur 12 826 cellules (âges 0 à 120 ; commentaire https://github.com/Gerard-Garey/outil_tables/issues/275#issuecomment-6892051278). Ce n'est pas le contrôle (h), qui reste à exécuter à l'étape (4) sur la grille assemblée.
