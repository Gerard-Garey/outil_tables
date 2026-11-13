# Assemblage de TGF05 et TGH05, et relevés par couche texte

Date : 13/11/2026. Dossier : `scratchpad/assemblage/`. Rien n'a été écrit dans le dépôt.

## 0. Gel des entrées

`sha256sum -c gel_tg.sha256` lancé depuis `scratchpad/` : toutes les lignes `OK` (tg_A, tg_B, tg_pilote_A). Aucun fichier gelé n'a été modifié.

## 1. Assemblage (`assembler.py`)

Commande : `python3 -I assembler.py ../tg_A .`, code de sortie 0. Sortie :

```
A1 : tgf05 cellule (120, 1921) VIDE écrite -
A2 : tgf05 âge 117, 11 nombres attribués dans l'ordre imprimé aux générations 1955 à 1965 ; VIDE final retiré
tgf05 : lignes sans valeur non transcrites, âges [] ; aucune au milieu
tgf05 : 106 générations (1900-2005), 121 âges (0-120), 8092 cellules à valeur, 4734 cellules '-'
tgh05 : lignes sans valeur non transcrites, âges [] ; aucune au milieu
tgh05 : 106 générations (1900-2005), 121 âges (0-120), 7921 cellules à valeur, 4905 cellules '-'
```

Contrôles du format 2 (ADR 0019, point 5.2) passés pour les deux tables : générations 1900 à 2005 consécutives ; âges 0 à 120 consécutifs ; chaque ligne et chaque colonne porte au moins une valeur ; cellules `0`, chiffres sans zéro initial ou `-` ; colonnes décroissantes au sens large ; aucun `-` entre deux valeurs d'une colonne. Le script échoue sur tout autre `VIDE`, toute autre ligne de longueur anormale, et si A1 et A2 ne sont pas rencontrés tous les deux dans TGF05 (ou si l'un d'eux se présente dans TGH05).

Déterminisme : deux exécutions donnent les mêmes octets. L'assemblage de `tg_B` donne les mêmes deux SHA-256 (A et B identiques).

**Lignes non transcrites : aucune**, dans aucune des deux tables. À chaque âge de 0 à 120, au moins une génération a une valeur (par exemple, l'âge 0 est à valeur pour les dernières générations). La question d'une ligne sans valeur au milieu de la grille ne se pose donc pas.

| Fichier | SHA-256 |
|---|---|
| `tgf05.csv` | `a1f43cc182f3cddd176cc9213d8e5d411e23ee54ee59fae69b9eee885b43542e` |
| `tgh05.csv` | `cb6ad2b740f151b3e8ee6689dafb979b630e4ab6209873249445edba0a7d9dc4` |
| `assembler.py` | `76de276ba38f765842eba84049a758f22bc199c16cfe7e9d54a53e2cd0859092` |
| `releves.py` | `db47aa40cf19737dd20bb9e0a668ba5890de678c11f0cf79b499194e476947e3` |

## 2. Relevés par couche texte (`releves.py`)

Commande : `python3 -I releves.py /home/user/outil_tables/docs/sources . .`. Outil : pdftotext 24.02.0 (poppler), en mode `-bbox`. Ce mode est retenu plutôt que `-layout` parce que le BO laisse vides les cellules sans valeur : pour placer une valeur dans sa colonne, il faut donc son abscisse.

Méthode :
- les lignes sont reconstruites par l'ordonnée des mots (tolérance 2 pt) ;
- chaque mot est attribué à l'année d'en-tête dont le centre est le plus proche. L'en-tête courant est repris sur les pages qui ne le répètent pas. Un mot à plus d'une demi-largeur de colonne est consigné au journal ;
- un groupe de milliers est réuni à son préfixe d'un à trois chiffres, à moins de 4 pt, un seul groupe au plus ;
- les lignes de titre courant et de pied de page sont ignorées (elles contiennent des mots non numériques) ;
- la table en cours est donnée par le titre `TABLE TGF05` ou `TABLE TGH05`.

Deux exécutions donnent les mêmes octets. Les contrôles du format 2 ont été faits sur chaque relevé.

**Normalisations consignées** (dans `journal_<source>.txt`) :
- JO 2015 : 9 449 « – » (U+2013) et 189 « - » (U+002D) écrits `-`, ainsi que 2 cellules sans mot. Les tirets ASCII se trouvent dans TGH05, p. 74 (19 lignes) et p. 77 (7 lignes). Ce sont des glyphes de la couche texte ; ils ne produisent aucun écart ;
- BO 2007-1 : 9 639 cellules sans mot écrites `-`, soit exactement 4 734 + 4 905, le nombre de `-` des grilles assemblées ;
- JO 2006 : 9 639 « – » écrits `-`.

Lignes sans valeur non écrites aux relevés : aucune, dans aucune source.

### Décomptes

Comparaison sur 121 âges × 106 générations, soit 12 826 cellules par table.

| Source | Table | Cellules comparées | Écarts | Relevé SHA-256 |
|---|---|---|---|---|
| (a) JO 2015, p. 19-83, contrôle de lecture | TGF05 | 12 826 | **11** | `0b970e3096df458896ea2b6958bb4f4ca7c197458612ed425c54bc2115130324` |
| (a) JO 2015 | TGH05 | 12 826 | 0 | `cb6ad2b7…` (identique à `tgh05.csv`) |
| (b) BO Santé 2007-1, annexe II | TGF05 | 12 826 | 0 | `a1f43cc1…` (identique à `tgf05.csv`) |
| (b) BO Santé 2007-1 | TGH05 | 12 826 | 0 | `cb6ad2b7…` (identique à `tgh05.csv`) |
| (c) JO 2006, comparaison à l'acte | TGF05 | 12 826 | 0 | `a1f43cc1…` (identique à `tgf05.csv`) |
| (c) JO 2006 | TGH05 | 12 826 | 0 | `cb6ad2b7…` (identique à `tgh05.csv`) |

### Liste complète des écarts : (a) JO 2015, TGF05 (`ecarts_jo2015_tgf05.csv`)

Forme : `<âge>,<génération>,<cellule du fichier de table>,<cellule du relevé>`. Les 11 écarts relèvent tous de l'anomalie **A2** (p. 38, ligne 117 du bloc 1955-1965). La couche texte place « 66 » et « 72 » dans la colonne 1955, décale ensuite chaque nombre d'une colonne, et laisse 1965 sans mot :

```
117,1955,66,?66/72
117,1956,72,79
117,1957,79,86
117,1958,86,94
117,1959,94,103
117,1960,103,112
117,1961,112,122
117,1962,122,133
117,1963,133,144
117,1964,144,156
117,1965,156,-
```

Constat sur l'image (rendu à 300 dpi, `img/jo2015_A2.png`) : le JO de 2015 imprime « 66 72 » dans la première cellule, puis 79, 86, 94, 103, 112, 122, 133, 144, 156, et une dernière cellule vide. C'est l'écart **attendu** A2.

Le relevé (a) de TGF05 n'est donc **pas conforme au format 2** sur cette ligne : la cellule `?66/72` porte deux nombres, et la cellule 1965 (`-`) crée un trou dans la colonne 1965 entre 118 et 116. Le format ne sait pas représenter une ligne mal composée (ADR 0019, point 5.3). Le marqueur est laissé tel quel pour ne rien fabriquer ; la forme à verser (marqueur, ou exclusion de la ligne avec déclaration) reste à décider.

**A1 n'apparaît pas comme écart** dans le relevé (a). La cellule (120, 1921), imprimée vide, est normalisée en `-`, comme le fichier de table. Elle est visible au journal : « tgf05 p.26 âge 120 : 10 mots pour 11 colonnes (1911-1921) ». Constat sur l'image (`img/jo2015_A1.png`) : dix « – », puis une dernière cellule vide.

### Écarts non attendus

**Aucun**, dans aucune source et aucune table. Aucune différence de mise en forme des lignes sans valeur n'a produit d'écart.

À titre de confirmation, les cellules A1 et A2 ont été rendues à 300 dpi dans les deux pièces de comparaison (`img/comparaison_A1_A2.png`, avec de haut en bas BO A1, BO A2, JO 2006 A1, JO 2006 A2) :
- **BO 2007-1** : la p. 16 imprime la ligne 120 du bloc 1911-1921 entièrement vide (forme ordinaire du BO). La p. 27 imprime la ligne 117 du bloc 1955-1965 en onze cellules bien alignées : 66, 72, 79, 86, 94, 103, 112, 122, 133, 144, 156 ;
- **JO 2006** : la p. 6 imprime la ligne 120 du bloc 1911-1921 avec onze « – », dont 1921. La p. 13 imprime la ligne 117 en onze cellules alignées, avec les mêmes nombres.

Les deux pièces de comparaison confirment donc la lecture M466 : la valeur de (120, 1921) est « – », et l'ordre imprimé de la ligne 117 correspond aux générations 1955 à 1965.

## 3. Difficultés d'extraction

1. **Titres courants et pieds de page** (JO : « 29 décembre 2015 … Texte 35 sur 159 » ; BO : « 10 sur 71 … 13/11/2026, 09:52 »). Leur premier mot est un nombre, lu d'abord comme un âge. Une ligne est écartée dès qu'elle contient un mot non numérique et autre qu'un tiret.
2. **BO, p. 36 et p. 65, âge 0** : les colonnes 1996 à 2005 sont serrées. La réunion des groupes de milliers sans borne fusionnait « 100 000 100 000 ». La réunion est désormais limitée à un seul groupe (les l_x imprimés valent au plus 100 000).
3. **BO** : les cellules sans valeur n'ont aucun mot. La colonne se déduit donc de l'abscisse, et non de l'ordre des mots. Le mode `-layout` ne suffit pas.
4. **JO 2015, A2** : la couche texte reproduit la cellule à deux nombres. Voir ci-dessus pour la non-conformité au format 2 du relevé (a) de TGF05.
5. **JO 2015, TGH05, p. 74 et 77** : 189 tirets codés U+002D au lieu de U+2013 dans la couche texte. Ils sont normalisés et ne produisent aucun écart.

## 4. Fichiers

- Scripts : `assembler.py`, `releves.py` ; `img/crop.py` (rendu et recadrage).
- Grilles assemblées : `tgf05.csv`, `tgh05.csv`.
- Relevés : `releve_{jo2015,bo2007,jo2006}_{tgf05,tgh05}.csv`.
- Écarts : `ecarts_{jo2015,bo2007,jo2006}_{tgf05,tgh05}.csv` (vides sauf `ecarts_jo2015_tgf05.csv`).
- Journaux : `journal_{jo2015,bo2007,jo2006}.txt`.
- Images : `img/jo2015_A1.png`, `img/jo2015_A2.png`, `img/bo_A1.png`, `img/bo_A2.png`, `img/jo2006_A1.png`, `img/jo2006_A2.png`, `img/comparaison_A1_A2.png`.
