# Relevé seconde source (couche texte) : décalages d'âge TF 00-02 et TH 00-02

Pièce : `docs/sources/bo_sante_2007-1_annonce58_impression_2026-11-13.pdf` (BO Santé n° 2007-1, annonce n° 58, arrêté du 8/12/2006), annexe I.
Extraction : `pdftotext -layout -f 9 -l 10` (sortie brute : `couche_texte_p9-10.txt`) ; contrôle visuel : `pdftoppm -f 9 -l 10 -r 200 -png` (rendus `img/p-09.png`, `img/p-10.png`).
Statut : seconde source, pour comparaison seule (M445) ; relevé fidèle, rien n'est corrigé.

## Titres et pages

- « Décalage d'âge - TF 00-02 » (apostrophe typographique U+2019) : titre et en-tête en p. 9 (impression « 9 sur 71 ») ; le tableau continue en p. 10 (ligne « 94 et plus » seule en haut de la p. 10, sans en-tête répété).
- « Décalage d'âge - TH 00-02 » : entièrement en p. 10 (« 10 sur 71 »).
- En-têtes de colonnes : « ÂGE » (Â U+00C2) et « DÉCALAGE » (É U+00C9).
- Le tableau TF est précédé, en p. 9, de la fin d'une colonne d'effectifs (âges 83 à 112) qui n'est pas relevée ; le tableau TH est suivi de « ANNEXE II / TABLE TGF05 » (non relevé).

## Glyphes

- Signe des décalages : trait d'union ASCII U+002D dans la couche texte (aucun signe moins U+2212 ni tiret long) ; le rendu affiche un trait court suivi d'une espace (« - 8 »).
- Espacement : la couche texte écrit « - 11 » (TF, première ligne) et « - 13 » (TH, première ligne) avec espace, mais « -8 », « -6 »… sans espace pour les autres lignes ; le rendu montre une espace partout. Les espaces sont retirées dans le CSV, conformément à la consigne : sans effet sur les valeurs.
- Bornes entre crochets « [a - b] » ; la première borne basse s'imprime « [ 0 - 32] » / « [ 0 - 38] » (espace avant le 0, alignement), relevée `0`.

## Particularités

- TF 00-02 : la ligne « [33 - 34] » avec son décalage est imprimée **deux fois** consécutivement (rendu et couche texte concordent) ; elle figure deux fois dans le CSV.
- TF 00-02 : dernière ligne « 94 et plus » avec décalage imprimé « - 0 », relevé `-0` ; bande ouverte, `age_max` vide.
- TH 00-02 : dernière ligne « 75 et plus », bande ouverte, `age_max` vide.
- Les bornes basses valent 0 telles qu'imprimées.

## Divergences couche texte / rendu

- Aucune divergence de valeur, de borne, d'ordre ni de nombre de lignes entre la couche texte et le rendu image pour les deux tableaux.
- Seules différences : espacement entre le signe et le chiffre (voir Glyphes) et la colonne d'alignement dans la sortie `-layout`, sans portée sur le relevé.
