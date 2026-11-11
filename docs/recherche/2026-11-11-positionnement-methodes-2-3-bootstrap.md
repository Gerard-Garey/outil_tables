# Positionnement, méthodes 2 et 3 (note II1291-12 v1.7) ; source v1 du bootstrap

> *Rédigé par la session de la branche documentaire `[D2]` `claude/lissage-documentaire` (vague lissage, M404) le 11 novembre 2026, tâche (e) du § 1.22 de la feuille de route de la porteuse. Lecture de la session ; non relue par `actuary`. Destinataire : le plan de `claude/positionnement-brass-glm` (M371) et celui de `claude/bootstrap` (M351, M368).*

Statuts : **vérifié** (lu à la source, sur le rendu de la page quand le tableau le dit) ; **incertain** ; **non trouvé** ; **non lu**. Une **remarque de la session** est un raisonnement de la session, non une affirmation de la source. Les notes II1291 sont une **référence de place** (M80), non un texte qui fait foi.

## 1. Note II1291-12 v1.7 (méthodes 2 et 3)

### 1.1 Identité

| Élément | Valeur | Statut |
|---|---|---|
| Fichier | `https://www.ressources-actuarielles.net/EXT/ISFA/fp-isfa.nsf/0/7ad72b268cdaa41dc1257b720042873e/$FILE/II1291-12_v1.7.pdf`, téléchargé le 11/11/2026 | vérifié |
| SHA-256 | `93537ddb13bf3c989f186b3d7676976e8759d5b6ca106910c8ab66cd0e4e6dce`, identique à l'empreinte consignée (`docs/recherche/2026-11-05-sources-v1.md`, § 2.1) | vérifié |
| `pdfinfo` | création 25/05/2014 ; 14 pages (pagination interne « n/13 », page PDF = page interne + 1) | vérifié |
| Pages lues sur le rendu (`pdftoppm`, 110 ppp) | p. 5/13 (§ 3.2, méthode 2), p. 6/13 et 7/13 (§ 3.3, méthode 3) | vérifié |
| Pages lues sur le texte extrait | p. 1/13 à 8/13 ; bibliographie | incertain hors des pages ci-dessus |

### 1.2 Méthode 2 (Brass), § 3.2, p. 5/13

| Point | Citation ou formule (p. 5/13, sur le rendu) | Statut |
|---|---|---|
| Modèle | « logit q̃_{x*}(t*) = α + β logit q^{réf}_{x*}(t*) + ε_{x*,t*} », x* et t* « la plage d'âges et période d'observation communes avec la référence de mortalité », ε le terme d'erreur ; « Les logits des probabilités conditionnelles de décès sont ajustées sur les logits des probabilités de décès de la table de référence. » | vérifié |
| Critère d'estimation | « L'estimation est effectuée par une minimisation de la distance pondérée par l'exposition entre les probabilités de décès observées q̂_{x*}(t*) et estimées q̃_{x*}(t*), Σ \|E_{x*,t*} × (q̂_{x*}(t*) − q̃_{x*}(t*))\| » : critère **L1 pondéré par l'exposition**, sur l'échelle des **probabilités** (non des logits) | vérifié |
| Sens de E | non précisé (initiale ou centrale) | non trouvé |
| Prévision | « q̃_x(t) = exp(α̂ + β̂ logit q^{réf}_x(t)) / (1 + exp(α̂ + β̂ logit q^{réf}_x(t))) », pour x ∈ [30, 130] et t ∈ [2012, 2060] ; la phrase qui précède écrit « q̂_x(t) » là où la formule écrit q̃_x(t) | vérifié (coquille de notation constatée sur le rendu) |
| Fermeture | « Nous appliquons la procédure de Denuit and Goderniaux (2005), cf. section 2.5 de la note II1291-11 v1.4. » ; q̃ conservés sous 85 ans, remplacés au-delà (p. 5/13 et 6/13) | vérifié |
| Mise en garde | « Les modèles utilisant les logits des données de mortalité doivent être utilisés avec prudence dans le cas du risque décès puisqu'ils sous-estiment les taux de mortalité, parfois avec des proportions importantes. Cependant, ils entrainent une marge de sécurité dans le cas du risque de survie. » (argument de Jensen, renvoi à Planchet et Thérond, 2011, chap. 7) | vérifié |

Ce relevé confirme celui de `docs/recherche/2026-11-05-sources-v1.md`, § 2.1 (« critère L1 pondéré Σ\|E (q̂ − q̃)\| »). Il ne tranche pas l'estimateur que retiendra l'outil (M371 : penchant d'`actuary` pour le maximum de vraisemblance de Poisson, écart documenté au sens de M80) ; il établit seulement que l'estimateur de la note est le critère L1 sur les probabilités.

### 1.3 Méthode 3 (GLM de Poisson avec référence), § 3.3, p. 6/13 et 7/13 : le membre de gauche « μ »

| Point | Citation ou formule (sur le rendu) | Statut |
|---|---|---|
| Modèle | « D_{x*,t*} ∼ P(E_{x*,t*} μ_{x*}(t*)), avec μ_{x*}(t*) = β_0 + β_1 log q^{réf}_{x*}(t*) + β_2 x* + β_3 t* + β_4 x* t* » ; « q^{réf}_x(t) sont les probabilités de décès de la référence » (p. 6/13) | **vérifié** : le membre de gauche imprimé est **μ**, non log μ |
| Fonction de lien | aucune mention (recherche des mots « lien », « link », « log μ », « offset » dans le texte extrait : aucune occurrence) | non trouvé |
| Prévision | « q̃_x(t) = exp(β̂_0 + β̂_1 log q^{réf}_x(t) + β̂_2 x + β̂_3 t + β̂_4 x t) », pour x ∈ [30, 130] et t ∈ [2012, 2060] (p. 7/13) | vérifié |
| Interactions | « les interactions avec l'année calendaire ne peuvent raisonnablement être prises en compte que si l'on dispose d'un historique commun avec la référence supérieur ou égal à 10 ans » ; illustration avec 3 années communes, donc β_3 et β_4 non estimés (tableaux 3 et 4 : β_0, β_1, β_2) (p. 7/13) | vérifié |
| Fermeture | Denuit et Goderniaux (2005), q̃ conservés sous 85 ans (p. 7/13) | vérifié |
| Source citée | Tomas, J. and Planchet, F. (2013). « Prospective mortality tables : taking heterogeneity into account ». Cahiers de Recherche de l'ISFA, 2013(5), 1–33 (bibliographie de la note) | référence relevée sur le texte extrait ; contenu **non lu** |

*Remarque de la session (non tirée de la source)* :
- La formule du modèle (p. 6/13) et celle de la prévision (p. 7/13) ne sont cohérentes que si le prédicteur linéaire est **log μ** (lien logarithmique, lien canonique du GLM de Poisson) : la prévision applique exp au prédicteur. Avec le membre de gauche imprimé (μ = prédicteur, lien identité), la prévision devrait être le prédicteur lui-même. La lecture « log μ » est la seule qui rende les deux formules cohérentes, mais elle n'est pas imprimée.
- La prévision identifie en outre q̃_x(t) à μ_x(t) (q = exp(prédicteur) au lieu de q = 1 − exp(−μ) sous la convention de force constante du moteur, `R/brut.R:122`, cité par l'avis M369). C'est un second écart d'écriture, distinct du premier.
- Pour le plan de `claude/positionnement-brass-glm` : la méthode 3 se spécifie sous sa forme statistique usuelle (log μ = prédicteur, log E en décalage), écart à l'écriture de la note documenté au sens de M80 et M192 ; la relation entre q et μ suit la convention du moteur. À arbitrer par `actuary` ; la source de la forme (Tomas et Planchet, 2013) reste à lire.

## 2. Source v1 du bootstrap par têtes (E2.14 ; M351 : « non trouvée »)

Objet cherché : une source de méthode pour le **rééchantillonnage par têtes** (E2.14), sous troncature à gauche et censure à droite, appliqué à une table lissée ou positionnée.

| Piste | Ce qui a été fait le 11/11/2026 | Statut |
|---|---|---|
| Efron, B. (1981). « Censored data and the bootstrap ». *JASA* 76, 312-319, DOI 10.1080/01621459.1981.10477650 | notice relevée par une recherche web (rééchantillonnage des couples (durée, indicateur de censure) avec remise) ; texte non accessible librement (JSTOR, éditeur) | notice **incertaine** (non contrôlée sur Crossref : `api.crossref.org` refusé par le proxy) ; contenu **non lu** |
| Gross, S. T. et Lai, T. L. (1996), « Bootstrap methods for truncated and censored data », *Statistica Sinica* 6 (troncature à gauche et censure à droite, cadre du projet) | page de la revue (`www3.stat.sinica.edu.tw`) **refusée par le proxy** de la session (HTTP 403 au CONNECT) | auteurs, titre et volume **incertains** (souvenir de la session, non contrôlé) ; contenu **non lu** |
| Portnoy (1987), « Bootstrapping a Graduation », volume d'actuariat de l'université de Western Ontario | titre seul, par recherche web | **non lu** |
| Notes II1291 et Lignes directrices | aucun passage sur le bootstrap dans les pages déjà lues (`docs/recherche/2026-11-05-sources-v1.md`) ; pas de recherche par mot dans cette session | non trouvé dans les pages lues |
| Bootstraps de modèles prospectifs (Lee-Carter : Brouhns, Denuit et Van Keilegom, 2005 ; Hardy et Tan, 2009) | hors objet de la v1 (E2.13) | non retenu |

Conclusion : **la source v1 du bootstrap reste non trouvée** au sens où aucune source n'a été lue à la source dans cette session. Deux candidats sont identifiés, dont un (Gross et Lai, 1996) couvre exactement le cadre de l'outil, mais sur un hôte refusé par le proxy. Demande portée à la session principale : session « Accès complet » ou ouverture de `www3.stat.sinica.edu.tw` (et de `api.crossref.org` pour les notices), avant le plan de `claude/bootstrap` (rang 17).
