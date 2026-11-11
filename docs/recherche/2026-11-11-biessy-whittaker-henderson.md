# Biessy, « Whittaker-Henderson Smoothing Revisited » : lecture à la source pour Whittaker-Henderson (b)

> *Rédigé par la session de la branche documentaire `[D2]` `claude/lissage-documentaire` (vague lissage, M404) le 11 novembre 2026, tâche (a) du § 1.22 de la feuille de route de la porteuse. Lecture de la session ; non relue par `actuary`. Destinataire : la porteuse `claude/lissage-whittaker`, plan (0') de I-L2 (WH (b)) et de I-L3 (ddl effectifs).*

Statuts : **vérifié** (lu à la source, passage cité retrouvé, formule lue sur le rendu de la page quand le tableau le dit) ; **incertain** (lu sur le texte extrait seulement, ou lecture qui demande une interprétation) ; **non trouvé** (cherché dans la source, absent). Une **remarque de la session** est un raisonnement de la session à partir de la source, non une affirmation de la source : elle est marquée comme telle.

## 1. Identité de la source

| Élément | Valeur | Statut |
|---|---|---|
| Titre (`pdfinfo`, p. 1) | « Whittaker-Henderson Smoothing Revisited: A Modern Statistical Framework for Practical Use » | vérifié |
| Auteur | Guillaume Biessy (LinkPact ; Sorbonne Université, LPSM) | vérifié |
| Notice | arXiv:2306.06932 [stat.ME] ; v1 du 12/06/2023, v2 du 19/09/2023, v3 du 16/10/2024, v4 du 06/06/2025, **v5 du 17/09/2025** (page `https://arxiv.org/abs/2306.06932`) ; commentaire de la notice : « ASTIN Bulletin, In press » | vérifié (page de la notice, consultée le 11/11/2026) |
| Version lue | v5 : `https://arxiv.org/pdf/2306.06932v5`, identique octet pour octet à `https://arxiv.org/pdf/2306.06932` (`cmp`, 11/11/2026) | vérifié |
| SHA-256 | `1af94c962b3f6b9cc1bc571ccec0885630454cb52f14b6d43753b8b0ed0b1cdd` | vérifié (mesure du 11/11/2026) |
| `pdfinfo` | création 18/09/2025 00:36:44 UTC ; 64 pages ; date imprimée sous le titre : « September 3, 2025 » | vérifié |
| Référence publiée (DOI de l'ASTIN Bulletin) | non relevée : `api.crossref.org` refusé par le proxy de la session (11/11/2026) | non trouvé |
| Licence | « view license » sur la notice, non ouverte ; aucune copie versée au dépôt (citation par lien, E2.25) | non lu |

Pages lues : p. 1 à 23 (§ 1 à § 5), p. 34 à 37 (§ 7.1, § 7.2), p. 40 à 43 (§ 8), p. 46 à 48 (annexe A), p. 52 à 55 (annexes C et D), p. 62 à 64 (annexe G), sur le texte extrait (`pdftotext -layout`). Pages lues **sur le rendu** (`pdftoppm`, 80 ppp) : p. 4, 12, 13, 15, 19, 53, 54. Le § 6 (calcul en bandes, rang réduit), le § 7.3 (extrapolation contrainte en deux dimensions), les annexes B, E, F et la bibliographie n'ont pas été lus en détail : sans objet pour une table à une dimension de la v1.

## 2. Ce que dit la source

### 2.1 Whittaker-Henderson « original » (§ 1.1, § 2, § 3)

| Point | Contenu (page) | Statut |
|---|---|---|
| Critère | ŷ = argmin_θ { (y − θ)ᵀ W (y − θ) + θᵀ P_λ θ }, P_λ = λ D_{n,q}ᵀ D_{n,q}, D_{n,q} matrice des différences avant d'ordre q, (n − q) × n, D_{n,q} = D_{n−1,q−1} D_{n,1} (éq. (1), (2), p. 3-4) | vérifié (rendu, p. 4) |
| Solution | ŷ = (W + P_λ)⁻¹ W y si W + P_λ inversible ; en une dimension, λ ≠ 0 : inversible dès que w a q éléments non nuls (éq. (3), p. 5) | incertain (texte extrait) |
| Lecture bayésienne | y \| θ ~ N(θ, σ² W⁻), θ ~ N(0, σ² P_λ⁻) (loi a priori impropre) ; ŷ est le mode a posteriori ; θ \| y ~ N(θ̂, σ² (W + P_λ)⁻¹), exacte dans le cas normal (§ 2.1, § 2.2, p. 9) ; intervalle de crédibilité ŷ ± Φ⁻¹(1 − α/2) √(σ² diag{(W + P_λ)⁻¹}) (éq. (5), p. 10) | incertain (texte extrait) |
| Poids | w doit être proportionnel à l'inverse des variances, « not chosen empirically » (§ 2.3, p. 10) | incertain (texte extrait) |
| Vecteurs (y, w) en analyse de survie | Intensité constante par morceaux sur des intervalles d'un an (discrétisation attribuée à Hoem, 1971) ; log-vraisemblance ℓ = Σ_x [ln μ(x) d(x) − μ(x) e_c(x)], e_c **exposition centrale** (éq. (7), p. 11 ; annexe A, p. 46-48) ; θ̂ = ln(d/e_c) asymptotiquement N(θ, W⁻¹) avec **W = Diag(d)** ; d'où **y = ln(d/e_c), w = d**, σ² = 1 (§ 3.2, § 3.3, p. 11-12) | vérifié (rendu, p. 12) |

**Le WH « original » de Biessy porte sur le log du taux brut central, pondéré par les décès, non sur les q_x.** La variante (a) d'E2.12 (« critère quadratique classique sur les q_x, Lignes directrices, chap. 3 IV.2 ») n'est pas celle de la source : Biessy ne traite pas un lissage sur q_x (*non trouvé* dans les pages lues). Pour I-L1, la source de (a) reste les Lignes directrices ; Biessy n'en donne ni les poids ni l'échelle.

### 2.2 Whittaker-Henderson « généralisé » : la variante (b) d'E2.12 (§ 4.1)

| Point | Contenu (page) | Statut |
|---|---|---|
| Modèle | Vraisemblance de Poisson exacte ℓ(θ) = θᵀd − exp(θ)ᵀe_c, θ = ln μ, lien exponentiel (éq. (8), p. 11) ; loi a priori θ ~ N(0, P_λ⁻) ; **log-vraisemblance pénalisée ℓ_P(θ) = ℓ(θ) − θᵀ P_λ θ / 2** ; l'estimateur est son maximum (p. 12) | vérifié (rendu, p. 12) |
| Loi a posteriori approchée | θ \| d, e_c ≈ N(θ̂, (W_θ̂ + P_λ)⁻¹), W_θ̂ = Diag(exp(θ̂) ⊙ e_c) (éq. (10), p. 13) ; intervalles **asymptotiques** θ̂ ± Φ⁻¹(1 − α/2) √diag{(W_θ̂ + P_λ)⁻¹}, puis exponentiation des bornes pour μ (p. 13 ; p. 12 pour l'exponentiation) | vérifié (rendu, p. 13) |
| Algorithme | Newton : θ_{k+1} = (W_k + P_λ)⁻¹ W_k z_k, z_k = θ_k + W_k⁻¹ [d − exp(θ_k) ⊙ e_c], W_k = Diag(exp(θ_k) ⊙ e_c) ; c'est l'algorithme PIRLS ; départ θ_0 = ln(d/e_c), dont la première itération redonne le WH original (p. 13) | vérifié (rendu, p. 13) |
| Algorithme 1 (annexe D) | Départ θ_0 = ln(d*/e_c), **d* = max(d, ϵ)** ; z_k ← θ_k + d/w_k − 1 ; factorisation de Cholesky de W_k + P_λ, deux substitutions ; arrêt quand dev_{k+1} > (1 − ϵ_d) dev_k, ϵ_d = 10⁻⁸ (p. 54) | vérifié (rendu, p. 54) |
| Valeur de ϵ dans d* = max(d, ϵ) | non donnée : ϵ n'est pas ϵ_d, et aucune valeur n'est imprimée (p. 54) | non trouvé |
| Définition de la « déviance pénalisée » dev_P | nommée (p. 26 ; algorithme 1, p. 54), jamais écrite | non trouvé |
| Conservation des décès | le WH généralisé « preserves total event counts by construction, always yielding an SMR of exactly 100% » (p. 15) | vérifié (rendu, p. 15) |
| Biais du WH original sur petites bases | 6 jeux simulés (rentes 50-95 ans ; dépendance 30 × 15) ; écart relatif ∆(θ̂_norm) de l'original — part de la perte de log-vraisemblance pénalisée par rapport à l'ajustement polynomial de degré q − 1, ∆ = 0 au maximum pénalisé, ∆ = 1 au polynôme (éq. (11), p. 14), non une erreur sur les taux — : à 20 000 têtes, 1,91 % (rentes) et 93,27 % (dépendance) ; à 500 000 têtes, 0,00 % et 0,24 % ; SMR de l'original de 86,86 % à 99,99 % (tableau 2, p. 15) ; explication : corrélation positive entre décès observés et poids w = d, qui surpondère les taux bruts élevés (p. 15) | vérifié (rendu, p. 15) ; **constat de simulation** de l'auteur, non un résultat exact |
| Ordre de différence | q = 2 dans toutes les illustrations (p. 14) ; défaut recommandé, a priori « log-linéaire », cohérent avec Gompertz (§ 8, p. 40-41 ; annexe G, p. 62-63) ; q > 3 « harder to justify » ; le choix de q ne relève pas du REML, mais d'AIC ou de GCV (annexe G, p. 63) | incertain (texte extrait) |
| Surdispersion | σ² = 1 dans le cas de Poisson ; aucun paramètre de dispersion estimé (p. 12, p. 36) | non trouvé (traitement de la surdispersion) |
| Cellules sans exposition (e_c = 0) | traitées comme des observations de poids nul **dans l'extrapolation** seulement (§ 7.1, p. 35) ; rien sur une cellule interne sans exposition, ni sur z_k quand w_k = 0 | non trouvé |

*Remarque de la session (non tirée de la source)* : la conservation des décès suit de l'équation du score ∂ℓ_P/∂θ = d − exp(θ) ⊙ e_c − P_λ θ = 0, prémultipliée par le vecteur 1 : P_λ 1 = 0 dès que q ≥ 1, d'où Σ d = Σ exp(θ̂) e_c. Plus généralement, x^k appartenant au noyau de D_q pour k < q, Σ x^k d = Σ x^k μ̂ e_c pour k = 0, …, q − 1 (résultat exact, vérifié numériquement par `actuary` le 11/11/2026) ; la « first q moment preservation » de la source (p. 6, Whittaker, 1923) porte sur le WH original, c'est-à-dire sur les moments pondérés de y, non sur les décès. La tolérance du contrôle dépend du conditionnement de W + P_λ (écart de 10⁻² mesuré à λ = 10¹²). C'est une propriété testable à la précision du calcul, utile comme contrôle de I-L2.

### 2.3 Choix du paramètre de lissage (§ 5)

| Point | Contenu (page) | Statut |
|---|---|---|
| Critère retenu | maximum de la **vraisemblance marginale** (Patterson et Thompson, 1971, « under the name REML in the Gaussian case » ; Anderssen et Bloomfield, 1974) de préférence à AIC et GCV (§ 5.2, p. 16-18) ; illustration : GCV a deux minima locaux, le minimum global à environ 35 ddl (fig. 3, p. 18) | incertain (texte extrait) ; illustration = constat de simulation de l'auteur |
| Cas normal (WH original) | ℓ^m_norm(λ) = −½ [ (y − θ̂_λ)ᵀ W (y − θ̂_λ)/σ² + θ̂_λᵀ P_λ θ̂_λ/σ² + ln\|W + P_λ\| − ln\|P_λ\|₊ + C ], θ̂_λ = (W + P_λ)⁻¹ W y, C = − ln\|W\|₊ + (n* − q) ln(2πσ²), n* nombre de poids non nuls (§ 5.3, p. 18 ; annexe C, p. 52-53) | vérifié (rendu, p. 53) |
| Cas généralisé (WH (b)) | **LAML** (approximation de Laplace de la vraisemblance marginale) : ℓ^m_LAML(λ) = ℓ(θ̂_λ) − ½ [ θ̂_λᵀ P_λ θ̂_λ + ln\|W_λ + P_λ\| − ln\|P_λ\|₊ − q ln(2π) ], W_λ = Diag(exp(θ̂_λ) ⊙ e_c) (§ 5.4, p. 19 ; annexe C, p. 53) | vérifié (rendu, p. 19 et p. 53) |
| Notations | \|A\|₊ : produit des valeurs propres non nulles ; A⁻ : pseudo-inverse de Moore-Penrose (p. 3) | incertain (texte extrait) |
| Imbrication | trois stratégies : itération externe (PIRLS complet pour chaque λ, puis LAML ; « the most principled ») ; itération de performance (Gu, 1992) ; itération alternée (Wood et al., 2017) ; les deux dernières « lack formal convergence guarantees » (p. 19-20) ; algorithmes 2 à 4 (p. 55-56) | vérifié (rendu, p. 19) |
| Optimiseur | sur ln λ ; Brent (`optimize`) en une dimension, Nelder-Mead (`optim`) en deux, « readily available in base R » ; Fellner-Schall généralisé (éq. (12), p. 20) ; Newton (dérivées dans Wood, 2011) (§ 5.5, p. 20-21) | incertain (texte extrait) |
| Comparaison | 100 réplications ; itération externe : erreur relative sur la LAML < 10⁻¹⁰ (Newton), < 10⁻⁷ (Brent, Nelder-Mead), < 10⁻⁵ (Fellner-Schall) (§ 5.6, p. 22 ; fig. 4, p. 23) | incertain (texte extrait) ; constat de simulation de l'auteur |
| Arrêt de l'itération externe (algorithme 2) | lamlk ≥ (1 + ϵ_laml) lamlk−1, ϵ_laml = 10⁻⁸ ; λ initial « arbitrary » (p. 55) | incertain (texte extrait) |

**Le terme « REML » d'E2.12 et de la feuille de route n'est celui de la source que dans le cas normal.** Pour la vraisemblance de Poisson pénalisée, la source maximise la **LAML**, approximation de Laplace de la vraisemblance marginale ; le statut est celui d'une **approximation** (l'approximation de Laplace est exacte seulement dans le cas normal, p. 13). Proposition de terme, au § 4.

*Remarque de la session (non tirée de la source)* : la ligne de l'annexe C qui précède la LAML (p. 53) écrit L^m_ML(λ) ≈ exp(ℓ_P(θ̂_λ)) √((2π)^p / \|W_λ + P_λ\|), sans la constante de normalisation de la loi a priori √(\|P_λ\|₊ / (2π)^{p−q}) ; la LAML qui suit la contient (terme − ln\|P_λ\|₊ et − q ln(2π) dans le crochet). La LAML imprimée est cohérente avec la loi a priori de la p. 52 ; la ligne intermédiaire est incomplète. Même page : la loi a priori est écrite N(0, P_λ⁻¹) alors que P_λ n'est pas inversible (p. 12 et p. 52 : P_λ⁻). Sans effet sur λ̂ : les termes en cause ne dépendent pas de λ ou sont repris dans la formule finale.

### 2.4 Degrés de liberté effectifs (§ 5.1), pour I-L3

| Point | Contenu (page) | Statut |
|---|---|---|
| Définition | ddl effectifs (« edf ») = **trace de la matrice chapeau H = (W + P_λ)⁻¹ W** ; « non-parametric analog of the number of free parameters », valeurs non entières (p. 15) | vérifié (rendu, p. 15) |
| Cas généralisé | la source n'écrit pas la trace pour le WH généralisé ; par analogie (algorithme PIRLS, p. 13), W serait W_λ = Diag(exp(θ̂_λ) ⊙ e_c) à la convergence | incertain : lecture de la session, à confirmer par `actuary` |
| Ordres de grandeur | rentes, 100 000 têtes, 45 âges, q = 2 : 35,21 ddl (λ = 10), 6,71 (λ = 10⁴), 2,10 (λ = 10⁷) (fig. 1, p. 16) ; ddl au maximum de vraisemblance marginale, une réplication : 6,08 (fig. 3, p. 18) | incertain (texte extrait des figures) |
| Limite λ → ∞ | polynôme de degré q − 1 du maximum de vraisemblance (p. 14) ; tr(H) → q (*remarque de la session*) | incertain |
| Estimation de σ² (cas normal, σ² inconnu) | σ̂² = (y − ŷ)ᵀ W (y − ŷ) / (n − tr(H)), loi de Student à n − tr(H) ddl (p. 10) | incertain (texte extrait) |

*Remarque de la session* : n − tr(H) est la forme que prend une relation à ddl effectifs pour une table lissée (I-L3 : `RELATIONS_TABLE` porte des entiers, `R/validation.R:87`, *non relu* par la session) ; la source ne la relie pas à un test d'adéquation (χ² ou autre). Le nombre de degrés de liberté d'un test de validation d'une table lissée reste à spécifier par `actuary` (plan (0') de I-L3).

### 2.5 Extrapolation en une dimension (§ 7.1, § 7.2)

| Point | Contenu (page) | Statut |
|---|---|---|
| Principe | domaine étendu x⁺ d'entiers consécutifs ; positions non observées de poids nul ; λ **inchangé** (« inherited from the original fit ») ; pour le WH généralisé, z_k et W_k pris à la convergence de PIRLS et σ² = 1 (p. 34-36) | incertain (texte extrait) |
| Une dimension | l'ajustement sur les âges observés n'est pas modifié (V⁺₁₁ = V) ; extrapolation polynomiale de degré q − 1 (droite sur log μ pour q = 2), raccord lisse ; variance comprenant un terme de propagation et un terme d'innovation (P⁺₂₂)⁻¹ (p. 36-37) | incertain (texte extrait) |

*Remarque de la session* : une droite sur log μ au-delà du dernier âge observé est une fermeture implicite de type Gompertz. Elle n'est pas une méthode de fermeture d'E2.12 (Coale-Kisker, Denuit-Goderniaux, Kannisto ; M406) : à ne pas restituer comme telle sans décision.

### 2.6 Mise en œuvre de référence

| Point | Contenu | Statut |
|---|---|---|
| Package | « All these techniques are available in the WH package for the statistical software R » (p. 42) | vérifié (texte extrait) |
| CRAN | `WH` 2.0.0, publié le 19/06/2025, Imports `Rcpp`, `stats` ; LinkingTo `Rcpp` ; SystemRequirements LAPACK ; GPL (≥ 3) ; description : « based on the methods described in Biessy (2025) <doi:10.48550/arXiv.2306.06932> » ; « generalizes the original smoothing algorithm to maximum likelihood estimation, automatically selects the smoothing parameter(s) » (`https://cran.r-project.org/package=WH`, 11/11/2026) | vérifié (page CRAN) |
| Couverture des deux critères par `WH` (hypothèse d'`architect`, § 1.20 de la feuille de la porteuse) | la description CRAN nomme l'original et sa généralisation au maximum de vraisemblance ; les arguments des fonctions et le critère de sélection effectivement employé (LAML, itération externe ou de performance) n'ont pas été lus | incertain : à vérifier par `actuary` sur la documentation du package, hors CI |
| Compatibilité moteur | `WH` dépend de `Rcpp` (code compilé) : exclu du moteur (ADR 0002), admissible pour la contre-vérification hors CI (valeurs en dur) | vérifié (Imports de la page CRAN) |

## 3. Ce que la porteuse peut en tirer (plan (0') de I-L1, I-L2, I-L3)

Propositions de la session, à arbitrer par `actuary-approfondi` (spécification) :

1. **WH (b) se spécifie depuis le § 4.1 et l'annexe D** : ℓ_P(θ) = θᵀd − exp(θ)ᵀe_c − ½ θᵀ λ D_qᵀ D_q θ, e_c exposition centrale, θ = ln μ ; PIRLS (Newton) avec départ ln(d*/e_c). Deux points sont **à fixer par la spécification, faute de source** : la valeur de ϵ pour les âges sans décès (d* = max(d, ϵ)) et la règle pour une cellule interne sans exposition (refus, ou poids nul ; la source ne traite que l'extrapolation).
2. **Critère de sélection** : LAML, itération externe, optimiseur `stats::optimize` sur ln λ (Brent) en une dimension, intervalle de recherche déclaré. Tout est en R base (source, p. 20 : `optimize`, `optim`). Les itérations de performance et alternée sont sans garantie de convergence (p. 20) : à écarter pour la v1.
3. **Statuts** : θ̂ : approximation numérique (PIRLS, arrêt à ϵ_d) du maximum de la vraisemblance pénalisée, lui-même défini exactement ; intervalles de crédibilité de (b) **asymptotiques** (approximation de Laplace, p. 13) ; λ̂ approximation numérique d'un critère lui-même approché (LAML). Les intervalles de crédibilité sont bayésiens : leur restitution comme intervalles de confiance demande un avis (la source invoque la bonne couverture fréquentiste d'après Marra et Wood, 2012, p. 10, pour le cas normal).
4. **Contrôles testables** : Σ d = Σ μ̂ e_c (SMR = 1) à la précision de calcul pour (b) (§ 2.2) ; première itération de PIRLS égale au WH original sur (ln(d/e_c), d) ; λ → ∞ : polynôme de degré q − 1 de maximum de vraisemblance ; contre-vérification par `WH` hors CI.
5. **I-L3** : ddl effectifs tr((W_λ + P_λ)⁻¹ W_λ), non entiers ; la relation de validation et le nombre de ddl d'un test d'adéquation restent à spécifier (§ 2.4).
6. **WH (a)** (I-L1) : la source n'en est pas une. Les λ de (a) (sur q_x, Lignes directrices) et de (b) (sur log μ) n'ont pas la même échelle : un λ n'est pas transposable de l'une à l'autre.

## 4. Termes et décisions proposés (au corps de la PR `[D2]`, à consigner par la porteuse)

- **Terme** : « LAML » (approximation de Laplace de la vraisemblance marginale) pour le critère de choix de λ de WH (b) ; « REML » réservé au cas normal. E2.12 écrit « paramètre de lissage choisi par REML » : proposition de lecture, sans changement de l'exigence tant que le mainteneur n'a pas tranché, « REML (au sens de la vraisemblance marginale ; pour la vraisemblance de Poisson, son approximation de Laplace, LAML, Biessy, 2025, § 5.4) ».
- **Terme** : « WH généralisé » (Biessy, § 4.1) désigne la variante (b) ; « WH original » (Biessy) désigne le lissage de ln(d/e_c) pondéré par d, **distinct** de la variante (a) d'E2.12 (q_x, Lignes directrices).
- **Point de décision** (spécification de I-L2) : traitement des âges sans décès (ϵ de départ) et des cellules internes sans exposition, non traités par la source.

*Note du 11/11/2026* : décisions prises après la rédaction — terme LAML retenu, E2.12 corrigée (M413, D5) ; validation d'une table lissée : statistiques d'ajustement à n − Σν degrés de liberté, statut d'approximation non établie (M413, D7) ; la proposition de lecture « REML (…) » ci-dessus et les mentions « à confirmer par `actuary` » des § 2.4 et § 3 sont sans objet. `R/validation.R:87` relu par `actuary` à `8ba2db4` : exact.

## 5. Ce qui n'a pas été fait

- DOI de l'ASTIN Bulletin non relevé (Crossref refusé par le proxy).
- Documentation et code du package `WH` non lus.
- § 6, § 7.3, annexes B, E, F non lus en détail ; références citées (Wood, 2011 ; Patterson et Thompson, 1971 ; Verrall, 1993 ; Marra et Wood, 2012) non lues.
