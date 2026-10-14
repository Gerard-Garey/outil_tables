---
status: accepted
date: 2026-10-14
---

# Package `tablexp` modulaire à point d'entrée unique, en dérogation au « fichier unique » d'`outil_USP`

## Contexte

`outil_USP` impose un moteur dans un fichier unique, `R/engine.R`, « pour permettre une revue indépendante » (son `docs/exigences.md`, § 4.1, lu le 14/10/2026 par `gh api`), une fonction d'orchestration `run_engine()` qui retourne un objet complet (§ 4.3), une couche d'affichage sans calcul (§ 4.4) et un rapport figé construit sur l'objet retourné (§ 5.6). La règle y est tenable : une chaîne de calibration, un jeu de tests, un seul livrable.

`outil_tables` n'a pas cette forme : dix modules (Q23), une vingtaine de méthodes dès la v1 (Q30), un noyau multi-états configuré par risque (ADR 0003), trois livrables (Q11) plus le suivi d'expérience (Q26) et le simulateur (Q10), des contre-vérifications par méthode (ADR 0002). Un fichier unique de plusieurs milliers de lignes ne permet plus la revue indépendante, il l'empêche : un relecteur ne peut ni localiser une méthode, ni tester un module seul, ni constater qu'une modification de l'exposition ne touche pas la fermeture. Le mainteneur a écarté ce dispositif (Q8, point 2) et retenu un package modulaire (Q23).

## Décision

Arrêtée par le mainteneur le 14/10/2026 (Q5, Q8 points 1 et 2, Q23, Q37 ; A1 et consigne hors question pour la documentation) :

1. **Un package R, `tablexp`**, dix modules, un fichier par module : `donnees`, `exposition`, `brut`, `lissage`, `positionnement`, `prospective`, `fermeture`, `validation`, `simulation`, `rapport` (Q23). Noms de fonctions et d'arguments en français sans accents.
2. **Point d'entrée unique de la construction : `construire_table(donnees, roles, plan)`** (Q23). Il exécute le plan de construction d'un bloc (ADR 0005) et retourne un objet complet — table, étapes intermédiaires, diagnostics, fiche de construction, empreintes — dans lequel l'application, le rapport figé et les tests lisent **sans recalculer** (dispositif `run_engine()` d'`outil_USP`, § 4.3, repris). Le suivi d'expérience (Q26) et le simulateur (Q33) reçoivent chacun un point d'entrée de même forme (entrées explicites, objet complet en retour), nommé au plan de la première branche.
3. **Contrat d'entrée** (Q37) : le moteur reçoit un `data.frame` et des rôles (ADR 0003), **jamais un fichier**. La lecture (CSV par R de base, xlsx par `openxlsx` optionnel) et l'écriture des livrables (Q11) sont hors moteur : le moteur fournit l'objet et ses métadonnées, l'application les écrit. La fiche porte deux empreintes : celle du fichier lu, calculée par la couche qui l'a lu et transmise au moteur comme une entrée déclarée, et celle des données reçues, calculée par le moteur.
4. **Frontière calcul / affichage absolue** (Q8, point 1 ; `outil_USP` § 4.4 repris) : tout résultat calculable indépendamment de l'interface est dans `tablexp` ; tout contrôle de saisie à sens métier existe aussi dans `tablexp` ; aucune fonction du moteur ne dépend d'un objet Shiny.
5. **Application Shiny à part, sans calcul** (Q5, Q23) : `app.R` et `R/affichage.R` (formatage, tracés à partir de quantités déjà calculées), lancée localement depuis R. Sa disposition (Q36) est une **décision ouverte** : le mainteneur tranchera sur pièces ; le présent ADR ne fixe que la frontière et le bouton unique (ADR 0005).
6. **Ce qui remplace le fichier unique pour la revue indépendante** :
   - (a) le point d'entrée unique du point 2 : un relecteur part d'une fonction et descend ;
   - (b) la frontière du point 4, vérifiée par un script en CI : aucun calcul ni appel de fonction interne de `tablexp` dans `app.R` et `affichage.R`, aucun symbole Shiny dans le package ;
   - (c) le **test de concordance documentation ↔ code** (Q8, point 6 ; A1), dès la première branche : chaque fonction citée dans la documentation LaTeX existe dans le code, et chaque fonction exportée y est citée ;
   - (d) la **documentation LaTeX** (`docs/latex/`, un document, une partie par module, charte d'`outil_USP` ; consigne hors question, A1) avec un **diagramme du code** et, par module, un **tableau des fonctions** (`\code{}`) qui relie chaque méthode à son implémentation : l'index que le fichier unique donnait par simple lecture.

## Options écartées

- **Fichier unique `R/engine.R`** (Q8, point 2) : voir le contexte ; de plus, un seul fichier rend illisibles les commits par issue (diffs entremêlés) et interdit le test de concordance par module.
- **Un package par module**, ou **moteur et application dans deux dépôts** : surcoût d'outillage sans gain de revue ; la frontière est tenue par la structure du package et par script, pas par la séparation des dépôts.
- **Calculs dans l'application** (totaux, taux, empreintes « à l'affichage ») : interdits par Q8, point 1 ; un chiffre affiché qui ne vient pas de l'objet retourné n'est ni reproductible ni présent dans le rapport figé.
- **Moteur recevant un chemin de fichier** : lierait le moteur au format et au système de fichiers, empêcherait de tester sur un `data.frame` construit et de tracer deux empreintes distinctes (Q37).
- **Points d'entrée par étape exposés à l'application** (appeler le lissage puis la fermeture depuis Shiny) : disperse l'état dans l'interface ; c'est l'option (B) de Q22, écartée (ADR 0005).

## Conséquences

- Fichiers : `DESCRIPTION`, `NAMESPACE` (exports limités aux points d'entrée et aux fonctions que les tests et la documentation citent), `R/<module>.R` × 10, `app.R`, `R/affichage.R`, `tests/`, `docs/latex/` (document, `CONVENTIONS.md` adapté, diagramme, tableaux de fonctions), scripts de contrôle en CI (frontière, concordance) ; `CLAUDE.md`, « Architecture » (invariants : points 2 à 5) et domaines de commit (A2 : `moteur:`, `app:`, `tests:`, `docs:`, `donnees:`, `claude:`, `repo:`) ; `docs/exigences.md` : E4.1, E4.3, E4.4, E4.9, E3.1 à E3.3, § 5 (interface, ouverte).
- Proposition d'`architect`, à confirmer au plan de la première branche : un module ne dépend que des modules situés avant lui dans l'ordre du plan (`donnees` → `exposition` → `brut` → `lissage` / `positionnement` → `prospective` → `fermeture` → `validation` → `rapport` ; `simulation` à part, en amont des tests), contrôlé par script ; c'est ce qui rend une modification locale, et vérifiable comme telle par `audit`.
- Effet sur les résultats : aucun à ce jour.
- Ce que l'ADR ne règle pas : l'emplacement du package dans le dépôt (racine ou sous-dossier) et la place d'`affichage.R` vis-à-vis de l'espace de noms (plan de la première branche, sous la contrainte du point 4) ; les noms des points d'entrée du suivi d'expérience et du simulateur ; la disposition de l'interface (Q36, ouverte) ; le format de l'objet retourné.
- Contrats partagés, inscrits dans `docs/agents/routage.md`, § 4.2 : signature de `construire_table`, catalogue des rôles et recettes, format du plan et de la fiche de construction (ADR 0005), objet résultat lu par l'application et le rapport figé, surface de mortalité, format des tables de référence et de leurs provenances (ADR 0006), format des jeux simulés, critère de non-régression. Toute issue qui en touche un est sensible (§ 4.2).

Issues : aucune à ce jour.
