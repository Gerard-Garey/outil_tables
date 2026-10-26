# Mesures de l'étape (3a) de #104 : écriture `%.17g` correctement arrondie

Ce dossier contient le prototype et les scripts de mesure de l'essai de l'option (b) de #104 (M148). Ils sont conservés dans le dépôt par décision du mainteneur du 26/10/2026 (question 4 du rapport). Ils sont hors des batteries et n'ont aucun seuil.

## Provenance

- **Rapport de mesure** : https://github.com/Gerard-Garey/outil_tables/pull/116#issuecomment-6411554386. Le prototype y figure en annexe.
- **Scripts** : https://github.com/Gerard-Garey/outil_tables/pull/116#issuecomment-6411554759. Ils sont recopiés tels quels.
- **Prototype** : `ecriture_17g.R`, MD5 `6f63d371d2e73638d6d7f14bc7417217`, mesuré à `abda2d8`. La forme intégrée au moteur (`R/rapport.R`) en dérive.
- **Spécification** : https://github.com/Gerard-Garey/outil_tables/issues/104#issuecomment-6410091378.

Cinq fichiers cités par le rapport n'ont pas été versés dans la PR et manquent ici : `appels_r4.R`, `types_operandes.R`, `artefacts_installes.R`, `essai_rapide.R` et `integrer.py`.

## Ordre d'exécution

Chaque script prend ses chemins en arguments. Il faut fournir :
- une bibliothèque où `tablexp` est installé à l'état mesuré ;
- un dossier de sorties.

Étapes :

1. `Rscript doubles_ecrits.R <racine> <bibliothèque> <sorties>` : doubles écrits par R1 à R6.
2. `Rscript echantillon.R <sorties>` : échantillon de l'oracle (graine `20261026`), écrit dans `echantillon.rds`.
3. `Rscript s0_echantillon.R ecriture_17g.R <sorties>` puis `python3 -I juge_python.py <sorties>/x.bin <sorties>/texte_proto.txt` : seuil S0, contre la glibc puis contre Python.
4. `Rscript s0_artefacts.R`, `couts.R` et `memoire.R` : artefacts R1 à R6 et seuils S1 à S7. Leurs arguments sont indiqués dans l'en-tête de chaque script.

`substitution.R` est sourcé par les autres scripts. Il ne s'exécute pas seul.
