---
status: proposed | accepted | superseded by NNNN
date: AAAA-MM-JJ
---

# <Titre : la décision en une phrase>

Gabarit des ADR : copier sous `docs/adr/NNNN-titre-court.md`, numéroté à la suite. Un ADR accepté ne se réécrit pas : on l'annote (section datée en fin de fichier) ou on le remplace par un nouveau ADR qui le cite.

**Renvois au code** (convention du 20/10/2026, décision M77, (b)). Un renvoi `fichier:ligne` vieillit au premier commit qui déplace la ligne, et un ADR accepté ne se réécrit pas. D'où deux règles :
- toute section (corps de l'ADR ou annotation) qui cite des renvois `fichier:ligne` nomme le **SHA de rédaction** auquel ils ont été relevés (« numéros de ligne à `abc1234` ») ;
- chaque renvoi nomme aussi l'**objet** (fonction, constante, champ) : **le nom de l'objet fait foi**, le numéro n'est qu'un repère au SHA de rédaction.

Conséquence : un numéro de ligne devenu périmé ne demande plus, à lui seul, d'annotation corrective. Une annotation reste due quand le constat change (objet renommé, retiré, ou dont le comportement cité a changé).

Les renvois des ADR antérieurs à cette convention se lisent au SHA de rédaction de leur section, quand elle le nomme (ADR 0011, neuvième annotation du 20/10/2026).

## Contexte

Le problème, mesuré quand c'est possible (commits, issues, coûts constatés) ; ce qui rend la décision nécessaire maintenant.

## Décision

Ce qui est décidé, par qui et quand (« arrêtée par le mainteneur le … »), en points numérotés citables.

## Options écartées

Chaque option sérieuse, et la raison de l'écarter.

## Conséquences

Ce qui change (fichiers, fiches d'agents, `CLAUDE.md`), effet sur les résultats (aucun, ou lesquels), ce qui reste à faire, ce que l'ADR ne règle pas.

Issues : #…
