#!/usr/bin/env python3
"""Assemblage des transcriptions par blocs de TGF05 et TGH05 (JO n° 301 du
29/12/2015, texte 35) en grilles au format 2 (ADR 0019, point 5.2).

Usage : python3 -I assembler.py <dossier_des_blocs> <dossier_de_sortie>

Script hors CI (ADR 0019, point 7.2). Déterministe : aucune horloge, aucun
aléa ; l'ordre des blocs se déduit de leurs générations.
Décisions du mainteneur appliquées (M466) :
  A1 : TGF05, cellule (120, 1921) imprimée vide (VIDE) -> écrite `-`.
  A2 : TGF05, bloc 1955-1965, ligne 117 : douze cellules transcrites, les
       onze nombres imprimés sont attribués dans l'ordre imprimé aux onze
       générations 1955 à 1965, chiffres inchangés ; le VIDE final disparaît.
Tout autre VIDE, ou toute autre ligne de longueur anormale, est un échec.
Les lignes entièrement en `-` ne sont pas transcrites (ADR 0019, point 5.2 ;
E2.26) ; elles sont listées sur la sortie standard.
"""
import hashlib
import os
import re
import sys

TABLES = ("tgf05", "tgh05")
AGES = range(0, 121)
CELLULE = re.compile(r"^(0|[1-9][0-9]{0,14}|-)$")
# Écarts déclarés : (table, première génération du bloc, âge)
A1 = ("tgf05", 1911, 120)
A2 = ("tgf05", 1955, 117)


def echec(msg):
    sys.stderr.write("ECHEC : " + msg + "\n")
    sys.exit(1)


def lire_bloc(chemin):
    with open(chemin, "rb") as f:
        brut = f.read()
    if not brut.endswith(b"\n") or b"\r" in brut:
        echec(f"{chemin} : fin de ligne")
    lignes = brut.decode("ascii").split("\n")[:-1]
    return [l.split(",") for l in lignes]


def assembler(dossier, table):
    fichiers = sorted(f for f in os.listdir(dossier)
                      if re.fullmatch(table + r"_\d{4}-\d{4}\.csv", f))
    if len(fichiers) != 10:
        echec(f"{table} : {len(fichiers)} blocs au lieu de 10")
    generations, colonnes = [], {a: [] for a in AGES}
    journal = []
    for nom in fichiers:
        g1, g2 = (int(x) for x in nom[len(table) + 1:-4].split("-"))
        lignes = lire_bloc(os.path.join(dossier, nom))
        entete = lignes[0]
        attendu = ["age"] + [str(g) for g in range(g1, g2 + 1)]
        if entete != attendu:
            echec(f"{nom} : en-tête {entete}")
        n = g2 - g1 + 1
        if [l[0] for l in lignes[1:]] != [str(a) for a in AGES]:
            echec(f"{nom} : âges")
        for l in lignes[1:]:
            age = int(l[0])
            cell = l[1:]
            cle = (table, g1, age)
            if cle == A2:
                if len(cell) != n + 1 or cell[-1] != "VIDE" or "VIDE" in cell[:-1]:
                    echec(f"{nom} : ligne {age} (A2) n'a pas la forme attendue")
                cell = cell[:-1]
                journal.append(f"A2 : {table} âge {age}, {n} nombres attribués "
                               f"dans l'ordre imprimé aux générations {g1} à {g2} ; VIDE final retiré")
            elif len(cell) != n:
                echec(f"{nom} : âge {age}, {len(cell)} cellules au lieu de {n}")
            for i, c in enumerate(cell):
                if c == "VIDE":
                    if cle == A1 and g1 + i == 1921:
                        cell[i] = "-"
                        journal.append(f"A1 : {table} cellule ({age}, {g1 + i}) VIDE écrite -")
                    else:
                        echec(f"{nom} : VIDE non déclaré, âge {age}, génération {g1 + i}")
                if not CELLULE.match(cell[i]):
                    echec(f"{nom} : cellule invalide {cell[i]!r}, âge {age}, génération {g1 + i}")
            colonnes[age].extend(cell)
        generations.extend(range(g1, g2 + 1))
    # Contrôle que A1 et A2 ont bien été rencontrés pour TGF05
    if table == "tgf05" and len(journal) != 2:
        echec(f"{table} : écarts A1/A2 non rencontrés ({journal})")
    if table == "tgh05" and journal:
        echec(f"{table} : écart inattendu {journal}")
    return generations, colonnes, journal


def controler(table, generations, grille):
    ages = [a for a, _ in grille]
    if generations != list(range(generations[0], generations[0] + len(generations))):
        echec(f"{table} : générations non consécutives")
    if ages != list(range(ages[0], ages[0] + len(ages))):
        echec(f"{table} : âges non consécutifs après retrait des lignes sans valeur "
              f"({ages[0]} à {ages[-1]}, {len(ages)} lignes)")
    for a, cell in grille:
        if len(cell) != len(generations):
            echec(f"{table} : âge {a}, longueur")
        if all(c == "-" for c in cell):
            echec(f"{table} : âge {a} sans valeur")
        for c in cell:
            if not CELLULE.match(c):
                echec(f"{table} : âge {a}, cellule {c!r}")
    for j, g in enumerate(generations):
        col = [(a, cell[j]) for a, cell in grille]
        val = [(a, int(c)) for a, c in col if c != "-"]
        if not val:
            echec(f"{table} : génération {g} sans valeur")
        for (a0, v0), (a1, v1) in zip(val, val[1:]):
            if v1 > v0:
                echec(f"{table} : génération {g}, hausse entre {a0} ({v0}) et {a1} ({v1})")
        premier, dernier = val[0][0], val[-1][0]
        trous = [a for a, c in col if premier < a < dernier and c == "-"]
        if trous:
            echec(f"{table} : génération {g}, '-' intercalé aux âges {trous}")


def main():
    if len(sys.argv) != 3:
        echec("usage : assembler.py <dossier_des_blocs> <dossier_de_sortie>")
    entree, sortie = sys.argv[1], sys.argv[2]
    for table in TABLES:
        generations, colonnes, journal = assembler(entree, table)
        for j in journal:
            print(j)
        grille, retirees = [], []
        for a in AGES:
            if all(c == "-" for c in colonnes[a]):
                retirees.append(a)
            else:
                grille.append((a, colonnes[a]))
        # Position des lignes retirées : en tête, en queue, ou au milieu
        gardes = [a for a, _ in grille]
        milieu = [a for a in retirees if gardes and gardes[0] < a < gardes[-1]]
        print(f"{table} : lignes sans valeur non transcrites, âges {retirees}"
              + (f" ; AU MILIEU de la grille : {milieu}" if milieu else " ; aucune au milieu"))
        controler(table, generations, grille)
        texte = "age," + ",".join(str(g) for g in generations) + "\n"
        texte += "".join(f"{a}," + ",".join(c) + "\n" for a, c in grille)
        donnees = texte.encode("ascii")
        chemin = os.path.join(sortie, table + ".csv")
        with open(chemin, "wb") as f:
            f.write(donnees)
        print(f"{table} : {len(generations)} générations ({generations[0]}-{generations[-1]}), "
              f"{len(grille)} âges ({grille[0][0]}-{grille[-1][0]}), "
              f"{sum(c != '-' for _, cell in grille for c in cell)} cellules à valeur, "
              f"{sum(c == '-' for _, cell in grille for c in cell)} cellules '-'")
        print(f"{hashlib.sha256(donnees).hexdigest()}  {table}.csv")


if __name__ == "__main__":
    main()
