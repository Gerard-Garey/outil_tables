#!/usr/bin/env python3
"""Relevés de TGF05 et TGH05 par la couche texte de trois pièces, pour la
seule comparaison (M445, M462) : la valeur retenue reste celle des
transcriptions visuelles assemblées (assembler.py).

Usage : python3 -I releves.py <dossier docs/sources> <dossier des grilles assemblées> <dossier de sortie>

Outil : pdftotext (poppler), mode -bbox (mots et coordonnées) ; la ligne est
reconstruite par l'ordonnée des mots, la colonne par l'abscisse du centre du
mot, rapportée au centre de l'année d'en-tête du bloc (en-tête courant, repris
sur les pages qui ne le répètent pas). Les groupes de milliers (« 100 000 »)
sont réunis quand un mot d'un à trois chiffres est suivi d'un mot de trois
chiffres à moins de 4 pt (un seul groupe : les l_x imprimés sont au plus
100 000 ; sans cette borne, deux colonnes serrées du BO, p. 36 et 65, âge 0,
se réunissaient). Normalisation consignée : « – » (U+2013), « - »
et cellule sans mot -> `-`.

Sorties, par source s et table t :
  releve_<s>_<t>.csv  : grille au format 2 (ADR 0019, point 5.2), lignes sans
                        aucune valeur non écrites ;
  ecarts_<s>_<t>.csv  : `<âge>,<génération>,<cellule du fichier de table>,<cellule du relevé>`
                        (ADR 0019, point 7.2), un côté vide quand la ligne
                        n'existe que de l'autre ;
  journal_<s>.txt     : normalisations, anomalies de lecture.
"""
import html
import os
import re
import subprocess
import sys

SOURCES = {
    # nom : (fichier, {table: (première page, dernière page)})
    "jo2015": ("jo_2015-12-29_n301_texte35.pdf", {"tgf05": (19, 51), "tgh05": (51, 83)}),
    "bo2007": ("bo_sante_2007-1_annonce58_impression_2026-11-13.pdf", {"tgf05": (10, 39), "tgh05": (39, 68)}),
    "jo2006": ("jo_2006-08-26_n197_texte11.pdf", {"tgf05": (3, 20), "tgh05": (20, 38)}),
}
GENS = list(range(1900, 2006))
AGES = list(range(0, 121))
MOT = re.compile(r'<word xMin="([\d.]+)" yMin="([\d.]+)" xMax="([\d.]+)" yMax="([\d.]+)">(.*?)</word>')
# La pièce imprime-t-elle un tiret dans les cellules sans valeur ?  Sinon
# (BO), une cellule sans mot est la forme normale d'une cellule sans valeur.
TIRETS_IMPRIMES = {"jo2015": True, "bo2007": False, "jo2006": True}
TIRETS = {"–": "-", "-": "-", "−": "-", "—": "-"}


def mots_par_page(pdf, p1, p2):
    sortie = subprocess.run(["pdftotext", "-bbox", "-f", str(p1), "-l", str(p2), pdf, "-"],
                            check=True, capture_output=True).stdout.decode("utf-8")
    pages = sortie.split("<page ")[1:]
    for i, page in enumerate(pages):
        mots = [(float(a), float(b), float(c), float(d), html.unescape(t))
                for a, b, c, d, t in MOT.findall(page)]
        yield p1 + i, mots


def lignes(mots):
    """Regroupe les mots par ordonnée (tolérance 2 pt), triés par abscisse."""
    out = []
    for m in sorted(mots, key=lambda m: (m[1], m[0])):
        if out and abs(out[-1][0] - m[1]) < 2:
            out[-1][1].append(m)
        else:
            out.append([m[1], [m]])
    return [sorted(l, key=lambda m: m[0]) for _, l in out]


def jetons(mots, journal, ctx):
    """Réunit les groupes de milliers ; rend (texte, centre, xmin, xmax)."""
    res = []
    for m in mots:
        t = m[4]
        if (res and re.fullmatch(r"\d{3}", t) and re.fullmatch(r"\d{1,3}", res[-1][0])
                and 0 <= m[0] - res[-1][3] < 4):
            p = res[-1]
            res[-1] = (p[0] + " " + t, None, p[2], m[2])
        else:
            res.append((t, None, m[0], m[2]))
    return [(t, (a + b) / 2, a, b) for t, _, a, b in res]


def normaliser(t, stats):
    if t in TIRETS:
        stats["tiret " + repr(t)] = stats.get("tiret " + repr(t), 0) + 1
        return "-"
    if re.fullmatch(r"\d{1,3}( \d{3})*", t):
        v = t.replace(" ", "")
        if len(v) > 1 and v[0] == "0":
            raise ValueError(t)
        return v
    raise ValueError(t)


def relever(dossier, nom):
    fichier, tables = SOURCES[nom]
    pdf = os.path.join(dossier, fichier)
    journal, stats = [], {}
    grilles = {}
    for table, (p1, p2) in tables.items():
        grille = {}
        courante = None          # table en cours de lecture sur la page
        entete = None            # [(génération, centre)]
        age_xmax = None
        for page, mots in mots_par_page(pdf, p1, p2):
            for ligne in lignes(mots):
                textes = [m[4] for m in ligne]
                if "TABLE" in textes or "TGF05" in textes or "TGH05" in textes:
                    if "TGF05" in textes:
                        courante = "tgf05"
                    elif "TGH05" in textes:
                        courante = "tgh05"
                    entete = None
                    continue
                if textes[0].upper() in ("ÂGE", "AGE") and len(textes) > 1 and all(re.fullmatch(r"(19|20)\d\d", t) for t in textes[1:]):
                    entete = [(int(m[4]), (m[0] + m[2]) / 2) for m in ligne[1:]]
                    age_xmax = ligne[0][2] + 15
                    continue
                if courante != table or entete is None:
                    continue
                if not re.fullmatch(r"\d{1,3}", textes[0]) or ligne[0][2] > age_xmax:
                    continue
                if not all(re.fullmatch(r"\d+|[\u2013\u2212\u2014-]", t) for t in textes):
                    continue  # en-tête ou pied de page (date, « sur », titre courant)
                age = int(textes[0])
                if age > 120:
                    continue
                toks = jetons(ligne[1:], journal, (page, age))
                ecart_col = min(b[1] - a[1] for a, b in zip(entete, entete[1:])) if len(entete) > 1 else 40
                cellules = {}
                for t, c, _, _ in toks:
                    g, cg = min(entete, key=lambda e: abs(e[1] - c))
                    if abs(cg - c) > ecart_col / 2:
                        journal.append(f"{table} p.{page} âge {age} : mot {t!r} hors colonne (centre {c:.1f})")
                        continue
                    if g in cellules:
                        journal.append(f"{table} p.{page} âge {age} : deux mots dans la colonne {g} : "
                                       f"{cellules[g]!r} et {t!r} ; cellule écrite '?<mot1>/<mot2>' (hors format 2)")
                        cellules[g] = cellules[g] + "|" + t
                        continue
                    cellules[g] = t
                if TIRETS_IMPRIMES[nom] and len(toks) != len(entete):
                    journal.append(f"{table} p.{page} âge {age} : {len(toks)} mots pour {len(entete)} colonnes "
                                   f"({entete[0][0]}-{entete[-1][0]}) : " + " ; ".join(t for t, *_ in toks))
                for g, _ in entete:
                    brut = cellules.get(g)
                    if brut is None:
                        stats["cellule sans mot"] = stats.get("cellule sans mot", 0) + 1
                        v = "-"
                    elif "|" in brut:
                        v = "?" + brut.replace(" ", "").replace("|", "/")
                    else:
                        try:
                            v = normaliser(brut, stats)
                        except ValueError:
                            journal.append(f"{table} p.{page} âge {age} gén. {g} : mot illisible {brut!r}")
                            v = "?" + brut
                    if (age, g) in grille:
                        journal.append(f"{table} p.{page} âge {age} gén. {g} : cellule lue deux fois")
                    grille[(age, g)] = v
        manquantes = [(a, g) for a in AGES for g in GENS if (a, g) not in grille]
        if manquantes:
            journal.append(f"{table} : {len(manquantes)} cellules non lues, ex. {manquantes[:5]}")
        grilles[table] = grille
    for k in sorted(stats):
        journal.insert(0, f"normalisation : {k} -> '-' : {stats[k]}" if k.startswith(("tiret", "cellule"))
                       else f"{k} : {stats[k]}")
    return grilles, journal


def lire_grille(chemin):
    lignes_ = open(chemin, encoding="ascii").read().split("\n")[:-1]
    gens = [int(g) for g in lignes_[0].split(",")[1:]]
    return {int(l.split(",")[0]): dict(zip(gens, l.split(",")[1:])) for l in lignes_[1:]}


def main():
    sources, assemblees, sortie = sys.argv[1:4]
    version = subprocess.run(["pdftotext", "-v"], capture_output=True).stderr.decode().splitlines()[0]
    for nom in SOURCES:
        grilles, journal = relever(sources, nom)
        journal.insert(0, f"source : {SOURCES[nom][0]} ; outil : {version} -bbox")
        for table, grille in grilles.items():
            rows, vides = [], []
            for a in AGES:
                cel = [grille.get((a, g), "?") for g in GENS]
                if all(c == "-" for c in cel):
                    vides.append(a)
                else:
                    rows.append((a, cel))
            journal.append(f"{table} : lignes sans valeur non écrites au relevé : {vides}")
            with open(os.path.join(sortie, f"releve_{nom}_{table}.csv"), "w", encoding="ascii", newline="\n") as f:
                f.write("age," + ",".join(map(str, GENS)) + "\n")
                for a, cel in rows:
                    f.write(f"{a}," + ",".join(cel) + "\n")
            ref = lire_grille(os.path.join(assemblees, table + ".csv"))
            rel = {a: dict(zip(GENS, cel)) for a, cel in rows}
            ecarts, n = [], 0
            for a in AGES:
                ra, rb = ref.get(a), rel.get(a)
                if ra is None and rb is None:
                    continue
                for g in GENS:
                    ca = "" if ra is None else ra[g]
                    cb = "" if rb is None else rb[g]
                    n += 1
                    if ca != cb:
                        ecarts.append(f"{a},{g},{ca},{cb}")
            with open(os.path.join(sortie, f"ecarts_{nom}_{table}.csv"), "w", encoding="ascii", newline="\n") as f:
                f.write("".join(e + "\n" for e in ecarts))
            journal.append(f"{table} : {n} cellules comparées, {len(ecarts)} écarts")
            print(f"{nom} {table} : {n} cellules comparées, {len(ecarts)} écarts")
        with open(os.path.join(sortie, f"journal_{nom}.txt"), "w", encoding="utf-8", newline="\n") as f:
            f.write("".join(j + "\n" for j in journal))


if __name__ == "__main__":
    main()
