# Second juge (#104, (3a)) : format(v, '.17g') de Python sur les doubles de x.bin
# (petit-boutiste, 8 octets), comparé ligne à ligne au texte du prototype ; décompte
# exact des égalités (développement décimal exact par decimal.Decimal : 18 chiffres
# significatifs, le dernier 5).
# Usage : python3 -I juge_python.py <x.bin> <texte du prototype>
import sys, array, decimal
x = array.array('d'); x.frombytes(open(sys.argv[1], 'rb').read())
if sys.byteorder != 'little': x.byteswap()
lignes = open(sys.argv[2], 'rb').read().decode('ascii').split('\n')
assert lignes[-1] == '' and len(lignes) - 1 == len(x), (len(lignes), len(x))
ecarts = 0; premiers = []; egalites = 0
for i, v in enumerate(x):
    t = format(v, '.17g')
    if t != lignes[i]:
        ecarts += 1
        if len(premiers) < 10: premiers.append((i + 1, repr(v), t, lignes[i]))
    if v != 0:
        s = decimal.Decimal(v).as_tuple().digits
        k = len(s)
        while k > 0 and s[k - 1] == 0: k -= 1
        if k == 18 and s[17] == 5: egalites += 1
print('python', sys.version.split()[0], '; doubles :', len(x), '; écarts :', ecarts, '; égalités exactes (decimal) :', egalites)
for p in premiers: print('  écart', p)
