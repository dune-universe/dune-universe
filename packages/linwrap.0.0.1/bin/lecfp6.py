#!/usr/bin/env python3

# Copyright (C) 2019, Francois Berenger
#
# Yamanishi laboratory,
# Department of Bioscience and Bioinformatics,
# Faculty of Computer Science and Systems Engineering,
# Kyushu Institute of Technology,
# 680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan.

# 16384 bits ECFP6 encoder: a good fingerprint according to
# O’Boyle, N. M., & Sayle, R. A. (2016).
# "Comparing structural fingerprints using a literature-based
# similarity benchmark."
# Journal of cheminformatics, 8(1), 36.

import sys, rdkit, time
from rdkit import Chem
from rdkit.Chem import AllChem

def RobustSmilesMolSupplier(filename):
    with open(filename) as f:
        for line in f:
            words = line.split()
            smile = words[0]
            name = " ".join(words[1:]) # everything after the SMILES
            yield (Chem.MolFromSmiles(smile), name)

if __name__ == '__main__':
    before = time.time()
    argc = len(sys.argv)
    if argc != 2:
        print("usage: %s input.smi > output.lecfp6" % sys.argv[0])
        sys.exit(1)
    input = sys.argv[1]
    ok_count = 0
    ko_count = 0
    for mol, name in RobustSmilesMolSupplier(input):
        if mol:
            ok_count += 1
            if (ok_count % 1000) == 0:
                # user feedback
                print("done: %d" % ok_count, end='\r',
                      file=sys.stderr, flush=True)
            fp = AllChem.GetMorganFingerprintAsBitVect(mol, 3, nBits=16384)
            # this format can be read by molenc's pubchem_decoder
            print("%s,0.0,%s" % (name, fp.ToBitString()))
        else:
            ko_count += 1
    after = time.time()
    dt = after - before
    total = ok_count + ko_count
    print("%d molecules (%d errors) at %.2f mol/s" %
          (total, ko_count, total / dt), file=sys.stderr)
