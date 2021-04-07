#!/usr/bin/env python3

# Copyright (C) 2019, Francois Berenger
#
# Yamanishi laboratory,
# Department of Bioscience and Bioinformatics,
# Faculty of Computer Science and Systems Engineering,
# Kyushu Institute of Technology,
# 680-4 Kawazu, Iizuka, Fukuoka, 820-8502, Japan.

import multiprocessing, sys, rdkit, time
from rdkit import Chem
from rdkit.Chem import AllChem
from multiprocessing import Pool
from optparse import OptionParser

# CLI setup -------------------------------------------------------------------
parser = OptionParser(usage = """Usage: %prog -i in.smi \
[-b nb_bits (def=2048)] [-r radius (def=3)] [--sparse] > out.ecfp6""")
parser.add_option("-i", action = "store", type = "string", dest = "in_fn")
parser.add_option("-b", action = "store", type = "int", dest = "nb_bits",
                  default = 2048) # rdkit's default
parser.add_option("-n", action = "store", type = "int", dest = "nprocs",
                  default = 1)
parser.add_option("-c", action = "store", type = "int", dest = "csize",
                  default = 1000)
parser.add_option("-r", action = "store", type = "int", dest = "radius",
                  default = 3) # ECFP6
# output in sparse format
parser.add_option("--sparse", action = "store_true", dest = "sparse_fmt")

def name2label(name):
    res = "-1"
    if name.startswith("active"):
        res = "+1"
    return res

def sparse_string(bit_string):
    res = ""
    j = 1 # liblinear feature indexes start at 1
    for c in bit_string:
        if c == '1':
            res += " %d:1" % j
        j += 1
    return res

def decode_mol(name, bit_string):
    return name2label(name) + sparse_string(bit_string)

# read a bunch of molecules at once
def read_several(count, opened_file):
    res = []
    for i in range(count):
        line = opened_file.readline()
        if line == '':
            break
        words = line.split()
        smile = words[0]
        mol = Chem.MolFromSmiles(smile)
        if mol:
            name = " ".join(words[1:]) # everything after the SMILES
            res.append((mol, name))
    return res

# main ------------------------------------------------------------------------
if __name__ == '__main__':
    before = time.time()
    (opts, args) = parser.parse_args()
    if len(sys.argv) == 1:
        parser.print_help()
        exit(0)
    input_fn = opts.in_fn
    nb_bits = opts.nb_bits
    nprocs = opts.nprocs
    csize = opts.csize
    assert(nb_bits == 1024 or nb_bits == 2048 or nb_bits == 16384)
    fp_radius = opts.radius
    assert(fp_radius == 2 or fp_radius == 3)
    sparse_output = opts.sparse_fmt
    ok_count = 0
    ko_count = 0
    opened_file = open(input_fn, 'r')

    # WARNING: this function is defined here in order to use some
    # "global" variables, because there is no partial function application
    # in Python...
    def process_one(x):
        mol, name = x # unpack params
        # ECFP fingerprint
        fp = AllChem.GetMorganFingerprintAsBitVect(mol, fp_radius,
                                                   nBits = nb_bits)
        bits = fp.ToBitString()
        if sparse_output:
            # liblinear-ready format
            return(decode_mol(name, bits))
        else:
            # format for molenc's pubchem_decoder
            return("%s,0.0,%s" % (name, bits))

    workers_pool = Pool(nprocs) # AFTER definition of process_one
    while True:
        # unfold
        mol_bunch = read_several(csize, opened_file)
        nb_read = len(mol_bunch)
        if nb_read == 0:
            break # got EOF
        # parallel map
        to_print = workers_pool.map(process_one, mol_bunch)
        # fold
        for x in to_print:
            print(x)
        ok_count += nb_read
        # user feedback
        print("done: %d" % ok_count, end='\r', file=sys.stderr, flush=True)

    workers_pool.close()
    opened_file.close()
    after = time.time()
    dt = after - before
    total = ok_count + ko_count
    print("%d molecules (%d errors) at %.2f mol/s" %
          (total, ko_count, total / dt), file=sys.stderr)
