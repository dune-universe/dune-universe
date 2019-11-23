#!/usr/bin/python3

# get the ECFP4 bitstring for each molecule in input file; using rdkit's python bindings

import sys

import rdkit
from rdkit import Chem
from rdkit.Chem import AllChem

def RobustSmilesMolSupplier(filename):
    with open(filename) as f:
        for line in f:
            split = line.split()
            smile = split[0]
            mol_name = split[1]
            mol = Chem.MolFromSmiles(smile)
            yield mol, mol_name

def SdfMolSupplier(filename):
    suppl = Chem.SDMolSupplier(filename)
    for mol in suppl:
        if mol is None:
            print("erroneous molecule",
                  file=sys.stderr)
        else:
            mol_name = mol.GetProp("_Name") # uses index in case name is absent
            yield mol, mol_name

def get_mol_reader(filename):
    if filename.endswith(".smi"):
        return RobustSmilesMolSupplier(filename)
    elif filename.endswith(".sdf"):
        return SdfMolSupplier(filename)
    else:
        print("get_mol_reader: fatal: unsupported file format: %s" % filename,
              file=sys.stderr)
        exit(1)

def string_of_bitstring(sparse, fp):
    bitstring = fp.ToBitString()
    if not sparse:
        return bitstring
    else:
        lst = ['[']
        not_started = True
        for i, c in enumerate(bitstring):
            if c == '1':
                if not_started:
                    lst.append('%d:1' % i)
                else:
                    lst.append(';%d:1' % i)
                not_started = False
        lst.append(']')
        return "".join(lst)

if __name__ == '__main__':
    argc = len(sys.argv)
    if argc != 2 and argc != 3:
        #                    0  1                  2
        print("fatal: usage: %s filename.{smi|sdf} [--sparse]" % sys.argv[0],
              file=sys.stderr)
        exit(1)
    mol_reader = get_mol_reader(sys.argv[1])
    sparse_format = (argc == 3) and (sys.argv[2] == '--sparse')
    # In ECFP4, 4 is a diameter but rdkit wants a radius
    fp_radius = 2
    fp_length = 2048
    for mol, mol_name in mol_reader:
        fp = AllChem.GetMorganFingerprintAsBitVect(mol, fp_radius,
                                                   nBits=fp_length)
        bitstring = string_of_bitstring(sparse_format, fp)
        print("%s,0.0,%s" % (mol_name, bitstring))
