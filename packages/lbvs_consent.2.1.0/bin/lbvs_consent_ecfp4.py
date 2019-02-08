#!/usr/bin/python

# get the ECFP4 bitstring for each molecule in input file; using rdkit's python bindings

from __future__ import print_function

import sys

from rdkit import Chem
from rdkit.Chem import AllChem

# In ECFP4, 4 stands for the diameter of the atom environment
fp_diameter = 4
# but rdkit wants a radius
fp_radius = fp_diameter / 2

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

argc = len(sys.argv)
if argc != 2:
    #                    0  1
    print("fatal: usage: %s filename.{smi|sdf}" % sys.argv[0],
          file=sys.stderr)
    exit(1)

mol_reader = get_mol_reader(sys.argv[1])
i = 0
# print("#mol_name,IC50 in mol/L (0.0 means unknown),ECFP4 bitstring");
for mol, mol_name in mol_reader:
    try:
        # nBits=2048 is the default ECFP4 length in rdkit
        fp = AllChem.GetMorganFingerprintAsBitVect(mol, fp_radius).ToBitString()
        print("%s,0.0,%s" % (mol_name, fp))
    except:
        print("%s: error: molecule at index %d" % (sys.argv[0], i),
              file=sys.stderr)
    i = i + 1
