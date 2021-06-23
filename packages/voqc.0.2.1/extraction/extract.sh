#!/bin/bash

# Make sure the SQIR submodule is up to date
git submodule init
git submodule update

# Change into the SQIR directory
cd SQIR

# Build VOQC Coq proofs
make voqc

# Back to the current directory
cd ..

# Perform extraction
coqc -R SQIR Top Extraction.v

# Remove unneeded files
rm -f *.glob *.mli *.vo*

# Remove empty and unused files. Some files are unused because we manually
# extract types like R and Q. We also use custom FSetAVL and FMapAVL files
# (see ml/extracted), which are wrappers around OCaml's maps and sets.
rm -f BinNums.ml ClassicalDedekindReals.ml ConstructiveCauchyReals.ml \
      FMap* FSet* Int.ml List.ml MSet* Nat.ml Order* QArith_base.ml \
      Ratan.ml Rdefinitions.ml Ring_theory.ml ROrderedType.ml Rtrigo1.ml \
      Rtrigo_def.ml Specif.ml ZArith_dec.ml

# Move remaining extracted files to the ml/extracted directory.
mv Bin*.ml CXCancellation.ml ChangeRotationBasis.ml ConnectivityGraph.ml Datatypes.ml \
   GateCancellation.ml HadamardReduction.ml IBMGateSet.ml Layouts.ml Main.ml \
   NotPropagation.ml Optimize1qGates.ml PeanoNat.ml RotationMerging.ml \
   RzQGateSet.ml SimpleMapping.ml StandardGateSet.ml UnitaryListRepresentation.ml \
   ../ml/extracted

