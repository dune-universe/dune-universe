#!/bin/bash

#set -x # DEBUG

# encoding an SDF or a SMILES file is the same
# and it is the one we expect
diff <(./bin/type_atoms.py data/caff_coca.sdf) data/caff_coca_types.ref
diff <(./bin/type_atoms.py data/caff_coca.smi) data/caff_coca_types.ref

# ph4 features are the same than the ones extracted by ShowFeats.py
# (that were checked by hand and stored in a reference file)
diff <(./bin/ph4_type_atoms.py data/caff_coca.sdf) data/caff_coca_feats.ref
