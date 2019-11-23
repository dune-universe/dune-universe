# Consent
Chemoinformatics software for Ligand-Based Virtual Screening (LBVS)
using consensus queries.

Cf. the INSTALL file for instructions on how to install consent.

# I) Command line help

    lbvs_consent -s {sing|oppo|opti|real|know}
                 -q queries.{sdf|mol2|csv|ecfp4}
                 -db candidates.{sdf|mol2|csv|ecfp4}

      -s <pol> consensus policy {sing|oppo|opti|real|know} (mandatory)
      -q <filename> queries file (known actives; mandatory)
      -db <filename> database to rank order (mandatory)
      -o <filename> where to write scores (can be combined with -top)
      -n <int> consensus size; #known actives used to create query (optional;
               default=all molecules in query file)
      -top <int> how many top scoring molecules to write out (optional;
           default=all; must be combined with -o)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1006728.svg)](https://doi.org/10.5281/zenodo.1006728)

# II) Usage recommendation

Please cite the corresponding paper (https://doi.org/10.1186/s13321-017-0248-5)
in case you use this software and publish about your results
(Consensus queries in ligand-based virtual screening experiments.
F. Berenger, O. Vu and J., Meiler. Journal of Cheminformatics, November 2017).

The opportunist consensus policy (-s oppo) is recommended.
It works well with any fingerprint and is usually the best performing
method.

However, if you really need to go faster, here are some recommendations:

- MACCS fingerprint (166 bits): use the realistic policy (-s real); it will
  average the MACCS fingerprints of your known actives.

- ECFP4 fingerprint (2048 bits; folded; uncounted):
  use the optimist policy (-s opti); it will
  do a logical union of the fingerprints of your known actives.

- UMOP2D (unfolded MOLPRINT2D; uncounted): same as for ECFP4, use -s opti.

# III) How to encode your molecules

First, we need some SDF and MOL2 files.
The obabel command is provided by the Open Babel package
(cf. http://openbabel.org).

    obabel data/ARm_actives.smi -O data/ARm_actives.sdf
    obabel data/ARm_inactives.smi -O data/ARm_inactives.sdf
    obabel data/ARm_actives.smi -O data/ARm_actives.mol2
    obabel data/ARm_inactives.smi -O data/ARm_inactives.mol2
    cat data/ARm_actives.mol2 data/ARm_inactives.mol2 > data/ARm_database.mol2

## With the MACCS fingerprint

    lbvs_consent_ob_maccs data/ARm_actives.sdf > data/ARm_actives.maccs
    lbvs_consent_ob_maccs data/ARm_inactives.sdf > data/ARm_inactives.maccs
    cat data/ARm_actives.maccs data/ARm_inactives.maccs > data/ARm_database.maccs

## With the ECFP4 fingerprint

    lbvs_consent_ecfp4.py data/ARm_actives.sdf > data/ARm_actives.ecfp4
    lbvs_consent_ecfp4.py data/ARm_inactives.sdf > data/ARm_inactives.ecfp4
    cat data/ARm_actives.ecfp4 data/ARm_inactives.ecfp4 > data/ARm_database.ecfp4

## With the UMOP2D fingerprint

    lbvs_consent_mop2di -i data/ARm_database.mol2 > data/ARm_database.idx
    lbvs_consent_mop2de -idx data/ARm_database.idx -i data/ARm_database.mol2 -o data/ARm_database.mop2d

# IV) How to query with a consensus query and a consensus policy

    # example with ECFP4 fingerprints and 20 actives
    head -20 data/ARm_actives.ecfp4 > data/ARm_query_20.ecfp4
    # recommended way; AUC ~= 0.60
    lbvs_consent -s oppo -q data/ARm_query_20.ecfp4 -db data/ARm_database.ecfp4 -o scores.txt
    # faster, but still with good performance in many cases; AUC ~= 0.61
    lbvs_consent -s opti -q data/ARm_query_20.ecfp4 -db data/ARm_database.ecfp4 -o scores.txt
