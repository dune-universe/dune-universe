# RanKers
Reference implementation of the Vanishing Ranking Kernels (VRK) method

# Example

![Logo](data/ROC.png?raw=true)
*Example ROC curve on a hold-out test set. The test set had 38 active
molecules and 664 inactives. ROC AUC: 0.861; BEDROC AUC: 0.766; PR AUC: 0.678.
The ROC curve is in purple; the precision-recall (PR) curve in cyan. The
probability of activity given a raw score is the red curve.
The green curve is the number of actives divided by the
number of decoys as a function of the scores filtering threshold.*

Train and test a model:
```
rankers_bwmine -i data/tox21_nrar_ligands_std_rand_01.txt
```

Same, but using 16 cores :
```
rankers_bwmine -np 16 -i data/tox21_nrar_ligands_std_rand_01.txt
```

# Usage

```
rankers_bwmine -i <train.txt>
  [-p <float>]: proportion of the (randomized) dataset
  used to train (default=0.80)
  [-k {uni|tri|epa|biw}]: kernel function choice (default=biw)
  [-np <int>]: max number of processes (default=1)
  [-o <filename>]: write raw test scores to file
  [--train <train.txt>]: training set (overrides -p)
  [--valid <valid.txt>]: validation set (overrides -p)
  [--test <test.txt>]: test set (overrides -p)
  [-n <int>]: max number of optimization steps; default=150
  [--capf <float>]: keep only fraction of decoys
  [--capx <int>]: keep only X decoys per active
  [--capi <int>]: limit total number of molecules
  (but keep all actives)
  [--seed <int>: fix random seed]
  [--pr]: use PR AUC instead of ROC AUC during optimization
  [-kb <float>]: user-chosen kernel bandwidth
  [--mcc-scan]: scan classif. threshold to maximize MCC
  [--tap]: tap the train-valid-test partitions to disk
  [-q|--quick]: exit early; just after model training
  [--noplot]: turn off gnuplot
  [-v]: verbose/debug mode
  [-h|--help]: show this help message
```
