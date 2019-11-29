# orrandomForest
OCaml wrapper to the R randomForest package

Can be used for classification or regression.
Features importance can be extracted out of a trained model.

Warning: any sparse matrix read in .csr format is converted to a dense matrix,
         because the R package randomForest doesn't support sparse matrices.
