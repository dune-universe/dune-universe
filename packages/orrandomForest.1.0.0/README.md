# orrandomForest
OCaml wrapper to the R randomForest package

Warning: any sparse matrix read in .csr format is converted to a dense one
         (the R package randomForest doesn't support sparse matrices...)
