# orsvm-e1071
Access from OCaml SVMs provided by the R e1071 and svmpath packages.

OCaml functions to drive the RBF and linear kernel SVMs of the R e1071 package
and the linear kernel SVM of the R svmpath package.

This is low level on purpose: we don't provide access to all functionalities
offered by the R packages.

This is probably not high performance: data are exchanged between
R and OCaml via ASCII files.

This project is part of the poor lonesome cowboy's plan to enhance
the OCaml ecosystem in the Machine Learning field.
Don't hesitate to contact me if you want to join this effort.
