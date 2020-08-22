# linwrap
Wrapper on top of liblinear-tools.

Linwrap can be used to train a L2-regularized logistic regression classifier
or a linear Support Vector Regressor.
You can optimize C (the L2 regularization parameter), w (the class weight)
or k (the number of bags, i.e. use bagging).
You can also find the optimal classification threshold using MCC maximization,
use k-folds cross validation, parallelization, etc.
In the regression case, you can only optimize C and epsilon.

Bibliography
============

[1] Fan, R. E., Chang, K. W., Hsieh, C. J., Wang, X. R., & Lin, C. J. (2008).
LIBLINEAR: A library for large linear classification.
Journal of machine learning research, 9(Aug), 1871-1874.

[2] Hsu, C. W., Chang, C. C., & Lin, C. J. (2003).
A practical guide to support vector classification.

[3] Hsia, J. Y., & Lin, C. J. (2020).
Parameter selection for linear support vector regression.
IEEE Transactions on Neural Networks and Learning Systems.

[4] Breiman, L. (1996).
Bagging predictors.
Machine learning, 24(2), 123-140.
