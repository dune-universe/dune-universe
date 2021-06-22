# ORF: OCaml Random Forests

Random Forests (RFs) are one of the workhorse of modern machine learning.
Especially, they cannot over-fit to the training set, are
fast to train, predict fast, parallelize well and give you a reasonable model
even without optimizing the model's default hyper-parameters.
In other words, it is hard to shoot yourself in the foot while
training or exploiting a Random Forests model.
In comparison, with deep neural networks
it is very easy to shoot yourself in the foot.

Using out of bag (OOB) samples, you can even get an idea
of a RFs performance, without the need for a held out
(test) dataset.

Their only drawback is that RFs, being an ensemble model,
cannot predict values which are outside of the training set
range of values (this _is_ a serious limitation in case you
are trying to optimize or minimize something in order to discover
outliers, compared to your training set samples).

For the moment, this implementation will only consider a sparse vector of
integers as features. i.e. categorical variables will need to be
one-hot-encoded.

# Bibliography

Breiman, Leo. (1996). "Bagging Predictors". Machine learning, 24(2), 123-140.

Breiman, Leo. (2001). "Random Forests". Machine learning, 45(1), 5-32.

Geurts, P., Ernst, D., & Wehenkel, L. (2006). "Extremely Randomized Trees".
Machine learning, 63(1), 3-42.
