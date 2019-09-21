library('xgboost')

# matrix with n rows (observations) and p columns (features)
x <- as.matrix(read.table("data/train_data.txt", colClasses = "numeric"))

# vector of size n and values +1 or -1 only
y <- as.vector(read.table("data/train_labels.txt"), mode = "numeric")

# transform [-1,1] to [0,1]
lut <- data.frame(old = c(-1.0,1.0), new = c(0.0,1.0))
labels <- lut$new[match(y, lut$old)]

# check number of rows
stopifnot(nrow(x) == length(labels))

# train
gbtree <- xgboost(data = x, label = labels, nrounds = 300, objective = "binary:logitraw", eval_metric = "auc", eta = 0.01, subsample = 0.5)

xgb.save(gbtree, "r_gbtree_model.bin")

xgb.load("r_gbtree_model.bin")

# stupid test on training data; don't do this at home !!!
values <- predict(gbtree, x)

write.table(values, file = "data/predictions.txt", sep = "\n", row.names = F, col.names = F)

quit()
