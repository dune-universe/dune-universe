
library("pls", quietly = TRUE)

# FBR: use the cLogP dataset

# load training set
data <- as.matrix(read.table("data/Boston_regr_train.csv", colClasses = "numeric",
                             header = TRUE))
ydim <- dim(data)[1]
xdim <- dim(data)[2]
stopifnot(xdim == 14 && ydim == 456)
xs <- data[, 2:14] # all lines, some columns
ys <- data[, 1:1] # all lines, 1st column only as the response var.
# I(x) prevents xs from having its columns split into different variables
train_data <- data.frame(y = ys, x = I(xs))

# just a train a model
model <- plsr(y ~ x, ncomp = 10, method = "simpls", data = train_data,
              validation = "none")

# load test set
data <- as.matrix(read.table("data/Boston_regr_test.csv",
                             colClasses = "numeric", header = TRUE))
ydim <- dim(data)[1]
xdim <- dim(data)[2]
stopifnot(xdim == 14 && ydim == 51)
xs_test <- data[, 2:14] # all lines, some columns
ys_test <- data[, 1:1] # all lines, 1st column only as the response var.
# I(x) prevents xs from having its columns split into different variables
test_data <- data.frame(y = ys_test, x = I(xs_test))

# find the best ncomp value; segments is the number of folds for NxCV
model <- plsr(y ~ x, method = "simpls", data = train_data,
              validation = "CV", segments = 5)
r2 <- R2(model)
plot(r2)
r2s <- unlist(r2[1])
ncomp_best <- which.max(r2s)
r2_max = r2s[ncomp_best]

# predict using trained model
values <- predict(model, ncomp = ncomp_best, newdata = test_data)

predplot(model, ncomp = ncomp_best, newdata = test_data, asp = 1, line = TRUE)
