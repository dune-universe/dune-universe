
# just train and store a model in a file

library("pls", quietly = TRUE)

# load training set
data <- as.matrix(read.table("data/solubility_train_std_01.csv",
                             colClasses = "numeric", header = TRUE))
ydim <- dim(data)[1]
xdim <- dim(data)[2]
stopifnot(xdim == 842 && ydim == 1004)
xs <- data[, 2:842] # all lines, only feature columns
ys <- data[, 1:1] # all lines, 1st column only (response var.)
# I(x) prevents xs from having its columns split into different variables
train_data <- data.frame(y = ys, x = I(xs))

# just train a model, assuming ncomp_best = 13
model <- plsr(y ~ x, ncomp = 13, method = "simpls", data = train_data,
              validation = "none")

save(model, file="oplsr_model.bin")
quit()
