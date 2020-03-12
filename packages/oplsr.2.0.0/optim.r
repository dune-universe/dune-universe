
# find the optimial number of PLS components

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

# find the best ncomp value; segments is the number of folds for NxCV
model <- plsr(y ~ x, method = "simpls", data = train_data,
              validation = "CV", segments = 5)
r2 <- R2(model)
## plot(r2)
r2s <- unlist(r2[1])
ncomp_best <- which.max(r2s)
r2_max = r2s[ncomp_best]

# define printf
printf <- function(...) cat(sprintf(...))

printf("ncomp: %d R2: %f\n", ncomp_best, r2_max)
quit()
