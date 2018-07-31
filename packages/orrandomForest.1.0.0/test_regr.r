# load lib
library('randomForest', quietly = TRUE)
library('Matrix')

read_csr_file <- function(file, ncol = NULL)
{
    lines <- readLines(file)
    nrow <- length(lines)
    res <- Matrix(0, nrow, ncol)
    i <- 1
    for (line in lines) {
      cols = strsplit(line, '[ ]+')
      for (col in cols[[1]]) {
        s <- strsplit(col, ':')
        j <- as.integer(s[[1]][1])
        k <- as.numeric(s[[1]][2])
        res[i, j + 1] <- k
      }
      i <- i + 1
    }
    res
}

# # read from a sparse file
# x <- read_csr_file("data/train_features.txt", ncol = 121632)
# # unsparse (sparce matrices are not supported...)
# # matrix with n rows (observations) and p columns (features)
# x <- as.matrix(x)

# read dense training data
x = as.matrix(read.table("data/Boston_train_features.csv"))

# vector of values
y <- scan("data/Boston_train_values.csv")

# check number of rows
stopifnot(nrow(x) == length(y))

# train
rf <- randomForest(x, y, importance = FALSE, do.trace = TRUE)

# save(rf, file = "rf_reg_model.bin")
# # restore the rf object
# load("rf_reg_model.bin")

## read sparse test data
# newdata <- read_csr_file("data/test_features.txt", ncol = 121632)
# newdata <- as.matrix(newdata)

# read dense test data
newdata = as.matrix(read.table("data/Boston_test_features.csv"))

values <- predict(rf, newdata)

write.table(values, file = "data/Boston_test_preds.csv", sep = "\n", row.names = F, col.names = F)

quit()
