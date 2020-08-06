
# use trained model

library("pls", quietly = TRUE)

load("oplsr_model.bin")

# read in test data

data <- as.matrix(read.table("data/solubility_test_std_01.csv",
                             colClasses = "numeric", header = TRUE))
ydim <- dim(data)[1]
xdim <- dim(data)[2]
stopifnot(xdim == 842 && ydim == 251)
xs <- data[, 2:842] # all lines, only feature columns
ys <- data[, 1:1] # all lines, 1st column only (response var.)
# I(x) prevents xs from having its columns split into different variables
test_data <- data.frame(y = ys, x = I(xs))

values <- predict(model, ncomp = 13, newdata = test_data)

write.table(values, file = "oplsr_preds.txt", sep = "\n",
            row.names = F, col.names = F)
quit()
