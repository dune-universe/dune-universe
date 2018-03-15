library('svmpath')

# matrix with n rows (observations) and p columns (features)
x = as.matrix(read.table("data/train_data.txt"))

# vector of size n and values +1 or -1 only
y = as.vector(read.table("data/train_labels.txt"), mode = "numeric")

# check number of rows
stopifnot(nrow(x) == length(y))

# TODO: scan C values for the linear kernel

path <- svmpath(x, y)
lambdas = path$lambda
write.table(lambdas, file = "data/lambdas.txt", sep = "\n", row.names = F, col.names = F)

# model <- svm(x, y, type = 'C-classification', scale = FALSE,
#              kernel = "radial", cost, gamma)

# save(model, file="r_svm_model.bin")

# load("r_svm_model.bin")

# values = attributes(predict(model, newdata = x, decision.values = TRUE))$decision.values

# write.table(values, file = "data/predictions.txt", sep = "\n", row.names = F, col.names = F)

# quit()
