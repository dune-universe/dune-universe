library('e1071')

# matrix with n rows (observations) and p columns (features)
x = as.matrix(read.table("data/train_data.txt"))

# vector of size n and values +1 or -1 only
y = as.factor(as.vector(read.table("data/train_labels.txt"),
                        mode = "numeric"))

# check number of rows
stopifnot(nrow(x) == length(y))

# RBF kernel parameters
cost = 1
gamma = 1 / ncol(x)

model <- svm(x, y, type = 'C-classification', scale = FALSE,
             kernel = "radial", cost, gamma)

save(model, file="r_svm_model.bin")

load("r_svm_model.bin")

values = attributes(predict(model, newdata = x, decision.values = TRUE))$decision.values

write.table(values, file = "data/predictions.txt", sep = "\n", row.names = F, col.names = F)

quit()
