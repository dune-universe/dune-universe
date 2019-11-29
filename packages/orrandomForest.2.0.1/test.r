# load lib
library('randomForest', quietly = TRUE)

# matrix with n rows (observations) and p columns (features)
x <- as.matrix(read.table("data/train_data.txt", colClasses = "numeric"))

# vector of size n and values +1 or -1 only
y <- as.vector(read.table("data/train_labels.txt"), mode = "numeric")

# transform y into factors (class labels) so that randomForest will do
# classification (else it does regression by default for numeric values)
y <- cut(y, breaks = 2, labels = c("0","1"))

# check number of rows
stopifnot(nrow(x) == length(y))

# train
rf <- randomForest(x, y, importance = TRUE)

save(rf, file = "rf_model.bin")

# restore the rf object
load("rf_model.bin")

# stupid test on training data; don't do this at home !!!
values <- predict(rf, newdata = x, type = 'vote')

# get votes for the active class
values <- values[,2]

write.table(values, file = "data/predictions.txt", sep = "\n", row.names = F, col.names = F)

quit()
