
library(keras, quietly = TRUE)

test_fn = "data/Boston_regr_test.csv"

load("saved.Rdata") # provides mean, std, serialized

# normalize test data
test_set <- as.matrix(read.table(test_fn, colClasses = "numeric",
                                 header = TRUE))
cols_count <- dim(test_set)[2]
test_data <- test_set[, 2:cols_count] # skip the target value 1st col
test_data <- scale(test_data, center = mean, scale = std)
test_targets <- test_set[, 1:1]

# retrieve model
model <- unserialize_model(serialized)

model %>% evaluate(test_data, test_targets, verbose = 0)
