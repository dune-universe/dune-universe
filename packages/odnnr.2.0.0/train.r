
library(keras, quietly = TRUE)

train_fn = "data/Boston_regr_train.csv"

training_set <- as.matrix(read.table(train_fn, colClasses = "numeric",
                                     header = TRUE))
lines_count = dim(training_set)[1]
cols_count = dim(training_set)[2]

train_data <- training_set[, 2:cols_count] # all lines, all columns except 1st
nb_cols <- dim(train_data)[2]
train_targets <- training_set[, 1:1] # all lines, only 1st column (resp. var)

# normalize training data
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)

# model architecture
build_model <- function() {
    # there are 64 units per hidden layer while the input has 13 columns ?!
    model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = nb_cols) %>%
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1)

    model %>% compile(
        optimizer = "rmsprop",
        loss = "mse",
        metrics = c("mae")
    )
}

model <- build_model()

model %>% fit(train_data, train_targets, epochs = 50, batch_size = 1,
              verbose = 1)

# save model and normalization params
serialized <- serialize_model(model)
save(list = c("mean", "std", "serialized"), file = "saved.Rdata")
