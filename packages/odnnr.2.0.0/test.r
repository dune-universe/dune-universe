
library(keras, quietly = TRUE)

# # CPU version
# install_keras(method = "conda", tensorflow = "cpu")

# # load data from library dataset
# dataset <- dataset_boston_housing()
# train_data <- dataset$train$x
# nb_lines <- dim(train_data)[1]
# nb_cols <- dim(train_data)[2]
# train_targets <- dataset$train$y
# test_data <- dataset$test$x
# nb_cols_test <- dim(test_data)[2]
# stopifnot(nb_cols == nb_cols_test)
# test_targets <- dataset$test$y

# load data from external csv files
train_fn = "data/Boston_regr_train.csv"
test_fn = "data/Boston_regr_test.csv"

# train_fn = "data/solubility_train_std_01.csv"
# test_fn = "data/solubility_test_std_01.csv" 

training_set <- as.matrix(read.table(train_fn, colClasses = "numeric",
                                     header = TRUE))
lines_count = dim(training_set)[1]
cols_count = dim(training_set)[2]


train_data <- training_set[, 2:cols_count] # all lines, all columns except 1st
nb_cols <- dim(train_data)[2]
train_targets <- training_set[, 1:1] # all lines, only 1st column (resp. var)

# normalize train data
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
train_data <- scale(train_data, center = mean, scale = std)

# define the model architecture
build_model <- function() {
    # there are 64 units per hidden layer while the input has 13 columns ?!
    model <- keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = nb_cols) %>%
    layer_dense(units = 64, activation = "relu") %>% 
    layer_dense(units = 1)

    ## with 2 layers of size 64: 1s 2ms/step - loss: 6.1750 - mae: 1.6072
    ## with 3 layers of size 64: 1s 2ms/step - loss: 29.8944 - mae: 1.4733
    ## with 2 layers of size 13: 1s 2ms/step - loss: 9.7488 - mae: 2.0388
    ## with 3 layers of size 13: 1s 2ms/step - loss: 7.6201 - mae: 1.8527

    # R2 as metric not available in keras...
    model %>% compile(
        # optimizers 8 choices !
        # SGD
        # RMSprop
        # Adam
        # Adadelta
        # Adagrad
        # Adamax
        # Nadam
        # Ftrl
        optimizer = "rmsprop",
        # mse "mean squared error"
        # mae "mean absolute error"
        # FBR: I would like (1 - R2)
        loss = "mse",
        # same choice as loss: mse or mae
        # FBR: I would like R2
        metrics = c("mae")
    )
}

model <- build_model()

model %>% fit(train_data, train_targets, epochs = 50, batch_size = 1,
              verbose = 1)

serialized_trained_model <- serialize_model(model)
save(list = c("mean", "std", "serialized"), file = "saved.Rdata")

load("saved.Rdata")
# normalize test data
test_set <- as.matrix(read.table(test_fn, colClasses = "numeric",
                                 header = TRUE))
nb_cols_test <- dim(test_set)[2]
stopifnot(cols_count == nb_cols_test)
test_data <- test_set[, 2:cols_count]

test_data <- scale(test_data, center = mean, scale = std)
test_targets <- test_set[, 1:1]




model %>% evaluate(test_data, test_targets, verbose = 0)
