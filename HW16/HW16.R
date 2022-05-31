# Load the data and remove missing values
cars <- read.table("auto-data.txt", header=FALSE, na.strings = "?")
names(cars) <- c("mpg", "cylinders", "displacement", "horsepower", 
                 "weight", "acceleration", "model_year", "origin", "car_name")
cars$car_name <- NULL
cars <- na.omit(cars)

# Shuffle the rows of cars
set.seed(27935752)
cars <- cars[sample(1:nrow(cars)),]

# Create a log transformed dataset also
cars_log <- with(cars, data.frame(log(mpg), log(cylinders), log(displacement), 
                                  log(horsepower), log(weight), 
                                  log(acceleration), model_year, origin))

# Linear model of mpg over all the variables that don’t have multicollinearity
cars_lm <- lm(mpg ~ weight + acceleration + model_year + factor(origin), 
              data=cars)

# Linear model of log mpg over all the log variables that 
# don’t have multicollinearity
cars_log_lm <- lm(log.mpg. ~ log.weight. + log.acceleration. + model_year + 
                    factor(origin), data=cars_log)

# Linear model of log mpg over all the log variables, including 
# multicollinear terms!
cars_log_full_lm <- lm(log.mpg. ~ log.cylinders. + log.displacement. + 
                         log.horsepower. + log.weight. + log.acceleration. + 
                         model_year + factor(origin), data=cars_log)

# Question 1
set.seed(48763)
train_indices <- sample(1:nrow(cars_log), size=0.70*nrow(cars_log)) # Split the dataset
test_indices <- setdiff(1:nrow(cars_log), train_indices)
train_set <- cars_log[train_indices,]
test_set <- cars_log[test_indices,]

# Question 1 (a)
lm_trained <- lm(log.mpg. ~ log.weight. + log.acceleration. + 
                   model_year + factor(origin), 
                 data=train_set) # train the model
model_report <- summary(lm_trained)
write.table(model_report$coefficients, file="1a.csv", sep = ",", col.names=NA)

# Question 1 (b)
mpg_actual_train <- train_set$log.mpg. # true label of train set
mpg_actual_test <- test_set$log.mpg. # true label of test set
mpg_predicted_train <- predict(lm_trained, train_set) # predict on train set
mpg_predicted_test <- predict(lm_trained, test_set) # predict on test set
pred_err_train <- mpg_actual_train - mpg_predicted_train # error on training set
pred_err_test <- mpg_actual_test - mpg_predicted_test # error on testing set
mse_is <- mean((mpg_predicted_train - mpg_actual_train)^2) # MSE_IS
mse_oos <- mean((mpg_predicted_test - mpg_actual_test)^2) # MSE_OOS

# Question 1 (c)
result_dataframe <- cbind(mpg_actual_test, mpg_predicted_test, pred_err_test)
names(result_dataframe) <- c("Actual log.mpg.", "Predict log.mpg.", "error")
write.table(result_dataframe[1:5, 1:3], file="1c.csv", sep = ",", col.names=NA)

# Question 2 (a)
MSE <- function(model, dataset, actual) { 
  predicted <- predict(model, dataset) # predict
  pred_err <- actual - predicted # error
  mse <- mean((predicted - actual)^2) # MSE
  return(mse)
}

# Split the origin dataset without log-transformed
train_set_org <- cars[train_indices,]

# Compute the MSE_IS'
MSE_cars_lm <- MSE(cars_lm, train_set_org, train_set_org$mpg)
MSE_cars_log_lm <- MSE(cars_log_lm, train_set, train_set$log.mpg.)
MSE_cars_log_full_lm <- MSE(cars_log_full_lm, train_set, train_set$log.mpg.)

# Question 2 (b)
# Calculates mse_oos across all folds
k_fold_mse <- function(model, dataset, k=10) { # model should be a string
  fold_pred_errors <- sapply(1:k, \(i) {
    fold_i_pe(model, i, k, dataset)
  })
  pred_errors <- unlist(fold_pred_errors)
  mean(pred_errors^2)
}

# Calculates prediction error for fold i out of k
fold_i_pe <- function(model, i, k, dataset) { 
  folds <- cut(1:nrow(dataset), k, labels = FALSE) # cut into 10 folds
  
  # Split the dataset
  test_indices <- which(folds == i)
  train_indices <- setdiff(1:nrow(dataset), train_indices)
  train_set <- dataset[train_indices,]
  test_set <- dataset[test_indices,]
  
  # train
  if (model == "cars_lm") {
    trained_model <- lm(mpg ~ weight + acceleration + 
                          model_year + factor(origin), data=train_set)
    actual <- test_set$mpg
  }
  else if (model == "cars_log_lm") {
    trained_model <- lm(log.mpg. ~ log.weight. + log.acceleration. + 
                          model_year + factor(origin), data=train_set)
    actual <- test_set$log.mpg.
  }
  else if (model == "cars_log_full_lm") {
    trained_model <- lm(log.mpg. ~ log.cylinders. + log.displacement. + 
                          log.horsepower. + log.weight. + log.acceleration. + 
                          model_year + factor(origin), data=train_set)
    actual <- test_set$log.mpg.
  }
  
  # predict
  predictions <- predict(trained_model, test_set)
  return(actual - predictions)
}

cars_lm_mse <- k_fold_mse("cars_lm", cars, 10)
cars_log_lm_mse <- k_fold_mse("cars_log_lm", cars_log, 10)
cars_log_full_lm_mse <- k_fold_mse("cars_log_full_lm", cars_log, 10)

# Question 2 (c)
cars_log_lm_392folds_mse <- k_fold_mse("cars_log_lm", cars_log, 392)
