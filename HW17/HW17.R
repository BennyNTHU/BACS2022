library(rpart)
library(rpart.plot)
library(magrittr)
library(dplyr)

dataset <- read.csv("insurance.csv")
dataset <- na.omit(dataset)
# Split the dataset
train_indices<- sample(1:nrow(dataset), size = 0.8*nrow(dataset)) 
train_set <- dataset[train_indices,]
test_set <- dataset[-train_indices,]

# Question 1 (a)
ins_lm <- lm(charges ~ age + factor(sex) + bmi + factor(smoker) + 
               factor(region), data=dataset)
report_lm <- summary(ins_lm)
write.table(round(report_lm$coefficients, digits=5), 
            file="1a.csv", 
            sep = ",", 
            col.names=NA)

# Question 1 (b)
ins_tree <- rpart(charges ~ age + sex + bmi + smoker + region, data=dataset)
rpart.plot(ins_tree) # visual representation of the tree
group_1 <- dataset%>%filter(smoker == "yes" & age<43)
group_2 <- dataset%>%filter(smoker == "yes" & age>=43)
group_3 <- dataset%>%filter(smoker == "no" & bmi<30)
group_4 <- dataset%>%filter(smoker == "no" & bmi>=30)
charges_avg <-  round(cbind(mean(group_1$charges), 
                            mean(group_2$charges), 
                            mean(group_3$charges), 
                            mean(group_4$charges), digits=3))
names(charges_avg) <- c("smoker, age<43", 
                        "smoker, age>=43", 
                        "non-smoker, bmi<30", 
                        "non-smoker, bmi>=30")
write.table(charges_avg, file="1b.csv", sep = ",", col.names=NA)

# Question 2
mse_oos <- function(actuals, preds) { # MSEs in this homework are all RMSE
  sqrt(mean( (actuals - preds)^2 ))
}

mse_oos(dataset$charges, predict(ins_lm, dataset))
mse_oos(dataset$charges, predict(ins_tree, dataset))

# Question 3 (a)
# Thanks to 108071021 for help of this part of codes
bagged_retrain <- function(model, dataset, b){ 
  resample <- unique(sample(1:nrow(dataset), replace = TRUE))
  train_data <- dataset[resample,]
  train_model <- update(model, data = train_data)
  train_model
}

bagged_learn <- function(model, dataset, b=100){
  lapply(1:b, bagged_retrain, model = model, dataset = dataset)
}

pred <- function(model, dataset, b){
  model = model[[b]]
  predict(model, dataset)
}

bagged_predict <- function(bagged_model, dataset, b){
  prediction <- lapply(1:b, pred, model = bagged_model, dataset = dataset)
  mse_oos(unlist(prediction), rep(unlist(dataset[7]), times = b))
}

# Question 3 (b)
lm_bagged_models <- bagged_learn(ins_lm, train_set, 100)
lm_bagged_mse <- bagged_predict(lm_bagged_models, test_set, 100)

# Question 3 (c)
ins_tree_models <- bagged_learn(ins_tree, train_set, 100)
ins_tree_bagged_mse <- bagged_predict(ins_tree_models, test_set, 100)

# Question 4 (a)
boosted_learn <- function(model, dataset, outcome, n=100, rate=0.1, type) {
  # get data frame of only predictor variables
  predictors <- dataset[, -which(names(dataset) %in% outcome)]
  # Initialize residuals and models
  res <- dataset[,outcome] # get vector of actuals to start
  models <- list()
  for (i in 1:n) {
    this_model <- update(model, data = cbind(charges = res, predictors))
    # update residuals with learning rate
    if (type == "l") 
    {
      res <- res - rate * this_model$fitted.values
    }
    else
    {
      res <- res - rate * this_model$y
    }
    models[[i]] <- this_model # Store model
  }
  list(models=models, rate=rate)
}

boost_predict <- function(boosted_learning, new_data) {
  boosted_models <- boosted_learning$models
  rate <- boosted_learning$rate
  n <- length(boosted_models)
  
  # get predictions of new_data from each model
  predictions = lapply(1:n, function(i){
    rate*predict(boosted_models[[i]], new_data)
  })
  
  pred_frame = as.data.frame(predictions) #%>% unname
  pred <- apply(pred_frame, 1, sum)
  mse_oos(pred, new_data[,7]) # 7 for charges
}

# Question 4 (b)
ins_lm_boosted_model <- boosted_learn(ins_lm, train_set, outcome = "charges", 
                                      type = "l")
ins_lm_boosted_mse <- boost_predict(ins_lm_boosted_model, test_set)

# Question 4 (c)
ins_tree_stump <- rpart(charges ~ age + sex + bmi + smoker + region, 
                        data=dataset, cp=0, maxdepth=1)
ins_tree_boosted_model <- boosted_learn(ins_tree_stump, train_set, 
                                        outcome="charges", type='t')
ins_tree_boost_mse <- boost_predict(ins_tree_boosted_model, test_set)

# Question 5 (a)
flag <- 0 # break the loop or not
maxdepth_ins <- 1 # maxdepth of the tree
maxdepth_vector <- c()
bagged_mse <- c()

while(flag == 0){
  control <- rpart.control(maxdepth = maxdepth_ins, cp = 0)
  
  # Using code in Question 3 (c)
  ins_tree <- rpart(charges ~ bmi + age + sex + children + smoker + region, 
                    data = train_set, control = control)
  ins_tree_models <- bagged_learn(ins_tree, train_set, 100)
  ins_tree_bagged_mse <- bagged_predict(ins_tree_models, test_set, 100)
  
  # append the result to the vector
  maxdepth_vector <- c(maxdepth_ins)
  bagged_mse <- c(bagged_mse, ins_tree_bagged_mse)
  
  # To determine to brake the loop or not
  if(length(bagged_mse) >= 2){
    if(bagged_mse[maxdepth_ins-1] < bagged_mse[maxdepth_ins]){
      flag <- 1 # break
    }
  }
  maxdepth_ins <- maxdepth_ins + 1
}

result_dataframe_5a <- round(cbind(maxdepth_vector, bagged_mse), digits=3)
names(result_dataframe_5a) <- c("maxdepth", "bagged_mse")
write.table(result_dataframe_5a, file="5a.csv", sep = ",", col.names=NA)

# Question 5 (b)
flag <- 0 # break the loop or not
maxdepth_ins <- 1 # maxdepth of the tree
maxdepth_vector <- c()
boost_mse <- c()

while(flag == 0){
  control <- rpart.control(maxdepth = maxdepth_ins, cp = 0)
  
  # Using code in Question 4 (c)
  ins_tree_stump <- rpart(charges ~ age + sex + bmi + smoker + region, 
                          data=dataset, control = control)
  ins_tree_boosted_model <- boosted_learn(ins_tree_stump, train_set, 
                                          outcome="charges", type='t')
  ins_tree_boost_mse <- boost_predict(ins_tree_boosted_model, test_set)
  
  # append the result to the vector
  maxdepth_vector <- c(maxdepth_ins)
  boost_mse <- c(boost_mse, ins_tree_boost_mse)
  
  # To determine to brake the loop or not
  if(length(boost_mse) >= 2){
    if(boost_mse[maxdepth_ins-1] < boost_mse[maxdepth_ins]){
      flag <- 1 # break
    }
  }
  maxdepth_ins <- maxdepth_ins + 1
}

result_dataframe_5b <- round(cbind(maxdepth_vector, boost_mse), digits=3)
names(result_dataframe_5b) <- c("maxdepth", "bagged_mse")
write.table(result_dataframe_5b, file="5b.csv", sep = ",", col.names=NA)
