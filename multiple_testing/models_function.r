setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/") #windows
setwd("/media/lospina/USB/chapter_2")

data <- read.csv("data_set.csv", header = T)
colnames(data)

brt_model <- function(data_train, data_test, number_trees = 2500, learning_rate = .002, predictor, response){
  
  exp11_mod1_2019 <- gbm.step(data            = data_train, 
                              gbm.x           = predictor, 
                              gbm.y           = response, 
                              family          = "gaussian", 
                              tree.complexity = 5,
                              max.trees       = number_trees, 
                              learning.rate   = learning_rate, 
                              bag.fraction    = .5)
  
  exp11_mod2_2019 <- gbm.simplify(exp11_mod1_2019)
  
  value  <- exp11_mod2_2019$deviance.summary  %>%
    filter(mean != 0) %>%
    arrange(abs(mean), se) %>%
    slice(1)
  
  position <- which(exp11_mod2_2019$deviance.summary$mean == value$mean)
  
  
  for (i in position){
    exp11_mod3_2019 <- gbm.step(data            = data_train,
                                gbm.x           = exp11_mod2_2019$pred.list[[i]],
                                gbm.y           = response,
                                family          = "gaussian",
                                max.trees       = number_trees,
                                tree.complexity = 5,
                                learning.rate   = learning_rate,
                                verbose         = FALSE)
    
    y_real_training <- data_train[ ,response]
    y_pred_training <- predict.gbm(object  = exp11_mod3_2019,
                                   newdata = data_train[,predictor],
                                   n.trees = exp11_mod3_2019$n.tree)
    
    y_real_test <- data_test[ ,response]
    y_pred_test <- predict.gbm(object  = exp11_mod3_2019,
                                   newdata = data_test[,predictor],
                                   n.trees = exp11_mod3_2019$n.tree)
    
    lm_train <- lm(y_real_training ~ y_pred_training)
    rmse_train <- sqrt(mean((y_real_training - y_pred_training)^2))
    mae_train <- mean(abs(y_real_training - y_pred_training))
    
    print(i)
    print("training metrics")
    print(summary(lm_train))
    print(rmse_train)
    print(mae_train)
    
    lm_test <- lm(y_real_test ~ y_pred_test)
    rmse_test <- sqrt(mean((y_real_test - y_pred_test)^2))
    mae_test <- mean(abs(y_real_test - y_pred_test))
    
    print("testing_metrics")
    print(summary(lm_test))
    print(rmse_test)
    print(mae_test)
    
    return(y_pred_test)
  }
 
}


rf_model <- function(data_train, data_test, number_trees = 2500, learning_rate = .002, predictor, response){
  
  data_train <- select(data_train, -any_of(c("x","FDiv","FEve","FDis","y")))
  data_test <- select(data_test, -any_of(c("x","FDiv","FEve","FDis","y")))
  
  #Applying mtry to get optimum number of trees
  mtry <- tuneRF(data_train[, c(predictor)],
                 data_train[, c(response)],
                 ntreeTry   = number_trees,
                 stepFactor = 1.5,
                 improve    = 0.01,
                 trace      = TRUE,
                 plot       = TRUE)
  
  best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
  
  rf <- randomForest(FRic ~ .,
                     data       = data_train,
                     mtry       = best_m,
                     importance = TRUE,
                     ntree      = number_trees)
  
  print(varImpPlot(rf))
  
  
  y_real_training <- data_train[ ,response]
  y_pred_training <- predict(object  = rf,
                             newdata = data_train[ ,predictor], # nolint
                             n.trees = rf$ntree)
  
  y_real_test <- data_test[ ,response]
  y_pred_test <- predict(object  = rf,
                         newdata = data_test[ ,predictor],
                         n.trees = rf$ntree)
  
  
  lm_train <- lm(y_real_training ~ y_pred_training)
  rmse_train <- sqrt(mean((y_real_training - y_pred_training)^2))
  mae_train <- mean(abs(y_real_training - y_pred_training))

  print("training metrics")
  print(summary(lm_train))
  print(rmse_train)
  print(mae_train)
  
  lm_test <- lm(y_real_test ~ y_pred_test)
  rmse_test <- sqrt(mean((y_real_test - y_pred_test)^2))
  mae_test <- mean(abs(y_real_test - y_pred_test))
  
  print("testing_metrics")
  print(summary(lm_test))
  print(rmse_test)
  print(mae_test)
  
  return(y_pred_test)
}

xgboost_model <- function(data_train, data_test, predictor, response){
  # Cross validation to find the number of trees
  cv <- xgb.cv(data  = as.matrix(data_train[ ,predictor]),
               label = data_train[, c(response)],
               objective = "reg:squarederror",
               nrounds = 1000,
               nfold = 5,
               eta = 0.02,
               max_depth = 6)
  
  elog <- as.data.frame(cv$evaluation_log)
  plot(elog$iter, elog$test_rmse_mean)
  nrounds <- which.min(elog$test_rmse_mean) # best number of trees
  
  model <- xgboost(data  = as.matrix(data_train[ ,predictor]),
                   label = data_train[, c(response)],
                   nrounds = nrounds,
                   objective = "reg:squarederror",
                   eta = 0.02,
                   max_depth = 6)
  
  y_real_training <- as.matrix(data_train[, c(response)])
  y_pred_training <- predict(model, as.matrix(data_train[, c(predictor)]))
  
  lm_train <- lm(y_real_training ~ y_pred_training)
  rmse_train <- sqrt(mean((y_real_training - y_pred_training)^2))
  mae_train <- mean(abs(y_real_training - y_pred_training))
  
  print("training metrics")
  print(summary(lm_train))
  print(rmse_train)
  print(mae_train)
  
  y_real_test <- as.matrix(data_test[, c(response)])
  y_pred_test <- predict(model, as.matrix(data_test[, c(predictor)]))
  
  lm_test <- lm(y_real_test ~ y_pred_test)
  rmse_test <- sqrt(mean((y_real_test - y_pred_test)^2))
  mae_test <- mean(abs(y_real_test - y_pred_test))
  
  print("testing_metrics")
  print(summary(lm_test))
  print(rmse_test)
  print(mae_test)
  
  return(y_pred_test)
}

