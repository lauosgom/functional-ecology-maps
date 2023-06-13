setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/") #windows
setwd("/media/lospina/USB/chapter_2")

data <- read.csv("data_set.csv", header = T)
colnames(data)

brt_model <- function(data, proportion_split = 3/4, number_trees = 2500, learning_rate = .002, predictor, response){
  
  data <- select(data, -any_of("X"))
  
  data_split <- initial_split(data, prop = proportion_split)
  data_train <- training(data_split)
  data_test  <- testing(data_split)
  
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
  }
 
}


rf_model <- function(data, proportion_split = 3/4, number_trees = 2500, learning_rate = .002, predictor, response){
  
}
