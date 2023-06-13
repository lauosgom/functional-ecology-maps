setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/") #windows
setwd("/media/lospina/USB/chapter_2")

data <- read.csv("data_set.csv", header = T)
colnames(data)

data <- select(data, -any_of("X"))

data_split <- initial_split(data, prop = 3/4)
data_train <- training(data_split)
data_test  <- testing(data_split)

results_brt <- brt_model(data_train, data_test ,predictor = 1:35, response = 36)

results_rf <- rf_model(data_train, data_test, predictor = 1:35, response = 36)

results_xgboost <- xgboost_model(data_train, data_test, predictor = 1:35, response = 36)



results <- cbind(data_test[, c(36)],results_brt, results_rf, results_xgboost)
