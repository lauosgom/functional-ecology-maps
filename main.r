setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/") #windows
setwd("/media/lospina/USB/chapter_2")

source("/media/lospina/USB/chapter_2/github/functional-ecology-maps/feature_engineering/functions.r")

data <- read.csv("data_set.csv", header = T)
colnames(data)

data <- select(data, -any_of("X"))

data_split <- initial_split(data, prop = 3/4)
data_train <- training(data_split)
data_test  <- testing(data_split)

data_train <- data_train[!duplicated(data_train$FRic), ]
data_test <- data_test[!duplicated(data_test$FRic), ]

results_brt <- brt_model(data_train, data_test ,predictor = 1:35, response = 36)

results_rf <- rf_model(data_train, data_test, predictor = 1:35, response = 36)

results_xgboost <- xgboost_model(data_train, data_test, predictor = 1:35, response = 36)



results <- cbind(data_test$FRic, results_brt, results_rf, results_xgboost)

View(results)
