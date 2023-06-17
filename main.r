setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/chapter_2/") #windows
setwd("/media/lospina/USB/chapter_2/")

source("github/functional-ecology-maps/feature_engineering/functions.r")
source("github/functional-ecology-maps/models/models_function.r")

data <- read.csv("data_set.csv", header = T)
colnames(data)

data <- select(data, -any_of("X"))

set.seed(123)
data_split <- initial_split(data, prop = 3/4)
data_train <- training(data_split)
data_test  <- testing(data_split)

data_train <- data_train[!duplicated(data_train$FRic), ]
data_test <- data_test[!duplicated(data_test$FRic), ]

# read the raster stack generated in the data engineering part
br_MeSl_2019_c <- stack("data/optical/brick_vif.tif")
names(br_MeSl_2019_c) <- new_names_vif

results_brt <- brt_model(data_train, data_test ,predictor = 1:35, response = 36, rasterstack = br_MeSl_2019_c)

results_rf <- rf_model(data_train, data_test, predictor = 1:35, response = 36)

results_xgboost <- xgboost_model(data_train, data_test, predictor = 1:35, response = 36)

results <- data.frame(y_real = data_test$FRic, results_brt = results_brt, results_rf = results_rf, results_xgboost = results_xgboost)

View(results)

# Set up the split screen layout
par(mfrow = c(2, 2))  # 1 row and 2 columns

# Plot figure 1 on the left side
plot(results$y_real, results$results_brt, main = "brt")

# Plot figure 2 on the right side
plot(results$y_real, results$results_rf, main = "rf")

plot(results$y_real, results$results_xgboost, main = "xgboost")



