install.packages("xgboost")

# libraries we're going to use
library(xgboost) # for xgboost
library(tidyverse)

# normalize raster
library(terra)

rescale01 <- function(x) {
  
  val <- values(x)
  
  values(x) <- (val - min(val, na.rm = TRUE)) / (max(val, na.rm = TRUE) - min(val, na.rm = TRUE))
  
  x
}

# Rescale values of SpatRaster object with nlyr = 1
r1_01 <- rescale01(r1)

# Rescale values of SpatRaster object with nlyr > 1
r_stack_01 <- lapply(r_stack, rescale01)


# get the numb 70/30 training test split
numberOfTrainingSamples <- round(length(diseaseLabels) * .7)

# training data
train_data <- diseaseInfo_matrix[1:numberOfTrainingSamples,]
train_labels <- diseaseLabels[1:numberOfTrainingSamples]

# testing data
test_data <- diseaseInfo_matrix[-(1:numberOfTrainingSamples),]
test_labels <- diseaseLabels[-(1:numberOfTrainingSamples)]


# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

# train a model using our training data
xgb_model <- xgb.train(data = dtrain, 
                       params = list(objective = "reg:squarederror", 
                                     eval_metric = "rmse"), 
                       max.depth = 3, # the maximum depth of each decision tree
                       nrounds = 100, 
                       watchlist = list(train = dtrain, test = dtest), 
                       verbose = 1)

pred <- predict(model_tuned, dtest)

exp11_pred_2019_V1 <- raster::predict(object   = br_MeSl_2019_c,
                                      model    = xgb_model,
                                      filename = nom_mapa,
                                      progress = "text",
                                      n.trees  = exp11_mod3_2019$n.tree,
                                      type     = "response",
                                      format   = "GTiff",
                                      overwrite = TRUE)