#set directory
setwd("/Volumes/USB") # mac
setwd("D:/") #windows
setwd("/media/lospina/USB") #linux

# Import libraries and functions
source("github/functional_ecology_maps/functions.R")

# Read data
data <- read.csv("data_set.csv", header = T)

# read the raster stack generated in the data engineering part
br_MeSl_2019_c <- stack("stack2.tiff")

# If the csv got a X column, remove it
data <-select(data, -any_of("X"))

data_split <- initial_split(data, prop = 2/4)

data_train <- training(data)
data_test  <- testing(data)

# Cross validation to find the number of trees
cv <- xgb.cv(data  = as.matrix(df_Me_B_SlGe_2019_train[,-c(ncol(df_Me_B_SlGe_2019_train))]),
             label = df_Me_B_SlGe_2019_train[,ncol(df_Me_B_SlGe_2019_train)],
             objective = "reg:squarederror",
             nrounds = 1000,
             nfold = 5,
             eta = 0.02,
             max_depth = 6)

elog <- as.data.frame(cv$evaluation_log)
plot(elog$iter, elog$test_rmse_mean)
nrounds <- which.min(elog$test_rmse_mean) # best number of trees

model <- xgboost(data  = as.matrix(df_Me_B_SlGe_2019_train[,-c(ncol(df_Me_B_SlGe_2019_train))]),
                 label = df_Me_B_SlGe_2019_train[,ncol(df_Me_B_SlGe_2019_train)],
                 nrounds = nrounds,
                 objective = "reg:squarederror",
                 eta = 0.02,
                 max_depth = 6)

df_Me_B_SlGe_2019_test$pred <- predict(model, as.matrix(df_Me_B_SlGe_2019_test[,-c(ncol(df_Me_B_SlGe_2019_test))]))

View(df_Me_B_SlGe_2019_test)

plot(df_Me_B_SlGe_2019_test$pred, df_Me_B_SlGe_2019_test$FRic)

lm1 <- lm(data = df_Me_B_SlGe_2019_test, formula = pred ~ FRic)
summary(lm1)

df_Me_B_SlGe_2019_train$pred <- predict(model, as.matrix(df_Me_B_SlGe_2019_train[,-c(ncol(df_Me_B_SlGe_2019_train))]))

plot(df_Me_B_SlGe_2019_train$pred, df_Me_B_SlGe_2019_train$FRic)

lm2 <- lm(data = df_Me_B_SlGe_2019_train, formula = pred ~ FRic)
summary(lm2)

#### prediction into a map
dir <- getwd()
nom_mapa<- paste0(dir, "model_xgboost3")

result   <- predict(model     = xgb1, 
                    #object    = br_MeSl_2019_c,
                    object   = br_MeSl_2019_c[1:(nrow(br_MeSl_2019_c)*ncol(br_MeSl_2019_c))],
                    filename = nom_mapa,
                    progress  = "text",
                    type      = "response",
                    format    = "GTiff",
                    overwrite = TRUE)

xgbpred<- function(model, input, ..){
  predict(model, newdata=as.matrix(input))
}

p<- predict(br_MeSl_2019_c, model = xgb1, fun = xgbpred)

writeRaster(p, "Fric_xgboost.tif")