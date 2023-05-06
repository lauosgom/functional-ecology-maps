install.packages("xgboost")

# libraries we're going to use
library(mlr)
library(xgboost) # for xgboost
library(tidyverse)


# read data
df_Me_B_SlGe_2019_train <- read.csv('data/df_Me_B_SlGe_2019_train_points_scale.csv', header = T)
df_Me_B_SlGe_2019_test <- read.csv('data/df_Me_B_SlGe_2019_test_points_scale.csv', header = T)


df_Me_B_SlGe_2019_train <-select(df_Me_B_SlGe_2019_train, -any_of("X"))
df_Me_B_SlGe_2019_test <-select(df_Me_B_SlGe_2019_test, -any_of("X"))

df_Me_B_SlGe_2019_test <- df_Me_B_SlGe_2019_test[!duplicated(df_Me_B_SlGe_2019_test$FRic), ]

############### DATACAMP
#xgb.cv()
#xgb.cv()$evaluation_log - records estimated RMSE for each round
#run xgboost(), setting nrounds = nbest

cv <- xgb.cv(data  = as.matrix(df_Me_B_SlGe_2019_train[,-c(ncol(df_Me_B_SlGe_2019_train))]),
             label = df_Me_B_SlGe_2019_train[,ncol(df_Me_B_SlGe_2019_train)],
             objective = "reg:squarederror",
             nrounds = 100,
             nfold = 5,
             eta = 0.3,
             max_depth = 6)

elog <- as.data.frame(cv$evaluation_log)
plot(elog$iter, elog$test_rmse_mean)
nrounds <- which.min(elog$test_rmse_mean) # best number of trees

model <- xgboost(data  = as.matrix(df_Me_B_SlGe_2019_train[,-c(ncol(df_Me_B_SlGe_2019_train))]),
                 label = df_Me_B_SlGe_2019_train[,ncol(df_Me_B_SlGe_2019_train)],
                 nrounds = nrounds,
                 objective = "reg:squarederror",
                 eta = 0.3,
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

#################

##OLD

# training data
train_data <- as.matrix(df_Me_B_SlGe_2019_train[,-c(ncol(df_Me_B_SlGe_2019_train))])
train_labels <- df_Me_B_SlGe_2019_train[,ncol(df_Me_B_SlGe_2019_train)]

# testing data
test_data <- as.matrix(df_Me_B_SlGe_2019_test[,-c(ncol(df_Me_B_SlGe_2019_test))])
test_labels <- df_Me_B_SlGe_2019_test[,ncol(df_Me_B_SlGe_2019_train)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label= train_labels)
dtest <- xgb.DMatrix(data = test_data, label= test_labels)

?xgb.DMatrix()

# train a model using our training data
xgb_model <- xgb.train(data = dtrain, 
                       params = list(objective = "reg:squarederror", 
                                     eval_metric = "rmse"), 
                       max.depth = 3, # the maximum depth of each decision tree
                       nrounds = 100, 
                       watchlist = list(train = dtrain, test = dtest), 
                       verbose = 1)

y_pred <- predict(xgb_model, train_data)

y_real <- train_labels

lm3<-lm(y_real~y_pred)
summary(lm3)

plot(y_real,y_pred)

y_pred <- predict(xgb_model, dtest)



params <- list(booster = "gbtree", 
               objective = "reg:squarederror", 
               eta=0.3, 
               gamma=0,
               max_depth=6, 
               min_child_weight=1,
               subsample=1, 
               colsample_bytree=1)

xgbcv <- xgb.cv(params = params,
                data = dtrain,
                nrounds = 100, 
                nfold = 5, 
                showsd = T, 
                stratified = T, 
                print_every.n = 10, 
                early_stop_round = 20, 
                maximize = F)


xgb1 <- xgb.train(params = params, 
                   data = dtrain, 
                   nrounds = 79, 
                   watchlist = list(val=dtest,train=dtrain), 
                   print_every_n = 10, 
                   early_stop_round = 10, 
                   maximize = F , 
                   eval_metric = "rmse")

xgbpred <- predict(xgb1,dtrain)

xgbpred <- predict(xgb1,dtest)


y_real <- train_labels
y_real

lm3<-lm(y_real~xgbpred)
summary(lm3)

rmse_test <- sqrt(mean((xgbpred - y_real)^2))
mae_test <- mean(abs(xgbpred - y_real))

xgbpred

y_real <- test_labels

data<- as.data.frame(cbind(y_real, xgbpred))



plot(xgbpred, y_real, ylim(0,1))

p<- ggplot(data, aes(x=y_real, y=xgbpred))+
  geom_point()

####
nom_mapa<- paste0(dir, "FRic_exp11_2019_2022_04_22_scaled_xgboost3")
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
plot(p)

writeRaster(p, "Fric_xgboost.tif")

result   <- predict(object    = xgb1, 
                    #object    = br_MeSl_2019_c,
                    newdata = train_data
                    )

resultest<- predict(xgb1, test_data)

lm4<- lm(train_labels~result)
summary(lm4)

lm5<- lm(test_labels~resultest)
summary(lm5)

?predict()

#create dummy result raster
res      <- raster(ras)
