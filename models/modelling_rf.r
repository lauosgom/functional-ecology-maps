library(tidyverse)
library(randomForest)
library(caret)

#Build a random forest model
rf <- randomForest(FRic ~ .,
                  data  = df_Me_B_SlGe_2019_train,
                  ntree = 1000)

print(rf)
plot(rf)
importance(rf)
varImpPlot(rf)

#Applying mtry to get optimum number of trees
mtry <- tuneRF(df_Me_B_SlGe_2019_train[, - c(191)],
               df_Me_B_SlGe_2019_train$FRic,
               ntreeTry   = 500,
               stepFactor = 1.5,
               improve    = 0.01,
               trace      = TRUE,
               plot       = TRUE)


best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]

#Build a random forest model with the best mtry
rf <- randomForest(FRic ~ .,
                  data       = df_Me_B_SlGe_2019_train,
                  mtry       = best.m,
                  importance = TRUE,
                  ntree      = 500)

print(rf)
varImpPlot(rf)

#Model with Important variables
rf_imp <- randomForest(FRic ~ layer.13 + layer.110 + layer.107 + layer.102 + layer.6 + layer.171 + layer.172,
                       data  = df_Me_B_SlGe_2019_train,
                       ntree = 500)
plot(rf_imp)
print(rf_imp)

importance(rf)
varImpPlot(rf)

#Predicting output for training data
y_pred <- predict(object  = rf_imp,
                  newdata = df_Me_B_SlGe_2019_train[ , 1:ncol(df_Me_B_SlGe_2019_train)-1], # nolint
                  n.trees = rf_imp$ntree)

y_real <- df_Me_B_SlGe_2019_train[, ncol(df_Me_B_SlGe_2019_train)]

plot(y_real, y_pred)
lm2 <- lm(y_pred ~ y_real)
summary(lm2)

rmse_train <- sqrt(mean((y_pred - y_real)^2))
mae_train <- mean(abs(y_pred - y_real))

#Predicting output for testing data

y_pred <- predict(object  = rf_imp,
                  newdata = df_Me_B_SlGe_2019_test[ , 1:ncol(df_Me_B_SlGe_2019_test)-1], # nolint
                  n.trees = modelo$ntree)

y_real <- df_Me_B_SlGe_2019_test[, ncol(df_Me_B_SlGe_2019_test)]

plot(y_real, y_pred)
lm2 <- lm(y_pred ~ y_real)
summary(lm2)

rmse_test <- sqrt(mean((y_pred - y_real)^2))
mae_test <- mean(abs(y_pred - y_real))

# Predicting output for the whole map
nom_mapa<- paste0(dir, "FRic_exp11_2019_2022_04_22_scaled_rf")

exp11_pred_2019_V1 <- raster::predict(object   = br_MeSl_2019_c,
                                      model    = rf_imp,
                                      filename = nom_mapa,
                                      progress = "text",
                                      n.trees  = rf_imp$ntree,
                                      type     = "response",
                                      format   = "GTiff",
                                      overwrite = TRUE)