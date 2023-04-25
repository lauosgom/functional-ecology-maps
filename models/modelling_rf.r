library(tidyverse)
library(randomForest)
library(caret)

#Build a random forest model
rf<- randomForest(AdoptionSpeed~.-Name-RescuerID-PetID-Description, 
                  data=train, 
                  ntree=100)

print(rf)
plot(rf)
importance(rf)
varImpPlot(rf)

#Applying mtry to get optimum number of trees
mtry <- tuneRF(train_data[-20], 
               train_data$AdoptionSpeed, 
               ntreeTry=100, 
               stepFactor=1.5, 
               improve=0.01, 
               trace=TRUE, 
               plot=TRUE)


best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)

#Build a random forest model with the best mtry
rf <-randomForest(AdoptionSpeed~.-Name-RescuerID-PetID-Description, 
                  data=train, 
                  mtry=best.m, 
                  importance=TRUE, 
                  ntree=100)

print(rf)
varImpPlot(rf)

#Model with Important variables
rf_imp <- randomForest(AdoptionSpeed~nlen_Desc+Age+PhotoAmt+Breed1+Color2+Color1+StateName, data=train, ntree=100,mtry =best.m)
plot(rf_imp)

#Predicting output
pred <- predict(rf_imp, newdata=test)
head(pred)