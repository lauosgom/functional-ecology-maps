# Modelling Boosted Regression Tree after the data engineering is done

# Set working directory
#set directory
setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/") #windows
setwd("/media/lospina/USB/chapter_2") #linux

# Import the required functions
source("github/functional-ecology-maps/feature_engineering/functions.r")

# Read data
data <- read.csv("data_set.csv", header = T)

# read the raster stack generated in the data engineering part
br_MeSl_2019_c <- stack("data/optical/brick_vif.tif")
names(br_MeSl_2019_c) <- new_names_vif

# If the csv got a X column, remove it
data <-select(data, -any_of("X"))

data_split <- initial_split(data, prop = 2/4)

data_train <- training(data_split)
data_test  <- testing(data_split)

write.csv(data_train, "data_train.csv")
write.csv(data_test, "data_test.csv")

colnames(data_train)

#=============< Modelling >==============================================
# ------- Hiper-tunning Modelo inicial ------------------------------

# FJAV: Por la recomendaci?n de Elith et al. 2008, hay que bajar "learning_rate"
#       para alcanzar m?s de 1000 ?rboles (BRT p11)
system.time(
  exp11_mod1_2019 <- gbm.step(data            = data_train, 
                              gbm.x           = 1:35, 
                              gbm.y           = 36, 
                              family          = "gaussian", 
                              tree.complexity = 5,
                              max.trees       = 2500, 
                              learning.rate   = .002, 
                              bag.fraction    = .5
  )
)

# ------- BCKP
saveRDS(exp11_mod1_2019, "exp11_mod1_2019_scale.RDS")

# ------- Reducir predictores ---------------------------------------
system.time(
  exp11_mod2_2019 <- gbm.simplify(exp11_mod1_2019)
)

exp11_mod2_2019 <- readRDS("exp11_mod2_2019.RDS")

# find the best from the predictor reduction
exp11_mod2_2019$deviance.summary <- exp11_mod2_2019$deviance.summary %>%
  filter(mean != 0)

value <- min(abs(exp11_mod2_2019$deviance.summary$mean))

position <- which(exp11_mod2_2019$deviance.summary$mean == value)
position

saveRDS(exp11_mod2_2019, "exp11_mod2_2019_tot_99.RDS")

# ------- Hiper-tunning Modelo final --------------------------------
gbm_x             <- exp11_mod2_2019$pred.list[[99]]
max_trees         <- 5000
tree_complexity   <- 5
learning_rate     <- .002

for (i in position) {
  exp11_mod3_2019 <- gbm.step(data            = df_Me_B_SlGe_2019_train,
                              gbm.x           = exp11_mod2_2019$pred.list[[i]],
                              gbm.y           = 191,
                              family          = "gaussian",
                              max.trees       = max_trees,
                              tree.complexity = tree_complexity,
                              learning.rate   = learning_rate,
                              verbose         = FALSE)
  
  pred_train <- evalua(df_Me_B_SlGe_2019_train, exp11_mod3_2019)
  lm_train <- lm(data = pred_train, Y_real ~ Y_hat)
  rmse_train <- sqrt(mean((pred_train$Y_hat - pred_train$Y_real)^2))
  mae_train <- mean(abs(pred_train$Y_hat - pred_train$Y_real))
  
  print(i)
  print(rmse_train)
  print(mae_train)
  print(summary(lm_train))
}

exp11_mod3_2019 <- gbm.step(data            = df_Me_B_SlGe_2019_train,
                            gbm.x           = exp11_mod2_2019$pred.list[[121]], #here you choose the best model from the loop before # nolint
                            gbm.y           = 191,
                            family          = "gaussian",
                            max.trees       = max.trees,
                            tree.complexity = tree.complexity,
                            learning.rate   = learning.rate)
  
dir <- getwd()
nom_mapa <- paste0(dir, "FRic_exp11_2019_2022_04_22_scaled_121")

exp11_pred_2019_V1 <- raster::predict(object   = br_MeSl_2019_c,
                                      model    = exp11_mod3_2019,
                                      filename = nom_mapa,
                                      progress = "text",
                                      n.trees  = exp11_mod3_2019$n.tree,
                                      type     = "response",
                                      format   = "GTiff",
                                      overwrite = TRUE)

# Evaluation metrics for training data  
pred_train <- evaluate(data_train, exp11_mod1_2019)

rmse_train <- sqrt(mean((pred_train$Y_hat - pred_train$Y_real)^2))
mae_train <- mean(abs(pred_train$Y_hat - pred_train$Y_real))

lm_train <- lm(data = pred_train, Y_real ~ Y_hat)
summary(lm_train)

# Evaluation metrics for testing data
pred_test <- evaluate(data_test, exp11_mod1_2019)

rmse_test <- sqrt(mean((pred_test$Y_hat - pred_test$Y_real)^2))
mae_test <- mean(abs(pred_test$Y_hat - pred_test$Y_real))

lm_test <- lm(data = pred_test, Y_real ~ Y_hat)
summary(lm_test)

# Check the importance of the variables
exp11_mod3_2019_find.int <- gbm.interactions(exp11_mod3_2019)
exp11_mod3_2019$var.names

summary(exp11_mod3_2019_find.int)

