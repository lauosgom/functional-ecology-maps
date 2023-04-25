library(mlflow)
Sys.setenv(MLFLOW_BIN=system("which mlflow", intern=TRUE))
Sys.setenv(MLFLOW_PYTHON_BIN=system("which python3", intern=TRUE))

s <- Sys.getenv()
s[grep("MLFLOW", names(s))]

#set directory
setwd("D:/")
setwd("/media/lospina/USB") #linux

source("functions.R")

#modelling after the data engineering is done

df_Me_B_SlGe_2019_train <- read.csv('df_Me_B_SlGe_2019_train_points_v2.csv', header = T)
df_Me_B_SlGe_2019_train <- df_Me_B_SlGe_2019_train[!duplicated(df_Me_B_SlGe_2019_train$FRic), ]

df_Me_B_SlGe_2019_test <- read.csv('df_Me_B_SlGe_2019_test_points_v2.csv', header = T)
df_Me_B_SlGe_2019_test <- df_Me_B_SlGe_2019_test[!duplicated(df_Me_B_SlGe_2019_test$FRic), ]

br_MeSl_2019_c <- stack('stack2.tiff')

df_Me_B_SlGe_2019_train<- rbind(df_Me_B_SlGe_2019_train, df_Me_B_SlGe_2019_test)

colnames(df_Me_B_SlGe_2019_train)
dim(df_Me_B_SlGe_2019_train)

#=============< Modelaci?n >==============================================
# ------- Hiper-tunning Modelo inicial ------------------------------

# FJAV: Por la recomendaci?n de Elith et al. 2008, hay que bajar "learning_rate"
#       para alcanzar m?s de 1000 ?rboles (BRT p11)
system.time(
  exp11_mod1_2019 <- gbm.step(data            = df_Me_B_SlGe_2019_train, 
                              gbm.x           = 2:192, 
                              gbm.y           = 194, 
                              family          = "gaussian", 
                              #var.monotone = rep(0, 192),
                              tree.complexity = 5,
                              max.trees       = 2500, 
                              learning.rate   = .005, 
                              bag.fraction    = .5
  )
)

# ------- BCKP
saveRDS(exp11_mod1_2019, "exp11_mod1_2019.RDS")

#exp11_mod1_2019$n.tree
#1500

#exp11_mod1_2019 <- readRDS("exp11_mod1_2019.RDS")

# ------- Reducir predictores ---------------------------------------
system.time(
  exp11_mod2_2019 <- gbm.simplify(exp11_mod1_2019)
)

exp11_mod2_2019<- readRDS('exp11_mod2_2019.RDS')

# find the best from the predictor reduction
exp11_mod2_2019$deviance.summary <- exp11_mod2_2019$deviance.summary %>%
  filter(mean != 0) 

value <- min(abs(exp11_mod2_2019$deviance.summary$mean))

position<- which(exp11_mod2_2019$deviance.summary$mean == value)
position

saveRDS(exp11_mod2_2019, 'exp11_mod2_2019_tot_99.RDS')

# ------- Hiper-tunning Modelo final --------------------------------
gbm.x             <- exp11_mod2_2019$pred.list[[99]]
max.trees         <- mlflow_param("max.trees", 5000, "numeric")
tree.complexity   <- mlflow_param("tree.complexity", 5, "numeric")
learning.rate     <- mlflow_param("learning.rate", .002, "numeric")

max.trees <- 5000
tree.complexity <- 5
learning.rate <- .002



for (i in position) {
  exp11_mod3_2019 <- gbm.step(data            = df_Me_B_SlGe_2019_train,
                              gbm.x           = exp11_mod2_2019$pred.list[[i]],
                              gbm.y           = 194,
                              family          = "gaussian",
                              max.trees       = max.trees,
                              tree.complexity = tree.complexity,
                              learning.rate   = learning.rate)
  
  pred_train <- evalua(df_Me_B_SlGe_2019_train, exp11_mod3_2019)
  
  lm_train <- lm(data = pred_train, Y_real ~ Y_hat) 
  
  rmse_train <- sqrt(mean((pred_train$Y_hat - pred_train$Y_real)^2))
  mae_train <- mean(abs(pred_train$Y_hat - pred_train$Y_real))
  
  print(i)
  print(rmse_train)
  print(mae_train)
  
  print(summary(lm_train))
  
  
}


with(mlflow_start_run(), {
  
  #model <- glmnet(train_x, train_y, alpha = alpha, lambda = lambda, family = "gaussian")
  system.time(
    exp11_mod3_2019 <- gbm.step(data            = df_Me_B_SlGe_2019_train,
                                gbm.x           = gbm.x,
                                gbm.y           = 194,
                                family          = "gaussian",
                                max.trees       = max.trees,
                                tree.complexity = tree.complexity,
                                learning.rate   = learning.rate)
    )
  
  dir<- getwd()
  dir<- setwd('/home/lospina/Desktop')
  nom_mapa<- paste0(dir, "FRic_exp11_2019_2022_04_22_tot_99")
  
  #predicted <- glmnet::predict(model, train_x)
  exp11_pred_2019_V1 <- raster::predict(object   = br_MeSl_2019_c,
                                        model    = exp11_mod3_2019,
                                        filename = nom_mapa,
                                        progress = "text",
                                        n.trees  = exp11_mod3_2019$n.tree,
                                        type     = "response",
                                        format   = "GTiff",
                                        overwrite = TRUE)
  
  pred_test <- evalua(df_Me_B_SlGe_2019_test, exp11_mod3_2019)
  pred_train <- evalua(df_Me_B_SlGe_2019_train, exp11_mod3_2019)
  
  rmse_test <- sqrt(mean((pred_test$Y_hat - pred_test$Y_real)^2))
  mae_test <- mean(abs(pred_test$Y_hat - pred_test$Y_real))
  r2_test <- as.numeric(cor(pred_test$Y_hat - pred_test$Y_real) ^ 2)
  
  lm <- lm(data = pred_test, Y_real ~ Y_hat) 
  summary(lm)

  
  lm_train <- lm(data = pred_train, Y_real ~ Y_hat) 
  summary(lm_train)
  View(pred_train)
  
  plot(pred_train$Y_real, pred_train$Y_hat)
  
  rmse_train <- sqrt(mean((pred_train$Y_hat - pred_train$Y_real)^2))
  mae_train <- mean(abs(pred_train$Y_hat - pred_train$Y_real))
  rmse_train
  mae_train
  
  r2_train <- as.numeric(cor(pred_train$Y_hat - pred_train$Y_real) ^ 2)
    
  mlflow_log_param("max.trees", max.trees)
  mlflow_log_param("tree.complexity", tree.complexity)
  mlflow_log_param("learning.rate", learning.rate)
  
  mlflow_log_metric("rmse_test", rmse_test)
  mlflow_log_metric("mae_test", mae_test)
  
  mlflow_log_metric("rmse_train", rmse_train)
  mlflow_log_metric("mae_train", mae_train)
  
})

mlflow_ui()

saveRDS(exp11_mod3_2019, "model_points_v2.RDS")

exp11_mod3_2019 <- readRDS('model_points.RDS')

#=============< Descripci?n >=============================================
# Display the relative influence of each variable as a table


exp11_mod3_2019_find.int <- gbm.interactions(exp11_mod3_2019)
exp11_mod3_2019$var.names

summary(exp11_mod3_2019_find.int)



class(br_MeSl_2019_c)
test <-(br_MeSl_2019_c@layers)
class(br_MeSl_2019_c@layers)

test



#=============< Predicci?n >==============================================
exp11_pred_2019_V1 <- raster::predict(object   = br_MeSl_2019_c,
                                      model    = exp11_mod3_2019,
                                      filename = nom_mapa,
                                      progress = "text",
                                      n.trees  = exp11_mod3_2019$n.tree,
                                      type     = "response",
                                      format   = "GTiff",
                                      overwrite = TRUE)
?raster::predict


# ------- BCKP
saveRDS(exp11_pred_2019_V1, "exp11_pred_2019.RDS")

exp11_pred_2019 <- readRDS("exp11_pred_2019.RDS")


par(mfcol=c(1, 2))
# ------- 1. S?lo para test set --------------------------------


df_Me_B_SlGe_2019_test<- read.csv("df_Me_B_SlGe_2019_test_points.csv",header = T)
df_Me_B_SlGe_2019_train<- read.csv("df_Me_B_SlGe_2019_train_points.csv",header = T)

pred_test <- evalua(df_Me_B_SlGe_2019_test, exp11_mod3_2019)

p.test    <- scatterplot(pred_test, "TEST")

# ------- 2. S?lo para el train set ----------------------------
pred_train <- evalua(df_Me_B_SlGe_2019_train, exp11_mod3_2019)
p.train    <- scatterplot(pred_train, "TRAIN")

# ------- Im?gen -----------------------------------------------
par(mfcol=c(1, 1))

View(df_Me_B_SlGe_2019_test)

y_real <- df_Me_B_SlGe_2019_train %>%
  select(FRic)
y_pred <- predict.gbm(object  = exp11_mod3_2019,
                      newdata = df_Me_B_SlGe_2019_train %>%
                        select(-any_of('FRic')),
                      n.trees = exp11_mod3_2019$n.tree)
#---
pred_trin <- data.frame(Y_real=y_real, Y_hat=y_pred)
}


y_real <- df_Me_B_SlGe_2019_test %>%
  select(FRic)
y_pred <- predict.gbm(object  = exp11_mod3_2019,
                      newdata = df_Me_B_SlGe_2019_test %>%
                        select(-any_of('FRic')),
                      n.trees = exp11_mod3_2019$n.tree)
#---
pred_test <- data.frame(Y_real=y_real, Y_hat=y_pred)

summary(pred_test)
View(pred_test)

plot(pred_test$FRic, pred_test$Y_hat)
plot(pred_trin$FRic, pred_trin$Y_hat)


plot(pred_test$FRic, 
     pred_test$Y_hat, 
     main = paste0("Scatterplot -", nom, "-"),
     xlab = "Y real", 
     ylab = "Y hat", 
     pch  = 19,
     cex  = .6,
     xlim = c(0, 1), 
     ylim = c(0, 1))
abline(a=0, b=1, col="red")

model<- lm(data= pred_trin, FRic~Y_hat)

model2<- lm(data= pred_test, FRic~Y_hat)
summary(model2)

write.csv(pred_test, "pred_test_v2.csv")

write.csv(pred_trin, "pred_test_v2.csv")