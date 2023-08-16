library(dismo)        # gbm.step()
library(gbm)          # gbm.simplify()
library(lightgbm)     # 
library(sf)           # st_as_sf()
library(stringr)      # str_replace()
library(randomForest) # randomForest(), tuneRF()
library(rsample)      # initial_split(), training(), testing()
library(tidyverse)
library(terra)        # crop(), extract(), mosaic(), rast(), vect()
library(xgboost)      # xgb.cv(), xgboost()
library(raster)
library(rgdal)


terraOptions(memmax=35.5, tempdir = "/media/laura/easystore/temp/")
dir <- "/media/laura/easystore/temp/"
tempdir()
options(tempdir = dir)
Sys.setenv(TMPDIR = dir)
options(rasterTmpDir = dir)

setwd("/media/laura/USB/chapter_2/")

#directories
dirmet_scaled           <- "data/optical/metrics_scaled/"
dirmet_merged           <- "data/optical/metricas_merge/"
points_slm              <- "data/points/field/results_fd_agg_XYTableToPoint.shp" # for points
zonagdb                 <- "data/area/no_water/area_slm3.shp"
dem_dir                 <- "data/dem/dem_cut.tif"
slope_dir               <- "data/dem/slope_glad_scaled.tif"
aspect_dir               <- "data/dem/aspect_scaled.tif"

#read the shapefiles - study area and plot points
sh_points   <- readOGR(points_slm)
zona        <- readOGR(zonagdb)
zona        <- spTransform(zona, '+proj=longlat +datum=WGS84 +no_defs')

#rasterstack with the quality layers
ar_tec      <- list.files(dirmet_merged, pattern = "_TEC_")
ar_tec      <- paste0(dirmet_merged, "/", ar_tec)
br_tec      <- rast(ar_tec)

#crop the water
br_tec <- terra::crop(br_tec, zona)

#evaluate quality Pheno_C_V2 in the points
df_tec      <- terra::extract(br_tec, sh_points)

names(df_tec) <- c('ID', 'TEC_count', 'TEC_pf', 'TEC_prcwater')
names(df_tec) <- paste0("v", names(df_tec))

#adjustment
threshold <- 7 #anything higher than 7 is acceptable
df_tec[ , 2][df_tec[ , 2] < threshold] <- NA
id_na_1 <- which(is.na(df_tec[ , 2]))

#data quality flag
df_tec[ , 3][df_tec[ , 3] != 1] <- NA
id_na_2 <- which(is.na(df_tec[ , 3]))

# water percentage in observations
threshold <- 200 #200 and less is acceptable
df_tec[ , 4][df_tec[ , 4] > threshold] <- NA
id_na_3 <- which(is.na(df_tec[ , 4]))

id_na <- unique(c(id_na_1, id_na_2, id_na_3))    

#topography  
slope  <- raster(slope_dir)
dem    <- raster(dem_dir)
aspect <- raster(aspect_dir)

br_top <- stack(slope, dem, aspect)

#functions - move later
Nombres_metricas    <- function(metClass, statGroup="Tend") { 
  
  
  
  df_NomMetPhe     <- read.csv('/media/laura/USB/chapter_2/data/combinations/Met_PhenoC_groups.csv', header = T) 
  
  ve_Set1          <- c("Set_1", "Set_2", "Set_3") 
  
  ve_Set2          <- c("Sets_1_2", "Sets_1_3", "Sets_2_3") 
  
  
  
  # sii una sóla Clase de Métrica ------------------ 
  
  if (metClass %in% ve_Set1) { 
    
    if (metClass != "Set_3") { 
      
      if (statGroup=="All") { 
        
        ve_Nombres <- subset(df_NomMetPhe, Class_Metric==metClass)$VAR 
        
      } else { 
        
        ve_Nombres <- subset(df_NomMetPhe, Class_Metric==metClass &  
                               
                               Statistical_group==statGroup)$VAR 
        
      } 
      
    } else { 
      
      ve_Nombres <- subset(df_NomMetPhe, Class_Metric==metClass)$VAR 
      
    } 
    
  } else { 
    
    
    
    # sii dos Clases de Métricas ----------------- 
    
    if (metClass %in% ve_Set2) { 
      
      
      
      ch_6 <- substr(metClass,  6, 6) 
      
      ch_8 <- substr(metClass,  8, 8) 
      
      
      
      if (statGroup[1]=="All") { 
        
        ve_Nombres1 <- subset(df_NomMetPhe,  
                              
                              Class_Metric==paste0("Set_", ch_6))$VAR 
        
      } else { 
        
        ve_Nombres1 <- subset(df_NomMetPhe,  
                              
                              Class_Metric==paste0("Set_", ch_6) &  
                                
                                Statistical_group==statGroup[1])$VAR 
        
      } 
      
      if (ch_8=="2") { 
        
        if (statGroup[2]=="All") { 
          
          ve_Nombres2 <- subset(df_NomMetPhe,  
                                
                                Class_Metric=="Set_2")$VAR 
          
        } else { 
          
          ve_Nombres2 <- subset(df_NomMetPhe,  
                                
                                Class_Metric=="Set_2" &  
                                  
                                  Statistical_group==statGroup[2])$VAR 
          
        } 
        
      } else { 
        
        ve_Nombres2 <- subset(df_NomMetPhe,  
                              
                              Class_Metric=="Set_3")$VAR 
        
      } 
      
      ve_Nombres <- c(ve_Nombres1, ve_Nombres2) 
      
    } else { 
      
      
      
      # sii las tres Clases de Métricas -------- 
      
      if (metClass =="Sets_All") { 
        
        if (statGroup[1]=="All") { 
          
          ve_Nombres1 <- subset(df_NomMetPhe,  
                                
                                Class_Metric=="Set_1")$VAR 
          
        } else { 
          
          ve_Nombres1 <- subset(df_NomMetPhe,  
                                
                                Class_Metric=="Set_1" &  
                                  
                                  Statistical_group==statGroup[1])$VAR 
          
        } 
        
        
        
        if (statGroup[2]=="All") { 
          
          ve_Nombres2 <- subset(df_NomMetPhe,  
                                
                                Class_Metric=="Set_2")$VAR 
          
        } else { 
          
          ve_Nombres2 <- subset(df_NomMetPhe,  
                                
                                Class_Metric=="Set_2" &  
                                  
                                  Statistical_group==statGroup[2])$VAR 
          
        } 
        
        
        
        ve_Nombres3 <- subset(df_NomMetPhe,  
                              
                              Class_Metric=="Set_3")$VAR 
        
        
        
        ve_Nombres <- c(ve_Nombres1, ve_Nombres2, ve_Nombres3) 
        
        
        
      } else { 
        
        print("ERROR en la especifición de la Clase de Métrica fenológica que se utilizará.") 
        
        ve_Nombres <- NA 
        
      } 
      
    } 
    
  } 
  
  #--- 
  
  return(ve_Nombres) 
  
} 
Nombres_metricas_LO <- function(nombre, Cant="22") { 
  
  
  
  df_NomMetPhe     <- read.csv('/media/laura/USB/chapter_2/data/combinations/Met_PhenoC_groups.csv', header = T) 
  
  ve_CorrVar       <- c("LST", "RN", "S2N") 
  
  
  
  if (nombre %in% ve_CorrVar) { 
    
    if (Cant=="All") { 
      
      ve_Nombres <- subset(df_NomMetPhe, CorrespVAR==nombre)$VAR 
      
    } else { 
      
      ve_Nombres <- subset(df_NomMetPhe, CorrespVAR==nombre & 
                             
                             Statistical_group==Cant)$VAR 
      
    } 
    
  } else { 
    
    nom_Col    <- ifelse(Cant=="10", "GLO_10_3", "GLO_22_3") 
    
    ve_Nombres <- subset(df_NomMetPhe, df_NomMetPhe[[nom_Col]]==nombre)$VAR 
    
  } 
  
  #--- 
  
  return(ve_Nombres) 
  
} 
Nombres_metricas_2  <- function(var_Ind, statGroup="Tend") { 
  
  # agno="2020"     var_Ind="blue"    statGroup="Disp" 
  
  
  
  df_NomMetPhe     <- read.csv('/media/laura/USB/chapter_2/data/combinations/Met_PhenoC_groups.csv', header = T) 
  
  
  
  if (statGroup=="All") { 
    
    ve_Nombres <- subset(df_NomMetPhe, IndexVAR==var_Ind)$VAR 
    
  } else { 
    
    ve_Nombres <- subset(df_NomMetPhe, IndexVAR==var_Ind & 
                           
                           Statistical_group==statGroup)$VAR 
    
  } 
  
  #--- 
  
  return(ve_Nombres) 
  
}  

brt_model           <- function(data_train, data_test, number_trees  = 2500, learning_rate = .002, predictor, response){ 
  
  
  
  mod_1 <- gbm.step(data            = data_train,  
                    
                    gbm.x           = predictor,  
                    
                    gbm.y           = response,  
                    
                    family          = "gaussian",  
                    
                    tree.complexity = 5, 
                    
                    max.trees       = number_trees,  
                    
                    learning.rate   = learning_rate,  
                    
                    bag.fraction    = .5, 
                    
                    verbose         = FALSE) 
  
  
  
  mod_2 <- gbm.simplify(mod_1) 
  
  
  
  value  <- mod_2$deviance.summary  %>% 
    
    filter(mean != 0) %>% 
    
    arrange(abs(mean), se) 
  
  value <- value[1, ] 
  
  
  
  position <- which(mod_2$deviance.summary$mean == value$mean)[1] 
  
  
  
  
  
  for (j in position){ #---------- 
    
    mod_3 <- gbm.step(data            = data_train, 
                      
                      gbm.x           = mod_2$pred.list[[j]], 
                      
                      gbm.y           = response, 
                      
                      family          = "gaussian", 
                      
                      max.trees       = number_trees, 
                      
                      tree.complexity = 5, 
                      
                      learning.rate   = learning_rate, 
                      
                      verbose         = FALSE) 
    
  } #Siguiente i ----------------- 
  
  
  
  y_real_training <- data_train[ , response] 
  
  y_pred_training <- predict.gbm(object  = mod_3, 
                                 
                                 newdata = data_train[ , predictor], 
                                 
                                 n.trees = mod_3$n.tree) 
  
  
  
  y_real_test <- data_test[ , response] 
  
  y_pred_test <- predict.gbm(object  = mod_3, 
                             
                             newdata = data_test[,predictor], 
                             
                             n.trees = mod_3$n.tree) 
  
  
  
  lm_train   <- lm(y_real_training ~ y_pred_training) 
  
  rmse_train <- sqrt(mean((y_real_training - y_pred_training)^2)) 
  
  mae_train  <- mean(abs(y_real_training - y_pred_training)) 
  
  
  
  lm_test   <- lm(y_real_test ~ y_pred_test) 
  
  rmse_test <- sqrt(mean((y_real_test - y_pred_test)^2)) 
  
  mae_test  <- mean(abs(y_real_test - y_pred_test)) 
  
  
  
  #--- 
  
  return(data.frame(RMSE_train = rmse_train,  
                    
                    RMSE_test  = rmse_test, 
                    
                    MAE_train  = mae_train, 
                    
                    MAE_test   = mae_test, 
                    
                    R2adj_trai = summary(lm_train)$adj.r.squared, 
                    
                    R2adj_test = summary(lm_test)$adj.r.squared, 
                    
                    RSE_train  = summary(lm_train)$sigma, 
                    
                    RSE_test   = summary(lm_test)$sigma)) 
  
} 
rf_model            <- function(data_train, data_test, number_trees  = 2500, learning_rate = .002){ 
  
  
  
  data_train_RF <- data_train[ , c(Var_Obj, c_ini:ncol(df_FDV_Pred))] 
  
  data_test_RF  <- data_test[ , c(Var_Obj, c_ini:ncol(df_FDV_Pred))] 
  
  
  
  #Applying mtry to get optimum number of trees 
  
  mtry <- tuneRF(data_train_RF[, -1], 
                 
                 data_train_RF[, 1], 
                 
                 ntreeTry   = number_trees, 
                 
                 stepFactor = 1.5, 
                 
                 improve    = 0.01, 
                 
                 trace      = TRUE, 
                 
                 plot       = TRUE) 
  
  
  
  best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1] 
  
  
  
  rf <- randomForest(FRic ~ ., 
                     
                     data       = data_train_RF, 
                     
                     mtry       = best_m, 
                     
                     importance = TRUE, 
                     
                     ntree      = number_trees) 
  
  
  
  y_real_training <- data_train_RF[ , 1] 
  
  y_pred_training <- predict(object  = rf, 
                             
                             newdata = data_train_RF[ , -1], # nolint 
                             
                             n.trees = rf$ntree) 
  
  
  
  y_real_test <- data_test_RF[ , 1] 
  
  y_pred_test <- predict(object  = rf, 
                         
                         newdata = data_test_RF[ , -1], 
                         
                         n.trees = rf$ntree) 
  
  
  
  lm_train   <- lm(y_real_training ~ y_pred_training) 
  
  rmse_train <- sqrt(mean((y_real_training - y_pred_training)^2)) 
  
  mae_train  <- mean(abs(y_real_training - y_pred_training)) 
  
  
  
  lm_test   <- lm(y_real_test ~ y_pred_test) 
  
  rmse_test <- sqrt(mean((y_real_test - y_pred_test)^2)) 
  
  mae_test  <- mean(abs(y_real_test - y_pred_test)) 
  
  
  
  #--- 
  
  return(data.frame(RMSE_train = rmse_train,  
                    
                    RMSE_test  = rmse_test, 
                    
                    MAE_train  = mae_train, 
                    
                    MAE_test   = mae_test, 
                    
                    R2adj_trai = summary(lm_train)$adj.r.squared, 
                    
                    R2adj_test = summary(lm_test)$adj.r.squared, 
                    
                    RSE_train  = summary(lm_train)$sigma, 
                    
                    RSE_test   = summary(lm_test)$sigma)) 
  
} 
xgboost_model       <- function(data_train, data_test, predictor, response){ 
  
  
  
  # Cross validation to find the number of trees 
  
  cv <- xgb.cv(data      = as.matrix(data_train[ , predictor]), 
               
               label     = data_train[ , c(response)], 
               
               objective = "reg:squarederror", 
               
               nrounds   = 1000, 
               
               nfold     = 5, 
               
               eta       = 0.02, 
               
               max_depth = 6,  
               
               verbose   = FALSE) 
  
  
  
  elog    <- as.data.frame(cv$evaluation_log) 
  
  plot(elog$iter, elog$test_rmse_mean) 
  
  nrounds <- which.min(elog$test_rmse_mean) # best number of trees 
  
  
  
  model <- xgboost(data      = as.matrix(data_train[ ,predictor]), 
                   
                   label     = data_train[, c(response)], 
                   
                   nrounds   = nrounds, 
                   
                   objective = "reg:squarederror", 
                   
                   eta       = 0.02, 
                   
                   max_depth = 6, 
                   
                   verbose   = 0) 
  
  
  
  y_real_training <- as.matrix(data_train[, c(response)]) 
  
  y_pred_training <- predict(model, as.matrix(data_train[, c(predictor)])) 
  
  
  
  lm_train   <- lm(y_real_training ~ y_pred_training) 
  
  rmse_train <- sqrt(mean((y_real_training - y_pred_training)^2)) 
  
  mae_train  <- mean(abs(y_real_training - y_pred_training)) 
  
  
  
  y_real_test <- as.matrix(data_test[, c(response)]) 
  
  y_pred_test <- predict(model, as.matrix(data_test[, c(predictor)])) 
  
  
  
  lm_test   <- lm(y_real_test ~ y_pred_test) 
  
  rmse_test <- sqrt(mean((y_real_test - y_pred_test)^2)) 
  
  mae_test  <- mean(abs(y_real_test - y_pred_test)) 
  
  
  
  #--- 
  
  return(data.frame(RMSE_train = rmse_train,  
                    
                    RMSE_test  = rmse_test, 
                    
                    MAE_train  = mae_train, 
                    
                    MAE_test   = mae_test, 
                    
                    R2adj_trai = summary(lm_train)$adj.r.squared, 
                    
                    R2adj_test = summary(lm_test)$adj.r.squared, 
                    
                    RSE_train  = summary(lm_train)$sigma, 
                    
                    RSE_test   = summary(lm_test)$sigma)) 
  
} 

### ---------------< 5**.  Seleccionar un grupo de métricas a utilizar  - i - 
# Seleccionar el grupo de métricas a utilizar:: PROCESO ITERATIVO 
# Anida los pasos 6 a 16. 

#Leer la BD con las 108 combinaciones 
Var_Obj <- 2
table_combinations <- read.csv("/media/laura/USB/chapter_2/data/combinations/combinations_selection_metrics.csv", header = T) 
table_results <- read.csv("/media/laura/USB/chapter_2/data/results/table_comparison_i_k.csv")
table_results <- table_results[,-1]
  
#table_results <- data.frame(1) 
name_table   <- "/media/laura/USB/chapter_2/data/results/table_comparison_i_k.csv" 


for (i in 1:108) {

  print(i)
  fu_Com <- table_combinations$Funcion[i] 
  pa2_Fu <- table_combinations$Param_2[i] 
  pa3_Fu <- table_combinations$Param_3[i]
  
  aux_pa2 <- c("Sets_1_2", "Sets_All")
  if (pa2_Fu %in% aux_pa2) {
  
    ch_1 <- substr(pa3_Fu, 1, 1)
    ch_3 <- substr(pa3_Fu, 3, 3)
    
    pa3_Fu_a <- ifelse(ch_1 == "1", "Disp", ifelse(ch_1 == "2", "Tend", "All"))
    pa3_Fu_b <- ifelse(ch_3 == "1", "Disp", ifelse(ch_3 == "2", "Tend", "All"))
    pa3_Fu <- c(pa3_Fu_a, pa3_Fu_b)
    
  }
    
  ve_metSel <- switch(fu_Com,
                      "Nombres_metricas" = Nombres_metricas(pa2_Fu, pa3_Fu),
                      "Nombres_metricas_LO" = Nombres_metricas_LO(pa2_Fu, pa3_Fu),
                      Nombres_metricas_2(pa2_Fu, pa3_Fu)
  )

  rm(aux_pa2, fu_Com, pa2_Fu, pa3_Fu)
  
  ### -----------< 6*. Leer las métricas seleccionadas 
  ar_Met_files      <- paste0(dirmet_scaled, ve_metSel)
  ar_Met            <- lapply(ar_Met_files, raster) 
  #br_Met            <- terra::rast(ar_Met)
  br_Met            <- brick(ar_Met)
  names(br_Met)     <- ve_metSel
  br_Met            <- terra::crop(br_Met, zona)
  
  ### -----------< 7.  Hacer un Brick con las métricas y las Topografías 
  #br_Pred <- terra::merge(br_Met, slope)
  br_Pred <- stack(br_Met, br_top) 
  
  ### -----------< 8.  Extraer los datos del Brick para los puntos del Shape 
  #--- Hacer la extracción 
  df_Pred        <- terra::extract(br_Pred, sh_points) 
  colnames(df_Pred) <- paste0("V_", colnames(df_Pred)) 
  
  rm(ar_Met, br_Met, br_Pred)
  
  #--- Ensamblar el dataframe para los modelos 
  df_FDV      <- st_as_sf(sh_points) 
  df_FDV_Pred <- cbind(as.data.frame(df_FDV)[ , -length(df_FDV)], df_Pred)
  
  if ('V_X2021_RN_sd.tif' %in% colnames(df_FDV_Pred)){
    df_FDV_Pred <- select(df_FDV_Pred, -('V_X2021_RN_sd.tif'))
  }
  
  c_ini       <- ncol(df_FDV)
  
  ### -----------< 9. **Ajustar 
  #df_FDV_Pred <- df_FDV_Pred[-id_na, ]
  
  # Primer modelo, BRT --------------------------------------- 
  
  set.seed(4326) 
  data_split  <- initial_split(df_FDV_Pred, prop = 3/4) 
  data_train  <- training(data_split) 
  data_test   <- testing(data_split) 
  
  results_brt <- brt_model(data_train,  
                           data_test, 
                           predictor = c_ini:ncol(df_FDV_Pred),  
                           response  = Var_Obj) 
  
  results_brt$SubMet_Mod <- paste0(i, "_BRT") 
  
  if (ncol(table_results) == 1) {
    table_results <- results_brt
  } else {
    table_results <- rbind(table_results, results_brt)
  }
  
  # Segundo modelo, RF --------------------------------------- 
  set.seed(4326) 
  results_rf            <- rf_model(data_train, data_test) 
  results_rf$SubMet_Mod <- paste0(i, "_RF") 
  table_results         <- rbind(table_results, results_rf) 
  
  # Tercer modelo, XGB --------------------------------------- 
  set.seed(4326) 
  results_xgb <- xgboost_model(data_train,
                               data_test, 
                               predictor = c_ini:ncol(df_FDV_Pred), 
                               response  = Var_Obj) 
  
  results_xgb$SubMet_Mod <- paste0(i, "_XGB") 
  table_results              <- rbind(table_results, results_xgb) 
  
  
  if (i == 1) {
    write.csv(table_results, nom_Tab)
  } else {
    write.table(c(results_brt, results_rf, results_xgb), name_table, append = TRUE, row.names = FALSE, col.names = FALSE, sep = ",")
  }
}



##########
