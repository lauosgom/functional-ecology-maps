## Install packages
#install.packages('rsample')
#install.packages('gbm')
#install.packages('tidyverse')
#remotes::install_github("r-spatial/sf")
#remotes::install_github("rspatial/terra")
#install.packages('raster')
#install.packages('dismo')

# Required libraries
library(raster)
library(rgdal)
library(rsample)   # initial_split()
library(gbm)
library(dismo)
library(sf)
library(terra)
library(lubridate)
library(mlr)
library(xgboost)
library(tidyverse)

## functions
# The 'cut_raster' function receives a raster and a shapefile and returns a rasterfile clipped by the shapefile
cut_raster <- function(capa, molde) {
  corte <- crop(capa, molde)
  corte <- mask(corte, molde)
  return(corte)
}

# the 'evaluate' function receives a data.frame with thw DF strucure of:
# 'df_Me_B_Ge_2019': {rh95, pred_1, pred_2, ..., pred_n}
# and the adjusted model
# returns a DF wit: {val_real, val_predicho}
evaluate <- function(DF, model) {
  y_real <- DF[ ,ncol(DF)]
  y_pred <- predict.gbm(object  = model,
                        newdata = DF[ , 1:ncol(DF)-1],
                        n.trees = model$n.tree)
  #---
  return(data.frame(Y_real = y_real, Y_hat=y_pred))
}

# the 'scatterplot' function receives the DF with: {val_real, val_predicho},
# and the name of the set of data to evaluate.
# returns a dispersion graph to evaluate the accuracy of the prediction
scatterplot <- function(DF, nom) {
  plot(DF$Y_real,
       DF$Y_hat,
       main = paste0("Scatterplot -", nom, "-"),
       xlab = "Y real",
       ylab = "Y hat",
       pch  = 19,
       cex  = .6,
       xlim = c(0, 100),
       ylim = c(0, 100))
  abline(a = 0, b = 1, col = "red")
}

aggregate_data <- function(data_family, code_plot) {
  # Convert into a dataframe
  data_test<- as.data.frame(data_family)
  
  # Extract the code and time you want to aggregate
  chrono_help <- data_test %>% 
    filter(grepl(code_plot, code))
  
  # Aggregate by taking the max value of the columns
  chrono_new<-as.data.frame(t(as.matrix(colMax(chrono_help))))
  
  for (i in 2:nrow(chrono_help)) {
    chrono_help[i,2:ncol(chrono_help)]<-chrono_new[,-c(1)]
  }
  
  # Remove the code and time from the original database
  data_family <- data_family %>% 
    filter(!str_detect(code, code_plot))
  
  # Paste the row at the end of the database
  data_family <- rbind(data_family,chrono_help)
  
  return(data_family)
}

raster01 = function(r){
  
  # get the min max values
  minmax_r = range(values(r), na.rm=TRUE) 
  
  # rescale 
  return( (r-minmax_r[1]) / (diff(minmax_r)))
}

# names for the rasterbrick
new_names <- c("2021_blue_absdif",
               "2021_blue_av2575",
               "2021_blue_av75max_LST",
               "2021_blue_av75max_RN",
               "2021_blue_av75max_S2N",
               "2021_blue_av75max",
               "2021_blue_avmin25_LST",
               "2021_blue_avmin25_RN",
               "2021_blue_avmin25_S2N",
               "2021_blue_avmin25",
               "2021_blue_avminmax",
               "2021_blue_max_LST",
               "2021_blue_max_RN",
               "2021_blue_max_S2N",
               "2021_blue_max",
               "2021_blue_median",
               "2021_blue_min_LST",
               "2021_blue_min_RN",
               "2021_blue_min_S2N",
               "2021_blue_min",
               "2021_blue_sd",
               "2021_GN_absdif",
               "2021_GN_av2575",
               "2021_GN_av75max",
               "2021_GN_avmin25",
               "2021_GN_avminmax",
               "2021_GN_max",
               "2021_GN_median",
               "2021_GN_min",
               "2021_GN_sd",
               "2021_green_absdif",
               "2021_green_av2575",
               "2021_green_av75max_LST",
               "2021_green_av75max_RN",
               "2021_green_av75max_S2N",
               "2021_green_av75max",
               "2021_green_avmin25_LST",
               "2021_green_avmin25_RN",
               "2021_green_avmin25_S2N",
               "2021_green_avmin25",
               "2021_green_avminmax",
               "2021_green_max_LST",
               "2021_green_max_RN",
               "2021_green_max_S2N",
               "2021_green_max",
               "2021_green_median",
               "2021_green_min_LST",
               "2021_green_min_RN",
               "2021_green_min_S2N",
               "2021_green_min",
               "2021_green_sd",
               "2021_nir_absdif",
               "2021_nir_av2575",
               "2021_nir_av75max_LST",
               "2021_nir_av75max_RN",
               "2021_nir_av75max_S2N",
               "2021_nir_av75max",
               "2021_nir_avmin25_LST",
               "2021_nir_avmin25_RN",
               "2021_nir_avmin25_S2N",
               "2021_nir_avmin25",
               "2021_nir_avminmax",
               "2021_nir_max_LST",
               "2021_nir_max_RN",
               "2021_nir_max_S2N",
               "2021_nir_max",
               "2021_nir_median",
               "2021_nir_min_LST",
               "2021_nir_min_RN",
               "2021_nir_min_S2N",
               "2021_nir_min",
               "2021_nir_sd",
               "2021_red_absdif",
               "2021_red_av2575",
               "2021_red_av75max_LST",
               "2021_red_av75max_RN",
               "2021_red_av75max_S2N",
               "2021_red_av75max",
               "2021_red_avmin25_LST",
               "2021_red_avmin25_RN",
               "2021_red_avmin25_S2N",
               "2021_red_avmin25",
               "2021_red_avminmax",
               "2021_red_max_LST",
               "2021_red_max_RN",
               "2021_red_max_S2N",
               "2021_red_max",
               "2021_red_median",
               "2021_red_min_LST",
               "2021_red_min_RN",
               "2021_red_min_S2N",
               "2021_red_min",
               "2021_red_sd",
               "2021_RN_absdif",
               "2021_RN_av2575",
               "2021_RN_av75max",
               "2021_RN_avmin25",
               "2021_RN_avminmax",
               "2021_RN_max",
               "2021_RN_median",
               "2021_RN_min",
               "2021_RN_sd",
               "2021_RNph_ave",
               "2021_RNph_eos_amp",
               "2021_RNph_eos_slope",
               "2021_RNph_eos",
               "2021_RNph_sos_amp",
               "2021_RNph_sos_slope",
               "2021_RNph_sos",
               "2021_RNph_sum",
               "2021_S1N_absdif",
               "2021_S1N_av2575",
               "2021_S1N_av75max",
               "2021_S1N_avmin25",
               "2021_S1N_avminmax",
               "2021_S1N_max",
               "2021_S1N_median",
               "2021_S1N_min",
               "2021_S1N_sd",
               "2021_S1S2_absdif",
               "2021_S1S2_av2575",
               "2021_S1S2_av75max",
               "2021_S1S2_avmin25",
               "2021_S1S2_avminmax",
               "2021_S1S2_max",
               "2021_S1S2_median",
               "2021_S1S2_min",
               "2021_S1S2_sd",
               "2021_S2N_absdif",
               "2021_S2N_av2575",
               "2021_S2N_av75max",
               "2021_S2N_avmin25",
               "2021_S2N_avminmax",
               "2021_S2N_max",
               "2021_S2N_median",
               "2021_S2N_min",
               "2021_S2N_sd",
               "2021_SVVI_absdif",
               "2021_SVVI_av2575",
               "2021_SVVI_av75max",
               "2021_SVVI_avmin25",
               "2021_SVVI_avminmax",
               "2021_SVVI_max",
               "2021_SVVI_median",
               "2021_SVVI_min",
               "2021_SVVI_sd",
               "2021_swir1_absdif",
               "2021_swir1_av2575",
               "2021_swir1_av75max_LST",
               "2021_swir1_av75max_RN",
               "2021_swir1_av75max_S2N",
               "2021_swir1_av75max",
               "2021_swir1_avmin25_LST",
               "2021_swir1_avmin25_RN",
               "2021_swir1_avmin25_S2N",
               "2021_swir1_avmin25",
               "2021_swir1_avminmax",
               "2021_swir1_max_LST",
               "2021_swir1_max_RN",
               "2021_swir1_max_S2N",
               "2021_swir1_max",
               "2021_swir1_median",
               "2021_swir1_min_LST",
               "2021_swir1_min_RN",
               "2021_swir1_min_S2N",
               "2021_swir1_min",
               "2021_swir1_sd",
               "2021_swir2_absdif",
               "2021_swir2_av2575",
               "2021_swir2_av75max_LST",
               "2021_swir2_av75max_RN",
               "2021_swir2_av75max_S2N",
               "2021_swir2_av75max",
               "2021_swir2_avmin25_LST",
               "2021_swir2_avmin25_RN",
               "2021_swir2_avmin25_S2N",
               "2021_swir2_avmin25",
               "2021_swir2_avminmax",
               "2021_swir2_max_LST",
               "2021_swir2_max_RN",
               "2021_swir2_max_S2N",
               "2021_swir2_max",
               "2021_swir2_median",
               "2021_swir2_min_LST",
               "2021_swir2_min_RN",
               "2021_swir2_min_S2N",
               "2021_swir2_min",
               "2021_swir2_sd",
               "slope")

new_names_vif <- c("2021_blue_absdif",
                  "2021_blue_avmin25_RN",
                  "2021_blue_avmin25_S2N",
                  "2021_blue_avminmax",
                  "2021_blue_max_LST",
                  "2021_blue_max_S2N",
                  "2021_blue_max",
                  "2021_GN_avmin25",
                  "2021_green_absdif",
                  "2021_green_av2575",
                  "2021_green_av75max_LST",
                  "2021_green_av75max_S2N",
                  "2021_green_median",
                  "2021_nir_av2575",
                  "2021_nir_av75max_LST",
                  "2021_nir_av75max",
                  "2021_nir_median",
                  "2021_red_absdif",
                  "2021_red_av75max_RN",
                  "2021_red_min_RN",
                  "2021_RN_median",
                  "2021_RN_sd",
                  "2021_RNph_eos_amp",
                  "2021_S2N_min",
                  "2021_S2N_sd",
                  "2021_SVVI_av75max",
                  "2021_SVVI_median",
                  "2021_swir1_av75max_RN",
                  "2021_swir1_av75max_S2N",
                  "2021_swir1_av75max",
                  "2021_swir1_avmin25_S2N",
                  "2021_swir1_avminmax",
                  "2021_swir1_max_LST",
                  "2021_swir1_max_RN",
                  "slope")
