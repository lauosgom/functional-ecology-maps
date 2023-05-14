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
  y_pred <- predict.gbm(object  = modelo,
                        newdata = DF[ , 1:ncol(DF)-1],
                        n.trees = modelo$n.tree)
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