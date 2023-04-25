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
library(tidyverse)

## functions

#=============< Funciones propias F Alvarez>=======================================
Cortar_Raster <- function(capa, molde) {
  corte <- crop(capa, molde)
  corte <- mask(corte, molde)
  return(corte)
}

# La funci?n 'evalua' recibe un data.frame con la estructura del DF 
# 'df_Me_B_Ge_2019': {rh95, pred_1, pred_2, ..., pred_n}
# y recibe tambi?n el modelo ajustado.
# Retorna un DF con: {val_real, val_predicho}
evalua <- function(DF, modelo) {
  y_real <- DF[ ,ncol(DF)]
  y_pred <- predict.gbm(object  = modelo,
                        newdata = DF[ , 1:ncol(DF)-1],
                        n.trees = modelo$n.tree)
  #---
  return(data.frame(Y_real=y_real, Y_hat=y_pred))
}

# La funci?n 'scatterplot' recibe el DF con: {val_real, val_predicho},
# y el nombre del conjunto de puntos a evaluar.
# Retorna una gr?fica de dispersi?n para evaluar la precisi?n de la
# predicci?n
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
  abline(a=0, b=1, col="red")
}

# Rescale the raster data
rescale01 <- function(x) {
  
  val <- values(x)
  
  values(x) <- (val - min(val, na.rm = TRUE)) / (max(val, na.rm = TRUE) - min(val, na.rm = TRUE))
  
  x
}