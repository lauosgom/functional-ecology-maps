# Functional Ecology maps
This repository is under construction. It belongs to one of the chapters for my masters thesis where I use field and GEDI data together with the GLAD metrics to fit models and predict the functional diversity across a mountain range located in Guatemala. Other contributors are Francisco Alvarez-Vargas and Carla Restrepo

# Data sources

## Field data
We collected data on the field from the period 2010-2019 and added data from other work developed in the area by other authors 

## GEDI data
https://gedi.umd.edu

## GLAD data
Global Land Analysis and Discovery from the University of Maryland https://glad.umd.edu/dataset

# Modelling
We fitted a Boosted Regression Tree, Random Forest and XGBoost using the different functional metrics (FRic, FEve, FDiv and FDis) as response variables and the 192 GLAD metrics Pheno-c + slope as predictors. You can find the code for each model, evaluation metrics and prediction in the Models folder

