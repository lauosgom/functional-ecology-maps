install.packages("SpatialML")
library(SpatialML)

data_train
data_test

data <- read.csv("data_set.csv", header = T)

# read the raster stack generated in the data engineering part
br_MeSl_2019_c <- stack("data/optical/brick_vif.tif")
names(br_MeSl_2019_c) <- new_names_vif

# If the csv got a X column, remove it
data <-select(data, -any_of("X"))

data_split <- initial_split(data, prop = 3/4)

data_train <- training(data_split)
data_test  <- testing(data_split)
colnames(data_train)

Coords <- data_train[ ,40:41]

excluded_vars <- c("FDiv", "FEve", "FDis", "HEIGHT", 'x','y')

data_train <-select(data_train, -any_of(excluded_vars))

# Load example data (replace with your own data)
data(meuse)
coordinates(meuse) <- ~x+y

results <- rf.mtry.optim(FRic ~ X2021_blue_absdif+
                           X2021_blue_avmin25_RN+
                           X2021_blue_avmin25_S2N+
                           X2021_blue_avminmax+
                           X2021_blue_max_LST+
                           X2021_blue_max_S2N+
                           X2021_blue_max+
                           X2021_GN_avmin25+
                           X2021_green_absdif+
                           X2021_green_av2575+
                           X2021_green_av75max_LST+
                           X2021_green_av75max_S2N+
                           X2021_green_median+
                           X2021_nir_av2575+
                           X2021_nir_av75max_LST+
                           X2021_nir_av75max+
                           X2021_nir_median+
                           X2021_red_absdif+
                           X2021_red_av75max_RN+
                           X2021_red_min_RN+
                           X2021_RN_median+
                           X2021_RN_sd+
                           X2021_RNph_eos_amp+
                           X2021_S2N_min+
                           X2021_S2N_sd+
                           X2021_SVVI_av75max+
                           X2021_SVVI_median+
                           X2021_swir1_av75max_RN+
                           X2021_swir1_av75max_S2N+
                           X2021_swir1_av75max+
                           X2021_swir1_avmin25_S2N+
                           X2021_swir1_avminmax+
                           X2021_swir1_max_LST+
                           X2021_swir1_max_RN+
                           slope, data_train)


# Fit a random forest model
results <- grf(FRic ~ X2021_blue_absdif+
             X2021_blue_avmin25_RN+
             X2021_blue_avmin25_S2N+
             X2021_blue_avminmax+
             X2021_blue_max_LST+
             X2021_blue_max_S2N+
             X2021_blue_max+
             X2021_GN_avmin25+
             X2021_green_absdif+
             X2021_green_av2575+
             X2021_green_av75max_LST+
             X2021_green_av75max_S2N+
             X2021_green_median+
             X2021_nir_av2575+
             X2021_nir_av75max_LST+
             X2021_nir_av75max+
             X2021_nir_median+
             X2021_red_absdif+
             X2021_red_av75max_RN+
             X2021_red_min_RN+
             X2021_RN_median+
             X2021_RN_sd+
             X2021_RNph_eos_amp+
             X2021_S2N_min+
             X2021_S2N_sd+
             X2021_SVVI_av75max+
             X2021_SVVI_median+
             X2021_swir1_av75max_RN+
             X2021_swir1_av75max_S2N+
             X2021_swir1_av75max+
             X2021_swir1_avmin25_S2N+
             X2021_swir1_avminmax+
             X2021_swir1_max_LST+
             X2021_swir1_max_RN+
             slope, dframe = data_train, bw=10,
           kernel="adaptive", coords=Coords, mtry = results$bestTune$mtry)

grf$LocalModelSummary


## S3 method for class 'grf'
prediction <- predict(results, data_test, x.var.name="x", y.var.name="y")

View(br_MeSl_2019_c$brick_vif_1)

colnames(data_test)

data_test2<- as.matrix(data_test[,-c(37:39)])
class(data_test2)

RDF.Test <- random.test.data(2,2,3)
