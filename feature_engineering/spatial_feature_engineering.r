#feature engineering - brt model

#set directory
setwd("/Volumes/USB")
setwd("D:/") #windows
setwd("/media/lospina/USB") #linux

# Import libraries and functions
source("functions.R")

#=============< Directories >=========================================
dirmet           <- "Modelo/metricas_merge/"
dirmet           <- "Modelo/metrics_scaled/"
gedigdb          <- "Archive/GEDI02_A_2021_Clip.shp"
points_slm       <- "puntos/results_fd_agg_XYTableToPoint.shp" # for points
zonagdb_small    <- "area/test_3_mod_geo.shp"
zonagdb_water    <- "area_slm/area_SLM_2.shp" # for points
zonagdb          <- "area/area_slm3.shp"
dem_dir          <- "dem/dem15final.img"


#====================< Reading the data>========================================
gedi  <- readOGR(gedigdb)

#recover
gedi_recover <- GEDI

# Filter by only the power beams
# Sensitivity 0.98
gedi@data <- gedi@data %>%
  filter(beam == 'BEAM0101' | beam == 'BEAM0110' | beam == 'BEAM1000' | beam == 'BEAM1011') %>% # nolint
  filter(sensitivit >= 0.98) %>%
  mutate(delta_time2 = as_datetime(delta_time, origin = "2018-01-1")) %>%
  mutate(delta_timeh = format(as.POSIXct(delta_time2), format = "%H:%M:%S")) %>%
  select(lat_lowest, lon_lowest, rh95, delta_time2, delta_timeh, sensitivit)

gedi@data <- GEDI2@data %>%
  filter(beam == "BEAM0101" | beam == "BEAM0110" | beam == "BEAM1000" | beam == "BEAM1011") %>% # nolint
  filter(sensitivit >= 0.98) %>%
  mutate(delta_time2 = as_datetime(delta_time, origin = "2018-01-1")) %>%
  mutate(delta_timeh = format(as.POSIXct(delta_time2), format = "%H:%M:%S")) %>%
  select(lat_lowest, lon_lowest, rh95, delta_time2, delta_timeh, sensitivit)

# Only night shots
gedi@data <- gedi@data %>%
  mutate(hour = format(as.POSIXct(gedi@data$delta_time2), format = "%H")) %>%
  mutate(hour = as.numeric(hour))

gedi2@data <- gedi@data %>%
  mutate(hour = format(as.POSIXct(gedi@data$delta_time2), format = "%H")) %>%
  mutate(hour = as.numeric(hour))

gedi@data <- filter(gedi@data, hour >= 18)
gedi2@data <- filter(gedi2@data, hour <= 6)

#merge both times
gedi@data <- rbind(gedi@data, gedi@data)

#for points FD data
points <- readOGR(points)


#==============< 3m height adjustment (Potapov et al 2021 [21828]) >============
gedi$rh95[GEDI$rh95 < 3] <- 0

#==========================< Study area >=======================================
zona     <- readOGR(zonagdb)

#==============< Reading the GLAD metrics PHENO_C >=============================
metrics <- list.files(path = dirmet, full.names = T)
metrics <- metrics[-c(189:191)] # remove water

system.time(Brmet_2019 <- lapply(metrics, raster))

# If your raster data is not scaled already
for (i in 76:length(metrics)){
  print(i)
  metric       <- raster(metrics[i])
  metric_scale <- raster01(metric)
  name <- paste0("metric", i, ".tif")
  writeRaster(metric_scale, filename = name)
}


# Assembling the lasagna
system.time(br_Me_B_2019  <- brick(Brmet_2019))

#=========================< Leyendo el DEM >====================================
dem   <- raster(dem_dir)
slope <- terrain(dem, opt = "slope", unit = "degrees")
slope <- projectRaster(slope, br_Me_B_2019)
slope <- raster01(slope)

#=======================< Extent adjustments >==================================
zona <- spTransform(zona, crs(br_Me_B_2019))
brick_metrics_c <- cut_raster(br_Me_B_2019, zona)
slope <- cut_raster(slope, zona)

#=======================< Merge slope with metrics >============================
brick_metrics_c <- stack(brick_metrics_c, slope)

#========================< VIF >================================================
# VIF for the metrics
v1 <- vifstep(brick_metrics_c)
v2 <- vifcor(brick_metrics_c, th = 0.7)

# keep only the variables that are not correlated
brick_metrics_c <- exclude(brick_metrics_c, v1)

#==============< Data.frame's for modelling >====================================
# ------- Complete DF -----------------------------------------------
df_metric_c <- as.data.frame(raster::extract(brick_metrics_c, gedi))
df_metric_c <- as.data.frame(raster::extract(brick_metrics_c, points))

df_metric_c_points <- cbind(df_metric_c, GEDI)
df_metric_c_points <- cbind(df_metric_c, points)

#for GEDI data
excluded_vars <- c("lat_lowest", "lon_lowest", "delta_time2", "delta_timeh", "sensitivit",  # nolint
                   "hour", "coords.x1", "coords.x2")
#for points
excluded_vars <- c("x", "y", "coords.x1", "coords.x2", "optional", "code")

df_Me_B_SlGe_2019 <-select(df_Me_B_SlGe_2019, -any_of(excluded_vars)) # nolint

write.csv(df_Me_B_SlGe_2019_test, "data_set.csv")

#save the raster stack
writeRaster(br_MeSl_2019_c, "stack2.tiff")