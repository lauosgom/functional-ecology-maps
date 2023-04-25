#data engineering - brt model

#set directory
setwd("/Volumes/USB")
setwd("D:/") #windows
setwd("/media/lospina/USB") #linux

# Import libraries and functions
source("functions.R")

#=============< Directories >=========================================
dirmet_2019 <- "Modelo/metricas_merge/"
gedigdb     <- "Archive/GEDI02_A_2021_Clip.shp"
points_slm  <- "puntos/results_fd_agg_XYTableToPoint.shp" # for points
#zonagdb    <- "area/test_3_mod_geo.shp"
#zonagdb    <- "area_slm/area_SLM_2.shp" # for points
zonagdb     <- "area/area_slm3.shp"
dem_dir     <- "dem/dem15final.img"


#====================< Reading the data>========================================
GEDI  <- readOGR(gedigdb)
GEDI2 <- GEDI
GEDI <- GEDI2

#recover
GEDI_recover <- GEDI2

# Filter by only the power beams
# Sensitivity 0.98
GEDI@data <- GEDI@data %>%
  filter(beam == 'BEAM0101' | beam == 'BEAM0110' | beam == 'BEAM1000' | beam == 'BEAM1011') %>%
  filter(sensitivit >= 0.98) %>%
  mutate(delta_time2 = as_datetime(delta_time, origin = "2018-01-1")) %>%
  mutate(delta_timeh = format(as.POSIXct(delta_time2), format = "%H:%M:%S")) %>%
  select(lat_lowest, lon_lowest, rh95, delta_time2, delta_timeh, sensitivit)

GEDI2@data <- GEDI2@data %>%
  filter(beam == "BEAM0101" | beam == "BEAM0110" | beam == "BEAM1000" | beam == "BEAM1011") %>%
  filter(sensitivit >= 0.98) %>%
  mutate(delta_time2 = as_datetime(delta_time, origin = "2018-01-1")) %>%
  mutate(delta_timeh = format(as.POSIXct(delta_time2), format = "%H:%M:%S")) %>%
  select(lat_lowest, lon_lowest, rh95, delta_time2, delta_timeh, sensitivit)

# Only night shots  
GEDI@data <- GEDI@data %>%
  mutate(hour = format(as.POSIXct(GEDI@data$delta_time2), format = "%H")) %>%
  mutate(hour = as.numeric(hour))

GEDI2@data <- GEDI2@data %>%
  mutate(hour = format(as.POSIXct(GEDI@data$delta_time2), format = "%H"))%>%
  mutate(hour = as.numeric(hour))

GEDI@data <- filter(GEDI@data, hour >= 18)
GEDI2@data <- filter(GEDI2@data, hour <= 6)

#merge both times
GEDI@data <- rbind(GEDI@data, GEDI2@data)

GEDI@data <- GEDI@data %>%
  filter(beam == 'BEAM0101' | beam == 'BEAM0110' | beam == 'BEAM1000' | beam == 'BEAM1011')%>%
  select(lat_lowest, lon_lowest, rh95)

#for points FD data
points <- readOGR(points_slm)
#points@data <- points@data

#==============< 3m height adjustment (Potapov et al 2021 [21828]) >============
GEDI$rh95[GEDI$rh95 < 3] <- 0

#==========================< Study area >=======================================
zona     <- readOGR(zonagdb)

#==============< Reading the GLAD metrics PHENO_C >=============================
Metricas_2019 <- list.files(path=dirmet_2019, full.names=T) 
Metricas_2019 <- Metricas_2019[-c(189,190)] # remove water

system.time(Brmet_2019<- lapply(Metricas_2019, raster))

# Rescale values of SpatRaster object with nlyr > 1
Brmet_2019 <- lapply(Brmet_2019, rescale01)

# Asembling the lasagna
system.time(br_Me_B_2019  <- brick(Brmet_2019))

#=========================< Leyendo el DEM >====================================
dem   <- raster(dem_dir)
slope <- terrain(dem, opt='slope', unit='degrees')
slope <- projectRaster(slope, br_Me_B_2019)


#=======================< Extent adjustments >==================================
zona <- spTransform(zona, crs(br_Me_B_2019))
br_Me_B_2019_c <- Cortar_Raster(br_Me_B_2019, zona)
slope <- Cortar_Raster(slope, zona) 

#GEDI <- GEDI[zona, ]

#=======================< Merge slope with metrics >============================
br_MeSl_2019_c <- stack(br_Me_B_2019_c, slope)
#num_vars <- dim(br_MeSl_2019_c)[3]

#==============< Data.frame's for modelling >====================================
# ------- Complete DF -----------------------------------------------
df_Me_B_Sl_2019   <- as.data.frame(raster::extract(br_MeSl_2019_c, GEDI))

df_Me_B_SlGe_2019 <- cbind(df_Me_B_Sl_2019, GEDI)

#store the objects
#writeRaster(br_MeSl_2019_c, 'br_MeSl_2019_V1.tif', overwrite=TRUE)
#write.csv(df_Me_B_SlGe_2019, 'df_Me_B_SlGe_2019.csv')

#df_Me_B_SlGe_2019 <- read.csv('df_Me_B_SlGe_2019.csv', header = T)

#for GEDI data
excluded_vars <- c("lat_lowest", "lon_lowest", "delta_time2", "delta_timeh", "sensitivit", 
                   "hour", "coords.x1", "coords.x2")
#for points
excluded_vars <- c("x", "y", "coords.x1", "coords.x2", "optional")

df_Me_B_SlGe_2019 <-select(df_Me_B_SlGe_2019, -any_of(excluded_vars))

# ------- Divide el DF en entrenamiento y test ----------------------
#dim(df_Me_B_SlGe_2019)
# [1] 60390    88

df_Me_B_SlGe_2019_split <- initial_split(df_Me_B_SlGe_2019, prop = .25)
#dim(df_Me_B_SlGe_2019_split)

df_Me_B_SlGe_2019_train <- training(df_Me_B_SlGe_2019_split)
df_Me_B_SlGe_2019_test  <- testing(df_Me_B_SlGe_2019_split)

df_Me_B_SlGe_2019_train <- na.omit(df_Me_B_SlGe_2019_train)
df_Me_B_SlGe_2019_test  <- na.omit(df_Me_B_SlGe_2019_test)

#rm(df_Me_B_SlGe_2019_split, df_Me_B_SlGe_2019)

write.csv(df_Me_B_SlGe_2019_train, "df_Me_B_SlGe_2019_train.csv")
write.csv(df_Me_B_SlGe_2019_train, "df_Me_B_SlGe_2019_train_points_v2.csv")
write.csv(df_Me_B_SlGe_2019_test, "df_Me_B_SlGe_2019_test.csv")
write.csv(df_Me_B_SlGe_2019_test, "df_Me_B_SlGe_2019_test_points_v2.csv")

writeRaster(br_MeSl_2019_c, "stack2.tiff")