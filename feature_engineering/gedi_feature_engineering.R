#set directory
setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/chapter_2/") #windows
setwd("/media/lospina/easystore/chapter_2/") #linux

# Import libraries and functions
source("github/functional-ecology-maps/feature_engineering/functions.R")

#=============< Directories >=========================================
dirmet           <- "data/optical/metrics_scaled/"
dirgedi_2a          <- "data/points/gedi_2a/GEDI02_A_202132620474_Select.shp"
dirgedi_2b          <- "data/points/gedi_2b/gedi_vpm.shp"
zonagdb          <- "data/area/no_water/area_slm3.shp"



#====================< Reading the data>========================================
gedip_2a  <- readOGR(dirgedi_2a)
gedip_2b  <- readOGR(dirgedi_2b)

# select columns
gedip_2a@data <- gedip_2a@data %>%
  select(beam, shot_numbe, degrade_fl, quality_fl, delta_time, sensitivit, lat_lowest, lon_lowest,
         rh25, rh95)

gedip_2b@data <- gedip_2b@data %>%
  select(beam, shot_numb0, delta_time, sensitivi0, pai, fhd_normal)

colnames(gedip_2b@data) <- c("beam", "shot_numbe", "delta_time", "sensitivi0", "pai", "fhd_normal")

# merge both products: L2A and L2B
gedip_2b@data <- merge(gedip_2b@data, gedip_2a@data, by = "shot_numbe")

gedip_2b@data$cr <- (gedip_2b@data$rh95 - gedip_2b@data$rh25)/gedip_2b@data$rh95

gedi_recover <- gedip_2b

# Filter by power beams and sensitivity 
gedi_recover@data <- gedi_recover@data %>%
  filter(rh25 >= 0) %>%
  filter(beam.x == 'BEAM0101' | beam.x == 'BEAM0110' | beam.x == 'BEAM1000' | beam.x == 'BEAM1011') %>% # nolint
  filter(sensitivit >= 0.98) %>%
  mutate(delta_time2 = as_datetime(delta_time.x, origin = "2018-01-1")) %>%
  mutate(delta_timeh = format(as.POSIXct(delta_time2), format = "%H:%M:%S")) %>%
  mutate(hour = hour(delta_time2))

gedi_night<- gedi_recover

# Filtrate by night shots
gedi_night@data <- gedi_night@data[gedi_night@data$hour >= 18 | gedi_night@data$hour < 6,]

# study area
zona     <- readOGR(zonagdb)

# Functional diversity or by each trait



