setwd("/Volumes/USB/chapter_2/") #mac
setwd("D:/") #windows
setwd("/media/lospina/USB/chapter_2")

data <- read.csv("data_set.csv", header = T)
colnames(data)

results_brt <- brt_model(data, proportion_split = 2/4 ,predictor = 1:35, response = 36)