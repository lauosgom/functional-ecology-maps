# Set working directory
setwd("/Volumes/USB") # mac
setwd("D:/")
setwd("/media/lospina/USB") #linux

library(DataCombine)
library(splancs)
library(TigR)
library(FD)

source('chapter_2/github/functional_ecology_maps/feature_engineering/functions.r')

data<- read_csv("chapter_2/github/functional_ecology_maps/data/vegetacion_guatemala.csv")
data_traits<- read_csv("chapter_2/github/functional_ecology_maps/data/traits_species_2022_09_03.csv")
replace<- read_csv("chapter_2/github/functional_ecology_maps/data/replacements_species.csv")

data<-as.data.frame(data)
Replaces <- data.frame(from = replace$antes, to = replace$despues)
data <- FindReplace(data = data, Var = "Genus_species", replaceData = Replaces,
                    from = "from", to = "to", exact = TRUE)

#data traits filter MOSS, LICHEN, LIVEWORTH
#list to exclude later the life forms that we don't want
data_traits_exclude<- data_traits %>%
  filter(GROWTH == "MOSS" | GROWTH == "LICHEN" |GROWTH == "LIVEWORTH") %>%
  select(genus_species)

#filter to clean the traits
data_traits <- data_traits %>%
  filter(GROWTH != "MOSS") %>%
  filter(GROWTH != "LICHEN") %>%
  filter(GROWTH != "LIVEWORTH")

#data for all the analysis
data_analysis <- data %>%
  filter(Family != "(blank)") %>%
  filter(Genus != "(blank)") %>%
  filter(Genus_species != "(blank) (blank)") %>%
  filter(!grepl("Inde", Genus_species)) %>%
  filter(!grepl("Cordoncillo", Genus_species)) %>%
  filter(!grepl("Unkn", Genus_species)) %>%
  filter(!grepl("Unidentified", Genus_species)) %>%
  filter(!grepl("zaca", Genus_species)) %>%
  filter(!grepl("Zacate", Genus_species)) %>%
  filter(!grepl("Bare", Genus_species)) %>%
  filter(!grepl("Cyatheaceae sp.", Genus_species)) %>% # correct later
  select(Family, Genus, Genus_species, abundancia, p_min, code)


#matrix sites x plant families
data_species<-data_analysis %>%
  group_by(Genus_species, code) %>%
  summarise(min_pre = max(p_min)) %>%
  pivot_wider(names_from= "Genus_species", values_from = "min_pre") %>%
  replace(is.na(.), 0)

#run aggregations to fix castellanos
data_species <- aggregate_data(data_species, "CAS")

class(data_species)<-"data.frame"

for (i in 2:ncol(data_species)) {
  class(data_species[,i])<-"numeric"
}

species_matrix = as.matrix(data_species[,-c(1)])
rownames(species_matrix) <- data_species$code

species_matrix<-species_matrix[,which(!colSums(species_matrix) == 0)]

#exclude lifeforms that we don't want
#if in data_traits_exclude, then remove that column
species_matrix<-species_matrix[,which(!colnames(species_matrix) %in% data_traits_exclude$genus_species)]


#matrix species x traits
test<-as.data.frame(colnames(species_matrix))
colnames(test)<-"genus_species"

traits<-merge(test,data_traits, by="genus_species", all.x = TRUE)
traits_matrix <- as.matrix(traits[,-c(1:5)])
traits_matrix <- traits[,-c(1:5)]
rownames(traits_matrix)<- traits$genus_species

Replaces <- data.frame(from = c("YES","NO"), to = c("1","0"))
traits_matrix <- FindReplace(data = traits_matrix, Var = "RESPROUTING", replaceData = Replaces,
                    from = "from", to = "to", exact = TRUE)

#traits_matrix<-traits_matrix[,c(1,2)]

class(traits_matrix$RESPROUTING) <- "numeric"
traits_matrix$SHADE<-as.factor(traits_matrix$SHADE)
traits_matrix$GROWTH<-as.factor(traits_matrix$GROWTH)
traits_matrix$ROOT_ARCH<-as.factor(traits_matrix$ROOT_ARCH)
traits_matrix$CLONALLITY<-as.factor(traits_matrix$CLONALLITY)

# check lengths, they must be equal
length(row.names(traits_matrix))                        
length(colnames(species_matrix))

write.csv(species_matrix,"species_matrix_guate.csv")

results_fd<-dbFD(traits_matrix, species_matrix, stand.FRic = TRUE, corr = "cailliez",
                 print.pco = TRUE, m = "max", calc.CWM = TRUE)


#Functional Diversity calculations
results2<-data.frame(FRic=results_fd$FRic,FDiv=results_fd$FDiv, FEve=results_fd$FEve,
                     FDis=results_fd$FDis)

results2$code<-rownames(results2)

#FRic manual corrections
results2[results2$code == "P492016",1]<- 0.1379
results2[results2$code == "MED_parcela 41",1]<- 0.08116
results2[results2$code == "MED_parcela 31",1]<- 0.1264
results2[results2$code == "MED_parcela 39",1]<- 0.1264
results2[results2$code == "MED_parcela 40",1]<- 0.1264
results2[results2$code == "P042009",1]<- 0.0960

# Save functional metrics into csv
write.csv(results2, "results_fd_agg.csv")

# Save Community Weighted Mean into csv
write.csv(results_fd$CWM, "results_fd_cwm.csv")






