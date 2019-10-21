# Katana script to generate heatmap data with SE

# Not winds only good until 2014, therefore remove after 2014


library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(car)
library(effects)
library(vegetarian)



# Load Fish Data
fish_data <- read.csv("allNIMO_dist.csv", header = T)
str(fish_data)

# Restrict to NSW and on the continental shelf
fish_data <- filter(fish_data, Latitude <= -30 & Latitude >= -36)
fish_data <- filter(fish_data, Bathym_m <= 200)
summary(fish_data$Bathym_m)
hist(fish_data$Bathym_m)

# Recognise Dates
fish_data$Date <- as.Date(as.character(fish_data$Date), format = "%d/%m/%Y")
fish_data <- filter(fish_data, Date < "2015-1-1")

# Load Wind Data
wind_data <- read.csv("Sydney_Daily Modelled Wind Data Final 45 degree.csv", header = T)
str(wind_data)

wind_data_SE <- read.csv("Sydney_Daily Modelled Wind Data Final 135 degree.csv", header = T)
str(wind_data_SE)

# Make Date Column and recognise as dates
wind_data <- unite(wind_data, col = "Date", c("Day","Month","Year"), sep="/")
str(wind_data)
wind_data$Date <- as.Date(as.character(wind_data$Date), format = "%d/%m/%Y")
str(wind_data)

wind_data_SE <- unite(wind_data_SE, col = "Date", c("Day","Month","Year"), sep="/")
str(wind_data_SE)
wind_data_SE$Date <- as.Date(as.character(wind_data_SE$Date), format = "%d/%m/%Y")
str(wind_data_SE)


# Make column in fish_data for the wind
fish_data$NE_Winds <- 0
fish_data$SE_Winds <- 0
head(fish_data)

# For each sample, find the wind two weeks prior, sum displacement and assign to wind column
for (i in 1:nrow(fish_data)){
  dat2 <- filter(wind_data, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-3)
  wind_tot <- sum(dat2$displacement)
  fish_data$NE_Winds[i] <- wind_tot
  dat3 <- filter(wind_data_SE, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-3)
  wind_tot_SE <- sum(dat3$displacement)
  fish_data$SE_Winds[i] <- wind_tot_SE
}


# Normalising some parameters (to help model fitting)
fish_data <-  fish_data %>% mutate(NE_Winds.standardised = as.numeric(scale(NE_Winds)),
                                   SE_Winds.standardised = as.numeric(scale(SE_Winds)),
                                   dists_km.standardised = as.numeric(scale(dists_km)))
head(fish_data)

## Normalised Coastal Species


norm_el <- fish_data[,22:239]  #select spp data

norm_el <- norm_el/fish_data$Volume_m3

norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species

row.names(norm_el)

# Subset to interested species
include_list <- c("Girellidae_Girella.tricuspidata_37361007", "Girellidae_Girella.spp_37361902", "Labridae_37384000",
                  "Monacanthidae_37465903", "Mugilidae_Liza.argentea_37381004", "Mugilidae_other_37381000",
                  "Sparidae_Acanthopagrus.australis_37353004",
                  "Sparidae_Chrysophrys.auratus_37353001", "Sparidae_Rhabdosargus.sarba_37353013", "Sparidae_other_37353000",
                  "Terapontidae_Pelates.spp_37321908", "Terapontidae_other_37321000")
norm_el <- norm_el[include_list, ]

norm_el <- t(norm_el)  ##re-transpose so columns are species and rows are sites

# sum to make family level abundances
head(norm_el)
norm_el <- as.data.frame(norm_el)
str(norm_el)
norm_el$Girellidae <- norm_el$Girellidae_Girella.tricuspidata_37361007 + norm_el$Girellidae_Girella.spp_37361902
norm_el$Girellidae_Girella.spp_37361902 <- NULL
norm_el$Girellidae_Girella.tricuspidata_37361007 <- NULL
str(norm_el)
norm_el$Mugilidae <- norm_el$Mugilidae_Liza.argentea_37381004 + norm_el$Mugilidae_other_37381000
norm_el$Mugilidae_Liza.argentea_37381004 <- NULL
norm_el$Mugilidae_other_37381000 <- NULL
str(norm_el)
norm_el$Sparidae <- norm_el$Sparidae_Acanthopagrus.australis_37353004 + norm_el$Sparidae_Chrysophrys.auratus_37353001 +
  norm_el$Sparidae_other_37353000 + norm_el$Sparidae_Rhabdosargus.sarba_37353013
norm_el$Sparidae_Acanthopagrus.australis_37353004 <- NULL
norm_el$Sparidae_Chrysophrys.auratus_37353001 <- NULL
norm_el$Sparidae_other_37353000 <- NULL
norm_el$Sparidae_Rhabdosargus.sarba_37353013 <- NULL
str(norm_el)
norm_el$Terapontidae <- norm_el$Terapontidae_other_37321000 + norm_el$Terapontidae_Pelates.spp_37321908
norm_el$Terapontidae_Pelates.spp_37321908 <- NULL
norm_el$Terapontidae_other_37321000 <- NULL
str(norm_el)

norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species

norm_el <- normalize.rows(norm_el) #normalise rows (ie each spp)


rowSums(norm_el) #checking it worked (sum to 1)

norm_el <- t(norm_el)  ##re-transpose so columns are species and rows are sites

norm_abund <- rowSums(norm_el)
norm_abund
fish_data$Coastal_Normalised_Abund <- norm_abund

# fit3 <- glmmTMB(Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#                 family=nbinom1, data = fish_data)

## Tweedie Family for positive continuous response variable
fit3 <- glmmTMB(Coastal_Normalised_Abund ~
                  poly(cbind(SE_Winds.standardised, NE_Winds.standardised), degree = 2)*
                  dists_km + (1|Project_ID), 
                family=tweedie(), data = fish_data)



# Heatmap data making (Run from here)
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81),
                        "Std_Error" = rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("NE_Winds.standardised" = nums[i],
                           "SE_Winds.standardised" = nums[j],
                           "dists_km" = 5,
                           "Project_ID" = "P1")
    PredX <- predict(fit3, newdata = pred_map, type = "response", se.fit = T)
    heat_data$Southeast.Winds[Nn] <- pred_map$SE_Winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$NE_Winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX$fit
    heat_data$Error[Nn] <- PredX$se.fit
    #print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

write.csv(heat_data, "heatmap data with error larval 3 day lag.csv", row.names = F)


