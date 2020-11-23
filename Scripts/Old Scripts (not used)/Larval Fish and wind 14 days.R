# NIMO Larval Fish and wind

library(tidyverse)
#library(lme4)
library(glmmTMB)
library(DHARMa)
library(car)
#library(effects)
library(viridis)
library(ggeffects)
library(vegetarian)
library(viridis)
library(cowplot)
library(performance)

# Load Fish Data
fish_data <- read.csv("../Data/allNIMO_dist.csv", header = T)
str(fish_data)

# Restrict to NSW and on the continental shelf
fish_data <- filter(fish_data, Latitude <= -30 & Latitude >= -36 & Longitude > 140)
fish_data <- filter(fish_data, Bathy >= -1000)
summary(fish_data$Bathym_m)
hist(fish_data$Bathym_m)

# Load Wind Data
wind_data <- read.csv("../Data/BARRA Data/BARRA Larval Daily 45 deg Wind Data Final.csv", header = T)
str(wind_data)

wind_data_SE <- read.csv("../Data/BARRA Data/BARRA Larval Daily 135 deg Wind Data Final.csv", header = T)
str(wind_data_SE)

# Recognise Dates
fish_data$Date <- as.Date(as.character(fish_data$Date), format = "%d/%m/%Y")
range(fish_data$Date)
plot(fish_data$Date)
fish_data <- filter(fish_data, Date > "1990-01-01") # to match wind model BARRA # loses ~100 points from 1980s

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
  dat2 <- filter(wind_data, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-14 & 
                   Latitude == fish_data$Latitude[i] & Longitude == fish_data$Longitude[i])
  wind_tot <- sum(dat2$displacement)
  fish_data$NE_Winds[i] <- wind_tot
  dat3 <- filter(wind_data_SE, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-14& 
                   Latitude == fish_data$Latitude[i] & Longitude == fish_data$Longitude[i])
  wind_tot_SE <- sum(dat3$displacement)
  fish_data$SE_Winds[i] <- wind_tot_SE
}

hist(fish_data$NE_Winds)
hist(fish_data$SE_Winds)
hist(fish_data$dists_km)
hist(fish_data$Bathym_m)


# Normalising some parameters (to help model fitting)
fish_data <-  fish_data %>% mutate(NE_Winds.standardised = as.numeric(scale(NE_Winds)),
                                   SE_Winds.standardised = as.numeric(scale(SE_Winds)),
                                   dists_km.standardised = as.numeric(scale(dists_km)))
head(fish_data)

## Normalise Coastal Species


norm_el <- fish_data[,22:239]  #select spp data

norm_el <- norm_el/fish_data$Volume_m3

norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species

row.names(norm_el)

# Subset to interested species Based upon Ford et al.
include_list <- c("Girellidae_Girella.tricuspidata_37361007",# "Girellidae_Girella.spp_37361902", "Labridae_37384000",
                  "Mugilidae_other_37381000", # "Monacanthidae_37465903", "Mugilidae_Liza.argentea_37381004",
                  "Sparidae_Acanthopagrus.australis_37353004",
                  "Sillaginidae_Sillago.ciliata_37330010",
                  #"Sparidae_Chrysophrys.auratus_37353001", "Sparidae_Rhabdosargus.sarba_37353013", "Sparidae_other_37353000",
                  #"Terapontidae_Pelates.spp_37321908", "Terapontidae_other_37321000"
                  "Platycephalidae_Platycephalus.fuscus_37296004")
norm_el <- norm_el[include_list, ]

norm_el <- t(norm_el)  ##re-transpose so columns are species and rows are sites

# sum to make family level abundances
head(norm_el)
norm_el <- as.data.frame(norm_el)
# str(norm_el)
# norm_el$Girellidae <- norm_el$Girellidae_Girella.tricuspidata_37361007 + norm_el$Girellidae_Girella.spp_37361902
# norm_el$Girellidae_Girella.spp_37361902 <- NULL
# norm_el$Girellidae_Girella.tricuspidata_37361007 <- NULL
# str(norm_el)
# norm_el$Mugilidae <- norm_el$Mugilidae_Liza.argentea_37381004 + norm_el$Mugilidae_other_37381000
# norm_el$Mugilidae_Liza.argentea_37381004 <- NULL
# norm_el$Mugilidae_other_37381000 <- NULL
# str(norm_el)
# norm_el$Sparidae <- norm_el$Sparidae_Acanthopagrus.australis_37353004 + norm_el$Sparidae_Chrysophrys.auratus_37353001 +
#   norm_el$Sparidae_other_37353000 + norm_el$Sparidae_Rhabdosargus.sarba_37353013
# norm_el$Sparidae_Acanthopagrus.australis_37353004 <- NULL
# norm_el$Sparidae_Chrysophrys.auratus_37353001 <- NULL
# norm_el$Sparidae_other_37353000 <- NULL
# norm_el$Sparidae_Rhabdosargus.sarba_37353013 <- NULL
# str(norm_el)
# norm_el$Terapontidae <- norm_el$Terapontidae_other_37321000 + norm_el$Terapontidae_Pelates.spp_37321908
# norm_el$Terapontidae_Pelates.spp_37321908 <- NULL
# norm_el$Terapontidae_other_37321000 <- NULL
str(norm_el)

norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species

norm_el <- normalize.rows(norm_el) #normalise rows (ie each spp)


rowSums(norm_el) #checking it worked (sum to 1)

norm_el <- t(norm_el)  ##re-transpose so columns are species and rows are sites

norm_abund <- rowSums(norm_el)
norm_abund
fish_data$Coastal_Normalised_Abund <- norm_abund

## Tweedie Family for positive continuous response variable
fit4 <- glmmTMB(Coastal_Normalised_Abund ~
                  poly(NE_Winds.standardised, degree = 2)*dists_km+
                  poly(SE_Winds.standardised, degree = 2)*dists_km+
                  SE_Winds.standardised:NE_Winds.standardised*
                  dists_km + (1|Project_ID), family=tweedie(), data = fish_data)

r2(fit4)
simulationOutput <- simulateResiduals(fittedModel = fit4, n = 250)
plot(simulationOutput)
hist(residuals(fit4))

## Save plots
# png("../plots/Model checks/Larval14 day1.png", width = 21, height = 14.8, units = "cm", res = 600)
# plot(simulationOutput)
# dev.off()
# 
# png("../plots/Model checks/Larval14 day2.png", width = 21, height = 14.8, units = "cm", res = 600)
# hist(residuals(fit4))
# dev.off()

Anova(fit4,type="II",test="Chisq") # significant interaction between NE and SE winds
summary(fit4)

plot(ggpredict(fit4, terms = "NE_Winds.standardised [all]"))
plot(ggpredict(fit4, terms = "SE_Winds.standardised [all]"))
plot(ggpredict(fit4, terms = "dists_km [all]"))


plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)

# Heatmap making for interaction plot (Run from here) - Very slow if standard error included (was run on HPC to get SE)
nums <- seq(-2,2, by = 0.05)
heat_dataM <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("NE_Winds.standardised" = nums[i],
                           "SE_Winds.standardised" = nums[j],
                           "dists_km" = 5,
                           "Project_ID" = "P1",
                           "Volume_m3" = 1000)
    PredX <- predict(fit4, newdata = pred_map, type = "response", se.fit = F)
    heat_dataM$Southeast.Winds[Nn] <- pred_map$SE_Winds.standardised[1]
    heat_dataM$Northeast.Winds[Nn] <- pred_map$NE_Winds.standardised[1]
    heat_dataM$Abundance[Nn] <- PredX#$fit
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}


################################# Final Plots

mydf <- ggpredict(fit4, terms = "dists_km [all]")

pD <- ggplot(mydf, aes(x, predicted)) + #facet_wrap(~Term, scales = "free_x", switch = "x") +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_classic() + theme(axis.title.x = element_text(colour="black", face = "bold", size = 15),
                          axis.text.x  = element_text(colour="black", size = 14), 
                          axis.title.y = element_text(face="bold", colour="black", size = 15),
                          axis.text.y  = element_text(colour="black", size = 14),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  ylab("Predicted Normalised \nCoastal Species Abundance") +
  xlab("Distance to \nCoast (km)")

pD



heat14 <- read.csv("../Data/heatmap data with error larval 14 day lag.csv", header = T)

# check which dataset is being called (new or saved one)
pH <- ggplot(heat_dataM, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  geom_contour(col="white", aes(z = Abundance), binwidth = 0.002) +
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + 
  scale_fill_viridis(option = "magma", name="Predicted\nNormalised\nCoastal\nSpecies\nAbundance") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 15),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 15),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(colour="black", size=12, face="bold"),
        legend.text = element_text(colour="black", size=10)) 

pH

#ggsave("../plots/Larvae heatmap 14 day.pdf", width = 21, height = 14.8, units = "cm")
#ggsave("../plots/Larvae heatmap 14 day.png", width = 21, height = 14.8, units = "cm", dpi = 600)


# Combine pD and pH

plot_grid(pD, pH, labels = c("A", "B"), label_size = 12, rel_widths = c(1,2))
## Run below to save final plots
#ggsave("../plots/Larvae 14 day plots.png", width = 21, height = 14.8, units = "cm", dpi = 600)
#ggsave("../plots/Larvae 14 day plots.pdf", width = 21, height = 14.8, units = "cm", dpi = 600)
