### larval fish sensitivity

# Larval 14 day model bayesian brms
# Larval Fish Models testing using the Categories from Miskie

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


#### This loop will be very slow - Do not need to run again ###
for (lag_days in 9:20){

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
  dat2 <- filter(wind_data, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-lag_days & 
                   Latitude == fish_data$Latitude[i] & Longitude == fish_data$Longitude[i])
  wind_tot <- sum(dat2$displacement)
  fish_data$NE_Winds[i] <- wind_tot
  dat3 <- filter(wind_data_SE, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-lag_days & 
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

### Load species category data
Misk_list <- read_csv("../Data/Copy of Smith et al 2018 habitat catgeories from Miskie.csv")
Estuary_sp <- Misk_list %>% filter(Category == "coast") %>% select(NIMO_NAME)
# Subset to interested species Based upon Ford et al.
include_list <- Estuary_sp$NIMO_NAME
# c(#"Girellidae_Girella.tricuspidata_37361007",# "Girellidae_Girella.spp_37361902", "Labridae_37384000",
# "Mugilidae_other_37381000", # "Monacanthidae_37465903", "Mugilidae_Liza.argentea_37381004",
# "Sparidae_Acanthopagrus.australis_37353004",
# "Sillaginidae_Sillago.ciliata_37330010",
# #"Sparidae_Chrysophrys.auratus_37353001", "Sparidae_Rhabdosargus.sarba_37353013", "Sparidae_other_37353000",
# #"Terapontidae_Pelates.spp_37321908", "Terapontidae_other_37321000"
# "Platycephalidae_Platycephalus.fuscus_37296004")
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
# library(tweedie)
# 
# fit4 <- glmmTMB(Coastal_Normalised_Abund~
#                   poly(NE_Winds.standardised, degree = 2)*dists_km+
#                   poly(SE_Winds.standardised, degree = 2)*dists_km+
#                   SE_Winds.standardised:NE_Winds.standardised*
#                   dists_km + (1|Project_ID), family=tweedie(), data = fish_data)

library(brms)
#fit4 <- brm(Coastal_Normalised_Abund~
#              poly(NE_Winds.standardised, degree = 2)*dists_km+
#              poly(SE_Winds.standardised, degree = 2)*dists_km+
#              SE_Winds.standardised:NE_Winds.standardised*
#              dists_km + (1|Project_ID), family = hurdle_gamma, data = fish_data, seed = 123)

#summary(fit4)
#plot(fit4)
#plot(conditional_effects(fit4))

fit5 <- brm(Coastal_Normalised_Abund~
               poly(NE_Winds.standardised, degree = 2)*dists_km+
               poly(SE_Winds.standardised, degree = 2)*dists_km+
               SE_Winds.standardised:NE_Winds.standardised*
               dists_km + (1|Project_ID), family = hurdle_gamma, data = fish_data, iter = 10000, seed = 1234)

saveRDS(fit5, paste0("../Data/Coastal 14 day brms model_",as.character(lag_days),".rds"))
#fit5 <- readRDS("../Data/Coastal 14 day brms model.rds")

#plot(fit5)
#summary(fit5)

x <- broom.mixed::tidyMCMC(fit5)

write_csv(x, paste0("../Data/Larval Fish Sensitivity/",as.character(lag_days),".csv"))

}


#### Load the sensitivity data and make plot
library(tidyverse)


file_list <- list.files("../Data/Larval Fish Sensitivity/", full.names = TRUE)
file_list2 <- list.files("../Data/Larval Fish Sensitivity/", full.names = F)
file_list2 <- str_remove(file_list2, ".csv")

full_dat <- data.frame()

for (i in 1:length(file_list)){
  mydata <- read_csv(file_list[i])
  mydata <- mydata[1:12,]
  mydata$Days_Prior <- as.numeric(file_list2[i])
  full_dat <- bind_rows(full_dat, mydata)
}

full_dat$Colours <- "black"
full_dat$Colours[full_dat$Days_Prior == 14] <- "red"

mycolours <- c("red" = "red", "black" = "black")

unique(full_dat$term)

# recode (dplyr)

full_dat$term <- recode(full_dat$term, "b_Intercept" = "Intercept",
                   "b_polyNE_Winds.standardiseddegreeEQ21" = "Up Winds",             
                   "b_polyNE_Winds.standardiseddegreeEQ22" = "Up Winds (quadratic)",
                   "b_dists_km" = "Distance to Coast",
                   "b_polySE_Winds.standardiseddegreeEQ21" = "Down Winds",
                   "b_polySE_Winds.standardiseddegreeEQ22" = "Down Winds (Quadratic)",
                   "b_polyNE_Winds.standardiseddegreeEQ21:dists_km" = "Up Winds * Distance",
                   "b_polyNE_Winds.standardiseddegreeEQ22:dists_km" = "Up Winds (Quadratic) *\n Distance",
                   "b_dists_km:polySE_Winds.standardiseddegreeEQ21" = "Down Winds * Distance",
                   "b_dists_km:polySE_Winds.standardiseddegreeEQ22" = "Down Winds (Quadratic) *\n Distance",
                   "b_SE_Winds.standardised:NE_Winds.standardised"  = "Up Winds * Down Winds",
                   "b_dists_km:SE_Winds.standardised:NE_Winds.standardised" = "Up Winds * Down Winds *\n Distance")

p2 <- ggplot(full_dat, aes(x = Days_Prior, y = estimate, col = Colours)) + geom_point() +
  facet_wrap(~term, scales = "free_y") +
  geom_errorbar(aes(ymin=estimate - std.error, ymax=estimate+std.error)) +
  scale_colour_manual(values = mycolours, guide = FALSE) + theme_classic() +
  ylab("Estimate (± SE)") + xlab("Days Prior") +
  theme(axis.title = element_text(face="bold", size = 14, colour = "black"),
      axis.text = element_text(colour = "black", size = 12),
      strip.text = element_text(face = "bold", size = 10))
p2  

ggsave("../plots/Larval Sensitivity.png", dpi = 600, width = 24, height = 18, units = "cm")


### Fish samples over time ###

fish_time <- fish_data %>% group_by(Project_name) %>% summarise(mean_Abund = mean(Coastal_Normalised_Abund, na.rm = T), SD_Abund = sd(Coastal_Normalised_Abund, na.rm = T))
fish_time

ggplot(fish_time, aes(x = Project_name, y = mean_Abund)) + geom_point() +
  geom_errorbar(aes(ymin=mean_Abund-SD_Abund, ymax = mean_Abund+SD_Abund)) + theme_classic()+
  xlab("Project") + ylab("Mean Coastal Larvae Abundance (SD)")+
  theme(axis.title = element_text(size=12, face ="bold"),
        axis.text.y = element_text(colour = "black", size=10),
        axis.text.x = element_text(colour = "black", size=10, angle=45, hjust = 1))

ggsave("../plots/Larval Fish by project.png", dpi = 600, units = "cm", width = 21, height = 14.8)
