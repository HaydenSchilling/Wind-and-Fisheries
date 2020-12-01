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
  dat3 <- filter(wind_data_SE, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-14 & 
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

fit_scale_d <- lm(dists_km ~ dists_km.standardised, data = fish_data)
summary(fit_scale_d)
## Normalise Coastal Species


norm_el <- fish_data[,22:239]  #select spp data

norm_el <- norm_el/fish_data$Volume_m3

norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species

row.names(norm_el)

### Load species category data
Misk_list <- read_csv("../Data/Copy of Smith et al 2018 habitat catgeories from Miskie.csv")
Estuary_sp <- Misk_list %>% filter(Category == "coast" | Category == "coast/pelagic"|
                                     Category == "estuary/coast" | Category == "coast/slope") %>% select(NIMO_NAME)
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

#fish_data <- write_csv(fish_data, "../Data/Fish_data_final_larval_for_katana.csv")



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
               poly(NE_Winds.standardised, degree = 2)*dists_km.standardised+
               poly(SE_Winds.standardised, degree = 2)*dists_km.standardised+
               SE_Winds.standardised:NE_Winds.standardised*
              dists_km.standardised + (1|Project_ID), family = hurdle_gamma, data = fish_data, iter = 10000, seed = 1234)

saveRDS(fit5, "../Data/Coastal 14 day brms modelv3_dist_stand.rds")
fit5 <- readRDS("../Data/Coastal 14 day brms modelv3_dist_stand.rds")

brms::prior_summary(fit5)

plot(fit5)
summary(fit5)
performance::r2_bayes(fit5)

library(tidybayes)
get_variables(fit5)


fit5 %>%
  spread_draws(b_Intercept, b_polyNE_Winds.standardiseddegreeEQ21,
               b_polyNE_Winds.standardiseddegreeEQ22, b_dists_km.standardised,
               b_polySE_Winds.standardiseddegreeEQ21, b_polySE_Winds.standardiseddegreeEQ22,
               `b_polyNE_Winds.standardiseddegreeEQ21:dists_km.standardised`, `b_polyNE_Winds.standardiseddegreeEQ22:dists_km.standardised`,
               `b_SE_Winds.standardised:NE_Winds.standardised`, `b_dists_km.standardised:SE_Winds.standardised:NE_Winds.standardised`,
               `b_dists_km.standardised:polySE_Winds.standardiseddegreeEQ22`, `b_dists_km.standardised:polySE_Winds.standardiseddegreeEQ21`) %>%
  rename("Intercept" = b_Intercept, "Up" = b_polyNE_Winds.standardiseddegreeEQ21,
         "Up (quadratic)"  = b_polyNE_Winds.standardiseddegreeEQ22, "Dist" = b_dists_km.standardised,
         "Down" = b_polySE_Winds.standardiseddegreeEQ21, "Down (quadratic)" = b_polySE_Winds.standardiseddegreeEQ22,
         "Up * Dist"= `b_polyNE_Winds.standardiseddegreeEQ21:dists_km.standardised`, "Up (quadratic) * Dist "= `b_polyNE_Winds.standardiseddegreeEQ22:dists_km.standardised`,
         "Down * Up" = `b_SE_Winds.standardised:NE_Winds.standardised`, "Down * Up * Dist"=`b_dists_km.standardised:SE_Winds.standardised:NE_Winds.standardised`,
         "Down (quadratic) * Dist" = `b_dists_km.standardised:polySE_Winds.standardiseddegreeEQ22`, "Down * Dist" = `b_dists_km.standardised:polySE_Winds.standardiseddegreeEQ21`) %>%
  tidyr::pivot_longer(cols = c(4:13), names_to = "Parameter") %>%
  #median_qi() %>%
  ggplot(aes(x = value, y = Parameter)) +
  stat_halfeye(normalize = "groups") + geom_vline(xintercept =0 , col = "red", lty = 2) +
  xlab("Estimate") +
  theme_classic() + theme(axis.text = element_text(colour="black"),
                          axis.title = element_text(face = "bold"))

ggsave("../plots/Larval Fish 14 Day Bayesian.png", dpi = 600, height = 12, width = 12, units= "cm")


# Which ones are actually important effects/interactions
fit5 %>%
  gather_draws(b_Intercept, b_polyNE_Winds.standardiseddegreeEQ21,
               b_polyNE_Winds.standardiseddegreeEQ22, b_dists_km.standardised,
               b_polySE_Winds.standardiseddegreeEQ21, b_polySE_Winds.standardiseddegreeEQ22,
               `b_polyNE_Winds.standardiseddegreeEQ21:dists_km.standardised`, `b_polyNE_Winds.standardiseddegreeEQ22:dists_km.standardised`,
               `b_SE_Winds.standardised:NE_Winds.standardised`, `b_dists_km.standardised:SE_Winds.standardised:NE_Winds.standardised`,
               `b_dists_km.standardised:polySE_Winds.standardiseddegreeEQ22`, `b_dists_km.standardised:polySE_Winds.standardiseddegreeEQ21`) %>%
  median_qi() #%>%
#  write_csv("../Data/Larval fish model coefs 14 day Bayesian.csv")

# Important effects do not cross 0 with 95% Credible Interval. These are:
# Distance
# Dist * Up (quadratic)
# Dist * Up * Down
# Intercept
# Up Winds

# prediction plots
plot(conditional_effects(fit5))


mydf1 <- ggpredict(fit5, terms = "dists_km.standardised [all]")
mydf1$x <- mydf1$x * fit_scale_d$coefficients[2] +  fit_scale_d$coefficients[1] # back transform to km
mydf2 <- ggpredict(fit5, terms = "NE_Winds.standardised")
mydf3 <- ggpredict(fit5, terms = "SE_Winds.standardised [all]")

mydf1$Term <- "a) Distance to\nCoast (km)"
mydf2$Term <- "b) Upwelling\nFavourable Winds"
mydf3$Term <- "c) Downwelling\nFavourable Winds"

mydf_all <- bind_rows(mydf1, mydf2,mydf3)


pD <- ggplot(mydf_all, aes(x, predicted)) + facet_wrap(~Term, scales = "free_x") +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_classic() + theme(axis.title.x = element_blank(),
                          axis.text.x  = element_text(colour="black", size = 14), 
                          axis.title.y = element_text(face="bold", colour="black", size = 15),
                          axis.text.y  = element_text(colour="black", size = 14),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  ylab("Predicted Normalised \nCoastal Taxa Abundance")# +


pD

ggsave("../plots/larval 14 predictions bayesian.png", width = 21, height = 12.8, units="cm", dpi = 600)


