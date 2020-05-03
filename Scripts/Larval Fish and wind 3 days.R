# NIMO Larval Fish and wind

# Not winds only good until 2014, therefore remove after 2014

# 22/9/19 - check lag dates for winds and winds in predictions. Need short lag for good SE Wind effects

library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(car)
library(effects)
library(viridis)
library(ggeffects)


# Load Fish Data
fish_data <- read.csv("../allNIMO_dist.csv", header = T)
str(fish_data)

# Restrict to NSW and on the continental shelf
fish_data <- filter(fish_data, Latitude <= -30 & Latitude >= -36 & Longitude > 140)
fish_data <- filter(fish_data, Bathy >= -1000)
summary(fish_data$Bathym_m)
hist(fish_data$Bathym_m)

# Load Wind Data
wind_data <- read.csv("../BOM Data/BARRA Model/BARRA Larval Daily 45 deg Wind Data Final.csv", header = T)
str(wind_data)

wind_data_SE <- read.csv("../BOM Data/BARRA Model/BARRA Larval Daily 135 deg Wind Data Final.csv", header = T)
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
  dat2 <- filter(wind_data, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-3 & 
                   Latitude == fish_data$Latitude[i] & Longitude == fish_data$Longitude[i])
  wind_tot <- sum(dat2$displacement)
  fish_data$NE_Winds[i] <- wind_tot
  dat3 <- filter(wind_data_SE, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-3& 
                   Latitude == fish_data$Latitude[i] & Longitude == fish_data$Longitude[i])
  wind_tot_SE <- sum(dat3$displacement)
  fish_data$SE_Winds[i] <- wind_tot_SE
}

hist(fish_data$NE_Winds)
hist(fish_data$SE_Winds)
hist(fish_data$dists_km)
hist(fish_data$Bathym_m)

# # Harmonic is to fit Hour and DOY
# Harm <- function (theta, k = 4) {
#   X <- matrix(0, length(theta), 2 * k)
#   nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
#   dimnames(X) <- list(names(theta), nam)
#   m <- 0
#   for (j in 1:k) {
#     X[, (m <- m + 1)] <- cos(j * theta)
#     X[, (m <- m + 1)] <- sin(j * theta)
#   }
#   X
# }
# 
# 
# # Make cyclic month and hour and day of year
# fish_data$Harm_Month <- (fish_data$Month/12) * 2 * pi
# 


plot(fish_data$Total_Abundance/fish_data$Volume_m3 ~ fish_data$NE_Winds)
plot(fish_data$Total_Abundance/fish_data$Volume_m3 ~ fish_data$SE_Winds)
plot(fish_data$NE_Winds, fish_data$SE_Winds)

#fit1 <- lm(sqrt(Total_Abundance/Volume_m3) ~ SE_Winds, data = fish_data)
#plot(fit1)
#summary(fit1)
#plot(fish_data$Total_Abundance/fish_data$Volume_m3 ~ fish_data$SE_Winds)
#abline(fit1)


# Normalising some parameters (to help model fitting)
fish_data <-  fish_data %>% mutate(NE_Winds.standardised = as.numeric(scale(NE_Winds)),
                                   SE_Winds.standardised = as.numeric(scale(SE_Winds)),
                                   dists_km.standardised = as.numeric(scale(dists_km)))
head(fish_data)
hist(fish_data$SE_Winds.standardised)
plot(fish_data$SE_Winds ~ fish_data$Month)
ggplot(fish_data, (aes(Month, SE_Winds))) + geom_smooth() + geom_point(alpha = 0.5) # no month effect needed

# # try modelling again
# # fit1 <- glmer.nb(Total_Abundance ~ SE_Winds.standardised*NE_Winds.standardised + dists_km + (1|Project_ID), offset =log(fish_data$Volume_m3), data = fish_data)
# fit2 <- glmmTMB(Total_Abundance ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#                 offset= log(Volume_m3), family=nbinom2, data = fish_data)
# 
# simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
# plot(simulationOutput)
# plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)
# 
# Anova(fit2,type="II",test="Chisq")
# summary(fit2)
# 
# 
# plot(allEffects(fit2))
# plot(Effect(c("SE_Winds.standardised","dists_km"), fit2))
# plot(Effect(c("NE_Winds.standardised","dists_km"), fit2))
# 
# # Try predictions
# pred1 <- data.frame("SE_Winds.standardised" = seq(from = -2,
#                                                   to =2, by = 0.05),
#                     "NE_Winds.standardised" = 0,
#                     "dists_km" = 1,
#                     "Project_ID" = "P1",
#                     "Volume_m3" = 1000)
# pred1
# 
# Pred <- predict(fit2, newdata = pred1, type = "response", se.fit = T)
# 
# plot(x = pred1$SE_Winds.standardised, y=Pred$fit, type = "l") #, ylim=c(0,1)
# lines(x = pred1$SE_Winds.standardised, y=(Pred$fit-Pred$se.fit), type = "l", col = "blue")
# lines(x = pred1$SE_Winds.standardised, y=(Pred$fit+Pred$se.fit), type = "l", col = "blue")
# 
# 
# # Coastal species only (Ford Identified species only)
# 
# fit2 <- glmmTMB(Ford_Fish ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#                 offset= log(Volume_m3), family=nbinom2, data = fish_data)
# 
# simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
# plot(simulationOutput)
# plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)
# 
# Anova(fit2,type="II",test="Chisq")
# summary(fit2)
# 
# 
# plot(allEffects(fit2))
# plot(Effect(c("SE_Winds.standardised","dists_km"), fit2))
# plot(Effect(c("NE_Winds.standardised","dists_km"), fit2))
# 
# # Try predictions
# pred1 <- data.frame("SE_Winds.standardised" = seq(from = -2,
#                                                   to =2, by = 0.05),
#                     "NE_Winds.standardised" = 0,
#                     "dists_km" = 1,
#                     "Project_ID" = "P1",
#                     "Volume_m3" = 1000)
# pred1
# 
# Pred <- predict(fit2, newdata = pred1, type = "response", se.fit = T)
# 
# plot(x = pred1$SE_Winds.standardised, y=Pred$fit, type = "l", ylim=c(-50,100)) #, 
# lines(x = pred1$SE_Winds.standardised, y=(Pred$fit-Pred$se.fit), type = "l", col = "blue")
# lines(x = pred1$SE_Winds.standardised, y=(Pred$fit+Pred$se.fit), type = "l", col = "blue")
# 
# # Normalising fish species
# library(vegetarian)
# 
# norm_el <- fish_data[,22:239]  #select spp data
# 
# norm_el <- norm_el/fish_data$Volume_m3
# 
# norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species
# 
# 
# norm_el <- normalize.rows(norm_el) #normalise rows (ie each spp)
# 
# rowSums(norm_el) #checking it worked (sum to 1)
# 
# norm_el <- t(norm_el)  ##re-transpose so columns are species and rows are sites
# 
# norm_abund <- rowSums(norm_el)
# norm_abund
# fish_data$Normalised_Abund <- norm_abund
# 
# # fit3 <- glmmTMB(Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
# #                 family=nbinom1, data = fish_data)
# 
# ## Tweedie Family for positive continuous response variable
# fit3 <- glmmTMB(Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#              family=nbinom2, data = fish_data)
# 
# simulationOutput <- simulateResiduals(fittedModel = fit3, n = 250)
# plot(simulationOutput)
# plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)
# 
# Anova(fit3,type="II",test="Chisq")
# summary(fit3)
# 
# hist(fish_data$Normalised_Abund)
# 
# plot(allEffects(fit3))
# plot(Effect(c("SE_Winds.standardised","dists_km"), fit3)) # not appropriate as NE*SE interaction
# plot(Effect(c("NE_Winds.standardised","dists_km"), fit3)) # not appropriate as NE*SE interaction
# plot(Effect(c("SE_Winds.standardised","NE_Winds.standardised"), fit3)) # not appropriate as NE*SE interaction
# 
# 
# # Try predictions
# pred2 <- data.frame("SE_Winds.standardised" = seq(from = -2,
#                                      to =2, by = 0.05),
#                     "NE_Winds.standardised" = 0,
#                     "dists_km" = 0.05,
#                     "Project_ID" = "P1")
# pred2
# 
# Pred2 <- predict(fit3, newdata = pred2, type = "response", se.fit = T)
# 
# plot(x = pred2$SE_Winds.standardised, y=Pred2$fit, type = "l", ylim=c(0,1))
# lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit-Pred2$se.fit), type = "l", col = "blue")
# lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit+Pred2$se.fit), type = "l", col = "blue")

## Normalised Coastal Species
library(vegetarian)

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


# equivalent code but allows marginal effects to be calculated
fit4 <- glmmTMB(Coastal_Normalised_Abund ~
                  poly(NE_Winds.standardised, degree = 2)*dists_km+
                  poly(SE_Winds.standardised, degree = 2)*dists_km+
                  SE_Winds.standardised:NE_Winds.standardised*
                  dists_km + (1|Project_ID), family=tweedie(), data = fish_data)

png("../plots/Model checks/Larval3 day1.png", width = 21, height = 14.8, units = "cm", res = 600)
plot(simulationOutput)
dev.off()

png("../plots/Model checks/Larval3 day2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(residuals(fit4))
dev.off()

summary(fit4)

plot(ggpredict(fit4, terms = "NE_Winds.standardised [all]"))
plot(ggpredict(fit4, terms = "SE_Winds.standardised [all]"))
plot(ggpredict(fit4, terms = "dists_km [all]"))

N_dat <- ggpredict(fit4, terms = "NE_Winds.standardised [all]")
s_dat <- ggpredict(fit4, terms = "SE_Winds.standardised [all]")
d_dat <- ggpredict(fit4, terms = "dists_km [all]")

N_dat$Term <- "Upwelling \nFavourable Winds"
s_dat$Term <- "Downwelling \nFavourable Winds"
d_dat $Term <- "Distance from Coast (km)"

plot_dat <- bind_rows(N_dat, s_dat, d_dat)

# marginal effects plot for distance from coast
library(ggplot2)



ggplot(plot_dat, aes(x, predicted)) + facet_wrap(~Term, scales = "free_x", switch = "x") +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_classic() + theme(axis.title.x = element_blank(),
  axis.text.x  = element_text(colour="black", size = 12), 
  axis.title.y = element_text(face="bold", colour="black", size = 14),
  axis.text.y  = element_text(colour="black", size = 14),
  axis.ticks = element_line(colour="black"),
  strip.text = element_text(colour="black", face = "bold", size = 13),
  strip.background = element_blank(),
  strip.placement = "outside",
#legend.justification=c(1,0), legend.position="right",
panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  ylab("Predicted Normalised \nCoastal Species Abundance")

ggsave("../plots/larval model 3 day.png", height = 14.8, width = 21, units = "cm", dpi = 600)




### larval plot

pI <- ggplot(fish_data, aes(x = SE_Winds.standardised,y = NE_Winds.standardised, size = Coastal_Normalised_Abund)) + geom_point(alpha = 0.5) +
  theme_classic() + labs(title = "Winds 3 days prior")
pI

ggsave("../Plots/Winds Larval dot size 3 days.png", width = 21, height = 14.8, units = "cm", dpi = 600)


pI2 <- ggplot(fish_data, aes(x = SE_Winds.standardised,y = Coastal_Normalised_Abund)) + geom_point(alpha = 0.5) + geom_smooth()+
  theme_classic() + labs(title = "Winds 3 days prior")
pI2
ggsave("Plots/SE Winds Larval Abund 3 days.png", width = 21, height = 14.8, units = "cm", dpi = 600)

pI3 <- ggplot(fish_data, aes(x = NE_Winds.standardised,y = Coastal_Normalised_Abund)) + geom_point(alpha = 0.5) + geom_smooth() +
  theme_classic() + labs(title = "Winds 3 days prior")
pI3
ggsave("Plots/NE Winds Larval Abund 3 days.png", width = 21, height = 14.8, units = "cm", dpi = 600)


# Test Jon Gillson comments about residuals (still to do autocorrelation?)
hist(simulationOutput$scaledResiduals)
hist(residuals(fit3))

hist(simulationOutput$fittedResiduals)

res <- residuals(fit3)
acf(res, plot = T)
head(res, type = "pearson")

library(ggfortify)
acf_p <- autoplot(acf(res)) + #simulationOutput$fittedResiduals
  geom_hline(yintercept = 0) +
  ylab('Autocorrelation function')
acf_p

### Continue on
plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)

Anova(fit4,type="II",test="Chisq")
summary(fit3)

hist(fish_data$Coastal_Normalised_Abund)

plot(allEffects(fit3))
plot(Effect(c("SE_Winds.standardised"), fit3)) 
plot(Effect(c("NE_Winds.standardised","dists_km"), fit3)) 
plot(Effect(c("SE_Winds.standardised","NE_Winds.standardised"), fit3))

cor.test(fish_data$NE_Winds.standardised, fish_data$SE_Winds.standardised)



