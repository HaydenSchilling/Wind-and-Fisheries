# NIMO Larval Fish and wind

# Not winds only good until 2014, therefore remove after 2014

library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(car)
library(effects)




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
wind_data <- read.csv("Wind Data/45 degree/Sydney_Daily Modelled Wind Data Final 45 degree.csv", header = T)
str(wind_data)

wind_data_SE <- read.csv("Wind Data/135 degree/Sydney_Daily Modelled Wind Data Final 135 degree.csv", header = T)
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
  dat2 <- filter(wind_data, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-14)
  wind_tot <- sum(dat2$displacement)
  fish_data$NE_Winds[i] <- wind_tot
  dat3 <- filter(wind_data_SE, Date <= fish_data$Date[i] & Date >= (fish_data$Date[i])-14)
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

# try modelling again
# fit1 <- glmer.nb(Total_Abundance ~ SE_Winds.standardised*NE_Winds.standardised + dists_km + (1|Project_ID), offset =log(fish_data$Volume_m3), data = fish_data)
fit2 <- glmmTMB(Total_Abundance ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
                offset= log(Volume_m3), family=nbinom2, data = fish_data)

simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
plot(simulationOutput)
plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)

Anova(fit2,type="II",test="Chisq")
summary(fit2)


plot(allEffects(fit2))
plot(Effect(c("SE_Winds.standardised","dists_km"), fit2))
plot(Effect(c("NE_Winds.standardised","dists_km"), fit2))

# Try predictions
pred1 <- data.frame("SE_Winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "NE_Winds.standardised" = 0,
                    "dists_km" = 1,
                    "Project_ID" = "P1",
                    "Volume_m3" = 1000)
pred1

Pred <- predict(fit2, newdata = pred1, type = "response", se.fit = T)

plot(x = pred1$SE_Winds.standardised, y=Pred$fit, type = "l") #, ylim=c(0,1)
lines(x = pred1$SE_Winds.standardised, y=(Pred$fit-Pred$se.fit), type = "l", col = "blue")
lines(x = pred1$SE_Winds.standardised, y=(Pred$fit+Pred$se.fit), type = "l", col = "blue")


# Coastal species only (Ford Identified species only)

fit2 <- glmmTMB(Ford_Fish ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
                offset= log(Volume_m3), family=nbinom2, data = fish_data)

simulationOutput <- simulateResiduals(fittedModel = fit2, n = 250)
plot(simulationOutput)
plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)

Anova(fit2,type="II",test="Chisq")
summary(fit2)


plot(allEffects(fit2))
plot(Effect(c("SE_Winds.standardised","dists_km"), fit2))
plot(Effect(c("NE_Winds.standardised","dists_km"), fit2))

# Try predictions
pred1 <- data.frame("SE_Winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "NE_Winds.standardised" = 0,
                    "dists_km" = 1,
                    "Project_ID" = "P1",
                    "Volume_m3" = 1000)
pred1

Pred <- predict(fit2, newdata = pred1, type = "response", se.fit = T)

plot(x = pred1$SE_Winds.standardised, y=Pred$fit, type = "l", ylim=c(-50,100)) #, 
lines(x = pred1$SE_Winds.standardised, y=(Pred$fit-Pred$se.fit), type = "l", col = "blue")
lines(x = pred1$SE_Winds.standardised, y=(Pred$fit+Pred$se.fit), type = "l", col = "blue")

# Normalising fish species
library(vegetarian)

norm_el <- fish_data[,22:239]  #select spp data

norm_el <- norm_el/fish_data$Volume_m3

norm_el <- t(norm_el) #transpose matrix so columns are sites and rows are species


norm_el <- normalize.rows(norm_el) #normalise rows (ie each spp)

rowSums(norm_el) #checking it worked (sum to 1)

norm_el <- t(norm_el)  ##re-transpose so columns are species and rows are sites

norm_abund <- rowSums(norm_el)
norm_abund
fish_data$Normalised_Abund <- norm_abund

# fit3 <- glmmTMB(Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#                 family=nbinom1, data = fish_data)

## Tweedie Family for positive continuous response variable
fit3 <- glmmTMB(Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
             family=nbinom2, data = fish_data)

simulationOutput <- simulateResiduals(fittedModel = fit3, n = 250)
plot(simulationOutput)
plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)

Anova(fit3,type="II",test="Chisq")
summary(fit3)

hist(fish_data$Normalised_Abund)

plot(allEffects(fit3))
plot(Effect(c("SE_Winds.standardised","dists_km"), fit3)) # not appropriate as NE*SE interaction
plot(Effect(c("NE_Winds.standardised","dists_km"), fit3)) # not appropriate as NE*SE interaction
plot(Effect(c("SE_Winds.standardised","NE_Winds.standardised"), fit3)) # not appropriate as NE*SE interaction


# Try predictions
pred2 <- data.frame("SE_Winds.standardised" = seq(from = -2,
                                     to =2, by = 0.05),
                    "NE_Winds.standardised" = 0,
                    "dists_km" = 0.05,
                    "Project_ID" = "P1")
pred2

Pred2 <- predict(fit3, newdata = pred2, type = "response", se.fit = T)

plot(x = pred2$SE_Winds.standardised, y=Pred2$fit, type = "l", ylim=c(0,1))
lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit-Pred2$se.fit), type = "l", col = "blue")
lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit+Pred2$se.fit), type = "l", col = "blue")

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

# fit3 <- glmmTMB(Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#                 family=nbinom1, data = fish_data)

## Tweedie Family for positive continuous response variable
fit3 <- glmmTMB(Coastal_Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
                family=tweedie(), data = fish_data)

simulationOutput <- simulateResiduals(fittedModel = fit3, n = 250)
plot(simulationOutput)
plotResiduals(fish_data$Project_ID, simulationOutput$scaledResiduals)

Anova(fit3,type="II",test="Chisq")
summary(fit3)

hist(fish_data$Coastal_Normalised_Abund)

plot(allEffects(fit3))
plot(Effect(c("SE_Winds.standardised","dists_km"), fit3)) 
plot(Effect(c("NE_Winds.standardised","dists_km"), fit3)) 
plot(Effect(c("SE_Winds.standardised","NE_Winds.standardised"), fit3))


# Try predictions SE Winds
pred2 <- data.frame("SE_Winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "NE_Winds.standardised" = 0,
                    "dists_km" = 1,
                    "Project_ID" = "P1")
pred2

Pred2 <- predict(fit3, newdata = pred2, type = "response", se.fit = T)

plot(x = pred2$SE_Winds.standardised, y=Pred2$fit, type = "l", 
     ylab = "Predicted Normalised Abundance", xlab = "Standardised Southeast Winds",
     main = "Predicted coastal species abundance \nfor 1km from coast and mean NE winds") #, ylim=c(0,1)
lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit-Pred2$se.fit), type = "l", col = "blue")
lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit+Pred2$se.fit), type = "l", col = "blue")


# To plot in ggplot
SE_1km_plot_dat <- data.frame(Pred2$fit, Pred2$se.fit, pred2$SE_Winds.standardised)
head(SE_1km_plot_dat)

p1 <- ggplot(SE_1km_plot_dat, aes(x = pred2.SE_Winds.standardised, y = Pred2.fit)) + ylim(0,0.035) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Predicted Normalised Abundance") +
  geom_ribbon(aes(ymax = Pred2.fit+Pred2.se.fit, ymin = Pred2.fit-Pred2.se.fit), fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p1

### 10km from coast
pred2 <- data.frame("SE_Winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "NE_Winds.standardised" = 0,
                    "dists_km" = 10,
                    "Project_ID" = "P1")
pred2

Pred2 <- predict(fit3, newdata = pred2, type = "response", se.fit = T)

plot(x = pred2$SE_Winds.standardised, y=Pred2$fit, type = "l", ylim = c(0,0.01),
     ylab = "Predicted Normalised Abundance", xlab = "Standardised Southeast Winds",
     main = "Predicted coastal species abundance \nfor 10km from coast and mean NE winds") #, ylim=c(0,1)
lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit-Pred2$se.fit), type = "l", col = "blue")
lines(x = pred2$SE_Winds.standardised, y=(Pred2$fit+Pred2$se.fit), type = "l", col = "blue")

SE_10km_plot_dat <- data.frame(Pred2$fit, Pred2$se.fit, pred2$SE_Winds.standardised)
head(SE_1km_plot_dat)

p2 <- ggplot(SE_10km_plot_dat, aes(x = pred2.SE_Winds.standardised, y = Pred2.fit)) + ylim(0,0.035) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Predicted Normalised Abundance") +
  geom_ribbon(aes(ymax = Pred2.fit+Pred2.se.fit, ymin = Pred2.fit-Pred2.se.fit), fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p2

# Try predictions NE Winds
pred2 <- data.frame("NE_Winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "SE_Winds.standardised" = 0,
                    "dists_km" = 1,
                    "Project_ID" = "P1")
pred2

Pred2 <- predict(fit3, newdata = pred2, type = "response", se.fit = T)

plot(x = pred2$NE_Winds.standardised, y=Pred2$fit, type = "l", ylim = c(0,0.01),
     ylab = "Predicted Normalised Abundance", xlab = "Standardised Northeast Winds",
     main = "Predicted coastal species abundance \nfor 1km from coast and weak SE winds") #, ylim=c(0,1)
lines(x = pred2$NE_Winds.standardised, y=(Pred2$fit-Pred2$se.fit), type = "l", col = "blue")
lines(x = pred2$NE_Winds.standardised, y=(Pred2$fit+Pred2$se.fit), type = "l", col = "blue")

NE_1km_plot_dat <- data.frame(Pred2$fit, Pred2$se.fit, pred2$NE_Winds.standardised)
head(SE_1km_plot_dat)

p3 <- ggplot(NE_1km_plot_dat, aes(x = pred2.NE_Winds.standardised, y = Pred2.fit)) + ylim(0,0.035) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Predicted Normalised Abundance") +
  geom_ribbon(aes(ymax = Pred2.fit+Pred2.se.fit, ymin = Pred2.fit-Pred2.se.fit), fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p3


### 10km from coast
pred2 <- data.frame("NE_Winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "SE_Winds.standardised" = 0,
                    "dists_km" = 10,
                    "Project_ID" = "P1")
pred2

Pred2 <- predict(fit3, newdata = pred2, type = "response", se.fit = T)

plot(x = pred2$NE_Winds.standardised, y=Pred2$fit, type = "l", ylim = c(0,0.01),
     ylab = "Predicted Normalised Abundance", xlab = "Standardised Northeast Winds",
     main = "Predicted coastal species abundance \nfor 10km from coast and mean SE winds") #, ylim=c(0,1)
lines(x = pred2$NE_Winds.standardised, y=(Pred2$fit-Pred2$se.fit), type = "l", col = "blue")
lines(x = pred2$NE_Winds.standardised, y=(Pred2$fit+Pred2$se.fit), type = "l", col = "blue")

NE_10km_plot_dat <- data.frame(Pred2$fit, Pred2$se.fit, pred2$NE_Winds.standardised)
head(SE_1km_plot_dat)

p4 <- ggplot(NE_10km_plot_dat, aes(x = pred2.NE_Winds.standardised, y = Pred2.fit)) + ylim(0,0.035) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Predicted Normalised Abundance") +
  geom_ribbon(aes(ymax = Pred2.fit+Pred2.se.fit, ymin = Pred2.fit-Pred2.se.fit), fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4


### Plot it all together
library(ggpubr)
ggarrange(p1, p2, p3, p4, 
          labels = c("a) 1km from Coast\n    Mean NE Winds ", "b) 10km from Coast\n    Mean NE Winds",
                     "c) 1km from Coast\n    Mean SE Winds", "d) 10km from Coast\n    Mean SE Winds"))


ggsave("plots/Larvae wind predictions.pdf", width = 21, height = 14.8, units = "cm")
ggsave("plots/Larvae wind predictions.png", width = 21, height = 14.8, units = "cm", dpi = 600)
# Note these were all edited after to fix labels
