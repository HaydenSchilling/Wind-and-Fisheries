# modelling

library(merTools)
library(MuMIn)
library(dplyr)
library(effects)
library(lme4)
library(lmerTest)

library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(car)
library(effects)
library(vegetarian)


# Load Data
mydata <- read.csv("Full_Data_Modelling_BARRA_by_estuary.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv
# mydata2 <-  read.csv("Full_Data_Modelling.csv", header = T)
# 
# cor.test(mydata$X45_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X45_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X45_degree_winds, mydata2$X45_degree_winds)
# # NE winds not correlated by SE winds are.....
# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")

### Standardise Winds
my.df <-  mydata %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))#, # Onshore winds removed



### Try random effect of estuary

Flathead <- subset(my.df, Species == "Flathead")

#m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type + Drought_Months + (1|Estuary), data = Flathead)
#AIC(m1) #215.42

m1 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
                   poly(X45_degree_winds.standardised, degree = 2) +
                   X135_degree_winds.standardised:X45_degree_winds.standardised+
                   Estuary_Type * Drought_Months + (1|Estuary), data = Flathead)

AIC(m1) # 197.0064

library(bootpredictlme4)


# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81),
                        "Error" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(Flathead$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m1, newdata = pred_map, type = "response", se.fit = T, nsim = 500)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX$fit
    heat_data$Error[Nn] <- PredX$se.fit
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

write.csv(heat_data, "Flathead CPUE heatmap data.csv", row.names = FALSE)