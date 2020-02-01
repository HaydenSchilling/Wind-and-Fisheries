# CPUE Modelling + Model Selection

# modelling

library(ggplot2)
library(DHARMa)
library(bootpredictlme4)
library(merTools)
library(MuMIn)

# Load Data
mydata <- read.csv("../Full_Data_Modelling.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv


p1 <- ggplot(mydata, aes(x = X135_degree_winds, y = CPUE)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")



# Standardise CPUE for each species in each estuary
library(dplyr)
my.df <-  mydata %>% group_by(Estuary, Species) %>% mutate(CPUE.standardised = as.numeric(scale(CPUE)))
head(my.df)

p1 <- ggplot(my.df, aes(x = Estuary, y = CPUE.standardised)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

library(effects)



### Standardise Winds
my.df <-  my.df %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))#, # Onshore winds removed


pI <- ggplot(my.df, aes(x = X135_degree_winds.standardised,y = X45_degree_winds.standardised, size = CPUE.standardised)) + geom_point(alpha = 0.1) +
  theme_classic() + labs(title = "CPUE by lagged Winds")
pI

pI <- ggplot(my.df, aes(x = X135_degree_winds.standardised, y = CPUE.standardised)) + geom_point(alpha = 0.1) +
  theme_classic() + labs(title = "CPUE by lagged Winds") + facet_wrap(~Species)
pI

### Try random effect of estuary

library(lme4)
library(lmerTest)
bream <- subset(my.df, Species == "Bream")

m1a <- lm(CPUE.standardised ~ poly(X135_degree_winds.standardised, degree = 2) + 
            poly(X45_degree_winds.standardised, degree = 2) +
            X135_degree_winds.standardised:X45_degree_winds.standardised+ 
              Drought_Months, data = bream) # (1|Estuary)
plot(m1a)
AIC(m1a) # 210
r.squaredGLMM(m1a) # 0.195

summary(m1a)

m1b <- lmer(CPUE.standardised ~ poly(X135_degree_winds.standardised, degree = 2) + 
  poly(X45_degree_winds.standardised, degree = 2) +
  X135_degree_winds.standardised:X45_degree_winds.standardised+
  (1|Estuary), data = bream)
AIC(m1b) # 191.32
r.squaredGLMM(m1b) # 0.385

m1c <- lmer(CPUE.standardised ~ poly(X135_degree_winds.standardised, degree = 2) + 
              poly(X45_degree_winds.standardised, degree = 2) +
              X135_degree_winds.standardised:X45_degree_winds.standardised+
              Estuary_Type  + (1|Estuary), data = bream)
AIC(m1c) # 198
r.squaredGLMM(m1c) # 0.378

m1d <- lmer(CPUE.standardised ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Drought_Months + (1|Estuary), data = bream)
AIC(m1d) # 187
r.squaredGLMM(m1d) # 0.468

m1e <- lmer(CPUE.standardised ~ X135_degree_winds.standardised +
                            Drought_Months + (1|Estuary), data = bream)
AIC(m1e) # 187
r.squaredGLMM(m1e) # 0.468

anova(m1d, m1a) # best model is m1d and is sig dif. to next best

plot(m1d)
summary(m1d)
anova(m1e) # bream interaction with winds

plot(allEffects(m1a))

simulationOutput <- simulateResiduals(fittedModel = m1a, n = 250)
plot(simulationOutput)

library(bootpredictlme4)
# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81) )#,
                        #"Error" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           #"Estuary" = "Hawkesbury River",
                           #"Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m1a, newdata = pred_map, type = "response", se.fit = F, nsim = 500)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX#$fit
    #heat_data$Error[Nn] <- PredX$se.fit
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

#write.csv(heat_data, "Bream CPUE heatmap data.csv", row.names = FALSE)

# visualise the matrix
library(viridis)

p <- ggplot(heat_data, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Abundance)) +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + scale_fill_viridis() + # or geom_raster()
  xlab("Standardised Southeast Winds") + ylab("Standardised Northeast Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"))

p
