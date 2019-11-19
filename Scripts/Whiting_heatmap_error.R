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
mydata <- read.csv("Full_Data_Modelling.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv
# mydata2 <-  read.csv("Full_Data_Modelling.csv", header = T)
# 
# cor.test(mydata$X45_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X45_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X45_degree_winds, mydata2$X45_degree_winds)
# # NE winds not correlated by SE winds are.....
# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")

# 
# Whiting <- subset(mydata, Species == "Whiting")
# 
# m1 <- lm(log(CPUE)~ Onshore_Winds + Current_Wind + X135_degree_winds, data = Whiting)
# plot(m1)
# summary(m1)
# anova(m1)
# 
# total <- subset(mydata, Species == "Total")
# 
# m1 <- lm(log(CPUE)~ Onshore_Winds + Current_Wind + X135_degree_winds, data = total)
# plot(m1)
# summary(m1)
# anova(m1)

# Standardise CPUE for each species in each estuary

my.df <-  mydata %>% group_by(Estuary, Species) %>% mutate(CPUE.standardised = as.numeric(scale(CPUE)))
head(my.df)

p1 <- ggplot(my.df, aes(x = X135_degree_winds, y = CPUE.standardised)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1



### Standardise Winds
my.df <-  my.df %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))#, # Onshore winds removed



### Try random effect of estuary

Whiting <- subset(my.df, Species == "Whiting")

#m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type + Drought_Months + (1|Estuary), data = Whiting)
#AIC(m1) #215.42

m1 <- lmer(CPUE.standardised ~ poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months + (1|Estuary), data = Whiting)

AIC(m1) # 197.0064

plot(m1)
summary(m1)
anova(m1) # Whiting interaction with winds

### Make Predictions
str(Whiting)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X45_degree_winds.standardised" = -2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(Whiting$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Whiting <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
# 
# b <- bootMer(m1, nsim=100, 
#              FUN=function(x)predict(x, newdata=pred_dat, re.form=NA))
# 
# hist(b$t, #breaks=seq(250,350,by=5),
#      #ylim=c(0,25),
#      main="", xlab="Reaction time at 5 Days (ms)",
#      col="cornflowerblue")
# box()

#library(devtools)
#install_github("remkoduursma/bootpredictlme4")

library(bootpredictlme4)
Pred_Whiting <- predict(m1, newdata=pred_dat,type= "response", re.form=NA, se.fit = T, nsim = 500)
Pred_Whiting

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Whiting$fit, type = "l",
#     ylab = "Predicted Whiting CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Whiting$fit-Pred_Whiting$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Whiting$fit+Pred_Whiting$se.fit), type = "l", col = "blue")

Whiting_plot_dat <- data.frame(Pred_Whiting$fit, Pred_Whiting$se.fit, pred_dat$X135_degree_winds.standardised)
head(Whiting_plot_dat)

p4 <- ggplot(Whiting_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_Whiting.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Whiting Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_Whiting.fit+Pred_Whiting.se.fit, ymin = Pred_Whiting.fit-Pred_Whiting.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4 ### need to do multiple levels of NE winds, weaker shows effect of SE

#ggsave("plots/Whiting CPUE and SE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
#ggsave("plots/Whiting CPUE and SE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)

### Make Predictions for NE
str(Whiting)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                             to =2, by = 0.05),
                       "X135_degree_winds.standardised" = -2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(Whiting$Current_Wind),
                       "Drought_Months" = 4)





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
                           #"Current_Wind" = mean(Whiting$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m1, newdata = pred_map, type = "response", se.fit = T, nsim = 300)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX$fit
    heat_data$Error[Nn] <- PredX$se.fit
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

write.csv(heat_data, "Whiting CPUE heatmap data.csv", row.names = FALSE)