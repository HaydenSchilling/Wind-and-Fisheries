# modelling

library(ggplot2)
library(DHARMa)
library(bootpredictlme4)
library(merTools)
library(MuMIn)

# Load Data
mydata <- read.csv("Full_Data_Modelling.csv", header = T)

p1 <- ggplot(mydata, aes(x = X135_degree_winds, y = CPUE)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")

# 
# bream <- subset(mydata, Species == "Bream")
# 
# m1 <- lm(log(CPUE)~ Onshore_Winds + Current_Wind + X135_degree_winds, data = bream)
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
library(dplyr)
my.df <-  mydata %>% group_by(Estuary, Species) %>% mutate(CPUE.standardised = as.numeric(scale(CPUE)))
head(my.df)

p1 <- ggplot(my.df, aes(x = X135_degree_winds, y = CPUE.standardised)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

library(effects)

# # Raw data modelling

# bream <- subset(my.df, Species == "Bream")
# 
# m1 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds * X45_degree_winds + Estuary_Type + Drought_Months, data = bream)
# plot(m1)
# summary(m1)
# anova(m1)
# 
# plot(allEffects(m1))
# 
# mullet <- subset(my.df, Species == "Mullet")
# 
# m2 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds * X45_degree_winds + Estuary_Type + Drought_Months, data = mullet)
# plot(m2)
# summary(m2)
# anova(m2)
# 
# plot(allEffects(m2))
# 
# flathead <- subset(my.df, Species == "Flathead")
# 
# m3 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds * X45_degree_winds + Estuary_Type + Drought_Months, data = flathead)
# plot(m3)
# summary(m3)
# anova(m3)
# 
# plot(allEffects(m3))
# 
# luderick <- subset(my.df, Species == "Luderick")
# 
# m4 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds * X45_degree_winds + Estuary_Type + Drought_Months, data = luderick)
# plot(m4)
# summary(m4)
# anova(m4)
# 
# plot(allEffects(m4))
# 
# whiting <- subset(my.df, Species == "Whiting")
# 
# m5 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds * X45_degree_winds + Estuary_Type + Drought_Months, data = whiting)
# plot(m5)
# summary(m5)
# anova(m5)
# 
# plot(allEffects(m5))
# 
# total <- subset(my.df, Species == "Total")
# 
# m6 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds * X45_degree_winds + Estuary_Type + Drought_Months, data = total)
# plot(m6)
# summary(m6)
# anova(m6)
# 
# plot(allEffects(m6))

### Standardise Winds
my.df <-  my.df %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                                   X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)),
                           Onshore_Winds.standardised = as.numeric(scale(Onshore_Winds)))

# bream <- subset(my.df, Species == "Bream")
# 
# m1 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#            Estuary_Type + Drought_Months, data = bream)
# plot(m1)
# summary(m1)
# anova(m1)
# 
# plot(allEffects(m1))
# 
# mullet <- subset(my.df, Species == "Mullet")
# 
# m2 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#            Drought_Months+Estuary_Type, data = mullet)
# plot(m2)
# summary(m2)
# anova(m2)
# 
# plot(allEffects(m2))
# 
# flathead <- subset(my.df, Species == "Flathead")
# 
# m3 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#            Estuary_Type + Drought_Months, data = flathead)
# plot(m3)
# summary(m3)
# anova(m3)
# 
# plot(allEffects(m3))
# 
# luderick <- subset(my.df, Species == "Luderick")
# 
# m4 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#            Estuary_Type + Drought_Months, data = luderick)
# plot(m4)
# summary(m4)
# anova(m4)
# 
# plot(allEffects(m4))
# 
# whiting <- subset(my.df, Species == "Whiting")
# 
# m5 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#            Estuary_Type + Drought_Months, data = whiting)
# plot(m5)
# summary(m5)
# anova(m5)
# 
# plot(allEffects(m5))
# 
# # total <- subset(my.df, Species == "Total")
# # 
# # m6 <- lm(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
# #            Estuary_Type + Drought_Months, data = total)
# # plot(m6)
# # summary(m6)
# # anova(m6)
# 
# # plot(allEffects(m6))



### Try random effect of estuary

library(lme4)
library(lmerTest)
bream <- subset(my.df, Species == "Bream")

#m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type + Drought_Months + (1|Estuary), data = bream)
#AIC(m1) #215.42

m1 <- lmer(CPUE.standardised ~ poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months + (1|Estuary), data = bream)

AIC(m1) # 197.0064

plot(m1)
summary(m1)
anova(m1) # bream interaction with winds

plot(allEffects(m1))

simulationOutput <- simulateResiduals(fittedModel = m1, n = 250)
plot(simulationOutput)

# histograms
hist(simulationOutput$fittedResiduals)

#autocorrelation
res <- residuals(m1)
acf(res, plot = T)
head(res, type = "pearson")

library(ggfortify)
acf_p <- autoplot(acf(res)) + #simulationOutput$fittedResiduals
  geom_hline(yintercept = 0) +
  ylab('Autocorrelation function')
acf_p



fastdisp(m1)

feEx <- FEsim(m1, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_bw() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")


### Make Predictions
str(bream)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                  to =2, by = 0.05),
                    "X45_degree_winds.standardised" = -2,
                    "Estuary" = "Hawkesbury River",
                    "Estuary_Type" = "Barrier Lagoon",
                    #"Current_Wind" = mean(bream$Current_Wind),
                    "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_Bream <- predict(m1, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_Bream

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

bream_plot_dat <- data.frame(Pred_Bream$fit, Pred_Bream$se.fit, pred_dat$X135_degree_winds.standardised)
head(bream_plot_dat)

p4 <- ggplot(bream_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_Bream.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Bream Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_Bream.fit+Pred_Bream.se.fit, ymin = Pred_Bream.fit-Pred_Bream.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4 ### need to do multiple levels of NE winds, weaker shows effect of SE

ggsave("plots/Bream CPUE and SE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Bream CPUE and SE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)

### Make Predictions for NE
str(bream)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X135_degree_winds.standardised" = -2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_Bream <- predict(m1, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_Bream

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

bream_plot_dat <- data.frame(Pred_Bream$fit, Pred_Bream$se.fit, pred_dat$X45_degree_winds.standardised)
head(bream_plot_dat)

p4 <- ggplot(bream_plot_dat, aes(x = pred_dat.X45_degree_winds.standardised, y = Pred_Bream.fit)) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Bream Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_Bream.fit+Pred_Bream.se.fit, ymin = Pred_Bream.fit-Pred_Bream.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4 ### need to do multiple levels of NE winds, weaker shows effect of SE
ggsave("plots/Bream CPUE and NE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Bream CPUE and NE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)



# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m1, newdata = pred_map, type = "response", se.fit = F)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

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

ggsave("plots/Bream CPUE and NE_SE wind interaction predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Bream CPUE and NE_SE wind interaction predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)

### Now Mullet

mullet <- subset(my.df, Species == "Mullet")

#m2 <- lmer(CPUE.standardised~  X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type * Drought_Months +
#           (1|Estuary), data = mullet)
#AIC(m2) # 241.189

m2 <- lmer(CPUE.standardised~  poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months +
             (1|Estuary), data = mullet)
AIC(m2) # 223.645


plot(m2)

simulationOutput <- simulateResiduals(fittedModel = m2, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m2)
anova(m2) # Wind interaction effects from mullet

plot(allEffects(m2))

fastdisp(m2)

feEx <- FEsim(m2, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_bw() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")


### Make Predictions
str(mullet)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X45_degree_winds.standardised" = -2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_mullet <- predict(m2, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_mullet

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

mullet_plot_dat <- data.frame(Pred_mullet$fit, Pred_mullet$se.fit, pred_dat$X135_degree_winds.standardised)
head(mullet_plot_dat)

p4 <- ggplot(mullet_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_mullet.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Mullet Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_mullet.fit+Pred_mullet.se.fit, ymin = Pred_mullet.fit-Pred_mullet.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/Mullet CPUE and SE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Mullet CPUE and SE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


### Make Predictions for NE
str(mullet)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X135_degree_winds.standardised" = -2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_mullet <- predict(m2, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_mullet

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

mullet_plot_dat <- data.frame(Pred_mullet$fit, Pred_mullet$se.fit, pred_dat$X45_degree_winds.standardised)
head(mullet_plot_dat)

p4 <- ggplot(mullet_plot_dat, aes(x = pred_dat.X45_degree_winds.standardised, y = Pred_mullet.fit)) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Mullet Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_mullet.fit+Pred_mullet.se.fit, ymin = Pred_mullet.fit-Pred_mullet.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/Mullet CPUE and NE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Mullet CPUE and NE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)

# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m2, newdata = pred_map, type = "response", se.fit = F)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

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

ggsave("plots/Mullet CPUE and NE_SE wind interaction predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Mullet CPUE and NE_SE wind interaction predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


# Flathead
flathead <- subset(my.df, Species == "Flathead")

#m3 <- lmer(CPUE.standardised~  X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type * Drought_Months +
#           (1|Estuary), data = flathead)
#AIC(m3) # 243.302
m3 <- lmer(CPUE.standardised~  poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months +
             (1|Estuary), data = flathead)
AIC(m3) # 221.5996

plot(m3)

simulationOutput <- simulateResiduals(fittedModel = m3, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m3)
anova(m3) 

plot(allEffects(m3))

fastdisp(m3)

feEx <- FEsim(m3, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_bw() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")



### Make Predictions
str(flathead)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X45_degree_winds.standardised" = -1,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_flathead <- predict(m3, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_flathead

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

flathead_plot_dat <- data.frame(Pred_flathead$fit, Pred_flathead$se.fit, pred_dat$X135_degree_winds.standardised)
head(flathead_plot_dat)

p4 <- ggplot(flathead_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_flathead.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Flathead Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_flathead.fit+Pred_flathead.se.fit, ymin = Pred_flathead.fit-Pred_flathead.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/flathead CPUE and SE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/flathead CPUE and SE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)

### Make Predictions
str(flathead)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X135_degree_winds.standardised" = -1,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_flathead <- predict(m3, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_flathead

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

flathead_plot_dat <- data.frame(Pred_flathead$fit, Pred_flathead$se.fit, pred_dat$X45_degree_winds.standardised)
head(flathead_plot_dat)

p4 <- ggplot(flathead_plot_dat, aes(x = pred_dat.X45_degree_winds.standardised, y = Pred_flathead.fit)) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Flathead Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_flathead.fit+Pred_flathead.se.fit, ymin = Pred_flathead.fit-Pred_flathead.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/flathead CPUE and NE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/flathead CPUE and NE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)

# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m3, newdata = pred_map, type = "response", se.fit = F)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

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

ggsave("plots/Flathead CPUE and NE_SE wind interaction predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Flathead CPUE and NE_SE wind interaction predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)




### Luderick

luderick <- subset(my.df, Species == "Luderick")

#m4 <- lmer(CPUE.standardised~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
#             Estuary_Type + Drought_Months +
#             (1|Estuary), data = luderick)
#AIC(m4) # 248.3677

m4 <- lmer(CPUE.standardised~ poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months +
             (1|Estuary), data = luderick)
AIC(m4) # 228.734

plot(m4)

simulationOutput <- simulateResiduals(fittedModel = m4, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m4)
anova(m4, type = 1) # weak effect of NE Winds

plot(allEffects(m4)) # weak evidence for an interaction between NE and SE Winds
plot(Effect(c("X45_degree_winds.standardised"), m4))


fastdisp(m4)

feEx <- FEsim(m4, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_bw() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")



### Make Predictions
str(luderick)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X45_degree_winds.standardised" = 0,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_luderick <- predict(m4, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_luderick

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

luderick_plot_dat <- data.frame(Pred_luderick$fit, Pred_luderick$se.fit, pred_dat$X135_degree_winds.standardised)
head(luderick_plot_dat)

p4 <- ggplot(luderick_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_luderick.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("luderick Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_luderick.fit+Pred_luderick.se.fit, ymin = Pred_luderick.fit-Pred_luderick.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/luderick CPUE and SE wind predictions mean.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/luderick CPUE and SE wind predictions mean.png", height = 14.8, width = 21, units = "cm", dpi = 600)


### Make Predictions for NE
str(luderick)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X135_degree_winds.standardised" = -2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_luderick <- predict(m4, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_luderick

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

luderick_plot_dat <- data.frame(Pred_luderick$fit, Pred_luderick$se.fit, pred_dat$X45_degree_winds.standardised)
head(luderick_plot_dat)

p4 <- ggplot(luderick_plot_dat, aes(x = pred_dat.X45_degree_winds.standardised, y = Pred_luderick.fit)) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("luderick Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_luderick.fit+Pred_luderick.se.fit, ymin = Pred_luderick.fit-Pred_luderick.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/luderick CPUE and NE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/luderick CPUE and NE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m4, newdata = pred_map, type = "response", se.fit = F)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

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

ggsave("plots/Luderick CPUE and NE_SE wind interaction predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Luderick CPUE and NE_SE wind interaction predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)




## Whiting

whiting <- subset(my.df, Species == "Whiting")

# m5 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
#            Estuary_Type * Drought_Months +
#            (1|Estuary), data = whiting)
# AIC(m5) # 242.887

m5 <- lmer(CPUE.standardised ~ poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months +
             (1|Estuary), data = whiting)
AIC(m5) # 217.227

plot(m5)

simulationOutput <- simulateResiduals(fittedModel = m5, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m5)
anova(m5) # drought increases catch of Whiting, no wind effects

plot(allEffects(m5))


fastdisp(m5)

feEx <- FEsim(m5, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_bw() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")




### Make Predictions
str(whiting)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X45_degree_winds.standardised" = 2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_whiting <- predict(m5, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_whiting

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

whiting_plot_dat <- data.frame(Pred_whiting$fit, Pred_whiting$se.fit, pred_dat$X135_degree_winds.standardised)
head(whiting_plot_dat)

p4 <- ggplot(whiting_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_whiting.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Whiting Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_whiting.fit+Pred_whiting.se.fit, ymin = Pred_whiting.fit-Pred_whiting.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/whiting CPUE and SE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/whiting CPUE and SE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


### Make Predictions NE Winds
str(whiting)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X135_degree_winds.standardised" = 2,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4)

# # Old prediction code - does not do SE

# Pred_Bream <- predict(m1, newdata = pred_dat, type = "response", se.fit = T)
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
Pred_whiting <- predict(m5, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_whiting

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

whiting_plot_dat <- data.frame(Pred_whiting$fit, Pred_whiting$se.fit, pred_dat$X45_degree_winds.standardised)
head(whiting_plot_dat)

p4 <- ggplot(whiting_plot_dat, aes(x = pred_dat.X45_degree_winds.standardised, y = Pred_whiting.fit)) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Whiting Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_whiting.fit+Pred_whiting.se.fit, ymin = Pred_whiting.fit-Pred_whiting.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4

ggsave("plots/whiting CPUE and NE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/whiting CPUE and NE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4)
    PredX <- predict(m5, newdata = pred_map, type = "response", se.fit = F)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

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

ggsave("plots/Whiting CPUE and NE_SE wind interaction predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Whiting CPUE and NE_SE wind interaction predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)




# total <- subset(my.df, Species == "Total")
# str(total)
# 
# m6 <- lmer(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised +
#              Drought_Months * Estuary_Type +
#              (1|Estuary), data = total) # no change if other drought factors included instead
# plot(m6)
# summary(m6)
# anova(m6)
# plot(allEffects(m6)) # No effects for total CPUE

# Test Single model with Species as a random effect
 m7 <- lmer(CPUE.standardised~  poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) +
            Estuary_Type *Drought_Months +
             (Species|Estuary), data = my.df) # AIC 1131.193
AIC(m7) # 1095.78
  

r.squaredGLMM(m7) # 0.14
 
#m7 <- lmer(CPUE.standardised~  X135_degree_winds.standardised *X45_degree_winds.standardised+
#             Estuary_Type *Drought_Months +
#             (Species|Estuary), data = my.df) # AIC 1129.727
#AIC(m7)
#library(glmmTMB)
#m7 <- glmmTMB(CPUE.standardised~  X135_degree_winds.standardised* X45_degree_winds.standardised +
#             Estuary_Type *Drought_Months +
#             (Species|Estuary), family = gaussian(), data = my.df)

#fit3 <- glmmTMB(Coastal_Normalised_Abund ~ SE_Winds.standardised * NE_Winds.standardised*dists_km + (1|Project_ID), 
#                family=tweedie(), data = fish_data)
plot(m7)
summary(m7)
anova(m7) # drought increases catch of Whiting, no wind effects

coef(m7)


plot(allEffects(m7))
plot(Effect(c("Estuary_Type"), m7))

plot(m7)

simulationOutput <- simulateResiduals(fittedModel = m7, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)


fastdisp(m7)

feEx <- FEsim(m7, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_classic() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")

### Make Predictions
str(my.df)
pred_dat <- data.frame("X135_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X45_degree_winds.standardised" = 1,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4,
                       "Species" == "Bream")

library(bootpredictlme4)
Pred_Total <- predict(m7, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)


# Pred_Total <- predict(m7, newdata=my.df, re.form=NA, se.fit=TRUE, nsim=100)
# 
# fitX <- lm(Pred_Total$fit~ my.df$CPUE.standardised)
# summary(fitX)
# 
# plot( my.df$CPUE.standardised,Pred_Total$fit, xlim = c(-2.5,2.5), ylim = c(-2.5,2.5))
# abline(fitX)
# abline(a = 0, b=1, col = "red")

Pred_Total

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

total_plot_dat <- data.frame(Pred_Total$fit, Pred_Total$se.fit, pred_dat$X135_degree_winds.standardised)
head(total_plot_dat)

p4 <- ggplot(total_plot_dat, aes(x = pred_dat.X135_degree_winds.standardised, y = Pred_Total.fit)) +
  theme_classic() + xlab("Standardised Southeast Winds") + ylab("Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_Total.fit+Pred_Total.se.fit, ymin = Pred_Total.fit-Pred_Total.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4 ### need to do multiple levels of NE winds, weaker shows effect of SE

ggsave("plots/All Species CPUE and SE wind predictions strong.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/All Species CPUE and SE wind predictions strong.png", height = 14.8, width = 21, units = "cm", dpi = 600)

### Make Predictions NE WINDS
str(my.df)
pred_dat <- data.frame("X45_degree_winds.standardised" = seq(from = -2,
                                                              to =2, by = 0.05),
                       "X135_degree_winds.standardised" = 0,
                       "Estuary" = "Hawkesbury River",
                       "Estuary_Type" = "Barrier Lagoon",
                       #"Current_Wind" = mean(bream$Current_Wind),
                       "Drought_Months" = 4,
                       "Species" == "Bream")

library(bootpredictlme4)
Pred_Total <- predict(m7, newdata=pred_dat, re.form=NA, se.fit=TRUE, nsim=100)

Pred_Total

#plot(x = pred_dat$X135_degree_winds.standardised, y=Pred_Bream$fit, type = "l",
#     ylab = "Predicted Bream CPUE standardised", xlab = "Standardised Southeast Winds",
#     main = "Predicted coastal species abundance \nfor no drought and mean NE winds") #, ylim=c(0,1)
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit-Pred_Bream$se.fit), type = "l", col = "blue")
#lines(x = pred_dat$X135_degree_winds.standardised, y=(Pred_Bream$fit+Pred_Bream$se.fit), type = "l", col = "blue")

total_plot_dat <- data.frame(Pred_Total$fit, Pred_Total$se.fit, pred_dat$X45_degree_winds.standardised)
head(total_plot_dat)

p4 <- ggplot(total_plot_dat, aes(x = pred_dat.X45_degree_winds.standardised, y = Pred_Total.fit)) +
  theme_classic() + xlab("Standardised Northeast Winds") + ylab("Predicted Standardised CPUE") +
  geom_ribbon(aes(ymax = Pred_Total.fit+Pred_Total.se.fit, ymin = Pred_Total.fit-Pred_Total.se.fit), 
              fill = "grey80", col = "grey80") + 
  geom_line(col = "blue", size = 1.5) +
  theme(axis.title = element_text(face="bold", colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        axis.text=element_text(size=12, face = "bold", colour = "black"))
p4 ### need to do multiple levels of NE winds, weaker shows effect of SE

ggsave("plots/All Species CPUE and NE wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/All Species CPUE and NE wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)



# Heatmap of interactions
nums <- seq(-2,2, by = 0.05)
heat_data <- data.frame("Southeast Winds" = rep(0,81*81), # makes empty dataframe ready for values
                        "Northeast Winds" =  rep(0,81*81), 
                        "Abundance" =  rep(0,81*81))
Nn <- 1
# loop for NE
for (i in 1:length(nums)){
  for (j in 1:length(nums)){
    pred_map <- data.frame("X45_degree_winds.standardised" = nums[i],
                           "X135_degree_winds.standardised" = nums[j],
                           "Estuary" = "Hawkesbury River",
                           "Estuary_Type" = "Barrier Lagoon",
                           #"Current_Wind" = mean(bream$Current_Wind),
                           "Drought_Months" = 4,
                           "Species" = "Bream")
    PredX <- predict(m7, newdata = pred_map, type = "response", se.fit = F)
    heat_data$Southeast.Winds[Nn] <- pred_map$X135_degree_winds.standardised[1]
    heat_data$Northeast.Winds[Nn] <- pred_map$X45_degree_winds.standardised[1]
    heat_data$Abundance[Nn] <- PredX
    print(paste("This is line ", Nn, " out of 6561"))
    Nn <- Nn + 1
  }
}

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

ggsave("plots/All Species NE_SE wind interaction predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/All Species NE_SE wind interaction predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


p <- ggplot(heat_data, aes(x = Southeast.Winds,y = Northeast.Winds, z = Abundance)) + geom_tile(aes(fill = Abundance)) + stat_contour() +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + scale_fill_viridis() + # or geom_raster()
  xlab("Standardised Southeast Winds") + ylab("Standardised Northeast Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"))

p

