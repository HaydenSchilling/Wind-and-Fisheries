# modelling

library(ggplot2)

# Load Data
mydata <- read.csv("Full_Data_Modelling.csv", header = T)

p1 <- ggplot(mydata, aes(x = Onshore_Winds, y = CPUE)) + geom_point() +
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

p1 <- ggplot(my.df, aes(x = Onshore_Winds, y = CPUE.standardised)) + geom_point() +
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
                                   X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))

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

m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
           Estuary_Type + Drought_Months + (1|Estuary), data = bream)
plot(m1)
summary(m1)
anova(m1) # bream increase with SE Winds

plot(allEffects(m1))

simulationOutput <- simulateResiduals(fittedModel = m1, n = 250)
plot(simulationOutput)

# histograms
hist(simulationOutput$fittedResiduals)

### Make Predictions
str(bream)
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
p4

ggsave("plots/Bream CPUE and wind predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Bream CPUE and wind predictions.png", height = 14.8, width = 21, units = "cm", dpi = 600)


### Now Mullet

mullet <- subset(my.df, Species == "Mullet")

m2 <- lmer(CPUE.standardised~  X135_degree_winds.standardised * X45_degree_winds.standardised + 
           Estuary_Type * Drought_Months +
           (1|Estuary), data = mullet)

#m2 <- glmer.nb(CPUE~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#                 Estuary_Type * Drought_Months +
#                 (1|Estuary), data = mullet)

plot(m2)

simulationOutput <- simulateResiduals(fittedModel = m2, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m2)
anova(m2) # No effects from mullet

plot(allEffects(m2))

# Flathead
flathead <- subset(my.df, Species == "Flathead")

m3 <- lmer(CPUE.standardised~  X135_degree_winds.standardised * X45_degree_winds.standardised + 
           Estuary_Type * Drought_Months +
           (1|Estuary), data = flathead)
plot(m3)

simulationOutput <- simulateResiduals(fittedModel = m3, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m3)
anova(m3) # No effects for Flathead

plot(allEffects(m3))

luderick <- subset(my.df, Species == "Luderick")

# Current wind may help for luderick
#m4 <- lmer(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#           Estuary_Type + Drought_Months +
#           (1|Estuary), data = luderick)

m4 <- lmer(CPUE.standardised~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
             Estuary_Type + Drought_Months +
             (1|Estuary), data = luderick)
plot(m4)

simulationOutput <- simulateResiduals(fittedModel = m4, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m4)
anova(m4)

plot(allEffects(m4)) # weak evidence for an interaction between NE and SE Winds
plot(Effect(c("SE_Winds.standardised","NE_Winds.standardised"), fit2))

whiting <- subset(my.df, Species == "Whiting")

m5 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
           Estuary_Type * Drought_Months +
           (1|Estuary), data = whiting)
plot(m5)

simulationOutput <- simulateResiduals(fittedModel = m5, n = 250)
plot(simulationOutput)

hist(simulationOutput$fittedResiduals)

summary(m5)
anova(m5) # drought increases catch of Whiting, no wind effects

plot(allEffects(m5))

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
# m7 <- lmer(CPUE.standardised~ Current_Wind + X135_degree_winds.standardised * X45_degree_winds.standardised + 
#             Estuary_Type *Drought_Months +
#              (1|Estuary) + (1|Species), data = my.df)
# plot(m7)
# summary(m7)
# anova(m7) # drought increases catch of Whiting, no wind effects

# plot(allEffects(m7))
