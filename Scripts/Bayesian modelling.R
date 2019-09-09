# Trying bayesian models

#install.packages("brms")

library(brms)

# modelling

library(ggplot2)

# Load Data
mydata <- read.csv("Full_Data_Modelling.csv", header = T)

p1 <- ggplot(mydata, aes(x = Onshore_Winds, y = CPUE)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")



# Standardise CPUE for each species in each estuary
library(dplyr)
my.df <-  mydata %>% group_by(Estuary, Species) %>% mutate(CPUE.standardised = as.numeric(scale(CPUE)))
head(my.df)

p1 <- ggplot(my.df, aes(x = Onshore_Winds, y = CPUE.standardised)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

library(effects)



### Standardise Winds
my.df <-  my.df %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))




### Try random effect of estuary

library(lme4)
library(lmerTest)
bream <- subset(my.df, Species == "Bream")

m1 <- lmer(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
             Estuary_Type + Drought_Months + (1|Estuary), data = bream)

library(merTools)
fastdisp(m1)

feEx <- FEsim(m1, 1000)
cbind(feEx[,1] , round(feEx[, 2:4], 3))


plotFEsim(feEx) + 
  theme_bw() + labs(title = "Coefficient Plot of InstEval Model", 
                    x = "Median Effect Estimate", y = "Evaluation Rating")

reEx <- REsim(m1)
head(reEx)

p1 <- plotREsim(reEx) + coord_flip() + geom_text(reEx, aes(label = groupID))
p1


coefplot(m1B)

m1B <- brm(CPUE.standardised ~ X135_degree_winds.standardised * X45_degree_winds.standardised + 
              Estuary_Type + Drought_Months + (1|Estuary), data = bream, control = list(adapt_delta = 0.99))

plot(m1B)
summary(m1B)
marginal_effects(m1B)
coefplot(m1B)

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
