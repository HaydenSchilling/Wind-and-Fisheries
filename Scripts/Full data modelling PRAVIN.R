# modelling

library(ggplot2)
library(DHARMa)
library(bootpredictlme4)
#library(merTools)
library(effects)
#library(MuMIn)
library(dplyr)
# Use Log10(CPUE) rather than standardise


# Load Data
mydata <- read.csv("../Full_Data_Modelling_BARRA_by_estuary.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv
# mydata2 <-  read.csv("Full_Data_Modelling.csv", header = T)
# 
# cor.test(mydata$X45_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X45_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X45_degree_winds, mydata2$X45_degree_winds)
# # NE winds not correlated by SE winds are.....

p1 <- ggplot(mydata, aes(x = X135_degree_winds, y = CPUE)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")

# Standardise CPUE for each species in each estuary
my.df <-  mydata %>% group_by(Estuary, Species) %>% mutate(CPUE.standardised = as.numeric(scale(CPUE)))
head(my.df)

p1 <- ggplot(my.df, aes(x = X135_degree_winds, y = CPUE.standardised)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1

### Standardise Winds
my.df <-  my.df %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))

p1 <- ggplot(my.df, aes(x = X135_degree_winds.standardised, y = CPUE.standardised)) + geom_point() +
  facet_wrap(~Species, scales = "free") + geom_smooth()
p1