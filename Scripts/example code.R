library(lme4)
library(lmerTest)

mydata <- read.csv("example_data.csv", header = T)

m1 <- lmer(CPUE.standardised ~ poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months + (1|Estuary), data = mydata)

anova(m1)
summary(m1)

