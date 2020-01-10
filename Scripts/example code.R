library(lme4)
library(lmerTest)
library(readr)

mydata <- read_csv("https://raw.githubusercontent.com/HaydenSchilling/Example_code_and_data/master/example_data2.csv")

m1 <- lmer(CPUE.standardised ~ poly(cbind(X135_degree_winds.standardised, X45_degree_winds.standardised), degree = 2) + 
             Estuary_Type * Drought_Months + (1|Estuary), data = mydata)

anova(m1)
summary(m1)
r.squaredGLMM(m1)


m2 <- lmer(CPUE.standardised ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type * Drought_Months + (1|Estuary), data = mydata)
anova(m2)

anova(m1, m2)
r.squaredGLMM(m2)
summary(m2)

library(ggeffects)
ggpredict(m2, terms = "X135_degree_winds.standardised")

mydf <- ggpredict(m2, terms = "X135_degree_winds.standardised")
ggplot(mydf, aes(x, predicted)) +
  geom_line() #+
  #geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)


mydf <- ggpredict(m2, terms = c("X45_degree_winds.standardised"))
ggplot(mydf, aes(x, predicted)) +
  geom_line() #+
#geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)
