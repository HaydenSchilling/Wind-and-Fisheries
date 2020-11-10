# Analysis of modelled wind data changing over time

library(lubridate)
library(ggplot2)
library(dplyr)

# First load NOAA data and BARRA data and compare

mydata <- read.csv("../Data/NOAA Winds V2c/Sydney_Daily Modelled Wind Data Final 135 degree.csv", header = T)
#soi_data <- read.csv("SOI data long.csv", header = T)
B_dat <- read.csv("../Data/BARRA Data/BARRA_Sydney_Monthly Modelled Wind Data Final 135 degree.csv")
B_dat2 <- B_dat %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))


head(mydata)

table(mydata$Year)

#mydata <- left_join(mydata, soi_data, by = c("Year", "Month"))

mydata$Date <- paste0(mydata$Year,"-",mydata$Month,"-",mydata$Day)
mydata$Date <- as.Date(mydata$Date)

str(mydata)

dat <- mydata %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))
head(dat)

datX <- left_join(B_dat2, dat, by = "Year")
cor.test(datX$Annual_displacement.x, datX$Annual_displacement.y)

## Therefore BARRA Model and NOAA v3 Model are significantly correlated. p = 0.00001, r = 0.696

#write.csv(dat, "Iain Annual 135 degree Sydney.csv", row.names = F)

plot(dat$Year, dat$Annual_displacement)
points(B_dat2$Year, B_dat2$Annual_displacement, col = "red")
dat2 <- dat
str(dat2)
fit1 <- lm(Annual_displacement ~ Year, data = dat2)
plot(fit1)

hist(fit1$residuals)

summary(fit1) # -17.94 decline per year (p = 0.094)
# 164 * -17 = -2942
anova(fit1)
plot(dat2$Year, dat2$Annual_displacement)
abline(fit1)


res <- residuals(fit1)
acf(res, plot = T) # no autocorelation

p1 <- ggplot(dat2, aes(x = Year, y = Annual_displacement)) + geom_point() + geom_smooth(method = "lm") +
  theme_classic() + ylab("Annual Displacement") + xlab("Year") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        #strip.text = element_text(colour="black", face = "bold", size = 14),
        #strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1)
        #legend.key.size = unit(1, "cm"),
        #legend.title = element_text(face = "bold", size = 14),
        #legend.text = element_text(size = 12, face = "bold"))
  )
p1

#ggsave("../plots/Historical SE Wind ChangeV3.pdf", height = 14.8, width = 21, units = "cm")
#ggsave("../plots/Historical SE Wind ChangeV3.png", height = 14.8, width = 21, units = "cm", dpi = 600)


### Now do NE Winds
NEdata <- read.csv("../Data/NOAA Winds V2c/Sydney_Daily Modelled Wind Data Final 45 degree.csv", header = T)


NEdata$Date <- paste0(NEdata$Year,"-",NEdata$Month,"-",NEdata$Day)
NEdata$Date <- as.Date(NEdata$Date)

str(NEdata)

dat_NE <- NEdata %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))
head(dat_NE)

B_dat <- read.csv("../Data/BARRA Data/BARRA_Sydney_Monthly Modelled Wind Data Final 45 degree.csv")
B_dat2 <- B_dat %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))

datX <- left_join(B_dat2, dat_NE, by = "Year")
cor.test(datX$Annual_displacement.x, datX$Annual_displacement.y)
datX # BARRA is correlated to NOAA v3 r = 0.599, p = 0.00155

plot(dat_NE$Year, dat_NE$Annual_displacement)

dat2_NE <- subset(dat_NE, Year >= 1849)

fit2 <- lm(Annual_displacement ~ Year, data = dat2_NE)
plot(fit2)

hist(fit2$residuals)

summary(fit2) # increase by 48.307 per year (p < 0.001)
# 48 * 164 = 7872
anova(fit2)
plot(dat2_NE$Year, dat2_NE$Annual_displacement)
abline(fit2)


res <- residuals(fit2)
acf(res, plot = T) #  autocorrelation at up to 3 year lag
head(res, type = "pearson")

## Try to remove autocorrelation
NEdata <- read.csv("../Data/NOAA Winds V2c/Sydney_Daily Modelled Wind Data Final 45 degree.csv", header = T)


NEdata$Date <- paste0(NEdata$Year,"-",NEdata$Month,"-",NEdata$Day)
NEdata$Date <- as.Date(NEdata$Date)

str(NEdata)

dat_NE <- NEdata %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))
head(dat_NE)

#write.csv(dat_NE, "Iain Annual 45 degree winds Sydney.csv", row.names = F)

plot(dat_NE$Year, dat_NE$Annual_displacement)

# Select every 3rd year
dat_NE2 <- dat_NE %>%
  slice(which(row_number() %% 3 == 1))

fit2 <- lm(Annual_displacement ~ Year, data = dat_NE2)
plot(fit2)

hist(fit2$residuals)

summary(fit2) # increase by 52.64 per year (p = 0.002)
# 52.64 * 164 = 8632
anova(fit2)
plot(dat2_NE$Year, dat2_NE$Annual_displacement)
abline(fit2)


res <- residuals(fit2)
acf(res, plot = T) # No more autocorrelation

# Make model assumption checking plots which were combined into single figures in Inkscape manually
png("../plots/Model checks/NE time1.png", width = 21, height = 14.8, units = "cm", res = 600)
par(mfrow=c(2,2))
plot(fit2)
dev.off()

png("../plots/Model checks/NE time2.png", width = 21, height = 14.8, units = "cm", res = 600)
hist(fit2$residuals)
dev.off()

png("../plots/Model checks/NE time3.png", width = 21, height = 14.8, units = "cm", res = 600)
res <- residuals(fit2)
acf(res, plot = T)
dev.off()

p2 <- ggplot(dat_NE2, aes(x = Year, y = Annual_displacement)) + geom_point() + geom_smooth(method = "lm") +
  theme_classic() + ylab("Annual Displacement") + xlab("Year") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        #strip.text = element_text(colour="black", face = "bold", size = 14),
        #strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1)
        #legend.key.size = unit(1, "cm"),
        #legend.title = element_text(face = "bold", size = 14),
        #legend.text = element_text(size = 12, face = "bold"))
  )
p2

## Now use every 3rd from from SE winds too
dat2_2 <- dat2 %>%
  slice(which(row_number() %% 3 == 1))


fit1 <- lm(Annual_displacement ~ Year, data = dat2_2)
plot(fit1)

hist(fit1$residuals)


### Try brms
library(brms)
fit1b <- brm(Annual_displacement ~ Year, data = dat2_2, iter = 10000, seed = 1234)

plot(fit1b)
summary(fit1b)

post_samplesM1_tbl <-
  posterior_samples(fit1b) %>%
  select(-lp__) %>%
  round(digits = 3)
post_samplesM1_tbl %>%
  head(10) %>%
  knitr::kable(align = rep("c", 3))


library(tidybayes)
get_variables(fit1b)

fit1b %>%
  spread_draws(b_Year) %>% #b_Intercept,
  rename( "Year" = b_Year) %>% #"Intercept" = b_Intercept,
  tidyr::pivot_longer(cols = c(4:4), names_to = "Variable") %>%
  #median_qi() %>%
  ggplot(aes(x = value, y = Variable)) +
  stat_halfeye(normalize = "groups") + geom_vline(xintercept =0 , col = "red", lty = 2) +
  xlab("Estimate")+
  theme_classic() + theme(axis.text = element_text(colour="black"),
                          axis.title = element_text(face = "bold"))# +


#mean_regressionM1_fig <-
  dat2_2 %>%
  ggplot(aes(x = Year, y = Annual_displacement)) +
  geom_point(
    colour = "black",
    size = 2,
    alpha = 1
  ) +
  geom_abline(aes(intercept = b_Intercept, slope = b_Year),
              data = post_samplesM1_tbl,
              alpha = 0.01, color = "gray50"
  ) +
  geom_abline(
    slope = mean(post_samplesM1_tbl$b_Year),
    intercept = mean(post_samplesM1_tbl$b_Intercept),
    color = "blue", size = 1
  ) +
    theme_classic() + ylab("Annual Displacement") + xlab("Year") +
    theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
          axis.text.x  = element_text(colour="black", size = 12), 
          axis.title.y = element_text(face="bold", colour="black", size = 18),
          axis.text.y  = element_text(colour="black", size = 14),
          axis.ticks = element_line(colour="black"),
          #strip.text = element_text(colour="black", face = "bold", size = 14),
          #strip.background = element_rect(colour = "white"),
          #legend.justification=c(1,0), legend.position="right",
          panel.border = element_rect(colour = "black", fill=NA, size = 1))#+


### Repeat for NE winds bayesian
#dat_NE2

library(brms)
fitNb <- brm(Annual_displacement ~ Year, data = dat_NE2, iter = 10000, seed = 1234)

plot(fitNb)
summary(fitNb)

post_samplesM1_tbl_N <-
  posterior_samples(fitNb) %>%
  select(-lp__) %>%
  round(digits = 3)
post_samplesM1_tbl_N %>%
  head(10) %>%
  knitr::kable(align = rep("c", 3))


library(tidybayes)
get_variables(fitNb)

fitNb %>%
  spread_draws(b_Year) %>% #b_Intercept,
  rename( "Year" = b_Year) %>% #"Intercept" = b_Intercept,
  tidyr::pivot_longer(cols = c(4:4), names_to = "Variable") %>%
  #median_qi() %>%
  ggplot(aes(x = value, y = Variable)) +
  stat_halfeye(normalize = "groups") + geom_vline(xintercept =0 , col = "red", lty = 2) +
  xlab("Estimate")+
  theme_classic() + theme(axis.text = element_text(colour="black"),
                          axis.title = element_text(face = "bold"))# +

#mean_regressionM1_fig <-
dat_NE2 %>%
  ggplot(aes(x = Year, y = Annual_displacement)) +
  geom_point(
    colour = "black",
    size = 2,
    alpha = 1
  ) +
  geom_abline(aes(intercept = b_Intercept, slope = b_Year),
              data = post_samplesM1_tbl_N,
              alpha = 0.01, color = "gray50"
  ) +
  geom_abline(
    slope = mean(post_samplesM1_tbl_N$b_Year),
    intercept = mean(post_samplesM1_tbl_N$b_Intercept),
    color = "blue", size = 1
  ) +
  theme_classic() + ylab("Annual Displacement") + xlab("Year") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        #strip.text = element_text(colour="black", face = "bold", size = 14),
        #strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1))#+


  
  
# # Save model assumption plots
# png("../plots/Model checks/SE time1.png", width = 21, height = 14.8, units = "cm", res = 600)
# par(mfrow=c(2,2))
# plot(fit1)
# dev.off()
# 
# png("../plots/Model checks/SE time2.png", width = 21, height = 14.8, units = "cm", res = 600)
# hist(fit1$residuals)
# dev.off()
# 
# png("../plots/Model checks/SE time3.png", width = 21, height = 14.8, units = "cm", res = 600)
# res <- residuals(fit1)
# acf(res, plot = T)
# dev.off()


#summary(fit1) # -40.54 decline per year (p = 0.03)
# 164 * -40.54 = -6648.56
#anova(fit1)
#plot(dat2_2$Year, dat2_2$Annual_displacement)
#abline(fit1)


#res <- residuals(fit1)
#acf(res, plot = T) # no autocorrelation

SE <- dat2_2
NE <- dat_NE2

full_data <- bind_rows(list("b) Downwelling Favourable Winds" = SE,"a) Upwelling Favourable Winds" = NE), .id ="Direction")
full_posteriors <- bind_rows(list("b) Downwelling Favourable Winds" = post_samplesM1_tbl,
                                  "a) Upwelling Favourable Winds" = post_samplesM1_tbl_N), .id ="Direction")
mean_trends_by_group <- full_posteriors %>% group_by(Direction) %>% summarise(Slope_Year = mean(b_Year), Intercept = mean(b_Intercept))
mean_trends_by_group

ggplot(full_data, aes(x = Year, y = Annual_displacement)) +
  geom_point(
    colour = "black",
    size = 2,
    alpha = 1
  ) +
  facet_wrap(~Direction, ncol = 2)+
  geom_abline(aes(intercept = b_Intercept, slope = b_Year),
              data = full_posteriors,
              alpha = 0.01, color = "gray50"
  ) +
  geom_abline(data = mean_trends_by_group, aes(
    slope = Slope_Year,
    intercept = Intercept),
    color = "blue", size = 1
  ) +
  scale_y_continuous(limits = c(-20000, 20000), breaks = seq(-20000, 20000, by = 10000)) +
  theme_classic() + ylab("Annual Displacement") + xlab("Year") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1))#+





# p3 <- ggplot(full_data, aes(x = Year, y = Annual_displacement)) + geom_point() + geom_smooth(method = "lm") +
#   theme_classic() + ylab("Annual Displacement") + xlab("Year") + 
#   scale_y_continuous(limits = c(-20000, 20000), breaks = seq(-20000, 20000, by = 10000)) +
#   facet_wrap(~Direction, ncol = 2) +
#   theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
#         axis.text.x  = element_text(colour="black", size = 14), 
#         axis.title.y = element_text(face="bold", colour="black", size = 18),
#         axis.text.y  = element_text(colour="black", size = 14),
#         axis.ticks = element_line(colour="black"),
#         strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
#         strip.background = element_rect(colour = "white"),
#         #legend.justification=c(1,0), legend.position="right",
#         panel.border = element_rect(colour = "black", fill=NA, size = 1)
#         #legend.key.size = unit(1, "cm"),
#         #legend.title = element_text(face = "bold", size = 14),
#         #legend.text = element_text(size = 12, face = "bold"))
#   )
# p3

ggsave("../plots/Historical Wind Change Bayesian.pdf", height = 14.8, width = 21, units = "cm")
ggsave("../plots/Historical Wind Change Bayesian.png", height = 14.8, width = 21, units = "cm", dpi = 600)
