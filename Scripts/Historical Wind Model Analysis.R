# Analysis of modelled wind data

#install.packages(c("effects", "lubridate", "ggplot2", "splines", "dplyr"))

library(lubridate)
library(ggplot2)
library(effects)
library(splines)
library(dplyr)


mydata <- read.csv("Wind Data/135 degree/Sydney_Daily Modelled Wind Data Final 135 degree.csv", header = T)
#soi_data <- read.csv("SOI data long.csv", header = T)

head(mydata)

#mydata <- left_join(mydata, soi_data, by = c("Year", "Month"))

mydata$Date <- paste0(mydata$Year,"-",mydata$Month,"-",mydata$Day)
mydata$Date <- as.Date(mydata$Date)

str(mydata)

dat <- mydata %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))
head(dat)

plot(dat$Year, dat$Annual_displacement)

dat2 <- subset(dat, Year >= 1900)

fit1 <- lm(Annual_displacement ~ Year, data = dat2)
plot(fit1)

hist(fit1$residuals)

# try cooks distance
#install.packages("olsrr")
library(olsrr)

ols_plot_cooksd_bar(fit1)
ols_plot_cooksd_chart(fit1)
ols_plot_dfbetas(fit1)
ols_plot_dffits(fit1)
ols_plot_resid_stud(fit1) # conclusions # 24 is a  outlier
ols_plot_resid_stand(fit1)
ols_plot_resid_lev(fit1) # but 24 has low leverage
ols_plot_resid_stud_fit(fit1)
ols_plot_hadi(fit1)
ols_plot_resid_pot(fit1)


summary(fit1)
anova(fit1)
plot(dat2$Year, dat2$Annual_displacement)
abline(fit1)


res <- residuals(fit1)
acf(res, plot = F)
head(res, type = "pearson")

library(ggfortify)
acf_p <- autoplot(acf(res)) + #simulationOutput$fittedResiduals
  geom_hline(yintercept = 0) +
  ylab('Autocorrelation function')
acf_p

# try to remove the outlier 
dat2_no_outlier <- subset(dat2, Year != 1923)

fit1_no_outlier <- lm(Annual_displacement ~ Year, data = dat2_no_outlier)
plot(fit1)

hist(fit1$residuals)
ols_plot_resid_stud(fit1_no_outlier) # no real outliers now
summary(fit1_no_outlier)# same significant effect but effect is greater

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

ggsave("plots/Historical SE Wind Change.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Historical SE Wind Change.png", height = 14.8, width = 21, units = "cm", dpi = 600)


### Now do NE Winds
NEdata <- read.csv("Wind Data/45 degree/Sydney_Daily Modelled Wind Data Final 45 degree.csv", header = T)


NEdata$Date <- paste0(NEdata$Year,"-",NEdata$Month,"-",NEdata$Day)
NEdata$Date <- as.Date(NEdata$Date)

str(NEdata)

dat_NE <- NEdata %>% group_by(Year) %>% summarise(Annual_displacement = sum(displacement))
head(dat_NE)

plot(dat_NE$Year, dat_NE$Annual_displacement)

dat2_NE <- subset(dat_NE, Year >= 1900)

fit2 <- lm(Annual_displacement ~ Year, data = dat2_NE)
plot(fit2)

hist(fit2$residuals)
ols_plot_cooksd_bar(fit2)
ols_plot_cooksd_chart(fit2)
ols_plot_dfbetas(fit2)
ols_plot_dffits(fit2)
ols_plot_resid_stud(fit2) # conclusions # 24 is a  outlier
ols_plot_resid_stand(fit2)
ols_plot_resid_lev(fit2) # but 24 has low leverage
ols_plot_resid_stud_fit(fit2)
ols_plot_hadi(fit2)
ols_plot_resid_pot(fit2)

summary(fit2)
anova(fit2)
plot(dat2_NE$Year, dat2_NE$Annual_displacement)
abline(fit2)


res <- residuals(fit2)
acf(res, plot = F)
head(res, type = "pearson")

library(ggfortify)
acf_p <- autoplot(acf(res)) + #simulationOutput$fittedResiduals
  geom_hline(yintercept = 0) +
  ylab('Autocorrelation function')
acf_p


p2 <- ggplot(dat2_NE, aes(x = Year, y = Annual_displacement)) + geom_point() + geom_smooth(method = "lm") +
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

#install.packages("ggpubr")
library(ggpubr)

ggarrange(p1, p2, labels = c("a) SE Winds", "b) NE Winds"), ncol=1)


## Try facet designed plot
SE <- dat2
NE <- dat2_NE

full_data <- bind_rows(list("b) SE Winds" = SE,"a) NE Winds" = NE), .id ="Direction")

p3 <- ggplot(full_data, aes(x = Year, y = Annual_displacement)) + geom_point() + geom_smooth(method = "lm") +
  theme_classic() + ylab("Annual Displacement") + xlab("Year") + 
  facet_wrap(~Direction, scales = "free_y", ncol = 2) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1)
        #legend.key.size = unit(1, "cm"),
        #legend.title = element_text(face = "bold", size = 14),
        #legend.text = element_text(size = 12, face = "bold"))
  )
p3

ggsave("plots/Historical Wind Change2.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/Historical Wind Change2.png", height = 14.8, width = 21, units = "cm", dpi = 600)

#mydata$Time <- ymd_hms(mydata$Time, tz="GMT")

#p <- ggplot(mydata, aes(Time, Wind.speed.adjusted)) + geom_point() + geom_smooth()
#p
# 
# # Harmonic is to fit Hour and DOY
# Harm <- function (theta, k = 4) {
#   X <- matrix(0, length(theta), 2 * k)
#   nam <- as.vector(outer(c("c", "s"), 1:k, paste, sep = ""))
#   dimnames(X) <- list(names(theta), nam)
#   m <- 0
#   for (j in 1:k) {
#     X[, (m <- m + 1)] <- cos(j * theta)
#     X[, (m <- m + 1)] <- sin(j * theta)
#   }
#   X
# }
# 
# # Make cyclic month and hour and day of year
# mydata$Harm_Month <- (mydata$Month/12) * 2 * pi
# #mydata$Harm_Hour <- (mydata$Hour/24) * 2 * pi
# mydata$DayofYear <- yday(mydata$Date)
# mydata$Harm_DayofYear <- (mydata$DayofYear/366) * 2 * pi
# 
# head(mydata)
# str(mydata)
# 
# p1 <- ggplot(mydata, aes(Date, displacement)) + geom_point(alpha = 0.2) + geom_smooth() +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p1
# 
# 
# # 
# # Linear Modelling
# # Tested log10 response doesn't work - very poor model assumptions
# 
# fit1 <- lm(displacement ~ Date, data = mydata)
# plot(fit1)
# summary(fit1)
# plot(allEffects(fit1))
# 
# 
# # Need to scale winds again
# 
# 
# 
# # fit2 <- lm(Wind.speed.adjusted ~ Year, data = mydata)
# # summary(fit2)
# # plot(allEffects(fit2))
# # 
# # 
# # fit3 <- lm(Wind.speed.adjusted ~ Year + Harm(Harm_Month, k = 1), data = mydata)
# # summary(fit3)
# # plot(allEffects(fit3))
# # 
# # fit4 <- lm(Wind.speed.adjusted ~ Year + Harm(Harm_Month, k = 1) + Harm(Harm_Hour, k = 1), data = mydata)
# # summary(fit4)
# # plot(allEffects(fit4))
# # plot(fit4)
# # 
# # fit5 <- lm(Wind.speed.adjusted ~ Year + Harm(Harm_DayofYear, k = 1) + Harm(Harm_Hour, k = 1) + SOI, data = mydata)
# # summary(fit5)
# # plot(allEffects(fit5))
# # plot(fit5)
# # 
# # # Raw speed - No sign adjustment - Year pattern flips
# # fit6 <- lm(Speed_km_hr ~ Year + Harm(Harm_DayofYear, k = 1) + Harm(Harm_Hour, k = 1), data = mydata)
# # summary(fit6)
# # plot(allEffects(fit6))
# # plot(fit5)
# 
# 
# ### Trying monthly bins like other analysis
# 
# dat <- mydata %>% group_by(Year, Month) %>%
#   summarise(displacement = ((sum(Wind.speed.adjusted, na.rm = TRUE)*3)), SOI_mean = mean(SOI), count = n())
# 
# dat$Harm_Month <- (dat$Month/12) * 2 * pi
# head(dat)
# 
# write.csv(dat, "Wind Data/Sydney Modelled Wind Data Final monthly binned.csv", row.names = FALSE)
# 
# ### Now try analysis again
# 
# fit1 <- lm(displacement ~ Year, data = dat)
# plot(fit1)
# summary(fit1)
# plot(allEffects(fit1))
# 
# fit2 <- lm(displacement ~ Year + Harm(Harm_Month, k = 2), data = dat)
# plot(fit2)
# summary(fit2)
# plot(allEffects(fit2))
# 
# fit3 <- lm(displacement ~ Harm(Harm_Month, k = 2), data = dat)
# plot(fit3)
# summary(fit3)
# plot(allEffects(fit3))
# AIC(fit3)
# 
# k <- length(coef(fit2))
# k
# 
# n <- nrow(dat)
# n
# 
# fit2_AIC <- AIC(fit2, k = k)
# fit2_AICc <- (fit2_AIC + ((2 * (k + 1))/(n - k - 1)))
# fit2_AICc
# 
# fit5 <- lm(displacement ~ Year + Harm(Harm_Month, k = 2) + SOI_mean, data = dat)
# plot(fit5)
# summary(fit5)
# plot(allEffects(fit5))
# AIC(fit5)
# 
# k <- length(coef(fit5))
# k
# 
# n <- nrow(dat)
# n
# 
# fit5_AIC <- AIC(fit5, k = k)
# fit5_AICc <- (fit5_AIC + ((2 * (k + 1))/(n - k - 1)))
# fit5_AICc
# 
# p1 <- ggplot(dat, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p1
# 
# p2 <- ggplot(dat, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic() + facet_wrap(~Month)
# p2
# 
# p3 <- ggplot(dat, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth() +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p3
# 
# p3 <- ggplot(dat, aes(Month, displacement)) + geom_point(alpha = 0.2) + geom_smooth() +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p3
# 
# p3 <- ggplot(dat, aes(Month, displacement)) + geom_point(alpha = 0.2) + geom_smooth() +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic() + facet_wrap(~Year)
# p3
# 
# 
# #### Testing on specific months
# # January - July
# 
# dat2 <- subset(dat, Month <= 7)
# 
# fit1_sm <- lm(displacement ~ Year , data = dat2) #+ SOI_mean
# plot(fit1_sm)
# summary(fit1_sm)
# plot(allEffects(fit1_sm))
# 
# p_Jan_July <- ggplot(dat2, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p_Jan_July
# 
# # April - September
# dat3 <- subset(dat, Month <= 9 & Month >= 4)
# 
# fit2_sm <- lm(displacement ~ Year , data = dat3) # + SOI_mean
# plot(fit2_sm)
# summary(fit2_sm)
# plot(allEffects(fit2_sm))
# 
# p_April_Sept <- ggplot(dat3, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p_April_Sept
# 
# # April - July
# dat4 <- subset(dat, Month >= 4 & Month <= 7)
# 
# fit4_sm <- lm(displacement ~ Year , data = dat4)
# plot(fit4_sm)
# summary(fit4_sm)
# plot(allEffects(fit4_sm))
# 
# p_April_July <- ggplot(dat4, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p_April_July
# 
# # July - August
# dat5 <- subset(dat, Month >= 7 & Month <= 8)
# 
# fit5_sm <- lm(displacement ~ Year , data = dat5)
# plot(fit5_sm)
# summary(fit5_sm)
# plot(allEffects(fit5_sm))
# 
# p_July_August <- ggplot(dat5, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p_July_August
# 
# 
# # November - April
# dat6 <- subset(dat, Month >= 11 | Month <= 4)
# 
# fit6_sm <- lm(displacement ~ Year , data = dat6)
# plot(fit6_sm)
# summary(fit6_sm)
# plot(allEffects(fit6_sm))
# 
# p_November_April <- ggplot(dat6, aes(Year, displacement)) + geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
#   ylab("Monthly Onshore Displacement (km)") + theme_classic()
# p_November_April
# 
