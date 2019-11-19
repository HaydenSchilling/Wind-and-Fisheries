# Testing vector correlations

mydata <- read.csv("Wind Data/Wind_data_full_all_estuaries_vectors.csv", header = T)
mydata2 <- read.csv("Wind DataV3/Wind_data_full_all_estuaries_vectors.csv", header = T)

head(mydata)
head(mydata2)

library(dplyr)

full_dat <- inner_join(mydata, mydata2, by = c("Time", "Estuary"))

cor.test(full_dat$U_Wind.x, full_dat$U_Wind.y)

cor.test(full_dat$V_Wind.x, full_dat$V_Wind.y)

cor.test(full_dat$V_Wind.x, full_dat$U_Wind.y)

cor.test(full_dat$U_Wind.x, full_dat$V_Wind.y)

head(full_dat)
