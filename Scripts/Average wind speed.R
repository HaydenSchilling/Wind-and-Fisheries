# Average wind speed for catchability analysis
library(dplyr)
library(data.table)

mydata <- read.csv("Wind Data/All Estuaries Modelled Wind Data speed direction.csv", header = T)

table(mydata$Estuary)
str(mydata)

# Group
dat <- mydata %>% group_by(Estuary, Year, Month) %>%
  summarise(Average_Wind_Speed = (mean(Speed_km_hr, na.rm = TRUE)), count = n())
head(dat)

library(ggplot2)
p1 <- ggplot(mydata, aes(x= Time, y = Speed_km_hr)) + #geom_point() +
  facet_wrap(~Estuary) + geom_smooth()
p1


estuaries <- levels(mydata$Estuary)

for (i in as.character(estuaries)) {
  dat2 <- subset(dat, Estuary == i)
  fwrite(dat2, file = paste("Wind Data/Average Speed/", i, "_Monthly Modelled Average Wind Speed Final.csv", sep = ""))
  
}
