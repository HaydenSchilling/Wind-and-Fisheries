library(tidyverse)
library(REdaS)

mydata <- read_csv("../BOM Data/BARRA Model/Estuaries_speed_direction.csv")

# Classify onshore/offshore
mydata$Onshore_Offshore <- "Offshore"

# Adjust onshore offshore for coastline angle

mydata$Onshore_Offshore[mydata$Direction < 225 & mydata$Direction > 45] <- "Onshore"

# Check it worked
table(mydata$Onshore_Offshore, mydata$estuary)

# make offshore negative wind speed
mydata$output <- ifelse((mydata$Onshore_Offshore) == "Offshore", -1, 1) # make output vector
mydata$Speed_km_hr <- mydata$Speed_km_hr*mydata$output

#### Identify coastline angle, Set to 24 for Hawkesbury, Camden, Wallis, Illawarra, St george, Sydney, and others different
mydata$Coastline_angle <- 135

# radians from adjusted coastline (24 deg coastline - need to fix for all estuaries)
mydata$Wind.direction.in.radians.adjusted <- NULL
mydata$Wind.direction.in.radians.adjusted <- deg2rad(mydata$Direction+mydata$Coastline_angle)

# Calculate effective wind speed
mydata$Wind.effect.size = sin(mydata$Wind.direction.in.radians.adjusted)*-1
#plot(mydata$Direction, mydata$Wind.effect.size) # check

mydata$Wind.effect.size = abs(mydata$Wind.effect.size)
mydata$Wind.speed.adjusted = mydata$Wind.effect.size * mydata$Speed_km_hr * -1

# Check (is slow)
plot(mydata$Direction, mydata$Wind.speed.adjusted)

# Group by month
datM <- mydata %>% group_by(estuary, Year, Month) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)*3), count = n())
head(datM)

hist(datM$displacement)

fwrite(datM, file = "../BOM Data/BARRA Model/BARRA Monthly Modelled Estuary 135 deg Wind Data Final.csv")

mydata$estuary <- as.factor(mydata$estuary)
estuaries <- levels(mydata$estuary)

for (i in as.character(estuaries)) {
  dat2 <- subset(datM, estuary == i)
  fwrite(dat2, file = paste("../BOM Data/BARRA Model/135 winds/BARRA_", i, "_Monthly Modelled Wind Data Final 135 degree.csv", sep = ""))
  
}
