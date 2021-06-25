library(tidyverse)
library(REdaS)

mydata <- read_csv("All_Locations_wind_speed_direction.csv")


# Classify onshore/offshore
mydata$Onshore_Offshore <- "Offshore"

# Adjust onshore offshore for coastline angle

mydata$Onshore_Offshore[mydata$Direction < 225 & mydata$Direction > 45] <- "Onshore"

# Check it worked
table(mydata$Onshore_Offshore, mydata$Year)

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
#plot(mydata$Direction, mydata$Wind.speed.adjusted)

# Group by day
datM <- mydata %>% group_by(Latitude, Longitude, Year, Month, Day) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)), count = n())


hist(datM$displacement)
datM$DownWind <- datM$displacement
datM$displacement <- NULL
head(datM)
write_csv(datM, path = "Pat Monthly Downwelling Wind Data Final.csv")
