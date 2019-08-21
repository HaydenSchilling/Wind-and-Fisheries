# Calculate adjusted wind for 45 deg to north
# Hayden Schilling 25/7/19

library(rWind)
library(lubridate)
library(REdaS)
library(data.table)
library(dplyr)



mydata <- read.csv("Wind Data/All Estuaries Modelled Wind Data speed direction.csv", header = T)


#####################################################################################################################################
##################### ONSHORE AND OFFSHORE ##########################################################################################
#####################################################################################################################################

# Classify onshore/offshore
mydata$Onshore_Offshore <- "Offshore"

### Coastline angles
# Clarence 11 deg
# Hunter 55 deg
# Hawkesbury 24 deg 
# Camden Haven 24 deg
# Wallis 24 deg
# Port Stephens 45 deg
# Tuggerah 24 deg
# Lake Illawarra 24 deg
# St Georges Basin 24 deg

# Adjust onshore offshore for coastline angle

mydata$Onshore_Offshore[mydata$Direction < 135 | mydata$Direction > 315] <- "Onshore"

# mydata$Onshore_Offshore[mydata$Estuary =="Hawkesbury_River" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Camden_Haven_River" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Wallis_Lake" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Tuggerah_Lakes" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Lake Illawarra" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="St_Georges_Basin" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Clarence_River" & mydata$Direction > 11 & mydata$Direction < 191] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Hunter_River" & mydata$Direction > 55 & mydata$Direction < 235] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Port_Stephens" & mydata$Direction > 45 & mydata$Direction < 225] <- "Onshore"
# mydata$Onshore_Offshore[mydata$Estuary =="Sydney" & mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"


# Check it worked
table(mydata$Onshore_Offshore, mydata$Estuary)
#sin(deg2rad(90))


#table(mydata$Direction)

# make offshore negative wind speed
# only the super slow loop appears to work
# mydata$Speed_km_hr[mydata$Onshore_Offshore == "Offshore"] <- mydata$Speed_km_hr * (-1)


for (i in 1:nrow(mydata)) {
  if (mydata$Onshore_Offshore[i] == "Offshore") {
    mydata$Speed_km_hr[i] = mydata$Speed_km_hr[i] * -1
  }
}




saveRDS(mydata, file = "22_6_19_progress.rds")

mydata <- NULL

mydata <- readRDS("22_6_19_progress.rds")

str(mydata)

#### Identify coastline angle, Set to 24 for Hawkesbury, Camden, Wallis, Illawarra, St george, Sydney, and others different
mydata$Coastline_angle <- 45


# mydata$Coastline_angle[mydata$Estuary =="Clarence_River" ] <- 11
# mydataCoastline_angle[mydata$Estuary =="Hunter_River" ] <- 55
# mydata$Coastline_angle[mydata$Estuary =="Port_Stephens" ] <- 45


# radians from adjusted coastline (24 deg coastline - need to fix for all estuaries)
mydata$Wind.direction.in.radians.adjusted <- NULL
mydata$Wind.direction.in.radians.adjusted <- deg2rad(mydata$Direction+mydata$Coastline_angle)

# Calculate effective wind speed
mydata$Wind.effect.size = sin(mydata$Wind.direction.in.radians.adjusted)
mydata$Wind.speed.adjusted = mydata$Wind.effect.size * mydata$Speed_km_hr

# Group by Day
dat <- mydata %>% group_by(Estuary, Year, Month, Day) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)*3), count = n())
head(dat)

# # Group by Month
# dat <- mydata %>% group_by(Estuary, Year, Month, Day) %>%
#   summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)*3), count = n())
# head(dat)

library(ggplot2)
p1 <- ggplot(mydata, aes(x= Time, y = Wind.speed.adjusted)) + #geom_point() +
  facet_wrap(~Estuary) + geom_smooth()
p1


# fwrite(dat, file = "Wind Data/Monthly Modelled Wind Data Final.csv")

estuaries <- levels(mydata$Estuary)

for (i in as.character(estuaries)) {
  dat2 <- subset(dat, Estuary == i)
  fwrite(dat2, file = paste("Wind Data/45 degree/", i, "_Daily Modelled Wind Data Final 45 degree.csv", sep = ""))
  
}

