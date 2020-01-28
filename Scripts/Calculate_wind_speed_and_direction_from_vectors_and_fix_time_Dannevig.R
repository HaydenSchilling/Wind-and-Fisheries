# Calculate wind speed and direction from full sydney wind data
# Hayden Schilling 25/10/19

#install.packages("rWind")
library(rWind)
library(lubridate)
#install.packages("REdas")
library(REdaS)
library(data.table)
library(dplyr)

mydata <- read.csv("../Wind DataV3/Wind_data_Dannevig_vectors.csv", header = T)
head(mydata)
# calculate speed and direction from vectors
sp_dir <- uv2ds(mydata$U_Wind, mydata$V_Wind)
head(sp_dir)

# Assign to dataframe
dir <- sp_dir[,1]
sp <- sp_dir[,2]
mydata$Direction <- dir
mydata$Speed_m_s <- sp
# Convert to km/hr
mydata$Speed_km_hr <- mydata$Speed_m_s * 3.6
mydata$Speed_m_s <- NULL

# Fix time
mydata$Time <- ymd_hms(mydata$Time, tz="GMT")
mydata$Year <- year(mydata$Time)
mydata$Month <- month(mydata$Time)
mydata$Day <- day(mydata$Time)
mydata$Hour <- hour(mydata$Time)

head(mydata)
table(mydata$Year)

fwrite(mydata, "../Wind DataV3/Dannevig Modelled Wind Data speed direction.csv", row.names = FALSE)

mydata <- read.csv("Wind DataV3/Dannevig Modelled Wind Data speed direction.csv", header = T)


d_dat <- filter(mydata, Year == 1914 & Month == 12 & Day <=10)

plot(d_dat$Speed_km_hr, col = d_dat$Day)

fwrite(d_dat, "../Wind DataV3/Dannevig Modelled Wind Data speed direction 1_10 Dec 1914.csv", row.names = FALSE)

#####################################################################################################################################
##################### ONSHORE AND OFFSHORE ##########################################################################################
# #####################################################################################################################################
# 
# # Classify onshore/offshore
# mydata$Onshore_Offshore <- "Offshore"
# 
# ### Coastline angles
# # Clarence 11 deg
# # Hunter 55 deg
# # Hawkesbury 24 deg 
# # Camden Haven 24 deg
# # Wallis 24 deg
# # Port Stephens 45 deg
# # Tuggerah 24 deg
# # Lake Illawarra 24 deg
# # St Georges Basin 24 deg
# 
# # Adjust onshore offshore for coastline angle
# 
# #mydata$Onshore_Offshore[mydata$Direction > 24 & mydata$Direction < 204] <- "Onshore"
# 
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
# 
# 
# # Check it worked
# table(mydata$Onshore_Offshore, mydata$Estuary)
# #sin(deg2rad(90))
# 
# 
# #table(mydata$Direction)
# 
# # make offshore negative wind speed
# # only the super slow loop appears to work
# # mydata$Speed_km_hr[mydata$Onshore_Offshore == "Offshore"] <- mydata$Speed_km_hr * (-1)
# 
# 
# for (i in 1:nrow(mydata)) {
#   if (mydata$Onshore_Offshore[i] == "Offshore") {
#     mydata$Speed_km_hr[i] = mydata$Speed_km_hr[i] * -1
#   }
# }
# 
# saveRDS(mydata, file = "22_6_19_progress.rds")
# 
# mydata <- NULL
# 
# mydata <- readRDS("22_6_19_progress.rds")
# 
# 
# 
# #### Identify coastline angle, Set to 24 for Hawkesbury, Camden, Wallis, Illawarra, St george, Sydney, and others different
# mydata$Coastline_angle <- 24
# 
# 
# mydata$Coastline_angle[mydata$Estuary =="Clarence_River" ] <- 11
# mydataCoastline_angle[mydata$Estuary =="Hunter_River" ] <- 55
# mydata$Coastline_angle[mydata$Estuary =="Port_Stephens" ] <- 45
# 
# 
# # radians from adjusted coastline (24 deg coastline - need to fix for all estuaries)
# mydata$Wind.direction.in.radians.adjusted <- NULL
# mydata$Wind.direction.in.radians.adjusted <- deg2rad(mydata$Direction+mydata$Coastline_angle)
# 
# # Calculate effective wind speed
# mydata$Wind.effect.size = sin(mydata$Wind.direction.in.radians.adjusted)
# mydata$Wind.speed.adjusted = mydata$Wind.effect.size * mydata$Speed_km_hr
# 
# # Group
# dat <- mydata %>% group_by(Estuary, Year, Month) %>%
#   summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)*3), count = n())
# head(dat)
# 
# library(ggplot2)
# p1 <- ggplot(mydata, aes(x= Time, y = Wind.speed.adjusted)) + #geom_point() +
#   facet_wrap(~Estuary) + geom_smooth()
# p1
# 
# 
# fwrite(dat, file = "Wind Data/Monthly Modelled Wind Data Final.csv")
# 
# estuaries <- levels(mydata$Estuary)
# 
# for (i in as.character(estuaries)) {
#   dat2 <- subset(dat, Estuary == i)
#   fwrite(dat2, file = paste("Wind Data/", i, "_Monthly Modelled Wind Data Final.csv", sep = ""))
#   
# }
# 
