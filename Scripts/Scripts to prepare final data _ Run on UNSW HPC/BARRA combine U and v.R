# Combine U and V
library(data.table)
library(tidyverse)
library(lubridate)

U_dat <- read_csv("../BOM Data/BARRA Model/Estuaries_u.csv ")
V_dat <- read_csv("../BOM Data/BARRA Model/Estuaries_v.csv ")

V_dat$V_wind <- V_dat$U_wind
V_dat$U_wind <- NULL

Full_dat <- left_join(U_dat,V_dat, by =c("time", "estuary"))
#fwrite(Full_dat, "../BOM Data/BARRA Model/Estuaries_u_v.csv")

hist(Full_dat$U_wind)
hist(Full_dat$V_wind)

# Now fix time
# Posix with origin: hours since 1970-01-01 00:00:00

Full_dat$Time <- as.POSIXct(Full_dat$time*3600,origin='1970-01-01 00:00', tz ='GMT')

head(Full_dat)

Full_dat$Year <- year(Full_dat$Time)
Full_dat$Month <- month(Full_dat$Time)
Full_dat$Day <- day(Full_dat$Time)
Full_dat$Hour <- hour(Full_dat$Time)

head(Full_dat)
range(Full_dat$Time)
#fwrite(Full_dat, "../BOM Data/BARRA Model/Estuaries_u_v.csv")

# Calculate speed and direction from vectors
library(rWind)
sp_dir <- uv2ds(Full_dat$U_wind, Full_dat$V_wind)
head(sp_dir)

# Assign to dataframe
dir <- sp_dir[,1]
sp <- sp_dir[,2]
Full_dat$Direction <- dir
Full_dat$Speed_m_s <- sp
# Convert to km/hr
Full_dat$Speed_km_hr <- Full_dat$Speed_m_s * 3.6
Full_dat$Speed_m_s <- NULL

head(Full_dat)
table(Full_dat$Year)

fwrite(Full_dat, "../BOM Data/BARRA Model/Estuaries_speed_direction.csv", row.names = FALSE)



