# Script to get larval fish winds from BARRA MODEL

# NIMO Larval Fish and wind

# This script extracts u and v for the 14 days prior to each larval fish sample at the location of each sample

# Step 1 generate date string 14 days prior for each date in list - see complete function
# https://blog.exploratory.io/populating-missing-dates-with-complete-and-fill-functions-in-r-and-exploratory-79f2a321e6b5 )

library(tidyverse)
library(lubridate)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tools)
library(data.table)
library(dplyr)
library(yaImpute)

# Load Fish Data
fish_data <- read.csv("../allNIMO_dist.csv", header = T)
str(fish_data)

#unique(fish_data$Latitude)
#range(fish_data$Date)

# Restrict to NSW and on the continental shelf
fish_data <- filter(fish_data, Latitude <= -30 & Latitude >= -36 & Longitude > 140)
fish_data <- filter(fish_data, Bathy >= -200)
summary(fish_data$Bathym_m)
hist(fish_data$Bathym_m)

# Recognise Dates
fish_data$Date <- as.Date(as.character(fish_data$Date), format = "%d/%m/%Y")
range(fish_data$Date)
plot(fish_data$Date)
fish_data <- filter(fish_data, Date > "1990-01-01") # to match wind model BARRA # loses ~100 points from 1980s

# get unique combinations of lat/long/date

Date_locations <- unique(fish_data[,c('Latitude','Longitude','Date')])

# Code to expand dat/lat/long combinations 14 days prior
# One way to do this using using tidyr::complete is to create a sequence of dates from (date - 6) to date for each row.
Date_locations <- Date_locations %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(row = row_number()) %>%
  complete(Date = seq(Date - 14, Date, "day")) %>%
  ungroup %>%
  arrange(row, desc(Date)) %>%
  fill(Latitude, Longitude)
head(Date_locations)

### Now to extract from BARRA MODEL


#file_list <- list.files("../../srv/scratch/z3374139/BARRA Model/av_vwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".nc")
file_list <- list.files("../BOM Data/BARRA Model/av_uwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".nc")
head(file_list)

#file_list <- list.files("../BOM Data/BARRA Model/av_uwnd10m/1990/01/", full.names = TRUE, recursive = TRUE, pattern =".nc")

#save_names <- file_path_sans_ext(file_list)


### Call a file based upon date in filename

head(Date_locations)

Date_locations$Year <- year(Date_locations$Date)
Date_locations$Month <- month(Date_locations$Date)
Date_locations$Day <- as.numeric(day(Date_locations$Date))

# pad to 2 digit days
Date_locations$Day <- sprintf("%02d", Date_locations$Day)
Date_locations$Month <- sprintf("%02d", Date_locations$Month)

#head(Date_locations)
#
#y = Date_locations$Year[1]
#m = Date_locations$Month[1]
#d = Date_locations$Day[1]
#
## Call files based upon dates
#
#paste0("../BOM Data/BARRA Model/av_uwnd10m//1990/01/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-19900101T0000Z.sub.nc")
#
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T0000Z.sub.nc")
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T0600Z.sub.nc")
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T1200Z.sub.nc")
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T1800Z.sub.nc")


file_list_test <- ""

##### Need to dp all 4 files for each day ,not just 1

#for (i in 1:nrow(Date_locations)){
#  y = Date_locations$Year[i]
#  m = Date_locations$Month[i]
#  d = Date_locations$Day[i]
#  file_list_test[i] <- paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T0000Z.sub.nc")
#  }

#head(file_list_test)


#nc <- nc_open("../BOM Data/BARRA Model/av_uwnd10m/1993/03/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-19930315T1200Z.sub.nc")
dat <- Date_locations
head(dat)
res_spat <- 10
vr <- "av_uwnd10m"
#mat <- data.frame(U_wind = numeric(length=10), time = numeric(length=10), estuary = character(length=10)) # ncol=length(pr)
#mat$estuary <- as.character(mat$estuary)
pr <- "u_Wind"
#full_dat <- data.frame(U_wind = as.numeric(), time = as.numeric(), estuary = as.character()) # ncol=length(pr)

### Looping here

#out_time <- ncvar_get(nc, "time") # vr is the variable you want
#out_time_real <- as.POSIXct(out_time*60*60, origin = "1970-01-01 00:00", tz ="GMT")
#out_time_real

full_dat <- data.frame(Date = character(), time = numeric(), U_wind = numeric(),
                       Latitude = numeric(), Longitude = numeric())
full_dat$Date <- as.character(full_dat$Date)

#list of times to loop through
file_times <- c("0000", "0600", "1200", "1800")

for (f in 1:nrow(dat)){
  y = Date_locations$Year[f]
  m = Date_locations$Month[f]
  d = Date_locations$Day[f]
  date = Date_locations$Date[f]
  
  t_temp = list()
  time_temp = list()
  for (tx in file_times){
    file <- paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T", tx,"Z.sub.nc")
    
    nc <- nc_open(file)
    out_time <- ncvar_get(nc, "time") # "hours since 1970-01-01 00:00:00"
    
    for (t in 1:length(out_time)){
      
            # Approximate nearest neighbour
        idx_lon <- ann(as.matrix(nc$dim$lon$vals), as.matrix(dat$Longitude[f]), k = 1, verbose = FALSE)$knnIndexDist[,1] #$Longitude[1] was [i]
        idx_lat <- ann(as.matrix(nc$dim$lat$vals), as.matrix(dat$Latitude[f]), k = 1, verbose = FALSE)$knnIndexDist[,1] # as above
        cnt <- c(1,1,1)
        # If more than 1x1 pixel is requested we adjust the idx by res_spat/2 and count by res_spa
        idx_lon <- idx_lon - floor(res_spat/2)
        idx_lon <- idx_lon - floor(res_spat/2)
        cnt <- c(res_spat, res_spat, 1)
        
        out <- ncvar_get(nc, vr, start=c(idx_lon, idx_lat, t), count = cnt) # vr is the variable you want
        
        #print(out)
        ## Works to here so far
        #temp_dat$U_wind[i] <- mean(out, na.rm = TRUE) #[i,j]
        #temp_dat$time[i] <- out_time[t]
        
       t_temp[t] =  mean(out, na.rm = TRUE)
       time_temp[t] = out_time[t]
  
  
     
    }
    nc_close(nc)
    temp_dat = data.frame(Latitude = as.numeric(dat$Latitude[f]), Longitude = as.numeric(dat$Longitude[f]), time = as.numeric(time_temp), 
                          Date = as.character(dat$Date[f]), U_Wind = as.numeric(t_temp))
    full_dat <- rbind(full_dat, temp_dat)
  }
    
  
}
fwrite(full_dat, file = "BARRA_larval_u_winds.csv")


### Now v winds
library(tidyverse)
library(lubridate)
library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tools)
library(data.table)
library(dplyr)
library(yaImpute)

# Load Fish Data
fish_data <- read.csv("../allNIMO_dist.csv", header = T)
str(fish_data)

#unique(fish_data$Latitude)
#range(fish_data$Date)

# Restrict to NSW and on the continental shelf
fish_data <- filter(fish_data, Latitude <= -30 & Latitude >= -36 & Longitude > 140)
fish_data <- filter(fish_data, Bathy >= -200)
summary(fish_data$Bathym_m)
hist(fish_data$Bathym_m)

# Recognise Dates
fish_data$Date <- as.Date(as.character(fish_data$Date), format = "%d/%m/%Y")
range(fish_data$Date)
plot(fish_data$Date)
fish_data <- filter(fish_data, Date > "1990-01-01") # to match wind model BARRA # loses ~100 points from 1980s

# get unique combinations of lat/long/date

Date_locations <- unique(fish_data[,c('Latitude','Longitude','Date')])

# Code to expand dat/lat/long combinations 14 days prior
# One way to do this using using tidyr::complete is to create a sequence of dates from (date - 6) to date for each row.
Date_locations <- Date_locations %>%
  mutate(Date = as.Date(Date)) %>%
  group_by(row = row_number()) %>%
  complete(Date = seq(Date - 14, Date, "day")) %>%
  ungroup %>%
  arrange(row, desc(Date)) %>%
  fill(Latitude, Longitude)
head(Date_locations)


### Now to extract from BARRA MODEL


#file_list <- list.files("../../srv/scratch/z3374139/BARRA Model/av_vwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".nc")
file_list <- list.files("../BOM Data/BARRA Model/av_vwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".nc")
head(file_list)

#file_list <- list.files("../BOM Data/BARRA Model/av_uwnd10m/1990/01/", full.names = TRUE, recursive = TRUE, pattern =".nc")

#save_names <- file_path_sans_ext(file_list)


### Call a file based upon date in filename

head(Date_locations)

Date_locations$Year <- year(Date_locations$Date)
Date_locations$Month <- month(Date_locations$Date)
Date_locations$Day <- as.numeric(day(Date_locations$Date))

# pad to 2 digit days
Date_locations$Day <- sprintf("%02d", Date_locations$Day)
Date_locations$Month <- sprintf("%02d", Date_locations$Month)

#head(Date_locations)
#
#y = Date_locations$Year[1]
#m = Date_locations$Month[1]
#d = Date_locations$Day[1]
#
## Call files based upon dates
#
#paste0("../BOM Data/BARRA Model/av_uwnd10m//1990/01/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-19900101T0000Z.sub.nc")
#
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T0000Z.sub.nc")
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T0600Z.sub.nc")
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T1200Z.sub.nc")
#paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T1800Z.sub.nc")


file_list_test <- ""

#for (i in 1:nrow(Date_locations)){ # loops through all date/location pairs as a test
#  y = Date_locations$Year[i]
#  m = Date_locations$Month[i]
#  d = Date_locations$Day[i]
#  file_list_test[i] <- paste0("../BOM Data/BARRA Model/av_uwnd10m//", y,"/", m, "/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T0000Z.sub.nc")
#}

#head(file_list_test)


#nc <- nc_open("../BOM Data/BARRA Model/av_uwnd10m/1993/03/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-19930315T1200Z.sub.nc")
dat <- Date_locations
head(dat)
res_spat <- 10
vr <- "av_vwnd10m"
#mat <- data.frame(U_wind = numeric(length=10), time = numeric(length=10), estuary = character(length=10)) # ncol=length(pr)
#mat$estuary <- as.character(mat$estuary)
pr <- "v_Wind"
#full_dat <- data.frame(U_wind = as.numeric(), time = as.numeric(), estuary = as.character()) # ncol=length(pr)

### Looping here

#out_time <- ncvar_get(nc, "time") # vr is the variable you want
#out_time_real <- as.POSIXct(out_time*60*60, origin = "1970-01-01 00:00", tz ="GMT")
#out_time_real

full_dat <- data.frame(Date = character(), time = numeric(), V_wind = numeric(),
                       Latitude = numeric(), Longitude = numeric())
full_dat$Date <- as.character(full_dat$Date)

#list of times to loop through
file_times <- c("0000", "0600", "1200", "1800")

for (f in 1:nrow(dat)){
  y = Date_locations$Year[f]
  m = Date_locations$Month[f]
  d = Date_locations$Day[f]
  date = Date_locations$Date[f]
  
  t_temp = list()
  time_temp = list()
  for (tx in file_times){
    file <- paste0("../BOM Data/BARRA Model/av_vwnd10m//", y,"/", m, "/av_vwnd10m-fc-slv-PT1H-BARRA_SY-v1-", y, m, d, "T", tx,"Z.sub.nc")
    
    nc <- nc_open(file)
    out_time <- ncvar_get(nc, "time") # "hours since 1970-01-01 00:00:00"
    
    for (t in 1:length(out_time)){
      
      # Approximate nearest neighbour
      idx_lon <- ann(as.matrix(nc$dim$lon$vals), as.matrix(dat$Longitude[f]), k = 1, verbose = FALSE)$knnIndexDist[,1] #$Longitude[1] was [i]
      idx_lat <- ann(as.matrix(nc$dim$lat$vals), as.matrix(dat$Latitude[f]), k = 1, verbose = FALSE)$knnIndexDist[,1] # as above
      cnt <- c(1,1,1)
      # If more than 1x1 pixel is requested we adjust the idx by res_spat/2 and count by res_spa
      idx_lon <- idx_lon - floor(res_spat/2)
      idx_lon <- idx_lon - floor(res_spat/2)
      cnt <- c(res_spat, res_spat, 1)
      
      out <- ncvar_get(nc, vr, start=c(idx_lon, idx_lat, t), count = cnt) # vr is the variable you want
      
      #print(out)
      ## Works to here so far
      #temp_dat$U_wind[i] <- mean(out, na.rm = TRUE) #[i,j]
      #temp_dat$time[i] <- out_time[t]
      
      t_temp[t] =  mean(out, na.rm = TRUE)
      time_temp[t] = out_time[t]
      
      
      
    }
    nc_close(nc)
    temp_dat = data.frame(Latitude = as.numeric(dat$Latitude[f]), Longitude = as.numeric(dat$Longitude[f]), time = as.numeric(time_temp), 
                          Date = as.character(dat$Date[f]), V_Wind = as.numeric(t_temp))
    full_dat <- rbind(full_dat, temp_dat)
  }
  
  
}
fwrite(full_dat, file = "BARRA_larval_v_winds.csv")

U_dat <- read_csv("BARRA_larval_u_winds.csv")
U_dat <- U_dat %>% distinct()

V_dat <- read_csv("BARRA_larval_v_winds.csv")
V_dat <- V_dat %>% distinct()


full_dat <- left_join(U_dat, V_dat, by = c("Latitude", "Longitude", "Date", "time"))
fwrite(full_dat, "../BOM Data/BARRA Model/Full_Larval_Vectors.csv")
