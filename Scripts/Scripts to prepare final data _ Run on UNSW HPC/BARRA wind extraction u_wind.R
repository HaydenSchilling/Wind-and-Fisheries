# To extract data from BARRA MODEL -- U Winds

# Wind Data get into usable format for each estuary location
# Hayden Schilling 21/6/19

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tools)
library(data.table)
library(dplyr)
library(yaImpute)


### FROM JASON'S IMOS CODE

file_list <- list.files("../../srv/scratch/z3374139/BARRA Model/av_uwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".nc")
#file_list <- list.files("../BOM Data/BARRA Model/av_uwnd10m/1990/01/", full.names = TRUE, recursive = TRUE, pattern =".nc")

save_names <- file_path_sans_ext(file_list)

#nc <- nc_open("../BOM Data/BARRA Model/av_uwnd10m/1993/03/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-19930315T1200Z.sub.nc")
dat <- estuary_info <- read.csv("Estuary Lat Longs.csv", header = T)
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

for (f in 1:length(file_list)) {
  nc <- nc_open(file_list[f])
  out_time <- ncvar_get(nc, "time")
  full_dat <- data.frame(U_wind = as.numeric(), time = as.numeric(), estuary = as.character()) # ncol=length(pr)
  mat <- data.frame(U_wind = numeric(length=10), time = numeric(length=10), estuary = character(length=10)) # ncol=length(pr)
  mat$estuary <- as.character(mat$estuary)
  
    for (t in 1:length(out_time)){
      
      for (i in 1:length(dat$Latitude)) { # Loop through all rows in the data for each variable you want
        # Approximate nearest neighbour
        idx_lon <- ann(as.matrix(nc$dim$lon$vals), as.matrix(dat$Longitude[i]+0.15), k = 1, verbose = FALSE)$knnIndexDist[,1] #$Longitude[1] was [i]
        idx_lat <- ann(as.matrix(nc$dim$lat$vals), as.matrix(-dat$Latitude[i]), k = 1, verbose = FALSE)$knnIndexDist[,1] # as above
        cnt <- c(1,1,1)
         # If more than 1x1 pixel is requested we adjust the idx by res_spat/2 and count by res_spa
          idx_lon <- idx_lon - floor(res_spat/2)
          idx_lon <- idx_lon - floor(res_spat/2)
          cnt <- c(res_spat, res_spat, 1)
        
        out <- ncvar_get(nc, vr, start=c(idx_lon, idx_lat, t), count = cnt) # vr is the variable you want
        
        #print(out)
        
        mat$U_wind[i] <- mean(out, na.rm = TRUE) #[i,j]
        mat$time[i] <- out_time[t]
        mat$estuary[i] <- as.character(dat$Estuary[i])
        
        
      }
      full_dat <- rbind(full_dat, mat)
    }
    nc_close(nc)
    
    fwrite(full_dat, file = paste(save_names[f],"_estuary_winds", '.csv', sep = ""))
}
    #mat

