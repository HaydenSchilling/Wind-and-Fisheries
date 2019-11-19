# Wind Data get into usable format for each estuary location
# Hayden Schilling 21/6/19

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(tools)
library(data.table)
library(dplyr)

file_list <- list.files("Wind Data/u Wind/", pattern = ".nc")
file_names <- paste("Wind Data/u Wind/", file_list, sep = "")
save_names <- file_path_sans_ext(file_names)

estuary_info <- read.csv("Estuary Lat Longs.csv", header = T)

for (i in 1:length(file_list)) {
nc_data <- nc_open(file_names[i])
#nc_data <- nc_open('Wind Data/u Wind/u_wind_1851_1900.nc')

# Save the print(nc) dump to a text file
{
  sink(paste(file_path_sans_ext(file_names[i]),'.txt', sep = ""))
  print(nc_data)
  sink()
}

lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time","units")
Time <- as.POSIXct(t*3600, origin ="1800-01-01 00:00", tz = "GMT") #+ as.difftime(t, units = "hours")

head(lat) # look at the first few entries in the latitude vector

wind.array <- ncvar_get(nc_data, "uwnd") # store the data in a 3-dimensional array
dim(wind.array)

fillvalue <- ncatt_get(nc_data, "uwnd", "missing_value")
fillvalue

nc_close(nc_data) 

# Replace missing values with NA
wind.array[wind.array == fillvalue$value] <- NA

# Get one slice - test
wind.slice <- wind.array[, , 1] 
dim(wind.slice)
plot(wind.slice)


r <- raster(t(wind.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

plot(r)

# Get data at a study site
r_brick <- brick(wind.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon),
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

for (j in 1:nrow(estuary_info)){

  # Estuary co-ordinates
  toolik_lon <- estuary_info$Longitude[j]
  toolik_lat <- estuary_info$Latitude[j] * -1 + 0.05 # To make negative Latitude and go to sea a little bit
  wind_series <- extract(r_brick, SpatialPoints(cbind(toolik_lat,toolik_lon)), method='simple')
  
  data_df <- data.frame(Time = Time, U_Wind=t(wind_series), Estuary = estuary_info$Estuary[j])
  
  fwrite(data_df, file = paste(save_names[i],"_", estuary_info$Estuary[j], '.csv', sep = ""))
  }
}

#######################################################################################################
#### Now for v winds ##################################################################################
#######################################################################################################

file_list <- list.files("Wind Data/V Wind/", pattern = ".nc")
file_names <- paste("Wind Data/V Wind/", file_list, sep = "")
save_names <- file_path_sans_ext(file_names)

for (i in 1:length(file_list)) {
  nc_data <- nc_open(file_names[i])
  #nc_data <- nc_open('Wind Data/V Wind/v_wind_1851_1900.nc')
  
  # Save the print(nc) dump to a text file
  {
    sink(paste(file_path_sans_ext(file_names[i]),'.txt', sep = ""))
    print(nc_data)
    sink()
  }
  
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = F)
  t <- ncvar_get(nc_data, "time")
  tunits <- ncatt_get(nc_data, "time","units")
  Time <- as.POSIXct(t*3600, origin ="1800-01-01 00:00", tz = "GMT") #+ as.difftime(t, units = "hours")
  
  head(lat) # look at the first few entries in the longitude vector
  
  wind.array <- ncvar_get(nc_data, "vwnd") # store the data in a 3-dimensional array
  dim(wind.array)
  
  fillvalue <- ncatt_get(nc_data, "vwnd", "missing_value")
  fillvalue
  
  nc_close(nc_data) 
  
  # Replace missing values with NA
  wind.array[wind.array == fillvalue$value] <- NA
  
  # Get one slice - test
  wind.slice <- wind.array[, , 1] 
  dim(wind.slice)
  plot(wind.slice)
  
  
  r <- raster(t(wind.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
              crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  plot(r)
  
  # Get data at a study site
  r_brick <- brick(wind.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon),
                   crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
  
  for (j in 1:nrow(estuary_info)){
    
    # Estuary co-ordinates
    toolik_lon <- estuary_info$Longitude[j]
    toolik_lat <- estuary_info$Latitude[j] * -1 + 0.05 # To make negative Latitude and go to sea a little bit
    wind_series <- extract(r_brick, SpatialPoints(cbind(toolik_lat,toolik_lon)), method='simple')
    
    data_df <- data.frame(Time = Time, V_Wind=t(wind_series), Estuary = estuary_info$Estuary[j])
    
    fwrite(data_df, file = paste(save_names[i],"_", estuary_info$Estuary[j], '.csv', sep = ""))
  }
}


#############################################################################################################
### Combine u and v wind year files into one u file and one v file ##########################################
#############################################################################################################

# U data first
file_list <- list.files("Wind Data/U Wind/", pattern = ".csv")
file_names <- paste("Wind Data/U Wind/", file_list, sep = "")

dat_list <- list()
for (i in 1:length(file_list)) {
  my_data <- read.csv(file_names[i], header = T)
  dat_list[[i]] <- my_data
}

full_data_U <- as.data.frame(rbindlist(dat_list, fill = TRUE))
#full_data_U <- full_data_U[,c(2:3)]
#fwrite(full_data_U, file = "Wind Data/U_data_full_data.csv")

# now V data
file_list <- list.files("Wind Data/V Wind/", pattern = ".csv")
file_names <- paste("Wind Data/V Wind/", file_list, sep = "")

dat_list <- list()
for (i in 1:length(file_list)) {
  my_data <- read.csv(file_names[i], header = T)
  dat_list[[i]] <- my_data
}

full_data_V <- as.data.frame(rbindlist(dat_list, fill = TRUE))
#full_data_V <- full_data_V[,c(2:3)]
#fwrite(full_data_V, file = "Wind Data/V_data_full_data.csv")

full_data <- inner_join(full_data_U,full_data_V, by = c("Time", "Estuary"))
fwrite(full_data, file = "Wind Data/Wind_data_full_all_estuaries_vectors.csv")
head(full_data)
