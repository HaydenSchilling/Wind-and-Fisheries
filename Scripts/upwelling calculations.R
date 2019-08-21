## Script to calculate upwelling index.

# Using this dataset: FNMOC Wind and Ekman Transport Data, 360x180, Monthly, from 6-hr Pressure
# https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnWPr.html

## download data first

#url = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnTran6.csv?curl[(1989-10-01):1:(2015-01-01)][(-37):1:(-28)][(149):1:(155)],ektrx[(1989-10-01):1:(2015-01-01)][(-37):1:(-28)][(149):1:(155)],ektry[(1989-10-01):1:(2015-01-01)][(-37):1:(-28)][(149):1:(155)]"

#download.file(url, destfile = "upwelling data_6hr.csv")

#mydata <- read.csv("upwelling data_6hr.csv")

# monthly url needs to be downloaded manually for some reason, not with R

#url = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/erdlasFnWPr.nc?pmsl[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],u_mean[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],v_mean[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],uv_mag_mean[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],taux_mean[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],tauy_mean[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],curl[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],ektrx[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)],ektry[(1989-09-16):1:(2019-05-17T12:00:00Z)][(-37):1:(-27)][(150):1:(155)]"

#download.file(url, destfile = "test")


library(ncdf4)


nc_data <- nc_open("Monthly_upwelling.nc")

str(nc_data)


lon <- ncvar_get(nc_data, "longitude")
lat <- ncvar_get(nc_data, "latitude", verbose = F)
t <- ncvar_get(nc_data, "time")
tunits <- ncatt_get(nc_data, "time","units")
Time <- as.POSIXct(t, origin ="1970-01-01 00:00", tz = "GMT") #+ as.difftime(t, units = "hours")

head(Time)

head(lat) # look at the first few entries in the longitude vector

Ex.array <- ncvar_get(nc_data, "ektrx") # store the data in a 3-dimensional array
dim(Ex.array)

#fillvalue <- ncatt_get(nc_data, "uwnd", "missing_value")
#fillvalue

Ey.array <- ncvar_get(nc_data, "ektry") # store the data in a 3-dimensional array
dim(Ey.array)

nc_close(nc_data) 

### Do Ektrx

# Get one slice - test
Ex.slice <- Ex.array[, , 1] 
dim(Ex.slice)
#plot(Ex.slice) # not sure what this is doing


r <- raster(t(Ex.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

plot(r)

Ey.slice <- Ey.array[, , 1] 
dim(Ey.slice)
#plot(Ex.slice) # not sure what this is doing


r2 <- raster(t(Ey.slice), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), 
            crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
plot(r2)



# Make raster brick
r_brickx <- brick(Ex.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon),
                 crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r_bricky <- brick(Ey.array, xmn=min(lat), xmx=max(lat), ymn=min(lon), ymx=max(lon),
                  crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

# get data at study sites
estuary_info <- read.csv("Estuary Lat Longs.csv", header = T)
library(data.table)

for (j in 1:nrow(estuary_info)){
  
  # Estuary co-ordinates
  toolik_lon <- estuary_info$Longitude[j]
  toolik_lat <- estuary_info$Latitude[j] * -1 + 0.05 # To make negative Latitude and go to sea a little bit
  Ex_series <- extract(r_brickx, SpatialPoints(cbind(toolik_lat,toolik_lon)), method='simple')
  Ey_series <- extract(r_bricky, SpatialPoints(cbind(toolik_lat,toolik_lon)), method='simple')
  
  data_df <- data.frame(Time = Time, ektrx=t(Ex_series),ektry=t(Ey_series), 
                        Estuary = estuary_info$Estuary[j], Coast_Angle = estuary_info$Coast_angle[j])
  
  fwrite(data_df, file = paste("Upwelling/", estuary_info$Estuary[j],"_eckman", '.csv', sep = ""))
}

### Calculate upwelling index

# Define upwelling function - From NOAA Southeast Fisheries (PFEG)
upwell <- function(ektrx, ektry, coast_angle) {
  pi <- 3.1415927
  degtorad <- pi/180.
  alpha <- (360 - coast_angle) * degtorad
  s1 <- cos(alpha)
  t1 <- sin(alpha)
  s2 <- -1 * t1
  t2 <- s1
  perp <- (s1 * ektrx) + (t1 * ektry)
  para <- (s2 * ektrx) + (t2 * ektry)
  return(perp/10)
}

library(tools)

# loop through all estuaries

file_list <- list.files("Upwelling/", pattern = ".csv")
file_names <- paste("Upwelling/", file_list, sep = "")
save_names <- file_path_sans_ext(file_names)
#file_names

for (i in 1:length(file_names)) {
  dat <- read.csv(file_names[i])
  dat$Upwelling_index <- upwell(ektrx = dat$ektrx, ektry = dat$ektry, coast_angle = dat$Coast_Angle) # calculate upwelling
  dat$Time <- ymd_hms(dat$Time, tz="UTC") # recognise time
  dat$Year <- year(dat$Time) # extract year
  dat$Month <- month(dat$Time) # extract month
  fwrite(dat, file = paste(save_names[i],"_upwelling", '.csv', sep = ""))
  #print(i)
  }

head(dat)

plot(dat$Upwelling_index)

library(ggplot2)

p1 <- ggplot(dat, aes(x = Time, y = Upwelling_index)) + geom_point() + geom_smooth()
p1
