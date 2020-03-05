# Test to open a BARRA File

library(ncdf4)
library(tidyverse)

mydata <- nc_open("../BOM Data/BARRA Model/av_uwnd10m/1993/03/av_uwnd10m-fc-slv-PT1H-BARRA_SY-v1-19930315T1200Z.sub.nc")


print(mydata)



# Get lat and long
lon <- ncvar_get(mydata,"longitude")
nlon <- dim(lon)
#head(lon)
lat <- ncvar_get(mydata,"latitude")
nlat <- dim(lat)
#head(lat)
print(head(c(nlon,nlat)))

# Get time and check format
time <- ncvar_get(mydata,"time")
#time
tunits <- ncatt_get(mydata,"time","units")
tunits
nt <- dim(time)
nt

# Get the Wind values
u_wind_array <- ncvar_get(mydata,"av_uwnd10m")
dlname <- ncatt_get(mydata,"av_uwnd10m","long_name")
dunits <- ncatt_get(mydata,"av_uwnd10m","units")
fillvalue <- ncatt_get(mydata,"av_uwnd10m","_FillValue") #fills empty cells with specified fill value (NaN)
dim(u_wind_array)

# close .nc file
nc_close(mydata)


library(lubridate)
## Choose hour you want and subset large file

hour = 1
u_wind_array2 <- u_wind_array[,,hour]

# Make into a dataframe
u_wind_df <- as.data.frame.array(u_wind_array2)
colnames(u_wind_df) <- lat
u_wind_df$Longitude <- lon

# Make Long Format
long_u_wind_df <- pivot_longer(u_wind_df, cols = 1:742, names_to = "Latitude", values_to = "U_Wind")
head(long_u_wind_df)

# Check data structure
str(long_u_wind_df)
long_u_wind_df$Latitude <- as.numeric(long_u_wind_df$Latitude) # make Latitude numeric

# Jet colour pallete
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# This will be slow! Very high resolution grid
p1 <- ggplot(long_u_wind_df, aes(x = Longitude, y = Latitude, col = U_Wind)) +
  geom_tile() + coord_quickmap() + scale_colour_gradientn(colors = jet.colors(7)) #, limits = c(7,28)
p1

ggsave("../BOM Data/BARRA Model/Demo_u_wind.png", dpi = 300, height = 15, width = 15, units = "cm")
ggsave("../BOM Data/BARRA Model/Demo_u_wind.pdf", height = 15, width = 15, units = "cm")

nc_close(mydata)




#### Try to make one big file
#install.packages("ncdf.tools")
library(ncdf.tools)

file_list <- list.files("../BOM Data/BARRA Model/av_uwnd10m/1990/01/", full.names = TRUE, recursive = TRUE, pattern =".nc")

output <- transNcdfMerge(file_list)




