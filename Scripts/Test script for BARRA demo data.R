# Test to open a BARRA File

library(ncdf4)
library(tidyverse)

mydata <- nc_open("../BOM Data/BARRA Model/ave_u_wind_Demo_file.nc")

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

# Get the Temperature values
temp_array <- ncvar_get(mydata,"av_uwnd10m")
dlname <- ncatt_get(mydata,"av_uwnd10m","long_name")
dunits <- ncatt_get(mydata,"av_uwnd10m","units")
fillvalue <- ncatt_get(mydata,"av_uwnd10m","_FillValue") #fills empty cells with specified fill value (NaN)
dim(temp_array)

# close .nc file
nc_close(mydata)


library(lubridate)
## Choose hour you want and subset large file

hour = 1
temp_array2 <- temp_array[,,hour]

# Make into a dataframe
temp_df <- as.data.frame.array(temp_array2)
colnames(temp_df) <- lat
temp_df$Longitude <- lon

# Make Long Format
long_temp_df <- pivot_longer(temp_df, cols = 1:508, names_to = "Latitude", values_to = "U_Wind")
head(long_temp_df)

# Check data structure
str(long_temp_df)
long_temp_df$Latitude <- as.numeric(long_temp_df$Latitude) # make Latitude numeric

# Jet colour pallete
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

# This will be slow! Very high resolution grid
p1 <- ggplot(long_temp_df, aes(x = Longitude, y = Latitude, col = U_Wind)) +
  geom_tile() + coord_quickmap() + scale_colour_gradientn(colors = jet.colors(7)) #, limits = c(7,28)
p1

ggsave("../BOM Data/BARRA Model/Demo_u_wind.png", dpi = 300, height = 15, width = 15, units = "cm")
ggsave("../BOM Data/BARRA Model/Demo_u_wind.pdf", height = 15, width = 15, units = "cm")
