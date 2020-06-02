### Map figure
library(rgdal) 
library(sp)
library(raster)
library(tidyverse)

# Load Fish Data
fish_data <- read.csv("../Data/allNIMO_dist.csv", header = T)
str(fish_data)

# Restrict to NSW and on the continental shelf
fish_data <- filter(fish_data, Latitude <= -30 & Latitude >= -36 & Longitude > 140)
fish_data <- filter(fish_data, Bathym_m <= 1000)
summary(fish_data$Bathym_m)
hist(fish_data$Bathym_m)

#Load map data
Aus <- readOGR(dsn = "../Shape files/australia",layer = "cstauscd_r")
#plot(Aus)
Aus_coast <- subset(Aus, FEAT_CODE != "sea" )

head(Aus_coast)

#plot(Aus_coast)

min_lon <- 147
max_lon <- 160
min_lat <- -37
max_lat <- -28

geo_bounds <- c(left = min_lon, bottom = min_lat, right = max_lon, top = max_lat)

Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

coordinates(Sites.grid) <- ~ lon_bound + lat_bound

Aus_crop <- crop(Aus_coast, extent(Sites.grid)) #rgeos must be installed to run

lon.min <- 147
lon.max <- 155
lat.min <- -37
lat.max <- -28

shelf <- read.csv("../Shape files/Hayden_1000m_contour.csv", header = T)

dots <- read.csv("../Data/Estuary Lat Longs2.csv", header = T)

cols = "red"
shapes = "1"



p1 <- ggplot(fish_data, aes(x = Longitude, y = Latitude)) + theme_classic() + 
  labs(x=expression(paste("Longitude (",degree, ")", sep="")), y=expression(paste("Latitude (", degree, ")"))) +
    scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + #, limits = c(147,155) , limits = c(-37,-28)
  #scale_fill_distiller(palette = "Spectral", na.value=NA, trans="log10") +
  coord_quickmap(xlim = c(lon.min, lon.max), ylim=c(lat.min, lat.max)) + #coord_map() + #  # this line could be very slow
  geom_polygon(data = Aus_crop, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60")+
  geom_polygon(data=shelf, aes(long,lat, group = group),colour="black", fill=NA, size=0.3)+
  #geom_path(data=shelf, aes(x=long, y = lat)) + 
  geom_point(alpha = 0.4, aes(shape = shapes), size = 2)+
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14),
        strip.background = element_rect(colour = NULL),
        legend.justification=c(1,0), legend.position="none", legend.direction = "horizontal",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size = 12, face = "bold"),
        legend.title=element_blank(),
        legend.background = element_blank())+
  geom_point(data = dots, aes(x = Longitude, y = Latitude*-1, col = cols), size = 2) +
  scale_colour_manual(values = cols, label ="Estuarine \nFishery")+
  scale_shape_manual(values = 1, label ="Larval Fish \nSample") +
  geom_point(data = NULL, aes(x = 151.258693, y = 33.838509*-1), size = 2, col = "blue") +
  geom_text(data = NULL, aes(x = 151.258693, y = 33.838509*-1, label = "Sydney"), nudge_x = -0.75)# +
 # geom_text(data = dots, aes(x = Longitude, y = Latitude*-1, label = Estuary), nudge_x = -1.5)

p1

#ggsave("../plots/Figure 1.pdf")
#ggsave("../plots/Figure 1.png", dpi = 600)


### Online code for inset
##################################
# Creating Inset Maps in ggplot2 #
##################################

library(ggplot2)
library(raster)
library(grid)
library(gridExtra)
# 
# ph0<-getData("GADM", country="PHL", level=0) # download PHL level 0 map for ucdavis site
# phl<-getData("GADM", country="PHL", level=2) # download PHL level 2 map for ucdavis site
# mrdq<-(phl[phl$NAME_1=="Marinduque",]) # subset province of Marinduque from PHL map
# munnames<-coordinates(mrdq) # get center coordinates of municipalities of Marinduque
# munnames<-data.frame(munnames) # convert matrix format munnames object to data.frame
# munnames$label<-mrdq@data$NAME_2

# Extent rectangle for inset map
pol<-data.frame(xmin=147,xmax=155 ,ymin=-37 ,ymax=-28)


# Main Map
# p1<-ggplot()+geom_polygon(data=mrdq, aes(long+0.008,lat-0.005, group=group), fill="#9ecae1")+
#   geom_polygon(data=mrdq, aes(long,lat, group=group), colour="grey10",fill="#fff7bc")+
#   geom_text(data=munnames, aes(x=X1, y=X2,label=label), size=3, colour="grey20")+
#   coord_equal()+theme_bw()+xlab("")+ylab("")+
#   scale_x_continuous(breaks=seq(121.8,122.2, 0.1), labels=c(paste(seq(121.8,122.2, 0.1),"°E", sep="")))+
#   scale_y_continuous(breaks=seq(13.2,13.6, 0.1), labels=c(paste(seq(13.2,13.6, 0.1),"°N", sep="")))+
#   theme(axis.text.y =element_text(angle = 90, hjust=0.5))

#Inset
p2<-ggplot()+geom_polygon(data=Aus_coast, aes(x=long, y = lat, group = group), fill = "gray60", colour = "gray60") +
  coord_quickmap()+theme_bw()+labs(x=NULL,y=NULL) + 
  #scale_x_continuous(breaks=seq(117.5,125, 2.5), labels=c(paste(seq(117.5,125, 2.5),"°E", sep="")))+
  #scale_y_continuous(breaks=seq(5,20, 5), labels=c(paste(seq(5,20, 5),"°N", sep="")))+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha=0, colour="red", size = 1, linetype=1)+
  theme(axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),axis.title.x =element_blank(),
        axis.title.y= element_blank(), plot.background = element_rect(fill = "transparent", color = NA)
        ) 
p2

#ggsave("test.pdf")

# Put inset on top of other figure
png(file="../plots/Figure 1.png",w=3600,h=3600, res=600)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.35, x = 0.37, y = 0.86, clip = "off") #plot area for the inset map
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()

# and make .pdf
pdf(file="../plots/Figure 1.pdf",w=7,h=7)
grid.newpage()
v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
v2<-viewport(width = 0.3, height = 0.35, x = 0.361, y = 0.86, clip = "off") #plot area for the inset map
print(p1,vp=v1) 
print(p2,vp=v2)
dev.off()
