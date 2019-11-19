## Make CPUE heatmaps for all species

library(ggplot2)
library(viridis)

mydata <- read.csv("Combined Species CPUE heatmap data.csv", header = T)


variable_names <- list(
  "Bream" = "a) Bream (P < 0.001)" ,
  "Flathead" = "b) Flathead (P =0.041)",
  "Luderick" = "c) Luderick (P = 0.278)" ,
  "Mullet" = "d) Mullet (P = 0.028)",
  "Whiting" = "e) Whiting (P = 0.161)"
)

variable_labeller <- function(variable,value){
  return(variable_names[value])
}


p <- ggplot(mydata, aes(x = Southeast.Winds,y = Northeast.Winds, z = Abundance)) + geom_tile(aes(fill = Abundance)) + #stat_contour() +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + scale_fill_viridis(name = "Predicted CPUE", option = "magma") + # or geom_raster() 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + 
  facet_wrap(~Species, labeller=variable_labeller) + geom_contour(col="white") +
  xlab("Standardised Southeast Winds") + ylab("Standardised Northeast Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.position = c(.85, .25),
        legend.title = element_text(colour="black", face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

p

ggsave("plots/All Species CPUE Predictions.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/All Species CPUE Predictions.png", dpi = 600, height = 14.8, width = 21, units = "cm")

### Now Standard error plot

p2 <- ggplot(mydata, aes(x = Southeast.Winds,y = Northeast.Winds, z = Error)) + geom_tile(aes(fill = Error)) + #stat_contour() +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + scale_fill_viridis(name = "Standard Error", option = "magma") + # or geom_raster() 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + 
  facet_wrap(~Species, labeller=variable_labeller) + geom_contour(col = "white")+
  xlab("Standardised Southeast Winds") + ylab("Standardised Northeast Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        legend.position = c(.85, .25),
        legend.title = element_text(colour="black", face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

p2

ggsave("plots/All Species CPUE Predictions Error.pdf", height = 14.8, width = 21, units = "cm")
ggsave("plots/All Species CPUE Predictions Error.png", dpi = 600, height = 14.8, width = 21, units = "cm")
