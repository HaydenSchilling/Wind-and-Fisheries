# Heatmap prediction plots for CPUE Species

library(tidyverse)

# load data
bream_dat <- read_csv("../Data/Bream heamap prediction data.csv")
bream_dat$Species <- "a) Bream"
mullet_dat <- read_csv("../Data/Mullet heamap prediction data.csv")
mullet_dat$Species <- "d) Mullet"
flathead_dat <- read_csv("../Data/Flathead heamap prediction data.csv")
flathead_dat$Species <- "b) Flathead"
whiting_dat <- read_csv("../Data/Whiting heamap prediction data.csv")
whiting_dat$Species <- "e) Whiting"
luderick_dat <- read_csv("../Data/Luderick heamap prediction data.csv")
luderick_dat$Species <- "c) Luderick"

# Combine dataframes
full_dat <- bind_rows(bream_dat, mullet_dat, flathead_dat, whiting_dat, luderick_dat)


# Rescale values to between zero and one to assist plotting
full_dat <- full_dat %>% group_by(Species) %>% mutate(Scaled_Catch = Abundance/(min(Abundance))) # rescale(Abundance)

# Make plot
pF <- ggplot(full_dat, aes(x = Southeast.Winds,y = Northeast.Winds)) + geom_tile(aes(fill = Scaled_Catch)) +
  facet_wrap(~Species, ncol=3)+
  geom_contour(col="white", aes(z = Scaled_Catch), binwidth = 0.2) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + 
  viridis::scale_fill_viridis(option = "magma", name="Predicted\nScaled\nCatch",
                              trans="log10", breaks=c(1,2,3,4,5)) + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_rect(colour = "white"),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10),
        legend.position = c(0.85,0.23)) 

pF

ggsave("../plots/CPUE Heatmap Predictions.png", width=21, height = 14.8, units ="cm", dpi = 600)
ggsave("../plots/CPUE Heatmap Predictions.pdf", width=21, height = 14.8, units ="cm", dpi = 600)
