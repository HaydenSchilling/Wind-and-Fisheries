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


# Final Plots
# Bream heatmap + Mullet and Flathead Upwelling Favourable Winds

B_plot <-  ggplot(bream_dat, aes(x = Southeast.Winds,y = Northeast.Winds, fill = Abundance/1000)) + geom_tile() +
   geom_contour(col="white", aes(z = Abundance/1000)) +#, binwidth = 0.002
  scale_x_continuous(expand = c(0,0)) + 
  scale_y_continuous(expand = c(0,0)) +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + 
  viridis::scale_fill_viridis(option = "magma", name="Predicted\nCatch\n(t / year)",
                              trans="log10") + # or geom_raster()
  xlab("Downwelling \nFavourable Winds") + ylab("Upwelling \nFavourable Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 14),
        axis.text.x  = element_text(colour="black", size = 12), 
        axis.title.y = element_text(face="bold", colour="black", size = 14),
        axis.text.y  = element_text(colour="black", size = 12),
        axis.ticks = element_line(colour="black"),
        legend.title = element_text(colour="black", size = 12, face = "bold"),
        legend.text = element_text(colour="black", size=10),
        title = element_text(size=12, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(0.5, "cm")) +
  ggtitle("a) Bream")

B_plot


# Need to run models in CPUE Modelling Final.rmd first

M_dat <- ggeffect(m2, terms = "X45_degree_winds.standardised [all]")
M_dat$Term <- "Upwelling \nFavourable Winds"

M_plot <- ggplot(M_dat, aes(x, predicted/1000))+
  geom_line() + xlab("Upwellwing\nFavourable Winds")+
  geom_ribbon(aes(ymin = conf.low/1000, ymax = conf.high/1000), alpha = .1) +
  theme_classic() + theme(axis.text  = element_text(colour="black", size = 12), 
                          axis.title = element_text(face="bold", colour="black", size = 14),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1),
                          title = element_text(size=12, face = "bold"))+
  ylab("Predicted Catch (t / year)") +
  ggtitle("c) Mullet")
M_plot  

F_dat <- ggeffect(m5, terms = "X45_degree_winds.standardised [all]")
F_dat$Term <- "Upwelling \nFavourable Winds"

F_plot <- ggplot(F_dat, aes(x, predicted/1000))+
  geom_line() + xlab("Upwellwing\nFavourable Winds")+
  geom_ribbon(aes(ymin = conf.low/1000, ymax = conf.high/1000), alpha = .1) +
  theme_classic() + theme(axis.text  = element_text(colour="black", size = 12), 
                          axis.title = element_text(face="bold", colour="black", size = 14),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1),
                          title = element_text(size=12, face = "bold"))+
  ylab("Predicted Catch (t / year)") +
  ggtitle("b) Flathead")
F_plot  

library(patchwork)
B_plot  + F_plot+ M_plot
ggsave("../plots/CPUE Prediction plots.png", width = 21, height = 11, units="cm", dpi = 600)
