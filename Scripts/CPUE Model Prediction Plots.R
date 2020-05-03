# CPUE Model predictions

library(ggplot2)
library(bootpredictlme4)
library(merTools)
library(MuMIn)
library(dplyr)
library(ggeffects)
# Use Log10(CPUE) rather than standardise

# Load Data
mydata2 <- read.csv("../Data/Full_Data_Modelling_BARRA_by_estuary.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv


# Remove Total CPUE
mydata2 <- subset(mydata2, Species != "Total")

### Standardise Winds
my.df <-  mydata2 %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                             X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))

# Multispecies
m9 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type *Drought_Months + (Species|Estuary), data = my.df)

N_dat <- ggpredict(m9, terms = "X45_degree_winds.standardised [all]")
s_dat <- ggpredict(m9, terms = "X135_degree_winds.standardised [all]")

N_dat$Term <- "Upwelling \nFavourable Winds"
s_dat$Term <- "Downwelling \nFavourable Winds"


plot_dat <- bind_rows(N_dat, s_dat)

ggplot(plot_dat, aes(x, predicted)) + facet_wrap(~Term, scales = "free_x", switch = "x") +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
  theme_classic() + theme(axis.title.x = element_blank(),
                          axis.text.x  = element_text(colour="black", size = 12), 
                          axis.title.y = element_text(face="bold", colour="black", size = 14),
                          axis.text.y  = element_text(colour="black", size = 14),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  ylab(expression("Predicted"~"log"[10]~"(CPUE;"~"Estimate"~"&"~"95%"~"CI)"))

## Save plots
#ggsave("../plots/Multispecies CPUE Prediction.png", height = 14.8, width = 21, units = "cm", dpi = 600)
#ggsave("../plots/Multispecies CPUE Prediction.pdf", height = 14.8, width = 21, units = "cm", dpi = 600)

# Predictions for Drought
plot(ggpredict(m9, terms = c("Drought_Months [all]", "Estuary_Type")))+
  scale_x_continuous(expand=c(0,0))+
  scale_colour_discrete(name="Estuary Type") + ggtitle(NULL) +
  theme_classic() + theme(axis.text.x  = element_text(colour="black", size = 14), 
                          axis.title.x = element_text(face="bold", colour="black", size = 14),
                          axis.title.y = element_text(face="bold", colour="black", size = 14),
                          axis.text.y  = element_text(colour="black", size = 14),
                          legend.title = element_text(colour="black", size=14, face="bold"),
                          legend.text = element_text(colour="black", size=13),
                          axis.ticks = element_line(colour="black"),
                          strip.text = element_text(colour="black", face = "bold", size = 13),
                          strip.background = element_blank(),
                          #strip.placement = "outside",
                          #legend.justification=c(1,0), legend.position="right",
                          panel.border = element_rect(colour = "black", fill=NA, size = 1))+
  ylab("Predicted CPUE (Estimate & 95% CI)") + xlab("Dought Months in Previous Year")

## Save plots
#ggsave("../plots/Multispecies drought and estuary type effects.png", height = 14.8, width = 21, units = "cm", dpi = 600)
#ggsave("../plots/Multispecies drought and estuary type effects.pdf", height = 14.8, width = 21, units = "cm", dpi = 600)
