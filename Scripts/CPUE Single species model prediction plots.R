# Bream heatmap prediction

library(ggplot2)
library(viridis)

mydata <- read.csv("../Bream CPUE heatmap data.csv", header = T)


p <- ggplot(mydata, aes(x = Southeast.Winds,y = Northeast.Winds, z = Abundance)) + geom_tile(aes(fill = Abundance)) + #stat_contour() +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + scale_fill_viridis(name = "Predicted CPUE", option = "magma") + # or geom_raster() 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + 
  geom_contour(col="white") +
  xlab("Standardised Southeast Winds") + ylab("Standardised Northeast Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        #legend.position = c(.85, .25),
        legend.title = element_text(colour="black", face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

p

# Standard error plot

p2 <- ggplot(mydata, aes(x = Southeast.Winds,y = Northeast.Winds, z = Error)) + geom_tile(aes(fill = Error)) + #stat_contour() +
  #scale_fill_gradient(low = "blue", high = "red") + 
  theme_classic() + scale_fill_viridis(name = "Standard Error", option = "magma") + # or geom_raster() 
  scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + 
  geom_contour(col = "white", binwidth = 0.2)+
  xlab("Standardised Southeast Winds") + ylab("Standardised Northeast Winds") +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 18),
        axis.text.x  = element_text(colour="black", size = 14), 
        axis.title.y = element_text(face="bold", colour="black", size = 18),
        axis.text.y  = element_text(colour="black", size = 14),
        axis.ticks = element_line(colour="black"),
        #legend.position = c(.85, .25),
        legend.title = element_text(colour="black", face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1))

p2


# Flathead
library(ggplot2)
library(DHARMa)
library(bootpredictlme4)
library(merTools)
library(effects)
library(MuMIn)
library(dplyr)
library(ggeffects)
# Use Log10(CPUE) rather than standardise


# Load Data
mydata2 <- read.csv("../Full_Data_Modelling_BARRA_by_estuary.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv
# mydata2 <-  read.csv("Full_Data_Modelling.csv", header = T)
# 
# cor.test(mydata$X45_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X45_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X45_degree_winds, mydata2$X45_degree_winds)
# # NE winds not correlated by SE winds are.....

# Remove Total CPUE
mydata2 <- subset(mydata2, Species != "Total")

### Standardise Winds
my.df <-  mydata2 %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                            X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))
# Bream
bream <- subset(my.df, Species == "Bream")
m2 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type * Drought_Months + (1|Estuary), data = bream)


r.squaredGLMM(m2) # 0.127
AIC(m2)

pF1 <- plot(ggpredict(m2, terms = "X135_degree_winds.standardised [all]"))
pF1
pF2 <- plot(ggpredict(m2, terms = "X45_degree_winds.standardised [all]"))
pF2


N_dat <- ggpredict(m2, terms = "X45_degree_winds.standardised [all]", type = "fe")
s_dat <- ggpredict(m2, terms = "X135_degree_winds.standardised [all]", type = "fe")

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
  ylab("Predicted CPUE (Mean & 95% CI)")

# Flathead
flathead <- subset(my.df, Species == "Flathead")
m5 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type * Drought_Months + (1|Estuary), data = flathead)


r.squaredGLMM(m5) # 0.127
AIC(m5)

pF1 <- plot(ggpredict(m5, terms = "X135_degree_winds.standardised [all]"))
pF1
pF2 <- plot(ggpredict(m5, terms = "X45_degree_winds.standardised [all]"))
pF2


N_dat <- ggpredict(m5, terms = "X45_degree_winds.standardised [all]", type = "fe")
s_dat <- ggpredict(m5, terms = "X135_degree_winds.standardised [all]", type = "fe")

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
  ylab("Predicted CPUE (Mean & 95% CI)")

# Mullet
mullet <- subset(my.df, Species == "Mullet")
m6 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type * Drought_Months + (1|Estuary), data = mullet)


r.squaredGLMM(m6) # 0.127
AIC(m6)

pF1 <- plot(ggpredict(m6, terms = "X135_degree_winds.standardised [all]"))
pF1
pF2 <- plot(ggpredict(m6, terms = "X45_degree_winds.standardised [all]"))
pF2


N_dat <- ggpredict(m6, terms = "X45_degree_winds.standardised [all]")
s_dat <- ggpredict(m6, terms = "X135_degree_winds.standardised [all]")

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
  ylab("Predicted CPUE (Mean & 95% CI) ")

# Whiting
whiting <- subset(my.df, Species == "Whiting")
m7 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type * Drought_Months + (1|Estuary), data = whiting)


r.squaredGLMM(m7) # 0.127
AIC(m7)

pF1 <- plot(ggpredict(m7, terms = "X135_degree_winds.standardised [all]"))
pF1
pF2 <- plot(ggpredict(m7, terms = "X45_degree_winds.standardised [all]"))
pF2


N_dat <- ggpredict(m7, terms = "X45_degree_winds.standardised [all]")
s_dat <- ggpredict(m7, terms = "X135_degree_winds.standardised [all]")

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
  ylab("Predicted CPUE (Mean & 95% CI")

# Luderick
luderick <- subset(my.df, Species == "Luderick")
m8 <- lmer(log10(CPUE) ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type * Drought_Months + (1|Estuary), data = luderick)


r.squaredGLMM(m8) # 0.127
AIC(m8)

pF1 <- plot(ggpredict(m8, terms = "X135_degree_winds.standardised [all]"))
pF1
pF2 <- plot(ggpredict(m8, terms = "X45_degree_winds.standardised [all]"))
pF2


N_dat <- ggpredict(m8, terms = "X45_degree_winds.standardised [all]")
s_dat <- ggpredict(m8, terms = "X135_degree_winds.standardised [all]")

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
  ylab("Predicted CPUE (Mean & 95% CI")


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

ggsave("../plots/Multispecies CPUE Prediction.png", height = 14.8, width = 21, units = "cm", dpi = 600)
ggsave("../plots/Multispecies CPUE Prediction.pdf", height = 14.8, width = 21, units = "cm", dpi = 600)


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

ggsave("../plots/Multispecies drought and estuary type effects.png", height = 14.8, width = 21, units = "cm", dpi = 600)
ggsave("../plots/Multispecies drought and estuary type effects.pdf", height = 14.8, width = 21, units = "cm", dpi = 600)
