# Coef plots larval fish 3 day

# The model coefs were extracted from the GLMMs for larval fish and LMM for the CPUE models into csv files which are loaded here
library(tidyverse)
library(ggplot2)
library(scales)
library(ggallin)

dat3 <- read.csv("../Data/Larval fish model coefs 3 day Bayesian.csv", header = T)
dat3$Model <- "B) 3 Day"
dat14 <- read.csv("../Data/Larval fish model coefs 14 day Bayesian.csv", header = T)
dat14$Model <- "A) 14 Day"

larval_coef_dat <- bind_rows(dat3, dat14)
larval_coef_dat$Parameter <- as.character(larval_coef_dat$Parameter)

# Faceted coefficient plot
p1 <- ggplot(larval_coef_dat, aes(Parameter, .value)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="red") +
  geom_errorbar(aes(ymin= .lower, ymax= .upper), 
                lwd=1, width=0, colour="black") +
  geom_point(size=2, colour = "black") +
  facet_grid(.~ Model, scales = "free") +
  coord_flip() +
  guides(colour=FALSE) +
  labs(x="Parameter", y="Estimate (95% CI)") +
  theme_classic() +
  scale_y_continuous(trans = ssqrt_trans, breaks = c(-40, -20, -10, -1,1,10,20)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 13), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 13),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"),
        #panel.spacing = unit(1, "lines") # adjusts spacing of panels
)

p1

## save plots
ggsave("../plots/Larval Model Coefs.pdf", width = 21, height = 14.8, units = "cm")
ggsave("../plots/Larval Model Coefs.png", width = 21, height = 14.8, units = "cm", dpi = 600)


### Now CPUE MODEL PLOT

mydata <- read.csv("../Data/LMM coefs.csv", header = T)

all_sp_dat <- subset(mydata, Species == "a) All Species")

pALL <- ggplot(all_sp_dat, aes(Model.Term, coef.est)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="red") +
  geom_errorbar(aes(ymin=coef.est - coef.se, ymax=coef.est + coef.se), 
                lwd=1, width=0, colour="black") +
  geom_point(size=2, colour = "black") +
  #facet_wrap(.~ Species) +
  coord_flip() +
  guides(colour=FALSE) +
  labs(x="Parameter", y="Estimate (± SE)") +
  theme_classic() +
  scale_y_continuous(trans = ssqrt_trans, breaks=seq(-4,1,1)) +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 13), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 13),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 14, hjust=0),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold")
        #panel.spacing = unit(1, "lines") # adjusts spacing of panels
  )

pALL

## Save plots
ggsave("../plots/All species CPUE Model Coefs.pdf", width = 21, height = 14.8, units = "cm")
ggsave("../plots/All species CPUE Model Coefs.png", width = 21, height = 14.8, units = "cm", dpi = 600)



# Faceted coefficient plot for all species
p2 <- ggplot(mydata, aes(Model.Term, coef.est)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="red") +
  geom_errorbar(aes(ymin=coef.est - coef.se, ymax=coef.est + coef.se), 
                lwd=1, width=0, colour="black") +
  geom_point(size=2, colour = "black") +
  facet_wrap(.~ Species) +
  coord_flip() +
  guides(colour=FALSE) +
  labs(x="Parameter", y="Estimate (± SE)") +
  theme_classic() +
  scale_y_continuous(trans = ssqrt_trans, breaks=c(-3, -2, -1, 0,1,2,4,6,8)) +
    theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 10), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 10),
        axis.ticks = element_line(colour="black"),
        strip.text = element_text(colour="black", face = "bold", size = 11, hjust=0.5),
        strip.background = element_rect(colour = "white"),
        #legend.justification=c(1,0), legend.position="right",
        panel.border = element_rect(colour = "black", fill=NA, size = 1),
        #legend.key.size = unit(1, "cm"),
        legend.title = element_text(face = "bold", size = 14),
        legend.text = element_text(size = 12, face = "bold"))

p2

## Save plots
ggsave("../plots/Faceted Species CPUE Model Coefs.pdf", width = 21, height = 14.8, units = "cm")
ggsave("../plots/Faceted Species CPUE Model Coefs.png", width = 21, height = 14.8, units = "cm", dpi = 600)

