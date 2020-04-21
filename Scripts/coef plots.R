# Coef plots larval fish 3 day
library(ggplot2)

dat3 <- read.csv("../Larval fish model coefs 3 day.csv", header = T)
dat14 <- read.csv("../Larval fish model coefs 14 day.csv", header = T)

larval_coef_dat <- bind_rows(dat3, dat14)


# Faceted coefficient plot
p1 <- ggplot(larval_coef_dat, aes(Parameter, Estimate)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="red") +
  geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se), 
                lwd=1, width=0, colour="black") +
  geom_point(size=2, colour = "black") +
  facet_grid(.~ Model, scales = "free") +
  coord_flip() +
  guides(colour=FALSE) +
  labs(x="Parameter", y="Estimate (± SE)") +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 10), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 10),
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

# slight edit in adobe - moved one axis tick label
ggsave("../plots/Larval Model Coefs.pdf", width = 21, height = 14.8, units = "cm")
ggsave("../plots/Larval Model Coefs.png", width = 21, height = 14.8, units = "cm", dpi = 600)

# 
# # Faceted coefficient plot
# ggplot(coefs, aes(vars, Estimate)) + 
#   geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
#   geom_errorbar(aes(ymin=Estimate - se, ymax=Estimate + se, colour=vars), 
#                 lwd=1, width=0) +
#   geom_point(size=3, aes(colour=vars)) +
#   facet_grid(. ~ model) +
#   coord_flip() +
#   guides(colour=FALSE) +
#   labs(x="Coefficient", y="Value") +
#   theme_grey(base_size=15)


### Now CPUE MODEL PLOT

mydata <- read.csv("LMM coefs.csv", header = T)
# Faceted coefficient plot
p2 <- ggplot(mydata, aes(Model.Term, coef.est)) + 
  geom_hline(yintercept=0, lty=2, lwd=1, colour="red") +
  geom_errorbar(aes(ymin=coef.est - coef.se, ymax=coef.est + coef.se), 
                lwd=1, width=0, colour="black") +
  geom_point(size=2, colour = "black") +
  facet_wrap(.~ Species, scales = "free_x") +
  coord_flip() +
  guides(colour=FALSE) +
  labs(x="Parameter", y="Estimate (± SE)") +
  theme_classic() +
  theme(axis.title.x = element_text(face="bold", colour="black", size = 16),
        axis.text.x  = element_text(colour="black", size = 10), 
        axis.title.y = element_text(face="bold", colour="black", size = 16),
        axis.text.y  = element_text(colour="black", size = 10),
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

p2

ggsave("plots/CPUE Model Coefs.pdf", width = 21, height = 14.8, units = "cm")
ggsave("plots/CPUE Model Coefs.png", width = 21, height = 14.8, units = "cm", dpi = 600)
