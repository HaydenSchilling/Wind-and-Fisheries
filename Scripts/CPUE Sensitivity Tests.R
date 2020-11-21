# CPUE Lag testing

library(tidyverse)
library(ggplot2)
library(DHARMa)
library(bootpredictlme4)
library(merTools)
library(effects)
library(MuMIn)
library(dplyr)
library(ggeffects)
library(glmmTMB)
library(tweedie)
library(brms)
# Use Log10(CPUE) rather than standardise


# Load Data
mydata <- read.csv("../Data/Full_Data_Modelling_BARRA_by_estuary.csv", header = T) # Full_Data_ModellingV3_by_estuary.csv
# mydata2 <-  read.csv("Full_Data_Modelling.csv", header = T)
# 
# cor.test(mydata$X45_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X45_degree_winds)
# cor.test(mydata$X135_degree_winds, mydata2$X135_degree_winds)
# cor.test(mydata$X45_degree_winds, mydata2$X45_degree_winds)
# # NE winds not correlated by SE winds are.....

# Remove Total CPUE
mydata <- subset(mydata, Species != "Total")
mydata <- subset(mydata, Species != "Luderick")

### Standardise Winds
my.df <-  mydata %>% group_by(Species, Estuary) %>% mutate(X45_degree_winds.standardised = as.numeric(scale(X45_degree_winds)),
                                                           X135_degree_winds.standardised = as.numeric(scale(X135_degree_winds)))


library(brms)

my.df2 <- my.df %>% drop_na(X135_degree_winds.standardised, X45_degree_winds.standardised)

m7a <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type *Drought_Months + Species + (Species|Estuary),
           data = my.df2, iter = 10000, seed = 1234)
saveRDS(m7a, "../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE no lag.rds")
m7a <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE no lag.rds")



### Now lag 1 year longer
my.df2 <- mydata %>% drop_na(X135_Longer_Lag_1, X45_Longer_Lag_1) %>%  group_by(Species, Estuary) %>% 
  mutate(X45_degree_winds.standardised = as.numeric(scale(X45_Longer_Lag_1)),
                                                           X135_degree_winds.standardised = as.numeric(scale(X135_Longer_Lag_1)))

m7_1L <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
             poly(X45_degree_winds.standardised, degree = 2) +
             X135_degree_winds.standardised:X45_degree_winds.standardised+
             Estuary_Type *Drought_Months + Species + (Species|Estuary),
           data = my.df2, iter = 10000, seed = 1234)
saveRDS(m7_1L, "../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 1 longer.rds")
m7_1L <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 1 longer.rds")



### Now lag 2 year longer
my.df2 <- my.df %>% drop_na(X135_Longer_Lag_2, X45_Longer_Lag_2) %>%  group_by(Species, Estuary) %>% 
  mutate(X45_degree_winds.standardised = as.numeric(scale(X45_Longer_Lag_2)),
         X135_degree_winds.standardised = as.numeric(scale(X135_Longer_Lag_2)))


m7_2L <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
               poly(X45_degree_winds.standardised, degree = 2) +
               X135_degree_winds.standardised:X45_degree_winds.standardised+
               Estuary_Type *Drought_Months + Species + (Species|Estuary),
             data = my.df2, iter = 10000, seed = 1234)
saveRDS(m7_2L, "../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 2 longer.rds")
m7_2L <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 2 longer.rds")


### Now lag 1 year shorter
my.df2 <- my.df %>% drop_na(X135_Shorter_Lag_1, X45_Shorter_Lag_1) %>%  group_by(Species, Estuary) %>% 
  mutate(X45_degree_winds.standardised = as.numeric(scale(X45_Shorter_Lag_1)),
         X135_degree_winds.standardised = as.numeric(scale(X135_Shorter_Lag_1)))


m7_1S <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
               poly(X45_degree_winds.standardised, degree = 2) +
               X135_degree_winds.standardised:X45_degree_winds.standardised+
               Estuary_Type *Drought_Months + Species + (Species|Estuary),
             data = my.df2, iter = 10000, seed = 1234)
saveRDS(m7_1S, "../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 1 shorter.rds")
m7_1S <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 1 shorter.rds")


### Now lag 2 year shorter
my.df2 <- my.df %>% drop_na(X135_Shorter_Lag_1, X45_Shorter_Lag_1) %>%  group_by(Species, Estuary) %>% 
  mutate(X45_degree_winds.standardised = as.numeric(scale(X45_Shorter_Lag_2)),
         X135_degree_winds.standardised = as.numeric(scale(X135_Shorter_Lag_2)))


m7_2S <- brm(CPUE ~ poly(X135_degree_winds.standardised, degree = 2) + 
               poly(X45_degree_winds.standardised, degree = 2) +
               X135_degree_winds.standardised:X45_degree_winds.standardised+
               Estuary_Type *Drought_Months + Species + (Species|Estuary),
             data = my.df2, iter = 10000, seed = 1234)
saveRDS(m7_2S, "../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 2 shorter.rds")
m7_2S <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 2 shorter.rds")

### Now for the results
m7_2L <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 2 longer.rds")

m7_1S <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 1 shorter.rds")

m7_2S <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 2 shorter.rds")
m7a <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE no lag.rds")
m7_1L <- readRDS("../Data/Fisheries Data/Lag Models/CPUE Multi_species brms model CPUE 1 longer.rds")


x <- broom.mixed::tidyMCMC(m7_2L)
x$Lag <- 2
write_csv(x, paste0("../Data/Fisheries Data/Lag Models/2L.csv"))

x <- broom.mixed::tidyMCMC(m7_1L)
x$Lag <- 1
write_csv(x, paste0("../Data/Fisheries Data/Lag Models/1L.csv"))
x <- broom.mixed::tidyMCMC(m7_1S)
x$Lag <- -1
write_csv(x, paste0("../Data/Fisheries Data/Lag Models/1S.csv"))
x <- broom.mixed::tidyMCMC(m7_2S)
x$Lag <- -2
write_csv(x, paste0("../Data/Fisheries Data/Lag Models/2S.csv"))
x <- broom.mixed::tidyMCMC(m7a)
x$Lag <- 0
write_csv(x, paste0("../Data/Fisheries Data/Lag Models/No_Lag.csv"))



file_list <- list.files("../Data/Fisheries Data/Lag Models/", pattern = ".csv", full.names = TRUE)
#file_list2 <- list.files("../Data/Fisheries Data/Lag Models/", full.names = F)
#file_list2 <- str_remove(file_list2, ".csv")

full_dat <- data.frame()

for (i in 1:length(file_list)){
  mydata <- read_csv(file_list[i])
  mydata <- mydata[1:14,]
  #mydata$Days_Prior <- as.numeric(file_list2[i])
  full_dat <- bind_rows(full_dat, mydata)
}

head(full_dat)

full_dat$Colours <- "black"
full_dat$Colours[full_dat$Lag == 0] <- "red"

mycolours <- c("red" = "red", "black" = "black")

unique(full_dat$term)

# recode (dplyr)

full_dat$term <- recode(full_dat$term, "b_Intercept" = "Intercept",
                        "b_polyX45_degree_winds.standardiseddegreeEQ21" = "Up Winds",             
                        "b_polyX45_degree_winds.standardiseddegreeEQ22" = "Up Winds (quadratic)",
                        "b_Drought_Months" = "Drought (Months)",
                        "b_polyX135_degree_winds.standardiseddegreeEQ21" = "Down Winds",
                        "b_polyX135_degree_winds.standardiseddegreeEQ22" = "Down Winds (Quadratic)",
                        "b_Estuary_TypeBarrierRiver" = "Estuary Type \nBarrier River",
                        "b_Estuary_TypeDrownedRiverValley" = "Estuary Type \nDrowned River Valley",
                        "b_SpeciesFlathead" = "Species: Flathead",
                        "b_SpeciesMullet"= "Species: Mullet",
                        "b_SpeciesWhiting"= "Species: Whiting",
                        "b_X135_degree_winds.standardised:X45_degree_winds.standardised" = "Up Winds * Down Winds",
                        "b_Estuary_TypeBarrierRiver:Drought_Months" = "Drought (Months) *\n Estuary Type\nBarrier River",
                        "b_Estuary_TypeDrownedRiverValley:Drought_Months" = "Drought (Months) *\n Estuary Type\nDrowned River Valley"                        )

p2 <- ggplot(full_dat, aes(x = Lag, y = estimate, col = Colours)) + geom_point() +
  facet_wrap(~term, scales = "free_y") +
  geom_errorbar(aes(ymin=estimate - std.error, ymax=estimate+std.error)) +
  scale_colour_manual(values = mycolours, guide = FALSE) + theme_classic() +
  ylab("Estimate (± SE)") + xlab("Lag Change") +
  theme(axis.title = element_text(face="bold", size = 14, colour = "black"),
        axis.text = element_text(colour = "black", size = 12),
        strip.text = element_text(face = "bold", size = 10))
p2  

ggsave("../plots/CPUE Sensitivity.png", dpi = 600, width = 24, height = 24, units = "cm")

