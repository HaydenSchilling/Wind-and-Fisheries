# Linar Modelling to test individual level estuary effects
#install.packages("broom")
library(broom)
library(dplyr)


mydata <- read.csv("Model Data/Illawarra/Bream.csv", header = T)
Overview_table <- list()
Model_names <- as.character()

# Bream

Bream_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Bream_fit1)
#plot(fit1)
Overview_table[[1]] <- glance(Bream_fit1)
Model_names[1] <- "Bream Lagged Winds"

Bream_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Bream_fit2)
#plot(fit2)
Overview_table[[2]] <- glance(Bream_fit2)
Model_names[2] <- "Bream Lagged Winds and Current Rain"

Bream_fit3 <- lm(CPUE ~ Lagged_Wind + Lagged_Rain,data = mydata)
summary(Bream_fit3)
#plot(fit3)
Overview_table[[3]] <- glance(Bream_fit3)
Model_names[3] <- "Bream Lagged Winds and Lagged Rain"

Bream_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Bream_fit4)
#plot(fit4)
Overview_table[[4]] <- glance(Bream_fit4)
Model_names[4] <- "Bream Lagged Winds and Current Wind"



### Blackfish

mydata <- read.csv("Model Data/Illawarra/Blackfish.csv", header = T)

Blackfish_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Blackfish_fit1)
#plot(fit1)
Overview_table[[5]] <- glance(Blackfish_fit1)
Model_names[5] <- "Blackfish Lagged Winds"

Blackfish_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Blackfish_fit2)
#plot(fit2)
Overview_table[[6]] <- glance(Blackfish_fit2)
Model_names[6] <- "Blackfish Lagged Winds and Current Rain"

Blackfish_fit3 <- lm(CPUE ~ Lagged_Wind + Lagged_Rain,data = mydata)
summary(Blackfish_fit3)
#plot(fit3)
Overview_table[[7]] <- glance(Blackfish_fit3)
Model_names[7] <- "Blackfish Lagged Winds and Lagged Rain"

Blackfish_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Blackfish_fit4)
#plot(fit4)
Overview_table[[8]] <- glance(Blackfish_fit4)
Model_names[8] <- "Blackfish Lagged Winds and Current Wind"


### Whiting

mydata <- read.csv("Model Data/Illawarra/Whiting.csv", header = T)

Whiting_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Whiting_fit1)
#plot(fit1)
Overview_table[[9]] <- glance(Whiting_fit1)
Model_names[9] <- "Whiting Lagged Winds"

Whiting_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Whiting_fit2)
#plot(fit2)
Overview_table[[10]] <- glance(Whiting_fit2)
Model_names[10] <- "Whiting Lagged Winds and Current Rain"

Whiting_fit3 <- lm(CPUE ~ Lagged_Wind + Lagged_Rain,data = mydata)
summary(Whiting_fit3)
#plot(fit3)
Overview_table[[11]] <- glance(Whiting_fit3)
Model_names[11] <- "Whiting Lagged Winds and Lagged Rain"

Whiting_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Whiting_fit4)
#plot(fit4)
Overview_table[[12]] <- glance(Whiting_fit4)
Model_names[12] <- "Whiting Lagged Winds and Current Wind"


### Flathead

mydata <- read.csv("Model Data/Illawarra/Flathead.csv", header = T)

Flathead_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Flathead_fit1)
#plot(fit1)
Overview_table[[13]] <- glance(Flathead_fit1)
Model_names[13] <- "Flathead Lagged Winds"

Flathead_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Flathead_fit2)
#plot(fit2)
Overview_table[[14]] <- glance(Flathead_fit2)
Model_names[14] <- "Flathead Lagged Winds and Current Rain"

Flathead_fit3 <- lm(CPUE ~ Lagged_Wind + Lagged_Rain,data = mydata) # lagged rain does improve adj r^2 by 0.03
summary(Flathead_fit3)
#plot(fit3)
Overview_table[[15]] <- glance(Flathead_fit3)
Model_names[15] <- "Flathead Lagged Winds and Lagged Rain"

Flathead_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata) #
summary(Flathead_fit4)
#plot(fit4)
Overview_table[[16]] <- glance(Flathead_fit4)
Model_names[16] <- "Flathead Lagged Winds and Current Wind"


### Mullet

mydata <- read.csv("Model Data/Illawarra/Mullet.csv", header = T)

Mullet_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Mullet_fit1)
#plot(fit1)
Overview_table[[17]] <- glance(Mullet_fit1)
Model_names[17] <- "Mullet Lagged Winds"

Mullet_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Mullet_fit2)
#plot(fit2)
Overview_table[[18]] <- glance(Mullet_fit2)
Model_names[18] <- "Mullet Lagged Winds and Current Rain"

Mullet_fit3 <- lm(CPUE ~ Lagged_Wind + Lagged_Rain,data = mydata) # lagged rain does improve adj r^2 by 0.03
summary(Mullet_fit3)
#plot(fit3)
Overview_table[[19]] <- glance(Mullet_fit3)
Model_names[19] <- "Mullet Lagged Winds and Lagged Rain"

Mullet_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata) 
summary(Mullet_fit4)
#plot(fit4)
Overview_table[[20]] <- glance(Mullet_fit4)
Model_names[20] <- "Mullet Lagged Winds and Current Wind"


# Trying to make a summary table
Overview_table <- rbind_list(Overview_table)
Overview_table$Model <- Model_names

#Reorder Columns
Overview_table <- Overview_table %>%  select(Model, everything())

write.csv(Overview_table, "Illawarra Modelling.csv", row.names = FALSE)


#### Tuggerah
Overview_table <- list()
Model_names <- as.character()


### Blackfish

mydata <- read.csv("Model Data/Tuggerah/Blackfish.csv", header = T)

Blackfish_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Blackfish_fit1)
#plot(fit1)
Overview_table[[1]] <- glance(Blackfish_fit1)
Model_names[1] <- "Blackfish Lagged Winds"

Blackfish_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Blackfish_fit2)
#plot(fit2)
Overview_table[[2]] <- glance(Blackfish_fit2)
Model_names[2] <- "Blackfish Lagged Winds and Current Rain"

Blackfish_fit3 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Blackfish_fit3)
#plot(fit4)
Overview_table[[3]] <- glance(Blackfish_fit3)
Model_names[3] <- "Blackfish Lagged Winds and Current Wind"

Blackfish_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind + Current_Rain ,data = mydata)
summary(Blackfish_fit4)
#plot(Blackfish_fit4)
Overview_table[[4]] <- glance(Blackfish_fit4)
Model_names[4] <- "Blackfish Lagged Winds and Current Wind and Current Rain"


### Bream

mydata <- read.csv("Model Data/Tuggerah/Bream.csv", header = T)

Bream_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Bream_fit1)
#plot(fit1)
Overview_table[[5]] <- glance(Bream_fit1)
Model_names[5] <- "Bream Lagged Winds"

Bream_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Bream_fit2)
#plot(fit2)
Overview_table[[6]] <- glance(Bream_fit2)
Model_names[6] <- "Bream Lagged Winds and Current Rain"

Bream_fit3 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Bream_fit3)
#plot(fit4)
Overview_table[[7]] <- glance(Bream_fit3)
Model_names[7] <- "Bream Lagged Winds and Current Wind"

Bream_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind + Current_Rain ,data = mydata)
summary(Bream_fit4)
#plot(Bream_fit4)
Overview_table[[8]] <- glance(Bream_fit4)
Model_names[8] <- "Bream Lagged Winds and Current Wind and Current Rain"


### Flathead

mydata <- read.csv("Model Data/Tuggerah/Flathead.csv", header = T)

Flathead_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Flathead_fit1)
#plot(fit1)
Overview_table[[9]] <- glance(Flathead_fit1)
Model_names[9] <- "Flathead Lagged Winds"

Flathead_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Flathead_fit2)
#plot(fit2)
Overview_table[[10]] <- glance(Flathead_fit2)
Model_names[10] <- "Flathead Lagged Winds and Current Rain"

Flathead_fit3 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Flathead_fit3)
#plot(fit4)
Overview_table[[11]] <- glance(Flathead_fit3)
Model_names[11] <- "Flathead Lagged Winds and Current Wind"

Flathead_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind + Current_Rain ,data = mydata)
summary(Flathead_fit4)
#plot(Flathead_fit4)
Overview_table[[12]] <- glance(Flathead_fit4)
Model_names[12] <- "Flathead Lagged Winds and Current Wind and Current Rain"


### Mullet

mydata <- read.csv("Model Data/Tuggerah/Mullet.csv", header = T)

Mullet_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Mullet_fit1)
#plot(fit1)
Overview_table[[13]] <- glance(Mullet_fit1)
Model_names[13] <- "Mullet Lagged Winds"

Mullet_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Mullet_fit2)
#plot(fit2)
Overview_table[[14]] <- glance(Mullet_fit2)
Model_names[14] <- "Mullet Lagged Winds and Current Rain"

Mullet_fit3 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Mullet_fit3)
#plot(fit4)
Overview_table[[15]] <- glance(Mullet_fit3)
Model_names[15] <- "Mullet Lagged Winds and Current Wind"

Mullet_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind + Current_Rain ,data = mydata)
summary(Mullet_fit4)
#plot(Mullet_fit4)
Overview_table[[16]] <- glance(Mullet_fit4)
Model_names[16] <- "Mullet Lagged Winds and Current Wind and Current Rain"



### Whiting

mydata <- read.csv("Model Data/Tuggerah/Whiting.csv", header = T)

Whiting_fit1 <- lm(CPUE ~ Lagged_Wind ,data = mydata)
summary(Whiting_fit1)
#plot(fit1)
Overview_table[[17]] <- glance(Whiting_fit1)
Model_names[17] <- "Whiting Lagged Winds"

Whiting_fit2 <- lm(CPUE ~ Lagged_Wind + Current_Rain,data = mydata)
summary(Whiting_fit2)
#plot(fit2)
Overview_table[[18]] <- glance(Whiting_fit2)
Model_names[18] <- "Whiting Lagged Winds and Current Rain"

Whiting_fit3 <- lm(CPUE ~ Lagged_Wind + Current_Wind,data = mydata)
summary(Whiting_fit3)
#plot(fit4)
Overview_table[[19]] <- glance(Whiting_fit3)
Model_names[19] <- "Whiting Lagged Winds and Current Wind"

Whiting_fit4 <- lm(CPUE ~ Lagged_Wind + Current_Wind + Current_Rain ,data = mydata)
summary(Whiting_fit4)
#plot(Whiting_fit4)
Overview_table[[20]] <- glance(Whiting_fit4)
Model_names[20] <- "Whiting Lagged Winds and Current Wind and Current Rain"


# Trying to make a summary table
Overview_table <- rbind_list(Overview_table)
Overview_table$Model <- Model_names

#Reorder Columns
Overview_table <- Overview_table %>%  select(Model, everything())

write.csv(Overview_table, "Tuggerah Modelling.csv", row.names = FALSE)
