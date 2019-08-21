# Load from here
library(dplyr)
library(tidyr)
library(ggplot2)

Illawarra_Rain <- read.csv("Illawarra Rain.csv", header = T)
Illawarra_Rain$date <- as.Date(with(Illawarra_Rain, paste(year, month, day,sep="-")), "%Y-%m-%d")
Illawarra_Rain$Financial_Year <- Illawarra_Rain$year
for (i in 1:nrow(Illawarra_Rain)) {
  if (Illawarra_Rain$month[i] > 6) {
    Illawarra_Rain$Financial_Year[i] <- Illawarra_Rain$year[i] + 1
  }
}

Illawarra_Rain <- Illawarra_Rain %>% drop_na(rainfall)

#str(Illawarra_Rain)
#summary(Illawarra_Rain)

Illawarra_Rain_Monthly <- Illawarra_Rain %>% group_by(year, month) %>% 
  summarise(Monthly_Rain = (sum(rainfall, na.rm = TRUE)), count = n())  
head(Illawarra_Rain_Monthly)

write.csv(Illawarra_Rain_Monthly, "Illawarra Rain Monthly.csv", row.names = FALSE)

Illawarra_Rain_Fin_year <- Illawarra_Rain %>% group_by(Financial_Year) %>% 
  summarise(Annual_Rain = (sum(rainfall, na.rm = TRUE)), count = n())  
head(Illawarra_Rain_Fin_year)

write.csv(Illawarra_Rain_Fin_year, "Illawarra Rain Financial Year.csv", row.names = FALSE)


g <- ggplot(Illawarra_Rain_Fin_year, aes(x = Financial_Year, y =Annual_Rain)) + geom_point() +
  geom_smooth()
g

# Now Tuggerah
Tuggerah_Rain <- read.csv("Tuggerah Rain.csv", header = T)

Tuggerah_Rain$date <- as.Date(with(Tuggerah_Rain, paste(year, month, day,sep="-")), "%Y-%m-%d")
Tuggerah_Rain$Financial_Year <- Tuggerah_Rain$year
for (i in 1:nrow(Tuggerah_Rain)) {
  if (Tuggerah_Rain$month[i] > 6) {
    Tuggerah_Rain$Financial_Year[i] <-Tuggerah_Rain$year[i] + 1
  }
}


Tuggerah_Rain <- Tuggerah_Rain %>% drop_na(rainfall)


Tuggerah_Rain_Monthly <- Tuggerah_Rain %>% group_by(year, month) %>% 
  summarise(Monthly_Rain = (sum(rainfall, na.rm = TRUE)), count = n())  
head(Tuggerah_Rain_Monthly)

write.csv(Tuggerah_Rain_Monthly, "Tuggerah Rain Monthly.csv", row.names = FALSE)

Tuggerah_Rain_Fin_year <- Tuggerah_Rain %>% group_by(Financial_Year) %>% 
  summarise(Annual_Rain = (sum(rainfall, na.rm = TRUE)), count = n())  
head(Tuggerah_Rain_Fin_year)

write.csv(Tuggerah_Rain_Fin_year, "Tuggerah Rain Financial Year.csv", row.names = FALSE)


g <- ggplot(Tuggerah_Rain_Fin_year, aes(x = Financial_Year, y =Annual_Rain)) + geom_point() +
  geom_smooth()
g

