# Summarising the Wind data into monthly Mean direction and mean speed
# Coastline has a bearing of 24 deg.
# Therefore if bearing is > 24 and less than 204 deg it is onshore, any other is offshore.

library(dplyr)
#library(ggplot2)
#install.packages("REdaS")
library(REdaS)
library(tidyr)
library(data.table)

mydata <- read.csv("Data/Wind_Combined_Data.csv", header = T)
mydata$Wind.speed.in.km.h <- mydata$Wind.speed.in.m.s * 3.6
table(is.na(mydata$Wind.speed.in.km.h), mydata$Location)

table(mydata$Location, mydata$Year.Month.Day.Hour.Minutes.in.YYYY.1)
#head(mydata)

# Clarence 11 deg - Yamba
# Hunter 55 deg - Newcastle
# Hawkesbury 24 deg - Norah Head
# Camden Haven 24 deg -Port Macquarie
# Wallis 24 deg - Newcastle
# Port Stephens 45 deg - Newcastle
# Tuggerah 24 deg - Norah Head
# Lake Illawarra 24 deg - Bellambi
# St Georges Basin 24 deg - Bellambi

# Yamba = 1
# Newcastle = 3 different angles
# Port Macquarie = 1
# Norah Head = 2 same angles
# Bellambi = 2 same angles



# Subset to only stations interested in 
mydata <- subset(mydata, Location == "Bellambi" | Location == "Yamba" | Location == "Norah Head" | Location == "Port Macquarie" |
                   Location == "Newcastle" | Location == "Sydney Airport")

mydata$Location <- droplevels(mydata$Location)
table(mydata$Location)

head(mydata)

# Make dataset for each estuary
mydata2 <- subset(mydata, Location == "Bellambi")
mydata2$Estuary <- "Lake Illawarra"
fwrite(mydata2, "Data/Estuary_winds/Lake_Illawarra_winds_raw.csv")
mydata2$Estuary <- "St Georges Basin"
fwrite(mydata2, "Data/Estuary_winds/St_Georges_Basin_winds_raw.csv")

mydata2 <- subset(mydata, Location == "Yamba")
mydata2$Estuary <- "Clarence River"
fwrite(mydata2, "Data/Estuary_winds/Clarence_River_winds_raw.csv")

mydata2 <- subset(mydata, Location == "Sydney Airport")
mydata2$Estuary <- "Sydney"
fwrite(mydata2, "Data/Estuary_winds/Sydney_Airport_winds_raw.csv")

mydata2 <- subset(mydata, Location == "Port Macquarie")
mydata2$Estuary <- "Camden Haven River"
fwrite(mydata2, "Data/Estuary_winds/Camden_Haven_River_winds_raw.csv")

mydata2 <- subset(mydata, Location == "Norah Head")
mydata2$Estuary <- "Tuggerah Lake"
fwrite(mydata2, "Data/Estuary_winds/Tuggerah_Lake_winds_raw.csv")
mydata2$Estuary <- "Hawkesbury River"
fwrite(mydata2, "Data/Estuary_winds/Hawkesbury_River_winds_raw.csv")

mydata2 <- subset(mydata, Location == "Newcastle")
mydata2$Estuary <- "Wallis Lake"
fwrite(mydata2, "Data/Estuary_winds/Wallis_Lake_winds_raw.csv")
mydata2$Estuary <- "Hunter River"
fwrite(mydata2, "Data/Estuary_winds/Hunter_River_winds_raw.csv")
mydata2$Estuary <- "Port Stephens"
fwrite(mydata2, "Data/Estuary_winds/Port_Stephens_winds_raw.csv")

# List Files
file_names <- list.files("Data/Estuary_winds/", pattern=".csv")
file_names

# Empty output table generation
combined_data <- list()

# Loop through and load each file into a list
for(i in 1:length(file_names)){
  mydat <- read.csv(paste("Data/Estuary_winds/",
                            file_names[i], sep = ""),header = T)
  combined_data[[i]] <- mydat
}

# Combine the list into a single dataframe
combined_data <- rbindlist(combined_data)

fwrite(combined_data, "Data/Estuaries_Combined_Data.csv")

table(combined_data$Estuary)

# Can start processing for here if wanted
mydata <- read.csv("Data/Estuaries_Combined_Data.csv", header = T) 
table(mydata$Estuary)

# Now detirimine onshore/offshore
# First set all to offshore
mydata$Onshore_Offshore <- "Offshore"

# Clarence 11 deg - Yamba
# Hunter 55 deg - Newcastle
# Hawkesbury 24 deg - Norah Head
# Camden Haven 24 deg -Port Macquarie
# Wallis 24 deg - Newcastle
# Port Stephens 45 deg - Newcastle
# Tuggerah 24 deg - Norah Head
# Lake Illawarra 24 deg - Bellambi
# St Georges Basin 24 deg - Bellambi

mydata$Onshore_Offshore[mydata$Estuary =="Hawkesbury River" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Camden Haven River" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Wallis Lake" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Tuggerah Lake" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Lake Illawarra" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="St Georges Basin" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Clarence River" & mydata$Wind.direction.in.degrees.true > 11 & mydata$Wind.direction.in.degrees.true < 191] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Hunter River" & mydata$Wind.direction.in.degrees.true > 55 & mydata$Wind.direction.in.degrees.true < 235] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Port Stephens" & mydata$Wind.direction.in.degrees.true > 45 & mydata$Wind.direction.in.degrees.true < 225] <- "Onshore"


# is Yamba correlated to Port Macquarie and Norah Head
mydata$Onshore_Offshore[mydata$Estuary =="Hawkesbury River" & mydata$Wind.direction.in.degrees.true > 11 & mydata$Wind.direction.in.degrees.true < 191] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Camden Haven River" & mydata$Wind.direction.in.degrees.true > 11 & mydata$Wind.direction.in.degrees.true < 191] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Wallis Lake" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Tuggerah Lake" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Lake Illawarra" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="St Georges Basin" & mydata$Wind.direction.in.degrees.true > 24 & mydata$Wind.direction.in.degrees.true < 204] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Clarence River" & mydata$Wind.direction.in.degrees.true > 11 & mydata$Wind.direction.in.degrees.true < 191] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Hunter River" & mydata$Wind.direction.in.degrees.true > 55 & mydata$Wind.direction.in.degrees.true < 235] <- "Onshore"
mydata$Onshore_Offshore[mydata$Estuary =="Port Stephens" & mydata$Wind.direction.in.degrees.true > 45 & mydata$Wind.direction.in.degrees.true < 225] <- "Onshore"



table(mydata$Onshore_Offshore)

sub_dat <- subset(mydata, Estuary == "Hawkesbury River" | Estuary == "Camden Haven River" | Estuary == "Clarence River")


for (i in 1:nrow(sub_dat)) {
  if (sub_dat$Onshore_Offshore[i] == "Offshore") {
    sub_dat$Wind.speed.in.km.h[i] = sub_dat$Wind.speed.in.km.h[i] * -1
  }
}

sub_dat$Wind.direction.in.radians.adjusted <- deg2rad(sub_dat$Wind.direction.in.degrees.true+24)

sub_dat$Wind.effect.size = sin(sub_dat$Wind.direction.in.radians.adjusted)
sub_dat$Wind.speed.adjusted = sub_dat$Wind.effect.size * sub_dat$Wind.speed.in.km.h

dat <- sub_dat %>% group_by(Estuary,Year.Month.Day.Hour.Minutes.in.YYYY.1, MM.1) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)/2), count = n())
head(dat)

camden <- subset(dat, Estuary == "Camden Haven River")
fwrite(camden, "Data/camden river.csv")
hawkes <- subset(dat, Estuary == "Hawkesbury River")
fwrite(hawkes, "Data/hawkesbury river.csv")
Clarence <- subset(dat, Estuary == "Clarence River")
fwrite(Clarence, "Data/clarence river.csv")











### Old


deg2rad(90)

table(mydata$Location, mydata$Year.Month.Day.Hour.Minutes.in.YYYY.1)

#table(mydata$Wind.direction.in.degrees.true)


#mydata <- subset(mydata, Location =="Bellambi AWS")
mydata <- subset(mydata, Location =="Norah Head AWS")


for (i in 1:nrow(mydata)) {
if (mydata$Onshore_Offshore[i] == "Offshore") {
  mydata$Wind.speed.in.km.h[i] = mydata$Wind.speed.in.km.h[i] * -1
}
}

mydata$Wind.direction.in.radians.adjusted <- deg2rad(mydata$Wind.direction.in.degrees.true+24)

mydata$Wind.effect.size = sin(mydata$Wind.direction.in.radians.adjusted)
mydata$Wind.speed.adjusted = mydata$Wind.effect.size * mydata$Wind.speed.in.km.h

dat <- mydata %>% group_by(Year.Month.Day.Hour.Minutes.in.YYYY.1, MM.1) %>%
  summarise(displacement = (sum(Wind.speed.adjusted, na.rm = TRUE)/2), count = n())
head(dat)

library(ggplot2)
p1 <- ggplot(dat, aes(x = MM.1, y = displacement)) + geom_line() +
  facet_wrap(~Year.Month.Day.Hour.Minutes.in.YYYY.1) + xlab("Month") + ylab("Onshore/Offshore Displacement")
p1

write.csv(dat, "Norah Head wind monthly2.csv", row.names = FALSE)

summary(dat)



