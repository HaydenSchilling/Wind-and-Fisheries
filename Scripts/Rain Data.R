# Get Get Rain data - No manipulation
#install.packages("devtools")
#library(devtools)

#install_github("ropensci/bomrang", build_vignettes = TRUE)
library(bomrang)


# Woonona (Popes Rd) rain - Illawarra
Illawarra_Rain <- get_historical(stationid = "68108", type = "rain")
write.csv(Illawarra_Rain, "Illawarra Rain.csv", row.names = F)

# Norah Head rain - Tuggorah
Tug_Rain <- get_historical(stationid = "61366", type = "rain")
write.csv(Tug_Rain, "Tuggerah Rain.csv", row.names = F)



