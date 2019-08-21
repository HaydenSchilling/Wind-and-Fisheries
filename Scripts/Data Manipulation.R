# Merge wind data files and remove empty columns: Hayden Schilling 8/5/2019

library(data.table)
library(dplyr)

# List Files
file_names <- list.files("Data/Raw/", pattern=".txt")
file_names

# Empty output table generation
combined_data <- list()

# Loop through and load each file into a list
for(i in 1:length(file_names)){
  mydat <- read.delim(paste("Data/Raw/",
                         file_names[i], sep = ""), sep = ",")
  combined_data[[i]] <- mydat
}

# Combine the list into a single dataframe
combined_data <- rbindlist(combined_data)

# Drop some columns
combined_data2 <-  combined_data[,-c(1,19,20)]

# Add location names
location_numbers <- read.csv("Data/Raw/Station_number_locations.csv")

combined_data3 <- left_join(combined_data2, location_numbers, by = "Station.Number")

table(combined_data2$Station.Number)
table(combined_data3$Location)

# Write the final csv file
fwrite(combined_data3, "Data/Wind_Combined_Data.csv")

paste("THIS SCRIPT IS FINISHED")