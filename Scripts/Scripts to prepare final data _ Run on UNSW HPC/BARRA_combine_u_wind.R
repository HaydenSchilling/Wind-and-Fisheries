# Combine lots of csv files to get a single big file for u and v

# First U-Wind

library(tidyverse)
library(data.table)

file_list <- list.files("../../srv/scratch/z3374139/BARRA Model/av_uwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".csv")
#file_list <- list.files("../BOM Data/BARRA Model/av_uwnd10m/1990/01/", full.names = TRUE, recursive = TRUE, pattern =".csv")
head(file_list)


dat_list <- list()
for (i in 1:length(file_list)) {
  my_data <- read.csv(file_list[i], header = T)
  dat_list[[i]] <- my_data
}

full_data_U <- bind_rows(dat_list)

fwrite(full_data_U, "../../srv/scratch/z3374139/BARRA Model/Estuaries_u.csv")

remove(list = ls())

# Now V-Wind
library(tidyverse)

file_list <- list.files("../../srv/scratch/z3374139/BARRA Model/av_vwnd10m/",full.names = TRUE, recursive = TRUE, pattern = ".csv")
head(file_list)


dat_list <- list()
for (i in 1:length(file_list)) {
  my_data <- read.csv(file_list[i], header = T)
  dat_list[[i]] <- my_data
}

full_data_V <- bind_rows(dat_list)

fwrite(full_data_U, "../../srv/scratch/z3374139/BARRA Model/Estuaries_v.csv")

remove(list = ls())


