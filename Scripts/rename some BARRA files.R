#Rename some BARRA FILES in order to stop other scripts crashing when they encounter the wrong file name.

library(stringr)

# February 2016

files <- list.files("../BOM Data/BARRA Model/av_uwnd10m/2016/02/", full.names = TRUE)

# replace "v1.1" with "v1" 

files_new_names <- str_replace(files, pattern = "v1.1", replacement = "v1")

head(files)
head(files_new_names)

tail(files)
tail(files_new_names)

file.rename(from = files, to = files_new_names)


# v winds
files <- list.files("../BOM Data/BARRA Model/av_vwnd10m/2016/02/", full.names = TRUE)

# replace "v1.1" with "v1" vor v winds

files_new_names <- str_replace(files, pattern = "v1.1", replacement = "v1")

head(files)
head(files_new_names)

tail(files)
tail(files_new_names)

file.rename(from = files, to = files_new_names)


# July 2016
# u winds
files <- list.files("../BOM Data/BARRA Model/av_uwnd10m/2016/07/", full.names = TRUE)

# replace "v1.1" with "v1" 

files_new_names <- str_replace(files, pattern = "v1.1", replacement = "v1")

head(files)
head(files_new_names)

tail(files)
tail(files_new_names)

file.rename(from = files, to = files_new_names)

# v winds
files <- list.files("../BOM Data/BARRA Model/av_vwnd10m/2016/07/", full.names = TRUE)

# replace "v1.1" with "v1" 

files_new_names <- str_replace(files, pattern = "v1.1", replacement = "v1")

head(files)
head(files_new_names)

tail(files)
tail(files_new_names)

file.rename(from = files, to = files_new_names)
