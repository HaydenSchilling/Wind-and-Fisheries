# Historical Wind data
# install.packages("remotes")
# install.packages("Rtools")
# remotes::install_github("skgrange/threadr")

library(threadr)

# Using this model https://www.esrl.noaa.gov/psd/data/gridded/data.20thC_ReanV2c.html
# gaussian model boxes version - See details on how to cite and acknowledge


# A test URL
#URL2 <- "ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV2c/gaussian/monolevel/uwnd.10m.1851.nc"

years <- seq(1851,2014,1)

URLS <- paste("ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV2c/gaussian/monolevel/uwnd.10m.",years, sep = "")
URLS <- paste(URLS, ".nc", sep = "")

head(URLS)

save_name_base <- "Wind Data/Wind_10m_u_"
save_names <- paste(save_name_base, years, sep = "")
save_names <- paste(save_names, ".nc", sep = "")

download_ftp_file(file_remote = URLS, file_output = save_names, verbose = TRUE)

# Now v wind

URLS <- paste("ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV2c/gaussian/monolevel/vwnd.10m.",years, sep = "")
URLS <- paste(URLS, ".nc", sep = "")

head(URLS)

save_name_base <- "Wind Data/Wind_10m_v_"
save_names <- paste(save_name_base, years, sep = "")
save_names <- paste(save_names, ".nc", sep = "")


download_ftp_file(file_remote = URLS, file_output = save_names, verbose = TRUE)

