# Historical Wind data from v3 using Katana
# install.packages("remotes")
# install.packages("Rtools")
#remotes::install_github("skgrange/threadr")

library(threadr)

# Using this model https://www.esrl.noaa.gov/psd/data/20thC_Rean/ 
# height above surface (12 m version)

# A test URL
URL2 <- "ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mMO/uwnd.10m.1836.nc"

years <- seq(2000,2015,1)

# URLS <- paste("ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mMO/uwnd.10m.",years, sep = "")
# URLS <- paste(URLS, ".nc", sep = "")
# 
# head(URLS)
# 
# save_name_base <- "../../srv/scratch/z3374139/Wind DataV3/Wind_10m_u_"
# save_names <- paste(save_name_base, years, sep = "")
# save_names <- paste(save_names, ".nc", sep = "")
# 
# download_ftp_file(file_remote = URLS, file_local = save_names, verbose = TRUE, curl = TRUE)

# Now v wind

URLS <- paste("ftp://ftp.cdc.noaa.gov/Datasets/20thC_ReanV3/10mMO/vwnd.10m.",years, sep = "")
URLS <- paste(URLS, ".nc", sep = "")

head(URLS)

save_name_base <- "../../srv/scratch/z3374139/Wind DataV3/Wind_10m_v_"
save_names <- paste(save_name_base, years, sep = "")
save_names <- paste(save_names, ".nc", sep = "")


download_ftp_file(file_remote = URLS, file_local = save_names, verbose = TRUE)

