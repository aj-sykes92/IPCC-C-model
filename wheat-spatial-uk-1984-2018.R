# script to implement IPCC steady-state C model for UK wheat production
# aim is to assess applicable soil carbon sequestration measures

#####################################################
# preparation
#####################################################
# load packages
library(ncdf4)
library(raster)
library(tidyverse)
library(readxl)
library(lubridate)

# read in parameters and functions
source("IPCC-C-model-main.R")

# data repositories
data_repo <- "GIS data repository"

# masking shapefile
Shp_UK <- find_onedrive(dir = data_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()
# temp <- shapefile(find_onedrive(dir = data_repo, path = "Country shapefile/countries.shp")) %>% subset(ISO3 == "GBR") # simpler option

#####################################################
# process crop production area and yield data
#####################################################
Ras_wheatarea <- raster(find_onedrive(dir = data_repo, path = "MapSpam data/Physical area/phys_area_wheat.tif"))
Ras_wheatyield <- raster(find_onedrive(dir = data_repo, path = "MapSpam data/Yield/yield_wheat.tif"))

# crop to UK extent (rough until , for processing effiency)
Ras_wheatarea <- Ras_wheatarea %>% crop(Shp_UK)
Ras_wheatyield <- Ras_wheatyield %>% crop(Shp_UK)

# time series of UK wheat area/yields from 
Dat_wheat_ts <- read_excel("AUK-Chapter7-09jul19.xlsx",
                           sheet = "Table_7_2",
                           range = "E9:AX13",
                           na = ". .",
                           col_types = "numeric") %>%
  slice(-(1:2)) %>%
  mutate(metric = c("Area_kha", "Yield_tha")) %>%
  gather(-metric, key = "Date", value = "value") %>%
  spread(key = metric, value = value) %>%
  drop_na() %>%
  mutate(Date = as.numeric(Date))

# normalise to 2010 (mapspam data year) to adjust raster data
Dat_wheat_ts <- Dat_wheat_ts %>%
  mutate(Area_kha = Area_kha / Area_kha[Date == 2010],
         Yield_tha = Yield_tha / Yield_tha[Date == 2010])

# loops to adjust mapspam data and create brick, by year
Brk_wheatyield <- brick()
for(i in 1:nrow(Dat_wheat_ts)){
  x <- Ras_wheatyield * Dat_wheat_ts$Yield_tha[i]
  x@data@names <- paste0("yield_wheat_", Dat_wheat_ts$Date[i])
  Brk_wheatyield <- Brk_wheatyield %>% addLayer(x)
  rm(x)
}

Brk_wheatarea <- brick()
for(i in 1:nrow(Dat_wheat_ts)){
  x <- Ras_wheatarea * Dat_wheat_ts$Area_kha[i]
  x@data@names <- paste0("area_wheat_", Dat_wheat_ts$Date[i])
  Brk_wheatarea <- Brk_wheatarea %>% addLayer(x)
  rm(x)
}
rm(i)
rm(Ras_wheatarea, Ras_wheatyield, Dat_wheat_ts)

#####################################################
# process spatial data for monthly climate variables
# opening full date range (1901-2018)
#####################################################

# precipitation, mm per month
nc_open(find_onedrive(dir = data_repo, path = "CRU TS v4-03/pre/cru_ts4.03.1901.2018.pre.dat.nc"))
Brk_precip <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/pre/cru_ts4.03.1901.2018.pre.dat.nc"), var = "pre")

# potential evapotranspiration, mm per day
nc_open(find_onedrive(dir = data_repo, path = "CRU TS v4-03/pet/cru_ts4.03.1901.2018.pet.dat.nc"))
Brk_pet <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/pet/cru_ts4.03.1901.2018.pet.dat.nc"), var = "pet")

# monthly average temperature, degrees Celsius
nc_open(find_onedrive(dir = data_repo, path = "CRU TS v4-03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc"))
Brk_temp <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc"), var = "tmp")

# crop to UK
Brk_precip <- Brk_precip %>% crop(Shp_UK)
Brk_pet <- Brk_pet %>% crop(Shp_UK)
Brk_temp <- Brk_temp %>% crop(Shp_UK)

# drop all layers pre-1984
post84 <- tibble(names = names(Brk_precip)) %>% # all three CRU-derived bricks have the same names
  mutate(dates = names %>%
           str_replace_all("X", "") %>%
           str_replace_all("\\.", "/") %>%
           ymd()) %>%
  filter(dates >= ymd("1984/01/01")) %>%
  pull(names)

Brk_precip <- subset(Brk_precip, post84)
Brk_pet <- subset(Brk_pet, post84)
Brk_temp <- subset(Brk_temp, post84)

# resample to mapspam
Brk_precip <- Brk_precip %>% resample(Brk_wheatarea)
Brk_pet <- Brk_pet %>% resample(Brk_wheatarea)
Brk_temp <- Brk_temp %>% resample(Brk_wheatarea)

# mask all the bricks
Brk_wheatyield <- Brk_wheatyield %>% mask(Shp_UK)
Brk_wheatarea <- Brk_wheatarea %>% mask(Shp_UK)
Brk_precip <- Brk_precip %>% mask(Shp_UK)
Brk_pet <- Brk_pet %>% mask(Shp_UK)
Brk_temp <- Brk_temp %>% mask(Shp_UK)

# figure out how the hell to turn into nested data frame
# purrr!

# what about soil sand %??


# crop variables
# N frac
# lignin frac

# annual variables
# C input

