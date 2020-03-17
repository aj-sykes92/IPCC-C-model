
library(ncdf4)
library(raster)
library(tidyverse)
library(lubridate)

data_repo <- "GIS data repository"

# read netCDF and check out variables
Ncdf_tasAnom <- nc_open(find_onedrive(dir = data_repo, path = "UKCP/tasAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc"))
Ncdf_tasAnom

# extract names / numbers of regions for later use
regions <- tibble(region = ncvar_get(Ncdf_tasAnom, "region"),
                  region_chr = ncvar_get(Ncdf_tasAnom, "geo_region") %>% str_replace_all("\\s+", ""))

# read in netCDF as brick, convert to dataframe, join regions by name and reshape
# this is time-consuming!
Dat_tasAnom <- brick(find_onedrive(dir = data_repo, path = "UKCP/tasAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc")) %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(sample = x, region = y) %>%
  gather(-sample, -region, key = "date", value = "tasAnom") %>%
  mutate(date = date %>%
           str_replace("X", "") %>%
           ymd_hms()) %>%
  left_join(regions, by = "region") %>%
  select(-region)

Dat_prAnom <- brick(find_onedrive(dir = data_repo, path = "UKCP/prAnom_rcp45_land-prob_uk_country_sample_b8100_1y_mon_19601201-20991130.nc")) %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  rename(sample = x, region = y) %>%
  gather(-sample, -region, key = "date", value = "prAnom") %>%
  mutate(date = date %>%
           str_replace("X", "") %>%
           ymd_hms()) %>%
  left_join(regions, by = "region") %>%
  select(-region)

# write out full data in this format
write_rds(Dat_tasAnom, find_onedrive(dir = data_repo, path = "UKCP/tasAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))
write_rds(Dat_prAnom, find_onedrive(dir = data_repo, path = "UKCP/prAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))

# use this to read in if working interactively/skipping above computation
# Dat_tasAnom <- read_rds(find_onedrive(dir = data_repo, path = "UKCP/tasAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))
# Dat_prAnom <- read_rds(find_onedrive(dir = data_repo, path = "UKCP/prAnom-b8100-1960-2099-regional-3000sample-reshaped.rds"))


# select random set of 100 samples to give us something manageable to work with
# random samples from 1:3000, no replacement
set.seed(2605)
samples <- tibble(x = 1:3000) %>%
  sample_n(100, replace = F) %>%
  arrange(x) %>%
  pull(x)

# filter for samples in random selection
# keeps samples consistent between dates
Dat_tasAnom <- Dat_tasAnom %>%
  filter(sample %in% samples) %>%
  arrange(sample) %>%
  arrange(date)

Dat_prAnom <- Dat_prAnom %>%
  filter(sample %in% samples) %>%
  arrange(sample) %>%
  arrange(date)

# check out work so far
glimpse(Dat_tasAnom)
glimpse(Dat_prAnom)

# for simplicity's sake we're just going to use the UK climate anomalies
Dat_tasAnom <- Dat_tasAnom %>% filter(region_chr == "UnitedKingdom")
Dat_prAnom <- Dat_prAnom %>% filter(region_chr == "UnitedKingdom")

# read in historical climate data

# masking shapefile
Shp_UK <- find_onedrive(dir = data_repo, path = "DA shapefile/GBR_adm_shp/GBR_adm1.shp") %>% shapefile()
# temp <- shapefile(find_onedrive(dir = data_repo, path = "Country shapefile/countries.shp")) %>% subset(ISO3 == "GBR") # simpler option

#####################################################
# process crop production area and yield data
#####################################################
Ras_wheatarea <- raster(find_onedrive(dir = data_repo, path = "MapSpam data/Physical area/phys_area_wheat.tif"))

# crop to UK extent (rough until , for processing effiency)
Ras_wheatarea <- Ras_wheatarea %>% crop(Shp_UK)

#####################################################
# process spatial data for monthly climate variables
# opening full date range (1901-2018)
#####################################################

# precipitation, mm per month
Brk_precip <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/pre/cru_ts4.03.1901.2018.pre.dat.nc"), var = "pre")

# monthly average temperature, degrees Celsius
Brk_temp <- brick(find_onedrive(dir = data_repo, path = "CRU TS v4-03/tmp/cru_ts4.03.1901.2018.tmp.dat.nc"), var = "tmp")

# crop to UK
Brk_precip <- Brk_precip %>% crop(Shp_UK)
Brk_temp <- Brk_temp %>% crop(Shp_UK)

# resample to mapspam
Brk_precip <- Brk_precip %>% resample(Ras_wheatarea)
Brk_temp <- Brk_temp %>% resample(Ras_wheatarea)

# mask all the bricks
Brk_precip <- Brk_precip %>% mask(Shp_UK)
Brk_temp <- Brk_temp %>% mask(Shp_UK)

# rename clim variable bricks so we can tell them apart in the same dataset
names(Brk_precip) <- names(Brk_precip) %>% paste0("_Precip")
names(Brk_temp) <- names(Brk_temp) %>% str_replace("X", "") %>% paste0("_Temp")

# convert all monthly climate bricks to data frame
# checked to ensure rows match, much less computationally intensive than joining by x & y
Dat_clim <- bind_cols(Brk_precip %>% as.data.frame(xy = T),
                      Brk_temp %>% as.data.frame()) %>%
  drop_na()

# gather all vars and spread by climate variable type
Dat_clim <- Dat_clim %>%
  gather(-x, -y, key = "key", value = "value") %>%
  mutate(var = key %>% str_extract("(?<=_).+"),
         Date = key %>% str_extract("\\d{4}\\.\\d{2}\\.\\d{2}") %>% ymd()) %>%
  select(-key) %>%
  spread(key = var, value = value)

glimpse(Dat_clim)

# get dates floored so they're joinable
Dat_tasAnom <- Dat_tasAnom %>%
  mutate(date = date %>%
           floor_date(unit = "months") %>%
           as_date())

Dat_prAnom <- Dat_prAnom %>%
  mutate(date = date %>%
           floor_date(unit = "months") %>%
           as_date())

glimpse(Dat_prAnom)

# complete years?
Dat_prAnom %>%
  distinct(date) %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(n = n()) %>%
  View()



PET <- function(mod){
  t <- c(1, 2, 3, 4, 4, 5, 6, 7, 7, 6, 3, 2) * mod
  
  I <- sum((t / 5) ^ 1.514)
  alpha <- 675*10^-9 * I^3 - 771*10^-7 * I^2 + 1792*10^-5 * I + 0.49239
  
  PET <- 16 * ((10 * t) / I)^alpha
  return(PET[6] / 46.81)
}

mod <- seq(from = 0.5, to = 1.5, by = 0.01)
pet <- sapply(mod, PET)
qplot(mod, pet)
days_in_month()
PET(1)
