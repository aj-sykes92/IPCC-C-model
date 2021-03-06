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

# read in soil sand %, crop & mask
Ras_sand <- find_onedrive(dir = data_repo, path = "SoilGrids 5km/Sand content/Fixed/SNDPPT_M_sl4_5km_ll.tif") %>%
  raster() %>%
  crop(Shp_UK) %>%
  resample(Brk_wheatarea) %>%
  mask(Shp_UK)

# all our data should be focused and compatible now
Brk_wheatyield
Brk_wheatarea
Brk_precip
Brk_pet
Brk_temp
Ras_sand

#####################################################
# starting with monthly variables, converting to df and condensing to annual modification factors (tfac and wfac)
#####################################################

# rename clim variable bricks so we can tell them apart in the same dataset
names(Brk_precip) <- names(Brk_precip) %>% paste0("_Precip")
names(Brk_pet) <- names(Brk_pet) %>% str_replace("X", "") %>% paste0("_PET")
names(Brk_temp) <- names(Brk_temp) %>% str_replace("X", "") %>% paste0("_Temp")

# convert all monthly climate bricks to data frame
# checked to ensure rows match, much less computationally intensive than joining by x & y
Dat_clim <- bind_cols(Brk_precip %>% as.data.frame(xy = T),
                      Brk_pet %>% as.data.frame(),
                      Brk_temp %>% as.data.frame()) %>%
  drop_na()

# gather all vars and spread by climate variable type
Dat_clim <- Dat_clim %>%
  gather(-x, -y, key = "key", value = "value") %>%
  mutate(var = key %>% str_extract("(?<=_).+"),
         Date = key %>% str_extract("\\d{4}\\.\\d{2}\\.\\d{2}") %>% ymd()) %>%
  select(-key) %>%
  spread(key = var, value = value)

# adjust PET to full month (not per day)
# calculate year from date
Dat_clim <- Dat_clim %>%
  mutate(Days_in_month = days_in_month(Date),
         PET = PET * Days_in_month,
         Year = year(Date))

# summarise using defined functions for wfac and tfac
Dat_clim <- Dat_clim %>%
  group_by(x, y, Year) %>%
  summarise(Wfac = wfac(precip = Precip, PET = PET),
            Tfac = tfac(temp = Temp)) %>%
  ungroup()

#####################################################
# convert crop data to df and calculate crop variables
#####################################################

# join up wheat data, convert to data frame, gather years and spread variables
Dat_wheat <- bind_cols(
  Brk_wheatarea %>% as.data.frame(xy = T), # as above, simpler and less computationally intensive. Checked and double-checked for mis-aligned rows
  Brk_wheatyield %>% as.data.frame()
  ) %>%
  as_tibble() %>%
  drop_na() %>%
  gather(-x, -y, key = "key", value = "value") %>%
  mutate(Year = key %>% str_extract("[:digit:]+") %>% as.numeric(),
         var = key %>% str_extract("^[:alpha:]+(?=_)")) %>%
  select(-key) %>%
  spread(key = var, value = value) %>%
  rename(Area_ha = area, Yield_tha = yield)

# calculate C inputs from residues
Dat_wheat <- Dat_wheat %>%
  mutate(C_res = C_in_residues(yield = Yield_tha,
                               crop = "Wheat",
                               frac_renew = 1,
                               frac_remove = 0.7))

# simulate manure N application rates (placeholder for now)
set.seed(2605)
Dat_man <- tibble(Year = 1984:2018,
                  Man_Nrate = rnorm(n = 35, mean = 30.9, sd = 5.4)) # mean and sd for manure N rate for spring crops from [Manure-application-rates.xlsx]

# calculate C inputs from manure
Dat_wheat <- Dat_wheat %>%
  left_join(Dat_man, by = "Year") %>%
  mutate(C_man = C_in_manure(man_nrate = Man_Nrate,
                             man_type = "Beef cattle"))

#####################################################
# convert sand data to df and join all data into main dataset
# using crop data as the basis limits join to only where the model is to be run
#####################################################
Dat_sand <- Ras_sand %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  drop_na() %>%
  mutate(Sand_frac = SNDPPT_M_sl4_5km_ll / 100) %>%
  select(-SNDPPT_M_sl4_5km_ll)

# miniscule discrepancy in n of sig figs prevents join if we don't take care of it here
Dat_wheat <- Dat_wheat %>%
  mutate_at(vars(x, y), funs(round(., 6)))
Dat_clim <- Dat_clim %>%
  mutate_at(vars(x, y), funs(round(., 6)))
Dat_sand <- Dat_sand %>%
  mutate_at(vars(x, y), funs(round(., 6)))

# join up all our work so far
Dat_main <- Dat_wheat %>%
  left_join(Dat_clim, by = c("x", "y", "Year")) %>% 
  left_join(Dat_sand, by = c("x", "y"))

#####################################################
# calculate environment specific model coefficients
#####################################################
Dat_crop <- read_csv("Crop-N-and-lignin-fractions.csv")
#till_type <- "full"

Dat_main <- Dat_main %>%
  mutate(N_frac = Dat_crop %>% filter(Crop == "Wheat") %>% pull(N_frac),
         Lignin_frac = Dat_crop %>% filter(Crop == "Wheat") %>% pull(Lignin_frac),
         #f2 = f2(tillage = till_type),
         #f4 = f4(sand = Sand_frac),
         C_tot = C_res + C_man)

rm(Dat_crop)

#####################################################
# convert all data to nested data frame to run model
# one row per grid cell, nested dataframes for each grid cell with rows as years
#####################################################

# nest up data by grid cell
Dat_nest <- Dat_main %>%
  group_by(x, y) %>%
  nest(data = c(Year:C_tot))

# create new dataset with run-in period
runin_period <- 20
Dat_nest <- Dat_nest %>%
  mutate(data_runin = map2(data, runin_period, run_in))

# run our model for a full till baseline scenario
fulltill_scen <- function(df){
  df %>% mutate(Till_type = "full")
}

Dat_nest <- Dat_nest %>%
  mutate(scen_baseline = data_runin %>%
           map(fulltill_scen) %>%
           map(run_model))

# baseline test plots
set.seed(2605)
ts_plot(Dat_nest, "scen_baseline", 500)

Dat_nest %>%
  ungroup() %>%
  unnest(cols = scen_baseline) %>%
  drop_na() %>%
  filter(Year == 2017) %>%
  ggplot(aes(x = x, y = y, fill = Stock_change)) +
  geom_raster() +
  #geom_polygon(data = Shp_UK, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) + # uncomment if needed — time-consuming
  coord_quickmap() +
  theme_void()

#####################################################
# model runs for modified tillage scenarios.....
#####################################################

# zero till after run-in
zerotill_scen <- function(df){
  df %>% mutate(Till_type = c("full", rep("zero", nrow(df) - 1)))
}

Dat_nest <- Dat_nest %>%
  mutate(scen_zerotill = data_runin %>%
           map(zerotill_scen) %>%
           map(run_model))

ts_plot(Dat_nest, "scen_zerotill", 500, "Zero tillage")

# reduced till after run-in
redtill_scen <- function(df){
  df %>% mutate(Till_type = c("full", rep("reduced", nrow(df) - 1)))
}

Dat_nest <- Dat_nest %>%
  mutate(scen_redtill = data_runin %>%
           map(redtill_scen) %>%
           map(run_model))

ts_plot(Dat_nest, "scen_redtill", 500, "Reduced tillage")

#####################################################
# model runs for modified manure scenarios.....
#####################################################

manure_mod <- function(df, factor){
  df %>%
    mutate(C_man = ifelse(!is.na(Year), C_man * factor, C_man),
           C_tot = C_res + C_man)
}

# manure x 2
Dat_nest <- Dat_nest %>%
  mutate(scen_man_x2 = data_runin %>%
           map(fulltill_scen) %>%
           map2(2, manure_mod) %>%
           map(run_model))

ts_plot(Dat_nest, "scen_man_x2", 500, "Manure x2")

# manure x 5
Dat_nest <- Dat_nest %>%
  mutate(scen_man_x5 = data_runin %>%
           map(fulltill_scen) %>%
           map2(5, manure_mod) %>%
           map(run_model))

ts_plot(Dat_nest, "scen_man_x5", 500, "Manure x5")

# no manure
Dat_nest <- Dat_nest %>%
  mutate(scen_man_x0 = data_runin %>%
           map(fulltill_scen) %>%
           map2(0, manure_mod) %>%
           map(run_model))

ts_plot(Dat_nest, "scen_man_x0", 500, "Manure removed")

#####################################################
# model runs for residues left on field
#####################################################

res_mod <- function(df, frac_remove){
  df %>%
    mutate(C_res = ifelse(!is.na(Year),
                          C_in_residues(yield = Yield_tha,
                                        crop = "Wheat",
                                        frac_renew = 1,
                                        frac_remove = frac_remove),
                          C_res),
           C_tot = C_res + C_man)
}

# residues retained
Dat_nest <- Dat_nest %>%
  mutate(scen_retainres = data_runin %>%
           map(fulltill_scen) %>%
           map2(0, res_mod) %>%
           map(run_model))

ts_plot(Dat_nest, "scen_retainres", 500, "No residue removal")

#####################################################
# write out scenario set for ease of access
#####################################################

write_rds(Dat_nest, "Model-scenarios/wheat-spatial-uk-1984-2018.rds")
