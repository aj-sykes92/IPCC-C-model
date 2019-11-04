# script to implement IPCC 3-pool soil carbon model

# load req'd packages
library(tidyverse)

##################
# implement top-level function for C stock change 
SOC_stock_change <- function(Active_y, Slow_y, Passive_y){
  SOC_y <- sum(c(Active_y, Slow_y, Passive_y))
  A_CMineral <- SOC_y - lag(SOC_y, default = 0)
  return(A_CMineral)
}

##################
# read in model parameters
# source: https://www.ipcc-nggip.iges.or.jp/public/2019rf/index.html, Vol. 4 .zip folder
# function to assign parameters to Global Environment objects
Dat_param <- read_csv("Model-parameters.csv")
bind_rows(Dat_param, tibble(Parameter = tillfac, ))

assign_params <- function(till_type){
  Dat_param <- Dat_param %>%
    filter(Practice == till_type | Practice == "All") %>%
    mutate(Parameter = Parameter %>% str_replace_all("_.+", ""))
  
  for(i in 1:nrow(Dat_param)){
    assign(Dat_param$Parameter[i], Dat_param$Default[i])
  }
  rm(i)
  
}

assign_params(till_type = "Full till")

##################
# algorithm to calculate active pool

