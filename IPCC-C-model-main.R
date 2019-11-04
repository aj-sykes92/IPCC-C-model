# script to implement IPCC 3-pool soil carbon model

# load req'd packages
library(tidyverse)

# implement top-level function for C stock change
SOC_stock_change <- function(Active_y, Slow_y, Passive_y){
  SOC_y <- sum(c(Active_y, Slow_y, Passive_y))
  A_CMineral <- SOC_y - lag(SOC_y, default = 0)
  return(A_CMineral)
}


