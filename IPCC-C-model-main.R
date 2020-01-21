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
# assign parameters into new environment [pm]
Dat_param <- read_csv("Model-parameters.csv")
pm <- new.env()
for(i in 1:nrow(Dat_param)){
  assign(Dat_param$Parameter[i], Dat_param$Default[i], envir = pm)
}
rm(i)

###################
# calculation of intermediate values
beta <- function(C_input, LC, NC){
  beta <- C_input * (0.85 - 0.018 * LC / NC)
  return(beta)
}

alpha <- function(C_input, LC, NC){
  beta <- beta(C_input = C_input, LC = LC, NC = NC)
  x <- beta * pm$f1
  y <- ((C_input * (1 - LC) - beta) * pm$f2)
  z <- ((C_input * LC) * pm$f3 * (pm$f7 + pm$f8 * pm$f6))
  d <- 1 - (pm$f4 * pm$f7) - (pm$f5 * pm$f8) - (pm$f4 * pm$f6 * pm$f8)
  alpha <- (x + y + z) / d
  return(alpha)
}

##################
# algorithms to calculate temperature and water factors

# temperature factor, equation 5
Ti <- function(tempi){
  Ti <- ifelse(tempi > 45,
               0,
               ((pm$tmax - tempi) / (pm$tmax / pm$topt))^0.2 * exp(0.076 * (1 - ((pm$tmax - tempi) / (pm$tmax / pm$topt))^2.63))
  )
  return(Ti)
}


tfac <- function()

##################
# algorithm to calculate active pool

