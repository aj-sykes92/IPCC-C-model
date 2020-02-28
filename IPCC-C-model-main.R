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
  assign(Dat_param$Parameter[i], Dat_param$BestEstimate[i], envir = pm)
}
rm(i)

##################
# calculate f4 (variable depending on sand fraction)
f4 <- function(sand){
  f4 <- 1 - pm$f5 - (pm$f4par1 + pm$f4par2 * sand)
  return(f4)
}

##################
# calculate f2 (variable depending on tillage)
f2 <- function(tillage){
  x <- match(tillage, c("unknown", "full", "reduced", "zero"))
  y <- c(pm$f2, pm$f2_ft, pm$f2_rt, pm$f2_nt)[x]
  return(y)
}

###################
# calculation of intermediate values
beta <- function(C_input, LC, NC){
  beta <- C_input * (pm$sp1 - pm$sp2 * LC / NC)
  return(beta)
}

alpha <- function(C_input, LC, NC, sand, tillage){
  beta <- beta(C_input = C_input, LC = LC, NC = NC)
  x <- beta * pm$f1
  y <- ((C_input * (1 - LC) - beta) * f2(tillage = tillage))
  z <- ((C_input * LC) * pm$f3 * (pm$f7 + pm$f8 * pm$f6))
  d <- 1 - (f4(sand = sand) * pm$f7) - (pm$f5 * pm$f8) - (f4(sand = sand) * pm$f6 * pm$f8)
  alpha <- (x + y + z) / d
  return(alpha)
}

##################
# soil water factor
wfac <- function(precip, PET){
  mappet_i <- pmin(1.25, precip/PET)
  W_i <- pm$wfacpar1 + pm$wfacpar2 * mappet_i + pm$wfacpar3 * mappet_i^2
  wfac <- 1.5 * (1 / 12 * sum(W_i))
  return(wfac)
}

##################
# temperature factor
tfac <- function(temp){
  prelim <- (pm$tmax - temp) / (pm$tmax - pm$topt)
  T_i <- prelim^pm$ta * exp(0.076 * (1 - prelim^pm$tb))
  tfac <- 1 / 12 * sum(T_i)
  return(tfac)
}

##################
# tillage factor
tillfac <- function(tillage){
  x <- match(tillage, c("unknown", "full", "reduced", "zero"))
  y <- c(pm$tillfac_ft, pm$tillfac_ft, pm$tillfac_rt, pm$tillfac_nt)[x]
  return(y)
}

##################
# algorithm to calculate active pool
k_a <- function(tfac, wfac, tillfac, sand){
  k_a <- pm$kfaca * tfac * wfac * (pm$k3par1 + (pm$k3par2 * sand)) * tillfac
  return(k_a)
}

active_y_ss <- function(k_a, alpha){
  active_y_ss <- alpha / k_a
  return(active_y_ss)
}

active_y <- function(k_a, active_y_m1, active_y_ss){
  active_y <- active_y_m1 + (active_y_ss - active_y_m1) * min(1, k_a)
  return(active_y)
}

###################
# algorithm to calculate slow pool
k_s <- function(tfac, wfac, tillfac){
  k_s <- pm$kfacs * tfac * wfac * tillfac
  return(k_s)
}

slow_y_ss <- function(C_input, LC, active_y_ss, k_s, k_a, sand){
  ((C_input * LC * pm$f3) + (active_y_ss * k_a * f4(sand = sand))) / k_s
  return(slow_y_ss)
}

slow_y <- function(k_s, slow_y_m1, slow_y_ss){
  slow_y <- slow_y_m1 + (slow_y_ss - slow_y_m1) * min(1, k_s)
  return(slow_y)
}

###################
# algorithm to calculate passive pool
k_p <- function(tfac, wfac){
  k_p <- pm$kfacp * tfac * wfac
  return(k_p)
}

passive_y_ss <- function(active_y_ss, slow_y_ss, k_a, k_s, k_p){
  passive_y_ss <- ((active_y_ss * k_a * pm$f5) + (slow_y_ss * k_s * pm$f6)) / k_p
  return(passive_y_ss)
}

passive_y <- function(passive_y_m1, passive_y_ss, k_p){
  passive_y <- passive_y_m1 + (passive_y_ss - passive_y_m1) * min(1, k_p)
  return(passive_y)
}

detach("package:tidyverse", unload = T)