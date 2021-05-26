#####################################
# Master's thesis, Propensity Score #
# File: set-up and functions        #
# Author: Kristin Jesse             #
# Last edit: 2021-05-20             #
#####################################


# empty workspace
rm(list=ls())
if(!is.null(dev.list())) dev.off()


# load packages
library(MatchIt)
library(ggplot2)
library(gridExtra)
library(cobalt)
library(viridis)
library(lmtest)
library(sandwich)
library(tidyverse)
library(ggpubr)


# ggplot2 options
theme_set(theme_bw())


# function: confidence intervals for treatment coefficient,
# this calculation is specificaly for a 95% confidence interval 
# for selected coefficient, much faster than the built-in 
# confint function
confint1 = function(coef,sd) {
  c(coef - qnorm(0.975)*sd,
    coef + qnorm(0.975)*sd)
}


# function: run all files
# if n = 1, print outputs
# if n > 1, only violin plots
runall = function(n_sims = 1000, n = 1000, type = "basic", 
                  baseline = "dep", beta_tr = 0) {
  
  .GlobalEnv$type = type           # RCT, allconfound, basic, smoke_x, smoke_l
  .GlobalEnv$baseline = baseline   # dep, indep
  
  .GlobalEnv$coefs_all = c()
  
  .GlobalEnv$n_sims = n_sims
  .GlobalEnv$n = n
  .GlobalEnv$beta_tr = beta_tr
  
  if (n_sims == 1) {
    
    .GlobalEnv$print_on = TRUE
    .GlobalEnv$graphics_on = TRUE
    
    ### CREATE DATA ###
    source("thesis_02_data_simulation.R")
    
    ### RUN METHODS ###
    source("thesis_03_cov_adj.R")
    source("thesis_04_matching.R")
    source("thesis_05_IPTW.R")
    
  } else {
    
    .GlobalEnv$print_on = FALSE
    .GlobalEnv$graphics_on = FALSE
    
    for (i in 1:n_sims) {
      if (i%%(n_sims%/%10)==0) {print(i)}
      
      ### CREATE DATA ###
      source("thesis_02_data_simulation.R")
      
      ### RUN METHODS ###
      source("thesis_03_cov_adj.R")
      source("thesis_04_matching.R")
      source("thesis_05_IPTW.R")
      
      ### ANALYSE ###
      source("thesis_06_confints.R")
    }
    
    ### COMPARISON ###
    source("thesis_07_compare.R")
    
  }
}