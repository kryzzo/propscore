#####################################
# Master's thesis, Propensity Score #
# File: main                        #
# Author: Kristin Jesse             #
# Last edit: 2021-05-20             #
#####################################


### SETUP ###
source("thesis_01_setup.R")


### RCT ###
runall(n_sims = 1, type = "RCT", beta_tr = 0)
runall(n_sims = 1, type = "RCT", beta_tr = -1)

runall(n_sims = 1000, type = "RCT", beta_tr = 0)
runall(n_sims = 1000, type = "RCT", beta_tr = -1)


### all covariates are confounders ###
runall(n_sims = 1, type = "allconfound", beta_tr = 0)
runall(n_sims = 1, type = "allconfound", beta_tr = -1)

runall(n_sims = 1000, type = "allconfound", beta_tr = 0)
runall(n_sims = 1000, type = "allconfound", beta_tr = -1)


### smoking affects treatment and outcome strongly (opposite ways) ###
runall(n_sims = 1, type = "smoke_x", beta_tr = 0)
runall(n_sims = 1, type = "smoke_x", beta_tr = -1)

runall(n_sims = 1000, type = "smoke_x", beta_tr = 0)
runall(n_sims = 1000, type = "smoke_x", beta_tr = -1)




