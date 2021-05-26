#####################################
# Master's thesis, Propensity Score #
# File: confints                    #
# Author: Kristin Jesse             #
# Last edit: 2021-05-20             #
#####################################


### Coefs from different methods ###
coefs_0 = summary(mod_0)$coef[2,1:2]
coefs_covadj = summary(mod_covadj)$coef[2,1:2]
coefs_match = summary(mod_match)$coef[2,1:2]
coefs_weigh = summary(mod_weigh)$coef[2,1:2]
coefs_st_weigh = summary(mod_st_weigh)$coef[2,1:2]
coefs_weigh_robust = mod_weigh_robust[2,1:2]
coefs_weigh_trim = summary(mod_weigh_trim)$coef[2,1:2]
coefs_st_weigh_trim = summary(mod_st_weigh_trim)$coef[2,1:2]
coefs_weigh_trim_robust = mod_weigh_trim_robust[2,1:2]

coefs = c(coefs_0,
          coefs_covadj,
          coefs_match,
          coefs_weigh,
          coefs_st_weigh,
          coefs_weigh_robust,
          coefs_weigh_trim,
          coefs_st_weigh_trim,
          coefs_weigh_trim_robust)

coefs_all = rbind(coefs_all, coefs)
