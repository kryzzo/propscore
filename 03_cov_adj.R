#####################################
# Master's thesis, Propensity Score #
# File: covariate adjustment        #
# Author: Kristin Jesse             #
# Last edit: 2021-04-22             #
#####################################


### model without adjustment ###
mod_0 = glm(death ~ treat,
            data = sim_data, 
            family = "binomial")


### conventional covariate adjustment ###
mod_covadj = glm(death ~ treat + gender + age + cardiac + 
                   COPD + liver + diab + smoke,
                 data = sim_data, 
                 family = "binomial")


### OUTPUT ###
if (print_on) {
  cat("\nModel with only treatment:\n")
  print(data.frame(round(summary(mod_0)$coef, 3),
                   signif = ifelse(summary(mod_0)$coef[,4]<0.05, "***", "")))
  
  cat("\nCovariate adjustment model:\n")
  print(data.frame(round(summary(mod_covadj)$coef, 3),
                   signif = ifelse(summary(mod_covadj)$coef[,4]<0.05, "***", "")))
}

