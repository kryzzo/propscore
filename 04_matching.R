#####################################
# Master's thesis, Propensity Score #
# File: matching                    #
# Author: Kristin Jesse             #
# Last edit: 2021-04-22             #
#####################################


### prop score model ###
PS_model = glm(treat ~ gender + age + cardiac + 
                 COPD + liver + diab + smoke,
               data = sim_data,
               family = "binomial")
sim_data$ps = predict(PS_model, type = "response")


### matching on PS ###
match = matchit(treat ~ gender + age + cardiac + 
                  COPD + liver + diab + smoke,
                caliper = 0.1,
                data = sim_data[, setdiff(names(sim_data),"ps")])
matched_data = match.data(match, distance = "ps")


### treatment effect models ###
mod_match = glm(death ~ treat, 
                data = matched_data,
                family = "binomial")


### OUTPUT ###
if (print_on) {
  cat("\nPS model:\n")
  print(data.frame(round(summary(PS_model)$coef, 3),
                   signif = ifelse(summary(PS_model)$coef[,4]<0.05, "***", "")))
  
  cat("\nMatched dataset summary:\n")
  print(summary(matched_data))
  
  cat("\nOutcome model:\n")
  print(data.frame(round(summary(mod_match)$coef, 3),
                   signif = ifelse(summary(mod_match)$coef[,4]<0.05, "***", "")))
}

if (graphics_on) {
  # PS figures
  fig_ps_orig = ggplot(sim_data, aes(x = ps, fill = as.factor(treat))) + 
    geom_density(alpha = 0.5) + 
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment") + 
    xlim(0,1)
  fig_ps_matched = ggplot(matched_data, aes(x = ps, fill = as.factor(treat))) + 
    geom_density(alpha = 0.5) + 
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment") +
    xlim(0,1)
  print(ggarrange(fig_ps_orig, fig_ps_matched, common.legend = TRUE))
  
  # Love plot for covariate balance
  bal_tab_match = bal.tab(match, abs = TRUE, un=TRUE, binary = "std")
  lp_matched = love.plot(bal_tab_match, var.order = "unadj",
                         drop.distance = TRUE,
                         stars = "std",
                         colors = c("firebrick3", "slateblue3"),
                         shapes = c(15,19),
                         sample.names = c("original","matched")) +
    ggtitle("Matched") +
    geom_vline(xintercept = 0.1, linetype="dashed")
  print(lp_matched)
}
