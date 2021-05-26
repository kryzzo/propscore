#####################################
# Master's thesis, Propensity Score #
# File: IPTW                        #
# Author: Kristin Jesse             #
# Last edit: 2021-05-17             #
#####################################


### weights ###
sim_data$w = ifelse(sim_data$treat == 1, 
                    1/sim_data$ps, 
                    1/(1-sim_data$ps))


### stabilised weights ###
sim_data$sw = ifelse(sim_data$treat == 1, 
                     (sum(sim_data$treat==1)/n)/sim_data$ps, 
                     (sum(sim_data$treat==0)/n)/(1-sim_data$ps))


### treatment effect models ###
mod_weigh = glm(death ~ treat, 
                data = sim_data,
                weights = w,
                family = "quasibinomial")
mod_weigh_robust = coeftest(mod_weigh, vcov = vcovHC(mod_weigh, type="HC1"))

mod_st_weigh = glm(death ~ treat, 
                   data = sim_data,
                   weights = sw,
                   family = "quasibinomial")
mod_st_weigh_robust = coeftest(mod_st_weigh, vcov = vcovHC(mod_st_weigh, type="HC1"))


### trim large weights ###
trim_data = sim_data[sim_data$w<=10,]

mod_weigh_trim = glm(death ~ treat,
                     data = trim_data,
                     weights = w,
                     family = "quasibinomial")
mod_st_weigh_trim = glm(death ~ treat,
                        data = trim_data,
                        weights = sw,
                        family = "quasibinomial")
mod_weigh_trim_robust = coeftest(mod_weigh_trim, vcov = vcovHC(mod_weigh_trim, type="HC1"))


### OUTPUT ###
if (print_on) {
  cat("\nregular weights\n")
  print(data.frame(round(summary(mod_weigh)$coef, 3),
                   signif = ifelse(summary(mod_weigh)$coef[,4]<0.05, "***", "")))
  
  cat("\ncorrected\n")
  print(data.frame(round(mod_weigh_robust[,], 3),
                   signif = ifelse(mod_weigh_robust[,4]<0.05, "***", "")))
  
  cat("\nstabilised weights\n")
  print(data.frame(round(summary(mod_st_weigh)$coef, 3),
                   signif = ifelse(summary(mod_st_weigh)$coef[,4]<0.05, "***", "")))
  
  cat("\nstabilised corrected\n")
  print(data.frame(round(mod_st_weigh_robust[,], 3),
                   signif = ifelse(mod_st_weigh_robust[,4]<0.05, "***", "")))
  }

if (graphics_on) {
  # weights
  fig_w = ggplot(sim_data, aes(x = w, fill = as.factor(treat))) +
    ggtitle("Weights") +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment")
  fig_sw = ggplot(sim_data, aes(x = sw, fill = as.factor(treat))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment")
  print(ggarrange(fig_w, fig_sw, common.legend = TRUE, 
                  ncol=1, nrow=2, legend = "bottom"))
  
  # trimmed weights
  fig_w_trim = ggplot(trim_data, aes(x = w, fill = as.factor(treat))) +
    ggtitle("Weights") +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment")
  fig_sw_trim = ggplot(trim_data, aes(x = sw, fill = as.factor(treat))) +
    geom_density(alpha = 0.5) +
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment")
  print(ggarrange(fig_w_trim, fig_sw_trim, common.legend = TRUE, 
                  ncol=1, nrow=2, legend = "bottom"))
  
  # PS before and after weighting
  fig_ps_orig = ggplot(sim_data, aes(x = ps, fill = as.factor(treat))) + 
    geom_density(alpha = 0.5) + 
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment") + 
    xlim(0,1)
  fig_ps_sw = ggplot(data = sim_data, aes(x = ps, fill = as.factor(treat))) +
    geom_density(aes(weight = sw), alpha = 0.5) +
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment") +
    xlab("weighted ps") +
    xlim(0,1)
  print(ggarrange(fig_ps_orig,fig_ps_sw, common.legend = TRUE))
  
  fig_ps_sw_trim = ggplot(data = trim_data, aes(x = ps, fill = as.factor(treat))) +
    geom_density(aes(weight = sw), alpha = 0.5) +
    scale_fill_manual(values = c("firebrick3", "slateblue3")) +
    labs(fill="treatment") +
    xlab("weighted ps") +
    xlim(0,1)
  
  # Love plots for covariate balance
  bal_tab_weight = bal.tab(sim_data[,1:7], treat = sim_data$treat,
                           weights = sim_data$w, method = "weigh",
                           estimand = "ATE", un = TRUE, abs = TRUE,
                           binary = "std")
  
  bal_tab_weight_trim = bal.tab(trim_data[,1:7], treat = trim_data$treat,
                                weights = trim_data$w, method = "weigh",
                                estimand = "ATE", un = TRUE, abs = TRUE,
                                binary = "std")
  lp_weighted = love.plot(bal_tab_weight,
                          var.order = "unadj",
                          stars = "std",
                          colors = c("firebrick3", "slateblue3"),
                          shapes = c(15,19),
                          sample.names = c("original","weighted")) +
    ggtitle("Weighted") +
    geom_vline(xintercept = 0.1, linetype="dashed")
  print(lp_weighted)
  
  lp_weighted_trim = love.plot(bal_tab_weight_trim,
                               var.order = "unadj",
                               stars = "std",
                               colors = c("firebrick3", "slateblue3"),
                               shapes = c(15,19),
                               sample.names = c("original","weighted")) +
    ggtitle("Weighted") +
    geom_vline(xintercept = 0.1, linetype="dashed")
  print(lp_weighted_trim)
}
