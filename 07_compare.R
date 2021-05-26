#####################################
# Master's thesis, Propensity Score #
# File: comparison                  #
# Author: Kristin Jesse             #
# Last edit: 2021-05-20             #
#####################################


### format ###
coefs_all = as.data.frame(coefs_all)

colnames(coefs_all) = c("coef_0","sd_0",
                        "coef_cov","sd_cov",
                        "coef_match","sd_match",
                        "coef_weigh","sd_weigh",
                        "coef_st_weigh","sd_st_weigh",
                        "coef_weigh_robust","sd_weigh_robust",
                        "coef_weigh_trim","sd_weigh_trim",
                        "coef_st_weigh_trim","sd_st_weigh_trim",
                        "coef_weigh_trim_robust","sd_weigh_trim_robust")


### confidence intervals ###
confints_all = cbind(matrix(confint1(coefs_all$coef_0, 
                                     coefs_all$sd_0), ncol = 2),
                     matrix(confint1(coefs_all$coef_cov, 
                                     coefs_all$sd_cov), ncol = 2),
                     matrix(confint1(coefs_all$coef_match, 
                                     coefs_all$sd_match), ncol = 2),
                     matrix(confint1(coefs_all$coef_weigh, 
                                     coefs_all$sd_weigh), ncol = 2),
                     matrix(confint1(coefs_all$coef_st_weigh, 
                                     coefs_all$sd_st_weigh), ncol = 2),
                     matrix(confint1(coefs_all$coef_weigh_robust, 
                                     coefs_all$sd_weigh_robust), ncol = 2),
                     matrix(confint1(coefs_all$coef_weigh_trim, 
                                     coefs_all$sd_weigh_trim), ncol = 2),
                     matrix(confint1(coefs_all$coef_st_weigh_trim, 
                                     coefs_all$sd_st_weigh_trim), ncol = 2),
                     matrix(confint1(coefs_all$coef_weigh_trim_robust, 
                                     coefs_all$sd_weigh_trim_robust), ncol = 2))


### is true beta in confint? ###
true_in_conf = cbind((confints_all[,1] < beta_tr) & 
                       (beta_tr < confints_all[,2]),
                     (confints_all[,3] < beta_tr) & 
                       (beta_tr < confints_all[,4]),
                     (confints_all[,5] < beta_tr) & 
                       (beta_tr < confints_all[,6]),
                     (confints_all[,7] < beta_tr) & 
                       (beta_tr < confints_all[,8]),
                     (confints_all[,9] < beta_tr) & 
                       (beta_tr < confints_all[,10]),
                     (confints_all[,11] < beta_tr) & 
                       (beta_tr < confints_all[,12]),
                     (confints_all[,13] < beta_tr) & 
                       (beta_tr < confints_all[,14]),
                     (confints_all[,15] < beta_tr) & 
                       (beta_tr < confints_all[,16]),
                     (confints_all[,17] < beta_tr) & 
                       (beta_tr < confints_all[,18]))

colnames(true_in_conf) = c("none","covadj","match",
                           "weight","weight_st",
                           "weight_robust","weight_trim",
                           "weight_st_trim","weight_trim_robust")
print(summary(true_in_conf))


### violin plots ###
fig_violin = ggplot(stack(coefs_all[c(1,3,5,7,13)]), aes(x = ind, y = values)) + 
  geom_violin() + 
  coord_flip() +
  geom_boxplot(width = 0.2) + 
  xlab("") + 
  ylab(expression(beta)) +
  geom_hline(yintercept = beta_tr, linetype="dashed") +
  scale_x_discrete(labels = c("no adj.", "cov. adj.", "matching", "weighting","trim. weigh."))
print(fig_violin)

fig_violin_sde = ggplot(stack(coefs_all[c(2,4,6,8,10,12,14,16,18)]), aes(x = ind, y = values)) + 
  geom_violin() + 
  coord_flip() +
  geom_boxplot(width = 0.2) + 
  xlab("") + 
  ylab("std. errors") +
  scale_x_discrete(labels = c("no adj.", "cov. adj.", "matching", "weighting", "stab. weigh.", 
                              "corr. weigh.", "trim. weigh.", "trim. stab. w.", "corr. trim. w."))

print(fig_violin_sde)