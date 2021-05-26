#####################################
# Master's thesis, Propensity Score #
# File: data simulation             #
# Author: Kristin Jesse             #
# Last edit: 2021-05-19             #
#####################################


### age ###
age_dist = data.frame(min=c(1, 18, 40, 65, 80), max=c(18, 40, 65, 80, 90),
                      prop=c(0.06, 0.22, 0.29, 0.35, 0.08))
age_group = sample(nrow(age_dist), n, replace=TRUE, prob=age_dist$prop)
age = age_dist$min[age_group] + runif(n) *
  (age_dist$max[age_group] - age_dist$min[age_group])


### gender ###
gender = as.factor(rbinom(n, 1, 0.6))


### comorbidities ###
# if independent (not reported in thesis, no difference in results)
if (baseline=="indep"){
  cardiac = as.factor(rbinom(n, 1, 0.2)) # congestive cardiac failure
  liver = as.factor(rbinom(n, 1, 0.2))   # liver disease
  diab = as.factor(rbinom(n, 1, 0.2))    # diabetes
  smoke = as.factor(rbinom(n, 1, 0.2))   # smoking status
  COPD = as.factor(rbinom(n, 1, 0.2))    # chronic obstructive pulmonary disease
}

# if dependent
if (baseline=="dep"){
  cardiac = as.factor((age<65)*(rbinom(n, 1, 0.01)) + 
                        (age>=65)*(rbinom(n, 1, 0.1))) # congestive cardiac failure
  liver = as.factor(rbinom(n, 1, 0.05))                # liver disease
  diab = as.factor(rbinom(n, 1, 0.15))                 # diabetes
  smoke = as.factor((age>=18)*rbinom(n, 1, 0.2))       # smoking status
  COPD = as.factor((smoke==0)*rbinom(n, 1, 0.25) + 
                     (smoke==1)*rbinom(n, 1, 0.5))     # chronic obstructive pulmonary disease
}


### combine data ###
sim_data = data.frame(age, gender, cardiac, liver, COPD, diab, smoke)


### RCT ###
if (type == "RCT") {
  treat = as.factor(rbinom(n, 1, 0.4))
  sim_data$treat = treat
  
  m_d = -3.5 + beta_tr*as.numeric(as.character(treat)) + 
    0.01*age +
    0.2*as.numeric(as.character(cardiac)) + 
    0.1*as.numeric(as.character(COPD)) + 
    -0.1*as.numeric(as.character(diab)) +
    1*as.numeric(as.character(smoke))
  p_d = 1/(1 + exp(-m_d))
  death = as.factor(rbinom(n, 1, p_d))
  sim_data$death = death
}


### all covariates are confounders ###
if (type == "allconfound") {
  m_tr = 1 - 0.02*age + 
    -0.2*as.numeric(as.character(gender)) +
    -0.2*as.numeric(as.character(cardiac)) +
    -0.2*as.numeric(as.character(liver)) + 
    -0.2*as.numeric(as.character(COPD)) + 
    -0.2*as.numeric(as.character(diab)) + 
    -0.2*as.numeric(as.character(smoke))
  p_tr = 1/(1 + exp(-m_tr))
  treat = as.factor(rbinom(n, 1, p_tr))
  sim_data$treat = treat
  
  m_d = -2 + beta_tr*as.numeric(as.character(treat)) + 
    0.01*age + 
    0.1*as.numeric(as.character(gender)) +
    0.1*as.numeric(as.character(cardiac)) +
    0.1*as.numeric(as.character(liver)) + 
    0.1*as.numeric(as.character(COPD)) + 
    0.1*as.numeric(as.character(diab)) + 
    0.1*as.numeric(as.character(smoke))
  p_d = 1/(1 + exp(-m_d))
  death = as.factor(rbinom(n, 1, p_d))
  sim_data$death = death
}


### basic model (not reported in thesis) ###
if (type == "basic") {
  m_tr = -0.02*age + 
    0.2*as.numeric(as.character(gender)) +
    -1*as.numeric(as.character(liver)) + 
    1*as.numeric(as.character(COPD)) + 
    0.2*as.numeric(as.character(smoke))
  p_tr = 1/(1 + exp(-m_tr))
  treat = as.factor(rbinom(n, 1, p_tr))
  sim_data$treat = treat
  
  m_d = -3.5 + beta_tr*as.numeric(as.character(treat)) + 
    0.01*age +
    0.2*as.numeric(as.character(cardiac)) + 
    0.1*as.numeric(as.character(COPD)) + 
    -0.1*as.numeric(as.character(diab)) +
    0.2*as.numeric(as.character(smoke))
  p_d = 1/(1 + exp(-m_d))
  death = as.factor(rbinom(n, 1, p_d))
  sim_data$death = death
}


### smoking affects treatment and outcome the opposite way ###
if (type == "smoke_x") {
  m_tr = 0.5 -0.02*age + 
    0.2*as.numeric(as.character(gender)) +
    -1*as.numeric(as.character(liver)) + 
    1*as.numeric(as.character(COPD)) + 
    -2*as.numeric(as.character(smoke))
  p_tr = 1/(1 + exp(-m_tr))
  treat = as.factor(rbinom(n, 1, p_tr))
  sim_data$treat = treat
  
  m_d = -3.5 + beta_tr*as.numeric(as.character(treat)) + 
    0.02*age +
    0.2*as.numeric(as.character(cardiac)) + 
    2*as.numeric(as.character(smoke)) + 
    -0.1*as.numeric(as.character(diab))
  p_d = 1/(1 + exp(-m_d))
  death = as.factor(rbinom(n, 1, p_d))
  sim_data$death = death
}


### smoke affects treatment and outcome the same way ###
if (type == "smoke_l") {
  m_tr = -0.02*age + 
    0.2*as.numeric(as.character(gender)) +
    -1*as.numeric(as.character(liver)) + 
    1*as.numeric(as.character(COPD)) + 
    2*as.numeric(as.character(smoke))
  p_tr = 1/(1 + exp(-m_tr))
  treat = as.factor(rbinom(n, 1, p_tr))
  sim_data$treat = treat
  
  m_d = -4 + beta_tr*as.numeric(as.character(treat)) + 
    0.02*age +
    0.2*as.numeric(as.character(cardiac)) + 
    2*as.numeric(as.character(smoke)) + 
    -0.1*as.numeric(as.character(diab))
  p_d = 1/(1 + exp(-m_d))
  death = as.factor(rbinom(n, 1, p_d))
  sim_data$death = death
}


### OUTPUT ###
if (print_on) {
  cat("Dataset summary:\n")
  print(summary(sim_data))
  print(table(treat, death))
  print(table(smoke, treat))
  print(table(smoke, death))
}

if (graphics_on) {hist(age, 20)}
