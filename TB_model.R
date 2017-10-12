#Packages
library(tidyverse)

## Model equations
TB_model <- function(t, x, params) {
  
  ## Specify model compartments
  S <- x[1]
  H <- x[2]
  L <- x[3]
  L_r <- x[4] 
  I_n <- x[5]
  I_p <- x[6]
  R <- x[7]
  
  with(as.list(params),{
    
    ## Specify total population
    N = S + H + L + L_r + I_n + I_p + R
    
    ## Force of infection
    foi = ecr * (rel_inf_n * I_n + I_p)/N

    ## Total infected
    total_infected <-  prim_dis_onset * H + sec_dis_onset * L + reinf_dis_onset * L_r + relapse * R
    
    ##Births
    births <- mu * N + mu_n * I_n + mu_p * I_p
    
    ## Derivative Expressions
    dS = births - foi * S - mu * S
    dH = foi * S - prim_dis_onset * H - rate_low_latent * H - mu * H
    dL = rate_low_latent * (H + L_r) - foi * L - sec_dis_onset * L - mu * L
    dL_r = foi * L + foi * R - reinf_dis_onset * L_r - rate_low_latent * L_r - mu * L_r
    dI_n = prop_n * total_infected - natural_cure * I_n - detect_recover_n * I_n - (mu + mu_n) * I_n
    dI_p = prop_p * total_infected - natural_cure * I_p - detect_recover_p * I_p - (mu + mu_p) * I_p
    dR = natural_cure * (I_n + I_p) + detect_recover_n * I_n + detect_recover_p * I_p - relapse * R - foi * R - mu * R
    
    ## derivatives
    derivatives <- c(dS, dH, dL, dL_r, dI_n, dI_p, dR)
    
    ##summary measures
    summary_measures <- c(
      year = t/timestep,
      ann_risk_inf = 100 * foi * timestep,
      total_deaths = births,
      total_inf = I_n + I_p,
      total_new_cases = prim_dis_onset * H + sec_dis_onset * L + reinf_dis_onset * L_r,
      total_new_deaths = mu_n * I_n + mu_p * I_p,
      total_new_infs = foi * S,
      total_new_prim_inf = prim_dis_onset * H,
      total_new_reinf_inf = reinf_dis_onset * L_r,
      total_new_react = sec_dis_onset * L)
    
      summary_measures <- c(summary_measures,
                            prop_dis_recent_inf = summary_measures["total_new_prim_inf.H"]/summary_measures["total_new_cases.H"],
                            prop_dis_recent_reinf = summary_measures["total_new_reinf_inf.L_r"]/summary_measures["total_new_cases.H"],
                            prop_dis_react = summary_measures["total_new_react.L"]/summary_measures["total_new_cases.H"],
                            new_cases_react_pHK = sec_dis_onset * L * 100000 * timestep / N,
                            new_cases_recent_ing_pHK = prim_dis_onset * H * 100000 * timestep / N,
                            new_cases_recent_reinf_pHK = reinf_dis_onset * L_r * 100000 * timestep / N,
                            ann_TB_incidence_pHK = summary_measures["total_new_cases.H"] * 100000 * timestep / N,
                            TB_mort_rate_pHK = summary_measures["total_new_deaths.I_n"] * 100000 * timestep / N,
                            TB_prevalence = (I_n + I_p)/N
      )
  
    ## output
    list(derivatives, summary_measures)
  })
}


## initial model
init_TB_model <- function(total_pop = 100000) {
  H <- 10
  L <- 10
  L_r <- 10
  I_n <- 50
  I_p <- 50
  R <- 10
  S <- total_pop - H - L - L_r - I_n - I_p - R
  
  return(c(S = S, H = H, L = L, L_r = L_r, I_n = I_n, I_p = I_p, R = R))
}


## Set parameters for model
params_TB_model <- function(ecr_pyr = 15, wks_infect_n = 95,
                            wks_infect_p = 51, prot_reinf = 0.65,
                            timestep = 52) {
  
  ## yearly rate of disease onset
  onset_yr <- 0.03

  ## proportion that are smear negative
  prop_p <- 0.7
  
  ## Mortality rates
  mu <- 0.021
  mu_n <- 0.19
  mu_p <- 0.22
  
  ## Cure rate
  natural_cure <- 0.2
  
  ## parameters
  params <- list(ecr = ecr_pyr / timestep,
                 rate_low_latent = 1 / (5 * timestep),
                 rel_inf_n = 0.22,
                 prim_dis_onset = onset_yr / timestep, 
                 sec_dis_onset  = 0.0003/timestep,
                 reinf_dis_onset = (onset_yr * (1 - prot_reinf))/timestep,
                 prop_n = 1 - prop_p, 
                 prop_p = prop_p,
                 mu = mu/timestep,
                 mu_n = mu_n/timestep,
                 mu_p = mu_p/timestep,
                 detect_recover_n = 52/(wks_infect_n * timestep) - (natural_cure + mu + mu_n)/timestep,
                 detect_recover_p = 52/(wks_infect_p * timestep)  - (natural_cure + mu + mu_p)/timestep,
                 natural_cure = natural_cure/timestep,
                 relapse = 0.001/timestep,
                 timestep = timestep
                 )
  return(params)
}

                      
##Summarise model rates
sum_model_rates <- function(model_traj, start_time = 2010) {
  model_traj <- as.data.frame(model_traj) %>% as_tibble
  ## Filter based on start time
  model_traj <- model_traj[unlist(model_traj[["year"]]) >= start_time, ]
  
  with(model_traj,{
    df <- data_frame(Year = year,
                     `Annual TB incidence rate` = ann_TB_incidence_pHK.total_new_cases.H,
                     `TB mortality rate` = TB_mort_rate_pHK.total_new_deaths.I_n,
                     `Annual risk of infection` = ann_risk_inf.I_n)
  })
}


##Sumarise model proportions
sum_model_proportions <- function(model_traj, start_time = 2010) {
  model_traj <- as.data.frame(model_traj) %>% as_tibble
  
  ## Filter based on start time
  model_traj <- model_traj[unlist(model_traj[["year"]]) >= start_time, ]
  
  with(model_traj,{
    df <- data_frame(Year = year,
                     `Recent infection` = prop_dis_recent_inf.total_new_prim_inf.H,
                     `Recent reinfection` = prop_dis_recent_reinf.total_new_reinf_inf.L_r,
                     `Reactivation` = prop_dis_react.total_new_react.L)
  })
}

                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 