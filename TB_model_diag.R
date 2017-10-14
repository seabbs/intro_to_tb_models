#Packages
library(tidyverse)

## Model equations
TB_model_diag <- function(t, x, params) {
  
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
    
    ## Evaluate if intervention is in place
    if (intervention == 1 && t/timestep >= intervention_start) { 
      detect_recover_n <- interv_detect_recover_n 
    } else{
        detect_recover_n <- preinterv_detect_recover_n 
    }
    
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

## Set parameters for model
params_TB_model_diag <- function(ecr_pyr = 15, wks_health_service = 52,
                            prot_reinf = 0.65,
                            intervention = 1, intervention_start = 2014,
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
  
  ## TB model parameters
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
                 natural_cure = natural_cure/timestep,
                 relapse = 0.001/timestep,
                 timestep = timestep
  )
  
  ## Treatment pathway parameters - weeks to treatmetns
  wks_to_health_services_n <- wks_health_service
  wks_to_health_services_p <- wks_health_service
  wks_HS_visit_to_treatment <- 5.4
  
  wks_to_treat_if_Xrayed_n <- wks_to_health_services_n + wks_HS_visit_to_treatment
  wks_to_treat_p <- wks_to_health_services_p + wks_HS_visit_to_treatment
  
  ## Xray related parameters
  prop_access_Xray <- 0.3
  X_ray_sensitivity <- 0.8
  prop_default_treatment <- 0.15
  prop_treated_successfully <- 0.75
  
  ## New Intervention adjusted weeks
  new_diag_sensitivity_n <- 0.7
  new_diag_wks_to_treat_n <- wks_to_treat_if_Xrayed_n
  
  ## Rates of detection and recovery for both postive and negative sputum smear
  intervention_rates <- list(
    preinterv_detect_recover_n = (52 * prop_access_Xray*X_ray_sensitivity*(1 - prop_default_treatment)*prop_treated_successfully) / (wks_to_treat_if_Xrayed_n * timestep),
    detect_recover_p = 52 * (1 - prop_default_treatment)*prop_treated_successfully / (wks_to_treat_p * timestep),
    preinterv_detect_only_n =  52 * prop_access_Xray*X_ray_sensitivity / (wks_to_treat_if_Xrayed_n * timestep),
    detect_only_p = 52 / (wks_to_treat_p * timestep),
    interv_detect_recover_n = 52 * new_diag_sensitivity_n * (1 - prop_default_treatment) * prop_treated_successfully / (new_diag_wks_to_treat_n * timestep),
    interv_detect_only_n = 52 * new_diag_sensitivity_n / (new_diag_wks_to_treat_n * timestep)
    )

  ## Intervention control parameters
  yrs_evaluate <- 10
  
  intervention_params <- list(
    intervention = intervention,
    intervention_start = intervention_start,
    yrs_evaulate = yrs_evaluate,
    yr_eval_impact = intervention_start + yrs_evaluate  
  )
  return(c(params, intervention_rates, intervention_params))
}
                                                                                                                                              
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 
                                                                                                                                                 