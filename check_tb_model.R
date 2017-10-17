#Load functions
source("TB_model.R")
source("graph_tb_model.R")

##Check model runs
timestep <- 52
initial <- init_TB_model()
times <- seq(0, 2024*timestep, 1)
params <- params_TB_model(ecr_pyr = 15, timestep = timestep)

model_traj <- deSolve::lsoda(initial, times, TB_model, params)

##Summarise model
sum_model_traj_rates <- sum_model_rates(model_traj, start_time = 2010)
sum_model_traj_props <- sum_model_proportions(model_traj, start_time = 2010)

##Graph model
graph_rates(sum_model_traj_rates)
graph_annual_risk(sum_model_traj_rates)
graph_proportions(sum_model_traj_props)


## Check diagnosis model
source("TB_model_diag.R")
source("graph_tb_model_diag.R")

## Check diag model runs
timestep <- 1
initial <- init_TB_model()
times <- seq(0, 2024*timestep, 1)
params <- params_TB_model_diag(ecr_pyr = 15, wks_health_service = 52,
                               prot_reinf = 0.65,
                               intervention = 1, intervention_start = 2014,
                               timestep = timestep)

model_traj <- deSolve::lsoda(initial, times, TB_model_diag, params)

## Check summary measures
cum_traj <- cum_measures(model_traj, params)

