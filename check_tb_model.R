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