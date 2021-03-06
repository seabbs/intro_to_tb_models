#Load packages
source("load_packages.R")


##Load code
source("TB_model.R")
source("graph_tb_model.R")
source("TB_model_diag.R")
source("graph_tb_model_diag.R")

## Stop spurious warnings
options(warn = -1)

shinyServer(function(input, output) {
  
  ## Run TB model
  model_traj <- reactive({
    ##Check model runs
    initial <- init_TB_model()
    times <- seq(0, 2024*as.numeric(input$timestep), 1)
    params <- params_TB_model(ecr_pyr = input$ecr, wks_infect_n = input$wks_inf_n,
                              wks_infect_p = input$wks_inf_p,  prot_reinf = input$prot_init_reint,
                              timestep = as.numeric(input$timestep))
    
    model_traj <- deSolve::lsoda(initial, times, TB_model, params)
    
  })
  
  ## summarise TB model rates
  sum_rates <- reactive({
    if (input$burn_in) {
      start_time <- 0
    }else{
      start_time <- 2010
    }
    
    sum_model_rates(model_traj(), start_time = start_time)
  })
  
  sum_props <- reactive({
    if (input$burn_in) {
      start_time <- 0
    }else{
      start_time <- 2010
    }
    
    sum_model_proportions(model_traj(), start_time = start_time)
  })
  
  ## Plot rates
  output$plot_rates <- renderPlotly({
    plot <- sum_rates() %>% 
      graph_rates
    
    ggplotly(plot)
  })
  
  ##Annual risk plot
  output$plot_annual_risk <- renderPlotly({
    plot <- sum_rates() %>% 
      graph_annual_risk()
    
    ggplotly(plot)
  })
  
  ## Rates as table
  output$rates_table <- DT::renderDataTable({
    sum_rates()},
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      scrollY = TRUE,
      orderClasses = TRUE)
  )
  
  ## Plot props
  output$plot_props <- renderPlotly({
    plot <- sum_props() %>% 
      graph_proportions
    
    ggplotly(plot)
  })
  ##Prop as table
  output$props_table <- DT::renderDataTable({
    sum_props()
  },
    options = list(
                  pageLength = 5,
                  scrollX = TRUE,
                  scrollY = TRUE,
                  orderClasses = TRUE)
  )
  
  ##Model Trajectories as table
  output$traj_table <- DT::renderDataTable({
    model_traj()
  },
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = TRUE,
    orderClasses = TRUE)
  )
  
  
  ## TB model with diagnostics
  
  ## Run TB model
  diag_params <- reactive({
    if (input$intervention) {
      intervention <- 1
    } else{
      intervention <- 0
    }
    
    params <- params_TB_model_diag(ecr_pyr = input$ecr_diag,  wks_health_service = 52,
                                   prot_reinf = 0.65, intervention = intervention, 
                                   intervention_start = 2014,
                                   timestep = as.numeric(input$timestep_diag))
  })
  
  model_traj_diag <- reactive({

    ##Check model runs
    initial <- init_TB_model()
    times <- seq(0, 2024*as.numeric(input$timestep_diag), 1)
    model_traj <- deSolve::lsoda(initial, times, TB_model_diag, diag_params())
    
  })
  
  ## summarise TB model rates
  sum_rates_diag <- reactive({
    if (input$burn_in) {
      start_time <- 0
    }else{
      start_time <- 2010
    }
    
    sum_model_rates(model_traj_diag(), start_time = start_time)
  })
  
  sum_props_diag <- reactive({
    if (input$burn_in_diag) {
      start_time <- 0
    }else{
      start_time <- 2010
    }
    
    sum_model_proportions(model_traj_diag(), start_time = start_time)
  })
  
  ## Plot rates
  output$plot_rates_diag <- renderPlotly({
    plot <- sum_rates_diag() %>% 
      graph_rates
    
    ggplotly(plot)
  })
  
  ##Annual risk plot
  output$plot_annual_risk_diag <- renderPlotly({
    plot <- sum_rates_diag() %>% 
      graph_annual_risk()
    
    ggplotly(plot)
  })
  
  ## Rates as table
  output$rates_table_diag <- DT::renderDataTable({
    sum_rates_diag()},
    options = list(
      pageLength = 5,
      scrollX = TRUE,
      scrollY = TRUE,
      orderClasses = TRUE)
  )
  
  ## Plot props
  output$plot_props_diag <- renderPlotly({
    plot <- sum_props_diag() %>% 
      graph_proportions
    
    ggplotly(plot)
  })
  ##Prop as table
  output$props_table_diag <- DT::renderDataTable({
    sum_props_diag()
  },
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = TRUE,
    orderClasses = TRUE)
  )
  
  ##Model Trajectories as table
  output$traj_table_diag <- DT::renderDataTable({
    model_traj_diag()
  },
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = TRUE,
    orderClasses = TRUE)
  )

  ## Cumulative measures
  cum_traj <- reactive({

    cum_measures(model_traj_diag(), diag_params())
  })
  
  ##Cum as table
  output$cum_table <- DT::renderDataTable({
    cum_traj()
  },
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = TRUE,
    orderClasses = TRUE)
  )
  
  ## Plot cum
  output$plot_cum <- renderPlotly({
    plot <- cum_traj() %>% 
      graph_cum
    
    ggplotly(plot)
  })
  
  ## Diagnoses measures
  sum_diag_traj <- reactive({
    if (input$burn_in_diag) {
      start_time <- 0
    }else{
      start_time <- 2010
    }
    
    sum_diag(model_traj_diag(), start_time = start_time)
  })
  
  ##Cum as table
  output$diag_table <- DT::renderDataTable({
    sum_diag_traj()
  },
  options = list(
    pageLength = 5,
    scrollX = TRUE,
    scrollY = TRUE,
    orderClasses = TRUE)
  )
  
  ## Plot cum
  output$plot_diag <- renderPlotly({
    plot <- sum_diag_traj() %>% 
      graph_diag
    
    ggplotly(plot)
  })
  
  output$downloadData2 <- downloadHandler(filename = "ui.R",
                                          content = function(file) {
                                            file.copy("ui.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData3 <- downloadHandler(filename = "server.R",
                                          content = function(file) {
                                            file.copy("server.R", file, overwrite = TRUE)
                                            }
                                          )
  output$downloadData4 <- downloadHandler(filename = "TB_model.R",
                                          content = function(file) {
                                            file.copy("TB_model.R", file, overwrite = TRUE)
                                          }
  )
  output$downloadData5 <- downloadHandler(filename = "graph_tb_model.R",
                                          content = function(file) {
                                            file.copy("graph_tb_model.R", file, overwrite = TRUE)
                                          }
  )
  
  output$downloadData6 <- downloadHandler(filename = "tb_model_diag.R",
                                          content = function(file) {
                                            file.copy("tb_model_diag.R", file, overwrite = TRUE)
                                          }
  )
  
  output$downloadData7 <- downloadHandler(filename = "graph_tb_model_diag.R",
                                          content = function(file) {
                                            file.copy("graph_tb_model_diag.R", file, overwrite = TRUE)
                                          }
  )
  
})
