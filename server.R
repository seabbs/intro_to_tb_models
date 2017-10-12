#Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(deSolve)


##Load code
source("TB_model.R")
source("graph_tb_model.R")

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
  
})
