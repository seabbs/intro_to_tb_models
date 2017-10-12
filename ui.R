## Load packages
library(shiny)
library(shinydashboard)
library(shinyBS)
library(tidyverse)
library(rmarkdown)
library(plotly)
library(DT)

##Load code
source("TB_model.R")
source("graph_tb_model.R")

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "menu",
              menuItem("TB model", tabName = "tb-model", icon = icon("line-chart")),
              menuItem("About", tabName = "readme", icon = icon("mortar-board"), selected = TRUE),
              menuItem("Code",  icon = icon("code"),
                       menuSubItem("Github", href = "https://github.com/seabbs/intro_to_tb_models", icon = icon("github")),
                       menuSubItem("ui.R", tabName = "ui", icon = icon("angle-right")),
                       menuSubItem("server.R", tabName = "server", icon = icon("angle-right"))
              )
  ),
  conditionalPanel(condition = 'input.menu == "tb-model"',
                   sliderInput("ecr",
                               "Effective Contacts (per year)",
                               min = 0, 
                               max = 20,
                               value = 15),
                   sliderInput("wks_inf_p", 
                               "Avg. No. of Weeks Infectious (Positive Sputum Smear)",
                               min = 0,
                               max = 102,
                               value = 52 ),
                   sliderInput("wks_inf_n", 
                               "Avg. No. of Weeks Infectious (Negative Sputum Smear)",
                               min = 0,
                               max = 190,
                               value = 95),
                   sliderInput("prot_init_reint",
                               "Protection from reinfection relative to infection",
                               min = 0, 
                               max = 1, 
                               value = 0.65),
                   checkboxInput("burn_in",
                                 "Show burn in",
                                 value = FALSE)
                   
                   
  ),
  hr(),
  helpText("Developed by ", a("Sam Abbott", href = "http://samabbott.co.uk"), 
           style = "padding-left:1em; padding-right:1em;position:absolute; bottom:1em; ")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "tb-model",
            fluidRow(
              tabBox(width = 12, 
                     title = "Model Plots", 
                     side = "right",
                     tabPanel(title = "Rates",
                              plotlyOutput("plot_rates")
                              ),
                     tabPanel(title = "Annual Risk",
                              plotlyOutput("plot_annual_risk")
                     ),
                     tabPanel(title = "Proportions",
                              plotlyOutput("plot_props")
                     )
              ),
                     tabBox(width = 12, 
                            title = "Model Statistics", 
                            side = "right",
                            tabPanel(title = "Rates and Annual Risk",
                                     DT::dataTableOutput("rates_table")
                            ),
                            tabPanel(title = "Proportions", 
                                     DT::dataTableOutput("props_table")
                            ),
                            tabPanel(title = "Trajectory",
                                     DT::dataTableOutput("traj_table")
                            )
                            )
              )
            ),
    tabItem(tabName = "readme",
            withMathJax(), 
            includeMarkdown("README.md")
    ),
    tabItem(tabName = "ui",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "UI",
                 downloadButton('downloadData2', 'Download'),
                 br(),br(),
                 pre(includeText("ui.R"))
            )
    ),
    tabItem(tabName = "server",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "Server",
                 downloadButton('downloadData3', 'Download'),
                 br(),br(),
                 pre(includeText("server.R"))
            )
    )
  )
)

dashboardPage(
  dashboardHeader(title = "Intro to TB Models"),
  sidebar,
  body,
  skin = "black"
)