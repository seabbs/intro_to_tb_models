library(tidyverse)

graph_rates <- function(df){
  plot <- df %>%
    gather(key = "Summary measure", value = "Incidence rate (per 100,000)",
           `Annual TB incidence rate`, `TB mortality rate`) %>% 
    ggplot(aes(x = Year, y = `Incidence rate (per 100,000)`,
                      colour = `Summary measure`, group = `Summary measure`)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    expand_limits(y = 0)
  
  return(plot)  
}

graph_annual_risk <- function(df) {
  plot <-  df %>% 
    ggplot(aes(x = Year, y = `Annual risk of infection`, colour = `Annual risk of infection`)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "none") +
    expand_limits(y = 0)
  
  return(plot)
}

graph_proportions <- function(df) {
  plot <- df %>% 
    gather(key = "Summary measures", value = "Proportions",
           `Recent infection`, `Recent reinfection`, Reactivation) %>% 
    ggplot(aes(x = Year, y = Proportions, colour = `Summary measures`, group = `Summary measures`)) +
    geom_line() + 
    theme_minimal() +
    theme(legend.position = "bottom") +
    expand_limits(y = 0)
  
  return(plot)
}