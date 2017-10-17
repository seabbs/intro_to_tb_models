## Graphs Implemented
## Graph 1: Incidence in pops
## Graphs to make
## Graph 2: cum cases since the start of the intervention
## Graph 3: Total new diagnosis
## Graph 4: Number of smear exams and xrays in sputum negative populations
## Graph 5: total cumulative costs 
## Graph 6: cost per_diagnosed case

## Thoughts about graphs
## Graph 2, 5, and 6 only need data from the start of the intervention
## 

graph_cum <- function(df){
  plot <- df %>%
    gather(key = "Measure", value = "Cumulative Count",
           Cases, Infections, Deaths, Diagnoses) %>% 
    ggplot(aes(x = Year, y = `Cumulative Count`,
               colour = Measure, group = Measure)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    expand_limits(y = 0)
  
  return(plot)  
}

graph_diag <- function(df) {
  plot <- df %>%
    gather(key = "Disease", value = "New Diagnoses",
           `Sputum negative`, `Sputum positive`, `TB cases`) %>% 
    ggplot(aes(x = Year, y = `New Diagnoses`,
               colour = Disease, group = Disease)) +
    geom_line() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    expand_limits(y = 0)
  
  return(plot)  
}