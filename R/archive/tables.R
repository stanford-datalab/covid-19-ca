# Table helper functions

# Author: Sara Altman
# Version: 2020-06-09

# Libraries
library(tidyverse)
library(reactable)

# Parameters
script_load_data <- here::here("R/load_data.R")
#===============================================================================

source(script_load_data)

food_insecurity_table <- function(data) {
  data %>% 
    mutate(
      n = format(n, big.mark = ","),
      pct = str_c(round(pct, 1), "%")
    ) %>% 
    arrange(desc(date_end)) %>% 
    select(
      Week = period,
      Individuals = n,
      Percent = pct
    ) %>% 
    reactable()
}
