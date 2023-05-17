# Libraries
library(tidyverse)

# Parameters
  # Household Pulse data for state
file_hps <- here::here("data/state.csv")
  # Number of digits to right of decimal point
nsmall = 6

#===============================================================================

# Household Pulse data for state
hps <- 
  file_hps %>% 
  read_csv(
    col_types = 
      cols(
        area_type = col_character(),
        area = col_character(),
        fips = col_character(),
        date_start = col_date(format = ""),
        date_end = col_date(format = ""),
        variable = col_character(),
        code = col_double(),
        response = col_character(),
        n = col_double(),
        n_error = col_double(),
        pct = col_double()
      )
  ) %>% 
  filter(area_type == "State")

# Calculate and print SNAP and WIC rates
hps %>%
  filter(str_detect(variable, "^fdbenefit")) %>% 
  group_by(date_end, variable) %>% 
  mutate(pct = 100 * n / sum(n)) %>% 
  ungroup() %>% 
  filter(code == 1) %>% 
  mutate(
    label = str_extract(response, "^\\w+"),
    pct = format(pct, nsmall = nsmall)
  ) %>% 
  select(date_start, date_end, label, pct) %>% 
  arrange(label, desc(date_end)) %>% 
  print(n = Inf)
