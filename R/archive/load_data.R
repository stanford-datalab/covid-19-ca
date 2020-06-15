# Read in necessary data, load libraries etc.

# Author: Sara Altman
# Version: 2020-06-09

# Libraries
library(tidyverse)

# Parameters
file_params <- here::here("../california-dev/data/params.yml")
file_counties <- here::here("../california-dev/data/counties.csv")
file_state_regions <- here::here("../california-dev/data/state_regions.csv")
file_unemployment <- here::here("../california-dev/data/unemployment.csv")
file_population <- here::here("../california-dev/data/population.csv")
codes_food_insecure <- c(3, 4)
file_msas <- here::here("../california-dev/data/msas.yml")
#===============================================================================

# Helper functions

create_period <- function(date_start, date_end) {
  str_glue("{format(date_start, '%b %d')}-{format(date_end, '%b %d')}") %>% 
    fct_reorder(date_start)
}


# Parameters and metadata
params <-
  file_params %>% 
  yaml::read_yaml()

msas <-
  file_msas %>% 
  yaml::read_yaml() %>% 
  keep(~ .$state == params$state)

# Populations
population <-
  file_population %>% 
  read_csv(
    col_type =
      cols(
        area_type = col_character(),
        area = col_character(),
        fips = col_character(),
        year = col_double(),
        population = col_double(),
        population_0_17 = col_double(),
        population_18p = col_double()
      )
  )

# State
state_regions <- 
  file_state_regions %>% 
  read_csv(
    col_types = 
      cols(
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
  mutate(period = create_period(date_start, date_end))

state <-
  state_regions %>% 
  filter(fips == params$state_fips)

state_food <-
  state %>% 
  filter(
    variable == "curfoodsuf",
    code %in% codes_food_insecure
  )

state_pre_covid <-
  state %>% 
  filter(variable == "prifoodsuf", code %in% codes_food_insecure) %>% 
  group_by(date_end) %>% 
  summarize(across(c(n, pct), sum)) %>% 
  summarize(across(c(n, pct), median))

# Regions
regions <-
  state_regions %>% 
  filter(fips != params$state_fips)

regions_food <-
  regions %>% 
  filter(
    variable == "curfoodsuf",
    code %in% codes_food_insecure
  )

# Counties
counties_food <- 
  file_counties %>% 
  read_csv(
    col_types = 
      cols(
        area = col_character(),
        fips = col_character(),
        region = col_character(),
        date_start = col_date(format = ""),
        date_end = col_date(format = ""),
        variable = col_character(),
        code = col_double(),
        response = col_character(),
        n = col_double()
      )
  ) %>% 
  left_join(population %>% select(fips, population_18p), by = "fips") %>% 
  mutate(
    pct = (n / population_18p) * 100,
    period = create_period(date_start, date_end)
  )

# Unemployment
unemployment <-
  file_unemployment %>% 
  read_csv(
    col_types = 
      cols(
        area_type = col_character(),
        area = col_character(),
        fips = col_character(),
        date = col_date(format = ""),
        status = col_character(),
        labor_force = col_double(),
        employment = col_double(),
        unemployment = col_double(),
        unemployment_rate = col_double()
      )
  ) 

# Latest update

last_week <- 
  state %>% 
  slice_max(order_by = date_end) %>% 
  pull(period) %>% 
  unique()

# Other
work <-
  state %>% 
  filter(
    variable == "wrkloss",
    response == "Yes"
  )

# latest_food <-
#   food %>% 
#   slice_max(order_by = date_end) %>% 
#   summarize(
#     pct = sum(pct, na.rm = TRUE),
#     n = sum(n, na.rm = TRUE)
#   )
# 
# latest_work <-
#   work %>% 
#   slice_max(order_by = date_end) 

pre_covid <-
  state %>% 
  filter(variable == "prifoodsuf", code %in% codes_food_insecure) %>% 
  group_by(date_end) %>% 
  summarize(across(c(n, pct), sum)) %>% 
  summarize(across(c(n, pct), median))

levels_periods <-
  c("Prior to March 13, 2020", levels(state$period))
