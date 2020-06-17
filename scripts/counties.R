# Estimate food insecurity for counties.

# Author: Sara Altman, Bill Behrman
# Version: 2020-06-11

# Libraries
library(tidyverse)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # Population data
file_population <- here::here("data/population.csv")
  # Household Pulse weeks
file_weeks <- here::here("data-raw/pulse/metadata/weeks.csv")
  # Household Pulse data for state and regions
file_state <- here::here("data/state.csv")
  # Unemployment data
file_unemployment <- here::here("data/unemployment.csv")
  # Output file
file_out <- here::here("data/counties.csv")

#===============================================================================

# Parameters for state and data
params <- yaml::read_yaml(file_params)

# Population data for counties
population <-
  read_csv(file_population) %>%
  filter(area_type == "County")

# Household Pulse weeks
weeks <- read_csv(file_weeks)

# Household Pulse food insecurity data for state and regions
state <-
  read_csv(file_state) %>%
  filter(area_type == "State", variable == "curfoodsuf", code %in% 3:4) %>%
  select(-n_error, -pct)

# Unemployment data for counties
unemployment <-
  read_csv(file_unemployment) %>%
  filter(area_type == "County")

# Counties in unemployment data
counties <-
  unemployment %>%
  pull(area) %>%
  unique() %>%
  sort()

# Restrict unemployment data to dates for which there is data for all counties
unemployment <-
  unemployment %>%
  group_by(date) %>%
  filter(setequal(area, counties)) %>%
  ungroup()

# Determine unemployment dates closest to Household Pulse Survey dates
dates_pulse <-
  state %>%
  distinct(date_start, date_end) %>%
  arrange(date_end)
dates_unemployment <-
  unique(unemployment$date) %>%
  sort()
date_closest <-
  tibble(
    date_end = dates_pulse$date_end,
    date =
      map(date_end, ~ max(dates_unemployment[dates_unemployment <= .])) %>%
      reduce(c)
  )

# Distribute x in proportion to y, round to whole numbers
distribute <- function(x, y) {
  assertthat::assert_that(length(x) == 1 && length(y) >= 1)
  x <- round(x)
  z <- round(x * y / sum(y))
  i_max <- which.max(z)
  z[i_max] <- x - sum(z[-i_max])
  z
}

# Distribute Household Pulse variables to counties by unemployment
distribute_state <- function(date_end_, data) {

  v <-
    state %>%
    filter(date_end == date_end_) %>%
    select(date_start, variable, code, response, n_state = n)
  assertthat::assert_that(
    nrow(v) >= 1,
    msg =
      str_glue(
        "Invalid Household Pulse data for:\n  ",
        "date_end: {date_end_}\n"
      )
  )

  data %>%
    left_join(v, by = "date_start") %>%
    group_by(variable, code) %>%
    mutate(n = distribute(first(n_state), unemployment)) %>%
    ungroup() %>%
    transmute(
      area,
      fips,
      date_start,
      date_end = date_end_,
      variable,
      code,
      response,
      n
    )
}

# Estimate food insecurity for counties
unemployment %>%
  distinct(area, fips) %>%
  expand_grid(date_start = dates_pulse$date_start) %>%
  left_join(dates_pulse, by = "date_start") %>%
  left_join(date_closest, by = "date_end") %>%
  left_join(
    unemployment %>% select(fips, date, unemployment),
    by = c("fips", "date")
  ) %>%
  group_by(date_end) %>%
  nest() %>%
  pmap_df(distribute_state) %>%
  left_join(
    population %>% select(fips, population_18p),
    by = "fips"
  ) %>%
  mutate(pct = (100 * n / population_18p) %>% round(digits = 1)) %>%
  select(-population_18p) %>%
  arrange(desc(date_end), fips, variable, code) %>%
  write_csv(file_out)
