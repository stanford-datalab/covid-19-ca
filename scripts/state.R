# Calculate estimates from raw Household Pulse data.

# Author: Bill Behrman
# Version: 2020-06-12

# Libraries
library(tidyverse)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # Household Pulse weeks
file_weeks <- here::here("data-raw/pulse/metadata/weeks.csv")
  # Directory with Household Pulse PUF raw data
dir_pulse_puf <- here::here("data-raw/pulse/puf")
  # Directory with Household Pulse table data
dir_pulse_table <- here::here("data-raw/pulse/tables")
  # All variables
file_vars_all <- str_c(dir_pulse_puf, "/metadata/vars_all.txt")
  # Variables to create estimates for
file_vars_estimate <- str_c(dir_pulse_puf, "/metadata/vars_estimate.txt")
  # Data recodes
file_recodes <- str_c(dir_pulse_puf, "/metadata/recodes.csv")
  # Population data
file_population <- here::here("data/population.csv")
  # Output file
file_out <- here::here("data/state.csv")

#===============================================================================

# Parameters for state and data
params <- yaml::read_yaml(file_params)

# Household Pulse weeks
weeks <- read_csv(file_weeks)

# All variables
vars_all <- read_lines(file_vars_all)

# Variables to create estimates for
vars_estimate <- read_lines(file_vars_estimate)

# Data recodes
recodes <- read_csv(file_recodes, col_types = cols(.default = col_character()))

# Population data for state
population <-
  read_csv(file_population) %>%
  filter(area_type %in% c("State"))

# Files with PUF raw data
files_puf <-
  fs::dir_ls(path = dir_pulse_puf, recurse = TRUE, regexp = "wk\\d+\\.rds$")

# Files with table data
files_table <-
  fs::dir_ls(path = dir_pulse_table, recurse = TRUE, regexp = "wk\\d+\\.rds$")

# Estimate number of individuals for each variable response
estimate <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarize(across(starts_with("pweight"), sum)) %>%
    ungroup() %>%
    rename(n = pweight) %>%
    rowwise() %>%
    mutate(
      n_error = sqrt(0.05 * sum((n - c_across(starts_with("pweight")))^2))
    ) %>%
    ungroup() %>%
    select(..., n, n_error)
}

# Read in raw data and calculate estimates for state
process <- function(file) {

  cli::cat_line(
    cli::rule(
      str_glue('Processing: {str_remove(file, str_c(dir_pulse_puf, "/"))}')
    )
  )

  # Survey year
  year_ <-
    file %>%
    str_extract("20../wk.*rds$") %>%
    str_extract("^20\\d{2}") %>%
    as.integer()

  # Read in raw data
  v_wide <-
    read_rds(file) %>%
    select(all_of(vars_all))
  v_long <-
    v_wide %>%
    pivot_longer(
      cols = all_of(vars_estimate),
      names_to = "variable",
      values_to = "code"
    ) %>%
    mutate(code = code %>% na_if(-88) %>% na_if(-99))

  assertthat::assert_that(
    n_distinct(v_wide$week) == 1,
    msg = message("Data does not contain one week")
  )
  assertthat::assert_that(
    n_distinct(v_wide$est_st) == 1,
    msg = message("Data does not contain one state")
  )
  assertthat::assert_that(
    (
      weeks %>%
        filter(year == year_, week == unique(v_wide$week)) %>%
        nrow()
    ) == 1,
    msg = message("Survey week not in weeks metadata")
  )

  # Estimate number of total individuals for state
  state_total <-
    v_wide %>%
    estimate(week, est_st) %>%
    transmute(
      week,
      fips = est_st,
      variable = "total",
      code = NA_integer_,
      n,
      n_error,
      area_type = "State"
    )

  # Estimate number of individuals for each variable response for state
  state <-
    v_long %>%
    estimate(week, est_st, variable, code) %>%
    rename(fips = est_st) %>%
    mutate(area_type = "State")

  # Combine estimates for state, calculate percentages, recode, and adjust
  # numbers of individuals to populations for state and regions
  state_total %>%
    bind_rows(state) %>%
    group_by(fips) %>%
    mutate(
      pct =
        case_when(
          str_detect(variable, "^foodsufrsn\\d$") & code %in% 1 ~
            100 * n /
            sum(n[variable == "curfoodsuf" & code %in% 2:4]),
          str_detect(variable, "^wherefree\\d$") & code %in% 1 ~
            100 * n / n[variable == "freefood" & code %in% 1],
          str_detect(variable, "^mortlmth$") & code %in% 1:3 ~
            100 * n / sum(n[variable == "tenure" & code %in% 2:3]),
          str_detect(variable, "^mortconf$") & code %in% 1:5 ~
            100 * n / sum(n[variable == "tenure" & code %in% 2:3]),
          TRUE ~ 100 * n / n[variable == "total"]
        )
    ) %>%
    ungroup() %>%
    left_join(
      recodes %>%
        filter(variable %in% c("est_st", "est_msa")) %>%
        select(fips = code, area = recode),
      by = "fips"
    ) %>%
    left_join(
      recodes %>%
        filter(!variable %in% c("est_st", "est_msa")) %>%
        transmute(
          variable,
          code = as.integer(code),
          response = recode
        ),
      by = c("variable", "code")
    ) %>%
    left_join(
      weeks %>%
        filter(year == year_) %>%
        select(-year),
      by = "week"
    ) %>%
    left_join(
      population %>%
        select(fips, population_18p),
      by = "fips"
    ) %>%
    group_by(fips) %>%
    mutate(
      n = n * population_18p / n[variable == "total"],
      n_error = n_error * population_18p / n[variable == "total"]
    ) %>%
    ungroup() %>%
    mutate(across(c(n, n_error), round)) %>%
    select(
      area_type,
      area,
      fips,
      date_start,
      date_end,
      variable,
      code,
      response,
      n,
      n_error,
      pct
    ) %>%
    arrange(fips, variable, code)
}

# Calculate estimates for all PUF files
puf <-
  files_puf %>%
  map_dfr(process)

# Read in table data, restrict to dates not in PUF data, and adjust numbers of
# individuals to populations for state
table <-
  files_table %>%
  map_dfr(read_rds) %>%
  filter(area == params$state, !date_end %in% unique(puf$date_end)) %>%
  left_join(
    population %>%
      select(fips, population_18p),
    by = "fips"
  ) %>%
  group_by(fips) %>%
  mutate(
    area_type = "State",
    n = (n * population_18p / n[variable == "total"]) %>% round()
  ) %>%
  ungroup() %>%
  select(
    area_type,
    area,
    fips,
    date_start,
    date_end,
    variable,
    code,
    response,
    n,
    n_error,
    pct
  )

# Combine table and PUF data and save
table %>%
  bind_rows(puf) %>%
  arrange(desc(date_end), fips, variable, code) %>%
  write_csv(file_out)
