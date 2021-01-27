# Calculate estimates from raw Household Pulse data.

# Author: Bill Behrman
# Version: 2021-01-26

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
  # Household Pulse PUF variables
file_vars <- str_c(dir_pulse_puf, "/metadata/vars.txt")
  # Household Pulse PUF variables to estimate for all respondents
file_vars_all <- str_c(dir_pulse_puf, "/metadata/vars_all.txt")
  # Household Pulse PUF variables to estimate for food insecure respondents
file_vars_curfoodsuf_34 <-
  str_c(dir_pulse_puf, "/metadata/vars_curfoodsuf_34.txt")
  # Data recodes
file_recodes <- str_c(dir_pulse_puf, "/metadata/recodes.csv")
  # Output file
file_out <- here::here("data/state.csv")

#===============================================================================

# Parameters for state and data
params <- yaml::read_yaml(file_params)

# Household Pulse weeks
weeks <-
  file_weeks %>%
  read_csv(
    col_types =
      cols(
        year = col_double(),
        week = col_double(),
        date_start = col_date(format = ""),
        date_end = col_date(format = ""),
        release_table = col_date(format = ""),
        release_puf = col_date(format = "")
      )
  )

# Household Pulse PUF variables
vars <- read_lines(file_vars)

# Household Pulse PUF variables to estimate for all respondents
vars_all <- read_lines(file_vars_all)

# Household Pulse PUF variables to estimate for food insecure respondents
vars_curfoodsuf_34 <- read_lines(file_vars_curfoodsuf_34)

# Data recodes
recodes <- read_csv(file_recodes, col_types = cols(.default = col_character()))

# Files with PUF raw data
files_puf <-
  fs::dir_ls(path = dir_pulse_puf, recurse = TRUE, regexp = "wk\\d+\\.rds$")

# Files with table data
files_table <-
  fs::dir_ls(path = dir_pulse_table, recurse = TRUE, regexp = "wk\\d+\\.rds$")

# Estimate number of adults for each variable response
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

# Read in raw PUF data and calculate estimates for state
process_puf <- function(file) {

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
  data <- read_rds(file)
  vars_data <- names(data)
  data <-
    data %>%
    select(all_of(intersect(vars_data, vars)))
  assertthat::assert_that(
    n_distinct(data$week) == 1,
    msg = message("Data does not contain one week")
  )
  assertthat::assert_that(
    n_distinct(data$est_st) == 1,
    msg = message("Data does not contain one state")
  )
  assertthat::assert_that(
    "curfoodsuf" %in% names(data),
    msg = "Data does not contain variable: curfoodsuf"
  )
  assertthat::assert_that(
    (
      weeks %>%
        filter(year == year_, week == unique(data$week)) %>%
        nrow()
    ) == 1,
    msg = message("Survey week not in weeks metadata")
  )

  # Estimate number of total adults for state
  state_total <-
    data %>%
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
  state_total_n <-
    state_total %>%
    pull(n)

  # Estimate number of adults for each variable response for state and
  # calculate percentages
  state <-
    data %>%
    pivot_longer(
      cols = all_of(intersect(vars_data, vars_all)),
      names_to = "variable",
      values_to = "code"
    ) %>%
    mutate(code = code %>% na_if(-88) %>% na_if(-99)) %>%
    estimate(week, est_st, variable, code) %>%
    rename(fips = est_st) %>%
    mutate(area_type = "State") %>%
    bind_rows(state_total) %>%
    mutate(
      pct =
        case_when(
          str_detect(variable, "^wherefree\\d$") & code %in% 1 ~
            100 * n / n[variable == "freefood" & code %in% 1],
          str_detect(variable, "^mortlmth$") & code %in% 1:3 ~
            100 * n / sum(n[variable == "tenure" & code %in% 2:3]),
          str_detect(variable, "^mortconf$") & code %in% 1:5 ~
            100 * n / sum(n[variable == "tenure" & code %in% 2:3]),
          is.na(code) ~ 100 * n / state_total_n,
          TRUE ~ NA_real_
        )
    ) %>%
    group_by(variable) %>%
    mutate(
      pct =
        if_else(
          !is.na(pct),
          pct,
          100 * n / (state_total_n - sum(n[is.na(code)]))
        )
    ) %>%
    ungroup()

  # Estimate number of adults for each variable response who responded to the
  # food insecurity question
  state_food_insecure_1234 <-
    data %>%
    filter(curfoodsuf %in% 1:4) %>%
    pivot_longer(
      cols = all_of(intersect(vars_data, vars_curfoodsuf_34)),
      names_to = "variable",
      values_to = "code"
    ) %>%
    mutate(
      variable = variable %>% str_replace("$", "_curfoodsuf_1234"),
      code = code %>% na_if(-88) %>% na_if(-99)
    ) %>%
    estimate(week, est_st, variable, code) %>%
    rename(fips = est_st) %>%
    mutate(area_type = "State") %>%
    mutate(pct = 100)

  # Estimate number of food insecure adults for each variable response
  state_food_insecure_34 <-
    data %>%
    filter(curfoodsuf %in% 3:4) %>%
    pivot_longer(
      cols = all_of(intersect(vars_data, vars_curfoodsuf_34)),
      names_to = "variable",
      values_to = "code"
    ) %>%
    mutate(
      variable = variable %>% str_replace("$", "_curfoodsuf_34"),
      code = code %>% na_if(-88) %>% na_if(-99)
    ) %>%
    estimate(week, est_st, variable, code) %>%
    rename(fips = est_st) %>%
    mutate(area_type = "State")

  # Calculate percentage of food insecure adults for each variable response
  state_food_insecure_34_pct <-
    state_food_insecure_34 %>%
    select(variable, code, n) %>%
    left_join(
      state_food_insecure_1234 %>%
        mutate(
          variable = str_replace(variable, "_curfoodsuf_1234", "_curfoodsuf_34")
        ) %>%
        select(variable, code, n_total = n),
      by = c("variable", "code")
    ) %>%
    mutate(pct = 100 * n / n_total) %>%
    select(!c(n, n_total))

  # Add percentage to number of food insecure adults for each variable response
  state_food_insecure_34 <-
    state_food_insecure_34 %>%
    left_join(state_food_insecure_34_pct, by = c("variable", "code"))

  # Combine estimates and recode
  state %>%
    bind_rows(state_food_insecure_1234) %>%
    bind_rows(state_food_insecure_34) %>%
    left_join(
      recodes %>%
        filter(variable %in% c("est_st", "est_msa")) %>%
        select(fips = code, area = recode),
      by = "fips"
    ) %>%
    left_join(
      recodes %>%
        filter(!variable %in% c("est_st", "est_msa")) %>%
        bind_rows(
          .,
          mutate(., variable = variable %>% str_replace("$", "_curfoodsuf_34")),
          mutate(
            .,
            variable = variable %>% str_replace("$", "_curfoodsuf_1234")
          )
        ) %>%
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
  str_sort(numeric = TRUE) %>%
  map_dfr(process_puf)

# Read in table data and restrict to dates not in PUF data
table <-
  files_table %>%
  map_dfr(read_rds) %>%
  filter(area == params$state, !date_end %in% unique(puf$date_end)) %>%
  transmute(
    area_type = "State",
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
