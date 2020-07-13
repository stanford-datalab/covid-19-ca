# Download employment data from the Bureau of Labor Statistics.

# Source: https://download.bls.gov/pub/time.series/la
# Documentation: https://download.bls.gov/pub/time.series/la/la.txt

# Authors: Sara Altman, Bill Behrman
# Version: 2020-06-19

# Libraries
library(tidyverse)
library(rvest)
library(yaml)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # Base URL for Bureau of Labor Statistics unemployment data
url_bls <- "https://download.bls.gov/pub/time.series/la/"
  # Area codes
url_area <- str_c(url_bls, "la.area")
  # Measure codes
url_measure <- str_c(url_bls, "la.measure")
  # Seasonal codes
url_seasonal <- str_c(url_bls, "la.seasonal")
  # Area types to include
area_types <-
  tribble(
    ~ area_type_code, ~ area_type, ~ n_fips,
                 "A",     "State",        2,
                 "F",    "County",        5
  )
  # Measures to include
measures <- c("labor force", "employment", "unemployment", "unemployment rate")
  # Seasonal values to include
seasonals <- "Not Seasonally Adjusted"
  # Household Pulse metropolitan statistical areas
file_msas <- here::here("data/msas.yml")
  # Output file
file_out <- here::here("data/unemployment.csv")

#===============================================================================

# Parameters for state and data
params <- read_yaml(file_params)

# URL for file with unemployment data for state and counties
url_state <-
  str_c(
    url_bls,
    url_bls %>%
      read_html() %>%
      html_text() %>%
      str_extract(str_c("la.data.\\d+.", params$state %>% str_remove(" ")))
  )

# Download and write out employment data for state and counties

area <-
  url_area %>%
  read_tsv(
    col_types =
      cols_only(
        area_type_code = col_character(),
        area_code = col_character(),
        area_text = col_character()
      )
  ) %>%
  rename(area = area_text)

measure <-
  url_measure %>%
  read_tsv(col_types = cols(.default = col_character())) %>%
  rename(measure = measure_text)

seasonal <-
  url_seasonal %>%
  read_tsv(col_types = cols(.default = col_character())) %>%
  rename(seasonal = seasonal_text)

state_counties <-
  url_state %>%
  read_tsv(
    col_types =
      cols(
        series_id = col_character(),
        year = col_integer(),
        period = col_character(),
        value = col_double(),
        footnote_codes = col_character()
      )
  ) %>%
  rename(period_code = period) %>%
  extract(
    col = series_id,
    into = c("seasonal_code", "area_code", "measure_code"),
    regex = "^..(.)(.{15})(..)$"
  ) %>%
  left_join(area, by = "area_code") %>%
  filter(area_type_code %in% area_types$area_type_code) %>%
  left_join(area_types, by = "area_type_code") %>%
  left_join(measure, by = "measure_code") %>%
  left_join(seasonal, by = "seasonal_code") %>%
  filter(
    measure %in% measures,
    seasonal %in% seasonals
  ) %>%
  mutate(
    area = str_remove(area, ",.*$") %>% str_remove("/city"),
    fips = str_sub(area_code, 3, n_fips + 2),
    month =
      if_else(
        period_code %in% str_glue('M{str_pad(1:12, width = 2, pad = "0")}'),
        str_sub(period_code, 2) %>% as.integer(),
        NA_integer_
      ),
    date = lubridate::make_date(year = year, month = month),
    status =
      if_else(
        !is.na(footnote_codes) & str_detect(footnote_codes, "P"),
        "Preliminary",
        "Final"
      ),
    measure = measure %>% str_replace_all("[ -]", "_")
  ) %>%
  filter(date >= params$date_bls) %>%
  select(area_type, area, fips, date, status, measure, value) %>%
  pivot_wider(
    names_from = measure,
    values_from = value
  ) %>%
  relocate(
    labor_force,
    employment,
    unemployment,
    unemployment_rate,
    .after = last_col()
  )

# Counties in unemployment data
counties <-
  state_counties %>%
  filter(area_type == "County") %>%
  pull(area) %>%
  unique() %>%
  sort()

# Report most recent month and county coverage for most recent month
v <-
  state_counties %>%
  filter(area_type == "County", date == max(date))
message(str_glue("Most recent month: {first(v$date)}"))
if (setequal(v$area, counties)) {
  message(str_glue("Month {first(v$date)} has data for all counties"))
} else {
  message(
    str_glue(
      "Month {first(v$date)} has data for {n_distinct(v$area)} of {length(counties)} counties"
    )
  )
}

# Household Pulse metropolitan statistical areas for state
msas <-
  yaml::read_yaml(file_msas) %>%
  map_dfr(
    ~ tibble(
      state = .$state,
      region = .$msa,
      fips = .$fips,
      area = .$counties
    )
  ) %>%
  filter(state == params$state) %>%
  select(-state)
assertthat::assert_that(
  all(msas$area %in% counties),
  msg =
    str_c(
      c(
        "These MSA counties do not match counties in unemployment data:",
        setdiff(msas$area, counties)
      ),
      collapse = "\n  "
    )
)

# Partition state into regions
if (nrow(msas) == 0) {
  county_region <-
    state_counties %>%
    filter(area_type == "County") %>%
    distinct(area) %>%
    mutate(
      region = "Balance",
      fips = NA_character_
    )
} else {
  county_region <-
    state_counties %>%
    filter(area_type == "County") %>%
    distinct(area) %>%
    left_join(msas, by = "area") %>%
    replace_na(list(region = "Balance"))
}

# Calculate unemployment data for MSAs and balance of state
regions <-
  state_counties %>%
  filter(area_type == "County") %>%
  select(-fips) %>%
  left_join(county_region, by = "area") %>%
  group_by(area = region, fips, date) %>%
  summarize(
    area_type = "Region",
    status = if_else(n_distinct(status) == 1, first(status), NA_character_),
    labor_force = sum(labor_force),
    employment = sum(employment),
    unemployment = sum(unemployment),
    unemployment_rate = (100 * unemployment / labor_force) %>% round(digits = 1)
  ) %>%
  relocate(area_type)

# Combine unemployment data for state, counties, and regions, and save
state_counties %>%
  bind_rows(regions) %>%
  arrange(desc(date), desc(area_type), fips) %>%
  write_csv(file_out)
