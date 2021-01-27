# Historical unemployment and food insufficiency data from CPS FSS.

# Author: Bill Behrman
# Version: 2020-07-06

# Libraries
library(tidyverse)
library(lubridate)

# Parameters
  # Start year
YEAR_START <- 2001
  # Base URL for Bureau of Labor Statistics unemployment data
url_bls <- "https://download.bls.gov/pub/time.series/la/"
  # Unemployment data for states
url_states <- str_c(url_bls, "la.data.2.AllStatesU")
  # Area codes
url_area <- str_c(url_bls, "la.area")
  # Measure codes
url_measure <- str_c(url_bls, "la.measure")
  # Period codes
url_period <- str_c(url_bls, "la.period")
  # Seasonal codes
url_seasonal <- str_c(url_bls, "la.seasonal")
  # Areas to exclude
areas_exclude <- "Puerto Rico"
  # Months to include
months <- 12
  # Measures to include
measures <- c("labor force", "employment", "unemployment", "unemployment rate")
  # Seasonal values to include
seasonals <- "Not Seasonally Adjusted"
  # CPS Food Security Supplement microdata
file_cps_fss <- here::here("data-raw/cps_fss.rds")
  # Output file
file_out <- here::here("data/unemployment_food_insufficiency_fss.csv")

#===============================================================================

# Unemployment data

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

unemployment_states <-
  url_states %>%
  read_tsv(
    col_types =
      cols(
        series_id = col_character(),
        year = col_integer(),
        period = col_character(),
        value = col_double(),
        footnote_codes = col_character()
      ),
    na = "-"
  ) %>%
  filter(year >= YEAR_START) %>%
  rename(period_code = period) %>%
  extract(
    col = series_id,
    into = c("seasonal_code", "area_code", "measure_code"),
    regex = "^..(.)(.{15})(..)$"
  ) %>%
  left_join(area, by = "area_code") %>%
  left_join(measure, by = "measure_code") %>%
  left_join(seasonal, by = "seasonal_code") %>%
  mutate(
    fips = str_sub(area_code, 3, 4),
    month =
      if_else(
        period_code %in% str_glue('M{str_pad(1:12, width = 2, pad = "0")}'),
        str_sub(period_code, 2) %>% as.integer(),
        NA_integer_
      )
  ) %>%
  filter(
    !area %in% areas_exclude,
    month %in% months,
    measure %in% measures,
    seasonal %in% seasonals
  ) %>%
  mutate(measure = measure %>% str_replace_all("[ -]", "_")) %>%
  select(area, fips, year, month, measure, value) %>%
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

unemployment_us <-
  unemployment_states %>%
  group_by(year, month) %>%
  summarize(
    area = "United States",
    fips = "00",
    across(c(labor_force, employment, unemployment), sum),
    unemployment_rate = 100 * unemployment / labor_force
  ) %>%
  ungroup() %>%
  relocate(area, fips)

unemployment_fss <-
  unemployment_us %>%
  bind_rows(unemployment_states) %>%
  arrange(year, month, fips) %>%
  mutate(
    date = make_date(year = year, month = month),
    unemployment_rate = 100 * unemployment / labor_force
  ) %>%
  relocate(date, .after = fips) %>%
  select(-year, -month)

# Food insufficiency from CPS Food Security Supplement

fips <-
  tidycensus::fips_codes %>%
  distinct(area = state_name, fips = state_code) %>%
  arrange(fips)

fi_states <-
  file_cps_fss %>%
  read_rds() %>%
  filter(
    !fsstatus %in% c("niu", "no response"),
    fsfoods %in%
      c(
        "enough of the kinds of food we want to eat",
        "enough but not always the kinds of food we want to eat",
        "sometimes not enough to eat",
        "often not enough to eat"
      )
  ) %>%
  group_by(area = str_to_title(statefip), year, response = fsfoods) %>%
  summarize(
    month = 12,
    variable = "fsfoods",
    n = sum(fssuppwt)
  ) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  left_join(fips, by = "area") %>%
  relocate(area, fips, year, month, variable, response)

fi_us <-
  fi_states %>%
  group_by(year, month, variable, response) %>%
  summarize(
    area = "United States",
    fips = "00",
    n = sum(n)
  ) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  relocate(area, fips)

fi_fss <-
  fi_us %>%
  bind_rows(fi_states) %>%
  filter(str_detect(response, "not enough to eat")) %>%
  mutate(date = make_date(year = year, month = month)) %>%
  group_by(area, fips, date) %>%
  summarize(
    food_insufficiency_n = sum(n),
    food_insufficiency_pct = sum(pct)
  ) %>%
  ungroup() %>%
  arrange(date, fips)

assertthat::assert_that(
  all(fi_fss$date %in% unemployment_fss$date),
  msg = "Not all CPS FSS months have unemployment data"
)

# Combine unemployment and food insufficiency
unemployment_fss %>%
  inner_join(fi_fss %>% select(-area), by = c("fips", "date")) %>%
  arrange(date, fips) %>%
  write_csv(file_out)
