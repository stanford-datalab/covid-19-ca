# Download population data from the U.S. Census Bureau.

# Sources:
# Household Pulse Survey (HPS)
# https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html
# Population Estimates Program (PEP) APIs:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest.html

# Author: Sara Altman, Bill Behrman
# Version: 2020-07-26

# Libraries
library(tidyverse)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # PEP API base URL for age group populations
api_pep_base <-
  "https://api.census.gov/data/{params$year_pep}/pep/charagegroups"
  # PEP age group populations for state
api_pep_state <-
  str_c(api_pep_base, "?get=NAME,AGEGROUP,POP&for=state:{params$state_fips}")
  # PEP age group populations for counties
api_pep_counties <-
  str_c(
    api_pep_base,
    "?get=NAME,AGEGROUP,POP&for=county:*&in=state:{params$state_fips}"
  )
  # PEP age group recode
recode_age_group <-
  c(
    "0" = "population",
    "19" = "population_0_17",
    "29" = "population_18p"
  )
  # URL for HPS PUF data
url_hps <-
  "https://www2.census.gov/programs-surveys/demo/datasets/hhp/2020/wk1/HPS_Week01_PUF_CSV.zip"
  # Temporary directory
dir_tmp <- str_glue("{tempdir()}/hps")
  # Output file
file_out <- here::here("data/population.csv")

#===============================================================================

# Create temporary directory
fs::dir_create(dir_tmp)

# Get parameters for state and data
params <- yaml::read_yaml(file_params)

# Get PEP child and adult populations
get_pep <- function(api_request, recode_age_group) {
  str_glue(api_request) %>%
    jsonlite::fromJSON() %>%
    as_tibble(.name_repair = "minimal") %>%
    janitor::row_to_names(row_number = 1) %>%
    filter(AGEGROUP %in% names(recode_age_group)) %>%
    mutate(
      AGEGROUP = recode(AGEGROUP, !!!recode_age_group),
      POP = as.double(POP)
    ) %>%
    pivot_wider(names_from = AGEGROUP, values_from = POP)
}

# Distribute x in proportion to y, round to whole numbers
distribute <- function(x, y) {
  assertthat::assert_that(length(x) == 1 && length(y) >= 1)
  x <- round(x)
  z <- round(x * y / sum(y))
  i_max <- which.max(z)
  z[i_max] <- x - sum(z[-i_max])
  z
}

# Get PEP child and adult populations for state
pep_state <-
  get_pep(api_pep_state, recode_age_group) %>%
  rename(area = NAME, fips = state) %>%
  mutate(area_type = "State") %>%
  relocate(area_type)

# Get PEP child and adult populations counties
pep_counties <-
  get_pep(api_pep_counties, recode_age_group) %>%
  mutate(
    area_type = "County",
    area = NAME %>% str_remove(",.*"),
    fips = str_c(state, county)
  ) %>%
  select(area_type, area, fips, starts_with("population")) %>%
  arrange(fips)

# Download and unzip HPS PUF data
dest <- str_c(dir_tmp, "/file.zip")
result <- download.file(url = url_hps, destfile = dest, quiet = TRUE)
assertthat::assert_that(
  result == 0L,
  msg = message("Download failed")
)
unzip(zipfile = dest, exdir = dir_tmp)

# Read in HPS PUF data
hps <-
  fs::dir_ls(
    path = dir_tmp,
    regexp = str_glue("pulse{params$year_pulse}_puf_.*csv")
  ) %>%
  read_csv(
    col_types =
      cols_only(
        WEEK = col_double(),
        EST_ST = col_character(),
        PWEIGHT = col_double()
      )
  ) %>%
  rename_with(str_to_lower) %>%
  filter(est_st == params$state_fips)
stopifnot(
  n_distinct(hps$week) == 1,
  all(!is.na(hps$pweight)) && 0 < min(hps$pweight)
)

# Remove temporary directory
fs::dir_delete(dir_tmp)

# Calculate adult population for state used by HPS PUF
population_18p_hps <- round(sum(hps$pweight))

# Scale state populations using HPS PUF adult population
state <-
  pep_state %>%
  mutate(
    population = round(population * population_18p_hps / population_18p),
    population_18p = population_18p_hps,
    population_0_17 = population - population_18p
  )

# Adjust county populations to scaled state populations
counties <-
  pep_counties %>%
  mutate(
    population_0_17 = distribute(state$population_0_17, population_0_17),
    population_18p = distribute(state$population_18p, population_18p),
    population = population_0_17 + population_18p
  )

# Combine state and county populations and save
state %>%
  bind_rows(counties) %>%
  arrange(desc(area_type), fips) %>%
  write_csv(file_out)
