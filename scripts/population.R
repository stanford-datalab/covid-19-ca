# Download population data from the U.S. Census Bureau.

# Sources:
# Population Estimates Program (PEP) APIs:
# https://www.census.gov/data/developers/data-sets/popest-popproj/popest.html
# American Community Survey 5-year (ACS) APIs:
# https://www.census.gov/data/developers/data-sets/acs-5year.html

# Author: Sara Altman, Bill Behrman
# Version: 2020-07-14

# Libraries
library(tidyverse)
library(yaml)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # Request from PEP population for state
api_pep_state <-
  "https://api.census.gov/data/{params$year_pep}/pep/population?get=NAME,POP&for=state:{params$state_fips}"
  # Request from PEP population for counties
api_pep_counties <-
  "https://api.census.gov/data/{params$year_pep}/pep/population?get=NAME,POP&for=county:*&in=state:{params$state_fips}"
  # Request from ACS data for proportion of population 18 and older for state
api_acs_state <-
  "https://api.census.gov/data/{params$year_acs}/acs/acs5/subject?get=NAME,S0101_C01_001E,S0101_C01_026E&for=state:{params$state_fips}"
  # Request from ACS data for proportion of population 18 and older for counties
api_acs_counties <-
  "https://api.census.gov/data/{params$year_acs}/acs/acs5/subject?get=NAME,S0101_C01_001E,S0101_C01_026E&for=county:*&in=state:{params$state_fips}"
  # Household Pulse metropolitan statistical areas
file_msas <- here::here("data/msas.yml")
  # Output file
file_out <- here::here("data/population.csv")

#===============================================================================

# Get parameters for state and data
params <- read_yaml(file_params)

# Get population from U.S. Census Bureau through API
get_population <- function(api_request) {
  str_glue(api_request) %>%
    jsonlite::fromJSON() %>%
    as_tibble(.name_repair = "minimal") %>%
    janitor::row_to_names(row_number = 1) %>%
    mutate(across(starts_with(c("POP", "S0101")), as.double))
}

# Get from PEP populations for state and counties
population_pep <-
  bind_rows(
    get_population(api_pep_state) %>%
      transmute(
        area_type = "State",
        area = NAME,
        fips = state,
        year = params$year_pep,
        population = POP
      ),
    get_population(api_pep_counties) %>%
      transmute(
        area_type = "County",
        area = NAME %>% str_remove(",.*"),
        fips = str_c(state, county),
        year = params$year_pep,
        population = POP
      ) %>%
      arrange(fips)
  )

# Get from ACS proportion of population 18 and older for state and counties
population_acs <-
  bind_rows(
    get_population(api_acs_state) %>%
      transmute(
        area = NAME,
        fips = state,
        prop_18p = S0101_C01_026E / S0101_C01_001E
      ),
    get_population(api_acs_counties) %>%
      transmute(
        area = NAME %>% str_remove(",.*"),
        fips = str_c(state, county),
        prop_18p = S0101_C01_026E / S0101_C01_001E
      ) %>%
      arrange(fips)
  )

# Combine PEP and ACS data for state and counties
state_counties <-
  population_pep %>%
  left_join(population_acs %>% select(-area), by = "fips") %>%
  mutate(
    population_18p = round(prop_18p * population),
    population_0_17 = population - population_18p
  ) %>%
  select(
    area_type,
    area,
    fips,
    year,
    population,
    population_0_17,
    population_18p
  )

# Counties in population data
counties <-
  state_counties %>%
  filter(area_type == "County") %>%
  pull(area) %>%
  unique() %>%
  sort()

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
        "These MSA counties do not match counties in population data:",
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

# Calculate population data for MSAs and balance of state
regions <-
  state_counties %>%
  filter(area_type == "County") %>%
  select(-fips) %>%
  left_join(county_region, by = "area") %>%
  group_by(area = region, fips, year) %>%
  summarize(
    area_type = "Region",
    across(starts_with("population"), sum)
  ) %>%
  relocate(area_type)

# Combine unemployment data for state, counties, and regions, and save
state_counties %>%
  bind_rows(regions) %>%
  arrange(desc(area_type), fips) %>%
  write_csv(file_out)
