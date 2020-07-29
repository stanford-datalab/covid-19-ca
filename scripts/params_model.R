# Calculate model parameters

# Author: Bill Behrman
# Version: 2020-07-25

# Libraries
library(tidyverse)
library(rstanarm)

# Parameters
  # CPS Food Security Supplement data
file_cps <- here::here("data-raw/cps_fss.rds")
  # Output file with model parameters
file_params_model <- here::here("data/params_model.yml")

#===============================================================================

# State FIPS codes
fips <-
  tidycensus::fips_codes %>%
  distinct(area = state_name, fips = state_code) %>%
  arrange(fips)

# Read in CPS Food Security Supplement data
cps <-
  file_cps %>%
  read_rds() %>%
  select(
    year,
    statefip,
    fsstatus,
    fsstatusc,
    fssuppwt,
    age
  ) %>%
  mutate(
    age_group =
      case_when(
        age %in% c("under 1 year", 1:17) ~ "0 - 17",
        age %in% c(18:89, "90 (90+, 1988-2002)") ~ "18+",
        TRUE ~ NA_character_
      )
  )

# Calculate food insecurity among adults by state and year
adults <-
  cps %>%
  filter(
    fsstatus %in% c("food secure", "low food secure", "very low food secure"),
    age_group == "18+"
  ) %>%
  group_by(area = str_to_title(statefip), year) %>%
  summarize(
    adult_total = sum(fssuppwt),
    adult_insecure_n =
      sum(fssuppwt[fsstatus %in% c("low food secure", "very low food secure")]),
    adult_insecure_pct = 100 * adult_insecure_n / adult_total
  ) %>%
  ungroup() %>%
  left_join(fips, by = "area") %>%
  relocate(fips, .after = area)

# Calculate food insecurity among children by state and year
children <-
  cps %>%
  filter(
    fsstatus %in% c("food secure", "low food secure", "very low food secure"),
    age_group == "0 - 17"
  ) %>%
  group_by(area = str_to_title(statefip), year) %>%
  summarize(
    child_total = sum(fssuppwt),
    child_insecure_n =
      sum(
        fssuppwt[
          fsstatusc %in%
            c(
              "low food security among children",
              "very low food security among children"
            )
        ]
      ),
    child_insecure_pct = 100 * child_insecure_n / child_total
  ) %>%
  ungroup() %>%
  left_join(fips, by = "area") %>%
  relocate(fips, .after = area)

# Combine adult and child data
adults_children <-
  adults %>%
  left_join(children, by = c("area", "fips", "year")) %>%
  arrange(fips, year)

# Calculate coefficients to linear model of child food insecurity as a function
# of adult food insecurity
child_model_coefs <-
  stan_glm(
    formula = child_insecure_pct ~ adult_insecure_pct,
    data = adults_children,
    refresh = 0
  ) %>%
  coef() %>%
  set_names(c("intercept", "slope"))

# Save model parameters
list(
  child_model =
    list(
      intercept = child_model_coefs[["intercept"]],
      slope = child_model_coefs[["slope"]]
    )
) %>%
  yaml::write_yaml(file_params_model)
