# Data from most recent CPS Food Security Supplement

# Author: Bill Behrman
# Version: 2020-06-25

# Libraries
library(tidyverse)

# Parameters
  # CPS Food Security Supplement microdata
file_cps_fss <- here::here("data-raw/cps_fss.rds")
  # Output file
file_out <- here::here("data/cps_fss.csv")

#===============================================================================

# Data from most recent CPS Food Security Supplement
file_cps_fss %>%
  read_rds() %>%
  filter(year == max(year)) %>%
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
    variable = "fsfoods",
    n = sum(fssuppwt)
  ) %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  relocate(response, .after = variable) %>%
  write_csv(file_out)
