# Download PUF data from Household Pulse Survey.

# Source: https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html

# Author: Bill Behrman
# Version: 2020-09-23

# Libraries
library(tidyverse)
library(rvest)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # Base URL for Household Pulse PUF data
url_pulse <-
  "https://www2.census.gov/programs-surveys/demo/datasets/hhp/{params$year_pulse}"
  # CSS for data directories
css_pulse <- "body > table"
  # Temporary directory
dir_tmp <- str_glue("{tempdir()}/pulse")
  # Output directory for Household Pulse PUF data
dir_pulse <- here::here("data-raw/pulse/puf/{params$year_pulse}")

#===============================================================================

# Parameters for state and data
params <- yaml::read_yaml(file_params)

# Create temporary directory
fs::dir_create(dir_tmp)

# If it doesn't already exist, create output directory
fs::dir_create(str_glue(dir_pulse))

# Determine weeks that have not been downloaded
weeks_pulse <-
  str_glue(url_pulse) %>%
  read_html(css = css_pulse) %>%
  html_table() %>%
  pluck(2) %>%
  unlist() %>%
  map_chr(str_extract, pattern = "wk\\d+") %>%
  discard(is.na)
weeks_local <-
  fs::dir_ls(str_glue(dir_pulse)) %>%
  str_extract("wk\\d+")
weeks <- setdiff(weeks_pulse, weeks_local)

# Download data for week
download <- function(week) {

  cli::cat_line(cli::rule(str_glue("Downloading: {week}")))

  # URL for data file
  file_pulse <-
    str_glue(url_pulse, "/{week}") %>%
    read_html(css = css_pulse) %>%
    html_table() %>%
    pluck(2) %>%
    unlist() %>%
    str_extract(pattern = "^HPS_.*_PUF_CSV") %>%
    discard(is.na)
  url <- str_glue(url_pulse, "/{week}/{file_pulse}.zip")

  # Download and unzip data file
  dest <- str_glue("{dir_tmp}/{file_pulse}.zip")
  result <- download.file(url = url, destfile = dest, quiet = TRUE)
  assertthat::assert_that(
    result == 0L,
    msg = message("Download failed")
  )
  unzip(zipfile = dest, exdir = str_glue("{dir_tmp}/{file_pulse}"))

  # Read in data
  data <-
    fs::dir_ls(
      path = str_glue("{dir_tmp}/{file_pulse}"),
      regexp = str_glue("pulse{params$year_pulse}_puf_.*csv")
    ) %>%
    read_csv(
      col_types =
        cols(
          .default = col_double(),
          SCRAM = col_character(),
          EST_ST = col_character(),
          EST_MSA = col_character()
        )
    ) %>%
    janitor::clean_names(case = "snake") %>%
    filter(est_st == params$state_fips) %>%
    mutate(
      rhispanic_rrace = 10L * rhispanic + rrace,
      ahispanic_arace = 10L * ahispanic + arace
    ) %>%
    relocate(rhispanic_rrace, ahispanic_arace, .after = arace)
  vars_integer <-
    names(data) %>%
    setdiff(c("scram", "est_st", "est_msa", "hweight", "pweight"))
  data <-
    data %>%
    mutate(across(all_of(vars_integer), as.integer))

  # Read in replication weights
  weights <-
    fs::dir_ls(
      path = str_glue("{dir_tmp}/{file_pulse}"),
      regexp = str_glue("pulse{params$year_pulse}_repwgt_puf_.*csv")
    ) %>%
    read_csv(
      col_types =
        cols(
          .default = col_double(),
          SCRAM = col_character(),
          WEEK = col_integer()
        )
    ) %>%
    janitor::clean_names(case = "snake")

  assertthat::assert_that(
    all(data$scram %in% weights$scram),
    msg = message("Not all respondents have replication weights")
  )

  # Combine data and replication weights and save
  data %>%
    left_join(weights, by = c("scram", "week")) %>%
    write_rds(str_glue(dir_pulse, "/{week}.rds"), compress = "gz")
}

# Download data for weeks that have not been downloaded
weeks %>%
  walk(download)

# Remove temporary directory
fs::dir_delete(dir_tmp)
