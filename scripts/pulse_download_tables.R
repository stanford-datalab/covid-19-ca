# Download table data from Household Pulse Survey.

# Source: https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html

# Author: Bill Behrman
# Version: 2020-10-07

# Libraries
library(tidyverse)
library(readxl)
library(rvest)

# Parameters
  # Parameters for state and data
file_params <- here::here("data/params.yml")
  # Base URL for Household Pulse table data
url_pulse <-
  "https://www2.census.gov/programs-surveys/demo/tables/hhp/{params$year_pulse}"
  # CSS for data directories
css_pulse <- "body > table"
  # Household Pulse weeks
file_weeks <- here::here("data-raw/pulse/metadata/weeks.csv")
  # Directory for Household Pulse table data
dir_pulse <- here::here("data-raw/pulse/tables")
  # Household Pulse table sheets
file_sheets <- str_c(dir_pulse, "/metadata/sheets.csv")
  # Household Pulse table variables
file_vars <- str_c(dir_pulse, "/metadata/vars.csv")
  # Data recodes
file_recodes <- here::here("data-raw/pulse/puf/metadata/recodes.csv")
  # Temporary directory
dir_tmp <- str_glue("{tempdir()}/pulse")
  # Output directory
dir_out <- str_c(dir_pulse, "/{params$year_pulse}")

#===============================================================================

# Parameters for state and data
params <- yaml::read_yaml(file_params)

# Create temporary directory
fs::dir_create(dir_tmp)

# Household Pulse weeks
weeks <- read_csv(file_weeks)

# Household Pulse table sheets for state
sheets <-
  read_csv(file_sheets) %>%
  filter(fips_state == params$state_fips) %>%
  select(-fips_state)

# Household Pulse table variables
vars <- read_csv(file_vars, na = "")

# Data recodes
recodes <-
  read_csv(
    file_recodes,
    col_types = cols(.default = col_character()),
    na = ""
  )

# If it doesn't already exist, create output directory
fs::dir_create(str_glue(dir_out))

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
  fs::dir_ls(str_glue(dir_pulse, "/{params$year_pulse}")) %>%
  str_extract("wk\\d+")
weeks_download <- setdiff(weeks_pulse, weeks_local)
assertthat::assert_that(
  all(
    (weeks_download %>% str_remove("^wk") %>% as.integer()) %in%
    (weeks %>% filter(year == params$year_pulse) %>% pull(week))
  ),
  msg = "Missing Pulse week metadata"
)

# Read cell from sheet
read_cell <- function(data, category_, response_, curfoodsuf_) {
  v <-
    data %>%
    filter(
      category == category_,
      response == response_,
      curfoodsuf == curfoodsuf_
    ) %>%
    pull(n)
  assertthat::assert_that(
    length(v) == 1,
    msg = message("Invalid query to read_cell()")
  )
  v
}

# Read sheet for state or region
read_sheet <- function(week, sheet) {
  v <-
    read_excel(
      path = str_glue("{dir_tmp}/{week}/food2b.xlsx"),
      col_types = "text",
      sheet = sheet,
      skip = if_else(parse_number(week) <= 12, 4, 5)
    ) %>%
    rename(
      response = "...1",
      Total = "...2",
      `1` = "Enough of the types of food wanted",
      `2` = "Enough food, but not always the types wanted",
      `3` = "Sometimes not enough to eat",
      `4` = "Often not enough to eat",
      `NA` = "Did not report"
    ) %>%
    filter(!str_detect(response, "^\\*"), !is.na(response)) %>%
    mutate(response = str_remove(response, "[ *]*$")) %>%
    pivot_longer(cols = -response, names_to = "curfoodsuf", values_to = "n") %>%
    mutate(
      n =
        n %>%
        str_replace(pattern = "^-$", replacement = "0") %>%
        as.double()
    ) %>%
    group_by(response) %>%
    mutate(
      category =
        if_else(
          first(response) == "Total" || all(is.na(n)),
          first(response),
          NA_character_
        )
    ) %>%
    ungroup() %>%
    fill(category) %>%
    drop_na(n) %>%
    select(category, response, curfoodsuf, n)

  vars %>%
    pmap_dfr(
      ~ tibble(
        variable = ..1,
        code = ..2,
        n =
          read_cell(
            data = v,
            category_ = ..3,
            response_ = ..4,
            curfoodsuf_ = ..5
          )
      )
    ) %>%
    mutate(
      week = week %>% str_remove("wk") %>% as.integer(),
      sheet = sheet,
      n_error = NA_real_,
      pct =
        case_when(
          str_detect(variable, "^curfoodsuf$") & code %in% 1:4 ~
            100 * n /
            sum(n[variable == "curfoodsuf" & code %in% 1:4]),
          str_detect(variable, "^foodsufrsn\\d$") & code %in% 1 ~
            100 * n /
            sum(n[variable == "curfoodsuf" & code %in% 2:4]),
          str_detect(variable, "^wherefree\\d$") & code %in% 1 ~
            100 * n / n[variable == "freefood" & code %in% 1],
          str_detect(variable, "^mortlmth$") & code %in% 1:3 ~
            NA_real_,
          str_detect(variable, "^mortconf$") & code %in% 1:5 ~
            NA_real_,
          TRUE ~ 100 * n / n[variable == "total"]
        )
    ) %>%
    left_join(sheets, by = "sheet") %>%
    left_join(weeks %>% filter(year == params$year_pulse), by = "week") %>%
    left_join(
      recodes %>%
        rename(response = recode),
      by = c("variable", "code")
    ) %>%
    mutate(code = code %>% na_if("NA") %>% as.integer()) %>%
    select(
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
}

# Download data for week
download <- function(week) {

  cli::cat_line(cli::rule(str_glue("Downloading: {week}")))

  # Download spreadsheet
  file <-
    str_glue(url_pulse, "/{week}") %>%
    read_html(css = css_pulse) %>%
    html_table() %>%
    pluck(2) %>%
    unlist() %>%
    keep(~ str_detect(., "^food2b") & !str_detect(., "_se_"))
  url <- str_glue(url_pulse, "/{week}/{file}")
  dest <- str_glue("{dir_tmp}/{week}/food2b.xlsx")
  fs::dir_create(str_glue("{dir_tmp}/{week}"))
  result <- download.file(url = url, destfile = dest, quiet = TRUE)
  assertthat::assert_that(
    result == 0L,
    msg = message("Download failed")
  )

  # Read sheets for state, combine, and save
  sheets$sheet %>%
    map_dfr(~ read_sheet(week, .)) %>%
    arrange(fips, variable, code) %>%
    write_rds(str_glue(dir_out, "/{week}.rds"))
}

# Download data for weeks that have not been downloaded
weeks_download %>%
  walk(download)

# Remove temporary directory
fs::dir_delete(dir_tmp)
