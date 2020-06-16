# Build script

# Author: Sara Altman
# Version: 2020-06-11

# Libraries
library(tidyverse)

# Parameters
  # Parameter file
file_params <- here::here("data/params.yml")
  # File with counties
file_counties <- here::here("data/counties.csv")
  # County page template
file_template_county <- here::here("templates/county.Rmd")
  # State page template
file_template_state <- here::here("templates/state.Rmd")
  # About page template
file_template_about <- here::here("templates/about.Rmd")
  # Output directory
dir_output <- here::here("content")

#===============================================================================

# Counties
counties <-
  file_counties %>%
  read_csv() %>%
  pull(area) %>%
  unique()

# County files
county_files <-
  counties %>%
  str_remove(" County$") %>%
  str_to_lower() %>%
  str_replace_all(" ", "-") %>%
  fs::path(dir_output, ., ext = "Rmd")

# Templates
template_county <-
  file_template_county %>%
  read_file()

template_state <-
  file_template_state %>%
  read_file()

template_about <-
  file_template_about %>%
  read_file()

create_page <- function(area, path, template) {
  template %>%
    str_replace_all(
      c(
        "params:\n  area: \"\"" =
          str_c("params: \n", str_glue("area: \"{area}\""), sep = "  "),
        "title: \"\"" = str_glue("title: \"{area}\"")
      )
    ) %>%
    write_file(path)
}

walk2(
  counties,
  county_files,
  create_page,
  template = template_county
)

create_page(
  area = yaml::read_yaml(file_params)$state,
  path =
    fs::path(
      dir_output,
      "state",
      ext = "Rmd"
    ),
  template = template_state
)

template_about %>%
  read_file() %>%
  write_file(fs::path(dir_output, "about", ext = "Rmd"))
