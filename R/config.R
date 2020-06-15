# Generates config text for county page
# Paste the output into config.toml

# Author: Sara Altman
# Version: 2020-06-03

# Libraries
library(tidyverse)

# Parameters
file_params <- here::here("../california-dev/data/params.yml")
file_counties <- here::here("../california-dev/data/counties.csv")
file_msas <- here::here("../california-dev/data/msas.yml")
#===============================================================================

print_config <- function(dropdown, areas) {
  urls <-
    areas %>% 
    str_to_lower() %>% 
    str_replace_all(" ", "-")
  
  str_glue(
    '\n[[menu.main]]\n\tparent = "{dropdown}"\n\tname = "{areas}"\n\turl = "/{urls}"', 
  ) %>% 
    str_c(collapse = "\n") %>% 
    cat()
}

state <- yaml::read_yaml(file_params)$state

msas <-
  file_msas %>% 
  yaml::read_yaml() %>% 
  keep(~ .$state == state) %>% 
  map_chr("msa") %>% 
  sort() %>% 
  append("Balance")

counties <- 
  file_counties %>% 
  read_csv() %>% 
  distinct(area) %>% 
  arrange(area) %>% 
  pull(area) %>% 
  str_remove(" County") 

# Copy into config.toml
print_config("Regions", msas)
print_config("Counties", counties)




