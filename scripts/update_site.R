# Update site.

# Authors: Sara Altman, Bill Behrman
# Version: 2020-06-16

# Libraries
library(tidyverse)

# Parameters
  # Directory for site
dir_site <- here::here("docs")

#===============================================================================

cli::cat_line(cli::rule(str_glue("Build site")))
options(blogdown.publishDir = dir_site)
blogdown::build_site(local = TRUE)

cli::cat_line(cli::rule(str_glue("Push site to GitHub")))
system("git add docs")
system("git commit -m 'Update site'")
system("git push")
