# Update site.

# Authors: Sara Altman, Bill Behrman
# Version: 2021-03-01

# Parameters
  # Script to generate site RMDs
script_build <- here::here("R/build.R")
  # Directory for site
dir_site <- here::here("docs")

#===============================================================================

Sys.time()

cli::cat_line(cli::rule("Building site"))
source(script_build)
options(blogdown.hugo.args = "--noTimes", blogdown.publishDir = dir_site)
blogdown::build_site(local = TRUE, build_rmd = TRUE)

cli::cat_line(cli::rule("Pushing site to GitHub"))
cli::cat_line(cli::rule("    Adding"))
system("git add docs")
cli::cat_line(cli::rule("    Committing"))
system("git commit -m 'Update site'")
cli::cat_line(cli::rule("    Pushing"))
system("git push")

Sys.time()
