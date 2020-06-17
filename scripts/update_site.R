# Update site.

# Authors: Sara Altman, Bill Behrman
# Version: 2020-06-16

# Parameters
  # Directory for site
dir_site <- here::here("docs")

#===============================================================================

cli::cat_line(cli::rule("Building site"))
options(blogdown.publishDir = dir_site)
blogdown::build_site(local = TRUE)

cli::cat_line(cli::rule("Pushing site to GitHub"))
cli::cat_line(cli::rule("    Adding"))
system("git add docs")
cli::cat_line(cli::rule("    Committing"))
system("git commit -m 'Update site'")
cli::cat_line(cli::rule("    Pushing"))
system("git push")
