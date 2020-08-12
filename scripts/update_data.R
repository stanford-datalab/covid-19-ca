# Update data

# Author: Bill Behrman
# Version: 2020-07-29

# Parameters
  # Script to update unemployment data
script_unemployment <- here::here("scripts/unemployment.R")
  # Script to download new Household Pulse PUF data
script_pulse_download_puf <- here::here("scripts/pulse_download_puf.R")
  # Script to download new Household Pulse table date
script_pulse_download_tables <- here::here("scripts/pulse_download_tables.R")
  # Script to calculate state data
script_state <- here::here("scripts/state.R")
  # Script to calculate county data
script_counties <- here::here("scripts/counties.R")

#===============================================================================

Sys.time()

cli::cat_line(cli::rule("Updating unemployment data"))
source(script_unemployment)

cli::cat_line(cli::rule("Downloading new Household Pulse PUF data"))
source(script_pulse_download_puf)

cli::cat_line(cli::rule("Downloading new Household Pulse table data"))
source(script_pulse_download_tables)

cli::cat_line(cli::rule("Calculating state data"))
source(script_state)

cli::cat_line(cli::rule("Calculating county data"))
source(script_counties)

cli::cat_line(cli::rule("Pushing new data to GitHub"))
system("git add -f data")
system("git commit -m 'Update data'")
system("git push")

Sys.time()
