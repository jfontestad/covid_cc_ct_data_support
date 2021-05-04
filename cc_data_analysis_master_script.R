#### Purpose: Prepare COVID-19 care coordination data for analysis and Tableau visualization
#### MASTER SCRIPT
# Eli Kern, PHSKC (APDE)
# 2021-05

##### Load libraries and specify file paths #####
options(max.print = 350, tibble.print_max = 50, scipen = 999)
origin <- "1970-01-01" # Date origin
Sys.setenv(TZ="America/Los_Angeles") # Set Time Zone
pacman::p_load(openxlsx, tidyverse, tidyselect, lubridate, blastula, glue)

#### Specify function for proper rounding ####
# this works as you would expect!
# https://stackoverflow.com/questions/12688717/round-up-from-5
round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/care_coordination_data_analysis/load_kc_data.R?token=ABL6ZKSHWJKCAE7W332BUGLASGXVS")
