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


#### STEP 1: Load current and historical contact tracing and care coordination data for PHSKC-assigned cases ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/load_kc_data.R")

#QA: Compare columns in current and archived data
if(kc_cc_archive_current_coldiff == 0) {
  qa_1 <- TRUE
} else {
  qa_1 <- FALSE
}


#### STEP 2: Load care coordination data for DOH-assigned cases ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/load_doh_data.R")

#QA: Check to make sure DOH data loaded
if(exists("doh_cc_current")) {
  qa_2 <- TRUE
} else {
  qa_2 <- FALSE
}


#### STEP 3: Clean and combine case, contact and household COVID-19-related data for PHSKC-assigned and DOH-assigned households ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/clean_ct_data.R")

#QA: Check to make sure all data has been processed, and that household data is distinct at household level
if(exists("complete_cc_household") &
   exists("complete_cc_wdrs") &
   kc_cc_household_row_count == 1 &
   doh_cc_household_row_count == 1) {
  qa_3 <- TRUE
} else {
  qa_3 <- FALSE
}


#### STEP 4: Clean and combine referral data for PHSKC-assigned and DOH-assigned households, include demographic data ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/clean_referral_data.R")

#QA: Check to make sure all referral data has been processed, and that demographic data is distinct at household level
if(exists("complete_referral_form_with_demo") &
   exists("complete_referrals_by_household") &
   kc_referral_based_demo_distinct_row_percent == 100 &
   doh_referral_based_demo_distinct_row_percent == 100) {
  qa_4 <- TRUE
} else {
  qa_4 <- FALSE
}


#### STEP 5: Combine cleaned case/contact/household and referral data ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/clean_referral_data.R")

#QA: Check to make sure all data has been processed, and that demographic data is distinct at household level
if(exists("final_data_by_household") &
   final_data_by_household_row_count == 1) {
  qa_5 <- TRUE
} else {
  qa_5 <- FALSE
}


#### STEP 6: Create CHW-level datasets for tabulation and analysis ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/chw_level_data.R")

#QA: Check to make sure all data has been processed, and that analytic dataset is distinct at agency, arm, record_id, referral_timedt level 
if(exists("complete_referrals_by_chw") &
   exists("complete_chw_analytic") &
   complete_analytic_distinct_row_percent == 100) {
  qa_6 <- TRUE
} else {
  qa_6 <- FALSE
}

#### STEP 7: Export analytic datasets ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/export_analytic.R")


#### STEP 8: Tabulate and export for Tableau ####
devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/export_tableau.R")

#QA: Check to make tabulated data has been processed
if(exists("complete_referrals_by_ct_date_tableau") &
   exists("complete_referrals_by_referral_date_tableau")) {
  qa_7 <- TRUE
} else {
  qa_7 <- FALSE
}