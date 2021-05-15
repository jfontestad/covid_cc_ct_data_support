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
if(qa_1 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/load_doh_data.R")}

#QA: Check to make sure DOH data loaded
if(exists("doh_cc_current")) {
  qa_2 <- TRUE
} else {
  qa_2 <- FALSE
}


#### STEP 3: Load COVID-19 case data exported daily by A&I, and load a weekly dataset maintained by CT ####
if(qa_1 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/load_case_data.R")}

#QA: Check to make sure DOH data loaded
if(exists("wdrs_final") &
   wdrs_final_rows_per_case == 1) {
  qa_3 <- TRUE
} else {
  qa_3 <- FALSE
}


#### STEP 4: Clean and combine case, contact and household COVID-19-related data for PHSKC-assigned and DOH-assigned households ####
if(qa_3 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/clean_ct_data.R")}

#QA: Check to make sure all data has been processed, and that household data is distinct at household level
if(exists("complete_cc_household") &
   exists("complete_cc_wdrs") &
   kc_cc_household_row_count == 1 &
   doh_cc_household_row_count == 1) {
  qa_4 <- TRUE
} else {
  qa_4 <- FALSE
}


#### STEP 5: Clean and combine referral data for PHSKC-assigned and DOH-assigned households, include demographic data ####
if(qa_4 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/clean_referral_data.R")}

#QA: Check to make sure all referral data has been processed, and that demographic data is distinct at household level
if(exists("complete_referral_form_with_demo") &
   exists("complete_referrals_by_household") &
   kc_referral_based_demo_distinct_row_percent == 100 &
   doh_referral_based_demo_distinct_row_percent == 100) {
  qa_5 <- TRUE
} else {
  qa_5 <- FALSE
}

#### STEP 6: Clean and combine HAR referral data for PHSKC-assigned and DOH-assigned households, include demographic data ####
if(qa_5 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/clean_referral_data_har_subset.R")}

#QA: Check to make sure all referral data has been processed, and that demographic data is distinct at household level
if(exists("complete_har_form_with_demo") &
   complete_har_form_qa_check == 0) {
  qa_6 <- TRUE
} else {
  qa_6 <- FALSE
}


#### STEP 7: Combine cleaned case/contact/household and referral data ####
if(qa_6 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/combine_ct_referral_data.R")}

#QA: Check to make sure all data has been processed, and that demographic data is distinct at household level
if(exists("final_data_by_household") &
   final_data_by_household_row_count == 1) {
  qa_7 <- TRUE
} else {
  qa_7 <- FALSE
}


#### STEP 8: Create CHW-level datasets for tabulation and analysis ####
if(qa_7 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/chw_level_data.R")}

#QA: Check to make sure all data has been processed, and that analytic dataset is distinct at agency, arm, record_id, referral_timedt level 
if(exists("complete_referrals_by_chw") &
   exists("complete_chw_analytic") &
   complete_analytic_distinct_row_percent == 100) {
  qa_8 <- TRUE
} else {
  qa_8 <- FALSE
}


#### STEP 9: Export analytic datasets ####
if(qa_8 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/export_analytic.R")}


#### STEP 10: Tabulate and export for Tableau ####
if(qa_8 == TRUE) {devtools::source_url("https://raw.githubusercontent.com/PHSKC-APDE/covid_cc_ct_data_support/main/export_tableau.R")}

#QA: Check to make tabulated data has been processed
if(exists("complete_referrals_by_ct_date_tableau") &
   exists("complete_referrals_by_referral_date_tableau")) {
  qa_9 <- TRUE
} else {
  qa_9 <- FALSE
}


#### STEP 11: Prepare final QA message and send email ####

# Set up email credentials (have to run this once to store credentials securely using keyring package, must be rerun each time password is updated)
# create_smtp_creds_key(id = "outlook", user = "eli.kern@kingcounty.gov", provider = "outlook", overwrite = T)

#Get a nicely formatted date/time string
date_time <- add_readable_time()
date <- Sys.Date()

#Check final QA check
if(qa_9 == TRUE) {
  qa_final <- "PASS"
} else {
  qa_final <- "FAIL"
}

#Create email body using dynamic code
if (qa_final == "PASS") {
  qa_result <- "**PASS**"
  qa_checkpoint_message <- "7 of 7 checkpoints completed."
  
  chw_analytic_file_demo_message <- paste0("CHW referral demographic QA for referrals between **", demo_qa_start, " - ", demo_qa_end, ":**")
  kc_analytic_file_message_race <- paste0("* PHSKC-assigned CHW referrals missing race/ethnicity: ",
                                           kc_raceethnicity_missing_count, " referrals (",
                                           kc_raceethnicity_missing_per, "%)")
  kc_analytic_file_message_language <- paste0("* PHSKC-assigned CHW referrals missing language: ",
                                              kc_language_missing_count, " referrals (",
                                              kc_language_missing_per, "%)")
  doh_analytic_file_message_race <- paste0("* DOH-assigned CHW referrals missing race/ethnicity: ",
                                          doh_raceethnicity_missing_count, " referrals (",
                                          doh_raceethnicity_missing_per, "%)")
  doh_analytic_file_message_language <- paste0("* DOH-assigned CHW referrals missing language: ",
                                              doh_language_missing_count, " referrals (",
                                              doh_language_missing_per, "%)")
} else {
  qa_result <- "**FAIL**"
  qa_checkpoint_message <- NULL
  chw_analytic_file_demo_message <- NULL
  kc_analytic_file_message_race <- NULL
  kc_analytic_file_message_language <- NULL
  doh_analytic_file_message_race <- NULL
  doh_analytic_file_message_language <- NULL
}

#Set subject of email
subject <- paste0("QA result: ", qa_result, ", COVID-19 Care Coordination metrics, ", date)

# Compose email
email <-
  compose_email(
    body = md(
      c("Automated update to: ",
        "* COVID-19 Care Coordination metrics datasets", "\n",
        "* complete_referrals_analytic_dataset.xlsx", "\n",
        "* complete_referrals_chw_analytic_dataset.xlsx", "\n",
        "* complete_har_analytic_dataset.xlsx", "\n",
        "QA result: ", qa_result, "\n",
        qa_checkpoint_message, "\n",
        chw_analytic_file_demo_message, "\n",
        kc_analytic_file_message_race, "\n",
        kc_analytic_file_message_language, "\n",
        doh_analytic_file_message_race, "\n",
        doh_analytic_file_message_language, "\n"
      )),
    footer = md(
      c("Email sent on ", date_time, ".")
    )
  )

# Send email
smtp_send(email = email,
          #to = c("eli.kern@kingcounty.gov"),
          to = c("eli.kern@kingcounty.gov", "krijohnson@kingcounty.gov", "SHernandez@kingcounty.gov"),
          from = creds_key("outlook")$user,
          subject = subject,
          credentials = creds_key("outlook")
)