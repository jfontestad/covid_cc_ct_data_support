#### Purpose: Export analytic datasets
# Eli Kern, PHSKC (APDE)
# 2021-05

#Set file paths
analytic_export <- "//Phshare01/cdi_share/Outbreaks & Investigations/Outbreaks 2020/2019-nCoV/Contact Tracing/Epi/Care Coordination/Analytic Datasets/"

#### STEP 1: Export household-level dataset and long-form WDRS for analytic purposes ####
data_ct_analytic <- list(final_data_by_household, complete_cc_wdrs)
sheet_ct_analytic <- list("complete_referrals_by_household", "complete_wdrs_long")
filename_ct_analytic <- file.path(analytic_export, "complete_referrals_analytic_dataset.xlsx")
system.time(write.xlsx(data_ct_analytic, file = filename_ct_analytic, sheetName = sheet_ct_analytic))

#### STEP 2: Export CHW analytic dataset ####
data_chw_analytic <- list(complete_chw_analytic)
sheet_chw_analytic <- list("complete_chw_analytic")
filename_chw_analytic <- file.path(analytic_export, "complete_referrals_chw_analytic_dataset.xlsx")
system.time(write.xlsx(data_chw_analytic, file = filename_chw_analytic, sheetName = sheet_chw_analytic))

#### STEP 3: Export HAR analytic dataset ####
data_har_analytic <- list(complete_har_form_with_demo)
sheet_har_analytic <- list("complete_har_analytic")
filename_har_analytic <- file.path(analytic_export, "complete_har_analytic_dataset.xlsx")
system.time(write.xlsx(data_har_analytic, file = filename_har_analytic, sheetName = sheet_har_analytic))