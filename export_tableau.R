#### Purpose: Tabulate for Tableau
# Eli Kern, PHSKC (APDE)
# 2021-05

#Set file paths
tableau_export <- "//Phshare01/cdi_share/Outbreaks & Investigations/Outbreaks 2020/2019-nCoV/Contact Tracing/Epi/Care Coordination/care_coordination_tableau_dashboard/"

#### STEP 1: Tabulate statistics by CT record creation date ####

## Tabulate with both agencies combined
ct_date_tabulate_both_agency <- final_data_by_household %>%
  group_by(form_created) %>%
  summarise(
    
    agency = "Both",
    
    #Household count
    household_count = n(),
    
    #Any referral
    any_referral_count = sum(any_referral, na.rm = T),
    
    #CT referral statistics
    ct_any_referral_count = sum(ct_any_referral, na.rm = T),
    ct_to_chw_count = sum(chw, na.rm = T),
    ct_swiq_count = sum(swiq, na.rm = T),
    ct_tpr_count = sum(tpr, na.rm = T),
    ct_groceries_count = sum(groceries, na.rm = T),
    ct_medconsult_count = sum(medconsult, na.rm = T),
    ct_iqmotel_count = sum(iqmotel, na.rm = T),
    ct_healthinsurance_count = sum(healthinsurance, na.rm = T),
    ct_healthcare_count = sum(healthcare, na.rm = T),
    ct_medical_cost_count = sum(medical_costs, na.rm = T),
    ct_cash_food_count = sum(cash_food, na.rm = T),
    ct_housing_count = sum(housing, na.rm = T),
    ct_phone_internet_count = sum(phone_internet, na.rm = T),
    ct_utilities_count = sum(utilities, na.rm = T),
    ct_transit_count = sum(transit, na.rm = T),
    ct_psychosocial_count = sum(pyschosocial, na.rm = T),
    ct_substance_use_count = sum(substance_use, na.rm = T),
    ct_senior_count = sum(senior, na.rm = T),
    ct_har_count = sum(har, na.rm = T),
    ct_testing_count = sum(test_referral, na.rm = T),
    
    #CHW referral statistics
    chw_any_referral_count = sum(chw_any_referral, na.rm = T),
    chw_chap_count = sum(chw_chap, na.rm = T),
    chw_insurance_count = sum(chw_insurance, na.rm = T),
    chw_referrals_hc_count = sum(chw_referrals_hc, na.rm = T),
    chw_medicare_savings_count = sum(chw_medicare_savings, na.rm = T),
    chw_cash_count = sum(chw_cash, na.rm = T),
    chw_charitycare_count = sum(chw_charitycare, na.rm = T),
    chw_utilities_count = sum(chw_utilities, na.rm = T),
    chw_food_count = sum(chw_food, na.rm = T),
    chw_snap_count = sum(chw_snap, na.rm = T),
    chw_food_ref_count = sum(chw_food_ref, na.rm = T),
    chw_housing_count = sum(chw_housing, na.rm = T),
    chw_orca_count = sum(chw_orca, na.rm = T),
    chw_legal_count = sum(chw_legal, na.rm = T),
    chw_mh_count = sum(chw_mh, na.rm = T),
    chw_grocery_count = sum(chw_grocery, na.rm = T),
    chw_ppe_count = sum(chw_ppe, na.rm = T),
    chw_edu_count = sum(chw_edu, na.rm = T),
    chw_financial_advice_count = sum(chw_financial_advice, na.rm = T),
    chw_emotional_count = sum(chw_emotional, na.rm = T),
    chw_swiq_count = sum(chw_swiq, na.rm = T),
    chw_har_count = sum(chw_har, na.rm = T),
    chw_other_count = sum(chw_other, na.rm = T)) %>%
  ungroup()

## Tabulate by agency
ct_date_tabulate_by_agency <- final_data_by_household %>%
  group_by(agency, form_created) %>%
  summarise(
    
    #Household count
    household_count = n(),
    
    #Any referral
    any_referral_count = sum(any_referral, na.rm = T),
    
    #CT referral statistics
    ct_any_referral_count = sum(ct_any_referral, na.rm = T),
    ct_to_chw_count = sum(chw, na.rm = T),
    ct_swiq_count = sum(swiq, na.rm = T),
    ct_tpr_count = sum(tpr, na.rm = T),
    ct_groceries_count = sum(groceries, na.rm = T),
    ct_medconsult_count = sum(medconsult, na.rm = T),
    ct_iqmotel_count = sum(iqmotel, na.rm = T),
    ct_healthinsurance_count = sum(healthinsurance, na.rm = T),
    ct_healthcare_count = sum(healthcare, na.rm = T),
    ct_medical_cost_count = sum(medical_costs, na.rm = T),
    ct_cash_food_count = sum(cash_food, na.rm = T),
    ct_housing_count = sum(housing, na.rm = T),
    ct_phone_internet_count = sum(phone_internet, na.rm = T),
    ct_utilities_count = sum(utilities, na.rm = T),
    ct_transit_count = sum(transit, na.rm = T),
    ct_psychosocial_count = sum(pyschosocial, na.rm = T),
    ct_substance_use_count = sum(substance_use, na.rm = T),
    ct_senior_count = sum(senior, na.rm = T),
    ct_har_count = sum(har, na.rm = T),
    ct_testing_count = sum(test_referral, na.rm = T),
    
    #CHW referral statistics
    chw_any_referral_count = sum(chw_any_referral, na.rm = T),
    chw_chap_count = sum(chw_chap, na.rm = T),
    chw_insurance_count = sum(chw_insurance, na.rm = T),
    chw_referrals_hc_count = sum(chw_referrals_hc, na.rm = T),
    chw_medicare_savings_count = sum(chw_medicare_savings, na.rm = T),
    chw_cash_count = sum(chw_cash, na.rm = T),
    chw_charitycare_count = sum(chw_charitycare, na.rm = T),
    chw_utilities_count = sum(chw_utilities, na.rm = T),
    chw_food_count = sum(chw_food, na.rm = T),
    chw_snap_count = sum(chw_snap, na.rm = T),
    chw_food_ref_count = sum(chw_food_ref, na.rm = T),
    chw_housing_count = sum(chw_housing, na.rm = T),
    chw_orca_count = sum(chw_orca, na.rm = T),
    chw_legal_count = sum(chw_legal, na.rm = T),
    chw_mh_count = sum(chw_mh, na.rm = T),
    chw_grocery_count = sum(chw_grocery, na.rm = T),
    chw_ppe_count = sum(chw_ppe, na.rm = T),
    chw_edu_count = sum(chw_edu, na.rm = T),
    chw_financial_advice_count = sum(chw_financial_advice, na.rm = T),
    chw_emotional_count = sum(chw_emotional, na.rm = T),
    chw_swiq_count = sum(chw_swiq, na.rm = T),
    chw_har_count = sum(chw_har, na.rm = T),
    chw_other_count = sum(chw_other, na.rm = T)) %>%
  ungroup()

## Combine tabulations by CT record date
complete_referrals_by_ct_date_tabulate <- bind_rows(ct_date_tabulate_both_agency, ct_date_tabulate_by_agency)

## Pivot long for Tableau format 
complete_referrals_by_ct_date_tableau <- pivot_longer(complete_referrals_by_ct_date_tabulate, !agency:form_created,
                                                names_to = "metric_name", values_to = "count") %>%
  
  #Add metadata date fields for Tableau
  mutate(
    last_updated = as.character(as.POSIXlt(lubridate::with_tz(Sys.time(), "America/Los_Angeles"))),
    max_date_filter = max(form_created, na.rm = T))


#### STEP 2: Tabulate statistics by CT referral date ####
#Note - this only includes households with referrals

## Prep referral data for tabulation
complete_referral_form_with_demo_for_tabulate <- complete_referral_form_with_demo %>%
  
  #Subset to desired variables
  select(agency, arm, record_id, referral_date, chw:test_referral, healthinsurance:senior, chw_chap:chw_other, dob_norm, race_eth_norm, language_norm) %>%
  
  #Create flags for any CT or CHW referral
  rowwise() %>%
  mutate(
    ct_referral_count = sum(c_across(swiq:senior), na.rm = T),
    chw_referral_count = sum(c_across(chw_chap:chw_other), na.rm = T))%>%
  ungroup() %>%  
  
  mutate(
    any_referral = case_when(ct_referral_count >= 1 | chw_referral_count >= 1 ~ 1L, TRUE ~ 0L),
    ct_any_referral = case_when(ct_referral_count >= 1 ~ 1L, TRUE ~ 0L),
    chw_any_referral = case_when(chw_referral_count >= 1 ~ 1L, TRUE ~ 0L)) %>%
  
  select(-ct_referral_count, -chw_referral_count) %>%
  
  #Filter to only households with a CT or CHW referral
  filter(any_referral == 1)

## Tabulate by referral date, both agencies combined
referral_date_tabulate_both_agency <- complete_referral_form_with_demo_for_tabulate %>%
  group_by(referral_date) %>%
  summarise(
    
    #Agency
    agency = "Both",
    
    #Any referral
    any_referral_count = sum(any_referral, na.rm = T),
    
    #CT referral statistics
    ct_any_referral_count = sum(ct_any_referral, na.rm = T),
    ct_to_chw_count = sum(chw, na.rm = T),
    ct_swiq_count = sum(swiq, na.rm = T),
    ct_tpr_count = sum(tpr, na.rm = T),
    ct_groceries_count = sum(groceries, na.rm = T),
    ct_medconsult_count = sum(medconsult, na.rm = T),
    ct_iqmotel_count = sum(iqmotel, na.rm = T),
    ct_healthinsurance_count = sum(healthinsurance, na.rm = T),
    ct_healthcare_count = sum(healthcare, na.rm = T),
    ct_medical_cost_count = sum(medical_costs, na.rm = T),
    ct_cash_food_count = sum(cash_food, na.rm = T),
    ct_housing_count = sum(housing, na.rm = T),
    ct_phone_internet_count = sum(phone_internet, na.rm = T),
    ct_utilities_count = sum(utilities, na.rm = T),
    ct_transit_count = sum(transit, na.rm = T),
    ct_psychosocial_count = sum(pyschosocial, na.rm = T),
    ct_substance_use_count = sum(substance_use, na.rm = T),
    ct_senior_count = sum(senior, na.rm = T),
    ct_har_count = sum(har, na.rm = T),
    ct_testing_count = sum(test_referral, na.rm =T),
    
    #CHW referral statistics
    chw_any_referral_count = sum(chw_any_referral, na.rm = T),
    chw_chap_count = sum(chw_chap, na.rm = T),
    chw_insurance_count = sum(chw_insurance, na.rm = T),
    chw_referrals_hc_count = sum(chw_referrals_hc, na.rm = T),
    chw_medicare_savings_count = sum(chw_medicare_savings, na.rm = T),
    chw_cash_count = sum(chw_cash, na.rm = T),
    chw_charitycare_count = sum(chw_charitycare, na.rm = T),
    chw_utilities_count = sum(chw_utilities, na.rm = T),
    chw_food_count = sum(chw_food, na.rm = T),
    chw_snap_count = sum(chw_snap, na.rm = T),
    chw_food_ref_count = sum(chw_food_ref, na.rm = T),
    chw_housing_count = sum(chw_housing, na.rm = T),
    chw_orca_count = sum(chw_orca, na.rm = T),
    chw_legal_count = sum(chw_legal, na.rm = T),
    chw_mh_count = sum(chw_mh, na.rm = T),
    chw_grocery_count = sum(chw_grocery, na.rm = T),
    chw_ppe_count = sum(chw_ppe, na.rm = T),
    chw_edu_count = sum(chw_edu, na.rm = T),
    chw_financial_advice_count = sum(chw_financial_advice, na.rm = T),
    chw_emotional_count = sum(chw_emotional, na.rm = T),
    chw_swiq_count = sum(chw_swiq, na.rm = T),
    chw_har_count = sum(chw_har, na.rm = T),
    chw_other_count = sum(chw_other, na.rm = T)) %>%
  ungroup()

## Tabulate by referral date, by agency
referral_date_tabulate_by_agency <- complete_referral_form_with_demo_for_tabulate %>%
  group_by(agency, referral_date) %>%
  summarise(
    
    #Any referral
    any_referral_count = sum(any_referral, na.rm = T),
    
    #CT referral statistics
    ct_any_referral_count = sum(ct_any_referral, na.rm = T),
    ct_to_chw_count = sum(chw, na.rm = T),
    ct_swiq_count = sum(swiq, na.rm = T),
    ct_tpr_count = sum(tpr, na.rm = T),
    ct_groceries_count = sum(groceries, na.rm = T),
    ct_medconsult_count = sum(medconsult, na.rm = T),
    ct_iqmotel_count = sum(iqmotel, na.rm = T),
    ct_healthinsurance_count = sum(healthinsurance, na.rm = T),
    ct_healthcare_count = sum(healthcare, na.rm = T),
    ct_medical_cost_count = sum(medical_costs, na.rm = T),
    ct_cash_food_count = sum(cash_food, na.rm = T),
    ct_housing_count = sum(housing, na.rm = T),
    ct_phone_internet_count = sum(phone_internet, na.rm = T),
    ct_utilities_count = sum(utilities, na.rm = T),
    ct_transit_count = sum(transit, na.rm = T),
    ct_psychosocial_count = sum(pyschosocial, na.rm = T),
    ct_substance_use_count = sum(substance_use, na.rm = T),
    ct_senior_count = sum(senior, na.rm = T),
    ct_har_count = sum(har, na.rm = T),
    ct_testing_count = sum(test_referral, na.rm =T),
    
    #CHW referral statistics
    chw_any_referral_count = sum(chw_any_referral, na.rm = T),
    chw_chap_count = sum(chw_chap, na.rm = T),
    chw_insurance_count = sum(chw_insurance, na.rm = T),
    chw_referrals_hc_count = sum(chw_referrals_hc, na.rm = T),
    chw_medicare_savings_count = sum(chw_medicare_savings, na.rm = T),
    chw_cash_count = sum(chw_cash, na.rm = T),
    chw_charitycare_count = sum(chw_charitycare, na.rm = T),
    chw_utilities_count = sum(chw_utilities, na.rm = T),
    chw_food_count = sum(chw_food, na.rm = T),
    chw_snap_count = sum(chw_snap, na.rm = T),
    chw_food_ref_count = sum(chw_food_ref, na.rm = T),
    chw_housing_count = sum(chw_housing, na.rm = T),
    chw_orca_count = sum(chw_orca, na.rm = T),
    chw_legal_count = sum(chw_legal, na.rm = T),
    chw_mh_count = sum(chw_mh, na.rm = T),
    chw_grocery_count = sum(chw_grocery, na.rm = T),
    chw_ppe_count = sum(chw_ppe, na.rm = T),
    chw_edu_count = sum(chw_edu, na.rm = T),
    chw_financial_advice_count = sum(chw_financial_advice, na.rm = T),
    chw_emotional_count = sum(chw_emotional, na.rm = T),
    chw_swiq_count = sum(chw_swiq, na.rm = T),
    chw_har_count = sum(chw_har, na.rm = T),
    chw_other_count = sum(chw_other, na.rm = T)) %>%
  ungroup()

## Combine tabulations by referral date
complete_referrals_by_referral_date_tabulate <- bind_rows(referral_date_tabulate_both_agency, referral_date_tabulate_by_agency)

## Pivot long for Tableau format 
complete_referrals_by_referral_date_tableau <- pivot_longer(complete_referrals_by_referral_date_tabulate, !agency:referral_date,
                                                      names_to = "metric_name", values_to = "count") %>%
  
  #Add metadata date fields for Tableau
  mutate(
    last_updated = as.character(as.POSIXlt(lubridate::with_tz(Sys.time(), "America/Los_Angeles"))),
    max_date_filter = max(referral_date, na.rm = T))
  

#### STEP 3: Export all data for Tableau dashboard, including CHW data ####
data_tableau <- list(complete_referrals_by_ct_date_tableau, complete_referrals_by_referral_date_tableau, complete_referrals_by_chw)
sheet_tableau <- list("referrals_by_ct_dt", "referrals_by_referral_dt", "referrals_by_chw")
filename_tableau <- file.path(tableau_export, "cc_metrics_tableau.xlsx")
write.xlsx(data_tableau, file = filename_tableau, sheetName = sheet_tableau)