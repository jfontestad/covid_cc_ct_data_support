#### Purpose: Create CHW-level datasets for tabulation and analysis
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Create referral data set at CHW level ####
complete_referrals_by_chw <- complete_referral_form_with_demo %>%
  
  #Select variables to keep
  select(agency, arm, record_id, referral_date, referral_timedt, call_attempt, chw_firstcall, chw_firstcall_date, chw_closed, chw_closed_date, referral_to_chw_hours,
         referral_to_chw_lte_24h, chw_open_to_closed_hours, chw:test_referral, healthinsurance:senior, chw_assigned, chw_assigned_other,
         chw_status, chw_firstcall_date, chw_firstcall, chw_closed, chw_closed, chw_time_est, chw_chap:chw_other, dob_norm, race_eth_norm, language_norm) %>%
  
  #Create summary variables for QA
  rowwise() %>%
  mutate(
    chw_referral_count = sum(c_across(chw_chap:chw_other), na.rm = T))%>%
  ungroup() %>%
  
  #Drop records when CHW assigned field is empty
  mutate(drop_flag = case_when(is.na(chw_assigned) ~ 1L, TRUE ~ 0L)) %>%
  filter(drop_flag == 0) %>%
  select(-drop_flag, -chw_referral_count) %>%
  
  #Add metadata date fields for Tableau
  mutate(
    last_updated = as.character(as.POSIXlt(lubridate::with_tz(Sys.time(), "America/Los_Angeles"))),
    max_date_filter = max(referral_date, na.rm = T))
  

#### STEP 2: Create analytic dataset with referrals at CHW level ####
complete_chw_analytic <- complete_referral_form_with_demo %>%
  
  #Filter out rows with null referral date or no assigned CHW
  filter(!is.na(referral_date) & !is.na(chw_assigned)) %>%
  
  #Subset to desired variables
  select(agency, arm, record_id, referral_date, referral_timedt, chw_firstcall, chw_firstcall_date, chw_closed, chw_closed_date, referral_to_chw_hours,
         referral_to_chw_lte_24h, chw_open_to_closed_hours, chw:iqmotel, healthinsurance:senior, chw_assigned, chw_assigned_other,
         chw_status, chw_firstcall_date, chw_firstcall, chw_closed, chw_closed, chw_time_est, chw_chap:chw_other, dob_norm, race_eth_norm, language_norm) %>%
  
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
  
  #Add flags for missing demographics
  mutate(race_eth_norm_missing = case_when(is.na(race_eth_norm) ~ 1L, TRUE ~ 0L),
         dob_norm_missing = case_when(is.na(dob_norm) ~ 1L, TRUE ~ 0L),
         language_norm_missing = case_when(is.na(language_norm) ~ 1L, TRUE ~ 0L)) %>%
  
  #Take distinct rows
  distinct()

## Tabulate missing demographics for past week (9 days ago through 2 days ago)
demo_qa_start <- Sys.Date() - days(9)
demo_qa_end <- Sys.Date() - days(2)

demo_qa <- filter(complete_chw_analytic, between(referral_date, demo_qa_start, demo_qa_end))

doh_raceethnicity_missing_count <- filter(count(filter(demo_qa, agency == "doh"), race_eth_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)),
                                          race_eth_norm_missing == 1)$n
doh_raceethnicity_missing_per <- filter(count(filter(demo_qa, agency == "doh"), race_eth_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)),
                                        race_eth_norm_missing == 1)$per
doh_dob_missing_count <- filter(count(filter(demo_qa, agency == "doh"), dob_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), dob_norm_missing == 1)$n
doh_dob_missing_per <- filter(count(filter(demo_qa, agency == "doh"), dob_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), dob_norm_missing == 1)$per
doh_language_missing_count <- filter(count(filter(demo_qa, agency == "doh"), language_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), language_norm_missing == 1)$n
doh_language_missing_per <- filter(count(filter(demo_qa, agency == "doh"), language_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), language_norm_missing == 1)$per

kc_raceethnicity_missing_count <- filter(count(filter(demo_qa, agency == "phskc"), race_eth_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)),
                                          race_eth_norm_missing == 1)$n
kc_raceethnicity_missing_per <- filter(count(filter(demo_qa, agency == "phskc"), race_eth_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)),
                                        race_eth_norm_missing == 1)$per
kc_dob_missing_count <- filter(count(filter(demo_qa, agency == "phskc"), dob_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), dob_norm_missing == 1)$n
kc_dob_missing_per <- filter(count(filter(demo_qa, agency == "phskc"), dob_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), dob_norm_missing == 1)$per
kc_language_missing_count <- filter(count(filter(demo_qa, agency == "phskc"), language_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), language_norm_missing == 1)$n
kc_language_missing_per <- filter(count(filter(demo_qa, agency == "phskc"), language_norm_missing) %>% mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), language_norm_missing == 1)$per


## QA to make sure that dataset has only one row per arm-record ID-referral_timedt
complete_analytic_distinct_row_percent <- filter(complete_chw_analytic %>% group_by(agency, arm, record_id, referral_timedt) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count) %>%
                                              mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), row_count == 1)$per