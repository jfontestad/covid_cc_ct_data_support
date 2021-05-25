#### Purpose: Clean and combine referral data for PHSKC-assigned and DOH-assigned households
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Clean PHSKC-assigned household referral data, adapting Julia's SAS code ####
kc_referral_form <- kc_cc_complete %>% 
  filter(redcap_repeat_instrument == "referral_form" | !is.na(har_startdt)) %>%
  
  select(redcap_event_name, record_id, referral_timedt,
         
         #CT referral variables
         iq_motel, grocery_referral, need_met_dt, med_referrals, tpr_referral, swiq_referral, healthinsurance, healthcare, medical_costs, cash_food, housing, phone_internet, 
         utilities, transit, pyschosocial, substance_use, senior, har_interest, referrals___1, referrals___2, referrals___3, referrals___4, referrals___5,
         
         #CHW referral variables
         chw_assigned, chw_assigned_other, chw_firstcall, chw_status, chw_closed, chw_svc_type___chap, chw_svc_type___insurance, chw_svc_type___referrals_hc,
         chw_svc_type___medicare_savings, chw_svc_type___cash, chw_svc_type___charitycare, chw_svc_type___utilities, chw_svc_type___food, chw_svc_type___snap, 
         chw_svc_type___food_ref, chw_svc_type___housing, chw_svc_type___orca, chw_svc_type___legal, chw_svc_type___mh, chw_svc_type___grocery, chw_svc_type___ppe,
         chw_svc_type___edu, chw_svc_type___financial_advice, chw_svc_type___emotional, chw_svc_type___swiq, chw_svc_type___har, chw_svc_type___other, chw_svc_type_othspec,
         chw_time_est, har_startdt,
         
         #CHW-collected demographics
         chw_language, chw_language_other, chw_raceethnicity___1:chw_raceethnicity___29, other_asian, other_blackafrica, other_latinx, other_nhpi, other_nhpi_2, chw_dob) %>%
  
  #Pull out dates from date-times (date-times are already in desired format)
  #Adaptation for HAR - set referral date equal to har_startdt if referral_timedt is null
  mutate(
    referral_date = case_when(
      !is.na(har_startdt) & is.na(referral_timedt) ~ har_startdt,
      !is.na(referral_timedt) ~ as.Date(referral_timedt, origin = origin),
      !is.na(chw_firstcall) ~ as.Date(chw_firstcall, origin = origin),
      TRUE ~ lubridate::NA_Date_),
    chw_firstcall_date = as.Date(chw_firstcall, origin = origin),
    chw_closed_date = as.Date(chw_closed, origin = origin),
    
    #calculate time interval stats in hours
    referral_to_chw_hours = round(as.numeric(difftime(chw_firstcall, referral_timedt, units = "hours")), 1),
    chw_open_to_closed_hours = round(as.numeric(difftime(chw_closed, chw_firstcall, units = "hours")), 1),
    
    #CHW performance metrics
    referral_to_chw_lte_24h = case_when(referral_to_chw_hours <= 24 ~ 1L, TRUE ~ 0L),
    
    #convert date-times to character for easier exporting
    referral_timedt = as.character(referral_timedt),
    chw_firstcall = as.character(chw_firstcall),
    chw_closed = as.character(chw_closed),
    
    #create arm variable
    arm = case_when(
      str_detect(redcap_event_name, "_1") == T ~ "1",
      str_detect(redcap_event_name, "_2") == T ~ "2",
      str_detect(redcap_event_name, "_3") == T ~ "3",
      TRUE ~ NA_character_),
    
    #create agency variable
    agency = "phskc",
    
    #create CT referral variables
    chw = case_when(
      (cash_food=="2" | healthcare=="2" | healthinsurance=="2" | medical_costs=="2" | phone_internet=="2" | pyschosocial=="2"
       | substance_use=="2" | transit=="2" | utilities=="2" | swiq_referral=="1" | housing== "2" | senior=="2") ~ 1L,
      TRUE ~ 0L),
    
    housing = case_when(housing %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    senior = case_when(senior %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    substance_use = case_when(substance_use %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    pyschosocial = case_when(pyschosocial %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    transit = case_when(transit %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    utilities = case_when(utilities %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    phone_internet = case_when(phone_internet %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    cash_food = case_when(cash_food %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    healthcare = case_when(healthcare %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    medical_costs = case_when(medical_costs %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    healthinsurance = case_when(healthinsurance %in% c("1", "2") ~ 1L, TRUE ~ 0L),
    swiq = case_when(swiq_referral == "1" ~ 1L, TRUE ~ 0L),
    tpr = case_when(tpr_referral == "1" ~ 1L, TRUE ~ 0L),
    har = case_when(har_interest == "1" ~ 1L, TRUE ~ 0L),
    
    groceries = case_when(
      !is.na(need_met_dt) | grocery_referral == "3" | referrals___4 == "1" ~ 1L,
      TRUE ~ 0L),
    
    medconsult = case_when(
      med_referrals == "MEDCONSULT" | referrals___2 == "1" ~ 1L,
      TRUE ~ 0L),  
    
    iqmotel = case_when(
      iq_motel == "1" | referrals___3 == "1" ~ 1L,
      TRUE ~ 0L)) %>%
  
  #create CHW referral variables
  mutate_at(
    vars(chw_svc_type___chap:chw_svc_type___other),
    ~case_when(. == "1" ~ 1L, TRUE ~ 0L)) %>%
  
  #Create CHW HAR variable (special case as separate form exists), note that this overwrites existing var with same name
  mutate(chw_svc_type___har = case_when(!is.na(har_startdt) ~ 1L, TRUE ~ 0L)) %>%
  
  #Rename CHW variables
  rename(
    chw_chap = chw_svc_type___chap,
    chw_insurance = chw_svc_type___insurance,
    chw_referrals_hc = chw_svc_type___referrals_hc,
    chw_medicare_savings = chw_svc_type___medicare_savings,
    chw_cash = chw_svc_type___cash,
    chw_charitycare = chw_svc_type___charitycare,
    chw_utilities = chw_svc_type___utilities,
    chw_food = chw_svc_type___food,
    chw_snap = chw_svc_type___snap,
    chw_food_ref = chw_svc_type___food_ref,
    chw_housing = chw_svc_type___housing,
    chw_orca = chw_svc_type___orca,
    chw_legal = chw_svc_type___legal,
    chw_mh = chw_svc_type___mh,
    chw_grocery = chw_svc_type___grocery,
    chw_ppe = chw_svc_type___ppe,
    chw_edu = chw_svc_type___edu,
    chw_financial_advice = chw_svc_type___financial_advice,
    chw_emotional = chw_svc_type___emotional,
    chw_swiq = chw_svc_type___swiq,
    chw_har = chw_svc_type___har,
    chw_other = chw_svc_type___other) %>%
  
  #Drop any records with a null referral_date variable
  filter(!is.na(referral_date)) %>%
  
  #Drop any records with "dup" chw_status
  mutate(dup_flag = case_when(chw_status == "dup" ~ 1, TRUE ~ 0)) %>%
  filter(dup_flag == 0) %>%
  select(-dup_flag) %>%
  
  #Drop raw versions of recoded variables
  select(-iq_motel:-swiq_referral, -har_interest:-referrals___5) %>%
  
  #Make sure dataset is distinct
  distinct()


#### STEP 2: Identify PHSKC-assigned households who received testing referrals ####
kc_cc_test_ref <- kc_cc_complete %>%
  
  filter(testing_plans == "REFERRED") %>%
  
  mutate(
    referral_date = case_when(
      !is.na(referral_timedt) ~ as.Date(referral_timedt, origin = origin),
      !is.na(ind_overview_dt) ~ ind_overview_dt,
      !is.na(hh_overview_dt)~ hh_overview_dt,
      !is.na(hh_overview_dt_new) ~ hh_overview_dt_new,
      !is.na(chw_firstcall) ~ as.Date(chw_firstcall, origin = origin),
      TRUE ~ lubridate::NA_Date_),
    
    test_referral = 1L,
    
    agency = "phskc",
  ) %>%
  
  filter(!is.na(referral_date)) %>%
  
  group_by(arm, record_id) %>%
  mutate(referral_date = max(referral_date, na.rm = T)) %>%
  ungroup() %>%
  
  select(agency, arm, record_id, referral_date, test_referral) %>%
  distinct()

#QA: All record IDs should not have more than 1 row
kc_cc_test_ref %>% group_by(arm, record_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count)

#Bind to referral dataframe
kc_referral_form <- bind_rows(kc_referral_form, kc_cc_test_ref)
rm(kc_cc_test_ref)


#### STEP 3: Clean DOH-assigned household referral data, adapting from code for PHSKC-assigned households ####
doh_referral_form <- doh_cc_current_clustered %>% 
  
  select(arm, record_id, referral_timedt,
         
    #CT referral variables
    iq_motel, grocery_referral, need_met_dt, med_referrals, tpr_referral, swiq_referral, healthinsurance, healthcare, medical_costs, cash_food, 
    utilities, har_interest, call_attempt,
    
    #CHW referral variables
    chw_assigned, chw_assigned_n, chw_firstcall, chw_status, chw_closed, chw_svc_type___chap, chw_svc_type___insurance, chw_svc_type___referrals_hc,
    chw_svc_type___medicare_savings, chw_svc_type___cash, chw_svc_type___charitycare, chw_svc_type___utilities, chw_svc_type___food, chw_svc_type___snap, 
    chw_svc_type___food_ref, chw_svc_type___housing, chw_svc_type___orca, chw_svc_type___legal, chw_svc_type___mh, chw_svc_type___grocery, chw_svc_type___ppe,
    chw_svc_type___edu, chw_svc_type___financial_advice, chw_svc_type___emotional, chw_svc_type___swiq, chw_svc_type___har, chw_svc_type___other, chw_svc_type_othspec,
    chw_time_est, har_startdt,
    
    #DOH-collected demographics
    language_c, date_of_birth_dob, race_eth) %>%
    
    #Pull out dates from date-times (date-times are already in desired format)
    #Adaptation for HAR - set referral date equal to har_startdt if referral_timedt is null
    mutate(
      referral_date = case_when(
        !is.na(har_startdt) & is.na(referral_timedt) ~ as.Date(har_startdt, origin = origin),
        !is.na(referral_timedt) ~ as.Date(referral_timedt, origin = origin),
        !is.na(chw_firstcall) ~ as.Date(chw_firstcall, origin = origin),
        !is.na(call_attempt) ~ as.Date(call_attempt, origin = origin),
        TRUE ~ lubridate::NA_Date_),
      chw_firstcall_date = as.Date(chw_firstcall, origin = origin),
      chw_closed_date = as.Date(chw_closed, origin = origin),
      
      #Replace missing referral_timedt with other timedt variables is present
      referral_timedt = case_when(
        !is.na(referral_timedt) ~ referral_timedt,
        !is.na(chw_firstcall) ~ chw_firstcall,
        !is.na(call_attempt) ~ call_attempt,
        TRUE ~ NA_character_),
    
      #calculate time interval stats in hours
      referral_to_chw_hours = round(as.numeric(difftime(chw_firstcall, referral_timedt, units = "hours")), 1),
      chw_open_to_closed_hours = round(as.numeric(difftime(chw_closed, chw_firstcall, units = "hours")), 1),
      
      #Normalize chw assigned variable
      chw_assigned = case_when(
        !is.na(chw_assigned_n) ~ chw_assigned_n,
        !is.na(chw_assigned) ~ chw_assigned,
        TRUE ~ NA_character_),
    
      #CHW performance metrics
      referral_to_chw_lte_24h = case_when(referral_to_chw_hours <= 24 ~ 1L, TRUE ~ 0L),
      
      #convert date-times to character for easier exporting
      referral_timedt = as.character(referral_timedt),
      chw_firstcall = as.character(chw_firstcall),
      chw_closed = as.character(chw_closed),
      
      #create agency variable
      agency = "doh",
      
      #create CT referral variables
      chw = case_when(
        (cash_food=="2" | healthcare=="2" | healthinsurance=="2" | medical_costs=="2" | utilities=="2" | swiq_referral=="1") ~ 1L,
        TRUE ~ 0L),
      
      utilities = case_when(utilities %in% c("1", "2") ~ 1L, TRUE ~ 0L),
      cash_food = case_when(cash_food %in% c("1", "2") ~ 1L, TRUE ~ 0L),
      healthcare = case_when(healthcare %in% c("1", "2") ~ 1L, TRUE ~ 0L),
      medical_costs = case_when(medical_costs %in% c("1", "2") ~ 1L, TRUE ~ 0L),
      healthinsurance = case_when(healthinsurance %in% c("1", "2") ~ 1L, TRUE ~ 0L),
      swiq = case_when(swiq_referral == "1" ~ 1L, TRUE ~ 0L),
      tpr = case_when(tpr_referral == "1" ~ 1L, TRUE ~ 0L),
      har = case_when(har_interest == "1" ~ 1L, TRUE ~ 0L),
    
      groceries = case_when(
        !is.na(need_met_dt) | grocery_referral == "3" ~ 1L,
        TRUE ~ 0L),
      
      medconsult = case_when(
        med_referrals == "MEDCONSULT" ~ 1L,
        TRUE ~ 0L),  
      
      iqmotel = case_when(
        iq_motel == "1" ~ 1L,
        TRUE ~ 0L)) %>%
    
    #create CHW referral variables
    mutate_at(
    vars(chw_svc_type___chap:chw_svc_type___other),
    ~case_when(. == "1" ~ 1L, TRUE ~ 0L)) %>%
    
    #Create CHW HAR variable (special case as separate form exists), note that this overwrites existing var with same name
    mutate(chw_svc_type___har = case_when(!is.na(har_startdt) ~ 1L, TRUE ~ 0L)) %>%
    
    #Rename CHW variables
    rename(
    chw_chap = chw_svc_type___chap,
    chw_insurance = chw_svc_type___insurance,
    chw_referrals_hc = chw_svc_type___referrals_hc,
    chw_medicare_savings = chw_svc_type___medicare_savings,
    chw_cash = chw_svc_type___cash,
    chw_charitycare = chw_svc_type___charitycare,
    chw_utilities = chw_svc_type___utilities,
    chw_food = chw_svc_type___food,
    chw_snap = chw_svc_type___snap,
    chw_food_ref = chw_svc_type___food_ref,
    chw_housing = chw_svc_type___housing,
    chw_orca = chw_svc_type___orca,
    chw_legal = chw_svc_type___legal,
    chw_mh = chw_svc_type___mh,
    chw_grocery = chw_svc_type___grocery,
    chw_ppe = chw_svc_type___ppe,
    chw_edu = chw_svc_type___edu,
    chw_financial_advice = chw_svc_type___financial_advice,
    chw_emotional = chw_svc_type___emotional,
    chw_swiq = chw_svc_type___swiq,
    chw_har = chw_svc_type___har,
    chw_other = chw_svc_type___other) %>%

  #Drop any records with a null referral_date variable
  filter(!is.na(referral_date)) %>%
  
  #Drop any records with "dup" chw_status
  mutate(dup_flag = case_when(chw_status == "dup" ~ 1, TRUE ~ 0)) %>%
  filter(dup_flag == 0) %>%
  select(-dup_flag) %>%
  
  #Drop raw versions of recoded variables
  select(-iq_motel:-swiq_referral, -har_interest, -chw_assigned_n) %>%
  
  #Make sure dataset is distinct
  distinct()


#### STEP 4: Identify DOH-assigned households who received testing referrals ####
doh_cc_test_ref <- doh_cc_current_clustered %>%
  
  filter(testing_plans == "REFERRED") %>%
  
  mutate(
    referral_date = case_when(
      !is.na(referral_timedt) ~ as.Date(referral_timedt, origin = origin),
      !is.na(chw_firstcall) ~ as.Date(chw_firstcall, origin = origin),
      !is.na(call_attempt) ~ as.Date(call_attempt, origin = origin),
      TRUE ~ lubridate::NA_Date_),
    
    test_referral = 1L,
    
    agency = "doh"
  ) %>%
  
  filter(!is.na(referral_date)) %>%
  
  group_by(arm, record_id) %>%
  mutate(referral_date = max(referral_date, na.rm = T)) %>%
  ungroup() %>%
  
  select(agency, arm, record_id, referral_date, test_referral) %>%
  distinct()

#QA: All record IDs should not have more than 1 row
doh_cc_test_ref %>% group_by(arm, record_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count)

#Bind to referral dataframe
doh_referral_form <- bind_rows(doh_referral_form, doh_cc_test_ref)
rm(doh_cc_test_ref)


#### STEP 5: Normalize referral form demographics at household level - PHSKC assigned cases ####

#For language - take maximum (selects non-English language over English), for DOB - take minimum (i.e. oldest household member)
kc_referral_based_demo <- select(kc_referral_form, agency, arm, record_id, chw_language, chw_raceethnicity___1:chw_dob) %>%
  
  #Turn all zeroes to NA
  mutate_at(vars(chw_raceethnicity___1:chw_raceethnicity___29), ~na_if(., "0")) %>%
  
  #Set invalidly formatted dates of birth to missing
  mutate(chw_dob = case_when(str_detect(chw_dob, "-") == T ~ chw_dob, TRUE ~ NA_character_)) %>%
  
  #Fill missing values with non-missing values
  group_by(arm, record_id) %>%
  fill(chw_language:chw_dob, .direction = "downup") %>%
  
  mutate(
    #Take minimum date of birth when multiple values exist for an arm-record ID
    #Note ifelse prevents Inf values as compared to case_when
    chw_dob = ifelse(sum(!is.na(chw_dob)) == 0, NA_character_, as.character(min(as.Date(chw_dob, origin = origin), na.rm = T))),
    
    #Take maximum language when multiple values exist for an arm-record ID
    #Note ifelse prevents Inf values as compared to case_when
    chw_language = ifelse(sum(!is.na(chw_language)) == 0, NA_character_, as.character(max(as.integer(chw_language), na.rm = T)))
  ) %>%
  ungroup() %>%
  distinct() %>%
  
  #Normalize language with DOH-assigned data
  mutate(
    language_norm = case_when(
      chw_language == "1" ~ "English",
      chw_language == "2" ~ "Chinese",
      chw_language == "3" ~ "Korean",
      chw_language == "4" ~ "Russian",
      chw_language == "5" ~ "Somali",
      chw_language == "6" ~ "Spanish",
      chw_language == "7" ~ "Vietnamese",
      chw_language == "8" ~ "Another language",
      TRUE ~ NA_character_),
  
  #Normalize race/ethnicity with DOH-assigned data
  race_aian = case_when(chw_raceethnicity___1 == 1 ~ 1L, TRUE ~ 0L),
  race_asian = case_when(
    chw_raceethnicity___2 == 1 | chw_raceethnicity___3 == 1 | chw_raceethnicity___4 == 1 | chw_raceethnicity___5 == 1 |
      chw_raceethnicity___6 == 1 | chw_raceethnicity___7 == 1 | chw_raceethnicity___8 == 1 | chw_raceethnicity___9 == 1 |
      chw_raceethnicity___10 == 1 ~ 1L, TRUE ~ 0L),
  race_black = case_when(
    chw_raceethnicity___11 == 1 | chw_raceethnicity___12 == 1 | chw_raceethnicity___13 == 1 | chw_raceethnicity___14 == 1 |
      chw_raceethnicity___15 == 1 | chw_raceethnicity___16 == 1 ~ 1L, TRUE ~ 0L),
  race_latino = case_when(
    chw_raceethnicity___17 == 1 | chw_raceethnicity___18 == 1 | chw_raceethnicity___19 == 1 | chw_raceethnicity___20 == 1 ~ 1L, TRUE ~ 0L),
  race_nhpi = case_when(
    chw_raceethnicity___22 == 1 | chw_raceethnicity___23 == 1 | chw_raceethnicity___24 == 1 | chw_raceethnicity___25 == 1 |
      chw_raceethnicity___26 == 1 ~ 1L, TRUE ~ 0L),
  race_white = case_when(
    chw_raceethnicity___21 == 1 | chw_raceethnicity___27 == 1 ~ 1L, TRUE ~ 0L),
  
  #Create additional race-specific variables
  race_asian_indian = case_when(chw_raceethnicity___2 == 1 ~ 1L, TRUE ~ 0L),
  race_chinese = case_when(chw_raceethnicity___3 == 1 ~ 1L, TRUE ~ 0L),
  race_filipino = case_when(chw_raceethnicity___4 == 1 ~ 1L, TRUE ~ 0L),
  race_japanese = case_when(chw_raceethnicity___5 == 1 ~ 1L, TRUE ~ 0L),
  race_korean = case_when(chw_raceethnicity___6 == 1 ~ 1L, TRUE ~ 0L),
  race_vietnamese = case_when(chw_raceethnicity___7 == 1 ~ 1L, TRUE ~ 0L),
  race_cambodian = case_when(chw_raceethnicity___8 == 1 ~ 1L, TRUE ~ 0L),
  race_thai = case_when(chw_raceethnicity___9 == 1 ~ 1L, TRUE ~ 0L),
  race_asian_other = case_when(chw_raceethnicity___10 == 1 ~ 1L, TRUE ~ 0L),
  
  race_aa = case_when(chw_raceethnicity___11 == 1 ~ 1L, TRUE ~ 0L),
  race_somali = case_when(chw_raceethnicity___12 == 1 ~ 1L, TRUE ~ 0L),
  race_ethiopian = case_when(chw_raceethnicity___13 == 1 ~ 1L, TRUE ~ 0L),
  race_eritrean = case_when(chw_raceethnicity___14 == 1 ~ 1L, TRUE ~ 0L),
  race_kenyan = case_when(chw_raceethnicity___15 == 1 ~ 1L, TRUE ~ 0L),
  race_black_or_african_other = case_when(chw_raceethnicity___16 == 1 ~ 1L, TRUE ~ 0L),
  
  race_mexican = case_when(chw_raceethnicity___17 == 1 ~ 1L, TRUE ~ 0L),
  race_puerto_rican = case_when(chw_raceethnicity___18 == 1 ~ 1L, TRUE ~ 0L),
  race_cuban = case_when(chw_raceethnicity___19 == 1 ~ 1L, TRUE ~ 0L),
  race_latinx_other = case_when(chw_raceethnicity___20 == 1 ~ 1L, TRUE ~ 0L),
  
  race_mena = case_when(chw_raceethnicity___21 == 1 ~ 1L, TRUE ~ 0L),
  
  race_nh = case_when(chw_raceethnicity___22 == 1 ~ 1L, TRUE ~ 0L),
  race_samoan = case_when(chw_raceethnicity___23 == 1 ~ 1L, TRUE ~ 0L),
  race_marshallese = case_when(chw_raceethnicity___24 == 1 ~ 1L, TRUE ~ 0L),
  race_guamanian = case_when(chw_raceethnicity___25 == 1 ~ 1L, TRUE ~ 0L),
  race_nhpi_other = case_when(chw_raceethnicity___26 == 1 ~ 1L, TRUE ~ 0L)) %>%
  
  rowwise() %>%
  mutate(race_eth_sum = sum(c_across(race_aian:race_white), na.rm = T)) %>%
  ungroup() %>%
  
  mutate(
    race_eth_norm = case_when(
      race_eth_sum == 0 ~ NA_character_,
      race_eth_sum > 1 ~ "Multiple race",
      race_aian == 1 ~ "American Indian or Alaska Native",
      race_asian == 1 ~ "Asian",
      race_black == 1 ~ "Black or African_American",
      race_latino == 1 ~ "Hispanic/Latino",
      race_nhpi == 1 ~ "Native Hawaiian or Pacific Islander",
      race_white == 1 ~ "White",
      TRUE ~ NA_character_)
  ) %>%
  
  #Drop original CHW race variables
  select(-chw_raceethnicity___1:-other_nhpi_2) %>%
  
  #Normalize date of birth with DOH-assigned data
  mutate(dob_norm = chw_dob)
    
# QA to make sure no more than one row per arm-record ID
kc_referral_based_demo_distinct_row_percent <- filter(kc_referral_based_demo %>% group_by(arm, record_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count) %>%
                                                        mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), row_count == 1)$per


#### STEP 6: Normalize referral form demographics at household level - DOH assigned cases ####

#For DOB - take minimum (oldest household member) & for language - take maximum (selects non-English language over English)
doh_referral_based_demo <- select(doh_referral_form, agency, arm, record_id, language_c, race_eth, date_of_birth_dob) %>%
            
  #Normalize language PHSKC-assigned data
  mutate(
    language_norm = case_when(
      str_detect(str_to_lower(language_c), "english") ~ "1",
      str_detect(str_to_lower(language_c), "chinese") | str_detect(str_to_lower(language_c), "cantonese")| str_detect(str_to_lower(language_c), "mandarin") ~ "2",
      str_detect(str_to_lower(language_c), "korean") ~ "3",
      str_detect(str_to_lower(language_c), "russian") ~ "4",
      str_detect(str_to_lower(language_c), "somali") ~ "5",
      str_detect(str_to_lower(language_c), "spanish") ~ "6",
      str_detect(str_to_lower(language_c), "vietnam") ~ "7",
      !is.na(language_c) ~ "8",
      TRUE ~ NA_character_),
    
    #Normalize race/ethnicity PHSKC-assigned data
    race_eth_norm = case_when(race_eth == "Unknown" ~ NA_character_, TRUE ~ race_eth),
  
    #Normalize dob with PHSKC-assigned data
    dob_norm = date_of_birth_dob) %>%
  
  select(-language_c, -race_eth, -date_of_birth_dob) %>%
  
  #Fill missing values with non-missing values
  group_by(arm, record_id) %>%
  fill(language_norm:dob_norm, .direction = "downup") %>%
  
  mutate(
    #Take minimum date of birth when multiple values exist for an arm-record ID
    #Note ifelse prevents Inf values as compared to case_when
    dob_norm = ifelse(sum(!is.na(dob_norm)) == 0, NA_character_, as.character(min(as.Date(dob_norm, origin = origin), na.rm = T))),
    
    #Take maximum language when multiple values exist for an arm-record ID
    #Note ifelse prevents Inf values as compared to case_when
    language_norm = ifelse(sum(!is.na(language_norm)) == 0, NA_character_, as.character(max(as.integer(language_norm), na.rm = T))),
    
    #Set race to multiple race if more than race reported at household
    race_eth_count = n_distinct(race_eth_norm),
    race_eth_norm = case_when(
      race_eth_count > 1 ~ "Multiple race",
      TRUE ~ race_eth_norm
    )) %>%
  select(-race_eth_count) %>%
  ungroup() %>%
  distinct() %>%
  
  #Convert language from numeric to character names
  mutate(
    language_norm = case_when(
      language_norm == "1" ~ "English",
      language_norm == "2" ~ "Chinese",
      language_norm == "3" ~ "Korean",
      language_norm == "4" ~ "Russian",
      language_norm == "5" ~ "Somali",
      language_norm == "6" ~ "Spanish",
      language_norm == "7" ~ "Vietnamese",
      language_norm == "8" ~ "Another language",
      TRUE ~ NA_character_))

# QA to make sure no more than one row per arm-record ID
doh_referral_based_demo_distinct_row_percent <- filter(doh_referral_based_demo %>% group_by(arm, record_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count) %>%
                                                        mutate(per = round2(n/sum(n, na.rm = T)*100, 1)), row_count == 1)$per


#### STEP 7: Bind PHSKC-assigned and DOH-assigned demographics ####
complete_referral_based_demo <- bind_rows(kc_referral_based_demo, doh_referral_based_demo) %>%
  select(agency, arm, record_id, dob_norm, race_eth_norm, language_norm, everything())
rm(kc_referral_based_demo, doh_referral_based_demo)


#### STEP 8: Prepare household-level demographics using A&I data ####

## Create a household-level demographic dataset using A&I demographics
#Filter out non-household member WDRS IDs before join
wdrs_demo_household <- left_join(
  filter(complete_cc_wdrs, !(wdrs_id_type %in% c("wdrs_hh_exp", "wdrs_ic"))) %>% select(agency, arm, record_id, wdrs_id),
  select(wdrs_final, case_id, language, lang_specify, birth_date, race_eth, race_eth_sub_grp, reporting_zipcode, census_tractid),
  by = c("wdrs_id" = "case_id")
)

## Normalize demographic data with CT/CC demographic data to prepare for eventual merge
wdrs_demo_household <- wdrs_demo_household %>%
  
  mutate(
  #Race/ethnicity
  race_eth_norm = case_when(
    race_eth == "American Indian or Alaskan Native, not hispanic" &
      (is.na(race_eth_sub_grp) | race_eth_sub_grp == "Other or multi-racial") ~ "American Indian or Alaska Native",
    race_eth == "Asian, not hispanic" &
      (race_eth_sub_grp %in% c("Cambodian", "Chinese", "Filipino", "Indian", "Japanese", "Korean", "Thai", "Vietnamese") |
         race_eth_sub_grp == "Other or multi-racial" | is.na(race_eth_sub_grp)) ~ "Asian",
    race_eth == "Black, not hispanic" &
      (race_eth_sub_grp %in% c("African American", "Eritrean", "Ethiopian", "Kenyan", "Somali") |
         race_eth_sub_grp == "Other or multi-racial" | is.na(race_eth_sub_grp)) ~ "Black or African_American",
    race_eth == "Hispanic or Latino, any race" &
      (race_eth_sub_grp %in% c("Cuban", "Mexican") |
         race_eth_sub_grp == "Other or multi-racial" | is.na(race_eth_sub_grp)) ~ "Hispanic/Latino",
    race_eth == "Native Hawaiian or other Pacific Islander, not hispanic" &
      (race_eth_sub_grp %in% c("Marshallese", "Native Hawaiian", "Samoan") |
         race_eth_sub_grp == "Other or multi-racial" | is.na(race_eth_sub_grp)) ~ "Native Hawaiian or Pacific Islander",
    race_eth == "White, not hispanic" &
      (is.na(race_eth_sub_grp) | race_eth_sub_grp == "Other or multi-racial") ~ "White",
    
    #Multiple race
    race_eth %in% c("American Indian or Alaskan Native, not hispanic", "Asian, not hispanic", "Black, not hispanic",
                    "Hispanic or Latino, any race", "Native Hawaiian or other Pacific Islander, not hispanic",
                    "White, not hispanic") ~ "Multiple race",
    
    #Missing race but sub group filled in
    race_eth_sub_grp %in% c("Cambodian", "Chinese", "Filipino", "Indian", "Japanese", "Korean", "Thai", "Vietnamese") ~ "Asian",
    race_eth_sub_grp %in% c("African American", "Eritrean", "Ethiopian", "Kenyan", "Somali") ~ "Black or African_American",
    race_eth_sub_grp %in% c("Cuban", "Mexican") ~ "Hispanic/Latino",
    race_eth_sub_grp %in% c("Marshallese", "Native Hawaiian", "Samoan") ~ "Native Hawaiian or Pacific Islander",
    
    #Everything else set to missing
    TRUE ~ NA_character_),
  
  #Create additional detailed race-specific variables
  race_aian = case_when(race_eth == "American Indian or Alaskan Native, not hispanic" ~ 1L, TRUE ~ 0L),
  race_asian = case_when(race_eth == "Asian, not hispanic" | race_eth_sub_grp %in% c("Cambodian", "Chinese", "Filipino", "Indian", "Japanese", "Korean", "Thai", "Vietnamese")
                         ~ 1L, TRUE ~ 0L),
  race_black = case_when(race_eth == "Black, not hispanic" | race_eth_sub_grp %in% c("African American", "Eritrean", "Ethiopian", "Kenyan", "Somali")
                         ~ 1L, TRUE ~ 0L),
  race_latino = case_when(race_eth == "Hispanic or Latino, any race" | race_eth_sub_grp %in% c("Cuban", "Mexican") ~ 1L, TRUE ~ 0L),
  race_nhpi = case_when(race_eth == "Native Hawaiian or other Pacific Islander, not hispanic" | race_eth_sub_grp %in% c("Marshallese", "Native Hawaiian", "Samoan")
                        ~ 1L, TRUE ~ 0L),
  race_white = case_when(race_eth == "White, not hispanic" ~ 1L, TRUE ~ 0L),
  
  race_asian_indian = case_when(race_eth_sub_grp == "Indian" ~ 1L, TRUE ~ 0L),
  race_chinese = case_when(race_eth_sub_grp == "Chinese" ~ 1L, TRUE ~ 0L),
  race_filipino = case_when(race_eth_sub_grp == "Filipino" ~ 1L, TRUE ~ 0L),
  race_japanese = case_when(race_eth_sub_grp == "Japanese" ~ 1L, TRUE ~ 0L),
  race_korean = case_when(race_eth_sub_grp == "Korean" ~ 1L, TRUE ~ 0L),
  race_vietnamese = case_when(race_eth_sub_grp == "Vietnamese" ~ 1L, TRUE ~ 0L),
  race_cambodian = case_when(race_eth_sub_grp == "Cambodian" ~ 1L, TRUE ~ 0L),
  race_thai = case_when(race_eth_sub_grp == "Thai" ~ 1L, TRUE ~ 0L),
  
  race_aa = case_when(race_eth_sub_grp == "African American" ~ 1L, TRUE ~ 0L),
  race_somali = case_when(race_eth_sub_grp == "Somali" ~ 1L, TRUE ~ 0L),
  race_ethiopian = case_when(race_eth_sub_grp == "Ethiopian" ~ 1L, TRUE ~ 0L),
  race_eritrean = case_when(race_eth_sub_grp == "Eritrean" ~ 1L, TRUE ~ 0L),
  race_kenyan = case_when(race_eth_sub_grp == "Kenyan" ~ 1L, TRUE ~ 0L),
  
  race_mexican = case_when(race_eth_sub_grp == "Mexican" ~ 1L, TRUE ~ 0L),
  race_cuban = case_when(race_eth_sub_grp == "Cuban" ~ 1L, TRUE ~ 0L),

  race_nh = case_when(race_eth_sub_grp == "Native Hawaiian" ~ 1L, TRUE ~ 0L),
  race_samoan = case_when(race_eth_sub_grp == "Samoan" ~ 1L, TRUE ~ 0L),
  race_marshallese = case_when(race_eth_sub_grp == "Marshallese" ~ 1L, TRUE ~ 0L),
  
  #Language
  language_norm = case_when(
    language == "English" ~ "1",
    str_detect(lang_specify, "Chinese") ~ "2",
    lang_specify == "Korean" ~ "3",
    lang_specify == "Russian" ~ "4",
    lang_specify == "Somali" ~ "5",
    lang_specify == "Spanish" ~ "6",
    lang_specify == "Vietnamese" ~ "7",
    !is.na(lang_specify) & lang_specify != "Unknown" & lang_specify != "Undetermined" ~ "8",
    TRUE ~ NA_character_)) %>%
  
  #Drop vars not needed
  select(-race_eth, -race_eth_sub_grp, -language, -lang_specify, -wdrs_id)

## Cluster to household level
wdrs_demo_household <- wdrs_demo_household %>%
  group_by(agency, arm, record_id) %>%
  
  mutate(
    #Take maximum language when multiple values exist for an arm-record ID
    #Note ifelse prevents Inf values as compared to case_when
    language_norm = ifelse(sum(!is.na(language_norm)) == 0, NA_character_, as.character(max(as.integer(language_norm), na.rm = T))),
    
    #Take minimum date of birth (i.e. oldest household member)
    dob_norm = ifelse(sum(!is.na(birth_date)) == 0, NA_character_, as.character(min(as.Date(birth_date, origin = origin), na.rm = T))),
    
    #Set race to multiple race if more than race reported at household
    race_eth_count = n_distinct(race_eth_norm),
    race_eth_norm = case_when(
      race_eth_count > 1 ~ "Multiple race",
      TRUE ~ race_eth_norm
    ),
    
    #Take min ZIP code and census tract
    reporting_zipcode = ifelse(sum(!is.na(reporting_zipcode)) == 0, NA_character_, min(reporting_zipcode, na.rm = T)),
    census_tractid = ifelse(sum(!is.na(census_tractid)) == 0, NA_character_, min(census_tractid, na.rm = T))) %>%
  select(-race_eth_count, -birth_date) %>%
  
  #Take max of all race-specific variables
  mutate_at(
    vars(race_aian:race_marshallese),
    ~(max(., na.rm = T))
  ) %>%
  
  ungroup() %>%
  distinct() %>%
  
  #Convert language from numeric to character names
  mutate(
    language_norm = case_when(
      language_norm == "1" ~ "English",
      language_norm == "2" ~ "Chinese",
      language_norm == "3" ~ "Korean",
      language_norm == "4" ~ "Russian",
      language_norm == "5" ~ "Somali",
      language_norm == "6" ~ "Spanish",
      language_norm == "7" ~ "Vietnamese",
      language_norm == "8" ~ "Another language",
      TRUE ~ NA_character_))


#### STEP 9: Bring in census tract-level SERI scores ####
col_types <- cols(.default = col_character())
seri_census <- read_csv("//phshare01/CDI_SHARE/Analytics and Informatics Team/COVID Daily Reports/SAS Code/Census Tract Analyses/data/seri_census.csv", col_types = col_types)
rm(col_types)

#Join to wdrs-based, household-level demographic data
wdrs_demo_household <- left_join(wdrs_demo_household, select(seri_census, census_tractid, composite_score), by = c("census_tractid" = "census_tractid")) %>%
  
  #rename variables to differentiate from CC variables
  rename(seri_composite_score = composite_score,
         race_eth_norm_wdrs = race_eth_norm,
         language_norm_wdrs = language_norm,
         dob_norm_wdrs = dob_norm,
         race_aian_wdrs = race_aian,
         race_asian_wdrs = race_asian,
         race_black_wdrs = race_black,
         race_latino_wdrs = race_latino,
         race_nhpi_wdrs = race_nhpi,
         race_white_wdrs = race_white,
         race_asian_indian_wdrs = race_asian_indian,
         race_chinese_wdrs = race_chinese,
         race_filipino_wdrs = race_filipino,
         race_japanese_wdrs = race_japanese,
         race_korean_wdrs = race_korean,
         race_vietnamese_wdrs = race_vietnamese,
         race_cambodian_wdrs = race_cambodian,
         race_thai_wdrs = race_thai,
         race_aa_wdrs = race_aa,
         race_somali_wdrs = race_somali,
         race_ethiopian_wdrs = race_ethiopian,
         race_eritrean_wdrs = race_eritrean,
         race_kenyan_wdrs = race_kenyan,
         race_mexican_wdrs = race_mexican,
         race_cuban_wdrs = race_cuban,
         race_nh_wdrs = race_nh,
         race_samoan_wdrs = race_samoan,
         race_marshallese_wdrs = race_marshallese)

rm(seri_census)


#### STEP 10: Combine referral-based demographics with A&I demographics ####
complete_demo <- full_join(complete_referral_based_demo, wdrs_demo_household, by = c("agency", "arm", "record_id")) %>%
  
  #Drop unneeded vars
  select(-chw_language, -chw_dob, -race_eth_sum) %>%
  
  #Replace missing referral-based demographic fields with data from A&I
  mutate(
    race_eth_norm = case_when(is.na(race_eth_norm) & !is.na(race_eth_norm_wdrs) ~ race_eth_norm_wdrs, TRUE ~ race_eth_norm),
    language_norm = case_when(is.na(language_norm) & !is.na(language_norm_wdrs) ~ language_norm_wdrs, TRUE ~ language_norm),
    dob_norm = case_when(is.na(dob_norm) & !is.na(dob_norm_wdrs) ~ dob_norm_wdrs, TRUE ~ dob_norm),
    
    race_aian = case_when((is.na(race_aian) | race_aian == 0) & !is.na(race_aian_wdrs) ~ race_aian_wdrs, TRUE ~ race_aian),
    race_asian = case_when((is.na(race_asian) | race_asian == 0) & !is.na(race_asian_wdrs) ~ race_asian_wdrs, TRUE ~ race_asian),
    race_black = case_when((is.na(race_black) | race_black == 0) & !is.na(race_black_wdrs) ~ race_black_wdrs, TRUE ~ race_black),
    race_latino = case_when((is.na(race_latino) | race_latino == 0) & !is.na(race_latino_wdrs) ~ race_latino_wdrs, TRUE ~ race_latino),
    race_nhpi = case_when((is.na(race_nhpi) | race_nhpi == 0) & !is.na(race_nhpi_wdrs) ~ race_nhpi_wdrs, TRUE ~ race_nhpi),
    race_white = case_when((is.na(race_white) | race_white == 0) & !is.na(race_white_wdrs) ~ race_white_wdrs, TRUE ~ race_white),
    race_asian_indian = case_when((is.na(race_asian_indian) | race_asian_indian == 0) & !is.na(race_asian_indian_wdrs) ~ race_asian_indian_wdrs, TRUE ~ race_asian_indian),
    race_chinese = case_when((is.na(race_chinese) | race_chinese == 0) & !is.na(race_chinese_wdrs) ~ race_chinese_wdrs, TRUE ~ race_chinese),
    race_filipino = case_when((is.na(race_filipino) | race_filipino == 0) & !is.na(race_filipino_wdrs) ~ race_filipino_wdrs, TRUE ~ race_filipino),
    race_japanese = case_when((is.na(race_japanese) | race_japanese == 0) & !is.na(race_japanese_wdrs) ~ race_japanese_wdrs, TRUE ~ race_japanese),
    race_korean = case_when((is.na(race_korean) | race_korean == 0) & !is.na(race_korean_wdrs) ~ race_korean_wdrs, TRUE ~ race_korean),
    race_vietnamese = case_when((is.na(race_vietnamese) | race_vietnamese == 0) & !is.na(race_vietnamese_wdrs) ~ race_vietnamese_wdrs, TRUE ~ race_vietnamese),
    race_cambodian = case_when((is.na(race_cambodian) | race_cambodian == 0) & !is.na(race_cambodian_wdrs) ~ race_cambodian_wdrs, TRUE ~ race_cambodian),
    race_thai = case_when((is.na(race_thai) | race_thai == 0) & !is.na(race_thai_wdrs) ~ race_thai_wdrs, TRUE ~ race_thai),
    race_aa = case_when((is.na(race_aa) | race_aa == 0) & !is.na(race_aa_wdrs) ~ race_aa_wdrs, TRUE ~ race_aa),
    race_somali = case_when((is.na(race_somali) | race_somali == 0) & !is.na(race_somali_wdrs) ~ race_somali_wdrs, TRUE ~ race_somali),
    race_ethiopian = case_when((is.na(race_ethiopian) | race_ethiopian == 0) & !is.na(race_ethiopian_wdrs) ~ race_ethiopian_wdrs, TRUE ~ race_ethiopian),
    race_eritrean = case_when((is.na(race_eritrean) | race_eritrean == 0) & !is.na(race_eritrean_wdrs) ~ race_eritrean_wdrs, TRUE ~ race_eritrean),
    race_kenyan = case_when((is.na(race_kenyan) | race_kenyan == 0) & !is.na(race_kenyan_wdrs) ~ race_kenyan_wdrs, TRUE ~ race_kenyan),
    race_mexican = case_when((is.na(race_mexican) | race_mexican == 0) & !is.na(race_mexican_wdrs) ~ race_mexican_wdrs, TRUE ~ race_mexican),
    race_cuban = case_when((is.na(race_cuban) | race_cuban == 0) & !is.na(race_cuban_wdrs) ~ race_cuban_wdrs, TRUE ~ race_cuban),
    race_nh = case_when((is.na(race_nh) | race_nh == 0) & !is.na(race_nh_wdrs) ~ race_nh_wdrs, TRUE ~ race_nh),
    race_samoan = case_when((is.na(race_samoan) | race_samoan == 0) & !is.na(race_samoan_wdrs) ~ race_samoan_wdrs, TRUE ~ race_samoan),
    race_marshallese = case_when((is.na(race_marshallese) | race_marshallese == 0) & !is.na(race_marshallese_wdrs) ~ race_marshallese_wdrs, TRUE ~ race_marshallese)) %>%
  
  #Drop helper vars
  select(-race_eth_norm_wdrs, -language_norm_wdrs, -dob_norm_wdrs, -race_aian_wdrs:-race_marshallese_wdrs)

rm(complete_referral_based_demo, wdrs_demo_household)


#### STEP 11: Add in household size var to demographic dataset ####
complete_demo <- left_join(complete_demo, select(complete_cc_household, agency, arm, record_id, hh_size), by = c("agency", "arm", "record_id"))


#### STEP 12: Bind PHSKC-assigned and DOH-assigned referrals ####
complete_referral_form <- bind_rows(kc_referral_form, doh_referral_form) %>%
  select(-chw_language:-chw_language_other, -chw_raceethnicity___1:-other_nhpi_2, -language_c:-race_eth, -redcap_event_name) %>%
  select(agency, arm, record_id, referral_date, referral_timedt, call_attempt, everything())

rm(kc_referral_form, doh_referral_form)


#### STEP 13: Join to household-level demographics ####
complete_referral_form_with_demo <- left_join(complete_referral_form, complete_demo, by = c("agency", "arm", "record_id"))


#### STEP 14: Collapse referral data to record ID level ####

#Take max of all referral type variables by arm and record ID
complete_referrals_by_household <- complete_referral_form %>%
              
  #select variables needed
  select(agency, arm, record_id, chw:test_referral, healthinsurance:senior, chw_chap:chw_other) %>%
  
  #Take max of each variable by arm and record ID
  #ifelse function prevents creation of Inf values when all values in group are NA
  group_by(agency, arm, record_id) %>%
  summarize_at(
    vars(chw:chw_other),
    ~(ifelse(sum(!is.na(.)) == 0, NA_integer_, max(., na.rm = T)))) %>%
  ungroup() %>%
  
  #Create count of referrals by arm and record ID
  rowwise() %>%
  mutate(
    ct_referral_count = sum(c_across(swiq:senior), na.rm = T),
    chw_referral_count = sum(c_across(chw_chap:chw_other), na.rm = T))%>%
  ungroup() %>%
  
  mutate(
    any_referral = case_when(ct_referral_count >= 1 | chw_referral_count >= 1 ~ 1L, TRUE ~ 0L),
    ct_any_referral = case_when(ct_referral_count >= 1 ~ 1L, TRUE ~ 0L),
    chw_any_referral = case_when(chw_referral_count >= 1 ~ 1L, TRUE ~ 0L)) %>%
  
  distinct()