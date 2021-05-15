#### Purpose: Clean and combine HAR referral data for PHSKC-assigned and DOH-assigned households
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Clean PHSKC-assigned household HAR referral data ####
kc_har_form <- kc_cc_complete %>% 
  
  #Filter to households who received a HAR referral
  filter(!is.na(har_startdt)) %>%
  
  #Select HAR form variables
  select(redcap_event_name, record_id, har_startdt:har_screening_complete) %>%
         
  #Add additional variables
  mutate(
   
    #create arm variable
    arm = case_when(
      str_detect(redcap_event_name, "_1") == T ~ "1",
      str_detect(redcap_event_name, "_2") == T ~ "2",
      str_detect(redcap_event_name, "_3") == T ~ "3",
      TRUE ~ NA_character_),
    
    #create agency variable
    agency = "phskc") %>%
  
  #Drop unncessary variables
  select(-redcap_event_name) %>%
  
  #Make sure dataset is distinct
  distinct()


#### STEP 2: Clean DOH-assigned household HAR referral data, adapting from code for PHSKC-assigned households ####
doh_har_form <- doh_cc_current_clustered %>% 
  
  #Filter to households who received a HAR referral
  filter(!is.na(har_startdt)) %>%
  
  #Select HAR form variables
  select(arm, record_id, har_startdt:har_complete) %>%
    
  #Add additional variables
    mutate(
      #create agency variable
      agency = "doh") %>%
  
  #Normalize name of HAR variables
  rename(har_screening_complete = har_complete) %>%
  
  #Make sure dataset is distinct
  distinct()


#### STEP 3: Bind PHSKC-assigned and DOH-assigned HAR referrals ####
complete_har_form <- bind_rows(kc_har_form, doh_har_form) %>%
  select(agency, arm, record_id, everything())

rm(kc_har_form, doh_har_form)

#QA: All records have har_startdt
complete_har_form_qa_check <- count(filter(complete_har_form, is.na(har_startdt)))$n


#### STEP 4: Join to household-level demographics ####
complete_har_form_with_demo <- left_join(complete_har_form, complete_demo, by = c("agency", "arm", "record_id"))