#### Purpose: Clean and combine case, contact and household COVID-19-related data for PHSKC-assigned and DOH-assigned households
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Process case and contact data by household for all 3 arms in PHSKC-assigned household data ####

## Pull WDRS IDs for all cases, keeping only valid WDRS IDs
kc_wdrs <- kc_cc_complete %>% select(arm, record_id, contains("wdrs"))
kc_wdrs_long <- pivot_longer(kc_wdrs, !arm:record_id, names_to = "wdrs_id_type", values_to = "wdrs_id") %>%
  mutate(wdrs_id = str_trim(wdrs_id, side = "both")) %>% #remove white space around WDRS IDs
  filter(wdrs_id != "" & !is.na(wdrs_id) & str_sub(wdrs_id, 1, 1) == "1" & str_length(wdrs_id) == 9 & str_detect(wdrs_id, "[:punct:]") == F) %>%
  mutate(agency = "phskc") %>%
  select(agency, arm, record_id, wdrs_id_type, wdrs_id)

## Count cases per arm 1 household (works only for old phase data)
kc_cases_per_house <- kc_cc_complete %>%
  filter((redcap_event_name == "cases_arm_1" | (redcap_event_name == "contacts_arm_1" & contact_became_case_yn == "1"))
         & (redcap_repeat_instrument == "" | is.na(redcap_repeat_instrument)))

kc_cases_per_house <- kc_cases_per_house %>%
  group_by(arm, record_id) %>%
  summarise(cases_per_house = n(), .groups = "drop")

## Count contacts per arm 1 household (works only for old phase data)
kc_contacts_per_house <- kc_cc_complete %>%
  filter(redcap_event_name == "contacts_arm_1" & (contact_became_case_yn != "1" | is.na(contact_became_case_yn))
         & (redcap_repeat_instrument == "" | is.na(redcap_repeat_instrument)))

kc_contacts_per_house <- kc_contacts_per_house %>%
  group_by(arm, record_id) %>%
  summarise(contacts_per_house = n(), .groups = "drop")

## Create a household-level data frame
kc_cc_household <- kc_cc_complete %>%
  
  #remove referral and followup rows
  filter(redcap_event_name %in% c("household_arm_1", "demographics__base_arm_2", "demographics__base_arm_3")
         & is.na(redcap_repeat_instrument)) %>%
  
  #remove arm 1 with no household overview date
  filter(!is.na(hh_overview_dt) | !is.na(hh_overview_dt_new) | arm %in% c("2", "3")) %>%
  
  #create final date variable
  mutate(form_created = case_when(
    
    #arm 3 and arm 2 contacts who became cases
    arm == "3" ~ case_form_created,
    arm == "2" & contact_became_case_yn == "1" ~ case_form_created,
    arm == "2" & (contact_became_case_yn == "0" | is.na(contact_became_case_yn)) ~ ind_overview_dt,
    arm == "1" & !is.na(hh_overview_dt) ~ hh_overview_dt,
    arm == "1" & !is.na(hh_overview_dt_new) ~ hh_overview_dt_new,
    TRUE ~ lubridate::NA_Date_),
    
    #create agency variable
    agency = "phskc"
    
    ) %>%
  
  #filter rows without any date
  filter(!is.na(form_created)) %>%
  
  #select desired variables
  select(agency, arm, record_id, form_created, hh_anycases, hh_no_cases, hh_no_contacts, n_hh_members,
         household_zipcode, hhpoc_eng, hhpoc_language, contactenglishspeaker, contactlanguageother, contact_became_case_yn, caseenglishspeaker,
         caselanguageother)

## Join to case and contact counts, and normalize variables
kc_cc_household <- left_join(kc_cc_household, kc_cases_per_house, by = c("arm" = "arm", "record_id" = "record_id"))
kc_cc_household <- left_join(kc_cc_household, kc_contacts_per_house, by = c("arm" = "arm", "record_id" = "record_id"))

#QA check to make sure 1 row per record ID
kc_cc_household_row_count <- nrow(kc_cc_household %>% group_by(arm, record_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count))

kc_cc_household <- kc_cc_household %>%
  
  mutate(
    cases_per_house = case_when(
      arm == "2" & contact_became_case_yn == "1" ~ 1L,
      arm == "2" & (contact_became_case_yn == "0" | is.na(contact_became_case_yn)) ~ 0L,
      arm == "3" ~ 1L,
      arm == "1" & !is.na(hh_no_cases) ~ as.integer(hh_no_cases),
      arm == "1" & !is.na(cases_per_house) ~ cases_per_house,
      arm == "1" & is.na(hh_no_cases) & is.na(cases_per_house) ~ 0L,
      TRUE ~ NA_integer_),
    
    contacts_per_house = case_when(
      arm == "2" & contact_became_case_yn == "1" ~ 0L,
      arm == "2" & (contact_became_case_yn == "0" | is.na(contact_became_case_yn)) ~ 1L,
      arm == "3" ~ 0L,
      arm == "1" & !is.na(hh_no_contacts) ~ as.integer(hh_no_contacts),
      arm == "1" & !is.na(contacts_per_house) ~ contacts_per_house,
      arm == "1" & is.na(hh_no_contacts) & is.na(contacts_per_house) ~ 0L,
      TRUE ~ NA_integer_),
    
    #take max of reported household size or cases + contacts when they differ
    hh_size = case_when(
      arm %in% c("2", "3") ~ 1L,
      arm == "1" & is.na(n_hh_members) ~ cases_per_house + contacts_per_house,
      arm == "1" & as.integer(n_hh_members) >= cases_per_house + contacts_per_house ~ as.integer(n_hh_members),
      arm == "1" & as.integer(n_hh_members) < cases_per_house + contacts_per_house ~ cases_per_house + contacts_per_house,
      TRUE ~ NA_integer_),
    
    #harmonize language
    language_english_flag = case_when(
      hhpoc_eng == "1" | caseenglishspeaker == "1" | contactenglishspeaker == "1" ~ "1",
      hhpoc_eng == "0" | caseenglishspeaker == "0" | contactenglishspeaker == "1" ~ "0",
      TRUE ~ NA_character_),
    
    language_name = case_when(
      language_english_flag == "1" ~ "ENGLISH",
      !is.na(contactlanguageother) ~ str_to_upper(contactlanguageother),
      !is.na(caselanguageother) ~ str_to_upper(caselanguageother),
      !is.na(hhpoc_language) ~ str_to_upper(hhpoc_language),
      TRUE ~ NA_character_),
    
    #replace other and unknown with missing
    language_name = case_when(
      language_name %in% c("OTHER", "UNKNOWN") ~ NA_character_,
      TRUE ~ language_name)) %>%
  
  #select final variables
  select(agency, arm, record_id, form_created, hh_size, cases_per_house, contacts_per_house, language_english_flag, language_name, household_zipcode, contact_became_case_yn)


#### STEP 2: Process case and contact data for DOH-assigned household data ####

## Pull WDRS IDs for all cases, keeping only valid WDRS IDs
doh_cc_wdrs <- select(doh_cc_current, redcap_record_id, wdrs_event_id, first_name_c, last_name_c) %>%
  
  rename(wdrs_id = wdrs_event_id) %>%
  
  #keep only valid WDRS IDs
  filter(wdrs_id != "" & !is.na(wdrs_id) & str_sub(wdrs_id, 1, 1) == "1" & str_length(wdrs_id) == 9 & str_detect(wdrs_id, "[:punct:]") == F) %>%
  
  #Variable processing, renaming, etc.
  mutate(agency = "doh", arm = "doh-assigned") %>%
  select(agency, arm, redcap_record_id, wdrs_id) %>%
  distinct()

## Import household clustering dataset
# Eventually replace this with path or SQL connection to permanent file
hh_cluster_file_path <- "//Phshare01/cdi_share/Outbreaks & Investigations/Outbreaks 2020/2019-nCoV/Contact Tracing/Epi/Care Coordination/Analytic Datasets/hh_cluster_vars.csv"
col_types <- cols(.default = col_character())
case_household_cluster <- read_csv(hh_cluster_file_path, col_types = col_types) %>%
  select(CASE_ID, locationid, householdid)
rm(col_types)

## Join DOH cases to household cluster dataset to group cases into households
doh_cc_wdrs_household <- left_join(doh_cc_wdrs, case_household_cluster, by = c("wdrs_id" = "CASE_ID")) %>%
  
  #Assign household ID based on following logic: 1) cluster based on hh address, 2) cluster based on hh lat/long, 3) cluster based on REDCap record ID (i.e. phone #)
  mutate(record_id = case_when(
    !is.na(householdid) ~ householdid,
    !is.na(locationid) & is.na(householdid) ~ locationid,
    is.na(householdid) & is.na(locationid) ~ redcap_record_id,
    TRUE ~ NA_character_
  )) %>%
  
  select(agency, arm, record_id, redcap_record_id, wdrs_id)

## Add new record_id to doh_cc_current dataframe for use in referral date cleaning
doh_cc_current_clustered <- left_join(doh_cc_current, select(doh_cc_wdrs_household, record_id, redcap_record_id), by = c("redcap_record_id")) %>%
  
  #rename
  
  #fill in missing record_id_hh with record_id if null
  mutate(record_id = case_when(
    is.na(record_id) ~ redcap_record_id,
    TRUE ~ record_id
  )) %>%

  select(arm, record_id, redcap_record_id, everything())


## Normalize DOH-assigned household data to match PHSKC-assigned data
doh_cc_household <- doh_cc_current_clustered %>%
  
  mutate(
    agency = "doh",
    
    #date of CI/CT record
    form_created = case_when(
      !is.na(created_on) ~ as.Date(created_on, origin = origin),
      !is.na(today_novariableneeded) ~ as.Date(today_novariableneeded, origin = origin),
      !is.na(call_attempt) ~ as.Date(call_attempt, origin = origin),
      TRUE ~ lubridate::NA_Date_),
    
    #household size
    hh_size = case_when(
      !is.na(how_many_people_live_in_your_hou) ~ as.integer(how_many_people_live_in_your_hou),
      !is.na(hh_size_sum) ~ as.integer(hh_size_sum),
      TRUE ~ NA_integer_),
    
    #household ZIP code
    household_zipcode = case_when(
      str_detect(str_sub(str_trim(postal_code, side = c("both")),1,5), "[:digit:][:digit:][:digit:][:digit:][:digit:]") ~ str_sub(str_trim(postal_code, side = c("both")),1,5),
      TRUE ~ NA_character_)
  ) %>%
  
  select(agency, arm, record_id, redcap_record_id, form_created, hh_size, household_zipcode)


#### STEP 3: Bind PHSKC-assigned and DOH-assigned case, contact and household data ####
complete_cc_household <- bind_rows(kc_cc_household, doh_cc_household)
complete_cc_wdrs <- bind_rows(kc_wdrs_long, doh_cc_wdrs_household)