#### Purpose: Load KC-assigned current and historical data
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Set tokens and file paths ####
#Set REDCap API tokens
redcap_uri <- "https://redcap.iths.org/api/"
#keyring::key_set("token_phskc_cc") #Run once per machine to store API token using keyring package

#Set file paths
archive_import <- "//Phshare01/cdi_share/Outbreaks & Investigations/Outbreaks 2020/2019-nCoV/Contact Tracing/Household Contact Tracing Project - Archive/Old data/Cumulative/"

#### STEP 2: Bring in current REDCap data from Household Contact Tracing project ####

#Run time: ~1 min
col_types <- cols(.default = col_character())

system.time(kc_cc_current_raw <- REDCapR::redcap_read(
  redcap_uri = redcap_uri,
  token = keyring::key_get("token_phskc_cc"), #Retrieve Eli's token for "Household Contact Tracing" Project from keyring package
  batch_size = 5000L,
  interbatch_delay = 1,
  col_types = col_types
)$data)

rm(col_types)

##Normalize date variables
kc_cc_current <- kc_cc_current_raw %>%

  mutate_at(
    vars(
      ind_overview_dt,
      contact_dob,
      cdc_n_cov_2019_contact_exposure_last_date,
      endofquarantine_dt,
      case_form_created,
      case_dob,
      contact_dob_1,
      contact_dob_2,
      endofisolation_dt,
      sx_onset_dt,
      last_fever_dt,
      hh_overview_dt,
      hh_overview_dt_new,
      exp_hh,
      endofquarantine_dt_1,
      endofquarantine_dt_2,
      endofquarantine_dt_3,
      endofquarantine_dt_4,
      iq_hh_enddt,
      fup_dt,
      fup_last_lefthome_dt,
      ind_contact_dob,
      symptom_change_dt,
      testing_dt,
      letter_iq_enddate,
      togopack_mailed,
      delivery_dt,
      complete_dt,
      har_startdt),
    ~(as.Date(., origin = origin))) %>%
  
  #create arm variable that is in archived data       
  mutate(arm = case_when(
           redcap_event_name %in% c("cases_arm_1", "contacts_arm_1", "household_arm_1") ~ "1",
           redcap_event_name %in% c("demographics__base_arm_2", "followup_arm_2") ~ "2",
           redcap_event_name %in% c("demographics__base_arm_3", "followup_arm_3") ~ "3",
           TRUE ~ NA_character_))

  #Temporary code for removing columns in current data but not yet in archive
  #%>% select(-oximeter_mailed_by, -safeway_account)

#QA arm variable
count(kc_cc_current, arm, redcap_event_name)


#### STEP 3: Bring in archived REDCap data from Household Contact Tracing project ####
system.time(kc_cc_archive_raw <- readRDS(file.path(archive_import, "all_old_data.RDS")))

##Normalize date and other variables
kc_cc_archive <- kc_cc_archive_raw %>%

  #convert all double, integer and logical variables to character
  mutate_if(is.double, as.character) %>%
  mutate_if(is.integer, as.character) %>%
  mutate_if(is.logical, as.character) %>%
  
  #normalize date variables
  mutate_at(
    vars(
      ind_overview_dt,
      contact_dob,
      cdc_n_cov_2019_contact_exposure_last_date,
      endofquarantine_dt,
      case_form_created,
      case_dob,
      contact_dob_1,
      contact_dob_2,
      endofisolation_dt,
      sx_onset_dt,
      last_fever_dt,
      hh_overview_dt,
      hh_overview_dt_new,
      exp_hh,
      endofquarantine_dt_1,
      endofquarantine_dt_2,
      endofquarantine_dt_3,
      endofquarantine_dt_4,
      iq_hh_enddt,
      fup_dt,
      fup_last_lefthome_dt,
      ind_contact_dob,
      symptom_change_dt,
      testing_dt,
      letter_iq_enddate,
      togopack_mailed,
      delivery_dt,
      complete_dt,
      har_startdt),
    ~(as.Date(., origin = origin))) %>%
  
  #drop extra vars not in current data
  select(-grocery_order_form_timestamp, -redcap_survey_identifier)
  
  #add extra vars that have not yet made it into
  if("har_incoming_calls_followup" %in% colnames(kc_cc_archive) == FALSE) {kc_cc_archive <- kc_cc_archive %>% mutate(har_incoming_calls_followup = NA_character_)}
  if("call_arm3_endiq" %in% colnames(kc_cc_archive) == FALSE) {kc_cc_archive <- kc_cc_archive %>% mutate(call_arm3_endiq = NA_character_)}
  if("vx_assist_arm3" %in% colnames(kc_cc_archive) == FALSE) {kc_cc_archive <- kc_cc_archive %>% mutate(vx_assist_arm3 = NA_character_)}
  if("vx_call_dispo_arm13" %in% colnames(kc_cc_archive) == FALSE) {kc_cc_archive <- kc_cc_archive %>% mutate(vx_call_dispo_arm13 = NA_character_)}
  if("vx_call_dispo_arm2" %in% colnames(kc_cc_archive) == FALSE) {kc_cc_archive <- kc_cc_archive %>% mutate(vx_call_dispo_arm2 = NA_character_)}

#### STEP 4: Compare columns in current and archived Household Contact Tracing data, and bind
kc_cc_archive_current_coldiff <- nrow(janitor::compare_df_cols(kc_cc_archive, kc_cc_current, return = "mismatch", bind_method = "rbind"))
#janitor::compare_df_cols(kc_cc_archive, kc_cc_current, return = "mismatch", bind_method = "rbind") #run for debugging
kc_cc_complete <- bind_rows(kc_cc_current, kc_cc_archive) %>% distinct()