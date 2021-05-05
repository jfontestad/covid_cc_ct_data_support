#### Purpose: Load DOH-assigned data from REDCap
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Set tokens and file paths ####
#Set REDCap API tokens
redcap_uri <- "https://redcap.iths.org/api/"
#keyring::key_set("token_doh_cc") #Run once per machine to store API token using keyring package


#### STEP 2: Bring in REDCap data from "Care Coordination - COVID19" project ####
#These are all DOH assigned cases/households
#Record ID is household phone #
#Run time: ~1 min
col_types <- cols(.default = col_character())

system.time(doh_cc_current_raw <- REDCapR::redcap_read(
  redcap_uri = redcap_uri,
  token = keyring::key_get("token_doh_cc"), #Retrieve Eli's token for "Care Coordination - COVID19" Project from keyring package
  batch_size = 5000L,
  interbatch_delay = 1,
  col_types = col_types
)$data)

rm(col_types)

#Create arm variable
doh_cc_current <- doh_cc_current_raw %>%
  
  #Convert select variables to dates
  mutate_at(
    vars(har_startdt),
    ~(as.Date(., origin = origin))) %>%
  
  mutate(arm = "doh-assigned") %>%
  
  #Remove dummy records from survey submission testing
  mutate(drop_flag = case_when(
    #drop records with missing names submitted via survey
    record_form_svy == "survey" & is.na(first_name_c) ~ 1,
    #drop specific records observed to be dummy records, submitted via survey
    record_id %in% c("9842429051", "9842429060", "9842429045", "9842429033", "9842429050") ~ 1,
    TRUE ~ 0
  )) %>%
  filter(drop_flag != 1) %>%
  select(-drop_flag)