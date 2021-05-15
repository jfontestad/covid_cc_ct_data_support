#### Purpose: Load COVID-19 case data exported daily by A&I, and load a weekly dataset maintained by CT
# Eli Kern, PHSKC (APDE)
# 2021-05

#### STEP 1: Load both datasets ####
base_conf_prob_cases <- haven::read_sas("//phshare01/cdi_share/Analytics and Informatics Team/COVID/Datasets/main code outputs/WDRS Datasets/base_conf_prob_cases.sas7bdat", NULL)
crf_wdrs_public <- haven::read_sas("//phshare01/cdi_share/Outbreaks & Investigations/Outbreaks 2020/2019-nCoV/Contact Tracing/Epi/Tableau Dashboard/crf_wdrs_public.sas7bdat", NULL)

#remove SAS formatting
base_conf_prob_cases[] <- lapply(base_conf_prob_cases, function(x) {attributes(x) <- NULL; x})
crf_wdrs_public[] <- lapply(crf_wdrs_public, function(x) {attributes(x) <- NULL; x})


#### STEP 2: Pull out desired variables from A&I dataset ####
wdrs_temp <- base_conf_prob_cases %>%
  select(CASE_ID, language, lang_specify, race_eth, race_eth_sub_grp, REPORTING_ZIPCODE, census_tractid, householdid, locationid, INVESTIGATION_START_DATE, INVESTIGATION_STATUS,
         investigator) %>%
  rename(case_id = CASE_ID,
         reporting_zipcode = REPORTING_ZIPCODE,
         investigation_start_date = INVESTIGATION_START_DATE,
         investigation_status = INVESTIGATION_STATUS)

#Clean ZIP code and dates
wdrs_temp <- wdrs_temp %>%
  mutate(reporting_zipcode = str_sub(str_trim(reporting_zipcode, side = c("both")), start = 1, end = 5),
         
         reporting_zipcode = case_when(
           str_length(reporting_zipcode) == 5 & 
             str_detect(reporting_zipcode, "[:digit:][:digit:][:digit:][:digit:][:digit:]") == T &
             reporting_zipcode != "00000"
           ~ reporting_zipcode,
           TRUE ~ NA_character_),
         
         investigation_start_date = as.Date(as.POSIXct(investigation_start_date, origin = "1970-01-01 00:00:00")))

#Set blanks to missing
wdrs_temp <- wdrs_temp %>%
  mutate_at(
    vars(lang_specify, race_eth_sub_grp, census_tractid, investigation_status, investigator),
    ~(na_if(., "")))


#### STEP 3: Prep CT dataset for merge ####
crf_wdrs_public_mod <- crf_wdrs_public %>%
  rename(investigator_ct = investigator,
         investigation_status_ct = investigation_status) %>%
  mutate(record_id = as.character(record_id)) %>%
  select(record_id, investigator_ct, investigation_status_ct, final_dispo)

#Set blanks to missing
crf_wdrs_public_mod <- crf_wdrs_public_mod %>%
  mutate_at(
    vars(investigator_ct, investigation_status_ct, final_dispo),
    ~(na_if(., "")))

#### STEP 4: Join and fill in missing information ####
wdrs_final <- left_join(wdrs_temp, crf_wdrs_public_mod, by = c("case_id" = "record_id")) %>%
  
  mutate(
    #assign missing investigator based on final_dispo field in CT dataset
    investigator = case_when(
      !is.na(investigator) ~ investigator,
      is.na(investigator) & !is.na(final_dispo) ~ "PHSKC",
      is.na(investigator) & is.na(final_dispo) ~ "DOH",
      TRUE ~ NA_character_),
    
    #assign missing investigation status based on final_dispo field in CT dataset
    investigation_status = case_when(
      !is.na(investigation_status) ~ investigation_status,
      is.na(investigation_status) & !is.na(final_dispo) ~ final_dispo,
      TRUE ~ NA_character_))

#Clean up investigator field
wdrs_final <- wdrs_final %>%
  
  mutate(
    #Normalize agency names
    investigator = case_when(
      investigator %in% c("King COVID-19 Investigator", "PHSKC") ~ "PHSKC",
      investigator %in% c("CREST COVID-19 Investigator", "DOH COVID-19 Investigator", "DOH", "DOH COVID-19 REDCap Direct to Case", "DOH Death Epidemiologist",
                          "Skagit COVID19 Investigator") ~ "DOH",
      TRUE ~ investigator),
    
    #Sort individually named investigator cases to agency based on final_dispo variable
    investigator = case_when(
      investigator %in% c("PHSKC", "DOH") ~ investigator,
      !is.na(final_dispo) ~ "PHSKC",
      is.na(final_dispo) ~ "DOH")) %>%
  
  #Drop helper variables
  select(-investigator_ct, -investigation_status_ct, -final_dispo) %>%
  
  #Take distinct
  distinct()

#Drop datasets no longer needed
rm(base_conf_prob_cases, crf_wdrs_public, crf_wdrs_public_mod, wdrs_temp)

#QA check for one row per case
wdrs_final_rows_per_case = nrow(wdrs_final %>% group_by(case_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count))