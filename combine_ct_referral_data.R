#### Purpose: Combine cleaned case/contact/household and referral data
# Eli Kern, PHSKC (APDE)
# 2021-05


#### STEP 1: Combine cleaned case/contact/household data, referral data, and household-level demographics data ####

final_data_by_household <- left_join(complete_cc_household, complete_referrals_by_household, by = c("agency", "arm", "record_id"))

final_data_by_household <- left_join(final_data_by_household, complete_referral_based_demo, by = c("agency", "arm", "record_id"))

## Convert all NAs to 0s for households with no referrals
final_data_by_household <- final_data_by_household %>%
  mutate_at(
    vars(chw:chw_any_referral),
    list(~ case_when(
      is.na(.) ~ 0L,
      TRUE ~ .)))

#QA check for 1 row per record ID
final_data_by_household_row_count <- nrow(
  final_data_by_household %>% group_by(agency, arm, record_id) %>% mutate(row_count = n()) %>% ungroup() %>% count(row_count))