# clean

superkedit <- superk

# get list of consented participants
consentedlist <- superkedit %>% 
  select(record_id, consent_tick) %>%
  group_by(record_id) %>%
  fill(consent_tick) %>%
  filter(consent_tick == 1) %>%
  distinct(record_id)

superkedit <- superkedit %>%
  rename_with(~varnames$newname, varnames$varname) %>%
  rename(completed_base = demographic_questionnaire_timestamp, # get first time-stamped item completed at each timepoint for completed variable
         # completed_pre1 = current_level_of_sport_and_physical_activity_timestamp,
         completed_lab = physical_testing_anthropometrics_timestamp,
         completed_proms = pfoos_timestamp,
         completed_eq5d = eq5d_timestamp,
         fn_completed = fortnightly_activity_monitoring_timestamp,
         mth_completed = monthly_activity_monitoring_timestamp,
         id = record_id) %>%
  filter(id %in% consentedlist$record_id) %>%
  mutate(group = recode(group, '0' = 7, '1' = 4),
         completed_lab = ifelse(completed_lab == "[not completed]", NA, completed_lab),
         sex = case_when( # create sex variable based on response
           sex == 1 ~ "Male", 
           sex == 2 ~ "Female",
           sex == 3 ~ as.character(sex_other)
           ),
         bl_birthcountry = case_when(
           bl_birthcountry == 1 ~ "Australia",
           bl_birthcountry == 2 ~ bl_birthcountrytext
         ),
         bl_dominantleg = recode(bl_dominantleg, '1' = "Right", '2' = "Left"),
         bl_aclrside = recode(bl_aclrside, '1' = "Left", '2' = "Right"),
         bl_aclrside_demographic = recode(bl_aclrside_demographic, '1' = "Left", '2' = "Right")
         ) %>% # recode group variable
  group_by(id) %>%
  fill(dob, .direction = "down") %>%
  ungroup() %>%
  select(!matches("timestamp|complete$|sex_other|bl_birthcountrytext")) %>% # remove un-needed redcap fields 
  select(!any_of( # remove predetermined variables
    varnames %>% filter(remove == "remove") %>% select(newname) %>% unlist() %>% as.character()
    ))

# extract postcode data
postcode <- superkedit %>%
  select(id, address_res) %>%
  filter(!is.na(address_res))

# use address field to search google to get full address
postcode <- bind_cols(postcode, geocode(as.character(postcode$address_res), output = "more")) %>%
  mutate(postcode = str_extract(address, "(?<=\\w{3}\\s)\\d{4}")) # extract postcode from full address

# remove address field from original dataframe
superkedit <- superkedit %>%
  select(!address_res)

# Select fortnighly and monthly questions into separate dataframe and remove from original
fnmonthly <- superkedit %>%
  select(id, redcap_event_name, starts_with("fn_"), starts_with("mth_"))

superkedit <- superkedit %>%
  select(!c(starts_with("fn_"), starts_with("mth_")))

# List of redcap arms for each timepoint
prebaseline <- c("online_consent_and_arm_1", "participant_regist_arm_1")
baseline <- c("prebaseline_questi_arm_1", "baseline_questionn_arm_1", "baseline_testing_arm_1",
              "randomisation_arm_1", "baseline_postrando_arm_1")
fourmonth <- c("pre4_month_questio_arm_1", "4_month_questionna_arm_1", "4_month_testing_arm_1")
eightmonth <- c("8_month_questionna_arm_1")
twelvemonth <- c("pre12_month_questi_arm_1", "12_month_quesitonn_arm_1", "12_month_testing_arm_1")

# Coalesce data into one row per participant

mpre <- superkedit %>%
  filter(redcap_event_name %in% prebaseline) %>%
  select(!starts_with("pfoos")) %>%
  rename_at(vars(starts_with("koos")), ~str_replace(., "koos", "pfoos")) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "mpre")

mpreto0 <- mpre %>% # move baseline questionnaire from pre to timepoint m0. 
  select(id, bl_birthcountry:bl_pass)

mpre <- mpre %>% # remove from timepoint mpre
  select(!bl_birthcountry:bl_pass)

m0 <- superkedit %>%
  filter(redcap_event_name %in% baseline) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  select(!bl_birthcountry:bl_pass) %>% # remove blank columns to be replaced from mpre move
  full_join(., mpreto0, by = "id") %>% # join baseline questionniare to m0 (full join as some mpre)
  mutate(timepoint = "m0",
         baseline_date = date(completed_proms),
         age = trunc((dob %--% baseline_date) / years(1))) 

m4 <- superkedit %>%
  filter(redcap_event_name %in% fourmonth) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "m4")

m8 <- superkedit %>%
  filter(redcap_event_name %in% eightmonth) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "m8")

m12 <- superkedit %>%
  filter(redcap_event_name %in% twelvemonth) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "m12")

# Join together timepoints
superkjoin <- bind_rows(mpre, m0) %>%
  bind_rows(., m4) %>%
  bind_rows(., m8) %>%
  bind_rows(., m12) %>%
  mutate(timepoint = factor(timepoint, levels = c("mpre" = "mpre", "m0" = "m0", "m4" = "m4", "m8" = "m8", "m12" = "m12")), # order timepoint
         completed = case_when( # compute date of completion of data
           !is.na(completed_base) ~ completed_base,
           !is.na(completed_proms) ~ completed_proms,
           is.na(completed_proms) & !is.na(completed_eq5d) ~ completed_eq5d
         )) %>%
  group_by(id) %>% # fill id variables for all timepoints
  fill(sex, .direction = "down") %>%
  fill(age, .direction = "downup") %>%
  fill(bl_dominantleg, .direction = "down") %>%
  fill(bl_aclrside, .direction = "downup") %>%
  fill(bl_aclrside_demographic, .direction = "downup") %>%
  fill(group, .direction = "downup") %>%
  fill(baseline_date, .direction = "downup") %>%
  fill(firstphysiosession_date, .direction = "downup") %>%
  fill(dob, .direction = "downup") %>%
  fill(bl_dominantleg, .direction = "up") %>%
  ungroup() %>%
  select(!c(completed_base, completed_proms, completed_eq5d, completed_lab, redcap_event_name, redcap_survey_identifier, starts_with("koos"))) %>%
  rename_at(vars(starts_with("pfoos")), ~str_replace(., "pfoos", "koos")) %>%
  rowwise() %>% # to calculate totals per participant
  mutate(across(starts_with("koos"), ~abs(.x - 4))) %>% # all the koos scales need to be flipped
  mutate(koos_s4 = abs(koos_s4 - 4), # koos_s4 and 5 have their scale correctin redcap (so need to flip back here)
         koos_s5 = abs(koos_s5 - 4), 
         koos_p_total = 100 - (mean(c_across(matches("koos\\_p\\d+")), na.omit = TRUE) * 100) / 4, # formula for calculating koos subscale totals
         koos_s_total = 100 - (mean(c_across(matches("koos\\_s\\d+")), na.omit = TRUE) * 100) / 4,
         koos_a_total = 100 - (mean(c_across(matches("koos\\_a\\d+")), na.omit = TRUE) * 100) / 4,
         koos_sp_total = 100 - (mean(c_across(matches("koos\\_sp\\d+")), na.omit = TRUE) * 100) /4,
         koos_q_total = 100 - (mean(c_across(matches("koos\\_q\\d+")), na.omit = TRUE) * 100) / 4,
         koos_pf_total = 100 - (mean(c_across(matches("koos\\_pf\\d+")), na.omit = TRUE) * 100) / 4,
         koos_4_total = mean(c(koos_p_total, koos_s_total, koos_sp_total, koos_q_total), na.omit = TRUE),
         aclqol_total = mean(c_across(matches("aclqol\\_\\d+")), na.rm = TRUE),
         pass = ifelse(!is.na(bl_pass), bl_pass, pass),
         completed = date(ymd_hms(completed)),
         timepoint_actual = case_when( # calculate actual time between baseline and timepoint 
           timepoint == "mpre" ~ NA_real_,
           timepoint == "m0" ~ 0,
           !is.na(firstphysiosession_date) ~ round((firstphysiosession_date %--% completed) / weeks(1),2), # if randomised to physio group then base on first session
           TRUE ~ round((baseline_date %--% completed) / weeks(1),2) # else go from baseline lab date.
         )
           ) %>%
  ungroup() %>%
  select(!bl_pass)

# Convert L and R data to side based
superkside <- superkjoin %>%
  select(id, timepoint, bl_aclrside, starts_with("l_"), starts_with("r_")) %>% # select variables with a side
  pivot_longer(-c(id, timepoint, bl_aclrside), names_to = c("side", "var"),
              names_pattern = "(\\w)\\_(.+)", # split names to side and var 
              values_to = "value", values_transform = list(value = as.character)) %>% # mix of types so need to use values_transform
  mutate(side = recode(side, "l" = "Left", "r" = "Right"), # apportion a (affected) and c (contralateral)
         newvar = case_when(
           bl_aclrside == side ~ paste0("a_", var),
           bl_aclrside != side ~ paste0("c_", var),
         )) %>%
  select(!c(var, side)) %>%
  filter(!is.na(newvar)) %>% # remove rows where no data (e.g. no sided data yet)
  pivot_wider(c(id, timepoint, bl_aclrside), names_from = "newvar", values_from = "value")

# Join everything back together again
superkjoin <- superkjoin %>%
  left_join(., superkside, by = c("id", "timepoint", "bl_aclrside")) %>%
  left_join(., postcode %>% select(id, postcode), by = "id") %>%
  select(!c(starts_with("l_"), starts_with("r_"))) %>% # remove side based data.
  rename(dominantleg = bl_dominantleg,
         aclrside = bl_aclrside) %>%
  mutate(aclrside = case_when(is.na(aclrside) ~ bl_aclrside_demographic, TRUE ~ aclrside)) %>%
  select(-bl_aclrside_demographic) %>%
  select(id, sex, age, dominantleg, aclrside, timepoint, timepoint_actual, baseline_date, firstphysiosession_date, completed, mridate, group, dob, postcode, starts_with("bl_"),
         starts_with("koos_"), starts_with("aclqol"), starts_with("tsk"), starts_with("nprs"), starts_with("pass"), starts_with("groc"), starts_with("hlq"), 
         starts_with("wlq"), starts_with("eq5d"), everything()) %>% # order variables
  select(id, sex, age, dominantleg, aclrside, timepoint, timepoint_actual, baseline_date, firstphysiosession_date, completed, mridate, group, dob, postcode, 
         bl_birthcountry:bl_familyhxoadetails, bl_medicalhx:bl_otherpainconditions_otherdetails, everything()) %>% # shuffle order of bl_ questions
  arrange(id, timepoint)

# Fortnightly and Monthly Questions

fnmonthly <- fnmonthly %>%
  filter(str_detect(redcap_event_name, "week|month"),
         !str_detect(redcap_event_name, "^4_month|^12_month")) %>%
  rowwise() %>%
  mutate(timepoint = case_when(
            str_detect(redcap_event_name, "week") ~ paste0("w", str_extract(redcap_event_name, "\\d{1,2}")),
            str_detect(redcap_event_name, "month") ~ paste0("m", str_extract(redcap_event_name, "\\d{1,2}"))
             ),
         completed = case_when(
           !is.na(fn_completed) ~ date(fn_completed),
           !is.na(mth_completed) ~ date(mth_completed)
         )
  ) %>%
  ungroup() %>%
  select(!c(fn_completed, mth_completed, redcap_event_name)) %>%
  mutate(across(matches("sport\\_type\\_\\d"), ~recode(.x, # recode variable to be more informative
                                  "1" = "Soccer",
                                  "2" = "Australian Rules Football",
                                  "3" = "Basketball",
                                  "4" = "Netball",
                                  "5" = "Tennis/Squash",
                                  "6" = "Cricket",
                                  "7" = "Hockey",
                                  "8" = "Volleyball",
                                  "9" = "Martial arts", 
                                  "10" = "Alpine skiing/snowboarding",
                                  "11" = "Dancing/aerobics/gymnastics/ballet",
                                  "12" = "Golf",
                                  "13" = "Running", 
                                  "14" = "Swimming",
                                  "15" = "Cycling",
                                  "16" = "Surfing",
                                  "17" = "Other")),
         fn_otherprof_gp = fn_whichotherprof_super___1 + fn_whichotherprof_control___1, # combine questions from 2 separate arms (super vs control)
         fn_otherprof_physio = fn_whichotherprof_super___2 + fn_whichotherprof_control___2,
         fn_otherprof_chiro = fn_whichotherprof_super___3 + fn_whichotherprof_control___3,
         fn_otherprof_osteoeo = fn_whichotherprof_super___4 + fn_whichotherprof_control___4,
         fn_otherprof_massage = fn_whichotherprof_super___5 + fn_whichotherprof_control___5,
         fn_otherprof_naturopath = fn_whichotherprof_super___6 + fn_whichotherprof_control___6,
         fn_otherprof_sportsdoc = fn_whichotherprof_super___7 + fn_whichotherprof_control___7,
         fn_otherprof_orthopod = fn_whichotherprof_super___8 + fn_whichotherprof_control___8,
         fn_otherprof_other = fn_whichotherprof_super___9 + fn_whichotherprof_control___9,
         fn_otherprof_otherdetail = fn_whichotherprof_other,
         mth_otherprof_gp = mth_whichotherprof_super___1 + mth_whichotherprof_control___1,
         mth_otherprof_physio = mth_whichotherprof_super___2 + mth_whichotherprof_control___2,
         mth_otherprof_chiro = mth_whichotherprof_super___3 + mth_whichotherprof_control___3,
         mth_otherprof_osteoeo = mth_whichotherprof_super___4 + mth_whichotherprof_control___4,
         mth_otherprof_massage = mth_whichotherprof_super___5 + mth_whichotherprof_control___5,
         mth_otherprof_naturopath = mth_whichotherprof_super___6 + mth_whichotherprof_control___6,
         mth_otherprof_sportsdoc = mth_whichotherprof_super___7 + mth_whichotherprof_control___7,
         mth_otherprof_orthopod = mth_whichotherprof_super___8 + mth_whichotherprof_control___8,
         mth_otherprof_other = mth_whichotherprof_super___9 + mth_whichotherprof_control___9,
         mth_otherprof_otherdetail = mth_whichotherprof_other,
         fn_otherprof_sessions = case_when(
           is.na(fn_otherprofsessions_super) & is.na(fn_otherprofsessions_control) ~ NA_character_,
           is.na(fn_otherprofsessions_super) ~ fn_otherprofsessions_control,
           is.na(fn_otherprofsessions_control) ~ fn_otherprofsessions_super
         ),
         mth_otherprof_sessions = case_when(
           is.na(mth_otherprofsessions_super) & is.na(mth_otherprofsessions_control) ~ NA_character_,
           is.na(mth_otherprofsessions_super) ~ mth_otherprofsessions_control,
           is.na(mth_otherprofsessions_control) ~ mth_otherprofsessions_super
         )
         ) %>%
  select(!c(matches("whichotherprof|otherprofsessions"))) %>% # remove old variables
  select(id, timepoint, completed, starts_with("fn_"), starts_with("mth_"))
  
# Blind dataset - remove variables which give away allocation
superkjoin <- superkjoin %>%
  select(!any_of(c("firstphysiosession_date", "expect_tegner_4m", "expect_pain_4m", "expect_qol_4m", "adherence_selfrated", "treatment_satisfaction",
                 contains("blgoals"))))

# Save database
write_csv(superkjoin, "data/processed/SUPERK Database.csv")
write_csv(fnmonthly, "data/processed/SUPERK Fortnightly.csv")
  

  
