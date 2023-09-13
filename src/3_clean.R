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
         gender = recode(gender, '0' = "Man", "1" = "Woman", '2' = "Other", '3' = "Prefer not to say"),
         bl_birthcountry = case_when(
           bl_birthcountry == 1 ~ "Australia",
           bl_birthcountry == 2 ~ bl_birthcountrytext
         ),
         bl_dominantleg = recode(bl_dominantleg, '1' = "Right", '2' = "Left"),
         bl_aclrside = recode(bl_aclrside, '1' = "Left", '2' = "Right"),
         bl_aclrside_demographic = recode(bl_aclrside_demographic, '1' = "Left", '2' = "Right"),
         dominanthand = recode(dominanthand, '0' = "Left", '1' = "Right")
         ) %>% # recode group variable
  group_by(id) %>%
  fill(dob, .direction = "down") %>%
  fill(gender, .direction = "updown") %>%
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

# Get surgical details informaiton
sxdetails <- superkedit %>%
  select(id, starts_with("sx_")) %>%
  filter(!is.na(sx_name)) %>% # remove blank rows
  mutate(sx_primary = recode(sx_primary, "1" = "Primary", "2" = "Revision", "3" = "Unknown"), # recoding information
         sx_side = recode(sx_side, "1" = "Left", "2" = "Right"),
         sx_graft = recode(sx_graft, "1" = "Hamstring",
                           "2" = "Quadriceps", 
                           "3" = "Patellar Tendon (BPTB)", 
                           "4" = "Allograft", 
                           "5" = "Other", 
                           "6" = "Unknown"),
         sx_graft = case_when(sx_graft == "Other" ~ sx_graft_other,
                              TRUE ~ sx_graft),
         sx_graftside = recode(sx_graftside, "1" = "Ipsilateral",
                               "2" = "Contralateral",
                               "3" = "Allograft (n/a)",
                               "4" = "Unknown")) %>%
  mutate(across(c(sx_meniscus_med, sx_meniscus_lat), ~recode(.x, 
                                                             "1" = "None reported", 
                                                             "2" = "Meniscectomy",
                                                             "3" = "Repair",
                                                             "4" = "Tear untreated",
                                                             "5" = "Other"))) %>%
  mutate(sx_meniscus_med = case_when(sx_meniscus_med == "Other" ~ sx_meniscus_med_other, TRUE ~ sx_meniscus_med),
         sx_meniscus_lat = case_when(sx_meniscus_lat == "Other" ~ sx_meniscus_lat_other, TRUE ~ sx_meniscus_lat),
         sx_cartilage_int = recode(sx_cartilage_int, "0" = "None reported", "1" = "Autologous Cartilage Implantation", 
                                   "2" = "Micro-fracture surgery", "3" = "Cartilage debridgement", "4" = "Other"),
         sx_cartilage_int = case_when(sx_cartilage_int == "Other" ~ sx_cartilage_int_other, TRUE ~ sx_cartilage_int)) %>%
  mutate(across(c(sx_patellacartilage:sx_femoralcartilage_lat), ~recode(.x, "1" = "Not reported", 
                                                                    "2" = "Grade 0", 
                                                                    "3" = "Grade 1", 
                                                                    "4" = "Grade 2",
                                                                    "5" = "Grade 3"))) %>%
  select(-c(sx_graft_other, sx_meniscus_med_other, sx_meniscus_lat_other, sx_cartilage_int_other)) # remove "other" variables not needed now

superkedit <- superkedit %>%
  select(!c(starts_with("fn_"), starts_with("mth_"), starts_with("sx_")))

# List of redcap arms for each timepoint
prebaseline <- c("online_consent_and_arm_1", "participant_regist_arm_1")
baseline <- c("prebaseline_questi_arm_1", "baseline_questionn_arm_1", "baseline_testing_arm_1",
              "randomisation_arm_1", "baseline_postrando_arm_1")
fourmonth <- c("pre4_month_questio_arm_1", "4_month_questionna_arm_1", "4_month_testing_arm_1")
eightmonth <- c("8_month_questionna_arm_1")
twelvemonth <- c("pre12_month_questi_arm_1", "12_month_questionn_arm_1", "12_month_testing_arm_1")
fourmonth_extra <- c("covid_interum_4_mo_arm_1")
twelvemonth_extra <- c("12_month_key_proms_arm_1")

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
  select(id, completed_base, dob, bl_birthcountry:bl_pass) %>%
  rename(completed_proms_mpre = completed_base,
         dob_pre = dob)

mpre <- mpre %>% # remove from timepoint mpre
  select(!bl_birthcountry:bl_pass)

m0 <- superkedit %>%
  filter(redcap_event_name %in% baseline) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  select(!bl_birthcountry:bl_pass) %>% # remove blank columns to be replaced from mpre move
  full_join(., mpreto0, by = "id") %>% # join baseline questionnaire to m0 (full join as some mpre)
  mutate(timepoint = "m0",
         baseline_date = date(completed_proms),
         dob = case_when(
           is.na(dob) ~ dob_pre,
           TRUE ~ dob
         ),
         completed_proms = case_when(
           is.na(completed_proms) ~ completed_proms_mpre,
           TRUE ~ completed_proms),
         age = case_when(
           !is.na(baseline_date) ~ trunc((dob %--% baseline_date) / years(1)),
           TRUE ~ trunc((dob %--% completed_proms) / years(1))
         )) %>%
  filter(!is.na(completed_proms), # remove non respondents
         completed_proms != "[not completed]") %>% # remove not fully completed responses
  select(-c(completed_proms_mpre,dob_pre))

m4 <- superkedit %>%
  filter(redcap_event_name %in% fourmonth) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "m4")

# those that completed 4 month proms extra/separate from main section
m4extra <- superkedit %>%
  filter(redcap_event_name %in% fourmonth_extra) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "m4") %>%
  select(-starts_with("koos")) %>% # remove these variables (empty anyway)
  #rename_at(vars(starts_with("pfoos")), ~str_replace(., "pfoos", "koos")) %>%
  discard(~all(is.na(.)))

# Adding in those that completed their 4 month proms separately or early
m4extra_join <- bind_rows(m4 %>% select(variable.names(m4extra)), m4extra) %>% # only take the variables that are duplicated
  arrange(id, completed_proms) %>%
  group_by(id) %>%
  slice(1) %>% # take the earliest entry (i.e. will be closest to timepoint.)
  ungroup()

# join everything back together
m4join <- left_join(m4 %>% select(-variable.names(m4extra)[-1]), # remove all the duplicated data (but keep id column ([-1]))
                    m4extra_join,
                    by = "id")

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


m12extra <- superkedit %>%
  filter(redcap_event_name %in% twelvemonth_extra) %>%
  group_by(id) %>%
  summarise_all(coalesce_by_column) %>%
  ungroup() %>%
  mutate(timepoint = "m12") %>%
  select(-starts_with("koos")) %>%
  #rename_at(vars(starts_with("pfoos")), ~str_replace(., "pfoos", "koos")) %>%
  discard(~all(is.na(.)))

# Adding in those that compelted their 4 month proms separately or early
m12extra_join <- bind_rows(m12 %>% select(variable.names(m12extra)), m12extra) %>% # only take the variables that are duplicated
  arrange(id, completed_proms) %>%
  group_by(id) %>%
  slice(1) %>% # take the earliest entry (i.e. will be closest to timepoint.)
  ungroup()

m12join <- left_join(m12 %>% select(-variable.names(m12extra)[-1]), # remove all the duplicated data (but keep id column ([-1]))
                    m12extra_join,
                    by = "id")

# Join together timepoints
superkjoin <- bind_rows(mpre, m0) %>%
  bind_rows(., m4join) %>%
  bind_rows(., m8) %>%
  bind_rows(., m12join) %>%
  mutate(timepoint = factor(timepoint, levels = c("mpre" = "mpre", "m0" = "m0", "m4" = "m4", "m8" = "m8", "m12" = "m12")), # order timepoint
         completed = case_when( # compute date of completion of data
           !is.na(completed_base) ~ completed_base,
           !is.na(completed_proms) ~ completed_proms,
           is.na(completed_proms) & !is.na(completed_eq5d) ~ completed_eq5d
         )) %>%
  group_by(id) %>% # fill id variables for all timepoints
  fill(c(sex, age, bl_dominantleg, bl_aclrside, bl_aclrside_demographic, group, baseline_date, firstphysiosession_date, dob, dominanthand), .direction = "downup") %>%
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
         aclqol_total = mean(c_across(matches("aclqol\\_\\d+")), na.omit = TRUE),
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
  select(id, sex, gender, age, dominantleg, dominanthand, aclrside, timepoint, timepoint_actual, baseline_date, firstphysiosession_date, completed, mridate, group, dob, postcode, starts_with("bl_"),
         starts_with("koos_"), starts_with("aclqol"), starts_with("tsk"), starts_with("nprs"), starts_with("pass"), starts_with("groc"), starts_with("hlq"), 
         starts_with("wlq"), starts_with("eq5d"), everything()) %>% # order variables
  select(id, sex, gender, age, dominantleg, dominanthand, aclrside, timepoint, timepoint_actual, baseline_date, firstphysiosession_date, completed, mridate, group, dob, postcode, 
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
  filter(!(is.na(completed) & is.na(koos_s1))) %>% # remove rows of missing data
  select(!any_of(c("firstphysiosession_date", "expect_tegner_4m", "expect_pain_4m", "expect_qol_4m", "adherence_selfrated", "treatment_satisfaction",
                 contains("blgoals"))))

# Save database
write_csv(superkjoin, "data/processed/SUPERK Database.csv")
write_csv(fnmonthly, "data/processed/SUPERK Fortnightly.csv")

# Export
# writing multiple sheets together

library(openxlsx)
openxlsx_setOp("dateFormat", value = "yyyy-mm-dd") # set date format for openxlsx to write in iso format
sheets <- list("SUPERK Database" = superkjoin, "SUPERK Fortnightly" = fnmonthly, "Surgical Details" = sxdetails) # list of different excel sheets
write.xlsx(sheets, "data/processed/SUPERK Database.xlsx", keepNA = TRUE, na.string = "NA") # write to xlsx file with 3 sheets.


  
