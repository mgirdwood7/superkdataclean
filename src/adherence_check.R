
weekly_data <- weekly_data %>%
  mutate(fn_strengthsessionsfreq_control = case_when(
    fn_strengthsessions_control == 0 ~ 0,
    TRUE ~ fn_strengthsessionsfreq_control
  ))


test <- weekly_data %>%
  group_by(id) %>%
  summarise(responses = sum(length(completed)),
            perc_complete = round(responses/7*100,2),
            sum_physioattendance_super = sum(fn_physioattendance_super),
            sum_strengthattendance_super = sum(fn_strengthsessions_super),
            sum_strengthattendance_control = sum(fn_strengthsessionsfreq_control),
            mean_physioattendance_super = mean(fn_physioattendance_super, na.rm = TRUE) / 2,
            mean_strengthattendance_super = mean(fn_strengthsessions_super, na.rm = TRUE) / 2,
            mean_strengthattendance_control = mean(fn_strengthsessionsfreq_control, na.rm = TRUE) / 2,
            mean_sessionlength = mean(fn_sessionlength, na.rm = TRUE),
            quad_perc = round(sum(length(fn_exercises_quad))/7*100, 2),
            hami_perc = round(sum(length(fn_exercises_hami))/7*100, 2),
            plyo_perc = round(sum(length(fn_exercises_plyo))/7*100, 2),
            balance_perc = round(sum(length(fn_exercises_balance))/7*100, 2),
            core_perc = round(sum(length(fn_exercises_core))/7*100, 2),
            gluthip_perc = round(sum(length(fn_exercises_gluthip))/7*100, 2),
            calf_perc = round(sum(length(fn_exercises_calf))/7*100, 2),
            mean_rpe = mean(fn_rpe, na.rm = TRUE),) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))


superk <- readxl::read_xlsx("data/processed/SUPERK Database v4.1 20230428.xlsx", sheet = 1, na = "NA")


researcher <- superk %>%
  filter(timepoint == "m4") %>%
  rowwise() %>%
  mutate(m4_physio = sum(c_across(c(m4_strength_physio1to1, m4_strength_physiogroup))),
         m4_strength = sum(c_across(c(m4_strength_home, m4_strength_gym)))) %>%
  ungroup() %>%
  group_by(id) %>%
  summarise(m4_phyiso_mean = m4_physio,
            m4_strength_mean = m4_strength,
            m4_mean_sessionlength = m4_sessionlength,
            m4_mean_rpe = m4_rpe) %>%
  mutate(id = paste0("SUPERK_", id))

adherence <- full_join(test, researcher, by = "id")

adherence <- adherence %>% 
  select(id, responses, perc_complete:sum_strengthattendance_control, mean_physioattendance_super, 
         m4_phyiso_mean, mean_strengthattendance_super, mean_strengthattendance_control, 
         m4_strength_mean, mean_sessionlength, m4_mean_sessionlength, mean_rpe, m4_mean_rpe, 
         everything()) %>% 
  rename(m4_physio_mean = m4_phyiso_mean) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

write_csv(adherence, "data/processed/SUPERK Adherence check 20230503.csv")

adherence %>% mutate(strength = case_when(
  !is.na(mean_strengthattendance_super) ~ mean_strengthattendance_super,
  TRUE ~ mean_strengthattendance_control
                     )) %>%
  ggplot(., aes(x = ceiling(strength), y = m4_strength_mean)) + geom_point()