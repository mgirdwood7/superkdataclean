#sae

library(tidyverse)
library(janitor)

sae <- readxl::read_xlsx("data/superknee_ae.xlsx", sheet = 1, na = "")

data <- readxl::read_xlsx("data/processed/SUPERK Database v8.0 20240819.xlsx", sheet = 1, na = "NA") %>%
  filter(id < 3000,
         timepoint == "m0") %>%
  select(id, group) 

sae <- left_join(data, sae, by = "id") %>%
  mutate(timepoint = factor(timepoint, levels = c("m4", "m12")))

totalnfunc <- function(name){
  name <- enquo(name)
  
  df <- sae %>%
    select(id, group, timepoint, !!name) %>%
    separate_longer_delim(!!name, ";") %>%
    filter(!is.na(!!name)) %>%
    group_by(group, timepoint) %>% distinct(id, .keep_all = TRUE) %>% summarise(n = length(id)) %>%
   pivot_wider(names_from = c(timepoint, group), values_from = n) %>%
    mutate(f = "Total n", .before = 1)
  return(df)
}

adverse_index <- sae %>%
  select(id, group, timepoint, adverse_index) %>%
  separate_longer_delim(adverse_index, ";") %>%
  filter(!is.na(adverse_index)) %>%
  group_by(group, timepoint) %>%
  summarise(fct_count(adverse_index)) %>%
  pivot_wider(id_cols = f, names_from = c(timepoint, group), values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  adorn_totals() %>%
  bind_rows(totalnfunc(adverse_index))


adverse_other <- sae %>%
  select(id, group, timepoint, adverse_other) %>%
  separate_longer_delim(adverse_other, delim = ";") %>%
  mutate(adverse_other = str_trim(adverse_other)) %>%
  filter(!is.na(adverse_other)) %>%
  group_by(group, timepoint) %>%
  #group_by(timepoint) %>%
  summarise(fct_count(adverse_other)) %>%
  pivot_wider(id_cols = f, names_from = c(timepoint, group), values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  adorn_totals() %>%
  bind_rows(totalnfunc(adverse_other))



serious_knee <- sae %>%
  select(id, group, timepoint, serious_knee) %>%
  separate_longer_delim(serious_knee, delim = ";") %>%
  mutate(serious_knee = str_trim(serious_knee)) %>%
  filter(!is.na(serious_knee)) %>%
  group_by(group, timepoint) %>%
  #group_by(timepoint) %>%
  summarise(fct_count(serious_knee)) %>%
  pivot_wider(id_cols = f, names_from = c(timepoint, group), values_from = n) %>%
  mutate(m12_7 = 0) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  adorn_totals() %>%
  bind_rows(totalnfunc(serious_knee))


serious_other <- sae %>%
  select(id, group, timepoint, serious_other) %>%
  separate_longer_delim(serious_other, delim = ";") %>%
  mutate(serious_other = str_trim(serious_other)) %>%
  filter(!is.na(serious_other)) %>%
  group_by(group, timepoint) %>%
  #group_by(timepoint) %>%
  summarise(fct_count(serious_other)) %>%
  pivot_wider(id_cols = f, names_from = c(timepoint, group), values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  adorn_totals() %>%
  bind_rows(totalnfunc(serious_other))


medication <- sae %>%
  select(id, group, timepoint, medication) %>%
  separate_longer_delim(medication, delim = ";") %>%
  mutate(medication = str_trim(medication)) %>%
  filter(!is.na(medication)) %>%
  group_by(group, timepoint) %>%
  #group_by(timepoint) %>%
  summarise(fct_count(medication)) %>%
  pivot_wider(id_cols = f, names_from = c(timepoint, group), values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  adorn_totals() %>%
  bind_rows(totalnfunc(medication))


cointervention <- sae %>%
  select(id, group, timepoint, cointervention) %>%
  separate_longer_delim(cointervention, delim = ";") %>%
  mutate(cointervention = str_trim(cointervention)) %>%
  filter(!is.na(cointervention)) %>%
  group_by(group, timepoint) %>%
  #group_by(timepoint) %>%
  summarise(fct_count(cointervention)) %>%
  pivot_wider(id_cols = f, names_from = c(timepoint, group), values_from = n) %>%
  mutate(across(everything(), ~replace_na(., 0))) %>%
  adorn_totals() %>%
  bind_rows(totalnfunc(cointervention))
