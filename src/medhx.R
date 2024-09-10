

medhx <- read_csv("data/raw/medhist_clean.csv")

medhx <- medhx %>%
  select(id, bl_medhist_done, bl_medication_done) %>%
  mutate(across(everything(), ~str_replace_all(.x, ",", ";"))) %>%
  mutate(across(-id, ~str_replace_all(.x, "\\d", ""))) %>%
  separate(bl_medhist_done, into = c("med1", "med2", "med3", "med4", "med5", "med6", "med7", "med8"), sep = ";") %>%
  separate(bl_medication_done, into = c("medication1", "medication2", "medication3", "medication4", "medication5", "medication6", "medication7", "medication8"), sep = ";")

rawmedhx <- medhx %>%
  select(-c(starts_with("medication"))) %>%
  pivot_longer(-id, names_to = "type", values_to = "med", values_drop_na = TRUE) %>%
  mutate(med = str_trim(med)) %>%
  summarise(fct_count(med, sort = TRUE))

write_csv(rawmedhx, "pmhx_freq_full.csv")

msk <-medhx %>%
  select(-c(starts_with("medication"))) %>%
  pivot_longer(-id, names_to = "type", values_to = "med", values_drop_na = TRUE) %>%
  filter(str_detect(med, "ankle|foot|knee|hip|back|collarbone|shoulder|wrist|arm|shoulder|lisfranc|ischium|femur|hamstring|syndesmosis|tibia|achilles|groin|quadriceps|humerus")) %>%
  mutate(med = str_replace(med, "index|contralateral|contra", "")) %>%
  mutate(med = str_trim(med)) %>%
  group_by(id) %>%
  distinct(med, .keep_all = TRUE)

write_csv(mskcount %>% unnest(), "msk_freq.csv")


medication <- medhx %>%
  select(-c(matches("med\\d"))) %>%
  pivot_longer(-id, names_to = "type", values_to = "medication", values_drop_na = TRUE) %>%
  mutate(medication = str_trim(medication)) %>%
  summarise(fct_count(medication, sort = TRUE))

write_csv(medication, "medication_freq.csv")


otherkneesurg <- super_knee_med_history_data_coding %>%
  select(id, bl_otherkneeinjsurg) %>%
  mutate(bl_otherkneeinjsurg = str_replace_all(bl_otherkneeinjsurg, "\\d", "")) %>%
  mutate(bl_otherkneeinjsurg = str_replace_all(bl_otherkneeinjsurg, "\\([^\\)]+\\)", "")) %>%
  mutate(across(everything(), ~str_replace_all(.x, ",", ";"))) %>%
  mutate(across(everything(), ~str_replace_all(.x, ";$", ""))) %>%
  separate(bl_otherkneeinjsurg, into = c("surg1", "surg2", "surg3", "surg4", "surg5", "surg6", "surg7", "surg8"), sep = ";") %>%
  pivot_longer(-id, names_to = "type", values_to = "surg", values_drop_na = TRUE) %>%
  mutate(surg = str_trim(surg))
  
# no of aclrs
aclrno <- otherkneesurg %>%
  filter(str_detect(surg, "aclr")) %>%
  group_by(id) %>%
  summarise(fct_count(surg)) %>%
  pivot_wider(id_cols = "id", names_from = f, values_from = n)

# no of extra surgeries
extrasurg <- otherkneesurg %>%
  filter(str_detect(surg, "aclr|bone graft|arthroscope|mua|wash out")) %>%
  group_by(id) %>%
  summarise(n_surgery = length(surg),
            n_indexsurgery = sum(str_detect(surg, "index")),
            n_contrasurgery = sum(str_detect(surg, "contra")))

data <- medhx %>%
  select(id) %>%
  left_join(., extrasurg, by = "id") %>%
  left_join(., aclrno, by = "id") 

write_csv(data, "surg_hx.csv")


# no of patients reporting each type
otherkneesurg %>%
  group_by(id) %>%
  distinct(surg, .keep_all = TRUE) %>%
  ungroup() %>%
  summarise(fct_count(surg, sort = TRUE)) %>%
  write_csv("freq_kneesurg.csv")


