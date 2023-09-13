library(ASGS)
library(ASGS.foyer)

library(ggmap)

# ASGS - Australian Statistical Geographic Standard
# foyer package acts as a wrapper around the ASGS API

# Test example here:
ASGS::latlon2SA(lon = 145.014789, lat = -37.712588, to = "SA1")

# use ggmap::geocode to get lat and lon of address
# then pass to ASGS::lat2lonSA() to get SA1 location.

superk_address <- read_csv("data/processed/superk_address.csv")

# To test on partial dataframe
# head <- head(superk_address)

# Don't need this - lat/lon in the files
# superk_address <- superk_address %>%
#  mutate_geocode(address_res)

sa1 <- superk_address %>%
  mutate(sa1 = map2(lat, lon, ~ASGS::latlon2SA(lon = .y, lat = .x, to = "SA1"))) %>%
  unnest(cols = sa1)

sa1 <- sa1 %>%
  mutate(id = paste0("SUPERKNEE_", id)) %>%
  select(id, SA1_CODE_2021, SA2_CODE_2021, SA3_CODE_2021, SA4_CODE_2021, GCCSA_CODE_2021) %>%
  rename_with(tolower) %>%
  mutate(sa1_code_2021 = as.numeric(as.character(sa1_code_2021)))

national_sa1 <- readxl::read_xlsx("data/raw/sa1_abs_codes.xlsx", sheet = 1) %>% rename(sa1_code_2021 = sa1)
state_sa1 <- readxl::read_xlsx("data/raw/sa1_abs_codes.xlsx", sheet = 2) %>% rename(sa1_code_2021 = sa1)

tomdata <- left_join(sa1, national_sa1, by = "sa1_code_2021") %>%
  left_join(., state_sa1, by = "sa1_code_2021")

