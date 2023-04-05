library(ASGS)
library(ASGS.foyer)

library(ggmap)

# ASGS - Australian Statistical Geographic Standard
# foyer package acts as a wrapper around the ASGS API

# Example here:
ASGS::latlon2SA(lon = 145.014789, lat = -37.712588, to = "SA1")

# geocode from ggmap to get address data?
superk_postcodes <- read_csv("data/processed/superk_postcodes.csv")

# use ggmap::geocode to get lat and lon of address
# then pass to ASGS::lat2lonSA() to get SA1 location.

superk_address <- readxl::read_xlsx("data/raw/superk_address_tompaper.xlsx")

head <- head(superk_address)

superk_address <- superk_address %>%
  mutate_geocode(consent_address_res)

sa1 <- superk_address %>%
  mutate(sa1 = map2(lat, lon, ~ASGS::latlon2SA(lon = .y, lat = .x, to = "SA1"))) %>%
  unnest(cols = sa1)

sa1 <- new %>%
  mutate(id = paste0("SUPERKNEE_", record_id)) %>%
  select(id, sa1) %>%
  mutate(sa1 = as.numeric(as.character(sa1)))

national_sa1 <- readxl::read_xlsx("data/raw/sa1_abs_codes.xlsx", sheet = 1)
state_sa1 <- readxl::read_xlsx("data/raw/sa1_abs_codes.xlsx", sheet = 2)

tomdata <- left_join(sa1, national_sa1, by = "sa1") %>%
  left_join(., state_sa1, by = "sa1")

