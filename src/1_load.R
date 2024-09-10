# load

library(tidyverse)
library(lubridate)
library(ggmap)

# Requires google API key for collection of postcode data from free text address field
# Register google API key with register_google(key = "", write = TRUE)

superk <- read.csv("data/raw/export_20240515.csv", header = TRUE, na.strings = "")
superk <- read.csv("data/raw/export_20240819.csv", header = TRUE, na.strings = "")
varnames <- readxl::read_xlsx("data/raw/superkneedata notes.xlsx", sheet = 1, na = "")
aclr_details <- read_csv("data/raw/aclr_details.csv")
medhx <- read_csv("data/raw/medhist_clean.csv")


