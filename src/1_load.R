# load

library(tidyverse)
library(lubridate)
library(ggmap)

# Requires google API key for collection of postcode data from free text address field
# Register google API key with register_google(key = "", write = TRUE)

superk <- read.csv("data/raw/export_230822.csv", header = T, na.strings = "")
varnames <- readxl::read_xlsx("data/raw/superkneedata notes.xlsx", sheet = 1, na = "")

