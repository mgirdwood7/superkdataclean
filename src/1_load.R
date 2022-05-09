# load

library(tidyverse)
library(lubridate)
library(ggmap)

superk <- read.csv("data/raw/export_9.5.22.csv", header = T, na.strings = "")
varnames <- readxl::read_xlsx("data/raw/superkneedata notes.xlsx", sheet = 1, na = "")

