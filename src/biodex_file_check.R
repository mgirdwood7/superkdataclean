# Biodex file check

superk <- readxl::read_xlsx("data/processed/SUPERK Database v7.0 20240515.xlsx", sheet = 1, na = "NA")

superkdata <- superk %>% select(id, timepoint, biodex_seatheight) %>%
  filter(!is.na(biodex_seatheight),
         id < 9000) %>%
  select(-biodex_seatheight)

# Biodex folder
path <- "/Volumes/ResearchDrives/PROJECT - SHE - SUPERKNEE/SUPER KNEE/DATA/Biodex data"

# Read all the file paths in
files <- list.files(path = path, pattern = ".txt", full.names = TRUE, recursive = TRUE) # find .csv files in 

# Extract all the information from the file name
data <- data.frame(files) %>%
  mutate(dir = map_chr(files, dirname)) %>%
  filter(!str_detect(dir, "Labview|desktop saves")) %>% # remove other
  mutate(name = str_extract(files, "/([^/.]+)\\.txt"),
         name = str_remove_all(name, "/|\\.txt")) %>% # take only the name from the path
  mutate(id = as.numeric(str_extract(name, "\\d+(?=_)")), # use the digits to extract the id
         side = case_when(
           str_detect(name, "LEFT|left|Left") ~ "left", # extract the side, allowing for a few different variations
           str_detect(name, "RIGHT|right|Right") ~ "right"),
         timepoint = case_when(
           str_detect(name, "T12$") ~ "m12", # extract timepoint
           str_detect(name, "T4$") ~ "m4",
           str_detect(name, "t$|T$") ~ "m0")) %>%
  mutate(correct_name = case_when(
    timepoint == "m0" ~ paste("SUPERKNEE", id, toupper(side), sep = "_"),
    timepoint != "m0" ~ paste("SUPERKNEE", id, toupper(side), toupper(str_replace(timepoint, "m", "t")), sep = "_")
  ))

# Missing data
missing <- anti_join(superkdata, data, by = c("id", "timepoint")) %>%
  mutate(side = "both")


missing_side <- data %>% group_by(id, timepoint) %>%
  filter(n() == 1) %>%
  select(id, side, timepoint) %>%
  mutate(side = case_when(side == "left" ~ "right",
                          side == "right" ~ "left"))

missing <- bind_rows(missing, missing_side)

write_csv(missing, "missing_biodex_files_20240301.csv")

## Checking the raw data
biodexdata <- data %>%
  mutate(sum_data = map(files, possibly(~read.csv(.x, skip = 5, sep = "") %>% # read in the data
           summarise(quad = max(N.M), # take the max value to get quad
                     hs = abs(min(N.M)))), otherwise = NULL) # take the absolute min value to get hamstring
  ) %>%
  unnest(sum_data)


## Checking names
name_check <- data %>%
  mutate(name = map_chr(files, possibly(~readLines(.x, n = 1)))) %>% # read the first line of each file with the name
  mutate(surname = str_split_i(name, ", ", 1),  # split into first and surname
         firstname = str_split_i(name, ", ", 2))

## File renaming



## Function to rename files to correct format.

rename_files <- function(file_list) {
  
  for (i in 1:nrow(file_list)) {
    current_name <- file_list$name[i]
    correct_name <- file_list$correct_name[i]
    
    
    current_path <- file_list$files[i]
    correct_path <- file.path(file_list$dir[i], paste0(correct_name, ".txt"))
  
    
    # Check if the current name is different from the desired name
    if (current_name != correct_name) {
      # Rename the file if it exists
      if (file.exists(current_path)) {
        file.rename(current_path, correct_path)
      }
    }
  }
}

