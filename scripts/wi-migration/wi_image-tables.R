# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
#
source(here("scripts/functions/fxn_utilities.R"))
source(here("scripts/functions/fxn_tracking.R"))
source(here("scripts/functions/fxn_folders.R"))
source(here("scripts/functions/fxn_images.R"))
source(here("scripts/functions/fxn_image-tables.R"))
# 
# ---------------------------------------------------------- -----
# Define site  ----
index_site = "PWD"
index_year = "2024"
fxn_define_camera_project(index_site)
path_out_wi_migration <- here("output/wi-migration")
# project_id = "Pepperwood"
# Define functions ----
#   read_xlsx_images_sheet ----
# Function to read the image sheet of each xlsx file 
read_xlsx_images_sheet <- function(file) {
  
  data <- 
  read_xlsx(file, 
            sheet = "Images",
            col_types = "text") %>%
    mutate(image_id = as.character(image_id),
           photo_type = as.character(photo_type),
           binomial_1 = as.character(binomial_1),
           count_1 = as.numeric(count_1),
           comments = as.character(comments),
           catalog_by = as.character(catalog_by),
           review = as.character(review),
           good = as.character(good),
           error = as.character(error),
           qc_by = as.character(qc_by),
           qc_certainty = as.character(qc_certainty),
           binomial_2 = as.character(binomial_2),
           count_2 = as.numeric(count_2),
           binomial_3 = as.character(binomial_3),
           count_3 = as.numeric(count_3),
           id = as.character(id),
           image_n = as.numeric(image_n),
           image_file = as.character(image_file))
  
  # For the 'date_time' column
  if (grepl("^\\d+\\.\\d+$", data$date_time[1])) {
    data$date_time <- 
      as.POSIXct(as.numeric(data$date_time) * 86400,
                 origin = "1899-12-30")
  } else {
    data$date_time <- as.POSIXct(data$date_time)
  }
  
  data$date <- as_date(data$date_time)
  # # For the 'date' column
  # if (is_all_numeric(data$date)) {
  #   data$date <- as.Date(as.numeric(data$date),
  #                        origin = "1899-12-30")
  # } else {
  #   data$date <- as.Date(data$date)
  # }
  
  data$time <- as_hms(strptime(data$time, 
                               format = "%H:%M:%S"), 
                      on_fail = NA)
  
  if (exists("data")) {
    cat(file, "\n")
  } else {
    message(paste0("Error with file ", file))
  }
  return(data)
}

#   remove_duplicates ----
# Define a function to remove duplicate strings
# comment = distinct_comments$comments
remove_duplicates <- function(comment) {
  # Split the comment into individual strings
  strings <- str_split(comment, ";\\s*")[[826]]
  
  # Remove duplicate strings while preserving the order
  unique_strings <- unique(strings)
  
  # Join the unique strings back together with "; " separator
  cleaned_comment <- paste(unique_strings, collapse = "; ")
  
  return(cleaned_comment)
}
# ========================================================== -----
# Create dlog_wi -----
dlog_wi <- 
  dlog %>%
  filter(migrate_wi %in% c("TRUE", "MAYBE")) 
# Collate all done_catalog image tables ----
# To confirm values in binomial_2
catalog_files <- 
  tibble(path = list.files(path = path_table_catalog_archive, 
                           pattern = "\\.xlsx$",
                           full.names = TRUE)) %>%
  mutate(file_index = tools::file_path_sans_ext(basename(path)), 
         id = str_sub(file_index, 1, 11)) %>%
  filter(id %in% unique(dlog_wi$id))

# Read and bind all files into one data tibble 
all_catalog <- unique(catalog_files$path) %>%
  map_dfr(read_xlsx_images_sheet)

#   Confirm no duplicate image files  -----
check_dupes_catalog <-
  all_catalog %>%
  get_dupes(image_id)

unique(check_dupes_catalog$id)

# Collate all image tables from vault -----
# Get the list of all files in the vault
vault_files <- 
  tibble(path = list.files(path = path_vault, 
                           pattern = "\\.xlsx$",
                           full.names = TRUE)) %>%
  mutate(file_index = tools::file_path_sans_ext(basename(path)), 
         id = str_sub(file_index, 1, 11)) %>%
  filter(id %in% unique(dlog_wi$id))

# Read and bind all files into one data tibble 
all_vault_init <- unique(vault_files$path) %>%
  map_dfr(read_xlsx_images_sheet)

# Tidy values ----
all_vault <- 
  all_vault_init %>%
  rename(orig_certainty = qc_certainty) %>%
  left_join(lookup_certainty, "orig_certainty") %>%
  mutate(catalog_by = ifelse(catalog_by == "Vf", "Viviane Fuller", catalog_by), 
         qc_by = ifelse(qc_by == "Vf", "Viviane Fuller", qc_by)) %>%
  select(-orig_certainty)
names(all_vault)
# 999,683 x 21

# #   Confirm no duplicate image files  -----
# check_dupes_vault <-
#   all_vault %>%
#   get_dupes(image_id)
# 
# unique(check_dupes_vault$id)
#   Check timestamps ----
# all_vault %>%
#   select(id, 
#          image_id, 
#          photo_type, 
#          starts_with("binomial"), 
#          starts_with("count"), 
#          comments, 
#          catalog_by, 
#          qc_by, 
#          qc_certainty, 
#          review, 
#          good, 
#          error, 
#          date_time,
#          image_n, 
#          image_file)

# ---------------------------------------------------------- -----
# Collate binomial columns ----
binomial_long <- 
  all_vault %>%
  select(image_id, 
         photo_type,
         starts_with("binomial")) %>%
  gather(column_n, binomial, binomial_1:binomial_3) %>%
  drop_na(binomial) %>%
  mutate(column_n = str_remove_all(column_n, "binomial_")) %>%
  unite(image_id_n, 
        c(image_id, column_n), 
        sep = "_",
        remove = FALSE)

#   Confirm all binomial_2 are present -----
has_binomial_2 <- 
  all_vault %>% 
  drop_na(binomial_2) %>%
  select(image_id)

binomial_long %>%
  filter(column_n  == 2) %>%
  distinct(image_id)

#   Confirm all binomial_3 are present -----
has_binomial_3 <- 
  all_vault %>% 
  drop_na(binomial_3) %>%
  select(image_id)

binomial_long %>%
  filter(column_n  == 3) %>%
  distinct(image_id)

#   Count rows ---- 
nrow(all_vault) + nrow(has_binomial_2) + nrow(has_binomial_3)
# ---------------------------------------------------------- -----
# Collate count columns ----
count_long <- 
  all_vault %>%
  select(image_id,  
         starts_with("count")) %>%
  gather(column_n, count, count_1:count_3)  %>%
  mutate(column_n = str_remove_all(column_n, "count_")) %>%
  unite(image_id_n, 
        c(image_id, column_n), 
        sep = "_",
        remove = TRUE)

# START WORKING HERE -----
# #   Check count values  ----
count_long %>%
  distinct(count) %>%
  arrange(count) %>%
  pull()

large_counts <- 
  count_long %>%
  filter(count > 40) %>%
  mutate(image_id = str_sub(image_id_n, 1, 17)) %>%
  pull(image_id)

binomial_long %>%
  filter(image_id %in% large_counts)

all_vault %>%
  filter(image_id %in% large_counts)

all_vault  %>%
  filter(count_2 > 0) %>%
  filter(is.na(binomial_2)) %>%
  distinct(id)

all_catalog %>%
  filter(image_id %in% large_counts) %>%
  filter(id %in% "P_D4_200722")

all_catalog  %>%
  filter(count_2 > 0) %>%
  filter(is.na(binomial_2)) %>%
  distinct(id)

# 
# all_vault %>%
#   distinct(count_1) %>%
#   pull()
# all_vault %>%
#   distinct(count_2) %>%
#   pull()
# all_vault %>%
#   distinct(count_3) %>%
#   pull()

#   Combine binomial_long and count_long ----
binomial_count_long <- 
  binomial_long %>%
  left_join(count_long, "image_id_n") %>%
  select(image_id, 
         photo_type, 
         binomial, 
         count, 
         column_n)

#   Check for rows without count ----
binomial_count_long %>%
  filter(is.na(count)) %>%
  View()

#   Confirm all image_id are present ----
all_vault %>% 
  distinct(image_id)  %>%
  left_join(binomial_count_long, "image_id") %>%
  filter(is.na(photo_type))

#   Confirm values by species -----
binomial_count_long %>%
  filter(image_id == "P_D4_200722_01291")
distinct(count) %>%
  arrange(count) %>%
  pull(count)
distinct(binomial, count) %>%
  arrange(binomial, count)

# ---------------------------------------------------------- -----
# Simplify comments column for image tables in vault ----
# Confirm needs review have done_qc ----
error_messages <- 
  lookup_photo_type_binomial %>%
  drop_na(error_message) %>%
  distinct(error_message) %>%
  arrange(error_message) 

list_error_messages_qc <- 
  error_messages %>%
  filter(str_detect(error_message, "CONFIRM|FIX|NEEDS")) %>%
  pull()

# Combine multiple patterns into a single regular expression
patterns_error_qc <- str_c(list_error_messages_qc, collapse = "|")

comments_error_qc <- 
  all_vault %>%
  drop_na(comments) %>%
  select(comments, 
         review, 
         starts_with("qc")) %>%
  distinct() %>%
  filter(str_detect(comments, patterns_error_qc)) 

comments_error_qc %>%
  distinct(review)
comments_error_qc %>%
  distinct(qc_by)
comments_error_qc %>%
  distinct(qc_certainty)

# Distinct comments ----


distinct_comments <- 
  all_vault %>%
  drop_na(comments) %>%
  distinct(comments) %>%
  mutate(length = nchar(comments), 
         n_sc = str_count(comments, ";")) 

# ---------------------------------------------------------- -----
# Create taxonomic data ----
# Distinct species 

# Crosswalk with wi_taxon_id 
# ---------------------------------------------------------- -----
# Create highlighted ----
# convert good = TRUE to 1, good = FALSE to 0
# Create identified_by ----
# create using qc_by, append catalog_by in NA
all_xlsx %>%
  distinct(catalog_by, qc_by)
# Replace Needs ID with unidentifiable ----
# ---------------------------------------------------------- -----
# ---------------------------------------------------------- -----
# Create _clean image tables for legacy data ----
#
# ---------------------------------------------------------- -----
# Create Image metadata ----
# Convert from image table
# Use our image tables (_clean in vault) as input
# Fields from _clean table 
list_columns_from_clean <- c("id",
                             "image_id",
                             "qc_by",
                             "catalog_by",
                             "binomial_1",
                             "binomial_2",
                             "binomial_3",
                             "count_1",
                             "count_2",
                             "count_3",
                             "qc_certainty",
                             "date_time",
                             "good",
                             "comments")


#
# project_id	:	confirm
# deployment_id	:	Create from id
# image_id	:	Create from image_id
# location	:	create file path with image
# identified_by	:	create using qc_by, append catalog_by in NA
# wi_taxon_id	:	create duplicate entries for images with 2+ species
# class	:	Create from species attributes table
# order	:	Create from species attributes table
# family	:	Create from species attributes table
# genus	:	Create from species attributes table
# species	:	Create from species attributes table
# common_name	:	Create from species attributes table
# uncertainty	:	Create from qc_certainty
# timestamp	:	Create from date_time
# number_of_objects	:	create duplicate entries for images with 2+ species
# highlighted	:	convert good = TRUE to 1, good = FALSE to 0
# age	:	Unknown
# sex	:	Unknown
# animal_recognizable	:	No
# individual_id	:	None
# individual_animal_notes	:	Create from comments
# markings	:	None
# external_sequence_id	:	NULL
# sequence_start_time	:	NULL
#
# ========================================================== -----
