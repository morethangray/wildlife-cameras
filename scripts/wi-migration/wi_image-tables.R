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
# Define site and attributes ----
index_site = "PWD"

fxn_define_camera_project(index_site)

path_out_wi_migration <- here("output/wi-migration")

dlog_wi <- 
  dlog %>%
  filter(migrate_wi %in% c("TRUE", "MAYBE")) 


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

# ========================================================== -----
# Create all_vault ----
# #   Collate all image tables from vault
# # Get the list of all files in the vault
# vault_files <-
#   tibble(path = list.files(path = path_vault, pattern = "\\.xlsx$", full.names = TRUE)) %>%
#   mutate(file_index = tools::file_path_sans_ext(basename(path)),
#          id = str_sub(file_index, 1, 11)) %>%
#   filter(id %in% unique(dlog_wi$id))
# 
# # Read and bind all files into one data tibble
# all_vault_init <- unique(vault_files$path) %>%
#   map_dfr(read_xlsx_images_sheet)
# 
# #   Tidy values
all_vault <-
  all_vault_init %>%
  rename(orig_certainty = qc_certainty) %>%
  left_join(lookup_certainty, "orig_certainty") %>%
  mutate(catalog_by = ifelse(catalog_by == "Vf", "Viviane Fuller", catalog_by),
         qc_by = ifelse(qc_by == "Vf", "Viviane Fuller", qc_by),
         photo_type = ifelse(photo_type == "Needs ID", "Unidentifiable", photo_type),
         binomial_1 = ifelse(binomial_1 == "Needs ID", "Unidentifiable", binomial_1),
         binomial_2 = ifelse(binomial_2 == "Needs ID", "Unidentifiable", binomial_2),
         binomial_3 = ifelse(binomial_3 == "Needs ID", "Unidentifiable", binomial_3)) %>%
  mutate(

    photo_type =
      case_when(
        photo_type == "Unidentifiable" & binomial_1 %in% c("Rodent species",
                                                           "Sciurus griseus",
                                                           "Urocyon cinereoargenteus",
                                                           "Mephitis mephitis") ~ "Animal",
        photo_type == "Animal" & binomial_1 == "Unidentifiable" ~ "Unidentifiable",
        TRUE ~ photo_type),

    binomial_1 =
      case_when(
        photo_type == "Unidentifiable" & binomial_1 == "Unidentifiable" ~ "N/A",
        photo_type == "Animal" & binomial_1 == "Unidentifiable" ~ "N/A",
        TRUE ~ binomial_1),

    comments =
      case_when(
        binomial_1 == "Bird species" & comments == "Unidentified bird" ~ "",
        TRUE ~ comments)
  ) %>%
  select(-orig_certainty)
# 
# 
# #   Confirm no duplicate image files
# check_dupes_vault <-
#   all_vault %>%
#   get_dupes(image_id)
# 
# unique(check_dupes_vault$id)
# #  Write/read csv ----
# all_vault %>%
#   write_csv(here(path_out_wi_migration, "all-vault_wi.csv"))

all_vault <- read_csv(here(path_out_wi_migration, "all-vault_wi.csv"),
                      col_types = cols(
                        .default = col_character(),
                        date = col_date(format = ""),
                        time = col_time(format = ""),
                        count_1 = col_double(),
                        review = col_logical(),
                        good = col_logical(),
                        error = col_logical(),
                        binomial_2 = col_character(),
                        count_2 = col_double(),
                        binomial_3 = col_character(),
                        count_3 = col_double(),
                        image_n = col_double(),
                        date_time = col_datetime(format = "")
                      ))
#
# ---------------------------------------------------------- -----
# Create all_catalog  ----
# from done_catalog image tables  
# To confirm values in binomial_2
# catalog_files <- 
#   tibble(path = list.files(path = path_table_catalog_archive, 
#                            pattern = "\\.xlsx$",
#                            full.names = TRUE)) %>%
#   mutate(file_index = tools::file_path_sans_ext(basename(path)), 
#          id = str_sub(file_index, 1, 11)) %>%
#   filter(id %in% unique(dlog_wi$id))
# 
# # Read and bind all files into one data tibble 
# all_catalog_init <- unique(catalog_files$path) %>%
#   map_dfr(read_xlsx_images_sheet)
# 
# all_catalog <- 
#   all_catalog_init %>%
#   rename(orig_certainty = qc_certainty) %>%
#   left_join(lookup_certainty, "orig_certainty") %>%
#   mutate(catalog_by = ifelse(catalog_by == "Vf", "Viviane Fuller", catalog_by), 
#          qc_by = ifelse(qc_by == "Vf", "Viviane Fuller", qc_by), 
#          image_id_x = paste(id, str_pad(image_n, width = 5, side = "left", pad = "0"), sep = "_")) %>%
#   distinct()
# 
# #   Confirm no duplicate image files 
# check_dupes_catalog <-
#   all_catalog %>%
#   get_dupes(image_id)
# 
# unique(check_dupes_catalog$id)
# 
#   Write/read csv ----
# all_catalog %>%
#   write_csv(here(path_out_wi_migration, "all_catalog_wi.csv"))

all_catalog <- read_csv(here(path_out_wi_migration, "all_catalog_wi.csv"),
                      col_types = cols(
                        .default = col_character(),
                        date = col_date(format = ""),
                        time = col_time(format = ""),
                        count_1 = col_double(),
                        review = col_character(),
                        good = col_character(),
                        error = col_character(),
                        binomial_2 = col_character(),
                        count_2 = col_double(),
                        binomial_3 = col_character(),
                        count_3 = col_double(),
                        image_n = col_double(),
                        date_time = col_datetime(format = "")
                      )) 

# Error flags aren't cleaned, some good, review, error are x, X, 1
# unique(all_catalog_x$error)

# ========================================================== -----
# BINOMIAL AND COUNT VALUES -----
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
#
# #   Check attributes ----
# #   Confirm nrow binomial_2 match  
# nrow_binomial_2 <- 
#   all_vault %>% 
#   drop_na(binomial_2) %>%
#   select(image_id) %>%
#   nrow()
# 
# binomial_long %>%
#   filter(column_n  == 2) %>%
#   distinct(image_id) %>%
#   nrow() == nrow_binomial_2
# 
# #   Confirm nrow binomial_3 match  
# nrow_binomial_3 <- 
#   all_vault %>% 
#   drop_na(binomial_3) %>%
#   select(image_id) %>%
#   nrow()
# 
# binomial_long %>%
#   filter(column_n  == 3) %>%
#   distinct(image_id) %>%
#   nrow() == nrow_binomial_3
# 
# #   Count rows  
# nrow(all_vault) + nrow_binomial_2 + nrow_binomial_3
# nrow(binomial_long)
# # 1001047
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
#
# #   Check attributes ----
# count_long %>%
#   distinct(count) %>%
#   arrange(count) %>%
#   pull()
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

# ---------------------------------------------------------- -----
# Create binomial_count_long ----
# Combine binomial_long and count_long  
binomial_count_long <- 
  binomial_long %>%
  left_join(count_long, "image_id_n") %>%
  mutate(
    id = str_sub(image_id, 1, 11), 
    photo_type =
      case_when(
        photo_type == "Unidentifiable" & binomial != "N/A" ~ "Animal",
        # Revise photo_type for Unidentifiable binomial_2 
        photo_type == "Animal" & binomial == "Unidentifiable" ~ "Unidentifiable",
        TRUE ~ photo_type), 
    
    # Revise binomial for Unidentifiable binomial_2
    binomial =
      case_when(
        photo_type == "Unidentifiable" ~ "N/A",
        TRUE ~ binomial),
    
    # Create action column for camera maintenance activities
    action = 
      case_when(
        photo_type %in% list_photo_type_maintenance ~ photo_type, 
        TRUE ~ NA_character_), 
    
    photo_type =
      case_when(
        !is.na(action) & binomial != "N/A" ~ "Animal",
        !is.na(action) ~ "Maintenance",
        TRUE ~ photo_type),
    
    # Set count NA if there are no animals or identified objects in image
    count = ifelse(binomial == "N/A", NA, count)) %>%
  
  # Change 0 count for Animal except cow
  mutate(count =
           case_when(
             count == 0 & photo_type == "Animal" & binomial %nin% c("Bos taurus") ~ 1,
             TRUE ~ count)) %>%
  
  arrange(image_id_n) %>%
  select(image_id_n, 
         action, 
         photo_type, 
         binomial, 
         count,
         id, 
         image_id,
         column_n) 


#
# #   Check attributes -----
# #   Check for rows without count 
# binomial_count_long %>%
#   filter(is.na(count)) %>%
#   distinct(binomial)
# 
# #   Confirm all image_id are present
# all_vault %>% 
#   distinct(image_id)  %>%
#   left_join(binomial_count_long, "image_id") %>%
#   filter(is.na(photo_type))
# 
# binomial_count_long %>%
#   distinct(binomial, count) %>%
#   arrange(binomial, count) %>%
#   group_by(binomial, count) %>%
#   count() %>%
#   spread(count, n) %>%
#   View()
#
# binomial_count_long %>%
#  distinct(action, 
#           photo_type, 
#           binomial, 
#           count) %>%
#   arrange(photo_type, binomial, action, count) %>%
#   View()

# Create all_vault_long  ----
all_vault_long <-
  binomial_count_long %>%
  # Add attributes from all_vault 
  left_join(all_vault %>%
              select(image_id, 
                     comments,
                     catalog_by, 
                     starts_with("qc"), 
                     review, 
                     good, 
                     error, 
                     date_time, 
                     image_file), "image_id") 

#   Write/read csv ----
# all_vault_long %>%
#   write_csv(here(path_out_wi_migration, "all_vault_long_wi.csv"))
# 
# 
all_vault_long <- read_csv(here(path_out_wi_migration, "all_vault_long_wi.csv"),
                             col_types = cols(
                               .default = col_character(),
                               count = col_double(),
                               review = col_logical(),
                               good = col_logical(),
                               error = col_logical(),
                               column_n = col_double(),
                               date_time = col_datetime(format = "")
                             ))

# ========================================================== -----
# COMMENTS ----
# Simplify comments column for image tables in vault 
# Define error messages ----
error_messages <- 
  read_excel(here(path_in, 
                  "binomial-crosswalk.xlsx"), 
             sheet = "error-messages") %>%
  select(error_type,
         error_message)   

# more_error_messages <- tibble(error_message = c(
#   "CONFIRM ID: unidentifiable animal",
#   "NEEDS REVIEW: flagged by cataloger for expert review",
#   "NEEDS ID: unknown image flagged for expert review",
#   "ERROR: error flagged by cataloger",
#   "CONFIRM COUNT: blank with count",
#   "ADD COUNT: animal missing; count",
#   "ADD COUNT: animal missing; count",
#   "EXCLUDE IMAGE: error flagged by cataloger"
# ))

# Combine error messages 
# error_messages <- lookup_photo_type_binomial %>%
#   drop_na(error_message) %>%
#   distinct(error_message) %>%
#   bind_rows(more_error_messages) %>%
#   arrange(error_message)


# [x] Confirm needs review have done_qc ----
# Filter error messages for QC  
list_error_messages_qc <- error_messages %>%
  filter(str_detect(error_message, "ADD|CONFIRM|FIX|NEEDS")) %>%
  pull(error_message)

# Combine multiple patterns into a single regular expression
patterns_error_qc <- str_c(list_error_messages_qc, collapse = "|")

comments_error_qc <- 
  all_vault_long %>%
  drop_na(comments) %>%
  select(comments, 
         starts_with("qc")) %>%
  distinct() %>%
  filter(str_detect(comments, patterns_error_qc)) 

all_vault_long %>%
  filter(comments == "CONFIRM ID: unidentifiable animal; Unidentifiable animal" & review == FALSE) %>%
  distinct(id, photo_type, binomial,column_n)
# NOTE: COMMENTS DUPLICATED WHEN IMAGE HAS >1 COLUMN (E.G., BINOMIAL_2)

comments_error_qc %>%
  distinct(qc_by)

comments_error_qc %>%
  filter(is.na(qc_by))

comments_error_qc %>%
  distinct(qc_certainty)

# [x] Remove resolved qc comments ----

fxn_simplify_comments <- function(){
  
}

#   Define a named vector with patterns and their replacements ----
fxn_replace_comment_patterns <- function(comments){
  
  replacements <- c(
    "review\\." = "review;",
    "missing\\." = "missing;",
    "CONFIRM ID: special species\\." = "CONFIRM ID: special species;",
    "CONFIRM ID: unidentifiable animal\\." = "CONFIRM ID: unidentifiable animal;",
    "CONFIRM COUNT: blank with count\\." = "CONFIRM COUNT: blank with count;",
    "ADD COUNT: animal missing; count" = "ADD COUNT: animal missing count",
    "EXCLUDE IMAGE: error flagged by cataloger" = "", 
    "ADD COUNT: animal missing count" = "",
    "ERROR: error flagged by cataloger; Do not catalog" = "Do not catalog",
    "ERROR: error flagged by cataloger; ; Do not catalog:" = "Do not catalog:", 
    "Camera knocked down; Camera knocked down" = "Camera knocked down; ",
    "Camera knocked down; Camera Knocked Down" = "Camera knocked down",
    "Camera knocked down; Camera knocked over" = "Camera knocked down; ", 
    "Camera knocked down; Camera down" = "Camera knocked down; ", 
    "; ," = ";"
  )
  
  str_replace_all(comments, replacements))
    
}
# Since all qc errors have been addressed, remove those comments


#   Define conditional replacement of "Unidentifiable bird" ----
replace_unidentified_bird <- function(binomial, comments) {
  ifelse(binomial == "Bird species", 
         str_replace_all(comments, "Unidentifiable bird", ""),
         comments)
}

#   Define function to remove error messages ----
# index_messages = error_messages 
fxn_remove_error_message <- function(index_data, index_list) {
  
  list_qc_error <- error_messages %>%
    filter(str_detect(error_message, "ADD|CONFIRM|FIX|NEEDS")) %>%
    pull(error_message)
  
  
  # Define a helper function to clean individual comments
  fxn_clean_comment <- function(comment, index_list) {
    # Split the comment into individual strings
    strings <- str_split(comment, ";\\s*")[[1]]
    
    # Remove duplicate strings while preserving the order
    unique_strings <- unique(strings)
    
    # Remove QC error messages
    filtered_strings <- setdiff(unique_strings, index_list = list_qc_error)
    
    # Join the unique strings back together with "; " separator
    cleaned_comment <- paste(filtered_strings, collapse = "; ")
    
    return(cleaned_comment)
  }
  
  #   Define function to clean text ----
  fxn_clean_text <- function(text) {
    text %>%
      str_remove_all("^\\s*;\\s*|\\s*;\\s*$") %>%
      str_remove_all("^\\s*\\.\\s*|\\s*\\.\\s*$") %>%
      str_remove_all("^\\s*,\\s*|\\s*\\,\\s*$") %>%
      str_trim()
  }
  
  # Apply the cleaning functions to each comment ----
  index_data <- index_data %>%
    rename(comments_init = comments) %>%
    mutate(comments = sapply(comments_init, fxn_clean_comment, index_list = list_qc_error)) %>%
    relocate(comments) %>%
    mutate(comments = str_trim(comments, side = "both"), 
           comments = str_replace_all(comments, ";\\s*;", ";"), 
           comments = str_trim(comments, side = "both"), 
           comments = sapply(comments, fxn_clean_text)) %>%
    select(-comments_init)
  
  return(index_data)
}


#   Create tidy comments ----
lookup_tidy_comments <- 
  all_vault_long %>%
  drop_na(comments) %>%
  distinct(binomial, comments) %>%
  mutate(comments_orig = comments) %>%
  mutate(comments = replace_unidentified_bird(binomial, comments_orig)) %>%
  mutate(comments = str_replace_all(comments, replacements)) %>%
  fxn_remove_error_message(list_error_messages_qc) %>%
  select(-binomial) %>%
  distinct()
  
all_vault_long_comments <- 
  all_vault_long %>%
  rename(comments_orig = comments) %>%
  left_join(lookup_tidy_comments, "comments_orig") %>%
  select(-comments_orig) %>%
  mutate(comments = replace_unidentified_bird(binomial, comments))

# #   Write/read csv ----
# all_vault_long_comments %>%
#   write_csv(here(path_out_wi_migration, "all_vault_long_comments_wi.csv"))

all_vault_long_comments <- 
  read_csv(here(path_out_wi_migration, "all_vault_long_comments_wi.csv"),
           col_types = cols(
             .default = col_character(),
             count = col_double(),
             review = col_logical(),
             good = col_logical(),
             error = col_logical(),
             column_n = col_double(),
             date_time = col_datetime(format = "")
           ))
  
# ========================================================== -----
# Create taxonomic data ----
#   Review binomials in data set ----
# Distinct species 
# all_vault_long_tidy_comments %>%
#   distinct(binomial) %>%
#   arrange(binomial) %>%
#   write_csv(here(path_out_wi_migration, 
#                  "distinct-binomials.csv"))
# 
# # Distinct species 
# all_vault_long_tidy_comments %>%
#   filter(photo_type == "Animal", 
#          binomial %in% c("Homo sapiens", 
#                          "Insect species", 
#                          "Reptile species")) %>%
#   drop_na(comments) %>%
#   distinct(binomial, comments) %>%
#   arrange(binomial) %>%
#   write_csv(here(path_out_wi_migration, 
#                  "distinct-binomials_comments.csv"))

#   Crosswalk binomial with wi_taxon_id -----
wi_taxonomy <- 
  read_excel(here(path_in, "attributes_species_wi.xlsx"), 
             sheet = "lookup_taxonomy") %>%
  select(binomial:common_name)

all_vault_long_comments_taxon <- 
  all_vault_long_comments %>%
  left_join(wi_taxonomy, "binomial") 
# ---------------------------------------------------------- -----
# START WORKING HERE -----
# Create identified_by ----
# Need catalog_by 
  # 1 P_A1_200610
  # 2 P_A1_210419
  # 3 P_A2_211018
  # 4 P_A3_170116
  # 5 P_B2_200610
  # 6 P_B3_211018
  # 7 P_C5_170425
  # 8 P_D4_190801
  # 9 P_D5_161201
  # 10 P_E1_200204
  # 11 P_E1_200427
  # 12 P_E1_210609
  # 13 P_E2_200427
  # 14 P_E2_200611
  # 15 P_E4_210831
  # 16 P_E5_200612
# create using qc_by, append catalog_by in NA
all_vault_long_comments_taxon %>%
    filter(is.na(qc_by)) %>%
    
    filter(is.na(catalog_by)) %>%
    distinct(id)
 
# ---------------------------------------------------------- -----
# Create Image metadata ----
list_columns_image <- c("project_id",
                        "deployment_id",
                        "image_id",
                        "location",
                        "identified_by",
                        "wi_taxon_id",
                        "class",
                        "order",
                        "family",
                        "genus",
                        "species",
                        "common_name",
                        "uncertainty",
                        "timestamp",
                        "number_of_objects",
                        "highlighted",
                        "age",
                        "sex",
                        "animal_recognizable",
                        "individual_id",
                        "individual_animal_notes",
                        "markings",
                        "external_sequence_id",
                        "sequence_start_time")
  
all_vault_long_comments_taxon %>%
  mutate(highlighted = ifelse(good == TRUE, 1, 0)) %>%
 
  rename(deployment_id = id, 
         location = image_file, 
         uncertainty = qc_certainty, 
         timestamp = date_time, 
         number_of_objects = count, 
         individual_animal_notes = comments) %>%
  mutate(age = "Unknown", 
         sex = "Unknown", 
         animal_recognizable = "No", 
         individual_id = "None", 
         markings = "None", 
         external_sequence_id = NULL, 
         sequence_start_time = NULL) %>%
  relocate(any_of(list_columns_image))
# Save as deployment_images.csv -----
#
# ========================================================== -----

# GRAVEYARD ----
# binomial_unid <- 
binomial_count_long %>%
  
  
  distinct(photo_type)  %>%
  View()
filter(binomial %in% c("Needs ID", "Unidentifiable")) %>%
  select(image_id, 
         photo_type_vault = photo_type, 
         binomial, 
         count, 
         column_n) %>%
  left_join(all_catalog %>%
              select(image_id, 
                     photo_type, 
                     starts_with("binomial"), 
                     starts_with("count"), 
                     comments), "image_id")

binomial_unid %>%
  select(photo_type_vault, 
         photo_type,
         starts_with("binomial"), 
         starts_with("count"), 
         column_n
  ) %>%
  distinct()

# Revise values with Needs ID -----
#   For binomial_1 ----
binomial_1_unid <- 
  binomial_unid %>%
  filter(column_n == 1) %>%
  select(image_id:photo_type, 
         binomial_1, 
         count_1, 
         comments) %>%
  arrange(photo_type, 
          binomial_1)  %>%
  unite(photo_type_binomial_1, 
        c(photo_type, 
          binomial_1), 
        sep = "_", 
        remove = FALSE)

binomial_1_unid_rev <- 
  binomial_1_unid %>%
  distinct(photo_type_vault, 
           photo_type, 
           binomial_1, 
           count,
           count_1,
           photo_type_binomial_1) %>%
  mutate(photo_type_rev = 
           case_when(
             binomial_1 == "Odocoileus hemionus" ~ "Animal", 
             photo_type == "Blank" ~ "Blank", 
             is.na(binomial_1) ~ "Unidentifiable", 
             TRUE ~ photo_type)) %>%
  mutate(binomial_1_rev = 
           case_when(
             photo_type_rev == "Blank" | photo_type_rev == "Unidentifiable" ~ "N/A", 
             TRUE ~ binomial_1)) %>%
  mutate(count_1_rev = 
           case_when(
             photo_type_rev == "Blank" | photo_type_rev == "Unidentifiable" ~ 0, 
             TRUE ~ count_1)) %>%
  select(photo_type_binomial_1, 
         photo_type_rev, 
         binomial_1_rev, 
         count_1_rev) %>%
  distinct()

binomial_1_unid_fixed <- 
  binomial_1_unid %>%
  left_join(binomial_1_unid_rev, "photo_type_binomial_1") %>%
  select(image_id, 
         photo_type = photo_type_rev, 
         binomial = binomial_1_rev, 
         count = count_1_rev, 
         column_n) %>%
  distinct()

#   For binomial_2 ----
# Can have photo_type = Animal (for binomial_1) and binomial_2 = Unidentifiable
binomial_2_unid <- 
  binomial_unid %>%
  filter(column_n == 2) %>%
  select(image_id:photo_type, 
         binomial_2, 
         count_2, 
         comments) %>%
  arrange(photo_type, 
          binomial_2)  %>%
  unite(photo_type_binomial_2, 
        c(photo_type, 
          binomial_2), 
        sep = "_", 
        remove = FALSE)

binomial_2_unid_rev <- 
  binomial_2_unid %>%
  distinct(photo_type_vault, 
           photo_type, 
           binomial,
           binomial_2, 
           count,
           count_2,
           comments,
           photo_type_binomial_2) %>%
  mutate(photo_type_rev = 
           case_when(
             is.na(binomial_2) & binomial == "Needs ID" ~ "Unidentifiable", 
             TRUE ~ photo_type_vault)) %>%
  mutate(binomial_2_rev = 
           case_when(
             photo_type_rev == "Unidentifiable" ~ "N/A", 
             TRUE ~ binomial)) %>%
  mutate(count_2_rev = 
           case_when(
             photo_type_rev == "Unidentifiable" ~ 0, 
             TRUE ~ count_2)) %>%
  select(photo_type_binomial_2, 
         photo_type_rev, 
         binomial_2_rev, 
         count_2_rev) %>%
  distinct()

binomial_2_unid_fixed <- 
  binomial_2_unid %>%
  left_join(binomial_2_unid_rev, "photo_type_binomial_2") %>%
  select(image_id, 
         photo_type = photo_type_rev, 
         binomial = binomial_2_rev, 
         count = count_2_rev, 
         column_n) %>%
  distinct()

#   binomial_3: No rows -----
# Combine corrected tables for binomial_1 and _2 ----
binomial_unid_rev <- 
  bind_rows(binomial_1_unid_fixed, binomial_2_unid_fixed) %>%
  unite(image_id_n, 
        c(image_id, column_n), 
        sep = "_", 
        remove = FALSE) %>%
  mutate(id = str_sub(image_id, 1, 11)) %>%
  select(id, 
         image_id, 
         image_id_n,
         photo_type, 
         binomial, 
         count, 
         column_n)

# # Check row counts 
# nrow(binomial_count_long)
# binomial_count_long %>%
#   filter(image_id_n %nin% list_image_id_n_drop) %>%
#   nrow() - nrow(binomial_count_long)
# Create binomial_count_long_revised with new values 
binomial_count_long_revised <- 
  binomial_count_long %>%
  filter(image_id_n %nin% unique(binomial_unid_rev$image_id_n)) %>%
  bind_rows(binomial_unid_rev) %>%
  arrange(image_id_n)
