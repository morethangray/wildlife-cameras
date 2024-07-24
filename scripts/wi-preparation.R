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
# README -----
# Output files are saved to path_out
#   K:\wildlife-cameras\output\<SITE>
# When an output file is created:
#   Existing file renamed with _date, moved to z_archive
#   New file is saved in main folder
# A new _error file created ONLY when errors are found
#   If there aren't errors, no new file is made
# ---------------------------------------------------------- -----
# Define site  ----
# PWD: Pepperwood WPI grid
# MMP: Modini WPI grid
#
index_site = "PWD"
index_year = "2024"
fxn_define_camera_project(index_site)
# project_id = "Pepperwood"
# ========================================================== -----
# CREATE HELPERS FOR WI MIGRATION ----
path_out_wi_migration <- here("output/wi-migration")
# ---------------------------------------------------------- -----
# DEFINE LEGACY ATTRIBUTES ----
# Create dlog for legacy data ----
dlog_legacy <-
  fxn_dlog_get(here("K:/WPI Project/legacy-data_PWD", 
                    "wpi_deployments_all_2013-2016.xlsx")) %>%
  filter(grid == "P", 
         use_data == TRUE) %>%
  tidyr::drop_na(id) %>%
  arrange(id)

# Bind dlog and dlog_legacy ----
dlog_pwd <- 
  dlog %>%
  filter(migrate_wi == TRUE) %>%
  bind_rows(dlog_legacy) %>%
  select(id, 
         camera,
         date_from, 
         date_to, 
         error_type, 
         error_subtype, 
         # comments,
         serial_number)
#
# Collate summary tables from wi sheets ----
# From WI format image tables 
# Define path to survey attributes from WI tables
path_survey_attributes <- here(path_site, "data/z_archive/survey-attributes_wi")

# Get the list of all CSV files in the directory
csv_files <- list.files(path = path_survey_attributes, pattern = "\\.csv$", full.names = TRUE)

#   legacy_attributes ----
# Read and bind all CSV files into one data tibble
legacy_attributes <-
  csv_files %>%
  map_dfr(~ read_csv(.))  %>%
  # Filter because have files from catalog and qc 
  distinct()
# Get camera info from legacy_attributes ----
# Use legacy_attributes, captain's log as input
legacy_attributes_camera <- 
  legacy_attributes %>%
  filter(sheet == "Cameras", 
         attribute != "project_id") %>%
  spread(attribute, value) %>%
  select(id, 
         make, 
         model, 
         serial_number, 
         year_purchased)

legacy_attributes_camera_distinct <- 
  legacy_attributes_camera  %>%
  select(-id) %>%
  distinct() %>%
  arrange(serial_number) 
# #   Write csv ----
# legacy_attributes_camera %>%
#   write_csv(here(path_out_wi_migration, 
#                  "legacy-attributes_camera.csv"))
# 
# legacy_attributes_camera_distinct %>%
#   write_csv(here(path_out_wi_migration, 
#                  "legacy-attributes_camera_distinct.csv"))
# Get deployment info from legacy_attributes ----
legacy_attributes_deployment <- 
  legacy_attributes %>%
  filter(sheet == "Deployment")  %>%
  spread(attribute, value) %>%
  mutate(camera = str_sub(id, 1, 4)) %>%
  select(id, 
         camera,
         serial_number = camera_id, 
         date_from = camera_deployment_begin_date, 
         date_to = camera_deployment_end_date, 
         latitude = latitude_resolution, 
         longitude = longitude_resolution,
         fail_details = camera_failure_details,
         fail_hardware = camera_hardware_failure, 
         deployment_id, 
         event_name) 

# #   Write csv ----
# Camera coordinates 
# legacy_attributes_deployment %>%
#   distinct(camera, 
#            latitude, 
#            longitude)  %>%
#   write_csv(here(path_out_wi_migration, 
#                  "camera_coordinates.csv"))


legacy_attributes_deployment %>%
  select(-latitude, 
         -longitude) %>%
    write_csv(here(path_out_wi_migration,
                   "camera_deployments.csv"))
  
# ========================================================== -----
# CAMERA METADATA ----
# Get camera info from captain's log ----
camera_sn_dates <- read_xlsx(here(path_out_wi_migration, 
                                    "camera_metadata.xlsx"), 
                               sheet = "camera-dates") %>%
  mutate(date_from = as_date(date_from), 
         date_to = as_date(date_to)) %>%
  filter(grid == "P") %>%
  select(camera, date_from, date_to, serial_number)

new_sn <- 
  dlog_pwd %>%
  select(id, camera, date_from)  %>%
  left_join(camera_sn_dates, by = "camera") %>%
  filter(date_from.x >= date_from.y & date_from.x <= date_to) %>%
  select(id, 
         serial_number) %>%
  distinct()

# new_sn %>%
#   write_csv(here(path_out_wi_migration,
#                  "camera_serial-number_revised_2.csv"))

dlog_pwd %>%
  select(id, camera, 
         serial_number_init = serial_number) %>%
  left_join(new_sn, "id") %>%
  distinct() %>%
  filter(serial_number_init != serial_number)
  

# Create Camera metadata ----
#
# project_id	:	confirm
# camera_id	:	define from serial number
# make	:	get from captain's log, deployment summary
# model	:	get from captain's log, deployment summary
# serial_number	:	get from captain's log, deployment summary
# year_purchased	:	confirm
#
# ========================================================== -----
# IMAGE TABLES ----
# Create dlog_wi -----
dlog_wi <- 
  dlog %>%
  filter(migrate_wi %in% c("TRUE", "MAYBE")) 
# read_xlsx_images_sheet ----
# Function to read the image sheet of each xlsx file 
read_xlsx_images_sheet <- function(file) {
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
              
}
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
all_vault <- unique(vault_files$path) %>%
  map_dfr(read_xlsx_images_sheet)

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
distinct_comments <- 
  all_xlsx %>%
  drop_na(comments) %>%
  distinct(comments)

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
# DEPLOYMENT METADATA ----
# Confirm camera attributes for all years (2012-2023) ----
# camera coordinates, quiet period, height, angle
#
# Create Deployment metadata ----

# 
# project_id	:	confirm
# deployment_id	:	Create from id
# subproject_name	:	None
# subproject_design	:	None
# placename	:	Create from camera
# longitude	:	get from spatial data and confirm with SH
# latitude	:	get from spatial data and confirm with SH
# start_date	:	Create from date_from
# end_date	:	Create from date_to
# event_name	:	None
# event_description	:	None
# event_type	:	None
# bait_type	:	None
# bait_description	:	None
# feature_type	:	None
# feature_type_methodology	:	None
# camera_id	:	define from serial number
# quiet_period	:	get from SH for each deployment
# camera_functioning	:	Crosswalk to our error types, also secondary error type
# sensor_height	:	get from SH for each deployment
# height_other	:	None
# sensor_orientation	:	get from SH for each deployment
# orientation_other	:	None
# recorded_by	:	get from SH for each deployment
# plot_treatment	:	None
# plot_treatment_description	:	None
# detection_distance	:	NULL
#
# ========================================================== -----
# PROJECT METADATA ----
# Create Project metadata ----
#
# project_id	:	confirm
# project_name	:	confirm
# project_id	:	confirm
# project_short_name	:	confirm
# project_objectives	:	confirm
# project_species	:	Multiple
# project_species_individual	:	None
# project_sensor_layout	:	Systematic
# project_sensor_layout_targeted_type	:	None
# project_bait_use	:	No
# project_bait_type	:	None
# project_stratification	:	Yes
# project_stratification_type	:	Distance
# project_sensor_method	:	Sensor Detection
# project_individual_animals	:	No
# project_blank_images	:	No
# project_sensor_cluster	:	No
# project_admin	:	confirm
# project_admin_email	:	confirm
# country_code	:	USA
# embargo	:	48
# metadata_license	:	confirm
# image_license	:	confirm
# project_type	:	Image
#
# ========================================================== -----