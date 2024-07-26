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

# ========================================================== -----
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
camera_sn_dates <- read_xlsx(here(path_site, 
                                    "pwd_serial-numbers.xlsx"), 
                               sheet = "camera_sn_dates") %>%
  mutate(date_from = as_date(date_from), 
         date_to = as_date(date_to)) %>%
  filter(grid == "P") %>%
  select(camera, 
         sn_from = date_from, 
         sn_to = date_to,
         serial_number)

new_sn <- 
  dlog_wi %>%
  filter(migrate_wi == TRUE) %>%
  select(id, camera, date_from, date_to)  %>%
  left_join(camera_sn_dates, by = "camera") %>%
  filter(date_to >= sn_from & date_to <= sn_to & date_from >= sn_from & date_from <= sn_to) %>%
  select(id, 
         serial_number) %>%
  distinct()

# new_sn %>%
#   write_csv(here(path_out_wi_migration,
#                  "camera_serial-number_revised_4.csv"))


camera_inventory <- read_xlsx(here(path_site, 
                                  "pwd_serial-numbers.xlsx"), 
                             sheet = "camera inventory") %>%
  
  select(serial_number, 
         make, 
         model, 
         year)

# Create Camera metadata ----
#
# project_id	:	confirm
# camera_id	:	define from serial number
# make	:	get from captain's log, deployment summary
# model	:	get from captain's log, deployment summary
# serial_number	:	get from captain's log, deployment summary
# year_purchased	:	confirm
#
list_columns_camera <- c("project_id",
                         "camera_id",
                         "make",
                         "model",
                         "serial_number",
                         "year_purchased")

metadata_camera_all <- 
  dlog_wi %>%
  filter(migrate_wi == TRUE) %>%
  select(id, serial_number) %>%

  left_join(camera_inventory, "serial_number") %>%
  mutate(project_id = "Pepperwood", 
         camera_id = serial_number) %>%
  rename(deployment_id = id,  
         year_purchased = year) %>%
  select(all_of(list_columns_camera), 
         deployment_id)

# Save as metadata_camera_all.csv  -----
metadata_camera_all %>%
    write_csv(here(path_out_wi_migration, "metadata", 
                   "metadata_camera_all.csv"))
  
# ========================================================== -----
