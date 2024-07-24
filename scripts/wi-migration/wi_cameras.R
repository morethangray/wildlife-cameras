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