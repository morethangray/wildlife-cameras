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
# ========================================================== -----
# CAMERA METADATA ----
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
#   Write csv ----
legacy_attributes_camera %>%
  write_csv(here(path_out_wi_migration, 
                 "legacy-attributes_camera.csv"))

legacy_attributes_camera_distinct %>%
  write_csv(here(path_out_wi_migration, 
                 "legacy-attributes_camera_distinct.csv"))

#
# Get camera info from captain's log ----
# 
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
# Simplify comments column for image tables in vault ----
# Check column format (count_1, count_2, count_3) ----
#   Identify value = character
#   Check count field for comment
#   Revise table if needed
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
legacy_attributes_deployment <- 
  legacy_attributes %>%
  filter(sheet == "Deployment")
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