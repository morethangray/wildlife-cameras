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
# FOR: Pepperwood forest plots
# DEN: DENDRA D-RAI cameras
#
index_site = "PWD"
index_year = "2024"
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
# CAMERA METADATA ----
# Collate deployment summary tables ----
# Create Camera metadata ----
# Use deployment summary, captain's log as input
#
# project_id	:	confirm
# camera_id	:	define from serial number
# make	:	get from captain's log, deployment summary
# model	:	get from captain's log, deployment summary
# serial_number	:	get from captain's log, deployment summary
# year_purchased	:	confirm
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