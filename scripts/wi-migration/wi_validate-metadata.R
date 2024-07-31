# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(googlesheets4)   
#
source(here("scripts/functions/fxn_utilities.R"))
source(here("scripts/functions/fxn_utilities_wi.R"))
source(here("scripts/functions/fxn_image-tables.R"))
# 
# ---------------------------------------------------------- -----
# Define site and attributes ----
fxn_define_camera_project(index_site)
# Define output file paths ----
path_wi <- here(path_site, "wi-migration")
path_wi_images <- here(path_wi, "images")
path_wi_metadata <- here(path_wi, "metadata")
path_templates <- here("output/wi-migration/templates")
# Define functions ----
#   get_prj_values -----
get_prj_values <- function() {
  prj_std <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1iEcHs0Y49W5hx7aoMSFge_1-Q_VfMdl8d56x27heuNY", sheet = "Projectv1.0")
  prj_std <- prj_std %>% select(`Column name`, `Values List`)
  prj_std <- prj_std %>% filter(`Values List` != "None")
  prj_std <- prj_std[-c(2,12,13,7),]
  prj_vals <- str_split(prj_std$`Values List`, ",")
  names(prj_vals) <- prj_std$`Column name`
  prj_vals
}
#   get_dep_values -----
# Function to get the valid values for required variables in the deployments.csv file
get_dep_values <- function() {
  dep_std <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1iEcHs0Y49W5hx7aoMSFge_1-Q_VfMdl8d56x27heuNY", sheet = "Deploymentv1.0")
  dep_std <- dep_std %>% select(`Column name`, `Values List`)
  dep_std <- dep_std %>% filter(`Values List` != "None")
  dep_std <- dep_std[-c(1,6,8),]
  dep_vals <- str_split(dep_std$`Values List`, ",")
  names(dep_vals) <- dep_std$`Column name`
  dep_vals
}
# ========================================================== -----
# CREATE METADATA INPUT ----
# image ----
image <- fxn_create_metadata_image(index_type = "archive")
#
# deployment ----
# NOTE: Some deployments missing camera serial number
deployment <- fxn_create_metadata_deployment()
#
# project ----
# This is the same for every deployment
project <- fxn_create_metadata_project()
#
# camera ----
# NOTE: Some cameras missing make, model, year
camera <- fxn_create_metadata_camera()
#
# ========================================================== -----
# Validation checks ----
source(here("scripts/wi-migration/wi_helper_migration-functions.R"))
source(here("scripts/wi-migration/wi_helper_validation-functions.R"))
# Get values ----
save_csv_from_sheet()
values_prj <- get_prj_values()
values_dep <- get_dep_values()
# Check for missing fields ----

missing_fields(image, "images")
missing_fields(camera, "cameras")
missing_fields(deployment, "deployments")
missing_fields(project, "projects")

# Check for extra fields
flag_extra_fields(image, "images")
flag_extra_fields(camera, "cameras")
flag_extra_fields(deployment, "deployments")
flag_extra_fields(project, "projects")

# Check for missing required values 
missing_req_values(image, "images")
missing_req_values(camera, "cameras")
missing_req_values(deployment, "deployments")
missing_req_values(project, "projects")

# Check taxonomy
taxonomy_check(image)
# Check for duplicate deployments 
dup_dep_ids(deployment)

# Check for orphaned cameras
orphaned_cameras(camera, deployment)
# Check for orphaned deployments
orphaned_deployments(deployment, image)
# 
# image %>%
#   filter(deployment_id %in% "P_A1_210226")
# deployment %>%
#   filter(deployment_id %in% "P_A1_210226")

# Confirm project ids match
projectids_match(image, camera, deployment, project)

# Check date format
date_format_check(deployment$start_date, "deployment start date...")
date_format_check(deployment$end_date, "deployment end date...")
date_format_check(image$timestamp, "image timestamp...")

validate_dep_dates(deployment)
images_in_dep_dt_range(image, deployment)
