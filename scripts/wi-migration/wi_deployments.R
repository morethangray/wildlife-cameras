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

# ========================================================== -----
# DEPLOYMENT METADATA ----
# Confirm camera attributes for all years (2012-2023) ----
# camera coordinates, quiet period, height, angle
#
# Create Deployment metadata ----
list_columns_deployments <- c("project_id",
                              "deployment_id",
                              "subproject_name",
                              "subproject_design",
                              "placename",
                              "longitude",
                              "latitude",
                              "start_date",
                              "end_date",
                              "event_name",
                              "event_description",
                              "event_type",
                              "bait_type",
                              "bait_description",
                              "feature_type",
                              "feature_type_methodology",
                              "camera_id",
                              "quiet_period",
                              "camera_functioning",
                              "sensor_height",
                              "height_other",
                              "sensor_orientation",
                              "orientation_other",
                              "recorded_by",
                              "plot_treatment",
                              "plot_treatment_description",
                              "detection_distance")

camera_lon_lat <- read_xlsx(here(path_in,
                                 "attributes_cameras.xlsx"), 
                            sheet = "attributes_cameras") %>%
  filter(grid == "P") %>%
  select(camera, lon_x, lat_y)

subset_dlog_wi <- 
  dlog_wi %>%
  filter(migrate_wi == TRUE) %>%
  left_join(camera_lon_lat, "camera") %>%
  select(deployment_id = id,
         placename = camera, 
         longitude = lon_x, 
         latitude = lat_y,
         start_date = date_from, 
         end_date = date_to, 
         camera_id = serial_number, 
         camera_functioning = cam_fail_details)  


deployments <- 
  subset_dlog_wi %>%
  mutate(
    project_id = "Pepperwood",
    subproject_name = "None",
    subproject_design = "None",
    event_name = "None",
    event_description = "None",
    event_type = "None",
    bait_type = "None",
    bait_description = "None",
    feature_type = "None",
    feature_type_methodology = "None",
    quiet_period = 5,
    sensor_height = "Knee height",
    height_other = "None",
    sensor_orientation = "Parallel",
    orientation_other = "None",
    recorded_by = "None",
    plot_treatment = "None",
    plot_treatment_description = "None",
    detection_distance = "None"
  ) %>%
  relocate(all_of(list_columns_deployments))
#
#   Write/read csv ----
# deployments %>%
#   write_csv(here(path_out_wi_migration, "metadata_deployments_all.csv"))

deployments <- read_csv(here(path_out_wi_migration, "metadata_deployments_all.csv"),
                           col_types = cols(
                             .default = col_character(),
                             longitude = col_double(),
                             latitude = col_double(),
                             start_date = col_date(format = ""),
                             end_date = col_date(format = ""), 
                             quiet_period = col_double(),
                           ))

# ========================================================== -----
# PROJECT METADATA ----
# Create Project metadata ----
project <- 
  tibble(project_name = "Pepperwood Wildlife Camera Grid",
         project_id = "Pepperwood",
         project_short_name = "PWD",
         project_objectives = "To collect observation data on terrestrial wildlife species present at Pepperwood Preserve using a long-term grid of 20 cameras",
         project_species = "Multiple",
         project_species_individual = "None",
         project_sensor_layout = "Systematic",
         project_sensor_layout_targeted_type = "None",
         project_bait_use = "No",
         project_bait_type = "None",
         project_stratification = "Yes",
         project_stratification_type = "Distance",
         project_sensor_method = "Sensor Detection",
         project_individual_animals = "No",
         project_blank_images = "No",
         project_sensor_cluster = "No",
         project_admin = "Pepperwood Foundation",
         project_admin_email = "research@pepperwoodpreserve.org",
         country_code = "USA",
         embargo = 48,
         metadata_license = "CC0",
         image_license = "CC0",
         project_type = "Image")

#   Write/read csv ----
project %>%
  write_csv(here(path_out_wi_migration, "metadata_project.csv"))

project <- read_csv(here(path_out_wi_migration, "metadata_project.csv"),
                        col_types = cols(
                          .default = col_character(),
                          embargo = col_double()))

# ========================================================== -----
