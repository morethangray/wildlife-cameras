# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
#
source(here("scripts/functions/fxn_utilities.R"))
source(here("scripts/functions/fxn_utilities_wi.R"))
source(here("scripts/functions/fxn_image-tables.R"))
# 
# ---------------------------------------------------------- -----
# Define site and attributes ----
fxn_define_camera_project(index_site)

# ========================================================== -----
# CREATE METADATA ----
# Create Deployment metadata ----
deployment <- fxn_create_metadata_deployment()
#
#   Write/read csv ----
# deployments %>%
#   write_csv(here(path_out_wi_migration, "metadata_deployment_all.csv"))
#
# deployments <- read_csv(here(path_out_wi_migration, "metadata_deployment_all.csv"),
#                            col_types = cols(
#                              .default = col_character(),
#                              longitude = col_double(),
#                              latitude = col_double(),
#                              start_date = col_date(format = ""),
#                              end_date = col_date(format = ""), 
#                              quiet_period = col_double(),
#                            ))
#
# Create Project metadata ----
project <- fxn_create_metadata_project()

#   Write/read csv ----
# project %>%
#   write_csv(here(path_out_wi_migration, "metadata_project.csv"))
# 
# project <- read_csv(here(path_out_wi_migration, "metadata_project.csv"),
#                         col_types = cols(
#                           .default = col_character(),
#                           embargo = col_double()))

# ========================================================== -----
