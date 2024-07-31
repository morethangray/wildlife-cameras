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
path_wi <- here(path_site, "wi-migration")

# ========================================================== -----
# CREATE METADATA INPUT ----
# image ----
image <- fxn_create_metadata_image(index_type = "archive")
#
write_csv(image, here(path_wi, "Metadata", "images.csv"))
#
# deployment ----
# NOTE: Some deployments missing camera serial number
deployment <- fxn_create_metadata_deployment()
#
write_csv(deployment, here(path_wi, "Metadata", "deployments.csv"),
          na = "")
#
# project ----
# This is the same for every deployment
project <- fxn_create_metadata_project()
#
write_csv(project, here(path_wi, "Metadata", "projects.csv"),
          na = "")
#
# camera ----
# NOTE: Some cameras missing make, model, year
camera <- fxn_create_metadata_camera()
#
write_csv(camera, here(path_wi, "Metadata", "cameras.csv"),
          na = "")
#
# ========================================================== -----
# CREATE METADATA CSV FILES ----
#   fxn_write_metadata_csv_test ----
# index_type = "test"
fxn_write_metadata_csv <- function(index_type){
  
  # Define type of data creation (test or actual) ----
  if(index_type == "test"){
    # For test folder
    path_wi_metadata <- here(path_wi, "test/Pepperwood/Metadata")
    list_id_wi <- c("P_E4_211201",
                    "P_B4_211129",
                    "P_B4_211018")
  }else{
    # For actual migration 
    path_wi_metadata <- here(path_wi, "metadata")
    list_id_wi <-
      dlog_wi %>%
      filter(migrate_wi == TRUE) %>%
      arrange(id) %>%
      pull(id)
  }
  
  # Create metadata and write to folder ----
  list_metadata <- c("image", 
                     "deployment", 
                     "camera", 
                     "project")
  
  # index_metadata = list_metadata[1]
  for(index_metadata in list_metadata){
    
    input_data <- get(index_metadata) 
    file_name <- paste0(index_metadata, "s.csv")
    
    if(index_metadata != "project"){
      column_names <- get(paste0("list_columns_", index_metadata))
      
      input_data <- input_data %>%
        filter(deployment_id %in% list_id_wi)  %>%
        select(all_of(column_names))
    }
    
    write_csv(input_data, here(path_wi_metadata, file_name), 
              na = "")
  }
}
#
# Apply function for test folder ----
fxn_write_metadata_csv(index_type = "test")
#
# ========================================================== -----
# To do ----
# Add 13 deployments after QC ----
# Need image tables
# "P_A1_210226" "P_A1_210607" "P_A2_201207" "P_A2_210308" 
# "P_A2_210719" "P_A2_210830" "P_A4_201013" "P_A4_211129" 
# "P_C3_210121"  "P_C5_210312" "P_D3_210122" "P_D3_210720" 
# "P_E4_210720"

# Address missing timestamps ----
# delete images and rows?
# 1 P_A1_200610_00011
# 2 P_C2_200902_00156
# 3 P_C4_200611_01421
# 4 P_C4_200611_02372
# 5 P_D2_201014_01528
# 6 P_E4_200903_04946
# Add 62 missing camera_id to camera ----
# Add 62 missing camera_id to deployment ----
# [x] Correct image$location ----
# add drive and folders
# ========================================================== -----
