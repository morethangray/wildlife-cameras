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
# Define output file paths ----
path_wi <- here(path_site, "wi-migration")
path_wi_images <- here(path_wi, "images")
path_wi_metadata <- here(path_wi, "metadata")
# Define functions ----
#   fxn_write_metadata_csv ----
fxn_write_metadata_csv <- function(){
  
  list_id_wi <-
    dlog_wi %>%
    filter(migrate_wi == TRUE) %>%
    arrange(id) %>%
    pull(id)
  
  # list_id_wi <- unique(dlog_wi$id)  
  
  for(index_id in list_id_wi){
    
    list_metadata <- c("image", 
                       "deployment", 
                       "camera", 
                       "project")
    
    for(index_metadata in list_metadata){
      
      input_data <- get(index_metadata) 
      
      if(index_metadata != "project"){
        
        input_data <- input_data %>%
          filter(deployment_id == index_id) 
      }
      
      file_name <- paste0(index_id, "_", index_metadata, ".csv")
      write_csv(input_data, here(path_wi_metadata, file_name))
      
    }
  }
}

#
# ========================================================== -----
# CREATE METADATA INPUT ----
# image ----
image <- fxn_create_metadata_image(index_type = "archive")
#
# write_csv(image, here(path_wi, "metadata_image_all.csv"))
#
# deployment ----
# NOTE: Some deployments missing camera serial number
deployment <- fxn_create_metadata_deployment()
#
# write_csv(deployment, here(path_wi, "metadata_deployment_all.csv"),
#           na = "")
#
# project ----
# This is the same for every deployment
project <- fxn_create_metadata_project()
#
# write_csv(project, here(path_wi, "metadata_project_all.csv"),
#           na = "")
#
# camera ----
# NOTE: Some cameras missing make, model, year
camera <- fxn_create_metadata_camera()
#
# write_csv(camera, here(path_wi, "metadata_camera_all.csv"),
#           na = "")
#
# ========================================================== -----
# ITERATE BY SURVEY TO CREATE CSV ----
fxn_write_metadata_csv()
#
# ========================================================== -----
# GRAVEYARD ----
#   all_vault ----
# To create new table (takes about 5 minutes)
# For after new image tables added to vault
# all_vault <- fxn_collate_all_vault(index_type = "new") %>%
#   filter(id %in% unique(dlog_wi$id))
#
# To use existing table 
# all_vault <- fxn_collate_all_vault(index_type = "archive") %>%
#   filter(id %in% unique(dlog_wi$id))
# 
#   tidy_vault_long -----
# tidy_vault_long <- 
#   all_vault %>%
#   fxn_create_binomial_count_long() %>%
#   fxn_correct_typos() %>%
#   fxn_tidy_comments() %>%
#   fxn_add_taxonomic_attributes() %>%
#   fxn_create_identified_by()
#
# ========================================================== -----