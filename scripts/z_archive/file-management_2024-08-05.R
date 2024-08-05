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
index_site = "FOR"
index_year = "2024"
# ========================================================== -----
# ========================================================== -----
# * * * REVISE MESSAGES * * *  ----
# "ok" messages as cat()
# only red when need action 
# Compare inventory tracking ----
dlog_ilog_compare <- fxn_dlog_ilog_compare(index_site)
# View(dlog_ilog_compare[["done_catalog"]])
# View(dlog_ilog_compare[["done_qc"]])
# View(dlog_ilog_compare[["done_tidy"]])
# View(dlog_ilog_compare[["done_vault"]])
# 
# Find new files ----
# Folders  0P, 0M
new_folders <- fxn_dir_jpg_find_new(index_site, index_year) 
#   * * * WRITE CSV AS DLOG TEMPLATE * * *  ----
#
# Image tables 
#   fxn_table_find_new ----
# index_type = "catalog"
fxn_table_find_new <- function(index_site) {
  
  # Define camera project (set up environment for function)
  fxn_define_camera_project(index_site)
  
  # Types to iterate over
  index_types <- c("blank", "catalog", "qc")
  
  # Helper functions ----
  #   get_done_ids ----
  get_done_ids <- function(index_type){
    
    # Initialize index_path
    index_path <- NULL
    
    # Use a single if-else construct
    if(index_type == "blank") {
      
      filter_dlog <- dlog %>%
        filter(use_data == TRUE,
               done_blank == TRUE)
      index_path <- path_table_blank
      
    } else if(index_type == "catalog") {
      
      filter_dlog <- dlog %>%
        filter(use_data == TRUE,
               done_catalog == TRUE)
      index_path <- path_table_catalog
      
    } else if(index_type == "qc") {
      
      filter_dlog <- dlog %>%
        filter(use_data == TRUE,
               done_qc == TRUE)
      index_path <- path_table_qc
      
    } else {
      stop("Invalid index_type provided")
    }
    
    # Return a list containing the IDs and the corresponding path
    list(
      ids = filter_dlog %>%
        arrange(id) %>%
        pull(id),
      path = index_path
    )
  }
  #   create_lookup_subset ----
  # Helper function to construct the lookup subset table
  create_lookup_subset <- function(index_type) {
    tibble(
      type = c("blank", "catalog", "tidy", "qc"),
      suffix = c("blank", "final", "final_tidy", "final_tidy_qc"),
      folder = c("blank", "catalog", "qc", "qc"),
      length = c(17, 17, 22, 28)
    ) %>%
      mutate(path = here(path_data,
                         paste0("image-tables_",
                                folder))) %>%
      filter(type %in% all_of(index_type))
  }
  
  #   map_image_tables ----
  # Helper function to map new image tables
  map_image_tables <- function(lookup_subset,
                               index_type_suffix,
                               list_id) {
    dir_ls(path = lookup_subset$path,
           recurse = FALSE,
           type = "file") %>%
      tibble(path = .) %>%
      mutate(
        file_name = path_file(path_ext_remove(path)),
        file_name = str_replace_all(file_name, "Final", "final"),
        file_name_length = nchar(file_name)
      ) %>%
      filter(
        str_detect(file_name, index_type_suffix),
        file_name_length == lookup_subset$length
      ) %>%
      mutate(
        id = str_sub(file_name, 1, 11),
        type = lookup_subset$type,
        suffix = lookup_subset$suffix,
        file_suffix = str_remove_all(file_name, paste0(id, "_"))
      ) %>%
      select(id, type, suffix, file_suffix, file_name) %>%
      filter(id %nin% list_id$ids)
  }
  # Main function to find new image tables for all index types ----
  # index_type = index_types[3]
  for (index_type in index_types) {
    index_done <- paste0("done_", index_type)
    list_id <- get_done_ids(index_type)
    
    lookup_subset <- create_lookup_subset(index_type)
    index_path <- lookup_subset$path
    index_type_suffix <- lookup_subset$suffix
    
    tbl_new <- map_image_tables(lookup_subset,
                                index_type_suffix,
                                list_id)
    
    list_id_new <- unique(tbl_new$id)
    
    n_new <- length(list_id_new)
    
    if (n_new == 0) {
      message("No new ", index_done,
              " image tables for ", index_site)
    } else {
      message("Revise deployment log: ",
              n_new, " new ", index_done,
              " image table(s) for ", index_site)
      print(tbl_new)
    }
  }
}

fxn_table_find_new(index_site)

# 
# Need processing ----
#   Rename images: 0P; 0M
check_rename <- fxn_find_files_to_process("rename")
#
#   Exif: 0P; 0M
check_exif <- fxn_find_files_to_process("exif")
check_exif %>%
  filter(need_process == TRUE)
#
#   Blank: 0P; 0M
check_blank <- fxn_find_files_to_process("blank")
#
#   Tidy: 0P; 0M
check_tidy <- fxn_find_files_to_process("tidy")
# check_tidy %>%
#   filter(need_process == TRUE)
#
#   Vault: 0P; 0M
check_vault <- fxn_find_files_to_process("vault")
# check_vault %>%
#   filter(need_process == TRUE) %>%
#   pull(id)
# #
# ========================================================== -----
# REVIEW IMAGE FOLDERS ----
# ---------------------------------------------------------- -----
#   [BY HAND] Add new surveys to dlog ----
#
# Use <SITE>_folder-names_init.csv 
# File located in K:\wildlife-cameras\output\<SITE>
# File name: <SITE>_folder-names_init
#
#   [BY HAND] Add id to ilog ----
# Check folder names ----
# Compare folder naming convention with dlog 
dir_summary_errors <- 
  fxn_dir_jpg_map(index_site,  index_year)  %>%
  select(id, 
           camera_name, 
           parent_name, 
           path,
           starts_with("err")) %>%
  filter(err_id == TRUE)
#
dir_summary_errors
#
# Check for multiple folders (i.e., errors)
dir_summary_errors %>%
  group_by(id, 
           parent_name) %>%
  count() %>%
  filter(n>1)
#
# ========================================================== -----
# PROCESS IMAGE FILES ----
# ---------------------------------------------------------- -----
# Rename images & create _exif files ----
#   Can take a long time if folders have >1000 images
#   This function does the following: 
#   - Confirms image timestamp is within survey dates
#   - Checks for file size = 0 (corrupt files)
#   - Shows error messages in console  
#   - Stops if an error is encountered
#   You need to fix any errors before resuming data processing
fxn_jpg_rename_exif(index_site, index_year)
#
#   [BY HAND] Update dlog (x2) ----
#   - Define has_data = TRUE
#   - Define done_rename = TRUE
#
# Check renamed files ----
# has_data in dlog must be updated (=TRUE)
check_rename <- fxn_jpg_check_rename(index_site, index_year) 
#   [BY HAND] Update dlog (x1) ----
#   - Define done_exif = TRUE
#
# ---------------------------------------------------------- -----
# Summarize survey attributes ----
fxn_exif_summary(index_site) %>%
  remove_empty("cols")
#
#   [BY HAND] Update dlog (x3) ----
#
# File located in K:\wildlife-cameras\output\<SITE>
# File name: <SITE>_exif-summary
#
# Use as source for dlog columns:
#   - Define date_img_from
#   - Define date_img_to
#   - Define n_img_to  
#
# Check for exif errors ----
# Subset of exif summary with errors
fxn_exif_summary_errors(index_site)
#
#   [BY HAND] Review _exif-summary_errors ----
#
# File located in K:\wildlife-cameras\output\<SITE>
# File name: <SITE>_exif-summary_errors
#
# No new file will be created if there are no errors
#
#   [BY HAND] Update dlog -----
#
# For each row in dlog do the following:
# When id is PRESENT in exif_summary_errors: 
#   - Flag error with done_fix = FALSE until addressed
#   - Get correct values (check exif table, jpg files)
#   - Add error_type
#   - Add error_subtype
#   - Add comments
#
# When id is ABSENT from exif_summary_errors:
#   - Define date_image_from = date_from
#   - Define date_image_to = date_to
#   - Define date_excl_from = N/A
#   - Define date_excl_to = N/A
#   - Define use_data = TRUE
#
# ---------------------------------------------------------- -----
# Create blank image tables ----
fxn_table_create_blank(index_site)
#
#   [BY HAND] Update done_blank in dlog ----
#   [BY HAND] Update done_blank in ilog ----
fxn_dlog_ilog_compare(index_site)
#
# Check blank image tables ----
fxn_table_check_blank(index_site) 
#
#   [BY HAND] Review _blank_error-summary ----
#
# File located in K:\wildlife-cameras\output\<SITE>
# File name: <SITE>_blank_error-summary
#
# No new file will be created if there are no errors
#
# ========================================================== -----
# PROCESS CATALOGED IMAGE TABLES (TIDY)  ----
# ---------------------------------------------------------- -----
# Check cataloged image table format and values ----
fxn_table_check_catalog(index_site, index_type = "catalog")
#
# Check count format ----
fxn_check_count_format(index_site, index_path = path_table_catalog)
#
# Identify new photo_type_binomial combinations ----
# OUTPUT: Writes _new-combinations.csv
fxn_new_photo_type_binomial_all(index_site, index_type = "catalog")
#
# Compare error flags with dlog ----
# These should be in dlog; add to dlog if not 
check_errors <- 
  fxn_table_check_errors(index_site,
                         index_type = "catalog")
check_errors
#
# Create _tidy image tables ----
fxn_tidy_for_qc(index_site)
#
#   [BY HAND] Update done_tidy in dlog ----
#   [BY HAND] Update done_tidy in ilog ----
fxn_dlog_ilog_compare(index_site)
#
# ========================================================== -----
# PROCESS QC IMAGE TABLES (VAULT) ----
# ---------------------------------------------------------- -----
# Check QC'd image tables ----
check_catalog <- fxn_table_check_catalog(index_site, index_type = "qc") 
check_catalog
#
fxn_check_count_format(index_site, index_path = path_table_qc)
#
check_qc <- fxn_table_check_qc(index_site)
#
# Create clean image tables in vault ----
fxn_tidy_for_vault(index_site)
#
#   [BY HAND] Update done_vault in dlog ----
#   [BY HAND] Update done_vault in ilog ----
fxn_dlog_ilog_compare(index_site)
#
# ========================================================== -----


# ========================================================== -----
# TROUBLESHOOTING FUNCTIONS -----
#   Not typically included in the data processing workflow 
#   Can be helpful for ad hoc troubleshooting 
# Map jpg files ----
jpg_map_files <- fxn_jpg_map_files(index_site,
                                   index_year,
                                   done_rename = FALSE)

jpg_map_files %>%
  group_by(id) %>%
  count()
#
# Preview file naming conventions ----
#   - Count number of images with and without parentheses
#   - Returns a table with examples for 0, 1, 2+ parentheses
jpg_summary <- fxn_jpg_summary(index_site, 
                               index_year,
                               done_rename = FALSE)
#
# Check for timestamp errors -----
#   Can take a long time if folders have >1000 images
#   - Confirms image timestamp is within survey dates
#   - Checks for file size = 0 (corrupt files)
#   - Shows error messages in console  
fxn_jpg_timestamp_check(index_site, index_year)
# Review cataloged image tables ----
dir_table <- fxn_dir_table_map()
#
# Rename image folders ----
# Preview names (without renaming):
# fxn_dir_jpg_rename(index_site, 
#                    index_year, 
#                    index_pattern = "YYYYMMDD_YYYYMMDD", 
#                    do_rename = FALSE)
#
# fxn_dir_jpg_rename(index_site, 
#                    index_year, 
#                    index_pattern = "YYYYMMDD_YYYYMMDD", 
#                    do_rename = TRUE)
# 
# ========================================================== -----