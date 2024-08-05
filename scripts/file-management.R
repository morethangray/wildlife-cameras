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
# Find image tables that need processing ----
check_file_status <- fxn_check_file_status()
# Individual tables can be viewed using check_file_status$<NAME>
#   Rename images: 0P; 0M
check_file_status$check_rename
#   Exif: 0P; 0M
check_file_status$check_exif 
#   Blank: 0P; 0M
check_file_status$check_blank
#   Tidy: 0P; 0M
check_file_status$check_tidy 
#   Vault: 0P; 0M
check_file_status$check_vault 
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