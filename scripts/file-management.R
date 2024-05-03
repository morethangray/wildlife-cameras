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
# Define site  ----
index_site = "PWD"
index_year = "2024"
# ========================================================== -----
# ========================================================== -----
# Compare inventory tracking ----
dlog_ilog_compare <- fxn_dlog_ilog_compare(index_site)
# View(dlog_ilog_compare[["done_catalog"]])
# View(dlog_ilog_compare[["done_qc"]])
# View(result_list[["done_tidy"]])
# 
# Find new files ----
# Folders  0P, 0M
new_folders <- fxn_dir_jpg_find_new(index_site, index_year) 
#
# Image tables 
fxn_table_find_new(index_site)
# 
# Need processing ----
# #   Blank: 0P; 0M
# check_blank <- fxn_find_files_to_process("blank")
# #
# #   Tidy: 0P; 0M
# check_tidy <- fxn_find_files_to_process("tidy")
# # check_tidy %>%
# #   filter(need_process == TRUE)
# #
# #   Vault: 24P; 0M
# check_vault <- fxn_find_files_to_process("vault")
# # check_vault %>%
# #   filter(need_process == TRUE)
# #
# ========================================================== -----
# ========================================================== -----
# REVIEW IMAGE FOLDERS ----
# ---------------------------------------------------------- -----
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
#   [NOT RUN] Rename image folders ----
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
# ========================================================== -----
# PROCESS IMAGE FILES ----
# ---------------------------------------------------------- -----
# Map jpg files ----
# jpg_map_files <- fxn_jpg_map_files(index_site, 
#                                    index_year,
#                                    done_rename = FALSE)
# 
# jpg_map_files %>%
#   group_by(id) %>%
#   count()
                  
# Check for timestamp errors -----
#   - Confirms image timestamp is within survey dates
#   - Checks for file size = 0 (corrupt files)
fxn_jpg_timestamp_check(index_site, index_year)
#
#   [BY HAND] Update has_data in dlog ----
#
# Preview file naming conventions ----
#   - Count number of images with and without parentheses
#   - Returns a table with examples for 0, 1, 2+ parentheses
jpg_summary <- fxn_jpg_summary(index_site, 
                               index_year,
                               done_rename = FALSE)

# Rename images & create _exif files ----
# Uses regex to replace "2019:01:01" with "2019-01-01"
fxn_jpg_rename_exif_01(index_site, index_year)
#
#   [BY HAND] Update done_rename in dlog ----
# Check renamed files ----
check_rename <- fxn_jpg_check_rename(index_site, index_year) 
# # Rename files if needed
# fxn_jpg_rename_redo(jpg_rename_check)
#
#   [BY HAND] Update done_exif in dlog ----
#
# ---------------------------------------------------------- -----
# Summarize survey attributes ----
# Use as source for date_img_from, date_img_to, count
fxn_exif_summary(index_site) %>%
  remove_empty("cols")
#
# Check for exif errors ----
fxn_exif_summary_errors(index_site)
#
#   [BY HAND] Update deployment log as follows: -----
#
# When id is PRESENT in exif_summary_errors: 
#   - flag error with done_fix = F until addressed
#   - check exif table, jpg files to get correct values
#   - Add error_type, error_subtype, and comments
#
# When id is ABSENT from exif_summary_errors:
#   - date_image_from matches date_from
#   - date_image_to matches date_to
#   - date_excl_from and date_excl_to are N/A
#   - use_data is TRUE
#
# ---------------------------------------------------------- -----
# Create blank image tables ----
fxn_table_create_blank(index_site)
#
#   [BY HAND] Update done_blank in dlog ----
# Check blank image tables ----
fxn_table_check_blank(index_site) 
#
# ========================================================== -----
# ========================================================== -----
# PROCESS CATALOGED IMAGE TABLES  ----
# ---------------------------------------------------------- -----
# Review cataloged image tables ----
# dir_table <- fxn_dir_table_map()
#
# Check cataloged image table format and values ----
fxn_table_check_catalog(index_site, 
                        index_type = "catalog")
#
# Identify new photo_type_binomial combinations ----
# OUTPUT: Writes _new-combinations.csv
fxn_new_photo_type_binomial_all(index_site, 
                                index_type = "catalog")
#
# Compare error flags with dlog ----
# These should be in dlog; add to dlog if not 
check_errors <- 
  fxn_table_check_errors(index_site,
                         index_type = "catalog")
check_errors
#
# Create _tidy image tables ----
#
fxn_tidy_for_qc(index_site)
#
# ========================================================== -----
# ========================================================== -----
# PROCESS QC IMAGE TABLES ----
# ---------------------------------------------------------- -----
# Check QC'd image tables ----
check_catalog <- fxn_table_check_catalog(index_site, 
                                         index_type = "qc") 
#
check_qc <- fxn_table_check_qc(index_site)
#
# Create clean image tables in vault ----
fxn_tidy_for_vault(index_site)
#
# ========================================================== -----
# README ----
# author: Morgan Gray
# created: 2023-01-09


