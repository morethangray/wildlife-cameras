# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
#
source(here("scripts/functions/fxn_utilities.R"))
source(here("scripts/functions/fxn_utilities_wi.R"))
source(here("scripts/functions/fxn_images.R"))
source(here("scripts/functions/fxn_image-tables.R"))
# 
# ---------------------------------------------------------- -----
# Define site and attributes ----
fxn_define_camera_project(index_site)
path_wi <- here(path_site, "wi-migration")

# ========================================================== -----
# Map jpg files ----
list_folders <- 
  c("L:/WPI Project/PWD/2016/images", 
    "L:/WPI Project/PWD/2017/images", 
    "L:/WPI Project/PWD/2018/images", 
    "L:/WPI Project/PWD/2019/images", 
    "L:/WPI Project/PWD/2020/images",
    "L:/WPI Project/PWD/2021/images",
    "L:/WPI Project/PWD/2022/images",
    "L:/WPI Project/PWD/2023/images")

# index_folder <- list_folders[5]
datalist <- list()
for(index_folder in list_folders){
  
  cat(index_folder, "\n")

  datalist[[index_folder]] <- 
    tibble(path = dir_ls(path = index_folder,
                         recurse = TRUE, 
                         type = "file")) %>%
    filter(str_detect(path, "archive") == FALSE) %>%
    mutate(parent_directory = basename(dirname(path)),
           file_name = path_file(path),
           extension = path_ext(path)) %>%
    select(parent_directory, 
           file_name, 
           extension,
           path)
    
}

combined_df <- bind_rows(datalist) %>%
  mutate(file_name = tools::file_path_sans_ext(file_name)) %>%
  select(id = parent_directory, 
         file_name, 
         extension) 
 
# Check file names  ----
# 21 characters
# contains ID
# No img text (IMG, img, JPG, jpg etc)
#
check_file_name <- 
combined_df %>%
  filter(extension %in% c("JPG")) %>%
  mutate(name_length = nchar(file_name)) %>%
  mutate(camera = str_sub(file_name, 1, 4), 
         date_to = str_sub(file_name, 6, 11), 
         img_n = str_sub(file_name, 13, 17))

check_file_name %>%
  filter(name_length == 17) %>%
  distinct(date_to) %>%
  pull()

check_file_name %>%
  filter(name_length != 17) %>%
  distinct(id)  

# Rename image files ----
path_jpg_l
index_list <- c("P_A1_210607", 
                "P_A1_210719",
                "P_B4_230620")
#
# Unnest folders  ----
# From: L:\WPI Project\PWD\2016\images\P_A1\P_A1_161019
# To: L:\WPI Project\PWD\2016-2021\P_A1_161019

# List of folders to process
list_folders <- c("L:/WPI Project/PWD/2016/images", 
                  "L:/WPI Project/PWD/2017/images", 
                  "L:/WPI Project/PWD/2018/images", 
                  "L:/WPI Project/PWD/2019/images", 
                  "L:/WPI Project/PWD/2020/images", 
                  "L:/WPI Project/PWD/2021/images")

# Function to unnest and move folders
unnest_and_move_folders <- function(folder_list, target_base) {
  for (folder in folder_list) {
    # List all subdirectories in the current folder
    subdirs <- list.dirs(folder, 
                         recursive = TRUE, 
                         full.names = TRUE)
    
    # Filter out the main folder itself from the list
    subdirs <- subdirs[subdirs != folder]
    
    # Move each subfolder to the target location
    for (subdir in subdirs) {
      # Extract the subfolder name
      subfolder_name <- basename(subdir)
      
      # Create the target directory path
      target_folder <- file.path(target_base,
                                 subfolder_name)
      
      # Create the target directory if it doesn't exist
      if (!dir.exists(target_folder)) {
        dir.create(target_folder, 
                   recursive = TRUE)
      }
      
      # Move the contents of the subfolder to the target directory
      file.rename(subdir, target_folder)
    }
  }
}

# Define the target base directory
target_base <- "L:/WPI Project/PWD/2016-2021"

# Unnest and move the folders
# unnest_and_move_folders(list_folders, target_base)
# ---------------------------------------------------------- -----
# ========================================================== -----

