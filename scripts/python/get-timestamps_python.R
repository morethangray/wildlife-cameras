# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
library(reticulate)   ## Interface to Python
#
source(here("scripts/functions/fxn_utilities.R"))
# source(here("scripts/functions/fxn_tracking.R"))
# source(here("scripts/functions/fxn_folders.R"))
# source(here("scripts/functions/fxn_images.R"))
# source(here("scripts/functions/fxn_image-tables.R"))
# 
# ---------------------------------------------------------- -----
# Define site  ----
# PWD: Pepperwood WPI grid
# MMP: Modini WPI grid
# FOR: Pepperwood forest plots
# DEN: DENDRA D-RAI cameras
#
index_site = "PWD"
# ========================================================== -----
# Install python ----


# Ensure the correct Python environment is used -----
path_python <- here(path_r, "scripts/python")
use_python(path_python) 

# Source the Python script
source_python("path/to/extract_timestamps.py")

# Define the folder path in R
folder_path <- "path/to/your/folder"

# Call the Python function with the folder path from R
timestamps_df <- py$get_image_timestamps(folder_path)

# Print the resulting DataFrame
print(timestamps_df)


# Define the path to the Python script
python_script <- here(path_r, 
                      "scripts/python",
                      "extract_timestamps.py")

path_folder <- path_jpg

# TO DO ----
# Confirm script is ok for PC
# Confirm pass folder to python script
# Confirm .jpg is case insensitive 
  
# Benchmark python vs R ----
#   Python ----
t1 <- Sys.time()
system(paste("python3", python_script))
Sys.time()-t1
# 4.43s for 2803 images

# Read the resulting CSV file into R
timestamps <- read.csv('timestamps.csv')
print(timestamps)

#   R ---- 
t2 <- Sys.time()
exif_read(path = path_folder, 
          recursive = TRUE,
          pipeline = "csv",
          tags = list_exif_tags) %>%
  clean_names()  %>%
  as_tibble() 
Sys.time()-t2
# 25.79s for 2803 images
