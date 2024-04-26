# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
#
source(here("scripts/functions/fxn_utilities.R"))
source(here("scripts/functions/fxn_image-tables.R"))
# 
# Define site  ----
index_site = "PWD"
# index_year = "2023"
fxn_define_camera_project(index_site = index_site)
#
# ---------------------------------------------------------- -----
# New libraries, functions ----
library(camtrapR)

camera_attributes <- 
  read_csv(here(path_in, "attributes_cameras.csv"))

# ========================================================== -----
# CHECK YEARS WITH DATA ----
dlog %>%
  filter(use_data == TRUE) %>%
  mutate(done = done_catalog) %>%
  group_by(year_to, done) %>%
  count() %>%
  spread(done, n)

dlog %>%
  filter(use_data == TRUE) %>%
  mutate(done = done_vault) %>%
  group_by(year_to, done) %>%
  count() %>%
  spread(done, n)

# ========================================================== -----
# COLLATE IMAGE TABLES FROM VAULT -----
# List the deployments to collate ----
list_id <- 
  dlog %>%
  filter(done_vault == TRUE) %>%
  arrange(id) %>%
  pull(id)

# index_id = list_id[2]
# Function to process each file (done_vault) ----
read_and_process_file_vault <- function(index_id) {

  index_path <- here(path_vault, 
                     paste0(index_id, "_clean.xlsx"))
  
  if(!file.exists(index_path)) {
    stop("File does not exist: ", index_path)
  }
  
  xlsx_data <- 
    read_excel(index_path, 
               sheet = "Images",
               col_types = 
                 c(image_id = "text",
                   date = "date",
                   time = "text",
                   photo_type = "text",
                   binomial_1 = "text",
                   count_1 = "numeric",
                   comment = "text",
                   catalog_by = "text",
                   review ="text",
                   good = "text",
                   error = "text",
                   qc_by = "text",
                   qc_certainty = "text",
                   binomial_2 = "text",
                   count_2 = "numeric",
                   binomial_3 = "text",
                   count_3 = "numeric",
                   id = "text",
                   image_n = "numeric",
                   image_file = "text",
                   date_time = "date")) %>%
    mutate(date = as_date(date_time),
           time = as_hms(strptime(time, 
                                  format = "%H:%M:%S")))  %>%
    rename(id_init = id) %>%
    mutate(id = index_id, 
           image_id = str_replace(image_id, id_init, id), 
           image_file = str_replace(image_file, id_init, id)) %>%
    select(all_of(list_column_names))  
  
  cat(index_id, "\n")
  
  return(xlsx_data)
}

# Read and collate files ----
datalist <- map_df(list_id, read_and_process_file_vault)

# Combine the list of data frames into one data frame
images_all <- bind_rows(datalist)


# ---------------------------------------------------------- -----
# COLLATE CATALOGED IMAGE TABLES -----
# Function to process each file (done_catalog) ----
read_and_process_file_catalog <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # List the deployments to collate ----
  list_id <- 
    dlog %>%
    filter(done_catalog == TRUE, 
           done_vault == FALSE) %>%
    arrange(id)  %>%
    pull(id)
  
  # Read and collate files ----
  datalist <- list()
  # index_id <- list_id[1]
  for(index_id in list_id){
    
    index_file <- paste0(index_id, "_final.xlsx")
    
    datalist[[index_id]] <-
      fxn_table_read_xlsx(index_file = index_file,
                          index_type = "catalog/z_archive")
  }
  bind_datalist <- do.call(bind_rows, datalist)
}

images_all_catalog <- read_and_process_file_catalog(index_site)
# ---------------------------------------------------------- -----
# Combine tables and write csv ----
bind_images_all <- 
 bind_rows(images_all, 
           images_all_catalog)
# Subset to images of wild mammal species (wms) 
images_wms <- 
  bind_images_all %>%
  filter(binomial_1 %in% list_binomial_wms |
           binomial_2 %in% list_binomial_wms |
           binomial_3 %in% list_binomial_wms)

# Get dlog information
list_id_tables <- unique(bind_images_all$id)

deployments <- 
  dlog %>%
  filter(id %in% list_id_tables_catalog) %>%
  select(-starts_with("done"), 
         -has_data,
         -use_data, 
         -date_added, 
         -id_orig) %>%
  remove_constant(na.rm = TRUE)

# Write files to csv ----
images_wms %>%
  write_csv(here(path_out,
                 paste0(index_site, 
                        "_images-wms_",
                        Sys.Date(),
                        ".csv")
  ))

#
deployments %>%
  write_csv(here(path_out,
                 paste0(index_site, 
                        "_deployments_",
                        Sys.Date(),
                        ".csv")
  ))
#     
# Image count ----
images_wms %>%
  group_by(year, species) %>%
  summarize(total = n()) %>%
  spread(year, total)

detections_30m_year_species %>%
  write_csv(here(path_out,
                 paste0(index_site, 
                        "_images-wms_detections_30m_year_species_",
                        Sys.Date(),
                        ".csv")), 
            na = "0"
  )
# ---------------------------------------------------------- -----
# CHECK IMAGE TABLES ----
# images_all <- 
#   read_csv(here(path_out, 
#                 "images-all_2020-2023_2023-11-06.csv"),
#            col_types = c(
#              image_id = "c", 
#              date = "D", 
#              time = "t", 
#              photo_type = "c", 
#              binomial_1 = "c", 
#              count_1 = "i", 
#              binomial_2 = "c", 
#              count_2 = "i", 
#              binomial_3 = "c", 
#              count_3 = "i", 
#              qc_certainty = "c", 
#              qc_by = "c", 
#              catalog_by = "c", 
#              good = "l",
#              review = "l",
#              error = "l", 
#              id = "c", 
#              image_n = "i", 
#              image_file = "c", 
#              date_time = "T"
#            )
#   ) 
# 
# images_all %>%
#   select(binomial_1, binomial_2, binomial_3, 
#          count_1, count_2, id) %>%
#   drop_na(count_1) %>%
#   filter(count_1 > 0) %>%
#   filter(is.na(binomial_1)) 
# 
# images_all %>%
#   select(binomial_1, binomial_2, binomial_3,
#          count_1, count_2, id) %>%
#   drop_na(count_2) %>%
#   filter(count_2 > 0) %>%
#   filter(is.na(binomial_2))
# 
# images_all %>%
#   select(binomial_1, binomial_2, binomial_3, 
#          count_1, count_2, 
#          count_3, id) %>%
#   drop_na(count_3) %>%
#   filter(count_3 > 0) %>%
#   filter(is.na(binomial_3))

# ---------------------------------------------------------- -----
# DETECTIONS ----
#   images_wms  ----
# images_wms <- 
#   read_csv(here(path_out, 
#                 "images-wms_2020-2023_2023-11-06.csv"),
#            col_types = c(
#              image_id = "c", 
#              date = "D", 
#              time = "t", 
#              photo_type = "c", 
#              binomial_1 = "c", 
#              count_1 = "i", 
#              binomial_2 = "c", 
#              count_2 = "i", 
#              binomial_3 = "c", 
#              count_3 = "i", 
#              qc_certainty = "c", 
#              qc_by = "c", 
#              catalog_by = "c", 
#              good = "l",
#              review = "l",
#              error = "l", 
#              id = "c", 
#              image_n = "i", 
#              image_file = "c", 
#              date_time = "T"
#            )
#            ) 



# Create subsets for multiple species in images ----
list_columns_subset <- c("image_id", 
                         "date_time", 
                         "date", 
                         "time", 
                         "binomial", 
                         "count",
                         "image_n")

subset_1 <- 
  images_wms %>%
  rename(binomial = binomial_1, 
         count = count_1) %>%
  select(all_of(list_columns_subset)) %>%
  mutate(index_n = 1)

subset_2  <- 
  images_wms %>%
  rename(binomial = binomial_2, 
         count = count_2) %>%
  select(all_of(list_columns_subset)) %>%
  mutate(index_n = 2)

subset_3  <- 
  images_wms %>%
  rename(binomial = binomial_3, 
         count = count_3) %>%
  select(all_of(list_columns_subset)) %>%
  mutate(index_n = 3)

join_subsets <- 
  bind_rows(subset_1, 
            subset_2, 
            subset_3) %>%
  filter(binomial %in% list_binomial_wms) %>%
  mutate(camera = str_sub(image_id, 1, 4)) %>%
  unite(binomial_camera_date, 
        c(binomial, 
          camera,
          date), 
        remove = FALSE) %>%
  unite(binomial_camera_date_image,
        c(binomial, 
          camera,
          date, 
          image_n), 
        remove = FALSE) %>%
  relocate(image_id, 
           date, 
           time,
           binomial, 
           count, 
           index_n)
#

# [NOT RUN] ----
# gather_binomial_n <- 
#   images_wms %>%
#   select(image_id, 
#          date_time,
#          date, 
#          time, 
#         starts_with("binomial"), 
#         starts_with("count"), 
#         image_n) %>%
#   rowid_to_column("row") %>%
#   gather(column, value, c(count_1, count_2, count_3)) %>%
#   drop_na(value) %>%
#   mutate(value = str_pad(string = value, 
#                          width = 2, 
#                          side = "left", 
#                          pad = "0")) %>%
#   spread(column, value) %>%
#   unite(n_1, 
#         c(binomial_1, count_1),
#         na.rm = TRUE) %>%
#   unite(n_2, 
#         c(binomial_2, count_2), 
#         na.rm = TRUE) %>%
#   unite(n_3, 
#         c(binomial_3, count_3), 
#         na.rm = TRUE) %>%
#   gather(index_n, binomial_n, c(n_1, n_2, n_3)) %>%
#   mutate(index_n = as.integer(str_remove(index_n, "n_")))
# 
# locate_underscore <-
#   str_locate(gather_binomial_n$binomial_n,
#              "_")
# 
# add_locate_underscore <- 
#   gather_binomial_n %>%
#   bind_cols(locate_underscore = locate_underscore[,1]) %>%
#   mutate(binomial_to = locate_underscore - 1, 
#          count_from = locate_underscore + 1, 
#          count_to = nchar(binomial_n))
# 
# add_locate_underscore %>%
#   mutate(binomial = str_sub(binomial_n, 1, binomial_to), 
#          count = as.integer(str_sub(binomial_n, 
#                                     count_from, 
#                                     count_to))) %>%
#   select(image_id, 
#          date_time,
#          date, 
#          time, 
#          binomial, 
#          count, 
#          image_n, 
#          index_n, 
#          binomial_n) %>%
#   filter(is.na(count)) %>%
#   filter(binomial_n %nin% c("", "00")) %>%
#   distinct(index_n)

# Calculate delta time ----
delta_time <- 
  join_subsets %>%
  arrange(binomial_camera_date, 
          time) %>%
  mutate(delta_time_seconds = 
           case_when(
             binomial_camera_date ==
               lag(binomial_camera_date) ~ 
               as.numeric(time - lag(time)), 
             TRUE ~ 99999)) %>%
  group_by(binomial_camera_date) %>%
  mutate(
    # Identify start of new burst
    # 1800 seconds = 30 minutes
    new_day =
      case_when(
        delta_time_seconds > 1800 ~ 1, 
        TRUE ~ 0), 
    # Label images by burst
    new_day_n = 
      cumsum(new_day)) %>%
  ungroup()  %>%
  unite(detection_event_n, 
        c(binomial_camera_date, 
          new_day_n), 
        remove = FALSE) %>%
  select(binomial_camera_date_image,
         detection_event_n,
         new_day,
         new_day_n,
         delta_time_seconds) %>%
  right_join(join_subsets, 
             "binomial_camera_date_image")

# Subset to distinct detection events ----
images_wms_detections_30m <- 
  delta_time %>% 
  group_by(camera, 
           binomial, 
           detection_event_n) %>%
  summarize(date_min = as_date(min(date_time, na.rm = TRUE)), 
            time_min = hms::as_hms(min(date_time, na.rm = TRUE)), 
            count_from = min(count, na.rm = TRUE),
            count_to = max(count, na.rm = TRUE), 
            image_from = min(image_n, na.rm = TRUE), 
            image_to = max(image_n, na.rm = TRUE)) %>% 
  ungroup() %>%
  relocate(binomial,
         camera, 
         date_min, 
         time_min, 
         starts_with("count"), 
         starts_with("image"))

# images_wms_detections_30m %>%
#   write_csv(here(path_out, 
#                paste0("images-wms_detections_30m_2020-2023_", 
#                       Sys.Date(), 
#                       ".csv")
# ))
#   recordTable ----
recordTable <- 
  delta_time %>%
  select(Station = camera, 
         Species = binomial, 
         DateTimeOriginal = date_time, 
         Date = date, 
         Time = time, 
         delta.time.secs = delta_time_seconds)
#
# data(recordTableSample)        
# Station	the station the image is from
# Species	species name
# DateTimeOriginal	Date and time of record in R-readable format
# Date	record date
# Time	record time of day
# delta.time.secs	time difference between record and last (independent) record of same species at same station / camera* (in seconds)
# delta.time.mins	time difference between record and last (independent) record of same species at same station / camera* (in minutes)
# delta.time.hours	time difference between record and last (independent) record of same species at same station / camera* (in hours)
# delta.time.days	time difference between record and last (independent) record of same species at same station / camera* (in days)
# Directory	directory the image is in
# FileName	image file name
#   recordTable_30m ----
recordTable_30m <- 
  recordTable %>%
  filter(delta.time.secs > 1800)
# ---------------------------------------------------------- -----
# CAMERA OPERATION ----
# Identify error dates ----
error_dates <- 
  dlog %>%
  drop_na(date_excl_from) %>%
  distinct(camera, 
           date_excl_from, 
           date_excl_to) %>%
  group_by(camera) %>%
  mutate(n = as.character(1:n())) %>%
  gather(column, 
         value, 
         c(date_excl_from, 
           date_excl_to)) %>%
  mutate(column = 
           str_replace(column, 
                       "date_excl", 
                       paste0("Problem", n))) %>%
  select(-n) %>%
  spread(column, value) %>%
  relocate(camera, 
           Problem1_from,
           Problem1_to,
           Problem2_from,
           Problem2_to,
           Problem3_from,
           Problem3_to,
           Problem4_from,
           Problem4_to,
           Problem5_from,
           Problem5_to,
           Problem6_from,
           Problem6_to,
           Problem7_from,
           Problem7_to,
           Problem8_from,
           Problem8_to,
           Problem9_from,
           Problem9_to)

#   cameraTable  ----
cameraTable <- 
  dlog %>%
  group_by(camera) %>%
  summarize(Setup_date = min(date_from), 
            Retrieval_date = max(date_to)) %>%
  ungroup() %>%
  left_join(error_dates, 
            "camera") %>%
  rename(Station = camera) %>%
  data.frame() 

#   camop_problem  ----
# Create camera operation history 
camop_problem <- 
  cameraOperation(CTtable      = cameraTable,
                  stationCol   = "Station",
                  setupCol     = "Setup_date",
                  retrievalCol = "Retrieval_date",
                  writecsv     = FALSE,
                  hasProblems  = TRUE,
                  dateFormat   = "ymd"
)
#
# Confirm contents of data.frame 
# as_tibble(camop_problem,
#           rownames = NA) %>%
#   rownames_to_column("Station") %>%
#   select(1, 2, 1157)
# 
# Plot the camera operation matrix ----
# camtrapR:::camopPlot(camOp = camop_problem)
# Add scale bar on right of plot
camtrapR:::camopPlot(camOp = camop_problem, lattice = TRUE)
# 
# Notes ----
# Saving and loading camera operation matrices
# The camera operation matrix can easily be saved as a csv file (by setting argument writecsv = TRUE, check.names = FALSE and defining outdir). In order to load the csv into R again, it is necessary to tell R to use the station IDs (the first column) as row names:
#   
#   camOp <- read.csv(file = ..., row.names = 1, check.names = FALSE)
# check.names = FALSE ensures that column names (the dates) are read back into R as they are (e.g. “2015-12-01”). Otherwise one may end up with unreadable column names (at least for camtrapR) such as “X2015.12.01”.
# 

# ---------------------------------------------------------- -----
# Create summary report ----
# surveyReport creates a summary report containing:
#   
# number of stations (total and operational)
# number of active trap days (total and by station)
# number of days with cameras set up (operational or not; total and by station)
# number of active trap days (taking into account multiple cameras accumulating effort independently at the same station)
# total trapping period
# camera trap and record date ranges
# number of species by station
# number of independent events by species
# number of stations at which species were recorded
# number of independent events by station and species
# It requires a record table, the camera trap table, and (since version 2.1) a camera operation matrix.
# 
# The camera operation matrix is required to provide more precise and flexible calculation of the number of active trap days. So we first create the camera operation matrix, here taking into account periods in which the cameras malfunctioned (hasProblems = TRUE).
summary_report <- 
  surveyReport (recordTable          = recordTable,
                CTtable              = cameraTable,
                camOp                = camop_problem,    
                speciesCol           = "Species",
                stationCol           = "Station",
                setupCol             = "Setup_date",
                retrievalCol         = "Retrieval_date",
                CTDateFormat         = "ymd", 
                recordDateTimeCol    = "DateTimeOriginal",
                recordDateTimeFormat = "ymd HMS", 
                sinkpath             = path_out, 
                makezip              = FALSE
)
# ---------------------------------------------------------- -----
# Detections (30m) ----
detections_30m <- 
  recordTable_30m %>%
  clean_names() %>%
  mutate(year = year(date)) %>%
  filter(year %nin% c(2016, 2023, 2024)) 

detections_30m_year <-
  detections_30m %>%
  group_by(year) %>%
  summarize(total = n()) 

detections_30m_year_species <-
  detections_30m %>%
  group_by(year, species) %>%
  summarize(total = n()) %>%
  spread(year, total)

detections_30m_year_species %>%
  write_csv(here(path_out,
                 paste0(index_site, 
                        "_images-wms_detections_30m_year_species_",
                        Sys.Date(),
                        ".csv")), 
                 na = "0"
  )


# detections_30m_year_deer <- 
#   detections_30m %>%
#   group_by(year, species) %>%
#   count() %>%
#   left_join(detections_30m_year, "year") %>%
#   filter(species %in% c("Odocoileus hemionus")) %>%
#   mutate(percent = n/total) %>%
#   ungroup()
# 
# detections_30m_year_deer %>%
#   summarize(mean = mean(percent), 
#             min = min(percent), 
#             max = max(percent), 
#             year_min = min(year), 
#             year_max = max(year))
# 
# detections_30m %>%
#   filter(species %in% c("Odocoileus hemionus")) %>%
#   group_by(year, station) %>%
#   count() %>%
#   spread(station, n)
# ---------------------------------------------------------- -----
# Create detection matrix for occupancy (single species) ----

# make detection history (with trapping effort)
DetHist2 <- 
  detectionHistory(recordTable          = recordTable,
                   camOp                = camop_problem,
                   stationCol           = "Station",
                   speciesCol           = "Species",
                   recordDateTimeCol    = "DateTimeOriginal",
                   species              = "Lynx rufus",
                   timeZone             = "America/Los_Angeles",
                   occasionLength       = 7,
                   # minActiveDaysPerOccasion = 3, 
                   day1                 = "station",
                   output               = "count", # or "binary"
                   includeEffort        = TRUE,
                   scaleEffort          = FALSE
  )
# Notes -----
# recordTable
# data.frame. the record table created by recordTable
# 
# species	
# character. the species for which to compute the detection history
# 
# camOp	
# The camera operability matrix as created by cameraOperation
# 
# output	
# character. Return binary detections ("binary") or counts of detections ("count")
# 
# stationCol	
# character. name of the column specifying Station ID in recordTable
# 
# speciesCol	
# character. name of the column specifying species in recordTable
# 
# recordDateTimeCol	
# character. name of the column specifying date and time in recordTable
# 
# recordDateTimeFormat	
# character. Format of column recordDateTimeCol in recordTable
# 
# occasionLength	
# integer. occasion length in days
# 
# minActiveDaysPerOccasion	
# integer. minimum number of active trap days for occasions to be included (optional)
# 
# maxNumberDays	
# integer. maximum number of trap days per station (optional)
# 
# day1	
# character. When should occasions begin: station setup date ("station"), first day of survey ("survey"), a specific date (e.g. "2015-12-31")?
#   
#   buffer	
# integer. Makes the first occasion begin a number of days after station setup. (optional)
# 
# includeEffort	
# logical. Compute trapping effort (number of active camera trap days per station and occasion)?
#   
#   scaleEffort	
# logical. scale and center effort matrix to mean = 0 and sd = 1?
#   
#   occasionStartTime	
# (DEPRECATED) integer. time of day (the full hour) at which to begin occasions. Please use argument occasionStartTime in cameraOperation instead.
# 
# datesAsOccasionNames	
# If day1 = "survey", occasion names in the detection history will be composed of first and last day of that occasion.
# 
# timeZone	
# character. Must be a value returned by OlsonNames
# 
# writecsv	
# logical. Should the detection history be saved as a .csv?
#   
#   outDir	
# character. Directory into which detection history .csv file is saved
# 
# unmarkedMultFrameInput	
# logical. Return input for multi-season occupancy models in unmarked (argument "y" in unmarkedMultFrame?


# ========================================================== -----
# README ----
# author: Morgan Gray
# created: 2023-07-26
#
# Purpose:  
# 1. Collate image tables for analysis
