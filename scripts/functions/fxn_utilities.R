# updated: 2024-04-01 ----
# ========================================================== -----
# Load libraries ----
# File management ---
library(exiftoolr)   ## To work with exif details from jpg files
library(fs)   ## To manage directories
library(readxl)   ## To read xlsx files
library(janitor)   ## To clean data tables
library(lubridate)   ## To work with dates and times
library(stringr)   ## To wrangle character variables
library(openxlsx)   ## To write xlsx files
library(here)
library(tidyverse)
library(plotrix)   ## To get color names from hex codes
library(hms)  ## For working with times
library(testthat)  ## For data checks
# library(magrittr)
# library(tidyr)
#
# Define basic functions ----
#   %nin%: Negate %in% 
"%nin%" <- Negate("%in%")
#   substrRight: Subset character string from the right 
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
is_all_numeric <- function(col) {
  return(all(!is.na(as.numeric(col))))
}
# Define file paths ----
# For R scripts and work
path_r <- here()
path_in <- here("input")

# Create helpers ----
#   For image tables
# Define exif columns to read  
# Names match EXIF format
list_exif_tags <- c("FileName",
                    "Directory",
                    "DateTimeOriginal",
                    "FileSize",
                    "Make",
                    "Model",
                    "FocalLength",
                    "ImageWidth",
                    "ImageHeight")

# column names in image table (21 columns) 
list_column_names <- 
  read_csv(here(path_in,
                "image-table_column-info.csv"), 
           col_types = cols()) %>%
  arrange(sheet, column_order) %>%
  filter(sheet %in% "images") %>%
  pull(column_name)

lookup_qc_initials <- 
  read_csv(here(path_in,
                "staff-names.csv"), 
           col_types = cols()) %>%
  drop_na(qc_initials) %>%
  select(qc_by = name, 
         qc_initials, 
         qc_initials_lower, 
         qc_initials_sentence,
         sort) 

lookup_certainty <- 
  tibble(orig_certainty = c("Absolutely sure",
                            "Pretty sure",
                            "Pretty Sure",
                            "absolutely sure",
                            "Prettys sure",
                            "Absolutely Sure",
                            "Absolutely"), 
         
         qc_certainty = c("Absolutely sure",
                          "Pretty sure",
                          "Pretty sure",
                          "Absolutely sure",
                          "Pretty sure",
                          "Absolutely sure",
                          "Absolutely sure"))


# Define camera project ----
fxn_define_camera_project <- function(index_site){
  
  # Project attributes ----
  # Define server location
  
  path_server <- "K:"
  
  if(index_site == "FOR"){
    
    index_project <<- "FOR Project"
    path_site <<- file.path(path_server, index_project)
    file_name_dlog <<- "for_deployments.xlsx"
    template_metadata <<- 
      read_csv(here(path_in, 
                    "image-table_metadata_for.csv"), 
               col_types = cols()) %>%
      as.data.frame()
    path_ilog <<- file.path(path_server, "FOR - Cataloguing Inventory_Updated.xlsx")
    
    path_jpg_l <<- "L:/FOR Project/images"
    
  }else if(index_site == "legacy-data"){
    
    index_project <<- "WPI Project"
    path_site <<- file.path(path_server, index_project, "legacy-data_PWD")
    file_name_dlog <<- "wpi_deployments_all_2013-2016.xlsx"
    template_metadata <<- 
      read_csv(here(path_in, 
                    "image-table_metadata_wpi.csv"), 
               col_types = cols()) %>%
      as.data.frame() 
    
    path_jpg_l <<- "L:/WPI Project/PWD/2013-2016/Images"
    
  }else if(index_site == "DEN"){
    
    index_project <<- "DEN Project"
    path_site <<- file.path(path_server, index_project)
    file_name_dlog <<- "den_deployments.xlsx"
    template_metadata <<- 
      read_csv(here(path_in, 
                    "image-table_metadata_wpi.csv"), 
               col_types = cols()) %>%
      as.data.frame()
    path_ilog <<- file.path(path_server, "DEN - Cataloguing Inventory.xlsx")
    
    
  }else{
    
    index_project <<- "WPI Project"
    path_site <<- file.path(path_server, index_project, index_site)
    file_name_dlog <<- paste0("wpi_deployments_", index_site, ".xlsx")
    template_metadata <<- 
      read_csv(here(path_in, 
                    "image-table_metadata_wpi.csv"), 
               col_types = cols()) %>%
      as.data.frame()
    path_ilog <<- file.path(path_server, "WPI - Cataloguing Inventory_Updated.xlsx")
    
    path_jpg_l <<- paste0("L:/WPI Project/", index_site)
    
  }
  
  # File paths ----
  # For summary tables 
  path_out <<- here("output", index_site)
  
  # For data tables 
  path_data <<- here(path_site, "data")
  #
  # For exif tables
  path_exif <<- here(path_data, "exif-tables")
  path_exif_archive <<- here(path_exif, "z_archive")
  #
  # For image tables
  path_table_blank <<- here(path_data, "image-tables_blank")
  path_table_catalog <<- here(path_data, "image-tables_catalog")
  path_table_qc <<- here(path_data, "image-tables_qc")
  
  path_table_archive <<- here(path_data,  "image-tables", "z_archive")
  path_table_blank_archive <<- here(path_table_blank, "z_archive")
  path_table_catalog_archive <<- here(path_table_catalog, "z_archive")
  path_table_qc_archive <<- here(path_table_qc, "z_archive")
  path_table_tidy_archive <<- here(path_table_qc_archive, "image-tables_final_tidy")
  #
  # For image files
  path_jpg <<- here(path_site, "images")
  #
  # For finalized image tables 
  path_vault <<- here(path_site, "vault")
  
  # Deployment & intern logs  ----
  # Function to read dlog with column formats
  fxn_dlog_get <- function(index_path){
    
    # Define is.Date function
    is.Date <- function(x) {
      inherits(x, c("Date", "POSIXt"))}
    
    # Read xlsx and define column types
    preview <-
      suppressWarnings(
        read_excel(here(index_path),
                   sheet = "deployments")
      )
    names <- colnames(preview)
    names_date <- names %>%
      as_tibble() %>%
      filter(substr(value, 1, 4) == "date") %>%
      pull(value)
    names_n <- names %>%
      as_tibble() %>%
      filter(substr(value, 1, 2) == "n_") %>%
      pull(value)
    data_types <- vector()
    
    for(i in 1 : length(names)){
      
      #where date_ is in a column name
      if(names[i] %in% names_date){
        data_types[i] <- "date"
        
        #where n_ is in a column name
      } else if (names[i] %in% names_n) {
        data_types[i] <- "numeric"
        
      } else{
        data_types[i] <- "text"
      }
    }
    
    # Reread xlsx file and assign column types
    tidy <-
      suppressWarnings(
        read_excel(here(index_path),
                   sheet = "deployments",
                   col_types = data_types)
      )%>%
      mutate(across(where(is.Date), as_date)) 
    
    
  }
  
  # Get logs
  dlog <<-
    fxn_dlog_get(here(path_site, 
                      file_name_dlog)) %>%
    tidyr::drop_na(id)  
  # tidyr::drop_na(n_survey) %>%
  # filter(n_survey %nin% "N/A")
  
  if(str_detect(index_site, "legacy-data") == FALSE){
    ilog <<-
      read_excel(here(path_ilog),
                 sheet = index_site) %>%
      select(id,
             drive_img,
             done_blank,
             done_catalog,
             done_tidy,
             done_qc, 
             done_vault)
  }
  
  
  # Species & photo_type ----
  list_photo_type <<- c("Animal", 
                       "Blank", 
                       "Start", 
                       "End", 
                       "Setup", 
                       "Pickup",
                       "Staff",
                       "Unidentifiable", 
                       "Needs ID",
                       "Corrupt")
  #
  list_photo_type_maintenance <<- c("Start", 
                                   "End", 
                                   "Setup", 
                                   "Pickup",
                                   "Staff",
                                   "Corrupt")
  
  if(index_site == "DEN"){
    
    species_table_den <<- 
      read_excel(here(path_in, 
                      "binomial-crosswalk.xlsx"), 
                 sheet = "photo-type_binomial_DEN")

    #
    list_binomial <<- 
      species_table_den %>%
      filter(binomial %nin% "N/A") %>%
      distinct(binomial) %>%
      pull()
    #
    # Create list of binomial with count
    list_binomial_count <<- 
      list_binomial[list_binomial %nin%
                      c("Needs ID")]
    #
    #   lookup_binomial: known orig_binomial (for binomial_2) 
    lookup_binomial <<- 
      species_table_den %>%
      distinct(orig_binomial, 
               binomial,
               comments_rev,
               error_message)
    
    
    expand_grid_photo_type_maintenance <<- 
      tibble(expand.grid(
        photo_type = c(list_photo_type_maintenance,
                       "Unknown"), 
        binomial = list_binomial)
      ) %>%
      unite(photo_type_binomial, 
            c("photo_type", 
              "binomial"), 
            remove = FALSE,
            sep = "_") %>%
      mutate(error_message =
               case_when(
                 photo_type %in% "Corrupt" ~ 
                   "IGNORE: image corrupted")
      )
    
    lookup_photo_type_binomial <<- 
      species_table_den %>%
      select(photo_type_binomial,
             photo_type,
             photo_type_qc,
             binomial,
             comments_rev,
             error_message) %>%
      bind_rows(expand_grid_photo_type_maintenance) %>%
      mutate(photo_type_qc = ifelse(is.na(photo_type_qc), 
                                    photo_type, 
                                    photo_type_qc))
    
    
  }else{
    
    species_attributes <<- 
      read_csv(here(path_in, 
                    "attributes_species.csv"), 
               col_types = cols()) %>%
      arrange(binomial) %>%
      select(binomial, 
             spp_cn,  
             diet_type, 
             is_special_species, 
             is_wms)
    #
    list_binomial <<- 
      c(unique(species_attributes$binomial), 
        "Needs ID")
    #
    list_binomial_special <<-
      species_attributes %>%
      filter(is_special_species == TRUE) %>%
      pull(binomial)
    #
    list_binomial_wms <<- 
      species_attributes %>%
      filter(is_wms == TRUE) %>%
      pull(binomial) 
    #
    # Create list of binomial with count
    list_binomial_count <<- 
      list_binomial[list_binomial %nin%
                      c("Bos taurus", 
                        "Needs ID",
                        "Unidentifiable")]
    #
    #   lookup_binomial: known orig_binomial (for binomial_2) 
    lookup_binomial <<- 
      read_excel(here(path_in, 
                      "binomial-crosswalk.xlsx"), 
                 sheet = "binomial") %>%
      distinct(orig_binomial, 
               binomial,
               comments_rev,
               error_message)
    # 
    #   Photo_type x binomial combinations  
    expand_grid_photo_type_maintenance <<- 
      tibble(expand.grid(
        photo_type = c(list_photo_type_maintenance,
                       "Unknown"), 
        binomial = list_binomial)
      ) %>%
      unite(photo_type_binomial, 
            c("photo_type", 
              "binomial"), 
            remove = FALSE,
            sep = "_") %>%
      mutate(error_message =
               case_when(
                 binomial %in% list_binomial_special ~ 
                   "CONFIRM ID: special species", 
                 photo_type %in% "Corrupt" ~ 
                   "IGNORE: image corrupted")
      )
    
    lookup_photo_type_binomial <<- 
      read_excel(here(path_in, 
                      "binomial-crosswalk.xlsx"), 
                 sheet = "photo-type_binomial") %>%
      select(photo_type_binomial,
             photo_type,
             photo_type_qc,
             binomial,
             comments_rev,
             error_message) %>%
      bind_rows(expand_grid_photo_type_maintenance) %>%
      mutate(photo_type_qc = ifelse(is.na(photo_type_qc), 
                                    photo_type, 
                                    photo_type_qc))

  }
  
  list_photo_type_binomial <<-
    unique(lookup_photo_type_binomial$photo_type_binomial)
 
  # Other attributes ----
  list_cameras <<- unique(dlog$camera)
  
  validation_lists <<- 
    create_validation_lists(index_site)  
}
# File management ----
#   fxn_table_check_dlog ----
# index_data <- exif_all
# index_data <- tables_all
fxn_table_check_dlog <- function(index_data, 
                                 index_type) {
  
  # Create dlog subset ----
  dlog_select <-
    dlog %>%
    filter(has_data == TRUE,
           done_exif == TRUE) %>%
    select(id,
           year_to,
           done_blank, 
           done_catalog,
           date_from,
           date_to,
           date_excl_from,
           date_excl_to,
           n_img_to)
  
  if(index_type == "exif"){
    dlog_select_subset <- 
      dlog_select %>%
      filter(done_blank == FALSE)
  }

  if(index_type == "blank"){
    dlog_select_subset <- 
      dlog_select %>%
      filter(done_blank == TRUE, 
             done_catalog == FALSE)
  }
 
  # Summarize by 'id' and calculate statistics ----
  summarized_data <- 
    index_data %>%
    group_by(id) %>%
    summarize(
      q_date_img_from = min(date, na.rm = TRUE),
      q_date_img_to = max(date, na.rm = TRUE),
      q_n_img = n_distinct(image_n, na.rm = TRUE),
      q_n_img_from = min(image_n, na.rm = TRUE),
      q_n_img_to = max(image_n, na.rm = TRUE),
      err_date_na = is.na(date),
      err_img_na = is.na(image_n)
    ) %>%
    distinct() %>%
    ungroup()
  
  # Join with dlog_subset and calculate date and image number errors ----
  checked_data <- 
    summarized_data %>%
    left_join(dlog_select_subset, "id") %>%
    rowwise() %>%
    mutate(
      
      # Compare image_n and time interval
      err_img_n = n_img_to != q_n_img,
      date_min = min(q_date_img_from, q_date_img_to, na.rm = TRUE),
      err_interval = !(date_min >= date_from & date_min <= date_to),
      err_img_n_from = q_n_img_from != 1,
      
      # Compare date with date_img
      # Flags when differnce between survey and image dates
      err_date_from = date_from - q_date_img_from != 0, 
      err_date_to = date_to - q_date_img_to != 0, 
      
      diff_date_excl_from_img_to = as.numeric(date_excl_from - q_date_img_to),
      diff_date_excl_to_date_to = as.numeric(date_excl_to - date_to),
      diff_date_excl_to_img_from = as.numeric(date_excl_to - q_date_img_from),
      diff_date_excl_from_date_from = as.numeric(date_excl_from - date_from),
      
      ends_early = 
        case_when(
          diff_date_excl_from_img_to == 1 & 
            diff_date_excl_to_date_to == 0 &
            date_from == q_date_img_from ~ TRUE, 
          TRUE ~ FALSE
        ),
      starts_late = 
        case_when(
          diff_date_excl_to_img_from == -1 & 
            diff_date_excl_from_date_from == 0 &
            date_to == q_date_img_to ~ TRUE, 
          TRUE ~ FALSE
        ), 
      err_date_from = ifelse(err_date_from == TRUE &
                               starts_late == TRUE,
                             FALSE, err_date_from), 
      err_date_to = ifelse(err_date_to == TRUE &
                             ends_early == TRUE,
                           FALSE, err_date_to))
        
     
  # Rearrange columns ----
  final_data <- 
    checked_data %>%
    rowwise() %>%
    mutate(has_error = any(c_across(starts_with("err")) == TRUE)) %>%
    ungroup() %>%
    arrange(desc(has_error), 
            id) %>%
    relocate(
      id, 
      has_error, 
      err_date_na, 
      err_date_from, 
      err_date_to, 
      err_img_na, 
      err_img_n, 
      err_img_n_from, 
      err_interval, 
      ends_early, 
      starts_late,
      date_from,
      date_to,
      date_excl_from,
      date_excl_to,
      q_date_img_from,
      q_date_img_to,
      starts_with("diff"),
      q_n_img_from,
      n_img_to,
      q_n_img_to,
      q_n_img,
      date_min,
      year_to) %>%
   
    # Replace FALSE with . for easier visual review
    mutate(across(where(is.logical), 
                  ~ifelse(. == FALSE, ".", 
                          as.character(.))))
  
  return(final_data)
}

#   fxn_write_csv_archive_old ----
fxn_archive_old_csv <- function(index_file_name){
  
  path_file_name <- here(path_out, index_file_name)
  
  # Check for file, archive if exists
  if(file.exists(path_file_name)){
    
    # Create file name with Sys.Date() to archive
    file_name_with_date <- 
      str_replace(index_file_name, 
                  ".csv", 
                  paste0("_", 
                         Sys.Date(), 
                         ".csv"))
    
    # Archive existing file
    file_move(path = path_file_name, 
              new_path = here(path_out, "z_archive", file_name_with_date))
    
  }
}

# ========================================================== -----
