# ========================================================== -----
# Define site and attributes ----
index_site = "PWD"

fxn_define_camera_project(index_site)

path_out_wi_migration <- here("output/wi-migration")

dlog_wi <- 
  dlog %>%
  filter(migrate_wi %in% c("TRUE", "MAYBE")) %>%
  arrange(id)
# ========================================================== -----
# Define functions -----
# To create metadata output ----
#   fxn_create_metadata_deployment ----
fxn_create_metadata_deployment <- function(){
  
  # Define camera coordinates
  camera_lon_lat <- read_xlsx(here(path_in,
                                   "attributes_cameras.xlsx"), 
                              sheet = "attributes_cameras") %>%
    filter(grid == "P") %>%
    select(camera, lon_x, lat_y)
  
  # Define column names 
  list_columns_deployment <- c("project_id",
                               "deployment_id",
                               "subproject_name",
                               "subproject_design",
                               "placename",
                               "longitude",
                               "latitude",
                               "start_date",
                               "end_date",
                               "event_name",
                               "event_description",
                               "event_type",
                               "bait_type",
                               "bait_description",
                               "feature_type",
                               "feature_type_methodology",
                               "camera_id",
                               "quiet_period",
                               "camera_functioning",
                               "sensor_height",
                               "height_other",
                               "sensor_orientation",
                               "orientation_other",
                               "recorded_by",
                               "plot_treatment",
                               "plot_treatment_description",
                               "detection_distance")
  
  # Create deployment metadata
  deployments <- 
    dlog_wi %>%
    filter(migrate_wi == TRUE) %>%
    left_join(camera_lon_lat, "camera") %>%
    select(deployment_id = id,
           placename = camera, 
           longitude = lon_x, 
           latitude = lat_y,
           start_date = date_from, 
           end_date = date_to, 
           camera_id = serial_number, 
           camera_functioning = cam_fail_details) %>%
    mutate(
      project_id = "Pepperwood",
      subproject_name = "None",
      subproject_design = "None",
      event_name = "None",
      event_description = "None",
      event_type = "None",
      bait_type = "None",
      bait_description = "None",
      feature_type = "None",
      feature_type_methodology = "None",
      quiet_period = 5,
      sensor_height = "Knee height",
      height_other = "None",
      sensor_orientation = "Parallel",
      orientation_other = "None",
      recorded_by = "None",
      plot_treatment = "None",
      plot_treatment_description = "None",
      detection_distance = "None"
    ) %>%
    select(all_of(list_columns_deployment))
  
}

#   fxn_create_metadata_project -----
fxn_create_metadata_project <- function(){
  
  project <- 
    tibble(project_name = "Pepperwood Wildlife Camera Grid",
           project_id = "Pepperwood",
           project_short_name = "Pepperwood",
           project_objectives = "To establish a long-term monitoring program for terrestrial wildlife species at Pepperwood Preserve, CA using a 20-camera grid to collect observational data.",
           project_species = "Multiple",
           project_species_individual = "None",
           project_sensor_layout = "Systematic",
           project_sensor_layout_targeted_type = "None",
           project_bait_use = "No",
           project_bait_type = "None",
           project_stratification = "Yes",
           project_stratification_type = "Distance",
           project_sensor_method = "Sensor Detection",
           project_individual_animals = "No",
           project_blank_images = "No",
           project_sensor_cluster = "No",
           project_admin = "Pepperwood Foundation",
           project_admin_email = "research@pepperwoodpreserve.org",
           country_code = "USA",
           embargo = 48,
           metadata_license = "CC-BY 4.0",
           image_license = "CC-BY 4.0",
           project_type = "Image")
  
  return(project)
}
#   fxn_create_metadata_camera ----
fxn_create_metadata_camera <- function(){
  
  list_columns_camera <- c("project_id",
                           "camera_id",
                           "make",
                           "model",
                           "serial_number",
                           "year_purchased")
  
  camera_inventory <- 
    read_xlsx(here(path_site, 
                   "pwd_serial-numbers.xlsx"), 
              sheet = "camera inventory") %>%
    select(serial_number, 
           make, 
           model, 
           year)
  
  camera <- 
    dlog_wi %>%
    filter(migrate_wi == TRUE) %>%
    select(id, serial_number) %>%
    
    left_join(camera_inventory, "serial_number") %>%
    mutate(project_id = "Pepperwood", 
           camera_id = serial_number) %>%
    rename(deployment_id = id,  
           year_purchased = year) %>%
    select(all_of(list_columns_camera), 
           deployment_id)
  
  return(camera)
}

#   fxn_create_metadata_image ----
# index_type = "archive"
fxn_create_metadata_image <- function(index_type){
  
  list_columns_image <- c("project_id",
                          "deployment_id",
                          "image_id",
                          "location",
                          "identified_by",
                          "wi_taxon_id",
                          "class",
                          "order",
                          "family",
                          "genus",
                          "species",
                          "common_name",
                          "uncertainty",
                          "timestamp",
                          "number_of_objects",
                          "highlighted",
                          "age",
                          "sex",
                          "animal_recognizable",
                          "individual_id",
                          "individual_animal_notes",
                          "markings",
                          "external_sequence_id",
                          "sequence_start_time")
  
  all_vault <<- fxn_collate_all_vault(index_type = index_type)  
  
  tidy_vault_long <- 
    all_vault %>%
    filter(id %in% unique(dlog_wi$id)) %>%
    fxn_create_binomial_count_long() %>%
    fxn_correct_typos() %>%
    fxn_tidy_comments() %>%
    fxn_add_taxonomic_attributes() %>%
    fxn_create_identified_by()
  
  image <- 
    tidy_vault_long %>%
    # Add attributes from all_vault 
    left_join(all_vault %>%
                select(image_id, 
                       good, 
                       date_time, 
                       image_file), "image_id") %>%
    
    mutate(highlighted = ifelse(good == TRUE, 1, 0)) %>%
    
    rename(deployment_id = id, 
           location = image_file, 
           uncertainty = qc_certainty, 
           timestamp = date_time, 
           number_of_objects = count, 
           individual_animal_notes = comments) %>%
    mutate(project_id = "Pepperwood", 
           age = "Unknown", 
           sex = "Unknown", 
           animal_recognizable = "No", 
           individual_id = "None", 
           markings = "None", 
           external_sequence_id = "None", 
           sequence_start_time = "None") %>%
    select(all_of(list_columns_image)) 
  # %>%
  #   select(-good, 
  #          -column_n)
  
  return(image)
  
}

# ---------------------------------------------------------- -----
# Helpers for fxn_create_metadata_image ----
#   read_xlsx_images_sheet ----
# Function to read the image sheet of each xlsx file 
read_xlsx_images_sheet <- function(file) {
  
  data <- 
    read_xlsx(file, 
              sheet = "Images",
              col_types = "text") %>%
    mutate(image_id = as.character(image_id),
           photo_type = as.character(photo_type),
           binomial_1 = as.character(binomial_1),
           count_1 = as.numeric(count_1),
           comments = as.character(comments),
           catalog_by = as.character(catalog_by),
           review = as.character(review),
           good = as.character(good),
           error = as.character(error),
           qc_by = as.character(qc_by),
           qc_certainty = as.character(qc_certainty),
           binomial_2 = as.character(binomial_2),
           count_2 = as.numeric(count_2),
           binomial_3 = as.character(binomial_3),
           count_3 = as.numeric(count_3),
           id = as.character(id),
           image_n = as.numeric(image_n),
           image_file = as.character(image_file))
  
  # For the 'date_time' column
  if (grepl("^\\d+\\.\\d+$", data$date_time[1])) {
    data$date_time <- 
      as.POSIXct(as.numeric(data$date_time) * 86400,
                 origin = "1899-12-30")
  } else {
    data$date_time <- as.POSIXct(data$date_time)
  }
  
  data$date <- as_date(data$date_time)
  # # For the 'date' column
  # if (is_all_numeric(data$date)) {
  #   data$date <- as.Date(as.numeric(data$date),
  #                        origin = "1899-12-30")
  # } else {
  #   data$date <- as.Date(data$date)
  # }
  
  data$time <- as_hms(strptime(data$time, 
                               format = "%H:%M:%S"), 
                      on_fail = NA)
  
  if (exists("data")) {
    cat(file, "\n")
  } else {
    message(paste0("Error with file ", file))
  }
  return(data)
}

#   fxn_collate_all_vault ----
fxn_collate_all_vault <- function(index_type){
  
  if(index_type == "new"){
    # Get the list of all files in the vault
    vault_files <-
      tibble(path = list.files(path = path_vault, pattern = "\\.xlsx$", full.names = TRUE)) %>%
      mutate(file_index = tools::file_path_sans_ext(basename(path)),
             id = str_sub(file_index, 1, 11))
    
    # Read and bind all files into one data tibble
    all_vault_init <- unique(vault_files$path) %>%
      map_dfr(read_xlsx_images_sheet)
    
    #   Tidy values
    all_vault <-
      all_vault_init %>%
      rename(orig_certainty = qc_certainty) %>%
      left_join(lookup_certainty, "orig_certainty") %>%
      mutate(catalog_by = ifelse(catalog_by == "Vf", "Viviane Fuller", catalog_by),
             qc_by = ifelse(qc_by == "Vf", "Viviane Fuller", qc_by),
             photo_type = ifelse(photo_type == "Needs ID", "Unidentifiable", photo_type),
             binomial_1 = ifelse(binomial_1 == "Needs ID", "Unidentifiable", binomial_1),
             binomial_2 = ifelse(binomial_2 == "Needs ID", "Unidentifiable", binomial_2),
             binomial_3 = ifelse(binomial_3 == "Needs ID", "Unidentifiable", binomial_3)) %>%
      mutate(
        
        photo_type =
          case_when(
            photo_type == "Unidentifiable" & binomial_1 %in% c("Rodent species",
                                                               "Sciurus griseus",
                                                               "Urocyon cinereoargenteus",
                                                               "Mephitis mephitis") ~ "Animal",
            photo_type == "Animal" & binomial_1 == "Unidentifiable" ~ "Unidentifiable",
            TRUE ~ photo_type),
        
        binomial_1 =
          case_when(
            photo_type == "Unidentifiable" & binomial_1 == "Unidentifiable" ~ "N/A",
            photo_type == "Animal" & binomial_1 == "Unidentifiable" ~ "N/A",
            TRUE ~ binomial_1)
      ) %>%
      select(-orig_certainty)
    
    all_vault %>%
      write_csv(here(path_out_wi_migration, "all-vault.csv"))
    
  }
  if(index_type == "archive"){
    all_vault <- read_csv(here(path_out_wi_migration, "all-vault.csv"),
                          col_types = cols(
                            .default = col_character(),
                            date = col_date(format = ""),
                            time = col_time(format = ""),
                            count_1 = col_double(),
                            review = col_logical(),
                            good = col_logical(),
                            error = col_logical(),
                            binomial_2 = col_character(),
                            count_2 = col_double(),
                            binomial_3 = col_character(),
                            count_3 = col_double(),
                            image_n = col_double(),
                            date_time = col_datetime(format = "")
                          ))
  }

  return(all_vault)
}
#   fxn_create_binomial_count_long ----
fxn_create_binomial_count_long <- function(index_data){
  
  # Collate binomial columns in long format----
  binomial_long <- 
    index_data %>%
    select(image_id, 
           photo_type,
           starts_with("binomial")) %>%
    gather(column_n, binomial, binomial_1:binomial_3) %>%
    drop_na(binomial) %>%
    mutate(column_n = str_remove_all(column_n, "binomial_")) %>%
    unite(image_id_n, 
          c(image_id, column_n), 
          sep = "_",
          remove = FALSE)
  #
  # Collate count columns in long format ----
  count_long <- 
    index_data %>%
    select(image_id,  
           starts_with("count")) %>%
    gather(column_n, count, count_1:count_3)  %>%
    mutate(column_n = str_remove_all(column_n, "count_")) %>%
    unite(image_id_n, 
          c(image_id, column_n), 
          sep = "_",
          remove = TRUE)
  #
  # Join binomial and count in long format ----
  binomial_count_long <- 
    binomial_long %>%
    left_join(count_long, "image_id_n") 
  
  return(binomial_count_long)
}
#   fxn_correct_typos ----
fxn_correct_typos <- function(index_data){
  
  output_data <- 
    index_data %>%
    
    mutate(
      id = str_sub(image_id, 1, 11), 
      photo_type =
        case_when(
          photo_type == "Unidentifiable" & binomial != "N/A" ~ "Animal",
          # Revise photo_type for Unidentifiable binomial_2 
          photo_type == "Animal" & binomial == "Unidentifiable" ~ "Unidentifiable",
          TRUE ~ photo_type), 
      
      # Revise binomial for Unidentifiable binomial_2
      binomial =
        case_when(
          photo_type == "Unidentifiable" ~ "N/A",
          TRUE ~ binomial),
      
      # Create action column for camera maintenance activities
      action = 
        case_when(
          photo_type %in% list_photo_type_maintenance ~ photo_type, 
          TRUE ~ NA_character_), 
      
      photo_type =
        case_when(
          !is.na(action) & binomial != "N/A" ~ "Animal",
          !is.na(action) ~ "Maintenance",
          TRUE ~ photo_type),
      
      # Set count NA if there are no animals or identified objects in image
      count = ifelse(binomial == "N/A", NA, count)) %>%
    
    # Change 0 count for Animal except cow
    mutate(count =
             case_when(
               count == 0 & photo_type == "Animal" & binomial %nin% c("Bos taurus") ~ 1,
               TRUE ~ count)) %>%
    
    arrange(image_id_n) %>%
    
    # Add attributes from all_vault 
    left_join(all_vault %>%
                select(image_id, 
                       comments,
                       starts_with("qc")), "image_id") %>%
    select(image_id_n, 
           action, 
           photo_type, 
           binomial, 
           count,
           comments, 
           starts_with("qc"),
           id, 
           image_id,
           column_n) 
}

#   fxn_tidy_comments ----
fxn_tidy_comments <- function(index_data){
  
  #   Function to conditionally replace "Unidentifiable bird" ----
  replace_unidentified_bird <- function(binomial, comments) {
    
    ifelse(binomial == "Bird species", str_replace_all(comments, "Unidentifiable bird", ""), comments)
  }
  
  #   Function to replace comment patterns ----
  fxn_replace_comment_patterns <- function(comments) {
    
    # Define a named vector with patterns and their replacements ----
    replacements <- c(
      "review\\." = "review;",
      "missing\\." = "missing;",
      "CONFIRM ID: special species\\." = "CONFIRM ID: special species;",
      "CONFIRM ID: unidentifiable animal\\." = "CONFIRM ID: unidentifiable animal;",
      "CONFIRM COUNT: blank with count\\." = "CONFIRM COUNT: blank with count;",
      "ADD COUNT: animal missing; count" = "ADD COUNT: animal missing count",
      "EXCLUDE IMAGE: error flagged by cataloger" = "", 
      "ADD COUNT: animal missing count" = "",
      "ERROR: error flagged by cataloger; Do not catalog" = "Do not catalog",
      "ERROR: error flagged by cataloger; ; Do not catalog:" = "Do not catalog:", 
      "Camera knocked down; Camera knocked down" = "Camera knocked down; ",
      "Camera knocked down; Camera Knocked Down" = "Camera knocked down",
      "Camera knocked down; Camera knocked over" = "Camera knocked down; ", 
      "Camera knocked down; Camera down" = "Camera knocked down; ", 
      "; ," = ";"
    )
    
    str_replace_all(comments, replacements)
  }
  
  #   Function to remove error messages ----
  # index_errors = error_messages
  fxn_remove_error_message <- function(index_data) {
    
    # Define error messages ----
    list_qc_error <- 
      read_excel(here(path_in, 
                      "binomial-crosswalk.xlsx"), 
                 sheet = "error-messages") %>%
      select(error_type,
             error_message)  %>%
      filter(str_detect(error_message, "ADD|CONFIRM|FIX|NEEDS")) %>%
      pull(error_message)
    
    # Remove duplicate text strings  ----
    fxn_clean_comment <- function(comment) {
      # Split the comment into individual strings
      strings <- str_split(comment, ";\\s*")[[1]]
      # Remove duplicate strings  
      unique_strings <- unique(strings)
      # Remove QC error messages
      filtered_strings <- setdiff(unique_strings, list_qc_error)
      # Join the unique strings back together with "; " separator
      paste(filtered_strings, collapse = "; ")
    }
    
    # Remove leading, trailing space and ; ----
    fxn_clean_text <- function(text) {
      text %>%
        str_remove_all("^\\s*;\\s*|\\s*;\\s*$") %>%
        str_remove_all("^\\s*\\.\\s*|\\s*\\.\\s*$") %>%
        str_remove_all("^\\s*,\\s*|\\s*,\\s*$") %>%
        str_trim()
    }
    
    # Create lookup table of clean comments ----
    output_table <-
      index_data %>%
      mutate(comments = sapply(comments, fxn_clean_comment)) %>%
      mutate(comments = sapply(comments, fxn_clean_text),
             comments = str_trim(comments)) 
    
    return(output_table)
    
  }
  
  #   Create tidy comments ----
  lookup_comments <-
    index_data %>%
    drop_na(comments) %>%
    distinct(binomial, comments) %>%
    mutate(comments_orig = comments) %>%
    mutate(comments = fxn_replace_comment_patterns(comments)) %>%
    fxn_remove_error_message() %>%
    select(-binomial) %>%
    distinct()
  
  output_data <- 
    index_data  %>%
    rename(comments_orig = comments) %>%
    left_join(lookup_comments, "comments_orig") %>%
    select(-comments_orig) %>%
    mutate(comments = replace_unidentified_bird(binomial, comments)) %>%
    distinct()
  
  return(output_data)
  
}
#   fxn_add_taxonomic_attributes ----
fxn_add_taxonomic_attributes <- function(index_data){
  wi_taxonomy <- 
    read_excel(here(path_in, "attributes_species_wi.xlsx"), 
               sheet = "lookup_taxonomy") %>%
    select(wi_taxon_id, binomial:common_name)
  
  output_data <- 
    index_data %>%
    left_join(wi_taxonomy, "binomial") 
  
  return(output_data)
}
#   fxn_create_identified_by ----
fxn_create_identified_by <- function(index_data){
  output_data <- 
    index_data %>%
    rename(identified_by = qc_by) %>%
    left_join(dlog_wi %>%
                select(id, catalog_by), "id") %>%
    mutate(
      catalog_by = 
        case_when(catalog_by == "Gonzalo Arcidiácono" |
                    catalog_by ==  "Gonzalo Arcidiôøωcono" ~ 
                    "Gonzalo Arcidiacono", 
                  catalog_by == "Steven Hammerichodo" ~ "Steven Hammerich",
                  catalog_by == "Kalie Fanucchi" ~ "Kalie Fanuchi", 
                  catalog_by == "Bobbilance" ~ "Bobbi Lance",
                  TRUE ~ catalog_by)) %>%
    mutate(identified_by = 
             case_when(
               is.na(identified_by) ~ catalog_by, 
               TRUE ~ identified_by
             ))  %>%
    select(-catalog_by)
  
  return(output_data)
}
# ========================================================== -----
