# ========================================================== -----
# IMAGE TABLES  ----
# Read and collate ----
#   fxn_dir_table_map  ----
# index_type = "catalog"
fxn_dir_table_map <- function(index_site, 
                              index_type){
  
  
  fxn_define_camera_project(index_site)
  
  # Define surveys by type
  if(index_type == "catalog") {
    
    filter_dlog <- dlog %>%
      filter(use_data == TRUE,
             done_catalog == TRUE,
             done_tidy == FALSE)
    index_path <- path_table_catalog
    
  } else if(index_type == "tidy") {
    
    filter_dlog <- dlog %>%
      filter(use_data == TRUE,
             done_catalog == TRUE,
             done_tidy == TRUE)
    index_path <- path_table_catalog
    
  } else if(index_type == "qc") {
    
    filter_dlog <- dlog %>%
      filter(use_data == TRUE,
             done_tidy == TRUE,
             done_qc == TRUE | done_qc == "CHECK", 
             done_vault == FALSE)
    index_path <- path_table_qc
    
  } else if(index_type == "clean") {
    
    filter_dlog <- dlog %>%
      filter(use_data == TRUE,
             done_qc == TRUE, 
             done_vault == FALSE)
    index_path <- path_table_qc
    
  }
  list_id <- unique(filter_dlog$id)
  
  # Use by index_type to define surveys  ----
  
  # if(index_type %in% "catalog"){
  #   
  #   list_id <- 
  #     dlog %>%
  #     filter(done_catalog == TRUE, 
  #            done_tidy != TRUE) %>%
  #     arrange(id) %>%
  #     pull(id)
  #   
  #   index_path <- path_table_catalog
  # }
  # if(index_type %in% "tidy"){
  #   
  #   list_id <- 
  #     dlog %>%
  #     filter(done_catalog == TRUE, 
  #            done_tidy == FALSE, 
  #            done_qc == FALSE,
  #            has_data != FALSE) %>%
  #     pull(id)
  #   
  #   index_path <- path_table_catalog
  # }
  # if(index_type %in% "qc"){
  #   
  #   list_id <- 
  #     dlog %>%
  #     filter(done_catalog != FALSE, 
  #            done_qc == TRUE, 
  #            done_vault == FALSE) %>%
  #     arrange(id) %>%
  #     pull(id)
  #   
  #   index_path <- path_table_qc
  # }
  
  # Map files ----
  tbl_files <-
    tibble(path = 
             dir_ls(path = index_path,
                    recurse = FALSE, 
                    type = "file")) %>%
    mutate(file_name = path_file(path),
           directory = path_dir(path),
           id = str_sub(file_name, 1, 11)) %>%
    filter(id %in% list_id) %>%
    select(id, 
           file_name,
           directory,
           path)
  
  if(index_type %in% c("qc", "vault")){
    tbl_files <- 
      tbl_files %>%
      filter(str_detect(file_name, "qc"))
  }
  return(tbl_files)
  
}
#   fxn_wrap_read_excel -----
fxn_wrap_read_excel <- function(index_path){
  
  index_id <- str_sub(path_file(index_path), 1, 11)
  cat(index_id, "\n")
  
  data <- 
    read_excel(here(index_path),
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
           image_file = as.character(image_file)) %>%
    select(all_of(list_column_names)) %>%
    fxn_fix_typos()
  
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
    data
  } else {
    message(paste0("Error with file ", index_path))
  }
}
#   fxn_table_read_xlsx: Read one table ----
fxn_table_read_xlsx <- function(index_file, 
                                index_type){
  index_folder <- here(path_data, 
                       paste0("image-tables_", 
                              index_type))
  index_path <- here(index_folder, index_file)
  
  
  
  column_count <- 
    read_excel(index_path,
               sheet = "Images", 
               col_types = "text") %>%
    clean_names() %>%
    ncol()
  
  if(column_count != 21){
    message(
      paste0(
        "ERROR: Wrong column count for ", 
        index_file))
  }else{
    
    data <- fxn_wrap_read_excel(index_path)
    
  }
}

#   fxn_table_collate_catalog  ----
# Collates all image tables into one data frame
fxn_table_collate_catalog <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # List the deployments to collate ----
  list_id <- 
    dlog %>%
    filter(done_catalog == TRUE, 
           done_tidy == FALSE) %>%
    arrange(id)  %>%
    pull(id)
  
  # Read and collate files ----
  datalist <- list()
  # index_id <- list_id[1]
  for(index_id in list_id){
    
    index_file <- paste0(index_id, "_Final.xlsx")
    
    datalist[[index_id]] <-
      fxn_table_read_xlsx(index_file = index_file,
                          index_type = "catalog")
    
  }
  
  bind_datalist <- do.call(bind_rows, datalist)
}
#   fxn_table_collate_qc  ----
# Collates all image tables into one data frame
fxn_table_collate_qc <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  list_id <- 
    dlog %>%
    filter(done_qc == TRUE) %>%
    arrange(id)  %>%
    pull(id)
  
  qc_files <- 
    tibble(file_name = 
             list.files(path_table_qc, 
                        pattern = ".xlsx")) %>%
    mutate(id = str_sub(file_name, 1, 11)) %>%
    filter(id %in% list_id) %>%
    # Exclude any _final_tidy files 
    filter(str_detect(file_name, "qc") == TRUE)  %>%
    mutate(qc_initials_lower = 
             str_to_lower(
               str_sub(file_name, 27, 28))) %>%
    left_join(lookup_qc_initials, "qc_initials_lower")
  
  list_id_files <- unique(qc_files$id)
  
  # Read xlsx files ----
  datalist <- list()
  for(index_id in list_id_files){
    # index_id = list_id_files[1]
    
    # Create helpers  ----
    subset <- 
      qc_files %>%
      filter(id %in% index_id)
    
    index_file <- subset$file_name
    index_qc_by <-  subset$qc_by
    # 
    
    # Define index_n (binomial_, count_) ----
    index_n <- 
      read_excel(here(path_table_qc,
                      index_file),
                 sheet = "Images") %>%
      fxn_define_index_n() 
    
    # data_raw: Image table -----
    data_raw <-
      fxn_table_read_xlsx(index_file = index_file, 
                          index_type = "qc") %>%
      distinct() 
    
    # data_tidy: Tidy values for binomial and count  ----
    data_tidy <- 
      fxn_tidy_binomial_count(index_data = data_raw,
                              index_type = "qc", 
                              index_n = index_n)
    
    
    first_image_n <- min(data_tidy$image_n, na.rm = TRUE)
    last_image_n <- max(data_tidy$image_n, na.rm = TRUE)
    
    # data_tidy_fixed -----
    data_tidy_fixed <- 
      data_tidy  %>%
      
      # Replace NA with N/A for non-Animal photo_type 
      mutate(
        binomial_1 = 
          case_when(
            is.na(binomial_1) & 
              photo_type %in% c("Blank", 
                                "Start", 
                                "End", 
                                "Setup", 
                                "Pickup",
                                "Unidentifiable", 
                                "Corrupt") ~
              "N/A", 
            TRUE ~ binomial_1), 
        
        # Correct Blank count 
        count_1 = 
          case_when(
            count_1 = 1 &
              photo_type %in% "Blank" &
              qc_certainty %in% "Absolutely sure" ~
              0, 
            TRUE ~ count_1), 
        
        # Correct Unidentifiable as End in photo_type
        binomial_1 =
          case_when(
            image_n == last_image_n &
              photo_type == "Unidentifiable" &
              qc_certainty == "Absolutely sure" ~
              "Unidentifiable",
            TRUE ~ binomial_1),
        photo_type =
          case_when(
            image_n == 
              last_image_n &
              binomial_1 == "Unidentifiable" &
              qc_certainty == "Absolutely sure" ~ "End",
            TRUE ~ photo_type)) %>%
      
      select(any_of(list_column_names)) 
    
    datalist[[index_id]] <- data_tidy_fixed
  }
  
  # Bind image tables and correct typos ----
  bind_datalist <- 
    do.call(bind_rows, datalist)
}
#
#   fxn_define_index_n ----
# Define function to find the maximum index number
# from count columns after data cleaning
fxn_define_index_n <- function(index_data) {
  
  # Clean data, select 'count' columns, get distinct values,
  # remove constant columns, extract names, remove 'count_' prefix
  max_index <- index_data %>%
    clean_names() %>%
    select(starts_with("count")) %>%
    distinct() %>%
    remove_constant() %>%
    names() %>%
    str_remove("count_") %>%
    # Ensure the counts are numeric for max operation
    as.numeric() %>%  
    # Remove NAs before finding max
    max(na.rm = TRUE)  
  
  # In case all 'count' columns are NAs or missing, default to 1
  index_n <- 
    case_when(
    is.infinite(max_index) | is.na(max_index) ~ 1, 
    TRUE ~ max_index)
  
  # Return the maximum index found or default value
  return(index_n)
}

#   fxn_new_photo_type_binomial  ----
fxn_new_photo_type_binomial <- function(index_data, index_n){
  
  # Construct the column name
  column_n <- paste0("binomial_", index_n)
  
  # Rename and optionally drop NA based on index_n
  input <- index_data %>%
    rename(subset = all_of(column_n)) %>%
    { if(index_n > 1) drop_na(.) else . }  %>%
    distinct(photo_type, subset) %>%
    unite(photo_type_binomial, c(photo_type, subset), sep = "_") %>%
    # Subset to distinct photo_type_binomial combinations
    distinct(photo_type_binomial)  %>%
    # Join with known photo_type_binomial combinations
    left_join(lookup_photo_type_binomial, by = "photo_type_binomial") %>%
    filter(is.na(photo_type)) %>%
    select(photo_type_binomial)
  
  return(input)
}
#   fxn_new_photo_type_binomial_all  ----
fxn_new_photo_type_binomial_all <- function(index_site, 
                                            index_type){
  
  fxn_define_camera_project(index_site) 
  
  # Read and collate cataloged tables ----
  if(index_type == "catalog"){
    index_data <- fxn_table_collate_catalog(index_site)
  }
  if(index_type == "qc"){
    index_data <- fxn_table_collate_qc(index_site)
  }
  
  # Identify new photo_type_binomial combinations ----
  # Get new photo_type_binomial_1
  new_1 <-
    fxn_new_photo_type_binomial(index_data, 
                                index_n = 1) %>%
    mutate(column_n = 1)
  #
  # Review photo_type_binomial_2
  new_2 <-
    fxn_new_photo_type_binomial(index_data, 
                                index_n = 2) %>%
    mutate(column_n = 2)
  #
  # Review photo_type_binomial_3
  new_3 <- 
    fxn_new_photo_type_binomial(index_data, 
                                index_n = 3) %>%
    mutate(column_n = 3)
  #
  # Create table with all new combinations ----
  new_all <- bind_rows(new_1, 
                       new_2, 
                       new_3) 
  
  if(nrow(new_all) > 0){
    new_all_split <- 
      str_split_fixed(new_all$photo_type_binomial,
                      "_", 2) %>%
      as_tibble(.name_repair = NULL, 
                rownames = NULL) %>%
      rename(photo_type = V1, 
             binomial = V2) %>%
      unite(photo_type_binomial, 
            c(photo_type, binomial), 
            remove = FALSE) %>%
      left_join(new_all,
                "photo_type_binomial") %>%
      arrange(photo_type, 
              binomial) %>%
      # Revise to facilitate updating binomial-crosswalk.csv
      select(orig_photo_type = photo_type, 
             orig__binomial = binomial, 
             photo_type_binomial) %>%
      mutate(photo_type = NA, 
             binomial = NA, 
             comments_rev = NA, 
             error_message = NA, 
             date_added = ymd(Sys.Date()))
    
    # Write to csv for manual revision of binomial-crosswalk.csv   
    index_file_name <- paste0(index_site, "_new-combinations.csv")
    
    fxn_archive_old_csv(index_file_name = index_file_name)
    
    write_csv(new_all_split,
              here(path_out,
                   index_file_name), 
              na = "")
    
    print(new_all_split)
    
  }else{
    message("No new photo_type x binomial combinations")
  }
}

# Check xlsx ----

#   fxn_table_check_blank  ----
fxn_table_check_blank <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # Create input for checks ----
  dlog_subset <- 
    dlog %>%
    filter(has_data != FALSE,
           done_exif == TRUE,
           done_blank == TRUE,
           done_catalog == FALSE) %>%
    arrange(id) %>%
    select(id,
           year_to, 
           date_from, 
           date_to, 
           date_excl_from, 
           date_excl_to, 
           n_img_to) 
  
  list_id_dlog_subset <- unique(dlog_subset$id)
  
  list_image_tables <- 
    tibble(file_name = 
             list.files(here(path_table_blank), 
                        pattern = "*.xlsx")) %>%
    mutate(id = str_sub(file_name, 1, 11)) %>%
    filter(id %in% list_id_dlog_subset) %>%
    pull(file_name)

  # tables_all: Read all tables ----
  # Read and process each file, then combine into one tibble
  tables_all <-
    map_df(list_image_tables, ~ {
      
      data <- 
        fxn_wrap_read_excel(index_path = 
                              here(path_table_blank, .x)) %>%
        clean_names()
    })

  # tables_summary: Summarize tables by id ----
  tables_summary <- fxn_table_check_dlog(tables_all, 
                                         index_type = "blank")
  # tables_errors: Subset summary to errors ----
  id_errors <- 
    tables_summary %>%
    # remove_constant()  %>%
    gather(column, value, starts_with("err")) %>%
    filter(value == TRUE, 
           ends_early != TRUE, 
           starts_late != TRUE) %>%
    group_by(id, column) %>%
    count() %>%
    spread(column, n)
  
  list_id_errors <- unique(id_errors$id)
  
  list_columns <- 
    tables_summary %>%
    select(starts_with("err")) %>%
    names() %>%
    c(
      "ends_early", 
      "starts_late",
      "date_from", 
      "date_to", 
      "date_excl_from",
      "date_excl_to", 
      "exif_date_img_from", 
      "exif_date_img_to", 
      "exif_n_img_from",
      "n_img_from",
      "exif_n_img_to", 
      "n_img_to"
    )
  
  tbl_errors <- 
    tables_summary %>%
    # remove_constant() %>%
    filter(id %in% list_id_errors) %>%
    relocate(id, 
             any_of(list_columns))  %>%
    left_join(dlog %>%
                select(id, 
                       error_type, 
                       error_subtype, 
                       comments), "id") %>%
    filter(error_type %in% "None" | 
             is.na(error_type))
  
  # Write to csv for manual revision of dlog ----
  if(nrow(tbl_errors) > 0){
    
    unique(tbl_errors$id)
    print(tbl_errors)
    
    index_file_name <- paste0(index_site, "_blank_error-summary.csv")
    
    fxn_archive_old_csv(index_file_name = index_file_name)
    
    write_csv(tbl_errors,
              here(path__out,
                   index_file_name), 
              na = "")
    
  }else{
    message("No errors found in _blank tables")
  }
}
#   fxn_table_check_errors -----
fxn_table_check_errors <- function(index_site, 
                                   index_type){
  
  fxn_define_camera_project(index_site)
  
  # Create helpers ----
  dir_table <- fxn_dir_table_map(index_site, 
                                 index_type)
  
  list_files <- unique(dir_table$file_name)
  
  # Iterate by image table ----
  datalist <- list()
  for(index_file in list_files){
    # index_file <- "M_B5_210221_Final.xlsx"
    # index_file <- list_files[2]
    
    subset <-
      dir_table %>%
      filter(file_name %in% index_file)
    
    # data_raw: Image table -----
    datalist[[index_file]] <- 
      fxn_table_read_xlsx(index_file = index_file, 
                          index_type = index_type)  %>%
      filter(!is.na(error)) %>%
      select(id, 
             image_n,
             date,
             time)
    
  }
  
  bind_datalist <- do.call(bind_rows, datalist) 
  
  if(nrow(bind_datalist) > 0){
    output <-
      bind_datalist %>%
      group_by(id) %>%
      summarize(n_error = n_distinct(image_n), 
                date_from_error = ymd(min(date, na.rm = TRUE)), 
                date_to_error = ymd(max(date, na.rm = TRUE)), 
                n_img_from_error = min(image_n, na.rm = TRUE), 
                n_img_to_error = max(image_n, na.rm = TRUE))
  }else{
    output <- bind_datalist
  }
  
  return(output)
}
#   fxn_table_check_catalog ----
# index_type = "catalog"

fxn_table_check_catalog <- function(index_site, 
                                    index_type){
  
  fxn_define_camera_project(index_site)
  
  # Create helpers: Cataloged image tables 
  dir_table <- fxn_dir_table_map(index_site, 
                                 index_type)
  
  list_files <- unique(dir_table$file_name)
  
  datalist <- list()
  for(index_file in list_files){
    # index_file = list_files[1]
    
    index_data <-
      dir_table %>%
      filter(file_name %in% index_file)
    
    datalist[[index_file]] <- 
      fxn_conduct_checks_catalog(index_data = index_data,
                                 index_file = index_file,
                                 index_type = index_type)
    
  }
  
  # Combine data frames
  bind_datalist <-
    bind_rows(datalist) %>%
    filter(has_fatal == TRUE) %>%
    select(-has_fatal)
  
  # Conditionally relocate columns if any rows are present
  if(nrow(bind_datalist) > 0){
    output <-
      bind_datalist %>%
      relocate(id,
               index_n,
               starts_with("err"),
               starts_with("has_column"))
    
    index_file_name <- paste0(index_site, "_catalog_errors.csv")
    
    fxn_archive_old_csv(index_file_name = index_file_name)
    
    write_csv(output,
              here(path_out,
                   index_file_name), 
              na = "")
    
  } else {
    output <- bind_datalist
  }
  
  print(output)
 
}

#   fxn_table_check_qc ----
fxn_table_check_qc <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  list_id_dlog <- 
    dlog %>%
    filter(done_qc == TRUE, 
           done_vault == FALSE) %>%
    pull(id)
  
  # Create helpers: Cataloged image tables 
  dir_table <-
    fxn_dir_table_map(index_site,
                      index_type = "clean") %>%
    mutate(extract_names =
             str_extract(file_name,
                         "(?<=_qc_).*(?=\\.xlsx)"),
           last_initials =
             substrRight(extract_names, 2)) %>%
    drop_na(extract_names) %>%
    rename(qc_initials_lower = last_initials) %>%
    left_join(lookup_qc_initials,
              "qc_initials_lower") %>%
    filter(id %in% list_id_dlog)
  
  # To migrate legacy data
  # dir_table <-
  #   fxn_dir_table_map(index_site,
  #                     index_type = "qc")  %>%
  #   left_join(dlog %>%
  #               select(id, 
  #                      qc_initials = qc_by, 
  #                      done_qc_migrate), "id") %>%
  #   left_join(lookup_qc_initials, "qc_initials")  

  list_files <- unique(dir_table$file_name)
  
  datalist <- list()
  # index_file = list_files[1]
  for(index_file in list_files){

    index_data <-
      dir_table %>%
      filter(file_name %in% index_file)
    
    datalist[[index_file]] <- 
      fxn_conduct_checks_qc(index_data = index_data,
                            index_file = index_file)
    
  }
  
    # Combine data frames
  bind_datalist <-
    bind_rows(datalist) %>%
    filter(has_fatal == TRUE) %>%
    select(-has_fatal)
  
  # Conditionally relocate columns if any rows are present
  if(nrow(bind_datalist) > 0){
    output <-
      bind_datalist %>%
      relocate(id,
               starts_with("err"),
               starts_with("has_column"))
    
    index_file_name <- paste0(index_site, "_qc_errors.csv")
    
    fxn_archive_old_csv(index_file_name = index_file_name)
    
    write_csv(output,
              here(path_out,
                   index_file_name), 
              na = "")
    
  } else {
    output <- bind_datalist
  }
  
  print(output)
  
}

#   fxn_conduct_checks_catalog ----
fxn_conduct_checks_catalog <- function(index_data,
                                       index_file, 
                                       index_type){
  
  cat(str_sub(index_file, 1, 11), "\n")
  # Create containers for check results ----
  check_errors <- 
    tibble(id = index_data$id, 
           err_column_names = NA, 
           err_start = NA, 
           err_end = NA, 
           err_photo_type_blank = NA, 
           err_binom_1 = NA, 
           err_binom_2 = NA, 
           err_binom_3 = NA, 
           err_dupe_2 = NA, 
           err_dupe_3 = NA, 
           err_dupe_4 = NA, 
           err_dupe_5 = NA)
  
  output <- 
    tibble(id = index_data$id, 
           has_binom_2 = NA, 
           has_binom_3 = NA, 
           has_count_2 = NA, 
           has_count_3 = NA, 
           has_column_2 = NA,
           has_column_3 = NA,
           has_column_1_only = NA)
  
  # data_raw: Image table -----
  data_raw <- 
    fxn_table_read_xlsx(index_file = index_file, 
                        index_type = index_type) 
  # Evaluate data entry errors ----
  #   err_column_names: Check column names ----
  input_column_names <- 
    tibble(column_names = names(data_raw), 
           has_column = TRUE) 
  
  check_column_names <- 
    tibble(column_names = list_column_names)  %>%
    left_join(input_column_names, "column_names") %>%
    filter(is.na(column_names) | has_column == FALSE)
  check_errors$err_column_names <- nrow(check_column_names) > 0
   
  #   err_start, err_end: Check photo_type for Start, End ----
  check_start_end <- 
    data_raw %>%
    filter(photo_type %in% c("Start", "End")) %>%
    distinct(photo_type) %>%
    mutate(photo_type = str_to_lower(photo_type)) 
  
  has_start <- 
    check_start_end %>%
    filter(photo_type == "start")
  check_errors$err_start <- nrow(has_start) != 1
  
  has_end <- 
    check_start_end %>%
    filter(photo_type == "end")
  check_errors$err_end <- nrow(has_end) != 1
  
  #   err_photo_type_blank ----
  check_photo_type_blank <- 
    data_raw %>%
    filter(is.na(binomial_1), 
           is.na(photo_type)) 
  check_errors$err_photo_type_blank <- nrow(check_photo_type_blank) > 0
  
  #   err_binom_1 ----
  check_binom_1 <- 
    data_raw %>%
    distinct(photo_type, binomial_1) %>%
    unite(photo_type_binomial, c(photo_type,
                                 binomial_1)) %>%
    # Subset to distinct photo_type_binomial combinations
    distinct(photo_type_binomial)  %>%
    # Join with known photo_type_binomial combinations
    left_join(lookup_photo_type_binomial, 
              "photo_type_binomial") %>%
    filter(is.na(photo_type))  
  check_errors$err_binom_1 <- nrow(check_binom_1) > 0
 
  #   err_binom_2 ----
  check_binom_2 <- 
    data_raw %>%
    drop_na(binomial_2) %>%
    distinct(binomial_2) %>%
    rename(orig_binomial = binomial_2) %>%
    left_join(lookup_binomial,
              "orig_binomial") %>%
    filter(is.na(binomial)) 
  check_errors$err_binom_2 <- nrow(check_binom_2) > 0
  
  #   err_binom_3 ----
  check_binom_3 <- 
    data_raw %>%
    drop_na(binomial_3) %>%
    distinct(binomial_3) %>%
    rename(orig_binomial = binomial_3) %>%
    left_join(lookup_binomial,
              "orig_binomial") %>%
    filter(is.na(binomial)) 
  check_errors$err_binom_3 <- nrow(check_binom_3) > 0

  #   err_dupe_n: Evaluate duplicate image_n ----
  check_dupes <- 
    data_raw %>%
    group_by(image_n) %>%
    count() %>%
    ungroup() %>%
    filter(n>1)  
 
    # Only proceed if there are any duplicates
  if(nrow(check_dupes) > 0){

    # For each unique count of duplicates, set the corresponding error flag
    for (i in 2:5) {
      check_errors[[paste0("err_dupe_", i)]] <- any(check_dupes$n == i)
    }
  }
  
  #   has_fatal: Check for fatal errors ---- 
  ncol_err <- 
    check_errors %>%
    select(starts_with("err")) %>%
    ncol() + 1
  
  check_fatal <- 
    check_errors %>%
    relocate(id) %>%
    gather(column, value, 2:all_of(ncol_err)) %>%
    filter(value == TRUE)
  
  has_fatal <- nrow(check_fatal) > 0
  
  # Evaluate column_n (multi-species) ---- 
  #   has_binom_2 ----
  check_binom_2 <- 
    data_raw %>%
    drop_na(binomial_2) %>%
    distinct(binomial_2) 
  output$has_binom_2 <- nrow(check_binom_2) > 0 
  
  #   has_count_2 ----
  check_count_2 <- 
    data_raw %>%
    drop_na(count_2) %>%
    distinct(count_2) 
  output$has_count_2 <- nrow(check_count_2) > 0 
  
  #   has_binom_3 ----
  check_binom_3 <- 
    data_raw %>%
    drop_na(binomial_3) %>%
    distinct(binomial_3) 
  output$has_binom_3 <- nrow(check_binom_3) > 0 
  
  #   has_count_3 ----
  check_count_3 <- 
    data_raw %>%
    drop_na(count_3) %>%
    distinct(count_3) 
  output$has_count_3 <- nrow(check_count_3) > 0 
  
  #   has_column_2 ----
  output$has_column_2 <- output$has_binom_2 | output$has_count_2
  
  #   has_column_3 ----
  output$has_column_3 <- output$has_binom_3 | output$has_count_3
  
  #   has_column_1_only ----
  output$has_column_1_only <-
    output$has_column_2 == FALSE & output$has_column_3 == FALSE
  
  # Check column_n combinations ----
  #   check_column_n -----
  check_column_n <- 
    output %>%
    mutate(index_n =
             case_when(
               has_column_3 == TRUE ~ 3, 
               has_column_2 == TRUE ~ 2,
               has_column_1_only == TRUE ~ 1 
             ), 
           has_fatal = has_fatal) %>%
    relocate(id, index_n, has_fatal)
  
  #   column_n ----
  column_n <- check_column_n$index_n
  
  #   subset_n ----
  if(column_n == 3){
    
    subset_n <- 
      check_column_n %>%
      mutate(column_n_ok = 
               case_when(
                 has_column_1_only == FALSE &
                   has_column_2 == TRUE &
                   has_column_3 == TRUE ~ TRUE, 
                 TRUE ~ FALSE)) %>%
      select(-starts_with("has_column"))
    
  }
  
  if(column_n == 2){
    
    subset_n <- 
      check_column_n %>%
      mutate(column_n_ok = 
               case_when(
                 has_column_1_only == FALSE &
                   has_column_2 == TRUE &
                   has_column_3 == FALSE ~ TRUE, 
                 TRUE ~ FALSE)) %>%
      select(-starts_with("has_column"))
  }
  
  if(column_n == 1){
    
    subset_n <- 
      check_column_n %>%
      mutate(column_n_ok = 
               case_when(
                 has_column_1_only == TRUE &
                   has_column_2 == FALSE &
                   has_column_3 == FALSE ~ TRUE, 
                 TRUE ~ FALSE)) %>%
      select(-starts_with("has_column"))
  }
  
  # Exclude unnecessary columns ----
  if(subset_n$column_n_ok == TRUE){
    thin_subset_n <- 
      subset_n %>%
      select(id,
             index_n, 
             column_n_ok,
             has_fatal)
  }else{
    thin_subset_n <- 
      subset_n %>%
      relocate(column_n_ok, 
               .after = "index_n")
    
  }
  
  # Combine results ----
  if(has_fatal == TRUE){
    spread_fatal <- 
      check_fatal %>%
      spread(column, value)
    
    results <- 
      thin_subset_n %>%
      left_join(spread_fatal, "id")
    
  }else{
    results <- thin_subset_n
  }
}
#   fxn_conduct_checks_qc ----
fxn_conduct_checks_qc <- function(index_data,
                                  index_file){
  
  cat(str_sub(index_file, 1, 11), "\n")
  index_qc_by <-  index_data$qc_by
  
  # Define index_n (binomial_, count_) ----
  index_path <- here(path_table_qc, index_file)
  index_n <- 
    fxn_wrap_read_excel(index_path) %>%
    fxn_define_index_n() 
  
  # data_raw: Image table -----
  data_raw <- 
    fxn_table_read_xlsx(index_file = index_file, 
                        index_type = "qc") %>%
    remove_empty("rows") 


  # data_tidy: Tidy values for binomial and count  ----
  data_tidy <- 
    fxn_tidy_binomial_count(index_data = data_raw,
                            index_type = "qc", 
                            index_n = index_n)
  
  first_image_n <- min(data_tidy$image_n, na.rm = TRUE)
  last_image_n <- max(data_tidy$image_n, na.rm = TRUE)
  
  # data_tidy_fixed -----
  data_tidy_fixed <- 
    data_tidy  %>%

    # Replace NA with N/A for non-Animal photo_type 
    mutate(
      binomial_1 = 
        case_when(
          is.na(binomial_1) & 
            photo_type %in% c("Blank", 
                              "Start", 
                              "End", 
                              "Setup", 
                              "Pickup",
                              "Unidentifiable", 
                              "Corrupt") ~
            "N/A", 
          TRUE ~ binomial_1), 
      
      # Correct Blank count 
      count_1 = 
        case_when(
          count_1 = 1 &
            photo_type %in% "Blank" &
            qc_certainty %in% "Absolutely sure" ~
            0, 
          TRUE ~ count_1), 
    
    # Correct Unidentifiable as End in photo_type
    binomial_1 =
        case_when(
          image_n == last_image_n &
            photo_type == "Unidentifiable" &
            qc_certainty == "Absolutely sure" ~
            "Unidentifiable",
          TRUE ~ binomial_1),
      photo_type =
        case_when(
          image_n == 
            last_image_n &
            binomial_1 == "Unidentifiable" &
            qc_certainty == "Absolutely sure" ~ "End",
          TRUE ~ photo_type)) %>%

    select(any_of(list_column_names)) 

  # Create container for check results ----
  check_errors <- 
    tibble(id = index_data$id, 
           err_needs_review = NA, 
           err_unidentifiable = NA, 
           err_binom_2_missing = NA, 
           err_unknown = NA, 
           err_photo_type_list = NA, 
           err_blank_binomial = NA, 
           err_count_animal = NA, 
           err_count_blank = NA, 
           err_special_species = NA, 
           err_qc_by = NA)
  
  # Conduct checks ----
  #   err_needs_review ----
  # Confirm qc_by, qc_certainty if review = TRUE
  check_needs_review <- 
    data_tidy_fixed %>%
    filter(review == TRUE, 
           is.na(qc_by) | is.na(qc_certainty)) 
  
  if(nrow(check_needs_review) > 0){
    check_errors$err_needs_review = TRUE
  }
  
  #   err_unidentifiable ----
  # Confirm unidentifiable have been reviewed 
  check_unidentifiable <-
    data_tidy_fixed %>%
    filter(photo_type %in% "Unidentifiable") %>%
    filter(is.na(qc_by) | is.na(qc_certainty))
  
  if(nrow(check_unidentifiable) > 0){
    check_errors$err_unidentifiable = TRUE
  }
  #   err_binom_2_missing ----
  check_binom_2_missing <- 
    data_tidy_fixed %>%
    filter(count_2 > 0) %>%
    filter(is.na(binomial_2))
  
  if(nrow(check_binom_2_missing) > 0){
    check_errors$err_binom_2_missing = TRUE 
  }
  
  #   err_unknown ----
  # Confirm no unknown images (Should be unidentifiable)
  check_unknown <- 
    data_tidy_fixed %>%
    filter(photo_type %in% "Unknown" | 
             binomial_1 %in% "Unknown")
  
  if(nrow(check_unknown) > 0){
    check_errors$err_unknown = TRUE
  }
  
  #   err_photo_type_list ----
  check_photo_type_list <- 
    data_tidy_fixed %>%
    filter(photo_type %nin% c(list_photo_type, 
                              "Maintenance"))
  
  if(nrow(check_photo_type_list) > 0){
    check_errors$err_photo_type_list = TRUE
  }
  #   err_blank_binomial ----
  # Confirm Blank has N/A binomial
  # Can have species in binomial_1 during maintenance
  check_blank_binomial <- 
    data_tidy_fixed %>%
    filter(photo_type %in% "Blank", 
           binomial_1 %nin% c("N/A", NA))
  
  if(nrow(check_blank_binomial) > 0){
    check_errors$err_blank_binomial = TRUE
  }
  
  #   err_count_animal ----
  check_count_animal <- 
    data_tidy_fixed %>%
    filter(binomial_1 %in% list_binomial_count, 
           is.na(count_1))
  
  if(nrow(check_count_animal) > 0){
    check_errors$err_count_animal = TRUE
  }
  #   err_count_blank ----
  check_count_blank <- 
    data_tidy_fixed %>%
    filter(photo_type %in% "Blank", 
           count_1 > 0)
  
  if(nrow(check_count_blank) > 0){
    check_errors$err_count_blank = TRUE
  }
  #   err_special_species ----
  check_special_species <- 
    data_tidy_fixed %>%
    filter(binomial_1 %in% list_binomial_special, 
           is.na(qc_by))
  
  if(nrow(check_special_species) > 0){
    check_errors$err_special_species = TRUE
  }
  
  #   err_qc_by ----
  check_qc_by <- 
    data_tidy_fixed %>%
    drop_na(qc_by) %>%
    distinct(qc_by) %>%
    filter(qc_by %nin% unique(lookup_qc_initials$qc_by))
  
  if(nrow(check_qc_by) > 0){
    check_errors$err_qc_by = TRUE
  }
  
  #   has_fatal: Check for fatal errors ---- 
  ncol_err <- 
    check_errors %>%
    select(starts_with("err")) %>%
    ncol() + 1
  
  check_fatal <- 
    check_errors %>%
    relocate(id) %>%
    gather(column, value, 2:all_of(ncol_err)) %>%
    filter(value == TRUE)
   
  # Combine results ----
  if(nrow(check_fatal) > 0){
    results <- 
      check_fatal %>%
      spread(column, value) %>%
      mutate(has_fatal = TRUE)
    
  }else{
    results <-
      tibble(id = index_data$id, 
             has_fatal = FALSE)
  }
}
#   fxn_check_count_format ----
# Check count columns 
fxn_check_count_format <- function(index_site, index_path){
  
  fxn_define_camera_project(index_site)
  
  # 1. Read all xlsx files in a folder
  file_paths <- list.files(path = index_path, 
                           pattern = "\\.xlsx$", 
                           full.names = TRUE)
  
  # 2. Create a tibble for each file
  data_list <- map(file_paths, function(file_path) {
    # Read the file
    data <- read_xlsx(file_path)
    
    # Identify columns that start with 'count'
    count_columns <- grep("^count", names(data), value = TRUE)
    
    # Gather information for each 'count' column
    count_data <- map_dfr(count_columns, function(column_name) {
      if(column_name %in% names(data)) {
        # Extract distinct values and format of the column
        distinct_values <- distinct(data, !!sym(column_name)) %>% pull(!!sym(column_name))
        data_type <- class(data[[column_name]])[1]
      } else {
        # Default values if the column does not exist
        distinct_values <- NA
        data_type <- "NA"
      }
      
      # Create a tibble for this column
      tibble(
        column_name = column_name,
        format = data_type,
        distinct_values = list(distinct_values)
      )
    })
    
    # Add file name to each row
    mutate(count_data, File_Name = basename(file_path))
  })
  
  # 3. Bind all tibbles into one
  final_data <- bind_rows(data_list)
  
  # Print or return the final tibble
  return(final_data)
  
  final_data_character <- 
    final_data %>%
    filter(format == "character")
  
  if(nrow(final_data_character) > 0){
    print(final_data_character)
    cat("ERROR: Count column format as character", "\n")
  }else{
    cat("No count column format as character", "\n")
  }
  
}

# Create ----
#   fxn_table_create_blank ----
# Convert _exif.csv to _blank.xlsx   
fxn_table_create_blank <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # List deployments for iteration ----
  list_id_blank <-
    dlog %>%
    filter(has_data == TRUE,
           done_exif == TRUE,
           done_blank == FALSE,
    ) %>%
    pull(id)
  
  # List deployments for iteration ----
  # list_id_blank <-
  #   dlog %>%
  #   filter(has_data == TRUE, 
  #          done_exif == TRUE, 
  #          done_blank == TRUE, 
  #          done_catalog == FALSE, 
  #   ) %>%
  #   pull(id)
  
  # Collate exif files and join dlog ----
  exif_map_dlog <- 
    fxn_exif_collate_csv(index_site)  %>%
    mutate(image_file = here(path_jpg, file_name), 
           image_id = str_remove(file_name, 
                                 "\\.JPG")) %>%
    left_join(dlog %>%
                select(-camera), "id") %>%
    select(id,  
           image_id,
           date_time, 
           date, 
           time,
           image_n,  
           date_from,
           date_to, 
           starts_with("date_func"),
           starts_with("date_excl"),
           starts_with("date_img"),
           n_img_from,
           n_img_to,
           starts_with("n_img_excl_from_"), 
           starts_with("n_img_excl_to_"), 
           starts_with("n_img"), 
           starts_with("error"),
           image_file) %>%
    arrange(id, image_n)
  
  # Create error messages for known problems ----
  problem_images <- 
    exif_map_dlog %>%
    filter(id %in% list_id_blank) %>%
    # Define image range and error message
    fxn_table_exclude_lookup() %>%
    # The following was fxn_table_exclude_sequence
    group_by(id) %>%
    summarize(
      data = list(
        map2_df(img_from, 
                img_to,
                ~ tibble(image_n = .x:.y, 
                         error_text = first(error_text))
        )
      ),
      .groups = "drop") %>%
    unnest(data)
  
  list_id_problems <- unique(problem_images$id)
  set_has_problems <- (length(list_id_problems) > 0)
  
  # Iterate by list_id_blank to create _blank.xlsx ----
  # index_id = list_id_blank[1]
  # index_id = "M_C4_190812"
  for(index_id in list_id_blank){
    
    cat(index_id, "\n")
    
    index_camera <- str_sub(index_id, 3, 4)
    id_has_problems <- index_id %in% list_id_problems
    
    # Bind the exif info with Images template  -----
    subset_files <- 
      exif_map_dlog %>%
      filter(id %in% index_id) %>%
      bind_rows(template_images) %>%
      mutate(image_n = as.numeric(image_n)) %>%
      select(all_of(list_name_images)) %>%
      arrange(image_id) %>%
      mutate(
        photo_type =
          case_when(image_n == min(image_n) ~ "Start", 
                    image_n == max(image_n) ~ "End", 
                    TRUE ~ photo_type))

    # Flag known image errors  ----
    #      Add "TRUE" flag to error column
    #      Define photo_type as corrupt
    #      Add "N/A" to binomial and catalog_by columns  
    #      Add error_text to comments
    
    # Create image_check based on presence/absence of problems  
    #    When survey DOES NOT HAVE e a problem ----
    if(id_has_problems == FALSE){
      
      image_check <- subset_files
      
    }
    
    #    When this survey HAS a known problem ----
    if(id_has_problems == TRUE){
      
      # Subset to problem images and error text
      subset_problems <-
        problem_images %>%
        filter(id %in% index_id)
      
      list_image_n_problems <- unique(subset_problems$image_n)
      
      error_text <- unique(subset_problems$error_text)
      
      last_image_n <- max(subset_files$image_n)
      
      image_check <- 
        subset_files %>%
        mutate(
          is_error = image_n %in% list_image_n_problems,
          error = ifelse(is_error, TRUE, error),
          photo_type = case_when(
            is_error ~ "Corrupt",
            image_n == last_image_n & is_error ~ "End",
            TRUE ~ photo_type),
          binomial_1 = ifelse(is_error, "N/A", binomial_1),
          comments = ifelse(is_error,
                            paste0("Do not catalog: ", 
                                   error_text), 
                            comments),
          catalog_by = ifelse(is_error, "N/A", catalog_by)
        ) %>%
        select(-is_error) %>%
        arrange(image_id)
    }
    
    # Create a formatted xlsx file ----
    fxn_xlsx_format(index_data = image_check, 
                    index_n = 1,
                    index_type = "blank")
    
    # Move exif table to archive ----
    path_table_exif_file <- paste0(index_id,
                                   "_exif.csv")
    
    if(file.exists(path_table_exif_file)){
      
      file_move(path = here(path_exif,
                            path_table_exif_file), 
                new_path = here(path_exif_archive,
                                path_table_exif_file))
    }
  }
  
}
#   fxn_tidy_for_qc ----
# Create xlsx file tidy image tables (pre-qc)
fxn_tidy_for_qc <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # Get file information for image tables, list files ----
  dir_table <- fxn_dir_table_map(index_site, 
                                 index_type = "catalog")  
  list_files <- unique(dir_table$file_name)
  
  # Iterate by image table  ----
  for(index_file in list_files){
    # index_file <- list_files[1]
    
    cat(str_sub(index_file, 1, 11), "\n")

    # Define index_n (binomial_, count_) ----
    index_path <- here(path_table_catalog, index_file)
    
    index_n <- 
      fxn_wrap_read_excel(index_path) %>%
      fxn_define_index_n()
    
    # Create helpers  ----
    subset <- 
      dir_table %>%
      filter(file_name %in% index_file) %>%
      mutate(index_n = index_n)
    
    index_id <- subset$id
    index_camera <- str_sub(index_id, 1, 4)
    
    # data_raw: Image table -----
    data_raw <-
      fxn_table_read_xlsx(index_file = index_file, 
                          index_type = "catalog")  %>%
      mutate(image_id = str_remove(image_id, 
                                   ".JPG"),
             image_file = str_remove(image_file,
                                     "IMG-")) %>%
      fxn_add_flags()
     
    # data_tidy: Tidy values for binomial and count  ----
    data_tidy <- 
      fxn_tidy_binomial_count(index_data = data_raw,
                              index_type = "catalog", 
                              index_n = index_n)  %>%
      
      # Revise any id with the old format 
      rename(id_init = id) %>%
      mutate(id = index_id) %>%
      mutate(image_id = 
               str_replace(image_id, 
                           id_init, 
                           id), 
             image_file = 
               str_replace(image_file, 
                           id_init, 
                           id)) %>%
      select(-id_init)
    
    # subset_for_qc: Working subset for qc script ----
    exclude_maintenance <- 
      data_tidy %>%
      filter(
        # Exclude all maintenance photo_types 
        photo_type %nin% c("Start",
                           "Pickup", 
                           "Setup", 
                           "End",
                           "Maintenance"), 
        # Exclude images already flagged by catalogers
        catalog_flag == FALSE, 
        # Exclude images already flagged by this script 
        # (special species, unidentifiable)
        review == FALSE
      ) 
    
    #   If no images need to be flagged (RARE!) ----
    if(nrow(exclude_maintenance) == 0){
      data_output <- 
        data_tidy %>%
        select(-catalog_flag)
    } 
    
    #   If images need to be flagged (ALMOST ALWAYS!) ----
    if(nrow(exclude_maintenance) > 0){
      
      subset_for_qc <- 
        exclude_maintenance %>%
        # Rename blanks in binomial_1 for easier data handling
        mutate(binomial_1 = ifelse(photo_type %in% "Blank", 
                                   "Blank", binomial_1))  %>%
        # Create indexes for easier data handling (joins)
        unite(id_image_n,
              c(id,image_n), 
              remove = FALSE) %>%
        unite(id_photo_type,
              c(id,photo_type),
              remove = FALSE) %>%
        arrange(id, image_n)
      
      # Identify additional images for visual qc ----
      if(nrow(subset_for_qc) < 100){
        
        flag_for_qc <- 
          subset_for_qc %>%
          unite(id_image_n, 
                c(id, image_n)) %>%
          select(id_image_n) %>%
          mutate(qc_image = TRUE)
      } else{
        
        flag_for_qc <- 
          fxn_flag_for_qc(index_data = subset_for_qc) %>%
          select(id_image_n,
                 qc_image)
        
      }
      
      # Revise the tidy table with images flagged for qc ----
      data_output <- 
        data_tidy %>%
        unite(id_image_n,
              c(id, 
                image_n), 
              remove = FALSE) %>%
        left_join(flag_for_qc,
                  "id_image_n")  %>%
        
        # Update review column with new images flagged for QC
        mutate(review = 
                 case_when(
                   qc_image == TRUE ~ TRUE, 
                   TRUE ~ review), 
               image_id = str_remove(image_id,
                                     ".JPG")) %>%
        select(-qc_image, 
               -catalog_flag, 
               -id_image_n)  %>%
        select(all_of(list_column_names))
    }
    
    
    # Write as .xlsx ----
    fxn_xlsx_format(index_data = data_output, 
                    index_n = index_n,
                    index_type = "tidy")
    
    # Move blank table to archive ----
    path_file_blank <- paste0(index_id, "_blank.xlsx")
    
    if(file_exists(here(path_table_blank,
                        path_file_blank))){
      
      file_move(path = here(path_table_blank,
                            path_file_blank), 
                new_path = here(path_table_blank_archive,
                                path_file_blank)
      )
    }
    
    # Move cataloged table to archive ----
    path_file_catalog <- paste0(index_id, "_Final.xlsx")
    
    if(file_exists(here(path_table_catalog,
                        path_file_catalog))){
      file_move(path = here(path_table_catalog,
                            path_file_catalog), 
                new_path = here(path_table_catalog_archive,
                                path_file_catalog)
      )
    }
  }
}

#   fxn_tidy_for_vault ----
fxn_tidy_for_vault <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # Get file information, list files ----
  dir_table <-
    fxn_dir_table_map(index_site,
                      index_type = "clean") %>%
    # Exclude any _final_tidy files
    filter(str_detect(file_name, "qc"))  %>%
    mutate(qc_initials_lower = str_sub(file_name,
                                       27, 28)) %>%
    left_join(lookup_qc_initials, "qc_initials_lower")
  
  
  # For legacy data 
  # dir_table <-
  #   fxn_dir_table_map(index_site,
  #                     index_type = "qc")  %>%
  #   left_join(dlog %>%
  #               select(id, 
  #                      qc_initials = qc_by), "id") %>%
  #   left_join(lookup_qc_initials, "qc_initials")
  
  list_files <- unique(dir_table$file_name)
  
  # Iterate by image table  ----
  for(index_file in list_files){
    # index_file <- list_files[1]
    
    index_id <- str_sub(index_file, 1, 11)
    cat(index_id, "\n", "...start", "\n")
    # Define index_n (binomial_, count_) ----
    index_path <- here(path_table_qc, index_file)
    
    index_n <- 
      fxn_wrap_read_excel(index_path) %>%
      fxn_define_index_n()
    
    # Create helpers  ----
    subset <- 
      dir_table %>%
      filter(file_name %in% index_file) %>%
      mutate(index_n = index_n)
    
    index_file <- subset$file_name
    index_qc_by <-  subset$qc_by
    
    # data_raw: Image table -----
    data_raw <-
      fxn_table_read_xlsx(index_file = index_file, 
                          index_type = "qc") %>%
      remove_empty("rows") %>%
      mutate(image_id = str_remove_all(image_id, ".JPG"), 
             image_id = str_remove_all(image_id, "IMG-"), 
             image_file = str_remove_all(image_file, "IMG-")) %>%
      distinct() %>%
      fxn_add_flags() %>%
      fxn_revise_qc_by_for_sentence_case() 
    
    # data_tidy: Tidy values for binomial and count  ----
    data_tidy <- 
      fxn_tidy_binomial_count(index_data = data_raw,
                              index_type = "qc", 
                              index_n = index_n) %>%
      rename(id_init = id) %>%
      mutate(id = index_id) %>%
      mutate(image_n_pad = str_pad(image_n,
                                   width = 5,
                                   pad = "0",
                                   side = "left"),
             image_n_jpg = paste0("_",
                                  image_n_pad,
                                  ".JPG"),
             image_file = paste0(id,
                                 image_n_jpg),
             
             image_id = str_remove(image_file,
                                   ".JPG")) %>%
      select(-id_init, 
             -image_n_pad, 
             -image_n_jpg)  
   
    first_image_n <- min(data_tidy$image_n, na.rm = TRUE)
    last_image_n <- max(data_tidy$image_n, na.rm = TRUE)
    
    # Revise the qc table ----
    data_output <- 
      data_tidy %>%
      
      # Replace NA with N/A for non-Animal photo_type 
      mutate(
        binomial_1 = 
          case_when(
            is.na(binomial_1) & 
              photo_type %in% c("Blank", 
                                "Start", 
                                "End", 
                                "Setup", 
                                "Pickup",
                                "Unidentifiable", 
                                "Corrupt") ~
              "N/A", 
            TRUE ~ binomial_1), 
        
        # Correct N/A spelling 
        binomial_1 = str_replace(binomial_1, 
                                 "N/a", 
                                 "N/A"), 
        
        # Correct "Unidentifiable" in binomial_1 
        photo_type = 
          case_when(
            binomial_1 == "Unidentifiable" &
              qc_certainty == "Absolutely sure" ~ 
              "Unidentifiable", 
            TRUE ~ photo_type), 
        binomial_1 = 
          case_when(
            binomial_1 == "Unidentifiable" &
              qc_certainty == "Absolutely sure" ~
              "N/A", 
            TRUE ~ binomial_1), 
        
        # # Correct Blank count 
        # count_1 = 
        #   case_when(
        #     count_1 = 1 &
        #       photo_type %in% "Blank" &
        #       qc_certainty %in% "Absolutely sure" ~
        #       0, 
        #     TRUE ~ count_1), 
        
        # Add missing Start and End in photo_type
        photo_type =
          case_when(
            image_n == last_image_n & 
              binomial_1 == "Unidentifiable" &
              qc_certainty == "Absolutely sure"  ~  "End",
            image_n == last_image_n & photo_type %nin% "End" ~ "End", 
            image_n == first_image_n & photo_type %nin% "Start" ~ "Start",
            TRUE ~ photo_type), 
        
        image_id = str_remove(image_id, ".JPG")) %>% 
      select(any_of(list_column_names)) 
    
    # Write as .xlsx ----
    fxn_xlsx_format(index_data = data_output, 
                    index_type = "clean",
                    index_n = 1) 
    
    # Move qc table to archive ----
    file_move(path = here(path_table_qc,
                          index_file), 
              new_path = here(path_table_qc_archive,
                              index_file))
    
    # Move final_tidy table to archive ----
    tidy_file <- paste0(index_id, "_final_tidy.xlsx")
    
    if(file.exists(here(path_table_qc,
                        tidy_file))){
      file_move(path = here(path_table_qc,
                            tidy_file), 
                new_path = here(path_table_qc_archive,
                                "image-tables_final_tidy", 
                                tidy_file
                ))
    }
    
    # Show completion message ----
    cat(" ...done", "\n")
  }
}

# ========================================================== -----

# CONFIGURE XLSX OUTPUT ----
# Define options for date, datetime 
options("openxlsx.dateFormat" = "mm/dd/yy")
options("openxlsx.datetimeFormat" = "yyyy-mm-dd hh:mm:ss")
#
# Create worksheet templates ----
#   template_images: Template (empty) for the "Images" sheet 
# Start the Images template as an empty table with column names 
# This lacks all exif info (id, date, time, datetime, image_id, image_n, file_name)
template_images <- 
  tibble(photo_type = character(),
         binomial_1 = character(),
         count_1 = numeric(),
         comments = character(),
         catalog_by = character(),
         review = character(),
         good = character(),
         error = character(),
         qc_by = character(),
         qc_certainty = character(),
         binomial_2 = character(),
         count_2 = numeric(), 
         binomial_3 = character(),
         count_3 = numeric())
# Define cell properties ----
# Create a lookup table containing the xlsx column properties  
# Use the lookup table to list the column properties to apply to each template
lookup_cell_properties <- 
  read_csv(here(path_in,
                "image-table_column-info.csv"), 
           col_types = cols()) %>%
  arrange(sheet, column_order)

# Subset the lookup table to eliminate a filter for each list
lookup_cell_properties_images <- 
  lookup_cell_properties %>%
  filter(sheet %in% "images")

# List the column names
list_name_images <- 
  lookup_cell_properties_images %>%
  pull(column_name)

# List the column widths
list_width_images <- 
  lookup_cell_properties_images %>%
  pull(column_width)

# List the qc columns to hide
list_hide_images_qc <- 
  lookup_cell_properties_images %>%
  filter(hide_blank == TRUE, 
         str_detect(column_name, "qc")) %>%
  pull(column_order)

# List additional columns to hide
# These hidden columns need to be specified separately; not adjacent to hidden qc columns 
list_hide_images_other <- 
  lookup_cell_properties_images %>%
  filter(hide_blank == TRUE, 
         !str_detect(column_name, "qc")) %>%
  pull(column_order)

# List columns to hide in final table
list_hide_images_clean <- 
  lookup_cell_properties_images %>%
  filter(hide_clean == TRUE) %>%
  pull(column_order)

# List the columns to format as date
list_date_images <- 
  lookup_cell_properties_images %>%
  filter(column_format %in% "date") %>%
  pull(column_order)

# List the columns to format as datetime
list_datetime_images <- 
  lookup_cell_properties_images %>%
  filter(column_format %in% "datetime") %>%
  pull(column_order)

# Define the number of named columns 
count_cols <- 
  lookup_cell_properties %>%
  filter(sheet %in% "images") %>%
  nrow()

list_width_metadata <<- 
  lookup_cell_properties %>%
  filter(sheet %in% "metadata") %>%
  pull(column_width)

# Define where to start erasing columns (for output) 
#   Extra columns are erased to restrict the use of filters to named columns 
#   Otherwise the sheet is super wide and all columns have filters
erase_cols_from <- count_cols + 1

# Create helper functions for fxn_xlsx_format ----
# Helper functions to keep the main function tidy and modular
create_lookup_path_out <- function(index_id, index_type){
  
  lookup_table <- 
    tibble(type = c("blank", "catalog", "tidy", "clean",
                    "migrate_qc", "migrate_catalog"),
           folder = c("blank", "catalog", "qc", "clean", "qc", "catalog"),
           suffix = c("blank", "final", "final_tidy", "clean",
                      "final_qc", "final"), 
           path_folder = paste0("path_table_", folder), 
           file_name = paste0(paste(index_id, 
                                    suffix, 
                                    sep = "_"), 
                              ".xlsx")) %>%
    mutate(path_folder = str_replace(path_folder,
                                     "path_table_clean",
                                     "path_vault")) %>%
    filter(type %in% index_type) %>%
    select(path_folder, file_name)

}

create_default_style <- function() {
  # Define cell format (default)
  body_style <- 
    createStyle(halign = "LEFT",
                fontSize = 11, 
                border = "TopBottomLeftRight", 
                borderColour = openxlsx_getOp("borderColour",
                                              "black"),
                borderStyle = openxlsx_getOp("borderStyle",
                                             "thin"))
  
}

create_image_id_style <- function() {
  # Define cell format for image_id with right-aligned text
  body_style_image_id <- 
    createStyle(halign = "RIGHT",
                fontSize = 11, 
                border = "TopBottomLeftRight", 
                borderColour = openxlsx_getOp("borderColour", 
                                              "black"),
                borderStyle = openxlsx_getOp("borderStyle", 
                                             "thin"))
}

create_erase_style <- function() {
  # Define format to use for erased cells
  erase_style <- 
    createStyle(halign = "LEFT",
                fontSize = 11, 
                borderStyle = "none")
}

create_header_style <- function(sheet_type) {
  
  if(sheet_type == "Images"){
    # Define header format for Images sheet
    header_style <- 
      createStyle(textDecoration = "Bold", 
                  halign = "LEFT",
                  fontSize = 11,
                  border = "TopBottomLeftRight", 
                  borderColour = openxlsx_getOp("borderColour", 
                                                "black"),
                  borderStyle = openxlsx_getOp("borderStyle", 
                                               "thin"))
  }else if(sheet_type == "Metadata"){
    # Define header format for Metadata sheet
    header_style <-
      createStyle(textDecoration = "Bold", 
                  halign = "LEFT",
                  fontSize = 11, 
                  border = "bottom")
  }
}

add_sheets_and_write_data <- function(wb, df_subset_files, index_id, 
                                      validation_lists_subset) {
  # Create, populate Images sheet 
  addWorksheet(wb, "Images")
  writeData(wb, 
            "Images", 
            df_subset_files,
            startRow = 1, 
            startCol = 1, 
            withFilter = TRUE)
  
  # Create, populate Metadata sheet 
  addWorksheet(wb, "Metadata")
  writeData(wb, 
            "Metadata", 
            template_metadata, 
            startRow = 1, 
            startCol = 1, 
            withFilter = FALSE)
  
  # Create, populate Validation sheet 
  addWorksheet(wb, 
               "Validation", 
               visible = FALSE)
  
  validation_lists_subset_thin <- 
    validation_lists_subset %>% 
    select(value, list)
  
  writeData(wb, 
            "Validation",
            x = validation_lists_subset_thin)
}

create_validation_lists_subset <- function(index_type){
  
  if(index_type == "blank"){
    
    validation_lists_subset <-
      validation_lists %>%
      filter(value %nin% "Unidentifiable") %>%
      arrange(list, sort) %>%
      rowid_to_column("row") %>%
      relocate(value, list)
    
  }else{
    validation_lists_subset <-
      validation_lists %>%
      filter(value %nin% "Unknown") %>%
      rowid_to_column("row") %>%
      relocate(value, list) 
  }
}

apply_data_validations <- function(wb, count_rows, validation_lists_subset) {
  # Create validation lists  ----

  validation_list_values <- 
    validation_lists_subset %>%
    group_by(list) %>%
    summarize(from = min(row) + 1, 
              to = max(row) + 1) %>%
    mutate(cells = paste0("'Validation'!$A$", 
                          from, 
                          ":$A$", 
                          to))
  
  cells_binomial <- 
    validation_list_values %>%
    filter(list == "binomial") %>%
    pull(cells) 
  
  cells_photo_type <- 
    validation_list_values %>%
    filter(list == "photo_type") %>%
    pull(cells) 
  
  cells_qc_certainty <- 
    validation_list_values %>%
    filter(list == "qc_certainty") %>%
    pull(cells)
  
  cells_logical <- 
    validation_list_values %>%
    filter(list == "logical") %>%
    pull(cells)
  
  # Define validation by column  ----
  # Validation for photo_type
  dataValidation(wb, 
                 "Images", 
                 col = 4,
                 rows = 1:count_rows,
                 type = "list", 
                 value = cells_photo_type)
  
  # Validation for binomial
  dataValidation(wb, 
                 "Images", 5,
                 list_cols_binomial,
                 rows = 1:count_rows,
                 type = "list", 
                 value = cells_binomial)
  
  dataValidation(wb, 
                 "Images", 14,
                 list_cols_binomial,
                 rows = 1:count_rows,
                 type = "list", 
                 value = cells_binomial)
  
  dataValidation(wb, 
                 "Images", 16,
                 list_cols_binomial,
                 rows = 1:count_rows,
                 type = "list", 
                 value = cells_binomial)
  
  # Validation for good, review, error 
  dataValidation(wb, 
                 "Images", 
                 col = 9:11,
                 rows = 1:count_rows,
                 type = "list", 
                 value = cells_logical)
  
  # Validation for qc_certainty
  dataValidation(wb, 
                 "Images", 
                 col = 13,
                 rows = 1:count_rows,
                 type = "list", 
                 value = cells_qc_certainty)
}

format_workbook <- function(wb, count_rows, count_cols) {
  # Format workbook (freeze panes, set column widths) here
  # Freeze the top row in the Images sheet
  freezePane(wb, 
             "Images",
             firstActiveRow = 2)
  
  # Set column width in the Images sheet
  setColWidths(wb, 
               "Images", 
               cols = 1:count_cols, 
               widths = list_width_images)
  
  # Set column width in the Metadata sheet
  setColWidths(wb, 
               "Metadata", 
               cols = 1:count_cols, 
               widths = list_width_metadata)
}

# Handle different index_types 
handle_index_type <- function(wb, index_type, index_n, index_id) {
  
  if(index_type %in% c("blank", "catalog")){
    
    # Hide qc columns in the Images sheet
    groupColumns(wb, 
                 "Images", 
                 cols = list_hide_images_qc, 
                 hidden = TRUE)
    
    # Hide other columns in the Images sheet
    groupColumns(wb,
                 "Images",
                 cols = list_hide_images_other,
                 hidden = TRUE)

  }
  if(index_type %in% c("tidy", "migrate_clean", "migrate_qc")){
    # Use index_n to define unused binomial, count columns
    if(index_n == 1){
      list_hide_images_tidy <- list_hide_images_tidy_1
    }
    if(index_n == 2){
      list_hide_images_tidy <- list_hide_images_tidy_2
    }
    if(index_n == 3){
      list_hide_images_tidy <- list_hide_images_tidy_3
    }
    
    groupColumns(wb,
                 "Images",
                 cols = list_hide_images_tidy,
                 hidden = TRUE)
  
  }
  if(index_type == "clean"){
    
    # Hide other columns in the Images sheet
    groupColumns(wb,
                 "Images",
                 cols = list_hide_images_clean,
                 hidden = TRUE)
    
   
  }
}

apply_styles_to_workbook <- function(wb, body_style, body_style_image_id, erase_style, header_style_images, header_style_metadata, count_rows, count_cols) {
  
  erase_rows_from <- count_rows + 1
  erase_cols_from <- count_cols + 1
  
  # DO NOT CHANGE THE ORDER OF addStyle CODE!!!
  # (unless you know what you are doing)
  # The order in which styles are applied matters
  #
  # Apply default body style to Images sheet
  addStyle(wb, 
           "Images", 
           style = body_style, 
           rows = 1:count_rows, 
           cols = 1:count_cols, 
           gridExpand = TRUE, 
           stack = FALSE)
  
  # Add right-aligned format for image_id to Images sheet
  addStyle(wb, 
           "Images", 
           style = body_style_image_id, 
           rows = 2:count_rows, 
           cols = 1, 
           gridExpand = FALSE, 
           stack = FALSE)
  
  # Erase styles outside data table on Images sheet
  addStyle(wb, 
           "Images", 
           style = erase_style, 
           rows = erase_rows_from:(erase_rows_from+count_rows), 
           cols = erase_cols_from:(erase_cols_from+count_cols),
           gridExpand = TRUE, 
           stack = TRUE)
  
  # Add images header format to Images sheet
  addStyle(wb, 
           "Images", 
           style = header_style_images, 
           rows = 1, 
           cols = 1:count_cols,
           gridExpand = TRUE, 
           stack = TRUE)
  
  # Add metadata header format to Metadata sheet
  addStyle(wb, 
           "Metadata", 
           style = header_style_metadata, 
           rows = 1, 
           cols = 1:count_cols,
           gridExpand = TRUE, 
           stack = TRUE)
  
  # Format date column in Images sheet 
  addStyle(wb, 
           "Images", 
           style = createStyle(numFmt = "DATE"), 
           rows = 2:count_rows, 
           cols = list_date_images, 
           gridExpand = TRUE, 
           stack = TRUE)
  
  # Format date_time column in  Image sheet
  addStyle(wb, 
           "Images", 
           style = createStyle(numFmt = "LONGDATE"), 
           rows = 2:count_rows, 
           cols = list_datetime_images, 
           gridExpand = TRUE, 
           stack = TRUE)
}

save_final_workbook <- function(wb, index_path_out) {
  saveWorkbook(wb, 
               file = index_path_out, 
               overwrite = TRUE,
               returnValue = FALSE)
}
#   fxn_xlsx_format ----
fxn_xlsx_format <- function(index_data, index_type, index_n) {
  require(openxlsx)  # Ensure openxlsx package is available
  
  index_id <- unique(index_data$id)
  df_subset_files <- as.data.frame(index_data) 
  count_rows <- nrow(df_subset_files) + 1
  count_cols <- ncol(df_subset_files)  
  lookup_path_out <- create_lookup_path_out(index_id, index_type)
  path_folder <- get(lookup_path_out$path_folder)
  file_name <- lookup_path_out$file_name
  index_path_out <- here(path_folder, file_name)

  # Define styles for the workbook
  body_style <- create_default_style()
  body_style_image_id <- create_image_id_style()
  erase_style <- create_erase_style()
  header_style_images <- create_header_style("Images")
  header_style_metadata <- create_header_style("Metadata")
  
  # Create and format workbook
  wb <- createWorkbook()
  validation_lists_subset <- create_validation_lists_subset(index_type)
  add_sheets_and_write_data(wb, df_subset_files, index_id, validation_lists_subset)
  apply_data_validations(wb, count_rows, validation_lists_subset)
  format_workbook(wb, count_rows, count_cols)
  handle_index_type(wb, index_type, index_n, index_id)
  
  # Apply styles
  apply_styles_to_workbook(wb, 
                           body_style,
                           body_style_image_id, 
                           erase_style, 
                           header_style_images,
                           header_style_metadata, 
                           count_rows,
                           count_cols)
                       
  # Save workbook
  save_final_workbook(wb, index_path_out)
}
# ---------------------------------------------------------- -----
# _BLANK ----
#   fxn_table_exclude_lookup ----
# Define image range, error text 
fxn_table_exclude_lookup <- function(index_data){
  
  # Function to reshape and create id_n for exclusion sets
  process_excl_cols <- function(prefix) {
    index_data %>%
      select(id, starts_with(prefix)) %>%
      pivot_longer(cols = starts_with(prefix), 
                   names_to = "index_col",
                   values_to = "img_val") %>%
      filter(img_val > 0) %>%
      mutate(
        prob_n = as.numeric(str_remove(index_col, paste0(prefix, "_"))),
        id_n = paste(id, prob_n, sep = "_")
      ) %>%
      distinct(id, id_n, prob_n, img_val)
  }

  # Process 'from' and 'to' exclusion sets
  from_data <- process_excl_cols("n_img_excl_from")
  to_data <- process_excl_cols("n_img_excl_to") %>%
    select(id_n, img_val)
  
  # Create lookup table
  lookup_table <- 
    index_data %>%
    mutate(error_text = paste(error_type, 
                              error_subtype,
                              sep = ", ")) %>%
    select(id, error_text) %>%
    distinct() %>%
    left_join(from_data, by = "id") %>%
    rename(img_from = img_val) %>%
    left_join(to_data, by = "id_n") %>%
    rename(img_to = img_val) %>%
    arrange(id_n) %>%
    filter(img_from > 0) %>%
    select(id, id_n, prob_n, img_from, img_to, error_text)
  
  return(lookup_table)
}

# ---------------------------------------------------------- -----
# _FINAL -----
#   fxn_add_flags ----
#   Add cataloger flags (review, good, error) 
#   Convert any value to "Y" 
# index_data <- data_raw
fxn_add_flags <- function(index_data){
  
  # Helper function to convert to logical and handle uppercasing
  to_logical_upper <- function(column) {
    case_when(
      str_to_upper(column) %in% c("X", "Y", "YES",
                                  "YE", "TRUE", "T", 
                                  "TR", "TRU") ~ TRUE,
      TRUE ~ FALSE
    )
  }
  
  # Convert review, error, good columns to uppercase
  # Standardize as logical values
  index_data %>%
    mutate(review = to_logical_upper(review),
           error = to_logical_upper(error),
           good = to_logical_upper(good)) %>%
    
    # Set a flag for items needing review or marked as error
    mutate(catalog_flag = review | error) %>%
    
    # Add annotations to comments based on review and error flags
    mutate(
      comments = str_squish(comments),
      comments_review = 
        ifelse(review, 
               "NEEDS REVIEW: flagged for expert review",
               NA_character_),
      comments_error = 
        ifelse(error, 
               "ERROR: error flagged by cataloger",
               NA_character_)
    ) %>%
    
    # Combine the review and error comments with existing comments
    unite(comments, 
          c("comments_review", "comments_error", "comments"), 
          sep = "; ", 
          na.rm = TRUE)  %>%
    
      # Remove trailing spaces from comments
    fxn_drop_end_space()
}

#   fxn_drop_end_space  ----
fxn_drop_end_space <- function(index_data){
  
  # Function to remove trailing "; "
  remove_trailing_semicolon <- function(comments) {
    return(ifelse(grepl("; $", comments), 
                  sub("; $", "", comments), 
                  comments))
  }
 
  # Function to remove leading "; "
  remove_leading_semicolon <- function(comments) {
    return(ifelse(grepl("^; ", comments), 
                  sub("^; ", "", comments),
                  comments))
  }
  
  # Function to replace "" with NA_character_ ----
  replace_quotes_with_NA <- function(comments){
    return(ifelse(comments %in% c("N/A", ""),
                  NA_character_, 
                  comments))
  }
  
  index_data %>%
    mutate(comments = remove_trailing_semicolon(comments)) %>%
    mutate(comments = remove_leading_semicolon(comments)) %>%
    mutate(comments = replace_quotes_with_NA(comments))
  
}


#   fxn_fix_typos ----
fxn_fix_typos <- function(index_data){
  
  # Helper function to correct common typos
  correct_common_typos <- function(text) {
    text %>%
      str_to_sentence() %>%
      str_squish() %>%
      str_replace_all(c("Aminal" = "Animal", 
                        "Amimal" = "Animal",
                        "Needsid" = "Needs ID",
                        "N/a" = "N/A",
                        "Needs id" = "Needs ID")) 
  }
  
  index_data %>%
    clean_names() %>%
    mutate(
      # Clean up 'photo_type' and correct typos
      photo_type = str_remove_all(photo_type, "\\s"),
      photo_type = correct_common_typos(photo_type),
      
      # Clean up 'binomial' columns and correct typos
      binomial_1 = correct_common_typos(binomial_1),
      binomial_2 = correct_common_typos(binomial_2),
      binomial_3 = correct_common_typos(binomial_3),
      
      # Title case for 'catalog_by', 'qc_by'; sentence case for 'qc_certainty'
      catalog_by = str_to_title(catalog_by),
      qc_by = str_to_title(qc_by),
      qc_by = if_else(qc_by == "Vf", "Viviane Fuller", qc_by),
      qc_certainty = correct_common_typos(qc_certainty),
      
      # Get first, last image numbers for 'photo_type' logic
      first_image_n = min(image_n, na.rm = TRUE),
      last_image_n = max(image_n, na.rm = TRUE)
    ) %>%
    mutate(
      # Adjust 'photo_type' based on image number
      photo_type = case_when(
        image_n == first_image_n & photo_type == "Unidentifiable" ~ photo_type,
        image_n == first_image_n ~ "Start",
        image_n == last_image_n & photo_type == "Unidentifiable" ~ photo_type,
        image_n == last_image_n ~ "End",
        TRUE ~ photo_type
      )
    ) %>%
    select(-first_image_n, -last_image_n)  # Remove the helper columns
  
}

#   fxn_revise_qc_by_for_sentence_case ----
fxn_revise_qc_by_for_sentence_case <- function(data){

check_qc_by <- 
  data %>%
  drop_na(qc_by) %>%
  mutate(nchar_qc_by = nchar(qc_by)) %>%
  filter(nchar_qc_by == 2)

if(nrow(check_qc_by) > 0){
  
  updated_qc_by <- 
    data %>%
    mutate(qc_initials_lower = str_to_lower(qc_by)) %>%
    rename(qc_by_init = qc_by) %>%
    left_join(lookup_qc_initials %>%
                select(qc_by, qc_initials_lower), 
              "qc_initials_lower") %>%
    select(-qc_by_init, 
           -qc_initials_lower)
  
  
}else{
  updated_qc_by <- data
}
return(updated_qc_by)
}
# ---------------------------------------------------------- -----
# _TIDY ----
#   fxn_revise_binomial_1 ----
fxn_revise_binomial_1 <- function(index_data, 
                                  index_type){
  
  if(index_type %in% "catalog"){
    join_data <- 
      index_data %>%
      unite(photo_type_binomial,
            c(photo_type, binomial_1)) %>%
      # Join with known photo_type_binomial combinations
      left_join(lookup_photo_type_binomial,
                "photo_type_binomial") %>%
      # Add flag to the review column
      mutate(review = ifelse(!is.na(error_message), 
                             TRUE, review)) %>%
      distinct() %>%
      unite(comments, 
            c(error_message, 
              comments,
              comments_rev), 
            sep = "; ", 
            na.rm = TRUE) 
    
  }
  if(index_type %in% "qc"){
    join_data <- 
      index_data %>%
      unite(photo_type_binomial, 
            c(photo_type, binomial_1)) %>%
      # Join with known photo_type_binomial combinations
      left_join(lookup_photo_type_binomial %>%
                  select(-error_message, 
                         -photo_type),
                "photo_type_binomial") %>%
      rename(photo_type = photo_type_qc) %>%
      distinct() %>%
      unite(comments, 
            c(comments,
              comments_rev), 
            sep = "; ", 
            na.rm = TRUE) 
    
  }
  
  join_data %>%
    fxn_drop_end_space() %>% 
    rename(binomial_1 = binomial) %>%
    relocate(any_of(list_name_images)) %>%
    select(-photo_type_binomial)
  
}
#   fxn_revise_count_1  ----
fxn_revise_count_1 <- function(index_data){
  
  index_data %>%
    mutate(
      # Convert NA to 0 for easier data manipulation
      count_1 = ifelse(is.na(count_1), 0, count_1),
      # Revise count for cows (should be 0 or NA)
      count_1 = ifelse(binomial_1 %in% "Bos taurus" &
                         count_1 > 0,
                       0,
                       count_1),
      
      # Flag Animal images without a count (either NA or 0)
      check_count = ifelse(binomial_1 %nin% "Bos taurus" &
                             photo_type %in% "Animal" &
                             count_1 < 1,
                           "ADD COUNT: animal missing count",
                           NA),
     
      # Flag Blank images with a count (should be NA)
      check_count = ifelse(photo_type %in% "Blank" &
                             count_1 > 0,
                           "CONFIRM COUNT: blank with count",
                           check_count),
      
      # Add flag to the review column
      review = ifelse(!is.na(check_count),
                      TRUE,
                      review)
      ) %>%
    
    unite(comments, 
          c(check_count, 
            comments),
          na.rm = TRUE, 
          sep = "; ") %>%
    fxn_drop_end_space()
  
}

#   fxn_revise_binomial_n: binomial_2, _3 ----
# index_data = data_tidy_1
# index_type = "catalog"
# index_binomial = "binomial_2"
fxn_revise_binomial_n <- function(index_data,  
                                  index_column_n, 
                                  index_type){
  
  index_binomial <- paste0("binomial_", index_column_n)
  
  add_row <- 
    tibble(
      orig_binomial = c("Unidentifiable", 
                        "Unidentifiable animal", 
                        "Unknown", 
                        "Unknown animal", 
                        "Animal"),
      binomial = c("Unidentifiable", 
                   "Unidentifiable", 
                   "Unknown", 
                   "Unknown", 
                   "Unknown"),
      error_message = c(
        paste0("CONFIRM ID: unidentifiable ", 
               index_binomial), 
        paste0("CONFIRM ID: unidentifiable ", 
               index_binomial), 
        paste0("CONFIRM ID: unknown ", 
               index_binomial, 
               " flagged for expert review"), 
        paste0("CONFIRM ID: unknown ", 
               index_binomial, 
               " flagged for expert review"), 
        paste0("CONFIRM ID: unknown ", 
               index_binomial, 
               " flagged for expert review")
      ))
  
  lookup_binomial_n <- 
    lookup_binomial %>%
    # bind_rows(add_row) %>%
    distinct()
  
  join_data <- 
    index_data %>%
    rename(!!"orig_binomial":=all_of(index_binomial)) %>%
    # Join with known photo_type_binomial combinations
    left_join(lookup_binomial_n,
              "orig_binomial")   
  
  
  if(index_type %in% "catalog"){
    add_flag <- 
      join_data %>%
      # Add flag to the review column
      mutate(review = ifelse(!is.na(error_message), 
                             TRUE, review))
  }
  if(index_type %in% "qc"){
    add_flag <- join_data
  }
  
  add_flag %>%
    unite(comments, 
          c(error_message, 
            comments,
            comments_rev), 
          sep = "; ", 
          na.rm = TRUE) %>%
    fxn_drop_end_space() %>% 
    rename(!!index_binomial:=binomial) %>%
    relocate(any_of(list_name_images)) %>%
    select(-orig_binomial)
  
}
#   fxn_revise_count_n: count_2, _3 ----
# index_data = data_tidy
fxn_revise_count_n <- function(index_data,
                               index_column_n){
  
  index_count <- paste0("count_", index_column_n)
  index_binomial <- paste0("binomial_", index_column_n)
  
  index_data %>%
    rename(!!"index_count":=all_of(index_count)) %>%
    rename(!!"index_binomial":=all_of(index_binomial)) %>%
    mutate(
      # Convert NA to 0 for easier data manipulation
      index_count = 
        ifelse(is.na(index_count), 
               0, 
               index_count),
      # Revise count for cows (should be 0 or NA)
      index_count = 
        ifelse(index_binomial %in% "Bos taurus" &
                 index_count > 0,
               0,
               index_count),
      # Flag images without a binomial but with a count
      check_count = 
        ifelse(photo_type %in% "Blank" &
                 index_count > 0,
               "CONFIRM COUNT: binomial missing but has a count",
               NA),
      # Add flag to the review column
      review = 
        ifelse(!is.na(check_count), 
               TRUE,
               review)) %>%
    # distinct(comments, check_count) %>%
    unite(comments, 
          c(check_count, 
            comments),
          na.rm = TRUE, 
          sep = "; ") %>%
    fxn_drop_end_space() %>%
    rename(!!index_count:=index_count) %>%
    rename(!!index_binomial:=index_binomial)
  
}
#   fxn_tidy_binomial_count ----
fxn_tidy_binomial_count <- function(index_data, 
                                    index_type, 
                                    index_n){
  data_tidy_1 <- 
    index_data %>%
    fxn_revise_binomial_1(index_type) %>%
    fxn_revise_count_1()
  
  if(index_n == 1){
    data_tidy <- data_tidy_1
  }
  if(index_n == 2){
    data_tidy <-
      data_tidy_1 %>%
      fxn_revise_binomial_n(index_type, 
                            index_column_n = 2) %>%
      fxn_revise_count_n(index_column_n = 2)
  }
  if(index_n == 3){
    data_tidy <-
      data_tidy_1 %>%
      fxn_revise_binomial_n(index_type, 
                            index_column_n = 2) %>%
      fxn_revise_count_n(index_column_n = 2) %>%
      fxn_revise_binomial_n(index_type, 
                            index_column_n = 3) %>%
      fxn_revise_count_n(index_column_n = 3)
  }
  
  data_tidy
}
# Define image subsets to flag for review -----
# Note about image counts < 10% target  
#   Problem with spanning the transition between blank, animal 
#   Messes up the counts for the two photo_types 
#   For now: identify more images than needed, then reduce to 10% 
#   Step 4 does the "identify more images" work
#   fxn_target_10 ----
# STEP 1 | Calculate 10% target by id_photo_type 
# Need to get targets for two photo_types in every id: blank, animal
# Hard coded for a run length of 5 images 
fxn_target_10 <- function(index_data){
  
  index_run_length <- 5
  
  calculate_target <- 
    index_data %>%
    group_by(id, 
             photo_type, 
             id_photo_type) %>%
    summarize(img_total = n()) %>%
    mutate(img_target_10 = 
             ceiling(img_total * 0.1),
           # Add buffer to run count; runs not always 5 images 
           run_target_13 = 
             ceiling(img_total * 0.13/index_run_length)) %>%
    select(id_photo_type, 
           img_target_10,
           run_target_13) %>%
    ungroup() 
}
#   fxn_sample_run_transition  ----
# STEP 2 | Add QC flags at transition between image runs 
# Exclude images that have already been flagged 
# Then flag 10% images that are blank or have common species 
# index_data <- subset_for_qc
# index_lookup <- lookup_target_10
# index_run_length <- n_images_in_run
fxn_sample_run_transition <- function(index_data, 
                                      index_lookup){
  
  index_run_length <- 5
  
  subset <-
    index_data %>%
    select(id, 
           photo_type, 
           binomial_1, 
           image_n, 
           starts_with("id")) %>%
    # Create column to hold cumulative run length
    mutate(run_length = NA)
  
  lookup <- 
    index_lookup %>%
    select(id_photo_type, 
           img_target_10, 
           run_target_13)
  
  # 1. Determine run lengths for repeated images  ----
  run_length <- rle(subset$binomial_1)$lengths
  # Append to subset 
  subset$run_length[cumsum(run_length)] <- run_length
  
  # 2. Create a lookup table of image runs ----
  # The resulting table captures the range of each image run
  # This will be used to create the list of image_n
  run_intervals <- 
    subset %>%
    # Remove id_image_n because range of images will map to same image_from 
    rename(image_to = image_n) %>%
    # Subset to runs of at least 5 images
    filter(run_length >= index_run_length) %>%
    group_by(id_photo_type) %>%
    # Create image_from by subtracting (run_length + 1) from image_to
    mutate(image_from = image_to - run_length + 1, 
           run_n = 1:n()) %>%
    ungroup() %>%
    # Prioritize long runs 
    arrange(id_photo_type, 
            desc(run_length)) %>%
    group_by(id_photo_type) %>%
    mutate(run_priority = 1:n()) %>%
    ungroup() %>%
    # Add the 10% target for image count
    left_join(lookup, "id_photo_type") %>%
    unite(id_photo_type_n, c(id_photo_type, run_n), remove = FALSE) %>%
    # Identify runs to include, with priority given to longest runs
    mutate(include_run = ifelse(run_priority <= run_target_13,
                                TRUE, FALSE)) %>%
    select(id, 
           photo_type, 
           binomial_1, 
           image_from, 
           image_to, 
           img_target_10, 
           include_run, 
           starts_with("id"))
  
  # 3. Convert run range to image_n sequence -----
  # Subset to the target number of runs and map image interval  
  image_sequence <- 
    run_intervals %>%
    filter(include_run == TRUE) %>%
    # Convert range to list of images 
    mutate(t_lag0 = image_from, 
           t_lag2 = t_lag0 -2, 
           t_lag1 = t_lag0 -1, 
           t_lead1 = t_lag0 + 1, 
           t_lead2 = t_lag0 +2) %>%
    gather(lag, image_n, c(t_lag0:t_lead2)) %>%
    arrange(id, photo_type, image_n) %>%
    # Add a QC flag 
    mutate(qc_image = TRUE) %>%
    # Create an index to join lookup table
    unite(id_image_n, c(id, image_n)) %>%
    select(id_image_n,
           # image_from,
           # image_to,
           img_target_10,
           qc_image, 
           id_photo_type_n) %>%
    # Join original image table to get all images 
    right_join(subset %>%
                 select(-run_length), "id_image_n") 
  
}
#   fxn_sample_check  ---- 
# STEP 3 | Compare image count, 10% target  
# Subset to surveys that need more images flagged for QC
# Also used for Step 5 to check revised image sequence
fxn_sample_check <- function(index_data, index_lookup){
  
  check <- 
    index_data %>%
    filter(qc_image == TRUE) %>%
    group_by(id_photo_type) %>%
    # Calculate the number of image flagged for QC 
    summarize(img_have = n()) %>%
    # Append target numbers
    left_join(index_lookup, "id_photo_type") %>%
    mutate(img_need = img_target_10 - img_have, 
           img_need = ifelse(img_need >0, img_need, 0)) %>%
    filter(img_need > 0)
}
#   fxn_sample_additional_images  ---- 
# STEP 4 | Sample available images to reach 10% target 
# index_lookup <- lookup_need_additional_images
# index_data <- run_image_sequence_all
# index_id <- list_id[3]
# index_id_photo_type <- list_id_photo_type[2]

fxn_sample_additional_images <- function(index_data, index_lookup){
  
  list_id_photo_type <- unique(index_lookup$id_photo_type)
  # First: Address surveys that need more images ----
  datalist <- list()
  
  for(index_id_photo_type in list_id_photo_type){
    
    # Create data subsets ----
    subset <-
      index_data %>%
      filter(id_photo_type %in% index_id_photo_type)
    # # Subset to images not yet flagged for QC
    # is.na(qc_image))
    
    # Images flagged for QC
    subset_qc <- 
      subset %>%
      filter(qc_image == TRUE)
    
    # Images not flagged for QC
    subset_na <- 
      subset %>%
      filter(is.na(qc_image)) %>%
      select(-qc_image)
    
    # Identify additional images for QC ----
    # List the available images 
    list_available_images <- unique(subset_na$image_n)
    
    # Get the count of needed images to reach 10% target
    n_images <- 
      index_lookup %>%
      filter(id_photo_type %in% index_id_photo_type) %>%
      pull(img_need)
    
    # Set the seed for reproducible sampling
    set.seed(143)
    
    # Sample the available images 
    selected_images <- 
      tibble(image_n = sample(x = list_available_images, size = n_images), 
             qc_image_rev = TRUE) %>%
      mutate(id = str_sub(index_id_photo_type, 1, 11)) %>%
      arrange(id, image_n) %>%
      unite(id_image_n, c(id, image_n)) 
    
    # Revise data table ---- 
    datalist[[index_id_photo_type]] <- 
      # Start with the images not flagged for QC
      subset_na %>%
      # Join new images for QC
      left_join(selected_images, "id_image_n") %>%
      # Bind images already flagged for QC
      bind_rows(subset_qc) %>%
      # Combine original and revised columns for the qc flag
      unite(qc_image, c(qc_image, qc_image_rev), na.rm = TRUE) %>%
      mutate(qc_image = as.logical(qc_image)) %>%
      select(id_image_n, 
             id, 
             photo_type, 
             binomial_1, 
             image_n, 
             qc_image)
    
  }
  
  bind_datalist <- do.call(bind_rows, datalist)
  
  # Second: Bind the revised and unchanged survey tables ----
  revised_data <- 
    index_data %>%
    # Exclude surveys that were revised
    filter(id_photo_type %nin% list_id_photo_type) %>%
    # Add the revised surveys
    bind_rows(bind_datalist)  
  
}

# Combine all subsetting functions ----
#   fxn_flag_for_qc ----
# Hard coded for a run length of 5 images 
# index_data <- subset_for_qc
fxn_flag_for_qc <- function(index_data){
  
  index_run_length <- 5
  
  # Step 1: Calculate 10% targets by id_photo_type ----
  # Need to get targets for two photo_types in every id: blank, animal
  lookup_target_10 <- fxn_target_10(index_data = index_data)
  
  # Step 2: Add QC flags at transition between image runs ----
  images_at_transition <- 
    fxn_sample_run_transition(index_data = index_data,
                              index_lookup = lookup_target_10)
  
  # Step 3: Compare image count and 10% targets ----
  # If enough images are flagged, skip step 4 
  lookup_additional_images <- 
    fxn_sample_check(index_data = images_at_transition, 
                     index_lookup = lookup_target_10)
  
  check_additional <- nrow(lookup_additional_images) 
  if(check_additional == 0){
    images_flagged <- images_at_transition
  }
  if(check_additional > 0){
    # Step 4: Sample available images to reach 10% target ----
    images_after_revision <-
      fxn_sample_additional_images(
        index_data = images_at_transition, 
        index_lookup = lookup_additional_images)
    
    # Step 5: Compare image count and 10% targets ----
    final_sample_check <- 
      fxn_sample_check(index_data = images_after_revision, 
                       index_lookup = lookup_target_10)
    
    check_revision <- nrow(final_sample_check)
    
    # Step 6: Confirm checks and finalize image table ----
    if(check_revision == 0){
      images_flagged <- images_after_revision
    }
  }
  images_flagged
}

# Create helpers for index_n ----
# Revises cell properties specific to _tidy output 
# List additional columns to hide: _2, _3 
# Must specify these columns separately; not next to other hidden columns
#
# For image tables with _1 only 
list_hide_images_tidy_1 <- 
  lookup_cell_properties_images %>%
  filter(hide_qc == TRUE) %>%
  pull(column_order)
#
# For image tables with _2 (binomial_2, count_2)
list_hide_images_tidy_2 <- 
  lookup_cell_properties_images %>%
  filter(hide_qc == TRUE, 
         column_name %nin% c("binomial_2", "count_2")) %>%
  pull(column_order)
#
#
# For image tables with _3 (binomial_3, count_3)
list_hide_images_tidy_3 <- 
  lookup_cell_properties_images %>%
  filter(hide_qc == TRUE, 
         column_name %nin% c("binomial_2",
                             "count_2", 
                             "binomial_3", 
                             "count_2")) %>%
  pull(column_order)

# ========================================================== -----


