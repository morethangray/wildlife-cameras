# ========================================================== -----
# Create helpers ----
#   fxn_wrap_file_rename ----
# Requires input to have new_name (full path and extension)
fxn_wrap_file_rename <- function(index_data){
  
  if(nrow(index_data) > 0) {
    file.rename(from = index_data$path_from,
                to = index_data$path_to)
  }
}
# ---------------------------------------------------------- -----
# Check image files ----
#   fxn_jpg_map_files  ----
# Create map of all jpg files 
# Assigns camera from deployment log 
fxn_jpg_map_files <- function(index_site,
                              index_year,
                              done_rename){
  
  fxn_define_camera_project(index_site)
  
  # List the deployments ----
  if(done_rename == FALSE){
    subset_dlog <- 
      dlog %>%
      filter(done_rename == FALSE)
  }
  if(done_rename == TRUE){
    subset_dlog <- 
      dlog %>%
      filter(done_rename == TRUE)
  }
  
  list_id_rename <- 
    subset_dlog %>%
    filter(done_blank == FALSE,
           has_data != FALSE, 
           year_to == index_year,
           drive_img == "K") %>%
    pull(id)
  
  # Map all jpg files ----
  datalist = list()
  
  # index_id = list_id_rename[1]
  for(index_id in list_id_rename){
    
    lookup <- 
      dlog %>%
      filter(id %in% index_id)
    
    index_path <- unique(lookup$path_server)
    index_camera <- unique(lookup$camera)
    
    datalist[[index_id]] <- 
      tibble(path = 
               dir_ls(path = index_path,
                      recurse = TRUE, 
                      type = "file")) %>%
      
      filter(str_detect(path, "archive") == FALSE) %>%
      
      mutate(id = index_id, 
             camera = index_camera,
             file_name = path_file(path), 
             extension = path_ext(path), 
             extension_lower = str_to_lower(extension), 
             path_dir = path_dir(path)) %>%
      filter(extension_lower %in% "jpg") %>%
      select(id, 
             camera,
             file_name, 
             extension,
             path_dir, 
             path)
    
  }
  bind_datalist <- do.call(bind_rows, datalist)
  
}

#   fxn_jpg_summary ---- 
# done_rename = FALSE
fxn_jpg_summary <- function(index_site,
                            index_year, 
                            done_rename){
  
  index_data <- fxn_jpg_map_files(index_site, 
                                index_year, 
                                done_rename) 
  
  if(nrow(index_data) == 0) stop("No deployments to summarize")
  
  summary <- 
    index_data  %>%
    mutate(file_name_id = str_sub(file_name, 1, 11),
           file_name_id_ok = id == file_name_id,
           file_name_length = nchar(file_name),
           from_to = str_sub(file_name, 6, 22), 
           
           yymmdd_to = substrRight(id, 6),
           has_camera = str_detect(file_name, 
                                   camera), 
           has_yymmdd_to = str_detect(file_name,
                                      yymmdd_to))
  
  
  # Count parentheses in image file name ---- 
  # show files with 0 or 1 set of parentheses
  # 0: confirm that these have been renamed
  check_parentheses <- 
    fxn_jpg_check_parentheses(index_data)
  
  # Create image_n from parentheses ----
  # Subset file_name to values before parentheses  
  split_parenthesis <- 
    str_split_fixed(index_data$file_name, ("\\("), 2)
  
  # Compare nomenclature with log ----
  # Compare id, date_from, date_to, camera with log
  jpg_init_review <-
    index_data %>%
    mutate(image_prefix = 
             split_parenthesis[,1], 
           has_jpg_ext = 
             str_detect(image_prefix, ".JPG")) %>%
    # Exclude renamed files
    filter(has_jpg_ext == FALSE) %>%
    distinct(id, camera, image_prefix) %>%
    left_join(dlog %>%
                select(id, 
                       log_camera = camera,
                       log_date_from = date_from, 
                       log_date_to = date_to), "id") %>%
    mutate(jpg_date_to = ymd(str_sub(image_prefix, 6, 11)),
           err_date_to = ifelse(log_date_to == jpg_date_to,
                                FALSE, TRUE), 
           err_camera = ifelse(camera == log_camera, 
                               FALSE, TRUE)) %>%
    relocate(id, 
             starts_with("err")) 
  
  jpg_init_review
  
 
}
#   fxn_jpg_timestamp_check ----
#   Check for timestamp errors 
fxn_jpg_timestamp_check <- function(index_site, index_year){

  fxn_define_camera_project(index_site)
  
  # List deployments to read ----
  list_id_exif <-
    dlog %>%
    filter(
           has_data != FALSE,
           # has_data == "UNK", 
           done_rename == FALSE,
           done_exif == FALSE) %>%
    pull(id) 
  
  # Create an initial map of image files ----
  # Set done_rename = TRUE here
  jpg_init <- 
    fxn_jpg_map_files(index_site, 
                      index_year,
                      done_rename = FALSE) %>%
    filter(id %in% list_id_exif)  
  
  # jpg_init %>%
  #   group_by(id) %>%
  #   count() %>%
  #   arrange(n)

  # Iterate by deployment to read exif info ----
  index_list <- unique(jpg_init$id)
  
  # index_id = index_list[8]
  for(index_id in index_list){
    
    cat(index_id, "\n")
    
    subset <- 
      dlog %>%
      filter(id %in% index_id) 
    
    index_path_jpg <-
      jpg_init %>%
      filter(id %in% index_id) %>%
      pull(path)
    
    exif_info <-
      fxn_exif_read(index_path_jpg = index_path_jpg, 
                    index_id = index_id)
    
    # Check date_to and date_from  ----
    check_dates <- fxn_jpg_check_dates(index_exif = exif_info)
    
    # Identify date errors
    if(nrow(check_dates) > 0){
      
      cat(" ...Check dates", "\n")
    }
    
    # Check file size  ----
    check_size <- 
      exif_info %>%
      filter(file_size == 0) 
    
    # Identify date errors
    if(nrow(check_size) > 0){
      
      cat(" ...", nrow(check_size), "files with size 0", "\n")
    }
  }
  
}
#   fxn_jpg_check_parentheses  ----
fxn_jpg_check_parentheses <- function(index_data) {
  
  # Count parentheses and check for mismatches
  check_parentheses <- 
    index_data %>%
    select(id,
           file_name) %>%
    mutate(count_from = str_count(file_name, "\\("), 
           count_to = str_count(file_name, "\\)"), 
           n_paren = 
             case_when(
               count_from == 1 & count_to == 1 ~ "one", 
               count_from == 0 | count_to == 0 ~ "none", 
               count_from > 1 | count_to > 1 ~ "multiple")) %>%
    relocate(id, n_paren)
  
  # Count images by parentheses
  count_parentheses <-
    check_parentheses %>%
    group_by(id, n_paren) %>%
    count() 
  
  # Identify ids with an error: 0 or 2+
  count_parentheses_example <- 
    check_parentheses %>%
    group_by(id, n_paren) %>%
    sample_n(1) %>%
    rename(example = file_name)
  
  list_ids_error <- 
    count_parentheses %>%
    filter(n_paren %nin% "one") %>%
    distinct(id) %>%
    pull(id)

    if(length(list_ids_error) > 0) {
    message("Zero or multiple parentheses")
    print(count_parentheses_example %>%
            filter(id %in% list_ids_error))
  }
  
  # Identify correct parentheses and sample file names
  list_ids_good <- 
    count_parentheses %>%
    filter(n_paren %in% "one") %>%
    distinct(id) %>%
    pull(id)
  
  if(length(list_ids_good) > 0) {
    message("One set of parentheses found")
    print(count_parentheses_example %>%
            filter(id %in% list_ids_good))
  }
}
#   fxn_jpg_check_dates ----
fxn_jpg_check_dates <- function(index_exif){
  
  # Get survey dates from dlog
  dlog_subset <- 
    dlog %>%
    select(id, date_from, date_to)
  
  # Check date_to and date_from
  check_dates <-
    index_exif %>%
    left_join(dlog_subset, "id") %>%
    filter(date < date_from |
             date > date_to)
  
  return(check_dates)
  
}
#   fxn_jpg_rename_01 ----
fxn_jpg_rename_01 <- function(index_exif, paren_n){
  
  if(paren_n == 0){
    add_image_n <-
      index_exif %>%
      arrange(id, date_time) %>%
      mutate(image_n = 1:n())
  }
  if(paren_n == 1){
    add_image_n <- 
      index_exif %>%
      # Create image_n from the number in parentheses
      mutate(
        image_n = as.numeric(
          str_extract(file_name,
                      "(?<=\\()\\d+(?=\\))")))
  }
  # Revise exif table with image_n ----
  revised_exif <-
    add_image_n %>%
    rename(file_name_init = file_name, 
           path_init = path_file) %>%
    mutate(image_n_pad = str_pad(image_n,
                                 width = 5,
                                 pad = "0",
                                 side = "left"),
           image_n_jpg = paste0("_",
                                image_n_pad,
                                ".JPG"),
           file_name = paste0(id,
                              image_n_jpg),
           path_file = paste(path_dir,
                            file_name,
                            sep = "/"),
           image_id = str_remove(file_name,
                                 ".JPG"), 
           file_name_temp = paste0("temp_", file_name), 
           path_temp = paste(path_dir, 
                             file_name_temp, 
                             sep = "/")) %>%
    select(id,
           image_id,
           camera,
           file_name,
           date_time,
           date,
           time,
           image_n,
           camera_make,
           camera_model,
           focal_length,
           image_width,
           image_height,
           file_size, 
           file_name_init, 
           file_name_temp,
           path_init,
           path_temp, 
           path_file,
           path_dir)
  
  return(revised_exif)
}

#   fxn_jpg_rename_exif_01 ----
# Add image_n to file_name using date_time order 
# paren_n = 1
fxn_jpg_rename_exif_01 <- function(index_site, index_year){
  
  # Note: Site and year defined in fxn_jpg_map_files
  # Map all images ----
  jpg_init <- 
    fxn_jpg_map_files(index_site, 
                      index_year,
                      done_rename = FALSE) 
  
  # unique(jpg_init$id)
  # Check for files to process ----
  if(nrow(jpg_init) == 0) stop("No files process")

  #   Iterate by deployment ----
  # Identify the deployments to renumber 
  index_list <- unique(jpg_init$id)
  
  # index_id = index_list[10]
  for(index_id in index_list){
    
    # Create helpers ----
    cat(index_id, "\n", " checking exif info", "\n")
    
    index_path_jpg <-
      jpg_init %>%
      filter(id %in% index_id) %>%
      pull(path)
    
    # Read exif info and sort by date  ----
    exif_info <-
      fxn_exif_read(index_path_jpg = index_path_jpg, 
                    index_id = index_id) %>%
      arrange(id, date_time) 
    
    # Check date_to and date_from  ----
    check_dates <- fxn_jpg_check_dates(index_exif = exif_info)
    
    # Break loop if there are date errors ----
    if(nrow(check_dates) > 0){
      message(paste0("Image date(s) outside survey period: ", 
                     index_id))
      break
    }else{
      cat("  dates ok", "\n")
    }
    # Check file size  ----
    check_size <- 
      exif_info %>%
      filter(file_size == 0) 
    
    # Break loop if there are file size errors ----
    if(nrow(check_size) > 0){
      cat(" ...", nrow(check_size), "files with size 0", "\n")
      break
    }else{
      cat("  file size ok", "\n")
    }
    
    # If no date or size errors: Revise image_n, write exif ----
    if(nrow(check_dates) == 0 & nrow(check_size) == 0){
      
      # Create new image numbers  
      add_image_n <- fxn_jpg_rename_01(index_exif = exif_info, 
                                       paren_n = 0)
    
      # Rename image files with temp_ prefix
      #  Prevents overwriting duplicate image names
      #  Important when "(2)" suffix mixed in on some images 
      add_image_n %>%
        select(path_from = path_init, 
               path_to = path_temp) %>%
        fxn_wrap_file_rename()
      
      # Rename image files 
      add_image_n %>%
        select(path_from = path_temp, 
               path_to = path_file) %>%
        fxn_wrap_file_rename()
      
      cat("  renamed files", "\n")
      
      # Write exif table 
      add_image_n %>%
        select(-path_init,
               -path_temp, 
               -file_name_init, 
               -file_name_temp) %>%
        write_csv(here(path_exif,
                       paste0(index_id, "_exif.csv")),
                  na = "")
      
      cat("  created _exif.xlsx", "\n")
      
      
    }
  }
}


#   fxn_jpg_check_rename  ----
fxn_jpg_check_rename <- function(index_site, index_year){
  
  fxn_define_camera_project(index_site)
  
  # Map renamed files
  jpg_map_done <- 
    fxn_jpg_map_files(index_site, 
                      index_year,
                      done_rename = TRUE) %>%
    mutate(file_name_id = str_sub(file_name, 1, 11),
           file_name_id_ok = id == file_name_id,
           file_name_length = nchar(file_name),
           file_name_length_ok = file_name_length == 21) %>%
  
    # Identify rename errors 
    filter(file_name_id_ok == FALSE | 
             file_name_length_ok == FALSE) 
  
  if(nrow(jpg_map_done) == 0){
    message("All files renamed correctly")
  }else{
    message("ERROR: files need rename")
    
    jpg_map_done %>%
      group_by(id, 
               file_name_id_ok, 
               file_name_length_ok) %>%
      count()
  }
  
  return(jpg_map_done)
}

# ---------------------------------------------------------- -----
# Create _exif tables ----
#   fxn_exif_read ----
fxn_exif_read <- function(index_path_jpg, 
                          index_id){
  
  exif_info <-
    exif_read(path = index_path_jpg,
              pipeline = "csv",
              tags = list_exif_tags) %>%
    clean_names()  %>%
    as_tibble() %>%
    rename(date_time = date_time_original, 
           path_file = source_file,
           path_dir = directory,
           camera_make = make,
           camera_model = model) %>%
    mutate(id = index_id,
           image_id = str_remove(file_name,
                                 ".JPG"),
  
           # For colon between date values
           regex =
             str_replace_all(
               date_time,
               "([0-9]{4}):([0-9]{2}):([0-9]{2})",
               "\\1-\\2-\\3"), 
           date_time = 
             case_when(
               str_detect(date_time, ":") ~
                 as_datetime(regex), 
               TRUE ~ as_datetime(date_time)
             ),
           
           camera = str_sub(index_id, 1, 4),
           date = as_date(date_time),
           time = hms::as_hms(date_time)) %>%
    select(-regex) %>%
    arrange(id, date_time) %>%
    mutate(image_n = 1:n()) %>%
    select(id,
           image_id,
           camera,
           file_name,
           date_time,
           date,
           time,
           image_n,
           path_file,
           path_dir,
           camera_make,
           camera_model,
           focal_length,
           image_width,
           image_height,
           file_size)
  
  return(exif_info)
}
#   fxn_exif_create: Create new exif tables ----
fxn_exif_create <- function(index_site, index_year){
  
  fxn_define_camera_project(index_site)
  
  # List deployments to read ----
  list_id_exif <-
    dlog %>%
    filter(has_data != FALSE, 
           done_rename == TRUE,
           done_exif == FALSE) %>%
    pull(id) 
  
  # Create an initial map of image files ----
  # Set done_rename = TRUE here
  jpg_init <- 
    fxn_jpg_map_files(index_site, 
                      index_year,
                      done_rename = TRUE) %>%
    filter(id %in% list_id_exif)
 
  # Iterate by deployment to read exif info ----
  index_list <- unique(jpg_init$id)
  
  # index_id = index_list[1]
  for(index_id in index_list){
    
    cat(index_id, "\n")
    
    subset <- 
      dlog %>%
      filter(id %in% index_id) 
    
    index_path_jpg <-
      jpg_init %>%
      filter(id %in% index_id) %>%
      pull(path)
    
    exif_info <-
      fxn_exif_read(index_path_jpg = index_path_jpg, 
                    index_id = index_id)
    
    # Check date_to and date_from  ----
    check_dates <- 
      exif_info %>%
      filter(date < subset$date_from |
               date > subset$date_to)
    
    # Identify date errors
    if(nrow(check_dates) > 0){
      message(paste0("Image date(s) outside survey period: ", 
                     index_id))
      break
    }
    # If no date errors: write _exif
    if(nrow(check_dates) == 0){
      
      # Write exif table ----
      exif_info %>%
        write_csv(here(path_exif,
                       paste0(index_id, "_exif.csv")),
                  na = "")
      
    }
    
  
  }
}

#   fxn_exif_collate_csv ----
# Used to check for missing data in exif tables 
fxn_exif_collate_csv <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # Only files in main folder (no archived files)
  list_files <- list.files(here(path_exif),
                           recursive = FALSE,
                           pattern = '*.csv')
  
  # # Recursive to include z_archive 
  # list_files <- list.files(here(path_exif),
  #                          recursive = TRUE,
  #                          pattern = '*.csv')  %>%
  #   as_tibble() %>%
  #   mutate(id = str_remove(basename(value),
  #                          "_exif.csv")) %>%
  #   filter(id %in% list_id_blank) %>%
  #   pull(value)
  # 
  datalist <- list()
  for(index_csv in list_files){
    
    path_file <- here(path_exif, index_csv)
    
    datalist[[index_csv]] <-
      read_csv(path_file, 
               col_types = 
                 cols(
                   id = col_character(),
                   image_id = col_character(),
                   camera = col_character(),
                   file_name = col_character(),
                   date_time = col_datetime(),
                   date = col_character(),
                   time = col_time(),
                   image_n = col_double(),
                   path_file = col_character(),
                   path_dir = col_character(),
                   camera_make = col_character(),
                   camera_model = col_character(),
                   focal_length = col_character(),
                   image_width = col_character(),
                   image_height = col_character(),
                   file_size = col_character()
                 )
      ) %>%
      mutate(id = str_remove(basename(index_csv), 
                             "_exif.csv"), 
             date = date(parse_date_time(date,
                                         c("ymd", 
                                           "mdy"))))
    
  }
  bind_datalist <- do.call(bind_rows, datalist)
}

#   fxn_exif_summary -----
fxn_exif_summary <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  exif_all <- fxn_exif_collate_csv(index_site)
  
  exif_summary <- fxn_table_check_dlog(index_data = exif_all, 
                                       index_type = "exif") 
  
  # Write to csv for manual revision of log  
  index_file_name <- paste0(index_site, "_exif-summary.csv")
  
  fxn_archive_old_csv(index_file_name = index_file_name)
  
  write_csv(exif_summary,
            here(path_out,
                 index_file_name), 
            na = "")
  
}

#   fxn_exif_summary_errors  ----
fxn_exif_summary_errors <- function(index_site) {
  
  fxn_define_camera_project(index_site)
  exif_all <- fxn_exif_collate_csv(index_site)
  exif_summary <- fxn_table_check_dlog(exif_all, 
                                       index_type = "exif") 
  
  # List all columns starting with "err_"
  err_cols <- grep("^err_", names(exif_summary), value = TRUE)
  
  # Filter rows with any "err_" column set to TRUE
  id_errors <- 
    exif_summary %>%
    filter(if_any(err_cols, ~ . == TRUE))
  
  # Join with dlog and rearrange columns
  exif_errors <-
    id_errors %>%
    left_join(
      dlog %>% 
        select(id,
               error_type,
               error_subtype, 
               starts_with("n_img_excl_from")), 
      by = "id"
    ) %>%
    relocate(id, error_type, error_subtype)
  
  if(nrow(exif_errors) == 0){
    print("No exif errors")
  }
  
  # Write to csv
  if(nrow(exif_errors) > 0){
    index_file_name <- paste0(index_site, "_exif-summary_errors.csv")
    
    fxn_archive_old_csv(index_file_name = index_file_name)
    
    write_csv(exif_errors,
              here(path_out,
                   index_file_name), 
              na = "")
  }
}

# ========================================================== -----


