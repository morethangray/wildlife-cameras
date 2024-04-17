# updated: 2024-02-26 ----
# ========================================================== -----
# # Compare inventory tracking ----
#   fxn_dlog_ilog_compare ----
fxn_dlog_ilog_compare <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # Create dlog_subset to compare with ilog ----
  if(index_site == "MMP"){
    year_filter <- 2016
  }else{
    year_filter <- 2019
  }
  
  dlog_subset <- 
    dlog %>%
    mutate(year_to = as.numeric(year_to)) %>%
    filter(year_to > year_filter) %>%
    
    select(id, 
           drive_img, 
           done_blank,
           done_catalog, 
           done_tidy, 
           done_qc, 
           done_vault) 
  
  # Compare id values in ilog, dlog ----
  common_ids <- intersect(ilog$id, dlog_subset$id)
  missing_in_ilog <- setdiff(dlog_subset$id, ilog$id)
  missing_in_dlog <- setdiff(ilog$id, dlog_subset$id)
  
  # Display results ----
  if(length(missing_in_ilog) == 0) {
    message("No IDs are missing in ilog")
  } else {
    message(paste("IDs missing in ilog:", 
                  paste(missing_in_ilog, 
                        collapse = ", ")))
  }
  if(length(missing_in_dlog) == 0) {
    message("No IDs are missing in dlog")
  } else {
    message(paste("IDs missing in dlog:", 
                  paste(missing_in_dlog, 
                        collapse = ", ")))
  }
  
  # Compare _done values in ilog, dlog ----
  # Merge tibbles
  comparison <- 
    full_join(ilog,
              dlog_subset,
              by = "id", 
              suffix = c("_ilog", "_dlog")) 
  
  # Loop through columns, identify differences
  columns_to_compare <- c("drive_img",
                          "done_blank",
                          "done_catalog", 
                          "done_tidy",
                          "done_qc", 
                          "done_vault")
  result_list <- list()
  
  # col_name <- columns_to_compare[3]
  
  for (col_name in columns_to_compare) {
    col_1 <- paste0(col_name, "_ilog")
    col_2 <- paste0(col_name, "_dlog")
    
    temp_df <- 
      comparison %>%
      select(id,
             .data[[col_1]],
             .data[[col_2]]) %>%
      filter((!is.na(.data[[col_1]]) | 
                !is.na(.data[[col_2]])) &
               .data[[col_1]] != .data[[col_2]])  %>%
      
      rename(!!"ilog" := .data[[col_1]],
             !!"dlog" := .data[[col_2]])
    
    if(nrow(temp_df) > 0){
      result_list[[col_name]] <- temp_df
    }
  }
  # result_list contains the discrepancies for each column
  # For example, to see discrepancies for 'done_blank', you can run:
  # View(result_list[["done_tidy"]])
  
  # Display results ----
  if(length(result_list) == 0) {
    message("All ilog and dlog values match")
  } else {
    message(paste("Different values found"))
    print(result_list)
    # View(result_list[["done_qc"]])
  }
}
#   fxn_dir_jpg_find_new ----
fxn_dir_jpg_find_new <- function(index_site, 
                                 index_year){
  
  fxn_define_camera_project(index_site)
  
  tbl_new <- 
    fxn_dir_jpg_map(index_site = index_site, 
                    index_year = index_year) %>%
    # Filter to id not in dlog
    filter(err_id == TRUE) %>%
    remove_empty("cols")
  
  if(nrow(tbl_new) == 0){
    message(paste0("No new image folders found for ",
                   index_site, "_", index_year))
    
    
  }else{
    n_new <- length(unique(tbl_new$id))
    message(paste0("Revise deployment log: ", 
                   n_new, 
                   " new image folders found for ",
                   index_site, "_", index_year))
    print(tbl_new)
    
    index_file_name <- paste0(index_site, "_folder-names_init.csv")
    
    fxn_archive_old_csv(index_file_name = index_file_name)
    
    write_csv(tbl_new,
              here(path_r_out,
                   index_file_name), 
              na = "")
  }
  
}

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
    # View(tbl_new)
    
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
      # View(tbl_new)
    }
  }
}

# ---------------------------------------------------------- -----
# Identify files that need management ----
# _exif files that need to be made into _blank
# _final files that need to be made into _tidy
# _qc files that need to be made into _clean and archived

# index_type = "vault"
fxn_find_files_to_process <- function(index_type){
  
  # Identify files that need processing ---- 
  # Need blank 
  if(index_type == "blank"){
    filtered_dlog <- 
      dlog %>%
      filter(done_exif == TRUE, 
             done_blank %in% c("FALSE", "REDO"))
    
    index_path <- 
      normalizePath(path_exif, 
                    winslash = "/", 
                    mustWork = FALSE)
    
  }
  
  # Need tidy 
  if(index_type == "tidy"){
    filtered_dlog <- 
      dlog %>%
      filter(done_catalog == TRUE, 
             done_tidy %in% c("FALSE", "REDO"))
    
    index_path <- 
      normalizePath(path_table_catalog, 
                    winslash = "/", 
                    mustWork = FALSE)
  }
  # Need vault 
  if(index_type == "vault"){
    filtered_dlog <- 
      dlog %>%
      filter(done_qc == TRUE, 
             done_vault %in% c("FALSE", "REDO"))
    
    index_path <- 
      normalizePath(path_table_qc, 
                    winslash = "/", 
                    mustWork = FALSE)
  }
  
  # List id that need processing
  list_id_need <- unique(filtered_dlog$id)
  
  
  # Identify all files by type ----
  # List the id that need processing
  # list_id <- unique(filtered_dlog$id)
  
  # Map files in folder
  all_files <- 
    tibble(path =
             dir_ls(path = index_path,
                    recurse = FALSE,
                    type = "file")) %>%
    mutate(file_name = path_file(path),
           id = str_sub(file_name, 1, 11), 
           need_process = id %in% list_id_need) %>%
    filter(str_detect(id, "Thumbs") == FALSE) %>%
    relocate(id, 
             need_process) 
  
  
  # Summarize files by status
  summarize_files <- 
    all_files %>%
    mutate(process_type = index_type) %>%
    group_by(process_type, need_process) %>%
    count() 
  
  if(nrow(summarize_files) > 0){
    n_files <- 
      summarize_files %>%
      filter(need_process == TRUE) %>%
      pull(n)
  }else{
    n_files <- 0
  }
  
  print(summarize_files)
  
  if(n_files == length(list_id_need)){
    cat("PASS: file count matches dlog", "\n")
  }else{
    cat("ERROR: file count is ", n_files, "; dlog count is ", length(list_id_need), "\n")
  }
  
  return(all_files)
  
}
# Move done_ files ----
fxn_unarchive_done_files <- function(index_type){
  
  # Identify files that are done ---- 
  
  if(index_type == "exif"){
    filtered_dlog <- 
      dlog %>%
      filter(done_exif == TRUE)
  }
  
  if(index_type == "blank"){
    filtered_dlog <- 
      dlog %>%
      filter(done_blank == TRUE)
  }
  
  if(index_type == "catalog"){
    filtered_dlog <- 
      dlog %>%
      filter(done_catalog == TRUE)
  }
  
  if(index_type == "tidy"){
    filtered_dlog <- 
      dlog %>%
      filter(done_tidy == TRUE)
  }
  
  if(index_type == "qc"){
    filtered_dlog <- 
      dlog %>%
      filter(done_qc == TRUE)
  }
  
  
  # List id that are done
  list_id_done <- unique(filtered_dlog$id)
  
  # Create paths for main and archive folders ----
  list_types <- c("exif",
                  "blank",
                  "catalog",
                  "tidy",
                  "qc")
  
  lookup_paths <- 
    tibble(index_type = list_types, 
           index_path_init = paste0("path_table_", index_type),
           index_path_rev = str_replace(index_path_init, 
                                        "path_table_exif", 
                                        "path_exif"),
           index_path_archive = paste0(index_path_rev, "_archive"), 
           index_path = str_replace(index_path_rev,
                                    "path_table_tidy", 
                                    "path_table_qc")) %>%
    select(-index_path_init, 
           -index_path_rev)
  
  filtered_paths <-  
    lookup_paths %>%
    filter(index_type == {{index_type}})
  
  index_path <- 
    normalizePath(get(filtered_paths$index_path), 
                  winslash = "/", 
                  mustWork = FALSE)
  index_path_archive <- 
    normalizePath(get(filtered_paths$index_path_archive), 
                  winslash = "/", 
                  mustWork = FALSE)
  
  # Identify all files by type ----
  all_files <- 
    tibble(path =
             dir_ls(path = index_path,
                    recurse = TRUE,
                    type = "file")) %>%
    mutate(file_name = path_file(path),
           id = str_sub(file_name, 1, 11),
           path_dir = path_dir(path)) %>%
    filter(str_detect(id, "Thumbs") == FALSE) %>%
    relocate(id,
             path_dir) 
  
  # Identify archived files that need processing ----
  # Identify files in archive that are NOT in list_id_done
  paths_to_move <- 
    all_files %>%
    filter(path_dir == index_path_archive) %>%
    filter(id %nin% list_id_done) %>%
    mutate(path_new = here(index_path, file_name))  %>%
    relocate(id,
             path_dir, 
             path_new) 
  
  if(nrow(paths_to_move) > 0){
    
    
  }
  # Move files out of archive into main folder ----
  # Unarchive files
  
  file_move(path = paths_to_move$path, 
            new_path = paths_to_move$path_new)
  
}

# ========================================================== -----
