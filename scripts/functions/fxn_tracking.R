# updated: 2024-02-26 ----
# ========================================================== -----
# # Compare inventory tracking ----
#   fxn_dlog_ilog_compare ----
fxn_dlog_ilog_compare <- function(index_site){
  
  fxn_define_camera_project(index_site)
  
  # Create dlog_subset to compare with ilog 
  # This is the minimum year tracked by ilog
  # Deployments are moved out of ilog when year is done

  year_filter <- min(ilog$year_to, na.rm = TRUE)-1
  
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
  
  # Display results 
  if(length(missing_in_ilog) == 0) {
    cat("No IDs are missing in ilog\n")
  } else {
    message(paste("IDs missing in ilog:", 
                  paste(missing_in_ilog, 
                        collapse = ", ")))
  }
  if(length(missing_in_dlog) == 0) {
    cat("No IDs are missing in dlog\n")
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
  
  # Display results 
  if(length(result_list) == 0) {
    cat("All ilog and dlog values match\n")
  } else {
    message(paste("Different values found"))
    print(result_list)
  }
}
#   fxn_find_new_folders ----
fxn_find_new_folders <- function(index_site, index_year){
  
  fxn_define_camera_project(index_site)
  
  tbl_init <- 
    fxn_dir_jpg_map(index_site, index_year) %>%
    # Filter to id not in dlog
    filter(err_id == TRUE) %>%
    remove_empty("cols") 
 
  # Display results of this check for new folders 
  if(nrow(tbl_init) == 0){
    cat(paste0("No new image folders found for ", index_site, "_", index_year, "\n"))
    
  }else{
    
    # Function to get end date of prior survey from dlog 
    fxn_get_last_date_from <- function(index_data){
      
      # Modify new_surveys to be tbl_init 
      new_surveys <- 
        index_data %>%
        select(id) %>%
        mutate(date = paste0("20", str_sub(id, 6, 11)), 
               date_to = ymd(date), 
               camera = str_sub(id, 1, 4))
      
      cameras_with_multiple_surveys <- 
        new_surveys %>%
        group_by(camera) %>%
        count() %>%
        filter(n > 1) %>%
        pull(camera)
      
      most_recent_survey <- 
        dlog %>% 
        filter(use_data != "UNK") %>%
        group_by(camera) %>%
        summarize(date_max = max(date_to, na.rm = TRUE)) %>%
        # Exclude cameras with multiple surveys 
        filter(camera %nin% cameras_with_multiple_surveys)
      
      with_date_from <- 
        new_surveys %>%
        left_join(most_recent_survey, "camera") %>%
        mutate(date_from = case_when(date_max < date_to ~ date_max, TRUE ~ NA_Date_), 
               year_to = year(date_to)) %>%
        select(id, camera, year_to, date_from, date_to)
      
      return(with_date_from)
    }
    
    # Function to create new dlog row ----
    fxn_create_dlog_row <- function(index_data){
      
      dlog_update <-
        index_data %>%
        mutate(
          has_data = "UNK",
          use_data = "UNK",
          done_fix = "N/A",
          done_intern = "FALSE",
          done_rename = "FALSE",
          done_exif = "FALSE",
          done_blank = "FALSE",
          done_catalog = "FALSE",
          done_tidy = "FALSE",
          done_qc = "FALSE",
          done_vault = "FALSE",
          date_excl_from = "N/A",
          date_excl_to = "N/A",
          n_img_to = "",
          n_img_excl_sets = "0",
          error_type = "None",
          error_subtype = "N/A",
          comments = "None",
          date_img_from = "",
          date_img_to = "",
          n_img_from = "1",
          n_img_excl_from_1 = "0",
          n_img_excl_to_1 = "0",
          n_img_excl_from_2 = "0",
          n_img_excl_to_2 = "0",
          n_img_excl_from_3 = "0",
          n_img_excl_to_3 = "0",
          drive_img = "K",
          date_added = Sys.Date(),
          id_orig = "N/A") %>%
        mutate(path_server = here(path_jpg, year_to, camera, id)) %>%
        select(id,
               camera,
               year_to,
               has_data,
               use_data,
               done_fix,
               done_intern,
               done_rename,
               done_exif,
               done_blank,
               done_catalog,
               done_tidy,
               done_qc,
               done_vault,
               date_from,
               date_to,
               date_excl_from,
               date_excl_to,
               n_img_to,
               n_img_excl_sets,
               error_type,
               error_subtype,
               comments,
               date_img_from,
               date_img_to,
               n_img_from,
               n_img_excl_from_1,
               n_img_excl_to_1,
               n_img_excl_from_2,
               n_img_excl_to_2,
               n_img_excl_from_3,
               n_img_excl_to_3,
               drive_img,
               date_added,
               path_server,
               id_orig) 
    }
    
    # Revise table of new folders with potential dates
    tbl_new <- fxn_get_last_date_from(tbl_init) %>%
      fxn_create_dlog_row()
    
    n_new <- length(unique(tbl_new$id))
    message(paste0("Revise deployment log: ", n_new, 
                   " new image folders found for ",
                   index_site, "_", index_year))
    print(tbl_new)
    
    index_file_name <- paste0(index_site, "_new-folder-names.csv")
    fxn_archive_old_csv(index_file_name = index_file_name)
    write_csv(tbl_new,  here(path_out, index_file_name), na = "")
  }
}


# ---------------------------------------------------------- -----
# Identify files that need management ----
#   fxn_find_files_to_process ----
# _exif files that need to be made into _blank
# _final files that need to be made into _tidy
# _qc files that need to be made into _clean and archived
# index_type = "vault"
fxn_find_files_to_process <- function(index_type) {
  
  # Archive files that are in vault  
  fxn_archive_final_dupes <- function() {
    index_path <- path_table_qc
    
    dlog_vault <- dlog %>% filter(done_vault == TRUE)
    
    all_files <- tibble(path = dir_ls(path = index_path, recurse = FALSE, type = "file")) %>%
      mutate(file_name = path_file(path), id = str_sub(file_name, 1, 11))
    
    files_to_archive <- all_files %>%
      filter(id %in% unique(dlog_vault$id)) %>%
      mutate(is_qc = str_detect(file_name, "tidy_qc"),
             folder_new = ifelse(is_qc, here(index_path, "z_archive/"), here(index_path, "z_archive/image-tables_final_tidy")),
             path_new = here(folder_new, file_name)) %>%
      select(path_new, path)
    
    file_move(path = files_to_archive$path, new_path = files_to_archive$path_new)
  }
  
  fxn_archive_final_dupes()
  
  # Identify files that need processing
  get_filtered_dlog <- function(index_type) {
    if (index_type == "rename") {
      return(dlog %>% 
               filter(done_rename %in% c("FALSE", "REDO")))
    }
    if (index_type == "exif") {
      return(dlog %>% 
               filter(has_data == TRUE, 
                      done_rename == TRUE, 
                      done_exif %in% c("FALSE", "REDO")))
    }
    if (index_type == "blank") {
      return(dlog %>% 
               filter(done_exif == TRUE, 
                      done_blank %in% c("FALSE", "REDO")))
    }
    if (index_type == "tidy") {
      return(dlog %>% 
               filter(done_catalog == TRUE, 
                      done_tidy %in% c("FALSE", "REDO")))
    }
    if (index_type == "vault") {
      return(dlog %>% 
               filter(done_qc == TRUE, 
                      done_vault %in% c("FALSE", "REDO")))
    }
    return(tibble())
  }
  
  index_path <- switch(index_type,
                       "exif" = normalizePath(path_exif, winslash = "/", mustWork = FALSE),
                       "blank" = normalizePath(path_exif, winslash = "/", mustWork = FALSE),
                       "tidy" = normalizePath(path_table_catalog, winslash = "/", mustWork = FALSE),
                       "vault" = normalizePath(path_table_qc, winslash = "/", mustWork = FALSE),
                       "")
  
  index_message <- switch(index_type,
                          "exif" = "ACTION NEEDED - Create exif image tables for ",
                          "blank" = "ACTION NEEDED - Create blank image tables for ",
                          "tidy" = "ACTION NEEDED - Create tidy image tables for ",
                          "vault" = "ACTION NEEDED - Move to vault: ",
                          "")
  
  filtered_dlog <- get_filtered_dlog(index_type)
  
  if (nrow(filtered_dlog) == 0) {
    cat(paste0("No action needed for ", index_type, "\n"))
    return(tibble())
  } else {
    if (index_type == "rename") {
      message(paste0("ACTION NEEDED - Rename images for ", nrow(filtered_dlog), " deployments in dlog"))
    } else {
      list_id_need <- unique(filtered_dlog$id)
      n_files_need <- length(list_id_need)
      
      all_files <- 
        tibble(path = dir_ls(path = index_path, 
                             recurse = FALSE, 
                             type = "file")) %>%
        mutate(file_name = path_file(path), 
               id = str_sub(file_name, 1, 11),
               need_process = id %in% list_id_need) %>%
        filter(!str_detect(id, "Thumbs")) %>%
        relocate(id, need_process)
      
      if (index_type == "vault") {
        all_files <- all_files %>%
          mutate(in_process = str_detect(file_name, regex("pro", ignore_case = TRUE))) %>%
          filter(str_detect(path, "final_tidy_qc"), !in_process)
      }
      
      # Identify deployments with files in folder, but missing from dlog
      missing_dlog <- setdiff(all_files$id, list_id_need)
      # Deployments in dlog, but missing files in folder
      missing_files <- setdiff(list_id_need, all_files$id)
      
      list_id_has_multiple <- all_files %>%
        group_by(id) %>%
        count() %>%
        filter(n > 1) %>%
        pull(id)
      
      if (length(list_id_has_multiple) > 0) {
        message("Multiple files for ", length(list_id_has_multiple), " deployments")
        print(list_id_has_multiple)
      }
      
      summarize_files <- all_files %>%
        mutate(process_type = index_type) %>%
        group_by(process_type, need_process) %>%
        count()
      
      if (nrow(summarize_files) > 0) {
        n_files <- summarize_files %>%
          filter(need_process == TRUE) %>%
          pull(n)
        
        if (length(n_files) > 0) {
          # For deployments with additional file tracking issues
          if (n_files != n_files_need) {
            
            # When there are MORE files in data folder than expected from dlog
            if (length(missing_dlog) > 0) {
              message(paste0("ACTION NEEDED - Update done_", index_type, " for ", length(missing_dlog), " deployments in dlog"))
              print(all_files %>% filter(id %in% missing_dlog))
            }
            
            # When there are FEWER files in data folder than expected from dlog
            if (length(missing_files) > 0) {
              message(paste0("ACTION NEEDED - Check Intern Drop Folders for ", length(missing_files), " missing ", index_type, " files"))
              print(filtered_dlog %>%
                      filter(id %in% missing_files) %>%
                      select(id, starts_with("done_"), starts_with("error")))
            }
          }
          message(paste0(index_message, n_files, " files"))
        } else {
          cat(paste0("No action needed for ", index_type, "\n"))
        }
      } else {
        if (nrow(filtered_dlog) > 0) {
          message(paste0("ACTION NEEDED - Create ", index_type, " tables for ", length(missing_files), " deployments in dlog"))
        }
      }
      return(all_files)
    }
  }
}
#
#   fxn_check_file_status -----
fxn_check_file_status <- function(){
  
  check_rename <- fxn_find_files_to_process("rename") 
  
  check_exif <- fxn_find_files_to_process("exif") 
  # %>%
  #   filter(need_process == TRUE)
  
  check_blank <- fxn_find_files_to_process("blank")  
  # %>%
  #   filter(need_process == TRUE)
 
  check_tidy <- fxn_find_files_to_process("tidy") 
  # %>%
  #   filter(need_process == TRUE)
  
  check_vault <- fxn_find_files_to_process("vault")  
  # %>%
  #   filter(need_process == TRUE)
  
  all_checks <- list(check_rename = check_rename, 
                     check_exif = check_exif, 
                     check_blank = check_blank, 
                     check_tidy = check_tidy, 
                     check_vault = check_vault)
}
# Unarchive done_ files ----
#   fxn_unarchive_done_files ----
# index_type = "catalog"
# fxn_define_camera_project(index_site)
fxn_unarchive_done_files <- function(index_type){

  # Identify files to unarchive ----

  if(index_type == "exif"){
    filtered_dlog <-
      dlog %>%
      filter(done_exif == TRUE, 
             done_blank == FALSE)
  }

  # if(index_type == "blank"){
  #   filtered_dlog <-
  #     dlog %>%
  #     filter(done_blank == TRUE)
  # }
  # 
  if(index_type == "catalog"){
    filtered_dlog <-
      dlog %>%
      filter(done_catalog == TRUE, 
             done_tidy == FALSE, 
             done_qc == FALSE)
  }
  # 
  # if(index_type == "tidy"){
  #   filtered_dlog <-
  #     dlog %>%
  #     filter(done_tidy == TRUE)
  # }
  # 
  # if(index_type == "qc"){
  #   filtered_dlog <-
  #     dlog %>%
  #     filter(done_qc == TRUE)
  # }


  # List id to unarchive
  list_id_need <- unique(filtered_dlog$id)

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
  # Identify files in archive that are in list_id_need
  paths_to_move <-
    all_files %>%
    filter(path_dir == index_path_archive) %>%
    filter(id %in% list_id_need) %>%
    mutate(path_new = here(index_path, file_name))  %>%
    relocate(id,
             path_dir,
             path_new)
  
  # Move files out of archive into main folder ----

  if(nrow(paths_to_move) > 0){
    
    # Unarchive files
    
    file_move(path = paths_to_move$path,
              new_path = paths_to_move$path_new)

  }
  

}

# ========================================================== -----

# GRAVEYARD ----
# fxn_find_files_to_process ----
# 
# fxn_find_files_to_process <- function(index_type){
#   
#   # Archive files that are in vault  
#   fxn_archive_final_dupes <- function(){
#     index_path = path_table_qc
#     
#     dlog_vault <- 
#       dlog %>%
#       filter(done_vault == TRUE)
#     
#     # Map files in folder
#     all_files <-
#       tibble(path = dir_ls(path = index_path,
#                            recurse = FALSE,
#                            type = "file")) %>%
#       mutate(file_name = path_file(path),
#              id = str_sub(file_name, 1, 11)) 
#     
#     files_to_archive <-
#       all_files %>%
#       filter(id %in% unique(dlog_vault$id)) %>%
#       mutate(is_qc = str_detect(file_name, "tidy_qc"), 
#              folder_new = ifelse(is_qc == TRUE,
#                                  here(index_path,  "z_archive/"), 
#                                  here(index_path, "z_archive/image-tables_final_tidy")), 
#              path_new = here(folder_new, file_name)) %>%
#       select(path_new, path) 
#     
#     # Move _tidy files to archive
#     file_move(path = files_to_archive$path,
#               new_path = files_to_archive$path_new)
#     
#   }
#   fxn_archive_final_dupes()
#   
#   # Identify files that need processing
#   # Filter dlog using done_ columns 
#   # Need image rename 
#   if(index_type == "rename"){
#     filtered_dlog <- 
#       dlog %>%
#       filter(done_rename %in% c("FALSE", "REDO"))
#     
#     if(nrow(filtered_dlog) == 0){
#       cat(paste0("No images need to be renamed\n"))
#     }
#     if(nrow(filtered_dlog) > 0){
#       message(paste0("ACTION NEEDED - Rename images for ", nrow(filtered_dlog),  " deployments in dlog"))
#       
#     }
#   }
#   if(index_type != "rename"){
#     # Need exif 
#     if(index_type == "exif"){
#       filtered_dlog <- 
#         dlog %>%
#         filter(has_data == TRUE, 
#                done_rename == TRUE,
#                done_exif %in% c("FALSE", "REDO"))
#       
#       index_path <- 
#         normalizePath(path_exif, 
#                       winslash = "/", 
#                       mustWork = FALSE)
#       
#       index_message <- paste0("ACTION NEEDED - Create exif image tables for ")
#       
#     }
#     
#     # Need blank 
#     if(index_type == "blank"){
#       filtered_dlog <- 
#         dlog %>%
#         filter(done_exif == TRUE, 
#                done_blank %in% c("FALSE", "REDO"))
#       
#       index_path <- 
#         normalizePath(path_exif, 
#                       winslash = "/", 
#                       mustWork = FALSE)
#       
#       index_message <- paste0("ACTION NEEDED - Create blank image tables for ")
#     }
#     
#     # Need tidy 
#     if(index_type == "tidy"){
#       filtered_dlog <- 
#         dlog %>%
#         filter(done_catalog == TRUE, 
#                done_tidy %in% c("FALSE", "REDO"))
#       
#       index_path <- 
#         normalizePath(path_table_catalog, 
#                       winslash = "/", 
#                       mustWork = FALSE)
#       
#       index_message <- paste0("ACTION NEEDED - Create tidy image tables for ")
#     }
#     # Need vault 
#     if(index_type == "vault"){
#       filtered_dlog <- 
#         dlog %>%
#         filter(done_qc == TRUE, 
#                done_vault %in% c("FALSE", "REDO"))
#       
#       index_path <- 
#         normalizePath(path_table_qc, 
#                       winslash = "/", 
#                       mustWork = FALSE)
#       
#       index_message <- paste0("ACTION NEEDED - Move to vault: ")
#     }
#     
#     # List id that need processing
#     list_id_need <- unique(filtered_dlog$id)
#     n_files_need <- length(list_id_need)
#     
#     
#     # Map files in folder
#     all_files <- 
#       tibble(path = dir_ls(path = index_path,
#                            recurse = FALSE,
#                            type = "file")) %>%
#       
#       mutate(file_name = path_file(path),
#              id = str_sub(file_name, 1, 11), 
#              need_process = id %in% list_id_need) %>%
#       filter(str_detect(id, "Thumbs") == FALSE) %>%
#       relocate(id, need_process) 
#     
#     if(index_type == "vault"){
#       
#       all_files <- 
#         all_files %>%
#         mutate(in_process = str_detect(file_name,
#                                        regex("pro",
#                                              ignore_case = TRUE))) %>%
#         filter(str_detect(path, "final_tidy_qc")) %>%
#         filter(in_process == FALSE)
#       
#     }
#     
#     # Identify missing files and deployments
#     # Deployments with files in folder, but missing from dlog
#     missing_dlog <- setdiff(all_files$id, list_id_need)
#     # Deployments in dlog, but missing files in folder
#     missing_files <- setdiff(list_id_need, all_files$id)
#     
#     # Identify deployments with multiple files 
#     list_id_has_multiple <- 
#       all_files %>%
#       group_by(id) %>%
#       count() %>%
#       filter(n>1) %>%
#       pull(id)
#     
#     if(length(list_id_has_multiple) > 0){
#       message("Multiple files for",  length(list_id_has_multiple), "deployments", "\n")
#       print(list_id_has_multiple)
#     }
#     
#     # Summarize files by status in dlog  
#     summarize_files <- 
#       all_files %>%
#       mutate(process_type = index_type) %>%
#       group_by(process_type, need_process) %>%
#       count() 
#     
#     # Identify deployments that need processing
#     if(nrow(summarize_files) > 0){
#       
#       n_files <- 
#         summarize_files %>%
#         filter(need_process == TRUE) %>%
#         pull(n)
#       
#       # Write messages based on type of processing needed
#       if(length(n_files) > 0){
#         # For deployments with additional file tracking issues
#         if(n_files != n_files_need){
#           
#           # When there are MORE files in data folder than expected from dlog
#           if(length(missing_dlog) > 0){
#             message(paste0("ACTION NEEDED - Update done_", index_type, " for ", length(missing_dlog), " deployments in dlog"))
#             
#             # List the file info
#             print(all_files %>%
#                     filter(id %in% missing_dlog))
#             
#           }
#           
#           # When there are FEWER files in data folder than expected from dlog
#           if(length(missing_files) > 0){
#             message(paste0("ACTION NEEDED - Check Intern Drop Folders for ", length(missing_files)," missing ", index_type, " files"))
#             
#             # List the file info
#             print(filtered_dlog %>%
#                     filter(id %in% missing_files) %>%
#                     select(id, 
#                            done_fix, 
#                            done_rename,
#                            done_exif,
#                            done_blank,
#                            done_catalog, 
#                            done_tidy, 
#                            done_qc,
#                            done_vault, 
#                            starts_with("error")))
#           }
#         }
#         # For all files that need processing
#         message(paste0(index_message,  n_files, " files"))
#       }
#       if(length(n_files) == 0){
#         cat(paste0("No action needed for ", index_type, "\n"))
#       }
#       
#     }else{
#       if(nrow(filtered_dlog) == 0){
#         cat(paste0("No action needed for ", index_type, "\n"))
#       }
#       if(nrow(filtered_dlog) > 0){
#         message(paste0("ACTION NEEDED - Create ", index_type, " tables for ", length(missing_files), " deployments in dlog"))
#         
#       }
#     }
#     return(all_files)
#   }
# }
# 
