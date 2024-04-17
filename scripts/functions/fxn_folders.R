# updated: 2023-12-20 ----
# ========================================================== -----
# Check image folders ----
#   fxn_dir_jpg_map ----
fxn_dir_jpg_map <- function(index_site, index_year){
  
  fxn_define_camera_project(index_site)
  
  list_id_dlog <- unique(dlog$id)
  
  map_dir <-
    tibble(path = 
             dir_ls(path = here(path_jpg, 
                                index_year),
                    recurse = TRUE, 
                    type = "directory")) %>%
    
    mutate(
      folder_name = path_file(path),
      directory_name = path_dir(path),
      parent_name = path_file(directory_name),
      camera_name = str_sub(folder_name, 1, 4), 
      # Check names 
      err_camera_folder = camera_name %nin% list_cameras
    ) %>%
    
    filter(
      # Exclude camera folders 
      folder_name %nin% list_cameras,
      # Exclude archive folders
      str_detect(path, "archive") == FALSE
    )  %>%
    rename(id = folder_name) %>%
    left_join(dlog, "id") %>%
    
    # Get attributes from file name
    mutate(
      date_to_char = paste0("20", substrRight(id, 6)),
      date_to_folder = ymd(date_to_char),
      year_to_folder = str_sub(date_to_char, 1, 4),
      
      err_id = !id %in% list_id_dlog,
      
      err_date_to = date_to != date_to_folder,
      err_camera = camera != camera_name,
      err_year_to = year_to != year_to_folder
    )  %>%
    
    select(id,
           starts_with("err_"), 
           starts_with("error"), 
           camera_name, 
           parent_name,
           path,
           date_from, 
           date_to, 
           date_excl_from, 
           date_excl_to) 
  
  
}
#   fxn_dir_jpg_rename ----
# index_pattern = c("YYYYMMDD_YYYYMMDD", 
#                   "YYYYMMDD")
# index_pattern = "YYYYMMDD_YYYYMMDD"
fxn_dir_jpg_rename <- function(index_site,
                               index_year, 
                               index_pattern, 
                               do_rename){
  
  fxn_define_camera_project(index_site)
  
  map_dir <-
    fxn_dir_jpg_map(index_site, index_year)
  
  if(index_pattern == "YYYYMMDD_YYYYMMDD"){
    
    rename_path <- 
      map_dir %>%
      filter(err_id == TRUE) %>%
      mutate(id_new = 
               paste(parent_name, 
                     substrRight(id, 6), 
                     sep = "_"), 
             path_new = 
               str_replace(path, 
                           id, 
                           id_new)) %>%
      select(id_new,
             id,
             path_new,
             path) 
    
  }
  
  print(rename_path)
  
  if(do_rename == TRUE){
    
    file.rename(from = rename_path$path,
                to = rename_path$path_new)
  }
}

# ========================================================== -----
