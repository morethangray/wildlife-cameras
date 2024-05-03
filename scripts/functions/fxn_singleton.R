# Combine all files in directory ----
# Set the directory path
dir_path <- path_table_qc

# List all xlsx files in the directory
file_paths <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read each file and combine them into a single tibble
combined_data <- file_paths %>% 
  map_df(~read_xlsx(.x))

# Get distinct comments ----
# index_path = path_table_qc

fxn_get_distinct_comments <- function(index_site, index_path){
  fxn_define_camera_project(index_site)
  
  # 1. Read all xlsx files in a folder
  file_paths <- list.files(path = index_path, 
                           pattern = "\\.xlsx$", 
                           full.names = TRUE)
  
  # 2. Extract distinct comments from each file
  comments_list <- map(file_paths, function(file_path) {
    # Read the file
    data <- read_xlsx(file_path)
    
    # Check if 'comments' column exists
    if("comments" %in% names(data)) {
      # Extract distinct comments
      distinct(data, comments) %>% pull(comments)
    } else {
      # Return NA if 'comments' column does not exist
      NA
    }
  })
  
  # Flatten the list of comments into a single vector
  all_comments <- unlist(comments_list)
  
  # Remove NAs and extract unique comments across all files
  distinct_comments <- unique(na.omit(all_comments))
  
  # 3. Create a tibble with all distinct comments
  final_data <- tibble(distinct_comments = distinct_comments)
}
# final_data %>%
#   write_csv(here(path_out, 
#                  "distinct-comments.csv"))
