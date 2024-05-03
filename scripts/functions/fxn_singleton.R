# Set the directory path
dir_path <- "path/to/your/folder"

# List all xlsx files in the directory
file_paths <- list.files(path = dir_path, pattern = "\\.xlsx$", full.names = TRUE)

# Read each file and combine them into a single tibble
combined_data <- file_paths %>% 
  map_df(~read_xlsx(.x))

# Print or return the combined tibble
print(combined_data)