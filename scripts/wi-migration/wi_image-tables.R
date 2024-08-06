# Load libraries, functions, workflows -----
rm(list = ls())
#
library(tidyverse)   ## To manipulate data frames
library(here)   ## To manage directories
#
source(here("scripts/functions/fxn_utilities.R"))
source(here("scripts/functions/fxn_utilities_wi.R"))
source(here("scripts/functions/fxn_image-tables.R"))
# 
# ---------------------------------------------------------- -----
# Define site and attributes ----
fxn_define_camera_project(index_site)
# ========================================================== -----
# Create all_vault ----
# To create new table (takes about 5 minutes)
# For after new image tables added to vault
# all_vault <- fxn_collate_all_vault(index_type = "new") %>%
#   filter(id %in% unique(dlog_wi$id))
#
# To use existing table 
all_vault <- fxn_collate_all_vault(index_type = "archive") %>%
  filter(id %in% unique(dlog_wi$id))
# 
# Process data in WI format -----
tidy_vault_long <- 
  all_vault %>%
  fxn_create_binomial_count_long() %>%
  fxn_correct_typos() %>%
  fxn_tidy_comments() %>%
  fxn_add_taxonomic_attributes() %>%
  fxn_create_identified_by()


wi_taxonomy <- 
  read_excel(here(path_in, "attributes_species_wi.xlsx"), 
             sheet = "lookup_taxonomy") %>%
  select(wi_taxon_id, binomial:common_name)

tidy_vault_long %>%
  distinct(action, photo_type, binomial) %>%
  mutate(binomial = 
           case_when(binomial == "N/A" ~ photo_type, 
                     TRUE ~ binomial)) %>%
  left_join(wi_taxonomy, "binomial") %>%
  filter(is.na(wi_taxon_id))  


#
# nrow(binomial_count_long)
# 1001047
#
# ---------------------------------------------------------- -----
image <- fxn_create_metadata_image(index_type = "archive")
# #   Write/read csv ----
# image %>%
#   write_csv(here(path_out_wi_migration, 
#                  "metadata", 
#                  "metadata_image_all.csv"))
# # 
# # 
# image <- read_csv(here(path_out_wi_migration, 
#                                 "metadata", 
#                                 "metadata_image_all.csv"),
#                            col_types = cols(
#                              .default = col_character(),
#                              number_of_objects = col_double(),
#                              highlighted = col_integer(),
#                              timestamp = col_datetime(format = "")
#                            ))
# #
# ========================================================== -----

