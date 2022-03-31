if(!require(pacman)) install.packages("pacman")
pacman::p_load(targets, tarchetypes)

# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/finding_columns.R")
source("R/data_cleaning.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "magrittr", "bookdown"))

#### Targets for cleaning data ####
clean_data_targets <- tar_plan(
  #### Set paths ####
  tar_target(data_path, "data/poster_data.csv", format = "file"),
  tar_target(coords_path, "reference/coords_list.csv", format = "file"),
  tar_target(questions_path, "reference/question_names.csv", format = "file"),
  tar_target(unneeded_cols_path, "reference/unneeded_cols.txt", format = "file"),
  tar_target(output_path, "data/poster_data_CLEANED.csv"),
  
  #### Read data ####
  data_raw = read_csv(data_path),
  question_names = get_question_names(data_raw, questions_path, unneeded_col_names),
  unneeded_col_names = get_unneeded_cols(data_raw, unneeded_cols_path),
  data = format_data(data_raw, question_names, unneeded_col_names),
  coords_list = read_csv(coords_path),
  
  #### Find required information about columns ####
  first_act_cols = find_first_act_cols(data),
  last_act_cols = find_last_act_cols(data),
  rank_cols = find_rank_cols(data),
  other_cols = find_other_cols(data),
  zone_order = find_zone_order(first_act_cols, last_act_cols),
  
  #### Clean data ####
  data_filtered = collapse_filter_data(data, other_cols),
  data_with_zones = format_zones(data_filtered, first_act_cols,
                                 last_act_cols, zone_order),
  data_with_ranks = format_rankings(data_with_zones, rank_cols),
  data_with_coords = format_coords(data_with_ranks, coords_list),
  
  #### Write cleaned data ####
  data_clean = write_clean_data(data_with_coords, output_path)
  
)

#### Targets for visualizing the data ####
viz_data_targets <- tar_plan(
  
)

# #### Targets for building the book / article ####
# book_targets <- tar_plan(
#   report = bookdown::render_book(input = ".", output_yaml = "_output.yml", 
#                                  config_file = "_bookdown.yml")
# )

#### Run all targets ####
tar_plan(clean_data_targets,
         viz_data_targets)