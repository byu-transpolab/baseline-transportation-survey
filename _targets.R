if(!require(pacman)) install.packages("pacman")
pacman::p_load(targets, tarchetypes, tidyverse)

# Set target-specific options such as packages.
package_list <- read_lines("package_list.txt")
tar_option_set(packages = package_list)

# Source script files
source("R/finding_columns.R")
source("R/data_cleaning.R")
source("R/data_visualizations.R")


# Set file paths
data_file <- "data/poster_data.csv"
coords_file <- "reference/coords_list.csv"
questions_file <- "reference/question_names.csv"
unneeded_cols_file <- "reference/unneeded_cols.txt"


# Info for outputs
output_file <- str_replace(data_file,
                           "data/(.+)\\.csv",
                           "output/data/\\1_CLEANED\\.csv")
output_plots_dir <- "output/plots"

# Misc info
BYUcoords <- c(longitude = 40.250318845549025,
               latitude = -111.64921975843211)



#### Targets for cleaning data ####
clean_data_targets <- tar_plan(
  #### Set paths ####
  tar_target(data_path, data_file, format = "file"),
  tar_target(coords_path, coords_file, format = "file"),
  tar_target(questions_path, questions_file, format = "file"),
  tar_target(unneeded_cols_path, unneeded_cols_file, format = "file"),
  tar_target(output_path, output_file),
  
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
  data_with_times = format_times(data_with_coords),
  
  #### Write cleaned data ####
  data_clean = write_clean_data(data_with_times, output_path)
  
)

#### Targets for analyzing the data ####
analyze_data_targets <- tar_plan(
  BYU_coords = BYUcoords
)

#### Targets for visualizing the data ####
viz_data_targets <- tar_plan(
  mode_choice_graph = create_mode_choice_graph(
    data_clean, paste0(output_plots_dir,"/mode_choice.png")),
  times_graph = create_times_graph(
    data_clean, paste0(output_plots_dir,"/arr_dept_times.png")),
  distance_by_mode = create_dist_mode_graph(
    data_clean, paste0(output_plots_dir,"/distance_by_mode.png"))
)

# #### Targets for building the book / article ####
# book_targets <- tar_plan(
#   report = bookdown::render_book(input = ".", output_yaml = "_output.yml", 
#                                  config_file = "_bookdown.yml")
# )

#### Run all targets ####
tar_plan(clean_data_targets,
         analyze_data_targets,
         viz_data_targets)