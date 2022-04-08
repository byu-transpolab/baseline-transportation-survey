if(!require(pacman)) install.packages("pacman")
pacman::p_load(targets, tarchetypes, tidyverse, gmapsdistance)

# Set target-specific options such as packages
package_list <- read_lines("package_list.txt")
tar_option_set(packages = package_list)

# Source script files
source("R/data_helpers.R")
source("R/finding_columns.R")
source("R/data_cleaning.R")
source("R/data_visualizations.R")
source("R/data_analysis.R")

# Setting environment variables
gmapsdistance::set.api.key(Sys.getenv("GOOGLE_MAPS_API_KEY"))

# Make output directories
if(!file.exists("output")) dir.create("output")
if(!file.exists("output/data")) dir.create("output/data")
if(!file.exists("output/plots")) dir.create("output/plots")
if(!file.exists("output/tables")) dir.create("output/tables")


################ Targets list ##################################################

#### Targets for cleaning data ####
clean_data_targets <- tar_plan(
  #### Set paths ####
  # tar_target(data_path, "data/survey_data.csv", format = "file"),
  tar_target(data_path, "data/survey_data_4_8.csv", format = "file"),
  tar_target(coords_path, "reference/coords_list.csv", format = "file"),
  tar_target(questions_path, "reference/question_names.csv", format = "file"),
  tar_target(unneeded_cols_path, "reference/unneeded_cols.txt", format = "file"),
  tar_target(mode_categories_path, "reference/mode_categories.csv", format = "file"),
  
  
  #### Info for outputs ####
  data_name = str_replace(data_path, "data/(.+)\\.csv", "\\1"),
  output_final = paste0("output/data/", data_name, "_FINAL.csv"),
  output_plots_dir = paste0("output/plots/", data_name, "/"),
  output_tables_dir = paste0("output/tables/", data_name, "/"),
  
  #### Read data ####
  data_raw = read_csv(data_path),
  unneeded_col_names = get_unneeded_cols(data_raw, unneeded_cols_path),
  question_names = get_question_names(data_raw, questions_path, unneeded_col_names),
  coords_ref = get_coords(coords_path, BYU_coords),
  mode_categories_list = read_csv(mode_categories_path),
  acceptable_modes = mode_categories_list %>% unlist() %>% unname(),
  
  data_formatted = format_data(data_raw, question_names, unneeded_col_names),
  
  
  #### Find required information about columns ####
  first_act_cols = find_first_act_cols(data_formatted),
  last_act_cols = find_last_act_cols(data_formatted),
  rank_cols = find_rank_cols(data_formatted),
  other_cols = find_other_cols(data_formatted),
  zone_order = find_zone_order(first_act_cols, last_act_cols),
  
  
  #### Clean data ####
  data = collapse_filter_data(data_formatted, other_cols),
  
  zones = format_zones(data, first_act_cols, last_act_cols, zone_order),
  ranks = format_rankings(data, rank_cols),
  coords = format_coords(data, coords_ref),
  times = format_times(data, 18, 8),
  
  
  #### Combine data ####
  tables_list = list(zones, ranks, coords, times),
  bad_columns_list = list(
    first_act_cols, last_act_cols, rank_cols,
    "longitude.x", "longitude.y", "latitude.x", "latitude.y", "coord_x", "coord_y",
    "time_arrive", "time_leave"),
  bad_column_names = get_bad_cols(data, bad_columns_list),
  
  data_full = combine_data(data, tables_list, bad_column_names)
  
)

#### Targets for analyzing the data ####
analyze_data_targets <- tar_plan(
  BYU_coords = c(latitude = 40.250318845549025,
                 longitude = -111.64921975843211),
  
  distance = get_distance(data_full, BYU_coords),
  mode_categories = get_mode_categories(data_full, mode_categories_list),
  
  #### Combine data ####
  analysis_list = list(distance, mode_categories),
  data_analyzed = combine_data(data_full, analysis_list),
  
  #### Write data ####
  data_final = write_data(data_analyzed, output_final)
)

#### Targets for summarizing the data ####
summ_data_targets <- tar_plan(
  
)

#### Targets for visualizing the data ####
viz_data_targets <- tar_plan(
  poi_list = c("Heritage Halls",
               # "Wymount Terrace",
               "Wyview Park",
               "Orem",
               "Springville"),
  
  poi = get_poi(coords_ref, poi_list),
  
  mode_choice_graph = create_mode_choice_graph(
    data_final, acceptable_modes, paste0(output_plots_dir,"/mode_choice.png")),
  
  times_graph = create_times_graph(
    data_final, paste0(output_plots_dir,"/arr_dept_times.png")),
  
  distance_by_mode = create_dist_mode_graph(
    data_final, poi, paste0(output_plots_dir,"/distance_by_mode.png"))
  
)

# #### Targets for building the book / article ####
# book_targets <- tar_plan(
#   report = bookdown::render_book(input = ".", output_yaml = "_output.yml", 
#                                  config_file = "_bookdown.yml")
# )

#### Run all targets ####
tar_plan(clean_data_targets,
         analyze_data_targets,
         summ_data_targets,
         viz_data_targets)