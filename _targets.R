# library(targets)
# library(tarchetypes)
# # This is an example _targets.R file. Every
# # {targets} pipeline needs one.
# # Use tar_script() to create _targets.R and tar_edit()
# # to open it again for editing.
# # Then, run tar_make() to run the pipeline
# # and tar_read(summary) to view the results.
# 
# # Set target-specific options such as packages.
# tar_option_set(packages = c("tidyverse", "bookdown"))
# 
# # Define custom functions and other global objects.
# # This is where you write source(\"R/functions.R\")
# # if you keep your functions in external scripts.
# source("R/functions.R")
# 
# 
# data_targets <- tar_plan(
#   data =  data.frame(x = sample.int(100), y = sample.int(100)),
#   summary = summ(data) # Call your custom functions as needed.
# )
# 
# 
# 
# # Targets necessary to build the book / article
# book_targets <- tar_plan(
#   report = bookdown::render_book(input = ".", output_yaml = "_output.yml", 
#                                  config_file = "_bookdown.yml")
# )
# 
# 
# 
# # run all targets
# tar_plan(
#   data = data_targets, 
#   book = book_targets
# )

library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
source("R/data_cleaning.R")


# Set target-specific options such as packages.
tar_option_set(packages = c("tidyverse", "magrittr"))

# End this file with a list of target objects.
list(
  ### Set parameters ###
  tar_target(datapath, "data/test_data_monday.csv", format = "file"),
  tar_target(coordspath, "reference/coords_list.csv", format = "file"),
  tar_target(questionspath, "reference/question_names.txt", format = "file"),
  tar_target(unneededcols, expr(c(StartDate:Finished, LastName:Language))),
  tar_target(othercols, c("mode", "complex", "city", "hs_modes", "gender", "parent_education")),
  tar_target(zoneorder, c(5, 12, 6, 7, 9, 8, 10, 11, 4, 2)), #order of zones in activity cols
  tar_target(firstactcols, expr(first_activity_5:first_activity_2)),
  tar_target(lastactcols, expr(last_activity_5:last_activity_2)),
  tar_target(rankcols, expr(rank_parking:rank_na)),
  tar_target(outputpath, "data/test_data_monday_CLEANED.csv"),
  
  ### Clean data ###
  tar_target(data, clean_data(datapath, coordspath, questionspath, unneededcols,
                              othercols, zoneorder, firstactcols, lastactcols,
                              rankcols, outputpath))
  
)
