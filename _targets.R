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
  tar_target(datapath, "data/test_data.csv", format = "file"),
  tar_target(coordspath, "reference/coords_list.csv", format = "file"),
  tar_target(questionspath, "reference/question_names.csv", format = "file"),
  tar_target(data, get_data(datapath, questionspath))
  
  
)
