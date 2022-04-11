#' Reformat the qualtrics data
#' 
#' @param data_raw Raw qualtrics data
#' @param question_names Annotated question labels object
#' @param unneeded_col_names Vector of column names that are extraneous
format_data <- function(data_raw, question_names, unneeded_col_names){
  data <- data_raw %>% 
    select(-any_of(unneeded_col_names)) %>% 
    rename_with(~ question_names$new_name, all_of(question_names$old_name)) %>% 
    {.[-(1:2),]} %>% #remove the first two rows due to their unhelpfulness
    relocate(ID)
  
  data
}


#' Gets unneeded column names
#' 
#' @param data_raw Raw qualtrics data
#' @param unneeded_cols_path Path to file listing extraneous columns
get_unneeded_cols <- function(data_raw, unneeded_cols_path){
  unneeded_cols <- read_lines(unneeded_cols_path)
  unneeded_col_names <- unneeded_cols[unneeded_cols %in% colnames(data_raw)]
  
  unneeded_col_names
}



#' Gets question names for existing columns
#' 
#' @param data_raw Raw qualtrics data
#' @param questions_path Path to annotated question labels file
#' @param unneeded_col_names Vector of column names that are extraneous
get_question_names <- function(data_raw, questions_path, unneeded_col_names){
  question_names <- read_csv(questions_path) %>% 
    filter(old_name %in% colnames(data_raw),
           !old_name %in% unneeded_col_names)
  
  question_names
}


#' Gets coords list and calculates distance to BYU
#' 
#' @param coords_path Path to coords_list file
#' @param BYU_coords Named (latitude/longitude) vector of BYU coordinates
get_coords <- function(coords_path, BYU_coords){
  coords <- read_csv(coords_path)
  
  coords_sf <- coords %>% 
    filter(!is.na(latitude),
           !is.na(longitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  BYU_sf <- BYU_coords %>% 
    as_tibble_row() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  distances <- coords_sf %>% 
    mutate(crow_distance = c(st_distance(geometry, BYU_sf$geometry)) %>% 
             drop_units() %>% 
             multiply_by(0.000621371)) %>% #convert to miles 
    as_tibble() %>% 
    select(-geometry)
  
  coords_list <- coords %>% 
    left_join(distances, by = "location")
  
  coords_list
}


#' Gets bad columns to be removed at the end
#'
#' @param data The data to compare column names against
#' @param bad_columns_list A list of column names to remove
get_bad_cols <- function(data, bad_columns_list){
  bad_column_names <- bad_columns_list %>%
    unlist() %>% 
    unname()
  
  bad_column_names
}


#' Gets Points of Interest from coordinate reference file and makes location
#' a factor based on distance
#' 
#' @param coords_ref Coordinate reference file
#' @param poi_list Character vector of locations to include
get_poi <- function(coords_ref, poi_list){
  poi <- coords_ref %>% 
    filter(location %in% poi_list)
  
  poi$location <- poi$location %>% 
    factor() %>% 
    reorder(poi$crow_distance)
  
  poi
}


#' Combine data
#' 
#' @param data The original data
#' @param tables_list A list of table objects containing the data to join
#' @param bad_column_names A character vector of column names to remove
combine_data <- function(data, tables_list, bad_column_names = NULL){
  
  #remove old (bad) columns
  data_full <- data %>% 
    select(-any_of(bad_column_names))
  
  #join new tables
  for(i in tables_list){
    data_full <- 
      left_join(data_full, i, by = "ID")
  }
  
  data_full
}


#' Write data (and return it)
#' 
#' @param data Data to write
#' @param output_path Output path to write file
write_data <- function(data, output_path){
  write_csv(data, output_path)
  data
}