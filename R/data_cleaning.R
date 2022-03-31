#' Reformat the qualtrics data
#' 
#' @param data_raw Raw qualtrics data
#' @param question_names Annotated question labels object
#' @param unneeded_col_names Vector of column names that are extraneous
format_data <- function(data_raw, question_names, unneeded_col_names){
  data <- data_raw %>% 
    select(-all_of(unneeded_col_names)) %>% 
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


#' Collapse "other" columns and filter data
#' 
#' @param data Data object
#' @param other_cols Vector of "other" column names
collapse_filter_data <- function(data, other_cols){
  other_cols_index <- which(colnames(data) %in% other_cols)
  data_filtered <- data
  for(i in other_cols_index) data_filtered %<>% replace(i-1, coalesce(data[[i-1]], data[[i]]))
  
  data_filtered %<>% select(-all_of(other_cols)) %>% 
    filter(!is.na(mode))
  
  data_filtered
}


#' Reformat campus zone data
#' 
#' @param data Data object
#' @param first_act_cols Vector of columns pertaining to first activities
#' @param last_act_cols Vector of columns pertaining to last activities
#' @param zone_order Vector listing activity zone order
format_zones <- function(data, first_act_cols, last_act_cols, zone_order){
  #some results say "Like" instead of "On"
  first_act <- data %>% 
    select(all_of(first_act_cols)) %>% 
    mutate(across(.fns = ~ replace(., . == "Like", "On")))
  last_act <- data %>% 
    select(all_of(last_act_cols)) %>% 
    mutate(across(.fns = ~ replace(., . == "Like", "On")))
  
  #find which zones are "On" (only the first in each row)
  first_act_zones <- apply(first_act == "On",
                           1,
                           function(x) min(which(x))) %>% 
    {ifelse(. != Inf, zone_order[.], NA)} #map index to zone via zone_order
  
  last_act_zones <- apply(last_act == "On",
                           1,
                           function(x) min(which(x))) %>% 
    {ifelse(. != Inf, zone_order[.], NA)} #map starting after the first activity zone order
  
  
  #add new activity cols and remove old ones
  data_with_zones <- data %>% 
    mutate(first_activity = first_act_zones,
           last_activity = last_act_zones) %>% 
    select(-all_of(first_act_cols), -all_of(last_act_cols)) %>%
    relocate(first_activity, last_activity, .before = school_year)
  
  data_with_zones
}


#' Reformat rankings for reasons
#' 
#' @param data Data object
#' @param rank_cols_index Vector of "rank" column names
format_rankings <- function(data, rank_cols){
  rankings <- data %>% 
    select(ID, all_of(rank_cols))
  
  rankings_t <- rankings %>% 
    pivot_longer(-ID) %>% 
    filter(!is.na(value)) %>% 
    arrange(ID,value) %>% 
    pivot_wider(ID, names_from = value, values_from = name) %>% 
    #make the values look prettier
    mutate(across(.fns = ~ gsub("rank_", "", .))) %>% 
    mutate(across(-ID, ~ str_to_title(.)))
  
  #re-add rankings to table
  data_with_ranks <- data %>% 
    left_join(rankings_t, by = "ID") %>% 
    rename(rank_1 = `1`,
           rank_2 = `2`,
           rank_3 = `3`,
           rank_4 = `4`,
           rank_5 = `5`) %>% 
    relocate(rank_1:rank_5, .after = reasons) %>%
    #remove old ranking columns and reasons list
    select(-c(all_of(rank_cols), reasons))
  
  data_with_ranks
}


#' Create coordinates for living location
#' 
#' @param data Data object
#' @param coords_list List of custom-defined coordinates for locations
format_coords <- function(data, coords_list){
  #empirical conversion formulas
  coord_longitude <- 111.7083078-(as.numeric(data$coord_x)*0.0000241039393939354)
  coord_latitude <- 40.2824881-(as.numeric(data$coord_y)*0.0000184697272727293)
  
  #join lats/longs based on above and predetermined values (coordslist)
  data_with_coords <- data %>%
    mutate(longitude = coord_longitude, latitude = coord_latitude) %>%
    left_join(coords_list, by = c("complex" = "location")) %>%
    left_join(coords_list, by = c("city" = "location")) %>%
    #coalesce all columns
    mutate(longitude = coalesce(longitude, longitude.x, longitude.y),
           latitude = coalesce(latitude, latitude.x, latitude.y)) %>%
    select(-c(coord_x,coord_y, longitude.x, latitude.x, longitude.y, latitude.y)) %>%
    relocate(longitude, latitude, .after = city)
  
  data_with_coords
}


#' Reformat times
#' 
#' @param data Data object
format_times <- function(data){
  data_times <- data %>% 
    mutate(time_arrive = hm(time_arrive),
           time_leave = hm(time_leave)) %>% 
    mutate(time_arrive = 
             case_when(
               hour(time_arrive) >= 18 ~ time_arrive - hours(12),
               TRUE ~ time_arrive),
           time_leave = 
             case_when(
               hour(time_leave) <= 8 ~ time_leave + hours(12),
               TRUE ~ time_leave))
  
  data_times
}


#' Write cleaned data (and return it)
#' 
#' @param data_clean Cleaned data
#' @param output_path Output path to write file
write_clean_data <- function(data_clean, output_path){
  write_csv(data_clean, output_path)
  data_clean
}