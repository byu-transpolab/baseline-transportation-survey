#' Find first activity columns
#' 
#' @param data Data object
find_first_act_cols <- function(data){
  first_act_col_index <- data %>% 
    colnames() %>% 
    str_detect("first_activity")
  
  first_act_cols <- data %>% 
    colnames %>% 
    {.[first_act_col_index]}
  
  first_act_cols
}


#' Find last activity columns
#' 
#' @param data Data object
find_last_act_cols <- function(data){
  last_act_col_index <- data %>% 
    colnames() %>% 
    str_detect("last_activity")
  
  last_act_cols <- data %>% 
    colnames %>% 
    {.[last_act_col_index]}
  
  last_act_cols
}


#' Find zone order for first and last activity columns
#' 
#' @param first_act_cols Vector of columns pertaining to first activities
#' @param last_act_cols Vector of columns pertaining to last activities
find_zone_order <- function(first_act_cols, last_act_cols){
  first_zone_order <- first_act_cols %>% 
    str_extract("\\d+") %>% 
    as.numeric()
  last_zone_order <- last_act_cols %>% 
    str_extract("\\d+") %>% 
    as.numeric()
  
  zone_order <- c(first_zone_order, last_zone_order) 
  
  #check that first_ and last_ have the same number of zones
  if((length(first_zone_order) != length(last_zone_order)) ||
     (length(zone_order) != 2*length(first_zone_order))){
    simpleWarning("The number of zones in the first_ and last_ activities don't match.")
  }
  
  zone_order
}


#' Find "rank" columns
#' 
#' @param data Data object
find_rank_cols <- function(data){
  rank_cols <- data %>% 
    colnames() %>% 
    str_detect("rank_") %>% 
    which() %>% 
    {colnames(data)[.]}
    
  rank_cols
}



#' Find columns with "other" text
#' 
#' @param data Data object
find_other_cols <- function(data){
  other_cols <- data %>% 
    colnames() %>% 
    str_detect("_other") %>% 
    which() %>% 
    {colnames(data)[.]}
  
  other_cols
}