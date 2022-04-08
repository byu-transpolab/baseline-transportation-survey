#' Collapse "other" columns and filter data
#' 
#' @param data Data object
#' @param other_cols Vector of "other" column names
collapse_filter_data <- function(data, other_cols){
  
  other_cols_index <- which(colnames(data) %in% other_cols)
  
  for(i in other_cols_index){
    data[,i-1] <- ifelse(data[,i-1] %>% 
                           as_vector() %>%
                           str_detect("Other"),
                         unlist(data[,i]),
                         unlist(data[,i-1]))
  }
  
  data_filtered <- data %>%
    select(-all_of(other_cols)) %>% 
    filter(!is.na(mode)) %>%
    mutate(city = case_when(city == "PG" ~ "Pleasant Grove",
                            city == "SLC" ~ "Salt Lake City",
                            T ~ city),
           city = str_to_title(city))
  
  data_filtered
}


#' Reformat campus zone data
#' 
#' @param data Data object
#' @param first_act_cols Vector of columns pertaining to first activities
#' @param last_act_cols Vector of columns pertaining to last activities
#' @param zone_order Vector listing activity zone order
format_zones <- function(data, first_act_cols, last_act_cols, zone_order){
  #some results may say "Like" instead of "On"
  first_act <- data %>% 
    select(all_of(first_act_cols)) %>% 
    mutate(across(.fns = ~ replace(., . == "Like", "On")))
  last_act <- data %>% 
    select(all_of(last_act_cols)) %>% 
    mutate(across(.fns = ~ replace(., . == "Like", "On")))
  
  #find which zones are "On" (only the first in each row)
  first_act_zones <- apply(first_act == "On", 1, \(x) min(which(x))) %>% 
    {ifelse(. != Inf, zone_order[.], NA)} #map index to zone via zone_order
  
  last_act_zones <- apply(last_act == "On", 1, \(x) min(which(x))) %>% 
    {ifelse(. != Inf, zone_order[. + length(first_act_cols)],
            NA)} #map starting after the first activity zone order
  
  
  #return new activity cols
  zones <- data %>% 
    mutate(first_activity = first_act_zones,
           last_activity = last_act_zones) %>% 
    select(ID, first_activity, last_activity)
  
  zones
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
  
  #return rankings
  ranks <- data %>% 
    left_join(rankings_t, by = "ID") %>% 
    rename(rank_1 = `1`,
           rank_2 = `2`,
           rank_3 = `3`,
           rank_4 = `4`,
           rank_5 = `5`) %>% 
    select(ID, rank_1, rank_2, rank_3, rank_4, rank_5)
  
  ranks
}


#' Create coordinates for living location
#' 
#' @param data Data object
#' @param coords_list List of custom-defined coordinates for locations
format_coords <- function(data, coords_list){
  #empirical conversion formulas
  coord_longitude <- -(111.7083078-(as.numeric(data$coord_x)*0.0000241039393939354))
  coord_latitude <- 40.2824881-(as.numeric(data$coord_y)*0.0000184697272727293)
  
  #return lats/longs based on above and predetermined values (coordslist)
  coords <- data %>% 
    mutate(longitude = coord_longitude, latitude = coord_latitude) %>%
    left_join(coords_list, by = c("complex" = "location")) %>%
    left_join(coords_list, by = c("city" = "location")) %>%
    #coalesce all columns
    mutate(longitude = coalesce(longitude, longitude.x, longitude.y),
           latitude = coalesce(latitude, latitude.x, latitude.y)) %>%
    select(ID, longitude, latitude)
  
  coords
}


#' Reformat times
#' 
#' @param data Data object
#' @param arrive_before The hour (24h) which all respondents should arrive before.
#' If a respondent puts a time equal to or after this, we assume an AM/PM error.
#' @param depart_after The hour (24h) which all respondents should depart after.
#' If a respondent puts a time equal to or before this, we assume an AM/PM error.
format_times <- function(data, arrive_before, depart_after){
  times <- data %>% 
    filter(!is.na(time_arrive),
           !is.na(time_leave)) %>% 
    mutate(time_arrive = hm(time_arrive),
           time_leave = hm(time_leave)) %>% 
    mutate(time_arrive = 
             case_when(
               hour(time_arrive) >= arrive_before ~ time_arrive - hours(12),
               TRUE ~ time_arrive),
           time_leave = 
             case_when(
               hour(time_leave) <= depart_after ~ time_leave + hours(12),
               TRUE ~ time_leave)) %>% 
    select(ID, time_arrive, time_leave)
  
  times
}
