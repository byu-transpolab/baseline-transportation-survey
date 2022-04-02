#' Gets distance between point and BYU campus
#' 
#' @param data Data object
#' @param BYU_coords Named vector of BYU latitude and longitude
#' @param mode Mode to use for distance calculations
#' (the mode for distance to BYU is set as bike since driving routing isn't great)
get_distance <- function(data, BYU_coords, mode = "bicycling"){
  origins <- data %>% 
    select(ID, latitude, longitude) %>% 
    filter(!is.na(latitude),
           !is.na(longitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  BYU_sf <- BYU_coords %>% 
    as_tibble_row() %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  
  distances <- origins %>% 
    mutate(crow_distance = c(st_distance(geometry, BYU_sf$geometry)) %>% 
             drop_units() %>% 
             multiply_by(0.000621371)) %>% #convert to miles 
    as_tibble() %>% 
    select(-geometry)
  
  # matrixs <- origins[1:10,] %>%
  #   group_by(1:n()) %>%
  #   mutate(matr = ifelse(is.na(latitude) || is.na(longitude),
  #                        NA,
  #                        mp_matrix(c(longitude, latitude),
  #                                  destination = c(BYU_coords["longitude"],
  #                                                  BYU_coords["latitude"]),
  #                                  mode = "bicycling",
  #                                  key = Sys.getenv("GOOGLE_MAPS_API_KEY")) %>% 
  #                          mp_get_matrix(value = "distance_m")))

  distances
}


#' Adds mode categories
#' 
#' @param data Data object
#' @param categories A table listing modes by category
get_mode_categories <- function(data, categories){
  mode_categories <- data %>% 
    mutate(mode_category =
             #finds index of column in 'categories' containing mode
             map(mode,
                 ~ which(unlist(map(categories,
                                    \(x) any(x == .x))))) %>%
             #replaces empty list elements with NA
             map(~ ifelse(length(.x) == 0, NA, .x)) %>% 
             unlist() %>% 
             #names mode category based on index, or 'Other' if NA
             {ifelse(!is.na(.), colnames(categories)[.], "Other")}) %>% 
    select(ID, mode_category)
  
  mode_categories
}