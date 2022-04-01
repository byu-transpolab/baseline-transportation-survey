#' Gets distance between point and BYU campus
#' 
#' @param data
#' @param BYU_coords
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
    mutate(crow_distance = c(st_distance(geometry, BYU_sf$geometry))) %>% 
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