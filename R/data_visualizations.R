#' Create mode choice bar graph
#' 
#' @param data The data to be graphed
#' @param acceptable_modes A vector of modes to be included
#' @param out_path Path to save image
create_mode_choice_graph <- function(data, acceptable_modes, out_path){
  mode_choice_graph <- data %>% 
    mutate(mode = ifelse(mode %in% acceptable_modes,
                         mode,
                         "Other")) %>%
    count(mode) %>% 
    mutate(pct = n / sum(n)) %>% 
    ggplot(aes(x = reorder(mode, -n),
               y = pct,
               fill = reorder(mode, -n))) +
    theme_minimal() +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(pct*100, 1), "%")),
              nudge_y = 0.02) +
    labs(x = element_blank(),
         y = element_blank(),
         fill = "Mode") +
    theme(panel.grid = element_blank()) +
    easy_remove_axes() +
    scale_fill_brewer(palette = "Paired")
  
  ggsave(out_path, mode_choice_graph,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 0.8,
         bg = "white")
  
  mode_choice_graph
}


#' Create graph of arrival and departure times
#'
#' @param data The data to be plotted
#' @param out_path Path to save image
create_times_graph <- function(data, out_path){
  times_graph <- data %>% 
    filter(!is.na(time_arrive),
           !is.na(time_leave)) %>% 
    mutate(time_arrive = as.numeric(time_arrive),
           time_leave = as.numeric(time_leave)) %>%
    pivot_longer(c(time_arrive, time_leave),
                 names_to = "time_type",
                 values_to = "time_value"
    ) %>% 
    ggplot(aes(x = time_value, fill = time_type)) +
    geom_density(aes(y = stat(count)), alpha = 0.5, trim = T) +
    scale_x_time(
      labels = function(x){
      str_replace(x, "([0-9]+:[0-9]+):[0-9]+", "\\1")},
      limits = c(hm("4:00"), hm("24:30"))
      ) +
    ylim(c(0, NA)) +
    labs(x = element_blank(),
         fill = element_blank()) +
    theme_minimal() +
    theme(legend.position = "bottom",
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    easy_remove_y_axis() +
    scale_fill_manual(labels = c("Arrival Time", "Departure Time"),
                      values = c("red", "blue"))

    
  ggsave(out_path, times_graph,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 1,
         bg = "white")
  
  times_graph
}


#' Create graph of trip length by mode
#'
#' @param data The data to be plotted
#' @param poi Table of reference coordinates with distances of points of interest
#' @param out_path Path to save image
create_dist_mode_graph <- function(data, poi, out_path){
  dist_mode_graph <- data %>%
    # mutate(mode = ifelse(mode_category == "Other",
    #                      "Other",
    #                      mode)) %>%
    filter(!is.na(crow_distance)) %>%
    ggplot() +
    geom_violin(aes(x = crow_distance, y = mode_category),
                size = 0.8, scale = "area") +
    # geom_density(aes(x = crow_distance, color = mode_category), size = 1, trim = T) +
    scale_x_continuous(limits = c(0, 7), expand = expansion(mult = c(0, 0.01))) +
    theme_minimal() +
    labs(x = "Crow-Fly Distance (mi)",
         y = element_blank()) +
    easy_remove_legend(fill) +
    geom_vline(aes(xintercept = crow_distance, color = location), poi,
               size = 1) +
    scale_color_brewer(palette = "Dark2") +
    easy_move_legend("bottom") +
    labs(color = "Points of Interest:")
  
  ggsave(out_path, dist_mode_graph,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 0.5,
         bg = "white")
  
  dist_mode_graph
}


#' Create mode choice by weather
#' 
#' @param data Data
#' @param breaks Breaks to bin temperature data
#' @param labels Labels for breaks
#' @param out_path output path
mode_choice_by_weather <- function(data, breaks, labels, out_path){
  weather_modes <- data %>%
    filter(!is.na(TMAX),
           mode_category != "Other") %>% 
    mutate(TMAX_binned = cut(TMAX, breaks, labels, ordered_result = T),
           # mode_category = case_when(
           #   mode_category %in% c("Walk", "Bike") ~ "Outside",
           #   T ~ "Inside"
           # )
           ) %>% 
    group_by(TMAX_binned, mode_category) %>%
    summarize(n = n()) %>% 
    mutate(pct = n/sum(n),
           n_obs = sum(n)) %>% 
    ggplot(aes(x = TMAX_binned, shape = mode_category, y = pct)) +
    geom_point() +
    geom_text(aes(x = TMAX_binned, y = 0, label = paste("n =", n_obs))) +
    theme_minimal() +
    theme(panel.grid = element_line(color = "grey70", size = 0.2)) +
    labs(x = "Maximum Daily Temperature",
         y = "Mode Share", 
         shape = "Mode")
  
  ggsave(out_path, weather_modes,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 1,
         bg = "white")
  
  weather_modes
}