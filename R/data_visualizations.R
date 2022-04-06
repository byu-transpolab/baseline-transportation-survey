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
    mutate(pct = n / nrow(data)
    ) %>% 
    ggplot(mapping = aes(x = reorder(mode, -n),
                         y = pct,
                         fill = reorder(mode, -n))) +
    theme_minimal() +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(pct*100), "%")), nudge_y = 0.02) +
    labs(x = element_blank(),
         y = element_blank(),
         fill = "Mode") +
    theme(panel.grid = element_blank(),
          legend.position = c(.7,.6)) +
    easy_remove_axes() +
    scale_fill_brewer(palette = "Paired")
  
  ggsave(out_path, mode_choice_graph,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 1,
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
                 values_to = "time"
    ) %>% 
    ggplot(aes(x = time, fill = time_type)) +
    geom_density(aes(y = ..count..), alpha = 0.5, trim = T) +
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
create_dist_mode_graph <- function(data, coords_ref, out_path){
  dist_mode_graph <- data %>%
    # mutate(mode = ifelse(mode_category == "Other",
    #                      "Other",
    #                      mode)) %>%
    filter(!is.na(crow_distance)) %>%
    ggplot() +
    geom_violin(aes(x = crow_distance, y = mode_category), size = 0.8) +
    # geom_density(aes(x = crow_distance, color = mode_category), size = 1, trim = T) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.01))) +
    theme_minimal() +
    labs(x = "Crow Distance (mi)",
         y = element_blank()) +
    easy_remove_legend(fill) +
    geom_vline(aes(xintercept = crow_distance, color = location), coords_ref,
               size = 1) +
    scale_color_brewer(palette = "Dark2") +
    easy_move_legend("bottom") +
    easy_adjust_legend("left") +
    labs(color = "Points of\nInterest")
  
  ggsave(out_path, dist_mode_graph,
         width = 8, height = 4, units = "in",
         device = ragg::agg_png, scaling = 0.5,
         bg = "white")
  
  dist_mode_graph
}
