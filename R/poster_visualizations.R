#' Create mode choice bar graph
#' 
#' @param data_clean The data to be graphed
#' @param out_path Path to save image
create_mode_choice_graph <- function(data_clean, out_path){
  mode_choice_graph <- data_clean %>% 
    # filter(! mode %in% c("Other (specify):")) %>% 
    mutate(across(.fns = ~ str_replace(.x," \\(.+\\):?", ""))) %>%
    count(mode) %>% 
    mutate(pct = n / nrow(data_clean)) %>% 
    ggplot(mapping = aes(x = reorder(mode, -n), y = pct, fill = reorder(mode, -n))) +
    theme_minimal() +
    geom_bar(stat = "identity") +
    geom_text(aes(label = paste0(round(pct*100), "%")),nudge_y = 0.02,
              size = 12) +
    labs(x = element_blank(),
         y = element_blank(),
         fill = "Mode") +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = c(.7,.6),
          legend.key.size = unit(1.5,"cm"),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 32)) +
    scale_fill_manual(values = wes_palette("FantasticFox1", 9, "continuous"))
  
  ggsave(out_path, mode_choice_graph,
         width = 40, height = 30,
         units = "cm",
         bg = NULL)
  
  mode_choice_graph
}


#' Create graph of arrival and departure times
#'
#' @param data_clean The data to be plotted
#' @param out_path Path to save image
create_times_graph <- function(data_clean, out_path){
  times_graph <- data_clean %>% 
    mutate(time_arrive = as.numeric(time_arrive),
           time_leave = as.numeric(time_leave)) %>%
    pivot_longer(c(time_arrive, time_leave), names_to = "time_type", values_to = "time") %>% 
    ggplot() +
    geom_dotplot(aes(x = time, fill = time_type, color = time_type),
                 stackdir = "center",
                 dotsize = 0.2,
                 position = position_jitter()) +
    theme_minimal() +
    scale_y_continuous(NULL, breaks = NULL) +
    scale_x_time(labels = function(x) str_replace(x, "([0-9]+:[0-9]+):[0-9]+", "\\1")) +
    scale_color_manual(labels = c("Arrival Time", "Departure Time"), values = c("red", "blue")) +
    scale_fill_manual(labels = c("Arrival Time", "Departure Time"), values = c("red", "blue")) +
    labs(x = element_blank(),
         fill = element_blank(),
         color = element_blank())
    
  ggsave(out_path, times_graph,
         width = 50, height = 30,
         units = "cm",
         bg = "white")
  
  times_graph
}
