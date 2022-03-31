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
    geom_text(aes(label = paste0(round(pct*100), "%")), nudge_y = 0.02) +
    labs(x = element_blank(),
         y = element_blank(),
         fill = "Mode") +
    theme(axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())
  
  ggsave(out_path, mode_choice_graph)
  
  mode_choice_graph
}


#' Create graph of arrival and departure times
#'
#'@param data_clean The data to be plotted
#' @param out_path Path to save image
create_times_graph <- function(data_clean, out_path){
  data_clean$time_arrive %<>%
    as.character() %>% 
    parse_time("%H:%M")
  data_clean$time_leave %<>%
    as.character() %>% 
    parse_time("%H:%M")
  
  # data_clean %>% 
  #   ggplot() +
  #   geom

}
