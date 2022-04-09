#' Plot gender distribution
#' 
#' @param data Data
#' @param out_path Output file path
plot_gender_dist <- function(data, out_path){
  gender_plot <- data %>% 
    count(gender) %>%
    filter(!is.na(gender)) %>% 
    ggplot(aes(x = gender, fill = gender, y = n)) +
    theme_minimal() +
    geom_bar(stat = "identity") +
    geom_text(aes(label = n), nudge_y = 20) +
    scale_fill_brewer(palette = "Set2") +
    easy_remove_y_axis() +
    easy_remove_legend(fill) +
    theme(panel.grid = element_blank()) +
    xlab("Gender")
  
  ggsave(out_path, gender_plot,
         width = 6, height = 4, units = "in",
         device = ragg::agg_png, scaling = 1,
         bg = "white")
  
  gender_plot
}
