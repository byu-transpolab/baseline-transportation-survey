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


#' Plot college distribution
#' 
#' @data Data
#' @param out_path Output file path
plot_college_dist <- function(data, out_path){
  college_plot <- data %>% 
    count(college) %>%
    filter(!is.na(college)) %>% 
    {ggplot(data = ., aes(x = college, fill = college, y = n)) +
        theme_minimal() +
        geom_bar(stat = "identity") +
        geom_text(aes(label = n), nudge_y = 5) +
        scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(nrow(.))) +
        easy_remove_y_axis() +
        easy_remove_legend(fill) +
        easy_rotate_x_labels(45, "right") +
        theme(panel.grid = element_blank()) +
        labs(x = element_blank())}
  
  ggsave(out_path, college_plot,
         width = 6, height = 4, units = "in",
         device = ragg::agg_png, scaling = 0.6,
         bg = "white")
  
  college_plot
}