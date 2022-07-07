plot_calendar_plot <- function(){
  
  library(extrafont)
  library(shadowtext)
  
  calendar_data <- tibble::tibble(date = seq(as.Date("2019-08-01"), as.Date("2019-08-31"), by = "days"),
                                  value = c(rep(80, 5), 120, rep(80, 2), 40, rep(30, 4), rep(40, 4),
                                            120, 30, 40, 120, 30, rep(40, 4), 120, 80, 40, 120,
                                            80),
                                  day = lubridate::day(date),
                                  day_week = lubridate::wday(date, label = TRUE, week_start = 6),
                                  week = lubridate:::.other_week(date, week_start = 6),
                                  month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>% 
    #Add variable text colour
    dplyr::mutate(fill_grp = as.factor(dplyr::case_when(value < 40 ~ "less than 40",
                                              between(value, 40, 79) ~ "40 - 79",
                                              between(value, 80, 119) ~ "80 - 119",
                                              value >= 120 ~ "120 or more")),
                  text_col = c(rep(TRUE, 9), rep(FALSE, 8), TRUE, rep(FALSE, 2), TRUE, rep(FALSE, 5), rep(TRUE, 2),
                               FALSE, TRUE, TRUE)) %>% 
    dplyr::mutate(fill_grp = forcats::fct_relevel(fill_grp, c("less than 40", "40 - 79", "80 - 119", "120 or more")))
  
  worst_date <- calendar_data %>% 
    dplyr::slice(11, 18, 27) %>% 
    dplyr::mutate(box_text = c("13 people died on the first day of Eid",
                               "92 people died when IS bombed a wedding",
                               "162 people are confirmed to have died"))
  
  my_plot <- ggplot(data = calendar_data) +
    geom_tile(aes(x = day_week, y = week, fill = fill_grp), width = 0.9, height = 0.9, size = 1) +
    geom_tile(data = worst_date, aes(x = day_week, y = week), fill = NA, width = 0.95, height = 0.95,
              colour = "black", size = 1) +
    geom_text(aes(x = as.numeric(day_week) - 0.39, y = week - 0.3, label = day, colour = text_col),
              family = "Ubuntu", size = 5, hjust = 0) +
    scale_y_reverse() +
    scale_x_discrete(position = "top") +
    scale_colour_manual(values = c("black", "white")) +
    scale_fill_manual(values = c("#E79F9F", "#D56666", "#990000", "#5B0000")) +
    labs(title = "August was a month of constant conflict",
         subtitle = "Number of people confirmed to have died per day") +
    theme_classic() +
    theme(legend.position = "top",
          legend.justification = c(0, 0),
          legend.text = element_text(family = "Ubuntu", size = 14),
          legend.text.align = 0, 
          legend.spacing.x = unit(x = 0.25, units = "cm"),
          axis.line = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(colour = "black", size = 12),
          plot.title = element_text(family = "Ubuntu", face = "bold", colour = "black",
                                          size = 24, vjust = 3),
          plot.subtitle = element_text(family = "Ubuntu", colour = "black",
                                       size = 17, vjust = 4),
          plot.background = element_rect(fill = "white", size = 5),
          panel.background = element_rect(fill = "white"),
          plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "mm")) +
    guides(fill = guide_legend(title = "", direction = "horizontal"),
           colour = "none")
  
  bbc_plot <- ggplot(data = calendar_data) +
    geom_tile(aes(x = day_week, y = week, fill = fill_grp), width = 0.9, height = 0.9, size = 1) +
    geom_tile(data = worst_date, aes(x = day_week, y = week), fill = NA, width = 0.95, height = 0.95,
              colour = "black", size = 1) +
    geom_text(aes(x = as.numeric(day_week) - 0.39, y = week - 0.3, label = day, colour = text_col),
              family = "Ubuntu", size = 5, hjust = 0) +
    scale_y_reverse() +
    scale_x_discrete(position = "top") +
    scale_colour_manual(values = c("black", "white")) +
    scale_fill_manual(values = c("#E79F9F", "#D56666", "#990000", "#5B0000")) +
    labs(title = "August was a month of constant conflict",
         subtitle = "Number of people confirmed to have died per day") +
    bbplot::bbc_style() +
    guides(colour = "none") +
    theme(axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.justification = c(0, 0))
  
  
  library(patchwork)
  
  ggsave(plot = my_plot + bbc_plot, filename = "bbc_plot.png", width = 16, height = 8, dpi = 600, type = "cairo")
    
  
}