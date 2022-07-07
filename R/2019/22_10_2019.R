plot_22_10_19 <- function(){
  
  #Set ggplot theme
  current_theme <- theme_classic() +
    theme(legend.position = c(0, 0.99),
          legend.direction = "horizontal",
          legend.margin = margin(0, 0, 0, -5),
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
          plot.margin = margin(t = 10, r = 10, b = 10, l = 0, unit = "mm"))
  
  theme_set(current_theme)
  
  #Load data
  horror_movies <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")
  
  movies_by_week <- horror_movies %>% 
    #Create date item
    dplyr::mutate(date = lubridate::dmy(release_date),
                  week = lubridate::week(date),
                  month = lubridate::month(date, label = TRUE, abbr = FALSE)) %>% 
    #Remove movies where no exact week could be determined
    dplyr::filter(!is.na(week)) %>% 
    #Now we can group by each week of the year and determine the number of horror movies released
    dplyr::group_by(week) %>% 
    dplyr::summarise(n = n()) %>% 
    #Make weeks into groups of 4 for our plotting axis (y)
    #The first way I thought to do this was a recursive function using modulo
    dplyr::mutate(four_week_grp = (week - 1) %/% 8) %>% 
    #Then go through and give them all value 1 - 4 for the x
    dplyr::group_by(four_week_grp) %>% 
    dplyr::mutate(week_number = 1:n()) %>% 
    dplyr::ungroup() %>% 
    #Remove week 53, because it's not a full week (plus I doubt there are many releases between Xmas and NYE)
    dplyr::filter(week != 53) %>%
    #We want to determine the first date of each week, because this is more readable than week numbers
    #Format it into an concise format
    dplyr::mutate(week_date = format.Date(lubridate::ymd("2019-01-01") + 7*(week - 1), format = "%b %d")) %>% 
    #Group data to create clearer fills
    dplyr::mutate(fill_grp = factor(dplyr::case_when(n < 50 ~ "<50",
                                              n >= 50 & n < 100 ~ "50 - 99",
                                              n >= 100 ~ ">99"), levels = c("<50", "50 - 99", ">99")))
  
  label_tiles <- movies_by_week %>% 
    dplyr::filter(week_date %in% c("Oct-01", "Oct-29")) %>% 
    dplyr::mutate(label = c("There have been 51 more horror movie releases in the first week of October than the week just before",
                            "The week of Halloween has had the most horror movie releases of any week (151)"))
  
  #Use the calendar plot inspired by BBC to show this data
  output <- ggplot(data = movies_by_week) +
    geom_tile(aes(x = week_number, y = four_week_grp, fill = fill_grp), width = 0.87, height = 0.87, size = 1) +
    geom_tile(data = label_tiles, aes(x = week_number, y = four_week_grp), fill = NA, width = 0.95, height = 0.95,
              colour = "black", size = 1) +
    geom_segment(data = label_tiles, aes(x = week_number, y = four_week_grp + 0.5,
                                         xend = week_number, yend = four_week_grp + 2), colour = "black") +
    geom_text(data = label_tiles, aes(x = week_number, y = four_week_grp + 2.5, label = stringr::str_wrap(label, 25)),
              colour = "black", size = 4, family = "Ubuntu") +
    geom_text(aes(x = as.numeric(week_number) - 0.39, y = four_week_grp - 0.3, label = week_date, colour = fill_grp),
              family = "Ubuntu", size = 5, hjust = 0) +
    scale_y_reverse() +
    scale_x_discrete(position = "top") +
    scale_fill_manual(values = c("#E79F9F", "#D56666", "#5B0000")) +
    scale_colour_manual(values = c("black", "black", "white")) +
    coord_equal(clip = "off") +
    labs(title = "October is by far the spookiest month",
         subtitle = "Number of horror movie releases per week (2012 - 2017)") +
    guides(fill = guide_legend(title = "", direction = "horizontal"),
           colour = "none")
  
  ggplot2::ggsave(plot = output, filename = "plots/22_10_19.png", height = 10, width = 10, dpi = 600, type = "cairo")
  
  invisible(NULL)
  
}