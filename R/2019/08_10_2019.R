plot_08_10_19 <- function(){
  
  ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")
  
  #We're going to look at the the top weightlifters, using a few different metrics
  lifts_cleaned <- ipf_lifts %>% 
    #Create a unique name for each competition so we can work out the standardised placing in every comp
    dplyr::mutate(comp = paste(meet_name, lubridate::year(date), sep = "_")) %>% 
    #Remove SB events as they are essentially non-existent (2 lifts?)
    #Remove guest lifters as these individuals weren't competing
    dplyr::filter(event != "SB" & place != "G") %>% 
    #Make disqualifications and guest lifters into a large number (999) so they are given last place in any event
    #Determine the max lift in all three categories from an event
    dplyr::mutate(place_numeric = purrr::map_int(place, ~{
      
      if(..1 %in% c("DQ", "DD")){
        
        return(999L)
        
      } else {
        
        return(as.integer(..1))
        
      }
      
    }),
    max_lift = pmax(best3squat_kg, best3bench_kg, best3deadlift_kg, na.rm = TRUE))
  
  #Go through every unique event at every competition (comp, event, division, weight class) and standardise the placing
  #between -1 and 1. Gold medal will always be 1, last place will always be -1.
  lifts_cleaned_std <- lifts_cleaned %>% 
    dplyr::group_by(comp, event, sex, division, weight_class_kg) %>% 
    dplyr::arrange(desc(place_numeric), .by_group = TRUE) %>% 
    dplyr::mutate(place_std = seq(-1, 1, length.out = n()), sum_place = sum(place_numeric, na.rm = TRUE), sum_place_expected = sum(1:n()),
                  DQs = any(place_numeric == 999L)) %>% 
    #Remove any cases where the number of places is not the same as expected (and can't be explained by DQs)
    #Having a quick look, these appear to be cases due to typos, missclassification etc.
    #We would need to fix these manually, for now just remove them
    #We lose a little under 1000 records from this
    dplyr::filter(sum_place == sum_place_expected | DQs == TRUE)
  
  #For all individuals in males and females find best individuals in 4 categories:
  #1. Most gold medals
  #3. Max weight lifted over all competition types
  #4. Longest career
  GOAT <- lifts_cleaned_std %>% 
    dplyr::filter(!is.na(name)) %>% 
    dplyr::group_by(name) %>% 
    #N.B. We define career length as number of years in which they competed.
    #There are some individuals who had long careers but very few events
    dplyr::summarise(sex = first(sex), career_length = length(unique(lubridate::year(date))),
                     last_yr = lubridate::year(max(date, na.rm = TRUE)),
                     mean_place = mean(place_std, na.rm = TRUE), total_events = sum(!is.na(place_std)),
                     total_podiums = sum(place <= 3, na.rm = TRUE), perc_podiums = total_podiums/total_events,
                     total_golds = sum(place == 1, na.rm = TRUE), perc_golds = total_golds/total_events,
                     max_lift = max(max_lift, na.rm = TRUE))
  
  #We only consider individuals who have participated in at least 10 events over their career
  longest_career <- GOAT %>% 
    dplyr::filter(total_events > 10) %>% 
    dplyr::group_by(sex) %>% 
    dplyr::arrange(-career_length, .by_group = TRUE) %>% 
    slice(1)
  most_medals    <- GOAT %>% 
    dplyr::filter(total_events > 10) %>% 
    dplyr::group_by(sex) %>% 
    dplyr::arrange(-perc_golds, -total_events, .by_group = TRUE) %>%
    slice(1)
  highest_weight <- GOAT %>% 
    dplyr::filter(total_events > 10) %>% 
    dplyr::group_by(sex) %>% 
    dplyr::arrange(-max_lift, .by_group = TRUE) %>% 
    slice(1)
  
  plot_data <- lifts_cleaned_std %>% 
    dplyr::filter(name %in% c(longest_career$name, most_medals$name, highest_weight$name)) %>% 
    #Create a event number column to compare individuals with different career time periods
    dplyr::arrange(name, date) %>% 
    dplyr::group_by(name) %>% 
    dplyr::mutate(event_nr = seq(1:n()),
                  cumsum_gold = cumsum(place == 1),
                  cumsum_podium = cumsum(place <= 3),
                  cumsum_place_std = cumsum(place_std)) %>% 
    dplyr::ungroup()
  
  #Colour data
  colour_palette <- tibble::tibble(colour = c("#8EA4D2", "#35678C", "#4C9F70", "#CA3C25", "#ED7D3A", "#E59F71")) %>% 
    #Join in related data for geom_segment
    dplyr::mutate(segment_data = list(filter(plot_data, name == highest_weight$name[1] & max_lift == highest_weight$max_lift[1]),
                                      filter(plot_data, name == highest_weight$name[2] & max_lift == highest_weight$max_lift[2]),
                                      filter(plot_data, name == most_medals$name[1]) %>% slice(n()),
                                      filter(plot_data, name == most_medals$name[2]) %>% slice(n()),
                                      filter(plot_data, name == longest_career$name[1]) %>% slice(n()),
                                      filter(plot_data, name == longest_career$name[2]) %>% slice(n() - 4)),
                  label_text = c(paste0("**<span style = 'color:", colour[1], "'>Bonica Brown</span> has the heaviest <br> lift of any women <br> (318kg)**"),
                                 paste0("**In 2014 <span style = 'color:", colour[2], "'>David Lup", "\U00E1", "\U010D", "</span> <br> lifted 450kg. <br> He was only 22!**"),
                                 paste0("**<span style = 'color:", colour[3], "'>Natalia Salnikova</span> <br> has won every event <br> she has entered but one <br> (she came second) <br>**"),
                                 paste0("**In mens, <span style = 'color:", colour[4], "'>Sergey Fedosienko</span> <br> has never lost an event...**"),
                                 paste0("**<span style = 'color:", colour[5], "'>Vuokko Viitasaari</span> has completed <br> 34 events, <br> more than any other woman**"),
                                 paste0("**<span style = 'color:", colour[6], "'>Hiroyuki Isagawa</span> participated in 52 events. <br> He was 62 for his last event!**"))) %>% 
    tidyr::unnest(segment_data) %>% 
    dplyr::arrange(name)
  
  title <- "Do you even lift?!"
  subtitle1 <- "How do you pick the best powerlifter? \n"
  subtitle2 <- paste0("The heaviest lift? ", emo::ji("weight_lifting_man"), " The longest career?", emo::ji("spiral_calendar"),
  " The most gold medals?", emo::ji("1st_place_medal"))
  subtitle3 <- "\n We look at the top male and female powerlifters using different metrics."
  
  #For these top lifters, plot their weight lifts and standardised place over their career
  weight_lifted <- ggplot() +
    geom_path(data = plot_data, aes(x = event_nr, y = max_lift, group = name, colour = name), size = 1, lineend = "round") +
    scale_colour_manual(values = colour_palette$colour) +
    scale_y_continuous(limits = c(0, 600), name = "Maximum weight lifted (kg)") +
    scale_x_continuous(limits = c(1, 60), breaks = seq(0, 60, 10), name = "Career event") +
    coord_equal(ratio = 0.8 * (25/600), clip = "off") +
    ggtext::geom_richtext(data = colour_palette %>% slice(1),
                  aes(x = event_nr + 10,
                      y = max_lift + 50, label = colour_palette$label_text[1]),
                  fill = NA, label.color = NA, family = "Ubuntu", colour = "white") +
    ggtext::geom_richtext(data = colour_palette %>% slice(2),
                  aes(x = event_nr,
                      y = max_lift + 80, label = colour_palette$label_text[2]),
                  fill = NA, label.color = NA, family = "Ubuntu", colour = "white") +
    ggtext::geom_richtext(data = colour_palette %>% slice(3),
                  aes(x = event_nr + 1,
                      y = max_lift + 90, label = colour_palette$label_text[3]),
                  fill = NA, label.color = NA, family = "Ubuntu", colour = "white") +
    ggtext::geom_richtext(data = colour_palette %>% slice(6),
                  aes(x = event_nr + 12,
                      y = max_lift - 60, label = colour_palette$label_text[6]),
                  fill = NA, label.color = NA, family = "Ubuntu", colour = "white") +
    theme_classic() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#2E2E2E", colour = NA),
          panel.background = element_rect(fill = "#2E2E2E", colour = NA),
          axis.text = element_text(family = "Ubuntu", colour = "white"),
          axis.title = element_text(family = "Ubuntu", colour = "white"),
          axis.line = element_line(colour = "white", size = 1),
          axis.ticks = element_line(colour = "white"))
  
  weight_lifted_gg <- ggplotGrob(weight_lifted)
  
  placed <- ggplot() +
    geom_path(data = plot_data, aes(x = event_nr, y = cumsum_gold, group = name, colour = name), size = 1, lineend = "round") +
    scale_colour_manual(values = colour_palette$colour) +
    scale_x_continuous(limits = c(1, 60), breaks = seq(0, 60, 10), name = "Career event") +
    scale_y_continuous(limits = c(0, 25), name = "Total gold medals") +
    ggtext::geom_richtext(data = colour_palette %>% slice(4),
                  aes(x = event_nr - 6,
                      y = cumsum_gold + 5, label = colour_palette$label_text[4]),
                  fill = NA, label.color = NA, family = "Ubuntu", colour = "white") +
    ggtext::geom_richtext(data = colour_palette %>% slice(5),
                  aes(x = event_nr + 11,
                      y = cumsum_gold - 2, label = colour_palette$label_text[5]),
                  fill = NA, label.color = NA, family = "Ubuntu", colour = "white") +
    coord_equal(ratio = 0.8, clip = "off") +
    theme_classic() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#2E2E2E", colour = NA),
          panel.background = element_rect(fill = "#2E2E2E", colour = NA),
          axis.text = element_text(family = "Ubuntu", colour = "white"),
          axis.title = element_text(family = "Ubuntu", colour = "white"),
          axis.line = element_line(colour = "white", size = 1),
          axis.ticks = element_line(colour = "white"))
  
  placed_gg <- ggplotGrob(placed)
  
  #Create plot with other plots placed around
  final_plot <- ggplot() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    annotate("text", label = title, family = "Alfa Slab One", x = 0.5, y = 0.95, size = 10, colour = "white") +
    annotate("text", label = paste(subtitle1, subtitle2, subtitle3),
             family = "Open Sans Semibold", x = 0.5, y = 0.85, size = 6, colour = "white") +
    annotation_custom(weight_lifted_gg, xmin = 0.05, xmax = 0.95, ymin = 0.4, ymax = 0.75) +
    annotation_custom(placed_gg, xmin = 0.05, xmax = 0.95, ymin = 0, ymax = 0.35) +
    labs(caption = "\nVisualisation by @ldbailey255 | Data: openpowerlifting.org") +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#2E2E2E", colour = NA),
          panel.background = element_rect(fill = "#2E2E2E", colour = NA),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.caption = element_text(family = "Ubuntu", colour = "white"))
  
  ggsave(final_plot, filename = "./plots/08_10_19.png", height = 10, width = 10, dpi = 600)
  
  
}