#' Create plot on night time bird collisions
#' 
#' Task for 30/4/2019. Plot night time bird collisions in the US from
#' Winger et al. (2019) https://doi.org/10.1098/rspb.2019.0364
#' @return Produces a ggplot object, which is also saved as a .png in /plots
#' @export
#' @import dplyr
#' @import ggplot2
#' @import extrafont
plot_30_4_19 <- function(){
  
  #Import data
  bird_collisions <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/bird_collisions.csv")
  mp_light <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-30/mp_light.csv")
  
  #Create grouping for bird collisions in autumn and spring migration period
  #(methods of paper show this is how they approached the work)
  total_collisions <- bird_collisions %>% 
    #Split date information into month and year
    mutate(month = lubridate::month(date),
           year = lubridate::year(date)) %>% 
    #Summarise by each month in each year
    #Only use McCormick Place post 2000 as this is when light and collision data is available.
    #This allows us to look at number of collisions ~ light score
    #Just use the Yes/No flight call dichotomy
    filter(locality == "MP" & year >= 2000 & flight_call != "Rare") %>% 
    group_by(flight_call, year, month) %>% 
    summarise(n_collisions = n()) %>%
    #Join so that we have a value for every month surveying was carried out.
    #Currently, if there were no collisions there will be no corresponding row even though a survey was conducted
    {left_join(tidyr::expand(., flight_call, year = 2000:2016, month = c(3:5, 8:11)), ., by = c("flight_call", "year", "month"))} %>% 
    #Any NAs should really be 0s
    mutate(n_collisions = purrr::map_dbl(.x = n_collisions, .f = ~ifelse(is.na(.x), 0, .x)))
  
  #Determine mean light score 
  avg_light <- mp_light %>%
    mutate(month = lubridate::month(date),
           year = lubridate::year(date)) %>%
    #Subset to remove everything after 2016 because there is no collision data
    filter(year <= 2016) %>% 
    group_by(year, month) %>% 
    summarise(avg_light_score = mean(light_score))
  
  #Now we can create a joined database that includes the total number of collisions each month and the corresponding avg light score
  birds_and_light <- left_join(total_collisions, avg_light, by = c("year", "month")) %>% 
    #Remove the few cases where light score was not recorded in a month
    filter(!is.na(avg_light_score))
  
  #Fit model with interaction
  model <- glm(n_collisions ~ avg_light_score + year + flight_call + flight_call:avg_light_score, data = birds_and_light, family = poisson(link = "log"))
  
  #Extract model predictions
  predictions <- birds_and_light %>% 
    ungroup() %>% 
    #Create a prediction dataset that covers all possible light scores in the average year.
    tidyr::expand(flight_call,
           avg_light_score = seq(min(avg_light$avg_light_score),
                                              max(avg_light$avg_light_score),
                                              length.out = 1000),
                        year = mean(avg_light$year)) %>%
    #Extract model estimates and standard errors
    mutate(pred = predict(model, newdata = ., type = "response"),
           se = predict(model, newdata = ., type = "response", se.fit = T)$se.fit)
  
  #Load png images to use in plots
  bird_img <- grid::rasterGrob(png::readPNG(source = system.file("extdata", "bird.png", package = "TidyTuesday", mustWork = TRUE)))
  music    <- grid::rasterGrob(png::readPNG(source = system.file("extdata", "music_note.png", package = "TidyTuesday", mustWork = TRUE)),
                                            width = unit(4, "cm"), height = unit(3, "cm"))
  
  #Because we only want to include music grob in one facet, we create a data set so that it has corresponding aesthetic data
  music_data <- tibble(avg_light_score = 5,
                       n_collisions = 350,
                       flight_call = "Yes") %>% 
    mutate(grob = list(music))
  
  #Create our plot
  output_plot <- ggplot() +
    #Create points for every month
    geom_point(data = birds_and_light, aes(x = avg_light_score, y = n_collisions, fill = avg_light_score), shape = 21, size = 3, stroke = 1) +
    #Create a colour scale that ranges from black - tungsten light yellow - beige to mimic light pollution
    scale_fill_gradient2(low = "black",
                         mid = "#FFD6AA",
                         high = "#f5f5dc", midpoint = mean(birds_and_light$avg_light_score)) +
    #Set axis names and create facet wrap for the two call types
    scale_x_continuous(name = "Average monthly light score") +
    scale_y_continuous(name = "Monthly bird collisions") +
    facet_wrap(facets = ~flight_call) +
    #Add prediction lines from our model
    geom_line(data = predictions, aes(x = avg_light_score, y = pred)) +
    #Add standard error around model predictions
    geom_ribbon(data = predictions, aes(x = avg_light_score, ymin = pred - se, ymax = pred + se), fill = NA, colour = "black") +
    #Add bird png on both facets
    annotation_custom(grob = bird_img, xmin = 3, xmax = 7, ymin = 300, ymax = 400) +
    #Add music note png on just one
    egg::geom_custom(data = music_data, aes(x = avg_light_score, y = n_collisions, data = grob), grob_fun = "identity") +
    #Add labels and adjust theme
    labs(caption = "\nVisualisation by @ldbailey255 | Picture credit: pixabay.com") +
    theme_classic() +
    theme(legend.position = "none",
          axis.title = element_text(size = 14, family = "Ubuntu"),
          axis.text = element_text(size = 12, colour = "black"),
          axis.line = element_line(size = 1),
          axis.ticks = element_line(size = 1, colour = "black"),
          plot.background = element_rect(fill = "white"),
          strip.background = element_blank(),
          strip.text = element_blank())
  
  #Save in plot folder
  ggsave(plot = output_plot, filename = "./plots/30_04_2019.png", width = 14, height = 5.2, dpi = 300)
  
  #Output to user
  return(output_plot)
  
}
