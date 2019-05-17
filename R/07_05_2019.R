#' Create plot of student teacher ratios.
#'
#' Create a plot of GDP and student teacher ratios over time.
#'
#' @param animated Logical. Should an animated (gif) or static (png) plot be returned?
#' @param log Should axes be plotted on the log scale? Can be "x", "y", or "xy".
#'
#' @return
#' @export
#' @import gganimate

plot_07_05_19 <- function(animated = TRUE, log = "x"){
  
  #Load data
  student_ratio <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-07/student_teacher_ratio.csv")
  #Load annual GDP data/capita sourced from World Bank (https://data.worldbank.org/indicator/ny.gdp.pcap.cd?end=2017&start=1960)
  Annual_GDP    <- readr::read_delim(system.file("extdata", "GDP_data.csv", package = "TidyTuesday", mustWork = TRUE), delim  = "\t")
  
  #Reshape GDP data so we have a year column
  Reshape_GDP <- Annual_GDP %>% 
    #Remove Indicator Name and Code, these are unimportant
    select(-3:-4) %>% 
    reshape2::melt(id.vars = 1:2, na.rm = TRUE, variable.name = "year", value.name = "GDP") %>% 
    #Rename columns to make them correspond between 2 datasets
    rename(country_GDP = `Country Name`, country_code = `Country Code`) %>% 
    #Mutate year into numeric
    mutate(year = as.integer(as.character(year)))
  
  #Subset to include student ratio data that also has GDP info
  student_ratio_GDP <- student_ratio %>% 
    filter(country_code %in% Reshape_GDP$country_code) %>% 
    left_join(Reshape_GDP, by = c("country_code", "year")) %>% 
    #Exclude those rows with no GDP
    filter(!is.na(GDP)) %>% 
    mutate(year = as.integer(year)) %>% 
    #Just look at primary education
    filter(indicator == "Primary Education")
  
  if(animated){
    
    #Find the best and worst student ratio
    worst <- student_ratio_GDP %>%
      group_by(country_code) %>% 
      summarise(mean_ratio = mean(student_ratio), n = n()) %>%
      filter(n == 6) %>% 
      arrange(mean_ratio) %>% 
      filter(row_number() %in% (n() - 1):n())
    
    best <- student_ratio_GDP %>%
      group_by(country_code) %>% 
      summarise(mean_ratio = mean(student_ratio), n = n()) %>%
      filter(n == 6) %>% 
      arrange(mean_ratio) %>%
      filter(row_number() %in% 1:2)
    
    animated_plot <- ggplot() +
      geom_point(data = student_ratio_GDP, aes(x = GDP, y = student_ratio, colour = country_code, size = GDP), alpha = 0.7)+
      ggrepel::geom_label_repel(data = filter(student_ratio_GDP, country_code %in% best$country_code),
                                aes(x = GDP, y = student_ratio, label = country), nudge_y = 5, segment.size = 0.5, family = "Ubuntu", size = 6)+
      ggrepel::geom_label_repel(data = filter(student_ratio_GDP, country_code %in% worst$country_code),
                                aes(x = GDP, y = student_ratio, label = country), nudge_x = 1, segment.size = 0.5, family = "Ubuntu", size = 6)+
      scale_colour_viridis_d()+
      labs(caption = "\nVisualisation by @ldbailey255 | GDP data: data.worldbank.org | Student ratio data: UNESCO",
           y = "Primary student-teacher ratio", x = "GDP per capita")+
      scale_y_continuous(limits = c(0, NA))+
      scale_size_continuous(range = c(3, 10))+
      theme_classic()+
      theme(title = element_text(family = "Ubuntu", colour = "black", size = 16, margin = margin(t = 10)),
            axis.text = element_text(family = "Ubuntu", size = 15, colour = "black"),
            axis.title.y = element_text(family = "Ubuntu", size = 18, colour = "black", margin = margin(r = 10)),
            axis.title.x = element_text(family = "Ubuntu", size = 18, colour = "black", margin = margin(t = 10)),
            legend.position = "none")+
      #Start gganimate code
      gganimate::transition_time(time = year)+
      labs(title = "Year: {frame_time}")+
      gganimate::shadow_mark(alpha = 0.25, wrap = FALSE, size = 2, exclude_layer = 2:3)+
      gganimate::ease_aes("linear")
    
    if(stringr::str_detect(log, "x")){
      
      animated_plot <- animated_plot +
        scale_x_log10()
      
    }
    
    if(stringr::str_detect(log, "y")){
      
      animated_plot <- animated_plot +
        scale_y_log10()
      
    }
    
    options(gganimate.dev_args = list(width = 600, height = 520))
    
    gganimate::anim_save("./plots/07_05_19.gif", animation = animated_plot)
    
    return(animated_plot)
    
  } else {
   
    non_animated <- ggplot() +
      geom_point(data = student_ratio_GDP, aes(x = GDP, y = student_ratio, colour = country_code, size = GDP), alpha = 0.7)+
      scale_colour_viridis_d()+
      labs(caption = "\nVisualisation by @ldbailey255 \n GDP data: data.worldbank.org | Student ratio data: UNESCO",
           y = "Primary student-teacher ratio", x = "GDP per capita")+
      scale_y_continuous(limits = c(0, NA))+
      scale_size_continuous(range = c(3, 10))+
      theme_classic()+
      theme(title = element_text(family = "Ubuntu", colour = "black", size = 14, margin = margin(t = 10)),
            axis.text = element_text(family = "Ubuntu", size = 14, colour = "black"),
            axis.title.y = element_text(family = "Ubuntu", size = 17, colour = "black", margin = margin(r = 10)),
            axis.title.x = element_text(family = "Ubuntu", size = 17, colour = "black", margin = margin(t = 10)),
            legend.position = "none")
    
    if(stringr::str_detect(log, "x")){
      
      non_animated <- non_animated +
        scale_x_log10()
      
    }
    
    if(stringr::str_detect(log, "y")){
      
      non_animated <- non_animated +
        scale_y_log10()
      
    }
    
    ggsave(plot = non_animated, filename = "./plots/07_05_2019.png", width = 6, height = 5.2, dpi = 300)
    
    return(non_animated)
    
  }
  
}