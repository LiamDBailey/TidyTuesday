plot_17_09_19 <- function(){
  
  library(extrafont)
  
  park_visits <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
  state_pop <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/state_pop.csv")
  gas_price <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/gas_price.csv")

  #Determine park densities (visitors/km2 of park)
  
  #Read in shape file (taken from NPS https://public-nps.opendata.arcgis.com/datasets/national-park-service-park-unit-boundaries)
  national_parks <- sf::st_read(dsn = "./inst/extdata/national_park_shp", layer = "National_Park_Service__Park_Unit_Boundaries") %>% 
    #Determine area of each polygon
    dplyr::mutate(Area = sf::st_area(.))
  
  #Use park visits only since WW2
  plot_data <- park_visits %>%
    #Remove total visits from each park
    dplyr::filter(year != "Total") %>%
    #Make year integer and filter from 1950+ where we have population data
    dplyr::mutate(year = as.integer(year)) %>%
    dplyr::filter(year > 1945) %>% 
    #Join in geometry and area
    dplyr::left_join(dplyr::select(national_parks, GNIS_ID, geometry, Area), by = c("gnis_id" = "GNIS_ID")) %>% 
    #Determine visitors/m2 in each year
    #Coerce to numeric to prevent errors in plotting
    dplyr::mutate(Area = as.numeric(Area), visitor_density = visitors/Area) %>% 
    #Remove cases where we can't estimate densities
    dplyr::filter(!is.na(visitor_density))
    
  
  #In plot1 we will look at how densities in parks have increased over time
  plot1_data <- plot_data %>% 
    dplyr::group_by(year) %>% 
    dplyr::summarise(mean_density = mean(visitor_density))
  
  (plot1 <- ggplot(plot1_data, aes(x = year, y = mean_density)) +
    geom_point(aes(fill = mean_density), shape = 21, size = 3) +
    scale_fill_gradient(low = "#7b4397", high = "#dc2430") +
    scale_x_continuous(limits = c(1940, 2016), breaks = seq(1940, 2010, 10),
                       labels = c("1940", paste0("'", seq(50, 90, 10)), "2000", "'10"), name = "Year") +
    scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, 2), name = bquote('Mean annual visitor density (visitors/'*~m^2*')')) +
    labs(title = "So many pick-a-nick baskets", subtitle = "Increasing visitor density in US national parks") +
    geom_smooth(se = FALSE, span = 100) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
          plot.title = element_text(size = 17, family = "Ubuntu", colour = "black"),
          axis.title = element_text(size = 12, family = "Ubuntu", colour = "#404040"),
          axis.text = element_text(size = 10, family = "Ubuntu", colour = "#404040"),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "#F0F0F0"),
          plot.background = element_rect(fill = "#F0F0F0"),
          panel.grid.major = element_line(colour = "#D0D0D0", size = 1),
          panel.grid.minor = element_blank(),
          legend.position = "none"))
  
  #In plot2, we look at how visitor density has changed in the biggest parks
  #We will highlight the top 6 favourite parks in 2016 (taken from the fivethirtyeight plot)
  big_parks <- plot_data %>% 
    dplyr::filter(Area > 600000000)
  
  top_parks <- big_parks %>% 
    dplyr::filter(unit_code %in% c("GRSM", "GRCA", "ROMO", "YOSE", "YELL", "ZION"))
  
  #Create labels for the plot
  park_labels <- top_parks %>% 
    dplyr::filter(year == 2016) %>% 
    #Remove 'National Park' from names
    dplyr::mutate(label = toupper(gsub(x = unit_name, pattern = " National Park", replacement = ""))) %>% 
    #Make Great Smokey and Rocky Mountain on 2 lines
    dplyr::mutate(label = ifelse(label == "GREAT SMOKY MOUNTAINS", "GREAT SMOKEY \n MOUNTAINS",
                                 ifelse(label == "ROCKY MOUNTAIN", "ROCKY\n MOUNTAIN", label)))
  
  (plot2 <- ggplot() +
      coord_cartesian(clip = 'off') +
      geom_path(data = big_parks, aes(x = year, y = visitor_density, group = unit_code), colour = "grey") +
      geom_path(data = top_parks, aes(x = year, y = visitor_density, colour = unit_code), size = 1) +
      geom_text(data = park_labels, aes(x = 2017, y = visitor_density, colour = unit_code, label = label),
                hjust = 0, size = 3) +
      scale_colour_manual(values = c("dark blue", "brown", "#00BFC4", "#B79F00", "#00BA38", "#F8766D")) +
      scale_x_continuous(limits = c(1940, NA), breaks = seq(1940, 2010, 10),
                         labels = c("1940", paste0("'", seq(50, 90, 10)), "2000", "'10"), name = "Year") +
      scale_y_continuous(limits = c(0, NA), breaks = seq(0, 0.007, 0.001), name = bquote('Mean annual visitor density (visitors/'*~m^2*')')) +
      labs(title = "", subtitle = "") +
      theme(plot.margin = margin(t = 5, r = 60, b = 5, l = 5),
            plot.title = element_text(size = 15, family = "Ubuntu", colour = "black"),
            axis.title = element_text(size = 12, family = "Ubuntu", colour = "#404040"),
            axis.text = element_text(size = 10, family = "Ubuntu", colour = "#404040"),
            axis.ticks = element_blank(),
            panel.background = element_rect(fill = "#F0F0F0"),
            plot.background = element_rect(fill = "#F0F0F0"),
            panel.grid.major = element_line(colour = "#D0D0D0", size = 1),
            panel.grid.minor = element_blank(),
            legend.position = "none"))
  
  #Combine plots
  combo_plot <- gridExtra::grid.arrange(plot1, plot2, nrow = 1)
  
  ggsave(plot = combo_plot, "./plots/17_09_19.png", width = 10, height = 5, dpi = 600)
  
  return(combo_plot)
  
}