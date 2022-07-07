plot_15_10_19 <- function(){
  
  options(scipen = 200)
  
  #Set theme
  current_theme <- theme_classic() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "#2E2E2E", colour = NA),
          panel.background = element_rect(fill = "#2E2E2E", colour = NA),
          axis.text = element_text(family = "Ubuntu", colour = "white", size = 14),
          axis.title = element_text(family = "Ubuntu", colour = "white", size = 18),
          axis.title.y = element_text(vjust = 2),
          axis.title.x = element_text(vjust = 0),
          axis.line = element_line(colour = "white", size = 1),
          axis.ticks = element_line(colour = "white"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
          plot.title = element_text(colour = "white", size = 20),
          plot.subtitle = element_text(colour = "white", size = 18))
  
  theme_set(current_theme)
  
  #Load data
  #EPA car emissions data
  big_epa_cars <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

  #We will look at change in energy efficiency and CO2 emissions over time for different car type
  
  #The col 'combo08' is listed as combined MPG in the city with 'fuelType1'.
  #fuelType2 is for duel fuel vehicles that can also run diesel. We will take fuel economy only from
  #regular gasoline as this is what we have prices for.
  
  #For every record, link the contemporary gas_constant price
  class_mpg_data <- big_epa_cars %>% 
    #Group different vehicle types
    dplyr::mutate(VClass2 = dplyr::case_when(grepl(pattern = "Van|van", VClass) ~ "Van",
                                             grepl(pattern = "Pickup", VClass) ~ "Pickup Truck",
                                             grepl(pattern = "Sport Utility", VClass) ~ "SUV",
                                             grepl(pattern = "Compact|compact", VClass) ~ "Hatchbatch",
                                             VClass == "Two Seaters" ~ "Sports car",
                                             grepl(pattern = "Station|Midsize", VClass) ~ "Medium car",
                                             VClass == "Large Cars" ~ "Large car",
                                             grepl(pattern = "Special", VClass) ~ "Other")) %>% 
    #These 'Special purpose vehicles' don't fit into any clear group and so are removed.
    dplyr::filter(VClass2 != "Other") %>% 
    dplyr::select(comb08, co2TailpipeGpm, co2, VClass2, make, model, trany, fuelType1, year) %>% 
    dplyr::mutate(co2 = dplyr::na_if(co2, -1))
  
  ####
  
  #Look at CO2 emissions over time
  
  #Determine the average for each year to draw a geom_path over the top
  co2_data_avg <- class_mpg_data %>%
    dplyr::filter(co2TailpipeGpm != 0) %>% 
    dplyr::group_by(year, VClass2) %>% 
    dplyr::filter(!all(is.na(co2TailpipeGpm))) %>% 
    dplyr::summarise(median = median(co2TailpipeGpm, na.rm = TRUE))
  
  #Label data
  label_data <- co2_data_avg %>% 
    dplyr::group_by(VClass2) %>% 
    slice(n())
  
  label_data[which(label_data$VClass2 == "Sports car"), 3] <- label_data[which(label_data$VClass2 == "Sports car"), 3] + 5
  label_data[which(label_data$VClass2 == "Large car"), 3] <- label_data[which(label_data$VClass2 == "Large car"), 3] + 12
  label_data[which(label_data$VClass2 == "SUV"), 3] <- label_data[which(label_data$VClass2 == "SUV"), 3] - 17
  
  #Create plot
  co2_plot <- ggplot() +
    geom_path(data = co2_data_avg, aes(x = year, y = median, colour = VClass2), size = 2, lineend = "round") +
    geom_point(data = co2_data_avg, aes(x = year, y = median, colour = VClass2), size = 3, stroke = 2, shape = 21, fill = "#2E2E2E") +
    geom_text(data = label_data, aes(x = year + 1, y = median, colour = VClass2, label = VClass2), family = "Ubuntu", size = 5, hjust = 0) +
    scale_colour_manual(values = colorspace::darken(c("#D46C4E", "#48BF84", "#F9E07F", "#43978D", "#386FA4", "#CA3C25", "#8EA4D2", "#03256C"))) +
    coord_fixed(clip = "off", ratio = 0.05) +
    scale_x_continuous(limits = c(1984, 2025), breaks = seq(1985, 2020, 5), name = "Year of production") +
    scale_y_continuous(name = "CO2 emissions (grams/mile)", limits = c(300, 700), breaks = seq(300, 700, 50))
  
  ggplot2::ggsave(co2_plot, filename = "./plots/15_10_19_plots/15_10_19.png", width = 10, height = 10, dpi = 600)
  
  ###
  
  #Next look at how much CO2 would be saved from different choice of car
  current_theme <- theme_classic() +
    theme(legend.position = "none",
          plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          axis.text = element_text(family = "Ubuntu", colour = "#2E2E2E", size = 14),
          axis.title = element_text(family = "Ubuntu", colour = "#2E2E2E", size = 18),
          axis.title.y = element_text(vjust = 2),
          axis.title.x = element_text(vjust = 0),
          axis.line = element_line(colour = "#2E2E2E", size = 1),
          axis.ticks = element_line(colour = "#2E2E2E"),
          plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
          plot.title = element_text(colour = "#2E2E2E", size = 20),
          plot.subtitle = element_text(colour = "#2E2E2E", size = 18))
  
  theme_set(current_theme)
  
  #Pick two cars that are similar but have different fuel efficiency and different co2 emissions
  our_cars <- filter(class_mpg_data, year == 2020 & VClass2 == "Medium car" & make %in% c("Toyota", "Honda", "Kia", "Ford", "Volvo")) %>%
    arrange(co2TailpipeGpm) %>%
    slice(10, n() - 2)
  
  #For each car, determine the amount of emissions over 100,000 miles
  plot_data <- tibble::tibble(x = "X", miles = seq(0, 100000, 500)) %>% 
    dplyr::mutate(co2kg_savings_total = (diff(our_cars$co2TailpipeGpm) * miles)/1000,
                  intercept = NA, text = NA)
  
  #Add lines and text for certain milestones
  #Doing this manually because I couldn't think a neat way to find the closest
  #Co2 emissions for each line
  plot_data[2, 4:5]   <- c(plot_data[2, 3], "500 miles = Producing and using an iPhoneX (~79kg CO2)")
  plot_data[30, 4:5] <- c(plot_data[30, 3], "14,500 miles = Eating 100 hamburgers (~3,000kg CO2)")
  plot_data[65, 4:5] <- c(plot_data[65, 3], "32,000 miles = US household annual electricity consumption (~6,800kg CO2)")
  plot_data[130, 4:5] <- c(plot_data[130, 3], "64,500 miles = Three return flights NYC - Melbourne, Australia (~13,800kg CO2)")
  # plot_data[141, 4:5] <- c(plot_data[141, 3], "70,000 miles = Producing 100 tonnes of cement (~15,000kg CO2)")
  
  emissions_plot <- ggplot(data = plot_data)+
    geom_col(aes(x = x, y = co2kg_savings_total, fill = co2kg_savings_total), colour = "#2E2E2E", size = 2) +
    geom_hline(aes(yintercept = intercept), lty = 2, colour = "#2E2E2E", size = 2) +
    geom_text(aes(y = intercept + 1000, x = 1, label = stringr::str_wrap(text, width = 50)), size = 7, colour = "#2E2E2E", family = "Ubuntu", hjust = 0.5) +
    scale_fill_gradientn(colors = c("#D46C4E", "#F9AD6A", "#5BA8A0", "#CBE54E", "#94B447", "#5D6E1E")) +
    scale_y_continuous(name = "CO2 savings (kg)", breaks = seq(0, 20000, 2500), labels = c("0", "2.5K", "5.0K",
                                                                                           "7.5K", "10.0K", "12.5K",
                                                                                           "15.0K", "17.5K", "20.0K")) +
    theme(axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_text(size = 20)) +
    gganimate::transition_states(miles, transition_length = 2, state_length = 1) +
    gganimate::shadow_mark(alpha = 1, wrap = FALSE, exclude_layer = 1) +
    labs(title = "Miles travelled: {closest_state}")+
    gganimate::ease_aes("linear")

  options(gganimate.dev_args = list(width = 1200, height = 1000))

  gganimate::anim_save("./plots/15_10_19_plots/15_10_19.gif", animation = emissions_plot, fps = 20, end_pause = 50, duration = 25)
  
}