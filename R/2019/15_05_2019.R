
#' Determine geographic variation in Nobel Prize winners
#'
#' @param start_year First year from which to start collating noble prize winners
#'
#' @return A ggplot plot grid
#' @export
#' @import sp
#' @import rgeos
#' @import rgdal

plot_15_05_19 <- function(start_year = 1969){
  
  library(patchwork)
  
  #Load my ggplot themes
  if(!"MyFuncs" %in% names(installed.packages()[, 1])){
    
    devtools::install_github("LiamDBailey/MyFuncs", upgrade = "never")
    
  }
  
  nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
  centre_points <- readr::read_csv(system.file("extdata", "country-capitals.csv", package = "TidyTuesday"))
  
  #Plot 1: Geographic distribution of nobel laureates
  #Load world map data
  world_map <- map_data("world")
  
  #Create a database where the country names differ between the capital locations and
  #the namein the nobel prize data
  #N.B. For now I just use Antigua and Barbuda instead of Guadeloupe
  #I guess Guadeloupe doesn't come up as having a capital because it is still French.
  #Doing individual cities would be much more effective!
  name_mismatch <- dplyr::tibble(nobel_name = c("East Timor", "Guadeloupe Island", "Northern Ireland",
                                                "People's Republic of China", "Republic of Macedonia",
                                                "Scotland", "Trinidad", "United States of America"),
                                 centre_name = c("Timor-Leste", "Antigua and Barbuda", "United Kingdom",
                                                 "China", "Macedonia", "United Kingdom", "Trinidad and Tobago",
                                                 "United States"))
  
  #Reformat nobel prize winner data to be cumulative.
  location_data <- nobel_winners %>% 
    #Create a column that combines birth and organisation country
    mutate(single_location = purrr::map2_chr(.x = .$birth_country,
                                             .y = .$organization_country,
                                             .f = function(.x, .y){
                                               
                                               country <- ifelse(is.na(.x), .y, .x)
                                               
                                               #Fix some of the country names so they match our capital locations
                                               country <- ifelse(grepl(pattern = "\\(", country),
                                                             substr(x = country,
                                                                    start = gregexpr(pattern = "\\(", text = country)[[1]][1] + 1,
                                                                    stop  = gregexpr(pattern = "\\)", text = country)[[1]][1] - 1
                                                             ),
                                                             country)
                                               
                                               #Check if any of the countries are in the mixmatch list
                                               country <- ifelse(country %in% name_mismatch$nobel_name,
                                                                 name_mismatch$centre_name[which(name_mismatch$nobel_name == country)],
                                                                 country)
                                                 
                                               return(country)
                                               
                                             })) %>% 
    #For now, just remove the NAs (it's for organisations with no country listed)
    filter(!is.na(single_location) & prize_year >= start_year) %>%
    #Link X and Y coordinates to each country
    left_join(centre_points %>% select(single_location = CountryName,
                                       Y = CapitalLatitude,
                                       X = CapitalLongitude), by = "single_location")
  
  #For every country in every year, determine number of prizes up until and including this year
  cumulative_prizes <- location_data %>%
    group_by(single_location, prize_year) %>% 
    #First, determine how many prizes each country got each year
    summarise(total_yr = n(),
              X = X[1],
              Y = Y[1]) %>%
    #Then (within country) determine the cumulative sum
    mutate(cum_total = cumsum(total_yr)) %>%
    #Arrange from newest to youngest so that we can see the stack
    arrange(single_location, desc(prize_year)) %>%
    ungroup()
  
  #Determine distance between each point as Oslo
  #Use the standard Google Maps projection of lat/long (EPSG:900913)
  spatial_pts <- sp::SpatialPointsDataFrame(coords = location_data[, c("X", "Y")],
                                            data = location_data,
                                            proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
  
  oslo <- sp::SpatialPoints(coords = centre_points[which(centre_points$CountryName == "Norway"), c("CapitalLongitude", "CapitalLatitude")],
                            proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"))
  
  #For every award, determine the distance to Oslo.
  location_data <- location_data %>% 
    mutate(dist = as.numeric(rgeos::gDistance(spatial_pts, oslo, byid = T)))
  
  #Determine the median distance for each category
  distance_data <- location_data %>% 
    group_by(category) %>% 
    summarise(median_dist = median(dist)) %>% 
    arrange(desc(median_dist)) %>% 
    mutate(category = forcats::fct_reorder(category, median_dist))
  
  #Rearrange the factor levels in the non-grouped data to match order of median distance
  location_data <- location_data %>% 
    mutate(category = forcats::fct_relevel(category, levels(distance_data$category)))
  
  #Create paths showing distance from Oslo
  map_plot <- ggplot() +
    geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
                 fill = "light grey", colour = "black") +
    geom_segment(data = location_data, aes(x = X, y = Y,
                                           xend = filter(centre_points, CountryName == "Norway") %>%
                                                    pull(CapitalLongitude),
                                           yend = filter(centre_points, CountryName == "Norway") %>%
                                                    pull(CapitalLatitude),
                                           colour = category), size = 1) +
    geom_point(data = location_data, aes(x = X, y = Y, fill = category),
               shape = 21, size = 2, colour = "black") +
    scale_y_continuous(limits = c(-60, NA)) +
    scale_colour_brewer(palette = "Spectral", type = "div") +
    scale_fill_brewer(palette = "Spectral", type = "div") +
    facet_wrap(facets = ~category, nrow = 1) +
    theme_tidydark() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          strip.background = element_rect(fill = "#444444"),
          plot.margin = margin(r = 10, l = -10, b = 20)) +
    labs(title = "Geographic spread of Nobel Prize winners (since 1969)",
         subtitle = "Distance between a Nobel Prize winners place of birth and Oslo")
  
  lollipop_plot <- ggplot()+
    geom_violin(data = location_data, aes(x = category, y = dist, fill = category), colour = "white", alpha = 0.25) +
    geom_text(aes(x = seq(0.85, 5.85, 1), y = 125, label = levels(location_data$category)), colour = "white",
             size = 8, vjust = 0, hjust = 0.75, family = "Arial Rounded MT Bold", angle = 90, alpha = 0.5) +
    geom_segment(data = distance_data,
                 aes(x = category, y = median_dist, xend = category, yend = 0), size = 1, colour = "white") +
    geom_point(data = distance_data,
               aes(x = category, y = median_dist, fill = category), shape = 21, size = 5, stroke = 1.5, colour = "white") +
    theme_tidydark() +
    scale_fill_brewer(palette = "Spectral", type = "div") +
    scale_x_discrete(position = "top") +
    scale_y_continuous(position = "right") +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.line.x = element_blank(),
          axis.line.y = element_line(colour = "white"),
          plot.margin = margin(t = -15))+
    labs(y = "Distance (km)",
         caption = "Visualisation by @ldbailey255 | Data: kaggle.com")
  
  #Set tidydark as the standard ggplot theme so it is applied in patchwork.
  theme_set(theme_tidydark())
  
  map_plot + lollipop_plot + plot_layout(ncol = 1, height = c(0.75, 1))
  
  ggsave("./plots/15_05_19.png", width = 17, height = 5.2, dpi = 300)

}
