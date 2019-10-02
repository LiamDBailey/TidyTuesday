plot_01_10_19 <- function(wrap_size = 15, hex_size = 1000){
  
  #Load barstool pizza data with ratings and geospatial info
  pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")
  
  #read in NYC borough limits
  #Isolate Manhattan (where the majority of reviewed places are)
  NYC_sp <- rgdal::readOGR(dsn = "./inst/extdata/nyc_boroughs", layer = "nybb") %>% 
    subset(., BoroName == "Manhattan") %>% 
    #Transform to use NAD83 CRS (best coordinate system for the US)
    sp::spTransform(CRS("+proj=utm +zone=11 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
  
  #Wrangle review data
  pizza_data <- pizza_barstool %>% 
    #Exclude any without geospatial info
    dplyr::filter(!is.na(longitude)) %>% 
    #Make into a spatial object (CRS WGS84)
    sp::SpatialPointsDataFrame(data = ., coords = .[c("longitude", "latitude")],
                               proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) %>% 
    #Transform to have the same CRS as the Manhattan polygon
    sp::spTransform(., CRS(sp::proj4string(NYC_sp))) %>% 
    #Filter only those records in Manhattan
    #Note, have to use subset (rather than dplyr::filter) for SpatialPointsDataFrame 
    subset(., complete.cases(sp::over(., NYC_sp)))
  
  #Create a hexagonal grid over Manhattan
  #Have a default size of 1000m
  #Set seed because this process involves some sampling so necessary to keep reproducible
  set.seed(666)
  
  #Adjust the Manhattan polygon to have a slight buffer
  buffer <- rgeos::gBuffer(NYC_sp, width = 100)
  #Draw a hex grid over this buffered polygon
  HexPts_large <- sp::spsample(buffer, type = "hexagonal", cellsize = hex_size)
  #Make this into a spatial polygon layer
  NYC_hex_large <- HexPoints2SpatialPolygons(HexPts_large)
  
  #For each hexagon, find the average of the 'all_average_score' column for all pizza places in the area
  NYC_hex_large$avg_score <- purrr::map_dbl(.x = 1:length(NYC_hex_large),
              .f = function(i, pizza_data, hex_data){
                
                local_pizza <- subset(pizza_data, complete.cases(sp::over(pizza_data, hex_data[i, ])))
                
                if(nrow(local_pizza) == 0){
                  
                  return(NA_real_)
                  
                } else {
                  
                  return(mean(local_pizza$review_stats_all_average_score))
                  
                }
                
              }, pizza_data = pizza_data, hex_data = NYC_hex_large)
  
  #Also record the number of pizza places in each hex
  NYC_hex_large@data$number_stores <- purrr::map_dbl(.x = 1:length(NYC_hex_large),
                                            .f = function(i, pizza_data, hex_data){
                                              
                                              return(nrow(subset(pizza_data, complete.cases(sp::over(pizza_data, hex_data[i, ])))))
                                              
                                            }, pizza_data = pizza_data, hex_data = NYC_hex_large)
  
  #Make data into sf objects which is easier to plot
  NYC_sf <- sf::st_as_sf(NYC_sp)
  NYC_hex_large_sf <- sf::st_as_sf(NYC_hex_large)
  
  #Determine the centre point of all hexes
  coord_centroid <- as_tibble(rgeos::gCentroid(NYC_hex_large, byid = TRUE)@coords) %>% 
    #Join in hex info
    dplyr::bind_cols(NYC_hex_large_sf)
  
  #Find centroid coordinates of some hexes that will be used for labels
  best_spot <- coord_centroid[which(NYC_hex_large$avg_score == max(NYC_hex_large$avg_score, na.rm = TRUE)), ]
  best_spot_multipizza <- coord_centroid[which(NYC_hex_large$avg_score == max(subset(NYC_hex_large, number_stores > 1)$avg_score, na.rm = TRUE)), ]
  empty_spot <- coord_centroid[103, ]
  
  base_plot_coarse <- ggplot() +
    #Add hexes and the outline of Manhattan
    geom_sf(data = NYC_hex_large_sf, aes(fill = avg_score)) +
    geom_sf(data = NYC_sf, colour = "black", fill = NA, alpha = 0.6) +
    #Add text with the number of pizza places in each hex
    geom_text(data = coord_centroid, aes(x = x, y = y, label = number_stores, colour = (number_stores > 0)),
              size = 3, family = "Courier New") + 
    #Adjust gradients
    scale_fill_gradientn(colours = c("#264D59", "#43978D", "#F9E07F", "#F9AD6A", "#D46C4E"),
                         name = "Average pizza score", na.value = "#09203f", breaks = seq(4, 8.5, 0.5)) +
    scale_colour_manual(values = c("white", "black")) +
    #Add arrow and text to show the best pizza hex (regardless of number of stores)
    geom_curve(aes(x = best_spot$x + 7100, xend = best_spot$x + 500, y = best_spot$y + 1000, yend = best_spot$y + 200),
               curvature = 0.10, size = 3, arrow = arrow(length = unit(3, "mm")), colour = "dark grey", lineend = "round") +
    geom_curve(aes(x = best_spot$x + 7000, xend = best_spot$x + 500, y = best_spot$y + 1000, yend = best_spot$y + 200),
              curvature = 0.10, size = 1, arrow = arrow(length = unit(3, "mm")), colour = "black", lineend = "round") +
    geom_text(aes(x = best_spot$x + 10000, y = best_spot$y + 1000,
                  label = paste0(stringr::str_wrap("The top rated area is in East Harlem", wrap_size), "\n", stringr::str_wrap("(but there's only one pizza place)", wrap_size + 5))),
              colour = "black", family = "Alfa Slab One") +
    #Add arrow and text to show the best pizza hex with >1 store
    geom_curve(aes(x = best_spot_multipizza$x - 6100, xend = best_spot_multipizza$x - 500, y = best_spot_multipizza$y - 1000, yend = best_spot_multipizza$y - 200),
               curvature = 0.10, size = 3, arrow = arrow(length = unit(3, "mm")), colour = "dark grey", lineend = "round") +
    geom_curve(aes(x = best_spot_multipizza$x - 6000, xend = best_spot_multipizza$x - 500, y = best_spot_multipizza$y - 1000, yend = best_spot_multipizza$y - 200),
               curvature = 0.10, size = 1, arrow = arrow(length = unit(3, "mm")), colour = "black", lineend = "round") +
    geom_text(aes(x = best_spot_multipizza$x - 9500, y = best_spot_multipizza$y - 1500,
                  label = paste(stringr::str_wrap("Want more than one option? Hudson Square is the best", wrap_size), emo::ji("thumbsup"))),
              colour = "black", family = "Alfa Slab One") +
    #Add arrow and text to show an empty hex and explain the scale
    geom_curve(aes(x = empty_spot$x - 6100, xend = empty_spot$x - 500, y = empty_spot$y + 1000, yend = empty_spot$y + 200),
               curvature = -0.10, size = 3, arrow = arrow(length = unit(3, "mm")), colour = "dark grey", lineend = "round") +
    geom_curve(aes(x = empty_spot$x - 6000, xend = empty_spot$x - 500, y = empty_spot$y + 1000, yend = empty_spot$y + 200),
               curvature = -0.10, size = 1, arrow = arrow(length = unit(3, "mm")), colour = "black", lineend = "round") +
    geom_text(aes(x = empty_spot$x - 9000, y = empty_spot$y + 1000,
                  label = paste0(stringr::str_wrap("Each hexagon is 1km across", wrap_size), "\n (about 15 minutes walk)")),
              colour = "black", family = "Alfa Slab One") +
    #Remove clipping so text is all visible
    coord_sf(clip = "off") +
    labs(title = "Where should you go in NYC?",
         subtitle = paste(emo::ji("pizza"), emo::ji("pizza"), "(if you want the best pizza)", emo::ji("pizza"), emo::ji("pizza")),
         caption = "\nVisualisation by @ldbailey255 | Pizza reviews: barstoolsports.com") +
    theme_classic()+
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(family = "Alfa Slab One", face = "bold", size = 16, hjust = 0.5),
          plot.subtitle = element_text(family = "Alfa Slab One", face = "plain", size = 14, hjust = 0.5),
          plot.caption = element_text(family = "Courier New", size = 12, hjust = 0.5, colour = "black", face = "bold"),
          legend.text = element_text(family = "Alfa Slab One", face = "plain", size = 10),
          legend.title = element_text(family = "Alfa Slab One", face = "plain", size = 12, vjust = 0.85),
          legend.position = "bottom",
          plot.margin = margin(r = -10, l = -10, unit = "mm")) +
    guides(fill = guide_colourbar(barwidth = unit(85, "mm"), barheight = unit(8, "mm")),
           colour = "none")
  
  ggsave(base_plot_coarse, filename = "./plots/01_10_19.jpeg", width = 8, height = 8, dpi = 600)
  
  return(base_plot_coarse)
  
}