#' Title
#'
#' @return
#' @export
#'
#' @examples
plot_28_05_19 <- function(){
  
  #Read in data
  wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")
  
  #Load my ggplot themes
  if(!"MyFuncs" %in% names(installed.packages()[, 1])){
    
    devtools::install_github("LiamDBailey/MyFuncs", upgrade = "never")
    
  }
  
  wine_ratings <- wine_ratings %>% 
    filter(!is.na(country))
  
  #Common words
  common_words <- glue::glue_collapse(c("the ", "and ", "a ", "of ", "with ", "is ", "with ", "wine ", "in ", "it ", "to ", "its ", "on ",
                                        "that ", "from ", "but ", "are ", "has ", "for ", "by ", "as ", "an ", "at ",
                                        "flavors ", "palate ", "aromas ", "notes "), sep = "|")
  
  #Split up words in wine ratings
  wine_words <- unlist(purrr::pmap(.l = list(words = wine_ratings$description),
                              .f = function(words){
                                
                                #Make all lower case
                                words <- tolower(words)
                                
                                #First, remove all punctuation and common words
                                no_punc <- stringr::str_remove_all(string = words, pattern = glue::glue("[.,'()?!\"%]|{common_words}"))
                                #Then break up all words
                                all_words <- stringi::stri_split_fixed(no_punc, pattern = " ")
                                
                                }))
    
  word_freq <- table(wine_words)
  word_freq <- word_freq[order(-word_freq)]
  
  plot_data <- wine_ratings %>% 
    mutate(good_words = grepl(pattern = "cherry|berry|plum|blackberry|apple",
                              x = description))
  
  plot_data_means <- plot_data %>% 
    group_by(good_words) %>% 
    summarise(avg_pts = mean(points),
              N = n())
  
  overall_avg <- mean(wine_ratings$points)
  
  #Read in glass image
  glass_img <- grid::rasterGrob(png::readPNG(source = system.file("extdata", "glass_w_wine.png", package = "TidyTuesday", mustWork = TRUE)))
  
  ggplot(plot_data)+
    geom_violin(aes(x = good_words, y = points)) +
    geom_point(data = plot_data_means, aes(x = good_words, y = avg_pts), shape = 21, size = 3, fill = "dark grey", alpha = 0.75)+
    geom_hline(yintercept = overall_avg, lty = 2)
  
  #Add bird png on both facets
  # ggplot(avg_ratings) +
  #   geom_point(aes(x = avg_rating, y = avg_price, fill = scale_price), shape = 21, size = 3) +
  #   scale_fill_gradient(low = "#722f37", high = "#722F58") +
  #   annotation_custom(grob = glass_img, xmin = 85, xmax = 87, ymin = 30, ymax = 35) +
    
  
}

########################################################
  
plot_28_05_19_oldmap <- function(){
  
  #Read in data
  wine_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-28/winemag-data-130k-v2.csv")
  
  #Load my ggplot themes
  if(!"MyFuncs" %in% names(installed.packages()[, 1])){
    
    devtools::install_github("LiamDBailey/MyFuncs", upgrade = "never")
    
  }
  
  wine_ratings <- wine_ratings %>% 
    filter(!is.na(country))
  
  #Determine average rating and average price
  avg_ratings <- wine_ratings %>% 
    group_by(country) %>% 
    summarise(avg_rating = mean(points, na.rm = T),
              avg_price = mean(price, na.rm = T),
              point_per_dollar = avg_rating/avg_price) %>%
    #Scale country ratings to be in SD
    mutate(scale_price = as.numeric(scale(point_per_dollar, center = FALSE))) 
  
  #Load map data
  data("wrld_simpl")
  
  world_map <- wrld_simpl %>%
    st_as_sf() %>%
    st_transform(crs = "+proj=robin") %>% 
    #Join in wine rating info
    left_join(avg_ratings %>% rename(NAME = country), by = "NAME") %>% 
    #Add centroids
    mutate(centre = st_centroid(geometry))
  
  #Separate out countries with ratings
  rate_country  <- filter(world_map, !is.na(scale_price)) %>% 
    mutate(geom2 = (geometry - centre) * scale_price + centre) %>% 
    #Extract exact XY coordinates for geom_repel
    left_join(purrr::map2_df(.x = .$centre,
                             .y = .$NAME,
                            .f = function(.x, .y){
                              
                              return(tibble(NAME = .y,
                                            X = .x[1],
                                            Y = .x[2]))
                              
                            }), by = "NAME")
  
  ggplot()+
    #First create an empty polygon for all countries
    geom_sf(data = world_map,
            aes(geometry = geometry), fill = "white", colour = "light grey") +
    geom_sf(data = rate_country,
            aes(geometry = geom2, fill = point_per_dollar), colour = "black") +
    geom_text_repel(data = rate_country,
                    aes(x = X, y = Y, label = NAME))
                    
    # geom_sf_text(data = rate_country,
    #              aes(geometry = centre, label = NAME), colour = "white")
  
  purrr::pwalk(.l = list(rate_country$geom2),
               .f = function(.x){
                 
                 x <- x +
                   geom_sf(aes(geometry = .x))
                 
               })
  
  
  
}