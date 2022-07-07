plot_10_09_19 <- function(detailed = TRUE){
  
  library(extrafont)
  
  # tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")
  data("tx_injuries")

  if(detailed){
    
    #Classify type of body part injured into head, arm, leg, torso
    injury_categories <- tx_injuries %>% 
      dplyr::mutate(head = grepl(x = tolower(tx_injuries$body_part), pattern = "mouth|head|eye|teeth|neck|chin|face|nose|ear|lip|cheek|tooth"),
                    legs = grepl(x = tolower(tx_injuries$body_part), pattern = "knee|leg|foot|ankle|toe|thigh"),
                    foot = grepl(x = tolower(tx_injuries$body_part), pattern = "foot|ankle|toe"),
                    torso = grepl(x = tolower(tx_injuries$body_part), pattern = "chest|rib|back|abdomen|back|torso|midsection|hip|collar|stomach|clavical"),
                    groin = grepl(x = tolower(tx_injuries$body_part), pattern = "groin|butt|vagina|tail|glute|genital"),
                    hand = grepl(x = tolower(tx_injuries$body_part), pattern = "hand|wrist|thumb|finger"),
                    arm = grepl(x = tolower(tx_injuries$body_part), pattern = "arm|elbow"),
                    shoulder = grepl(x = tolower(tx_injuries$body_part), pattern = "shoulder")) %>% 
      dplyr::select(head:shoulder) %>% 
      dplyr::summarise_all(sum)
    
    #Determine the % of injuries for each body part
    total_body_part <- tidyr::gather(injury_categories, key = "body_part_grp", value = "total") %>% 
      mutate(perc = total/nrow(tx_injuries)*100)
    
    #Read in body part polygons
    body <- sf::st_read(dsn = "./inst/extdata/body_shp", layer = "body_parts2") %>% 
      dplyr::rename(body_part_grp = body_part) %>% 
      dplyr::left_join(total_body_part, by = "body_part_grp")
    
    #Create x and y end for lines and labels
    lines <- tibble(x = c(7, 7.5, 7.5, 7, 7.5, 2, 2, 2),
                    y = c(-0.75, -8, -9.75, -4, -5.25, -5.25, -4, -2),
                    xend = c(5, 5.5, 5.25, 5, 5, 3.25, 3.5, 3.75),
                    yend = c(-0.75, -8, -9.75, -4, -5.25, -5.25, -4, -2),
                    label = glue::glue("{perc}%", perc = round(total_body_part$perc, 1)))
    
    #Create plot
    plot <- ggplot() +
      geom_sf(data = body, aes(fill = perc), colour = "black") + 
      scale_fill_gradient(low = "#ff6347", high = "#7f0000",
                          limits = c(0, 50), breaks = c(0, 25, 50),
                          name = "Percentage \n of injuries") +
      scale_x_continuous(limits = c(0, 10)) +
      geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend), size = 1,
                   lineend = "round") +
      geom_text(data = lines[1:5, ], aes(x = x + 0.85, y = y, label = label), family = "Ubuntu") +
      geom_text(data = lines[6:8, ], aes(x = x - 0.75, y = y, label = label), family = "Ubuntu") +
      labs(title = "Type of amusement park injuries \n by body part") +
      theme_classic() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            legend.background = element_rect(fill = "white"),
            plot.title = element_text(hjust = 0.5, size = 15, family = "Ubuntu"),
            legend.title = element_text(family = "Ubuntu", size = 12),
            legend.text = element_text(family = "Ubuntu", size = 11))
    
    ggsave("./plots/10_09_19_detailed.png", width = 5.2, height = 5.2, dpi = 300)
    
    return(plot)
    
  } else {
    
    #Classify type of body part injured into head, arm, leg, torso
    injury_categories <- tx_injuries %>% 
      dplyr::mutate(head = grepl(x = tolower(tx_injuries$body_part), pattern = "mouth|head|eye|teeth|neck|chin|face|nose|ear|lip|cheek|tooth"),
                    legs = grepl(x = tolower(tx_injuries$body_part), pattern = "knee|leg|foot|ankle|toe|thigh|foot|ankle|toe"),
                    torso = grepl(x = tolower(tx_injuries$body_part), pattern = "groin|butt|vagina|tail|glute|genital|chest|rib|back|abdomen|back|torso|midsection|hip|collar|stomach|clavical"),
                    arm = grepl(x = tolower(tx_injuries$body_part), pattern = "hand|wrist|thumb|finger|arm|elbow|shoulder")) %>% 
      dplyr::select(head:arm) %>% 
      dplyr::summarise_all(sum)
    
    #Determine the % of injuries for each body part
    total_body_part <- tidyr::gather(injury_categories, key = "body_part_grp", value = "total") %>% 
      mutate(perc = total/nrow(tx_injuries)*100)
    
    #Read in body part polygons
    body <- sf::st_read(dsn = "./inst/extdata/body_shp", layer = "body_parts") %>% 
      dplyr::rename(body_part_grp = body_part) %>% 
      dplyr::left_join(total_body_part, by = "body_part_grp")
    
    #Create x and y end for lines and labels
    lines <- tibble(x = c(7, 7, 7.5, 2),
                    y = c(-1, -4, -8, -2),
                    xend = c(5, 5, 5.5, 3.75),
                    yend = c(-1, -4, -8, -2),
                    label = glue::glue("{perc}%", perc = round(total_body_part$perc, 1)))
    
    #Create plot
    plot <- ggplot() +
      geom_sf(data = body, aes(fill = perc), colour = "black") + 
      scale_fill_gradient(low = "#ff6347", high = "#7f0000",
                          limits = c(0, 60), breaks = c(0, 20, 40, 60),
                          name = "Percentage \n of injuries") +
      scale_x_continuous(limits = c(0, 10)) +
      geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend), size = 1,
                   lineend = "round") +
      geom_text(data = lines[4, ], aes(x = x - 0.75, y = y, label = label), family = "Ubuntu") +
      geom_text(data = lines[1:3, ], aes(x = x + 0.75, y = y, label = label), family = "Ubuntu") +
      labs(title = "Type of amusement park injuries \n by body part") +
      theme_classic() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            axis.ticks = element_blank(),
            panel.border = element_rect(colour = "black", fill = NA),
            plot.title = element_text(hjust = 0.5, size = 15, family = "Ubuntu"),
            legend.title = element_text(family = "Ubuntu", size = 12),
            legend.text = element_text(family = "Ubuntu", size = 11))
    
    ggsave("./plots/10_09_19.png", width = 5.2, height = 5.2, dpi = 300)
    
    return(plot)
    
  }
  
}