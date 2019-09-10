plot_10_09_19 <- function(){
  
  library(extrafont)
  
  tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")
  
  #Classify type of body part injured into head, arm, leg, torso
  tx_injuries <- tx_injuries %>% 
    dplyr::mutate(body_part_grp = dplyr::case_when(grepl(x = tolower(tx_injuries$body_part), pattern = "mouth|head|eye|teeth|neck|chin|face|nose|ear|lip|cheek|tooth") ~ "head",
                                                   grepl(x = tolower(tx_injuries$body_part), pattern = "knee|leg|foot|ankle|toe|thigh") ~ "legs",
                                                   grepl(x = tolower(tx_injuries$body_part), pattern = "shoulder|chest|rib|back|abdomen|back|groin|torso|midsection|hip|collar|stomach|toe|butt|vagina|tail|glute|clavical|genital") ~ "torso",
                                                   grepl(x = tolower(tx_injuries$body_part), pattern = "arm|elbow|hand|wrist|thumb|finger") ~ "arm")) %>% 
    dplyr::filter(!is.na(body_part) & !is.na(body_part_grp))
  
  #Determine the % of injuries for each body part
  total_body_part <- tx_injuries %>% 
    dplyr::group_by(body_part_grp) %>% 
    summarise(total = n()) %>% 
    mutate(perc = total/n())
  
  #Read in body part polygons
  body <- sf::st_read(dsn = "./inst/extdata/body_shp", layer = "body_parts") %>% 
    dplyr::rename(body_part_grp = body_part) %>% 
    dplyr::left_join(total_body_part, by = "body_part_grp")
  
  #Create x and y end for lines and labels
  lines <- tibble(x = c(2, 7, 7, 7.5),
                  y = c(-2, -1, -4, -8),
                  xend = c(3.75, 5, 5, 5.5),
                  yend = c(-2, -1, -4, -8),
                  label = c("13.5%", "56.8%", "37.2%", "23.5%"))
  
  #Create plot
  plot <- ggplot()+
    geom_sf(data = body, aes(fill = perc), colour = "black") + 
    scale_fill_gradient(low = "#ff6666", high = "#7f0000",
                        limits = c(0, 60), breaks = c(0, 20, 40, 60),
                        name = "Percentage \n of injuries") +
    scale_x_continuous(limits = c(0, 10)) +
    geom_segment(data = lines, aes(x = x, y = y, xend = xend, yend = yend), size = 1,
                 lineend = "round") +
    geom_text(data = lines[1, ], aes(x = x - 0.75, y = y, label = label), family = "Ubuntu") +
    geom_text(data = lines[2:4, ], aes(x = x + 0.75, y = y, label = label), family = "Ubuntu") +
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
  
}