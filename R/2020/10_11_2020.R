#TidyTuesday 2020 week 46

#Load packages
library(rnaturalearth)
library(ggplot2)
library(dplyr)
library(sf)
library(gganimate)
library(extrafont)

#Phone data
tuesdata <- tidytuesdayR::tt_load(2020, week = 46)

mobile   <- tuesdata$mobile
landline <- tuesdata$landline

#Link landline and mobile data by country/year
phone_types <- mobile %>% 
  left_join(select(landline, -total_pop, -gdp_per_cap, -continent), by = c("entity", "code", "year")) %>% 
  #Remove 2016+ because the data becomes patchy after this
  filter(year <= 2015) %>% 
  #Convert NAs into 0s
  mutate(across(ends_with('subs'), .fns = ~tidyr::replace_na(., 0L))) %>% 
  #Determine total number of subscriptions in each year and then determine what % of these are mobile
  mutate(total_subs = mobile_subs + landline_subs,
         perc_mobile = mobile_subs/total_subs) %>% 
  #Remove any countries that have any missing subs data in some years
  group_by(code) %>% 
  filter(all(!is.na(perc_mobile))) %>% 
  ungroup() %>% 
  #When we create our points we don't want any that are truly size 0 (will fail to plot), so we will add a small value
  mutate(perc_mobile = dplyr::case_when(perc_mobile == 0 ~ 0.001,
                                        TRUE ~ perc_mobile))

#Load polygons of the world
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  #groupby and merge any multiple polygons per country (e.g. Tasmania)
  group_by(iso_a3) %>% 
  summarise()

#For each year, scale based on the percentage of mobile subs
plot_list <- purrr::map_df(.x = seq(min(phone_types$year), max(phone_types$year)),
           .f = function(focal_year){
             
             #Extract focal countries with data in that year
             countries <- phone_types %>% 
               filter(year == focal_year)
             
             #Filter world polygons
             world_polygons <- world %>% 
               left_join(select(countries, code, perc_mobile), by = c("iso_a3" = "code")) %>% 
               filter(!is.na(perc_mobile))
             
             #Extract geometries (need to be separate from data for scaling)
             world_filter <- world_polygons %>% 
               st_geometry()
             
             #Extract centroids of all polygons
             centroids <- st_centroid(world_filter)
             
             #Scale geometries relative to the percentage of mobile subscriptions
             #When mobile subs are 100% countries will be at actual size
             world_scaled <- (world_filter - centroids) * world_polygons$perc_mobile + centroids
             
             #Replace existing geometry with scale geometry
             world_polygons <- world_polygons %>% 
               mutate(geometry = world_scaled,
                      year = focal_year)
             
             world_polygons
             
           }) %>% 
  #Fix any broken polygons that prevent plotting
  sf::st_make_valid()

#Create a gganimate plot
animated_plot <- ggplot()+
  #Plot scaled polygons
  geom_sf(data = plot_list, aes(fill = perc_mobile)) +
  #Colour based on mobile subs
  scale_fill_gradientn(colours = paletteer::paletteer_c("grDevices::Blues", n = 100, direction = -1) %>% as.character(),
                                              limits = c(0, 1), name = "") +
  theme_void() +
  theme(legend.text = element_text(family = "Roboto")) +
  #Ensure fixed lat/long range
  scale_x_continuous(limits = c(-180, 180)) +
  scale_y_continuous(limits = c(-90, 90)) +
  labs(x = "", y = "", title = "Proportion of mobile phone subscriptions",
       subtitle = "{current_frame}", caption = "Data: OurWorldInData.org | Map: @ldbailey255") +
  theme(legend.text = element_text(family = "Ubuntu", size = 14),
        plot.title = element_text(family = "Alfa Slab One", size = 25),
        plot.subtitle = element_text(family = "Ubuntu", size = 16),
        plot.caption = element_text(family = "Ubuntu Mono", size = 10)) +
  #Use transition manual rather than transition_time
  #transition_time has problems rendering sf polygons over time (they jump around)
  transition_manual(year) +
  ease_aes("linear")

options(gganimate.dev_args = list(width = 800, height = 665))

gganimate::anim_save("./plots/10_11_20.gif", animation = animate(animated_plot, detail = 5, end_pause = 48, rewind = FALSE))
