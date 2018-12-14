library(leaflet)
library(tidyverse)
library(councildown)

source("code/load_farmers_boxes.R")

pal <- colorFactor(nycc_pal("cool")(2), domain = markets_boxes$service_type)

markets_boxes %>% 
  # as_tibble() %>% 
  mutate_at(vars(sunday, monday, tuesday, wednesday, thursday, friday, saturday), as.character) %>% 
  mutate(hours = pmap(list(sunday, monday, tuesday, wednesday, thursday, friday, saturday), 
                     ~ tibble(Sunday = ..1,
                              Monday = ..2, 
                              Tuesday = ..3, 
                              Wednesday = ..4, 
                              Thursday = ..5, 
                              Friday = ..6, 
                              Saturday = ..7) %>% 
                       gather("Day", "Hours") %>% 
                       drop_na(Hours))) %>%
  mutate(caption = pmap_chr(list(facilityname, hours), ~paste(..1, "<hr>", knitr::kable(..2, format = "html")))) %>% 
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(color = ~pal(service_type), radius = 1,
             popup = ~caption) %>% 
  addLegend(pal = pal, values = ~ service_type)
