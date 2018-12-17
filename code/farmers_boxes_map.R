library(leaflet)
library(tidyverse)
library(councildown)
library(lubridate)

source("code/load_farmers_boxes.R")

pal <- colorFactor(nycc_pal("cool")(2), domain = markets_boxes$service_type)

to_map <- markets_boxes %>% 
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
                       drop_na(Hours)),
         start = paste(month.name[month(startdate)], day(startdate)),
         end = paste(month.name[month(enddate)], day(enddate)),
         start = ifelse(open_year_round, "January 1", start),
         end = ifelse(open_year_round, "December 31", end)) %>% 
  mutate(caption = pmap_chr(list(facilityname, 
                                 hours, 
                                 website, 
                                 start,
                                 end,
                                 address,
                                 accepts_ebt), ~paste("<a href='", ..3, "' target='_blank'>", ..1, "</a>", "<br>",
                                                  "<small><em>", ..4, "-",  ..5, "<br>", ..6, "</small></em>",
                                                  "<hr>", 
                                                  knitr::kable(..2, format = "html"), "<br>",
                                                  "EBT:", ifelse(..7, "Yes", "No")))) 
market_map <- to_map %>%     
  leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addCircles(color = ~pal(service_type), radius = 1,
             popup = ~caption) %>% 
  addLegend(pal = pal, values = ~ service_type,
            title = "", position = "bottomleft")

htmlwidgets::saveWidget(market_map, "market_map.html")
file.rename("market_map.html", "results/market_map.html")
