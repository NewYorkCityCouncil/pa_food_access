library(leaflet)
library(tidyverse)
library(councildown)
library(lubridate)
library(leaflet.extras)

source("code/load_farmers_boxes.R")

pal <- colorFactor(nycc_pal("cool")(2), domain = markets_boxes$service_type)

make_caption <- function(facilityname,
                         hours,
                         website,
                         start,
                         end,
                         address,
                         accepts_ebt) {
  
  if (!is.na(website)){
    out <- paste("<div style=\"font-family:'Open Sans', sans-serif;\">","<a href='", website, "' target='_blank'>", facilityname, "</a>", "<br>",
                 "<small><em>", start, "-",  end, "<br>", address, "</small></em>",
                 "<hr>", 
                 knitr::kable(hours, format = "html"), "<br>",
                 "EBT:", ifelse(accepts_ebt, "Yes", "No"),
                 "</div>")
  } else {
    out <- paste("<div style=\"font-family:'Open Sans', sans-serif;\">", facilityname, "<br>",
                 "<small><em>", start, "-",  end, "<br>", address, "</small></em>",
                 "<hr>", 
                 knitr::kable(hours, format = "html"), "<br>",
                 "EBT:", ifelse(accepts_ebt, "Yes", "No"),
                 "</div>")
  }
  
  return(out)
}


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
                                 accepts_ebt), make_caption)) 


(market_map <- to_map %>% 
  leaflet() %>% 
  addCouncilStyle() %>% 
  addPolygons(data = dists, fill = FALSE, weight = .5, color = "black", opacity = .2) %>% 
  addCircles(color = ~pal(service_type), radius = 1.5,
             popup = ~caption,
             group = "markets") %>%
  addLegend(pal = pal, values = ~ service_type,
            title = "", position = "bottomleft") %>%
    addSearchOSM(options = list(position = "topright", collapsed = FALSE, zoom = 14, marker = TRUE)) %>% 
    addControlGPS(options = gpsOptions(autoCenter = TRUE, setView = TRUE, maxZoom = 14))
)

htmlwidgets::saveWidget(market_map, "market_map.html")
file.rename("market_map.html", "results/market_map.html")

