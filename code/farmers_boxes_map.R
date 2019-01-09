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
    out <- paste(
      #"<div style=\"font-family:'Open Sans', sans-serif;\">",
      "<h4><a href='", website, "' target='_blank'>", facilityname, "</a></h4>",
      "<small><em>", start, "-",  end, "<br>", address, "</em></small>",
      "<hr>", 
      knitr::kable(hours, format = "html"), "<br>",
      "EBT:", ifelse(accepts_ebt, "Yes", "No")
      #,
      #"</div>"
    )
  } else {
    out <- paste(
      #"<div style=\"font-family:'Open Sans', sans-serif;\">",
      "<h4>", facilityname, "</h4>",
      "<small><em>", start, "-",  end, "<br>", address, "</em></small>",
      "<hr>", 
      knitr::kable(hours, format = "html"), "<br>",
      "EBT:", ifelse(accepts_ebt, "Yes", "No")
      #,
      # "</div>"
    )
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


bounds <- st_bbox(to_map)
names(bounds) <- NULL

(market_map <- to_map %>% 
    leaflet() %>% 
    addCouncilStyle() %>% 
    addCircleMarkers(color = ~pal(service_type), radius = 2,
               popup = ~councilPopup(caption),
               group = "markets",
               fillOpacity = 1,
               weight = 50,
               opacity = 0) %>%
    addLegend(pal = pal, values = ~ service_type,
              title = "", position = "bottomleft") %>%
    addSearchOSM(options = list(position = "topright", collapsed = FALSE, zoom = 14, marker = TRUE)) %>% 
    addControlGPS(options = gpsOptions(autoCenter = TRUE, setView = TRUE, maxZoom = 14)) %>% 
    setView(mean(bounds[c(1,3)]), mean(bounds[c(2,4)]), zoom = 10.5)
)

htmlwidgets::saveWidget(market_map, "market_map.html")
file.rename("market_map.html", "results/market_map.html")

