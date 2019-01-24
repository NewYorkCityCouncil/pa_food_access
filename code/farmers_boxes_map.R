library(leaflet)
library(tidyverse)
library(councildown)
library(lubridate)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

source("code/load_farmers_boxes.R")
source("code/util.R")
source("code/just_food_scrape.R")
source("code/local_roots.R")


make_caption_dohmh <- function(facilityname,
                         hours,
                         website,
                         start,
                         end,
                         address,
                         accepts_ebt) {
  
  if (!is.na(website)){
    out <- paste(
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


dohmh <- markets_boxes %>% 
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
                                 accepts_ebt), make_caption_dohmh)) 


just_food <- just_food %>% 
  st_as_sf(coords = c("loc_long", "loc_lat"), crs = st_crs(dohmh))


to_map <- dohmh %>% 
  select(caption, type = service_type) %>% 
  rbind(just_food %>% select(caption, type)) %>%
  mutate(type = case_when(
    type == "CSA site" ~ "CSAs",
    type == "Farmers Market Site" ~ "Farmers Markets",
    TRUE ~ type
  )) %>% 
  rbind(local_roots)

pal <- colorFactor(nycc_pal()(3), domain = to_map$type)


bounds <- st_bbox(to_map)
names(bounds) <- NULL

(market_map <- to_map %>% 
    leaflet() %>% 
    addCouncilStyle() %>% 
    addCircleMarkers(color = ~pal(type), radius = 4,
               popup = ~councilPopup(caption),
               group = ~type,
               fillOpacity = 1,
               weight = 30,
               opacity = 0) %>%
    addLegend(pal = pal, values = ~ type,
              title = "", position = "bottomleft") %>%
    addControlGPS(options = gpsOptions(autoCenter = TRUE, setView = TRUE, maxZoom = 14)) %>% 
    addLayersControl(overlayGroups = ~unique(type), position = "bottomright", options = layersControlOptions(collapsed = FALSE, sortLayers = "false")) %>% 
    setView(mean(bounds[c(1,3)]), mean(bounds[c(2,4)]), zoom = 10.5) %>% 
    registerPlugin(geocoder) %>% 
    registerPlugin(fontawsome_markers) %>% 
    onRender(geocode_js, data = list(key = Sys.getenv("GEOCODE_API_KEY"))) %>%
    prependContent(tags$link(href = "https://use.fontawesome.com/releases/v5.0.8/css/all.css", rel = "stylesheet")) %>% 
    identity()
)

htmlwidgets::saveWidget(market_map, "market_map.html", selfcontained = FALSE)
unlink("results/market_map_files/", recursive = TRUE)
file.rename("market_map.html", "results/market_map.html")
file.rename("market_map_files", "results/market_map_files")

to_map %>% 
  filter(type == "Farmers Markets") %>% 
  summarize(n = n())
