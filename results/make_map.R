library(leaflet)
library(tidyverse)
library(councildown)
library(lubridate)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

files <- list.files(here::here("code"), pattern = "[[:digit:]]{2}.*\\.(R|r)", full.names = TRUE)

walk(files, source)

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
  rbind(local_roots) %>% 
  rbind(pantries)

pal <- colorFactor(nycc_pal()(4), domain = to_map$type)


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
