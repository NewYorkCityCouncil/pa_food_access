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

cols <- c( "#0f518a",
           "#8cc3f2",
           "#e69500",
           "#ffc14d")


pal <- colorFactor(cols, domain = to_map$type)


bounds <- st_bbox(to_map)
names(bounds) <- NULL

make_map <- function(mobile) {
  stroke <- ifelse(mobile, 40, 10)
  
  (market_map <- to_map %>% 
      leaflet() %>% 
      addCouncilStyle() %>% 
      addCircleMarkers(color = ~pal(type), radius = 4,
                 popup = ~councilPopup(caption),
                 group = ~type,
                 fillOpacity = 1,
                 weight = stroke,
                 opacity = 0) %>%
      addLegend(pal = pal, values = ~ type,
                title = "", position = "bottomleft", opacity = 1) %>%
      addControlGPS(options = gpsOptions(autoCenter = TRUE, setView = TRUE, maxZoom = 14)) %>% 
      addLayersControl(overlayGroups = ~unique(type), position = "bottomright", options = layersControlOptions(collapsed = FALSE, sortLayers = "false")) %>% 
      setView(-73.88099670410158,40.72540497175607,  zoom = 10.5) %>% 
      registerPlugin(geocoder) %>% 
      registerPlugin(fontawsome_markers) %>% 
      onRender(geocode_js, data = list(key = Sys.getenv("GEOCODE_API_KEY"))) %>%
      prependContent(tags$link(href = "https://use.fontawesome.com/releases/v5.0.8/css/all.css", rel = "stylesheet")) %>% 
      identity()
  )
  
  file_base <- "market_map"
  if(mobile) file_base <- paste0(file_base, "_mobile")
  
  htmlwidgets::saveWidget(market_map, paste0(file_base, ".html"), selfcontained = FALSE)
  unlink(paste0("results/", file_base, "_files/"), recursive = TRUE)
  file.rename(paste0(file_base, ".html"), paste0("results/", file_base, ".html"))
  file.rename(paste0(file_base, "_files/"), paste0("results/", file_base, "_files/"))
  
  market_map
}

make_map(mobile = FALSE)
make_map(mobile = TRUE)

to_map %>% 
  # filter(type == "Farmers Markets") %>% 
  summarize(n = n())
