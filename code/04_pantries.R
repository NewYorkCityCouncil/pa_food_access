library(tidyverse)
library(sf)

pantries_raw <- read_csv("data/Pantries2.csv")

make_caption_pantry <- function(facilityname,
                               day_hours,
                               calendar,
                               address,
                               #description,
                               ebt) {
  

    out <- paste(
      "<h4>", facilityname, "</h4>",
      "<small><em>", calendar, "<br>", address, "<br>",  "</em></small>",
      "<hr>", 
      "<strong>Days open:</strong>", day_hours, "<br>",
       ifelse(!is.na(ebt), ebt, NULL)
    )
  
  return(out)
}


pantries <- pantries_raw %>% 
  drop_na(longitude) %>% 
  select(Food.Pantry, address = Address2, location_desc = Address, Phone, Days, Snap2, Calendar, longitude, latitude) %>% 
  mutate_if(is.character, ~str_trim(tools::toTitleCase(tolower(.)))) %>% 
  mutate(Days = str_remove_all(pantries_raw$Days, "\\\\n"),
         caption = pmap_chr(list(Food.Pantry, Days, Calendar, address, Snap2), make_caption_pantry),
         type = "Fresh Pantry Project") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  select(caption, type)
