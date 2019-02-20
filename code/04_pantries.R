library(tidyverse)
library(sf)

pantries_raw <- read_csv("data/food_pantry_updated.csv")

make_caption_pantry <- function(facilityname,
                               day_hours,
                               # calendar,
                               address,
                               #description,
                               ebt) {
  

    out <- paste(
      "<h4>", facilityname, "</h4>",
      "<small><em>", address, "<br>",  "</em></small>",
      "<hr>", 
      "<strong>Days open:</strong>", ifelse(is.na(day_hours), "Contact for information", day_hours), "<br>",
       ifelse(!is.na(ebt), ebt, "")
    )
  
  return(out)
}


pantries <- pantries_raw %>% 
  drop_na(longitude) %>% 
  select(Food.Pantry, address = Address2, Phone, Days, Snap2, longitude, latitude) %>% 
  mutate_if(is.character, ~str_trim(tools::toTitleCase(tolower(.)))) %>% 
  mutate(Days = str_remove_all(pantries_raw$Days, "\\\\n"),
         caption = pmap_chr(list(Food.Pantry, Days, address, Snap2), make_caption_pantry),
         type = "Fresh Pantry Project") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  select(caption, type)
