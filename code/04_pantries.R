library(tidyverse)
library(sf)

pantries_raw <- read_csv("data/Pantries2.csv")

make_caption_pantry <- function(facilityname,
                               day_hours,
                               calendar,
                               address,
                               description,
                               ebt) {
  

    out <- paste(
      "<h4>", facilityname, "</h4>",
      "<small><em>", calendar, "<br>", address, "<br>", description, "</em></small>",
      "<hr>", 
      "<strong>Days open:</strong>", day_hours, "<br>",
      "<strong>SNAP</strong>:", ifelse(str_detect(ebt, "Ebt"), "Yes", "No")
    )
  
  return(out)
}


pantries <- pantries_raw %>% 
  drop_na(longitude) %>% 
  select(Food.Pantry, address = Address2, location_desc = Address, Phone, Days, Programs, Calendar, longitude, latitude) %>% 
  mutate_if(is.character, ~str_trim(tools::toTitleCase(tolower(.)))) %>% 
  mutate(Days = str_remove_all(pantries_raw$Days, "\\\\n"),
         caption = pmap_chr(list(Food.Pantry, Days, Calendar, address, location_desc, Programs), make_caption_pantry),
         type = "Food Pantry") %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  select(caption, type)
