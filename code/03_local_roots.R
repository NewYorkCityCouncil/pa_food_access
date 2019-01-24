library(tidyverse)
library(ggmap)
library(sf)

local_roots <- tibble::tribble(
  ~Name,            ~Address,          ~Hours,        ~Day,
  "61 Local",      "61 Bergen St",         "5-8pm",   "TUESDAY",
  "Brooklyn Farmacy and Soda Fountain",      "513 Henry St", "5:30 - 7:30pm",   "TUESDAY",
  "Threes @ Franklin + Kent",  "113 Franklin St.",         "6-8pm",   "TUESDAY",
  "Irving Farm Coffee Roasters",      "1424 3rd Ave",         "5-7pm",   "TUESDAY",
  "Berg'n", "899 Bergen Street",         "6-8pm", "WEDNESDAY",
  "Irving Farm Coffee Roasters",   "71 Irving Place",         "5-7pm", "WEDNESDAY",
  "Alexandria Center",    "430 E. 29th St",   "3:30-6:30pm", "WEDNESDAY",
  "Navy Wine Merchants",  "138 Flushing Ave",         "6-8pm", "WEDNESDAY",
  "Stone Fruit Espresso",  "1058 Bedford Ave",         "5-7pm",  "THURSDAY",
  "Crystal Lake",  "647 Grand Street",   "6:00-8:00pm",  "THURSDAY",
  "Footlight",    "465 Seneca Ave",   "6:00-8:00pm",  "THURSDAY"
)


register_google(Sys.getenv("GEOCODE_API_KEY"))


locations <- geocode(paste(local_roots$Address, "New York, NY"))


make_caption_lr <- function(title, website, address, day, hours) {
  out <- paste(
    "<h4><a href='", website, "' target='_blank'>", title, "</a></h4>",
    "<small><em>", address, "</em></small>",
    "<hr>",
    "<strong>Dates & hours</strong>:", day, hours
  )
  
  
  return(out)
}

local_roots <- local_roots %>% 
  bind_cols(locations) %>% 
  mutate(type = "Food Boxes",
         website = "https://localrootsnyc.com/",
         Day = tools::toTitleCase(tolower(Day)),
         caption = pmap_chr(list(Name, website, Address, Day, Hours), make_caption_lr)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs") %>% 
  select(caption, type)

