library(jsonlite)
library(tidyverse)
library(rvest)
library(tidyverse)
library(sf)

just_food_raw <- fromJSON("https://api.storepoint.co/v1/159401e25d17b1/locations")$results$locations %>% 
  as_tibble()




get_benefits <- function(url) {
  if(is.na(url)) return(NA)
  
  page <- read_html(url)
  
  text <- page %>% 
    html_nodes("p") %>% 
    html_text()
  
  benefits <- text[str_detect(text, "Benefits")] %>% 
    str_remove("Benefits") %>% 
    str_remove(":") %>% 
    str_remove("(A|a)ccepted") %>% 
    str_remove_all("\\(|\\)") %>% 
    str_trim()
  # browser()
  
  if(is_empty(benefits)) return(NA)
  
  return(benefits)
}

make_caption_jf <- function(title, website, address, desc, benefit) {
  out <- paste(
    "<h4><a href='", website, "' target='_blank'>", title, "</a></h4>",
    "<small><em>", address, "<br>", desc, "</em></small>",
    "<hr>",
    "<strong>Dates & hours</strong>: Contact for more information"
  )
  
  if(!is.na(benefit)) {
    out <- paste(out,
                 "<br><strong>Benefits accepted</strong>:", benefit)
  }
  
  return(out)
}

# test <- c()
# for (i in 1:10) {
#   test[i] <- get_benefits(just_food$website[i])
# }
# 
# map_chr(just_food$website[1:10], possibly(get_benefits, otherwise = NA))
# 

just_food <- just_food_raw %>% 
  filter(active == 1, !str_detect(tags, "Farmer / Producer")) %>% 
  select(loc_lat, loc_long, name, description, streetaddress, type = tags, website) %>% 
  mutate(website = paste0("https://www.justfood.org", website),
         benefits = map_chr(website, possibly(get_benefits, otherwise = NA)),
         caption = pmap_chr(list(name, website, streetaddress, description, benefits), make_caption_jf))
