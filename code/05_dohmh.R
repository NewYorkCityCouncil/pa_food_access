library(leaflet)
library(tidyverse)
library(councildown)
library(lubridate)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)

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
      "<strong>SNAP</strong>:", ifelse(accepts_ebt, "Yes", "No")
      #,
      #"</div>"
    )
  } else {
    out <- paste(
      "<h4>", facilityname, "</h4>",
      "<small><em>", start, "-",  end, "<br>", address, "</em></small>",
      "<hr>", 
      knitr::kable(hours, format = "html"), "<br>",
      "<strong>SNAP</strong>:", ifelse(accepts_ebt, "Yes", "No")
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
