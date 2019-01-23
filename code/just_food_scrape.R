library(jsonlite)
library(tidyverse)

just_food_raw <- fromJSON("https://api.storepoint.co/v1/159401e25d17b1/locations")$results$locations %>% 
  as_tibble()  
