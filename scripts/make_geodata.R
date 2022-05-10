library(sf)
library(dplyr)
library(readr)
setwd(here::here())
geodata <- read_csv("derived_data/combined_bio_data.csv") %>%
    select(year, region, latitude, longitude, site) %>%
    st_as_sf(coords = c("longitude", "latitude"), 
             crs = 4326)


saveRDS(geodata, "shiny/site_data.rds")
