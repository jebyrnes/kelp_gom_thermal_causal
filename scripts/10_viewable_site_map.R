#' -------------------------------------------------
#' 
#' Quick script to make an interactive leaflet map
#' in order to visualize where sample sites
#' occured for conversation between authors or 
#' enhanced understanding by readers
#' 
#' @author Jarrett Byrnes
#' -------------------------------------------


library(sf)
library(tmap)


geodata <- combined_bio_data %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tmap_mode("view")


tm_basemap(leaflet::providers$Esri.WorldImagery)  +
tm_shape(geodata) +
    tm_markers(popup.vars = c("year", "region", "site"))
