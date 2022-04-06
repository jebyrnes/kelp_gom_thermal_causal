library(sf)
library(tmap)


geodata <- combined_bio_data %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

tmap_mode("view")


tm_basemap(leaflet::providers$Esri.WorldImagery)  +
tm_shape(geodata) +
    tm_markers(popup.vars = c("year", "region", "site"))
