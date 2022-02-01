library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(scales)

#get map and check
new_england <- st_as_sf(states50) %>%
    filter(name %in% c("Maine", "New Hampshire", "Massachusetts", "Rhode Island",
                       "Vermont", "Connecticut"))


#crop box
mbox <- c(xmin = -71.5,
          xmax = -66,
          ymin = 43.,
          ymax = 45) %>% st_bbox()


ggplot() +
    geom_sf(data = new_england %>% st_crop(mbox) , fill = "lightgreen") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          panel.background = element_rect(fill = "lightblue"))

#get sites
sites <- read_csv("derived_data/combined_bio_data.csv") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs=4326)  %>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York"))) %>%
    group_by(year, region, site) %>%
    slice(1L)

# get buoys
buoy_ids <- read_csv("raw_data/buoyID.csv") %>%
    st_as_sf(coords = c("longitude", "latitude"), crs =4326)

# The plot
ggplot() +
    geom_sf(data = new_england %>% st_crop(mbox) , fill = "#AAd1AC") +
    geom_sf(data = sites, aes(color = region)) +
    geom_sf(data = buoy_ids, color = "red", shape = 17, size = 5, alpha = 0.7) +
    theme_bw(base_size = 14) +
    theme(axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          panel.grid.major = element_blank(),
          #panel.background = element_rect(fill = "#9ebddc77"),
          panel.background = element_rect(fill = "white"),
          legend.title.align = 0.5,
          legend.position = "bottom") +
    scale_color_brewer(type = "div") +
    scale_fill_brewer(type = "div") +
    labs(color = "Sites Sampled", fill = "Sites Sampled") +
    guides(colour = guide_legend(title.position = "top",
                                 override.aes = list(shape = 15, size = 5))
    )


ggsave("figures/map_of_project.jpg", dpi = 800)
