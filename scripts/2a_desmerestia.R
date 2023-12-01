library(ggplot2)
library(ggridges)
library(dplyr)
library(readr)
library(patchwork)

theme_set(theme_bw(base_size = 12))

rasher_steneck_combined <- read_csv("derived_data/rasher_steneck_combined.csv") %>%
    filter(depth == 5) %>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = rev(c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York"))))


regional <- ggplot(rasher_steneck_combined,
       aes(x = desm,
           y = region)) +
    stat_density_ridges(from = 0) +
    labs(x = "Percent Desmerestia", title = "B", y = "") 


all <- ggplot(rasher_steneck_combined,
       aes(x = desm)) +
    stat_density(fill = "lightgrey", color = "black") +
    labs(x = "Percent Desmerestia", title = "A", y = "")


all + regional 
ggsave("figures/desmarestia_supplement.jpg", dpi = 600,
       width = 12)



# What are the ranges?
mean(rasher_steneck_combined$desm)
range(rasher_steneck_combined$desm)
sd(rasher_steneck_combined$desm)
