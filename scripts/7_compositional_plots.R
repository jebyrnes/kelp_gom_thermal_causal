#' -------------------------------------------
#' Plot composition data
#' 
#' -------------------------------------------

#--- load libraries and get environment setup
setwd(here::here()) 
library(tidyverse)

comp_data <- read_csv("derived_data/compositional_change_data.csv") %>%
    mutate( region = factor(region,
                            levels = rev(c("Downeast", "MDI", "Penobscot Bay",
                                       "Midcoast", "Casco Bay", "York"))),
            year = factor(year, levels = c(2018, 2004)))

ggplot(comp_data %>% filter(type == "kelp"), 
       aes(x = region, y = cover, color = factor(year))) +
    geom_point(alpha = 0.2, position = position_dodge(width = 1)) +
    stat_summary(alpha = 1, position = position_dodge(width = 1),
                 fun.data = mean_cl_boot) +
    coord_flip() +
    facet_wrap(vars(sp_code)) +
    labs(color = "Year",
         y = "% Cover",
         x = "") +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Accent")
ggsave("figures/kelp_composition_2004_2018.jpg", dpi = 600)


ggplot(comp_data %>% filter(type != "kelp"), 
       aes(x = region, y = cover, color = factor(year))) +
    geom_point(alpha = 0.2, position = position_dodge(width = 1)) +
    stat_summary(alpha = 1, position = position_dodge(width = 1),
                 fun.data = mean_cl_boot) +
    coord_flip() +
    facet_wrap(vars(sp_code)) +
    labs(color = "Year",
         y = "% Cover",
         x = "") +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Accent")

ggsave("figures/kelp_understory_2004_2018.jpg", dpi = 600)

