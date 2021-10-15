#' -------------------------------------------
#' Process compositional data
#' 
#' -------------------------------------------

#--- load libraries and get environment setup
setwd(here::here()) 
library(tidyverse)

comp_data <- read_csv("derived_data/compositional_change_data.csv") %>%
    mutate( region = factor(region,
                            levels = c("Downeast", "MDI", "Penobscot Bay",
                                       "Midcoast", "Casco Bay", "York")))

ggplot(comp_data %>% filter(type == "kelp"), 
       aes(x = region, y = cover, color = factor(year))) +
    #    geom_point(position = position_dodge(width = 0.5)) +
    stat_summary(alpha = 0.9) +
    coord_flip() +
    facet_wrap(vars(sp_code)) +
    labs(color = "Year",
         y = "% Cover",
         x = "") +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Accent")




ggplot(comp_data %>% filter(type != "kelp"), 
       aes(x = sp_code, y = cover, color = factor(year))) +
    #    geom_point(position = position_dodge(width = 0.5)) +
    stat_summary(alpha = 0.9) +
    coord_flip() +
    facet_wrap(vars(region)) +
    labs(color = "Year",
         y = "% Cover",
         x = "") +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Accent")

