#' ----------------------------------------------------
#' Temperature Timeseries and Temp-Nutrient Relationships
#' 
#' @author Jarrett Byrnes
#' @date 2023-10-17
#' ----------------------------------------------------

library(car) #for Anova
library(ggplot2)
library(dplyr)
library(here) # paths to data should 'just work' (though having problems with it)
library(readr)
library(betareg)
library(glmmTMB)
library(broom)
library(broom.mixed)

library(wesanderson)
pal <- wes_palette("Zissou1", 6, type = "continuous")

setwd(here::here())

### read in temp data
temp_timeseries <- read_csv("derived_data/temp_timeseries.csv") %>%
    filter(year >= 2001)%>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))


##
# Nutrients
##

# we need to get instrument corrected data to do this
# so far, no corrected data

nuts <- readxl::read_excel("raw_data/nutrients/LOBO_Bombazine_CB03_NO3.xlsx",
                           skip = 2) |>
    mutate(temp = `temperature [C]`,
           nitrate = `nitrate [uM]`,
           cdom =  `CDOM [QSDE]`)

ggplot(nuts,
       aes(x = `temperature [C]`,
           y = `nitrate [uM]`)) +
    geom_point() +
    stat_smooth(method = "gam",
                formula = y~s(x, bs = "cr"))

library(mgcv)
nut_mod <- gam(nitrate ~ s(temp,cdom), data = nuts)



augment(nut_mod, interval = "confidence") |>
    mutate(.lower = .fitted - 2*.se.fit) |>
    filter(.lower <= 0) |>
    arrange(temp)
##
# Spring
##

spring_temp <- ggplot(temp_timeseries,
                      aes(x = year, y = mean_temp_spring, color = region)) +
    geom_point(alpha = 1, size = 2) +
    #  facet_wrap(vars(region), ncol = 2) +
    #    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    stat_smooth(method = "lm", formula = y ~ x,fill = NA) +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Average Spring\nTemperature C", color = "") +
    #    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none")

spring_temp

ggsave("figures/spring_temp_trends.jpg", dpi = 600)

##
# Summer Mean
##


summer_temp <- ggplot(temp_timeseries,
                      aes(x = year, y = mean_temp_summer, color = region)) +
    geom_point(alpha = 1, size = 2) +
    # facet_wrap(vars(region), ncol = 2) +
    stat_smooth(method = "lm", formula = y ~ x, fill = NA) +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Average Summer\nTemperature C", color = "") +
    #    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 20, color = "black", lty = 2)+
    annotate("label", x = 2014.5, y = 20.8, label = "thermal threshold", label.size = 0) +
    geom_hline(yintercept = 15, color = "brown", lty = 2) +
    annotate("label", x = 2014.5, y = 15.5, 
             label = "nutrient threshold", color = "brown", label.size = 0,
             alpha = 0.5)+
    scale_y_continuous(breaks = seq(9,22, by = 2), limits = c(9,22)) 
    

summer_temp
ggsave("figures/summer_temp_trends.jpg", dpi = 600)



##
# Summer Max
##

summer_temp_max <- ggplot(temp_timeseries,
                      aes(x = year, y = max_temp_summer, color = region)) +
    geom_point(alpha = 1, size = 2) +
    # facet_wrap(vars(region), ncol = 2) +
    stat_smooth(method = "lm", formula = y ~ x, fill = NA) +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Maximum Summer\nTemperature C", color = "") +
    #    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none")+
    geom_hline(yintercept = 20, color = "black", lty = 2)+
    annotate("label", x = 2014.5, y = 20.8, label = "thermal threshold", label.size = 0) +
    geom_hline(yintercept = 15, color = "brown", lty = 2) +
    annotate("label", x = 2014.5, y = 15.5, 
             label = "nutrient threshold", color = "brown", label.size = 0,
             alpha = 0.5) +
    scale_y_continuous(breaks = seq(9,22, by = 2), limits = c(9,22))

summer_temp_max
ggsave("figures/summer_temp_trends.jpg", dpi = 600)


##
# Meanmax cor
##

temp_timeseries |> 
    summarize(cor(mean_temp_summer, max_temp_summer))


##
# Put it all together
##

##
# The combined plot
##

library(patchwork)

layout <- 
    "A#
    BC"

spring_temp + 
    summer_temp + summer_temp_max +
    plot_layout(design = layout) +
    plot_annotation(tag_levels = 'A')   

ggsave("figures/temp_both_trends.jpg", dpi = 600, width = 8, height = 7) 

##
# temperature models ####
##

temp_dat <- temp_timeseries %>%
    group_by(region, year) %>%
    slice(1L) %>%
    ungroup() %>%
    select(region, year, mean_temp_spring, mean_temp_summer, max_temp_summer)


temp_mods <- temp_dat %>%
    group_by(region) %>%
    nest() %>%
    summarize(spring_mod =
                  map(data, ~ lm(mean_temp_spring ~ year, data = .)),
              summer_mod =
                  map(data, ~ lm(mean_temp_summer ~ year, data = .)),
              summer_max_mod =
                  map(data, ~ lm(max_temp_summer ~ year, data = .)),
              spring_slope = map_dbl(spring_mod, ~coef(.)[2]),
              summer_slope = map_dbl(summer_mod, ~coef(.)[2]),
              summer_max_slope = map_dbl(summer_max_mod, ~coef(.)[2]))

temp_mods %>%
    select(-spring_mod, -summer_mod, -summer_max_mod)

lm(mean_temp_summer~ year*region, data = temp_timeseries) %>%
    saveRDS("model_output/summer_temp_timeseries.rds")
lm(max_temp_summer~ year*region, data = temp_timeseries) %>%
    saveRDS("model_output/summer_max_temp_timeseries.rds")
lm(mean_temp_spring~ year*region, data = temp_timeseries) %>%
    saveRDS("model_output/spring_temp_timeseries.rds")

#whole shebang
lm(mean_temp_spring ~ year + region, data = temp_dat) %>% coef %>% `[`(2)
lm(mean_temp_summer ~ year + region, data = temp_dat) %>% coef %>% `[`(2)
lm(max_temp_summer ~ year + region, data = temp_dat) %>% coef %>% `[`(2)
