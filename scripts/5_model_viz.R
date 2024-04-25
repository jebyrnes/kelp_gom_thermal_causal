#' ------------------------------------------------------------------------------------
#' Code to query and visualize implications from the causal models of 
#' influence of temperature and urchins on kelp abundance.
#' 
#' @date 2023-10-30 last update
#' @author Jarrett Byrnes
#' ------------------------------------------------------------------------------------

library(ggplot2)
library(emmeans)
library(dplyr)
library(tidyr)
library(glmmTMB)
library(here) # paths to data should 'just work' (though having problems with it)
library(wesanderson)
pal <- wes_palette("Zissou1", 6, type = "continuous")

setwd(here::here())
source("scripts/2_load_combined_data.R")
mod_urchin_add <- readRDS("model_output/mod_urchin_add.RDS")

regional_values <- combined_bio_temp_gmc %>%
    dplyr::group_by(region) %>%
    dplyr::select(mean_regional_urchin, mean_mean_temp_spring, mean_mean_temp_summer) %>%
    dplyr::slice(1L) %>%
    ungroup() %>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))

# Look at the spring temperature effect
spring_temp_effect <-
    emmeans(
        mod_urchin_add,
        ~ mean_temp_spring_dev + lag_mean_temp_summer_dev|
            mean_regional_urchin + mean_mean_temp_spring + mean_mean_temp_summer,
        at = list(
            mean_temp_spring_dev = seq(-2.5, 2.5, length.out=100),
            urchin_anom_from_region = 0,
            lag_mean_temp_summer_dev = c(-2.5, 0, 2.5),
            mean_regional_urchin = regional_values$mean_regional_urchin,
            mean_mean_temp_spring = regional_values$mean_mean_temp_spring,
            mean_mean_temp_summer = regional_values$mean_mean_temp_summer
        ),
        type = "response", rg.limit = 324000
    ) %>%
    as_tibble() %>%
    right_join(regional_values) 

add_lag_label <- function(x, value) paste("Lag Temp. Anomaly =", x[value])

ggplot(spring_temp_effect%>% mutate(lag_mean_temp_summer_dev = paste0("Lag Temp. Anomaly: ", lag_mean_temp_summer_dev)),
       aes(x = mean_temp_spring_dev, y = 100*response, color = region)) +
    geom_line(size = 1) +
    facet_wrap(vars(lag_mean_temp_summer_dev)) +
    theme_bw(base_size = 12) +
    scale_color_manual(values = pal) +
    theme(legend.position = "bottom") +
    labs(color = "", 
         x = "Regional Spring Temperature Anomaly (C)",
         y = "Kelp Percent Cover",
         subtitle = "Urchin anomaly held at 0")#, edited for publication
         #title ="Effect of spring temperature and lagged summer\ntemperature on kelp cover")

ggsave("figures/temperature_effect_on_kelp.jpg", dpi = 600)

# Now look at urchins

# Look at the spring temperature effect
temp_by_urchin_effect <-
    emmeans(
        mod_urchin_add,
        ~ mean_temp_spring_dev |urchin_anom_from_region +
            mean_regional_urchin + mean_mean_temp_spring + mean_mean_temp_summer,
        at = list(
            mean_temp_spring_dev = seq(-2.5, 2.5, length.out=100),
            urchin_anom_from_region = c(-10,40,80),
            lag_mean_temp_summer_dev = 0,
            mean_regional_urchin = regional_values$mean_regional_urchin,
            mean_mean_temp_spring = regional_values$mean_mean_temp_spring,
            mean_mean_temp_summer = regional_values$mean_mean_temp_summer
        ),
        type = "response", rg.limit = 1e10
    ) %>%
    as_tibble() %>%
    right_join(regional_values) 


ggplot(temp_by_urchin_effect %>% mutate(urchin_anom_from_region = paste0("Urchin Anomaly: ", urchin_anom_from_region)),
       aes(x = mean_temp_spring_dev, y = 100*response, color = region)) +
    geom_line(size = 1) +
    facet_wrap(vars(urchin_anom_from_region)) +
    theme_bw(base_size = 12) +
    #scale_y_continuous(labels = function(x) paste0(x, "%")) +
#    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "bottom") +
    labs(color = "", 
         x = "Regional Spring Temperature Anomaly (C)",
         y = "Kelp Percent Cover",
         subtitle = "Lag Summer Temp Anomaly held at 0")

ggsave("figures/urchin_temperature_effect_on_kelp.jpg", dpi = 600)


# Look at how temperature affects urchin effects

# Look at the spring temperature effect
urchin_by_temp_effect <-
    emmeans(
        mod_urchin_add,
        ~  urchin_anom_from_region | mean_temp_spring_dev +
            mean_regional_urchin + mean_mean_temp_spring + mean_mean_temp_summer,
        at = list(
            mean_temp_spring_dev = c(-2.5, 0, 2.5),
            urchin_anom_from_region = -10:80,
            lag_mean_temp_summer_dev = 0,
            mean_regional_urchin = regional_values$mean_regional_urchin,
            mean_mean_temp_spring = regional_values$mean_mean_temp_spring,
            mean_mean_temp_summer = regional_values$mean_mean_temp_summer
        ),
        type = "response", rg.limit = 1e10
    ) %>%
    as_tibble() %>%
    right_join(regional_values) 


ggplot(urchin_by_temp_effect %>% mutate(mean_temp_spring_dev = paste0("Spring Temp. Anomaly: ", mean_temp_spring_dev)),
       aes(x = urchin_anom_from_region, y = 100*response, color = region)) +
    geom_line(size = 1) +
    facet_wrap(vars(mean_temp_spring_dev)) +
    theme_bw(base_size = 12) +
    scale_color_manual(values = pal) +
    theme(legend.position = "bottom") +
    labs(color = "", 
         x = "Regional Urchin Anomaly (# per sq. m)",
         y = "Kelp Percent Cover",
         subtitle = "Lag Summer Temp anomaly held at 0")

ggsave("figures/temperature_changing_urchin_effect.jpg", dpi = 600)
