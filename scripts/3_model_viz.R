##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Model Visualization                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Running a model to test if temp and/or urchin influences kelp cover       ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz   - May 27, 2020                                         ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Last Worked On: May 7th, 2021 (emerging from COVID like a cicada)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

library(ggplot2)
library(modelr)
library(dplyr)
library(tidyr)
library(glmmTMB)
library(here) # paths to data should 'just work' (though having problems with it)
source("https://raw.githubusercontent.com/glmmTMB/glmmTMB/master/glmmTMB/inst/misc/rsqglmm.R")

setwd(here::here())

DF.join <- read_csv("derived_data/combined_data_for_analysis.csv")
mod_urchin_add <- readRDS("derived_data/mod_urchin_add.RDS")


# Some info we will need
region_df <- DF.join %>%
    group_by(region) %>%
    summarize(mean_temp_mn = mean_temp_mn[1],
              urchin_mn = urchin_mn[1])

timeseries_df <- DF.join %>%
    group_by(year) %>%
    summarize(urchin_dev = mean(urchin_dev, na.rm=TRUE),
              mean_temp_dev = mean(mean_temp_dev, na.rm=TRUE),
              urchin = mean(urchin, na.rm = TRUE)) %>%
    arrange(year)


# heatmap-a-palooza
make_kelp_heatmap <- function(mean_temp_mn = 15, urchin_mn = 0,
                              xlim = c(0,80)){
    visreg::visreg2d(mod_urchin_add, xvar = "urchin_dev", yvar = "mean_temp_dev", 
                     scale = "response", plot.type="gg", 
                     cond = list(urchin_mn = urchin_mn,
                                 mean_temp_mn = mean_temp_mn)) +
        scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                             midpoint = 0.5, limits = c(0,1)) +
        xlim(xlim) +
        labs(fill = "Percent Cover of Kelp",
             y = "Deviation from Regional Temperature Mean",
             x = "Deviation from Regional Urchin Mean",
             subtitle = paste0("Assumes regional mean summer temp of ",
                               mean_temp_mn, "C \nand urchin abundance of ", urchin_mn," per sq m"))
    
    
}


# two sites at 0 and 50 urchins
# note - we start with known urchin density and temp
# then subtract our regional means to get the anomalies

pred_frame <- crossing(region_df,
         urchin  = c(0,50),
                temp = seq(11, 18, length.out = 100),
         year = 2015) %>%
    mutate(mean_temp_dev = temp - mean_temp_mn,
           urchin_dev = urchin - urchin_mn)

pred_values <- predict(mod_urchin_add, 
                                newdata = pred_frame, 
                                re.form = NULL, 
                                type = "response",
                                se.fit = TRUE)

pred_frame <- pred_frame %>%
    mutate(kelp.perc = pred_values$fit,
           lwr = kelp.perc - 1*pred_values$se.fit,
           upr = kelp.perc + 1*pred_values$se.fit,
           lwr = ifelse(lwr < 0, 0, lwr))
#all sites
ggplot(pred_frame,
       aes(x = temp,
           y = kelp.perc,
           group = region, 
           color = mean_temp_mn)) +
    geom_line() +
    facet_wrap(~urchin, ncol = 1) +
    scale_color_gradient(low = "blue", high = "red")
# 

#york v. downeast
ggplot(pred_frame %>% 
           filter(region %in% c("downeast", "york")) %>%
           mutate(urchin = paste0(urchin, " urchins"),
                  region = stringr::str_to_title(region)),
       aes(x = temp,
           y = kelp.perc*100,
           group = region, 
           color = region)) +
    geom_line(size = 2) +
    geom_ribbon(aes(ymin = lwr*100, ymax = upr*100), alpha = 0.2, color = NA, fill = "grey") +
    facet_wrap(~urchin, ncol = 1) +
    scale_color_manual(values = c("blue", "red")) +
    theme_bw() +
    labs(color = "",
         x = "Temperature C",
         y = "Percent Cover Kelp")


## scenarios!

scenario <- crossing(region_df, timeseries_df)


scenario_values <- predict(mod_urchin_add, 
                       newdata = scenario, 
                       re.form = NULL, 
                       type = "response",
                       se.fit = TRUE)

scenario <- scenario %>%
    mutate(kelp.perc = scenario_values$fit,
           lwr = kelp.perc - 1*scenario_values$se.fit,
           upr = kelp.perc + 1*scenario_values$se.fit,
           lwr = ifelse(lwr < 0, 0, lwr))




#york v. downeast
ggplot(scenario %>% 
           filter(region %in% c("downeast", "york")) %>%
           mutate(region = stringr::str_to_title(region)),
       aes(x = year,
           y = kelp.perc*100,
           group = region, 
           color = region)) +
    geom_line(size = 2) +
    geom_ribbon(aes(ymin = lwr*100, ymax = upr*100), alpha = 0.2, color = NA, fill = "grey") +
    scale_color_manual(values = c("blue", "red")) +
    theme_bw() +
    labs(color = "",
         x = "",
         y = "Percent Cover Kelp")

## What happened according to the model!
fit_df <- DF.join %>%
    filter(!is.na(urchin)) %>%
    group_by(region, year) %>%
    summarize_all(mean, na.rm = TRUE) %>%
    ungroup()

fit_values <- predict(mod_urchin_add, 
                           newdata = fit_df, 
                           re.form = NULL, 
                           type = "response",
                           se.fit = TRUE)

fit_df <- fit_df %>%
    mutate(kelp.perc_raw = kelp.perc,
           kelp.perc = fit_values$fit,
           lwr = kelp.perc - 1*fit_values$se.fit,
           upr = kelp.perc + 1*fit_values$se.fit,
           lwr = ifelse(lwr < 0, 0, lwr))



#york v. downeast
ggplot(fit_df %>% 
           filter(region %in% c("downeast", "york")) %>%
           mutate(region = stringr::str_to_title(region)),
       aes(x = year,
           y = kelp.perc*100,
           group = region, 
           color = region)) +
    geom_line(size = 2) +
    geom_ribbon(aes(ymin = lwr*100, ymax = upr*100), alpha = 0.2, color = NA, fill = "grey") +
    scale_color_manual(values = c("blue", "red")) +
    theme_bw() +
    labs(color = "",
         x = "",
         y = "Percent Cover Kelp") +
    geom_point(aes(y = kelp.perc_raw*100), size = 3)


# all

ggplot(fit_df %>% 
           mutate(region = stringr::str_to_title(region)),
       aes(x = year,
           y = kelp.perc*100,
           group = region)) +
    geom_line(size = 2) +
    geom_ribbon(aes(ymin = lwr*100, ymax = upr*100), alpha = 0.2, color = NA, fill = "grey") +
    theme_bw() +
    labs(color = "",
         x = "",
         y = "Percent Cover Kelp") +
    geom_point(aes(y = kelp.perc_raw*100), size = 3) +
    facet_wrap(~region)
