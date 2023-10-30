#' ---------------------------------------------------------------
#' Urchin Size Distributions over Time as a response to reviewer
#' 
#' @author Jarrett Byrnes
#' @date 2023-10-30 last update
#' ---------------------------------------------------------------


library(car) #for Anova
library(ggplot2)
library(dplyr)
library(here) # paths to data should 'just work' (though having problems with it)
library(readxl)
library(broom)
library(broom.mixed)
library(ggridges)
library(emmeans)

library(wesanderson)
pal <- wes_palette("Zissou1", 6, type = "continuous")
theme_set(theme_bw(base_size = 16))

setwd(here::here())

# data to get regions
source("scripts/2_load_combined_data.R")
north_limit <- combined_bio_temp_gmc |>
    select(region, latitude) |>
    group_by(region) |>
    filter(latitude == max(latitude)) |>
    ungroup() |>
    arrange(desc(latitude))

as.data.frame(north_limit)

# urchin data
urchin_size <- read_excel("raw_data/Urchin Diameters 2001-2018.xlsx") |> 
    filter(`Depth stratum code` == 1,
           is.na(`Sentinel number`),
           !is.na(Diameter))

urchin_size <- urchin_size |>
    mutate(region = case_when( #from north_limit above
        Latitude <= 43.54978 ~ "York",
        Latitude <= 43.75650 ~ "Casco Bay",
        Latitude <= 43.92510 ~ "Midcoast",
        Latitude <= 44.11994 ~ "Penobscot Bay",
        Latitude <= 44.39520 ~ "MDI",
        Latitude > 44.39520 ~ "Downeast",
        TRUE ~ "unlabeled",
        
    )) |>
    mutate(region = fct_relevel(region, 
                                c("York", "Casco Bay", "Midcoast",
                                  "Penobscot Bay", "MDI", "Downeast")),
           sample = paste(Latitude, Longitude)) #replicate


# all of it
ggplot(urchin_size,
       aes(x = Diameter,
           y = region)) +
    stat_density_ridges()

# by time
ggplot(urchin_size,
       aes(x = Diameter,
           y = as.character(Year))) +
    stat_density_ridges() +
    facet_wrap(vars(region)) +
    labs(y = "")


ggsave("figures/urchin_size_over_time.jpg", dpi = 600,
       height = 8)

size_mod <- glmmTMB(Diameter ~ Year*region + (1|sample), data = urchin_size)

car::Anova(size_mod)


## 
# posthoc trend analysis
## 

size_trends <- emtrends(size_mod, "Year", specs =~ region)


size_trends |>
    broom.mixed::tidy() %>%
    rename(Region = region,
           `Trend over Time` = Year.trend,
           SE = `std.error`,
           Z = statistic,
           p = p.value) |>
    write_csv("tables/urchin_size_trends.csv")

size_trends |>
    contrast(method = "pairwise") %>%
    as.data.frame() |>
    rename(Contrast = contrast,
           Estimate = estimate,
           t = t.ratio,
           p = p.value) |>
    write_csv("tables/urchin_size_trends_contrast.csv")
