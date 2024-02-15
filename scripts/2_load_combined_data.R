#' ---------------------------------------------------------
#' Script to load combined data (temperature and community)
#' and modify it slightly to create derived parameters and
#' better region names for plots, etc.
#' @date 2023-10-30 last update
#' @author Jarrett Byrnes
#'-----------------------------------------------------------

library(car)
library(dplyr)

# read in the data
# make a logit kelp cover variable
# filter to 5m
combined_bio_temp_gmc <- read.csv("derived_data/combined_data_for_analysis.csv") %>%
    as_tibble() %>%
    filter(depth == 5) %>%
    mutate(logit_kelp = logit(kelp, adjust = 0.01),
           kelp = ifelse(kelp== 0, 0.01, kelp),
           kelp = ifelse(kelp == 100, 99, kelp),
           kelp_porp = kelp/100) %>%
    # if sampling = spring, use spring, if sampling = summer, use summer
    mutate(temp_dev = ifelse(month <7,
                             mean_temp_spring_dev,
                             mean_temp_summer_dev))
#add some lags
lag_var <- combined_bio_temp_gmc %>%
    group_by(region, year) %>%
    summarize(degree_heat_days_15_summer = degree_heat_days_15_summer[1],
              degree_heat_days_20_summer = degree_heat_days_20_summer[1]
              ) %>%
    group_by(region) %>%
    arrange(year) %>%
    mutate(lag_degree_heat_days_15_summer = lag(degree_heat_days_15_summer),
           lag_degree_heat_days_20_summer = lag(degree_heat_days_20_summer)
           ) %>%
    ungroup() %>%
    select(-degree_heat_days_15_summer, -degree_heat_days_20_summer)

combined_bio_temp_gmc <- combined_bio_temp_gmc %>%
    left_join(lag_var)


#get some oisst data in here
oisst_dat <- read.csv("derived_data/oisst_temp_data.csv")

#add oisst data
combined_bio_temp_gmc <- left_join(combined_bio_temp_gmc, oisst_dat) 


#add good region names
combined_bio_temp_gmc <- combined_bio_temp_gmc %>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))



#get some regional values for use
regional_values <- combined_bio_temp_gmc %>%
    dplyr::group_by(region) %>%
    dplyr::select(mean_regional_urchin, mean_mean_temp_spring, mean_mean_temp_summer) %>%
    dplyr::slice(1L) %>%
    ungroup() 