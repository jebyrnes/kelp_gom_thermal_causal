
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
    summarize(degree_heat_days_summer = degree_heat_days_summer[1]) %>%
    group_by(region) %>%
    arrange(year) %>%
    mutate(lag_degree_heat_days_summer = lag(degree_heat_days_summer)) %>%
    ungroup() %>%
    select(-degree_heat_days_summer)

combined_bio_temp_gmc <- combined_bio_temp_gmc %>%
    left_join(lag_var) %>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))



regional_values <- combined_bio_temp_gmc %>%
    dplyr::group_by(region) %>%
    dplyr::select(mean_regional_urchin, mean_mean_temp_spring, mean_mean_temp_summer) %>%
    dplyr::slice(1L) %>%
    ungroup() 