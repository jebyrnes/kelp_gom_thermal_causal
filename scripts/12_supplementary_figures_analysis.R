library(car)
library(dplyr)
library(ggplot2)
library(betareg)
library(wesanderson)
pal <- wes_palette("Zissou1", 6, type = "continuous")

# read in the data
# make a logit kelp cover variable
# filter to 10m
combined_bio_temp_gmc <- read.csv("derived_data/combined_data_for_analysis.csv") %>%
    as_tibble() %>%
    filter(depth == 10) %>%
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


## Timeseries of Kelp at 10m
#Fit a model

mod_time_only_beta <- betareg(kelp_porp ~ year*region, 
                              data =  combined_bio_temp_gmc)



mod_time_only_beta_noint <- betareg(kelp_porp ~ year+region, 
                              data =  combined_bio_temp_gmc)

library(modelr)
kelp_time_pred <- data_grid(combined_bio_temp_gmc,
                            year = 2001:2018,
                            region = levels(combined_bio_temp_gmc$region) %>% 
                                as.factor) %>%
    mutate(kelp_perc_pred = 100*predict(mod_time_only_beta_noint,
                                        newdata = .,
                                        type = "response"),
           kelp_perc_pred_var = 100*predict(mod_time_only_beta_noint,
                                            newdata = .,
                                            type = "variance")
    )


ggplot(combined_bio_temp_gmc,
       aes(x = year, y = 100*kelp_porp, color = region)) +
    geom_point(alpha = 0.4) +
    facet_wrap(vars(region)) +
    geom_line(data = kelp_time_pred, 
              mapping = aes(y = kelp_perc_pred),
              color = "black",
              size = 1.5) +
    geom_ribbon(data = kelp_time_pred, 
                mapping = aes(y = kelp_perc_pred,
                              ymin = kelp_perc_pred-2*sqrt(kelp_perc_pred_var),
                              ymax = kelp_perc_pred+2*sqrt(kelp_perc_pred_var)),
                color = "lightgrey",
                alpha = 0.5)+
    theme_bw(base_size = 16) +
    labs(x = "", y = "Kelp % Cover", color = "") +
#    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none")

ggsave("figures/kelp_pred_over_time_10m.jpg", dpi = 600)
saveRDS(mod_time_only_beta, "model_output/kelp_timeseries_mod_10m.rds")

