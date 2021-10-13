##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Model Generation                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Running a model to test if kelp declines have happened over time      ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz   - May 27, 2020                                         ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Worked On: May 7th, 2021 (emerging from COVID like a cicada)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Worked On: Sept 29th, 2021 (major rewrite)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

library(car) #for Anova
library(ggplot2)
library(dplyr)
library(DHARMa) # Residual diagnostics for hierarchical regression models
library(here) # paths to data should 'just work' (though having problems with it)
library(readr)
library(modelr)
library(betareg)

setwd(here::here())

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
                             mean_temp_summer_dev)) %>%
    mutate(region = gsub("\\.", " ", region),
        region = stringr::str_to_title(region),
        region = gsub("Mdi", "MDI", region)) %>%
    mutate(region = factor(region, 
                           levels = c("York", "Casco Bay",
                                      "Midcoast", "Penobscot Bay",
                                      "MDI", "Downeast")))

# Does month of sampling bias kelp measurements?
# Does spring or summer sampling matter?

ggplot(data = combined_bio_temp_gmc,
       aes(y = logit_kelp,
           x = year, color = factor(month))) +
    geom_jitter(alpha = 0.5) +
    facet_wrap(vars(region))


### Bob's objections to Krumhansl
mod_time_only <- lm(logit_kelp ~ year*region, data =  combined_bio_temp_gmc)
mod_time_only_linear <- lm(kelp ~ year*region, data =  combined_bio_temp_gmc)

mod_time_only_beta <- betareg(kelp_porp ~ year*region, 
                              data =  combined_bio_temp_gmc)
Anova(mod_time_only)
summary(mod_time_only)

ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region)) +
    geom_point(alpha = 0.4) +
    facet_wrap(vars(region)) +
    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Logit Kelp % Cover", color = "")



ggplot(combined_bio_temp_gmc,
       aes(x = year, y = mean_temp_spring, color = region)) +
    geom_point(alpha = 0.4) +
    facet_wrap(vars(region)) +
    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Average Spring\nTemperature C", color = "")

ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region)) +
    #  geom_point() +
    stat_summary() +
    facet_wrap(vars(region)) +
    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    ylim(c(-5,5))

library(emmeans)
slopes <- emtrends(mod_time_only,
                   ~region,
                   var = "year")


slopes_beta <- emtrends(mod_time_only_beta,
                        ~region,
                        var = "year")

#check clustered residuals
library(broom)
aug_time <- augment(mod_time_only, data = combined_bio_temp_gmc)
ggplot(data = aug_time, aes(x = .resid)) +
    geom_histogram() + 
    facet_wrap(~region)

#urchin
ggplot(combined_bio_temp_gmc,
       aes(x = year, y = urchin, color = region,
           group = paste(region, site))) +
    geom_point(alpha = 0.6) +
    facet_wrap(vars(region)) +
    theme_bw() +
    labs(y = "Urchins per sq. m.", x = "", color = "") +
    theme_bw(base_size = 18) 
    


#kelp
ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region,
           group = paste(region, site))) +
    geom_line(alpha = 0.6) +
    facet_wrap(vars(region)) +
    ylim(c(-5,5)) +
    theme_bw()

mod_time <- lmer(logit_kelp ~ year*region + (1 + year|site:region), 
                 data =  combined_bio_temp_gmc)
