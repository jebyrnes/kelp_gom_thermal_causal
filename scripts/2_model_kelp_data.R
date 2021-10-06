##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Model Generation                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Running a model to test if temp and/or urchin influences kelp cover       ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz   - May 27, 2020                                         ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Worked On: May 7th, 2021 (emerging from COVID like a cicada)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Worked On: Sept 29th, 2021 (major rewrite)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

library(car) #for Anova
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(DHARMa) # Residual diagnostics for hierarchical regression models
library(bbmle) #for AICtab
library(here) # paths to data should 'just work' (though having problems with it)
library(readr)
library(modelr)
library(lme4)
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

### Additive Model
# Our hypothesis is that kelp forests in the GOM are changing
# in the wake of the urchin decline, but, that those changes
# are likely to be region specific. We are interested in asking
# the question of to what degree are regional trends predicted
# by known drivers - temperature stress and urchin abundance.
# It really is about STRESS that is experienced in a given region.
# BUT - if we have declines in places that haven't experienced stress,
# what is that about? Or is our perception of stress wrong? OR - is there
# a lagged effect? 
# % cover in may is net damage from previous year offset by winter growth
# We cannot know the mechanism, but, both process operate
# kelp = loss + growth(or further loss)


mod_urchin_add <- glmmTMB(kelp_porp ~ 
                            #drivers
                            urchin_anom_from_region + #chomp chomp
                            mean_temp_spring_dev + #current temp = growth
                            lag_mean_temp_summer_dev + # loss last year
                            #lag_degree_heat_days_summer + # loss last year
                            
                            #causal controls for hierarchical sampling
                            mean_regional_urchin +
                            mean_mean_temp_spring +

                            #REs
                            (1|year) + (1|region),
                          family = beta_family("logit"),
                          data = combined_bio_temp_gmc)

# Evaluate Assumptions

simulateResiduals(mod_urchin_add) %>% plotQQunif()
simulateResiduals(mod_urchin_add) %>% testDispersion()
simulateResiduals(mod_urchin_add) %>% plotResiduals()

performance::check_model(mod_urchin_add)
performance::check_collinearity(mod_urchin_add)

# Evaluate Model
Anova(mod_urchin_add)

summary(mod_urchin_add)

#r2
with(DF.join %>% add_predictions(mod_urchin_add, type = "response"),
    cor(cbind(kelp.perc, pred), use = "pairwise.complete"))

performance::r2_nakagawa(mod_urchin_add)


# Compare to a lag stress model

mod_urchin_lag_dhd <- glmmTMB(kelp_porp ~ 
                            #drivers
                            urchin_anom_from_region + #chomp chomp
                            mean_temp_spring_dev + #current temp = growth
                            lag_degree_heat_days_summer + # loss last year
                            
                            #causal controls for hierarchical sampling
                            mean_regional_urchin +
                            mean_mean_temp_spring +
                            
                            #REs
                            (1|year) + (1|region),
                          family = beta_family("logit"),
                          data = combined_bio_temp_gmc)


AIC(mod_urchin_add, mod_urchin_lag_dhd)

# Compare to a model where last summer interacts with current temp
# To see if we can drive over a cliff

mod_urchin_int <- glmmTMB(kelp_porp ~ 
                            #drivers
                            urchin_anom_from_region + #chomp chomp
                            mean_temp_spring_dev * #current temp = growth
                            lag_mean_temp_summer_dev + # loss last year

                            #causal controls for hierarchical sampling
                            mean_regional_urchin +
                            mean_mean_temp_spring +
                            
                            #REs
                            (1|year) + (1|region),
                          family = beta_family("logit"),
                          data = combined_bio_temp_gmc)


# saveRDS(mod_urchin_add, "derived_data/mod_urchin_add.RDS")
# saveRDS(mod_urchin_int, "derived_data/mod_urchin_int.RDS")
# saveRDS(mod_urchin_add_logit, "derived_data/mod_urchin_add_logit.Rds")
# saveRDS(mod_urchin_add_probit, "derived_data/mod_urchin_add_probit.Rds")
# saveRDS(mod_urchin_naieve, "derived_data/mod_urchin_naieve.Rds")
