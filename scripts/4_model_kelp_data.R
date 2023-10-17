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
library(here) # paths to data should 'just work' (though having problems with it)
library(readr)

setwd(here::here())
source("scripts/2_load_combined_data.R")

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

                            #causal controls for hierarchical sampling
                            mean_regional_urchin +
                            mean_mean_temp_spring +
                            mean_mean_temp_summer +
                            
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
performance::check_autocorrelation(mod_urchin_add)

# Evaluate Model
Anova(mod_urchin_add)

summary(mod_urchin_add)

#r2
performance::r2_nakagawa(mod_urchin_add)
#cor(fitted(mod_urchin_add), combined_bio_temp_gmc$kelp)

#table output

mod_urchin_add %>%
  broom.mixed::tidy() %>%
  filter(grepl("spring", term)) %>%
  select(-effect, -component, -group) %>%
  knitr::kable(digits = 3) %>% kableExtra::kable_minimal()

# Compare to a model where last summer interacts with current temp
# To see if we can drive over a cliff - but, we do not have warm places with
# lots of urchins

mod_urchin_int <- glmmTMB(kelp_porp ~ 
                           #drivers
                           urchin_anom_from_region * #chomp chomp
                           (mean_temp_spring_dev + #current temp = growth
                           lag_mean_temp_summer_dev) + # loss last year
                           
                           #causal controls for hierarchical sampling
                           mean_regional_urchin +
                           mean_mean_temp_spring +
                           mean_mean_temp_summer +
                           
                           #REs
                           (1|year) + (1|region),
                         family = beta_family("logit"),
                         data = combined_bio_temp_gmc)

Anova(mod_urchin_int)

# compare additive and interaction model
AIC(mod_urchin_add, mod_urchin_int)

# Work with OISST data


mod_urchin_add_oisst <- glmmTMB(kelp_porp ~ 
                                    #drivers
                                    urchin_anom_from_region + #chomp chomp
                                    mean_spring_temp_oisst_dev + #current temp = growth
                                    lag_mean_summer_temp_oisst_dev + # loss last year
                                    
                                    #causal controls for hierarchical sampling
                                    mean_regional_urchin +
                                    mean_spring_temp_oisst_site +
                                    mean_summer_temp_oisst_site +
                                    
                                    #REs
                                    (1|year) + (1|region),
                                family = beta_family("logit"),
                                data = combined_bio_temp_gmc)

Anova(mod_urchin_add_oisst)
summary(mod_urchin_add_oisst)

performance::check_collinearity(mod_urchin_add_oisst)


# save the fit models out
saveRDS(mod_urchin_add, "model_output/mod_urchin_add.RDS")
saveRDS(mod_urchin_add_oisst, "model_output/mod_urchin_add_oisst.RDS")
saveRDS(mod_urchin_int, "model_output/mod_urchin_int.RDS")


# Test of causal robustness using OISST data with year as a FE

combined_bio_temp_gmc$year_c <- as.character(combined_bio_temp_gmc$year)

mod_urchin_add_oisst_year <- glmmTMB(kelp_porp ~ 
                                         #drivers
                                         urchin_anom_from_region + #chomp chomp
                                         mean_spring_temp_oisst_dev + #current temp = growth
                                         lag_mean_summer_temp_oisst_dev + # loss last year
                                         
                                         #causal controls for hierarchical sampling
                                         mean_regional_urchin +
                                         mean_spring_temp_oisst_site +
                                         mean_summer_temp_oisst_site +
                                         
                                         #REs
                                         year_c + (1|region),
                                     family = beta_family("logit"),
                                     data = combined_bio_temp_gmc)

Anova(mod_urchin_add_oisst)
Anova(mod_urchin_add_oisst_year)
