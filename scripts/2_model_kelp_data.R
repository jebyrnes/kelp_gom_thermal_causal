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

setwd(here::here())

# read in the data
# make a logit kelp cover variable
# filter to 5m
combined_bio_temp_gmc <- read.csv("derived_data/combined_data_for_analysis.csv") %>%
    as_tibble() %>%
    filter(depth == 5) %>%
    mutate(logit_kelp = logit(kelp, adjust = 0.01),
           kelp = ifelse(kelp== 0, 0.01, kelp),
           kelp = ifelse(kelp == 100, 99, kelp)) %>%
    # if sampling = spring, use spring, if sampling = summer, use summer
    mutate(temp_dev = ifelse(month <7,
                             mean_temp_spring_dev,
                             mean_temp_summer_dev))
  
### Bob's objections to Krumhansl
mod_time_only <- lm(logit_kelp ~ year*region, data =  combined_bio_temp_gmc)
Anova(mod_time_only)
summary(mod_time_only)

ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region)) +
    geom_point() +
    facet_wrap(vars(region)) +
    stat_smooth(method = "lm", formula = y ~ x) +
    ylim(c(-5,5))

#urchin
ggplot(combined_bio_temp_gmc,
       aes(x = year, y = urchin, color = region,
           group = paste(region, site))) +
    geom_line(alpha = 0.6) +
    facet_wrap(vars(region), scale = "free_y") +
    theme_bw()


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

### Additive Model

mod_urchin_add <- glmmTMB(kelp/100 ~ mean_regional_urchin + 
                              urchin_anom_from_region +
                              mean_temp_summer_dev +
                              mean_mean_temp_summer + 
                              (1|year) + (1|region),
                          family = beta_family("logit"),
                          data = combined_bio_temp_gmc)

# Evaluate Assumptions

simulateResiduals(mod_urchin_add) %>% plotQQunif()
simulateResiduals(mod_urchin_add) %>% testDispersion()
simulateResiduals(mod_urchin_add) %>% plotResiduals()

performance::check_model(mod_urchin_add)

# Evaluate Model
Anova(mod_urchin_add)

summary(mod_urchin_add)

#r2
with(DF.join %>% add_predictions(mod_urchin_add, type = "response"),
    cor(cbind(kelp.perc, pred), use = "pairwise.complete"))

performance::r2_nakagawa(mod_urchin_add)

### Compare link functions

mod_urchin_add_logit <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev +
                              mean_temp_dev + mean_temp_mn +
                              (1|year) + (1|region),
                          family = beta_family("logit"),
                          data = DF.join)

mod_urchin_add_probit <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev +
                                    mean_temp_dev + mean_temp_mn +
                                    (1|year) + (1|region),
                                family = beta_family("probit"),
                                data = DF.join)
# compare - note, cloglog
AICtab(mod_urchin_add, mod_urchin_add_logit, mod_urchin_add_probit)


#### Model with an Interaction

mod_urchin_int <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev * 
                              mean_temp_dev + mean_temp_mn +
                              (1|year) + (1|region),
                          family = beta_family("cloglog"),
                          data = DF.join)
# Evaluate Assumptions

simulateResiduals(mod_urchin_int) %>% plotQQunif()

# Evaluate Model
Anova(mod_urchin_int)


#### Compare models with AIC
AICtab(mod_urchin_add, mod_urchin_int)

### The two are the same. Parsimony suggests go with the simpler model


### Additive Model

mod_urchin_naieve <- glmmTMB(kelp.perc ~ urchin+
                              mean_temp +
                              (1|year) + (1|region),
                          family = beta_family("cloglog"),
                          data = DF.join)

AICtab(mod_urchin_add, mod_urchin_naieve)

saveRDS(mod_urchin_add, "derived_data/mod_urchin_add.RDS")
saveRDS(mod_urchin_int, "derived_data/mod_urchin_int.RDS")
saveRDS(mod_urchin_add_logit, "derived_data/mod_urchin_add_logit.Rds")
saveRDS(mod_urchin_add_probit, "derived_data/mod_urchin_add_probit.Rds")
saveRDS(mod_urchin_naieve, "derived_data/mod_urchin_naieve.Rds")
