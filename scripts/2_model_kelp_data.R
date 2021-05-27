##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Model Generation                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Running a model to test if temp and/or urchin influences kelp cover       ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz   - May 27, 2020                                         ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Last Worked On: May 7th, 2021 (emerging from COVID like a cicada)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

library(car) #for Anova
library(ggplot2)
library(dplyr)
library(glmmTMB)
library(DHARMa) # Residual diagnostics for hierarchal regression models
library(bbmle) #for AICtab
library(here) # paths to data should 'just work' (though having problems with it)

setwd(here::here())

DF.join <- read_csv("derived_data/combined_data_for_analysis.csv")


### Additive Model

mod_urchin_add <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev +
                              mean_temp_dev + mean_temp_mn +
                              (1|year) + (1|region),
                          family = beta_family(),
                          data = DF.join)

# Evaluate Assumptions

simulateResiduals(mod_urchin_add) %>% plotQQunif()
simulateResiduals(mod_urchin_add) %>% testDispersion()
simulateResiduals(mod_urchin_add) %>% plotResiduals()

# Evaluate Model
Anova(mod_urchin_add)

summary(mod_urchin_add)

#r2
with(DF.join %>% add_predictions(mod_urchin_add, type = "response"),
    cor(cbind(kelp.perc, pred), use = "pairwise.complete"))

performance::r2_nakagawa(mod_urchin_add)

#### Model with an Interaction

mod_urchin_int <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev * 
                              mean_temp_dev + mean_temp_mn +
                              (1|year) + (1|region),
                          family = beta_family(),
                          data = DF.join)
# Evaluate Assumptions

simulateResiduals(mod_urchin_int) %>% plotQQunif()

# Evaluate Model
Anova(mod_urchin_int)


#### Compare models with AIC
AICtab(mod_urchin_add, mod_urchin_int)

### The two are the same. Parsimony suggests go with the simpler model

saveRDS(mod_urchin_add, "derived_data/mod_urchin_add.RDS")
