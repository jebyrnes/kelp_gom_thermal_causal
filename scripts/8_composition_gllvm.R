#' -------------------------------------------
#' Analyze compositional data
#' 
#' -------------------------------------------

#--- load libraries and get environment setup
setwd(here::here()) 
library(tidyverse)
library(gllvm)
source("scripts/anova_gllvm_tmp.R")

comp_data <- read_csv("derived_data/compositional_change_data.csv") %>%
    mutate( region = factor(region,
                            levels = rev(c("Downeast", "MDI", "Penobscot Bay",
                                           "Midcoast", "Casco Bay", "York"))),
            year = factor(year, levels = c(2018, 2004)))

comp_data_wide <- comp_data %>%
    select(-type) %>%
    pivot_wider(names_from = "sp_code",
                values_from = "cover") %>%
    mutate(year = as.character(year))

Y_kelp <- comp_data_wide %>%
    select(c(agar, alar, ldig, sac)) %>%
    mutate(across(everything(), ~car::logit(.x/100)))
    
    
Y_understory <- comp_data_wide %>%
        select("desm", "desm", "ulva",
               "chaet", "codm", "poly", "poly", "rhod", "ptilo",
               "porph", "palm", "phyc", "ccrisp", "coral") %>%
    mutate(across(everything(), ~car::logit(.x/100)))
    
X_design <- comp_data_wide %>% select(year, region)

mod_gllvm <- gllvm(y = Y_kelp, X = X_design,
                   formula = ~year*region,
                   family = gaussian(link = "identity"),
                   num.lv=2,
                   starting.val = 'zero')

mod_gllvm_noint <- gllvm(y = Y_kelp, X = X_design,
                   formula = ~year+region,
                   family = gaussian(link = "identity"),
                   num.lv=2,
                   starting.val = 'zero')

mod_gllvm_year <- gllvm(y = Y_kelp, X = X_design,
                         formula = ~year,
                         family = gaussian(link = "identity"),
                         num.lv=2,
                         starting.val = 'zero')

mod_gllvm_region <- gllvm(y = Y_kelp, X = X_design,
                         formula = ~region,
                         family = gaussian(link = "identity"),
                         num.lv=2,
                         starting.val = 'zero')


# must be whole, year, region, no interaction
anova_gllvm(mod_gllvm, mod_gllvm_year, mod_gllvm_region, mod_gllvm_noint)  %>%
    write_csv("tables/kelp_gllvm_lrchisq.csv")


anova_gllvm_uni(mod_gllvm, mod_gllvm_year, mod_gllvm_region, mod_gllvm_noint) %>%
    write_csv("tables/kelp_comp_gllvm_lrchisq.csv")

saveRDS(mod_gllvm, "model_output/kelp_composition_gllvm.rds")

# 
# #for individual species
# anova(mod_gllvm, mod_gllvm_noint, 
#       which = "uni",
#       method = "fdr") %>%
#     `$`("data") %>%
#     as_tibble() %>%
#     select(-`Model. 1`) %>%
#     set_names(c("col", "species", "value")) %>%
#     pivot_wider(names_from = col,
#                 values_from = value)

#### Coefficient plot

#refit with 0 to test are things diff from 0, not York
mod_gllvm_0 <- gllvm(y = Y_kelp, X = X_design,
                    formula = ~region*year+0,
                    family = gaussian(link = "identity"),
                    num.lv=2,
                    starting.val = 'zero')

dats <- summary(mod_gllvm_0)$Coef.tableX %>%
    as.data.frame() %>%
    mutate(coef = rownames(.)) %>%
    as_tibble() %>%
    mutate(species = str_remove(coef, "^.*:"),
           term = str_remove(coef, paste0(":", species)))

# What we want is change within a region for each species
# increase or decrease?
library(modelr)
X_unique <- X_design %>%
    group_by(year, region) %>%
    slice(1L) %>%
    ungroup()

pred_kelp <- predict(mod_gllvm, newX = X_unique,
                     level = 0) %>%
    cbind(X_unique) 

diff_kelp <- pred_kelp %>%
    pivot_longer(agar:sac,
                 names_to = "sp_code",
                 values_to = "prediction") %>%
    mutate(prediction = boot::inv.logit(prediction)*100) %>%
    pivot_wider(names_from = year, values_from = prediction) %>%
    group_by(region, sp_code) %>%
    summarize(change = `2018` - `2004`)

ggplot(diff_kelp  ,
       aes(x = region, y = change)) +
    geom_point( position = position_dodge(width = 1)) +
    coord_flip() +
    facet_wrap(vars(sp_code)) +
    labs(y = "Change in % Cover",
         x = "") +
    ggthemes::theme_clean() +
    geom_hline(yintercept = 0, lty = 1)

ggsave("figures/kelp_change_model_2004_2018.jpg", dpi = 600)

###
ggplot(dats,
       aes(x = species, y = Estimate, color = `Pr(>|z|)`<0.05,
           ymin = Estimate - 2*`Std. Error`,
       ymax = Estimate + 2*`Std. Error`)) +
    geom_point() +
    geom_linerange() +
    facet_wrap(vars(term)) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    labs(subtitle = "Coefs relative to 2014 in York")

##### What do those latent variables look like...
ordiplot(mod_gllvm, biplot = TRUE, 
         symbols = TRUE)


#### Understory

mod_gllvm_understory <- gllvm(y = Y_understory, X = X_design,
                   formula = ~year*region,
                   family = gaussian(link = "identity"),
                   num.lv=2,
                   starting.val = 'zero')

mod_gllvm_noint_understory <- gllvm(y = Y_understory, X = X_design,
                         formula = ~year+region,
                         family = gaussian(link = "identity"),
                         num.lv=2,
                         starting.val = 'zero')

mod_gllvm_year_understory <- gllvm(y = Y_understory, X = X_design,
                        formula = ~year,
                        family = gaussian(link = "identity"),
                        num.lv=2,
                        starting.val = 'zero')

mod_gllvm_region_understory <- gllvm(y = Y_understory, X = X_design,
                          formula = ~region,
                          family = gaussian(link = "identity"),
                          num.lv=2,
                          starting.val = 'zero')


# must be whole, year, region, no interaction
anova_gllvm(mod_gllvm_understory, 
            mod_gllvm_year_understory, 
            mod_gllvm_region_understory, 
            mod_gllvm_noint_understory) %>%
    write_csv("tables/understory_gllvm_lrchisq.csv")

#for individual species
anova_gllvm_uni(mod_gllvm_understory, 
                mod_gllvm_year_understory, 
                mod_gllvm_region_understory, 
                mod_gllvm_noint_understory,
                method = "fdr") %>%
    write_csv("tables/understory_comp_gllvm_lrchisq.csv")

saveRDS(mod_gllvm_understory, "model_output/understory_composition_gllvm.rds")

# anova(mod_gllvm_understory, mod_gllvm_noint_understory, 
#       which = "uni",
#       method = "fdr")

