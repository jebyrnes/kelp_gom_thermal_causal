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
                                       "Midcoast", "Casco Bay", "York")))) %>%
    mutate(id = as.numeric(as.factor(sp_code)))

comp_data_wide <- comp_data %>%
    select(-type, -id) %>%
    pivot_wider(names_from = "sp_code",
                values_from = "cover") %>%
    mutate(year = as.character(year))

Y_kelp <- comp_data_wide %>%
    select(c(agar, alar, ldig, sac)) %>%
    mutate(across(everything(), ~car::logit(.x/100)))
    
    
Y_understory <- comp_data_wide %>%
        select("sder", "desm", "desm", "ulva",
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


anova_gllvm_uni(mod_gllvm, mod_gllvm_year, mod_gllvm_region, mod_gllvm_noint)  #%>%
    write_csv("tables/kelp_comp_gllvm_lrchisq.csv")

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
#mod_gllvm_0 <- update(mod_gllvm, formulate = .~.+0)

dats <- summary(mod_gllvm)$Coef.tableX %>%
    as.data.frame() %>%
    mutate(coef = rownames(.)) %>%
    as_tibble() %>%
    mutate(species = str_remove(coef, "^.*:"),
           term = str_remove(coef, paste0(":", species)))


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

# anova(mod_gllvm_understory, mod_gllvm_noint_understory, 
#       which = "uni",
#       method = "fdr")

####
library(DirichletReg)
library(lmtest)
library(ggtern)

comp_data_wide <- comp_data_wide %>%
    mutate(kelp_dir = DR_data(cbind(agar, alar, ldig, sac)/100), base = 1)

##### ANOVA

analysis <- comp_data %>%
    group_by(sp_code) %>%
    mutate(logit_cover = car::logit(cover/100)) %>%
    nest() %>%
    mutate(mod = purrr::map(data, ~lm(logit_cover ~ year*region, data = .x)),
           atab = purrr::map(mod, ~broom::tidy(car::Anova(.x)))) %>%
    unnest(atab) %>%
    filter(term != "Residuals")


ggplot(analysis,
       aes(x = sp_code, y = p.value, color = p.value <=0.05)) +
    geom_point() +
    geom_hline(yintercept = 0.05, lty=2) +
    facet_wrap(vars(term)) +
    scale_y_log10(breaks = c(1e-09, 1e-04, 1))+
    coord_flip() +
    theme_bw()

