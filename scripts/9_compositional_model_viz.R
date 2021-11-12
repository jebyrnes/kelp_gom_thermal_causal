#' -------------------------------------------
#' Analyze output of compositional gllvm
#' 
#' -------------------------------------------

#--- load libraries and get environment setup
setwd(here::here()) 
library(tidyverse)
library(gllvm)
library(modelr)
source("scripts/anova_gllvm_tmp.R")
source("scripts/get_gllvm_predictions_sim.R")

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

X_design <- comp_data_wide %>% select(year, region)

# load models
kelp_gllvm <- readRDS("model_output/kelp_composition_gllvm.rds")
understory_gllvm <- readRDS("model_output/understory_composition_gllvm.rds")

# let's do posthocs! ####
X_unique <- X_design %>%
    group_by(year, region) %>%
    slice(1L) %>%
    ungroup()

#get predictions for each model
kelp_pred <- get_sim_fit_gllvm(kelp_gllvm, X_unique, n = 1e5)
understory_pred <- get_sim_fit_gllvm(understory_gllvm, X_unique, n = 1e5)


#posthoc workflow

posthoc_frame <- . %>%
    mutate(value = boot::inv.logit(value)*100) %>%
        group_by(sim, name, region) %>%
        arrange(year) %>%
        summarize(diff_2004_to_2018 = value[2] - value[1]) %>%
        ungroup() %>%
        group_by(region, name) %>%
        summarize(mean_diff_2004_to_2018 = mean(diff_2004_to_2018),
                  lq = quantile(diff_2004_to_2018, prob = 0.025),
                  uq = quantile(diff_2004_to_2018, prob = 0.975)) %>%
    ungroup()


# posthocs!
kelp_change <- kelp_pred %>%
    posthoc_frame

write_csv(kelp_change, "tables/kelp_change_posthoc_2004_2018.csv")

ggplot(kelp_change,
       aes(x = region, y = mean_diff_2004_to_2018, 
           ymin = lq, ymax = uq, color = name)) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    ggthemes::theme_clean() +
    scale_color_brewer(palette = "Set2") +
    labs(color = "Species", 
         x = "", 
         y = "Change in % Cover from 2004 to 2018")
ggsave("figures/kelp_change_posthoc_2004_2018.jpg", dpi = 600)


#understory

understory_change <- understory_pred %>%
    posthoc_frame

write_csv(understory_change, "tables/understory_change_posthoc_2004_2018.csv")


library(randomcoloR)
set.seed(2021)
ggplot(understory_change,
       aes(x = region, y = mean_diff_2004_to_2018, 
           ymin = lq, ymax = uq, color = name)) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    geom_hline(yintercept = 0, lty = 2) +
    coord_flip() +
    ggthemes::theme_clean() +
    scale_color_manual(values = 
                           distinctColorPalette(n_distinct(understory_change$name)))+
    labs(color = "Species", 
         x = "", 
         y = "Change in % Cover from 2004 to 2018")

ggsave("figures/understory_change_posthoc_2004_2018.jpg", dpi = 600)
