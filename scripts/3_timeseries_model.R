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
library(here) # paths to data should 'just work' (though having problems with it)
library(readr)
library(betareg)
library(glmmTMB)
library(broom)
library(broom.mixed)

library(wesanderson)
pal <- wes_palette("Zissou1", 6, type = "continuous")

setwd(here::here())

# read in the data
# make a logit kelp cover variable
# filter to 5m
source("scripts/2_load_combined_data.R")

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
saveRDS(mod_time_only_beta, "model_output/kelp_timeseries_mod.rds")

Anova(mod_time_only)
summary(mod_time_only)


ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region)) +
    geom_point(alpha = 0.8) +
    facet_wrap(vars(region), ncol = 2) +
    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Logit Kelp % Cover", color = "") +
   # scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none")

ggsave("figures/kelp_over_time.jpg", dpi = 600)

# compare slopes
library(emmeans)
timeseries_trend <- emtrends(mod_time_only_beta,
                             ~region,
                             var = "year")
slope_cont <- contrast(timeseries_trend, "pairwise")

plot(slope_cont) +
    geom_vline(xintercept = 0, lty=2)


###
library(modelr)
kelp_time_pred <- data_grid(combined_bio_temp_gmc,
                            year = 2001:2018,
                            region = levels(combined_bio_temp_gmc$region) %>% 
                                as.factor) %>%
    mutate(kelp_perc_pred = 100*predict(mod_time_only_beta,
                                        newdata = .,
                                        type = "response"),
           kelp_perc_pred_var = 100*predict(mod_time_only_beta,
                                            newdata = .,
                                            type = "variance")
    )


ggplot(combined_bio_temp_gmc,
       aes(x = year, y = 100*kelp_porp, color = region)) +
    geom_point(alpha = 0.8) +
    facet_wrap(vars(region), ncol = 2) +
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

ggsave("figures/kelp_pred_over_time.jpg", dpi = 600)

###
temp_timeseries <- read_csv("derived_data/temp_timeseries.csv") %>%
    filter(year >= 2001)%>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))

spring_temp <- ggplot(temp_timeseries,
       aes(x = year, y = mean_temp_spring, color = region)) +
    geom_point(alpha = 1, size = 2) +
  #  facet_wrap(vars(region), ncol = 2) +
    #    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
        stat_smooth(method = "lm", formula = y ~ x,fill = NA) +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Average Spring\nTemperature C", color = "") +
#    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none")
spring_temp

ggsave("figures/spring_temp_trends.jpg", dpi = 600)



summer_temp <- ggplot(temp_timeseries,
       aes(x = year, y = mean_temp_summer, color = region)) +
    geom_point(alpha = 1, size = 2) +
   # facet_wrap(vars(region), ncol = 2) +
    stat_smooth(method = "lm", formula = y ~ x, fill = NA) +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Average Summer\nTemperature C", color = "") +
#    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none")
summer_temp
ggsave("figures/summer_temp_trends.jpg", dpi = 600)

library(patchwork)
summer_temp + spring_temp
ggsave("figures/temp_both_trends.jpg", dpi = 600, width = 10, height = 5)


# temperature models ####
temp_dat <- temp_timeseries %>%
    group_by(region, year) %>%
    slice(1L) %>%
    ungroup() %>%
    select(region, year, mean_temp_spring, mean_temp_summer)


temp_mods <- temp_dat %>%
    group_by(region) %>%
    nest() %>%
    summarize(spring_mod = 
                  map(data, ~ lm(mean_temp_spring ~ year, data = .)),
              summer_mod = 
                  map(data, ~ lm(mean_temp_summer ~ year, data = .)),
              spring_slope = map_dbl(spring_mod, ~coef(.)[2]),
              summer_slope = map_dbl(summer_mod, ~coef(.)[2]))

temp_mods %>%
    select(-spring_mod, -summer_mod)

lm(mean_temp_summer~ year*region, data = temp_timeseries) %>%
    saveRDS("model_output/summer_temp_timeseries.rds")
lm(mean_temp_spring~ year*region, data = temp_timeseries) %>%
    saveRDS("model_output/spring_temp_timeseries.rds")

#whole shebang
lm(mean_temp_spring ~ year + region, data = temp_dat) %>% coef %>% `[`(2)
lm(mean_temp_summer ~ year + region, data = temp_dat) %>% coef %>% `[`(2)

ggplot(combined_bio_temp_gmc,
       aes(x = year, y = urchin, color = region)) +
    geom_point(alpha = 0.8) +
    facet_wrap(vars(region), ncol = 2) +
    #stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    #ylim(c(0,100))  +
    theme_bw(base_size = 16) +
    labs(x = "", y = "Urchins per sq. m", color = "") +
#    scale_color_brewer(type = "div") +
    scale_color_manual(values = pal) +
    theme(legend.position = "none") +
    geom_hline(yintercept = 30, lty = 2)# +
    # geom_text(x = 2001, y = 49, label = "Urchin barren\nconditions",
    #            color = "black", hjust = 0, alpha = 0.5)

ggsave("figures/urchin_trends.jpg", dpi = 600)


ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region)) +
    #  geom_point() +
    stat_summary() +
    facet_wrap(vars(region), ncol = 2) +
    stat_smooth(method = "lm", formula = y ~ x, color = "black") +
    scale_color_manual(values = pal) +
    ylim(c(-5,5)) +
    theme_bw()

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

# urchin timeseries ####
library(glmmTMB)
combined_bio_temp_gmc$cent_year <- combined_bio_temp_gmc$year - 2010
urchin_mod <- glmmTMB(urchin ~ cent_year*region, 
                      zi = ~ cent_year*region,
                      data = combined_bio_temp_gmc,
                      family =ziGamma(link = "log"))
saveRDS(urchin_mod, "model_output/urchin_timeseries_mod.rds")


#residuals
u_res <- DHARMa::simulateResiduals(urchin_mod)
DHARMa::plotQQunif(u_res)

#temporal problems?
urchin_out <- augment(urchin_mod,type.predict = "response")
Anova(lm(.resid ~ cent_year*region, data = urchin_out))

ggplot(urchin_out, aes(x = cent_year, color = region, y = .resid)) +
    geom_point() +
    facet_wrap(vars(region), ncol = 2)

ggplot(urchin_out, aes(x = cent_year, color = region, y = .fitted)) +
    geom_point() +
    facet_wrap(vars(region), ncol = 2)


#tests
Anova(urchin_mod)
Anova(urchin_mod, component = c("zi"))

ggplot(combined_bio_temp_gmc,
       aes(x = year, y = urchin, color = region,
           group = paste(region, site))) +
    geom_point(alpha = 0.6) +
    facet_wrap(vars(region), ncol = 2) +
    theme_bw() +
    labs(y = "Urchins per sq. m.", x = "", color = "") +
    theme_bw(base_size = 18) 
    


#kelp
ggplot(combined_bio_temp_gmc,
       aes(x = year, y = logit_kelp, color = region,
           group = paste(region, site))) +
    geom_line(alpha = 0.6) +
    facet_wrap(vars(region), ncol = 2) +
    scale_color_manual(values = pal) +
    ylim(c(-5,5)) +
    theme_bw()

mod_time <- lmer(logit_kelp ~ year*region + (1 + year|site:region), 
                 data =  combined_bio_temp_gmc)
