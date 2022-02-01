#--- load libraries and get environment setup
setwd(here::here()) #understand premise of 'here' but can't get it to 'find' appropriate folder.
library(tidyverse)
library(lubridate)
library(glue)



dmr <- read_csv("raw_data/DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv") 
    # filter(depth == 5) %>% #filter later
    # no substrate filter



dmr_filtered <- dmr %>%
    filter(exposure.code > 2) %>%
    filter(coastal.code > 2) %>%
    mutate(date = mdy(date),
           month = month(date), 
           day = day(date),
           survey = "dmr") %>%
    filter(depth == 5) %>%
    mutate(site = as.character(site.number)) 



# ---- Regional # of samples
dmr_filtered %>% 
    group_by(year, site.number, region) %>%
    slice(1L) %>%
    group_by(year, region) %>%
    tally() %>%
    ggplot(aes(x = year, y = n, color = region)) +
    geom_line() + geom_point() +
    theme_bw(base_size = 12) +
    scale_color_brewer(type = "div") +
    theme(legend.position = "bottom") +
    labs(x="", y = "number of sites", title = "DMR survey intensity", color = "")

ggsave("figures/dmr_survey_sample_size.jpg", dpi = 600)


# ----- Composition

rasher_2004 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_allsites_alldepths_2004.csv",
                        na = c("nr", "", "NA"))


rasher_2016 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2016.csv",
                        na = c("nr", "", "NA")) 


rasher_2017 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2017.csv",
                        na = c("nr", "", "NA")) 

rasher_2018 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv",
                        na = c("nr", "", "NA")) 

n_comp <- . %>%
    group_by(region, year, site) %>%
    slice(1L) %>%
    group_by(region) %>%
    tally()

n_sites <- . %>%
    n_comp %>% pull(n) %>% sum

#where was sampled and how much?
rasher_2004 %>% n_comp
rasher_2016 %>% n_comp
rasher_2017 %>% n_comp
rasher_2018 %>% n_comp

#total sample sizes
rasher_2004 %>% n_sites
rasher_2016 %>% n_sites
rasher_2017 %>% n_sites
rasher_2018 %>% n_sites


## How many species did we get in composition?

comp_data <- read_csv("derived_data/compositional_change_data.csv") %>%
    mutate( region = factor(region,
                            levels = rev(c("Downeast", "MDI", "Penobscot Bay",
                                           "Midcoast", "Casco Bay", "York"))),
            year = factor(year, levels = c(2018, 2004)))


comp_data %>%
    pull(sp_code) %>% 
    unique() %>% length


# missing data checks
source("scripts/2_load_combined_data.R")

