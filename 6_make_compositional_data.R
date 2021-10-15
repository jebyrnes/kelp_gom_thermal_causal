#' -------------------------------------------
#' Process compositional data
#' 
#' -------------------------------------------

#--- load libraries and get environment setup
setwd(here::here()) 
library(tidyverse)
library(lubridate)
library(glue)

#--- a data filter to make sure everything is correct

comp_data_filter <- . %>%
    filter(exposure.code > 2) %>%
    filter(coastal.code > 2) %>%
    filter(depth == 5) %>%
#    filter((sand + pebble) < 40) %>% #no sand data in 2004
#    filter(!is.na(urchin)) %>% #no urchin data in 2004
    mutate(date = mdy(date),
           month = month(date), 
           day = day(date),
          # total_kelp = sac + alar + agar + ldig,
          # understory = sder + desm + ulva +
           #    chaet + codm + poly + rhod + ptilo + porph +
            #   palm + phyc + ccrisp + coral,
           survey = "rasher_steneck") %>%
    select(-c(kelp, sand:ledge, urchin, callo, phyll, bonne:crust)) %>%
     pivot_longer(cols = c(sac, alar, agar, ldig,
                           sder, desm, ulva,
                               chaet, codm, poly, rhod, ptilo,
                           porph, palm , phyc, ccrisp, coral),
                  names_to = "sp_code",
                  values_to = "cover") %>%
    mutate(type = ifelse(sp_code %in% c("sac", "alar", 
                                        "agar", "ldig"),
                         "kelp",
                         "understory")) %>%
    # #average to site level
     group_by(year, month, day, region, site,
              coastal.code, exposure.code,
              latitude, longitude, 
              depth, sp_code, type) %>%
     summarize(cover = mean(cover, na.rm=TRUE)) %>% 
         ungroup()

# Rasher_Steneck_benthicsurvey_algae_allsites_alldepths_2004.csv - Algae counts for all regions from Rasher/Steneck. No urchin data. DOH!

rasher_2004 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_allsites_alldepths_2004.csv",
                        na = c("nr", "", "NA")) %>%
    comp_data_filter

# Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv - Algae counts for all regions and sites from Rasher/Steneck. WITH urchin data.  5m and 10m

rasher_2018 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv",
                        na = c("nr", "", "NA")) %>%
    comp_data_filter

comp_data <- bind_rows(rasher_2004, rasher_2018)%>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))

write_csv(comp_data, "derived_data/compositional_change_data.csv")
