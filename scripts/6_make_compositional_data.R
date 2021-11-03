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
                  values_to = "cover")  %>%
    # #average to site level
     group_by(year, month, day, region, site,
              coastal.code, exposure.code,
              latitude, longitude, 
              depth, sp_code) %>%
     summarize(cover = mean(cover, na.rm=TRUE)) %>% 
         ungroup()

# Rasher_Steneck_benthicsurvey_algae_allsites_alldepths_2004.csv - Algae counts for all regions from Rasher/Steneck. No urchin data. DOH!

rasher_2004 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_allsites_alldepths_2004.csv",
                        na = c("nr", "", "NA")) %>%
    comp_data_filter

# Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv - Algae counts for all regions and sites from Rasher/Steneck. WITH urchin data.  5m and 10m
# in 2004 poly had more things in it?
# poly in 2004 is poly + callo + bonne
rasher_2018 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv",
                        na = c("nr", "", "NA")) %>%
  #fix for 2004 to 2018 taxonomy differences
  mutate(poly = poly+callo+bonne,
         rhod = rhod + cystoc) %>%
    comp_data_filter

# Get Byrnes KEEN data ####
byrnes <- read_csv("https://raw.githubusercontent.com/kelpecosystems/observational_data/master/cleaned_data/keen_cover.csv") %>%
    filter(SITE %in% c("NE Appledore", "NW Appledore"),
           YEAR == 2018)
# c(sac, alar, agar, ldig,
#   sder, desm, ulva,
#   chaet, codm, poly, rhod, ptilo,
#   porph, palm , phyc, ccrisp, coral)

translate <- data.frame(
    sp_code = c("SLJ", "SL", "ALES", "AGCL", "LADI",
      "SADE", "DEAC", "DEVI", "UV",
      "FG", "COF", "POLS", "HJ", "CYPU", "PTSE",
      "PORS", "PAPA", "PHRU", "CHCR", "CO", "BOHA"),
    
    new_sp_code = c("sac", "sac", "alar", "agar", "ldig",
                    "sder", "desm", "desm", "ulva",
                    "chaet", "codm", "poly", "poly", "rhod", "ptilo",
                    "porph", "palm", "phyc", "ccrisp", "coral", "poly"))



byrnes_2018 <- byrnes %>%
    filter(SP_CODE %in%
               c("SLJ", "SL", "ALES", "AGCL", "LADI",
                 "SADE", "DEAC", "DEVI", "UV",
                 "FG", "COF", "POLS", "HJ", "CYPU", "PTSE",
                 "PORS", "PAPA", "PHRU", "CHCR", "CO")) %>%
    mutate(REGION = "york") %>%
    select(YEAR, MONTH, DAY, REGION, SITE, TRANSECT, SP_CODE, PERCENT_COVER) %>%
    rename_with(tolower) %>%
    rename(cover = percent_cover) %>%
    
    #deal with sp codes
    left_join(translate) %>%
    select(-sp_code) %>% rename(sp_code = new_sp_code) %>%
    
    pivot_wider(names_from = sp_code, #add 0s
                values_from = cover,
                values_fill = list(cover = 0),
                values_fn = sum) %>%
    mutate(ldig = 0, alar = 0, ulva = 0, 
           sder = 0, porph = 0, phyc = 0, codm=0,
           ptilo = 0) %>% #fill in 0s for missing species
    pivot_longer(-c(year, month, day, region, site, transect),
                 names_to = "sp_code",
                 values_to = "cover") %>%
    select(-site) %>%
    rename(site = transect)
    
comp_data <- bind_rows(rasher_2004, rasher_2018, byrnes_2018)%>%
    mutate(region = gsub("\\.", " ", region),
           region = stringr::str_to_title(region),
           region = gsub("Mdi", "MDI", region),
           region = factor(region,
                           levels = c("Downeast", "MDI", "Penobscot Bay",
                                      "Midcoast", "Casco Bay", "York")))%>%
    mutate(type = ifelse(sp_code %in% c("sac", "alar", 
                                        "agar", "ldig"),
                         "kelp",
                         "understory"))

write_csv(comp_data, "derived_data/compositional_change_data.csv")
