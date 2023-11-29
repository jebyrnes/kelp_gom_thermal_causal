##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Rasher/DMR/Steneck Data Processing                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Process and combine DMR, Steneck/Rasher, and buoy temperature data      ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz  & Jarrett Byrnes                                        ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Last Worked On: Sept 21st, 2021 (major rewrite)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Previously Worked On: May 7th, 2021 (emerging from COVID like a cicada)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

#--- load libraries and get environment setup
setwd(here::here()) #understand premise of 'here' but can't get it to 'find' appropriate folder.
library(tidyverse)
library(lubridate)
library(glue)

## Notes on Data set(s)
##
## These analyses require merging several datasets collected by different groups for different purposes
## Datasets Used in raw_data:
# DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv - DMR surveys of algae and urchins from 2001 - 2018 at 5 and 10m
#   Sites are chosen randomly. Also, some fixed sites were chosen for urchin fishery, but we have filtered those out of this data
# NOAA_temperature_allsites_2001_2018.csv - Oceanographic Buoy data from each region with daily means
# Rasher_Steneck_benthicsurvey_algae_allsites_alldepths_2004.csv - Algae counts for all regions from Rasher/Steneck. No urchin data. DOH!
# Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2016.csv - Algae counts for a subset of regions and sites from Rasher/Steneck. WITH urchin data. 5m and 10m 
# Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2017.csv - Algae counts for a subset of regions and sites from Rasher/Steneck. WITH urchin data.  5m and 10m
# Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv - Algae counts for all regions and sites from Rasher/Steneck. WITH urchin data.  5m and 10m
# ADD BYRNES exposed site SML DATA 2014 - 2018
# GET NUMBER OF QUADS FOR DMR DATA OR RAW DATA?

# What we want for analysis
# - filter to 5m depth
# - where the survey has included urchins (even if they are not present)
# - need to be filtered by exposure score
# basically, depth==5, exposure >2, coastal >2
# - need to be filtered to substrate categorization
# - nr/NA are missing data - if all urchin samples are nr/NA, do not include
# - 2004 data is good for species composition analysis only

## Preparing urchin data
# then, for the urchin analysis, we want a data set like so
# 1. year, month, day, site, region, lat, long, crust, understory, kelp, urchin, 
# averaged at the SITE/YEAR level
# 2. THEN merge all of the data
# 3. then mean (and other metric) region temp for region/year for june, july, august
# maybe do the same for march/april/may?
# 4. THEN mean year-region urchin, deviation from year-region urchin, year-mean temp, temp anomoly
# will need to do some aggregation steneck data for crust/understory
##

#' ----------------------------------------------------------------------
#' 1. load kelp-urchindataframes and process to standard format of 
#' year, month, day, site, region, lat, long, crust, understory, kelp, urchin, 
#' averaged at the SITE/YEAR level
#' ----------------------------------------------------------------------

dmr <- read_csv("raw_data/DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv") %>%
    # filter(depth == 5) %>% #filter later
    # no substrate filter
    filter(exposure.code > 2) %>%
    filter(coastal.code > 2) %>%
    mutate(date = mdy(date),
           month = month(date), 
           day = day(date),
           survey = "dmr") %>%
    mutate(site = as.character(site.number)) %>%
    select(-region.code, -depth.stratum.code, -date, -site.number)
    
# make a rasher/steneck processing workflow

rasher_steneck_workflow <- . %>%
    filter(exposure.code > 2) %>%
    filter(coastal.code > 2) %>%
    filter((sand + pebble) < 40) %>%
    filter(!is.na(urchin)) %>%
    mutate(date = mdy(date),
           month = month(date), 
           day = day(date),
           kelp = sac + alar + agar + ldig,
           desm = desm,
           understory = sder + desm + ulva +
               chaet + codm + poly + rhod + ptilo + porph +
               palm + phyc + ccrisp + coral,
           survey = "rasher_steneck") %>%
    select(year, month, day, region, site, 
           coastal.code, exposure.code,
           latitude, longitude, 
           depth, crust, understory, kelp, desm, urchin) %>%
    #average to site level
    group_by(year, month, day, region, site,
             coastal.code, exposure.code,
             latitude, longitude, 
             depth) %>%
    summarize(kelp = mean(kelp, na.rm=TRUE),
              desm = mean(desm, na.rm = TRUE),
              understory = mean(understory, na.rm=TRUE),
              crust = mean(crust, na.rm=TRUE),
              urchin = mean(urchin, na.rm=TRUE),
    ) %>% ungroup()

rasher_2016 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2016.csv",
                        na = c("nr", "", "NA")) %>%
    rasher_steneck_workflow


rasher_2017 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2017.csv",
                        na = c("nr", "", "NA")) %>%
    rasher_steneck_workflow

rasher_2018 <- read_csv("raw_data/Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv",
                        na = c("nr", "", "NA")) %>%
    rasher_steneck_workflow

#' ----------------------------------------------------------------------
#' -- 2. create the big kahuna combined data set
#' ----------------------------------------------------------------------

combined_bio_data <- bind_rows(dmr,
                               rasher_2016, 
                               rasher_2017, 
                               rasher_2018) %>%
    mutate(survey = "rasher_steneck")

#how much desmerestia was there relative to kelp
rasher_steneck_combined <- bind_rows(rasher_2016, 
                                     rasher_2017, 
                                     rasher_2018) |>
    mutate(desm_to_desm_plus_kelp = desm / (desm + kelp)*100)


mean(rasher_steneck_combined$desm)
range(rasher_steneck_combined$desm)
sd(rasher_steneck_combined$desm)


write_csv(rasher_steneck_combined, "derived_data/rasher_steneck_combined.csv")

write_csv(combined_bio_data, "derived_data/combined_bio_data.csv")

#' ----------------------------------------------------------------------
#' 3. then mean (and other metric) region temp for region/year for june, july, august
# maybe do the same for march/april/may?
# question to ponder - should we be using OISST instead?
#' ----------------------------------------------------------------------

temp <- read_csv("raw_data/NOAA_temperature_allsites_2001_2018.csv") %>%
    select(-`...1`) %>%
    filter(variable %in% c("B01.1mc", "casco", "E01.1m", "F01.1m", "I01.1m","noaa44027.1m"))


# add regions to data
temp_regional <- temp %>%
    mutate(region = factor(variable,
                           levels = c("B01.1mc", "casco", "E01.1m", "F01.1m", "I01.1m","noaa44027.1m"),
                           labels = c("york", "casco.bay", "midcoast", "penobscot.bay", "mdi", "downeast"))) %>%
    mutate(region = as.character(region)) 

# do some interpolation for missing data
source("scripts/1a_interpolate_downeast_temps.R")

# now make an aggregated temperature data set from the interpolated data
temp_aggregated <- temp_regional_interpolated %>%
    filter(month %in% 3:8) %>% #month in march:august
    mutate(season = ifelse(month < 6, "spring", "summer"),
           stress_15 = as.numeric(value > 15),
           stress_20 = as.numeric(value > 20),
    ) %>% #degree heat days
    group_by(year, region, variable, season) %>%
    summarize(mean_temp = mean(value, na.rm=TRUE),
              max_temp = max(value, na.rm=TRUE),
              min_temp = min(value, na.rm=TRUE),
              degree_heat_days_15 = sum(stress_15, na.rm=TRUE),
              degree_heat_days_20 = sum(stress_20, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    #fix some missing data errors
    mutate(degree_heat_days_15 = ifelse(is.na(mean_temp), NA, degree_heat_days_15),
           degree_heat_days_20 = ifelse(is.na(mean_temp), NA, degree_heat_days_20),
           max_temp = ifelse(is.na(mean_temp), NA, max_temp),
           min_temp = ifelse(is.na(mean_temp), NA, min_temp)
    ) %>% 
    pivot_wider(names_from = "season",
                values_from = c("mean_temp",
                                "max_temp",
                                "min_temp",
                                "degree_heat_days_15",
                                "degree_heat_days_20"
                                
                )) %>%
    rename(temp_source = variable)

#check missing
temp_regional_interpolated %>%
  filter(month %in% 3:8) %>% #month in march:august
  mutate(season = ifelse(month < 6, "spring", "summer")) %>%
  group_by(region, season) %>%
  summarize(missing = sum(is.na(value))/n())

# 
# #check mdi and downeast
# ggplot(temp_regional %>% filter(month > 5 & month < 9) %>%
#          filter(region %in% c("mdi", "downeast")),
#        aes(x =  value, fill = region)) +
#   geom_density(alpha = 0.5) #+
# #  facet_wrap(vars(region))
# 
# ggplot(temp_regional %>%# filter(month > 5 & month < 9) %>%
#          filter(region %in% c("mdi", "downeast")) %>%
#          select(-variable) %>%
#          pivot_wider(names_from = region, values_from = value),
#        aes(x =  downeast, y = mdi, color = factor(month))) +
#   geom_point() +
#   geom_abline(slope = 1, intercept = 0, lty = 2)

temp_aggregated %>% 
  group_by(region) %>%
  summarize(missing_spring = sum(is.na(mean_temp_spring)),
            missing_summer = sum(is.na(mean_temp_summer))
  )
  
write_csv(temp_aggregated, "derived_data/temp_timeseries.csv")

#' -----------------------------------------
#' Merge temp and bio data
#' -----------------------------------------

combined_bio_temp <- left_join(combined_bio_data, temp_aggregated)


#' -----------------------------------------
#' 4. THEN mean year-region urchin, deviation from year-region urchin, 
#'     year-mean temp, temp anomoly 
#' -----------------------------------------

combined_bio_temp_gmc <- combined_bio_temp %>%
  group_by(region) %>%
  mutate(across(mean_temp_spring:degree_heat_days_20_summer, 
                ~mean(.x, na.rm = TRUE),
                .names = "mean_{.col}"),
         mean_regional_urchin = mean(urchin, na.rm = TRUE),
         urchin_anom_from_region = urchin - mean_regional_urchin) %>%
  #calculate temp anomolies using black magic with get, glue, and across
  mutate(across(mean_temp_spring:degree_heat_days_20_summer, 
                ~ .x - get(glue("mean_{cur_column()}")),
                .names = "{.col}_dev")) %>%
  ungroup() %>%
  group_by(year) %>%
  mutate(mean_annual_urchin = mean(urchin, na.rm=TRUE)) %>%
  ungroup()


# Add lagged variables

lagged_vars <- combined_bio_temp_gmc %>%
  group_by(region, year) %>%
  summarize(across(mean_temp_spring_dev:degree_heat_days_20_summer_dev,
                   ~ max(.x, na.rm=TRUE))) %>%
  group_by(region) %>%
  arrange(year) %>%
  mutate(across(mean_temp_spring_dev:degree_heat_days_20_summer_dev,
                lag)) %>%
  mutate(across(mean_temp_spring_dev:degree_heat_days_20_summer_dev,
                ~ifelse(!is.finite(.x), NA, .x))) %>%
  ungroup() %>%
  rename_with(~paste0("lag_", .x), 
              mean_temp_spring_dev:degree_heat_days_20_summer_dev)

combined_bio_temp_gmc <- left_join(combined_bio_temp_gmc, lagged_vars)  

write_csv(combined_bio_temp_gmc, "derived_data/combined_data_for_analysis.csv")
