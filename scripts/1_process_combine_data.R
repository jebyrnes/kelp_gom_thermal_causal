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
           understory = sder + desm + ulva +
               chaet + codm + poly + rhod + ptilo + porph +
               palm + phyc + ccrisp + coral,
           survey = "rasher_steneck") %>%
    select(year, month, day, region, site, 
           coastal.code, exposure.code,
           latitude, longitude, 
           depth, crust, understory, kelp, urchin) %>%
    #average to site level
    group_by(year, month, day, region, site,
             coastal.code, exposure.code,
             latitude, longitude, 
             depth) %>%
    summarize(kelp = mean(kelp, na.rm=TRUE),
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

write_csv(combined_bio_data, "derived_data/combined_bio_data.csv")

#' ----------------------------------------------------------------------
#' 3. then mean (and other metric) region temp for region/year for june, july, august
# maybe do the same for march/april/may?
# question to ponder - should we be using OISST instead?
#' ----------------------------------------------------------------------

temp <- read_csv("raw_data/NOAA_temperature_allsites_2001_2018.csv") %>%
    select(-X1) %>%
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
           stress = as.numeric(value > 17)) %>%
    group_by(year, region, variable, season) %>%
    summarize(mean_temp = mean(value, na.rm=TRUE),
              max_temp = max(value, na.rm=TRUE),
              min_temp = min(value, na.rm=TRUE),
              degree_heat_days = sum(stress, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    #fix some missing data errors
    mutate(degree_heat_days = ifelse(is.na(mean_temp), NA, degree_heat_days),
           max_temp = ifelse(is.na(mean_temp), NA, max_temp),
           min_temp = ifelse(is.na(mean_temp), NA, min_temp)
    ) %>% 
    pivot_wider(names_from = "season",
                values_from = c("mean_temp",
                                "max_temp",
                                "min_temp",
                                "degree_heat_days"
                )) %>%
    rename(temp_source = variable)
  
#' -----------------------------------------
#' Merge temp and bio data
#' -----------------------------------------

combined_bio_temp <- left_join(combined_bio_data, temp_aggregated)


#' -----------------------------------------
#' 4. THEN mean year-region urchin, deviation from year-region urchin, year-mean temp, temp anomoly
# will need to do some aggregation steneck data for crust/understory
#' -----------------------------------------


#' -----------------------         
#' Below here is old code. Ignore and use for notes
#' -----------------------

dmr <- read.csv("dmr.csv", header=TRUE) #DMR's randomly surveyed annual urchin/kelp dives
stenecksg <- read.csv("stenecksg.csv", header=TRUE) #merged datasets of algal % cover from Steneck, Adey and Rasher/Suskiewicz

#check structure
str(dmr) #note: Date is a factor, but currently this doesn't matter for inner_join
str(stenecksg) #looks good

#select only categories needed for merging (joining) dataframes, create two new DFs - dmr.join & stenecksg.join
dmr.join <- dmr %>%
    select (c(year,
              region,
              depth,
              urchin,
              kelp,
              latitude,
              longitude,
              exposure,
              coastal))  %>%
    filter (depth==5, exposure >2, coastal >2)

stenecksg.join <-stenecksg %>%
    select (c(year,
              region,
              depth,
              urchin,
              kelp,
              latitude,
              longitude,
              exposure,
              coastal))  %>%
    filter (depth==5, exposure >2, coastal >2) %>%
    mutate(urchin = as.numeric(urchin))

#stenecksg.join$urchin <- as.numeric(stenecksg.join$urchin) #forces $urchin to be numeric instead of 'logi'

#change values where kelp >100 to equal 100 (these are outliers but problematic - mostly desmarestia confounding kelp cover estimates).
stenecksg.join <- stenecksg.join %>% 
    mutate(kelp = replace(kelp, kelp > 100, 100),
           kelp.perc=(kelp/100)) #creates new column 'kelp.perc'

#set epsilon to 1/2 of lowest value (0.1%, or 0.001, ergo epsilon = 0.0005).  Then mutate 0 values to 0+epsilon, and 100 values to 100-epsilon.
#this eliminates -Inf/Inf values from transformation
stenecksg.join <- stenecksg.join %>% 
    mutate(kelp.perc = replace(kelp.perc, kelp.perc == 1, 0.9995)) %>% 
    mutate(kelp.perc = replace(kelp.perc, kelp.perc == 0, 0.0005))

#Joining Dataframes####
#First, create kelp.perc column with values from 0 to 1; mutate values so 0 = 0.0005 and 1 = 0.9995
dmr.join <- dmr.join %>% 
    mutate(kelp.perc=(kelp/100),
           kelp.perc = replace(kelp.perc, kelp.perc == 1, 0.9995),
           kelp.perc = replace(kelp.perc, kelp.perc == 0, 0.0005)) #this avoids inf/-inf errors

#use full_join to merge two dataframes (stenecksg.join and dmr.join). Create master dataframe 'DF'
#DF <-full_join(dmr.join,stenecksg.join, by=c('year','latitude', 'longitude', 'depth', 'urchin', 'region', 'kelp', 'exposure', 'coastal', 'kelp.perc'))
DF <- bind_rows(dmr.join, stenecksg.join)

#logit transformation####
#perform logit transformation on kelp.perc within DF ($logit.kelp)
DF$logit.kelp <- logit(DF$kelp.perc)

#Set Urchin values to 0 (< 10) and 1 (> 10) for binary determination.
# Urchin 1/0 ####
DF <- mutate(DF, urchin.limit = as.numeric(urchin>10)) #-- no longer necessary(?)


#GMC dataframes####
#take group mean centering approach for both Kelp%Cover and Temperature Degree Days
#NOTE: Currently kelp uses 'logit.kelp' transformation.  Change if/when better method is found (beta-regression)

GMC.kelp <-gmc(DF, #c("urchin.limit", "logit.kelp"), 
               "urchin",
               by =c("region"), #took out year
               FUN = mean, 
               suffix = c("_mn", "_dev"),
               fulldataframe = TRUE)


#Temperature Data####
#load in dataframe for temperature
combined <- read.csv ("gom_combined.csv", header=TRUE)
str(combined) #check structure

#rename the variables we are interested in, drop others
combined$variable <- factor(combined$variable,
                            levels = c("B01.1mc", "casco", "E01.1m", "F01.1m", "I01.1m","noaa44027.1m"),
                            labels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"))

#when downeast has no data, interpolate from MDI
mdi_down <- combined %>%
    select(-X) %>%
    filter(as.character(variable) %in% c("mdi", "downeast")) %>%
    tidyr::pivot_wider(names_from = "variable",
                       values_from = "value")

ggplot(mdi_down,
       aes(x = mdi, downeast)) +
    geom_point(shape = 1) +
    #    geom_abline(slope = 1, intercept = 0, lty = 2, color = "red") +
    stat_smooth(method = "lm", color = "blue") +
    geom_hline(yintercept = 17, color = "orange") + facet_wrap(~year)

#fit a model detailing the relationship between the MDI temps and downeast
mdi_down_lm <- lm(downeast ~ mdi , data = mdi_down)
anova(mdi_down_lm)

# interpolate missing temperatures
mdi_down_int <- mdi_down %>%
    modelr::add_predictions(mdi_down_lm) %>%
    mutate(downeast = ifelse(is.na(downeast), pred, downeast)) %>%
    select(-pred) %>%
    tidyr::pivot_longer(cols =  c("mdi", "downeast"),
                        names_to = "variable",
                        values_to = "value")

# add interpolated data back
combined <- combined %>%
    filter(!(as.character(variable) %in% c("mdi", "downeast"))) %>%
    bind_rows(mdi_down_int)

#filter out all values which exceed 10.0ºC (this speeds things up downstream)
stress <- combined %>%
    filter(month %in% c(6,7,8), 
           year>2000, 
           variable %in%c("casco", "york", "midcoast", "penbay", "mdi", "downeast")) 

stress$stress.temp <- stress$value - 17 # takes the value from the 'column'value'  and subtracts 17.0 to yield 'stress day' value

#rename column 'variable' to 'region' to facilitate merging downstream
stress <- rename(stress, region=variable)

#change negative values to zero (negative values are degree days < 17ºC and not of interest to us here)
stress$stress.temp[stress$stress.temp<0] <-0

stress <- rename(stress, temp_c = value)

# make annual regional stress and temp values
stress <-  stress %>% 
    group_by(year, region) %>% 
    summarise(stress.temp = sum(stress.temp, na.rm = TRUE),
              mean_temp = mean(temp_c, na.rm = TRUE),
              max_temp = max(temp_c, na.rm = TRUE)) %>%
    ungroup()

#Create GMC value for stress.temp by region
stress_gmc <- gmc(stress, c("stress.temp", "mean_temp", "max_temp"), 
                  by =c("region"), 
                  FUN = mean, suffix = c("_mn", "_dev"),
                  fulldataframe = TRUE)


# combine stress.tbl1 and GMC.kelp to create master dataframe.  
#new DF (DF.join) will have new columns: 'urchin_mn' & kelp_mn (from GMC.kelp) and 'stress.temp' from stress.tbl
#As I understand, these values will populate for the entire year/site combo.
DF.join <- right_join(stress_gmc, GMC.kelp, by=c('year', 'region'))

#check to see things
visdat::vis_dat(DF.join %>%
                    arrange(year))

write_csv(DF.join, "derived_data/combined_data_for_analysis.csv")
