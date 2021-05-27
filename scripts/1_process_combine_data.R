##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Rasher/DMR/Steneck Data Processing                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Process and combine DMR, Steneck/Rasher, and buoy temperature data      ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz  & Jarrett Byrnes                                        ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Last Worked On: May 7th, 2021 (emerging from COVID like a cicada)             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

## Notes on Data set(s)
##
## These analyses require merging several datasets collected by different groups for different purposes
## Datasets Used:
##      dmr.csv - Annual random surveys of kelp and urchin cover by DMR from 2001 thru 2019.  Kelp is % cover. urchins are count
##      stenecksg.csv  - Fleshy macroalgae surveys (in % cover) by Bob Steneck (1997-present) & Doug Rasher (2016-present)
##      gom_combined.csv  - Water Temperature derived from NOAA & NERACOOS ocean buoys (2001-present), EC'd and compiled by TSS

#working model is: Kelp%Cover ~GMC Degree Days * GMC Urchin Threshold + 1|Region + 1|Year
#where GMC = Group Mean Centering
#see notepad for further explanation and train of thought between Brynes, Rasher and myself

#Top####
#load necessary packages
library(tidyverse) #for datawrangling
library(rockchalk) #regression functions
library(gtools) 
library(lattice) #data exploration

#clear R brain, set WD
#rm(list=ls())

#setwd("~/Desktop/Seagrant-UrchinKelp2018") #apparently not...
#getwd()
setwd(here::here()) #understand premise of 'here' but can't get it to 'find' appropriate folder.

#load dataframes
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
