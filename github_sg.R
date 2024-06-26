##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  RoyalB_Model Generation                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Running a model to test if temp and/or urchin influences kelp cover       ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz   - March 10th, 2020                                         ##~~##~~##~~##~~##~
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
library(gllvm) #generalize linear latent variable models
library(car) #for quantile-quantile plots
library(lattice) #data exploration
library(glmmTMB) # General Lineawr Mixed Models & beta regression
library(lme4) # linear Mixed-effects models using 'Eigen' and S4
library(DHARMa) # Residual diagnostics for hierarchal regression models
library(here) # paths to data should 'just work' (though having problems with it)

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

#plot logit transformation of kelp to check if normalcy is better
qqPlot (DF$logit.kelp)
qqPlot (DF$kelp.perc)

histogram(~ kelp.perc | region, data = DF,
          breaks = 20)

#below is for non-transformed kelp data
densityplot(~ kelp.perc | region, data = DF,
            group = region,
            plot.points = FALSE)

#below is for logit transformed kelp data
densityplot(~ logit.kelp | region, data = DF,
            group = region,
            plot.points = FALSE)

xyplot (logit.kelp~year | region, group = region, data=DF,
        type = c("p", "smooth"),
        scales = "free")

xyplot (kelp.perc~year | region, group = region, data=DF,
        type = c("p", "smooth"),
        scales = "free")

##~~##~~##~~##~~##~~##~~##~~##~~##
#Note: Logit transformation does not appear any better than non-transformed data.  
# Try alternative transformation? Guidance of JEB - beta regression!
##~~##~~##~~##~~##~~##~~##~~##~~##

#Set Urchin values to 0 (< 10) and 1 (> 10) for binary determination.
# Urchin 1/0 ####
DF <- mutate(DF, urchin.limit = as.numeric(urchin>10)) #-- no longer necessary(?)

# DF<- DF %>% 
#     mutate(urchin.limit = replace(urchin.limit, urchin >10, 1)) %>% 
#     mutate(urchin.limit = replace(urchin.limit, urchin <9.99, 0))

#GMC dataframes####
#take group mean centering approach for both Kelp%Cover and Temperature Degree Days
#NOTE: Currently kelp uses 'logit.kelp' transformation.  Change if/when better method is found (beta-regression)

GMC.kelp <-gmc(DF, #c("urchin.limit", "logit.kelp"), 
               "urchin",
               by =c("region"), #took out year
               FUN = mean, 
               suffix = c("_mn", "_dev"),
               fulldataframe = TRUE)

#ATTEMPT to bring numerous urchin limits into this DF
# GMC.kelp <-gmc(DF, c("urchin.limit1", 
#                      "urchin.limit5", 
#                      "urchin.limit8", 
#                      "urchin.limit10", 
#                      "urchin.limit12", 
#                      "urchin.limit15", 
#                      "urchin.limit20",
#                      "logit.kelp"), 
#                by =c("year", "region"), 
#                FUN = mean, suffix = c("_mn", "_dev"),
#                fulldataframe = TRUE)

#ATTEMPT to bring numerous urchin limits into this DF
# GMC.kelp <-gmc(DF, c("urchin.limit1", 
#                      "logit.kelp"), 
#                by =c("year", "region"), 
#                FUN = mean, suffix = c("_mn", "_dev"),
#                fulldataframe = TRUE)

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

# #create tibble with stress temp GMC value for each region/year combo
# stress.tbl <- stress %>% 
#     group_by(year, region) %>% 
#     summarise(stress.temp_mn = sum(stress.temp_mn))
# 
# #create tibble with stress temp value for each region/year combo
# temp.tbl <- stress %>% 
#     group_by(year, region) %>% 
#     summarise(stress.temp = sum(stress.temp))


#check stress.tbl by plotting values
xyplot (stress.temp_mn~year | region, group = region, data=stress_gmc,
        type = c("p", "smooth"),
        scales = "free")

xyplot (stress.temp~year | region, group = region, data=stress_gmc,
        type = c("p", "smooth"),
        scales = "free")

# combine stress.tbl1 and GMC.kelp to create master dataframe.  
#new DF (DF.join) will have new columns: 'urchin_mn' & kelp_mn (from GMC.kelp) and 'stress.temp' from stress.tbl
#As I understand, these values will populate for the entire year/site combo.
DF.join <- right_join(stress_gmc, GMC.kelp, by=c('year', 'region'))

#check to see things
visdat::vis_dat(DF.join %>%
                    arrange(year))

#Success??!!

#beta regression####
#problem: kelp.perc is zero-inflated and non-linear.  Normal logit transformation did little.
#approach: use beta-regression analysis (glmmTMB package) to improve model.
#I'm lost as how to accomplish this.  From my limited understanding beta-regression (if successful) would have a lower aAIC score than logit-transformed kelp.perc
#from glmmTMB:  Beta distribution: parameterization of Ferrari and Cribari-Neto (2004) and the betareg package (Cribari-Neto and Zeileis 2010); V=mu*(1-mu)/(phi+1)
#see Douma & Weedon (2019) in Methods Ecol Evol.

#Run Model####

#working model: Kelp%Cover ~GMC Degree Days * GMC Urchin Threshold + 1|Region + 1|Year

DF.join <- DF.join %>%
    group_by(region) %>%
    mutate(stress.temp_mn = mean(stress.temp, na.rm=TRUE)) %>%
    ungroup()

mod <- lmer(logit(kelp.perc) ~ urchin.limit * stress.temp + stress.temp_mn +
                   (1|year) + (1|region),
               data = DF.join)

simRes <- simulationOutput <- simulateResiduals(fittedModel = mod)
plotQQunif(simRes)

#'---------------------------------------
# Models
#'---------------------------------------

library(glmmTMB)

# - Which temperature model to use?

# - Model results of urchins and temperature
mod_urchin <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev * 
                          mean_temp_dev + mean_temp_mn +
                          (1|year) + (1|region),
                      family = beta_family(),
                      data = DF.join)

mod_urchin_add <- glmmTMB(kelp.perc ~ urchin_mn + urchin_dev +
                              mean_temp_dev + mean_temp_mn +
                          (1|year) + (1|region),
                      family = beta_family(),
                      data = DF.join)


simResUrch <- simulationOutput <- simulateResiduals(fittedModel = mod_urchin)
plotQQunif(simResUrch) 
summary(mod_urchin_add)
Anova(mod_urchin)
Anova(mod_urchin_add)


#values for visreg
median(DF.join$urchin_mn)
median(DF.join$mean_temp_mn)

visreg::visreg2d(mod_urchin_add, xvar = "urchin_dev", yvar = "mean_temp_dev", 
                 scale = "response", plot.type="gg") +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                         midpoint = 0.5, limits = c(0,1)) +
    labs(fill = "Percent Cover of Kelp",
         y = "Deviation from Regional Temperature Mean",
         x = "Deviation from Regional Urchin Mean",
         subtitle = "Assumes regional mean summer temp of 14C \nand urchin abundance of 1.3 per sq m")

visreg::visreg2d(mod_urchin_add, xvar = "urchin_dev", yvar = "mean_temp_dev", 
                 scale = "response", plot.type="gg", 
                 cond = list(urchin_mn = 0,
                             mean_temp_mn = 15)) +
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                         midpoint = 0.5, limits = c(0,1)) +
    labs(fill = "Percent Cover of Kelp",
         y = "Deviation from Regional Temperature Mean",
         x = "Deviation from Regional Urchin Mean",
         subtitle = "Assumes regional mean summer temp of 16C \nand urchin abundance of 0 per sq m")



#### What conveys the story in the cleanest way?
## The overall story is still this idea that we have seen low urchins in GOM 
## for 2 decades. What are the kelp forests of Maine doing? In the S they are not
## doing well because of temperature. From Penobscot Bay east, they are doing fine




## summer mean getting worse in the west
## kelp data says kelp has come back except in the south
## S has experienced anomolously high temp



###
mod_temp <- glmmTMB(kelp.perc ~ mean_temp_dev +
                        mean_temp_mn +
                          (1|year) + (1|region),
                      family = beta_family(),
                      data = DF.join %>% filter(urchin > 10))

simResTemp <- simulationOutput <- simulateResiduals(fittedModel = mod_temp)
plotQQunif(simResTemp) 
summary(mod_temp)


ggplot(DF.join, aes(x = year, y = urchin)) + geom_point() + facet_wrap(~region)


# what does it mean?
library(visreg)
visreg2d(mod_urchin, xvar = "urchin", yvar = "stress.temp", scale = "response")


ggplot(DF.join, aes(x = urchin, y = stress.temp)) + geom_point()

library(modelr)

data_grid(DF.join,
          urchin = seq_range(urchin, n = 100),
          stress.temp = seq_range(stress.temp, n = 100),
          stress.temp_mn = mean(stress.temp_mn, na.rm = TRUE)) %>%
    mutate(kelp.perc = predict(mod_urchin, 
                               newdata = .,
                               type = "response",
                               re.form = NULL))




#here, it's a temp driven model with a ZI driven by the urchin threshold
modzi <- glmmTMB(kelp.perc ~ stress.temp +
                     stress.temp_mn +
                (1|year) + (1|region),
                family = beta_family(),
                ziformula = ~urchin,
                data = DF.join)

simResZi <- simulationOutput <- simulateResiduals(fittedModel = modzi)
plotQQunif(simResZi) 

summary(modzi)
car::Anova(modzi)

## Thew's Thoughts:
## Why are we using stress.temp + mean stress temp; 
## Should we attempt to chane the urgin limit to test whether fit is better at different thresholds?
## We are still using logit.kelp in this model (go back to GMC.kelp before DF.join is created)

#urchin limits####
#Below will create different urchin 'thresholds' to run glmmTMB model for best fit (lowest AIC score)

#set urchin limit to different levels (verify a limit of 10 is appropriate)
DF <- transform(DF, urchin.limit1=(urchin)) # set urchin threshold at 1
DF <- transform(DF, urchin.limit5=(urchin)) # set urchin threshold at 5
DF <- transform(DF, urchin.limit8=(urchin)) # set urchin threshold at 8
DF <- transform(DF, urchin.limit10=(urchin)) # set urchin threshold at 10
DF <- transform(DF, urchin.limit12=(urchin)) # set urchin threshold at 12
DF <- transform(DF, urchin.limit15=(urchin)) # set urchin threshold at 15
DF <- transform(DF, urchin.limit20=(urchin)) # set urchin threshold at 20

DF<- DF %>% 
    mutate(urchin.limit1 = replace(urchin.limit1, urchin >1, 1)) %>% 
        mutate(urchin.limit1 = replace(urchin.limit1, urchin <0.99, 0))%>%  #urchin threshold at 1
    mutate(urchin.limit5 = replace(urchin.limit5, urchin >5, 1)) %>% 
        mutate(urchin.limit5 = replace(urchin.limit5, urchin <4.99, 0))%>%  #urchin threshold at 5
    mutate(urchin.limit8 = replace(urchin.limit8, urchin >8, 1)) %>% 
        mutate(urchin.limit8 = replace(urchin.limit8, urchin <7.99, 0))%>%  #urchin threshold at 8
    mutate(urchin.limit10 = replace(urchin.limit10, urchin >10, 1)) %>% 
        mutate(urchin.limit10 = replace(urchin.limit10, urchin <9.99, 0))%>%  #urchin threshold at 10
    mutate(urchin.limit12 = replace(urchin.limit12, urchin >12, 1)) %>% 
        mutate(urchin.limit12 = replace(urchin.limit12, urchin <11.99, 0)) %>% #urchin threshold at 12
    mutate(urchin.limit15 = replace(urchin.limit15, urchin >15, 1)) %>% 
        mutate(urchin.limit15 = replace(urchin.limit15, urchin <14.99, 0))%>%  #urchin threshold at 15
    mutate(urchin.limit20 = replace(urchin.limit20, urchin >20, 1)) %>% 
        mutate(urchin.limit20 = replace(urchin.limit20, urchin <19.99, 0)) #urchin threshold at 20


str(DF)
summary(DF$urchin.limit10) 
#next create DF.join from DF and ____ (STUCK)
