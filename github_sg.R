##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  RoyalB_Model Generation                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Purpose: Running a model to test if temp and/or urchin influences kelp cover       ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Thew Suskiewicz   - March 10th, 2020                                         ##~~##~~##~~##~~##~
##~~##~~##~~##~~  Last Worked On: July 2020 (The COVID Days)                                 ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~

## Notes on Data set(s)
##
## These analyses require merging several datasets collected by different groups for different purposes
## Datasets Used:
##      dmr.csv - Annual random surveys of kelp and urchin cover by DMR from 2001 thru 2019.  Kelp is % cover. urchins are count
##      stenecksg.csv  - Fleshy macroalgae surveys (in % cover) by Bob Steneck (1997-present) & Doug Rasher (2016-present)
##      gom_combined.csv  - Water Temperature derived from NOAA & NERACOOS ocean buoys (2001-present)

#working model is: Kelp%Cover ~GMC Degree Days * GMC Urchin Threshold + 1|Region + 1|Year
#where GMC = Group Mean Centering
#see notepad for further explanation and train of thought between Brynes, Rasher and myself

#Top####
#clear R brain, set WD
rm(list=ls())

setwd("~/Desktop/data_depot/gom_seaweed")
getwd()

#load necessary packages
library(tidyverse) #for datawrangling
library(rockchalk) #regression functions
library(gtools) 
library(gllvm) #generalize linear latent variable models
library(car) #for quantile-quantile plots
library(lattice) #data exploration
library(glmmTMB) # General Lineawr Mixed Models & beta regression


#glmmTMB

#load dataframes: dmr = DMR's randomly surveyed annual urchin dives (with kelp data), stenecksg = steneck and Sea Grant master file
dmr <- read.csv("dmr.csv", header=TRUE)
stenecksg <- read.csv("stenecksg.csv", header=TRUE)

#check structure
str(dmr) #note: Date is a factor, but currently this doesn't matter for inner_join
str(stenecksg)

#select only categories needed for merging (joining) dataframes, create two new DFs - dmr.join & stenecksg.join
dmr.join <-dmr %>%
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
                    filter (depth==5, exposure >2, coastal >2)

stenecksg.join$urchin <- as.numeric(stenecksg.join$urchin) #forces $urchin to be numeric instead of 'logi'

#change values where kelp >100 to equal 100 (these are outliers but problematic - mostly desmarestia confounding kelp cover estimates).
stenecksg.join <- stenecksg.join %>% 
    mutate(kelp = replace(kelp, kelp > 100, 100))

#create new column: kelp.perc = kelp/100
stenecksg.join <- transform(stenecksg.join, kelp.perc=(kelp/100))

#set epsilon to 1/2 of lowest value (0.1%, or 0.001, ergo epsilon = 0.0005).  Then mutate 0 values to 0+epsilon, and 100 values to 100-epsilon.
#this eliminates -Inf/Inf values from transformation
stenecksg.join <- stenecksg.join %>% 
    mutate(kelp.perc = replace(kelp.perc, kelp.perc == 1, 0.9995)) %>% 
    mutate(kelp.perc = replace(kelp.perc, kelp.perc == 0, 0.0005))

stenecksg.join$urchin <- as.numeric(stenecksg.join$urchin)

#First, create kelp.perc column with values from 0 to 1
dmr.join <- transform(dmr.join, kelp.perc=(kelp/100))

#Second, mutate values so 0 = 0.0005 and 1 = 0.9995
dmr.join <- dmr.join %>% 
    mutate(kelp.perc = replace(kelp.perc, kelp.perc == 1, 0.9995)) %>% 
    mutate(kelp.perc = replace(kelp.perc, kelp.perc == 0, 0.0005))

#use full_join to merge two dataframes (stenecksg.join and dmr.join)
DF <-full_join(dmr.join,stenecksg.join, by=c('year','latitude', 'longitude', 'depth', 'urchin', 'region', 'kelp', 'exposure', 'coastal', 'kelp.perc'))

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
# Try alternative transformation? Guidance of JEB - beta regression
##~~##~~##~~##~~##~~##~~##~~##~~##


#Set Urchin values to 0 (< 10) and 1 (> 10) for binary determination.
# Urchin 1/0 ####
DF <- transform(DF, urchin.limit=(urchin)) #-- no longer necessary(?)

DF<- DF %>% 
    mutate(urchin.limit = replace(urchin.limit, urchin >10, 1)) %>% 
    mutate(urchin.limit = replace(urchin.limit, urchin <9.99, 0))

#GMC dataframes####

##~~##~~  Train of Thought ##~~##~~##
##~~##~~##~~##~~##~~##~~##~~##
##   Here (below) we are taking the logit-transformation of kelp and TEMPERATURE in order to 
##    ARG>>>> NEED TO GET THIS WORKED OUT>  I THINK IT IS TEMP KELP GMC, BUT KELP IS LOGIT TRANSFERED TO ACCOUNT FOR DISTRIBUTION 
##  Kelp is logit transformed.  This creates a more linear distrbution, rather than clumps at the low end and high end.
## asked for guidance... DBR deferred to JEB, waiting response
## Model (conceptuatlly) is: the effect of temperature degree days (GMC) and urchin abundance (GMC) on kelp cover (logit trasnformed) 
##... with year and region as random variables



GMC.kelp <-gmc(DF, c("urchin.limit", "logit.kelp"), by =c("year", "region"), FUN = mean, suffix = c("_mn", "_dev"),
    fulldataframe = TRUE)


#GMC for temperature data
#load in dataframe for temperature
combined <- read.csv ("gom_combined.csv", header=TRUE)
str(combined)

#rename the variables we are interested in, drop others
combined$variable <- factor(combined$variable,
                            levels = c("B01.1mc", "casco", "E01.1m", "F01.1m", "I01.1m","noaa44027.1m"),
                            labels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"))

#filter out all values which exceed 17.0ºC
stress <- combined %>%
    filter(value > 10, year>2000, variable %in%c("casco", "york", "midcoast", "penbay", "mdi", "downeast")) 

stress$stress.temp <- stress$value - 17. # takes the value from the 'column'value'  and subtracts 17.0 to yield 'stress day' value

#rename column 'variable' to 'region' to facilitate merging downstream
stress <- rename(stress, region=variable)

#change negative values to zero
stress$stress.temp[stress$stress.temp<0] <-0

#Create GMC value for stress.temp by region
stress <-gmc(stress, c("stress.temp"), by =c("year", "region"), FUN = mean, suffix = c("_mn", "_dev"),
               fulldataframe = TRUE)

#create tibble with stress temp GMC value for each region/year combo
stress.tbl <- stress %>% 
    group_by(year, region) %>% 
    summarise(stress.temp_mn = sum(stress.temp_mn))

#create tibble with stress temp value for each region/year combo
temp.tbl <- stress %>% 
    group_by(year, region) %>% 
    summarise(stress.temp = sum(stress.temp))


#check stress.tbl by plotting values
xyplot (stress.temp_mn~year | region, group = region, data=stress.tbl,
        type = c("p", "smooth"),
        scales = "free")

xyplot (stress.temp~year | region, group = region, data=temp.tbl,
        type = c("p", "smooth"),
        scales = "free")

# combine stress.tbl1 and GMC.kelp to create master dataframe.  
#new DF (DF.join) will have new columns: 'urchin_mn' & kelp_mn (from GMC.kelp) and 'stress.temp' from stress.tbl
#As I understand, these values will populate for the entire year/site combo.
DF.join <- full_join(temp.tbl, GMC.kelp, by=c('year', 'region'))

#Success??!!

#Run Model####

#working model: Kelp%Cover ~GMC Degree Days * GMC Urchin Threshold + 1|Region + 1|Year
#syntax is much different for gllvm package though.


              

##BELOW are notes and musings.  Largely where I'm getting stuck.

# MODEL GENERATION ####
# if I use the gllvm and compare via anova, my syntax should look like this:
# y =  ... a datafram
# x =  ... a dataframe
# TR = ... a dataframe
# gllvm(y, X, TR, formula = ~ Kelp Percent  + TempStress + Urchin Abundance )



#what are we trying to measure here...?   "Is Kelp Percent Cover by region a function of Temperature and Urchin Abundance?"  
    

#y in example = abundance data; kelp abundance
#X in exmaple = enviornmental data; temperature data
#TR in example = traits of ants.  





##NEXT: Before generating model, need better kelp data transformation, as kelp cover is highly non-linear and bound between 0 and 100.
# Beta regression a may improve for porportional data such as this 
#see Douma & Weedon (2019) in Methods Ecol Evol.
#use glmmTMB package for beta-regression transformation



\##Additional problems:  Which gllvm syntax to use for question being asked.
## how to make 'region' and 'year' random factors ('1|year' does not work within package) - STILL A PROBLEM (post JEB conversation)




