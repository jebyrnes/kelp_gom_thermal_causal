##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  Maine Sea Grant 2019                                ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~  ©Thew Suskiewicz - (start) March 29, 2019             ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~                      (last edited) 8/08/2019            ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~                                                         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~ Function: Panultimate script for all figures for Seaweed/Royal B manuscript.   
##~~##~~##~~##~~ At completion script should be uploadable for journal reviewers and casual observers to use.
##~~##~~##~~##~~
##~~##~~##~~##~~ Notes;   CORRECT NOW THAT FIGURES HAVE BEEN RENUMBERED!                                                                  ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~    (Fig 1):      Map of sites (done in GIS)
##~~##~~##~~##~~    (Fig 2):      Urchin Density within each region (DMR random sites)                                                                        ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~    (Fig 3a,b,c): Water Temp data for a)summer ave, b)cumulative days above 17 & c) stress-degree days (above 1&)         ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~    (Fig 3a, b):  Total Kelp/Understory cover @ 5m                                                   ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~    (Fig S.1a,b): Total Kelp/Understory cover @ 10m
##~~##~~##~~##~~    (Fig S 2a-d): Total % cover from DMR annual survey data
##~~##~~##~~##~~                                                        ##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~##~~
#Top####

rm(list=ls()) # Clears the R brain. 

#set working directory and load data
setwd("~/Desktop/data_depot/sg2018")
getwd()

#load necessary packages
library(tidyverse)
library(ggpubr)
library(car) #allows levene's test for homogeneity


#Figure 2 - DMR Urchin Data
#Fig 2 - Urchins####
#load in relevant dataframe
dmr<- read.csv("data_files/DMR_random.csv", header=TRUE)

#check structure & summary
str(dmr)
summary(dmr)

#Create unique df for 5m, 10m and 15m depth zones. Outercoastal & Exposed sites only.
#set order of dmr factor 'region' for plotting purposes
dmr$region <- factor(dmr$region, levels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"), order = TRUE)
#Relabel subregions to be capitalized
levels(dmr$region) <- c("York", "Casco", "Midcoast", "Pen Bay", "MDI", "Downeast")

dmr5m <- dmr %>% 
  filter(exposure > 2,  coastal > 2, Depth.stratum.code ==1) %>%
  group_by(region, Year) %>% 
  summarise(mean_urchin = mean(AvgOfUrchin.count, na.rm=TRUE), sd_urchin = sd(AvgOfUrchin.count),
            mean_kelp = mean(AvgOfCanopy.percent, na.rm=TRUE), sd_kelp = sd(AvgOfCanopy.percent),
            mean_under = mean(AvgOfUnderstory.percent,na.rm=TRUE), sd_under = sd(AvgOfUnderstory.percent),
            mean_cca = mean(AvgOfCrust.percent, na.rm=TRUE), sd_cca = sd(AvgOfCrust.percent)
  )

dmr10m <- dmr %>% 
  filter(exposure > 2,  coastal > 2, Depth.stratum.code ==2) %>%
  group_by(region, Year) %>% 
  summarise(mean_urchin = mean(AvgOfUrchin.count, na.rm=TRUE), sd_urchin = sd(AvgOfUrchin.count),
            mean_kelp = mean(AvgOfCanopy.percent, na.rm=TRUE), sd_kelp = sd(AvgOfCanopy.percent),
            mean_under = mean(AvgOfUnderstory.percent,na.rm=TRUE), sd_under = sd(AvgOfUnderstory.percent),
            mean_cca = mean(AvgOfCrust.percent, na.rm=TRUE), sd_cca = sd(AvgOfCrust.percent)
  )

dmr15m <- dmr %>% 
  filter(exposure > 2,  coastal > 2, Depth.stratum.code ==3) %>%
  group_by(region, Year) %>% 
  summarise(mean_urchin = mean(AvgOfUrchin.count, na.rm=TRUE), sd_urchin = sd(AvgOfUrchin.count),
            mean_kelp = mean(AvgOfCanopy.percent, na.rm=TRUE), sd_kelp = sd(AvgOfCanopy.percent),
            mean_under = mean(AvgOfUnderstory.percent,na.rm=TRUE), sd_under = sd(AvgOfUnderstory.percent),
            mean_cca = mean(AvgOfCrust.percent, na.rm=TRUE), sd_cca = sd(AvgOfCrust.percent)
  ) 

#plot - lineplot by year grouped by region

plot.urchins<- ggplot(data=dmr10m, aes(x=Year, y=mean_urchin, colour=region)) +
  geom_line()+
  geom_point()+
  ylim(0,15)+
  labs(x = 'Year', y = bquote('Urchin Abundance (#/m'^2*')'), colour="Sub-Region", labels = c("York", "Casco", "Midcoast", "Pen Bay", "MDI", "Downeast"))+
  scale_color_brewer(palette="Dark2")+
  geom_hline(yintercept=10, linetype="dashed", color = "black", size=0.8)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

plot.urchins + facet_grid(rows = vars(region))
#Fix: Add sd to each of the lines (i.e. sd_urchins)


#FIG 3a,b Kelp/Understory####
#read-in the data

every <- read.csv("data_files/everything.csv", header=TRUE)

#check structure
str(every)
summary(every)

#subset data for year, exposure, coastal, and depth
percent5m <- every %>%
  filter(exposure > 2, coastal > 2, year %in% c(2004, 2018), depth == 2)  %>% 
  group_by (region, year, latitude) %>% 
  rename(Year = year) %>%
  summarize(under_mean = mean(under),
            kelp_mean = mean(kelp),
            count = n())


#change year to a factor
percent5m$Year <- as.factor(percent5m$Year)

#use levels to change the order of the factor 'region'.
percent5m$region <- factor(percent5m$region, levels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"), order = TRUE)
#Relabel subregions to be capitalized
levels(percent5m$region) <- c("York", "Casco", "Midcoast", "Pen Bay", "MDI", "Downeast")

#STATS - two way anova
aov_5m <- aov(kelp_mean ~ region * year, data = percent5m)
summary(aov_5m)
TukeyHSD(aov_5m, which = "region")
plot(aov_5m, 1) #tests for homogeneity of variance assumption.  Points  16, 29 & 49 are identified as outliers
plot(aov_5m, 2) #test normality assumption
aov_residuals <- residuals(object = aov_5m) #extracts residuals
shapiro.test(x = aov_residuals ) #normalicy is < 0.05; ergo NORMALICY HAS BEEN VIOLATED.
library(car)
leveneTest(kelp_mean ~ region * year, data = percent5m) #if p value is < 0.05 we can assume variance across groups is not sig. different therefore homogeneity of variances.


#here's an alternative way of doing this.
model_kelp = lm(kelp_mean ~ region + year + region:year,
                data=percent5m)
Anova(model_kelp, type="II")


#plot (fig 3a)
p_kelp5m <-    ggplot(data = percent5m, mapping = aes(x=region, y=kelp_mean, group_by(region))) +
  geom_boxplot(alpha = 0.6, aes(color=Year))+
  geom_point(alpha=0.5, width = 0.1, position = position_jitterdodge(), aes(color=Year))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4, position = position_jitterdodge(), aes(color=Year, fill=Year))+
  labs(x="", y = "Kelp Abundance (% Cover)")+
  theme(axis.text.x=element_blank())+
  ylim(0,100)+
  ggtitle("(a)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


#plot (fig 3b)
p_under5m <-   ggplot(data = percent5m, mapping = aes(x=region, y=under_mean, group_by(region))) +
  geom_point(alpha=0.5, width = 0.1, position = position_jitterdodge(), aes(color=Year))+
  geom_boxplot(alpha = 0.6, aes(color=Year))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4, position = position_jitterdodge(), aes(color=Year, fill=Year))+
  labs(x="Region", y = "Understory Abundance (% Cover)")+
  ylim(0,100)+
  ggtitle("(b)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


library(gridExtra)

grid.arrange (p_kelp5m, p_under5m, nrow=2)


#plot these two side-by-side
#this is cowplot....  plot_grid (p_kelp5m , p_under5m, labels = c("(a)", "(b)"), hjust = -3.5, label_size = 16, ncol = 1)

## CHANGE - plot mean, SE and site means and drop the boxplots.


#Fig 4ac TEMP####
#load in the data

## FIX:  Get rid of 'Fig a" etc and just just (a) (b) (c)
##FIX Get rid of "variable" in legend
## FIX: condense Y axis to 20ºC on Fig a
#FIX alignment along vertical axis
#FIX get rid of x axis for Fig a and Fig b

combined <- read.csv ("data_files/gom_combined.csv", header=TRUE)
str(combined)



#rename the variables we are interested in
combined$variable <- factor(combined$variable,
                    levels = c("B01.1mc", "casco", "E01.1m", "F01.1m", "I01.1m"),
                    labels = c("York", "Casco", "Midcoast", "Pen Bay", "MDI"))




  
#Summer Ave Temp####
#select out only summer months
summer <- combined %>%
  filter (month %in% c("7", "8","9"), variable %in% c("Casco", "York", "Midcoast", "Pen Bay", "MDI"), year >2000) %>%
  select (c(date,
            year,
            month,
            variable, value)) 


ave_summer <- summer %>%
  group_by(year, variable) %>%
  summarize(mean_temp = mean(value))


p_fig4a<- ggplot(data=ave_summer,
                 aes(x=year, y=mean_temp, colour=variable)) +
  geom_line()+
  geom_point()+
  geom_smooth(method = "gam")+
  ylim(10,20)+
  xlab('') + ylab('mean summer temperature (ºC)') +
  labs(color="Sub-Region")+
  scale_color_brewer(palette="Dark2")+
  theme(axis.text.x=element_blank(), legend.position = "none")+
  geom_hline(yintercept=17, linetype="dotted", color = "black", size=0.8) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
  

# Days >17.0 --- aka fig 4b

#create DF which groups by year 
over17 <- combined %>% 
  filter (variable %in% c("Casco", "York", "Midcoast", "Pen Bay", "MDI"), value>17.0) %>% 
  group_by(year, variable) %>% 
  summarise(no_rows = length(year))

#plot total # of days each year above 17ºC
p_fig4b <- ggplot (data = over17,
                   mapping = aes (x = year, y = no_rows, color=variable)) +
  geom_point (aes(color=variable))+
  geom_line (aes(color=variable))+
  geom_smooth(method = "gam")+
  xlim(2001, 2018)+  
  xlab('') + ylab('cumulative days above 17ºC')+
  labs(color="Sub-Region")+
  theme(axis.text.x=element_blank(), legend.position = "none")+
  scale_color_brewer(palette="Dark2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

#Stress Days####
#use dplyr to select out only temperatures which exceed 17.0ºC
stress <- combined %>%
  filter(value > 17, year>2000, variable %in%c("Casco", "York", "Midcoast", "Pen Bay", "MDI")) 

stress$stress.temp <- stress$value - 17. # takes the value from the 'column'value'  and subtracts 17.0 to yield 'stress day' value

stress.tbl <- stress %>% 
  group_by(year, variable) %>% 
  summarise(stress.temp = sum(stress.temp))

#MDI (i.e. I01.1m) will not plot because there is just one value in 2012.  Create table and merge
mdi_df <- data.frame ("year" = 2001:2018, "stress.temp" = c(0,0,0,0,0,0,0,0,0,0,0,0.4,0,0,0,0,0,0), variable = "MDI")

stress.test <- merge(mdi_df, stress.tbl, all=TRUE) #merges the two data tables to create a single table for plotting
#names(stress.test)[1:3]<-c("year", "stress.temp", "region")

#reorder the factor 'region' to match the other temperature graphs
stress.test$variable <- factor(stress.test$variable, levels = c("York", "Casco", "Midcoast", "Pen Bay", "MDI"), order = TRUE)

p_fig4c<- ggplot(data=stress.test,
                 aes(x=year, y=stress.temp, colour=variable)) +
  geom_line()+
  geom_smooth(method = "gam")+
  ylim(0,200)+
  xlab('Year') + ylab('degree days (> 17ºC)')+
  labs(color="Sub-Region")+
  scale_color_brewer(palette="Dark2")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

grid.arrange (p_fig4a, p_fig4b, p_fig4c, nrow=3)

#plot in one row via cowplot
#
plot_grid (p_fig4a , p_fig4b, p_fig4c, align = 'v', labels = c("(a)", "(b)", "(c)"),hjust = -3.5, label_size = 16, ncol = 1)



#Suppl 5m####
percent5m <- every %>%
  filter(exposure > 2, coastal > 2, year %in% c(2004, 2018), depth == 2)  %>% 
  group_by (region, year, latitude) %>% 
  summarize(under_mean = mean(under),
            kelp_mean = mean(kelp),
            count = n())



#Supp 10m####
#use--- every <- read.csv("data_files/everything.csv", header=TRUE)
percent10m <- every %>%
  filter(exposure > 2, coastal > 2, year %in% c(1998, 2004, 2018), depth == 3)  %>% 
  group_by (region, year, latitude) %>% 
  rename(Year = year) %>%
  summarize(under_mean = mean(under),
            kelp_mean = mean(kelp),
            count = n())

#boxplot for Kelp and Understory at 10m.  Figure should be 1x2
#change year to as.factor to plot
percent10m$Year <- as.factor(percent10m$Year)

#use levels to change the order of the factor 'region'.
percent10m$region <- factor(percent10m$region, levels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"), order = TRUE)

#change names of levels to be capitalized
levels(percent10m$region) <- c("York", "Casco", "Midcoast", "Pen Bay", "MDI", "Downeast")






#plot (fig supp 1a)
p_kelp10m <-   ggplot(data = percent10m, mapping = aes(x=region, y=kelp_mean, group_by(region))) +
  geom_point(alpha=0.5, width = 0.1, position = position_jitterdodge(), aes(color=Year))+
  geom_boxplot(alpha = 0.6, aes(color=Year))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4, position = position_jitterdodge(), aes(color=Year, fill=Year))+
  labs(x="", y = "Kelp Abundance (% Cover)")+
  theme(axis.text.x=element_blank())+
  ylim(0,100)

#plot (fig suppl 1b)
p_under10m <-   ggplot(data = percent10m, mapping = aes(x=region, y=under_mean, group_by(region))) +
  geom_point(alpha=0.5, width = 0.1, position = position_jitterdodge(), aes(color=Year))+
  geom_boxplot(alpha = 0.6, aes(color=Year))+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4, position = position_jitterdodge(), aes(color=Year, fill=Year))+
  labs(x="Sub-Region", y = "Understory Abundance (% Cover)")

#plot these two side-by-side
plot_grid (p_kelp10m , p_under10m, labels = c("(a)", "(b)"), hjust = -3.5, label_size = 16, ncol = 1)


#Supp DMR kelp/Understory####
#use dmr random data
#order the 'region' factor so it plots west to east.
dmr10m$region <- factor(dmr10m$region, levels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"), order = TRUE)
dmr5m$region <- factor(dmr5m$region, levels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"), order = TRUE)

levels(dmr5m$region) <- c("York", "Casco", "Midcoast", "Pen Bay", "MDI", "Downeast")
levels(dmr10m$region) <- c("York", "Casco", "Midcoast", "Pen Bay", "MDI", "Downeast")


plot.kelp10m <- ggplot(data=dmr10m, aes(x=Year, y=mean_kelp, colour=region)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=50, linetype="dotted", color = "grey")+
  geom_errorbar(aes(ymin = (mean_kelp-sd_kelp), ymax = (sd_kelp+mean_kelp), width = 0.2))+
  ylim(0,100)+
  xlim(2002, 2018)+
  xlab('Year') + ylab('% kelp (10m DMR)')

plot.under10m <- ggplot(data=dmr10m, aes(x=Year, y=mean_under, colour=region)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=50, linetype="dotted", color = "grey")+
  geom_errorbar(aes(ymin = (mean_under-sd_under), ymax = (sd_under+mean_under), width = 0.2))+
  ylim(0,100)+
  xlim(2002, 2018)+
  xlab('Year') + ylab('% Understory (10m DMR)')

plot.kelp5m <- ggplot(data=dmr5m, aes(x=Year, y=mean_kelp, colour=region)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=50, linetype="dotted", color = "grey")+
  geom_errorbar(aes(ymin = (mean_kelp-sd_kelp), ymax = (sd_kelp+mean_kelp), width = 0.2))+
  scale_color_brewer(palette="Dark2")+
  ylim(0,100)+
  xlim(2002, 2018)+
  labs(color="Sub-Region")+
  xlab('Year') + ylab('Kelp Abundance (% Cover at 5m)')

plot.under5m <- ggplot(data=dmr5m, aes(x=Year, y=mean_under, colour=region)) +
  geom_line()+
  geom_point()+
  geom_hline(yintercept=50, linetype="dotted", color = "grey")+
  geom_errorbar(aes(ymin = (mean_under-sd_under), ymax = (sd_under+mean_under), width = 0.2))+
  scale_color_brewer(palette="Dark2")+
  ylim(0,100)+
  xlim(2002, 2018)+
  labs(color="Sub-Region")+
  xlab('Year') + ylab('Understory Abundance (% Cover at 5m)')

#plot using facet grid.
# plot.kelp10m + facet_grid(rows = vars(region)) + ggtitle("supp. 2b")
# plot.under10m + facet_grid(rows = vars(region)) + ggtitle("supp. 2d")
plot.kelp5m + facet_grid(rows = vars(region)) + ggtitle("(a)")
plot.under5m + facet_grid(rows = vars(region)) + ggtitle("(b)")

#Species Specific####
#load in complete datafile
species <- read.csv("data_files/percent_all.csv", header=TRUE)
str(species)

#subset data, filtering out year, coastal, exposure and keeping only certain columns which remain useful
df.5m <- species %>%
  filter (year %in% c(2004, 2018), coastal >2, exposure >2, depth == 5) %>%
  select (year, site, region, exposure, coastal, lat, long, depth, sac, agar, alar, ldig, desm, kelp, ulva, chaet, codm, poly, rhod, ptilo, porph, palm, phyc, ccrisp, under
  )

df.10m <- species %>%
  filter (year %in% c(1997, 2004, 2018), coastal >2, depth == 10) %>%
  select (year, site, region, exposure, coastal, lat, long, depth, sac, agar, alar, ldig, desm, kelp, ulva, chaet, codm, poly, rhod, ptilo, porph, palm, phyc, ccrisp, under
  )

#summarize findings by looking at mean value by region for each year
df5.region <- df.5m %>%
  group_by(region, year) %>% 
  summarise(mean_kelp = mean(kelp, na.rm=FALSE),
            mean_ldig = mean(ldig, na.rm=FALSE),
            mean_sac = mean(sac, na.rm=FALSE),
            mean_desm = mean(desm, na.rm=FALSE),
            mean_agar = mean(agar, na.rm=FALSE),
            mean_alar = mean(alar, na.rm=FALSE),
            mean_under = mean(under, na.rm=FALSE),
            mean_ptilo = mean(ptilo, na.rm=FALSE),
            mean_porph = mean(porph, na.rm=FALSE),
            mean_phyc = mean(phyc, na.rm=FALSE),
            mean_ccrisp = mean(ccrisp, na.rm=FALSE),
            mean_poly = mean(poly, na.rm=FALSE),
            mean_ulva = mean(ulva, na.rm=FALSE),
            mean_chaet = mean(chaet, na.rm=FALSE),
            n_obs=n_distinct(site))

#...and just by year
df5.all <- df.5m %>%
  group_by(year) %>% 
  summarise(mean_kelp = mean(kelp, na.rm=FALSE),
            mean_ldig = mean(ldig, na.rm=FALSE),
            mean_sac = mean(sac, na.rm=FALSE),
            mean_desm = mean(desm, na.rm=FALSE),
            mean_agar = mean(agar, na.rm=FALSE),
            mean_alar = mean(alar, na.rm=FALSE),
            mean_under = mean(under, na.rm=FALSE),
            mean_ptilo = mean(ptilo, na.rm=FALSE),
            mean_porph = mean(porph, na.rm=FALSE),
            mean_phyc = mean(phyc, na.rm=FALSE),
            mean_ccrisp = mean(ccrisp, na.rm=FALSE),
            mean_poly = mean(poly, na.rm=FALSE),
            mean_ulva = mean(ulva, na.rm=FALSE),
            mean_chaet = mean(chaet, na.rm=FALSE),
            n_obs=n_distinct(site))
#save results as its own csv file for further data-wrangling
write.csv(df5.region, file = "species_5m.csv")
write.csv(df5.all, file = "species_5m_all.csv")

#STATS#### 
#- two way anova for kelp & understory data

aov_5m <- aov(kelp_mean ~ year * region, data = percent5m)
summary(aov_5m)
TukeyHSD(aov_5m, ordered = TRUE)
plot(aov_5m, 1) #tests for homogeneity of variance assumption.  Points  16, 29 & 49 are identified as outliers
plot(aov_5m, 2) #test normality assumption
aov_residuals <- residuals(object = aov_5m) #extracts residuals
shapiro.test(x = aov_residuals ) #normalicy is > 0.05; ergo NORMALICY HAS BEEN VIOLATED.
library(car)
leveneTest(kelp_mean ~ region * year, data = percent5m) #if p value is < 0.05 we can assume variance across groups is not sig. different therefore homogeneity of variances.

#below synax breaks up the vsarious TukeyHSD tests, but this does not eliminate the ones we don't want.  Also, no differences between years recorded for region (seems fishy)
TukeyHSD(aov(kelp_mean~year + region + region:year, data = percent5m))

#here's an alternative way of doing this.
model_kelp = lm(kelp_mean ~ region + year + region:year,
                data=percent5m)
Anova(model_kelp, type="II")


#since we cannot analyze kelp ~ region * year without the YORK-2018 data, lets' just look at 2004 and 2018 individualy for S&G
#first, subset data for only 2004 and only 2018
percent5m_2004 <- every %>%
  filter(exposure > 2, coastal > 2, year==2004, depth == 2)  %>% 
  group_by (region, year, latitude) %>% 
  summarize(under_mean = mean(under),
            kelp_mean = mean(kelp),
            count = n())

percent5m_2018 <- every %>%
  filter(exposure > 2, coastal > 2, year==2018, depth == 2)  %>% 
  group_by (region, year, latitude) %>% 
  summarize(under_mean = mean(under),
            kelp_mean = mean(kelp),
            count = n())

#run one-way ANOVA on each
aov_5m_2004 <- aov(kelp_mean ~ region, data = percent5m_2004)
summary(aov_5m_2004)
TukeyHSD(aov_5m_2004, which = "region")
plot(aov_5m_2004, 1) #tests for homogeneity of variance assumption. 
plot(aov_5m_2004, 2) #test normality assumption

leveneTest(kelp_mean ~ region * year, data = percent5m_2004)


aov_5m_2018 <- aov(kelp_mean ~ region, data = percent5m_2018)
summary(aov_5m_2018)
TukeyHSD(aov_5m_2018, which = "region")
plot(aov_5m_2018, 1) #tests for homogeneity of variance assumption. 
plot(aov_5m_2018, 2) #test normality assumption

#Adjust P-values for Multiple Comparisons#
#e.g. Bonferroni Correction (which multiplies p values * number of comparisons)
# USAGE: p.adjust(p, method = p.adjust.methods, n = length(p))


# logit transformations####
#we want to transform kelp percentage values (arcsin)
percent5m$kelp_percent <- (percent5m$kelp_mean)/100 #creates column 'kelp_percent' which transforms values to 0 - 1 format (0% to 100%)

percent5m$logit_kelp <- logit(percent5m$kelp_mean) #logit transforms 'kelp_percent' data, hopefully normalizing data
summary(percent5m)

sum(percent5m$kelp_mean >100)

aov_5mTrans <- aov (asin_kelp ~ region * year, data = percent5m)
summary(aov_5mTrans)
plot(aov_5mTrans, 1)
plot(aov_5mTrans, 2)

aov_residuals <- residuals(object = aov_5mTrans) #extracts residuals
shapiro.test(x = aov_residuals ) #tests for normalicy.  If p > 0.05 Normalicy has been violated. Residuals are normally distributed

aov_5mTrans <- aov (logit_kelp ~ region * year, data = percent5m)

#boneyard####

#gg ridgeplots####

#suggested replacement for figure 3, possible including 10m (in a 2by figure)
#sample code by JEKB, and made at his suggestion

#add package #ggridges
library(ggplot2)
library(ggridges)

demo_ridges <-percent5m, (region = factor(c("York", "Casco", "Midcoast", "Penbay", "MDI", "Downeast")),
                        Year = c(2004, 2018),
                          rep = 1:100) %>%
  mutate(value = rnorm(n(), 
                       year-1990 + 10*as.numeric(factor(region)), 
                       2),
         region = fct_rev(region))

#sample code
demo_dat <- crossing(region = factor(c("south", "north")),
                     year = c(1999, 2008, 2010),
                     rep = 1:100) %>%
  mutate(value = rnorm(n(), 
                       year-1990 + 10*as.numeric(factor(region)), 
                       2),
         region = fct_rev(region))

ggplot(demo_dat,
       aes(x = value, y = region, fill = factor(Year))) +
  stat_density_ridges(alpha = 0.9) +
  theme_bw() +
  scale_fill_manual(values = c("darkblue", "blue", "aquamarine"))


#sample code form cran.r project

percent5m %>%
  mutate(YearFct = fct_rev(as.factor(Year)))

percent5m$region <- factor(
  percent5m$region,
  levels = c("york", "casco", "midcoast", "penbay", "mdi", "downeast"),
  labels = c("York", "Casco Bay", "Mid-coast", "Penobscot Bay", "MDI", "Downeast")
)

ggplot(percent5m, aes(x=kelp_mean, y=region, color=Year, point_color=Year, fill=Year, from = 0, to = 100)) +
  geom_density_ridges(scale = .95, rel_min_height = .01, jittered_points=TRUE, scale = .95, rel_min_height = .01,
                      point_shape = "|", point_size = 3, size = 0.25,
                      position = position_points_jitter(height = 0)) +
  scale_y_discrete(expand = c(.01, 0))+
  scale_x_continuous(limits =c(0,100), name = "Kelp Cover") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("2004", "2018")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none")+
  ggtitle("Average Kelp Cover by Region at 5m") +
  theme_ridges(center = TRUE)

#change Year to be a factor (for plotting)
percent5m$Year <- as.factor(percent5m$Year)

rm(demo_dat)

