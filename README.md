# Seagrant-UrchinKelp2018

This is code, data and associated files for the Rasher-Suskiewicz SeaGrant project from 2018-2019. 

It includes additional data mined from Department of Marine Resources (ME) and Dr. Robert Steneck's long term surveys plus a few years of Walter Adey (Smithsonian) algal biomass sampling.

## Data Files

- dmr_random.csv - DMR random urchin surveys which include algal surveys at the level of kelp and understory, not at the species level, 2001-2018. This includes all depths with strata
     - Depth strata code is 1,2,3, which corresponds to 5m, 10m, 15m

- dmr.csv - Subset of dmr_random, filtered to only the 5m depth strata 
  
- Spitball.R - Thew's testing code. Can be ignored.

- algae_biomass.csv - Quadrat biomass data from 2016-2018 measured to species sampled by the Rasher lab     

- gom_combined.csv - temperature data from NOAA/Neracoos buoys combined into one master file.  

- stenecksg.csv - algal/urchin surveys conducted by Steneck, Adey, Rasher and Suskiewicz (various years). Algae measured as percent cover by species.

## Code Files

- github_sg.R - main analysis script. Models of kelp as predicted by temperature and urchins.

- manuscript_seaweedRoyalB_edit.R - R Script that makes figures for manuscript. Temperature data plotted, kelp and understory data plotted. All figures for DRAFT version of manuscript to Royal Academy B


Comments herein are largely from TSS (suskiewicz.thew@gmail.com) with input from DBR and JEB.
