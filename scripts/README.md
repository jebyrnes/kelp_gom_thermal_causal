# Code and Data from Suskiewicz, T., Byrnes, J.E.K., Steneck, R.S., Russell, R., Wilson, C.J., Rasher, D.B. 2024. Ocean warming undermines the recovery resilience of New England kelp forests following a fishery-induced trophic cascade. Ecology.

## Scripts

In order to replicate analysis from the paper, code in this scripts
folder is enumerated in the order it should be run. We have tried to
provide brief overviews and comment our code in order to facilitate
understanding of precisely what we did. The code was developed via a
back and forth between authors Rasher and Byrnes in order to make sure
lead authors knew precisely what we were doing and why.

The scripts are as follows

### For analysis:

`1_process_combine_data.R` - Processes kelp data from different sources
as well buoy temperature data. Combines into a single source CSV for
analysis.

`1a_interpolate_downeast_temps.R` - Interpolates missing buoy sea
surface temperatures using SST from adjacent buoys and statistical fits.

`1b_make_site_buoy_map.R` - Makes Fig. 1, our sites sampled and buoy
locations.

`2_load_combined_data.R` - Script used to take combined CSV and add
transformations and nicer region names for analysis and visualization.

`2a_desmerestia.R` - Supplementary analysis looking at Desmarestia as a fraction of brown algae.

`3_timeseries_model.R` - Script that models and visualizes biological
time series data.

`3b_temperature_timeseries.R` - Script that models and visualizes
temperature data.

`4_model_kelp_data.R` - Script to fit causal models to kelp forest data
according to methods of [Byrnes and Dee
2024](https://www.biorxiv.org/content/10.1101/2024.02.26.582072v1).

`5_model_viz.R` - Script to visualize causal model results as well as
counterfactual predictions.

`6_make_compositional_data.R` - Script to take subtidal sampling data
and generate a unified dataset to explore benthic algal community
composition.

`7_compositional_plots.R` - Plot benthic algal community data for
visualization.

`8_composition_gllvm.R` - GLLVM models to analyze benthic kelp and algal
community composition data.

`9_compositional_model_viz.R` - Visualizations from fit GLLVM models of
community composition.

`10_viewable_site_map.R` - Makes a `leaflet` map of sites sampled to aid
in exploration of geographic extent of the analysis.

`12_supplementary_figures_analysis.R` - Scripts to generate
visualizations of our results using either the 10m data, as opposed ot
the 5m data used for the main text, or to use the OISST data instead of
the buoy data. Figures go into supplement.

`13_urchin_sizes.R` - Requested script by reviewer to look at change in
urchin size distribution over time.

### Helpers:

`anova_gllvm_tmp.R` - Script with functions to automate model comparison
for GLLVM fits.

`get_gllvm_predictions_sim.R` - Script with function to get predicted
values from GLLVM fits.

`oisst_fetch.R` - Script to get OISST data from NOAA.

### Additional Scripts:

`make_geodata.R` - Script to turn combined data into an `sf` object for
later use by other scripts and apps.

`make_kelp_heatmap.R` - Make heatmap of kelp results for markdown files.

`sample_properties.R` - Script to explore properties and sample sizes of
DMR and Rasher/Steneck subtidal sampling.
