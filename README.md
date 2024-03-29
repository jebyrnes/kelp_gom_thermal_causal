# Code and Data from Suskiewicz, T., Byrnes, J.E.K., Steneck, R.S., Russell, R., Wilson, C.J., Rasher, D.B. 2024. Ocean warming undermines the recovery resilience of New England kelp forests following a fishery-induced trophic cascade. Ecology.

## To Replicate our Analysis

In order to replicate analysis from the paper, code in the scripts folder is enumerated in the order it should be run. We have tried to provide brief overviews and comment our code in order to facilitate understanding of precisely what we did. The code was developed via a back and forth between authors Rasher and Byrnes in order to make sure lead authors knew precisely what we were doing and why.

For specific questions, feel free to contact [jarrett.byrnes\@umb.edu](mailto:jarrett.byrnes@umb.edu){.email} - and we welcome any pull requests with additional code comments that users find helpful to better understand what we did.

## Directories

-   `scripts`: contains all of the R scripts to run our analytic workflow from start to finish. Scripts are put in numeric order to run to replicate our analyses and figures. Scripts without a number contain code used across multiple other scripts or are for the Shiny apps.

-   `raw_data`: contains the raw unprocessed data used for all analyses. Data consists of

    -   NOAA buoy data for temperature across all regions in `NOAA_temperature_allsites_2001_2018.csv`.
    -   Identifying information for those buoys in `buoyID.csv`.
    -   Data from Maine Department of Marine Resources (DMR) surveys of urchins and benthic algae in `DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv`
    -   Data from Rasher and Steneck lab surveys of algae and urchins in a series of files labelled `Rasher_Steneck_benthicsurvey_algae` followed by information on depths and years in csv format.
    -   Diameters of urchins from the Maine DMR surveys in `Urchin Diameters 2001-2018.xlsx`.

-   `derived_data`: data products created by initial data processing scripts.

    -   `combined_bio_data.csv` - the combined DMR/Rasher/Steneck biological data set
    -   `combined_data_for_analysis.csv`- the combined DMR/Rasher/Steneck biological data set with derived temperature data from regional buoys
    -   `compositional_change_data.csv`- Rasher/Steneck benthic algal community composition data set for `gllvm`
    -   `oisst_temp_data.csv`- data for the Gulf of Maine obtained from the NOAA Optimum Interpolation SST product v. 2.1 <https://www.ncei.noaa.gov/products/optimum-interpolation-sst>
    -   `rasher_steneck_combined.csv` - the combined Rasher/Steneck biological data set
    -   `temp_timeseries.csv` - the regional buoy temperature time series, including interpolated values

-   `model_output` - saved model outputs in `.rds` file format (see `?saveRDS` in R) for ease of using the same fit model by multiple scripts.

-   `figures` - figure output from analyses and data visualization

-   `tables` - tables output from analyses and data exploration

-   `markdown` - a folder containing a script exploring the causal model in more detail for early communication between co-authors. Results might be out of date to the current manuscript, but we leave it for those with some interest in ways we explored our model outputs.

- `shiny` - see below

## Shiny App

For the purposes of understanding when and where different sites were sampled, we provide an R Shiny app in the folder `shiny`. This app visualizes a map of the coast of Maine with a selector to scroll to different years. Regions are also color coded. A running version of the app can be found at [https://shiny.umb.edu/shiny/users/jarrett.byrnes/gom_kelp_sampling/](https://shiny.umb.edu/shiny/users/jarrett.byrnes/gom_kelp_sampling/) at time of publication. If this is down and you wish to run the app, but are unfamiliar with Shiny, you can run the app by opening `app.R` in R. Make sure you have the requisite libraries installed listed in the first few lines of the app. You can then copy and paste the entire script into your R console (or if in RStudio either run it or click the "Run App" button) in order to run the script.

For more about shiny, see [https://shiny.posit.co/](https://shiny.posit.co/).


## Funding

Funding for this project comes form the 2018-2020 Maine Sea Grant [R-18-03](https://seagrant.umaine.edu/research/projects/r-18-03-the-return-of-maines-kelp-forests-patterns-drivers-and-implications-for-industry/): The return of Maine’s kelp forests: patterns, drivers, and implications for industry to Doug Rasher, Robert Steneck, and Thew Suskiewicz

## Contact

For more information, contact [jarrett.byrnes\@umb.edu](mailto:jarrett.byrnes@umb.edu)
