# Data from Suskiewicz, T., Byrnes, J.E.K., Steneck, R.S., Russell, R., Wilson, C.J., Rasher, D.B. 2024. Ocean warming undermines the recovery resilience of New England kelp forests following a fishery-induced trophic cascade. Ecology.

Below, find the metadata for columns in each raw data file.

### DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv

-   year - Integer for year.
-   date - Date in M/D/Y format.
-   site.number - Integer identifier for site. Numbers were reused
    between years, but do not correspond to the same sites.
-   region.code - Integer code of region. Controlled vocabulary matches
    the region column.
-   region - Character identifier for name of region on the coast of
    Maine.
-   exposure.code - Integer identifier for 1-5 qualitative scale of
    total arc (in degrees) where a site was exposed to the open ocean.
-   coastal.code - Integer identifier for 1-5 qualitative scale of a
    site's - coastal status. Exposed mainland coast at a level of 3.
    Offshore islands have higher scores and sites within rivers or
    estuaries with lower scores.
-   latitude - Latitude in decimal degrees.
-   longitude - Longitude in decimal degrees.
-   depth.stratum.code - Integer identifier of depth where survey was
    conducted. 1 = 5m depth, 2 = 10m depth.
-   depth - Integer of depth below mean low lower water in meters.
-   crust - Percent cover of crustose coralline algae in a 1m\^2
    quadrat..
-   understory - Percent cover of fleshy non-kelp algae in a 1m\^2
    quadrat..
-   kelp - Percent cover of the kelps *Saccharina latissima*, *Laminaria
    digitata*, *Alaria esculenta*, and *Agarum clathratum* (order
    Laminariales) as well as two *Desmarestia* species in a 1m\^2
    quadrat.\
-   urchin - Number of urchins in a 1m\^2 quadrat.

### Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2016.csv

### Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2017.cs

### Rasher_Steneck_benthicsurvey_algae_urchin_allsites_alldepths_2018.csv

Note: for all, nr = Not Recorded. For substrate classifications, see
Wentworth (1922) for defitinions.

-   year - Integer for year.
-   date - Date in M/D/Y format.
-   site.number - Integer identifier for site. Numbers were reused
    between years, but do not correspond to the same sites.
-   region.code - Integer code of region. Controlled vocabulary matches
    the region column.
-   region - Character identifier for name of region on the coast of
    Maine.
-   exposure.code - Integer identifier for 1-5 qualitative scale of
    total arc (in degrees) where a site was exposed to the open ocean.
-   coastal.code - Integer identifier for 1-5 qualitative scale of a
    site's - coastal status. Exposed mainland coast at a level of 3.
    Offshore islands have higher scores and sites within rivers or
    estuaries with lower scores.
-   latitude - Latitude in decimal degrees.
-   longitude - Longitude in decimal degrees.\
-   depth - Numeric depth in m.
-   quadrat - Integer ID of quadrat along a transect
-   time - HH:MM time of day of dive.
-   sand - Percent cover in 1m\^2 quadrat of sand.
-   pebble - Percent cover in 1m\^2 quadrat of pebbles.
-   rock - Percent cover in 1m\^2 quadrat of solid rock.
-   cobble - Percent cover in 1m\^2 quadrat of cobbles.
-   boulder - Percent cover in 1m\^2 quadrat of boulder.
-   ledge - Percent cover in 1m\^2 quadrat of ledge habitat.
-   sac - Percent cover in 1m\^2 quadrat of *Saccharina latissima*.
-   alar - Percent cover in 1m\^2 quadrat of *Alaria esculenta*.
-   agar - Percent cover in 1m\^2 quadrat of *Agarum clathratum*.
-   ldig - Percent cover in 1m\^2 quadrat of *Laminaria digitata*.
-   sder - Percent cover in 1m\^2 quadrat of *Saccorhiza dermatodea*.
-   desm - Percent cover in 1m\^2 quadrat of *Desmarestia spp.*.
-   kelp - Percent cover in 1m\^2 quadrat of summed canopy brown algae
    (sac - desm)
-   ulva - Percent cover in 1m\^2 quadrat of *Ulva spp.*
-   chaet - Percent cover in 1m\^2 quadrat of *Chaetomorpha linum*.
-   codm - Percent cover in 1m\^2 quadrat of *Codium fragile*.
-   poly - Percent cover in 1m\^2 quadrat of *Polysiphonia spp.*.
-   rhod - Percent cover in 1m\^2 quadrat of *Rhodomela spp.*.
-   ptilo - Percent cover in 1m\^2 quadrat of *Ptilota serrata*.
-   porph - Percent cover in 1m\^2 quadrat of *Prophyra spp.*.
-   palm - Percent cover in 1m\^2 quadrat of *Palmaria palmata*.
-   phyc - Percent cover in 1m\^2 quadrat of *Phycodurus rubens*.
-   ccrisp - Percent cover in 1m\^2 quadrat of *Chondrus crispus*.
-   callo - - Percent cover in 1m\^2 quadrat of *Callophyllis spp.*.
-   phyll - Percent cover in 1m\^2 quadrat of *???*
-   coral - Percent cover in 1m\^2 quadrat of *Corallina officionalis*.
-   bonne - Percent cover in 1m\^2 quadrat of *Bonnemaisonia hamifera*.
-   cystoc - Percent cover in 1m\^2 quadrat of *Cystoclonium purpureum*.
-   crust - Percent cover in 1m\^2 quadrat of crustose coralline algae.
-   urchin - Number of urchins in 1m\^2 quadrat.

#### byrnes_2018_appledore.csv

-   NETWORK - Character name of group doing the sampling.
-   PI - Character name of PI leading the sampling effort.
-   YEAR - Integer year.
-   MONTH - Integer month (1-12).
-   DAY - Integer day of month.
-   SITE - Character identifier of site sampled around Appledore Island,
    ME.
-   TRANSECT - Character identifier of transect corresponding to
    features of Appledore Island, ME.
-   SP_CODE - Character identifier of species/substrate sampled.
    Controlled vocabulary. See taxonomy in file to translate.
-   PERCENT_COVER - Percent cover as determined by 80 point counts along
    a 40m transect. Two points were sampled every meter, with 1 point
    0.5m to either side of the transect line. Cover is determined as
    number of points of a species/substrate divided by 80.
-   GROUP - Character identifier of lumped group. Invertebrate, Algae,
    or Substrate.
-   DIVISION.FAMILY - Character identifier used for grouping based on
    coarse taxonomy. Mixes between phylum, class, and order.
-   COMMON.DIVISION.NAME - Character identifier of common vernacular
    name of coarse taxonomic division.
-   SIZE - Character of whether an organism was an adult or juvenile or
    had another size class identifier. Different SP_CODE entries for
    some species correspond to different age or size classes.
-   COMMON.NAME - Character common name of species or substrate.
-   KINGDOM - Character of organism Kingdom, as determined by the World
    Registry of Marine Species.
-   PHYLUM - Character of organism phylum, as determined by the World
    Registry of Marine Species. NA if not an organism.
-   CLASS - Character of organism class, as determined by the World
    Registry of Marine Species. NA if not an organism.
-   ORDER - Character of organism order, as determined by the World
    Registry of Marine Species. NA if not an organism.
-   FAMILY - Character of organism family, as determined by the World
    Registry of Marine Species. NA if not an organism.
-   GENUS - Character of organism genus, as determined by the World
    Registry of Marine Species. NA if not an organism.
-   SPECIES - Character of organism species, if identified to species
    level, as determined by the World Registry of Marine Species. For
    multispecies groupings, will be Byrnes lab name for multispecies
    grouping (e.g., Red Algal Turf). NA if not an organism.

### Urchin Diameters 2011-2018.xlsx

Year - Integer of year. Latitude - Decimal degrees of Latitude.
Longitude - Decimal Degrees of Longitude. Region - Integer region code
matching regions in
DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv. Site
number - Site number for year sampled matching site number in
DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv.
Sentinel number - Character code for DMR sentinel sites. Depth stratum
code - Depth stratum code corresponding to depth stratum code in
DMR_benthicsurvey_algae_urchin_randomsites_alldepths_2001_2018.csv.
Diameter - Integer urchin diameter in millimeters.

#### NOAA_temperature_allsites_2001_2018.csv

-   date - M/D/Y date.
-   month - Integer of month (1-12).
-   year - Integer of year.
-   variable - variable code for measurement. Includes buoy ID and depth
    of temperature measurement.
-   value - Numeric temperature in degrees C. NA = missing value.

### buoyID.csv

-   buoyID - Character ID code for each buoy according to National Buoy
    Data Center <https://www.ndbc.noaa.gov/>

-   latitude - Latitude in decimal degrees.

-   longitude - Longitude in decimal degrees.
