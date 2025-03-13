# Intrinsic predictability analysis for VERA (Virginia Reservoir Ecosystem Analysis)

This repository provides the code needed to reproduce the analyses in the manuscript "High-frequency monitoring data reveal substantial variability in the intrinsic predictability of ecosystem dynamics" by Carey, Olsson, Breef-Pilz, and Thomas.

## Workflow scripts

These files need to be run in order and do not create any intermediate files, so they should all be run every session.

1.  `01_generate_targets.R` - script to download data from EDI and down-sample, interpolate targets data etc., and generate shuffled realisations of the datasets.

2.  `02_calculate_PE.R` - uses the processed targets from script 01 to calculate the PE values needed in the plots (timeseries of PE from a moving window, PE of shuffled realisations, PE of total time series).

3.  `03_plots.R` - generates manuscript plots.

4.  `04_mixed_effects_model.R` - fits a number of mixed effects models to identify differences among reservoirs, depths, and variables.

5.  `05_gams.R` - fits a number of generalised additive mixed models to identify differences in seasonality of PE dynamics among reservoirs.

6.  `06_analyses_for_text.R` - generates numerical values that are included in the main text.

These scripts require custom R functions to be loaded that are sourced in the 01 script.

## R functions

-   `find_depths.R` is a copy of the function used in the VERA targets generation to figure out the depths of the sensors at Beaverdam Reservoir.

-   `get_targets.R` has the functions for downloading the data from EDI and doing some initial processing to get the data into a long format and subset based on the target variables for this analysis.

-   `PE_functions.R` contains all the functions to calculate PE from the processed EDI (Environmental Data Initiative) data. Functions for PE calculations that return a single PE value for time-series or using a moving window to generate a time series of PE values. The functions require at least 30 non-NA observations to calculate PE - otherwise returns NA for that time step.

-   `timeseries_function.R` contains `resample()`, `downsample()`, `shuffle()`, and `season()` functions that manipulate the raw time series for use in the PE calculations.

## SI analysis

This directory (SI_analysis) contains Rmd scripts to carry out supplementary analysis:

-   PE calculations on the daily *averaged* data
-   PE calculations using *different hyperparameters* (on the downsampled data)

If you have any questions about the analysis, please email Freya Olsson (freyao@vt.edu) and Cayelan Carey (cayelan@vt.edu).
