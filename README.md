# Permutation Entropy for VERA

## Workflow scripts
These need to be run in order and it doesn't save any intermediate files, so run every session. 

1. 01_generate_targets.R - script to download data from EDI and down-sample, interpolate etc., and generate shuffled realisations of the datasets 

2. 02_calculate_PE.R - uses the processed targets from script 01 to calculate the PE values needed in the plots (timeseries of PE from a moving window, PE of shuffled realisations, PE of total time series)

3. 03_plots.R - generates manuscript plots

These scripts require custom functions to be loaded. 

## R functions

- find_depths.R is a copy of the function used in the VERA targets generation in order to figure out the depths of the sensors at BVR.

- get_targets.R has the functions for downloading the data from EDI and doing some initial processing to get the data into a long format and subset based on the target variables for this analysis.

- PE_functions.R contains all the functions to calculate PE from the processed EDI data. Functions for PE calculations that return a single PE value for time-series or using a moving window to generate a time serie of PE values.
The functions require at least 30 non-NA observations to calculate PE - otherwise returns NA for that time step. 

- timeseries_function.R contains `resample()`, `downsample()`, `shuffle()`, and `season()` functions that manipulate the raw time series for use in the PE caluclations.
