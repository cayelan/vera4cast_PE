library(tidyverse)
library(zoo)
library(RCurl)

source('R/get_targets.R')
source('R/PE_functions.R')
source('R/timeseries_functions.R')


# Get targets ---------------------------------------
# Get observational target data

fcre_EDI <- "https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce"
bvre_EDI <- "https://pasta.lternet.edu/package/data/eml/edi/725/4/9adadd2a7c2319e54227ab31a161ea12"

fcre_depths <- c(1.6, 9)
bvre_depths <- c(1.5, 13)

targets <- get_targets(infiles = c(fcre_EDI, bvre_EDI)) 

targets_P1D <- downsample(ts = targets, 
                          out_freq = 'daily', 
                          method = 'sample', 
                          target_out = '12:00:00',
                          max_distance = 2) # 2 hours either side

targets_PT1H <- downsample(ts = targets, 
                           out_freq = 'hourly', 
                           method = 'sample', 
                           target_out = '00:00',
                           max_distance = 40) # 40 minutes either side 

targets_P1W <- downsample(ts = targets, 
                          out_freq = 'weekly', 
                          method = 'sample', 
                          target_out = '3', # 3rd DOW ie Wednesday
                          max_distance = 1) # one day either side

# Get temperature profiles
temp_profiles <- 
  map2(c('none', 'none'), c(fcre_EDI, bvre_EDI), get_temp_profiles) |>
  list_rbind() |> 
  filter(year(datetime) %in% c(2019, 2020, 2021, 2022, 2023))


strat_dates <- calc_strat_dates(density_diff = 0.1, temp_profiles = temp_profiles) |> 
  mutate(start = as_date(start),
         end = as_date(end))

# Plot the observations
targets_P1D |> 
  filter(site_id == 'bvre') |> 
  ggplot(aes(x=yday(date), y=observation, colour = as_factor(year(date)))) +
  geom_line() +
  facet_wrap(variable~depth_m, scales = 'free_y')

# =====================================================#

# Set parameters for PE calculations
D  <- 4 # length of the word, embedding dimension
tau <- 1 # embedding time delay
window_length <- 30 # for a rolling PE how long should the time series be
resample_n <- 50 # how many samples? for the resampled PE

#======================================================#

