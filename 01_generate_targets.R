library(tidyverse)
library(zoo)
library(RCurl)
library(lubridate)

source('R/get_targets.R')
source('R/PE_functions.R')
source('R/timeseries_functions.R')


# Get targets ---------------------------------------
# Get observational target data

fcre_EDI <- "https://pasta.lternet.edu/package/data/eml/edi/271/9/f23d27b67f71c25cb8e6232af739f986"
bvre_EDI <- "https://pasta.lternet.edu/package/data/eml/edi/725/5/f649de0e8a468922b40dcfa34285055e" 

fcre_depths <- c(1.6, 9)
bvre_depths <- c(1.5, 13)
# these are the top and bottom depths of each reservoir

targets <- get_targets(infiles = c(fcre_EDI, bvre_EDI),
                       is_met = c(F,F),
                       interpolate = T, maxgap = 12) |> 
  mutate(depth_m = ifelse(depth_m < 5, 'surface', ifelse(depth_m > 5, 'bottom', NA))) 

# targets_PT1H <- downsample(ts = targets, 
#                            out_freq = 'hourly', 
#                            method = 'sample', 
#                            target_out = '00:00',
#                            max_distance = 20) # 20 minutes either side 

targets_P1D <- downsample(ts = targets, 
                          out_freq = 'daily', 
                          method = 'sample', 
                          target_out = '12:00:00',
                          max_distance = 2) # 2 hours either side

targets_P1D_av <- downsample(ts = targets, 
                             out_freq = 'daily', 
                             method = 'aggregate')

# interpolation
targets_P1D_interp <- targets_P1D |> 
  na.omit() |> 
  tsibble::as_tsibble(index = date, key = c(site_id, variable, depth_m)) |> 
  tsibble::fill_gaps() |> 
  as_tibble() |> 
  group_by(variable, site_id, depth_m) |> 
  arrange(date) |> 
  mutate(observation = zoo::na.approx(observation, na.rm = T, maxgap = 3)) |> ungroup()

targets_P1D_av_interp <- targets_P1D_av |> 
  na.omit() |> 
  tsibble::as_tsibble(index = date, key = c(site_id, variable, depth_m)) |> 
  tsibble::fill_gaps() |> 
  as_tibble() |> 
  group_by(variable, site_id, depth_m) |> 
  arrange(date) |> 
  mutate(observation = zoo::na.approx(observation, na.rm = T, maxgap = 3)) |> ungroup()

# make it consistent across reservoirs
start_date <- targets_P1D_interp |> 
  reframe(.by = c(variable, depth_m, site_id), 
          start = min(date, na.rm = T)) |> 
  reframe(last_start = max(start)) |> 
  pull(last_start)

targets_P1D_interp <- filter(targets_P1D_interp, date > start_date)
targets_P1D_av_interp <- filter(targets_P1D_av_interp, date > start_date)

# Get temperature profiles
temp_profiles <- 
  map2(c('none', 'none'), c(fcre_EDI, bvre_EDI), get_temp_profiles) |>
  list_rbind() |> 
  filter(year(datetime) %in% c(2021, 2022, 2023, 2024))


strat_dates <- calc_strat_dates(density_diff = 0.1, temp_profiles = temp_profiles) |> 
  mutate(start = as_date(start),
         end = as_date(end))


# Resample timeseries
# resample_length <- 50 # how long are the sections?
# resample_n <- 500 # how many times should the timeseries be sampled
# targets_P1D_resample <- targets_P1D |>
#   filter(year(date) >= 2021) |> # only the years where there are the same data
#   na.omit() |> 
#   tsibble::as_tsibble(index = date, key = c(variable, site_id, depth_m)) |> 
#   arrange(variable, depth_m, site_id, date) |> 
#   reframe(.by = c(variable, site_id, depth_m),
#           resample(ts = observation, 
#                    length.out = resample_length, 
#                    n = resample_n, 
#                    doy = date))

# Generate shuffles
targets_P1D_shuffled <- targets_P1D_interp |>
  na.omit() |> 
  tsibble::as_tsibble(index = date, key = c(variable, site_id, depth_m)) |> 
  arrange(variable, depth_m, site_id, date) |>
  reframe(.by = c(variable, site_id, depth_m),
          shuffle(ts = observation, times = 500))

targets_P1D_av_shuffled <- targets_P1D_av_interp |>
  na.omit() |> 
  tsibble::as_tsibble(index = date, key = c(variable, site_id, depth_m)) |> 
  arrange(variable, depth_m, site_id, date) |>
  reframe(.by = c(variable, site_id, depth_m),
          shuffle(ts = observation, times = 500))
