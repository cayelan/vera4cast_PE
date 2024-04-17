library(tidyverse)
library(zoo)
library(RCurl)

source('R/get_targets.R')
source('R/PE_functions.R')
source('R/timeseries_functions.R')


# Get targets ---------------------------------------
# Get observational target data

fcre_EDI <- "https://pasta.lternet.edu/package/data/eml/edi/271/8/fbb8c7a0230f4587f1c6e11417fe9dce"
bvre_L1 <- "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data-qaqc/bvre-waterquality_L1.csv"
bvre_EDI <- "https://pasta.lternet.edu/package/data/eml/edi/725/3/a9a7ff6fe8dc20f7a8f89447d4dc2038"
fcre_L1 <- 'none'

fcre_depths <- c(1.6, 9)
bvre_depths <- c(1.5, 13)

targets <- get_targets_P1D(fcre_file = fcre_EDI, bvre_file = c(bvre_L1, bvre_EDI)) |> 
  filter(year(datetime) %in% c(2019,2020, 2021, 2022, 2023))

# Get temperature profiles
temp_profiles <- 
  map2(c(fcre_L1, bvre_L1), c(fcre_EDI, bvre_EDI), get_temp_profiles) |>
  list_rbind() |> 
  filter(year(datetime) %in% c(2019,2020, 2021, 2022, 2023))


strat_dates <- calc_strat_dates(density_diff = 0.1, temp_profiles = temp_profiles)


# =====================================================#

# Set parameters for PE calculations
D  <- 3 # length of the word, embedding dimension
tau <- 1 # embedding time delay
window_length <- 30 # for a rolling PE how long should the time series be
resample_n <- 100 # how many samples? for the resampled PE

#======================================================#


# Summarise PE for full time series ---------------------------------------
# Summaries of PE by variable, site and depth
PE_byvar <- targets |>
  group_by(depth_m, variable, site_id) |> 
  summarise(PE = calculate_PE(observation)) 

PE_byvar |> 
  ggplot(aes(y = PE, x = variable, fill = as_factor(depth_m))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x='Variable', y = 'Normalised PE') +
  theme_bw() +
  facet_wrap(~site_id, nrow = 2) +
  scale_fill_viridis_d(option = 'C', name = 'Depth_m') +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))
# ----------------------------#


# Daily dataset resampling ------------------------------------------------
# Resample the time series to calculate the PE on subsets of the data to generate a distribution
targets_interp <-
  targets |>
  na.omit() |> 
  mutate(datetime = as_date(datetime)) |> 
  tsibble::as_tsibble(index = datetime, key = c(variable, site_id, depth_m)) |> 
  tsibble::fill_gaps(.full = FALSE) |> 
  as_tibble() |> 
  mutate(observation = imputeTS::na_interpolation(observation, 'linear', maxgap = 10, rule = 1))

targets_resample <- targets_interp |>
  arrange(variable, depth_m, site_id, datetime) |> 
  reframe(.by = c(variable, site_id, depth_m),
          resample(ts = observation, length.out = 100, n = 100))

PE_resampled <- targets_resample |> 
  group_by(n, site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation), .groups = 'drop')

PE_resampled |> 
  mutate(depth_m = as_factor(depth_m)) |> 
  ggplot(aes(x=PE, fill = depth_m, y = after_stat(ndensity))) +
  geom_histogram(bins = 15, position = 'identity', alpha = 0.5) +
  # geom_freqpoly(bins = 30, position = 'identity', aes(colour = depth_m)) +
  facet_grid(variable~site_id) +
  scale_fill_viridis_d(option = 'C', name = 'Depth_m') +
  scale_colour_viridis_d(option = 'C', name = 'Depth_m') +
  theme_bw()

#========================================#

# Time series of PE ---------------------------------------

# Returns NA for timeseries where all values are NA
PE_ts <- targets_interp |> 
  group_by(site_id, variable, depth_m) |> 
  group_split() |>
  map_df(calculate_PE_ts, 
         tie_method = 'first', 
         D = D,
         tau = tau, 
         use_weights = T, 
         window_width = window_length) |>
  bind_rows()

PE_ts |> 
  filter(variable == 'DO_mgL_mean') |> 
  # remove the values before after NA runs
  mutate(doy = yday(datetime),
         year = year(datetime)) |> 
  ggplot(aes(x=doy, y=PE, colour = as.factor(year))) +
  geom_line() +
  geom_smooth(method = 'gam') +
  scale_colour_viridis_d(name = '', option = 'magma', begin = 0.9, end = 0) +
  theme_bw() +
  labs(title = 'DO_mgL_mean') +
  facet_wrap(site_id~depth_m)

#============================================#

# Seasonal analysis ---------------------------------------
# Using the stratified period dates
PE_season <- targets_interp |> 
  mutate(year = year(datetime)) |> 
  right_join(strat_dates, by = c('site_id', 'year'),
            relationship = "many-to-many") |> 
  # is the date between the start and end dates defined above
  mutate(strat = if_else(between(datetime, 
                                 as_date(start), as_date(end)),
                         T, F))  |>
  group_by(depth_m, strat, variable, site_id) |> 
  summarise(PE = calculate_PE(observation)) 

PE_season |> 
  ggplot(aes(y = PE, x = variable, fill = as_factor(depth_m))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x='Variable', y = 'Normalised PE') +
  theme_bw() +
  facet_wrap(strat~site_id, nrow = 2, labeller = label_both) +
  scale_fill_viridis_d(option = 'A', name = 'Depth', begin = 0.2, end = 0.9) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
  

PE_season_byyear <- targets_interp |> 
  mutate(year = year(datetime)) |> 
  right_join(strat_dates, by = c('site_id', 'year'),
             relationship = "many-to-many") |> 
  # is the date between the start and end dates defined above
  mutate(strat = if_else(between(datetime, 
                                 as_date(start), as_date(end)),
                         T, F))  |>
  group_by(depth_m, strat, variable, site_id, year) |> 
  summarise(PE = calculate_PE(observation)) 


PE_season_byyear |> 
  ggplot(aes(y = PE, x = variable, fill = as_factor(strat))) +
  geom_boxplot() +
  labs(x='Variable', y = 'Normalised PE') +
  theme_bw() +
  facet_wrap(depth_m~site_id, nrow = 2, labeller = label_both) +
  scale_fill_viridis_d(option = 'A', name = 'strat', begin = 0.2, end = 0.9) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 

