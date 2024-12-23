# =====================================================#
# Set parameters for PE calculations
D  <- 3 # length of the word, embedding dimension
tau <- 1 # embedding time delay
window_length <- 50 # for a rolling PE how long should the time series be

#======================================================#

# Summmary --------------
summary_PE <- targets_P1D_interp |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  group_by(site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))

summary_av_PE <- targets_P1D_av_interp |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  group_by(site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))

#======================================================#

# PE_resampled_P1D <- targets_P1D_resample |> 
#   group_by(n, site_id, variable, depth_m, doy) |> 
#   summarise(PE = calculate_PE(observation, D = D, tau = tau, use_weights = T, ignore_gaps = T),
#             .groups = 'drop')

#================================#
# Rolling window of PE ---------
PE_ts_P1D <- targets_P1D_interp |> 
  group_by(site_id, variable, depth_m) |> 
  group_split() |> 
  map(.f = ~calculate_PE_ts(x = .x,
                            time_col = 'date', 
                            window_width = window_length,
                            tie_method = 'first', 
                            D = 3, tau = 1, 
                            use_weights = T)) |> 
  bind_rows()

PE_ts_P1D_av <- targets_P1D_av_interp |> 
  group_by(site_id, variable, depth_m) |> 
  group_split() |> 
  map(.f = ~calculate_PE_ts(x = .x,
                            time_col = 'date', 
                            window_width = window_length,
                            tie_method = 'first', 
                            D = 3, tau = 1, 
                            use_weights = T)) |> 
  bind_rows()


# Comparison with shuffled reaslisations ------
# Shuffle timeseries and calculate 
PE_shuffled_P1D <- targets_P1D_shuffled |> 
  reframe(.by = c(variable, site_id, depth_m, n),
          PE = calculate_PE(observation, D = D, tau = tau))


