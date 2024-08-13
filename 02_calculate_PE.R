# =====================================================#
# Set parameters for PE calculations
D  <- 3 # length of the word, embedding dimension
tau <- 1 # embedding time delay
window_length <- 30 # for a rolling PE how long should the time series be
# resample_n <- 50 # how many samples? for the resampled PE

#======================================================#

PE_resampled_P1D <- targets_P1D_resample |> 
  group_by(n, site_id, variable, depth_m, doy) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau, use_weights = T, ignore_gaps = T),
            .groups = 'drop')

#================================#
PE_ts_P1D <- targets_P1D_interp |> 
  group_by(site_id, variable, depth_m) |> 
  group_split() |> 
  map(.f = ~calculate_PE_ts(x = .x,
                            time_col = 'date', 
                            window_width = 50,
                            tie_method = 'first', 
                            D = 3, tau = 1, 
                            use_weights = T)) |> 
  bind_rows()

# Shuffle timeseries and calculate 
PE_shuffled_P1D <- targets_P1D_shuffled |> 
  group_by(variable, site_id, depth_m, n) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))
