# =====================================================#
# Set parameters for PE calculations
D  <- 3 # length of the word, embedding dimension
tau <- 1 # embedding time delay
window_length <- 50 # for a rolling PE how long should the time series be

#======================================================#

# Summary --------------
summary_PE <- targets_P1D_interp |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  group_by(site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))

#======================================================#

# Comparison with shuffled realisations ------
# Shuffle timeseries and calculate 
PE_shuffled_P1D <- targets_P1D_shuffled |> 
  reframe(.by = c(variable, site_id, depth_m, n),
          PE = calculate_PE(observation, D = D, tau = tau))

# compare the shuffled values with the calculated values in the summary to obtain a p-value
summary_PE |> 
  rename(summary_PE = PE) |> 
  full_join(PE_shuffled_P1D, by = join_by(site_id, variable, depth_m)) |> ungroup() |> 
  # is the value greater/less than the summary value
  reframe(.by = c(site_id, variable, depth_m),
          mean = mean(PE),
          sd = sd(PE),
          p = sum(ifelse(PE < summary_PE, 1, 0))/n())

#================================#
# Rolling window of PE ---------
PE_ts_P1D <- targets_P1D_interp |> 
  group_by(site_id, variable, depth_m) |> 
  group_split() |> 
  map(.f = ~calculate_PE_ts(x = .x,
                            time_col = 'date', 
                            window_width = window_length,
                            tie_method = 'first', 
                            D = D, tau = tau, 
                            use_weights = T)) |> 
  bind_rows()



