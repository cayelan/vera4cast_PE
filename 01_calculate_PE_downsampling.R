# Downsample the HF daily data
targets_weekly <- targets_interp |> 
  group_by(site_id, variable, depth_m) |> 
  group_split() |>
  map_df(downsample, 
         out.freq = 'weekly', 
         method = 'sample') |>
  bind_rows()

targets_weekly |>
  group_by(depth_m, variable, site_id) |> 
  summarise(PE = calculate_PE(observation)) |> 
  ggplot(aes(y = PE, x = variable, fill = as_factor(depth_m))) +
  geom_bar(stat = 'identity', position = 'dodge') +
  labs(x='Variable', y = 'Normalised PE') +
  theme_bw() +
  facet_wrap(~site_id, nrow = 2) +
  scale_fill_viridis_d(option = 'C', name = 'Depth_m') +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))

# resample the downsampled data?
# Resample the time series to calculate the PE on subsets of the data to generate a distribution
targets_weekly_resample <- targets_weekly |>
  na.omit() |> 
  tsibble::as_tsibble(index = year_week, key = c(variable, site_id, depth_m)) |> 
  tsibble::fill_gaps(.full = FALSE) |> 
  as_tibble() |> 
  mutate(observation = imputeTS::na_interpolation(observation, 'linear', maxgap = 10, rule = 1)) |>
  arrange(variable, depth_m, site_id, year_week) |> 
  reframe(.by = c(variable, site_id, depth_m),
          resample(ts = observation, length.out = 15, n = 100))

PE_resampled_weekly <- targets_weekly_resample |> 
  group_by(n, site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation), .groups = 'drop')

PE_resampled_weekly |> 
  mutate(depth_m = as_factor(depth_m)) |> 
  ggplot(aes(x=PE, fill = depth_m, y = after_stat(ndensity))) +
  geom_histogram(bins = 15, position = 'identity', alpha = 0.5) +
  # geom_freqpoly(bins = 30, position = 'identity', aes(colour = depth_m)) +
  facet_grid(variable~site_id) +
  scale_fill_viridis_d(option = 'C', name = 'Depth_m') +
  scale_colour_viridis_d(option = 'C', name = 'Depth_m') +
  theme_bw()


# calcaulte the equivalent of the sample
targets_weekly |> 
  filter(month(year_week) %in% 4:11, 
         year(year_week) >= 2014) |> 
  arrange(site_id, variable, depth_m, year_week)|> 
  mutate(year = year(year_week)) |>
  # calculate the PE for each year seperately 
  group_by(year, depth_m, variable, site_id) |> 
  summarise(PE = calculate_PE(observation), .groups = 'drop')  |> 
  ggplot(aes(y = PE, x = variable, colour = site_id)) +
  geom_boxplot() +
  labs(x='Variable', y = 'Normalised PE') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  facet_wrap(~depth_m, ncol = 2) + theme(legend.position = 'top')+
  scale_colour_viridis_d(option = 'C', name = 'site', end = 0.9) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))

