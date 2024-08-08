test <- filter(targets_P1D, depth_m == 'surface', site_id == 'fcre', variable == 'Temp_C')
# shuffle the timeseries
calculate_PE(test$observation, tie_method = 'first', D = D, tau = tau, use_weights = T, ignore_gaps = T)
# calculate_PE(sample(test$observation), tie_method = 'first', D = D, tau = tau, use_weights = T,ignore_gaps = T)

ggplot(test, aes(x=date, y=observation)) +
  geom_line()
ggplot(test, aes(x=date, y=sample(observation))) +
  geom_line()

# PE_test <- test |> 
#   reframe(resample(ts = observation, length.out = 50, n = nrow(test) - D + 1)) |> 
#   group_by(n) |> 
#   summarise(PE = calculate_PE(observation, D = D, tau = tau, use_weights = T, ignore_gaps = T),
#             .groups = 'drop')

PE_random <- test |> 
  mutate(observation = sample(observation)) |> 
  reframe(resample(ts = observation, length.out = 50, n = nrow(test) - D + 1))  |> 
  group_by(n) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau, use_weights = T, ignore_gaps = T),
            .groups = 'drop')  


# bind_rows(ts = PE_test, random = PE_random, .id = 'option') |> 
#   ggplot(aes(x=PE, fill = option)) +
#   geom_density(alpha = 0.5) +
#   theme_bw()
# 
# 
#  bind_rows(ts = PE_test, random = PE_random, .id = 'option') |> 
#   ggplot(aes(y=option, fill = option, x=PE)) +
#   geom_jitter(alpha = 0.2, shape = 1) +
#   # geom_boxplot(alpha = 0.5) +
#   theme_bw()
 
 
 ts_PE <- calculate_PE(test$observation, tie_method = 'first', D = D, tau = tau, use_weights = T, ignore_gaps = T)
 
 ggplot(PE_random, aes(x=PE)) +
   # geom_jitter(alpha = 0.2, shape = 1) +
   geom_boxplot(alpha = 0.5) +
   theme_bw() +
   annotate(geom = 'point', x = ts_PE, y= 0, colour = 'red')
 
 confint(lm(PE~1, data = PE_random))
 ts_PE