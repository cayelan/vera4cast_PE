# Plots for manuscript

### Figure 1 - observations ####
targets_P1D |> 
  filter(variable != 'SpCond_uScm') |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom'))) |> 
  ggplot(aes(x=date, y=observation, colour = depth_m)) +
  geom_line() +
  facet_grid(variable~site_id, scales = 'free') +
  scale_x_date(date_labels = "%d %b %y", breaks = '1 year', name = 'Date') +
  scale_colour_viridis_d(name = '', begin = 0.8, end = 0.4, option = 'viridis') +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'top', 
        strip.background = element_rect(fill = 'white'))


### Figure 2 - PE distributions ####
summary_PE <- targets_P1D_interp |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  group_by(site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))

PE_resampled_P1D |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  ggplot()+
  geom_text(data = summary_PE, 
            aes(x = PE, y = 8, label = "*", colour = variable), show.legend = F, size = 7) +
  geom_density(aes(x=PE, 
                   colour = variable, 
                   fill = variable), 
               alpha = 0.4) +
  facet_grid(depth_m~site_id) +
  scale_fill_viridis_d(name = 'Variable_unit', option = 'plasma') +
  scale_colour_viridis_d(name = 'Variable_unit', option = 'plasma') +
  scale_x_continuous(expand = c(0.01,0.01))+
  scale_y_continuous(expand = c(0,0), limits = c(0,10), name = 'density')+
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'right', 
        strip.background = element_rect(fill = 'white')) 


### Figure 3 - time series ####
PE_ts_P1D |> 
  filter(variable == 'DO_mgL',
         year(date) != 2018) |> 
  ggplot() +
  geom_line(aes(x=yday(date), y=PE, group = year(date), colour = as_factor(year(date))),
            alpha = 0.6) +
  geom_smooth(aes(x=yday(date), y=PE, group = year(date), colour = as_factor(year(date))), 
              method = 'gam', 
              formula = y ~ s(x, bs = "cs")) + 
  facet_grid(depth_m~site_id) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'right', 
        strip.background = element_rect(fill = 'white'))


PE_ts_P1D |> 
  filter(year(date) > 2018,
         variable != 'SpCond_uScm') |>  
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  ggplot() +
  geom_line(aes(x=yday(date), y=PE, group = interaction(depth_m, year(date)), colour = depth_m),
            alpha = 0.6) +
  geom_smooth(aes(x=yday(date), y=PE, colour = depth_m), 
              method = 'gam', 
              formula = y ~ s(x, bs = "cs")) + 
  facet_grid(variable~site_id) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'right', 
        strip.background = element_rect(fill = 'white')) +
  scale_colour_viridis_d(name = '', begin = 0.8, end = 0.4, option = 'viridis') +
  scale_x_continuous(name = 'day of year', breaks = seq(0,350,50))
  

