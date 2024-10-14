library(ggh4x)
library(ggridges)
# Plots for manuscript
D = 3
tau = 1
### Figure 1 - observations ####
targets_P1D |>
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom'))) |> 
  ggplot(aes(x=date, y=observation, colour = depth_m)) +
  geom_line(linewidth = 0.8) +
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
  filter(year(date) > 2020) |> 
  mutate(depth_m = factor(depth_m, levels = c('met', 'surface', 'bottom')),
         variable = factor(variable, levels = c('AirTemp_C',
                                                'Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  group_by(site_id, variable, depth_m) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))

central_tendancy_PE_sitewise <- PE_ts_P1D |> 
  filter(depth_m != 'met',
         year(date) >= 2021) |> 
  na.omit() |> 
  mutate(depth_m = factor(depth_m, levels = c('met', 'surface', 'bottom')),
         variable = factor(variable, levels = c('AirTemp_C',
                                                'Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL')),
         predictability = 1-PE) |> 
  reframe(.by = c(variable, depth_m, site_id),
          quantile_80 = quantile(PE, 0.8),
          CDF = mean(PE <= 0.5),
          median = median(PE)) 

PE_sitewise <- PE_ts_P1D |> 
  filter(depth_m != 'met',
         year(date) >= 2021) |> 
  mutate(depth_m = factor(depth_m, levels = c('met', 'surface', 'bottom')),
         variable = factor(variable, levels = c('AirTemp_C',
                                                'Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL')),
         predictability = 1-PE) |> 
  na.omit() |> 
  ggplot()+
  geom_density_ridges(aes(x=PE, y= variable,
                   colour = variable, 
                   fill = variable), 
               alpha = 0.5, rel_min_height = 0.005) +
  # geom_text(data = summary_PE, 
  #           aes(x = PE, y = variable, label = "*", colour = variable), show.legend = F, size = 7) +
  geom_vline(data = filter(central_tendancy_PE_sitewise),
             aes(xintercept = median, colour = variable), 
             show.legend = F, linewidth = 0.8, alpha = 0.7, linetype = 'longdash') +
  facet_grid(depth_m~site_id, scales = 'free') +
  scale_fill_viridis_d(name = 'Variable_unit', option = 'plasma', begin = 0, end = 0.8) +
  scale_colour_viridis_d(name = 'Variable_unit', option = 'plasma', begin = 0, end = 0.8) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1, 0.2))+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top', 
        axis.text.y = element_text(hjust = 1, vjust = -1),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white', colour = NA)) 

central_tendancy_PE_combined <-PE_ts_P1D |> 
  filter(depth_m != 'met',
         year(date) >= 2021) |> 
  na.omit() |> 
  mutate(depth_m = factor(depth_m, levels = c('met', 'surface', 'bottom')),
         variable = factor(variable, levels = c('AirTemp_C',
                                                'Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL')),
         predictability = 1-PE) |> 
  reframe(.by = c(variable, depth_m),
          quantile_80 = quantile(PE, 0.8),
          CDF = mean(PE <= 0.5),
          median = median(PE)) 

PE_combined <- PE_ts_P1D |> 
  filter(depth_m != 'met',
         year(date) >= 2021)  |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('AirTemp_C',
                                                'Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL')),
         predictability = 1-PE) |> 
  na.omit() |> 
  mutate(sites = 'combined') |> 
  ggplot()+
  geom_density_ridges(aes(x=PE, y= variable,
                          colour = variable, 
                          fill = variable), 
                      alpha = 0.5, rel_min_height = 0.005) +
  geom_vline(data = filter(central_tendancy_PE_combined, depth_m != 'met'),
             aes(xintercept = median, colour = variable), 
             show.legend = F, linewidth = 0.8, alpha = 0.7, linetype = 'longdash') +
  facet_grid(depth_m~sites, scales = 'free_y') +
  scale_fill_viridis_d(name = 'Variable_unit', option = 'plasma', begin = 0, end = 0.8) +
  scale_colour_viridis_d(name = 'Variable_unit', option = 'plasma', begin = 0, end = 0.8) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1, 0.2))+
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top', 
        axis.text.y = element_text(hjust = 1, vjust = -1),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white', colour = NA), 
        panel.spacing.x = unit(1, "lines")) 

ggpubr::ggarrange(PE_combined, PE_sitewise, common.legend = T, 
                  widths = c(1,1.75), 
                  labels = c('a)', 'b)'))


### Figure 3 - time series ####

PE_ts_P1D |> 
  filter(year(date) > 2020 & site_id == 'BVR' | 
           year(date) > 2018 & site_id == 'FCR') |>  
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL')),
         predictability = 1 - PE) |> 
  ggplot() +
  geom_line(aes(x=yday(date), y=predictability, group = interaction(depth_m, year(date)), colour = depth_m),
            alpha = 0.6) +
  geom_smooth(aes(x=yday(date), y=predictability, colour = depth_m), 
              method = 'gam', 
              formula = y ~ s(x, bs = "cs")) + 
  facet_grid(variable~site_id) +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'right', 
        strip.background = element_rect(fill = 'white')) +
  scale_colour_viridis_d(name = '', begin = 0.8, end = 0.4, option = 'viridis') +
  scale_x_continuous(name = 'day of year', breaks = seq(0,350,50))+
  scale_y_continuous(name = 'predictability', breaks = seq(0,1,0.2))
  

# PE of shuffled realisations
PE_shuffled_P1D |>
  filter(n %in% 900:1000) |> 
  mutate(depth_m = factor(depth_m, levels = c('met', 'surface', 'bottom')),
         variable = factor(variable, levels = c('AirTemp_C',
                                                'Temp_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL')),
         predictability = 1-PE) |> 
  ggplot() +
  geom_jitter(aes(y= variable, 
                  x = 1-PE, 
                  colour = site_id), 
              size=0.4, shape = 16, alpha=0.6, 
              position=position_jitterdodge(dodge.width=0.9)) +
  geom_boxplot(aes(y= variable,
                   x = 1-PE, 
                   colour = site_id), 
               fill = NA,  position=position_dodge(width=0.9), outlier.shape = NA) +
  facet_wrap(variable~depth_m, scales = 'free') +
  # facet_nested_wrap(vars(variable, depth_m), 
  #                   dir = "h",
  #                   # axes = "x",
  #                   remove_labels = 'all',
  #                   ncol = 3,
  #                   scales = 'free') +
  geom_point(data = summary_PE, 
             aes(y= variable, 
                 x = 1-PE, 
                 colour = site_id), 
             show.legend = F, size = 2.5, shape = 8, 
             position=position_dodge(width = 1))+
  theme_bw()  +
  scale_colour_viridis_d(name = '', begin = 0.8, end = 0.4, option = 'mako') +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position.inside = c(0.6, 0.15),
        legend.background = element_rect(fill = "white", colour = NA))
