#--------------------------------------#
## Project: vera4cast PE
## Script purpose: generates plots for manuscript
## Date: 2025-02-23
## Author: Freya Olsson, Cayelan Carey
#--------------------------------------#

library(ggh4x)
library(ggridges)
library(ggpubr)
# Plots for manuscript
# Set the hyperparameters for the PE calculations -----
D = 3
tau = 1

# Figure 1 = map
# Figure 2 = conceptual methods figure

# Figure 3 - observations ####
Fig3 <- targets_P1D_interp |>
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw',
                                      'SpCond',
                                      'fDOM',
                                      'DO',
                                      'Chla')),
         units = ifelse(variable == 'Chla', 
                        'ug/L',
                        ifelse(variable == 'DO', 'mg/L',
                               ifelse(variable == 'fDOM', 'QSU',
                                      ifelse(variable == 'SpCond', 'uS/cm', 'Celsius')))),
         description = ifelse(site_id == 'FCR', 'with oxygenation',
                              'without oxygenation')) |> 
  ggplot(aes(x=date, y=observation, colour = depth_m)) +
  geom_line(linewidth = 0.8) +
  facet_grid(variable + units ~ site_id + description, scales="free_y") +
  scale_x_date(date_labels = "%d %b %y", breaks = '1 year', name = 'Date') +
  scale_colour_viridis_d(name = '', begin = 0.8, end = 0.4, option = 'viridis') +
  theme_bw(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'top', 
        strip.background = element_rect(fill = 'white'))
ggsave(plot = Fig3, filename = 'Figure_3.png', height = 15, width  = 15, units = 'cm')

# Figure 4 - PE distributions ####
summary_PE <- targets_P1D_interp |> 
  mutate(depth_m = factor(depth_m, levels = c( 'surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw_C',
                                      'SpCond_uScm',
                                      'fDOM_QSU',
                                      'DO_mgL',
                                      'Chla_ugL')),
         description = ifelse(site_id == 'FCR', 'with oxygenation', 'without oxygenation')) |> 
  group_by(site_id, variable, depth_m, description) |> 
  summarise(PE = calculate_PE(observation, D = D, tau = tau))

# plot distributions by site
central_tendancy_PE_sitewise <- PE_ts_P1D |> 
  na.omit() |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw_C',
                                      'SpCond_uScm',
                                      'fDOM_QSU',
                                      'DO_mgL',
                                      'Chla_ugL')),
         description = ifelse(site_id == 'FCR', 'with oxygenation', 'without oxygenation'),
         predictability = 1-PE) |> 
  reframe(.by = c(variable, depth_m, site_id, description),
          quantile_80 = quantile(predictability, 0.2),
          CDF = mean(predictability >= 0.5),
          median = median(predictability)) 

PE_sitewise <- PE_ts_P1D |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw_C',
                                      'SpCond_uScm',
                                      'fDOM_QSU',
                                      'DO_mgL',
                                      'Chla_ugL')),
         description = ifelse(site_id == 'FCR', 'with oxygenation', 'without oxygenation'),
         predictability = 1-PE) |> 
  na.omit() |> 
  ggplot()+
  geom_density_ridges(aes(x=predictability, y= fct_rev(variable),
                          colour = variable, 
                          fill = variable), 
                      alpha = 0.5, rel_min_height = 0.005) +
  geom_vline(data = filter(central_tendancy_PE_sitewise),
             aes(xintercept = median, colour = variable), 
             show.legend = F, linewidth = 0.8, alpha = 0.7, linetype = 'longdash') +
  facet_grid(depth_m~site_id + description, scales = 'free') +
  scale_fill_viridis_d(name = 'Variable_unit', option = 'plasma', begin = 0, end = 0.8) +
  scale_colour_viridis_d(name = 'Variable_unit', option = 'plasma', begin = 0, end = 0.8) +
  scale_x_continuous(expand = c(0.01,0.01), limits = c(0,1), breaks = seq(0,1, 0.2)) +
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top', 
        axis.text.y = element_text(hjust = 1, vjust = -1),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white', colour = NA)) 

# plot distributions for both sites
central_tendancy_PE_combined <-PE_ts_P1D |> 
  na.omit() |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw_C',
                                      'SpCond_uScm',
                                      'fDOM_QSU',
                                      'DO_mgL',
                                      'Chla_ugL')),
         description = 'both sites',
         predictability = 1-PE) |> 
  reframe(.by = c(variable, depth_m),
          quantile_80 = quantile(predictability, 0.2),
          CDF = mean(predictability >= 0.5),
          median = median(predictability)) 

PE_combined <- PE_ts_P1D |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw_C',
                                      'SpCond_uScm',
                                      'fDOM_QSU',
                                      'DO_mgL',
                                      'Chla_ugL')),
         predictability = 1-PE) |> 
  na.omit() |> 
  mutate(sites = 'both sites',) |> 
  ggplot()+
  geom_density_ridges(aes(x=predictability, y= fct_rev(variable),
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
  theme_minimal(base_size = 14) +
  theme(panel.grid.minor = element_blank(),
        legend.position = 'top', 
        axis.text.y = element_text(hjust = 1, vjust = -1),
        axis.title.y = element_blank(),
        strip.background = element_rect(fill = 'white', colour = NA), 
        panel.spacing.x = unit(1, "lines")) 

Fig4 <- ggpubr::ggarrange(PE_combined, PE_sitewise, common.legend = T, 
                          widths = c(1,1.75), align = 'h',
                          labels = c('(a)', '(b)'))

ggsave(Fig4, filename = 'Figure_4.png', width = 25, height = 15, units = 'cm')
# Figure 5 is generated in "05_gams.R" script

# Appendix S1 plots -----------------
# Figure S1 is generated in the supplementary analysis script (see: "SI_analysis/SI_analysis.Rmd")

# Figure S2
# Example of shuffled time series
original_ts <- targets_P1D_interp |> 
  filter(variable == 'Chla_ugL',
         site_id == 'FCR') |> 
  ggplot(aes(x=date, y= observation)) +
  geom_line() +
  theme_bw() +
  labs(title = 'Observed time series')

example_shuffled <- targets_P1D_shuffled |> 
  filter(n %in% 1:3, # 3 examples
         variable == 'Chla_ugL',
         site_id == 'FCR')  |> 
  group_by(n) |> 
  mutate(x = row_number()) |> 
  ggplot(aes(x = x, y= observation)) +
  geom_line() +
  theme_bw() +
  labs(title = 'Examples of shuffled realisations') +
  facet_wrap(~n, ncol = 1)

ggarrange(ggarrange(NULL, original_ts, NULL, heights = c(0.6, 1.5, 0.5), ncol = 1),
          example_shuffled) |> 
  ggsave(filename = 'Figure_S2.png', height = 15, width = 25, units = 'cm')


# Figure S3
# PE of shuffled realisations -----
design <- "
 AB#
 DEF
 GH#
"

FigS3 <- PE_shuffled_P1D |>
  filter(n %in% 400:500) |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'))) |> 
  ggplot() +
  geom_jitter(aes(y= variable, 
                  x = 1-PE, 
                  colour = site_id), 
              size=0.7, shape = 16, alpha=0.6, 
              position=position_jitterdodge(dodge.width=0.9)) +
  geom_boxplot(aes(y= variable,
                   x = 1-PE, 
                   colour = site_id), 
               fill = NA,  position=position_dodge(width=0.9), outlier.shape = NA) +
  ggh4x::facet_nested_wrap(~variable + depth_m, scales = 'free', ncol = 3) +
  geom_point(data = summary_PE, 
             aes(y= variable, 
                 x = 1-PE, 
                 colour = site_id), 
             show.legend = F, size = 2.5, shape = 8, 
             position=position_dodge(width = 1)) +
  theme_bw(base_size = 14)  + 
  labs(x='predictability') +
  scale_colour_manual(name = '', values = viridis::mako(n=2, begin = 0.8, end = 0.2), 
                      labels = c('BVR - without oxygenation',
                                 'FCR - with oxygenation')) +
  theme(panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = 'white'),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position.inside = c(0.65, 0.2),
        legend.position = "inside",
        legend.background = element_rect(fill = "white", colour = NA),
        plot.margin = margin(0.6,0.6,0.6,0.6, "cm"))

ggsave(FigS3, filename = 'Figure_S3.png', height = 15, width = 15, units = 'cm')
# Time series of PE

# Figure S4
FigS4 <- PE_ts_P1D |> 
  mutate(depth_m = factor(depth_m, levels = c('surface', 'bottom')),
         variable = factor(variable, levels = c('Tw_C',
                                                'SpCond_uScm',
                                                'fDOM_QSU',
                                                'DO_mgL',
                                                'Chla_ugL'), 
                           labels = c('Tw',
                                      'SpCond',
                                      'fDOM',
                                      'DO',
                                      'Chla')),
         units = ifelse(variable == 'Chla', 
                        'ug/L',
                        ifelse(variable == 'DO', 'mg/L',
                               ifelse(variable == 'fDOM', 'QSU',
                                      ifelse(variable == 'SpCond', 'uS/cm', 'Celsius')))),
         description = ifelse(site_id == 'FCR', 'with oxygenation',
                              'without oxygenation'),
         predictability = 1 - PE) |> 
  ggplot(aes(x=date, y=predictability, colour = depth_m)) +
  geom_line(linewidth = 0.8) +
  facet_grid(variable + units ~ site_id + description) +
  scale_x_date(date_labels = "%d %b %y", breaks = '1 year', name = 'Date') +
  scale_y_continuous(breaks = seq(0, 1, 0.5)) +
  scale_colour_viridis_d(name = '', begin = 0.8, end = 0.4, option = 'viridis') +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = 'top', 
        strip.background = element_rect(fill = 'white'))
ggsave(plot = FigS4, filename = 'Figure_S4.png', height = 15, width  = 15, units = 'cm')

# Figure S5 is generated in the supplementary analysis script (see: "SI_analysis/SI_analysis.Rmd")
