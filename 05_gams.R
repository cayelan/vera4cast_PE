library(tidyverse)
library(mgcv)
library(lubridate)
library(gratia)
library(marginaleffects)

# PE_ts_P1D <- read_csv('temp_dat.csv')
write_model_output <- TRUE

# Surface Tw ----------------------
# Filter the data for each model
surface_Tw_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'Tw_C', 
         depth_m == 'surface') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))


bam_mod_sTw <- bam(predictability ~ 
                 s(doy, bs = 'cc') + 
                 s(doy, by = site_id, bs = 'cc', m = 1) + 
                 site_id,
               family = betar(link = 'logit'),
               method = "fREML",
               data = surface_Tw_df,
               discrete = T)

# summary(bam_mod_sTw)
# draw(bam_mod_sTw)
# 
# gam.check(bam_mod_sTw)
# pacf(residuals(bam_mod_sTw,
#                type = 'working'))

# test for significant autocorrelation
surface_Tw_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sTw,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sTw <- itsadug::start_value_rho(bam_mod_sTw, plot = T, lag = 2)


bam_mod_sTw_2 <- bam(predictability ~ 
                  s(doy, bs = 'cc', k = 20) + 
                  s(doy, by = site_id, bs = 'cc', k=20, m = 1) + 
                  site_id,
                family = betar(link = 'logit'),
                method = "fREML",
                data = surface_Tw_df, 
                discrete = T, 
                AR.start = surface_Tw_df$ar.start,
                rho = rho_sTw)

summary(bam_mod_sTw_2)

draw(bam_mod_sTw_2)
gam.check(bam_mod_sTw_2)
plot_predictions(bam_mod_sTw_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_sTw_2, select = "s(doy)") |>
  draw()

# Bottom DO ----------------------
bottom_DO_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE,
         #recode the sites to have fcr split into DO and non-DO years
         site_id = ifelse(site_id == 'FCR' & date < as_date('2023-01-01'),
                          'FCR_anoxic', ifelse(site_id == 'FCR' & date >= as_date('2023-01-01'),
                                               'FCR_oxic',
                                               'BVR'))) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'DO_mgL', 
         depth_m == 'bottom') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))



bam_mod_bDO <- bam(predictability ~ 
                     s(doy, bs = 'cc', k= 10) + 
                     s(doy, by = site_id, bs = 'cc', k = 10, m = 1) + 
                     site_id,
                   family = betar(link = 'logit'),
                   method = "fREML",
                   data = bottom_DO_df, discrete = T)

summary(bam_mod_bDO)
draw(bam_mod_bDO)

gam.check(bam_mod_bDO)
pacf(residuals(bam_mod_bDO,
               type = 'working'))

# test for significant autocorrelation
bottom_DO_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_bDO,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_bDO <- itsadug::start_value_rho(bam_mod_bDO, plot = T, lag = 2)


bam_mod_bDO_2 <- bam(predictability ~ 
                       s(doy, bs = 'cc', k = 12) +
                       s(doy, by = site_id, bs = 'cc', k = 12, m = 1) + 
                       site_id,
                     family = betar(link = 'logit'),
                     method = "fREML",
                     data = bottom_DO_df, 
                     discrete = T, 
                     AR.start = bottom_DO_df$ar.start,
                     rho = rho_bDO)

summary(bam_mod_bDO_2)

draw(bam_mod_bDO_2)
gam.check(bam_mod_bDO_2)
concurvity(bam_mod_bDO_2)
plot_predictions(bam_mod_bDO_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_bDO_2, select = "s(doy)") |>
  draw()


# Surface DO ----------------------
surface_DO_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'DO_mgL', 
         depth_m == 'surface') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))


bam_mod_sDO <- bam(predictability ~ 
                     s(doy, bs = 'cc') + 
                     s(doy, by = site_id, bs = 'cc', m = 1) + 
                     site_id,
                   family = betar(link = 'logit'),
                   method = "fREML",
                   data = surface_DO_df, discrete = T)

# summary(bam_mod_sDO)
# draw(bam_mod_sDO)
# 
# gam.check(bam_mod_sDO)
# pacf(residuals(bam_mod_sDO,
#                type = 'working'))

# test for significant autocorrelation
surface_DO_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sDO,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sDO <- itsadug::start_value_rho(bam_mod_sDO, plot = T, lag = 2)


bam_mod_sDO_2 <- bam(predictability ~ 
                       s(doy, bs = 'cc', k = 20) +
                       s(doy, by = site_id, bs = 'cc', k = 20, m = 1) + 
                       site_id,
                     family = betar(link = 'logit'),
                     method = "fREML",
                     data = surface_DO_df, 
                     discrete = T, 
                     AR.start = surface_DO_df$ar.start,
                     rho = rho_sDO)

summary(bam_mod_sDO_2)

draw(bam_mod_sDO_2)
gam.check(bam_mod_sDO_2)
plot_predictions(bam_mod_sDO_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_sDO_2, select = "s(doy)") |>
  draw()



# Bottom Tw ----------------------
# Filter the data for each model
bottom_Tw_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'Tw_C', 
         depth_m == 'bottom') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))


bam_mod_bTw <- bam(predictability ~ 
                     s(doy, bs = 'cc') + 
                     s(doy, by = site_id, bs = 'cc', m = 1) + 
                     site_id,
                   family = betar(link = 'logit'),
                   method = "fREML",
                   data = bottom_Tw_df, 
                   discrete = T)

# summary(bam_mod_bTw)
# draw(bam_mod_bTw)
# 
# gam.check(bam_mod_bTw)
# pacf(residuals(bam_mod_bTw,
#                type = 'working'))

# test for significant autocorrelation
bottom_Tw_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_bTw,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_bTw <- itsadug::start_value_rho(bam_mod_bTw, plot = T, lag = 2)


bam_mod_bTw_2 <- bam(predictability ~ 
                       s(doy, bs = 'cc', k = 15) + 
                       s(doy, by = site_id, bs = 'cc', m = 1, k = 15) + 
                       site_id,
                     family = betar(link = 'logit'),
                     method = "fREML",
                     data = bottom_Tw_df, 
                     discrete = T, 
                     AR.start = bottom_Tw_df$ar.start,
                     rho = rho_bTw)

summary(bam_mod_bTw_2)

draw(bam_mod_bTw_2)
gam.check(bam_mod_bTw_2)
plot_predictions(bam_mod_bTw_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_bTw_2, select = "s(doy)") |>
  draw()
# Surface SpCond ----------------------
# Filter the data for each model
surface_SpCond_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'SpCond_uScm', 
         depth_m == 'surface') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))


bam_mod_sSpCond <- bam(predictability ~ 
                     s(doy, bs = 'cc') + 
                     s(doy, by = site_id, bs = 'cc', m = 1) + 
                     site_id,
                   family = betar(link = 'logit'),
                   method = "fREML",
                   data = surface_SpCond_df,
                   discrete = T)

# summary(bam_mod_sSpCond)
# draw(bam_mod_sSpCond)
# 
# gam.check(bam_mod_sSpCond)
# pacf(residuals(bam_mod_sSpCond,
#                type = 'working'))

# test for significant autocorrelation
surface_SpCond_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sSpCond,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sSpCond <- itsadug::start_value_rho(bam_mod_sSpCond, plot = T, lag = 2)


bam_mod_sSpCond_2 <- bam(predictability ~ 
                       s(doy, bs = 'cc', k = 20) + 
                       s(doy, by = site_id, bs = 'cc', k=20, m = 1) + 
                       site_id,
                     family = betar(link = 'logit'),
                     method = "fREML",
                     data = surface_SpCond_df, 
                     discrete = T, 
                     AR.start = surface_SpCond_df$ar.start,
                     rho = rho_sSpCond)

summary(bam_mod_sSpCond_2)

draw(bam_mod_sSpCond_2)
gam.check(bam_mod_sSpCond_2)
plot_predictions(bam_mod_sSpCond_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_sSpCond_2, select = "s(doy)") |>
  draw()


# Surface fDOM ----------------------
# Filter the data for each model
surface_fDOM_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'fDOM_QSU', 
         depth_m == 'surface') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))


bam_mod_sfDOM <- bam(predictability ~ 
                     s(doy, bs = 'cc') + 
                     s(doy, by = site_id, bs = 'cc', m = 1) + 
                     site_id,
                   family = betar(link = 'logit'),
                   method = "fREML",
                   data = surface_fDOM_df,
                   discrete = T)

# summary(bam_mod_sfDOM)
# draw(bam_mod_sfDOM)
# 
# gam.check(bam_mod_sfDOM)
# pacf(residuals(bam_mod_sfDOM,
#                type = 'working'))

# test for significant autocorrelation
surface_fDOM_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sfDOM,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sfDOM <- itsadug::start_value_rho(bam_mod_sfDOM, plot = T, lag = 2)


bam_mod_sfDOM_2 <- bam(predictability ~ 
                       s(doy, bs = 'cc', k = 20) + 
                       s(doy, by = site_id, bs = 'cc', k=20, m = 1) + 
                       site_id,
                     family = betar(link = 'logit'),
                     method = "fREML",
                     data = surface_fDOM_df, 
                     discrete = T, 
                     AR.start = surface_fDOM_df$ar.start,
                     rho = rho_sfDOM)

summary(bam_mod_sfDOM_2)

draw(bam_mod_sfDOM_2)
gam.check(bam_mod_sfDOM_2, old.style = F)
plot_predictions(bam_mod_sfDOM_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_sfDOM_2, select = "s(doy)") |>
  draw()

# Surface Chla ----------------------
# Filter the data for each model
surface_Chla_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE),
         predictability = 1 - PE) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2024-12-31'))) |> 
  filter(variable == 'Chla_ugL', 
         depth_m == 'surface') |> 
  mutate(depth_m = factor(depth_m,ordered = F),
         site_id = factor(site_id,ordered = F),
         variable = factor(variable),
         doy = yday(date),
         year = year(date)) |> 
  group_by(site_id, variable, depth_m) |> 
  mutate(n = 1:n()) %>%
  mutate(ar.start = ifelse(n == 1, T, F)) %>%
  mutate(time = as.numeric(date))


bam_mod_sChla <- bam(predictability ~ 
                       s(doy, bs = 'cc') + 
                       s(doy, by = site_id, bs = 'cc', m = 1) + 
                       site_id,
                     family = betar(link = 'logit'),
                     method = "fREML",
                     data = surface_Chla_df,
                     discrete = T)

# summary(bam_mod_sChla)
# draw(bam_mod_sChla)
# 
# gam.check(bam_mod_sChla)
# pacf(residuals(bam_mod_sChla,
#                type = 'working'))

# test for significant autocorrelation
surface_Chla_df %>%
  filter(!is.na(predictability)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sChla,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sChla <- itsadug::start_value_rho(bam_mod_sChla, plot = T, lag = 2)


bam_mod_sChla_2 <- bam(predictability ~ 
                         s(doy, bs = 'cc', k = 20) + 
                         s(doy, by = site_id, bs = 'cc', k=20, m=1) + 
                         site_id,
                       family = betar(link = 'logit'),
                       method = "fREML",
                       data = surface_Chla_df, 
                       discrete = T, 
                       AR.start = surface_Chla_df$ar.start,
                       rho = rho_sChla)

summary(bam_mod_sChla_2)

draw(bam_mod_sChla_2)
gam.check(bam_mod_sChla_2)
plot_predictions(bam_mod_sChla_2, 
                 condition = c('doy', 
                               'site_id'))

difference_smooths(bam_mod_sChla_2, select = "s(doy)") |>
  draw()



# Plots for main text ----
theme_ms <- function() {
  theme_bw() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.grid.minor = element_blank())
  
}

library(ggpubr)
p_sDO <- plot_predictions(bam_mod_sDO_2, 
                          condition = c('doy', 
                                        'site_id'))  + 
  scale_colour_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_fill_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  # labs(subtitle = 'Surface DO') +
  theme_ms()

man_colours <- viridis::rocket(n=2, begin = 0.8, end=0.2)

p_bDO <- plot_predictions(bam_mod_bDO_2, 
                          condition = c('doy', 
                                        'site_id'), 
                          draw = F) |> 
  # manually make the plot to use linetype
  ggplot(aes(x=doy)) +
  geom_line(aes(y=estimate, linetype = site_id, colour = site_id)) +
  geom_ribbon(aes(ymax = conf.high, ymin = conf.low, fill = site_id), alpha = 0.1) +
  scale_colour_manual(values = c(man_colours[1], man_colours[2], man_colours[2])) +
  scale_fill_manual(values = c(man_colours[1], man_colours[2], man_colours[2])) +
  scale_linetype_manual(values = c('solid', 'solid', 'dashed')) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(y = 'predictability') +
  theme_ms()   

p_sTw <- plot_predictions(bam_mod_sTw_2, 
                          condition = c('doy', 
                                        'site_id'))  + 
  scale_colour_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_fill_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  # labs(subtitle = 'Surface Tw') +
  theme_ms()

p_bTw <- plot_predictions(bam_mod_bTw_2, 
                          condition = c('doy', 
                                        'site_id'))  + 
  scale_colour_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_fill_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  # labs(subtitle = 'Bottom Tw') +
  theme_ms()

p_sfDOM <- plot_predictions(bam_mod_sfDOM_2, 
                            condition = c('doy', 
                                          'site_id'))  + 
  scale_colour_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_fill_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  # labs(subtitle = 'Surface fDOM') +
  theme_ms()

p_sChla <- plot_predictions(bam_mod_sChla_2, 
                            condition = c('doy', 
                                          'site_id'))  + 
  scale_colour_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_fill_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  # labs(subtitle = 'Surface Chla') +
  theme_ms()

p_sSpCond <- plot_predictions(bam_mod_sSpCond_2, 
                              condition = c('doy', 
                                            'site_id'))  + 
  scale_colour_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_fill_viridis_d(option = 'rocket', begin = 0.8, end = 0.2) +
  scale_y_continuous(limits = c(0, 1)) +
  # labs(subtitle = 'Surface SpCond') +
  theme_ms()

p_legend <- ggpubr::get_legend(p_sTw)

p_all <- 
  ggpubr::ggarrange(plotlist = list(NULL, NULL, NULL, NULL, NULL, 
                                    p_sTw, p_sSpCond,  p_sfDOM, p_sDO, p_sChla,
                                    p_bTw, as_ggplot(p_legend), NULL, p_bDO, NULL),
                    nrow = 3, ncol = 5, heights = c(0.2, 1, 1), 
                    labels = c('Tw', 'SpCond', 'fDOM','DO',  'Chla'),
                    label.x = 0.6, label.y = 0.6, hjust = 0.5,
                    font.label = list(size = 12, face = 'bold'),
                    legend = F) 
ggpubr::annotate_figure(p_all, 
                        left = text_grob("Bottom                           Surface",
                                         face = "bold", size = 12, rot = 90))

# Write output -----
# Write the model output
if (write_model_output == T) {
  final_fitted_models <- ls(pattern = '^bam.*2$')
  
  dev.table <- NULL
  
  for (i in 1:length(final_fitted_models)) {
    
    mod <- get(final_fitted_models[i])
    par_terms <- broom::tidy(mod, parametric = T) 
    s_terms <- broom::tidy(mod, parametric = F) 
    
    bind_rows(par_terms, s_terms)  |> 
      write_csv(file = paste0(final_fitted_models[i],'_summary.csv'))
    
    # recalculate the explained deviance
    dev.expl <- data.frame(mod = final_fitted_models[i],
                           dev.expl = (mod$null.deviance - mod$deviance) / mod$null.deviance *100,
                           n = mod$df.null)
    
    dev.table <- bind_rows(dev.table, dev.expl)
   
  }
  write_csv(dev.table, 'deviance_explained_GAMs.csv')
}

