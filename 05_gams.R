library(tidyverse)
library(mgcv)
library(lubridate)
library(gratia)
library(marginaleffects)

# PE_ts_P1D <- read_csv('temp_dat.csv')


# Surface Tw ----------------------
# Filter the data for each model
surface_Tw_df <- PE_ts_P1D |> 
  mutate(PE = ifelse(PE == 0, 
                     sample(.Machine$double.eps*100:.Machine$double.eps*1000, 
                            n(), 
                            replace = TRUE), 
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_sTw <- bam(PE ~ 
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
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sTw,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sTw <- itsadug::start_value_rho(bam_mod_sTw, plot = T, lag = 2)


bam_mod_sTw_2 <- bam(PE ~ 
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
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_bDO <- bam(PE ~ 
                 s(doy, bs = 'cc') + 
                 s(doy, by = site_id, bs = 'cc', m = 1) + 
                 site_id,
               family = betar(link = 'logit'),
               method = "fREML",
               data = bottom_DO_df, discrete = T)

# summary(bam_mod_bDO)
# draw(bam_mod_bDO)
# 
# gam.check(bam_mod_bDO)
# pacf(residuals(bam_mod_bDO,
#                type = 'working'))

# test for significant autocorrelation
bottom_DO_df %>%
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_bDO,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_bDO <- itsadug::start_value_rho(bam_mod_bDO, plot = T, lag = 2)


bam_mod_bDO_2 <- bam(PE ~ 
                  s(doy, bs = 'cc', k = 10) +
                  s(doy, by = site_id, bs = 'cc', k = 10, m = 1) + 
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
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_sDO <- bam(PE ~ 
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
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sDO,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sDO <- itsadug::start_value_rho(bam_mod_sDO, plot = T, lag = 2)


bam_mod_sDO_2 <- bam(PE ~ 
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
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_bTw <- bam(PE ~ 
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
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_bTw,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_bTw <- itsadug::start_value_rho(bam_mod_bTw, plot = T, lag = 2)


bam_mod_bTw_2 <- bam(PE ~ 
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
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_sSpCond <- bam(PE ~ 
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
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sSpCond,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sSpCond <- itsadug::start_value_rho(bam_mod_sSpCond, plot = T, lag = 2)


bam_mod_sSpCond_2 <- bam(PE ~ 
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
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_sfDOM <- bam(PE ~ 
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
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sfDOM,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sfDOM <- itsadug::start_value_rho(bam_mod_sfDOM, plot = T, lag = 2)


bam_mod_sfDOM_2 <- bam(PE ~ 
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
                     PE)) |> 
  filter(between(date, as_date('2021-01-01'), as_date('2023-12-31'))) |> 
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


bam_mod_sChla <- bam(PE ~ 
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
  filter(!is.na(PE)) |> 
  ungroup() |> 
  mutate(resids = residuals(bam_mod_sChla,
                            type = 'working')) %>%
  group_by(site_id) %>%
  arrange(doy) %>%
  mutate(autocor = Box.test(resids, type = 'Ljung-Box')$p.value) %>%
  summarise(autocor = mean(autocor),
            sig = mean(autocor) < 0.05)


rho_sChla <- itsadug::start_value_rho(bam_mod_sChla, plot = T, lag = 2)


bam_mod_sChla_2 <- bam(PE ~ 
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

