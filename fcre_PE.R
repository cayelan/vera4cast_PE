library(tidyverse)
library(zoo)

source('R/PE_functions.R')

# Get observational data
fcr_targets <- read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz") |> 
  filter(site_id == 'fcre',
         datetime < as_datetime('2024-01-01'),
         datetime >= as_datetime('2019-01-01'))

# Set parameters for PE calculations
D  <- 3 # length of the word
tau <- 1 # time delay
window_length <- 30 # for a rolling PE how long should the time series be

# Summaries of PE by variable
PE_byvar <- fcr_targets |>
  filter(variable %in% c("Temp_C_mean", "SpCond_uScm_mean", "Chla_ugL_mean", "DO_mgL_mean")) |>
  group_by(depth_m, variable) |> 
  summarise(PE = calculate_PE(observation)) 

PE_byvar |> 
  ggplot(aes(x=PE, y = depth_m, colour = variable)) +
  geom_point() +
  scale_x_continuous(lim = c(0,1)) +
  scale_y_reverse() +
  theme_bw() +
  scale_colour_viridis_d(option = 'turbo', name = '')

# ----------------------------#

# how does PE vary over time?
x <- fcr_targets |>  
  filter(variable == 'DO_mgL_mean',
         depth_m == 1.6) |>
  na.omit() # remove all NAs


all_dates <- data.frame(datetime = seq.Date(min(as_date(x$datetime)), 
                                            max(as_date(x$datetime)), 'day'))  

x <- full_join(all_dates, x) |> 
  select(datetime, observation)

PE_ts <- calculate_PE_ts(x = x,
                         tie_method = tie_method, 
                         D = D,
                         tau = tau, 
                         use_weights = T, 
                         window_width = 30) 

PE_ts |> 
  mutate(doy = yday(datetime),
         year = year(datetime)) |> 
  ggplot(aes(x=doy, y=PE, colour = as.factor(year))) +
  geom_line() +
  geom_smooth(method = 'gam') +
  scale_colour_viridis_d(name = '', option = 'magma', begin = 0.9, end = 0) +
  theme_bw()

#---------------------------------------#

# how to deal with 'sample' data?
sample <- fcr_targets |> 
  filter(variable == 'Secchi_m_sample', 
         !is.na(observation))  |> 
  mutate(week = week(datetime),
         year = year(datetime)) |> 
  filter(week > 12)

# Get at least a weekly time series
all_dates <- data.frame(datetime = seq.Date(min(as_date(sample$datetime)), 
                                              max(as_date(sample$datetime)), '7 days')) |> 
  mutate(year = year(datetime), 
         week = week(datetime))  |> 
  select(year, week)

x <- full_join(all_dates, sample, by = join_by(year, week)) |> 
  select(observation, datetime, week, year) |> 
  mutate(week_year = paste(week, year))


ggplot(x, aes(x=week, y = observation, colour = as.factor(year))) +
  geom_line()

calculate_PE(x$observation)

calculate_PE_ts(x = x,
                tie_method = tie_method, 
                D = D,
                tau = tau, 
                use_weights = T, 
                window_width = 4, 
                time_col = 'week_year') 
