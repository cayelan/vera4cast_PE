library(tidyverse)
library(zoo)
library(RCurl)

source('R/get_targets.R')
source('R/PE_functions.R')
source('R/timeseries_functions.R')

# ================= Get targets ===========================
# Get observational target data
secchi <- "https://pasta.lternet.edu/package/data/eml/edi/198/11/81f396b3e910d3359907b7264e689052"
chem <-  "https://pasta.lternet.edu/package/data/eml/edi/199/12/a33a5283120c56e90ea414e76d5b7ddb"


# get the targets data
targets_sample <- get_targets_sample(infiles = c(chem, secchi), start_date = 2014, end_date = 2023) |> 
  filter((depth_m %in% c(0.1, 9) & variable %in% c('TN_ugL', 'TP_ugL', 'SRP_ugL', 'NH4_ugL', 'NO3NO2_ugL')) |
           variable == 'Secchi_m') |> 
  pivot_wider(id_cols = c(site_id, datetime, depth_m),
              names_from = variable,
              values_from = observation) |> 
  mutate(DIN_ugL = NH4_ugL + NO3NO2_ugL) |> 
  select(-NH4_ugL, -NO3NO2_ugL) |> 
  pivot_longer(SRP_ugL:DIN_ugL, 
               names_to = 'variable', 
               values_to = 'observation') |> 
  filter(!is.na(observation)) |> 
  mutate(year_week = tsibble::yearweek(datetime),
         observation = as.numeric(observation)) |> 
  reframe(observation = mean(observation, na.rm = T),
          .by = c(site_id, year_week, depth_m, variable)) |> 
  tsibble::as_tsibble(key = any_of(c("site_id", "depth_m", "variable")),
                      index = "year_week") |>
  tsibble::fill_gaps() |> 
  filter(month(year_week) %in% 4:11, 
         year(year_week) >= 2014) |> 
  as_tibble()

#===================================================

# Calculate PE for each year separately?
targets_sample |> 
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
  facet_wrap(~depth_m, ncol = 3) + theme(legend.position = 'top')+
  scale_colour_viridis_d(option = 'C', name = 'site', end = 0.9) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1))



targets_sample |> 
  arrange(site_id, variable, depth_m, year_week)|> 
  mutate(year = year(year_week)) |>
  # calculate the PE for each year seperately 
  group_by(year, depth_m, variable, site_id) |> 
  summarise(PE = calculate_PE(observation), .groups = 'drop')  |> 
  ggplot(aes(y = PE, x = year, colour = site_id)) +
  geom_point() +
  labs(x='Variable', y = 'Normalised PE') +
  theme_bw() +
  facet_grid(depth_m~variable) + theme(legend.position = 'top')+
  scale_y_continuous(expand = c(0,0), limits = c(0,1))


targets_sample |> 
  filter(site_id == 'bvre', 
         variable == 'TN_ugL',
         depth_m == 9) |> 
  mutate(week = week(year_week), 
         year = year(year_week)) |> 
  filter(year %in% c(2021, 2022)) |> 
  ggplot(aes(x=week, y= observation, colour = as_factor(year))) +
  geom_line() +
  geom_point()


targets_sample |> 
  filter(site_id == 'bvre', 
         variable == 'TN_ugL',
         depth_m == 9) |> 
  mutate(week = week(year_week), 
         year = year(year_week)) |> 
  filter(year %in% c(2022)) |> 
  summarise(calculate_PE(observation))

#===============================================