#--------------------------------------#
## Project: vera4cast PE
## Script purpose: Calculates text-specific statistics/analyses for the manuscript
## Date: 2025-02-23
## Author: Cayelan Carey
#--------------------------------------#

library(tidyr)


#Methods site description
BVRdepth <- read.csv(bvre_EDI) |> 
  filter(DateTime>"2020-06-18") |> 
  select(DateTime, LvlDepth_m_13) |> 
  drop_na() |> 
  summarise(BVRmax_depth = max(LvlDepth_m_13))

FCRdepth <- read.csv(fcre_EDI) |> 
  filter(DateTime>"2020-06-18") |> 
  select(DateTime, LvlDepth_m_9) |> 
  drop_na() |> 
  summarise(FCRmax_depth = max(LvlDepth_m_9))

#Results paragraph 1
anoxia_cal <- targets_P1D |>
  filter(date>"2020-06-18",
         variable=="DO_mgL") |> 
  group_by(site_id) |> 
  count(observation<=2)
#gives number of days that meet 2 mg/L DO anoxia criteria in both reservoirs

#Results paragraph 1
spcond_median <- targets_P1D |> 
  filter(date>"2020-06-18",
         variable=="SpCond_uScm") |>
  group_by(site_id) |> 
  drop_na() |> 
  summarise(medSC = median(observation))
#gives median specific conductance in FCR vs. BVR

#Results paragraph 1
fdom_avg <- targets_P1D |> 
  filter(date>"2020-06-18",
         variable=="fDOM_QSU") |>
  group_by(site_id) |> 
  drop_na() |> 
  summarise(fdom_avg = mean(observation))

chla_avg <- targets_P1D |> 
  filter(date>"2020-06-18",
         variable=="Chla_ugL") |>
  group_by(site_id) |> 
  drop_na() |> 
  summarise(chla_avg = mean(observation))
#gives mean fDOM and chla concentrations in BVR vs FCR

#Results paragraph 2
#need to get whole-period PE for all variables
sort(1-summary_PE$PE)
#gives range of minimum and maximum PE

#Results paragraph X
depth_analysis_DO <- PE_ts_P1D |> 
  filter(date>"2020-06-18",
         variable=="DO_mgL",
         depth_m=="bottom") |> 
  group_by(site_id) |> 
  drop_na() |> 
  mutate(PE_1=1-PE) |> 
  count(PE_1>0.5)
#how many bottom DO 1-PE values are >0.5?

depth_analysis_Tw <- PE_ts_P1D |> 
  filter(date>"2020-06-18",
         variable=="Tw_C",
         depth_m=="bottom") |> 
  group_by(site_id) |> 
  drop_na() |> 
  mutate(PE_1=1-PE) |> 
  count(PE_1>0.5)
#how many bottom water temp 1-PE values are >0.5?

