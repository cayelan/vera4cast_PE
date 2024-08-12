# How to split the dataframe up to just have bits of the time series that don't have any gaps?

test <- targets_P1W %>% 
  mutate(group = paste0('grp_', cumsum(is.na(observation)))) |> 
  # Group the data frame by consecutive NAs in the `y` column
  group_by(group, variable, depth_m, site_id) |> 
  # Remove groups that have more than one NA in the `observation` column
  filter(!(is.na(observation) & n() > 1)) |> 
  filter(n() > 30) |> 
  # Split the data frame into a list of data frames based on the groups
  group_split(.keep = F)

PE_test <- list()
sample_PE <- vector()

for (i in 1:length(test)) {
  PE_test[[i]] <- test[[i]] |> 
    reframe(PE = calculate_PE(observation, bootstrap_CI = T, bootstrap_n = 1000, CI = 0.9),
            names = names(PE)) |> 
    pivot_wider(names_from = names, values_from = PE)
  
  
}


calculate_PE(test[[2]]$observation, bootstrap_CI = T)
plot(test[[2]]$observation)


