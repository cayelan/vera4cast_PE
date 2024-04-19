#' Resample timeseries
#' 
#' @param ts a vector of data 
#' @param length.out number - how long should the resampled timeseries be
#' @param n number - how many times should the sampling occur
#' @param doy vector of days associates to the ts (optional)
#' @returns a dataframe 

resample <- function(ts, length.out = 100, n = 100, doy = NA) {
  
  length_ts <- length(ts)
  
  resample_list <- NULL
  
  for (i in 1:n) {
    start_index <- floor(runif(1, min=1, max=length_ts - length.out))
    
    ts_subset <- data.frame(observation = ts[start_index:(start_index+length.out-1)],
                            index = seq(1, length.out, by=1)) |> 
      mutate(n = i)
    
    if (is.na(doy[1])) {
      resample_list <- bind_rows(ts_subset, resample_list)
    } else {
      ts_subset <- ts_subset |> 
        mutate(doy = doy[start_index])
      resample_list <- bind_rows(ts_subset, resample_list)
    }
    
    # print(i)
  }
  return(resample_list)
}

#' Downsample timeseries
#' 
#' @param ts a dataframe of timeseries data (single site-variable-depth combination), needs a datetime column
#' @param out.freq string - what should the temporal frequency of the output be (weekly, monthly)
#' @param in.freq string - what is the temporal frequency of the input - should be regular timestep
#' @param method string - how should the downsampling occur - aggregate or sample
#' @returns a dataframe

downsample <- function(ts, in.freq = 'daily', out.freq, method = 'sample') {
  
  # make in to a tsibble object with explicit gaps
  if (in.freq == 'daily') {
    ts <- ts |> 
      mutate(datetime = as_date(datetime)) |> 
      tsibble::as_tsibble(key = any_of(c("site_id", "depth_m", "variable")),
                          index = "datetime") |>
      tsibble::fill_gaps() 
  }
  
  # if the data are weekly, will find a mean if there are multiple
  if (in.freq == 'weekly') {
    ts <- ts |> 
      mutate(datetime = tsibble::yearweek(datetime)) |> 
      summarise(observation = mean(observation),
                .by = any_of(c('datetime', 'depth_m', 'variable', 'site_id'))) |> 
      tsibble::as_tsibble(key = any_of(c("site_id", "depth_m", "variable")),
                          index = "datetime") |>
      tsibble::fill_gaps()
  }
  
  
  # Do the downsampling
  if (str_detect(out.freq, pattern = 'week')) {
    ts_downsample <- 
      ts |> 
      tsibble::index_by(year_week = ~ tsibble::yearweek(.)) |> 
      tsibble::group_by_key() |> 
      dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
                       observation_sample = nth(observation, 3),
                       n = n()) |> 
      dplyr::filter(n >= 4) |> 
      as_tibble()  |> 
      pivot_longer(cols = observation_aggregate:observation_sample,
                   values_to='observation', names_prefix = 'observation_') |> 
      filter(name == method) |> 
      rename(method = name) |> 
      select(any_of(c("year_week", "observation", "site_id", "depth_m", "variable", "method")))
  }
  
  if (str_detect(out.freq, pattern='month')) {
    ts_downsample <- ts |> 
      tsibble::index_by(year_month = ~ tsibble::yearmonth(.)) |> 
      tsibble::group_by_key() |> 
      dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
                       observation_sample = nth(observation, 3),
                       n = n()) |> 
      # dplyr::filter(n >= 20) |> 
      as_tibble()  |> 
      pivot_longer(cols = observation_aggregate:observation_sample,
                   values_to='observation', names_prefix = 'observation_') |> 
      rename(method = name) |> 
      select(any_of(c("year_week", "observation", "site_id", "depth_m", "variable", "method")))
  }
  
  return(ts_downsample)
}
