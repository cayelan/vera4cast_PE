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
#' @param ts a dataframe of raw timeseries data (single site-variable-depth combination), needs a datetime column
#' @param out_freq string - what should the temporal frequency of the output be (hourly, weekly)
#' @param method string - how should the downsampling occur - aggregate or sample
#' @param max_distance
#' @returns a dataframe

downsample <- function(ts, 
                       out_freq = 'daily', 
                       target_out = '12:00:00',
                       max_distance = Inf,
                       method = 'sample') {
  
  ts <- ts |> 
    tsibble::as_tsibble(key = any_of(c("site_id", "depth_m", "variable")),
                      index = "datetime") |>
    tsibble::fill_gaps() 
  
  if (method == 'aggregate') {
    if (out_freq == 'hourly') {
      target_out <- '00:00'
    }
    
    if (out_freq == 'daily') {
      target_out <- '12:00:00'
    }
    
    if (out_freq == 'weekly') {
      target_out <- '1'
    }
  }
  
  
  # Do the downsampling
  if (str_detect(out_freq, pattern = 'hour')) {
    ts_downsample <- 
      ts |> 
      mutate(target = as_datetime(paste0(as_date(datetime), ' ',hour(datetime),':', target_out)), 
             hour = as_datetime(paste0(as_date(datetime), ' ', hour(datetime), ':00:00'))) |> 
      tsibble::index_by(hour) |> 
      tsibble::group_by_key() |> 
      # na.omit() |> 
      dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
                       observation_sample = nth(observation, which.min(abs(as.numeric(datetime - target)))), # takes the observation closest
                       n = n(),
                       distance = min(abs(as.numeric(datetime - target)))) |> 
      dplyr::mutate(observation_aggregate = ifelse(method == 'aggregate' & n > 3,
                                                   observation_aggregate, NA),
                    observation_sample = ifelse(method == 'sample' & distance <= max_distance,
                                                observation_sample, NA)) |> # removes if there are less than 3 obs in any one hour
      as_tibble()  |> 
      pivot_longer(cols = observation_aggregate:observation_sample,
                   values_to='observation', names_prefix = 'observation_') |> 
      filter(name == method) |> 
      rename(method = name,
             datetime = hour) |> 
      select(any_of(c("datetime", "observation", "site_id", "depth_m", "variable", "method")))
  }
  
  
  if (str_detect(out_freq, pattern = 'daily')) {
    ts_downsample <- 
      ts |> 
      mutate(target = as_datetime(paste0(as_date(datetime), ' ', target_out)), 
             date = as_date(datetime)) |> 
      tsibble::index_by(date) |> 
      tsibble::group_by_key() |> 
      # na.omit() |> 
      dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
                       observation_sample = nth(observation, which.min(abs(as.numeric(datetime - target)))), # takes the observation closest
                       n = n(),
                       distance = min(abs(as.numeric(datetime - target, 'hours')))) |> 
      dplyr::mutate(observation_aggregate = ifelse(method == 'aggregate' & n > 6*24/2,
                                                   observation_aggregate, NA),
                    observation_sample = ifelse(method == 'sample' & distance <= max_distance,
                                                observation_sample, NA)) |> 
      as_tibble()  |> 
      pivot_longer(cols = observation_aggregate:observation_sample,
                   values_to='observation', names_prefix = 'observation_') |> 
      filter(name == method) |> 
      rename(method = name) |> 
      select(any_of(c("date", "observation", "site_id", "depth_m", "variable", "method")))
  }
  
  
  
  if (str_detect(out_freq, pattern = 'week')) {
    # ts |>
    #   mutate(target = paste0(tsibble::yearweek(datetime), ' ', target_out),
    #          week_day_year = paste0(tsibble::yearweek(datetime), ' ', wday(datetime, week_start = 1)),
    #          
    #          year_week = tsibble::yearweek(datetime),
    #          dow = wday(datetime, week_start = 1)) |>
    #   tsibble::index_by(year_week) |>
    #   tsibble::group_by_key() |>
    #   # na.omit() |>
    #   dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
    #                    observation_sample = nth(observation, which.min(abs(as.numeric(dow - as.numeric(target_out))))), # takes the observation closest
    #                    n = n(),
    #                    distance = min(abs(as.numeric(dow - as.numeric(target_out), 'hours')))) |>
    #   dplyr::filter((method == 'aggregate' & n > 6*24/2) | (method == 'sample' & distance <= max_distance)) |> # removes if there are less than 3 obs in any one hour
    #   as_tibble()  |>
    #   pivot_longer(cols = observation_aggregate:observation_sample,
    #                values_to='observation', names_prefix = 'observation_') |>
    #   filter(name == method) |>
    #   rename(method = name) |>
    #   select(any_of(c("date", "observation", "site_id", "depth_m", "variable", "method")))

    stop('Havent figured weekly out yet. Do daily or hourly instead')
    

  }
  
  return(ts_downsample)
}
