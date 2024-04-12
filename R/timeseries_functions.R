#' Resample timeseries
#' 
#' @param ts a vector of data 
#' @param length.out number - how long should the resampled timeseries be
#' @param n number - how many times should the sampling occur
#' @returns a dataframe 

resample <- function(ts, length.out = 100, n = 100) {

  length_ts <- length(ts)
 
  resample_list <- NULL
 
  for (i in 1:n) {
    start_index <- floor(runif(1, min=1, max=length_ts - length.out))
    
    ts_subset <- data.frame(observation = ts[start_index:(start_index+length.out-1)]) |> 
      mutate(n = i)
    
    resample_list <- bind_rows(ts_subset, resample_list)
    # print(i)
  }
  return(resample_list)
}

#' Downsample timeseries
#' 
#' @param ts a dataframe of timeseries data (single site-variable-depth combination)
#' @param out.freq number - what should the temporal frequency of the output be (daily, weekly, monthly)
#' @param method string - how should the downsampling occur - aggregate or subset
#' @returns a dataframe

downsample <- function(ts, out.freq, method = 'subset') {
  
 # make in to a tsibble object with explicit gaps
  x <- tsibble::as_tsibble(x, key = any_of(c("site_id", "depth_m", "variable")),
                           index = "datetime") |>
    tsibble::fill_gaps(observation)  
    

  # x_interval <- x |> 
  #   tsibble::interval() |> 
  #   unlist()
  # 
  # x_interval <- names(x_interval)[which(x_interval != 0)]
  #   
  
  if (str_detect(out.freq, pattern = 'week')) {
    x_downsample <- 
      x |> 
      tsibble::index_by(year_week = ~ yearweek(.)) |> 
      tsibble::group_by_key() |> 
      dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
                       observation_subset = nth(observation, 3),
                       n = n()) |> 
      dplyr::filter(n >= 4) |> 
      as_tibble()  |> 
      pivot_longer(cols = observation_aggregate:observation_subset,
                   values_to='observation', names_prefix = 'observation_') |> 
      filter(name == method) |> 
      select(any_of(c("year_week", "observation", "site_id", "depth_m", "variable")))
  }
  
  if (str_detect(out.freq, pattern='month')) {
    x_downsample <- x |> 
      tsibble::index_by(year_month = ~ yearmonth(.)) |> 
      tsibble::group_by_key() |> 
      dplyr::summarise(observation_aggregate = mean(observation, na.rm = T),
                       observation_subset = nth(observation, 3),
                       n = n()) |> 
      dplyr::filter(n >= 20) |> 
      as_tibble()  |> 
      pivot_longer(cols = observation_aggregate:observation_subset,
                   values_to='observation', names_prefix = 'observation_') |> 
      filter(name == method) |> 
      select(any_of(c("year_month", "observation", "site_id", "depth_m", "variable")))
  }
  
  return(x_downsample)
}
