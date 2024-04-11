#' Resample timeseries
#' 
#' @param ts a vector timeseries data 
#' @param length.out number - how long should the resampled timeseries be
#' @param n number - how many times should the sampling occur
#' @returns a dataframe 

resample <- function(ts, length.out = 100, n = 100) {

  length_ts <- length(ts)
  start_index <- floor(runif(1, min=1, max=length_ts - length.out))
  
  resample_list <- NULL
 
   for (i in 1:n) {
    ts_subset <- data.frame(observation = ts[start_index:(start_index+length.out-1)]) |> 
      mutate(n = i)
    
    resample_list <- bind_rows(ts_subset, resample_list)
  }
  return(resample_list)
}
