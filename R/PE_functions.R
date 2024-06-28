#' calculate permutation entropy
#' 
#' @param x a time series
#' @param tie_method method used to break ties
#' @param D embedding dimension
#' @param tau the embedding time delay
#' @param use_weights should the weighted or unweighted PE be calculated?
#' @returns a number

calculate_PE <- function(x, 
                         tie_method = 'first',
                         D = 3, # Embedding dimension (3)
                         tau = 1, # Embedding time delay (1)
                         use_weights = T) {
  
  # add in test if the deltas are zero then returns 0
  if (sum(x-lag(x), na.rm = T) == 0) {
    PE <- 0
    return(PE)
  } else {
    
    # step 1 - embed data, generate matrix
    x_emb <- embed_data(x = x, D = D, tau = tau)
    ## Remove any run containing NAs
    x_emb <- na.omit(x_emb)
    if (nrow(x_emb) < 10) {
      message('Not enough runs that dont contain NAs, min is 10')
      return(NA)
    } else {
      # step 2 - calculate ordinal lengths
      wd <- calc_distr_runs(x_emb, tie_method, use_weights)
      
      # step 3 - calculate PE
      sum_weights <- -sum(wd * log2(wd))
      denom <- log2(factorial(D))
      
      PE <- sum_weights/denom 
      
      return(PE)
    }
    
  }
  
  
  
}

#' generate embedded matrix
#' 
#' @param x a time series vector
#' @param D embedding dimension
#' @param tau the embedding time delay
#' @returns a matrix

embed_data <- function(x, 
                       D = 3,
                       tau) {
  
  n <- length(x) - (D-1)*tau # nrows in matrix
  x1 <- seq_along(x)
  
  out <- matrix(rep(x1[seq_len(n)], D), ncol = D)
  out[,-1] <- out[,-1, drop = FALSE] + rep(seq_len(D - 1) * tau, each = nrow(out))
  out <- out[, seq_len(ncol(out))]
  x_emb <- matrix(x[out], ncol = D)
  
  return(x_emb)
}

#' calculate distribution of runs
#' 
#' @param x_emb a embedded matrix from embed data
#' @param tie_method method used to break ties
#' @param use_weights should the weighted or unweighted PE be calculated?
#' @returns a dataframe
calc_distr_runs <- function(x_emb, 
                            tie_method, 
                            use_weights 
) {
  if (use_weights == T) {
    words <- apply(x_emb, 1, function(i) paste(rank(i, ties.method = tie_method), collapse="-"))
    
    weights <- apply(x_emb, 1, function(i) var(i))
    # replace any zero weights with a very small number 
    weights[weights == 0] <- 10e-10
    wd <- aggregate(weights, list(words), sum)$x / sum(weights)
  }
  
  if (use_weights != T) {
    words <-  unlist(lapply(lapply(1:nrow(x_emb), function(i) (rank(-x_emb[i,]))), paste, collapse="-")) 
    wd <- table(words)/nrow(x_emb)
  }
  
  return(wd)
}


#' calculate a timeseries of permutation entropy
#' 
#' @param x a dataframe time series with explicit gaps, needs a datetime and observation column, single ts
#' @param tie_method method used to break ties
#' @param D embedding dimension
#' @param tau the embedding time delay
#' @param use_weights should the weighted or unweighted PE be calculated?
#' @param window_width how long should the time series be (the window is left aligned to the date)
#' @param time_col an alternative column name if it's not datetime
#' @returns a number
#' 
#' 
calculate_PE_ts <- function(x, # a time series, explicit gaps
                            # x needs to have a datetime, and a observation column
                            # contain only a sinlge ts (one site/variable for example)
                            tie_method = 'first', # method used to break ties
                            D = 3, # Embedding dimension (3)
                            tau = 1, # Embedding time delay (1)
                            use_weights = T,
                            window_width, 
                            time_col = 'datetime') {
  x1 <- x |> 
    # select(-any_of(time_col)) |> 
    rename(datetime = all_of(time_col))
  
  if (sum(is.na(x$observation[1:window_width])) == window_width) {
    stop('the must be at least some non-NA values in the first window. Try removing the first run of NAs before.')
  }
  
  if (length(x1$observation) == 0 | length(x1$datetime) == 0 ) {
    stop('No observation or datetime columns.')
  }
  
  
  length_ts <- nrow(x)
  
  PE <- data.frame(PE = zoo::rollapply(data = x1$observation,
                                       width = window_width, # how many data points to calculate PE over
                                       by = 1,# how far to move the window each time
                                       calculate_PE,
                                       tie_method = tie_method,
                                       D = D,
                                       tau = tau,
                                       use_weights = use_weights)) |> 
    dplyr::mutate(datetime = x1$datetime[1:(length_ts - window_width + 1)],
                  variable = unique(x1$variable),
                  site_id = unique(x1$site_id),
                  depth_m = unique(x1$depth_m)) 
  
  return(PE)
}
