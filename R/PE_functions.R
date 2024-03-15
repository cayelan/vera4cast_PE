calculate_PE <- function(x, # a time series
                         tie_method = 'first', # method used to break ties
                         D = 3, # Embedding dimension (3)
                         tau = 1, # Embedding time delay (1)
                         use_weights = T) {
  
  # step 1 - embed data, generate matrix
  x_emb <- embed_data(x = x, D = D, tau = tau)
  ## Remove any run containing NAs
  x_emb <- na.omit(x_emb)
  if (length(x_emb) == 0) {
    stop('All runs contain NAs')
  }
  # step 2 - calculate ordinal lengths
  wd <- calc_distr_runs(x_emb, tie_method, use_weights)
  
  # step 3 - calculate PE
  sum_weights <- -sum(wd * log2(wd))
  denom <- log2(factorial(D))
  
  PE <- sum_weights/denom 
  
  return(PE)
}

# function to generate the matrix
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


# function to calculate the distribution of runs
calc_distr_runs <- function(x_emb, # a embedded matrix from embed data
                            tie_method, # what method should be used to break ties
                            use_weights # should use weights?
                            ) {
  if (use_weights == T) {
    words <- apply(x_emb, 1, function(i) paste(rank(i, ties.method = tie_method), collapse="-"))
    
    weights <- apply(x_emb, 1, function(i) var(i))
    wd <- aggregate(weights, list(words), sum)$x / sum(weights)
  }
  
  if (use_weights != T) {
    words <-  unlist(lapply(lapply(1:nrow(x_emb), function(i) (rank(-x_emb[i,]))), paste, collapse="-")) 
    wd <- table(words)/nrow(x_emb)
  }
  
  return(wd)
}

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
    select(-any_of('datetime')) |> 
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
    dplyr::mutate(datetime = x1$datetime[1:(length_ts - window_width + 1)]) |> 
    purrr::set_names('PE', time_col)

  return(PE)
}
