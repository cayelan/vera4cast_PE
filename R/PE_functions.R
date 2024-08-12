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
                         use_weights = T,
                         ignore_gaps = T,
                         max_missing = 0.1,
                         bootstrap_CI = F,
                         CI = 0.95,
                         bootstrap_n = 1000) {

  # message('start ', x[1], ' end ', x[length(x)])
  # figure out how many continuous missing there are
  x_missing <- data.frame(values = rle(is.na(x))$values,
                          lengths = rle(is.na(x))$lengths) |> 
    filter(values == T)
  
  if (nrow(x_missing) > 0 & !ignore_gaps) {
    if (sum(x_missing$lengths) > max_missing * length(x) | max(x_missing$lengths) > 10) {
      message(paste0('Theres too many NAs. Currently more than ', 100*max_missing, '% missing or > 10 missing consecutively.'))
      return(NA)
    }  
  }
  
  
  
  # add in test if the deltas are zero then returns 0
  if (sum(abs(x-lag(x)), na.rm = T) == 0) {
    PE <- 0
    return(PE)
  } else {
    
    # step 1 - embed data, generate matrix
    x_emb <- embed_data(x = x, D = D, tau = tau)
    ## Remove any run containing NAs
    x_emb <- x_emb[!rowSums(is.na(x_emb)),]
    
    if (is.null(nrow(x_emb))) {
      message('Not enough runs that dont contain NAs, min is 10')
      return(NA)
    } else if (nrow(x_emb) < 10 ) {
      message('Not enough runs that dont contain NAs, min is 10')
      return(NA)
    } else {
      # step 2 - calculate ordinal lengths
      wd <- calc_distr_runs(x_emb, tie_method, use_weights)
      
      # step 3 - calculate PE
      sum_weights <- -sum(wd * log2(wd))
      denom <- log2(factorial(D))
      
      PE <- sum_weights/denom 
      
      if (bootstrap_CI) {
        PE_confint <- calculate_PE_CI(PE = PE,
                                      D =  D,
                                      tau =  tau,
                                      tie_method = tie_method,
                                      x_emb =  x_emb,
                                      B = bootstrap_n,
                                      use_weights = use_weights,
                                      CI = CI)
        
        return(c(mean=PE, PE_confint))
        
      }else {
        return(PE)
      }
      
      
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
                       tau = 1) {
  
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
                  depth_m = unique(x1$depth_m)) |> 
    dplyr::rename_with(~time_col, datetime)
  
  return(PE)
}


#' calculate permutation entropy confidence intervals
#' 
#' @param PE a PE calculated from a time series
#' @param tie_method method used to break ties
#' @param D embedding dimension
#' @param tau the embedding time delay
#' @param words the sequences from the observed time series
#' @param weights calculated weights from the observed time series
#' @param n how many bootstraps should there be
#' @param CI what confidence level should be output
#' @returns a named vector
#' 
calculate_PE_CI <- function(PE, 
                            D, 
                            tau, 
                            tie_method,
                            x_emb, 
                            use_weights,
                            B = 1000, 
                            CI = 0.95) {
  
  # get the weights and words
  if (use_weights == T) {
    words <- apply(x_emb, 1, function(i) paste(rank(i, ties.method = tie_method), collapse="-"))
    
    weights <- apply(x_emb, 1, function(i) var(i))
    # replace any zero weights with a very small number 
    weights[weights == 0] <- 10e-10
  }
  
  if (use_weights != T) {
    words <-  unlist(lapply(lapply(1:nrow(x_emb), function(i) (rank(-x_emb[i,]))), paste, collapse="-")) 
  }
  
  # observed probabilities of each of the sequences
  pni <- get_sequence_probs(words = words, D = D)
  # pni
  
  transitions <- get_transitions(words = words, D = D)
  # transitions
  
  # Bootstrap the permutation entropy
  
  # Bootstrap the PE
  PE_b <- bootstrap_PE(words = words, 
                       weights = weights, 
                       transitions = transitions, 
                       sequence_probs = pni,
                       B = B, 
                       D = D)
  
  # calculate the confidence intervals
  mean_PE_b <- mean(PE_b)
  sd_PE_b <- sd(PE_b)
  
  PE_b_bias <- sort((mean_PE_b - PE_b))
  
  alpha <- 1 - CI
  
  lb_index <- alpha/2 * B
  ub_index <- (1- (alpha/2)) * B
  
  quantile_lower <- max((2*PE) - mean_PE_b + PE_b_bias[lb_index], 0)
  quantile_upper <- min((2*PE) - mean_PE_b + PE_b_bias[ub_index], 1)
  
  confidence_int <- c(quantile_lower, quantile_upper, sd_PE_b)
  names(confidence_int) <- c(paste0('quantile_', 100*(alpha/2)),
                             paste0('quantile_', 100*(1- alpha/2)),
                             'sd')
  
  
  return(confidence_int)
  
  
}

#' find the probability of the transitions
#' 
#' @param D embedding dimension
#' @param words the sequences from the observed time series
#' @returns a matrix
#'
get_transitions <- function(words, D, use_percents = T) {
  
  # what are the possible observed sequences based on the D and tau selected?
  all_perm <- perm(c(1:D))
  
  # Generate an empty matrix with columns and rows equal to possible per
  transitions <- matrix(NA, nrow = length(all_perm), ncol = length(all_perm))
  # Naming rows
  rownames(transitions) <- all_perm
  # Naming columns
  colnames(transitions) <- all_perm
  
  # at what rate do different motifs follow each other
  for (i in 1:length(all_perm)) {
    ni <- length(which(words == all_perm[i]))
    for (j in 1:length(all_perm)) {
      # Goes from i to j
      index <- which(words == all_perm[i] & lead(words) == all_perm[j])
      
      if (use_percents) {
        transitions[i,j] <- round(length(index)/ni, 3)
        
      } else {
        transitions[i,j] <- length(index)
      }
    }
    
  }
  
  return(transitions)
}


#' find the probability of the sequences
#' 
#' @param D embedding dimension
#' @param words the sequences from the observed time series
#' @returns a matrix
#'
get_sequence_probs <- function(words, D, use_percents = T) {
  # what are the possible observed sequences based on the D and tau selected?
  all_perm <- perm(c(1:D))
  
  pni <- matrix(nrow = length(all_perm), ncol = 1)
  colnames(pni) <- 'prob'
  rownames(pni) <- all_perm
  
  for (i in 1:length(all_perm)) {
    ni <- length(which(words == all_perm[i]))
    if (use_percents) {
      pni[i] <- ni/length(words)
    } else {
      pni[i] <- ni
    }
   
  }
  return(pni)
}

#' calculate bootstrap permutation entropy
#' 
#' @param D embedding dimension
#' @param words the sequences from the observed time series
#' @param weights calculated weights from the observed time series
#' @param n how many bootstraps should there be
#' @param transitions matrix of transition probabilities
#' @param sequence_probs matrix of word/sequence probabilities
#' @returns a vector
#'
bootstrap_PE <- function(words, 
                         weights, 
                         transitions, 
                         B = 1000,
                         sequence_probs = pni, 
                         D) {
  
  PE_b <- NULL

  all_perm <- perm(c(1:D)) 
  for (b in 1:B) {
    
    # 1. Initial state, based on observed probabilities
    s <- vector(length = length(words))
    
    # choosing a starting sequence based on their occurrence
    s[1] <- sample(all_perm, size = 1, replace = T, prob=sequence_probs)
    
    # for each of the next sequence...
    for (i in 2:length(words)) {
      
      #extract the probability of moving from sequence i to j 
      pij <- transitions[which(rownames(transitions) == s[i-1]),]
      
      # select the next in the sequence based on these probabilities
      s[i] <- sample(all_perm, size = 1, replace = T, prob=pij)  
    }
    # using this new words list aggregate and apply the weightings
    wd <- aggregate(weights, list(s), sum)$x / sum(weights)
    
    # wd <- table(s)/nrow(x_emb)
    
    # calculate PE
    sum_weights <- -sum(wd * log2(wd))
    denom <- log2(factorial(D))
    
    PE_b[b] <- sum_weights/denom 
    
  }
  
  return(PE_b)
}





#' function to determine all word permutations
#' 
#' @param x a vector of possible values
#' @returns a vector of all possible permutations of x, as - seperated values
perm <- function(x) {
  n <- length(x)
  if (n == 1) x
  else {
    permutations <- NULL
    for (i in 1:n) permutations <- rbind(permutations, paste0(x[i], '-', perm(x[-i])))
    as.vector(permutations)
  }
}
