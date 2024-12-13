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
