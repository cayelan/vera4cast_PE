D <- 3
tau <- 1
tie_method <- 'first'

x <- targets_P1W |> 
  filter(depth_m == 'bottom', 
         site_id == 'bvre',
         variable == 'Temp_C') |> 
  pull(observation)

PE <- calculate_PE(x, tie_method = tie_method, D = D, tau = tau, use_weights = T)


# embed the data
x_emb <- embed_data(x = x, D = D, tau = tau)
x_emb <- na.omit(x_emb)

# generate the sequences by ranking 
words <- apply(x_emb, 1, function(i) paste(rank(i, ties.method = tie_method), collapse="-"))
weights <- apply(x_emb, 1, function(i) var(i))
# replace any zero weights with a very small number 
weights[weights == 0] <- 10e-10




# what are the possible observed sequences based on the D and tau selected?
all_perm <- perm(c(1:D))
# all_perm <- unique(words)
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
    transitions[i,j] <- round(length(index)/ni, 3)
  }
  
}
transitions
# Bootstrap the permutation entropy
# 1. Initial state, based on observed probabilities

# observed probabilities of each of the sequences
pni <- matrix(nrow = length(all_perm), ncol = 1)
colnames(pni) <- 'prob'
rownames(pni) <- all_perm

for (i in 1:length(all_perm)) {
  ni <- length(which(words == all_perm[i]))
  pni[i] <- ni/length(words)
}
pni


PE_b <- NULL
B <- 1000 # how many bootstraps
for (b in 1:B) {
  
  s <- vector(length = length(words), mode = 'numeric')
  
  # choosing a starting sequence based on their occurrence
  s[1] <- sample(all_perm, size = 1, replace = T, prob=pni)
  
  # for each of the next words...
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

# calculate the confidence intervals
mean_PE_b <- mean(PE_b)
sd_PE_b <- sd(PE_b)
bootstrap_bias <- mean_PE_b - PE

PE_b_bias <- sort(mean_PE_b - PE_b)
CI_level <- 0.95
alpha <- 1 - CI_level

lb_index <- alpha/2 * B
ub_index <- (1- (alpha/2)) * B

quantile_lower <- max(2*PE - mean_PE_b + PE_b_bias[lb_index], 0)
quantile_upper <- min(2*PE - mean_PE_b + PE_b_bias[ub_index], 1)

# quantile_lower <- max(mean_PE_b + PE_b_bias[lb_index] - bootstrap_bias, 0)
# quantile_upper <- min(mean_PE_b + PE_b_bias[ub_index] - bootstrap_bias, 1)

tibble(PE = PE, 
           quantile_lower = quantile_lower,
           quantile_upper = quantile_upper) |> 
  ggplot(aes(x = 1, y = PE)) +
  geom_point() +
  geom_errorbar(ymin = quantile_lower,
                ymax = quantile_upper, width = 0.2) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(limits = c(0,1))

# Example using the function
source('r/PE_functions.R')

calculate_PE(x, 
             tie_method = tie_method,
             D = D,
             tau = tau, bootstrap_CI = T,
             bootstrap_n = 100)


