library(tidyverse)
# Test to code to check understanding of PE calculation

# a synthetic, short timseries to look at
# x <- c(1,4,7,7,5,7,9,12,2,10)

# try with vera targets
vera_targets <- read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz") |> 
  filter(variable == 'Chla_ugL_mean',
         site_id == 'fcre',
         depth_m == 1.6) 

all_dates <- data.frame(datetime = seq.Date(min(as_date(vera_targets$datetime)), max(as_date(vera_targets$datetime)), 'day'))  

x <- full_join(all_dates, vera_targets) |> 
  pull(observation)

# parameters needed for PE calcualtions
m <- 3 # length of the permutation
delay <- 1 # time delay
tie_method <- 'first'
#--------------------------#


#Generate the matrix
n <- length(x) - (m-1)*delay # nrows in matrix
x1 <- seq_along(x)

out <- matrix(rep(x1[seq_len(n)], m), ncol = m)
out[,-1] <- out[,-1, drop = FALSE] + rep(seq_len(m - 1) * delay, each = nrow(out))
out <- out[, seq_len(ncol(out))]
x_emb <- matrix(x[out], ncol = m)

# x_emb

## Remove run containing NAs
x_emb <- na.omit(x_emb)


# find the ordinal values
# words <-  unlist(lapply(lapply(1:nrow(x_emb), function(x) (rev(rank(-x_emb[x,])))), paste, collapse="-"))
words1 <-  unlist(lapply(lapply(1:nrow(x_emb), function(x) (rank(-x_emb[x,]))), paste, collapse="-")) # removed the rev()

wd <- table(words1)/nrow(x_emb)


# or if using weights
# words <- apply(x_emb, 1, function(i) paste(rev(rank(i, ties.method = tie_method)), collapse="-"))
words1 <- apply(x_emb, 1, function(i) paste(rank(i, ties.method = tie_method), collapse="-"))

weights <- apply(x_emb, 1, function(i) var(i))
wd <- aggregate(weights, list(words1), sum)$x / sum(weights)


numo <- -sum(wd * log2(wd))
denom <- log2(factorial(m))

numo/denom # this is the PE

#======================================#
# trying out a rolling PE
# calculate some rolling PE 
library(zoo)
library(tidyverse)
source('R/PE_functions.R')
x <- rnorm(n = 100, mean = 5, sd = 5)

calculate_PE(x, tie_method = tie_method, D = m, tau = delay)

rollapply(data = x,
          width = 30, # how many data points in each time series
          by = 1,# how far to move the window each time
          calculate_PE)
#=====================================


# try with vera targets
vera_targets <- read_csv("https://renc.osn.xsede.org/bio230121-bucket01/vera4cast/targets/project_id=vera4cast/duration=P1D/daily-insitu-targets.csv.gz") |> 
  na.omit()



targets_ts <- vera_targets |> 
  distinct(site_id, depth_m, variable)


x_vera <- filter(vera_targets,
                 variable == 'Temp_C_mean',
                 site_id == 'fcre',
                 depth_m == 1.6) |> 
  na.omit()

all_dates <- data.frame(datetime = seq.Date(min(as_date(x_vera$datetime)), 
                                            max(as_date(x_vera$datetime)), 'day'))  

x_vera  <- full_join(all_dates, x_vera) |> 
  select(datetime, observation)

D  <- 3
tau <- 1
length_ts <- nrow(x_vera)
window_length <- 30

PE <- data.frame(PE = rolling_PE(x_vera$observation,
                                 tie_method = 'first', 
                                 D = D,
                                 tau = tau, 
                                 use_weights = T, 
                                 window_width = window_length),
                 datetime = x_vera$datetime[1:(length_ts - window_length + 1)]) 

PE |> 
  mutate(doy = yday(datetime), 
         year = as.factor(year(datetime))) |> 
  filter(year != 2024, 
         year != 2018) |> 
  ggplot(aes(x=doy, y=PE, colour = year)) +
  
  geom_line() +
  # geom_smooth(method = 'gam') +
  scale_colour_viridis_d()
