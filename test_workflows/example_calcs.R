x <- c(2,1,2.5,3,0.9,1.1,2.7,2.6,0.5,0.7,0.6,2.7)
y <- c(0.9,1,1.1, 1.2,1.3,1.5, 1.4, 1.2, 1, 0.9,0.8, 0.7)
z <- c(0.1, 0.2, 0.3, 0.41, 0.53, 0.58, 0.67, 0.72, 0.84, 0.9, 0.93, 0.8)
plot(z, type = 'l')

calculate_PE(x, use_weights = F)
calculate_PE(y, use_weights = F)
calculate_PE(z, use_weights = F)


calculate_PE(x, use_weights = T)
calculate_PE(y, use_weights = T)
calculate_PE(z, use_weights = T)
