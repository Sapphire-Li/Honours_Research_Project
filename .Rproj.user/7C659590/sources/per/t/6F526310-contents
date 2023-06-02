
library(tidyverse)
library(dplyr)
library(mvtnorm)

set.seed(12345678)
R = 100

beta0 <- 1
beta1 <- 2
beta2 <- 3
Beta <- matrix(c(beta1, beta2), nrow = 2)
X_varcov <- matrix(c(1,0.7,0.7,1), ncol = 2)
X <- rmvnorm(100, mean = c(0,0), sigma = X_varcov)
colnames(X) <- c('x1','x2')
error <- rnorm(100, 5, 10)

# The True Model

Y <- Beta0 + X%*%Beta + error


# Assumed Models








