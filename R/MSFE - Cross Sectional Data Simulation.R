
library(tidyverse)
library(dplyr)
library(mvtnorm)
library(gridExtra)
library(latex2exp)
rm(list=ls())


set.seed(12345678)
N = 1000

beta1 <- 2
beta2 <- 2

Beta <- matrix(c(beta1, beta2), nrow = 2)
X_varcov <- matrix(c(1,0.3,0.3,1), ncol = 2)
X <- rmvnorm(N, mean = c(0,0), sigma = X_varcov)
error <- rnorm(N, 0, 4)




# The True Model
Y <- X%*%Beta + error

Data <- cbind(Y,X)
colnames(Data) <- c('y','x1','x2')
Data <- Data |> as_tibble() |> mutate(row = (1:N))

# Split the dataset (60% and 40%)
# In-sample
train <- Data |> filter(row <= nrow(Data)*.6) |> select(1:3)
R <- nrow(train)
# Out-of-sample
test <- Data |> filter(row > nrow(Data)*.6) |> select(1:3)
P <- nrow(test)




# Assumed Models
fit1 <- lm(y ~ 0 + x1, train)
fit1_coef <- fit1$coefficients

fit1_mean <- fit1_coef[1]*train$x1

fit1_SE <- numeric(R) |> as.numeric()
for (j in 1:R) {
  fit1_SE[j] <- (fit1_mean[[j]] - train$y[j])^2
}
fit1_MSE_train <- sum(fit1_SE) / R
fit1_MSE_train



fit1_mean_test <- fit1_coef[1]*test$x1

fit1_SE <- numeric(P) |> as.numeric()
for (j in 1:P) {
  fit1_SE[j] <- (fit1_mean_test[[j]] - test$y[j])^2
}
fit1_MSE_test <- sum(fit1_SE) / P
fit1_MSE_test




fit2 <- lm(y ~ 0 + x2, train)
fit2_coef <- fit2$coefficients

fit2_mean <- fit2_coef[1]*train$x2

fit2_SE <- numeric(R) |> as.numeric()
for (j in 1:R) {
  fit2_SE[j] <- (fit2_mean[[j]] - train$y[j])^2
}
fit2_MSE_train <- sum(fit2_SE) / R
fit2_MSE_train



fit2_mean_test <- fit2_coef[1]*test$x2

fit2_SE <- numeric(P) |> as.numeric()
for (j in 1:P) {
  fit2_SE[j] <- (fit2_mean_test[[j]] - test$y[j])^2
}
fit2_MSE_test <- sum(fit2_SE) / P
fit2_MSE_test






w <- seq(from = 0, to = 1, by = 0.01)

# In-sample
pool_train <- matrix(NA, length(w), R)
for (i in 1:length(w)) {
  for (j in 1:R) {
    pool_train[i,j] <-  (w[i]*fit1_mean[[j]] + (1-w[i])*fit2_mean[[j]] - train$y[j])^2
  }
}
pool_train <- rowSums(pool_train) / R

comb <- cbind(w, pool_train) |> as_tibble()
comb |> filter(pool_train == min(comb$pool_train))
weight_optimal <- comb |> filter(pool_train == min(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal <- comb |> filter(pool_train == min(comb$pool_train)) |> select(pool_train) |> as.numeric()

p3 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(caption = TeX(r'(N = 10000, $\beta_1$=2, $\beta_2$=2, $var(X_1)$=1, $var(X_2)$=1, $cov(X_1,X_2)$=0.3)'),
       x = TeX(r'(Weight on $M_1$)'),
       y = "Mean squared error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Min: ", round(LS_comb_optimal,9)), vjust = -3, size = 4) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Optimal Weight: ", round(weight_optimal,4)), vjust = -5, size = 4) +
  geom_point(aes(x = weight_optimal, y = LS_comb_optimal), size = 3, color = "orange")



# Out-of-sample
pool <- matrix(NA, length(w), P)
for (i in 1:length(w)) {
  for (j in 1:P) {
    pool[i,j] <-  (w[i]*fit1_mean_test[[j]] + (1-w[i])*fit2_mean_test[[j]] - test$y[j])^2
  }
}
pool <- rowSums(pool) / P

comb <- cbind(w, pool) |> as_tibble()
comb |> filter(pool == min(comb$pool))
weight_test <- comb |> filter(pool == min(comb$pool)) |> select(w) |> as.numeric()
LS_comb_test <- comb |> filter(pool == min(comb$pool)) |> select(pool) |> as.numeric()
LS_comb_4 <- comb |> filter(w == weight_optimal) |> select(pool) |> as.numeric()
equ_2 <- comb |> filter(w == 0.5) |> select(pool) |> as.numeric()

p4 <- comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(caption = TeX(r'(N = 10000, $\beta_1$=2, $\beta_2$=2, $var(X_1)$=1, $var(X_2)$=1, $cov(X_1,X_2)$=0.3)'),
       x = TeX(r'(Weight on $M_1$)'),
       y = "Mean squared forecast error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  geom_point(aes(x = weight_optimal, y = LS_comb_4), size = 2, color = "orange") +
  geom_point(aes(x = 0.5, y = equ_2), size = 2, color = "blue") +
  annotate("text", x = weight_optimal, y = LS_comb_4,
           label = paste0("Optimal Weight: ", round(weight_optimal,4)), vjust = -5, size = 4) +
  annotate("text", x = weight_optimal, y = LS_comb_4,
           label = paste0("MSFE: ", round(LS_comb_4,4)), vjust = -3, size = 4) +
  annotate("text", x = 0.5, y = equ_2,
           label = paste0("Simple Average: ", round(equ_2,4)), vjust = -7, size = 4)

grid.arrange(p3,p4)


# pdf("MSFE.pdf", width = 10, height = 5)
# grid.arrange(p3,p4,ncol=2,top = 'The point combination of Model 1 and Model 2')
# dev.off()




