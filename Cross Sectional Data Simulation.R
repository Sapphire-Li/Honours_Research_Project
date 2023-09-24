
library(tidyverse)
library(dplyr)
library(mvtnorm)
library(gridExtra)
library(latex2exp)
rm(list=ls())


set.seed(12345678)
N = 10000

beta1 <- 2
beta2 <- 2

# beta1 <- 1.2
# beta2 <- -1.1

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

fit_true <- lm(y ~ 0 + x1 + x2, train)
fit_true_R2 <- summary(fit_true)$r.squared
sse <- sum((fitted(fit_true) - train$y)^2)
R2 <- numeric(2)
partial_R2 <- numeric(2)



# Assumed Models
fit1 <- lm(y ~ 0 + x1, train)
fit1_coef <- fit1$coefficients
fit1_R2 <- summary(fit1)$r.squared
R2[1] <- fit1_R2/fit_true_R2

sse_1 <- sum((fitted(fit1) - train$y)^2)
partial_R2[1] <- (sse_1-sse)/sse_1

# What percent of the variation not explained by x1 is explained by x2.
# In other words, given x1, what additional percent of the variation can be explained by x2?

fit1_mean <- fit1_coef[1]*train$x1
fit1_sd <- sd(fit1$residuals)

fit1_pd <- numeric(R) |> as.numeric()
for (j in 1:R) {
  fit1_pd[j] <- dnorm(train[[j,1]], fit1_mean[j], fit1_sd)
}
fit1_LS <- sum(log(fit1_pd))
fit1_LS
# -17433.86




fit1_mean_test <- fit1_coef[1]*test$x1
fit1_pd_test <- numeric(P) |> as.numeric()
for (j in 1:P) {
  fit1_pd_test[j] <- dnorm(test[[j,1]], fit1_mean_test[j], fit1_sd)
}
fit1_LS_test <- sum(log(fit1_pd_test))
fit1_LS_test
# -15025.35





fit2 <- lm(y ~ 0 + x2, train)
fit2_coef <- fit2$coefficients
fit2_R2 <- summary(fit2)$r.squared
R2[2] <- fit2_R2/fit_true_R2

sse_2 <- sum((fitted(fit2) - train$y)^2)
partial_R2[2] <- (sse_2-sse)/sse_2

fit2_mean <- fit2_coef[1]*train$x2
fit2_sd <- sd(fit2$residuals)

fit2_pd <- numeric(R) |> as.numeric()
for (j in 1:R) {
  fit2_pd[j] <- dnorm(train[[j,1]], fit2_mean[j], fit2_sd)
}
fit2_LS <- sum(log(fit2_pd))
fit2_LS
# -22385.71





fit2_mean_test <- fit2_coef[1]*test$x2
fit2_pd_test <- numeric(P) |> as.numeric()
for (j in 1:P) {
  fit2_pd_test[j] <- dnorm(test[[j,1]], fit2_mean_test[j], fit2_sd)
}
fit2_LS_test <- sum(log(fit2_pd_test))
fit2_LS_test
# -11666.74





w <- seq(from = 0, to = 1, by = 0.01)

# In-sample
pool_train <- numeric(length(w)) |> as.numeric()
for (j in 1:length(w)) {
  pool_train[j] <-  sum(log(w[j]*fit1_pd + (1-w[j])*fit2_pd))
}

comb <- cbind(w,pool_train) |> as_tibble()
optimal <- comb |> filter(pool_train == max(comb$pool_train))

p1 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(caption = TeX(r'(N = 1000, $\beta_1$=1.2, $\beta_2$=-1.1, $var(X_1)$=1, $var(X_2)$=1, $cov(X_1,X_2)$=0.3)'),
    # title = TeX(r'(The density combination of $M_1$ and $M_2$)'),
       x = TeX(r'(Weight on $M_1$)'),
       y = "Log socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  geom_point(aes(x = optimal[[1]], y = optimal[[2]]), size = 3, color = "orange") +
  annotate("text", x = optimal[[1]], y = optimal[[2]],
           label = paste0("Max: ", round(optimal[[2]],4)), vjust = 3, size = 4) +
  annotate("text", x = optimal[[1]], y = optimal[[2]],
           label = paste0("Optimal Weight: ", round(optimal[[1]],4)), vjust = 5, size = 4)
  # annotate("text", x = optimal[[1]], y = optimal[[2]],
  #        label = paste0("Max: ", round(optimal[[2]],4)), vjust = 3, hjust = 0.7, size = 4) +
  # annotate("text", x = optimal[[1]], y = optimal[[2]],
  #          label = paste0("Optimal Weight: ", round(optimal[[1]],4)), vjust = 5, hjust = 0.7, size = 4)
  


# Out-of-sample
pool_test <- numeric(length(w)) |> as.numeric()
for (j in 1:length(w)) {
  pool_test[j] <-  sum(log(w[j]*fit1_pd_test + (1-w[j])*fit2_pd_test))
}

comb <- cbind(w,pool_test) |> as_tibble()
weight <- comb |> filter(pool_test == max(comb$pool_test))
opt <- comb |> filter(w == optimal[[1]]) |> select(pool_test) |> as.numeric()
equ <- comb |> filter(w == 0.5) |> select(pool_test) |> as.numeric()

p2 <- comb |> ggplot(aes(w, pool_test)) +
  geom_line(color = "red") +
  labs(caption = TeX(r'(N = 1000, $\beta_1$=1.2, $\beta_2$=-1.1, $var(X_1)$=1, $var(X_2)$=1, $cov(X_1,X_2)$=0.3)'),
    # title = "The out-of-sample combination of M1 and M2 when coefficients have different magnitudes but the same sign",
    # title = "The out-of-sample combination of M1 and M2 (N=1000)",
       x = TeX(r'(Weight on $M_1$)'),
       y = "Log predictive socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  geom_point(aes(x = optimal[[1]], y =  opt), size = 2, color = "orange") +
  geom_point(aes(x = 0.5, y = equ), size = 2, color = "blue") +
  annotate("text", x = optimal[[1]], y = opt, 
           label = paste0("Optimal Weight: ", round(optimal[[1]],4)), vjust = 3, size = 4) +
  annotate("text", x = optimal[[1]], y = opt,
           label = paste0("LPS: ", round(opt,4)), vjust = 5, size = 4) +
  annotate("text", x = 0.5, y = equ,
           label = paste0("Simple Average: ", round(equ,4)), vjust = 7, size = 4)
  # annotate("text", x = optimal[[1]], y = opt, 
  #          label = paste0("Optimal Weight: ", round(optimal[[1]],4)), vjust = 3, hjust = -0.02, size = 4) +
  # annotate("text", x = optimal[[1]], y = opt,
  #          label = paste0("LPS: ", round(opt[[2]],4)), vjust = 5, hjust = -0.05, size = 4) +
  # annotate("text", x = 0.5, y = equ,
  #          label = paste0("Simple Average: ", round(equ,4)), vjust = -1, hjust = -0.1, size = 4)






grid.arrange(p1,p2)

# pdf("ss_100.pdf", width = 5, height = 8)
# grid.arrange(p1,p2,top = 'The density combination of Model 1 and Model 2 when N is 100')
# dev.off()

# pdf("beta0.1.pdf", width = 5, height = 8)
# grid.arrange(p1,p2)
# dev.off()

# pdf("LPS_10000.pdf", width = 10, height = 5)
# grid.arrange(p1,p2,ncol=2, top = 'The density combination of Model 1 and Model 2')
# dev.off()


# ls2= log(.5*fit1_pd_test+(1-.5)*fit2_pd_test)
# ls1= log(optimal[[1]]*fit1_pd_test+(1-optimal[[1]])*fit2_pd_test)
# vars = var(ls1)+var(ls2)-2*cov(ls1,ls2)
# dms_stat=sqrt(.4*N)*mean(ls1-ls2)/vars
# dms_stat

# library(boot)
# tsboot



# fit1_R2
# fit2_R2
# fit2_R2 - fit1_R2
# # R-squared of each model
# 
# R2
# R2[2] - R2[1]
# # R-squared of each model over the total R-squared (from the true model)
# 
# partial_R2
# partial_R2[2] - partial_R2[1]
# # partial R-squared


