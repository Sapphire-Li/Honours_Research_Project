library(fpp3)
library(tidyverse)
library(dplyr)

sp <- read.csv("SP500.csv")
sp <- sp |> filter(SP500 != ".") |>
  mutate(trading_day = row_number(), SP500 = as.numeric(SP500)) |> 
  as_tsibble(index=trading_day, regular=TRUE)
sp <- sp |> mutate(log = log(SP500),
                   log_diff = difference(log(SP500)))
T <- nrow(sp)

train <- sp |> 
  filter(trading_day <= floor(nrow(sp)*.6))
R <- nrow(train)

test <- sp |> 
  filter(trading_day > floor(nrow(sp)*.6))
P <- nrow(test)


fit_4 <- train |> model(ARIMA(log))
fit_4_coef <- coef(fit_4) |> select(term, estimate)
resid_4_train <- residuals(fit_4) |> as_tibble() |> select(.resid)
mean_4_train <- numeric(R) |> as.numeric()
mean_4_train[1] <- fit_4_coef[3,2]
mean_4_train[2] <- fit_4_coef[3,2] + train[1,4] + fit_4_coef[1,2]*train[1,4] + fit_4_coef[2,2]*resid_4_train[1,]

for (j in 3:R) {
  mean_4_train[j] <- fit_4_coef[3,2] + train[j-1,4] + fit_4_coef[1,2]*(train[j-1,4] - train[j-2,4]) + fit_4_coef[2,2]*resid_4_train[j-1,]
}

sd_4 <- glance(fit_4) |> select(sigma2) |> sqrt() |> as.numeric()
pd_4_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  pd_4_train[j] <- dnorm(train[[j,4]], mean_4_train[[j]], sd_4[[1]])
}
# individual predictive density (in-sample period)
LS_4_train <- sum(log(pd_4_train[3:1511]))
LS_4_train
# 5109.8417



resid_4 <- numeric(P) |> as.numeric()
resid_4[1] <- test[1,4] - (fit_4_coef[3,2] + train[R,4] + fit_4_coef[1,2]*(train[R,4] - train[R-1,4]) + fit_4_coef[2,2]*resid_4_train[R,])
resid_4[2] <- test[2,4] - (fit_4_coef[3,2] + test[1,4] + fit_4_coef[1,2]*(test[1,4] - train[R,4]) + fit_4_coef[2,2]*resid_4[1])
for (j in 3:P) {
  resid_4[j] <- test[j,4] - (fit_4_coef[3,2] + test[j-1,4] + fit_4_coef[1,2]*(test[j-1,4] - test[j-2,4]) + fit_4_coef[2,2]*resid_4[j-1])
}
mean_4 <- numeric(P) |> as.numeric()
mean_4[1] <- fit_4_coef[3,2] + train[R,4] + fit_4_coef[1,2]*(train[R,4] - train[R-1,4]) + fit_4_coef[2,2]*resid_4_train[R,]
mean_4[2] <- fit_4_coef[3,2] + test[1,4] + fit_4_coef[1,2]*(test[1,4] - train[R,4]) + fit_4_coef[2,2]*resid_4[1]
for (j in 3:P) {
  mean_4[j] <- fit_4_coef[3,2] + test[j-1,4] + fit_4_coef[1,2]*(test[j-1,4] - test[j-2,4]) + fit_4_coef[2,2]*resid_4[j-1]
}

pd_4 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_4[j] <- dnorm(test[[j,4]], mean_4[[j]], sd_4[[1]])
}
# individual predictive density (out-of-sample period)
LS_4 <- sum(log(pd_4))
LS_4
# 2345.9262






fit_5 <- train |> model(ARIMA(log ~ trend()))
fit_5_coef <- coef(fit_5) |> select(term, estimate)
mean_5_train <- numeric(R) |> as.numeric()
mean_5_train_4 <- numeric(R) |> as.numeric()
# log(y)_4
mean_5_train[1] <- fit_5_coef[3,2] - fit_5_coef[1,2]*fit_5_coef[3,2] + fit_5_coef[2,2]
# log(y)_2 to log(y)_t
for (j in 2:R) {
  mean_5_train[j] <- fit_5_coef[1,2]*train[j-1,4] + fit_5_coef[3,2] - fit_5_coef[1,2]*fit_5_coef[3,2] + fit_5_coef[2,2]*j - fit_5_coef[1,2]*fit_5_coef[2,2]*(j-1)
}

var_5_train <- glance(fit_5) |> select(sigma2)
sd_5_train <- sqrt(var_5_train) |> as.numeric()
pd_5_train <- numeric(R-1) |> as.numeric()
for (j in 1:R) {
  pd_5_train[j] <- dnorm(train[[j,4]], mean_5_train[[j]], sd_5_train[[1]])
}
LS_5_train <- sum(log(pd_5_train[2:1511]))
LS_5_train
# 5861.1674



mean_5 <- numeric(P) |> as.numeric()
mean_5[1] <- fit_5_coef[1,2]*train[R,4] + fit_5_coef[3,2] - fit_5_coef[1,2]*fit_5_coef[3,2] + fit_5_coef[2,2]*(R+1) - fit_5_coef[1,2]*fit_5_coef[2,2]*R
for (j in 2:P) {
  mean_5[j] <- fit_5_coef[1,2]*test[j-1,4] + fit_5_coef[3,2] - fit_5_coef[1,2]*fit_5_coef[3,2] + fit_5_coef[2,2]*(R+j) - fit_5_coef[1,2]*fit_5_coef[2,2]*(R-1+j)
}
var_5 <- glance(fit_5) |> select(sigma2)
sd_5 <- sqrt(var_5) |> as.numeric()
pd_5 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_5[j] <- dnorm(test[[j,4]], mean_5[[j]], sd_5[[1]])
}
LS_5 <- sum(log(pd_5))
LS_5
# 2338.6005





w <- seq(from = 0, to = 1, by = 0.01)
pool_train <- numeric(length(w)) |> as.numeric()

# In-sample

for (j in 1:length(w)) {
  pool_train[j] <-  sum(log(w[j]*pd_4_train[3:1511] + (1-w[j])*pd_5_train[3:1511]))
}

comb <- cbind(w,pool_train) |> as_tibble()
comb |> filter(pool_train == max(comb$pool_train))
weight_optimal <- comb |> filter(pool_train == max(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal <- comb |> filter(pool_train == max(comb$pool_train)) |> select(pool_train) |> as.numeric()

p7 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(title = "ARIMA(1,1,1) and Linear Regression",
       x = "Weight on model ARIMA(1,1,1)",
       y = "Log socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Max: ", round(LS_comb_optimal,4)), vjust = 2, size = 3) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Weight: ", round(weight_optimal,4)), vjust = 4, size = 3) +
  geom_point(aes(x = weight_optimal, y = LS_comb_optimal), size = 3, color = "orange")



# Out-of-sample
pool <- numeric(length(w)) |> as.numeric()

for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_4 + (1-w[j])*pd_5))
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
weight_test <- comb |> filter(pool == max(comb$pool)) |> select(w) |> as.numeric()
LS_comb_test <- comb |> filter(pool == max(comb$pool)) |> select(pool) |> as.numeric()
LS_comb_2 <- comb |> filter(w == weight_optimal) |> select(pool) |> as.numeric()

p8 <- comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ARIMA(1,1,1) and Linear Regression",
       x = "Weight on model ARIMA(1,1,1)",
       y = "Log predictive socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  annotate("text", x = weight_test, y = LS_comb_test[2],
           label = paste0("Max: ", round(LS_comb_test[2],4)), vjust = 3, size = 3) +
  annotate("text", x = weight_test, y = LS_comb_test[2],
           label = paste0("Weight: ", round(weight_test,4)), vjust = 5, size = 3) +
  geom_point(aes(x = weight_test, y = LS_comb_test[2]), size = 3, color = "blue") +
  geom_point(aes(x = weight_optimal, y = LS_comb_2), size = 2, color = "orange") +
  annotate("text", x = weight_optimal, y = LS_comb_2,
           label = paste0("Optimal Weight: ", round(weight_optimal,4)), hjust = 1.1, size = 3)





library(gridExtra)
grid.arrange(p7,p8)

pdf("SP500.pdf", width = 14, height = 10)
grid.arrange(p7,p8)
dev.off()




