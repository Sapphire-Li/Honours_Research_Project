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


fit_1 <- train |> model(ARIMA(log))
fit_1_coef <- coef(fit_1) |> select(term, estimate)
resid_1_train <- residuals(fit_1) |> as_tibble() |> select(.resid)
mean_1_train <- numeric(R) |> as.numeric()
mean_1_train[1] <- fit_1_coef[3,2]
mean_1_train[2] <- fit_1_coef[3,2] + train[1,4] + fit_1_coef[1,2]*train[1,4] + fit_1_coef[2,2]*resid_1_train[1,]

for (j in 3:R) {
  mean_1_train[j] <- fit_1_coef[3,2] + train[j-1,4] + fit_1_coef[1,2]*(train[j-1,4] - train[j-2,4]) + fit_1_coef[2,2]*resid_1_train[j-1,]
}

sd_1 <- glance(fit_1) |> select(sigma2) |> sqrt() |> as.numeric()
pd_1_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  pd_1_train[j] <- dnorm(train[[j,4]], mean_1_train[[j]], sd_1[[1]])
}
# individual predictive density (in-sample period)
LS_1_train <- sum(log(pd_1_train[3:1511]))
LS_1_train
# 5109.8417



resid_1 <- numeric(P) |> as.numeric()
resid_1[1] <- test[1,4] - (fit_1_coef[3,2] + train[R,4] + fit_1_coef[1,2]*(train[R,4] - train[R-1,4]) + fit_1_coef[2,2]*resid_1_train[R,])
resid_1[2] <- test[2,4] - (fit_1_coef[3,2] + test[1,4] + fit_1_coef[1,2]*(test[1,4] - train[R,4]) + fit_1_coef[2,2]*resid_1[1])
for (j in 3:P) {
  resid_1[j] <- test[j,4] - (fit_1_coef[3,2] + test[j-1,4] + fit_1_coef[1,2]*(test[j-1,4] - test[j-2,4]) + fit_1_coef[2,2]*resid_1[j-1])
}
mean_1 <- numeric(P) |> as.numeric()
mean_1[1] <- fit_1_coef[3,2] + train[R,4] + fit_1_coef[1,2]*(train[R,4] - train[R-1,4]) + fit_1_coef[2,2]*resid_1_train[R,]
mean_1[2] <- fit_1_coef[3,2] + test[1,4] + fit_1_coef[1,2]*(test[1,4] - train[R,4]) + fit_1_coef[2,2]*resid_1[1]
for (j in 3:P) {
  mean_1[j] <- fit_1_coef[3,2] + test[j-1,4] + fit_1_coef[1,2]*(test[j-1,4] - test[j-2,4]) + fit_1_coef[2,2]*resid_1[j-1]
}

pd_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_1[j] <- dnorm(test[[j,4]], mean_1[[j]], sd_1[[1]])
}
# individual predictive density (out-of-sample period)
LS_1 <- sum(log(pd_1))
LS_1
# 2345.9262





fit_2 <- train |> model(ETS(log))
fit_2_coef <- coef(fit_2) |> select(term, estimate)
# epsilon ~ NID(0,sigma2)
resid_2_train <- residuals(fit_2) |> as_tibble() |> select(.resid)
level <- numeric(T+1) |> as.numeric()
# level (0)
level[1] <- fit_2_coef[2,2]
# level(1) - level(t-1): expectation of y_1 - y_t
for (j in 2:(R+1)) {
  level[j] <- level[j-1]*(1 + fit_2_coef[1,2]*resid_2_train[j-1,1])
}
mean_2_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  mean_2_train[j] <- level[[j]]
}
var_2_train_e <- glance(fit_2) |> select(sigma2) |> as.numeric()
var_2_train <- (mean_2_train^2)*var_2_train_e
sd_2_train <- sqrt(var_2_train)
pd_2_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  pd_2_train[j] <- dnorm(train[[j,4]], mean_2_train[j], sd_2_train[j])
}
# individual predictive density (in-sample period)
LS_2_train <- sum(log(pd_2_train[3:1511]))
LS_2_train
# 5111.7788



resid_2 <- numeric(P) |> as.numeric()
for(i in 1:P) {
  resid_2[i] <- (test[i,4] - level[R+i]) / level[R+i]
  level[R+1+i] <- level[R+i]*(1 + fit_2_coef[1,2]*resid_2[i])
}
mean_2 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  mean_2[j] <- level[[R+j]]
}
mean_2 <- as_tibble(mean_2)
var_2_e <- glance(fit_2) |> select(sigma2) |> as.numeric()
var_2 <- (mean_2^2)*var_2_e
sd_2 <- sqrt(var_2)
pd_2 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_2[j] <- dnorm(test[[j,4]], mean_2[[j,1]], sd_2[[j,1]])
}
# individual predictive density (out-of-sample period)
LS_2 <- sum(log(pd_2))
LS_2
# 2442.297





fit_3 <- train |> model(ARIMA(log ~ trend()))
fit_3_coef <- coef(fit_3) |> select(term, estimate)
mean_3_train <- numeric(R) |> as.numeric()
mean_3_train_1 <- numeric(R) |> as.numeric()
# log(y)_1
mean_3_train[1] <- fit_3_coef[3,2] - fit_3_coef[1,2]*fit_3_coef[3,2] + fit_3_coef[2,2]
# log(y)_2 to log(y)_t
for (j in 2:R) {
  mean_3_train[j] <- fit_3_coef[1,2]*train[j-1,4] + fit_3_coef[3,2] - fit_3_coef[1,2]*fit_3_coef[3,2] + fit_3_coef[2,2]*j - fit_3_coef[1,2]*fit_3_coef[2,2]*(j-1)
}

var_3_train <- glance(fit_3) |> select(sigma2)
sd_3_train <- sqrt(var_3_train) |> as.numeric()
pd_3_train <- numeric(R-1) |> as.numeric()
for (j in 1:R) {
  pd_3_train[j] <- dnorm(train[[j,4]], mean_3_train[[j]], sd_3_train[[1]])
}
LS_3_train <- sum(log(pd_3_train[3:1511]))
LS_3_train
# 5861.1674



mean_3 <- numeric(P) |> as.numeric()
mean_3[1] <- fit_3_coef[1,2]*train[R,4] + fit_3_coef[3,2] - fit_3_coef[1,2]*fit_3_coef[3,2] + fit_3_coef[2,2]*(R+1) - fit_3_coef[1,2]*fit_3_coef[2,2]*R
for (j in 2:P) {
  mean_3[j] <- fit_3_coef[1,2]*test[j-1,4] + fit_3_coef[3,2] - fit_3_coef[1,2]*fit_3_coef[3,2] + fit_3_coef[2,2]*(R+j) - fit_3_coef[1,2]*fit_3_coef[2,2]*(R-1+j)
}
var_3 <- glance(fit_3) |> select(sigma2)
sd_3 <- sqrt(var_3) |> as.numeric()
pd_3 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_3[j] <- dnorm(test[[j,4]], mean_3[[j]], sd_3[[1]])
}
LS_3 <- sum(log(pd_3))
LS_3
# 2338.6005





w <- seq(from = 0, to = 1, by = 0.01)
pool_train <- numeric(length(w)) |> as.numeric()

# In-sample
weight_optimal <- numeric(3) |> as.numeric()
LS_comb_optimal <- numeric(3) |> as.numeric()

for (j in 1:length(w)) {
  pool_train[j] <-  sum(log(w[j]*pd_1_train[3:1511] + (1-w[j])*pd_2_train[3:1511]))
}

comb <- cbind(w,pool_train) |> as_tibble()
comb |> filter(pool_train == max(comb$pool_train))
weight_optimal[1] <- comb |> filter(pool_train == max(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal[1] <- comb |> filter(pool_train == max(comb$pool_train)) |> select(pool_train) |> as.numeric()

p1 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(title = paste0("P(ARIMA,ETS; ", round(weight_optimal[1],4), ")"),
    caption = "ARIMA(1,1,1) and ETS(M,N,N)",
       x = "Weight on model ARIMA(1,1,1)",
       y = "Log socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  annotate("text", x = weight_optimal[1], y = LS_comb_optimal[1],
           label = paste0("Max: ", round(LS_comb_optimal[1],4)), vjust = 3, size = 4) +
  annotate("text", x = weight_optimal[1], y = LS_comb_optimal[1],
           label = paste0("Weight: ", round(weight_optimal[1],4)), vjust = 5, size = 4) +
  geom_point(aes(x = weight_optimal[1], y = LS_comb_optimal[1]), size = 3, color = "orange")



for (j in 1:length(w)) {
  pool_train[j] <-  sum(log(w[j]*pd_1_train[3:1511] + (1-w[j])*pd_3_train[3:1511]))
}

comb <- cbind(w,pool_train) |> as_tibble()
comb |> filter(pool_train == max(comb$pool_train))
weight_optimal[2] <- comb |> filter(pool_train == max(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal[2] <- comb |> filter(pool_train == max(comb$pool_train)) |> select(pool_train) |> as.numeric()

p2 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(title = paste0("P(ARIMA,LR; ", round(weight_optimal[2],4), ")"),
    caption = "ARIMA(1,1,1) and Linear Regression",
       x = "Weight on model ARIMA(1,1,1)",
       y = "Log socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  annotate("text", x = weight_optimal[2], y = LS_comb_optimal[2],
           label = paste0("Max: ", round(LS_comb_optimal[2],4)), vjust = 3, size = 4) +
  annotate("text", x = weight_optimal[2], y = LS_comb_optimal[2],
           label = paste0("Weight: ", round(weight_optimal[2],4)), vjust = 5, size = 4) +
  geom_point(aes(x = weight_optimal[2], y = LS_comb_optimal[2]), size = 3, color = "orange")



for (j in 1:length(w)) {
  pool_train[j] <-  sum(log(w[j]*pd_2_train[3:1511] + (1-w[j])*pd_3_train[3:1511]))
}

comb <- cbind(w,pool_train) |> as_tibble()
comb |> filter(pool_train == max(comb$pool_train))
weight_optimal[3] <- comb |> filter(pool_train == max(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal[3] <- comb |> filter(pool_train == max(comb$pool_train)) |> select(pool_train) |> as.numeric()

p3 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(title = paste0("P(ETS, LR; ", round(weight_optimal[3],4), ")"),
    caption = "ETS(M,N,N) and Linear Regression",
       x = "Weight on model ETS(M,N,N)",
       y = "Log socre") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  annotate("text", x = weight_optimal[3], y = LS_comb_optimal[3],
           label = paste0("Max: ", round(LS_comb_optimal[3],4)), vjust = 2, size = 4) +
  annotate("text", x = weight_optimal[3], y = LS_comb_optimal[3],
           label = paste0("Weight: ", round(weight_optimal[3],4)), vjust = 4, size = 4) +
  geom_point(aes(x = weight_optimal[3], y = LS_comb_optimal[3]), size = 3, color = "orange")



# Out-of-sample
pool <- numeric(length(w)) |> as.numeric()
weight_test <- numeric(3) |> as.numeric()
LS_comb_test <- numeric(3) |> as.numeric()

for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1 + (1-w[j])*pd_2))
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
weight_test[1] <- comb |> filter(pool == max(comb$pool)) |> select(w) |> as.numeric()
LS_comb_test[1] <- comb |> filter(pool == max(comb$pool)) |> select(pool) |> as.numeric()
LS_comb_1 <- comb |> filter(w == weight_optimal[1]) |> select(pool) |> as.numeric()
equ_1 <- comb |> filter(w == 0.5) |> select(pool) |> as.numeric()

p4 <- comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = paste0("P(ARIMA,ETS; ", round(weight_optimal[1],4), ")"),
       caption = "ARIMA(1,1,1) and ETS(M,N,N)",
       x = "Weight on model ARIMA(1,1,1)",
       y = "Log predictive socre") +
  theme_minimal() +
  # ylim(2338,2455) +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  geom_point(aes(x = weight_optimal[1], y = LS_comb_1), size = 2, color = "orange") +
  # geom_point(aes(x = weight_test[1], y = LS_comb_test[1]), size = 1.5, color = "green") +
  geom_point(aes(x = 0.5, y = equ_1), size = 2, color = "blue") +
  # annotate("text", x = weight_test[1], y = LS_comb_test[1],
  #          label = paste0("Max: ", round(LS_comb_test[1],4)), vjust = 3, size = 3) +
  # annotate("text", x = weight_test[1], y = LS_comb_test[1],
  #          label = paste0("Weight: ", round(weight_test[1],4)), vjust = 5, size = 3) +
  annotate("text", x = weight_optimal[1], y = LS_comb_1,
           label = paste0("Optimal Weight: ", round(weight_optimal[1],4)), vjust = 2, hjust = 1, size = 4) + 
  annotate("text", x = weight_optimal[1], y = LS_comb_1,
           label = paste0("LPS: ", round(LS_comb_1,4)), vjust = 4, hjust = 1, size = 4) +
  annotate("text", x = 0.5, y = equ_1,
           label = paste0("Simple Average: ", round(equ_1,4)), vjust = -1, size = 4)



for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1 + (1-w[j])*pd_3))
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
weight_test[2] <- comb |> filter(pool == max(comb$pool)) |> select(w) |> as.numeric()
LS_comb_test[2] <- comb |> filter(pool == max(comb$pool)) |> select(pool) |> as.numeric()
LS_comb_2 <- comb |> filter(w == weight_optimal[2]) |> select(pool) |> as.numeric()
equ_2 <- comb |> filter(w == 0.5) |> select(pool) |> as.numeric()

p5 <- comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = paste0("P(ARIMA,LR; ", round(weight_optimal[2],4), ")"),
       caption = "ARIMA(1,1,1) and Linear Regression",
       x = "Weight on model ARIMA(1,1,1)",
       y = "Log predictive socre") +
  theme_minimal() +
  # ylim(2338,2455) +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  geom_point(aes(x = weight_optimal[2], y = LS_comb_2), size = 2, color = "orange") +
  # geom_point(aes(x = weight_test[2], y = LS_comb_test[2]), size = 1.5, color = "green") +
  geom_point(aes(x = 0.5, y = equ_2), size = 2, color = "blue") +
  # annotate("text", x = weight_test[2], y = LS_comb_test[2],
  #          label = paste0("Max: ", round(LS_comb_test[2],4)), vjust = 3, size = 3) +
  # annotate("text", x = weight_test[2], y = LS_comb_test[2],
  #          label = paste0("Weight: ", round(weight_test[2],4)), vjust = 5, size = 3) +
  annotate("text", x = weight_optimal[2], y = LS_comb_2,
           label = paste0("Optimal Weight: ", round(weight_optimal[2],4)), vjust = 3, hjust = 0.5, size = 4) + 
  annotate("text", x = weight_optimal[2], y = LS_comb_2,
           label = paste0("LPS: ", round(LS_comb_2,4)), vjust = 5, hjust = 0.5, size = 4) +
  annotate("text", x = 0.5, y = equ_2,
           label = paste0("Simple Average: ", round(equ_2,4)), vjust = 2, hjust = -0.001, size = 4)




for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_2 + (1-w[j])*pd_3))
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
weight_test[3] <- comb |> filter(pool == max(comb$pool)) |> select(w) |> as.numeric()
LS_comb_test[3] <- comb |> filter(pool == max(comb$pool)) |> select(pool) |> as.numeric()
LS_comb_3 <- comb |> filter(w == weight_optimal[3]) |> select(pool) |> as.numeric()
equ_3 <- comb |> filter(w == 0.5) |> select(pool) |> as.numeric()

p6 <- comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = paste0("P(ETS, LR; ", round(weight_optimal[3],4), ")"),
    caption = "ETS(M,N,N) and Linear Regression",
       x = "Weight on model ETS(M,N,N)",
       y = "Log predictive socre") +
  theme_minimal() +
  # ylim(2338,2455) +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  geom_point(aes(x = weight_optimal[3], y = LS_comb_3), size = 2, color = "orange") +
  # geom_point(aes(x = weight_test[3], y = LS_comb_test[3]), size = 1.5, color = "green") +
  geom_point(aes(x = 0.5, y = equ_3), size = 2, color = "blue") +
  # annotate("text", x = weight_test[3], y = LS_comb_test[3],
  #          label = paste0("Max: ", round(LS_comb_test[3],4)), vjust = 3, hjust = 0.8, size = 3) +
  # annotate("text", x = weight_test[3], y = LS_comb_test[3],
  #          label = paste0("Weight: ", round(weight_test[3],4)), vjust = 5, hjust = 0.8, size = 3) +
  annotate("text", x = weight_optimal[3], y = LS_comb_3,
           label = paste0("Optimal Weight: ", round(weight_optimal[3],4)), vjust = 2, hjust = -0.03, size = 4) + 
  annotate("text", x = weight_optimal[3], y = LS_comb_3,
           label = paste0("LPS: ", round(LS_comb_3,4)), vjust = 4, hjust = -0.03, size = 4) +
  annotate("text", x = 0.5, y = equ_3,
           label = paste0("Simple Average: ", round(equ_3,4)), vjust = 2, size = 4)

glance(fit_1)$log_lik
glance(fit_2)$log_lik
glance(fit_3)$log_lik

glance(fit_1)$log_lik - glance(fit_2)$log_lik
glance(fit_1)$log_lik - glance(fit_3)$log_lik
glance(fit_2)$log_lik - glance(fit_3)$log_lik

library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,ncol = 3)

# pdf("SP500_nonstat_1.pdf", width = 10, height = 5)
# grid.arrange(p1,p4,ncol=2)
# dev.off()
# 
# pdf("SP500_nonstat_2.pdf", width = 10, height = 5)
# grid.arrange(p2,p5,ncol=2)
# dev.off()
# 
# pdf("SP500_nonstat_3.pdf", width = 10, height = 5)
# grid.arrange(p3,p6,ncol=2)
# dev.off()

# pdf("SP500_nonstationary.pdf", width = 14, height = 10)
# grid.arrange(p1,p2,p3,p4,p5,p6,ncol = 3)
# dev.off()




# lg <- sp |> ggplot(aes(trading_day, log)) +
#   geom_line(color = "blue") +
#   geom_vline(xintercept = R) +
#   labs(title = "Line Graph of Log S&P500 Index",
#        x = "Trading Day",
#        y = "Log of S&P500 Index") +
#   theme_minimal()
# 
# pdf("log_linegraph.pdf", width = 8, height = 6)
# print(lg)
# dev.off()




