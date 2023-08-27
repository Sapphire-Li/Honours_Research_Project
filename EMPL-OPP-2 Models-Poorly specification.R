library(readxl)
library(fpp3)
library(tidyverse)
library(dplyr)

# https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release
Quarter <- read_xlsx("6291004.xlsx", range = "Data1!A12:A164", col_names = "Quarter")
Total <- read_xlsx("6291004.xlsx", range = "Data1!BI12:BI164", col_names = "Total")

EMPL <- cbind(Quarter, Total)
# 1978 Feb - 2023 Feb

EMPL <- EMPL |> 
  mutate(Quarter = yearquarter(Quarter)) |> 
  as_tsibble(index = "Quarter")
EMPL <- EMPL |> mutate(Log = log(Total))

T <- nrow(EMPL)
# In-sample - Training Set
train <- EMPL[1:floor(nrow(EMPL)*.6),] |> as_tsibble()
R <- nrow(train)
# Out-of-sample - Test set
test <- EMPL[(R+1):nrow(EMPL),] |> as_tsibble()
P <- nrow(test)



# Model: ARIMA(2,1,0) w/ drift
fit_3 <- train |> model(ARIMA(Log ~ 1 + PDQ(0,0,0)))
report(fit_3)
# Coefficients:
#    ar1      ar2  constant
# -0.1665  0.5790     3e-03
# sigma^2 estimated as 4.694e-05

fit_3_coef <- coef(fit_3) |> select(term, estimate)

# (1 - φ₁B - φ₂B²)(1 - B)Yₜ = c + εₜ
# (1 - φ₁B - φ₂B² - B + φ₁B² + φ₂B³)Yₜ = c + εₜ
# Yₜ = c + y(t-1)+ φ₁y(t-1) + φ₂y(t-2) - φ₁y(t-2) - φ₂y(t-3) + εₜ

# mean_3_train <- numeric(R)
# mean_3_train[1] <- fit_3_coef[3,2]
# mean_3_train[2] <- fit_3_coef[3,2] + train[1,3] + fit_3_coef[1,2]*train[1,3]
# mean_3_train[3] <- fit_3_coef[3,2] + train[2,3] + fit_3_coef[1,2]*train[2,3] + fit_3_coef[2,2]*train[1,3] - fit_3_coef[1,2]*train[1,3]
# 
# for (j in 4:R) {
#   mean_3_train[j] <- fit_3_coef[3,2] + train[j-1,3] + fit_3_coef[1,2]*train[j-1,3] + fit_3_coef[2,2]*train[j-2,3] - fit_3_coef[1,2]*train[j-2,3] - fit_3_coef[2,2]*train[j-3,3]
# }

mean_3_train <- fitted(fit_3)$.fitted

SE_3_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  SE_3_train[j] <- (mean_3_train[[j]] - train$Log[j])^2
}

MSE_3_train <- sum(SE_3_train) / R
# 0.00004539034



mean_3_test <- numeric(P) |> as.numeric()
mean_3_test[1] <- fit_3_coef[3,2] + train[R,3] + fit_3_coef[1,2]*train[R,3] + fit_3_coef[2,2]*train[R-1,3] - fit_3_coef[1,2]*train[R-1,3] - fit_3_coef[2,2]*train[R-2,3]
mean_3_test[2] <- fit_3_coef[3,2] + test[1,3] + fit_3_coef[1,2]*test[1,3] + fit_3_coef[2,2]*train[R,3] - fit_3_coef[1,2]*train[R,3] - fit_3_coef[2,2]*train[R-1,3]
mean_3_test[3] <- fit_3_coef[3,2] + test[2,3] + fit_3_coef[1,2]*test[2,3] + fit_3_coef[2,2]*test[1,3] - fit_3_coef[1,2]*test[1,3] - fit_3_coef[2,2]*train[R,3]

for (j in 4:P) {
  mean_3_test[j] <- fit_3_coef[3,2] + test[j-1,3] + fit_3_coef[1,2]*test[j-1,3] + fit_3_coef[2,2]*test[j-2,3] - fit_3_coef[1,2]*test[j-2,3] - fit_3_coef[2,2]*test[j-3,3]
}

SE_3_test <- numeric(P) |> as.numeric()
for (j in 1:P) {
  SE_3_test[j] <- (mean_3_test[[j]] - test$Log[j])^2
}
MSE_3_test <- sum(SE_3_test) / P
# 0.0002422809



# fit_3_train <- fitted(fit_3)$.fitted
# fit_3_test <- forecast(fit_3, h=P)$.mean





# Model: ETS(A,A,N)
fit_4 <- train |> model(ETS(Log ~ trend("A") + season("N")))
# Smoothing parameters:
#   alpha = 0.4823716
#   beta  = 0.4336891 

# Initial states:
#      l[0]        b[0]
#  8.779004 0.01247828

fit_4_coef <- coef(fit_4) |> select(term, estimate)
resid_4_train <- residuals(fit_4) |> as_tibble() |> select(.resid)
l <- numeric(T+1) |> as.numeric()
b <- numeric(T+1) |> as.numeric()
# initial values
l[1] <- fit_4_coef[3,2]
b[1] <- fit_4_coef[4,2]

# (1) - (R)
for (j in 1:R) {
  l[j+1] <- l[[j]] + b[[j]] + fit_4_coef[1,2]*resid_4_train[j,1]
  b[j+1] <- b[[j]] + fit_4_coef[2,2]*resid_4_train[j,1]
}

mean_4_train <- numeric(R)
for (j in 1:R) {
  mean_4_train[j] <- l[[j]] + b[[j]]
}

SE_4_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  SE_4_train[j] <- (mean_4_train[[j]] - train$Log[j])^2
}

MSE_4_train <- sum(SE_4_train) / R
# 0.00006714365 (6.714365e-05)



resid_4 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  resid_4[j] <- test[j,3] - (l[[R+j]] + b[[R+j]])
  l[R+j+1] <- l[[R+j]] + b[[R+j]] + fit_4_coef[1,2]*resid_4[j]
  b[R+j+1] <- b[[R+j]] + fit_4_coef[2,2]*resid_4[j]
}
mean_4_test <- numeric(P)
for (j in 1:P) {
  mean_4_test[j] <- l[[R+j]] + b[[R+j]]
}

SE_4_test <- numeric(P) |> as.numeric()
for (j in 1:P) {
  SE_4_test[j] <- (mean_4_test[[j]] - test$Log[j])^2
}
MSE_4_test <- sum(SE_4_test) / P
# 0.0002284498



# fit_4_train <- fitted(fit_4)$.fitted
# fit_4_test <- forecast(fit_4, h=P)$.mean




w <- seq(from = 0, to = 1, by = 0.01)

# In-sample
pool_train <- matrix(NA, length(w), R)
for (i in 1:length(w)) {
  for (j in 1:R) {
    pool_train[i,j] <-  (w[i]*mean_3_train[[j]] + (1-w[i])*mean_4_train[[j]] - train$Log[j])^2
  }
}
pool_train <- rowSums(pool_train) / R

comb <- cbind(w, pool_train) |> as_tibble()
comb |> filter(pool_train == min(comb$pool_train))
weight_optimal <- comb |> filter(pool_train == min(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal <- comb |> filter(pool_train == min(comb$pool_train)) |> select(pool_train) |> as.numeric()

p3 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(
    # title = "The In-sample Combination between ARIMA(2,1,0) w/ drift and ETS(A,A,N)",
       x = "Weight on model ARIMA",
       y = "Mean squared error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Min: ", round(LS_comb_optimal,9)), vjust = -2, size = 4) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Weight: ", round(weight_optimal,4)), vjust = -4, size = 4) +
  geom_point(aes(x = weight_optimal, y = LS_comb_optimal), size = 3, color = "orange")



# Out-of-sample
pool <- matrix(NA, length(w), P)
for (i in 1:length(w)) {
  for (j in 1:P) {
    pool[i,j] <-  (w[i]*mean_3_test[[j]] + (1-w[i])*mean_4_test[[j]] - test$Log[j])^2
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
  labs(
    # title = "The Out-of-sample Combination between ARIMA(2,1,0) w/ drift and ETS(A,A,N)",
       x = "Weight on model ARIMA",
       y = "Mean squared forecast error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 12),
        axis.text = element_text(size = 9)) +
  geom_point(aes(x = weight_optimal, y = LS_comb_4), size = 2, color = "orange") +
  # geom_point(aes(x = weight_test, y = LS_comb_test), size = 1.5, color = "green") +
  geom_point(aes(x = 0.5, y = equ_2), size = 2, color = "blue") +
  # annotate("text", x = weight_test, y = LS_comb_test,
  #          label = paste0("Min: ", round(LS_comb_test,7)), vjust = -3, hjust = 0.8, size = 3) +
  # annotate("text", x = weight_test, y = LS_comb_test,
  #          label = paste0("Weight: ", round(weight_test,4)), vjust = -5, hjust = 0.8, size = 3) +
  annotate("text", x = weight_optimal, y = LS_comb_4,
           label = paste0("Optimal Weight: ", round(weight_optimal,4)), hjust = 1.1, size = 4) +
  annotate("text", x = weight_optimal, y = LS_comb_4,
           label = paste0("MSFE: ", round(LS_comb_4,6)), vjust = 3, hjust = 1.2, size = 4) +
  annotate("text", x = 0.5, y = equ_2,
           label = paste0("Simple Average: ", round(equ_2,6)), hjust = -0.1, size = 4)



glance(fit_3)$log_lik
glance(fit_4)$log_lik

glance(fit_3)$log_lik - glance(fit_4)$log_lik


library(gridExtra)
grid.arrange(p3,p4)

# pdf("EMPL_misspecified.pdf", width = 8, height = 6)
# grid.arrange(p3,p4,nrow=1,top="The point combination between ARIMA(2,1,0) w/ drift and ETS(A,A,N) - Misspecification")
# dev.off()











