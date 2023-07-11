library(readxl)
library(fpp3)
library(tidyverse)
library(dplyr)

# https://www.abs.gov.au/statistics/labour/employment-and-unemployment/labour-force-australia-detailed/latest-release
Quarter <- read_xlsx("6291004.xlsx", range = "Data1!A11:A164", col_names = "Quarter")
Total <- read_xlsx("6291004.xlsx", range = "Data1!BI11:BI164", col_names = "Total")

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

# Preliminary Analysis
# train |> autoplot(Total)
# train |> autoplot(log(Total))
# train |> features(Total, features = guerrero)
# train |> gg_season(Total)
# train |> gg_subseries(Total)
# train |> autoplot(difference(log(Total),m))
# train |> gg_tsdisplay(difference(log(Total),m), "partial")
# train |> gg_tsdisplay(difference(difference(log(Total),m),1), "partial")



# Model: ARIMA(2,0,2)(0,1,1)[4] w/ drift 
fit_1 <- train |> model(ARIMA(Log))
# Coefficients:
#    ar1      ar2      ma1     ma2     sma1  constant
# 1.7753  -0.8137  -0.7612  0.2615  -0.8917     7e-04
# sigma^2 estimated as 3.881e-05

fit_1_coef <- coef(fit_1) |> select(term, estimate)
# fit_1_coef[1,2] ar1 (phi_1)
# fit_1_coef[2,2] ar2 (phi_2)
# fit_1_coef[3,2] ma1 (theta_1)
# fit_1_coef[4,2] ma2 (theta_2)
# fit_1_coef[5,2] sma1 (Theta_1)
# fit_1_coef[6,2] constant

# (1 - φ₁B - φ₂B²)(1 - B⁴)yₜ = c + (1 + θ₁B + θ₂B²)(1 + Θ₁B⁴)eₜ
# yt = fit_1_coef[6,2] + (yt-4) + fit_1_coef[1,2]*(yt-1) + fit_1_coef[2,2]*(yt-2) - fit_1_coef[1,2]*(yt-5) - fit_1_coef[2,2]*(yt-6)
# + et + fit_1_coef[3,2]*(et-1) + fit_1_coef[4,2]*(et-2) + fit_1_coef[5,2]*(et-4) + fit_1_coef[3,2]*fit_1_coef[5,2]*(et-5) + fit_1_coef[4,2]*fit_1_coef[5,2]*(et-6)


resid_1_train <- residuals(fit_1) |> as_tibble() |> select(.resid)

# Calculation by hand
# mean_1_train <- numeric(R)
# mean_1_train[1] <- fit_1_coef[6,2]
# mean_1_train[2] <- fit_1_coef[6,2] + fit_1_coef[1,2]*train[1,3] + fit_1_coef[3,2]*resid_1_train[1,1]
# mean_1_train[3] <- fit_1_coef[6,2] + fit_1_coef[1,2]*train[2,3] + fit_1_coef[2,2]*train[1,3] + fit_1_coef[3,2]*resid_1_train[2,1] + fit_1_coef[4,2]*resid_1_train[1,1]
# mean_1_train[4] <- fit_1_coef[6,2] + fit_1_coef[1,2]*train[3,3] + fit_1_coef[2,2]*train[2,3] + fit_1_coef[3,2]*resid_1_train[3,1] + fit_1_coef[4,2]*resid_1_train[2,1]
# mean_1_train[5] <- fit_1_coef[6,2] + train[1,3] + fit_1_coef[1,2]*train[4,3] + fit_1_coef[2,2]*train[3,3] + fit_1_coef[3,2]*resid_1_train[4,1] + fit_1_coef[4,2]*resid_1_train[3,1] + fit_1_coef[5,2]*resid_1_train[1,1]
# mean_1_train[6] <- fit_1_coef[6,2] + train[2,3] + fit_1_coef[1,2]*train[5,3] + fit_1_coef[2,2]*train[4,3] - fit_1_coef[1,2]*train[1,3] + fit_1_coef[3,2]*resid_1_train[5,1] + fit_1_coef[4,2]*resid_1_train[4,1] + fit_1_coef[5,2]*resid_1_train[2,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[1,1]
# mean_1_train[7] <- fit_1_coef[6,2] + train[3,3] + fit_1_coef[1,2]*train[6,3] + fit_1_coef[2,2]*train[5,3] - fit_1_coef[1,2]*train[2,3] - fit_1_coef[2,2]*train[1,3] + fit_1_coef[3,2]*resid_1_train[6,1] + fit_1_coef[4,2]*resid_1_train[5,1] + fit_1_coef[5,2]*resid_1_train[3,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[2,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[1,1]
# 
# for (j in 8:R) {
#   mean_1_train[j] <- fit_1_coef[6,2] + train[j-4,3] + fit_1_coef[1,2]*train[j-1,3] + fit_1_coef[2,2]*train[j-2,3] - fit_1_coef[1,2]*train[j-5,3] - fit_1_coef[2,2]*train[j-6,3] + fit_1_coef[3,2]*resid_1_train[j-1,1] + fit_1_coef[4,2]*resid_1_train[j-2,1] + fit_1_coef[5,2]*resid_1_train[j-4,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[j-5,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[j-6,1]
# }
# 
# SE_1_train <- numeric(R) |> as.numeric()
# for (j in 1:R) {
#   SE_1_train[j] <- (mean_1_train[[j]] - train$Log[j])^2
# }
# MSE_1_train <- sum(SE_1_train[7:R]) / R
# # 2.668196
# # 0.0000335834

mean_1_train <- fitted(fit_1)$.fitted

SE_1_fit <- numeric(R) |> as.numeric()
for (j in 1:R) {
  SE_1_fit[j] <- (mean_1_train[[j]] - train$Log[j])^2
}
MSE_1_fit <- sum(SE_1_fit) / R
# 3.458818e-05


resid_1 <- numeric(P) |> as.numeric()
resid_1[1] <- test[1,3] - (fit_1_coef[6,2] + train[R-3,3] + fit_1_coef[1,2]*train[R,3] + fit_1_coef[2,2]*train[R-1,3] - fit_1_coef[1,2]*train[R-4,3] - fit_1_coef[2,2]*train[R-5,3] + fit_1_coef[3,2]*resid_1_train[R,1] + fit_1_coef[4,2]*resid_1_train[R-1,1] + fit_1_coef[5,2]*resid_1_train[R-3,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-4,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-5,1])
resid_1[2] <- test[2,3] - (fit_1_coef[6,2] + train[R-2,3] + fit_1_coef[1,2]*test[1,3] + fit_1_coef[2,2]*train[R,3] - fit_1_coef[1,2]*train[R-3,3] - fit_1_coef[2,2]*train[R-4,3] + fit_1_coef[3,2]*resid_1[1] + fit_1_coef[4,2]*resid_1_train[R,1] + fit_1_coef[5,2]*resid_1_train[R-2,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-3,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-4,1])
resid_1[3] <- test[3,3] - (fit_1_coef[6,2] + train[R-1,3] + fit_1_coef[1,2]*test[2,3] + fit_1_coef[2,2]*test[1,3] - fit_1_coef[1,2]*train[R-2,3] - fit_1_coef[2,2]*train[R-3,3] + fit_1_coef[3,2]*resid_1[2] + fit_1_coef[4,2]*resid_1[1] + fit_1_coef[5,2]*resid_1_train[R-1,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-2,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-3,1])
resid_1[4] <- test[4,3] - (fit_1_coef[6,2] + train[R,3] + fit_1_coef[1,2]*test[3,3] + fit_1_coef[2,2]*test[2,3] - fit_1_coef[1,2]*train[R-1,3] - fit_1_coef[2,2]*train[R-2,3] + fit_1_coef[3,2]*resid_1[3] + fit_1_coef[4,2]*resid_1[2] + fit_1_coef[5,2]*resid_1_train[R,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-1,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-2,1])
resid_1[5] <- test[5,3] - (fit_1_coef[6,2] + test[1,3] + fit_1_coef[1,2]*test[4,3] + fit_1_coef[2,2]*test[3,3] - fit_1_coef[1,2]*train[R,3] - fit_1_coef[2,2]*train[R-1,3] + fit_1_coef[3,2]*resid_1[4] + fit_1_coef[4,2]*resid_1[3] + fit_1_coef[5,2]*resid_1[1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-1,1])
resid_1[6] <- test[6,3] - (fit_1_coef[6,2] + test[2,3] + fit_1_coef[1,2]*test[5,3] + fit_1_coef[2,2]*test[4,3] - fit_1_coef[1,2]*test[1,3] - fit_1_coef[2,2]*train[R,3] + fit_1_coef[3,2]*resid_1[5] + fit_1_coef[4,2]*resid_1[4] + fit_1_coef[5,2]*resid_1[2] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1[1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R,1])

for (j in 7:P) {
  resid_1[j] <- test[j,3] - (fit_1_coef[6,2] + test[j-4,3] + fit_1_coef[1,2]*test[j-1,3] + fit_1_coef[2,2]*test[j-2,3] - fit_1_coef[1,2]*test[j-5,3] - fit_1_coef[2,2]*test[j-6,3] + fit_1_coef[3,2]*resid_1[j-1] + fit_1_coef[4,2]*resid_1[j-2] + fit_1_coef[5,2]*resid_1[j-4] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1[j-5] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1[j-6])
}

mean_1_test <- numeric(P)
mean_1_test[1] <- fit_1_coef[6,2] + train[R-3,3] + fit_1_coef[1,2]*train[R,3] + fit_1_coef[2,2]*train[R-1,3] - fit_1_coef[1,2]*train[R-4,3] - fit_1_coef[2,2]*train[R-5,3] + fit_1_coef[3,2]*resid_1_train[R,1] + fit_1_coef[4,2]*resid_1_train[R-1,1] + fit_1_coef[5,2]*resid_1_train[R-3,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-4,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-5,1]
mean_1_test[2] <- fit_1_coef[6,2] + train[R-2,3] + fit_1_coef[1,2]*test[1,3] + fit_1_coef[2,2]*train[R,3] - fit_1_coef[1,2]*train[R-3,3] - fit_1_coef[2,2]*train[R-4,3] + fit_1_coef[3,2]*resid_1[1] + fit_1_coef[4,2]*resid_1_train[R,1] + fit_1_coef[5,2]*resid_1_train[R-2,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-3,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-4,1]
mean_1_test[3] <- fit_1_coef[6,2] + train[R-1,3] + fit_1_coef[1,2]*test[2,3] + fit_1_coef[2,2]*test[1,3] - fit_1_coef[1,2]*train[R-2,3] - fit_1_coef[2,2]*train[R-3,3] + fit_1_coef[3,2]*resid_1[2] + fit_1_coef[4,2]*resid_1[1] + fit_1_coef[5,2]*resid_1_train[R-1,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-2,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-3,1]
mean_1_test[4] <- fit_1_coef[6,2] + train[R,3] + fit_1_coef[1,2]*test[3,3] + fit_1_coef[2,2]*test[2,3] - fit_1_coef[1,2]*train[R-1,3] - fit_1_coef[2,2]*train[R-2,3] + fit_1_coef[3,2]*resid_1[3] + fit_1_coef[4,2]*resid_1[2] + fit_1_coef[5,2]*resid_1_train[R,1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R-1,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-2,1]
mean_1_test[5] <- fit_1_coef[6,2] + test[1,3] + fit_1_coef[1,2]*test[4,3] + fit_1_coef[2,2]*test[3,3] - fit_1_coef[1,2]*train[R,3] - fit_1_coef[2,2]*train[R-1,3] + fit_1_coef[3,2]*resid_1[4] + fit_1_coef[4,2]*resid_1[3] + fit_1_coef[5,2]*resid_1[1] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1_train[R,1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R-1,1]
mean_1_test[6] <- fit_1_coef[6,2] + test[2,3] + fit_1_coef[1,2]*test[5,3] + fit_1_coef[2,2]*test[4,3] - fit_1_coef[1,2]*test[1,3] - fit_1_coef[2,2]*train[R,3] + fit_1_coef[3,2]*resid_1[5] + fit_1_coef[4,2]*resid_1[4] + fit_1_coef[5,2]*resid_1[2] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1[1] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1_train[R,1]

for (j in 7:P) {
  mean_1_test[j] <- fit_1_coef[6,2] + test[j-4,3] + fit_1_coef[1,2]*test[j-1,3] + fit_1_coef[2,2]*test[j-2,3] - fit_1_coef[1,2]*test[j-5,3] - fit_1_coef[2,2]*test[j-6,3] + fit_1_coef[3,2]*resid_1[j-1] + fit_1_coef[4,2]*resid_1[j-2] + fit_1_coef[5,2]*resid_1[j-4] + fit_1_coef[3,2]*fit_1_coef[5,2]*resid_1[j-5] + fit_1_coef[4,2]*fit_1_coef[5,2]*resid_1[j-6]
}

SE_1_test <- numeric(P) |> as.numeric()
for (j in 1:P) {
  SE_1_test[j] <- (mean_1_test[[j]] - test$Log[j])^2
}
MSE_1_test <- sum(SE_1_test) / P
# 0.000161011



# fit_1_test <- forecast(fit_1, h=P)$.mean
# 
# SE_1_fit <- numeric(P) |> as.numeric()
# for (j in 1:P) {
#   SE_1_fit[j] <- (fit_1_test[[j]] - test$Log[j])^2
# }
# MSE_1_fit <- sum(SE_1_fit) / P
# # 0.0001987525




# Model: ETS(A,A,A) 
fit_2 <- train |> model(ETS(Log))
# Smoothing parameters:
#   alpha = 0.7814383 
#   beta  = 0.417706 
#   gamma = 0.000106257 

# Initial states:
#     l[0]        b[0]        s[0]       s[-1]        s[-2]      s[-3]
# 8.779075 0.007883923 -0.00470008 0.003222494 -0.001751044 0.00322863

# sigma^2:  2710.225

fit_2_coef <- coef(fit_2) |> select(term, estimate)
resid_2_train <- residuals(fit_2) |> as_tibble() |> select(.resid)
l <- numeric(T+1) |> as.numeric()
b <- numeric(T+1) |> as.numeric()
s <- numeric(T+4) |> as.numeric()
# initial values
l[1] <- fit_2_coef[4,2]
b[1] <- fit_2_coef[5,2]
s[1] <- fit_2_coef[9,2] #-3
s[2] <- fit_2_coef[8,2] #-2
s[3] <- fit_2_coef[7,2] #-1
s[4] <- fit_2_coef[6,2] #0
# (1) - (R)
for (j in 2:(R+1)) {
  l[j] <- l[[j-1]] + b[[j-1]] + fit_2_coef[1,2]*resid_2_train[j-1,1]
  b[j] <- b[[j-1]] + fit_2_coef[2,2]*resid_2_train[j-1,1]
}
for (j in 5:(R+4)) {
  s[j] <- s[[j-4]] + fit_2_coef[3,2]*resid_2_train[j-4,1]
}

mean_2_train <- numeric(R)
for (j in 1:R) {
  mean_2_train[j] <- l[[j]] + b[[j]] + s[[j]]
}

SE_2_train <- numeric(R) |> as.numeric()
for (j in 1:R) {
  SE_2_train[j] <- (mean_2_train[[j]] - train$Log[j])^2
}

MSE_2_train <- sum(SE_2_train) / R
# 0.00003521677 (3.521677e-05)



resid_2 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  resid_2[j] <- test[j,3] - (l[[R+j]] + b[[R+j]] + s[[R+j]])
  l[R+j+1] <- l[[R+j]] + b[[R+j]] + fit_2_coef[1,2]*resid_2[j]
  b[R+j+1] <- b[[R+j]] + fit_2_coef[2,2]*resid_2[j]
  s[R+j+4] <- s[[R+j]] + fit_2_coef[3,2]*resid_2[j]
}

mean_2_test <- numeric(P)
for (j in 1:P) {
  mean_2_test[j] <- l[[R+j]] + b[[R+j]] + s[[R+j]]
}

SE_2_test <- numeric(P) |> as.numeric()
for (j in 1:P) {
  SE_2_test[j] <- (mean_2_test[[j]] - test$Log[j])^2
}
MSE_2_test <- sum(SE_2_test) / P
# 0.0002005282



# fit_2_train <- fitted(fit_2)$.fitted
# fit_2_test <- forecast(fit_2, h=P)$.mean
# 
# SE_2_fit <- numeric(R) |> as.numeric()
# for (j in 1:R) {
#   SE_2_fit[j] <- (fit_2_train[[j]] - train$Log[j])^2
# }
# MSE_2_fit <- sum(SE_2_fit) / R
# # 3.521677e-05
# 
# SE_2_fit <- numeric(P) |> as.numeric()
# for (j in 1:P) {
#   SE_2_fit[j] <- (fit_2_test[[j]] - test$Log[j])^2
# }
# MSE_2_fit <- sum(SE_2_fit) / P
# # 0.02167402







w <- seq(from = 0, to = 1, by = 0.01)

# In-sample
pool_train <- matrix(NA, length(w), R)
for (i in 1:length(w)) {
  for (j in 1:R) {
    pool_train[i,j] <-  (w[i]*mean_1_train[[j]] + (1-w[i])*mean_2_train[[j]] - train$Log[j])^2
  }
}
pool_train <- rowSums(pool_train) / R

# Remove the effects from the initial values
# pool_train <- matrix(NA, length(w), (R-6))
# for (i in 1:length(w)) {
#   for (j in 7:R) {
#     pool_train[i,j-6] <-  (w[i]*mean_1_train[[j]] + (1-w[i])*mean_2_train[[j]] - train$Log[j])^2
#   }
# }
# pool_train <- rowSums(pool_train) / R

comb <- cbind(w, pool_train) |> as_tibble()
comb |> filter(pool_train == min(comb$pool_train))
weight_optimal <- comb |> filter(pool_train == min(comb$pool_train)) |> select(w) |> as.numeric()
LS_comb_optimal <- comb |> filter(pool_train == min(comb$pool_train)) |> select(pool_train) |> as.numeric()

p1 <- comb |> ggplot(aes(w, pool_train)) +
  geom_line(color = "red") +
  labs(title = "The In-sample Combination between SARIMA and ETS(A,A,A)",
       x = "Weight on model SARIMA",
       y = "Mean squared error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Min: ", round(LS_comb_optimal,9)), vjust = -2, size = 3) +
  annotate("text", x = weight_optimal, y = LS_comb_optimal,
           label = paste0("Weight: ", round(weight_optimal,4)), vjust = -4, size = 3) +
  geom_point(aes(x = weight_optimal, y = LS_comb_optimal), size = 3, color = "orange")



# Out-of-sample
pool <- matrix(NA, length(w), P)
for (i in 1:length(w)) {
  for (j in 1:P) {
    pool[i,j] <-  (w[i]*mean_1_test[[j]] + (1-w[i])*mean_2_test[[j]] - test$Log[j])^2
  }
}
pool <- rowSums(pool) / P

comb <- cbind(w, pool) |> as_tibble()
comb |> filter(pool == min(comb$pool))
weight_test <- comb |> filter(pool == min(comb$pool)) |> select(w) |> as.numeric()
LS_comb_test <- comb |> filter(pool == min(comb$pool)) |> select(pool) |> as.numeric()
LS_comb_2 <- comb |> filter(w == weight_optimal) |> select(pool) |> as.numeric()
equ_1 <- comb |> filter(w == 0.5) |> select(pool) |> as.numeric()

p2 <- comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "The Out-of-sample Combination between SARIMA and ETS(A,A,A)",
       x = "Weight on model SARIMA",
       y = "Mean squared forecast error") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        title = element_text(size = 9),
        axis.text = element_text(size = 6)) +
  geom_point(aes(x = weight_optimal, y = LS_comb_2), size = 2, color = "orange") +
  geom_point(aes(x = weight_test, y = LS_comb_test), size = 1.5, color = "green") +
  geom_point(aes(x = 0.5, y = equ_1), size = 2, color = "blue") +
  # annotate("text", x = weight_test, y = LS_comb_test,
  #          label = paste0("Min: ", round(LS_comb_test,7)), vjust = -3, hjust = 0.8, size = 3) +
  # annotate("text", x = weight_test, y = LS_comb_test,
  #          label = paste0("Weight: ", round(weight_test,4)), vjust = -5, hjust = 0.8, size = 3) +
  annotate("text", x = weight_optimal, y = LS_comb_2,
           label = paste0("Optimal Weight: ", round(weight_optimal,4)), vjust = 2, hjust = 1.1, size = 3) +
  annotate("text", x = weight_optimal, y = LS_comb_2,
           label = paste0("MSFE: ", round(LS_comb_2,6)), vjust = 4, hjust = 1.1, size = 3) +
  annotate("text", x = 0.5, y = equ_1,
           label = paste0("Simple Average: ", round(equ_1,6)), vjust = -1, hjust = -0.1, size = 3)





library(gridExtra)
grid.arrange(p1,p2)

pdf("EMPL_correct.pdf", width = 8, height = 6)
grid.arrange(p1,p2)
dev.off()




