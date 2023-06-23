library(fpp3)
library(tidyverse)
library(dplyr)

# Read data
sp <- read.csv("SP500.csv")
# 2013-2023: 10 years in total

sp <- sp |> 
  filter(SP500 != ".") |>
  mutate(trading_day = row_number(), SP500 = as.numeric(SP500)) |> 
  as_tsibble(index=trading_day, regular=TRUE)

# Total number of observations
T <- nrow(sp)
# In-sample - Training Set
train <- sp |> filter(trading_day <= floor(nrow(sp)*.6))
R <- nrow(train)
# Out-of-sample - Test set
test <- sp |> filter(trading_day > floor(nrow(sp)*.6))
P <- nrow(test)

# Preliminary Analysis
# train |> autoplot(SP500)

# Use the training set to fit an ARIMA model
# The model requires a stationary series - log return of SP500
train |> gg_tsdisplay(difference(log(SP500)), plot_type = "partial")
# train |> gg_tsdisplay(difference(SP500)^2, plot_type = "partial")

train <- train |> mutate(log = log(SP500))
test <- test |> mutate(log = log(SP500))

train <- train |> mutate(return = difference(log(SP500)))


# Model: ARIMA(1,1,1) w/ drift 
fit_1 <- train |> model(ARIMA(log(SP500)))
report(fit_1)
# Coefficients:
#          ar1      ma1  constant
#       0.9478  -0.9729         0
# s.e.  0.0232   0.0166         0
# sigma^2 estimated as 6.715e-05


fit_1_coef <- coef(fit_1) |> 
  select(term, estimate)
# fit_1_coef[1,2] # ar_1
# fit_1_coef[2,2] # ma_1
# fit_1_coef[3,2] # constant


# Residual vector for time from t to T (P+1)
# Observed value - Fitted value
resid_1 <- numeric(P+1) |> as.numeric()

resid_1[1] <- residuals(fit_1) |> 
  as_tibble() |> 
  select(.resid) |> 
  tail(1) # epsilon_t

# epsilon_t+1 = y_t+1 - yhat_t+1
# y_t+1 is in the test set
# yhat_t+1 = constant + (1+ar_1) * y_t - ar_1 * y_t-1 + ma_1 * epsilon_t  
resid_1[2] <- test[1,4] - (fit_1_coef[3,2] + (1+fit_1_coef[1,2])*train[R,4] - fit_1_coef[1,2]*train[R-1,4] + fit_1_coef[2,2]*resid_1[1])

resid_1[3] <- test[2,4] - (fit_1_coef[3,2] + (1+fit_1_coef[1,2])*test[1,4] - fit_1_coef[1,2]*train[R,4] + fit_1_coef[2,2]*resid_1[2])

# epsilon_t+3 to epsilon_T
for (j in 4:(P+1)) {
  resid_1[j] <- test[j-1,4] - (fit_1_coef[3,2] + (1+fit_1_coef[1,2])*test[j-2,4] - fit_1_coef[1,2]*test[j-3,4] + fit_1_coef[2,2]*resid_1[j-1])
}


# Conditional Mean for log(SP500) from R+1 to T (P in total)
mean_1 <- numeric(P) |> as.numeric()
# E(y_t+1|t) = constant + (1+ar_1) * y_t - ar_1 * y_t-1 + ma_1 * epsilon_t
mean_1[1] <- fit_1_coef[3,2] + (1+fit_1_coef[1,2])*train[R,4] - fit_1_coef[1,2]*train[R-1,4] + fit_1_coef[2,2]*resid_1[1]

mean_1[2] <- fit_1_coef[3,2] + (1+fit_1_coef[1,2])*test[1,4] - fit_1_coef[1,2]*train[R,4] + fit_1_coef[2,2]*resid_1[2]

# Conditional mean for R+3 to T
for (j in 3:P) {
  mean_1[j] <- fit_1_coef[3,2] + (1+fit_1_coef[1,2])*test[j-1,4] - fit_1_coef[1,2]*test[j-2,4] + fit_1_coef[2,2]*resid_1[j]
}


# (Conditional) Variance for SP500
var_1 <- glance(fit_1) |> select(sigma2)
sd_1 <- sqrt(var_1) |> as.numeric()


# Predictive Density

# If the log of a variable follows a normal distribution, then the variable itself follows a log-normal distribution.
# A log-normal distribution is a probability distribution of a random variable whose logarithm follows a normal distribution. 
# Mathematically, the log-normal probability density function (pdf) can be expressed as:
#   f(x) = 1 / (x * σ * sqrt(2π)) * exp(-((ln(x) - μ)^2) / (2 * σ^2))
# where μ and σ are the mean and standard deviation of the logarithm of X, respectively.

pd_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_1[j] <- dlnorm(test[[j,2]], mean_1[[j]], sd_1[[1]])
}


LS_1 <- sum(log(pd_1)) / P
LS_1
# -5.864283








# Model: ETS(M,N,N)
fit_2 <- train |> model(ETS(SP500))
report(fit_2)
# Smoothing parameters:
#   alpha = 0.9753512 
# Initial states:
#   l[0]
# 1516.869
# sigma^2:  1e-04


fit_2_coef <- coef(fit_2) |>
  select(term, estimate)
# fit_2_coef[1,2] # alpha
# fit_2_coef[2,2] # l[0]


# Residuals from 1 to R
resid_2 <- residuals(fit_2) |>
  as_tibble() |> 
  select(.resid)

level <- numeric(T+1) |> as.numeric()
level[1] <- fit_2_coef[2,2] # l_0
# level[2] <- level[1]*(1 + fit_2_coef[1,2]*resid_2[1,1]) # l_1
# level[3] <- level[2]*(1 + fit_2_coef[1,2]*resid_2[2,1]) # l_2

# Level and Trend from initial value (0) to R (R+1 in total)
for (j in 1:R) {
  level[j+1] <- level[j]*(1 + fit_2_coef[1,2]*resid_2[j,1])
}


# Residuals from R+1 to T (P in total)
resid_2_new <- numeric(P) |> as.numeric()
for(i in 1:P) {
  resid_2_new[i] <- (test[i,2] - level[R+i]) / level[R+i]
  level[R+i+1] <- level[R+i]*(1 + fit_2_coef[1,2]*resid_2_new[i])
}


# Conditional Mean for SP500
mean_2 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  mean_2[j] <- level[[R+j]]
}
mean_2 <- as_tibble(mean_2)


# Conditional Variance for SP500
# The full prediction density can be estimated using a kernel density estimator (Silverman 1986) applied to yn+h|n.
# The forecast variance is given by
var_2 <- glance(fit_2) |> select(sigma2) |> as.numeric()
var_2 <- mean_2^2*var_2
sd_2 <- sqrt(var_2)


# Predictive Density
pd_2 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_2[j] <- dnorm(test[[j,2]], mean_2[[j,1]], sd_2[[j,1]])
}

LS_2 <- sum(log(pd_2)) / P
LS_2
# -5.837271








train |>
  model(STL(SP500 ~ trend(window = 7), 
            robust = TRUE)) |>
  components() |> autoplot()

# Model: ETS(M,A,N) 
fit_3 <- train |> model(ETS(SP500 ~ error("M") + trend("A") + season("N")))
report(fit_3)
# Smoothing parameters:
#   alpha = 0.9727122 
#   beta  = 0.0001000545 
# Initial states:
#   l[0]      b[0]
# 1530.841 0.9328007
# sigma^2:  1e-04


fit_3_coef <- coef(fit_3) |> 
  select(term, estimate)
# fit_3_coef[1,2] # alpha
# fit_3_coef[2,2] # beta
# fit_3_coef[3,2] # l[0]
# fit_3_coef[4,2] # b[0]


# Residuals from 1 to R
resid_3 <- residuals(fit_3) |> 
  as_tibble() |> select(.resid)

level <- numeric(T+1) |> as.numeric()
level[1] <- fit_3_coef[3,2] # l_0
trend <- numeric(T+1) |> as.numeric()
trend[1] <- fit_3_coef[4,2] # b_0

# level[2] <- (level[1] + trend[1])*(1 + fit_3_coef[1,2]*resid_3[1,1]) # l_1
# level[3] <- (level[2] + trend[2])*(1 + fit_3_coef[1,2]*resid_3[2,1]) # l_2
# 
# trend[2] <- trend[1] + fit_3_coef[1,2]*fit_3_coef[2,2]*(level[1] + trend[1])*resid_3[1,1] # b_1
# trend[3] <- trend[2] + fit_3_coef[1,2]*fit_3_coef[2,2]*(level[2] + trend[2])*resid_3[2,1] # b_2


# Level and Trend from initial value (0) to R (R+1 in total)
for (j in 1:R) {
  level[j+1] <- (level[[j]] + trend[[j]])*(1 + fit_3_coef[1,2]*resid_3[j,1])
  trend[j+1] <- trend[[j]] + fit_3_coef[1,2]*fit_3_coef[2,2]*(level[[j]] + trend[[j]])*resid_3[j,1]
}

# Residuals from t+1 to T
resid_3_new <- numeric(P) |> as.numeric()

for(i in 1:P) {
  resid_3_new[i] <- (test[i,2] - (level[[R+i]] + trend[[R+i]])) / ((level[[R+i]] + trend[[R+i]]))
  level[R+i+1] <- (level[[R+i]] + trend[[R+i]])*(1 + fit_3_coef[1,2]*resid_3_new[i])
  trend[R+i+1] <- trend[[R+i]] + fit_3_coef[1,2]*fit_3_coef[2,2]*(level[[R+i]] + trend[[R+i]])*resid_3_new[i]  
}


# Conditional Mean for SP500
mean_3 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  mean_3[j] <- level[[R+j]] + trend[[R+j]]
}
mean_3 <- as_tibble(mean_3)


# Conditional Variance for SP500
# The full prediction density can be estimated using a kernel density estimator (Silverman 1986) applied to yn+h|n.
# The forecast variance is given by
var_3 <- glance(fit_3) |> select(sigma2) |> as.numeric()
var_3 <- mean_3^2*var_3
sd_3 <- sqrt(var_3)


# Predictive Density
pd_3 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_3[j] <- dnorm(test[[j,2]], mean_3[[j,1]], sd_3[[j,1]])
}

LS_3 <- sum(log(pd_3)) / P
LS_3
# -5.835116








# Model: LM w/ ARIMA(1,0,0) errors 
fit_4 <- train |> model(ARIMA(SP500 ~ trend()))
report(fit_4)
# fit_4 <- train |> model(ARIMA(log(SP500) ~ trend()))
# fit_4 |> gg_tsresiduals()

# Coefficients:
#          ar1  trend()  intercept
#       0.9859   0.7904  1576.9529
# s.e.  0.0041   0.0664    59.3815
# sigma^2 estimated as 328.5

# The model can be rewritten as 
# y_t = (\beta_0 - \phi_1\beta_0 + \phi_1\beta_1)+ \phi_1 y_{t-1} + (\beta_1 - \phi_1\beta_1) t + u_t

fit_4_coef <- coef(fit_4) |> 
  select(term, estimate)
# fit_4_coef[1,2] # ar1
# fit_4_coef[2,2] # trend
# fit_4_coef[3,2] # constant


# # Residual vector for time from t to T (P+1)
# # Observed value - Fitted value
# resid_4 <- numeric(P) |> as.numeric()
# # u_2 (t=2) = train[2,2] - (fit_4_coef[3,2] - fit_4_coef[1,2]*fit_4_coef[3,2] + fit_4_coef[1,2]*fit_4_coef[2,2] + fit_4_coef[1,2]*train[1,2] + (fit_4_coef[2,2] - fit_4_coef[1,2]*fit_4_coef[2,2])*2)
# # u_t (t=R) = train[R,2] - (fit_4_coef[3,2] - fit_4_coef[1,2]*fit_4_coef[3,2] + fit_4_coef[1,2]*fit_4_coef[2,2] + fit_4_coef[1,2]*train[R-1,2] + (fit_4_coef[2,2] - fit_4_coef[1,2]*fit_4_coef[2,2])*R)
# # u_t+1 = y_t+1 - yhat_t+1
# # yhat_t+1 = (constant - ar_1*constant + ar_1*trend)+ ar_1* y_{t-1} + (trend - ar_1*trend) t
# resid_4[1] <- test[1,2] - (fit_4_coef[3,2] - fit_4_coef[1,2]*fit_4_coef[3,2] + fit_4_coef[1,2]*fit_4_coef[2,2] + fit_4_coef[1,2]*train[R,2] + (fit_4_coef[2,2] - fit_4_coef[1,2]*fit_4_coef[2,2])*(R+1))
# for (j in 2:P) {
#   resid_4[j] <- test[j,2] - (fit_4_coef[3,2] - fit_4_coef[1,2]*fit_4_coef[3,2] + fit_4_coef[1,2]*fit_4_coef[2,2] + fit_4_coef[1,2]*test[j-1,2] + (fit_4_coef[2,2] - fit_4_coef[1,2]*fit_4_coef[2,2])*(R+j))
# }


# Conditional Mean for log(SP500) from R+1 to T (P in total)
mean_4 <- numeric(P) |> as.numeric()
# E(y_t+1|t) = (\beta_0 - \phi_1\beta_0 + \phi_1\beta_1)+ \phi_1 y_{t} + (\beta_1 - \phi_1\beta_1)*(t+1)
# Conditional mean for R+1
mean_4[1] <- (fit_4_coef[3,2] - fit_4_coef[1,2]*fit_4_coef[3,2] + fit_4_coef[1,2]*fit_4_coef[2,2] + fit_4_coef[1,2]*train[R,2] + (fit_4_coef[2,2] - fit_4_coef[1,2]*fit_4_coef[2,2])*(R+1))
# Conditional mean for R+2 to T
for (j in 2:P) {
  mean_4[j] <- (fit_4_coef[3,2] - fit_4_coef[1,2]*fit_4_coef[3,2] + fit_4_coef[1,2]*fit_4_coef[2,2] + fit_4_coef[1,2]*test[j-1,2] + (fit_4_coef[2,2] - fit_4_coef[1,2]*fit_4_coef[2,2])*(R+j))
}


# (Conditional) Variance for SP500
var_4 <- glance(fit_4) |> select(sigma2)
sd_4 <- sqrt(var_4) |> as.numeric()


# Predictive Density
pd_4 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_4[j] <- dnorm(test[[j,2]], mean_4[[j]], sd_4[[1]])
}


LS_4 <- sum(log(pd_4)) / P
LS_4
# -7.472367







# Model: LM w/ ARIMA(1,0,0) errors 
fit_5 <- train |> model(ARIMA(log(SP500) ~ trend()))
report(fit_5)
# Coefficients:
#          ar1  trend()  intercept
#       0.9855    4e-04     7.3957
# s.e.  0.0043    0e+00     0.0262
# sigma^2 estimated as 6.706e-05


fit_5_coef <- coef(fit_5) |> 
  select(term, estimate)
# fit_5_coef[1,2] # ar1
# fit_5_coef[2,2] # trend
# fit_5_coef[3,2] # constant


# Conditional Mean for log(SP500) from R+1 to T (P in total)
mean_5 <- numeric(P) |> as.numeric()
# Conditional mean for R+1 to T
mean_5[1] <- (fit_5_coef[3,2] - fit_5_coef[1,2]*fit_5_coef[3,2] + fit_5_coef[1,2]*fit_5_coef[2,2] + fit_5_coef[1,2]*train[R,4] + (fit_5_coef[2,2] - fit_5_coef[1,2]*fit_5_coef[2,2])*(R+1))
for (j in 2:P) {
  mean_5[j] <- (fit_5_coef[3,2] - fit_5_coef[1,2]*fit_5_coef[3,2] + fit_5_coef[1,2]*fit_5_coef[2,2] + fit_5_coef[1,2]*test[j-1,4] + (fit_5_coef[2,2] - fit_5_coef[1,2]*fit_5_coef[2,2])*(R+j))
}


# (Conditional) Variance for SP500
var_5 <- glance(fit_5) |> select(sigma2)
sd_5 <- sqrt(var_5) |> as.numeric()


# Predictive Density
pd_5 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_5[j] <- dlnorm(test[[j,2]], mean_5[[j]], sd_5[[1]])
}


LS_5 <- sum(log(pd_5)) / P
LS_5
# -5.871551








# Find the optimal weight by optimazing the log predictive score
w <- seq(from = 0, to = 1, by = 0.01)
pool <- numeric(length(w)) |> as.numeric()

# ARIMA(1,1,1) w/ drift and ETS(M,N,N)
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1 + (1-w[j])*pd_2))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.45 
# pool = -5.79

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ARIMA(1,1,1) w/ drift, ETS(M,N,N)",
       x = "Weight on model ARIMA",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



# ARIMA(1,1,1) w/ drift and ETS(M,A,N)
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1 + (1-w[j])*pd_3))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.43 
# pool = -5.80

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ARIMA(1,1,1) w/ drift, ETS(M,A,N)",
       x = "Weight on model ARIMA",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



# ETS(M,N,N) and ETS(M,A,N)
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_2 + (1-w[j])*pd_3))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.08 
# pool = -5.84

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ETS(M,N,N), ETS(M,A,N)",
       x = "Weight on model ETS(M,N,N)",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



# ARIMA(1,1,1) w/ drift and LM
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1 + (1-w[j])*pd_5))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.49 
# pool = -2.95

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ARIMA(1,1,1) w/ drift, LM",
       x = "Weight on model ARIMA",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



# ETS(M,N,N) and LM
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_2 + (1-w[j])*pd_5))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.49 
# pool = -2.92

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ETS(M,N,N), LM",
       x = "Weight on model ETS(M,N,N)",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



# ETS(M,A,N) and LM
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_3 + (1-w[j])*pd_5))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.49 
# pool = -2.92

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ETS(M,A,N), LM",
       x = "Weight on model ETS(M,A,N)",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



