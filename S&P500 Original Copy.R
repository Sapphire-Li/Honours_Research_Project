library(fpp3)
library(tidyverse)
library(dplyr)

# Read data and make some modifications
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
pd_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_1[j] <- dnorm(test[[j,4]], mean_1[[j]], sd_1[[1]])
}


LS_1 <- sum(log(pd_1)) / P
LS_1
# 2.327308


# If the log of a variable follows a normal distribution, then the variable itself follows a log-normal distribution.
# A log-normal distribution is a probability distribution of a random variable whose logarithm follows a normal distribution. 
# Mathematically, the log-normal probability density function (pdf) can be expressed as:
#   f(x) = 1 / (x * σ * sqrt(2π)) * exp(-((ln(x) - μ)^2) / (2 * σ^2))
# where μ and σ are the mean and standard deviation of the logarithm of X, respectively.

pd_1b <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_1b[j] <- dlnorm(test[[j,2]], mean_1[[j]], sd_1[[1]])
}


LS_1b <- sum(log(pd_1b)) / P
LS_1b
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


fit_4_coef <- coef(fit_4) |> 
  select(term, estimate)
# fit_4_coef[1,2] # ar1
# fit_4_coef[2,2] # trend
# fit_4_coef[3,2] # constant


# Residual vector for time from t to T (P+1)
# Observed value - Fitted value
resid_4 <- numeric(P+1) |> as.numeric()

resid_4[1] <- residuals(fit_4) |> 
  as_tibble() |> 
  select(.resid) |> 
  tail(1) # epsilon_t


# epsilon_t+1 = y_t+1 - yhat_t+1
# y_t+1 is in the test set
# yhat_t+1 = constant + trend * (t+1) + ar_1 * epsilon_t  
# epsilon_t+1 to epsilon_T
for (j in 2:(P+1)) {
  resid_4[j] <- test[j-1,2] - (fit_4_coef[3,2] + fit_4_coef[2,2]*(R+j-1) + fit_4_coef[1,2]*resid_4[j-1])
}


# Conditional Mean for log(SP500) from R+1 to T (P in total)
mean_4 <- numeric(P) |> as.numeric()
# E(y_t+1|t) = constant + trend * (t+1) + ar_1 * epsilon_t
# Conditional mean for R+1 to T
for (j in 1:P) {
  mean_4[j] <- fit_4_coef[3,2] + fit_4_coef[2,2]*(R+j) + fit_4_coef[1,2]*resid_4[j]
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


# Residual vector for time from t to T (P+1)
# Observed value - Fitted value
resid_5 <- numeric(P+1) |> as.numeric()

resid_5[1] <- residuals(fit_5) |> 
  as_tibble() |> 
  select(.resid) |> 
  tail(1) # epsilon_t

# -1.214400e-02  1.932038e-04
# -7.907420e-04 -6.891591e-04

for (j in 2:(P+1)) {
  resid_5[j] <- test[j-1,4] - (fit_5_coef[3,2] + fit_5_coef[2,2]*(R+j-1) + fit_5_coef[1,2]*resid_5[j-1])
}


# Conditional Mean for log(SP500) from R+1 to T (P in total)
mean_5 <- numeric(P) |> as.numeric()
# E(y_t+1|t) = constant + trend * (t+1) + ar_1 * epsilon_t
# Conditional mean for R+1 to T
for (j in 1:P) {
  mean_5[j] <- fit_5_coef[3,2] + fit_5_coef[2,2]*(R+j) + fit_5_coef[1,2]*resid_5[j]
}


# (Conditional) Variance for SP500
var_5 <- glance(fit_5) |> select(sigma2)
sd_5 <- sqrt(var_5) |> as.numeric()


# Predictive Density
pd_5 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_5[j] <- dnorm(test[[j,4]], mean_5[[j]], sd_5[[1]])
}


LS_5 <- sum(log(pd_5)) / P
LS_5
# -30.0727








# Find the optimal weight by optimazing the log predictive score
w <- seq(from = 0, to = 1, by = 0.01)
pool <- numeric(length(w)) |> as.numeric()

# ARIMA(1,1,1) w/ drift and ETS(M,N,N)
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1b + (1-w[j])*pd_2))/P
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
  pool[j] <-  sum(log(w[j]*pd_1b + (1-w[j])*pd_3))/P
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
  pool[j] <-  sum(log(w[j]*pd_1b + (1-w[j])*pd_5))/P
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













return <- train |> model(ARIMA(return^2))





# 分数据方法3 - 2000平分各为1000
# sp_2 <- sp |> 
#   filter(trading_day > 519) |> 
#   mutate(trading_day = row_number())
# t <- 1000
# T <- nrow(sp_2) - t
# train_3 <- sp_2 |> filter(trading_day <= t)
# test_3 <- sp_2 |> filter(trading_day > t)

# Use the training set to fit the model
fit <- train_3 |> 
  model(
    arma = ARIMA(SP500 ~ 1 + pdq(1,0,1)),
    ets = ETS(SP500 ~ error("M") + trend("A") + season("N"))
  )


diff(log(sp$SP500))^2 # stationary

# Model: ARIMA(1,0,1) w/ mean 
fit |> 
  select(arma) |> 
  report()

# Coefficients:
#   ar1      ma1  constant
# 0.9980  -0.0125    4.7688
# s.e.  0.0017   0.0332    0.4532

# sigma^2 estimated as 412.9



arma_coef <- coef(fit) |> 
  filter(.model == "arma") |> 
  select(term, estimate)

# arma_coef[1,2] # phi_1
# arma_coef[2,2] # theta_1
# arma_coef[3,2] # phi_0


# arma_resid <- numeric(T) |> as_tibble()
# arma_resid[1,1]

# Residuals from t to T
# Observed value - Fitted value
arma_resid <- numeric(T+1) |> as.numeric()

# epsilon_t
arma_resid[1] <- residuals(fit) |> 
  filter(.model == "arma") |> 
  as_tibble() |> 
  select(.resid) |> 
  tail(1)

# epsilon_t+1 = y_t+1 - phi_0 - phi_1 * y_t - theta_1 * epsilon_t
arma_resid[2] <- test_3[1,2] - arma_coef[3,2] - arma_coef[1,2]*train_3[t,2] - arma_coef[2,2]*arma_resid[1]

# epsilon_t+2 to epsilon_t+T
for (j in 3:(T+1)) {
  arma_resid[j] <- test_3[j-1,2] - arma_coef[3,2] - arma_coef[1,2]*test_3[j-2,2] - arma_coef[2,2]*arma_resid[j-1]
}


# Conditional Mean for SP500
arma_mean <- numeric(T) |> as.numeric()
# t+1 = phi_0 + phi_1 * y_t + theta_1 * epsilon_t
arma_mean[1] <- arma_coef[3,2] + arma_coef[1,2]*train_3[t,2] + arma_coef[2,2]*arma_resid[1]

for (j in 2:T) {
  arma_mean[j] <- arma_coef[3,2] + arma_coef[1,2]*test_3[j-1,2] + arma_coef[2,2]*arma_resid[j]
}



# (Conditional) Variance for SP500
arma_eps_var <- glance(fit) |> 
  filter(.model =="arma") |> 
  select(sigma2)

# arma_var <- ((arma_coef[2,2]^2+2*arma_coef[1,2]*arma_coef[2,2]+1)*arma_eps_var)/(1-arma_coef[1,2]^2)
arma_var <- arma_eps_var
arma_sd <- sqrt(arma_var)


arma_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  arma_pd[j] <- dnorm(test_3[[j,2]], arma_mean[[j]], arma_sd[[1]])
}


LS_arma <- sum(log(arma_pd)) / T
LS_arma












# Model: ETS(M,A,N) 
fit |> 
  select(ets) |> 
  report()

# Smoothing parameters:
#   alpha = 0.9719179 
# beta  = 0.0001000134 

# Initial states:
#   l[0]      b[0]
# 2069.986 0.8465183

# sigma^2:  1e-04


ets_coef <- coef(fit) |> 
  filter(.model == "ets") |> 
  select(term, estimate)

# ets_coef[1,2] # alpha

# ets_coef[2,2] # beta

# ets_coef[3,2] # l[0]

# ets_coef[4,2] # b[0]



# Residuals from 1 to t
ets_resid <- residuals(fit) |> 
  filter(.model == "ets") |> 
  as_tibble() |> 
  select(.resid)

level <- numeric(t+T+1) |> as.numeric()
level[1] <- ets_coef[3,2] # l_0
trend <- numeric(t+T+1) |> as.numeric()
trend[1] <- ets_coef[4,2] # b_0

# level[2] <- (level[1] + trend[1])*(1 + ets_coef[1,2]*ets_resid[1,1]) # l_1
# level[3] <- (level[2] + trend[2])*(1 + ets_coef[1,2]*ets_resid[2,1]) # l_2
# 
# trend[2] <- trend[1] + ets_coef[1,2]*ets_coef[2,2]*(level[1] + trend[1])*ets_resid[1,1] # b_1
# trend[3] <- trend[2] + ets_coef[1,2]*ets_coef[2,2]*(level[2] + trend[2])*ets_resid[2,1] # b_2


# Level and Trend from initial value (0) to t=1000 (1001 in total)
for (j in 1:t) {
  level[j+1] <- (level[[j]] + trend[[j]])*(1 + ets_coef[1,2]*ets_resid[j,1])
  trend[j+1] <- trend[[j]] + ets_coef[1,2]*ets_coef[2,2]*(level[[j]] + trend[[j]])*ets_resid[j,1]
}

# Residuals from t+1 to T
ets_resid_new <- numeric(T) |> as.numeric()

for(i in 1:T) {
  ets_resid_new[i] <- (test_3[i,2] - (level[[t+i]] + trend[[t+i]])) / ((level[[t+i]] + trend[[t+i]]))
  level[t+i+1] <- (level[[t+i]] + trend[[t+i]])*(1 + ets_coef[1,2]*ets_resid_new[i])
  trend[t+i+1] <- trend[[t+i]] + ets_coef[1,2]*ets_coef[2,2]*(level[[t+i]] + trend[[t+i]])*ets_resid_new[i]  
}


# Conditional Mean for SP500
ets_mean <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_mean[j] <- level[[1000+j]] + trend[[1000+j]]
}


# (Conditional) Variance for SP500 - assume it to be the variance of the error term
ets_var <- glance(fit) |> 
  filter(.model =="ets") |> 
  select(sigma2)
ets_sd <- sqrt(ets_var) |> as.numeric()


ets_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_pd[j] <- dnorm(test_3[[j,2]], ets_mean[j], ets_sd)
}


LS_ets <- sum(log(ets_pd)) / T
LS_ets



# pred <- predict(fit,
#                 formula = ~ dnorm(sp$SP500, arma_mean, arma_sd),
#                 n.samples = 2000
# )
# log_score <- log(pred$mean)



# Take the variance of residuals 
# 数值太大了，感觉是forecast variance，这和y的variance有区别（参考）
# temp<-generate(ets, times=1000)
# var(temp$.sim)

ets <- train_3 |> 
  model(
    ets = ETS(SP500 ~ error("M") + trend("A") + season("N"))
  )
temp <- generate(ets, times=1000)
ets_sd <- sd(temp$.sim)
ets_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_pd[j] <- dnorm(test_3[[j,2]], ets_mean[j], ets_sd)
}

LS_ets <- sum(log(ets_pd)) / T
LS_ets






ets_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_pd[j] <- dnorm(test_3[[j,2]], ets_mean[j], arma_sd[[1]])
}


LS_ets <- sum(log(ets_pd)) / T
LS_ets








# Find the optimal weight by optimazing the log predictive score
w <- seq(from = 0, to = 1, by = 0.01)
pool <- numeric(length(w)) |> as.numeric()

# weight on ARMA(1,1)
# for (j in 1:length(w)) {
#   pool[j] <-  sum(log(w[j]*arma_pd + (1-w[j])*ets_pd))/T
# }
#  
# comb <- cbind(w,pool) |> as_tibble()
# comb |> filter(pool == max(comb$pool))
# 
# comb |> ggplot(aes(w, pool)) +
#   geom_line()


# weight on ETS(M,A,N)
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*ets_pd + (1-w[j])*arma_pd))/T
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ARMA, ETS",
       x = "Weight on model ETS",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))





-5.64 #

-6.75 # ARIMA(1,0,1)








# R推荐的模型
fit_auto <- train_3 |> 
  model(
    arma = ARIMA(SP500),
    ets = ETS(SP500)
  )



# ARIMA(0,1,0)
fit_auto |> 
  select(arma) |> 
  report()


# Conditional Mean for SP500
arma_mean <- numeric(T) |> as.numeric()
# t+1
arma_mean[1] <- train_3[t,2]

for (j in 2:T) {
  arma_mean[j] <-test_3[j-1,2]
}

# (Conditional) Variance for SP500
arma_var <- glance(fit_auto) |> 
  filter(.model =="arma") |> 
  select(sigma2)
arma_sd <- sqrt(arma_var)

arma_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  arma_pd[j] <- dnorm(test_3[[j,2]], arma_mean[[j]], arma_sd[[1]])
}


LS_arma <- sum(log(arma_pd)) / T
LS_arma



# ETS(M,N,N)
fit_auto |> 
  select(ets) |> 
  report()

# Smoothing parameters:
#   alpha = 0.9771535 

# Initial states:
#   l[0]
# 2099.889

# sigma^2:  1e-04

ets_coef <- coef(fit_auto) |> 
  filter(.model == "ets") |> 
  select(term, estimate)

# ets_coef[1,2] # alpha
# ets_coef[2,2] # l[0]


# Residuals from 1 to t
ets_resid <- residuals(fit_auto) |> 
  filter(.model == "ets") |> 
  as_tibble() |> 
  select(.resid)

level <- numeric(t+T+1) |> as.numeric()
level[1] <- ets_coef[2,2] # l_0

# level[2] <- level[1]*(1 + ets_coef[1,2]*ets_resid[1,1]) # l_1
# level[3] <- level[2]*(1 + ets_coef[1,2]*ets_resid[2,1]) # l_2

# Level and Trend from initial value (0) to t=1000 (1001 in total)
for (j in 1:t) {
  level[j+1] <- level[j]*(1 + ets_coef[1,2]*ets_resid[j,1])
}


# Residuals from t+1 to T
ets_resid_new <- numeric(T) |> as.numeric()


for(i in 1:T) {
  ets_resid_new[i] <- (test_3[i,2] - level[t+i]) / level[t+i]
  level[t+i+1] <- level[t+i]*(1 + ets_coef[1,2]*ets_resid_new[i])
}


# Conditional Mean for SP500
ets_mean <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_mean[j] <- level[[1000+j]]
}


# (Conditional) Variance for SP500 - assume it to be the variance of the error term
ets_var <- glance(fit_auto) |> 
  filter(.model =="ets") |> 
  select(sigma2)
ets_sd <- sqrt(ets_var) |> as.numeric()

# Way too SMALL!!!


ets_auto <- train_3 |> 
  model(
    ets = ETS(SP500)
  )
temp <- generate(ets_auto, times=1000)
ets_sd <- sd(temp$.sim)
ets_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_pd[j] <- dnorm(test_3[[j,2]], ets_mean[j], ets_sd)
}

LS_ets <- sum(log(ets_pd)) / T
LS_ets



# Find the optimal weight by optimazing the log predictive score
w <- seq(from = 0.01, to = 0.99, by = 0.01)
pool <- numeric(length(w)) |> as.numeric()

# weight on ETS
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*ets_pd + (1-w[j])*arma_pd))/T
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ARMA, ETS",
       x = "Weight on model ETS",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))





