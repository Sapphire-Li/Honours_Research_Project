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


# 分数据方法1
# Split the training set for model estimation (in proportion)
# 8 years as the training set
# 2 years as the test set
train <- sp |> filter(DATE <= "2021-02-10")
test <- sp |> filter(DATE > "2021-02-10")

t <- nrow(train)
T <- nrow(test)

fit_auto <- train |> 
  model(
    arma = ARIMA(SP500),
    ets = ETS(SP500)
  )

# Model: ARIMA(0,1,2) w/ drift
fit_auto |> 
  select(arma) |> 
  report()
# Coefficients:
#   ma1     ma2  constant
# -0.1692  0.1711    1.1880
# s.e.   0.0217  0.0232    0.6087
# sigma^2 estimated as 744.6

arma_coef <- coef(fit_auto) |> 
  filter(.model == "arma") |> 
  select(term, estimate)
# arma_coef[1,2] # theta_1
# arma_coef[2,2] # theta_2
# arma_coef[3,2] # phi_0

arma_resid <- numeric(T+2) |> as.numeric()
# epsilon_t-1
arma_resid[1] <- residuals(fit) |> 
  filter(.model == "arma") |> 
  as_tibble() |> 
  select(.resid) |> 
  tail(2)

#####################
# Problems

# epsilon_t
arma_resid[2] <- residuals(fit) |> 
  filter(.model == "arma") |> 
  as_tibble() |> 
  select(.resid) |> 
  tail(1)

# epsilon_t+1
arma_resid[3] <- test[1,2] - (train[t,2] + arma_coef[3,2] + arma_coef[1,2]*arma_resid[2] + arma_coef[2,2]*arma_resid[1])

# epsilon_t+2 - epsilon_T
for (j in 3:(T+2)) {
  arma_resid[j] <- test[j-2,2] - (test[j-1,2] + arma_coef[3,2] + arma_coef[1,2]*arma_resid[j-1] + arma_coef[2,2]*arma_resid[j-2])
}

# Conditional Mean for SP500

arma_mean <- numeric(T) |> as.numeric()
# t+1
arma_mean[1] <- train[t,2] + arma_coef[3,2] + arma_coef[1,2]*arma_resid[2] + arma_coef[2,2]*arma_resid[1]

#### 写到这里了
for (j in 2:T) {
  arma_mean[j] <- test[j-1,2] + arma_coef[3,2] + arma_coef[1,2]*arma_resid[j+1] + arma_coef[2,2]*arma_resid[j]
}

# (Conditional) Variance for SP500
arma_eps_var <- glance(fit) |> 
  filter(.model =="arma") |> 
  select(sigma2)

arma_var <- ((arma_coef[2,2]^2+2*arma_coef[1,2]*arma_coef[2,2]+1)*arma_eps_var)/(1-arma_coef[1,2]^2)
arma_sd <- sqrt(arma_var)


arma_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  arma_pd[j] <- dnorm(test[[j,2]], arma_mean[[j]], arma_sd[[1]])
}


LS_arma <- sum(log(arma_pd)) / T
LS_arma




# Model: ETS(M,A,N) 
fit_auto |> 
  select(ets) |> 
  report()

# Smoothing parameters:
#   alpha = 0.8371315 
# beta  = 0.000100035 

# Initial states:
#   l[0]     b[0]
# 1530.393 1.105824

# sigma^2:  1e-04


ets_coef <- coef(fit_auto) |> 
  filter(.model == "ets") |> 
  select(term, estimate)
# ets_coef[1,2] # alpha
# ets_coef[2,2] # beta
# ets_coef[3,2] # l[0]
# ets_coef[4,2] # b[0]

ets_resid <- residuals(fit_auto) |> 
  filter(.model == "ets") |> 
  as_tibble() |> 
  select(.resid)

level <- numeric(t+T+1) |> as.numeric()
level[1] <- ets_coef[3,2] # l_0
trend <- numeric(t+T+1) |> as.numeric()
trend[1] <- ets_coef[4,2] # b_0


# Level and Trend from initial value (0) to t=1000 (1001 in total)
for (j in 1:t) {
  level[j+1] <- (level[[j]] + trend[[j]])*(1 + ets_coef[1,2]*ets_resid[j,1])
  trend[j+1] <- trend[[j]] + ets_coef[1,2]*ets_coef[2,2]*(level[[j]] + trend[[j]])*ets_resid[j,1]
}

# Residuals from t+1 to T
ets_resid_new <- numeric(T) |> as.numeric()

for(i in 1:T) {
  ets_resid_new[i] <- (test[i,2] - (level[[t+i]] + trend[[t+i]])) / ((level[[t+i]] + trend[[t+i]]))
  level[t+i+1] <- (level[[t+i]] + trend[[t+i]])*(1 + ets_coef[1,2]*ets_resid_new[i])
  trend[t+i+1] <- trend[[t+i]] + ets_coef[1,2]*ets_coef[2,2]*(level[[t+i]] + trend[[t+i]])*ets_resid_new[i]  
}


# Conditional Mean for SP500
ets_mean <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_mean[j] <- level[[1000+j]] + trend[[1000+j]]
}


# (Conditional) Variance for SP500 - assume it to be the variance of the error term
ets_var <- glance(fit_auto) |> 
  filter(.model =="ets") |> 
  select(sigma2)
ets_sd <- sqrt(ets_var) |> as.numeric()

ets_auto <- train |> 
  model(
    ets = ETS(SP500)
  )
temp <- generate(ets_auto, times=1000)
ets_sd <- sd(temp$.sim)

ets_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_pd[j] <- dnorm(test[[j,2]], ets_mean[j], ets_sd)
}

LS_ets <- sum(log(ets_pd)) / T
LS_ets


















