library(readxl)
library(fpp3)
library(tidyverse)
library(dplyr)

# https://www.abs.gov.au/statistics/industry/retail-and-wholesale-trade/retail-trade-australia/latest-release#data-downloads
# food <- read.csv("Food retailing.csv")
food <- read_xlsx("850101.xlsx", range = "Data1!A11:B500", col_names = c("Month", "Turnover"))
food <- food |> 
  mutate(Month = yearmonth(Month)) |> 
  as_tsibble(index = "Month")
food <- food |> mutate(Log = log(Turnover))

T <- nrow(food)
# In-sample - Training Set
train <- food[1:floor(nrow(food)*.75),] |> as_tsibble()
R <- nrow(train)
# Out-of-sample - Test set
test <- food[(R+1):nrow(food),] |> as_tsibble()
P <- nrow(test)
m <- 12


# Preliminary Analysis
# train |> autoplot(Turnover)
# train |> autoplot(log(Turnover))
# train |> features(Turnover, features = guerrero)
# train |> gg_season(Turnover)
# train |> gg_subseries(Turnover)
# train |> autoplot(difference(log(Turnover),12))
# train |> gg_tsdisplay(difference(log(Turnover),12), "partial")
# train |> gg_tsdisplay(difference(difference(log(Turnover),12),1), "partial")



# Model: ETS(M,A,M) 
fit_1 <- train |> 
  model(ETS(Turnover))
report(fit_1)
# Smoothing parameters:
#   alpha = 0.2379844 
#   beta  = 0.01125871 
#   gamma = 0.0001107135 
# Initial states:
#     l[0]    b[0]     s[0]     s[-1]    s[-2]    s[-3]    s[-4]    s[-5]
# 1169.327 9.67961 1.012064 0.9370364 1.003077 1.161438 1.011628 1.015776
#     s[-6]     s[-7]     s[-8]     s[-9]    s[-10]    s[-11]
# 0.9706842 0.9924231 0.9833296 0.9509841 0.9853554 0.9762041
# sigma^2:  4e-04



# train |>
#   model(STL(log(Turnover) ~ trend(window = 7) + season(window = "periodic"),
#             robust = TRUE)) |>
#   components() |> autoplot()

# Model: ETS(M,Ad,M) 
# fit_4 <- train |> 
#   model(ETS(Turnover ~ error("M") + trend("Ad") + season("M")))
# report(fit_4)

# Smoothing parameters:
#   alpha = 0.1501255 
#   beta  = 0.08061994 
#   gamma = 0.0001026361 
#   phi   = 0.9636318 
# Initial states:
#    l[0]     b[0]     s[0]     s[-1]    s[-2]    s[-3]    s[-4]    s[-5]
# 1168.37 14.13597 1.012941 0.9373244 1.003313 1.162376 1.010097 1.015824
#    s[-6]     s[-7]     s[-8]    s[-9]    s[-10]    s[-11]
# 0.969727 0.9924589 0.9821148 0.950848 0.9865899 0.9763853
# sigma^2:  4e-04


fit_1_coef <- coef(fit_1) |> 
  select(term, estimate)
# fit_1_coef[1,2] # alpha
# fit_1_coef[2,2] # beta
# fit_1_coef[3,2] # gamma
# fit_1_coef[4,2] # l[0]
# fit_1_coef[5,2] # b[0]
# fit_1_coef[6,2] # s[0]
# fit_1_coef[7,2] # s[-1]
# ...
# fit_1_coef[17,2] # s[-11]

# Residuals from 1 to R
resid_1 <- residuals(fit_1) |>
  as_tibble() |>
  select(.resid)


level <- numeric(T+1) |> as.numeric()
level[1] <- fit_1_coef[4,2] # l_0
trend <- numeric(T+1) |> as.numeric()
trend[1] <- fit_1_coef[5,2] # b_0
season <- numeric(T+12) |> as.numeric()
for (j in 1:12) {
  season[j] <- fit_1_coef[18-j,2]
}


# Level and Trend from initial value (0) to R (R+1 in total)
for (j in 1:R) {
  level[j+1] <- (level[[j]] + trend[[j]]) * (1 + fit_1_coef[1,2]*resid_1[j,1])
  trend[j+1] <- trend[[j]] + (fit_1_coef[1,2]*fit_1_coef[2,2])*(level[[j]] + trend[[j]])*resid_1[j,1]
}

# Season from t=1 to R
for (j in 1:R) {
  season[j+m] <- season[[j]]*(1 + fit_1_coef[3,2]*resid_1[j,1])
}


# Residuals from t+1 to P
resid_1_new <- numeric(P) |> as.numeric()

# resid_1_new[1] <- test[1,2] - (level[[R+1]] + trend[[R+1]])*season[[R+1]]
# (level[[R+1]] + trend[[R+1]])*(1 + fit_1_coef[1,2]*resid_1_new[[1]])
# trend[[R+1]] + fit_1_coef[1,2]*fit_1_coef[2,2]*(level[[R+1]] + trend[[R+1]])*resid_1_new[[1]]
# season[[R+1]]*(1 + fit_1_coef[3,2]*resid_1_new[[1]])

# resid[R+1] = y_R+1 - (l_R + b_R)*s_R+1-m  season[R+1]-S(R+1-m)
# resid[R+2] = y_R+2 - (l_R+1 + b_R+1)*s_R+2-m  season[R+2]-S(R+2-m)
for(i in 1:P) {
  resid_1_new[i] <- (test[i,2] - (level[[R+i]] + trend[[R+i]])*season[[R+i]]) / ((level[[R+i]] + trend[[R+i]])*season[[R+i]])
  level[R+i+1] <- (level[[R+i]] + trend[[R+i]])*(1 + fit_1_coef[1,2]*resid_1_new[[i]])
  trend[R+i+1] <- trend[[R+i]] + fit_1_coef[1,2]*fit_1_coef[2,2]*(level[[R+i]] + trend[[R+i]])*resid_1_new[[i]]
  season[R+m+i] <- season[[R+i]]*(1 + fit_1_coef[3,2]*resid_1_new[[i]])
}
# season[13]-S1  season[14]-S2   season[R+1+m]-S(R+1)


# Conditional Mean for SP500
mean_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  mean_1[j] <- (level[[R+j-1]] + trend[[R+j-1]])*season[[R+j]]
}

# Conditional Variance for SP500
var <- glance(fit_1) |> select(sigma2) |> as.numeric()
var_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  var_1[j] <- mean_1[j]^2*var
}
sd_1 <- as.numeric(var_1) |> sqrt()


pd_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_1[j] <- dnorm(test[[j,2]], mean_1[j], sd_1[j])
}


LS_1 <- sum(log(pd_1)) / P
LS_1

#-14.27422






# Model: ARIMA(0,1,2)(0,1,0)[12] 
fit_2 <- train |> 
  model(ARIMA(log(Turnover) ~ pdq(0,1,2) + PDQ(0,1,0)))
report(fit_2)
# gg_tsresiduals(fit_2)
# Coefficients:
#           ma1     ma2
#       -0.9942  0.3695
# s.e.   0.0489  0.0436
# sigma^2 estimated as 0.0005144

fit_2_coef <- coef(fit_2) |> 
  select(term, estimate)
# fit_2_coef[1,2] ma1
# fit_2_coef[2,2] ma2


resid_2 <- numeric(P+2) |> as.numeric()

resid_2[1] <- residuals(fit_2) |> 
  as_tibble() |> 
  select(.resid) |> 
  slice(R-1) # epsilon_t-1

resid_2[2] <- residuals(fit_2) |> 
  as_tibble() |> 
  select(.resid) |> 
  slice(R) # epsilon_t


(train[,3]-log(fitted(fit_2)$.fitted)) == residuals(fit_2)$.resid


# Residual vector for time from t+1 to T (P)
# Observed value - Fitted value
# yhat_t = y_t-1 + y_t-12 - y_t-13 + ma1*resid_t-1 + ma2*resid_t-2

# e_t+1
resid_2[3] <- test[1,3] - (train[R,3] + train[R-11,3] - train[R-12,3] + fit_2_coef[1,2]*resid_2[2] + fit_2_coef[2,2]*resid_2[1])
# e_t+2 to e_t+12
for(j in 1:11){
  resid_2[j+3] <- test[j+1,3] - (test[j,3] + train[R-11+j,3] - train[R-12+j,3] + fit_2_coef[1,2]*resid_2[j+2] + fit_2_coef[2,2]*resid_2[j+1])
}

# e_t+13
resid_2[15] <- test[13,3] - (test[12,3] + test[1,3] - train[R,3] + fit_2_coef[1,2]*resid_2[14] + fit_2_coef[2,2]*resid_2[13])
# e_t+14
resid_2[16] <- test[14,3] - (test[13,3] + test[2,3] - test[1,3] + fit_2_coef[1,2]*resid_2[15] + fit_2_coef[2,2]*resid_2[14])

# e_t+15 to e_t+P
for(j in 17:(P+2)){
  resid_2[j] <- test[j-2,3] - (test[j-3,3] + test[j-14,3] - test[j-15,3] + fit_2_coef[1,2]*resid_2[j-1] + fit_2_coef[2,2]*resid_2[j-2])
}


# Conditional Mean for turnover
mean_2 <- numeric(P) |> as.numeric()
# E(y_t+1) = y_t + y_t-11 + y_t-12 + ma1*resid_t + ma2*resid_t-1
mean_2[1] <- train[R,3] + train[R-11,3] - train[R-12,3] + fit_2_coef[1,2]*resid_2[2] + fit_2_coef[2,2]*resid_2[1]
# E(y_t+2) = y_t+1 + y_t-10 + y_t-11 + ma1*resid_t+1 + ma2*resid_t
for (j in 2:12) {
  mean_2[j] <- test[j-1,3] + train[R-12+j,3] - train[R-13+j,3] + fit_2_coef[1,2]*resid_2[j+1] + fit_2_coef[2,2]*resid_2[j]
}
# E(y_t+13) = y_t+12 + y_t+1 + y_t + ma1*resid_t+12 + ma2*resid_t+11
mean_2[13] <- test[12,3] + test[1,3] - train[R,3] + fit_2_coef[1,2]*resid_2[14] + fit_2_coef[2,2]*resid_2[13]
# E(y_t+14) = y_t+13 + y_t+2 + y_t+1 + ma1*resid_t+13 + ma2*resid_t+12
for (j in 14:P) {
  mean_2[j] <- test[j-1,3] + test[j-12,3] - test[j-13,3] + fit_2_coef[1,2]*resid_2[j+1] + fit_2_coef[2,2]*resid_2[j]
}


# Conditional Variance for SP500
var_2 <- glance(fit_2) |> select(sigma2) |> as.numeric()
sd_2 <- sqrt(var_2)


pd_2 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_2[j] <- dnorm(test[[j,3]], mean_2[[j]], sd_2[[1]])
}


LS_2 <- sum(log(pd_2)) / P
LS_2

#1.775308


# If the log of a variable follows a normal distribution, then the variable itself follows a log-normal distribution.
# A log-normal distribution is a probability distribution of a random variable whose logarithm follows a normal distribution. 
# Mathematically, the log-normal probability density function (pdf) can be expressed as:
#   f(x) = 1 / (x * σ * sqrt(2π)) * exp(-((ln(x) - μ)^2) / (2 * σ^2))
# where μ and σ are the mean and standard deviation of the logarithm of X, respectively.

pd_2b <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_2b[j] <- dlnorm(test[[j,2]], mean_2[[j]], sd_2[[1]])
}


LS_2b <- sum(log(pd_2b)) / P
LS_2b
# -7.52116








# Model: ARIMA(2,1,0)(2,1,2)[12] 
fit_3 <- train |> 
  model(ARIMA(log(Turnover)))
report(fit_3)
# Coefficients:
#           ar1      ar2    sar1     sar2     sma1    sma2
#       -0.8446  -0.5058  0.7321  -0.5378  -1.4076  0.5836
# s.e.   0.0514   0.0536  0.0714   0.0650   0.0722  0.0630
# sigma^2 estimated as 0.0002777








# Model: LM w/ ARIMA(2,0,2)(0,0,2)[12] errors 
fit_4 <- train |> 
  model(ARIMA(log(Turnover) ~ trend()))
report(fit_4)
gg_tsresiduals(fit_4, lag = 60)
# Coefficients:
#          ar1     ar2     ma1      ma2    sma1    sma2  trend()  intercept
#       0.0850  0.7877  0.2339  -0.6452  1.0433  0.5120   0.0055     7.1638
# s.e.  0.0633  0.0511  0.0720   0.0581  0.0531  0.0367   0.0002     0.0342
# sigma^2 estimated as 0.0009931








# Model: LM w/ ARIMA(0,0,2)(2,0,0)[12] errors
fit_5 <- train |> 
  model(ARIMA(log(Turnover) ~ trend() + season()))
report(fit_5)
gg_tsresiduals(fit_5, lag = 60)

# Coefficients:
#   ma1     ma2    sar1     sar2  trend()  season()year2  season()year3
# 0.0911  0.5077  0.8750  -0.1972   0.0055        -0.0647         0.0132
# season()year4  season()year5  season()year6  season()year7  season()year8
# -0.0283        -0.0189        -0.0528        -0.0193        -0.0146
# season()year9  season()year10  season()year11  season()year12  intercept
# -0.0332          0.0115          0.0114          0.1505     7.1751
# sigma^2 estimated as 0.0006332





# Find the optimal weight by optimazing the log predictive score
w <- seq(from = 0, to = 1, by = 0.01)
pool <- numeric(length(w)) |> as.numeric()

# ETS(M,A,M) and ARIMA(0,1,2)(0,1,0)[12] 
# weight on the first model
for (j in 1:length(w)) {
  pool[j] <-  sum(log(w[j]*pd_1 + (1-w[j])*pd_2b))/P
}

comb <- cbind(w,pool) |> as_tibble()
comb |> filter(pool == max(comb$pool))
# w = 0.35
# pool = -7.10

comb |> ggplot(aes(w, pool)) +
  geom_line(color = "red") +
  labs(title = "ETS(M,A,M) , ARIMA(0,1,2)(0,1,0)[12] ",
       x = "Weight on model ETS",
       y = "Log predictive socre") +
  theme(plot.title = element_text(hjust = 0.5))



