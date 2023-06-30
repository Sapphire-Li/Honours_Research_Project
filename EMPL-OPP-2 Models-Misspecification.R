library(readxl)
library(fpp3)
library(tidyverse)
library(dplyr)

# https://www.abs.gov.au/statistics/labour/EMPL-and-unEMPL/labour-force-australia-detailed/latest-release
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
m <- 4

# Preliminary Analysis
# train |> autoplot(Total)
# train |> autoplot(log(Total))
# train |> features(Total, features = guerrero)
# train |> gg_season(Total)
# train |> gg_subseries(Total)
# train |> autoplot(difference(log(Total),m))
# train |> gg_tsdisplay(difference(log(Total),m), "partial")
# train |> gg_tsdisplay(difference(difference(log(Total),m),1), "partial")




# Model: ARIMA(2,1,0) w/ drift
fit_3 <- train |> model(ARIMA(log(Total) ~ 1 + PDQ(0,0,0)))
report(fit_3)
# Coefficients:
#    ar1      ar2  constant
# -0.1665  0.5790     3e-03
# sigma^2 estimated as 4.694e-05

fit_3_coef <- coef(fit_3) |> select(term, estimate)







# Model: ETS(A,A,N)
fit_4 <- train |> 
  model(ETS(log(Total) ~ trend("A") + season("N")))
report(fit_4)
gg_tsresiduals(fit_4, lag = 16)

# Smoothing parameters:
#   alpha = 0.513919 
#   beta  = 0.3965017 

# Initial states:
#      l[0]        b[0]
#  8.777956 0.007725023

# sigma^2:  1e-04













