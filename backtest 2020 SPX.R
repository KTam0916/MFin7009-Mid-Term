library(Quandl)
library(quantmod)
library(PerformanceAnalytics)

# set parameters for out-of-the-money options
X <- 0.05 # put X% OOM
Y <- 0.1 # call Y% OOM
underlying <- "^GSPC"
fr.date <- "2020-01-01"
to.date <- "2021-01-31"
# calculate S_tp0 (S&P 100 beginning of the month) and S_tp1 (S&P 100 end of the month)
S.orig <- getSymbols(underlying, from = fr.date, to = to.date, periodicity = "monthly", auto.assign = FALSE) # values for S_tp0 and S_tp1
S <- coredata(S.orig[ , 4])
S_tp0 <- S[-length(S)] # remove last value -> November is only necessary as S_tp1 for October
S_tp1 <- S[-1]         # remove first value -> starting value of S_tp1 is equal to the end value of S_tp0
VOL <- getSymbols("^VIX", from = fr.date, to = as.Date(to.date)-30, periodicity = "monthly", auto.assign = FALSE) # Values for implied volatility
sigma_t <- coredata(VOL[ , 4]) / 100
yield_t <- read.csv("TB3MS.csv", header = TRUE, stringsAsFactors=FALSE)
yield_t <- yield_t[as.Date(yield_t$DATE, "%m/%d/%Y") > as.Date(fr.date) - 10 & as.Date(yield_t$DATE, "%m/%d/%Y") < as.Date(to.date) - 31, ]
yield_t <- yield_t[,2] / 100

P_t <- 100 * (1 - 91/360 * yield_t)
r_t <- 4 * log(100/P_t)

Put_t <- function(S_tp0, X, r_t, sigma_t) {
  d1 <- (log(1/(1-X)) + (r_t + sigma_t^2/2) * 1/12) / (sigma_t*sqrt(1/12))
  d2 <- d1 - sigma_t * sqrt(1/12)
  (1-X) * S_tp0 * exp(-r_t/12) * pnorm(-d2) - S_tp0 * pnorm(-d1) # pnorm is cumulative density function of normal distribution
}              

PutPayoff_tp1 <- function(S_tp0, S_tp1, X) {
  pmax(0, (1-X) * S_tp0 - S_tp1)
}

Call_t <- function(S_tp0, Y, r_t, sigma_t) {
  d1 <- (log(1/(1+Y)) + (r_t + sigma_t^2/2) * 1/12) / (sigma_t * sqrt(1/12))
  d2 <- d1 - sigma_t * sqrt(1/12)
  S_tp0 * pnorm(d1) - (1+Y) * S_tp0*exp(-r_t/12) * pnorm(d2)
}                                                                               
CallPayout_tp1 <- function(S_tp0, S_tp1, Y) {
  pmax(0, S_tp1 - (1+Y) * S_tp0)
}


ROR_t <- function(S_tp0, S_tp1, X, Y, r_t, sigma_t, P, C) {
  (S_tp1 + PutPayoff_tp1(S_tp0, S_tp1, X) * P - CallPayout_tp1(S_tp0, S_tp1, Y) * C) / (S_tp0 + Put_t(S_tp0, X , r_t , sigma_t) * P - Call_t(S_tp0, Y, r_t, sigma_t) * C)
} # adapted for different option strategies and underlyings

collar_strategy_return <- ROR_t(S_tp0, S_tp1, X, Y, r_t,sigma_t, TRUE, TRUE) - 1
# cc_strategy_return <- ROR_t(S_tp0, S_tp1, X, Y, r_t,sigma_t, FALSE, TRUE) - 1
benchmark_return <- (S_tp1 / S_tp0) - 1
# create a data.frame with both data and with the dates (monthly basis)
months <- index(S.orig[-1])
perf <- data.frame(months, collar_strategy_return, benchmark_return) # as these are month-end returns, we use the first day of the next month as index. 
colnames(perf) <- c("months", 
                    "Collar", 
                    # "Covered-Call", 
                    "Underlying")
write.zoo(perf, "SPX comparison.csv", sep=",")

# for evaluation within R 
# create an xts out of the data frame
perfxts <- xts(perf[ , -1], order.by = perf[ , 1])  
charts.PerformanceSummary(perfxts)
table.AnnualizedReturns(perfxts)
perfxts

