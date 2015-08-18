library(mosaic)
library(fImport)
library(foreach)
library('quantmod')

# displaying risk/return
getSymbols("SPY")
chartSeries(SPY, subset='last 5 years')
getSymbols("TLT")
chartSeries(TLT, subset='last 5 years')
getSymbols("LQD")
chartSeries(LQD, subset='last 5 years')
getSymbols("EEM")
chartSeries(EEM, subset='last 5 years')
getSymbols("VNQ")
chartSeries(VNQ, subset='last 5 years')

# creating portfolio
mystocks = c("SPY", "TLT", "LQD", "EEM", "VNQ")
myprices = yahooSeries(mystocks, from='2010-08-01', to='2015-08-01')
summary(myprices)

PercentReturn = function(series) {
    mycols = grep('Adj.Close', colnames(series))
    closingprice = series[,mycols]
    N = nrow(closingprice)
    percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
    mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
    mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
    colnames(percentreturn) = mynames
    as.matrix(na.omit(percentreturn))
}

myreturns = PercentReturn(myprices)
n_days = 20
## Even Split Portfolio:
set.seed(100)
EvenSplit = foreach(i=1:20, .combine='rbind') %do% {
    totalwealth = 100000
    weights = c(0.2, 0.2, 0.2, 0.2, 0.2)
    holdings = weights * totalwealth
    wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
    for(today in 1:n_days) {
        return.today = resample(myreturns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        totalwealth = sum(holdings)
        wealthtracker[today] = totalwealth
        holdings = weights * totalwealth
    }
    wealthtracker
}
hist(EvenSplit[,n_days], 25)
hist(EvenSplit[,n_days]- 100000) # Profit/loss
quantile(EvenSplit[,n_days], 0.05) - 100000 # Calculate 5% value at risk


## Safe Portfolio:
set.seed(100)
Safe = foreach(i=1:20, .combine='rbind') %do% {
    totalwealth = 100000
    weights = c(0.3, 0.35, 0.35, 0.0, 0.0)
    holdings = weights * totalwealth
    wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
    for(today in 1:n_days) {
        return.today = resample(myreturns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        totalwealth = sum(holdings)
        wealthtracker[today] = totalwealth
        holdings = weights * totalwealth
    }
    wealthtracker
}
hist(Safe[,n_days], 25)
hist(Safe[,n_days]- 100000) # Profit/loss
quantile(Safe[,n_days], 0.05) - 100000 # Calculate 5% value at risk

## Aggressive Portfolio:
set.seed(100)
Aggressive = foreach(i=1:20, .combine='rbind') %do% {
    totalwealth = 100000
    weights = c(0.40, 0.0, 0.0, 0.25, 0.35)
    holdings = weights * totalwealth
    wealthtracker = rep(0, n_days) # Set up a placeholder to track total wealth
    for(today in 1:n_days) {
        return.today = resample(myreturns, 1, orig.ids=FALSE)
        holdings = holdings + holdings*return.today
        totalwealth = sum(holdings)
        wealthtracker[today] = totalwealth
        holdings = weights * totalwealth
    }
    wealthtracker
}
hist(Aggressive[,n_days], 25)
hist(Aggressive[,n_days]- 100000) # Profit/loss
quantile(Aggressive[,n_days], 0.05) - 100000 # Calculate 5% value at risk

## 5% value at risk
## Even Split Portfolio
quantile(EvenSplit[,n_days], 0.05) - 100000 # Calculate 5% value at risk

## Safe Portfolio
quantile(Safe[,n_days], 0.05) - 100000 # Calculate 5% value at risk

## Aggressive Portfolio
quantile(Aggressive[,n_days], 0.05) - 100000 # Calculate 5% value at risk
