###############################################################################
## SignalGenMacoLongOnlyOpaat
## Signal Generator for the Moving Average Cross-Over (MACO) strategy
## Details:
## 1) Only long entry postions (LongOnly) are allowed
## 2) Only one position at a time (Opaat) is allowed. In other words, a
##    currently open postion must be closed before another can be opened.
## BUY Signal is generated when fast (S/E)MA rises above slow (S/E)MA
## SELL Signal is generated when fast (S/E)MA drops below slow (S/E)MA
## HOLD signal is generated when under two conditions:
##  i. a position is open and fast (S/E)MA is near or above the slow (S/E)MA
## ii. no position is open and slow (S/E)MA is near or above the fast (S/E)MA
###############################################################################

source("TechIndicators.R")

## Adds 2 columns used as signal parameters to the stockPrices data frame:
## a fast simple moving average (SMA): FastSma and a slow SMA: SlowSma
## 
## stockPrices - a dataframe or object created from a call to getSymbols
##               in the quantmod package (xts object)
## fastDays - integer number of days to create the fast SMA over
## slowDays - integer number of days to create the slow SMA over
## maType - Type of MA to use for signal generation: 'simple' or 'exponential'
##          Default = 'simple', anything else assumed to be 'exponential'.
## calcCol - name of the column that SMA will be calculated on. Possible values:
##           Open, High, Low, Close, Adj.Close, Volume.
## IMPORTANT: slowDays > fastDays
appendMAcolumns <- function(stockPrices, signalParms=c(fastDays=8, slowDays=16),
                            maType='SMA', calcCol='Adj.Close') {
    fastDays <- signalParms["fastDays"]
    slowDays <- signalParms["slowDays"]
    # cat('appendMAcolumns: maType =', maType, '\n')
    # cat('appendMAcolumns: fastDays=', fastDays, "| slowDays=", slowDays, '\n')
    # cat('appendMAcolumns:  nrow(stockPrices)=', nrow(stockPrices), '\n')
    calcPrices <- stockPrices[, as.character(calcCol)]
    fastName <- "FastMa"
    slowName <- "SlowMa"
    # calcSma and calcEma functions reside in TechIndicators.R
    if(maType =='SMA') {
        stockPrices[, fastName] <- calcSma(calcPrices, fastDays)
        stockPrices[, slowName] <- calcSma(calcPrices, slowDays)
    } else if(maType =='EMA') {
        stockPrices[, fastName] <- calcEma(calcPrices, fastDays)
        stockPrices[, slowName] <- calcEma(calcPrices, slowDays)
    } else {
        # TODO: WMA
        cat('appendMAcolumns: WMA currently unsupported.')
        return(NULL)
    }
    # cat('appendMAcolumns: maType =', maType)
    # cat('appendMAcolumns - first few rows of returned stockPrices:\n');
    # print(stockPrices[1:30,])
    return(stockPrices)
}

## Returns an integer vector corresponding to a 2 MA cross-over strategy. For
## each fastMa in fastMas and slowMa in slowMas, values are calc'd as follows:
##    1 if (fastMa - slowMa) >  tol * price, i.e. fast MA is above slow MA
##   -1 if (slowMa - fastMa) >  tol * price, i.e. fast MA is below fast MA
##    0 if |fastMa - slowMa| <= tol * price, fast & slow MA close together OR
##      fastMa in NaN OR slow MA is NaN
## prices - vector of prices used to calc fast & slow (S/E)MA values. These
##          will typically be one of the 4 std prices: High, Low, Open or Close
##          but could be any vector of values
## fastMas - vector of fast (S/E)MA values
## slowMas - vector of slow (S/E)MA values
## tol - tolerance which is used to determine whether fast and slow (S/E)MAs are 
##       different enough to be considered being above or below one another.
##       When |fastMa - slowMma| <= (price * tol), fastMa and slowMa are
##       considered to be the same from a signal perspective.
##
## Precondtion - The 3 vectors: prices, fastMas, slowMas must have same length
getMaSignals <- function(prices, fastMas, slowMas, tol) {
    maSignals <- vector(mode="integer", length=length(prices)) # init 0's
    for(i in 1:length(prices)) {
        if(is.nan(fastMas[i]) || is.nan(slowMas[i])) { next }
        if((fastMas[i] - slowMas[i]) >  tol * prices[i]) {
            maSignals[i] <- 1
        }
        else if((slowMas[i] - fastMas[i]) >  tol * prices[i]) {
            maSignals[i] <- -1
        }
    }
    
    return(maSignals)
}

## Appends a column to the passed in dataframe named Signal.
## Signal - integer vector, 1 if (fastMa - slowMa) >  tol * price
##                          0 if |fastMa - slowMa| <= tol * price
##                         -1 if (slowMa - fastMa) >  tol * price
## 
## stockPrices is assumed to have at least 4 columns:
## "Date", "FastMa", "SlowMa", and one named whatever is passed in for
## for the calcCol parameter which will be'High', 'Low', 'Open' 'Close',
## of 'Adj.Close'.
appendSignals <- function(stockPrices, calcCol='Adj.Close', tol=0.003) {
    prices <- stockPrices[, as.character(calcCol)]
    smaPos <- getMaSignals(prices, stockPrices$FastMa, stockPrices$SlowMa, tol)
    stockPrices[, "Signal"] <- smaPos
    
    # cat('appendSignals - first row of returned stockPrices:\n'); print(stockPrices[1,])
    return(stockPrices)
}

## Appends two columns to pricesWithSignals and returns the updated data frame.
## Two new columns: Action which can have one of 3 values: HOLD, BUY or SELL &
##                  Open_Position which can be either TRUE or FALSE
##
## pricesWithSignal is expected to have the following columns at minimum:
## Date and Signal. Signal will be either -1, 0 or +1. When Signal goes from
## -1 to +1, the recommended Action is BUY on the day that signal goes to +1.
##
## When Signal goes from +1 to -1, the recommended Action is SELL on the
## day that signal goes to -1.
## 
## The Action is always HOLD under the following conditions:
## 1) when it is not BUY or SELL or
## 2) on the first 2 trading days in the priceWithSignal data
## 
getActionsBHS <- function(pricesWithSignal) {
    actions <- rep("HOLD", length(pricesWithSignal$Signal))
    openPosition <- rep(FALSE, length(pricesWithSignal$Signal))
    sampleCount <- length(pricesWithSignal$Signal)
    for(i in 3:sampleCount) {
        signalDayMinus0 <- pricesWithSignal$Signal[i]
        signalDayMinus1 <- pricesWithSignal$Signal[i-1]
        if(openPosition[i-1]) {  # only HOLD or SELL allowable
            if(signalDayMinus1 < 0) { actions[i] <- "UNREACHABLE_STATE1" }
            if(signalDayMinus1 == 0) {
                if(signalDayMinus0 < 0) {
                    actions[i] <- "SELL"
                    # reset downstream states, will be reset after next BUY
                    openPosition[i:sampleCount] <- FALSE
                }
                else {
                    # signalDayMinus0 = 0 or 1, leave action as HOLD, but
                    # reset openPositions downstream to the previous value
                    openPosition[i:sampleCount] <- openPosition[i-1]
                }
                
            }
            else {  # signalDayMinus1 > 0
                if(signalDayMinus0 < 0) {
                    actions[i] <- "SELL"
                    # reset downstream states, will be reset after next BUY
                    openPosition[i:sampleCount] <- FALSE
                }
                else {
                    # signalDayMinus0 = 0 or 1, leave action as HOLD, but
                    # reset openPositions downstream to the previous value
                    openPosition[i:sampleCount] <- openPosition[i-1]
                }
            }
        }
        else {  # openPosition[i-1] == FALSE, only HOLD or BUY allowable
            if(signalDayMinus1 > 0) { actions[i] <- "UNREACHABLE_STATE2" }
            if(signalDayMinus1 == 0) {
                if(signalDayMinus0 > 0) {
                    actions[i] <- "BUY"
                    # reset downstream states, will be reset after sell
                    openPosition[i:sampleCount] <- TRUE
                }
                else {
                    openPosition[i:sampleCount] <- openPosition[i-1]
                }
            }
            else {  # signalDayMinus1 < 0
                if(signalDayMinus0 > 0) {
                    actions[i] <- "BUY"
                    # reset downstream states, will be reset after sell
                    openPosition[i:sampleCount] <- TRUE
                }
                else {
                    openPosition[i:sampleCount] <- openPosition[i-1]
                }
            }
        }
    }
    pricesWithSignal[, "Actions"] <- as.factor(actions)
    pricesWithSignal[, "Open_Position"] <- openPosition
    
    # cat('getActionsBHS - first row of returned stockPrices:\n'); print(pricesWithSignal[1,])
    return(pricesWithSignal)
}

