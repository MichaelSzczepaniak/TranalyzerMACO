

## Development convenience method. Just changes working dir to where it needs
## to be for this project.  Add salt to taste...
# setWorking <- function(laptopSys=TRUE) {
#     dirSys <- "C:/data/"  # laptop
#     if(!laptopSys) { dirSys <- "D:/" }  # workstation
#     dirProject <- "dev/TradeAnalyzer"
#     dirWorking <- paste0(dirSys, dirProject)
#     setwd(dirWorking)
# }

## 
addSimColumns <- function(prices, signalGen, sigParms, maType, startBalance) {
    source(signalGen)
    # add MA cols and calc them based on closing price
    # cat('addSimColumns - first row of prices:\n'); print(prices[1,])
    # cat('\naddSimColumns - sigParms:\n'); print(sigParms)
    priceData <- appendMAcolumns(prices, sigParms, maType, calcCol="Close")
    priceData <- appendSignals(priceData) # add col of 1, -1, 0 signals
    priceData <- getActionsBHS(priceData) # add Actions & Open_Position col's
    source("StrategySimulator.R")
    # cat('addSimColumns - startBalance =', startBalance)
    priceData <- allInAllOutOaatOnlyLong(priceData, startBalance)
    
    return(priceData)
}

## Returns a data frame of the results of a simulation for a stock of a given
## ticker, from startDate to endDate, using the strategy specified by the 
## signal generator signalGen.
##
## ticker - Ticker symbol for stock to run signalGen on (e.g. JNJ, AAPL, etc.)
## priceData - Price data to use for simulation.  If it's passed in, use it.
##             If it's not passed in, query for it.
## signalParms - Vector of named values used by signalGen to generate signals.
##               Default is a vector with the fast and slow SMA periods used by
##               SignalGenMacoLongOnlyOpaat.R
## startDate - Character array of the format: yyyy-mm-dd designating the
##             starting date of the simulation. Default is 365 day prior today.
## endDate - Character array of the format: yyyy-mm-dd designating the
##           ending date of the simulation. Default is today
## signalGen - R source file used to implement the signal generator to use for
##             the simulation. Default is SignalGenMacoLongOnlyOpaat.R
## startingBalance - starting balance used in simulating the strategy with the
##                   given set of parameters
doSimulation <- function(ticker,
                         priceData,
                         startDate = as.character(Sys.Date()-365),
                         endDate = as.character(Sys.Date()),
                         signalParms=c(fastDays=9, slowDays=18),
                         maType = 'Simple',
                         signalGen = "SignalGenMacoLongOnlyOpaat.R",
                         startBalance = 10000) {
    
    # cat('doSimulation - priceData[1,]:\n'); print(priceData[1,])
    
    # addSimColumns sources StrategySimulator.R for getNetTable
    priceData <- addSimColumns(priceData, signalGen, signalParms, maType,
                               startBalance)
    netTable <- getNetTable(priceData)
    
    # cat('doSimulation - netTable[1,]:\n'); print(netTable[1,])
    return(netTable)
}

## Create plot that identifies the trades called out by the signal
## shift - fraction amount to shift buy signal point down and sell signal up
## TODO need a solution for configuring legend position
makeTradeSignalsPlot <- function(ticker, startDate ,endDate, signalParms,
                                 signalGen, startBalance, shift) {
    source("DataManager.R")
    # priceData <- getDemoQuotes(ticker, startDate, endDate) # read repo csv
    priceData <- getStockQuotes(ticker, startDate, endDate) # query for data
    priceData <- addSimColumns(priceData, signalGen, signalParms, startBalance)
    x <- as.Date(priceData$Date) # x axis values
    plot(x, y=priceData$Close, type="l", lwd=2,
         col='black', xlab="Date", ylab="Price ($ USD)")
    title(paste0("Trade signals for ", ticker, " using SMA cross-over"))
    lines(x, y=priceData$FastSma, col='red')
    lines(x, y=priceData$SlowSma, col='blue')
    # get the sell points
    sells <- filter(priceData, Actions=="SELL")
    exitDates <- as.Date(sells$Date)
    sellSignals <- pmax(sells$FastSma, sells$SlowSma) * (1 + shift)
    points(exitDates, sellSignals, pch=6, cex=1.75, col='red', lwd=2)
    completeCount <- length(sells$Shares)
    buys <- filter(priceData, Actions=="BUY")
    entryDates <- as.Date(buys$Date)
    buySignals <- pmin(buys$FastSma, buys$SlowSma) * (1 - shift)
    points(entryDates, buySignals, pch=2, cex=1.75, col='green', lwd=2)
    fastSma <- paste0("Fast SMA ", signalParms["fastDays"])
    slowSma <- paste0("Slow SMA ", signalParms["slowDays"])
    legend('bottom',
           c("Close Price", fastSma, slowSma, "Buy Signal", "Sell Signal"),
           lty=c(1,1,1,0,0), pch=c(NA, NA, NA, 2, 6),
           col=c('black', 'red', 'blue', 'green', 'red'))
}

makeTradesResultsHist <- function(ticker, startDate,endDate,
                                  signalParms, signalGen, startBalance) {
    sim <- doSimulation(ticker, startDate, endDate,
                        signalParms, signalGen, startBalance)
    histTitle <- paste0("Results of completed ", ticker,
                        " trades using SMA cross-over")
    hist(sim$ProfitLoss, main=histTitle, xlab="Trade Net ($ USD)",
         ylab="Trade Count", yaxt='n')
    axis(2, at=0:length(sim$ProfitLoss))
}