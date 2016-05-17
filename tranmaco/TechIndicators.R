

## Returns a vector of simple moving averages of prices.
## prices - numeric vector of price data
## smaInterval - period of the simple moving average.
## NOTE: SMA values for first (smaInterval - 1) days are NaN
calcSma <- function(prices, smaInterval) {
    priceCount <- length(prices)
    smavgs <- vector(mode = "numeric", length = priceCount)
    for(i in 1:(smaInterval - 1)) { smavgs[i] <- NaN }
    if(priceCount >= smaInterval) {
        for(j in smaInterval:length(prices)) {
            smavgs[j] <- mean(prices[(1 + j - smaInterval):j])
        }
    }
    
    return(smavgs)
}

## Returns a vector of exponential moving averages of prices
## prices - numeric vector of price data
## emaInterval - period of the exponential moving average.
## NOTE: EMA values for first (emaInterval - 1) days are NaN
## Ref: The New Trading For a Living - Elder, page 76
calcEma <- function(prices, emaInterval) {
    priceCount <- length(prices)
    emavgs <- vector(mode = "numeric", length = priceCount)
    for(i in 1:(emaInterval - 1)) { emavgs[i] <- NaN }
    k <- 2 / (emaInterval + 1)
    oneMinusK <- 1 - k
    if(priceCount >= emaInterval) {
        # set first EMA value to SMA
        emavgs[emaInterval] <- mean(prices[1:emaInterval])
        if(priceCount > emaInterval) {
            for(j in (emaInterval+1):length(prices)) {
                emavgs[j] <- (prices[j] * k) + (emavgs[j-1] * oneMinusK)
            }
        }
    }
    
    return(emavgs)
}