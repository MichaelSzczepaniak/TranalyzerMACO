source("./tranmaco/TechIndicators.R")
testDataDir <- './tests/data/'  # relative to project

test.calcSma <- function() {
    period <- 10
    prices <- c(22.27, 22.19, 22.08, 22.17, 22.18,
                22.13, 22.23, 22.43, 22.24, 22.29,
                22.15, 22.39)
    targSma10 <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
                   mean(prices[1:10]), mean(prices[2:11]), mean(prices[3:12]))
    currSma10 <- calcSma(prices, period)
    for(i in 1:length(prices)) {
        checkEqualsNumeric(targSma10[i], currSma10[i], tolerance=0.001)
    }
}

test.calcEma <- function() {
    period <- 10
    prices <- c(22.27, 22.19, 22.08, 22.17, 22.18,
                22.13, 22.23, 22.43, 22.24, 22.29,
                22.15, 22.39)
    targEma10 <- c(NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
                   mean(prices[1:10]), 22.2081, 22.2412)
    currEma10 <- calcEma(prices, period)
    for(i in 1:length(prices)) {
        checkEqualsNumeric(targEma10[i], currEma10[i], tolerance=0.001)
    }
}