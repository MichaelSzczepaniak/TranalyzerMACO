source("./tranmaco/DataManager.R")
testDataDir <- './tests/data/'  # relative to project
#testDataDir <- './data/'  # relative to current dir

test.getSinglePeriodYqlQuotes <- function() {
    expHLOCV2016.01.04 <- c(105.370003, 102, 102.610001, 105.349998, 67649400)
    expHLOCV2016.01.05 <- c(105.849998, 102.410004, 105.75, 102.709999, 55791000)
    quotes <- getSinglePeriodYqlQuotes(ticker='AAPL',
                                       startYYYY_MM_DD='2016-01-04',
                                       endYYYY_MM_DD='2016-01-05',
                                       dataFrame=NULL)
    ## test Apple quote for 2016-01-04
    checkEqualsNumeric(expHLOCV2016.01.04[1], quotes$High[1])
    checkEqualsNumeric(expHLOCV2016.01.04[2], quotes$Low[1])
    checkEqualsNumeric(expHLOCV2016.01.04[3], quotes$Open[1])
    checkEqualsNumeric(expHLOCV2016.01.04[4], quotes$Close[1])
    checkEqualsNumeric(expHLOCV2016.01.04[5], quotes$Volume[1])
    ## test Apple quote for 2016-01-05
    checkEqualsNumeric(expHLOCV2016.01.05[1], quotes$High[2])
    checkEqualsNumeric(expHLOCV2016.01.05[2], quotes$Low[2])
    checkEqualsNumeric(expHLOCV2016.01.05[3], quotes$Open[2])
    checkEqualsNumeric(expHLOCV2016.01.05[4], quotes$Close[2])
    checkEqualsNumeric(expHLOCV2016.01.05[5], quotes$Volume[2])
}

## As of early 2016, YQL queries for quotes broke when trying to go back much
## futher than about 1 year. This motivated the development of functions that
## break a large date range into a set of smaller ranges. This test tests a
## call to the finance.yahoo REST service that spans ove 5 years which should
## result in 5 separate calls to the service.
test.getQuotesFromService <- function() {
    jnjHLOCV2005.12.19 <- c(62, 61.10, 61.55, 61.19, 10639100)
    jnjHLOCV2010.12.20 <- c(62.65, 62.23, 62.54, 62.49, 8907000)
    bigQuotes <- getQuotesFromService(ticker='JNJ',
                                      startDate='2005-12-15',
                                      endDate='2010-12-20',
                                      service='finance.yahoo')
    jnjStart <- as.numeric(bigQuotes[3,3:7])
    jnjEnd <- as.numeric(bigQuotes[nrow(bigQuotes),3:7])
    checkEquals(jnjHLOCV2005.12.19, jnjStart, tolerance = 0.001)
    checkEquals(jnjHLOCV2010.12.20, jnjEnd, tolerance = 0.001)
}

## Tests the initial creation of the quotes data file and that the first and 
## last three records in the quote file are as expected.
test.initCreateStockQuoteFile <- function() {
    # create quote data file and check that it exists
    tickSym <- "DIS"
    outFile <- sprintf('%s%s%s', testDataDir, tickSym, '.csv')
    disQuotes <- getStockQuotes(tickSym, '2005-12-15', '2010-12-20',
                                dataDir=testDataDir)
    checkTrue(file.exists(outFile))
    # check that written file has first 3 and last 3 quotes
    dis1stThree <- data.frame(Date=c('2005-12-15', '2005-12-16', '2005-12-19'),
                              High=c(25.09, 24.91, 24.85),
                              Low=c(24.71, 24.54, 24.32),
                              Open=c(25.00, 24.90, 24.32),
                              Close=c(24.74, 24.70, 24.54),
                              Volume=c(8150900, 16282000, 6918800))
    disLastThree <- data.frame(Date=c('2010-12-16', '2010-12-17', '2010-12-20'),
                               High=c(37.18,37.17, 37.34),
                               Low=c(36.90, 36.77, 36.84),
                               Open=c(37.02, 36.97, 37.03),
                               Close=c(37.01, 37.05, 37.06),
                               Volume=c(7649800, 11285400, 5114100))
    ## check first 3 quotes
    checkEquals(dis1stThree$High, disQuotes[1:3,]$High, tolerance = 0.001)
    checkEquals(dis1stThree$Low, disQuotes[1:3,]$Low, tolerance = 0.001)
    checkEquals(dis1stThree$Open, disQuotes[1:3,]$Open, tolerance = 0.001)
    checkEquals(dis1stThree$Close, disQuotes[1:3,]$Close, tolerance = 0.001)
    checkEquals(dis1stThree$Volume, disQuotes[1:3,]$Volume, tolerance = 0.001)
    ## check last 3 quotes
    lastIndex <- nrow(disQuotes)
    l2i <- seq(lastIndex-2, lastIndex)
    checkEquals(disLastThree$High, disQuotes[l2i,]$High, tolerance = 0.001)
    checkEquals(disLastThree$Low, disQuotes[l2i,]$Low, tolerance = 0.001)
    checkEquals(disLastThree$Open, disQuotes[l2i,]$Open, tolerance = 0.001)
    checkEquals(disLastThree$Close, disQuotes[l2i,]$Close, tolerance = 0.001)
    checkEquals(disLastThree$Volume, disQuotes[l2i,]$Volume)
    ## clean up
    if(file.exists(outFile)) file.remove(outFile)
}

## Tests the case when some of the quote data requested exists in the local
## data file (<ticker symbol>.csv), but more recent data needs to be added to
## to this file needs to bring it up to date
test.appendStockQuoteFile <- function() {
    # create a file with quotes for DIS from 2005-12-15 through 2010-12-20
    tickSym <- "DIS"
    outFile <- sprintf('%s%s%s', testDataDir, tickSym, '.csv')
    disQuotes <- getStockQuotes(tickSym, '2005-12-15', '2010-12-20',
                                dataDir=testDataDir)
    # request DIS data from 2008-01-07 through 2012-05-10 and append DIS.csv
    disQuotes <- getStockQuotes(tickSym, '2008-01-07', '2012-05-10',
                                dataDir=testDataDir)
    # test that appended data contains proper 1st and last 3 records of
    # new section added - top of new added section:
    addTopDates <- c('2010-12-21', '2010-12-22', '2010-12-23')
    disTop3Secn <- data.frame(Date=addTopDates,
                              High=c(37.42, 37.99, 37.94),
                              Low=c(37.10, 37.34, 37.52),
                              Open=c(37.17, 37.43, 37.85),
                              Close=c(37.33, 37.95, 37.70),
                              Volume=c(4783900, 7229000, 4506000))
    disNewQuotesTop3 <- disQuotes[disQuotes$Date %in% addTopDates,]
    checkEquals(disTop3Secn$High, disNewQuotesTop3$High, tolerance = 0.001)
    checkEquals(disTop3Secn$Low, disNewQuotesTop3$Low, tolerance = 0.001)
    checkEquals(disTop3Secn$Open, disNewQuotesTop3$Open, tolerance = 0.001)
    checkEquals(disTop3Secn$Close, disNewQuotesTop3$Close, tolerance = 0.001)
    checkEquals(disTop3Secn$Volume, disNewQuotesTop3$Volume)
    # bottom of new added section
    addBotDates <- c('2012-05-08', '2012-05-09', '2012-05-10')
    disBot3Secn <- data.frame(Date=c('2012-05-08', '2012-05-09', '2012-05-10'),
                              High=c(44.49, 45.80, 45.59),
                              Low=c(43.09, 44.41, 45.05),
                              Open=c(43.78, 44.46, 45.14),
                              Close=c(44.30, 45.02, 45.28),
                              Volume=c(19680900, 21348300, 14082900))
    disNewQuotesBot3 <- disQuotes[disQuotes$Date %in% addBotDates,]
    checkEquals(disBot3Secn$High, disNewQuotesBot3$High, tolerance = 0.001)
    checkEquals(disBot3Secn$Low, disNewQuotesBot3$Low, tolerance = 0.001)
    checkEquals(disBot3Secn$Open, disNewQuotesBot3$Open, tolerance = 0.001)
    checkEquals(disBot3Secn$Close, disNewQuotesBot3$Close, tolerance = 0.001)
    checkEquals(disBot3Secn$Volume, disNewQuotesBot3$Volume)
    ## clean up
    if(file.exists(outFile)) file.remove(outFile)
}

## Tests the case where all the data exists locally
test.getQuotesFromExisting <- function() {
    # create a file with quotes for DIS from 2005-12-15 through 2010-12-20
    tickSym <- "DIS"
    outFile <- sprintf('%s%s%s', testDataDir, tickSym, '.csv')
    disQuotes1 <- getStockQuotes(tickSym, '2005-12-15', '2010-12-20',
                                 dataDir=testDataDir)
    # requests subset of existing data
    disQuotes2 <- getStockQuotes(tickSym, '2010-01-15', '2010-05-20',
                                 dataDir=testDataDir)
    # check the first 2 records
    first2Dates <- c('2010-01-15', '2010-01-19')
    first2recs <- data.frame(Date=first2Dates,
                             High=c(31.15, 31.19), Low=c(30.41, 30.5),
                             Open=c(31.01, 30.59), Close=c(30.6, 31.01),
                             Volume=c(13936400, 9662100))
    disTop2recs <- disQuotes2[disQuotes2$Date %in% first2Dates, ]
    checkEquals(first2recs$High, disTop2recs$High, tolerance = 0.001)
    checkEquals(first2recs$Low, disTop2recs$Low, tolerance = 0.001)
    checkEquals(first2recs$Open, disTop2recs$Open, tolerance = 0.001)
    checkEquals(first2recs$Close, disTop2recs$Close, tolerance = 0.001)
    checkEquals(first2recs$Volume, disTop2recs$Volume)
    # check the last 2 records
    last2Dates <- c('2010-05-19', '2010-05-20')
    last2recs <- data.frame(Data=last2Dates,
                            High=c(33.80, 32.96), Low=c(33.09, 31.99),
                            Open=c(33.47, 32.76), Close=c(33.39, 31.99),
                            Volume=c(18536600, 26615500))
    disBot2recs <- disQuotes2[disQuotes2$Date %in% last2Dates, ]
    checkEquals(last2recs$High, disBot2recs$High, tolerance = 0.001)
    checkEquals(last2recs$Low, disBot2recs$Low, tolerance = 0.001)
    checkEquals(last2recs$Open, disBot2recs$Open, tolerance = 0.001)
    checkEquals(last2recs$Close, disBot2recs$Close, tolerance = 0.001)
    checkEquals(last2recs$Volume, disBot2recs$Volume)
    ## clean up
    if(file.exists(outFile)) file.remove(outFile)
}