

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
                                      service="finance.yahoo")
    jnjStart <- as.numeric(bigQuotes[3,3:7])
    jnjEnd <- as.numeric(bigQuotes[nrow(bigQuotes),3:7])
    checkEquals(jnjHLOCV2005.12.19, jnjStart, tolerance = 0.001)
    checkEquals(jnjHLOCV2010.12.20, jnjEnd, tolerance = 0.001)
}