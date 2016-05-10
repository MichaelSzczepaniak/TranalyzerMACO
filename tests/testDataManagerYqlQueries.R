

test.getSinglePeriodYqlQuotes <- function() {
    
    expHLOCV2016.01.04 <- c(105.370003, 102, 102.610001, 105.349998, 67649400)
    expHLOCV2016.01.05 <- c(105.849998, 102.410004, 105.75, 102.709999, 55791000)
    quotes <- getSinglePeriodYqlQuotes(ticker='AAPL',
                                       startYYYY_MM_DD='2016-01-04',
                                       endYYYY_MM_DD='2016-01-05',
                                       dataFrame=NULL)
    ## 
    checkEqualsNumeric(expHLOCV2016.01.04[1], quotes$High[1])
    checkEqualsNumeric(expHLOCV2016.01.04[2], quotes$Low[1])
    checkEqualsNumeric(expHLOCV2016.01.04[3], quotes$Open[1])
    checkEqualsNumeric(expHLOCV2016.01.04[4], quotes$Close[1])
    checkEqualsNumeric(expHLOCV2016.01.04[5], quotes$Volume[1])
    ## 
    checkEqualsNumeric(expHLOCV2016.01.05[1], quotes$High[2])
    checkEqualsNumeric(expHLOCV2016.01.05[2], quotes$Low[2])
    checkEqualsNumeric(expHLOCV2016.01.05[3], quotes$Open[2])
    checkEqualsNumeric(expHLOCV2016.01.05[4], quotes$Close[2])
    checkEqualsNumeric(expHLOCV2016.01.05[5], quotes$Volume[2])
}