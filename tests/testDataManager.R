# Using the testthat scheme:
#
# test_that("agoFrom: agoFromWhen default", {
#     cat("agoFrom: agoFromWhen default testing started...\n")
#     delta <- 10
#     expectedValue <- "2006-05-02"
#     # test default agoFromWhen
#     ymdToday <- unlist(strsplit(as.character(Sys.Date()), "-"))
#     yFromToday <- as.integer(ymdToday[1]) - delta
#     expectedYmd <- sprintf("%s%s%s%s%s", yFromToday, "-", ymdToday[2],
#                            "-", ymdToday[3])
#     funResult <- agoFrom(dateDelta = "years", deltaCount = delta)
#     result <- expect_identical(funResult, expected = expectedValue)
#     cat("what it is=", result, ", what's expected=", expectedValue, "\n")
# })
#
# test_that("agoFrom: agoFromWhen assigned", {
#     cat("agoFrom: agoFromWhen assigned testing started...\n")
#     delta <- 10
#     expectedValue <- "2006-05-02"
#     # test assigned agoFromWhen
#     funResult <- agoFrom(dateDelta = "years",
#                          deltaCount = delta,
#                          agoFromWhen = "2016-05-02")
#     result <- expect_identical(funResult, expected = expectedValue)
#     cat("what it is=", result, ", what's expected=", expectedValue, "\n")
# })

# Using the RUnit scheme:
# checkEquals: Are two objects equal, including named attributes?
# checkEqualsNumeric: Are two numeric values equal?
# checkIdentical: Are two objects exactly the same?
# checkTrue: Does an expression evaluate to TRUE?
# checkException: Does an expression raise an error?

test.agoFrom.default <- function() {
    cat("\nagoFrom: agoFromWhen default testing started...\n")
    delta <- 10
    # test default agoFromWhen
    ymdToday <- unlist(strsplit(as.character(Sys.Date()), "-"))
    yFromToday <- as.integer(ymdToday[1]) - delta
    expectedYmd <- sprintf("%s%s%s%s%s", yFromToday, "-", ymdToday[2],
                           "-", ymdToday[3])
    funResult <- agoFrom(dateDelta = "years", deltaCount = delta)
    result <- checkIdentical(funResult, expectedYmd)
    cat("function output = expected value?", result, "| expected =", expectedYmd, "\n")
}

test.agoFrom.assigned <- function() {
    cat("agoFrom: agoFromWhen assigned testing started...\n")
    delta <- 10
    expectedYmd <- "2006-05-02"
    # test assigned agoFromWhen
    funResult <- agoFrom(dateDelta = "years",
                         deltaCount = delta,
                         agoFromWhen = "2016-05-02")
    result <- checkIdentical(funResult, expectedYmd)
    cat("function output = expected value?", result, "| expected =", expectedYmd, "\n")
}

test.getQueryPeriods <- function() {
    expectedValues <- c(4, 3, 3, 3, 2)
    testValues <- getQueryPeriods("2016-04-05", "2016-04-19", 4)
    testValues <- c(testValues, getQueryPeriods("2016-04-05", "2016-04-19", 5))
    testValues <- c(testValues, getQueryPeriods("2016-04-05", "2016-04-19", 6))
    testValues <- c(testValues, getQueryPeriods("2016-04-05", "2016-04-19", 7))
    testValues <- c(testValues, getQueryPeriods("2016-04-05", "2016-04-18", 7))
    checkEqualsNumeric(testValues, expectedValues)
}

test.getCompletedYearsBetweenDates <- function() {
    expectedValues <- c(0,1,1,0,2,0,10,10,9)
    testValues <- getCompletedYearsBetweenDates("2016-01-01", "2016-05-01")
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2015-01-01", "2016-05-01"))
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2015-05-01", "2016-05-01"))
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2015-05-02", "2016-05-01"))
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2013-07-02", "2016-05-01"))
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2015-07-02", "2016-05-01"))
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2006-04-30", "2016-05-01"))
    testValues <- c(testValues,
                    getCompletedYearsBetweenDates("2006-04-30", "2017-04-01"))
    testValues <- append(testValues,
                         getCompletedYearsBetweenDates("2007-04-30", "2017-04-22"))
    
    checkEqualsNumeric(testValues, expectedValues)
}

test.getDateRanges <- function() {
    # tests with defaults maxAllowableDays=360, maxAllowableYears=10
    sDate="2011-03-01"; eDate="2016-05-04";
    sDates <- c("2011-03-01", "2012-02-25", "2013-02-20",
                "2014-02-16", "2015-02-12", "2016-02-08")
    eDates <- c("2012-02-24", "2013-02-19", "2014-02-15",
                "2015-02-11", "2016-02-07", "2016-05-04")
    dateRanges <- getDateRanges(sDate, eDate)
    rangeCount <- length(dateRanges)
    correctRangeCount <- checkEquals(rangeCount, 6)
    if(correctRangeCount) {
        cat("test.getDateRange default case generated correct # of data ranges",
            "\nchecking start and end dates of ranges...\n")
        for(i in 1:rangeCount) {
            checkStart <- dateRanges[[i]]['start']; names(checkStart) = NULL;
            checkEquals(checkStart, sDates[i])
            checkEnd <- dateRanges[[i]]['end']; names(checkEnd) = NULL;
            checkEquals(checkEnd, eDates[i])
        }
    } else {
        cat("test.getDateRange default case should have 6 date ranges",
            "but generated", rangeCount, "\n")
    }
    
    # test with non-default values for maxAllowableDays, maxAllowableYears
    # sDate="2016-03-01"; eDate="2016-04-01";
    # sDates <- c("2016-03-01", "2016-03-07", "2016-03-13",
    #             "2016-03-19", "2016-03-25", "2016-03-31")
    # eDates <- c("2012-02-24", "2013-02-19", "2014-02-15",
    #             "2015-02-11", "2016-02-07", "2016-05-04")
    # dateRanges <- getDateRanges(sDate, eDate, maxAllowableDays = 5)
}