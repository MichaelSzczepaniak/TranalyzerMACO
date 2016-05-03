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