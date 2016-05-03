# testthat script
# cat("testDataManager.R - Start testing...")
# delta <- 10
# # test default agoFromWhen
# ymdToday <- unlist(strsplit(as.character(Sys.Date()), "-"))
# yFromToday <- as.integer(ymdToday[1]) - delta
# expectedYmd <- sprintf("%s%s%s%s%s", yFromToday, "-", ymdToday[2],
#                        "-", ymdToday[3])
# funResult <- agoFrom(dateDelta = "years", deltaCount = delta)
# expect_identical(funResult, "2006-05-02")
# # test specified agoFromWhen
# funResult <- agoFrom(dateDelta = "years",
#                      deltaCount = delta,
#                      agoFromWhen = "2016-05-02")
# expect_identical(funResult, "2006-05-02")

cat("Testing DataManager.R\n\n")

test_that("agoFrom: agoFromWhen default", {
    cat("agoFrom: agoFromWhen default testing started...\n")
    delta <- 10
    expectedValue <- "2006-05-02"
    # test default agoFromWhen
    ymdToday <- unlist(strsplit(as.character(Sys.Date()), "-"))
    yFromToday <- as.integer(ymdToday[1]) - delta
    expectedYmd <- sprintf("%s%s%s%s%s", yFromToday, "-", ymdToday[2],
                           "-", ymdToday[3])
    funResult <- agoFrom(dateDelta = "years", deltaCount = delta)
    result <- expect_identical(funResult, expected = expectedValue)
    cat("what it is=", result, ", what's expected=", expectedValue, "\n")
})

test_that("agoFrom: agoFromWhen assigned", {
    cat("agoFrom: agoFromWhen assigned testing started...\n")
    delta <- 10
    expectedValue <- "2006-05-02"
    # test default agoFromWhen
    funResult <- agoFrom(dateDelta = "years",
                         deltaCount = delta,
                         agoFromWhen = "2016-05-02")
    result <- expect_identical(funResult, expected = expectedValue)
    cat("what it is=", result, ", what's expected=", expectedValue, "\n")
})