# install packages if needed
list.of.packages <- c('dplyr', 'readr', 'XML')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) install.packages(new.packages)
# load libraries
lapply(list.of.packages, require, character.only=TRUE)  # load libs
options(stringsAsFactors = FALSE) # want dates as strings, not factors

## Returns a string of the form yyyy-mm-dd which is deltaCount number of
## dateDelta time periods in the past from today. E.g. if defaults are used
## (dateDelta="years", deltaCount=10) will return the date that is 10 years
## ago from today.
## 
## agoFromWhen - A string of the form yyyy-mm-dd to start counting back from.
##               If not set, will default to the current day.
agoFrom <- function(dateDelta="years", deltaCount=10, agoFromWhen="today") {
    yyyy.mm.dd <- NULL
    if(agoFromWhen == "today") {
        if(dateDelta == "years") {
            yyyy.mm.dd <- unlist(strsplit(as.character(Sys.Date()), "-"))
        }
        # TODO process requests for dateDelta="months"
        # TODO process requests for dateDelta="weeks"
        # TODO process requests for dateDelta="days"
    } else {
        yyyy.mm.dd <- unlist(strsplit(agoFromWhen, "-"))
    }
    
    # increment years the easy way
    if(dateDelta == "years") {
        requestedYear <- as.integer(yyyy.mm.dd[1]) - deltaCount
        yyyy.mm.dd <- paste0(as.character(requestedYear), "-",
                             yyyy.mm.dd[2], "-", yyyy.mm.dd[3])
    }
    
    
    return(yyyy.mm.dd)
}

## Returns the number of query periods between startDate and endDate where
## the number of days in all but the most recent (last) query period are
## daysInInterval  The most recent period will usually be less than
## daysInInterval because it will typically be a partial period.
##
## daysInInterval includes the first and last date in the interval therefore,
## the difference between two dates = daysInInterval - 1. For example,
## if startDate = "2016-04-05" and endDate = "2016-04-06", daysInInterval = 2
## and difftime(as.Date(endDate), as.Date(startDate), units="days") = 1
##
## Preconditions: 1) daysInInterval > 0 and 2) startDate on or before endDate
##
## If 1) is violated, -1 is returned. If 2) is violated, -2 is returned.
getQueryPeriods <- function(startDate, endDate, daysInInterval) {
    if(daysInInterval <= 0) return(-1)
    diffEndStart <- as.integer(difftime(as.Date(endDate), as.Date(startDate),
                                        units="days"))
    if(diffEndStart < 0) return(-2)
    # single query period if the days in the overall interval between
    # endDays and startDays <= daysInInterval
    if(daysInInterval >= diffEndStart + 1) return(1)
    # Because our intervals include both startDate & endDate, this app expects
    # the days in the interval between consecutive dates such as "2016-04-02"
    # and "2016-04-01" to be 2. However, because
    # difftime("2016-04-02", "2016-04-01", units="days") returns 1, we need to
    # to add 1 below to get the number of days in the interval we expect.
    daysInPeriod <- diffEndStart + 1
    queryPeriods <- ceiling(as.integer(daysInPeriod) / daysInInterval)
    
    return(queryPeriods)
}

## Returns the number of completed years between startDate and endDate where:
## startDate - date further back in time from endDate
## endDate - further forward in time than startDate
getCompletedYearsBetweenDates <- function(startDate, endDate) {
    d1 <- as.Date(startDate)
    d2 <- as.Date(endDate)
    #http://stackoverflow.com/questions/19687334/19687995#19687995
    completedYears <- length(seq(from=d1, to=d2, by='year')) - 1
    
    return(completedYears)
}

## This function is used to break a date range into a list of component date
## ranges.  It returns a list of character vectors where each vector contains
## two strings of the form yyyy-mm-dd.
##
## Precondition: startDate precedes endDate in time
##
## startDate - string: Starting (earlier) date of the overall range to break out
## endDate - string: Ending (later) date of the overall range to break out
## daysInInterval - integer: The number of days in each of date ranges returned
##                  except for the most recent because it will usually be a
##                  partially filled range
##
## The first string in each character vector (named "start") of the returned
## list is the starting date of a query period.  The second sting (named "end")
## is the ending date of the period.
## 
## If the number of days between startDate and endDate is less than
## daysInInterval, then a list with a single vector containing startDate and
## endDate will be returned.
##
## If the number of days between startDate and endDate is greater than
## daysInInterval, then a list with 2 or more vectors will be returned.  Each
## vector in this case will span at most a daysInInterval number of days in a
## portion of the range between startDate and endDate.
getDateRanges <- function(startDate, endDate,
                          daysInInterval=360,
                          maxAllowableYears=10) {
    dateIntervals <- list(c(start=startDate, end=endDate))
    # back-off on the start date if period entered exceeds maxAllowableYears
    if(getCompletedYearsBetweenDates(startDate, endDate) >= maxAllowableYears) {
        adjustedStart <- as.POSIXlt(as.Date(endDate))
        adjustedStart$year <- adjustedStart$year - maxAllowableYears
        adjustedStart <- as.Date(adjustedStart)
        adjustedStart <- as.character(adjustedStart)
        dateIntervals <- list(c(start=adjustedStart, end=endDate))
        startDate <- adjustedStart
        queryPeriods <- getQueryPeriods(startDate, endDate, daysInInterval)
    } else {
        queryPeriods <- getQueryPeriods(startDate, endDate, daysInInterval)
    }
    
    if(queryPeriods < 0) {
        cat("getDateRanges recieved negative queryPeriods:", queryPeriods,
            "returning NULL\n")
        return(NULL)
    } else if(queryPeriods == 1) {
        return(dateIntervals)
    } else {
        firstEnd <- as.Date(startDate) + daysInInterval - 1
        dateIntervals <- list(c(start=startDate, end=as.character(firstEnd)))
        for(i in 2:queryPeriods) {
            dateInterval <- dateIntervals[[i-1]]
            nextStart <- as.Date(dateInterval["end"]) + 1
            newEnd <- nextStart + daysInInterval - 1
            if(i >= queryPeriods) {  # terminal/truncated range
                dateIntervals[[i]] <- c(start=as.character(nextStart),
                                        end=endDate)
            }
            else {
                dateIntervals[[i]] <- c(start=as.character(nextStart),
                                        end=as.character(newEnd))
            }
        }
    }
    
    return(dateIntervals)
}

## Builds and returns a string that makes a REST call to retrieve
## quote data from finance.yahoo.com.  This is an alternative to using YQL
## which seems to be simplier and more reliable.
##
## ticker - sting, a valid ticker symbol the service recognizes
## startYYYY_MM_DD - string, start date for quotes in the form yyyy-mm-dd
## endYYYY_MM_DD - string, end date for quotes in the form yyyy-mm-dd
##
## You should be able to put the string returned from this function directly
## into a browser and the browser should download the file.
##
## TODO Error checking to assure that valid date-related values are being used.
getYahooQuoteTableString <- function(ticker, startYYYY_MM_DD, endYYYY_MM_DD) {
    startYear <- strsplit(startYYYY_MM_DD, '-')[[1]][1]
    startMonth <- strsplit(startYYYY_MM_DD, '-')[[1]][2]
    startMonth <- as.character(as.integer(startMonth) - 1) # make 0-based
    startDay <- strsplit(startYYYY_MM_DD, '-')[[1]][3]
    
    endYear <- strsplit(endYYYY_MM_DD, '-')[[1]][1]
    endMonth <- strsplit(endYYYY_MM_DD, '-')[[1]][2]
    endMonth <- as.character(as.integer(endMonth) - 1) # make 0-based
    endDay <- strsplit(endYYYY_MM_DD, '-')[[1]][3]
    
    requestPrefix <- 'http://real-chart.finance.yahoo.com/table.csv?'
    requestPostfix <- '&g=d&ignore=.csv'
    requestString <- paste0(requestPrefix, 's=', ticker, '&',
                            'a=', startMonth, '&', 'b=', startDay, '&',
                            'c=', startYear, '&', 'd=', endMonth, '&',
                            'e=', endDay, '&', 'f=', endYear, requestPostfix)
}

## Creates and returns a valid YQL string to request quotes for symbol ticker
## between start date = startYYYY_MM_DD and end date = endYYYY_MM_DD
## ticker - sting, a valid ticker symbol the YQL recognizes
## startYYYY_MM_DD - string, start date for quotes in the form yyyy-mm-dd
## endYYYY_MM_DD - string, end date for quotes in the form yyyy-mm-dd
##
## TODO Error checking to assure that valid date-related values are being used.
getYqlQuoteString <- function(ticker, startYYYY_MM_DD, endYYYY_MM_DD) {
    # change https to http stackoverflow.com/questions/23584514/23584751#23584751
    baseQuery <- paste0("http://query.yahooapis.com/v1/public/yql",
                        "?q=select%20*%20from%20yahoo.finance.historicaldata")
    symbolClause <- paste0("%20where%20symbol%20%3D%20%22", ticker, "%22%20and")
    dateClause <- paste0("%20startDate%20%3D%20%22", startYYYY_MM_DD,
                         "%22%20and%20endDate%20%3D%20%22", endYYYY_MM_DD)
    yqlSuffix <- paste0("%22&diagnostics=true",
                        "&env=store%3A%2F%2Fdatatables.org",
                        "%2Falltableswithkeys")
    yqlRequestString <- paste0(baseQuery, symbolClause, dateClause, yqlSuffix)
    
    return(yqlRequestString)
}

## Makes a single REST call to get historical stock prices from yahoo finance 
## and returns a dataframe with the Date, High, Low, open, close and volume for 
## the symbol passed in over the requested date range.
##
## IF NO DATA COULD BE FOUND FOR TICKER, AN EMPTY DATAFRAME WILL BE RETURNED!
##
## ticker - ticker symbol to get quotes for
## startYYYY_MM_DD - starting date for the quote in format yyyy-mm-dd
## endYYYY_MM_DD - ending date for the quote in format yyyy-mm-dd
## 
## YQL queries like this were entered into the YQL console to generate
## the REST call:
##
## select * from yahoo.finance.historicaldata where symbol = "YHOO" and
##          startDate = "2015-01-01" and endDate = "2015-12-01"
##
## Resulting generated call looked like this
## (line breaks used for readability purposes only):
##
## https://query.yahooapis.com/v1/public/yql?
## q=select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol
## %20%3D%20%22AAPL%22%20and%20startDate%20%3D%20%222015-01-01%22%20and
## %20endDate%20%3D%20%222015-12-01%22&env=store%3A%2F%2Fdatatables.org%2
## Falltableswithkeys
##
## IMPORTANT: HAD TO CHANGE https TO http FOR THE CALL TO xmlTreeParse TO WORK!
getSinglePeriodYqlQuotes <- function(ticker, startYYYY_MM_DD,
                                     endYYYY_MM_DD, dataFrame=NULL) {
    #install.packages("XML"); install.packages("dplyr")
    cat("getSinglePeriodYqlQuotes parameters:", ticker,
        startYYYY_MM_DD, endYYYY_MM_DD, dataFrame, "\n")
    yqlCall <- getYqlQuoteString(ticker, startYYYY_MM_DD, endYYYY_MM_DD)
    library(XML)
    doc <- xmlTreeParse(yqlCall, useInternalNodes = TRUE)
    rootNode <- xmlRoot(doc)
    quotes <- xpathSApply(rootNode, "//quote")
    if(length(quotes) > 0) {
        High <- as.numeric(xpathSApply(rootNode, "//High", xmlValue))
        Low <- as.numeric(xpathSApply(rootNode, "//Low", xmlValue))
        Open <- as.numeric(xpathSApply(rootNode, "//Open", xmlValue))
        Close <- as.numeric(xpathSApply(rootNode, "//Close", xmlValue))
        Volume <- as.numeric(xpathSApply(rootNode, "//Volume", xmlValue))
        date.format <- "%Y-%m-%d"
        Date <- as.Date(xpathSApply(rootNode, "//Date", xmlValue), date.format)
        # If we pass in a populated dataframe, assume we need to append to it
        library(dplyr)
        if(is.null(dataFrame)) {
            df <- data.frame(Symbol=ticker, Date=as.character(Date),
                             High, Low, Open, Close, Volume)
            df <- arrange(df, Date)
        }
        else {
            temp <- data.frame(Symbol=ticker, Date=as.character(Date),
                               High, Low, Open, Close, Volume)
            temp <- arrange(temp, Date)
            df <- rbind(dataFrame, temp)
        }
    }
    else {
        # no quotes came back, return an empty data frame
        df <- data.frame()
    }
    
    return(df)
}

## Returns quote data from web service for a given
## ticker symbol ticker from startDate to endDate inclusive.
##
## This function calls getDateRanges to break up the requests to the data
## service (e.g. finance.yahoo) into manageable chunks.
##
## ticker - string | ticker symbol for requested quote (e.g. AAPL or JNJ)
## startDate - string | start/first date for requested price quotes
## endDate - string | end/last date for requested price quotes
## service - service from which to obtain quotes (finance.yahoo by default)
getQuotesFromService <- function(ticker, startDate, endDate,
                                 service="finance.yahoo") {
    library(XML)
    cat("getQuotesFromService parameters:\n",
        "startDate=", startDate,
        ", endDate=", endDate,
        ", service=", service, "\n")
    quotes <- NULL
    ## TODO put this code into its own function and source it
    if(service == "finance.yahoo") {
        dateRanges <- getDateRanges(startDate, endDate)
        quoteCount <- length(dateRanges)
        sdate <- dateRanges[[1]]["start"]
        edate <- dateRanges[[1]]["end"]
        quotes <- getSinglePeriodYqlQuotes(ticker, sdate, edate)
        if(quoteCount > 1) {
            for(i in 2:quoteCount) {
                sdate <- dateRanges[[i]]["start"]
                edate <- dateRanges[[i]]["end"]
                subQuotes <- getSinglePeriodYqlQuotes(ticker, sdate, edate)
                if(nrow(subQuotes) > 0) {
                    quotes <- rbind(quotes, subQuotes)
                }
            }
        }
    }
    # else if(service == "some other service") { lines to process quotes here }
    
    return(quotes)
}

## Returns a dataframe with quotes between startDate and endDate
## for ticker symbol ticker.  If query for ticker has not been made, function
## will query and write a file to ./data/[ticker].csv which will be read when
## future queries for ticker symbol ticker are made.
##
## If the file ./data/[ticker].csv exists and contains the requested data, a 
## data.frame will be returned, but no query to the quote service will be made.
##
## If the file ./data/[ticker].csv exists but does NOT contain the requested
## data, a query to the quote service will be made for the missing data as long
## as the request doesn't go back further than maxAllowableYears from today.
##
## ticker - string | ticker symbol for requested quote (e.g. AAPL or JNJ)
## startDate - string: start/first date for requested price quotes,
##             default is 10 years ago from todays date
## endDate - string: end/last date for requested price quotes, default is today
## maxAllowableYears - integer: max # of years of quote history to request
##                     from service, default is 10.
getStockQuotes <- function(ticker,
                           startDate=agoFrom(), 
                           endDate=as.character(Sys.Date()),
                           maxAllowableYears=10,
                           dataDir='./data/') {
    sDateObj <- as.Date(startDate); eDateObj <- as.Date(endDate) # for comp's
    # cat("getStockQuotes parameters:\n", "ticker=", ticker, "\n",
    #     "startDate=", startDate, ", endDate=", endDate, "\n",
    #     "maxAllowableYears=", maxAllowableYears, "\n")
    # TODO handle invalid ticker
    today <- as.character(Sys.Date())
    quoteCols <- c("Date", "High", "Low", "Open", "Close", "Volume")
    # check if data for the ticker has been downloaded already
    tickerFile <- paste0(dataDir, ticker, ".csv")
    if(file.exists(tickerFile)) {
        # quote file exists for ticker, but does is have all the data requested?
        existingQuotesAppended <- FALSE
        quotes <- read.csv(tickerFile, as.is=TRUE)
        startDataDate <- as.Date(quotes$Date[1])
        endDataDate <- as.Date(quotes$Date[nrow(quotes)])
        # check if existing data is current enough
        if(eDateObj > endDataDate) {
            # Requesting data that is not current enough. Need to
            # bring existing quotes up-to-date & append quote file.
            addStartDate <- as.character(endDataDate+1)
            addData <- getQuotesFromService(ticker, addStartDate, endDate)
            # leave quotes as-is if no add'l quotes come back 
            if(nrow(addData) > 0) {
                quotes <- rbind(quotes[,quoteCols], addData[,quoteCols])
                existingQuotesAppended <- TRUE
            }
        }
        # check if existing data goes back in time far enough
        if(startDataDate > sDateObj) {
            # existing data doesn't go back as far as being requested
            if(getCompletedYearsBetweenDates(startDataDate, endDataDate) <
               maxAllowableYears) {
                # request doesn't go back futher than allowable # of years, so
                # get more history: get max allowable amount of history while
                # we're at it...
                addData <- getQuotesFromService(ticker,
                                                startDate,
                                                as.character(startDataDate-1))
                if(nrow(addData) > 0) {
                    quotes <- rbind(addData[, -1], quotes)
                    existingQuotesAppended <- TRUE
                }
            }
            else {
                # request is for too much history, go back as far as allowed
                # by maxAllowableYears TODO
                # 1) earliesAllowableStartDate <- maxAllowableYears from today
                # 2) addData <- getQuotesFromService(ticker,
                #                                    earliesAllowableStartDate),
                #                                    startDataDate-1)
                # 3) append addData to backend of quote data (earliest side)
                # 4) existingQuotesAppended <- TRUE
                # remaining code should handle the rest
            }
        }
        if(existingQuotesAppended) {
            # quotes has gotten data appended to it: write out updated quotes
            filePath <- paste0(dataDir, ticker, ".csv")
            write.csv(quotes, filePath, row.names=FALSE)
            cat("Completed writing updated quotes to ", filePath, "\n")
        }
    }
    else {
        # quote file doesn't exist yet: query, write, then read written file
        # TODO startDate and endDate allowable?
        # cat('getStockQuotes: No', tickerFile, 'exists.',
        #     'Getting quote between', startDate, 'and', endDate, '...\n')
        quotes <- getQuotesFromService(ticker, startDate, endDate)
        # cat('getStockQuotes: Got quotes. Writing', tickerFile, '...')
        writeQuotes(tickers = c(ticker), startDate, endDate, dataDir)
        quotes <- read.csv(tickerFile, as.is=TRUE)
    }
    
    # Lastly, only return what's asked for
    quotes <- quotes[quotes$Date >= startDate & quotes$Date <= endDate,]
    
    return(quotes)
}

## Writes downloaded quotes to a csv file to be reread later
## tickers - vector of ticker symbols
## startDate - starting date for the quote in format yyyy-mm-dd
## endDate - ending date for the quote in format yyyy-mm-dd
writeQuotes <- function(tickers, startDate, endDate=as.character(Sys.Date()),
                        dataDir='./data/') {
    quoteCols <- c("Date", "High", "Low", "Open", "Close", "Volume")
    for(i in 1:length(tickers)) {
        quotes <- getQuotesFromService(tickers[i], startDate, endDate)
        quotes <- quotes[, quoteCols] # don't need Symbol column
        filePath <- paste0(dataDir, tickers[i], ".csv") # symbol in filename
        write.csv(quotes, filePath, row.names=FALSE)
        cat("Completed writing quotes to ", filePath, "\n")
    }
}

## Lower risk way to get data for demo purposes. This reads a csv from the 
## project repo rather than querying finance.yahoo for the data
getDemoQuotes <- function(ticker, startDate,
                          endDate=as.character(Sys.Date())) {
    library(dplyr)
    demoQuotesPrefix <- "./data/"
    demoQuotesPath <- paste0(demoQuotesPrefix, ticker, ".csv")
    
    quotes <- read.csv(demoQuotesPath, as.is=TRUE)
    quotes$Date <- as.Date(quotes$Date)
    quotes <- filter(quotes,
                     Date >= as.Date(startDate) & Date <=as.Date(endDate))
    quotes$Date <- as.character(quotes$Date)
    
    return(quotes)
}