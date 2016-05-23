
## Returns the date that's 10 years ago today in the format:
## yyyy-mm-dd.
tenYearsAgoToday <- function() {
    today <- as.POSIXlt(Sys.Date())
    today$year <- today$year - 10
    return(as.character(today))
}

makeOptList <- function(labels, values) {
    names(values) <- labels
    return(as.list(values))
}

# Default dates to use with live data
simEndDate <- as.character(Sys.Date())
simStartDate <- as.character(Sys.Date() - 365)
simStartDateMin = tenYearsAgoToday(); simEndDateMax = simEndDate

# Get moving average options for radio buttons
movingAvgUrl <- "./data/nonquotes/moving_avgs.csv"
maOptions <- read.csv(movingAvgUrl, stringsAsFactors = FALSE)
maOptionsList <- makeOptList(maOptions$MA_Type, maOptions$option)

# Get postion mgmt strategies to populate Position Management select list
posMgmtStratsUrl <- "./data/nonquotes/positionStrats.csv"
pmOptions <- read.csv(posMgmtStratsUrl, stringsAsFactors = FALSE)
pmOptionsList <- as.list((pmOptions$Position_Sizing_Strategy))

fluidPage(
    headerPanel("MACO Analyzer"),
    sidebarPanel(
        textInput('ticker', label=h4("Company:")),
        dateRangeInput('inQueryDateRange', label = h4("Quotes Date Range:"),
                       start=simStartDate, end=simEndDate,
                       min=simStartDateMin, max=simEndDateMax),
        actionButton('inQueryQuotes', 'Get Quote Data'),
        radioButtons('inMovAvg', label=h4("Moving Average (MA):"),
                     choices=maOptionsList, selected=1, inline = TRUE),
        sliderInput('inFastSlowMavg', h4("Fast & Slow MA Days"),
                    min = 2, max = 100, value = c(9,18)),
        numericInput('inAccBalance', 'Starting Account Balance:',
                     10000, min = 5000, max = 1000000, step = 500),
        selectInput('inPosMgmt', label=h4("Position Management:"),
                    choices=pmOptionsList, selected=1),
        actionButton('inRunSimButton', 'Run Simulation')

    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Analyzer",
                h4('Quote Data Status:'),
                verbatimTextOutput("outQuoteDataStatus"),
                h4('Simulation Parameters:'),
                verbatimTextOutput("outSimParams"),
                h4("Trades using this signal and position management:"),
                h6("(ProfitLoss calculation assumes $10 commission for each buy or sell)"),
                div(style='height:240px; overflow-y: scroll',
                    tableOutput("outTrades")
                ),
                h4('Net Trading Profit/Loss:'),
                verbatimTextOutput("outTradesNet")
            ),
            tabPanel("Graphics", h3("Trades identified using this signal:"),
                     h5(paste0("In the chart below, BUY signal triangles are ",
                               "shifted down and SELL triangles are shifted ",
                               "up so signals are more visible:"))
                     
            ),
            tabPanel("Signal", h3(textOutput("oidTradeSignal"))
                     
            )
        )
    )
)